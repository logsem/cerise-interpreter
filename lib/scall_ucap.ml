open Ast

let in_list (e : 'a) (l : 'a list) : bool =
  match List.find_opt (fun x -> x = e) l with None -> false | Some _ -> true

let all_registers : regname list = [ PC; stk ] @ List.init 32 (fun i -> Reg i)

let rec rclear (regs : regname list) : machine_op list =
  match regs with [] -> [] | r :: regs' -> Move (r, const 0) :: rclear regs'

let rclear_inv (regs : regname list) : machine_op list =
  rclear (List.filter (fun r -> not (in_list r regs)) all_registers)

let mclearU : machine_op list =
  let mclear_off_end = 9 in
  let mclear_off_iter = 2 in
  [
    Move (Reg 4, Register stk);
    GetB (Reg 1, Reg 4);
    GetB (Reg 2, Reg 4);
    Sub (Reg 2, Register (Reg 1), Register (Reg 2));
    Lea (Reg 4, Register (Reg 2));
    GetE (Reg 5, Reg 4);
    Sub (Reg 5, Register (Reg 5), const 1);
    Move (Reg 2, Register PC);
    Lea (Reg 2, const mclear_off_end);
    Move (Reg 3, Register PC);
    Lea (Reg 3, const mclear_off_iter);
    (* iter *)
    Lt (Reg 6, Register (Reg 5), Register (Reg 1));
    Jnz (Reg 2, Reg 6);
    StoreU (Reg 4, const 0, const 0);
    Add (Reg 1, Register (Reg 1), const 1);
    Jmp (Reg 3);
    (* end *)
  ]
  @ rclear [ Reg 1; Reg 2; Reg 3; Reg 4; Reg 5; Reg 6 ]

let pushU r = StoreU (stk, const 0, r)

let popU r =
  [ LoadU (r, stk, const (-1)); Sub (Reg 1, const 0, const 1); Lea (stk, Register (Reg 1)) ]

(* TODO the macro is wrong *)
let reqglob_instrs r =
  [
    GetL (Reg 1, r);
    Sub (Reg 1, Register (Reg 1), Const (Encode.encode_locality Global));
    Move (Reg 2, Register PC);
    Lea (Reg 2, Register (Reg 1));
    Jnz (Reg 2, Reg 1);
    Move (Reg 2, Register PC);
    Lea (Reg 2, const 4);
    Jmp (Reg 2);
    Fail;
    Move (Reg 1, const 0);
    Move (Reg 2, const 0);
  ]

let reqperm r (p : Z.t) =
  [
    GetP (Reg 1, r);
    Sub (Reg 1, Register (Reg 1), Const p);
    Move (Reg 2, Register PC);
    Lea (Reg 2, const 6);
    Jnz (Reg 2, Reg 1);
    Move (Reg 2, Register PC);
    Lea (Reg 2, const 4);
    Jmp (Reg 2);
    Fail;
    Move (Reg 1, const 0);
    Move (Reg 1, const 0);
  ]

let reqsize r s =
  [
    GetP (Reg 1, r);
    GetE (Reg 2, r);
    Sub (Reg 1, Register (Reg 2), Register (Reg 1));
    Lt (Reg 1, Const s, Register (Reg 1));
    Move (Reg 2, Register PC);
    Lea (Reg 2, const 4);
    Jnz (Reg 2, Reg 1);
    Fail;
  ]

let prepstack r minsize =
  reqperm r (Encode.encode_perm URWLX)
  @ reqsize r minsize
  @ [
      GetB (Reg 1, r);
      GetA (Reg 2, r);
      Sub (Reg 1, Register (Reg 1), Register (Reg 2));
      Lea (r, Register (Reg 1));
      Move (Reg 1, const 0);
      Move (Reg 2, const 0);
    ]

let epilogue_scall radv : machine_op list =
  let w1 = Encode.encode_machine_op (Move (Reg 1, Register PC)) in
  let w2 = Encode.encode_machine_op (Lea (Reg 1, const 6)) in
  let w3 = Encode.encode_machine_op (Load (stk, Reg 1)) in
  let w4a = Encode.encode_machine_op (Sub (Reg 1, const 0, const 1)) in
  let w4b = Encode.encode_machine_op (LoadU (PC, stk, Register (Reg 1))) in
  let epilogue_b =
    [
      (* Push activation record *)
      pushU (Const w1);
      pushU (Const w2);
      pushU (Const w3);
      pushU (Const w4a);
      pushU (Const w4b);
    ]
  in

  let epilogue_e epilogue_off =
    let e_local_perm = Const (Encode.encode_perm_loc_pair E Local) in
    (* push old pc *)
    [
      Move (Reg 1, Register PC);
      Lea (Reg 1, Const epilogue_off);
      pushU (Register (Reg 1));
      (* push stack pointer *)
      (* Note that the stored r_stk will not be able to read itself, *)
      (*  but that should not be a problem. *)
      pushU (Register stk);
      (* set up protected return pointer *)
      (* since a URWLX capability cannot be made into an E, we have to promote r_t0 first *)
      Move (Reg 0, Register stk);
      PromoteU (Reg 0);
      Lea (Reg 0, const (-7));
      Restrict (Reg 0, e_local_perm);
      (* restrict stack capability *)
      GetA (Reg 1, stk);
      GetE (Reg 2, stk);
      SubSeg (stk, Register (Reg 1), Register (Reg 2));
    ]
    @ rclear_inv [ PC; stk; Reg 0; radv ]
  in
  let off = List.length (epilogue_e (Z.of_int 0)) in
  epilogue_b @ epilogue_e (Z.of_int off)

let awkward_example radv =
  let renv = Reg 30 in

  (* let awkward_epilogue_off = 40 in *)
  reqglob_instrs radv
  @ prepstack stk (Z.of_int 10)
  @ [
      Store (renv, const 0);
      (* Local encapsulation *)
      pushU (Register renv);
      pushU (Register (Reg 0));
      pushU (Register radv);
    ]
  @ epilogue_scall radv
  @ [
      Jmp radv;
      StoreU (stk, const 0, const 0);
      Sub (Reg 1, const 0, const 7);
      Lea (stk, Register (Reg 1));
    ]
  @ popU radv @ popU (Reg 0) @ popU renv
  @ [ Store (renv, const 1); pushU (Register renv); pushU (Register (Reg 0)) ]
  @ epilogue_scall radv
  @ [ Jmp radv; Sub (Reg 1, const 0, const 6); Lea (stk, Register (Reg 1)) ]
  @ popU (Reg 0) @ popU renv (* Skip the assert *)
  @ [
      GetB (Reg 1, stk);
      Add (Reg 2, Register (Reg 1), Register (Reg 2));
      SubSeg (stk, Register (Reg 1), Register (Reg 2));
    ]
  @ mclearU
  @ rclear_inv [ PC; Reg 0 ]
  @ [ Jmp (Reg 0) ]

let adv_instr = [ Jmp (Reg 0) ]
let ret_instr = [ Halt ]
