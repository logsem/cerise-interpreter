open Ast

(** Calling convention from
 https://github.com/logsem/cerise-stack-monotone/blob/master/theories/overlay/call.v *)

let const n = Const (Z.of_int n)

let in_list (e : 'a) (l : 'a list) : bool =
  match List.find_opt (fun x -> x = e) l with None -> false | Some _ -> true

let all_registers : regname list = [ PC; stk ] @ List.init 32 (fun i -> Reg i)
let rstk : regname = Reg 0

let rec rclear (regs : regname list) : machine_op list =
  match regs with [] -> [] | r :: regs' -> Move (r, const 0) :: rclear regs'

let rclear_inv (regs : regname list) : machine_op list =
  rclear (List.filter (fun r -> not (in_list r regs)) all_registers)

let reg_diff (regs : regname list) : regname list =
  List.filter (fun r -> not (in_list r regs)) all_registers

let push_instrs ps = List.map (fun p -> StoreU (rstk, const 0, p)) ps

let pop_instrs r =
  [
    Encode.encode_machine_op (LoadU (r, rstk, const (-1)));
    Encode.encode_machine_op (Lea (rstk, const (-1)));
  ]

let push_env = push_instrs (List.map (fun r -> Register r) (reg_diff [ PC; rstk ]))
let pop_env = List.fold_left (fun b r -> pop_instrs r @ b) [] (reg_diff [ PC; rstk ])

let call_prologue (rargs : regname list) : machine_op list =
  push_instrs [ const 0 ]
  @ push_env @ push_instrs [ Register rstk ]
  @ push_instrs
      (List.map
         (fun i -> Const (Encode.encode_machine_op i))
         [ Move (Reg 31, Register PC); Lea (Reg 31, const (-1)); Load (rstk, Reg 31) ]
      @ List.map (fun i -> Const i) pop_env
      @ [ Const (Encode.encode_machine_op (LoadU (PC, rstk, const (-1)))) ])
  @ [
      Move (Reg 31, Register PC);
      Lea (Reg 31, const (41 + List.length rargs));
      StoreU (rstk, const (-99), Register (Reg 1));
    ]

let scall (r : regname) (rargs : regname list) : machine_op list =
  let e_directed_perm = Const (Encode.encode_perm_loc_pair E Directed) in
  call_prologue rargs
  @ [
      Move (Reg 30, Register rstk);
      PromoteU (Reg 30);
      Lea (Reg 30, const (-66));
      Restrict (Reg 30, e_directed_perm);
      GetA (Reg 31, rstk);
      GetB (Reg 29, rstk);
      SubSeg (rstk, Register (Reg 31), Register (Reg 31));
    ]
  @ push_instrs [ Register (Reg 30) ]
  @ push_instrs (List.map (fun x -> Register x) rargs)
  @ rclear (reg_diff [ PC; rstk ])
  @ [ Jmp r ]
