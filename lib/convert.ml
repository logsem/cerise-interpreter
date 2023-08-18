open Extract

let translate_perm (p : perm) : Ast.perm =
  match p with
  | O -> Ast.O
  | E -> Ast.E
  | RO -> Ast.RO
  | RX -> Ast.RX
  | RW -> Ast.RW
  | RWX -> Ast.RWX
  | RWL -> Ast.RWL
  | RWLX -> Ast.RWLX
  | URW -> Ast.URW
  | URWL -> Ast.URWL
  | URWX -> Ast.URWX
  | URWLX -> Ast.URWLX

let translate_locality (g : locality) : Ast.locality =
  match g with
  | Local -> Ast.Local
  | Global -> Ast.Global
  | Directed -> Ast.Directed

let translate_wtype (wt : word) : Ast.wtype =
  match wt with
  | WInt _ -> Ast.W_I
  | WSealable (SCap _) -> Ast.W_Cap
  | WSealable (SSealRange _) -> Ast.W_SealRange
  | WSealed _ -> Ast.W_Sealed

let translate_regname (r : regName) : Ast.regname =
  match r with
  | PC -> Ast.PC
  | STK -> Ast.STK
  | R n -> Ast.Reg (Big_int_Z.int_of_big_int n)

let translate_sum (s : (Big_int_Z.big_int, regName) sum) : Ast.reg_or_const =
match s with
| Inr r -> Ast.Register (translate_regname r)
| Inl z -> Ast.Const z

let translate_instr (i : cerise_instruction) : Ast.machine_op =
  match i with
  | Jmp r -> Ast.Jmp (translate_regname r)
  | Jnz (r1, r2) -> Ast.Jnz (translate_regname r1, translate_regname r2)
  | Mov (r, c) -> Ast.Move (translate_regname r,
                             translate_sum c)
  | Load (r1, r2) -> Ast.Load (translate_regname r1,
                               translate_regname r2)
  | Store (r, c) -> Ast.Store (translate_regname r,
                               translate_sum c)
  | Add (r, c1, c2) -> Ast.Add (translate_regname r,
                                translate_sum c1,
                                translate_sum c2)
  | Sub (r, c1, c2) -> Ast.Sub (translate_regname r,
                                translate_sum c1,
                                translate_sum c2)
  | Mul (r, c1, c2) -> Ast.Mul (translate_regname r,
                                translate_sum c1,
                                translate_sum c2)
  | Rem (r, c1, c2) -> Ast.Rem (translate_regname r,
                                translate_sum c1,
                                translate_sum c2)
  | Div (r, c1, c2) -> Ast.Div (translate_regname r,
                                translate_sum c1,
                                translate_sum c2)
  | Lt0 (r, c1, c2) -> Ast.Lt (translate_regname r,
                              translate_sum c1,
                              translate_sum c2)
  | Lea (r, c) -> Ast.Lea (translate_regname r, translate_sum c)
  | Restrict (r, c) -> Ast.Restrict (translate_regname r,
                                     translate_sum c)
  | Subseg (r, c1, c2) -> Ast.SubSeg (translate_regname r,
                                      translate_sum c1,
                                      translate_sum c2)
  | GetL (r1, r2) -> Ast.GetL (translate_regname r1, translate_regname r2)
  | GetB (r1, r2) -> Ast.GetB (translate_regname r1, translate_regname r2)
  | GetE (r1, r2) -> Ast.GetE (translate_regname r1, translate_regname r2)
  | GetA (r1, r2) -> Ast.GetA (translate_regname r1, translate_regname r2)
  | GetP (r1, r2) -> Ast.GetP (translate_regname r1, translate_regname r2)
  | GetWType (r1, r2) -> Ast.GetWType (translate_regname r1, translate_regname r2)
  | GetOType (r1, r2) -> Ast.GetWType (translate_regname r1, translate_regname r2)
  | Seal (r1, r2, r3) -> Ast.Seal (translate_regname r1, translate_regname r2, translate_regname r3)
  | UnSeal (r1, r2, r3) -> Ast.UnSeal (translate_regname r1, translate_regname r2, translate_regname r3)

  | LoadU (r1, r2, c) -> Ast.LoadU (translate_regname r1,
                                    translate_regname r2,
                                    translate_sum c)
  | StoreU (r, c1, c2) -> Ast.StoreU (translate_regname r,
                                      translate_sum c1,
                                      translate_sum c2)
  | PromoteU r -> Ast.PromoteU (translate_regname r)
  | Fail -> Ast.Fail
  | Halt -> Ast.Halt

let tr_reg (r : Ast.regname) : regName =
  match r with
  | Ast.PC -> PC
  | Ast.STK -> STK
  | Ast.Reg n -> R (Big_int_Z.big_int_of_int n)

let tr_sum (c : Ast.reg_or_const) : (Big_int_Z.big_int, regName) sum =
  match c with
  | Register r -> Inr (tr_reg r)
  | Const z -> Inl z
    (* TODO might require to use decode/encode from Ir *)
    (* Ir.decode *)
    (* match cp with *)
    (* | Const n -> Inl n *)
    (* | Perm (p,g)-> *)
    (*   let n = Encode.encode_perm_pair p g in *)
    (*   Inl n *)

let tr_machine_op (s : Ast.machine_op) : cerise_instruction =
  match s with
  | Ast.Jmp r -> Jmp (tr_reg r)
  | Ast.Jnz (r1, r2) -> Jnz (tr_reg r1, tr_reg r2)
  | Ast.Move (r, c) -> Mov (tr_reg r, tr_sum c)
  | Ast.Load (r1, r2) -> Load (tr_reg r1, tr_reg r2)
  | Ast.Store (r, c) -> Store (tr_reg r, tr_sum c)
  | Ast.Add (r, c1, c2) -> Add (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.Sub (r, c1, c2) -> Sub (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.Mul (r, c1, c2) -> Mul (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.Rem (r, c1, c2) -> Rem (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.Div (r, c1, c2) -> Div (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.Lt (r, c1, c2) -> Lt0 (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.Lea (r, c) -> Lea (tr_reg r, tr_sum c)
  | Ast.Restrict (r, c) -> Restrict (tr_reg r, tr_sum c)
  | Ast.SubSeg (r, c1, c2) -> Subseg (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.GetL (r1, r2) -> GetL (tr_reg r1, tr_reg r2)
  | Ast.GetB (r1, r2) -> GetB (tr_reg r1, tr_reg r2)
  | Ast.GetE (r1, r2) -> GetE (tr_reg r1, tr_reg r2)
  | Ast.GetA (r1, r2) -> GetA (tr_reg r1, tr_reg r2)
  | Ast.GetP (r1, r2) -> GetP (tr_reg r1, tr_reg r2)
  | Ast.GetOType (r1, r2) -> GetOType (tr_reg r1, tr_reg r2)
  | Ast.GetWType (r1, r2) -> GetWType (tr_reg r1, tr_reg r2)
  | Ast.Seal (r1, r2, r3) -> Seal (tr_reg r1, tr_reg r2, tr_reg r3)
  | Ast.UnSeal (r1, r2, r3) -> UnSeal (tr_reg r1, tr_reg r2, tr_reg r3)
  | Ast.LoadU (r1, r2, c) -> LoadU (tr_reg r1, tr_reg r2, tr_sum c)
  | Ast.StoreU (r, c1, c2) -> StoreU (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.PromoteU r -> PromoteU (tr_reg r)
  | Ast.Fail -> Fail
  | Ast.Halt -> Halt

let tr_perm (p : Ast.perm) : perm =
  match p with
  | Ast.O -> O
  | Ast.E -> E
  | Ast.RO -> RO
  | Ast.RX -> RX
  | Ast.RW -> RW
  | Ast.RWX -> RWX
  | Ast.RWL -> RWL
  | Ast.RWLX -> RWLX
  | Ast.URW -> URW
  | Ast.URWL -> URWL
  | Ast.URWX -> URWX
  | Ast.URWLX -> URWLX

let tr_loc (g : Ast.locality) : locality =
  match g with
  | Ast.Local -> Local
  | Ast.Global -> Global
  | Ast.Directed -> Directed

let tr_wtype (wt : Ast.wtype) : word =
  match wt with
  | Ast.W_I -> wt_int
  | Ast.W_Cap -> wt_cap
  | Ast.W_SealRange -> wt_sealrange
  | Ast.W_Sealed -> wt_sealed

let encode_label_name (l : Big_int_Z.big_int list) : Big_int_Z.big_int =
  let rec encode_label_name' (l : Big_int_Z.big_int list)
    : Big_int_Z.big_int =
    match l with
    | [] -> Big_int_Z.big_int_of_int (-1)
    | n::l'-> Encode.encode_int_int n (encode_label_name' l')
    in
    encode_label_name' l

let encode_labeled_instr (i : labeled_instr) : Big_int_Z.big_int =
  let (^!) (opcode : Big_int_Z.big_int) (args : Big_int_Z.big_int) =
    Big_int_Z.add_big_int opcode (Big_int_Z.shift_left_big_int args 8)
  in
  match i with
  | BInstr i' ->
    (Big_int_Z.big_int_of_int (0x1)) ^! Encode.encode_machine_op (translate_instr i')
  | Label n -> (Big_int_Z.big_int_of_int (0x2)) ^! (encode_label_name n)
  | Br_Jmp n -> (Big_int_Z.big_int_of_int (0x3)) ^! (encode_label_name n)
  | Br_Jnz (n,r) -> (Big_int_Z.big_int_of_int (0x4))
                    ^! ( Encode.encode_int_int (encode_label_name n) r)

let decode_label_name (l : Big_int_Z.big_int) : Big_int_Z.big_int list =
  let rec decode_label_name' (l : Big_int_Z.big_int)
    : Big_int_Z.big_int list =
    if (Big_int_Z.eq_big_int l (Big_int_Z.big_int_of_int (-1)))
    then []
    else
      let (n, l') = Encode.decode_int l in
      n::(decode_label_name' l')
  in
  decode_label_name' l

let decode_labeled_instr (z : Big_int_Z.big_int) : labeled_instr =
    let opc = Big_int_Z.extract_big_int z 0 8 in
    let payload = Big_int_Z.shift_right_big_int z 8 in
    (* Label *)
    if opc = (Big_int_Z.big_int_of_int 0x1)
    then
      match (Encode.decode_machine_op payload) with
    | i -> BInstr (tr_machine_op i)
    | exception Encode.DecodeException s ->
      raise @@ Encode.DecodeException (s ^ " Error decoding labeled instruction: unrecognized BInstr")
    else

    if opc = (Big_int_Z.big_int_of_int 0x2)
    then Label (decode_label_name payload)
    else

    if opc = (Big_int_Z.big_int_of_int 0x3)
    then Br_Jmp (decode_label_name payload)
    else

    if opc = (Big_int_Z.big_int_of_int 0x4)
    then
      let (l_enc, r) = Encode.decode_int payload in
      Br_Jnz (decode_label_name l_enc, r)
    else
      raise @@ Encode.DecodeException "Error decoding labeled instruction: unrecognized opcode"

let machine_param = {
  decodeInstr = (function z -> tr_machine_op (Encode.decode_machine_op z));
  encodeInstr = (function i -> Encode.encode_machine_op (translate_instr i));
  encodePerm = (function p -> Encode.encode_perm (translate_perm p));
  encodeLoc = (function g -> Encode.encode_locality (translate_locality g));

  decodePermPair = (function z -> let (p,g) = Encode.decode_perm_loc_pair z in (tr_perm p, tr_loc g));
  encodePermPair = (function p -> let (p,g) = p in Encode.encode_perm_loc_pair (translate_perm p) (translate_locality g));

  decodeSealPermPair = (function z -> let (p,g) = Encode.decode_seal_perm_loc_pair z in (p, tr_loc g));
  encodeSealPermPair = (function p -> let (p,g) = p in Encode.encode_seal_perm_loc_pair p (translate_locality g));

  encodeSealPerms = (function p -> Encode.encode_seal_perm p);
  decodeSealPerms = (function p -> Encode.decode_seal_perm p);
  encodeWordType = (function wt -> Encode.encode_wtype (translate_wtype wt));
  decodeWordType = (function wt -> tr_wtype (Encode.decode_wtype wt));
}

let translate_nbar (nb : Extract.nbar) =
  match nb with | Finite z -> Infinite_z.Int z | P_infty -> Infinite_z.Inf

let translate_sealable (sb : Extract.sealable) =
  match sb with
  | SCap ((p,l), b, e, a) ->
    Ast.Cap (translate_perm p, translate_locality l, b, translate_nbar e, a)
  | SSealRange ((p,l), b, e, a) -> Ast.SealRange (p, translate_locality l, b, e, a)


let translate_word (w : Extract.word) =
  match w with
  | WInt z -> Ast.I z
  | WSealable sb -> Ast.Sealable (translate_sealable sb)
  | WSealed (ot, sb) -> Ast.Sealed (ot, translate_sealable sb)

let convert_error_msg (m : errorMsg) = (String.concat "" (List.map (String.make 1) m))
