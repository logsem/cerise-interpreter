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

let translate_regname (r : regName) : Ast.regname =
  match r with
  | PC -> Ast.PC
  | STK -> Ast.STK
  | R n -> Ast.Reg (Big_int_Z.int_of_big_int n)

let translate_sum (s : (Big_int_Z.big_int, regName) sum) : Ast.reg_or_const =
match s with
| Inr r -> Ast.Register (translate_regname r)
| Inl z -> Ast.CP (Const z)

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
  | Lt0 (r, c1, c2) -> Ast.Lt (translate_regname r,
                              translate_sum c1,
                              translate_sum c2)
  | Lea (r, c) -> Ast.Lea (translate_regname r, translate_sum c)
  | Restrict (r, c) -> Ast.Restrict (translate_regname r,
                                     translate_sum c)
  | Subseg (r, c1, c2) -> Ast.SubSeg (translate_regname r,
                                      translate_sum c1,
                                      translate_sum c2)
  | IsPtr (r1, r2) -> Ast.IsPtr (translate_regname r1, translate_regname r2)
  | GetL (r1, r2) -> Ast.GetL (translate_regname r1, translate_regname r2)
  | GetP (r1, r2) -> Ast.GetP (translate_regname r1, translate_regname r2)
  | GetB (r1, r2) -> Ast.GetB (translate_regname r1, translate_regname r2)
  | GetE (r1, r2) -> Ast.GetE (translate_regname r1, translate_regname r2)
  | GetA (r1, r2) -> Ast.GetA (translate_regname r1, translate_regname r2)
  | LoadU (r1, r2, c) -> Ast.LoadU (translate_regname r1,
                                    translate_regname r2,
                                    translate_sum c)
  | StoreU (r, c1, c2) -> Ast.StoreU (translate_regname r,
                                      translate_sum c1,
                                      translate_sum c2)
  | PromoteU r -> Ast.PromoteU (translate_regname r)
  | Fail -> Ast.Fail
  | Halt -> Ast.Halt
  | Nop -> Ast.Nop

let tr_reg (r : Ast.regname) : regName =
  match r with
  | Ast.PC -> PC
  | Ast.STK -> STK
  | Ast.Reg n -> R (Big_int_Z.big_int_of_int n)

let tr_sum (c : Ast.reg_or_const) : (Big_int_Z.big_int, regName) sum =
  match c with
  | Register r -> Inr (tr_reg r)
  | CP cp ->
    match cp with
    | Const n -> Inl n
    | Perm (p,g)->
      let n = Encode.encode_perm_pair p g in
      Inl n

let tr_machine_op (s : Ast.machine_op) : cerise_instruction =
  match s with
  | Ast.Jmp r -> Jmp (tr_reg r)
  | Ast.Jnz (r1, r2) -> Jnz (tr_reg r1, tr_reg r2)
  | Ast.Move (r, c) -> Mov (tr_reg r, tr_sum c)
  | Ast.Load (r1, r2) -> Load (tr_reg r1, tr_reg r2)
  | Ast.Store (r, c) -> Store (tr_reg r, tr_sum c)
  | Ast.Add (r, c1, c2) -> Add (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.Sub (r, c1, c2) -> Sub (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.Lt (r, c1, c2) -> Lt0 (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.Lea (r, c) -> Lea (tr_reg r, tr_sum c)
  | Ast.Restrict (r, c) -> Restrict (tr_reg r, tr_sum c)
  | Ast.SubSeg (r, c1, c2) -> Subseg (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.IsPtr (r1, r2) -> IsPtr (tr_reg r1, tr_reg r2)
  | Ast.GetL (r1, r2) -> GetL (tr_reg r1, tr_reg r2)
  | Ast.GetP (r1, r2) -> GetP (tr_reg r1, tr_reg r2)
  | Ast.GetB (r1, r2) -> GetB (tr_reg r1, tr_reg r2)
  | Ast.GetE (r1, r2) -> GetE (tr_reg r1, tr_reg r2)
  | Ast.GetA (r1, r2) -> GetA (tr_reg r1, tr_reg r2)
  | Ast.LoadU (r1, r2, c) -> LoadU (tr_reg r1, tr_reg r2, tr_sum c)
  | Ast.StoreU (r, c1, c2) -> StoreU (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.PromoteU r -> PromoteU (tr_reg r)
  | Ast.Fail -> Fail
  | Ast.Halt -> Halt
  | Ast.Nop -> Nop

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

let driver = {
  decodeInstr = (function z -> tr_machine_op (Encode.decode_machine_op z));
  encodeInstr = (function i -> Encode.encode_machine_op (translate_instr i));
  encodePerm = (function p -> Encode.encode_perm (translate_perm p));
  encodeLoc = (function g -> Encode.encode_locality (translate_locality g));
  decodePermPair = (function z ->
      let (p,g) = Encode.decode_perm_pair z in
      (tr_perm p, tr_loc g));
  encodePermPair = (function p ->
    let (p,g) = p in Encode.encode_perm_pair (translate_perm p) (translate_locality g));
  (* Dummy encoding *)
  l_decodeInstr = (function z -> BInstr Fail);
  l_encodeInstr = (function i -> Encode.encode_machine_op Fail);
}
