open Ast
open Misc

let (^-) s1 s2 = s1 ^ " " ^ s2

let string_of_regname (r: regname) : string =
  match r with
  | PC -> "pc"
  | STK -> "stk"
  | Reg i -> "r" ^ (string_of_int i)

let string_of_seal_perm (p : seal_perm) : string =
  match p with
  | (false, false) -> "SO"
  | (true, false) -> "S"
  | (false, true) -> "U"
  | (true, true) -> "SU"

let string_of_perm (p: perm): string =
  match p with
  | O -> "O"
  | E -> "E"
  | RO -> "RO"
  | RX -> "RX"
  | RW -> "RW"
  | RWX -> "RWX"
  | RWL -> "RWL"
  | RWLX -> "RWLX"
  | URW -> "URW"
  | URWX -> "URWX"
  | URWL -> "URWL"
  | URWLX -> "URWLX"

let string_of_locality (g: locality): string =
  match g with
  | Local -> "Local"
  | Global -> "Global"
  | Directed -> "Directed"

let string_of_wtype (w : wtype) : string =
  match w with
  | W_I -> "Int"
  | W_Cap -> "Cap"
  | W_SealRange -> "SealRange"
  | W_Sealed -> "Sealed"

let string_of_reg_or_const (c: reg_or_const) : string =
  match c with
  | Register r -> string_of_regname r
  | Const c -> (Z.to_string c)

let string_of_reg_or_const_restrict (c: reg_or_const) : string =
  match c with
  | Register r -> string_of_regname r
  | Const c ->
    let (dec_type, dec_z) = Encode.decode_const c in
    try
      if (dec_type = Encode._PERM_LOC_ENC)
      then
        let (p,g) = (Encode.perm_loc_pair_decoding dec_z) in
        "(" ^ (string_of_perm p) ^ "," ^- (string_of_locality g) ^ ")"
      else if (dec_type = Encode._SEAL_LOC_ENC)
      then
        let (p,g) = (Encode.seal_perm_loc_pair_decoding dec_z) in
        "(" ^ (string_of_seal_perm p) ^ "," ^- (string_of_locality g) ^ ")"
      else if (dec_type = Encode._PERM_ENC)
      then
        let p = (Encode.perm_decoding dec_z) in
        string_of_perm p
      else if (dec_type = Encode._SEAL_PERM_ENC)
      then
        let p = (Encode.seal_perm_decoding dec_z) in
        string_of_seal_perm p
      else (Z.to_string c)
    with | Encode.DecodeException _ -> (Z.to_string c)

let string_of_machine_op (s: machine_op): string =
  let string_of_rr r1 r2 =
    string_of_regname r1 ^- string_of_regname r2
  and string_of_rrr r1 r2 r3 =
    string_of_regname r1 ^- string_of_regname r2 ^- string_of_regname r3
  and string_of_rc r c =
    string_of_regname r ^- string_of_reg_or_const c
  and string_of_rcc r c1 c2  =
    string_of_regname r ^- string_of_reg_or_const c1 ^- string_of_reg_or_const c2
  and string_of_rrc r1 r2 c  =
    string_of_regname r1 ^- string_of_regname r2 ^- string_of_reg_or_const c
  in match s with
  | Jmp r -> "jmp" ^- string_of_regname r
  | Jnz (r1, r2) -> "jnz" ^- string_of_rr r1 r2
  | Move (r, c) -> "mov" ^- string_of_rc r c
  | Load (r1, r2) -> "load" ^- string_of_rr r1 r2
  | Store (r, c) -> "store" ^- string_of_rc r c
  | Add (r, c1, c2) -> "add" ^- string_of_rcc r c1 c2
  | Sub (r, c1, c2) -> "sub" ^- string_of_rcc r c1 c2
  | Mul (r, c1, c2) -> "mul" ^- string_of_rcc r c1 c2
  | Rem (r, c1, c2) -> "rem" ^- string_of_rcc r c1 c2
  | Div (r, c1, c2) -> "div" ^- string_of_rcc r c1 c2
  | Lt (r, c1, c2) -> "lt" ^- string_of_rcc r c1 c2
  | Lea (r, c) -> "lea" ^- string_of_rc r c
  (* NOTE Restrict is a special case, because we know that we are supposed to restrict with a
   permission, we can try to decode it *)
  | Restrict (r, c) -> "restrict" ^- string_of_regname r ^- string_of_reg_or_const_restrict c
  | SubSeg (r, c1, c2) -> "subseg" ^- string_of_rcc r c1 c2
  | GetL (r1, r2) -> "getl" ^- string_of_rr r1 r2
  | GetB (r1, r2) -> "getb" ^- string_of_rr r1 r2
  | GetE (r1, r2) -> "gete" ^- string_of_rr r1 r2
  | GetA (r1, r2) -> "geta" ^- string_of_rr r1 r2
  | GetP (r1, r2) -> "getp" ^- string_of_rr r1 r2
  | GetOType (r1, r2) -> "getotype" ^- string_of_rr r1 r2
  | GetWType (r1, r2) -> "getwtype" ^- string_of_rr r1 r2
  | Seal (r1, r2, r3) -> "seal" ^- string_of_rrr r1 r2 r3
  | UnSeal (r1, r2, r3) -> "unseal" ^- string_of_rrr r1 r2 r3
  | LoadU (r1, r2, c) -> "loadU" ^- string_of_rrc r1 r2 c
  | StoreU (r, c1, c2) -> "storeU" ^- string_of_rcc r c1 c2
  | PromoteU r -> "promoteU" ^- string_of_regname r
  | Fail -> "fail"
  | Halt -> "halt"

let string_of_sealable (sb : sealable) : string =
  let open Big_int_Z in
  match sb with
  | Cap (p, g, b, e, a) ->
    Printf.sprintf "(%s, %s, %s, %s, %s)" (string_of_perm p)
      (string_of_locality g)
      (string_of_big_int b)
      (Infinite_z.to_string e)
      (string_of_big_int a)
  | SealRange (p, g, b, e, a) ->
    Printf.sprintf "[%s, %s, %s, %s, %s]" (string_of_seal_perm p)
      (string_of_locality g)
      (string_of_big_int b)
      (string_of_big_int e)
      (string_of_big_int a)

let string_of_word (w : word) : string =
  let open Big_int_Z in
  match w with
  | I z -> Printf.sprintf "%s" (string_of_big_int z)
  | Sealable sb -> string_of_sealable sb
  | Sealed (ot, sb) ->
    Printf.sprintf "{%s: %s}" (string_of_big_int ot) (string_of_sealable sb)

let pp_raw_word (w : word) : string = string_of_word w
let pp_asm_word ( w : word ) : string =
  let open Big_int_Z in
  match w with
  | I z -> (string_of_machine_op (Encode.decode_machine_op z))
  | Sealable sb -> Printf.sprintf "#%s" (string_of_sealable sb)
  | Sealed (ot, sb) ->
    Printf.sprintf "#{%s: %s}" (string_of_big_int ot) (string_of_sealable sb)

let string_of_statement (s : statement) : string =
  match s with
  | Op op -> string_of_machine_op op
  | Ast.Word w -> string_of_word w

let string_of_reg_word (r : regname) (w : word) : string =
  Printf.sprintf "| %s : %s |" (string_of_regname r) (string_of_word w)

let string_of_exec_state (st : Machine.exec_state) : string =
  match st with
  | Running -> "Running"
  | Halted -> "Halted"
  | Failed -> "Failed"
