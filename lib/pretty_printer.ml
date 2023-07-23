open Ast
open Machine

let (^-) s1 s2 = s1 ^ " " ^ s2

let string_of_regname (r: regname) : string =
  match r with
  | PC -> "pc"
  | Reg i -> "r" ^ (string_of_int i)

let string_of_perm (p: perm): string =
  match p with
  | O -> "O"
  | E -> "E"
  | RO -> "RO"
  | RX -> "RX"
  | RW -> "RW"
  | RWX -> "RWX"

let string_of_reg_or_const (c: reg_or_const) : string =
  match c with
  | Register r -> string_of_regname r
  | CP (Const c) -> (Z.to_string c)
  | CP (Perm p) -> string_of_perm p

let string_of_machine_op (s: machine_op): string =
  let string_of_rr r1 r2 =
    string_of_regname r1 ^- string_of_regname r2
  and string_of_rc r c =
    string_of_regname r ^- string_of_reg_or_const c
  and string_of_rcc r c1 c2  =
    string_of_regname r ^- string_of_reg_or_const c1 ^- string_of_reg_or_const c2
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
  | Restrict (r, c) -> "restrict" ^- string_of_rc r c
  | SubSeg (r, c1, c2) -> "subseg" ^- string_of_rcc r c1 c2
  | IsPtr (r1, r2) -> "isptr" ^- string_of_rr r1 r2
  | GetP (r1, r2) -> "getp" ^- string_of_rr r1 r2
  | GetB (r1, r2) -> "getb" ^- string_of_rr r1 r2
  | GetE (r1, r2) -> "gete" ^- string_of_rr r1 r2
  | GetA (r1, r2) -> "geta" ^- string_of_rr r1 r2
  | Fail -> "fail"
  | Halt -> "halt"

let string_of_word (w : word) : string =
  match w with
  | Cap (p, b, e, a) ->
    Printf.sprintf "Cap (%s, %s, %s, %s)" (string_of_perm p) (Z.to_string b) (Z.to_string e) (Z.to_string a)
  | I z -> Z.to_string z

let string_of_ast_word (w : Ast.word) : string =
  match w with
  | Ast.Cap (p, b, e, a) ->
    Printf.sprintf "Cap (%s,  %s, %s, %s)"
    (string_of_perm p) (Z.to_string b) (Z.to_string e) (Z.to_string a)
  | Ast.I z -> Z.to_string z

let string_of_statement (s : statement) : string =
  match s with
  | Op op -> string_of_machine_op op
  | Ast.Word w -> string_of_ast_word w

let string_of_reg_word (r : regname) (w : word) : string =
  Printf.sprintf "| %s : %s |" (string_of_regname r) (string_of_word w)

let string_of_exec_state (st : exec_state) : string =
  match st with
  | Running -> "Running"
  | Halted -> "Halted"
  | Failed -> "Failed"
