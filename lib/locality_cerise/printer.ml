open Ast

let permission (matched_value : permission) : string = match matched_value with
  | O -> "O"
  | E -> "E"
  | RO -> "RO"
  | RX -> "RX"
  | RW -> "RW"
  | RWX -> "RWX"
  | RWL -> "RWL"
  | RWLX -> "RWLX"

let locality (matched_value : locality) : string = match matched_value with Global -> "GLOBAL" | Local -> "LOCAL"

let seal_permission (matched_value : bool * bool) : string = match matched_value with
  | false, false -> "SO"
  | true, false -> "S"
  | false, true -> "U"
  | true, true -> "SU"

let sealable (matched_value : sealable) : string = match matched_value with
  | Cap (p, l, b, e, a) ->
      Printf.sprintf "(%s, %s, %s, %s, %s)" (permission p) (locality l) (Z.to_string b)
        (Z.to_string e) (Z.to_string a)
  | SealRange (p, l, b, e, a) ->
      Printf.sprintf "[%s, %s, %s, %s, %s]" (seal_permission p) (locality l) (Z.to_string b)
        (Z.to_string e) (Z.to_string a)

let word (matched_value : word) : string = match matched_value with
  | I z -> Z.to_string z
  | Sealable s -> sealable s
  | Sealed (o, s) -> Printf.sprintf "{%s: %s}" (Z.to_string o) (sealable s)

let register (matched_value : register) : string = match matched_value with PC -> "pc" | Reg n -> "r" ^ string_of_int n
let operand (matched_value : reg_or_const) : string = match matched_value with Register r -> register r | Constant z -> Z.to_string z

let instruction (matched_value : instruction) : string = match matched_value with
  | Jmp r -> Printf.sprintf "jmp %s" (register r)
  | Jnz (a, b) -> Printf.sprintf "jnz %s %s" (register a) (register b)
  | Move (a, b) -> Printf.sprintf "mov %s %s" (register a) (operand b)
  | Load (a, b) -> Printf.sprintf "load %s %s" (register a) (register b)
  | Store (a, b) -> Printf.sprintf "store %s %s" (register a) (operand b)
  | Add (a, b, c) -> Printf.sprintf "add %s %s %s" (register a) (operand b) (operand c)
  | Sub (a, b, c) -> Printf.sprintf "sub %s %s %s" (register a) (operand b) (operand c)
  | Mul (a, b, c) -> Printf.sprintf "mul %s %s %s" (register a) (operand b) (operand c)
  | Rem (a, b, c) -> Printf.sprintf "rem %s %s %s" (register a) (operand b) (operand c)
  | Div (a, b, c) -> Printf.sprintf "div %s %s %s" (register a) (operand b) (operand c)
  | Lt (a, b, c) -> Printf.sprintf "lt %s %s %s" (register a) (operand b) (operand c)
  | Lea (a, b) -> Printf.sprintf "lea %s %s" (register a) (operand b)
  | Restrict (a, b) -> Printf.sprintf "restrict %s %s" (register a) (operand b)
  | SubSeg (a, b, c) -> Printf.sprintf "subseg %s %s %s" (register a) (operand b) (operand c)
  | GetL (a, b) -> Printf.sprintf "getl %s %s" (register a) (register b)
  | GetB (a, b) -> Printf.sprintf "getb %s %s" (register a) (register b)
  | GetE (a, b) -> Printf.sprintf "gete %s %s" (register a) (register b)
  | GetA (a, b) -> Printf.sprintf "geta %s %s" (register a) (register b)
  | GetP (a, b) -> Printf.sprintf "getp %s %s" (register a) (register b)
  | GetOType (a, b) -> Printf.sprintf "getotype %s %s" (register a) (register b)
  | GetWType (a, b) -> Printf.sprintf "getwtype %s %s" (register a) (register b)
  | Seal (a, b, c) -> Printf.sprintf "seal %s %s %s" (register a) (register b) (register c)
  | UnSeal (a, b, c) -> Printf.sprintf "unseal %s %s %s" (register a) (register b) (register c)
  | Invoke (a, b) -> Printf.sprintf "invoke %s %s" (register a) (register b)
  | Fail -> "fail"
  | Halt -> "halt"
