(** Canonical textual rendering for Cerisier machine values and instructions. *)

open Ast

let permission (rendered_value : permission) : string =
  match rendered_value with
  | O -> "O"
  | E -> "E"
  | RO -> "RO"
  | RX -> "RX"
  | RW -> "RW"
  | RWX -> "RWX"

let seal_permission (rendered_value : bool * bool) : string =
  match rendered_value with
  | false, false -> "SO"
  | true, false -> "S"
  | false, true -> "U"
  | true, true -> "SU"

let sealable (rendered_value : sealable) : string =
  match rendered_value with
  | Cap (p, b, e, a) ->
      Printf.sprintf "(%s, %s, %s, %s)" (permission p) (Z.to_string b) (Z.to_string e)
        (Z.to_string a)
  | SealRange (p, b, e, a) ->
      Printf.sprintf "[%s, %s, %s, %s]" (seal_permission p) (Z.to_string b) (Z.to_string e)
        (Z.to_string a)

let word (rendered_value : word) : string =
  match rendered_value with
  | I z -> Z.to_string z
  | Sealable s -> sealable s
  | Sealed (o, s) -> Printf.sprintf "{%s: %s}" (Z.to_string o) (sealable s)

let register (rendered_value : register) : string =
  match rendered_value with PC -> "pc" | Reg n -> "r" ^ string_of_int n

let operand (rendered_value : reg_or_const) : string =
  match rendered_value with Register r -> register r | Constant z -> Z.to_string z

let instruction (rendered_value : instruction) : string =
  match rendered_value with
  | Jmp r -> "jmp " ^ register r
  | Jnz (a, b) -> Printf.sprintf "jnz %s %s" (register a) (register b)
  | Move (r, a) -> Printf.sprintf "mov %s %s" (register r) (operand a)
  | Load (a, b) -> Printf.sprintf "load %s %s" (register a) (register b)
  | Store (a, b) -> Printf.sprintf "store %s %s" (register a) (operand b)
  | Add (r, a, b) -> Printf.sprintf "add %s %s %s" (register r) (operand a) (operand b)
  | Sub (r, a, b) -> Printf.sprintf "sub %s %s %s" (register r) (operand a) (operand b)
  | Mul (r, a, b) -> Printf.sprintf "mul %s %s %s" (register r) (operand a) (operand b)
  | Rem (r, a, b) -> Printf.sprintf "rem %s %s %s" (register r) (operand a) (operand b)
  | Div (r, a, b) -> Printf.sprintf "div %s %s %s" (register r) (operand a) (operand b)
  | Lt (r, a, b) -> Printf.sprintf "lt %s %s %s" (register r) (operand a) (operand b)
  | Lea (r, a) -> Printf.sprintf "lea %s %s" (register r) (operand a)
  | Restrict (r, a) -> Printf.sprintf "restrict %s %s" (register r) (operand a)
  | SubSeg (r, a, b) -> Printf.sprintf "subseg %s %s %s" (register r) (operand a) (operand b)
  | GetB (a, b) -> Printf.sprintf "getb %s %s" (register a) (register b)
  | GetE (a, b) -> Printf.sprintf "gete %s %s" (register a) (register b)
  | GetA (a, b) -> Printf.sprintf "geta %s %s" (register a) (register b)
  | GetP (a, b) -> Printf.sprintf "getp %s %s" (register a) (register b)
  | GetOType (a, b) -> Printf.sprintf "getotype %s %s" (register a) (register b)
  | GetWType (a, b) -> Printf.sprintf "getwtype %s %s" (register a) (register b)
  | Seal (a, b, c) -> Printf.sprintf "seal %s %s %s" (register a) (register b) (register c)
  | UnSeal (a, b, c) -> Printf.sprintf "unseal %s %s %s" (register a) (register b) (register c)
  | Invoke (a, b) -> Printf.sprintf "invoke %s %s" (register a) (register b)
  | Hash (a, b) -> Printf.sprintf "hash %s %s" (register a) (register b)
  | HashConcat (a, b, c) ->
      Printf.sprintf "hashconcat %s %s %s" (register a) (operand b) (operand c)
  | EInit (a, b) -> Printf.sprintf "einit %s %s" (register a) (register b)
  | EDeInit a -> Printf.sprintf "edeinit %s" (register a)
  | EStoreId (a, b) -> Printf.sprintf "estoreid %s %s" (register a) (register b)
  | IsUnique (a, b) -> Printf.sprintf "isunique %s %s" (register a) (register b)
  | Fail -> "fail"
  | Halt -> "halt"
