(* Render Griotte values and instructions in the canonical assembly spelling used
   by diagnostics, editable machine views, and round-trip tests. *)

open Ast

let register (value : register) : string =
  match value with
  | PC -> "pc"
  | Reg n -> (
      match n with
      | 0 -> "cnull"
      | 1 -> "cra"
      | 2 -> "csp"
      | 3 -> "cgp"
      | 4 -> "ctp"
      | 5 -> "ct0"
      | 6 -> "ct1"
      | 7 -> "ct2"
      | 28 -> "ct3"
      | 29 -> "ct4"
      | 30 -> "ct5"
      | 31 -> "ct6"
      | 8 -> "cs0"
      | 9 -> "cs1"
      | 18 -> "cs2"
      | 19 -> "cs3"
      | 20 -> "cs4"
      | 21 -> "cs5"
      | 22 -> "cs6"
      | 23 -> "cs7"
      | 24 -> "cs8"
      | 25 -> "cs9"
      | 26 -> "cs10"
      | 27 -> "cs11"
      | 10 -> "ca0"
      | 11 -> "ca1"
      | 12 -> "ca2"
      | 13 -> "ca3"
      | 14 -> "ca4"
      | 15 -> "ca5"
      | 16 -> "ca6"
      | 17 -> "ca7"
      | n -> "r" ^ string_of_int n)

let system_register (value : system_register) : string = match value with MTDC -> "mtdc"

let rx_permission (value : rx_permission) : string =
  match value with Orx -> "Orx" | R -> "R" | X -> "X" | XSR -> "XSR"

let write_permission (value : write_permission) : string =
  match value with Ow -> "Ow" | W -> "W" | WL -> "WL"

let deep_local_permission (value : deep_local_permission) : string =
  match value with DL -> "DL" | LG -> "LG"

let deep_read_only_permission (value : deep_read_only_permission) : string =
  match value with DRO -> "DRO" | LM -> "LM"

let permission
    ((rx, w, dl, dro) :
      rx_permission * write_permission * deep_local_permission * deep_read_only_permission) : string
    =
  Printf.sprintf "[%s %s %s %s]" (rx_permission rx) (write_permission w) (deep_local_permission dl)
    (deep_read_only_permission dro)

let locality (value : locality) : string = match value with Global -> "Global" | Local -> "Local"

let seal_permission (value : bool * bool) : string =
  match value with
  | false, false -> "SO"
  | true, false -> "S"
  | false, true -> "U"
  | true, true -> "SU"

let word_type (value : word_type) : string =
  match value with
  | W_I -> "Int"
  | W_Cap -> "Cap"
  | W_SealRange -> "SealRange"
  | W_Sealed -> "Sealed"
  | W_Sentry -> "Sentry"

let sealable (value : sealable) : string =
  match value with
  | Cap (p, l, b, e, a) ->
      Printf.sprintf "(%s, %s, %s, %s, %s)" (permission p) (locality l) (Z.to_string b)
        (Z.to_string e) (Z.to_string a)
  | SealRange (p, l, b, e, a) ->
      Printf.sprintf "[%s, %s, %s, %s, %s]" (seal_permission p) (locality l) (Z.to_string b)
        (Z.to_string e) (Z.to_string a)

let word (value : word) : string =
  match value with
  | I z -> Z.to_string z
  | Sealable s -> sealable s
  | Sentry (p, l, b, e, a) ->
      Printf.sprintf "(E-%s, %s, %s, %s, %s)" (permission p) (locality l) (Z.to_string b)
        (Z.to_string e) (Z.to_string a)
  | Sealed (otype, s) -> Printf.sprintf "{%s: %s}" (Z.to_string otype) (sealable s)

let operand (value : reg_or_const) : string =
  match value with Register r -> register r | Constant z -> Z.to_string z

let instruction (value : instruction) : string =
  match value with
  | Jalr (a, b) -> Printf.sprintf "jalr %s %s" (register a) (register b)
  | Jmp a -> Printf.sprintf "jmp %s" (operand a)
  | Jnz (a, b) -> Printf.sprintf "jnz %s %s" (register a) (operand b)
  | ReadSR (a, b) -> Printf.sprintf "readsr %s %s" (register a) (system_register b)
  | WriteSR (a, b) -> Printf.sprintf "writesr %s %s" (system_register a) (register b)
  | Move (a, b) -> Printf.sprintf "mov %s %s" (register a) (operand b)
  | Load (a, b) -> Printf.sprintf "load %s %s" (register a) (register b)
  | Store (a, b) -> Printf.sprintf "store %s %s" (register a) (operand b)
  | Add (a, b, c) -> Printf.sprintf "add %s %s %s" (register a) (operand b) (operand c)
  | Sub (a, b, c) -> Printf.sprintf "sub %s %s %s" (register a) (operand b) (operand c)
  | Mul (a, b, c) -> Printf.sprintf "mul %s %s %s" (register a) (operand b) (operand c)
  | LAnd (a, b, c) -> Printf.sprintf "land %s %s %s" (register a) (operand b) (operand c)
  | LOr (a, b, c) -> Printf.sprintf "lor %s %s %s" (register a) (operand b) (operand c)
  | LShiftL (a, b, c) -> Printf.sprintf "lshiftl %s %s %s" (register a) (operand b) (operand c)
  | LShiftR (a, b, c) -> Printf.sprintf "lshiftr %s %s %s" (register a) (operand b) (operand c)
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
  | Fail -> "fail"
  | Halt -> "halt"
