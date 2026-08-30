(** Stable textual rendering for concrete mCerise values and instructions. The backend view reuses
    these strings for editing, inspection, and decoding. *)

open Ast

let permission (value : permission) : string =
  match value with
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

let locality (value : locality) : string =
  match value with Global -> "GLOBAL" | Local -> "LOCAL" | Directed -> "DIRECTED"

let capability (Cap (p, l, b, e, a) : capability) : string =
  Printf.sprintf "(%s, %s, %s, %s, %s)" (permission p) (locality l) (Z.to_string b) (Z.to_string e)
    (Z.to_string a)

let word (value : word) : string = match value with I z -> Z.to_string z | Cap c -> capability c

let register (value : register) : string =
  match value with PC -> "pc" | Reg n -> "r" ^ string_of_int n

let operand (value : reg_or_const) : string =
  match value with Register r -> register r | Constant z -> Z.to_string z

let instruction (value : instruction) : string =
  match value with
  | Jmp r -> Printf.sprintf "jmp %s" (register r)
  | Jnz (a, b) -> Printf.sprintf "jnz %s %s" (register a) (register b)
  | Move (a, b) -> Printf.sprintf "mov %s %s" (register a) (operand b)
  | Load (a, b) -> Printf.sprintf "load %s %s" (register a) (register b)
  | Store (a, b) -> Printf.sprintf "store %s %s" (register a) (operand b)
  | Add (a, b, c) -> Printf.sprintf "add %s %s %s" (register a) (operand b) (operand c)
  | Sub (a, b, c) -> Printf.sprintf "sub %s %s %s" (register a) (operand b) (operand c)
  | Lt (a, b, c) -> Printf.sprintf "lt %s %s %s" (register a) (operand b) (operand c)
  | Lea (a, b) -> Printf.sprintf "lea %s %s" (register a) (operand b)
  | Restrict (a, b) -> Printf.sprintf "restrict %s %s" (register a) (operand b)
  | SubSeg (a, b, c) -> Printf.sprintf "subseg %s %s %s" (register a) (operand b) (operand c)
  | IsPtr (a, b) -> Printf.sprintf "isptr %s %s" (register a) (register b)
  | GetP (a, b) -> Printf.sprintf "getp %s %s" (register a) (register b)
  | GetL (a, b) -> Printf.sprintf "getl %s %s" (register a) (register b)
  | GetB (a, b) -> Printf.sprintf "getb %s %s" (register a) (register b)
  | GetE (a, b) -> Printf.sprintf "gete %s %s" (register a) (register b)
  | GetA (a, b) -> Printf.sprintf "geta %s %s" (register a) (register b)
  | LoadU (a, b, c) -> Printf.sprintf "loadu %s %s %s" (register a) (register b) (operand c)
  | StoreU (a, b, c) -> Printf.sprintf "storeu %s %s %s" (register a) (operand b) (operand c)
  | PromoteU a -> Printf.sprintf "promoteu %s" (register a)
  | Fail -> "fail"
  | Halt -> "halt"
