open Ast

let permission = function
  | O -> "O"
  | E -> "E"
  | RO -> "RO"
  | RX -> "RX"
  | RW -> "RW"
  | RWX -> "RWX"
  | RWL -> "RWL"
  | RWLX -> "RWLX"
  | URW -> "URW"
  | URWL -> "URWL"
  | URWX -> "URWX"
  | URWLX -> "URWLX"

let locality = function Global -> "GLOBAL" | Local -> "LOCAL" | Directed -> "DIRECTED"

let seal_permission = function
  | false, false -> "SO"
  | true, false -> "S"
  | false, true -> "U"
  | true, true -> "SU"

let sealable = function
  | Cap (p, l, b, e, a) ->
      Printf.sprintf "(%s, %s, %s, %s, %s)" (permission p) (locality l) (Z.to_string b)
        (Z.to_string e) (Z.to_string a)
  | SealRange (p, l, b, e, a) ->
      Printf.sprintf "[%s, %s, %s, %s, %s]" (seal_permission p) (locality l) (Z.to_string b)
        (Z.to_string e) (Z.to_string a)

let word = function
  | I z -> Z.to_string z
  | Sealable s -> sealable s
  | Sealed (o, s) -> Printf.sprintf "{%s: %s}" (Z.to_string o) (sealable s)
