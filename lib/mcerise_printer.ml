open Mcerise_ast
let permission = function
  | O -> "O" | E -> "E" | RO -> "RO" | RX -> "RX" | RW -> "RW" | RWX -> "RWX"
  | RWL -> "RWL" | RWLX -> "RWLX" | URW -> "URW" | URWX -> "URWX"
  | URWL -> "URWL" | URWLX -> "URWLX"
let locality = function Global -> "GLOBAL" | Local -> "LOCAL" | Directed -> "DIRECTED"
let capability ((Cap (p,l,b,e,a)) : capability) =
  Printf.sprintf "(%s, %s, %s, %s, %s)" (permission p) (locality l)
    (Z.to_string b) (Z.to_string e) (Z.to_string a)
let word = function I z -> Z.to_string z | Cap c -> capability c
