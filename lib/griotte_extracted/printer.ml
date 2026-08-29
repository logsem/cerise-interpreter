open Ast

let register = function
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

let system_register = function MTDC -> "mtdc"
let rx_permission = function Orx -> "Orx" | R -> "R" | X -> "X" | XSR -> "XSR"
let write_permission = function Ow -> "Ow" | W -> "W" | WL -> "WL"
let deep_local_permission = function DL -> "DL" | LG -> "LG"
let deep_read_only_permission = function DRO -> "DRO" | LM -> "LM"

let permission (rx, w, dl, dro) =
  Printf.sprintf "[%s %s %s %s]" (rx_permission rx) (write_permission w) (deep_local_permission dl)
    (deep_read_only_permission dro)

let locality = function Global -> "Global" | Local -> "Local"

let seal_permission = function
  | false, false -> "SO"
  | true, false -> "S"
  | false, true -> "U"
  | true, true -> "SU"

let word_type = function
  | W_I -> "Int"
  | W_Cap -> "Cap"
  | W_SealRange -> "SealRange"
  | W_Sealed -> "Sealed"
  | W_Sentry -> "Sentry"

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
  | Sentry (p, l, b, e, a) ->
      Printf.sprintf "(E-%s, %s, %s, %s, %s)" (permission p) (locality l) (Z.to_string b)
        (Z.to_string e) (Z.to_string a)
  | Sealed (otype, s) -> Printf.sprintf "{%s: %s}" (Z.to_string otype) (sealable s)
