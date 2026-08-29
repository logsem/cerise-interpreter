open Vanilla_ast

let register_codec =
  Instruction_codec.scalar_codec ~name:"vanilla register"
    ~encode:(function
      | PC -> Ok Z.zero
      | Reg n when n >= 0 && n <= 31 -> Ok (Z.of_int (n + 1))
      | Reg _ -> Error "register number must be between 0 and 31")
    ~decode:(fun value ->
      if Z.equal value Z.zero then Ok PC
      else if Z.fits_int value then
        let n = Z.to_int value - 1 in
        if n >= 0 && n <= 31 then Ok (Reg n) else Error "invalid register encoding"
      else Error "invalid register encoding")

let operand = Instruction_codec.register_or_constant register_codec Instruction_codec.zarith

let from_operand = function
  | Instruction_codec.Register r -> Register r
  | Instruction_codec.Constant z -> Constant z

let to_operand = function
  | Register r -> Instruction_codec.Register r
  | Constant z -> Instruction_codec.Constant z

let r = Instruction_codec.register register_codec
let rr = Instruction_codec.pair r r
let ro = Instruction_codec.pair r operand
let roo = Instruction_codec.triple r operand operand
let rrr = Instruction_codec.triple r r r

let unit_case name construct project =
  Instruction_codec.case ~name Instruction_codec.unit
    ~construct:(fun () -> construct)
    ~project:(fun x -> if project x then Some () else None)

let cases =
  [
    Instruction_codec.case ~name:"Jmp" r
      ~construct:(fun x -> Jmp x)
      ~project:(function Jmp x -> Some x | _ -> None);
    Instruction_codec.case ~name:"Jnz" rr
      ~construct:(fun (a, b) -> Jnz (a, b))
      ~project:(function Jnz (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.case ~name:"Move" ro
      ~construct:(fun (a, b) -> Move (a, from_operand b))
      ~project:(function Move (a, b) -> Some (a, to_operand b) | _ -> None);
    Instruction_codec.case ~name:"Load" rr
      ~construct:(fun (a, b) -> Load (a, b))
      ~project:(function Load (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.case ~name:"Store" ro
      ~construct:(fun (a, b) -> Store (a, from_operand b))
      ~project:(function Store (a, b) -> Some (a, to_operand b) | _ -> None);
    Instruction_codec.case ~name:"Add" roo
      ~construct:(fun (a, b, c) -> Add (a, from_operand b, from_operand c))
      ~project:(function Add (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    Instruction_codec.case ~name:"Sub" roo
      ~construct:(fun (a, b, c) -> Sub (a, from_operand b, from_operand c))
      ~project:(function Sub (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    Instruction_codec.case ~name:"Mul" roo
      ~construct:(fun (a, b, c) -> Mul (a, from_operand b, from_operand c))
      ~project:(function Mul (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    Instruction_codec.case ~name:"Rem" roo
      ~construct:(fun (a, b, c) -> Rem (a, from_operand b, from_operand c))
      ~project:(function Rem (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    Instruction_codec.case ~name:"Div" roo
      ~construct:(fun (a, b, c) -> Div (a, from_operand b, from_operand c))
      ~project:(function Div (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    Instruction_codec.case ~name:"Lt" roo
      ~construct:(fun (a, b, c) -> Lt (a, from_operand b, from_operand c))
      ~project:(function Lt (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    Instruction_codec.case ~name:"Lea" ro
      ~construct:(fun (a, b) -> Lea (a, from_operand b))
      ~project:(function Lea (a, b) -> Some (a, to_operand b) | _ -> None);
    Instruction_codec.case ~name:"Restrict" ro
      ~construct:(fun (a, b) -> Restrict (a, from_operand b))
      ~project:(function Restrict (a, b) -> Some (a, to_operand b) | _ -> None);
    Instruction_codec.case ~name:"SubSeg" roo
      ~construct:(fun (a, b, c) -> SubSeg (a, from_operand b, from_operand c))
      ~project:(function SubSeg (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    Instruction_codec.case ~name:"GetB" rr
      ~construct:(fun (a, b) -> GetB (a, b))
      ~project:(function GetB (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.case ~name:"GetE" rr
      ~construct:(fun (a, b) -> GetE (a, b))
      ~project:(function GetE (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.case ~name:"GetA" rr
      ~construct:(fun (a, b) -> GetA (a, b))
      ~project:(function GetA (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.case ~name:"GetP" rr
      ~construct:(fun (a, b) -> GetP (a, b))
      ~project:(function GetP (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.case ~name:"GetOType" rr
      ~construct:(fun (a, b) -> GetOType (a, b))
      ~project:(function GetOType (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.case ~name:"GetWType" rr
      ~construct:(fun (a, b) -> GetWType (a, b))
      ~project:(function GetWType (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.case ~name:"Seal" rrr
      ~construct:(fun (a, b, c) -> Seal (a, b, c))
      ~project:(function Seal (a, b, c) -> Some (a, b, c) | _ -> None);
    Instruction_codec.case ~name:"UnSeal" rrr
      ~construct:(fun (a, b, c) -> UnSeal (a, b, c))
      ~project:(function UnSeal (a, b, c) -> Some (a, b, c) | _ -> None);
    Instruction_codec.case ~name:"Invoke" rr
      ~construct:(fun (a, b) -> Invoke (a, b))
      ~project:(function Invoke (a, b) -> Some (a, b) | _ -> None);
    unit_case "Fail" Fail (function Fail -> true | _ -> false);
    unit_case "Halt" Halt (function Halt -> true | _ -> false);
  ]

let table =
  match Instruction_codec.compile cases with
  | Ok t -> t
  | Error errors -> failwith (String.concat "; " (List.map Instruction_codec.error_message errors))

let encode = Instruction_codec.encode table
let decode = Instruction_codec.decode table
let allocations = Instruction_codec.allocations table
let encode_tag tag scalar = Z.logor (Z.of_int tag) (Z.shift_left scalar 3)
let permission_scalar = function O -> 0 | E -> 1 | RO -> 4 | RX -> 5 | RW -> 6 | RWX -> 7
let encode_permission p = encode_tag 0 (Z.of_int (permission_scalar p))

let decode_permission z =
  if Z.sign z < 0 || not (Z.equal (Z.extract z 0 3) Z.zero) then
    Error "not a vanilla permission encoding"
  else
    match Z.shift_right z 3 with
    | payload when Z.equal payload Z.zero -> Ok O
    | payload when Z.equal payload Z.one -> Ok E
    | payload when Z.equal payload (Z.of_int 4) -> Ok RO
    | payload when Z.equal payload (Z.of_int 5) -> Ok RX
    | payload when Z.equal payload (Z.of_int 6) -> Ok RW
    | payload when Z.equal payload (Z.of_int 7) -> Ok RWX
    | _ -> Error "unknown vanilla permission"

let encode_seal_permission (s, u) =
  encode_tag 1 (Z.of_int ((if s then 2 else 0) + if u then 1 else 0))

let decode_seal_permission z =
  if Z.sign z < 0 || not (Z.equal (Z.extract z 0 3) (Z.of_int 1)) then
    Error "not a seal permission encoding"
  else
    match Z.shift_right z 3 with
    | payload when Z.equal payload Z.zero -> Ok (false, false)
    | payload when Z.equal payload Z.one -> Ok (false, true)
    | payload when Z.equal payload (Z.of_int 2) -> Ok (true, false)
    | payload when Z.equal payload (Z.of_int 3) -> Ok (true, true)
    | _ -> Error "unknown seal permission"

let encode_word_type = function
  | Integer -> encode_tag 3 Z.zero
  | Capability -> encode_tag 3 Z.one
  | Seal_range -> encode_tag 3 (Z.of_int 2)
  | Sealed -> encode_tag 3 (Z.of_int 3)
