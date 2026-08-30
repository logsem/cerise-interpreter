open Ast

let register_codec =
  Instruction_codec.scalar_codec ~name:"Cerisier register"
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

let from_operand (matched_value : (register, Z.t) Instruction_codec.register_or_constant) : reg_or_const = match matched_value with
  | Instruction_codec.Register r -> Register r
  | Instruction_codec.Constant z -> Const z

let to_operand (matched_value : reg_or_const) : (register, Z.t) Instruction_codec.register_or_constant = match matched_value with
  | Register r -> Instruction_codec.Register r
  | Const z -> Instruction_codec.Constant z

let r = Instruction_codec.register register_codec
let rr = Instruction_codec.pair r r
let ro = Instruction_codec.pair r operand
let roo = Instruction_codec.triple r operand operand
let rrr = Instruction_codec.triple r r r
let rrc = Instruction_codec.pair rr operand

let fixed_opcode (matched_value : string) : int = match matched_value with
  | "Jmp" -> 0x00
  | "Jnz" -> 0x01
  | "Move" -> 0x02
  | "Load" -> 0x04
  | "Store" -> 0x05
  | "Add" -> 0x07
  | "Sub" -> 0x0b
  | "Mul" -> 0x0f
  | "Rem" -> 0x13
  | "Div" -> 0x17
  | "Lt" -> 0x1b
  | "Lea" -> 0x1f
  | "Restrict" -> 0x21
  | "SubSeg" -> 0x23
  | "GetL" -> 0x27
  | "GetB" -> 0x28
  | "GetE" -> 0x29
  | "GetA" -> 0x2a
  | "GetP" -> 0x2b
  | "GetOType" -> 0x2c
  | "GetWType" -> 0x2d
  | "Seal" -> 0x2e
  | "UnSeal" -> 0x2f
  | "Invoke" -> 0x30
  | "LoadU" -> 0x31
  | "StoreU" -> 0x33
  | "PromoteU" -> 0x37
  | "EInit" -> 0x38
  | "EDeInit" -> 0x39
  | "EStoreId" -> 0x3a
  | "IsUnique" -> 0x3b
  | "Fail" -> 0x3c
  | "Halt" -> 0x3d
  | name -> invalid_arg ("unknown fixed Cerisier instruction " ^ name)

let case ~name:(name : string) (shape : 'a Instruction_codec.shape) : construct:('a -> 'b) -> project:('b -> 'a option) -> 'b Instruction_codec.case = Instruction_codec.case ~name ~allocation:(Fixed (fixed_opcode name)) shape

let unit_case (name : string) (construct : 'a) (project : ('a -> bool)) : 'a Instruction_codec.case =
  case ~name Instruction_codec.unit
    ~construct:(fun () -> construct)
    ~project:(fun x -> if project x then Some () else None)

let cases =
  [
    case ~name:"Jmp" r ~construct:(fun x -> Jmp x) ~project:(function Jmp x -> Some x | _ -> None);
    case ~name:"Jnz" rr
      ~construct:(fun (a, b) -> Jnz (a, b))
      ~project:(function Jnz (a, b) -> Some (a, b) | _ -> None);
    case ~name:"Move" ro
      ~construct:(fun (a, b) -> Move (a, from_operand b))
      ~project:(function Move (a, b) -> Some (a, to_operand b) | _ -> None);
    case ~name:"Load" rr
      ~construct:(fun (a, b) -> Load (a, b))
      ~project:(function Load (a, b) -> Some (a, b) | _ -> None);
    case ~name:"Store" ro
      ~construct:(fun (a, b) -> Store (a, from_operand b))
      ~project:(function Store (a, b) -> Some (a, to_operand b) | _ -> None);
    case ~name:"Add" roo
      ~construct:(fun (a, b, c) -> Add (a, from_operand b, from_operand c))
      ~project:(function Add (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case ~name:"Sub" roo
      ~construct:(fun (a, b, c) -> Sub (a, from_operand b, from_operand c))
      ~project:(function Sub (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case ~name:"Mul" roo
      ~construct:(fun (a, b, c) -> Mul (a, from_operand b, from_operand c))
      ~project:(function Mul (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case ~name:"Rem" roo
      ~construct:(fun (a, b, c) -> Rem (a, from_operand b, from_operand c))
      ~project:(function Rem (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case ~name:"Div" roo
      ~construct:(fun (a, b, c) -> Div (a, from_operand b, from_operand c))
      ~project:(function Div (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case ~name:"Lt" roo
      ~construct:(fun (a, b, c) -> Lt (a, from_operand b, from_operand c))
      ~project:(function Lt (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case ~name:"Lea" ro
      ~construct:(fun (a, b) -> Lea (a, from_operand b))
      ~project:(function Lea (a, b) -> Some (a, to_operand b) | _ -> None);
    case ~name:"Restrict" ro
      ~construct:(fun (a, b) -> Restrict (a, from_operand b))
      ~project:(function Restrict (a, b) -> Some (a, to_operand b) | _ -> None);
    case ~name:"SubSeg" roo
      ~construct:(fun (a, b, c) -> SubSeg (a, from_operand b, from_operand c))
      ~project:(function SubSeg (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case ~name:"GetL" rr
      ~construct:(fun (a, b) -> GetL (a, b))
      ~project:(function GetL (a, b) -> Some (a, b) | _ -> None);
    case ~name:"GetB" rr
      ~construct:(fun (a, b) -> GetB (a, b))
      ~project:(function GetB (a, b) -> Some (a, b) | _ -> None);
    case ~name:"GetE" rr
      ~construct:(fun (a, b) -> GetE (a, b))
      ~project:(function GetE (a, b) -> Some (a, b) | _ -> None);
    case ~name:"GetA" rr
      ~construct:(fun (a, b) -> GetA (a, b))
      ~project:(function GetA (a, b) -> Some (a, b) | _ -> None);
    case ~name:"GetP" rr
      ~construct:(fun (a, b) -> GetP (a, b))
      ~project:(function GetP (a, b) -> Some (a, b) | _ -> None);
    case ~name:"GetOType" rr
      ~construct:(fun (a, b) -> GetOType (a, b))
      ~project:(function GetOType (a, b) -> Some (a, b) | _ -> None);
    case ~name:"GetWType" rr
      ~construct:(fun (a, b) -> GetWType (a, b))
      ~project:(function GetWType (a, b) -> Some (a, b) | _ -> None);
    case ~name:"Seal" rrr
      ~construct:(fun (a, b, c) -> Seal (a, b, c))
      ~project:(function Seal (a, b, c) -> Some (a, b, c) | _ -> None);
    case ~name:"UnSeal" rrr
      ~construct:(fun (a, b, c) -> UnSeal (a, b, c))
      ~project:(function UnSeal (a, b, c) -> Some (a, b, c) | _ -> None);
    case ~name:"Invoke" rr
      ~construct:(fun (a, b) -> Invoke (a, b))
      ~project:(function Invoke (a, b) -> Some (a, b) | _ -> None);
    case ~name:"LoadU" rrc
      ~construct:(fun ((a, b), c) -> LoadU (a, b, from_operand c))
      ~project:(function LoadU (a, b, c) -> Some ((a, b), to_operand c) | _ -> None);
    case ~name:"StoreU" roo
      ~construct:(fun (a, b, c) -> StoreU (a, from_operand b, from_operand c))
      ~project:(function StoreU (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case ~name:"PromoteU" r
      ~construct:(fun a -> PromoteU a)
      ~project:(function PromoteU a -> Some a | _ -> None);
    case ~name:"EInit" rr
      ~construct:(fun (a, b) -> EInit (a, b))
      ~project:(function EInit (a, b) -> Some (a, b) | _ -> None);
    case ~name:"EDeInit" r
      ~construct:(fun a -> EDeInit a)
      ~project:(function EDeInit a -> Some a | _ -> None);
    case ~name:"EStoreId" rr
      ~construct:(fun (a, b) -> EStoreId (a, b))
      ~project:(function EStoreId (a, b) -> Some (a, b) | _ -> None);
    case ~name:"IsUnique" rr
      ~construct:(fun (a, b) -> IsUnique (a, b))
      ~project:(function IsUnique (a, b) -> Some (a, b) | _ -> None);
    unit_case "Fail" Fail (function Fail -> true | _ -> false);
    unit_case "Halt" Halt (function Halt -> true | _ -> false);
  ]

let table =
  match Instruction_codec.compile cases with
  | Ok t -> t
  | Error errors -> failwith (String.concat "; " (List.map Instruction_codec.error_message errors))

let encode = Instruction_codec.encode table

let decode (encoded : Z.t) : (instruction, Instruction_codec.error) result =
  try Instruction_codec.decode table encoded
  with Stack_overflow ->
    Error
      (Instruction_codec.Malformed_encoding
         {
           opcode = (if Z.sign encoded < 0 then -1 else Z.to_int (Z.extract encoded 0 8));
           case_name = "Cerisier";
           message = "instruction payload nesting exceeds the decoder limit";
         })

let allocations = Instruction_codec.allocations table
let encode_tag (tag : int) (scalar : Z.t) : Z.t = Z.logor (Z.of_int tag) (Z.shift_left scalar 3)

let permission_scalar (matched_value : permission) : int = match matched_value with
  | O -> 0
  | E -> 1
  | RO -> 4
  | RX -> 5
  | RW -> 6
  | RWX -> 7
  | RWL -> 14
  | RWLX -> 15
  | URW -> 22
  | URWX -> 23
  | URWL -> 30
  | URWLX -> 31

let encode_permission (p : permission) : Z.t = encode_tag 0 (Z.of_int (permission_scalar p))

let decode_permission (z : Z.t) : (permission, string) result =
  if Z.sign z < 0 || not (Z.equal (Z.extract z 0 3) Z.zero) then
    Error "not a Cerisier permission encoding"
  else
    match Z.shift_right z 3 with
    | payload when Z.equal payload Z.zero -> Ok O
    | payload when Z.equal payload Z.one -> Ok E
    | payload when Z.equal payload (Z.of_int 4) -> Ok RO
    | payload when Z.equal payload (Z.of_int 5) -> Ok RX
    | payload when Z.equal payload (Z.of_int 6) -> Ok RW
    | payload when Z.equal payload (Z.of_int 7) -> Ok RWX
    | payload when Z.equal payload (Z.of_int 14) -> Ok RWL
    | payload when Z.equal payload (Z.of_int 15) -> Ok RWLX
    | payload when Z.equal payload (Z.of_int 22) -> Ok URW
    | payload when Z.equal payload (Z.of_int 23) -> Ok URWX
    | payload when Z.equal payload (Z.of_int 30) -> Ok URWL
    | payload when Z.equal payload (Z.of_int 31) -> Ok URWLX
    | _ -> Error "unknown Cerisier permission"

let encode_seal_permission ((s, u) : bool * bool) : Z.t =
  encode_tag 1 (Z.of_int ((if s then 2 else 0) + if u then 1 else 0))

let decode_seal_permission (z : Z.t) : (bool * bool, string) result =
  if Z.sign z < 0 || not (Z.equal (Z.extract z 0 3) (Z.of_int 1)) then
    Error "not a seal permission encoding"
  else
    match Z.shift_right z 3 with
    | payload when Z.equal payload Z.zero -> Ok (false, false)
    | payload when Z.equal payload Z.one -> Ok (false, true)
    | payload when Z.equal payload (Z.of_int 2) -> Ok (true, false)
    | payload when Z.equal payload (Z.of_int 3) -> Ok (true, true)
    | _ -> Error "unknown seal permission"

let encode_word_type (matched_value : word_type) : Z.t = match matched_value with
  | Integer -> encode_tag 3 Z.zero
  | Capability -> encode_tag 3 Z.one
  | Seal_range -> encode_tag 3 (Z.of_int 2)
  | Sealed -> encode_tag 3 (Z.of_int 3)

let decode_word_type (z : Z.t) : (word_type, string) result =
  if Z.sign z < 0 || not (Z.equal (Z.extract z 0 3) (Z.of_int 3)) then
    Error "not a word-type encoding"
  else
    match Z.shift_right z 3 with
    | payload when Z.equal payload Z.zero -> Ok Integer
    | payload when Z.equal payload Z.one -> Ok Capability
    | payload when Z.equal payload (Z.of_int 2) -> Ok Seal_range
    | payload when Z.equal payload (Z.of_int 3) -> Ok Sealed
    | _ -> Error "unknown word type"

let locality_scalar (matched_value : locality) : int = match matched_value with Directed -> 0 | Local -> 1 | Global -> 2
let encode_locality (l : locality) : Z.t = encode_tag 2 (Z.of_int (locality_scalar l))

let decode_locality (z : Z.t) : (locality, string) result =
  if Z.sign z < 0 || not (Z.equal (Z.extract z 0 3) (Z.of_int 2)) then
    Error "not a locality encoding"
  else
    match Z.shift_right z 3 with
    | payload when Z.equal payload Z.zero -> Ok Directed
    | payload when Z.equal payload Z.one -> Ok Local
    | payload when Z.equal payload (Z.of_int 2) -> Ok Global
    | _ -> Error "unknown locality"

let encode_permission_locality (p : permission) (l : locality) : Z.t =
  encode_tag 4 (Z.of_int ((locality_scalar l lsl 5) + permission_scalar p))

let decode_permission_locality (z : Z.t) : (permission * locality, string) result =
  if Z.sign z < 0 || not (Z.equal (Z.extract z 0 3) (Z.of_int 4)) then
    Error "not a permission/locality encoding"
  else
    let payload = Z.shift_right z 3 in
    if not (Z.fits_int payload) then Error "unknown permission/locality"
    else
      let payload = Z.to_int payload in
      let p = payload land 31 and l = payload lsr 5 in
      match
        ( (match p with
          | 0 -> Some O
          | 1 -> Some E
          | 4 -> Some RO
          | 5 -> Some RX
          | 6 -> Some RW
          | 7 -> Some RWX
          | 14 -> Some RWL
          | 15 -> Some RWLX
          | 22 -> Some URW
          | 23 -> Some URWX
          | 30 -> Some URWL
          | 31 -> Some URWLX
          | _ -> None),
          match l with 0 -> Some Directed | 1 -> Some Local | 2 -> Some Global | _ -> None )
      with
      | Some p, Some l -> Ok (p, l)
      | _ -> Error "unknown permission/locality"

let encode_seal_permission_locality (p : bool * bool) (l : locality) : Z.t =
  let scalar = Z.shift_right (encode_seal_permission p) 3 in
  encode_tag 5 Z.(scalar + of_int Stdlib.(locality_scalar l lsl 2))

let decode_seal_permission_locality (z : Z.t) : ((bool * bool) * locality, string) result =
  if Z.sign z < 0 || not (Z.equal (Z.extract z 0 3) (Z.of_int 5)) then
    Error "not a seal-permission/locality encoding"
  else
    let payload = Z.shift_right z 3 in
    if not (Z.fits_int payload) then Error "unknown seal-permission/locality"
    else
      let payload = Z.to_int payload in
      let sp = payload land 3 and l = payload lsr 2 in
      match
        ( (match sp with
          | 0 -> Some (false, false)
          | 1 -> Some (false, true)
          | 2 -> Some (true, false)
          | 3 -> Some (true, true)
          | _ -> None),
          match l with 0 -> Some Directed | 1 -> Some Local | 2 -> Some Global | _ -> None )
      with
      | Some p, Some l -> Ok (p, l)
      | _ -> Error "unknown seal-permission/locality"
