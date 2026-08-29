open Griotte_ast

let register_codec =
  Instruction_codec.scalar_codec ~name:"Griotte register"
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

let system_register_codec =
  Instruction_codec.scalar_codec ~name:"Griotte system register"
    ~encode:(function MTDC -> Ok Z.zero)
    ~decode:(fun value -> if Z.equal value Z.zero then Ok MTDC else Error "unknown system register")

let operand = Instruction_codec.register_or_constant register_codec Instruction_codec.zarith
let from_operand = function Instruction_codec.Register r -> Register r | Constant z -> Constant z
let to_operand = function Register r -> Instruction_codec.Register r | Constant z -> Constant z
let r = Instruction_codec.register register_codec
let sr = Instruction_codec.register system_register_codec
let o = operand
let rr = Instruction_codec.pair r r
let rs = Instruction_codec.pair r sr
let sr_r = Instruction_codec.pair sr r
let ro = Instruction_codec.pair r o
let roo = Instruction_codec.triple r o o
let rrr = Instruction_codec.triple r r r

let case name opcode codec construct project =
  Instruction_codec.case ~name ~allocation:(Fixed opcode) codec ~construct ~project

let unit_case name opcode construct project =
  case name opcode Instruction_codec.unit
    (fun () -> construct)
    (fun x -> if project x then Some () else None)

let cases =
  [
    case "Jmp" 0x00 o
      (fun x -> Jmp (from_operand x))
      (function Jmp x -> Some (to_operand x) | _ -> None);
    case "Jnz" 0x02 ro
      (fun (a, b) -> Jnz (a, from_operand b))
      (function Jnz (a, b) -> Some (a, to_operand b) | _ -> None);
    case "Jalr" 0x04 rr
      (fun (a, b) -> Jalr (a, b))
      (function Jalr (a, b) -> Some (a, b) | _ -> None);
    case "ReadSR" 0x05 rs
      (fun (a, b) -> ReadSR (a, b))
      (function ReadSR (a, b) -> Some (a, b) | _ -> None);
    case "WriteSR" 0x06 sr_r
      (fun (a, b) -> WriteSR (a, b))
      (function WriteSR (a, b) -> Some (a, b) | _ -> None);
    case "Move" 0x07 ro
      (fun (a, b) -> Move (a, from_operand b))
      (function Move (a, b) -> Some (a, to_operand b) | _ -> None);
    case "Load" 0x09 rr
      (fun (a, b) -> Load (a, b))
      (function Load (a, b) -> Some (a, b) | _ -> None);
    case "Store" 0x0a ro
      (fun (a, b) -> Store (a, from_operand b))
      (function Store (a, b) -> Some (a, to_operand b) | _ -> None);
    case "Add" 0x0c roo
      (fun (a, b, c) -> Add (a, from_operand b, from_operand c))
      (function Add (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case "Sub" 0x10 roo
      (fun (a, b, c) -> Sub (a, from_operand b, from_operand c))
      (function Sub (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case "Mul" 0x14 roo
      (fun (a, b, c) -> Mul (a, from_operand b, from_operand c))
      (function Mul (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case "Rem" 0x18 roo
      (fun (a, b, c) -> Rem (a, from_operand b, from_operand c))
      (function Rem (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case "Div" 0x1c roo
      (fun (a, b, c) -> Div (a, from_operand b, from_operand c))
      (function Div (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case "Lt" 0x20 roo
      (fun (a, b, c) -> Lt (a, from_operand b, from_operand c))
      (function Lt (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case "Lea" 0x24 ro
      (fun (a, b) -> Lea (a, from_operand b))
      (function Lea (a, b) -> Some (a, to_operand b) | _ -> None);
    case "Restrict" 0x26 ro
      (fun (a, b) -> Restrict (a, from_operand b))
      (function Restrict (a, b) -> Some (a, to_operand b) | _ -> None);
    case "SubSeg" 0x28 roo
      (fun (a, b, c) -> SubSeg (a, from_operand b, from_operand c))
      (function SubSeg (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case "GetL" 0x2c rr
      (fun (a, b) -> GetL (a, b))
      (function GetL (a, b) -> Some (a, b) | _ -> None);
    case "GetB" 0x2d rr
      (fun (a, b) -> GetB (a, b))
      (function GetB (a, b) -> Some (a, b) | _ -> None);
    case "GetE" 0x2e rr
      (fun (a, b) -> GetE (a, b))
      (function GetE (a, b) -> Some (a, b) | _ -> None);
    case "GetA" 0x2f rr
      (fun (a, b) -> GetA (a, b))
      (function GetA (a, b) -> Some (a, b) | _ -> None);
    case "GetP" 0x30 rr
      (fun (a, b) -> GetP (a, b))
      (function GetP (a, b) -> Some (a, b) | _ -> None);
    case "GetOType" 0x31 rr
      (fun (a, b) -> GetOType (a, b))
      (function GetOType (a, b) -> Some (a, b) | _ -> None);
    case "GetWType" 0x32 rr
      (fun (a, b) -> GetWType (a, b))
      (function GetWType (a, b) -> Some (a, b) | _ -> None);
    case "Seal" 0x33 rrr
      (fun (a, b, c) -> Seal (a, b, c))
      (function Seal (a, b, c) -> Some (a, b, c) | _ -> None);
    case "UnSeal" 0x34 rrr
      (fun (a, b, c) -> UnSeal (a, b, c))
      (function UnSeal (a, b, c) -> Some (a, b, c) | _ -> None);
    unit_case "Fail" 0x35 Fail (function Fail -> true | _ -> false);
    unit_case "Halt" 0x36 Halt (function Halt -> true | _ -> false);
    case "LAnd" 0x37 roo
      (fun (a, b, c) -> LAnd (a, from_operand b, from_operand c))
      (function LAnd (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case "LOr" 0x3b roo
      (fun (a, b, c) -> LOr (a, from_operand b, from_operand c))
      (function LOr (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case "LShiftL" 0x3f roo
      (fun (a, b, c) -> LShiftL (a, from_operand b, from_operand c))
      (function LShiftL (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    case "LShiftR" 0x43 roo
      (fun (a, b, c) -> LShiftR (a, from_operand b, from_operand c))
      (function LShiftR (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
  ]

let table =
  match Instruction_codec.compile cases with
  | Ok table -> table
  | Error errors -> failwith (String.concat "; " (List.map Instruction_codec.error_message errors))

let encode = function
  | Jmp (Constant immediate) when Z.sign immediate < 0 -> Ok Z.(logor one (shift_left immediate 8))
  | instruction -> Instruction_codec.encode table instruction

let decode encoded =
  if Z.sign encoded < 0 then
    let opcode = Z.to_int (Z.extract encoded 0 8) in
    if opcode = 0x01 then Ok (Jmp (Constant (Z.shift_right encoded 8)))
    else Error (Instruction_codec.Negative_encoding encoded)
  else Instruction_codec.decode table encoded

let allocations = Instruction_codec.allocations table
let encode_tag tag payload = Z.logor (Z.of_int tag) (Z.shift_left payload 3)

let decode_tag tag name value =
  if Z.sign value < 0 || not (Z.equal (Z.extract value 0 3) (Z.of_int tag)) then
    Error ("not a Griotte " ^ name ^ " encoding")
  else Ok (Z.shift_right value 3)

let rx_scalar = function Orx -> 0 | R -> 1 | X -> 2 | XSR -> 3
let write_scalar = function Ow -> 0 | W -> 1 | WL -> 2
let dl_scalar = function DL -> 0 | LG -> 1
let dro_scalar = function DRO -> 0 | LM -> 1

let permission_scalar (rx, w, dl, dro) =
  (rx_scalar rx lsl 4) lor (write_scalar w lsl 2) lor (dl_scalar dl lsl 1) lor dro_scalar dro

let permission_of_scalar scalar =
  let rx = match (scalar lsr 4) land 3 with 0 -> Orx | 1 -> R | 2 -> X | _ -> XSR in
  let w =
    match (scalar lsr 2) land 3 with 0 -> Some Ow | 1 -> Some W | 2 -> Some WL | _ -> None
  in
  match w with
  | None -> None
  | Some w ->
      Some (rx, w, (if scalar land 2 = 0 then DL else LG), if scalar land 1 = 0 then DRO else LM)

let payload_at_most maximum message payload =
  if Z.compare payload (Z.of_int maximum) <= 0 then Ok payload else Error message

let decode_permission_payload payload =
  Result.bind (payload_at_most 0x3f "unknown Griotte permission" payload) (fun payload ->
      match permission_of_scalar (Z.to_int payload) with
      | Some permission -> Ok permission
      | None -> Error "unknown Griotte permission")

let encode_permission p = encode_tag 0 (Z.of_int (permission_scalar p))
let decode_permission z = Result.bind (decode_tag 0 "permission" z) decode_permission_payload

let seal_permission_scalar = function
  | false, false -> 0
  | false, true -> 1
  | true, false -> 2
  | true, true -> 3

let encode_seal_permission p = encode_tag 1 (Z.of_int (seal_permission_scalar p))

let decode_seal_permission z =
  Result.bind (decode_tag 1 "seal permission" z) (fun p ->
      if Z.equal p Z.zero then Ok (false, false)
      else if Z.equal p Z.one then Ok (false, true)
      else if Z.equal p (Z.of_int 2) then Ok (true, false)
      else if Z.equal p (Z.of_int 3) then Ok (true, true)
      else Error "unknown Griotte seal permission")

let locality_scalar = function Local -> 0 | Global -> 1
let encode_locality l = encode_tag 2 (Z.of_int (locality_scalar l))

let decode_locality z =
  Result.bind (decode_tag 2 "locality" z) (fun p ->
      if Z.equal p Z.zero then Ok Local
      else if Z.equal p Z.one then Ok Global
      else Error "unknown Griotte locality")

let word_type_scalar = function
  | W_I -> 0
  | W_Cap -> 1
  | W_SealRange -> 2
  | W_Sealed -> 3
  | W_Sentry -> 4

let encode_word_type w = encode_tag 3 (Z.of_int (word_type_scalar w))

let decode_word_type z =
  Result.bind (decode_tag 3 "word type" z) (fun p ->
      if Z.equal p Z.zero then Ok W_I
      else if Z.equal p Z.one then Ok W_Cap
      else if Z.equal p (Z.of_int 2) then Ok W_SealRange
      else if Z.equal p (Z.of_int 3) then Ok W_Sealed
      else if Z.equal p (Z.of_int 4) then Ok W_Sentry
      else Error "unknown Griotte word type")

let encode_permission_locality p l =
  encode_tag 4 (Z.of_int ((locality_scalar l lsl 6) lor permission_scalar p))

let decode_permission_locality z =
  Result.bind (decode_tag 4 "permission/locality" z) (fun p ->
      Result.bind (payload_at_most 0x7f "unknown Griotte permission/locality" p) (fun p ->
          let locality = if Z.testbit p 6 then Global else Local in
          Result.map
            (fun permission -> (permission, locality))
            (decode_permission_payload (Z.extract p 0 6))))

let encode_seal_permission_locality p l =
  encode_tag 5 (Z.of_int ((locality_scalar l lsl 2) lor seal_permission_scalar p))

let decode_seal_permission_locality z =
  Result.bind (decode_tag 5 "seal permission/locality" z) (fun p ->
      Result.bind (payload_at_most 0x7 "unknown Griotte seal permission/locality" p) (fun p ->
          let locality = if Z.testbit p 2 then Global else Local in
          let seal =
            match Z.to_int (Z.extract p 0 2) with
            | 0 -> (false, false)
            | 1 -> (false, true)
            | 2 -> (true, false)
            | _ -> (true, true)
          in
          Ok (seal, locality)))
