(* Encode and decode handwritten Griotte instructions and tagged metadata. The
   instruction table owns opcode allocation; the helpers below own capability tags. *)

open Ast

(* Instruction encoding. *)
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

let from_operand (value : (register, Z.t) Instruction_codec.register_or_constant) : reg_or_const =
  match value with
  | Instruction_codec.Register register -> Register register
  | Constant z -> Constant z

let to_operand (value : reg_or_const) : (register, Z.t) Instruction_codec.register_or_constant =
  match value with
  | Register register -> Instruction_codec.Register register
  | Constant z -> Constant z

let register_operand_codec = Instruction_codec.register register_codec
let system_register_operand_codec = Instruction_codec.register system_register_codec
let value_operand_codec = operand

let register_pair_operand_codec =
  Instruction_codec.pair register_operand_codec register_operand_codec

let register_system_pair_operand_codec =
  Instruction_codec.pair register_operand_codec system_register_operand_codec

let system_register_register_pair_operand_codec =
  Instruction_codec.pair system_register_operand_codec register_operand_codec

let register_value_pair_operand_codec =
  Instruction_codec.pair register_operand_codec value_operand_codec

let register_two_values_operand_codec =
  Instruction_codec.triple register_operand_codec value_operand_codec value_operand_codec

let register_triple_operand_codec =
  Instruction_codec.triple register_operand_codec register_operand_codec register_operand_codec

let pattern (name : string) (codec : 'a Instruction_codec.operand_codec) (construct : 'a -> 'b)
    (project : 'b -> 'a option) : 'b Instruction_codec.encoding_pattern =
  Instruction_codec.encoding_pattern ~name codec ~construct ~project

let unit_pattern (name : string) (construct : 'a) (project : 'a -> bool) :
    'a Instruction_codec.encoding_pattern =
  pattern name Instruction_codec.unit
    (fun () -> construct)
    (fun x -> if project x then Some () else None)

let encoding_patterns =
  [
    pattern "Jmp-register" register_operand_codec
      (fun register -> Jmp (Register register))
      (function Jmp (Register register) -> Some register | _ -> None);
    pattern "Jmp-constant" Instruction_codec.signed_zarith
      (fun constant -> Jmp (Constant constant))
      (function Jmp (Constant constant) -> Some constant | _ -> None);
    pattern "Jnz" register_value_pair_operand_codec
      (fun (a, b) -> Jnz (a, from_operand b))
      (function Jnz (a, b) -> Some (a, to_operand b) | _ -> None);
    pattern "Jalr" register_pair_operand_codec
      (fun (a, b) -> Jalr (a, b))
      (function Jalr (a, b) -> Some (a, b) | _ -> None);
    pattern "ReadSR" register_system_pair_operand_codec
      (fun (a, b) -> ReadSR (a, b))
      (function ReadSR (a, b) -> Some (a, b) | _ -> None);
    pattern "WriteSR" system_register_register_pair_operand_codec
      (fun (a, b) -> WriteSR (a, b))
      (function WriteSR (a, b) -> Some (a, b) | _ -> None);
    pattern "Move" register_value_pair_operand_codec
      (fun (a, b) -> Move (a, from_operand b))
      (function Move (a, b) -> Some (a, to_operand b) | _ -> None);
    pattern "Load" register_pair_operand_codec
      (fun (a, b) -> Load (a, b))
      (function Load (a, b) -> Some (a, b) | _ -> None);
    pattern "Store" register_value_pair_operand_codec
      (fun (a, b) -> Store (a, from_operand b))
      (function Store (a, b) -> Some (a, to_operand b) | _ -> None);
    pattern "Add" register_two_values_operand_codec
      (fun (a, b, c) -> Add (a, from_operand b, from_operand c))
      (function Add (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "Sub" register_two_values_operand_codec
      (fun (a, b, c) -> Sub (a, from_operand b, from_operand c))
      (function Sub (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "Mul" register_two_values_operand_codec
      (fun (a, b, c) -> Mul (a, from_operand b, from_operand c))
      (function Mul (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "Lt" register_two_values_operand_codec
      (fun (a, b, c) -> Lt (a, from_operand b, from_operand c))
      (function Lt (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "Lea" register_value_pair_operand_codec
      (fun (a, b) -> Lea (a, from_operand b))
      (function Lea (a, b) -> Some (a, to_operand b) | _ -> None);
    pattern "Restrict" register_value_pair_operand_codec
      (fun (a, b) -> Restrict (a, from_operand b))
      (function Restrict (a, b) -> Some (a, to_operand b) | _ -> None);
    pattern "SubSeg" register_two_values_operand_codec
      (fun (a, b, c) -> SubSeg (a, from_operand b, from_operand c))
      (function SubSeg (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "GetL" register_pair_operand_codec
      (fun (a, b) -> GetL (a, b))
      (function GetL (a, b) -> Some (a, b) | _ -> None);
    pattern "GetB" register_pair_operand_codec
      (fun (a, b) -> GetB (a, b))
      (function GetB (a, b) -> Some (a, b) | _ -> None);
    pattern "GetE" register_pair_operand_codec
      (fun (a, b) -> GetE (a, b))
      (function GetE (a, b) -> Some (a, b) | _ -> None);
    pattern "GetA" register_pair_operand_codec
      (fun (a, b) -> GetA (a, b))
      (function GetA (a, b) -> Some (a, b) | _ -> None);
    pattern "GetP" register_pair_operand_codec
      (fun (a, b) -> GetP (a, b))
      (function GetP (a, b) -> Some (a, b) | _ -> None);
    pattern "GetOType" register_pair_operand_codec
      (fun (a, b) -> GetOType (a, b))
      (function GetOType (a, b) -> Some (a, b) | _ -> None);
    pattern "GetWType" register_pair_operand_codec
      (fun (a, b) -> GetWType (a, b))
      (function GetWType (a, b) -> Some (a, b) | _ -> None);
    pattern "Seal" register_triple_operand_codec
      (fun (a, b, c) -> Seal (a, b, c))
      (function Seal (a, b, c) -> Some (a, b, c) | _ -> None);
    pattern "UnSeal" register_triple_operand_codec
      (fun (a, b, c) -> UnSeal (a, b, c))
      (function UnSeal (a, b, c) -> Some (a, b, c) | _ -> None);
    unit_pattern "Fail" Fail (function Fail -> true | _ -> false);
    unit_pattern "Halt" Halt (function Halt -> true | _ -> false);
    pattern "LAnd" register_two_values_operand_codec
      (fun (a, b, c) -> LAnd (a, from_operand b, from_operand c))
      (function LAnd (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "LOr" register_two_values_operand_codec
      (fun (a, b, c) -> LOr (a, from_operand b, from_operand c))
      (function LOr (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "LShiftL" register_two_values_operand_codec
      (fun (a, b, c) -> LShiftL (a, from_operand b, from_operand c))
      (function LShiftL (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "LShiftR" register_two_values_operand_codec
      (fun (a, b, c) -> LShiftR (a, from_operand b, from_operand c))
      (function LShiftR (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
  ]

let table =
  match Instruction_codec.compile encoding_patterns with
  | Ok table -> table
  | Error errors -> failwith (String.concat "; " (List.map Instruction_codec.error_message errors))

let encode = Instruction_codec.encode table
let decode = Instruction_codec.decode table

(* Capability-metadata encoding. These fixed tags are part of the assembly and
   machine contract, so decoding deliberately rejects rather than normalizes them. *)
let encode_tag (tag : int) (payload : Z.t) : Z.t = Z.logor (Z.of_int tag) (Z.shift_left payload 3)

let decode_tag (tag : int) (name : string) (value : Z.t) : (Z.t, string) result =
  if Z.sign value < 0 || not (Z.equal (Z.extract value 0 3) (Z.of_int tag)) then
    Error ("not a Griotte " ^ name ^ " encoding")
  else Ok (Z.shift_right value 3)

let rx_scalar (value : rx_permission) : int =
  match value with Orx -> 0 | R -> 1 | X -> 2 | XSR -> 3

let write_scalar (value : write_permission) : int = match value with Ow -> 0 | W -> 1 | WL -> 2
let dl_scalar (value : deep_local_permission) : int = match value with DL -> 0 | LG -> 1
let dro_scalar (value : deep_read_only_permission) : int = match value with DRO -> 0 | LM -> 1

let permission_scalar
    ((rx, w, dl, dro) :
      rx_permission * write_permission * deep_local_permission * deep_read_only_permission) : int =
  (rx_scalar rx lsl 4) lor (write_scalar w lsl 2) lor (dl_scalar dl lsl 1) lor dro_scalar dro

let permission_of_scalar (scalar : int) :
    (rx_permission * write_permission * deep_local_permission * deep_read_only_permission) option =
  let rx = match (scalar lsr 4) land 3 with 0 -> Orx | 1 -> R | 2 -> X | _ -> XSR in
  let w =
    match (scalar lsr 2) land 3 with 0 -> Some Ow | 1 -> Some W | 2 -> Some WL | _ -> None
  in
  match w with
  | None -> None
  | Some w ->
      Some (rx, w, (if scalar land 2 = 0 then DL else LG), if scalar land 1 = 0 then DRO else LM)

let payload_at_most (maximum : int) (message : 'a) (payload : Z.t) : (Z.t, 'a) result =
  if Z.compare payload (Z.of_int maximum) <= 0 then Ok payload else Error message

let decode_permission_payload (payload : Z.t) :
    ( rx_permission * write_permission * deep_local_permission * deep_read_only_permission,
      string )
    result =
  Result.bind (payload_at_most 0x3f "unknown Griotte permission" payload) (fun payload ->
      match permission_of_scalar (Z.to_int payload) with
      | Some permission -> Ok permission
      | None -> Error "unknown Griotte permission")

let encode_permission
    (p : rx_permission * write_permission * deep_local_permission * deep_read_only_permission) : Z.t
    =
  encode_tag 0 (Z.of_int (permission_scalar p))

let decode_permission (z : Z.t) :
    ( rx_permission * write_permission * deep_local_permission * deep_read_only_permission,
      string )
    result =
  Result.bind (decode_tag 0 "permission" z) decode_permission_payload

let seal_permission_scalar (value : bool * bool) : int =
  match value with false, false -> 0 | false, true -> 1 | true, false -> 2 | true, true -> 3

let encode_seal_permission (p : bool * bool) : Z.t =
  encode_tag 1 (Z.of_int (seal_permission_scalar p))

let decode_seal_permission (z : Z.t) : (bool * bool, string) result =
  Result.bind (decode_tag 1 "seal permission" z) (fun p ->
      if Z.equal p Z.zero then Ok (false, false)
      else if Z.equal p Z.one then Ok (false, true)
      else if Z.equal p (Z.of_int 2) then Ok (true, false)
      else if Z.equal p (Z.of_int 3) then Ok (true, true)
      else Error "unknown Griotte seal permission")

let locality_scalar (value : locality) : int = match value with Local -> 0 | Global -> 1
let encode_locality (l : locality) : Z.t = encode_tag 2 (Z.of_int (locality_scalar l))

let decode_locality (z : Z.t) : (locality, string) result =
  Result.bind (decode_tag 2 "locality" z) (fun p ->
      if Z.equal p Z.zero then Ok Local
      else if Z.equal p Z.one then Ok Global
      else Error "unknown Griotte locality")

let word_type_scalar (value : word_type) : int =
  match value with W_I -> 0 | W_Cap -> 1 | W_SealRange -> 2 | W_Sealed -> 3 | W_Sentry -> 4

let encode_word_type (w : word_type) : Z.t = encode_tag 3 (Z.of_int (word_type_scalar w))

let decode_word_type (z : Z.t) : (word_type, string) result =
  Result.bind (decode_tag 3 "word type" z) (fun p ->
      if Z.equal p Z.zero then Ok W_I
      else if Z.equal p Z.one then Ok W_Cap
      else if Z.equal p (Z.of_int 2) then Ok W_SealRange
      else if Z.equal p (Z.of_int 3) then Ok W_Sealed
      else if Z.equal p (Z.of_int 4) then Ok W_Sentry
      else Error "unknown Griotte word type")

let encode_permission_locality
    (p : rx_permission * write_permission * deep_local_permission * deep_read_only_permission)
    (l : locality) : Z.t =
  encode_tag 4 (Z.of_int ((locality_scalar l lsl 6) lor permission_scalar p))

let decode_permission_locality (z : Z.t) :
    ( (rx_permission * write_permission * deep_local_permission * deep_read_only_permission)
      * locality,
      string )
    result =
  Result.bind (decode_tag 4 "permission/locality" z) (fun p ->
      Result.bind (payload_at_most 0x7f "unknown Griotte permission/locality" p) (fun p ->
          let locality = if Z.testbit p 6 then Global else Local in
          Result.map
            (fun permission -> (permission, locality))
            (decode_permission_payload (Z.extract p 0 6))))

let encode_seal_permission_locality (p : bool * bool) (l : locality) : Z.t =
  encode_tag 5 (Z.of_int ((locality_scalar l lsl 2) lor seal_permission_scalar p))

let decode_seal_permission_locality (z : Z.t) : ((bool * bool) * locality, string) result =
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
