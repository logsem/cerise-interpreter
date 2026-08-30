open Ast

(* UNTRUSTED ADAPTER CODE.  This is intentionally a small, direct copy of the
   numeric format expected by the Rocq MachineParameters instance, rather than
   a use of either of the interpreter's codec abstractions. *)

let ( let* ) (type value next error) (result : (value, error) result)
        (continuation : value -> (next, error) result) : (next, error) result =
  Result.bind result continuation
let error (message : 'a) : ('b, 'a) result = Error message
let opcode_width = 8
let pack (opcode : int) (payload : Z.t) : Z.t = Z.logor (Z.of_int opcode) (Z.shift_left payload opcode_width)

let encode_register (matched_value : register) : (Z.t, string) result = match matched_value with
  | PC -> Ok Z.zero
  | Reg n when n >= 0 && n <= 31 -> Ok (Z.of_int (n + 1))
  | Reg n -> error (Printf.sprintf "invalid Griotte register r%d" n)

let decode_register (encoded : Z.t) : (register, string) result =
  if not (Z.fits_int encoded) then error "oversized Griotte register encoding"
  else
    let n = Z.to_int encoded in
    if n = 0 then Ok PC
    else if n >= 1 && n <= 32 then Ok (Reg (n - 1))
    else error "invalid Griotte register encoding"

let spread_nibble =
  Array.init 16 (fun nibble ->
      nibble land 1
      lor ((nibble land 2) lsl 1)
      lor ((nibble land 4) lsl 2)
      lor ((nibble land 8) lsl 3))

let compact_even_bits =
  Array.init 256 (fun byte ->
      byte land 1 lor ((byte lsr 1) land 2) lor ((byte lsr 2) land 4) lor ((byte lsr 3) land 8))

let interleave_unsigned (x : Z.t) (y : Z.t) : Z.t =
  let x = Z.to_bits x in
  let y = Z.to_bits y in
  let input_length = max (String.length x) (String.length y) in
  let interleaved = Bytes.create (input_length * 2) in
  for input_index = 0 to input_length - 1 do
    let x_byte = if input_index < String.length x then Char.code x.[input_index] else 0 in
    let y_byte = if input_index < String.length y then Char.code y.[input_index] else 0 in
    let output_index = input_index * 2 in
    Bytes.set interleaved output_index
      (Char.chr (spread_nibble.(x_byte land 0xf) lor (spread_nibble.(y_byte land 0xf) lsl 1)));
    Bytes.set interleaved (output_index + 1)
      (Char.chr (spread_nibble.(x_byte lsr 4) lor (spread_nibble.(y_byte lsr 4) lsl 1)))
  done;
  Z.of_bits (Bytes.unsafe_to_string interleaved)

let encode_pair (x : Z.t) (y : Z.t) : Z.t =
  let signs =
    match (Z.sign x < 0, Z.sign y < 0) with
    | false, false -> Z.zero
    | true, false -> Z.one
    | false, true -> Z.of_int 2
    | true, true -> Z.of_int 3
  in
  Z.logor signs (Z.shift_left (interleave_unsigned (Z.abs x) (Z.abs y)) 2)

let split_unsigned (value : Z.t) : Z.t * Z.t =
  let interleaved = Z.to_bits value in
  let interleaved_length = String.length interleaved in
  let output_length = (interleaved_length + 1) / 2 in
  let x = Bytes.create output_length in
  let y = Bytes.create output_length in
  for output_index = 0 to output_length - 1 do
    let input_index = output_index * 2 in
    let low = Char.code interleaved.[input_index] in
    let high =
      if input_index + 1 < interleaved_length then Char.code interleaved.[input_index + 1] else 0
    in
    Bytes.set x output_index
      (Char.chr (compact_even_bits.(low) lor (compact_even_bits.(high) lsl 4)));
    Bytes.set y output_index
      (Char.chr (compact_even_bits.(low lsr 1) lor (compact_even_bits.(high lsr 1) lsl 4)))
  done;
  (Z.of_bits (Bytes.unsafe_to_string x), Z.of_bits (Bytes.unsafe_to_string y))

let decode_pair (value : Z.t) : (Z.t * Z.t, string) result =
  if Z.sign value < 0 then error "negative tuple payload"
  else
    let x, y = split_unsigned (Z.shift_right value 2) in
    Ok ((if Z.testbit value 0 then Z.neg x else x), if Z.testbit value 1 then Z.neg y else y)

let encode_operand (base : int) (matched_value : reg_or_const) : (int * Z.t, string) result = match matched_value with
  | Register r -> Result.map (fun encoded -> (base, encoded)) (encode_register r)
  | Constant z -> Ok (base + 1, z)

let encode_two_operands (base : int) (first : reg_or_const) (second : reg_or_const) : (int * Z.t, string) result =
  let first_opcode, first =
    match first with Register r -> (base, encode_register r) | Constant z -> (base + 2, Ok z)
  in
  let* first = first in
  let* second_opcode, second = encode_operand first_opcode second in
  Ok (second_opcode, encode_pair first second)

let encode_rr (opcode : int) (a : register) (b : register) : (Z.t, string) result =
  let* a = encode_register a in
  let* b = encode_register b in
  Ok (pack opcode (encode_pair a b))

let encode_ro (opcode : int) (r : register) (operand : reg_or_const) : (Z.t, string) result =
  let* r = encode_register r in
  let* opcode, operand = encode_operand opcode operand in
  Ok (pack opcode (encode_pair r operand))

let encode_roo (opcode : int) (r : register) (a : reg_or_const) (b : reg_or_const) : (Z.t, string) result =
  let* r = encode_register r in
  let* opcode, operands = encode_two_operands opcode a b in
  Ok (pack opcode (encode_pair r operands))

let encode_rrr (opcode : int) (a : register) (b : register) (c : register) : (Z.t, string) result =
  let* a = encode_register a in
  let* b = encode_register b in
  let* c = encode_register c in
  Ok (pack opcode (encode_pair a (encode_pair b c)))

let encode (matched_value : instruction) : (Z.t, string) result = match matched_value with
  | Jmp operand ->
      let* opcode, payload = encode_operand 0x00 operand in
      Ok (pack opcode payload)
  | Jnz (r, operand) -> encode_ro 0x02 r operand
  | Jalr (a, b) -> encode_rr 0x04 a b
  | ReadSR (r, MTDC) ->
      let* r = encode_register r in
      Ok (pack 0x05 (encode_pair r Z.zero))
  | WriteSR (MTDC, r) ->
      let* r = encode_register r in
      Ok (pack 0x06 (encode_pair Z.zero r))
  | Move (r, operand) -> encode_ro 0x07 r operand
  | Load (a, b) -> encode_rr 0x09 a b
  | Store (r, operand) -> encode_ro 0x0a r operand
  | Add (r, a, b) -> encode_roo 0x0c r a b
  | Sub (r, a, b) -> encode_roo 0x10 r a b
  | Mul (r, a, b) -> encode_roo 0x14 r a b
  | Lt (r, a, b) -> encode_roo 0x20 r a b
  | Lea (r, operand) -> encode_ro 0x24 r operand
  | Restrict (r, operand) -> encode_ro 0x26 r operand
  | SubSeg (r, a, b) -> encode_roo 0x28 r a b
  | GetL (a, b) -> encode_rr 0x2c a b
  | GetB (a, b) -> encode_rr 0x2d a b
  | GetE (a, b) -> encode_rr 0x2e a b
  | GetA (a, b) -> encode_rr 0x2f a b
  | GetP (a, b) -> encode_rr 0x30 a b
  | GetOType (a, b) -> encode_rr 0x31 a b
  | GetWType (a, b) -> encode_rr 0x32 a b
  | Seal (a, b, c) -> encode_rrr 0x33 a b c
  | UnSeal (a, b, c) -> encode_rrr 0x34 a b c
  | Fail -> Ok (Z.of_int 0x35)
  | Halt -> Ok (Z.of_int 0x36)
  | LAnd (r, a, b) -> encode_roo 0x37 r a b
  | LOr (r, a, b) -> encode_roo 0x3b r a b
  | LShiftL (r, a, b) -> encode_roo 0x3f r a b
  | LShiftR (r, a, b) -> encode_roo 0x43 r a b

let decode_operand ~constant:(constant : bool) (encoded : Z.t) : (reg_or_const, string) result =
  if constant then Ok (Constant encoded)
  else Result.map (fun r -> Register r) (decode_register encoded)

let decode_rr (payload : Z.t) (construct : (register -> register -> 'a)) : ('a, string) result =
  let* a, b = decode_pair payload in
  let* a = decode_register a in
  let* b = decode_register b in
  Ok (construct a b)

let decode_ro (base : int) (opcode : int) (payload : Z.t) (construct : (register -> reg_or_const -> 'a)) : ('a, string) result =
  let* r, operand = decode_pair payload in
  let* r = decode_register r in
  let* operand = decode_operand ~constant:(opcode = base + 1) operand in
  Ok (construct r operand)

let decode_roo (base : int) (opcode : int) (payload : Z.t) (construct : (register -> reg_or_const -> reg_or_const -> 'a)) : ('a, string) result =
  let* r, operands = decode_pair payload in
  let* a, b = decode_pair operands in
  let variant = opcode - base in
  let* r = decode_register r in
  let* a = decode_operand ~constant:(variant land 2 <> 0) a in
  let* b = decode_operand ~constant:(variant land 1 <> 0) b in
  Ok (construct r a b)

let decode_rrr (payload : Z.t) (construct : (register -> register -> register -> 'a)) : ('a, string) result =
  let* a, rest = decode_pair payload in
  let* b, c = decode_pair rest in
  let* a = decode_register a in
  let* b = decode_register b in
  let* c = decode_register c in
  Ok (construct a b c)

let decode (encoded : Z.t) : (instruction, string) result =
  let opcode = Z.to_int (Z.extract encoded 0 opcode_width) in
  let payload = Z.shift_right encoded opcode_width in
  let in_span (base : int) : bool = opcode >= base && opcode < base + 4 in
  if Z.sign encoded < 0 && opcode <> 0x01 then error "negative extracted instruction encoding"
  else
    match opcode with
    | 0x00 | 0x01 ->
        Result.map (fun operand -> Jmp operand) (decode_operand ~constant:(opcode = 0x01) payload)
    | 0x02 | 0x03 -> decode_ro 0x02 opcode payload (fun r o -> Jnz (r, o))
    | 0x04 -> decode_rr payload (fun a b -> Jalr (a, b))
    | 0x05 ->
        let* r, sr = decode_pair payload in
        let* r = decode_register r in
        if Z.equal sr Z.zero then Ok (ReadSR (r, MTDC)) else error "unknown system register"
    | 0x06 ->
        let* sr, r = decode_pair payload in
        let* r = decode_register r in
        if Z.equal sr Z.zero then Ok (WriteSR (MTDC, r)) else error "unknown system register"
    | 0x07 | 0x08 -> decode_ro 0x07 opcode payload (fun r o -> Move (r, o))
    | 0x09 -> decode_rr payload (fun a b -> Load (a, b))
    | 0x0a | 0x0b -> decode_ro 0x0a opcode payload (fun r o -> Store (r, o))
    | _ when in_span 0x0c -> decode_roo 0x0c opcode payload (fun r a b -> Add (r, a, b))
    | _ when in_span 0x10 -> decode_roo 0x10 opcode payload (fun r a b -> Sub (r, a, b))
    | _ when in_span 0x14 -> decode_roo 0x14 opcode payload (fun r a b -> Mul (r, a, b))
    | _ when in_span 0x20 -> decode_roo 0x20 opcode payload (fun r a b -> Lt (r, a, b))
    | 0x24 | 0x25 -> decode_ro 0x24 opcode payload (fun r o -> Lea (r, o))
    | 0x26 | 0x27 -> decode_ro 0x26 opcode payload (fun r o -> Restrict (r, o))
    | _ when in_span 0x28 -> decode_roo 0x28 opcode payload (fun r a b -> SubSeg (r, a, b))
    | 0x2c -> decode_rr payload (fun a b -> GetL (a, b))
    | 0x2d -> decode_rr payload (fun a b -> GetB (a, b))
    | 0x2e -> decode_rr payload (fun a b -> GetE (a, b))
    | 0x2f -> decode_rr payload (fun a b -> GetA (a, b))
    | 0x30 -> decode_rr payload (fun a b -> GetP (a, b))
    | 0x31 -> decode_rr payload (fun a b -> GetOType (a, b))
    | 0x32 -> decode_rr payload (fun a b -> GetWType (a, b))
    | 0x33 -> decode_rrr payload (fun a b c -> Seal (a, b, c))
    | 0x34 -> decode_rrr payload (fun a b c -> UnSeal (a, b, c))
    | 0x35 when Z.equal payload Z.zero -> Ok Fail
    | 0x36 when Z.equal payload Z.zero -> Ok Halt
    | _ when in_span 0x37 -> decode_roo 0x37 opcode payload (fun r a b -> LAnd (r, a, b))
    | _ when in_span 0x3b -> decode_roo 0x3b opcode payload (fun r a b -> LOr (r, a, b))
    | _ when in_span 0x3f -> decode_roo 0x3f opcode payload (fun r a b -> LShiftL (r, a, b))
    | _ when in_span 0x43 -> decode_roo 0x43 opcode payload (fun r a b -> LShiftR (r, a, b))
    | _ -> error (Printf.sprintf "malformed or unknown extracted Griotte opcode 0x%02x" opcode)

let tagged (tag : int) (payload : Z.t) : Z.t = Z.logor (Z.of_int tag) (Z.shift_left payload 3)

let untag (tag : int) (kind : string) (encoded : Z.t) : (Z.t, string) result =
  if Z.sign encoded < 0 || not (Z.equal (Z.extract encoded 0 3) (Z.of_int tag)) then
    error ("not an extracted Griotte " ^ kind ^ " encoding")
  else Ok (Z.shift_right encoded 3)

let permission_scalar ((rx, w, dl, dro) : rx_permission * write_permission * deep_local_permission *
deep_read_only_permission) : int =
  let rx = match rx with Orx -> 0 | R -> 1 | X -> 2 | XSR -> 3 in
  let w = match w with Ow -> 0 | W -> 1 | WL -> 2 in
  let dl = match dl with DL -> 0 | LG -> 1 in
  let dro = match dro with DRO -> 0 | LM -> 1 in
  (rx lsl 4) lor (w lsl 2) lor (dl lsl 1) lor dro

let permission_of_scalar (n : int) : (rx_permission * write_permission * deep_local_permission *
 deep_read_only_permission, string)
result =
  if n < 0 || n > 0x3f || (n lsr 2) land 3 = 3 then error "unknown extracted permission"
  else
    let rx = match (n lsr 4) land 3 with 0 -> Orx | 1 -> R | 2 -> X | _ -> XSR in
    let w = match (n lsr 2) land 3 with 0 -> Ow | 1 -> W | _ -> WL in
    Ok (rx, w, (if n land 2 = 0 then DL else LG), if n land 1 = 0 then DRO else LM)

let encode_permission (p : rx_permission * write_permission * deep_local_permission *
deep_read_only_permission) : Z.t = tagged 0 (Z.of_int (permission_scalar p))

let decode_permission (encoded : Z.t) : (rx_permission * write_permission * deep_local_permission *
 deep_read_only_permission, string)
result =
  let* payload = untag 0 "permission" encoded in
  if Z.fits_int payload then permission_of_scalar (Z.to_int payload)
  else error "oversized extracted permission"

let locality_scalar (matched_value : locality) : int = match matched_value with Local -> 0 | Global -> 1
let encode_locality (locality : locality) : Z.t = tagged 2 (Z.of_int (locality_scalar locality))

let seal_scalar (matched_value : bool * bool) : int = match matched_value with
  | false, false -> 0
  | false, true -> 1
  | true, false -> 2
  | true, true -> 3

let encode_seal_permission (p : bool * bool) : Z.t = tagged 1 (Z.of_int (seal_scalar p))

let decode_seal_permission (encoded : Z.t) : (bool * bool, string) result =
  let* payload = untag 1 "seal permission" encoded in
  if not (Z.fits_int payload) then error "oversized extracted seal permission"
  else
    match Z.to_int payload with
    | 0 -> Ok (false, false)
    | 1 -> Ok (false, true)
    | 2 -> Ok (true, false)
    | 3 -> Ok (true, true)
    | _ -> error "unknown extracted seal permission"

let encode_permission_locality (p : rx_permission * write_permission * deep_local_permission *
deep_read_only_permission) (locality : locality) : Z.t =
  tagged 4 (Z.of_int ((locality_scalar locality lsl 6) lor permission_scalar p))

let decode_permission_locality (encoded : Z.t) : ((rx_permission * write_permission * deep_local_permission *
  deep_read_only_permission) *
 locality, string)
result =
  let* payload = untag 4 "permission/locality" encoded in
  if (not (Z.fits_int payload)) || Z.to_int payload > 0x7f then
    error "unknown extracted permission/locality"
  else
    let n = Z.to_int payload in
    let* p = permission_of_scalar (n land 0x3f) in
    Ok (p, if n land 0x40 = 0 then Local else Global)

let encode_seal_permission_locality (p : bool * bool) (locality : locality) : Z.t =
  tagged 5 (Z.of_int ((locality_scalar locality lsl 2) lor seal_scalar p))

let decode_seal_permission_locality (encoded : Z.t) : ((bool * bool) * locality, string) result =
  let* payload = untag 5 "seal permission/locality" encoded in
  if (not (Z.fits_int payload)) || Z.to_int payload > 7 then
    error "unknown extracted seal permission/locality"
  else
    let n = Z.to_int payload in
    let p =
      match n land 3 with
      | 0 -> (false, false)
      | 1 -> (false, true)
      | 2 -> (true, false)
      | _ -> (true, true)
    in
    Ok (p, if n land 4 = 0 then Local else Global)

let word_type_scalar (matched_value : word_type) : int = match matched_value with
  | W_I -> 0
  | W_Cap -> 1
  | W_SealRange -> 2
  | W_Sealed -> 3
  | W_Sentry -> 4

let encode_word_type (word_type : word_type) : Z.t = tagged 3 (Z.of_int (word_type_scalar word_type))

let decode_word_type (encoded : Z.t) : (word_type, string) result =
  let* payload = untag 3 "word type" encoded in
  if not (Z.fits_int payload) then error "oversized extracted word type"
  else
    match Z.to_int payload with
    | 0 -> Ok W_I
    | 1 -> Ok W_Cap
    | 2 -> Ok W_SealRange
    | 3 -> Ok W_Sealed
    | 4 -> Ok W_Sentry
    | _ -> error "unknown extracted word type"
