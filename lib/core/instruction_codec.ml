(** Declarative instruction encoding.

    Authors build a codec in four layers: scalar codecs encode atomic values, operand codecs compose
    operands and their opcode variants, encoding patterns associate operands with instruction
    constructors, and compilation assigns contiguous opcode ranges in declaration order. *)

type error =
  | Duplicate_pattern_name of string
  | Opcode_overflow of { pattern_name : string; first_opcode : int; span : int }
  | Unrecognized_instruction
  | Ambiguous_instruction of string list
  | Invalid_operand of { pattern_name : string; message : string }
  | Negative_encoding of Z.t
  | Unknown_opcode of int
  | Malformed_encoding of { opcode : int; pattern_name : string; message : string }

let error_message (error : error) : string =
  match error with
  | Duplicate_pattern_name name ->
      Printf.sprintf "Duplicate instruction encoding pattern name %S." name
  | Opcode_overflow { pattern_name; first_opcode; span } ->
      Printf.sprintf
        "Instruction encoding pattern %S needs %d opcodes starting at %d, beyond the 8-bit field."
        pattern_name span first_opcode
  | Unrecognized_instruction -> "No instruction encoding pattern projected the value being encoded."
  | Ambiguous_instruction names ->
      Printf.sprintf "Multiple instruction encoding patterns projected the value: %s."
        (String.concat ", " names)
  | Invalid_operand { pattern_name; message } ->
      Printf.sprintf "Invalid operand for instruction encoding pattern %S: %s" pattern_name message
  | Negative_encoding value ->
      Printf.sprintf "Instruction encodings must be non-negative, got %s." (Z.to_string value)
  | Unknown_opcode opcode -> Printf.sprintf "Unknown instruction opcode 0x%02x." opcode
  | Malformed_encoding { opcode; pattern_name; message } ->
      Printf.sprintf "Malformed encoding for %S at opcode 0x%02x: %s" pattern_name opcode message

type 'a scalar_codec = {
  scalar_name : string;
  encode_scalar : 'a -> (Z.t, string) result;
  decode_scalar : Z.t -> ('a, string) result;
}

let scalar_codec ~(name : string) ~(encode : 'a -> (Z.t, string) result)
    ~(decode : Z.t -> ('a, string) result) : 'a scalar_codec =
  { scalar_name = name; encode_scalar = encode; decode_scalar = decode }

let zarith = scalar_codec ~name:"integer" ~encode:Result.ok ~decode:Result.ok

let nonnegative_zarith =
  let check (value : Z.t) : (Z.t, string) result =
    if Z.sign value < 0 then Error "expected a non-negative integer" else Ok value
  in
  scalar_codec ~name:"non-negative integer" ~encode:check ~decode:check

let enum ~(name : string) (values : 'a list) : 'a scalar_codec =
  let rec encode (index : int) (value : 'a) (remaining_values : 'a list) : (Z.t, string) result =
    match remaining_values with
    | [] -> Error (Printf.sprintf "unknown %s value" name)
    | candidate :: _ when candidate = value -> Ok (Z.of_int index)
    | _ :: rest -> encode (index + 1) value rest
  in
  let decode (encoded : Z.t) : ('a, string) result =
    if Z.sign encoded < 0 || not (Z.fits_int encoded) then
      Error (Printf.sprintf "invalid %s encoding %s" name (Z.to_string encoded))
    else
      match List.nth_opt values (Z.to_int encoded) with
      | Some value -> Ok value
      | None -> Error (Printf.sprintf "invalid %s encoding %s" name (Z.to_string encoded))
  in
  scalar_codec ~name ~encode:(fun value -> encode 0 value values) ~decode

type ('register, 'constant) register_or_constant = Register of 'register | Constant of 'constant

(* Pair payloads by interleaving bits, rather than bounding either component to a machine word. *)
let spread_nibble : int array =
  Array.init 16 (fun nibble ->
      nibble land 1
      lor ((nibble land 2) lsl 1)
      lor ((nibble land 4) lsl 2)
      lor ((nibble land 8) lsl 3))

let compact_even_bits : int array =
  Array.init 256 (fun byte ->
      byte land 1 lor ((byte lsr 1) land 2) lor ((byte lsr 2) land 4) lor ((byte lsr 3) land 8))

let split_unsigned (value : Z.t) : Z.t * Z.t =
  let interleaved = Z.to_bits value in
  let interleaved_length = String.length interleaved in
  let output_length = (interleaved_length + 1) / 2 in
  let first = Bytes.create output_length in
  let second = Bytes.create output_length in
  for output_index = 0 to output_length - 1 do
    let input_index = output_index * 2 in
    let low = Char.code interleaved.[input_index] in
    let high =
      if input_index + 1 < interleaved_length then Char.code interleaved.[input_index + 1] else 0
    in
    Bytes.set first output_index
      (Char.chr (compact_even_bits.(low) lor (compact_even_bits.(high) lsl 4)));
    Bytes.set second output_index
      (Char.chr (compact_even_bits.(low lsr 1) lor (compact_even_bits.(high lsr 1) lsl 4)))
  done;
  (Z.of_bits (Bytes.unsafe_to_string first), Z.of_bits (Bytes.unsafe_to_string second))

let interleave_unsigned (first : Z.t) (second : Z.t) : Z.t =
  let first = Z.to_bits first in
  let second = Z.to_bits second in
  let input_length = max (String.length first) (String.length second) in
  let interleaved = Bytes.create (input_length * 2) in
  for input_index = 0 to input_length - 1 do
    let first_byte =
      if input_index < String.length first then Char.code first.[input_index] else 0
    in
    let second_byte =
      if input_index < String.length second then Char.code second.[input_index] else 0
    in
    let output_index = input_index * 2 in
    Bytes.set interleaved output_index
      (Char.chr
         (spread_nibble.(first_byte land 0xf) lor (spread_nibble.(second_byte land 0xf) lsl 1)));
    Bytes.set interleaved (output_index + 1)
      (Char.chr (spread_nibble.(first_byte lsr 4) lor (spread_nibble.(second_byte lsr 4) lsl 1)))
  done;
  Z.of_bits (Bytes.unsafe_to_string interleaved)

let encode_signed_pair (first : Z.t) (second : Z.t) : Z.t =
  let signs =
    match (Z.sign second < 0, Z.sign first < 0) with
    | false, false -> Z.zero
    | false, true -> Z.one
    | true, false -> Z.of_int 2
    | true, true -> Z.of_int 3
  in
  Z.logor signs (Z.shift_left (interleave_unsigned (Z.abs first) (Z.abs second)) 2)

let decode_signed_pair (encoded : Z.t) : Z.t * Z.t =
  let first_negative = Z.testbit encoded 0 in
  let second_negative = Z.testbit encoded 1 in
  let first, second = split_unsigned (Z.shift_right encoded 2) in
  ((if first_negative then Z.neg first else first), if second_negative then Z.neg second else second)

type 'a operand_codec = {
  span : int;
  encode_operand : 'a -> (int * Z.t, string) result;
  decode_operand : int -> Z.t -> ('a, string) result;
}

let opcode_count = 256

(* Spans larger than the opcode field are indistinguishable to compilation. Saturating at one
   beyond the field keeps nested composition overflow-safe on every host integer width. *)
let multiply_spans (left : int) (right : int) : int =
  if left > opcode_count || right > opcode_count || left > opcode_count / right then
    opcode_count + 1
  else left * right

let unit : unit operand_codec =
  {
    span = 1;
    encode_operand = (fun () -> Ok (0, Z.zero));
    decode_operand =
      (fun variant payload ->
        if variant <> 0 then Error "unit operand has an invalid opcode variant"
        else if not (Z.equal payload Z.zero) then Error "unit operand has a non-zero payload"
        else Ok ());
  }

let scalar (codec : 'a scalar_codec) : 'a operand_codec =
  {
    span = 1;
    encode_operand =
      (fun value -> Result.map (fun encoded -> (0, encoded)) (codec.encode_scalar value));
    decode_operand =
      (fun variant payload ->
        if variant <> 0 then Error (codec.scalar_name ^ " has an invalid opcode variant")
        else codec.decode_scalar payload);
  }

let register (type value) (codec : value scalar_codec) : value operand_codec = scalar codec

let signed_zarith : Z.t operand_codec =
  let encode (value : Z.t) : Z.t =
    if Z.sign value < 0 then Z.pred (Z.mul (Z.abs value) (Z.of_int 2)) else Z.mul value (Z.of_int 2)
  in
  let decode (value : Z.t) : (Z.t, string) result =
    if Z.sign value < 0 then Error "expected a non-negative signed-integer payload"
    else if Z.testbit value 0 then Ok (Z.neg (Z.div (Z.succ value) (Z.of_int 2)))
    else Ok (Z.div value (Z.of_int 2))
  in
  scalar (scalar_codec ~name:"signed integer" ~encode:(fun value -> Ok (encode value)) ~decode)

let register_or_constant (register_codec : 'a scalar_codec) (constant_codec : 'b scalar_codec) :
    ('a, 'b) register_or_constant operand_codec =
  {
    span = 2;
    encode_operand =
      (function
      | Register register ->
          Result.map (fun payload -> (0, payload)) (register_codec.encode_scalar register)
      | Constant constant ->
          Result.map (fun payload -> (1, payload)) (constant_codec.encode_scalar constant));
    decode_operand =
      (fun variant payload ->
        match variant with
        | 0 -> Result.map (fun register -> Register register) (register_codec.decode_scalar payload)
        | 1 -> Result.map (fun constant -> Constant constant) (constant_codec.decode_scalar payload)
        | _ -> Error "register-or-constant operand has an invalid opcode variant");
  }

let pair (left : 'a operand_codec) (right : 'b operand_codec) : ('a * 'b) operand_codec =
  {
    span = multiply_spans left.span right.span;
    encode_operand =
      (fun (left_value, right_value) ->
        match (left.encode_operand left_value, right.encode_operand right_value) with
        | Ok (left_variant, left_payload), Ok (right_variant, right_payload) ->
            Ok
              ( (left_variant * right.span) + right_variant,
                encode_signed_pair left_payload right_payload )
        | Error message, _ | _, Error message -> Error message);
    decode_operand =
      (fun variant payload ->
        if variant < 0 || variant >= multiply_spans left.span right.span then
          Error "tuple operand has an invalid opcode variant"
        else
          let left_variant = variant / right.span in
          let right_variant = variant mod right.span in
          let left_payload, right_payload = decode_signed_pair payload in
          match
            ( left.decode_operand left_variant left_payload,
              right.decode_operand right_variant right_payload )
          with
          | Ok left_value, Ok right_value -> Ok (left_value, right_value)
          | Error message, _ | _, Error message -> Error message);
  }

let triple (first : 'a operand_codec) (second : 'b operand_codec) (third : 'c operand_codec) :
    ('a * 'b * 'c) operand_codec =
  let nested = pair first (pair second third) in
  {
    span = nested.span;
    encode_operand = (fun (first, second, third) -> nested.encode_operand (first, (second, third)));
    decode_operand =
      (fun variant payload ->
        Result.map
          (fun (first, (second, third)) -> (first, second, third))
          (nested.decode_operand variant payload));
  }

type 'instruction encoding_pattern =
  | Encoding_pattern : {
      name : string;
      operand_codec : 'operand operand_codec;
      construct : 'operand -> 'instruction;
      project : 'instruction -> 'operand option;
    }
      -> 'instruction encoding_pattern

let encoding_pattern ~(name : string) (operand_codec : 'a operand_codec) ~(construct : 'a -> 'b)
    ~(project : 'b -> 'a option) : 'b encoding_pattern =
  Encoding_pattern { name; operand_codec; construct; project }

type 'instruction compiled_pattern =
  | Compiled_pattern : {
      name : string;
      first_opcode : int;
      operand_codec : 'operand operand_codec;
      construct : 'operand -> 'instruction;
      project : 'instruction -> 'operand option;
    }
      -> 'instruction compiled_pattern

type 'instruction t = { patterns : 'instruction compiled_pattern list }

type 'instruction projection =
  | Projection : {
      name : string;
      first_opcode : int;
      operand_codec : 'operand operand_codec;
      operand : 'operand;
    }
      -> 'instruction projection

let pattern_name (Encoding_pattern pattern : 'a encoding_pattern) : string = pattern.name

let duplicate_name_errors (patterns : 'a encoding_pattern list) : error list =
  let seen = Hashtbl.create (List.length patterns) in
  List.filter_map
    (fun pattern ->
      let name = pattern_name pattern in
      if Hashtbl.mem seen name then Some (Duplicate_pattern_name name)
      else (
        Hashtbl.add seen name ();
        None))
    patterns

let compile (patterns : 'a encoding_pattern list) : ('a t, error list) result =
  let errors = ref (duplicate_name_errors patterns) in
  let cursor = ref 0 in
  let compiled = ref [] in
  List.iter
    (fun (Encoding_pattern pattern) ->
      let first_opcode = !cursor in
      if first_opcode > opcode_count || pattern.operand_codec.span > opcode_count - first_opcode
      then (
        errors :=
          Opcode_overflow
            { pattern_name = pattern.name; first_opcode; span = pattern.operand_codec.span }
          :: !errors;
        cursor := opcode_count + 1)
      else (
        compiled :=
          Compiled_pattern
            {
              name = pattern.name;
              first_opcode;
              operand_codec = pattern.operand_codec;
              construct = pattern.construct;
              project = pattern.project;
            }
          :: !compiled;
        cursor := first_opcode + pattern.operand_codec.span))
    patterns;
  match List.rev !errors with
  | _ :: _ as errors -> Error errors
  | [] -> Ok { patterns = List.rev !compiled }

let encode (codec : 'a t) (instruction : 'a) : (Z.t, error) result =
  let projected =
    List.filter_map
      (fun (Compiled_pattern pattern) ->
        Option.map
          (fun operand ->
            Projection
              {
                name = pattern.name;
                first_opcode = pattern.first_opcode;
                operand_codec = pattern.operand_codec;
                operand;
              })
          (pattern.project instruction))
      codec.patterns
  in
  match projected with
  | [] -> Error Unrecognized_instruction
  | [ Projection projected ] -> (
      match projected.operand_codec.encode_operand projected.operand with
      | Error message -> Error (Invalid_operand { pattern_name = projected.name; message })
      | Ok (variant, payload) ->
          if variant < 0 || variant >= projected.operand_codec.span then
            Error
              (Invalid_operand
                 {
                   pattern_name = projected.name;
                   message = "operand codec produced an invalid opcode variant";
                 })
          else if Z.sign payload < 0 then
            Error
              (Invalid_operand
                 {
                   pattern_name = projected.name;
                   message = "operand codec produced a negative top-level payload";
                 })
          else
            let opcode = Z.of_int (projected.first_opcode + variant) in
            Ok Z.(logor opcode (shift_left payload 8)))
  | projected ->
      let names = List.map (fun (Projection projected) -> projected.name) projected in
      Error (Ambiguous_instruction names)

let decode (codec : 'a t) (encoded : Z.t) : ('a, error) result =
  if Z.sign encoded < 0 then Error (Negative_encoding encoded)
  else
    let opcode = Z.to_int (Z.extract encoded 0 8) in
    let payload = Z.shift_right encoded 8 in
    let selected =
      List.find_opt
        (fun (Compiled_pattern pattern) ->
          opcode >= pattern.first_opcode
          && opcode < pattern.first_opcode + pattern.operand_codec.span)
        codec.patterns
    in
    match selected with
    | None -> Error (Unknown_opcode opcode)
    | Some (Compiled_pattern pattern) -> (
        let variant = opcode - pattern.first_opcode in
        match pattern.operand_codec.decode_operand variant payload with
        | Ok operand -> Ok (pattern.construct operand)
        | Error message ->
            Error (Malformed_encoding { opcode; pattern_name = pattern.name; message }))
