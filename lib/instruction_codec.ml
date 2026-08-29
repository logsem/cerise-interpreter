type allocation = Auto | Fixed of int

type error =
  | Duplicate_case_name of string
  | Invalid_fixed_opcode of { case_name : string; opcode : int }
  | Opcode_collision of { opcode : int; first_case : string; second_case : string }
  | Opcode_overflow of { case_name : string; first_opcode : int; span : int }
  | Unrecognized_instruction
  | Ambiguous_instruction of string list
  | Invalid_operand of { case_name : string; message : string }
  | Negative_encoding of Z.t
  | Unknown_opcode of int
  | Malformed_encoding of { opcode : int; case_name : string; message : string }

let error_message = function
  | Duplicate_case_name name -> Printf.sprintf "Duplicate instruction case name %S." name
  | Invalid_fixed_opcode { case_name; opcode } ->
      Printf.sprintf "Instruction case %S has invalid fixed opcode %d." case_name opcode
  | Opcode_collision { opcode; first_case; second_case } ->
      Printf.sprintf "Opcode 0x%02x is allocated by both %S and %S." opcode first_case second_case
  | Opcode_overflow { case_name; first_opcode; span } ->
      Printf.sprintf "Instruction case %S needs %d opcodes starting at %d, beyond the 8-bit field."
        case_name span first_opcode
  | Unrecognized_instruction -> "No instruction case projected the value being encoded."
  | Ambiguous_instruction names ->
      Printf.sprintf "Multiple instruction cases projected the value: %s."
        (String.concat ", " names)
  | Invalid_operand { case_name; message } ->
      Printf.sprintf "Invalid operand for instruction case %S: %s" case_name message
  | Negative_encoding value ->
      Printf.sprintf "Instruction encodings must be non-negative, got %s." (Z.to_string value)
  | Unknown_opcode opcode -> Printf.sprintf "Unknown instruction opcode 0x%02x." opcode
  | Malformed_encoding { opcode; case_name; message } ->
      Printf.sprintf "Malformed encoding for %S at opcode 0x%02x: %s" case_name opcode message

type 'a scalar_codec = {
  scalar_name : string;
  encode_scalar : 'a -> (Z.t, string) result;
  decode_scalar : Z.t -> ('a, string) result;
}

let scalar_codec ~name ~encode ~decode =
  { scalar_name = name; encode_scalar = encode; decode_scalar = decode }

let zarith = scalar_codec ~name:"integer" ~encode:Result.ok ~decode:Result.ok

let nonnegative_zarith =
  let check value =
    if Z.sign value < 0 then Error "expected a non-negative integer" else Ok value
  in
  scalar_codec ~name:"non-negative integer" ~encode:check ~decode:check

let enum ~name values =
  let rec encode index value = function
    | [] -> Error (Printf.sprintf "unknown %s value" name)
    | candidate :: _ when candidate = value -> Ok (Z.of_int index)
    | _ :: rest -> encode (index + 1) value rest
  in
  let decode encoded =
    if Z.sign encoded < 0 || not (Z.fits_int encoded) then
      Error (Printf.sprintf "invalid %s encoding %s" name (Z.to_string encoded))
    else
      match List.nth_opt values (Z.to_int encoded) with
      | Some value -> Ok value
      | None -> Error (Printf.sprintf "invalid %s encoding %s" name (Z.to_string encoded))
  in
  scalar_codec ~name ~encode:(fun value -> encode 0 value values) ~decode

type ('register, 'constant) register_or_constant = Register of 'register | Constant of 'constant

let rec split_unsigned value =
  if Z.equal value Z.zero then (Z.zero, Z.zero)
  else
    let first_bit = Z.extract value 0 1 in
    let second_bit = Z.extract value 1 1 in
    let first, second = split_unsigned (Z.shift_right value 2) in
    (Z.logor first_bit (Z.shift_left first 1), Z.logor second_bit (Z.shift_left second 1))

let rec interleave_unsigned first second =
  if Z.equal first Z.zero && Z.equal second Z.zero then Z.zero
  else
    let first_bit = Z.extract first 0 1 in
    let second_bit = Z.shift_left (Z.extract second 0 1) 1 in
    Z.logor (Z.logor first_bit second_bit)
      (Z.shift_left (interleave_unsigned (Z.shift_right first 1) (Z.shift_right second 1)) 2)

let encode_signed_pair first second =
  let signs =
    match (Z.sign second < 0, Z.sign first < 0) with
    | false, false -> Z.zero
    | false, true -> Z.one
    | true, false -> Z.of_int 2
    | true, true -> Z.of_int 3
  in
  Z.logor signs (Z.shift_left (interleave_unsigned (Z.abs first) (Z.abs second)) 2)

let decode_signed_pair encoded =
  let first_negative = Z.testbit encoded 0 in
  let second_negative = Z.testbit encoded 1 in
  let first, second = split_unsigned (Z.shift_right encoded 2) in
  ((if first_negative then Z.neg first else first), if second_negative then Z.neg second else second)

type 'a shape = {
  span : int;
  encode_shape : 'a -> (int * Z.t, string) result;
  decode_shape : int -> Z.t -> ('a, string) result;
}

let variant_span shape = shape.span

let unit =
  {
    span = 1;
    encode_shape = (fun () -> Ok (0, Z.zero));
    decode_shape =
      (fun variant payload ->
        if variant <> 0 then Error "unit operand has an invalid opcode variant"
        else if not (Z.equal payload Z.zero) then Error "unit operand has a non-zero payload"
        else Ok ());
  }

let scalar codec =
  {
    span = 1;
    encode_shape =
      (fun value -> Result.map (fun encoded -> (0, encoded)) (codec.encode_scalar value));
    decode_shape =
      (fun variant payload ->
        if variant <> 0 then Error (codec.scalar_name ^ " has an invalid opcode variant")
        else codec.decode_scalar payload);
  }

let register = scalar

let signed_zarith =
  let encode value =
    if Z.sign value < 0 then Z.pred (Z.mul (Z.abs value) (Z.of_int 2)) else Z.mul value (Z.of_int 2)
  in
  let decode value =
    if Z.sign value < 0 then Error "expected a non-negative signed-integer payload"
    else if Z.testbit value 0 then Ok (Z.neg (Z.div (Z.succ value) (Z.of_int 2)))
    else Ok (Z.div value (Z.of_int 2))
  in
  scalar (scalar_codec ~name:"signed integer" ~encode:(fun value -> Ok (encode value)) ~decode)

let register_or_constant register_codec constant_codec =
  {
    span = 2;
    encode_shape =
      (function
      | Register register ->
          Result.map (fun payload -> (0, payload)) (register_codec.encode_scalar register)
      | Constant constant ->
          Result.map (fun payload -> (1, payload)) (constant_codec.encode_scalar constant));
    decode_shape =
      (fun variant payload ->
        match variant with
        | 0 -> Result.map (fun register -> Register register) (register_codec.decode_scalar payload)
        | 1 -> Result.map (fun constant -> Constant constant) (constant_codec.decode_scalar payload)
        | _ -> Error "register-or-constant operand has an invalid opcode variant");
  }

let pair left right =
  {
    span = left.span * right.span;
    encode_shape =
      (fun (left_value, right_value) ->
        match (left.encode_shape left_value, right.encode_shape right_value) with
        | Ok (left_variant, left_payload), Ok (right_variant, right_payload) ->
            Ok
              ( (left_variant * right.span) + right_variant,
                encode_signed_pair left_payload right_payload )
        | Error message, _ | _, Error message -> Error message);
    decode_shape =
      (fun variant payload ->
        if variant < 0 || variant >= left.span * right.span then
          Error "tuple operand has an invalid opcode variant"
        else
          let left_variant = variant / right.span in
          let right_variant = variant mod right.span in
          let left_payload, right_payload = decode_signed_pair payload in
          match
            ( left.decode_shape left_variant left_payload,
              right.decode_shape right_variant right_payload )
          with
          | Ok left_value, Ok right_value -> Ok (left_value, right_value)
          | Error message, _ | _, Error message -> Error message);
  }

let triple first second third =
  let nested = pair first (pair second third) in
  {
    span = nested.span;
    encode_shape = (fun (first, second, third) -> nested.encode_shape (first, (second, third)));
    decode_shape =
      (fun variant payload ->
        Result.map
          (fun (first, (second, third)) -> (first, second, third))
          (nested.decode_shape variant payload));
  }

type 'instruction case =
  | Case : {
      name : string;
      allocation : allocation;
      shape : 'operand shape;
      construct : 'operand -> 'instruction;
      project : 'instruction -> 'operand option;
    }
      -> 'instruction case

let case ~name ?(allocation = Auto) shape ~construct ~project =
  Case { name; allocation; shape; construct; project }

type 'instruction compiled_case =
  | Compiled_case : {
      name : string;
      first_opcode : int;
      shape : 'operand shape;
      construct : 'operand -> 'instruction;
      project : 'instruction -> 'operand option;
    }
      -> 'instruction compiled_case

type 'instruction t = { cases : 'instruction compiled_case list }

type 'instruction projection =
  | Projection : {
      name : string;
      first_opcode : int;
      shape : 'operand shape;
      operand : 'operand;
    }
      -> 'instruction projection

let case_name (Case case) = case.name

let duplicate_name_errors cases =
  let seen = Hashtbl.create (List.length cases) in
  List.filter_map
    (fun case ->
      let name = case_name case in
      if Hashtbl.mem seen name then Some (Duplicate_case_name name)
      else (
        Hashtbl.add seen name ();
        None))
    cases

let compile cases =
  let owners : string option array = Array.make 256 None in
  let starts : (string, int) Hashtbl.t = Hashtbl.create (List.length cases) in
  let errors = ref (duplicate_name_errors cases) in
  let reserve name first span =
    if first < 0 then errors := Invalid_fixed_opcode { case_name = name; opcode = first } :: !errors
    else if span <= 0 || first + span > 256 then
      errors := Opcode_overflow { case_name = name; first_opcode = first; span } :: !errors
    else
      for opcode = first to first + span - 1 do
        match owners.(opcode) with
        | None -> owners.(opcode) <- Some name
        | Some first_case ->
            errors := Opcode_collision { opcode; first_case; second_case = name } :: !errors
      done
  in
  List.iter
    (fun (Case case) ->
      match case.allocation with
      | Auto -> ()
      | Fixed first ->
          Hashtbl.replace starts case.name first;
          reserve case.name first case.shape.span)
    cases;
  let cursor = ref 0 in
  let rec find_free span candidate =
    if candidate + span > 256 then None
    else
      let rec range_is_free opcode =
        opcode = candidate + span || (Option.is_none owners.(opcode) && range_is_free (opcode + 1))
      in
      if range_is_free candidate then Some candidate else find_free span (candidate + 1)
  in
  List.iter
    (fun (Case case) ->
      match case.allocation with
      | Fixed _ -> ()
      | Auto -> (
          match find_free case.shape.span !cursor with
          | None ->
              errors :=
                Opcode_overflow
                  { case_name = case.name; first_opcode = !cursor; span = case.shape.span }
                :: !errors
          | Some first ->
              Hashtbl.replace starts case.name first;
              reserve case.name first case.shape.span;
              cursor := first + case.shape.span))
    cases;
  match List.rev !errors with
  | _ :: _ as errors -> Error errors
  | [] ->
      let compiled =
        List.map
          (fun (Case case) ->
            Compiled_case
              {
                name = case.name;
                first_opcode = Hashtbl.find starts case.name;
                shape = case.shape;
                construct = case.construct;
                project = case.project;
              })
          cases
      in
      Ok { cases = compiled }

let allocations codec =
  List.map (fun (Compiled_case case) -> (case.name, case.first_opcode, case.shape.span)) codec.cases

let encode codec instruction =
  let projected =
    List.filter_map
      (fun (Compiled_case case) ->
        Option.map
          (fun operand ->
            Projection
              { name = case.name; first_opcode = case.first_opcode; shape = case.shape; operand })
          (case.project instruction))
      codec.cases
  in
  match projected with
  | [] -> Error Unrecognized_instruction
  | [ Projection projected ] -> (
      match projected.shape.encode_shape projected.operand with
      | Error message -> Error (Invalid_operand { case_name = projected.name; message })
      | Ok (variant, payload) ->
          if variant < 0 || variant >= projected.shape.span then
            Error
              (Invalid_operand
                 {
                   case_name = projected.name;
                   message = "shape produced an invalid opcode variant";
                 })
          else if Z.sign payload < 0 then
            Error
              (Invalid_operand
                 {
                   case_name = projected.name;
                   message = "shape produced a negative top-level payload";
                 })
          else
            let opcode = Z.of_int (projected.first_opcode + variant) in
            Ok Z.(logor opcode (shift_left payload 8)))
  | projected ->
      let names = List.map (fun (Projection projected) -> projected.name) projected in
      Error (Ambiguous_instruction names)

let decode codec encoded =
  if Z.sign encoded < 0 then Error (Negative_encoding encoded)
  else
    let opcode = Z.to_int (Z.extract encoded 0 8) in
    let payload = Z.shift_right encoded 8 in
    let selected =
      List.find_opt
        (fun (Compiled_case case) ->
          opcode >= case.first_opcode && opcode < case.first_opcode + case.shape.span)
        codec.cases
    in
    match selected with
    | None -> Error (Unknown_opcode opcode)
    | Some (Compiled_case case) -> (
        let variant = opcode - case.first_opcode in
        match case.shape.decode_shape variant payload with
        | Ok operand -> Ok (case.construct operand)
        | Error message -> Error (Malformed_encoding { opcode; case_name = case.name; message }))
