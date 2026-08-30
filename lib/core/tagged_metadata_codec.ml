(** Declarative tagged-metadata encoding shared by the handwritten backends. The low three bits are
    a fixed pattern tag; the remaining arbitrary-precision bits hold the typed payload. *)

type error =
  | Invalid_pattern_name of string
  | Invalid_scalar_name of string
  | Invalid_tag of { pattern_name : string; tag : int }
  | Duplicate_pattern_name of string
  | Duplicate_tag of int
  | Duplicate_scalar_encoding of { scalar_name : string; encoding : Z.t }
  | Duplicate_scalar_value of string
  | Negative_scalar_encoding of { scalar_name : string; encoding : Z.t }
  | Invalid_field_width of { pattern_name : string; scalar_name : string; width : int }
  | Field_width_overflow of {
      pattern_name : string;
      scalar_name : string;
      encoding : Z.t;
      width : int;
    }

let error_message (error : error) : string =
  match error with
  | Invalid_pattern_name name ->
      Printf.sprintf "Invalid tagged-metadata encoding pattern name %S." name
  | Invalid_scalar_name name -> Printf.sprintf "Invalid tagged-metadata scalar name %S." name
  | Invalid_tag { pattern_name; tag } ->
      Printf.sprintf "Tagged-metadata encoding pattern %S has invalid three-bit tag %d."
        pattern_name tag
  | Duplicate_pattern_name name ->
      Printf.sprintf "Duplicate tagged-metadata encoding pattern name %S." name
  | Duplicate_tag tag -> Printf.sprintf "Duplicate tagged-metadata tag %d." tag
  | Duplicate_scalar_encoding { scalar_name; encoding } ->
      Printf.sprintf "Tagged-metadata scalar %S declares encoding %s more than once." scalar_name
        (Z.to_string encoding)
  | Duplicate_scalar_value scalar_name ->
      Printf.sprintf "Tagged-metadata scalar %S declares a value more than once." scalar_name
  | Negative_scalar_encoding { scalar_name; encoding } ->
      Printf.sprintf "Tagged-metadata scalar %S has negative encoding %s." scalar_name
        (Z.to_string encoding)
  | Invalid_field_width { pattern_name; scalar_name; width } ->
      Printf.sprintf "Tagged-metadata encoding pattern %S gives scalar %S invalid field width %d."
        pattern_name scalar_name width
  | Field_width_overflow { pattern_name; scalar_name; encoding; width } ->
      Printf.sprintf
        "Tagged-metadata encoding pattern %S cannot fit scalar %S encoding %s in %d bits."
        pattern_name scalar_name (Z.to_string encoding) width

type 'a finite_scalar = { name : string; mappings : ('a * Z.t) list }

(* Keeping the forward and reverse directions as one finite relation is the central maintenance
   invariant. Compilation checks uniqueness before the linear searches below are allowed, making
   structural lookup deterministic without a backend-owned reverse decoder. *)

let finite_scalar ~(name : string) (mappings : ('a * Z.t) list) : 'a finite_scalar =
  { name; mappings }

type 'a payload_codec =
  | Scalar_payload : 'a finite_scalar -> 'a payload_codec
  | Packed_pair : {
      low_width : int;
      high_width : int;
      low : 'a finite_scalar;
      high : 'b finite_scalar;
    }
      -> ('a * 'b) payload_codec

(* This GADT records the semantic type described by a layout. In the pair case it also proves that
   the low and high scalar declarations correspond to the two components supplied to [encode] and
   reconstructed by [decode]. *)

let scalar_payload (scalar : 'a finite_scalar) : 'a payload_codec = Scalar_payload scalar

let packed_pair ~(low_width : int) ~(high_width : int) (low : 'a finite_scalar)
    (high : 'b finite_scalar) : ('a * 'b) payload_codec =
  Packed_pair { low_width; high_width; low; high }

type 'a encoding_pattern = {
  id : int;
  name : string;
  tag : int;
  wrong_tag_error : string;
  malformed_payload_error : string;
  payload_codec : 'a payload_codec;
}

(* Pattern identity is generative. A compiled codec stores identities rather than existential typed
   records, so membership can be checked without unsafe casts or relying on names/tags as proxies for
   type equality. Creating another equal-looking declaration does not make it part of the layout. *)
let next_pattern_id = ref 0

let encoding_pattern ~(name : string) ~(tag : int) ~(wrong_tag_error : string)
    ~(malformed_payload_error : string) (payload_codec : 'a payload_codec) : 'a encoding_pattern =
  let id = !next_pattern_id in
  incr next_pattern_id;
  { id; name; tag; wrong_tag_error; malformed_payload_error; payload_codec }

type pattern = Pattern : 'a encoding_pattern -> pattern

(* The existential wrapper erases only the payload type needed by clients; validation itself needs
   names, tags, and payload layouts but never constructs a value of that hidden type. *)

let pattern (pattern : 'a encoding_pattern) : pattern = Pattern pattern

type t = { pattern_ids : int list }

let valid_name (name : string) : bool = not (String.equal (String.trim name) "")

let scalar_errors (scalar : 'a finite_scalar) : error list =
  let errors = ref [] in
  if not (valid_name scalar.name) then errors := Invalid_scalar_name scalar.name :: !errors;
  let seen_encodings = ref [] in
  let seen_values = ref [] in
  List.iter
    (fun (value, encoding) ->
      if Z.sign encoding < 0 then
        errors := Negative_scalar_encoding { scalar_name = scalar.name; encoding } :: !errors;
      if List.exists (Z.equal encoding) !seen_encodings then
        errors := Duplicate_scalar_encoding { scalar_name = scalar.name; encoding } :: !errors
      else seen_encodings := encoding :: !seen_encodings;
      if List.exists (fun candidate -> candidate = value) !seen_values then
        errors := Duplicate_scalar_value scalar.name :: !errors
      else seen_values := value :: !seen_values)
    scalar.mappings;
  List.rev !errors

let field_errors ~(pattern_name : string) ~(width : int) (scalar : 'a finite_scalar) : error list =
  if width <= 0 then [ Invalid_field_width { pattern_name; scalar_name = scalar.name; width } ]
  else
    List.filter_map
      (fun (_, encoding) ->
        if Z.sign encoding >= 0 && Z.numbits encoding > width then
          Some (Field_width_overflow { pattern_name; scalar_name = scalar.name; encoding; width })
        else None)
      scalar.mappings

let payload_errors (type value) ~(pattern_name : string) (payload : value payload_codec) :
    error list =
  match payload with
  | Scalar_payload scalar -> scalar_errors scalar
  | Packed_pair { low_width; high_width; low; high } ->
      scalar_errors low @ scalar_errors high
      @ field_errors ~pattern_name ~width:low_width low
      @ field_errors ~pattern_name ~width:high_width high

let compile (patterns : pattern list) : (t, error list) result =
  (* Walk every declaration so callers receive the whole layout audit at once. No partially valid
     pattern set escapes: identities become usable only when every name, tag, scalar mapping, and
     packed width has passed validation. *)
  let errors = ref [] in
  let names = Hashtbl.create (List.length patterns) in
  let tags = Hashtbl.create (List.length patterns) in
  List.iter
    (fun (Pattern pattern) ->
      if not (valid_name pattern.name) then errors := Invalid_pattern_name pattern.name :: !errors;
      if pattern.tag < 0 || pattern.tag > 7 then
        errors := Invalid_tag { pattern_name = pattern.name; tag = pattern.tag } :: !errors;
      if Hashtbl.mem names pattern.name then
        errors := Duplicate_pattern_name pattern.name :: !errors
      else Hashtbl.add names pattern.name ();
      if Hashtbl.mem tags pattern.tag then errors := Duplicate_tag pattern.tag :: !errors
      else Hashtbl.add tags pattern.tag ();
      errors :=
        List.rev_append (payload_errors ~pattern_name:pattern.name pattern.payload_codec) !errors)
    patterns;
  match List.rev !errors with
  | _ :: _ as errors -> Error errors
  | [] -> Ok { pattern_ids = List.map (fun (Pattern pattern) -> pattern.id) patterns }

let compiled (codec : t) (pattern : 'a encoding_pattern) : bool =
  List.mem pattern.id codec.pattern_ids

let encode_scalar (scalar : 'a finite_scalar) (value : 'a) : (Z.t, string) result =
  match List.find_opt (fun (candidate, _) -> candidate = value) scalar.mappings with
  | Some (_, encoding) -> Ok encoding
  | None -> Error (Printf.sprintf "unknown %s value" scalar.name)

let decode_scalar (scalar : 'a finite_scalar) (encoding : Z.t) : ('a, string) result =
  match List.find_opt (fun (_, candidate) -> Z.equal candidate encoding) scalar.mappings with
  | Some (value, _) -> Ok value
  | None -> Error (Printf.sprintf "unknown %s encoding" scalar.name)

let encode_payload (type value) (payload : value payload_codec) (value : value) :
    (Z.t, string) result =
  match payload with
  | Scalar_payload scalar -> encode_scalar scalar value
  | Packed_pair { low_width; low; high; _ } ->
      (* Compilation proves both scalar encodings fit. Placing the high field above [low_width] and
         combining with [logor] is therefore concatenation rather than a potentially overlapping
         merge. *)
      let low_value, high_value = value in
      Result.bind (encode_scalar low low_value) (fun low_encoding ->
          Result.map
            (fun high_encoding -> Z.logor low_encoding (Z.shift_left high_encoding low_width))
            (encode_scalar high high_value))

let decode_payload (type value) (payload : value payload_codec) (encoding : Z.t) :
    (value, string) result =
  match payload with
  | Scalar_payload scalar -> decode_scalar scalar encoding
  | Packed_pair { low_width; high_width; low; high } ->
      (* Check the untruncated high remainder first. This rejects negative values and any bits beyond
         the declared pair instead of silently discarding them when the low field is extracted. *)
      let high_encoding = Z.shift_right encoding low_width in
      if Z.sign encoding < 0 || Z.numbits high_encoding > high_width then
        Error "packed payload exceeds its declared fields"
      else
        let low_encoding = Z.extract encoding 0 low_width in
        Result.bind (decode_scalar low low_encoding) (fun low_value ->
            Result.map
              (fun high_value -> (low_value, high_value))
              (decode_scalar high high_encoding))

let encode (codec : t) (pattern : 'a encoding_pattern) (value : 'a) : (Z.t, string) result =
  (* Membership by identity prevents using an individually well-formed pattern which was not part of
     the backend's validated layout. The fixed low tag remains independent of payload size. *)
  if not (compiled codec pattern) then
    Error (Printf.sprintf "tagged-metadata pattern %S was not compiled" pattern.name)
  else
    Result.map
      (fun payload -> Z.logor (Z.of_int pattern.tag) (Z.shift_left payload 3))
      (encode_payload pattern.payload_codec value)

let decode (codec : t) (pattern : 'a encoding_pattern) (encoded : Z.t) : ('a, string) result =
  (* Public backends historically expose exact decoder strings. Tag failures and all internal
     payload failures are deliberately collapsed to the two messages stored on the pattern while
     declaration-time problems remain structured [error] values from [compile]. *)
  if not (compiled codec pattern) then
    Error (Printf.sprintf "tagged-metadata pattern %S was not compiled" pattern.name)
  else if Z.sign encoded < 0 || not (Z.equal (Z.extract encoded 0 3) (Z.of_int pattern.tag)) then
    Error pattern.wrong_tag_error
  else
    match decode_payload pattern.payload_codec (Z.shift_right encoded 3) with
    | Ok value -> Ok value
    | Error _ -> Error pattern.malformed_payload_error
