(** Repository-private combinators shared by the handwritten backends for tagged capability
    metadata.

    A finite scalar declaration is the single source of truth for both directions. Payload codecs
    either carry one scalar or pack two scalars into fixed-width low and high fields. Encoding
    patterns attach the fixed three-bit metadata tag and the backend's public decoder messages.
    [compile] validates the complete layout before it can be used. This module is deliberately not
    re-exported by {!Cerise}. *)

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

val error_message : error -> string

type 'a finite_scalar

val finite_scalar : name:string -> ('a * Z.t) list -> 'a finite_scalar
(** Values are compared structurally. Reverse decoding is derived from the declared mappings. *)

type 'a payload_codec

val scalar_payload : 'a finite_scalar -> 'a payload_codec

val packed_pair :
  low_width:int -> high_width:int -> 'a finite_scalar -> 'b finite_scalar -> ('a * 'b) payload_codec
(** [packed_pair] places the first scalar in the low field and the second scalar in the high field.
    Both widths are checked during compilation, and decoding rejects bits beyond the two fields. *)

type 'a encoding_pattern

val encoding_pattern :
  name:string ->
  tag:int ->
  wrong_tag_error:string ->
  malformed_payload_error:string ->
  'a payload_codec ->
  'a encoding_pattern

type pattern

val pattern : 'a encoding_pattern -> pattern

type t

val compile : pattern list -> (t, error list) result
val encode : t -> 'a encoding_pattern -> 'a -> (Z.t, string) result
val decode : t -> 'a encoding_pattern -> Z.t -> ('a, string) result
