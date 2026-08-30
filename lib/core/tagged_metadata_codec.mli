(** Repository-private combinators shared by the handwritten backends for tagged capability
    metadata.

    A finite scalar declaration is the single source of truth for both directions. Payload codecs
    either carry one scalar or pack two scalars into fixed-width low and high fields. Encoding
    patterns attach the fixed three-bit metadata tag and the backend's public decoder messages.
    [compile] validates the complete layout before it can be used. Encoded values are non-negative
    arbitrary-precision integers with the tag in bits 0--2 and the payload above bit 2. This module
    is deliberately not re-exported by {!Cerise}. *)

(** Declaration-time layout failures. Compilation accumulates invalid or duplicate pattern names or
    tags, scalar-mapping errors, and fixed-field-width errors instead of exposing a partial layout.
*)
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
(** Stable human-readable rendering of a structured declaration error. *)

type 'a finite_scalar
(** A named finite mapping between a semantic value and a non-negative integer. One declaration is
    used for both directions so a backend cannot accidentally maintain divergent encoders and
    decoders. *)

val finite_scalar : name:string -> ('a * Z.t) list -> 'a finite_scalar
(** Declare, but do not yet validate, a scalar. Values are compared structurally; the name must be
    non-empty, values and integer encodings must each be unique within the declaration, and
    encodings must be non-negative. Reverse decoding is derived from this same mapping list. *)

type 'a payload_codec
(** Typed payload layout. Its value type keeps the semantic value consumed by [encode] and produced
    by [decode] synchronized with the scalar declarations. *)

val scalar_payload : 'a finite_scalar -> 'a payload_codec
(** Store one scalar above the tag; reject payload integers absent from its mapping. *)

val packed_pair :
  low_width:int -> high_width:int -> 'a finite_scalar -> 'b finite_scalar -> ('a * 'b) payload_codec
(** [packed_pair] places the first scalar in the low field and the second scalar in the high field.
    Both widths must be positive and every declared scalar encoding must fit its field; these rules
    are checked during compilation. Decoding rejects negative payloads and bits beyond the two
    fields rather than truncating them. *)

type 'a encoding_pattern
(** A typed metadata shape with a unique private identity. The identity, rather than equal-looking
    fields, proves that the exact declaration passed to [encode] or [decode] belongs to a compiled
    layout. *)

val encoding_pattern :
  name:string ->
  tag:int ->
  wrong_tag_error:string ->
  malformed_payload_error:string ->
  'a payload_codec ->
  'a encoding_pattern
(** Declare a named pattern. [tag] must fit in three bits and be unique in its layout.
    [wrong_tag_error] is the backend's public error for negative values or a different low tag;
    [malformed_payload_error] intentionally hides internal scalar/field decoding details. *)

type pattern
(** Existential wrapper allowing patterns with different payload types to be validated together. *)

val pattern : 'a encoding_pattern -> pattern
(** Erase only the payload type for [compile]; the pattern's private identity remains. *)

type t
(** Validated set of pattern identities. Typed patterns retain their own encode/decode data. *)

val compile : pattern list -> (t, error list) result
(** Validate the complete layout: non-empty pattern/scalar names, unique pattern names and tags,
    tags in 0--7, bijective non-negative mappings within each scalar, and packed field
    widths/capacities. Returns all detected declaration errors or an immutable all-valid membership
    set. *)

val encode : t -> 'a encoding_pattern -> 'a -> (Z.t, string) result
(** Require that the exact pattern was compiled, encode its payload, then place the pattern tag in
    bits 0--2 and shift the payload above it. Unknown semantic scalar values remain descriptive
    errors from their declarations. *)

val decode : t -> 'a encoding_pattern -> Z.t -> ('a, string) result
(** Require that the exact pattern was compiled, reject a negative or wrong-tag integer with the
    declared public tag error, and decode the remaining high bits. Any unknown scalar, field
    overflow, or otherwise malformed payload becomes the declared public malformed-payload error. *)
