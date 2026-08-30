(** Runtime combinators for declaring symmetric instruction encoders and decoders.

    A declaration has four layers: a [scalar_codec] handles one atomic value; an [operand_codec]
    composes typed operands and any opcode variants; an [encoding_pattern] associates one operand
    codec with an instruction constructor; and [compile] assigns every pattern a contiguous opcode
    range, starting at zero in declaration order.

    Encoded instructions are arbitrary-precision non-negative integers. The low eight bits select
    one of 256 opcodes and all higher bits are the operand payload. Pattern order and operand spans
    are therefore part of a backend's numeric encoding, not merely an implementation detail. *)

(** Structured declaration, encoding, and decoding failures. Compilation reports declaration errors
    as a list. Encoding distinguishes missing or overlapping projectors from an operand-domain
    failure; decoding distinguishes a negative integer, an unassigned opcode, and a malformed
    payload in an assigned range. *)
type error =
  | Duplicate_pattern_name of string
  | Opcode_overflow of { pattern_name : string; first_opcode : int; span : int }
  | Unrecognized_instruction
  | Ambiguous_instruction of string list
  | Invalid_operand of { pattern_name : string; message : string }
  | Negative_encoding of Z.t
  | Unknown_opcode of int
  | Malformed_encoding of { opcode : int; pattern_name : string; message : string }

val error_message : error -> string
(** Stable human-readable rendering of structured codec failures. *)

type 'a scalar_codec
(** A bidirectional mapping for one typed atomic value. Authors must make [encode] and [decode]
    inverses on the accepted domain and return [Error] for external values outside it. The codec
    does not catch exceptions or validate this law. Scalar payloads may be signed while nested in a
    pair; only the final top-level operand payload is required to be non-negative. *)

val scalar_codec :
  name:string ->
  encode:('a -> (Z.t, string) result) ->
  decode:(Z.t -> ('a, string) result) ->
  'a scalar_codec
(** Declare a scalar codec. [name] appears in composed error messages; construction itself performs
    no eager validation. *)

val zarith : Z.t scalar_codec
(** Identity codec for arbitrary signed Zarith integers. A negative value must be nested inside a
    pair, or encoded with [signed_zarith], before it can be a top-level instruction payload. *)

val nonnegative_zarith : Z.t scalar_codec
(** Identity codec restricted to non-negative values in both directions. *)

val enum : name:string -> 'a list -> 'a scalar_codec
(** [enum] uses zero-based declaration order as the scalar encoding. Values are compared
    structurally and must be unique for round-trip symmetry. Unknown values and indexes are errors.
*)

type ('register, 'constant) register_or_constant = Register of 'register | Constant of 'constant

type 'a operand_codec
(** A typed operand codec owns an opcode [span] and maps a value to a variant within that span plus
    a Zarith payload. The representation is hidden so composition preserves span/variant invariants.
*)

val unit : unit operand_codec
(** One opcode variant with a zero payload; decoding rejects non-zero payload bits. *)

val scalar : 'a scalar_codec -> 'a operand_codec
(** Lift a scalar into a one-variant operand codec. *)

val register : 'register scalar_codec -> 'register operand_codec
(** Naming alias for [scalar], used to make instruction declarations read by role. *)

val signed_zarith : Z.t operand_codec
(** One-variant signed integer codec using a non-negative even/odd mapping: non-negative values map
    to even payloads and negative values to odd payloads. *)

val register_or_constant :
  'register scalar_codec ->
  'constant scalar_codec ->
  ('register, 'constant) register_or_constant operand_codec
(** A two-variant codec: variant zero carries the register scalar and variant one the constant. *)

val pair : 'a operand_codec -> 'b operand_codec -> ('a * 'b) operand_codec
(** Product codec. Its span is the product of child spans and variants use mixed-radix order
    [left_variant * right_span + right_variant]. The two possibly signed, arbitrary-precision
    payloads are packed reversibly by reserving two sign bits and interleaving their magnitude bits;
    neither component has a fixed field width. *)

val triple :
  'a operand_codec -> 'b operand_codec -> 'c operand_codec -> ('a * 'b * 'c) operand_codec
(** Typed convenience wrapper over [pair first (pair second third)], with the same layout. *)

type 'instruction encoding_pattern
(** An existential package tying one operand type to its codec, constructor, and partial projector.
    The hidden operand type lets patterns for different instruction constructors share one list
    without unsafe casts. *)

val encoding_pattern :
  name:string ->
  'operand operand_codec ->
  construct:('operand -> 'instruction) ->
  project:('instruction -> 'operand option) ->
  'instruction encoding_pattern
(** Declare one instruction shape. [construct] and [project] must be inverses for this pattern, and
    [project] must return [None] for every value owned by a different pattern. These laws are
    checked dynamically only insofar as [encode] requires exactly one successful projector. *)

type 'instruction t
(** Immutable compiled layout. *)

val compile : 'instruction encoding_pattern list -> ('instruction t, error list) result
(** Assign consecutive opcode ranges from zero in declaration order. The operation is all-or-error:
    duplicate names and any span extending beyond the 256-entry field reject the complete layout.
    Product spans saturate just beyond 256 so host-integer overflow cannot hide an oversized range.
*)

val encode : 'instruction t -> 'instruction -> (Z.t, error) result
(** Select exactly one pattern through its projector, validate the emitted variant and non-negative
    top-level payload, then store [first_opcode + variant] in bits 0--7 and the payload above it. *)

val decode : 'instruction t -> Z.t -> ('instruction, error) result
(** Reject negative encodings, select the compiled range named by bits 0--7, decode the high payload
    with that range's operand codec, and reconstruct the typed instruction. Malformed external
    integers are returned as errors rather than exceptions. *)
