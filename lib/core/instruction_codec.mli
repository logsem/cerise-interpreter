(** Runtime combinators for declaring symmetric instruction encoders and decoders.

    A declaration has four layers: a [scalar_codec] handles one atomic value; an [operand_codec]
    composes typed operands and any opcode variants; an [encoding_pattern] associates one operand
    codec with an instruction constructor; and [compile] assigns every pattern a contiguous opcode
    range, starting at zero in declaration order. *)

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

type 'a scalar_codec

val scalar_codec :
  name:string ->
  encode:('a -> (Z.t, string) result) ->
  decode:(Z.t -> ('a, string) result) ->
  'a scalar_codec

val zarith : Z.t scalar_codec
val nonnegative_zarith : Z.t scalar_codec

val enum : name:string -> 'a list -> 'a scalar_codec
(** [enum] uses the declaration order as the scalar encoding. Values are compared structurally. *)

type ('register, 'constant) register_or_constant = Register of 'register | Constant of 'constant
type 'a operand_codec

val unit : unit operand_codec
val scalar : 'a scalar_codec -> 'a operand_codec
val register : 'register scalar_codec -> 'register operand_codec
val signed_zarith : Z.t operand_codec

val register_or_constant :
  'register scalar_codec ->
  'constant scalar_codec ->
  ('register, 'constant) register_or_constant operand_codec

val pair : 'a operand_codec -> 'b operand_codec -> ('a * 'b) operand_codec

val triple :
  'a operand_codec -> 'b operand_codec -> 'c operand_codec -> ('a * 'b * 'c) operand_codec

type 'instruction encoding_pattern

val encoding_pattern :
  name:string ->
  'operand operand_codec ->
  construct:('operand -> 'instruction) ->
  project:('instruction -> 'operand option) ->
  'instruction encoding_pattern

type 'instruction t

val compile : 'instruction encoding_pattern list -> ('instruction t, error list) result
val encode : 'instruction t -> 'instruction -> (Z.t, error) result
val decode : 'instruction t -> Z.t -> ('instruction, error) result
