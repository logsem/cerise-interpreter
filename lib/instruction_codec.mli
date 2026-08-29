(** Runtime combinators for declaring symmetric instruction encoders and decoders. *)

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
type 'a shape

val unit : unit shape
val scalar : 'a scalar_codec -> 'a shape
val register : 'register scalar_codec -> 'register shape
val signed_zarith : Z.t shape

val register_or_constant :
  'register scalar_codec ->
  'constant scalar_codec ->
  ('register, 'constant) register_or_constant shape

val pair : 'a shape -> 'b shape -> ('a * 'b) shape
val triple : 'a shape -> 'b shape -> 'c shape -> ('a * 'b * 'c) shape
val variant_span : 'a shape -> int
val encode_signed_pair : Z.t -> Z.t -> Z.t

val decode_signed_pair : Z.t -> Z.t * Z.t
(** Bitwise interleaving with two low sign bits, matching the historical Cerise convention. *)

type 'instruction case

val case :
  name:string ->
  ?allocation:allocation ->
  'operand shape ->
  construct:('operand -> 'instruction) ->
  project:('instruction -> 'operand option) ->
  'instruction case

type 'instruction t

val compile : 'instruction case list -> ('instruction t, error list) result
val encode : 'instruction t -> 'instruction -> (Z.t, error) result
val decode : 'instruction t -> Z.t -> ('instruction, error) result

val allocations : 'instruction t -> (string * int * int) list
(** Case name, first opcode, and contiguous opcode span, in declaration order. *)
