(** Handwritten, untrusted boundary codec for the extracted Griotte machine. This module
    deliberately does not depend on [Instruction_codec] or [Griotte_codec]. Its fixed numbers mirror
    the Rocq extraction parameters. *)

val encode : Griotte_ast.instruction -> (Z.t, string) result
val decode : Z.t -> (Griotte_ast.instruction, string) result
val encode_permission : Griotte_ast.permission -> Z.t
val decode_permission : Z.t -> (Griotte_ast.permission, string) result
val encode_locality : Griotte_ast.locality -> Z.t
val encode_permission_locality : Griotte_ast.permission -> Griotte_ast.locality -> Z.t

val decode_permission_locality :
  Z.t -> (Griotte_ast.permission * Griotte_ast.locality, string) result

val encode_seal_permission : Griotte_ast.seal_permission -> Z.t
val decode_seal_permission : Z.t -> (Griotte_ast.seal_permission, string) result
val encode_seal_permission_locality : Griotte_ast.seal_permission -> Griotte_ast.locality -> Z.t

val decode_seal_permission_locality :
  Z.t -> (Griotte_ast.seal_permission * Griotte_ast.locality, string) result

val encode_word_type : Griotte_ast.word_type -> Z.t
val decode_word_type : Z.t -> (Griotte_ast.word_type, string) result
