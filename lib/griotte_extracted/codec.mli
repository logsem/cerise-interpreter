(** Handwritten, untrusted boundary codec for the extracted Griotte machine. This module
    deliberately does not depend on [Instruction_codec] or [Griotte.Codec]. Its fixed numbers mirror
    the Rocq extraction parameters. *)

val encode : Cerise_griotte_contract.Ast.instruction -> (Z.t, string) result
val decode : Z.t -> (Cerise_griotte_contract.Ast.instruction, string) result
val encode_permission : Cerise_griotte_contract.Ast.permission -> Z.t
val decode_permission : Z.t -> (Cerise_griotte_contract.Ast.permission, string) result
val encode_locality : Cerise_griotte_contract.Ast.locality -> Z.t
val encode_permission_locality :
  Cerise_griotte_contract.Ast.permission -> Cerise_griotte_contract.Ast.locality -> Z.t

val decode_permission_locality :
  Z.t -> (Cerise_griotte_contract.Ast.permission * Cerise_griotte_contract.Ast.locality, string) result

val encode_seal_permission : Cerise_griotte_contract.Ast.seal_permission -> Z.t
val decode_seal_permission : Z.t -> (Cerise_griotte_contract.Ast.seal_permission, string) result
val encode_seal_permission_locality :
  Cerise_griotte_contract.Ast.seal_permission -> Cerise_griotte_contract.Ast.locality -> Z.t

val decode_seal_permission_locality :
  Z.t -> (Cerise_griotte_contract.Ast.seal_permission * Cerise_griotte_contract.Ast.locality, string) result

val encode_word_type : Cerise_griotte_contract.Ast.word_type -> Z.t
val decode_word_type : Z.t -> (Cerise_griotte_contract.Ast.word_type, string) result
