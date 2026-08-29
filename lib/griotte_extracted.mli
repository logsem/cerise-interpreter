module Ast = Cerise_griotte_contract.Ast
module Parser = Cerise_griotte_contract.Parser
module Printer = Cerise_griotte_contract.Printer
module Codec :
  sig
    val encode :
      Ast.instruction -> (Z.t, string) result
    val decode :
      Z.t -> (Ast.instruction, string) result
    val encode_permission : Ast.permission -> Z.t
    val decode_permission :
      Z.t -> (Ast.permission, string) result
    val encode_locality : Ast.locality -> Z.t
    val encode_permission_locality :
      Ast.permission ->
      Ast.locality -> Z.t
    val decode_permission_locality :
      Z.t ->
      (Ast.permission *
       Ast.locality, string)
      result
    val encode_seal_permission :
      Ast.seal_permission -> Z.t
    val decode_seal_permission :
      Z.t -> (Ast.seal_permission, string) result
    val encode_seal_permission_locality :
      Ast.seal_permission ->
      Ast.locality -> Z.t
    val decode_seal_permission_locality :
      Z.t ->
      (Ast.seal_permission *
       Ast.locality, string)
      result
    val encode_word_type : Ast.word_type -> Z.t
    val decode_word_type :
      Z.t -> (Ast.word_type, string) result
  end

module Backend : Machine_backend.S with type asm_program = Ast.program and type asm_regfile = Ast.regfile and type asm_word = Ast.word_term
