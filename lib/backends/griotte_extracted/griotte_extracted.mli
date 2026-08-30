module Ast : sig
  type register = PC | Reg of int
  type system_register = MTDC

  val cnull : register
  val cra : register
  val csp : register
  val cgp : register
  val ctp : register
  val ct0 : register
  val ct1 : register
  val ct2 : register
  val ct3 : register
  val ct4 : register
  val ct5 : register
  val ct6 : register
  val cs0 : register
  val cs1 : register
  val cs2 : register
  val cs3 : register
  val cs4 : register
  val cs5 : register
  val cs6 : register
  val cs7 : register
  val cs8 : register
  val cs9 : register
  val cs10 : register
  val cs11 : register
  val ca0 : register
  val ca1 : register
  val ca2 : register
  val ca3 : register
  val ca4 : register
  val ca5 : register
  val ca6 : register
  val ca7 : register

  type rx_permission = Orx | R | X | XSR
  type write_permission = Ow | W | WL
  type deep_local_permission = DL | LG
  type deep_read_only_permission = DRO | LM

  type permission =
    rx_permission * write_permission * deep_local_permission * deep_read_only_permission

  type locality = Global | Local
  type word_type = W_I | W_Cap | W_SealRange | W_Sealed | W_Sentry
  type seal_permission = bool * bool
  type reg_or_const = Register of register | Constant of Z.t

  type sealable =
    | Cap of permission * locality * Z.t * Z.t * Z.t
    | SealRange of seal_permission * locality * Z.t * Z.t * Z.t

  type word =
    | I of Z.t
    | Sealable of sealable
    | Sentry of permission * locality * Z.t * Z.t * Z.t
    | Sealed of Z.t * sealable

  type instruction =
    | Jalr of register * register
    | Jmp of reg_or_const
    | Jnz of register * reg_or_const
    | ReadSR of register * system_register
    | WriteSR of system_register * register
    | Move of register * reg_or_const
    | Load of register * register
    | Store of register * reg_or_const
    | Add of register * reg_or_const * reg_or_const
    | Sub of register * reg_or_const * reg_or_const
    | Mul of register * reg_or_const * reg_or_const
    | LAnd of register * reg_or_const * reg_or_const
    | LOr of register * reg_or_const * reg_or_const
    | LShiftL of register * reg_or_const * reg_or_const
    | LShiftR of register * reg_or_const * reg_or_const
    | Lt of register * reg_or_const * reg_or_const
    | Lea of register * reg_or_const
    | Restrict of register * reg_or_const
    | SubSeg of register * reg_or_const * reg_or_const
    | GetL of register * register
    | GetB of register * register
    | GetE of register * register
    | GetA of register * register
    | GetP of register * register
    | GetOType of register * register
    | GetWType of register * register
    | Seal of register * register * register
    | UnSeal of register * register * register
    | Fail
    | Halt

  val null_permission : permission
  val max_object_type : Z.t
end

module Asm_ir : sig
  type expression
  type register_term = Named of Ast.register | Register_parameter of string
  type permission_term = Permission_literal of Ast.permission | Permission_parameter of string

  type seal_permission_term =
    | Seal_permission_literal of Ast.seal_permission
    | Seal_permission_parameter of string

  type locality_term = Locality_literal of Ast.locality | Locality_parameter of string
  type word_type_term = Word_type_literal of Ast.word_type | Word_type_parameter of string

  type constant_term =
    | Expression of expression
    | Permission of Ast.permission
    | Seal_permission of Ast.seal_permission
    | Permission_locality of permission_term * locality_term
    | Seal_permission_locality of seal_permission_term * locality_term
    | Word_type of Ast.word_type
    | Locality of Ast.locality
    | Value_parameter of string

  type operand_term = Register_term of register_term | Constant_term of constant_term

  type sealable_term =
    | Cap_term of permission_term * locality_term * expression * expression * expression
    | Seal_range_term of seal_permission_term * locality_term * expression * expression * expression

  type word_term =
    | I_term of expression
    | Sealable_term of sealable_term
    | Sentry_term of permission_term * locality_term * expression * expression * expression
    | Sealed_term of expression * sealable_term

  type word = word_term

  type instruction_term =
    | Jalr_term of register_term * register_term
    | Jmp_term of operand_term
    | Jnz_term of register_term * operand_term
    | ReadSR_term of register_term * Ast.system_register
    | WriteSR_term of Ast.system_register * register_term
    | Move_term of register_term * operand_term
    | Load_term of register_term * register_term
    | Store_term of register_term * operand_term
    | Add_term of register_term * operand_term * operand_term
    | Sub_term of register_term * operand_term * operand_term
    | Mul_term of register_term * operand_term * operand_term
    | LAnd_term of register_term * operand_term * operand_term
    | LOr_term of register_term * operand_term * operand_term
    | LShiftL_term of register_term * operand_term * operand_term
    | LShiftR_term of register_term * operand_term * operand_term
    | Lt_term of register_term * operand_term * operand_term
    | Lea_term of register_term * operand_term
    | Restrict_term of register_term * operand_term
    | SubSeg_term of register_term * operand_term * operand_term
    | GetL_term of register_term * register_term
    | GetB_term of register_term * register_term
    | GetE_term of register_term * register_term
    | GetA_term of register_term * register_term
    | GetP_term of register_term * register_term
    | GetOType_term of register_term * register_term
    | GetWType_term of register_term * register_term
    | Seal_term of register_term * register_term * register_term
    | UnSeal_term of register_term * register_term * register_term
    | Fail_term
    | Halt_term

  type statement = Op of instruction_term | Word of word_term
  type program = statement list

  type regfile_entry =
    | Register_entry of Ast.register * word_term
    | System_register_entry of Ast.system_register * word_term

  type regfile = regfile_entry list

  val parse_register_name : string -> Ast.register option
  val lower_word : Runtime_config.t -> word -> (Ast.word, Diagnostic.t list) result

  val lower_instruction :
    Runtime_config.t -> instruction_term -> (Ast.instruction, Diagnostic.t list) result

  val lower_program : Runtime_config.t -> program -> (Ast.word list, Diagnostic.t list) result

  val lower_regfile :
    Runtime_config.t ->
    regfile ->
    ( (Ast.register * Ast.word) list * (Ast.system_register * Ast.word) list,
      Diagnostic.t list )
    result
end

module Parser : sig
  type program = Asm_ir.program
  type regfile = Asm_ir.regfile
  type word = Asm_ir.word

  val parse_program : ?filename:string -> string -> (program, Diagnostic.t list) result
  val parse_regfile : ?filename:string -> string -> (regfile, Diagnostic.t list) result
  val parse_word : ?filename:string -> string -> (word, Diagnostic.t list) result
end

module Printer : sig
  val register : Ast.register -> string
  val system_register : Ast.system_register -> string
  val rx_permission : Ast.rx_permission -> string
  val write_permission : Ast.write_permission -> string
  val deep_local_permission : Ast.deep_local_permission -> string
  val deep_read_only_permission : Ast.deep_read_only_permission -> string
  val permission : Ast.permission -> string
  val locality : Ast.locality -> string
  val seal_permission : Ast.seal_permission -> string
  val word_type : Ast.word_type -> string
  val sealable : Ast.sealable -> string
  val word : Ast.word -> string
end

module Codec : sig
  val encode : Ast.instruction -> (Z.t, string) result
  val decode : Z.t -> (Ast.instruction, string) result
  val encode_permission : Ast.permission -> Z.t
  val decode_permission : Z.t -> (Ast.permission, string) result
  val encode_locality : Ast.locality -> Z.t
  val encode_permission_locality : Ast.permission -> Ast.locality -> Z.t
  val decode_permission_locality : Z.t -> (Ast.permission * Ast.locality, string) result
  val encode_seal_permission : Ast.seal_permission -> Z.t
  val decode_seal_permission : Z.t -> (Ast.seal_permission, string) result
  val encode_seal_permission_locality : Ast.seal_permission -> Ast.locality -> Z.t
  val decode_seal_permission_locality : Z.t -> (Ast.seal_permission * Ast.locality, string) result
  val encode_word_type : Ast.word_type -> Z.t
  val decode_word_type : Z.t -> (Ast.word_type, string) result
end

module Backend :
  Machine_backend.S
    with type asm_program = Asm_ir.program
     and type asm_regfile = Asm_ir.regfile
     and type asm_word = Asm_ir.word
