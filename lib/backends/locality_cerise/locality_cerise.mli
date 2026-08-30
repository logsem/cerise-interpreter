module Ast : sig
  type register = PC | Reg of int
  type permission = O | E | RO | RX | RW | RWX | RWL | RWLX
  type locality = Global | Local
  type seal_permission = bool * bool
  type word_type = Integer | Capability | Seal_range | Sealed
  type reg_or_const = Register of register | Constant of Z.t

  type sealable =
    | Cap of permission * locality * Z.t * Z.t * Z.t
    | SealRange of seal_permission * locality * Z.t * Z.t * Z.t

  type word = I of Z.t | Sealable of sealable | Sealed of Z.t * sealable

  type instruction =
    | Jmp of register
    | Jnz of register * register
    | Move of register * reg_or_const
    | Load of register * register
    | Store of register * reg_or_const
    | Add of register * reg_or_const * reg_or_const
    | Sub of register * reg_or_const * reg_or_const
    | Mul of register * reg_or_const * reg_or_const
    | Rem of register * reg_or_const * reg_or_const
    | Div of register * reg_or_const * reg_or_const
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
    | Invoke of register * register
    | Fail
    | Halt
end

module Asm_ir : sig
  type expression
  type register_term = Named of Ast.register | Register_parameter of string
  type permission_term = Permission_literal of Ast.permission | Permission_parameter of string

  type seal_permission_term =
    | Seal_permission_literal of Ast.seal_permission
    | Seal_permission_parameter of string

  type locality_term = Locality of Ast.locality | Locality_parameter of string

  type constant_term =
    | Expression of expression
    | Permission of Ast.permission
    | Seal_permission of Ast.seal_permission
    | Permission_locality of permission_term * locality_term
    | Seal_permission_locality of seal_permission_term * locality_term
    | Parameterized_permission_locality of string * locality_term
    | Locality_constant of Ast.locality
    | Word_type of Ast.word_type
    | Value_parameter of string

  type operand_term = Register_term of register_term | Constant_term of constant_term

  type sealable_term =
    | Cap_term of permission_term * locality_term * expression * expression * expression
    | SealRange_term of seal_permission_term * locality_term * expression * expression * expression

  type word_term =
    | I_term of expression
    | Sealable_term of sealable_term
    | Sealed_term of expression * sealable_term

  type word = word_term

  type instruction_term =
    | Jmp_term of register_term
    | Jnz_term of register_term * register_term
    | Move_term of register_term * operand_term
    | Load_term of register_term * register_term
    | Store_term of register_term * operand_term
    | Add_term of register_term * operand_term * operand_term
    | Sub_term of register_term * operand_term * operand_term
    | Mul_term of register_term * operand_term * operand_term
    | Rem_term of register_term * operand_term * operand_term
    | Div_term of register_term * operand_term * operand_term
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
    | Invoke_term of register_term * register_term
    | Fail_term
    | Halt_term

  type statement = Op of instruction_term | Word of word_term
  type program = statement list
  type regfile = (Ast.register * word_term) list

  type parameter_kind =
    | Register_kind
    | Expression_kind
    | Value_kind
    | Permission_kind
    | Seal_permission_kind
    | Word_type_kind
    | Locality_kind

  type macro_argument = Register_argument of Ast.register | Constant_argument of constant_term

  val lower_word : Runtime_config.t -> word -> (Ast.word, Diagnostic.t list) result
  val lower_program : Runtime_config.t -> program -> (Ast.word list, Diagnostic.t list) result

  val lower_regfile :
    Runtime_config.t -> regfile -> ((Ast.register * Ast.word) list, Diagnostic.t list) result
end

module Printer : sig
  val permission : Ast.permission -> string
  val locality : Ast.locality -> string
  val seal_permission : Ast.seal_permission -> string
  val sealable : Ast.sealable -> string
  val word : Ast.word -> string
end

module Codec : sig
  val encode : Ast.instruction -> (Z.t, Instruction_codec.error) result
  val decode : Z.t -> (Ast.instruction, Instruction_codec.error) result
  val allocations : (string * int * int) list
  val encode_permission : Ast.permission -> Z.t
  val decode_permission : Z.t -> (Ast.permission, string) result
  val encode_seal_permission : Ast.seal_permission -> Z.t
  val decode_seal_permission : Z.t -> (Ast.seal_permission, string) result
  val encode_word_type : Ast.word_type -> Z.t
  val encode_locality : Ast.locality -> Z.t
  val decode_locality : Z.t -> (Ast.locality, string) result
  val encode_permission_locality : Ast.permission -> Ast.locality -> Z.t
  val decode_permission_locality : Z.t -> (Ast.permission * Ast.locality, string) result
  val encode_seal_permission_locality : Ast.seal_permission -> Ast.locality -> Z.t
  val decode_seal_permission_locality : Z.t -> (Ast.seal_permission * Ast.locality, string) result
end

module Machine : sig
  module RegMap : Map.S with type key = Ast.register
  module MemMap : Map.S with type key = Z.t

  type status = Running | Halted | Failed

  type t = {
    config : Runtime_config.t;
    status : status;
    registers : Ast.word RegMap.t;
    memory : Ast.word MemMap.t;
  }

  val init : Runtime_config.t -> Ast.word list -> (Ast.register * Ast.word) list option -> t
  val read_register : Ast.register -> t -> Ast.word
  val read_memory : Z.t -> t -> Ast.word option
  val set_register : Ast.register -> Ast.word -> t -> t
  val set_memory_raw : Z.t -> Ast.word -> t -> t
  val execute : Ast.instruction -> t -> t
  val step : t -> (t, Machine_backend.execution_error) result
  val step_n : int -> t -> (t, Machine_backend.execution_error) result
  val run : t -> t
end

module Parser : sig
  type program = Asm_ir.program
  type regfile = Asm_ir.regfile
  type word = Asm_ir.word

  val parse_program : ?filename:string -> string -> (program, Diagnostic.t list) result
  val parse_regfile : ?filename:string -> string -> (regfile, Diagnostic.t list) result
  val parse_word : ?filename:string -> string -> (word, Diagnostic.t list) result
end

module Backend :
  Machine_backend.S
    with type asm_program = Asm_ir.program
     and type asm_regfile = Asm_ir.regfile
     and type asm_word = Asm_ir.word
     and type state = Machine.t
