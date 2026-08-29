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

type expression = Assembly_frontend.Expression.t
type register_term = Named of register | Register_parameter of string
type permission_term = Permission_literal of permission | Permission_parameter of string

type seal_permission_term =
  | Seal_permission_literal of seal_permission
  | Seal_permission_parameter of string

type locality_term = Locality of locality | Locality_parameter of string

type constant_term =
  | Expression of expression
  | Permission of permission
  | Seal_permission of seal_permission
  | Permission_locality of permission_term * locality_term
  | Seal_permission_locality of seal_permission_term * locality_term
  | Parameterized_permission_locality of string * locality_term
  | Locality_constant of locality
  | Word_type of word_type
  | Value_parameter of string

type operand_term = Register_term of register_term | Constant_term of constant_term

type sealable_term =
  | Cap_term of permission_term * locality_term * expression * expression * expression
  | SealRange_term of seal_permission_term * locality_term * expression * expression * expression

type word_term =
  | I_term of expression
  | Sealable_term of sealable_term
  | Sealed_term of expression * sealable_term

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
type regfile = (register * word_term) list
