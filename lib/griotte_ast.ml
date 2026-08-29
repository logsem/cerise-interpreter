type register = PC | Reg of int
type system_register = MTDC

let cnull = Reg 0
let cra = Reg 1
let csp = Reg 2
let cgp = Reg 3
let ctp = Reg 4
let ct0 = Reg 5
let ct1 = Reg 6
let ct2 = Reg 7
let ct3 = Reg 28
let ct4 = Reg 29
let ct5 = Reg 30
let ct6 = Reg 31
let cs0 = Reg 8
let cs1 = Reg 9
let cs2 = Reg 18
let cs3 = Reg 19
let cs4 = Reg 20
let cs5 = Reg 21
let cs6 = Reg 22
let cs7 = Reg 23
let cs8 = Reg 24
let cs9 = Reg 25
let cs10 = Reg 26
let cs11 = Reg 27
let ca0 = Reg 10
let ca1 = Reg 11
let ca2 = Reg 12
let ca3 = Reg 13
let ca4 = Reg 14
let ca5 = Reg 15
let ca6 = Reg 16
let ca7 = Reg 17

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
  | Rem of register * reg_or_const * reg_or_const
  | Div of register * reg_or_const * reg_or_const
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

type expression = Assembly_frontend.Expression.t
type register_term = Named of register | Register_parameter of string
type permission_term = Permission_literal of permission | Permission_parameter of string

type seal_permission_term =
  | Seal_permission_literal of seal_permission
  | Seal_permission_parameter of string

type locality_term = Locality_literal of locality | Locality_parameter of string
type word_type_term = Word_type_literal of word_type | Word_type_parameter of string

type constant_term =
  | Expression of expression
  | Permission of permission
  | Seal_permission of seal_permission
  | Permission_locality of permission_term * locality_term
  | Seal_permission_locality of seal_permission_term * locality_term
  | Word_type of word_type
  | Locality of locality
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

type instruction_term =
  | Jalr_term of register_term * register_term
  | Jmp_term of operand_term
  | Jnz_term of register_term * operand_term
  | ReadSR_term of register_term * system_register
  | WriteSR_term of system_register * register_term
  | Move_term of register_term * operand_term
  | Load_term of register_term * register_term
  | Store_term of register_term * operand_term
  | Add_term of register_term * operand_term * operand_term
  | Sub_term of register_term * operand_term * operand_term
  | Mul_term of register_term * operand_term * operand_term
  | Rem_term of register_term * operand_term * operand_term
  | Div_term of register_term * operand_term * operand_term
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
type regfile = (register * word_term) list * (system_register * word_term) list

let null_permission = (Orx, Ow, DL, DRO)
let max_object_type = Z.of_int 15
