type register = PC | Reg of int
type permission = O | E | RO | RX | RW | RWX | RWL | RWLX | URW | URWX | URWL | URWLX
type locality = Global | Local
type reg_or_const = Register of register | Constant of Z.t
type capability = Cap of permission * locality * Z.t * Z.t * Z.t
type word = I of Z.t | Cap of capability

type instruction =
  | Jmp of register
  | Jnz of register * register
  | Move of register * reg_or_const
  | Load of register * register
  | Store of register * reg_or_const
  | Add of register * reg_or_const * reg_or_const
  | Sub of register * reg_or_const * reg_or_const
  | Lt of register * reg_or_const * reg_or_const
  | Lea of register * reg_or_const
  | Restrict of register * reg_or_const
  | SubSeg of register * reg_or_const * reg_or_const
  | IsPtr of register * register
  | GetP of register * register
  | GetL of register * register
  | GetB of register * register
  | GetE of register * register
  | GetA of register * register
  | Fail
  | Halt
  | LoadU of register * register * reg_or_const
  | StoreU of register * reg_or_const * reg_or_const
  | PromoteU of register

type expression = Assembly_frontend.Expression.t
type register_term = Named of register | Register_parameter of string
type permission_term = Permission_literal of permission | Permission_parameter of string
type locality_term = Locality of locality | Locality_parameter of string

type constant_term =
  | Expression of expression
  | Permission of permission
  | Permission_locality of permission_term * locality_term
  | Parameterized_permission_locality of string * locality_term
  | Locality_constant of locality
  | Value_parameter of string

type operand_term = Register_term of register_term | Constant_term of constant_term

type word_term =
  | I_term of expression
  | Cap_term of permission_term * locality_term * expression * expression * expression

type instruction_term =
  | Jmp_term of register_term
  | Jnz_term of register_term * register_term
  | Move_term of register_term * operand_term
  | Load_term of register_term * register_term
  | Store_term of register_term * operand_term
  | Add_term of register_term * operand_term * operand_term
  | Sub_term of register_term * operand_term * operand_term
  | Lt_term of register_term * operand_term * operand_term
  | Lea_term of register_term * operand_term
  | Restrict_term of register_term * operand_term
  | SubSeg_term of register_term * operand_term * operand_term
  | IsPtr_term of register_term * register_term
  | GetP_term of register_term * register_term
  | GetL_term of register_term * register_term
  | GetB_term of register_term * register_term
  | GetE_term of register_term * register_term
  | GetA_term of register_term * register_term
  | Fail_term
  | Halt_term
  | LoadU_term of register_term * register_term * operand_term
  | StoreU_term of register_term * operand_term * operand_term
  | PromoteU_term of register_term

type statement = Op of instruction_term | Word of word_term
type program = statement list
type regfile = (register * word_term) list
