type register = PC | Reg of int
type permission = O | E | RO | RX | RW | RWX | RWL | RWLX | URW | URWL | URWX | URWLX
type locality = Global | Local | Directed
type seal_permission = bool * bool
type word_type = Integer | Capability | Seal_range | Sealed
type reg_or_const = Register of register | Const of Z.t

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
  | LoadU of register * register * reg_or_const
  | StoreU of register * reg_or_const * reg_or_const
  | PromoteU of register
  | EInit of register * register
  | EDeInit of register
  | EStoreId of register * register
  | IsUnique of register * register
  | Fail
  | Halt
