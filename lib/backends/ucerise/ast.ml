(** Core uCerise machine values and instructions. Concrete assembly converts source terms into these
    values before the machine executes them. *)

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
