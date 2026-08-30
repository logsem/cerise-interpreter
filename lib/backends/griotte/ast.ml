(* Core values and instructions for the handwritten Griotte machine. Assembly terms
   lower to these values; the codec, machine, printer, and UI adapter consume them. *)

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

let null_permission = (Orx, Ow, DL, DRO)
let max_object_type = Z.of_int 15
