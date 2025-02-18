(* Type definitions for the syntax AST *)
type regname = PC | Reg of int
type sregname = MTDC

(* https://github.com/CHERIoT-Platform/llvm-project/blob/f39e8860b29668f986b11d29fa953c96a25373f1/llvm/lib/Target/RISCV/RISCVRegisterInfo.td#L138 *)
let cnull = Reg 0
let cra = Reg 1 (* link register *)
let csp = Reg 2 (* compartment stack register *)
let cgp = Reg 3 (* global data register *)
let ctp = Reg 4 (* temporary registers *)
let ct0 = Reg 5
let ct1 = Reg 6
let ct2 = Reg 7
let ct3 = Reg 28
let ct4 = Reg 29
let ct5 = Reg 30
let ct6 = Reg 31
let cs0 = Reg 8 (* temporary registers ? *)
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
let ca0 = Reg 10 (* arguments registers *)
let ca1 = Reg 11
let ca2 = Reg 12
let ca3 = Reg 13
let ca4 = Reg 14
let ca5 = Reg 15
let ca6 = Reg 16
let ca7 = Reg 17

type rxperm = Orx | R | X | XSR
type wperm = Ow | W | WL
type dlperm = DL | LG
type droperm = DRO | LM
type perm = rxperm * wperm * dlperm * droperm
type locality = Global | Local
type wtype = W_I | W_Cap | W_SealRange | W_Sealed
type seal_perm = bool * bool
type reg_or_const = Register of regname | Const of Z.t

type sealable =
  | Cap of perm * locality * Z.t * Z.t * Z.t
  | SealRange of seal_perm * locality * Z.t * Z.t * Z.t

type word = I of Z.t | Sealable of sealable | Sealed of Z.t * sealable

type machine_op =
  | Jalr of regname * regname (* jump and link return *)
  | Jmp of reg_or_const (* immediate jump *)
  | Jnz of regname * reg_or_const (* jump non zero *)
  (* system access registers *)
  | ReadSR of regname * sregname
  | WriteSR of sregname * regname
  | Move of regname * reg_or_const
  | Load of regname * regname
  | Store of regname * reg_or_const
  | Add of regname * reg_or_const * reg_or_const
  | Sub of regname * reg_or_const * reg_or_const
  | Mul of regname * reg_or_const * reg_or_const
  | Rem of regname * reg_or_const * reg_or_const
  | Div of regname * reg_or_const * reg_or_const
  | LAnd of regname * reg_or_const * reg_or_const
  | LOr of regname * reg_or_const * reg_or_const
  | LShiftL of regname * reg_or_const * reg_or_const
  | LShiftR of regname * reg_or_const * reg_or_const
  | Lt of regname * reg_or_const * reg_or_const
  | Lea of regname * reg_or_const
  | Restrict of regname * reg_or_const
  | SubSeg of regname * reg_or_const * reg_or_const
  | GetL of regname * regname
  | GetB of regname * regname
  | GetE of regname * regname
  | GetA of regname * regname
  | GetP of regname * regname
  | GetOType of regname * regname
  | GetWType of regname * regname
  | Seal of regname * regname * regname
  | UnSeal of regname * regname * regname
  | Fail
  | Halt

type statement = Op of machine_op | Word of word (* TODO: PseudoOp and LabelDefs *)
type t = statement list

let null_perm = (Orx, Ow, DL, DRO)

let compare_regname (r1 : regname) (r2 : regname) : int =
  match (r1, r2) with
  | PC, PC -> 0
  | PC, Reg _ -> -1
  | Reg _, PC -> 1
  | Reg i, Reg j -> Int.compare i j

let compare_sregname (sr1 : sregname) (sr2 : sregname) : int =
  match (sr1, sr2) with MTDC, MTDC -> 0

let const n = Const (Z.of_int n)
