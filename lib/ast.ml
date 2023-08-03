(* Type definitions for the syntax AST *)
type regname = PC | Reg of int
type perm = O | E | RO | RX | RW | RWX
type wtype = W_I | W_Cap | W_SealRange | W_Sealed
type seal_perm = bool * bool
type reg_or_const = Register of regname | Const of Z.t
type sealable = Cap of perm * Z.t * Z.t * Z.t | SealRange of seal_perm * Z.t * Z.t * Z.t
type word = I of Z.t | Sealable of sealable | Sealed of Z.t * sealable
type machine_op
  = Jmp of regname
  | Jnz of regname * regname
  | Move of regname * reg_or_const
  | Load of regname * regname
  | Store of regname * reg_or_const
  | Add of regname * reg_or_const * reg_or_const
  | Sub of regname * reg_or_const * reg_or_const
  | Mul of regname * reg_or_const * reg_or_const
  | Rem of regname * reg_or_const * reg_or_const
  | Div of regname * reg_or_const * reg_or_const
  | Lt of regname * reg_or_const * reg_or_const
  | Lea of regname * reg_or_const
  | Restrict of regname * reg_or_const
  | SubSeg of regname * reg_or_const * reg_or_const
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

let compare_regname (r1 : regname) (r2: regname) : int =
  match r1, r2 with
  | PC, PC -> 0
  | PC, Reg _ -> -1
  | Reg _, PC -> 1
  | Reg i, Reg j -> Int.compare i j

let const n = Const (Z.of_int n)
