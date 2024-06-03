(* Type definitions for the syntax AST *)
type regname = PC | Reg of int

let cgp = Reg 0
let stk = Reg 31
let sreg = Reg 30

module Perm =
struct
  type t =
  | E   (* sentry                  ---  sealed value with otype 1, 2 or 3 *)
  | R   (* read                    ---  LG, MC and LD *)
  | X   (* execute                 ---  EX *)
  | W   (* write                   ---  LM, MC and SD *)
  | WL  (* write local             ---  SL *)
  | SR  (* system register access  ---  SR *)
  | DL  (* deep locality           ---  inverse of LG *)
  | DI  (* deep immutability       ---  inverse of LM *)
  let compare p1 p2 = if (p1 = p2) then 0 else 1
  let allowed_with (p1 : t) (p2 : t) =
    match (p1,p2) with
    (* sentry are not allowed with any other capabilities *)
    | E,_ -> false
    (* system access permission only with X *)
    | SR,X -> true
    | SR,_ -> false

    (* no write and execute permission at the same time *)
    | W,X -> false
    | WL,X -> false

    | _,_ -> true
end
module PermSet = Set.Make(Perm)


type locality = Global | Local
type wtype = W_I | W_Cap | W_SealRange | W_Sealed
type seal_perm = bool * bool
type reg_or_const = Register of regname | Const of Z.t

type sealable =
  | Cap of (PermSet.t) * locality * Z.t * Z.t * Z.t
  | SealRange of seal_perm * locality * Z.t * Z.t * Z.t

type word = I of Z.t | Sealable of sealable | Sealed of Z.t * sealable

type machine_op =
  | Jmp of regname
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

let compare_regname (r1 : regname) (r2 : regname) : int =
  match (r1, r2) with
  | PC, PC -> 0
  | PC, Reg _ -> -1
  | Reg _, PC -> 1
  | Reg i, Reg j -> Int.compare i j

let const n = Const (Z.of_int n)
