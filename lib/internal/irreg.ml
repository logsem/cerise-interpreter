(* Type definitions for the registers init *)

exception ExprException of string

type regname = PC | STK | CGP | Reg of int

type expr =
  | IntLit of Z.t
  | AddOp of expr * expr
  | SubOp of expr * expr
  | MaxAddr
  | StkAddr

type perm = O | E | RO | RX | RW | RWX | RWL | RWLX
type locality = Global | Local
type seal_perm = bool * bool

type sealable =
  | WCap of perm * locality * expr * expr * expr
  | WSealRange of seal_perm * locality * expr * expr * expr

type word = WI of expr | WSealable of sealable | WSealed of expr * sealable
type t = (regname * word) list

let rec eval_expr (e : expr) (max_addr : Z.t) (stk_addr : Z.t) : Z.t =
  match e with
  | IntLit i -> i
  | MaxAddr -> max_addr
  | StkAddr -> stk_addr
  | AddOp (e1, e2) -> Z.(eval_expr e1 max_addr stk_addr + eval_expr e2 max_addr stk_addr)
  | SubOp (e1, e2) -> Z.(eval_expr e1 max_addr stk_addr - eval_expr e2 max_addr stk_addr)

let translate_perm (p : perm) : Ast.perm =
  match p with
  | O -> Ast.O
  | E -> Ast.E
  | RO -> Ast.RO
  | RX -> Ast.RX
  | RW -> Ast.RW
  | RWX -> Ast.RWX
  | RWL -> Ast.RWL
  | RWLX -> Ast.RWLX

let translate_locality (g : locality) : Ast.locality =
  match g with Local -> Ast.Local | Global -> Ast.Global

let translate_regname (r : regname) : Ast.regname =
  match r with PC -> Ast.PC | CGP -> Ast.cgp | STK -> Ast.stk | Reg i -> Ast.Reg i

let translate_sealable (sb : sealable) (max_addr : Z.t) (stk_addr : Z.t) : Ast.sealable =
  match sb with
  | WCap (p, g, b, e, a) ->
      let g = translate_locality g in
      let p = translate_perm p in
      let b = eval_expr b max_addr stk_addr in
      let e = eval_expr e max_addr stk_addr in
      let a = eval_expr a max_addr stk_addr in
      Cap (p, g, b, e, a)
  | WSealRange (p, g, b, e, a) ->
      let g = translate_locality g in
      let b = eval_expr b max_addr stk_addr in
      let e = eval_expr e max_addr stk_addr in
      let a = eval_expr a max_addr stk_addr in
      SealRange (p, g, b, e, a)

let translate_word (w : word) (max_addr : Z.t) (stk_addr : Z.t) : Ast.word =
  match w with
  | WI e ->
      let z = eval_expr e max_addr stk_addr in
      Ast.I z
  | WSealable sb -> Ast.Sealable (translate_sealable sb max_addr stk_addr)
  | WSealed (o, sb) ->
      let ot = eval_expr o max_addr stk_addr in
      Ast.Sealed (ot, translate_sealable sb max_addr stk_addr)

let rec translate_regfile (regfile : t) (max_addr : Z.t) (stk_addr : Z.t) :
    Ast.word Machine.RegMap.t =
  let init_regfile = Machine.RegMap.empty in
  match regfile with
  | [] -> init_regfile
  | (r, w) :: rf ->
      let nrf = translate_regfile rf max_addr stk_addr in
      Machine.RegMap.add (translate_regname r) (translate_word w max_addr stk_addr) nrf
