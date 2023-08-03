(* Type definitions for the registers init *)


type regname = PC | STK | Reg of int
type expr
  = IntLit of int
  | AddOp of expr * expr
  | SubOp of expr * expr
  | MaxAddr
  | StkAddr

type perm = O | E | RO | RX | RW | RWX | RWL | RWLX | URW | URWL | URWX | URWLX
type locality = Global | Local | Directed
type seal_perm = bool * bool

(* TODO special addresses: min_addr, max_addr, stk_addr ... *)
type addr = Addr of expr

type sealable = WCap of perm * locality * addr * addr * addr
              | WSealRange of seal_perm * locality * addr * addr * addr
type word = WI of expr | WSealable of sealable | WSealed of addr * sealable

type t = (regname * word) list


let rec eval_expr  (e : expr) (max_addr : Z.t) (stk_addr : Z.t) : Z.t =
  match e with
  | IntLit i -> Z.(~$i)
  | MaxAddr -> max_addr
  | StkAddr -> stk_addr
  | AddOp (e1, e2) -> Z.((eval_expr e1 max_addr stk_addr) + (eval_expr e2 max_addr stk_addr))
  | SubOp (e1, e2) -> Z.((eval_expr e1 max_addr stk_addr) + (eval_expr e2 max_addr stk_addr))

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
  | URW -> Ast.URW
  | URWL -> Ast.URWL
  | URWX -> Ast.URWX
  | URWLX -> Ast.URWLX

let translate_locality (g : locality) : Ast.locality =
  match g with
  | Local -> Ast.Local
  | Global -> Ast.Global
  | Directed -> Ast.Directed

let translate_regname (r : regname) : Ast.regname =
  match r with
  | PC -> Ast.PC
  | STK -> Ast.STK
  | Reg i -> Ast.Reg i

let translate_addr (a : addr) (max_addr : Z.t) (stk_addr : Z.t): Z.t =
  match a with
  | Addr e -> (eval_expr e max_addr stk_addr)

let translate_sealable (sb : sealable) (max_addr : Z.t) (stk_addr : Z.t) : Ast.sealable =
  match sb with
  | WCap (p,g,b,e,a) ->
    let g = translate_locality g in
    let p = translate_perm p in
    let b = translate_addr b max_addr stk_addr in
    let e = translate_addr e max_addr stk_addr in
    let a = translate_addr a max_addr stk_addr in
    Cap (p,g,b,e,a)
  | WSealRange (p,g,b,e,a) ->
    let g = translate_locality g in
    let b = translate_addr b max_addr stk_addr in
    let e = translate_addr e max_addr stk_addr in
    let a = translate_addr a max_addr stk_addr in
    SealRange (p,g,b,e,a)

let translate_word (w : word) (max_addr : Z.t) (stk_addr : Z.t) : Ast.word =
  match w with
  | WI e -> Ast.I (eval_expr e max_addr stk_addr)
  | WSealable sb -> Ast.Sealable (translate_sealable sb max_addr stk_addr)
  | WSealed (o,sb) -> Ast.Sealed (translate_addr o max_addr stk_addr, translate_sealable sb max_addr stk_addr)

let rec translate_regfile (regfile : t) (max_addr : Z.t) (stk_addr : Z.t):
  (Ast.word Machine.RegMap.t) =

  let init_regfile =
    Machine.RegMap.empty in
  match regfile with
  | [] -> init_regfile
  | (r,w)::rf ->
    let nrf = translate_regfile rf max_addr stk_addr in
    (Machine.RegMap.add (translate_regname r) (translate_word w max_addr stk_addr) nrf)
