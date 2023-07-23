(* Type definitions for the registers init *)


type regname = PC | Reg of int
type expr
  = IntLit of int
  | AddOp of expr * expr
  | SubOp of expr * expr
  | MaxAddr

type perm = O | E | RO | RX | RW | RWX

(* TODO special addresses: min_addr, max_addr, stk_addr ... *)
type addr = Addr of expr

type word = WI of expr | WCap of perm * addr * addr * addr

type t = (regname * word) list


let rec eval_expr  (e : expr) (max_addr : Z.t) : Z.t =
  match e with
  | IntLit i -> Z.(~$i)
  | MaxAddr -> max_addr
  | AddOp (e1, e2) -> Z.((eval_expr e1 max_addr) + (eval_expr e2 max_addr))
  | SubOp (e1, e2) -> Z.((eval_expr e1 max_addr) + (eval_expr e2 max_addr))

let translate_perm (p : perm) : Ast.perm =
  match p with
  | O -> Ast.O
  | E -> Ast.E
  | RO -> Ast.RO
  | RX -> Ast.RX
  | RW -> Ast.RW
  | RWX -> Ast.RWX

let translate_regname (r : regname) : Ast.regname =
  match r with
  | PC -> Ast.PC
  | Reg i -> Ast.Reg i

let translate_addr (a : addr) (max_addr : Z.t) : Z.t =
  match a with
  | Addr e -> (eval_expr e max_addr)


let translate_word (w : word) (max_addr : Z.t) : Machine.word =
  match w with
  | WI e -> Machine.I (eval_expr e max_addr)
  | WCap (p,b,e,a) ->
    let p = translate_perm p in
    let b = translate_addr b max_addr in
    let e = translate_addr e max_addr in
    let a = translate_addr a max_addr in
    Machine.Cap (p,b,e,a)

let rec translate_regfile (regfile : t) (max_addr : Z.t):
  (Machine.word Machine.RegMap.t) =
  let init_regfile =
    Machine.RegMap.empty in
  match regfile with
  | [] -> init_regfile
  | (r,w)::rf ->
    let nrf = translate_regfile rf max_addr in
      (Machine.RegMap.add (translate_regname r) (translate_word w max_addr) nrf)
