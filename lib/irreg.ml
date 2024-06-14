(* Type definitions for the registers init *)

exception ExprException of string

(* type regname = PC | Reg of int *)
type expr =
  | IntLit of Z.t
  | AddOp of expr * expr
  | SubOp of expr * expr
  | MultOp of expr * expr
  | MaxAddr

type perm = Ast.PermSet.t
type locality = Ast.locality
type seal_perm = Ast.seal_perm

type sealable =
  | WCap of perm * locality * expr * expr * expr
  | WSealRange of seal_perm * locality * expr * expr * expr

type word = WI of expr | WSealable of sealable | WSealed of expr * sealable
type t = (Ast.regname * word) list

let rec eval_expr (e : expr) (max_addr : Z.t) : Z.t =
  match e with
  | IntLit i -> i
  | MaxAddr -> max_addr
  | AddOp (e1, e2) -> Z.(eval_expr e1 max_addr + eval_expr e2 max_addr)
  | SubOp (e1, e2) -> Z.(eval_expr e1 max_addr - eval_expr e2 max_addr)
  | MultOp (e1, e2) -> Z.(eval_expr e1 max_addr * eval_expr e2 max_addr)

let translate_perm (p : perm) : Ast.PermSet.t = p
let translate_locality (g : locality) : Ast.locality = g

let translate_sealable (sb : sealable) (max_addr : Z.t) : Ast.sealable =
  match sb with
  | WCap (p, g, b, e, a) ->
      let g = translate_locality g in
      let p = translate_perm p in
      let b = eval_expr b max_addr in
      let e = eval_expr e max_addr in
      let a = eval_expr a max_addr in
      Cap (p, g, b, e, a)
  | WSealRange (p, g, b, e, a) ->
      let g = translate_locality g in
      let b = eval_expr b max_addr in
      let e = eval_expr e max_addr in
      let a = eval_expr a max_addr in
      SealRange (p, g, b, e, a)

let translate_word (w : word) (max_addr : Z.t) : Ast.word =
  match w with
  | WI e ->
      let z = eval_expr e max_addr in
      Ast.I z
  | WSealable sb -> Ast.Sealable (translate_sealable sb max_addr)
  | WSealed (o, sb) ->
      let ot = eval_expr o max_addr in
      Ast.Sealed (ot, translate_sealable sb max_addr)

let rec translate_regfile (regfile : t) (max_addr : Z.t) : Ast.word Machine.RegMap.t =
  let init_regfile = Machine.RegMap.empty in
  match regfile with
  | [] -> init_regfile
  | (r, w) :: rf ->
      let nrf = translate_regfile rf max_addr in
      Machine.RegMap.add r (translate_word w max_addr) nrf
