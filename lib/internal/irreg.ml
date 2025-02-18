(* Type definitions for the registers init *)

exception ExprException of string

(* type regname = PC | Reg of int *)
type expr =
  | IntLit of Z.t
  | AddOp of expr * expr
  | SubOp of expr * expr
  | MultOp of expr * expr
  | LandOp of expr * expr
  | LorOp of expr * expr
  | LslOp of expr * expr
  | LsrOp of expr * expr
  | MaxAddr

type perm = Ast.perm
type locality = Ast.locality
type seal_perm = Ast.seal_perm

type sealable =
  | WCap of perm * locality * expr * expr * expr
  | WSealRange of seal_perm * locality * expr * expr * expr

type word = WI of expr | WSealable of sealable | WSealed of expr * sealable
type regfile_t = (Ast.regname * word) list
type sregfile_t = (Ast.sregname * word) list
type t = regfile_t * sregfile_t

let rec eval_expr (e : expr) (max_addr : Z.t) : Z.t =
  let binop_eval (binop : Z.t -> Z.t -> Z.t) ( e1 : expr ) ( e2 : expr ) : Z.t =
    binop (eval_expr e1 max_addr) (eval_expr e2 max_addr)
  in
  let lshiftl (z1 : Z.t) (z2 : Z.t) : Z.t =
    print_string (Z.to_string z1 ^ "\n");
    print_newline;
    print_string (Z.to_string z2 ^ "\n");
    print_newline;
    print_string (Z.to_string (Z.of_int ((Z.to_int z1) lsl (Z.to_int z2))) ^ "\n");
    Z.of_int ((Z.to_int z1) lsl (Z.to_int z2)) in
  let lshiftr (z1 : Z.t) (z2 : Z.t) : Z.t = Z.of_int ((Z.to_int z1) lsr (Z.to_int z2)) in
  match e with
  | IntLit i -> i
  | MaxAddr -> max_addr
  | AddOp (e1, e2) -> binop_eval Z.(+) e1 e2
  | SubOp (e1, e2) -> binop_eval Z.(-) e1 e2
  | MultOp (e1, e2) -> binop_eval Z.( * ) e1 e2
  | LandOp (e1, e2) -> binop_eval Z.(land) e1 e2
  | LorOp (e1, e2) -> binop_eval Z.(lor) e1 e2
  | LslOp (e1, e2) -> binop_eval lshiftl e1 e2
  | LsrOp (e1, e2) -> binop_eval lshiftr e1 e2

let translate_perm (p : perm) : Ast.perm = p
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

let rec translate_regfile (regfile : regfile_t) (max_addr : Z.t) : Ast.word Machine.RegMap.t =
  let init_regfile = Machine.RegMap.empty in
  match regfile with
  | [] -> init_regfile
  | (r, w) :: rf ->
      let nrf = translate_regfile rf max_addr in
      Machine.RegMap.add r (translate_word w max_addr) nrf

let rec translate_sregfile (sregfile : sregfile_t) (max_addr : Z.t) : Ast.word Machine.SRegMap.t =
  let init_sregfile = Machine.SRegMap.empty in
  match sregfile with
  | [] -> init_sregfile
  | (r, w) :: rf ->
      let nrf = translate_sregfile rf max_addr in
      Machine.SRegMap.add r (translate_word w max_addr) nrf
