(* Type definitions for the syntax AST *)

exception UnknownLabelException of string
exception ExprException of string

type regname = Ast.regname
type sregname = Ast.sregname

type expr =
  | IntLit of Z.t
  | Label of string
  | AddOp of expr * expr
  | SubOp of expr * expr
  | MultOp of expr * expr

type perm = Ast.perm
type locality = Ast.locality
type seal_perm = Ast.seal_perm
type wtype = Ast.wtype

type const_encoded =
  | ConstExpr of expr
  | Perm of perm
  | SealPerm of seal_perm
  | Locality of locality
  | Wtype of wtype
  | PermLoc of perm * locality
  | SealPermLoc of seal_perm * locality

type reg_or_const = Register of regname | Const of const_encoded

type sealable =
  | Cap of perm * locality * expr * expr * expr
  | SealRange of seal_perm * locality * expr * expr * expr

type word = I of expr | Sealable of sealable | Sealed of expr * sealable

exception WordException of word

type machine_op =
  | Jalr of regname * regname
  | Jmp of reg_or_const
  | Jnz of regname * reg_or_const
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
  | Lbl of string
  | Word of word

type statement = machine_op (* TODO: PseudoOp and LabelDefs *)
type t = statement list
type env = (string * int) list

let rec compute_env (i : int) (prog : t) (envr : env) : env =
  match prog with
  | [] -> envr
  | Lbl s :: p -> compute_env (i + 1) p ((s, i - List.length envr) :: envr)
  | _ :: p -> compute_env (i + 1) p envr

let rec eval_expr (envr : env) (e : expr) : Z.t =
  match e with
  | IntLit i -> i
  | Label s -> (
      match List.find_opt (fun p -> fst p = s) envr with
      | Some (_, i) -> Z.of_int i
      | None -> raise (UnknownLabelException s))
  | AddOp (e1, e2) -> Z.(eval_expr envr e1 + eval_expr envr e2)
  | SubOp (e1, e2) -> Z.(eval_expr envr e1 - eval_expr envr e2)
  | MultOp (e1, e2) -> Z.(eval_expr envr e1 * eval_expr envr e2)

let translate_perm (p : perm) : Ast.perm = p
let translate_locality (g : locality) : Ast.locality = g
let translate_wt (wt : wtype) : Ast.wtype = wt
let translate_regname (r : regname) : Ast.regname = r
let translate_sregname (sr : sregname) : Ast.sregname = sr

let translate_reg_or_const (envr : env) (roc : reg_or_const) : Ast.reg_or_const =
  match roc with
  | Register r -> Ast.Register (translate_regname r)
  | Const c ->
      Ast.Const
        (match c with
        | ConstExpr e -> eval_expr envr e
        | Locality l -> Encode.encode_locality (translate_locality l)
        | Perm p -> Encode.encode_perm (translate_perm p)
        | SealPerm sp -> Encode.encode_seal_perm sp
        | Wtype wt -> Encode.encode_wtype (translate_wt wt)
        | PermLoc (p, l) -> Encode.encode_perm_loc_pair (translate_perm p) (translate_locality l)
        | SealPermLoc (p, l) -> Encode.encode_seal_perm_loc_pair p (translate_locality l))

let translate_sealable (envr : env) (s : sealable) : Ast.sealable =
  match s with
  | Cap (p, l, b, e, a) ->
      let b' = eval_expr envr b in
      let a' = eval_expr envr a in
      Ast.Cap (translate_perm p, translate_locality l, b', eval_expr envr e, a')
  | SealRange (p, l, b, e, a) ->
      let b' = eval_expr envr b in
      let e' = eval_expr envr e in
      let a' = eval_expr envr a in
      Ast.SealRange (p, translate_locality l, b', e', a')

let translate_word (envr : env) (w : word) : Ast.statement =
  match w with
  | I e ->
      let z' = eval_expr envr e in
      Ast.Word (Ast.I z')
  | Sealable sb -> Ast.Word (Ast.Sealable (translate_sealable envr sb))
  | Sealed (o, sb) ->
      let ot = eval_expr envr o in
      Ast.Word (Ast.Sealed (ot, translate_sealable envr sb))

let translate_instr (envr : env) (instr : machine_op) : Ast.machine_op =
  match instr with
  | Jalr (r1, r2) -> Ast.Jalr (translate_regname r1, translate_regname r2)
  | Jmp r -> Ast.Jmp (translate_reg_or_const envr r)
  | Jnz (r1, r2) -> Ast.Jnz (translate_regname r1, translate_reg_or_const envr r2)
  | ReadSR (r, sr) -> Ast.ReadSR (translate_regname r, translate_sregname sr)
  | WriteSR (sr, r) -> Ast.WriteSR (translate_sregname sr, translate_regname r)
  | Move (r, c) -> Ast.Move (translate_regname r, translate_reg_or_const envr c)
  | Load (r1, r2) -> Ast.Load (translate_regname r1, translate_regname r2)
  | Store (r, c) -> Ast.Store (translate_regname r, translate_reg_or_const envr c)
  | Add (r, c1, c2) ->
      Ast.Add (translate_regname r, translate_reg_or_const envr c1, translate_reg_or_const envr c2)
  | Sub (r, c1, c2) ->
      Ast.Sub (translate_regname r, translate_reg_or_const envr c1, translate_reg_or_const envr c2)
  | Mul (r, c1, c2) ->
      Ast.Mul (translate_regname r, translate_reg_or_const envr c1, translate_reg_or_const envr c2)
  | Rem (r, c1, c2) ->
      Ast.Rem (translate_regname r, translate_reg_or_const envr c1, translate_reg_or_const envr c2)
  | Div (r, c1, c2) ->
      Ast.Div (translate_regname r, translate_reg_or_const envr c1, translate_reg_or_const envr c2)
  | Lt (r, c1, c2) ->
      Ast.Lt (translate_regname r, translate_reg_or_const envr c1, translate_reg_or_const envr c2)
  | Lea (r, c) -> Ast.Lea (translate_regname r, translate_reg_or_const envr c)
  | Restrict (r, c) -> Ast.Restrict (translate_regname r, translate_reg_or_const envr c)
  | SubSeg (r, c1, c2) ->
      Ast.SubSeg
        (translate_regname r, translate_reg_or_const envr c1, translate_reg_or_const envr c2)
  | GetL (r1, r2) -> Ast.GetL (translate_regname r1, translate_regname r2)
  | GetB (r1, r2) -> Ast.GetB (translate_regname r1, translate_regname r2)
  | GetE (r1, r2) -> Ast.GetE (translate_regname r1, translate_regname r2)
  | GetA (r1, r2) -> Ast.GetA (translate_regname r1, translate_regname r2)
  | GetP (r1, r2) -> Ast.GetP (translate_regname r1, translate_regname r2)
  | GetOType (r1, r2) -> Ast.GetOType (translate_regname r1, translate_regname r2)
  | GetWType (r1, r2) -> Ast.GetWType (translate_regname r1, translate_regname r2)
  | Seal (r1, r2, r3) -> Ast.Seal (translate_regname r1, translate_regname r2, translate_regname r3)
  | UnSeal (r1, r2, r3) ->
      Ast.UnSeal (translate_regname r1, translate_regname r2, translate_regname r3)
  | Fail -> Ast.Fail
  | Halt -> Ast.Halt
  | Word w -> raise (WordException w)
  | Lbl s -> raise (UnknownLabelException s)

let rec translate_prog_aux (envr : env) (prog : t) : Ast.t =
  match prog with
  | [] -> []
  | Lbl _ :: p -> translate_prog_aux envr p
  | Word w :: p -> translate_word envr w :: translate_prog_aux envr p
  | instr :: p -> Op (translate_instr envr instr) :: translate_prog_aux envr p

let translate_prog (prog : t) : Ast.t =
  let envr = compute_env 0 prog [] in
  translate_prog_aux envr prog
