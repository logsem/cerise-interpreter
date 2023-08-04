(* Type definitions for the syntax AST *)

exception UnknownLabelException of string

type regname = PC | Reg of int
type expr
  = IntLit of int
  | Label of string
  | AddOp of expr * expr
  | SubOp of expr * expr

type perm = O | E | RO | RX | RW | RWX
type seal_perm = bool * bool
type wtype = W_I | W_Cap | W_SealRange | W_Sealed
type const_encoded = ConstExpr of expr | Perm of perm | SealPerm of seal_perm | Wtype of wtype
type reg_or_const = Register of regname | Const of const_encoded
type sealable = Cap of perm * expr * expr * expr | SealRange of seal_perm * expr * expr * expr
type word = I of expr | Sealable of sealable | Sealed of expr * sealable
exception WordException of word

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
  | Lbl of string
  | Word of word
type statement = machine_op (* TODO: PseudoOp and LabelDefs *)

type t = statement list
type env = (string * int) list

let rec compute_env (i : int) (prog : t) (envr : env) : env =
  match prog with
  | [] -> envr
  | (Lbl s) :: p -> compute_env (i+1) p ((s, i - (List.length envr)) :: envr)
  | _ :: p -> compute_env (i+1) p envr

let rec eval_expr (envr : env) (e : expr) : Z.t =
  match e with
  | IntLit i -> (Z.of_int i)
  | Label s -> begin
      match List.find_opt (fun p -> (fst p) = s) envr with
      | Some (_,i) -> (Z.of_int i)
      | None -> raise (UnknownLabelException s)
    end
  | AddOp (e1, e2) -> Z.((eval_expr envr e1) + (eval_expr envr e2))
  | SubOp (e1, e2) -> Z.((eval_expr envr e1) - (eval_expr envr e2))

let translate_perm (p : perm) : Ast.perm =
  match p with
  | O -> Ast.O
  | E -> Ast.E
  | RO -> Ast.RO
  | RX -> Ast.RX
  | RW -> Ast.RW
  | RWX -> Ast.RWX

let translate_wt (wt : wtype) : Ast.wtype =
  (match wt with
   | W_I -> Ast.W_I
   | W_Cap -> Ast.W_Cap
   | W_SealRange -> Ast.W_SealRange
   | W_Sealed -> Ast.W_Sealed)

let translate_regname (r : regname) : Ast.regname =
  match r with
  | PC -> Ast.PC
  | Reg i -> Ast.Reg i

let translate_reg_or_const (envr : env) (roc : reg_or_const) : Ast.reg_or_const =
  match roc with
  | Register r -> Ast.Register (translate_regname r)
  | Const c ->
    Ast.Const
    (match c with
        | ConstExpr e -> (eval_expr envr e)
        | Perm p -> Encode.encode_perm (translate_perm p)
        | SealPerm sp -> Encode.encode_seal_perm sp
        | Wtype wt -> Encode.encode_wtype (translate_wt wt))

let translate_sealable (envr : env) (s : sealable) : Ast.sealable =
  match s with
  | Cap (p,b,e,a) ->
    Ast.Cap ((translate_perm p),
             (eval_expr envr b),
             (eval_expr envr e),
             (eval_expr envr a))
  | SealRange (p, b, e, a) ->
    Ast.SealRange (p,
                   (eval_expr envr b),
                   (eval_expr envr e),
                   (eval_expr envr a))

let translate_word (envr : env) (w : word) : Ast.statement =
  match w with
  | I e -> Ast.Word (Ast.I (eval_expr envr e))
  | Sealable sb -> Ast.Word (Ast.Sealable (translate_sealable envr sb))
  | Sealed (o,sb) -> Ast.Word (Ast.Sealed (eval_expr envr o, (translate_sealable envr sb)))

let translate_instr (envr : env) (instr : machine_op) : Ast.machine_op =
  match instr with
  | Jmp r -> Ast.Jmp (translate_regname r)
  | Jnz (r1, r2) -> Ast.Jnz (translate_regname r1, translate_regname r2)
  | Move (r, c) -> Ast.Move (translate_regname r,
                             translate_reg_or_const envr c)
  | Load (r1, r2) -> Ast.Load (translate_regname r1,
                               translate_regname r2)
  | Store (r, c) -> Ast.Store (translate_regname r,
                               translate_reg_or_const envr c)
  | Add (r, c1, c2) -> Ast.Add (translate_regname r,
                                translate_reg_or_const envr c1,
                                translate_reg_or_const envr c2)
  | Sub (r, c1, c2) -> Ast.Sub (translate_regname r,
                                translate_reg_or_const envr c1,
                                translate_reg_or_const envr c2)
  | Mul (r, c1, c2) -> Ast.Mul (translate_regname r,
                                translate_reg_or_const envr c1,
                                translate_reg_or_const envr c2)
  | Rem (r, c1, c2) -> Ast.Rem (translate_regname r,
                                translate_reg_or_const envr c1,
                                translate_reg_or_const envr c2)
  | Div (r, c1, c2) -> Ast.Div (translate_regname r,
                                translate_reg_or_const envr c1,
                                translate_reg_or_const envr c2)
  | Lt (r, c1, c2) -> Ast.Lt (translate_regname r,
                              translate_reg_or_const envr c1,
                              translate_reg_or_const envr c2)
  | Lea (r, c) -> Ast.Lea (translate_regname r, translate_reg_or_const envr c)
  | Restrict (r, c) -> Ast.Restrict (translate_regname r,
                                     translate_reg_or_const envr c)
  | SubSeg (r, c1, c2) -> Ast.SubSeg (translate_regname r,
                                      translate_reg_or_const envr c1,
                                      translate_reg_or_const envr c2)
  | GetB (r1, r2) -> Ast.GetB (translate_regname r1, translate_regname r2)
  | GetE (r1, r2) -> Ast.GetE (translate_regname r1, translate_regname r2)
  | GetA (r1, r2) -> Ast.GetA (translate_regname r1, translate_regname r2)
  | GetP (r1, r2) -> Ast.GetP (translate_regname r1, translate_regname r2)
  | GetOType (r1, r2) -> Ast.GetOType (translate_regname r1, translate_regname r2)
  | GetWType (r1, r2) -> Ast.GetWType (translate_regname r1, translate_regname r2)
  | Seal (r1, r2, r3) -> Ast.Seal (translate_regname r1, translate_regname r2, translate_regname r3)
  | UnSeal (r1, r2, r3) -> Ast.UnSeal (translate_regname r1, translate_regname r2, translate_regname r3)
  | Fail -> Ast.Fail
  | Halt -> Ast.Halt
  | Word w -> raise (WordException w)
  | Lbl s -> raise (UnknownLabelException s)

let rec translate_prog_aux (envr : env) (prog : t) : Ast.t =
  match prog with
  | [] -> []
  | (Lbl _) :: p -> translate_prog_aux envr p
  | (Word w) :: p -> translate_word envr w :: (translate_prog_aux envr p)
  | instr :: p -> (Op (translate_instr envr instr)) :: (translate_prog_aux envr p)

let translate_prog (prog : t) : Ast.t =
  let envr = compute_env 0 prog [] in
  translate_prog_aux envr prog
