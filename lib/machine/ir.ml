(* Type definitions for the syntax AST *)
open Misc

exception UnknownLabelException of string
exception ExprException of string

type regname = PC | Reg of int
let ddc = Reg 0
let stk = Reg 31

type expr
  = IntLit of Infinite_z.t
  | Label of string
  | AddOp of expr * expr
  | SubOp of expr * expr

type perm = O | E | RO | RX | RW | RWX | RWL | RWLX | URW | URWL | URWX | URWLX
type locality = Global | Local | Directed
type seal_perm = bool * bool
type wtype = W_I | W_Cap | W_SealRange | W_Sealed
type const_encoded = ConstExpr of expr
                   | Perm of perm
                   | SealPerm of seal_perm
                   | Locality of locality
                   | Wtype of wtype
                   | PermLoc of perm * locality
                   | SealPermLoc of seal_perm * locality

type reg_or_const = Register of regname | Const of const_encoded
type sealable = Cap of perm * locality * expr * expr * expr | SealRange of seal_perm * locality * expr * expr * expr
type word = I of expr | Sealable of sealable | Sealed of expr * sealable
exception WordException of word

type machine_op
  =
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
  | Invoke of regname * regname
  | LoadU of regname * regname * reg_or_const
  | StoreU of regname * reg_or_const * reg_or_const
  | PromoteU of regname
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

let rec eval_expr (envr : env) (e : expr) : Infinite_z.t =
  match e with
  | IntLit i -> i
  | Label s -> begin
      match List.find_opt (fun p -> (fst p) = s) envr with
      | Some (_,i) -> Int (Z.of_int i)
      | None -> raise (UnknownLabelException s)
    end
  | AddOp (e1, e2) -> Infinite_z.((eval_expr envr e1) + (eval_expr envr e2))
  | SubOp (e1, e2) -> Infinite_z.((eval_expr envr e1) - (eval_expr envr e2))

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

(* Check whether the encoded constant is supported *)
let check_ir_const (c : const_encoded) =
  let open Parameters in
  match c with
  | Perm p ->
    (match p with
     | RWL
     | RWLX ->
       if !flags.locality = Global
       then not_supported "Parsing: Write-local permissions are not supported."
     | URW
     | URWX ->
       if not !flags.unitialized
       then not_supported "Parsing: U-permissions are not supported."
     | URWL
     | URWLX ->
       if not !flags.unitialized
       then not_supported "Parsing: U-permissions are not supported."
       else if !flags.locality = Global
       then not_supported "Parsing: Write-local permissions are not supported."
     | _ -> ()
    )
  | SealPerm _ ->
    if not !flags.sealing
    then not_supported "Parsing: Sealing permissions are not supported."
  | PermLoc (_, _)
  | Locality _ ->
    if !flags.locality = Global
    then not_supported "Parsing: Locality is not supported."
  | SealPermLoc (_, _) ->
    if !flags.locality = Global
    then not_supported "Parsing: Locality is not supported."
    else if not !flags.sealing
    then not_supported "Parsing: Sealing permissions are not supported."
  | Wtype _
  | ConstExpr _ -> ()

let eval_to_z (envr : env) (e : expr) (except_str : string) =
  match (eval_expr envr e) with
  | Int z -> z
  | Inf -> raise @@ ExprException except_str

let translate_reg_or_const (envr : env) (roc : reg_or_const) : Ast.reg_or_const =
  match roc with
  | Register r -> Ast.Register (translate_regname r)
  | Const c ->
    check_ir_const c;
    Ast.Const
      (match c with
       | ConstExpr e -> eval_to_z envr e "Constants expressions cannot be ∞"
       | Locality l -> Encode.encode_locality (translate_locality l)
       | Perm p -> Encode.encode_perm (translate_perm p)
       | SealPerm sp -> Encode.encode_seal_perm sp
       | Wtype wt -> Encode.encode_wtype (translate_wt wt)
       | PermLoc (p,l) -> Encode.encode_perm_loc_pair (translate_perm p) (translate_locality l)
       | SealPermLoc (p,l) -> Encode.encode_seal_perm_loc_pair p (translate_locality l)
      )

let translate_sealable (envr : env) (s : sealable) : Ast.sealable =
  match s with
  | Cap (p, l, b, e, a) ->
    let b' = eval_to_z envr b "Lower capability bound cannot be ∞" in
    let a' = eval_to_z envr a "Current capability address cannot be ∞" in
    Ast.Cap ((translate_perm p), (translate_locality l), b', (eval_expr envr e), a')
  | SealRange (p, l, b, e, a) ->
    let b' = eval_to_z envr b "Lower otype bound cannot be ∞" in
    let e' = eval_to_z envr e "Upper otype bound cannot be ∞" in
    let a' = eval_to_z envr a "Current sealing otype cannot be ∞" in
    Ast.SealRange (p, (translate_locality l), b', e', a')

let translate_word (envr : env) (w : word) : Ast.statement =
  match w with
  | I e ->
    let z' = eval_to_z envr e "Integer machine word cannot be ∞" in
    Ast.Word (Ast.I z')
  | Sealable sb -> Ast.Word (Ast.Sealable (translate_sealable envr sb))
  | Sealed (o,sb) ->
    let ot = eval_to_z envr o "OType of sealed word cannot be ∞" in
    Ast.Word (Ast.Sealed (ot, (translate_sealable envr sb)))

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
  | GetL (r1, r2) -> Ast.GetL (translate_regname r1, translate_regname r2)
  | GetB (r1, r2) -> Ast.GetB (translate_regname r1, translate_regname r2)
  | GetE (r1, r2) -> Ast.GetE (translate_regname r1, translate_regname r2)
  | GetA (r1, r2) -> Ast.GetA (translate_regname r1, translate_regname r2)
  | GetP (r1, r2) -> Ast.GetP (translate_regname r1, translate_regname r2)
  | GetOType (r1, r2) -> Ast.GetOType (translate_regname r1, translate_regname r2)
  | GetWType (r1, r2) -> Ast.GetWType (translate_regname r1, translate_regname r2)
  | Seal (r1, r2, r3) -> Ast.Seal (translate_regname r1, translate_regname r2, translate_regname r3)
  | UnSeal (r1, r2, r3) -> Ast.UnSeal (translate_regname r1, translate_regname r2, translate_regname r3)
  | Invoke (r1, r2) -> Ast.Invoke (translate_regname r1, translate_regname r2)
  | LoadU (r1, r2, c) -> Ast.LoadU (translate_regname r1,
                                    translate_regname r2,
                                    translate_reg_or_const envr c)
  | StoreU (r, c1, c2) -> Ast.StoreU (translate_regname r,
                                      translate_reg_or_const envr c1,
                                      translate_reg_or_const envr c2)
  | PromoteU r -> Ast.PromoteU (translate_regname r)
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
