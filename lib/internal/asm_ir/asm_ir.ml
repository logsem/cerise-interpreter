(* Types and final AST translation for the assembler intermediate representation. *)

exception ExprException of string
exception CurrentAddrException

type regname = Ast.regname
type sregname = Ast.sregname

type expr =
  | IntLit of Z.t
  | Label of string
  | CurrentAddr
  | AddOp of expr * expr
  | SubOp of expr * expr
  | MultOp of expr * expr
  | LandOp of expr * expr
  | LorOp of expr * expr
  | LslOp of expr * expr
  | LsrOp of expr * expr

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
  | PairParam of string * locality

type reg_or_const = Register of regname | Const of const_encoded | ValueParam of string

type sealable =
  | Cap of perm * locality * expr * expr * expr
  | SealRange of seal_perm * locality * expr * expr * expr

type word = I of expr | Sealable of sealable | Sealed of expr * sealable

exception WordException of word

type macro_call = { name : string; arguments : reg_or_const list; location : location }

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
  | LAnd of regname * reg_or_const * reg_or_const
  | LOr of regname * reg_or_const * reg_or_const
  | LShiftL of regname * reg_or_const * reg_or_const
  | LShiftR of regname * reg_or_const * reg_or_const
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
  | Define of string * expr * location
  | MacroDef of macro_definition
  | MacroCall of macro_call

and macro_definition = {
  name : string;
  parameters : parameter list;
  body : machine_op list;
  location : location;
}

type statement = machine_op (* TODO: PseudoOp and LabelDefs *)
type t = statement list
type env = (string * int) list

let current_label_str = "&reserved_current_lbl_"
let current_label_val = ref 0

let fresh_current_label () =
  let cur_val = !current_label_val in
  current_label_val := cur_val + 1;
  current_label_str ^ string_of_int cur_val

let rec pre_eval_expr (e : expr) : expr * string list =
  match e with
  | CurrentAddr ->
      let lbl = fresh_current_label () in
      (Label lbl, [ lbl ])
  | IntLit i -> (IntLit i, [])
  | Label s -> (Label s, [])
  | AddOp (e1, e2) ->
      let pre_e1, l_e1 = pre_eval_expr e1 in
      let pre_e2, l_e2 = pre_eval_expr e2 in
      (AddOp (pre_e1, pre_e2), List.append l_e1 l_e2)
  | SubOp (e1, e2) ->
      let pre_e1, l_e1 = pre_eval_expr e1 in
      let pre_e2, l_e2 = pre_eval_expr e2 in
      (SubOp (pre_e1, pre_e2), List.append l_e1 l_e2)
  | MultOp (e1, e2) ->
      let pre_e1, l_e1 = pre_eval_expr e1 in
      let pre_e2, l_e2 = pre_eval_expr e2 in
      (MultOp (pre_e1, pre_e2), List.append l_e1 l_e2)
  | LandOp (e1, e2) ->
      let pre_e1, l_e1 = pre_eval_expr e1 in
      let pre_e2, l_e2 = pre_eval_expr e2 in
      (LandOp (pre_e1, pre_e2), List.append l_e1 l_e2)
  | LorOp (e1, e2) ->
      let pre_e1, l_e1 = pre_eval_expr e1 in
      let pre_e2, l_e2 = pre_eval_expr e2 in
      (LorOp (pre_e1, pre_e2), List.append l_e1 l_e2)
  | LslOp (e1, e2) ->
      let pre_e1, l_e1 = pre_eval_expr e1 in
      let pre_e2, l_e2 = pre_eval_expr e2 in
      (LslOp (pre_e1, pre_e2), List.append l_e1 l_e2)
  | LsrOp (e1, e2) ->
      let pre_e1, l_e1 = pre_eval_expr e1 in
      let pre_e2, l_e2 = pre_eval_expr e2 in
      (LsrOp (pre_e1, pre_e2), List.append l_e1 l_e2)

let pre_eval_const (c : const_encoded) : const_encoded * string list =
  match c with
  | ConstExpr e ->
      let e, l = pre_eval_expr e in
      (ConstExpr e, l)
  | c -> (c, [])

let pre_eval_reg_or_const (rc : reg_or_const) : reg_or_const * string list =
  match rc with
  | Register r -> (Register r, [])
  | Const c ->
      let c, l = pre_eval_const c in
      (Const c, l)

let pre_eval_sealable (s : sealable) : sealable * string list =
  match s with
  | Cap (p, l, b, e, a) ->
      let b, lb = pre_eval_expr b in
      let e, le = pre_eval_expr e in
      let a, la = pre_eval_expr a in
      (Cap (p, l, b, e, a), List.append (List.append lb le) la)
  | SealRange (p, l, b, e, a) ->
      let b, lb = pre_eval_expr b in
      let e, le = pre_eval_expr e in
      let a, la = pre_eval_expr a in
      (SealRange (p, l, b, e, a), List.append (List.append lb le) la)

let pre_eval_word (w : word) : word * string list =
  match w with
  | I e ->
      let e, l = pre_eval_expr e in
      (I e, l)
  | Sealable s ->
      let s, l = pre_eval_sealable s in
      (Sealable s, l)
  | Sealed (e, s) ->
      let e, le = pre_eval_expr e in
      let s, ls = pre_eval_sealable s in
      (Sealed (e, s), List.append le ls)

let pre_eval_machine_op (o : machine_op) : machine_op * string list =
  match o with
  | Jalr (r1, r2) -> (Jalr (r1, r2), [])
  | Jmp r ->
      let r, l = pre_eval_reg_or_const r in
      (Jmp r, l)
  | Jnz (r1, r2) ->
      let r2, l = pre_eval_reg_or_const r2 in
      (Jnz (r1, r2), l)
  | ReadSR (r, sr) -> (ReadSR (r, sr), [])
  | WriteSR (sr, r) -> (WriteSR (sr, r), [])
  | Move (r, c) ->
      let c, l = pre_eval_reg_or_const c in
      (Move (r, c), l)
  | Load (r1, r2) -> (Load (r1, r2), [])
  | Store (r, c) ->
      let c, l = pre_eval_reg_or_const c in
      (Store (r, c), l)
  | Add (r, c1, c2) ->
      let c1, l1 = pre_eval_reg_or_const c1 in
      let c2, l2 = pre_eval_reg_or_const c2 in
      (Add (r, c1, c2), l1 @ l2)
  | Sub (r, c1, c2) ->
      let c1, l1 = pre_eval_reg_or_const c1 in
      let c2, l2 = pre_eval_reg_or_const c2 in
      (Sub (r, c1, c2), l1 @ l2)
  | Mul (r, c1, c2) ->
      let c1, l1 = pre_eval_reg_or_const c1 in
      let c2, l2 = pre_eval_reg_or_const c2 in
      (Mul (r, c1, c2), l1 @ l2)
  | Rem (r, c1, c2) ->
      let c1, l1 = pre_eval_reg_or_const c1 in
      let c2, l2 = pre_eval_reg_or_const c2 in
      (Rem (r, c1, c2), l1 @ l2)
  | Div (r, c1, c2) ->
      let c1, l1 = pre_eval_reg_or_const c1 in
      let c2, l2 = pre_eval_reg_or_const c2 in
      (Div (r, c1, c2), l1 @ l2)
  | LAnd (r, c1, c2) ->
      let c1, l1 = pre_eval_reg_or_const c1 in
      let c2, l2 = pre_eval_reg_or_const c2 in
      (LAnd (r, c1, c2), l1 @ l2)
  | LOr (r, c1, c2) ->
      let c1, l1 = pre_eval_reg_or_const c1 in
      let c2, l2 = pre_eval_reg_or_const c2 in
      (LOr (r, c1, c2), l1 @ l2)
  | LShiftL (r, c1, c2) ->
      let c1, l1 = pre_eval_reg_or_const c1 in
      let c2, l2 = pre_eval_reg_or_const c2 in
      (LShiftL (r, c1, c2), l1 @ l2)
  | LShiftR (r, c1, c2) ->
      let c1, l1 = pre_eval_reg_or_const c1 in
      let c2, l2 = pre_eval_reg_or_const c2 in
      (LShiftR (r, c1, c2), l1 @ l2)
  | Lt (r, c1, c2) ->
      let c1, l1 = pre_eval_reg_or_const c1 in
      let c2, l2 = pre_eval_reg_or_const c2 in
      (Lt (r, c1, c2), l1 @ l2)
  | Lea (r, c) ->
      let c, l = pre_eval_reg_or_const c in
      (Lea (r, c), l)
  | Restrict (r, c) ->
      let c, l = pre_eval_reg_or_const c in
      (Restrict (r, c), l)
  | SubSeg (r, c1, c2) ->
      let c1, l1 = pre_eval_reg_or_const c1 in
      let c2, l2 = pre_eval_reg_or_const c2 in
      (SubSeg (r, c1, c2), l1 @ l2)
  | GetL (r1, r2) -> (GetL (r1, r2), [])
  | GetB (r1, r2) -> (GetB (r1, r2), [])
  | GetE (r1, r2) -> (GetE (r1, r2), [])
  | GetA (r1, r2) -> (GetA (r1, r2), [])
  | GetP (r1, r2) -> (GetP (r1, r2), [])
  | GetOType (r1, r2) -> (GetOType (r1, r2), [])
  | GetWType (r1, r2) -> (GetWType (r1, r2), [])
  | Seal (r1, r2, r3) -> (Seal (r1, r2, r3), [])
  | UnSeal (r1, r2, r3) -> (UnSeal (r1, r2, r3), [])
  | Fail -> (Fail, [])
  | Halt -> (Halt, [])
  | Word w ->
      let w, l = pre_eval_word w in
      (Word w, l)
  | Lbl s -> (Lbl s, [])

let rec pre_eval_prog (prog : t) : t =
  match prog with
  | [] -> []
  | mchn_op :: p ->
      let pre_evaled_op, lbls = pre_eval_machine_op mchn_op in
      List.append (List.map (fun s -> Lbl s) lbls) (pre_evaled_op :: pre_eval_prog p)

let rec compute_env (i : int) (prog : t) (envr : env) : env =
  match prog with
  | [] -> envr
  | Lbl s :: p -> compute_env (i + 1) p ((s, i - List.length envr) :: envr)
  | _ :: p -> compute_env (i + 1) p envr

let rec eval_expr (envr : env) (e : expr) : Z.t =
  let binop_eval (binop : Z.t -> Z.t -> Z.t) (e1 : expr) (e2 : expr) : Z.t =
    binop (eval_expr envr e1) (eval_expr envr e2)
  in
  let lshiftl (z1 : Z.t) (z2 : Z.t) : Z.t = Z.of_int (Z.to_int z1 lsl Z.to_int z2) in
  let lshiftr (z1 : Z.t) (z2 : Z.t) : Z.t = Z.of_int (Z.to_int z1 lsr Z.to_int z2) in
  match e with
  | IntLit i -> i
  | CurrentAddr -> raise CurrentAddrException
  | Label s -> (
      match List.find_opt (fun p -> fst p = s) envr with
      | Some (_, i) -> Z.of_int i
      | None -> raise (UnknownLabelException s))
  | AddOp (e1, e2) -> binop_eval Z.( + ) e1 e2
  | SubOp (e1, e2) -> binop_eval Z.( - ) e1 e2
  | MultOp (e1, e2) -> binop_eval Z.( * ) e1 e2
  | LandOp (e1, e2) -> binop_eval Z.( land ) e1 e2
  | LorOp (e1, e2) -> binop_eval Z.( lor ) e1 e2
  | LslOp (e1, e2) -> binop_eval lshiftl e1 e2
  | LsrOp (e1, e2) -> binop_eval lshiftr e1 e2

let translate_perm (p : perm) : Ast.perm = p
let translate_locality (g : locality) : Ast.locality = g
let translate_wt (wt : wtype) : Ast.wtype = wt
let translate_regname (r : regname) : Ast.regname = r
let translate_sregname (sr : sregname) : Ast.sregname = sr

let translate_reg_or_const (envr : env) (roc : reg_or_const) : Ast.reg_or_const =
  match roc with
  | Register r -> Ast.Register (translate_regname r)
  | ValueParam name -> raise (UnexpandedMacroException ("value parameter $" ^ name))
  | Const c ->
      Ast.Const
        (match c with
        | ConstExpr e -> eval_expr envr e
        | Locality l -> Encode.encode_locality (translate_locality l)
        | Perm p -> Encode.encode_perm (translate_perm p)
        | SealPerm sp -> Encode.encode_seal_perm (translate_seal_perm sp)
        | Wtype wt -> Encode.encode_wtype (translate_wt wt)
        | PermLoc (p, l) -> Encode.encode_perm_loc_pair (translate_perm p) (translate_locality l)
        | SealPermLoc (p, l) ->
            Encode.encode_seal_perm_loc_pair (translate_seal_perm p) (translate_locality l)
        | PairParam (name, _) ->
            raise (UnexpandedMacroException ("permission-pair parameter $" ^ name)))

let translate_sealable (s : sealable) : Ast.sealable =
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

let translate_word (w : word) : Ast.statement =
  match w with
  | I e ->
      let z' = eval_expr envr e in
      Ast.Word (Ast.I z')
  | Sealable sb -> Ast.Word (Ast.Sealable (translate_sealable sb))
  | Sealed (o, sb) ->
      let ot = eval_expr envr o in
      Ast.Word (Ast.Sealed (ot, translate_sealable envr sb))

let translate_instr (instr : machine_op) : Ast.machine_op =
  match instr with
  | Jalr (r1, r2) -> Ast.Jalr (translate_regname r1, translate_regname r2)
  | Jmp r -> Ast.Jmp (translate_reg_or_const envr r)
  | Jnz (r1, r2) -> Ast.Jnz (translate_regname r1, translate_reg_or_const envr r2)
  | ReadSR (r, sr) -> Ast.ReadSR (translate_regname r, translate_sregname sr)
  | WriteSR (sr, r) -> Ast.WriteSR (translate_sregname sr, translate_regname r)
  | Move (r, c) -> Ast.Move (translate_regname r, translate_reg_or_const envr c)
  | Load (r1, r2) -> Ast.Load (translate_regname r1, translate_regname r2)
  | Store (r, c) -> Ast.Store (translate_regname r, translate_reg_or_const c)
  | Add (r, c1, c2) ->
      Ast.Add (translate_regname r, translate_reg_or_const c1, translate_reg_or_const c2)
  | Sub (r, c1, c2) ->
      Ast.Sub (translate_regname r, translate_reg_or_const c1, translate_reg_or_const c2)
  | Mul (r, c1, c2) ->
      Ast.Mul (translate_regname r, translate_reg_or_const c1, translate_reg_or_const c2)
  | Rem (r, c1, c2) ->
      Ast.Rem (translate_regname r, translate_reg_or_const c1, translate_reg_or_const c2)
  | Div (r, c1, c2) ->
      Ast.Div (translate_regname r, translate_reg_or_const envr c1, translate_reg_or_const envr c2)
  | LAnd (r, c1, c2) ->
      Ast.LAnd (translate_regname r, translate_reg_or_const envr c1, translate_reg_or_const envr c2)
  | LOr (r, c1, c2) ->
      Ast.LOr (translate_regname r, translate_reg_or_const envr c1, translate_reg_or_const envr c2)
  | LShiftL (r, c1, c2) ->
      Ast.LShiftL
        (translate_regname r, translate_reg_or_const envr c1, translate_reg_or_const envr c2)
  | LShiftR (r, c1, c2) ->
      Ast.LShiftR
        (translate_regname r, translate_reg_or_const envr c1, translate_reg_or_const envr c2)
  | Lt (r, c1, c2) ->
      Ast.Lt (translate_regname r, translate_reg_or_const c1, translate_reg_or_const c2)
  | Lea (r, c) -> Ast.Lea (translate_regname r, translate_reg_or_const c)
  | Restrict (r, c) -> Ast.Restrict (translate_regname r, translate_reg_or_const c)
  | SubSeg (r, c1, c2) ->
      Ast.SubSeg (translate_regname r, translate_reg_or_const c1, translate_reg_or_const c2)
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
  | Lbl name -> raise (UnresolvedIrException ("label declaration " ^ name))
  | Define (_, _, location) ->
      raise
        (UnexpandedMacroException
           (Printf.sprintf "%s:%d: integer definition" location.filename location.line))
  | MacroDef definition -> raise (UnexpandedMacroException ("macro definition " ^ definition.name))
  | MacroCall call -> raise (UnexpandedMacroException ("macro call " ^ call.name))

let translate_statement (statement : statement) : Ast.statement =
  match statement with
  | Word word -> translate_word word
  | instruction -> Op (translate_instr instruction)

let translate_prog (prog : t) : Ast.t =
  let prog = pre_eval_prog prog in
  let envr = compute_env 0 prog [] in
  translate_prog_aux envr prog
