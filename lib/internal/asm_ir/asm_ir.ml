(* Types and final AST translation for the assembler intermediate representation. *)

exception ExprException of string
exception UnexpandedMacroException of string
exception UnresolvedIrException of string

type location = { filename : string; line : int; column : int }

type parameter_kind =
  | RegKind
  | ValueKind
  | ExprKind
  | PermKind
  | SealPermKind
  | LocalityKind
  | WtypeKind
  | UnknownKind of string

type parameter = { name : string; kind : parameter_kind }
type regname = PC | Reg of int | RegParam of string

let cgp = Reg 0
let stk = Reg 31

type expr = IntLit of Z.t | Label of string | AddOp of expr * expr | SubOp of expr * expr
type perm = O | E | RO | RX | RW | RWX | RWL | RWLX
type locality = Global | Local
type seal_perm = bool * bool
type wtype = W_I | W_Cap | W_SealRange | W_Sealed

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

let translate_wt (wt : wtype) : Ast.wtype =
  match wt with
  | W_I -> Ast.W_I
  | W_Cap -> Ast.W_Cap
  | W_SealRange -> Ast.W_SealRange
  | W_Sealed -> Ast.W_Sealed
  | WtypeParam name -> raise (UnexpandedMacroException ("word-type parameter $" ^ name))

let translate_regname (r : regname) : Ast.regname =
  match r with
  | PC -> Ast.PC
  | Reg i -> Ast.Reg i
  | RegParam name -> raise (UnexpandedMacroException ("register parameter $" ^ name))

let translate_seal_perm = function
  | SealPermLit (seal, unseal) -> (seal, unseal)
  | SealPermParam name -> raise (UnexpandedMacroException ("sealing-permission parameter $" ^ name))

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
  | Jmp r -> Ast.Jmp (translate_regname r)
  | Jnz (r1, r2) -> Ast.Jnz (translate_regname r1, translate_regname r2)
  | Move (r, c) -> Ast.Move (translate_regname r, translate_reg_or_const c)
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
      Ast.Div (translate_regname r, translate_reg_or_const c1, translate_reg_or_const c2)
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

let translate_prog (program : t) : Ast.t = List.map translate_statement program
