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

let ddc = Reg 0
let stk = Reg 31

type expr =
  | IntLit of Infinite_z.t
  | Symbol of string
  | Label of string
  | AddOp of expr * expr
  | SubOp of expr * expr
  | ExprParam of string

exception UnresolvedExpressionException of expr

type perm =
  | O
  | E
  | RO
  | RX
  | RW
  | RWX
  | RWL
  | RWLX
  | URW
  | URWL
  | URWX
  | URWLX
  | PermParam of string

type locality = Global | Local | Directed | LocalityParam of string
type seal_perm = SealPermLit of bool * bool | SealPermParam of string
type wtype = W_I | W_Cap | W_SealRange | W_Sealed | WtypeParam of string

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
  | Invoke of regname * regname
  | LoadU of regname * regname * reg_or_const
  | StoreU of regname * reg_or_const * reg_or_const
  | PromoteU of regname
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
  | PermParam name -> raise (UnexpandedMacroException ("permission parameter $" ^ name))

let translate_locality (g : locality) : Ast.locality =
  match g with
  | Local -> Ast.Local
  | Global -> Ast.Global
  | Directed -> Ast.Directed
  | LocalityParam name -> raise (UnexpandedMacroException ("locality parameter $" ^ name))

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

(* Check whether the encoded constant is supported *)
let check_ir_const (c : const_encoded) =
  let open Parameters in
  match c with
  | Perm p -> (
      match p with
      | RWL | RWLX ->
          if !flags.locality = Global then
            not_supported "Parsing: Write-local permissions are not supported."
      | URW | URWX ->
          if not !flags.unitialized then not_supported "Parsing: U-permissions are not supported."
      | URWL | URWLX ->
          if not !flags.unitialized then not_supported "Parsing: U-permissions are not supported."
          else if !flags.locality = Global then
            not_supported "Parsing: Write-local permissions are not supported."
      | _ -> ())
  | SealPerm _ ->
      if not !flags.sealing then not_supported "Parsing: Sealing permissions are not supported."
  | PermLoc (_, _) | Locality _ ->
      if !flags.locality = Global then not_supported "Parsing: Locality is not supported."
  | SealPermLoc (_, _) ->
      if !flags.locality = Global then not_supported "Parsing: Locality is not supported."
      else if not !flags.sealing then
        not_supported "Parsing: Sealing permissions are not supported."
  | PairParam (name, _) -> raise (UnexpandedMacroException ("permission-pair parameter $" ^ name))
  | Wtype _ | ConstExpr _ -> ()

let resolved_expression (expression : expr) : Infinite_z.t =
  match expression with
  | IntLit value -> value
  | _ -> raise (UnresolvedExpressionException expression)

let resolved_finite_expression (expression : expr) (error : string) : Z.t =
  match resolved_expression expression with
  | Int value -> value
  | Inf -> raise (ExprException error)

let translate_reg_or_const (roc : reg_or_const) : Ast.reg_or_const =
  match roc with
  | Register r -> Ast.Register (translate_regname r)
  | ValueParam name -> raise (UnexpandedMacroException ("value parameter $" ^ name))
  | Const c ->
      check_ir_const c;
      Ast.Const
        (match c with
        | ConstExpr expression ->
            resolved_finite_expression expression "Constants expressions cannot be ∞"
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
      let b' = resolved_finite_expression b "Lower capability bound cannot be ∞" in
      let a' = resolved_finite_expression a "Current capability address cannot be ∞" in
      Ast.Cap (translate_perm p, translate_locality l, b', resolved_expression e, a')
  | SealRange (p, l, b, e, a) ->
      let b' = resolved_finite_expression b "Lower otype bound cannot be ∞" in
      let e' = resolved_finite_expression e "Upper otype bound cannot be ∞" in
      let a' = resolved_finite_expression a "Current sealing otype cannot be ∞" in
      Ast.SealRange (translate_seal_perm p, translate_locality l, b', e', a')

let translate_word (w : word) : Ast.statement =
  match w with
  | I e ->
      let z' = resolved_finite_expression e "Integer machine word cannot be ∞" in
      Ast.Word (Ast.I z')
  | Sealable sb -> Ast.Word (Ast.Sealable (translate_sealable sb))
  | Sealed (o, sb) ->
      let ot = resolved_finite_expression o "OType of sealed word cannot be ∞" in
      Ast.Word (Ast.Sealed (ot, translate_sealable sb))

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
  | Invoke (r1, r2) -> Ast.Invoke (translate_regname r1, translate_regname r2)
  | LoadU (r1, r2, c) ->
      Ast.LoadU (translate_regname r1, translate_regname r2, translate_reg_or_const c)
  | StoreU (r, c1, c2) ->
      Ast.StoreU (translate_regname r, translate_reg_or_const c1, translate_reg_or_const c2)
  | PromoteU r -> Ast.PromoteU (translate_regname r)
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
