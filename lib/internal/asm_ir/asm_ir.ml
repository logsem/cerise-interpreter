(* Types and final AST translation for the Griotte assembler intermediate representation. *)

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
type sregname = Ast.sregname

type expr =
  | IntLit of Z.t
  | CurrentAddr
  | Symbol of string
  | Label of string
  | AddOp of expr * expr
  | SubOp of expr * expr
  | MultOp of expr * expr
  | LandOp of expr * expr
  | LorOp of expr * expr
  | LslOp of expr * expr
  | LsrOp of expr * expr
  | ExprParam of string

exception UnresolvedExpressionException of expr

type perm = PermLit of Ast.perm | PermParam of string
type locality = Global | Local | LocalityParam of string
type seal_perm = SealPermLit of bool * bool | SealPermParam of string
type wtype = W_I | W_Cap | W_SealRange | W_Sealed | W_Sentry | WtypeParam of string

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

type word =
  | I of expr
  | Sealable of sealable
  | Sentry of perm * locality * expr * expr * expr
  | Sealed of expr * sealable

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

type statement = machine_op
type t = statement list

let translate_regname (register : regname) : Ast.regname =
  match register with
  | PC -> Ast.PC
  | Reg number -> Ast.Reg number
  | RegParam name -> raise (UnexpandedMacroException ("register parameter $" ^ name))

let translate_perm (permission : perm) : Ast.perm =
  match permission with
  | PermLit permission -> permission
  | PermParam name -> raise (UnexpandedMacroException ("permission parameter $" ^ name))

let translate_locality (locality : locality) : Ast.locality =
  match locality with
  | Global -> Ast.Global
  | Local -> Ast.Local
  | LocalityParam name -> raise (UnexpandedMacroException ("locality parameter $" ^ name))

let translate_seal_perm (permission : seal_perm) : Ast.seal_perm =
  match permission with
  | SealPermLit (seal, unseal) -> (seal, unseal)
  | SealPermParam name -> raise (UnexpandedMacroException ("sealing-permission parameter $" ^ name))

let translate_wtype (word_type : wtype) : Ast.wtype =
  match word_type with
  | W_I -> Ast.W_I
  | W_Cap -> Ast.W_Cap
  | W_SealRange -> Ast.W_SealRange
  | W_Sealed -> Ast.W_Sealed
  | W_Sentry -> Ast.W_Sentry
  | WtypeParam name -> raise (UnexpandedMacroException ("word-type parameter $" ^ name))

let resolved_expression (expression : expr) : Z.t =
  match expression with
  | IntLit value -> value
  | _ -> raise (UnresolvedExpressionException expression)

let translate_constant (constant : const_encoded) : Z.t =
  match constant with
  | ConstExpr expression -> resolved_expression expression
  | Perm permission -> Encode.encode_perm (translate_perm permission)
  | SealPerm permission -> Encode.encode_seal_perm (translate_seal_perm permission)
  | Locality locality -> Encode.encode_locality (translate_locality locality)
  | Wtype word_type -> Encode.encode_wtype (translate_wtype word_type)
  | PermLoc (permission, locality) ->
      Encode.encode_perm_loc_pair (translate_perm permission) (translate_locality locality)
  | SealPermLoc (permission, locality) ->
      Encode.encode_seal_perm_loc_pair (translate_seal_perm permission)
        (translate_locality locality)
  | PairParam (name, _) -> raise (UnexpandedMacroException ("permission-pair parameter $" ^ name))

let translate_reg_or_const (value : reg_or_const) : Ast.reg_or_const =
  match value with
  | Register register -> Ast.Register (translate_regname register)
  | Const constant -> Ast.Const (translate_constant constant)
  | ValueParam name -> raise (UnexpandedMacroException ("value parameter $" ^ name))

let translate_sealable (sealable : sealable) : Ast.sealable =
  match sealable with
  | Cap (permission, locality, base, ending, address) ->
      Ast.Cap
        ( translate_perm permission,
          translate_locality locality,
          resolved_expression base,
          resolved_expression ending,
          resolved_expression address )
  | SealRange (permission, locality, base, ending, address) ->
      Ast.SealRange
        ( translate_seal_perm permission,
          translate_locality locality,
          resolved_expression base,
          resolved_expression ending,
          resolved_expression address )

let translate_word (word : word) : Ast.statement =
  match word with
  | I expression -> Ast.Word (Ast.I (resolved_expression expression))
  | Sealable sealable -> Ast.Word (Ast.Sealable (translate_sealable sealable))
  | Sentry (permission, locality, base, ending, address) ->
      Ast.Word
        (Ast.Sentry
           ( translate_perm permission,
             translate_locality locality,
             resolved_expression base,
             resolved_expression ending,
             resolved_expression address ))
  | Sealed (otype, sealable) ->
      Ast.Word (Ast.Sealed (resolved_expression otype, translate_sealable sealable))

let translate_instr (instruction : machine_op) : Ast.machine_op =
  match instruction with
  | Jalr (left, right) -> Ast.Jalr (translate_regname left, translate_regname right)
  | Jmp value -> Ast.Jmp (translate_reg_or_const value)
  | Jnz (register, value) -> Ast.Jnz (translate_regname register, translate_reg_or_const value)
  | ReadSR (register, system_register) -> Ast.ReadSR (translate_regname register, system_register)
  | WriteSR (system_register, register) -> Ast.WriteSR (system_register, translate_regname register)
  | Move (register, value) -> Ast.Move (translate_regname register, translate_reg_or_const value)
  | Load (left, right) -> Ast.Load (translate_regname left, translate_regname right)
  | Store (register, value) -> Ast.Store (translate_regname register, translate_reg_or_const value)
  | Add (register, left, right) ->
      Ast.Add (translate_regname register, translate_reg_or_const left, translate_reg_or_const right)
  | Sub (register, left, right) ->
      Ast.Sub (translate_regname register, translate_reg_or_const left, translate_reg_or_const right)
  | Mul (register, left, right) ->
      Ast.Mul (translate_regname register, translate_reg_or_const left, translate_reg_or_const right)
  | Rem (register, left, right) ->
      Ast.Rem (translate_regname register, translate_reg_or_const left, translate_reg_or_const right)
  | Div (register, left, right) ->
      Ast.Div (translate_regname register, translate_reg_or_const left, translate_reg_or_const right)
  | LAnd (register, left, right) ->
      Ast.LAnd
        (translate_regname register, translate_reg_or_const left, translate_reg_or_const right)
  | LOr (register, left, right) ->
      Ast.LOr (translate_regname register, translate_reg_or_const left, translate_reg_or_const right)
  | LShiftL (register, left, right) ->
      Ast.LShiftL
        (translate_regname register, translate_reg_or_const left, translate_reg_or_const right)
  | LShiftR (register, left, right) ->
      Ast.LShiftR
        (translate_regname register, translate_reg_or_const left, translate_reg_or_const right)
  | Lt (register, left, right) ->
      Ast.Lt (translate_regname register, translate_reg_or_const left, translate_reg_or_const right)
  | Lea (register, value) -> Ast.Lea (translate_regname register, translate_reg_or_const value)
  | Restrict (register, value) ->
      Ast.Restrict (translate_regname register, translate_reg_or_const value)
  | SubSeg (register, base, ending) ->
      Ast.SubSeg
        (translate_regname register, translate_reg_or_const base, translate_reg_or_const ending)
  | GetL (left, right) -> Ast.GetL (translate_regname left, translate_regname right)
  | GetB (left, right) -> Ast.GetB (translate_regname left, translate_regname right)
  | GetE (left, right) -> Ast.GetE (translate_regname left, translate_regname right)
  | GetA (left, right) -> Ast.GetA (translate_regname left, translate_regname right)
  | GetP (left, right) -> Ast.GetP (translate_regname left, translate_regname right)
  | GetOType (left, right) -> Ast.GetOType (translate_regname left, translate_regname right)
  | GetWType (left, right) -> Ast.GetWType (translate_regname left, translate_regname right)
  | Seal (first, second, third) ->
      Ast.Seal (translate_regname first, translate_regname second, translate_regname third)
  | UnSeal (first, second, third) ->
      Ast.UnSeal (translate_regname first, translate_regname second, translate_regname third)
  | Fail -> Ast.Fail
  | Halt -> Ast.Halt
  | Word word -> raise (WordException word)
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
  | instruction -> Ast.Op (translate_instr instruction)

let translate_prog (program : t) : Ast.t = List.map translate_statement program
