(* Typed macro expansion over assembler IR. *)
open Asm_ir

exception Expansion_error of string

(* Format a source location for a user-facing diagnostic. *)
let location_string (location : location) : string =
  if String.equal location.filename "" then
    Printf.sprintf "line %d, column %d" location.line location.column
  else Printf.sprintf "%s:%d:%d" location.filename location.line location.column

(* Raise the private early-exit exception with a location-prefixed message. *)
let fail (location : location) (format : ('a, unit, string, 'b) format4) : 'a =
  Printf.ksprintf
    (fun message -> raise (Expansion_error (location_string location ^ ": " ^ message)))
    format

(* Return the assembler spelling of a macro parameter kind. *)
let kind_name (kind : parameter_kind) : string =
  match kind with
  | RegKind -> "reg"
  | ValueKind -> "value"
  | ExprKind -> "expr"
  | PermKind -> "perm"
  | SealPermKind -> "sealperm"
  | LocalityKind -> "locality"
  | WtypeKind -> "wtype"
  | UnknownKind kind -> kind

type slot_kind =
  | RegSlot
  | ValueSlot
  | ExprSlot
  | PermSlot
  | SealPermSlot
  | LocalitySlot
  | WtypeSlot

(* Report whether a declared parameter kind may be used in an operand slot. *)
let kind_fits_slot (kind : parameter_kind) (slot : slot_kind) : bool =
  match (kind, slot) with
  | RegKind, (RegSlot | ValueSlot)
  | ExprKind, (ExprSlot | ValueSlot)
  | PermKind, (PermSlot | ValueSlot)
  | SealPermKind, (SealPermSlot | ValueSlot)
  | LocalityKind, (LocalitySlot | ValueSlot)
  | WtypeKind, (WtypeSlot | ValueSlot)
  | ValueKind, ValueSlot ->
      true
  | _ -> false

(* Report whether an expression contains the infinite literal. *)
let rec expression_contains_inf (expression : expr) : bool =
  match expression with
  | IntLit Infinite_z.Inf -> true
  | IntLit (Infinite_z.Int _) | CurrentAddr | Symbol _ | Label _ | ExprParam _ -> false
  | AddOp (left, right) | SubOp (left, right) ->
      expression_contains_inf left || expression_contains_inf right

(* Report whether an expression still contains a macro parameter hole. *)
let rec expression_contains_parameter (expression : expr) : bool =
  match expression with
  | ExprParam _ -> true
  | IntLit _ | CurrentAddr | Symbol _ | Label _ -> false
  | AddOp (left, right) | SubOp (left, right) ->
      expression_contains_parameter left || expression_contains_parameter right

type mapper = {
  symbol_ref : string -> expr;
  label_ref : string -> expr;
  label_def : string -> string;
  reg : regname -> regname;
  expr_param : string -> expr;
  perm : perm -> perm;
  seal_perm : seal_perm -> seal_perm;
  locality : locality -> locality;
  wtype : wtype -> wtype;
  pair_param : string -> locality -> const_encoded;
  value_param : string -> reg_or_const;
}

type macro_table = (string, macro_definition) Hashtbl.t
type integer_definition = expr * location
type definition_table = (string, integer_definition) Hashtbl.t
type resolved_definition_table = (string, expr) Hashtbl.t
type parameter_table = (string, parameter_kind) Hashtbl.t
type binding_table = (string, reg_or_const) Hashtbl.t
type label_table = (string, unit) Hashtbl.t
type rename_table = (string, string) Hashtbl.t

(* These initial capacities are small, approximate performance hints rather than limits. They are
   deliberately conservative, and OCaml hash tables grow automatically when more entries are added. *)
let initial_macro_capacity : int = 16
let initial_definition_capacity : int = 16
let initial_label_capacity : int = 32
let initial_private_label_capacity : int = 8

(* Structurally rewrite every symbolic component of an expression. *)
let rec map_expr (mapper : mapper) (expression : expr) : expr =
  match expression with
  | (IntLit _ | CurrentAddr) as expression -> expression
  | Symbol name -> mapper.symbol_ref name
  | Label name -> mapper.label_ref name
  | AddOp (left, right) -> AddOp (map_expr mapper left, map_expr mapper right)
  | SubOp (left, right) -> SubOp (map_expr mapper left, map_expr mapper right)
  | ExprParam name -> mapper.expr_param name

(* Structurally rewrite an encoded constant. *)
let map_const (mapper : mapper) (constant : const_encoded) : const_encoded =
  match constant with
  | ConstExpr expression -> ConstExpr (map_expr mapper expression)
  | Perm permission -> Perm (mapper.perm permission)
  | SealPerm permission -> SealPerm (mapper.seal_perm permission)
  | Locality locality -> Locality (mapper.locality locality)
  | Wtype word_type -> Wtype (mapper.wtype word_type)
  | PermLoc (permission, locality) -> PermLoc (mapper.perm permission, mapper.locality locality)
  | SealPermLoc (permission, locality) ->
      SealPermLoc (mapper.seal_perm permission, mapper.locality locality)
  | PairParam (name, locality) -> mapper.pair_param name (mapper.locality locality)

(* Structurally rewrite an instruction value. *)
let map_value (mapper : mapper) (value : reg_or_const) : reg_or_const =
  match value with
  | Register register -> Register (mapper.reg register)
  | Const constant -> Const (map_const mapper constant)
  | ValueParam name -> mapper.value_param name

(* Structurally rewrite all fields of a capability or sealing range. *)
let map_sealable (mapper : mapper) (sealable : sealable) : sealable =
  match sealable with
  | Cap (permission, locality, base, ending, address) ->
      Cap
        ( mapper.perm permission,
          mapper.locality locality,
          map_expr mapper base,
          map_expr mapper ending,
          map_expr mapper address )
  | SealRange (permission, locality, base, ending, address) ->
      SealRange
        ( mapper.seal_perm permission,
          mapper.locality locality,
          map_expr mapper base,
          map_expr mapper ending,
          map_expr mapper address )

(* Structurally rewrite a literal machine word. *)
let map_word (mapper : mapper) (word : word) : word =
  match word with
  | I expression -> I (map_expr mapper expression)
  | Sealable sealable -> Sealable (map_sealable mapper sealable)
  | Sealed (otype, sealable) -> Sealed (map_expr mapper otype, map_sealable mapper sealable)

(* Structurally rewrite every operand of one assembler operation. *)
let map_op (mapper : mapper) (operation : machine_op) : machine_op =
  match operation with
  | Jmp register -> Jmp (mapper.reg register)
  | Jnz (left, right) -> Jnz (mapper.reg left, mapper.reg right)
  | Move (register, value) -> Move (mapper.reg register, map_value mapper value)
  | Load (left, right) -> Load (mapper.reg left, mapper.reg right)
  | Store (register, value) -> Store (mapper.reg register, map_value mapper value)
  | Add (register, left, right) ->
      Add (mapper.reg register, map_value mapper left, map_value mapper right)
  | Sub (register, left, right) ->
      Sub (mapper.reg register, map_value mapper left, map_value mapper right)
  | Mul (register, left, right) ->
      Mul (mapper.reg register, map_value mapper left, map_value mapper right)
  | Rem (register, left, right) ->
      Rem (mapper.reg register, map_value mapper left, map_value mapper right)
  | Div (register, left, right) ->
      Div (mapper.reg register, map_value mapper left, map_value mapper right)
  | Lt (register, left, right) ->
      Lt (mapper.reg register, map_value mapper left, map_value mapper right)
  | Lea (register, value) -> Lea (mapper.reg register, map_value mapper value)
  | Restrict (register, value) -> Restrict (mapper.reg register, map_value mapper value)
  | SubSeg (register, base, ending) ->
      SubSeg (mapper.reg register, map_value mapper base, map_value mapper ending)
  | GetL (left, right) -> GetL (mapper.reg left, mapper.reg right)
  | GetB (left, right) -> GetB (mapper.reg left, mapper.reg right)
  | GetE (left, right) -> GetE (mapper.reg left, mapper.reg right)
  | GetA (left, right) -> GetA (mapper.reg left, mapper.reg right)
  | GetP (left, right) -> GetP (mapper.reg left, mapper.reg right)
  | GetOType (left, right) -> GetOType (mapper.reg left, mapper.reg right)
  | GetWType (left, right) -> GetWType (mapper.reg left, mapper.reg right)
  | Seal (first, second, third) -> Seal (mapper.reg first, mapper.reg second, mapper.reg third)
  | UnSeal (first, second, third) -> UnSeal (mapper.reg first, mapper.reg second, mapper.reg third)
  | Invoke (left, right) -> Invoke (mapper.reg left, mapper.reg right)
  | LoadU (first, second, value) ->
      LoadU (mapper.reg first, mapper.reg second, map_value mapper value)
  | StoreU (register, first, second) ->
      StoreU (mapper.reg register, map_value mapper first, map_value mapper second)
  | PromoteU register -> PromoteU (mapper.reg register)
  | Fail -> Fail
  | Halt -> Halt
  | Lbl name -> Lbl (mapper.label_def name)
  | Word word -> Word (map_word mapper word)
  | Define _ as definition -> definition
  | MacroDef _ as definition -> definition
  | MacroCall _ as call -> call

(* A mapper that preserves the complete assembler IR unchanged. *)
let identity_mapper : mapper =
  {
    symbol_ref = (fun name -> Symbol name);
    label_ref = (fun name -> Label name);
    label_def = Fun.id;
    reg = Fun.id;
    expr_param = (fun name -> ExprParam name);
    perm = Fun.id;
    seal_perm = Fun.id;
    locality = Fun.id;
    wtype = Fun.id;
    pair_param = (fun name locality -> PairParam (name, locality));
    value_param = (fun name -> ValueParam name);
  }

type validation_context = { definition : macro_definition; parameters : parameter_table }

(* Add one formal parameter to the validation table, rejecting invalid declarations. *)
let add_parameter (definition : macro_definition) (parameters : parameter_table)
    (parameter : parameter) : unit =
  (match parameter.kind with
  | UnknownKind kind ->
      fail definition.location
        "macro declaration error: unknown parameter type %S; expected reg, value, expr, perm, \
         sealperm, locality, or wtype"
        kind
  | _ -> ());
  if Hashtbl.mem parameters parameter.name then
    fail definition.location "macro declaration error: duplicate parameter %S" parameter.name;
  Hashtbl.add parameters parameter.name parameter.kind

(* Build and return the validated name-to-kind table for a macro declaration. *)
let collect_parameters (definition : macro_definition) : parameter_table =
  let parameters = Hashtbl.create (List.length definition.parameters) in
  List.iter (add_parameter definition parameters) definition.parameters;
  parameters

(* Find a declared parameter kind, reporting an unknown body reference. *)
let parameter_kind_of_name (context : validation_context) (name : string) : parameter_kind =
  match Hashtbl.find_opt context.parameters name with
  | Some kind -> kind
  | None ->
      fail context.definition.location "macro declaration error: unknown parameter $%s in macro %S"
        name context.definition.name

(* Check one parameter occurrence against its structural operand slot. *)
let validate_parameter_use (context : validation_context) (name : string) (slot : slot_kind) : unit
    =
  let kind = parameter_kind_of_name context name in
  if not (kind_fits_slot kind slot) then
    fail context.definition.location
      "macro declaration error: parameter $%s has type %s, which is invalid in this operand \
       position"
      name (kind_name kind)

(* Check a parameter used as the permission half of a paired constant. *)
let validate_pair_parameter (context : validation_context) (name : string) : unit =
  match parameter_kind_of_name context name with
  | PermKind | SealPermKind -> ()
  | kind ->
      fail context.definition.location
        "macro declaration error: parameter $%s has type %s, which is invalid as a paired \
         permission"
        name (kind_name kind)

(* Validate parameter holes contained in an integer expression. *)
let rec validate_expression (context : validation_context) (expression : expr) : unit =
  match expression with
  | IntLit _ | CurrentAddr | Symbol _ | Label _ -> ()
  | AddOp (left, right) | SubOp (left, right) ->
      validate_expression context left;
      validate_expression context right
  | ExprParam name -> validate_parameter_use context name ExprSlot

(* Validate a register-position parameter hole. *)
let validate_register (context : validation_context) (register : regname) : unit =
  match register with
  | RegParam name -> validate_parameter_use context name RegSlot
  | PC | Reg _ -> ()

(* Validate a capability-permission parameter hole. *)
let validate_permission (context : validation_context) (permission : perm) : unit =
  match permission with PermParam name -> validate_parameter_use context name PermSlot | _ -> ()

(* Validate a sealing-permission parameter hole. *)
let validate_seal_permission (context : validation_context) (permission : seal_perm) : unit =
  match permission with
  | SealPermParam name -> validate_parameter_use context name SealPermSlot
  | SealPermLit _ -> ()

(* Validate a locality parameter hole. *)
let validate_locality (context : validation_context) (locality : locality) : unit =
  match locality with
  | LocalityParam name -> validate_parameter_use context name LocalitySlot
  | _ -> ()

(* Validate a word-type parameter hole. *)
let validate_wtype (context : validation_context) (word_type : wtype) : unit =
  match word_type with WtypeParam name -> validate_parameter_use context name WtypeSlot | _ -> ()

(* Validate every parameter hole inside an encoded constant. *)
let validate_constant (context : validation_context) (constant : const_encoded) : unit =
  match constant with
  | ConstExpr expression -> validate_expression context expression
  | Perm permission -> validate_permission context permission
  | SealPerm permission -> validate_seal_permission context permission
  | Locality locality -> validate_locality context locality
  | Wtype word_type -> validate_wtype context word_type
  | PermLoc (permission, locality) ->
      validate_permission context permission;
      validate_locality context locality
  | SealPermLoc (permission, locality) ->
      validate_seal_permission context permission;
      validate_locality context locality
  | PairParam (name, locality) ->
      validate_pair_parameter context name;
      validate_locality context locality

(* Validate every parameter hole inside a general instruction value. *)
let validate_value (context : validation_context) (value : reg_or_const) : unit =
  match value with
  | Register register -> validate_register context register
  | Const constant -> validate_constant context constant
  | ValueParam name -> validate_parameter_use context name ValueSlot

(* Validate every parameter hole inside a capability or sealing range. *)
let validate_sealable (context : validation_context) (sealable : sealable) : unit =
  match sealable with
  | Cap (permission, locality, base, ending, address) ->
      validate_permission context permission;
      validate_locality context locality;
      validate_expression context base;
      validate_expression context ending;
      validate_expression context address
  | SealRange (permission, locality, base, ending, address) ->
      validate_seal_permission context permission;
      validate_locality context locality;
      validate_expression context base;
      validate_expression context ending;
      validate_expression context address

(* Validate every parameter hole inside a literal word. *)
let validate_word (context : validation_context) (word : word) : unit =
  match word with
  | I expression -> validate_expression context expression
  | Sealable sealable -> validate_sealable context sealable
  | Sealed (otype, sealable) ->
      validate_expression context otype;
      validate_sealable context sealable

(* Validate an operation whose operands are two registers. *)
let validate_two_registers (context : validation_context) (left : regname) (right : regname) : unit
    =
  validate_register context left;
  validate_register context right

(* Validate an operation whose operands are one register and one general value. *)
let validate_register_value (context : validation_context) (register : regname)
    (value : reg_or_const) : unit =
  validate_register context register;
  validate_value context value

(* Validate an operation whose operands are one register and two general values. *)
let validate_register_two_values (context : validation_context) (register : regname)
    (left : reg_or_const) (right : reg_or_const) : unit =
  validate_register context register;
  validate_value context left;
  validate_value context right

(* Validate an operation whose operands are three registers. *)
let validate_three_registers (context : validation_context) (first : regname) (second : regname)
    (third : regname) : unit =
  validate_register context first;
  validate_register context second;
  validate_register context third

(* Validate an operation whose operands are two registers and one general value. *)
let validate_two_registers_value (context : validation_context) (first : regname) (second : regname)
    (value : reg_or_const) : unit =
  validate_register context first;
  validate_register context second;
  validate_value context value

(* Validate the operands and allowed nesting of one macro-body operation. *)
let validate_operation (context : validation_context) (operation : machine_op) : unit =
  let definition = context.definition in
  match operation with
  | Jmp register | PromoteU register -> validate_register context register
  | Jnz (left, right)
  | Load (left, right)
  | GetL (left, right)
  | GetB (left, right)
  | GetE (left, right)
  | GetA (left, right)
  | GetP (left, right)
  | GetOType (left, right)
  | GetWType (left, right)
  | Invoke (left, right) ->
      validate_two_registers context left right
  | Move (register, source)
  | Store (register, source)
  | Lea (register, source)
  | Restrict (register, source) ->
      validate_register_value context register source
  | Add (register, left, right)
  | Sub (register, left, right)
  | Mul (register, left, right)
  | Rem (register, left, right)
  | Div (register, left, right)
  | Lt (register, left, right)
  | SubSeg (register, left, right)
  | StoreU (register, left, right) ->
      validate_register_two_values context register left right
  | Seal (first, second, third) | UnSeal (first, second, third) ->
      validate_three_registers context first second third
  | LoadU (first, second, source) -> validate_two_registers_value context first second source
  | Word contents -> validate_word context contents
  | Lbl _ | Fail | Halt -> ()
  | Define (_, _, location) ->
      fail location "macro declaration error: declarations are not allowed inside macro %S"
        definition.name
  | MacroDef nested ->
      fail nested.location "macro declaration error: declarations are not allowed inside macro %S"
        definition.name
  | MacroCall call ->
      fail call.location "macro declaration error: calls are not allowed inside macro %S"
        definition.name

(* Validate a complete macro declaration and return no value on success. *)
let validate_macro (definition : macro_definition) : unit =
  let parameters = collect_parameters definition in
  let context = { definition; parameters } in
  List.iter (validate_operation context) definition.body

(* Classify a concrete macro-call argument, or return None for an unresolved value. *)
let argument_kind (argument : reg_or_const) : parameter_kind option =
  match argument with
  | Register (PC | Reg _) -> Some RegKind
  | Const (ConstExpr expression) when not (expression_contains_parameter expression) ->
      Some ExprKind
  | Const (Perm (O | E | RO | RX | RW | RWX | RWL | RWLX | URW | URWL | URWX | URWLX)) ->
      Some PermKind
  | Const (SealPerm (SealPermLit _)) -> Some SealPermKind
  | Const (Locality (Global | Local | Directed)) -> Some LocalityKind
  | Const (Wtype (W_I | W_Cap | W_SealRange | W_Sealed)) -> Some WtypeKind
  | Const (PermLoc _) | Const (SealPermLoc _) -> Some ValueKind
  | Const (PairParam _) -> None
  | Register (RegParam _)
  | ValueParam _
  | Const (ConstExpr _)
  | Const (Perm (PermParam _))
  | Const (SealPerm (SealPermParam _))
  | Const (Locality (LocalityParam _))
  | Const (Wtype (WtypeParam _)) ->
      None

(* Report whether a concrete call argument satisfies a formal parameter kind. *)
let argument_fits (kind : parameter_kind) (argument : reg_or_const) : bool =
  match (kind, argument_kind argument) with
  | ValueKind, Some _ -> true
  | expected, Some actual -> expected = actual
  | _, None -> false

(* Validate and add one sequence-macro declaration to its name table. *)
let add_macro_declaration (macros : macro_table) (definition : macro_definition) : unit =
  if Hashtbl.mem macros definition.name then
    fail definition.location "macro declaration error: duplicate macro %S" definition.name;
  validate_macro definition;
  Hashtbl.add macros definition.name definition

(* Validate and add one integer definition to its name table. *)
let add_integer_definition (definitions : definition_table) (name : string) (expression : expr)
    (location : location) : unit =
  if Hashtbl.mem definitions name then
    fail location "integer definition error: duplicate definition %S" name;
  if expression_contains_inf expression then
    fail location
      "integer definition error: %S must evaluate to a finite integer; `Inf` is not allowed" name;
  if expression_contains_parameter expression then
    fail location "integer definition error: %S cannot contain a macro parameter" name;
  Hashtbl.add definitions name (expression, location)

(* Collect one parsed operation when it is a declaration; ignore ordinary operations. *)
let collect_declaration (macros : macro_table) (definitions : definition_table)
    (operation : machine_op) : unit =
  match operation with
  | MacroDef definition -> add_macro_declaration macros definition
  | Define (name, expression, location) ->
      add_integer_definition definitions name expression location
  | _ -> ()

(* Return file-wide macro and integer-definition tables for the parsed program. *)
let collect_declarations (program : t) : macro_table * definition_table =
  let macros = Hashtbl.create initial_macro_capacity in
  let definitions = Hashtbl.create initial_definition_capacity in
  List.iter (collect_declaration macros definitions) program;
  (macros, definitions)

type definition_resolver = { definitions : definition_table; resolved : resolved_definition_table }

(* Resolve one named integer definition, memoizing and returning its expression. *)
let rec resolve_definition (resolver : definition_resolver) (stack : string list) (name : string) :
    expr =
  match Hashtbl.find_opt resolver.resolved name with
  | Some expression -> expression
  | None ->
      let expression, location = Hashtbl.find resolver.definitions name in
      if List.mem name stack then
        fail location "integer definition error: cyclic definition involving %S" name;
      let expression = resolve_definition_expression resolver (name :: stack) expression in
      Hashtbl.add resolver.resolved name expression;
      expression

(* Resolve symbols recursively inside one integer-definition expression. *)
and resolve_definition_expression (resolver : definition_resolver) (stack : string list)
    (expression : expr) : expr =
  match expression with
  | IntLit _ | CurrentAddr -> expression
  | Symbol name ->
      if Hashtbl.mem resolver.definitions name then resolve_definition resolver stack name
      else Label name
  | Label _ -> expression
  | AddOp (left, right) ->
      AddOp
        ( resolve_definition_expression resolver stack left,
          resolve_definition_expression resolver stack right )
  | SubOp (left, right) ->
      SubOp
        ( resolve_definition_expression resolver stack left,
          resolve_definition_expression resolver stack right )
  | ExprParam name -> raise (Expansion_error ("integer definition contains parameter $" ^ name))

(* Force resolution of one table entry; used as a named Hashtbl.iter callback. *)
let resolve_definition_entry (resolver : definition_resolver) (name : string)
    (_definition : integer_definition) : unit =
  ignore (resolve_definition resolver [] name)

(* Return a memoized table in which definition dependencies have all been resolved. *)
let resolve_definitions (definitions : definition_table) : resolved_definition_table =
  let resolver = { definitions; resolved = Hashtbl.create (Hashtbl.length definitions) } in
  Hashtbl.iter (resolve_definition_entry resolver) definitions;
  resolver.resolved

(* Record a top-level label in the set of all source label names. *)
let collect_top_level_label (all_labels : label_table) (operation : machine_op) : unit =
  match operation with Lbl name -> Hashtbl.replace all_labels name () | _ -> ()

(* Record one private label, rejecting duplicates within the same macro body. *)
let collect_private_label (definition : macro_definition) (private_labels : label_table)
    (all_labels : label_table) (operation : machine_op) : unit =
  match operation with
  | Lbl name ->
      if Hashtbl.mem private_labels name then
        fail definition.location "macro declaration error: duplicate private label %S in %S" name
          definition.name;
      Hashtbl.add private_labels name ();
      Hashtbl.replace all_labels name ()
  | _ -> ()

(* Record all private labels belonging to one macro declaration. *)
let collect_macro_labels (all_labels : label_table) (_name : string) (definition : macro_definition)
    : unit =
  let private_labels = Hashtbl.create initial_private_label_capacity in
  List.iter (collect_private_label definition private_labels all_labels) definition.body

(* Reject an integer definition whose name is also used by any label. *)
let check_definition_label_collision (all_labels : label_table) (name : string)
    ((_expression, location) : integer_definition) : unit =
  if Hashtbl.mem all_labels name then
    fail location "integer definition error: %S conflicts with a label of the same name" name

(* Return all global/private label names after validating their shared definition namespace. *)
let collect_labels (program : t) (macros : macro_table) (definitions : definition_table) :
    label_table =
  let all_labels = Hashtbl.create initial_label_capacity in
  List.iter (collect_top_level_label all_labels) program;
  Hashtbl.iter (collect_macro_labels all_labels) macros;
  Hashtbl.iter (check_definition_label_collision all_labels) definitions;
  all_labels

(* Resolve an unresolved symbol as an integer definition or a definite label. *)
let resolve_symbol (definitions : resolved_definition_table) (name : string) : expr =
  match Hashtbl.find_opt definitions name with Some expression -> expression | None -> Label name

(* Raise a defensive error for a parameter that survived substitution. *)
let unbound_parameter (category : string) (name : string) : 'a =
  raise (Expansion_error (Printf.sprintf "unbound %s parameter $%s" category name))

(* Preserve a concrete register and reject an unresolved register parameter. *)
let resolve_register (register : regname) : regname =
  match register with RegParam name -> unbound_parameter "register" name | _ -> register

(* Preserve a concrete permission and reject an unresolved permission parameter. *)
let resolve_permission (permission : perm) : perm =
  match permission with PermParam name -> unbound_parameter "permission" name | _ -> permission

(* Preserve a concrete sealing permission and reject an unresolved parameter. *)
let resolve_seal_permission (permission : seal_perm) : seal_perm =
  match permission with
  | SealPermParam name -> unbound_parameter "sealing-permission" name
  | _ -> permission

(* Preserve a concrete locality and reject an unresolved locality parameter. *)
let resolve_locality (locality : locality) : locality =
  match locality with LocalityParam name -> unbound_parameter "locality" name | _ -> locality

(* Preserve a concrete word type and reject an unresolved word-type parameter. *)
let resolve_wtype (word_type : wtype) : wtype =
  match word_type with WtypeParam name -> unbound_parameter "word-type" name | _ -> word_type

(* Return a mapper that resolves symbols and enforces the no-parameter invariant. *)
let make_resolve_mapper (resolved_definitions : resolved_definition_table) : mapper =
  {
    identity_mapper with
    symbol_ref = resolve_symbol resolved_definitions;
    reg = resolve_register;
    expr_param = unbound_parameter "expression";
    perm = resolve_permission;
    seal_perm = resolve_seal_permission;
    locality = resolve_locality;
    wtype = resolve_wtype;
    pair_param = (fun name _locality -> unbound_parameter "permission-pair" name);
    value_param = unbound_parameter "value";
  }

type expansion_context = {
  macros : macro_table;
  all_labels : label_table;
  resolve_mapper : mapper;
  mutable invocation : int;
}

(* Construct one deterministic candidate for a hygienic private label. *)
let label_candidate (context : expansion_context) (macro_name : string) (label : string)
    (suffix : int) : string =
  Printf.sprintf "__macro_%d_%s_%s_%d" context.invocation macro_name label suffix

(* Find, reserve, and return a generated label not already present in the program. *)
let rec choose_fresh_label (context : expansion_context) (macro_name : string) (label : string)
    (suffix : int) : string =
  let candidate = label_candidate context macro_name label suffix in
  if Hashtbl.mem context.all_labels candidate then
    choose_fresh_label context macro_name label (suffix + 1)
  else (
    Hashtbl.add context.all_labels candidate ();
    candidate)

(* Return a fresh hygienic name for one macro-private label. *)
let fresh_label (context : expansion_context) (macro_name : string) (label : string) : string =
  choose_fresh_label context macro_name label 0

(* Check that a call supplies exactly one actual argument per formal parameter. *)
let check_arity (definition : macro_definition) (call : macro_call) : unit =
  if List.length call.arguments <> List.length definition.parameters then
    fail call.location "macro call error: %S expects %d argument(s), but received %d" call.name
      (List.length definition.parameters)
      (List.length call.arguments)

(* Type-check and add one formal-to-actual binding to a call's binding table. *)
let bind_argument (call : macro_call) (bindings : binding_table) (parameter : parameter)
    (argument : reg_or_const) : unit =
  if not (argument_fits parameter.kind argument) then
    fail call.location "macro call error: argument for $%s is not a %s" parameter.name
      (kind_name parameter.kind);
  Hashtbl.add bindings parameter.name argument

(* Resolve, type-check, and return all formal-to-actual bindings for one call. *)
let bind_arguments (context : expansion_context) (definition : macro_definition) (call : macro_call)
    : binding_table =
  check_arity definition call;
  let arguments = List.map (map_value context.resolve_mapper) call.arguments in
  let bindings = Hashtbl.create (List.length arguments) in
  List.iter2 (bind_argument call bindings) definition.parameters arguments;
  bindings

type substitution_context = {
  definition : macro_definition;
  call : macro_call;
  bindings : binding_table;
}

(* Return the actual value bound to a formal parameter. *)
let lookup_binding (context : substitution_context) (name : string) : reg_or_const =
  match Hashtbl.find_opt context.bindings name with
  | Some argument -> argument
  | None -> fail context.definition.location "macro declaration error: unknown parameter $%s" name

(* Substitute a register hole and preserve concrete registers. *)
let substitute_register (context : substitution_context) (register : regname) : regname =
  match register with
  | RegParam name -> (
      match lookup_binding context name with
      | Register register -> register
      | _ -> fail context.call.location "macro call error: $%s is not a register" name)
  | _ -> register

(* Substitute and return the expression bound to an expression hole. *)
let substitute_expression (context : substitution_context) (name : string) : expr =
  match lookup_binding context name with
  | Const (ConstExpr expression) -> expression
  | _ -> fail context.call.location "macro call error: $%s is not an expression" name

(* Substitute a capability-permission hole and preserve concrete permissions. *)
let substitute_permission (context : substitution_context) (permission : perm) : perm =
  match permission with
  | PermParam name -> (
      match lookup_binding context name with
      | Const (Perm permission) -> permission
      | _ -> fail context.call.location "macro call error: $%s is not a permission" name)
  | _ -> permission

(* Substitute a sealing-permission hole and preserve concrete permissions. *)
let substitute_seal_permission (context : substitution_context) (permission : seal_perm) : seal_perm
    =
  match permission with
  | SealPermParam name -> (
      match lookup_binding context name with
      | Const (SealPerm permission) -> permission
      | _ -> fail context.call.location "macro call error: $%s is not a sealing permission" name)
  | _ -> permission

(* Substitute a locality hole and preserve concrete localities. *)
let substitute_locality (context : substitution_context) (locality : locality) : locality =
  match locality with
  | LocalityParam name -> (
      match lookup_binding context name with
      | Const (Locality locality) -> locality
      | _ -> fail context.call.location "macro call error: $%s is not a locality" name)
  | _ -> locality

(* Substitute a word-type hole and preserve concrete word types. *)
let substitute_wtype (context : substitution_context) (word_type : wtype) : wtype =
  match word_type with
  | WtypeParam name -> (
      match lookup_binding context name with
      | Const (Wtype word_type) -> word_type
      | _ -> fail context.call.location "macro call error: $%s is not a word type" name)
  | _ -> word_type

(* Resolve an ambiguous permission/locality pair from the bound permission category. *)
let substitute_pair (context : substitution_context) (name : string) (locality : locality) :
    const_encoded =
  match lookup_binding context name with
  | Const (Perm permission) -> PermLoc (permission, locality)
  | Const (SealPerm permission) -> SealPermLoc (permission, locality)
  | _ -> fail context.call.location "macro call error: $%s is not a permission" name

(* Return a mapper that substitutes every typed parameter hole from one call. *)
let make_substitute_mapper (definition : macro_definition) (call : macro_call)
    (bindings : binding_table) : mapper =
  let context = { definition; call; bindings } in
  {
    identity_mapper with
    reg = substitute_register context;
    expr_param = substitute_expression context;
    perm = substitute_permission context;
    seal_perm = substitute_seal_permission context;
    locality = substitute_locality context;
    wtype = substitute_wtype context;
    pair_param = substitute_pair context;
    value_param = lookup_binding context;
  }

(* Add one macro-private label and its fresh name to an invocation's rename table. *)
let add_private_label_rename (context : expansion_context) (definition : macro_definition)
    (labels : rename_table) (operation : machine_op) : unit =
  match operation with
  | Lbl name -> Hashtbl.add labels name (fresh_label context definition.name name)
  | _ -> ()

(* Rename a reference when it names a private label, otherwise use the supplied constructor. *)
let rename_reference (labels : rename_table) (constructor : string -> expr) (name : string) : expr =
  match Hashtbl.find_opt labels name with Some renamed -> Label renamed | None -> constructor name

(* Rename a private label definition and preserve a non-private definition. *)
let rename_label_definition (labels : rename_table) (name : string) : string =
  match Hashtbl.find_opt labels name with Some renamed -> renamed | None -> name

(* Return a mapper that makes one invocation's private labels hygienic. *)
let make_rename_mapper (context : expansion_context) (definition : macro_definition) : mapper =
  context.invocation <- context.invocation + 1;
  let labels = Hashtbl.create initial_private_label_capacity in
  List.iter (add_private_label_rename context definition labels) definition.body;
  {
    identity_mapper with
    symbol_ref = rename_reference labels (fun name -> Symbol name);
    label_ref = rename_reference labels (fun name -> Label name);
    label_def = rename_label_definition labels;
  }

(* Return the macro declaration named by a call, or report an unknown macro. *)
let find_macro (context : expansion_context) (call : macro_call) : macro_definition =
  match Hashtbl.find_opt context.macros call.name with
  | Some definition -> definition
  | None -> fail call.location "macro call error: unknown macro %S" call.name

(* Apply all three call-specific rewrites to one macro-body statement. *)
let rewrite_body_statement (context : expansion_context) (rename_mapper : mapper)
    (substitute_mapper : mapper) (statement : machine_op) : machine_op =
  statement |> map_op rename_mapper |> map_op substitute_mapper |> map_op context.resolve_mapper

(* Expand one macro call and return the list of ordinary operations replacing it. *)
let expand_call (context : expansion_context) (call : macro_call) : machine_op list =
  let definition = find_macro context call in
  let bindings = bind_arguments context definition call in
  let rename_mapper = make_rename_mapper context definition in
  let substitute_mapper = make_substitute_mapper definition call bindings in
  List.map (rewrite_body_statement context rename_mapper substitute_mapper) definition.body

(* Expand or resolve one top-level operation and return zero, one, or many output operations. *)
let expand_operation (context : expansion_context) (operation : machine_op) : machine_op list =
  match operation with
  | Define _ | MacroDef _ -> []
  | MacroCall call -> expand_call context call
  | operation -> [ map_op context.resolve_mapper operation ]

(* Return the declaration-free, parameter-free, name-resolved assembler IR. *)
let expand_program (context : expansion_context) (program : t) : t =
  List.concat_map (expand_operation context) program

(* Run every expansion phase and return either macro-free IR or a user-facing error. *)
let expand (program : t) : (t, string) result =
  try
    let macros, definitions = collect_declarations program in
    let resolved_definitions = resolve_definitions definitions in
    let all_labels = collect_labels program macros definitions in
    let resolve_mapper = make_resolve_mapper resolved_definitions in
    let context = { macros; all_labels; resolve_mapper; invocation = 0 } in
    Ok (expand_program context program)
  with Expansion_error message -> Error message
