type register = string

type expression =
  | Integer of Z.t
  | Max_address
  | Stack_address
  | Add of expression * expression
  | Subtract of expression * expression

type permission = O | E | RO | RX | RW | RWX | RWL | RWLX | URW | URWL | URWX | URWLX
type locality = Global | Local | Directed
type seal_permission = { seal : bool; unseal : bool }
type word_type = Integer_type | Capability_type | Seal_range_type | Sealed_type

type constant =
  | Constant_expression of expression
  | Permission of permission
  | Seal_permission of seal_permission
  | Locality of locality
  | Word_type of word_type
  | Permission_locality of permission * locality
  | Seal_permission_locality of seal_permission * locality

type operand = Register of register | Constant of constant

type sealable =
  | Capability of permission * locality * expression * expression * expression
  | Seal_range of seal_permission * locality * expression * expression * expression

type word = Integer_word of expression | Sealable of sealable | Sealed of expression * sealable
type instruction = { opcode : string; operands : operand list }
type statement_node = Instruction of instruction | Word of word
type statement = { node : statement_node; location : Diagnostic.source_location option }
type program = statement list

type regfile_entry = {
  register : register;
  word : word;
  location : Diagnostic.source_location option;
}

type regfile = regfile_entry list

let source_location ?filename position =
  {
    Diagnostic.source = filename;
    line = max 1 position.Lexing.pos_lnum;
    column = max 1 (position.pos_cnum - position.pos_bol + 1);
    offset = Some position.pos_cnum;
  }

let start_location filename =
  Some { Diagnostic.source = filename; line = 1; column = 1; offset = Some 0 }

let diagnostic_at_lexbuf filename lexbuf message =
  let position = Lexing.lexeme_start_p lexbuf in
  Diagnostic.error ~location:(source_location ?filename position) message

let with_lexbuf ?filename source parse convert =
  let lexbuf = Lexing.from_string source in
  Option.iter (Lexing.set_filename lexbuf) filename;
  match parse lexbuf with
  | Ok value -> Ok (convert (start_location filename) value)
  | Error message -> Error [ diagnostic_at_lexbuf filename lexbuf message ]

let register_of_ir = function
  | Cerise_internal.Asm_ir.PC -> "pc"
  | Reg 0 -> "ddc"
  | Reg 31 -> "stk"
  | Reg number -> "r" ^ string_of_int number
  | RegParam name -> "$" ^ name

let expression_of_ir = function
  | Cerise_internal.Asm_ir.IntLit value -> Integer value
  | expression -> raise (Cerise_internal.Asm_ir.UnresolvedExpressionException expression)

let permission_of_ir = function
  | Cerise_internal.Asm_ir.O -> O
  | E -> E
  | RO -> RO
  | RX -> RX
  | RW -> RW
  | RWX -> RWX
  | RWL -> RWL
  | RWLX -> RWLX
  | URW -> URW
  | URWL -> URWL
  | URWX -> URWX
  | URWLX -> URWLX
  | PermParam name ->
      raise (Cerise_internal.Asm_ir.UnexpandedMacroException ("permission $" ^ name))

let locality_of_ir = function
  | Cerise_internal.Asm_ir.Global -> Global
  | Local -> Local
  | Directed -> Directed
  | LocalityParam name ->
      raise (Cerise_internal.Asm_ir.UnexpandedMacroException ("locality $" ^ name))

let seal_permission_of_ir = function
  | Cerise_internal.Asm_ir.SealPermLit (seal, unseal) -> { seal; unseal }
  | SealPermParam name ->
      raise (Cerise_internal.Asm_ir.UnexpandedMacroException ("seal permission $" ^ name))

let word_type_of_ir = function
  | Cerise_internal.Asm_ir.W_I -> Integer_type
  | W_Cap -> Capability_type
  | W_SealRange -> Seal_range_type
  | W_Sealed -> Sealed_type
  | WtypeParam name ->
      raise (Cerise_internal.Asm_ir.UnexpandedMacroException ("word type $" ^ name))

let constant_of_ir = function
  | Cerise_internal.Asm_ir.ConstExpr expression -> Constant_expression (expression_of_ir expression)
  | Perm permission -> Permission (permission_of_ir permission)
  | SealPerm permission -> Seal_permission (seal_permission_of_ir permission)
  | Locality locality -> Locality (locality_of_ir locality)
  | Wtype word_type -> Word_type (word_type_of_ir word_type)
  | PermLoc (permission, locality) ->
      Permission_locality (permission_of_ir permission, locality_of_ir locality)
  | SealPermLoc (permission, locality) ->
      Seal_permission_locality (seal_permission_of_ir permission, locality_of_ir locality)
  | PairParam (name, _) ->
      raise (Cerise_internal.Asm_ir.UnexpandedMacroException ("permission pair $" ^ name))

let operand_of_ir = function
  | Cerise_internal.Asm_ir.Register register -> Register (register_of_ir register)
  | Const constant -> Constant (constant_of_ir constant)
  | ValueParam name ->
      raise (Cerise_internal.Asm_ir.UnexpandedMacroException ("value parameter $" ^ name))

let sealable_of_ir = function
  | Cerise_internal.Asm_ir.Cap (permission, locality, base, limit, cursor) ->
      Capability
        ( permission_of_ir permission,
          locality_of_ir locality,
          expression_of_ir base,
          expression_of_ir limit,
          expression_of_ir cursor )
  | SealRange (permission, locality, base, limit, cursor) ->
      Seal_range
        ( seal_permission_of_ir permission,
          locality_of_ir locality,
          expression_of_ir base,
          expression_of_ir limit,
          expression_of_ir cursor )

let word_of_ir = function
  | Cerise_internal.Asm_ir.I expression -> Integer_word (expression_of_ir expression)
  | Sealable sealable -> Sealable (sealable_of_ir sealable)
  | Sealed (object_type, sealable) -> Sealed (expression_of_ir object_type, sealable_of_ir sealable)

let instruction opcode operands = Instruction { opcode; operands }
let reg register = Register (register_of_ir register)

let node_of_ir = function
  | Cerise_internal.Asm_ir.Jmp r -> instruction "jmp" [ reg r ]
  | Jnz (r1, r2) -> instruction "jnz" [ reg r1; reg r2 ]
  | Move (r, value) -> instruction "mov" [ reg r; operand_of_ir value ]
  | Load (r1, r2) -> instruction "load" [ reg r1; reg r2 ]
  | Store (r, value) -> instruction "store" [ reg r; operand_of_ir value ]
  | Add (r, v1, v2) -> instruction "add" [ reg r; operand_of_ir v1; operand_of_ir v2 ]
  | Sub (r, v1, v2) -> instruction "sub" [ reg r; operand_of_ir v1; operand_of_ir v2 ]
  | Mul (r, v1, v2) -> instruction "mul" [ reg r; operand_of_ir v1; operand_of_ir v2 ]
  | Rem (r, v1, v2) -> instruction "rem" [ reg r; operand_of_ir v1; operand_of_ir v2 ]
  | Div (r, v1, v2) -> instruction "div" [ reg r; operand_of_ir v1; operand_of_ir v2 ]
  | Lt (r, v1, v2) -> instruction "lt" [ reg r; operand_of_ir v1; operand_of_ir v2 ]
  | Lea (r, value) -> instruction "lea" [ reg r; operand_of_ir value ]
  | Restrict (r, value) -> instruction "restrict" [ reg r; operand_of_ir value ]
  | SubSeg (r, v1, v2) -> instruction "subseg" [ reg r; operand_of_ir v1; operand_of_ir v2 ]
  | GetL (r1, r2) -> instruction "getl" [ reg r1; reg r2 ]
  | GetB (r1, r2) -> instruction "getb" [ reg r1; reg r2 ]
  | GetE (r1, r2) -> instruction "gete" [ reg r1; reg r2 ]
  | GetA (r1, r2) -> instruction "geta" [ reg r1; reg r2 ]
  | GetP (r1, r2) -> instruction "getp" [ reg r1; reg r2 ]
  | GetOType (r1, r2) -> instruction "getotype" [ reg r1; reg r2 ]
  | GetWType (r1, r2) -> instruction "getwtype" [ reg r1; reg r2 ]
  | Seal (r1, r2, r3) -> instruction "seal" [ reg r1; reg r2; reg r3 ]
  | UnSeal (r1, r2, r3) -> instruction "unseal" [ reg r1; reg r2; reg r3 ]
  | Invoke (r1, r2) -> instruction "invoke" [ reg r1; reg r2 ]
  | LoadU (r1, r2, value) -> instruction "loadU" [ reg r1; reg r2; operand_of_ir value ]
  | StoreU (r, v1, v2) -> instruction "storeU" [ reg r; operand_of_ir v1; operand_of_ir v2 ]
  | PromoteU r -> instruction "promoteU" [ reg r ]
  | Fail -> instruction "fail" []
  | Halt -> instruction "halt" []
  | Word word -> Word (word_of_ir word)
  | Lbl name -> raise (Cerise_internal.Asm_ir.UnresolvedIrException ("label " ^ name))
  | Define (_, _, _) -> raise (Cerise_internal.Asm_ir.UnexpandedMacroException "integer definition")
  | MacroDef definition ->
      raise (Cerise_internal.Asm_ir.UnexpandedMacroException ("macro " ^ definition.name))
  | MacroCall call ->
      raise (Cerise_internal.Asm_ir.UnexpandedMacroException ("macro call " ^ call.name))

let parse_program ?filename source =
  with_lexbuf ?filename source Cerise_internal.Surface_frontend.parse_program
    (fun location program ->
      List.map (fun statement -> { node = node_of_ir statement; location }) program)

let expression_of_regfile_ir =
  let rec convert = function
    | Cerise_internal.Irreg.IntLit value -> Integer value
    | MaxAddr -> Max_address
    | StkAddr -> Stack_address
    | AddOp (left, right) -> Add (convert left, convert right)
    | SubOp (left, right) -> Subtract (convert left, convert right)
  in
  convert

let permission_of_regfile_ir = function
  | Cerise_internal.Irreg.O -> O
  | E -> E
  | RO -> RO
  | RX -> RX
  | RW -> RW
  | RWX -> RWX
  | RWL -> RWL
  | RWLX -> RWLX
  | URW -> URW
  | URWL -> URWL
  | URWX -> URWX
  | URWLX -> URWLX

let locality_of_regfile_ir = function
  | Cerise_internal.Irreg.Global -> Global
  | Local -> Local
  | Directed -> Directed

let sealable_of_regfile_ir = function
  | Cerise_internal.Irreg.WCap (permission, locality, base, limit, cursor) ->
      Capability
        ( permission_of_regfile_ir permission,
          locality_of_regfile_ir locality,
          expression_of_regfile_ir base,
          expression_of_regfile_ir limit,
          expression_of_regfile_ir cursor )
  | WSealRange ((seal, unseal), locality, base, limit, cursor) ->
      Seal_range
        ( { seal; unseal },
          locality_of_regfile_ir locality,
          expression_of_regfile_ir base,
          expression_of_regfile_ir limit,
          expression_of_regfile_ir cursor )

let word_of_regfile_ir = function
  | Cerise_internal.Irreg.WI expression -> Integer_word (expression_of_regfile_ir expression)
  | WSealable sealable -> Sealable (sealable_of_regfile_ir sealable)
  | WSealed (object_type, sealable) ->
      Sealed (expression_of_regfile_ir object_type, sealable_of_regfile_ir sealable)

let register_of_regfile_ir = function
  | Cerise_internal.Irreg.PC -> "pc"
  | DDC -> "ddc"
  | STK -> "stk"
  | Reg number -> "r" ^ string_of_int number

let regfile_of_ir location regfile =
  List.map
    (fun (register, word) ->
      { register = register_of_regfile_ir register; word = word_of_regfile_ir word; location })
    regfile

let parse_regfile ?filename source =
  with_lexbuf ?filename source Cerise_internal.Surface_frontend.parse_regfile regfile_of_ir

let rec evaluate_expression config = function
  | Integer value -> value
  | Max_address -> Runtime_config.max_addr config
  | Stack_address -> Runtime_config.stack_addr config
  | Add (left, right) -> Z.add (evaluate_expression config left) (evaluate_expression config right)
  | Subtract (left, right) ->
      Z.sub (evaluate_expression config left) (evaluate_expression config right)

let resolve_expression config expression = Integer (evaluate_expression config expression)

let resolve_sealable config = function
  | Capability (permission, locality, base, limit, cursor) ->
      Capability
        ( permission,
          locality,
          resolve_expression config base,
          resolve_expression config limit,
          resolve_expression config cursor )
  | Seal_range (permission, locality, base, limit, cursor) ->
      Seal_range
        ( permission,
          locality,
          resolve_expression config base,
          resolve_expression config limit,
          resolve_expression config cursor )

let resolve_word config = function
  | Integer_word expression -> Integer_word (resolve_expression config expression)
  | Sealable sealable -> Sealable (resolve_sealable config sealable)
  | Sealed (object_type, sealable) ->
      Sealed (resolve_expression config object_type, resolve_sealable config sealable)

let resolve_regfile config =
  List.map (fun entry -> { entry with word = resolve_word config entry.word })

let parse_word ?filename source =
  match parse_regfile ?filename ("r1 := " ^ source) with
  | Ok [ { word; _ } ] -> Ok word
  | Ok _ -> Error [ Diagnostic.error "Expected exactly one word." ]
  | Error diagnostics -> Error diagnostics
