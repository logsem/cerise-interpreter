%{
open Assembly_construction

let at (position : Lexing.position) : Diagnostic.source_location =
  source_location_of_lexing_position position

let item (position : Lexing.position)
    (node : ('statement, 'word, 'argument, 'kind) Assembly_construction.item_node) :
    ('statement, 'word, 'argument, 'kind) Assembly_construction.item =
  { node; location = at position }

let parameter (position : Lexing.position) (name : string) (kind : string) :
    Asm_ir.parameter_kind Assembly_construction.parameter =
  if Asm_ir.valid_parameter_kind kind then { name; kind = Asm_ir.parameter_kind kind }
  else
    raise
      (Parse_error
         (at position, Printf.sprintf "Unsupported macro parameter kind %S." kind))
%}

%nonassoc OPERAND_END
/* Griotte's E-[...] sentry form must win over expression subtraction. */
%nonassoc PERMISSION
%nonassoc MINUS

%start <Asm_ir.source_program> program
%start <Asm_ir.regfile> regfile
%start <Asm_ir.word> word

%%

program:
  | items = list(source_item); EOF { items }

regfile:
  | entries = list(regfile_entry); EOF { entries }

word:
  | value = raw_word; EOF { value }

source_item:
  | value = statement
      { item $startpos (Statement value) }
  | HASH; value = raw_word
      { item $startpos (Raw_word value) }
  | name = declared_name; COLON
      { item $startpos (Label name) }
  | DEFINE; name = declared_name; value = expression
      { item $startpos (Definition (name, value)) }
  | CALL; LPAREN; arguments = separated_list(COMMA, macro_argument); RPAREN
      { item $startpos (Macro_call ($1, arguments)) }
  | definition = macro_definition
      { item $startpos (Macro_definition definition) }

macro_definition:
  | MACRO; name = declared_name; LPAREN;
      parameters = separated_list(COMMA, parameter_declaration); RPAREN;
      body = list(macro_item); ENDMACRO
      {
        {
          name;
          parameters;
          body;
          declaration_location = at $startpos;
        }
      }

macro_item:
  | value = statement
      { item $startpos (Statement value) }
  | HASH; value = raw_word
      { item $startpos (Raw_word value) }
  | name = declared_name; COLON
      { item $startpos (Label name) }
  | DEFINE; name = declared_name; value = expression
      { item $startpos (Definition (name, value)) }
  | CALL; LPAREN; arguments = separated_list(COMMA, macro_argument); RPAREN
      { item $startpos (Macro_call ($1, arguments)) }

parameter_declaration:
  | name = declared_name; COLON; kind = IDENT { parameter $startpos(kind) name kind }

declared_name:
  | name = IDENT { name }
  | name = REGISTER { name }
  | name = PERMISSION { name }
  | name = SEAL_PERMISSION { name }
  | name = LOCALITY { name }
  | name = WORD_TYPE { name }
  | name = JMP { name } | name = JNZ { name } | name = MOVE { name }
  | name = LOAD { name } | name = STORE { name } | name = ADD { name }
  | name = SUB { name } | name = MUL { name } | name = REM { name }
  | name = DIV { name } | name = LT { name } | name = LEA { name }
  | name = RESTRICT { name } | name = SUBSEG { name } | name = GETL { name }
  | name = GETB { name } | name = GETE { name } | name = GETA { name }
  | name = GETP { name } | name = GETOTYPE { name } | name = GETWTYPE { name }
  | name = SEAL { name } | name = UNSEAL { name } | name = INVOKE { name }
  | name = FAIL { name } | name = HALT { name } | name = LOADU { name }
  | name = STOREU { name } | name = ISPTR { name } | name = PROMOTEU { name }
  | name = EINIT { name } | name = EDEINIT { name }
  | name = ESTOREID { name } | name = ISUNIQUE { name }
  | name = JALR { name } | name = READSR { name } | name = WRITESR { name }
  | name = LAND { name } | name = LOR { name } | name = LSHIFTL { name }
  | name = LSHIFTR { name } | name = SYSTEM_REGISTER { name }
  | name = GRIOTTE_PERMISSION { name }

%public expression:
  | value = expression_primary { value }
  | left = expression; PLUS; right = expression_primary { Expression.Add (left, right) }
  | left = expression; MINUS; right = expression_primary { Expression.Subtract (left, right) }
  | left = expression; STAR; right = expression_primary { Expression.Multiply (left, right) }
  | left = expression; LOGAND; right = expression_primary { Expression.Logand (left, right) }
  | left = expression; LOGOR; right = expression_primary { Expression.Logor (left, right) }
  | left = expression; SHIFT_LEFT; right = expression_primary { Expression.Shift_left (left, right) }
  | left = expression; SHIFT_RIGHT; right = expression_primary { Expression.Shift_right (left, right) }

expression_primary:
  | value = INTEGER { Expression.Integer value }
  | CURRENT_ADDRESS { Expression.Current_address }
  | MAX_ADDRESS { Expression.Max_address }
  | STACK_ADDRESS { Expression.Stack_address }
  | name = expression_name { Expression.Symbol name }
  | name = PARAMETER { Expression.Parameter name }
  | MINUS; value = expression_primary
      { Expression.Subtract (Expression.Integer Z.zero, value) }
  | LPAREN; value = expression; RPAREN { value }

(** The old operand parser treats a leading [$parameter] as an unresolved value operand.
    Parenthesized parameter expressions remain available through the final production. *)
%public operand_expression:
  | value = operand_expression_primary { value }
  | left = operand_expression; PLUS; right = expression_primary { Expression.Add (left, right) }
  | left = operand_expression; MINUS; right = expression_primary { Expression.Subtract (left, right) }
  | left = operand_expression; STAR; right = expression_primary { Expression.Multiply (left, right) }
  | left = operand_expression; LOGAND; right = expression_primary { Expression.Logand (left, right) }
  | left = operand_expression; LOGOR; right = expression_primary { Expression.Logor (left, right) }
  | left = operand_expression; SHIFT_LEFT; right = expression_primary { Expression.Shift_left (left, right) }
  | left = operand_expression; SHIFT_RIGHT; right = expression_primary { Expression.Shift_right (left, right) }

operand_expression_primary:
  | value = INTEGER { Expression.Integer value }
  | CURRENT_ADDRESS { Expression.Current_address }
  | MAX_ADDRESS { Expression.Max_address }
  | STACK_ADDRESS { Expression.Stack_address }
  | name = operand_symbol_name { Expression.Symbol name }
  | MINUS; value = operand_expression_primary
      { Expression.Subtract (Expression.Integer Z.zero, value) }
  | LPAREN; value = expression; RPAREN { value }

expression_name:
  | name = declared_name { name }

operand_symbol_name:
  | name = IDENT { name }
  | name = JMP { name } | name = JNZ { name } | name = MOVE { name }
  | name = LOAD { name } | name = STORE { name } | name = ADD { name }
  | name = SUB { name } | name = MUL { name } | name = REM { name }
  | name = DIV { name } | name = LT { name } | name = LEA { name }
  | name = RESTRICT { name } | name = SUBSEG { name } | name = GETL { name }
  | name = GETB { name } | name = GETE { name } | name = GETA { name }
  | name = GETP { name } | name = GETOTYPE { name } | name = GETWTYPE { name }
  | name = SEAL { name } | name = UNSEAL { name } | name = INVOKE { name }
  | name = FAIL { name } | name = HALT { name } | name = LOADU { name }
  | name = STOREU { name } | name = ISPTR { name } | name = PROMOTEU { name }
  | name = EINIT { name } | name = EDEINIT { name }
  | name = ESTOREID { name } | name = ISUNIQUE { name }
  | name = JALR { name } | name = READSR { name } | name = WRITESR { name }
  | name = LAND { name } | name = LOR { name } | name = LSHIFTL { name }
  | name = LSHIFTR { name } | name = SYSTEM_REGISTER { name }
  | name = GRIOTTE_PERMISSION { name }
