%token EOF
%token PC STK DDC
%token <int> REG
%token <int> INT
%token CURRENTADDR
%token <string> LABELDEF
%token <string> LABEL
%token LPAREN RPAREN LSBRK RSBRK LCBRK RCBRK
%token PLUS MINUS COMMA SHARP COLON
%token JMP JNZ MOVE LOAD STORE ADD SUB MUL REM DIV LT LEA RESTRICT SUBSEG
%token GETL GETB GETE GETA GETP GETOTYPE GETWTYPE SEAL UNSEAL INVOKE
%token LOADU STOREU PROMOTEU FAIL HALT
%token MACRO ENDMACRO DEFINE
%token <string> MACROCALL PARAM
%token LOCAL GLOBAL DIRECTED
%token O E RO RX RW RWX RWL RWLX URW URWX URWL URWLX
%token SO S U SU
%token Int Cap SealRange Sealed
%left PLUS MINUS EXPR
%left UMINUS

%start <Asm_ir.t> main
%{
  open! Asm_ir

  let location position =
    {
      filename = position.Lexing.pos_fname;
      line = position.pos_lnum;
      column = position.pos_cnum - position.pos_bol + 1;
    }

  let parameter_kind = function
    | "reg" -> RegKind
    | "value" -> ValueKind
    | "expr" -> ExprKind
    | "perm" -> PermKind
    | "sealperm" -> SealPermKind
    | "locality" -> LocalityKind
    | "wtype" -> WtypeKind
    | kind -> UnknownKind kind

  type parsed_expression = ParsedExpression of expr | BareParameter of string

  let as_expr = function
    | ParsedExpression expression -> expression
    | BareParameter name -> ExprParam name
%}

%%

main:
  | EOF; { ([]: Asm_ir.t) }
  | statement = top_statement; program = main; { statement :: program }

top_statement:
  | statement = basic_statement; { statement }
  | definition = integer_definition; { definition }
  | call = macro_call; { call }
  | MACRO; name = LABEL; LPAREN; parameters = separated_list(COMMA, macro_parameter); RPAREN;
    body = macro_body; ENDMACRO;
    { MacroDef { name; parameters; body; location = location $startpos } }

macro_body:
  | { [] }
  | statement = basic_statement; body = macro_body; { statement :: body }
  | definition = integer_definition; body = macro_body; { definition :: body }
  | call = macro_call; body = macro_body; { call :: body }

integer_definition:
  | DEFINE; name = LABEL; value = expr;
    { Define (name, value, location $startpos) }

macro_call:
  | name = MACROCALL; LPAREN; arguments = separated_list(COMMA, macro_argument); RPAREN;
    { MacroCall { name; arguments; location = location $startpos } }

macro_parameter:
  | name = LABELDEF; kind = LABEL; { { name; kind = parameter_kind kind } }

macro_argument:
  | r = concrete_reg; { Register r }
  | c = raw_expr;
    { match c with BareParameter name -> ValueParam name | ParsedExpression expression -> Const (ConstExpr expression) }
  | p = concrete_perm; { Const (Perm p) }
  | sp = concrete_seal_perm; { Const (SealPerm sp) }
  | LPAREN; p = concrete_perm; COMMA; l = concrete_locality; RPAREN; { Const (PermLoc (p, l)) }
  | LPAREN; p = concrete_seal_perm; COMMA; l = concrete_locality; RPAREN;
    { Const (SealPermLoc (p, l)) }
  | w = concrete_wtype; { Const (Wtype w) }
  | l = concrete_locality; { Const (Locality l) }

basic_statement:
  | JMP; r = reg; { Jmp r }
  | JNZ; r1 = reg; r2 = reg; { Jnz (r1, r2) }
  | MOVE; r = reg; c = reg_const; { Move (r, c) }
  | LOAD; r1 = reg; r2 = reg; { Load (r1, r2) }
  | STORE; r = reg; c = reg_const; { Store (r, c) }
  | ADD; r = reg; c1 = reg_const; c2 = reg_const; { Add (r, c1, c2) }
  | SUB; r = reg; c1 = reg_const; c2 = reg_const; { Sub (r, c1, c2) }
  | MUL; r = reg; c1 = reg_const; c2 = reg_const; { Mul (r, c1, c2) }
  | REM; r = reg; c1 = reg_const; c2 = reg_const; { Rem (r, c1, c2) }
  | DIV; r = reg; c1 = reg_const; c2 = reg_const; { Div (r, c1, c2) }
  | LT; r = reg; c1 = reg_const; c2 = reg_const; { Lt (r, c1, c2) }
  | LEA; r = reg; c = reg_const; { Lea (r, c) }
  | RESTRICT; r = reg; c = reg_const; { Restrict (r, c) }
  | SUBSEG; r = reg; c1 = reg_const; c2 = reg_const; { SubSeg (r, c1, c2) }
  | GETL; r1 = reg; r2 = reg; { GetL (r1, r2) }
  | GETB; r1 = reg; r2 = reg; { GetB (r1, r2) }
  | GETE; r1 = reg; r2 = reg; { GetE (r1, r2) }
  | GETA; r1 = reg; r2 = reg; { GetA (r1, r2) }
  | GETP; r1 = reg; r2 = reg; { GetP (r1, r2) }
  | GETOTYPE; r1 = reg; r2 = reg; { GetOType (r1, r2) }
  | GETWTYPE; r1 = reg; r2 = reg; { GetWType (r1, r2) }
  | SEAL; r1 = reg; r2 = reg; r3 = reg; { Seal (r1, r2, r3) }
  | UNSEAL; r1 = reg; r2 = reg; r3 = reg; { UnSeal (r1, r2, r3) }
  | INVOKE; r1 = reg; r2 = reg; { Invoke (r1, r2) }
  | LOADU; r1 = reg; r2 = reg; c = reg_const; { LoadU (r1, r2, c) }
  | STOREU; r = reg; c1 = reg_const; c2 = reg_const; { StoreU (r, c1, c2) }
  | PROMOTEU; r = reg; { PromoteU r }
  | FAIL; { Fail }
  | HALT; { Halt }
  | lbl = LABELDEF; { Lbl lbl }
  | SHARP; w = word_def; { Word w }

word_def:
  | sb = sealable_def; { Sealable sb }
  | sealed = sealed_def; { sealed }
  | z = expr; { I z }

sealable_def:
  | LPAREN; p = perm; COMMA; g = locality; COMMA; b = expr; COMMA; e = expr; COMMA; a = expr; RPAREN;
    { Cap (p, g, b, e, a) }
  | LSBRK; p = seal_perm; COMMA; g = locality; COMMA; b = expr; COMMA; e = expr; COMMA; a = expr; RSBRK;
    { SealRange (p, g, b, e, a) }

sealed_def:
  | LCBRK; o = expr; COLON; sb = sealable_def ; RCBRK
    { Sealed (o, sb) }

reg:
  | r = concrete_reg; { r }
  | name = PARAM; { RegParam name }

concrete_reg:
  | PC; { PC }
  | STK; { stk }
  | DDC; { ddc }
  | i = REG; { Reg i }

reg_const:
  | r = concrete_reg; { Register r }
  | c = raw_expr %prec EXPR
    { match c with BareParameter name -> ValueParam name | ParsedExpression expression -> Const (ConstExpr expression) }
  | p = concrete_perm; { Const (Perm p) }
  | sp = concrete_seal_perm; { Const (SealPerm sp) }
  | LPAREN ; p = concrete_perm; COMMA ;  g = locality ; RPAREN ; { Const (PermLoc (p,g)) }
  | LPAREN ; p = concrete_seal_perm; COMMA ;  g = locality ; RPAREN ; { Const (SealPermLoc (p,g)) }
  | LPAREN; name = PARAM; COMMA; g = locality; RPAREN; { Const (PairParam (name, g)) }
  | w = concrete_wtype; { Const (Wtype w) }
  | l = concrete_locality; { Const (Locality l) }

seal_perm:
  | sp = concrete_seal_perm; { sp }
  | name = PARAM; { SealPermParam name }

concrete_seal_perm:
  | SO; { SealPermLit (false, false) }
  | S; { SealPermLit (true, false) }
  | U; { SealPermLit (false, true) }
  | SU; { SealPermLit (true, true) }

concrete_wtype:
  | Int ; { W_I }
  | Cap ; { W_Cap }
  | SealRange ; { W_SealRange }
  | Sealed ; { W_Sealed }

locality:
  | l = concrete_locality; { l }
  | name = PARAM; { LocalityParam name }

concrete_locality:
  | LOCAL; { Local }
  | GLOBAL; { Global }
  | DIRECTED; { Directed }

perm:
  | p = concrete_perm; { p }
  | name = PARAM; { PermParam name }

concrete_perm:
  | O; { O }
  | E; { E }
  | RO; { RO }
  | RX; { RX }
  | RW; { RW }
  | RWX; { RWX }
  | RWL; { RWL }
  | RWLX; { RWLX }
  | URW; { URW }
  | URWX; { URWX }
  | URWL; { URWL }
  | URWLX; { URWLX }

expr:
  | e = raw_expr; { as_expr e }

raw_expr:
  | LPAREN; e = raw_expr; RPAREN { ParsedExpression (as_expr e) }
  | e1 = raw_expr; PLUS; e2 = raw_expr { ParsedExpression (AddOp (as_expr e1, as_expr e2)) }
  | e1 = raw_expr; MINUS; e2 = raw_expr { ParsedExpression (SubOp (as_expr e1, as_expr e2)) }
  | MINUS; e = raw_expr %prec UMINUS {
      ParsedExpression (SubOp (IntLit (Z.of_int 0), as_expr e))
    }
  | i = INT { ParsedExpression (IntLit (Z.of_int i)) }
  | CURRENTADDR { ParsedExpression CurrentAddr }
  | symbol = LABEL { ParsedExpression (Symbol symbol) }
  | name = PARAM { BareParameter name }

%%
