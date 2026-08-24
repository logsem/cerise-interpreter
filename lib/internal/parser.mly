%token EOF
%token PC MTDC CNULL CRA CSP CGP
%token CTP CT0 CT1 CT2 CT3 CT4 CT5 CT6
%token CS0 CS1 CS2 CS3 CS4 CS5 CS6 CS7 CS8 CS9 CS10 CS11
%token CA0 CA1 CA2 CA3 CA4 CA5 CA6 CA7
%token <int> REG
%token <int> INT
%token <string> LABELDEF
%token <string> LABEL
%token CURRENTADDR
%token LPAREN RPAREN LSBRK RSBRK LCBRK RCBRK
%token PLUS MINUS MULT COMMA SHARP COLON LANDOP LOROP LSL LSR
%token JALR JMP JNZ READSR WRITESR MOVE LOAD STORE ADD SUB MUL REM DIV LT LEA RESTRICT SUBSEG
%token LAND LOR LSHIFTL LSHIFTR
%token GETL GETB GETE GETA GETP GETOTYPE GETWTYPE SEAL UNSEAL
%token FAIL HALT
%token MACRO ENDMACRO DEFINE
%token <string> MACROCALL PARAM
%token LOCAL GLOBAL
%token O Orx R X XSR Ow W WL DL LG DRO LM
%token SO S U SU
%token Int Cap SealRange Sealed SENTRY E
%left PLUS MINUS MULT EXPR LANDOP LOROP LSL LSR
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
  | EOF; { ([] : Asm_ir.t) }
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
  | LPAREN; p = concrete_perm; COMMA; l = concrete_locality; RPAREN;
    { Const (PermLoc (p, l)) }
  | LPAREN; p = concrete_seal_perm; COMMA; l = concrete_locality; RPAREN;
    { Const (SealPermLoc (p, l)) }
  | w = concrete_wtype; { Const (Wtype w) }
  | l = concrete_locality; { Const (Locality l) }

basic_statement:
  | JALR; r1 = reg; r2 = reg; { Jalr (r1, r2) }
  | JMP; value = reg_const; { Jmp value }
  | JNZ; register = reg; value = reg_const; { Jnz (register, value) }
  | READSR; register = reg; system_register = sreg; { ReadSR (register, system_register) }
  | WRITESR; system_register = sreg; register = reg; { WriteSR (system_register, register) }
  | MOVE; r = reg; c = reg_const; { Move (r, c) }
  | LOAD; r1 = reg; r2 = reg; { Load (r1, r2) }
  | STORE; r = reg; c = reg_const; { Store (r, c) }
  | ADD; r = reg; c1 = reg_const; c2 = reg_const; { Add (r, c1, c2) }
  | SUB; r = reg; c1 = reg_const; c2 = reg_const; { Sub (r, c1, c2) }
  | MUL; r = reg; c1 = reg_const; c2 = reg_const; { Mul (r, c1, c2) }
  | REM; r = reg; c1 = reg_const; c2 = reg_const; { Rem (r, c1, c2) }
  | DIV; r = reg; c1 = reg_const; c2 = reg_const; { Div (r, c1, c2) }
  | LAND; r = reg; c1 = reg_const; c2 = reg_const; { LAnd (r, c1, c2) }
  | LOR; r = reg; c1 = reg_const; c2 = reg_const; { LOr (r, c1, c2) }
  | LSHIFTL; r = reg; c1 = reg_const; c2 = reg_const; { LShiftL (r, c1, c2) }
  | LSHIFTR; r = reg; c1 = reg_const; c2 = reg_const; { LShiftR (r, c1, c2) }
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
  | FAIL; { Fail }
  | HALT; { Halt }
  | label = LABELDEF; { Lbl label }
  | SHARP; word = word_def; { Word word }

word_def:
  | sealable = sealable_def; { Sealable sealable }
  | LPAREN; E; MINUS; permission = perm; COMMA; locality = locality; COMMA; base = expr;
    COMMA; ending = expr; COMMA; address = expr; RPAREN;
    { Sentry (permission, locality, base, ending, address) }
  | sealed = sealed_def; { sealed }
  | expression = expr; { I expression }

sealable_def:
  | LPAREN; permission = perm; COMMA; locality = locality; COMMA; base = expr;
    COMMA; ending = expr; COMMA; address = expr; RPAREN;
    { Cap (permission, locality, base, ending, address) }
  | LSBRK; permission = seal_perm; COMMA; locality = locality; COMMA; base = expr;
    COMMA; ending = expr; COMMA; address = expr; RSBRK;
    { SealRange (permission, locality, base, ending, address) }

sealed_def:
  | LCBRK; otype = expr; COLON; sealable = sealable_def; RCBRK;
    { Sealed (otype, sealable) }

sreg:
  | MTDC; { Ast.MTDC }

reg:
  | r = concrete_reg; { r }
  | name = PARAM; { RegParam name }

concrete_reg:
  | PC; { PC }
  | CNULL; { Reg 0 }
  | CRA; { Reg 1 }
  | CSP; { Reg 2 }
  | CGP; { Reg 3 }
  | CTP; { Reg 4 }
  | CT0; { Reg 5 }
  | CT1; { Reg 6 }
  | CT2; { Reg 7 }
  | CT3; { Reg 28 }
  | CT4; { Reg 29 }
  | CT5; { Reg 30 }
  | CT6; { Reg 31 }
  | CS0; { Reg 8 }
  | CS1; { Reg 9 }
  | CS2; { Reg 18 }
  | CS3; { Reg 19 }
  | CS4; { Reg 20 }
  | CS5; { Reg 21 }
  | CS6; { Reg 22 }
  | CS7; { Reg 23 }
  | CS8; { Reg 24 }
  | CS9; { Reg 25 }
  | CS10; { Reg 26 }
  | CS11; { Reg 27 }
  | CA0; { Reg 10 }
  | CA1; { Reg 11 }
  | CA2; { Reg 12 }
  | CA3; { Reg 13 }
  | CA4; { Reg 14 }
  | CA5; { Reg 15 }
  | CA6; { Reg 16 }
  | CA7; { Reg 17 }
  | i = REG; { Reg i }

reg_const:
  | r = concrete_reg; { Register r }
  | c = raw_expr %prec EXPR
    { match c with BareParameter name -> ValueParam name | ParsedExpression expression -> Const (ConstExpr expression) }
  | p = concrete_perm; { Const (Perm p) }
  | sp = concrete_seal_perm; { Const (SealPerm sp) }
  | LPAREN; p = concrete_perm; COMMA; l = locality; RPAREN; { Const (PermLoc (p, l)) }
  | LPAREN; p = concrete_seal_perm; COMMA; l = locality; RPAREN;
    { Const (SealPermLoc (p, l)) }
  | LPAREN; name = PARAM; COMMA; l = locality; RPAREN; { Const (PairParam (name, l)) }
  | w = concrete_wtype; { Const (Wtype w) }
  | l = concrete_locality; { Const (Locality l) }

rxperm:
  | Orx; { Ast.Orx }
  | R; { Ast.R }
  | X; { Ast.X }
  | XSR; { Ast.XSR }

wperm:
  | Ow; { Ast.Ow }
  | W; { Ast.W }
  | WL; { Ast.WL }

dlperm:
  | DL; { Ast.DL }
  | LG; { Ast.LG }

droperm:
  | DRO; { Ast.DRO }
  | LM; { Ast.LM }

perm:
  | p = concrete_perm; { p }
  | name = PARAM; { PermParam name }

concrete_perm:
  | O; { PermLit Ast.null_perm }
  | LSBRK; rx = rxperm; w = wperm; dl = dlperm; dro = droperm; RSBRK;
    { PermLit (rx, w, dl, dro) }

seal_perm:
  | p = concrete_seal_perm; { p }
  | name = PARAM; { SealPermParam name }

concrete_seal_perm:
  | SO; { SealPermLit (false, false) }
  | S; { SealPermLit (true, false) }
  | U; { SealPermLit (false, true) }
  | SU; { SealPermLit (true, true) }

wtype:
  | word_type = concrete_wtype; { word_type }
  | name = PARAM; { WtypeParam name }

concrete_wtype:
  | Int; { W_I }
  | Cap; { W_Cap }
  | SealRange; { W_SealRange }
  | Sealed; { W_Sealed }
  | SENTRY; { W_Sentry }

locality:
  | l = concrete_locality; { l }
  | name = PARAM; { LocalityParam name }

concrete_locality:
  | LOCAL; { Local }
  | GLOBAL; { Global }

expr:
  | e = raw_expr; { as_expr e }

raw_expr:
  | LPAREN; e = raw_expr; RPAREN; { ParsedExpression (as_expr e) }
  | left = raw_expr; PLUS; right = raw_expr; { ParsedExpression (AddOp (as_expr left, as_expr right)) }
  | left = raw_expr; MINUS; right = raw_expr; { ParsedExpression (SubOp (as_expr left, as_expr right)) }
  | left = raw_expr; MULT; right = raw_expr; { ParsedExpression (MultOp (as_expr left, as_expr right)) }
  | left = raw_expr; LANDOP; right = raw_expr; { ParsedExpression (LandOp (as_expr left, as_expr right)) }
  | left = raw_expr; LOROP; right = raw_expr; { ParsedExpression (LorOp (as_expr left, as_expr right)) }
  | left = raw_expr; LSL; right = raw_expr; { ParsedExpression (LslOp (as_expr left, as_expr right)) }
  | left = raw_expr; LSR; right = raw_expr; { ParsedExpression (LsrOp (as_expr left, as_expr right)) }
  | MINUS; e = raw_expr %prec UMINUS
    { ParsedExpression (SubOp (IntLit Z.zero, as_expr e)) }
  | i = INT; { ParsedExpression (IntLit (Z.of_int i)) }
  | CURRENTADDR; { ParsedExpression CurrentAddr }
  | symbol = LABEL; { ParsedExpression (Symbol symbol) }
  | name = PARAM; { BareParameter name }

%%
