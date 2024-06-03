%token EOF
%token PC STK CGP
%token <int> REG
%token <int> INT
%token <string> LABELDEF
%token <string> LABEL
%token LPAREN RPAREN LSBRK RSBRK LCBRK RCBRK
%token PLUS MINUS COMMA SHARP COLON
%token JMP JNZ MOVE LOAD STORE ADD SUB MUL REM DIV LT LEA RESTRICT SUBSEG
%token GETL GETB GETE GETA GETP GETOTYPE GETWTYPE SEAL UNSEAL
%token FAIL HALT
%token LOCAL GLOBAL
%token O E RO RX RW RWX RWL RWLX
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
  | EOF; { ([]: Ir.t) }
  | JMP; r = reg; p = main; { Jmp r :: p }
  | JNZ; r1 = reg; r2 = reg; p = main; { Jnz (r1, r2) :: p }
  | MOVE; r = reg; c = reg_const; p = main; { Move (r, c) :: p }
  | LOAD; r1 = reg; r2 = reg; p = main; { Load (r1, r2) :: p }
  | STORE; r = reg; c = reg_const; p = main; { Store (r, c) :: p }
  | ADD; r = reg; c1 = reg_const; c2 = reg_const; p = main; { Add (r, c1, c2) :: p }
  | SUB; r = reg; c1 = reg_const; c2 = reg_const; p = main; { Sub (r, c1, c2) :: p }
  | MUL; r = reg; c1 = reg_const; c2 = reg_const; p = main; { Mul (r, c1, c2) :: p }
  | REM; r = reg; c1 = reg_const; c2 = reg_const; p = main; { Rem (r, c1, c2) :: p }
  | DIV; r = reg; c1 = reg_const; c2 = reg_const; p = main; { Div (r, c1, c2) :: p }
  | LT; r = reg; c1 = reg_const; c2 = reg_const; p = main; { Lt (r, c1, c2) :: p }
  | LEA; r = reg; c = reg_const; p = main; { Lea (r, c) :: p }
  | RESTRICT; r = reg; c = reg_const; p = main; { Restrict (r, c) :: p }
  | SUBSEG; r = reg; c1 = reg_const; c2 = reg_const; p = main; { SubSeg (r, c1, c2) :: p }
  | GETL; r1 = reg; r2 = reg; p = main; { GetL (r1, r2) :: p }
  | GETB; r1 = reg; r2 = reg; p = main; { GetB (r1, r2) :: p }
  | GETE; r1 = reg; r2 = reg; p = main; { GetE (r1, r2) :: p }
  | GETA; r1 = reg; r2 = reg; p = main; { GetA (r1, r2) :: p }
  | GETP; r1 = reg; r2 = reg; p = main; { GetP (r1, r2) :: p }
  | GETOTYPE; r1 = reg; r2 = reg; p = main; { GetOType (r1, r2) :: p }
  | GETWTYPE; r1 = reg; r2 = reg; p = main; { GetWType (r1, r2) :: p }
  | SEAL; r1 = reg; r2 = reg; r3 = reg; p = main; { Seal (r1, r2, r3) :: p }
  | UNSEAL; r1 = reg; r2 = reg; r3 = reg; p = main; { UnSeal (r1, r2, r3) :: p }
  | FAIL; p = main; { Fail :: p }
  | HALT; p = main; { Halt :: p }
  | lbl = LABELDEF; p = main; { Lbl lbl :: p }
  | SHARP ; w = word_def; p = main { Word w :: p }

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
  | CGP; { cgp }
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

expr:
  | LPAREN; e = expr; RPAREN { e }
  | e1 = expr; PLUS; e2 = expr { AddOp (e1,e2) }
  | e1 = expr; MINUS; e2 = expr { SubOp (e1,e2) }
  | MINUS; e = expr %prec UMINUS { SubOp (IntLit (Z.of_int 0),e) }
  | i = INT { IntLit (Z.of_int i) }
  | lbl = LABEL { Label lbl }

%%
