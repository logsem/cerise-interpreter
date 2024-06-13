%token EOF
%token PC STK CGP MTDC
%token <int> REG
%token <int> INT
%token <string> LABELDEF
%token <string> LABEL
%token LPAREN RPAREN LSBRK RSBRK LCBRK RCBRK
%token PLUS MINUS COMMA SHARP COLON
%token JALR JMP JNZ MOVESR MOVE LOAD STORE ADD SUB MUL REM DIV LT LEA RESTRICT SUBSEG
%token GETL GETB GETE GETA GETP GETOTYPE GETWTYPE SEAL UNSEAL
%token FAIL HALT
%token LOCAL GLOBAL
%token O R X W WL SR DI DL
%token SO S U SU
%token Int Cap SealRange Sealed
%left PLUS MINUS EXPR
%left UMINUS

%start <Ir.t> main
%{ open! Ast %}
%{ open! Ir %}

%%

main:
  | EOF; { ([]: Ir.t) }

  | JALR; r1 = reg; r2 = reg; p = main; { Jalr (r1,r2) :: p }
  | JMP; c = reg_const; p = main; { Jmp c :: p }
  | JNZ; r = reg; c = reg_const; p = main; { Jnz (r, c) :: p }

  | MOVESR; r = reg; c = reg_const; p = main; { MoveSR (r, c) :: p }

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
  | PC; { PC }
  | STK; { stk }
  | CGP; { cgp }
  | MTDC; { mtdc }
  | i = REG; { Reg i }

reg_const:
  | r = reg; { Register r }
  | c = expr %prec EXPR { Const (ConstExpr c) }
  | p = perm; { Const (Perm p) }
  | sp = seal_perm; { Const (SealPerm sp) }
  | LPAREN ; p = perm; COMMA ;  g = locality ; RPAREN ; { Const (PermLoc (p,g)) }
  | LPAREN ; p = seal_perm; COMMA ;  g = locality ; RPAREN ; { Const (SealPermLoc (p,g)) }
  | w = wtype; { Const (Wtype w) }
  | l = locality; { Const (Locality l) }

seal_perm:
  | SO; { (false, false) }
  | S; { (true, false) }
  | U; { (false, true) }
  | SU; { (true, true) }

wtype:
  | Int ; { W_I }
  | Cap ; { W_Cap }
  | SealRange ; { W_SealRange }
  | Sealed ; { Ast.W_Sealed }

locality:
  | LOCAL; { Local }
  | GLOBAL; { Global }

perm_enc:
  | R; { R: Perm.t }
  | X; { X: Perm.t }
  | W; { W: Perm.t }
  | WL; { WL: Perm.t }
  | SR; { SR: Perm.t }
  | DL; { DL: Perm.t }
  | DI; { DI: Perm.t }

perm:
  | O; { PermSet.empty }
  | p = perm_enc; { PermSet.singleton p }
  | LSBRK; p = perm_list ; RSBRK ; { p }
perm_list:
  | p = perm_enc; { PermSet.singleton p }
  | p = perm_enc ; tl = perm_list { PermSet.add p tl }

expr:
  | LPAREN; e = expr; RPAREN { e }
  | e1 = expr; PLUS; e2 = expr { AddOp (e1,e2) }
  | e1 = expr; MINUS; e2 = expr { SubOp (e1,e2) }
  | MINUS; e = expr %prec UMINUS { SubOp (IntLit (Z.of_int 0),e) }
  | i = INT { IntLit (Z.of_int i) }
  | lbl = LABEL { Label lbl }

%%
