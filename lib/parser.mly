%token EOF
%token PC MTDC CNULL CRA CSP CGP
%token CTP CT0 CT1 CT2 CT3 CT4 CT5 CT6
%token CS0 CS1 CS2 CS3 CS4 CS5 CS6 CS7 CS8 CS9 CS10 CS11
%token CA0 CA1 CA2 CA3 CA4 CA5 CA6 CA7
%token <int> REG
%token <int> INT
%token <string> LABELDEF
%token <string> LABEL
%token LPAREN RPAREN LSBRK RSBRK LCBRK RCBRK
%token PLUS MINUS MULT COMMA SHARP COLON LANDOP LOROP LSL LSR
%token JALR JMP JNZ READSR WRITESR MOVE LOAD STORE ADD SUB MUL REM DIV LT LEA RESTRICT SUBSEG
%token LAND LOR LSHIFTL LSHIFTR
%token GETL GETB GETE GETA GETP GETOTYPE GETWTYPE SEAL UNSEAL
%token FAIL HALT
%token LOCAL GLOBAL
%token O Orx R X XSR Ow W WL DL LG DRO LM
%token SO S U SU
%token Int Cap SealRange Sealed
%left PLUS MINUS MULT EXPR LANDOP LOROP LSL LSR
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

  | READSR; r = reg; sr = sreg; p = main; { ReadSR (r, sr) :: p }
  | WRITESR; sr = sreg; r = reg; p = main; { WriteSR (sr, r) :: p }

  | MOVE; r = reg; c = reg_const; p = main; { Move (r, c) :: p }
  | LOAD; r1 = reg; r2 = reg; p = main; { Load (r1, r2) :: p }
  | STORE; r = reg; c = reg_const; p = main; { Store (r, c) :: p }
  | ADD; r = reg; c1 = reg_const; c2 = reg_const; p = main; { Add (r, c1, c2) :: p }
  | SUB; r = reg; c1 = reg_const; c2 = reg_const; p = main; { Sub (r, c1, c2) :: p }
  | MUL; r = reg; c1 = reg_const; c2 = reg_const; p = main; { Mul (r, c1, c2) :: p }
  | REM; r = reg; c1 = reg_const; c2 = reg_const; p = main; { Rem (r, c1, c2) :: p }
  | DIV; r = reg; c1 = reg_const; c2 = reg_const; p = main; { Div (r, c1, c2) :: p }
  | LT; r = reg; c1 = reg_const; c2 = reg_const; p = main; { Lt (r, c1, c2) :: p }
  | LAND; r = reg; c1 = reg_const; c2 = reg_const; p = main; { LAnd (r, c1, c2) :: p }
  | LOR; r = reg; c1 = reg_const; c2 = reg_const; p = main; { LOr (r, c1, c2) :: p }
  | LSHIFTL; r = reg; c1 = reg_const; c2 = reg_const; p = main; { LShiftL (r, c1, c2) :: p }
  | LSHIFTR; r = reg; c1 = reg_const; c2 = reg_const; p = main; { LShiftR (r, c1, c2) :: p }
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

sreg:
  | MTDC; { Ast.MTDC }

reg:
  | PC; { PC }
  | CNULL; { Ast.cnull }
  | CRA; { Ast.cra }
  | CSP; { Ast.csp }
  | CGP; { Ast.cgp }

  | CTP; { Ast.ctp }
  | CT0; { Ast.ct0 }
  | CT1; { Ast.ct1 }
  | CT2; { Ast.ct2 }
  | CT3; { Ast.ct3 }
  | CT4; { Ast.ct4 }
  | CT5; { Ast.ct5 }
  | CT6; { Ast.ct6 }

  | CS0; { Ast.cs0 }
  | CS1; { Ast.cs1 }
  | CS2; { Ast.cs2 }
  | CS3; { Ast.cs3 }
  | CS4; { Ast.cs4 }
  | CS5; { Ast.cs5 }
  | CS6; { Ast.cs6 }
  | CS7; { Ast.cs7 }
  | CS8; { Ast.cs8 }
  | CS9; { Ast.cs9 }
  | CS10; { Ast.cs10 }
  | CS11; { Ast.cs11 }

  | CA0; { Ast.ca0 }
  | CA1; { Ast.ca1 }
  | CA2; { Ast.ca2 }
  | CA3; { Ast.ca3 }
  | CA4; { Ast.ca4 }
  | CA5; { Ast.ca5 }
  | CA6; { Ast.ca6 }
  | CA7; { Ast.ca7 }

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

rxperm_enc:
  | Orx ; { Orx : rxperm }
  | R ; { R : rxperm }
  | X ; { X : rxperm }
  | XSR ; { XSR : rxperm }

wperm_enc:
  | Ow ; { Ow : wperm }
  | W ; { W : wperm }
  | WL ; { WL : wperm }

dlperm_enc:
  | DL ; { DL : dlperm }
  | LG ; { LG : dlperm }

droperm_enc:
  | DRO ; { DRO : droperm }
  | LM ; { LM : droperm }

perm:
  | O ; { null_perm : perm }
  | LSBRK; rx = rxperm_enc ; w = wperm_enc ; dl = dlperm_enc ; dro = droperm_enc ; RSBRK ; { (rx,w,dl,dro) : perm}

expr:
  | LPAREN; e = expr; RPAREN { e }
  | e1 = expr; PLUS; e2 = expr { AddOp (e1,e2) }
  | e1 = expr; MINUS; e2 = expr { SubOp (e1,e2) }
  | e1 = expr; MULT; e2 = expr { MultOp (e1,e2) }
  | e1 = expr; LANDOP; e2 = expr { LandOp (e1,e2) }
  | e1 = expr; LOROP; e2 = expr { LorOp (e1,e2) }
  | e1 = expr; LSL; e2 = expr { LslOp (e1,e2) }
  | e1 = expr; LSR; e2 = expr { LsrOp (e1,e2) }
  | MINUS; e = expr %prec UMINUS { SubOp (IntLit (Z.of_int 0),e) }
  | i = INT { IntLit (Z.of_int i) }
  | lbl = LABEL { Label lbl }

%%
