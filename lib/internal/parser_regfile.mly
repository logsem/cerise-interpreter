%token EOF
%token PC MTDC CRA CSP CGP CNULL
%token CTP CT0 CT1 CT2 CT3 CT4 CT5 CT6
%token CS0 CS1 CS2 CS3 CS4 CS5 CS6 CS7 CS8 CS9 CS10 CS11
%token CA0 CA1 CA2 CA3 CA4 CA5 CA6 CA7
%token <int> REG
%token <int> INT
%token MAX_ADDR
%token LPAREN RPAREN LSBRK RSBRK LCBRK RCBRK
%token PLUS MINUS MULT AFFECT COMMA COLON LANDOP LOROP LSL LSR
%token O Orx R X XSR Ow W WL DL LG DRO LM
%token SO S U SU
%token LOCAL GLOBAL
%left PLUS MINUS MULT EXPR LANDOP LOROP LSL LSR
%left UMINUS

%start <Irreg.t> main
%{ open! Ast %}
%{ open! Irreg %}

%%

main:
  | EOF; { (([],[]): Irreg.t) }
  | r = reg ; AFFECT ; w = word ; p = main ;{ ( ( (r,w) :: fst p), snd p) }
  | sr = sreg ; AFFECT ; w = word ; p = main ;{ ( fst p, ( (sr,w) :: snd p) ) }

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

word:
  | e = expr %prec EXPR { WI (e) }
  | sb = sealable_def {WSealable sb}
  | s = sealed_def {s}

sealable_def:
  | LPAREN; p = perm; COMMA; g = locality ; COMMA ; b = expr; COMMA; e = expr; COMMA; a = expr; RPAREN;
    { WCap (p, g, b, e, a) }
  | LSBRK; p = seal_perm; COMMA; g = locality ; COMMA ; b = expr; COMMA; e = expr; COMMA; a = expr; RSBRK;
    { WSealRange (p, g, b, e, a) }

sealed_def:
  | LCBRK; o = expr; COLON; sb = sealable_def ; RCBRK
    { WSealed (o, sb) }

locality:
  | LOCAL; { Local }
  | GLOBAL; { Global }

seal_perm:
  | SO; { (false, false) }
  | S; { (true, false) }
  | U; { (false, true) }
  | SU; { (true, true) }

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
  | O ; { (Orx,Ow,DL,DRO) : perm }
  | LSBRK; rx = rxperm_enc ; w = wperm_enc ; dl = dlperm_enc ; dro = droperm_enc ; RSBRK ; { (rx,w,dl,dro) : perm}

expr:
  | LPAREN; e = expr; RPAREN { e }
  | MAX_ADDR { MaxAddr }
  | e1 = expr; PLUS; e2 = expr { AddOp (e1,e2) }
  | e1 = expr; MINUS; e2 = expr { SubOp (e1,e2) }
  | e1 = expr; MULT; e2 = expr { MultOp (e1,e2) }
  | e1 = expr; LANDOP; e2 = expr { LandOp (e1,e2) }
  | e1 = expr; LOROP; e2 = expr { LorOp (e1,e2) }
  | e1 = expr; LSL; e2 = expr { LslOp (e1,e2) }
  | e1 = expr; LSR; e2 = expr { LsrOp (e1,e2) }
  | MINUS; e = expr %prec UMINUS { SubOp (IntLit (Z.of_int 0),e) }
  | i = INT { IntLit (Z.of_int i) }

%%
