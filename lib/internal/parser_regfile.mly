%token EOF
%token PC MTDC CRA CSP CGP
%token CTP CT0 CT1 CT2 CT3 CT4 CT5 CT6
%token CS0 CS1 CS2 CS3 CS4 CS5 CS6 CS7 CS8 CS9 CS10 CS11
%token CA0 CA1 CA2 CA3 CA4 CA5 CA6 CA7
%token <int> REG
%token <int> INT
%token MAX_ADDR
%token LPAREN RPAREN LSBRK RSBRK LCBRK RCBRK
%token PLUS MINUS MULT AFFECT COMMA COLON
%token O R X W WL SR DI DL
%token SO S U SU
%token LOCAL GLOBAL
%left PLUS MINUS MULT EXPR
%left UMINUS

%start <Irreg.t> main
%{ open! Ast %}
%{ open! Irreg %}

%%

main:
  | EOF; { ([]: Irreg.t) }
  | r = reg ; AFFECT ; w = word ; p = main ;{ (r,w) :: p }

reg:
  | PC; { PC }
  | MTDC; { Ast.mtdc }
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
  | MAX_ADDR { MaxAddr }
  | e1 = expr; PLUS; e2 = expr { AddOp (e1,e2) }
  | e1 = expr; MINUS; e2 = expr { SubOp (e1,e2) }
  | e1 = expr; MULT; e2 = expr { MultOp (e1,e2) }
  | MINUS; e = expr %prec UMINUS { SubOp (IntLit (Z.of_int 0),e) }
  | i = INT { IntLit (Z.of_int i) }

%%
