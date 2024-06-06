%token EOF
%token PC CGP STK MTDC
%token <int> REG
%token <int> INT
%token MAX_ADDR
%token LPAREN RPAREN LSBRK RSBRK LCBRK RCBRK
%token PLUS MINUS AFFECT COMMA COLON
%token O E R X W WL SR DI DL
%token SO S U SU
%token LOCAL GLOBAL
%left PLUS MINUS EXPR
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
  | CGP; { CGP }
  | STK; { STK }
  | MTDC; { MTDC }
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
  | E; { E: Perm.t }
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
  | MINUS; e = expr %prec UMINUS { SubOp (IntLit (Z.of_int 0),e) }
  | i = INT { IntLit (Z.of_int i) }

%%
