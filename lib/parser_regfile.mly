%token EOF
%token PC CGP STK
%token <int> REG
%token <int> INT
%token MAX_ADDR STK_ADDR
%token LPAREN RPAREN LSBRK RSBRK LCBRK RCBRK
%token PLUS MINUS AFFECT COMMA COLON
%token O E RO RX RW RWX RWL RWLX
%token SO S U SU
%token LOCAL GLOBAL
%left PLUS MINUS EXPR
%left UMINUS

%start <Irreg.t> main
%{ open! Irreg %}

%%

main:
  | EOF; { ([]: Irreg.t) }
  | r = reg ; AFFECT ; w = word ; p = main ;{ (r,w) :: p }

reg:
  | PC; { PC }
  | CGP; { CGP }
  | STK; { STK }
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

perm:
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
  | MAX_ADDR { MaxAddr }
  | STK_ADDR { StkAddr }
  | e1 = expr; PLUS; e2 = expr { AddOp (e1,e2) }
  | e1 = expr; MINUS; e2 = expr { SubOp (e1,e2) }
  | MINUS; e = expr %prec UMINUS { SubOp (IntLit (Z.of_int 0),e) }
  | i = INT { IntLit (Z.of_int i) }

%%
