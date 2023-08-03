%token EOF
%token PC STK
%token <int> REG
%token <int> INT
%token MAX_ADDR STK_ADDR
%token LPAREN RPAREN LSBRK RSBRK LCBRK RCBRK
%token PLUS MINUS AFFECT COMMA COLON
%token O E RO RX RW RWX RWL RWLX URW URWX URWL URWLX
%token SO S U SU
%token LOCAL GLOBAL DIRECTED
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
  | STK; { STK }
  | i = REG; { Reg i }

word:
  | e = expr %prec EXPR { WI (e) }
  | sb = sealable_def {WSealable sb}
  | s = sealed_def {s}

sealable_def:
  | LPAREN; p = perm; COMMA; g = locality ; COMMA ; b = addr; COMMA; e = addr; COMMA; a = addr; RPAREN;
    { WCap (p, g, b, e, a) }
  | LSBRK; p = seal_perm; COMMA; g = locality ; COMMA ; b = addr; COMMA; e = addr; COMMA; a = addr; RSBRK;
    { WSealRange (p, g, b, e, a) }

sealed_def:
  | LCBRK; o = addr; COLON; sb = sealable_def ; RCBRK
    { WSealed (o, sb) }

addr:
  | e = expr %prec EXPR { Addr (e) }
(* TODO support hexa addresses *)

locality:
  | LOCAL; { Local }
  | GLOBAL; { Global }
  | DIRECTED; { Directed }

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
  | URW; { URW }
  | URWX; { URWX }
  | URWL; { URWL }
  | URWLX; { URWLX }

expr:
  | LPAREN; e = expr; RPAREN { e }
  | MAX_ADDR { MaxAddr }
  | STK_ADDR { StkAddr }
  | e1 = expr; PLUS; e2 = expr { AddOp (e1,e2) }
  | e1 = expr; MINUS; e2 = expr { SubOp (e1,e2) }
  | MINUS; e = expr %prec UMINUS { SubOp ((IntLit 0),e) }
  | i = INT { IntLit i }

%%
