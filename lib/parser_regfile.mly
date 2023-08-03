%token EOF
%token PC
%token <int> REG
%token <int> INT
%token MAX_ADDR
%token LPAREN RPAREN LSBRK RSBRK LCBRK RCBRK
%token PLUS MINUS AFFECT COMMA COLON
%token O E RO RX RW RWX
%token S U SU

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
  | i = REG; { Reg i }

word:
  | e = expr %prec EXPR { WI (e) }
  | sb = sealable_def {WSealable sb}
  | s = sealed_def {s}

sealable_def:
  | LPAREN; p = perm; COMMA; b = addr; COMMA; e = addr; COMMA; a = addr; RPAREN;
    { WCap (p, b, e, a) }
  | LSBRK; p = seal_perm; COMMA; b = addr; COMMA; e = addr; COMMA; a = addr; RSBRK;
    { WSealRange (p, b, e, a) }

sealed_def:
  | LCBRK; o = addr; COLON; sb = sealable_def ; RCBRK
    { WSealed (o, sb) }

addr:
  | e = expr %prec EXPR { Addr (e) }
(* TODO support hexa addresses *)

seal_perm:
  | O; { (false, false) }
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

expr:
  | LPAREN; e = expr; RPAREN { e }
  | MAX_ADDR { MaxAddr }
  | e1 = expr; PLUS; e2 = expr { AddOp (e1,e2) }
  | e1 = expr; MINUS; e2 = expr { SubOp (e1,e2) }
  | MINUS; e = expr %prec UMINUS { SubOp ((IntLit 0),e) }
  | i = INT { IntLit i }

%%
