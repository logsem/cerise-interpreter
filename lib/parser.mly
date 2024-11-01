%token EOF
%token PC STK DDC
%token <int> REG
%token <int> INT
%token INF
%token <string> LABELDEF
%token <string> LABEL
%token LPAREN RPAREN LSBRK RSBRK LCBRK RCBRK
%token PLUS MINUS COMMA SHARP COLON
%token JMP JNZ MOVE LOAD STORE ADD SUB MUL REM DIV LT LEA RESTRICT SUBSEG
%token GETL GETB GETE GETA GETP GETOTYPE GETWTYPE SEAL UNSEAL INVOKE
%token LOADU STOREU PROMOTEU FAIL HALT
%token EINIT EDEINIT ESTOREID ISUNIQUE
%token LOCAL GLOBAL DIRECTED
%token O E RO RX RW RWX RWL RWLX URW URWX URWL URWLX
%token SO S U SU
%token Int Cap SealRange Sealed
%left PLUS MINUS EXPR
%left UMINUS

%start <Ir.t> main
%{ open! Ir %}

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
  | INVOKE; r1 = reg; r2 = reg; p = main; { Invoke (r1, r2) :: p }
  | LOADU; r1 = reg; r2 = reg; c = reg_const; p = main; { LoadU (r1, r2, c) :: p }
  | STOREU; r = reg; c1 = reg_const; c2 = reg_const; p = main; { StoreU (r, c1, c2) :: p }
  | PROMOTEU; r = reg; p = main ; { PromoteU r :: p }
  | EINIT; r1 = reg; r2 = reg; p = main; { EInit (r1, r2) :: p }
  | EDEINIT; r1 = reg; r2 = reg; p = main; { EDeInit (r1, r2) :: p }
  | ESTOREID; r1 = reg; r2 = reg; p = main; { EStoreId (r1, r2) :: p }
  | ISUNIQUE; r1 = reg; r2 = reg; p = main; { IsUnique (r1, r2) :: p }
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
  | DDC; { ddc }
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
  | Sealed ; { W_Sealed }

locality:
  | LOCAL; { Local }
  | GLOBAL; { Global }
  | DIRECTED; { Directed }

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
  | e1 = expr; PLUS; e2 = expr { AddOp (e1,e2) }
  | e1 = expr; MINUS; e2 = expr { SubOp (e1,e2) }
  | MINUS; e = expr %prec UMINUS { SubOp (IntLit (Infinite_z.of_int 0),e) }
  | i = INT { IntLit (Infinite_z.of_int i) }
  | INF { IntLit (Infinite_z.Inf) }
  | lbl = LABEL { Label lbl }

%%
