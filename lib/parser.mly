%token EOF
%token PC
%token <int> REG
%token <int> INT
%token <string> LABELDEF
%token <string> LABEL
%token LPAREN RPAREN LSBRK RSBRK LCBRK RCBRK
%token PLUS MINUS COMMA SHARP COLON
%token JMP JNZ MOVE LOAD STORE ADD SUB MUL REM DIV LT LEA RESTRICT SUBSEG
%token GETB GETE GETA GETP GETOTYPE GETWTYPE SEAL UNSEAL FAIL HALT
%token O E RO RX RW RWX
%token S U SU

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
  | LPAREN; p = perm; COMMA; b = expr; COMMA; e = expr; COMMA; a = expr; RPAREN;
    { Cap (p, b, e, a) }
  | LSBRK; p = seal_perm; COMMA; b = expr; COMMA; e = expr; COMMA; a = expr; RSBRK;
    { SealRange (p, b, e, a) }

sealed_def:
  | LCBRK; o = expr; COLON; sb = sealable_def ; RCBRK
    { Sealed (o, sb) }

reg:
  | PC; { PC }
  | i = REG; { Reg i }

(* TODO we want to also capture seal_perm and wtype *)
reg_const:
  | r = reg; { Register r }
  | c = expr %prec EXPR { CP (Const (c)) }
  | p = perm; { CP (Perm p) }

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
  | e1 = expr; PLUS; e2 = expr { AddOp (e1,e2) }
  | e1 = expr; MINUS; e2 = expr { SubOp (e1,e2) }
  | MINUS; e = expr %prec UMINUS { SubOp ((IntLit 0),e) }
  | i = INT { IntLit i }
  | lbl = LABEL { Label lbl }

%%
