%token EOF
%token PC STK
%token <int> REG
%token <int> INT
%token <string> SYMBOL
%token INF
%token <string> LABELDEF
%token <string> LABEL
%token LPAREN RPAREN LSBRK RSBRK LCBRK RCBRK
%token PLUS MINUS COMMA SHARP COLON
%token JMP JNZ MOVE LOAD STORE ADD SUB MUL REM DIV LT LEA RESTRICT SUBSEG
%token GETL GETB GETE GETA GETP GETOTYPE GETWTYPE SEAL UNSEAL
%token LOADU STOREU PROMOTEU FAIL HALT
%token LOCAL GLOBAL DIRECTED
%token O E RO RX RW RWX RWL RWLX URW URWX URWL URWLX
%token SO S U SU
%token Int Cap SealRange Sealed

%token TEXT_SECTION DATA_SECTION EXPORT_SECTION IMPORT_SECTION INIT_SECTION START_SECTION

%left PLUS MINUS EXPR
%left UMINUS

%start <Ir_linkable_object.t> main
%{ open! Ir_linkable_object
   open! Ir
   open! Misc %}

%%

main:
TEXT_SECTION ; text_section = list(symb_word) ;
DATA_SECTION ; data_section = list(symb_word);
(* TODO imports, exports, and init should be optional, right ?*)
IMPORT_SECTION ; imports_section = imports ;
EXPORT_SECTION ; exports_section = exports ;
INIT_SECTION ; init_section = inits ;
start_section = main_entry ; EOF
{
  {
    text_section = text_section;
    data_section = data_section;
    imports_section = imports_section;
    exports_section = exports_section;
    init_section = init_section;
    start_offset = start_section;
  }
}

(* TODO *)
imports:
  | { SymbolMap.empty }

inits:
  | { SymbolMap.empty }

exports:
  | e = export_entry ; exps = exports { let (s,n) = e in SymbolMap.add s n exps }
  | { SymbolMap.empty }

export_entry: s = SYMBOL ; COLON ; sec = section_type ; offset = INT { (s, (sec, offset)) }
main_entry:
| START_SECTION ; sec = section_type ; offset = INT { Some (sec, offset) }
| { None }

section_type:
| TEXT_SECTION { CodeSection }
| DATA_SECTION { DataSection }

symb_word:
  | w = concrete_word { ConcreteWord w }
  | s = SYMBOL { Symbol s }

concrete_word:
  | JMP; r = reg; { (Jmp r) }
  | JNZ; r1 = reg; r2 = reg; { Jnz (r1, r2) }
  | MOVE; r = reg; c = reg_const; { Move (r, c) }
  | LOAD; r1 = reg; r2 = reg; { Load (r1, r2) }
  | STORE; r = reg; c = reg_const; { Store (r, c) }
  | ADD; r = reg; c1 = reg_const; c2 = reg_const; { Add (r, c1, c2) }
  | SUB; r = reg; c1 = reg_const; c2 = reg_const; { Sub (r, c1, c2) }
  | MUL; r = reg; c1 = reg_const; c2 = reg_const; { Mul (r, c1, c2) }
  | REM; r = reg; c1 = reg_const; c2 = reg_const; { Rem (r, c1, c2) }
  | DIV; r = reg; c1 = reg_const; c2 = reg_const; { Div (r, c1, c2) }
  | LT; r = reg; c1 = reg_const; c2 = reg_const; { Lt (r, c1, c2) }
  | LEA; r = reg; c = reg_const; { Lea (r, c) }
  | RESTRICT; r = reg; c = reg_const; { Restrict (r, c) }
  | SUBSEG; r = reg; c1 = reg_const; c2 = reg_const; { SubSeg (r, c1, c2) }
  | GETL; r1 = reg; r2 = reg; { GetL (r1, r2) }
  | GETB; r1 = reg; r2 = reg; { GetB (r1, r2) }
  | GETE; r1 = reg; r2 = reg; { GetE (r1, r2) }
  | GETA; r1 = reg; r2 = reg; { GetA (r1, r2) }
  | GETP; r1 = reg; r2 = reg; { GetP (r1, r2) }
  | GETOTYPE; r1 = reg; r2 = reg; { GetOType (r1, r2) }
  | GETWTYPE; r1 = reg; r2 = reg; { GetWType (r1, r2) }
  | SEAL; r1 = reg; r2 = reg; r3 = reg; { Seal (r1, r2, r3) }
  | UNSEAL; r1 = reg; r2 = reg; r3 = reg; { UnSeal (r1, r2, r3) }
  | LOADU; r1 = reg; r2 = reg; c = reg_const; { LoadU (r1, r2, c) }
  | STOREU; r = reg; c1 = reg_const; c2 = reg_const; { StoreU (r, c1, c2) }
  | PROMOTEU; r = reg; ; { PromoteU r }
  | FAIL; { Fail }
  | HALT; { Halt }
  | lbl = LABELDEF; { Lbl lbl }
  | SHARP ; w = word_def; { Ir.Word w }

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
  | STK; { STK }
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
