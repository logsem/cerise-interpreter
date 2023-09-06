{
  open Parser_linkable_object
  exception Error of string
  let error lexbuf msg =
    let position = Lexing.lexeme_start_p lexbuf in
    let err_str = Printf.sprintf "Lexing error in file %s at position %d:%d\n"
                  position.pos_fname position.pos_lnum (position.pos_cnum - position.pos_bol + 1)
                  ^ msg ^ "\n" in
    raise (Error err_str)
}

let digit = ['0'-'9']
let hex = (digit | ['a'-'f'] | ['A'-'F'])
let reg_num = ((digit) | ('1' digit) | ('2' digit) | "30" | "31")
let perm = ('O' | 'E' | "RO" | "RW" | "RWX")
let locality = ("LOCAL" | "GLOBAL" | "DIRECTED" | "Local" | "Global" | "Directed")
let letter = ['a'-'z' 'A'-'Z']
let label = ( '_' | letter) (letter | '_' | digit)*
let symbol = (letter | '_' | '.' | digit)+

rule token = parse
| eof { EOF }
| [' ' '\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| ';' { comment lexbuf }
| ((digit+) | ("0x" hex+)) as i { try INT (int_of_string i)
                                  with Failure _ -> error lexbuf ("Invalid integer '" ^ i ^ "'.")}
| ("Inf" | "inf" | "∞") { INF }

(* registers *)
| ['p' 'P'] ['c' 'C'] { PC }
| ['s' 'S'] ['t' 'T'] ['k' 'K'] { STK }
| ['r' 'R'] (reg_num as n) { try REG (int_of_string n) 
                             with Failure _ -> error lexbuf ("Invalid register id '" ^ n ^ "'.")}

(* machine_op *)
| "jmp" { JMP }
| "jnz" { JNZ }
| "mov" { MOVE }
| "load" { LOAD }
| "store" { STORE }
| "add" { ADD }
| "sub" { SUB }
| "mul" { MUL }
| "rem" { REM }
| "div" { DIV }
| "lt" { LT }
| "lea" { LEA }
| "restrict" { RESTRICT }
| "subseg" { SUBSEG }
| "getl" { GETL }
| "getb" { GETB }
| "gete" { GETE }
| "geta" { GETA }
| "getp" { GETP }
| "getotype" { GETOTYPE }
| "getwtype" { GETWTYPE }
| "getl" { GETL }
| "seal" { SEAL }
| "unseal" { UNSEAL }
| "load" ['u' 'U'] { LOADU }
| "store" ['u' 'U'] { STOREU }
| "promote" ['u' 'U'] { PROMOTEU }
| "fail" { FAIL }
| "halt" { HALT }

(* single-character tokens *)
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LCBRK }
| '}' { RCBRK }
| '[' { LSBRK }
| ']' { RSBRK }
| '+' { PLUS }
| '-' { MINUS }
| ',' { COMMA }
| ':' { COLON }
| '#' { SHARP }

(* locality *)
| "LOCAL"    | "Local" { LOCAL }
| "GLOBAL"   | "Global"  { GLOBAL }
| "DIRECTED" | "Directed"  { DIRECTED }

(* permissions *)
| 'O' { O }
| 'E' { E }
| "RO" { RO }
| "RX" { RX }
| "RW" { RW }
| "RWX" { RWX }
| "RWL" { RWL }
| "RWLX" { RWLX }
| "URW" { URW }
| "URWX" { URWX }
| "URWL" { URWL }
| "URWLX" { URWLX }
| "SO" { SO }
| 'S' { S }
| 'U' { U }
| "SU" { SU }

(* word type *)
| "Int" { Int }
| "Cap" { Cap }
| "SealRange" { SealRange }
| "Sealed" { Sealed }

(* labels *)
| label as lbl ':' { LABELDEF (lbl) }
| label as lbl { LABEL (lbl) }

(* symbol and sections *)
| '$' (symbol as s) { SYMBOL s }
| ".text" { TEXT_SECTION }
| ".data" { DATA_SECTION }
| ".exports" { EXPORT_SECTION }
| ".imports" { IMPORT_SECTION }
| ".init" { INIT_SECTION }
| ".start" { START_SECTION }

and comment = parse
| eof { EOF }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _ { comment lexbuf }
