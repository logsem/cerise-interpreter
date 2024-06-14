{
  open Parser
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
let locality = ("LOCAL" | "GLOBAL" | "Local" | "Global")
let letter = ['a'-'z' 'A'-'Z']
let label = ('_' | letter) (letter | '_' | digit)*

rule token = parse
| eof { EOF }
| [' ' '\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| ';' { comment lexbuf }
| ((digit+) | ("0x" hex+)) as i { try INT (int_of_string i)
                                  with Failure _ -> error lexbuf ("Invalid integer '" ^ i ^ "'.")}

(* registers *)
| ['p' 'P'] ['c' 'C'] { PC }
| ['s' 'S'] ['t' 'T'] ['k' 'K'] { STK }
| ['c' 'C'] ['g' 'G'] ['p' 'P'] { CGP }
| ['m' 'M'] ['t' 'T'] ['d' 'D'] ['c' 'C'] { MTDC }
| ['r' 'R'] (reg_num as n) { try REG (int_of_string n) 
                             with Failure _ -> error lexbuf ("Invalid register id '" ^ n ^ "'.")}

(* machine_op *)
| "jalr" { JALR }
| "jmp" { JMP }
| "jnz" { JNZ }
| "movsr" { MOVESR }
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
| '*' { MULT }
| ',' { COMMA }
| ':' { COLON }
| '#' { SHARP }

(* locality *)
| "LOCAL"    | "Local" { LOCAL }
| "GLOBAL"   | "Global"  { GLOBAL }

(* permissions *)
| 'O' { O }
| 'R' { R }
| 'X' { X }
| 'W' { W }
| "WL" { WL }
| "SR" { SR }
| "DL" { DL }
| "DI" { DI }
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

and comment = parse
| eof { EOF }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _ { comment lexbuf }
