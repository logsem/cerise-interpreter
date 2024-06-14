{
  open Parser_regfile
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
let addr = ("MAX_ADDR" | "STK_ADDR")
let locality = ("LOCAL" | "GLOBAL" | "Local" | "Global")
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
| eof { EOF }
| [' ' '\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| ';' { comment lexbuf }
| ((digit+) | ("0x" hex+)) as i { try INT (int_of_string i)
                                  with Failure _ -> error lexbuf ("Invalid integer '" ^ i ^ "'.")}

(* registers *)
| ['p' 'P'] ['c' 'C'] { PC }
| ['m' 'M'] ['t' 'T'] ['d' 'D'] ['c' 'C'] { MTDC }
| ['c' 'C'] ['r' 'R'] ['a' 'A'] { CRA }
| ['c' 'C'] ['s' 'S'] ['p' 'P'] { CSP }
| ['c' 'C'] ['g' 'G'] ['p' 'P'] { CGP }

| ['c' 'C'] ['t' 'T'] ['p' 'P'] { CTP }
| ['c' 'C'] ['t' 'T'] '0' { CT0 }
| ['c' 'C'] ['t' 'T'] '1' { CT1 }
| ['c' 'C'] ['t' 'T'] '2' { CT2 }
| ['c' 'C'] ['t' 'T'] '3' { CT3 }
| ['c' 'C'] ['t' 'T'] '4' { CT4 }
| ['c' 'C'] ['t' 'T'] '5' { CT5 }
| ['c' 'C'] ['t' 'T'] '6' { CT6 }

| ['c' 'C'] ['s' 'S'] '0' { CS0 }
| ['c' 'C'] ['s' 'S'] '1' { CS1 }
| ['c' 'C'] ['s' 'S'] '2' { CS2 }
| ['c' 'C'] ['s' 'S'] '3' { CS3 }
| ['c' 'C'] ['s' 'S'] '4' { CS4 }
| ['c' 'C'] ['s' 'S'] '5' { CS5 }
| ['c' 'C'] ['s' 'S'] '6' { CS6 }
| ['c' 'C'] ['s' 'S'] '7' { CS7 }
| ['c' 'C'] ['s' 'S'] '8' { CS8 }
| ['c' 'C'] ['s' 'S'] '9' { CS9 }
| ['c' 'C'] ['s' 'S'] "10" { CS10 }
| ['c' 'C'] ['s' 'S'] "11" { CS11 }

| ['c' 'C'] ['a' 'A'] '0' { CA0 }
| ['c' 'C'] ['a' 'A'] '1' { CA1 }
| ['c' 'C'] ['a' 'A'] '2' { CA2 }
| ['c' 'C'] ['a' 'A'] '3' { CA3 }
| ['c' 'C'] ['a' 'A'] '4' { CA4 }
| ['c' 'C'] ['a' 'A'] '5' { CA5 }
| ['c' 'C'] ['a' 'A'] '6' { CA6 }
| ['c' 'C'] ['a' 'A'] '7' { CA7 }

| ['r' 'R'] (reg_num as n) { try REG (int_of_string n)
                             with Failure _ -> error lexbuf ("Invalid register id '" ^ n ^ "'.")}

(* addresses *)
| "MAX_ADDR" { MAX_ADDR }

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
| ":=" { AFFECT }

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

and comment = parse
| eof { EOF }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _ { comment lexbuf }
