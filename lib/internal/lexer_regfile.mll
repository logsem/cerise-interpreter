{
  open Parser_regfile
  exception Error of string
  let error lexbuf msg =
    let position = Lexing.lexeme_start_p lexbuf in
    let column = position.pos_cnum - position.pos_bol + 1 in
    let location =
      if String.equal position.pos_fname "" then
        Printf.sprintf "line %d, column %d" position.pos_lnum column
      else Printf.sprintf "%s:%d:%d" position.pos_fname position.pos_lnum column
    in
    let err_str = Printf.sprintf "%s: lexical error: %s" location msg in
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
                                  with Failure _ -> error lexbuf
                                    ("invalid integer '" ^ i
                                     ^ "'; use a value that fits in a machine integer")}

(* registers *)
| ['p' 'P'] ['c' 'C'] { PC }
| ['s' 'S'] ['t' 'T'] ['k' 'K'] { STK }
| ['c' 'C'] ['g' 'G'] ['p' 'P'] { CGP }
| ['m' 'M'] ['t' 'T'] ['c' 'C'] ['c' 'C'] { MTCC }
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
| ',' { COMMA }
| ':' { COLON }
| ":=" { AFFECT }

(* locality *)
| "LOCAL"    | "Local" { LOCAL }
| "GLOBAL"   | "Global"  { GLOBAL }

(* permissions *)
| 'O' { O }
| 'E' { E }
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
| _ as c { error lexbuf
              (Printf.sprintf
                 "unexpected character %C; remove it or replace it with a valid Cerise token" c) }

and comment = parse
| eof { EOF }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _ { comment lexbuf }
