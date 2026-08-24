{
  open Parser
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
let locality = ("LOCAL" | "GLOBAL" | "DIRECTED" | "Local" | "Global" | "Directed")
let letter = ['a'-'z' 'A'-'Z']
let label = ('_' | letter) (letter | '_' | digit)*

rule token = parse
| eof { EOF }
| [' ' '\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| ';' { comment lexbuf }
| ((digit+) | ("0x" hex+)) as i { try INT (int_of_string i)
                                  with Failure _ -> error lexbuf
                                    ("invalid integer '" ^ i
                                     ^ "'; use a value that fits in a machine integer")}
| ("Inf" | "inf" | "∞") { INF }

(* assembler directives and macro references *)
| "&CURRENT_ADDR" { CURRENTADDR }
| "%macro" { MACRO }
| "%endmacro" { ENDMACRO }
| "%define" { DEFINE }
| '%' (label as name) { MACROCALL name }
| '$' (label as name) { PARAM name }

(* registers *)
| ['p' 'P'] ['c' 'C'] { PC }
| ['s' 'S'] ['t' 'T'] ['k' 'K'] { STK }
| ['d' 'D'] ['d' 'D'] ['c' 'C'] { DDC }
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
| "invoke" { INVOKE }
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
| _ as c { error lexbuf
              (Printf.sprintf
                 "unexpected character %C; remove it or replace it with a valid Cerise token" c) }

and comment = parse
| eof { EOF }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _ { comment lexbuf }
