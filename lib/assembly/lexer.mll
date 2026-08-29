{
open Generated_parser

exception Error of Diagnostic.source_location * string

let location lexbuf = Assembly_construction.location (Lexing.lexeme_start_p lexbuf)

let fail lexbuf message = raise (Error (location lexbuf, message))

let word lexbuf =
  let original = Lexing.lexeme lexbuf in
  match String.lowercase_ascii original with
  | "jmp" -> JMP original
  | "jnz" -> JNZ original
  | "move" | "mov" -> MOVE original
  | "load" -> LOAD original
  | "store" -> STORE original
  | "add" -> ADD original
  | "sub" -> SUB original
  | "mul" -> MUL original
  | "rem" -> REM original
  | "div" -> DIV original
  | "lt" -> LT original
  | "lea" -> LEA original
  | "restrict" -> RESTRICT original
  | "subseg" -> SUBSEG original
  | "getl" -> GETL original
  | "getb" -> GETB original
  | "gete" -> GETE original
  | "geta" -> GETA original
  | "getp" -> GETP original
  | "getotype" -> GETOTYPE original
  | "getwtype" -> GETWTYPE original
  | "seal" -> SEAL original
  | "unseal" -> UNSEAL original
  | "invoke" -> INVOKE original
  | "fail" -> FAIL original
  | "halt" -> HALT original
  | "loadu" -> LOADU original
  | "storeu" -> STOREU original
  | "isptr" -> ISPTR original
  | "promoteu" -> PROMOTEU original
  | "pc" | "ddc" | "stk" -> REGISTER original
  | name
    when String.length name > 1
         && name.[0] = 'r'
         && Option.is_some (int_of_string_opt (String.sub name 1 (String.length name - 1))) ->
      REGISTER original
  | "max_addr" -> MAX_ADDRESS
  | "stk_addr" -> STACK_ADDRESS
  | "o" | "e" | "ro" | "rx" | "rw" | "rwx" | "rwl" | "rwlx" | "urw"
  | "urwx" | "urwl" | "urwlx" -> PERMISSION original
  | "so" | "s" | "u" | "su" -> SEAL_PERMISSION original
  | "global" | "local" | "directed" -> LOCALITY original
  | "int" | "cap" | "sealrange" | "sealed" -> WORD_TYPE original
  | _ -> IDENT original

let directive name =
  match name with
  | "macro" -> MACRO
  | "define" -> DEFINE
  | "endmacro" -> ENDMACRO
  | _ -> CALL name
}

let letter = ['A'-'Z' 'a'-'z' '_']
let digit = ['0'-'9']
let name = letter (letter | digit)*
let hex = ['0'-'9' 'A'-'F' 'a'-'f']

rule token = parse
  | [' ' '\t' '\r']+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | ';' [^ '\n']* { token lexbuf }
  | "&CURRENT_ADDR" { CURRENT_ADDRESS }
  | "&&" { LOGAND }
  | "||" { LOGOR }
  | "<<" { SHIFT_LEFT }
  | ">>" { SHIFT_RIGHT }
  | ":=" { ASSIGN }
  | '%' (name as value) { directive value }
  | '$' (name as value) { PARAMETER value }
  | "0x" (hex+ as value) | "0X" (hex+ as value) {
      INTEGER (Z.of_string_base 16 value)
    }
  | "0x" | "0X" { fail lexbuf "A hexadecimal literal needs at least one digit." }
  | digit+ as value { INTEGER (Z.of_string value) }
  | name { word lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ',' { COMMA }
  | ':' { COLON }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '#' { HASH }
  | eof { EOF }
  | '%' | '$' as prefix {
      fail lexbuf (Printf.sprintf "Expected a name after %C." prefix)
    }
  | _ as character {
      fail lexbuf (Printf.sprintf "Unexpected character %C in assembly input." character)
    }
