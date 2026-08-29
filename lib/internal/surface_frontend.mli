val parse_program : Lexing.lexbuf -> (Asm_ir.t, string) result
val parse_regfile : Lexing.lexbuf -> (Irreg.t, string) result
