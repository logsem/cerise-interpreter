let diagnostic (location : Diagnostic.source_location) (message : string) : ('a, Diagnostic.t list) result = Error [ Diagnostic.error ~location message ]

let lexbuf ?filename:(filename : string option) (source : string) : Lexing.lexbuf =
  let lexbuf = Lexing.from_string source in
  Lexing.set_filename lexbuf (Option.value filename ~default:"");
  lexbuf

let parse ?filename:(filename : string option) (entry : ((Lexing.lexbuf -> Generated_parser.token) -> Lexing.lexbuf -> 'a)) (source : string) : ('a, Diagnostic.t list) result =
  let lexbuf = lexbuf ?filename source in
  try Ok (entry Lexer.token lexbuf) with
  | Lexer.Error (location, message) -> diagnostic location message
  | Assembly_construction.Parse_error (location, message) -> diagnostic location message
  | Generated_parser.Error ->
      diagnostic
        (Assembly_construction.source_location_of_lexing_position (Lexing.lexeme_start_p lexbuf))
        (Printf.sprintf "Invalid Cerisier assembly syntax near %S." (Lexing.lexeme lexbuf))

let parse_program ?filename:(filename : string option) (source : string) : (Asm_ir.statement list, Diagnostic.t list) result =
  Result.bind (parse ?filename Generated_parser.program source) Asm_ir.assemble_source_program

let parse_regfile ?filename:(filename : string option) (source : string) : (Asm_ir.regfile, Diagnostic.t list) result = parse ?filename Generated_parser.regfile source

let parse_word ?filename:(filename : string option) (source : string) : (Asm_ir.word_term, Diagnostic.t list) result = parse ?filename Generated_parser.word source

type program = Asm_ir.program
type regfile = Asm_ir.regfile
type word = Asm_ir.word
