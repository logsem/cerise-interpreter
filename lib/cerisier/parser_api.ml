let diagnostic location message = Error [ Diagnostic.error ~location message ]

let lexbuf ?filename source =
  let lexbuf = Lexing.from_string source in
  Lexing.set_filename lexbuf (Option.value filename ~default:"");
  lexbuf

let parse ?filename entry source =
  let lexbuf = lexbuf ?filename source in
  try Ok (entry Lexer.token lexbuf) with
  | Lexer.Error (location, message) -> diagnostic location message
  | Assembly_construction.Parse_error (location, message) -> diagnostic location message
  | Generated_parser.Error ->
      diagnostic
        (Assembly_construction.location (Lexing.lexeme_start_p lexbuf))
        (Printf.sprintf "Invalid Cerisier assembly syntax near %S." (Lexing.lexeme lexbuf))

let parse_program ?filename source =
  Result.bind (parse ?filename Generated_parser.program source) Asm_ir.assemble

let parse_regfile ?filename source = parse ?filename Generated_parser.regfile source

let parse_word ?filename source = parse ?filename Generated_parser.word source

type program = Asm_ir.program
type regfile = Asm_ir.regfile
type word = Asm_ir.word
