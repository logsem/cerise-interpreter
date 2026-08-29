let diagnostic location message = Error [ Diagnostic.error ~location message ]

let lexbuf ?filename source =
  let lexbuf = Lexing.from_string source in
  Lexing.set_filename lexbuf (Option.value filename ~default:"");
  lexbuf

let check_finite ?filename source =
  let lexbuf = lexbuf ?filename source in
  let rec loop () =
    match Lexer.token lexbuf with
    | Generated_parser.IDENT name
      when List.mem (String.lowercase_ascii name) [ "inf"; "infinity" ] ->
        diagnostic
          (Assembly_construction.location (Lexing.lexeme_start_p lexbuf))
          "Cerisier bounds are finite; infinity is not accepted."
    | Generated_parser.EOF -> Ok ()
    | _ -> loop ()
  in
  try loop () with
  | Lexer.Error (location, message) -> diagnostic location message

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
  Result.bind (check_finite ?filename source) (fun () ->
      Result.bind (parse ?filename Generated_parser.program source) Asm_ir.assemble)

let parse_regfile ?filename source =
  Result.bind (check_finite ?filename source) (fun () ->
      parse ?filename Generated_parser.regfile source)

let parse_word ?filename source =
  Result.bind (check_finite ?filename source) (fun () -> parse ?filename Generated_parser.word source)

type program = Asm_ir.program
type regfile = Asm_ir.regfile
type word = Asm_ir.word
