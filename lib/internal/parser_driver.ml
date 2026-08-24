let location (filebuf : Lexing.lexbuf) =
  let position = Lexing.lexeme_start_p filebuf in
  let column = position.pos_cnum - position.pos_bol + 1 in
  if String.equal position.pos_fname "" then
    Printf.sprintf "line %d, column %d" position.pos_lnum column
  else Printf.sprintf "%s:%d:%d" position.pos_fname position.pos_lnum column

let line_excerpt (filebuf : Lexing.lexbuf) =
  let position = Lexing.lexeme_start_p filebuf in
  let line_start = position.pos_bol - filebuf.lex_abs_pos in
  let buffer_length = filebuf.lex_buffer_len in
  if line_start < 0 || line_start > buffer_length then None
  else
    let rec line_end index =
      if index >= buffer_length || Bytes.get filebuf.lex_buffer index = '\n' then index
      else line_end (index + 1)
    in
    let line_end = line_end line_start in
    let source_line = Bytes.sub_string filebuf.lex_buffer line_start (line_end - line_start) in
    let source_line =
      if String.ends_with ~suffix:"\r" source_line then
        String.sub source_line 0 (String.length source_line - 1)
      else source_line
    in
    let byte_column = max 0 (position.pos_cnum - position.pos_bol) in
    let prefix_length = min byte_column (String.length source_line) in
    let prefix = String.sub source_line 0 prefix_length in
    let marker_indent =
      String.to_seq prefix
      |> Seq.filter_map (fun character ->
          if Char.code character land 0xc0 = 0x80 then None
          else Some (if Char.equal character '\t' then '\t' else ' '))
      |> String.of_seq
    in
    Some (Printf.sprintf "%4d | %s\n     | %s^" position.pos_lnum source_line marker_indent)

let with_excerpt filebuf message =
  match line_excerpt filebuf with None -> message | Some excerpt -> message ^ "\n" ^ excerpt

let rec options = function
  | [] -> "a valid Cerise token"
  | [ option ] -> option
  | [ first; second ] -> first ^ " or " ^ second
  | first :: rest -> first ^ ", " ^ options rest

let syntax_error filebuf expected =
  let lexeme = Lexing.lexeme filebuf in
  let expected = options expected in
  if String.equal lexeme "" then
    with_excerpt filebuf
      (Printf.sprintf
         "%s: syntax error: the input ended before this construct was complete. Expected %s. Add \
          the missing value or delimiter."
         (location filebuf) expected)
  else
    with_excerpt filebuf
      (Printf.sprintf
         "%s: syntax error: unexpected token %S. Expected %s. Replace this token, or check the \
          preceding instruction's operands."
         (location filebuf) lexeme expected)

let failure_message message =
  let detail = if String.equal message "" then "unknown parser failure" else message in
  "Parsing failed: " ^ detail ^ ". Check the token at the reported location and its operands."

let program_candidates =
  [
    ("an instruction", Parser.HALT);
    ("a register (`pc`, `stk`, `ddc`, or `r0`–`r31`)", Parser.REG 0);
    ("an integer, `Inf`, or label", Parser.INT 0);
    ("a permission such as `RO` or `RWX`", Parser.RO);
    ("a sealing permission (`SO`, `S`, `U`, or `SU`)", Parser.SO);
    ("a locality (`GLOBAL`, `LOCAL`, or `DIRECTED`)", Parser.GLOBAL);
    ("a word type such as `Int` or `Cap`", Parser.Int);
    ("a label definition ending in `:`", Parser.LABELDEF "label");
    ("a word declaration beginning with `#`", Parser.SHARP);
    ("an integer definition beginning with `%define`", Parser.DEFINE);
    ("a macro declaration beginning with `%macro`", Parser.MACRO);
    ("a macro call such as `%name(...)`", Parser.MACROCALL "name");
    ("a macro parameter such as `$name`", Parser.PARAM "name");
    ("`%endmacro`", Parser.ENDMACRO);
    ("`(`", Parser.LPAREN);
    ("`)`", Parser.RPAREN);
    ("`[`", Parser.LSBRK);
    ("`]`", Parser.RSBRK);
    ("`{`", Parser.LCBRK);
    ("`}`", Parser.RCBRK);
    ("`,`", Parser.COMMA);
    ("`:`", Parser.COLON);
    ("`+`", Parser.PLUS);
    ("`-`", Parser.MINUS);
    ("the end of input", Parser.EOF);
  ]

let regfile_candidates =
  [
    ("a register (`pc`, `stk`, `ddc`, or `r0`–`r31`)", Parser_regfile.REG 0);
    ("an integer, `Inf`, `MAX_ADDR`, or `STK_ADDR`", Parser_regfile.INT 0);
    ("a permission such as `RO` or `RWX`", Parser_regfile.RO);
    ("a sealing permission (`SO`, `S`, `U`, or `SU`)", Parser_regfile.SO);
    ("a locality (`GLOBAL`, `LOCAL`, or `DIRECTED`)", Parser_regfile.GLOBAL);
    ("`:=`", Parser_regfile.AFFECT);
    ("`(`", Parser_regfile.LPAREN);
    ("`)`", Parser_regfile.RPAREN);
    ("`[`", Parser_regfile.LSBRK);
    ("`]`", Parser_regfile.RSBRK);
    ("`{`", Parser_regfile.LCBRK);
    ("`}`", Parser_regfile.RCBRK);
    ("`,`", Parser_regfile.COMMA);
    ("`:`", Parser_regfile.COLON);
    ("`+`", Parser_regfile.PLUS);
    ("`-`", Parser_regfile.MINUS);
    ("the end of input", Parser_regfile.EOF);
  ]

let program_expected checkpoint position =
  List.filter_map
    (fun (description, token) ->
      if Parser.MenhirInterpreter.acceptable checkpoint token position then Some description
      else None)
    program_candidates

let regfile_expected checkpoint position =
  List.filter_map
    (fun (description, token) ->
      if Parser_regfile.MenhirInterpreter.acceptable checkpoint token position then Some description
      else None)
    regfile_candidates

let parse_program_syntax filebuf =
  let module I = Parser.MenhirInterpreter in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token filebuf in
  I.loop_handle_undo
    (fun program -> Ok program)
    (fun checkpoint _ ->
      let position = Lexing.lexeme_start_p filebuf in
      Error (syntax_error filebuf (program_expected checkpoint position)))
    supplier
    (Parser.Incremental.main filebuf.lex_curr_p)

let parse_regfile_syntax filebuf =
  let module I = Parser_regfile.MenhirInterpreter in
  let supplier = I.lexer_lexbuf_to_supplier Lexer_regfile.token filebuf in
  I.loop_handle_undo
    (fun regfile -> Ok regfile)
    (fun checkpoint _ ->
      let position = Lexing.lexeme_start_p filebuf in
      Error (syntax_error filebuf (regfile_expected checkpoint position)))
    supplier
    (Parser_regfile.Incremental.main filebuf.lex_curr_p)

let parse_program filebuf =
  try parse_program_syntax filebuf with
  | Lexer.Error message -> Error (with_excerpt filebuf message)
  | Failure message -> Error (failure_message message)

let parse_regfile filebuf =
  try parse_regfile_syntax filebuf with
  | Lexer_regfile.Error message -> Error (with_excerpt filebuf message)
  | Failure message -> Error (failure_message message)
