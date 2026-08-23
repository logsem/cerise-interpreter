let location (filebuf : Lexing.lexbuf) =
  let position = Lexing.lexeme_start_p filebuf in
  let column = position.pos_cnum - position.pos_bol + 1 in
  if String.equal position.pos_fname "" then
    Printf.sprintf "line %d, column %d" position.pos_lnum column
  else Printf.sprintf "%s:%d:%d" position.pos_fname position.pos_lnum column

let syntax_error filebuf =
  let lexeme = Lexing.lexeme filebuf in
  if String.equal lexeme "" then
    Printf.sprintf "%s: syntax error: unexpected end of input" (location filebuf)
  else Printf.sprintf "%s: syntax error: unexpected token %S" (location filebuf) lexeme

let failure_message message =
  if String.equal message "" then "Parsing failed" else "Parsing failed: " ^ message

let parse_prog_from_lexbuf (filebuf : Lexing.lexbuf) : (Ast.t, string) Result.t =
  try
    let parse_res = Ir.translate_prog @@ Parser.main Lexer.token filebuf in
    Parameters.check_program parse_res;
    Result.Ok parse_res
  with
  | Parser.Error -> Result.Error (syntax_error filebuf)
  | Lexer.Error message -> Result.Error message
  | Ir.UnknownLabelException label -> Result.Error (Printf.sprintf "Unknown label %S" label)
  | Ir.ExprException message -> Result.Error message
  | Ir.WordException _ -> Result.Error "A word was used where an instruction was expected"
  | Parameters.NotSupported message -> Result.Error message
  | Failure message -> Result.Error (failure_message message)

let parse_prog_from_file (filename : string) : (Ast.t, string) Result.t =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  Lexing.set_filename filebuf filename;
  let res = parse_prog_from_lexbuf filebuf in
  close_in input;
  res

let parse_prog_from_string (source : string) : (Ast.t, string) Result.t =
  let filebuf = Lexing.from_string source in
  parse_prog_from_lexbuf filebuf

let parse_regfile_from_lexbuf (filebuf : Lexing.lexbuf) (stk_addr : Z.t) :
    (Ast.word Machine.RegMap.t, string) Result.t =
  try
    let parse_res = Irreg.translate_regfile @@ Parser_regfile.main Lexer_regfile.token filebuf in
    let parse_regfile = parse_res !Parameters.flags.max_addr stk_addr in
    Machine.RegMap.iter (fun _ w -> Parameters.check_word w) parse_regfile;
    Result.Ok parse_regfile
  with
  | Parser_regfile.Error -> Result.Error (syntax_error filebuf)
  | Lexer_regfile.Error message -> Result.Error message
  | Irreg.ExprException message -> Result.Error message
  | Parameters.NotSupported message -> Result.Error message
  | Failure message -> Result.Error (failure_message message)

let parse_regfile_from_file (filename : string) (stk_addr : Z.t) :
    (Ast.word Machine.RegMap.t, string) Result.t =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  Lexing.set_filename filebuf filename;
  let res = parse_regfile_from_lexbuf filebuf stk_addr in
  close_in input;
  res

let parse_regfile_from_string (source : string) (stk_addr : Z.t) :
    (Ast.word Machine.RegMap.t, string) Result.t =
  let filebuf = Lexing.from_string source in
  parse_regfile_from_lexbuf filebuf stk_addr

let init_machine (prog : Ast.t) (init_regs : Ast.word Machine.RegMap.t) : Machine.t =
  let addr_start = Z.(~$0) in
  (* TODO lookup the PC *)
  let init_mems = Machine.init_mem_state addr_start prog in
  Machine.init init_regs init_mems
