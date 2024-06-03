let failure_message message =
  let detail = if String.equal message "" then "unknown parser failure" else message in
  "Parsing failed: " ^ detail ^ ". Check the token at the reported location and its operands."

let parse_prog_from_lexbuf (filebuf : Lexing.lexbuf) : (Ast.t, string) Result.t =
  try
    match Parser_driver.parse_program filebuf with
    | Error _ as error -> error
    | Ok parsed ->
        let program = Ir.translate_prog parsed in
        Result.Ok program
  with
  | Label_resolver.Unknown_label label ->
      Result.Error
        (Printf.sprintf "Unknown label %S. Define it with `%s:` or correct the label reference."
           label label)
  | Asm_ir.ExprException message ->
      Result.Error (message ^ ". Replace `Inf` with a finite integer in this expression.")
  | Asm_ir.WordException _ ->
      Result.Error
        "A word was used where an instruction was expected. Prefix literal data with `#`, or use a \
         machine instruction."
  | Asm_ir.UnexpandedMacroException construct ->
      Result.Error ("Internal assembler error: unexpanded " ^ construct ^ ".")
  | Asm_ir.UnresolvedExpressionException _ ->
      Result.Error "Internal assembler error: unresolved expression reached IR translation."
  | Asm_ir.UnresolvedIrException construct ->
      Result.Error ("Internal assembler error: unresolved " ^ construct ^ " reached IR translation.")
  | Parameters.NotSupported message ->
      Result.Error
        (message ^ ". Choose a compatible architecture or remove the unsupported construct.")
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
    match Parser_driver.parse_regfile filebuf with
    | Error _ as error -> error
    | Ok parsed ->
        let regfile = Irreg.translate_regfile parsed !Parameters.flags.max_addr stk_addr in
        Result.Ok regfile
  with
  | Irreg.ExprException message ->
      Result.Error (message ^ ". Replace `Inf` with a finite integer in this value.")
  | Parameters.NotSupported message ->
      Result.Error (message ^ ". Choose a compatible architecture or remove the unsupported value.")
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
