let failure_message message =
  let detail = if String.equal message "" then "unknown parser failure" else message in
  "Parsing failed: " ^ detail ^ ". Check the token at the reported location and its operands."

let parse_prog_from_lexbuf (filebuf : Lexing.lexbuf) : (Ast.t, string) Result.t =
  try
    match Parser_driver.parse_program filebuf with
    | Error _ as error -> error
    | Ok parsed -> (
        match Macro_expander.expand parsed with
        | Error _ as error -> error
        | Ok expanded ->
            let current_addresses_resolved = Current_address_resolver.resolve expanded in
            let labels_resolved = Label_resolver.resolve current_addresses_resolved in
            let expressions_evaluated = Expression_evaluator.evaluate labels_resolved in
            Result.Ok (Asm_ir.translate_prog expressions_evaluated))
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

let parse_regfile_from_lexbuf (filebuf : Lexing.lexbuf) :
    (Ast.word Machine.RegMap.t * Ast.word Machine.SRegMap.t, string) Result.t =
  try
    match Parser_driver.parse_regfile filebuf with
    | Error _ as error -> error
    | Ok (parsed, sparse) ->
        let regfile = Irreg.translate_regfile parsed !Parameters.flags.max_addr in
        let sregfile = Irreg.translate_sregfile sparse !Parameters.flags.max_addr in
        Result.Ok (regfile, sregfile)
  with
  | Irreg.ExprException message ->
      Result.Error (message ^ ". Replace `Inf` with a finite integer in this value.")
  | Failure message -> Result.Error (failure_message message)

let parse_regfile_from_file (filename : string) :
    (Ast.word Machine.RegMap.t * Ast.word Machine.SRegMap.t, string) Result.t =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  Lexing.set_filename filebuf filename;
  let res = parse_regfile_from_lexbuf filebuf in
  close_in input;
  res

let parse_regfile_from_string (source : string) :
    (Ast.word Machine.RegMap.t * Ast.word Machine.SRegMap.t, string) Result.t =
  let filebuf = Lexing.from_string source in
  parse_regfile_from_lexbuf filebuf

let init_machine (prog : Ast.t) (init_regs : Ast.word Machine.RegMap.t)
    (init_sregs : Ast.word Machine.SRegMap.t) : Machine.t =
  let addr_start = Z.(~$0) in
  (* TODO lookup the PC *)
  let init_mems = Machine.init_mem_state addr_start prog in
  let init_config = Machine.init init_regs init_sregs init_mems in
  Machine.check_init_config init_config;
  init_config
