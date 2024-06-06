open Machine

let parse_prog (filename : string) : (Ast.t, string) Result.t =
  let input = open_in filename in
  try
    let filebuf = Lexing.from_channel input in
    let parse_res = Ir.translate_prog @@ Parser.main Lexer.token filebuf in
    close_in input;
    Result.Ok parse_res
  with Failure _ ->
    close_in input;
    Result.Error "Parsing Failed"

let parse_regfile (filename : string) : (Ast.word RegMap.t, string) Result.t =
  let input = open_in filename in
  try
    let filebuf = Lexing.from_channel input in
    let parse_res = Irreg.translate_regfile @@ Parser_regfile.main Lexer_regfile.token filebuf in
    let parse_regfile = parse_res (Parameters.get_max_addr ()) in
    close_in input;
    Result.Ok parse_regfile
  with Failure _ ->
    close_in input;
    Result.Error "Parsing Failed"

let init_machine (prog : Ast.t) (init_regs : Ast.word RegMap.t) : mchn =
  let addr_start = Z.(~$0) in
  (* TODO lookup the PC *)
  let init_mems = init_mem_state addr_start prog in
  let init_config = init init_regs init_mems in
  check_init_config init_config;
  init_config
