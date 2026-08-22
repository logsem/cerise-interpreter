let parse_prog_from_lexbuf (filebuf : Lexing.lexbuf) : (Ast.t, string) Result.t =
  try
    let parse_res = Ir.translate_prog @@ Parser.main Lexer.token filebuf in
    Parameters.check_program parse_res;
    Result.Ok parse_res
  with Failure _ -> Result.Error "Parsing Failed"

let parse_prog_from_file (filename : string) : (Ast.t, string) Result.t =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
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
  with Failure _ -> Result.Error "Parsing Failed"

let parse_regfile_from_file (filename : string) (stk_addr : Z.t) :
    (Ast.word Machine.RegMap.t, string) Result.t =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
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
