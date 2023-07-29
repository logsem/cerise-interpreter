let parse_prog (filename: string): (Ast.t, string) Result.t =
  let input = open_in filename in
  try
    let filebuf = Lexing.from_channel input in
    let parse_res = Ir.translate_prog @@ Parser.main Lexer.token filebuf in
    close_in input; Result.Ok parse_res
  with Failure _ -> close_in input; Result.Error "Parsing Failed"

let parse_regfile (filename: string) (max_addr : Z.t)
  : (Ast.word Machine.RegMap.t, string) Result.t =
  let input = open_in filename in
  try
    let filebuf = Lexing.from_channel input in
    let parse_res =
      Irreg.translate_regfile @@
      Parser_regfile.main Lexer_regfile.token filebuf
    in
    close_in input; Result.Ok (parse_res max_addr)
  with Failure _ -> close_in input; Result.Error "Parsing Failed"

let init_machine
    (prog : Ast.t)
    (addr_max : Z.t option)
    (init_regs : Ast.word Machine.RegMap.t)
  : Machine.mchn =
  let addr_max =
    match addr_max with
    | Some a_max -> a_max
    | None -> Z.of_int (List.length prog) in
  let addr_start = Z.(~$0) in (* TODO lookup the PC *)
  let init_mems = Machine.init_mem_state addr_start addr_max prog in
  Machine.init init_regs init_mems
