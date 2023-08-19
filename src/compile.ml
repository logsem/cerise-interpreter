open Libinterp

let full_compile filename_reg filename_asm prog =
  let (regs, mem) = Compiler.glue_compile prog in
  Compiler.output_machine filename_reg filename_asm regs mem


let parse_wat filename : (Ir_wasm.ws_module, string) Result.t =
  let input = open_in filename in
  try
    let filebuf = Lexing.from_channel input in
    let parse_res = Parser_wasm.main Lexer_wasm.token filebuf in
    close_in input;
    Result.Ok parse_res
  with Failure _ ->
    close_in input; Result.Error (Printf.sprintf "Parsing %s failed" filename)

exception CompileException of string

let () =
  let usage_msg =
    "compile [--output <asm-file> <reg-filename>] <files>"
  in
  let asmfile_name = ref "a.s" in
  let regfile_name = ref "a.reg" in
  let input_files = ref [] in
  let anon_fun filename = input_files := filename :: !input_files in
  let speclist =
    [
      ("--output",
       Arg.Tuple [Arg.Set_string asmfile_name; Arg.Set_string regfile_name],
       "Output asm file and register file");
      ("-o",
       Arg.Tuple [Arg.Set_string asmfile_name; Arg.Set_string regfile_name],
       "Output asm file and register file");
    ] in
  Arg.parse speclist anon_fun usage_msg;

  let input_wasm_modules =
    (List.map
       (fun f ->
          (match parse_wat f with
           | Result.Ok prog ->
             prog
           | Result.Error s ->
             raise @@ CompileException s))
       !input_files)
  in
  let (regs, mem) = Compiler.default_compiler input_wasm_modules in
  Compiler.output_machine !regfile_name !asmfile_name regs mem;

  full_compile
      "asm-toys/stack_loaded.reg"
      "asm-toys/stack_loaded.s"
      Extract.loaded_stack_example;
  full_compile
    "asm-toys/bank_unsafe_loaded.reg"
    "asm-toys/bank_unsafe_loaded.s"
    Extract.loaded_bank_unsafe_example;
  full_compile
    "asm-toys/dummy_loaded.reg"
    "asm-toys/dummy_loaded.s"
    Extract.loaded_dummy_example;
  full_compile
    "asm-toys/reg_alloc_loaded.reg"
    "asm-toys/reg_alloc_loaded.s"
    Extract.loaded_reg_alloc_example;
