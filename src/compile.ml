open Libmachine
open Libcompile
open Libui
open Wasm

let parse_wat filename : Ir_wasm.ws_module =
  let input = open_in filename in
  try
    let filebuf = Lexing.from_channel input in
    let parse_res = Parser_wasm.main Lexer_wasm.token filebuf in
    close_in input;
    parse_res
  with Failure _ -> close_in input; failwith (Printf.sprintf "Parsing %s failed" filename)

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
    (List.map parse_wat !input_files)
  in
  let (regs, mem) = Compiler.default_compiler input_wasm_modules in
  Compiler.output_machine !regfile_name !asmfile_name regs mem;
