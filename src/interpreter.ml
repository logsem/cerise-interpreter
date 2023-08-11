open Libinterp

let () =
  let (mode, filename_prog, regfile_name) =
    Cli_parser.parse_arguments ()
  in

  (* Parse initial memory (program) *)
  let prog =
    match Program.parse_prog filename_prog with
    | Ok prog -> prog
    | Error msg ->
      Printf.eprintf "Program parse error: %s\n" msg;
      exit 1
  in

  let stk_addr =
    Z.(if !Parameters.flags.stack
       then
         match !Parameters.flags.max_addr with
         | Int z -> (z/ ~$2)
         | Inf -> (Parameters.max_addr/ ~$2)
       else ~$0)
  in

  (* Parse initial register file *)
  let regfile =
    let init_regfile = (Machine.init_reg_state stk_addr) in
    if regfile_name = ""
    then init_regfile
    else
      (match Program.parse_regfile regfile_name stk_addr with
       | Ok regs ->
         (Machine.RegMap.fold
            (fun r w rf -> Machine.RegMap.add r w rf) regs) init_regfile
       | Error msg ->
         Printf.eprintf "Regfile parse error: %s\n" msg;
         exit 1)
  in
  let m_init = Program.init_machine prog regfile in

  match mode with
  | Cli_parser.Interactive_mode ->
    let module Cfg = struct let addr_max : Z.t = Parameters.get_max_addr () end in
    let module Ui = Interactive_ui.MkUi (Cfg) in
    let prog_panel_start = ref Z.zero in
    let stk_panel_start = ref stk_addr in
    Ui.render_loop ~show_stack:(!Parameters.flags.stack) prog_panel_start stk_panel_start m_init

  | Cli_parser.Interpreter_mode ->
    Interpreter_ui.interpreter m_init
