open Libinterp

let () =
  let mode, filename_prog, regfile_name = Cli_parser.parse_arguments () in

  (* Parse initial memory (program) *)
  let prog =
    match Program.parse_prog filename_prog with
    | Ok prog -> prog
    | Error msg ->
        Printf.eprintf "Program parse error: %s\n" msg;
        exit 1
  in

  let stk_addr = Z.(!Parameters.flags.max_addr / ~$2) in

  (* Parse initial register file *)
  let regfile =
    let init_regfile = Machine.init_reg_state in
    if regfile_name = "" then init_regfile
    else
      match Program.parse_regfile regfile_name with
      | Ok regs ->
          (Machine.RegMap.fold (fun r w rf -> Machine.RegMap.add r w rf) regs)
            Machine.init_reg_state_zeros
      | Error msg ->
          Printf.eprintf "Regfile parse error: %s\n" msg;
          exit 1
  in
  let m_init =
    try Program.init_machine prog regfile
    with Machine.CheckInitFailed w ->
      failwith
        ("The word "
        ^ Pretty_printer.string_of_ast_word w
        ^ " is not derived from an architectural root.")
  in

  match mode with
  | Cli_parser.Interactive_mode ->
      let module Cfg = struct
        let addr_max : Z.t = Parameters.get_max_addr ()
      end in
      let module Ui = Interactive_ui.MkUi (Cfg) in
      let show_stack = false in
      let prog_panel_start = ref Z.zero in
      let stk_panel_start = ref stk_addr in
      Ui.render_loop ~show_stack prog_panel_start stk_panel_start m_init
  | Cli_parser.Interpreter_mode -> Interpreter_ui.interpreter m_init
