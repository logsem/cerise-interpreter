open Libinterp

(* main loop *)
let () =
  let addr_max = (Int32.to_int Int32.max_int)/4096 in
  let (filename_prog, regfile_name, size_mem) =
    Cli_parser.parse_argument_interactive addr_max
  in

  let module Cfg = struct let addr_max : Z.t = size_mem end in
  let module Ui = Interactive_ui.MkUi (Cfg) in

  let prog_panel_start = ref Z.zero in
  let stk_panel_start =
    ref Z.(if !Parameters.flags.stack then ((Cfg.addr_max)/ ~$2) else ~$0)
  in

  let prog =
    match Program.parse_prog filename_prog with
    | Ok prog -> prog
    | Error msg ->
      Printf.eprintf "Program parse error: %s\n" msg;
      exit 1
  in

  let regfile =
    let init_regfile = (Machine.init_reg_state Cfg.addr_max) in
    if regfile_name = ""
    then init_regfile
    else
      (match Program.parse_regfile regfile_name Cfg.addr_max !stk_panel_start with
       | Ok regs ->
         (Machine.RegMap.fold
            (fun r w rf -> Machine.RegMap.add r w rf) regs) init_regfile
       | Error msg ->
         Printf.eprintf "Regfile parse error: %s\n" msg;
         exit 1)
  in

  let m_init = Program.init_machine prog (Some Cfg.addr_max) regfile in

  Ui.render_loop ~show_stack:(!Parameters.flags.stack) prog_panel_start stk_panel_start m_init
