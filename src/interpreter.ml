open Cerise

let () =
  let mode, backend, filename_prog, regfile_name = Cli_parser.parse_arguments () in
  let module M = (val Legacy_machine_backend.select backend) in

  (* Parse initial memory (program) *)
  let prog =
    match Program.parse_prog_from_file filename_prog with
    | Ok prog -> prog
    | Error msg ->
        Printf.eprintf "Program parse error: %s\n" msg;
        exit 1
  in

  let stk_addr =
    Z.(
      if !Parameters.flags.stack then !Parameters.flags.max_addr / ~$2 else ~$0)
  in

  (* Parse initial register file *)
  let regfile =
    let init_regfile = M.init_reg_state stk_addr in
    if regfile_name = "" then init_regfile
    else
      match Program.parse_regfile_from_file regfile_name stk_addr with
      | Ok regs -> (M.RegMap.fold (fun r w rf -> M.RegMap.add r w rf) regs) init_regfile
      | Error msg ->
          Printf.eprintf "Regfile parse error: %s\n" msg;
          exit 1
  in
  let m_init = M.init regfile (M.init_mem_state Z.zero prog) in

  match mode with
  | Cli_parser.Interactive_mode ->
      let module Cfg = struct
        let addr_max : Z.t = Parameters.get_max_addr ()
      end in
      let module Ui = Interactive_ui.MkUi (M) (Cfg) in
      let prog_panel_start = ref Z.zero in
      let stk_panel_start = ref stk_addr in
      Ui.render_loop ~show_stack:!Parameters.flags.stack prog_panel_start stk_panel_start m_init
  | Cli_parser.Interpreter_mode ->
      let module Ui = Interpreter_ui.Make (M) in
      Ui.interpreter m_init
