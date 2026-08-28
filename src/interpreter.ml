open Cerise

let () =
  let mode, backend, filename_prog, regfile_name = Cli_parser.parse_arguments () in
  let module M = (val Machine_backend.select backend) in

  let prog =
    match Program.parse_prog_from_file filename_prog with
    | Ok prog -> prog
    | Error msg ->
        Printf.eprintf "Program parse error: %s\n" msg;
        exit 1
  in

  let stk_addr = Z.(!Parameters.flags.max_addr / ~$2) in
  let regfile, sregfile =
    if regfile_name = "" then (M.init_reg_state, M.init_sreg_state_zeros)
    else
      match Program.parse_regfile_from_file regfile_name with
      | Ok (regs, sregs) ->
          let regfile =
            M.RegMap.fold (fun r w rf -> M.RegMap.add r w rf) regs M.init_reg_state_zeros
          in
          let sregfile =
            M.SRegMap.fold (fun sr w srf -> M.SRegMap.add sr w srf) sregs
              M.init_sreg_state_zeros
          in
          (regfile, sregfile)
      | Error msg ->
          Printf.eprintf "Regfile parse error: %s\n" msg;
          exit 1
  in
  let m_init =
    try
      let memory = M.init_mem_state Z.zero prog in
      let machine = M.init regfile sregfile memory in
      M.check_init_config machine;
      machine
    with M.CheckInitFailed w ->
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
      let module Ui = Interactive_ui.MkUi (M) (Cfg) in
      let show_stack = true in
      let prog_panel_start = ref Z.zero in
      let stk_panel_start = ref stk_addr in
      Ui.render_loop ~show_stack prog_panel_start stk_panel_start m_init
  | Cli_parser.Interpreter_mode ->
      let m_final = M.run m_init in
      let regs = M.get_regfile m_final in
      print_endline "+-----------------------";
      M.RegMap.iter
        (fun r w -> print_endline @@ Pretty_printer.string_of_reg_word r w)
        regs;
      print_endline "+-----------------------";
      let state =
        match M.get_exec_state m_final with
        | M.Running -> "Running"
        | M.Halted -> "Halted"
        | M.Failed -> "Failed"
      in
      Printf.printf "Final execution state: %s\n" state
