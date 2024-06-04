(* TODO test of the CLI *)

type cli_mode = Interactive_mode | Interpreter_mode

(** Initialize the Cerise version and returns
    (mode, program_filename, register_filename, size_mem) *)
let parse_arguments () : cli_mode * string * string =
  let usage_msg = "interpreter [-I] [--interactive] [--version version] [--mem-size size] <file>" in
  let interactive_option = ref false in
  let version_option = ref "default" in
  let mem_size_option = ref "" in
  let regfile_name_option = ref "" in
  let input_files = ref [] in

  let anon_fun filename = input_files := filename :: !input_files in
  let speclist =
    [
      ("--interactive", Arg.Set interactive_option, "Interactive mode of the interpreter");
      ("-I", Arg.Set interactive_option, "Interactive mode of the interpreter");
      ("--version", Arg.Set_string version_option, "Version Cerise: default");
      ("--mem-size", Arg.Set_string mem_size_option, "Size of the memory, integer");
      ("--regfile", Arg.Set_string regfile_name_option, "Initial state of the registers");
    ]
  in
  Arg.parse speclist anon_fun usage_msg;

  let mode = if !interactive_option then Interactive_mode else Interpreter_mode in

  (* Construct the configuration *)
  let _ =
    match !version_option with
    | "default" -> Parameters.flags := Parameters.full_cerise
    | _ -> raise @@ Arg.Help "The --version option requires one of the following values: default"
  in

  let _ =
    match !mem_size_option with
    | "" -> ()
    | s ->
        let n = Z.of_string s in
        if Z.(n < ~$0) then (
          Printf.eprintf "Size of memory must be positive (%s)" s;
          exit 1)
        else Parameters.set_max_addr n
  in

  let filename_prog =
    match !input_files with
    | [ filename ] -> filename
    | _ ->
        Printf.eprintf "%s\n" usage_msg;
        exit 1
  in

  (mode, filename_prog, !regfile_name_option)
