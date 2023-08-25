(* TODO test of the CLI *)
open Libmachine
open Misc

type cli_mode = Interactive_mode | Interpreter_mode

(** Initialize the Cerise version and returns
    (mode, program_filename, register_filename, size_mem) *)
let parse_arguments () : (cli_mode * string * string)
  =
  let usage_msg =
    "interpreter [-I] [--interactive] [--version version] [--locality locality] [--sealing | --no-sealing]  [--stack | --no-stack] [--uperms | --no-uperms] [--mem-size size] <file>"
  in
  let interactive_option = ref false in
  let version_option = ref "default" in
  let stack_option = ref false in
  let no_stack_option = ref false in
  let sealing_option = ref false in
  let no_sealing_option = ref false in
  let uperms_option = ref false in
  let no_uperms_option = ref false in
  let locality_option = ref "" in
  let mem_size_option = ref "" in
  let regfile_name_option = ref "" in
  let input_files = ref [] in

  let anon_fun filename = input_files := filename :: !input_files in
  let speclist =
    [
      ("--interactive", Arg.Set interactive_option, "Interactive mode of the interpreter");
      ("-I", Arg.Set interactive_option, "Interactive mode of the interpreter");
      ("--version", Arg.Set_string version_option, "Version Cerise: default, vanilla, ucerise, mcerise, seal_cerise, custom");
      ("--sealing", Arg.Set sealing_option, "Enable the seals");
      ("--no-sealing", Arg.Set no_sealing_option, "Disable the seals");
      ("--stack", Arg.Set stack_option, "Enable the stack");
      ("--no-stack", Arg.Set no_stack_option, "Disable the stack");
      ("--uperms", Arg.Set uperms_option, "Enable the U-permission");
      ("--no-uperms", Arg.Set no_uperms_option, "Disable the U-permission");
      ("--locality", Arg.Set_string locality_option, "Choose the minimum locality: Global, Local, Directed");
      ("--mem-size", Arg.Set_string mem_size_option, "Size of the memory, Inf or integer");
      ("--regfile", Arg.Set_string regfile_name_option, "Initial state of the registers");
    ] in
  Arg.parse speclist anon_fun usage_msg;

  let mode = if !interactive_option then Interactive_mode else Interpreter_mode in

  (* Construct the configuration *)
  let _ =
    match !version_option with
    | "default" -> (Parameters.flags := Parameters.full_cerise)
    | "vanilla" -> (Parameters.flags := Parameters.vanilla_cerise)
    | "ucerise" -> (Parameters.flags := Parameters.stack_cerise)
    | "mcerise" -> (Parameters.flags := Parameters.mcerise)
    | "seal_cerise" -> (Parameters.flags := Parameters.sealing_cerise)
    | "custom" -> (Parameters.flags := Parameters.custom_cerise)
    | _ -> raise @@ Arg.Help
        "The --version option requires one of the following values: default, vanilla, ucerise, mcerise, custom"
  in

  let _ =
    match !stack_option, !no_stack_option with
    | (false, false) -> ()
    | (true, false) -> Parameters.set_stack true
    | (false, true) -> Parameters.set_stack false
    | (true, true) -> raise @@ Arg.Help "Option --stack and --no-stack cannot be used together."
  in

  let _ =
    match !sealing_option, !no_sealing_option with
    | (false, false) -> ()
    | (true, false) -> Parameters.set_sealing true
    | (false, true) -> Parameters.set_sealing false
    | (true, true) -> raise @@ Arg.Help "Option --sealing and --no-sealing cannot be used together."
  in

  let _ =
    match !uperms_option, !no_uperms_option with
    | (false, false) -> ()
    | (true, false) -> Parameters.set_uperms true
    | (false, true) -> Parameters.set_uperms false
    | (true, true) -> raise @@ Arg.Help "Option --uperms and --no-uperms cannot be used together."
  in

  let _ =
    match !locality_option with
    | "" -> ()
    | "Directed" -> Parameters.set_locality Directed
    | "Local" -> Parameters.set_locality Local
    | "Global" -> Parameters.set_locality Global
    | _ -> raise @@ Arg.Help "The --locality option requires one of the following values: Global, Local or Directed"
  in

  let _ =
      match !mem_size_option with
       | "inf"
       | "Inf" -> Parameters.set_max_addr Infinite_z.Inf
       | "" -> ()
       | s ->
         let n = (Z.of_string s) in
         if Z.(n < ~$0)
         then (Printf.eprintf "Size of memory must be positive (%s)" s; exit 1)
         else Parameters.set_max_addr (Infinite_z.Int n)
  in

  let filename_prog =
    match !input_files with
    | [filename] -> filename
    | _ ->
      Printf.eprintf "%s\n" usage_msg;
      exit 1
  in

  (mode, filename_prog, !regfile_name_option)
