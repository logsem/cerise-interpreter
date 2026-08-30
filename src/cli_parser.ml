(* TODO test of the CLI *)
open Cerise

type cli_mode = Interactive_mode | Interpreter_mode

type arguments = {
  mode : cli_mode;
  backend : string;
  program_filename : string;
  regfile_filename : string option;
  config : Runtime_config.t;
}

let of_options (options : Cli_options.t) : arguments =
  {
    mode = (match options.mode with Cli_options.Interactive -> Interactive_mode | Noninteractive -> Interpreter_mode);
    backend = options.backend;
    program_filename = options.program_filename;
    regfile_filename = options.regfile_filename;
    config = options.config;
  }

let parse_argv (argv : string array) : (arguments, string) result = Result.map of_options (Cli_options.parse argv)

let parse_arguments (() : unit) : arguments =
  if Array.exists (fun argument -> String.equal argument "--help" || String.equal argument "-help") Sys.argv
  then (
    print_endline Cli_options.usage;
    exit 0)
  else
    match parse_argv Sys.argv with
    | Ok arguments -> arguments
    | Error message ->
        prerr_endline message;
        exit 2
