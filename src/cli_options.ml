(* Command-line option parsing translates [Arg] callbacks into one validated
   configuration. It deliberately reports errors as values so the executable can
   apply its established help and exit-code policy. *)

type mode = Interactive | Noninteractive

type t = {
  mode : mode;
  backend : string;
  program_filename : string;
  regfile_filename : string option;
  config : Cerise.Runtime_config.t;
}

type parser_state = {
  interactive : bool ref;
  backend : string ref;
  memory_size : string option ref;
  regfile : string option ref;
  programs : string list ref;
}

let usage : string =
  "Usage: cerise-interpreter [-I|--interactive] [--backend NAME] [--mem-size SIZE]"
  ^ " \\\n   [--regfile FILE] <program>"

let available_backends (() : unit) : string =
  String.concat ", " (Cerise.Backend_registry.available_backend_names ())

let create_parser_state (() : unit) : parser_state =
  {
    interactive = ref false;
    backend = ref Cerise.Backend_registry.default_backend_name;
    memory_size = ref None;
    regfile = ref None;
    programs = ref [];
  }

let option_specifications (state : parser_state) : (string * Arg.spec * string) list =
  [
    ("--interactive", Arg.Set state.interactive, "Run the terminal UI");
    ("-I", Arg.Set state.interactive, "Run the terminal UI");
    ("--backend", Arg.Set_string state.backend, "Select a registered backend");
    ( "--mem-size",
      Arg.String (fun size -> state.memory_size := Some size),
      "Positive address-space size" );
    ( "--regfile",
      Arg.String (fun filename -> state.regfile := Some filename),
      "Initial register file" );
  ]

let parse_runtime_config (memory_size : string option) : (Cerise.Runtime_config.t, string) result =
  match memory_size with
  | None -> Ok Cerise.Runtime_config.default
  | Some text -> (
      try
        let max_addr = Z.of_string text in
        if Z.sign max_addr <= 0 then Error "--mem-size requires a positive integer."
        else Ok (Cerise.Runtime_config.create ~max_addr ())
      with Invalid_argument _ -> Error "--mem-size requires a positive integer.")

let validate (state : parser_state) : (t, string) result =
  if Cerise.Backend_registry.find_backend !(state.backend) = None then
    Error
      (Printf.sprintf "Unknown backend %S. Available backends: %s." !(state.backend)
         (available_backends ()))
  else
    match (parse_runtime_config !(state.memory_size), List.rev !(state.programs)) with
    | Error message, _ -> Error message
    | Ok _, [] | Ok _, _ :: _ :: _ -> Error (usage ^ "\nExactly one program file is required.")
    | Ok config, [ program_filename ] ->
        Ok
          {
            mode = (if !(state.interactive) then Interactive else Noninteractive);
            backend = !(state.backend);
            program_filename;
            regfile_filename = !(state.regfile);
            config;
          }

let parse (argv : string array) : (t, string) result =
  let state = create_parser_state () in
  try
    let current_argument = ref 0 in
    Arg.parse_argv ~current:current_argument argv (option_specifications state)
      (fun filename -> state.programs := filename :: !(state.programs))
      usage;
    validate state
  with
  | Arg.Bad message -> Error (String.trim message)
  | Arg.Help message -> Error message
