type mode = Interactive | Noninteractive

type t = {
  mode : mode;
  backend : string;
  program_filename : string;
  regfile_filename : string option;
  config : Runtime_config.t;
}

let usage =
  "Usage: cerise-interpreter [-I|--interactive] [--backend NAME] [--mem-size SIZE] \
   [--regfile FILE] <program>"

let available_backends () = String.concat ", " (Backend_registry.names ())

let parse argv =
  let interactive = ref false in
  let backend = ref Backend_registry.default in
  let mem_size = ref None in
  let regfile = ref None in
  let programs = ref [] in
  let add_program filename = programs := filename :: !programs in
  let specifications =
    [
      ("--interactive", Arg.Set interactive, "Run the terminal UI");
      ("-I", Arg.Set interactive, "Run the terminal UI");
      ("--backend", Arg.Set_string backend, "Select a registered backend");
      ("--mem-size", Arg.String (fun size -> mem_size := Some size), "Positive address-space size");
      ("--regfile", Arg.String (fun filename -> regfile := Some filename), "Initial register file");
    ]
  in
  try
    let current = ref 0 in
    Arg.parse_argv ~current argv specifications add_program usage;
    if Backend_registry.find !backend = None then
      Error
        (Printf.sprintf "Unknown backend %S. Available backends: %s." !backend
           (available_backends ()))
    else
      let config =
        match !mem_size with
        | None -> Ok Runtime_config.default
        | Some text -> (
            try
              let max_addr = Z.of_string text in
              if Z.sign max_addr <= 0 then Error "--mem-size requires a positive integer."
              else Ok (Runtime_config.create ~max_addr ())
            with Invalid_argument _ -> Error "--mem-size requires a positive integer.")
      in
      match (config, List.rev !programs) with
      | Error message, _ -> Error message
      | Ok _, [] | Ok _, _ :: _ :: _ -> Error (usage ^ "\nExactly one program file is required.")
      | Ok config, [ program_filename ] ->
          Ok
            {
              mode = if !interactive then Interactive else Noninteractive;
              backend = !backend;
              program_filename;
              regfile_filename = !regfile;
              config;
            }
  with
  | Arg.Bad message -> Error (String.trim message)
  | Arg.Help message -> Error message
