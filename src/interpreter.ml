open Cerise

let read_file (kind : string) (filename : string) : (string, string) result =
  try Ok (In_channel.with_open_bin filename In_channel.input_all)
  with Sys_error message -> Error (Printf.sprintf "%s file %s: %s" kind filename message)

let print_diagnostics (diagnostics : Diagnostic.t list) : unit =
  List.iter
    (fun diagnostic -> prerr_endline (Diagnostic.to_string diagnostic))
    diagnostics

let exit_with_error (message : string) : 'a =
  prerr_endline message;
  exit 1

let () =
  let arguments = Cli_parser.parse_arguments () in
  match read_file "Program" arguments.program_filename with
  | Error message -> exit_with_error message
  | Ok source -> (
      let regfile =
        match arguments.regfile_filename with
        | None -> Ok None
        | Some filename -> Result.map Option.some (read_file "Regfile" filename)
      in
      match regfile with
      | Error message -> exit_with_error message
      | Ok regfile -> (
          match
            Machine_session.create_with_filenames ~source_filename:arguments.program_filename
              ~regfile_filename:arguments.regfile_filename ~backend:arguments.backend
              ~config:arguments.config ~source ~regfile
          with
          | Error diagnostics ->
              print_diagnostics diagnostics;
              exit 1
          | Ok session ->
              match arguments.mode with
              | Cli_parser.Interpreter_mode -> Interpreter_ui.interpreter session
              | Interactive_mode -> Interactive_ui.render_loop session))
