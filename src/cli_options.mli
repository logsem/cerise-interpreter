(** Reusable command-line parsing and validation, without process exits. *)

type mode = Interactive | Noninteractive

type t = {
  mode : mode;
  backend : string;
  program_filename : string;
  regfile_filename : string option;
  config : Cerise.Runtime_config.t;
}

val parse : string array -> (t, string) result
val usage : string
