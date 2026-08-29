type mode = Interactive | Noninteractive

type t = {
  mode : mode;
  backend : string;
  program_filename : string;
  regfile_filename : string option;
  config : Runtime_config.t;
}

val parse : string array -> (t, string) result
val usage : string
