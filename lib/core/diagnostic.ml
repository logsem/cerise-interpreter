(** Structured diagnostics retain source coordinates until the presentation boundary. *)

type severity = Error | Warning
type source_location = { source : string option; line : int; column : int; offset : int option }
type t = { severity : severity; location : source_location option; message : string }

let make ?(severity : severity = Error) ?(location : source_location option) (message : string) : t
    =
  { severity; location; message }

let error ?(location : source_location option) (message : string) : t = make ?location message

let warning ?(location : source_location option) (message : string) : t =
  make ~severity:Warning ?location message

let severity (diagnostic : t) : severity = diagnostic.severity
let location (diagnostic : t) : source_location option = diagnostic.location
let message (diagnostic : t) : string = diagnostic.message

let to_string (diagnostic : t) : string =
  match diagnostic.location with
  | None -> diagnostic.message
  | Some location ->
      let source = Option.value location.source ~default:"<input>" in
      Printf.sprintf "%s:%d:%d: %s" source location.line location.column diagnostic.message

let pp (formatter : Format.formatter) (diagnostic : t) : unit =
  Format.pp_print_string formatter (to_string diagnostic)
