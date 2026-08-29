type severity = Error | Warning
type source_location = { source : string option; line : int; column : int; offset : int option }
type t = { severity : severity; location : source_location option; message : string }

let make ?(severity = Error) ?location message = { severity; location; message }
let error ?location message = make ?location message
let warning ?location message = make ~severity:Warning ?location message
let severity diagnostic = diagnostic.severity
let location diagnostic = diagnostic.location
let message diagnostic = diagnostic.message

let to_string diagnostic =
  match diagnostic.location with
  | None -> diagnostic.message
  | Some location ->
      let source = Option.value location.source ~default:"<input>" in
      Printf.sprintf "%s:%d:%d: %s" source location.line location.column diagnostic.message

let pp formatter diagnostic = Format.pp_print_string formatter (to_string diagnostic)
