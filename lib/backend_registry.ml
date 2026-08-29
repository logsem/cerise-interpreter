type entry = { requested_name : string; backend : (module Machine_backend.S) }

let backends =
  [ { requested_name = "cerise"; backend = (module Interim_legacy_backend : Machine_backend.S) } ]

let default = "cerise"
let names () = List.map (fun entry -> entry.requested_name) backends

let find name =
  List.find_map
    (fun entry -> if String.equal name entry.requested_name then Some entry.backend else None)
    backends
