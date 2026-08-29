let backends : (module Machine_backend.S) list =
  [ (module Interim_legacy_backend : Machine_backend.S) ]

let default = Interim_legacy_backend.name
let names () = List.map (fun (module Backend : Machine_backend.S) -> Backend.name) backends

let find name =
  List.find_opt
    (fun (module Backend : Machine_backend.S) -> String.equal name Backend.name)
    backends
