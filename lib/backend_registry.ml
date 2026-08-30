type entry = { requested_name : string; backend : (module Machine_backend.S) }

let vanilla = (module Backends.Vanilla.Backend : Machine_backend.S)
let locality_cerise = (module Backends.Locality_cerise.Backend : Machine_backend.S)
let ucerise = (module Backends.Ucerise.Backend : Machine_backend.S)
let mcerise = (module Backends.Mcerise.Backend : Machine_backend.S)
let cerisier = (module Backends.Cerisier.Backend : Machine_backend.S)
let griotte = (module Backends.Griotte.Backend : Machine_backend.S)
let griotte_extracted = (module Backends.Griotte_extracted.Backend : Machine_backend.S)

let backends =
  [
    { requested_name = "vanilla"; backend = vanilla };
    { requested_name = "cerise"; backend = vanilla };
    { requested_name = "locality-cerise"; backend = locality_cerise };
    { requested_name = "ucerise"; backend = ucerise };
    { requested_name = "mcerise"; backend = mcerise };
    { requested_name = "cerisier"; backend = cerisier };
    { requested_name = "griotte"; backend = griotte };
    { requested_name = "griotte-extracted"; backend = griotte_extracted };
  ]

let default_backend_name = "vanilla"
let available_backend_names (() : unit) : string list = List.map (fun entry -> entry.requested_name) backends

let find_backend (name : string) : (module Machine_backend.S) option =
  List.find_map
    (fun entry -> if String.equal name entry.requested_name then Some entry.backend else None)
    backends
