type entry = { requested_name : string; backend : (module Machine_backend.S) }

let vanilla = (module Vanilla.Backend : Machine_backend.S)
let locality_cerise = (module Locality_cerise.Backend : Machine_backend.S)
let ucerise = (module Ucerise.Backend : Machine_backend.S)
let mcerise = (module Mcerise.Backend : Machine_backend.S)
let cerisier = (module Cerisier.Backend : Machine_backend.S)
let griotte = (module Griotte.Backend : Machine_backend.S)
let griotte_extracted = (module Griotte_extracted.Backend : Machine_backend.S)

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

let default = "vanilla"
let names () = List.map (fun entry -> entry.requested_name) backends

let find name =
  List.find_map
    (fun entry -> if String.equal name entry.requested_name then Some entry.backend else None)
    backends
