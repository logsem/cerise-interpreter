type machineFlags = {
  version : string; (* Name of the version of Cerise *)
  max_addr : Z.t; (* Maximum memory address *)
  max_otype : Z.t; (* Maximum otype *)
}

let max_addr = Z.of_int (Int32.to_int Int32.max_int / 4096) (* (2^31 - 1) / 2^12 *)
let max_otype = Z.of_int (Int32.to_int Int32.max_int / 4096) (* (2^31 - 1) / 2^12 *)
let vanilla_cerise : machineFlags = { version = "vanilla-cerise"; max_addr; max_otype }
let full_cerise : machineFlags = { version = "cerise"; max_addr; max_otype }
let griotte : machineFlags = { version = "griotte"; max_addr; max_otype = Z.of_int 15 }
let default : machineFlags = griotte
let flags : machineFlags ref = ref default

exception MalformedConfiguration of string

let set_max_addr a =
  flags := { version = !flags.version; max_addr = a; max_otype = !flags.max_otype }

let get_max_addr () : Z.t = !flags.max_addr

let set_max_otype o =
  flags := { version = !flags.version; max_otype = o; max_addr = !flags.max_addr }

let get_max_otype () : Z.t = !flags.max_otype
