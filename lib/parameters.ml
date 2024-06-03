type machineFlags = {
  version : string; (* Name of the version of Cerise *)
  max_addr : Z.t; (* Maximum memory address (can be infinite) *)
}

let max_addr = Z.of_int (Int32.to_int Int32.max_int / 4096) (* (2^31 - 1) / 2^12 *)

let vanilla_cerise : machineFlags =
  {
    version = "vanilla-cerise";
    max_addr = max_addr;
  }

let full_cerise : machineFlags =
  {
    version = "cerise";
    max_addr = max_addr;
  }

let griotte : machineFlags =
  {
    version = "griotte";
    max_addr = max_addr;
  }

let default : machineFlags = griotte
let flags : machineFlags ref = ref default

exception MalformedConfiguration of string

let set_max_addr a =
  flags :=
    {
      max_addr = a;
      version = !flags.version
    }

let get_max_addr () : Z.t = !flags.max_addr
