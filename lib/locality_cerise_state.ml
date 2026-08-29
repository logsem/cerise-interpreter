type t = Locality_cerise_machine.t
type status = Locality_cerise_machine.status = Running | Halted | Failed

let status (state : t) = state.status
let registers (state : t) = state.registers
let memory (state : t) = state.memory
