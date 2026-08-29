type t = Vanilla_machine.t
type status = Vanilla_machine.status = Running | Halted | Failed

let status (state : t) = state.status
let registers (state : t) = state.registers
let memory (state : t) = state.memory
