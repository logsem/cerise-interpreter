type t = Machine.t
type status = Machine.status = Running | Halted | Failed

let status (state : t) = state.status
let registers (state : t) = state.registers
let system_registers (state : t) = state.system_registers
let memory (state : t) = state.memory
let inspect = Backend.inspect
