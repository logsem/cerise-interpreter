type t = Machine.t
type status = Machine.status = Running | Halted | Failed
let status (state:t) = state.status
let registers (state:t) = state.registers
let memory (state:t) = state.memory
let inspect = Backend.inspect
let word = Backend.view_word
