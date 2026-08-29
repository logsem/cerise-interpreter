type t = Ucerise_machine.t
type status = Ucerise_machine.status = Running | Halted | Failed
let status (state:t) = state.status
let registers (state:t) = state.registers
let memory (state:t) = state.memory
let inspect = Ucerise_backend.inspect
let word = Ucerise_backend.view_word
