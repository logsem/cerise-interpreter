type t = Mcerise_machine.t
type status = Mcerise_machine.status = Running | Halted | Failed
let status (state:t) = state.status
let registers (state:t) = state.registers
let memory (state:t) = state.memory
let inspect = Mcerise_backend.inspect
let word = Mcerise_backend.view_word
