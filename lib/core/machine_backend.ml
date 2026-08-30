type execution_error = Stopped of Machine_view.status | Backend_error of string

let execution_error_message (matched_value : execution_error) : string = match matched_value with
  | Stopped Machine_view.Running -> "machine unexpectedly refused a step while running"
  | Stopped Machine_view.Halted -> "machine is halted"
  | Stopped Machine_view.Failed -> "machine has failed"
  | Backend_error message -> message

module type S = sig
  val name : string
  val description : string

  type asm_program
  type asm_regfile
  type asm_word
  type state

  val parse_program : ?filename:string -> string -> (asm_program, Diagnostic.t list) result
  val parse_regfile : ?filename:string -> string -> (asm_regfile, Diagnostic.t list) result
  val parse_word : ?filename:string -> string -> (asm_word, Diagnostic.t list) result
  val init : Runtime_config.t -> asm_program -> asm_regfile option -> (state, Diagnostic.t list) result
  val step : state -> (state, execution_error) result
  val step_n : int -> state -> (state, execution_error) result
  val inspect : state -> Machine_view.t

  val set_register :
    Machine_view.Register_id.t -> asm_word -> state -> (state, Diagnostic.t list) result

  val set_memory : Z.t -> asm_word -> state -> (state, Diagnostic.t list) result
end
