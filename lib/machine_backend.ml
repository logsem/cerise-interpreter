type execution_error = Stopped of Machine_view.status | Backend_error of string

let execution_error_message = function
  | Stopped Machine_view.Running -> "machine unexpectedly refused a step while running"
  | Stopped Machine_view.Halted -> "machine is halted"
  | Stopped Machine_view.Failed -> "machine has failed"
  | Backend_error message -> message

module type S = sig
  val name : string
  val description : string

  type program
  type regfile
  type word
  type state

  val parse_program : ?filename:string -> string -> (program, Diagnostic.t list) result
  val parse_regfile : ?filename:string -> string -> (regfile, Diagnostic.t list) result
  val parse_word : ?filename:string -> string -> (word, Diagnostic.t list) result
  val init : Runtime_config.t -> program -> regfile option -> (state, Diagnostic.t list) result
  val step : state -> (state, execution_error) result
  val step_n : int -> state -> (state, execution_error) result
  val inspect : state -> Machine_view.t

  val set_register :
    Machine_view.Register_id.t -> word -> state -> (state, Diagnostic.t list) result

  val set_memory : Z.t -> word -> state -> (state, Diagnostic.t list) result
end
