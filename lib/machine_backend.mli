type execution_error = Stopped of Machine_view.status | Backend_error of string

val execution_error_message : execution_error -> string

module type S = sig
  val name : string
  val description : string

  type program
  type regfile
  type word
  type state

  val lower_program : Surface_ast.program -> (program, Diagnostic.t list) result
  val lower_regfile : Surface_ast.regfile -> (regfile, Diagnostic.t list) result
  val init : Runtime_config.t -> program -> regfile option -> (state, Diagnostic.t list) result
  val step : state -> (state, execution_error) result
  val step_n : int -> state -> (state, execution_error) result
  val inspect : state -> Machine_view.t
  val parse_word : string -> (word, Diagnostic.t list) result

  val set_register :
    Machine_view.Register_id.t -> word -> state -> (state, Diagnostic.t list) result

  val set_memory : Z.t -> word -> state -> (state, Diagnostic.t list) result
end
