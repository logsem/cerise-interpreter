type execution_error = Stopped of Machine_view.status | Backend_error of string

val execution_error_message : execution_error -> string

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

  val init :
    Runtime_config.t -> asm_program -> asm_regfile option -> (state, Diagnostic.t list) result

  val step : Runtime_config.t -> state -> (state, execution_error) result
  val step_n : Runtime_config.t -> int -> state -> (state, execution_error) result
  val inspect : Runtime_config.t -> state -> Machine_view.t

  val set_register :
    Runtime_config.t ->
    Machine_view.Register_id.t ->
    asm_word ->
    state ->
    (state, Diagnostic.t list) result

  val set_memory : Runtime_config.t -> Z.t -> asm_word -> state -> (state, Diagnostic.t list) result
end
