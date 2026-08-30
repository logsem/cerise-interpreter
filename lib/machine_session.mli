type t
type execution_error = Machine_backend.execution_error

type stop_reason =
  | Halted
  | Failed
  | Breakpoint of Z.t
  | Step_limit
  | Execution_error of execution_error

type run_result = { session : t; reason : stop_reason; steps : int }

(** A session is the sole long-lived owner of its immutable runtime configuration. Backend machine
    states contain only dynamic semantic state, and every session operation automatically reuses the
    configuration supplied at creation. *)

val create :
  backend:string ->
  config:Runtime_config.t ->
  source:string ->
  regfile:string option ->
  (t, Diagnostic.t list) result

val create_with_filenames :
  source_filename:string ->
  regfile_filename:string option ->
  backend:string ->
  config:Runtime_config.t ->
  source:string ->
  regfile:string option ->
  (t, Diagnostic.t list) result
(** Like [create], but attaches filenames to parser diagnostics. *)

val backend_name : t -> string
val view : t -> Machine_view.t

val control : t -> Machine_backend.control
(** Return the current status and program counter without constructing a full [Machine_view.t]. *)

val step : t -> (t, execution_error) result
val step_n : int -> t -> (t, execution_error) result

val run : ?breakpoints:Z.t list -> ?max_steps:int -> t -> run_result
(** Run until the machine stops, reaches a breakpoint before executing that address, or consumes
    [max_steps]. The execution loop reads only [control], avoiding full view construction per
    instruction. A zero limit performs no steps, while a negative limit is an [Execution_error]. *)

val set_register_text : Machine_view.Register_id.t -> string -> t -> (t, Diagnostic.t list) result
val set_memory_text : Z.t -> string -> t -> (t, Diagnostic.t list) result
