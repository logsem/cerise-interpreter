type t

val create : Cerise.Machine_session.t -> t
val session : t -> Cerise.Machine_session.t
val view : t -> Cerise.Machine_view.t
val history_length : t -> int
val undo : t -> t
val step : t -> (t, Cerise.Machine_session.execution_error) result
val step_n : int -> t -> (t, Cerise.Machine_session.execution_error) result
val set_register_text : Cerise.Machine_view.register_id -> string -> t -> (t, Cerise.Diagnostic.t list) result
val set_memory_text : Z.t -> string -> t -> (t, Cerise.Diagnostic.t list) result
val primary_start : t -> Z.t
val secondary_start : t -> Z.t
val move_primary : Z.t -> t -> t
val move_secondary : Z.t -> t -> t
val page_primary : int -> int -> t -> t
val page_secondary : int -> int -> t -> t
val follow_primary : ?rows:int -> t -> t
val follow_secondary : ?rows:int -> t -> t
val row_budget : height:int -> register_count:int -> int * int * int
val capability_registers : t -> Cerise.Machine_view.register list
val selected_capability : t -> Cerise.Machine_view.register option
val active_stack_pointer : t -> Cerise.Machine_view.register option
val select_next_capability : t -> t
