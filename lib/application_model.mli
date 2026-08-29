type t

val create : Machine_session.t -> t
val session : t -> Machine_session.t
val view : t -> Machine_view.t
val history_length : t -> int
val undo : t -> t

val step : t -> (t, Machine_session.execution_error) result
val step_n : int -> t -> (t, Machine_session.execution_error) result
val set_register_text : Machine_view.register_id -> string -> t -> (t, Diagnostic.t list) result
val set_memory_text : Z.t -> string -> t -> (t, Diagnostic.t list) result

val primary_start : t -> Z.t
val secondary_start : t -> Z.t
val move_primary : Z.t -> t -> t
val move_secondary : Z.t -> t -> t
val page_primary : int -> int -> t -> t
val page_secondary : int -> int -> t -> t
val follow_primary : t -> t
val follow_secondary : t -> t

(** [register_rows, primary_memory_rows, secondary_memory_rows] that fit alongside one header
    and three panel labels.  A terminal shorter than four rows has no content rows. *)
val row_budget : height:int -> register_count:int -> int * int * int

val capability_registers : t -> Machine_view.register list
val selected_capability : t -> Machine_view.register option
val select_next_capability : t -> t
