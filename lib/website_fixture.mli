(** A minimal model of the interpreter APIs used by the website.  It deliberately retains prior
    immutable sessions rather than serializing snapshots. *)
type t

val create : backend:string -> config:Runtime_config.t -> source:string -> regfile:string option -> (t, Diagnostic.t list) result
val session : t -> Machine_session.t
val view : t -> Machine_view.t
val undo : t -> t
val step : t -> (t, Machine_session.execution_error) result
val edit_register : Machine_view.register_id -> string -> t -> (t, Diagnostic.t list) result
val edit_memory : Z.t -> string -> t -> (t, Diagnostic.t list) result
val selected_capability : t -> Machine_view.register option
val select_next_capability : t -> t
val memory_start : t -> Z.t
val navigate_memory : Z.t -> t -> t
val follow_selected_capability : t -> t
