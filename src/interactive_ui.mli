(** Stateful terminal controller with pure snapshot helpers for its rendered machine view. *)

type t
type side = Left | Right

type event =
  | Quit
  | Step
  | Step_ten
  | Undo
  | Move_primary of Z.t
  | Move_secondary of Z.t
  | Page_primary of int
  | Page_secondary of int
  | Follow_primary
  | Follow_secondary
  | Toggle_secondary
  | Cycle_capability
  | Resize of int * int

val create : Cerise.Machine_session.t -> t
val application : t -> Application_model.t
val transition : rows:int -> event -> t -> t option
val render : width:int -> height:int -> t -> Notty.I.t
val snapshot : width:int -> height:int -> t -> string
val ansi_snapshot : width:int -> height:int -> t -> string

val word_snapshot :
  address_limit:Z.t -> width:int -> side:side -> Cerise.Machine_view.word -> string

val word_ansi_snapshot :
  address_limit:Z.t -> width:int -> side:side -> Cerise.Machine_view.word -> string

val scroll_event :
  width:int -> height:int -> x:int -> ctrl:bool -> direction:[ `Up | `Down ] -> t -> event

val render_loop : Cerise.Machine_session.t -> unit
