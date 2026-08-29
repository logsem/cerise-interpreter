type t
(** Immutable values that affect a machine instance but do not select an architecture. Addresses are
    interpreted as an exclusive upper bound. *)

val default : t

val create : ?max_addr:Z.t -> ?stack_addr:Z.t -> unit -> t
(** [create ()] uses the same address limit as the legacy interpreter. If no [stack_addr] is
    supplied it is placed halfway through the address space. Invalid bounds raise
    [Invalid_argument]. *)

val max_addr : t -> Z.t
val stack_addr : t -> Z.t
