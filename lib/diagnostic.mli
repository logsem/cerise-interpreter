type severity = Error | Warning
type source_location = { source : string option; line : int; column : int; offset : int option }
type t

val make : ?severity:severity -> ?location:source_location -> string -> t
val error : ?location:source_location -> string -> t
val warning : ?location:source_location -> string -> t
val severity : t -> severity
val location : t -> source_location option
val message : t -> string
val to_string : t -> string
val pp : Format.formatter -> t -> unit
