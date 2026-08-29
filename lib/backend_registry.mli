val default : string
val names : unit -> string list
val find : string -> (module Machine_backend.S) option
