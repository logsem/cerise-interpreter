val default_backend_name : string
val available_backend_names : unit -> string list
val find_backend : string -> (module Machine_backend.S) option
