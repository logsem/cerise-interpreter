(** Lookup table from stable CLI spellings to sealed backend modules. Registry spellings are a
    user-facing interface and may include compatibility aliases for one canonical backend. *)

val default_backend_name : string
(** Canonical spelling selected when the CLI receives no explicit backend. *)

val available_backend_names : unit -> string list
(** All accepted CLI spellings in diagnostic/display order, including compatibility aliases. *)

val find_backend : string -> (module Machine_backend.S) option
(** Resolve an exact CLI spelling without exposing the selected backend's private types. *)
