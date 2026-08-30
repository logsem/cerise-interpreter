(** Stable public façade for backends and shared interpreter services. *)

module Vanilla = Backends.Vanilla
module Locality_cerise = Backends.Locality_cerise
module Ucerise = Backends.Ucerise
module Mcerise = Backends.Mcerise
module Cerisier = Backends.Cerisier
module Griotte = Backends.Griotte
module Griotte_extracted = Backends.Griotte_extracted
module Machine = Backends.Vanilla.Machine
module Runtime_config = Runtime_config
module Diagnostic = Diagnostic
module Instruction_codec = Instruction_codec
module Machine_view = Machine_view
module Machine_backend = Machine_backend
module Machine_session = Machine_session
module Backend_registry = Backend_registry
