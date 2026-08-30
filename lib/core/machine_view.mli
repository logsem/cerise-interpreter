(** A pure-data snapshot of a backend state. This module deliberately has no dependency on a backend
    implementation or a terminal library. *)

type status = Running | Halted | Failed
type register_bank = General | System

module Register_id : sig
  type t = { bank : register_bank; key : string }

  val compare : t -> t -> int
  val equal : t -> t -> bool
end

type register_id = Register_id.t
type register_role = Program_counter | Stack_pointer | General | Backend_specific of string
type semantic_kind = Integer | Capability | Sealed_capability | Sentry | Seal_range | Opaque

type capability = {
  base : Z.t;
  limit : Z.t;
  cursor : Z.t;
  permissions : string list;
  locality : string option;
}

type seal_range = {
  base : Z.t;
  limit : Z.t;
  cursor : Z.t;
  locality : string option;
}

type sealing = {
  object_type : Z.t option;
  can_seal : bool option;
  can_unseal : bool option;
  is_sealed : bool;
}

type word = {
  edit_text : string;
  short_text : string;
  detail_text : string;
  (** Human-readable instruction text when this integer decodes in the owning backend. *)
  decoded_instruction : string option;
  fingerprint : string;
  kind : semantic_kind;
  integer : Z.t option;
  capability : capability option;
  seal_range : seal_range option;
  sealing : sealing option;
  annotations : (string * string) list;
}

type register = { id : register_id; label : string; role : register_role; word : word }
type memory_cell = { address : Z.t; word : word }
type missing_cell = Unmapped | Default of word
type enclave_table_entry = { id : Z.t; identity : Z.t }
type enclave_table = { counter : Z.t; entries : enclave_table_entry list }

type t = {
  backend_name : string;
  status : status;
  address_limit : Z.t;
  pc : Z.t option;
  registers : register list;
  enclave_table : enclave_table option;
  memory : memory_cell list;
  missing_cell : missing_cell;
}

val find_register : register_id -> t -> register option
val find_memory_word : Z.t -> t -> word option
