type status = Running | Halted | Failed
type register_bank = General | System

module Register_id = struct
  type t = { bank : register_bank; key : string }

  let compare left right =
    match Stdlib.compare left.bank right.bank with
    | 0 -> String.compare left.key right.key
    | ordering -> ordering

  let equal left right = compare left right = 0
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
  fingerprint : string;
  kind : semantic_kind;
  integer : Z.t option;
  capability : capability option;
  sealing : sealing option;
  annotations : (string * string) list;
}

type register = { id : register_id; label : string; role : register_role; word : word }
type memory_cell = { address : Z.t; word : word }
type missing_cell = Unmapped | Default of word

type t = {
  backend_name : string;
  status : status;
  address_limit : Z.t;
  pc : Z.t option;
  registers : register list;
  memory : memory_cell list;
  missing_cell : missing_cell;
}

let find_register id view =
  List.find_opt (fun register -> Register_id.equal id register.id) view.registers

let memory_at address view =
  if Z.sign address < 0 || Z.compare address view.address_limit >= 0 then None
  else
    match List.find_opt (fun cell -> Z.equal address cell.address) view.memory with
    | Some cell -> Some cell.word
    | None -> ( match view.missing_cell with Unmapped -> None | Default word -> Some word)
