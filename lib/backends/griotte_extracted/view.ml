open Ast

let permission_parts ((rx, write, deep_local, deep_read_only) : rx_permission * write_permission * deep_local_permission *
deep_read_only_permission) : string list =
  [
    Printer.rx_permission rx;
    Printer.write_permission write;
    Printer.deep_local_permission deep_local;
    Printer.deep_read_only_permission deep_read_only;
  ]

let capability (permission : rx_permission * write_permission * deep_local_permission *
deep_read_only_permission) (locality : locality) (base : Z.t) (limit : Z.t) (cursor : Z.t) : Machine_view.capability option =
  Some
    {
      Machine_view.base;
      limit;
      cursor;
      permissions = permission_parts permission;
      locality = Some (Printer.locality locality);
    }

let sealing ?object_type:(object_type : Z.t option) ~sealed:(sealed : bool)
    ((can_seal, can_unseal) : bool * bool) : Machine_view.sealing option =
  Some
    {
      Machine_view.object_type;
      can_seal = Some can_seal;
      can_unseal = Some can_unseal;
      is_sealed = sealed;
    }

let word (word : word) : Machine_view.word =
  let edit_text = Printer.word word in
  let fingerprint = Digest.to_hex (Digest.string edit_text) in
  let decoded_instruction =
    match word with
    | I encoded ->
        Result.to_option (Codec.decode encoded)
        |> Option.map Printer.instruction
    | _ -> None
  in
  let base (kind : Machine_view.semantic_kind) (integer : Z.t option) (capability : Machine_view.capability option) (seal_range : Machine_view.seal_range option) (sealing : Machine_view.sealing option) (annotations : (string * string) list) : Machine_view.word =
    {
      Machine_view.edit_text;
      short_text = edit_text;
      detail_text = edit_text;
      decoded_instruction;
      fingerprint;
      kind;
      integer;
      capability;
      seal_range;
      sealing;
      annotations;
    }
  in
  match word with
  | I value -> base Integer (Some value) None None None []
  | Sealable (Cap (permission, locality, first, last, cursor)) ->
      base Capability None (capability permission locality first last cursor) None None []
  | Sentry (permission, locality, first, last, cursor) ->
      base Sentry None
        (capability permission locality first last cursor)
        None None
        [ ("entry", "sentry") ]
  | Sealable (SealRange (permission, locality, first, last, cursor)) ->
      base Seal_range None None
        (Some { Machine_view.base = first; limit = last; cursor; locality = Some (Printer.locality locality) })
        (sealing ~sealed:false permission)
        [
          ("locality", Printer.locality locality);
          ("base", Z.to_string first);
          ("limit", Z.to_string last);
          ("cursor", Z.to_string cursor);
        ]
  | Sealed (object_type, Cap (permission, locality, first, last, cursor)) ->
      base Sealed_capability None
        (capability permission locality first last cursor)
        None
        (Some
           { object_type = Some object_type; can_seal = None; can_unseal = None; is_sealed = true })
        []
  | Sealed (object_type, SealRange (permission, locality, first, last, cursor)) ->
      base Sealed_capability None None
        (Some { Machine_view.base = first; limit = last; cursor; locality = Some (Printer.locality locality) })
        (sealing ~object_type ~sealed:true permission)
        [
          ("locality", Printer.locality locality);
          ("base", Z.to_string first);
          ("limit", Z.to_string last);
          ("cursor", Z.to_string cursor);
        ]

let register_description (register : register) : Machine_view.register_id * string * Machine_view.register_role =
  let label = Printer.register register in
  match register with
  | PC ->
      ({ Machine_view.Register_id.bank = System; key = "pc" }, label, Machine_view.Program_counter)
  | Reg 0 ->
      ( { Machine_view.Register_id.bank = General; key = label },
        label,
        Machine_view.Backend_specific "null-capability" )
  | Reg 2 ->
      ({ Machine_view.Register_id.bank = General; key = label }, label, Machine_view.Stack_pointer)
  | Reg _ -> ({ Machine_view.Register_id.bank = General; key = label }, label, Machine_view.General)

let inspect ~backend_name:(backend_name : string) (state : State.t) : Machine_view.t =
  let registers =
    State.RegMap.bindings state.State.registers
    |> List.map (fun (register, value) ->
        let id, label, role = register_description register in
        { Machine_view.id; label; role; word = word value })
  in
  let registers =
    registers
    @ (State.SRegMap.bindings state.State.system_registers
      |> List.map (fun (register, value) ->
          let label = Printer.system_register register in
          {
            Machine_view.id = { bank = System; key = label };
            label;
            role = Backend_specific "machine-trusted-domain-capability";
            word = word value;
          }))
  in
  let memory =
    State.MemMap.bindings state.State.memory
    |> List.map (fun (address, value) -> { Machine_view.address; word = word value })
  in
  let pc =
    match State.read_register PC state with
    | Sealable (Cap (_, _, _, _, cursor)) -> Some cursor
    | _ -> None
  in
  {
    Machine_view.backend_name;
    status =
      (match state.State.status with Running -> Running | Halted -> Halted | Failed -> Failed);
    address_limit = Runtime_config.max_addr state.config;
    pc;
    registers;
    memory;
    missing_cell = Default (word (I Z.zero));
  }

let register_of_id (id : Machine_view.Register_id.t) : (register, Diagnostic.t list) result =
  match (id.bank, id.key) with
  | System, "pc" -> Ok PC
  | General, name -> (
      match Asm_ir.parse_register_name name with
      | Some register -> Ok register
      | None -> Error [ Diagnostic.error (Printf.sprintf "Unknown Griotte register %S." name) ])
  | _, name ->
      Error [ Diagnostic.error (Printf.sprintf "Register %S does not belong to that bank." name) ]
