module Ast = Cerise_internal.Ast
module Machine = Cerise_internal.Machine
module Irreg = Cerise_internal.Irreg
module Parameters = Cerise_internal.Parameters
module Program = Cerise_internal.Program

let name = "interim-legacy"
let description = "Interim adapter for the complete legacy Cerise machine"

type program = Ast.t
type regfile = Irreg.t
type word = Irreg.word
type state = { config : Runtime_config.t; machine : Machine.t }

let full_flags config = { Parameters.full_cerise with max_addr = Runtime_config.max_addr config }

let with_flags flags operation =
  let previous = !Parameters.flags in
  Fun.protect
    ~finally:(fun () -> Parameters.flags := previous)
    (fun () ->
      Parameters.flags := flags;
      operation ())

let with_full config operation = with_flags (full_flags config) operation

exception Adapter_error of string

let adapter_error ?location message = Error [ Diagnostic.error ?location message ]
let fail_adapter format = Printf.ksprintf (fun message -> raise (Adapter_error message)) format

let source_location ?filename lexbuf =
  let position = Lexing.lexeme_start_p lexbuf in
  {
    Diagnostic.source = filename;
    line = max 1 position.pos_lnum;
    column = max 1 (position.pos_cnum - position.pos_bol + 1);
    offset = Some (max 0 position.pos_cnum);
  }

let parse_with ?filename source parser =
  let lexbuf = Lexing.from_string source in
  Option.iter (Lexing.set_filename lexbuf) filename;
  match with_flags Parameters.full_cerise (fun () -> parser lexbuf) with
  | Ok value -> Ok value
  | Error message -> Error [ Diagnostic.error ~location:(source_location ?filename lexbuf) message ]

let parse_program ?filename source = parse_with ?filename source Program.parse_prog_from_lexbuf

let parse_regfile ?filename source =
  parse_with ?filename source Program.parse_regfile_ir_from_lexbuf

let parse_word ?filename source =
  match parse_regfile ?filename ("r1 := " ^ source) with
  | Ok [ (_, word) ] -> Ok word
  | Ok _ -> adapter_error "Expected exactly one word."
  | Error diagnostics ->
      let adjust diagnostic =
        match Diagnostic.location diagnostic with
        | None -> diagnostic
        | Some location when location.line = 1 ->
            let location =
              {
                location with
                column = max 1 (location.column - 6);
                offset = Option.map (fun offset -> max 0 (offset - 6)) location.offset;
              }
            in
            Diagnostic.make ~severity:(Diagnostic.severity diagnostic) ~location
              (Diagnostic.message diagnostic)
        | Some _ -> diagnostic
      in
      Error (List.map adjust diagnostics)

let ast_register_of_id = function
  | "pc" -> Ast.PC
  | "ddc" | "r0" -> Ast.Reg 0
  | "stk" | "r31" -> Ast.Reg 31
  | name when String.length name > 1 && Char.equal name.[0] 'r' -> (
      match int_of_string_opt (String.sub name 1 (String.length name - 1)) with
      | Some number when number >= 0 && number <= 31 -> Ast.Reg number
      | _ -> fail_adapter "Unknown legacy register %S." name)
  | name -> fail_adapter "Unknown legacy register %S." name

let overlay_regfile config regfile registers =
  List.fold_left
    (fun registers (register, word) ->
      let register = Irreg.translate_regname register in
      let word =
        Irreg.translate_word word (Runtime_config.max_addr config)
          (Runtime_config.stack_addr config)
      in
      Parameters.check_word word;
      Machine.RegMap.add register word registers)
    registers regfile

let init config program regfile =
  try
    with_full config (fun () ->
        let registers = Machine.init_reg_state (Runtime_config.stack_addr config) in
        let registers =
          Option.fold ~none:registers ~some:(fun r -> overlay_regfile config r registers) regfile
        in
        let memory = Machine.init_mem_state Z.zero program in
        Ok { config; machine = Machine.init registers memory })
  with
  | Adapter_error message -> adapter_error message
  | Parameters.NotSupported message -> adapter_error message
  | Invalid_argument message -> adapter_error message

let view_status = function
  | Machine.Running -> Machine_view.Running
  | Halted -> Halted
  | Failed -> Failed

let step state =
  with_full state.config (fun () ->
      match Machine.step state.machine with
      | Some machine -> Ok { state with machine }
      | None -> Error (Machine_backend.Stopped (view_status (Machine.get_exec_state state.machine))))

let step_n count state =
  if count < 0 then Error (Machine_backend.Backend_error "step count must be non-negative")
  else
    with_full state.config (fun () ->
        match Machine.step_n state.machine count with
        | Some machine -> Ok { state with machine }
        | None ->
            Error (Machine_backend.Stopped (view_status (Machine.get_exec_state state.machine))))

let permission_text = function
  | Ast.O -> "O"
  | E -> "E"
  | RO -> "RO"
  | RX -> "RX"
  | RW -> "RW"
  | RWX -> "RWX"
  | RWL -> "RWL"
  | RWLX -> "RWLX"
  | URW -> "URW"
  | URWL -> "URWL"
  | URWX -> "URWX"
  | URWLX -> "URWLX"

let locality_text = function Ast.Global -> "GLOBAL" | Local -> "LOCAL" | Directed -> "DIRECTED"

let seal_permission_text (seal, unseal) =
  match (seal, unseal) with
  | false, false -> "SO"
  | true, false -> "S"
  | false, true -> "U"
  | true, true -> "SU"

let sealable_text = function
  | Ast.Cap (permission, locality, base, limit, cursor) ->
      Printf.sprintf "(%s, %s, %s, %s, %s)" (permission_text permission) (locality_text locality)
        (Z.to_string base) (Z.to_string limit) (Z.to_string cursor)
  | SealRange (permission, locality, base, limit, cursor) ->
      Printf.sprintf "[%s, %s, %s, %s, %s]" (seal_permission_text permission)
        (locality_text locality) (Z.to_string base) (Z.to_string limit) (Z.to_string cursor)

let word_text = function
  | Ast.I value -> Z.to_string value
  | Sealable sealable -> sealable_text sealable
  | Sealed (object_type, sealable) ->
      Printf.sprintf "{%s: %s}" (Z.to_string object_type) (sealable_text sealable)

let capability_of_sealable = function
  | Ast.Cap (permission, locality, base, limit, cursor) ->
      Some
        {
          Machine_view.base;
          limit;
          cursor;
          permissions = [ permission_text permission ];
          locality = Some (locality_text locality);
        }
  | SealRange _ -> None

let sealing_of_sealable ?object_type ~is_sealed = function
  | Ast.Cap _ ->
      if is_sealed then
        Some { Machine_view.object_type; can_seal = None; can_unseal = None; is_sealed }
      else None
  | SealRange ((seal, unseal), _, _, _, _) ->
      Some { Machine_view.object_type; can_seal = Some seal; can_unseal = Some unseal; is_sealed }

let view_word word =
  let edit_text = word_text word in
  let fingerprint = Digest.to_hex (Digest.string edit_text) in
  match word with
  | Ast.I integer ->
      {
        Machine_view.edit_text;
        short_text = edit_text;
        detail_text = edit_text;
        fingerprint;
        kind = Integer;
        integer = Some integer;
        capability = None;
        sealing = None;
        annotations = [];
      }
  | Sealable (Ast.Cap (Ast.E, _, _, _, _) as sealable) ->
      {
        edit_text;
        short_text = edit_text;
        detail_text = edit_text;
        fingerprint;
        kind = Sentry;
        integer = None;
        capability = capability_of_sealable sealable;
        sealing = None;
        annotations = [];
      }
  | Sealable (Ast.Cap _ as sealable) ->
      {
        edit_text;
        short_text = edit_text;
        detail_text = edit_text;
        fingerprint;
        kind = Capability;
        integer = None;
        capability = capability_of_sealable sealable;
        sealing = None;
        annotations = [];
      }
  | Sealable (Ast.SealRange _ as sealable) ->
      {
        edit_text;
        short_text = edit_text;
        detail_text = edit_text;
        fingerprint;
        kind = Seal_range;
        integer = None;
        capability = None;
        sealing = sealing_of_sealable ~is_sealed:false sealable;
        annotations = [];
      }
  | Sealed (object_type, sealable) ->
      {
        edit_text;
        short_text = edit_text;
        detail_text = edit_text;
        fingerprint;
        kind = Sealed_capability;
        integer = None;
        capability = capability_of_sealable sealable;
        sealing = sealing_of_sealable ~object_type ~is_sealed:true sealable;
        annotations = [];
      }

let register_description = function
  | Ast.PC ->
      ({ Machine_view.Register_id.bank = System; key = "pc" }, "pc", Machine_view.Program_counter)
  | Ast.Reg 0 ->
      ( { Machine_view.Register_id.bank = System; key = "ddc" },
        "ddc",
        Machine_view.Backend_specific "default-data-capability" )
  | Ast.Reg 31 ->
      ({ Machine_view.Register_id.bank = System; key = "stk" }, "stk", Machine_view.Stack_pointer)
  | Ast.Reg number ->
      let label = "r" ^ string_of_int number in
      ({ Machine_view.Register_id.bank = General; key = label }, label, Machine_view.General)

let inspect state =
  with_full state.config (fun () ->
      let registers =
        Machine.RegMap.bindings (Machine.get_regfile state.machine)
        |> List.map (fun (register, word) ->
            let id, label, role = register_description register in
            { Machine_view.id; label; role; word = view_word word })
      in
      let memory =
        Machine.MemMap.bindings (Machine.get_memory state.machine)
        |> List.map (fun (address, word) -> { Machine_view.address; word = view_word word })
      in
      let pc =
        match Machine.read_reg Ast.PC state.machine with
        | Ast.Sealable (Ast.Cap (_, _, _, _, cursor)) -> Some cursor
        | _ -> None
      in
      {
        Machine_view.backend_name = name;
        status = view_status (Machine.get_exec_state state.machine);
        address_limit = Runtime_config.max_addr state.config;
        pc;
        registers;
        memory;
        missing_cell = Default (view_word (Ast.I Z.zero));
      })

let ast_word_for_state state word =
  Irreg.translate_word word
    (Runtime_config.max_addr state.config)
    (Runtime_config.stack_addr state.config)

let register_of_id (id : Machine_view.Register_id.t) =
  match (id.bank, id.key) with
  | System, (("pc" | "ddc" | "stk") as key) -> ast_register_of_id key
  | General, key -> ast_register_of_id key
  | _, key -> fail_adapter "Register %S does not belong to that register bank." key

let set_register id word state =
  try
    let register = register_of_id id in
    let word = ast_word_for_state state word in
    Ok { state with machine = Machine.set_reg register word state.machine }
  with Adapter_error message -> adapter_error message

let set_memory address word state =
  if Z.sign address < 0 || Z.compare address (Runtime_config.max_addr state.config) >= 0 then
    adapter_error
      (Printf.sprintf "Memory address %s is outside the configured address space."
         (Z.to_string address))
  else
    try
      let word = ast_word_for_state state word in
      Ok { state with machine = Machine.set_mem address word state.machine }
    with Adapter_error message -> adapter_error message
