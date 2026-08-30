(** Adapts Cerisier to the uniform machine backend and view interfaces consumed by sessions and the
    terminal application. *)

module Ast = Ast
module Asm_ir = Asm_ir
module Parser = Parser
module Printer = Printer
module Codec = Codec
module Machine = Machine

let name = "cerisier"
let description = "Vanilla Cerise machine extended with local attestation"

type asm_program = Asm_ir.program
type asm_regfile = Asm_ir.regfile
type asm_word = Asm_ir.word
type state = Machine.t

let parse_program ?(filename : string option) (source : string) :
    (asm_program, Diagnostic.t list) result =
  Parser.parse_program ?filename source

let parse_regfile ?(filename : string option) (source : string) :
    (asm_regfile, Diagnostic.t list) result =
  Parser.parse_regfile ?filename source

let parse_word ?(filename : string option) (source : string) : (asm_word, Diagnostic.t list) result
    =
  Parser.parse_word ?filename source

let init (config : Runtime_config.t) (program : Asm_ir.statement list)
    (regfile : (Ast.register * asm_word) list option) : (state, Diagnostic.t list) result =
  let ( let* ) (type value next error) (result : (value, error) result)
      (continuation : value -> (next, error) result) : (next, error) result =
    Result.bind result continuation
  in
  let* program = Asm_ir.assemble_program config program in
  let* regfile =
    match regfile with
    | None -> Ok None
    | Some entries -> Result.map Option.some (Asm_ir.assemble_regfile config entries)
  in
  Ok (Machine.init config program regfile)

let step (config : Runtime_config.t) (state : state) :
    (state, Machine_backend.execution_error) result =
  Machine.step config state

let step_n (config : Runtime_config.t) (count : int) (state : state) :
    (state, Machine_backend.execution_error) result =
  Machine.step_n config count state

let permission_text (permission : Ast.permission) : string = Printer.permission permission

let view_word (word : Ast.word) : Machine_view.word =
  let edit_text = Printer.word word in
  let fingerprint = Digest.to_hex (Digest.string edit_text) in
  let decoded_instruction =
    match word with
    | Ast.I encoded -> Result.to_option (Codec.decode encoded) |> Option.map Printer.instruction
    | _ -> None
  in
  let base (kind : Machine_view.semantic_kind) (integer : Z.t option)
      (capability : Machine_view.capability option) (seal_range : Machine_view.seal_range option)
      (sealing : Machine_view.sealing option) : Machine_view.word =
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
      annotations = [];
    }
  in
  let capability (backend_value : Ast.sealable) : Machine_view.capability option =
    match backend_value with
    | Ast.Cap (p, b, e, a) ->
        Some
          {
            Machine_view.base = b;
            limit = e;
            cursor = a;
            permissions = [ permission_text p ];
            locality = None;
          }
    | SealRange _ -> None
  in
  let sealing ?(object_type : Z.t option) ~(sealed : bool) (backend_value : Ast.sealable) :
      Machine_view.sealing option =
    match backend_value with
    | Ast.Cap _ ->
        if sealed then
          Some { Machine_view.object_type; can_seal = None; can_unseal = None; is_sealed = true }
        else None
    | SealRange ((s, u), _, _, _) ->
        Some
          { Machine_view.object_type; can_seal = Some s; can_unseal = Some u; is_sealed = sealed }
  in
  let seal_range (backend_value : Ast.sealable) : Machine_view.seal_range option =
    match backend_value with
    | Ast.SealRange (_, b, e, a) ->
        Some { Machine_view.base = b; limit = e; cursor = a; locality = None }
    | Cap _ -> None
  in
  match word with
  | Ast.I z -> base Integer (Some z) None None None
  | Sealable (Cap (Ast.E, _, _, _) as c) -> base Sentry None (capability c) None None
  | Sealable (Cap _ as c) -> base Capability None (capability c) None None
  | Sealable (SealRange _ as s) ->
      base Seal_range None None (seal_range s) (sealing ~sealed:false s)
  | Sealed (o, s) ->
      base Sealed_capability None (capability s) (seal_range s)
        (sealing ~object_type:o ~sealed:true s)

let register_description (backend_value : Ast.register) :
    Machine_view.register_id * string * Machine_view.register_role =
  match backend_value with
  | Ast.PC ->
      ({ Machine_view.Register_id.bank = System; key = "pc" }, "pc", Machine_view.Program_counter)
  | Reg 0 ->
      ( { Machine_view.Register_id.bank = System; key = "ddc" },
        "ddc",
        Machine_view.Backend_specific "default-data-capability" )
  | Reg n ->
      let label = "r" ^ string_of_int n in
      ({ Machine_view.Register_id.bank = General; key = label }, label, Machine_view.General)

let inspect (config : Runtime_config.t) (state : state) : Machine_view.t =
  let registers =
    Machine.RegMap.bindings state.Machine.registers
    |> List.map (fun (r, w) ->
        let id, label, role = register_description r in
        { Machine_view.id; label; role; word = view_word w })
  in
  let memory =
    Machine.MemMap.bindings state.Machine.memory
    |> List.map (fun (address, word) -> { Machine_view.address; word = view_word word })
  in
  let pc =
    match Machine.read_register Ast.PC state with
    | Ast.Sealable (Ast.Cap (_, _, _, a)) -> Some a
    | _ -> None
  in
  let enclave_table =
    {
      Machine_view.counter = state.Machine.enclave_counter;
      entries =
        Machine.ETableMap.bindings state.Machine.enclave_table
        |> List.map (fun (id, identity) -> { Machine_view.id; identity });
    }
  in
  {
    Machine_view.backend_name = name;
    status =
      (match state.Machine.status with Running -> Running | Halted -> Halted | Failed -> Failed);
    address_limit = Runtime_config.max_addr config;
    pc;
    registers;
    enclave_table = Some enclave_table;
    memory;
    missing_cell = Default (view_word (Ast.I Z.zero));
  }

let register_of_id (id : Machine_view.Register_id.t) : (Ast.register, Diagnostic.t list) result =
  match (id.bank, id.key) with
  | System, "pc" -> Ok Ast.PC
  | System, ("ddc" | "r0") -> Ok (Ast.Reg 0)
  | General, name when String.length name > 1 && name.[0] = 'r' -> (
      match int_of_string_opt (String.sub name 1 (String.length name - 1)) with
      | Some n when n >= 1 && n <= 31 -> Ok (Ast.Reg n)
      | _ -> Error [ Diagnostic.error (Printf.sprintf "Unknown Cerisier register %S." name) ])
  | _, name ->
      Error [ Diagnostic.error (Printf.sprintf "Register %S does not belong to that bank." name) ]

let ( let* ) (type value next error) (result : (value, error) result)
    (continuation : value -> (next, error) result) : (next, error) result =
  Result.bind result continuation

let set_register (config : Runtime_config.t) (id : Machine_view.register_id) (term : asm_word)
    (state : state) : (state, Diagnostic.t list) result =
  let* r = register_of_id id in
  Result.map (fun word -> Machine.set_register r word state) (Asm_ir.assemble_word config term)

let set_memory (config : Runtime_config.t) (address : Z.t) (term : asm_word) (state : state) :
    (state, Diagnostic.t list) result =
  if Z.sign address < 0 || Z.compare address (Runtime_config.max_addr config) >= 0 then
    Error
      [
        Diagnostic.error
          (Printf.sprintf "Memory address %s is outside the configured address space."
             (Z.to_string address));
      ]
  else
    Result.map
      (fun word -> Machine.set_memory_raw address word state)
      (Asm_ir.assemble_word config term)
