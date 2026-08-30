module Ast = Ast
module Asm_ir = Asm_ir
module Parser = Parser
module Printer = Printer
module Codec = Codec
module Machine = Machine

let name = "griotte"
let description = "Handwritten CHERIoT-inspired Griotte capability machine"

type asm_program = Asm_ir.program
type asm_regfile = Asm_ir.regfile
type asm_word = Asm_ir.word
type state = Machine.t

let parse_program ?filename:(filename : string option) (source : string) :
    (asm_program, Diagnostic.t list) result = Parser.parse_program ?filename source
let parse_regfile ?filename:(filename : string option) (source : string) :
    (asm_regfile, Diagnostic.t list) result = Parser.parse_regfile ?filename source
let parse_word ?filename:(filename : string option) (source : string) :
    (asm_word, Diagnostic.t list) result = Parser.parse_word ?filename source

let init (config : Runtime_config.t) (program : Asm_ir.statement list) (regfile : Asm_ir.regfile_entry list option) : (state, Diagnostic.t list) result =
  let ( let* ) (type value next error) (result : (value, error) result)
          (continuation : value -> (next, error) result) : (next, error) result =
    Result.bind result continuation in
  let* program = Asm_ir.lower_program config program in
  let* regfile =
    match regfile with
    | None -> Ok None
    | Some regfile -> Result.map Option.some (Asm_ir.lower_regfile config regfile)
  in
  Machine.init config program regfile

let step (state : state) : (state, Machine_backend.execution_error) result = Machine.step state
let step_n (count : int) (state : state) : (state, Machine_backend.execution_error) result =
  Machine.step_n count state

let permission_parts ((rx, w, dl, dro) : Ast.rx_permission * Ast.write_permission * Ast.deep_local_permission *
Ast.deep_read_only_permission) : string list =
  [
    Printer.rx_permission rx;
    Printer.write_permission w;
    Printer.deep_local_permission dl;
    Printer.deep_read_only_permission dro;
  ]

let capability (p : Ast.rx_permission * Ast.write_permission * Ast.deep_local_permission *
Ast.deep_read_only_permission) (l : Ast.locality) (b : Z.t) (e : Z.t) (a : Z.t) : Machine_view.capability option =
  Some
    {
      Machine_view.base = b;
      limit = e;
      cursor = a;
      permissions = permission_parts p;
      locality = Some (Printer.locality l);
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

let view_word (word : Ast.word) : Machine_view.word =
  let edit_text = Printer.word word in
  let fingerprint = Digest.to_hex (Digest.string edit_text) in
  let decoded_instruction =
    match word with
    | Ast.I encoded ->
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
  | Ast.I z -> base Integer (Some z) None None None []
  | Sealable (Cap (p, l, b, e, a)) -> base Capability None (capability p l b e a) None None []
  | Sentry (p, l, b, e, a) -> base Sentry None (capability p l b e a) None None [ ("entry", "sentry") ]
  | Sealable (SealRange (sp, l, b, e, a)) ->
      base Seal_range None None
        (Some { Machine_view.base = b; limit = e; cursor = a; locality = Some (Printer.locality l) })
        (sealing ~sealed:false sp)
        [
          ("locality", Printer.locality l);
          ("base", Z.to_string b);
          ("limit", Z.to_string e);
          ("cursor", Z.to_string a);
        ]
  | Sealed (otype, Cap (p, l, b, e, a)) ->
      base Sealed_capability None (capability p l b e a)
        None
        (Some { object_type = Some otype; can_seal = None; can_unseal = None; is_sealed = true })
        []
  | Sealed (otype, SealRange (sp, l, b, e, a)) ->
      base Sealed_capability None None
        (Some { Machine_view.base = b; limit = e; cursor = a; locality = Some (Printer.locality l) })
        (sealing ~object_type:otype ~sealed:true sp)
        [
          ("locality", Printer.locality l);
          ("base", Z.to_string b);
          ("limit", Z.to_string e);
          ("cursor", Z.to_string a);
        ]

let register_description (register : Ast.register) : Machine_view.register_id * string * Machine_view.register_role =
  let label = Printer.register register in
  match register with
  | Ast.PC ->
      ({ Machine_view.Register_id.bank = System; key = "pc" }, label, Machine_view.Program_counter)
  | Ast.Reg 0 ->
      ( { Machine_view.Register_id.bank = General; key = label },
        label,
        Machine_view.Backend_specific "null-capability" )
  | Ast.Reg 2 ->
      ({ Machine_view.Register_id.bank = General; key = label }, label, Machine_view.Stack_pointer)
  | Ast.Reg _ ->
      ({ Machine_view.Register_id.bank = General; key = label }, label, Machine_view.General)

let inspect (state : state) : Machine_view.t =
  let registers =
    Machine.RegMap.bindings state.Machine.registers
    |> List.map (fun (r, w) ->
        let id, label, role = register_description r in
        { Machine_view.id; label; role; word = view_word w })
  in
  let registers =
    registers
    @ (Machine.SRegMap.bindings state.Machine.system_registers
      |> List.map (fun (sr, w) ->
          let label = Printer.system_register sr in
          {
            Machine_view.id = { bank = System; key = label };
            label;
            role = Backend_specific "machine-trusted-domain-capability";
            word = view_word w;
          }))
  in
  let memory =
    Machine.MemMap.bindings state.Machine.memory
    |> List.map (fun (address, word) -> { Machine_view.address; word = view_word word })
  in
  let pc =
    match Machine.read_register Ast.PC state with
    | Ast.Sealable (Ast.Cap (_, _, _, _, a)) -> Some a
    | _ -> None
  in
  {
    Machine_view.backend_name = name;
    status =
      (match state.Machine.status with Running -> Running | Halted -> Halted | Failed -> Failed);
    address_limit = Runtime_config.max_addr state.config;
    pc;
    registers;
    enclave_table = None;
    memory;
    missing_cell = Default (view_word (Ast.I Z.zero));
  }

let register_of_id (id : Machine_view.Register_id.t) : (Ast.register, Diagnostic.t list) result =
  match (id.bank, id.key) with
  | System, "pc" -> Ok Ast.PC
  | General, name -> (
      match Asm_ir.parse_register_name name with
      | Some r -> Ok r
      | None -> Error [ Diagnostic.error (Printf.sprintf "Unknown Griotte register %S." name) ])
  | _, name ->
      Error [ Diagnostic.error (Printf.sprintf "Register %S does not belong to that bank." name) ]

let ( let* ) (type value next error) (result : (value, error) result)
        (continuation : value -> (next, error) result) : (next, error) result =
  Result.bind result continuation

let set_register (id : Machine_view.register_id) (term : asm_word) (state : state) : (state, Diagnostic.t list) result =
  match (id.Machine_view.Register_id.bank, id.key) with
  | System, "mtdc" ->
      Result.map
        (fun w -> Machine.set_system_register Ast.MTDC w state)
        (Asm_ir.lower_word state.Machine.config term)
  | _ ->
      let* r = register_of_id id in
      Result.map
        (fun w -> Machine.set_register r w state)
        (Asm_ir.lower_word state.Machine.config term)

let set_memory (address : Z.t) (term : asm_word) (state : state) : (state, Diagnostic.t list) result =
  if Z.sign address < 0 || Z.compare address (Runtime_config.max_addr state.Machine.config) >= 0
  then
    Error
      [
        Diagnostic.error
          (Printf.sprintf "Memory address %s is outside the configured address space."
             (Z.to_string address));
      ]
  else
    Result.map
      (fun w -> Machine.set_memory_raw address w state)
      (Asm_ir.lower_word state.Machine.config term)
