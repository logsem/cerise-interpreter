(** Adapter from the concrete mCerise parser and machine to the uniform backend interface consumed
    by sessions and user interfaces. *)

module Ast = Ast
module Asm_ir = Asm_ir
module Parser = Parser
module Printer = Printer
module Codec = Codec
module Machine = Machine

let name = "mcerise"
let description = "Frozen historical mCerise uninitialized-capability machine"

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

(** Backend-neutral rendering for session and interface consumers. *)
let view_word (word : Ast.word) : Machine_view.word =
  let edit_text = Printer.word word in
  let fingerprint = Digest.to_hex (Digest.string edit_text) in
  let decoded_instruction =
    match word with
    | Ast.I encoded -> Result.to_option (Codec.decode encoded) |> Option.map Printer.instruction
    | _ -> None
  in
  match word with
  | Ast.I z ->
      {
        Machine_view.edit_text;
        short_text = edit_text;
        detail_text = edit_text;
        decoded_instruction;
        fingerprint;
        kind = Integer;
        integer = Some z;
        capability = None;
        seal_range = None;
        sealing = None;
        annotations = [];
      }
  | Cap (Cap (p, l, b, e, a)) ->
      {
        Machine_view.edit_text;
        short_text = edit_text;
        detail_text = edit_text;
        decoded_instruction;
        fingerprint;
        kind = (if p = Ast.E then Sentry else Capability);
        integer = None;
        capability =
          Some
            {
              base = b;
              limit = e;
              cursor = a;
              permissions = [ Printer.permission p ];
              locality = Some (Printer.locality l);
            };
        seal_range = None;
        sealing = None;
        annotations = [];
      }

let register_description (register : Ast.register) :
    Machine_view.register_id * string * Machine_view.register_role =
  match register with
  | Ast.PC ->
      ({ Machine_view.Register_id.bank = System; key = "pc" }, "pc", Machine_view.Program_counter)
  | Reg 0 ->
      ( { Machine_view.Register_id.bank = System; key = "ddc" },
        "ddc",
        Machine_view.Backend_specific "default-data-capability" )
  | Reg 31 ->
      ({ Machine_view.Register_id.bank = System; key = "stk" }, "stk", Machine_view.Stack_pointer)
  | Reg n ->
      let label = "r" ^ string_of_int n in
      ({ Machine_view.Register_id.bank = General; key = label }, label, Machine_view.General)

let control_status (state : state) : Machine_view.status =
  match state.Machine.status with Running -> Running | Halted -> Halted | Failed -> Failed

let program_counter (state : state) : Z.t option =
  match Machine.read_register Ast.PC state with
  | Ast.Cap (Ast.Cap (_, _, _, _, cursor)) -> Some cursor
  | _ -> None

let control (_config : Runtime_config.t) (state : state) : Machine_backend.control =
  { status = control_status state; pc = program_counter state }

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
  {
    Machine_view.backend_name = name;
    status = control_status state;
    address_limit = Runtime_config.max_addr config;
    pc = program_counter state;
    registers;
    enclave_table = None;
    memory;
    missing_cell = Default (view_word (Ast.I Z.zero));
  }

(** Validated state edits from backend-neutral register and memory identifiers. *)
let register_of_id (id : Machine_view.Register_id.t) : (Ast.register, Diagnostic.t list) result =
  match (id.bank, id.key) with
  | System, "pc" -> Ok Ast.PC
  | System, ("ddc" | "r0") -> Ok (Ast.Reg 0)
  | System, ("stk" | "r31") -> Ok (Ast.Reg 31)
  | General, name when String.length name > 1 && name.[0] = 'r' -> (
      match int_of_string_opt (String.sub name 1 (String.length name - 1)) with
      | Some n when n >= 1 && n <= 30 -> Ok (Ast.Reg n)
      | _ -> Error [ Diagnostic.error (Printf.sprintf "Unknown mCerise register %S." name) ])
  | _, name ->
      Error [ Diagnostic.error (Printf.sprintf "Register %S does not belong to that bank." name) ]

let ( let* ) (type value next error) (result : (value, error) result)
    (continuation : value -> (next, error) result) : (next, error) result =
  Result.bind result continuation

let set_register (config : Runtime_config.t) (id : Machine_view.register_id) (term : asm_word)
    (state : state) : (state, Diagnostic.t list) result =
  let* r = register_of_id id in
  Result.map (fun w -> Machine.set_register r w state) (Asm_ir.assemble_word config term)

let set_memory (config : Runtime_config.t) (address : Z.t) (term : asm_word) (state : state) :
    (state, Diagnostic.t list) result =
  (* Editing observes the same finite address-space bound as machine execution. *)
  if Z.sign address < 0 || Z.compare address (Runtime_config.max_addr config) >= 0 then
    Error
      [
        Diagnostic.error
          (Printf.sprintf "Memory address %s is outside the configured address space."
             (Z.to_string address));
      ]
  else
    Result.map (fun w -> Machine.set_memory_raw address w state) (Asm_ir.assemble_word config term)
