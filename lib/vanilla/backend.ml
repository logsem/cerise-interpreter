module Ast = Ast
module Parser = Parser
module Printer = Printer
module Codec = Codec
module Machine = Machine

let name = "vanilla"
let description = "Canonical global sealing Cerise machine"

type program = Ast.program
type regfile = Ast.regfile
type word = Ast.word_term
type state = Machine.t

let parse_program = Parser.parse_program
let parse_regfile = Parser.parse_regfile
let parse_word = Parser.parse_word
let init = Machine.init
let step = Machine.step
let step_n = Machine.step_n
let permission_text = Printer.permission

let view_word word =
  let edit_text = Printer.word word in
  let fingerprint = Digest.to_hex (Digest.string edit_text) in
  let base kind integer capability sealing =
    {
      Machine_view.edit_text;
      short_text = edit_text;
      detail_text = edit_text;
      fingerprint;
      kind;
      integer;
      capability;
      sealing;
      annotations = [];
    }
  in
  let capability = function
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
  let sealing ?object_type ~sealed = function
    | Ast.Cap _ ->
        if sealed then
          Some { Machine_view.object_type; can_seal = None; can_unseal = None; is_sealed = true }
        else None
    | SealRange ((s, u), _, _, _) ->
        Some
          { Machine_view.object_type; can_seal = Some s; can_unseal = Some u; is_sealed = sealed }
  in
  match word with
  | Ast.I z -> base Integer (Some z) None None
  | Sealable (Cap (Ast.E, _, _, _) as c) -> base Sentry None (capability c) None
  | Sealable (Cap _ as c) -> base Capability None (capability c) None
  | Sealable (SealRange _ as s) -> base Seal_range None None (sealing ~sealed:false s)
  | Sealed (o, s) ->
      base Sealed_capability None (capability s) (sealing ~object_type:o ~sealed:true s)

let register_description = function
  | Ast.PC ->
      ({ Machine_view.Register_id.bank = System; key = "pc" }, "pc", Machine_view.Program_counter)
  | Reg 0 ->
      ( { Machine_view.Register_id.bank = System; key = "ddc" },
        "ddc",
        Machine_view.Backend_specific "default-data-capability" )
  | Reg n ->
      let label = "r" ^ string_of_int n in
      ({ Machine_view.Register_id.bank = General; key = label }, label, Machine_view.General)

let inspect state =
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
  {
    Machine_view.backend_name = name;
    status =
      (match state.Machine.status with Running -> Running | Halted -> Halted | Failed -> Failed);
    address_limit = Runtime_config.max_addr state.config;
    pc;
    registers;
    memory;
    missing_cell = Default (view_word (Ast.I Z.zero));
  }

let register_of_id (id : Machine_view.Register_id.t) =
  match (id.bank, id.key) with
  | System, "pc" -> Ok Ast.PC
  | System, ("ddc" | "r0") -> Ok (Ast.Reg 0)
  | General, name when String.length name > 1 && name.[0] = 'r' -> (
      match int_of_string_opt (String.sub name 1 (String.length name - 1)) with
      | Some n when n >= 1 && n <= 31 -> Ok (Ast.Reg n)
      | _ -> Error [ Diagnostic.error (Printf.sprintf "Unknown vanilla register %S." name) ])
  | _, name ->
      Error [ Diagnostic.error (Printf.sprintf "Register %S does not belong to that bank." name) ]

let ( let* ) = Result.bind

let set_register id term (state : state) =
  let* r = register_of_id id in
  Result.map (fun word -> Machine.set_register r word state) (Machine.lower_word state.config term)

let set_memory address term (state : state) =
  if Z.sign address < 0 || Z.compare address (Runtime_config.max_addr state.config) >= 0 then
    Error
      [
        Diagnostic.error
          (Printf.sprintf "Memory address %s is outside the configured address space."
             (Z.to_string address));
      ]
  else
    Result.map
      (fun word -> Machine.set_memory_raw address word state)
      (Machine.lower_word state.config term)
