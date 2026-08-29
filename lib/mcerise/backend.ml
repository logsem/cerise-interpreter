module Ast = Ast
module Parser = Parser
module Printer = Printer
module Codec = Codec
module Machine = Machine
let name = "mcerise"
let description = "Frozen historical mCerise uninitialized-capability machine"
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
let view_word word =
  let edit_text = Printer.word word in
  let fingerprint = Digest.to_hex (Digest.string edit_text) in
  match word with
  | Ast.I z -> { Machine_view.edit_text; short_text=edit_text; detail_text=edit_text; fingerprint;
      kind=Integer; integer=Some z; capability=None; sealing=None; annotations=[] }
  | Cap (Cap (p,l,b,e,a)) ->
      { Machine_view.edit_text; short_text=edit_text; detail_text=edit_text; fingerprint;
        kind=(if p=Ast.E then Sentry else Capability); integer=None;
        capability=Some {base=b;limit=e;cursor=a;permissions=[Printer.permission p];
          locality=Some (Printer.locality l)}; sealing=None; annotations=[] }
let register_description = function
  | Ast.PC -> ({Machine_view.Register_id.bank=System;key="pc"},"pc",Machine_view.Program_counter)
  | Reg 0 -> ({Machine_view.Register_id.bank=System;key="ddc"},"ddc",
      Machine_view.Backend_specific "default-data-capability")
  | Reg 31 -> ({Machine_view.Register_id.bank=System;key="stk"},"stk",Machine_view.Stack_pointer)
  | Reg n -> let label="r"^string_of_int n in
      ({Machine_view.Register_id.bank=General;key=label},label,Machine_view.General)
let inspect state =
  let registers = Machine.RegMap.bindings state.Machine.registers |> List.map (fun (r,w) ->
    let id,label,role=register_description r in {Machine_view.id;label;role;word=view_word w}) in
  let memory = Machine.MemMap.bindings state.Machine.memory |> List.map
    (fun (address,word) -> {Machine_view.address;word=view_word word}) in
  let pc = match Machine.read_register Ast.PC state with
    | Ast.Cap (Ast.Cap (_,_,_,_,a)) -> Some a | _ -> None in
  { Machine_view.backend_name=name;
    status=(match state.Machine.status with Running->Running|Halted->Halted|Failed->Failed);
    address_limit=Runtime_config.max_addr state.config;pc;registers;memory;
    missing_cell=Default (view_word (Ast.I Z.zero)) }
let register_of_id (id:Machine_view.Register_id.t) =
  match id.bank,id.key with
  | System,"pc" -> Ok Ast.PC | System,("ddc"|"r0") -> Ok (Ast.Reg 0)
  | System,("stk"|"r31") -> Ok (Ast.Reg 31)
  | General,name when String.length name>1 && name.[0]='r' -> (
      match int_of_string_opt (String.sub name 1 (String.length name-1)) with
      | Some n when n>=1 && n<=30 -> Ok (Ast.Reg n)
      | _ -> Error [Diagnostic.error (Printf.sprintf "Unknown mCerise register %S." name)])
  | _,name -> Error [Diagnostic.error (Printf.sprintf "Register %S does not belong to that bank." name)]
let ( let* ) = Result.bind
let set_register id term state =
  let* r=register_of_id id in Result.map (fun w -> Machine.set_register r w state)
    (Machine.lower_word state.Machine.config term)
let set_memory address term state =
  if Z.sign address<0 || Z.compare address (Runtime_config.max_addr state.Machine.config)>=0
  then Error [Diagnostic.error (Printf.sprintf "Memory address %s is outside the configured address space."
    (Z.to_string address))]
  else Result.map (fun w -> Machine.set_memory_raw address w state)
    (Machine.lower_word state.Machine.config term)
