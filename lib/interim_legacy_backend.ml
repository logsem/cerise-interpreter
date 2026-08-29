module Ast = Cerise_internal.Ast
module Machine = Cerise_internal.Machine
module Encode = Cerise_internal.Encode
module Parameters = Cerise_internal.Parameters

let name = "cerise"
let description = "Interim adapter for the complete legacy Cerise machine"

type program = Ast.t
type regfile = Surface_ast.regfile
type word = Surface_ast.word
type state = { config : Runtime_config.t; machine : Machine.t }

let full_flags config = { Parameters.full_cerise with max_addr = Runtime_config.max_addr config }

let with_full config operation =
  let previous = !Parameters.flags in
  Fun.protect
    ~finally:(fun () -> Parameters.flags := previous)
    (fun () ->
      Parameters.flags := full_flags config;
      operation ())

exception Lower_error of string

let lower_error ?location message = Error [ Diagnostic.error ?location message ]
let fail_lower format = Printf.ksprintf (fun message -> raise (Lower_error message)) format

let ast_register = function
  | "pc" -> Ast.PC
  | "ddc" | "r0" -> Ast.Reg 0
  | "stk" | "r31" -> Ast.Reg 31
  | name when String.length name > 1 && Char.equal name.[0] 'r' -> (
      match int_of_string_opt (String.sub name 1 (String.length name - 1)) with
      | Some number when number >= 0 && number <= 31 -> Ast.Reg number
      | _ -> fail_lower "Unknown legacy register %S." name)
  | name -> fail_lower "Unknown legacy register %S." name

let ast_permission = function
  | Surface_ast.O -> Ast.O
  | E -> E
  | RO -> RO
  | RX -> RX
  | RW -> RW
  | RWX -> RWX
  | RWL -> RWL
  | RWLX -> RWLX
  | URW -> URW
  | URWL -> URWL
  | URWX -> URWX
  | URWLX -> URWLX

let ast_locality = function
  | Surface_ast.Global -> Ast.Global
  | Local -> Local
  | Directed -> Directed

let ast_seal_permission (permission : Surface_ast.seal_permission) =
  (permission.seal, permission.unseal)

let ast_word_type = function
  | Surface_ast.Integer_type -> Ast.W_I
  | Capability_type -> W_Cap
  | Seal_range_type -> W_SealRange
  | Sealed_type -> W_Sealed

let concrete_expression = function
  | Surface_ast.Integer value -> value
  | _ -> fail_lower "An unresolved runtime expression reached backend lowering."

let encode_constant = function
  | Surface_ast.Constant_expression expression -> concrete_expression expression
  | Permission permission -> Encode.encode_perm (ast_permission permission)
  | Seal_permission permission -> Encode.encode_seal_perm (ast_seal_permission permission)
  | Locality locality -> Encode.encode_locality (ast_locality locality)
  | Word_type word_type -> Encode.encode_wtype (ast_word_type word_type)
  | Permission_locality (permission, locality) ->
      Encode.encode_perm_loc_pair (ast_permission permission) (ast_locality locality)
  | Seal_permission_locality (permission, locality) ->
      Encode.encode_seal_perm_loc_pair (ast_seal_permission permission) (ast_locality locality)

let ast_operand = function
  | Surface_ast.Register register -> Ast.Register (ast_register register)
  | Constant constant -> Ast.Const (encode_constant constant)

let ast_sealable = function
  | Surface_ast.Capability (permission, locality, base, limit, cursor) ->
      Ast.Cap
        ( ast_permission permission,
          ast_locality locality,
          concrete_expression base,
          concrete_expression limit,
          concrete_expression cursor )
  | Seal_range (permission, locality, base, limit, cursor) ->
      Ast.SealRange
        ( ast_seal_permission permission,
          ast_locality locality,
          concrete_expression base,
          concrete_expression limit,
          concrete_expression cursor )

let ast_word = function
  | Surface_ast.Integer_word expression -> Ast.I (concrete_expression expression)
  | Sealable sealable -> Ast.Sealable (ast_sealable sealable)
  | Sealed (object_type, sealable) ->
      Ast.Sealed (concrete_expression object_type, ast_sealable sealable)

let expect_register = function
  | Surface_ast.Register register -> ast_register register
  | _ -> fail_lower "Expected a register operand."

let instruction opcode operands =
  match (opcode, operands) with
  | "jmp", [ r ] -> Ast.Jmp (expect_register r)
  | "jnz", [ r1; r2 ] -> Ast.Jnz (expect_register r1, expect_register r2)
  | "mov", [ r; value ] -> Ast.Move (expect_register r, ast_operand value)
  | "load", [ r1; r2 ] -> Ast.Load (expect_register r1, expect_register r2)
  | "store", [ r; value ] -> Ast.Store (expect_register r, ast_operand value)
  | "add", [ r; v1; v2 ] -> Ast.Add (expect_register r, ast_operand v1, ast_operand v2)
  | "sub", [ r; v1; v2 ] -> Ast.Sub (expect_register r, ast_operand v1, ast_operand v2)
  | "mul", [ r; v1; v2 ] -> Ast.Mul (expect_register r, ast_operand v1, ast_operand v2)
  | "rem", [ r; v1; v2 ] -> Ast.Rem (expect_register r, ast_operand v1, ast_operand v2)
  | "div", [ r; v1; v2 ] -> Ast.Div (expect_register r, ast_operand v1, ast_operand v2)
  | "lt", [ r; v1; v2 ] -> Ast.Lt (expect_register r, ast_operand v1, ast_operand v2)
  | "lea", [ r; value ] -> Ast.Lea (expect_register r, ast_operand value)
  | "restrict", [ r; value ] -> Ast.Restrict (expect_register r, ast_operand value)
  | "subseg", [ r; v1; v2 ] -> Ast.SubSeg (expect_register r, ast_operand v1, ast_operand v2)
  | "getl", [ r1; r2 ] -> Ast.GetL (expect_register r1, expect_register r2)
  | "getb", [ r1; r2 ] -> Ast.GetB (expect_register r1, expect_register r2)
  | "gete", [ r1; r2 ] -> Ast.GetE (expect_register r1, expect_register r2)
  | "geta", [ r1; r2 ] -> Ast.GetA (expect_register r1, expect_register r2)
  | "getp", [ r1; r2 ] -> Ast.GetP (expect_register r1, expect_register r2)
  | "getotype", [ r1; r2 ] -> Ast.GetOType (expect_register r1, expect_register r2)
  | "getwtype", [ r1; r2 ] -> Ast.GetWType (expect_register r1, expect_register r2)
  | "seal", [ r1; r2; r3 ] -> Ast.Seal (expect_register r1, expect_register r2, expect_register r3)
  | "unseal", [ r1; r2; r3 ] ->
      Ast.UnSeal (expect_register r1, expect_register r2, expect_register r3)
  | "invoke", [ r1; r2 ] -> Ast.Invoke (expect_register r1, expect_register r2)
  | "loadU", [ r1; r2; value ] ->
      Ast.LoadU (expect_register r1, expect_register r2, ast_operand value)
  | "storeU", [ r; v1; v2 ] -> Ast.StoreU (expect_register r, ast_operand v1, ast_operand v2)
  | "promoteU", [ r ] -> Ast.PromoteU (expect_register r)
  | "fail", [] -> Ast.Fail
  | "halt", [] -> Ast.Halt
  | _ -> fail_lower "Unsupported instruction shape for %S." opcode

let lower_statement statement =
  try
    Ok
      (match statement.Surface_ast.node with
      | Surface_ast.Word word -> Ast.Word (ast_word word)
      | Instruction { opcode; operands } -> Ast.Op (instruction opcode operands))
  with Lower_error message -> lower_error ?location:statement.location message

let lower_program program =
  let rec loop lowered diagnostics = function
    | [] -> if diagnostics = [] then Ok (List.rev lowered) else Error (List.rev diagnostics)
    | statement :: rest -> (
        match lower_statement statement with
        | Ok statement -> loop (statement :: lowered) diagnostics rest
        | Error errors -> loop lowered (List.rev_append errors diagnostics) rest)
  in
  loop [] [] program

let lower_regfile regfile =
  let validate entry =
    try
      ignore (ast_register entry.Surface_ast.register);
      ignore (ast_word entry.word);
      Ok ()
    with Lower_error message -> lower_error ?location:entry.location message
  in
  let rec loop diagnostics = function
    | [] -> if diagnostics = [] then Ok regfile else Error (List.rev diagnostics)
    | entry :: rest -> (
        match validate entry with
        | Ok () -> loop diagnostics rest
        | Error errors -> loop (List.rev_append errors diagnostics) rest)
  in
  loop [] regfile

let overlay_regfile regfile registers =
  List.fold_left
    (fun registers entry ->
      Machine.RegMap.add (ast_register entry.Surface_ast.register) (ast_word entry.word) registers)
    registers regfile

let init config program regfile =
  try
    with_full config (fun () ->
        let registers = Machine.init_reg_state (Runtime_config.stack_addr config) in
        let registers =
          Option.fold ~none:registers ~some:(fun r -> overlay_regfile r registers) regfile
        in
        let memory = Machine.init_mem_state Z.zero program in
        Ok { config; machine = Machine.init registers memory })
  with
  | Lower_error message -> lower_error message
  | Parameters.NotSupported message -> lower_error message
  | Invalid_argument message -> lower_error message

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

let parse_word source = Surface_ast.parse_word source

let ast_word_for_state state word =
  let resolved =
    match
      Surface_ast.resolve_regfile state.config
        [ { Surface_ast.register = "r1"; word; location = None } ]
    with
    | [ entry ] -> entry.word
    | _ -> assert false
  in
  ast_word resolved

let register_of_id (id : Machine_view.Register_id.t) =
  match (id.bank, id.key) with
  | System, (("pc" | "ddc" | "stk") as key) -> ast_register key
  | General, key -> ast_register key
  | _, key -> fail_lower "Register %S does not belong to that register bank." key

let set_register id word state =
  try
    let register = register_of_id id in
    let word = ast_word_for_state state word in
    Ok { state with machine = Machine.set_reg register word state.machine }
  with Lower_error message -> lower_error message

let set_memory address word state =
  if Z.sign address < 0 || Z.compare address (Runtime_config.max_addr state.config) >= 0 then
    lower_error
      (Printf.sprintf "Memory address %s is outside the configured address space."
         (Z.to_string address))
  else
    try
      let word = ast_word_for_state state word in
      Ok { state with machine = Machine.set_mem address word state.machine }
    with Lower_error message -> lower_error message
