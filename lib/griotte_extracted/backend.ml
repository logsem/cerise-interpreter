module Ast = Ast
module Asm_ir = Asm_ir
module Parser = Parser_api
module Printer = Printer
module State = State
module View = View
module E = Cerise_griotte_extracted_generated.Griotte_extracted
module Xcodec = Codec

let name = "griotte-extracted"
let description = "Rocq-extracted CHERIoT-inspired Griotte capability machine"

type asm_program = Asm_ir.program
type asm_regfile = Asm_ir.regfile
type asm_word = Asm_ir.word

(* The generated machine fixes MemNum and ONum at this exclusive bound. *)
let extracted_bound = Z.of_int 2_000_000
let ( let* ) (type value next error) (result : (value, error) result)
        (continuation : value -> (next, error) result) : (next, error) result =
  Result.bind result continuation
let boundary_error (message : 'a) : ('b, 'a) result = Error message
let diagnostic (message : string) : ('a, Diagnostic.t list) result = Error [ Diagnostic.error message ]
let rec nat_of_int (n : int) : E.nat = if n = 0 then E.O else E.S (nat_of_int (n - 1))

let int_of_nat_bounded (value : E.nat) : (int, string) result =
  let rec loop (count : int) (matched_value : E.nat) : (int, string) result = match matched_value with
    | E.O -> Ok count
    | E.S rest when count < 32 -> loop (count + 1) rest
    | E.S _ -> boundary_error "extracted register number exceeds 31"
  in
  loop 0 value

let rec positive_of_z (n : Z.t) : E.positive =
  if Z.equal n Z.one then E.XH
  else
    let quotient, remainder = Z.ediv_rem n (Z.of_int 2) in
    if Z.equal remainder Z.zero then E.XO (positive_of_z quotient)
    else E.XI (positive_of_z quotient)

let z_to_e (n : Z.t) : E.z =
  match Z.sign n with
  | 0 -> E.Z0
  | 1 -> E.Zpos (positive_of_z n)
  | _ -> E.Zneg (positive_of_z (Z.neg n))

let rec z_of_positive (matched_value : E.positive) : Z.t = match matched_value with
  | E.XH -> Z.one
  | E.XO p -> Z.mul (Z.of_int 2) (z_of_positive p)
  | E.XI p -> Z.succ (Z.mul (Z.of_int 2) (z_of_positive p))

let z_of_e (matched_value : E.z) : Z.t = match matched_value with
  | E.Z0 -> Z.zero
  | E.Zpos p -> z_of_positive p
  | E.Zneg p -> Z.neg (z_of_positive p)

let register_to_e (matched_value : Ast.register) : (E.regName, string) result = match matched_value with
  | Ast.PC -> Ok E.PC
  | Ast.Reg n when n >= 0 && n <= 31 -> Ok (E.R (nat_of_int n))
  | Ast.Reg n -> boundary_error (Printf.sprintf "register r%d is outside Griotte's r0--r31 range" n)

let register_of_e (matched_value : E.regName) : (Ast.register, string) result = match matched_value with
  | E.PC -> Ok Ast.PC
  | E.R n -> Result.map (fun n -> Ast.Reg n) (int_of_nat_bounded n)

let system_register_to_e (Ast.MTDC : Ast.system_register) : E.sRegName = E.MTDC
let system_register_of_e (E.MTDC : E.sRegName) : Ast.system_register = Ast.MTDC

let permission_to_e ((rx, w, dl, dro) : Ast.rx_permission * Ast.write_permission * Ast.deep_local_permission *
Ast.deep_read_only_permission) : E.perm =
  let rx = match rx with Ast.Orx -> E.Orx | R -> E.R0 | X -> E.X | XSR -> E.XSR in
  let w = match w with Ast.Ow -> E.Ow | W -> E.W | WL -> E.WL in
  let dl = match dl with Ast.LG -> E.LG | DL -> E.DL in
  let dro = match dro with Ast.LM -> E.LM | DRO -> E.DRO in
  E.BPerm (rx, w, dl, dro)

let permission_of_e ((E.BPerm (rx, w, dl, dro)) : E.perm) : Ast.rx_permission * Ast.write_permission * Ast.deep_local_permission *
Ast.deep_read_only_permission =
  let rx = match rx with E.Orx -> Ast.Orx | E.R0 -> R | E.X -> X | E.XSR -> XSR in
  let w = match w with E.Ow -> Ast.Ow | E.W -> W | E.WL -> WL in
  let dl = match dl with E.LG -> Ast.LG | E.DL -> DL in
  let dro = match dro with E.LM -> Ast.LM | E.DRO -> DRO in
  (rx, w, dl, dro)

let locality_to_e (matched_value : Ast.locality) : E.locality = match matched_value with Ast.Global -> E.Global | Local -> E.Local
let locality_of_e (matched_value : E.locality) : Ast.locality = match matched_value with E.Global -> Ast.Global | E.Local -> Local

let finz_to_e (kind : string) (value : Z.t) : (E.Coq_finz.finz, string) result =
  if Z.sign value < 0 || Z.compare value extracted_bound >= 0 then
    boundary_error
      (Printf.sprintf "%s %s is outside the extracted Griotte range [0,%s)" kind (Z.to_string value)
         (Z.to_string extracted_bound))
  else Ok (E.Coq_finz.FinZ (z_to_e value))

let finz_of_e ((E.Coq_finz.FinZ value) : E.Coq_finz.finz) : (Z.t, string) result =
  let value = z_of_e value in
  if Z.sign value < 0 || Z.compare value extracted_bound >= 0 then
    boundary_error "generated machine produced an invalid finite value"
  else Ok value

let sealable_to_e (matched_value : Ast.sealable) : (E.sealable, string) result = match matched_value with
  | Ast.Cap (p, l, b, e, a) ->
      let* b = finz_to_e "capability base" b in
      let* e = finz_to_e "capability limit" e in
      let* a = finz_to_e "capability cursor" a in
      Ok (E.SCap (permission_to_e p, locality_to_e l, b, e, a))
  | Ast.SealRange (p, l, b, e, a) ->
      let* b = finz_to_e "seal-range base" b in
      let* e = finz_to_e "seal-range limit" e in
      let* a = finz_to_e "seal-range cursor" a in
      Ok (E.SSealRange (p, locality_to_e l, b, e, a))

let sealable_of_e (matched_value : E.sealable) : (Ast.sealable, string) result = match matched_value with
  | E.SCap (p, l, b, e, a) ->
      let* b = finz_of_e b in
      let* e = finz_of_e e in
      let* a = finz_of_e a in
      Ok (Ast.Cap (permission_of_e p, locality_of_e l, b, e, a))
  | E.SSealRange (p, l, b, e, a) ->
      let* b = finz_of_e b in
      let* e = finz_of_e e in
      let* a = finz_of_e a in
      Ok (Ast.SealRange (p, locality_of_e l, b, e, a))

let word_to_e (matched_value : Ast.word) : (E.word, string) result = match matched_value with
  | Ast.I z -> Ok (E.WInt (z_to_e z))
  | Ast.Sealable sealable -> Result.map (fun s -> E.WSealable s) (sealable_to_e sealable)
  | Ast.Sentry (p, l, b, e, a) ->
      let* b = finz_to_e "sentry base" b in
      let* e = finz_to_e "sentry limit" e in
      let* a = finz_to_e "sentry cursor" a in
      Ok (E.WSentry (permission_to_e p, locality_to_e l, b, e, a))
  | Ast.Sealed (otype, sealable) ->
      let* otype = finz_to_e "object type" otype in
      let* sealable = sealable_to_e sealable in
      Ok (E.WSealed (otype, sealable))

let word_of_e (matched_value : E.word) : (Ast.word, string) result = match matched_value with
  | E.WInt z -> Ok (Ast.I (z_of_e z))
  | E.WSealable sealable -> Result.map (fun s -> Ast.Sealable s) (sealable_of_e sealable)
  | E.WSentry (p, l, b, e, a) ->
      let* b = finz_of_e b in
      let* e = finz_of_e e in
      let* a = finz_of_e a in
      Ok (Ast.Sentry (permission_of_e p, locality_of_e l, b, e, a))
  | E.WSealed (otype, sealable) ->
      let* otype = finz_of_e otype in
      let* sealable = sealable_of_e sealable in
      Ok (Ast.Sealed (otype, sealable))

let operand_to_e (matched_value : Ast.reg_or_const) : ((E.z, E.regName) E.sum, string) result = match matched_value with
  | Ast.Constant z -> Ok (E.Inl (z_to_e z))
  | Ast.Register r -> Result.map (fun r -> E.Inr r) (register_to_e r)

let operand_of_e (matched_value : (E.z, E.regName) E.sum) : (Ast.reg_or_const, string) result = match matched_value with
  | E.Inl z -> Ok (Ast.Constant (z_of_e z))
  | E.Inr r -> Result.map (fun r -> Ast.Register r) (register_of_e r)

let instruction_to_e (matched_value : Ast.instruction) : (E.instr, string) result = match matched_value with
  | Ast.Jmp a -> Result.map (fun a -> E.Jmp a) (operand_to_e a)
  | Jnz (r, a) ->
      let* r = register_to_e r in
      Result.map (fun a -> E.Jnz (a, r)) (operand_to_e a)
  | Jalr (a, b) ->
      let* a = register_to_e a in
      Result.map (fun b -> E.Jalr (a, b)) (register_to_e b)
  | ReadSR (r, sr) -> Result.map (fun r -> E.ReadSR (r, system_register_to_e sr)) (register_to_e r)
  | WriteSR (sr, r) ->
      Result.map (fun r -> E.WriteSR (system_register_to_e sr, r)) (register_to_e r)
  | Move (r, a) ->
      let* r = register_to_e r in
      Result.map (fun a -> E.Mov (r, a)) (operand_to_e a)
  | Load (a, b) ->
      let* a = register_to_e a in
      Result.map (fun b -> E.Load (a, b)) (register_to_e b)
  | Store (r, a) ->
      let* r = register_to_e r in
      Result.map (fun a -> E.Store (r, a)) (operand_to_e a)
  | ( Add (r, a, b)
    | Sub (r, a, b)
    | Mul (r, a, b)
    | LAnd (r, a, b)
    | LOr (r, a, b)
    | LShiftL (r, a, b)
    | LShiftR (r, a, b)
    | Lt (r, a, b)
    | SubSeg (r, a, b) ) as op ->
      let* r = register_to_e r in
      let* a = operand_to_e a in
      let* b = operand_to_e b in
      Ok
        (match op with
        | Add _ -> E.Add (r, a, b)
        | Sub _ -> E.Sub (r, a, b)
        | Mul _ -> E.Mul (r, a, b)
        | LAnd _ -> E.LAnd (r, a, b)
        | LOr _ -> E.LOr (r, a, b)
        | LShiftL _ -> E.LShiftL (r, a, b)
        | LShiftR _ -> E.LShiftR (r, a, b)
        | Lt _ -> E.Lt0 (r, a, b)
        | SubSeg _ -> E.Subseg (r, a, b)
        | _ -> assert false)
  | (Lea (r, a) | Restrict (r, a)) as op ->
      let* r = register_to_e r in
      let* a = operand_to_e a in
      Ok
        (match op with
        | Lea _ -> E.Lea (r, a)
        | Restrict _ -> E.Restrict (r, a)
        | _ -> assert false)
  | ( GetL (a, b)
    | GetB (a, b)
    | GetE (a, b)
    | GetA (a, b)
    | GetP (a, b)
    | GetOType (a, b)
    | GetWType (a, b) ) as op ->
      let* a = register_to_e a in
      let* b = register_to_e b in
      Ok
        (match op with
        | GetL _ -> E.GetL (a, b)
        | GetB _ -> E.GetB (a, b)
        | GetE _ -> E.GetE (a, b)
        | GetA _ -> E.GetA (a, b)
        | GetP _ -> E.GetP (a, b)
        | GetOType _ -> E.GetOType (a, b)
        | GetWType _ -> E.GetWType (a, b)
        | _ -> assert false)
  | (Seal (a, b, c) | UnSeal (a, b, c)) as op ->
      let* a = register_to_e a in
      let* b = register_to_e b in
      let* c = register_to_e c in
      Ok
        (match op with
        | Seal _ -> E.Seal (a, b, c)
        | UnSeal _ -> E.UnSeal (a, b, c)
        | _ -> assert false)
  | Fail -> Ok E.Fail
  | Halt -> Ok E.Halt

let instruction_of_e (matched_value : E.instr) : (Ast.instruction, string) result = match matched_value with
  | E.Jmp a -> Result.map (fun a -> Ast.Jmp a) (operand_of_e a)
  | E.Jnz (a, r) ->
      let* r = register_of_e r in
      Result.map (fun a -> Ast.Jnz (r, a)) (operand_of_e a)
  | E.Jalr (a, b) ->
      let* a = register_of_e a in
      Result.map (fun b -> Ast.Jalr (a, b)) (register_of_e b)
  | E.ReadSR (r, E.MTDC) -> Result.map (fun r -> Ast.ReadSR (r, Ast.MTDC)) (register_of_e r)
  | E.WriteSR (E.MTDC, r) -> Result.map (fun r -> Ast.WriteSR (Ast.MTDC, r)) (register_of_e r)
  | E.Mov (r, a) ->
      let* r = register_of_e r in
      Result.map (fun a -> Ast.Move (r, a)) (operand_of_e a)
  | E.Load (a, b) ->
      let* a = register_of_e a in
      Result.map (fun b -> Ast.Load (a, b)) (register_of_e b)
  | E.Store (r, a) ->
      let* r = register_of_e r in
      Result.map (fun a -> Ast.Store (r, a)) (operand_of_e a)
  | ( E.Add (r, a, b)
    | E.Sub (r, a, b)
    | E.Mul (r, a, b)
    | E.LAnd (r, a, b)
    | E.LOr (r, a, b)
    | E.LShiftL (r, a, b)
    | E.LShiftR (r, a, b)
    | E.Lt0 (r, a, b)
    | E.Subseg (r, a, b) ) as op ->
      let* r = register_of_e r in
      let* a = operand_of_e a in
      let* b = operand_of_e b in
      Ok
        (match op with
        | E.Add _ -> Ast.Add (r, a, b)
        | E.Sub _ -> Ast.Sub (r, a, b)
        | E.Mul _ -> Ast.Mul (r, a, b)
        | E.LAnd _ -> Ast.LAnd (r, a, b)
        | E.LOr _ -> Ast.LOr (r, a, b)
        | E.LShiftL _ -> Ast.LShiftL (r, a, b)
        | E.LShiftR _ -> Ast.LShiftR (r, a, b)
        | E.Lt0 _ -> Ast.Lt (r, a, b)
        | E.Subseg _ -> Ast.SubSeg (r, a, b)
        | _ -> assert false)
  | (E.Lea (r, a) | E.Restrict (r, a)) as op ->
      let* r = register_of_e r in
      let* a = operand_of_e a in
      Ok
        (match op with
        | E.Lea _ -> Ast.Lea (r, a)
        | E.Restrict _ -> Ast.Restrict (r, a)
        | _ -> assert false)
  | ( E.GetL (a, b)
    | E.GetB (a, b)
    | E.GetE (a, b)
    | E.GetA (a, b)
    | E.GetP (a, b)
    | E.GetOType (a, b)
    | E.GetWType (a, b) ) as op ->
      let* a = register_of_e a in
      let* b = register_of_e b in
      Ok
        (match op with
        | E.GetL _ -> Ast.GetL (a, b)
        | E.GetB _ -> Ast.GetB (a, b)
        | E.GetE _ -> Ast.GetE (a, b)
        | E.GetA _ -> Ast.GetA (a, b)
        | E.GetP _ -> Ast.GetP (a, b)
        | E.GetOType _ -> Ast.GetOType (a, b)
        | E.GetWType _ -> Ast.GetWType (a, b)
        | _ -> assert false)
  | (E.Seal (a, b, c) | E.UnSeal (a, b, c)) as op ->
      let* a = register_of_e a in
      let* b = register_of_e b in
      let* c = register_of_e c in
      Ok
        (match op with
        | E.Seal _ -> Ast.Seal (a, b, c)
        | E.UnSeal _ -> Ast.UnSeal (a, b, c)
        | _ -> assert false)
  | E.Fail -> Ok Ast.Fail
  | E.Halt -> Ok Ast.Halt

let dummy_finz = E.Coq_finz.FinZ E.Z0
let dummy_permission = permission_to_e Ast.null_permission

exception Boundary_decode

let decoded_or_fail (matched_value : ('a, 'b) result) : 'a = match matched_value with Ok value -> value | Error _ -> raise Boundary_decode

(* Instruction decoding is totalized to [Fail]. Invalid scalar decodings raise
   the private [Boundary_decode] signal because substituting a valid permission
   could make malformed [Restrict] succeed; [step] catches every such signal
   around the generated call and exposes a deterministic Failed state. *)
let parameters : E.machineParameters =
  {
    decodeInstr =
      (fun encoded ->
        match Xcodec.decode (z_of_e encoded) with
        | Error _ -> E.Fail
        | Ok instruction -> Result.value ~default:E.Fail (instruction_to_e instruction));
    encodeInstr =
      (fun instruction ->
        match instruction_of_e instruction with
        | Ok instruction ->
            z_to_e (Result.value ~default:(Z.of_int 0x35) (Xcodec.encode instruction))
        | Error _ -> z_to_e (Z.of_int 0x35));
    encodePerm = (fun permission -> z_to_e (Xcodec.encode_permission (permission_of_e permission)));
    decodePerm =
      (fun encoded ->
        Xcodec.decode_permission (z_of_e encoded) |> Result.map permission_to_e |> decoded_or_fail);
    encodeLoc = (fun locality -> z_to_e (Xcodec.encode_locality (locality_of_e locality)));
    decodePermPair =
      (fun encoded ->
        Xcodec.decode_permission_locality (z_of_e encoded)
        |> Result.map (fun (p, l) -> (permission_to_e p, locality_to_e l))
        |> decoded_or_fail);
    encodePermPair =
      (fun (p, l) ->
        z_to_e (Xcodec.encode_permission_locality (permission_of_e p) (locality_of_e l)));
    encodeSealPerms = (fun p -> z_to_e (Xcodec.encode_seal_permission p));
    decodeSealPerms =
      (fun encoded -> decoded_or_fail (Xcodec.decode_seal_permission (z_of_e encoded)));
    decodeSealPermPair =
      (fun encoded ->
        Xcodec.decode_seal_permission_locality (z_of_e encoded)
        |> Result.map (fun (p, l) -> (p, locality_to_e l))
        |> decoded_or_fail);
    encodeSealPermPair =
      (fun (p, l) -> z_to_e (Xcodec.encode_seal_permission_locality p (locality_of_e l)));
    encodeWordType =
      (fun word ->
        let word_type =
          match word with
          | E.WInt _ -> Ast.W_I
          | E.WSealable (E.SCap _) -> W_Cap
          | E.WSealable (E.SSealRange _) -> W_SealRange
          | E.WSealed _ -> W_Sealed
          | E.WSentry _ -> W_Sentry
        in
        z_to_e (Xcodec.encode_word_type word_type));
    decodeWordType =
      (fun encoded ->
        match Xcodec.decode_word_type (z_of_e encoded) with
        | Ok Ast.W_Cap ->
            E.WSealable (E.SCap (dummy_permission, E.Global, dummy_finz, dummy_finz, dummy_finz))
        | Ok W_SealRange ->
            E.WSealable
              (E.SSealRange ((false, false), E.Global, dummy_finz, dummy_finz, dummy_finz))
        | Ok W_Sealed ->
            E.WSealed
              (dummy_finz, E.SCap (dummy_permission, E.Global, dummy_finz, dummy_finz, dummy_finz))
        | Ok W_Sentry -> E.WSentry (dummy_permission, E.Global, dummy_finz, dummy_finz, dummy_finz)
        | Ok W_I -> E.WInt E.Z0
        | Error _ -> raise Boundary_decode);
  }

type state = { config : Runtime_config.t; raw : E.conf; snapshot : State.t }

let map_bindings_to_e (bindings : ('a * Ast.word) list) (empty : 'b) (insert : ('b -> 'c -> E.word -> 'b)) (key_convert : ('a -> ('c, string) result)) : ('b, string) result =
  List.fold_left
    (fun result (key, word) ->
      let* map = result in
      let* key = key_convert key in
      let* word = word_to_e word in
      Ok (insert map key word))
    (Ok empty) bindings

let raw_of_snapshot (snapshot : State.t) : (E.confFlag * ((E.reg * E.sReg) * E.mem), string) result =
  let* registers =
    map_bindings_to_e
      (State.RegMap.bindings snapshot.registers)
      E.reg_empty E.reg_insert register_to_e
  in
  let* system_registers =
    map_bindings_to_e (State.SRegMap.bindings snapshot.system_registers) E.sreg_empty E.sreg_insert
      (fun sr -> Ok (system_register_to_e sr))
  in
  let* memory =
    map_bindings_to_e
      (State.MemMap.bindings snapshot.memory)
      E.mem_empty E.mem_insert (finz_to_e "memory address")
  in
  Ok (E.Executable, ((registers, system_registers), memory))

let fold_extracted (bindings : ('a * E.word) list) (empty : 'b) (add : ('c -> Ast.word -> 'b -> 'b)) (key_convert : ('a -> ('c, string) result)) : ('b, string) result =
  List.fold_left
    (fun result (key, word) ->
      let* map = result in
      let* key = key_convert key in
      let* word = word_of_e word in
      Ok (add key word map))
    (Ok empty) bindings

let snapshot_of_raw (config : Runtime_config.t) ((flag, conf) : E.confFlag * E.execConf) : (State.t, string) result =
  try
    let* registers =
      fold_extracted
        (E.reg_elements (E.reg0 conf))
        State.RegMap.empty State.RegMap.add register_of_e
    in
    let registers = State.RegMap.add Ast.cnull (Ast.I Z.zero) registers in
    let* system_registers =
      fold_extracted
        (E.sreg_elements (E.sreg conf))
        State.SRegMap.empty State.SRegMap.add
        (fun sr -> Ok (system_register_of_e sr))
    in
    let* memory =
      fold_extracted (E.mem_elements (E.mem0 conf)) State.MemMap.empty State.MemMap.add finz_of_e
    in
    let status =
      match flag with
      | E.Executable | NextI -> State.Running
      | Halted -> State.Halted
      | Failed -> State.Failed
    in
    Ok { State.config; status; registers; system_registers; memory }
  with _ -> boundary_error "exception while converting an extracted configuration"

let fail_state (state : state) : state = { state with snapshot = { state.snapshot with status = State.Failed } }

let init (config : Runtime_config.t) (program : Asm_ir.statement list) (regfile : Asm_ir.regfile_entry list option) : (state, Diagnostic.t list) result =
  if Z.compare (Runtime_config.max_addr config) extracted_bound > 0 then
    diagnostic
      (Printf.sprintf "Configured address limit %s exceeds extracted Griotte's fixed limit %s."
         (Z.to_string (Runtime_config.max_addr config))
         (Z.to_string extracted_bound))
  else
    let* program = Asm_ir.lower_program config program in
    let* regfile =
      match regfile with
      | None -> Ok None
      | Some regfile -> Result.map Option.some (Asm_ir.lower_regfile config regfile)
    in
    let* snapshot = State.init config program regfile in
    match raw_of_snapshot snapshot with
    | Error message -> diagnostic ("Cannot represent initial extracted Griotte state: " ^ message)
    | Ok raw -> Ok { config; raw; snapshot }

let parse_program ?filename:(filename : string option) (source : string) :
    (asm_program, Diagnostic.t list) result = Parser.parse_program ?filename source
let parse_regfile ?filename:(filename : string option) (source : string) :
    (asm_regfile, Diagnostic.t list) result = Parser.parse_regfile ?filename source
let parse_word ?filename:(filename : string option) (source : string) :
    (asm_word, Diagnostic.t list) result = Parser.parse_word ?filename source
let inspect (state : state) : Machine_view.t = View.inspect ~backend_name:name state.snapshot

let step (state : state) : (state, Machine_backend.execution_error) result =
  match state.snapshot.status with
  | State.Halted -> Error (Machine_backend.Stopped Machine_view.Halted)
  | Failed -> Error (Machine_backend.Stopped Machine_view.Failed)
  | Running -> (
      let _, conf = state.raw in
      match try E.machine_step parameters (E.Executable, conf) with _ -> None with
      | None -> Ok (fail_state state)
      | Some raw -> (
          match snapshot_of_raw state.config raw with
          | Ok snapshot -> Ok { state with raw; snapshot }
          | Error _ -> Ok (fail_state state)))

let rec step_n (count : int) (state : state) : (state, Machine_backend.execution_error) result =
  if count < 0 then Error (Machine_backend.Backend_error "step count must be non-negative")
  else if count = 0 then Ok state
  else
    match step state with
    | Ok next -> step_n (count - 1) next
    | Error (Machine_backend.Stopped _) -> Ok state
    | Error _ as error -> error

let lower_edit (state : state) (term : asm_word) : (Ast.word, Diagnostic.t list) result = Asm_ir.lower_word state.config term

let set_register (id : Machine_view.register_id) (term : asm_word) (state : state) : (state, Diagnostic.t list) result =
  let* word = lower_edit state term in
  match (id.Machine_view.Register_id.bank, id.key) with
  | System, "mtdc" -> (
      match word_to_e word with
      | Error message -> diagnostic ("Cannot represent extracted Griotte register value: " ^ message)
      | Ok extracted ->
          let flag, conf = state.raw in
          let raw = (flag, E.update_sreg conf E.MTDC extracted) in
          let snapshot = State.set_system_register Ast.MTDC word state.snapshot in
          Ok { state with raw; snapshot })
  | _ -> (
      let* register = View.register_of_id id in
      let word = match register with Ast.Reg 0 -> Ast.I Z.zero | _ -> word in
      match (register_to_e register, word_to_e word) with
      | Error message, _ | _, Error message ->
          diagnostic ("Cannot represent extracted Griotte register edit: " ^ message)
      | Ok extracted_register, Ok extracted_word ->
          let flag, conf = state.raw in
          let raw = (flag, E.update_reg conf extracted_register extracted_word) in
          let snapshot = State.set_register register word state.snapshot in
          Ok { state with raw; snapshot })

let set_memory (address : Z.t) (term : asm_word) (state : state) : (state, Diagnostic.t list) result =
  if Z.sign address < 0 || Z.compare address (Runtime_config.max_addr state.config) >= 0 then
    diagnostic
      (Printf.sprintf "Memory address %s is outside the configured address space."
         (Z.to_string address))
  else
    let* word = lower_edit state term in
    match (finz_to_e "memory address" address, word_to_e word) with
    | Error message, _ | _, Error message ->
        diagnostic ("Cannot represent extracted Griotte memory edit: " ^ message)
    | Ok address', Ok word' ->
        let flag, conf = state.raw in
        let raw = (flag, E.update_mem conf address' word') in
        let snapshot = State.set_memory_raw address word state.snapshot in
        Ok { state with raw; snapshot }
