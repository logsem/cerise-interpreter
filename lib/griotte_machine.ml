open Griotte_ast

module RegMap = Map.Make (struct
  type t = register

  let compare = compare
end)

module SRegMap = Map.Make (struct
  type t = system_register

  let compare = compare
end)

module MemMap = Map.Make (Z)

type status = Running | Halted | Failed

type t = {
  config : Runtime_config.t;
  status : status;
  registers : word RegMap.t;
  system_registers : word SRegMap.t;
  memory : word MemMap.t;
}

let diagnostic message = Error [ Diagnostic.error message ]
let ( let* ) = Result.bind
let arch_root_memory_permission = (R, WL, LG, LM)
let arch_root_executable_permission = (XSR, Ow, LG, LM)

let rx_flows requested current =
  match (requested, current) with
  | Orx, _ -> true
  | R, Orx -> false
  | R, _ -> true
  | X, (X | XSR) -> true
  | X, _ -> false
  | XSR, XSR -> true
  | XSR, _ -> false

let write_flows requested current =
  match (requested, current) with
  | Ow, _ -> true
  | W, Ow -> false
  | W, _ -> true
  | WL, WL -> true
  | WL, _ -> false

let deep_local_flows requested current =
  match (requested, current) with DL, _ -> true | LG, LG -> true | LG, DL -> false

let deep_read_only_flows requested current =
  match (requested, current) with DRO, _ -> true | LM, LM -> true | LM, DRO -> false

let permission_flows (rx, w, dl, dro) (rx', w', dl', dro') =
  rx_flows rx rx' && write_flows w w' && deep_local_flows dl dl' && deep_read_only_flows dro dro'

let locality_flows requested current =
  match (requested, current) with Local, _ | Global, Global -> true | Global, Local -> false

let seal_permission_flows (s, u) (s', u') = ((not s) || s') && ((not u) || u')

let permission_of_word = function
  | Sealable (Cap (p, _, _, _, _)) | Sentry (p, _, _, _, _) | Sealed (_, Cap (p, _, _, _, _)) -> p
  | _ -> null_permission

let word_is_derived word =
  permission_flows (permission_of_word word) arch_root_executable_permission
  || permission_flows (permission_of_word word) arch_root_memory_permission

let eval config expression =
  match Assembly_frontend.Expression.evaluate_runtime config expression with
  | Ok value -> Ok value
  | Error message -> diagnostic message

let lower_register = function
  | Named r -> Ok r
  | Register_parameter name -> diagnostic (Printf.sprintf "Unexpanded register parameter $%s." name)

let lower_permission = function
  | Permission_literal p -> Ok p
  | Permission_parameter name ->
      diagnostic (Printf.sprintf "Unexpanded permission parameter $%s." name)

let lower_seal_permission = function
  | Seal_permission_literal p -> Ok p
  | Seal_permission_parameter name ->
      diagnostic (Printf.sprintf "Unexpanded seal permission parameter $%s." name)

let lower_locality = function
  | Locality_literal l -> Ok l
  | Locality_parameter name -> diagnostic (Printf.sprintf "Unexpanded locality parameter $%s." name)

let lower_word_type = function
  | Word_type_literal w -> Ok w
  | Word_type_parameter name ->
      diagnostic (Printf.sprintf "Unexpanded word-type parameter $%s." name)

let lower_constant config = function
  | Expression expression -> eval config expression
  | Permission p -> Ok (Griotte_codec.encode_permission p)
  | Seal_permission p -> Ok (Griotte_codec.encode_seal_permission p)
  | Permission_locality (p, l) ->
      let* p = lower_permission p in
      Result.map (Griotte_codec.encode_permission_locality p) (lower_locality l)
  | Seal_permission_locality (p, l) ->
      let* p = lower_seal_permission p in
      Result.map (Griotte_codec.encode_seal_permission_locality p) (lower_locality l)
  | Word_type w -> Ok (Griotte_codec.encode_word_type w)
  | Locality l -> Ok (Griotte_codec.encode_locality l)
  | Value_parameter name -> diagnostic (Printf.sprintf "Unexpanded value parameter $%s." name)

let lower_operand config = function
  | Register_term r -> Result.map (fun r -> Register r) (lower_register r)
  | Constant_term c -> Result.map (fun c -> Constant c) (lower_constant config c)

let lower_sealable config = function
  | Cap_term (p, l, b, e, a) ->
      let* p = lower_permission p in
      let* l = lower_locality l in
      let* b = eval config b in
      let* e = eval config e in
      Result.map (fun a -> Cap (p, l, b, e, a)) (eval config a)
  | Seal_range_term (p, l, b, e, a) ->
      let* p = lower_seal_permission p in
      let* l = lower_locality l in
      let* b = eval config b in
      let* e = eval config e in
      Result.map (fun a -> SealRange (p, l, b, e, a)) (eval config a)

let lower_word config = function
  | I_term expression -> Result.map (fun z -> I z) (eval config expression)
  | Sealable_term s -> Result.map (fun s -> Sealable s) (lower_sealable config s)
  | Sentry_term (p, l, b, e, a) ->
      let* p = lower_permission p in
      let* l = lower_locality l in
      let* b = eval config b in
      let* e = eval config e in
      Result.map (fun a -> Sentry (p, l, b, e, a)) (eval config a)
  | Sealed_term (otype, s) ->
      let* otype = eval config otype in
      Result.map (fun s -> Sealed (otype, s)) (lower_sealable config s)

let lower_instruction config op =
  let r = lower_register and o = lower_operand config in
  let rr c a b =
    let* a = r a in
    Result.map (fun b -> c (a, b)) (r b)
  in
  let ro c a b =
    let* a = r a in
    Result.map (fun b -> c (a, b)) (o b)
  in
  let roo c a b d =
    let* a = r a in
    let* b = o b in
    Result.map (fun d -> c (a, b, d)) (o d)
  in
  let rrr c a b d =
    let* a = r a in
    let* b = r b in
    Result.map (fun d -> c (a, b, d)) (r d)
  in
  match op with
  | Jalr_term (a, b) -> rr (fun (a, b) -> Jalr (a, b)) a b
  | Jmp_term a -> Result.map (fun a -> Jmp a) (o a)
  | Jnz_term (a, b) -> ro (fun (a, b) -> Jnz (a, b)) a b
  | ReadSR_term (a, s) -> Result.map (fun a -> ReadSR (a, s)) (r a)
  | WriteSR_term (s, a) -> Result.map (fun a -> WriteSR (s, a)) (r a)
  | Move_term (a, b) -> ro (fun (a, b) -> Move (a, b)) a b
  | Load_term (a, b) -> rr (fun (a, b) -> Load (a, b)) a b
  | Store_term (a, b) -> ro (fun (a, b) -> Store (a, b)) a b
  | Add_term (a, b, c) -> roo (fun (a, b, c) -> Add (a, b, c)) a b c
  | Sub_term (a, b, c) -> roo (fun (a, b, c) -> Sub (a, b, c)) a b c
  | Mul_term (a, b, c) -> roo (fun (a, b, c) -> Mul (a, b, c)) a b c
  | LAnd_term (a, b, c) -> roo (fun (a, b, c) -> LAnd (a, b, c)) a b c
  | LOr_term (a, b, c) -> roo (fun (a, b, c) -> LOr (a, b, c)) a b c
  | LShiftL_term (a, b, c) -> roo (fun (a, b, c) -> LShiftL (a, b, c)) a b c
  | LShiftR_term (a, b, c) -> roo (fun (a, b, c) -> LShiftR (a, b, c)) a b c
  | Lt_term (a, b, c) -> roo (fun (a, b, c) -> Lt (a, b, c)) a b c
  | Lea_term (a, b) -> ro (fun (a, b) -> Lea (a, b)) a b
  | Restrict_term (a, b) -> ro (fun (a, b) -> Restrict (a, b)) a b
  | SubSeg_term (a, b, c) -> roo (fun (a, b, c) -> SubSeg (a, b, c)) a b c
  | GetL_term (a, b) -> rr (fun (a, b) -> GetL (a, b)) a b
  | GetB_term (a, b) -> rr (fun (a, b) -> GetB (a, b)) a b
  | GetE_term (a, b) -> rr (fun (a, b) -> GetE (a, b)) a b
  | GetA_term (a, b) -> rr (fun (a, b) -> GetA (a, b)) a b
  | GetP_term (a, b) -> rr (fun (a, b) -> GetP (a, b)) a b
  | GetOType_term (a, b) -> rr (fun (a, b) -> GetOType (a, b)) a b
  | GetWType_term (a, b) -> rr (fun (a, b) -> GetWType (a, b)) a b
  | Seal_term (a, b, c) -> rrr (fun (a, b, c) -> Seal (a, b, c)) a b c
  | UnSeal_term (a, b, c) -> rrr (fun (a, b, c) -> UnSeal (a, b, c)) a b c
  | Fail_term -> Ok Fail
  | Halt_term -> Ok Halt

let zero_registers () =
  let registers = List.init 32 (fun n -> (Reg n, I Z.zero)) |> List.to_seq |> RegMap.of_seq in
  registers |> RegMap.add PC (I Z.zero) |> RegMap.add cnull (I Z.zero)

let initial_registers config =
  let limit = Runtime_config.max_addr config in
  zero_registers ()
  |> RegMap.add PC (Sealable (Cap (arch_root_executable_permission, Global, Z.zero, limit, Z.zero)))
  |> RegMap.add cgp (Sealable (Cap (arch_root_memory_permission, Global, Z.zero, limit, Z.zero)))
  |> RegMap.add ca3 (Sealable (SealRange ((true, true), Global, Z.zero, max_object_type, Z.zero)))
  |> RegMap.add cnull (I Z.zero)

let read_register r state = match r with Reg 0 -> I Z.zero | _ -> RegMap.find r state.registers
let read_system_register r state = SRegMap.find r state.system_registers

let read_memory address state =
  match MemMap.find_opt address state.memory with
  | Some word -> Some word
  | None when Z.sign address >= 0 && Z.compare address (Runtime_config.max_addr state.config) < 0 ->
      Some (I Z.zero)
  | None -> None

let set_register r word state =
  match r with
  | Reg 0 -> { state with registers = RegMap.add cnull (I Z.zero) state.registers }
  | _ -> { state with registers = RegMap.add r word state.registers }

let set_system_register r word state =
  { state with system_registers = SRegMap.add r word state.system_registers }

let set_memory_raw address word state = { state with memory = MemMap.add address word state.memory }
let fail state = { state with status = Failed }

let lower_program config program =
  let rec loop address memory = function
    | [] -> Ok memory
    | statement :: rest ->
        let* word =
          match statement with
          | Word term -> lower_word config term
          | Op term -> (
              let* op = lower_instruction config term in
              match Griotte_codec.encode op with
              | Ok z -> Ok (I z)
              | Error e -> diagnostic (Instruction_codec.error_message e))
        in
        if word_is_derived word then loop Z.(succ address) (MemMap.add address word memory) rest
        else
          diagnostic
            (Printf.sprintf
               "Initial program word at address %s is not derived from a Griotte architectural \
                root."
               (Z.to_string address))
  in
  loop Z.zero MemMap.empty program

let init config program regfile =
  let registers =
    match regfile with None -> initial_registers config | Some _ -> zero_registers ()
  in
  let system_registers = SRegMap.singleton MTDC (I Z.zero) in
  let reg_terms, sreg_terms = Option.value regfile ~default:([], []) in
  let* registers =
    List.fold_left
      (fun acc (r, term) ->
        let* regs = acc in
        let* word = lower_word config term in
        if word_is_derived word then Ok (RegMap.add r word regs)
        else
          diagnostic
            (Printf.sprintf "Initial value for %s is not derived from a Griotte architectural root."
               (Griotte_printer.register r)))
      (Ok registers) reg_terms
  in
  let registers = RegMap.add cnull (I Z.zero) registers in
  let* system_registers =
    List.fold_left
      (fun acc (r, term) ->
        let* regs = acc in
        let* word = lower_word config term in
        if word_is_derived word then Ok (SRegMap.add r word regs)
        else diagnostic "Initial MTDC value is not derived from a Griotte architectural root.")
      (Ok system_registers) sreg_terms
  in
  let* memory = lower_program config program in
  Ok { config; status = Running; registers; system_registers; memory }

let value state = function Register r -> read_register r state | Constant z -> I z
let is_wl (_, w, _, _) = w = WL
let is_dl (_, _, dl, _) = dl = DL
let is_dro (_, _, _, dro) = dro = DRO
let executable (rx, _, _, _) = rx = X || rx = XSR
let can_read (rx, _, _, _) = rx <> Orx
let can_write (_, w, _, _) = w <> Ow
let locality_of_sealable = function Cap (_, l, _, _, _) | SealRange (_, l, _, _, _) -> l

let locality_of_word = function
  | Sealable s | Sealed (_, s) -> Some (locality_of_sealable s)
  | Sentry (_, l, _, _, _) -> Some l
  | I _ -> None

let deep_localize_sealable = function
  | Cap ((rx, w, _, dro), _, b, e, a) -> Cap ((rx, w, DL, dro), Local, b, e, a)
  | SealRange (p, _, b, e, a) -> SealRange (p, Local, b, e, a)

let deep_localize = function
  | Sealable s -> Sealable (deep_localize_sealable s)
  | Sentry (p, _, b, e, a) -> Sentry (p, Local, b, e, a)
  | Sealed (o, Cap (p, _, b, e, a)) -> Sealed (o, Cap (p, Local, b, e, a))
  | Sealed (o, SealRange (p, _, b, e, a)) -> Sealed (o, SealRange (p, Local, b, e, a))
  | I _ as w -> w

let read_only = function
  | Sealable (Cap ((rx, _, dl, _), l, b, e, a)) -> Sealable (Cap ((rx, Ow, dl, DRO), l, b, e, a))
  | word -> word

let loaded_word permission word =
  let word = if is_dl permission then deep_localize word else word in
  if is_dro permission then read_only word else word

let pc_next state =
  match read_register PC state with
  | Sealable (Cap (p, l, b, e, a)) -> set_register PC (Sealable (Cap (p, l, b, e, Z.succ a))) state
  | _ -> fail state

let write_next r word state = pc_next (set_register r word state)
let enter = function Sentry (p, l, b, e, a) -> Sealable (Cap (p, l, b, e, a)) | word -> word

let valid_pc state =
  match read_register PC state with
  | Sealable (Cap (p, _, b, e, a)) when executable p ->
      b <= a && a < e && Option.is_some (read_memory a state)
  | _ -> false

let authorized_system state =
  match read_register PC state with
  | Sealable (Cap ((XSR, _, _, _), _, _, _, _)) -> true
  | _ -> false

let word_type = function
  | I _ -> W_I
  | Sealable (Cap _) -> W_Cap
  | Sealable (SealRange _) -> W_SealRange
  | Sealed _ -> W_Sealed
  | Sentry _ -> W_Sentry

let arithmetic result r a b state =
  match (value state a, value state b) with
  | I x, I y -> ( match result x y with Some z -> write_next r (I z) state | None -> fail state)
  | _ -> fail state

let rec execute instruction state =
  let get r = read_register r state and v o = value state o in
  match instruction with
  | Fail -> fail state
  | Halt -> { state with status = Halted }
  | Jalr (dst, src) -> (
      match get PC with
      | Sealable (Cap (p, l, b, e, a)) ->
          set_register PC (enter (get src)) (set_register dst (Sentry (p, l, b, e, Z.succ a)) state)
      | _ -> fail state)
  | Jmp offset -> (
      match (v offset, get PC) with
      | I z, Sealable (Cap (p, l, b, e, a)) ->
          set_register PC (Sealable (Cap (p, l, b, e, Z.add a z))) state
      | _ -> fail state)
  | Jnz (test, offset) -> (
      match get test with
      | I z when Z.equal z Z.zero -> pc_next state
      | _ -> execute (Jmp offset) state)
  | ReadSR (r, sr) ->
      if authorized_system state then write_next r (read_system_register sr state) state
      else fail state
  | WriteSR (sr, r) ->
      if authorized_system state then pc_next (set_system_register sr (get r) state) else fail state
  | Move (r, o) -> write_next r (v o) state
  | Load (dst, src) -> (
      match get src with
      | Sealable (Cap (p, _, b, e, a)) when can_read p && b <= a && a < e -> (
          match read_memory a state with
          | Some w -> write_next dst (loaded_word p w) state
          | None -> fail state)
      | _ -> fail state)
  | Store (dst, o) -> (
      match get dst with
      | Sealable (Cap (p, _, b, e, a)) when can_write p && b <= a && a < e -> (
          let word = v o in
          match locality_of_word word with
          | Some Local when not (is_wl p) -> fail state
          | _ -> pc_next (set_memory_raw a word state))
      | _ -> fail state)
  | Restrict (r, o) -> (
      match (get r, v o) with
      | Sealable (Cap (p, l, b, e, a)), I encoded -> (
          match Griotte_codec.decode_permission_locality encoded with
          | Ok (p', l') when permission_flows p' p && locality_flows l' l ->
              write_next r (Sealable (Cap (p', l', b, e, a))) state
          | _ -> fail state)
      | Sealable (SealRange (p, l, b, e, a)), I encoded -> (
          match Griotte_codec.decode_seal_permission_locality encoded with
          | Ok (p', l') when seal_permission_flows p' p && locality_flows l' l ->
              write_next r (Sealable (SealRange (p', l', b, e, a))) state
          | _ -> fail state)
      | _ -> fail state)
  | SubSeg (r, o1, o2) -> (
      match (get r, v o1, v o2) with
      | Sealable (Cap (p, l, b, e, a)), I b', I e' when b <= b' && Z.sign e' >= 0 && Z.sign e >= 0
        ->
          write_next r (Sealable (Cap (p, l, b', e', a))) state
      | Sealable (SealRange (p, l, b, e, a)), I b', I e'
        when b <= b' && Z.sign e' >= 0 && Z.sign e >= 0 ->
          write_next r (Sealable (SealRange (p, l, b', e', a))) state
      | _ -> fail state)
  | Lea (r, o) -> (
      match (get r, v o) with
      | Sealable (Cap (p, l, b, e, a)), I z ->
          write_next r (Sealable (Cap (p, l, b, e, Z.add a z))) state
      | Sealable (SealRange (p, l, b, e, a)), I z ->
          write_next r (Sealable (SealRange (p, l, b, e, Z.add a z))) state
      | _ -> fail state)
  | Add (r, a, b) -> arithmetic (fun x y -> Some Z.(x + y)) r a b state
  | Sub (r, a, b) -> arithmetic (fun x y -> Some Z.(x - y)) r a b state
  | Mul (r, a, b) -> arithmetic (fun x y -> Some Z.(x * y)) r a b state
  | Lt (r, a, b) -> arithmetic (fun x y -> Some (if Z.lt x y then Z.one else Z.zero)) r a b state
  | LAnd (r, a, b) -> arithmetic (fun x y -> Some (Z.logand x y)) r a b state
  | LOr (r, a, b) -> arithmetic (fun x y -> Some (Z.logor x y)) r a b state
  | LShiftL (r, a, b) ->
      arithmetic
        (fun x y -> try Some (Z.of_int (Z.to_int x lsl Z.to_int y)) with _ -> None)
        r a b state
  | LShiftR (r, a, b) ->
      arithmetic
        (fun x y -> try Some (Z.of_int (Z.to_int x lsr Z.to_int y)) with _ -> None)
        r a b state
  | GetL (dst, src) -> (
      match locality_of_word (get src) with
      | Some l -> write_next dst (I (Griotte_codec.encode_locality l)) state
      | None -> fail state)
  | GetB (dst, src) -> (
      match get src with
      | Sealable (Cap (_, _, b, _, _))
      | Sealable (SealRange (_, _, b, _, _))
      | Sentry (_, _, b, _, _) ->
          write_next dst (I b) state
      | _ -> fail state)
  | GetE (dst, src) -> (
      match get src with
      | Sealable (Cap (_, _, _, e, _))
      | Sealable (SealRange (_, _, _, e, _))
      | Sentry (_, _, _, e, _) ->
          write_next dst (I e) state
      | _ -> fail state)
  | GetA (dst, src) -> (
      match get src with
      | Sealable (Cap (_, _, _, _, a))
      | Sealable (SealRange (_, _, _, _, a))
      | Sentry (_, _, _, _, a) ->
          write_next dst (I a) state
      | _ -> fail state)
  | GetP (dst, src) -> (
      match get src with
      | Sealable (Cap (p, _, _, _, _)) | Sentry (p, _, _, _, _) ->
          write_next dst (I (Griotte_codec.encode_permission p)) state
      | Sealable (SealRange (p, _, _, _, _)) ->
          write_next dst (I (Griotte_codec.encode_seal_permission p)) state
      | _ -> fail state)
  | GetOType (dst, src) -> (
      match get src with
      | Sealed (o, _) -> write_next dst (I o) state
      | _ -> write_next dst (I Z.minus_one) state)
  | GetWType (dst, src) ->
      write_next dst (I (Griotte_codec.encode_word_type (word_type (get src)))) state
  | Seal (dst, range, value_reg) -> (
      match (get range, get value_reg) with
      | Sealable (SealRange ((true, _), _, b, e, a)), Sealable sealable when b <= a && a < e ->
          write_next dst (Sealed (a, sealable)) state
      | _ -> fail state)
  | UnSeal (dst, range, sealed_reg) -> (
      match (get range, get sealed_reg) with
      | Sealable (SealRange ((_, true), _, b, e, a)), Sealed (o, sealable)
        when b <= a && a < e && Z.equal a o ->
          write_next dst (Sealable sealable) state
      | _ -> fail state)

let step state =
  if state.status <> Running then
    Error
      (Machine_backend.Stopped
         (if state.status = Halted then Machine_view.Halted else Machine_view.Failed))
  else if not (valid_pc state) then Ok (fail state)
  else
    match read_register PC state with
    | Sealable (Cap (_, _, _, _, address)) -> (
        match read_memory address state with
        | Some (I encoded) -> (
            match Griotte_codec.decode encoded with
            | Ok op -> Ok (execute op state)
            | Error _ -> Ok (fail state))
        | _ -> Ok (fail state))
    | _ -> Ok (fail state)

let rec step_n count state =
  if count < 0 then Error (Machine_backend.Backend_error "step count must be non-negative")
  else if count = 0 then Ok state
  else
    match step state with
    | Ok next -> step_n (count - 1) next
    | Error (Machine_backend.Stopped _) -> Ok state
    | Error _ as error -> error
