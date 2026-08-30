open Ast

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

(* The Rocq machine represents both addresses and object types as [FinZ 2000000]. *)
let finite_address_bound = Z.of_int 2_000_000
let finite_object_type_bound = Z.of_int 2_000_000
let in_finite_domain bound value = Z.sign value >= 0 && Z.compare value bound < 0

(* A right shift beyond every representable bit has already reached its
   sign-extension fixed point, so it needs no machine-integer conversion. *)
let shift_right_nonnegative value count =
  if Z.fits_int count then try Some (Z.shift_right value (Z.to_int count)) with _ -> None
  else Some (if Z.sign value < 0 then Z.minus_one else Z.zero)

let shift_left_z value count =
  match Z.sign count with
  | 0 -> Some value
  | -1 -> shift_right_nonnegative value (Z.neg count)
  | _ when Z.equal value Z.zero -> Some Z.zero
  (* A nonzero result this wide cannot be represented by the runtime. *)
  | _ when not (Z.fits_int count) -> None
  | _ -> ( try Some (Z.shift_left value (Z.to_int count)) with _ -> None)

let shift_right_z value count = shift_left_z value (Z.neg count)

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

let init config program regfile =
  let registers =
    match regfile with None -> initial_registers config | Some _ -> zero_registers ()
  in
  let system_registers = SRegMap.singleton MTDC (I Z.zero) in
  let reg_words, sreg_words = Option.value regfile ~default:([], []) in
  let* registers =
    List.fold_left
      (fun acc (r, word) ->
        let* regs = acc in
        if word_is_derived word then Ok (RegMap.add r word regs)
        else
          diagnostic
            (Printf.sprintf "Initial value for %s is not derived from a Griotte architectural root."
               (Printer.register r)))
      (Ok registers) reg_words
  in
  let registers = RegMap.add cnull (I Z.zero) registers in
  let* system_registers =
    List.fold_left
      (fun acc (r, word) ->
        let* regs = acc in
        if word_is_derived word then Ok (SRegMap.add r word regs)
        else diagnostic "Initial MTDC value is not derived from a Griotte architectural root.")
      (Ok system_registers) sreg_words
  in
  let* memory =
    List.fold_left
      (fun result word ->
        let* address, memory = result in
        if word_is_derived word then Ok (Z.succ address, MemMap.add address word memory)
        else
          diagnostic
            (Printf.sprintf
               "Initial program word at address %s is not derived from a Griotte architectural \
                root."
               (Z.to_string address)))
      (Ok (Z.zero, MemMap.empty))
      program
    |> Result.map snd
  in
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
          match Codec.decode_permission_locality encoded with
          | Ok (p', l') when permission_flows p' p && locality_flows l' l ->
              write_next r (Sealable (Cap (p', l', b, e, a))) state
          | _ -> fail state)
      | Sealable (SealRange (p, l, b, e, a)), I encoded -> (
          match Codec.decode_seal_permission_locality encoded with
          | Ok (p', l') when seal_permission_flows p' p && locality_flows l' l ->
              write_next r (Sealable (SealRange (p', l', b, e, a))) state
          | _ -> fail state)
      | _ -> fail state)
  | SubSeg (r, o1, o2) -> (
      match (get r, v o1, v o2) with
      | Sealable (Cap (p, l, b, e, a)), I b', I e'
        when in_finite_domain finite_address_bound b'
             && in_finite_domain finite_address_bound e'
             && b <= b' && e' <= e ->
          write_next r (Sealable (Cap (p, l, b', e', a))) state
      | Sealable (SealRange (p, l, b, e, a)), I b', I e'
        when in_finite_domain finite_object_type_bound b'
             && in_finite_domain finite_object_type_bound e'
             && b <= b' && e' <= e ->
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
  | LShiftL (r, a, b) -> arithmetic shift_left_z r a b state
  | LShiftR (r, a, b) -> arithmetic shift_right_z r a b state
  | GetL (dst, src) -> (
      match locality_of_word (get src) with
      | Some l -> write_next dst (I (Codec.encode_locality l)) state
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
          write_next dst (I (Codec.encode_permission p)) state
      | Sealable (SealRange (p, _, _, _, _)) ->
          write_next dst (I (Codec.encode_seal_permission p)) state
      | _ -> fail state)
  | GetOType (dst, src) -> (
      match get src with
      | Sealed (o, _) -> write_next dst (I o) state
      | _ -> write_next dst (I Z.minus_one) state)
  | GetWType (dst, src) ->
      write_next dst (I (Codec.encode_word_type (word_type (get src)))) state
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
            match Codec.decode encoded with
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
