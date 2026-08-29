open Ast

module RegMap = Map.Make (struct
  type t = register

  let compare a b =
    match (a, b) with PC, PC -> 0 | PC, _ -> -1 | _, PC -> 1 | Reg a, Reg b -> Int.compare a b
end)

module MemMap = Map.Make (Z)

type status = Running | Halted | Failed

type t = {
  config : Runtime_config.t;
  status : status;
  registers : word RegMap.t;
  memory : word MemMap.t;
}

let diagnostic message = Error [ Diagnostic.error message ]

let eval config expression =
  match Assembly_frontend.Expression.evaluate_runtime config expression with
  | Ok z -> Ok z
  | Error message -> diagnostic message

let ( let* ) = Result.bind

let lower_permission = function
  | Permission_literal permission -> Ok permission
  | Permission_parameter name ->
      diagnostic (Printf.sprintf "Unexpanded permission parameter $%s." name)

let lower_seal_permission = function
  | Seal_permission_literal permission -> Ok permission
  | Seal_permission_parameter name ->
      diagnostic (Printf.sprintf "Unexpanded seal-permission parameter $%s." name)

let lower_locality = function
  | Locality locality -> Ok locality
  | Locality_parameter name -> diagnostic (Printf.sprintf "Unexpanded locality parameter $%s." name)

let lower_sealable config = function
  | Cap_term (p, l, b, e, a) ->
      let* p = lower_permission p in
      let* l = lower_locality l in
      let* b = eval config b in
      let* e = eval config e in
      let* a = eval config a in
      Ok (Cap (p, l, b, e, a))
  | SealRange_term (p, l, b, e, a) ->
      let* p = lower_seal_permission p in
      let* l = lower_locality l in
      let* b = eval config b in
      let* e = eval config e in
      let* a = eval config a in
      Ok (SealRange (p, l, b, e, a))

let lower_word config = function
  | I_term e -> Result.map (fun z -> I z) (eval config e)
  | Sealable_term s -> Result.map (fun s -> Sealable s) (lower_sealable config s)
  | Sealed_term (o, s) ->
      let* o = eval config o in
      Result.map (fun s -> Sealed (o, s)) (lower_sealable config s)

let lower_register = function
  | Named r -> Ok r
  | Register_parameter n -> diagnostic (Printf.sprintf "Unexpanded register parameter $%s." n)

let lower_constant config = function
  | Expression e -> eval config e
  | Permission p -> Ok (Codec.encode_permission p)
  | Seal_permission p -> Ok (Codec.encode_seal_permission p)
  | Word_type w -> Ok (Codec.encode_word_type w)
  | Permission_locality (p, l) ->
      let* p = lower_permission p in
      Result.map (Codec.encode_permission_locality p) (lower_locality l)
  | Seal_permission_locality (p, l) ->
      let* p = lower_seal_permission p in
      Result.map (Codec.encode_seal_permission_locality p) (lower_locality l)
  | Parameterized_permission_locality (name, _) ->
      diagnostic (Printf.sprintf "Unexpanded permission parameter $%s." name)
  | Locality_constant l -> Ok (Codec.encode_locality l)
  | Value_parameter n -> diagnostic (Printf.sprintf "Unexpanded value parameter $%s." n)

let lower_operand config = function
  | Register_term r -> Result.map (fun r -> Register r) (lower_register r)
  | Constant_term c -> Result.map (fun z -> Constant z) (lower_constant config c)

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
  | Jmp_term a -> Result.map (fun a -> Jmp a) (r a)
  | Jnz_term (a, b) -> rr (fun (a, b) -> Jnz (a, b)) a b
  | Move_term (a, b) -> ro (fun (a, b) -> Move (a, b)) a b
  | Load_term (a, b) -> rr (fun (a, b) -> Load (a, b)) a b
  | Store_term (a, b) -> ro (fun (a, b) -> Store (a, b)) a b
  | Add_term (a, b, c) -> roo (fun (a, b, c) -> Add (a, b, c)) a b c
  | Sub_term (a, b, c) -> roo (fun (a, b, c) -> Sub (a, b, c)) a b c
  | Mul_term (a, b, c) -> roo (fun (a, b, c) -> Mul (a, b, c)) a b c
  | Rem_term (a, b, c) -> roo (fun (a, b, c) -> Rem (a, b, c)) a b c
  | Div_term (a, b, c) -> roo (fun (a, b, c) -> Div (a, b, c)) a b c
  | Lt_term (a, b, c) -> roo (fun (a, b, c) -> Lt (a, b, c)) a b c
  | Lea_term (a, b) -> ro (fun (a, b) -> Lea (a, b)) a b
  | Restrict_term (a, b) -> ro (fun (a, b) -> Restrict (a, b)) a b
  | SubSeg_term (a, b, c) -> roo (fun (a, b, c) -> SubSeg (a, b, c)) a b c
  | GetB_term (a, b) -> rr (fun (a, b) -> GetB (a, b)) a b
  | GetE_term (a, b) -> rr (fun (a, b) -> GetE (a, b)) a b
  | GetA_term (a, b) -> rr (fun (a, b) -> GetA (a, b)) a b
  | GetL_term (a, b) -> rr (fun (a, b) -> GetL (a, b)) a b
  | GetP_term (a, b) -> rr (fun (a, b) -> GetP (a, b)) a b
  | GetOType_term (a, b) -> rr (fun (a, b) -> GetOType (a, b)) a b
  | GetWType_term (a, b) -> rr (fun (a, b) -> GetWType (a, b)) a b
  | Seal_term (a, b, c) -> rrr (fun (a, b, c) -> Seal (a, b, c)) a b c
  | UnSeal_term (a, b, c) -> rrr (fun (a, b, c) -> UnSeal (a, b, c)) a b c
  | Invoke_term (a, b) -> rr (fun (a, b) -> Invoke (a, b)) a b
  | Fail_term -> Ok Fail
  | Halt_term -> Ok Halt

let lower_program config program =
  let rec loop address memory = function
    | [] -> Ok memory
    | statement :: rest -> (
        match statement with
        | Op term -> (
            let* op = lower_instruction config term in
            match Codec.encode op with
            | Ok z -> loop Z.(succ address) (MemMap.add address (I z) memory) rest
            | Error e -> diagnostic (Instruction_codec.error_message e))
        | Word term ->
            let* word = lower_word config term in
            loop Z.(succ address) (MemMap.add address word memory) rest)
  in
  loop Z.zero MemMap.empty program

let init config program regfile =
  let max_addr = Runtime_config.max_addr config and stack_addr = Runtime_config.stack_addr config in
  let registers = List.init 32 (fun n -> (Reg n, I Z.zero)) |> List.to_seq |> RegMap.of_seq in
  let registers =
    registers
    |> RegMap.add PC (Sealable (Cap (RWX, Global, Z.zero, stack_addr, Z.zero)))
    |> RegMap.add (Reg 0) (Sealable (SealRange ((true, true), Global, Z.zero, stack_addr, Z.zero)))
    |> RegMap.add (Reg 31) (Sealable (Cap (RWLX, Local, stack_addr, max_addr, stack_addr)))
  in
  let* registers =
    match regfile with
    | None -> Ok registers
    | Some entries ->
        List.fold_left
          (fun result (r, w) ->
            let* registers = result in
            Result.map (fun word -> RegMap.add r word registers) (lower_word config w))
          (Ok registers) entries
  in
  Result.map
    (fun memory -> { config; status = Running; registers; memory })
    (lower_program config program)

let read_register r state = RegMap.find r state.registers

let read_memory a state =
  match MemMap.find_opt a state.memory with
  | Some w -> Some w
  | None when Z.sign a >= 0 && Z.compare a (Runtime_config.max_addr state.config) < 0 ->
      Some (I Z.zero)
  | None -> None

let set_register r word state = { state with registers = RegMap.add r word state.registers }
let set_memory_raw a word state = { state with memory = MemMap.add a word state.memory }

let pc_next state =
  match read_register PC state with
  | Sealable (Cap (p, l, b, e, a)) ->
      {
        state with
        registers = RegMap.add PC (Sealable (Cap (p, l, b, e, Z.succ a))) state.registers;
      }
  | _ -> { state with status = Failed }

let fail state = { state with status = Failed }
let word_of_operand state = function Register r -> read_register r state | Constant z -> I z

let permission_flows requested current =
  match requested with
  | O -> true
  | E -> ( match current with E | RX | RWX | RWLX -> true | _ -> false)
  | RO -> ( match current with RO | RX | RW | RWX | RWL | RWLX -> true | _ -> false)
  | RX -> ( match current with RX | RWX | RWLX -> true | _ -> false)
  | RW -> ( match current with RW | RWX | RWL | RWLX -> true | _ -> false)
  | RWX -> ( match current with RWX | RWLX -> true | _ -> false)
  | RWL -> ( match current with RWL | RWLX -> true | _ -> false)
  | RWLX -> current = RWLX

let seal_permission_flows (s, u) (s', u') = ((not s) || s') && ((not u) || u')
let can_read = function RO | RX | RW | RWX | RWL | RWLX -> true | _ -> false
let can_write = function RW | RWX | RWL | RWLX -> true | _ -> false
let can_store_local = function RWL | RWLX -> true | _ -> false
let is_exec = function RX | RWX | RWLX -> true | _ -> false

let word_type = function
  | I _ -> Integer
  | Sealable (Cap _) -> Capability
  | Sealable (SealRange _) -> Seal_range
  | Sealed _ -> Sealed

let bounds = function Cap (_, _, b, e, a) | SealRange (_, _, b, e, a) -> (b, e, a)
let locality = function Cap (_, l, _, _, _) | SealRange (_, l, _, _, _) -> l

let with_cursor s cursor =
  match s with
  | Cap (p, l, b, e, _) -> Cap (p, l, b, e, cursor)
  | SealRange (p, l, b, e, _) -> SealRange (p, l, b, e, cursor)

let with_bounds s base limit =
  match s with
  | Cap (p, l, _, _, a) -> Cap (p, l, base, limit, a)
  | SealRange (p, l, _, _, a) -> SealRange (p, l, base, limit, a)

let valid_pc state =
  match read_register PC state with
  | Sealable (Cap ((RX | RWX | RWLX), _, b, e, a)) ->
      b <= a && a < e && Option.is_some (read_memory a state)
  | _ -> false

let write_next r w state = pc_next (set_register r w state)

let rec execute op state =
  let get = read_register and value = word_of_operand state in
  match op with
  | Fail -> fail state
  | Halt -> { state with status = Halted }
  | Move (r, o) -> write_next r (value o) state
  | Load (dst, src) -> (
      match get src state with
      | Sealable (Cap (p, _, b, e, a)) when can_read p && b <= a && a < e -> (
          match read_memory a state with Some w -> write_next dst w state | None -> fail state)
      | _ -> fail state)
  | Store (dst, o) -> (
      match get dst state with
      | Sealable (Cap (p, _, b, e, a)) when can_write p && b <= a && a < e ->
          let w = value o in
          if
            (match w with Sealable s | Sealed (_, s) -> locality s = Local | _ -> false)
            && not (can_store_local p)
          then fail state
          else pc_next (set_memory_raw a w state)
      | _ -> fail state)
  | Jmp r -> (
      match get r state with
      | Sealable (Cap (E, l, b, e, a)) -> set_register PC (Sealable (Cap (RX, l, b, e, a))) state
      | w -> set_register PC w state)
  | Jnz (r, test) -> (
      match get test state with
      | I z when Z.equal z Z.zero -> pc_next state
      | _ -> execute (Jmp r) state)
  | Add (r, a, b) | Sub (r, a, b) | Mul (r, a, b) | Rem (r, a, b) | Div (r, a, b) | Lt (r, a, b)
    -> (
      match (value a, value b) with
      | I x, I y -> (
          let result =
            match op with
            | Add _ -> Some Z.(x + y)
            | Sub _ -> Some Z.(x - y)
            | Mul _ -> Some Z.(x * y)
            | Rem _ when not (Z.equal y Z.zero) -> Some Z.(rem x y)
            | Div _ when not (Z.equal y Z.zero) -> Some Z.(div x y)
            | Lt _ -> Some (if Z.lt x y then Z.one else Z.zero)
            | _ -> None
          in
          match result with Some z -> write_next r (I z) state | None -> fail state)
      | _ -> fail state)
  | Lea (r, o) -> (
      match (get r state, value o) with
      | Sealable s, I z -> (
          match s with
          | Cap (E, _, _, _, _) -> fail state
          | _ ->
              write_next r
                (Sealable
                   (with_cursor s
                      Z.(
                        let _, _, a = bounds s in
                        a + z)))
                state)
      | _ -> fail state)
  | Restrict (r, o) -> (
      match (get r state, value o) with
      | Sealable (Cap (p, l, b, e, a)), I z -> (
          match Codec.decode_permission_locality z with
          | Ok (p', l') when permission_flows p' p && (l' = l || (l = Local && l' = Global)) ->
              write_next r (Sealable (Cap (p', l', b, e, a))) state
          | _ -> fail state)
      | Sealable (SealRange (p, l, b, e, a)), I z -> (
          match Codec.decode_seal_permission_locality z with
          | Ok (p', l') when seal_permission_flows p' p && (l' = l || (l = Local && l' = Global)) ->
              write_next r (Sealable (SealRange (p', l', b, e, a))) state
          | _ -> fail state)
      | _ -> fail state)
  | SubSeg (r, o1, o2) -> (
      match (get r state, value o1, value o2) with
      | Sealable (Cap (E, _, _, _, _)), _, _ -> fail state
      | Sealable s, I b', I e' ->
          let b, e, _ = bounds s in
          if b <= b' && Z.sign e' >= 0 && Z.sign e >= 0 then
            write_next r (Sealable (with_bounds s b' e')) state
          else fail state
      | _ -> fail state)
  | GetB (r, s) -> (
      match get s state with
      | Sealable sb ->
          let b, _, _ = bounds sb in
          write_next r (I b) state
      | _ -> fail state)
  | GetE (r, s) -> (
      match get s state with
      | Sealable sb ->
          let _, e, _ = bounds sb in
          write_next r (I e) state
      | _ -> fail state)
  | GetA (r, s) -> (
      match get s state with
      | Sealable sb ->
          let _, _, a = bounds sb in
          write_next r (I a) state
      | _ -> fail state)
  | GetP (r, s) -> (
      match get s state with
      | Sealable (Cap (p, _, _, _, _)) ->
          write_next r (I (Codec.encode_permission p)) state
      | Sealable (SealRange (p, _, _, _, _)) ->
          write_next r (I (Codec.encode_seal_permission p)) state
      | _ -> fail state)
  | GetL (r, s) -> (
      match get s state with
      | Sealable sb | Sealed (_, sb) ->
          write_next r (I (Codec.encode_locality (locality sb))) state
      | _ -> fail state)
  | GetOType (r, s) -> (
      match get s state with
      | Sealed (o, _) -> write_next r (I o) state
      | _ -> write_next r (I Z.minus_one) state)
  | GetWType (r, s) ->
      write_next r (I (Codec.encode_word_type (word_type (get s state)))) state
  | Seal (dst, seal, value_reg) -> (
      match (get seal state, get value_reg state) with
      | Sealable (SealRange ((true, _), _, b, e, a)), Sealable sb when b <= a && a < e ->
          write_next dst (Sealed (a, sb)) state
      | _ -> fail state)
  | UnSeal (dst, seal, value_reg) -> (
      match (get seal state, get value_reg state) with
      | Sealable (SealRange ((_, true), _, b, e, a)), Sealed (o, sb)
        when b <= a && a < e && Z.equal a o ->
          write_next dst (Sealable sb) state
      | _ -> fail state)
  | Invoke (code, data) -> (
      match (get code state, get data state) with
      | Sealed (o, Cap (p, l, b, e, a)), Sealed (o', sb) when Z.equal o o' && is_exec p -> (
          match sb with
          | Cap (p', _, _, _, _) when not (is_exec p') ->
              let state = set_register data (Sealable sb) state in
              set_register PC (Sealable (Cap ((if p = E then RX else p), l, b, e, a))) state
          | _ -> fail state)
      | _ -> fail state)

let step state =
  match state.status with
  | Halted -> Error (Machine_backend.Stopped Machine_view.Halted)
  | Failed -> Error (Machine_backend.Stopped Machine_view.Failed)
  | Running -> (
      if not (valid_pc state) then Ok (fail state)
      else
        match read_register PC state with
        | Sealable (Cap (_, _, _, _, a)) -> (
            match read_memory a state with
            | Some (I encoded) -> (
                match Codec.decode encoded with
                | Ok op -> Ok (execute op state)
                | Error _ -> Ok (fail state))
            | _ -> Ok (fail state))
        | _ -> Ok (fail state))

let rec step_n count state =
  if count < 0 then Error (Machine_backend.Backend_error "step count must be non-negative")
  else if count = 0 then Ok state
  else
    match step state with
    | Ok next -> step_n (count - 1) next
    | Error (Machine_backend.Stopped _) -> Ok state
    | Error _ as e -> e

let get_exec_state state = state.status
let get_regfile state = state.registers
let get_memory state = state.memory
let read_reg = read_register
let read_mem = read_memory
let set_reg = set_register
let set_mem = set_memory_raw

let rec run state =
  match step state with
  | Ok next -> run next
  | Error (Machine_backend.Stopped _) -> state
  | Error _ -> state
