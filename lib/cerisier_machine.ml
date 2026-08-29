open Cerisier_ast

module RegMap = Map.Make (struct
  type t = register

  let compare a b =
    match (a, b) with PC, PC -> 0 | PC, _ -> -1 | _, PC -> 1 | Reg a, Reg b -> Int.compare a b
end)

module MemMap = Map.Make (Z)
module ETableMap = Map.Make (Z)

type status = Running | Halted | Failed

type t = {
  config : Runtime_config.t;
  status : status;
  registers : word RegMap.t;
  memory : word MemMap.t;
  enclave_table : Z.t ETableMap.t;
  enclave_counter : Z.t;
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
  | Permission p -> Ok (Cerisier_codec.encode_permission p)
  | Seal_permission p -> Ok (Cerisier_codec.encode_seal_permission p)
  | Word_type w -> Ok (Cerisier_codec.encode_word_type w)
  | Permission_locality (p, l) ->
      let* p = lower_permission p in
      Result.map (Cerisier_codec.encode_permission_locality p) (lower_locality l)
  | Seal_permission_locality (p, l) ->
      let* p = lower_seal_permission p in
      Result.map (Cerisier_codec.encode_seal_permission_locality p) (lower_locality l)
  | Parameterized_permission_locality (name, _) ->
      diagnostic (Printf.sprintf "Unexpanded permission parameter $%s." name)
  | Locality_constant l -> Ok (Cerisier_codec.encode_locality l)
  | Value_parameter n -> diagnostic (Printf.sprintf "Unexpanded value parameter $%s." n)

let lower_operand config = function
  | Register_term r -> Result.map (fun r -> Register r) (lower_register r)
  | Constant_term c -> Result.map (fun z -> Const z) (lower_constant config c)

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
  | LoadU_term (a, b, c) ->
      let* a = r a in
      let* b = r b in
      Result.map (fun c -> LoadU (a, b, c)) (o c)
  | StoreU_term (a, b, c) -> roo (fun (a, b, c) -> StoreU (a, b, c)) a b c
  | PromoteU_term a -> Result.map (fun a -> PromoteU a) (r a)
  | EInit_term (a, b) -> rr (fun (a, b) -> EInit (a, b)) a b
  | EDeInit_term a -> Result.map (fun a -> EDeInit a) (r a)
  | EStoreId_term (a, b) -> rr (fun (a, b) -> EStoreId (a, b)) a b
  | IsUnique_term (a, b) -> rr (fun (a, b) -> IsUnique (a, b)) a b
  | Fail_term -> Ok Fail
  | Halt_term -> Ok Halt

let lower_program config program =
  let rec loop address memory = function
    | [] -> Ok memory
    | statement :: rest -> (
        match statement with
        | Op term -> (
            let* op = lower_instruction config term in
            match Cerisier_codec.encode op with
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
    |> RegMap.add (Reg 31) (Sealable (Cap (URWLX, Directed, stack_addr, max_addr, stack_addr)))
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
    (fun memory ->
      {
        config;
        status = Running;
        registers;
        memory;
        enclave_table = ETableMap.empty;
        enclave_counter = Z.zero;
      })
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
let word_of_operand state = function Register r -> read_register r state | Const z -> I z

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
  | URW -> (
      match current with URW | URWL | URWX | URWLX | RW | RWX | RWL | RWLX -> true | _ -> false)
  | URWL -> ( match current with URWL | URWLX | RWL | RWLX -> true | _ -> false)
  | URWX -> ( match current with URWX | URWLX | RWX | RWLX -> true | _ -> false)
  | URWLX -> ( match current with URWLX | RWLX -> true | _ -> false)

let seal_permission_flows (s, u) (s', u') = ((not s) || s') && ((not u) || u')
let can_read = function RO | RX | RW | RWX | RWL | RWLX -> true | _ -> false
let can_write = function RW | RWX | RWL | RWLX -> true | _ -> false
let can_store_local = function RWL | RWLX | URWL | URWLX -> true | _ -> false
let is_exec = function RX | RWX | RWLX | URWX | URWLX -> true | _ -> false
let is_uninitialized = function URW | URWL | URWX | URWLX -> true | _ -> false
let promote = function URW -> RW | URWL -> RWL | URWX -> RWX | URWLX -> RWLX | p -> p

let locality_flows requested current =
  match requested with
  | Directed -> true
  | Local -> current <> Directed
  | Global -> current = Global

let readable_limit = function
  | Sealable (Cap (p, _, _, e, a)) when is_uninitialized p -> Z.min a e
  | Sealable (Cap (_, _, _, e, _)) -> e
  | _ -> Z.zero

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

let capability_of_word = function
  | Sealable (Cap (_, _, b, e, _)) | Sealed (_, Cap (_, _, b, e, _)) -> Some (b, e)
  | _ -> None

let overlaps left right =
  match (capability_of_word left, capability_of_word right) with
  | Some (b1, e1), Some (b2, e2) -> if b1 < b2 then b2 < e1 else b1 < e2
  | _ -> false

let unique_register state source =
  let word = read_register source state in
  RegMap.for_all
    (fun register other -> register = source || not (overlaps word other))
    state.registers
  && MemMap.for_all (fun _ other -> not (overlaps word other)) state.memory

let unique_address state address =
  match MemMap.find_opt address state.memory with
  | None -> false
  | Some word ->
      RegMap.for_all (fun _ other -> not (overlaps word other)) state.registers
      && MemMap.for_all
           (fun other_address other -> Z.equal address other_address || not (overlaps word other))
           state.memory

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
      | Sealable (Cap (p, _, b, e, a)) when can_write p && b <= a && a < e -> (
          let w = value o in
          match w with
          | (Sealable s | Sealed (_, s))
            when (locality s = Local
                 || (locality s = Directed && match s with SealRange _ -> true | _ -> false))
                 && not (can_store_local p) ->
              fail state
          | Sealable (Cap (_, Directed, _, _, _)) | Sealed (_, Cap (_, Directed, _, _, _)) ->
              if can_store_local p && readable_limit w <= a then pc_next (set_memory_raw a w state)
              else fail state
          | _ -> pc_next (set_memory_raw a w state))
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
          match Cerisier_codec.decode_permission_locality z with
          | Ok (p', l') when permission_flows p' p && locality_flows l' l ->
              write_next r (Sealable (Cap (p', l', b, e, a))) state
          | _ -> fail state)
      | Sealable (SealRange (p, l, b, e, a)), I z -> (
          match Cerisier_codec.decode_seal_permission_locality z with
          | Ok (p', l') when seal_permission_flows p' p && locality_flows l' l ->
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
          write_next r (I (Cerisier_codec.encode_permission p)) state
      | Sealable (SealRange (p, _, _, _, _)) ->
          write_next r (I (Cerisier_codec.encode_seal_permission p)) state
      | _ -> fail state)
  | GetL (r, s) -> (
      match get s state with
      | Sealable sb | Sealed (_, sb) ->
          write_next r (I (Cerisier_codec.encode_locality (locality sb))) state
      | _ -> fail state)
  | GetOType (r, s) -> (
      match get s state with
      | Sealed (o, _) -> write_next r (I o) state
      | _ -> write_next r (I Z.minus_one) state)
  | GetWType (r, s) ->
      write_next r (I (Cerisier_codec.encode_word_type (word_type (get s state)))) state
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
  | LoadU (dst, source, offset) -> (
      match (get source state, value offset) with
      | Sealable (Cap (p, _, b, e, a)), I off ->
          let address = Z.add a off in
          if is_uninitialized p && b <= address && address < a && a <= e then
            match read_memory address state with
            | Some word -> write_next dst word state
            | None -> fail state
          else fail state
      | _ -> fail state)
  | StoreU (target, offset, operand) -> (
      match (get target state, value offset) with
      | Sealable (Cap (p, l, b, e, a)), I off -> (
          let address = Z.add a off and word = value operand in
          if (not (is_uninitialized p)) || address < b || address > a || a > e then fail state
          else
            match word with
            | Sealable (Cap (_, locality, _, _, _))
              when locality <> Global && not (can_store_local p) ->
                fail state
            | Sealable (SealRange (_, Directed, _, _, _)) when not (can_store_local p) -> fail state
            | Sealable (Cap (_, Directed, _, _, _)) when readable_limit word > address -> fail state
            | _ ->
                let state =
                  if Z.equal off Z.zero then
                    set_register target (Sealable (Cap (p, l, b, e, Z.succ a))) state
                  else state
                in
                pc_next (set_memory_raw address word state))
      | _ -> fail state)
  | PromoteU register -> (
      match get register state with
      | Sealable (Cap (p, l, b, e, a)) when is_uninitialized p ->
          write_next register (Sealable (Cap (promote p, l, b, Z.min e a, a))) state
      | _ -> fail state)
  | EInit (destination, source) -> (
      match get source state with
      | Sealable (Cap (p, _, b, e, a)) when can_read p && is_exec p -> (
          match read_memory b state with
          | Some (Sealable (Cap (data_permission, _, data_base, _, _)))
            when can_read data_permission && can_write data_permission ->
              if unique_address state b && unique_register state source then
                let object_type = Z.mul (Z.of_int 2) state.enclave_counter in
                let seal_keys =
                  Sealable
                    (SealRange
                       ( (true, true),
                         Global,
                         object_type,
                         Z.add object_type (Z.of_int 2),
                         object_type ))
                in
                let first_address = Z.max Z.zero (Z.succ b) in
                let last_address = Z.min e (Z.pred (Runtime_config.max_addr state.config)) in
                let rec region address words =
                  if address > last_address then List.rev words
                  else
                    let word = Option.value (read_memory address state) ~default:(I Z.zero) in
                    region (Z.succ address) (word :: words)
                in
                let code_region = region first_address [] in
                let identity = Z.of_int (Hashtbl.hash (b, code_region)) in
                let state =
                  {
                    state with
                    enclave_table = ETableMap.add state.enclave_counter identity state.enclave_table;
                    enclave_counter = Z.succ state.enclave_counter;
                  }
                  |> set_memory_raw data_base seal_keys
                in
                write_next destination (Sealable (Cap (E, Global, b, e, a))) state
              else fail state
          | _ -> fail state)
      | _ -> fail state)
  | EDeInit source -> (
      match get source state with
      | Sealable (SealRange ((true, true), Global, b, e, _))
        when Z.equal e (Z.add b (Z.of_int 2)) && Z.is_even b ->
          pc_next
            { state with enclave_table = ETableMap.remove Z.(b / of_int 2) state.enclave_table }
      | _ -> fail state)
  | EStoreId (destination, source) -> (
      match get source state with
      | I sealed_type -> (
          let table_id =
            if Z.is_even sealed_type then Z.div sealed_type (Z.of_int 2)
            else Z.div Z.(sealed_type - one) (Z.of_int 2)
          in
          match ETableMap.find_opt table_id state.enclave_table with
          | Some identity -> write_next destination (I identity) state
          | None -> fail state)
      | _ -> fail state)
  | IsUnique (destination, source) -> (
      match get source state with
      | Sealable (Cap _) | Sealed (_, Cap _) ->
          write_next destination (I (if unique_register state source then Z.one else Z.zero)) state
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
                match Cerisier_codec.decode encoded with
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
