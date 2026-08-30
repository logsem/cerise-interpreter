open Ast

module RegMap = Map.Make (struct
  type t = register

  let compare (a : t) (b : t) : int =
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

let init (config : Runtime_config.t) (program : word list) (regfile : (register * word) list option) : t =
  let max_addr = Runtime_config.max_addr config and stack_addr = Runtime_config.stack_addr config in
  let registers = List.init 32 (fun n -> (Reg n, I Z.zero)) |> List.to_seq |> RegMap.of_seq in
  let registers =
    registers
    |> RegMap.add PC (Sealable (Cap (RWX, Z.zero, max_addr, Z.zero)))
    |> RegMap.add (Reg 0) (Sealable (SealRange ((true, true), Z.zero, stack_addr, Z.zero)))
  in
  let registers =
    match regfile with
    | None -> registers
    | Some entries ->
        List.fold_left (fun registers (register, word) -> RegMap.add register word registers)
          registers entries
  in
  let memory =
    List.mapi (fun address word -> (Z.of_int address, word)) program
    |> List.to_seq |> MemMap.of_seq
  in
  { config; status = Running; registers; memory }

let read_register (r : register) (state : t) : word = RegMap.find r state.registers

let read_memory (a : Z.t) (state : t) : word option =
  match MemMap.find_opt a state.memory with
  | Some w -> Some w
  | None when Z.sign a >= 0 && Z.compare a (Runtime_config.max_addr state.config) < 0 ->
      Some (I Z.zero)
  | None -> None

let set_register (r : register) (word : word) (state : t) : t = { state with registers = RegMap.add r word state.registers }
let set_memory_raw (a : Z.t) (word : word) (state : t) : t = { state with memory = MemMap.add a word state.memory }

let pc_next (state : t) : t =
  match read_register PC state with
  | Sealable (Cap (p, b, e, a)) ->
      { state with registers = RegMap.add PC (Sealable (Cap (p, b, e, Z.succ a))) state.registers }
  | _ -> { state with status = Failed }

let fail (state : t) : t = { state with status = Failed }
let word_of_operand (state : t) (matched_value : reg_or_const) : word = match matched_value with Register r -> read_register r state | Constant z -> I z

let permission_flows (requested : permission) (current : permission) : bool =
  match requested with
  | O -> true
  | E -> ( match current with E | RX | RWX -> true | _ -> false)
  | RO -> ( match current with RO | RX | RW | RWX -> true | _ -> false)
  | RX -> ( match current with RX | RWX -> true | _ -> false)
  | RW -> ( match current with RW | RWX -> true | _ -> false)
  | RWX -> current = RWX

let seal_permission_flows ((s, u) : bool * bool) ((s', u') : bool * bool) : bool = ((not s) || s') && ((not u) || u')
let can_read (matched_value : permission) : bool = match matched_value with RO | RX | RW | RWX -> true | _ -> false
let can_write (matched_value : permission) : bool = match matched_value with RW | RWX -> true | _ -> false
let is_exec (matched_value : permission) : bool = match matched_value with RX | RWX -> true | _ -> false

let word_type (matched_value : word) : word_type = match matched_value with
  | I _ -> Integer
  | Sealable (Cap _) -> Capability
  | Sealable (SealRange _) -> Seal_range
  | Sealed _ -> Sealed

let bounds (matched_value : sealable) : Z.t * Z.t * Z.t = match matched_value with Cap (_, b, e, a) | SealRange (_, b, e, a) -> (b, e, a)

let with_cursor (s : sealable) (cursor : Z.t) : sealable =
  match s with
  | Cap (p, b, e, _) -> Cap (p, b, e, cursor)
  | SealRange (p, b, e, _) -> SealRange (p, b, e, cursor)

let with_bounds (s : sealable) (base : Z.t) (limit : Z.t) : sealable =
  match s with
  | Cap (p, _, _, a) -> Cap (p, base, limit, a)
  | SealRange (p, _, _, a) -> SealRange (p, base, limit, a)

let valid_pc (state : t) : bool =
  match read_register PC state with
  | Sealable (Cap ((RX | RWX), b, e, a)) -> b <= a && a < e && Option.is_some (read_memory a state)
  | _ -> false

let write_next (r : register) (w : word) (state : t) : t = pc_next (set_register r w state)

let rec execute (op : instruction) (state : t) : t =
  let get = read_register and value = word_of_operand state in
  match op with
  | Fail -> fail state
  | Halt -> { state with status = Halted }
  | Move (r, o) -> write_next r (value o) state
  | Load (dst, src) -> (
      match get src state with
      | Sealable (Cap (p, b, e, a)) when can_read p && b <= a && a < e -> (
          match read_memory a state with Some w -> write_next dst w state | None -> fail state)
      | _ -> fail state)
  | Store (dst, o) -> (
      match get dst state with
      | Sealable (Cap (p, b, e, a)) when can_write p && b <= a && a < e ->
          pc_next (set_memory_raw a (value o) state)
      | _ -> fail state)
  | Jmp r -> (
      match get r state with
      | Sealable (Cap (E, b, e, a)) -> set_register PC (Sealable (Cap (RX, b, e, a))) state
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
          | Cap (E, _, _, _) -> fail state
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
      | Sealable (Cap (p, b, e, a)), I z -> (
          match Codec.decode_permission z with
          | Ok p' when permission_flows p' p -> write_next r (Sealable (Cap (p', b, e, a))) state
          | _ -> fail state)
      | Sealable (SealRange (p, b, e, a)), I z -> (
          match Codec.decode_seal_permission z with
          | Ok p' when seal_permission_flows p' p ->
              write_next r (Sealable (SealRange (p', b, e, a))) state
          | _ -> fail state)
      | _ -> fail state)
  | SubSeg (r, o1, o2) -> (
      match (get r state, value o1, value o2) with
      | Sealable (Cap (E, _, _, _)), _, _ -> fail state
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
      | Sealable (Cap (p, _, _, _)) -> write_next r (I (Codec.encode_permission p)) state
      | Sealable (SealRange (p, _, _, _)) ->
          write_next r (I (Codec.encode_seal_permission p)) state
      | _ -> fail state)
  | GetOType (r, s) -> (
      match get s state with
      | Sealed (o, _) -> write_next r (I o) state
      | _ -> write_next r (I Z.minus_one) state)
  | GetWType (r, s) ->
      write_next r (I (Codec.encode_word_type (word_type (get s state)))) state
  | Seal (dst, seal, value_reg) -> (
      match (get seal state, get value_reg state) with
      | Sealable (SealRange ((true, _), b, e, a)), Sealable sb when b <= a && a < e ->
          write_next dst (Sealed (a, sb)) state
      | _ -> fail state)
  | UnSeal (dst, seal, value_reg) -> (
      match (get seal state, get value_reg state) with
      | Sealable (SealRange ((_, true), b, e, a)), Sealed (o, sb)
        when b <= a && a < e && Z.equal a o ->
          write_next dst (Sealable sb) state
      | _ -> fail state)
  | Invoke (code, data) -> (
      match (get code state, get data state) with
      | Sealed (o, Cap (p, b, e, a)), Sealed (o', sb) when Z.equal o o' && is_exec p -> (
          match sb with
          | Cap (p', _, _, _) when not (is_exec p') ->
              let state = set_register data (Sealable sb) state in
              set_register PC (Sealable (Cap ((if p = E then RX else p), b, e, a))) state
          | _ -> fail state)
      | _ -> fail state)

let step (state : t) : (t, Machine_backend.execution_error) result =
  match state.status with
  | Halted -> Error (Machine_backend.Stopped Machine_view.Halted)
  | Failed -> Error (Machine_backend.Stopped Machine_view.Failed)
  | Running -> (
      if not (valid_pc state) then Ok (fail state)
      else
        match read_register PC state with
        | Sealable (Cap (_, _, _, a)) -> (
            match read_memory a state with
            | Some (I encoded) -> (
                match Codec.decode encoded with
                | Ok op -> Ok (execute op state)
                | Error _ -> Ok (fail state))
            | _ -> Ok (fail state))
        | _ -> Ok (fail state))

let rec step_n (count : int) (state : t) : (t, Machine_backend.execution_error) result =
  if count < 0 then Error (Machine_backend.Backend_error "step count must be non-negative")
  else if count = 0 then Ok state
  else
    match step state with
    | Ok next -> step_n (count - 1) next
    | Error (Machine_backend.Stopped _) -> Ok state
    | Error _ as e -> e


let rec run (state : t) : t =
  match step state with
  | Ok next -> run next
  | Error (Machine_backend.Stopped _) -> state
  | Error _ -> state
