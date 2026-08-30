(** The executable Cerisier machine owns register and memory state and applies one
    capability-machine transition at a time. *)

open Ast

module RegMap = Map.Make (struct
  type t = register

  let compare (a : t) (b : t) : int =
    match (a, b) with PC, PC -> 0 | PC, _ -> -1 | _, PC -> 1 | Reg a, Reg b -> Int.compare a b
end)

module MemMap = Map.Make (Z)
module ETableMap = Map.Make (Z)

type status = Running | Halted | Failed

type t = {
  status : status;
  registers : word RegMap.t;
  memory : word MemMap.t;
  enclave_table : Z.t ETableMap.t;
  enclave_counter : Z.t;
}

let init (config : Runtime_config.t) (program : word list) (regfile : (register * word) list option)
    : t =
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
        List.fold_left
          (fun registers (register, word) -> RegMap.add register word registers)
          registers entries
  in
  let memory =
    List.mapi (fun address word -> (Z.of_int address, word)) program |> List.to_seq |> MemMap.of_seq
  in
  { status = Running; registers; memory; enclave_table = ETableMap.empty; enclave_counter = Z.zero }

let read_register (r : register) (state : t) : word = RegMap.find r state.registers

let read_memory (config : Runtime_config.t) (a : Z.t) (state : t) : word option =
  match MemMap.find_opt a state.memory with
  | Some w -> Some w
  (* Sparse memory represents every finite in-range, unwritten address as zero;
     out-of-range addresses remain absent so capability checks can fail. *)
  | None when Z.sign a >= 0 && Z.compare a (Runtime_config.max_addr config) < 0 -> Some (I Z.zero)
  | None -> None

let ( @! ) (register : register) (state : t) : word = read_register register state

let ( @? ) (address : Z.t) ((config, state) : Runtime_config.t * t) : word option =
  read_memory config address state

let set_register (r : register) (word : word) (state : t) : t =
  { state with registers = RegMap.add r word state.registers }

let set_memory_raw (a : Z.t) (word : word) (state : t) : t =
  { state with memory = MemMap.add a word state.memory }

let pc_next (state : t) : t =
  match PC @! state with
  | Sealable (Cap (p, b, e, a)) ->
      { state with registers = RegMap.add PC (Sealable (Cap (p, b, e, Z.succ a))) state.registers }
  | _ -> { state with status = Failed }

let ( !> ) (state : t) : t = pc_next state
let fail (state : t) : t = { state with status = Failed }

let word_of_operand (state : t) (operand_or_word : reg_or_const) : word =
  match operand_or_word with Register r -> r @! state | Constant z -> I z

let permission_flows (requested : permission) (current : permission) : bool =
  match requested with
  | O -> true
  | E -> ( match current with E | RX | RWX -> true | _ -> false)
  | RO -> ( match current with RO | RX | RW | RWX -> true | _ -> false)
  | RX -> ( match current with RX | RWX -> true | _ -> false)
  | RW -> ( match current with RW | RWX -> true | _ -> false)
  | RWX -> current = RWX

let seal_permission_flows ((s, u) : bool * bool) ((s', u') : bool * bool) : bool =
  ((not s) || s') && ((not u) || u')

let can_read (operand_or_word : permission) : bool =
  match operand_or_word with RO | RX | RW | RWX -> true | _ -> false

let can_write (operand_or_word : permission) : bool =
  match operand_or_word with RW | RWX -> true | _ -> false

let is_exec (operand_or_word : permission) : bool =
  match operand_or_word with RX | RWX -> true | _ -> false

let word_type (operand_or_word : word) : word_type =
  match operand_or_word with
  | I _ -> Integer
  | Sealable (Cap _) -> Capability
  | Sealable (SealRange _) -> Seal_range
  | Sealed _ -> Sealed

let bounds (operand_or_word : sealable) : Z.t * Z.t * Z.t =
  match operand_or_word with Cap (_, b, e, a) | SealRange (_, b, e, a) -> (b, e, a)

let with_cursor (s : sealable) (cursor : Z.t) : sealable =
  match s with
  | Cap (p, b, e, _) -> Cap (p, b, e, cursor)
  | SealRange (p, b, e, _) -> SealRange (p, b, e, cursor)

let with_bounds (s : sealable) (base : Z.t) (limit : Z.t) : sealable =
  match s with
  | Cap (p, _, _, a) -> Cap (p, base, limit, a)
  | SealRange (p, _, _, a) -> SealRange (p, base, limit, a)

let capability_bounds (operand_or_word : word) : (Z.t * Z.t) option =
  match operand_or_word with
  | Sealable (Cap (_, b, e, _)) | Sealed (_, Cap (_, b, e, _)) -> Some (b, e)
  | _ -> None

let overlaps (left : word) (right : word) : bool =
  match (capability_bounds left, capability_bounds right) with
  | Some (b1, e1), Some (b2, e2) -> Z.lt b1 e2 && Z.lt b2 e1
  | _ -> false

let unique_register (state : t) (source : register) : bool =
  let candidate = source @! state in
  RegMap.for_all
    (fun register other -> register = source || not (overlaps candidate other))
    state.registers
  && MemMap.for_all (fun _ other -> not (overlaps candidate other)) state.memory

let usable_region (config : Runtime_config.t) (base : Z.t) (limit : Z.t) : bool =
  Z.sign base >= 0 && Z.lt base limit && Z.leq limit (Runtime_config.max_addr config)

let rec integer_region (config : Runtime_config.t) (state : t) (address : Z.t) (limit : Z.t)
    (acc : word list) : word list option =
  if Z.geq address limit then Some (List.rev acc)
  else
    match address @? (config, state) with
    | Some (I _ as word) -> integer_region config state (Z.succ address) limit (word :: acc)
    | _ -> None

let valid_pc (config : Runtime_config.t) (state : t) : bool =
  match PC @! state with
  | Sealable (Cap ((RX | RWX), b, e, a)) -> b <= a && a < e && Option.is_some (a @? (config, state))
  | _ -> false

let rec execute (config : Runtime_config.t) (instruction : instruction) (state : t) : t =
  let resolve_operand = word_of_operand state in
  match instruction with
  | Fail -> fail state
  | Halt -> { state with status = Halted }
  | Move (r, o) -> !>(set_register r (resolve_operand o) state)
  | Load (dst, src) -> (
      match src @! state with
      | Sealable (Cap (p, b, e, a)) when can_read p && b <= a && a < e -> (
          match a @? (config, state) with
          | Some w -> !>(set_register dst w state)
          | None -> fail state)
      | _ -> fail state)
  | Store (dst, o) -> (
      match dst @! state with
      | Sealable (Cap (p, b, e, a)) when can_write p && b <= a && a < e ->
          !>(set_memory_raw a (resolve_operand o) state)
      | _ -> fail state)
  | Jmp r -> (
      match r @! state with
      | Sealable (Cap (E, b, e, a)) -> set_register PC (Sealable (Cap (RX, b, e, a))) state
      | w -> set_register PC w state)
  | Jnz (r, test) -> (
      match test @! state with
      | I z when Z.equal z Z.zero -> !>state
      | _ -> execute config (Jmp r) state)
  | Add (r, a, b) | Sub (r, a, b) | Mul (r, a, b) | Rem (r, a, b) | Div (r, a, b) | Lt (r, a, b)
    -> (
      match (resolve_operand a, resolve_operand b) with
      | I x, I y -> (
          let result =
            match instruction with
            | Add _ -> Some Z.(x + y)
            | Sub _ -> Some Z.(x - y)
            | Mul _ -> Some Z.(x * y)
            | Rem _ when not (Z.equal y Z.zero) -> Some Z.(rem x y)
            | Div _ when not (Z.equal y Z.zero) -> Some Z.(div x y)
            | Lt _ -> Some (if Z.lt x y then Z.one else Z.zero)
            | _ -> None
          in
          match result with Some z -> !>(set_register r (I z) state) | None -> fail state)
      | _ -> fail state)
  | Lea (r, o) -> (
      match (r @! state, resolve_operand o) with
      | Sealable s, I z -> (
          match s with
          | Cap (E, _, _, _) -> fail state
          | _ ->
              !>(set_register r
                   (Sealable
                      (with_cursor s
                         Z.(
                           let _, _, a = bounds s in
                           a + z)))
                   state))
      | _ -> fail state)
  | Restrict (r, o) -> (
      match (r @! state, resolve_operand o) with
      | Sealable (Cap (p, b, e, a)), I z -> (
          match Codec.decode_permission z with
          | Ok p' when permission_flows p' p ->
              !>(set_register r (Sealable (Cap (p', b, e, a))) state)
          | _ -> fail state)
      | Sealable (SealRange (p, b, e, a)), I z -> (
          match Codec.decode_seal_permission z with
          | Ok p' when seal_permission_flows p' p ->
              !>(set_register r (Sealable (SealRange (p', b, e, a))) state)
          | _ -> fail state)
      | _ -> fail state)
  | SubSeg (r, o1, o2) -> (
      match (r @! state, resolve_operand o1, resolve_operand o2) with
      | Sealable (Cap (E, _, _, _)), _, _ -> fail state
      | Sealable s, I b', I e' ->
          let b, e, _ = bounds s in
          if b <= b' && Z.sign e' >= 0 && Z.sign e >= 0 then
            !>(set_register r (Sealable (with_bounds s b' e')) state)
          else fail state
      | _ -> fail state)
  | GetB (r, s) -> (
      match s @! state with
      | Sealable sb ->
          let b, _, _ = bounds sb in
          !>(set_register r (I b) state)
      | _ -> fail state)
  | GetE (r, s) -> (
      match s @! state with
      | Sealable sb ->
          let _, e, _ = bounds sb in
          !>(set_register r (I e) state)
      | _ -> fail state)
  | GetA (r, s) -> (
      match s @! state with
      | Sealable sb ->
          let _, _, a = bounds sb in
          !>(set_register r (I a) state)
      | _ -> fail state)
  | GetP (r, s) -> (
      match s @! state with
      | Sealable (Cap (p, _, _, _)) -> !>(set_register r (I (Codec.encode_permission p)) state)
      | Sealable (SealRange (p, _, _, _)) ->
          !>(set_register r (I (Codec.encode_seal_permission p)) state)
      | _ -> fail state)
  | GetOType (r, s) -> (
      match s @! state with
      | Sealed (o, _) -> !>(set_register r (I o) state)
      | _ -> !>(set_register r (I Z.minus_one) state))
  | GetWType (r, s) ->
      !>(set_register r (I (Codec.encode_word_type (word_type (s @! state)))) state)
  | Seal (dst, seal, value_reg) -> (
      match (seal @! state, value_reg @! state) with
      | Sealable (SealRange ((true, _), b, e, a)), Sealable sb when b <= a && a < e ->
          !>(set_register dst (Sealed (a, sb)) state)
      | _ -> fail state)
  | UnSeal (dst, seal, value_reg) -> (
      match (seal @! state, value_reg @! state) with
      | Sealable (SealRange ((_, true), b, e, a)), Sealed (o, sb)
        when b <= a && a < e && Z.equal a o ->
          !>(set_register dst (Sealable sb) state)
      | _ -> fail state)
  | Invoke (code, data) -> (
      match (code @! state, data @! state) with
      | Sealed (o, Cap (p, b, e, a)), Sealed (o', sb) when Z.equal o o' && is_exec p -> (
          match sb with
          | Cap (p', _, _, _) when not (is_exec p') ->
              let state = set_register data (Sealable sb) state in
              set_register PC (Sealable (Cap ((if p = E then RX else p), b, e, a))) state
          | _ -> fail state)
      | _ -> fail state)
  | Hash (destination, source) ->
      !>(set_register destination (I (Hash_algebra.word (source @! state))) state)
  | HashConcat (destination, left, right) -> (
      match (resolve_operand left, resolve_operand right) with
      | I left, I right -> (
          match Hash_algebra.concat left right with
          | Some hash -> !>(set_register destination (I hash) state)
          | None -> fail state)
      | _ -> fail state)
  | IsUnique (destination, source) -> (
      match source @! state with
      | Sealable (Cap _) | Sealed (_, Cap _) ->
          !>(set_register destination
               (I (if unique_register state source then Z.one else Z.zero))
               state)
      | _ -> fail state)
  | EInit (code, data) -> (
      match (code @! state, data @! state) with
      | ( Sealable (Cap (RX, code_base, code_end, _)),
          (Sealable (Cap (RW, data_base, data_end, _)) as data_capability) )
        when code <> PC && data <> PC && code <> data
             && usable_region config code_base code_end
             && usable_region config data_base data_end
             && (not (Z.lt code_base data_end && Z.lt data_base code_end))
             && unique_register state code && unique_register state data -> (
          match integer_region config state (Z.succ code_base) code_end [] with
          | None -> fail state
          | Some code_words ->
              let counter = state.enclave_counter in
              let object_type = Z.mul (Z.of_int 2) counter in
              let identity =
                match
                  Hash_algebra.concat (Hash_algebra.address code_base)
                    (Hash_algebra.words code_words)
                with
                | Some identity -> identity
                | None -> assert false
              in
              let seal_keys =
                Sealable
                  (SealRange ((true, true), object_type, Z.add object_type (Z.of_int 2), object_type))
              in
              state
              |> set_memory_raw code_base data_capability
              |> set_memory_raw data_base seal_keys
              |> set_register code (Sealable (Cap (E, code_base, code_end, Z.succ code_base)))
              |> set_register data (I Z.zero)
              |> fun state ->
              !>{
                  state with
                  enclave_table = ETableMap.add counter identity state.enclave_table;
                  enclave_counter = Z.succ counter;
                })
      | _ -> fail state)
  | EDeInit source -> (
      match source @! state with
      | Sealable (SealRange ((true, true), object_type, limit, _))
        when Z.equal limit (Z.add object_type (Z.of_int 2)) ->
          let table_id = Z.fdiv object_type (Z.of_int 2) in
          if ETableMap.mem table_id state.enclave_table then
            !>{ state with enclave_table = ETableMap.remove table_id state.enclave_table }
          else fail state
      | _ -> fail state)
  | EStoreId (destination, source) -> (
      match source @! state with
      | I object_type -> (
          match ETableMap.find_opt (Z.fdiv object_type (Z.of_int 2)) state.enclave_table with
          | Some identity -> !>(set_register destination (I identity) state)
          | None -> fail state)
      | _ -> fail state)

let step (config : Runtime_config.t) (state : t) : (t, Machine_backend.execution_error) result =
  match state.status with
  | Halted -> Error (Machine_backend.Stopped Machine_view.Halted)
  | Failed -> Error (Machine_backend.Stopped Machine_view.Failed)
  | Running -> (
      if not (valid_pc config state) then Ok (fail state)
      else
        match PC @! state with
        | Sealable (Cap (_, _, _, a)) -> (
            match a @? (config, state) with
            | Some (I encoded) -> (
                match Codec.decode encoded with
                | Ok instruction -> Ok (execute config instruction state)
                | Error _ -> Ok (fail state))
            | _ -> Ok (fail state))
        | _ -> Ok (fail state))

let rec step_n (config : Runtime_config.t) (count : int) (state : t) :
    (t, Machine_backend.execution_error) result =
  if count < 0 then Error (Machine_backend.Backend_error "step count must be non-negative")
  else if count = 0 then Ok state
  else
    match step config state with
    | Ok next -> step_n config (count - 1) next
    | Error (Machine_backend.Stopped _) -> Ok state
    | Error _ as e -> e

let rec run (config : Runtime_config.t) (state : t) : t =
  match step config state with
  | Ok next -> run config next
  | Error (Machine_backend.Stopped _) -> state
  | Error _ -> state
