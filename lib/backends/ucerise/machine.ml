(** Pure uCerise machine transitions. Assembly initializes concrete register and memory maps; each
    step fetches, decodes, and executes one instruction. *)

open Ast

module RegMap = Map.Make (struct
  type t = register

  let compare (left : register) (right : register) : int = compare left right
end)

module MemMap = Map.Make (Z)

type status = Running | Halted | Failed
type t = { status : status; registers : word RegMap.t; memory : word MemMap.t }

let init (config : Runtime_config.t) (program : word list) (regfile : (register * word) list option)
    : t =
  let stack = Runtime_config.stack_addr config and limit = Runtime_config.max_addr config in
  let registers = List.init 32 (fun n -> (Reg n, I Z.zero)) |> List.to_seq |> RegMap.of_seq in
  let registers =
    registers
    |> RegMap.add PC (Cap (Cap (RWX, Global, Z.zero, stack, Z.zero)))
    |> RegMap.add (Reg 31) (Cap (Cap (URWLX, Local, stack, limit, stack)))
  in
  let registers =
    match regfile with
    | None -> registers
    | Some entries ->
        List.fold_left (fun registers (r, w) -> RegMap.add r w registers) registers entries
  in
  let memory =
    List.mapi (fun address word -> (Z.of_int address, word)) program |> List.to_seq |> MemMap.of_seq
  in
  { status = Running; registers; memory }

let read_register (r : register) (state : t) : word = RegMap.find r state.registers

let read_memory (config : Runtime_config.t) (a : Z.t) (state : t) : word option =
  match MemMap.find_opt a state.memory with
  | Some w -> Some w
  (* Uninitialized cells inside the configured finite address space read as zero;
     addresses outside it remain invalid instead of extending memory implicitly. *)
  | None when Z.sign a >= 0 && Z.compare a (Runtime_config.max_addr config) < 0 -> Some (I Z.zero)
  | None -> None

let ( @! ) (register : register) (state : t) : word = read_register register state

let ( @? ) (address : Z.t) ((config, state) : Runtime_config.t * t) : word option =
  read_memory config address state

let set_register (r : register) (w : word) (state : t) : t =
  { state with registers = RegMap.add r w state.registers }

let set_memory_raw (a : Z.t) (w : word) (state : t) : t =
  { state with memory = MemMap.add a w state.memory }

let mark_failed (state : t) : t = { state with status = Failed }

let advance_program_counter (state : t) : t =
  match PC @! state with
  | Cap (Cap (p, l, b, e, a)) -> set_register PC (Cap (Cap (p, l, b, e, Z.succ a))) state
  | _ -> mark_failed state

let ( !> ) (state : t) : t = advance_program_counter state

let evaluate_operand (state : t) (term : reg_or_const) : word =
  match term with Register r -> r @! state | Constant z -> I z

let is_uninitialized_permission (term : permission) : bool =
  match term with URW | URWX | URWL | URWLX -> true | _ -> false

let has_write_local_permission (term : permission) : bool =
  match term with RWL | RWLX | URWL | URWLX -> true | _ -> false

let can_read_memory (term : permission) : bool =
  match term with RO | RX | RW | RWX | RWL | RWLX -> true | _ -> false

let can_write_memory (term : permission) : bool =
  match term with RW | RWX | RWL | RWLX -> true | _ -> false

let is_executable_permission (term : permission) : bool =
  match term with RX | RWX | RWLX | URWX | URWLX -> true | _ -> false

let promote_permission (term : permission) : permission =
  match term with URW -> RW | URWX -> RWX | URWL -> RWL | URWLX -> RWLX | p -> p

let permission_flows (requested : permission) (current : permission) : bool =
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

let locality_flows (requested : locality) (current : locality) : bool =
  match (requested, current) with
  | Local, Local | Local, Global | Global, Global -> true
  | _ -> false

let has_valid_program_counter (config : Runtime_config.t) (state : t) : bool =
  match PC @! state with
  | Cap (Cap (p, _, b, e, a)) when is_executable_permission p ->
      b <= a && a < e && Option.is_some (a @? (config, state))
  | _ -> false

(** Instruction execution preserves persistence by returning a fresh state. *)
let rec execute (config : Runtime_config.t) (instruction : instruction) (state : t) : t =
  let read (r : register) : word = r @! state
  and operand_value (operand : reg_or_const) : word = evaluate_operand state operand in
  match instruction with
  | Fail -> mark_failed state
  | Halt -> { state with status = Halted }
  | Move (r, o) -> !>(set_register r (operand_value o) state)
  | Load (d, r) -> (
      match read r with
      | Cap (Cap (p, _, b, e, a)) when can_read_memory p && b <= a && a < e -> (
          match a @? (config, state) with
          | Some w -> !>(set_register d w state)
          | None -> mark_failed state)
      | _ -> mark_failed state)
  | Store (r, o) -> (
      match read r with
      | Cap (Cap (p, _, b, e, a)) when can_write_memory p && b <= a && a < e -> (
          let w = operand_value o in
          match w with
          (* A local capability requires write-local authority so it cannot escape
             through ordinary writable memory. *)
          | Cap (Cap (_, Local, _, _, _)) when not (has_write_local_permission p) ->
              mark_failed state
          | _ -> !>(set_memory_raw a w state))
      | _ -> mark_failed state)
  | Jmp r -> (
      match read r with
      | Cap (Cap (E, l, b, e, a)) -> set_register PC (Cap (Cap (RX, l, b, e, a))) state
      | w -> set_register PC w state)
  | Jnz (r, t) -> (
      match read t with I z when Z.equal z Z.zero -> !>state | _ -> execute config (Jmp r) state)
  | Add (r, a, b) | Sub (r, a, b) | Lt (r, a, b) -> (
      match (operand_value a, operand_value b) with
      | I x, I y ->
          let z =
            match instruction with
            | Add _ -> Z.add x y
            | Sub _ -> Z.sub x y
            | _ -> if Z.lt x y then Z.one else Z.zero
          in
          !>(set_register r (I z) state)
      | _ -> mark_failed state)
  | Lea (r, o) -> (
      match (read r, operand_value o) with
      | Cap (Cap (E, _, _, _, _)), _ -> mark_failed state
      | Cap (Cap (p, l, b, e, a)), I z ->
          !>(set_register r (Cap (Cap (p, l, b, e, Z.add a z))) state)
      | _ -> mark_failed state)
  | Restrict (r, o) -> (
      match (read r, operand_value o) with
      | Cap (Cap (p, l, b, e, a)), I z -> (
          match Codec.decode_permission_locality z with
          | Ok (p', l') when permission_flows p' p && locality_flows l' l ->
              !>(set_register r (Cap (Cap (p', l', b, e, a))) state)
          | _ -> mark_failed state)
      | _ -> mark_failed state)
  | SubSeg (r, o1, o2) -> (
      match (read r, operand_value o1, operand_value o2) with
      | Cap (Cap (E, _, _, _, _)), _, _ -> mark_failed state
      | Cap (Cap (p, l, b, e, a)), I b', I e' when b <= b' && Z.sign e' >= 0 && Z.sign e >= 0 ->
          !>(set_register r (Cap (Cap (p, l, b', e', a))) state)
      | _ -> mark_failed state)
  | IsPtr (r, x) ->
      !>(set_register r (I (match read x with Cap _ -> Z.one | I _ -> Z.zero)) state)
  | GetP (r, x) -> (
      match read x with
      | Cap (Cap (p, _, _, _, _)) -> !>(set_register r (I (Codec.encode_permission p)) state)
      | _ -> mark_failed state)
  | GetL (r, x) -> (
      match read x with
      | Cap (Cap (_, l, _, _, _)) -> !>(set_register r (I (Codec.encode_locality l)) state)
      | _ -> mark_failed state)
  | GetB (r, x) -> (
      match read x with
      | Cap (Cap (_, _, b, _, _)) -> !>(set_register r (I b) state)
      | _ -> mark_failed state)
  | GetE (r, x) -> (
      match read x with
      | Cap (Cap (_, _, _, e, _)) -> !>(set_register r (I e) state)
      | _ -> mark_failed state)
  | GetA (r, x) -> (
      match read x with
      | Cap (Cap (_, _, _, _, a)) -> !>(set_register r (I a) state)
      | _ -> mark_failed state)
  | LoadU (d, r, o) -> (
      match (read r, operand_value o) with
      | Cap (Cap (p, _, b, e, a)), I off
        when is_uninitialized_permission p && b <= Z.add a off && Z.add a off < a && a <= e -> (
          match Z.add a off @? (config, state) with
          | Some w -> !>(set_register d w state)
          | None -> mark_failed state)
      | _ -> mark_failed state)
  | StoreU (r, o, w) -> (
      match (read r, operand_value o) with
      | Cap (Cap (p, l, b, e, a)), I off
        when is_uninitialized_permission p && b <= Z.add a off && Z.add a off <= a && a <= e -> (
          let w = operand_value w in
          match w with
          | Cap (Cap (_, Local, _, _, _)) when not (has_write_local_permission p) ->
              mark_failed state
          | _ ->
              let state =
                if Z.equal off Z.zero then set_register r (Cap (Cap (p, l, b, e, Z.succ a))) state
                else state
              in
              !>(set_memory_raw (Z.add a off) w state))
      | _ -> mark_failed state)
  | PromoteU r -> (
      match read r with
      | Cap (Cap (p, l, b, e, a)) when is_uninitialized_permission p ->
          !>(set_register r (Cap (Cap (promote_permission p, l, b, Z.min e a, a))) state)
      | _ -> mark_failed state)

let step (config : Runtime_config.t) (state : t) : (t, Machine_backend.execution_error) result =
  if state.status <> Running then
    Error
      (Machine_backend.Stopped
         (if state.status = Halted then Machine_view.Halted else Machine_view.Failed))
  else if not (has_valid_program_counter config state) then Ok (mark_failed state)
  else
    match PC @! state with
    | Cap (Cap (_, _, _, _, a)) -> (
        match a @? (config, state) with
        | Some (I z) -> (
            match Codec.decode z with
            | Ok instruction -> Ok (execute config instruction state)
            | Error _ -> Ok (mark_failed state))
        | _ -> Ok (mark_failed state))
    | _ -> Ok (mark_failed state)

let rec step_n (config : Runtime_config.t) (n : int) (state : t) :
    (t, Machine_backend.execution_error) result =
  if n < 0 then Error (Machine_backend.Backend_error "step count must be non-negative")
  else if n = 0 then Ok state
  else
    match step config state with
    | Ok next -> step_n config (n - 1) next
    | Error (Machine_backend.Stopped _) -> Ok state
    | Error _ as e -> e
