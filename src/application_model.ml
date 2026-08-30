(* The application model adds reversible navigation state to an immutable machine
   session. Machine operations retain the previous session, while viewport
   operations only adjust the two memory windows. *)

open Cerise

type t = {
  current : Machine_session.t;
  history : Machine_session.t list;
  primary_start : Z.t;
  secondary_start : Z.t;
  selected : Machine_view.register_id option;
}

type update_result = (t, Machine_backend.execution_error) result
type edit_result = (t, Diagnostic.t list) result

(* Session state and capability selection *)

let view (state : t) : Machine_view.t = Machine_session.view state.current
let session (state : t) : Machine_session.t = state.current
let history_length (state : t) : int = List.length state.history
let primary_start (state : t) : Z.t = state.primary_start
let secondary_start (state : t) : Z.t = state.secondary_start

let capability_registers (state : t) : Machine_view.register list =
  List.filter
    (fun (register : Machine_view.register) -> Option.is_some register.word.capability)
    (view state).registers

let selected_capability (state : t) : Machine_view.register option =
  Option.bind state.selected (fun selected_id ->
      List.find_opt
        (fun (register : Machine_view.register) ->
          Machine_view.Register_id.equal selected_id register.id)
        (capability_registers state))

let clamp_address (machine_view : Machine_view.t) (address : Z.t) : Z.t =
  if Z.sign address <= 0 then Z.zero
  else if Z.compare address machine_view.Machine_view.address_limit >= 0 then
    Z.pred machine_view.address_limit
  else address

let has_finite_cursor (machine_view : Machine_view.t) (capability : Machine_view.capability) : bool
    =
  (* The viewport can only follow addresses represented in the configured finite
     address space, even when a backend can describe wider authority. *)
  Z.sign capability.cursor >= 0
  && Z.compare capability.cursor machine_view.Machine_view.address_limit < 0

let has_valid_authority (machine_view : Machine_view.t) (capability : Machine_view.capability) :
    bool =
  has_finite_cursor machine_view capability
  && Z.sign capability.base >= 0
  && Z.compare capability.base capability.cursor <= 0
  && Z.compare capability.cursor capability.limit < 0
  && Z.compare capability.limit machine_view.Machine_view.address_limit <= 0

let active_stack_pointer (state : t) : Machine_view.register option =
  let machine_view = view state in
  List.find_opt
    (fun (register : Machine_view.register) ->
      register.role = Machine_view.Stack_pointer
      && Option.fold ~none:false ~some:(has_valid_authority machine_view) register.word.capability)
    machine_view.registers

let secondary_target (state : t) : Z.t option =
  match active_stack_pointer state with
  | Some { word = { capability = Some capability; _ }; _ } -> Some capability.cursor
  | _ ->
      Option.bind (selected_capability state) (fun register ->
          Option.bind register.word.capability (fun capability ->
              if has_finite_cursor (view state) capability then Some capability.cursor else None))

(* Viewport navigation *)

let contextual_start (machine_view : Machine_view.t) ~(rows : int) ~(start : Z.t) (target : Z.t) :
    Z.t =
  if rows <= 0 then start
  else
    let bottom = Z.(start + of_int rows) in
    if Z.compare target start >= 0 && Z.compare target bottom < 0 then start
    else
      let context_rows = min 2 (rows - 1) in
      clamp_address machine_view Z.(target - of_int context_rows)

let follow_primary ?(rows : int = 3) (state : t) : t =
  match (view state).pc with
  | None -> state
  | Some program_counter ->
      {
        state with
        primary_start =
          contextual_start (view state) ~rows ~start:state.primary_start program_counter;
      }

let follow_secondary ?(rows : int = 3) (state : t) : t =
  match secondary_target state with
  | None -> state
  | Some address ->
      {
        state with
        secondary_start = contextual_start (view state) ~rows ~start:state.secondary_start address;
      }

let create (current : Machine_session.t) : t =
  let selected =
    (Machine_session.view current).registers
    |> List.find_map (fun (register : Machine_view.register) ->
        Option.map (fun _capability -> register.id) register.word.capability)
  in
  { current; history = []; primary_start = Z.zero; secondary_start = Z.zero; selected }
  |> follow_primary |> follow_secondary

let with_current (previous : t) (current : Machine_session.t) : t =
  { previous with current; history = previous.current :: previous.history }

let undo (state : t) : t =
  match state.history with
  | previous :: remaining_history -> { state with current = previous; history = remaining_history }
  | [] -> state

let step (state : t) : update_result =
  Result.map (with_current state) (Machine_session.step state.current)

let step_n (count : int) (state : t) : update_result =
  Result.map (with_current state) (Machine_session.step_n count state.current)

let set_register_text (register_id : Machine_view.register_id) (text : string) (state : t) :
    edit_result =
  Result.map (with_current state) (Machine_session.set_register_text register_id text state.current)

let set_memory_text (address : Z.t) (text : string) (state : t) : edit_result =
  Result.map (with_current state) (Machine_session.set_memory_text address text state.current)

let move_primary (delta : Z.t) (state : t) : t =
  { state with primary_start = clamp_address (view state) Z.(state.primary_start + delta) }

let move_secondary (delta : Z.t) (state : t) : t =
  { state with secondary_start = clamp_address (view state) Z.(state.secondary_start + delta) }

let page_delta (rows : int) : int = max 1 (rows - 2)

let page_primary (rows : int) (pages : int) (state : t) : t =
  move_primary Z.(of_int (page_delta rows) * of_int pages) state

let page_secondary (rows : int) (pages : int) (state : t) : t =
  move_secondary Z.(of_int (page_delta rows) * of_int pages) state

let row_budget ~(height : int) ~(register_count : int) : int * int * int =
  let content_rows = max 0 (height - 4) in
  let register_rows = min register_count (content_rows / 2) in
  let memory_rows = content_rows - register_rows in
  (register_rows, memory_rows / 2, memory_rows - (memory_rows / 2))

let select_next_capability (state : t) : t =
  match capability_registers state with
  | [] -> { state with selected = None }
  | registers ->
      let selected =
        match state.selected with
        | None -> (List.hd registers).id
        | Some selected_id -> (
            match
              List.find_index
                (fun (register : Machine_view.register) ->
                  Machine_view.Register_id.equal selected_id register.id)
                registers
            with
            | Some index -> (List.nth registers ((index + 1) mod List.length registers)).id
            | None -> (List.hd registers).id)
      in
      { state with selected = Some selected }
