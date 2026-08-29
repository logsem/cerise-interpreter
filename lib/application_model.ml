type t = {
  current : Machine_session.t;
  history : Machine_session.t list;
  primary_start : Z.t;
  secondary_start : Z.t;
  selected : Machine_view.register_id option;
}

let view state = Machine_session.view state.current
let session state = state.current
let history_length state = List.length state.history
let primary_start state = state.primary_start
let secondary_start state = state.secondary_start

let capabilities state =
  List.filter
    (fun (register : Machine_view.register) -> Option.is_some register.word.capability)
    (view state).registers

let selected_capability state =
  match state.selected with
  | None -> None
  | Some id ->
      List.find_opt
        (fun (register : Machine_view.register) -> Machine_view.Register_id.equal id register.id)
        (capabilities state)

let create current =
  let selected =
    (Machine_session.view current).registers
    |> List.find_map (fun (register : Machine_view.register) ->
           Option.map (fun _ -> register.id) register.word.capability)
  in
  { current; history = []; primary_start = Z.zero; secondary_start = Z.zero; selected }

let clamp (view : Machine_view.t) address =
  if Z.sign address <= 0 then Z.zero
  else if Z.compare address view.address_limit >= 0 then Z.pred view.address_limit
  else address

let with_current previous current = { previous with current; history = previous.current :: previous.history }
let undo state = match state.history with prior :: rest -> { state with current = prior; history = rest } | [] -> state

let step state = Result.map (with_current state) (Machine_session.step state.current)
let step_n count state = Result.map (with_current state) (Machine_session.step_n count state.current)

let set_register_text id text state =
  Result.map (with_current state) (Machine_session.set_register_text id text state.current)

let set_memory_text address text state =
  Result.map (with_current state) (Machine_session.set_memory_text address text state.current)

let move_primary delta state = { state with primary_start = clamp (view state) Z.(state.primary_start + delta) }
let move_secondary delta state = { state with secondary_start = clamp (view state) Z.(state.secondary_start + delta) }
let page_primary rows pages state = move_primary Z.(of_int rows * of_int pages) state
let page_secondary rows pages state = move_secondary Z.(of_int rows * of_int pages) state

let follow_primary state =
  match (view state).pc with
  | None -> state
  | Some pc -> { state with primary_start = clamp (view state) pc }

let follow_secondary state =
  match selected_capability state with
  | Some { word = { capability = Some capability; _ }; _ } ->
      { state with secondary_start = clamp (view state) capability.cursor }
  | _ -> state

let row_budget ~height ~register_count =
  let content_rows = max 0 (height - 4) in
  let register_rows = min register_count (content_rows / 2) in
  let memory_rows = content_rows - register_rows in
  (register_rows, memory_rows / 2, memory_rows - (memory_rows / 2))

let capability_registers = capabilities

let select_next_capability state =
  match capabilities state with
  | [] -> { state with selected = None }
  | registers ->
      let selected =
        match state.selected with
        | None -> (List.hd registers).id
        | Some id -> (
            match
              List.find_index
                (fun (register : Machine_view.register) -> Machine_view.Register_id.equal id register.id)
                registers
            with
            | Some index -> (List.nth registers ((index + 1) mod List.length registers)).id
            | None -> (List.hd registers).id)
      in
      { state with selected = Some selected }
