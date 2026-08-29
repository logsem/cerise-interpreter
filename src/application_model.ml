open Cerise

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
let capabilities state = List.filter (fun (r : Machine_view.register) -> Option.is_some r.word.capability) (view state).registers
let selected_capability state =
  Option.bind state.selected (fun id -> List.find_opt (fun (r : Machine_view.register) -> Machine_view.Register_id.equal id r.id) (capabilities state))
let create current =
  let selected = (Machine_session.view current).registers |> List.find_map (fun (r : Machine_view.register) -> Option.map (fun _ -> r.id) r.word.capability) in
  { current; history = []; primary_start = Z.zero; secondary_start = Z.zero; selected }
let clamp view address = if Z.sign address <= 0 then Z.zero else if Z.compare address view.Machine_view.address_limit >= 0 then Z.pred view.address_limit else address
let with_current previous current = { previous with current; history = previous.current :: previous.history }
let undo state = match state.history with prior :: rest -> { state with current = prior; history = rest } | [] -> state
let step state = Result.map (with_current state) (Machine_session.step state.current)
let step_n count state = Result.map (with_current state) (Machine_session.step_n count state.current)
let set_register_text id text state = Result.map (with_current state) (Machine_session.set_register_text id text state.current)
let set_memory_text address text state = Result.map (with_current state) (Machine_session.set_memory_text address text state.current)
let move_primary delta state = { state with primary_start = clamp (view state) Z.(state.primary_start + delta) }
let move_secondary delta state = { state with secondary_start = clamp (view state) Z.(state.secondary_start + delta) }
let page_primary rows pages state = move_primary Z.(of_int rows * of_int pages) state
let page_secondary rows pages state = move_secondary Z.(of_int rows * of_int pages) state
let follow_primary state = match (view state).pc with None -> state | Some pc -> { state with primary_start = clamp (view state) pc }
let secondary_target state =
  match List.find_opt (fun (r : Machine_view.register) -> r.role = Machine_view.Stack_pointer) (view state).registers with
  | Some { word = { capability = Some c; _ }; _ } -> Some c.cursor
  | _ -> Option.bind (selected_capability state) (fun r -> Option.map (fun c -> c.Machine_view.cursor) r.word.capability)
let follow_secondary state = match secondary_target state with None -> state | Some address -> { state with secondary_start = clamp (view state) address }
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
      let selected = match state.selected with
        | None -> (List.hd registers).id
        | Some id -> (match List.find_index (fun (r : Machine_view.register) -> Machine_view.Register_id.equal id r.id) registers with
          | Some i -> (List.nth registers ((i + 1) mod List.length registers)).id | None -> (List.hd registers).id)
      in { state with selected = Some selected }
