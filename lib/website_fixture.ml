type t = {
  session : Machine_session.t;
  history : Machine_session.t list;
  selected : Machine_view.register_id option;
  memory_start : Z.t;
}

let create ~backend ~config ~source ~regfile =
  Result.map
    (fun session ->
      let selected =
        (Machine_session.view session).registers
        |> List.find_map (fun (register : Machine_view.register) ->
               Option.map (fun _ -> register.id) register.word.capability)
      in
      { session; history = []; selected; memory_start = Z.zero })
    (Machine_session.create ~backend ~config ~source ~regfile)

let session fixture = fixture.session
let view fixture = Machine_session.view fixture.session
let replace fixture session = { fixture with session; history = fixture.session :: fixture.history }
let undo fixture = match fixture.history with prior :: rest -> { fixture with session = prior; history = rest } | [] -> fixture
let step fixture = Result.map (replace fixture) (Machine_session.step fixture.session)
let edit_register id text fixture = Result.map (replace fixture) (Machine_session.set_register_text id text fixture.session)
let edit_memory address text fixture = Result.map (replace fixture) (Machine_session.set_memory_text address text fixture.session)

let capability_registers fixture =
  List.filter
    (fun (register : Machine_view.register) -> Option.is_some register.word.capability)
    (view fixture).registers

let selected_capability fixture =
  match fixture.selected with
  | None -> None
  | Some id ->
      List.find_opt (fun (register : Machine_view.register) -> Machine_view.Register_id.equal id register.id)
        (capability_registers fixture)

let select_next_capability fixture =
  match capability_registers fixture with
  | [] -> { fixture with selected = None }
  | registers ->
      let selected =
        match fixture.selected with
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
      { fixture with selected = Some selected }

let memory_start fixture = fixture.memory_start
let clamp fixture address =
  let limit = ((view fixture) : Machine_view.t).address_limit in
  if Z.sign address <= 0 then Z.zero else if Z.compare address limit >= 0 then Z.pred limit else address
let navigate_memory delta fixture = { fixture with memory_start = clamp fixture Z.(fixture.memory_start + delta) }

let follow_selected_capability fixture =
  match selected_capability fixture with
  | Some { word = { capability = Some capability; _ }; _ } ->
      { fixture with memory_start = clamp fixture capability.cursor }
  | _ -> fixture
