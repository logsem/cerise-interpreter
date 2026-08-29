type t =
  | Session :
      (module Machine_backend.S with type state = 'state and type word = 'word) * string * 'state
      -> t

type execution_error = Machine_backend.execution_error

type stop_reason =
  | Halted
  | Failed
  | Breakpoint of Z.t
  | Step_limit
  | Execution_error of execution_error

type run_result = { session : t; reason : stop_reason; steps : int }

let unknown_backend name =
  let available = String.concat ", " (Backend_registry.names ()) in
  Diagnostic.error (Printf.sprintf "Unknown backend %S. Available backends: %s." name available)

let create_with_backend ?source_filename ?regfile_filename requested_name config source regfile
    (module Backend : Machine_backend.S) =
  match Backend.parse_program ?filename:source_filename source with
  | Error _ as error -> error
  | Ok program -> (
      let regfile_result =
        match regfile with
        | None -> Ok None
        | Some source -> Result.map Option.some (Backend.parse_regfile ?filename:regfile_filename source)
      in
      match regfile_result with
      | Error _ as error -> error
      | Ok regfile -> (
          match Backend.init config program regfile with
          | Error _ as error -> error
          | Ok state ->
              Ok
                (Session
                   ( (module Backend : Machine_backend.S
                       with type state = Backend.state
                        and type word = Backend.word),
                     requested_name,
                     state ))))

let create_with_filename_options ~source_filename ~regfile_filename ~backend ~config ~source ~regfile =
  match Backend_registry.find backend with
  | None -> Error [ unknown_backend backend ]
  | Some selected ->
      create_with_backend ?source_filename ?regfile_filename backend config source regfile selected

let create ~backend ~config ~source ~regfile =
  create_with_filename_options ~source_filename:None ~regfile_filename:None ~backend ~config ~source ~regfile

let create_with_filenames ~source_filename ~regfile_filename ~backend ~config ~source ~regfile =
  create_with_filename_options ~source_filename:(Some source_filename) ~regfile_filename ~backend ~config
    ~source ~regfile

let backend_name (Session (_, requested_name, _)) = requested_name

let view (Session ((module Backend), requested_name, state)) =
  { (Backend.inspect state) with backend_name = requested_name }

let step (Session ((module Backend), requested_name, state)) =
  Result.map (fun state -> Session ((module Backend), requested_name, state)) (Backend.step state)

let step_n count (Session ((module Backend), requested_name, state)) =
  Result.map
    (fun state -> Session ((module Backend), requested_name, state))
    (Backend.step_n count state)

let has_breakpoint breakpoints pc =
  match pc with
  | None -> None
  | Some pc when List.exists (Z.equal pc) breakpoints -> Some pc
  | Some _ -> None

let run ?(breakpoints = []) ?max_steps session =
  let rec loop steps session =
    let current_view = view session in
    match current_view.status with
    | Machine_view.Halted -> { session; reason = Halted; steps }
    | Failed -> { session; reason = Failed; steps }
    | Running -> (
        match has_breakpoint breakpoints current_view.pc with
        | Some address -> { session; reason = Breakpoint address; steps }
        | None -> (
            match max_steps with
            | Some limit when steps >= limit -> { session; reason = Step_limit; steps }
            | _ -> (
                match step session with
                | Ok next -> loop (steps + 1) next
                | Error error -> { session; reason = Execution_error error; steps })))
  in
  match max_steps with
  | Some limit when limit < 0 ->
      {
        session;
        reason = Execution_error (Backend_error "step limit must be non-negative");
        steps = 0;
      }
  | _ -> loop 0 session

let set_register_text id source (Session ((module Backend), requested_name, state)) =
  match Backend.parse_word source with
  | Error _ as error -> error
  | Ok word ->
      Result.map
        (fun state -> Session ((module Backend), requested_name, state))
        (Backend.set_register id word state)

let set_memory_text address source (Session ((module Backend), requested_name, state)) =
  match Backend.parse_word source with
  | Error _ as error -> error
  | Ok word ->
      Result.map
        (fun state -> Session ((module Backend), requested_name, state))
        (Backend.set_memory address word state)
