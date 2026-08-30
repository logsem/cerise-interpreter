(** Existential sessions keep configuration, backend-specific state, and words hidden from shared
    clients.

    Creation delegates parsing and initialization to the selected backend. Later operations unpack
    that same backend module, reuse the session-owned execution configuration, transform its state,
    and repack it with the requested CLI alias. *)

type t =
  | Session :
      (module Machine_backend.S with type state = 'state and type asm_word = 'word)
      * string
      * Runtime_config.t
      * 'state
      -> t

type execution_error = Machine_backend.execution_error

type stop_reason =
  | Halted
  | Failed
  | Breakpoint of Z.t
  | Step_limit
  | Execution_error of execution_error

type run_result = { session : t; reason : stop_reason; steps : int }

let unknown_backend (name : string) : Diagnostic.t =
  let available = String.concat ", " (Backend_registry.available_backend_names ()) in
  Diagnostic.error (Printf.sprintf "Unknown backend %S. Available backends: %s." name available)

let create_with_backend ?(source_filename : string option) ?(regfile_filename : string option)
    (requested_name : string) (config : Runtime_config.t) (source : string)
    (regfile : string option) (module Backend : Machine_backend.S) : (t, Diagnostic.t list) result =
  match Backend.parse_program ?filename:source_filename source with
  | Error _ as error -> error
  | Ok program -> (
      let regfile_result =
        match regfile with
        | None -> Ok None
        | Some source ->
            Result.map Option.some (Backend.parse_regfile ?filename:regfile_filename source)
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
                        and type asm_word = Backend.asm_word),
                     requested_name,
                     config,
                     state ))))

let create_with_filename_options ~(source_filename : string option)
    ~(regfile_filename : string option) ~(backend : string) ~(config : Cerise_core.Runtime_config.t)
    ~(source : string) ~(regfile : string option) : (t, Diagnostic.t list) result =
  match Backend_registry.find_backend backend with
  | None -> Error [ unknown_backend backend ]
  | Some selected ->
      create_with_backend ?source_filename ?regfile_filename backend config source regfile selected

let create ~(backend : string) ~(config : Cerise_core.Runtime_config.t) ~(source : string)
    ~(regfile : string option) : (t, Diagnostic.t list) result =
  create_with_filename_options ~source_filename:None ~regfile_filename:None ~backend ~config ~source
    ~regfile

let create_with_filenames ~(source_filename : string) ~(regfile_filename : string option)
    ~(backend : string) ~(config : Cerise_core.Runtime_config.t) ~(source : string)
    ~(regfile : string option) : (t, Diagnostic.t list) result =
  create_with_filename_options ~source_filename:(Some source_filename) ~regfile_filename ~backend
    ~config ~source ~regfile

let backend_name (Session (_, requested_name, _, _) : t) : string = requested_name

let view (Session ((module Backend), requested_name, config, state) : t) : Machine_view.t =
  { (Backend.inspect config state) with backend_name = requested_name }

let control (Session ((module Backend), _, config, state) : t) : Machine_backend.control =
  Backend.control config state

let step (Session ((module Backend), requested_name, config, state) : t) :
    (t, execution_error) result =
  Result.map
    (fun state -> Session ((module Backend), requested_name, config, state))
    (Backend.step config state)

let step_n (count : int) (Session ((module Backend), requested_name, config, state) : t) :
    (t, execution_error) result =
  Result.map
    (fun state -> Session ((module Backend), requested_name, config, state))
    (Backend.step_n config count state)

let matching_breakpoint (breakpoints : Z.t list) (program_counter : Z.t option) : Z.t option =
  match program_counter with
  | None -> None
  | Some pc when List.exists (Z.equal pc) breakpoints -> Some pc
  | Some _ -> None

let run ?(breakpoints : Z.t list = []) ?(max_steps : int option) (session : t) : run_result =
  let rec loop (steps : int) (session : t) : run_result =
    let current_control = control session in
    match current_control.status with
    | Machine_view.Halted -> { session; reason = Halted; steps }
    | Failed -> { session; reason = Failed; steps }
    | Running -> (
        match matching_breakpoint breakpoints current_control.pc with
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

let set_register_text (id : Machine_view.register_id) (source : string)
    (Session ((module Backend), requested_name, config, state) : t) : (t, Diagnostic.t list) result
    =
  match Backend.parse_word source with
  | Error _ as error -> error
  | Ok word ->
      Result.map
        (fun state -> Session ((module Backend), requested_name, config, state))
        (Backend.set_register config id word state)

let set_memory_text (address : Z.t) (source : string)
    (Session ((module Backend), requested_name, config, state) : t) : (t, Diagnostic.t list) result
    =
  match Backend.parse_word source with
  | Error _ as error -> error
  | Ok word ->
      Result.map
        (fun state -> Session ((module Backend), requested_name, config, state))
        (Backend.set_memory config address word state)
