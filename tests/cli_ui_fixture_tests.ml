open Cerise

let get_ok = function
  | Ok value -> value
  | Error diagnostics ->
      Alcotest.failf "unexpected diagnostics: %s"
        (String.concat "\n" (List.map Diagnostic.to_string diagnostics))

let check_z message expected actual =
  Alcotest.(check string) message (Z.to_string expected) (Z.to_string actual)

let register bank key = { Machine_view.Register_id.bank; key }

let word_integer word =
  match word.Machine_view.integer with
  | Some integer -> integer
  | None -> Alcotest.fail "expected an integer"

let register_integer id session =
  match Machine_view.find_register id (Machine_session.view session) with
  | Some register -> word_integer register.word
  | None -> Alcotest.fail "register was absent"

let create ?(backend = "vanilla") ?(source = "halt") ?regfile config =
  Machine_session.create ~backend ~config ~source ~regfile |> get_ok

let parse argv = Cli_options.parse (Array.of_list ("cerise-interpreter" :: argv))

let contains text fragment =
  let text_length = String.length text and fragment_length = String.length fragment in
  let rec loop index =
    if index + fragment_length > text_length then false
    else if String.sub text index fragment_length = fragment then true
    else loop (index + 1)
  in
  loop 0

let check_same_session message expected actual =
  Alcotest.(check bool) message true (expected == actual)

let diagnostic_source = function
  | diagnostic :: _ -> (
      match Diagnostic.location diagnostic with
      | Some { source = Some source; _ } -> source
      | _ -> Alcotest.fail "diagnostic has no source filename")
  | [] -> Alcotest.fail "expected a diagnostic"

let test_cli () =
  let default = parse [ "program.s" ] |> Result.get_ok in
  Alcotest.(check string) "default backend" "vanilla" default.backend;
  let alias = parse [ "--backend"; "cerise"; "program.s" ] |> Result.get_ok in
  Alcotest.(check string) "alias spelling is preserved" "cerise" alias.backend;
  List.iter
    (fun backend ->
      let options = parse [ "--backend"; backend; "program.s" ] |> Result.get_ok in
      Alcotest.(check string) ("registered spelling " ^ backend) backend options.backend)
    (Backend_registry.names ());
  let configured = parse [ "--mem-size"; "17"; "program.s" ] |> Result.get_ok in
  check_z "configured address limit" (Z.of_int 17) (Runtime_config.max_addr configured.config);
  List.iter
    (fun arguments ->
      Alcotest.(check bool) "invalid command line rejected" true (Result.is_error (parse arguments)))
    [
      [ "--backend"; "missing"; "program.s" ];
      [ "--mem-size"; "0"; "program.s" ];
      [ "--mem-size"; "-1"; "program.s" ];
      [ "--mem-size"; "bad"; "program.s" ];
      [ "--sealing"; "program.s" ];
      [ "--no-sealing"; "program.s" ];
      [ "--stack"; "program.s" ];
      [ "--no-stack"; "program.s" ];
      [ "--uperms"; "program.s" ];
      [ "--no-uperms"; "program.s" ];
      [ "--locality"; "Global"; "program.s" ];
      [ "--version"; "vanilla"; "program.s" ];
      [ "--backend"; "default"; "program.s" ];
      [ "--backend"; "vanilla-cerise"; "program.s" ];
      [ "--backend"; "stack-cerise"; "program.s" ];
      [ "--backend"; "sealing-cerise"; "program.s" ];
      [ "--backend"; "seal_cerise"; "program.s" ];
      [ "--backend"; "custom"; "program.s" ];
    ];
  match parse [ "--backend"; "missing"; "program.s" ] with
  | Error message ->
      Alcotest.(check string) "diagnostic lists names"
        "Unknown backend \"missing\". Available backends: vanilla, cerise, locality-cerise, ucerise, mcerise, cerisier, griotte, griotte-extracted."
        message
  | Ok _ -> Alcotest.fail "missing backend accepted"

let test_application_history_and_navigation () =
  let config = Runtime_config.create ~max_addr:(Z.of_int 8) () in
  let initial = create ~source:"mov r1 7\nhalt" config in
  let state = Application_model.create initial in
  let after_step = Application_model.step state |> Result.get_ok in
  Alcotest.(check int) "step retains prior session" 1 (Application_model.history_length after_step);
  check_same_session "step undo restores initial session" initial
    (Application_model.session (Application_model.undo after_step));
  let after_ten = Application_model.step_n 10 after_step |> Result.get_ok in
  Alcotest.(check int) "ten-step retains prior session" 2 (Application_model.history_length after_ten);
  check_same_session "ten-step undo restores stepped session" (Application_model.session after_step)
    (Application_model.session (Application_model.undo after_ten));
  let register_edited =
    Application_model.set_register_text (register Machine_view.General "r2") "99" after_ten |> get_ok
  in
  check_same_session "register edit undo restores exact prior session" (Application_model.session after_ten)
    (Application_model.session (Application_model.undo register_edited));
  let memory_edited = Application_model.set_memory_text (Z.of_int 7) "33" register_edited |> get_ok in
  Alcotest.(check int) "both edits are undoable" 4 (Application_model.history_length memory_edited);
  check_same_session "memory edit undo restores exact register-edited session"
    (Application_model.session register_edited) (Application_model.session (Application_model.undo memory_edited));
  let undone = Application_model.undo memory_edited |> Application_model.undo in
  check_z "undo retains prior register view" Z.zero
    (register_integer (register Machine_view.General "r2") (Application_model.session undone));
  let moved = Application_model.move_primary (Z.of_int 999) state in
  check_z "sparse navigation clamps at limit" (Z.of_int 7) (Application_model.primary_start moved);
  let moved = Application_model.move_primary (Z.of_int (-999)) moved in
  check_z "sparse navigation clamps at zero" Z.zero (Application_model.primary_start moved);
  let no_capabilities =
    initial
    |> Machine_session.set_register_text (register Machine_view.System "pc") "0" |> get_ok
    |> Machine_session.set_register_text (register Machine_view.System "ddc") "0" |> get_ok
    |> Application_model.create
  in
  Alcotest.(check int) "edited vanilla can have no capability registers" 0
    (List.length (Application_model.capability_registers no_capabilities));
  let no_capability_state = Application_model.select_next_capability no_capabilities in
  Alcotest.(check bool) "no-capability selection stays absent" true
    (Application_model.selected_capability no_capability_state = None);
  let no_capability_state = Application_model.follow_secondary no_capability_state in
  check_z "no-capability follow is safe" Z.zero (Application_model.secondary_start no_capability_state);
  List.iter
    (fun height ->
      let registers, primary, secondary = Application_model.row_budget ~height ~register_count:33 in
      let rendered_rows =
        if height <= 0 then 0 else if height = 1 then 1 else if height = 2 then 2
        else if height = 3 then 3
        else 4 + registers + primary + secondary
      in
      Alcotest.(check bool) "row budget fits terminal" true (rendered_rows <= height))
    [ 0; 1; 2; 3; 4; 5; 24 ]

let test_filename_diagnostics () =
  let config = Runtime_config.create ~max_addr:(Z.of_int 16) () in
  (match
     Machine_session.create_with_filenames ~source_filename:"broken-program.s" ~regfile_filename:None
       ~backend:"vanilla" ~config ~source:"mov r1" ~regfile:None
   with
  | Error diagnostics -> Alcotest.(check string) "program filename reaches parser" "broken-program.s" (diagnostic_source diagnostics)
  | Ok _ -> Alcotest.fail "invalid program was accepted");
  match
    Machine_session.create_with_filenames ~source_filename:"program.s"
      ~regfile_filename:(Some "broken-registers.reg") ~backend:"vanilla" ~config ~source:"halt"
      ~regfile:(Some "r1 := @")
  with
  | Error diagnostics ->
      Alcotest.(check string) "regfile filename reaches parser" "broken-registers.reg" (diagnostic_source diagnostics)
  | Ok _ -> Alcotest.fail "invalid regfile was accepted"

let test_capabilities_and_rendering () =
  let config = Runtime_config.create ~max_addr:(Z.of_int 32) () in
  let vanilla = create ~backend:"vanilla" config in
  List.iter
    (fun (backend, expected) ->
      let decoded = create ~backend ~source:"mov r1 7\nhalt" config |> Machine_session.view in
      match Machine_view.memory_at Z.zero decoded with
      | Some { decoded_instruction = Some text; _ } ->
          Alcotest.(check string) (backend ^ " decoded instruction includes operands") expected text
      | _ -> Alcotest.failf "%s encoded move did not produce decoded instruction text" backend)
    [
      ("vanilla", "mov r1 7");
      ("locality-cerise", "mov r1 7");
      ("ucerise", "mov r1 7");
      ("mcerise", "mov r1 7");
      ("cerisier", "mov r1 7");
      ("griotte", "mov cra 7");
      ("griotte-extracted", "mov cra 7");
    ];
  let vanilla_state = Application_model.create vanilla |> Application_model.select_next_capability in
  (match Application_model.selected_capability vanilla_state with
  | Some register ->
      Alcotest.(check (option string)) "vanilla has no locality metadata" None
        (Option.bind register.word.capability (fun capability -> capability.locality))
  | None -> Alcotest.fail "pc capability should be selectable");
  let locality = create ~backend:"locality-cerise" config in
  let locality_state = Application_model.create locality |> Application_model.select_next_capability in
  (match Application_model.selected_capability locality_state with
  | Some register ->
      Alcotest.(check bool) "locality backend exposes locality" true
        (Option.is_some (Option.bind register.word.capability (fun capability -> capability.locality)))
  | None -> Alcotest.fail "locality pc should be selectable");
  let state = Interactive_ui.create vanilla in
  let wide_image = Interactive_ui.render ~width:120 ~height:8 state in
  let wide = Interactive_ui.snapshot ~width:120 ~height:8 state in
  let narrow = Interactive_ui.snapshot ~width:30 ~height:4 state in
  Alcotest.(check int) "wide image width" 120 (Notty.I.width wide_image);
  Alcotest.(check int) "wide image height" 8 (Notty.I.height wide_image);
  Alcotest.(check bool) "wide composed layout has both panel titles" true
    (contains wide "HEAP" && contains wide "CAPABILITY pc");
  Alcotest.(check bool) "wide composed layout has status labels" true
    (contains wide "backend: vanilla" && contains wide "machine state: Running");
  Alcotest.(check bool) "wide composed layout has PC bounds and cursor" true
    (contains wide "┏ ▶ ");
  Alcotest.(check bool) "narrow layout leads with status and HEAP" true
    (contains narrow "machine state: Running" && contains narrow "HEAP");
  ignore (Interactive_ui.render ~width:1 ~height:1 state);
  let next event state =
    match Interactive_ui.transition ~rows:8 event state with
    | Some state -> state | None -> Alcotest.fail "unexpected quit"
  in
  let event_state = Interactive_ui.create (create ~source:"mov r1 7\nhalt" config) in
  let event_state = next Interactive_ui.Step event_state in
  Alcotest.(check int) "step event retains history" 1
    (Application_model.history_length (Interactive_ui.application event_state));
  let event_state = next Interactive_ui.Step_ten event_state in
  Alcotest.(check int) "ten-step event retains history" 2
    (Application_model.history_length (Interactive_ui.application event_state));
  let event_state = next Interactive_ui.Undo event_state in
  let event_state = next (Interactive_ui.Move_primary Z.one) event_state in
  let event_state = next (Interactive_ui.Page_primary 1) event_state in
  let event_state = next (Interactive_ui.Move_secondary Z.one) event_state in
  let event_state = next (Interactive_ui.Page_secondary 1) event_state in
  let event_state = next Interactive_ui.Follow_primary event_state in
  let event_state = next Interactive_ui.Follow_secondary event_state in
  let event_state = next Interactive_ui.Cycle_capability event_state in
  let event_state = next Interactive_ui.Toggle_secondary event_state in
  let event_state = next (Interactive_ui.Resize (40, 8)) event_state in
  Alcotest.(check bool) "toggle removes secondary panel" true
    (not (contains (Interactive_ui.snapshot ~width:120 ~height:8 event_state) "CAPABILITY /"));
  Alcotest.(check bool) "quit event is terminal" true
    (Interactive_ui.transition ~rows:8 Interactive_ui.Quit event_state = None);
  let halted = next Interactive_ui.Step (Interactive_ui.create (create ~source:"halt" config)) in
  Alcotest.(check bool) "halted step keeps UI alive" true
    (Option.is_some (Interactive_ui.transition ~rows:8 Interactive_ui.Step halted));
  let failed = next Interactive_ui.Step (Interactive_ui.create (create ~source:"fail" config)) in
  Alcotest.(check bool) "failed step keeps UI alive" true
    (Option.is_some (Interactive_ui.transition ~rows:8 Interactive_ui.Step failed));
  List.iter
    (fun backend ->
      let session = create ~backend config in
      let state = Interactive_ui.create session in
      let state = next Interactive_ui.Step state in
      let state = next Interactive_ui.Step_ten state in
      let state = next Interactive_ui.Undo state in
      let state = next Interactive_ui.Cycle_capability state in
      let state = next Interactive_ui.Toggle_secondary state in
      ignore (Interactive_ui.render ~width:100 ~height:24 state))
    (Backend_registry.names ())

let () =
  Alcotest.run "cli-ui-fixture"
    [
      ("cli", [ Alcotest.test_case "backend and sizes" `Quick test_cli ]);
      ( "application",
        [ Alcotest.test_case "history and sparse navigation" `Quick test_application_history_and_navigation ] );
      ("diagnostics", [ Alcotest.test_case "source filenames" `Quick test_filename_diagnostics ]);
      ( "rendering",
        [ Alcotest.test_case "capabilities and fixed rendering" `Quick test_capabilities_and_rendering ] );
    ]
