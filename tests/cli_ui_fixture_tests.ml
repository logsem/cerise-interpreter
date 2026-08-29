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
      [ "--version"; "vanilla"; "program.s" ];
      [ "--sealing"; "program.s" ];
      [ "--no-sealing"; "program.s" ];
      [ "--stack"; "program.s" ];
      [ "--no-stack"; "program.s" ];
      [ "--uperms"; "program.s" ];
      [ "--no-uperms"; "program.s" ];
      [ "--locality"; "Global"; "program.s" ];
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

let test_capabilities_and_website_fixture () =
  let config = Runtime_config.create ~max_addr:(Z.of_int 32) () in
  let vanilla = create ~backend:"vanilla" config in
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
  let fixture =
    Website_fixture.create ~backend:"cerise" ~config ~source:"mov r1 4\nhalt" ~regfile:None |> get_ok
  in
  Alcotest.(check string) "fixture session keeps alias spelling" "cerise"
    (Machine_session.backend_name (Website_fixture.session fixture));
  Alcotest.(check string) "fixture keeps alias spelling" "cerise" (Website_fixture.view fixture).backend_name;
  let fixture =
    Website_fixture.edit_register (register Machine_view.General "r2") "11" fixture |> get_ok
  in
  let fixture = Website_fixture.edit_memory (Z.of_int 31) "22" fixture |> get_ok in
  let before_step = Website_fixture.session fixture in
  let fixture = Website_fixture.step fixture |> Result.get_ok in
  let fixture = Website_fixture.undo fixture in
  Alcotest.(check bool) "website undo retains exact immutable session" true
    (Machine_session.view before_step = Website_fixture.view fixture);
  let fixture = Website_fixture.select_next_capability fixture |> Website_fixture.follow_selected_capability in
  Alcotest.(check bool) "fixture selects a capability" true
    (Option.is_some (Website_fixture.selected_capability fixture));
  let fixture = Website_fixture.navigate_memory (Z.of_int 999) fixture in
  check_z "fixture navigation remains bounded" (Z.of_int 31) (Website_fixture.memory_start fixture)

let () =
  Alcotest.run "cli-ui-fixture"
    [
      ("cli", [ Alcotest.test_case "backend and sizes" `Quick test_cli ]);
      ( "application",
        [ Alcotest.test_case "history and sparse navigation" `Quick test_application_history_and_navigation ] );
      ("diagnostics", [ Alcotest.test_case "source filenames" `Quick test_filename_diagnostics ]);
      ( "website",
        [ Alcotest.test_case "public session/view fixture" `Quick test_capabilities_and_website_fixture ] );
    ]
