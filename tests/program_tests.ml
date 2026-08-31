open Cerise

let ok (matched_value : ('a, Diagnostic.t list) result) : 'a =
  match matched_value with
  | Ok value -> value
  | Error diagnostics ->
      Alcotest.fail (String.concat "; " (List.map Diagnostic.to_string diagnostics))

let resolve_file (path : string) : string =
  let candidates = [ path; "../" ^ path; "../../../" ^ path; "../../../../" ^ path ] in
  match List.find_opt Sys.file_exists candidates with
  | Some resolved -> resolved
  | None -> Alcotest.failf "program file does not exist: %s" path

let read_file (path : string) : string =
  let resolved = resolve_file path in
  In_channel.with_open_bin resolved In_channel.input_all

let config = Runtime_config.create ~max_addr:(Z.of_int 65_536) ~stack_addr:(Z.of_int 32_768) ()

let parse_program (backend : string) (path : string) : unit =
  let source = read_file path in
  match backend with
  | "vanilla" -> ignore (ok (Vanilla.Parser.parse_program ~filename:path source))
  | "locality-cerise" -> ignore (ok (Locality_cerise.Parser.parse_program ~filename:path source))
  | "ucerise" -> ignore (ok (Ucerise.Parser.parse_program ~filename:path source))
  | "mcerise" -> ignore (ok (Mcerise.Parser.parse_program ~filename:path source))
  | _ -> Alcotest.failf "unsupported program-test backend: %s" backend

let create_session (backend : string) (source_path : string) (regfile_path : string option) :
    Machine_session.t =
  let regfile = Option.map read_file regfile_path in
  ok
    (Machine_session.create_with_filenames ~source_filename:source_path
       ~regfile_filename:regfile_path ~backend ~config ~source:(read_file source_path) ~regfile)

let run_program ?(max_steps = 100_000) (backend : string) (source_path : string)
    (regfile_path : string option) : Machine_session.run_result =
  create_session backend source_path regfile_path |> Machine_session.run ~max_steps

let require_halted (path : string) (result : Machine_session.run_result) : unit =
  match result.reason with
  | Machine_session.Halted ->
      Alcotest.(check bool)
        (path ^ " final status") true
        ((Machine_session.view result.session).status = Machine_view.Halted)
  | Failed -> Alcotest.failf "%s failed after %d steps" path result.steps
  | Step_limit -> Alcotest.failf "%s reached the %d-step limit" path result.steps
  | Breakpoint pc -> Alcotest.failf "%s hit breakpoint %s" path (Z.to_string pc)
  | Execution_error error ->
      Alcotest.failf "%s produced an execution error after %d steps: %s" path result.steps
        (Machine_backend.execution_error_message error)

let require_failed (backend : string) (path : string) () : unit =
  let result = run_program backend path None in
  (match result.reason with
  | Machine_session.Failed -> ()
  | Halted -> Alcotest.failf "%s unexpectedly halted after %d steps" path result.steps
  | Step_limit -> Alcotest.failf "%s reached the %d-step limit" path result.steps
  | Breakpoint pc -> Alcotest.failf "%s hit breakpoint %s" path (Z.to_string pc)
  | Execution_error error ->
      Alcotest.failf "%s produced an execution error after %d steps: %s" path result.steps
        (Machine_backend.execution_error_message error));
  Alcotest.(check bool)
    (path ^ " final status") true
    ((Machine_session.view result.session).status = Machine_view.Failed)

let register (key : string) (view : Machine_view.t) : Machine_view.register =
  let id : Machine_view.register_id =
    if String.equal key "r0" then { bank = System; key = "ddc" } else { bank = General; key }
  in
  match Machine_view.find_register id view with
  | Some value -> value
  | None -> Alcotest.failf "missing register %s" key

let check_integer (path : string) (key : string) (expected : int) (view : Machine_view.t) : unit =
  match (register key view).word.integer with
  | Some actual ->
      Alcotest.(check string) (path ^ " " ^ key) (string_of_int expected) (Z.to_string actual)
  | None -> Alcotest.failf "%s %s is not an integer" path key

let check_permission (path : string) (key : string) (expected : string) (view : Machine_view.t) :
    unit =
  match (register key view).word.capability with
  | Some capability ->
      Alcotest.(check (list string))
        (path ^ " " ^ key ^ " permission")
        [ expected ] capability.permissions
  | None -> Alcotest.failf "%s %s is not a capability" path key

let positive_programs =
  [
    ("vanilla", "tests/test_files/vanilla/pos/macros.s");
    ("vanilla", "tests/test_files/vanilla/pos/mov_test.s");
    ("vanilla", "tests/test_files/vanilla/pos/jmper.s");
    ("vanilla", "tests/test_files/vanilla/pos/test1.s");
    ("vanilla", "tests/test_files/vanilla/pos/test1_labels.s");
    ("locality-cerise", "tests/test_files/locality/pos/get_otype.s");
    ("locality-cerise", "tests/test_files/locality/pos/get_wtype.s");
    ("locality-cerise", "tests/test_files/locality/pos/jmper.s");
    ("locality-cerise", "tests/test_files/locality/pos/seal_unseal.s");
    ("locality-cerise", "tests/test_files/locality/pos/sealing_counter.s");
    ("locality-cerise", "tests/test_files/locality/pos/test1_labels.s");
    ("locality-cerise", "tests/test_files/locality/pos/test_invoke.s");
    ("ucerise", "tests/test_files/ucerise/pos/test1.s");
    ("ucerise", "tests/test_files/ucerise/pos/test_stk.s");
    ("ucerise", "tests/test_files/ucerise/pos/test_ucaps.s");
    ("mcerise", "tests/test_files/mcerise/pos/test_directed_store.s");
    ("mcerise", "tests/test_files/mcerise/pos/test_locality_flow.s");
    ("mcerise", "tests/test_files/mcerise/pos/ucap_promote.s");
  ]

let parse_positive_programs () : unit =
  List.iter (fun (backend, path) -> parse_program backend path) positive_programs

let run_and_view (backend : string) (path : string) : Machine_view.t =
  let result = run_program backend path None in
  require_halted path result;
  Machine_session.view result.session

let mov_test () =
  let path = "tests/test_files/vanilla/pos/mov_test.s" in
  let view = run_and_view "vanilla" path in
  check_integer path "r2" 28 view;
  check_integer path "r5" (-30) view

let locality_jmper () =
  let path = "tests/test_files/locality/pos/jmper.s" in
  let view = run_and_view "locality-cerise" path in
  check_integer path "r2" 12 view;
  check_permission path "r1" "E" view

let macros () =
  let path = "tests/test_files/vanilla/pos/macros.s" in
  let view = run_and_view "vanilla" path in
  check_integer path "r2" 7 view

let ucap_promote () =
  let path = "tests/test_files/mcerise/pos/ucap_promote.s" in
  let view = run_and_view "mcerise" path in
  List.iter
    (fun (key, permission) -> check_permission path key permission view)
    [ ("r0", "RWLX"); ("r1", "RWL"); ("r2", "RWX"); ("r3", "RW") ]

let test_ucaps () =
  let path = "tests/test_files/ucerise/pos/test_ucaps.s" in
  let view = run_and_view "ucerise" path in
  check_integer path "r0" 42 view;
  check_integer path "r1" 43 view;
  check_permission path "r2" "RWLX" view

let halting_program (backend : string) (path : string) () : unit =
  ignore (run_and_view backend path)

let get_otype () =
  let path = "tests/test_files/locality/pos/get_otype.s" in
  let view = run_and_view "locality-cerise" path in
  List.iter (fun key -> check_integer path key (-1) view) [ "r0"; "r1"; "r2" ];
  check_integer path "r3" 10 view

let get_wtype () =
  let path = "tests/test_files/locality/pos/get_wtype.s" in
  let view = run_and_view "locality-cerise" path in
  List.iter (fun key -> check_integer path key 0 view) [ "r0"; "r1"; "r2"; "r3" ]

let sealing_counter () =
  let path = "tests/test_files/locality/pos/sealing_counter.s" in
  let view = run_and_view "locality-cerise" path in
  check_integer path "r2" 3 view

let negative_programs =
  let paths backend directory names =
    List.map (fun name -> (backend, directory ^ "/" ^ name ^ ".s")) names
  in
  paths "vanilla" "tests/test_files/vanilla/neg"
    [ "bad_encoding1"; "bad_encoding2"; "bad_encoding3"; "bad_sealing_int"; "bad_sealing_sealed" ]
  @ paths "locality-cerise" "tests/test_files/locality/neg"
      [
        "bad_flow1";
        "bad_flow2";
        "bad_flow3";
        "bad_flow_WL";
        "bad_flow_locality";
        "bad_flow_seal1";
        "bad_flow_seal2";
        "bad_invoke1";
        "bad_invoke2";
        "bad_invoke3";
        "bad_perm_seal";
        "bad_perm_unseal";
        "bad_write_local";
        "bad_write_local_sealed";
        "lea_perm_not_entry";
      ]
  @ paths "ucerise" "tests/test_files/ucerise/neg" [ "bad_loadU"; "bad_promote"; "bad_storeU" ]
  @ paths "mcerise" "tests/test_files/mcerise/neg"
      [ "bad_flow_locality2"; "bad_flow_locality3"; "bad_store_directed"; "bad_store_udirected" ]

let parse_support_fragments () : unit =
  List.iter
    (fun path -> parse_program "vanilla" path)
    [ "case_studies/vanilla/assert.s"; "case_studies/vanilla/malloc.s" ]

let case_study_programs =
  [
    ( "vanilla",
      "case_studies/vanilla/cap_machine_lecture_exercise.s",
      Some "case_studies/vanilla/cap_machine_lecture_exercise.reg" );
    ("vanilla", "case_studies/vanilla/malloc_test.s", None);
    ( "vanilla",
      "case_studies/vanilla/encapsulated_counter.s",
      Some "case_studies/vanilla/encapsulated_counter.reg" );
    ( "vanilla",
      "case_studies/vanilla/interval_object.s",
      Some "case_studies/vanilla/interval_object.reg" );
    ( "vanilla",
      "case_studies/vanilla/local_state_encapsulation.s",
      Some "case_studies/vanilla/local_state_encapsulation.reg" );
    ( "vanilla",
      "case_studies/vanilla/buffer_sharing.s",
      Some "case_studies/vanilla/buffer_sharing.reg" );
    ( "vanilla",
      "case_studies/vanilla/read_only_sharing.s",
      Some "case_studies/vanilla/read_only_sharing.reg" );
    ( "vanilla",
      "case_studies/vanilla/dynamic_sealing.s",
      Some "case_studies/vanilla/dynamic_sealing.reg" );
    ( "ucerise",
      "case_studies/ucerise/awkward_revocation.s",
      Some "case_studies/ucerise/awkward_revocation.reg" );
    ("mcerise", "case_studies/mcerise/downward_lse.s", Some "case_studies/mcerise/downward_lse.reg");
    ("mcerise", "case_studies/mcerise/stack_object.s", Some "case_studies/mcerise/stack_object.reg");
  ]

let run_case_study (backend : string) (path : string) (regfile : string option) () : unit =
  run_program backend path regfile |> require_halted path

let historical_runtime_tests =
  [
    Alcotest.test_case "vanilla mov_test runtime" `Quick mov_test;
    Alcotest.test_case "locality jmper runtime" `Quick locality_jmper;
    Alcotest.test_case "vanilla macros runtime" `Quick macros;
    Alcotest.test_case "mcerise ucap_promote runtime" `Quick ucap_promote;
    Alcotest.test_case "ucerise test_ucaps runtime" `Quick test_ucaps;
    Alcotest.test_case "mcerise locality flow runtime" `Quick
      (halting_program "mcerise" "tests/test_files/mcerise/pos/test_locality_flow.s");
    Alcotest.test_case "mcerise directed store runtime" `Quick
      (halting_program "mcerise" "tests/test_files/mcerise/pos/test_directed_store.s");
    Alcotest.test_case "locality get_otype runtime" `Quick get_otype;
    Alcotest.test_case "locality get_wtype runtime" `Quick get_wtype;
    Alcotest.test_case "locality seal and unseal runtime" `Quick
      (halting_program "locality-cerise" "tests/test_files/locality/pos/seal_unseal.s");
    Alcotest.test_case "locality sealing counter runtime" `Quick sealing_counter;
  ]

let () =
  Alcotest.run "checked-in programs"
    [
      ( "positive programs",
        [ Alcotest.test_case "all active positive programs parse" `Quick parse_positive_programs ]
      );
      ("historical runtime behavior", historical_runtime_tests);
      ( "negative programs",
        List.map
          (fun (backend, path) -> Alcotest.test_case path `Quick (require_failed backend path))
          negative_programs );
      ( "case studies",
        Alcotest.test_case "vanilla support fragments parse" `Quick parse_support_fragments
        :: List.map
             (fun (backend, path, regfile) ->
               Alcotest.test_case path `Quick (run_case_study backend path regfile))
             case_study_programs );
    ]
