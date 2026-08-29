open Cerise

let get_ok = function
  | Ok value -> value
  | Error diagnostics ->
      Alcotest.failf "unexpected diagnostics: %s"
        (String.concat "\n" (List.map Diagnostic.to_string diagnostics))

let int_word word =
  match word.Machine_view.integer with
  | Some value -> value
  | None -> Alcotest.failf "expected integer word, got %s" word.detail_text

let register bank key = { Machine_view.Register_id.bank; key }

let register_word bank key session =
  let view = Machine_session.view session in
  match Machine_view.find_register (register bank key) view with
  | Some register -> register.word
  | None -> Alcotest.failf "register %s is absent" key

let check_z message expected actual =
  Alcotest.(check string) message (Z.to_string expected) (Z.to_string actual)

let create ?(source = "halt") ?regfile () =
  Machine_session.create ~backend:Backend_registry.default ~config:Runtime_config.default ~source
    ~regfile
  |> get_ok

let test_backend_owns_parser () =
  let source = "getl r1 stk\nseal r2 r0 r0\npromoteU r3\nrestrict r4 (RW, DIRECTED)\nhalt" in
  let profiles =
    [
      Parameters.vanilla_cerise;
      Parameters.stack_cerise;
      Parameters.mcerise;
      Parameters.sealing_cerise;
      Parameters.full_cerise;
      Parameters.custom_cerise;
    ]
  in
  let previous = !Parameters.flags in
  Fun.protect
    ~finally:(fun () -> Parameters.flags := previous)
    (fun () ->
      List.iter
        (fun profile ->
          Parameters.flags := profile;
          let selected = Backend_registry.find Backend_registry.default |> Option.get in
          let module Backend = (val selected : Machine_backend.S) in
          Alcotest.(check bool)
            "vanilla syntax remains fixed" true
            (Result.is_error (Backend.parse_program source));
          Alcotest.(check string)
            "backend parser does not leak the selected legacy profile" profile.version
            !Parameters.flags.version)
        profiles)

let capability_metadata bank key session =
  let word = register_word bank key session in
  match word.capability with
  | Some capability -> capability
  | None -> Alcotest.failf "expected capability metadata for register %s" key

let test_interleaved_runtime_configs () =
  let first_config = Runtime_config.create ~max_addr:(Z.of_int 64) ~stack_addr:(Z.of_int 20) () in
  let second_config =
    Runtime_config.create ~max_addr:(Z.of_int 1000) ~stack_addr:(Z.of_int 750) ()
  in
  let source = "mov r3 r1\nhalt" in
  let regfile = Some "r1 := MAX_ADDR\nr2 := STK_ADDR" in
  let previous = !Parameters.flags in
  Fun.protect
    ~finally:(fun () -> Parameters.flags := previous)
    (fun () ->
      Parameters.flags := Parameters.vanilla_cerise;
      let check_legacy_profile_restored message =
        Alcotest.(check string) message Parameters.vanilla_cerise.version !Parameters.flags.version;
        check_z (message ^ " (max_addr)") Parameters.vanilla_cerise.max_addr
          !Parameters.flags.max_addr
      in
      let first =
        Machine_session.create ~backend:Backend_registry.default ~config:first_config ~source
          ~regfile
        |> get_ok
      in
      check_legacy_profile_restored "first creation restores legacy globals";
      let second =
        Machine_session.create ~backend:Backend_registry.default ~config:second_config ~source
          ~regfile
        |> get_ok
      in
      check_legacy_profile_restored "second creation restores legacy globals";
      let first_view = Machine_session.view first in
      check_legacy_profile_restored "first inspection restores legacy globals";
      let second_view = Machine_session.view second in
      check_legacy_profile_restored "second inspection restores legacy globals";
      check_z "first view keeps its address limit" (Z.of_int 64) first_view.address_limit;
      check_z "second view keeps its address limit" (Z.of_int 1000) second_view.address_limit;
      check_z "first PC limit uses its address space" (Z.of_int 64)
        (capability_metadata Machine_view.System "pc" first).limit;
      check_z "second PC limit uses its address space" (Z.of_int 1000)
        (capability_metadata Machine_view.System "pc" second).limit;
      Alcotest.(check bool)
        "vanilla has no stack-role register" true
        (Machine_view.find_register
           (register Machine_view.System "stk")
           (Machine_session.view first)
        = None);
      let first_after_step = Machine_session.step first |> Result.get_ok in
      check_legacy_profile_restored "first step restores legacy globals";
      check_z "other session stays unstepped" Z.zero
        (int_word (register_word Machine_view.General "r3" second));
      let second_after_step = Machine_session.step second |> Result.get_ok in
      check_legacy_profile_restored "second step restores legacy globals";
      check_z "first transition uses first MAX_ADDR" (Z.of_int 64)
        (int_word (register_word Machine_view.General "r3" first_after_step));
      check_z "second transition uses second MAX_ADDR" (Z.of_int 1000)
        (int_word (register_word Machine_view.General "r3" second_after_step));
      check_z "prior first session remains immutable" Z.zero
        (int_word (register_word Machine_view.General "r3" first));
      check_z "prior second session remains immutable" Z.zero
        (int_word (register_word Machine_view.General "r3" second));
      check_z "first stepped view is not contaminated by second" (Z.of_int 64)
        (Machine_session.view first_after_step).address_limit)

let test_registry_and_shared_frontend () =
  Alcotest.(check string) "canonical default" "vanilla" Backend_registry.default;
  Alcotest.(check (list string))
    "deterministic active backends"
    [ "vanilla"; "cerise"; "locality-cerise"; "ucerise"; "mcerise"; "griotte" ]
    (Backend_registry.names ());
  let selected = Backend_registry.find "cerise" |> Option.get in
  let module Backend = (val selected : Machine_backend.S) in
  Alcotest.(check string) "alias selects canonical module" "vanilla" Backend.name;
  let source =
    "%macro put(reg:reg, value:value)\n\
     mov $reg $value\n\
     %endmacro\n\
     %put(r1, target)\n\
     halt\n\
     target:\n\
     # 7"
  in
  let session = create ~source () in
  Alcotest.(check string)
    "requested backend spelling retained" "vanilla"
    (Machine_session.backend_name session);
  Alcotest.(check string)
    "requested spelling reaches the view" "vanilla" (Machine_session.view session).backend_name;
  let stepped = Machine_session.step session |> Result.get_ok in
  check_z "macro and label resolved before lowering" (Z.of_int 2)
    (int_word (register_word Machine_view.General "r1" stepped));
  let config = Runtime_config.create ~max_addr:(Z.of_int 100) ~stack_addr:(Z.of_int 75) () in
  let configured =
    Machine_session.create ~backend:Backend_registry.default ~config ~source:"halt"
      ~regfile:(Some "r1 := MAX_ADDR\nr2 := STK_ADDR")
    |> get_ok
  in
  check_z "runtime max address is resolved in the shared frontend" (Z.of_int 100)
    (int_word (register_word Machine_view.General "r1" configured));
  check_z "runtime stack address is resolved in the shared frontend" (Z.of_int 75)
    (int_word (register_word Machine_view.General "r2" configured))

let test_view_purity_and_ordering () =
  let session = create ~source:"halt\n# 7" () in
  let view = Machine_session.view session in
  Alcotest.(check string) "backend metadata" "vanilla" view.backend_name;
  Alcotest.(check string)
    "address limit exposed"
    (Z.to_string (Runtime_config.max_addr Runtime_config.default))
    (Z.to_string view.address_limit);
  Alcotest.(check (option string)) "pc cursor" (Some "0") (Option.map Z.to_string view.pc);
  let addresses = List.map (fun cell -> Z.to_int cell.Machine_view.address) view.memory in
  Alcotest.(check (list int)) "ascending sparse memory" [ 0; 1 ] addresses;
  let register_labels = List.map (fun register -> register.Machine_view.label) view.registers in
  Alcotest.(check string) "first register" "pc" (List.hd register_labels);
  Alcotest.(check string) "last register" "r31" (List.hd (List.rev register_labels));
  let missing = Machine_view.memory_at (Z.of_int 20) view |> Option.get in
  check_z "addressable missing memory is backend zero" Z.zero (int_word missing)

let test_immutable_stepping_and_stops () =
  let initial = create ~source:"mov r1 41\nadd r2 r1 1\nhalt" () in
  let after_one = Machine_session.step initial |> Result.get_ok in
  check_z "prior session remains unchanged" Z.zero
    (int_word (register_word Machine_view.General "r1" initial));
  check_z "new session has transition" (Z.of_int 41)
    (int_word (register_word Machine_view.General "r1" after_one));
  let finished = Machine_session.step_n 20 initial |> Result.get_ok in
  Alcotest.(check bool)
    "step_n stops at halt" true
    ((Machine_session.view finished).status = Machine_view.Halted);
  check_z "arithmetic through bounded stepping" (Z.of_int 42)
    (int_word (register_word Machine_view.General "r2" finished));
  Alcotest.(check bool)
    "stepping a stopped state reports an error" true
    (match Machine_session.step finished with
    | Error (Machine_backend.Stopped Machine_view.Halted) -> true
    | _ -> false)

let test_run_results () =
  let initial = create ~source:"mov r1 1\nhalt" () in
  let at_breakpoint = Machine_session.run ~breakpoints:[ Z.one ] initial in
  Alcotest.(check int) "one instruction before breakpoint" 1 at_breakpoint.steps;
  Alcotest.(check bool)
    "breakpoint reason retains address" true
    (match at_breakpoint.reason with
    | Machine_session.Breakpoint address -> Z.equal address Z.one
    | _ -> false);
  let loop = create ~source:"jmp pc" () in
  let bounded = Machine_session.run ~max_steps:3 loop in
  Alcotest.(check int) "bounded run count" 3 bounded.steps;
  Alcotest.(check bool) "bounded run reason" true (bounded.reason = Machine_session.Step_limit);
  let failed = Machine_session.run (create ~source:"fail" ()) in
  Alcotest.(check bool)
    "failed transition is preserved" true
    (failed.reason = Machine_session.Failed
    && (Machine_session.view failed.session).status = Machine_view.Failed)

let test_text_edits () =
  let initial = create ~source:"halt" () in
  let edited =
    Machine_session.set_register_text (register Machine_view.General "r1") "99" initial |> get_ok
  in
  check_z "register text edit" (Z.of_int 99)
    (int_word (register_word Machine_view.General "r1" edited));
  check_z "register edit is persistent" Z.zero
    (int_word (register_word Machine_view.General "r1" initial));
  let edited = Machine_session.set_memory_text (Z.of_int 9) "123" edited |> get_ok in
  let memory_word =
    Machine_view.memory_at (Z.of_int 9) (Machine_session.view edited) |> Option.get
  in
  check_z "memory text edit" (Z.of_int 123) (int_word memory_word);
  let capability =
    Machine_session.set_register_text (register Machine_view.General "r2") "(RW, 0, 10, 3)" edited
    |> get_ok
    |> register_word Machine_view.General "r2"
  in
  Alcotest.(check bool)
    "capability metadata" true
    (match capability.capability with
    | Some metadata ->
        Z.equal metadata.base Z.zero
        && Z.equal metadata.limit (Z.of_int 10)
        && Z.equal metadata.cursor (Z.of_int 3)
    | None -> false)

let test_diagnostics () =
  Alcotest.(check bool)
    "unknown backend diagnostic" true
    (match
       Machine_session.create ~backend:"missing" ~config:Runtime_config.default ~source:"halt"
         ~regfile:None
     with
    | Error [ diagnostic ] -> String.length (Diagnostic.message diagnostic) > 0
    | _ -> false);
  Alcotest.(check bool)
    "parse diagnostic carries a source position" true
    (let selected = Backend_registry.find Backend_registry.default |> Option.get in
     let module Backend = (val selected : Machine_backend.S) in
     match Backend.parse_program ~filename:"broken.s" "mov r1" with
     | Error (diagnostic :: _) -> Option.is_some (Diagnostic.location diagnostic)
     | _ -> false)

let () =
  Alcotest.run "Core contracts"
    [
      ( "session/view",
        [
          Alcotest.test_case "backend-owned parser" `Quick test_backend_owns_parser;
          Alcotest.test_case "interleaved runtime configs" `Quick test_interleaved_runtime_configs;
          Alcotest.test_case "registry and shared frontend" `Quick test_registry_and_shared_frontend;
          Alcotest.test_case "pure ordered view" `Quick test_view_purity_and_ordering;
          Alcotest.test_case "persistent stepping" `Quick test_immutable_stepping_and_stops;
          Alcotest.test_case "run reasons" `Quick test_run_results;
          Alcotest.test_case "text edits" `Quick test_text_edits;
          Alcotest.test_case "diagnostics" `Quick test_diagnostics;
        ] );
    ]
