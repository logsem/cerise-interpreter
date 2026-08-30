open Cerise

let get_ok (matched_value : ('a, Diagnostic.t list) result) : 'a =
  match matched_value with
  | Ok value -> value
  | Error diagnostics ->
      Alcotest.failf "unexpected diagnostics: %s"
        (String.concat "\n" (List.map Diagnostic.to_string diagnostics))

let int_word (word : Machine_view.word) : Z.t =
  match word.Machine_view.integer with
  | Some value -> value
  | None -> Alcotest.failf "expected integer word, got %s" word.detail_text

let register (bank : Machine_view.register_bank) (key : string) : Machine_view.register_id =
  { Machine_view.Register_id.bank; key }

let register_word (bank : Machine_view.register_bank) (key : string) (session : Machine_session.t) :
    Machine_view.word =
  let view = Machine_session.view session in
  match Machine_view.find_register (register bank key) view with
  | Some register -> register.word
  | None -> Alcotest.failf "register %s is absent" key

let check_z (message : string) (expected : Z.t) (actual : Z.t) : unit =
  Alcotest.(check string) message (Z.to_string expected) (Z.to_string actual)

let create ?(source : string = "halt") ?(regfile : string option) (() : unit) : Machine_session.t =
  Machine_session.create ~backend:Backend_registry.default_backend_name
    ~config:Runtime_config.default ~source ~regfile
  |> get_ok

let create_backend (backend : string) (source : string) : Machine_session.t =
  Machine_session.create ~backend ~config:Runtime_config.default ~source ~regfile:None |> get_ok

let check_control_view ~(backend : string) ~(phase : string) ~(expected : Machine_view.status)
    (session : Machine_session.t) : unit =
  let control = Machine_session.control session in
  let view = Machine_session.view session in
  let label = backend ^ " " ^ phase in
  Alcotest.(check bool) (label ^ " expected status") true (control.status = expected);
  Alcotest.(check bool) (label ^ " status parity") true (control.status = view.status);
  Alcotest.(check (option string))
    (label ^ " PC parity") (Option.map Z.to_string view.pc)
    (Option.map Z.to_string control.pc)

let test_control_view_parity (() : unit) : unit =
  List.iter
    (fun backend ->
      let running = create_backend backend "halt" in
      check_control_view ~backend ~phase:"running" ~expected:Machine_view.Running running;
      let halted = Machine_session.step running |> Result.get_ok in
      check_control_view ~backend ~phase:"halted" ~expected:Machine_view.Halted halted;
      let failed = create_backend backend "fail" |> Machine_session.step |> Result.get_ok in
      check_control_view ~backend ~phase:"failed" ~expected:Machine_view.Failed failed)
    [
      "vanilla"; "locality-cerise"; "ucerise"; "mcerise"; "cerisier"; "griotte"; "griotte-extracted";
    ]

let test_backend_owns_parser (() : unit) : unit =
  let selected = Backend_registry.find_backend "vanilla" |> Option.get in
  let module Backend = (val selected : Machine_backend.S) in
  List.iter
    (fun source ->
      Alcotest.(check bool)
        "vanilla rejects unsupported syntax" true
        (Result.is_error (Backend.parse_program source)))
    [ "getl r1 r2 halt"; "promoteU r1 halt"; "restrict r1 (RW, DIRECTED) halt" ]

let capability_metadata (bank : Machine_view.register_bank) (key : string)
    (session : Machine_session.t) : Machine_view.capability =
  let word = register_word bank key session in
  match word.capability with
  | Some capability -> capability
  | None -> Alcotest.failf "expected capability metadata for register %s" key

let test_interleaved_runtime_configs (() : unit) : unit =
  let first_config = Runtime_config.create ~max_addr:(Z.of_int 64) ~stack_addr:(Z.of_int 20) () in
  let second_config =
    Runtime_config.create ~max_addr:(Z.of_int 1000) ~stack_addr:(Z.of_int 750) ()
  in
  let source = "mov r3 r1\nload r6 r5\nhalt" in
  let regfile = Some "r1 := MAX_ADDR\nr2 := STK_ADDR\nr5 := (RW, 0, MAX_ADDR, MAX_ADDR - 1)" in
  let first =
    Machine_session.create ~backend:Backend_registry.default_backend_name ~config:first_config
      ~source ~regfile
    |> get_ok
  in
  let second =
    Machine_session.create ~backend:Backend_registry.default_backend_name ~config:second_config
      ~source ~regfile
    |> get_ok
  in
  let first_view = Machine_session.view first in
  let second_view = Machine_session.view second in
  check_z "first view keeps its address limit" (Z.of_int 64) first_view.address_limit;
  check_z "second view keeps its address limit" (Z.of_int 1000) second_view.address_limit;
  check_z "first PC limit uses its address space" (Z.of_int 64)
    (capability_metadata Machine_view.System "pc" first).limit;
  check_z "second PC limit uses its address space" (Z.of_int 1000)
    (capability_metadata Machine_view.System "pc" second).limit;
  Alcotest.(check bool)
    "vanilla has no stack-role register" true
    (Machine_view.find_register (register Machine_view.System "stk") (Machine_session.view first)
    = None);
  let first_after_step = Machine_session.step first |> Result.get_ok in
  check_z "other session stays unstepped" Z.zero
    (int_word (register_word Machine_view.General "r3" second));
  let second_after_step = Machine_session.step second |> Result.get_ok in
  check_z "first transition uses first MAX_ADDR" (Z.of_int 64)
    (int_word (register_word Machine_view.General "r3" first_after_step));
  check_z "second transition uses second MAX_ADDR" (Z.of_int 1000)
    (int_word (register_word Machine_view.General "r3" second_after_step));
  check_z "prior first session remains immutable" Z.zero
    (int_word (register_word Machine_view.General "r3" first));
  check_z "prior second session remains immutable" Z.zero
    (int_word (register_word Machine_view.General "r3" second));
  check_z "first stepped view is not contaminated by second" (Z.of_int 64)
    (Machine_session.view first_after_step).address_limit;
  let first_after_memory_step = Machine_session.step first_after_step |> Result.get_ok in
  let second_after_memory_step = Machine_session.step second_after_step |> Result.get_ok in
  check_z "first step reads its last sparse cell" Z.zero
    (int_word (register_word Machine_view.General "r6" first_after_memory_step));
  check_z "second step reads its last sparse cell" Z.zero
    (int_word (register_word Machine_view.General "r6" second_after_memory_step));
  Alcotest.(check bool)
    "bounds-sensitive steps remain running" true
    ((Machine_session.view first_after_memory_step).status = Machine_view.Running
    && (Machine_session.view second_after_memory_step).status = Machine_view.Running);
  let first_edited =
    Machine_session.set_register_text
      (register Machine_view.General "r4")
      "MAX_ADDR" first_after_memory_step
    |> get_ok
  in
  let second_edited =
    Machine_session.set_register_text
      (register Machine_view.General "r4")
      "STK_ADDR" second_after_memory_step
    |> get_ok
  in
  check_z "first edit reuses first MAX_ADDR" (Z.of_int 64)
    (int_word (register_word Machine_view.General "r4" first_edited));
  check_z "second edit reuses second STK_ADDR" (Z.of_int 750)
    (int_word (register_word Machine_view.General "r4" second_edited));
  let first_memory =
    Machine_session.set_memory_text (Z.of_int 63) "MAX_ADDR" first_edited |> get_ok
  in
  let second_memory =
    Machine_session.set_memory_text (Z.of_int 999) "MAX_ADDR" second_edited |> get_ok
  in
  check_z "first memory edit assembles with first config" (Z.of_int 64)
    (Machine_view.find_memory_word (Z.of_int 63) (Machine_session.view first_memory)
    |> Option.get |> int_word);
  check_z "second memory edit assembles with second config" (Z.of_int 1000)
    (Machine_view.find_memory_word (Z.of_int 999) (Machine_session.view second_memory)
    |> Option.get |> int_word);
  Alcotest.(check bool)
    "first sparse bound remains finite" true
    (Machine_view.find_memory_word (Z.of_int 62) (Machine_session.view first_memory)
     |> Option.is_some
    && Machine_view.find_memory_word (Z.of_int 64) (Machine_session.view first_memory) = None);
  Alcotest.(check bool)
    "second sparse bound remains finite" true
    (Machine_view.find_memory_word (Z.of_int 998) (Machine_session.view second_memory)
     |> Option.is_some
    && Machine_view.find_memory_word (Z.of_int 1000) (Machine_session.view second_memory) = None);
  Alcotest.(check bool)
    "first out-of-range edit rejected" true
    (Machine_session.set_memory_text (Z.of_int 64) "0" first_memory |> Result.is_error);
  Alcotest.(check bool)
    "second out-of-range edit rejected" true
    (Machine_session.set_memory_text (Z.of_int 1000) "0" second_memory |> Result.is_error)

let test_registry_and_shared_frontend (() : unit) : unit =
  Alcotest.(check string) "canonical default" "vanilla" Backend_registry.default_backend_name;
  Alcotest.(check (list string))
    "deterministic active backends"
    [
      "vanilla";
      "cerise";
      "locality-cerise";
      "ucerise";
      "mcerise";
      "cerisier";
      "griotte";
      "griotte-extracted";
    ]
    (Backend_registry.available_backend_names ());
  let selected = Backend_registry.find_backend "cerise" |> Option.get in
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
  check_z "macro and label resolved before concrete assembly" (Z.of_int 2)
    (int_word (register_word Machine_view.General "r1" stepped));
  let config = Runtime_config.create ~max_addr:(Z.of_int 100) ~stack_addr:(Z.of_int 75) () in
  let configured =
    Machine_session.create ~backend:Backend_registry.default_backend_name ~config ~source:"halt"
      ~regfile:(Some "r1 := MAX_ADDR\nr2 := STK_ADDR")
    |> get_ok
  in
  check_z "runtime max address is resolved in the shared frontend" (Z.of_int 100)
    (int_word (register_word Machine_view.General "r1" configured));
  check_z "runtime stack address is resolved in the shared frontend" (Z.of_int 75)
    (int_word (register_word Machine_view.General "r2" configured))

let test_view_purity_and_ordering (() : unit) : unit =
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
  let missing = Machine_view.find_memory_word (Z.of_int 20) view |> Option.get in
  check_z "addressable missing memory is backend zero" Z.zero (int_word missing)

let test_immutable_stepping_and_stops (() : unit) : unit =
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

let test_run_results (() : unit) : unit =
  let initial = create ~source:"mov r1 1\nhalt" () in
  let before_first_instruction = Machine_session.run ~breakpoints:[ Z.zero ] initial in
  Alcotest.(check int) "breakpoint at initial PC executes nothing" 0 before_first_instruction.steps;
  Alcotest.(check bool)
    "initial breakpoint reason" true
    (before_first_instruction.reason = Machine_session.Breakpoint Z.zero);
  check_z "initial breakpoint preserves state" Z.zero
    (int_word (register_word Machine_view.General "r1" before_first_instruction.session));
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
  let zero_bounded = Machine_session.run ~max_steps:0 initial in
  Alcotest.(check int) "zero step limit executes nothing" 0 zero_bounded.steps;
  Alcotest.(check bool)
    "zero step limit reason" true
    (zero_bounded.reason = Machine_session.Step_limit);
  check_z "zero step limit preserves state" Z.zero
    (int_word (register_word Machine_view.General "r1" zero_bounded.session));
  let breakpoint_before_zero_limit =
    Machine_session.run ~breakpoints:[ Z.zero ] ~max_steps:0 initial
  in
  Alcotest.(check bool)
    "breakpoint precedes zero step limit" true
    (breakpoint_before_zero_limit.reason = Machine_session.Breakpoint Z.zero);
  let negative = Machine_session.run ~max_steps:(-1) initial in
  Alcotest.(check int) "negative step limit executes nothing" 0 negative.steps;
  Alcotest.(check bool)
    "negative step limit is an execution error" true
    (match negative.reason with
    | Machine_session.Execution_error (Machine_backend.Backend_error message) ->
        String.equal message "step limit must be non-negative"
    | _ -> false);
  check_z "negative step limit preserves state" Z.zero
    (int_word (register_word Machine_view.General "r1" negative.session));
  let halted = Machine_session.run initial in
  let rerun_halted = Machine_session.run halted.session in
  Alcotest.(check int) "rerunning a halted state executes nothing" 0 rerun_halted.steps;
  Alcotest.(check bool) "rerunning halted reason" true (rerun_halted.reason = Machine_session.Halted);
  let failed = Machine_session.run (create ~source:"fail" ()) in
  Alcotest.(check bool)
    "failed transition is preserved" true
    (failed.reason = Machine_session.Failed
    && (Machine_session.view failed.session).status = Machine_view.Failed);
  let rerun_failed = Machine_session.run failed.session in
  Alcotest.(check int) "rerunning a failed state executes nothing" 0 rerun_failed.steps;
  Alcotest.(check bool) "rerunning failed reason" true (rerun_failed.reason = Machine_session.Failed)

let test_text_edits (() : unit) : unit =
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
    Machine_view.find_memory_word (Z.of_int 9) (Machine_session.view edited) |> Option.get
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

let test_diagnostics (() : unit) : unit =
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
    (let selected =
       Backend_registry.find_backend Backend_registry.default_backend_name |> Option.get
     in
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
          Alcotest.test_case "control/view parity" `Quick test_control_view_parity;
          Alcotest.test_case "persistent stepping" `Quick test_immutable_stepping_and_stops;
          Alcotest.test_case "run reasons" `Quick test_run_results;
          Alcotest.test_case "text edits" `Quick test_text_edits;
          Alcotest.test_case "diagnostics" `Quick test_diagnostics;
        ] );
    ]
