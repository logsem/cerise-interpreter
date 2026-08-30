open Cerise

let ok (matched_value : ('a, Diagnostic.t list) result) : 'a = match matched_value with
  | Ok x -> x
  | Error diagnostics ->
      Alcotest.fail (String.concat "; " (List.map Diagnostic.message diagnostics))

let check_reject (parser : (string -> ('a, 'b) result)) (source : string) : unit = Alcotest.(check bool) source true (Result.is_error (parser source))

let read_file (path : string) : string =
  let channel = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let allocation_test (() : unit) : unit =
  let expected_v =
    [
      ("Jmp", 0, 1);
      ("Jnz", 1, 1);
      ("Move", 2, 2);
      ("Load", 4, 1);
      ("Store", 5, 2);
      ("Add", 7, 4);
      ("Sub", 11, 4);
      ("Mul", 15, 4);
      ("Rem", 19, 4);
      ("Div", 23, 4);
      ("Lt", 27, 4);
      ("Lea", 31, 2);
      ("Restrict", 33, 2);
      ("SubSeg", 35, 4);
      ("GetB", 39, 1);
      ("GetE", 40, 1);
      ("GetA", 41, 1);
      ("GetP", 42, 1);
      ("GetOType", 43, 1);
      ("GetWType", 44, 1);
      ("Seal", 45, 1);
      ("UnSeal", 46, 1);
      ("Invoke", 47, 1);
      ("Fail", 48, 1);
      ("Halt", 49, 1);
    ]
  in
  Alcotest.(check (list (triple string int int)))
    "vanilla golden table" expected_v Vanilla.Codec.allocations;
  let expected_l =
    List.map (fun (n, o, s) -> if o < 39 then (n, o, s) else (n, o + 1, s)) expected_v
  in
  let expected_l =
    let before, after = List.partition (fun (_, o, _) -> o < 39) expected_l in
    before @ [ ("GetL", 39, 1) ] @ after
  in
  Alcotest.(check (list (triple string int int)))
    "locality golden table" expected_l Locality_cerise.Codec.allocations

let parser_matrix (() : unit) : unit =
  ignore (ok (Vanilla.Parser.parse_program "%define N 2 start: move r1 start + N # 4 halt"));
  ignore
    (ok
       (Vanilla.Parser.parse_program
          "%macro put(dst: reg, x: value) move $dst $x %endmacro %put(r2, 9) halt"));
  ignore
    (ok
       (Vanilla.Parser.parse_program
          "%macro narrow(p: perm) restrict r1 $p %endmacro %narrow(RO) halt"));
  ignore (ok (Vanilla.Parser.parse_word "(RW, 0, MAX_ADDR, 0)"));
  ignore (ok (Locality_cerise.Parser.parse_word "(RWLX, LOCAL, STK_ADDR, MAX_ADDR, STK_ADDR)"));
  ignore (ok (Locality_cerise.Parser.parse_program "getl r1 r31 halt"));
  ignore
    (ok
       (Locality_cerise.Parser.parse_program
          "%macro loc(x: locality) move r1 $x %endmacro %loc(GLOBAL) halt"));
  check_reject Vanilla.Parser.parse_word "(RW, GLOBAL, 0, 10, 0)";
  check_reject Vanilla.Parser.parse_program "getl r1 r2";
  check_reject Vanilla.Parser.parse_program "loadu r1 r2 0";
  check_reject Locality_cerise.Parser.parse_program "storeu r1 0 0";
  check_reject Locality_cerise.Parser.parse_word "(RW, DIRECTED, 0, 10, 0)";
  check_reject Locality_cerise.Parser.parse_word "(URW, GLOBAL, 0, 10, 0)"

let session (backend : string) (source : string) : Machine_session.t =
  ok
    (Machine_session.create ~backend
       ~config:(Runtime_config.create ~max_addr:(Z.of_int 64) ~stack_addr:(Z.of_int 32) ())
       ~source ~regfile:None)

let integer_register (label : string) (view : Machine_view.t) : Z.t =
  match Machine_view.find_register { bank = General; key = label } view with
  | Some { word = { integer = Some z; _ }; _ } -> z
  | _ -> Alcotest.fail ("missing " ^ label)

let execution (() : unit) : unit =
  let initial = session "vanilla" "move r1 40 add r2 r1 2 halt" in
  let after = (Machine_session.run initial).session in
  Alcotest.(check string)
    "arithmetic" "42"
    (Z.to_string (integer_register "r2" (Machine_session.view after)));
  Alcotest.(check bool)
    "persistent source" true
    (Z.equal (integer_register "r1" (Machine_session.view initial)) Z.zero);
  let locality = session "locality-cerise" "getl r1 r31 halt" in
  let finished = (Machine_session.run locality).session in
  Alcotest.(check string)
    "GetL Local"
    (Z.to_string (Locality_cerise.Codec.encode_locality Locality_cerise.Ast.Local))
    (Z.to_string (integer_register "r1" (Machine_session.view finished)))

let memory_and_sealing (() : unit) : unit =
  let config = Runtime_config.create ~max_addr:(Z.of_int 64) ~stack_addr:(Z.of_int 32) () in
  let make (source : string) (regfile : string) : Machine_session.t =
    ok (Machine_session.create ~backend:"vanilla" ~config ~source ~regfile:(Some regfile))
  in
  let memory = Machine_session.run (make "store r1 42 load r2 r1 halt" "r1 := (RW, 10, 11, 10)") in
  Alcotest.(check string)
    "store/load" "42"
    (Z.to_string (integer_register "r2" (Machine_session.view memory.session)));
  let sealed =
    Machine_session.run
      (make "seal r3 r1 r2 unseal r4 r1 r3 halt" "r1 := [SU, 0, 10, 3] r2 := (RO, 0, 10, 2)")
  in
  let r4 =
    Option.get
      (Machine_view.find_register { bank = General; key = "r4" }
         (Machine_session.view sealed.session))
  in
  Alcotest.(check bool) "unsealed capability" true (r4.word.kind = Capability);
  let invoked =
    Machine_session.run
      (make "invoke r1 r2 fail fail fail fail halt"
         "r1 := {6: (RX, 0, 7, 5)} r2 := {6: (RO, 0, 10, 1)}")
  in
  Alcotest.(check bool)
    "invoke reaches sentry target" true
    ((Machine_session.view invoked.session).status = Halted)

let restriction_locality_and_edits (() : unit) : unit =
  let config = Runtime_config.create ~max_addr:(Z.of_int 64) ~stack_addr:(Z.of_int 32) () in
  let restricted =
    ok
      (Machine_session.create ~backend:"vanilla" ~config
         ~source:"restrict r1 RO subseg r1 2 10 halt" ~regfile:(Some "r1 := (RW, 0, 20, 5)"))
    |> Machine_session.run
    |> fun result -> result.session
  in
  let r1 =
    Option.get
      (Machine_view.find_register { bank = General; key = "r1" } (Machine_session.view restricted))
  in
  let capability = Option.get r1.word.capability in
  Alcotest.(check (list string)) "restricted permission" [ "RO" ] capability.permissions;
  Alcotest.(check string) "subseg base" "2" (Z.to_string capability.base);
  Alcotest.(check string) "subseg limit" "10" (Z.to_string capability.limit);
  let locality = session "locality-cerise" "restrict r1 (RWX, GLOBAL) halt" in
  let locality =
    ok
      (Machine_session.set_register_text { bank = General; key = "r1" } "(RWLX, LOCAL, 0, 20, 4)"
         locality)
    |> Machine_session.run
    |> fun result -> result.session
  in
  let r1 =
    Option.get
      (Machine_view.find_register { bank = General; key = "r1" } (Machine_session.view locality))
  in
  Alcotest.(check (option string))
    "locality can flow Local to Global" (Some "GLOBAL")
    (Option.bind r1.word.capability (fun capability -> capability.locality));
  let vanilla =
    session "vanilla" "move r1 8 add r2 r1 1 halt" |> Machine_session.step_n 2 |> Result.get_ok
  in
  let local =
    session "locality-cerise" "move r1 8 add r2 r1 1 halt"
    |> Machine_session.step_n 2 |> Result.get_ok
  in
  Alcotest.(check string)
    "global-only r1 transition equivalence"
    (Z.to_string (integer_register "r1" (Machine_session.view vanilla)))
    (Z.to_string (integer_register "r1" (Machine_session.view local)));
  Alcotest.(check string)
    "global-only r2 transition equivalence"
    (Z.to_string (integer_register "r2" (Machine_session.view vanilla)))
    (Z.to_string (integer_register "r2" (Machine_session.view local)))

let codec_round_trips (() : unit) : unit =
  let open Vanilla.Ast in
  let r1 = Reg 1 and r2 = Reg 2 and o = Constant (Z.of_int (-7)) in
  let instructions =
    [
      Jmp r1;
      Jnz (r1, r2);
      Move (r1, o);
      Load (r1, r2);
      Store (r1, o);
      Add (r1, o, Register r2);
      Sub (r1, o, o);
      Mul (r1, o, o);
      Rem (r1, o, o);
      Div (r1, o, o);
      Lt (r1, o, o);
      Lea (r1, o);
      Restrict (r1, o);
      SubSeg (r1, o, o);
      GetB (r1, r2);
      GetE (r1, r2);
      GetA (r1, r2);
      GetP (r1, r2);
      GetOType (r1, r2);
      GetWType (r1, r2);
      Seal (r1, r2, PC);
      UnSeal (r1, r2, PC);
      Invoke (r1, r2);
      Fail;
      Halt;
    ]
  in
  List.iter
    (fun instruction ->
      let encoded = Result.get_ok (Vanilla.Codec.encode instruction) in
      Alcotest.(check bool)
        "codec round trip" true
        (Result.get_ok (Vanilla.Codec.decode encoded) = instruction))
    instructions;
  let large_constant = Z.neg (Z.logor (Z.shift_left Z.one 100_003) (Z.of_int 0x35)) in
  let large_instruction = Move (Reg 1, Constant large_constant) in
  let encoded = Result.get_ok (Vanilla.Codec.encode large_instruction) in
  Alcotest.(check bool)
    "large finite vanilla instruction round trip" true
    (Vanilla.Codec.decode encoded = Ok large_instruction)

let parameterized_values_and_total_decoders (() : unit) : unit =
  let vanilla_source =
    "%macro cap(p: perm, e: expr) # ($p, 0, $e, 0) %endmacro %cap(RW, 10) halt"
  in
  let vanilla = Machine_session.view (session "vanilla" vanilla_source) in
  let vanilla_capability =
    Option.get (Option.get (Machine_view.find_memory_word Z.zero vanilla)).capability
  in
  Alcotest.(check (list string))
    "parameterized vanilla capability" [ "RW" ] vanilla_capability.permissions;
  Alcotest.(check string) "parameterized vanilla limit" "10" (Z.to_string vanilla_capability.limit);
  ignore
    (session "vanilla"
       "%macro range(sp: sealperm, e: expr) # [$sp, 0, $e, 0] %endmacro %range(SU, 12) halt");
  let locality_source =
    "%macro cap(p: perm, l: locality, e: expr) # ($p, $l, 0, $e, 0) %endmacro %cap(RWL, LOCAL, 11) \
     halt"
  in
  let locality = Machine_session.view (session "locality-cerise" locality_source) in
  let locality_capability =
    Option.get (Option.get (Machine_view.find_memory_word Z.zero locality)).capability
  in
  Alcotest.(check (option string))
    "parameterized capability locality" (Some "LOCAL") locality_capability.locality;
  let config = Runtime_config.create ~max_addr:(Z.of_int 64) ~stack_addr:(Z.of_int 32) () in
  let seal_pair =
    ok
      (Machine_session.create ~backend:"locality-cerise" ~config
         ~source:
           "%macro narrow(sp: sealperm, l: locality) restrict r4 ($sp, $l) %endmacro %narrow(S, \
            GLOBAL) halt"
         ~regfile:(Some "r4 := [SU, LOCAL, 0, 10, 2]"))
    |> Machine_session.run
  in
  Alcotest.(check bool)
    "parameterized seal/locality restriction" true
    ((Machine_session.view seal_pair.session).status = Halted);
  ignore (ok (Vanilla.Parser.parse_program "MOVE R1 DDC JNZ PC R0 MOVE STK R31 HALT"));
  ignore (ok (Locality_cerise.Parser.parse_program "MOVE R1 DDC JNZ PC R0 MOVE STK R31 HALT"));
  let huge = Z.shift_left Z.one 10000 in
  let decoder_is_total (type input output error) (decoder : input -> (output, error) result)
      (value : input) : bool =
    match decoder value with Error _ -> true | Ok _ -> false | exception _ -> false
  in
  List.iter
    (fun value ->
      Alcotest.(check bool)
        "vanilla permission decoder total" true
        (decoder_is_total Vanilla.Codec.decode_permission value);
      Alcotest.(check bool)
        "vanilla seal decoder total" true
        (decoder_is_total Vanilla.Codec.decode_seal_permission value);
      Alcotest.(check bool)
        "local permission decoder total" true
        (decoder_is_total Locality_cerise.Codec.decode_permission value);
      Alcotest.(check bool)
        "local seal decoder total" true
        (decoder_is_total Locality_cerise.Codec.decode_seal_permission value);
      Alcotest.(check bool)
        "local locality decoder total" true
        (decoder_is_total Locality_cerise.Codec.decode_locality value);
      Alcotest.(check bool)
        "local permission/locality decoder total" true
        (decoder_is_total Locality_cerise.Codec.decode_permission_locality value);
      Alcotest.(check bool)
        "local seal/locality decoder total" true
        (decoder_is_total Locality_cerise.Codec.decode_seal_permission_locality value))
    [ huge; Z.neg huge ];
  Alcotest.(check bool)
    "huge correctly tagged vanilla seal decoder total" true
    (decoder_is_total Vanilla.Codec.decode_seal_permission Z.(huge + one));
  Alcotest.(check bool)
    "huge correctly tagged locality decoder total" true
    (decoder_is_total Locality_cerise.Codec.decode_locality Z.(huge + of_int 2));
  Alcotest.(check bool)
    "huge correctly tagged permission/locality decoder total" true
    (decoder_is_total Locality_cerise.Codec.decode_permission_locality Z.(huge + of_int 4));
  Alcotest.(check bool)
    "huge correctly tagged seal/locality decoder total" true
    (decoder_is_total Locality_cerise.Codec.decode_seal_permission_locality Z.(huge + of_int 5));
  let run_malformed (backend : string) (malformed : Z.t) (regfile : string) : Machine_session.run_result =
    ok
      (Machine_session.create ~backend ~config
         ~source:("restrict r1 " ^ Z.to_string malformed ^ " halt")
         ~regfile:(Some regfile))
    |> Machine_session.run
  in
  Alcotest.(check bool)
    "huge vanilla restrict fails without exception" true
    ((Machine_session.view (run_malformed "vanilla" huge "r1 := (RW, 0, 10, 2)").session).status
   = Failed);
  Alcotest.(check bool)
    "huge locality restrict fails without exception" true
    ((Machine_session.view
        (run_malformed "locality-cerise" Z.(huge + of_int 4) "r1 := (RW, GLOBAL, 0, 10, 2)").session)
       .status = Failed)

let initial_views_and_alias (() : unit) : unit =
  Alcotest.(check string) "default" "vanilla" Backend_registry.default_backend_name;
  Alcotest.(check (list string))
    "names"
    [ "vanilla"; "cerise"; "locality-cerise"; "ucerise"; "mcerise"; "cerisier"; "griotte"; "griotte-extracted" ]
    (Backend_registry.available_backend_names ());
  List.iter
    (fun name -> Alcotest.(check bool) ("old name absent: " ^ name) true
      (Option.is_none (Backend_registry.find_backend name)))
    [ "default"; "vanilla-cerise"; "stack-cerise"; "sealing-cerise"; "seal_cerise"; "custom" ];
  let alias = session "cerise" "halt" in
  Alcotest.(check string) "requested alias" "cerise" (Machine_session.backend_name alias);
  let vanilla = Machine_session.view (session "vanilla" "halt") in
  let ddc = Option.get (Machine_view.find_register { bank = System; key = "ddc" } vanilla) in
  Alcotest.(check bool)
    "vanilla no locality" true
    (Option.bind ddc.word.capability (fun c -> c.locality) = None);
  let local = Machine_session.view (session "locality-cerise" "halt") in
  let stk = Option.get (Machine_view.find_register { bank = System; key = "stk" } local) in
  Alcotest.(check bool) "stack role" true (stk.role = Stack_pointer);
  Alcotest.(check string)
    "stack locality" "LOCAL"
    (Option.get (Option.bind stk.word.capability (fun c -> c.locality)))

let generated_parser_locations_and_round_trips (() : unit) : unit =
  let check_location (label : string) (expected_line : int) (expected_column : int) (matched_value : ('a, Diagnostic.t list) result) : unit = match matched_value with
    | Error (diagnostic :: _) -> (
        match Diagnostic.location diagnostic with
        | Some location ->
            Alcotest.(check (option string)) (label ^ " filename") (Some "active.s") location.source;
            Alcotest.(check int) (label ^ " line") expected_line location.line;
            Alcotest.(check int) (label ^ " column") expected_column location.column
        | None -> Alcotest.fail (label ^ " diagnostic has no source location"))
    | Error [] -> Alcotest.fail (label ^ " returned no diagnostic")
    | Ok _ -> Alcotest.fail (label ^ " unexpectedly parsed")
  in
  Vanilla.Parser.parse_program ~filename:"active.s" "halt\n@" |> check_location "lexer" 2 1;
  Vanilla.Parser.parse_program ~filename:"active.s" "halt\nmove r1"
  |> check_location "parser" 2 8;
  let config = Runtime_config.create ~max_addr:(Z.of_int 64) ~stack_addr:(Z.of_int 32) () in
  let vanilla_term = ok (Vanilla.Parser.parse_word "{7: (RW, 0, MAX_ADDR, 3)}") in
  let vanilla_word = ok (Vanilla.Asm_ir.lower_word config vanilla_term) in
  let vanilla_round_trip =
    Vanilla.Printer.word vanilla_word |> Vanilla.Parser.parse_word |> ok
    |> Vanilla.Asm_ir.lower_word config |> ok
  in
  Alcotest.(check bool) "vanilla word round trip" true (vanilla_word = vanilla_round_trip);
  let locality_term =
    ok (Locality_cerise.Parser.parse_word "[SU, LOCAL, 0, STK_ADDR, 3]")
  in
  let locality_word = ok (Locality_cerise.Asm_ir.lower_word config locality_term) in
  let locality_round_trip =
    Locality_cerise.Printer.word locality_word |> Locality_cerise.Parser.parse_word |> ok
    |> Locality_cerise.Asm_ir.lower_word config |> ok
  in
  Alcotest.(check bool) "locality word round trip" true (locality_word = locality_round_trip);
  ignore (ok (Vanilla.Parser.parse_regfile "r1 := 1 ; comment with spaces\n r2 := (RO, 0, 4, 0)"))

let active_macro_hygiene_and_resolution (() : unit) : unit =
  let source =
    "%define OFFSET 2 \
     %macro inner(dst: reg) private: move $dst private + OFFSET %endmacro \
     %macro outer(dst: reg) %inner($dst) %endmacro \
     %outer(r1) %outer(r2) move r3 &CURRENT_ADDR halt"
  in
  let final = Machine_session.run (session "vanilla" source) |> fun result -> result.session in
  let view = Machine_session.view final in
  Alcotest.(check string) "first hygienic label" "2"
    (Z.to_string (integer_register "r1" view));
  Alcotest.(check string) "second hygienic label" "3"
    (Z.to_string (integer_register "r2" view));
  Alcotest.(check string) "resolved current address" "2"
    (Z.to_string (integer_register "r3" view));
  let collision =
    "__macro_0_inner_private: move r8 __macro_0_inner_private " ^ source
  in
  ignore (ok (Vanilla.Parser.parse_program collision));
  ignore
    (ok
       (Vanilla.Parser.parse_program
          "%macro halt(dst: reg) move $dst halt %endmacro halt: %halt(r4) halt"));
  check_reject Vanilla.Parser.parse_word "(RW, LOCAL, 0, 4, 0)";
  ignore (ok (Locality_cerise.Parser.parse_word "(RW, GLOBAL, 0, 4, 0)"))

let checked_in_vanilla_corpus (() : unit) : unit =
  let base = "test_files/vanilla/pos/" in
  List.iter
    (fun name -> ignore (ok (Vanilla.Parser.parse_program ~filename:name (read_file (base ^ name)))))
    [
      "cap_machine_lecture_exercise.s";
      "jmper.s";
      "mov_test.s";
      "test1.s";
      "test1_labels.s";
    ];
  ignore
    (ok
       (Vanilla.Parser.parse_regfile ~filename:"cap_machine_lecture_exercise.reg"
          (read_file (base ^ "cap_machine_lecture_exercise.reg"))))

let () =
  Alcotest.run "active backends"
    [
      ( "contracts",
        [
          Alcotest.test_case "allocation" `Quick allocation_test;
          Alcotest.test_case "parser matrix" `Quick parser_matrix;
          Alcotest.test_case "codec round trips" `Quick codec_round_trips;
          Alcotest.test_case "parameterized values and total decoders" `Quick
            parameterized_values_and_total_decoders;
          Alcotest.test_case "execution" `Quick execution;
          Alcotest.test_case "memory sealing invoke" `Quick memory_and_sealing;
          Alcotest.test_case "restriction locality edits" `Quick restriction_locality_and_edits;
          Alcotest.test_case "views and alias" `Quick initial_views_and_alias;
          Alcotest.test_case "generated diagnostics and round trips" `Quick
            generated_parser_locations_and_round_trips;
          Alcotest.test_case "active macro hygiene and resolution" `Quick
            active_macro_hygiene_and_resolution;
          Alcotest.test_case "checked-in vanilla syntax corpus" `Quick checked_in_vanilla_corpus;
        ] );
    ]
