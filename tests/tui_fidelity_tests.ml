open Cerise

let get_ok (matched_value : ('a, Diagnostic.t list) result) : 'a = match matched_value with
  | Ok value -> value
  | Error diagnostics ->
      Alcotest.failf "unexpected diagnostics: %s"
        (String.concat "\n" (List.map Diagnostic.to_string diagnostics))

let config = Runtime_config.create ~max_addr:(Z.of_int 64) ()

let create ?(source : string = "halt") ?regfile:(regfile : string option) (backend : string) : Machine_session.t =
  Machine_session.create ~backend ~config ~source ~regfile |> get_ok

let set_word (text : string) (session : Machine_session.t) : Machine_view.word =
  Machine_session.set_memory_text (Z.of_int 10) text session |> get_ok
  |> Machine_session.view |> Machine_view.find_memory_word (Z.of_int 10) |> Option.get

let check_z (label : string) (expected : Z.t) (actual : Z.t) : unit =
  Alcotest.(check string) label (Z.to_string expected) (Z.to_string actual)

let check_range (label : string) ~locality:(locality : string option) (word : Machine_view.word) : unit =
  Alcotest.(check bool) (label ^ " has no capability metadata") true
    (Option.is_none word.Machine_view.capability);
  match word.seal_range with
  | None -> Alcotest.failf "%s has no typed seal range" label
  | Some range ->
      check_z (label ^ " base") Z.zero range.base;
      check_z (label ^ " limit") (Z.of_int 8) range.limit;
      check_z (label ^ " cursor") Z.one range.cursor;
      Alcotest.(check (option string)) (label ^ " locality") locality range.locality

let check_capability (label : string) ~locality:(locality : string option) (word : Machine_view.word) : unit =
  Alcotest.(check bool) (label ^ " has no seal-range metadata") true
    (Option.is_none word.Machine_view.seal_range);
  match word.capability with
  | None -> Alcotest.failf "%s has no capability metadata" label
  | Some capability ->
      check_z (label ^ " base") Z.zero capability.base;
      check_z (label ^ " limit") (Z.of_int 8) capability.limit;
      check_z (label ^ " cursor") Z.one capability.cursor;
      Alcotest.(check (option string)) (label ^ " locality") locality capability.locality

let metadata_contracts (() : unit) : unit =
  let fixtures =
    [
      ("vanilla", "(RW, 0, 8, 1)", "{3: (RW, 0, 8, 1)}",
       "[SU, 0, 8, 1]", "{3: [S, 0, 8, 1]}", None);
      ("locality-cerise", "(RW, LOCAL, 0, 8, 1)", "{3: (RW, LOCAL, 0, 8, 1)}",
       "[SU, LOCAL, 0, 8, 1]", "{3: [S, LOCAL, 0, 8, 1]}", Some "LOCAL");
      ("cerisier", "(RW, 0, 8, 1)", "{3: (RW, 0, 8, 1)}",
       "[SU, 0, 8, 1]", "{3: [S, 0, 8, 1]}", None);
      ("griotte", "([R W DL DRO], Local, 0, 8, 1)",
       "{3: ([R W DL DRO], Local, 0, 8, 1)}",
       "[SU, Local, 0, 8, 1]", "{3: [S, Local, 0, 8, 1]}", Some "Local");
      ("griotte-extracted", "([R W DL DRO], Local, 0, 8, 1)",
       "{3: ([R W DL DRO], Local, 0, 8, 1)}",
       "[SU, Local, 0, 8, 1]", "{3: [S, Local, 0, 8, 1]}", Some "Local");
    ]
  in
  List.iter
    (fun (backend, capability, sealed_capability, seal_range, sealed_seal_range, locality) ->
      let session = create backend in
      let capability = set_word capability session in
      check_capability (backend ^ " capability") ~locality capability;
      Alcotest.(check bool) (backend ^ " capability unsealed") true
        (Option.is_none capability.sealing);
      let sealed_capability = set_word sealed_capability session in
      check_capability (backend ^ " sealed capability") ~locality sealed_capability;
      Alcotest.(check bool) (backend ^ " sealed capability flag") true
        (Option.fold ~none:false ~some:(fun s -> s.Machine_view.is_sealed)
           sealed_capability.sealing);
      let seal_range = set_word seal_range session in
      check_range (backend ^ " seal range") ~locality seal_range;
      Alcotest.(check bool) (backend ^ " seal range permission") true
        (match seal_range.sealing with
        | Some { can_seal = Some true; can_unseal = Some true; is_sealed = false; _ } -> true
        | _ -> false);
      let sealed_seal_range = set_word sealed_seal_range session in
      check_range (backend ^ " sealed seal range") ~locality sealed_seal_range;
      Alcotest.(check bool) (backend ^ " sealed seal-range flag") true
        (Option.fold ~none:false ~some:(fun s -> s.Machine_view.is_sealed)
           sealed_seal_range.sealing))
    fixtures;
  List.iter
    (fun backend ->
      let view = Machine_session.view (create backend) in
      let words =
        List.map (fun (r : Machine_view.register) -> r.word) view.registers
        @ List.map (fun (c : Machine_view.memory_cell) -> c.word) view.memory
      in
      Alcotest.(check bool) (backend ^ " is sealing-free") true
        (List.for_all
           (fun word -> Option.is_none word.Machine_view.seal_range
             && Option.is_none word.sealing)
           words))
    [ "ucerise"; "mcerise" ]

let base_word (kind : Machine_view.semantic_kind) : Machine_view.word =
  {
    Machine_view.edit_text = "fallback";
    short_text = "fallback";
    detail_text = "fallback";
    decoded_instruction = None;
    fingerprint = "";
    kind;
    integer = None;
    capability = None;
    seal_range = None;
    sealing = None;
    annotations = [];
  }

let integer (value : Z.t) : Machine_view.word = { (base_word Machine_view.Integer) with integer = Some value }
let capability ?locality:(locality : string option)
    ?(permissions : string list = [ "RWX" ]) (kind : Machine_view.semantic_kind) :
    Machine_view.word =
  {
    (base_word kind) with
    capability =
      Some
        {
          Machine_view.base = Z.of_int 0x1200;
          limit = Z.of_int 0x12ff;
          cursor = Z.of_int 0x1234;
          permissions;
          locality;
        };
  }

let seal_range ?locality:(locality : string option) ~sealed:(sealed : bool) (permission : string) : Machine_view.word =
  {
    (base_word (if sealed then Machine_view.Sealed_capability else Seal_range)) with
    seal_range =
      Some
        {
          Machine_view.base = Z.of_int 0x1200;
          limit = Z.of_int 0x12ff;
          cursor = Z.of_int 0x1234;
          locality;
        };
    sealing =
      Some
        {
          object_type = (if sealed then Some (Z.of_int 0x33) else None);
          can_seal = Some (permission = "S" || permission = "SU");
          can_unseal = Some (permission = "U" || permission = "SU");
          is_sealed = sealed;
        };
  }

let sealed_capability =
  {
    (capability ~locality:"Global" Machine_view.Sealed_capability) with
    sealing =
      Some
        {
          object_type = Some (Z.of_int 0x33);
          can_seal = None;
          can_unseal = None;
          is_sealed = true;
        };
  }

let component_goldens (() : unit) : unit =
  let render (side : Interactive_ui.side) (word : Machine_view.word) : string =
    Interactive_ui.word_snapshot ~address_limit:(Z.of_int 0x10000) ~width:52 ~side word
  in
  let cases =
    [
      ("positive integer", render Left (integer (Z.of_int 0x1234)));
      ("negative integer", render Left (integer (Z.of_int (-17))));
      ("large integer", render Left (integer Z.(shift_left one 180 + of_int 0x55)));
      ("global capability", render Left (capability ~locality:"GLOBAL" Capability));
      ("local composite capability",
       render Left
         (capability ~locality:"local" ~permissions:[ "R"; "W"; "DL"; "DRO" ] Capability));
      ("directed sentry", render Left (capability ~locality:"DIRECTED" Sentry));
      ("seal range", render Left (seal_range ~locality:"Local" ~sealed:false "SU"));
      ("sealed capability", render Left sealed_capability);
      ("sealed seal range", render Left (seal_range ~locality:"Directed" ~sealed:true "S"));
      ("mirrored capability", render Right (capability ~locality:"Global" Capability));
      ("mirrored sealed range", render Right (seal_range ~locality:"Local" ~sealed:true "U"));
      ("opaque fallback", render Left { (base_word Opaque) with short_text = "opaque backend word" });
      ("malformed fallback",
       render Left { (base_word Capability) with short_text = "malformed capability" });
    ]
  in
  let actual =
    String.concat "\n"
      (List.map (fun (label, value) -> Printf.sprintf "%s=%s|" label value) cases)
  in
  let expected =
    {|positive integer=                            1234|
negative integer=                             -11|
large integer=100000000000000..000000000000055|
global capability=         RWX   Global    12[00-FF]   1234|
local composite capability=         [R W DL DRO]    Local     12[00-FF]   1234|
directed sentry=         RWX   Directed  12[00-FF]   1234|
seal range=         SU    Local     12[00-FF]   1234|
sealed capability={   33: RWX   Global    12[00-FF]   1234}|
sealed seal range={   33: S     Directed  12[00-FF]   1234}|
mirrored capability= 1234 RWX   Global    12[00-FF] |
mirrored sealed range={   33:  1234 U     Local     12[00-FF] }|
opaque fallback=             opaque backend word|
malformed fallback=            malformed capability||}
  in
  Alcotest.(check string) "semantic word text golden" expected actual

let contains (text : string) (fragment : string) : bool =
  let rec loop (index : int) : bool =
    if index + String.length fragment > String.length text then false
    else if String.sub text index (String.length fragment) = fragment then true
    else loop (index + 1)
  in
  fragment = "" || loop 0

let ansi_styles (() : unit) : unit =
  let render (word : Machine_view.word) : string =
    Interactive_ui.word_ansi_snapshot ~address_limit:(Z.of_int 0x10000)
      ~width:52 ~side:Interactive_ui.Left word
  in
  let styled =
    [
      ("capability", render (capability Capability));
      ("sentry", render (capability Sentry));
      ("seal range", render (seal_range ~sealed:false "SU"));
      ("sealed capability", render sealed_capability);
      ("sealed seal range", render (seal_range ~sealed:true "SU"));
      ("fallback", render (base_word Opaque));
    ]
  in
  List.iter
    (fun (label, output) ->
      Alcotest.(check bool) (label ^ " emits ANSI styling") true (contains output "\027["))
    styled;
  let output (label : string) : string = List.assoc label styled in
  List.iter
    (fun (label, sgr) ->
      Alcotest.(check bool) (label ^ " exact ANSI color") true
        (contains (output label) sgr))
    [
      ("capability", "\027[0;95m");
      ("sentry", "\027[0;95m");
      ("seal range", "\027[0;96m");
      ("sealed capability", "\027[0;35m");
      ("sealed seal range", "\027[0;36m");
      ("fallback", "\027[0;38;5;246m");
    ];
  Alcotest.(check bool) "sealed capability wrapper is gray" true
    (contains (output "sealed capability") "\027[0;38;5;246m");
  Alcotest.(check bool) "sealed seal-range wrapper is gray" true
    (contains (output "sealed seal range") "\027[0;38;5;246m");
  let session = create ~source:"fail" "vanilla" in
  let running = Interactive_ui.ansi_snapshot ~width:120 ~height:12 (Interactive_ui.create session) in
  List.iter
    (fun (label, sgr) ->
      Alcotest.(check bool) label true (contains running sgr))
    [
      ("primary bounds/cursor are red", "\027[0;31m");
      ("secondary bounds/cursor are light magenta", "\027[0;95m");
      ("addresses are yellow", "\027[0;33m");
      ("instructions are green", "\027[0;32m");
    ];
  let failed =
    match Interactive_ui.transition ~rows:5 Interactive_ui.Step (Interactive_ui.create session) with
    | Some state -> Interactive_ui.ansi_snapshot ~width:120 ~height:12 state
    | None -> Alcotest.fail "step quit"
  in
  Alcotest.(check bool) "failed state is bold red" true
    (contains failed "machine state:"
     && (contains failed "\027[0;1;31m" || contains failed "\027[0;31;1m"));
  let halted_session = create ~source:"halt" "vanilla" in
  let halted =
    match Interactive_ui.transition ~rows:5 Interactive_ui.Step
            (Interactive_ui.create halted_session) with
    | Some state -> Interactive_ui.ansi_snapshot ~width:120 ~height:12 state
    | None -> Alcotest.fail "halt step quit"
  in
  Alcotest.(check bool) "halted state is bold" true (contains halted "\027[0;1m")

let next ~rows:(rows : int) (event : Interactive_ui.event) (state : Interactive_ui.t) : Interactive_ui.t =
  match Interactive_ui.transition ~rows event state with
  | Some state -> state
  | None -> Alcotest.fail "unexpected quit"

let navigation_transitions (() : unit) : unit =
  let register (bank : Machine_view.register_bank) (key : string) : Machine_view.register_id = { Machine_view.Register_id.bank; key } in
  let stepping =
    create ~source:"mov r1 7\nhalt"
      ~regfile:"stk := (RWLX, LOCAL, 8, 56, 20)" "locality-cerise"
    |> Interactive_ui.create
    |> next ~rows:5 (Interactive_ui.Move_primary (Z.of_int 10))
    |> next ~rows:5 (Interactive_ui.Move_secondary (Z.of_int (-18)))
    |> next ~rows:5 Interactive_ui.Step
  in
  check_z "step follows restored PC context" Z.zero
    (Application_model.primary_start (Interactive_ui.application stepping));
  check_z "step follows secondary context" (Z.of_int 18)
    (Application_model.secondary_start (Interactive_ui.application stepping));
  Alcotest.(check int) "step adds one history entry" 1
    (Application_model.history_length (Interactive_ui.application stepping));
  let ten = next ~rows:5 Interactive_ui.Step_ten stepping in
  Alcotest.(check int) "ten-step command adds one history entry" 2
    (Application_model.history_length (Interactive_ui.application ten));
  let before_undo =
    next ~rows:5 (Interactive_ui.Move_secondary (Z.of_int 5)) ten
  in
  let secondary_before =
    Application_model.secondary_start (Interactive_ui.application before_undo)
  in
  let undone = next ~rows:5 Interactive_ui.Undo before_undo in
  check_z "undo retains secondary start" secondary_before
    (Application_model.secondary_start (Interactive_ui.application undone));
  Alcotest.(check int) "undo removes one command history entry" 1
    (Application_model.history_length (Interactive_ui.application undone));
  let vanilla =
    create "vanilla"
    |> Machine_session.set_register_text (register System "pc") "(RWX, 0, 64, 20)"
    |> get_ok
    |> Machine_session.set_register_text (register System "ddc") "(RW, 0, 64, 30)"
    |> get_ok
  in
  let state = Interactive_ui.create vanilla in
  Alcotest.(check bool) "one-row initial follow keeps cursor visible" true
    (contains (Interactive_ui.snapshot ~width:48 ~height:3 state) "▶ 14");
  let state = next ~rows:5 (Interactive_ui.Move_primary (Z.of_int (-18))) state in
  check_z "row movement" Z.zero
    (Application_model.primary_start (Interactive_ui.application state));
  let state = next ~rows:5 Interactive_ui.Follow_primary state in
  check_z "follow gives two rows of context" (Z.of_int 18)
    (Application_model.primary_start (Interactive_ui.application state));
  let state = next ~rows:8 (Interactive_ui.Move_primary (Z.of_int (-18))) state in
  let state = next ~rows:8 (Interactive_ui.Page_primary 1) state in
  check_z "page retains two-row overlap" (Z.of_int 6)
    (Application_model.primary_start (Interactive_ui.application state));
  let state = next ~rows:8 (Interactive_ui.Page_primary (-1)) state in
  check_z "previous page overlap" Z.zero
    (Application_model.primary_start (Interactive_ui.application state));
  let cycled = next ~rows:5 Interactive_ui.Cycle_capability state in
  check_z "fallback cycling follows selected authority" (Z.of_int 28)
    (Application_model.secondary_start (Interactive_ui.application cycled));
  let hidden = next ~rows:5 Interactive_ui.Toggle_secondary cycled in
  let moved = next ~rows:5 (Interactive_ui.Move_secondary (Z.of_int (-28))) hidden in
  let shown = next ~rows:5 Interactive_ui.Toggle_secondary moved in
  check_z "re-enabling follows secondary" (Z.of_int 28)
    (Application_model.secondary_start (Interactive_ui.application shown));
  (match Interactive_ui.scroll_event ~width:120 ~height:12 ~x:119 ~ctrl:true
           ~direction:`Down shown with
  | Page_secondary 1 -> ()
  | _ -> Alcotest.fail "wide right-side mouse scroll did not target secondary");
  (match Interactive_ui.scroll_event ~width:48 ~height:12 ~x:47 ~ctrl:false
           ~direction:`Down shown with
  | Move_primary delta when Z.equal delta Z.one -> ()
  | _ -> Alcotest.fail "narrow mouse scroll did not target primary");
  let resized = next ~rows:5 (Interactive_ui.Resize (1, 1)) shown in
  Alcotest.(check int) "resize preserves history" 0
    (Application_model.history_length (Interactive_ui.application resized));
  let boundary = next ~rows:5 (Interactive_ui.Move_primary (Z.of_int 999)) resized in
  check_z "terminal upper boundary" (Z.of_int 63)
    (Application_model.primary_start (Interactive_ui.application boundary));
  let boundary = next ~rows:5 (Interactive_ui.Move_primary (Z.of_int (-999))) boundary in
  check_z "terminal lower boundary" Z.zero
    (Application_model.primary_start (Interactive_ui.application boundary))

let all_backend_dimensions (() : unit) : unit =
  List.iter
    (fun backend ->
      let state = Interactive_ui.create (create backend) in
      List.iter
        (fun (width, height) ->
          let image = Interactive_ui.render ~width ~height state in
          Alcotest.(check int) (backend ^ " width") width (Notty.I.width image);
          Alcotest.(check int) (backend ^ " height") height (Notty.I.height image))
        [ (120, 24); (48, 12); (1, 1); (0, 0) ])
    (Backend_registry.available_backend_names ())

let read_golden (name : string) : string =
  let relative = "goldens/" ^ name ^ ".txt" in
  let path = if Sys.file_exists relative then relative else "tests/" ^ relative in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () ->
      let contents = really_input_string channel (in_channel_length channel) in
      let length = String.length contents in
      if length > 0 && contents.[length - 1] = '\n'
      then String.sub contents 0 (length - 1)
      else contents)

let trim_line_end (line : string) : string =
  let rec find (index : int) : int =
    if index < 0 || line.[index] <> ' ' then index + 1 else find (index - 1)
  in
  String.sub line 0 (find (String.length line - 1))

let normalize_frame (text : string) : string =
  String.split_on_char '\n' text |> List.map trim_line_end |> String.concat "\n"

let cerisier_enclave_session (() : unit) : Machine_session.t =
  let relative = "test_files/cerisier/pos/enclave.s" in
  let path = if Sys.file_exists relative then relative else "tests/" ^ relative in
  let source = In_channel.with_open_bin path In_channel.input_all in
  let rec advance remaining session =
    let populated =
      match (Machine_session.view session).Machine_view.enclave_table with
      | Some { entries = _ :: _; _ } -> true
      | _ -> false
    in
    if populated then session
    else if remaining = 0 then Alcotest.fail "Cerisier example did not initialize an enclave"
    else
      match Machine_session.step session with
      | Ok session -> advance (remaining - 1) session
      | Error _ -> Alcotest.fail "Cerisier example stopped before EInit"
  in
  advance 100 (create ~source "cerisier")

let enclave_layout_and_truncation (() : unit) : unit =
  let snapshot =
    Interactive_ui.snapshot ~width:120 ~height:19
      (Interactive_ui.create (cerisier_enclave_session ()))
    |> normalize_frame
  in
  let lines = String.split_on_char '\n' snapshot in
  Alcotest.(check int) "fixed constrained height" 19 (List.length lines);
  Alcotest.(check bool) "enclave entry remains visible" true
    (contains snapshot "0   ");
  Alcotest.(check string) "footer occupies final row" "backend: cerisier"
    (List.nth lines 18)

let full_frame_goldens (() : unit) : unit =
  let session ?regfile:(regfile : string option) (backend : string) (source : string) : Machine_session.t = create ?regfile ~source backend in
  let fixtures =
    [
      ( "locality-120x24",
        120,
        24,
        session ~regfile:"stk := (RWLX, LOCAL, 16, 48, 24)"
          "locality-cerise" "mov r1 7\nhalt" );
      ( "vanilla-120x24",
        120,
        24,
        session ~regfile:"ddc := (RW, 8, 48, 24)"
          "vanilla" "mov r1 7\nhalt" );
      ( "griotte-160x30",
        160,
        30,
        session
          ~regfile:
            "pc := ([X Ow LG LM], Global, 0, 64, 0) csp := ([R WL LG LM], Local, 8, 56, 24) mtdc := ([R W DL DRO], Global, 0, 64, 0)"
          "griotte" "mov ca0 7\nhalt" );
      ( "adaptive-48x12",
        48,
        12,
        session ~regfile:"stk := (RWLX, LOCAL, 16, 48, 24)"
          "locality-cerise" "mov r1 7\nhalt" );
      ("cerisier-empty-120x30", 120, 30, session "cerisier" "halt");
      ("cerisier-populated-120x30", 120, 30, cerisier_enclave_session ());
    ]
  in
  List.iter
    (fun (name, width, height, session) ->
      let actual =
        Interactive_ui.snapshot ~width ~height (Interactive_ui.create session)
      in
      Alcotest.(check string) name (read_golden name) (normalize_frame actual))
    fixtures

let () =
  Alcotest.run "tui-fidelity"
    [
      ("metadata", [ Alcotest.test_case "typed backend contracts" `Quick metadata_contracts ]);
      ("components", [ Alcotest.test_case "semantic text goldens" `Quick component_goldens ]);
      ("ansi", [ Alcotest.test_case "legacy style palette" `Quick ansi_styles ]);
      ("navigation", [ Alcotest.test_case "context and panel transitions" `Quick navigation_transitions ]);
      ("dimensions", [ Alcotest.test_case "all backends and terminal sizes" `Quick all_backend_dimensions ]);
      ("enclaves", [ Alcotest.test_case "layout and truncation" `Quick enclave_layout_and_truncation ]);
      ("frames", [ Alcotest.test_case "complete text goldens" `Quick full_frame_goldens ]);
    ]
