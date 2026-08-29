open Cerise

let ok = function
  | Ok x -> x
  | Error diagnostics ->
      Alcotest.fail (String.concat "; " (List.map Diagnostic.to_string diagnostics))

let z n = Z.of_int n

let check_z message expected actual =
  Alcotest.(check string) message (Z.to_string expected) (Z.to_string actual)

let expected_allocations =
  [
    ("Jmp", 0x00, 2);
    ("Jnz", 0x02, 2);
    ("Jalr", 0x04, 1);
    ("ReadSR", 0x05, 1);
    ("WriteSR", 0x06, 1);
    ("Move", 0x07, 2);
    ("Load", 0x09, 1);
    ("Store", 0x0a, 2);
    ("Add", 0x0c, 4);
    ("Sub", 0x10, 4);
    ("Mul", 0x14, 4);
    ("Lt", 0x20, 4);
    ("Lea", 0x24, 2);
    ("Restrict", 0x26, 2);
    ("SubSeg", 0x28, 4);
    ("GetL", 0x2c, 1);
    ("GetB", 0x2d, 1);
    ("GetE", 0x2e, 1);
    ("GetA", 0x2f, 1);
    ("GetP", 0x30, 1);
    ("GetOType", 0x31, 1);
    ("GetWType", 0x32, 1);
    ("Seal", 0x33, 1);
    ("UnSeal", 0x34, 1);
    ("Fail", 0x35, 1);
    ("Halt", 0x36, 1);
    ("LAnd", 0x37, 4);
    ("LOr", 0x3b, 4);
    ("LShiftL", 0x3f, 4);
    ("LShiftR", 0x43, 4);
  ]

let instructions =
  let open Griotte.Ast in
  let a = Reg 1 and b = Reg 2 and rr = Register (Reg 3) and c = Constant (z (-7)) in
  [
    Jmp rr;
    Jmp c;
    Jnz (a, rr);
    Jnz (a, c);
    Jalr (a, b);
    ReadSR (a, MTDC);
    WriteSR (MTDC, a);
    Move (a, rr);
    Move (a, c);
    Load (a, b);
    Store (a, rr);
    Store (a, c);
    Add (a, rr, rr);
    Add (a, rr, c);
    Add (a, c, rr);
    Add (a, c, c);
    Sub (a, rr, rr);
    Sub (a, rr, c);
    Sub (a, c, rr);
    Sub (a, c, c);
    Mul (a, rr, rr);
    Mul (a, rr, c);
    Mul (a, c, rr);
    Mul (a, c, c);
    Lt (a, rr, rr);
    Lt (a, rr, c);
    Lt (a, c, rr);
    Lt (a, c, c);
    Lea (a, rr);
    Lea (a, c);
    Restrict (a, rr);
    Restrict (a, c);
    SubSeg (a, rr, rr);
    SubSeg (a, rr, c);
    SubSeg (a, c, rr);
    SubSeg (a, c, c);
    GetL (a, b);
    GetB (a, b);
    GetE (a, b);
    GetA (a, b);
    GetP (a, b);
    GetOType (a, b);
    GetWType (a, b);
    Seal (a, b, PC);
    UnSeal (a, b, PC);
    Fail;
    Halt;
    LAnd (a, rr, rr);
    LAnd (a, rr, c);
    LAnd (a, c, rr);
    LAnd (a, c, c);
    LOr (a, rr, rr);
    LOr (a, rr, c);
    LOr (a, c, rr);
    LOr (a, c, c);
    LShiftL (a, rr, rr);
    LShiftL (a, rr, c);
    LShiftL (a, c, rr);
    LShiftL (a, c, c);
    LShiftR (a, rr, rr);
    LShiftR (a, rr, c);
    LShiftR (a, c, rr);
    LShiftR (a, c, c);
  ]

let codec () =
  Alcotest.(check (list (triple string int int)))
    "historical fixed allocation" expected_allocations Griotte.Codec.allocations;
  List.iter
    (fun instruction ->
      let encoded = Result.get_ok (Griotte.Codec.encode instruction) in
      Alcotest.(check bool)
        "round trip" true
        (Result.get_ok (Griotte.Codec.decode encoded) = instruction))
    instructions;
  let open Griotte.Ast in
  check_z "negative immediate is historical signed high payload" (z (-767))
    (Result.get_ok (Griotte.Codec.encode (Jmp (Constant (z (-3))))));
  check_z "Jalr golden signed pair" (z 14340)
    (Result.get_ok (Griotte.Codec.encode (Jalr (Reg 1, Reg 2))));
  check_z "Move reg/negative constant golden" (z 47624)
    (Result.get_ok (Griotte.Codec.encode (Move (Reg 1, Constant (z (-7))))));
  List.iter
    (fun encoded ->
      Alcotest.(check bool)
        "malformed/unknown rejected" true
        (Result.is_error (Griotte.Codec.decode encoded)))
    [
      Z.minus_one;
      z 0xfe;
      Z.logor (z 0x36) (Z.shift_left Z.one 8);
      Z.logor Z.zero (Z.shift_left (z 99) 8);
    ];
  List.iter
    (fun opcode ->
      Alcotest.(check bool)
        (Printf.sprintf "unallocated opcode 0x%02x rejected" opcode)
        true
        (Result.is_error (Griotte.Codec.decode (z opcode))))
    (List.init 8 (fun offset -> 0x18 + offset));
  let p = (XSR, WL, LG, LM) in
  Alcotest.(check bool)
    "permission round trip" true
    (Result.get_ok (Griotte.Codec.decode_permission (Griotte.Codec.encode_permission p)) = p);
  Alcotest.(check bool)
    "permission/locality round trip" true
    (Result.get_ok
       (Griotte.Codec.decode_permission_locality (Griotte.Codec.encode_permission_locality p Local))
    = (p, Local));
  List.iter
    (fun wt ->
      Alcotest.(check bool)
        "word type round trip" true
        (Result.get_ok (Griotte.Codec.decode_word_type (Griotte.Codec.encode_word_type wt)) = wt))
    [ W_I; W_Cap; W_SealRange; W_Sealed; W_Sentry ];
  let tagged tag payload = Z.logor (z tag) (Z.shift_left payload 3) in
  let rejects_without_exception : type value.
      string -> (Z.t -> (value, string) result) -> Z.t -> unit =
   fun name decoder encoded ->
    Alcotest.(check bool)
      name true
      (match decoder encoded with Error _ -> true | Ok _ -> false | exception _ -> false)
  in
  let huge = Z.shift_left Z.one 10_000 in
  rejects_without_exception "oversized permission" Griotte.Codec.decode_permission (tagged 0 huge);
  rejects_without_exception "high-bit permission" Griotte.Codec.decode_permission (tagged 0 (z 64));
  rejects_without_exception "oversized seal permission" Griotte.Codec.decode_seal_permission
    (tagged 1 huge);
  rejects_without_exception "oversized locality" Griotte.Codec.decode_locality (tagged 2 huge);
  rejects_without_exception "oversized word type" Griotte.Codec.decode_word_type (tagged 3 huge);
  rejects_without_exception "oversized permission/locality" Griotte.Codec.decode_permission_locality
    (tagged 4 huge);
  rejects_without_exception "high-bit permission/locality" Griotte.Codec.decode_permission_locality
    (tagged 4 (z 128));
  rejects_without_exception "oversized seal permission/locality"
    Griotte.Codec.decode_seal_permission_locality (tagged 5 huge);
  rejects_without_exception "high-bit seal permission/locality"
    Griotte.Codec.decode_seal_permission_locality
    (tagged 5 (z 8))

let parser () =
  let source =
    "jalr cra csp jmp -2 jnz ca0 2 readsR ca1 MTDC writeSR mtdc ca1 mov ct0 cnull load ct1 cgp \
     store cgp ct1 add ca0 ca1 1 sub ca0 3 ca1 mul ca0 ca1 2 land ca0 7 3 \
     lor ca0 4 1 lshiftl ca0 1 3 lshiftr ca0 8 2 lt ca0 1 2 lea cgp -1 restrict cgp ([R WL LG LM], \
     Global) subseg cgp 0 8 getl ca0 cgp getb ca0 cgp gete ca0 cgp geta ca0 cgp getp ca0 cgp \
     getotype ca0 cgp getwtype ca0 cgp seal ca0 ca3 cgp unseal ca1 ca3 ca0 fail halt"
  in
  ignore (ok (Griotte.Parser.parse_program source));
  let aliases =
    [
      "pc";
      "cnull";
      "cra";
      "csp";
      "cgp";
      "ctp";
      "ct0";
      "ct1";
      "ct2";
      "ct3";
      "ct4";
      "ct5";
      "ct6";
      "cs0";
      "cs1";
      "cs2";
      "cs3";
      "cs4";
      "cs5";
      "cs6";
      "cs7";
      "cs8";
      "cs9";
      "cs10";
      "cs11";
      "ca0";
      "ca1";
      "ca2";
      "ca3";
      "ca4";
      "ca5";
      "ca6";
      "ca7";
    ]
  in
  ignore
    (ok
       (Griotte.Parser.parse_program
          (String.concat " " (List.map (fun r -> "mov " ^ r ^ " 0") aliases))));
  List.iter
    (fun word -> ignore (ok (Griotte.Parser.parse_word word)))
    [
      "9";
      "([XSR Ow LG LM], Global, 0, MAX_ADDR, 0)";
      "[SU, Local, 0, 15, 3]";
      "(E-[X Ow LG LM], Global, 0, 8, 2)";
      "{3: ([R W DL DRO], Local, 0, 8, 1)}";
      "{3: [S, Global, 0, 15, 3]}";
    ];
  List.iter
    (fun value -> ignore (ok (Griotte.Parser.parse_program ("mov ca0 " ^ value ^ " halt"))))
    [
      "O";
      "[R WL LG LM]";
      "SO";
      "S";
      "U";
      "SU";
      "Local";
      "Global";
      "Int";
      "Cap";
      "SealRange";
      "Sealed";
      "Sentry";
      "([R WL LG LM], Local)";
      "(SU, Global)";
    ];
  ignore
    (ok
       (Griotte.Parser.parse_regfile
          "PC := ([XSR Ow LG LM], Global, 0, MAX_ADDR, 0) CSP := ([R WL LG LM], Local, 8, \
           MAX_ADDR, 8) MTDC := 0"));
  ignore
    (ok
       (Griotte.Parser.parse_program
          "%define N 3 start: mov ca0 &CURRENT_ADDR add ca1 ca0 N # (((start + N) << 3) || 1) halt"));
  ignore
    (ok
       (Griotte.Parser.parse_program
          "%macro typed(dst: reg, e: expr, v: value, p: perm, sp: sealperm, l: locality, wt: \
           wtype) mov $dst $v restrict cgp ($p, $l) # ($p, $l, 0, $e, 0) # [$sp, $l, 0, 15, 0] mov \
           ca1 $wt %endmacro %typed(ca0, 8, -2, [R WL LG LM], SU, Local, Sentry) halt"));
  List.iter
    (fun source ->
      Alcotest.(check bool)
        ("reject " ^ source) true
        (Result.is_error (Griotte.Parser.parse_program source)))
    [
      "invoke ca0 ca1";
      "loadu ca0 ca1 0";
      "promoteu ca0";
      "mov ca0 RWX";
      "mov ca0 Directed";
      "readsr ca0 ddc";
      "mov r32 0";
      "isptr ca0 ca1";
    ];
  Alcotest.(check bool)
    "reject vanilla word" true
    (Result.is_error (Griotte.Parser.parse_word "(RW, 0, 8, 0)"));
  Alcotest.(check bool)
    "reject malformed permission" true
    (Result.is_error (Griotte.Parser.parse_word "([R BAD LG LM], Global, 0, 8, 0)"))

let config = Runtime_config.create ~max_addr:(z 128) ~stack_addr:(z 64) ()

let session ?regfile source =
  ok (Machine_session.create ~backend:"griotte" ~config ~source ~regfile)

let architectural_pc = "pc := ([XSR Ow LG LM], Global, 0, MAX_ADDR, 0) "
let executable_session ?(regfile = "") source = session ~regfile:(architectural_pc ^ regfile) source
let run s = (Machine_session.run ~max_steps:1000 s).session

let find bank key session =
  Option.get
    (Machine_view.find_register
       { Machine_view.Register_id.bank; key }
       (Machine_session.view session))

let int_reg key session = Option.get (find Machine_view.General key session).word.integer
let cap_reg key session = Option.get (find Machine_view.General key session).word.capability
let status session = (Machine_session.view session).status

let initialization_and_program_validation () =
  let defaults = session "halt" in
  Alcotest.(check bool)
    "no regfile installs cgp architectural root" true
    ((find General "cgp" defaults).word.kind = Capability);
  Alcotest.(check bool)
    "no regfile installs ca3 sealing root" true
    ((find General "ca3" defaults).word.kind = Seal_range);
  let partial = session ~regfile:"cra := 7" "halt" in
  check_z "explicit regfile starts omitted cgp at zero" Z.zero (int_reg "cgp" partial);
  check_z "explicit regfile starts omitted ca3 at zero" Z.zero (int_reg "ca3" partial);
  check_z "explicit regfile starts omitted pc at zero" Z.zero
    (Option.get (find System "pc" partial).word.integer);
  check_z "explicit regfile applies supplied entry" (z 7) (int_reg "cra" partial);
  check_z "explicit regfile starts MTDC at zero" Z.zero
    (Option.get (find System "mtdc" partial).word.integer);
  let create source = Machine_session.create ~backend:"griotte" ~config ~source ~regfile:None in
  List.iter
    (fun (name, source) -> Alcotest.(check bool) name true (Result.is_ok (create source)))
    [
      ("derived capability program word", "# ([R W LG LM], Global, 0, 8, 0)");
      ("derived sentry program word", "# (E-[X Ow LG LM], Global, 0, 8, 0)");
      ("derived sealed capability program word", "# {0: ([R W LG LM], Global, 0, 8, 0)}");
      ("integer program word", "# 9");
      ("seal-range program word", "# [SU, Global, 0, 15, 0]");
      ("null-permission program word", "# (O, Global, 0, 8, 0)");
    ];
  List.iter
    (fun (name, source) ->
      Alcotest.(check bool)
        name true
        (match create source with
        | Error (_ :: _) -> true
        | Error [] | Ok _ -> false
        | exception _ -> false))
    [
      ("reject underived capability program word", "# ([X W LG LM], Global, 0, 8, 0)");
      ("reject underived sentry program word", "# (E-[X W LG LM], Global, 0, 8, 0)");
      ("reject underived sealed capability program word", "# {0: ([X W LG LM], Global, 0, 8, 0)}");
    ]

let arithmetic_and_control () =
  let s =
    session
      "mov ca0 20 mov ca1 6 add ca2 ca0 ca1 sub ca3 ca0 ca1 mul ca4 ca1 3 land ca7 7 3 lor cs0 4 \
       1 lshiftl cs1 1 4 lshiftr cs2 16 2 lt cs3 ca1 ca0 halt"
    |> run
  in
  List.iter
    (fun (key, value) -> check_z key (z value) (int_reg key s))
    [
      ("ca2", 26);
      ("ca3", 14);
      ("ca4", 18);
      ("ca7", 3);
      ("cs0", 5);
      ("cs1", 16);
      ("cs2", 4);
      ("cs3", 1);
    ];
  Alcotest.(check bool) "halt" true (status s = Halted);
  let jumped = session "jmp 2 fail halt" |> run in
  Alcotest.(check bool) "jmp" true (status jumped = Halted);
  let jnz = session "mov ca0 1 jnz ca0 2 fail halt" |> run in
  Alcotest.(check bool) "jnz" true (status jnz = Halted);
  let zero = session "mov ca0 0 jnz ca0 2 halt" |> run in
  Alcotest.(check bool) "jnz zero" true (status zero = Halted);
  let jalr =
    executable_session ~regfile:"ca0 := (E-[X Ow LG LM], Global, 0, 4, 2)" "jalr cra ca0 fail halt"
    |> run
  in
  Alcotest.(check bool) "sentry entry" true (status jalr = Halted);
  Alcotest.(check bool) "jalr link is sentry" true ((find General "cra" jalr).word.kind = Sentry)

let memory_permissions_and_system () =
  let stored =
    executable_session ~regfile:"ca0 := ([R WL LG LM], Global, 20, 21, 20)"
      "store ca0 42 load ca1 ca0 halt"
    |> run
  in
  check_z "store/load" (z 42) (int_reg "ca1" stored);
  let no_local =
    executable_session
      ~regfile:"ca0 := ([R W LG LM], Global, 20, 21, 20) ca1 := ([R WL LG LM], Local, 0, 8, 0)"
      "store ca0 ca1 halt"
    |> run
  in
  Alcotest.(check bool) "W rejects Local" true (status no_local = Failed);
  let deep =
    executable_session ~regfile:"ca0 := ([R WL DL LM], Global, 2, 3, 2)"
      "load ca1 ca0 halt # ([R WL LG LM], Global, 0, 8, 0)"
    |> run
  in
  Alcotest.(check (option string)) "DL load localizes" (Some "Local") (cap_reg "ca1" deep).locality;
  Alcotest.(check string) "DL component" "DL" (List.nth (cap_reg "ca1" deep).permissions 2);
  let dro =
    executable_session ~regfile:"ca0 := ([R WL LG DRO], Global, 2, 3, 2)"
      "load ca1 ca0 halt # ([R WL LG LM], Global, 0, 8, 0)"
    |> run
  in
  Alcotest.(check (list string))
    "DRO load read-only" [ "R"; "Ow"; "LG"; "DRO" ] (cap_reg "ca1" dro).permissions;
  let sr = session "mov ca0 77 writesr mtdc ca0 readsR ca1 mtdc mov cnull 99 halt" |> run in
  check_z "MTDC transition" (z 77) (int_reg "ca1" sr);
  check_z "cnull immutable" Z.zero (int_reg "cnull" sr);
  let no_sr =
    session ~regfile:"pc := ([X Ow LG LM], Global, 0, 2, 0)" "readsr ca0 mtdc halt" |> run
  in
  Alcotest.(check bool) "X lacks system access" true (status no_sr = Failed)

let capabilities_sealing_and_view () =
  let restricted =
    executable_session ~regfile:"ca0 := ([R WL LG LM], Global, 1, 20, 4)"
      "restrict ca0 ([R W DL DRO], Local) subseg ca0 2 10 lea ca0 1 getl ca1 ca0 getb ca2 ca0 gete \
       ca4 ca0 geta ca5 ca0 getp ca6 ca0 getwtype ca7 ca0 getotype cs0 ca0 halt"
    |> run
  in
  let cap = cap_reg "ca0" restricted in
  check_z "base" (z 2) cap.base;
  check_z "limit" (z 10) cap.limit;
  check_z "cursor" (z 5) cap.cursor;
  Alcotest.(check (option string)) "restricted locality" (Some "Local") cap.locality;
  check_z "getb" (z 2) (int_reg "ca2" restricted);
  check_z "gete" (z 10) (int_reg "ca4" restricted);
  check_z "geta" (z 5) (int_reg "ca5" restricted);
  check_z "unsealed otype" Z.minus_one (int_reg "cs0" restricted);
  let sealed =
    executable_session
      ~regfile:"ca0 := ([R WL LG LM], Global, 0, 8, 2) ca3 := [SU, Global, 0, 15, 0]"
      "seal ca1 ca3 ca0 getotype ca2 ca1 getwtype ca4 ca1 unseal ca5 ca3 ca1 halt"
    |> run
  in
  Alcotest.(check bool)
    "sealed metadata" true
    ((find General "ca1" sealed).word.kind = Sealed_capability);
  check_z "otype" Z.zero (int_reg "ca2" sealed);
  Alcotest.(check bool) "unseal" true ((find General "ca5" sealed).word.kind = Capability);
  let initial = session "halt" in
  let view = Machine_session.view initial in
  Alcotest.(check string) "selected backend" "griotte" view.backend_name;
  Alcotest.(check bool)
    "MTDC in view" true
    (Option.is_some (Machine_view.find_register { bank = System; key = "mtdc" } view));
  Alcotest.(check bool) "sparse memory" true (List.length view.memory = 1);
  let edited = ok (Machine_session.set_register_text { bank = General; key = "r1" } "31" initial) in
  check_z "numeric alias edit" (z 31) (int_reg "cra" edited);
  check_z "immutable session" Z.zero (int_reg "cra" initial);
  Alcotest.(check (list string))
    "registry order"
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
    (Backend_registry.names ());
  Alcotest.(check bool)
    "extracted registered" true
    (Result.is_ok
       (Machine_session.create ~backend:"griotte-extracted" ~config ~source:"halt" ~regfile:None))

let resolve_file path = if Sys.file_exists path then path else "../../../" ^ path

let read_file path =
  let path = resolve_file path in
  let channel = open_in path in
  let length = in_channel_length channel in
  let source = really_input_string channel length in
  close_in channel;
  source

let examples () =
  let base = "tests/test_files/griotte/" in
  let program_files =
    [
      "case_studies/counter.s";
      "case_studies/deep_immutability.s";
      "case_studies/deep_locality.s";
      "case_studies/kvs.s";
      "case_studies/lse.s";
      "case_studies/mutually_distrustful.s";
      "case_studies/stack_object.s";
      "case_studies/vae.s";
      "switcher/switcher.s";
      "switcher/switcher_commented.s";
      "switcher/switcher_example.s";
    ]
  in
  let regfile_files =
    [
      "case_studies/counter.reg";
      "case_studies/deep_immutability.reg";
      "case_studies/deep_locality.reg";
      "case_studies/kvs.reg";
      "case_studies/lse.reg";
      "case_studies/mutually_distrustful.reg";
      "case_studies/stack_object.reg";
      "case_studies/vae.reg";
      "switcher/switcher.reg";
    ]
  in
  List.iter
    (fun relative ->
      let path = base ^ relative in
      match Griotte.Parser.parse_program ~filename:path (read_file path) with
      | Ok _ -> ()
      | Error diagnostics ->
          Alcotest.failf "Griotte program fixture %s failed: %s" path
            (String.concat "; " (List.map Diagnostic.to_string diagnostics)))
    program_files;
  List.iter
    (fun relative ->
      let path = base ^ relative in
      match Griotte.Parser.parse_regfile ~filename:path (read_file path) with
      | Ok _ -> ()
      | Error diagnostics ->
          Alcotest.failf "Griotte regfile fixture %s failed: %s" path
            (String.concat "; " (List.map Diagnostic.to_string diagnostics)))
    regfile_files;
  let example_config = Runtime_config.create ~max_addr:(z 0x20000) ~stack_addr:(z 0x10000) () in
  let execute program regfile limit =
    let source = read_file ("tests/test_files/griotte/" ^ program) in
    let regfile = Some (read_file ("tests/test_files/griotte/" ^ regfile)) in
    ok (Machine_session.create ~backend:"griotte" ~config:example_config ~source ~regfile)
    |> Machine_session.run ~max_steps:limit
  in
  let switcher = execute "switcher/switcher.s" "switcher/switcher.reg" 64 in
  Alcotest.(check bool) "switcher executes" true (switcher.steps > 0);
  let counter = execute "case_studies/counter.s" "case_studies/counter.reg" 20_000 in
  Alcotest.(check bool) "counter executes" true (counter.steps > 0);
  Alcotest.(check bool)
    "counter halts" true
    ((Machine_session.view counter.session).status = Machine_view.Halted)

let () =
  Alcotest.run "griotte"
    [
      ("codec", [ Alcotest.test_case "fixed historical codec" `Quick codec ]);
      ( "parser",
        [
          Alcotest.test_case "acceptance and rejection" `Quick parser;
          Alcotest.test_case "historical examples" `Quick examples;
        ] );
      ( "machine",
        [
          Alcotest.test_case "initialization and program validation" `Quick
            initialization_and_program_validation;
          Alcotest.test_case "arithmetic and control" `Quick arithmetic_and_control;
          Alcotest.test_case "memory permissions and system" `Quick memory_permissions_and_system;
          Alcotest.test_case "capabilities sealing and view" `Quick capabilities_sealing_and_view;
        ] );
    ]
