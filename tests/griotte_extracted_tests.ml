open Cerise

let ok = function
  | Ok value -> value
  | Error diagnostics ->
      Alcotest.fail (String.concat "; " (List.map Diagnostic.to_string diagnostics))

let z = Z.of_int
let config = Runtime_config.create ~max_addr:(z 128) ~stack_addr:(z 64) ()

let instructions =
  let open Griotte_ast in
  let a = Reg 1 and b = Reg 2 and r = Register (Reg 3) and c = Constant (z (-7)) in
  [
    Jmp r;
    Jmp c;
    Jnz (a, r);
    Jnz (a, c);
    Jalr (a, b);
    ReadSR (a, MTDC);
    WriteSR (MTDC, a);
    Move (a, r);
    Move (a, c);
    Load (a, b);
    Store (a, r);
    Store (a, c);
    Add (a, r, r);
    Add (a, r, c);
    Add (a, c, r);
    Add (a, c, c);
    Sub (a, r, r);
    Sub (a, r, c);
    Sub (a, c, r);
    Sub (a, c, c);
    Mul (a, r, r);
    Mul (a, r, c);
    Mul (a, c, r);
    Mul (a, c, c);
    Lt (a, r, r);
    Lt (a, r, c);
    Lt (a, c, r);
    Lt (a, c, c);
    Lea (a, r);
    Lea (a, c);
    Restrict (a, r);
    Restrict (a, c);
    SubSeg (a, r, r);
    SubSeg (a, r, c);
    SubSeg (a, c, r);
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
    LAnd (a, r, r);
    LAnd (a, r, c);
    LAnd (a, c, r);
    LAnd (a, c, c);
    LOr (a, r, r);
    LOr (a, r, c);
    LOr (a, c, r);
    LOr (a, c, c);
    LShiftL (a, r, r);
    LShiftL (a, r, c);
    LShiftL (a, c, r);
    LShiftL (a, c, c);
    LShiftR (a, r, r);
    LShiftR (a, r, c);
    LShiftR (a, c, r);
    LShiftR (a, c, c);
  ]

let encoding_identity () =
  List.iter
    (fun instruction ->
      let expected = Result.get_ok (Griotte_codec.encode instruction) in
      let actual = Result.get_ok (Griotte_extracted_codec.encode instruction) in
      Alcotest.(check string) "fixed numeric encoding" (Z.to_string expected) (Z.to_string actual);
      Alcotest.(check bool)
        "standalone round trip" true
        (Griotte_extracted_codec.decode actual = Ok instruction))
    instructions;
  let open Griotte_ast in
  let limitations =
    [
      Rem (Reg 1, Constant (z (-9)), Register (Reg 2)); Div (Reg 1, Register PC, Constant (z (-3)));
    ]
  in
  List.iter
    (fun instruction ->
      Alcotest.(check string)
        "unsupported constructor still has the canonical program encoding"
        (Z.to_string (Result.get_ok (Griotte_codec.encode instruction)))
        (Z.to_string (Result.get_ok (Griotte_extracted_codec.encode instruction))))
    limitations;
  Alcotest.(check string)
    "negative Jmp golden" "-767"
    (Z.to_string (Result.get_ok (Griotte_extracted_codec.encode (Jmp (Constant (z (-3)))))));
  Alcotest.(check string)
    "Jalr golden" "14340"
    (Z.to_string (Result.get_ok (Griotte_extracted_codec.encode (Jalr (Reg 1, Reg 2)))));
  Alcotest.(check string)
    "Move negative constant golden" "47624"
    (Z.to_string (Result.get_ok (Griotte_extracted_codec.encode (Move (Reg 1, Constant (z (-7)))))));
  let permissions =
    List.concat_map
      (fun rx ->
        List.concat_map
          (fun w ->
            List.concat_map
              (fun dl -> List.map (fun dro -> (rx, w, dl, dro)) [ DRO; LM ])
              [ DL; LG ])
          [ Ow; W; WL ])
      [ Orx; R; X; XSR ]
  in
  List.iter
    (fun permission ->
      Alcotest.(check string)
        "permission encoding identity"
        (Z.to_string (Griotte_codec.encode_permission permission))
        (Z.to_string (Griotte_extracted_codec.encode_permission permission));
      List.iter
        (fun locality ->
          Alcotest.(check string)
            "permission/locality encoding identity"
            (Z.to_string (Griotte_codec.encode_permission_locality permission locality))
            (Z.to_string (Griotte_extracted_codec.encode_permission_locality permission locality)))
        [ Local; Global ])
    permissions;
  List.iter
    (fun seal_permission ->
      Alcotest.(check string)
        "seal-permission encoding identity"
        (Z.to_string (Griotte_codec.encode_seal_permission seal_permission))
        (Z.to_string (Griotte_extracted_codec.encode_seal_permission seal_permission));
      List.iter
        (fun locality ->
          Alcotest.(check string)
            "seal-permission/locality encoding identity"
            (Z.to_string (Griotte_codec.encode_seal_permission_locality seal_permission locality))
            (Z.to_string
               (Griotte_extracted_codec.encode_seal_permission_locality seal_permission locality)))
        [ Local; Global ])
    [ (false, false); (false, true); (true, false); (true, true) ];
  List.iter
    (fun word_type ->
      Alcotest.(check string)
        "word-type encoding identity"
        (Z.to_string (Griotte_codec.encode_word_type word_type))
        (Z.to_string (Griotte_extracted_codec.encode_word_type word_type)))
    [ W_I; W_Cap; W_SealRange; W_Sealed; W_Sentry ]

let decoder_totality () =
  let rejects value =
    match Griotte_extracted_codec.decode value with
    | Error _ -> true
    | Ok _ -> false
    | exception _ -> false
  in
  Alcotest.(check bool) "negative malformed encoding" true (rejects Z.minus_one);
  (* Opcode Add, outer tuple (r1,-1): the inner pair is negative. *)
  let nested_negative = Z.logor (z 0x0c) (Z.shift_left (z 14) 8) in
  Alcotest.(check bool) "nested negative tuple" true (rejects nested_negative);
  let huge = Z.shift_left Z.one 20_000 in
  Alcotest.(check bool) "huge malformed register" true (rejects (Z.shift_left huge 8))

let normalize view = { view with Machine_view.backend_name = "griotte" }

let compare_view label handwritten extracted =
  Alcotest.(check bool)
    label true
    (normalize (Machine_session.view handwritten) = normalize (Machine_session.view extracted))

let sessions ?regfile source =
  ( ok (Machine_session.create ~backend:"griotte" ~config ~source ~regfile),
    ok (Machine_session.create ~backend:"griotte-extracted" ~config ~source ~regfile) )

let differential ?regfile label source =
  let rec loop step_number handwritten extracted =
    compare_view
      (Printf.sprintf "%s after %d instruction steps" label step_number)
      handwritten extracted;
    match (Machine_session.view handwritten).status with
    | Machine_view.Halted | Failed -> ()
    | Running when step_number >= 100 -> Alcotest.fail (label ^ " did not stop")
    | Running ->
        let handwritten = Result.get_ok (Machine_session.step handwritten) in
        let extracted = Result.get_ok (Machine_session.step extracted) in
        loop (step_number + 1) handwritten extracted
  in
  let handwritten, extracted = sessions ?regfile source in
  loop 0 handwritten extracted

let architectural_pc = "pc := ([XSR Ow LG LM], Global, 0, MAX_ADDR, 0) "

let arithmetic_logic_control () =
  differential "arithmetic/logic/control"
    "mov ca0 20 mov ca1 6 add ca2 ca0 ca1 sub ca3 ca0 ca1 mul ca4 ca1 3 land ca5 7 3 lor ca6 4 1 \
     lshiftl ca7 1 4 lshiftr cs0 16 2 lt cs1 ca1 ca0 jnz cs1 2 fail jmp 2 fail halt";
  differential "jalr/sentry"
    ~regfile:(architectural_pc ^ "ca0 := (E-[X Ow LG LM], Global, 0, 4, 2)")
    "jalr cra ca0 fail halt"

let memory_and_locality () =
  differential "load/store"
    ~regfile:(architectural_pc ^ "ca0 := ([R WL LG LM], Global, 20, 21, 20)")
    "store ca0 42 load ca1 ca0 halt";
  differential "deep-local load"
    ~regfile:(architectural_pc ^ "ca0 := ([R WL DL LM], Global, 2, 3, 2)")
    "load ca1 ca0 halt # ([R WL LG LM], Global, 0, 8, 0)";
  differential "deep-read-only load"
    ~regfile:(architectural_pc ^ "ca0 := ([R WL LG DRO], Global, 2, 3, 2)")
    "load ca1 ca0 halt # ([R WL LG LM], Global, 0, 8, 0)";
  differential "local store rejection"
    ~regfile:
      (architectural_pc
     ^ "ca0 := ([R W LG LM], Global, 20, 21, 20) ca1 := ([R WL LG LM], Local, 0, 8, 0)")
    "store ca0 ca1 halt"

let capabilities_and_sealing () =
  differential "restrict/subseg/lea/getters"
    ~regfile:(architectural_pc ^ "ca0 := ([R WL LG LM], Global, 1, 20, 4)")
    "restrict ca0 ([R W DL DRO], Local) subseg ca0 2 10 lea ca0 1 getl ca1 ca0 getb ca2 ca0 gete \
     ca3 ca0 geta ca4 ca0 getp ca5 ca0 getwtype ca6 ca0 getotype ca7 ca0 halt";
  differential "seal/unseal"
    ~regfile:
      (architectural_pc ^ "ca0 := ([R WL LG LM], Global, 0, 8, 2) ca3 := [SU, Global, 0, 15, 0]")
    "seal ca1 ca3 ca0 getotype ca2 ca1 getwtype ca4 ca1 unseal ca5 ca3 ca1 halt";
  differential "malformed capability Restrict immediate"
    ~regfile:(architectural_pc ^ "ca0 := ([R WL LG LM], Global, 0, 8, 0)")
    "restrict ca0 999 halt";
  differential "malformed capability Restrict register"
    ~regfile:(architectural_pc ^ "ca0 := ([R WL LG LM], Global, 0, 8, 0)")
    "mov ca1 999 restrict ca0 ca1 halt";
  differential "malformed seal-range Restrict"
    ~regfile:(architectural_pc ^ "ca3 := [SU, Global, 0, 15, 0]")
    "restrict ca3 999 halt"

let system_halt_fail_and_malformed () =
  differential "system authorization"
    "mov ca0 77 writesr mtdc ca0 readsR ca1 mtdc mov cnull 99 halt";
  differential "system rejection" ~regfile:"pc := ([X Ow LG LM], Global, 0, 2, 0)"
    "readsr ca0 mtdc halt";
  differential "explicit fail" "fail";
  differential "malformed instruction word" "# 254";
  differential "non-integer instruction word" "# ([R W LG LM], Global, 0, 1, 0)";
  differential "missing sparse instruction"
    ~regfile:"pc := ([XSR Ow LG LM], Global, 0, MAX_ADDR, 10)" "halt"

let edits_and_boundaries () =
  let handwritten, extracted = sessions "halt" in
  let register = { Machine_view.Register_id.bank = General; key = "cra" } in
  let handwritten =
    ok (Machine_session.set_register_text register "([R WL LG LM], Local, 0, 8, 1)" handwritten)
  in
  let extracted =
    ok (Machine_session.set_register_text register "([R WL LG LM], Local, 0, 8, 1)" extracted)
  in
  compare_view "register edit" handwritten extracted;
  let handwritten =
    ok (Machine_session.set_memory_text (z 17) "{3: [SU, Global, 0, 15, 2]}" handwritten)
  in
  let extracted =
    ok (Machine_session.set_memory_text (z 17) "{3: [SU, Global, 0, 15, 2]}" extracted)
  in
  compare_view "memory edit and sealing metadata" handwritten extracted;
  let cnull = { Machine_view.Register_id.bank = General; key = "cnull" } in
  let extracted = ok (Machine_session.set_register_text cnull "99" extracted) in
  let cnull_word = Option.get (Machine_view.find_register cnull (Machine_session.view extracted)) in
  Alcotest.(check (option string))
    "cnull immutable" (Some "0")
    (Option.map Z.to_string cnull_word.word.integer);
  let mtdc = { Machine_view.Register_id.bank = System; key = "mtdc" } in
  let handwritten = ok (Machine_session.set_register_text mtdc "37" handwritten) in
  let extracted = ok (Machine_session.set_register_text mtdc "37" extracted) in
  compare_view "MTDC edit" handwritten extracted;
  let invalid_word = "([R WL LG LM], Global, 0, 2000001, 0)" in
  Alcotest.(check bool)
    "finite-value mismatch is diagnostic" true
    (Result.is_error (Machine_session.set_register_text register invalid_word extracted));
  let too_large = Runtime_config.create ~max_addr:(z 2_000_001) ~stack_addr:(z 64) () in
  Alcotest.(check bool)
    "configuration mismatch is diagnostic" true
    (Result.is_error
       (Machine_session.create ~backend:"griotte-extracted" ~config:too_large ~source:"halt"
          ~regfile:None));
  Alcotest.(check bool)
    "program finite mismatch is diagnostic" true
    (Result.is_error
       (Machine_session.create ~backend:"griotte-extracted" ~config
          ~source:"# ([R W LG LM], Global, 0, 2000001, 0)" ~regfile:None));
  Alcotest.(check bool)
    "regfile object-type mismatch is diagnostic" true
    (Result.is_error
       (Machine_session.create ~backend:"griotte-extracted" ~config ~source:"halt"
          ~regfile:(Some "cra := {2000001: ([R W LG LM], Global, 0, 8, 0)}")))

let rem_div_limitation () =
  let handwritten, extracted = sessions "rem ca0 5 2 halt" in
  let handwritten = Result.get_ok (Machine_session.step handwritten) in
  let extracted = Result.get_ok (Machine_session.step extracted) in
  Alcotest.(check bool)
    "handwritten Rem continues" true
    ((Machine_session.view handwritten).status = Running);
  Alcotest.(check bool)
    "extracted Rem is explicit Fail" true
    ((Machine_session.view extracted).status = Failed);
  let _, extracted = sessions "div ca0 8 2 halt" in
  let extracted = Result.get_ok (Machine_session.step extracted) in
  Alcotest.(check bool)
    "extracted Div is explicit Fail" true
    ((Machine_session.view extracted).status = Failed)

let step_n_contract () =
  let handwritten, extracted = sessions "mov ca0 1 mov ca1 2 halt" in
  let handwritten = Result.get_ok (Machine_session.step_n 2 handwritten) in
  let extracted = Result.get_ok (Machine_session.step_n 2 extracted) in
  compare_view "positive step_n" handwritten extracted;
  let _, extracted = sessions "halt" in
  Alcotest.(check bool)
    "negative step_n is structured backend error" true
    (match Machine_session.step_n (-1) extracted with
    | Error (Machine_backend.Backend_error _) -> true
    | Error (Stopped _) | Ok _ -> false)

let () =
  Alcotest.run "extracted Griotte"
    [
      ( "codec",
        [
          Alcotest.test_case "encoding identity" `Quick encoding_identity;
          Alcotest.test_case "malformed totality" `Quick decoder_totality;
        ] );
      ( "differential",
        [
          Alcotest.test_case "arithmetic, logic, and control" `Quick arithmetic_logic_control;
          Alcotest.test_case "memory and locality" `Quick memory_and_locality;
          Alcotest.test_case "capabilities and sealing" `Quick capabilities_and_sealing;
          Alcotest.test_case "system, terminal, malformed" `Quick system_halt_fail_and_malformed;
          Alcotest.test_case "edits and boundaries" `Quick edits_and_boundaries;
          Alcotest.test_case "step_n contract" `Quick step_n_contract;
          Alcotest.test_case "documented Rem/Div limitation" `Quick rem_div_limitation;
        ] );
    ]
