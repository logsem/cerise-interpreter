open Cerise

let ok = function
  | Ok value -> value
  | Error diagnostics ->
      Alcotest.fail (String.concat "; " (List.map Diagnostic.message diagnostics))

let z = Z.of_int
let config = Runtime_config.create ~max_addr:(z 128) ~stack_addr:(z 64) ()

let parser_and_printer () =
  let source =
    "jmp r1 jnz r1 r2 mov r1 -1 load r1 r2 store r1 r2 add r1 r2 1      sub r1 2 r2 mul r1 r2 3 rem r1 r2 4 div r1 r2 5 lt r1 r2 6      lea r1 -1 restrict r1 RW subseg r1 0 MAX_ADDR getb r1 r2 gete r1 r2      geta r1 r2 getp r1 r2 getotype r1 r2 getwtype r1 r2 seal r1 r2 r3      unseal r1 r2 r3 invoke r1 r2 isunique r1 r2 hash r1 r2      hashconcat r1 r2 -7 einit r1 r2 edeinit r1 estoreid r1 r2 fail halt"
  in
  ignore (ok (Cerisier.Parser.parse_program source));
  List.iter
    (fun text ->
      let term = ok (Cerisier.Parser.parse_word text) in
      let word = ok (Cerisier.Asm_ir.lower_word config term) in
      let printed = Cerisier.Printer.word word in
      let reparsed = ok (Cerisier.Parser.parse_word printed) in
      Alcotest.(check bool) text true (word = ok (Cerisier.Asm_ir.lower_word config reparsed)))
    [ "-17"; "(RW, 0, MAX_ADDR, 4)"; "[SU, 0, 8, 1]"; "{3: (RX, 0, 8, 1)}" ];
  List.iter
    (fun source ->
      Alcotest.(check bool) source true (Result.is_error (Cerisier.Parser.parse_program source)))
    [
      "getl r1 r2";
      "loadu r1 r2 0";
      "storeu r1 0 r2";
      "promoteu r1";
      "mov stk 0";
    ];
  List.iter
    (fun source ->
      Alcotest.(check bool) source true (Result.is_error (Cerisier.Parser.parse_word source)))
    [ "(RW, GLOBAL, 0, 8, 1)"; "(URW, 0, 8, 1)"; "(RWLX, 0, 8, 1)" ];
  Alcotest.(check string) "hash printer" "hash r1 r2"
    (Cerisier.Printer.instruction (Cerisier.Ast.Hash (Reg 1, Reg 2)));
  Alcotest.(check string) "hashconcat printer" "hashconcat r1 r2 -7"
    (Cerisier.Printer.instruction
       (Cerisier.Ast.HashConcat (Reg 1, Register (Reg 2), Constant (z (-7)))))

let instructions =
  let open Cerisier.Ast in
  let r1 = Reg 1 and r2 = Reg 2 and r3 = Reg 3 in
  let rr = Register (Reg 4) and c = Constant (z (-7)) in
  [
    Jmp r1; Jnz (r1, r2); Move (r1, rr); Move (r1, c); Load (r1, r2);
    Store (r1, rr); Store (r1, c); Add (r1, rr, rr); Add (r1, rr, c);
    Sub (r1, c, rr); Mul (r1, rr, c); Rem (r1, c, rr); Div (r1, c, c);
    Lt (r1, rr, rr); Lea (r1, c); Restrict (r1, c); SubSeg (r1, rr, c);
    GetB (r1, r2); GetE (r1, r2); GetA (r1, r2); GetP (r1, r2);
    GetOType (r1, r2); GetWType (r1, r2); Seal (r1, r2, r3);
    UnSeal (r1, r2, r3); Invoke (r1, r2); Fail; Halt; IsUnique (r1, r2);
    Hash (r1, r2); HashConcat (r1, rr, c); EInit (r1, r2); EDeInit r1;
    EStoreId (r1, r2);
  ]

let codec () =
  let vanilla = Vanilla.Codec.allocations in
  let expected =
    vanilla
    @ [
        ("IsUnique", 50, 1);
        ("Hash", 51, 1);
        ("HashConcat", 52, 4);
        ("EInit", 56, 1);
        ("EDeInit", 57, 1);
        ("EStoreId", 58, 1);
      ]
  in
  Alcotest.(check (list (triple string int int))) "allocations" expected
    Cerisier.Codec.allocations;
  List.iter
    (fun instruction ->
      let encoded = Result.get_ok (Cerisier.Codec.encode instruction) in
      Alcotest.(check bool) "codec round trip" true
        (Result.get_ok (Cerisier.Codec.decode encoded) = instruction))
    instructions;
  let open Cerisier.Ast in
  List.iter
    (fun (instruction, opcode) ->
      let encoded = Result.get_ok (Cerisier.Codec.encode instruction) in
      Alcotest.(check int) "opcode" opcode (Z.to_int (Z.extract encoded 0 8)))
    [
      (IsUnique (Reg 1, Reg 2), 50);
      (Hash (Reg 1, Reg 2), 51);
      (HashConcat (Reg 1, Register (Reg 2), Register (Reg 3)), 52);
      (HashConcat (Reg 1, Constant Z.zero, Register (Reg 3)), 54);
      (HashConcat (Reg 1, Register (Reg 2), Constant Z.zero), 53);
      (HashConcat (Reg 1, Constant Z.zero, Constant Z.zero), 55);
      (EInit (Reg 1, Reg 2), 56);
      (EDeInit (Reg 1), 57);
      (EStoreId (Reg 1, Reg 2), 58);
    ]

let base_state () =
  let open Cerisier in
  Machine.init config [] None
  |> Machine.set_register Ast.PC
       (Ast.Sealable (Ast.Cap (Ast.RWX, Z.zero, z 2, Z.zero)))

let check_word label expected actual =
  Alcotest.(check bool) label true (expected = actual)

let hashing () =
  let open Cerisier in
  let source = Ast.Sealed (z 7, Ast.Cap (Ast.RO, z 10, z 12, z 10)) in
  let state =
    base_state ()
    |> Machine.set_register (Ast.Reg 2) source
    |> Machine.execute (Ast.Hash (Ast.Reg 1, Ast.Reg 2))
  in
  check_word "hash any word"
    (Ast.I (Z.of_int (Hashtbl.hash source)))
    (Machine.read_register (Ast.Reg 1) state);
  let state =
    base_state ()
    |> Machine.set_register (Ast.Reg 2) (Ast.I (z 11))
    |> Machine.execute
         (Ast.HashConcat (Ast.Reg 1, Ast.Register (Ast.Reg 2), Ast.Constant (z 13)))
  in
  check_word "hashconcat integers"
    (Ast.I (Z.of_int (Hashtbl.hash (z 11, z 13))))
    (Machine.read_register (Ast.Reg 1) state);
  let failed =
    base_state ()
    |> Machine.set_register (Ast.Reg 2) source
    |> Machine.execute
         (Ast.HashConcat (Ast.Reg 1, Ast.Register (Ast.Reg 2), Ast.Constant Z.zero))
  in
  Alcotest.(check bool) "hashconcat rejects non-integer" true
    (failed.status = Machine.Failed)

let uniqueness () =
  let open Cerisier in
  let candidate = Ast.Sealable (Ast.Cap (Ast.RW, z 20, z 30, z 20)) in
  let unique =
    base_state ()
    |> Machine.set_register (Ast.Reg 2) candidate
    |> Machine.execute (Ast.IsUnique (Ast.Reg 1, Ast.Reg 2))
  in
  check_word "unique capability" (Ast.I Z.one)
    (Machine.read_register (Ast.Reg 1) unique);
  let overlapping =
    base_state ()
    |> Machine.set_register (Ast.Reg 2)
         (Ast.Sealed (z 9, Ast.Cap (Ast.RW, z 20, z 30, z 20)))
    |> Machine.set_memory_raw (z 80)
         (Ast.Sealable (Ast.Cap (Ast.RO, z 29, z 35, z 29)))
    |> Machine.execute (Ast.IsUnique (Ast.Reg 1, Ast.Reg 2))
  in
  check_word "sealed overlap" (Ast.I Z.zero)
    (Machine.read_register (Ast.Reg 1) overlapping);
  let failed =
    base_state ()
    |> Machine.set_register (Ast.Reg 2) (Ast.I Z.zero)
    |> Machine.execute (Ast.IsUnique (Ast.Reg 1, Ast.Reg 2))
  in
  Alcotest.(check bool) "non-capability rejected" true
    (failed.status = Machine.Failed)

let einit_state () =
  let open Cerisier in
  base_state ()
  |> Machine.set_register (Ast.Reg 1)
       (Ast.Sealable (Ast.Cap (Ast.RX, z 10, z 14, z 10)))
  |> Machine.set_register (Ast.Reg 2)
       (Ast.Sealable (Ast.Cap (Ast.RW, z 20, z 24, z 21)))
  |> Machine.set_memory_raw (z 11) (Ast.I (z 101))
  |> Machine.set_memory_raw (z 12) (Ast.I (z 102))
  |> Machine.set_memory_raw (z 13) (Ast.I (z 103))

let attestation () =
  let open Cerisier in
  let initialized = Machine.execute (Ast.EInit (Ast.Reg 1, Ast.Reg 2)) (einit_state ()) in
  Alcotest.(check bool) "einit running" true (initialized.status = Machine.Running);
  check_word "entry capability"
    (Ast.Sealable (Ast.Cap (Ast.E, z 10, z 14, z 11)))
    (Machine.read_register (Ast.Reg 1) initialized);
  check_word "data register cleared" (Ast.I Z.zero)
    (Machine.read_register (Ast.Reg 2) initialized);
  check_word "data capability installed"
    (Ast.Sealable (Ast.Cap (Ast.RW, z 20, z 24, z 21)))
    (Option.get (Machine.read_memory (z 10) initialized));
  let keys = Ast.Sealable (Ast.SealRange ((true, true), Z.zero, z 2, Z.zero)) in
  check_word "seal keys installed" keys
    (Option.get (Machine.read_memory (z 20) initialized));
  Alcotest.(check string) "counter incremented" "1"
    (Z.to_string initialized.enclave_counter);
  let code_words = [ Ast.I (z 101); Ast.I (z 102); Ast.I (z 103) ] in
  let expected_identity =
    Z.of_int
      (Hashtbl.hash
         (Z.of_int (Hashtbl.hash (z 10)), Z.of_int (Hashtbl.hash code_words)))
  in
  Alcotest.(check string) "identity" (Z.to_string expected_identity)
    (Z.to_string (Machine.ETableMap.find Z.zero initialized.enclave_table));
  let twice_initialized =
    initialized
    |> Machine.set_register (Ast.Reg 1)
         (Ast.Sealable (Ast.Cap (Ast.RX, z 30, z 34, z 30)))
    |> Machine.set_register (Ast.Reg 2)
         (Ast.Sealable (Ast.Cap (Ast.RW, z 40, z 44, z 40)))
    |> Machine.set_memory_raw (z 31) (Ast.I (z 201))
    |> Machine.set_memory_raw (z 32) (Ast.I (z 202))
    |> Machine.set_memory_raw (z 33) (Ast.I (z 203))
    |> Machine.execute (Ast.EInit (Ast.Reg 1, Ast.Reg 2))
  in
  Alcotest.(check (list string)) "ordered table entries" [ "0"; "1" ]
    (Machine.ETableMap.bindings twice_initialized.enclave_table
    |> List.map (fun (id, _) -> Z.to_string id));
  Alcotest.(check string) "monotonic second counter" "2"
    (Z.to_string twice_initialized.enclave_counter);
  check_word "second seal-key allocation"
    (Ast.Sealable (Ast.SealRange ((true, true), z 2, z 4, z 2)))
    (Option.get (Machine.read_memory (z 40) twice_initialized));
  let stored =
    initialized
    |> Machine.set_register (Ast.Reg 3) (Ast.I Z.one)
    |> Machine.execute (Ast.EStoreId (Ast.Reg 4, Ast.Reg 3))
  in
  check_word "odd otype maps down" (Ast.I expected_identity)
    (Machine.read_register (Ast.Reg 4) stored);
  let deinitialized =
    initialized
    |> Machine.set_register (Ast.Reg 3) keys
    |> Machine.execute (Ast.EDeInit (Ast.Reg 3))
  in
  Alcotest.(check int) "entry removed" 0
    (Machine.ETableMap.cardinal deinitialized.enclave_table);
  Alcotest.(check string) "counter monotonic" "1"
    (Z.to_string deinitialized.enclave_counter);
  let missing =
    deinitialized |> Machine.execute (Ast.EDeInit (Ast.Reg 3))
  in
  Alcotest.(check bool) "missing entry fails" true (missing.status = Machine.Failed);
  let bad_before =
    einit_state ()
    |> Machine.set_memory_raw (z 12)
         (Ast.Sealable (Ast.Cap (Ast.RO, z 40, z 41, z 40)))
  in
  let bad_after = Machine.execute (Ast.EInit (Ast.Reg 1, Ast.Reg 2)) bad_before in
  Alcotest.(check bool) "non-integer code fails" true (bad_after.status = Machine.Failed);
  Alcotest.(check bool) "failed einit preserves registers" true
    (bad_after.registers = bad_before.registers);
  Alcotest.(check bool) "failed einit preserves memory" true
    (bad_after.memory = bad_before.memory);
  Alcotest.(check bool) "failed einit preserves table" true
    (bad_after.enclave_table = bad_before.enclave_table);
  Alcotest.(check bool) "failed einit preserves counter" true
    (Z.equal bad_after.enclave_counter bad_before.enclave_counter)

let initialization_and_view () =
  let vanilla = Vanilla.Machine.init config [] None in
  let cerisier = Cerisier.Machine.init config [] None in
  List.iter
    (fun (vanilla_register, cerisier_register) ->
      Alcotest.(check string) "vanilla register initialization"
        (Vanilla.Printer.word (Vanilla.Machine.read_register vanilla_register vanilla))
        (Cerisier.Printer.word
           (Cerisier.Machine.read_register cerisier_register cerisier)))
    ((Vanilla.Ast.PC, Cerisier.Ast.PC)
    :: List.init 32 (fun n -> (Vanilla.Ast.Reg n, Cerisier.Ast.Reg n)));
  let view = Cerisier.Backend.inspect cerisier in
  Alcotest.(check bool) "table exposed" true (Option.is_some view.enclave_table);
  let r31 =
    Machine_view.find_register { bank = Machine_view.General; key = "r31" } view
  in
  Alcotest.(check bool) "r31 ordinary register" true (Option.is_some r31);
  Alcotest.(check bool) "no stk alias" true
    (Option.is_none
       (Machine_view.find_register { bank = Machine_view.System; key = "stk" } view));
  let ddc =
    Option.get
      (Machine_view.find_register { bank = Machine_view.System; key = "ddc" } view)
  in
  Alcotest.(check bool) "no locality metadata" true
    (Option.bind ddc.word.seal_range (fun range -> range.locality) = None)

let enclave_example () =
  let relative = "test_files/cerisier/pos/enclave.s" in
  let path = if Sys.file_exists relative then relative else "tests/" ^ relative in
  let source = In_channel.with_open_bin path In_channel.input_all in
  let initial =
    ok
      (Machine_session.create ~backend:"cerisier" ~config ~source ~regfile:None)
  in
  let result = Machine_session.run ~max_steps:200 initial in
  (match result.reason with
  | Machine_session.Halted -> ()
  | Failed -> Alcotest.failf "example failed after %d steps" result.steps
  | Step_limit -> Alcotest.failf "example reached step limit after %d steps" result.steps
  | Breakpoint pc -> Alcotest.failf "example hit breakpoint %s" (Z.to_string pc)
  | Execution_error _ -> Alcotest.fail "example produced an execution error");
  match (Machine_session.view result.session).enclave_table with
  | Some table ->
      Alcotest.(check string) "counter remains monotonic" "1"
        (Z.to_string table.counter);
      Alcotest.(check int) "example deinitializes enclave" 0
        (List.length table.entries)
  | None -> Alcotest.fail "Cerisier enclave table is absent"

let () =
  Alcotest.run "cerisier backend"
    [
      ("syntax", [ Alcotest.test_case "parser and printer" `Quick parser_and_printer ]);
      ("codec", [ Alcotest.test_case "fixed allocations" `Quick codec ]);
      ("machine",
       [
         Alcotest.test_case "initialization and view" `Quick initialization_and_view;
         Alcotest.test_case "hashing" `Quick hashing;
         Alcotest.test_case "uniqueness" `Quick uniqueness;
         Alcotest.test_case "attestation" `Quick attestation;
         Alcotest.test_case "full enclave example" `Quick enclave_example;
       ]);
    ]
