open Cerise

let ok = function
  | Ok value -> value
  | Error diagnostics ->
      Alcotest.fail (String.concat "; " (List.map Diagnostic.message diagnostics))

let z = Z.of_int
let config = Runtime_config.create ~max_addr:(z 128) ~stack_addr:(z 64) ()

let parser_and_printer () =
  let source =
    "jmp r1 jnz r1 r2 mov r1 -1 load r1 r2 store r1 r2 add r1 r2 1      sub r1 2 r2 mul r1 r2 3 \
     rem r1 r2 4 div r1 r2 5 lt r1 r2 6      lea r1 -1 restrict r1 RW subseg r1 0 MAX_ADDR getb r1 \
     r2 gete r1 r2      geta r1 r2 getp r1 r2 getotype r1 r2 getwtype r1 r2 seal r1 r2 r3      \
     unseal r1 r2 r3 invoke r1 r2 isunique r1 r2 hash r1 r2      hashconcat r1 r2 -7 einit r1 r2 \
     edeinit r1 estoreid r1 r2 fail halt"
  in
  ignore (ok (Cerisier.Parser.parse_program source));
  List.iter
    (fun text ->
      let term = ok (Cerisier.Parser.parse_word text) in
      let word = ok (Cerisier.Asm_ir.assemble_word config term) in
      let printed = Cerisier.Printer.word word in
      let reparsed = ok (Cerisier.Parser.parse_word printed) in
      Alcotest.(check bool) text true (word = ok (Cerisier.Asm_ir.assemble_word config reparsed)))
    [ "-17"; "(RW, 0, MAX_ADDR, 4)"; "[SU, 0, 8, 1]"; "{3: (RX, 0, 8, 1)}" ];
  List.iter
    (fun source ->
      Alcotest.(check bool) source true (Result.is_error (Cerisier.Parser.parse_program source)))
    [ "getl r1 r2"; "loadu r1 r2 0"; "storeu r1 0 r2"; "promoteu r1"; "mov stk 0" ];
  List.iter
    (fun source ->
      Alcotest.(check bool) source true (Result.is_error (Cerisier.Parser.parse_word source)))
    [ "(RW, GLOBAL, 0, 8, 1)"; "(URW, 0, 8, 1)"; "(RWLX, 0, 8, 1)" ];
  Alcotest.(check string)
    "hash printer" "hash r1 r2"
    (Cerisier.Printer.instruction (Cerisier.Ast.Hash (Reg 1, Reg 2)));
  Alcotest.(check string)
    "hashconcat printer" "hashconcat r1 r2 -7"
    (Cerisier.Printer.instruction
       (Cerisier.Ast.HashConcat (Reg 1, Register (Reg 2), Constant (z (-7)))))

let instructions =
  let open Cerisier.Ast in
  let r1 = Reg 1 and r2 = Reg 2 and r3 = Reg 3 in
  let rr = Register (Reg 4) and c = Constant (z (-7)) in
  [
    Jmp r1;
    Jnz (r1, r2);
    Move (r1, rr);
    Move (r1, c);
    Load (r1, r2);
    Store (r1, rr);
    Store (r1, c);
    Add (r1, rr, rr);
    Add (r1, rr, c);
    Sub (r1, c, rr);
    Mul (r1, rr, c);
    Rem (r1, c, rr);
    Div (r1, c, c);
    Lt (r1, rr, rr);
    Lea (r1, c);
    Restrict (r1, c);
    SubSeg (r1, rr, c);
    GetB (r1, r2);
    GetE (r1, r2);
    GetA (r1, r2);
    GetP (r1, r2);
    GetOType (r1, r2);
    GetWType (r1, r2);
    Seal (r1, r2, r3);
    UnSeal (r1, r2, r3);
    Invoke (r1, r2);
    Fail;
    Halt;
    IsUnique (r1, r2);
    Hash (r1, r2);
    HashConcat (r1, rr, c);
    EInit (r1, r2);
    EDeInit r1;
    EStoreId (r1, r2);
  ]

let codec () =
  List.iter
    (fun instruction ->
      let encoded = Result.get_ok (Cerisier.Codec.encode instruction) in
      Alcotest.(check bool)
        "codec round trip" true
        (Result.get_ok (Cerisier.Codec.decode encoded) = instruction))
    instructions

let base_state () =
  let open Cerisier in
  Machine.init config [] None
  |> Machine.set_register Ast.PC (Ast.Sealable (Ast.Cap (Ast.RWX, Z.zero, z 2, Z.zero)))

let check_word label expected actual = Alcotest.(check bool) label true (expected = actual)

let machine_hash_word (value : Cerisier.Ast.word) : Z.t =
  let open Cerisier in
  let state =
    base_state ()
    |> Machine.set_register (Ast.Reg 2) value
    |> Machine.execute config (Ast.Hash (Ast.Reg 1, Ast.Reg 2))
  in
  match Machine.read_register (Ast.Reg 1) state with
  | Ast.I hash -> hash
  | _ -> Alcotest.fail "Hash did not produce an integer"

let machine_hash_concat (left : Z.t) (right : Z.t) : Z.t =
  let open Cerisier in
  let state =
    base_state ()
    |> Machine.set_register (Ast.Reg 2) (Ast.I left)
    |> Machine.set_register (Ast.Reg 3) (Ast.I right)
    |> Machine.execute config
         (Ast.HashConcat (Ast.Reg 1, Ast.Register (Ast.Reg 2), Ast.Register (Ast.Reg 3)))
  in
  match Machine.read_register (Ast.Reg 1) state with
  | Ast.I hash -> hash
  | _ -> Alcotest.fail "HashConcat did not produce an integer"

let machine_address_hash (address : int) : Z.t =
  let open Cerisier in
  let initialized =
    base_state ()
    |> Machine.set_register (Ast.Reg 1)
         (Ast.Sealable (Ast.Cap (Ast.RX, z address, z (address + 1), z address)))
    |> Machine.set_register (Ast.Reg 2) (Ast.Sealable (Ast.Cap (Ast.RW, z 20, z 22, z 20)))
    |> Machine.execute config (Ast.EInit (Ast.Reg 1, Ast.Reg 2))
  in
  Machine.ETableMap.find Z.zero initialized.enclave_table

let hashing () =
  let open Cerisier in
  let goldens =
    [
      (Ast.I (z (-17)), "170141183460469231731687303715884106274");
      (Ast.Sealable (Ast.Cap (Ast.RW, z 10, z 12, z 11)), "170141183460469231731687303729057717065");
      ( Ast.Sealable (Ast.SealRange ((true, false), z 2, z 8, z 3)),
        "170141183460469231731687303733385179424" );
      ( Ast.Sealed (z 7, Ast.Cap (Ast.RO, z 10, z 12, z 10)),
        "170141183460469231731687309382785407573" );
      ( Ast.Sealed (z 9, Ast.SealRange ((false, true), Z.zero, z 4, Z.one)),
        "170141183460469231731687310521371882531" );
    ]
  in
  List.iter
    (fun (word, expected) ->
      Alcotest.(check string)
        "stable structural word hash" expected
        (Z.to_string (machine_hash_word word)))
    goldens;
  Alcotest.(check string)
    "stable address hash" "170141183460469231731687303715884106004"
    (Z.to_string (machine_address_hash 10));
  Alcotest.(check string)
    "nonnegative raw fragments" "24"
    (Z.to_string (machine_hash_concat (z 11) (z 13)));
  let h1 = machine_hash_word (Ast.I Z.one)
  and h2 = machine_hash_word (Ast.I (z 2))
  and h3 = machine_hash_word (Ast.I (z 3)) in
  Alcotest.(check string)
    "left concat identity" (Z.to_string h1)
    (Z.to_string (machine_hash_concat Z.zero h1));
  Alcotest.(check string)
    "right concat identity" (Z.to_string h1)
    (Z.to_string (machine_hash_concat h1 Z.zero));
  Alcotest.(check string)
    "associative list concatenation"
    (Z.to_string (machine_hash_concat (machine_hash_concat h1 h2) h3))
    (Z.to_string (machine_hash_concat h1 (machine_hash_concat h2 h3)));
  let failed =
    base_state ()
    |> Machine.set_register (Ast.Reg 2) (Ast.I h1)
    |> Machine.execute config
         (Ast.HashConcat (Ast.Reg 1, Ast.Register (Ast.Reg 2), Ast.Constant Z.minus_one))
  in
  Alcotest.(check bool) "hashconcat rejects negative fragments" true (failed.status = Machine.Failed)

let uniqueness () =
  let open Cerisier in
  let candidate = Ast.Sealable (Ast.Cap (Ast.RW, z 20, z 30, z 20)) in
  let unique =
    base_state ()
    |> Machine.set_register (Ast.Reg 2) candidate
    |> Machine.execute config (Ast.IsUnique (Ast.Reg 1, Ast.Reg 2))
  in
  check_word "unique capability" (Ast.I Z.one) (Machine.read_register (Ast.Reg 1) unique);
  let overlapping =
    base_state ()
    |> Machine.set_register (Ast.Reg 2) (Ast.Sealed (z 9, Ast.Cap (Ast.RW, z 20, z 30, z 20)))
    |> Machine.set_memory_raw (z 80) (Ast.Sealable (Ast.Cap (Ast.RO, z 29, z 35, z 29)))
    |> Machine.execute config (Ast.IsUnique (Ast.Reg 1, Ast.Reg 2))
  in
  check_word "sealed overlap" (Ast.I Z.zero) (Machine.read_register (Ast.Reg 1) overlapping);
  let failed =
    base_state ()
    |> Machine.set_register (Ast.Reg 2) (Ast.I Z.zero)
    |> Machine.execute config (Ast.IsUnique (Ast.Reg 1, Ast.Reg 2))
  in
  Alcotest.(check bool) "non-capability rejected" true (failed.status = Machine.Failed)

let einit_state () =
  let open Cerisier in
  base_state ()
  |> Machine.set_register (Ast.Reg 1) (Ast.Sealable (Ast.Cap (Ast.RX, z 10, z 14, z 10)))
  |> Machine.set_register (Ast.Reg 2) (Ast.Sealable (Ast.Cap (Ast.RW, z 20, z 24, z 21)))
  |> Machine.set_memory_raw (z 11) (Ast.I (z 101))
  |> Machine.set_memory_raw (z 12) (Ast.I (z 102))
  |> Machine.set_memory_raw (z 13) (Ast.I (z 103))

let attestation () =
  let open Cerisier in
  let initialized = Machine.execute config (Ast.EInit (Ast.Reg 1, Ast.Reg 2)) (einit_state ()) in
  Alcotest.(check bool) "einit running" true (initialized.status = Machine.Running);
  check_word "entry capability"
    (Ast.Sealable (Ast.Cap (Ast.E, z 10, z 14, z 11)))
    (Machine.read_register (Ast.Reg 1) initialized);
  check_word "data register cleared" (Ast.I Z.zero) (Machine.read_register (Ast.Reg 2) initialized);
  check_word "data capability installed"
    (Ast.Sealable (Ast.Cap (Ast.RW, z 20, z 24, z 21)))
    (Option.get (Machine.read_memory config (z 10) initialized));
  let keys = Ast.Sealable (Ast.SealRange ((true, true), Z.zero, z 2, Z.zero)) in
  check_word "seal keys installed" keys (Option.get (Machine.read_memory config (z 20) initialized));
  Alcotest.(check string) "counter incremented" "1" (Z.to_string initialized.enclave_counter);
  let expected_identity = Z.of_string "680564733841876926926749214868285861499" in
  let code_hash =
    List.fold_left
      (fun accumulated word -> machine_hash_concat accumulated (machine_hash_word word))
      Z.zero
      [ Ast.I (z 101); Ast.I (z 102); Ast.I (z 103) ]
  in
  Alcotest.(check string)
    "EInit address/code composition" (Z.to_string expected_identity)
    (Z.to_string (machine_hash_concat (machine_address_hash 10) code_hash));
  Alcotest.(check string)
    "identity" (Z.to_string expected_identity)
    (Z.to_string (Machine.ETableMap.find Z.zero initialized.enclave_table));
  let twice_initialized =
    initialized
    |> Machine.set_register (Ast.Reg 1) (Ast.Sealable (Ast.Cap (Ast.RX, z 30, z 34, z 30)))
    |> Machine.set_register (Ast.Reg 2) (Ast.Sealable (Ast.Cap (Ast.RW, z 40, z 44, z 40)))
    |> Machine.set_memory_raw (z 31) (Ast.I (z 201))
    |> Machine.set_memory_raw (z 32) (Ast.I (z 202))
    |> Machine.set_memory_raw (z 33) (Ast.I (z 203))
    |> Machine.execute config (Ast.EInit (Ast.Reg 1, Ast.Reg 2))
  in
  Alcotest.(check (list string))
    "ordered table entries" [ "0"; "1" ]
    (Machine.ETableMap.bindings twice_initialized.enclave_table
    |> List.map (fun (id, _) -> Z.to_string id));
  Alcotest.(check string)
    "monotonic second counter" "2"
    (Z.to_string twice_initialized.enclave_counter);
  check_word "second seal-key allocation"
    (Ast.Sealable (Ast.SealRange ((true, true), z 2, z 4, z 2)))
    (Option.get (Machine.read_memory config (z 40) twice_initialized));
  let stored =
    initialized
    |> Machine.set_register (Ast.Reg 3) (Ast.I Z.one)
    |> Machine.execute config (Ast.EStoreId (Ast.Reg 4, Ast.Reg 3))
  in
  check_word "odd otype maps down" (Ast.I expected_identity)
    (Machine.read_register (Ast.Reg 4) stored);
  let deinitialized =
    initialized
    |> Machine.set_register (Ast.Reg 3) keys
    |> Machine.execute config (Ast.EDeInit (Ast.Reg 3))
  in
  Alcotest.(check int) "entry removed" 0 (Machine.ETableMap.cardinal deinitialized.enclave_table);
  Alcotest.(check string) "counter monotonic" "1" (Z.to_string deinitialized.enclave_counter);
  let missing = deinitialized |> Machine.execute config (Ast.EDeInit (Ast.Reg 3)) in
  Alcotest.(check bool) "missing entry fails" true (missing.status = Machine.Failed);
  let bad_before =
    einit_state ()
    |> Machine.set_memory_raw (z 12) (Ast.Sealable (Ast.Cap (Ast.RO, z 40, z 41, z 40)))
  in
  let bad_after = Machine.execute config (Ast.EInit (Ast.Reg 1, Ast.Reg 2)) bad_before in
  Alcotest.(check bool) "non-integer code fails" true (bad_after.status = Machine.Failed);
  Alcotest.(check bool)
    "failed einit preserves registers" true
    (bad_after.registers = bad_before.registers);
  Alcotest.(check bool) "failed einit preserves memory" true (bad_after.memory = bad_before.memory);
  Alcotest.(check bool)
    "failed einit preserves table" true
    (bad_after.enclave_table = bad_before.enclave_table);
  Alcotest.(check bool)
    "failed einit preserves counter" true
    (Z.equal bad_after.enclave_counter bad_before.enclave_counter)

let initialization_and_view () =
  let vanilla = Vanilla.Machine.init config [] None in
  let cerisier = Cerisier.Machine.init config [] None in
  List.iter
    (fun (vanilla_register, cerisier_register) ->
      Alcotest.(check string)
        "vanilla register initialization"
        (Vanilla.Printer.word (Vanilla.Machine.read_register vanilla_register vanilla))
        (Cerisier.Printer.word (Cerisier.Machine.read_register cerisier_register cerisier)))
    ((Vanilla.Ast.PC, Cerisier.Ast.PC)
    :: List.init 32 (fun n -> (Vanilla.Ast.Reg n, Cerisier.Ast.Reg n)));
  let view = Cerisier.Backend.inspect config cerisier in
  Alcotest.(check bool) "table exposed" true (Option.is_some view.enclave_table);
  let r31 = Machine_view.find_register { bank = Machine_view.General; key = "r31" } view in
  Alcotest.(check bool) "r31 ordinary register" true (Option.is_some r31);
  Alcotest.(check bool)
    "no stk alias" true
    (Option.is_none (Machine_view.find_register { bank = Machine_view.System; key = "stk" } view));
  let ddc =
    Option.get (Machine_view.find_register { bank = Machine_view.System; key = "ddc" } view)
  in
  Alcotest.(check bool)
    "no locality metadata" true
    (Option.bind ddc.word.seal_range (fun range -> range.locality) = None)

let enclave_example () =
  let relative = "test_files/cerisier/pos/enclave.s" in
  let path = if Sys.file_exists relative then relative else "tests/" ^ relative in
  let source = In_channel.with_open_bin path In_channel.input_all in
  let initial = ok (Machine_session.create ~backend:"cerisier" ~config ~source ~regfile:None) in
  let result = Machine_session.run ~max_steps:200 initial in
  (match result.reason with
  | Machine_session.Halted -> ()
  | Failed -> Alcotest.failf "example failed after %d steps" result.steps
  | Step_limit -> Alcotest.failf "example reached step limit after %d steps" result.steps
  | Breakpoint pc -> Alcotest.failf "example hit breakpoint %s" (Z.to_string pc)
  | Execution_error _ -> Alcotest.fail "example produced an execution error");
  match (Machine_session.view result.session).enclave_table with
  | Some table ->
      Alcotest.(check string) "counter remains monotonic" "1" (Z.to_string table.counter);
      Alcotest.(check int) "example deinitializes enclave" 0 (List.length table.entries)
  | None -> Alcotest.fail "Cerisier enclave table is absent"

type case_study_expectation = {
  fixture : string;
  regfile : string option;
  max_steps : int;
  registers : (string * int) list;
  memory : (int * int) list;
  enclave_counter : int;
  active_enclaves : int;
  enclave_identities : Z.t list;
}

let case_study_config = Runtime_config.create ~max_addr:(z 4096) ~stack_addr:(z 3072) ()

let case_study_path (fixture : string) : string =
  let relative = "case_studies/cerisier/" ^ fixture in
  if Sys.file_exists relative then relative else "../../../" ^ relative

let read_case_study_file (fixture : string) : string =
  In_channel.with_open_bin (case_study_path fixture) In_channel.input_all

let register_id (key : string) : Machine_view.register_id =
  match key with
  | "pc" -> { bank = Machine_view.System; key }
  | "r0" | "ddc" -> { bank = Machine_view.System; key = "ddc" }
  | _ -> { bank = Machine_view.General; key }

let integer_register (key : string) (view : Machine_view.t) : Z.t =
  match Machine_view.find_register (register_id key) view with
  | Some { word = { integer = Some value; _ }; _ } -> value
  | Some register ->
      Alcotest.failf "case study register %s is not an integer: %s" key register.word.detail_text
  | None -> Alcotest.failf "case study register %s is absent" key

let integer_memory (address : int) (view : Machine_view.t) : Z.t =
  match Machine_view.find_memory_word (z address) view with
  | Some { integer = Some value; _ } -> value
  | Some word ->
      Alcotest.failf "case study memory %d is not an integer: %s" address word.detail_text
  | None -> Alcotest.failf "case study memory %d is absent" address

let create_case_study_session ~(fixture : string) ~(regfile : string option) ~(source : string) :
    Machine_session.t =
  let path = case_study_path fixture in
  let regfile_filename = Option.map case_study_path regfile in
  let regfile_source = Option.map read_case_study_file regfile in
  ok
    (Machine_session.create_with_filenames ~source_filename:path ~regfile_filename
       ~backend:"cerisier" ~config:case_study_config ~source ~regfile:regfile_source)

let run_case_study (expected : case_study_expectation) () =
  let source = read_case_study_file expected.fixture in
  let initial =
    create_case_study_session ~fixture:expected.fixture ~regfile:expected.regfile ~source
  in
  let result = Machine_session.run ~max_steps:expected.max_steps initial in
  (match result.reason with
  | Machine_session.Halted -> ()
  | Failed -> Alcotest.failf "%s failed after %d steps" expected.fixture result.steps
  | Step_limit -> Alcotest.failf "%s reached its %d-step limit" expected.fixture result.steps
  | Breakpoint pc -> Alcotest.failf "%s hit breakpoint %s" expected.fixture (Z.to_string pc)
  | Execution_error error ->
      Alcotest.failf "%s produced an execution error after %d steps: %s" expected.fixture
        result.steps
        (Machine_backend.execution_error_message error));
  let view = Machine_session.view result.session in
  List.iter
    (fun (register, value) ->
      Alcotest.(check string)
        (expected.fixture ^ " " ^ register)
        (string_of_int value)
        (Z.to_string (integer_register register view)))
    expected.registers;
  List.iter
    (fun (address, value) ->
      Alcotest.(check string)
        (Printf.sprintf "%s memory %d" expected.fixture address)
        (string_of_int value)
        (Z.to_string (integer_memory address view)))
    expected.memory;
  match view.enclave_table with
  | Some table ->
      Alcotest.(check string)
        (expected.fixture ^ " enclave counter")
        (string_of_int expected.enclave_counter)
        (Z.to_string table.counter);
      Alcotest.(check int)
        (expected.fixture ^ " active enclaves")
        expected.active_enclaves (List.length table.entries);
      if expected.enclave_identities <> [] then
        Alcotest.(check (list string))
          (expected.fixture ^ " enclave identities")
          (List.map Z.to_string expected.enclave_identities)
          (List.map
             (fun (entry : Machine_view.enclave_table_entry) -> Z.to_string entry.identity)
             table.entries)
  | None -> Alcotest.failf "%s has no Cerisier enclave table" expected.fixture

let replace_once ~(needle : string) ~(replacement : string) (source : string) : string =
  let needle_length = String.length needle in
  let rec find index =
    if index + needle_length > String.length source then
      Alcotest.failf "case-study test mutation did not find %S" needle
    else if String.sub source index needle_length = needle then index
    else find (index + 1)
  in
  let index = find 0 in
  String.sub source 0 index ^ replacement
  ^ String.sub source (index + needle_length) (String.length source - index - needle_length)

let soc_hash = Z.of_string "3664546399000423895061894810877788924077"
let soc_fixture = "secure_outsourced_computation.s"
let soc_regfile = Some "secure_outsourced_computation.reg"
let ma_pre_a = Z.of_string "28249013258660799722221594376824205332309"
let ma_pre_b = Z.of_string "23075161495184733793446375969728546363081"
let ma_hash_a = Z.of_string "28586538272572871769842225592882347710580"
let ma_hash_b = Z.of_string "23463451373229852078782113054873498149463"
let ma_fixture = "mutual_attestation.s"
let ma_regfile = Some "mutual_attestation.reg"
let ts_sensor_hash = Z.of_string "7182681053998082077296997438663194947164"
let ts_client_hash = Z.of_string "7795339977280813461738391276462452401620"
let ts_fixture = "trusted_sensor_readout.s"
let ts_regfile = Some "trusted_sensor_readout.reg"

let soc_session (source : string) : Machine_session.t =
  create_case_study_session ~fixture:soc_fixture ~regfile:soc_regfile ~source

let ma_session (source : string) : Machine_session.t =
  create_case_study_session ~fixture:ma_fixture ~regfile:ma_regfile ~source

let ts_session (source : string) : Machine_session.t =
  create_case_study_session ~fixture:ts_fixture ~regfile:ts_regfile ~source

let enclave_identity (id : int) (view : Machine_view.t) : Z.t =
  match view.enclave_table with
  | Some table -> (
      match
        List.find_opt
          (fun (entry : Machine_view.enclave_table_entry) -> Z.equal entry.id (z id))
          table.entries
      with
      | Some entry -> entry.identity
      | None -> Alcotest.failf "Cerisier enclave table has no entry %d" id)
  | None -> Alcotest.fail "Cerisier enclave table is absent"

let ma_derived_pre_a () =
  let source =
    read_case_study_file ma_fixture
    |> replace_once ~needle:"    subseg r10 enclave_a_start enclave_a_end\n"
         ~replacement:"    subseg r10 enclave_a_start enclave_a_identity_table\n"
  in
  let after_einit = Result.get_ok (Machine_session.step_n 17 (ma_session source)) in
  Alcotest.(check string)
    "derived A pre-hash" (Z.to_string ma_pre_a)
    (Z.to_string (enclave_identity 0 (Machine_session.view after_einit)))

let ma_derived_pre_b () =
  let source =
    read_case_study_file ma_fixture
    |> replace_once ~needle:"    subseg r12 enclave_b_start enclave_b_end\n"
         ~replacement:"    subseg r12 enclave_b_start enclave_b_identity_table\n"
  in
  let after_einit = Result.get_ok (Machine_session.step_n 18 (ma_session source)) in
  Alcotest.(check string)
    "derived B pre-hash" (Z.to_string ma_pre_b)
    (Z.to_string (enclave_identity 1 (Machine_session.view after_einit)))

let ma_derived_hash_a () =
  let after_einit =
    Result.get_ok (Machine_session.step_n 18 (ma_session (read_case_study_file ma_fixture)))
  in
  Alcotest.(check string)
    "derived A identity" (Z.to_string ma_hash_a)
    (Z.to_string (enclave_identity 0 (Machine_session.view after_einit)))

let ma_derived_hash_b () =
  let after_einit =
    Result.get_ok (Machine_session.step_n 18 (ma_session (read_case_study_file ma_fixture)))
  in
  Alcotest.(check string)
    "derived B identity" (Z.to_string ma_hash_b)
    (Z.to_string (enclave_identity 1 (Machine_session.view after_einit)))

let capability_register (key : string) (view : Machine_view.t) : Machine_view.capability =
  match Machine_view.find_register (register_id key) view with
  | Some { word = { capability = Some capability; _ }; _ } -> capability
  | Some register ->
      Alcotest.failf "case-study register %s is not a capability: %s" key register.word.detail_text
  | None -> Alcotest.failf "case-study register %s is absent" key

let capability_memory (address : int) (view : Machine_view.t) : Machine_view.capability =
  match Machine_view.find_memory_word (z address) view with
  | Some { capability = Some capability; _ } -> capability
  | Some word ->
      Alcotest.failf "case-study memory %d is not a capability: %s" address word.detail_text
  | None -> Alcotest.failf "case-study memory %d is absent" address

let check_capability (label : string) ~(permission : string) ~(base : int) ~(limit : int)
    ~(cursor : int) (capability : Machine_view.capability) : unit =
  Alcotest.(check (list string)) (label ^ " permission") [ permission ] capability.permissions;
  Alcotest.(check string) (label ^ " base") (string_of_int base) (Z.to_string capability.base);
  Alcotest.(check string) (label ^ " limit") (string_of_int limit) (Z.to_string capability.limit);
  Alcotest.(check string) (label ^ " cursor") (string_of_int cursor) (Z.to_string capability.cursor)

let overlaps (start : int) (limit : int) (capability : Machine_view.capability) : bool =
  Z.lt capability.base (z limit) && Z.lt (z start) capability.limit

let all_capabilities (view : Machine_view.t) : Machine_view.capability list =
  List.filter_map
    (fun (register : Machine_view.register) -> register.word.capability)
    view.registers
  @ List.filter_map (fun (cell : Machine_view.memory_cell) -> cell.word.capability) view.memory

let soc_setup_and_identity () =
  let initial = soc_session (read_case_study_file soc_fixture) in
  let initial_view = Machine_session.view initial in
  check_capability "initial PC" ~permission:"RWX" ~base:0 ~limit:39 ~cursor:0
    (capability_register "pc" initial_view);
  check_capability "initial r0" ~permission:"RWX" ~base:39 ~limit:76 ~cursor:39
    (capability_register "r0" initial_view);
  Alcotest.(check int) "fixed SOC memory layout" 92 (List.length initial_view.memory);
  check_capability "verifier link capability" ~permission:"RO" ~base:91 ~limit:92 ~cursor:91
    (capability_memory 37 initial_view);
  check_capability "verifier data capability" ~permission:"RWX" ~base:0 ~limit:39 ~cursor:37
    (capability_memory 38 initial_view);
  Alcotest.(check string) "enclave reserved cell" "0" (Z.to_string (integer_memory 53 initial_view));
  Alcotest.(check string)
    "first enclave data cell" "0"
    (Z.to_string (integer_memory 74 initial_view));
  Alcotest.(check string)
    "second enclave data cell" "0"
    (Z.to_string (integer_memory 75 initial_view));
  check_capability "assertion flag capability" ~permission:"RW" ~base:90 ~limit:91 ~cursor:90
    (capability_memory 89 initial_view);
  Alcotest.(check string)
    "initial assertion flag" "0"
    (Z.to_string (integer_memory 90 initial_view));
  check_capability "assertion link entry" ~permission:"E" ~base:76 ~limit:91 ~cursor:76
    (capability_memory 91 initial_view);
  let before_einit = Result.get_ok (Machine_session.step_n 10 initial) in
  let before_view = Machine_session.view before_einit in
  Alcotest.(check bool)
    "SOC remains running before EInit" true
    (before_view.status = Machine_view.Running);
  check_capability "pre-EInit PC" ~permission:"RWX" ~base:39 ~limit:53 ~cursor:45
    (capability_register "pc" before_view);
  check_capability "pre-EInit r3" ~permission:"RX" ~base:53 ~limit:74 ~cursor:39
    (capability_register "r3" before_view);
  check_capability "pre-EInit r0" ~permission:"RW" ~base:74 ~limit:76 ~cursor:39
    (capability_register "r0" before_view);
  let all_capabilities = all_capabilities before_view in
  Alcotest.(check int)
    "unique retained enclave-code authority" 1
    (List.length (List.filter (overlaps 53 74) all_capabilities));
  Alcotest.(check int)
    "unique retained enclave-data authority" 1
    (List.length (List.filter (overlaps 74 76) all_capabilities));
  let after_einit = Result.get_ok (Machine_session.step before_einit) in
  let after_view = Machine_session.view after_einit in
  Alcotest.(check bool)
    "SOC remains running after EInit" true
    (after_view.status = Machine_view.Running);
  match after_view.enclave_table with
  | Some { counter; entries = [ { id; identity } ] } ->
      Alcotest.(check string) "EInit counter" "1" (Z.to_string counter);
      Alcotest.(check string) "EInit table id" "0" (Z.to_string id);
      Alcotest.(check string) "derived SOC identity" (Z.to_string soc_hash) (Z.to_string identity)
  | Some table -> Alcotest.failf "EInit produced %d active entries" (List.length table.entries)
  | None -> Alcotest.fail "Cerisier enclave table is absent after EInit"

let soc_wrong_result_sets_flag () =
  let source =
    read_case_study_file soc_fixture
    |> replace_once ~needle:"    mov r5 42\n" ~replacement:"    mov r5 43\n"
  in
  let result = Machine_session.run ~max_steps:2_000 (soc_session source) in
  (match result.reason with
  | Machine_session.Halted -> ()
  | Failed -> Alcotest.fail "SOC result mismatch unexpectedly failed"
  | Step_limit -> Alcotest.fail "SOC result mismatch reached the step limit"
  | Breakpoint _ -> Alcotest.fail "SOC result mismatch hit a breakpoint"
  | Execution_error error ->
      Alcotest.failf "SOC result mismatch produced an execution error: %s"
        (Machine_backend.execution_error_message error));
  Alcotest.(check string)
    "SOC assertion failure flag" "1"
    (Z.to_string (integer_memory 90 (Machine_session.view result.session)))

let soc_rejects_wrong_hash () =
  let source =
    read_case_study_file soc_fixture
    |> replace_once
         ~needle:(Printf.sprintf "%%define SOC_HASH %s\n" (Z.to_string soc_hash))
         ~replacement:"%define SOC_HASH 0\n"
  in
  let result = Machine_session.run ~max_steps:2_000 (soc_session source) in
  match result.reason with
  | Machine_session.Failed ->
      Alcotest.(check (option string))
        "wrong identity reaches explicit fail" (Some "36")
        (Option.map Z.to_string (Machine_session.view result.session).pc)
  | Halted -> Alcotest.fail "wrong SOC identity unexpectedly halted"
  | Step_limit -> Alcotest.fail "wrong SOC identity reached the step limit"
  | Breakpoint _ -> Alcotest.fail "wrong SOC identity hit a breakpoint"
  | Execution_error error ->
      Alcotest.failf "wrong SOC identity produced an execution error: %s"
        (Machine_backend.execution_error_message error)

let soc_rejects_retained_pc_authority () =
  let source =
    read_case_study_file soc_fixture
    |> replace_once ~needle:"    subseg pc adversary_start adversary_end\n"
         ~replacement:"    subseg pc adversary_start soc_enclave_data_end\n"
  in
  let before_einit = Result.get_ok (Machine_session.step_n 10 (soc_session source)) in
  Alcotest.(check bool)
    "overlapping PC reaches EInit" true
    ((Machine_session.view before_einit).status = Machine_view.Running);
  let after_einit = Result.get_ok (Machine_session.step before_einit) in
  let view = Machine_session.view after_einit in
  Alcotest.(check bool)
    "EInit rejects overlapping PC authority" true
    (view.status = Machine_view.Failed);
  match view.enclave_table with
  | Some table ->
      Alcotest.(check string) "rejected EInit counter" "0" (Z.to_string table.counter);
      Alcotest.(check int) "rejected EInit table" 0 (List.length table.entries)
  | None -> Alcotest.fail "Cerisier enclave table is absent"

let ma_setup_and_identities () =
  let initial = ma_session (read_case_study_file ma_fixture) in
  let initial_view = Machine_session.view initial in
  check_capability "mutual initial PC" ~permission:"RWX" ~base:0 ~limit:73 ~cursor:0
    (capability_register "pc" initial_view);
  check_capability "mutual initial r0" ~permission:"RWX" ~base:73 ~limit:420 ~cursor:73
    (capability_register "r0" initial_view);
  Alcotest.(check int) "fixed mutual memory layout" 436 (List.length initial_view.memory);
  check_capability "mutual verifier link" ~permission:"RO" ~base:435 ~limit:436 ~cursor:435
    (capability_memory 72 initial_view);
  Alcotest.(check string) "A reserved cell" "0" (Z.to_string (integer_memory 109 initial_view));
  Alcotest.(check string) "B reserved cell" "0" (Z.to_string (integer_memory 280 initial_view));
  List.iter
    (fun address ->
      Alcotest.(check string)
        (Printf.sprintf "initial data/pad %d" address)
        "0"
        (Z.to_string (integer_memory address initial_view)))
    [ 277; 278; 279; 417; 418; 419 ];
  List.iter
    (fun address ->
      Alcotest.(check string)
        (Printf.sprintf "A table pre-hash %d" address)
        (Z.to_string (if address = 275 then ma_pre_a else ma_pre_b))
        (Z.to_string (integer_memory address initial_view)))
    [ 275; 276 ];
  List.iter
    (fun address ->
      Alcotest.(check string)
        (Printf.sprintf "B table pre-hash %d" address)
        (Z.to_string (if address = 415 then ma_pre_a else ma_pre_b))
        (Z.to_string (integer_memory address initial_view)))
    [ 415; 416 ];
  check_capability "mutual assertion flag capability" ~permission:"RW" ~base:434 ~limit:435
    ~cursor:434
    (capability_memory 433 initial_view);
  Alcotest.(check string)
    "mutual initial assertion flag" "0"
    (Z.to_string (integer_memory 434 initial_view));
  check_capability "mutual assertion link" ~permission:"E" ~base:420 ~limit:435 ~cursor:420
    (capability_memory 435 initial_view);
  let before_a = Result.get_ok (Machine_session.step_n 16 initial) in
  let before_view = Machine_session.view before_a in
  Alcotest.(check bool)
    "mutual remains running before A EInit" true
    (before_view.status = Machine_view.Running);
  check_capability "mutual pre-EInit PC" ~permission:"RWX" ~base:73 ~limit:109 ~cursor:85
    (capability_register "pc" before_view);
  check_capability "mutual pre-EInit A code" ~permission:"RX" ~base:109 ~limit:277 ~cursor:73
    (capability_register "r10" before_view);
  check_capability "mutual pre-EInit A data" ~permission:"RW" ~base:278 ~limit:280 ~cursor:73
    (capability_register "r11" before_view);
  check_capability "mutual pre-EInit B code" ~permission:"RX" ~base:280 ~limit:417 ~cursor:73
    (capability_register "r12" before_view);
  check_capability "mutual pre-EInit B data" ~permission:"RW" ~base:418 ~limit:420 ~cursor:73
    (capability_register "r0" before_view);
  let all_capabilities = all_capabilities before_view in
  List.iter
    (fun (label, start, limit) ->
      Alcotest.(check int)
        ("unique retained " ^ label ^ " authority")
        1
        (List.length (List.filter (overlaps start limit) all_capabilities)))
    [ ("A code", 109, 277); ("A data", 278, 280); ("B code", 280, 417); ("B data", 418, 420) ];
  let after_a = Result.get_ok (Machine_session.step before_a) in
  let after_a_view = Machine_session.view after_a in
  Alcotest.(check string)
    "derived live A identity" (Z.to_string ma_hash_a)
    (Z.to_string (enclave_identity 0 after_a_view));
  let after_b = Result.get_ok (Machine_session.step after_a) in
  let after_b_view = Machine_session.view after_b in
  Alcotest.(check string)
    "derived live B identity" (Z.to_string ma_hash_b)
    (Z.to_string (enclave_identity 1 after_b_view));
  match after_b_view.enclave_table with
  | Some table ->
      Alcotest.(check string) "mutual post-EInit counter" "2" (Z.to_string table.counter);
      Alcotest.(check int) "mutual post-EInit active table" 2 (List.length table.entries)
  | None -> Alcotest.fail "Cerisier enclave table is absent after mutual EInit"

let ma_wrong_confirmation_sets_flag () =
  let source =
    read_case_study_file ma_fixture
    |> replace_once ~needle:"    mov r5 1\n" ~replacement:"    mov r5 2\n"
  in
  let result = Machine_session.run ~max_steps:5_000 (ma_session source) in
  (match result.reason with
  | Machine_session.Halted -> ()
  | Failed -> Alcotest.fail "mutual confirmation mismatch unexpectedly failed"
  | Step_limit -> Alcotest.fail "mutual confirmation mismatch reached the step limit"
  | Breakpoint _ -> Alcotest.fail "mutual confirmation mismatch hit a breakpoint"
  | Execution_error error ->
      Alcotest.failf "mutual confirmation mismatch produced an execution error: %s"
        (Machine_backend.execution_error_message error));
  Alcotest.(check string)
    "mutual assertion failure flag" "1"
    (Z.to_string (integer_memory 434 (Machine_session.view result.session)))

let ma_rejects_wrong_hash () =
  let source =
    read_case_study_file ma_fixture
    |> replace_once
         ~needle:(Printf.sprintf "%%define MA_HASH_A %s\n" (Z.to_string ma_hash_a))
         ~replacement:"%define MA_HASH_A 0\n"
  in
  let result = Machine_session.run ~max_steps:5_000 (ma_session source) in
  Alcotest.(check bool)
    "mutual verifier rejects a wrong full identity" true
    (result.reason = Machine_session.Failed);
  Alcotest.(check (option string))
    "wrong mutual identity reaches explicit fail" (Some "12")
    (Option.map Z.to_string (Machine_session.view result.session).pc)

let ma_rejects_wrong_pre_hash () =
  let source =
    read_case_study_file ma_fixture
    |> replace_once
         ~needle:(Printf.sprintf "%%define MA_PRE_A %s\n" (Z.to_string ma_pre_a))
         ~replacement:"%define MA_PRE_A 0\n"
  in
  let result = Machine_session.run ~max_steps:5_000 (ma_session source) in
  Alcotest.(check bool)
    "mutual enclave rejects a wrong peer pre-hash" true
    (result.reason = Machine_session.Failed);
  Alcotest.(check (option string))
    "wrong mutual pre-hash reaches explicit fail" (Some "354")
    (Option.map Z.to_string (Machine_session.view result.session).pc)

let ma_rejects_retained_pc_authority () =
  let source =
    read_case_study_file ma_fixture
    |> replace_once ~needle:"    subseg pc adversary_start adversary_end\n"
         ~replacement:"    subseg pc adversary_start enclave_b_data_end\n"
  in
  let before_einit = Result.get_ok (Machine_session.step_n 16 (ma_session source)) in
  Alcotest.(check bool)
    "overlapping mutual PC reaches A EInit" true
    ((Machine_session.view before_einit).status = Machine_view.Running);
  let after_einit = Result.get_ok (Machine_session.step before_einit) in
  let view = Machine_session.view after_einit in
  Alcotest.(check bool)
    "A EInit rejects retained PC authority" true
    (view.status = Machine_view.Failed);
  match view.enclave_table with
  | Some table ->
      Alcotest.(check string) "rejected mutual EInit counter" "0" (Z.to_string table.counter);
      Alcotest.(check int) "rejected mutual EInit table" 0 (List.length table.entries)
  | None -> Alcotest.fail "Cerisier enclave table is absent"

let ts_derived_sensor_hash () =
  let source =
    read_case_study_file ts_fixture
    |> replace_once
         ~needle:(Printf.sprintf "%%define TS_SENSOR_HASH %s\n" (Z.to_string ts_sensor_hash))
         ~replacement:"%define TS_SENSOR_HASH 0\n"
    |> replace_once
         ~needle:(Printf.sprintf "%%define TS_CLIENT_HASH %s\n" (Z.to_string ts_client_hash))
         ~replacement:"%define TS_CLIENT_HASH 0\n"
  in
  let before_einit = Result.get_ok (Machine_session.step_n 22 (ts_session source)) in
  let after_einit = Result.get_ok (Machine_session.step before_einit) in
  Alcotest.(check string)
    "derived sensor identity" (Z.to_string ts_sensor_hash)
    (Z.to_string (enclave_identity 0 (Machine_session.view after_einit)))

let ts_derived_client_hash () =
  let source =
    read_case_study_file ts_fixture
    |> replace_once
         ~needle:(Printf.sprintf "%%define TS_CLIENT_HASH %s\n" (Z.to_string ts_client_hash))
         ~replacement:"%define TS_CLIENT_HASH 0\n"
  in
  let before_einit = Result.get_ok (Machine_session.step_n 61 (ts_session source)) in
  let after_einit = Result.get_ok (Machine_session.step before_einit) in
  Alcotest.(check string)
    "derived client identity" (Z.to_string ts_client_hash)
    (Z.to_string (enclave_identity 1 (Machine_session.view after_einit)))

let ts_setup_and_identities () =
  let initial = ts_session (read_case_study_file ts_fixture) in
  let initial_view = Machine_session.view initial in
  check_capability "sensor initial PC" ~permission:"RWX" ~base:0 ~limit:39 ~cursor:0
    (capability_register "pc" initial_view);
  check_capability "sensor initial r0" ~permission:"RWX" ~base:39 ~limit:164 ~cursor:39
    (capability_register "r0" initial_view);
  Alcotest.(check int) "fixed sensor memory layout" 180 (List.length initial_view.memory);
  check_capability "sensor verifier link" ~permission:"RO" ~base:179 ~limit:180 ~cursor:179
    (capability_memory 37 initial_view);
  check_capability "sensor verifier data" ~permission:"RWX" ~base:0 ~limit:39 ~cursor:37
    (capability_memory 38 initial_view);
  Alcotest.(check string) "sensor reserved cell" "0" (Z.to_string (integer_memory 72 initial_view));
  Alcotest.(check string) "client reserved cell" "0" (Z.to_string (integer_memory 116 initial_view));
  check_capability "sensor assertion flag capability" ~permission:"RW" ~base:178 ~limit:179
    ~cursor:178
    (capability_memory 177 initial_view);
  Alcotest.(check string)
    "sensor initial assertion flag" "0"
    (Z.to_string (integer_memory 178 initial_view));
  check_capability "sensor assertion link" ~permission:"E" ~base:164 ~limit:179 ~cursor:164
    (capability_memory 179 initial_view);

  let before_sensor = Result.get_ok (Machine_session.step_n 22 initial) in
  let before_sensor_view = Machine_session.view before_sensor in
  Alcotest.(check bool)
    "sensor remains running before first EInit" true
    (before_sensor_view.status = Machine_view.Running);
  check_capability "sensor pre-EInit PC" ~permission:"RWX" ~base:39 ~limit:72 ~cursor:57
    (capability_register "pc" before_sensor_view);
  check_capability "sensor pre-EInit code" ~permission:"RX" ~base:72 ~limit:114 ~cursor:40
    (capability_register "r10" before_sensor_view);
  check_capability "sensor pre-EInit data" ~permission:"RW" ~base:114 ~limit:116 ~cursor:114
    (capability_register "r0" before_sensor_view);
  check_capability "client pre-EInit code" ~permission:"RX" ~base:116 ~limit:161 ~cursor:41
    (capability_register "r11" before_sensor_view);
  check_capability "client pre-EInit data" ~permission:"RW" ~base:161 ~limit:163 ~cursor:42
    (capability_register "r12" before_sensor_view);
  check_capability "sensor pre-EInit MMIO" ~permission:"RW" ~base:163 ~limit:164 ~cursor:163
    (capability_register "r13" before_sensor_view);
  let capabilities = all_capabilities before_sensor_view in
  List.iter
    (fun (label, start, limit) ->
      Alcotest.(check int)
        ("unique retained " ^ label ^ " authority")
        1
        (List.length (List.filter (overlaps start limit) capabilities)))
    [
      ("sensor code", 72, 114);
      ("sensor data", 114, 116);
      ("client code", 116, 161);
      ("client data", 161, 163);
      ("sensor MMIO", 163, 164);
    ];

  let before_client = Result.get_ok (Machine_session.step_n 39 before_sensor) in
  let before_client_view = Machine_session.view before_client in
  Alcotest.(check bool)
    "sensor remains running before second EInit" true
    (before_client_view.status = Machine_view.Running);
  check_capability "client EInit PC" ~permission:"RX" ~base:39 ~limit:72 ~cursor:64
    (capability_register "pc" before_client_view);
  check_capability "client EInit code" ~permission:"RX" ~base:116 ~limit:161 ~cursor:41
    (capability_register "r11" before_client_view);
  check_capability "client EInit data" ~permission:"RW" ~base:161 ~limit:163 ~cursor:42
    (capability_register "r12" before_client_view);
  Alcotest.(check string)
    "initialized sensor MMIO" "21"
    (Z.to_string (integer_memory 163 before_client_view));
  let capabilities = all_capabilities before_client_view in
  List.iter
    (fun (label, start, limit) ->
      Alcotest.(check int)
        ("unique retained " ^ label ^ " authority before client EInit")
        1
        (List.length (List.filter (overlaps start limit) capabilities)))
    [ ("client code", 116, 161); ("client data", 161, 163) ];

  let after_client = Result.get_ok (Machine_session.step before_client) in
  let after_client_view = Machine_session.view after_client in
  Alcotest.(check string)
    "live sensor identity" (Z.to_string ts_sensor_hash)
    (Z.to_string (enclave_identity 0 after_client_view));
  Alcotest.(check string)
    "live client identity" (Z.to_string ts_client_hash)
    (Z.to_string (enclave_identity 1 after_client_view));
  match after_client_view.enclave_table with
  | Some table ->
      Alcotest.(check string) "sensor post-EInit counter" "2" (Z.to_string table.counter);
      Alcotest.(check int) "sensor post-EInit active table" 2 (List.length table.entries)
  | None -> Alcotest.fail "Cerisier enclave table is absent after sensor/client EInit"

let ts_wrong_result_sets_flag () =
  let source =
    read_case_study_file ts_fixture
    |> replace_once ~needle:"    mov r5 42\n" ~replacement:"    mov r5 43\n"
  in
  let result = Machine_session.run ~max_steps:5_000 (ts_session source) in
  (match result.reason with
  | Machine_session.Halted -> ()
  | Failed -> Alcotest.fail "sensor result mismatch unexpectedly failed"
  | Step_limit -> Alcotest.fail "sensor result mismatch reached the step limit"
  | Breakpoint _ -> Alcotest.fail "sensor result mismatch hit a breakpoint"
  | Execution_error error ->
      Alcotest.failf "sensor result mismatch produced an execution error: %s"
        (Machine_backend.execution_error_message error));
  Alcotest.(check string)
    "sensor assertion failure flag" "1"
    (Z.to_string (integer_memory 178 (Machine_session.view result.session)))

let ts_rejects_wrong_client_hash () =
  let source =
    read_case_study_file ts_fixture
    |> replace_once
         ~needle:(Printf.sprintf "%%define TS_CLIENT_HASH %s\n" (Z.to_string ts_client_hash))
         ~replacement:"%define TS_CLIENT_HASH 0\n"
  in
  let result = Machine_session.run ~max_steps:5_000 (ts_session source) in
  Alcotest.(check bool)
    "sensor verifier rejects a wrong client identity" true
    (result.reason = Machine_session.Failed);
  Alcotest.(check (option string))
    "wrong client identity reaches explicit fail" (Some "36")
    (Option.map Z.to_string (Machine_session.view result.session).pc)

let ts_rejects_wrong_sensor_hash () =
  let source =
    read_case_study_file ts_fixture
    |> replace_once
         ~needle:(Printf.sprintf "%%define TS_SENSOR_HASH %s\n" (Z.to_string ts_sensor_hash))
         ~replacement:"%define TS_SENSOR_HASH 0\n"
  in
  let result = Machine_session.run ~max_steps:5_000 (ts_session source) in
  Alcotest.(check bool)
    "sensor client rejects a wrong sensor identity" true
    (result.reason = Machine_session.Failed);
  Alcotest.(check (option string))
    "wrong sensor identity reaches explicit fail" (Some "160")
    (Option.map Z.to_string (Machine_session.view result.session).pc)

let ts_rejects_retained_pc_authority () =
  let source =
    read_case_study_file ts_fixture
    |> replace_once ~needle:"    subseg pc adversary_start adversary_end\n"
         ~replacement:"    mov pc pc\n"
  in
  let before_einit = Result.get_ok (Machine_session.step_n 22 (ts_session source)) in
  Alcotest.(check bool)
    "overlapping sensor PC reaches EInit" true
    ((Machine_session.view before_einit).status = Machine_view.Running);
  let after_einit = Result.get_ok (Machine_session.step before_einit) in
  let view = Machine_session.view after_einit in
  Alcotest.(check bool)
    "sensor EInit rejects retained PC authority" true
    (view.status = Machine_view.Failed);
  match view.enclave_table with
  | Some table ->
      Alcotest.(check string) "rejected sensor EInit counter" "0" (Z.to_string table.counter);
      Alcotest.(check int) "rejected sensor EInit table" 0 (List.length table.entries)
  | None -> Alcotest.fail "Cerisier enclave table is absent"

let ts_rejects_retained_mmio_authority () =
  let source =
    read_case_study_file ts_fixture
    |> replace_once ~needle:"    mov r13 0\n" ~replacement:"    mov r13 r13\n"
  in
  let result = Machine_session.run ~max_steps:5_000 (ts_session source) in
  Alcotest.(check bool)
    "sensor rejects retained MMIO authority" true
    (result.reason = Machine_session.Failed);
  Alcotest.(check (option string))
    "retained MMIO authority reaches sensor fail" (Some "113")
    (Option.map Z.to_string (Machine_session.view result.session).pc)

let case_studies =
  [
    ( "secure outsourced computation",
      {
        fixture = "secure_outsourced_computation.s";
        regfile = soc_regfile;
        max_steps = 2_000;
        registers = [ ("r30", 0) ];
        memory = [ (90, 0) ];
        enclave_counter = 1;
        active_enclaves = 1;
        enclave_identities = [ soc_hash ];
      } );
    ( "mutual attestation",
      {
        fixture = ma_fixture;
        regfile = ma_regfile;
        max_steps = 5_000;
        registers = [ ("r28", 0); ("r29", 0); ("r30", 0) ];
        memory = [ (434, 0) ];
        enclave_counter = 2;
        active_enclaves = 2;
        enclave_identities = [ ma_hash_a; ma_hash_b ];
      } );
    ( "trusted sensor readout",
      {
        fixture = ts_fixture;
        regfile = ts_regfile;
        max_steps = 5_000;
        registers = [ ("r0", 0); ("r1", 0); ("r2", 0); ("r3", 0); ("r4", 0); ("r5", 0); ("r30", 0) ];
        memory = [ (163, 21); (178, 0) ];
        enclave_counter = 2;
        active_enclaves = 2;
        enclave_identities = [ ts_sensor_hash; ts_client_hash ];
      } );
  ]

let () =
  Alcotest.run "cerisier backend"
    [
      ("syntax", [ Alcotest.test_case "parser and printer" `Quick parser_and_printer ]);
      ("codec", [ Alcotest.test_case "instruction round trips" `Quick codec ]);
      ( "machine",
        [
          Alcotest.test_case "initialization and view" `Quick initialization_and_view;
          Alcotest.test_case "hashing" `Quick hashing;
          Alcotest.test_case "uniqueness" `Quick uniqueness;
          Alcotest.test_case "attestation" `Quick attestation;
          Alcotest.test_case "full enclave example" `Quick enclave_example;
        ] );
      ( "paper case studies",
        List.map
          (fun (name, expected) -> Alcotest.test_case name `Quick (run_case_study expected))
          case_studies
        @ [
            Alcotest.test_case "SOC setup and concrete identity" `Quick soc_setup_and_identity;
            Alcotest.test_case "SOC result mismatch sets the assertion flag" `Quick
              soc_wrong_result_sets_flag;
            Alcotest.test_case "SOC rejects a wrong identity" `Quick soc_rejects_wrong_hash;
            Alcotest.test_case "SOC rejects retained PC authority" `Quick
              soc_rejects_retained_pc_authority;
            Alcotest.test_case "mutual A pre-hash derivation" `Quick ma_derived_pre_a;
            Alcotest.test_case "mutual B pre-hash derivation" `Quick ma_derived_pre_b;
            Alcotest.test_case "mutual A identity derivation" `Quick ma_derived_hash_a;
            Alcotest.test_case "mutual B identity derivation" `Quick ma_derived_hash_b;
            Alcotest.test_case "mutual setup and concrete identities" `Quick ma_setup_and_identities;
            Alcotest.test_case "mutual confirmation mismatch sets the assertion flag" `Quick
              ma_wrong_confirmation_sets_flag;
            Alcotest.test_case "mutual rejects a wrong full identity" `Quick ma_rejects_wrong_hash;
            Alcotest.test_case "mutual rejects a wrong peer pre-hash" `Quick
              ma_rejects_wrong_pre_hash;
            Alcotest.test_case "mutual rejects retained PC authority" `Quick
              ma_rejects_retained_pc_authority;
            Alcotest.test_case "sensor identity derivation" `Quick ts_derived_sensor_hash;
            Alcotest.test_case "client identity derivation" `Quick ts_derived_client_hash;
            Alcotest.test_case "sensor setup and concrete identities" `Quick ts_setup_and_identities;
            Alcotest.test_case "sensor result mismatch sets the assertion flag" `Quick
              ts_wrong_result_sets_flag;
            Alcotest.test_case "sensor rejects a wrong client identity" `Quick
              ts_rejects_wrong_client_hash;
            Alcotest.test_case "sensor rejects a wrong sensor identity" `Quick
              ts_rejects_wrong_sensor_hash;
            Alcotest.test_case "sensor rejects retained PC authority" `Quick
              ts_rejects_retained_pc_authority;
            Alcotest.test_case "sensor rejects retained MMIO authority" `Quick
              ts_rejects_retained_mmio_authority;
          ] );
    ]
