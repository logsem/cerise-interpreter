open Libinterp
open Libinterp.Machine
open Libinterp.Ast

let make_test_list (dir : string) : string array =
  try Sys.readdir dir with Failure _ -> raise Sys.Break

let z_tst = Alcotest.testable Z.pp_print Z.equal

let state_tst =
  Alcotest.testable
    (Fmt.of_to_string (fun (st : exec_state) ->
         match st with Running -> "Running" | Failed -> "Failed" | Halted -> "Halted"))
    (fun a b -> a = b)

let perm_tst =
  Alcotest.testable (Fmt.of_to_string @@ Pretty_printer.string_of_fperm) (fun a b ->
      PermSet.equal a b)

let locality_tst =
  Alcotest.testable (Fmt.of_to_string @@ Pretty_printer.string_of_locality) (fun a b -> a = b)

(* TODO I should add a try/catch in case of parsing failure *)
(* TODO also test multiple flags configurations *)
let run_prog (filename : string) : mchn =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  let parse_res = Ir.translate_prog @@ Parser.main Lexer.token filebuf in
  let _ = close_in input in

  let _ = Parameters.flags := Parameters.griotte in

  let init_regs = Machine.init_reg_state in
  let init_mems = Machine.init_mem_state Z.(~$0) parse_res in
  let m = Machine.init init_regs init_mems in

  run m

let test_const_word expected actual _ = Alcotest.(check z_tst) "Integers match" expected actual
let test_state expected actual _ = Alcotest.(check state_tst) "States match" expected actual
let test_perm expected actual _ = Alcotest.(check perm_tst) "Permission match" expected actual
let test_locality expected actual _ = Alcotest.(check locality_tst) "Locality match" expected actual

let get_reg_int_word (r : Ast.regname) (m : mchn) (d : Z.t) =
  match r @! snd m with I z -> z | _ -> d

let get_reg_cap_perm (r : regname) (m : mchn) (d : PermSet.t) =
  match r @! snd m with Sealable (Cap (p, _, _, _, _)) -> p | _ -> d

let get_reg_cap_locality (r : regname) (m : mchn) (l : locality) =
  match r @! snd m with Sealable (Cap (_, g, _, _, _)) -> g | _ -> l

let test_path s = "../../../tests/test_files/default/" ^ s

let test_negatives =
  let open Alcotest in
  let path = test_path "neg/" in
  let test_names = make_test_list path in
  Array.to_list
  @@ Array.map
       (fun t ->
         test_case
           (Printf.sprintf "%s should end in Failed state" t)
           `Quick
           (test_state Failed (fst @@ run_prog (path ^ t))))
       test_names

let test_mov_test =
  let open Alcotest in
  let m = run_prog (test_path "pos/mov_test.s") in
  let pc_a =
    match get_reg PC @@ snd m with Sealable (Cap (_, _, _, _, a)) -> a | _ -> Z.(~$(-1))
  in
  let r2_res = match Reg 2 @! snd m with I z -> z | _ -> Z.zero in
  let r5_res = match Reg 5 @! snd m with I z -> z | _ -> Z.zero in
  [
    test_case "mov_test.s should end in halted state" `Quick (test_state Halted (fst m));
    test_case "mov_test.s PC should point to address 13" `Quick (fun _ ->
        check int "Ints match" 13 (Z.to_int pc_a));
    test_case "mov_test.s R2 should contain 28" `Quick (test_const_word Z.(~$28) r2_res);
    test_case "mov_test.s R5 should contain -30" `Quick (test_const_word Z.(~$(-30)) r5_res);
  ]

let test_jmper =
  let open Alcotest in
  let m = run_prog (test_path "pos/jmper.s") in
  [
    test_case "jmper.s should end in halted state" `Quick (test_state Halted (fst m));
    test_case "jmper.s should end with r2 containing 12" `Quick
      (test_const_word Z.(~$12) (get_reg_int_word (Ast.Reg 2) m Z.zero));
  ]

let test_jmper_jalr =
  let open Alcotest in
  let m = run_prog (test_path "pos/jmper_jalr.s") in
  [
    test_case "jmper_jalr.s should end in halted state" `Quick (test_state Halted (fst m));
    test_case "jmper_jalr.s should end with r2 containing 12" `Quick
      (test_const_word Z.(~$12) (get_reg_int_word (Ast.Reg 2) m Z.zero));
  ]

let test_locality_flow =
  let open Alcotest in
  let m = run_prog (test_path "pos/test_locality_flow.s") in
  [ test_case "test_locality.s should end in halted state" `Quick (test_state Halted (fst m)) ]

let test_getotype =
  let open Alcotest in
  let m = run_prog (test_path "pos/get_otype.s") in
  [
    test_case "get_otype.s should end in halted state" `Quick (test_state Halted (fst m));
    test_case "get_otype.s should end with r0 containing (-1)" `Quick
      (test_const_word Z.(~$(-1)) (get_reg_int_word (Ast.Reg 0) m Z.zero));
    test_case "get_otype.s should end with r1 containing (-1)" `Quick
      (test_const_word Z.(~$(-1)) (get_reg_int_word (Ast.Reg 1) m Z.zero));
    test_case "get_otype.s should end with r2 containing (-1)" `Quick
      (test_const_word Z.(~$(-1)) (get_reg_int_word (Ast.Reg 2) m Z.zero));
    test_case "get_otype.s should end with r3 containing 10" `Quick
      (test_const_word Z.(~$10) (get_reg_int_word (Ast.Reg 3) m Z.zero));
  ]

let test_deep_local =
  let open Alcotest in
  let m = run_prog (test_path "pos/deep_local.s") in
  [
    test_case "deep_local.s should end in halted state" `Quick (test_state Halted (fst m));
    test_case "deep_local.s should contain Local locality in r1" `Quick
      (test_locality Local (get_reg_cap_locality (Reg 1) m Global));
    test_case "deep_local.s should contain (RW-DL) permission in r1" `Quick
      (test_perm (PermSet.of_list [ R; W; DL ]) (get_reg_cap_perm (Reg 1) m PermSet.empty));
    test_case "deep_local.s should contain Local locality in r2" `Quick
      (test_locality Local (get_reg_cap_locality (Reg 2) m Global));
    test_case "deep_local.s should contain (RW-DL) permission in r2" `Quick
      (test_perm (PermSet.of_list [ R; W; DL ]) (get_reg_cap_perm (Reg 2) m PermSet.empty));
  ]

let test_deep_ro =
  let open Alcotest in
  let m = run_prog (test_path "pos/deep_ro.s") in
  [
    test_case "deep_ro.s should end in halted state" `Quick (test_state Halted (fst m));
    test_case "deep_ro.s should contain (R-DI) permission in r1" `Quick
      (test_perm (PermSet.of_list [ R; DI ]) (get_reg_cap_perm (Reg 1) m PermSet.empty));
    test_case "deep_ro.s should contain (R-DI) permission in r2" `Quick
      (test_perm (PermSet.of_list [ R; DI ]) (get_reg_cap_perm (Reg 2) m PermSet.empty));
  ]

let test_getwtype =
  let open Alcotest in
  let m = run_prog (test_path "pos/get_wtype.s") in
  [
    test_case "get_wtype.s should end in halted state" `Quick (test_state Halted (fst m));
    test_case "get_wtype.s should end with r0 containing 0" `Quick
      (test_const_word Z.zero (get_reg_int_word (Ast.Reg 0) m Z.zero));
    test_case "get_wtype.s should end with r1 containing 0" `Quick
      (test_const_word Z.zero (get_reg_int_word (Ast.Reg 1) m Z.zero));
    test_case "get_wtype.s should end with r2 containing 0" `Quick
      (test_const_word Z.zero (get_reg_int_word (Ast.Reg 2) m Z.zero));
    test_case "get_wtype.s should end with r3 containing 0" `Quick
      (test_const_word Z.zero (get_reg_int_word (Ast.Reg 3) m Z.zero));
  ]

let test_sealing =
  let open Alcotest in
  let m = run_prog (test_path "pos/seal_unseal.s") in
  [ test_case "seal_unseal.s should end in halted state" `Quick (test_state Halted (fst m)) ]

let test_sealing_counter =
  let open Alcotest in
  let m = run_prog (test_path "pos/sealing_counter.s") in
  [
    test_case "sealing_counter.s should end in halted state" `Quick (test_state Halted (fst m));
    test_case "sealing_counter.s should end with r2 containing 4" `Quick
      (test_const_word Z.(~$4) (get_reg_int_word (Ast.Reg 2) m Z.zero));
  ]

let () =
  let open Alcotest in
  run "Run"
    [
      ( "Pos",
        test_mov_test @ test_jmper @ test_jmper_jalr @ test_locality_flow @ test_getotype
        @ test_getwtype @ test_deep_local @ test_deep_ro @ test_sealing @ test_sealing_counter );
      ("Neg", test_negatives);
    ]
