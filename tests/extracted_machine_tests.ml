open Cerise_internal.Ast

module M = Cerise_internal.Machine_extracted

let z = Z.of_int

let machine_with program =
  M.init M.init_reg_state M.init_sreg_state_zeros (M.init_mem_state Z.zero program)

let check_state expected machine =
  Alcotest.(check bool) "execution state" true (M.get_exec_state machine = expected)

let step_exn machine = Option.get (M.step machine)

let test_halt () =
  machine_with [ Op Halt ] |> step_exn |> check_state M.Halted

let test_move () =
  let machine = machine_with [ Op (Move (Reg 1, Const (z 42))) ] |> step_exn in
  check_state M.Running machine;
  Alcotest.(check bool) "destination register" true (M.read_reg (Reg 1) machine = I (z 42))

let test_missing_instruction () =
  machine_with [] |> step_exn |> check_state M.Failed

let test_rem_is_unsupported () =
  machine_with [ Op (Rem (Reg 1, Const (z 5), Const (z 2))) ]
  |> step_exn |> check_state M.Failed

let test_sparse_read_api () =
  let machine = machine_with [] in
  Alcotest.(check bool)
    "inspection keeps the legacy zero fill" true
    (M.read_mem (z 10) machine = Some (I Z.zero))

let test_direct_map_update_and_snapshot () =
  let machine = machine_with [] |> M.set_mem (z 10) (I (z 7)) in
  Alcotest.(check bool)
    "direct lookup sees inserted word" true
    (M.read_mem (z 10) machine = Some (I (z 7)));
  Alcotest.(check bool)
    "whole-map snapshot sees inserted word" true
    (M.MemMap.find_opt (z 10) (M.get_memory machine) = Some (I (z 7)))

let test_out_of_griotte_range () =
  machine_with [ Op Halt ]
  |> M.set_mem (z 2_000_000) (I Z.zero)
  |> step_exn |> check_state M.Failed

let () =
  Alcotest.run "extracted machine"
    [
      ( "step",
        [
          Alcotest.test_case "halt" `Quick test_halt;
          Alcotest.test_case "move" `Quick test_move;
          Alcotest.test_case "missing instruction" `Quick test_missing_instruction;
          Alcotest.test_case "unsupported remainder" `Quick test_rem_is_unsupported;
          Alcotest.test_case "sparse inspection" `Quick test_sparse_read_api;
          Alcotest.test_case "direct map update and snapshot" `Quick
            test_direct_map_update_and_snapshot;
          Alcotest.test_case "out-of-range configuration" `Quick test_out_of_griotte_range;
        ] );
    ]
