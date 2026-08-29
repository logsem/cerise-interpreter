(* Baseline contracts for the profiles selected by the CLI.  These deliberately
   record current behaviour, including the distinction between parser gates and
   instruction decoding gates, so backend work has a concrete compatibility
   target. *)

open Cerise
open Ast
module Machine = Cerise.Legacy_machine

module Encode = Cerise_internal.Encode

type profile = {
  cli_name : string;
  flags : Parameters.machineFlags;
  sealing : bool;
  stack : bool;
  uninitialized : bool;
  locality : locality;
}

let profiles =
  [
    { cli_name = "default"; flags = Parameters.full_cerise; sealing = true; stack = true;
      uninitialized = true; locality = Directed };
    { cli_name = "vanilla"; flags = Parameters.vanilla_cerise; sealing = false; stack = false;
      uninitialized = false; locality = Global };
    { cli_name = "ucerise"; flags = Parameters.stack_cerise; sealing = false; stack = true;
      uninitialized = true; locality = Local };
    { cli_name = "mcerise"; flags = Parameters.mcerise; sealing = false; stack = true;
      uninitialized = true; locality = Directed };
    { cli_name = "seal_cerise"; flags = Parameters.sealing_cerise; sealing = true; stack = false;
      uninitialized = false; locality = Global };
    { cli_name = "custom"; flags = Parameters.custom_cerise; sealing = false; stack = false;
      uninitialized = false; locality = Global };
  ]

let with_profile profile f =
  let previous = !Parameters.flags in
  Fun.protect ~finally:(fun () -> Parameters.flags := previous) (fun () ->
      Parameters.flags := profile.flags;
      f ())

let locality_name = function Global -> "Global" | Local -> "Local" | Directed -> "Directed"

let expect_parse expected source =
  match (expected, Program.parse_prog_from_string source) with
  | true, Ok _ | false, Error _ -> ()
  | true, Error message -> Alcotest.failf "expected parser acceptance, got: %s" message
  | false, Ok _ -> Alcotest.fail "expected parser rejection"

let decode_succeeds op =
  match Encode.decode_machine_op (Encode.encode_machine_op op) with
  | _ -> true
  | exception Encode.DecodeException _ -> false

let initial_machine profile =
  let max_addr = Parameters.get_max_addr () in
  let stk_addr = if profile.stack then Z.div max_addr (Z.of_int 2) else Z.zero in
  let regs = Machine.init_reg_state stk_addr in
  let machine = Machine.init regs (Machine.init_mem_state Z.zero []) in
  (stk_addr, regs, machine)

let parse_program source =
  match Program.parse_prog_from_string source with
  | Ok program -> program
  | Error message -> Alcotest.fail message

let run_source profile source =
  let _, regs, _ = initial_machine profile in
  let mem = Machine.init_mem_state Z.zero (parse_program source) in
  Machine.run (Machine.init regs mem)

let int_reg reg machine =
  match Machine.read_reg reg machine with
  | I value -> value
  | word -> Alcotest.failf "expected integer in register, got %s" (Pretty_printer.string_of_word word)

let test_profile_shape profile () =
  with_profile profile (fun () ->
      Alcotest.(check string) "profile version" profile.flags.version (!Parameters.flags).version;
      Alcotest.(check bool) "sealing flag" profile.sealing (!Parameters.flags).sealing;
      Alcotest.(check bool) "stack flag" profile.stack (!Parameters.flags).stack;
      Alcotest.(check bool) "uninitialized flag" profile.uninitialized
        (!Parameters.flags).unitialized;
      Alcotest.(check string) "minimum locality" (locality_name profile.locality)
        (locality_name (!Parameters.flags).locality))

let test_initial_state_and_display profile () =
  with_profile profile (fun () ->
      let stk_addr, regs, machine = initial_machine profile in
      Alcotest.(check int) "initial register count" (if profile.stack then 33 else 32)
        (Machine.RegMap.cardinal regs);
      Alcotest.(check bool) "PC has the complete heap authority" true
        (match Machine.read_reg PC machine with
        | Sealable (Cap (RWX, Global, base, ending, address)) ->
            Z.equal base Z.zero && Z.equal address Z.zero
            && Z.equal ending (if profile.stack then stk_addr else Parameters.get_max_addr ())
        | _ -> false);
      Alcotest.(check string) "stack register's displayed name"
        (if profile.stack then "stk" else "r31") (Pretty_printer.string_of_regname stk);
      Alcotest.(check bool) "stack register presence" profile.stack (Machine.RegMap.mem stk regs);
      Alcotest.(check bool) "sealing register replaces ddc" profile.sealing
        (match Machine.read_reg ddc machine with Sealable (SealRange _) -> true | _ -> false);
      if profile.stack then
        Alcotest.(check bool) "stack capability matches flags" true
          (match Machine.read_reg stk machine with
          | Sealable (Cap (perm, locality, base, ending, address)) ->
              perm = (if profile.uninitialized then URWLX else RWLX)
              && locality = profile.locality && Z.equal base stk_addr
              && Z.equal ending (Parameters.get_max_addr ()) && Z.equal address stk_addr
          | _ -> false))

let test_parser_gates profile () =
  with_profile profile (fun () ->
      expect_parse true "mov r1 1\nhalt";
      expect_parse profile.stack "getl r1 pc";
      expect_parse profile.sealing "seal r1 r0 r0";
      expect_parse profile.uninitialized "promoteU r1";
      (* Current parser behaviour: Local and Directed syntax is accepted by both
         Local and Directed profiles; Global profiles reject it during IR translation. *)
      expect_parse (profile.locality <> Global) "restrict r1 (RW, DIRECTED)";
      expect_parse (profile.locality <> Global) "restrict r1 RWL";
      expect_parse profile.uninitialized "restrict r1 URW")

let test_encoder_gates profile () =
  with_profile profile (fun () ->
      Alcotest.(check bool) "common instruction decodes" true (decode_succeeds Halt);
      Alcotest.(check bool) "GetL decode gate" profile.stack
        (decode_succeeds (GetL (Reg 1, PC)));
      Alcotest.(check bool) "sealing decode gate" profile.sealing
        (decode_succeeds (Seal (Reg 1, Reg 0, Reg 0)));
      Alcotest.(check bool) "uninitialized decode gate" profile.uninitialized
        (decode_succeeds (PromoteU (Reg 1))))

let test_common_execution profile () =
  with_profile profile (fun () ->
      let machine = run_source profile "mov r1 41\nadd r2 r1 1\nhalt" in
      Alcotest.(check bool) "program halts" true (Machine.get_exec_state machine = Machine.Halted);
      Alcotest.(check string) "arithmetic result" "42" (Z.to_string (int_reg (Reg 2) machine)))

let test_feature_execution profile () =
  with_profile profile (fun () ->
      if profile.stack then (
        let machine = run_source profile "getl r1 stk\nhalt" in
        Alcotest.(check bool) "GetL program halts" true
          (Machine.get_exec_state machine = Machine.Halted);
        Alcotest.(check bool) "GetL returns the stack locality encoding" true
          (Z.equal (int_reg (Reg 1) machine) (Encode.encode_locality profile.locality)));
      if profile.uninitialized then (
        let stk_addr, regs, _ = initial_machine profile in
        let regs =
          Machine.RegMap.add (Reg 1)
            (Sealable (Cap (URW, profile.locality, Z.zero, stk_addr, stk_addr))) regs
        in
        let machine = Machine.run (Machine.init regs (Machine.init_mem_state Z.zero [ Op (PromoteU (Reg 1)); Op Halt ])) in
        Alcotest.(check bool) "PromoteU promotes and truncates" true
          (match Machine.read_reg (Reg 1) machine with
          | Sealable (Cap (RW, locality, base, ending, address)) ->
              locality = profile.locality && Z.equal base Z.zero && Z.equal ending stk_addr
              && Z.equal address stk_addr
          | _ -> false));
      if profile.sealing then (
        let machine = run_source profile "getotype r1 r0\nhalt" in
        Alcotest.(check bool) "GetOType program halts" true
          (Machine.get_exec_state machine = Machine.Halted);
        Alcotest.(check string) "unsealed words have otype -1" "-1"
          (Z.to_string (int_reg (Reg 1) machine))))

let make_profile_tests profile =
  [
    Alcotest.test_case "flags" `Quick (test_profile_shape profile);
    Alcotest.test_case "initial state and terminal display" `Quick (test_initial_state_and_display profile);
    Alcotest.test_case "parser acceptance and rejection" `Quick (test_parser_gates profile);
    Alcotest.test_case "instruction encoding and decode gates" `Quick (test_encoder_gates profile);
    Alcotest.test_case "common execution transition" `Quick (test_common_execution profile);
    Alcotest.test_case "enabled feature execution" `Quick (test_feature_execution profile);
  ]

let () = Alcotest.run "Baseline characterization" (List.map (fun profile -> (profile.cli_name, make_profile_tests profile)) profiles)
