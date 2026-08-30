open Cerise

module Codec_fixture = struct
  type register = R0 | R1 | R2 | Invalid_register
  type operand = (register, Z.t) Instruction_codec.register_or_constant

  type instruction =
    | Halt
    | Move of register * operand
    | Calculate of register * operand * operand
    | Signed_pair of Z.t * Z.t
    | Marker
    | Ghost

  let register_codec = Instruction_codec.enum ~name:"fixture register" [ R0; R1; R2 ]
  let operand_codec = Instruction_codec.register_or_constant register_codec Instruction_codec.zarith

  let encoding_patterns =
    [
      Instruction_codec.encoding_pattern ~name:"halt" Instruction_codec.unit
        ~construct:(fun () -> Halt)
        ~project:(function Halt -> Some () | _ -> None);
      Instruction_codec.encoding_pattern ~name:"move"
        (Instruction_codec.pair (Instruction_codec.register register_codec) operand_codec)
        ~construct:(fun (register, operand) -> Move (register, operand))
        ~project:(function Move (register, operand) -> Some (register, operand) | _ -> None);
      Instruction_codec.encoding_pattern ~name:"calculate"
        (Instruction_codec.triple
           (Instruction_codec.register register_codec)
           operand_codec operand_codec)
        ~construct:(fun (register, first, second) -> Calculate (register, first, second))
        ~project:(function
          | Calculate (register, first, second) -> Some (register, first, second) | _ -> None);
      Instruction_codec.encoding_pattern ~name:"signed-pair"
        (Instruction_codec.pair
           (Instruction_codec.scalar Instruction_codec.zarith)
           (Instruction_codec.scalar Instruction_codec.zarith))
        ~construct:(fun (first, second) -> Signed_pair (first, second))
        ~project:(function Signed_pair (first, second) -> Some (first, second) | _ -> None);
      Instruction_codec.encoding_pattern ~name:"marker" Instruction_codec.unit
        ~construct:(fun () -> Marker)
        ~project:(function Marker -> Some () | _ -> None);
    ]

  let codec = Instruction_codec.compile encoding_patterns |> Result.get_ok
end

let encoded_opcode (codec : 'a Instruction_codec.t) (value : 'a) : int =
  Instruction_codec.encode codec value |> Result.get_ok |> fun encoded ->
  Z.to_int (Z.extract encoded 0 8)

let check_round_trip (instruction : Codec_fixture.instruction) : unit =
  let encoded = Instruction_codec.encode Codec_fixture.codec instruction |> Result.get_ok in
  let decoded = Instruction_codec.decode Codec_fixture.codec encoded |> Result.get_ok in
  Alcotest.(check bool) "codec round trip" true (instruction = decoded)

let test_declaration_order_and_variants (() : unit) : unit =
  let open Codec_fixture in
  let register = Instruction_codec.Register R1 in
  let constant = Instruction_codec.Constant (Z.of_int (-7)) in
  List.iter
    (fun (instruction, expected_opcode) ->
      Alcotest.(check int)
        "declaration-order opcode" expected_opcode
        (encoded_opcode codec instruction))
    [
      (Halt, 0);
      (Move (R0, register), 1);
      (Move (R0, constant), 2);
      (Calculate (R2, register, register), 3);
      (Calculate (R2, register, constant), 4);
      (Calculate (R2, constant, register), 5);
      (Calculate (R2, constant, constant), 6);
      (Signed_pair (Z.one, Z.one), 7);
      (Marker, 8);
    ];
  let marker =
    Instruction_codec.encoding_pattern ~name:"marker" Instruction_codec.unit
      ~construct:(fun () -> Marker)
      ~project:(function Marker -> Some () | _ -> None)
  in
  let halt =
    Instruction_codec.encoding_pattern ~name:"halt" Instruction_codec.unit
      ~construct:(fun () -> Halt)
      ~project:(function Halt -> Some () | _ -> None)
  in
  let reordered = Instruction_codec.compile [ marker; halt ] |> Result.get_ok in
  Alcotest.(check int) "first declaration starts at zero" 0 (encoded_opcode reordered Marker);
  Alcotest.(check int) "second declaration follows" 1 (encoded_opcode reordered Halt)

let test_complete_round_trips (() : unit) : unit =
  let open Codec_fixture in
  let operands = [ Instruction_codec.Register R1; Instruction_codec.Constant (Z.of_int (-7)) ] in
  check_round_trip Halt;
  check_round_trip Marker;
  List.iter (fun operand -> check_round_trip (Move (R0, operand))) operands;
  List.iter
    (fun first ->
      List.iter (fun second -> check_round_trip (Calculate (R2, first, second))) operands)
    operands;
  check_round_trip (Signed_pair (Z.of_int (-123456), Z.of_int 987654));
  let bit_count = 100_000 in
  let positive = Z.logor (Z.shift_left Z.one bit_count) (Z.of_int 0x5a) in
  let negative = Z.neg (Z.logor (Z.shift_left Z.one (bit_count + 37)) (Z.of_int 0xa5)) in
  check_round_trip (Signed_pair (negative, positive))

let compile_without_exception (type instruction) (name : string)
    (patterns : instruction Instruction_codec.encoding_pattern list) :
    (instruction Instruction_codec.t, Instruction_codec.error list) result =
  match Instruction_codec.compile patterns with
  | result -> result
  | exception exn ->
      Alcotest.failf "%s raised %s instead of returning a structured error" name
        (Printexc.to_string exn)

let halt_pattern (type operand) ~(name : string)
    (operand_codec : operand Instruction_codec.operand_codec) :
    Codec_fixture.instruction Instruction_codec.encoding_pattern =
  Instruction_codec.encoding_pattern ~name operand_codec
    ~construct:(fun _ -> Codec_fixture.Halt)
    ~project:(fun _ -> None)

let test_structured_failures (() : unit) : unit =
  let module C = Codec_fixture in
  Alcotest.(check bool)
    "negative encoding" true
    (match Instruction_codec.decode C.codec Z.minus_one with
    | Error (Instruction_codec.Negative_encoding value) -> Z.equal value Z.minus_one
    | _ -> false);
  Alcotest.(check bool)
    "malformed payload" true
    (match Instruction_codec.decode C.codec (Z.shift_left Z.one 8) with
    | Error (Instruction_codec.Malformed_encoding { pattern_name = "halt"; _ }) -> true
    | _ -> false);
  Alcotest.(check bool)
    "malformed register operand" true
    (match Instruction_codec.decode C.codec Z.(of_int 1 + shift_left (of_int 99) 8) with
    | Error (Instruction_codec.Malformed_encoding { pattern_name = "move"; _ }) -> true
    | _ -> false);
  Alcotest.(check bool)
    "unknown opcode" true
    (match Instruction_codec.decode C.codec (Z.of_int 250) with
    | Error (Instruction_codec.Unknown_opcode 250) -> true
    | _ -> false);
  Alcotest.(check bool)
    "unrecognized instruction" true
    (Instruction_codec.encode C.codec C.Ghost = Error Instruction_codec.Unrecognized_instruction);
  Alcotest.(check bool)
    "invalid operand" true
    (match
       Instruction_codec.encode C.codec
         (C.Move (C.Invalid_register, Instruction_codec.Register C.R0))
     with
    | Error (Instruction_codec.Invalid_operand { pattern_name = "move"; _ }) -> true
    | _ -> false);
  let duplicate =
    [
      halt_pattern ~name:"duplicate" Instruction_codec.unit;
      halt_pattern ~name:"duplicate" Instruction_codec.unit;
    ]
  in
  Alcotest.(check bool)
    "duplicate pattern names rejected" true
    (match Instruction_codec.compile duplicate with
    | Error errors ->
        List.exists
          (function Instruction_codec.Duplicate_pattern_name "duplicate" -> true | _ -> false)
          errors
    | Ok _ -> false);
  let first =
    Instruction_codec.encoding_pattern ~name:"first" Instruction_codec.unit
      ~construct:(fun () -> C.Halt)
      ~project:(function C.Halt -> Some () | _ -> None)
  in
  let second =
    Instruction_codec.encoding_pattern ~name:"second" Instruction_codec.unit
      ~construct:(fun () -> C.Halt)
      ~project:(function C.Halt -> Some () | _ -> None)
  in
  let ambiguous = Instruction_codec.compile [ first; second ] |> Result.get_ok in
  Alcotest.(check bool)
    "ambiguous projection" true
    (Instruction_codec.encode ambiguous C.Halt
    = Error (Instruction_codec.Ambiguous_instruction [ "first"; "second" ]))

let test_safe_opcode_exhaustion (() : unit) : unit =
  let automatic_overflow =
    List.init 257 (fun index ->
        halt_pattern ~name:(Printf.sprintf "pattern-%d" index) Instruction_codec.unit)
  in
  Alcotest.(check bool)
    "opcode space exhaustion rejected" true
    (match compile_without_exception "opcode exhaustion" automatic_overflow with
    | Error errors ->
        List.exists
          (function
            | Instruction_codec.Opcode_overflow
                { pattern_name = "pattern-256"; first_opcode = 256; span = 1 } ->
                true
            | _ -> false)
          errors
    | Ok _ -> false);
  let span_2 =
    Instruction_codec.register_or_constant Codec_fixture.register_codec Instruction_codec.zarith
  in
  let span_4 = Instruction_codec.pair span_2 span_2 in
  let span_16 = Instruction_codec.pair span_4 span_4 in
  let span_256 = Instruction_codec.pair span_16 span_16 in
  let oversized_operand_codec = Instruction_codec.pair span_256 span_2 in
  let oversized = [ halt_pattern ~name:"oversized" oversized_operand_codec ] in
  Alcotest.(check bool)
    "composed span arithmetic is overflow-safe" true
    (match compile_without_exception "oversized operand codec" oversized with
    | Error
        [
          Instruction_codec.Opcode_overflow
            { pattern_name = "oversized"; first_opcode = 0; span = 257 };
        ] ->
        true
    | Error _ | Ok _ -> false)

let () =
  Alcotest.run "Instruction codec"
    [
      ( "codec",
        [
          Alcotest.test_case "declaration order and variants" `Quick
            test_declaration_order_and_variants;
          Alcotest.test_case "complete round trips" `Quick test_complete_round_trips;
          Alcotest.test_case "structured failures" `Quick test_structured_failures;
          Alcotest.test_case "safe opcode exhaustion" `Quick test_safe_opcode_exhaustion;
        ] );
    ]
