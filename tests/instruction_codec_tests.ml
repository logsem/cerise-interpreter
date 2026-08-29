open Cerise
module Codec_fixture = struct
  type register = R0 | R1 | R2
  type operand = (register, Z.t) Instruction_codec.register_or_constant

  type instruction =
    | Halt
    | Move of register * operand
    | Calculate of register * operand * operand
    | Signed_pair of Z.t * Z.t
    | Pinned

  let register_codec = Instruction_codec.enum ~name:"fixture register" [ R0; R1; R2 ]
  let operand = Instruction_codec.register_or_constant register_codec Instruction_codec.zarith

  let cases =
    [
      Instruction_codec.case ~name:"halt" Instruction_codec.unit
        ~construct:(fun () -> Halt)
        ~project:(function Halt -> Some () | _ -> None);
      Instruction_codec.case ~name:"move"
        (Instruction_codec.pair (Instruction_codec.register register_codec) operand)
        ~construct:(fun (register, operand) -> Move (register, operand))
        ~project:(function Move (register, operand) -> Some (register, operand) | _ -> None);
      Instruction_codec.case ~name:"calculate"
        (Instruction_codec.triple (Instruction_codec.register register_codec) operand operand)
        ~construct:(fun (register, first, second) -> Calculate (register, first, second))
        ~project:(function
          | Calculate (register, first, second) -> Some (register, first, second) | _ -> None);
      Instruction_codec.case ~name:"signed-pair"
        (Instruction_codec.pair
           (Instruction_codec.scalar Instruction_codec.zarith)
           (Instruction_codec.scalar Instruction_codec.zarith))
        ~construct:(fun (first, second) -> Signed_pair (first, second))
        ~project:(function Signed_pair (first, second) -> Some (first, second) | _ -> None);
      Instruction_codec.case ~name:"pinned" ~allocation:(Fixed 200) Instruction_codec.unit
        ~construct:(fun () -> Pinned)
        ~project:(function Pinned -> Some () | _ -> None);
    ]

  let codec = Instruction_codec.compile cases |> Result.get_ok
end

let check_round_trip instruction =
  let encoded = Instruction_codec.encode Codec_fixture.codec instruction |> Result.get_ok in
  let decoded = Instruction_codec.decode Codec_fixture.codec encoded |> Result.get_ok in
  Alcotest.(check bool) "codec round trip" true (instruction = decoded)

let test_codec_allocation_and_variants () =
  Alcotest.(check (list (triple string int int)))
    "automatic and fixed allocations"
    [
      ("halt", 0, 1); ("move", 1, 2); ("calculate", 3, 4); ("signed-pair", 7, 1); ("pinned", 200, 1);
    ]
    (Instruction_codec.allocations Codec_fixture.codec);
  let open Codec_fixture in
  let operands = [ Instruction_codec.Register R1; Instruction_codec.Constant (Z.of_int (-7)) ] in
  List.iter (fun operand -> check_round_trip (Move (R0, operand))) operands;
  List.iter
    (fun first ->
      List.iter (fun second -> check_round_trip (Calculate (R2, first, second))) operands)
    operands;
  check_round_trip (Signed_pair (Z.of_int (-123456), Z.of_int 987654));
  check_round_trip Pinned;
  List.iter
    (fun (first, second, expected) ->
      let encoded = Instruction_codec.encode_signed_pair first second in
      Alcotest.(check string) "signed-pair compatibility golden"
        (Z.to_string expected) (Z.to_string encoded);
      Alcotest.(check (pair string string)) "signed-pair golden decodes"
        (Z.to_string first, Z.to_string second)
        (let left, right = Instruction_codec.decode_signed_pair encoded in
         Z.to_string left, Z.to_string right))
    [ (Z.of_int (-5), Z.of_int 8, Z.of_int 581);
      (Z.of_int 13, Z.of_int (-21), Z.of_int 2510);
      (Z.zero, Z.zero, Z.zero) ];
  let encoded =
    Instruction_codec.encode codec (Calculate (R2, List.hd operands, List.hd operands))
    |> Result.get_ok
  in
  Alcotest.(check bool)
    "opcode occupies low eight bits" true
    (Z.equal (Z.extract encoded 0 8) (Z.of_int 3))

let test_codec_large_signed_pairs () =
  let bit_count = 100_000 in
  let positive = Z.logor (Z.shift_left Z.one bit_count) (Z.of_int 0x5a) in
  let negative = Z.neg (Z.logor (Z.shift_left Z.one (bit_count + 37)) (Z.of_int 0xa5)) in
  let encoded = Instruction_codec.encode_signed_pair positive negative in
  let decoded = Instruction_codec.decode_signed_pair encoded in
  Alcotest.(check (pair string string))
    "large helper round trip"
    (Z.to_string positive, Z.to_string negative)
    (let first, second = decoded in
     (Z.to_string first, Z.to_string second));
  check_round_trip (Codec_fixture.Signed_pair (negative, positive))

let test_codec_structured_failures () =
  let module C = Codec_fixture in
  let compile_without_exception name cases =
    match Instruction_codec.compile cases with
    | result -> result
    | exception exn ->
        Alcotest.failf "%s raised %s instead of returning a structured error" name
          (Printexc.to_string exn)
  in
  let halt_case ~name ?(allocation = Instruction_codec.Auto) shape =
    Instruction_codec.case ~name ~allocation shape ~construct:(fun _ -> C.Halt)
      ~project:(fun _ -> None)
  in
  Alcotest.(check bool)
    "negative encoding" true
    (match Instruction_codec.decode C.codec Z.minus_one with
    | Error (Instruction_codec.Negative_encoding value) -> Z.equal value Z.minus_one
    | _ -> false);
  Alcotest.(check bool)
    "malformed payload" true
    (match Instruction_codec.decode C.codec (Z.shift_left Z.one 8) with
    | Error (Instruction_codec.Malformed_encoding _) -> true
    | _ -> false);
  Alcotest.(check bool)
    "unknown opcode" true
    (match Instruction_codec.decode C.codec (Z.of_int 250) with
    | Error (Instruction_codec.Unknown_opcode 250) -> true
    | _ -> false);
  let collision =
    [
      Instruction_codec.case ~name:"first" ~allocation:(Fixed 10)
        (Instruction_codec.register_or_constant C.register_codec Instruction_codec.zarith)
        ~construct:(fun _ -> C.Halt)
        ~project:(fun _ -> None);
      Instruction_codec.case ~name:"second" ~allocation:(Fixed 11) Instruction_codec.unit
        ~construct:(fun () -> C.Halt)
        ~project:(fun _ -> None);
    ]
  in
  Alcotest.(check bool)
    "fixed collision rejected" true
    (match Instruction_codec.compile collision with
    | Error errors ->
        List.exists (function Instruction_codec.Opcode_collision _ -> true | _ -> false) errors
    | Ok _ -> false);
  let overflow =
    [
      Instruction_codec.case ~name:"overflow" ~allocation:(Fixed 255)
        (Instruction_codec.register_or_constant C.register_codec Instruction_codec.zarith)
        ~construct:(fun _ -> C.Halt)
        ~project:(fun _ -> None);
    ]
  in
  Alcotest.(check bool)
    "opcode overflow rejected" true
    (match Instruction_codec.compile overflow with
    | Error errors ->
        List.exists (function Instruction_codec.Opcode_overflow _ -> true | _ -> false) errors
    | Ok _ -> false);
  let automatic_overflow =
    List.init 257 (fun index ->
        Instruction_codec.case ~name:(Printf.sprintf "auto-%d" index) Instruction_codec.unit
          ~construct:(fun () -> C.Halt)
          ~project:(fun _ -> None))
  in
  Alcotest.(check bool)
    "automatic opcode overflow rejected" true
    (match compile_without_exception "automatic exhaustion" automatic_overflow with
    | Error errors ->
        List.exists (function Instruction_codec.Opcode_overflow _ -> true | _ -> false) errors
    | Ok _ -> false);
  List.iter
    (fun (name, opcode) ->
      let cases = [ halt_case ~name ~allocation:(Fixed opcode) Instruction_codec.unit ] in
      Alcotest.(check bool)
        (name ^ " rejected as an invalid fixed opcode") true
        (match compile_without_exception name cases with
        | Error [ Instruction_codec.Invalid_fixed_opcode error ] ->
            error.case_name = name && error.opcode = opcode
        | Error _ | Ok _ -> false))
    [ ("fixed-max-int", max_int); ("fixed-min-int", min_int) ];
  let boundary_valid =
    [ halt_case ~name:"boundary-valid" ~allocation:(Fixed 255) Instruction_codec.unit ]
  in
  Alcotest.(check (list (triple string int int)))
    "fixed opcode 255 accepts a unit span"
    [ ("boundary-valid", 255, 1) ]
    (match compile_without_exception "boundary-valid" boundary_valid with
    | Ok codec -> Instruction_codec.allocations codec
    | Error errors ->
        Alcotest.failf "boundary-valid returned errors: %s"
          (String.concat "; " (List.map Instruction_codec.error_message errors)));
  let boundary_overflow =
    [
      halt_case ~name:"boundary-overflow" ~allocation:(Fixed 255)
        (Instruction_codec.register_or_constant C.register_codec Instruction_codec.zarith);
    ]
  in
  Alcotest.(check bool)
    "fixed opcode 255 rejects a two-opcode span" true
    (match compile_without_exception "boundary-overflow" boundary_overflow with
    | Error [ Instruction_codec.Opcode_overflow error ] ->
        error.case_name = "boundary-overflow" && error.first_opcode = 255 && error.span = 2
    | Error _ | Ok _ -> false);
  let span_2 =
    Instruction_codec.register_or_constant C.register_codec Instruction_codec.zarith
  in
  let span_4 = Instruction_codec.pair span_2 span_2 in
  let span_16 = Instruction_codec.pair span_4 span_4 in
  let span_64 = Instruction_codec.pair span_4 span_16 in
  let span_256 = Instruction_codec.pair span_16 span_16 in
  let span_16k = Instruction_codec.pair span_256 span_64 in
  let span_64k = Instruction_codec.pair span_256 span_256 in
  let span_1g = Instruction_codec.pair span_64k span_16k in
  let span_4g = Instruction_codec.pair span_64k span_64k in
  let oversized_shape = Instruction_codec.pair span_4g span_1g in
  let oversized_span = Instruction_codec.variant_span oversized_shape in
  let oversized = [ halt_case ~name:"oversized-auto" oversized_shape ] in
  Alcotest.(check bool)
    "overflowed composed shape returns structured overflow" true
    (match compile_without_exception "oversized-auto" oversized with
    | Error [ Instruction_codec.Opcode_overflow error ] ->
        error.case_name = "oversized-auto" && error.first_opcode = 0
        && error.span = oversized_span
    | Error _ | Ok _ -> false)

let () = Alcotest.run "Instruction codec" [ ("codec", [ Alcotest.test_case "allocations" `Quick test_codec_allocation_and_variants; Alcotest.test_case "large signed" `Quick test_codec_large_signed_pairs; Alcotest.test_case "failures" `Quick test_codec_structured_failures ]) ]
