open Cerise

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search index =
    index + fragment_length <= text_length
    && (String.equal (String.sub text index fragment_length) fragment || search (index + 1))
  in
  fragment_length = 0 || search 0

let expect_error expected_fragments = function
  | Ok _ -> Alcotest.fail "Expected parsing to fail"
  | Error actual ->
      List.iter
        (fun fragment ->
          Alcotest.(check bool)
            (Printf.sprintf "error contains %S\nactual error: %s" fragment actual)
            true (contains actual fragment))
        expected_fragments

let program source expected_fragments () =
  expect_error expected_fragments (Program.parse_prog_from_string source)

let regfile source expected_fragments () =
  expect_error expected_fragments (Program.parse_regfile_from_string source Z.zero)

let () =
  Alcotest.run "Program errors"
    [
      ( "program",
        [
          Alcotest.test_case "unexpected token" `Quick
            (program "halt\njmp )"
               [
                 "line 2, column 5";
                 "unexpected token \")\"";
                 "Expected a register";
                 "Replace this token";
                 "2 | jmp )";
                 "|     ^";
               ]);
          Alcotest.test_case "unexpected end of input" `Quick
            (program "mov r1"
               [
                 "line 1, column 7";
                 "input ended before this construct was complete";
                 "Expected a register";
                 "Add the missing value or delimiter";
                 "1 | mov r1";
                 "|       ^";
               ]);
          Alcotest.test_case "unexpected character" `Quick
            (program "halt\n@"
               [
                 "line 2, column 1";
                 "unexpected character '@'";
                 "remove it or replace it with a valid Cerise token";
                 "2 | @";
                 "| ^";
               ]);
          Alcotest.test_case "invalid register" `Quick
            (program "jmp r32" [ "unexpected token \"r32\""; "`r0`–`r31`" ]);
          Alcotest.test_case "unknown label" `Quick
            (program "mov r1 missing" [ "Unknown label \"missing\""; "Define it with `missing:`" ]);
          Alcotest.test_case "invalid expression" `Quick
            (program "mov r1 Inf"
               [ "Constants expressions cannot be ∞"; "Replace `Inf` with a finite integer" ]);
          Alcotest.test_case "invalid integer" `Quick
            (program "mov r1 999999999999999999999999999999"
               [ "invalid integer"; "use a value that fits in a machine integer" ]);
        ] );
      ( "register file",
        [
          Alcotest.test_case "unexpected end of input" `Quick
            (regfile "pc := "
               [
                 "line 1, column 7";
                 "input ended before this construct was complete";
                 "Expected an integer";
                 "Add the missing value or delimiter";
                 "1 | pc := ";
                 "|       ^";
               ]);
          Alcotest.test_case "unexpected character" `Quick
            (regfile "pc = 0"
               [
                 "unexpected character '='";
                 "replace it with a valid Cerise token";
                 "1 | pc = 0";
                 "|    ^";
               ]);
          Alcotest.test_case "invalid expression" `Quick
            (regfile "pc := Inf"
               [ "Integer machine word cannot be ∞"; "Replace `Inf` with a finite integer" ]);
        ] );
    ]
