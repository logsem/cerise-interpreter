open Cerise

let expect_error expected = function
  | Ok _ -> Alcotest.fail "Expected parsing to fail"
  | Error actual -> Alcotest.(check string) "error message" expected actual

let program source expected () = expect_error expected (Program.parse_prog_from_string source)

let regfile source expected () =
  expect_error expected (Program.parse_regfile_from_string source Z.zero)

let () =
  Alcotest.run "Program errors"
    [
      ( "program",
        [
          Alcotest.test_case "unexpected token" `Quick
            (program "halt\nmov r1 )" "line 2, column 8: syntax error: unexpected token \")\"");
          Alcotest.test_case "unexpected end of input" `Quick
            (program "mov r1" "line 1, column 7: syntax error: unexpected end of input");
          Alcotest.test_case "unexpected character" `Quick
            (program "halt\n@" "line 2, column 1: lexical error: unexpected character '@'");
          Alcotest.test_case "unknown label" `Quick
            (program "mov r1 missing" "Unknown label \"missing\"");
          Alcotest.test_case "invalid expression" `Quick
            (program "mov r1 Inf" "Constants expressions cannot be ∞");
        ] );
      ( "register file",
        [
          Alcotest.test_case "unexpected end of input" `Quick
            (regfile "pc := " "line 1, column 7: syntax error: unexpected end of input");
          Alcotest.test_case "unexpected character" `Quick
            (regfile "pc = 0" "line 1, column 4: lexical error: unexpected character '='");
          Alcotest.test_case "invalid expression" `Quick
            (regfile "pc := Inf" "Integer machine word cannot be ∞");
        ] );
    ]
