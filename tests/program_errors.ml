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
  expect_error expected_fragments (Program.parse_regfile_from_string source)

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
            (program "jmp r32" [ "invalid register 'r32'"; "`r0`–`r31`" ]);
          Alcotest.test_case "unknown label" `Quick
            (program "mov r1 missing" [ "Unknown label \"missing\""; "Define it with `missing:`" ]);
          Alcotest.test_case "invalid expression" `Quick
            (program "mov r1 Inf"
               [ "infinite values are not supported by Griotte"; "use a finite integer" ]);
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
               [ "infinite values are not supported by Griotte"; "use a finite integer" ]);
        ] );
      ( "macros",
        [
          Alcotest.test_case "duplicate definition" `Quick
            (program "%define VALUE 1\n%define VALUE 2\nhalt"
               [ "line 2, column 1"; "duplicate definition \"VALUE\"" ]);
          Alcotest.test_case "cyclic definition" `Quick
            (program "%define FIRST SECOND\n%define SECOND FIRST\nmov r1 FIRST"
               [ "cyclic definition"; "FIRST" ]);
          Alcotest.test_case "infinite definition" `Quick
            (program "%define VALUE Inf\nmov r1 VALUE"
               [ "infinite values are not supported by Griotte"; "use a finite integer" ]);
          Alcotest.test_case "non-integer definition" `Quick
            (program "%define VALUE [R W DL DRO]\nmov r1 VALUE"
               [ "unexpected token \"[\""; "Expected an integer" ]);
          Alcotest.test_case "definition label collision" `Quick
            (program "%define value 1\nvalue:\nhalt" [ "conflicts with a label"; "\"value\"" ]);
          Alcotest.test_case "unknown macro" `Quick
            (program "%missing()" [ "line 1, column 1"; "unknown macro \"missing\"" ]);
          Alcotest.test_case "wrong arity" `Quick
            (program "%macro one(dst: reg)\nmov $dst 1\n%endmacro\n%one(r1, r2)"
               [ "expects 1 argument(s), but received 2" ]);
          Alcotest.test_case "wrong argument type" `Quick
            (program "%macro jump(dst: reg)\njmp $dst\n%endmacro\n%jump(4)"
               [ "argument for $dst"; "is not a reg" ]);
          Alcotest.test_case "value parameter in expression position" `Quick
            (program "%macro bad(item: value)\n# $item\n%endmacro\n%bad(1)"
               [ "parameter $item has type value"; "invalid in this operand position" ]);
          Alcotest.test_case "unknown parameter" `Quick
            (program "%macro bad(dst: reg)\nmov $missing 1\n%endmacro\n%bad(r1)"
               [ "unknown parameter $missing"; "macro \"bad\"" ]);
          Alcotest.test_case "nested macro call" `Quick
            (program "%macro outer()\n%inner()\n%endmacro\nhalt"
               [ "line 2, column 1"; "calls are not allowed inside macro" ]);
          Alcotest.test_case "duplicate private label" `Quick
            (program "%macro bad()\nhere:\nhalt\nhere:\nhalt\n%endmacro\n%bad()"
               [ "duplicate private label \"here\"" ]);
          Alcotest.test_case "unterminated macro" `Quick
            (program "%macro bad()\nhalt"
               [ "line 2, column 5"; "input ended before this construct was complete" ]);
        ] );
    ]
