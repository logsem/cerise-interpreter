open Cerise
open Cerise.Ast
module Asm_ir = Cerise_internal.Asm_ir
module Asm_current_address_resolver = Cerise_internal.Current_address_resolver
module Asm_expression_evaluator = Cerise_internal.Expression_evaluator
module Asm_label_resolver = Cerise_internal.Label_resolver
module Asm_lexer = Cerise_internal.Lexer
module Asm_parser = Cerise_internal.Parser

let statement_tst = Alcotest.testable (Fmt.of_to_string Pretty_printer.string_of_statement) ( = )

let program source =
  match Program.parse_prog_from_string source with
  | Ok program -> program
  | Error message -> Alcotest.fail message

let check_program message expected source =
  Alcotest.(check (list statement_tst)) message expected (program source)

let test_parser_uses_symbol () =
  match Asm_parser.main Asm_lexer.token (Lexing.from_string "mov r1 VALUE") with
  | [ Asm_ir.Move (Asm_ir.Reg 1, Asm_ir.Const (Asm_ir.ConstExpr (Asm_ir.Symbol "VALUE"))) ] -> ()
  | _ -> Alcotest.fail "expected an unresolved Symbol, not a resolved Label"

let test_parser_uses_current_address () =
  match Asm_parser.main Asm_lexer.token (Lexing.from_string "mov r1 &CURRENT_ADDR") with
  | [ Asm_ir.Move (Asm_ir.Reg 1, Asm_ir.Const (Asm_ir.ConstExpr Asm_ir.CurrentAddr)) ] -> ()
  | _ -> Alcotest.fail "expected a CurrentAddr expression"

let test_current_address_resolution_pass () =
  let open Asm_ir in
  let input =
    [
      Lbl "start";
      Move (Reg 1, Const (ConstExpr CurrentAddr));
      Word (I (AddOp (CurrentAddr, IntLit (Infinite_z.of_int 2))));
      Lbl "ending";
      Halt;
    ]
  in
  let expected =
    [
      Lbl "start";
      Move (Reg 1, Const (ConstExpr (IntLit (Infinite_z.of_int 0))));
      Word (I (AddOp (IntLit (Infinite_z.of_int 1), IntLit (Infinite_z.of_int 2))));
      Lbl "ending";
      Halt;
    ]
  in
  Alcotest.(check bool)
    "current addresses resolved" true
    (Asm_current_address_resolver.resolve input = expected)

let test_current_address_in_program () =
  check_program "current instruction address"
    [ Op (Move (Reg 1, const 0)); Op Halt; Word (I (Z.of_int 2)); Op (Move (Reg 2, const 4)) ]
    {|
mov r1 &CURRENT_ADDR
halt
# &CURRENT_ADDR
after:
mov r2 (&CURRENT_ADDR + 1)
|}

let test_current_address_in_macro () =
  check_program "current address after macro expansion"
    [ Op (Move (Reg 1, const 0)); Op (Move (Reg 1, const 1)) ]
    {|
%macro here(dst: reg)
  mov $dst &CURRENT_ADDR
%endmacro
%here(r1)
%here(r1)
|}

let test_label_resolution_pass () =
  let open Asm_ir in
  let input =
    [
      Lbl "start";
      Move (Reg 1, Const (ConstExpr (AddOp (Label "ending", IntLit (Infinite_z.of_int 1)))));
      Word (I (SubOp (Label "ending", Label "start")));
      Lbl "ending";
      Halt;
    ]
  in
  let expected =
    [
      Move
        ( Reg 1,
          Const (ConstExpr (AddOp (IntLit (Infinite_z.of_int 2), IntLit (Infinite_z.of_int 1)))) );
      Word (I (SubOp (IntLit (Infinite_z.of_int 2), IntLit (Infinite_z.of_int 0))));
      Halt;
    ]
  in
  Alcotest.(check bool) "resolved IR" true (Asm_label_resolver.resolve input = expected)

let test_expression_evaluation_pass () =
  let open Asm_ir in
  let input =
    [
      Move
        ( Reg 1,
          Const (ConstExpr (AddOp (IntLit (Infinite_z.of_int 2), IntLit (Infinite_z.of_int 1)))) );
      Word (I (SubOp (IntLit (Infinite_z.of_int 2), IntLit (Infinite_z.of_int 1))));
    ]
  in
  let expected =
    [
      Move (Reg 1, Const (ConstExpr (IntLit (Infinite_z.of_int 3))));
      Word (I (IntLit (Infinite_z.of_int 1)));
    ]
  in
  Alcotest.(check bool) "evaluated IR" true (Asm_expression_evaluator.evaluate input = expected)

let test_literal_definition () =
  check_program "literal definition" [ Op (Move (Reg 1, const 3)) ] "%define VALUE 3\nmov r1 VALUE"

let test_chained_definitions () =
  check_program "chained definitions"
    [ Op (Move (Reg 1, const 5)); Op (Move (Reg 2, const (-3))) ]
    "%define RESULT BASE + 3\n%define BASE 0x2\n%define NEGATIVE -3\nmov r1 RESULT\nmov r2 NEGATIVE"

let test_label_definition () =
  check_program "label-valued definition" [ Op Halt; Word (I Z.one) ]
    "%define SIZE end - start\nstart:\nhalt\nend:\n# SIZE"

let test_definition_after_macro_expansion () =
  check_program "definition sees expanded addresses"
    [ Op Halt; Op Halt; Word (I (Z.of_int 2)) ]
    {|
%define AFTER after
%macro two()
  halt
  halt
%endmacro
%two()
after:
# AFTER
|}

let test_definition_in_capability () =
  match program {|
%define LIMIT 5
# (RW, GLOBAL, 0, LIMIT, 0)
|} with
  | [ Word (Sealable (Cap (RW, Global, base, Infinite_z.Int ending, address))) ] ->
      Alcotest.(check bool)
        "capability expression fields" true
        (Z.equal base Z.zero && Z.equal ending (Z.of_int 5) && Z.equal address Z.zero)
  | _ -> Alcotest.fail "expected one capability word"

let test_basic_macro () =
  check_program "basic macro"
    [ Op (Add (Reg 1, Register (Reg 1), const 4)) ]
    {|
%macro increment(dst: reg, amount: expr)
  add $dst $dst $amount
%endmacro
%increment(r1, 4)
|}

let test_forward_macro () =
  check_program "call before declaration"
    [ Op (Move (Reg 2, const 9)) ]
    {|
%set(r2, 9)
%macro set(dst: reg, source: value)
  mov $dst $source
%endmacro
|}

let test_value_expression_argument () =
  check_program "compound expression passed as value"
    [ Op (Move (Reg 2, const 3)); Op Halt; Op Halt ]
    {|
%macro set(dst: reg, source: value)
  mov $dst $source
%endmacro
start:
%set(r2, end - start)
halt
halt
end:
|}

let test_hygienic_labels () =
  check_program "private labels"
    [ Op (Move (Reg 1, const 0)); Op (Move (Reg 1, const 1)) ]
    {|
%macro address()
private:
  mov r1 private
%endmacro
%address()
%address()
|}

let test_external_label () =
  check_program "external label reference"
    [ Op (Move (Reg 1, const 1)); Op Halt ]
    {|
%macro load_end(dst: reg)
  mov $dst outside
%endmacro
%load_end(r1)
outside:
halt
|}

let test_all_parameter_types () =
  let source =
    {|
%macro typed(r: reg, v: value, e: expr, p: perm, sp: sealperm, l: locality, w: wtype)
  mov $r $v
  add r2 1 $e
  restrict r3 $p
  restrict r4 ($sp, $l)
  mov r5 $w
%endmacro
%typed(r1, r2, 3, RW, S, GLOBAL, Int)
|}
  in
  Alcotest.(check int) "expanded statement count" 5 (List.length (program source))

let test_whitespace_oriented_macros () =
  check_program "macro syntax does not depend on newlines"
    [ Op (Move (Reg 1, const 2)) ]
    "%define VALUE 2 %macro set(dst: reg) mov $dst VALUE %endmacro %set(r1)"

let test_parameters_in_capability_word () =
  match
    program
      "%macro cap(p: perm, l: locality, e: expr) # ($p, $l, 0, $e, 0) %endmacro %cap(RW, GLOBAL, \
       Inf)"
  with
  | [ Word (Sealable (Cap (RW, Global, base, Infinite_z.Inf, address))) ] ->
      Alcotest.(check bool)
        "parameterized capability" true
        (Z.equal base Z.zero && Z.equal address Z.zero)
  | _ -> Alcotest.fail "expected one parameterized capability word"

let () =
  Alcotest.run "Assembler macros"
    [
      ( "integer definitions",
        [
          Alcotest.test_case "parser symbols" `Quick test_parser_uses_symbol;
          Alcotest.test_case "parser current address" `Quick test_parser_uses_current_address;
          Alcotest.test_case "current-address resolution pass" `Quick
            test_current_address_resolution_pass;
          Alcotest.test_case "current address" `Quick test_current_address_in_program;
          Alcotest.test_case "current address in macro" `Quick test_current_address_in_macro;
          Alcotest.test_case "label resolution pass" `Quick test_label_resolution_pass;
          Alcotest.test_case "expression evaluation pass" `Quick test_expression_evaluation_pass;
          Alcotest.test_case "literal" `Quick test_literal_definition;
          Alcotest.test_case "chained and forward" `Quick test_chained_definitions;
          Alcotest.test_case "labels" `Quick test_label_definition;
          Alcotest.test_case "expanded addresses" `Quick test_definition_after_macro_expansion;
          Alcotest.test_case "capability field" `Quick test_definition_in_capability;
        ] );
      ( "sequence macros",
        [
          Alcotest.test_case "basic" `Quick test_basic_macro;
          Alcotest.test_case "forward declaration" `Quick test_forward_macro;
          Alcotest.test_case "value expression" `Quick test_value_expression_argument;
          Alcotest.test_case "hygienic labels" `Quick test_hygienic_labels;
          Alcotest.test_case "external label" `Quick test_external_label;
          Alcotest.test_case "parameter types" `Quick test_all_parameter_types;
          Alcotest.test_case "whitespace-oriented syntax" `Quick test_whitespace_oriented_macros;
          Alcotest.test_case "capability parameters" `Quick test_parameters_in_capability_word;
        ] );
    ]
