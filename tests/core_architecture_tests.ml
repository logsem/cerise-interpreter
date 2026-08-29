open Cerise
module Frontend = Assembly_frontend

let get_ok = function
  | Ok value -> value
  | Error diagnostics ->
      Alcotest.failf "unexpected diagnostics: %s"
        (String.concat "\n" (List.map Diagnostic.to_string diagnostics))

let token_error tokens message =
  match tokens with
  | token :: _ -> Error [ Diagnostic.error ~location:(Frontend.Token.location token) message ]
  | [] -> Error [ Diagnostic.error message ]

module Syntax_a = struct
  type register = Named of string | Register_parameter of string
  type statement = Set of register * Frontend.Expression.t | Data of Frontend.Expression.t
  type raw_word = Frontend.Expression.t
  type regfile = (string * Frontend.Expression.t) list

  type macro_argument =
    | Register_argument of register
    | Expression_argument of Frontend.Expression.t

  type parameter_kind = Register_kind | Expression_kind

  let parse_register = function
    | token :: rest -> (
        match Frontend.Token.kind token with
        | Identifier name -> Ok (Named name, rest)
        | Parameter name -> Ok (Register_parameter name, rest)
        | _ -> token_error [ token ] "Fixture A expected a register name.")
    | [] -> token_error [] "Fixture A expected a register name."

  let parse_statement = function
    | opcode :: rest when Frontend.Token.kind opcode = Identifier "set" -> (
        match parse_register rest with
        | Error _ as error -> error
        | Ok (register, rest) ->
            Result.map
              (fun (expression, rest) -> (Set (register, expression), rest))
              (Frontend.parse_expression rest))
    | tokens -> token_error tokens "Fixture A supports only the `set` instruction."

  let parse_raw_word = Frontend.parse_expression

  let rec parse_regfile entries = function
    | [] -> Ok (List.rev entries, [])
    | register :: assign :: rest -> (
        match (Frontend.Token.kind register, Frontend.Token.kind assign) with
        | Identifier name, Assign -> (
            match Frontend.parse_expression rest with
            | Error _ as error -> error
            | Ok (expression, rest) -> parse_regfile ((name, expression) :: entries) rest)
        | _ -> token_error [ register ] "Fixture A expected `register := expression`.")
    | tokens -> token_error tokens "Fixture A expected `register := expression`."

  let parse_regfile tokens = parse_regfile [] tokens

  let parse_macro_argument = function
    | token :: rest -> (
        match Frontend.Token.kind token with
        | Identifier name when String.length name > 0 && Char.equal name.[0] 'r' ->
            Ok (Register_argument (Named name), rest)
        | Parameter name -> Ok (Register_argument (Register_parameter name), rest)
        | _ ->
            Result.map
              (fun (expression, rest) -> (Expression_argument expression, rest))
              (Frontend.parse_expression (token :: rest)))
    | [] -> token_error [] "Fixture A expected a macro argument."

  let statement_of_raw_word expression = Data expression

  let parameter_kind = function
    | "reg" -> Some Register_kind
    | "expr" -> Some Expression_kind
    | _ -> None

  let parameter_kind_name = function Register_kind -> "reg" | Expression_kind -> "expr"

  let argument_kind = function
    | Register_argument _ -> Register_kind
    | Expression_argument _ -> Expression_kind

  let accepts_argument kind argument = kind = argument_kind argument

  let expression_of_argument = function
    | Expression_argument expression -> Some expression
    | Register_argument _ -> None

  let map_statement_expressions mapper = function
    | Set (register, expression) -> Set (register, mapper expression)
    | Data expression -> Data (mapper expression)

  let map_raw_word_expressions mapper expression = mapper expression

  let map_regfile_expressions mapper entries =
    List.map (fun (register, expression) -> (register, mapper expression)) entries

  let map_argument_expressions mapper = function
    | Expression_argument expression -> Expression_argument (mapper expression)
    | Register_argument _ as argument -> argument

  let rec expression_parameters parameters diagnostics = function
    | Frontend.Expression.Parameter name -> (
        match List.assoc_opt name parameters with
        | Some Expression_kind -> diagnostics
        | Some Register_kind ->
            Diagnostic.error (Printf.sprintf "$%s is a register parameter in an expression." name)
            :: diagnostics
        | None ->
            Diagnostic.error (Printf.sprintf "Unknown expression parameter $%s." name)
            :: diagnostics)
    | Add (left, right)
    | Subtract (left, right)
    | Multiply (left, right)
    | Logand (left, right)
    | Logor (left, right)
    | Shift_left (left, right)
    | Shift_right (left, right) ->
        expression_parameters parameters (expression_parameters parameters diagnostics left) right
    | Integer _ | Current_address | Max_address | Stack_address | Symbol _ -> diagnostics

  let validate_statement ~parameters = function
    | Set (register, expression) -> (
        let diagnostics = expression_parameters parameters [] expression in
        match register with
        | Named _ -> diagnostics
        | Register_parameter name -> (
            match List.assoc_opt name parameters with
            | Some Register_kind -> diagnostics
            | Some Expression_kind ->
                Diagnostic.error
                  (Printf.sprintf "$%s is an expression parameter in a register position." name)
                :: diagnostics
            | None ->
                Diagnostic.error (Printf.sprintf "Unknown register parameter $%s." name)
                :: diagnostics))
    | Data expression -> expression_parameters parameters [] expression

  let validate_raw_word ~parameters expression = expression_parameters parameters [] expression

  let substitute_register arguments = function
    | Named _ as register -> Ok register
    | Register_parameter name -> (
        match List.assoc_opt name arguments with
        | Some (Register_argument register) -> Ok register
        | Some (Expression_argument _) ->
            Error [ Diagnostic.error (Printf.sprintf "$%s is not a register argument." name) ]
        | None -> Error [ Diagnostic.error (Printf.sprintf "No argument for $%s." name) ])

  let substitute_statement ~arguments = function
    | Set (register, expression) ->
        Result.map
          (fun register -> Set (register, expression))
          (substitute_register arguments register)
    | Data _ as statement -> Ok statement

  let substitute_raw_word ~arguments:_ word = Ok word

  let substitute_argument ~arguments = function
    | Register_argument register ->
        Result.map
          (fun register -> Register_argument register)
          (substitute_register arguments register)
    | Expression_argument _ as argument -> Ok argument
end

module Syntax_b = struct
  type statement = Noop | Push of Frontend.Expression.t | Blob of raw_word
  and raw_word = Frontend.Expression.t * Frontend.Expression.t

  type regfile = unit
  type macro_argument = Frontend.Expression.t
  type parameter_kind = Expression_kind

  let parse_statement = function
    | token :: rest when Frontend.Token.kind token = Identifier "noop" -> Ok (Noop, rest)
    | token :: rest when Frontend.Token.kind token = Identifier "push" ->
        Result.map
          (fun (expression, rest) -> (Push expression, rest))
          (Frontend.parse_expression rest)
    | tokens -> token_error tokens "Fixture B supports only `push` and `noop`."

  let parse_raw_word = function
    | opening :: rest when Frontend.Token.kind opening = Punctuation '[' -> (
        match Frontend.parse_expression rest with
        | Error _ as error -> error
        | Ok (first, comma :: rest) when Frontend.Token.kind comma = Punctuation ',' -> (
            match Frontend.parse_expression rest with
            | Error _ as error -> error
            | Ok (second, closing :: rest) when Frontend.Token.kind closing = Punctuation ']' ->
                Ok ((first, second), rest)
            | Ok (_, tokens) -> token_error tokens "Fixture B expected `]`.")
        | Ok (_, tokens) -> token_error tokens "Fixture B expected `,`.")
    | tokens -> token_error tokens "Fixture B raw words use `[left, right]`."

  let parse_regfile = function
    | [] -> Ok ((), [])
    | tokens -> token_error tokens "Fixture B has no register-file syntax."

  let parse_macro_argument tokens = Frontend.parse_expression tokens
  let statement_of_raw_word word = Blob word
  let parameter_kind = function "expr" -> Some Expression_kind | _ -> None
  let parameter_kind_name Expression_kind = "expr"
  let argument_kind _ = Expression_kind
  let accepts_argument Expression_kind _ = true
  let expression_of_argument expression = Some expression

  let map_statement_expressions mapper = function
    | Noop -> Noop
    | Push expression -> Push (mapper expression)
    | Blob (left, right) -> Blob (mapper left, mapper right)

  let map_raw_word_expressions mapper (left, right) = (mapper left, mapper right)
  let map_regfile_expressions _ () = ()
  let map_argument_expressions mapper expression = mapper expression
  let validate_statement ~parameters:_ _ = []
  let validate_raw_word ~parameters:_ _ = []
  let substitute_statement ~arguments:_ statement = Ok statement
  let substitute_raw_word ~arguments:_ word = Ok word
  let substitute_argument ~arguments:_ argument = Ok argument
end

module Frontend_a = Frontend.Make (Syntax_a)
module Frontend_b = Frontend.Make (Syntax_b)

let integer = function
  | Frontend.Expression.Integer value -> value
  | _ -> Alcotest.fail "expected a resolved integer expression"

let check_z message expected actual =
  Alcotest.(check string) message (Z.to_string expected) (Z.to_string actual)

let test_common_frontend_construction () =
  let source =
    "%define OFFSET 2 %macro emit(dst: reg, amount: expr) private: set $dst private + $amount \
     %endmacro %emit(r1, OFFSET) # target - 1 target: set r2 &CURRENT_ADDR"
  in
  match Frontend_a.parse_program source |> get_ok with
  | [ Syntax_a.Set (Named "r1", first); Data raw; Set (Named "r2", current) ] ->
      check_z "definition and private label" (Z.of_int 2) (integer first);
      check_z "raw word and global label" Z.one (integer raw);
      check_z "current address" (Z.of_int 2) (integer current)
  | _ -> Alcotest.fail "unexpected Fixture A program shape"

let test_hygienic_typed_macros () =
  let source =
    "%macro address(dst: reg) private: set $dst private %endmacro %address(r1) %address(r2)"
  in
  (match Frontend_a.parse_program source |> get_ok with
  | [ Syntax_a.Set (Named "r1", first); Set (Named "r2", second) ] ->
      check_z "first private label" Z.zero (integer first);
      check_z "second private label" Z.one (integer second)
  | _ -> Alcotest.fail "macro labels were not expanded hygienically");
  Alcotest.(check bool)
    "typed call rejected" true
    (match Frontend_a.parse_program "%macro bad(dst: reg) set $dst 1 %endmacro %bad(4)" with
    | Error _ -> true
    | Ok _ -> false)

let test_runtime_expressions_and_backend_specific_syntax () =
  let regfile = Frontend_a.parse_regfile "r1 := MAX_ADDR - 1 r2 := STK_ADDR + 2" |> get_ok in
  let config = Runtime_config.create ~max_addr:(Z.of_int 100) ~stack_addr:(Z.of_int 60) () in
  let lookup name =
    List.assoc name regfile |> Frontend.Expression.evaluate_runtime config |> Result.get_ok
  in
  check_z "runtime max expression" (Z.of_int 99) (lookup "r1");
  check_z "runtime stack expression" (Z.of_int 62) (lookup "r2");
  let negative_left_shift = Frontend.Expression.Shift_left (Integer Z.one, Integer Z.minus_one) in
  let negative_right_shift = Frontend.Expression.Shift_right (Integer Z.one, Integer Z.minus_one) in
  Alcotest.(check bool)
    "invalid constant shift is not folded" true
    (match Frontend.Expression.simplify negative_left_shift with
    | Shift_left (Integer left, Integer right) -> Z.equal left Z.one && Z.equal right Z.minus_one
    | _ -> false);
  List.iter
    (fun (name, expression) ->
      Alcotest.(check (result string string))
        name (Error "shift count must be non-negative")
        (Result.map Z.to_string (Frontend.Expression.evaluate_runtime config expression)))
    [ ("negative left shift", negative_left_shift); ("negative right shift", negative_right_shift) ];
  (match Frontend_a.parse_program "set r1 1 + 2 * 3" |> get_ok with
  | [ Syntax_a.Set (_, Integer result) ] ->
      check_z "flat left-associative operators" (Z.of_int 9) result
  | _ -> Alcotest.fail "common operators are not flat and left-associative");
  Alcotest.(check bool)
    "Fixture B has a different AST and raw shape" true
    (match Frontend_b.parse_program "push 4 # [5, 6] noop" |> get_ok with
    | [ Syntax_b.Push _; Blob _; Noop ] -> true
    | _ -> false);
  Alcotest.(check bool)
    "Fixture B rejects Fixture A syntax in its parser" true
    (match Frontend_b.parse_program ~filename:"fixture-b.s" "set r1 2" with
    | Error (diagnostic :: _) -> (
        match Diagnostic.location diagnostic with
        | Some location -> location.Diagnostic.source = Some "fixture-b.s" && location.line = 1
        | None -> false)
    | _ -> false);
  Alcotest.(check bool)
    "Fixture A rejects Fixture B syntax" true
    (Result.is_error (Frontend_a.parse_program "push 4"))

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
    (fun (first, second) ->
      Alcotest.(check string)
        "historical signed interleaving"
        (Z.to_string (Cerise_internal.Encode.encode_int_int first second))
        (Z.to_string (Instruction_codec.encode_signed_pair first second)))
    [ (Z.of_int (-5), Z.of_int 8); (Z.of_int 13, Z.of_int (-21)); (Z.zero, Z.zero) ];
  let encoded =
    Instruction_codec.encode codec (Calculate (R2, List.hd operands, List.hd operands))
    |> Result.get_ok
  in
  Alcotest.(check bool)
    "opcode occupies low eight bits" true
    (Z.equal (Z.extract encoded 0 8) (Z.of_int 3))

let test_codec_structured_failures () =
  let module C = Codec_fixture in
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
    (match Instruction_codec.compile automatic_overflow with
    | Error errors ->
        List.exists (function Instruction_codec.Opcode_overflow _ -> true | _ -> false) errors
    | Ok _ -> false)

let () =
  Alcotest.run "Core architecture"
    [
      ( "assembly frontend",
        [
          Alcotest.test_case "expressions labels definitions raw" `Quick
            test_common_frontend_construction;
          Alcotest.test_case "typed hygienic macros" `Quick test_hygienic_typed_macros;
          Alcotest.test_case "different backend syntaxes" `Quick
            test_runtime_expressions_and_backend_specific_syntax;
        ] );
      ( "instruction codec",
        [
          Alcotest.test_case "allocation variants signed round trips" `Quick
            test_codec_allocation_and_variants;
          Alcotest.test_case "structured validation failures" `Quick test_codec_structured_failures;
        ] );
    ]
