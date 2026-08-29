open Ast
open Assembly_frontend

let error tokens message =
  match tokens with
  | token :: _ -> Error [ Diagnostic.error ~location:(Token.location token) message ]
  | [] -> Error [ Diagnostic.error message ]

let identifier expected = function
  | token :: rest when Token.kind token = Token.Identifier expected -> Ok rest
  | tokens -> error tokens (Printf.sprintf "Expected `%s`." expected)

let punctuation expected = function
  | token :: rest when Token.kind token = Token.Punctuation expected -> Ok rest
  | tokens -> error tokens (Printf.sprintf "Expected `%c`." expected)

let parse_register_name name =
  match String.lowercase_ascii name with
  | "pc" -> Some PC
  | "ddc" -> Some (Reg 0)
  | "r0" -> Some (Reg 0)
  | "stk" -> Some (Reg 31)
  | "r31" -> Some (Reg 31)
  | name when String.length name > 1 && name.[0] = 'r' -> (
      match int_of_string_opt (String.sub name 1 (String.length name - 1)) with
      | Some n when n >= 0 && n <= 31 -> Some (Reg n)
      | _ -> None)
  | _ -> None

let parse_register = function
  | token :: rest -> (
      match Token.kind token with
      | Identifier name -> (
          match parse_register_name name with
          | Some register -> Ok (Named register, rest)
          | None -> error [ token ] "Expected a register.")
      | Parameter name -> Ok (Register_parameter name, rest)
      | _ -> error [ token ] "Expected a register.")
  | [] -> error [] "Expected a register."

let parse_permission = function
  | "O" -> Some O
  | "E" -> Some E
  | "RO" -> Some RO
  | "RX" -> Some RX
  | "RW" -> Some RW
  | "RWX" -> Some RWX
  | _ -> None

let parse_seal_permission = function
  | "SO" -> Some (false, false)
  | "S" -> Some (true, false)
  | "U" -> Some (false, true)
  | "SU" -> Some (true, true)
  | _ -> None

let parse_word_type = function
  | "Int" -> Some Integer
  | "Cap" -> Some Capability
  | "SealRange" -> Some Seal_range
  | "Sealed" -> Some Sealed
  | _ -> None

let scalar_constant token =
  match Token.kind token with
  | Identifier name -> (
      match parse_permission name with
      | Some permission -> Some (Permission permission)
      | None -> (
          match parse_seal_permission name with
          | Some permission -> Some (Seal_permission permission)
          | None -> Option.map (fun word_type -> Word_type word_type) (parse_word_type name)))
  | _ -> None

let parse_operand = function
  | token :: rest as tokens -> (
      match Token.kind token with
      | Parameter name -> Ok (Constant_term (Value_parameter name), rest)
      | Identifier name -> (
          match parse_register_name name with
          | Some register -> Ok (Register_term (Named register), rest)
          | None -> (
              match scalar_constant token with
              | Some constant -> Ok (Constant_term constant, rest)
              | None ->
                  Result.map
                    (fun (e, rest) -> (Constant_term (Expression e), rest))
                    (parse_expression tokens)))
      | _ ->
          Result.map
            (fun (e, rest) -> (Constant_term (Expression e), rest))
            (parse_expression tokens))
  | [] -> error [] "Expected a register or constant."

let unary constructor rest =
  Result.map (fun (r, rest) -> (constructor r, rest)) (parse_register rest)

let binary constructor rest =
  match parse_register rest with
  | Error _ as e -> e
  | Ok (a, rest) -> Result.map (fun (b, rest) -> (constructor a b, rest)) (parse_register rest)

let reg_operand constructor rest =
  match parse_register rest with
  | Error _ as e -> e
  | Ok (a, rest) -> Result.map (fun (b, rest) -> (constructor a b, rest)) (parse_operand rest)

let ternary_operands constructor rest =
  match parse_register rest with
  | Error _ as e -> e
  | Ok (a, rest) -> (
      match parse_operand rest with
      | Error _ as e -> e
      | Ok (b, rest) -> Result.map (fun (c, rest) -> (constructor a b c, rest)) (parse_operand rest)
      )

let three_registers constructor rest =
  match parse_register rest with
  | Error _ as e -> e
  | Ok (a, rest) -> (
      match parse_register rest with
      | Error _ as e -> e
      | Ok (b, rest) ->
          Result.map (fun (c, rest) -> (constructor a b c, rest)) (parse_register rest))

let parse_instruction = function
  | token :: rest -> (
      match Token.kind token with
      | Identifier name ->
          let result =
            match String.lowercase_ascii name with
            | "jmp" -> unary (fun r -> Jmp_term r) rest
            | "jnz" -> binary (fun a b -> Jnz_term (a, b)) rest
            | "move" | "mov" -> reg_operand (fun a b -> Move_term (a, b)) rest
            | "load" -> binary (fun a b -> Load_term (a, b)) rest
            | "store" -> reg_operand (fun a b -> Store_term (a, b)) rest
            | "add" -> ternary_operands (fun a b c -> Add_term (a, b, c)) rest
            | "sub" -> ternary_operands (fun a b c -> Sub_term (a, b, c)) rest
            | "mul" -> ternary_operands (fun a b c -> Mul_term (a, b, c)) rest
            | "rem" -> ternary_operands (fun a b c -> Rem_term (a, b, c)) rest
            | "div" -> ternary_operands (fun a b c -> Div_term (a, b, c)) rest
            | "lt" -> ternary_operands (fun a b c -> Lt_term (a, b, c)) rest
            | "lea" -> reg_operand (fun a b -> Lea_term (a, b)) rest
            | "restrict" -> reg_operand (fun a b -> Restrict_term (a, b)) rest
            | "subseg" -> ternary_operands (fun a b c -> SubSeg_term (a, b, c)) rest
            | "getb" -> binary (fun a b -> GetB_term (a, b)) rest
            | "gete" -> binary (fun a b -> GetE_term (a, b)) rest
            | "geta" -> binary (fun a b -> GetA_term (a, b)) rest
            | "getp" -> binary (fun a b -> GetP_term (a, b)) rest
            | "getotype" -> binary (fun a b -> GetOType_term (a, b)) rest
            | "getwtype" -> binary (fun a b -> GetWType_term (a, b)) rest
            | "seal" -> three_registers (fun a b c -> Seal_term (a, b, c)) rest
            | "unseal" -> three_registers (fun a b c -> UnSeal_term (a, b, c)) rest
            | "invoke" -> binary (fun a b -> Invoke_term (a, b)) rest
            | "fail" -> Ok (Fail_term, rest)
            | "halt" -> Ok (Halt_term, rest)
            | unsupported ->
                error [ token ] (Printf.sprintf "Unsupported vanilla instruction `%s`." unsupported)
          in
          Result.map_error
            (List.map (fun diagnostic ->
                 match Diagnostic.location diagnostic with
                 | Some _ -> diagnostic
                 | None ->
                     Diagnostic.make ~severity:(Diagnostic.severity diagnostic)
                       ~location:(Token.location token) (Diagnostic.message diagnostic)))
            result
      | _ -> error [ token ] "Expected an instruction.")
  | [] -> error [] "Expected an instruction."

let parse_expr_then separator closing constructor tokens =
  match parse_expression tokens with
  | Error _ as e -> e
  | Ok (b, rest) -> (
      match punctuation separator rest with
      | Error _ as e -> e
      | Ok rest -> (
          match parse_expression rest with
          | Error _ as e -> e
          | Ok (e, rest) -> (
              match punctuation separator rest with
              | Error _ as e -> e
              | Ok rest -> (
                  match parse_expression rest with
                  | Error _ as e -> e
                  | Ok (a, rest) ->
                      Result.map (fun rest -> (constructor b e a, rest)) (punctuation closing rest))
              )))

let parse_permission_term = function
  | token :: rest -> (
      match Token.kind token with
      | Identifier name -> (
          match parse_permission name with
          | Some permission -> Ok (Permission_literal permission, rest)
          | None -> error [ token ] "Expected a vanilla permission.")
      | Parameter name -> Ok (Permission_parameter name, rest)
      | _ -> error [ token ] "Expected a vanilla permission.")
  | [] -> error [] "Expected a vanilla permission."

let parse_seal_permission_term = function
  | token :: rest -> (
      match Token.kind token with
      | Identifier name -> (
          match parse_seal_permission name with
          | Some permission -> Ok (Seal_permission_literal permission, rest)
          | None -> error [ token ] "Expected a sealing permission.")
      | Parameter name -> Ok (Seal_permission_parameter name, rest)
      | _ -> error [ token ] "Expected a sealing permission.")
  | [] -> error [] "Expected a sealing permission."

let parse_sealable = function
  | open_token :: rest when Token.kind open_token = Punctuation '(' -> (
      match parse_permission_term rest with
      | Error _ as error -> error
      | Ok (permission, rest) -> (
          match punctuation ',' rest with
          | Error _ as error -> error
          | Ok rest -> parse_expr_then ',' ')' (fun b e a -> Cap_term (permission, b, e, a)) rest))
  | open_token :: rest when Token.kind open_token = Punctuation '[' -> (
      match parse_seal_permission_term rest with
      | Error _ as error -> error
      | Ok (permission, rest) -> (
          match punctuation ',' rest with
          | Error _ as error -> error
          | Ok rest ->
              parse_expr_then ',' ']' (fun b e a -> SealRange_term (permission, b, e, a)) rest))
  | tokens -> error tokens "Expected a capability or seal range."

let parse_word tokens =
  match tokens with
  | token :: _ when Token.kind token = Punctuation '(' || Token.kind token = Punctuation '[' ->
      Result.map (fun (s, rest) -> (Sealable_term s, rest)) (parse_sealable tokens)
  | token :: rest when Token.kind token = Punctuation '{' -> (
      match parse_expression rest with
      | Error _ as e -> e
      | Ok (otype, rest) -> (
          match punctuation ':' rest with
          | Error _ as e -> e
          | Ok rest -> (
              match parse_sealable rest with
              | Error _ as e -> e
              | Ok (s, rest) ->
                  Result.map (fun rest -> (Sealed_term (otype, s), rest)) (punctuation '}' rest))))
  | _ -> Result.map (fun (e, rest) -> (I_term e, rest)) (parse_expression tokens)

type parameter_kind =
  | Register_kind
  | Expression_kind
  | Value_kind
  | Permission_kind
  | Seal_permission_kind
  | Word_type_kind

type macro_argument = Register_argument of register | Constant_argument of constant_term

module Syntax = struct
  type nonrec statement = statement
  type raw_word = word_term
  type nonrec regfile = regfile
  type nonrec macro_argument = macro_argument
  type nonrec parameter_kind = parameter_kind

  let parse_statement tokens =
    Result.map (fun (op, rest) -> (Op op, rest)) (parse_instruction tokens)

  let parse_raw_word = parse_word

  let parse_regfile tokens =
    let rec loop acc = function
      | [] -> Ok (List.rev acc, [])
      | token :: rest -> (
          match Token.kind token with
          | Identifier name -> (
              match parse_register_name name with
              | None -> error [ token ] "Expected a register assignment."
              | Some register -> (
                  match rest with
                  | assign :: rest when Token.kind assign = Assign -> (
                      match parse_word rest with
                      | Error _ as e -> e
                      | Ok (word, rest) -> loop ((register, word) :: acc) rest)
                  | _ -> error rest "Expected `:=` after the register."))
          | _ -> error [ token ] "Expected a register assignment.")
    in
    loop [] tokens

  let parse_macro_argument tokens =
    match tokens with
    | token :: rest when match Token.kind token with Parameter _ -> true | _ -> false -> (
        match Token.kind token with
        | Parameter name -> Ok (Constant_argument (Value_parameter name), rest)
        | _ -> assert false)
    | token :: rest -> (
        match Token.kind token with
        | Identifier name -> (
            match parse_register_name name with
            | Some r -> Ok (Register_argument r, rest)
            | None -> (
                match scalar_constant token with
                | Some c -> Ok (Constant_argument c, rest)
                | None ->
                    Result.map
                      (fun (e, r) -> (Constant_argument (Expression e), r))
                      (parse_expression tokens)))
        | _ ->
            Result.map
              (fun (e, r) -> (Constant_argument (Expression e), r))
              (parse_expression tokens))
    | [] -> error [] "Expected a macro argument."

  let statement_of_raw_word word = Word word

  let parameter_kind = function
    | "reg" -> Some Register_kind
    | "expr" -> Some Expression_kind
    | "value" -> Some Value_kind
    | "perm" -> Some Permission_kind
    | "sealperm" -> Some Seal_permission_kind
    | "wtype" -> Some Word_type_kind
    | _ -> None

  let parameter_kind_name = function
    | Register_kind -> "reg"
    | Expression_kind -> "expr"
    | Value_kind -> "value"
    | Permission_kind -> "perm"
    | Seal_permission_kind -> "sealperm"
    | Word_type_kind -> "wtype"

  let argument_kind = function
    | Register_argument _ -> Register_kind
    | Constant_argument (Expression _) -> Expression_kind
    | Constant_argument (Permission _) -> Permission_kind
    | Constant_argument (Seal_permission _) -> Seal_permission_kind
    | Constant_argument (Word_type _) -> Word_type_kind
    | Constant_argument (Value_parameter _) -> Value_kind

  let accepts_argument kind argument =
    match (kind, argument_kind argument) with
    | Value_kind, _ -> true
    | Expression_kind, Expression_kind
    | Register_kind, Register_kind
    | Permission_kind, Permission_kind
    | Seal_permission_kind, Seal_permission_kind
    | Word_type_kind, Word_type_kind ->
        true
    | _ -> false

  let expression_of_argument = function Constant_argument (Expression e) -> Some e | _ -> None
  let map_register _ r = r
  let map_constant f = function Expression e -> Expression (f e) | c -> c

  let map_operand f = function
    | Register_term r -> Register_term r
    | Constant_term c -> Constant_term (map_constant f c)

  let map_instruction f = function
    | Jmp_term r -> Jmp_term r
    | Jnz_term (a, b) -> Jnz_term (a, b)
    | Load_term (a, b) -> Load_term (a, b)
    | GetB_term (a, b) -> GetB_term (a, b)
    | GetE_term (a, b) -> GetE_term (a, b)
    | GetA_term (a, b) -> GetA_term (a, b)
    | GetP_term (a, b) -> GetP_term (a, b)
    | GetOType_term (a, b) -> GetOType_term (a, b)
    | GetWType_term (a, b) -> GetWType_term (a, b)
    | Invoke_term (a, b) -> Invoke_term (a, b)
    | Seal_term (a, b, c) -> Seal_term (a, b, c)
    | UnSeal_term (a, b, c) -> UnSeal_term (a, b, c)
    | Move_term (r, o) -> Move_term (r, map_operand f o)
    | Store_term (r, o) -> Store_term (r, map_operand f o)
    | Lea_term (r, o) -> Lea_term (r, map_operand f o)
    | Restrict_term (r, o) -> Restrict_term (r, map_operand f o)
    | Add_term (r, a, b) -> Add_term (r, map_operand f a, map_operand f b)
    | Sub_term (r, a, b) -> Sub_term (r, map_operand f a, map_operand f b)
    | Mul_term (r, a, b) -> Mul_term (r, map_operand f a, map_operand f b)
    | Rem_term (r, a, b) -> Rem_term (r, map_operand f a, map_operand f b)
    | Div_term (r, a, b) -> Div_term (r, map_operand f a, map_operand f b)
    | Lt_term (r, a, b) -> Lt_term (r, map_operand f a, map_operand f b)
    | SubSeg_term (r, a, b) -> SubSeg_term (r, map_operand f a, map_operand f b)
    | Fail_term -> Fail_term
    | Halt_term -> Halt_term

  let map_sealable f = function
    | Cap_term (p, b, e, a) -> Cap_term (p, f b, f e, f a)
    | SealRange_term (p, b, e, a) -> SealRange_term (p, f b, f e, f a)

  let map_word f = function
    | I_term e -> I_term (f e)
    | Sealable_term s -> Sealable_term (map_sealable f s)
    | Sealed_term (o, s) -> Sealed_term (f o, map_sealable f s)

  let map_statement_expressions f = function
    | Op op -> Op (map_instruction f op)
    | Word w -> Word (map_word f w)

  let map_raw_word_expressions = map_word
  let map_regfile_expressions f rf = List.map (fun (r, w) -> (r, map_word f w)) rf

  let map_argument_expressions f = function
    | Constant_argument c -> Constant_argument (map_constant f c)
    | a -> a

  let rec expression_parameters parameters acc = function
    | Assembly_frontend.Expression.Parameter name -> (
        match List.assoc_opt name parameters with
        | Some (Expression_kind | Value_kind) -> acc
        | Some _ ->
            Diagnostic.error (Printf.sprintf "$%s is not an expression parameter." name) :: acc
        | None -> Diagnostic.error (Printf.sprintf "Unknown macro parameter $%s." name) :: acc)
    | Add (a, b) | Subtract (a, b) | Multiply (a, b) | Logand (a, b) | Logor (a, b)
    | Shift_left (a, b) | Shift_right (a, b) ->
        expression_parameters parameters (expression_parameters parameters acc a) b
    | _ -> acc

  let validate_register parameters acc = function
    | Named _ -> acc
    | Register_parameter n -> (
        match List.assoc_opt n parameters with
        | Some (Register_kind | Value_kind) -> acc
        | _ -> Diagnostic.error (Printf.sprintf "Invalid register parameter $%s." n) :: acc)

  let validate_constant parameters acc = function
    | Expression e -> expression_parameters parameters acc e
    | Value_parameter n -> (
        match List.assoc_opt n parameters with
        | Some
            (Value_kind | Expression_kind | Permission_kind | Seal_permission_kind | Word_type_kind)
          ->
            acc
        | _ -> Diagnostic.error (Printf.sprintf "Invalid value parameter $%s." n) :: acc)
    | _ -> acc

  let validate_operand parameters acc = function
    | Register_term r -> validate_register parameters acc r
    | Constant_term c -> validate_constant parameters acc c

  let validate_permission parameters acc = function
    | Permission_literal _ -> acc
    | Permission_parameter name -> (
        match List.assoc_opt name parameters with
        | Some Permission_kind -> acc
        | _ -> Diagnostic.error (Printf.sprintf "Invalid permission parameter $%s." name) :: acc)

  let validate_seal_permission parameters acc = function
    | Seal_permission_literal _ -> acc
    | Seal_permission_parameter name -> (
        match List.assoc_opt name parameters with
        | Some Seal_permission_kind -> acc
        | _ ->
            Diagnostic.error (Printf.sprintf "Invalid seal-permission parameter $%s." name) :: acc)

  let validate_word_terms parameters acc = function
    | I_term _ -> acc
    | Sealable_term (Cap_term (permission, _, _, _))
    | Sealed_term (_, Cap_term (permission, _, _, _)) ->
        validate_permission parameters acc permission
    | Sealable_term (SealRange_term (permission, _, _, _))
    | Sealed_term (_, SealRange_term (permission, _, _, _)) ->
        validate_seal_permission parameters acc permission

  let validate_statement ~parameters statement =
    let acc = ref [] in
    let r x = acc := validate_register parameters !acc x
    and o x = acc := validate_operand parameters !acc x in
    (match statement with
    | Word w ->
        acc := validate_word_terms parameters !acc w;
        ignore
          (map_word
             (fun e ->
               acc := expression_parameters parameters !acc e;
               e)
             w)
    | Op op -> (
        match op with
        | Jmp_term a -> r a
        | Jnz_term (a, b)
        | Load_term (a, b)
        | GetB_term (a, b)
        | GetE_term (a, b)
        | GetA_term (a, b)
        | GetP_term (a, b)
        | GetOType_term (a, b)
        | GetWType_term (a, b)
        | Invoke_term (a, b) ->
            r a;
            r b
        | Move_term (a, b) | Store_term (a, b) | Lea_term (a, b) | Restrict_term (a, b) ->
            r a;
            o b
        | Add_term (a, b, c)
        | Sub_term (a, b, c)
        | Mul_term (a, b, c)
        | Rem_term (a, b, c)
        | Div_term (a, b, c)
        | Lt_term (a, b, c)
        | SubSeg_term (a, b, c) ->
            r a;
            o b;
            o c
        | Seal_term (a, b, c) | UnSeal_term (a, b, c) ->
            r a;
            r b;
            r c
        | Fail_term | Halt_term -> ()));
    List.rev !acc

  let validate_raw_word ~parameters word = validate_statement ~parameters (Word word)
  let lookup arguments name = List.assoc_opt name arguments

  let sub_register arguments = function
    | Named _ as r -> Ok r
    | Register_parameter n -> (
        match lookup arguments n with
        | Some (Register_argument r) -> Ok (Named r)
        | _ -> Error [ Diagnostic.error (Printf.sprintf "No register argument for $%s." n) ])

  let sub_operand arguments = function
    | Register_term r -> Result.map (fun r -> Register_term r) (sub_register arguments r)
    | Constant_term (Value_parameter n) -> (
        match lookup arguments n with
        | Some (Register_argument r) -> Ok (Register_term (Named r))
        | Some (Constant_argument c) -> Ok (Constant_term c)
        | None -> Error [ Diagnostic.error (Printf.sprintf "No value argument for $%s." n) ])
    | o -> Ok o

  let sub_permission arguments = function
    | Permission_literal _ as permission -> Ok permission
    | Permission_parameter name -> (
        match lookup arguments name with
        | Some (Constant_argument (Permission permission)) -> Ok (Permission_literal permission)
        | _ -> Error [ Diagnostic.error (Printf.sprintf "No permission argument for $%s." name) ])

  let sub_seal_permission arguments = function
    | Seal_permission_literal _ as permission -> Ok permission
    | Seal_permission_parameter name -> (
        match lookup arguments name with
        | Some (Constant_argument (Seal_permission permission)) ->
            Ok (Seal_permission_literal permission)
        | _ ->
            Error [ Diagnostic.error (Printf.sprintf "No seal-permission argument for $%s." name) ])

  let sub_word arguments = function
    | I_term _ as word -> Ok word
    | Sealable_term (Cap_term (permission, b, e, a)) ->
        Result.map
          (fun permission -> Sealable_term (Cap_term (permission, b, e, a)))
          (sub_permission arguments permission)
    | Sealed_term (otype, Cap_term (permission, b, e, a)) ->
        Result.map
          (fun permission -> Sealed_term (otype, Cap_term (permission, b, e, a)))
          (sub_permission arguments permission)
    | Sealable_term (SealRange_term (permission, b, e, a)) ->
        Result.map
          (fun permission -> Sealable_term (SealRange_term (permission, b, e, a)))
          (sub_seal_permission arguments permission)
    | Sealed_term (otype, SealRange_term (permission, b, e, a)) ->
        Result.map
          (fun permission -> Sealed_term (otype, SealRange_term (permission, b, e, a)))
          (sub_seal_permission arguments permission)

  let bind1 c arguments a = Result.map c (sub_register arguments a)

  let bind2 c arguments a b =
    match (sub_register arguments a, sub_register arguments b) with
    | Ok a, Ok b -> Ok (c a b)
    | Error e, _ -> Error e
    | _, Error e -> Error e

  let bind3 c arguments a b d =
    match (sub_register arguments a, sub_register arguments b, sub_register arguments d) with
    | Ok a, Ok b, Ok d -> Ok (c a b d)
    | Error e, _, _ -> Error e
    | _, Error e, _ -> Error e
    | _, _, Error e -> Error e

  let bindro c arguments r o =
    match (sub_register arguments r, sub_operand arguments o) with
    | Ok r, Ok o -> Ok (c r o)
    | Error e, _ -> Error e
    | _, Error e -> Error e

  let bindroo c arguments r a b =
    match (sub_register arguments r, sub_operand arguments a, sub_operand arguments b) with
    | Ok r, Ok a, Ok b -> Ok (c r a b)
    | Error e, _, _ -> Error e
    | _, Error e, _ -> Error e
    | _, _, Error e -> Error e

  let substitute_instruction arguments = function
    | Jmp_term a -> bind1 (fun a -> Jmp_term a) arguments a
    | Jnz_term (a, b) -> bind2 (fun a b -> Jnz_term (a, b)) arguments a b
    | Load_term (a, b) -> bind2 (fun a b -> Load_term (a, b)) arguments a b
    | GetB_term (a, b) -> bind2 (fun a b -> GetB_term (a, b)) arguments a b
    | GetE_term (a, b) -> bind2 (fun a b -> GetE_term (a, b)) arguments a b
    | GetA_term (a, b) -> bind2 (fun a b -> GetA_term (a, b)) arguments a b
    | GetP_term (a, b) -> bind2 (fun a b -> GetP_term (a, b)) arguments a b
    | GetOType_term (a, b) -> bind2 (fun a b -> GetOType_term (a, b)) arguments a b
    | GetWType_term (a, b) -> bind2 (fun a b -> GetWType_term (a, b)) arguments a b
    | Invoke_term (a, b) -> bind2 (fun a b -> Invoke_term (a, b)) arguments a b
    | Move_term (r, o) -> bindro (fun r o -> Move_term (r, o)) arguments r o
    | Store_term (r, o) -> bindro (fun r o -> Store_term (r, o)) arguments r o
    | Lea_term (r, o) -> bindro (fun r o -> Lea_term (r, o)) arguments r o
    | Restrict_term (r, o) -> bindro (fun r o -> Restrict_term (r, o)) arguments r o
    | Add_term (r, a, b) -> bindroo (fun r a b -> Add_term (r, a, b)) arguments r a b
    | Sub_term (r, a, b) -> bindroo (fun r a b -> Sub_term (r, a, b)) arguments r a b
    | Mul_term (r, a, b) -> bindroo (fun r a b -> Mul_term (r, a, b)) arguments r a b
    | Rem_term (r, a, b) -> bindroo (fun r a b -> Rem_term (r, a, b)) arguments r a b
    | Div_term (r, a, b) -> bindroo (fun r a b -> Div_term (r, a, b)) arguments r a b
    | Lt_term (r, a, b) -> bindroo (fun r a b -> Lt_term (r, a, b)) arguments r a b
    | SubSeg_term (r, a, b) -> bindroo (fun r a b -> SubSeg_term (r, a, b)) arguments r a b
    | Seal_term (a, b, c) -> bind3 (fun a b c -> Seal_term (a, b, c)) arguments a b c
    | UnSeal_term (a, b, c) -> bind3 (fun a b c -> UnSeal_term (a, b, c)) arguments a b c
    | Fail_term -> Ok Fail_term
    | Halt_term -> Ok Halt_term

  let substitute_statement ~arguments = function
    | Op op -> Result.map (fun op -> Op op) (substitute_instruction arguments op)
    | Word w -> Result.map (fun word -> Word word) (sub_word arguments w)

  let substitute_raw_word ~arguments w = sub_word arguments w

  let substitute_argument ~arguments = function
    | Register_argument _ as a -> Ok a
    | Constant_argument (Value_parameter n) -> (
        match lookup arguments n with
        | Some a -> Ok a
        | None -> Error [ Diagnostic.error (Printf.sprintf "No argument for $%s." n) ])
    | a -> Ok a
end

module Frontend = Assembly_frontend.Make (Syntax)
include Frontend
