open Ast
open Assembly_frontend

let ( let* ) = Result.bind

let error tokens message =
  match tokens with
  | token :: _ -> Error [ Diagnostic.error ~location:(Token.location token) message ]
  | [] -> Error [ Diagnostic.error message ]

let punctuation expected = function
  | token :: rest when Token.kind token = Punctuation expected -> Ok rest
  | tokens -> error tokens (Printf.sprintf "Expected `%c`." expected)

let identifier = function
  | token :: rest -> (
      match Token.kind token with
      | Identifier name -> Ok (name, rest)
      | _ -> error [ token ] "Expected a name.")
  | [] -> error [] "Expected a name."

let parse_register_name name =
  match String.lowercase_ascii name with
  | "pc" -> Some PC
  | "cnull" -> Some cnull
  | "cra" -> Some cra
  | "csp" -> Some csp
  | "cgp" -> Some cgp
  | "ctp" -> Some ctp
  | "ct0" -> Some ct0
  | "ct1" -> Some ct1
  | "ct2" -> Some ct2
  | "ct3" -> Some ct3
  | "ct4" -> Some ct4
  | "ct5" -> Some ct5
  | "ct6" -> Some ct6
  | "cs0" -> Some cs0
  | "cs1" -> Some cs1
  | "cs2" -> Some cs2
  | "cs3" -> Some cs3
  | "cs4" -> Some cs4
  | "cs5" -> Some cs5
  | "cs6" -> Some cs6
  | "cs7" -> Some cs7
  | "cs8" -> Some cs8
  | "cs9" -> Some cs9
  | "cs10" -> Some cs10
  | "cs11" -> Some cs11
  | "ca0" -> Some ca0
  | "ca1" -> Some ca1
  | "ca2" -> Some ca2
  | "ca3" -> Some ca3
  | "ca4" -> Some ca4
  | "ca5" -> Some ca5
  | "ca6" -> Some ca6
  | "ca7" -> Some ca7
  | name when String.length name > 1 && name.[0] = 'r' ->
      Option.bind
        (int_of_string_opt (String.sub name 1 (String.length name - 1)))
        (fun n -> if n >= 0 && n <= 31 then Some (Reg n) else None)
  | _ -> None

let parse_register = function
  | token :: rest -> (
      match Token.kind token with
      | Identifier name -> (
          match parse_register_name name with
          | Some r -> Ok (Named r, rest)
          | None -> error [ token ] "Expected a Griotte register.")
      | Parameter name -> Ok (Register_parameter name, rest)
      | _ -> error [ token ] "Expected a Griotte register.")
  | [] -> error [] "Expected a Griotte register."

let parse_system_register = function
  | token :: rest when Token.kind token = Identifier "mtdc" || Token.kind token = Identifier "MTDC"
    ->
      Ok (MTDC, rest)
  | tokens -> error tokens "Expected the Griotte system register MTDC."

let parse_rx = function
  | "orx" -> Some Orx
  | "r" -> Some R
  | "x" -> Some X
  | "xsr" -> Some XSR
  | _ -> None

let parse_write = function "ow" -> Some Ow | "w" -> Some W | "wl" -> Some WL | _ -> None
let parse_dl = function "dl" -> Some DL | "lg" -> Some LG | _ -> None
let parse_dro = function "dro" -> Some DRO | "lm" -> Some LM | _ -> None

let parse_locality_name name =
  match String.lowercase_ascii name with
  | "local" -> Some Local
  | "global" -> Some Global
  | _ -> None

let parse_seal_permission_name name =
  match String.uppercase_ascii name with
  | "SO" -> Some (false, false)
  | "S" -> Some (true, false)
  | "U" -> Some (false, true)
  | "SU" -> Some (true, true)
  | _ -> None

let parse_word_type_name name =
  match String.lowercase_ascii name with
  | "int" -> Some W_I
  | "cap" -> Some W_Cap
  | "sealrange" -> Some W_SealRange
  | "sealed" -> Some W_Sealed
  | "sentry" -> Some W_Sentry
  | _ -> None

let parse_permission_term = function
  | token :: rest -> (
      match Token.kind token with
      | Parameter name -> Ok (Permission_parameter name, rest)
      | Identifier "O" -> Ok (Permission_literal null_permission, rest)
      | _ -> (
          match token :: rest with
          | open_t :: rx_t :: w_t :: dl_t :: dro_t :: close_t :: rest
            when Token.kind open_t = Punctuation '[' && Token.kind close_t = Punctuation ']' -> (
              match (Token.kind rx_t, Token.kind w_t, Token.kind dl_t, Token.kind dro_t) with
              | Identifier rx, Identifier w, Identifier dl, Identifier dro -> (
                  match
                    ( parse_rx (String.lowercase_ascii rx),
                      parse_write (String.lowercase_ascii w),
                      parse_dl (String.lowercase_ascii dl),
                      parse_dro (String.lowercase_ascii dro) )
                  with
                  | Some rx, Some w, Some dl, Some dro ->
                      Ok (Permission_literal (rx, w, dl, dro), rest)
                  | _ -> error [ rx_t ] "Invalid Griotte composite permission component.")
              | _ -> error [ rx_t ] "Expected four Griotte composite permission components.")
          | tokens ->
              error tokens
                "Expected O, [rx write deep-local deep-read-only], or a permission parameter."))
  | [] -> error [] "Expected O, [rx write deep-local deep-read-only], or a permission parameter."

let parse_seal_permission_term = function
  | token :: rest -> (
      match Token.kind token with
      | Parameter name -> Ok (Seal_permission_parameter name, rest)
      | Identifier name -> (
          match parse_seal_permission_name name with
          | Some p -> Ok (Seal_permission_literal p, rest)
          | None -> error [ token ] "Expected SO, S, U, or SU.")
      | _ -> error [ token ] "Expected a sealing permission.")
  | [] -> error [] "Expected a sealing permission."

let parse_locality_term = function
  | token :: rest -> (
      match Token.kind token with
      | Parameter name -> Ok (Locality_parameter name, rest)
      | Identifier name -> (
          match parse_locality_name name with
          | Some l -> Ok (Locality_literal l, rest)
          | None -> error [ token ] "Expected Local or Global.")
      | _ -> error [ token ] "Expected Local or Global.")
  | [] -> error [] "Expected Local or Global."

let parse_word_type_term = function
  | token :: rest -> (
      match Token.kind token with
      | Parameter name -> Ok (Word_type_parameter name, rest)
      | Identifier name -> (
          match parse_word_type_name name with
          | Some w -> Ok (Word_type_literal w, rest)
          | None -> error [ token ] "Expected Int, Cap, SealRange, Sealed, or Sentry.")
      | _ -> error [ token ] "Expected a Griotte word type.")
  | [] -> error [] "Expected a Griotte word type."

let parse_pair first second construct tokens =
  let ( let* ) = Result.bind in
  let* rest = punctuation '(' tokens in
  let* a, rest = first rest in
  let* rest = punctuation ',' rest in
  let* b, rest = second rest in
  let* rest = punctuation ')' rest in
  Ok (construct a b, rest)

let parse_scalar_constant = function
  | token :: rest -> (
      match Token.kind token with
      | Identifier name -> (
          match
            (parse_seal_permission_name name, parse_word_type_name name, parse_locality_name name)
          with
          | Some p, _, _ -> Some (Seal_permission p, rest)
          | _, Some w, _ -> Some (Word_type w, rest)
          | _, _, Some l -> Some (Locality l, rest)
          | _ -> None)
      | _ -> None)
  | [] -> None

let parse_operand tokens =
  match tokens with
  | token :: _ when Token.kind token = Punctuation '(' -> (
      match
        parse_pair parse_permission_term parse_locality_term
          (fun p l -> Permission_locality (p, l))
          tokens
      with
      | Ok (c, rest) -> Ok (Constant_term c, rest)
      | Error _ -> (
          match
            parse_pair parse_seal_permission_term parse_locality_term
              (fun p l -> Seal_permission_locality (p, l))
              tokens
          with
          | Ok (c, rest) -> Ok (Constant_term c, rest)
          | Error _ ->
              Result.map
                (fun (e, rest) -> (Constant_term (Expression e), rest))
                (parse_expression tokens)))
  | token :: rest -> (
      match Token.kind token with
      | Parameter name -> Ok (Constant_term (Value_parameter name), rest)
      | Identifier name -> (
          match (parse_register_name name, parse_scalar_constant tokens) with
          | Some r, _ -> Ok (Register_term (Named r), rest)
          | None, Some (c, rest) -> Ok (Constant_term c, rest)
          | None, None -> (
              match parse_permission_term tokens with
              | Ok (Permission_literal p, rest) -> Ok (Constant_term (Permission p), rest)
              | _ ->
                  Result.map
                    (fun (e, rest) -> (Constant_term (Expression e), rest))
                    (parse_expression tokens)))
      | Punctuation '[' -> (
          match parse_permission_term tokens with
          | Ok (Permission_literal p, rest) -> Ok (Constant_term (Permission p), rest)
          | Ok _ -> error tokens "A permission parameter must be used as $name."
          | Error _ as e -> e)
      | _ ->
          Result.map
            (fun (e, rest) -> (Constant_term (Expression e), rest))
            (parse_expression tokens))
  | [] -> error [] "Expected a Griotte register or constant."

let unary parser construct tokens = Result.map (fun (a, r) -> (construct a, r)) (parser tokens)

let binary p1 p2 construct tokens =
  match p1 tokens with
  | Error _ as e -> e
  | Ok (a, tokens) -> Result.map (fun (b, r) -> (construct a b, r)) (p2 tokens)

let ternary p1 p2 p3 construct tokens =
  match p1 tokens with
  | Error _ as e -> e
  | Ok (a, tokens) -> (
      match p2 tokens with
      | Error _ as e -> e
      | Ok (b, tokens) -> Result.map (fun (c, r) -> (construct a b c, r)) (p3 tokens))

let parse_instruction = function
  | token :: rest -> (
      match Token.kind token with
      | Identifier name ->
          let parsed =
            match String.lowercase_ascii name with
            | "jalr" -> binary parse_register parse_register (fun a b -> Jalr_term (a, b)) rest
            | "jmp" -> unary parse_operand (fun a -> Jmp_term a) rest
            | "jnz" -> binary parse_register parse_operand (fun a b -> Jnz_term (a, b)) rest
            | "readsr" ->
                binary parse_register parse_system_register (fun a b -> ReadSR_term (a, b)) rest
            | "writesr" ->
                binary parse_system_register parse_register (fun a b -> WriteSR_term (a, b)) rest
            | "mov" | "move" ->
                binary parse_register parse_operand (fun a b -> Move_term (a, b)) rest
            | "load" -> binary parse_register parse_register (fun a b -> Load_term (a, b)) rest
            | "store" -> binary parse_register parse_operand (fun a b -> Store_term (a, b)) rest
            | "add" ->
                ternary parse_register parse_operand parse_operand
                  (fun a b c -> Add_term (a, b, c))
                  rest
            | "sub" ->
                ternary parse_register parse_operand parse_operand
                  (fun a b c -> Sub_term (a, b, c))
                  rest
            | "mul" ->
                ternary parse_register parse_operand parse_operand
                  (fun a b c -> Mul_term (a, b, c))
                  rest
            | "land" ->
                ternary parse_register parse_operand parse_operand
                  (fun a b c -> LAnd_term (a, b, c))
                  rest
            | "lor" ->
                ternary parse_register parse_operand parse_operand
                  (fun a b c -> LOr_term (a, b, c))
                  rest
            | "lshiftl" ->
                ternary parse_register parse_operand parse_operand
                  (fun a b c -> LShiftL_term (a, b, c))
                  rest
            | "lshiftr" ->
                ternary parse_register parse_operand parse_operand
                  (fun a b c -> LShiftR_term (a, b, c))
                  rest
            | "lt" ->
                ternary parse_register parse_operand parse_operand
                  (fun a b c -> Lt_term (a, b, c))
                  rest
            | "lea" -> binary parse_register parse_operand (fun a b -> Lea_term (a, b)) rest
            | "restrict" ->
                binary parse_register parse_operand (fun a b -> Restrict_term (a, b)) rest
            | "subseg" ->
                ternary parse_register parse_operand parse_operand
                  (fun a b c -> SubSeg_term (a, b, c))
                  rest
            | "getl" -> binary parse_register parse_register (fun a b -> GetL_term (a, b)) rest
            | "getb" -> binary parse_register parse_register (fun a b -> GetB_term (a, b)) rest
            | "gete" -> binary parse_register parse_register (fun a b -> GetE_term (a, b)) rest
            | "geta" -> binary parse_register parse_register (fun a b -> GetA_term (a, b)) rest
            | "getp" -> binary parse_register parse_register (fun a b -> GetP_term (a, b)) rest
            | "getotype" ->
                binary parse_register parse_register (fun a b -> GetOType_term (a, b)) rest
            | "getwtype" ->
                binary parse_register parse_register (fun a b -> GetWType_term (a, b)) rest
            | "seal" ->
                ternary parse_register parse_register parse_register
                  (fun a b c -> Seal_term (a, b, c))
                  rest
            | "unseal" ->
                ternary parse_register parse_register parse_register
                  (fun a b c -> UnSeal_term (a, b, c))
                  rest
            | "fail" -> Ok (Fail_term, rest)
            | "halt" -> Ok (Halt_term, rest)
            | unsupported ->
                error [ token ] (Printf.sprintf "Unsupported Griotte instruction `%s`." unsupported)
          in
          parsed
      | _ -> error [ token ] "Expected a Griotte instruction.")
  | [] -> error [] "Expected a Griotte instruction."

let parse_fields5 p1 p2 tokens =
  let ( let* ) = Result.bind in
  let* a, tokens = p1 tokens in
  let* tokens = punctuation ',' tokens in
  let* b, tokens = p2 tokens in
  let* tokens = punctuation ',' tokens in
  let* c, tokens = parse_expression tokens in
  let* tokens = punctuation ',' tokens in
  let* d, tokens = parse_expression tokens in
  let* tokens = punctuation ',' tokens in
  let* e, tokens = parse_expression tokens in
  Ok ((a, b, c, d, e), tokens)

let parse_sealable = function
  | open_t :: rest when Token.kind open_t = Punctuation '(' ->
      let ( let* ) = Result.bind in
      let* (p, l, b, e, a), rest = parse_fields5 parse_permission_term parse_locality_term rest in
      Result.map (fun rest -> (Cap_term (p, l, b, e, a), rest)) (punctuation ')' rest)
  | open_t :: rest when Token.kind open_t = Punctuation '[' ->
      let ( let* ) = Result.bind in
      let* (p, l, b, e, a), rest =
        parse_fields5 parse_seal_permission_term parse_locality_term rest
      in
      Result.map (fun rest -> (Seal_range_term (p, l, b, e, a), rest)) (punctuation ']' rest)
  | tokens -> error tokens "Expected a Griotte capability or seal range."

let parse_word = function
  | open_t :: e_t :: minus_t :: rest
    when Token.kind open_t = Punctuation '('
         && Token.kind e_t = Identifier "E"
         && Token.kind minus_t = Punctuation '-' ->
      let ( let* ) = Result.bind in
      let* (p, l, b, e, a), rest = parse_fields5 parse_permission_term parse_locality_term rest in
      Result.map (fun rest -> (Sentry_term (p, l, b, e, a), rest)) (punctuation ')' rest)
  | open_t :: rest when Token.kind open_t = Punctuation '{' ->
      let ( let* ) = Result.bind in
      let* otype, rest = parse_expression rest in
      let* rest = punctuation ':' rest in
      let* sealable, rest = parse_sealable rest in
      Result.map (fun rest -> (Sealed_term (otype, sealable), rest)) (punctuation '}' rest)
  | open_t :: _ as tokens when Token.kind open_t = Punctuation '[' ->
      Result.map (fun (s, rest) -> (Sealable_term s, rest)) (parse_sealable tokens)
  | open_t :: _ as tokens when Token.kind open_t = Punctuation '(' -> (
      match parse_sealable tokens with
      | Ok (s, rest) -> Ok (Sealable_term s, rest)
      | Error _ -> Result.map (fun (e, rest) -> (I_term e, rest)) (parse_expression tokens))
  | tokens -> Result.map (fun (e, rest) -> (I_term e, rest)) (parse_expression tokens)

type parameter_kind =
  | Register_kind
  | Expression_kind
  | Value_kind
  | Permission_kind
  | Seal_permission_kind
  | Locality_kind
  | Word_type_kind

type macro_argument = Register_argument of register | Constant_argument of constant_term

module Syntax = struct
  type nonrec statement = statement
  type raw_word = word_term
  type nonrec regfile = regfile
  type nonrec macro_argument = macro_argument
  type nonrec parameter_kind = parameter_kind

  let parse_statement ts = Result.map (fun (x, r) -> (Op x, r)) (parse_instruction ts)
  let parse_raw_word = parse_word

  let parse_regfile tokens =
    let rec loop regs sregs = function
      | [] -> Ok ((List.rev regs, List.rev sregs), [])
      | token :: assign :: rest when Token.kind assign = Assign -> (
          match Token.kind token with
          | Identifier name -> (
              match (parse_register_name name, String.lowercase_ascii name) with
              | Some r, _ ->
                  Result.bind (parse_word rest) (fun (w, rest) -> loop ((r, w) :: regs) sregs rest)
              | None, "mtdc" ->
                  Result.bind (parse_word rest) (fun (w, rest) ->
                      loop regs ((MTDC, w) :: sregs) rest)
              | _ -> error [ token ] "Expected a Griotte register or MTDC assignment.")
          | _ -> error [ token ] "Expected a Griotte register assignment.")
      | tokens -> error tokens "Expected `register := word`."
    in
    loop [] [] tokens

  let parse_macro_argument tokens =
    match parse_operand tokens with
    | Ok (Register_term (Named r), rest) -> Ok (Register_argument r, rest)
    | Ok (Constant_term c, rest) -> Ok (Constant_argument c, rest)
    | Ok _ -> error tokens "A macro argument cannot contain a register parameter."
    | Error e -> Error e

  let statement_of_raw_word w = Word w

  let parameter_kind = function
    | "reg" -> Some Register_kind
    | "expr" -> Some Expression_kind
    | "value" -> Some Value_kind
    | "perm" -> Some Permission_kind
    | "sealperm" -> Some Seal_permission_kind
    | "locality" -> Some Locality_kind
    | "wtype" -> Some Word_type_kind
    | _ -> None

  let parameter_kind_name = function
    | Register_kind -> "reg"
    | Expression_kind -> "expr"
    | Value_kind -> "value"
    | Permission_kind -> "perm"
    | Seal_permission_kind -> "sealperm"
    | Locality_kind -> "locality"
    | Word_type_kind -> "wtype"

  let argument_kind = function
    | Register_argument _ -> Register_kind
    | Constant_argument (Expression _) -> Expression_kind
    | Constant_argument (Permission _) -> Permission_kind
    | Constant_argument (Seal_permission _) -> Seal_permission_kind
    | Constant_argument (Locality _) -> Locality_kind
    | Constant_argument (Word_type _) -> Word_type_kind
    | Constant_argument _ -> Value_kind

  let accepts_argument kind arg = kind = Value_kind || kind = argument_kind arg
  let expression_of_argument = function Constant_argument (Expression e) -> Some e | _ -> None
  let map_constant f = function Expression e -> Expression (f e) | c -> c

  let map_operand f = function
    | Register_term r -> Register_term r
    | Constant_term c -> Constant_term (map_constant f c)

  let map_sealable f = function
    | Cap_term (p, l, b, e, a) -> Cap_term (p, l, f b, f e, f a)
    | Seal_range_term (p, l, b, e, a) -> Seal_range_term (p, l, f b, f e, f a)

  let map_word f = function
    | I_term e -> I_term (f e)
    | Sealable_term s -> Sealable_term (map_sealable f s)
    | Sentry_term (p, l, b, e, a) -> Sentry_term (p, l, f b, f e, f a)
    | Sealed_term (o, s) -> Sealed_term (f o, map_sealable f s)

  let map_instruction f = function
    | Jmp_term o -> Jmp_term (map_operand f o)
    | Jnz_term (r, o) -> Jnz_term (r, map_operand f o)
    | Move_term (r, o) -> Move_term (r, map_operand f o)
    | Store_term (r, o) -> Store_term (r, map_operand f o)
    | Add_term (r, a, b) -> Add_term (r, map_operand f a, map_operand f b)
    | Sub_term (r, a, b) -> Sub_term (r, map_operand f a, map_operand f b)
    | Mul_term (r, a, b) -> Mul_term (r, map_operand f a, map_operand f b)
    | LAnd_term (r, a, b) -> LAnd_term (r, map_operand f a, map_operand f b)
    | LOr_term (r, a, b) -> LOr_term (r, map_operand f a, map_operand f b)
    | LShiftL_term (r, a, b) -> LShiftL_term (r, map_operand f a, map_operand f b)
    | LShiftR_term (r, a, b) -> LShiftR_term (r, map_operand f a, map_operand f b)
    | Lt_term (r, a, b) -> Lt_term (r, map_operand f a, map_operand f b)
    | Lea_term (r, o) -> Lea_term (r, map_operand f o)
    | Restrict_term (r, o) -> Restrict_term (r, map_operand f o)
    | SubSeg_term (r, a, b) -> SubSeg_term (r, map_operand f a, map_operand f b)
    | op -> op

  let map_statement_expressions f = function
    | Op op -> Op (map_instruction f op)
    | Word w -> Word (map_word f w)

  let map_raw_word_expressions = map_word

  let map_regfile_expressions f (regs, sregs) =
    ( List.map (fun (r, w) -> (r, map_word f w)) regs,
      List.map (fun (r, w) -> (r, map_word f w)) sregs )

  let map_argument_expressions f = function
    | Constant_argument c -> Constant_argument (map_constant f c)
    | a -> a

  let rec validate_expression parameters acc = function
    | Expression.Parameter name -> (
        match List.assoc_opt name parameters with
        | Some (Expression_kind | Value_kind) -> acc
        | Some _ ->
            Diagnostic.error (Printf.sprintf "$%s is not an expression parameter." name) :: acc
        | None -> Diagnostic.error (Printf.sprintf "Unknown macro parameter $%s." name) :: acc)
    | Add (a, b)
    | Subtract (a, b)
    | Multiply (a, b)
    | Logand (a, b)
    | Logor (a, b)
    | Shift_left (a, b)
    | Shift_right (a, b) ->
        validate_expression parameters (validate_expression parameters acc a) b
    | _ -> acc

  let valid_param parameters expected name message acc =
    match List.assoc_opt name parameters with
    | Some kind when List.mem kind expected -> acc
    | _ -> Diagnostic.error (Printf.sprintf message name) :: acc

  let vr parameters acc = function
    | Named _ -> acc
    | Register_parameter n ->
        valid_param parameters [ Register_kind; Value_kind ] n "Invalid register parameter $%s." acc

  let vp parameters acc = function
    | Permission_literal _ -> acc
    | Permission_parameter n ->
        valid_param parameters [ Permission_kind ] n "Invalid permission parameter $%s." acc

  let vsp parameters acc = function
    | Seal_permission_literal _ -> acc
    | Seal_permission_parameter n ->
        valid_param parameters [ Seal_permission_kind ] n "Invalid seal permission parameter $%s."
          acc

  let vl parameters acc = function
    | Locality_literal _ -> acc
    | Locality_parameter n ->
        valid_param parameters [ Locality_kind ] n "Invalid locality parameter $%s." acc

  let vc parameters acc = function
    | Expression e -> validate_expression parameters acc e
    | Value_parameter n ->
        valid_param parameters
          [
            Register_kind;
            Value_kind;
            Expression_kind;
            Permission_kind;
            Seal_permission_kind;
            Locality_kind;
            Word_type_kind;
          ]
          n "Invalid value parameter $%s." acc
    | Permission_locality (p, l) -> vl parameters (vp parameters acc p) l
    | Seal_permission_locality (p, l) -> vl parameters (vsp parameters acc p) l
    | _ -> acc

  let vo parameters acc = function
    | Register_term r -> vr parameters acc r
    | Constant_term c -> vc parameters acc c

  let vs parameters acc = function
    | Cap_term (p, l, b, e, a) ->
        validate_expression parameters
          (validate_expression parameters
             (validate_expression parameters (vl parameters (vp parameters acc p) l) b)
             e)
          a
    | Seal_range_term (p, l, b, e, a) ->
        validate_expression parameters
          (validate_expression parameters
             (validate_expression parameters (vl parameters (vsp parameters acc p) l) b)
             e)
          a

  let vw parameters acc = function
    | I_term e -> validate_expression parameters acc e
    | Sealable_term s -> vs parameters acc s
    | Sentry_term (p, l, b, e, a) ->
        validate_expression parameters
          (validate_expression parameters
             (validate_expression parameters (vl parameters (vp parameters acc p) l) b)
             e)
          a
    | Sealed_term (o, s) -> vs parameters (validate_expression parameters acc o) s

  let validate_statement ~parameters statement =
    let acc = ref [] in
    let r x = acc := vr parameters !acc x and o x = acc := vo parameters !acc x in
    (match statement with
    | Word w -> acc := vw parameters !acc w
    | Op op -> (
        match op with
        | Jmp_term a -> o a
        | Jalr_term (a, b)
        | Load_term (a, b)
        | GetL_term (a, b)
        | GetB_term (a, b)
        | GetE_term (a, b)
        | GetA_term (a, b)
        | GetP_term (a, b)
        | GetOType_term (a, b)
        | GetWType_term (a, b) ->
            r a;
            r b
        | Jnz_term (a, b)
        | Move_term (a, b)
        | Store_term (a, b)
        | Lea_term (a, b)
        | Restrict_term (a, b) ->
            r a;
            o b
        | ReadSR_term (a, _) | WriteSR_term (_, a) -> r a
        | Add_term (a, b, c)
        | Sub_term (a, b, c)
        | Mul_term (a, b, c)
        | LAnd_term (a, b, c)
        | LOr_term (a, b, c)
        | LShiftL_term (a, b, c)
        | LShiftR_term (a, b, c)
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

  let validate_raw_word ~parameters word = List.rev (vw parameters [] word)
  let lookup args name = List.assoc_opt name args

  let sr args = function
    | Named _ as r -> Ok r
    | Register_parameter n -> (
        match lookup args n with
        | Some (Register_argument r) -> Ok (Named r)
        | _ -> error [] (Printf.sprintf "No register argument for $%s." n))

  let sp args = function
    | Permission_literal _ as p -> Ok p
    | Permission_parameter n -> (
        match lookup args n with
        | Some (Constant_argument (Permission p)) -> Ok (Permission_literal p)
        | _ -> error [] (Printf.sprintf "No permission argument for $%s." n))

  let ssp args = function
    | Seal_permission_literal _ as p -> Ok p
    | Seal_permission_parameter n -> (
        match lookup args n with
        | Some (Constant_argument (Seal_permission p)) -> Ok (Seal_permission_literal p)
        | _ -> error [] (Printf.sprintf "No seal permission argument for $%s." n))

  let sl args = function
    | Locality_literal _ as l -> Ok l
    | Locality_parameter n -> (
        match lookup args n with
        | Some (Constant_argument (Locality l)) -> Ok (Locality_literal l)
        | _ -> error [] (Printf.sprintf "No locality argument for $%s." n))

  let sc args = function
    | Value_parameter n -> (
        match lookup args n with
        | Some (Constant_argument c) -> Ok (Constant_term c)
        | Some (Register_argument r) -> Ok (Register_term (Named r))
        | None -> error [] (Printf.sprintf "No value argument for $%s." n))
    | Permission_locality (p, l) ->
        let* p = sp args p in
        Result.map (fun l -> Constant_term (Permission_locality (p, l))) (sl args l)
    | Seal_permission_locality (p, l) ->
        let* p = ssp args p in
        Result.map (fun l -> Constant_term (Seal_permission_locality (p, l))) (sl args l)
    | c -> Ok (Constant_term c)

  let so args = function
    | Register_term r -> Result.map (fun r -> Register_term r) (sr args r)
    | Constant_term c -> sc args c

  let b1 c args a = Result.map c (sr args a)

  let b2 c args a b =
    let* a = sr args a in
    Result.map (c a) (sr args b)

  let bo c args o = Result.map c (so args o)

  let bro c args r o =
    let* r = sr args r in
    Result.map (c r) (so args o)

  let broo c args r a b =
    let* r = sr args r in
    let* a = so args a in
    Result.map (c r a) (so args b)

  let brrr c args a b d =
    let* a = sr args a in
    let* b = sr args b in
    Result.map (c a b) (sr args d)

  let substitute_instruction args = function
    | Jmp_term o -> bo (fun o -> Jmp_term o) args o
    | Jalr_term (a, b) -> b2 (fun a b -> Jalr_term (a, b)) args a b
    | Jnz_term (r, o) -> bro (fun r o -> Jnz_term (r, o)) args r o
    | ReadSR_term (r, s) -> b1 (fun r -> ReadSR_term (r, s)) args r
    | WriteSR_term (s, r) -> b1 (fun r -> WriteSR_term (s, r)) args r
    | Move_term (r, o) -> bro (fun r o -> Move_term (r, o)) args r o
    | Load_term (a, b) -> b2 (fun a b -> Load_term (a, b)) args a b
    | Store_term (r, o) -> bro (fun r o -> Store_term (r, o)) args r o
    | Add_term (r, a, b) -> broo (fun r a b -> Add_term (r, a, b)) args r a b
    | Sub_term (r, a, b) -> broo (fun r a b -> Sub_term (r, a, b)) args r a b
    | Mul_term (r, a, b) -> broo (fun r a b -> Mul_term (r, a, b)) args r a b
    | LAnd_term (r, a, b) -> broo (fun r a b -> LAnd_term (r, a, b)) args r a b
    | LOr_term (r, a, b) -> broo (fun r a b -> LOr_term (r, a, b)) args r a b
    | LShiftL_term (r, a, b) -> broo (fun r a b -> LShiftL_term (r, a, b)) args r a b
    | LShiftR_term (r, a, b) -> broo (fun r a b -> LShiftR_term (r, a, b)) args r a b
    | Lt_term (r, a, b) -> broo (fun r a b -> Lt_term (r, a, b)) args r a b
    | Lea_term (r, o) -> bro (fun r o -> Lea_term (r, o)) args r o
    | Restrict_term (r, o) -> bro (fun r o -> Restrict_term (r, o)) args r o
    | SubSeg_term (r, a, b) -> broo (fun r a b -> SubSeg_term (r, a, b)) args r a b
    | GetL_term (a, b) -> b2 (fun a b -> GetL_term (a, b)) args a b
    | GetB_term (a, b) -> b2 (fun a b -> GetB_term (a, b)) args a b
    | GetE_term (a, b) -> b2 (fun a b -> GetE_term (a, b)) args a b
    | GetA_term (a, b) -> b2 (fun a b -> GetA_term (a, b)) args a b
    | GetP_term (a, b) -> b2 (fun a b -> GetP_term (a, b)) args a b
    | GetOType_term (a, b) -> b2 (fun a b -> GetOType_term (a, b)) args a b
    | GetWType_term (a, b) -> b2 (fun a b -> GetWType_term (a, b)) args a b
    | Seal_term (a, b, c) -> brrr (fun a b c -> Seal_term (a, b, c)) args a b c
    | UnSeal_term (a, b, c) -> brrr (fun a b c -> UnSeal_term (a, b, c)) args a b c
    | Fail_term -> Ok Fail_term
    | Halt_term -> Ok Halt_term

  let ss args = function
    | Cap_term (p, l, b, e, a) ->
        let* p = sp args p in
        Result.map (fun l -> Cap_term (p, l, b, e, a)) (sl args l)
    | Seal_range_term (p, l, b, e, a) ->
        let* p = ssp args p in
        Result.map (fun l -> Seal_range_term (p, l, b, e, a)) (sl args l)

  let sw args = function
    | I_term _ as w -> Ok w
    | Sealable_term s -> Result.map (fun s -> Sealable_term s) (ss args s)
    | Sentry_term (p, l, b, e, a) ->
        let* p = sp args p in
        Result.map (fun l -> Sentry_term (p, l, b, e, a)) (sl args l)
    | Sealed_term (o, s) -> Result.map (fun s -> Sealed_term (o, s)) (ss args s)

  let substitute_statement ~arguments = function
    | Op op -> Result.map (fun op -> Op op) (substitute_instruction arguments op)
    | Word w -> Result.map (fun w -> Word w) (sw arguments w)

  let substitute_raw_word ~arguments w = sw arguments w

  let substitute_argument ~arguments = function
    | Register_argument _ as a -> Ok a
    | Constant_argument (Value_parameter n) -> (
        match lookup arguments n with
        | Some a -> Ok a
        | None -> error [] (Printf.sprintf "No argument for $%s." n))
    | a -> Ok a
end

module Frontend = Assembly_frontend.Make (Syntax)
include Frontend
