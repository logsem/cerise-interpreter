module Token = struct
  type kind =
    | Identifier of string
    | Integer of Z.t
    | Parameter of string
    | Directive of string
    | Punctuation of char
    | Assign
    | Current_address

  type t = { kind : kind; text : string; location : Diagnostic.source_location }

  let kind token = token.kind
  let text token = token.text
  let location token = token.location
end

type 'a parsed = ('a * Token.t list, Diagnostic.t list) result

let is_letter character = match character with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
let is_digit character = match character with '0' .. '9' -> true | _ -> false

let is_hex character =
  is_digit character || match character with 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

let is_name_character character = is_letter character || is_digit character

let tokenize ?filename source =
  let length = String.length source in
  let location line column offset =
    { Diagnostic.source = filename; line; column; offset = Some offset }
  in
  let diagnostic line column offset message =
    Diagnostic.error ~location:(location line column offset) message
  in
  let rec skip_comment index line column =
    if index >= length then (index, line, column)
    else if Char.equal source.[index] '\n' then (index, line, column)
    else skip_comment (index + 1) line (column + 1)
  in
  let rec name_end index =
    if index < length && is_name_character source.[index] then name_end (index + 1) else index
  in
  let rec decimal_end index =
    if index < length && is_digit source.[index] then decimal_end (index + 1) else index
  in
  let rec hexadecimal_end index =
    if index < length && is_hex source.[index] then hexadecimal_end (index + 1) else index
  in
  let token kind text line column offset =
    { Token.kind; text; location = location line column offset }
  in
  let rec scan index line column tokens =
    if index >= length then Ok (List.rev tokens)
    else
      match source.[index] with
      | (' ' | '\t' | '\r') as whitespace ->
          let width = if Char.equal whitespace '\t' then 1 else 1 in
          scan (index + 1) line (column + width) tokens
      | '\n' -> scan (index + 1) (line + 1) 1 tokens
      | ';' ->
          let index, line, column = skip_comment (index + 1) line (column + 1) in
          scan index line column tokens
      | ':' when index + 1 < length && Char.equal source.[index + 1] '=' ->
          scan (index + 2) line (column + 2) (token Assign ":=" line column index :: tokens)
      | '&'
        when String.length source - index >= 13
             && String.equal (String.sub source index 13) "&CURRENT_ADDR" ->
          scan (index + 13) line (column + 13)
            (token Current_address "&CURRENT_ADDR" line column index :: tokens)
      | ('%' | '$') as prefix ->
          let ending = name_end (index + 1) in
          if ending = index + 1 then
            Error
              [ diagnostic line column index (Printf.sprintf "Expected a name after %C." prefix) ]
          else
            let name = String.sub source (index + 1) (ending - index - 1) in
            let text = String.sub source index (ending - index) in
            let kind = if Char.equal prefix '%' then Token.Directive name else Parameter name in
            scan ending line (column + ending - index) (token kind text line column index :: tokens)
      | '0'
        when index + 2 <= length
             && index + 1 < length
             && (Char.equal source.[index + 1] 'x' || Char.equal source.[index + 1] 'X') ->
          let ending = hexadecimal_end (index + 2) in
          if ending = index + 2 then
            Error [ diagnostic line column index "A hexadecimal literal needs at least one digit." ]
          else
            let text = String.sub source index (ending - index) in
            let value = Z.of_string text in
            scan ending line
              (column + ending - index)
              (token (Integer value) text line column index :: tokens)
      | character when is_digit character ->
          let ending = decimal_end index in
          let text = String.sub source index (ending - index) in
          let value = Z.of_string text in
          scan ending line
            (column + ending - index)
            (token (Integer value) text line column index :: tokens)
      | character when is_letter character ->
          let ending = name_end index in
          let text = String.sub source index (ending - index) in
          scan ending line
            (column + ending - index)
            (token (Identifier text) text line column index :: tokens)
      | '&' when index + 1 < length && Char.equal source.[index + 1] '&' ->
          scan (index + 2) line (column + 2)
            (token (Identifier "&&") "&&" line column index :: tokens)
      | '|' when index + 1 < length && Char.equal source.[index + 1] '|' ->
          scan (index + 2) line (column + 2)
            (token (Identifier "||") "||" line column index :: tokens)
      | '<' when index + 1 < length && Char.equal source.[index + 1] '<' ->
          scan (index + 2) line (column + 2)
            (token (Identifier "<<") "<<" line column index :: tokens)
      | '>' when index + 1 < length && Char.equal source.[index + 1] '>' ->
          scan (index + 2) line (column + 2)
            (token (Identifier ">>") ">>" line column index :: tokens)
      | ('(' | ')' | '[' | ']' | '{' | '}' | ',' | ':' | '+' | '-' | '*' | '#') as punctuation ->
          scan (index + 1) line (column + 1)
            (token (Punctuation punctuation) (String.make 1 punctuation) line column index :: tokens)
      | character ->
          Error
            [
              diagnostic line column index
                (Printf.sprintf "Unexpected character %C in assembly input." character);
            ]
  in
  scan 0 1 1 []

module Expression = struct
  type t =
    | Integer of Z.t
    | Current_address
    | Max_address
    | Stack_address
    | Symbol of string
    | Parameter of string
    | Add of t * t
    | Subtract of t * t
    | Multiply of t * t
    | Logand of t * t
    | Logor of t * t
    | Shift_left of t * t
    | Shift_right of t * t

  let shift_count value =
    if Z.sign value < 0 then Error "shift count must be non-negative"
    else if not (Z.fits_int value) then Error "shift count does not fit in a machine integer"
    else Ok (Z.to_int value)

  let rec map_symbols mapper = function
    | Symbol name -> mapper name
    | Add (left, right) -> Add (map_symbols mapper left, map_symbols mapper right)
    | Subtract (left, right) -> Subtract (map_symbols mapper left, map_symbols mapper right)
    | Multiply (left, right) -> Multiply (map_symbols mapper left, map_symbols mapper right)
    | Logand (left, right) -> Logand (map_symbols mapper left, map_symbols mapper right)
    | Logor (left, right) -> Logor (map_symbols mapper left, map_symbols mapper right)
    | Shift_left (left, right) -> Shift_left (map_symbols mapper left, map_symbols mapper right)
    | Shift_right (left, right) -> Shift_right (map_symbols mapper left, map_symbols mapper right)
    | (Integer _ | Current_address | Max_address | Stack_address | Parameter _) as expression ->
        expression

  let rec map_parameters mapper = function
    | Parameter name as expression -> Option.value (mapper name) ~default:expression
    | Add (left, right) -> Add (map_parameters mapper left, map_parameters mapper right)
    | Subtract (left, right) -> Subtract (map_parameters mapper left, map_parameters mapper right)
    | Multiply (left, right) -> Multiply (map_parameters mapper left, map_parameters mapper right)
    | Logand (left, right) -> Logand (map_parameters mapper left, map_parameters mapper right)
    | Logor (left, right) -> Logor (map_parameters mapper left, map_parameters mapper right)
    | Shift_left (left, right) ->
        Shift_left (map_parameters mapper left, map_parameters mapper right)
    | Shift_right (left, right) ->
        Shift_right (map_parameters mapper left, map_parameters mapper right)
    | (Integer _ | Current_address | Max_address | Stack_address | Symbol _) as expression ->
        expression

  let rec simplify = function
    | Add (left, right) -> (
        match (simplify left, simplify right) with
        | Integer left, Integer right -> Integer Z.(left + right)
        | left, right -> Add (left, right))
    | Subtract (left, right) -> (
        match (simplify left, simplify right) with
        | Integer left, Integer right -> Integer Z.(left - right)
        | left, right -> Subtract (left, right))
    | Multiply (left, right) -> (
        match (simplify left, simplify right) with
        | Integer left, Integer right -> Integer Z.(left * right)
        | left, right -> Multiply (left, right))
    | Logand (left, right) -> (
        match (simplify left, simplify right) with
        | Integer left, Integer right -> Integer (Z.logand left right)
        | left, right -> Logand (left, right))
    | Logor (left, right) -> (
        match (simplify left, simplify right) with
        | Integer left, Integer right -> Integer (Z.logor left right)
        | left, right -> Logor (left, right))
    | Shift_left (left, right) -> (
        match (simplify left, simplify right) with
        | Integer left, Integer right when Z.sign right >= 0 && Z.fits_int right ->
            Integer (Z.shift_left left (Z.to_int right))
        | left, right -> Shift_left (left, right))
    | Shift_right (left, right) -> (
        match (simplify left, simplify right) with
        | Integer left, Integer right when Z.sign right >= 0 && Z.fits_int right ->
            Integer (Z.shift_right left (Z.to_int right))
        | left, right -> Shift_right (left, right))
    | expression -> expression

  let rec evaluate_runtime config = function
    | Integer value -> Ok value
    | Max_address -> Ok (Runtime_config.max_addr config)
    | Stack_address -> Ok (Runtime_config.stack_addr config)
    | Add (left, right) -> (
        match (evaluate_runtime config left, evaluate_runtime config right) with
        | Ok left, Ok right -> Ok Z.(left + right)
        | Error message, _ | _, Error message -> Error message)
    | Subtract (left, right) -> (
        match (evaluate_runtime config left, evaluate_runtime config right) with
        | Ok left, Ok right -> Ok Z.(left - right)
        | Error message, _ | _, Error message -> Error message)
    | Multiply (left, right) -> (
        match (evaluate_runtime config left, evaluate_runtime config right) with
        | Ok left, Ok right -> Ok Z.(left * right)
        | Error message, _ | _, Error message -> Error message)
    | Logand (left, right) -> (
        match (evaluate_runtime config left, evaluate_runtime config right) with
        | Ok left, Ok right -> Ok (Z.logand left right)
        | Error message, _ | _, Error message -> Error message)
    | Logor (left, right) -> (
        match (evaluate_runtime config left, evaluate_runtime config right) with
        | Ok left, Ok right -> Ok (Z.logor left right)
        | Error message, _ | _, Error message -> Error message)
    | Shift_left (left, right) -> (
        match (evaluate_runtime config left, evaluate_runtime config right) with
        | Ok left, Ok right -> Result.map (Z.shift_left left) (shift_count right)
        | Error message, _ | _, Error message -> Error message)
    | Shift_right (left, right) -> (
        match (evaluate_runtime config left, evaluate_runtime config right) with
        | Ok left, Ok right -> Result.map (Z.shift_right left) (shift_count right)
        | Error message, _ | _, Error message -> Error message)
    | Current_address -> Error "an unresolved current-address expression remains"
    | Symbol name -> Error (Printf.sprintf "an unresolved symbol %S remains" name)
    | Parameter name -> Error (Printf.sprintf "an unexpanded macro parameter $%s remains" name)
end

let error_at token message = Diagnostic.error ~location:(Token.location token) message

let parse_expression tokens =
  let rec primary = function
    | [] -> Error [ Diagnostic.error "Expected an arithmetic expression." ]
    | token :: rest -> (
        match Token.kind token with
        | Integer value -> Ok (Expression.Integer value, rest)
        | Current_address -> Ok (Current_address, rest)
        | Parameter name -> Ok (Parameter name, rest)
        | Identifier "MAX_ADDR" -> Ok (Max_address, rest)
        | Identifier "STK_ADDR" -> Ok (Stack_address, rest)
        | Identifier name -> Ok (Symbol name, rest)
        | Punctuation '-' ->
            Result.map
              (fun (expression, rest) -> (Expression.Subtract (Integer Z.zero, expression), rest))
              (primary rest)
        | Punctuation '(' -> (
            match addition rest with
            | Error _ as error -> error
            | Ok (expression, closing :: rest) when Token.kind closing = Punctuation ')' ->
                Ok (expression, rest)
            | Ok (_, token :: _) -> Error [ error_at token "Expected `)` after the expression." ]
            | Ok (_, []) -> Error [ error_at token "Expected `)` after the expression." ])
        | _ -> Error [ error_at token "Expected an arithmetic expression." ])
  and addition tokens =
    match primary tokens with Error _ as error -> error | Ok (left, rest) -> continue left rest
  and continue left = function
    | operator :: rest when Token.kind operator = Punctuation '+' -> (
        match primary rest with
        | Error _ as error -> error
        | Ok (right, rest) -> continue (Expression.Add (left, right)) rest)
    | operator :: rest when Token.kind operator = Punctuation '-' -> (
        match primary rest with
        | Error _ as error -> error
        | Ok (right, rest) -> continue (Expression.Subtract (left, right)) rest)
    | operator :: rest when Token.kind operator = Punctuation '*' -> (
        match primary rest with
        | Error _ as error -> error
        | Ok (right, rest) -> continue (Expression.Multiply (left, right)) rest)
    | operator :: rest when Token.kind operator = Identifier "&&" -> (
        match primary rest with
        | Error _ as error -> error
        | Ok (right, rest) -> continue (Expression.Logand (left, right)) rest)
    | operator :: rest when Token.kind operator = Identifier "||" -> (
        match primary rest with
        | Error _ as error -> error
        | Ok (right, rest) -> continue (Expression.Logor (left, right)) rest)
    | operator :: rest when Token.kind operator = Identifier "<<" -> (
        match primary rest with
        | Error _ as error -> error
        | Ok (right, rest) -> continue (Expression.Shift_left (left, right)) rest)
    | operator :: rest when Token.kind operator = Identifier ">>" -> (
        match primary rest with
        | Error _ as error -> error
        | Ok (right, rest) -> continue (Expression.Shift_right (left, right)) rest)
    | rest -> Ok (left, rest)
  in
  addition tokens

module type SYNTAX = sig
  type statement
  type raw_word
  type regfile
  type macro_argument
  type parameter_kind

  val parse_statement : Token.t list -> statement parsed
  val parse_raw_word : Token.t list -> raw_word parsed
  val parse_regfile : Token.t list -> regfile parsed
  val parse_macro_argument : Token.t list -> macro_argument parsed
  val statement_of_raw_word : raw_word -> statement
  val parameter_kind : string -> parameter_kind option
  val parameter_kind_name : parameter_kind -> string
  val argument_kind : macro_argument -> parameter_kind
  val accepts_argument : parameter_kind -> macro_argument -> bool
  val expression_of_argument : macro_argument -> Expression.t option
  val map_statement_expressions : (Expression.t -> Expression.t) -> statement -> statement
  val map_raw_word_expressions : (Expression.t -> Expression.t) -> raw_word -> raw_word
  val map_regfile_expressions : (Expression.t -> Expression.t) -> regfile -> regfile
  val map_argument_expressions : (Expression.t -> Expression.t) -> macro_argument -> macro_argument

  val validate_statement :
    parameters:(string * parameter_kind) list -> statement -> Diagnostic.t list

  val validate_raw_word : parameters:(string * parameter_kind) list -> raw_word -> Diagnostic.t list

  val substitute_statement :
    arguments:(string * macro_argument) list -> statement -> (statement, Diagnostic.t list) result

  val substitute_raw_word :
    arguments:(string * macro_argument) list -> raw_word -> (raw_word, Diagnostic.t list) result

  val substitute_argument :
    arguments:(string * macro_argument) list ->
    macro_argument ->
    (macro_argument, Diagnostic.t list) result
end

module Make (Syntax : SYNTAX) = struct
  type program = Syntax.statement list
  type regfile = Syntax.regfile
  type word = Syntax.raw_word
  type parameter = { name : string; kind : Syntax.parameter_kind }

  type item_node =
    | Statement of Syntax.statement
    | Raw_word of Syntax.raw_word
    | Label of string
    | Definition of string * Expression.t
    | Macro_call of string * Syntax.macro_argument list
    | Macro_definition of macro_definition

  and item = { node : item_node; location : Diagnostic.source_location }

  and macro_definition = {
    name : string;
    parameters : parameter list;
    body : item list;
    declaration_location : Diagnostic.source_location;
  }

  let diagnostic_at location message = Diagnostic.error ~location message

  let expect_punctuation punctuation = function
    | token :: rest when Token.kind token = Token.Punctuation punctuation -> Ok rest
    | token :: _ ->
        Error
          [
            error_at token (Printf.sprintf "Expected `%c`, got %S." punctuation (Token.text token));
          ]
    | [] -> Error [ Diagnostic.error (Printf.sprintf "Expected `%c`." punctuation) ]

  let expect_identifier = function
    | token :: rest -> (
        match Token.kind token with
        | Identifier name -> Ok (name, token, rest)
        | _ -> Error [ error_at token "Expected a name." ])
    | [] -> Error [ Diagnostic.error "Expected a name." ]

  let rec parse_parameters parameters = function
    | closing :: rest when Token.kind closing = Punctuation ')' -> Ok (List.rev parameters, rest)
    | tokens -> (
        match expect_identifier tokens with
        | Error _ as error -> error
        | Ok (name, name_token, rest) -> (
            match expect_punctuation ':' rest with
            | Error _ as error -> error
            | Ok rest -> (
                match expect_identifier rest with
                | Error _ as error -> error
                | Ok (kind_name, kind_token, rest) -> (
                    match Syntax.parameter_kind kind_name with
                    | None ->
                        Error
                          [
                            error_at kind_token
                              (Printf.sprintf "Unsupported macro parameter kind %S." kind_name);
                          ]
                    | Some kind -> (
                        let parameter = { name; kind } in
                        match rest with
                        | comma :: rest when Token.kind comma = Punctuation ',' ->
                            parse_parameters (parameter :: parameters) rest
                        | closing :: rest when Token.kind closing = Punctuation ')' ->
                            Ok (List.rev (parameter :: parameters), rest)
                        | token :: _ ->
                            Error [ error_at token "Expected `,` or `)` in the parameter list." ]
                        | [] -> Error [ error_at name_token "Expected `)` after parameters." ])))))

  let rec parse_arguments arguments = function
    | closing :: rest when Token.kind closing = Punctuation ')' -> Ok (List.rev arguments, rest)
    | tokens -> (
        match Syntax.parse_macro_argument tokens with
        | Error _ as error -> error
        | Ok (argument, rest) -> (
            match rest with
            | comma :: rest when Token.kind comma = Punctuation ',' ->
                parse_arguments (argument :: arguments) rest
            | closing :: rest when Token.kind closing = Punctuation ')' ->
                Ok (List.rev (argument :: arguments), rest)
            | token :: _ -> Error [ error_at token "Expected `,` or `)` after macro argument." ]
            | [] -> Error [ Diagnostic.error "Expected `)` after macro arguments." ]))

  let rec parse_items ~inside_macro items = function
    | [] when inside_macro ->
        Error [ Diagnostic.error "A macro declaration is missing `%endmacro`." ]
    | [] -> Ok (List.rev items, [])
    | token :: rest when Token.kind token = Directive "endmacro" ->
        if inside_macro then Ok (List.rev items, rest)
        else Error [ error_at token "`%endmacro` does not have a matching `%macro`." ]
    | token :: rest when Token.kind token = Directive "macro" -> (
        if inside_macro then Error [ error_at token "Nested macro declarations are not supported." ]
        else
          match expect_identifier rest with
          | Error _ as error -> error
          | Ok (name, _, rest) -> (
              match expect_punctuation '(' rest with
              | Error _ as error -> error
              | Ok rest -> (
                  match parse_parameters [] rest with
                  | Error _ as error -> error
                  | Ok (parameters, rest) -> (
                      match parse_items ~inside_macro:true [] rest with
                      | Error _ as error -> error
                      | Ok (body, rest) ->
                          let definition =
                            { name; parameters; body; declaration_location = Token.location token }
                          in
                          parse_items ~inside_macro
                            ({ node = Macro_definition definition; location = Token.location token }
                            :: items)
                            rest))))
    | token :: rest when Token.kind token = Directive "define" -> (
        match expect_identifier rest with
        | Error _ as error -> error
        | Ok (name, _, rest) -> (
            match parse_expression rest with
            | Error _ as error -> error
            | Ok (expression, rest) ->
                parse_items ~inside_macro
                  ({ node = Definition (name, expression); location = Token.location token }
                  :: items)
                  rest))
    | token :: rest -> (
        match Token.kind token with
        | Directive name -> (
            match expect_punctuation '(' rest with
            | Error _ as error -> error
            | Ok rest -> (
                match parse_arguments [] rest with
                | Error _ as error -> error
                | Ok (arguments, rest) ->
                    parse_items ~inside_macro
                      ({ node = Macro_call (name, arguments); location = Token.location token }
                      :: items)
                      rest))
        | Punctuation '#' -> (
            match Syntax.parse_raw_word rest with
            | Error _ as error -> error
            | Ok (word, rest) ->
                parse_items ~inside_macro
                  ({ node = Raw_word word; location = Token.location token } :: items)
                  rest)
        | Identifier name -> (
            match rest with
            | colon :: rest when Token.kind colon = Punctuation ':' ->
                parse_items ~inside_macro
                  ({ node = Label name; location = Token.location token } :: items)
                  rest
            | _ -> (
                match Syntax.parse_statement (token :: rest) with
                | Error _ as error -> error
                | Ok (_, remaining) when List.length remaining = List.length (token :: rest) ->
                    Error [ error_at token "The backend statement parser consumed no input." ]
                | Ok (statement, rest) ->
                    parse_items ~inside_macro
                      ({ node = Statement statement; location = Token.location token } :: items)
                      rest))
        | _ -> (
            match Syntax.parse_statement (token :: rest) with
            | Error _ as error -> error
            | Ok (statement, rest) ->
                parse_items ~inside_macro
                  ({ node = Statement statement; location = Token.location token } :: items)
                  rest))

  let duplicate_parameters (definition : macro_definition) =
    let seen = Hashtbl.create (List.length definition.parameters) in
    List.filter_map
      (fun (parameter : parameter) ->
        if Hashtbl.mem seen parameter.name then
          Some
            (diagnostic_at definition.declaration_location
               (Printf.sprintf "Macro %S declares parameter $%s more than once." definition.name
                  parameter.name))
        else (
          Hashtbl.add seen parameter.name ();
          None))
      definition.parameters

  let validate_definition (definition : macro_definition) =
    let parameters =
      List.map
        (fun (parameter : parameter) -> (parameter.name, parameter.kind))
        definition.parameters
    in
    let body_errors =
      List.concat_map
        (fun item ->
          match item.node with
          | Statement statement -> Syntax.validate_statement ~parameters statement
          | Raw_word word -> Syntax.validate_raw_word ~parameters word
          | _ -> [])
        definition.body
    in
    duplicate_parameters definition @ body_errors

  let collect_macros items =
    let macros = Hashtbl.create 16 in
    let errors = ref [] in
    List.iter
      (fun item ->
        match item.node with
        | Macro_definition definition ->
            if Hashtbl.mem macros definition.name then
              errors :=
                diagnostic_at definition.declaration_location
                  (Printf.sprintf "Macro %S is declared more than once." definition.name)
                :: !errors
            else Hashtbl.add macros definition.name definition;
            errors := List.rev_append (validate_definition definition) !errors
        | _ -> ())
      items;
    if !errors = [] then Ok macros else Error (List.rev !errors)

  let local_labels (definition : macro_definition) =
    let seen = Hashtbl.create 8 in
    let labels = ref [] in
    let duplicates = ref [] in
    List.iter
      (fun item ->
        match item.node with
        | Label name ->
            if Hashtbl.mem seen name then
              duplicates :=
                diagnostic_at item.location
                  (Printf.sprintf "Macro %S has duplicate private label %S." definition.name name)
                :: !duplicates
            else (
              Hashtbl.add seen name ();
              labels := name :: !labels)
        | _ -> ())
      definition.body;
    if !duplicates = [] then Ok (List.rev !labels) else Error (List.rev !duplicates)

  let bind_arguments (definition : macro_definition) arguments location =
    if List.length definition.parameters <> List.length arguments then
      Error
        [
          diagnostic_at location
            (Printf.sprintf "Macro %S expects %d arguments but received %d." definition.name
               (List.length definition.parameters)
               (List.length arguments));
        ]
    else
      let bindings = List.combine definition.parameters arguments in
      let errors =
        List.filter_map
          (fun (parameter, argument) ->
            if Syntax.accepts_argument parameter.kind argument then None
            else
              Some
                (diagnostic_at location
                   (Printf.sprintf "Macro %S parameter $%s expects %s, not %s." definition.name
                      parameter.name
                      (Syntax.parameter_kind_name parameter.kind)
                      (Syntax.parameter_kind_name (Syntax.argument_kind argument)))))
          bindings
      in
      if errors = [] then
        Ok
          (List.map
             (fun ((parameter : parameter), argument) -> (parameter.name, argument))
             bindings)
      else Error errors

  let expression_mapper bindings labels expression =
    expression
    |> Expression.map_parameters (fun name ->
        Option.bind (List.assoc_opt name bindings) Syntax.expression_of_argument)
    |> Expression.map_symbols (fun name ->
        match Hashtbl.find_opt labels name with
        | Some renamed -> Expression.Symbol renamed
        | None -> Expression.Symbol name)

  let expand macros items =
    let invocation = ref 0 in
    let reserved_names = Hashtbl.create 32 in
    let reserve name = Hashtbl.replace reserved_names name () in
    List.iter
      (fun item ->
        match item.node with
        | Label name | Definition (name, _) -> reserve name
        | Macro_definition definition ->
            List.iter
              (fun body_item ->
                match body_item.node with Definition (name, _) -> reserve name | _ -> ())
              definition.body
        | Statement _ | Raw_word _ | Macro_call _ -> ())
      items;
    let fresh_private_name base =
      let rec choose suffix =
        let candidate = if suffix = 0 then base else Printf.sprintf "%s_%d" base suffix in
        if Hashtbl.mem reserved_names candidate then choose (suffix + 1)
        else (
          reserve candidate;
          candidate)
      in
      choose 0
    in
    let rec expand_items stack items =
      let rec loop expanded = function
        | [] -> Ok (List.rev expanded)
        | item :: rest -> (
            match item.node with
            | Macro_definition _ -> loop expanded rest
            | Macro_call (name, arguments) -> (
                match Hashtbl.find_opt macros name with
                | None ->
                    Error [ diagnostic_at item.location (Printf.sprintf "Unknown macro %S." name) ]
                | Some definition -> (
                    if List.mem name stack then
                      Error
                        [
                          diagnostic_at item.location
                            (Printf.sprintf "Recursive macro call involving %S." name);
                        ]
                    else
                      match bind_arguments definition arguments item.location with
                      | Error _ as error -> error
                      | Ok bindings -> (
                          match local_labels definition with
                          | Error _ as error -> error
                          | Ok private_labels -> (
                              let labels = Hashtbl.create (List.length private_labels) in
                              List.iter
                                (fun label ->
                                  let base =
                                    Printf.sprintf "__macro_%d_%s_%s" !invocation name label
                                  in
                                  let fresh = fresh_private_name base in
                                  Hashtbl.add labels label fresh)
                                private_labels;
                              incr invocation;
                              let mapper = expression_mapper bindings labels in
                              let transform body_item =
                                match body_item.node with
                                | Statement statement ->
                                    let statement =
                                      Syntax.map_statement_expressions mapper statement
                                    in
                                    Result.map
                                      (fun statement ->
                                        { body_item with node = Statement statement })
                                      (Syntax.substitute_statement ~arguments:bindings statement)
                                | Raw_word word ->
                                    let word = Syntax.map_raw_word_expressions mapper word in
                                    Result.map
                                      (fun word -> { body_item with node = Raw_word word })
                                      (Syntax.substitute_raw_word ~arguments:bindings word)
                                | Label label ->
                                    let label =
                                      Option.value (Hashtbl.find_opt labels label) ~default:label
                                    in
                                    Ok { body_item with node = Label label }
                                | Definition (definition_name, expression) ->
                                    Ok
                                      {
                                        body_item with
                                        node = Definition (definition_name, mapper expression);
                                      }
                                | Macro_call (called, call_arguments) ->
                                    let rec transform_arguments transformed = function
                                      | [] -> Ok (List.rev transformed)
                                      | argument :: rest -> (
                                          let argument =
                                            Syntax.map_argument_expressions mapper argument
                                          in
                                          match
                                            Syntax.substitute_argument ~arguments:bindings argument
                                          with
                                          | Error _ as error -> error
                                          | Ok argument ->
                                              transform_arguments (argument :: transformed) rest)
                                    in
                                    Result.map
                                      (fun arguments ->
                                        { body_item with node = Macro_call (called, arguments) })
                                      (transform_arguments [] call_arguments)
                                | Macro_definition _ ->
                                    Error
                                      [
                                        diagnostic_at body_item.location
                                          "Nested macro declarations are not supported.";
                                      ]
                              in
                              let rec transform_body transformed = function
                                | [] -> expand_items (name :: stack) (List.rev transformed)
                                | body_item :: rest -> (
                                    match transform body_item with
                                    | Error _ as error -> error
                                    | Ok body_item -> transform_body (body_item :: transformed) rest
                                    )
                              in
                              match transform_body [] definition.body with
                              | Error _ as error -> error
                              | Ok body -> loop (List.rev_append body expanded) rest))))
            | _ -> loop (item :: expanded) rest)
      in
      loop [] items
    in
    expand_items [] items

  let resolve items =
    let definitions = Hashtbl.create 16 in
    let labels = Hashtbl.create 32 in
    let errors = ref [] in
    let address = ref 0 in
    List.iter
      (fun item ->
        match item.node with
        | Definition (name, expression) ->
            if Hashtbl.mem definitions name then
              errors :=
                diagnostic_at item.location
                  (Printf.sprintf "Integer definition %S is declared more than once." name)
                :: !errors
            else Hashtbl.add definitions name (expression, item.location)
        | Label name ->
            if Hashtbl.mem labels name then
              errors :=
                diagnostic_at item.location
                  (Printf.sprintf "Label %S is declared more than once." name)
                :: !errors
            else Hashtbl.add labels name (Z.of_int !address, item.location)
        | Statement _ | Raw_word _ -> incr address
        | Macro_call _ | Macro_definition _ -> assert false)
      items;
    Hashtbl.iter
      (fun name (_, location) ->
        if Hashtbl.mem labels name then
          errors :=
            diagnostic_at location
              (Printf.sprintf "Integer definition %S conflicts with a label of the same name." name)
            :: !errors)
      definitions;
    let rec resolve_expression visiting current location = function
      | Expression.Integer _ as expression -> expression
      | Current_address -> Integer (Z.of_int current)
      | Max_address -> Max_address
      | Stack_address -> Stack_address
      | Parameter name ->
          errors :=
            diagnostic_at location
              (Printf.sprintf "Unexpanded macro parameter $%s reached label resolution." name)
            :: !errors;
          Parameter name
      | Symbol name -> (
          match Hashtbl.find_opt definitions name with
          | Some (expression, definition_location) ->
              if List.mem name visiting then (
                errors :=
                  diagnostic_at definition_location
                    (Printf.sprintf "Cyclic integer definition involving %S." name)
                  :: !errors;
                Symbol name)
              else resolve_expression (name :: visiting) current definition_location expression
          | None -> (
              match Hashtbl.find_opt labels name with
              | Some (address, _) -> Integer address
              | None ->
                  errors :=
                    diagnostic_at location
                      (Printf.sprintf "Unknown label or integer definition %S." name)
                    :: !errors;
                  Symbol name))
      | Add (left, right) ->
          Expression.simplify
            (Add
               ( resolve_expression visiting current location left,
                 resolve_expression visiting current location right ))
      | Subtract (left, right) ->
          Expression.simplify
            (Subtract
               ( resolve_expression visiting current location left,
                 resolve_expression visiting current location right ))
      | Multiply (left, right) ->
          Expression.simplify
            (Multiply
               ( resolve_expression visiting current location left,
                 resolve_expression visiting current location right ))
      | Logand (left, right) ->
          Expression.simplify
            (Logand
               ( resolve_expression visiting current location left,
                 resolve_expression visiting current location right ))
      | Logor (left, right) ->
          Expression.simplify
            (Logor
               ( resolve_expression visiting current location left,
                 resolve_expression visiting current location right ))
      | Shift_left (left, right) ->
          Expression.simplify
            (Shift_left
               ( resolve_expression visiting current location left,
                 resolve_expression visiting current location right ))
      | Shift_right (left, right) ->
          Expression.simplify
            (Shift_right
               ( resolve_expression visiting current location left,
                 resolve_expression visiting current location right ))
    in
    let current = ref 0 in
    let program =
      List.filter_map
        (fun item ->
          let mapper = resolve_expression [] !current item.location in
          match item.node with
          | Statement statement ->
              incr current;
              Some (Syntax.map_statement_expressions mapper statement)
          | Raw_word word ->
              incr current;
              Some (Syntax.statement_of_raw_word (Syntax.map_raw_word_expressions mapper word))
          | Definition _ | Label _ -> None
          | Macro_call _ | Macro_definition _ -> assert false)
        items
    in
    if !errors = [] then Ok program else Error (List.rev !errors)

  let parse_program ?filename source =
    match tokenize ?filename source with
    | Error _ as error -> error
    | Ok tokens -> (
        match parse_items ~inside_macro:false [] tokens with
        | Error _ as error -> error
        | Ok (items, []) -> (
            match collect_macros items with
            | Error _ as error -> error
            | Ok macros -> (
                match expand macros items with
                | Error _ as error -> error
                | Ok items -> resolve items))
        | Ok (_, token :: _) -> Error [ error_at token "Unexpected trailing input." ])

  let parse_complete ?filename source parser mapper =
    match tokenize ?filename source with
    | Error _ as error -> error
    | Ok tokens -> (
        match parser tokens with
        | Error _ as error -> error
        | Ok (value, []) -> Ok (mapper value)
        | Ok (_, token :: _) -> Error [ error_at token "Unexpected trailing input." ])

  let parse_regfile ?filename source = parse_complete ?filename source Syntax.parse_regfile Fun.id
  let parse_word ?filename source = parse_complete ?filename source Syntax.parse_raw_word Fun.id
end
