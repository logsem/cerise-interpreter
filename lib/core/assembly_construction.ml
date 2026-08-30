module Expression = struct
  type t = Integer of Z.t | Current_address | Max_address | Stack_address | Symbol of string | Parameter of string
    | Add of t * t | Subtract of t * t | Multiply of t * t | Logand of t * t | Logor of t * t
    | Shift_left of t * t | Shift_right of t * t
  let shift_count (value : Z.t) : (int, string) result =
    if Z.sign value < 0 then Error "shift count must be non-negative"
    else if not (Z.fits_int value) then Error "shift count does not fit in a machine integer"
    else Ok (Z.to_int value)
  let rec map_symbols (mapper : (string -> t)) (matched_value : t) : t = match matched_value with
    | Symbol name -> mapper name
    | Add (a,b) -> Add (map_symbols mapper a,map_symbols mapper b) | Subtract (a,b) -> Subtract (map_symbols mapper a,map_symbols mapper b)
    | Multiply (a,b) -> Multiply (map_symbols mapper a,map_symbols mapper b) | Logand (a,b) -> Logand (map_symbols mapper a,map_symbols mapper b)
    | Logor (a,b) -> Logor (map_symbols mapper a,map_symbols mapper b) | Shift_left (a,b) -> Shift_left (map_symbols mapper a,map_symbols mapper b)
    | Shift_right (a,b) -> Shift_right (map_symbols mapper a,map_symbols mapper b)
    | (Integer _ | Current_address | Max_address | Stack_address | Parameter _) as e -> e
  let rec map_parameters (mapper : (string -> t option)) (matched_value : t) : t = match matched_value with
    | Parameter name as e -> Option.value (mapper name) ~default:e
    | Add (a,b) -> Add (map_parameters mapper a,map_parameters mapper b) | Subtract (a,b) -> Subtract (map_parameters mapper a,map_parameters mapper b)
    | Multiply (a,b) -> Multiply (map_parameters mapper a,map_parameters mapper b) | Logand (a,b) -> Logand (map_parameters mapper a,map_parameters mapper b)
    | Logor (a,b) -> Logor (map_parameters mapper a,map_parameters mapper b) | Shift_left (a,b) -> Shift_left (map_parameters mapper a,map_parameters mapper b)
    | Shift_right (a,b) -> Shift_right (map_parameters mapper a,map_parameters mapper b)
    | (Integer _ | Current_address | Max_address | Stack_address | Symbol _) as e -> e
  let rec simplify (matched_value : t) : t = match matched_value with
    | Add (a,b) -> (match simplify a,simplify b with Integer x,Integer y -> Integer Z.(x+y) | a,b -> Add(a,b))
    | Subtract (a,b) -> (match simplify a,simplify b with Integer x,Integer y -> Integer Z.(x-y) | a,b -> Subtract(a,b))
    | Multiply (a,b) -> (match simplify a,simplify b with Integer x,Integer y -> Integer Z.(x*y) | a,b -> Multiply(a,b))
    | Logand (a,b) -> (match simplify a,simplify b with Integer x,Integer y -> Integer (Z.logand x y) | a,b -> Logand(a,b))
    | Logor (a,b) -> (match simplify a,simplify b with Integer x,Integer y -> Integer (Z.logor x y) | a,b -> Logor(a,b))
    | Shift_left (a,b) -> (match simplify a,simplify b with Integer x,Integer y when Z.sign y >= 0 && Z.fits_int y -> Integer (Z.shift_left x (Z.to_int y)) | a,b -> Shift_left(a,b))
    | Shift_right (a,b) -> (match simplify a,simplify b with Integer x,Integer y when Z.sign y >= 0 && Z.fits_int y -> Integer (Z.shift_right x (Z.to_int y)) | a,b -> Shift_right(a,b))
    | e -> e
  let rec evaluate_runtime (config : Runtime_config.t) (matched_value : t) : (Z.t, string) result = match matched_value with
    | Integer v -> Ok v | Max_address -> Ok (Runtime_config.max_addr config) | Stack_address -> Ok (Runtime_config.stack_addr config)
    | Add(a,b) -> Result.bind (evaluate_runtime config a) (fun x -> Result.map (Z.add x) (evaluate_runtime config b))
    | Subtract(a,b) -> Result.bind (evaluate_runtime config a) (fun x -> Result.map (Z.sub x) (evaluate_runtime config b))
    | Multiply(a,b) -> Result.bind (evaluate_runtime config a) (fun x -> Result.map (Z.mul x) (evaluate_runtime config b))
    | Logand(a,b) -> Result.bind (evaluate_runtime config a) (fun x -> Result.map (Z.logand x) (evaluate_runtime config b))
    | Logor(a,b) -> Result.bind (evaluate_runtime config a) (fun x -> Result.map (Z.logor x) (evaluate_runtime config b))
    | Shift_left(a,b) -> Result.bind (evaluate_runtime config a) (fun x -> Result.bind (evaluate_runtime config b) (fun y -> Result.map (Z.shift_left x) (shift_count y)))
    | Shift_right(a,b) -> Result.bind (evaluate_runtime config a) (fun x -> Result.bind (evaluate_runtime config b) (fun y -> Result.map (Z.shift_right x) (shift_count y)))
    | Current_address -> Error "an unresolved current-address expression remains" | Symbol n -> Error (Printf.sprintf "an unresolved symbol %S remains" n)
    | Parameter n -> Error (Printf.sprintf "an unexpanded macro parameter $%s remains" n)
end

exception Parse_error of Diagnostic.source_location * string

let location (position : Lexing.position) : Diagnostic.source_location =
  {
    Diagnostic.source =
      (if String.equal position.Lexing.pos_fname "" then None else Some position.pos_fname);
    line = position.pos_lnum;
    column = position.pos_cnum - position.pos_bol + 1;
    offset = Some position.pos_cnum;
  }

type 'kind parameter = { name : string; kind : 'kind }

type ('statement, 'word, 'argument, 'kind) item_node =
  | Statement of 'statement
  | Raw_word of 'word
  | Label of string
  | Definition of string * Expression.t
  | Macro_call of string * 'argument list
  | Macro_definition of ('statement, 'word, 'argument, 'kind) macro_definition

and ('statement, 'word, 'argument, 'kind) item = {
  node : ('statement, 'word, 'argument, 'kind) item_node;
  location : Diagnostic.source_location;
}

and ('statement, 'word, 'argument, 'kind) macro_definition = {
  name : string;
  parameters : 'kind parameter list;
  body : ('statement, 'word, 'argument, 'kind) item list;
  declaration_location : Diagnostic.source_location;
}

module type SYNTAX = sig
  type statement
  type raw_word
  type macro_argument
  type parameter_kind

  val statement_of_raw_word : raw_word -> statement
  val parameter_kind_name : parameter_kind -> string
  val argument_kind : macro_argument -> parameter_kind
  val accepts_argument : parameter_kind -> macro_argument -> bool
  val expression_of_argument : macro_argument -> Expression.t option
  val map_statement_expressions : (Expression.t -> Expression.t) -> statement -> statement
  val map_raw_word_expressions : (Expression.t -> Expression.t) -> raw_word -> raw_word
  val map_argument_expressions : (Expression.t -> Expression.t) -> macro_argument -> macro_argument

  val validate_statement :
    parameters:(string * parameter_kind) list -> statement -> Diagnostic.t list

  val validate_raw_word :
    parameters:(string * parameter_kind) list -> raw_word -> Diagnostic.t list

  val substitute_statement :
    arguments:(string * macro_argument) list ->
    statement ->
    (statement, Diagnostic.t list) result

  val substitute_raw_word :
    arguments:(string * macro_argument) list ->
    raw_word ->
    (raw_word, Diagnostic.t list) result

  val substitute_argument :
    arguments:(string * macro_argument) list ->
    macro_argument ->
    (macro_argument, Diagnostic.t list) result
end

module Make (Syntax : SYNTAX) = struct
  type source_program =
    (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, Syntax.parameter_kind) item list

  let diagnostic_at (location : Diagnostic.source_location) (message : string) : Diagnostic.t = Diagnostic.error ~location message

  let locate_if_missing (location : Diagnostic.source_location) (diagnostic : Diagnostic.t) : Diagnostic.t =
    match Diagnostic.location diagnostic with
    | Some _ -> diagnostic
    | None ->
        Diagnostic.make ~severity:(Diagnostic.severity diagnostic) ~location
          (Diagnostic.message diagnostic)

  let duplicate_parameters
      (definition :
        (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, Syntax.parameter_kind)
        macro_definition) : Diagnostic.t list =
    let seen = Hashtbl.create (List.length definition.parameters) in
    List.filter_map
      (fun (parameter : Syntax.parameter_kind parameter) ->
        if Hashtbl.mem seen parameter.name then
          Some
            (diagnostic_at definition.declaration_location
               (Printf.sprintf "Macro %S declares parameter $%s more than once." definition.name
                  parameter.name))
        else (
          Hashtbl.add seen parameter.name ();
          None))
      definition.parameters

  let validate_definition
      (definition :
        (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, Syntax.parameter_kind)
        macro_definition) : Diagnostic.t list =
    let parameters =
      List.map
        (fun (parameter : Syntax.parameter_kind parameter) ->
          (parameter.name, parameter.kind))
        definition.parameters
    in
    let body_errors =
      List.concat_map
        (fun item ->
          let diagnostics =
            match item.node with
            | Statement statement -> Syntax.validate_statement ~parameters statement
            | Raw_word word -> Syntax.validate_raw_word ~parameters word
            | Label _ | Definition _ | Macro_call _ | Macro_definition _ -> []
          in
          List.map (locate_if_missing item.location) diagnostics)
        definition.body
    in
    duplicate_parameters definition @ body_errors

  let collect_macros (items : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument,
 Syntax.parameter_kind)
item list) : ((string,
  (Syntax.statement, Syntax.raw_word, Syntax.macro_argument,
   Syntax.parameter_kind)
  macro_definition)
 Hashtbl.t, Diagnostic.t list)
result =
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
        | Statement _ | Raw_word _ | Label _ | Definition _ | Macro_call _ -> ())
      items;
    if !errors = [] then Ok macros else Error (List.rev !errors)

  let local_labels
      (definition :
        (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, Syntax.parameter_kind)
        macro_definition) : (string list, Diagnostic.t list) result =
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
        | Statement _ | Raw_word _ | Definition _ | Macro_call _ | Macro_definition _ -> ())
      definition.body;
    if !duplicates = [] then Ok (List.rev !labels) else Error (List.rev !duplicates)

  let bind_arguments
      (definition :
        (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, Syntax.parameter_kind)
        macro_definition)
      (arguments : Syntax.macro_argument list) (location : Diagnostic.source_location) : ((string * Syntax.macro_argument) list, Diagnostic.t list) result =
    if List.length definition.parameters <> List.length arguments then
      Error
        [
          diagnostic_at location
            (Printf.sprintf "Macro %S expects %d arguments but received %d." definition.name
               (List.length definition.parameters) (List.length arguments));
        ]
    else
      let bindings = List.combine definition.parameters arguments in
      let errors =
        List.filter_map
          (fun ((parameter : Syntax.parameter_kind parameter), argument) ->
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
             (fun ((parameter : Syntax.parameter_kind parameter), argument) ->
               (parameter.name, argument))
             bindings)
      else Error errors

  let expression_mapper (bindings : (string * Syntax.macro_argument) list) (labels : (string, string) Hashtbl.t) (expression : Expression.t) : Expression.t =
    expression
    |> Expression.map_parameters (fun name ->
           Option.bind (List.assoc_opt name bindings) Syntax.expression_of_argument)
    |> Expression.map_symbols (fun name ->
           match Hashtbl.find_opt labels name with
           | Some renamed -> Expression.Symbol renamed
           | None -> Expression.Symbol name)

  let expand (macros : (string,
 (Syntax.statement, Syntax.raw_word, Syntax.macro_argument,
  Syntax.parameter_kind)
 macro_definition)
Hashtbl.t) (items : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list) : ((Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list,
 Diagnostic.t list)
result =
    let invocation = ref 0 in
    let reserved_names = Hashtbl.create 32 in
    let reserve (name : string) : unit = Hashtbl.replace reserved_names name () in
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
    let fresh_private_name (base : string) : string =
      let rec choose (suffix : int) : string =
        let candidate = if suffix = 0 then base else Printf.sprintf "%s_%d" base suffix in
        if Hashtbl.mem reserved_names candidate then choose (suffix + 1)
        else (
          reserve candidate;
          candidate)
      in
      choose 0
    in
    let rec expand_items (stack : string list) (items : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list) : ((Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list,
 Diagnostic.t list)
result =
      let rec loop (expanded : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list) (matched_value : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list) : ((Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list,
 Diagnostic.t list)
result = match matched_value with
        | [] -> Ok (List.rev expanded)
        | item :: rest -> (
            match item.node with
            | Macro_definition _ -> loop expanded rest
            | Macro_call (name, arguments) -> (
                match Hashtbl.find_opt macros name with
                | None ->
                    Error [ diagnostic_at item.location (Printf.sprintf "Unknown macro %S." name) ]
                | Some definition ->
                    if List.mem name stack then
                      Error
                        [
                          diagnostic_at item.location
                            (Printf.sprintf "Recursive macro call involving %S." name);
                        ]
                    else (
                      match bind_arguments definition arguments item.location with
                      | Error _ as error -> error
                      | Ok bindings -> (
                          match local_labels definition with
                          | Error _ as error -> error
                          | Ok private_labels ->
                              let labels = Hashtbl.create (List.length private_labels) in
                              List.iter
                                (fun label ->
                                  let base =
                                    Printf.sprintf "__macro_%d_%s_%s" !invocation name label
                                  in
                                  Hashtbl.add labels label (fresh_private_name base))
                                private_labels;
                              incr invocation;
                              let mapper = expression_mapper bindings labels in
                              let transform (body_item : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item) : ((Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'b) item,
 Diagnostic.t list)
result =
                                match body_item.node with
                                | Statement statement ->
                                    let statement =
                                      Syntax.map_statement_expressions mapper statement
                                    in
                                    Result.map
                                      (fun statement -> { body_item with node = Statement statement })
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
                                    let rec transform_arguments (transformed : Syntax.macro_argument list) (matched_value : Syntax.macro_argument list) : (Syntax.macro_argument list, Diagnostic.t list) result = match matched_value with
                                      | [] -> Ok (List.rev transformed)
                                      | argument :: rest ->
                                          let argument =
                                            Syntax.map_argument_expressions mapper argument
                                          in
                                          (match
                                             Syntax.substitute_argument ~arguments:bindings argument
                                           with
                                          | Error diagnostics ->
                                              Error
                                                (List.map
                                                   (locate_if_missing body_item.location)
                                                   diagnostics)
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
                              let rec transform_body (transformed : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list) (matched_value : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'b) item list) : ((Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list,
 Diagnostic.t list)
result = match matched_value with
                                | [] -> expand_items (name :: stack) (List.rev transformed)
                                | body_item :: rest -> (
                                    match transform body_item with
                                    | Error diagnostics ->
                                        Error
                                          (List.map
                                             (locate_if_missing body_item.location)
                                             diagnostics)
                                    | Ok body_item ->
                                        transform_body (body_item :: transformed) rest)
                              in
                              (match transform_body [] definition.body with
                              | Error _ as error -> error
                              | Ok body -> loop (List.rev_append body expanded) rest))))
            | Statement _ | Raw_word _ | Label _ | Definition _ -> loop (item :: expanded) rest)
      in
      loop [] items
    in
    expand_items [] items

  let resolve (items : (Syntax.statement, Syntax.raw_word, 'a, 'b) item list) : (Syntax.statement list, Diagnostic.t list) result =
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
    let rec resolve_expression (visiting : string list) (current : int) (location : Diagnostic.source_location) (matched_value : Expression.t) : Expression.t = match matched_value with
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

  let assemble (items : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument,
 Syntax.parameter_kind)
item list) : (Syntax.statement list, Diagnostic.t list) result =
    match collect_macros items with
    | Error _ as error -> error
    | Ok macros -> (
        match expand macros items with Error _ as error -> error | Ok items -> resolve items)
end
