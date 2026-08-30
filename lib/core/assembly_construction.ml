module Expression = struct
  type t = Integer of Z.t | Current_address | Max_address | Stack_address | Symbol of string | Parameter of string
    | Add of t * t | Subtract of t * t | Multiply of t * t | Logand of t * t | Logor of t * t
    | Shift_left of t * t | Shift_right of t * t
  (* Convert an evaluated shift count to the bounded OCaml integer expected by Zarith. *)
  let nonnegative_shift_count_as_int (value : Z.t) : (int, string) result =
    if Z.sign value < 0 then Error "shift count must be non-negative"
    else if not (Z.fits_int value) then Error "shift count does not fit in a machine integer"
    else Ok (Z.to_int value)
  (* Rewrite every symbolic name in an expression while preserving its arithmetic structure. *)
  let rec map_symbol_references (rewrite_symbol : string -> t) (matched_value : t) : t = match matched_value with
    | Symbol name -> rewrite_symbol name
    | Add (a,b) -> Add (map_symbol_references rewrite_symbol a,map_symbol_references rewrite_symbol b) | Subtract (a,b) -> Subtract (map_symbol_references rewrite_symbol a,map_symbol_references rewrite_symbol b)
    | Multiply (a,b) -> Multiply (map_symbol_references rewrite_symbol a,map_symbol_references rewrite_symbol b) | Logand (a,b) -> Logand (map_symbol_references rewrite_symbol a,map_symbol_references rewrite_symbol b)
    | Logor (a,b) -> Logor (map_symbol_references rewrite_symbol a,map_symbol_references rewrite_symbol b) | Shift_left (a,b) -> Shift_left (map_symbol_references rewrite_symbol a,map_symbol_references rewrite_symbol b)
    | Shift_right (a,b) -> Shift_right (map_symbol_references rewrite_symbol a,map_symbol_references rewrite_symbol b)
    | (Integer _ | Current_address | Max_address | Stack_address | Parameter _) as e -> e
  (* Substitute macro expression parameters, leaving parameters without a binding unresolved. *)
  let rec map_parameter_references (substitute_parameter : string -> t option) (matched_value : t) : t = match matched_value with
    | Parameter name as expression -> Option.value (substitute_parameter name) ~default:expression
    | Add (a,b) -> Add (map_parameter_references substitute_parameter a,map_parameter_references substitute_parameter b) | Subtract (a,b) -> Subtract (map_parameter_references substitute_parameter a,map_parameter_references substitute_parameter b)
    | Multiply (a,b) -> Multiply (map_parameter_references substitute_parameter a,map_parameter_references substitute_parameter b) | Logand (a,b) -> Logand (map_parameter_references substitute_parameter a,map_parameter_references substitute_parameter b)
    | Logor (a,b) -> Logor (map_parameter_references substitute_parameter a,map_parameter_references substitute_parameter b) | Shift_left (a,b) -> Shift_left (map_parameter_references substitute_parameter a,map_parameter_references substitute_parameter b)
    | Shift_right (a,b) -> Shift_right (map_parameter_references substitute_parameter a,map_parameter_references substitute_parameter b)
    | (Integer _ | Current_address | Max_address | Stack_address | Symbol _) as e -> e
  (* Fold arithmetic whose operands are already integer literals. Symbolic runtime values remain. *)
  let rec fold_constant_operations (matched_value : t) : t = match matched_value with
    | Add (a,b) -> (match fold_constant_operations a,fold_constant_operations b with Integer x,Integer y -> Integer Z.(x+y) | a,b -> Add(a,b))
    | Subtract (a,b) -> (match fold_constant_operations a,fold_constant_operations b with Integer x,Integer y -> Integer Z.(x-y) | a,b -> Subtract(a,b))
    | Multiply (a,b) -> (match fold_constant_operations a,fold_constant_operations b with Integer x,Integer y -> Integer Z.(x*y) | a,b -> Multiply(a,b))
    | Logand (a,b) -> (match fold_constant_operations a,fold_constant_operations b with Integer x,Integer y -> Integer (Z.logand x y) | a,b -> Logand(a,b))
    | Logor (a,b) -> (match fold_constant_operations a,fold_constant_operations b with Integer x,Integer y -> Integer (Z.logor x y) | a,b -> Logor(a,b))
    | Shift_left (a,b) -> (match fold_constant_operations a,fold_constant_operations b with Integer x,Integer y when Z.sign y >= 0 && Z.fits_int y -> Integer (Z.shift_left x (Z.to_int y)) | a,b -> Shift_left(a,b))
    | Shift_right (a,b) -> (match fold_constant_operations a,fold_constant_operations b with Integer x,Integer y when Z.sign y >= 0 && Z.fits_int y -> Integer (Z.shift_right x (Z.to_int y)) | a,b -> Shift_right(a,b))
    | expression -> expression

  (* Evaluate an expression after labels and macro parameters have been resolved. Runtime address
      constants are read from [config]; any remaining source-only construct is an error. *)
  let rec evaluate_with_runtime_config (config : Runtime_config.t) (matched_value : t) : (Z.t, string) result = match matched_value with
    | Integer v -> Ok v | Max_address -> Ok (Runtime_config.max_addr config) | Stack_address -> Ok (Runtime_config.stack_addr config)
    | Add(a,b) -> Result.bind (evaluate_with_runtime_config config a) (fun x -> Result.map (Z.add x) (evaluate_with_runtime_config config b))
    | Subtract(a,b) -> Result.bind (evaluate_with_runtime_config config a) (fun x -> Result.map (Z.sub x) (evaluate_with_runtime_config config b))
    | Multiply(a,b) -> Result.bind (evaluate_with_runtime_config config a) (fun x -> Result.map (Z.mul x) (evaluate_with_runtime_config config b))
    | Logand(a,b) -> Result.bind (evaluate_with_runtime_config config a) (fun x -> Result.map (Z.logand x) (evaluate_with_runtime_config config b))
    | Logor(a,b) -> Result.bind (evaluate_with_runtime_config config a) (fun x -> Result.map (Z.logor x) (evaluate_with_runtime_config config b))
    | Shift_left(a,b) -> Result.bind (evaluate_with_runtime_config config a) (fun x -> Result.bind (evaluate_with_runtime_config config b) (fun y -> Result.map (Z.shift_left x) (nonnegative_shift_count_as_int y)))
    | Shift_right(a,b) -> Result.bind (evaluate_with_runtime_config config a) (fun x -> Result.bind (evaluate_with_runtime_config config b) (fun y -> Result.map (Z.shift_right x) (nonnegative_shift_count_as_int y)))
    | Current_address -> Error "an unresolved current-address expression remains" | Symbol n -> Error (Printf.sprintf "an unresolved symbol %S remains" n)
    | Parameter n -> Error (Printf.sprintf "an unexpanded macro parameter $%s remains" n)
end

exception Parse_error of Diagnostic.source_location * string

(* Convert a lexer position to the source-location representation used in diagnostics. *)
let source_location_of_lexing_position (position : Lexing.position) : Diagnostic.source_location =
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

  (* Build an assembly diagnostic at the location of the source construct that caused it. *)
  let diagnostic_at_source_location (location : Diagnostic.source_location) (message : string) : Diagnostic.t = Diagnostic.error ~location message

  (* Preserve a precise backend diagnostic location, or attach the enclosing assembly item's
      location when the backend did not provide one. *)
  let add_source_location_if_missing (location : Diagnostic.source_location) (diagnostic : Diagnostic.t) : Diagnostic.t =
    match Diagnostic.location diagnostic with
    | Some _ -> diagnostic
    | None ->
        Diagnostic.make ~severity:(Diagnostic.severity diagnostic) ~location
          (Diagnostic.message diagnostic)

  (* Report every repeated parameter name in one macro declaration. *)
  let find_duplicate_macro_parameters
      (definition :
        (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, Syntax.parameter_kind)
        macro_definition) : Diagnostic.t list =
    let seen = Hashtbl.create (List.length definition.parameters) in
    List.filter_map
      (fun (parameter : Syntax.parameter_kind parameter) ->
        if Hashtbl.mem seen parameter.name then
          Some
            (diagnostic_at_source_location definition.declaration_location
               (Printf.sprintf "Macro %S declares parameter $%s more than once." definition.name
                  parameter.name))
        else (
          Hashtbl.add seen parameter.name ();
          None))
      definition.parameters

  (* Validate parameter declarations and backend-specific uses of parameters in a macro body. *)
  let validate_macro_definition
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
          List.map (add_source_location_if_missing item.location) diagnostics)
        definition.body
    in
    find_duplicate_macro_parameters definition @ body_errors

  (* Build the macro lookup table and reject invalid or duplicate definitions before expansion. *)
  let collect_and_validate_macro_definitions (items : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument,
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
                diagnostic_at_source_location definition.declaration_location
                  (Printf.sprintf "Macro %S is declared more than once." definition.name)
                :: !errors
            else Hashtbl.add macros definition.name definition;
            errors := List.rev_append (validate_macro_definition definition) !errors
        | Statement _ | Raw_word _ | Label _ | Definition _ | Macro_call _ -> ())
      items;
    if !errors = [] then Ok macros else Error (List.rev !errors)

  (* Collect labels declared inside a macro. They are renamed per invocation during expansion so
      different calls cannot capture or collide with one another. *)
  let collect_macro_local_labels
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
                diagnostic_at_source_location item.location
                  (Printf.sprintf "Macro %S has duplicate private label %S." definition.name name)
                :: !duplicates
            else (
              Hashtbl.add seen name ();
              labels := name :: !labels)
        | Statement _ | Raw_word _ | Definition _ | Macro_call _ | Macro_definition _ -> ())
      definition.body;
    if !duplicates = [] then Ok (List.rev !labels) else Error (List.rev !duplicates)

  (* Pair a call's arguments with the declared parameters after checking arity and argument kinds. *)
  let bind_macro_call_arguments
      (definition :
        (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, Syntax.parameter_kind)
        macro_definition)
      (arguments : Syntax.macro_argument list) (location : Diagnostic.source_location) : ((string * Syntax.macro_argument) list, Diagnostic.t list) result =
    if List.length definition.parameters <> List.length arguments then
      Error
        [
          diagnostic_at_source_location location
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
                (diagnostic_at_source_location location
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

  (* Substitute expression-valued macro parameters and rewrite macro-local label references. *)
  let rewrite_macro_expression (bindings : (string * Syntax.macro_argument) list) (renamed_local_labels : (string, string) Hashtbl.t) (expression : Expression.t) : Expression.t =
    expression
    |> Expression.map_parameter_references (fun name ->
           Option.bind (List.assoc_opt name bindings) Syntax.expression_of_argument)
    |> Expression.map_symbol_references (fun name ->
           match Hashtbl.find_opt renamed_local_labels name with
           | Some renamed -> Expression.Symbol renamed
           | None -> Expression.Symbol name)

  (* Replace every macro call with a validated, substituted copy of its body. Expansion is
      recursive, rejects call cycles, and gives each invocation fresh names for its local labels. *)
  let expand_macro_calls (macro_definitions : (string,
 (Syntax.statement, Syntax.raw_word, Syntax.macro_argument,
  Syntax.parameter_kind)
 macro_definition)
Hashtbl.t) (items : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list) : ((Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list,
 Diagnostic.t list)
result =
    let invocation_counter = ref 0 in
    let reserved_symbol_names = Hashtbl.create 32 in
    let reserve_symbol_name (name : string) : unit =
      Hashtbl.replace reserved_symbol_names name ()
    in
    List.iter
      (fun item ->
        match item.node with
        | Label name | Definition (name, _) -> reserve_symbol_name name
        | Macro_definition definition ->
            List.iter
              (fun body_item ->
                match body_item.node with
                | Definition (name, _) -> reserve_symbol_name name
                | _ -> ())
              definition.body
        | Statement _ | Raw_word _ | Macro_call _ -> ())
      items;
    let fresh_macro_local_label (base : string) : string =
      let rec choose_unused_label (suffix : int) : string =
        let candidate = if suffix = 0 then base else Printf.sprintf "%s_%d" base suffix in
        if Hashtbl.mem reserved_symbol_names candidate then choose_unused_label (suffix + 1)
        else (
          reserve_symbol_name candidate;
          candidate)
      in
      choose_unused_label 0
    in
    let rec expand_item_sequence (expansion_stack : string list) (items : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list) : ((Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list,
 Diagnostic.t list)
result =
      let rec expand_remaining_items (expanded_items : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list) (remaining_items : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list) : ((Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list,
 Diagnostic.t list)
result = match remaining_items with
        | [] -> Ok (List.rev expanded_items)
        | item :: rest -> (
            match item.node with
            | Macro_definition _ -> expand_remaining_items expanded_items rest
            | Macro_call (name, arguments) -> (
                match Hashtbl.find_opt macro_definitions name with
                | None ->
                    Error [ diagnostic_at_source_location item.location (Printf.sprintf "Unknown macro %S." name) ]
                | Some definition ->
                    if List.mem name expansion_stack then
                      Error
                        [
                          diagnostic_at_source_location item.location
                            (Printf.sprintf "Recursive macro call involving %S." name);
                        ]
                    else (
                      match bind_macro_call_arguments definition arguments item.location with
                      | Error _ as error -> error
                      | Ok bindings -> (
                          match collect_macro_local_labels definition with
                          | Error _ as error -> error
                          | Ok private_labels ->
                              let renamed_local_labels = Hashtbl.create (List.length private_labels) in
                              List.iter
                                (fun label ->
                                  let base =
                                    Printf.sprintf "__macro_%d_%s_%s" !invocation_counter name label
                                  in
                                  Hashtbl.add renamed_local_labels label (fresh_macro_local_label base))
                                private_labels;
                              incr invocation_counter;
                              let rewrite_expression =
                                rewrite_macro_expression bindings renamed_local_labels
                              in
                              let substitute_macro_body_item (body_item : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item) : ((Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'b) item,
 Diagnostic.t list)
result =
                                match body_item.node with
                                | Statement statement ->
                                    let statement =
                                      Syntax.map_statement_expressions rewrite_expression statement
                                    in
                                    Result.map
                                      (fun statement -> { body_item with node = Statement statement })
                                      (Syntax.substitute_statement ~arguments:bindings statement)
                                | Raw_word word ->
                                    let word =
                                      Syntax.map_raw_word_expressions rewrite_expression word
                                    in
                                    Result.map
                                      (fun word -> { body_item with node = Raw_word word })
                                      (Syntax.substitute_raw_word ~arguments:bindings word)
                                | Label label ->
                                    let label =
                                      Option.value
                                        (Hashtbl.find_opt renamed_local_labels label)
                                        ~default:label
                                    in
                                    Ok { body_item with node = Label label }
                                | Definition (definition_name, expression) ->
                                    Ok
                                      {
                                        body_item with
                                        node =
                                          Definition
                                            (definition_name, rewrite_expression expression);
                                      }
                                | Macro_call (called, call_arguments) ->
                                    let rec substitute_nested_call_arguments (transformed : Syntax.macro_argument list) (matched_value : Syntax.macro_argument list) : (Syntax.macro_argument list, Diagnostic.t list) result = match matched_value with
                                      | [] -> Ok (List.rev transformed)
                                      | argument :: rest ->
                                          let argument =
                                            Syntax.map_argument_expressions rewrite_expression argument
                                          in
                                          (match
                                             Syntax.substitute_argument ~arguments:bindings argument
                                           with
                                          | Error diagnostics ->
                                              Error
                                                (List.map
                                                   (add_source_location_if_missing body_item.location)
                                                   diagnostics)
                                          | Ok argument ->
                                              substitute_nested_call_arguments (argument :: transformed) rest)
                                    in
                                    Result.map
                                      (fun arguments ->
                                        { body_item with node = Macro_call (called, arguments) })
                                      (substitute_nested_call_arguments [] call_arguments)
                                | Macro_definition _ ->
                                    Error
                                      [
                                        diagnostic_at_source_location body_item.location
                                          "Nested macro declarations are not supported.";
                                      ]
                              in
                              let rec substitute_macro_body (transformed : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list) (matched_value : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'b) item list) : ((Syntax.statement, Syntax.raw_word, Syntax.macro_argument, 'a) item list,
 Diagnostic.t list)
result = match matched_value with
                                | [] ->
                                    expand_item_sequence
                                      (name :: expansion_stack)
                                      (List.rev transformed)
                                | body_item :: rest -> (
                                    match substitute_macro_body_item body_item with
                                    | Error diagnostics ->
                                        Error
                                          (List.map
                                             (add_source_location_if_missing body_item.location)
                                             diagnostics)
                                    | Ok body_item ->
                                        substitute_macro_body (body_item :: transformed) rest)
                              in
                              (match substitute_macro_body [] definition.body with
                              | Error _ as error -> error
                              | Ok body ->
                                  expand_remaining_items
                                    (List.rev_append body expanded_items)
                                    rest))))
            | Statement _ | Raw_word _ | Label _ | Definition _ ->
                expand_remaining_items (item :: expanded_items) rest)
      in
      expand_remaining_items [] items
    in
    expand_item_sequence [] items

  (* Resolve integer definitions, labels, and current-address expressions, then discard source-only
      declarations. The first pass records addresses; the second rewrites emitted statements. *)
  let resolve_symbols_and_remove_declarations (items : (Syntax.statement, Syntax.raw_word, 'a, 'b) item list) : (Syntax.statement list, Diagnostic.t list) result =
    let integer_definitions = Hashtbl.create 16 in
    let label_addresses = Hashtbl.create 32 in
    let errors = ref [] in
    let next_address = ref 0 in
    List.iter
      (fun item ->
        match item.node with
        | Definition (name, expression) ->
            if Hashtbl.mem integer_definitions name then
              errors :=
                diagnostic_at_source_location item.location
                  (Printf.sprintf "Integer definition %S is declared more than once." name)
                :: !errors
            else Hashtbl.add integer_definitions name (expression, item.location)
        | Label name ->
            if Hashtbl.mem label_addresses name then
              errors :=
                diagnostic_at_source_location item.location
                  (Printf.sprintf "Label %S is declared more than once." name)
                :: !errors
            else Hashtbl.add label_addresses name (Z.of_int !next_address, item.location)
        | Statement _ | Raw_word _ -> incr next_address
        | Macro_call _ | Macro_definition _ -> assert false)
      items;
    Hashtbl.iter
      (fun name (_, location) ->
        if Hashtbl.mem label_addresses name then
          errors :=
            diagnostic_at_source_location location
              (Printf.sprintf "Integer definition %S conflicts with a label of the same name." name)
            :: !errors)
      integer_definitions;
    let rec resolve_expression_symbols (definitions_being_resolved : string list) (current_address : int) (location : Diagnostic.source_location) (matched_value : Expression.t) : Expression.t = match matched_value with
      | Expression.Integer _ as expression -> expression
      | Current_address -> Integer (Z.of_int current_address)
      | Max_address -> Max_address
      | Stack_address -> Stack_address
      | Parameter name ->
          errors :=
            diagnostic_at_source_location location
              (Printf.sprintf "Unexpanded macro parameter $%s reached label resolution." name)
            :: !errors;
          Parameter name
      | Symbol name -> (
          match Hashtbl.find_opt integer_definitions name with
          | Some (expression, definition_location) ->
              if List.mem name definitions_being_resolved then (
                errors :=
                  diagnostic_at_source_location definition_location
                    (Printf.sprintf "Cyclic integer definition involving %S." name)
                  :: !errors;
                Symbol name)
              else
                resolve_expression_symbols
                  (name :: definitions_being_resolved)
                  current_address definition_location expression
          | None -> (
              match Hashtbl.find_opt label_addresses name with
              | Some (address, _) -> Integer address
              | None ->
                  errors :=
                    diagnostic_at_source_location location
                      (Printf.sprintf "Unknown label or integer definition %S." name)
                    :: !errors;
                  Symbol name))
      | Add (left, right) ->
          Expression.fold_constant_operations
            (Add
               ( resolve_expression_symbols definitions_being_resolved current_address location left,
                 resolve_expression_symbols definitions_being_resolved current_address location right ))
      | Subtract (left, right) ->
          Expression.fold_constant_operations
            (Subtract
               ( resolve_expression_symbols definitions_being_resolved current_address location left,
                 resolve_expression_symbols definitions_being_resolved current_address location right ))
      | Multiply (left, right) ->
          Expression.fold_constant_operations
            (Multiply
               ( resolve_expression_symbols definitions_being_resolved current_address location left,
                 resolve_expression_symbols definitions_being_resolved current_address location right ))
      | Logand (left, right) ->
          Expression.fold_constant_operations
            (Logand
               ( resolve_expression_symbols definitions_being_resolved current_address location left,
                 resolve_expression_symbols definitions_being_resolved current_address location right ))
      | Logor (left, right) ->
          Expression.fold_constant_operations
            (Logor
               ( resolve_expression_symbols definitions_being_resolved current_address location left,
                 resolve_expression_symbols definitions_being_resolved current_address location right ))
      | Shift_left (left, right) ->
          Expression.fold_constant_operations
            (Shift_left
               ( resolve_expression_symbols definitions_being_resolved current_address location left,
                 resolve_expression_symbols definitions_being_resolved current_address location right ))
      | Shift_right (left, right) ->
          Expression.fold_constant_operations
            (Shift_right
               ( resolve_expression_symbols definitions_being_resolved current_address location left,
                 resolve_expression_symbols definitions_being_resolved current_address location right ))
    in
    let current_address = ref 0 in
    let resolved_statements =
      List.filter_map
        (fun item ->
          let resolve_expression =
            resolve_expression_symbols [] !current_address item.location
          in
          match item.node with
          | Statement statement ->
              incr current_address;
              Some (Syntax.map_statement_expressions resolve_expression statement)
          | Raw_word word ->
              incr current_address;
              Some
                (Syntax.statement_of_raw_word
                   (Syntax.map_raw_word_expressions resolve_expression word))
          | Definition _ | Label _ -> None
          | Macro_call _ | Macro_definition _ -> assert false)
        items
    in
    if !errors = [] then Ok resolved_statements else Error (List.rev !errors)

  (* Run the assembly-construction pipeline in dependency order: validate macro definitions,
      expand calls, then resolve symbols into the final backend statements. *)
  let assemble_source_program (items : (Syntax.statement, Syntax.raw_word, Syntax.macro_argument,
 Syntax.parameter_kind)
item list) : (Syntax.statement list, Diagnostic.t list) result =
    match collect_and_validate_macro_definitions items with
    | Error _ as error -> error
    | Ok macros -> (
        match expand_macro_calls macros items with Error _ as error -> error | Ok items -> resolve_symbols_and_remove_declarations items)
end
