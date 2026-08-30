(** Backend-neutral source construction and expansion for generated assembly parsers. *)

module Expression : sig
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

  val map_symbol_references : (string -> t) -> t -> t
  (** Rewrite every symbolic name while preserving the expression's arithmetic structure. *)

  val map_parameter_references : (string -> t option) -> t -> t
  (** Substitute macro parameters for which the callback supplies an expression. *)

  val fold_constant_operations : t -> t
  (** Fold operations whose operands are integer literals, leaving symbolic values intact. *)

  val evaluate_with_runtime_config : Runtime_config.t -> t -> (Z.t, string) result
  (** Evaluate a fully resolved expression using the runtime-configured address constants. *)
end

exception Parse_error of Diagnostic.source_location * string

val source_location_of_lexing_position : Lexing.position -> Diagnostic.source_location
(** Convert a lexer position to the source-location representation used in diagnostics. *)

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

module Make (Syntax : SYNTAX) : sig
  type source_program =
    (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, Syntax.parameter_kind) item list

  val assemble_source_program : source_program -> (Syntax.statement list, Diagnostic.t list) result
  (** Validate and expand macros, resolve declarations and labels, and return emitted statements. *)
end
