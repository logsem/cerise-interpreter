(** Backend-neutral source construction and expansion for generated assembly parsers. *)

module Expression = Assembly_frontend.Expression

exception Parse_error of Diagnostic.source_location * string

val location : Lexing.position -> Diagnostic.source_location

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

module Make (Syntax : SYNTAX) : sig
  type source_program =
    (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, Syntax.parameter_kind) item list

  val assemble : source_program -> (Syntax.statement list, Diagnostic.t list) result
end
