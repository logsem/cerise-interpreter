(** Backend-neutral assembly construction. Instructions, raw words, register files, and macro
    argument vocabularies remain abstract and are supplied by each backend. *)

module Token : sig
  type kind =
    | Identifier of string
    | Integer of Z.t
    | Parameter of string
    | Directive of string
    | Punctuation of char
    | Assign
    | Current_address

  type t

  val kind : t -> kind
  val text : t -> string
  val location : t -> Diagnostic.source_location
end

type 'a parsed = ('a * Token.t list, Diagnostic.t list) result

val tokenize : ?filename:string -> string -> (Token.t list, Diagnostic.t list) result

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

  val map_symbols : (string -> t) -> t -> t
  val map_parameters : (string -> t option) -> t -> t
  val simplify : t -> t
  val evaluate_runtime : Runtime_config.t -> t -> (Z.t, string) result
end

val parse_expression : Token.t list -> Expression.t parsed

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

module Make (Syntax : SYNTAX) : sig
  type program = Syntax.statement list
  type regfile = Syntax.regfile
  type word = Syntax.raw_word

  val parse_program : ?filename:string -> string -> (program, Diagnostic.t list) result
  val parse_regfile : ?filename:string -> string -> (regfile, Diagnostic.t list) result
  val parse_word : ?filename:string -> string -> (word, Diagnostic.t list) result
end
