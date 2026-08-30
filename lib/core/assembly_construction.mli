(** Backend-neutral source construction and expansion for generated assembly parsers. *)

module Expression : sig
  (** Parser-facing integer expressions. [Current_address], [Symbol], and [Parameter] are
      source-only and are eliminated by successful construction. [Max_address] and [Stack_address]
      deliberately survive until the backend performs concrete assembly with a {!Runtime_config.t}.
  *)
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
  (** Rewrite every existing [Symbol] node while preserving the surrounding arithmetic structure. A
      replacement is inserted as-is rather than traversed again. *)

  val map_parameter_references : (string -> t option) -> t -> t
  (** Substitute existing [Parameter] nodes for which the callback supplies an expression. Missing
      bindings remain visible so a later phase can report them; replacements are not traversed. *)

  val fold_constant_operations : t -> t
  (** Fold operations whose recursively folded operands are integer literals. Symbolic/runtime
      values and shifts with negative or non-machine-sized counts remain expressions for checked
      evaluation later. *)

  val evaluate_with_runtime_config : Runtime_config.t -> t -> (Z.t, string) result
  (** Evaluate a constructed expression using the runtime-configured address constants. The function
      rejects any source-only node left by an incomplete construction pass and reports invalid shift
      counts as errors rather than raising. *)
end

exception Parse_error of Diagnostic.source_location * string
(** Located semantic error raised by shared or backend Menhir actions. Parser entry points catch it
    and return a normal {!Diagnostic.t}; it is not an assembly-construction failure channel. *)

val source_location_of_lexing_position : Lexing.position -> Diagnostic.source_location
(** Convert a lexer position to the source-location representation used in diagnostics. Empty lexer
    filenames become [None], and columns are one-based. *)

type 'kind parameter = { name : string; kind : 'kind }
(** A declared macro parameter. Names omit the source-level [$] sigil. *)

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
(** Located parser output before construction. [Statement] and [Raw_word] each emit one word and
    therefore advance the source address by one. Labels, definitions, calls, and macro definitions
    emit no word themselves. Parsed macro bodies cannot contain a nested [Macro_definition]. *)

module type SYNTAX = sig
  (** Backend hooks which let the shared pipeline traverse and substitute a backend-owned source IR.
      Implementations must cover every expression and parameter-bearing position: successful
      construction assumes these hooks leave no source symbol or macro placeholder behind. *)

  type statement
  (** One parser-facing emitted statement. *)

  type raw_word
  (** A word written with the shared raw-word marker, before it is wrapped as a statement. *)

  type macro_argument
  (** A typed call-site argument, including any backend-specific scalar alternatives. *)

  type parameter_kind
  (** The backend's macro parameter-kind vocabulary. *)

  val statement_of_raw_word : raw_word -> statement
  (** Wrap a resolved raw word as the backend statement that emits it. This is called only after
      macro expansion and symbol rewriting and must not discard the transformed word. *)

  val parameter_kind_name : parameter_kind -> string
  (** Return the stable source spelling used in type-mismatch diagnostics. *)

  val argument_kind : macro_argument -> parameter_kind
  (** Classify a call-site argument for diagnostics. The result must describe the argument itself,
      even when [accepts_argument] supports broader parameter kinds. *)

  val accepts_argument : parameter_kind -> macro_argument -> bool
  (** Decide whether a declared kind accepts an argument. This is the authoritative call-site type
      check and may implement a deliberate super-kind such as a generic value kind. *)

  val expression_of_argument : macro_argument -> Expression.t option
  (** Expose an argument which may replace an [Expression.Parameter]. Return [None] for arguments
      that require backend-specific substitution instead; the validation/substitution hooks must be
      consistent with this distinction. *)

  val map_statement_expressions : (Expression.t -> Expression.t) -> statement -> statement
  (** Apply the mapper exactly once to every complete expression embedded in a statement while
      preserving all non-expression structure. Used both for macro hygiene/substitution and later
      for symbol resolution. *)

  val map_raw_word_expressions : (Expression.t -> Expression.t) -> raw_word -> raw_word
  (** The raw-word counterpart of [map_statement_expressions]. *)

  val map_argument_expressions : (Expression.t -> Expression.t) -> macro_argument -> macro_argument
  (** The macro-argument counterpart of [map_statement_expressions]. This is required for arguments
      of nested macro calls, which are rewritten in the outer invocation before recursive expansion.
  *)

  val validate_statement :
    parameters:(string * parameter_kind) list -> statement -> Diagnostic.t list
  (** Validate every parameter reference in a statement belonging to a macro definition against the
      complete declaration list. Return all applicable diagnostics. A diagnostic without a location
      inherits the containing source item's location; a precise backend location is kept. *)

  val validate_raw_word : parameters:(string * parameter_kind) list -> raw_word -> Diagnostic.t list
  (** The raw-word counterpart of [validate_statement]. *)

  val substitute_statement :
    arguments:(string * macro_argument) list -> statement -> (statement, Diagnostic.t list) result
  (** Replace every non-expression parameter position in an already expression-rewritten statement.
      [arguments] has passed arity and kind checks. Return diagnostics rather than silently
      retaining a placeholder when a binding cannot be represented at its use site. *)

  val substitute_raw_word :
    arguments:(string * macro_argument) list -> raw_word -> (raw_word, Diagnostic.t list) result
  (** The raw-word counterpart of [substitute_statement]. *)

  val substitute_argument :
    arguments:(string * macro_argument) list ->
    macro_argument ->
    (macro_argument, Diagnostic.t list) result
  (** Substitute non-expression positions inside one nested-call argument after its embedded
      expressions have been rewritten. The returned argument is then checked against the nested
      macro's declaration during recursive expansion. *)
end

module Make (Syntax : SYNTAX) : sig
  type source_program =
    (Syntax.statement, Syntax.raw_word, Syntax.macro_argument, Syntax.parameter_kind) item list

  val assemble_source_program : source_program -> (Syntax.statement list, Diagnostic.t list) result
  (** Run the source-construction phases in order: validate every macro declaration (including
      unused ones), expand calls recursively with hygienic per-invocation labels, then resolve
      labels/integer definitions and discard declarations. Macro calls may precede definitions.

      On success, item order and one-item/one-address accounting are preserved; [Current_address]
      has the post-expansion address of the emitted use, and no [Symbol] or [Parameter] remains.
      Configuration-dependent expressions remain for the backend's concrete-assembly phase. *)
end
