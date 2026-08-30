type execution_error =
  | Stopped of Machine_view.status
      (** The input state was already non-running; the status must agree with [inspect].
          [Stopped Running] reports an unexpected backend refusal. *)
  | Backend_error of string
      (** An invalid request or a failure that is not a machine-level halt/fail state. *)

val execution_error_message : execution_error -> string
(** Render an execution error for CLI/UI diagnostics. Backend error text is preserved verbatim. *)

type control = { status : Machine_view.status; pc : Z.t option }
(** Lightweight execution-loop data. *)

module type S = sig
  val name : string
  (** Canonical, stable backend identifier. A registry alias may be shown instead by
      [Machine_session], so machine semantics must not depend on this string. *)

  val description : string
  (** Human-readable backend summary available to clients and registries. *)

  type asm_program
  (** Backend-owned parser result for a complete program, after shared macro and symbol processing
      but before configuration-dependent concrete assembly. *)

  type asm_regfile
  (** Backend-owned parser result for an optional initial register file. *)

  type asm_word
  (** Backend-owned parser result for one editable word. The accepted syntax should include every
      [Machine_view.word.edit_text] emitted by [inspect]. *)

  type state
  (** Private dynamic machine state. States returned by this interface must remain persistent: a
      later transition or edit must not mutate an earlier state retained for undo or inspection.
      Runtime configuration is caller-owned execution context rather than mutable global state. *)

  val parse_program : ?filename:string -> string -> (asm_program, Diagnostic.t list) result
  (** Parse a complete source program and perform source-level construction. Routine user errors are
      returned as diagnostics, not raised; when [filename] is supplied, source diagnostics should
      retain it. *)

  val parse_regfile : ?filename:string -> string -> (asm_regfile, Diagnostic.t list) result
  (** Parse register-file source with the same diagnostic and filename rules as [parse_program]. *)

  val parse_word : ?filename:string -> string -> (asm_word, Diagnostic.t list) result
  (** Parse exactly one word for an interactive edit. Acceptance must be symmetric with the
      round-trippable [edit_text] exposed by [inspect]. *)

  val init :
    Runtime_config.t -> asm_program -> asm_regfile option -> (state, Diagnostic.t list) result
  (** Concretely assemble the parser-owned values under the supplied immutable configuration,
      validate initial registers and finite memory, and return the initial state. Failure is atomic:
      no partially initialized state is exposed. Later callbacks receive the same configuration. *)

  val step : Runtime_config.t -> state -> (state, execution_error) result
  (** Perform exactly one transition from a running state. A transition which changes status to
      halted or failed is still [Ok next_state]; asking to step that stopped state later returns
      [Stopped] with its status. *)

  val step_n : Runtime_config.t -> int -> state -> (state, execution_error) result
  (** Perform up to the requested number of transitions. Zero is the identity, a negative count is a
      [Backend_error], and reaching a halted/failed state early returns [Ok] with that final state.
      Other execution errors are propagated. *)

  val inspect : Runtime_config.t -> state -> Machine_view.t
  (** Produce a renderer-independent snapshot without changing [state]. Register and sparse-memory
      entries must have deterministic order, [status] and [pc] must describe the same state observed
      by stepping, [backend_name] is [name], and [address_limit] is the configuration's exclusive
      bound. Edit text must round-trip through [parse_word]; word and missing-cell semantics must be
      populated directly rather than encoded only in display strings. *)

  val control : Runtime_config.t -> state -> control
  (** Read only the execution-loop control data without constructing a full view. This operation is
      side-effect-free, and its [status] and [pc] must equal those of [inspect] for the same
      configuration and state. *)

  val set_register :
    Runtime_config.t ->
    Machine_view.Register_id.t ->
    asm_word ->
    state ->
    (state, Diagnostic.t list) result
  (** Concretely assemble [asm_word], resolve the stable view identifier, and apply a checked
      register edit. Unknown or unrepresentable identifiers and invalid words are diagnostics. On
      error the input state is unchanged; on success earlier persistent states remain valid. *)

  val set_memory : Runtime_config.t -> Z.t -> asm_word -> state -> (state, Diagnostic.t list) result
  (** Concretely assemble and write one memory word after enforcing the configured finite address
      space and backend-specific write rules. The same atomicity and persistence contract as
      [set_register] applies. *)
end
