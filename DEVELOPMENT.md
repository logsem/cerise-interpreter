# Development guide

This guide is for engineers extending the interpreter. See [assembler.md](assembler.md) for the
assembly language itself and [architecture.md](architecture.md) for the shorter architectural
overview. See [INTERNALS.md](INTERNALS.md) for worked traces through assembly construction,
instruction encoding, and tagged metadata.

## From the CLI to rendered output

The executable is intentionally thin. Data flows through it as follows:

1. `src/cli_options.ml` parses reusable command-line values; `src/cli_parser.ml` turns them into the
   executable's arguments, including a `Runtime_config.t`, backend name, program path, optional
   register-file path, and batch or interactive mode.
2. `src/interpreter.ml` reads the files and calls `Machine_session.create_with_filenames`. Filenames
   are retained so lexer, parser, macro-expansion, symbol-resolution, and concrete-assembly
   diagnostics can report source locations.
3. `lib/backend_registry.ml` resolves the requested name to a first-class module satisfying
   `Machine_backend.S`. Compatibility names such as `cerise` select the same module while the
   session retains the spelling requested by the user.
4. `Machine_session` asks that backend to parse the program and optional register file, then calls
   `Backend.init`. A typical backend parser runs the shared lexer and its merged Menhir grammar,
   expands typed macros and resolves labels and definitions through `Assembly_construction`, and
   returns its backend-owned `Asm_ir`. `Backend.init` concretely assembles that syntax IR using the
   runtime configuration and initializes the machine's semantic `Ast.word` values. In short, the
   execution pipeline is `parse → macro expansion → symbol resolution → concrete assembly →
   execution`.
5. `Machine_session` existentially packages the selected backend module, its private state, and the
   immutable runtime configuration. Calls to `step`, `step_n`, text edits, and `view` always unpack
   and repack that same backend and configuration, so backend-specific state and word types never
   leak into shared clients and no machine state needs to retain execution context.
6. In batch mode, `src/interpreter_ui.ml` uses `Machine_session.run`, obtains the final
   `Machine_view.t`, and prints its stable line-oriented representation. In interactive mode,
   `Application_model` adds history, selection, and two finite memory viewports;
   `Interactive_ui` maps terminal events to model transitions and renders the pure view through
   Notty.

The important boundary is `Machine_view`: backends describe registers, memory, status, semantic
word metadata, and optional enclave data as pure backend-neutral values. Neither the installable
library nor a backend depends on terminal rendering.

## Backend change-impact checklist

The existing directories under `lib/backends/` are the templates. Choose the backend whose syntax
and machine are closest, but keep the new backend's semantic types, parsing, codec, and behavior
independent.

1. **Define semantic types in `ast.ml`.** Add registers, permissions, words, and instructions used
   by execution. These are post-concrete-assembly values: source expressions, parameters, and
   labels do not belong here.
2. **Define source syntax and concrete assembly in `asm_ir.ml`.** Represent parser-facing expressions,
   register/constant alternatives, words, instructions, definitions, and typed macro arguments.
   Its `Syntax` module supplies the hooks required by `Assembly_construction`; its `assemble_*`
   functions resolve configuration-dependent values and produce `Ast` values with diagnostics.
3. **Declare assembly syntax in `grammar.mly`.** The backend grammar supplies public productions
   such as `statement`, `raw_word`, `regfile_entry`, and `macro_argument`. It is merged with
   `lib/assembly/token_spec.mly` and `lib/assembly/common_parser.mly`. The shared lexer is
   `lib/assembly/lexer.mll`; add shared tokens deliberately because every backend composes these
   files and must account for unused tokens in its Dune stanza.
4. **Compose and wrap parsing.** `generated_parser.ml` includes the merged Menhir engine.
   `parser.ml` owns the `parse_program`, `parse_regfile`, and `parse_word` entry points, translates
   lexer/Menhir/construction failures to located `Diagnostic.t` values, and sends programs through
   `Asm_ir.assemble_source_program` for definition, label, and macro processing.
5. **Add `printer.ml`.** It must print machine words in syntax accepted by `parse_word`; instruction
   text is also used when an integer in a `Machine_view.word` decodes successfully. Add round-trip
   tests rather than relying on display-only examples.
6. **Declare `codec.ml`.** Define scalar codecs and typed operand codecs, then symmetric
   `Instruction_codec.encoding_pattern` declarations for the `Ast.instruction` constructors.
   A constructor may use disjoint patterns when its operand alternatives need different codecs,
   as handwritten Griotte does for register and signed-constant `Jmp` targets. Declare patterns in
   the intended opcode order and expose `encode` and `decode`. For capability metadata used by
   instructions such as `restrict`, `getp`, or `getwtype`, use the private tagged-metadata engine
   described below instead of maintaining separate scalar encoders and decoders.
7. **Implement `machine.ml`.** Keep its state limited to dynamic semantic state. Use the runtime
   configuration to initialize registers and finite sparse memory, then discard it; accept the
   configuration first in `read_memory`, `execute`, `step`, `step_n`, and any exposed `run`, and
   thread it through bounds-sensitive helpers. Preserve stopped-state and error behavior in `step`
   and `step_n`.
8. **Adapt through `backend.ml`.** Implement `Machine_backend.S`: parser delegation, concrete
   assembly and initialization, stepping, word/register mapping, `inspect`, and checked text edits.
   `inspect` should return registers in stable order and enough structured metadata for renderers;
   do not make UI code parse `short_text` to recover capability semantics.
9. **Expose the backend group.** Add `<backend>.ml` as the group index, a matching explicit
   `<backend>.mli` public surface, and an alias in `lib/cerise.ml` if it is public. Keep private
   implementation helpers out of the signature.
10. **Register CLI names.** Add the first-class backend module and canonical/compatibility entries
    to `lib/backend_registry.ml`; update user-facing backend lists where appropriate.
11. **Wire Dune.** Give the directory a `dune` file that copies the shared `.mly` files, runs
    `ocamllex`, and merges `token_spec`, `common_parser`, and the backend `grammar` into a uniquely
    named parser engine. The root library uses `(include_subdirs qualified)`, so a correctly named
    backend group is discovered without flattening its modules. Update `src/dune` only when adding
    executable-side modules, not for an ordinary library backend.
12. **Test the whole contract.** Cover every instruction constructor and operand variant with codec
    round trips and malformed values; accepted and rejected parser forms; macro expansion and
    symbol resolution; concrete assembly; initialization, transitions, failure, and finite memory;
    word/printer round trips; `Machine_session` edits; `Machine_view` metadata; registry selection;
    and public API compilation. Add CLI or TUI goldens only when output is intentionally changing.

When extending an existing instruction, follow the same route end to end: grammar and `Asm_ir`,
concrete assembly to `Ast`, encoding pattern, printer, machine transition, view implications,
signature, and tests. Changing only the parser or machine usually leaves an inconsistent
assembly/encoding contract.

## Instruction, metadata, and syntax change-impact checklists

For an instruction change, account for every consumer of the instruction rather than treating its
constructor as the feature boundary:

1. Add the concrete constructor and operand types to the backend `Ast`, and the unresolved term plus
   macro validation, expression mapping, and substitution cases to `Asm_ir`.
2. Add the backend grammar production and concrete-assembly case. If the mnemonic needs a new shared
   lexer token, add it to `token_spec.mly` and `lexer.mll`, then mark it as unused in every backend
   Dune stanza whose grammar does not consume it. Token recognition is not permission to accept it.
3. Add a symmetric codec pattern and printer support. Pattern order is encoding state: inserting or
   widening a compact-codec pattern can renumber later opcodes, so review integer goldens as an ISA
   change rather than accepting regenerated expectations mechanically.
4. Implement success, failure, stopped-state, PC-update, and finite-memory behavior in the machine;
   extend `inspect` when the instruction introduces semantic state a renderer needs.
5. Update the group signature if the public types changed, the exact ISA in `assembler.md`, and tests
   for parsing, macros, concrete assembly, all operand variants, encoding failures, execution, view
   data, and public API compilation.

For a metadata value or layout change, update the semantic type, parser/`Asm_ir` constant handling,
printer, machine consumers, and structured `Machine_view` projection together. Extend the existing
finite scalar declaration and compiled tagged layout instead of adding a separate reverse decoder.
Treat scalar numbers, field widths, tags, and established error strings as encoding compatibility;
cover every value, composite pair, wrong tag, extra high bits, and public wrapper with explicit
goldens before accepting a layout change.

For an assembly-language change, first decide its ownership. Expressions, declarations, labels,
placement, and macro containers are shared construction concepts; instruction and architectural
value shapes are backend grammar concepts. Then:

1. Keep the shared lexer a classifier only. Add backend acceptance in its `grammar.mly`, and add
   negative parser cases for other backends that can now receive the same token.
2. Exercise all affected entry points independently: program, register file, and single-word edits.
   Preserve source filenames and locations when translating lexer, Menhir, expansion, resolution,
   and concrete-assembly failures to diagnostics.
3. If a new form can occur inside a macro, update the backend's `Assembly_construction.SYNTAX`
   mappings, validation, and substitution together; test nested calls, typed arguments, private
   labels, and `&CURRENT_ADDR` after expansion where relevant.
4. Update accepted and rejected fixtures for every affected backend, run Menhir with strict unused
   token accounting, and document the exact selected-backend behavior in `assembler.md`.

## Codec authoring boundaries

[INTERNALS.md](INTERNALS.md) explains opcode spans, arbitrary-precision payload pairing, the low
eight-bit opcode layout, and tagged metadata validation in detail. For extension work, keep these
authoring contracts in view:

- A handwritten instruction codec is declared as scalar codecs, typed operand codecs, and named
  encoding patterns, then compiled once. A pattern's constructor and projector must be inverses,
  and projectors for different patterns must not overlap. Declaration order and operand span assign
  opcode ranges, so inserting, reordering, or widening a pattern is an integer-encoding change.
- Decoding handles external `Z.t` values and must return structured errors for negative values,
  unknown opcodes, invalid variants, and malformed payloads rather than raise. Cover each operand
  variant and constructor/projector direction, not only one successful instruction per mnemonic.
- The six handwritten backends use the repository-private `Tagged_metadata_codec`. Declare each
  semantic scalar mapping once, compose any fixed-width payload, compile the typed tagged patterns,
  and have named public wrappers reuse those exact pattern values. Do not maintain a separate
  decoding table.
- Assembly text is the interchange form between backends with a shared textual ISA; encoded
  instruction and metadata integers remain backend-owned. Extracted Griotte retains an independent
  fixed adapter codec and extracted representation rather than using either shared authoring engine.

## Building or extending a UI

Start with `Machine_session`, not a concrete backend. It is the stable execution façade:

- `create`/`create_with_filenames` select, parse, concretely assemble, and initialize a backend;
- `step`, `step_n`, and `run` return new persistent sessions;
- `set_register_text` and `set_memory_text` parse edits with the owning backend;
- `view` returns the renderer-independent `Machine_view.t`.

The session owns the runtime configuration as execution context and automatically supplies the
same immutable value to every backend operation. Low-level callers of a concrete `Machine` module
must provide the configuration first and consistently reuse the value passed to `Machine.init`;
mixing configurations changes bounds-sensitive semantics and is unsupported.

Build reusable renderers from `Machine_view` fields. Use `register.id` for edits, `role` for PC and
stack behavior, `semantic_kind` and structured capability/sealing records for styling, `edit_text`
for round-trippable edits, and `short_text`/`detail_text` for display. Honor `missing_cell`: sparse
backends may define an in-range default word or report an address as unmapped.

For a stateful UI, `src/application_model.ml` is the next layer. It records prior immutable sessions
for undo, clamps both viewports to `view.address_limit`, follows the PC or a valid capability cursor,
and owns capability selection. Keep navigation and history rules here so they can be tested without
a terminal.

`src/interactive_ui.ml` is the terminal controller and renderer. Extend its `event` type and pure
`transition` first, then update Notty rendering and input mapping. The `snapshot`, `ansi_snapshot`,
and word snapshot helpers support deterministic tests. Preserve behavior at narrow dimensions and
for both viewport sides; update `tests/tui_fidelity_tests.ml` and goldens only for intended visual
changes. A non-terminal frontend can stop at `Machine_session`/`Machine_view`, or reuse the
application model if its history and viewport behavior fit.

### Terminal UI contract

Treat these layout and navigation rules as behavior, not incidental rendering details:

- Registers are dense and column-major. When space is limited, retain the program counter first,
  then the valid stack pointer, backend-specific registers, and the selected capability; show an
  omitted-register count instead of silently clipping. Word layout comes from structured semantic
  fields, uses Zarith-safe widths, and falls back to elided neutral text for incomplete metadata.
- The primary `HEAP` panel follows the program counter. The mirrored secondary panel is `STACK`
  when a valid `Stack_pointer` capability exists, otherwise the selected capability. Capability
  limits are exclusive, cursor and range indicators belong to their panel, and decoded instruction
  text is shown only where an integer lies inside the panel capability's authority.
- A wide layout shows both memory panels only when each half fits all fixed semantic fields. A
  narrower layout removes the secondary panel first and gives the primary the full width. The
  normal-height layout reserves at least three memory data rows; very short terminals use a minimal
  status/HEAP/footer layout. The `machine state:` status is right-aligned and the backend footer is
  retained. Rendering must always return the requested dimensions.
- Initial display follows both targets. A successful one- or ten-step command creates one undo
  entry and follows both panels; an execution error leaves UI state and history unchanged. Undo
  removes one command entry, follows the restored primary target, and preserves the secondary
  viewport.
- Following leaves an already visible target in place. An off-screen target receives up to two
  rows of leading context. A page moves `max 1 (rows - 2)` addresses, preserving a two-row overlap,
  while row moves use one address. Every start address is clamped to
  `[0, address_limit - 1]`.
- A valid stack pointer always wins as the secondary target. Capability cycling changes the
  fallback selection and follows it only when no valid stack pointer exists. Hiding the secondary
  panel preserves its viewport; showing it again refollows the active secondary target.
- Mouse scrolling targets the secondary viewport only when the pointer is in the secondary panel
  of the layout that was actually rendered; narrow layouts always target primary. `Ctrl` changes a
  wheel row move into a page move.

When changing the TUI, keep the pure state transition in `Application_model` or
`Interactive_ui.transition`, then update event mapping and rendering. Add transition tests for both
panels, text and ANSI snapshots for intentional visuals, malformed-metadata fallbacks, narrow/wide
and zero-size dimensions, and the terminal release smoke test. If `Machine_view` gains a field,
populate it in every backend and verify the batch renderer as well as the TUI.

## Machine conventions and invariants

The six handwritten machines use historical private notation from the original
interpreter:

- `register @! state` delegates to `read_register`;
- `address @? (config, state)` delegates to `read_memory config`;
- `!> state` delegates to `pc_next` or `advance_program_counter`.

There are deliberately no setter operators. Write-and-advance transitions spell out their ordering:
`!> (set_register destination word state)` or `!> (set_memory_raw address word state)`. The paired
right operand of `@?` makes its execution context explicit without storing it in the state. This
matters
when the destination is `PC`, when a backend has a hard-wired zero register, and when advancing an
invalid PC changes status. Jumps and other control transfers that install a new PC intentionally do
not use `!>`. Keep the named read/write functions as the public backend API; the operators remain
private implementation notation.

All active runtimes have a finite configured address space. Sparse memory means an unwritten
in-range cell may read as the backend's default word, while negative or out-of-range addresses are
invalid. Enforce the same bound during initialization, fetch/PC validation, loads and stores, edits,
and UI navigation. Handwritten Griotte additionally models finite architectural address and object
type domains; preserve those checks even when `Z.t` can represent a larger value.

`lib/backends/griotte/` is handwritten. `lib/backends/griotte_extracted/generated/` is a checked-in
Rocq extraction snapshot and must not be hand-edited or formatted. Its handwritten adapter lives
outside `generated/`. The two Griotte backends intentionally differ outside their documented shared
subset; see [griotte-snapshots.md](griotte-snapshots.md). Regeneration scripts are the trust boundary
for generated bytes and provenance.

## Formatting and validation

Format handwritten OCaml with the repository's `ocamlformat` configuration (or run
`nix develop --command dune fmt`). Never include extracted generated files in a formatting rewrite.
Before submitting a backend, codec, machine, or UI change, run the relevant focused tests and then:

```sh
make test
make no-warning
make format-production-check
make check-griotte-extracted
make regeneration-gate
nix build --no-link --print-build-logs --rebuild .#
```

`make test` includes public contracts, backend suites, codec failures, differential Griotte coverage,
and CLI/TUI goldens. The two Griotte checks verify provenance, byte identity, atomic installation,
and offline idempotence. Before committing generated-sensitive work, also confirm that
`git diff -- lib/backends/griotte_extracted/generated` is empty. Review public `.mli` and intended
instruction-integer changes separately; successful execution tests alone do not prove API or ISA
compatibility.
