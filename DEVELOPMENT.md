# Development guide

This guide is for engineers extending the interpreter. See [assembler.md](assembler.md) for the
assembly language itself and [architecture.md](architecture.md) for the shorter architectural
overview.

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
5. `Machine_session` existentially packages the selected backend module with its private state.
   Calls to `step`, `step_n`, text edits, and `view` always unpack and repack that same backend, so
   backend-specific state and word types never leak into shared clients.
6. In batch mode, `src/interpreter_ui.ml` uses `Machine_session.run`, obtains the final
   `Machine_view.t`, and prints its stable line-oriented representation. In interactive mode,
   `Application_model` adds history, selection, and two finite memory viewports;
   `Interactive_ui` maps terminal events to model transitions and renders the pure view through
   Notty.

The important boundary is `Machine_view`: backends describe registers, memory, status, semantic
word metadata, and optional enclave data as pure backend-neutral values. Neither the installable
library nor a backend depends on terminal rendering.

## Adding or extending a backend

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
6. **Declare `codec.ml`.** Define scalar codecs and typed operand shapes, then one symmetric
   `Instruction_codec.case` per `Ast.instruction` constructor. Preserve historical fixed allocations
   where the ISA requires them. Expose `encode`, `decode`, and `allocations`, plus scalar encodings
   needed by instructions such as `restrict`, `getp`, or `getwtype`.
7. **Implement `machine.ml`.** Store the runtime configuration in the state, initialize registers
   and finite sparse memory, implement transitions, and decode fetched integer words with the
   backend codec. Preserve stopped-state and error behavior in `step` and `step_n`.
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
12. **Test the whole contract.** Cover fixed allocations, codec round trips and malformed values;
    accepted and rejected parser forms; macro expansion and symbol resolution; concrete assembly;
    initialization, transitions, failure, and finite memory; word/printer round trips;
    `Machine_session` edits; `Machine_view` metadata; registry selection; and public API
    compilation. Add CLI or TUI goldens only when output is intentionally changing.

When extending an existing instruction, follow the same route end to end: grammar and `Asm_ir`,
concrete assembly to `Ast`, codec case, printer, machine transition, view implications, signature,
and tests. Changing only the parser or machine usually leaves an inconsistent assembly/encoding
contract.

## Instruction codecs

`Instruction_codec` builds one immutable bidirectional table from typed cases. A case contains a
name, an operand `shape`, a constructor, a partial projector, and either `Auto` or `Fixed opcode`.
The constructor and projector must be inverses for that instruction constructor.

The low eight bits of an encoded instruction are the opcode; the non-negative arbitrary-precision
payload occupies the remaining high bits. Shapes determine both:

- `unit` and `scalar` use one opcode variant. `register` is a scalar shape, while `signed_zarith`
  maps signed integers to a non-negative payload.
- `register_or_constant` has span two: variant 0 is a register and variant 1 is a constant.
- `pair` multiplies the spans of its children and combines their variant indexes. Its two signed
  payloads are encoded by bit interleaving with two low sign bits, so neither component is limited
  to a machine word. `triple` is a typed nested pair.
- `variant_span` reports how many contiguous opcodes a shape requires.

Compilation first reserves every fixed range in the 256-entry opcode space. It then visits auto
cases in declaration order and assigns each the first contiguous free range at or after the moving
cursor. Duplicate names, invalid fixed opcodes, collisions, and ranges that exceed 255 are errors.
Consequently, inserting or reshaping an auto case can renumber later cases. Treat every backend's
`Codec.allocations` table as an ISA artifact: pin historical ranges with `Fixed`, test the complete
table, and review allocation diffs explicitly.

Encoding requires exactly one projector to match. It adds the shape's variant to the case's first
opcode and places the shape payload above bit 7. Decoding selects the case by its contiguous range,
derives the variant from the range offset, decodes the payload with the same shape, and invokes the
constructor. Scalar codecs should reject values outside their semantic domain in both directions;
decoders must return errors for malformed external integers rather than raise exceptions.

## Building or extending a UI

Start with `Machine_session`, not a concrete backend. It is the stable execution façade:

- `create`/`create_with_filenames` select, parse, concretely assemble, and initialize a backend;
- `step`, `step_n`, and `run` return new persistent sessions;
- `set_register_text` and `set_memory_text` parse edits with the owning backend;
- `view` returns the renderer-independent `Machine_view.t`.

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

## Machine conventions and invariants

The six handwritten machines use historical private notation from the original interpreter:

- `register @! state` delegates to `read_register`;
- `address @? state` delegates to `read_memory`;
- `!> state` delegates to `pc_next` or `advance_program_counter`.

There are deliberately no setter operators. Write-and-advance transitions spell out their ordering:
`!> (set_register destination word state)` or `!> (set_memory_raw address word state)`. This matters
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
`git diff -- lib/backends/griotte_extracted/generated` is empty. Review public `.mli` and codec
allocation diffs separately; successful execution tests alone do not prove API or ISA compatibility.
