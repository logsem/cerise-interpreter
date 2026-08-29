# Backend Refactor — Second Pass

## Summary

Rebuild the accepted work as a clean, linear history rooted at the current `origin/main`,
preserving `c7f9009` exactly as its first child. Then reorganize every backend into a namespaced
directory, separate concrete machine ASTs from assembly IRs, replace handwritten parsing with
ocamllex/Menhir, restore the rich Notty terminal UI, and remove all web-UI compatibility code.

No merge commits, compatibility aliases, pushes, or web-facing fixtures remain.

## History and Integration

- Freeze the currently fetched `origin/main` at
  `c7a9ad037527c1f0ba54d3c7bf044d89c972ee52`; verify that its exact child is
  `c7f900962382694b352c64ce3797fac7573a0af6`.
- Preserve the accepted first-pass tip
  `bba78ebcf121ef63c2b79ed797dea4116d02311f` on a local safety branch.
- Build a new integration branch beginning with the unchanged `c7f9009`, followed by these
  buildable, ordinary commits:

  1. Establish backend sessions and characterization tests.
  2. Replace surface lowering with backend-owned parsing.
  3. Add vanilla and locality Cerise backends.
  4. Add historical uCerise and mCerise backends.
  5. Migrate the terminal application to backend sessions.
  6. Add the handwritten Griotte backend.
  7. Add the Rocq-extracted Griotte backend.
  8. Add the historical Cerisier backend.
  9. Complete backend cleanup and correctness hardening.

- Recreate the Griotte and Cerisier merge diffs as normal commits. Do not retain their branch
  ancestry or any subject beginning with `Merge`.
- Before second-pass changes, require the curated tree to equal the accepted first-pass tree and
  pass the full suite. Commit this revised plan afterward.
- Move local `main` to the accepted linear branch only after verification. Keep the safety branch
  through final acceptance.
- Nothing is pushed. This leaves `c7f9009` independently pushable without exposing later commits.

## Architecture and Public Interfaces

### Backend layout

Use private wrapped implementation libraries in:

```text
lib/vanilla/
lib/locality_cerise/
lib/ucerise/
lib/mcerise/
lib/cerisier/
lib/griotte/
lib/griotte_extracted/
```

`cerise` remains a registry alias for vanilla and therefore has no implementation directory.

The public library provides sealed façade modules:

```text
Cerise.Vanilla
Cerise.Locality_cerise
Cerise.Ucerise
Cerise.Mcerise
Cerise.Cerisier
Cerise.Griotte
Cerise.Griotte_extracted
```

Each exposes its own `Ast`, `Asm_ir`, `Parser`, `Printer`, `Codec`, `Machine`, and
`Backend` modules as applicable. Private library module names must not escape installed
interfaces.

Remove flat names such as `Cerise.Vanilla_ast`; there are no compatibility aliases.
`Cerise.Machine` remains an alias for `Cerise.Vanilla.Machine`.

### AST and assembly IR

- Each backend's `Ast` contains only concrete machine syntax: registers, permissions,
  capabilities, words, instructions, and other concrete backend values.
- `Ast` must not contain expressions, labels, definitions, macro declarations, macro
  parameters/arguments, unresolved operands, or assembly placement nodes.
- Each backend's `Asm_ir` owns its exact unresolved instruction, word, register-file, and
  macro-argument forms.
- The common assembly layer owns only source locations, expressions, labels, definitions, macro
  containers, token infrastructure, hygienic expansion, and generic traversal machinery. It has no
  cross-backend instruction, capability, locality, permission, or word union.
- Rename `Machine_backend.S`'s parsed types to `asm_program`, `asm_regfile`, and `asm_word`.
  Parsing returns backend `Asm_ir`; resolution produces concrete `Ast` values before
  initialization or editing.

### Generated parsing

- Add `(using menhir 3.0)`, Menhir build dependencies, and any required `menhirLib` runtime
  dependency to Dune/Nix configuration.
- Define one shared token specification and one `lexer.mll`. A lexical token universe may include
  all backend keywords; tokenization alone does not imply acceptance.
- Define one shared partial `common_parser.mly` for expressions, labels, definitions, macros,
  calls, placement, regfiles, comments, and whitespace-insensitive construction syntax.
- Each backend supplies its own `parser.mly` fragment for exact instructions, values,
  permissions, localities, and macro-argument kinds. Dune feeds the shared fragment into each
  backend's Menhir `merge_into` build.
- Each generated parser has separate program, regfile, and word entry points and reports
  source-located diagnostics.
- Preserve every assembly form currently accepted by the correct backend. In particular, vanilla's
  grammar rejects a locality-bearing capability during parsing, even though the shared lexer can
  recognize locality tokens.
- Menhir conflicts and warnings are gate failures.

### Frozen backend semantics

The second pass changes organization and parsing, not machine behavior or encodings.

- Vanilla remains global-only with sealing and no locality type or field.
- Locality Cerise adds exactly `Global | Local`, `RWL`, `RWLX`, and `GetL`; it has no
  `Directed` or uninitialized capabilities.
- uCerise and mCerise retain exactly:

```text
Jmp Jnz Move Load Store Add Sub Lt Lea Restrict SubSeg IsPtr
GetP GetL GetB GetE GetA Fail Halt LoadU StoreU PromoteU
```

- The following instructions remain absent from both uCerise and mCerise:

```text
Mul Rem Div Invoke GetOType GetWType Seal UnSeal
```

- uCerise has `Global | Local`; mCerise additionally has `Directed`. Their duplicated ASTs and
  semantics remain intentionally independent.
- Cerisier remains a finite-`Z.t` historical implementation.
- Handwritten and extracted Griotte remain independent sibling backends; both reject `Rem` and
  `Div`, preserve their opcode gaps, and remain differentially equivalent.
- Existing automatic/fixed instruction-codec tables and golden encodings remain unchanged.

### Terminal application

- Create a non-public terminal application library entirely under `src/`, containing CLI
  options/parsing, application state, noninteractive output, event handling, and Notty rendering.
- Remove `lib/cli_options*`, the dead `lib/cli_parser.ml`, and `lib/application_model*`. The
  public Cerise library has no CLI/TUI dependency.
- Delete `website_fixture.ml/.mli`, its exports, and its tests. Add no replacement web abstraction.
- Restore the old rich Notty UI through backend-neutral `Machine_view` data:

  - styled, aligned multi-column registers;
  - capability/sealing/locality-aware word rendering;
  - primary heap/PC and secondary stack-or-selected-capability panels;
  - decoded instruction display;
  - bounds and cursor indicators;
  - styled machine status.

- Extend `Machine_view.word` with optional decoded-instruction text rather than introducing Notty
  or backend-specific callbacks into `lib`.
- Preserve old controls: quit/Escape, single step, ten steps, undo, row/page navigation, follow
  primary/secondary, secondary-panel toggle, resize, and mouse scrolling. Follow the
  `Stack_pointer` role when available; otherwise use the selected capability and retain capability
  cycling.
- Render responsively: two memory panels when width permits and a usable primary panel on narrow
  terminals.

## Sub-Agent Orchestration

### Operating rules

Use one root orchestrator and at most three simultaneous sub-agents. The root owns architecture,
dependency ordering, accepted main, integration, conflict resolution, and final acceptance.

Before every coding task, the root:

1. Records the accepted main SHA.
2. Creates a local branch `agent/<task-id>`.
3. Creates `/tmp/cerise-agent-<task-id>`.
4. Spawns the assigned model with `fork_turns: "none"`, its stated reasoning effort, and a
   self-contained task-specific brief.
5. Requires one atomic local implementation commit and a handoff containing the commit, changed
   subsystems, test results, and unresolved risks.

Every prompt states the exact objective, acceptance criteria, starting commit, worktree, branch,
owned subsystems, frozen interfaces, required checks, and forbidden actions. Agents do not push,
switch branches, edit other worktrees, perform unrelated cleanup, or spawn agents.

Coding tasks never overlap. The root reviews each diff, runs the task gate, cherry-picks the commit,
reruns the gate on main, and only then accepts the new SHA and starts its successor. No integration
uses a merge commit.

### T0 — Linear baseline and revised plan

- Root only; no coding delegation.
- Construct and validate the curated history above.
- Commit this revised plan as the second-pass accepted base.
- Gate: every curated commit builds; final tree matches the accepted first-pass tree; full tests
  pass; history contains no merge commits or `Merge` subjects.

### T1 — Backend directories and namespaces

- Model: `gpt-5.6-sol`; reasoning: `high`.
- Move every implementation into its backend directory and private library.
- Add sealed public façade modules and update registry/tests to the clean namespaced API.
- Do not change parsing, semantics, or encodings yet.
- Gate: full suite, `@install`, and an external-client compile using the new namespaces; old flat
  modules are unavailable.

### T2 — Common generated parser and active backends

- Model: `gpt-5.6-sol`; reasoning: `xhigh`.
- Add shared tokens, ocamllex lexer, modular Menhir grammar, common assembly shell, and renamed
  backend contract.
- Split vanilla and locality Cerise into concrete `Ast` and exact `Asm_ir`, then migrate both
  parsers.
- Gate: syntax-compatibility corpus, macro hygiene, labels/expressions, located failures, parser
  round trips, vanilla locality rejection, locality acceptance, no Menhir conflicts, and full suite.

### T3 — uCerise and mCerise parser/IR migration

- Model: `gpt-5.6-sol`; reasoning: `high`.
- Split both historical snapshots and migrate them to generated parsers without sharing their
  semantic ASTs.
- Gate: exact constructor and parser matrices, negative checks for all eight removed instructions,
  `IsPtr` and U-capability behavior, golden codecs, paper examples, and full suite.

### T4 — Cerisier parser/IR migration

- Model: `gpt-5.6-sol`; reasoning: `high`.
- Separate concrete enclave AST from assembly IR and migrate its parser while preserving finite
  bounds and fixed encoding.
- Gate: enclave lifecycle, parsing corpus, fixed codec tests, finite-bound behavior, examples, and
  full suite.

### T5 — Griotte parser/IR migration

- Model: `gpt-5.6-sol`; reasoning: `xhigh`.
- Give handwritten and extracted Griotte independent `Ast`, `Asm_ir`, and generated parsers in
  sibling directories.
- Keep extracted/generated sources and regeneration scripts within the extracted backend boundary.
- Gate: examples, rejection of `Rem`/`Div`, opcode gaps, regeneration twice with no diff,
  per-step differential equivalence, and full suite.

### T6 — CLI relocation and Notty restoration

- Model: `gpt-5.6-terra`; reasoning: `high`.
- Move all terminal application code into the private `src` library, delete website-fixture code,
  extend `Machine_view`, and restore the full Notty UX.
- Gate: CLI selection and errors, session isolation, stepping/undo/navigation event tests,
  fixed-size Notty render snapshots, narrow/wide layouts, every backend in interactive mode,
  absence of Notty/CLI/web dependencies from `lib`, and full suite.

### T7 — Cleanup and documentation

- Model: `gpt-5.6-luna`; reasoning: `medium`.
- Remove the handwritten `Assembly_frontend`, transitional adapters, stale flat files,
  compatibility names, and outdated documentation.
- Update assembler, architecture, CLI, backend, regeneration, and UI documentation.
- Gate: repository checks find no assembler-only types in backend `Ast` modules, no old parser
  path, no flat backend modules, no website fixture, no merge-language history requirements, and
  full Dune/Nix/regeneration checks.

### T8 — Final acceptance

After T7 is accepted, run two read-only agents concurrently:

- `gpt-5.6-sol`, `xhigh`: architecture, AST/Asm_ir boundaries, ISA, encoding, and parser
  correctness.
- `gpt-5.6-terra`, `high`: Dune/Nix/install, CLI, Notty snapshots, examples, regeneration, and
  history audit.

Material findings become new sequential fix tasks from the latest accepted SHA, followed by both
reviews again.

### Spawn template

Each coding invocation must resemble:

```text
spawn_agent(
  task_name = "<task-id>",
  fork_turns = "none",
  model = "<assigned-model>",
  reasoning_effort = "<assigned-effort>",
  message = """
  Repository: /home/june/Work/Cerise/cerise-interpreter
  Worktree: /tmp/cerise-agent-<task-id>
  Branch: agent/<task-id>
  Starting commit: <accepted-sha>

  Objective: <single bounded objective>
  Owned subsystem: <subsystems>
  Frozen interfaces: <interfaces>
  Required behavior: <acceptance criteria>
  Required checks: <commands/tests>

  Do not push, switch branches, edit other worktrees, perform unrelated cleanup, or spawn
  sub-agents. Commit the completed work atomically. Return the commit, changed subsystems, test
  results, and unresolved risks.
  """
)
```

The root replaces every placeholder and sends task-specific invariants, never the entire plan as
the task prompt.

### Failure policy

- A failed check remains owned by the task whose accepted commit introduced it.
- Cross-subsystem interface changes return to the root for approval.
- A replacement agent receives a fresh branch/worktree from the last accepted SHA and an explicit
  inventory of reusable committed work.
- Dirty or unaccepted worktrees are never reused by replacement agents.
- No integration proceeds while accepted main is failing.
- Nothing is pushed.

## Final Acceptance

- `c7f9009` is the exact first commit after the frozen `origin/main`.
- `git rev-list --min-parents=2 origin/main..main` is empty, and no new subject begins with
  `Merge`.
- Every backend has its required directory and clean `Cerise.<Backend>` namespace.
- All backend `Ast` modules are concrete machine ASTs; all unresolved assembly concepts live in
  `Asm_ir` or the common construction layer.
- Generated parsers preserve accepted syntax and reject backend-invalid shapes directly.
- CLI/TUI code exists only under `src`; website compatibility code is absent.
- The terminal UI uses composed Notty images and restores the old interactive experience.
- All unit, characterization, parser, codec, differential, CLI, render-snapshot, install, Nix,
  example, and regeneration gates pass.
- No commits are pushed.
