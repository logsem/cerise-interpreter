# Backend-Owned AST and Parser Refactor

## Summary

Replace the provisional `Surface_ast` architecture with backend-owned ASTs, parsers, encodings,
and machines. Share only genuine assembly construction blocks: locations, expressions, labels,
definitions, typed macros, token infrastructure, and instruction-codec combinators.

There are seven canonical implementations and eight accepted registry names:

1. `vanilla` — default; global capabilities and sealing, with no locality type.
2. `cerise` — permanent listed alias of `vanilla`.
3. `locality-cerise` — vanilla plus `Global | Local`.
4. `ucerise` — historical paper machine.
5. `mcerise` — historical paper machine.
6. `cerisier` — historical enclave machine.
7. `griotte` — handwritten.
8. `griotte-extracted` — Rocq-extracted.

`sealing-cerise`, `vanilla-cerise`, `stack-cerise`, `custom`, and other legacy profile names are
rejected.

The accepted main baseline is `33f9fd228c8c0fc5d61f63e8b7b76d382375f64d`. T0 and T1 remain
accepted, but T1's `Surface_ast` is provisional and will be removed. The old unaccepted T2 commit
`27bf3ee` and its dirty worktree must not be integrated.

## Architecture and Interfaces

### Backend contract and shared frontend

`Machine_backend.S` keeps backend-owned `program`, `regfile`, `word`, and `state` types. Replace
`lower_program` and `lower_regfile` with:

```ocaml
val parse_program :
  ?filename:string ->
  string ->
  (program, Diagnostic.t list) result

val parse_regfile :
  ?filename:string ->
  string ->
  (regfile, Diagnostic.t list) result

val parse_word :
  ?filename:string ->
  string ->
  (word, Diagnostic.t list) result
```

`Machine_session` invokes the selected backend parser directly. There is no backend-independent
instruction, capability, permission, locality, or word AST, and no lowering phase.

Introduce `Assembly_frontend.Make (Syntax)`:

- The common layer owns tokens, source locations, arithmetic/runtime expressions, labels,
  definitions, typed macro declarations and calls, hygienic expansion, and raw-word placement.
- `Syntax` supplies backend-specific statement, raw-word, register-file, and macro-argument parsers.
- `Syntax` supplies expression traversal/substitution hooks so common label and macro passes can
  transform backend AST nodes without knowing their constructors.
- The common driver remains whitespace-compatible with existing assembly and does not make
  newlines semantically significant.
- Backend parsers construct their backend AST directly. Unsupported instructions or value shapes
  fail during parsing with source locations.
- Typed macros remain supported, but each backend declares the parameter kinds it accepts. A
  backend without locality or sealing has no corresponding macro parameter kind.

`Runtime_config.max_addr`, `Runtime_config.stack_addr`, `Machine_view.address_limit`, and capability
bounds remain finite `Z.t`. Cerisier's `Infinite_z` is removed; `inf` is not accepted syntax.

### Active backends

`vanilla` has no locality type, locality AST field, locality parser production, locality encoding,
or locality text in editable/displayed words.

Its capability syntax contains four fields:

```text
(permission, base, limit, cursor)
```

Seal ranges likewise omit locality. A five-field capability such as `(RW, GLOBAL, 0, 10, 0)` is a
parse error.

Its exact instruction set, in codec-table order, is:

```text
Jmp Jnz Move Load Store Add Sub Mul Rem Div Lt Lea Restrict SubSeg
GetB GetE GetA GetP GetOType GetWType Seal UnSeal Invoke Fail Halt
```

Its permissions are:

```text
O E RO RX RW RWX
```

`locality-cerise` has its own AST and parser. It supports sealing and the same instructions as
vanilla, with `GetL` inserted after `SubSeg`. Its localities are exactly `Global | Local`, and it
additionally supports `RWL | RWLX`. It has no `Directed`, uninitialized permissions, `LoadU`,
`StoreU`, or `PromoteU`.

The two active implementations may share mechanical helpers and equivalence tests, but not a
capability/instruction superset AST.

### Historical uCerise and mCerise

`ucerise` implements [Efficient and Provable Local Capability Revocation using Uninitialized
Capabilities](https://doi.org/10.1145/3434287). `mcerise` implements [Le Temps des Cerises:
Efficient Temporal Stack Safety on Capability Machines using Directed
Capabilities](https://doi.org/10.1145/3527318).

Both have exactly this instruction set, in codec-table order:

```text
Jmp Jnz Move Load Store Add Sub Lt Lea Restrict SubSeg IsPtr
GetP GetL GetB GetE GetA Fail Halt LoadU StoreU PromoteU
```

Relative to the current shared AST, remove exactly these eight instructions from both backend ASTs,
parsers, encoders, decoders, printers, and evaluators:

```text
Mul
Rem
Div
Invoke
GetOType
GetWType
Seal
UnSeal
```

Add `IsPtr`, which is present in both paper machines but absent from the current shared AST.

Both backends retain `GetL`, `LoadU`, `StoreU`, and `PromoteU`. Their words are only integers or
capabilities. Remove sealing permissions, seal ranges, sealed words, and sealing word-type constants.

`ucerise` has only `Global | Local`. `mcerise` additionally has `Directed`. Their AST and machine
implementations remain independent historical snapshots; semantic duplication is intentional.
Shared plumbing and codec combinators may evolve, but their instruction/value shapes may not grow
with active backends.

### Instruction codec

Add a shared internal `Instruction_codec` combinator library. A backend declares a table of cases
containing:

- Constructor and projector.
- Operand shape.
- `Auto` or fixed opcode allocation.
- Backend-specific register, scalar, permission, and system-register codecs.

Compiling the table creates symmetric `encode` and `decode` functions. Shape combinators cover
registers, constants, register-or-constant operands, tuples, signed Zarith values, and backend enums.

The compiler must:

- Allocate contiguous opcode spans automatically; each register-or-constant operand doubles the
  required span.
- Preserve the existing low-eight-bit opcode/high-bit payload convention where that convention
  applies.
- Reuse the existing signed integer-pair interleaving behavior.
- Reject duplicate or overlapping opcode ranges and ranges exceeding the opcode field.
- Return structured errors for malformed or unknown encodings.

`vanilla`, `locality-cerise`, `ucerise`, and `mcerise` use automatic sequential allocation starting
at opcode zero in the instruction orders listed above. Their numeric instruction encodings may
change and are frozen by new golden tests.

Cerisier uses fixed descriptors preserving its historical encoding. Handwritten Griotte uses fixed
descriptors equivalent to extracted Griotte. The extracted implementation remains independent of
this codec.

### Registry, views, and compatibility

`Backend_registry.names ()` returns, in deterministic order:

```text
vanilla
cerise
locality-cerise
ucerise
mcerise
cerisier
griotte
griotte-extracted
```

`find "cerise"` returns the vanilla implementation. `Machine_session` stores the selected registry
name separately from the canonical backend module name, so both `Machine_session.backend_name` and
`Machine_view.backend_name` report `cerise` when that alias was selected.

`Cerise.Machine` aliases vanilla. Vanilla reports `None` in the backend-neutral optional locality
view field and never includes locality in editable text.

Machine states and sessions remain immutable. Snapshot support remains excluded. The website
repository is not modified; only the interpreter-side session/view compatibility fixture is
maintained.

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

The root reviews the task diff and runs its gate before integration, integrates it, reruns the gate
on main, and only then accepts the SHA and starts the successor. Ordinary task commits are
cherry-picked. Historical integration branches are merged with `--no-ff`. No coding tasks overlap.

### Dependency-ordered task graph

Before the first new coding task, the root commits this revised plan on accepted main. That
documentation commit becomes the accepted base for T2-revised.

#### T2-revised — Frontend, contracts, and codec

- Model: `gpt-5.6-sol`; reasoning: `xhigh`.
- Depends on accepted T1 plus the revised-plan commit.
- Remove `Surface_ast` and all lowering APIs.
- Add the parameterized common frontend and instruction codec.
- Revise session alias tracking while retaining `Machine_view`, diagnostics, and finite runtime
  configuration.
- Convert the interim adapter only as needed to prove the new existential session contract.
- Gate: frontend macro/label/expression tests, two deliberately different fixture syntaxes, codec
  collision/round-trip tests, session isolation tests, and existing characterization tests all pass.

This remains an architectural serialization point.

#### T3 — Vanilla and locality-cerise

- Model: `gpt-5.6-sol`; reasoning: `high`.
- Implement separate ASTs, parsers, printers, codec tables, and machine adapters for both active
  backends.
- Merge sealing into vanilla and remove locality completely from vanilla syntax and types.
- Add the permanent `cerise` alias and change `Cerise.Machine` to vanilla.
- Gate: exact parser acceptance matrices, global-only cross-backend execution equivalence, alias
  identity tests, golden encodings, and rejection of locality/U/Directed constructs.

#### T4 — uCerise and mCerise snapshots

- Model: `gpt-5.6-sol`; reasoning: `high`.
- Implement independent historical AST/parser/machine modules using current behavior as the semantic
  baseline and the two papers as the ISA authority.
- Apply the exact eight-instruction removal list and add `IsPtr`.
- Remove all sealing value forms from both backends.
- Use automatically repacked codec tables.
- Gate: exact constructor/parser/codec matrices, paper examples, locality and
  uninitialized-capability transitions, negative tests for every removed instruction, and repository
  checks showing the removed constructors do not exist in either backend.

Baseline T0 profile tests may be retired only after these replacement tests pass.

#### T5 — CLI, terminal UI, and website fixture

- Model: `gpt-5.6-terra`; reasoning: `high`.
- Replace version/feature flags with `--backend`; omitted selection uses `vanilla`.
- Ensure CLI and UI share one immutable session.
- Adapt the website compatibility fixture exclusively to `Machine_session` and `Machine_view`.
- Gate: multiple backend selections, alias reporting, unknown-name diagnostics, editing, stepping,
  undo retention, capability selection, and sparse-memory navigation.

#### T6 — Handwritten Griotte integration

- Model: `gpt-5.6-sol`; reasoning: `xhigh`.
- Root creates `agent/t6-griotte` from the local `griotte` branch and records both its historical tip
  and the accepted main SHA.
- Agent merges accepted main without committing separately, resolves the port into backend-owned
  Griotte syntax, and creates one atomic merge commit.
- Handwritten Griotte uses the common frontend and codec while preserving the extracted encoding.
- Gate: Griotte examples, parser/codec tests, and the full accepted suite.
- Root merges the task branch with `--no-ff`; it is never cherry-picked.

#### T7 — Extracted Griotte

- Model: `gpt-5.6-sol`; reasoning: `high`.
- Add the extracted adapter, temporary-clone/Nix regeneration workflow, and state-by-state
  differential tests.
- Both Griotte implementations share the Griotte-specific source syntax, not a cross-backend AST.
- Gate: reproducible regeneration, clean second regeneration, identical instruction encodings, and
  observable-state equivalence after every step.

#### T8 — Cerisier integration

- Model: `gpt-5.6-sol`; reasoning: `high`.
- Root creates `agent/t8-cerisier` from local `june/cerisier` and records both historical and
  accepted-main SHAs.
- Merge accepted main without a preliminary commit and port Cerisier as a complete independent
  historical backend, not a vanilla extension.
- Preserve its AST, enclave instructions, transitions, and fixed encoding.
- Replace every `Infinite_z` use with finite `Z.t`; initialize bounds from
  `Runtime_config.max_addr`.
- Produce one atomic merge commit.
- Gate: enclave lifecycle, ID storage, uniqueness, finite-bound behavior, examples, fixed encoding
  tests, and full suite.
- Root integrates with `--no-ff`; it is never cherry-picked.

#### T9 — Cleanup and documentation

- Model: `gpt-5.6-luna`; reasoning: `medium`.
- Remove the obsolete monolithic AST/machine profiles, mutable flags, interim adapter, old parser
  paths, old names, snapshot references, and `Infinite_z`.
- Update examples and documentation.
- Gate: no semantic changes, no public-interface redesign, and repository searches find no
  operational dependency on removed infrastructure.

#### T10 — Final independent acceptance

Run two read-only agents concurrently in separate worktrees:

- `gpt-5.6-sol`, `xhigh`: architecture and semantic correctness review.
- `gpt-5.6-terra`, `medium`: complete Dune, CLI, UI, Nix, regeneration, example, and
  differential-test audit.

Material findings become new sequential, narrowly scoped coding tasks based on the latest accepted
SHA. Repeat both reviews after fixes.

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

The root replaces all placeholders and sends task-specific invariants, never the entire plan as the
task prompt.

### Failure policy

- A failed check remains owned by the task whose accepted commit introduced it.
- Cross-subsystem interface changes return to the root for approval.
- A replacement agent receives a fresh branch/worktree from the last accepted SHA and an explicit
  inventory of reusable committed work.
- Dirty or unaccepted worktrees are never reused by replacement agents.
- No integration proceeds while accepted main is failing.
- Nothing is pushed.

## Acceptance Criteria and Assumptions

- Backend parsers reject unsupported shapes directly; there is no superset semantic AST or delayed
  feature lowering.
- Vanilla cannot represent, parse, print, or encode locality.
- Locality-cerise supports only Global and Local; uCerise supports Global and Local; mCerise supports
  Global, Local, and Directed.
- The eight instructions listed above are absent from both uCerise and mCerise, while `IsPtr` is
  present.
- Codec tables have no collisions and satisfy decode/encode round trips over every operand-shape
  variant.
- All accepted backend names resolve; old profile names fail with the deterministic valid-name list.
- Interleaved sessions demonstrate absence of global backend/configuration state.
- Cerisier intentionally uses finite Zarith bounds instead of its branch's infinite bound.
- Griotte ancestry and Cerisier ancestry are preserved through merge commits.
- Snapshotting is removed without replacement; website adaptation occurs after the interpreter API
  stabilizes.
- No work is pushed.
