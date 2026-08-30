# Griotte sibling snapshots

`griotte` (handwritten) and `griotte-extracted` (Rocq-extracted with its adapter) are independent
sibling snapshots. They share the textual instruction list documented in
[assembler.md](assembler.md) and reject `Rem` and `Div`, but they retain independent parsers and
instruction codecs. Handwritten Griotte assigns compact opcodes in pattern declaration order;
the extracted adapter mirrors the fixed layout expected by its Rocq machine parameters. Encoded
instruction integers are therefore not a shared interface. Semantic identity between the two
snapshots is deliberately not a requirement.

Neither backend is universally authoritative. Select the backend whose snapshot is being studied,
rather than treating one as a replacement or oracle for the other.

## Extracted-code trust boundary

Only `lib/backends/griotte_extracted/generated/` is generated Rocq output. Do not edit or format its
`.ml` or `.mli` files. The generated code includes an `Obj.magic`-based map runtime and depends on
erased invariants; those files and invariants are the extracted trust boundary.

Everything beside that directory in `lib/backends/griotte_extracted/` is handwritten adapter code,
including its stable `Ast`/`Asm_ir`, generated-parser composition, printer, fixed codec, state/view
snapshots, and `Machine_backend.S` adapter. It is not formally verified. The adapter explicitly
converts values in both directions, rejects values outside the generated finite domains, and
totalizes malformed decoding and execution failures so generated representation details do not
escape through `Machine_session` or `Machine_view`. It deliberately does not reuse handwritten
Griotte's codec or semantic callbacks.

`make check-griotte-extracted` checks the provenance hashes of the checked-in generated pair.
`make regeneration-gate` installs that pair twice into a temporary destination and checks byte
identity, idempotence, and provenance without rewriting the repository. Full upstream extraction is
the explicit, networked `make regenerate-griotte-extracted` operation; it clones and builds in a
temporary directory and atomically replaces both generated files. See the generated directory's
README for the pinned provenance details.

## Differential checks

The per-step differential suite is a regression corpus for the explicitly shared, covered subset.
It is not a proof of universal semantic equivalence and is not a release gate for universal
equivalence. Shared-subset differential checks remain useful and required: they cover arithmetic
and control flow, memory and capability cases, and nested parser behavior. In particular, shifts
and `SubSeg` agree on the semantics covered by that corpus; they are not known intentional
differences.

## Known intentional differences in the checked-in snapshots

- An absent, in-range sparse-memory load in handwritten Griotte yields integer
  zero.  A generated extracted `Load` instead requires an explicit finite-map
  binding.  The extracted backend-neutral view displays a missing address as
  default zero, but that presentation-level missing-cell value does not create
  a binding for generated execution.
- In the relevant handwritten paths, cursor, address, and object-type values
  use runtime Zarith/configuration bounds.  The extracted snapshot represents
  them with fixed `FinZ [0, 2_000_000)`.  This affects `Lea`, `Jmp` and taken
  `Jnz`, `Jalr` link updates, initialization and edit boundary acceptance, and
  configurations with `max_addr = 2_000_000` (the extracted upper bound is
  exclusive).
- `Jalr` whose destination is `PC` has a different update order.  Handwritten
  Griotte writes the destination link before installing the jump target;
  extracted Griotte installs the target before writing the destination link.
- Failed extracted steps retain the original machine snapshot transactionally.
  Some handwritten failures after a partial PC/destination update retain that
  partial mutation instead.

The generated implementation remains governed by its regeneration and provenance checks;
independence of these snapshots does not relax those checks or the shared textual ISA contract.
