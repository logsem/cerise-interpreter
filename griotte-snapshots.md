# Griotte sibling snapshots

`griotte` (handwritten) and `griotte-extracted` (Rocq-extracted with its adapter) are independent
sibling snapshots. They share the textual instruction list documented in
[assembler.md](assembler.md) and reject `Rem` and `Div`, but they retain independent parsers and
instruction codecs. Handwritten Griotte assigns compact opcodes in pattern declaration order;
extracted Griotte retains its fixed Rocq layout. Encoded instruction integers are therefore not a
shared interface. Semantic identity between the two snapshots is deliberately not a requirement.

Neither backend is universally authoritative.  Select the backend whose
snapshot is the one being studied, rather than treating one as a replacement
for the other.

## Differential checks

The per-step differential suite is a regression corpus for the explicitly
shared, covered subset.  It is not a proof of universal semantic equivalence
and is not a release gate for universal equivalence.  Shared-subset
differential checks remain useful and required: at the accepted revision they
cover arithmetic and control flow, memory and capability cases, and nested
parser behavior.  In particular, shifts and `SubSeg` agree on the semantics
covered by that corpus; they are not known intentional differences.

## Known intentional differences at the accepted revision

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
