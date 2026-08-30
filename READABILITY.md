# Production readability contract

This contract applies to handwritten production sources under `lib/` and `src/`. Tests,
fixtures, goldens, regeneration scripts, and the Rocq-generated Griotte snapshot are not part of
the readability rewrite. The goal is that a maintainer can extend a backend and audit the assembly
and codec paths from the repository itself, without needing unstated context or an AI assistant.

- Preserve public module paths, constructors, signatures, diagnostics, encodings, machine
  semantics, and rendered output.
- Give every named function an explicit parameter and return type. Keep callback and pattern
  annotations when they disambiguate records, functors, GADTs, or backend-specific types.
- Break long signatures across lines and introduce private aliases for repeated result,
  environment, or tuple shapes. Do not remove useful type information to satisfy formatting.
- Prefer names that describe domain roles (`instruction`, `diagnostics`, `register`) over names
  that describe syntax or position (`matched_value`, `x`, `sr`) when the role is not already
  obvious from a small local pattern.
- Start substantial modules with a responsibility and data-flow comment. Use section comments
  for parsing, macro expansion, symbol resolution, concrete assembly, execution, and rendering
  phases. Inline comments explain invariants and non-obvious failure branches, not assignments or
  constructor names.
- Document public signatures as contracts. For callbacks and abstraction boundaries, state who owns
  configuration and persistent state, what must remain stable or ordered, which inputs are already
  validated, and how stopped states, malformed values, and diagnostics are reported. A restatement
  of the OCaml type is not sufficient documentation.
- Put a docstring on a private function when its correctness depends on a non-obvious precondition,
  bit layout, phase boundary, ordering rule, or failure policy. Explain why the rule exists and what
  a future change must preserve; do not narrate pattern matches, assignments, or obvious wrappers.
- Keep long, cross-module explanations in [INTERNALS.md](INTERNALS.md) and practical extension
  checklists in [DEVELOPMENT.md](DEVELOPMENT.md). Local comments should still name the relevant
  invariant so they remain useful when read in an editor without the guide open.
- Treat canonical documentation as part of a behavioral change. Update the architecture, assembler
  reference, internals trace, or development checklist when an ownership boundary, syntax form,
  encoding rule, or UI invariant changes; historical plans are not sources of current truth.
- Keep backend semantics independent. New implementation modules remain private behind the
  existing `Assembly_construction`, backend `Asm_ir`, `Application_model`, and `Interactive_ui`
  facades. Similar backends may intentionally duplicate code when sharing would couple historical
  semantics; leave a short ownership comment at such a boundary rather than abstracting by shape
  alone.

Run `make format-production-check` to check formatting without changing files. The generated
Griotte directory is excluded both by that target and by `.ocamlformat-ignore`.
