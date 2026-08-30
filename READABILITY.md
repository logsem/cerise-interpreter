# Production readability contract

This contract applies to handwritten production sources under `lib/` and `src/`. Tests,
fixtures, goldens, regeneration scripts, and the Rocq-generated Griotte snapshot are not part of
the readability rewrite.

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
  for parsing, macro processing, lowering, execution, and rendering phases. Inline comments
  explain invariants and non-obvious failure branches, not assignments or constructor names.
- Keep backend semantics independent. New implementation modules remain private behind the
  existing `Assembly_construction`, backend `Asm_ir`, `Application_model`, and `Interactive_ui`
  facades.

Run `make format-production-check` to check formatting without changing files. The generated
Griotte directory is excluded both by that target and by `.ocamlformat-ignore`.
