# Extracted Griotte provenance

`griotte_extracted.ml` and `griotte_extracted.mli` are generated Rocq output. Do not edit or format
them by hand. They were vendored byte-for-byte from the historical Griotte extraction workflow:

- repository: `https://github.com/logsem/griotte.git`
- branch: `interpreter-extraction`
- OCaml implementation SHA-256:
  `9fd26cc7c473a1e19421d2745c7edd799f58bb82a275f02c7d2f043e1550f0fa`
- OCaml interface SHA-256:
  `e645b108d85d2bd99ab8d60be1beea352daf60fae22f44ef3c395b2853ac45ba`

Run `make regenerate-griotte-extracted` to clone upstream into a fresh `mktemp -d` directory and
build the `extraction` Nix flake output. The script atomically installs both generated files and
always removes its temporary clone. A local Git source can be supplied with `--source` (or
`GRIOTTE_URL`) and a revision with `--branch` (or `GRIOTTE_BRANCH`).

`make regeneration-gate` uses the factored `--install-from` mode to exercise installation twice in
a temporary destination, prove byte identity/idempotence, and verify the provenance hashes without
network access. This is the normal CI gate; it intentionally does not rewrite the vendored source.

The generated code, its `Obj.magic`-based map runtime, and erased dependent invariants form a trust
boundary. `Griotte_extracted_backend` and `Griotte_extracted_codec` are explicitly untrusted adapter
code. They totalize public parsing/editing/execution boundaries and do not use `Instruction_codec`
or `Griotte_codec` for the extracted machine's semantic callbacks.
