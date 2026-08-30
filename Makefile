.PHONY: all no-warning clean test format-production-check install regenerate-griotte-extracted check-griotte-extracted regeneration-gate

all:
	dune build

no-warning:
	dune build --profile release

clean:
	dune clean

test:
	dune build @install
	dune runtest --force

# Check handwritten production OCaml without promoting formatter output or touching extracted code.
format-production-check:
	@find lib src -type f \( -name '*.ml' -o -name '*.mli' \) \
		! -path 'lib/backends/griotte_extracted/generated/*' -print0 \
		| xargs -0 ocamlformat --check

regenerate-griotte-extracted:
	./lib/backends/griotte_extracted/scripts/regenerate.sh

check-griotte-extracted:
	./lib/backends/griotte_extracted/scripts/check.sh --root "$(CURDIR)"

regeneration-gate:
	./lib/backends/griotte_extracted/scripts/regeneration-gate.sh --root "$(CURDIR)"

install:
	@test -s interpreter || ln -s ./_build/default/src/interpreter.exe interpreter
