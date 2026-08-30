.PHONY: all no-warning clean test install regenerate-griotte-extracted check-griotte-extracted regeneration-gate

all:
	dune build

no-warning:
	dune build --profile release

clean:
	dune clean

test:
	dune build @install
	dune runtest --force

regenerate-griotte-extracted:
	./lib/backends/griotte_extracted/scripts/regenerate.sh

check-griotte-extracted:
	./lib/backends/griotte_extracted/scripts/check.sh --root "$(CURDIR)"

regeneration-gate:
	./lib/backends/griotte_extracted/scripts/regeneration-gate.sh --root "$(CURDIR)"

install:
	@test -s interpreter || ln -s ./_build/default/src/interpreter.exe interpreter
