.PHONY: all no-warning clean test install regenerate-griotte-extracted check-griotte-extracted regeneration-gate

all:
	dune build

no-warning:
	dune build --profile release

clean:
	dune clean

test:
	dune test

regenerate-griotte-extracted:
	./lib/griotte_extracted/scripts/regenerate.sh

check-griotte-extracted:
	./lib/griotte_extracted/scripts/check.sh --root "$(CURDIR)"

regeneration-gate:
	./scripts/regeneration-gate.sh --root "$(CURDIR)"

install:
	@test -s interpreter || ln -s ./_build/default/src/interpreter.exe interpreter
