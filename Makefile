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
	./scripts/regenerate-griotte-extracted.sh

check-griotte-extracted:
	./scripts/check-griotte-extracted.sh --root "$(CURDIR)"

regeneration-gate:
	./scripts/regeneration-gate.sh --root "$(CURDIR)"

install:
	@test -s interpreter || ln -s ./_build/default/src/interpreter.exe interpreter
