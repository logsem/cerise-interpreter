.PHONY: all clean test format-check install regenerate-griotte-extracted

all:
	dune build

clean:
	dune clean

test:
	dune build @install
	dune runtest --force

format-check:
	dune build @fmt

regenerate-griotte-extracted:
	./lib/backends/griotte_extracted/scripts/regenerate.sh

install:
	@test -s interpreter || ln -s ./_build/default/src/interpreter.exe interpreter
