all:
	dune build

no-warning:
	dune build --profile release

clean:
	dune clean

test:
	dune test

install:
	@test -s interpreter || ln -s ./_build/default/src/interpreter.exe interpreter
