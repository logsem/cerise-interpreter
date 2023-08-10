all:
	dune build

no-warning:
	dune build --profile release

clean:
	dune clean

test:
	dune test

install:
	@test -s interactive || ln -s ./_build/default/src/interactive.exe interactive
