EXEC = default
EXTRACT_SRC = ../../cerise-compilation/extract.ml
EXTRACT_TARGET = ./lib/compiler/extract.ml

REG_FILE = ./asm-toys/$(EXEC).reg
S_FILE = ./asm-toys/$(EXEC).s

all: release

release:
	# dune build
	dune build --profile release

get-extract:
	cp $(EXTRACT_SRC) $(EXTRACT_TARGET)

no-warning:
	dune build --profile release

run:
	./compile
	./interpreter -I --version default --regfile $(REG_FILE) $(S_FILE)

compiler: get-extract no-warning run

clean:
	dune clean

test:
	# dune test
	dune test --profile release
install:
	@test -s interpreter || ln -s ./_build/default/src/interpreter.exe interpreter
	@test -s compile || ln -s ./_build/default/src/compile.exe compile
