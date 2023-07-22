EXEC = default
EXTRACT_SRC = ../../cerise-compilation/extract.ml
EXTRACT_TARGET = ./lib/extract.ml

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
ifeq (,$(wildcard $(REG_FILE)))
	$(error $(REG_FILE) does not exists.)
endif

ifeq (,$(wildcard $(S_FILE)))
	$(error $(S_FILE) does not exists.)
endif
	./compile
	./interactive --regfile $(REG_FILE) $(S_FILE)

compiler: get-extract no-warning run

clean:
	dune clean

test:
	# dune test
	dune test --profile release
