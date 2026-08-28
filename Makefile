.PHONY: all no-warning clean test install regenerate-extracted build-extracted

GRIOTTE_DIR ?= $(CURDIR)/griotte
EXTRACTED_DIR := $(CURDIR)/lib/internal/extracted

all:
	dune build

no-warning:
	dune build --profile release

clean:
	dune clean

test:
	dune test

regenerate-extracted:
	$(MAKE) -C $(GRIOTTE_DIR) extract EXTRACT_DEST=$(EXTRACTED_DIR)

build-extracted: regenerate-extracted
	dune build

install:
	@test -s interpreter || ln -s ./_build/default/src/interpreter.exe interpreter
