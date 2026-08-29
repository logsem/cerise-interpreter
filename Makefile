.PHONY: all no-warning clean test install regenerate-extracted build-extracted

GRIOTTE_URL ?= https://github.com/logsem/griotte.git
GRIOTTE_BRANCH ?= interpreter-extraction
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
	@set -e; \
	tmp_dir="$$(mktemp -d)"; \
	trap 'rm -rf "$$tmp_dir"' EXIT HUP INT TERM; \
	git clone --depth 1 --branch "$(GRIOTTE_BRANCH)" "$(GRIOTTE_URL)" "$$tmp_dir"; \
	nix build --out-link "$$tmp_dir/nix-extraction" "$$tmp_dir#extraction"; \
	install -m 0644 "$$tmp_dir/nix-extraction/griotte_extracted.ml" "$(EXTRACTED_DIR)/griotte_extracted.ml"; \
	install -m 0644 "$$tmp_dir/nix-extraction/griotte_extracted.mli" "$(EXTRACTED_DIR)/griotte_extracted.mli"; \
	echo "Updated extracted OCaml files in $(EXTRACTED_DIR)"

build-extracted: regenerate-extracted
	dune build

install:
	@test -s interpreter || ln -s ./_build/default/src/interpreter.exe interpreter
