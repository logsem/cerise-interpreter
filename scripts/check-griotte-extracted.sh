#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repository_root=$(CDPATH= cd -- "$script_dir/.." && pwd)
if [ "${1:-}" = "--root" ]; then repository_root=$2; shift 2; fi
if [ "$#" -ne 0 ]; then echo "Usage: $0 [--root REPOSITORY]" >&2; exit 2; fi

generated=$repository_root/lib/internal/extracted
expected_ml=9fd26cc7c473a1e19421d2745c7edd799f58bb82a275f02c7d2f043e1550f0fa
expected_mli=e645b108d85d2bd99ab8d60be1beea352daf60fae22f44ef3c395b2853ac45ba
actual_ml=$(sha256sum "$generated/griotte_extracted.ml" | awk '{print $1}')
actual_mli=$(sha256sum "$generated/griotte_extracted.mli" | awk '{print $1}')
test "$actual_ml" = "$expected_ml"
test "$actual_mli" = "$expected_mli"
echo "Extracted Griotte provenance hashes verified"
