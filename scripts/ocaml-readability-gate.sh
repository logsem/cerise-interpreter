#!/bin/sh
set -eu

root=.
if [ "${1:-}" = "--root" ]; then root=${2:-.}; fi
python3 "$root/scripts/ocaml-readability-gate.py" "$root"
