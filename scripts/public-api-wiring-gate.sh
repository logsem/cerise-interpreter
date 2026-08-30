#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 [--root DIR]" >&2
  exit 2
}

repository_root=$(cd "$(dirname "$0")/.." && pwd)
if [[ $# -gt 0 ]]; then
  [[ $# -eq 2 && $1 == --root ]] || usage
  repository_root=$(cd "$2" && pwd)
fi

makefile="$repository_root/Makefile"
flake="$repository_root/flake.nix"
runner="$repository_root/scripts/full-test.sh"

grep -Fqx $'\t./scripts/full-test.sh --root "$(CURDIR)"' "$makefile"
grep -Fqx 'dune build @install' "$runner"
grep -Fqx 'dune runtest --force' "$runner"
grep -Fqx '"$repository_root/scripts/public-api-gate.sh" --root "$repository_root" --skip-build' "$runner"

grep -Fqx '                "Makefile"' "$flake"
grep -Fqx '                "flake.nix"' "$flake"
grep -Fqx "          checkPhase = ''" "$flake"
grep -Fqx '            make test' "$flake"
