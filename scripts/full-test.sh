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

cd "$repository_root"
dune build @install
dune runtest --force
"$repository_root/scripts/public-api-gate.sh" --root "$repository_root" --skip-build
