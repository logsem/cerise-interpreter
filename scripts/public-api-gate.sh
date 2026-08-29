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

stage=$(mktemp -d)
trap 'rm -rf -- "$stage"' EXIT

cd "$repository_root"
dune build @install
dune install --prefix "$stage" cerise-interpreter

export OCAMLPATH="$stage/lib${OCAMLPATH:+:$OCAMLPATH}"
cp "$repository_root/tests/public_api_compile.ml" "$stage/public_api_compile.ml"
ocamlfind ocamlc -package cerise-interpreter -linkpkg \
  "$stage/public_api_compile.ml" -o "$stage/public-api-client"
"$stage/public-api-client"

if ocamlfind ocamlc -package cerise-interpreter -c \
  "$repository_root/tests/api_gate/old_flat_api_client.ml" \
  -o "$stage/old-flat-api-client.cmo" >"$stage/old-flat.log" 2>&1; then
  echo "legacy flat backend modules are still public" >&2
  exit 1
fi
grep -Eq 'Unbound module.*Vanilla_ast' "$stage/old-flat.log"

public_dir="$stage/lib/cerise-interpreter"
if find "$public_dir" -path "$public_dir/__private__" -prune -o \
  -type f -name '*_ast.cmi' -print | grep -q .; then
  echo "flat backend implementation interface escaped the private install area" >&2
  exit 1
fi

while IFS= read -r interface; do
  if ocamlobjinfo "$interface" | grep -Eq 'Cerise_.*_private|__private__'; then
    echo "private implementation identity escaped through $interface" >&2
    exit 1
  fi
done < <(find "$public_dir" -path "$public_dir/__private__" -prune -o -type f -name '*.cmi' -print)

echo "Installed namespaced API compiled; legacy flat API is absent; implementation artifacts are private"
