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
  "$repository_root/tests/api_gate/griotte_nominal_types_client.ml" \
  -o "$stage/griotte-nominal-types-client.cmo" >"$stage/griotte-nominal.log" 2>&1; then
  echo "handwritten and extracted Griotte AST identities are not nominally independent" >&2
  exit 1
fi
grep -q 'Griotte_extracted.Ast.instruction' "$stage/griotte-nominal.log"

if ocamlfind ocamlc -package cerise-interpreter -c \
  "$repository_root/tests/api_gate/griotte_nominal_asm_ir_client.ml" \
  -o "$stage/griotte-nominal-asm-ir-client.cmo" >"$stage/griotte-nominal-asm-ir.log" 2>&1; then
  echo "handwritten and extracted Griotte Asm_ir identities are not nominally independent" >&2
  exit 1
fi
grep -q 'Griotte_extracted.Asm_ir.program' "$stage/griotte-nominal-asm-ir.log"

contract_package=cerise-interpreter.griotte_"contract"
if OCAMLPATH="$stage/lib${OCAMLPATH:+:$OCAMLPATH}" \
  ocamlfind query "$contract_package" >"$stage/griotte-contract.log" 2>&1; then
  echo "temporary Griotte contract package is still installed" >&2
  exit 1
fi

if ocamlfind ocamlc -package cerise-interpreter -c \
  "$repository_root/tests/api_gate/old_flat_api_client.ml" \
  -o "$stage/old-flat-api-client.cmo" >"$stage/old-flat.log" 2>&1; then
  echo "legacy flat backend modules are still public" >&2
  exit 1
fi
grep -Eq 'Unbound module.*Vanilla_ast' "$stage/old-flat.log"

if ocamlfind ocamlc -package cerise-interpreter -c \
  "$repository_root/tests/api_gate/obsolete_state_view_client.ml" \
  -o "$stage/obsolete-state-view-client.cmo" >"$stage/obsolete-state-view.log" 2>&1; then
  echo "legacy State/View compatibility names are still public" >&2
  exit 1
fi
grep -Eq 'Unbound module|Unbound value|Unbound constructor' "$stage/obsolete-state-view.log"

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
