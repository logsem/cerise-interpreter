#!/usr/bin/env bash
set -euo pipefail

root=""
if [[ "${1:-}" == "--root" ]]; then
  root="${2:-}"
fi
if [[ -z "$root" ]]; then
  root="$(git rev-parse --show-toplevel)"
fi

fail() {
  echo "historical parser architecture gate: $*" >&2
  exit 1
}

for backend in ucerise mcerise; do
  directory="$root/lib/$backend"
  for source in ast.ml asm_ir.ml parser.mly parser_api.ml; do
    [[ -f "$directory/$source" ]] || fail "missing $backend/$source"
  done
  [[ ! -e "$directory/parser.ml" ]] || fail "$backend retains its handwritten parser"

  if rg -n '^type (expression|.*_term|macro_argument|parameter_kind|statement|program|regfile)\b' \
      "$directory/ast.ml"; then
    fail "$backend Ast contains assembler-only syntax"
  fi
  if rg -n '\b(Mul|Rem|Div|Invoke|GetOType|GetWType|Seal|UnSeal)(?:_term)?\b' \
      "$directory/ast.ml" "$directory/parser.mly"; then
    fail "$backend exposes a removed historical instruction"
  fi
  rg -q '^type expression\b' "$directory/asm_ir.ml" \
    || fail "$backend does not own unresolved assembly syntax"
  rg -q 'module Assembler = Assembly_construction.Make \(Syntax\)' "$directory/asm_ir.ml" \
    || fail "$backend does not use shared assembly construction"
  rg -q -- '--strict' "$directory/dune" || fail "$backend Menhir grammar is not strict"
  rg -q 'merge_into generated_parser' "$directory/dune" \
    || fail "$backend grammar is not merged with the shared grammar"
done

rg -q '^type locality = Global | Local$' "$root/lib/ucerise/ast.ml" \
  || fail "uCerise locality is not exactly Global | Local"
rg -q '^type locality = Global | Local | Directed$' "$root/lib/mcerise/ast.ml" \
  || fail "mCerise locality is not exactly Global | Local | Directed"

git_root="$(git -C "$root" rev-parse --show-toplevel 2>/dev/null || true)"
if [[ "$(realpath "$root")" == "$(realpath "${git_root:-/}")" ]]; then
  if find "$root/lib/ucerise" "$root/lib/mcerise" -maxdepth 1 \
      \( -name lexer.mll -o -name common_parser.mly -o -name token_spec.mly \) -print -quit \
      | grep -q .; then
    fail "a historical backend checks in a duplicate shared lexer or grammar"
  fi
fi

if rg -n 'Ucerise|Mcerise|ucerise|mcerise' "$root/lib/ucerise/ast.ml" "$root/lib/mcerise/ast.ml"; then
  fail "historical semantic Ast modules alias or mention one another"
fi

echo "Historical Ast/Asm_ir separation, ISA, locality, and shared generated parser layout verified"
