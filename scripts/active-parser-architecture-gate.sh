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
  echo "active parser architecture gate: $*" >&2
  exit 1
}

for backend in vanilla locality_cerise; do
  ast="$root/lib/$backend/ast.ml"
  ir="$root/lib/$backend/asm_ir.ml"
  grammar="$root/lib/$backend/parser.mly"
  [[ -f "$ast" && -f "$ir" && -f "$grammar" ]] || fail "missing active $backend source module"
  if rg -n '^type (expression|.*_term|macro_argument|parameter_kind|statement|program|regfile)\b' "$ast"; then
    fail "$backend Ast contains assembler-only syntax"
  fi
done

[[ -f "$root/lib/assembly/token_spec.mly" ]] || fail "missing shared token specification"
[[ -f "$root/lib/assembly/lexer.mll" ]] || fail "missing shared lexer"
[[ -f "$root/lib/assembly/common_parser.mly" ]] || fail "missing shared common grammar"

git_root="$(git -C "$root" rev-parse --show-toplevel 2>/dev/null || true)"
if [[ "$(realpath "$root")" == "$(realpath "${git_root:-/}")" ]]; then
  if find "$root/lib/vanilla" "$root/lib/locality_cerise" -maxdepth 1 \
      \( -name lexer.mll -o -name common_parser.mly -o -name token_spec.mly \) -print -quit \
      | grep -q .; then
    fail "an active backend checks in a duplicate shared lexer or grammar"
  fi
fi

if rg -n '\b(type|and) (permission|locality|sealable|word|instruction)\b' \
    "$root/lib/core/assembly_construction.ml" "$root/lib/core/assembly_construction.mli"; then
  fail "shared assembly construction contains backend semantic syntax"
fi

echo "Active Ast/Asm_ir separation and single-source generated parser layout verified"
