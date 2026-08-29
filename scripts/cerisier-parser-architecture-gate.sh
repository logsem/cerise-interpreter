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
  echo "Cerisier parser architecture gate: $*" >&2
  exit 1
}

directory="$root/lib/cerisier"
for source in ast.ml asm_ir.ml parser.mly parser_api.ml; do
  [[ -f "$directory/$source" ]] || fail "missing cerisier/$source"
done
[[ ! -e "$directory/parser.ml" ]] || fail "Cerisier retains its handwritten parser"

if rg -n '^type (expression|.*_term|macro_argument|parameter_kind|statement|program|regfile)\b' "$directory/ast.ml"; then
  fail "Cerisier Ast contains assembler-only syntax"
fi

constructors=$(sed -n '/^type instruction =/,/^  | Halt$/p' "$directory/ast.ml" |
  sed -E 's/^  \| ([A-Za-z0-9_]+).*/\1/' | tail -n +2)
expected='Jmp
Jnz
Move
Load
Store
Add
Sub
Mul
Rem
Div
Lt
Lea
Restrict
SubSeg
GetL
GetB
GetE
GetA
GetP
GetOType
GetWType
Seal
UnSeal
Invoke
LoadU
StoreU
PromoteU
EInit
EDeInit
EStoreId
IsUnique
Fail
Halt'
[[ "$constructors" == "$expected" ]] || fail "concrete instruction constructors changed"

rg -Fq 'module Assembler = Assembly_construction.Make (Syntax)' "$directory/asm_ir.ml" ||
  fail "Cerisier does not use shared assembly construction"
rg -q -- '--strict' "$directory/dune" || fail "Cerisier Menhir grammar is not strict"
rg -q 'merge_into generated_parser' "$directory/dune" ||
  fail "Cerisier grammar is not merged with the shared grammar"

git_root="$(git -C "$root" rev-parse --show-toplevel 2>/dev/null || true)"
if [[ "$(realpath "$root")" == "$(realpath "${git_root:-/}")" ]]; then
  if find "$directory" -maxdepth 1 \( -name lexer.mll -o -name common_parser.mly -o -name token_spec.mly \) -print -quit | grep -q .; then
    fail "Cerisier checks in a duplicate shared lexer or grammar"
  fi
fi

echo "Cerisier concrete Ast, owned Asm_ir, exact ISA, and shared generated parser layout verified"
