#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repository_root=$(CDPATH= cd -- "$script_dir/.." && pwd)
if [ "${1:-}" = "--root" ]; then repository_root=$2; shift 2; fi
if [ "$#" -ne 0 ]; then echo "Usage: $0 [--root REPOSITORY]" >&2; exit 2; fi

handwritten=$repository_root/lib/griotte
extracted=$repository_root/lib/griotte_extracted

expected='Jalr Jmp Jnz ReadSR WriteSR Move Load Store Add Sub Mul LAnd LOr LShiftL LShiftR Lt Lea Restrict SubSeg GetL GetB GetE GetA GetP GetOType GetWType Seal UnSeal Fail Halt'

instruction_constructors() {
  sed -n '/^type instruction =/,/^let null_permission/p' "$1/ast.ml" \
    | sed -n 's/^  | \([A-Za-z][A-Za-z0-9_]*\).*/\1/p' \
    | tr '\n' ' ' \
    | sed 's/ $//'
}

for backend in "$handwritten" "$extracted"; do
  test "$(instruction_constructors "$backend")" = "$expected"
  for owned in ast.ml asm_ir.ml parser.mly parser_api.ml printer.ml codec.ml backend.ml dune; do
    test -f "$backend/$owned"
  done
  grep -q '^type instruction =' "$backend/ast.ml"
  grep -q '^type program = statement list' "$backend/asm_ir.ml"
  grep -q '(modules token_spec common_parser parser)' "$backend/dune"
  grep -q -- '--strict' "$backend/dune"
  grep -q 'mnemonic = REM' "$backend/parser.mly"
  grep -q 'mnemonic = DIV' "$backend/parser.mly"
  if grep -Eq '^type (expression|register_term|permission_term|seal_permission_term|locality_term|word_type_term|constant_term|operand_term|sealable_term|word_term|instruction_term|statement|program|regfile)' "$backend/ast.ml"; then
    echo "assembler-only declaration escaped into $backend/ast.ml" >&2
    exit 1
  fi
  if grep -Eq 'module (Ast|Asm_ir|Parser|Printer) *= *(Griotte|Cerise_griotte|Cerise_griotte_extracted)' "$backend"/*.ml; then
    echo "Griotte backend aliases another backend's syntax modules" >&2
    exit 1
  fi
done

git_root=$(git -C "$repository_root" rev-parse --show-toplevel 2>/dev/null || true)
if [ "$(realpath "$repository_root")" = "$(realpath "${git_root:-/}")" ]; then
  if find "$handwritten" "$extracted" -maxdepth 1 \
    \( -name token_spec.mly -o -name lexer.mll -o -name common_parser.mly \) -print | grep -q .; then
    echo "shared assembly lexer or common grammar was duplicated" >&2
    exit 1
  fi
fi

for shared in token_spec.mly lexer.mll common_parser.mly; do
  test -f "$repository_root/lib/assembly/$shared"
done

contract_fragment=griotte_"contract"
if grep -R -i "$contract_fragment" \
  "$repository_root/lib" "$repository_root/tests" "$repository_root/dune-project" 2>/dev/null; then
  echo "temporary Griotte contract still exists" >&2
  exit 1
fi

if grep -R -E 'cerise_griotte_private|Cerise_griotte_private|Griotte\.(Ast|Asm_ir|Parser|Printer)' \
  "$extracted" --exclude-dir=generated --exclude-dir='.*' --exclude='*.md'; then
  echo "extracted Griotte depends on handwritten private implementation" >&2
  exit 1
fi

test -d "$extracted/generated"
test -x "$extracted/scripts/regenerate.sh"
test -x "$extracted/scripts/check.sh"
test -x "$extracted/scripts/regeneration-gate.sh"
test ! -e "$repository_root/scripts/regenerate-griotte-extracted.sh"
test ! -e "$repository_root/scripts/check-griotte-extracted.sh"

echo "Independent Griotte Ast/Asm_ir/parser boundaries, exact ISA, and extracted ownership verified"
