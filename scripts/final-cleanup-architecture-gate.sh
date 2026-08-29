#!/bin/sh
set -eu
root=.
if [ "${1:-}" = "--root" ]; then root=${2:-.}; fi
cd "$root"
fail() { echo "final cleanup architecture gate: $*" >&2; exit 1; }
frontend_name=Assembly_$(printf frontend)
frontend_file=assembly_$(printf frontend)
[ ! -e "lib/core/$frontend_file.ml" ] && [ ! -e "lib/$frontend_file.ml" ] || fail "obsolete handwritten frontend remains"
! rg -n "$frontend_name|Surface_ast|Website_fixture|Notty|Cli_options|Application_model" lib --glob '*.ml' --glob '*.mli' || fail "obsolete compatibility or UI code under lib"
for b in vanilla locality_cerise ucerise mcerise cerisier griotte griotte_extracted; do
  for f in ast.ml asm_ir.ml parser.mly parser_api.ml dune; do
    [ -f "lib/$b/$f" ] || fail "missing $b/$f"
  done
  if [ -d .git ] && git ls-files --error-unmatch "lib/$b/parser.ml" >/dev/null 2>&1; then fail "$b retains flat generated parser implementation"; fi
  ! rg -n '^type (expression|.*_term|macro_argument|parameter_kind|statement|program|regfile)\b' "lib/$b/ast.ml" || fail "$b Ast contains assembler-only syntax"
done
[ -f lib/core/assembly_construction.ml ] && [ -f lib/core/assembly_construction.mli ] || fail "missing shared construction"
[ -f lib/assembly/lexer.mll ] && [ -f lib/assembly/common_parser.mly ] && [ -f lib/assembly/token_spec.mly ] || fail "missing shared lexer/grammar"
[ -f lib/cerise.ml ] && [ -f lib/backend_registry.ml ] || fail "missing public façade"
if [ -d .git ] && git ls-files 'lib/*_ast.ml' 'lib/*_asm_ir.ml' 'lib/*_parser.ml' | grep -q .; then fail "flat backend implementation file remains"; fi
for b in vanilla locality_cerise ucerise mcerise cerisier griotte; do
  [ ! -e "lib/$b/state.ml" ] && [ ! -e "lib/$b/view.ml" ] || fail "non-extracted State/View file remains"
done
[ -f lib/griotte_extracted/generated/griotte_extracted.ml ] || fail "missing extracted generated implementation"
[ -f lib/griotte_extracted/generated/griotte_extracted.mli ] || fail "missing extracted generated interface"
! rg -n 'module (State|View) = Cerise_(vanilla|locality_cerise|ucerise|mcerise|griotte)_private' lib --glob '*.ml' || fail "obsolete State/View façade"
! rg -n 'pr\.txt|merge conflict|pull request history' --glob '!BACKEND_REFACTORING_PLAN.md' --glob '!_build/**' --glob '!scripts/final-cleanup-architecture-gate.sh' . || fail "stale history prose"
! rg -n 'Assembly_frontend|assembly_frontend' --glob '!BACKEND_REFACTORING_PLAN.md' --glob '!scripts/*' --glob '!_build/**' . || fail "obsolete frontend reference"
echo "Final cleanup architecture verified"
