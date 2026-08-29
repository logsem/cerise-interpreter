#!/usr/bin/env sh
set -eu

root=
if [ "$1" = "--root" ]; then root=$2; fi
cd "$root"

test ! -e lib/cli_options.ml
test ! -e lib/cli_options.mli
test ! -e lib/cli_parser.ml
test ! -e lib/application_model.ml
test ! -e lib/application_model.mli
test ! -e lib/website_fixture.ml
test ! -e lib/website_fixture.mli
! rg -n 'Notty|Cli_options|Application_model|Website_fixture' lib --glob '*.ml' --glob '*.mli'
! rg -n 'Website_fixture' --glob '!BACKEND_REFACTORING_PLAN.md' --glob '!scripts/terminal-architecture-gate.sh' --glob '!_build/**' .
test -f src/cli_options.ml
test -f src/application_model.ml
test -f src/interactive_ui.ml
rg -q '^ *\(name cerise_terminal\)' src/dune
