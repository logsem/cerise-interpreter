#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repository_root=$(CDPATH= cd -- "$script_dir/../../../.." && pwd)
if [ "${1:-}" = "--root" ]; then repository_root=$2; shift 2; fi
if [ "$#" -ne 0 ]; then echo "Usage: $0 [--root REPOSITORY]" >&2; exit 2; fi

generated=$repository_root/lib/backends/griotte_extracted/generated
installer=$script_dir/regenerate.sh
temporary_root=$(mktemp -d)
trap 'rm -rf -- "$temporary_root"' EXIT HUP INT TERM
destination=$temporary_root/generated
mkdir -p "$destination"

"$installer" --install-from "$generated" --destination "$destination"
cmp "$generated/griotte_extracted.ml" "$destination/griotte_extracted.ml"
cmp "$generated/griotte_extracted.mli" "$destination/griotte_extracted.mli"
first=$(sha256sum "$destination/griotte_extracted.ml" "$destination/griotte_extracted.mli")
"$installer" --install-from "$generated" --destination "$destination"
second=$(sha256sum "$destination/griotte_extracted.ml" "$destination/griotte_extracted.mli")
test "$first" = "$second"
"$installer" --install-from "$generated" --destination "$destination" --check
"$script_dir/check.sh" --root "$repository_root"
echo "Extracted Griotte offline regeneration is byte-identical and idempotent"
