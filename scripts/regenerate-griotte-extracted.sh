#!/bin/sh
set -eu

usage() {
  cat <<'EOF'
Usage: scripts/regenerate-griotte-extracted.sh [options]

Clone and build options:
  --source REPOSITORY   Git URL or local Git repository
  --branch REVISION     Branch/revision to clone

Factored/offline options:
  --install-from DIR    Install already-built griotte_extracted.ml/.mli
  --check               Compare output to destination without installing

Common options:
  --destination DIR     Destination directory for generated files
  --help                Show this help

Defaults may also be set with GRIOTTE_URL and GRIOTTE_BRANCH. The normal mode
always clones into mktemp -d, builds source#extraction with Nix, and removes the
temporary clone. --install-from exists for deterministic offline/CI gating.
EOF
}

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repository_root=$(CDPATH= cd -- "$script_dir/.." && pwd)
source_repository=${GRIOTTE_URL:-https://github.com/logsem/griotte.git}
source_branch=${GRIOTTE_BRANCH:-interpreter-extraction}
destination=$repository_root/lib/internal/extracted
install_from=
check_only=false
temporary_root=
stage_ml=
stage_mli=

cleanup() {
  if [ -n "$stage_ml" ]; then rm -f -- "$stage_ml"; fi
  if [ -n "$stage_mli" ]; then rm -f -- "$stage_mli"; fi
  if [ -n "$temporary_root" ]; then rm -rf -- "$temporary_root"; fi
}
trap cleanup EXIT HUP INT TERM

while [ "$#" -gt 0 ]; do
  case "$1" in
    --source) source_repository=$2; shift 2 ;;
    --branch) source_branch=$2; shift 2 ;;
    --destination) destination=$2; shift 2 ;;
    --install-from) install_from=$2; shift 2 ;;
    --check) check_only=true; shift ;;
    --help|-h) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
done

if [ -z "$install_from" ]; then
  temporary_root=$(mktemp -d)
  git clone --depth 1 --branch "$source_branch" "$source_repository" "$temporary_root/source"
  nix build --out-link "$temporary_root/result" "$temporary_root/source#extraction"
  install_from=$temporary_root/result
fi

source_ml=$install_from/griotte_extracted.ml
source_mli=$install_from/griotte_extracted.mli
for generated in "$source_ml" "$source_mli"; do
  if [ ! -s "$generated" ]; then
    echo "Missing generated output: $generated" >&2
    exit 1
  fi
done

if [ "$check_only" = true ]; then
  cmp -- "$source_ml" "$destination/griotte_extracted.ml"
  cmp -- "$source_mli" "$destination/griotte_extracted.mli"
  echo "Extracted Griotte output is byte-identical to $destination"
  exit 0
fi

mkdir -p -- "$destination"
stage_ml=$(mktemp "$destination/.griotte_extracted.ml.XXXXXX")
stage_mli=$(mktemp "$destination/.griotte_extracted.mli.XXXXXX")
install -m 0644 -- "$source_ml" "$stage_ml"
install -m 0644 -- "$source_mli" "$stage_mli"
mv -f -- "$stage_ml" "$destination/griotte_extracted.ml"
stage_ml=
mv -f -- "$stage_mli" "$destination/griotte_extracted.mli"
stage_mli=
echo "Installed extracted Griotte output atomically in $destination"
