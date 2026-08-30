#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 3 ]]; then
  echo "Usage: $0 ROOT INTERPRETER PROGRAM" >&2
  exit 2
fi

repository_root=$(cd "$1" && pwd)
export CERISE_PTY_EXE=$(cd "$(dirname "$2")" && pwd)/$(basename "$2")
export CERISE_PTY_PROGRAM=$(cd "$(dirname "$3")" && pwd)/$(basename "$3")
export TERM=xterm

typescript=$(mktemp)
trap 'rm -f -- "$typescript"' EXIT

script_bin=$(command -v script || true)
if [[ -z $script_bin && -x /usr/bin/script ]]; then
  script_bin=/usr/bin/script
fi
if [[ -z $script_bin ]]; then
  echo "The PTY smoke test requires util-linux script." >&2
  exit 127
fi

cd "$repository_root"
set +e
printf q | timeout 15s "$script_bin" --quiet --return --flush --command 'stty rows 12 cols 80; "$CERISE_PTY_EXE" -I "$CERISE_PTY_PROGRAM"' "$typescript" >/dev/null
status=$?
set -e
if [[ $status -ne 0 ]]; then
  echo "Interactive PTY process exited with status $status." >&2
  sed -n '1,80p' "$typescript" >&2
  exit "$status"
fi

grep -q 'HEAP' "$typescript"
grep -q 'machine state:' "$typescript"
