#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT=$(git rev-parse --show-toplevel)
SCRIPT_DIR="$REPO_ROOT/scripts"
TEMPLATE_ROOT="$REPO_ROOT"
FIXTURE_DIR="$REPO_ROOT/fixtures/sample"
if [[ -n ${WORKDIR:-} ]]; then
  WORK_DIR="$WORKDIR"
  mkdir -p "$WORK_DIR"
  CLEAN_WORKDIR=0
  find "$WORK_DIR" -mindepth 1 -maxdepth 1 -exec rm -rf {} +
else
  WORK_DIR=$(mktemp -d)
  CLEAN_WORKDIR=1
  trap 'rm -rf "$WORK_DIR"' EXIT
fi
PROJECT_NAME="sample-app"

# Use the shared Cabal store/config provided by CI for faster rebuilds.
unset CABAL_DIR
unset CABAL_CONFIG
unset XDG_CACHE_HOME

cp -R "$FIXTURE_DIR"/* "$WORK_DIR"

cat <<'EOF' > "$WORK_DIR/cabal.project"
packages: .
EOF

ROOT_CABAL="$REPO_ROOT/cabal.project"
DEST_CABAL="$WORK_DIR/cabal.project.local"
export ROOT_CABAL DEST_CABAL
python3 <<'PYGEN'
import os
from pathlib import Path

root = Path(os.environ['ROOT_CABAL'])
dest = Path(os.environ['DEST_CABAL'])
lines = root.read_text().splitlines()
allow_newer = [line.strip() for line in lines if line.strip().startswith('allow-newer')]
blocks = []
current = []
collect = False
for line in lines:
    if line.startswith('source-repository-package'):
        if current:
            blocks.append(current)
        current = [line]
        collect = True
    elif collect:
        if line.strip() == '':
            current.append(line)
            blocks.append(current)
            current = []
            collect = False
        else:
            current.append(line)
if current:
    blocks.append(current)
with dest.open('w', encoding='utf-8') as f:
    f.write('tests: False\n')
    for line in allow_newer:
        f.write(f"{line}\n")
    for block in blocks:
        f.write("\n".join(block).rstrip())
        f.write("\n\n")
PYGEN

pushd "$WORK_DIR" >/dev/null

if command -v hpack >/dev/null 2>&1; then
  hpack --force >/dev/null
else
  echo "warning: hpack not available; skipping initial cabal generation" >&2
fi

"$SCRIPT_DIR/add_as_http_layer.sh" Sample.Web "$PROJECT_NAME" "$WORK_DIR"

if command -v hpack >/dev/null 2>&1; then
  hpack --force >/dev/null
fi

cabal build all >/dev/null

if [[ -n ${WORKDIR:-} ]]; then
  echo "cached dist path: $WORK_DIR/dist-newstyle" >&2
fi

if rg -iw 'starter' \
    --glob '!**/dist/**' \
    --glob '!**/dist-*/**' \
    --glob '!.git' \
    "$WORK_DIR" >/dev/null; then
  echo "error: generated project still contains references to 'starter'" >&2
  rg -iw 'starter' --glob '!**/dist/**' --glob '!**/dist-*/**' --glob '!.git' "$WORK_DIR"
  exit 1
fi

popd >/dev/null

echo "add_as_http_layer.sh sample test completed"
