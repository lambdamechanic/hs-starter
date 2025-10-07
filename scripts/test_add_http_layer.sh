#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT=$(git rev-parse --show-toplevel)
SCRIPT_DIR="$REPO_ROOT/scripts"
TEMPLATE_ROOT="$REPO_ROOT"
FIXTURE_DIR="$REPO_ROOT/fixtures/sample"
DIST_CACHE_DIR=${1:-}
WORK_DIR=$(mktemp -d)
cleanup() {
  if [[ -n $DIST_CACHE_DIR ]]; then
    mkdir -p "$DIST_CACHE_DIR"
    if [[ -d "$WORK_DIR/dist-newstyle" ]]; then
      if command -v rsync >/dev/null 2>&1; then
        rsync -a --delete "$WORK_DIR/dist-newstyle/" "$DIST_CACHE_DIR/"
      else
        rm -rf "$DIST_CACHE_DIR"/*
        cp -a "$WORK_DIR/dist-newstyle"/. "$DIST_CACHE_DIR"/
      fi
    fi
  fi
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT
PROJECT_NAME="sample-app"

# Keep cabal artefacts contained inside the temporary work tree so test runs
# avoid mutating the user's global store and package DB.
CABAL_HOME="$WORK_DIR/.cabal"
mkdir -p "$CABAL_HOME"
export CABAL_DIR="$CABAL_HOME"
export CABAL_CONFIG="$CABAL_HOME/config"
export XDG_CACHE_HOME="$WORK_DIR/.cache"

if [[ -n $DIST_CACHE_DIR && -d $DIST_CACHE_DIR ]]; then
  mkdir -p "$WORK_DIR/dist-newstyle"
  if command -v rsync >/dev/null 2>&1; then
    rsync -a "$DIST_CACHE_DIR/" "$WORK_DIR/dist-newstyle/"
  else
    rm -rf "$WORK_DIR/dist-newstyle"/*
    cp -a "$DIST_CACHE_DIR"/. "$WORK_DIR/dist-newstyle"/
  fi
fi

if [[ ! -f "$CABAL_CONFIG" ]]; then
  cabal user-config init --force >/dev/null
  cabal update >/dev/null
fi

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

cabal build all >/dev/null

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
