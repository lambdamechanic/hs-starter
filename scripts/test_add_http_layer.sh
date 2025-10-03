#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT=$(git rev-parse --show-toplevel)
SCRIPT_DIR="$REPO_ROOT/scripts"
TEMPLATE_ROOT="$REPO_ROOT"
FIXTURE_DIR="$REPO_ROOT/fixtures/sample"
WORK_DIR=$(mktemp -d)
PROJECT_NAME="sample-app"
trap 'rm -rf "$WORK_DIR"' EXIT

# Keep cabal artefacts contained inside the temporary work tree so test runs
# avoid mutating the user's global store and package DB.
CABAL_HOME="$WORK_DIR/.cabal"
mkdir -p "$CABAL_HOME"
export CABAL_DIR="$CABAL_HOME"
export CABAL_CONFIG="$CABAL_HOME/config"
export XDG_CACHE_HOME="$WORK_DIR/.cache"

if [[ ! -f "$CABAL_CONFIG" ]]; then
  cabal user-config init --force >/dev/null
  cabal update >/dev/null
fi

cp -R "$FIXTURE_DIR"/* "$WORK_DIR"

pushd "$WORK_DIR" >/dev/null

if command -v hpack >/dev/null 2>&1; then
  hpack --force >/dev/null
else
  echo "warning: hpack not available; skipping initial cabal generation" >&2
fi

cabal build all --builddir="$WORK_DIR/dist" >/dev/null

"$SCRIPT_DIR/add_as_http_layer.sh" Sample.Web "$PROJECT_NAME" "$WORK_DIR"

if command -v hpack >/dev/null 2>&1; then
  hpack --force >/dev/null
fi

cabal build all --builddir="$WORK_DIR/dist" >/dev/null

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
