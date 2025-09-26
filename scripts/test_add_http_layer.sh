#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT=$(git rev-parse --show-toplevel)
SCRIPT_DIR="$REPO_ROOT/scripts"
TEMPLATE_ROOT="$REPO_ROOT"
FIXTURE_DIR="$REPO_ROOT/fixtures/sample"
WORK_DIR=$(mktemp -d)
trap 'rm -rf "$WORK_DIR"' EXIT

cp -R "$FIXTURE_DIR"/* "$WORK_DIR"

pushd "$WORK_DIR" >/dev/null

if command -v hpack >/dev/null 2>&1; then
  hpack --force >/dev/null
else
  echo "warning: hpack not available; skipping initial cabal generation" >&2
fi

cabal build >/dev/null

"$SCRIPT_DIR/add_as_http_layer.sh" Sample.Web "$WORK_DIR"

if command -v hpack >/dev/null 2>&1; then
  hpack --force >/dev/null
fi

cabal build >/dev/null

popd >/dev/null

echo "add_as_http_layer.sh sample test completed"
