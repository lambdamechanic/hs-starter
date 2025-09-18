#!/usr/bin/env bash
set -euo pipefail

echo "Starting migrations" >&2

MIGRATIONS_DIR="/opt/app/db/pgroll"

if [[ ! -d "$MIGRATIONS_DIR" ]]; then
  echo "error: migrations directory '$MIGRATIONS_DIR' not found" >&2
  exit 1
fi

echo "==> Listing pgroll migrations in $MIGRATIONS_DIR" >&2
ls -al "$MIGRATIONS_DIR"

echo "==> Running pgroll init" >&2
pgroll init \
  --postgres-url "$DATABASE_URL" \
  --schema public \
  --pgroll-schema pgroll

echo "==> Running pgroll migrate" >&2
pgroll migrate "$MIGRATIONS_DIR" \
  --postgres-url "$DATABASE_URL" \
  --schema public \
  --pgroll-schema pgroll \
  --complete
