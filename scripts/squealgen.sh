#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

PG_IMAGE=${PG_IMAGE:-postgres:16-alpine}
CONTAINER_NAME=${CONTAINER_NAME:-hs-starter-squealgen}
DB_NAME=${DB_NAME:-hs_starter}
DB_USER=${DB_USER:-postgres}
DB_PASSWORD=${DB_PASSWORD:-postgres}
if [[ -n "${HOST_PORT:-}" ]]; then
  HOST_PORT_VALUE=$HOST_PORT
elif command -v python3 >/dev/null 2>&1; then
  HOST_PORT_VALUE=$(python3 - <<'PY'
import socket
with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
    s.bind(('', 0))
    print(s.getsockname()[1])
PY
  )
else
  HOST_PORT_VALUE=65432
fi

HOST_PORT=$HOST_PORT_VALUE
MIGRATIONS_DIR=${MIGRATIONS_DIR:-"$ROOT_DIR/db/pgroll"}
OUTPUT_PATH=${OUTPUT_PATH:-"$ROOT_DIR/src/Starter/Database/Generated.hs"}

cleanup() {
  docker rm -f "$CONTAINER_NAME" >/dev/null 2>&1 || true
}

trap cleanup EXIT

for bin in pgroll docker squealgen; do
  if ! command -v "$bin" >/dev/null 2>&1; then
    case "$bin" in
      squealgen)
        cat >&2 <<'MSG'
squealgen CLI not found. Install it from https://github.com/mwotton/squealgen (e.g. `make prefix=$HOME/.local install`).
MSG
        ;;
      *)
        echo "$bin binary not found on PATH" >&2
        ;;
    esac
    exit 1
  fi
done

cleanup

docker run \
  --rm \
  --detach \
  --name "$CONTAINER_NAME" \
  -e POSTGRES_DB="$DB_NAME" \
  -e POSTGRES_USER="$DB_USER" \
  -e POSTGRES_PASSWORD="$DB_PASSWORD" \
  -p "$HOST_PORT":5432 \
  "$PG_IMAGE" >/dev/null

echo "Waiting for PostgreSQL to accept connections..."
until docker exec "$CONTAINER_NAME" pg_isready -U "$DB_USER" >/dev/null 2>&1; do
  sleep 1
  printf '.'
 done
printf '\n'

PGROLL_SCHEMA=${PGROLL_SCHEMA:-public}
PGROLL_STATE_SCHEMA=${PGROLL_PGROLL_SCHEMA:-pgroll}
POSTGRES_URL="postgres://${DB_USER}:${DB_PASSWORD}@localhost:${HOST_PORT}/${DB_NAME}?sslmode=disable"

echo "Initializing pgroll state..."
pgroll init \
  --postgres-url "$POSTGRES_URL" \
  --schema "$PGROLL_SCHEMA" \
  --pgroll-schema "$PGROLL_STATE_SCHEMA" || true

echo "Applying migrations from $MIGRATIONS_DIR"
pgroll migrate "$MIGRATIONS_DIR" \
  --postgres-url "$POSTGRES_URL" \
  --schema "$PGROLL_SCHEMA" \
  --pgroll-schema "$PGROLL_STATE_SCHEMA" --complete

echo "Generating Squeal schema to $OUTPUT_PATH"

MODULE_NAME=${MODULE_NAME:-Starter.Database.Generated}
SCHEMA=$PGROLL_SCHEMA
IMPORTS=${SQUEALGEN_IMPORTS:-}

mkdir -p "$(dirname "$OUTPUT_PATH")"

PGHOST=localhost \
PGPORT=$HOST_PORT \
PGUSER=$DB_USER \
PGPASSWORD=$DB_PASSWORD \
  squealgen "$DB_NAME" "$MODULE_NAME" "$SCHEMA" "$IMPORTS" >"$OUTPUT_PATH.tmp"

mv "$OUTPUT_PATH.tmp" "$OUTPUT_PATH"

echo "Done"
