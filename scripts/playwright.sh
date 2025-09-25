#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd "$SCRIPT_DIR/.." && pwd)
PLAYWRIGHT_DIR="$REPO_ROOT/test/playwright"
IMAGE=${PLAYWRIGHT_IMAGE:-mcr.microsoft.com/playwright:v1.44.1-jammy}
DOKKU_CLI=${DOKKU_CLI:-"$HOME/.dokku/contrib/dokku_client.sh"}
DOKKU_APP=${DOKKU_APP:-hs-starter}
POSTGRES_IMAGE=${PLAYWRIGHT_POSTGRES_IMAGE:-postgres:16-alpine}
PGROLL_BIN=${PGROLL_BIN:-pgroll}
MIGRATIONS_DIR="$REPO_ROOT/db/pgroll"

if [[ -n "${PLAYWRIGHT_DOCKER_NETWORK_ARGS:-}" ]]; then
  # shellcheck disable=SC2206
  NETWORK_ARGS=($PLAYWRIGHT_DOCKER_NETWORK_ARGS)
else
  NETWORK_ARGS=(--network=host --ipc=host)
fi

cleanup() {
  local exit_code=$?
  if [[ -n "${SERVER_PID:-}" ]]; then
    if kill -0 "$SERVER_PID" >/dev/null 2>&1; then
      kill "$SERVER_PID" >/dev/null 2>&1 || true
      wait "$SERVER_PID" 2>/dev/null || true
    fi
  fi
  if [[ -n "${DB_CONTAINER:-}" ]]; then
    docker rm -f "$DB_CONTAINER" >/dev/null 2>&1 || true
  fi
  if [[ -n "${SERVER_LOG:-}" && -f "$SERVER_LOG" ]]; then
    if [[ ${PLAYWRIGHT_KEEP_SERVER_LOG:-0} -eq 0 ]]; then
      rm -f "$SERVER_LOG"
    else
      echo "server log retained at $SERVER_LOG" >&2
    fi
  fi
  exit $exit_code
}
trap cleanup EXIT INT TERM

if ! command -v docker >/dev/null 2>&1; then
  echo "error: docker is required to run Playwright tests" >&2
  exit 1
fi

if ! command -v "$PGROLL_BIN" >/dev/null 2>&1; then
  echo "error: pgroll is required to apply database migrations (expected binary '$PGROLL_BIN')" >&2
  exit 1
fi

if ! command -v cabal >/dev/null 2>&1; then
  echo "error: cabal is required to launch the hs-starter server" >&2
  exit 1
fi

if [[ ! -d "$PLAYWRIGHT_DIR" ]]; then
  echo "error: Playwright directory '$PLAYWRIGHT_DIR' not found" >&2
  exit 1
fi

if [[ ! -d "$MIGRATIONS_DIR" ]]; then
  echo "error: migrations directory '$MIGRATIONS_DIR' not found" >&2
  exit 1
fi

choose_port() {
  local python_bin=${PYTHON_BIN:-}
  if [[ -z "$python_bin" ]]; then
    if command -v python3 >/dev/null 2>&1; then
      python_bin=python3
    elif command -v python >/dev/null 2>&1; then
      python_bin=python
    else
      echo "error: python3 or python is required to select a free port" >&2
      return 1
    fi
  fi
  "$python_bin" -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1])'
}

populate_env_from_dokku() {
  if [[ -n "${HS_STARTER_SKIP_DOKKU_CONFIG:-}" ]]; then
    return
  fi
  if [[ ! -f "$DOKKU_CLI" ]]; then
    return
  fi
  local output
  if ! output=$(bash "$DOKKU_CLI" config "$DOKKU_APP" 2>/dev/null); then
    return
  fi
  while IFS= read -r line; do
    if [[ $line =~ ^([A-Z0-9_]+):[[:space:]]*(.*)$ ]]; then
      local key="${BASH_REMATCH[1]}"
      local value="${BASH_REMATCH[2]}"
      case "$key" in
        FIREBASE_API_KEY|FIREBASE_PROJECT_ID|FIREBASE_AUTH_DOMAIN|SESSION_SECRET)
          if [[ -z "${!key:-}" ]]; then
            export "$key"="$value"
          fi
          ;;
      esac
    fi
  done <<<"$output"
}

populate_env_from_dokku

required_env=(FIREBASE_API_KEY FIREBASE_PROJECT_ID SESSION_SECRET)
for var in "${required_env[@]}"; do
  if [[ -z "${!var:-}" ]]; then
    echo "error: required environment variable '$var' is not set" >&2
    exit 1
  fi
done

APP_PORT=$(choose_port)
DB_PORT=$(choose_port)

if [[ -z "$APP_PORT" || -z "$DB_PORT" ]]; then
  echo "error: failed to allocate local ports" >&2
  exit 1
fi

HS_STARTER_BASE_URL=${HS_STARTER_BASE_URL:-http://localhost:$APP_PORT}
export HS_STARTER_BASE_URL

DATABASE_URL="postgres://postgres:postgres@127.0.0.1:$DB_PORT/hs_starter?sslmode=disable"

DB_CONTAINER="hs-starter-playwright-db-$$"
echo "Starting Postgres container '$DB_CONTAINER' on port $DB_PORT" >&2

docker run --rm -d \
  --name "$DB_CONTAINER" \
  -e POSTGRES_PASSWORD=postgres \
  -e POSTGRES_USER=postgres \
  -e POSTGRES_DB=hs_starter \
  -p "$DB_PORT:5432" \
  "$POSTGRES_IMAGE" >/dev/null

wait_for_postgres() {
  for _ in {1..40}; do
    if docker exec "$DB_CONTAINER" pg_isready -U postgres -d hs_starter >/dev/null 2>&1; then
      return 0
    fi
    sleep 0.5
  done
  return 1
}

if ! wait_for_postgres; then
  echo "error: postgres did not become ready" >&2
  exit 1
fi

echo "Applying pgroll migrations" >&2

if ! "$PGROLL_BIN" init \
  --postgres-url "$DATABASE_URL" \
  --schema public \
  --pgroll-schema pgroll >/dev/null
then
  echo "error: pgroll init failed" >&2
  exit 1
fi

if ! "$PGROLL_BIN" migrate "$MIGRATIONS_DIR" \
  --postgres-url "$DATABASE_URL" \
  --schema public \
  --pgroll-schema pgroll \
  --complete >/dev/null
then
  echo "error: pgroll migrate failed" >&2
  exit 1
fi

echo "Starting hs-starter server on $HS_STARTER_BASE_URL" >&2

SERVER_LOG=$(mktemp "hs-starter-playwright-server.XXXXXX.log")
cd "$REPO_ROOT"

PORT=$APP_PORT \
DATABASE_URL=$DATABASE_URL \
FIREBASE_API_KEY=$FIREBASE_API_KEY \
FIREBASE_PROJECT_ID=$FIREBASE_PROJECT_ID \
FIREBASE_AUTH_DOMAIN=${FIREBASE_AUTH_DOMAIN:-} \
SESSION_SECRET=$SESSION_SECRET \
  cabal run hs-starter >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!

SERVER_WAIT_SECONDS=${HS_STARTER_SERVER_TIMEOUT:-300}
wait_for_server() {
  for ((i = 0; i < SERVER_WAIT_SECONDS; ++i)); do
    if ! kill -0 "$SERVER_PID" >/dev/null 2>&1; then
      return 1
    fi
    if curl --silent --fail "$HS_STARTER_BASE_URL/health" >/dev/null 2>&1; then
      return 0
    fi
    sleep 1
  done
  return 1
}

if ! wait_for_server; then
  echo "error: hs-starter server failed to start; tail of log:" >&2
  tail -n 40 "$SERVER_LOG" >&2
  exit 1
fi

echo "Server is ready; running Playwright tests" >&2

playwright_args=()
for arg in "$@"; do
  playwright_args+=("$(printf '%q' "$arg")")
done
if [[ ${#playwright_args[@]} -gt 0 ]]; then
  PLAYWRIGHT_EXTRA_ARGS=" ${playwright_args[*]}"
else
  PLAYWRIGHT_EXTRA_ARGS=""
fi

env_vars=(HS_STARTER_BASE_URL FIREBASE_API_KEY FIREBASE_PROJECT_ID FIREBASE_AUTH_DOMAIN SESSION_SECRET HS_STARTER_PLAYWRIGHT_TIMEOUT_MS)
env_args=()
for var in "${env_vars[@]}"; do
  if [[ -n "${!var:-}" ]]; then
    env_args+=(-e "$var=${!var}")
  fi
done

set -x
docker run --rm "${NETWORK_ARGS[@]}" \
  -v "$PLAYWRIGHT_DIR:/work/tests" \
  -w /work/tests \
  "${env_args[@]}" \
  "$IMAGE" \
  bash -lc "npm ci && npx playwright test$PLAYWRIGHT_EXTRA_ARGS"
