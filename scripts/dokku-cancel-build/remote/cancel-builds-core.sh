#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: cancel-builds-core.sh [options] <app>

Options:
  --dry-run      Print actions without executing
  -h, --help     Show this help text
USAGE
}

log() {
  printf '=====> %s\n' "$1"
}

warn() {
  printf ' !     %s\n' "$1" >&2
}

DRY_RUN=false
APP=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --)
      shift
      APP="${1:-}"
      break
      ;;
    -* )
      warn "unknown option: $1"
      usage
      exit 1
      ;;
    * )
      APP="$1"
      shift
      ;;
  esac
done

if [[ -z "$APP" ]]; then
  warn "app name required"
  usage
  exit 1
fi

DOCKER_BIN=${DOCKER_BIN:-$(command -v docker || true)}
if [[ -z "$DOCKER_BIN" ]]; then
  warn "docker binary not found"
  exit 1
fi

DOKKU_ROOT=${DOKKU_ROOT:-/home/dokku}
LOCK_FILE="$DOKKU_ROOT/$APP/.deploy.lock"
CANCELLED=0

kill_pids_matching() {
  local pattern="$1"
  local label="$2"
  mapfile -t pids < <(pgrep -f "$pattern" 2>/dev/null || true)
  if [[ ${#pids[@]} -eq 0 ]]; then
    return
  fi
  CANCELLED=1
  for pid in "${pids[@]}"; do
    if $DRY_RUN; then
      log "would terminate $label process pid=$pid"
    else
      log "terminating $label process pid=$pid"
      kill "$pid" 2>/dev/null || true
    fi
  done
  if ! $DRY_RUN; then
    sleep 1
    for pid in "${pids[@]}"; do
      kill -0 "$pid" 2>/dev/null || continue
      log "force killing $label process pid=$pid"
      kill -9 "$pid" 2>/dev/null || true
    done
  fi
}

stop_containers_matching() {
  local description="$1"
  shift
  local filters=("$@")
  mapfile -t containers < <($DOCKER_BIN ps "${filters[@]}" --format '{{.ID}} {{.Command}}' 2>/dev/null || true)
  local matched=()
  for entry in "${containers[@]}"; do
    local id=${entry%% *}
    local cmd=${entry#* }
    if [[ "$cmd" != *"build"* && "$cmd" != *"/build"* && "$cmd" != *"pack"* ]]; then
      continue
    fi
    matched+=("$id::$cmd")
  done
  if [[ ${#matched[@]} -eq 0 ]]; then
    return
  fi
  CANCELLED=1
  for entry in "${matched[@]}"; do
    local id=${entry%%::*}
    local cmd=${entry#*::}
    if $DRY_RUN; then
      log "would remove $description container $id (cmd: $cmd)"
    else
      log "removing $description container $id (cmd: $cmd)"
      $DOCKER_BIN rm -f "$id" >/dev/null 2>&1 || $DOCKER_BIN kill "$id" >/dev/null 2>&1 || true
    fi
  done
}

# Kill docker build processes for the app image
APP_IMAGE="dokku/${APP}:latest"
kill_pids_matching "$DOCKER_BIN[[:space:]]+image[[:space:]]+build.*$APP_IMAGE" "docker build"

# Kill pack (cloud native buildpacks) builds
kill_pids_matching "pack[[:space:]]+build[[:space:]]+$APP_IMAGE" "pack build"

# Kill pack builds invoked via app name without registry prefix
kill_pids_matching "pack[[:space:]]+build[[:space:]].*${APP}" "pack build"

# Kill docker container based build steps (herokuish/buildpacks)
stop_containers_matching "build phase" --filter "label=com.dokku.app-name=${APP}" --filter "status=running"

# Clean up deploy lock if present
if [[ -f "$LOCK_FILE" ]]; then
  CANCELLED=1
  if $DRY_RUN; then
    log "would remove deploy lock $LOCK_FILE"
  else
    log "removing deploy lock $LOCK_FILE"
    rm -f "$LOCK_FILE"
  fi
fi

if [[ $CANCELLED -eq 0 ]]; then
  log "no running builds detected for ${APP}"
else
  if $DRY_RUN; then
    log "dry run complete (no changes made)"
  else
    log "cancelled build activity for ${APP}"
  fi
fi
