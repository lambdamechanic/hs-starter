#!/usr/bin/env bash
set -euo pipefail

APP_NAME=${1:-${DOKKU_APP:-}}
DEFAULT_APP=${DEFAULT_APP:-hs-starter}
DOKKU_BIN=${DOKKU_BIN:-dokku}
JQ_BIN=${JQ_BIN:-jq}

resolve_dokku() {
  local candidate="${DOKKU_BIN:-dokku}"
  local -a cmd
  read -r -a cmd <<<"$candidate"

  if [[ ${#cmd[@]} -gt 0 && ${cmd[0]} == */* && -x ${cmd[0]} ]]; then
    DOKKU_CMD=("${cmd[@]}")
    return 0
  fi

  if command -v "${cmd[0]}" >/dev/null 2>&1; then
    DOKKU_CMD=("${cmd[@]}")
    return 0
  fi

  if [[ -x "$HOME/.dokku/contrib/dokku_client.sh" ]]; then
    DOKKU_CMD=(bash "$HOME/.dokku/contrib/dokku_client.sh")
    return 0
  fi

  return 1
}

usage() {
  cat <<USAGE
Usage: $(basename "$0") [dokku-app]

Checks that the Dokku config for the given app contains the required Firebase
settings and runs a couple of remote probes against the Firebase project.

Environment variables recognised:
  DOKKU_APP   default Dokku app name
  DEFAULT_APP fallback app when nothing else is provided (default: hs-starter)
  DOKKU_BIN   dokku CLI path (default: dokku)
  JQ_BIN      jq binary (default: jq)
USAGE
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi

if ! resolve_dokku; then
  echo "error: dokku CLI not found (set DOKKU_BIN or install dokku_client.sh)" >&2
  exit 1
fi

if [[ -z "$APP_NAME" ]]; then
  if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    remote_url=$(git remote get-url dokku 2>/dev/null || true)
    if [[ -z "$remote_url" ]]; then
      remote_url=$(git remote -v | awk '$2 ~ /@.+:/ {print $2; exit}')
    fi
    if [[ -n "$remote_url" ]]; then
      candidate=$remote_url
      candidate=${candidate% (fetch)}
      candidate=${candidate% (push)}
      if [[ $candidate == *:* ]]; then
        candidate=${candidate##*:}
      fi
      if [[ $candidate == */* ]]; then
        candidate=${candidate##*/}
      fi
      candidate=${candidate%.git}
      APP_NAME=$candidate
    fi
  fi
fi

if [[ -z "$APP_NAME" ]]; then
  APP_NAME=$DEFAULT_APP
fi

if ! command -v "$JQ_BIN" >/dev/null 2>&1; then
  echo "error: jq is required (set JQ_BIN or install jq)" >&2
  exit 1
fi

printf '==> Inspecting Dokku config for app %s\n' "$APP_NAME" >&2
CONFIG_OUTPUT=$("${DOKKU_CMD[@]}" config "$APP_NAME" 2>/dev/null || true)
if [[ -z "$CONFIG_OUTPUT" ]]; then
  echo "error: failed to read dokku config (does app '$APP_NAME' exist?)" >&2
  exit 1
fi

declare -A CONFIG
while IFS="" read -r line; do
  if [[ $line =~ ^([A-Z0-9_]+):[[:space:]]*(.*)$ ]]; then
    key=${BASH_REMATCH[1]}
    value=${BASH_REMATCH[2]}
    CONFIG[$key]="$value"
  fi
done <<<"$CONFIG_OUTPUT"

missing=()
for key in FIREBASE_API_KEY FIREBASE_PROJECT_ID; do
  if [[ -z "${CONFIG[$key]:-}" ]]; then
    missing+=("$key")
  fi
done

if [[ ${#missing[@]} -gt 0 ]]; then
  echo "error: missing required Firebase settings in Dokku config:" >&2
  for key in "${missing[@]}"; do
    echo "  - $key" >&2
  done
  cat <<'NOTE' >&2

To populate these values:
  1. Open https://console.firebase.google.com/
  2. Select your project (or create one).
  3. Navigate to Project settings → General → Your apps (Web).
     - Web API Key  → FIREBASE_API_KEY
     - Project ID   → FIREBASE_PROJECT_ID
     - Auth domain  → FIREBASE_AUTH_DOMAIN (usually <project>.firebaseapp.com unless you set a custom domain)
  4. In the Dokku host, run for each key:
       dokku config:set <app> KEY=value
NOTE
  exit 1
fi

API_KEY=${CONFIG[FIREBASE_API_KEY]}
PROJECT_ID=${CONFIG[FIREBASE_PROJECT_ID]}
AUTH_DOMAIN=${CONFIG[FIREBASE_AUTH_DOMAIN]:-${PROJECT_ID}.firebaseapp.com}
AUTH_DOMAIN_DERIVED=false
if [[ -z ${CONFIG[FIREBASE_AUTH_DOMAIN]:-} ]]; then
  AUTH_DOMAIN_DERIVED=true
fi
APP_ID=${CONFIG[FIREBASE_APP_ID]:-}
MSG_SENDER=${CONFIG[FIREBASE_MESSAGING_SENDER_ID]:-}
MEASUREMENT_ID=${CONFIG[FIREBASE_MEASUREMENT_ID]:-}
STORAGE_BUCKET=${CONFIG[FIREBASE_STORAGE_BUCKET]:-}

printf '\n==> Firebase settings\n'
printf '  FIREBASE_PROJECT_ID:        %s\n' "$PROJECT_ID"
if $AUTH_DOMAIN_DERIVED; then
  printf '  FIREBASE_AUTH_DOMAIN:       %s (derived)\n' "$AUTH_DOMAIN"
else
  printf '  FIREBASE_AUTH_DOMAIN:       %s\n' "$AUTH_DOMAIN"
fi
[[ -n "$APP_ID" ]] && printf '  FIREBASE_APP_ID:            %s\n' "$APP_ID"
[[ -n "$MSG_SENDER" ]] && printf '  FIREBASE_MESSAGING_SENDER_ID: %s\n' "$MSG_SENDER"
[[ -n "$STORAGE_BUCKET" ]] && printf '  FIREBASE_STORAGE_BUCKET:    %s\n' "$STORAGE_BUCKET"
[[ -n "$MEASUREMENT_ID" ]] && printf '  FIREBASE_MEASUREMENT_ID:    %s\n' "$MEASUREMENT_ID"

status=0

check_auth_domain() {
  local url="https://$AUTH_DOMAIN/__/firebase/init.json"
  printf '\n==> Probing auth domain: %s\n' "$url"
  local http_code
  http_code=$(curl -sS -o /dev/null -w '%{http_code}' "$url" || echo "000")
  if [[ "$http_code" == "200" ]]; then
    echo '  ✔ auth domain responded with 200'
  else
    if $AUTH_DOMAIN_DERIVED; then
      echo "  ⚠ auth domain returned HTTP $http_code (derived default domain)" >&2
      echo '    Deploy Firebase Hosting if you plan to use the default firebaseapp.com host.' >&2
    else
      echo "  ✖ auth domain returned HTTP $http_code" >&2
      echo '    The Firebase hosting/domain setup may be incomplete.' >&2
      status=1
    fi
  fi
}

probe_identity_toolkit() {
  printf '\n==> Calling Identity Toolkit with a fake account\n'
  local tmp
  tmp=$(mktemp)
  local payload
  payload='{"identifier":"fake.user@example.com","providerId":"google.com","continueUri":"https://example.com"}'
  local http_code
  http_code=$(curl -sS -X POST \
    -H 'Content-Type: application/json' \
    -d "$payload" \
    -o "$tmp" \
    -w '%{http_code}' \
    "https://identitytoolkit.googleapis.com/v1/accounts:createAuthUri?key=$API_KEY" || echo "000")

  if [[ "$http_code" != "200" ]]; then
    echo "  ✖ identitytoolkit API returned HTTP $http_code" >&2
    echo "    Response:" >&2
    sed 's/^/    /' "$tmp" >&2
    status=1
    rm -f "$tmp"
    return
  fi

  if "$JQ_BIN" -e 'has("registered") or has("authUri")' "$tmp" >/dev/null 2>&1; then
    echo '  ✔ identitytoolkit responded (API key appears valid)'
  else
    echo '  ⚠ identitytoolkit responded but payload was unexpected:' >&2
    sed 's/^/    /' "$tmp" >&2
    status=1
  fi
  rm -f "$tmp"
}

check_auth_domain
probe_identity_toolkit

if [[ $status -eq 0 ]]; then
  printf '\nAll Firebase checks passed.\n'
else
  printf '\nFirebase configuration has issues (see above).\n' >&2
fi

exit $status