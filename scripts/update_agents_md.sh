#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 3 ]]; then
  echo "usage: $(basename "$0") PREFIX PROJECT_NAME TARGET_FOLDER" >&2
  exit 1
fi

PREFIX=$1
PROJECT_NAME=$2
TARGET_INPUT=$3

if [[ -z "$PROJECT_NAME" ]]; then
  echo "error: project name must not be empty" >&2
  exit 1
fi

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
TEMPLATE_ROOT=$(cd "$SCRIPT_DIR/.." && pwd)

SOURCE_AGENTS="$TEMPLATE_ROOT/AGENTS.md"
if [[ ! -f "$SOURCE_AGENTS" ]]; then
  echo "warning: AGENTS.md not found in template at $SOURCE_AGENTS; skipping" >&2
  exit 0
fi

if [[ ! -d "$TARGET_INPUT" ]]; then
  echo "error: target folder '$TARGET_INPUT' does not exist" >&2
  exit 1
fi

TARGET=$(cd "$TARGET_INPUT" && pwd)
DEST_AGENTS="$TARGET/AGENTS.md"

PREFIX_PATH=${PREFIX//./\/}
PROJECT_UNDERSCORE=${PROJECT_NAME//-/_}
PROJECT_ENV_PREFIX=$(printf '%s' "$PROJECT_NAME" | tr '[:lower:]' '[:upper:]' | tr '-' '_')
PROJECT_DISPLAY=$(python3 - "$PROJECT_NAME" <<'PY'
import sys

slug = sys.argv[1]
slug = slug.replace('_', '-')
parts = [part for part in slug.split('-') if part]
if not parts:
    print(slug)
else:
    print(' '.join(part[:1].upper() + part[1:] for part in parts))
PY
)

rewrite_in_place() {
  local target_path="$1"
  python3 - "$target_path" <<'PY'
import os
import re
import sys
from pathlib import Path

path = Path(sys.argv[1])
if not path.exists():
    sys.exit(0)

prefix = os.environ['PREFIX']
prefix_path = os.environ['PREFIX_PATH']
project_name = os.environ['PROJECT_NAME']
project_underscore = os.environ['PROJECT_UNDERSCORE']
project_env_prefix = os.environ['PROJECT_ENV_PREFIX']
project_display = os.environ['PROJECT_DISPLAY']

content = path.read_text(encoding='utf-8')
content = re.sub(r'\bStarter\.', f'{prefix}.', content)
content = content.replace('Starter/', f'{prefix_path}/')
content = content.replace('hs-starter', project_name)
content = content.replace('hs_starter', project_underscore)
content = content.replace('HS_STARTER', project_env_prefix)
content = content.replace('LambdaLabs Starter', f'LambdaLabs {project_display}')
path.write_text(content, encoding='utf-8')
PY
}

export PREFIX PREFIX_PATH PROJECT_NAME PROJECT_UNDERSCORE PROJECT_ENV_PREFIX PROJECT_DISPLAY

temp_block=$(mktemp)
trap 'rm -f "$temp_block"' EXIT

cp "$SOURCE_AGENTS" "$temp_block"
rewrite_in_place "$temp_block"

if [[ ! -f "$DEST_AGENTS" ]]; then
  cp "$temp_block" "$DEST_AGENTS"
  exit 0
fi

rewrite_in_place "$DEST_AGENTS"

if python3 - "$DEST_AGENTS" "$temp_block" <<'PY'
import sys
from pathlib import Path

dest = Path(sys.argv[1]).read_text()
block = Path(sys.argv[2]).read_text()

if not block:
    sys.exit(0)

sys.exit(0 if block in dest else 1)
PY
then
  exit 0
fi

if [[ -s "$DEST_AGENTS" ]]; then
  last_char=$(tail -c 1 "$DEST_AGENTS" 2>/dev/null || printf '')
  if [[ "$last_char" != $'\n' ]]; then
    printf '\n' >>"$DEST_AGENTS"
  fi
fi

cat "$temp_block" >>"$DEST_AGENTS"
rewrite_in_place "$DEST_AGENTS"
