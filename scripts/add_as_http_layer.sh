#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $(basename "$0") PREFIX TARGET_FOLDER" >&2
  exit 1
fi

PREFIX=$1
TARGET_INPUT=$2

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
TEMPLATE_ROOT=$(cd "$SCRIPT_DIR/.." && pwd)
SOURCE_PACKAGE="$TEMPLATE_ROOT/package.yaml"

if [[ ! -f "$SOURCE_PACKAGE" ]]; then
  echo "error: package.yaml not found in template at $SOURCE_PACKAGE" >&2
  exit 1
fi

if ! python3 - <<'PY' >/dev/null 2>&1
import yaml
PY
then
  echo "error: python3-yaml is required" >&2
  exit 1
fi

if [[ ! -d "$TARGET_INPUT" ]]; then
  echo "error: target folder '$TARGET_INPUT' does not exist" >&2
  exit 1
fi

TARGET=$(cd "$TARGET_INPUT" && pwd)
TARGET_PACKAGE="$TARGET/package.yaml"
if [[ ! -f "$TARGET_PACKAGE" ]]; then
  echo "error: target package.yaml not found at $TARGET_PACKAGE" >&2
  exit 1
fi

PACKAGE_NAME=$(python3 - "$TARGET_PACKAGE" <<'PY'
import sys, yaml
with open(sys.argv[1]) as f:
    data = yaml.safe_load(f)
print(data.get("name", "app"))
PY
)
PACKAGE_MODULE=${PACKAGE_NAME//-/_}
PREFIX_PATH=${PREFIX//./\/}

abs_path() {
  python3 - "$1" <<'PY'
import os, sys
print(os.path.realpath(sys.argv[1]))
PY
}

copy_dir_if_missing() {
  local src="$1"
  local dest="$2"
  local label="$3"
  if [[ -e "$dest" ]]; then
    echo "warning: $label already exists at $dest; skipping" >&2
  else
    mkdir -p "$(dirname "$dest")"
    cp -R "$src" "$dest"
  fi
}

copy_file() {
  local src="$1"
  local rel="${src#$TEMPLATE_ROOT/}"
  local dest_rel="$rel"

  case "$rel" in
    src/Starter/*)
      dest_rel="src/${PREFIX_PATH}/${rel#src/Starter/}"
      ;;
    test/Starter/*)
      dest_rel="test/${PREFIX_PATH}/${rel#test/Starter/}"
      ;;
    app/Starter/*)
      dest_rel="app/${PREFIX_PATH}/${rel#app/Starter/}"
      ;;
    *)
      return
      ;;
  esac

  local dest="$TARGET/$dest_rel"
  mkdir -p "$(dirname "$dest")"

  if [[ -e "$dest" ]]; then
    local src_real=$(abs_path "$src")
    local dest_real=$(abs_path "$dest")
    if [[ "$src_real" == "$dest_real" ]]; then
      echo "warning: skipping $src (same as destination)" >&2
      return
    fi
  fi

  cp "$src" "$dest"
  perl -0pi -e 's/\bStarter\./'"$PREFIX"'./g; s/Paths_hs_starter/Paths_'"$PACKAGE_MODULE"'/g' "$dest"
}

export TEMPLATE_ROOT TARGET PREFIX PREFIX_PATH PACKAGE_MODULE
export -f copy_file abs_path

find "$TEMPLATE_ROOT" \
  \( -path "$TEMPLATE_ROOT/.git" -o -path "$TEMPLATE_ROOT/dist-newstyle" -o -path "$TEMPLATE_ROOT/.stack-work" \) -prune \
  -o -name '*.hs' -print | while read -r file; do
  copy_file "$file"
done

if [[ -d "$TEMPLATE_ROOT/db/pgroll" ]]; then
  copy_dir_if_missing "$TEMPLATE_ROOT/db/pgroll" "$TARGET/db/pgroll" "db/pgroll migrations"
fi

if [[ -f "$TEMPLATE_ROOT/Dockerfile" ]]; then
  copy_dir_if_missing "$TEMPLATE_ROOT/Dockerfile" "$TARGET/Dockerfile" "Dockerfile"
fi

if [[ -d "$TEMPLATE_ROOT/.github/workflows" ]]; then
  mkdir -p "$TARGET/.github/workflows"
  for wf in "$TEMPLATE_ROOT/.github/workflows"/*; do
    [[ -f "$wf" ]] || continue
    dest="$TARGET/.github/workflows/$(basename "$wf")"
    if [[ -e "$dest" ]]; then
      echo "warning: workflow $(basename "$wf") already exists; skipping" >&2
    else
      cp "$wf" "$dest"
    fi
  done
fi

cabal_local_update() {
  local cabal_local="$TARGET/cabal.project.local"
  local block=$(cat <<'EOS'
allow-newer: records-sop:ghc-prim, records-sop:deepseq

source-repository-package
  type: git
  location: https://github.com/jfischoff/tmp-postgres.git
  tag: 7f2467a6d6d5f6db7eed59919a6773fe006cf22b

source-repository-package
  type: git
  location: https://github.com/mwotton/roboservant.git
  tag: f06d8ac99ce13a55c4f35e98065963ce08634806

source-repository-package
  type: git
  location: https://github.com/cachix/hs-opentelemetry-instrumentation-servant.git
  tag: e29af39edfce5434c985223cfd5a0a4b24440b4e
EOS
)
  if [[ ! -f "$cabal_local" ]]; then
    printf '%s\n' "$block" > "$cabal_local"
  elif ! grep -q 'records-sop:ghc-prim' "$cabal_local"; then
    [[ -s "$cabal_local" ]] && printf '\n' >> "$cabal_local"
    printf '%s\n' "$block" >> "$cabal_local"
  fi
}

cabal_local_update

python3 - "$SOURCE_PACKAGE" "$TARGET_PACKAGE" "$PREFIX" <<'PYEMBED'
import sys
from pathlib import Path
import yaml

source_pkg = Path(sys.argv[1])
target_pkg = Path(sys.argv[2])
prefix = sys.argv[3]

source_data = yaml.safe_load(source_pkg.read_text())
target_data = yaml.safe_load(target_pkg.read_text())
if target_data is None:
    target_data = {}


def merge_list(key, source, target):
    src_list = source.get(key, []) or []
    tgt_list = list(target.get(key, []) or [])
    for item in src_list:
        if item not in tgt_list:
            tgt_list.append(item)
    if tgt_list:
        target[key] = tgt_list

merge_list('dependencies', source_data, target_data)
merge_list('default-extensions', source_data, target_data)
merge_list('ghc-options', source_data, target_data)
if 'language' in source_data and 'language' not in target_data:
    target_data['language'] = source_data['language']

pkg_name = target_data.get('name', 'app')


def ensure_web_exe(data):
    executables = data.setdefault('executables', {})
    executables.setdefault('web', {
        'main': 'Main.hs',
        'source-dirs': 'app',
        'other-modules': [],
        'ghc-options': ['-threaded', '-rtsopts', '-with-rtsopts=-N'],
        'dependencies': [pkg_name]
    })


def ensure_web_tests(data):
    source_tests = source_data.get('tests', {}).get('hs-starter-tests', {})
    source_test_deps = source_tests.get('dependencies', [])
    deps = [pkg_name]
    for dep in source_test_deps:
        if dep == source_data.get('name'):
            continue
        if dep not in deps:
            deps.append(dep)
    tests = data.setdefault('tests', {})
    tests.setdefault('web-tests', {
        'main': 'Spec.hs',
        'source-dirs': 'test',
        'generated-other-modules': [f"Paths_{pkg_name.replace('-', '_') }"],
        'dependencies': deps
    })

ensure_web_exe(target_data)
ensure_web_tests(target_data)

with target_pkg.open('w') as f:
    yaml.safe_dump(target_data, f, sort_keys=False)
PYEMBED
