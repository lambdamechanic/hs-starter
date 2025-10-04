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
SOURCE_PACKAGE="$TEMPLATE_ROOT/package.yaml"
CABAL_LOCAL_TEMPLATE="$SCRIPT_DIR/templates/cabal.project.local"

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

abs_path() {
  python3 - "$1" <<'PY'
import os, sys
print(os.path.realpath(sys.argv[1]))
PY
}

rewrite_file() {
  local path="$1"
  python3 - "$path" <<'PY'
import os
import re
import sys

path = sys.argv[1]

prefix = os.environ['PREFIX']
package_name = os.environ['PACKAGE_NAME']
package_module = os.environ['PACKAGE_MODULE']
project_name = os.environ['PROJECT_NAME']
project_underscore = os.environ['PROJECT_UNDERSCORE']
project_env_prefix = os.environ['PROJECT_ENV_PREFIX']
project_display = os.environ['PROJECT_DISPLAY']

try:
    with open(path, 'r', encoding='utf-8') as infile:
        content = infile.read()
except UnicodeDecodeError:
    sys.exit(0)

original = content

if path.endswith('.hs'):
    content = re.sub(r'\bStarter\.', f'{prefix}.', content)
    content = content.replace('Paths_hs_starter', f'Paths_{package_module}')

content = content.replace('hs-starter', project_name)
content = content.replace('hs_starter', project_underscore)
content = content.replace('HS_STARTER', project_env_prefix)
content = content.replace('LambdaLabs Starter', f'LambdaLabs {project_display}')

basename = os.path.basename(path)
if basename == 'Dockerfile':
    content = content.replace(f'{project_name}.cabal', f'{package_name}.cabal')
    content = content.replace(f'exe:{project_name}', f'exe:{package_name}')
    content = content.replace(f'/usr/local/bin/{project_name}', f'/usr/local/bin/{package_name}')
    content = content.replace(f'CMD ["{project_name}"]', f'CMD ["{package_name}"]')

if content != original:
    with open(path, 'w', encoding='utf-8') as outfile:
        outfile.write(content)
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
    if [[ -f "$dest" ]]; then
      rewrite_file "$dest"
    fi
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
    app/Main.hs)
      dest_rel="app/web/Main.hs"
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
  rewrite_file "$dest"
}

export TEMPLATE_ROOT TARGET PREFIX PREFIX_PATH PACKAGE_NAME PACKAGE_MODULE PROJECT_NAME PROJECT_UNDERSCORE PROJECT_ENV_PREFIX PROJECT_DISPLAY
export -f copy_file abs_path rewrite_file

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
  local template="$CABAL_LOCAL_TEMPLATE"
  if [[ ! -f "$template" ]]; then
    echo "error: cabal.project.local template missing at $template" >&2
    exit 1
  fi
  if [[ ! -f "$cabal_local" ]]; then
    cp "$template" "$cabal_local"
  elif ! grep -q 'records-sop:ghc-prim' "$cabal_local"; then
    [[ -s "$cabal_local" ]] && printf '\n' >> "$cabal_local"
    cat "$template" >> "$cabal_local"
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


def coerce_to_list(value):
    if value is None:
        return []
    if isinstance(value, list):
        return list(value)
    return [value]


def merge_list(key, source, target):
    src_list = coerce_to_list(source.get(key))
    tgt_list = coerce_to_list(target.get(key))
    for item in src_list:
        if item not in tgt_list:
            tgt_list.append(item)
    if tgt_list:
        target[key] = tgt_list


merge_list('dependencies', source_data, target_data)
if 'language' in source_data and 'language' not in target_data:
    target_data['language'] = source_data['language']

pkg_name = target_data.get('name', 'app')
pkg_module = pkg_name.replace('-', '_')
source_tests = source_data.get('tests', {}).get('hs-starter-tests', {}) or {}
source_pkg_name = source_data.get('name')
source_paths_module = f"Paths_{source_pkg_name.replace('-', '_')}" if source_pkg_name else None
source_test_deps = [dep for dep in coerce_to_list(source_tests.get('dependencies')) if dep != source_pkg_name]
source_test_generated = [mod for mod in coerce_to_list(source_tests.get('generated-other-modules')) if mod != source_paths_module]
sanitary_tests = dict(source_tests)
sanitary_tests['dependencies'] = source_test_deps
if source_test_generated:
    sanitary_tests['generated-other-modules'] = source_test_generated
else:
    sanitary_tests.pop('generated-other-modules', None)


def ensure_web_exe(data):
    executables = data.setdefault('executables', {})
    web_exe = executables.setdefault('web', {
        'main': 'Main.hs',
        'source-dirs': 'app/web',
        'other-modules': [],
        'ghc-options': ['-threaded', '-rtsopts', '-with-rtsopts=-N'],
        'dependencies': [pkg_name]
    })
    web_exe['source-dirs'] = 'app/web'


def ensure_web_tests(data):
    tests = data.setdefault('tests', {})
    web_suite = tests.setdefault('web-tests', {
        'main': 'Spec.hs',
        'source-dirs': 'test',
        'generated-other-modules': [f"Paths_{pkg_module}"],
        'dependencies': [pkg_name]
    })
    merge_list('dependencies', sanitary_tests, web_suite)
    merge_list('generated-other-modules', sanitary_tests, web_suite)
    deps = coerce_to_list(web_suite.get('dependencies'))
    for dep in collect_component_values(data, 'dependencies', 'hspec', skip={'web-tests'}):
        if dep not in deps:
            deps.append(dep)
    for dep in collect_all_component_dependencies(data, skip={'web-tests'}):
        if dep not in deps and dep != pkg_name:
            deps.append(dep)
    web_suite['dependencies'] = deps
    extra_tools = collect_component_values(data, 'build-tools', 'hspec-discover', skip={'web-tests'})
    if extra_tools:
        tools = coerce_to_list(web_suite.get('build-tools'))
        for tool in extra_tools:
            if tool not in tools:
                tools.append(tool)
        web_suite['build-tools'] = tools


def replace_module_prefix(module_name: str) -> str:
    if module_name.startswith('Starter.'):
        return module_name.replace('Starter', prefix, 1)
    return module_name


def collect_all_component_dependencies(data, skip=None):
    results = []
    skip = set(skip or ())
    for section_key in ('tests', 'internal-libraries'):
        components = (data.get(section_key, {}) or {})
        for name, component in components.items():
            if name in skip:
                continue
            for value in coerce_to_list(component.get('dependencies')):
                if isinstance(value, str):
                    results.append(value)
    return results


def collect_component_values(data, key, prefix, skip=None):
    results = []
    skip = set(skip or ())
    for section_key in ('tests', 'internal-libraries'):
        components = (data.get(section_key, {}) or {})
        for name, component in components.items():
            if name in skip:
                continue
            values = coerce_to_list(component.get(key))
            for value in values:
                if isinstance(value, str) and value.startswith(prefix):
                    results.append(value)
    return results


def ensure_library_modules(data):
    library = data.setdefault('library', {})
    source_library = source_data.get('library', {})
    for key in ('exposed-modules', 'other-modules'):
        desired = [replace_module_prefix(m) for m in coerce_to_list(source_library.get(key))]
        if not desired:
            continue
        existing = coerce_to_list(library.get(key))
        for mod in desired:
            if mod not in existing:
                existing.append(mod)
        if existing:
            library[key] = existing
    generated = coerce_to_list(library.get('generated-other-modules'))
    paths_module = f'Paths_{pkg_module}'
    if paths_module not in generated:
        generated.append(paths_module)
    library['generated-other-modules'] = generated


def ensure_default_extensions(data):
    required = ['DerivingStrategies', 'OverloadedStrings']
    exts = coerce_to_list(data.get('default-extensions'))
    changed = False
    for ext in required:
        if ext not in exts:
            exts.append(ext)
            changed = True
    if changed:
        data['default-extensions'] = exts


def ensure_internal_test_dependencies(data):
    internal_libs = data.get('internal-libraries', {}) or {}
    if not internal_libs:
        return
    extras = [dep for dep in source_test_deps if dep != pkg_name]
    if not extras:
        return
    for lib in internal_libs.values():
        deps = coerce_to_list(lib.get('dependencies'))
        updated = False
        for dep in extras:
            if dep not in deps:
                deps.append(dep)
                updated = True
        if updated:
            lib['dependencies'] = deps


ensure_web_exe(target_data)
ensure_web_tests(target_data)
ensure_library_modules(target_data)
ensure_internal_test_dependencies(target_data)
ensure_default_extensions(target_data)

with target_pkg.open('w') as f:
    yaml.safe_dump(target_data, f, sort_keys=False)
PYEMBED
