#!/usr/bin/env bash
set -euo pipefail

THRESHOLD_FILE="coverage/threshold"
if [[ ! -f "${THRESHOLD_FILE}" ]]; then
  echo "Coverage threshold file ${THRESHOLD_FILE} not found" >&2
  exit 1
fi
THRESHOLD=$(<"${THRESHOLD_FILE}")
THRESHOLD=${THRESHOLD%%[[:space:]]*}
if [[ -z "${THRESHOLD}" ]]; then
  echo "Coverage threshold is empty" >&2
  exit 1
fi

# Locate the generated .tix file. Prefer explicit env override, then dist-newstyle, then repo root.
TIX_FILE_CANDIDATE=""
if [[ -n "${HPCTIXFILE:-}" && -f "${HPCTIXFILE}" ]]; then
  TIX_FILE_CANDIDATE="${HPCTIXFILE}"
fi

if [[ -z "${TIX_FILE_CANDIDATE}" ]]; then
  TIX_FILE_CANDIDATE=$(find dist-newstyle -name 'hs-starter-tests.tix' -print -quit 2>/dev/null || true)
fi

if [[ -z "${TIX_FILE_CANDIDATE}" ]]; then
  # Fall back to any single .tix in dist-newstyle if naming differs
  mapfile -t TIX_CANDIDATES < <(find dist-newstyle -name '*.tix' -type f 2>/dev/null || true)
  if [[ ${#TIX_CANDIDATES[@]} -eq 1 ]]; then
    TIX_FILE_CANDIDATE="${TIX_CANDIDATES[0]}"
  fi
fi

if [[ -z "${TIX_FILE_CANDIDATE}" ]]; then
  # Cabal sometimes runs tests from the package root; HPC then writes the .tix here.
  if [[ -f "hs-starter-tests.tix" ]]; then
    TIX_FILE_CANDIDATE="hs-starter-tests.tix"
  fi
fi

if [[ -z "${TIX_FILE_CANDIDATE}" ]]; then
  echo "Coverage .tix file not found in dist-newstyle or repo root. Run cabal test --enable-coverage, or set HPCTIXFILE to the .tix path." >&2
  exit 1
fi
TIX_FILE="${TIX_FILE_CANDIDATE}"

# Gather top-level mix directories produced by cabal for both libraries and tests.
# We pass the parent "mix" dir to hpc; it resolves unit subdirs inside it.
mapfile -t HPC_DIRS < <(find dist-newstyle -type d -path '*extra-compilation-artifacts/hpc/vanilla/mix' 2>/dev/null || true)
if [[ ${#HPC_DIRS[@]} -eq 0 ]]; then
  echo "No HPC mix directories found." >&2
  exit 1
fi

HPC_ARGS=()
for dir in "${HPC_DIRS[@]}"; do
  HPC_ARGS+=("--hpcdir=${dir}")
done

REPORT=$(hpc report "${HPC_ARGS[@]}" "${TIX_FILE}")
echo "${REPORT}"

COVERAGE_RAW=$(awk '/expressions used/ {gsub("%", "", $1); print $1}' <<<"${REPORT}")
if [[ -z "${COVERAGE_RAW}" ]]; then
  echo "Failed to parse coverage percentage" >&2
  exit 1
fi

# Round to nearest integer for comparison.
COVERAGE_INT=$(printf '%.0f' "${COVERAGE_RAW}")
if (( COVERAGE_INT < THRESHOLD )); then
  echo "Coverage ${COVERAGE_INT}% is below threshold ${THRESHOLD}%" >&2
  exit 1
fi

echo "Coverage ${COVERAGE_RAW}% (rounded ${COVERAGE_INT}%) meets threshold ${THRESHOLD}%"
