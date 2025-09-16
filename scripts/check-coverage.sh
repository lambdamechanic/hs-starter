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

TIX_FILE=$(find dist-newstyle -name 'hs-starter-tests.tix' -print -quit)
if [[ -z "${TIX_FILE}" ]]; then
  echo "Coverage .tix file not found. Run cabal test --enable-coverage first." >&2
  exit 1
fi

# Gather mix directories produced by cabal for both libraries and tests.
mapfile -t HPC_DIRS < <(find dist-newstyle -type d -path '*extra-compilation-artifacts/hpc/vanilla/mix*')
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
