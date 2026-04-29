#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PLATFORM="${1:-${DELPHI_PLATFORM:-Win64}}"

case "$PLATFORM" in
  Win32|win32)
    PLATFORM="Win32"
    BENCH_EXE="$ROOT/Bin/${PLATFORM}/TLuaBenchmarks.exe"
    ;;
  Win64|win64)
    PLATFORM="Win64"
    BENCH_EXE="$ROOT/Bin/${PLATFORM}/TLuaBenchmarks.exe"
    ;;
  Linux|linux|Linux64|linux64)
    PLATFORM="Linux64"
    BENCH_EXE="$ROOT/Bin/${PLATFORM}/TLuaBenchmarks"
    ;;
  *)
    printf 'Unsupported platform: %s\n' "$PLATFORM" >&2
    printf 'Use Win32, Win64, or Linux64.\n' >&2
    exit 1
    ;;
esac

shift || true

"$ROOT/build.sh" "Benchmarks/TLuaBenchmarks.dpr" "$PLATFORM"

if [[ ! -f "$BENCH_EXE" ]]; then
  printf 'Expected benchmark executable was not produced: %s\n' "$BENCH_EXE" >&2
  exit 1
fi

"$BENCH_EXE" "$@"
