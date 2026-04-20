#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_REL="${1:-Tests/TLuaTests.dpr}"
PROJECT_REL="${PROJECT_REL#./}"
PROJECT_NAME="$(basename "${PROJECT_REL%.dpr}")"
PLATFORM="${2:-${DELPHI_PLATFORM:-Win64}}"

case "$PLATFORM" in
  Win32|win32)
    PLATFORM="Win32"
    TEST_EXE="$ROOT/Bin/${PLATFORM}/${PROJECT_NAME}.exe"
    ;;
  Win64|win64)
    PLATFORM="Win64"
    TEST_EXE="$ROOT/Bin/${PLATFORM}/${PROJECT_NAME}.exe"
    ;;
  Linux|linux|Linux64|linux64)
    PLATFORM="Linux64"
    TEST_EXE="$ROOT/Bin/${PLATFORM}/${PROJECT_NAME}"
    ;;
  *)
    printf 'Unsupported platform: %s\n' "$PLATFORM" >&2
    printf 'Use Win32, Win64, or Linux64.\n' >&2
    exit 1
    ;;
esac

"$ROOT/build.sh" "$PROJECT_REL" "$PLATFORM"

if [[ ! -f "$TEST_EXE" ]]; then
  printf 'Expected test executable was not produced: %s\n' "$TEST_EXE" >&2
  exit 1
fi

"$TEST_EXE"
