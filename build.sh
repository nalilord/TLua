#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_REL="${1:-Demo/TestApp.dpr}"
PROJECT_REL="${PROJECT_REL#./}"
PROJECT_PATH="$ROOT/$PROJECT_REL"
PROJECT_DIR="$(dirname "$PROJECT_PATH")"
PROJECT_FILE="$(basename "$PROJECT_PATH")"
PROJECT_NAME="$(basename "${PROJECT_REL%.dpr}")"
PLATFORM="${2:-${DELPHI_PLATFORM:-Win64}}"
PROJECT_DIR_WIN="$(wslpath -w "$PROJECT_DIR")"
CUSTOM_DEFINES="${DELPHI_DEFINES:-}"

BDS_VERSION="${BDS_VERSION:-23.0}"
SOURCE_DIR_WIN="$(wslpath -w "$ROOT/Source")"
BIN_ROOT="$ROOT/Bin"
BUILD_DIR="$BIN_ROOT/$PLATFORM"
RUNTIME_ROOT="$ROOT/Runtime"

case "$PLATFORM" in
  Win32|win32)
    PLATFORM="Win32"
    DCC="${DCC32:-/mnt/c/Program Files (x86)/Embarcadero/Studio/${BDS_VERSION}/bin/dcc32.exe}"
    DELPHI_LIB="${DELPHI_LIB_WIN32:-c:\\program files (x86)\\embarcadero\\studio\\${BDS_VERSION}\\lib\\win32\\release}"
    RUNTIME_DLL_NAME="lua55.dll"
    ;;
  Win64|win64)
    PLATFORM="Win64"
    DCC="${DCC64:-/mnt/c/Program Files (x86)/Embarcadero/Studio/${BDS_VERSION}/bin/dcc64.exe}"
    DELPHI_LIB="${DELPHI_LIB_WIN64:-c:\\program files (x86)\\embarcadero\\studio\\${BDS_VERSION}\\lib\\win64\\release}"
    RUNTIME_DLL_NAME="lua55_64.dll"
    ;;
  *)
    printf 'Unsupported Delphi platform: %s\n' "$PLATFORM" >&2
    printf 'Use Win32 or Win64.\n' >&2
    exit 1
    ;;
esac

BUILD_DIR="$BIN_ROOT/$PLATFORM"
BUILD_DIR_WIN="${BUILD_DIR_WIN:-$(wslpath -w "$BUILD_DIR")}"
RUNTIME_DLL_SOURCE="$RUNTIME_ROOT/$PLATFORM/$RUNTIME_DLL_NAME"
RUNTIME_DLL_TARGET="$BUILD_DIR/$RUNTIME_DLL_NAME"
DEFINE_SET="PLATFORM_${PLATFORM}"
STATIC_LUA_ENABLED=0

if [[ -n "$CUSTOM_DEFINES" ]]; then
  DEFINE_SET="${DEFINE_SET};${CUSTOM_DEFINES}"
fi

if [[ ";${CUSTOM_DEFINES};" == *";LUA_STATIC;"* ]]; then
  STATIC_LUA_ENABLED=1
fi

SEARCH_PATH_WIN="${PROJECT_DIR_WIN};${SOURCE_DIR_WIN};${DELPHI_LIB}"

if [[ ! -f "$PROJECT_PATH" ]]; then
  printf 'Project not found: %s\n' "$PROJECT_REL" >&2
  exit 1
fi

if [[ ! -x "$DCC" ]]; then
  printf 'Compiler not found: %s\n' "$DCC" >&2
  printf 'Set DCC32/DCC64 or BDS_VERSION to match your local Delphi installation.\n' >&2
  exit 1
fi

mkdir -p "$BUILD_DIR"

if [[ "$STATIC_LUA_ENABLED" -eq 0 && ! -f "$RUNTIME_DLL_SOURCE" ]]; then
  printf 'Runtime DLL not found: %s\n' "$RUNTIME_DLL_SOURCE" >&2
  exit 1
fi

if [[ "$PLATFORM" == "Win64" && "$STATIC_LUA_ENABLED" -eq 1 ]]; then
  "$ROOT/StaticLua/rebuild-win64-minilua.sh"
fi

cd "$PROJECT_DIR"
"$DCC" \
  -B \
  -U"$SEARCH_PATH_WIN" \
  -I"${PROJECT_DIR_WIN};${SOURCE_DIR_WIN}" \
  -N0"$BUILD_DIR_WIN" \
  -NU"$BUILD_DIR_WIN" \
  -D"$DEFINE_SET" \
  -E"$BUILD_DIR_WIN" \
  "$PROJECT_FILE"

if [[ "$STATIC_LUA_ENABLED" -eq 0 ]]; then
  cp -f "$RUNTIME_DLL_SOURCE" "$RUNTIME_DLL_TARGET"
fi

printf 'Built %s for %s in %s\n' "$PROJECT_NAME" "$PLATFORM" "$BUILD_DIR"
