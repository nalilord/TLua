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
CUSTOM_DEFINES="${DELPHI_DEFINES:-}"

BDS_VERSION="${BDS_VERSION:-23.0}"
BIN_ROOT="$ROOT/Bin"
RUNTIME_ROOT="$ROOT/Runtime"
SOURCE_DIR="$ROOT/Source"

build_linux_runtime() {
  local target="$1"
  local cc="${CC:-gcc}"
  local rebuild=0
  local source
  local -a sources=()

  for source in "$ROOT"/Dll/*.c; do
    case "$(basename "$source")" in
      lua.c|luac.c)
        continue
        ;;
    esac
    sources+=("$source")
  done

  if [[ ${#sources[@]} -eq 0 ]]; then
    printf 'Lua C sources not found under %s/Dll\n' "$ROOT" >&2
    exit 1
  fi

  if [[ ! -f "$target" ]]; then
    rebuild=1
  else
    for source in "${sources[@]}"; do
      if [[ "$source" -nt "$target" ]]; then
        rebuild=1
        break
      fi
    done
  fi

  if [[ "$rebuild" -eq 0 ]]; then
    return
  fi

  "$cc" \
    -std=gnu99 \
    -O2 \
    -Wall \
    -Wextra \
    -fPIC \
    -shared \
    -DLUA_USE_LINUX \
    -o "$target" \
    "${sources[@]}" \
    -Wl,-E \
    -ldl \
    -lm
}

append_fpc_defines() {
  local define
  local normalized="${CUSTOM_DEFINES//,/;}"
  normalized="${normalized// /;}"

  IFS=';' read -r -a FPC_DEFINE_LIST <<< "$normalized"
  for define in "${FPC_DEFINE_LIST[@]}"; do
    if [[ -n "$define" ]]; then
      FPC_ARGS+=("-d$define")
    fi
  done
}

case "$PLATFORM" in
  Win32|win32)
    PLATFORM="Win32"
    PROJECT_DIR_WIN="$(wslpath -w "$PROJECT_DIR")"
    SOURCE_DIR_WIN="$(wslpath -w "$SOURCE_DIR")"
    DCC="${DCC32:-/mnt/c/Program Files (x86)/Embarcadero/Studio/${BDS_VERSION}/bin/dcc32.exe}"
    DELPHI_LIB="${DELPHI_LIB_WIN32:-c:\\program files (x86)\\embarcadero\\studio\\${BDS_VERSION}\\lib\\win32\\release}"
    NAMESPACE_SET="Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;System;Xml;Data;Datasnap;Web;Soap"
    RUNTIME_DLL_NAME="lua55.dll"
    ;;
  Win64|win64)
    PLATFORM="Win64"
    PROJECT_DIR_WIN="$(wslpath -w "$PROJECT_DIR")"
    SOURCE_DIR_WIN="$(wslpath -w "$SOURCE_DIR")"
    DCC="${DCC64:-/mnt/c/Program Files (x86)/Embarcadero/Studio/${BDS_VERSION}/bin/dcc64.exe}"
    DELPHI_LIB="${DELPHI_LIB_WIN64:-c:\\program files (x86)\\embarcadero\\studio\\${BDS_VERSION}\\lib\\win64\\release}"
    NAMESPACE_SET="Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;System;Xml;Data;Datasnap;Web;Soap"
    RUNTIME_DLL_NAME="lua55_64.dll"
    ;;
  Linux|linux|Linux64|linux64)
    PLATFORM="Linux64"
    FPC="${FPC:-fpc}"
    RUNTIME_SO_NAME="liblua55.so"
    ;;
  *)
    printf 'Unsupported platform: %s\n' "$PLATFORM" >&2
    printf 'Use Win32, Win64, or Linux64.\n' >&2
    exit 1
    ;;
esac

BUILD_DIR="$BIN_ROOT/$PLATFORM"
UNIT_DIR="$BUILD_DIR/Units"

if [[ ! -f "$PROJECT_PATH" ]]; then
  printf 'Project not found: %s\n' "$PROJECT_REL" >&2
  exit 1
fi

mkdir -p "$BUILD_DIR" "$UNIT_DIR"

if [[ "$PLATFORM" == "Linux64" ]]; then
  if ! command -v "$FPC" >/dev/null 2>&1; then
    printf 'Compiler not found: %s\n' "$FPC" >&2
    printf 'Set FPC to the Free Pascal compiler you want to use.\n' >&2
    exit 1
  fi

  RUNTIME_DIR="$RUNTIME_ROOT/$PLATFORM"
  RUNTIME_SO_SOURCE="$RUNTIME_DIR/$RUNTIME_SO_NAME"
  RUNTIME_SO_TARGET="$BUILD_DIR/$RUNTIME_SO_NAME"
  mkdir -p "$RUNTIME_DIR"
  build_linux_runtime "$RUNTIME_SO_SOURCE"
  if [[ ! -f "$RUNTIME_SO_TARGET" ]] || ! cmp -s "$RUNTIME_SO_SOURCE" "$RUNTIME_SO_TARGET"; then
    cp -f "$RUNTIME_SO_SOURCE" "$RUNTIME_SO_TARGET"
  fi

  FPC_ARGS=(
    -B
    "-Fu$SOURCE_DIR"
    "-Fu$PROJECT_DIR"
    "-FU$UNIT_DIR"
    "-FE$BUILD_DIR"
    "-Fl$BUILD_DIR"
    "-k-rpath=\$ORIGIN"
    "-o$BUILD_DIR/$PROJECT_NAME"
  )
  append_fpc_defines

  cd "$ROOT"
  "$FPC" "${FPC_ARGS[@]}" "$PROJECT_PATH"
else
  BUILD_DIR_WIN="${BUILD_DIR_WIN:-$(wslpath -w "$BUILD_DIR")}"
  RUNTIME_DLL_SOURCE="$RUNTIME_ROOT/$PLATFORM/$RUNTIME_DLL_NAME"
  RUNTIME_DLL_TARGET="$BUILD_DIR/$RUNTIME_DLL_NAME"
  DEFINE_SET="PLATFORM_${PLATFORM}"
  STATIC_LUA_ENABLED=0
  SEARCH_PATH_WIN="${PROJECT_DIR_WIN};${SOURCE_DIR_WIN};${DELPHI_LIB}"

  if [[ -n "$CUSTOM_DEFINES" ]]; then
    DEFINE_SET="${DEFINE_SET};${CUSTOM_DEFINES}"
  fi

  if [[ ";${CUSTOM_DEFINES};" == *";LUA_STATIC;"* ]]; then
    STATIC_LUA_ENABLED=1
  fi

  if [[ ! -x "$DCC" ]]; then
    printf 'Compiler not found: %s\n' "$DCC" >&2
    printf 'Set DCC32/DCC64 or BDS_VERSION to match your local Delphi installation.\n' >&2
    exit 1
  fi

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
    -NS"$NAMESPACE_SET" \
    -N0"$BUILD_DIR_WIN" \
    -NU"$BUILD_DIR_WIN" \
    -D"$DEFINE_SET" \
    -E"$BUILD_DIR_WIN" \
    "$PROJECT_FILE"

  if [[ "$STATIC_LUA_ENABLED" -eq 0 ]]; then
    cp -f "$RUNTIME_DLL_SOURCE" "$RUNTIME_DLL_TARGET"
  fi
fi

printf 'Built %s for %s in %s\n' "$PROJECT_NAME" "$PLATFORM" "$BUILD_DIR"
