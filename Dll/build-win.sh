#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$ROOT/.." && pwd)"
PLATFORM="${1:-Win64}"
BDS_VERSION="${BDS_VERSION:-23.0}"

COMMON_SOURCES=(
  lapi.c lcode.c lctype.c ldebug.c ldo.c ldump.c lfunc.c lgc.c llex.c lmem.c
  lobject.c lopcodes.c lparser.c lstate.c lstring.c ltable.c ltm.c lundump.c
  lvm.c lzio.c lauxlib.c lbaselib.c lcorolib.c ldblib.c liolib.c lmathlib.c
  loadlib.c loslib.c lstrlib.c ltablib.c lutf8lib.c linit.c
)

case "$PLATFORM" in
  Win32|win32)
    PLATFORM="Win32"
    COMPILER="${BCC32:-/mnt/c/Program Files (x86)/Embarcadero/Studio/${BDS_VERSION}/bin/bcc32.exe}"
    OUTPUT_DLL="$REPO_ROOT/Runtime/Win32/lua55.dll"
    COMPILER_ARGS=(-DLUA_BUILD_AS_DLL -tWCD -vu -e"$(wslpath -w "$OUTPUT_DLL")")
    ;;
  Win64|win64)
    PLATFORM="Win64"
    COMPILER="${BCC64X:-/mnt/c/Program Files (x86)/Embarcadero/Studio/${BDS_VERSION}/bin64/bcc64x.exe}"
    OUTPUT_DLL="$REPO_ROOT/Runtime/Win64/lua55_64.dll"
    COMPILER_ARGS=(-DLUA_BUILD_AS_DLL -tD -o"$(wslpath -w "$OUTPUT_DLL")")
    ;;
  *)
    printf 'Unsupported platform: %s\n' "$PLATFORM" >&2
    printf 'Use Win32 or Win64.\n' >&2
    exit 1
    ;;
esac

if [[ ! -x "$COMPILER" ]]; then
  printf 'Compiler not found: %s\n' "$COMPILER" >&2
  printf 'Set BCC32/BCC64X or BDS_VERSION to match your RAD Studio installation.\n' >&2
  exit 1
fi

mkdir -p "$(dirname "$OUTPUT_DLL")"

cd "$ROOT"
"$COMPILER" "${COMPILER_ARGS[@]}" "${COMMON_SOURCES[@]}"

rm -f "${OUTPUT_DLL%.dll}.tds"

printf 'Built %s for %s\n' "$OUTPUT_DLL" "$PLATFORM"
