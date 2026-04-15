#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MINILUA_DIR="$ROOT/minilua"
BDS_VERSION="${BDS_VERSION:-23.0}"
BCC64X="${BCC64X:-/mnt/c/Program Files (x86)/Embarcadero/Studio/${BDS_VERSION}/bin64/bcc64x.exe}"
ACTIVE_HEADER="${MINILUA_HEADER:-minilua.h}"
OUTPUT_OBJ="$ROOT/../Source/Obj/Win64/minilua.obj"
OUTPUT_OBJ_WIN="$(wslpath -w "$OUTPUT_OBJ")"

if [[ ! -x "$BCC64X" ]]; then
  printf 'Compiler not found: %s\n' "$BCC64X" >&2
  printf 'Set BCC64X or BDS_VERSION to match your RAD Studio installation.\n' >&2
  exit 1
fi

if [[ ! -f "$MINILUA_DIR/$ACTIVE_HEADER" ]]; then
  printf 'Requested MiniLua header not found: %s\n' "$MINILUA_DIR/$ACTIVE_HEADER" >&2
  exit 1
fi

mkdir -p "$ROOT/Win64"

cd "$MINILUA_DIR"
"$BCC64X" -c -o"$OUTPUT_OBJ_WIN" "-DMINILUA_HEADER=\"$ACTIVE_HEADER\"" minilua.c

printf 'Built %s using %s\n' "$OUTPUT_OBJ" "$ACTIVE_HEADER"
