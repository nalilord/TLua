# Static Lua

This directory contains the project-owned Win64 static-link integration for Lua.

## Layout

- `minilua/` holds the upstream single-header source bundle plus the local `minilua.c` wrapper used for Delphi builds.
- `../Source/Obj/Win64/minilua.obj` is the staged build artifact linked by `Source/LuaAPI.pas` when `LUA_STATIC` is defined.
- `rebuild-win64-minilua.sh` rebuilds the staged object with `bcc64x`.

## Header Selection

The default build uses `minilua.h`, which now contains the project-owned Lua 5.5.0 single-header bundle.

`rebuild-win64-minilua.sh` still accepts `MINILUA_HEADER` if you want to point it at a different compatible header explicitly, but the repository baseline is `minilua.h`.

Current status:

- `minilua.h` is the tested Lua 5.5.0 default.
- The Win64 static path passes the same `TLua` regression suite used for the DLL-backed build.

## Dependency Model

The goal of `LUA_STATIC` is to remove the Lua DLL dependency while preserving normal Lua behavior.

That does not currently eliminate the Microsoft C runtime dependency. The Win64 static build still resolves CRT symbols from `ucrtbase.dll`.

On Windows 10 and Windows 11, `ucrtbase.dll` is typically available as part of the operating system runtime, so this is usually not an extra deployment concern in the way a separate Lua DLL would be.
