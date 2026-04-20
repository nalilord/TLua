# Development

## Local Workflow

`build.sh` is the supported command-line entry point for this repository. It supports Delphi `Win32`/`Win64` builds from WSL and Free Pascal `Linux64` builds in the local Linux environment.

Examples:

```bash
./build.sh Demo/TestApp.dpr Win64
./build.sh Demo/TestApp.dpr Win32
./build.sh Demo/TestApp.dpr Linux64
./build.sh Tests/TLuaTests.dpr Win64
./build.sh Tests/TLuaTests.dpr Win32
./build.sh Tests/TLuaTests.dpr Linux64
DELPHI_DEFINES=LUA_STATIC ./build.sh Tests/TLuaTests.dpr Win64
```

The script:

- resolves the repo root dynamically
- adds `Source/` to the compiler unit and include search paths
- chooses `dcc32.exe`, `dcc64.exe`, or `fpc` based on the requested platform
- emits compiled outputs into `Bin/Win32`, `Bin/Win64`, or `Bin/Linux64`
- stages the matching Lua runtime library beside the built executable
- rebuilds the Win64 static Lua object automatically when `DELPHI_DEFINES=LUA_STATIC` is set
- allows compiler-path overrides through environment variables

## Static Mode

`LUA_STATIC` is currently supported for `Win64` only.

Example:

```bash
DELPHI_DEFINES=LUA_STATIC ./build.sh Demo/TestApp.dpr Win64
DELPHI_DEFINES=LUA_STATIC ./test.sh Tests/TLuaTests.dpr Win64
```

In this mode:

- `Source/LuaAPI.pas` links against the staged object at `Source/Obj/Win64/minilua.obj`
- `build.sh` does not copy `lua55_64.dll`
- the final executable still imports `ucrtbase.dll`
- on Windows 10/11 that DLL is normally part of the OS runtime baseline

Static-source layout:

- `StaticLua/minilua/minilua.c` is the local wrapper used to build the Win64 object.
- `StaticLua/minilua/minilua.h` is the project-owned Lua 5.5.0 single-header bundle.
- `rebuild-win64-minilua.sh` still accepts `MINILUA_HEADER` for explicit experiments, but the repo baseline is now just `minilua.h`.

## Runtime Layout

Runtime and output layout:

- `Runtime/Win32/lua55.dll` and `Runtime/Win64/lua55_64.dll` are the canonical Lua runtime copies.
- `Runtime/Linux64/liblua55.so` is the canonical Linux shared-library copy.
- `Bin/Win32`, `Bin/Win64`, and `Bin/Linux64` contain platform-specific build outputs plus the copied runtime library needed to launch them directly.
- `Bin/Linux64/Units` contains FPC-generated unit/object files so they do not spill into `Source/` or `Tests/`.

## Project Structure

- `Source/Lua.pas` contains the higher-level framework implementation.
- `Source/LuaAPI.pas` contains the raw external Lua declarations.
- `Demo/` is useful for manual exploratory testing.
- `Tests/` is intended for quick regression checks and future coverage growth.

## Compatibility

This repo now supports:

- Delphi on `Win32`
- Delphi on `Win64`
- FPC on `Linux64`

The remaining platform/compiler branches are intentionally concentrated in compatibility and FFI boundary files rather than the higher-level wrapper code.

## CI

The repository now includes a GitHub Actions workflow at [.github/workflows/delphi-ci.yml](../.github/workflows/delphi-ci.yml).

Runner expectations:

- Windows machine
- GitHub self-hosted runner
- runner labels: `self-hosted`, `windows`, `delphi`
- Delphi installed locally with `dcc32.exe` and `dcc64.exe`
- Lua runtime libraries present in `Runtime/Win32` and `Runtime/Win64`

Windows-native helpers:

- [ci/windows-build.ps1](../ci/windows-build.ps1)
- [ci/windows-test.ps1](../ci/windows-test.ps1)

The workflow runs:

- regression tests for `Win32`
- regression tests for `Win64`
- demo build for `Win32`
- demo build for `Win64`
