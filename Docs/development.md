# Development

## Local Workflow

`build.sh` is the supported command-line entry point for this repository. It is intended for WSL users who compile Delphi projects with Delphi's Windows compilers.

Examples:

```bash
./build.sh Demo/TestApp.dpr Win64
./build.sh Demo/TestApp.dpr Win32
./build.sh Tests/TLuaTests.dpr Win64
./build.sh Tests/TLuaTests.dpr Win32
DELPHI_DEFINES=LUA_STATIC ./build.sh Tests/TLuaTests.dpr Win64
```

The script:

- resolves the repo root dynamically
- adds `Source/` to the Delphi unit and include search paths
- chooses `dcc32.exe` or `dcc64.exe` based on the requested platform
- emits DCUs and executables into `Bin/Win32` or `Bin/Win64`
- copies the matching Lua runtime DLL beside the built executable
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
- `Bin/Win32` and `Bin/Win64` contain platform-specific build outputs plus the copied runtime DLL needed to launch them directly.

## Project Structure

- `Source/Lua.pas` contains the higher-level framework implementation.
- `Source/LuaAPI.pas` contains the raw external Lua declarations.
- `Demo/` is useful for manual exploratory testing.
- `Tests/` is intended for quick regression checks and future coverage growth.

## Compatibility

This repo is currently oriented around Windows targets because `Lua.pas` depends on Windows units and the bundled build flow uses Delphi's Windows compiler.

If you want to extend the build pipeline later, the next practical steps are:

- add a CI-friendly strategy for compiling on a licensed Windows runner
- split Windows-specific code from the higher-level Lua wrappers where possible

## CI

The repository now includes a GitHub Actions workflow at [.github/workflows/delphi-ci.yml](../.github/workflows/delphi-ci.yml).

Runner expectations:

- Windows machine
- GitHub self-hosted runner
- runner labels: `self-hosted`, `windows`, `delphi`
- Delphi installed locally with `dcc32.exe` and `dcc64.exe`
- Lua runtime DLLs present in `Runtime/Win32` and `Runtime/Win64`

Windows-native helpers:

- [ci/windows-build.ps1](../ci/windows-build.ps1)
- [ci/windows-test.ps1](../ci/windows-test.ps1)

The workflow runs:

- regression tests for `Win32`
- regression tests for `Win64`
- demo build for `Win32`
- demo build for `Win64`
