# TLua

`TLua` is a Delphi wrapper around Lua 5.5.0 for Win32/Win64 applications. It provides a higher-level `TLua` facade over the raw Lua C API and includes helpers for values, tables, functions, threads, libraries, and Delphi class binding.

This repository now includes:

- A fixed command-line build script for WSL-to-Delphi workflows.
- A console regression suite under `Tests/`.
- Updated project documentation that matches the current repo layout.

## Repository Layout

- `Source/` - framework source (`Lua.pas`, `LuaAPI.pas`)
- `Demo/` - VCL sample application
- `Tests/` - console regression project
- `Dll/` - Lua 5.5.0 C sources / DLL build assets
- `Runtime/` - canonical Lua runtime DLL copies used by the scripts
- `Bin/` - platform-specific build output folders

## Requirements

- Delphi 12 Athens or a compatible `dcc32.exe` / `dcc64.exe`
- Windows runtime DLL beside the executable:
  - `lua55.dll` for Win32
  - `lua55_64.dll` for Win64
- For the provided shell scripts: WSL with access to the Delphi installation
- Static mode note:
  - `LUA_STATIC` is currently supported on `Win64` only
  - static builds do not require `lua55_64.dll`
  - static builds still depend on `ucrtbase.dll`, which is part of the Windows runtime on modern Windows 10/11 systems

## Build

Build the demo:

```bash
./build.sh Demo/TestApp.dpr Win64
./build.sh Demo/TestApp.dpr Win32
```

Build the regression suite:

```bash
./build.sh Tests/TLuaTests.dpr Win64
./build.sh Tests/TLuaTests.dpr Win32
```

The script auto-adds the repo `Source` directory to Delphi's search path, selects the matching Delphi compiler for `Win32` or `Win64`, copies the matching Lua runtime DLL, and emits binaries into `Bin/Win32` or `Bin/Win64`.

Build the regression suite in static mode:

```bash
DELPHI_DEFINES=LUA_STATIC ./build.sh Tests/TLuaTests.dpr Win64
```

Optional environment overrides:

- `DCC32` - full path to `dcc32.exe`
- `DCC64` - full path to `dcc64.exe`
- `BDS_VERSION` - Delphi install version used to derive the default compiler path
- `BUILD_DIR_WIN` - Windows output directory passed to the compiler
- `DELPHI_PLATFORM` - default target platform when no explicit second argument is provided
- `DELPHI_LIB_WIN32` - Win32 Delphi library path when your install layout differs
- `DELPHI_LIB_WIN64` - Win64 Delphi library path when your install layout differs
- `DELPHI_DEFINES` - extra Delphi conditional defines, for example `LUA_STATIC`

## Test

Compile and run the console regression suite:

```bash
./test.sh Tests/TLuaTests.dpr Win64
./test.sh Tests/TLuaTests.dpr Win32
DELPHI_DEFINES=LUA_STATIC ./test.sh Tests/TLuaTests.dpr Win64
```

Platform outputs now land in:

- `Bin/Win64`
- `Bin/Win32`

Current coverage includes:

- global value round-trips
- direct Lua execution
- Delphi callback registration
- Lua function introduction and result handling
- table creation/access
- library registration and constant/function access
- thread execution and failure reporting
- error handlers plus last-error state tracking
- `LoadSource` success and failure cases
- inheritance helpers, return propagation, and Lua-defined child overrides
- allocator-backed memory tracking growth
- blueprint-based class binding

Details: [Docs/testing.md](Docs/testing.md)

## Quick Start

```pascal
uses
  Lua, System.SysUtils;

var
  L: TLua;
  F: TLuaFunction;
begin
  L := TLua.Create;
  try
    L.Globals['answer'] := 42;
    L.ExecuteDirect(
      'function greet(name) ' +
      '  return "Hello, " .. name .. "! answer=" .. tostring(answer) ' +
      'end'
    );

    if L.IntroduceFunction('greet') then
    begin
      F := L.Functions['greet'];
      F.Args.Clear;
      F.Args.PushStr('Delphi');
      if F.Execute then
        Writeln(F.Results[0].AsStr);
    end;
  finally
    L.Free;
  end;
end;
```

## API Notes

Important public types:

- `TLua` - owns a Lua state and exposes execution, globals, tables, functions, classes, libraries, and threads
- `TLuaValue` - typed wrapper for stack/registry values
- `TLuaTable` - table helper with typed indexers
- `TLuaFunction` - callable function wrapper with `Args` and `Results`
- `TLuaClassBlueprint` - Delphi-to-Lua class binding metadata
- `TLuaLibrary` - grouped functions/constants exposed as a Lua table
- `TLuaThread` - coroutine/thread wrapper

Preferred convenience aliases:

- `TLua.LoadFromFile(...)` mirrors `LoadSource(...)`
- `TLua.ExecuteText(...)` mirrors `ExecuteDirect(...)`
- `TLua.ScriptText` mirrors `ScriptSource.Text`

Class binding note:

- The historical property name `OnConstructon` is still available for compatibility.
- `OnConstruction` is now provided as the preferred spelling.

Error reporting note:

- `TLua` now exposes `LastErrorCode`, `LastErrorName`, `LastErrorMessage`, and `LastErrorLuaMessage`.
- `TLuaThread` exposes `LastErrorCode`, `LastErrorName`, `LastErrorCategory`, and `LastErrorLuaMessage`.
- `TLuaThread.LastErrorMessage` remains available as a compatibility alias for the Lua error text.
- These are updated for `Execute`, `ExecuteDirect`, thread execution, and other callback-driven failures handled by the framework.

Static-link note:

- `Source/LuaAPI.pas` supports `{$DEFINE LUA_STATIC}` for Win64 builds.
- In that mode the raw Lua API resolves against the statically linked object built from `StaticLua/minilua/minilua.c`.
- The shell scripts rebuild that object automatically when `DELPHI_DEFINES=LUA_STATIC` is set for a `Win64` build.
- `StaticLua/minilua/minilua.h` is now the project-owned Lua 5.5.0 header used by the static build.
- The current Win64 static path depends on `ucrtbase.dll`, but no longer on `lua55_64.dll`.
- On Windows 10 and Windows 11, `ucrtbase.dll` is normally present as part of the OS runtime rather than an extra Lua-specific dependency.

## Development Notes

Additional workflow details: [Docs/development.md](Docs/development.md)

Potential next steps: [Docs/roadmap.md](Docs/roadmap.md)

CI notes:

- A GitHub Actions workflow is provided in [.github/workflows/delphi-ci.yml](.github/workflows/delphi-ci.yml).
- It targets a self-hosted Windows runner labeled `delphi`.
- Windows-native CI helper scripts live in [ci/windows-build.ps1](ci/windows-build.ps1) and [ci/windows-test.ps1](ci/windows-test.ps1).

## License

MIT. See [LICENSE](LICENSE).
