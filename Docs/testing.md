# Testing

## Command

Run the baseline regression suite with:

```bash
./test.sh Tests/TLuaTests.dpr Win64
./test.sh Tests/TLuaTests.dpr Win32
DELPHI_DEFINES=LUA_STATIC ./test.sh Tests/TLuaTests.dpr Win64
```

This builds `Tests/TLuaTests.dpr` and executes the resulting console program from `Bin/`.

The `LUA_STATIC` variant validates the Win64 static-link path in `Source/LuaAPI.pas` against the same regression suite used for the DLL-backed build.

## Current Coverage

- `TLua.ExecuteDirect` with globals
- Delphi callback registration through `RegisterMethod`
- `IntroduceFunction` and typed result access
- `TLuaTable` create/read scenarios
- `TLuaLibrary` constants and functions
- `TLuaThread` success and failure paths
- `ILuaErrorHandler` callbacks and `LastError*` properties
- `LoadSource` file loading and missing-file handling
- alias surface for `LoadFromFile`, `ExecuteText`, and `ScriptText`
- inherited class helpers, return propagation, and Lua-defined child methods
- allocator-backed memory growth via `MemoryUsage`
- class blueprints with property access, method calls, and construction callback wiring

## Design

The regression suite is intentionally self-contained and does not require DUnitX. That keeps the repo easy to build on a stock Delphi installation while still providing automated checks.

The tests return a non-zero exit code on failure, so the suite can be used from scripts or future CI jobs without extra adapters.
