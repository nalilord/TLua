# Benchmarking

## Command

Build and run the benchmark suite with:

```bash
./bench.sh Win64
./bench.sh Win32
./bench.sh Linux64
```

Optional filters:

```bash
./bench.sh Win64 --scale=200000
./bench.sh Win64 --filter=callback
./bench.sh Win64 --scale=50000 --filter="cached invoker"
./bench.sh Win64 --scale=50000 --repeat=5 --filter="native class invoke"
```

## Current Coverage

- repeated `ExecuteDirect` no-op cost
- Lua -> Delphi function callback throughput
- Lua -> Delphi class method callback throughput
- Delphi -> Lua function invoke throughput
- Delphi -> Lua class method invoke throughput
- Delphi -> Lua class method invoke throughput with a cached invoker
- Delphi -> native class method invoke throughput
- Delphi -> native class method invoke throughput with a cached invoker

## Reading The Numbers

The suite prints:

- `ops` - logical benchmark operations
- `total` - wall-clock runtime for the measured loop
- `ns/op` - approximate nanoseconds per logical operation
- `ops/s` - operations per second
- `checksum` - a correctness guard so the benchmark body is actually exercised

## Usage Notes

- Use the suite for relative comparisons between TLua revisions, not for absolute claims across machines.
- Use `--repeat=N` when you want a steadier sample without editing the benchmark project.
- Keep platform, compiler, and Lua mode (`DLL` vs `LUA_STATIC`) consistent when comparing runs.
- Prefer a warm machine and close other heavy workloads before collecting numbers.
- The callback and invoker benchmarks are the most relevant ones for engine-side scripting hot paths.
