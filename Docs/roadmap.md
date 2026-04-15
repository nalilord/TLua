# Roadmap Ideas

Practical follow-up improvements worth considering:

- Introduce a DUnitX project as an optional richer test harness for IDE users.
- Add package/project templates for consumers who only want the library without the demo.
- Consider dynamic DLL loading to support custom Lua runtime names without recompilation.
- Add deeper regression cases around coroutine result handling, `LoadSource` I/O exceptions, and library mutation semantics.
- Decide whether older names like `LoadSource` and `ExecuteDirect` should eventually be documented as legacy aliases in favor of the clearer convenience names.
- If a hosted CI path is ever needed, investigate whether a compliant licensed Windows image can be used without relying on self-hosted runners.
