#define LUA_IMPL

/*
 * Delphi can link the Win64 COFF object and run normal Lua code, but the
 * default bcc64x setjmp context crashes when Lua unwinds after an error.
 * Using the plain _setjmp(..., NULL) form keeps protected-call error
 * handling stable in the Delphi-linked executable.
 */
#if defined(_WIN64)
#define LUAI_THROW(L,c) longjmp((c)->b, 1)
#define LUA_TRY_SELECT(_1,_2,_3,_4,NAME,...) NAME
#define LUAI_TRY(...) LUA_TRY_SELECT(__VA_ARGS__, LUAI_TRY4, LUAI_TRY3)(__VA_ARGS__)
#define LUAI_TRY3(L,c,a) if (_setjmp((c)->b, ((void*)0)) == 0) { a }
#define LUAI_TRY4(L,c,f,ud) if (_setjmp((c)->b, ((void*)0)) == 0) ((f)(L, ud))
#define luai_jmpbuf jmp_buf
#endif

#if defined(__BORLANDC__)
#define __STDC__ 1
#endif

#if defined(__BORLANDC__)
#undef __STDC__
#endif

#define feof lua_feof_compat
#define system lua_system_compat
#define exit lua_exit_compat

#ifndef MINILUA_HEADER
#define MINILUA_HEADER "minilua.h"
#endif

#include MINILUA_HEADER

/* Avoid a Delphi linker collision on the intrinsic name "Abs". */
int abs(int x) {
  return x < 0 ? -x : x;
}

int lua_feof_compat(FILE *stream) {
  return feof(stream);
}

int lua_system_compat(const char *command) {
  return system(command);
}

void lua_exit_compat(int status) {
  exit(status);
}
