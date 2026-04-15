unit LuaStaticWin64CrtImports;

interface

const
  UCRTBASE_DLL = 'ucrtbase.dll';

procedure longjmp(Buf: Pointer; Value: Integer); cdecl; external UCRTBASE_DLL name 'longjmp';
function __intrinsic_setjmpex(Buf, Ctx: Pointer): Integer; cdecl; external UCRTBASE_DLL name '__intrinsic_setjmpex';

function strcmp(Str1, Str2: PAnsiChar): Integer; cdecl; external UCRTBASE_DLL name 'strcmp';
function strchr(Str: PAnsiChar; Chr: Integer): PAnsiChar; cdecl; external UCRTBASE_DLL name 'strchr';
function pow(Base, Exponent: Double): Double; cdecl; external UCRTBASE_DLL name 'pow';
function floor(Value: Double): Double; cdecl; external UCRTBASE_DLL name 'floor';
function strpbrk(Str1, Str2: PAnsiChar): PAnsiChar; cdecl; external UCRTBASE_DLL name 'strpbrk';
function strcpy(Dest: PAnsiChar; const Src: PAnsiChar): PAnsiChar; cdecl; external UCRTBASE_DLL name 'strcpy';
function localeconv: Pointer; cdecl; external UCRTBASE_DLL name 'localeconv';
function __stdio_common_vsprintf(Options: UInt64; Buffer: PAnsiChar; BufferCount: NativeUInt; const Format: PAnsiChar; Locale, Args: Pointer): Integer; cdecl; external UCRTBASE_DLL name '__stdio_common_vsprintf';
function fmod(X, Y: Double): Double; cdecl; external UCRTBASE_DLL name 'fmod';
function _errno: PInteger; cdecl; external UCRTBASE_DLL name '_errno';
function strerror(ErrNo: Integer): PAnsiChar; cdecl; external UCRTBASE_DLL name 'strerror';
function __acrt_iob_func(Index: Cardinal): Pointer; cdecl; external UCRTBASE_DLL name '__acrt_iob_func';
function fopen(const FileName, Mode: PAnsiChar): Pointer; cdecl; external UCRTBASE_DLL name 'fopen';
function freopen(const FileName, Mode: PAnsiChar; Stream: Pointer): Pointer; cdecl; external UCRTBASE_DLL name 'freopen';
function ferror(Stream: Pointer): Integer; cdecl; external UCRTBASE_DLL name 'ferror';
function fclose(Stream: Pointer): Integer; cdecl; external UCRTBASE_DLL name 'fclose';
function getc(Stream: Pointer): Integer; cdecl; external UCRTBASE_DLL name 'getc';
function fread(Ptr: Pointer; Size, Count: NativeUInt; Stream: Pointer): NativeUInt; cdecl; external UCRTBASE_DLL name 'fread';
function strstr(Str1, Str2: PAnsiChar): PAnsiChar; cdecl; external UCRTBASE_DLL name 'strstr';
function fflush(Stream: Pointer): Integer; cdecl; external UCRTBASE_DLL name 'fflush';
function getenv(const Name: PAnsiChar): PAnsiChar; cdecl; external UCRTBASE_DLL name 'getenv';
function _time64(TimeValue: PInt64): Int64; cdecl; external UCRTBASE_DLL name '_time64';
function ldexp(X: Double; Exponent: Integer): Double; cdecl; external UCRTBASE_DLL name 'ldexp';
function __mingw_strtod(const Str: PAnsiChar; EndPtr: PPointer): Double; cdecl; external UCRTBASE_DLL name 'strtod';
function frexp(X: Double; Exponent: PInteger): Double; cdecl; external UCRTBASE_DLL name 'frexp';
function strcoll(Str1, Str2: PAnsiChar): Integer; cdecl; external UCRTBASE_DLL name 'strcoll';
function __stdio_common_vfprintf(Options: UInt64; Stream: Pointer; const Format: PAnsiChar; Locale, Args: Pointer): Integer; cdecl; external UCRTBASE_DLL name '__stdio_common_vfprintf';
function fwrite(const Ptr: Pointer; Size, Count: NativeUInt; Stream: Pointer): NativeUInt; cdecl; external UCRTBASE_DLL name 'fwrite';
function isalnum(Ch: Integer): Integer; cdecl; external UCRTBASE_DLL name 'isalnum';
function isdigit(Ch: Integer): Integer; cdecl; external UCRTBASE_DLL name 'isdigit';
function toupper(Ch: Integer): Integer; cdecl; external UCRTBASE_DLL name 'toupper';
function fgets(Buffer: PAnsiChar; MaxCount: Integer; Stream: Pointer): PAnsiChar; cdecl; external UCRTBASE_DLL name 'fgets';
function _popen(const Command, Mode: PAnsiChar): Pointer; cdecl; external UCRTBASE_DLL name '_popen';
function tmpfile: Pointer; cdecl; external UCRTBASE_DLL name 'tmpfile';
procedure clearerr(Stream: Pointer); cdecl; external UCRTBASE_DLL name 'clearerr';
function ungetc(Character: Integer; Stream: Pointer): Integer; cdecl; external UCRTBASE_DLL name 'ungetc';
function isspace(Ch: Integer): Integer; cdecl; external UCRTBASE_DLL name 'isspace';
function isxdigit(Ch: Integer): Integer; cdecl; external UCRTBASE_DLL name 'isxdigit';
function _pclose(Stream: Pointer): Integer; cdecl; external UCRTBASE_DLL name '_pclose';
function fseek(Stream: Pointer; Offset: Int64; Origin: Integer): Integer; cdecl; external UCRTBASE_DLL name 'fseek';
function ftell(Stream: Pointer): Int64; cdecl; external UCRTBASE_DLL name 'ftell';
function setvbuf(Stream: Pointer; Buffer: PAnsiChar; Mode: Integer; Size: NativeUInt): Integer; cdecl; external UCRTBASE_DLL name 'setvbuf';
function acos(Value: Double): Double; cdecl; external UCRTBASE_DLL name 'acos';
function asin(Value: Double): Double; cdecl; external UCRTBASE_DLL name 'asin';
function atan2(Y, X: Double): Double; cdecl; external UCRTBASE_DLL name 'atan2';
function ceil(Value: Double): Double; cdecl; external UCRTBASE_DLL name 'ceil';
function log(Value: Double): Double; cdecl; external UCRTBASE_DLL name 'log';
function log10(Value: Double): Double; cdecl; external UCRTBASE_DLL name 'log10';
function tan(Value: Double): Double; cdecl; external UCRTBASE_DLL name 'tan';
function clock: Int64; cdecl; external UCRTBASE_DLL name 'clock';
function strftime(Buffer: PAnsiChar; MaxCount: NativeUInt; const Format: PAnsiChar; TimeInfo: Pointer): NativeUInt; cdecl; external UCRTBASE_DLL name 'strftime';
function remove(const FileName: PAnsiChar): Integer; cdecl; external UCRTBASE_DLL name 'remove';
function setlocale(Category: Integer; const Locale: PAnsiChar): PAnsiChar; cdecl; external UCRTBASE_DLL name 'setlocale';
function tmpnam(Buffer: PAnsiChar): PAnsiChar; cdecl; external UCRTBASE_DLL name 'tmpnam';
function _gmtime64(TimeValue: PInt64): Pointer; cdecl; external UCRTBASE_DLL name '_gmtime64';
function _localtime64(TimeValue: PInt64): Pointer; cdecl; external UCRTBASE_DLL name '_localtime64';
function _difftime64(Time1, Time2: Int64): Double; cdecl; external UCRTBASE_DLL name '_difftime64';
function _mktime64(TimeInfo: Pointer): Int64; cdecl; external UCRTBASE_DLL name '_mktime64';
function tolower(Ch: Integer): Integer; cdecl; external UCRTBASE_DLL name 'tolower';
function memchr(const Buffer: Pointer; Value: Integer; Count: NativeUInt): Pointer; cdecl; external UCRTBASE_DLL name 'memchr';
function isalpha(Ch: Integer): Integer; cdecl; external UCRTBASE_DLL name 'isalpha';
function iscntrl(Ch: Integer): Integer; cdecl; external UCRTBASE_DLL name 'iscntrl';
function isgraph(Ch: Integer): Integer; cdecl; external UCRTBASE_DLL name 'isgraph';
function islower(Ch: Integer): Integer; cdecl; external UCRTBASE_DLL name 'islower';
function ispunct(Ch: Integer): Integer; cdecl; external UCRTBASE_DLL name 'ispunct';
function isupper(Ch: Integer): Integer; cdecl; external UCRTBASE_DLL name 'isupper';

function __mingw_vsprintf(Buffer, Format: PAnsiChar; Args: Pointer): Integer; cdecl;
function __mingw_vfprintf(Stream: Pointer; const Format: PAnsiChar; Args: Pointer): Integer; cdecl;

implementation

function __mingw_vsprintf(Buffer, Format: PAnsiChar; Args: Pointer): Integer; cdecl;
begin
  Result := __stdio_common_vsprintf(0, Buffer, High(NativeUInt), Format, nil, Args);
end;

function __mingw_vfprintf(Stream: Pointer; const Format: PAnsiChar; Args: Pointer): Integer; cdecl;
begin
  Result := __stdio_common_vfprintf(0, Stream, Format, nil, Args);
end;

end.
