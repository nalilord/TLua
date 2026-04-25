unit LuaCompat;

{$IFDEF FPC}
  {$MODE DELPHIUNICODE}
  {$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, IniFiles,
  LuaAPI
  {$IFDEF MSWINDOWS}, Winapi.Windows{$ENDIF};

type
  TLuaCallbackHandle = Pointer;

  TLuaPlatform = record
  public
    class function CallbackHandle(ACallback: lua_CFunction): TLuaCallbackHandle; overload; static; inline;
    class function CallbackHandle(AObject: TObject): TLuaCallbackHandle; overload; static; inline;
    class function CFunction(AHandle: TLuaCallbackHandle): lua_CFunction; static; inline;
    class procedure PushCallback(L: Plua_State; AHandle: TLuaCallbackHandle; ADispatcher: lua_CFunction); static;
    class procedure DebugOut(const AMessage: string); static;
    class procedure AtomicIncrement(var AValue: Integer); static; inline;
    class procedure AtomicDecrement(var AValue: Integer); static; inline;
    class function CreateGuidHex: string; static;
    class function NewHashedStringList(ADuplicates: TDuplicates; ACaseSensitive: Boolean): THashedStringList; static;
  end;

  TLuaCallbackThunk = class
  private
    FOwner: TObject;
    {$IFDEF MSWINDOWS}
    FEntryPoint: lua_CFunction;
    {$ENDIF}
  public
    constructor Create(AOwner: TObject; const Method: TMethod; ArgCount: Shortint);
    destructor Destroy; override;
    function Handle: TLuaCallbackHandle; inline;
  end;

implementation

{$IFDEF MSWINDOWS}
{$IFNDEF CPUX64}
function MakeCdeclCallback(const Method: TMethod; StackSize: Shortint): Pointer;
type
  PCallbackPush = ^TCallbackPush;

  TCallbackPush = packed record
    PushParmOps: Array [0..2] of Byte;
    PushParmVal: Shortint;
  end;

  PCallbackCall = ^TCallbackCall;

  TCallbackCall = packed record
    PushDataOps: Array [0..1] of Byte;
    PushDataVal: Pointer;
    CallCodeOps: Array [0..1] of Byte;
    CallCodeVal: Pointer;
    AddEspXXOps: Array [0..1] of Byte;
    AddEspXXVal: Shortint;
    Return: Byte;
  end;
var
  Size: Shortint;
  Loop: Shortint;
  Buff: Pointer;
begin
  if (StackSize < 0) OR
    (StackSize > High(Shortint) + 1 - 2 * SizeOf(Longword)) then
  begin
    Result:=nil;
    Exit;
  end;

  Result:=VirtualAlloc(nil, $100, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if Assigned(Result) then
  begin
    try
      Buff:=Result;
      if StackSize <= 0 then
      begin
        Size:=0;
      end else
      begin
        Size:=((StackSize - 1) div SizeOf(Longword) + 1) * SizeOf(Longword);
        for Loop:=1 to Size div SizeOf(Longword) do
        begin
          with PCallbackPush(Buff)^ do
          begin
            PushParmOps[0]:=$FF;
            PushParmOps[1]:=$74;
            PushParmOps[2]:=$24;
            PushParmVal:=Size;
          end;
          Inc(PCallbackPush(Buff));
        end;
      end;

      with PCallbackCall(Buff)^ do
      begin
        PushDataOps[0]:=$FF;
        PushDataOps[1]:=$35;
        PushDataVal:=Addr(Method.Data);
        CallCodeOps[0]:=$FF;
        CallCodeOps[1]:=$15;
        CallCodeVal:=Addr(Method.Code);
        AddEspXXOps[0]:=$83;
        AddEspXXOps[1]:=$C4;
        AddEspXXVal:=Size + SizeOf(Longword);
        Return:=$C3;
      end;
    except
      VirtualFree(Result, 0, MEM_RELEASE);
      Result:=nil;
    end;
  end;
end;

procedure FreeCallbackThunk(Callback: Pointer);
begin
  if Assigned(Callback) then
    VirtualFree(Callback, 0, MEM_RELEASE);
end;
{$ELSE}
function MakeCallback(const Method: TMethod; NumArgs: Shortint): Pointer;
const
  RegParamCount = 4;
  ShadowParamCount = 4;
  Size32Bit = 4;
  Size64Bit = 8;
  ShadowStack = ShadowParamCount * Size64Bit;
  SkipParamCount = RegParamCount - ShadowParamCount;
  StackSrsOffset = 3;
  c64stack: Array[0..14] of Byte = (
    $48, $81, $ec, 00, 00, 00, 00,
    $4c, $89, $8c, $24, ShadowStack, 00, 00, 00
  );
  CopySrcOffset = 4;
  CopyDstOffset = 4;
  c64copy: Array[0..15] of Byte = (
    $4c, $8b, $8c, $24,  00, 00, 00, 00,
    $4c, $89, $8c, $24, 00, 00, 00, 00
  );
  RegMethodOffset = 10;
  RegSelfOffset = 11;
  c64regs: Array[0..28] of Byte = (
    $4d, $89, $c1,
    $49, $89, $d0,
    $48, $89, $ca,
    $48, $b9, 00, 00, 00, 00, 00, 00, 00, 00,
    $48, $b8, 00, 00, 00, 00, 00, 00, 00, 00
  );
  c64jump: Array[0..2] of Byte = (
    $48, $ff, $e0
  );
  CallOffset = 6;
  c64call: Array[0..10] of Byte = (
    $48, $ff, $d0,
    $48, $81, $c4,  00, 00, 00, 00,
    $c3
  );
var
  I, Count, Size, Offset: Integer;
  Ptr, Ptr2, CallbackPtr: PByte;
begin
  Count:=SizeOf(c64regs);
  if NumArgs >= RegParamCount then
    Inc(Count, SizeOf(c64stack) + (NumArgs - RegParamCount) * SizeOf(c64copy) + SizeOf(c64call))
  else
    Inc(Count, SizeOf(c64jump));

  CallbackPtr:=VirtualAlloc(nil, Count, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  Ptr:=CallbackPtr;

  Size:=0;
  if NumArgs >= RegParamCount then
  begin
    Size:=(1 + ((NumArgs + 1 - SkipParamCount) div 2) * 2) * Size64Bit;

    Ptr2:=Ptr;
    System.Move(c64stack, Ptr^, SizeOf(c64stack));
    Inc(Ptr, StackSrsOffset);
    System.Move(Size, Ptr^, Size32Bit);
    Ptr:=Ptr2;
    Inc(Ptr, SizeOf(c64stack));

    for I:=0 to NumArgs - RegParamCount - 1 do
    begin
      Ptr2:=Ptr;
      System.Move(c64copy, Ptr^, SizeOf(c64copy));
      Inc(Ptr, CopySrcOffset);
      Offset:=Size + (I + ShadowParamCount + 1) * Size64Bit;
      System.Move(Offset, Ptr^, Size32Bit);
      Inc(Ptr, CopyDstOffset + Size32Bit);
      Offset:=(I + ShadowParamCount + 1) * Size64Bit;
      System.Move(Offset, Ptr^, Size32Bit);
      Ptr:=Ptr2;
      Inc(Ptr, SizeOf(c64copy));
    end;
  end;

  Ptr2:=Ptr;
  System.Move(c64regs, Ptr^, SizeOf(c64regs));
  Inc(Ptr, RegSelfOffset);
  System.Move(Method.Data, Ptr^, SizeOf(Method.Data));
  Inc(Ptr, RegMethodOffset);
  System.Move(Method.Code, Ptr^, SizeOf(Method.Code));
  Ptr:=Ptr2;
  Inc(Ptr, SizeOf(c64regs));

  if NumArgs < RegParamCount then
  begin
    System.Move(c64jump, Ptr^, SizeOf(c64jump));
  end else
  begin
    System.Move(c64call, Ptr^, SizeOf(c64call));
    Inc(Ptr, CallOffset);
    System.Move(Size, Ptr^, Size32Bit);
  end;

  Result:=CallbackPtr;
end;

procedure FreeCallbackThunk(Callback: Pointer);
begin
  if Assigned(Callback) then
    VirtualFree(Callback, 0, MEM_RELEASE);
end;
{$ENDIF}
{$ENDIF}

{ TLuaPlatform }

class function TLuaPlatform.CallbackHandle(ACallback: lua_CFunction): TLuaCallbackHandle;
var
  CallbackPtr: Pointer absolute ACallback;
begin
  Result:=CallbackPtr;
end;

class function TLuaPlatform.CallbackHandle(AObject: TObject): TLuaCallbackHandle;
begin
  Result:=AObject;
end;

class function TLuaPlatform.CFunction(AHandle: TLuaCallbackHandle): lua_CFunction;
var
  Callback: lua_CFunction absolute AHandle;
begin
  Result:=Callback;
end;

class procedure TLuaPlatform.PushCallback(L: Plua_State; AHandle: TLuaCallbackHandle; ADispatcher: lua_CFunction);
begin
  {$IFDEF MSWINDOWS}
  lua_pushcfunction(L, CFunction(AHandle));
  {$ELSE}
  lua_pushlightuserdata(L, AHandle);
  lua_pushcclosure(L, ADispatcher, 1);
  {$ENDIF}
end;

class procedure TLuaPlatform.DebugOut(const AMessage: string);
begin
  {$IFDEF MSWINDOWS}
  OutputDebugString(PChar(AMessage));
  {$ELSE}
  WriteLn(AMessage);
  {$ENDIF}
end;

class procedure TLuaPlatform.AtomicIncrement(var AValue: Integer);
begin
  Inc(AValue);
end;

class procedure TLuaPlatform.AtomicDecrement(var AValue: Integer);
begin
  Dec(AValue);
end;

class function TLuaPlatform.CreateGuidHex: string;
const
  HexChars: array[0..15] of Char = '0123456789ABCDEF';
var
  Guid: TGUID;
  Bytes: PByte;
  I: Integer;
begin
  SetLength(Result, SizeOf(TGUID) * 2);
  if CreateGUID(Guid) = 0 then
  begin
    Bytes:=@Guid;
    for I:=0 to SizeOf(TGUID) - 1 do
    begin
      Result[I * 2 + 1]:=HexChars[Bytes^ shr 4];
      Result[I * 2 + 2]:=HexChars[Bytes^ AND $0F];
      Inc(Bytes);
    end;
  end else
  begin
    Result:=StringOfChar('0', SizeOf(TGUID) * 2);
  end;
end;

class function TLuaPlatform.NewHashedStringList(ADuplicates: TDuplicates; ACaseSensitive: Boolean): THashedStringList;
begin
  {$IFDEF FPC}
  Result:=THashedStringList.Create;
  Result.Duplicates:=ADuplicates;
  Result.CaseSensitive:=ACaseSensitive;
  {$ELSE}
  Result:=THashedStringList.Create(ADuplicates, True, False);
  {$ENDIF}
end;

{ TLuaCallbackThunk }

constructor TLuaCallbackThunk.Create(AOwner: TObject; const Method: TMethod; ArgCount: Shortint);
begin
  inherited Create;

  FOwner:=AOwner;
  {$IFDEF MSWINDOWS}
  {$IFNDEF CPUX64}
  FEntryPoint:=lua_CFunction(MakeCdeclCallback(Method, SizeOf(NativeInt)));
  {$ELSE}
  FEntryPoint:=lua_CFunction(MakeCallback(Method, ArgCount));
  {$ENDIF}
  {$ENDIF}
end;

destructor TLuaCallbackThunk.Destroy;
begin
  {$IFDEF MSWINDOWS}
  FreeCallbackThunk(TLuaPlatform.CallbackHandle(FEntryPoint));
  {$ENDIF}
  inherited;
end;

function TLuaCallbackThunk.Handle: TLuaCallbackHandle;
begin
  {$IFDEF MSWINDOWS}
  Result:=TLuaPlatform.CallbackHandle(FEntryPoint);
  {$ELSE}
  Result:=TLuaPlatform.CallbackHandle(FOwner);
  {$ENDIF}
end;

end.
