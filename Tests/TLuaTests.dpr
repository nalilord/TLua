program TLuaTests;

{$APPTYPE CONSOLE}
{$IFDEF FPC}
  {$MODE DELPHIUNICODE}
  {$H+}
{$ENDIF}

uses
  SysUtils,
  Math,
  Variants,
  LuaTestCompat,
  Lua,
  LuaAPI;

type
  ETestFailure = class(Exception);

  TTestProc = procedure of object;

  TErrorHandler = class(TInterfacedObject, ILuaErrorHandler)
  public
    LoadCount: Integer;
    ExecutionCount: Integer;
    LastName: string;
    LastMessage: string;
    LastCode: Integer;
    LastLuaMessage: string;
    procedure OnScriptLoadError(Name, Message: WideString; Code: Integer; LuaMessage: WideString); stdcall;
    procedure OnScriptExecutionError(Name, Message: WideString; Code: Integer; LuaMessage: WideString); stdcall;
  end;

type
  TLuaRegressionSuite = class
  private
    FAssertionCount: Integer;
    FBaseSpeakCount: Integer;
    FConstructed: Boolean;
    FProxyMethodCount: Integer;
    FProxyReleaseCount: Integer;
    FStoredName: string;
    procedure AssertTrue(ACondition: Boolean; const AMessage: string);
    procedure AssertEqual(const AExpected, AActual, AMessage: string); overload;
    procedure AssertEqual(AExpected, AActual: Int64; const AMessage: string); overload;
    procedure AssertEqual(AExpected, AActual: Double; const AMessage: string); overload;
    procedure AssertEqual(AExpected, AActual: Boolean; const AMessage: string); overload;
    procedure RunTest(const AName: string; ATest: TTestProc);
    procedure SumFunction(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
    procedure LibraryJoin(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
    procedure LibraryJoinReplacement(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
    procedure InvokeAttachFromArgs(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
    procedure ProxyLifetimePing(Sender: TLua; Clazz: TLuaClass; Method: TLuaClassMethod; Args: TLuaArgs; Results: TLuaResults);
    procedure ProxyLifetimeRelease(Sender: TLua; Clazz: TLuaClass; var UserClass: TObject);
    procedure BaseSpeak(Sender: TLua; Clazz: TLuaClass; Method: TLuaClassMethod; Args: TLuaArgs; Results: TLuaResults);
    procedure GreeterConstruct(Sender: TLua; Blueprint: TLuaClassBlueprint; Args: TLuaArgs; var UserClass: TObject; var Allow: Boolean);
    procedure GreeterDescribe(Sender: TLua; Clazz: TLuaClass; Method: TLuaClassMethod; Args: TLuaArgs; Results: TLuaResults);
    procedure GreeterNameGet(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassProperty; Value: TLuaValue);
    procedure GreeterNameSet(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassProperty; Value: TLuaValue);
    procedure TestGlobalsAndExecuteDirect;
    procedure TestRegisteredFunction;
    procedure TestIntroduceFunction;
    procedure TestTables;
    procedure TestLibraries;
    procedure TestThreads;
    procedure TestErrorHandling;
    procedure TestLoadSource;
    procedure TestInheritance;
    procedure TestCallbackArgTableInvoke;
    procedure TestMemoryUsage;
    procedure TestCopyTable;
    procedure TestClassBlueprintBinding;
    procedure TestClassDestroyLifetime;
  public
    procedure Run;
  end;

procedure TErrorHandler.OnScriptLoadError(Name, Message: WideString; Code: Integer; LuaMessage: WideString);
begin
  Inc(LoadCount);
  LastName:=Name;
  LastMessage:=Message;
  LastCode:=Code;
  LastLuaMessage:=LuaMessage;
end;

procedure TErrorHandler.OnScriptExecutionError(Name, Message: WideString; Code: Integer; LuaMessage: WideString);
begin
  Inc(ExecutionCount);
  LastName:=Name;
  LastMessage:=Message;
  LastCode:=Code;
  LastLuaMessage:=LuaMessage;
end;

procedure TLuaRegressionSuite.AssertTrue(ACondition: Boolean; const AMessage: string);
begin
  Inc(FAssertionCount);
  if NOT ACondition then
    raise ETestFailure.Create(AMessage);
end;

procedure TLuaRegressionSuite.AssertEqual(const AExpected, AActual, AMessage: string);
begin
  AssertTrue(AExpected = AActual, Format('%s Expected "%s" but got "%s".', [AMessage, AExpected, AActual]));
end;

procedure TLuaRegressionSuite.AssertEqual(AExpected, AActual: Int64; const AMessage: string);
begin
  AssertTrue(
    AExpected = AActual,
    Format('%s Expected %d but got %d.', [AMessage, AExpected, AActual])
  );
end;

procedure TLuaRegressionSuite.AssertEqual(AExpected, AActual: Double; const AMessage: string);
begin
  AssertTrue(
    SameValue(AExpected, AActual, 1E-9),
    Format('%s Expected %.12f but got %.12f.', [AMessage, AExpected, AActual])
  );
end;

procedure TLuaRegressionSuite.AssertEqual(AExpected, AActual: Boolean; const AMessage: string);
begin
  AssertTrue(
    AExpected = AActual,
    Format('%s Expected %s but got %s.', [AMessage, BoolToStr(AExpected, True), BoolToStr(AActual, True)])
  );
end;

procedure TLuaRegressionSuite.RunTest(const AName: string; ATest: TTestProc);
begin
  Write(Format('[RUN ] %s ... ', [AName]));
  ATest;
  Writeln('ok');
end;

procedure TLuaRegressionSuite.SumFunction(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
begin
  AssertTrue(Args.Check([ltNumber, ltNumber]), 'sum should receive two numbers.');
  Results.PushFloat(Args[0].AsFloat + Args[1].AsFloat);
end;

procedure TLuaRegressionSuite.LibraryJoin(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
begin
  AssertTrue(Args.Check([ltString, ltString]), 'cfg.join should receive two strings.');
  Results.PushStr(Args[0].AsStr + ':' + Args[1].AsStr);
end;

procedure TLuaRegressionSuite.LibraryJoinReplacement(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
begin
  AssertTrue(Args.Check([ltString, ltString]), 'cfg.join replacement should receive two strings.');
  Results.PushStr(UpperCase(Args[0].AsStr) + '|' + UpperCase(Args[1].AsStr));
end;

procedure TLuaRegressionSuite.InvokeAttachFromArgs(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
var
  Invoker: TLuaClassMethodInvoker;
begin
  Invoker:=nil;
  AssertTrue(Args.Check([ltClass, ltClass, ltTable]), 'invoke_attach should receive class, class, table.');
  AssertTrue(Args[0].AsClass.TryInvoke('onAttach', Invoker), 'onAttach should resolve from callback target class.');
  try
    Invoker.Args.PushClass(Args[1].AsClass);
    Invoker.Args.PushStr('runtime_state');
    Invoker.Args.PushValue(Args[2]);
    Results.PushBool(Invoker.Execute);
  finally
    Invoker.Free;
  end;
end;

procedure TLuaRegressionSuite.ProxyLifetimePing(Sender: TLua; Clazz: TLuaClass; Method: TLuaClassMethod; Args: TLuaArgs; Results: TLuaResults);
begin
  Inc(FProxyMethodCount);
  Results.PushStr('pong');
end;

procedure TLuaRegressionSuite.ProxyLifetimeRelease(Sender: TLua; Clazz: TLuaClass; var UserClass: TObject);
begin
  Inc(FProxyReleaseCount);
end;

procedure TLuaRegressionSuite.BaseSpeak(Sender: TLua; Clazz: TLuaClass; Method: TLuaClassMethod; Args: TLuaArgs; Results: TLuaResults);
begin
  Inc(FBaseSpeakCount);
  Results.PushStr('base');
end;

procedure TLuaRegressionSuite.GreeterConstruct(Sender: TLua; Blueprint: TLuaClassBlueprint; Args: TLuaArgs; var UserClass: TObject; var Allow: Boolean);
begin
  FConstructed:=True;
end;

procedure TLuaRegressionSuite.GreeterDescribe(Sender: TLua; Clazz: TLuaClass; Method: TLuaClassMethod; Args: TLuaArgs; Results: TLuaResults);
begin
  Results.PushStr('Hello, ' + FStoredName);
end;

procedure TLuaRegressionSuite.GreeterNameGet(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassProperty; Value: TLuaValue);
begin
  Value.AsStr:=FStoredName;
end;

procedure TLuaRegressionSuite.GreeterNameSet(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassProperty; Value: TLuaValue);
begin
  FStoredName:=Value.AsStr;
end;

procedure TLuaRegressionSuite.TestGlobalsAndExecuteDirect;
var
  LuaState: TLua;
begin
  LuaState:=TLua.Create;
  try
    LuaState.Globals['answer']:=41;
    AssertEqual(41, VarAsType(LuaState.Globals['answer'], varInt64), 'Initial global round-trip failed.');

    AssertTrue(
      LuaState.ExecuteDirect('answer = answer + 1'),
      'ExecuteDirect should succeed for a valid script.'
    );

    AssertEqual(42, VarAsType(LuaState.Globals['answer'], varInt64), 'Lua should update Delphi-managed globals.');
  finally
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.TestRegisteredFunction;
var
  LuaState: TLua;
begin
  LuaState:=TLua.Create;
  try
    LuaState.RegisterMethod('sum', SumFunction);

    AssertTrue(
      LuaState.ExecuteDirect('sum_result = sum(2.5, 3.25)'),
      'Lua callback execution should succeed.'
    );

    AssertEqual(
      5.75,
      VarAsType(LuaState.Globals['sum_result'], varDouble),
      'Registered Delphi function returned an unexpected result.'
    );
  finally
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.TestIntroduceFunction;
var
  LuaState: TLua;
  Func: TLuaFunction;
begin
  LuaState:=TLua.Create;
  try
    AssertTrue(
      LuaState.ExecuteDirect('function greet(name) return true, "Hello, " .. name end'),
      'Test function declaration failed.'
    );

    AssertTrue(LuaState.IntroduceFunction('greet'), 'First IntroduceFunction call should succeed.');
    AssertTrue(NOT LuaState.IntroduceFunction('greet'), 'Second IntroduceFunction call should report the cached function.');

    Func:=LuaState.Functions['greet'];
    AssertTrue(Assigned(Func), 'Introduced function should be accessible through TLua.Functions.');

    Func.Args.Clear;
    Func.Args.PushStr('Delphi');
    AssertTrue(Func.Execute, 'TLuaFunction.Execute should succeed.');
    AssertTrue(Func.Results.Check([ltBoolean, ltString], True), 'Unexpected function result signature.');
    AssertEqual(True, Func.Results[0].AsBool, 'Unexpected first function result.');
    AssertEqual('Hello, Delphi', Func.Results[1].AsStr, 'Unexpected second function result.');
  finally
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.TestTables;
var
  LuaState: TLua;
  Table: TLuaTable;
begin
  LuaState:=TLua.Create;
  try
    Table:=LuaState.NewTable('settings');
    try
      Table.Add('name', 'TLua');
      Table.Add('enabled', True);
      Table.Add('retries', 3);
    finally
      Table.Free;
    end;

    Table:=LuaState.Tables['settings'];
    try
      AssertTrue(Assigned(Table), 'Expected to retrieve the table from globals.');
      AssertEqual('TLua', Table.AsStr['name'], 'Unexpected string value in table.');
      AssertEqual(True, Table.AsBool['enabled'], 'Unexpected boolean value in table.');
      AssertEqual(3, Table.AsInt['retries'], 'Unexpected integer value in table.');
    finally
      Table.Free;
    end;
  finally
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.TestLibraries;
var
  LuaState: TLua;
  Lib: TLuaLibrary;
begin
  LuaState:=TLua.Create;
  try
    Lib:=LuaState.NewLibrary('cfg');
    Lib.AddConstant('MODE', 'test');
    Lib.AddConstant('MODE', 'prod');
    Lib.AddConstant('RETRIES', 2);
    Lib.AddFunction('join', LibraryJoin);
    Lib.AddFunction('join', LibraryJoinReplacement);
    Lib.Register;

    AssertTrue(
      LuaState.ExecuteDirect(
        'library_mode = cfg.MODE ' +
        'library_retries = cfg.RETRIES ' +
        'library_join = cfg.join("left", "right")'
      ),
      'Library-backed script should execute successfully.'
    );

    AssertEqual('prod', VarToStr(LuaState.Globals['library_mode']), 'Unexpected library constant value.');
    AssertEqual(2, VarAsType(LuaState.Globals['library_retries'], varInt64), 'Unexpected numeric library constant.');
    AssertEqual('LEFT|RIGHT', VarToStr(LuaState.Globals['library_join']), 'Unexpected library function result.');
  finally
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.TestThreads;
var
  LuaState: TLua;
  Thread: TLuaThread;
begin
  LuaState:=TLua.Create;
  try
    Thread:=LuaState.NewThread;
    try
      AssertTrue(
        Thread.Execute('thread_answer = 6 * 7'),
        'Thread execution should succeed.'
      );

      AssertEqual(42, VarAsType(LuaState.Globals['thread_answer'], varInt64), 'Thread did not update the shared Lua state.');
      AssertEqual(0, Thread.Stack.Top, 'Thread stack should be clear after successful execution.');

      AssertTrue(
        Thread.Execute('return 1, 2, 3'),
        'Thread should handle return values without leaking stack entries.'
      );

      AssertEqual(0, Thread.Stack.Top, 'Thread stack should be clear after returned values are discarded.');

      AssertTrue(
        NOT Thread.Execute('error("thread-fail")'),
        'Thread runtime error should be reported as failure.'
      );

      AssertTrue(Thread.LastErrorCode <> LUA_OK, 'Thread error code should be populated after failure.');
      AssertTrue(Pos('<thread>', Thread.LastErrorName) > 0, 'Thread error name should include the thread context.');
      AssertEqual('Runtime error', Thread.LastErrorCategory, 'Thread error category should be captured.');
      AssertTrue(Pos('thread-fail', Thread.LastErrorMessage) > 0, 'Thread error message should include the Lua failure text.');
      AssertTrue(Pos('thread-fail', Thread.LastErrorLuaMessage) > 0, 'Thread should expose the Lua error text explicitly.');
      AssertEqual(0, Thread.Stack.Top, 'Thread stack should be clear after failed execution.');
      AssertEqual('Runtime error', LuaState.LastErrorMessage, 'TLua should expose the thread error category.');
      AssertTrue(Pos('thread-fail', LuaState.LastErrorLuaMessage) > 0, 'TLua should retain the thread Lua error text.');
    finally
      Thread.Free;
    end;
  finally
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.TestErrorHandling;
var
  LuaState: TLua;
  Handler: TErrorHandler;
begin
  LuaState:=TLua.Create;
  Handler:=TErrorHandler.Create;
  try
    LuaState.ScriptName:='ErrorSuite';
    LuaState.RegisterErrorHandler(Handler);

    LuaState.ScriptText:='function broken(';
    AssertTrue(NOT LuaState.Execute, 'Invalid source should fail during load.');
    AssertEqual(1, Handler.LoadCount, 'Load error handler should be called once.');
    AssertEqual('ErrorSuite', LuaState.LastErrorName, 'Last error name should reflect the script context.');
    AssertEqual('Syntax error during precompilation', LuaState.LastErrorMessage, 'Unexpected load error category.');
    AssertTrue(LuaState.LastErrorCode <> LUA_OK, 'Load error code should be set.');
    AssertTrue(LuaState.LastErrorLuaMessage <> '', 'Load error should preserve the Lua message.');

    AssertTrue(NOT LuaState.ExecuteText('error("direct-fail")'), 'ExecuteText runtime error should fail.');
    AssertEqual(1, Handler.ExecutionCount, 'ExecuteDirect should notify the execution error handler.');
    AssertEqual('ErrorSuite', LuaState.LastErrorName, 'ExecuteDirect should reuse the script name when present.');
    AssertEqual('Runtime error', LuaState.LastErrorMessage, 'Unexpected direct execution error category.');
    AssertTrue(Pos('direct-fail', LuaState.LastErrorLuaMessage) > 0, 'Direct execution Lua error text was not preserved.');

    LuaState.ScriptText:='error("execute-fail")';
    AssertTrue(NOT LuaState.Execute, 'Runtime source should fail during execution.');
    AssertEqual(2, Handler.ExecutionCount, 'Script execution errors should be tracked separately from load errors.');
    AssertTrue(Pos('execute-fail', Handler.LastLuaMessage) > 0, 'Error handler should receive the Lua execution message.');
  finally
    LuaState.UnregisterErrorHandler(Handler);
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.TestLoadSource;
var
  LuaState: TLua;
  TempFile: string;
  MissingFile: string;
begin
  LuaState:=TLua.Create;
  TempFile:=TempFilePath('tlua-loadsource-test.lua');
  MissingFile:=TempFilePath('tlua-loadsource-missing.lua');

  try
    WriteAllText(TempFile, 'loaded_value = 314');

    AssertTrue(LuaState.LoadFromFile(TempFile), 'LoadFromFile should accept an existing script file.');
    AssertEqual('tlua-loadsource-test.lua', LuaState.ScriptName, 'LoadSource should derive ScriptName from the file name.');
    AssertEqual('', LuaState.LastErrorMessage, 'Successful LoadSource should leave no last error message.');
    AssertTrue(Pos('loaded_value = 314', LuaState.ScriptText) > 0, 'ScriptText should mirror the loaded file contents.');
    AssertTrue(LuaState.Execute, 'Loaded script should execute successfully.');
    AssertEqual(314, VarAsType(LuaState.Globals['loaded_value'], varInt64), 'Loaded script did not execute as expected.');

    if FileExistsCompat(MissingFile) then
      DeleteFileCompat(MissingFile);

    AssertTrue(NOT LuaState.LoadFromFile(MissingFile), 'LoadFromFile should fail for a missing file.');
    AssertEqual(LUA_ERRFILE, LuaState.LastErrorCode, 'Missing file should map to LUA_ERRFILE.');
    AssertEqual('Script file not found', LuaState.LastErrorMessage, 'Unexpected missing-file error message.');
    AssertTrue(Pos('tlua-loadsource-missing.lua', LuaState.LastErrorName) > 0, 'Missing file name should be preserved.');
  finally
    if FileExistsCompat(TempFile) then
      DeleteFileCompat(TempFile);
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.TestInheritance;
var
  LuaState: TLua;
  Blueprint: TLuaClassBlueprint;
  ChildBlueprint: TLuaClassBlueprint;
  ChildInstance: TLuaClass;
  Invoker: TLuaClassMethodInvoker;
  MethodType: TLuaCallType;
begin
  LuaState:=TLua.Create;
  try
    FBaseSpeakCount:=0;
    Blueprint:=LuaState.NewClass('BaseGreeter');
    Blueprint.AddMethod('speak', BaseSpeak);
    Blueprint.Register;
    ChildBlueprint:=Blueprint.Inherit('ChildGreeter');
    ChildBlueprint.Register;

    AssertTrue(
      LuaState.ExecuteDirect(
        'function ChildGreeter:speak() ' +
        '  local explicit = inherited(self, "speak") ' +
        '  local implicit = self:inherited("speak") ' +
        '  return explicit .. "|" .. implicit ' +
        'end ' +
        'local c = ChildGreeter:new() ' +
        'inherit_message = c:speak()'
      ),
      'Inheritance helper script should execute successfully. ' +
      LuaState.LastErrorMessage + ' / ' + LuaState.LastErrorLuaMessage
    );

    AssertEqual('base|base', VarToStr(LuaState.Globals['inherit_message']), 'Inherited helper calls should return the parent result.');
    AssertEqual(2, FBaseSpeakCount, 'Both inherited helper forms should invoke the parent method.');

    ChildInstance:=ChildBlueprint.Construct;
    try
      AssertTrue(Assigned(ChildInstance), 'Native code should be able to construct the Lua child blueprint.');
      AssertTrue(ChildInstance.HasMethod('speak'), 'HasMethod should report Lua-defined child methods.');
      AssertTrue(ChildInstance.HasLuaMethod('speak'), 'HasLuaMethod should report Lua-defined child methods.');
      AssertTrue(NOT ChildInstance.HasNativeMethod('speak'), 'HasNativeMethod should stay false for Lua-defined child methods.');
      AssertTrue(ChildInstance.TryGetMethodType('speak', MethodType), 'TryGetMethodType should resolve Lua-defined child methods.');
      AssertTrue(MethodType = mtLua, 'Lua-defined child methods should report mtLua.');
      AssertTrue(NOT ChildInstance.TryGetMethodType('missing', MethodType), 'TryGetMethodType should fail for unknown methods.');
      AssertTrue(ChildInstance.Methods['speak'] = nil, 'Methods[] should remain native-only for compatibility.');
      AssertTrue(ChildInstance.TryInvoke('speak', Invoker), 'TryInvoke should resolve Lua-defined child methods.');
      try
        AssertTrue(Invoker.Execute, 'Lua-defined child method invoker should execute successfully.');
        AssertTrue(Invoker.Results.Count > 0, 'Lua-defined child method should return a result.');
        AssertEqual('base|base', Invoker.Results[0].AsStr, 'Native invocation of Lua-defined child method returned the wrong result.');
      finally
        Invoker.Free;
      end;
    finally
      ChildInstance.Free;
    end;
  finally
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.TestCallbackArgTableInvoke;
var
  LuaState: TLua;
  BaseBlueprint: TLuaClassBlueprint;
  EntityBlueprint: TLuaClassBlueprint;
  ExecuteOk: Boolean;
begin
  LuaState:=TLua.Create;
  try
    BaseBlueprint:=LuaState.NewClass('BaseComponent');
    BaseBlueprint.Register;

    EntityBlueprint:=LuaState.NewClass('EntityProxy');
    EntityBlueprint.Register;

    LuaState.RegisterMethod('invoke_attach', InvokeAttachFromArgs);

    ExecuteOk:=LuaState.ExecuteDirect(
      'RuntimeState = class(BaseComponent) ' +
      'function RuntimeState:onAttach(entity, name, props) ' +
      '  callback_attached_name = name ' +
      '  callback_attached_label = props.label ' +
      'end ' +
      'local instance = RuntimeState:new() ' +
      'local entity = EntityProxy:new() ' +
      'callback_ok = invoke_attach(instance, entity, { label = "player_hp" })'
    );

    AssertTrue(
      ExecuteOk,
      Format(
        'Callback arg-table invoke should execute successfully. code=%d name="%s" message="%s" lua="%s"',
        [
          LuaState.LastErrorCode,
          LuaState.LastErrorName,
          LuaState.LastErrorMessage,
          LuaState.LastErrorLuaMessage
        ]
      )
    );

    AssertEqual(
      True,
      VarAsType(LuaState.Globals['callback_ok'], varBoolean),
      'Callback arg-table invoke should report success.'
    );
    AssertEqual(
      'runtime_state',
      VarToStr(LuaState.Globals['callback_attached_name']),
      'Forwarded string argument should reach onAttach.'
    );
    AssertEqual(
      'player_hp',
      VarToStr(LuaState.Globals['callback_attached_label']),
      'Forwarded callback table should reach onAttach.'
    );
  finally
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.TestMemoryUsage;
var
  LuaState: TLua;
  BeforeUsage: NativeInt;
  AfterUsage: NativeInt;
begin
  LuaState:=TLua.Create;
  try
    BeforeUsage:=LuaState.MemoryUsage;
    AssertTrue(BeforeUsage > 0, 'Lua allocator should report memory usage after state initialization.');

    AssertTrue(
      LuaState.ExecuteDirect('memory_blob = string.rep("x", 200000)'),
      'Memory usage setup script should succeed.'
    );

    AfterUsage:=LuaState.MemoryUsage;
    AssertTrue(AfterUsage > BeforeUsage, 'MemoryUsage should increase after retaining a large Lua string.');
  finally
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.TestCopyTable;
var
  LuaState: TLua;
  SourceTable: Integer;
  DestTable: Integer;
  MetaTable: Integer;
begin
  LuaState:=TLua.Create;
  try
    AssertTrue(
      LuaState.ExecuteDirect(
        'source = { value = 5, nested = { child = "base" } } ' +
        'source.shared = source.nested ' +
        'source.loop = source ' +
        'source.__index = { lookup = "meta" } ' +
        'source.__index.self = source.__index ' +
        'dest = {} ' +
        'meta = {}'
      ),
      'CopyTable setup script should execute successfully.'
    );

    SourceTable:=LuaState.Stack.GetGlobal('source');
    DestTable:=LuaState.Stack.GetGlobal('dest');
    MetaTable:=LuaState.Stack.GetGlobal('meta');
    try
      LuaState.Stack.CopyTable(SourceTable, DestTable, MetaTable);
    finally
      LuaState.Stack.Pop(3);
    end;

    AssertTrue(
      LuaState.ExecuteDirect(
        'dest.nested.child = "copy" ' +
        'meta.__index.lookup = "meta-copy" ' +
        'copied_nested = dest.nested.child ' +
        'source_nested = source.nested.child ' +
        'copied_alias = dest.shared == dest.nested ' +
        'copied_loop = dest.loop == dest ' +
        'copied_meta = meta.__index.lookup ' +
        'source_meta = source.__index.lookup ' +
        'copied_meta_loop = meta.__index.self == meta.__index'
      ),
      'CopyTable verification script should execute successfully.'
    );

    AssertEqual('copy', VarToStr(LuaState.Globals['copied_nested']), 'Destination nested table should be writable.');
    AssertEqual('base', VarToStr(LuaState.Globals['source_nested']), 'CopyTable should deep-copy nested tables.');
    AssertEqual(True, VarAsType(LuaState.Globals['copied_alias'], varBoolean), 'CopyTable should preserve repeated table references.');
    AssertEqual(True, VarAsType(LuaState.Globals['copied_loop'], varBoolean), 'CopyTable should preserve self-referential tables.');
    AssertEqual('meta-copy', VarToStr(LuaState.Globals['copied_meta']), 'Meta table nested values should be writable after copy.');
    AssertEqual('meta', VarToStr(LuaState.Globals['source_meta']), 'CopyTable should deep-copy nested meta tables.');
    AssertEqual(True, VarAsType(LuaState.Globals['copied_meta_loop'], varBoolean), 'CopyTable should preserve cycles inside copied meta tables.');
  finally
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.TestClassBlueprintBinding;
var
  LuaState: TLua;
  Blueprint: TLuaClassBlueprint;
begin
  FConstructed:=False;
  FStoredName:='World';

  LuaState:=TLua.Create;
  try
    Blueprint:=LuaState.NewClass('Greeter');
    Blueprint.AddProperty('name', GreeterNameGet, GreeterNameSet);
    Blueprint.AddMethod('describe', GreeterDescribe);
    Blueprint.OnConstruction:=GreeterConstruct;
    Blueprint.Register;

    AssertTrue(
      LuaState.ExecuteDirect(
        'local g = Greeter:new() ' +
        'g.name = "Lua" ' +
        'greeter_name = g.name ' +
        'greeter_message = g:describe()'
      ),
      'Blueprint-backed script should execute successfully.'
    );

    AssertEqual(True, FConstructed, 'Construction callback was not triggered.');
    AssertEqual('Lua', FStoredName, 'Property setter did not persist the assigned value.');
    AssertEqual('Lua', VarToStr(LuaState.Globals['greeter_name']), 'Property getter returned an unexpected value.');
    AssertEqual('Hello, Lua', VarToStr(LuaState.Globals['greeter_message']), 'Method callback returned an unexpected value.');
  finally
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.TestClassDestroyLifetime;
const
  ProxyCount = 24;
var
  I: Integer;
  LuaState: TLua;
  Blueprint: TLuaClassBlueprint;
  Invoker: TLuaClassMethodInvoker;
  Proxies: array[0..ProxyCount - 1] of TLuaClass;
begin
  FProxyMethodCount:=0;
  FProxyReleaseCount:=0;
  LuaState:=TLua.Create;
  try
    Blueprint:=LuaState.NewClass('LifetimeProxy');
    Blueprint.AddMethod('ping', ProxyLifetimePing);
    Blueprint.OnRelease:=ProxyLifetimeRelease;
    Blueprint.Register;

    for I:=0 to High(Proxies) do
    begin
      Proxies[I]:=Blueprint.Construct(Pointer(I + 1));
      AssertTrue(Assigned(Proxies[I]), 'Construct should create native lifetime proxies.');
      AssertTrue(Proxies[I].TryInvoke('ping', Invoker), 'Lifetime proxy method should resolve before destroy.');
      try
        AssertTrue(Invoker.Execute, 'Lifetime proxy method should execute before destroy.');
        AssertEqual('pong', Invoker.Results[0].AsStr, 'Lifetime proxy method returned the wrong value.');
      finally
        Invoker.Free;
      end;
    end;

    for I:=0 to High(Proxies) do
    begin
      Proxies[I].Free;
      Proxies[I]:=nil;
    end;

    AssertEqual(ProxyCount, FProxyMethodCount, 'Every lifetime proxy should have executed its method before destroy.');
    AssertEqual(ProxyCount, FProxyReleaseCount, 'Host-owned lifetime proxy destroy should release each proxy once.');
  finally
    LuaState.Free;
  end;

  FProxyMethodCount:=0;
  FProxyReleaseCount:=0;
  LuaState:=TLua.Create;
  try
    Blueprint:=LuaState.NewClass('LateLifetimeProxy');
    Blueprint.AddMethod('ping', ProxyLifetimePing);
    Blueprint.OnRelease:=ProxyLifetimeRelease;
    Blueprint.Register;

    for I:=0 to High(Proxies) do
    begin
      Proxies[I]:=Blueprint.Construct(Pointer(I + 1));
      AssertTrue(Assigned(Proxies[I]), 'Construct should create late lifetime proxies.');
      AssertTrue(Proxies[I].TryInvoke('ping', Invoker), 'Late lifetime proxy method should resolve before shutdown.');
      try
        AssertTrue(Invoker.Execute, 'Late lifetime proxy method should execute before shutdown.');
      finally
        Invoker.Free;
      end;
    end;

    LuaState.Free;
    LuaState:=nil;

    for I:=0 to High(Proxies) do
    begin
      Proxies[I].Free;
      Proxies[I]:=nil;
    end;

    AssertEqual(ProxyCount, FProxyMethodCount, 'Late lifetime proxies should execute before TLua shutdown.');
    AssertEqual(ProxyCount, FProxyReleaseCount, 'Late lifetime proxies should release exactly once during shutdown.');
  finally
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.Run;
begin
  RunTest('Globals and ExecuteDirect', TestGlobalsAndExecuteDirect);
  RunTest('RegisterMethod', TestRegisteredFunction);
  RunTest('IntroduceFunction', TestIntroduceFunction);
  RunTest('Tables', TestTables);
  RunTest('Libraries', TestLibraries);
  RunTest('Threads', TestThreads);
  RunTest('Error handling', TestErrorHandling);
  RunTest('LoadSource', TestLoadSource);
  RunTest('Inheritance', TestInheritance);
  RunTest('Callback arg-table invoke', TestCallbackArgTableInvoke);
  RunTest('Memory usage', TestMemoryUsage);
  RunTest('CopyTable', TestCopyTable);
  RunTest('Class blueprints', TestClassBlueprintBinding);
  RunTest('Class destroy lifetime', TestClassDestroyLifetime);

  Writeln(Format('[PASS] %d assertions', [FAssertionCount]));
end;

var
  Suite: TLuaRegressionSuite;
begin
  ExitCode:=1;
  Suite:=TLuaRegressionSuite.Create;
  try
    Suite.Run;
    ExitCode:=0;
  finally
    Suite.Free;
  end;
end.
