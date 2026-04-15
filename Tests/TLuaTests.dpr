program TLuaTests;

{$APPTYPE CONSOLE}

uses
  System.IOUtils,
  System.SysUtils,
  System.Math,
  System.Variants,
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

  TLuaRegressionSuite = class
  private
    FAssertionCount: Integer;
    FBaseSpeakCount: Integer;
    FConstructed: Boolean;
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
    procedure TestMemoryUsage;
    procedure TestClassBlueprintBinding;
  public
    procedure Run;
  end;

procedure TErrorHandler.OnScriptLoadError(Name, Message: WideString; Code: Integer; LuaMessage: WideString);
begin
  Inc(LoadCount);
  LastName := Name;
  LastMessage := Message;
  LastCode := Code;
  LastLuaMessage := LuaMessage;
end;

procedure TErrorHandler.OnScriptExecutionError(Name, Message: WideString; Code: Integer; LuaMessage: WideString);
begin
  Inc(ExecutionCount);
  LastName := Name;
  LastMessage := Message;
  LastCode := Code;
  LastLuaMessage := LuaMessage;
end;

procedure TLuaRegressionSuite.AssertTrue(ACondition: Boolean; const AMessage: string);
begin
  Inc(FAssertionCount);
  if not ACondition then
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

procedure TLuaRegressionSuite.BaseSpeak(Sender: TLua; Clazz: TLuaClass; Method: TLuaClassMethod; Args: TLuaArgs; Results: TLuaResults);
begin
  Inc(FBaseSpeakCount);
  Results.PushStr('base');
end;

procedure TLuaRegressionSuite.GreeterConstruct(Sender: TLua; Blueprint: TLuaClassBlueprint; Args: TLuaArgs; var UserClass: TObject; var Allow: Boolean);
begin
  FConstructed := True;
end;

procedure TLuaRegressionSuite.GreeterDescribe(Sender: TLua; Clazz: TLuaClass; Method: TLuaClassMethod; Args: TLuaArgs; Results: TLuaResults);
begin
  Results.PushStr('Hello, ' + FStoredName);
end;

procedure TLuaRegressionSuite.GreeterNameGet(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassProperty; Value: TLuaValue);
begin
  Value.AsStr := FStoredName;
end;

procedure TLuaRegressionSuite.GreeterNameSet(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassProperty; Value: TLuaValue);
begin
  FStoredName := Value.AsStr;
end;

procedure TLuaRegressionSuite.TestGlobalsAndExecuteDirect;
var
  LuaState: TLua;
begin
  LuaState := TLua.Create;
  try
    LuaState.Globals['answer'] := 41;
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
  LuaState := TLua.Create;
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
  LuaState := TLua.Create;
  try
    AssertTrue(
      LuaState.ExecuteDirect('function greet(name) return true, "Hello, " .. name end'),
      'Test function declaration failed.'
    );

    AssertTrue(LuaState.IntroduceFunction('greet'), 'First IntroduceFunction call should succeed.');
    AssertTrue(not LuaState.IntroduceFunction('greet'), 'Second IntroduceFunction call should report the cached function.');

    Func := LuaState.Functions['greet'];
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
  LuaState := TLua.Create;
  try
    Table := LuaState.NewTable('settings');
    try
      Table.Add('name', 'TLua');
      Table.Add('enabled', True);
      Table.Add('retries', 3);
    finally
      Table.Free;
    end;

    Table := LuaState.Tables['settings'];
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
  LuaState := TLua.Create;
  try
    Lib := LuaState.NewLibrary('cfg');
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
  LuaState := TLua.Create;
  try
    Thread := LuaState.NewThread;
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
        not Thread.Execute('error("thread-fail")'),
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
  LuaState := TLua.Create;
  Handler := TErrorHandler.Create;
  try
    LuaState.ScriptName := 'ErrorSuite';
    LuaState.RegisterErrorHandler(Handler);

    LuaState.ScriptText := 'function broken(';
    AssertTrue(not LuaState.Execute, 'Invalid source should fail during load.');
    AssertEqual(1, Handler.LoadCount, 'Load error handler should be called once.');
    AssertEqual('ErrorSuite', LuaState.LastErrorName, 'Last error name should reflect the script context.');
    AssertEqual('Syntax error during precompilation', LuaState.LastErrorMessage, 'Unexpected load error category.');
    AssertTrue(LuaState.LastErrorCode <> LUA_OK, 'Load error code should be set.');
    AssertTrue(LuaState.LastErrorLuaMessage <> '', 'Load error should preserve the Lua message.');

    AssertTrue(not LuaState.ExecuteText('error("direct-fail")'), 'ExecuteText runtime error should fail.');
    AssertEqual(1, Handler.ExecutionCount, 'ExecuteDirect should notify the execution error handler.');
    AssertEqual('ErrorSuite', LuaState.LastErrorName, 'ExecuteDirect should reuse the script name when present.');
    AssertEqual('Runtime error', LuaState.LastErrorMessage, 'Unexpected direct execution error category.');
    AssertTrue(Pos('direct-fail', LuaState.LastErrorLuaMessage) > 0, 'Direct execution Lua error text was not preserved.');

    LuaState.ScriptText := 'error("execute-fail")';
    AssertTrue(not LuaState.Execute, 'Runtime source should fail during execution.');
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
  LuaState := TLua.Create;
  TempFile := TPath.Combine(TPath.GetTempPath, 'tlua-loadsource-test.lua');
  MissingFile := TPath.Combine(TPath.GetTempPath, 'tlua-loadsource-missing.lua');

  try
    TFile.WriteAllText(TempFile, 'loaded_value = 314');

    AssertTrue(LuaState.LoadFromFile(TempFile), 'LoadFromFile should accept an existing script file.');
    AssertEqual('tlua-loadsource-test.lua', LuaState.ScriptName, 'LoadSource should derive ScriptName from the file name.');
    AssertEqual('', LuaState.LastErrorMessage, 'Successful LoadSource should leave no last error message.');
    AssertTrue(Pos('loaded_value = 314', LuaState.ScriptText) > 0, 'ScriptText should mirror the loaded file contents.');
    AssertTrue(LuaState.Execute, 'Loaded script should execute successfully.');
    AssertEqual(314, VarAsType(LuaState.Globals['loaded_value'], varInt64), 'Loaded script did not execute as expected.');

    if TFile.Exists(MissingFile) then
      TFile.Delete(MissingFile);

    AssertTrue(not LuaState.LoadFromFile(MissingFile), 'LoadFromFile should fail for a missing file.');
    AssertEqual(LUA_ERRFILE, LuaState.LastErrorCode, 'Missing file should map to LUA_ERRFILE.');
    AssertEqual('Script file not found', LuaState.LastErrorMessage, 'Unexpected missing-file error message.');
    AssertTrue(Pos('tlua-loadsource-missing.lua', LuaState.LastErrorName) > 0, 'Missing file name should be preserved.');
  finally
    if TFile.Exists(TempFile) then
      TFile.Delete(TempFile);
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.TestInheritance;
var
  LuaState: TLua;
  Blueprint: TLuaClassBlueprint;
begin
  LuaState := TLua.Create;
  try
    FBaseSpeakCount := 0;
    Blueprint := LuaState.NewClass('BaseGreeter');
    Blueprint.AddMethod('speak', BaseSpeak);
    Blueprint.Register;

    AssertTrue(
      LuaState.ExecuteDirect(
        'ChildGreeter = class(BaseGreeter) ' +
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
  LuaState := TLua.Create;
  try
    BeforeUsage := LuaState.MemoryUsage;
    AssertTrue(BeforeUsage > 0, 'Lua allocator should report memory usage after state initialization.');

    AssertTrue(
      LuaState.ExecuteDirect('memory_blob = string.rep("x", 200000)'),
      'Memory usage setup script should succeed.'
    );

    AfterUsage := LuaState.MemoryUsage;
    AssertTrue(AfterUsage > BeforeUsage, 'MemoryUsage should increase after retaining a large Lua string.');
  finally
    LuaState.Free;
  end;
end;

procedure TLuaRegressionSuite.TestClassBlueprintBinding;
var
  LuaState: TLua;
  Blueprint: TLuaClassBlueprint;
begin
  FConstructed := False;
  FStoredName := 'World';

  LuaState := TLua.Create;
  try
    Blueprint := LuaState.NewClass('Greeter');
    Blueprint.AddProperty('name', GreeterNameGet, GreeterNameSet);
    Blueprint.AddMethod('describe', GreeterDescribe);
    Blueprint.OnConstruction := GreeterConstruct;
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
  RunTest('Memory usage', TestMemoryUsage);
  RunTest('Class blueprints', TestClassBlueprintBinding);

  Writeln(Format('[PASS] %d assertions', [FAssertionCount]));
end;

var
  Suite: TLuaRegressionSuite;
begin
  ExitCode := 1;
  Suite := TLuaRegressionSuite.Create;
  try
    Suite.Run;
    ExitCode := 0;
  finally
    Suite.Free;
  end;
end.
