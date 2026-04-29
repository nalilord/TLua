program TLuaBenchmarks;

{$APPTYPE CONSOLE}
{$IFDEF FPC}
  {$MODE DELPHIUNICODE}
  {$H+}
{$ENDIF}

uses
  SysUtils,
  StrUtils,
  Lua,
  LuaAPI
  {$IFDEF MSWINDOWS}, Winapi.Windows{$ENDIF};

type
  EBenchmarkFailure = class(Exception);

  TBenchmarkCallbacks = class
  public
    procedure CallbackAdd(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
    procedure CallbackTick(Sender: TLua; Clazz: TLuaClass; Method: TLuaClassMethod; Args: TLuaArgs; Results: TLuaResults);
  end;

  TBenchmarkSuite = class
  private
    FFilter: string;
    FRepeat: Integer;
    FRunCount: Integer;
    FScale: Integer;
    function CaptureSeconds: Double;
    function ExecuteFunctionBench(AFunc: TLuaFunction; AIterations: Integer; const AName: string): Int64;
    function GetWarmupOperations(AOperations: Integer): Integer;
    function LuaSumExpected(AOperations: Integer; AFactor, ABias: Int64): Int64;
    function MatchesFilter(const AName: string): Boolean;
    procedure Ensure(ACondition: Boolean; const AMessage: string);
    procedure Report(const AName: string; AOperations: Int64; ASeconds: Double; AChecksum: Int64);
    procedure RunDelphiToLuaClassMethod;
    procedure RunDelphiToLuaClassMethodCached;
    procedure RunDelphiToLuaFunction;
    procedure RunDelphiToNativeClassMethod;
    procedure RunDelphiToNativeClassMethodCached;
    procedure RunExecuteDirectNoop;
    procedure RunLuaToDelphiClassMethod;
    procedure RunLuaToDelphiFunction;
  public
    constructor Create(AScale, ARepeat: Integer; const AFilter: string);
    procedure Run;
  end;

var
  GCallbacks: TBenchmarkCallbacks;

function MaxIntValue(A, B: Integer): Integer;
begin
  if A > B then
    Result:=A
  else
    Result:=B;
end;

function ParseScale: Integer;
var
  I: Integer;
  Param: string;
begin
  Result:=100000;

  for I:=1 to ParamCount do
  begin
    Param:=ParamStr(I);
    if StartsText('--scale=', Param) then
    begin
      Result:=StrToIntDef(Copy(Param, Length('--scale=') + 1, MaxInt), Result);
    end;
  end;

  if Result < 1 then
    Result:=1;
end;

function ParseFilter: string;
var
  I: Integer;
  Param: string;
begin
  Result:='';

  for I:=1 to ParamCount do
  begin
    Param:=ParamStr(I);
    if StartsText('--filter=', Param) then
      Result:=Copy(Param, Length('--filter=') + 1, MaxInt);
  end;
end;

function ParseRepeat: Integer;
var
  I: Integer;
  Param: string;
begin
  Result:=1;

  for I:=1 to ParamCount do
  begin
    Param:=ParamStr(I);
    if StartsText('--repeat=', Param) then
    begin
      Result:=StrToIntDef(Copy(Param, Length('--repeat=') + 1, MaxInt), Result);
    end;
  end;

  if Result < 1 then
    Result:=1;
end;

function TBenchmarkSuite.CaptureSeconds: Double;
{$IFDEF MSWINDOWS}
var
  Counter: Int64;
  Frequency: Int64;
begin
  QueryPerformanceCounter(Counter);
  QueryPerformanceFrequency(Frequency);
  Result:=Counter / Frequency;
end;
{$ELSE}
begin
  Result:=GetTickCount64 / 1000.0;
end;
{$ENDIF}

constructor TBenchmarkSuite.Create(AScale, ARepeat: Integer; const AFilter: string);
begin
  inherited Create;

  FScale:=AScale;
  FRepeat:=ARepeat;
  FFilter:=AFilter;
end;

procedure TBenchmarkSuite.Ensure(ACondition: Boolean; const AMessage: string);
begin
  if NOT ACondition then
    raise EBenchmarkFailure.Create(AMessage);
end;

procedure TBenchmarkCallbacks.CallbackAdd(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
begin
  if NOT Args.Check([ltNumber, ltNumber]) then
    raise EBenchmarkFailure.Create('bench_add expected two numeric arguments.');

  Results.PushInt(Args[0].AsInt + Args[1].AsInt);
end;

procedure TBenchmarkCallbacks.CallbackTick(Sender: TLua; Clazz: TLuaClass; Method: TLuaClassMethod; Args: TLuaArgs; Results: TLuaResults);
begin
  if NOT Args.Check([ltNumber]) then
    raise EBenchmarkFailure.Create(
      Method.Name + ' expected one numeric argument, got count=' + IntToStr(Args.Count) +
      ', arg0=' + IfThen((Args.Count > 0) AND Assigned(Args[0]), Args[0].TypName, '<none>')
    );

  Results.PushInt(Args[0].AsInt + 1);
end;

function TBenchmarkSuite.ExecuteFunctionBench(AFunc: TLuaFunction; AIterations: Integer; const AName: string): Int64;
begin
  AFunc.Args.Clear;
  AFunc.Args.PushInt(AIterations);
  Ensure(AFunc.Execute, AName + ' execution failed.');
  Ensure(AFunc.Results.Count > 0, AName + ' returned no result.');
  Result:=AFunc.Results[0].AsInt;
end;

function TBenchmarkSuite.GetWarmupOperations(AOperations: Integer): Integer;
begin
  Result:=AOperations div 10;
  if Result < 1 then
    Result:=1;
end;

function TBenchmarkSuite.LuaSumExpected(AOperations: Integer; AFactor, ABias: Int64): Int64;
begin
  Result:=(AFactor * AOperations * (AOperations + 1) div 2) + (ABias * AOperations);
end;

function TBenchmarkSuite.MatchesFilter(const AName: string): Boolean;
begin
  Result:=(FFilter = '') OR ContainsText(AName, FFilter);
end;

procedure TBenchmarkSuite.Report(const AName: string; AOperations: Int64; ASeconds: Double; AChecksum: Int64);
var
  NsPerOp: Double;
  OpsPerSecond: Double;
begin
  if ASeconds <= 0 then
    ASeconds:=0.000000001;

  NsPerOp:=(ASeconds * 1000000000.0) / AOperations;
  OpsPerSecond:=AOperations / ASeconds;

  Inc(FRunCount);
  Writeln(
    Format(
      '%-42s  ops=%10d  total=%9.3f ms  ns/op=%12.1f  ops/s=%12.1f  checksum=%d',
      [AName, AOperations, ASeconds * 1000.0, NsPerOp, OpsPerSecond, AChecksum]
    )
  );
end;

procedure TBenchmarkSuite.RunExecuteDirectNoop;
const
  BenchmarkName = 'ExecuteDirect no-op';
var
  Checksum: Int64;
  I: Integer;
  LuaState: TLua;
  Operations: Integer;
  StartTime: Double;
  WarmupOperations: Integer;
begin
  if NOT MatchesFilter(BenchmarkName) then
    Exit;

  Operations:=MaxIntValue(1000, FScale);
  WarmupOperations:=GetWarmupOperations(Operations);
  Checksum:=0;
  LuaState:=TLua.Create;
  try
    for I:=1 to WarmupOperations do
      Ensure(LuaState.ExecuteDirect('local value = 1'), BenchmarkName + ' warmup failed.');

    StartTime:=CaptureSeconds;
    for I:=1 to Operations do
    begin
      Ensure(LuaState.ExecuteDirect('local value = 1'), BenchmarkName + ' iteration failed.');
      Inc(Checksum);
    end;
    Report(BenchmarkName, Operations, CaptureSeconds - StartTime, Checksum);
  finally
    LuaState.Free;
  end;
end;

procedure TBenchmarkSuite.RunLuaToDelphiFunction;
const
  BenchmarkName = 'Lua -> Delphi function callback';
var
  BenchmarkFunc: TLuaFunction;
  Checksum: Int64;
  LuaState: TLua;
  Operations: Integer;
  StartTime: Double;
begin
  if NOT MatchesFilter(BenchmarkName) then
    Exit;

  Operations:=FScale;
  LuaState:=TLua.Create;
  try
    LuaState.RegisterMethod('bench_add', GCallbacks.CallbackAdd);
    Ensure(
      LuaState.ExecuteDirect(
        'function run_function_callback_bench(iterations) ' +
        '  local acc = 0 ' +
        '  for i = 1, iterations do ' +
        '    acc = acc + bench_add(i, i) ' +
        '  end ' +
        '  return acc ' +
        'end'
      ),
      BenchmarkName + ' setup failed.'
    );
    Ensure(LuaState.IntroduceFunction('run_function_callback_bench'), BenchmarkName + ' function registration failed.');
    BenchmarkFunc:=LuaState.Functions['run_function_callback_bench'];

    Ensure(
      ExecuteFunctionBench(BenchmarkFunc, GetWarmupOperations(Operations), BenchmarkName) =
      LuaSumExpected(GetWarmupOperations(Operations), 2, 0),
      BenchmarkName + ' warmup checksum mismatch.'
    );

    StartTime:=CaptureSeconds;
    Checksum:=ExecuteFunctionBench(BenchmarkFunc, Operations, BenchmarkName);
    Report(BenchmarkName, Operations, CaptureSeconds - StartTime, Checksum);
    Ensure(Checksum = LuaSumExpected(Operations, 2, 0), BenchmarkName + ' checksum mismatch.');
  finally
    LuaState.Free;
  end;
end;

procedure TBenchmarkSuite.RunLuaToDelphiClassMethod;
const
  BenchmarkName = 'Lua -> Delphi class method callback';
var
  BenchmarkFunc: TLuaFunction;
  Blueprint: TLuaClassBlueprint;
  Checksum: Int64;
  LuaState: TLua;
  Operations: Integer;
  StartTime: Double;
begin
  if NOT MatchesFilter(BenchmarkName) then
    Exit;

  Operations:=FScale;
  LuaState:=TLua.Create;
  try
    Blueprint:=LuaState.NewClass('BenchNativeClass');
    Blueprint.AddMethod('tick', GCallbacks.CallbackTick);
    Blueprint.Register;

    Ensure(
      LuaState.ExecuteDirect(
        'function run_class_callback_bench(iterations) ' +
        '  local obj = BenchNativeClass:new() ' +
        '  local acc = 0 ' +
        '  for i = 1, iterations do ' +
        '    acc = acc + obj:tick(i) ' +
        '  end ' +
        '  return acc ' +
        'end'
      ),
      BenchmarkName + ' setup failed.'
    );
    Ensure(LuaState.IntroduceFunction('run_class_callback_bench'), BenchmarkName + ' function registration failed.');
    BenchmarkFunc:=LuaState.Functions['run_class_callback_bench'];

    Ensure(
      ExecuteFunctionBench(BenchmarkFunc, GetWarmupOperations(Operations), BenchmarkName) =
      LuaSumExpected(GetWarmupOperations(Operations), 1, 1),
      BenchmarkName + ' warmup checksum mismatch.'
    );

    StartTime:=CaptureSeconds;
    Checksum:=ExecuteFunctionBench(BenchmarkFunc, Operations, BenchmarkName);
    Report(BenchmarkName, Operations, CaptureSeconds - StartTime, Checksum);
    Ensure(Checksum = LuaSumExpected(Operations, 1, 1), BenchmarkName + ' checksum mismatch.');
  finally
    LuaState.Free;
  end;
end;

procedure TBenchmarkSuite.RunDelphiToLuaFunction;
const
  BenchmarkName = 'Delphi -> Lua function invoke';
var
  BenchmarkFunc: TLuaFunction;
  Checksum: Int64;
  I: Integer;
  LuaState: TLua;
  Operations: Integer;
  StartTime: Double;
  WarmupOperations: Integer;
begin
  if NOT MatchesFilter(BenchmarkName) then
    Exit;

  Operations:=MaxIntValue(1, FScale div 5);
  WarmupOperations:=GetWarmupOperations(Operations);
  Checksum:=0;
  LuaState:=TLua.Create;
  try
    Ensure(
      LuaState.ExecuteDirect('function bench_add(a, b) return a + b end'),
      BenchmarkName + ' setup failed.'
    );
    Ensure(LuaState.IntroduceFunction('bench_add'), BenchmarkName + ' function registration failed.');
    BenchmarkFunc:=LuaState.Functions['bench_add'];

    for I:=1 to WarmupOperations do
    begin
      BenchmarkFunc.Args.Clear;
      BenchmarkFunc.Args.PushInt(I);
      BenchmarkFunc.Args.PushInt(I);
      Ensure(BenchmarkFunc.Execute, BenchmarkName + ' warmup execution failed.');
    end;

    StartTime:=CaptureSeconds;
    for I:=1 to Operations do
    begin
      BenchmarkFunc.Args.Clear;
      BenchmarkFunc.Args.PushInt(I);
      BenchmarkFunc.Args.PushInt(I);
      Ensure(BenchmarkFunc.Execute, BenchmarkName + ' execution failed.');
      Inc(Checksum, BenchmarkFunc.Results[0].AsInt);
    end;
    Report(BenchmarkName, Operations, CaptureSeconds - StartTime, Checksum);
    Ensure(Checksum = LuaSumExpected(Operations, 2, 0), BenchmarkName + ' checksum mismatch.');
  finally
    LuaState.Free;
  end;
end;

procedure TBenchmarkSuite.RunDelphiToLuaClassMethod;
const
  BenchmarkName = 'Delphi -> Lua class invoke';
var
  Blueprint: TLuaClassBlueprint;
  ChildBlueprint: TLuaClassBlueprint;
  Checksum: Int64;
  I: Integer;
  Instance: TLuaClass;
  Invoker: TLuaClassMethodInvoker;
  LuaState: TLua;
  Operations: Integer;
  StartTime: Double;
  WarmupOperations: Integer;
begin
  if NOT MatchesFilter(BenchmarkName) then
    Exit;

  Operations:=MaxIntValue(1, FScale div 10);
  WarmupOperations:=GetWarmupOperations(Operations);
  Checksum:=0;
  LuaState:=TLua.Create;
  try
    Blueprint:=LuaState.NewClass('BenchLuaClass');
    Blueprint.Register;
    ChildBlueprint:=Blueprint.Inherit('BenchLuaChildClass');
    ChildBlueprint.Register;
    Ensure(
      LuaState.ExecuteDirect('function BenchLuaChildClass:tick(value) return value + 1 end'),
      BenchmarkName + ' setup failed.'
    );
    Instance:=ChildBlueprint.Construct;
    try
      for I:=1 to WarmupOperations do
      begin
        Ensure(Instance.TryInvoke('tick', Invoker), BenchmarkName + ' warmup resolve failed.');
        try
          Invoker.Args.Clear;
          Invoker.Args.PushInt(I);
          Ensure(Invoker.Execute, BenchmarkName + ' warmup execution failed.');
        finally
          Invoker.Free;
        end;
      end;

      StartTime:=CaptureSeconds;
      for I:=1 to Operations do
      begin
        Ensure(Instance.TryInvoke('tick', Invoker), BenchmarkName + ' resolve failed.');
        try
          Invoker.Args.Clear;
          Invoker.Args.PushInt(I);
          Ensure(Invoker.Execute, BenchmarkName + ' execution failed.');
          Inc(Checksum, Invoker.Results[0].AsInt);
        finally
          Invoker.Free;
        end;
      end;
      Report(BenchmarkName, Operations, CaptureSeconds - StartTime, Checksum);
      Ensure(Checksum = LuaSumExpected(Operations, 1, 1), BenchmarkName + ' checksum mismatch.');
    finally
      Instance.Free;
    end;
  finally
    LuaState.Free;
  end;
end;

procedure TBenchmarkSuite.RunDelphiToLuaClassMethodCached;
const
  BenchmarkName = 'Delphi -> Lua class invoke (cached invoker)';
var
  Blueprint: TLuaClassBlueprint;
  ChildBlueprint: TLuaClassBlueprint;
  Checksum: Int64;
  I: Integer;
  Instance: TLuaClass;
  Invoker: TLuaClassMethodInvoker;
  LuaState: TLua;
  Operations: Integer;
  StartTime: Double;
  WarmupOperations: Integer;
begin
  if NOT MatchesFilter(BenchmarkName) then
    Exit;

  Operations:=MaxIntValue(1, FScale div 5);
  WarmupOperations:=GetWarmupOperations(Operations);
  Checksum:=0;
  LuaState:=TLua.Create;
  try
    Blueprint:=LuaState.NewClass('BenchLuaCachedClass');
    Blueprint.Register;
    ChildBlueprint:=Blueprint.Inherit('BenchLuaCachedChildClass');
    ChildBlueprint.Register;
    Ensure(
      LuaState.ExecuteDirect('function BenchLuaCachedChildClass:tick(value) return value + 1 end'),
      BenchmarkName + ' setup failed.'
    );
    Instance:=ChildBlueprint.Construct;
    try
      Invoker:=Instance.Invoke('tick');
      Ensure(Assigned(Invoker), BenchmarkName + ' resolve failed.');
      try
        for I:=1 to WarmupOperations do
        begin
          Invoker.Args.Clear;
          Invoker.Args.PushInt(I);
          Ensure(Invoker.Execute, BenchmarkName + ' warmup execution failed.');
        end;

        StartTime:=CaptureSeconds;
        for I:=1 to Operations do
        begin
          Invoker.Args.Clear;
          Invoker.Args.PushInt(I);
          Ensure(Invoker.Execute, BenchmarkName + ' execution failed.');
          Inc(Checksum, Invoker.Results[0].AsInt);
        end;
        Report(BenchmarkName, Operations, CaptureSeconds - StartTime, Checksum);
        Ensure(Checksum = LuaSumExpected(Operations, 1, 1), BenchmarkName + ' checksum mismatch.');
      finally
        Invoker.Free;
      end;
    finally
      Instance.Free;
    end;
  finally
    LuaState.Free;
  end;
end;

procedure TBenchmarkSuite.RunDelphiToNativeClassMethod;
const
  BenchmarkName = 'Delphi -> native class invoke';
var
  Blueprint: TLuaClassBlueprint;
  Checksum: Int64;
  I: Integer;
  Instance: TLuaClass;
  Invoker: TLuaClassMethodInvoker;
  LuaState: TLua;
  Operations: Integer;
  StartTime: Double;
  WarmupOperations: Integer;
begin
  if NOT MatchesFilter(BenchmarkName) then
    Exit;

  Operations:=MaxIntValue(1, FScale div 10);
  WarmupOperations:=GetWarmupOperations(Operations);
  Checksum:=0;
  LuaState:=TLua.Create;
  try
    Blueprint:=LuaState.NewClass('BenchNativeInvokeClass');
    Blueprint.AddMethod('tick', GCallbacks.CallbackTick);
    Blueprint.Register;
    Instance:=Blueprint.Construct;
    try
      for I:=1 to WarmupOperations do
      begin
        Ensure(Instance.TryInvoke('tick', Invoker), BenchmarkName + ' warmup resolve failed.');
        try
          Invoker.Args.Clear;
          Invoker.Args.PushInt(I);
          Ensure(Invoker.Execute, BenchmarkName + ' warmup execution failed.');
        finally
          Invoker.Free;
        end;
      end;

      StartTime:=CaptureSeconds;
      for I:=1 to Operations do
      begin
        Ensure(Instance.TryInvoke('tick', Invoker), BenchmarkName + ' resolve failed.');
        try
          Invoker.Args.Clear;
          Invoker.Args.PushInt(I);
          Ensure(Invoker.Execute, BenchmarkName + ' execution failed.');
          Inc(Checksum, Invoker.Results[0].AsInt);
        finally
          Invoker.Free;
        end;
      end;
      Report(BenchmarkName, Operations, CaptureSeconds - StartTime, Checksum);
      Ensure(Checksum = LuaSumExpected(Operations, 1, 1), BenchmarkName + ' checksum mismatch.');
    finally
      Instance.Free;
    end;
  finally
    LuaState.Free;
  end;
end;

procedure TBenchmarkSuite.RunDelphiToNativeClassMethodCached;
const
  BenchmarkName = 'Delphi -> native class invoke (cached invoker)';
var
  Blueprint: TLuaClassBlueprint;
  Checksum: Int64;
  I: Integer;
  Instance: TLuaClass;
  Invoker: TLuaClassMethodInvoker;
  LuaState: TLua;
  Operations: Integer;
  StartTime: Double;
  WarmupOperations: Integer;
begin
  if NOT MatchesFilter(BenchmarkName) then
    Exit;

  Operations:=MaxIntValue(1, FScale div 5);
  WarmupOperations:=GetWarmupOperations(Operations);
  Checksum:=0;
  LuaState:=TLua.Create;
  try
    Blueprint:=LuaState.NewClass('BenchNativeCachedInvokeClass');
    Blueprint.AddMethod('tick', GCallbacks.CallbackTick);
    Blueprint.Register;
    Instance:=Blueprint.Construct;
    try
      Invoker:=Instance.Invoke('tick');
      Ensure(Assigned(Invoker), BenchmarkName + ' resolve failed.');
      try
        for I:=1 to WarmupOperations do
        begin
          Invoker.Args.Clear;
          Invoker.Args.PushInt(I);
          Ensure(Invoker.Execute, BenchmarkName + ' warmup execution failed.');
        end;

        StartTime:=CaptureSeconds;
        for I:=1 to Operations do
        begin
          Invoker.Args.Clear;
          Invoker.Args.PushInt(I);
          Ensure(Invoker.Execute, BenchmarkName + ' execution failed.');
          Inc(Checksum, Invoker.Results[0].AsInt);
        end;
        Report(BenchmarkName, Operations, CaptureSeconds - StartTime, Checksum);
        Ensure(Checksum = LuaSumExpected(Operations, 1, 1), BenchmarkName + ' checksum mismatch.');
      finally
        Invoker.Free;
      end;
    finally
      Instance.Free;
    end;
  finally
    LuaState.Free;
  end;
end;

procedure TBenchmarkSuite.Run;
var
  I: Integer;
begin
  Writeln('TLua Benchmark Suite');
  Writeln(Format('Scale: %d', [FScale]));
  Writeln(Format('Repeat: %d', [FRepeat]));
  if FFilter <> '' then
    Writeln('Filter: ' + FFilter);
  Writeln('');

  for I:=1 to FRepeat do
  begin
    if FRepeat > 1 then
    begin
      Writeln(Format('Run %d/%d', [I, FRepeat]));
    end;

    RunExecuteDirectNoop;
    RunLuaToDelphiFunction;
    RunLuaToDelphiClassMethod;
    RunDelphiToLuaFunction;
    RunDelphiToLuaClassMethod;
    RunDelphiToLuaClassMethodCached;
    RunDelphiToNativeClassMethod;
    RunDelphiToNativeClassMethodCached;

    if (FRepeat > 1) AND (I < FRepeat) then
    begin
      Writeln('');
    end;
  end;

  if FRunCount = 0 then
    Writeln('No benchmarks matched the current filter.');
end;

var
  Suite: TBenchmarkSuite;
begin
  GCallbacks:=TBenchmarkCallbacks.Create;
  Suite:=TBenchmarkSuite.Create(ParseScale, ParseRepeat, ParseFilter);
  try
    Suite.Run;
  finally
    Suite.Free;
  end;
end.
