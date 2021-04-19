unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Lua;

type
  TfrmMain = class(TForm)
    btnExecute: TButton;
    syndtCode: TMemo;
    procedure btnExecuteClick(Sender: TObject);
  private
    procedure Tst_fooBar(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
    procedure LuaTestFunc2(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
    procedure MyClass_construct(Sender: TLua; Blueprint: TLuaClassBlueprint; Args: TLuaArgs; var UserClass: TObject; var Allow: Boolean);
    procedure MyClass_release(Sender: TLua; Clazz: TLuaClass; var UserClass: TObject);
    procedure MyClass_gc(Sender: TLua; Clazz: TLuaClass);
    procedure MyClass_testFunc(Sender: TLua; Clazz: TLuaClass; Method: TLuaClassMethod; Args: TLuaArgs; Results: TLuaResults);
    procedure MyClass_blubb_get(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassProperty; Value: TLuaValue);
    procedure MyClass_blubb_set(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassProperty; Value: TLuaValue);
    procedure MyClass_idx_get(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassIndexProperty; Index, Value: TLuaValue);
    procedure MyClass_idx_set(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassIndexProperty; Index, Value: TLuaValue);
    procedure MyClass_get(Sender: TLua; Clazz: TLuaClass; Access, Value: TLuaValue);
    procedure MyClass_set(Sender: TLua; Clazz: TLuaClass; Access, Value: TLuaValue);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ Global }

procedure LuaOut(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
var
  I: Integer;
begin
  for I:=0 to Args.Count - 1 do
  begin
    if Args[I].IsTable then
    begin
      WriteLn('[OUTPUT] ' + Args[I].AsTable.ToString(''));
    end else
    begin
      WriteLn('[OUTPUT] ' + Args[I].AsStr);
    end;
  end;
end;

procedure LuaTestFunc1(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
var
  I: Integer;
begin
  if Args.Check([ltString, ltNumber, ltNumber, ltBoolean, ltNil]) then
  begin
    for I:=0 to Args.Count - 1 do
    begin
      WriteLn(PChar(Args[I].TypName));
    end;

    WriteLn('Good arg check! - Called #1 from "' + Sender.ScriptName + '"! (' + Args[0].AsStr + ')');
  end else
  begin
    WriteLn('Bad arg check! - Called #1 from "' + Sender.ScriptName + '"! (' + Args[0].AsStr + ')');
  end;

  Results.PushStr('LuaTestFunc1');
end;

{ TfrmMain }

procedure TfrmMain.btnExecuteClick(Sender: TObject);
var
  Lua: TLua;
  Clazz: TLuaClassBlueprint;
  Thread: TLuaThread;
  Lib: TLuaLibrary;
  TableA, TableB: TLuaTable;
  ValA, ValB: TLuaValue;
begin
  Lua:=TLua.Create;

  // ******************
  // * Init
  // ******************

  Lua.ScriptName:='TestApp';
  Lua.ScriptSource:=syndtCode.Lines;

  // ******************
  // * Globals test
  // ******************

  Lua.Globals['TESTVALUE']:='FooBar';

  // ******************
  // * Func and method tests
  // ******************

  Lua.RegisterFunction('Out', LuaOut);
  Lua.RegisterFunction('TestFunc1', LuaTestFunc1);
  Lua.RegisterProcedure('TestProc1', procedure(Sender: TLua; Args: TLuaArgs; Results: TLuaResults)
    begin
      WriteLn('Hello World from anonymous procedure!');
    end);
  Lua.RegisterMethod('TestFunc2', LuaTestFunc2);

  // ******************
  // * Class test
  // ******************

  Clazz:=Lua.NewClass('MyClass');

  Clazz.AddMethod('testFunc', MyClass_testFunc);
  Clazz.AddProperty('blubb', MyClass_blubb_get, MyClass_blubb_set);
  Clazz.AddIndexProperty('idx', MyClass_idx_get, MyClass_idx_set);

  Clazz.OnConstructon:=MyClass_construct;
  Clazz.OnRelease:=MyClass_release;
  Clazz.OnGarbageCollection:=MyClass_gc;
  Clazz.OnDefaultPropertyGet:=MyClass_get;
  Clazz.OnDefaultPropertySet:=MyClass_set;

  Clazz.Register;

  // ******************
  // * Library test
  // ******************

  Lib:=Lua.NewLibrary('tst');

  Lib.AddFunction('fooBar', Tst_fooBar);
  Lib.AddConstant('TEST', 'Hello World');

  Lib.Register;

  // ******************
  // * Execute
  // ******************

  WriteLn('--------------------------- Script Execution ---------------------------');
  Lua.Execute;
  WriteLn('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');

  // ******************
  // * Misc Tests
  // ******************

  ValA:=Lua.NewValue(1337);
  ValB:=Lua.NewValue($B00B, 'gnihihi');

  WriteLn('Compare A-B LT  : ', ValA.Compare(ValB, coLessThan));
  WriteLn('Compare A-B LoE : ', ValA.Compare(ValB, coLessOrEqual));
  WriteLn('Compare B-A LT  : ', ValB.Compare(ValA, coLessThan));
  WriteLn('Compare B-A LoE : ', ValB.Compare(ValA, coLessOrEqual));

  ValA.Free;
  ValB.Free;

  WriteLn('Global Value B: ', Lua.Globals['gnihihi']);

  // ******************
  // * Table test
  // ******************

  WriteLn('--------------------------- Tables from Code ---------------------------');

  // Create a table and register it as global by name, if no name is supplied the table will not be registered in globals
  TableA:=Lua.NewTable('table_new');
  TableA.Add('foo', 'bar');
  TableA.Free;

  // Now get the table again, we only do this for testing global/registering functionality
  TableA:=Lua.Tables['table_new'];

  // Get the table
  TableB:=Lua.Tables['table'];

  WriteLn('Declared table field count: ', TableA.Count);

  // Add some fields
  TableB.BeginUpdate; // This will prevent the internal structure from updating with every change, so it's just for performance!
  TableB.Add('#1');
  TableB.Add('#2');
  TableB.Add('#3');
  TableB.EndUpdate;

  WriteLn('Table: ', TableB.ToString);
  WriteLn('Declared table field count: ', TableB.Count);
  WriteLn('Key: [3] = ', TableB.AsStr[3]);

  TableB.Records[0].AsTable:=TableA; // Updating a record directly will not trigger a table update, as you can see in the next line
  WriteLn('Records[0]: ', TableB.Records[0].AsVariant); // Record 0 still contains the string "FooBar" which was set by initializing the table in lua
  TableB.Update; // Force table update
  WriteLn('Records[0]: ', TableB.Records[0].AsVariant);
  WriteLn('Records[0]: ', TableB.Records[0].AsTable.ToString);

  // Update fields
  TableB.BeginUpdate;
  TableB.AsStr[2]:='Changed Value #1';
  TableB.AsStr[3]:='Changed Value #2';
  TableB.AsStr[4]:='Changed Value #3';
  TableB.EndUpdate;

  WriteLn('Table: ', TableB.ToString);
  WriteLn('Declared table field count: ', TableB.Count);
  WriteLn('Key: [3] = ', TableB.AsStr[3]);

  // We are done with this table(s), release the native object instance
  TableA.Free;
  TableB.Free;

  WriteLn('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');

  // ******************
  // * Thread test
  // ******************

  WriteLn('--------------------------- Lua Thread ---------------------------');

  Lua.ExecuteDirect('TestFunc1("testVar1: " .. testVar1, 1, 0.1, false, nil);');

  Thread:=Lua.NewThread;
  Thread.Execute('TestFunc1("testVar1: " .. testVar1, 2, 0.2, true, nil);');
  Thread.Free;

  WriteLn('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');

  // ******************
  // * Cleanup
  // ******************

  WriteLn('--------------------------- Cleanup ---------------------------');

  Lua.Free;

  WriteLn('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');
end;

procedure TfrmMain.LuaTestFunc2(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
begin
  if Args.Check([ltFunction]) then
  begin
    WriteLn('Called #2 from "' + Sender.ScriptName + '"!');

    with Args[0].AsFunc do
    begin
      Args.PushInt(1337);
      Execute;
      if Results.Check([ltString]) then
      begin
        WriteLn('Result = ' + Results[0].AsStr);
      end;
    end;
  end;

  Results.PushStr('LuaTestFunc2');
end;

procedure TfrmMain.MyClass_blubb_get(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassProperty; Value: TLuaValue);
begin
  WriteLn('GET: ' + Prop.Name);
  Value.AsStr:=FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
end;

procedure TfrmMain.MyClass_blubb_set(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassProperty; Value: TLuaValue);
begin
  WriteLn('SET: ' + Prop.Name + ' = ' + Value.AsStr);
end;

procedure TfrmMain.MyClass_construct(Sender: TLua; Blueprint: TLuaClassBlueprint; Args: TLuaArgs; var UserClass: TObject; var Allow: Boolean);
begin
  WriteLn(Blueprint.Name + ' constructed!');
end;

procedure TfrmMain.MyClass_gc(Sender: TLua; Clazz: TLuaClass);
begin
  WriteLn(Clazz.Blueprint.Name + ' gc!');
end;

procedure TfrmMain.MyClass_get(Sender: TLua; Clazz: TLuaClass; Access, Value: TLuaValue);
begin
  WriteLn('GET (Default): ' + Clazz.Blueprint.Name + '.' + Access.AsStr);
  Value.AsStr:=FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
end;

procedure TfrmMain.MyClass_set(Sender: TLua; Clazz: TLuaClass; Access, Value: TLuaValue);
begin
  WriteLn('SET (Default): ' + Clazz.Blueprint.Name + '.' + Access.AsStr + ' = ' + Value.AsStr);
end;

procedure TfrmMain.MyClass_idx_get(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassIndexProperty; Index, Value: TLuaValue);
begin
  WriteLn('GET (Index): ' + Clazz.Blueprint.Name + '.' + Prop.Name + '[' + Index.AsStr + '] = ' + Value.AsStr);
  Value.AsStr:=FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
end;

procedure TfrmMain.MyClass_idx_set(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassIndexProperty; Index, Value: TLuaValue);
begin
  WriteLn('SET (Index): ' + Clazz.Blueprint.Name + '.' + Prop.Name + '[' + Index.AsStr + '] = ' + Value.AsStr);
end;

procedure TfrmMain.MyClass_release(Sender: TLua; Clazz: TLuaClass; var UserClass: TObject);
begin
  WriteLn(Clazz.Blueprint.Name + ' released!');
end;

procedure TfrmMain.MyClass_testFunc(Sender: TLua; Clazz: TLuaClass; Method: TLuaClassMethod; Args: TLuaArgs; Results: TLuaResults);
var
  I: Integer;
begin
  Write(Clazz.Blueprint.Name + ':' + Method.Name + '(');
  for I:=0 to Args.Count - 1 do
  begin
    Write('"' + Args[I].AsStr + '"');
    if I <> Args.Count - 1 then
      Write(', ');
  end;
  WriteLn(')');

  Results.PushBool(True);
end;

procedure TfrmMain.Tst_fooBar(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
var
  I: Integer;
begin
  Write('Lib Func fooBar(');
  for I:=0 to Args.Count - 1 do
  begin
    Write('"' + Args[I].AsStr + '"');
    if I <> Args.Count - 1 then
      Write(', ');
  end;
  WriteLn(')');
end;

initialization
  AllocConsole;

end.
