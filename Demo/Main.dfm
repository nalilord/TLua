object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'TestApp'
  ClientHeight = 704
  ClientWidth = 1144
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    1144
    704)
  PixelsPerInch = 96
  TextHeight = 13
  object btnExecute: TButton
    Left = 8
    Top = 671
    Width = 137
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Execute'
    TabOrder = 0
    OnClick = btnExecuteClick
  end
  object syndtCode: TMemo
    Left = 8
    Top = 8
    Width = 1128
    Height = 656
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '--[[ Init Test Variables ]]--'
      ''
      'testVar1 = "test1";'
      'testVar2 = "test2";'
      ''
      'table = {'
      ''
      '  [0] = "Hello World",'
      '  [1] = "FooBar",'
      ''
      '};'
      ''
      'Out("=============== Tables ===============");'
      ''
      'Out(table, "TESTVALUE: " .. tostring(TESTVALUE));'
      ''
      'Out("=============== Functions ===============");'
      ''
      'TestProc1("FooBar");'
      ''
      'local test1 = TestFunc1("Hello World", 1, 1.2, false, nil);'
      'Out("Result from '#39'TestCall'#39': " .. test1);'
      ''
      'tst.fooBar(tst.TEST);'
      ''
      'Out("=============== Functions 2nd ===============");'
      'local func = function(...)'
      ''
      '  local arg = {...};'
      '  for i,v in ipairs(arg) do'
      '    Out("Func: " .. tostring(v));'
      '  end;'
      ''
      '  return "Hello world from dynamic func!";'
      ''
      'end;'
      'local test2 = TestFunc2(func);'
      ''
      'Out("=============== Classes/Methods ===============");'
      ''
      'clazz = MyClass:new();'
      'resVar1 = clazz:testFunc(testVar1);'
      'Out(resVar1);'
      ''
      'Out("=============== Classes/Properties ===============");'
      ''
      'clazz.blubb = 1;'
      'resVar2 = clazz.blubb;'
      'Out(resVar2);'
      ''
      'Out("=============== Classes/Indexing ===============");'
      ''
      'clazz[0] = 1;'
      'clazz["A"] = "B";'
      'clazz.idx[0] = "foo";'
      ''
      'clazz = nil;'
      ''
      'Out("=============== Classes/Inheritance ===============");'
      ''
      'MyNewClass = class(MyClass);'
      'MyNewClass.testFunc = function(self);'
      ''
      '  Out("Calling by self inherited...");'
      '  self:inherited();'
      ''
      '  Out("Calling by global inherited...");'
      '  inherited(self, "testFunc");'
      ''
      '  Out("Hello World from MyNewClass:testFunc()!");'
      ''
      '  self:testFunc3();'
      ''
      'end;'
      ''
      'MyNewClass.testFunc2 = function(self)'
      ''
      '  Out("Hello World from MyNewClass:testFunc2()!");'
      ''
      'end;'
      ''
      'function MyNewClass:testFunc3()'
      ''
      '  Out("Hello World from MyNewClass:testFunc3()!");'
      ''
      '  self:testFunc2();'
      ''
      'end;'
      ''
      'clazz2 = MyNewClass:new();'
      'clazz2:testFunc();'
      'clazz2:testFunc2();'
      'clazz2:testFunc3();')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
