{***************************************************************************}
{                                                                           }
{       TLua - Lua Framework for Delphi                                     }
{                                                                           }
{       Version 1.4                                                         }
{                                                                           }
{       2022-09-13 - 1.4                                                    }
{         + Added "Classes" property to TLua                                }
{         + Added "SetGlobal" to TLuaObject                                 }
{         + Added "FromGlobal" to TLuaObject for internal use               }
{         - Fixed missing stack pops                                        }
{       2022-09-12 - 1.3                                                    }
{         + Added "IntroduceFunction" to TLua                               }
{         + Added "Functions" property to TLua                              }
{         - Fixed wrong order of results in TLuaFunction                    }
{       2021-10-27 - 1.2                                                    }
{         - Fixed rnd AV on free of TLua when using Classes and Blueprints  }
{         - Fixed mem leak caused by Callback handlers not beeing freed     }
{         ! Changed the free order of objects                               }
{         ! Callback handlers now getting freed after the state is closed   }
{       2021-04-18 - 1.1                                                    }
{         + Added "NewValue" to TLua                                        }
{         + Added "Compare" to TLuaObject                                   }
{         + Added missing setter functionalities for TLuaTable              }
{       2021-04-16 - 1.0                                                    }
{         ! Digged it out of the code graveyard and made it compile         }
{           again with the latest Delphi.                                   }
{       2012/13-??-?? - 0.1                                                 }
{         ! Initial Release                                                 }
{                                                                           }
{       (c) 2022 by Daniel M. (NaliLord)                                    }
{                                                                           }
{***************************************************************************}

unit Lua;

interface

uses
  Winapi.Windows, System.SysUtils, System.Variants, System.Classes, System.IniFiles, System.Generics.Collections, System.Contnrs, System.Types, LuaAPI;

const
  IID_ILuaErrorHandler = '{BE37CBD0-959E-4BC4-B874-C1F429A748D2}';

type
  TLuaObject = class;
  TLuaTable = class;
  TLuaFunction = class;
  TLuaValue = class;
  TLuaClassMethod = class;
  TLuaClassProperty = class;
  TLuaClassIndexProperty = class;
  TLuaClassBlueprint = class;
  TLuaClass = class;
  TLuaClassMethodInvoker = class;
  TLuaClassInheritor = class;
  TLuaLibrary = class;
  TLua = class;
  TLuaThread = class;
  TLuaStack = class;
  TLuaArgs = class;
  TLuaFunctionArgs = class;
  TLuaResults = class;
  TLuaFunctionResults = class;

  // Lua exception
  ELuaException = class(Exception)
  private
    FName: String;
    FLuaMessage: String;
    FCode: Integer;
  public
    property Name: String       read FName;
    property Code: Integer      read FCode;
    property LuaMessage: String read FLuaMessage;
  end;

  ELuaLoadException = class(ELuaException);
  ELuaExecuteException = class(ELuaException);

  // Enums
  TLuaStateStatus = (lssInitialize, lssReady, lssDestroy);
  TLuaObjectAcquisition = (oaNone, oaDefault, oaStackIndex, oaRefId);
  TLuaCompareOperation = (coEqual, coLessThan, coLessOrEqual);
  TLuaType = (ltNone = -1, ltNil, ltBoolean, ltLightUserdata, ltNumber, ltString, ltTable, ltFunction, ltUserdata,
    ltThread, (* new types *) ltBlueprint, ltClass);

  // Type mappings
  TLuaState = Plua_State;
  TLuaReg = PLuaL_Reg;
  TLuaCFunctionEvent = lua_CFunction;

  // Events
  TLuaProcedure = reference to procedure(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
  TLuaFunctionEvent = procedure(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
  TLuaMethodEvent = procedure(Sender: TLua; Args: TLuaArgs; Results: TLuaResults) of object;
  TLuaClassConstructionEvent = procedure(Sender: TLua; Blueprint: TLuaClassBlueprint; Args: TLuaArgs; var UserClass: TObject; var Allow: Boolean) of object;
  TLuaClassReleaseEvent = procedure(Sender: TLua; Clazz: TLuaClass; var UserClass: TObject) of object;
  TLuaClassGarbageCollectionEvent = procedure(Sender: TLua; Clazz: TLuaClass) of object;
  TLuaClassBaseMethodEvent = procedure(Sender: TLua; Clazz: TLuaClass; Args: TLuaArgs; Results: TLuaResults) of object;
  TLuaClassMethodEvent = procedure(Sender: TLua; Clazz: TLuaClass; Method: TLuaClassMethod; Args: TLuaArgs; Results: TLuaResults) of object;
  TLuaClassPropertyEvent = procedure(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassProperty; Value: TLuaValue) of object;
  TLuaClassIndexPropertyEvent = procedure(Sender: TLua; Clazz: TLuaClass; Prop: TLuaClassIndexProperty; Index, Value: TLuaValue) of object;
  TLuaClassDefaultPropertyEvent = procedure(Sender: TLua; Clazz: TLuaClass; Access, Value: TLuaValue) of object;

  // Interfaces
  ILuaErrorHandler = interface(IUnknown)
    [IID_ILuaErrorHandler]
    procedure OnScriptLoadError(Name, Message: WideString; Code: Integer; LuaMessage: WideString); stdcall;
    procedure OnScriptExecutionError(Name, Message: WideString; Code: Integer; LuaMessage: WideString); stdcall;
  end;

  // Classes
  TLuaObject = class(TPersistent)
  private
    FLua: TLua;
    FRefId: Integer;
    FTyp: Integer;
    function GetTypName: String;
    function GetTyp: TLuaType;
  strict protected
    constructor Create(ALua: TLua; AType: TLuaObjectAcquisition = oaDefault; AValue: Integer = 0);
    class function FromGlobal(ALua: TLua; AName: String): TLuaObject; virtual;
  protected
    procedure Initialize; virtual;
    procedure UnRef; virtual;
    procedure PopFromStack;
    procedure FromStack(AIndex: Integer);
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetGlobal(AName: String);
    function CheckState(ALua: TLua): Boolean;
    function CopyToRef: Integer;
    function PushToStack: Integer;
    function Compare(AValue: TLuaObject; AOperation: TLuaCompareOperation): Boolean;
    property Lua: TLua       read FLua;
    property Typ: TLuaType   read GetTyp;
    property TypName: String read GetTypName;
    property RefId: Integer  read FRefId;
  end;

  TLuaValue = class(TLuaObject)
  strict private type
    LuaValueType = (lvtEmpty, lvtSimple, lvtPointer, lvtFunction, lvtBlueprint, lvtClass, lvtTable, lvtThread);
    LuaValue = packed record
      V: Variant;
      case K: LuaValueType of
        lvtEmpty,
        lvtSimple: (X: NativeInt);
        lvtPointer: (P: Pointer);
        lvtFunction: (F: TLuaFunction);
        lvtBlueprint: (B: TLuaClassBlueprint);
        lvtClass: (C: TLuaClass);
        lvtTable: (T: TLuaTable);
        lvtThread: (H: TLuaThread);
    end;
  private
    FValue: LuaValue;
  protected
    procedure Initialize; override;
    procedure UnRef; override;
    class function Clear(ALua: TLua): TLuaValue;
    class function New(ALua: TLua; AIndex: Integer): TLuaValue;
    class function Copy(AValue: TLuaValue): TLuaValue;
    class function FromRefId(ALua: TLua; ARefId: Integer): TLuaValue;
    class function FromValue(ALua: TLua; AValue: Int64): TLuaValue; overload;
    class function FromValue(ALua: TLua; AValue: Double): TLuaValue; overload;
    class function FromValue(ALua: TLua; AValue: Boolean): TLuaValue; overload;
    class function FromValue(ALua: TLua; AValue: String): TLuaValue; overload;
    class function FromValue(ALua: TLua; AValue: Pointer): TLuaValue; overload;
    function GetAsBlueprint: TLuaClassBlueprint; virtual;
    function GetAsBool: Boolean; virtual;
    function GetAsClass: TLuaClass; virtual;
    function GetAsFloat: Double; virtual;
    function GetAsFunc: TLuaFunction; virtual;
    function GetAsInt: Int64; virtual;
    function GetAsPtr: Pointer; virtual;
    function GetAsStr: String; virtual;
    function GetAsTable: TLuaTable; virtual;
    function GetIsBlueprint: Boolean; virtual;
    function GetIsBool: Boolean; virtual;
    function GetIsClass: Boolean; virtual;
    function GetIsFloat: Boolean; virtual;
    function GetIsFunc: Boolean; virtual;
    function GetIsInt: Boolean; virtual;
    function GetIsNil: Boolean; virtual;
    function GetIsPtr: Boolean; virtual;
    function GetIsSet: Boolean; virtual;
    function GetIsStr: Boolean; virtual;
    function GetIsTable: Boolean; virtual;
    function GetAsVariant: Variant; virtual;
    procedure SetAsBlueprint(const Value: TLuaClassBlueprint); virtual;
    procedure SetAsBool(const Value: Boolean); virtual;
    procedure SetAsClass(const Value: TLuaClass); virtual;
    procedure SetAsFloat(const Value: Double); virtual;
    procedure SetAsFunc(const Value: TLuaFunction); virtual;
    procedure SetAsInt(const Value: Int64); virtual;
    procedure SetAsPtr(const Value: Pointer); virtual;
    procedure SetAsStr(const Value: String); virtual;
    procedure SetAsTable(const Value: TLuaTable); virtual;
    procedure SetAsVariant(const Value: Variant); virtual;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetClear;
    procedure SetNil;
    property IsSet: Boolean                   read GetIsSet;
    property IsNil: Boolean                   read GetIsNil;
    property IsStr: Boolean                   read GetIsStr;
    property IsInt: Boolean                   read GetIsInt;
    property IsBool: Boolean                  read GetIsBool;
    property IsFloat: Boolean                 read GetIsFloat;
    property IsPtr: Boolean                   read GetIsPtr;
    property IsTable: Boolean                 read GetIsTable;
    property IsClass: Boolean                 read GetIsClass;
    property IsBlueprint: Boolean             read GetIsBlueprint;
    property IsFunc: Boolean                  read GetIsFunc;
    property AsStr: String                    read GetAsStr       write SetAsStr;
    property AsInt: Int64                     read GetAsInt       write SetAsInt;
    property AsBool: Boolean                  read GetAsBool      write SetAsBool;
    property AsFloat: Double                  read GetAsFloat     write SetAsFloat;
    property AsPtr: Pointer                   read GetAsPtr       write SetAsPtr;
    property AsTable: TLuaTable               read GetAsTable     write SetAsTable;
    property AsBlueprint: TLuaClassBlueprint  read GetAsBlueprint write SetAsBlueprint;
    property AsClass: TLuaClass               read GetAsClass     write SetAsClass;
    property AsFunc: TLuaFunction             read GetAsFunc      write SetAsFunc;
    property AsVariant: Variant               read GetAsVariant   write SetAsVariant;
  end;

  TLuaTableRecord = class(TLuaValue)
  private
    FTable: TLuaTable;
    FKey: TLuaValue;
    FIsIndexKey: Boolean;
  protected
    constructor Create(ATable: TLuaTable; AKeyIdx, AValIdx: Integer);
    procedure SetAsBlueprint(const Value: TLuaClassBlueprint); override;
    procedure SetAsBool(const Value: Boolean); override;
    procedure SetAsClass(const Value: TLuaClass); override;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsFunc(const Value: TLuaFunction); override;
    procedure SetAsInt(const Value: Int64); override;
    procedure SetAsPtr(const Value: Pointer); override;
    procedure SetAsStr(const Value: String); override;
    procedure SetAsTable(const Value: TLuaTable); override;
    procedure SetAsVariant(const Value: Variant); override;
  public
    destructor Destroy; override;
    property IsIndexKey: Boolean read FIsIndexKey;
    property Key: TLuaValue      read FKey;
  end;

  TLuaTable = class(TLuaObject)
  strict private type
    KeyValue = packed record
      K: Variant;
      V: TLuaTableRecord;
      class function New(K: Variant; V: TLuaTableRecord): KeyValue; static;
    end;
  private
    FName: String;
    FLastNumIndex: Int64;
    FRecords: TList<KeyValue>;
    FUpdating: Integer;
    function GetCount: Integer;
    function GetRecords(Index: Integer): TLuaTableRecord;
    function GetAsBlueprint(Key: Variant): TLuaClassBlueprint;
    function GetAsBool(Key: Variant): Boolean;
    function GetAsClass(Key: Variant): TLuaClass;
    function GetAsFloat(Key: Variant): Double;
    function GetAsFunc(Key: Variant): TLuaFunction;
    function GetAsInt(Key: Variant): Int64;
    function GetAsPtr(Key: Variant): Pointer;
    function GetAsStr(Key: Variant): String;
    function GetAsTable(Key: Variant): TLuaTable;
    procedure SetAsBlueprint(Key: Variant; const Value: TLuaClassBlueprint);
    procedure SetAsBool(Key: Variant; const Value: Boolean);
    procedure SetAsClass(Key: Variant; const Value: TLuaClass);
    procedure SetAsFloat(Key: Variant; const Value: Double);
    procedure SetAsFunc(Key: Variant; const Value: TLuaFunction);
    procedure SetAsInt(Key: Variant; const Value: Int64);
    procedure SetAsPtr(Key: Variant; const Value: Pointer);
    procedure SetAsStr(Key: Variant; const Value: String);
    procedure SetAsTable(Key: Variant; const Value: TLuaTable);
    procedure SetName(const Value: String);
  protected
    class function New(ALua: TLua): TLuaTable; overload;
    class function New(ALua: TLua; AName: String): TLuaTable; overload;
    class function New(ALua: TLua; AIndex: Integer): TLuaTable; overload;
    procedure Initialize; override;
    procedure Iterate;
    function GetRecordByKey(AKey: Variant): TLuaTableRecord; overload;
    function GetRecordByIndex(AIndex: Integer): TLuaTableRecord; overload;
    function GetRecordByKey(AKey: Variant; var ARecord: TLuaTableRecord): Boolean; overload;
    function GetRecordByIndex(AIndex: Integer; var ARecord: TLuaTableRecord): Boolean; overload;
  public
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Update;
    procedure Clear;
    procedure Add(AValue: Variant); overload;
    procedure Add(AValue: TLuaValue); overload;
    procedure Add(AKey: Variant; AValue: Variant); overload;
    procedure Add(AKey: Variant; AValue: TLuaValue); overload;
    function ToString(const AVarName: String = ''): String; reintroduce;
    property Count: Integer                                read GetCount;
    property Records[Index: Integer]: TLuaTableRecord      read GetRecords;                         default;
    property Name: String                                  read FName          write SetName;
    property AsStr[Key: Variant]: String                   read GetAsStr       write SetAsStr;
    property AsInt[Key: Variant]: Int64                    read GetAsInt       write SetAsInt;
    property AsBool[Key: Variant]: Boolean                 read GetAsBool      write SetAsBool;
    property AsFloat[Key: Variant]: Double                 read GetAsFloat     write SetAsFloat;
    property AsPtr[Key: Variant]: Pointer                  read GetAsPtr       write SetAsPtr;
    property AsTable[Key: Variant]: TLuaTable              read GetAsTable     write SetAsTable;
    property AsBlueprint[Key: Variant]: TLuaClassBlueprint read GetAsBlueprint write SetAsBlueprint;
    property AsClass[Key: Variant]: TLuaClass              read GetAsClass     write SetAsClass;
    property AsFunc[Key: Variant]: TLuaFunction            read GetAsFunc      write SetAsFunc;
  end;

  TLuaFunction = class(TLuaObject)
  private
    FResults: TLuaFunctionResults;
    FArgs: TLuaFunctionArgs;
  protected
    procedure Initialize; override;
    class function New(ALua: TLua; AIndex: Integer): TLuaFunction;
  public
    destructor Destroy; override;
    function Execute: Boolean; virtual;
    property Args: TLuaFunctionArgs       read FArgs;
    property Results: TLuaFunctionResults read FResults;
  end;

  TLuaClassMethod = class(TPersistent)
  private
    FClass: TLuaClass;
    FName: String;
    FOnCallback: TLuaClassMethodEvent;
  protected
    constructor Create(AClass: TLuaClass; AName: String; ACallback: TLuaClassMethodEvent);
  public
    destructor Destroy; override;
    property Name: String                     read FName;
    property Clazz: TLuaClass                 read FClass;
    property OnCallback: TLuaClassMethodEvent read FOnCallback write FOnCallback;
  end;

  TLuaClassProperty = class(TPersistent)
  private
    FClass: TLuaClass;
    FName: String;
    FOnGetCallback: TLuaClassPropertyEvent;
    FOnSetCallback: TLuaClassPropertyEvent;
  protected
    constructor Create(AClass: TLuaClass; AName: String; AGetCallback, ASetCallback: TLuaClassPropertyEvent);
  public
    destructor Destroy; override;
    property Name: String                          read FName;
    property Clazz: TLuaClass                      read FClass;
    property OnGetCallback: TLuaClassPropertyEvent read FOnGetCallback write FOnGetCallback;
    property OnSetCallback: TLuaClassPropertyEvent read FOnSetCallback write FOnSetCallback;
  end;

  TLuaClassIndexProperty = class(TPersistent)
  private
    FClass: TLuaClass;
    FName: String;
    FOnGetCallback: TLuaClassIndexPropertyEvent;
    FOnSetCallback: TLuaClassIndexPropertyEvent;
  protected
    constructor Create(AClass: TLuaClass; AName: String; AGetCallback, ASetCallback: TLuaClassIndexPropertyEvent);
  public
    destructor Destroy; override;
    property Name: String                               read FName;
    property Clazz: TLuaClass                           read FClass;
    property OnGetCallback: TLuaClassIndexPropertyEvent read FOnGetCallback write FOnGetCallback;
    property OnSetCallback: TLuaClassIndexPropertyEvent read FOnSetCallback write FOnSetCallback;
  end;

  TLuaClassBlueprint = class(TLuaObject)
  private type
    CallType = (mtNative, mtLua);
  strict private type
    MethodEntry = packed record
      N: String;
      case T: CallType of
        mtNative: (D: TLuaClassMethodEvent);
        mtLua: (L: Integer);
    end;
    PropertyEntry = packed record
      N: String;
      G, S: TLuaClassPropertyEvent;
    end;
    IndexPropertyEntry = packed record
      N: String;
      G, S: TLuaClassIndexPropertyEvent;
    end;
  private
    FName: String;
    FIsInherited: Boolean;
    FMethods: TList<MethodEntry>;
    FProperties: TList<PropertyEntry>;
    FIndexProperties: TList<IndexPropertyEntry>;
    FAllowConstruct: Boolean;
    FInstances: TObjectList<TLuaClass>;
    FInheritances: TObjectList<TLuaClassBlueprint>;
    FLuaIndexHandler: TLuaCFunctionEvent;
    FLuaGCHandler: TLuaCFunctionEvent;
    FLuaCallHandler: TLuaCFunctionEvent;
    FLuaNewHandler: TLuaCFunctionEvent;
    FOnDefaultPropertyGet: TLuaClassDefaultPropertyEvent;
    FOnDefaultPropertySet: TLuaClassDefaultPropertyEvent;
    FOnConstructon: TLuaClassConstructionEvent;
    FOnGarbageCollection: TLuaClassGarbageCollectionEvent;
    FOnRelease: TLuaClassReleaseEvent;
    FParent: TLuaClassBlueprint;
    function GetMethodCount: Integer;
    function GetPropertyCount: Integer;
    function GetIndexPropertyCount: Integer;
    function GetIndexProperties(Index: Integer): String;
    function GetMethods(Index: Integer): String;
    function GetProperties(Index: Integer): String;
  protected
    class function FromGlobal(ALua: TLua; AName: String): TLuaObject; override; // We need to override this here, otherwise Initialize would be called...
    class function Acquire(AState: TLuaState; AKeepOnStack: Boolean = False): TLuaClassBlueprint;
    class function FromTable(ATable: TLuaTable): TLuaClassBlueprint;
    constructor Create(ALua: TLua; AName: String); overload;
    constructor Create(ABlueprint: TLuaClassBlueprint; AName: String); overload;
    procedure Initialize; override;
    procedure Finalize;
    procedure CleanupInstances;
    procedure CopyMethods(ASource: TList<MethodEntry>);
    procedure CopyProperties(ASource: TList<PropertyEntry>);
    procedure CopyIndexProperties(ASource: TList<IndexPropertyEntry>);
    procedure AddOrReplaceMethod(AMethodEntry: MethodEntry);
    procedure AddOrReplaceProperty(APropertyEntry: PropertyEntry);
    procedure AddOrReplaceIndexProperty(AIndexPropertyEntry: IndexPropertyEntry);
    // Internal stuff
    function GetMethod(AName: String; var AMethod: MethodEntry): Boolean;
    function NewMethodInvoker(AMethodName: String; AClass: TLuaClass): TLuaClassMethodInvoker;
    // Lua callbacks
    procedure LuaNewHandler(Sender: TLua; Blueprint: TLuaClassBlueprint; Args: TLuaArgs; var UserClass: TObject; var Allow: Boolean);
    procedure LuaCallHandler(Sender: TLua; Blueprint: TLuaClassBlueprint; Args: TLuaArgs; var UserClass: TObject; var Allow: Boolean);
    procedure LuaIndexHandler(Sender: TLua; Clazz: TLuaClass; Access, Value: TLuaValue);
    procedure LuaNewIndexHandler(Sender: TLua; Clazz: TLuaClass; Access, Value: TLuaValue);
    procedure LuaGCHandler(Sender: TLua; Clazz: TLuaClass);
  public
    class function IsRegistered(ALua: TLua; AName: String): Boolean;
    destructor Destroy; override;
    procedure AddMethod(AName: String; ACallback: TLuaClassMethodEvent); overload;
    procedure AddMethod(AName: String; AFunc: TLuaFunction); overload;
    procedure AddProperty(AName: String; AGet, ASet: TLuaClassPropertyEvent);
    procedure AddIndexProperty(AName: String; AGet, ASet: TLuaClassIndexPropertyEvent);
    procedure DeleteMethod(AIndex: Integer);
    procedure DeleteProperty(AIndex: Integer);
    procedure DeleteIndexProperty(AIndex: Integer);
    procedure Register;
    function HasMethod(AName: String): Boolean;
    function HasProperty(AName: String): Boolean;
    function Construct(AData: Pointer = nil): TLuaClass;
    function Inherit(AName: String = ''): TLuaClassBlueprint;
    property Name: String                                         read FName;
    property Parent: TLuaClassBlueprint                           read FParent;
    property MethodCount: Integer                                 read GetMethodCount;
    property PropertyCount: Integer                               read GetPropertyCount;
    property IndexPropertyCount: Integer                          read GetIndexPropertyCount;
    property Methods[Index: Integer]: String                      read GetMethods;
    property Properties[Index: Integer]: String                   read GetProperties;
    property IndexProperties[Index: Integer]: String              read GetIndexProperties;
    property AllowConstruct: Boolean                              read FAllowConstruct       write FAllowConstruct;
    property OnConstructon: TLuaClassConstructionEvent            read FOnConstructon        write FOnConstructon;
    property OnRelease: TLuaClassReleaseEvent                     read FOnRelease            write FOnRelease;
    property OnGarbageCollection: TLuaClassGarbageCollectionEvent read FOnGarbageCollection  write FOnGarbageCollection;
    property OnDefaultPropertyGet: TLuaClassDefaultPropertyEvent  read FOnDefaultPropertyGet write FOnDefaultPropertyGet;
    property OnDefaultPropertySet: TLuaClassDefaultPropertyEvent  read FOnDefaultPropertySet write FOnDefaultPropertySet;
  end;

  TLuaClass = class(TLuaObject)
  private
    FBlueprint: TLuaClassBlueprint;
    FTag: NativeInt;
    FData: Pointer;
    FUserClass: TObject;
    FMethods: THashedStringList;
    FProperties: THashedStringList;
    FIndexProperties: THashedStringList;
    FCleanupList: TObjectList;
    FLuaReleaseHandler: TLuaCFunctionEvent;
    FLuaInheritedHandler: TLuaCFunctionEvent;
    function GetMethods(Name: String): TLuaClassMethod;
    function GetProperties(Name: String): TLuaClassProperty;
    function GetIndexProperties(Name: String): TLuaClassIndexProperty;
    procedure LuaReleaseHandler(Sender: TLua; Clazz: TLuaClass);
    procedure LuaInheritedHandler(Sender: TLua; Clazz: TLuaClass; Args: TLuaArgs; Results: TLuaResults);
  protected
    class function FromGlobal(ALua: TLua; AName: String): TLuaObject; override; // We need to override this here, otherwise Initialize would be called...
    class function Acquire(AState: TLuaState; AKeepOnStack: Boolean = False): TLuaClass;
    class function New(ALua: TLua; ABlueprint: TLuaClassBlueprint; AData: Pointer = nil): TLuaClass;
    procedure ReleaseCleanup;
  public
    destructor Destroy; override;
    function Invoke(AMethod: String): TLuaClassMethodInvoker;
    function Inherit(AMethod: String): TLuaClassMethodInvoker;
    property Blueprint: TLuaClassBlueprint                         read FBlueprint;
    property Methods[Name: String]: TLuaClassMethod                read GetMethods;
    property Properties[Name: String]: TLuaClassProperty           read GetProperties;
    property IndexProperties[Name: String]: TLuaClassIndexProperty read GetIndexProperties;
    property UserClass: TObject                                    read FUserClass;
    property Data: Pointer                                         read FData              write FData;
    property Tag: NativeInt                                        read FTag               write FTag;
  end;

  TLuaClassMethodInvoker = class(TLuaFunction)
  strict private type
    InvokerData = packed record
      N: String;
      C: TLuaClass;
      case T: TLuaClassBlueprint.CallType of
        mtNative: (E: TLuaClassMethodEvent; F: TLuaCFunctionEvent);
        mtLua: (R: Integer);
    end;
  private
    FInvoker: InvokerData;
  protected
    class function NewLua(AMethodName: String; AClass: TLuaClass; ARefId: Integer): TLuaClassMethodInvoker;
    class function NewNative(AMethodName: String; AClass: TLuaClass; ACallback: TLuaClassMethodEvent): TLuaClassMethodInvoker;
    procedure LuaInvokeHandler(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
  public
    destructor Destroy; override;
    function Execute: Boolean; override;
  end;

  TLuaClassInheritor = class(TPersistent)
  private
    FLua: TLua;
  protected
    constructor Create(ALua: TLua); // This class is not meant for public/user creation
    procedure Activate;
    procedure LuaInheritClass(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
    procedure LuaInheritMethod(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
  public
    destructor Destroy; override;
  end;

  TLuaLibrary = class(TPersistent)
  strict private const
    HASH_BUCKET_SIZE = 256;
  strict private type
    LibFunction = packed record
      N: String;
      C: TLuaCFunctionEvent;
    end;
    LibConstant = packed record
      N: String;
      V: Variant;
    end;
  private
    FLua: TLua;
    FName: String;
    FLuaIndexHandler: TLuaCFunctionEvent;
    FFunctions: TList<LibFunction>;
    FConstants: Array of TList<LibConstant>;
  protected
    constructor Create(ALua: TLua; ALibName: String);
    procedure LuaIndexHandler(Sender: TLua; Clazz: TLuaClass; Access, Value: TLuaValue);
    procedure LuaNewIndexHandler(Sender: TLua; Clazz: TLuaClass; Access, Value: TLuaValue);
    function HashOf(const AName: String): Cardinal;
    function Find(const AName: String; var AItem: LibConstant): Integer;
  public
    destructor Destroy; override;
    procedure AddFunction(AName: String; ACallback: TLuaMethodEvent);
    procedure AddConstant(AName: String; AValue: Variant);
    procedure Register;
  end;

  TLua = class(TPersistent)
  private
    FStatus: TLuaStateStatus;
    FState: TLuaState;
    FMemoryUsage: NativeInt;
    FScriptName: String;
    FScriptSource: TStrings;
    FVolatile: Boolean;
    FStack: TLuaStack;
    FCleanupList: TObjectList;
    FErrorHandlers: TInterfaceList;
    FClassBlueprints: TObjectList<TLuaClassBlueprint>;
    FClassInheritor: TLuaClassInheritor;
    FFunctions: THashedStringList;
    procedure SetScriptSource(const Value: TStrings);
    function GetGlobals(Name: String): Variant;
    procedure SetGlobals(Name: String; const Value: Variant);
    function GetTables(Name: String): TLuaTable;
    procedure SetTables(Name: String; const Value: TLuaTable);
    function GetFunctions(Name: String): TLuaFunction;
    function GetClasses(AName: String): TLuaClassBlueprint;
  strict protected
    constructor Create(AState: TLuaState); overload;
  protected
    procedure PreCleanup;
    procedure HandleLuaError(AException: ELuaException);
    procedure NotifyCleanup(ASubject: TObject; AOperation: TOperation);
  public
    class function Acquire(AState: TLuaState): TLua;
    class function Volatile(AState: TLuaState): TLua;
    constructor Create; overload;
    destructor Destroy; override;
    procedure RegisterErrorHandler(const AErrorHandler: ILuaErrorHandler);
    procedure UnregisterErrorHandler(const AErrorHandler: ILuaErrorHandler);
    procedure RegisterCFunction(AName: String; AFunc: TLuaCFunctionEvent);
    procedure RegisterProcedure(AName: String; AProc: TLuaProcedure);
    procedure RegisterFunction(AName: String; AFunc: TLuaFunctionEvent);
    procedure RegisterMethod(AName: String; AMethod: TLuaMethodEvent);
    function IntroduceFunction(AName: String): Boolean;
    function PushRef(ARefId: Integer): Integer;
    function PushAndUnref(ARefId: Integer): Integer;
    function NewValue(AValue: Variant; AName: String = ''): TLuaValue;
    function NewClass(AName: String): TLuaClassBlueprint;
    function NewTable(AName: String = ''): TLuaTable;
    function NewLibrary(AName: String = ''): TLuaLibrary;
    function NewThread: TLuaThread;
    function LoadSource(AFileName: String): Boolean;
    function Execute: Boolean;
    function ExecuteDirect(ASource: String): Boolean;
    property State: TLuaState                           read FState;
    property IsVolatile: Boolean                        read FVolatile;
    property Functions[Name: String]: TLuaFunction      read GetFunctions;
    property Classes[Name: String]: TLuaClassBlueprint  read GetClasses;
    property Globals[Name: String]: Variant             read GetGlobals   write SetGlobals;
    property Tables[Name: String]: TLuaTable            read GetTables    write SetTables;
  published
    property MemoryUsage: NativeInt read FMemoryUsage;
    property Stack: TLuaStack       read FStack;
    property ScriptName: String     read FScriptName   write FScriptName;
    property ScriptSource: TStrings read FScriptSource write SetScriptSource;
  end;

  TLuaThread = class(TPersistent)
  private
    FLua: TLua;
    FThread: TLuaState;
    FOwned: Boolean;
    FRefId: Integer;
    FLastErrorMessage: String;
    FStack: TLuaStack;
  protected
    constructor Create(ALua: TLua); overload;
    constructor Create(ALua: TLua; AStackIdx: Integer); overload;
  public
    destructor Destroy; override;
    function Execute(AString: String): Boolean;
    property Stack: TLuaStack read FStack;
  end;

  TLuaStack = class(TPersistent)
  strict private const
    LUA_TYPES: Array[-1..8] of String = ('LUA_TNONE', 'LUA_TNIL', 'LUA_TBOOLEAN', 'LUA_TLIGHTUSERDATA', 'LUA_TNUMBER',
      'LUA_TSTRING', 'LUA_TTABLE', 'LUA_TFUNCTION', 'LUA_TUSERDATA', 'LUA_TTHREAD');
  private
    FState: TLuaState;
  protected
    constructor Create(AState: TLuaState);
  public
    procedure DumpToStdOut;
    procedure DumpToDebugOut;
    procedure Remove(AIndex: Integer); inline;
    procedure Pop(ACount: Integer = 1); inline;
    procedure RawSet(AIndex: Integer); inline;
    procedure RawSetI(AIndex: Integer; AKey: NativeInt); inline;
    procedure UnRef(ATable, ARefId: Integer); inline;
    procedure SetGlobal(AName: String); inline;
    procedure SetTable(AIndex: Integer); inline;
    procedure SetMetaTable(AObjectIndex: Integer); inline;
    procedure SetFuncs(AFuncs: TLuaReg; ANumUpvalues: Integer); inline;
    procedure SetField(ATableIndex: Integer; AName: String); inline;
    procedure CopyTable(ASource, ADest, AMeta: Integer); inline;
    function PCall(AArgsCount, AResultCount, AErrorFuncIdx: Integer): Integer; inline;
    function GetMetaTable(AName: String): Integer; inline;
    function GetGlobal(AName: String): Integer; inline;
    function Ref(ATable: Integer): Integer; inline;
    function RawGet(AIndex: Integer): Integer; inline;
    function RawGetI(AIndex: Integer; AKey: NativeInt): Integer; inline;
    function NewTable: Integer; inline;
    function NewMetaTable(AName: String): Integer; inline;
    function Typ(AIndex: Integer): Integer; inline;
    function TypName(AIndex: Integer): String; inline;
    function TypAsName(AIndex: Integer): String; inline;
    function Top: Integer; inline;
    function Next(AIndex: Integer): Boolean; inline;
    function PushNil: Integer; inline;
    function PushInteger(AValue: Int64): Integer; inline;
    function PushNumber(AValue: Double): Integer; inline;
    function PushBoolean(AValue: Boolean): Integer; inline;
    function PushString(AValue: String): Integer; inline;
    function PushPointer(AValue: Pointer): Integer; inline;
    function PushFunction(AValue: TLuaCFunctionEvent): Integer; inline;
    function PushVariant(AValue: Variant): Integer; inline;
    function PushValue(AIndex: Integer): Integer; inline;
    function IsNil(AIndex: Integer): Boolean; inline;
    function IsNone(AIndex: Integer): Boolean; inline;
    function IsInteger(AIndex: Integer): Boolean; inline;
    function IsNumber(AIndex: Integer): Boolean; inline;
    function IsBoolean(AIndex: Integer): Boolean; inline;
    function IsString(AIndex: Integer): Boolean; reintroduce; inline;
    function IsPointer(AIndex: Integer): Boolean; inline;
    function IsFunction(AIndex: Integer): Boolean; inline;
    function IsTable(AIndex: Integer): Boolean; inline;
    function IsThread(AIndex: Integer): Boolean; inline;
    function ToInteger(AIndex: Integer): Int64; inline;
    function ToNumber(AIndex: Integer): Double; inline;
    function ToBoolean(AIndex: Integer): Boolean; inline;
    function ToString(AIndex: Integer): String; reintroduce; inline;
    function ToPointer(AIndex: Integer): Pointer; inline;
    function ToFunction(AIndex: Integer): TLuaCFunctionEvent; inline;
    function ToThread(AIndex: Integer): TLuaState; inline;
    function ToVariant(AIndex: Integer): Variant; inline;
  end;

  TLuaArgs = class(TPersistent)
  private
    FLua: TLua;
    FCount: Integer;
    FValues: TObjectList<TLuaValue>;
    function GetValue(Index: Integer): TLuaValue;
  protected
    constructor Create(ALua: TLua; ACount: Integer = -1); // This class is not meant for public/user creation
    procedure GetArgs;
    procedure Update(ACount: Integer = -1);
  public
    destructor Destroy; override;
    procedure Purge(AIndex: Integer);
    function Check(ATypes: Array of TLuaType; AExactLength: Boolean = True): Boolean;
    property Count: Integer                   read FCount   write FCount;
    property Value[Index: Integer]: TLuaValue read GetValue;             default;
  end;

  TLuaFunctionResults = class(TLuaArgs)
  end;

  TLuaResults = class(TPersistent)
  private
    FLua: TLua;
    FValues: TObjectList<TLuaValue>;
    function GetCount: Integer;
  protected
    constructor Create(ALua: TLua); // This class is not meant for public/user creation
    procedure ApplyToStack;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure PushNil;
    procedure PushInt(AValue: Int64);
    procedure PushFloat(AValue: Double);
    procedure PushBool(AValue: Boolean);
    procedure PushStr(AValue: String);
    procedure PushValue(AValue: TLuaValue);
    procedure PushClass(AValue: TLuaClass);
    procedure PushByRefId(ARefId: Integer);
    procedure PushByStack(AIndex: Integer);
    property Count: Integer read GetCount;
  end;

  TLuaFunctionArgs = class(TLuaResults)
  end;

implementation

uses
  Winapi.ActiveX, System.Math, System.AnsiStrings;

{ Helper }

type
  TLuaInternalCore = class(TPersistent)
  strict private type
    LuaRefId = packed record
      R: Integer;
      L: TLua;
    end;
  strict private
    class var InternalCore: TLuaInternalCore;
  strict private
    FLuaRefIds: TList<LuaRefId>;
  strict protected
    constructor Create; virtual; abstract;
  protected
    procedure Initialize;
  public
    class function GetInstance: TLuaInternalCore;
    destructor Destroy; override;
    procedure RegisterThread(AThread: TLuaThread);
    procedure UnregisterThread(AThread: TLuaThread);
    function AddRef(ARefId: Integer; ALua: TLua): Boolean;
    function HasRef(ARefId: Integer; ALua: TLua): Boolean;
    function RemoveRef(ARefId: Integer; ALua: TLua): Boolean;
  end;

  TLuaClassMethodCallback = function(L: TLuaState): Integer of object; cdecl;

  TLuaCallbackWrapperEvent = (lceNone, lceFunction, lceProcedure, lceMethod, lceClassConstruction,
    lceClassGarbageCollection, lceClassBaseMethod, lceClassMethod, lceClassProperty, lceClassIndexProperty);

  TLuaCallbackWrapper = class
  strict private type
    WrapperEvents = packed record
      case Event: TLuaCallbackWrapperEvent of
        lceNone: ();
        lceFunction: (Func: TLuaFunctionEvent);
        lceProcedure: (Proc: ^TLuaProcedure);
        lceMethod: (Method: TLuaMethodEvent);
        lceClassConstruction: (ClassConstruct: TLuaClassConstructionEvent);
        lceClassGarbageCollection: (ClassGarbageCollection: TLuaClassGarbageCollectionEvent);
        lceClassBaseMethod: (ClassBaseMethod: TLuaClassBaseMethodEvent);
        lceClassMethod: (ClassMethod: TLuaClassMethod);
        lceClassProperty: (ClassDefaultPropertyGet, ClassDefaultPropertySet: TLuaClassDefaultPropertyEvent);
        lceClassIndexProperty: (ClassIndexProperty: TLuaClassIndexProperty);
    end;
  strict private // Global class var
    class var CallbackWrappers: TList<TLuaCallbackWrapper>;
    class var CallbackWrappersRelease: TList<TLuaCallbackWrapper>;
  strict private // Prevent showing class fields
    FCallback: TLuaCFunctionEvent;
    FCallbackMethod: TLuaClassMethodCallback;
    FWrapperEvents: WrapperEvents;
  strict protected // Hide, this is class internal stuff, do NOT touch
    class procedure AddWrapper(AWrapper: TLuaCallbackWrapper);
    constructor Create;
    function CallbackFunc(L: TLuaState): Integer; cdecl;
  private // This is "public" for the unit
    class procedure Initialize;
    class procedure Finalize;
  public
    class function New(AFunc: TLuaFunctionEvent): TLuaCallbackWrapper; overload;
    class function New(AProc: TLuaProcedure): TLuaCallbackWrapper; overload;
    class function New(AMethod: TLuaMethodEvent): TLuaCallbackWrapper; overload;
    class function New(AClassConstruct: TLuaClassConstructionEvent): TLuaCallbackWrapper; overload;
    class function New(AClassGarbageCollection: TLuaClassGarbageCollectionEvent): TLuaCallbackWrapper; overload;
    class function New(AClassBaseMethod: TLuaClassBaseMethodEvent): TLuaCallbackWrapper; overload;
    class function New(AClassMethod: TLuaClassMethod): TLuaCallbackWrapper; overload;
    class function New(AClassDefaultPropertyGet, AClassDefaultPropertySet: TLuaClassDefaultPropertyEvent): TLuaCallbackWrapper; overload;
    class function New(AClassIndexProperty: TLuaClassIndexProperty): TLuaCallbackWrapper; overload;
    class procedure Release(ACallback: TLuaCFunctionEvent);
    class procedure PrepareRelease(ACallback: TLuaCFunctionEvent);
    class procedure CleanupRelease;
    destructor Destroy; override;
    property Callback: TLuaCFunctionEvent read FCallback;
  end;

{ Global }

{$IFNDEF CPUX64}
function MakeCdeclCallback(const Method: TMethod; StackSize: Shortint): Pointer;
type
  PCallbackPush = ^TCallbackPush;

  TCallbackPush = packed record
    // push dword ptr [esp+x]
    PushParmOps: Array [0..2] of Byte;
    PushParmVal: Shortint;
  end;

  PCallbackCall = ^TCallbackCall;

  TCallbackCall = packed record
    // push dword ptr [offset]
    PushDataOps: Array [0..1] of Byte;
    PushDataVal: Pointer;
    // call [offset]
    CallCodeOps: Array [0..1] of Byte;
    CallCodeVal: Pointer;
    // add esp,x
    AddEspXXOps: Array [0..1] of Byte;
    AddEspXXVal: Shortint;
    // ret
    Return: Byte;
  end;
var
  Size: Shortint;
  Loop: Shortint;
  Buff: Pointer;
begin
  if (StackSize < 0) or // check for invalid parameter and Shortint overflow
    (StackSize > High(Shortint) + 1 - 2 * SizeOf(Longword)) then
  begin
    Result := nil;
    Exit;
  end;
  Result := VirtualAlloc(nil, $100, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if Assigned(Result) then
  begin
    try
      Buff := Result;
      if StackSize <= 0 then
      begin
        Size := 0;
      end else
      begin
        // Copy parameters (used Longwords)
        Size := ((StackSize - 1) div SizeOf(Longword) + 1) * SizeOf(Longword);
        for Loop := 1 to Size div SizeOf(Longword) do
        begin
          with PCallbackPush(Buff)^ do
          begin
            PushParmOps[0] := $FF;
            PushParmOps[1] := $74;
            PushParmOps[2] := $24;
            PushParmVal := Size;
          end;
          Inc(PCallbackPush(Buff));
        end;
      end;
      with PCallbackCall(Buff)^ do
      begin
        // Push Self
        PushDataOps[0] := $FF;
        PushDataOps[1] := $35;
        PushDataVal := Addr(Method.Data);
        // Call Method
        CallCodeOps[0] := $FF;
        CallCodeOps[1] := $15;
        CallCodeVal := Addr(Method.Code);
        // Fix Stack
        AddEspXXOps[0] := $83;
        AddEspXXOps[1] := $C4;
        AddEspXXVal := Size + SizeOf(Longword);
        // Return
        Return := $C3;
      end;
    except
      VirtualFree(Result, 0, MEM_RELEASE);
      Result := nil;
    end;
  end;
end;

procedure FreeCdeclCallback(Callback: Pointer);
begin
  if Assigned(Callback) then
  begin
    VirtualFree(Callback, 0, MEM_RELEASE);
  end;
end;
{$ELSE}
function MakeCallback(const Method: TMethod; NumArgs: Shortint): Pointer;
const
  RegParamCount = 4;
  ShadowParamCount = 4;
  Size32Bit = 4;
  Size64Bit = 8;
  ShadowStack   = ShadowParamCount * Size64Bit;
  SkipParamCount = RegParamCount - ShadowParamCount;
  StackSrsOffset = 3;
  c64stack: Array[0..14] of Byte = (
    $48, $81, $ec, 00, 00, 00, 00,              // sub rsp,$0
    $4c, $89, $8c, $24, ShadowStack, 00, 00, 00 // mov [rsp+$20],r9
  );
  CopySrcOffset = 4;
  CopyDstOffset = 4;
  c64copy: Array[0..15] of Byte = (
    $4c, $8b, $8c, $24,  00, 00, 00, 00, // mov r9,[rsp+0]
    $4c, $89, $8c, $24, 00, 00, 00, 00   // mov [rsp+0],r9
  );
  RegMethodOffset = 10;
  RegSelfOffset = 11;
  c64regs: Array[0..28] of Byte = (
    $4d, $89, $c1,                            // mov r9,r8
    $49, $89, $d0,                            // mov r8,rdx
    $48, $89, $ca,                            // mov rdx,rcx
    $48, $b9, 00, 00, 00, 00, 00, 00, 00, 00, // mov rcx, Obj
    $48, $b8, 00, 00, 00, 00, 00, 00, 00, 00  // mov rax, MethodPtr
  );
  c64jump: Array[0..2] of Byte = (
    $48, $ff, $e0  // jump rax
  );
  CallOffset = 6;
  c64call: Array[0..10] of Byte = (
    $48, $ff, $d0,                 // call rax
    $48, $81,$c4,  00, 00, 00, 00, // add rsp,$0
    $c3                            // ret
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
    Size:=( 1 + ((NumArgs + 1 - SkipParamCount) div 2) * 2 ) * Size64Bit;   // 16 byte stack align

    Ptr2:=Ptr;
    Move(c64stack, Ptr^, SizeOf(c64stack));
    Inc(Ptr, StackSrsOffset);
    Move(Size, Ptr^, Size32Bit);
    Ptr:=Ptr2;
    Inc(Ptr, SizeOf(c64stack));

    for I:=0 to NumArgs - RegParamCount -1 do
    begin
      Ptr2:=Ptr;
      Move(c64copy, Ptr^, SizeOf(c64copy));
      Inc(Ptr, CopySrcOffset);
      Offset:=Size + (I + ShadowParamCount + 1) * Size64Bit;
      Move(Offset, Ptr^, Size32Bit);
      Inc(Ptr, CopyDstOffset + Size32Bit);
      Offset:=(I + ShadowParamCount + 1) * Size64Bit;
      Move(Offset, Ptr^, Size32Bit);
      Ptr:=Ptr2;
      Inc(Ptr, SizeOf(c64copy));
    end;
  end;

  Ptr2:=Ptr;
  Move(c64regs, Ptr^, SizeOf(c64regs));
  Inc(Ptr, RegSelfOffset);
  Move(Method.Data, Ptr^, SizeOf(Method.Data));
  Inc(Ptr, RegMethodOffset);
  Move(Method.Code, Ptr^, SizeOf(Method.Code));
  Ptr:=Ptr2;
  Inc(Ptr, SizeOf(c64regs));

  if NumArgs < RegParamCount then
  begin
    Move(c64jump, Ptr^, SizeOf(c64jump))
  end else
  begin
    Move(c64call, Ptr^, SizeOf(c64call));
    Inc(Ptr, CallOffset);
    Move(Size, Ptr^, Size32Bit);
  end;

  Result:=CallbackPtr;
end;

procedure FreeCallback(Callback: Pointer);
begin
  if Assigned(Callback) then
  begin
    VirtualFree(Callback, 0, MEM_RELEASE);
  end;
end;
{$ENDIF}

function LuaDefaultAllocator(ud, ptr: Pointer; osize, nsize: size_t): Pointer; cdecl;
var
  Lua: TLua;
begin
  Lua:=ud;
  Result:=nil;

  if nsize = 0 then
  begin
    FreeMemory(ptr);

    Dec(Lua.FMemoryUsage, osize);
  end else
  begin
    Result:=ReallocMemory(ptr, nsize);

    Dec(Lua.FMemoryUsage, osize);
    Inc(Lua.FMemoryUsage, nsize);
  end;
end;

{ TLuaInternalCore }

procedure TLuaInternalCore.Initialize;
begin
  // Initialize classes
  TLuaCallbackWrapper.Initialize;

  // Create fields
  FLuaRefIds:=TList<LuaRefId>.Create;
end;

procedure TLuaInternalCore.RegisterThread(AThread: TLuaThread);
begin

end;

procedure TLuaInternalCore.UnregisterThread(AThread: TLuaThread);
begin

end;

function TLuaInternalCore.RemoveRef(ARefId: Integer; ALua: TLua): Boolean;
var
  I: Integer;
begin
  Result:=False;

  for I:=0 to FLuaRefIds.Count - 1 do
  begin
    if (FLuaRefIds[I].R = ARefId) AND (FLuaRefIds[I].L = ALua) then
    begin
      Result:=True;
      FLuaRefIds.Delete(I);
      Break;
    end;
  end;
end;

function TLuaInternalCore.AddRef(ARefId: Integer; ALua: TLua): Boolean;
var
  Item: LuaRefId;
begin
  Result:=False;

  if NOT HasRef(ARefId, ALua) then
  begin
    Item.R:=ARefId;
    Item.L:=ALua;

    Result:=FLuaRefIds.Add(Item) >= 0;
  end;
end;

function TLuaInternalCore.HasRef(ARefId: Integer; ALua: TLua): Boolean;
var
  I: Integer;
begin
  Result:=False;

  for I:=0 to FLuaRefIds.Count - 1 do
  begin
    if (FLuaRefIds[I].R = ARefId) AND (FLuaRefIds[I].L = ALua) then
    begin
      Result:=True;
      Break;
    end;
  end;
end;

destructor TLuaInternalCore.Destroy;
begin
  // Finalize classes
  TLuaCallbackWrapper.Finalize;

  // Free up fields
  FreeAndNil(FLuaRefIds);

  inherited;
end;

class function TLuaInternalCore.GetInstance: TLuaInternalCore;
begin
  try
    if NOT Assigned(InternalCore) then
    begin
      InternalCore:=inherited Create;
      InternalCore.Initialize;
    end;
  finally
    Result:=InternalCore;
  end;
end;

{ TLuaCallbackWrapper }

constructor TLuaCallbackWrapper.Create;
begin
  inherited Create;

  FCallbackMethod:=CallbackFunc;

  {$IFNDEF CPUX64}
  FCallback:=lua_CFunction(MakeCdeclCallback(TMethod(FCallbackMethod), SizeOf(NativeInt)));
  {$ELSE}
  FCallback:=lua_CFunction(MakeCallback(TMethod(FCallbackMethod), 1));
  {$ENDIF}

  TLuaCallbackWrapper.CallbackWrappers.Add(Self);
end;

destructor TLuaCallbackWrapper.Destroy;
begin
  if FWrapperEvents.Event = lceProcedure then
  begin
    System.Finalize(FWrapperEvents.Proc^);
    FreeMemory(FWrapperEvents.Proc);
  end;

  {$IFNDEF CPUX64}
  FreeCdeclCallback(@FCallback);
  {$ELSE}
  FreeCallback(@FCallback);
  {$ENDIF}

  inherited;
end;

class procedure TLuaCallbackWrapper.Initialize;
begin
  TLuaCallbackWrapper.CallbackWrappers:=TList<TLuaCallbackWrapper>.Create;
  TLuaCallbackWrapper.CallbackWrappersRelease:=TList<TLuaCallbackWrapper>.Create;
end;

class procedure TLuaCallbackWrapper.Finalize;
begin
  FreeAndNil(TLuaCallbackWrapper.CallbackWrappersRelease);
  FreeAndNil(TLuaCallbackWrapper.CallbackWrappers);
end;

class function TLuaCallbackWrapper.New(AFunc: TLuaFunctionEvent): TLuaCallbackWrapper;
begin
  Result:=TLuaCallbackWrapper.Create;
  with Result do
  begin
    FWrapperEvents.Event:=lceFunction;
    FWrapperEvents.Func:=AFunc;
  end;
end;

class function TLuaCallbackWrapper.New(AProc: TLuaProcedure): TLuaCallbackWrapper;
begin
  Result:=TLuaCallbackWrapper.Create;
  with Result do
  begin
    FWrapperEvents.Event:=lceProcedure;
    FWrapperEvents.Proc:=GetMemory(SizeOf(TLuaProcedure));
    System.Initialize(FWrapperEvents.Proc^);
    FWrapperEvents.Proc^:=AProc;
  end;
end;

class function TLuaCallbackWrapper.New(AMethod: TLuaMethodEvent): TLuaCallbackWrapper;
begin
  Result:=TLuaCallbackWrapper.Create;
  with Result do
  begin
    FWrapperEvents.Event:=lceMethod;
    FWrapperEvents.Method:=AMethod;
  end;
end;

class function TLuaCallbackWrapper.New(AClassConstruct: TLuaClassConstructionEvent): TLuaCallbackWrapper;
begin
  Result:=TLuaCallbackWrapper.Create;
  with Result do
  begin
    FWrapperEvents.Event:=lceClassConstruction;
    FWrapperEvents.ClassConstruct:=AClassConstruct;
  end;
end;

class function TLuaCallbackWrapper.New(AClassGarbageCollection: TLuaClassGarbageCollectionEvent): TLuaCallbackWrapper;
begin
  Result:=TLuaCallbackWrapper.Create;
  with Result do
  begin
    FWrapperEvents.Event:=lceClassGarbageCollection;
    FWrapperEvents.ClassGarbageCollection:=AClassGarbageCollection;
  end;
end;

class function TLuaCallbackWrapper.New(AClassMethod: TLuaClassMethod): TLuaCallbackWrapper;
begin
  Result:=TLuaCallbackWrapper.Create;
  with Result do
  begin
    FWrapperEvents.Event:=lceClassMethod;
    FWrapperEvents.ClassMethod:=AClassMethod;
  end;
end;

class function TLuaCallbackWrapper.New(AClassBaseMethod: TLuaClassBaseMethodEvent): TLuaCallbackWrapper;
begin
  Result:=TLuaCallbackWrapper.Create;
  with Result do
  begin
    FWrapperEvents.Event:=lceClassBaseMethod;
    FWrapperEvents.ClassBaseMethod:=AClassBaseMethod;
  end;
end;

class function TLuaCallbackWrapper.New(AClassIndexProperty: TLuaClassIndexProperty): TLuaCallbackWrapper;
begin
  Result:=TLuaCallbackWrapper.Create;
  with Result do
  begin
    FWrapperEvents.Event:=lceClassIndexProperty;
    FWrapperEvents.ClassIndexProperty:=AClassIndexProperty;
  end;
end;

class procedure TLuaCallbackWrapper.PrepareRelease(ACallback: TLuaCFunctionEvent);
var
  I: Integer;
begin
  for I:=0 to CallbackWrappers.Count - 1 do
  begin
    if @CallbackWrappers[I].Callback = @ACallback then
    begin
      CallbackWrappersRelease.Add(CallbackWrappers[I]);
      CallbackWrappers.Delete(I);

      Break;
    end;
  end;
end;

class function TLuaCallbackWrapper.New(AClassDefaultPropertyGet, AClassDefaultPropertySet: TLuaClassDefaultPropertyEvent): TLuaCallbackWrapper;
begin
  Result:=TLuaCallbackWrapper.Create;
  with Result do
  begin
    FWrapperEvents.Event:=lceClassProperty;
    FWrapperEvents.ClassDefaultPropertyGet:=AClassDefaultPropertyGet;
    FWrapperEvents.ClassDefaultPropertySet:=AClassDefaultPropertySet;
  end;
end;

class procedure TLuaCallbackWrapper.AddWrapper(AWrapper: TLuaCallbackWrapper);
begin
  if Assigned(TLuaCallbackWrapper.CallbackWrappers) then
  begin
    TLuaCallbackWrapper.CallbackWrappers.Add(AWrapper);
  end;
end;

class procedure TLuaCallbackWrapper.Release(ACallback: TLuaCFunctionEvent);
var
  I: Integer;
begin
  for I:=0 to CallbackWrappers.Count - 1 do
  begin
    if @CallbackWrappers[I].Callback = @ACallback then
    begin
      try
        CallbackWrappers[I].Free;
      finally
        CallbackWrappers.Delete(I);
      end;

      Break;
    end;
  end;
end;

function TLuaCallbackWrapper.CallbackFunc(L: TLuaState): Integer;
var
  Lua: TLua;
  Args: TLuaArgs;
  Value: TLuaValue;
  Clazz: TLuaClass;
  Results: TLuaResults;
  AllowConstruct: Boolean;
  Prop: TLuaClassProperty;
  Blueprint: TLuaClassBlueprint;
  UserClass: TObject;
begin
  Result:=0;
  Args:=nil;
  Results:=nil;
  UserClass:=nil;
  AllowConstruct:=True;

  // Get lua object
  Lua:=TLua.Acquire(L);
  try
    // Always create the results object, we need it anyways
    Results:=TLuaResults.Create(Lua);

    // So, what kind of event is diz?
    case FWrapperEvents.Event of
      // Lua C function callback
      // ---------------------------------------------------------------------------------------------------------------
      lceFunction:
      begin
        // Process args
        Args:=TLuaArgs.Create(Lua);

        // Call target
        FWrapperEvents.Func(Lua, Args, Results);
      end;
      // Method callback
      // ---------------------------------------------------------------------------------------------------------------
      lceProcedure:
      begin
        // Process args
        Args:=TLuaArgs.Create(Lua);

        // Call the procedure
        FWrapperEvents.Proc^(Lua, Args, Results);
      end;
      // Method callback
      // ---------------------------------------------------------------------------------------------------------------
      lceMethod:
      begin
        // Process args
        Args:=TLuaArgs.Create(Lua);

        // Call target
        FWrapperEvents.Method(Lua, Args, Results);
      end;
      // Class construction callback
      // ---------------------------------------------------------------------------------------------------------------
      lceClassConstruction:
      begin
        // Get blueprint
        Blueprint:=TLuaClassBlueprint.Acquire(Lua.State);

        // Process args
        Args:=TLuaArgs.Create(Lua);

        // Is diz a blueprint? Can it be constructed?
        if Assigned(Blueprint) AND Blueprint.AllowConstruct then
        begin
          try
            FWrapperEvents.ClassConstruct(Lua, Blueprint, Args, UserClass, AllowConstruct);
          finally
            if AllowConstruct then
            begin
              // Construct the class
              Clazz:=Blueprint.Construct;

              // Set the user class object
              Clazz.FUserClass:=UserClass;

              // Push class to result list
              Results.PushByRefId(Clazz.RefId);
            end;
          end;
        end;
      end;
      // Class destruction callback
      // ---------------------------------------------------------------------------------------------------------------
      lceClassGarbageCollection:
      begin
        // Acquire the class
        Clazz:=TLuaClass.Acquire(Lua.State);

        if Assigned(Clazz) then
        begin
          // Call target
          FWrapperEvents.ClassGarbageCollection(Lua, Clazz);
        end;
      end;
      // Class base method callback
      // ---------------------------------------------------------------------------------------------------------------
      lceClassBaseMethod:
      begin
        // Acquire the class
        Clazz:=TLuaClass.Acquire(Lua.State);

        // Get args
        Args:=TLuaArgs.Create(Lua);

        // Call the target
        if Assigned(Clazz) then
        begin
          FWrapperEvents.ClassBaseMethod(Clazz.Lua, Clazz, Args, Results);
        end;
      end;
      // Class method callback
      // ---------------------------------------------------------------------------------------------------------------
      lceClassMethod:
      begin
        // Acquire the class
        Clazz:=TLuaClass.Acquire(Lua.State);

        // Get args
        Args:=TLuaArgs.Create(Lua);

        // Call the target
        if Assigned(Clazz) then
        begin
          FWrapperEvents.ClassMethod.OnCallback(Clazz.Lua, Clazz, FWrapperEvents.ClassMethod, Args, Results);
        end;
      end;
      // Class property callback
      // ---------------------------------------------------------------------------------------------------------------
      lceClassProperty:
      begin
        // Acquire the class
        Clazz:=TLuaClass.Acquire(Lua.State);

        if Assigned(Clazz) then
        begin
          // Get args
          Args:=TLuaArgs.Create(Lua);

          // Process, get or set?
          case Args.Count of
            1: // Get
            begin
              Value:=TLuaValue.Clear(Lua);
              try
                Prop:=Clazz.GetProperties(Args[0].AsStr);
                if Assigned(Prop) then
                  Prop.OnGetCallback(Clazz.Lua, Clazz, Prop, Value)
                else
                  FWrapperEvents.ClassDefaultPropertyGet(Clazz.Lua, Clazz, Args[0], Value);

                // Push value to results
                Results.PushValue(Value);
              finally
                Value.Free;
              end;
            end;
            2: // Set
            begin
              Prop:=Clazz.GetProperties(Args[0].AsStr);
              if Assigned(Prop) then
                Prop.OnSetCallback(Clazz.Lua, Clazz, Prop, Args[1])
              else
                FWrapperEvents.ClassDefaultPropertySet(Lua, Clazz, Args[0], Args[1]);
            end;
          end;
        end else
        begin
          Blueprint:=TLuaClassBlueprint.Acquire(Lua.State);

          if Assigned(Blueprint) then
          begin
            // Get the args
            Args:=TLuaArgs.Create(Lua);

            // Check if this is a new method for the class
            if Args.Check([ltString, ltFunction]) then
            begin
              Blueprint.AddMethod(Args[0].AsStr, Args[1].AsFunc);
            end;
          end else
          begin
            // Get the args for whatever we have here...
            Args:=TLuaArgs.Create(Lua);

            if Args.Check([ltTable, ltString]) then // Seems we found a lib constant request...
            begin
              Value:=TLuaValue.Clear(Lua);
              try
                FWrapperEvents.ClassDefaultPropertyGet(Lua, nil, Args[1], Value);

                // Push value to results
                Results.PushValue(Value);
              finally
                Value.Free;
              end;
            end;
          end;
        end;
      end;
      // Class index property callback
      // ---------------------------------------------------------------------------------------------------------------
      lceClassIndexProperty:
      begin
        if Assigned(FWrapperEvents.ClassIndexProperty) then
        begin
          // Acquire the class
          Clazz:=TLuaClass.Acquire(Lua.State);

          // Check the validity
          if Assigned(Clazz) then
          begin
            // Get args
            Args:=TLuaArgs.Create(Lua);

            // Process, get or set?
            case Args.Count of
              1:
              begin
                Value:=TLuaValue.Clear(Lua);
                try
                  FWrapperEvents.ClassIndexProperty.OnGetCallback(Clazz.Lua, Clazz, FWrapperEvents.ClassIndexProperty, Args[0], Value);

                  // Push value to results
                  Results.PushValue(Value);
                finally
                  Value.Free;
                end;
              end;
              2:
              begin
                FWrapperEvents.ClassIndexProperty.OnSetCallback(Clazz.Lua, Clazz, FWrapperEvents.ClassIndexProperty, Args[0], Args[1]);
              end;
            end;
          end;
        end;
      end;
      // New or unimplemented
      // ---------------------------------------------------------------------------------------------------------------
      else
      begin
        raise Exception.Create('New or unimplemented wrapper event!');
      end;
    end;

    // Haz results?
    if Assigned(Results) then
    begin
      // Apply the results to the stack
      Results.ApplyToStack;

      // Return the result count
      Result:=Results.Count;
    end;
  finally
    // Free args object
    if Assigned(Args) then
    begin
      Args.Free;
    end;

    // Free results object
    if Assigned(Results) then
    begin
      Results.Free;
    end;

    // Is this volatile? Free this shit...
    if Lua.IsVolatile then
    begin
      Lua.Free;
    end;
  end;
end;

class procedure TLuaCallbackWrapper.CleanupRelease;
var
  I: Integer;
begin
  try
    for I:=0 to CallbackWrappersRelease.Count - 1 do
    try
      CallbackWrappersRelease[I].Free;
    except
    end;
  finally
    CallbackWrappersRelease.Clear;
  end;
end;

{ TLuaObject }

constructor TLuaObject.Create(ALua: TLua; AType: TLuaObjectAcquisition = oaDefault; AValue: Integer = 0);
begin
  inherited Create;

  FLua:=ALua;
  FRefId:=LUA_NOREF;
  FTyp:=LUA_TNONE;

  if AType = oaStackIndex then
  begin
    // Create ref from the given stack index
    FromStack(AValue);
  end else
  if AType = oaRefId then
  begin
    // Push the refed value to the stack
    FLua.Stack.RawGetI(LUA_REGISTRYINDEX, AValue);

    // Create a copy
    FromStack(-1);

    // Remove the refed value
    PopFromStack;
  end else
  if AType = oaDefault then
  begin
    // Create ref from top most stack index
    FromStack(-1);

    // Remove the original value from stack
    PopFromStack;
  end;

  Initialize;
end;

destructor TLuaObject.Destroy;
begin
  UnRef;

  inherited;
end;

class function TLuaObject.FromGlobal(ALua: TLua; AName: String): TLuaObject;
begin
  ALua.Stack.GetGlobal(AName);
  Result:=Create(ALua);
end;

function TLuaObject.CheckState(ALua: TLua): Boolean;
begin
  Result:=FLua.State = ALua.State;
end;

function TLuaObject.Compare(AValue: TLuaObject; AOperation: TLuaCompareOperation): Boolean;
begin
  AValue.PushToStack;
  PushToStack;
  try
    Result:=lua_compare(FLua.State, -1, -2, Ord(AOperation));
  finally
    PopFromStack;
    AValue.PopFromStack;
  end;
end;

function TLuaObject.CopyToRef: Integer;
begin
  PushToStack;

  Result:=FLua.Stack.Ref(LUA_REGISTRYINDEX);

  TLuaInternalCore.GetInstance.AddRef(Result, FLua);
end;

procedure TLuaObject.UnRef;
begin
  if (FRefId <> LUA_NOREF) AND ((FRefId <> LUA_REFNIL)) then
  begin
    FLua.Stack.UnRef(LUA_REGISTRYINDEX, FRefId);
  end;

  FRefId:=LUA_NOREF;
  FTyp:=LUA_TNONE;
end;

procedure TLuaObject.Assign(Source: TPersistent);
begin
  if Source IS TLuaObject then
  begin
    // Remove existing reference
    UnRef;

    FLua:=TLuaObject(Source).FLua;

    TLuaObject(Source).PushToStack;
    FromStack(-1);
    TLuaObject(Source).PopFromStack;
  end;
end;

procedure TLuaObject.FromStack(AIndex: Integer);
begin
  // Remove existing reference
  UnRef;

  // Always duplicate the stack entry
  FLua.Stack.PushValue(AIndex);

  // Get the type and reference it
  FTyp:=FLua.Stack.Typ(-1);
  FRefId:=FLua.Stack.Ref(LUA_REGISTRYINDEX);
end;

function TLuaObject.GetTyp: TLuaType;
begin
  Result:=TLuaType(FTyp);
end;

function TLuaObject.GetTypName: String;
begin
  Result:=String(System.AnsiStrings.StrPas(lua_typename(FLua.State, FTyp)));
end;

procedure TLuaObject.Initialize;
begin
end;

procedure TLuaObject.PopFromStack;
begin
  FLua.Stack.Pop;
end;

function TLuaObject.PushToStack: Integer;
begin
  // Push RefId value to the stack
  FLua.Stack.RawGetI(LUA_REGISTRYINDEX, FRefId);

  // Return stack position
  Result:=FLua.Stack.Top;
end;

procedure TLuaObject.SetGlobal(AName: String);
begin
  PushToStack;
  FLua.Stack.SetGlobal(AName);
end;

{ TLuaValue }

destructor TLuaValue.Destroy;
begin
  // Remove the valie from the TLua cleanup list, if it is in there...
  FLua.NotifyCleanup(Self, opRemove);

  inherited;
end;

class function TLuaValue.New(ALua: TLua; AIndex: Integer): TLuaValue;
begin
  Result:=TLuaValue.Create(ALua, oaStackIndex, AIndex);
end;

class function TLuaValue.Clear(ALua: TLua): TLuaValue;
begin
  Result:=TLuaValue.Create(ALua, oaNone);
end;

class function TLuaValue.Copy(AValue: TLuaValue): TLuaValue;
begin
  Result:=TLuaValue.Create(AValue.Lua, oaNone);
  Result.Assign(AValue);
end;

class function TLuaValue.FromRefId(ALua: TLua; ARefId: Integer): TLuaValue;
begin
  Result:=TLuaValue.Create(ALua, oaRefId, ARefId);
end;

class function TLuaValue.FromValue(ALua: TLua; AValue: Double): TLuaValue;
begin
  Result:=TLuaValue.Create(ALua, oaNone);
  Result.SetAsFloat(AValue);
end;

class function TLuaValue.FromValue(ALua: TLua; AValue: Int64): TLuaValue;
begin
  Result:=TLuaValue.Create(ALua, oaNone);
  Result.SetAsInt(AValue);
end;

class function TLuaValue.FromValue(ALua: TLua; AValue: String): TLuaValue;
begin
  Result:=TLuaValue.Create(ALua, oaNone);
  Result.SetAsStr(AValue);
end;

class function TLuaValue.FromValue(ALua: TLua; AValue: Boolean): TLuaValue;
begin
  Result:=TLuaValue.Create(ALua, oaNone);
  Result.SetAsBool(AValue);
end;

class function TLuaValue.FromValue(ALua: TLua; AValue: Pointer): TLuaValue;
begin
  Result:=TLuaValue.Create(ALua, oaNone);
  Result.SetAsPtr(AValue);
end;

procedure TLuaValue.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source IS TLuaValue then
  begin
    FValue:=TLuaValue(Source).FValue;
  end;
end;

procedure TLuaValue.SetClear;
begin
  UnRef;
end;

procedure TLuaValue.SetNil;
begin
  UnRef;

  FValue.V:=Null;
  FTyp:=LUA_TNIL;
end;

procedure TLuaValue.UnRef;
begin
  // Free self allocated objects
  case FValue.K of
    lvtFunction: FValue.F.Free;
    lvtTable: FValue.T.Free;
    lvtThread: FValue.H.Free;
  end;

  FValue.V:=Unassigned;
  FValue.K:=lvtEmpty;
  FValue.X:=0;

  // Continue
  inherited;
end;

procedure TLuaValue.SetAsBlueprint(const Value: TLuaClassBlueprint);
begin
  Value.PushToStack;
  FromStack(-1);
  PopFromStack;

  Initialize;
end;

procedure TLuaValue.SetAsBool(const Value: Boolean);
begin
  FLua.Stack.PushBoolean(Value);
  FromStack(-1);
  PopFromStack;

  Initialize;
end;

procedure TLuaValue.SetAsClass(const Value: TLuaClass);
begin
  Value.PushToStack;
  FromStack(-1);
  PopFromStack;

  Initialize;
end;

procedure TLuaValue.SetAsFloat(const Value: Double);
begin
  FLua.Stack.PushNumber(Value);
  FromStack(-1);
  PopFromStack;

  Initialize;
end;

procedure TLuaValue.SetAsFunc(const Value: TLuaFunction);
begin
  Value.PushToStack;
  FromStack(-1);
  PopFromStack;

  Initialize;
end;

procedure TLuaValue.SetAsInt(const Value: Int64);
begin
  FLua.Stack.PushInteger(Value);
  FromStack(-1);
  PopFromStack;

  Initialize;
end;

procedure TLuaValue.SetAsPtr(const Value: Pointer);
begin
  FLua.Stack.PushPointer(Value);
  FromStack(-1);
  PopFromStack;

  Initialize;
end;

procedure TLuaValue.SetAsStr(const Value: String);
begin
  FLua.Stack.PushString(Value);
  FromStack(-1);
  PopFromStack;

  Initialize;
end;

procedure TLuaValue.SetAsTable(const Value: TLuaTable);
begin
  Value.PushToStack;
  FromStack(-1);
  PopFromStack;

  Initialize;
end;

procedure TLuaValue.SetAsVariant(const Value: Variant);
begin
  FLua.Stack.PushVariant(Value);
  FromStack(-1);
  PopFromStack;

  Initialize;
end;

procedure TLuaValue.Initialize;
begin
  PushToStack;

  FValue.X:=0;

  case FTyp of
    LUA_TNONE,
    LUA_TNIL:
    begin
      FValue.K:=lvtEmpty;
      FValue.V:=Null;
    end;
    LUA_TBOOLEAN:
    begin
      FValue.K:=lvtSimple;
      FValue.V:=FLua.Stack.ToBoolean(-1);
    end;
    LUA_TLIGHTUSERDATA:
    begin
      FValue.K:=lvtPointer;
      FValue.P:=FLua.Stack.ToPointer(-1);
    end;
    LUA_TNUMBER:
    begin
      if FLua.Stack.IsInteger(-1) then
      begin
        FValue.K:=lvtSimple;
        FValue.V:=VarAsType(FLua.Stack.ToInteger(-1), varInt64);
      end else
      begin
        FValue.K:=lvtSimple;
        FValue.V:=VarAsType(FLua.Stack.ToNumber(-1), varDouble);
      end;
    end;
    LUA_TSTRING:
    begin
      FValue.K:=lvtSimple;
      FValue.V:=FLua.Stack.ToString(-1);
    end;
    LUA_TTABLE:
    begin
      // Check if this is a blueprint
      FValue.K:=lvtBlueprint;
      FValue.B:=TLuaClassBlueprint.Acquire(FLua.State, True);

      // No bp? Maybe a class?
      if NOT Assigned(FValue.B) then
      begin
        FValue.K:=lvtClass;
        FValue.C:=TLuaClass.Acquire(FLua.State, True);
      end;

      // Okok now we can assume it is "only" a table
      if NOT Assigned(FValue.C) then
      begin
        FValue.K:=lvtTable;
        FValue.T:=TLuaTable.New(FLua, -1);
      end;
    end;
    LUA_TFUNCTION:
    begin
      FValue.K:=lvtFunction;
      FValue.F:=TLuaFunction.New(FLua, -1);
    end;
    LUA_TUSERDATA:
    begin
      FValue.K:=lvtPointer;
      FValue.P:=lua_touserdata(FLua.State, -1);
    end;
    LUA_TTHREAD:
    begin
      FValue.K:=lvtThread;
      FValue.H:=TLuaThread.Create(FLua, -1);
    end;
  end;

  PopFromStack;
end;

function TLuaValue.GetAsBlueprint: TLuaClassBlueprint;
begin
  if FValue.K = lvtBlueprint then
    Result:=FValue.B
  else
    Result:=nil;
end;

function TLuaValue.GetAsBool: Boolean;
begin
  if FValue.K = lvtSimple then
    Result:=VarAsType(FValue.V, varBoolean)
  else
    Result:=False;
end;

function TLuaValue.GetAsClass: TLuaClass;
begin
  if FValue.K = lvtClass then
    Result:=FValue.C
  else
    Result:=nil;
end;

function TLuaValue.GetAsFloat: Double;
begin
  if FValue.K = lvtSimple then
    Result:=VarAsType(FValue.V, varDouble)
  else
    Result:=0.0;
end;

function TLuaValue.GetAsFunc: TLuaFunction;
begin
  if FValue.K = lvtFunction then
    Result:=FValue.F
  else
    Result:=nil;
end;

function TLuaValue.GetAsInt: Int64;
begin
  if FValue.K = lvtSimple then
    Result:=VarAsType(FValue.V, varInt64)
  else
    Result:=0;
end;

function TLuaValue.GetAsPtr: Pointer;
begin
  if FValue.K = lvtPointer then
    Result:=FValue.P
  else
    Result:=nil;
end;

function TLuaValue.GetAsStr: String;
begin
  Result:='';

  if FValue.K = lvtSimple then
    Result:=VarToStrDef(FValue.V, '')
  else
    case FValue.K of
      lvtPointer   : Result:='0x' + IntToHex(NativeInt(FValue.P), SizeOf(NativeInt) * 2);
      lvtFunction  : Result:='0x' + IntToHex(NativeInt(FValue.F), SizeOf(NativeInt) * 2);
      lvtBlueprint : Result:='0x' + IntToHex(NativeInt(FValue.B), SizeOf(NativeInt) * 2);
      lvtClass     : Result:='0x' + IntToHex(NativeInt(FValue.C), SizeOf(NativeInt) * 2);
      lvtTable     : Result:='0x' + IntToHex(NativeInt(FValue.T), SizeOf(NativeInt) * 2);
      lvtThread    : Result:='0x' + IntToHex(NativeInt(FValue.H), SizeOf(NativeInt) * 2);
    end;
end;

function TLuaValue.GetAsTable: TLuaTable;
begin
  if FValue.K = lvtTable then
    Result:=FValue.T
  else
    Result:=nil;
end;

function TLuaValue.GetAsVariant: Variant;
begin
  Result:=Unassigned;

  if FValue.K = lvtSimple then
  begin
    Result:=FValue.V;
  end;
end;

function TLuaValue.GetIsBlueprint: Boolean;
begin
  Result:=FValue.K = lvtBlueprint;
end;

function TLuaValue.GetIsBool: Boolean;
begin
  Result:=FTyp = LUA_TBOOLEAN;
end;

function TLuaValue.GetIsClass: Boolean;
begin
  Result:=FValue.K = lvtClass;
end;

function TLuaValue.GetIsFloat: Boolean;
begin
  Result:=(FTyp = LUA_TNUMBER) AND (VarType(FValue.V) = varDouble);
end;

function TLuaValue.GetIsFunc: Boolean;
begin
  Result:=FTyp = LUA_TFUNCTION;
end;

function TLuaValue.GetIsInt: Boolean;
begin
  Result:=(FTyp = LUA_TNUMBER) AND (VarType(FValue.V) = varInt64);
end;

function TLuaValue.GetIsNil: Boolean;
begin
  Result:=FValue.K = lvtEmpty;
end;

function TLuaValue.GetIsPtr: Boolean;
begin
  Result:=FValue.K = lvtPointer;
end;

function TLuaValue.GetIsSet: Boolean;
begin
  Result:=FValue.K <> lvtEmpty;
end;

function TLuaValue.GetIsStr: Boolean;
begin
  Result:=FTyp = LUA_TSTRING;
end;

function TLuaValue.GetIsTable: Boolean;
begin
  Result:=FValue.K = lvtTable;
end;

{ TLuaTableRecord }

constructor TLuaTableRecord.Create(ATable: TLuaTable; AKeyIdx, AValIdx: Integer);
begin
  inherited Create(ATable.Lua, oaStackIndex, AValIdx);

  FIsIndexKey:=False;
  FTable:=ATable;
  FKey:=TLuaValue.New(FLua, AKeyIdx);

  if FKey.IsInt then
  begin
    FIsIndexKey:=True;
  end;
end;

destructor TLuaTableRecord.Destroy;
begin
  FreeAndNil(FKey);

  inherited;
end;

procedure TLuaTableRecord.SetAsBlueprint(const Value: TLuaClassBlueprint);
begin
end;

procedure TLuaTableRecord.SetAsBool(const Value: Boolean);
begin
  // Push table to stack
  FTable.PushToStack;

  // Set new values
  FKey.PushToStack;
  FLua.Stack.PushBoolean(Value);
  FLua.Stack.SetTable(-3);

  // Remove the table from stack
  PopFromStack;
end;

procedure TLuaTableRecord.SetAsClass(const Value: TLuaClass);
begin
  // Push table to stack
  FTable.PushToStack;

  // Set new values
  FKey.PushToStack;
  Value.PushToStack;
  FLua.Stack.SetTable(-3);

  // Remove the table from stack
  PopFromStack;
end;

procedure TLuaTableRecord.SetAsFloat(const Value: Double);
begin
  // Push table to stack
  FTable.PushToStack;

  // Set new values
  FKey.PushToStack;
  FLua.Stack.PushNumber(Value);
  FLua.Stack.SetTable(-3);

  // Remove the table from stack
  PopFromStack;
end;

procedure TLuaTableRecord.SetAsFunc(const Value: TLuaFunction);
begin
  // Push table to stack
  FTable.PushToStack;

  // Set new values
  FKey.PushToStack;
  Value.PushToStack;
  FLua.Stack.SetTable(-3);

  // Remove the table from stack
  PopFromStack;
end;

procedure TLuaTableRecord.SetAsInt(const Value: Int64);
begin
  // Push table to stack
  FTable.PushToStack;

  // Set new values
  FKey.PushToStack;
  FLua.Stack.PushInteger(Value);
  FLua.Stack.SetTable(-3);

  // Remove the table from stack
  PopFromStack;
end;

procedure TLuaTableRecord.SetAsPtr(const Value: Pointer);
begin
  // Push table to stack
  FTable.PushToStack;

  // Set new values
  FKey.PushToStack;
  FLua.Stack.PushPointer(Value);
  FLua.Stack.SetTable(-3);

  // Remove the table from stack
  PopFromStack;
end;

procedure TLuaTableRecord.SetAsStr(const Value: string);
begin
  // Push table to stack
  FTable.PushToStack;

  // Set new values
  FKey.PushToStack;
  FLua.Stack.PushString(Value);
  FLua.Stack.SetTable(-3);

  // Remove the table from stack
  PopFromStack;
end;

procedure TLuaTableRecord.SetAsTable(const Value: TLuaTable);
begin
  // Do not allow to set the table to itself, this will fuck up things...
  if FTable.Compare(Value, coEqual) then
  begin
    raise ELuaException.Create('Table recursion prevented!');
  end;

  // Push table to stack
  FTable.PushToStack;

  // Set new values
  FKey.PushToStack;
  Value.PushToStack;
  FLua.Stack.SetTable(-3);

  // Remove the table from stack
  PopFromStack;
end;

procedure TLuaTableRecord.SetAsVariant(const Value: Variant);
begin
  // Push table to stack
  FTable.PushToStack;

  // Set new values
  FKey.PushToStack;
  FLua.Stack.PushVariant(Value);
  FLua.Stack.SetTable(-3);

  // Remove the table from stack
  PopFromStack;
end;

{ TLuaTable.KeyValue }

class function TLuaTable.KeyValue.New(K: Variant; V: TLuaTableRecord): KeyValue;
begin
  Result.K:=K;
  Result.V:=V;
end;

{ TLuaTable }

class function TLuaTable.New(ALua: TLua): TLuaTable;
begin
  Result:=TLuaTable.Create(ALua, oaNone);

  with Result do
  begin
    FLua.Stack.NewTable;
    FromStack(-1);
    PopFromStack;
  end;
end;

class function TLuaTable.New(ALua: TLua; AName: String): TLuaTable;
begin
  Result:=New(ALua);
  Result.Name:=AName;
end;

class function TLuaTable.New(ALua: TLua; AIndex: Integer): TLuaTable;
begin
  Result:=TLuaTable.Create(ALua, oaStackIndex, AIndex);
end;

procedure TLuaTable.SetAsBlueprint(Key: Variant; const Value: TLuaClassBlueprint);
begin
end;

procedure TLuaTable.SetAsBool(Key: Variant; const Value: Boolean);
var
  Entry: TLuaTableRecord;
begin
  if GetRecordByKey(Key, Entry) then
  begin
    Entry.AsBool:=Value;
    Update;
  end;
end;

procedure TLuaTable.SetAsClass(Key: Variant; const Value: TLuaClass);
var
  Entry: TLuaTableRecord;
begin
  if GetRecordByKey(Key, Entry) then
  begin
    Entry.AsClass:=Value;
    Update;
  end;
end;

procedure TLuaTable.SetAsFloat(Key: Variant; const Value: Double);
var
  Entry: TLuaTableRecord;
begin
  if GetRecordByKey(Key, Entry) then
  begin
    Entry.AsFloat:=Value;
    Update;
  end;
end;

procedure TLuaTable.SetAsFunc(Key: Variant; const Value: TLuaFunction);
var
  Entry: TLuaTableRecord;
begin
  if GetRecordByKey(Key, Entry) then
  begin
    Entry.AsFunc:=Value;
    Update;
  end;
end;

procedure TLuaTable.SetAsInt(Key: Variant; const Value: Int64);
var
  Entry: TLuaTableRecord;
begin
  if GetRecordByKey(Key, Entry) then
  begin
    Entry.AsInt:=Value;
    Update;
  end;
end;

procedure TLuaTable.SetAsPtr(Key: Variant; const Value: Pointer);
var
  Entry: TLuaTableRecord;
begin
  if GetRecordByKey(Key, Entry) then
  begin
    Entry.AsPtr:=Value;
    Update;
  end;
end;

procedure TLuaTable.SetAsStr(Key: Variant; const Value: String);
var
  Entry: TLuaTableRecord;
begin
  if GetRecordByKey(Key, Entry) then
  begin
    Entry.AsStr:=Value;
    Update;
  end;
end;

procedure TLuaTable.SetAsTable(Key: Variant; const Value: TLuaTable);
var
  Entry: TLuaTableRecord;
begin
  if GetRecordByKey(Key, Entry) then
  begin
    Entry.AsTable:=Value;
    Update;
  end;
end;

procedure TLuaTable.SetName(const Value: String);
begin
  FName:=Value;

  // If we have a name, register the table in globals
  if FName <> '' then
  begin
    FLua.SetTables(FName, Self);
  end;
end;

procedure TLuaTable.Add(AValue: Variant);
begin
  // Increment the numeric index
  Inc(FLastNumIndex);

  // Push the table to the stack
  PushToStack;

  // Push the new key and value to the stack
  FLua.Stack.PushInteger(FLastNumIndex);
  FLua.Stack.PushVariant(AValue);
  FLua.Stack.SetTable(-3);

  // Remove the table from stack
  PopFromStack;

  // Update table
  Update;
end;

procedure TLuaTable.Add(AValue: TLuaValue);
begin
  if AValue.CheckState(FLua) then
  begin
    // Increment the numeric index
    Inc(FLastNumIndex);

    // Push the table to the stack
    PushToStack;

    // Push the new key and value to the stack
    FLua.Stack.PushInteger(FLastNumIndex);
    AValue.PushToStack;
    FLua.Stack.SetTable(-3);

    // Remove the table from stack
    PopFromStack;

    // Update table
    Update;
  end;
end;

procedure TLuaTable.Add(AKey, AValue: Variant);
begin
  // Push the table to the stack
  PushToStack;

  // Push the new key and value to the stack
  FLua.Stack.PushVariant(AKey);
  FLua.Stack.PushVariant(AValue);
  FLua.Stack.SetTable(-3);

  // Remove the table from stack
  PopFromStack;

  // Update table
  Update;
end;


procedure TLuaTable.Add(AKey: Variant; AValue: TLuaValue);
begin
  if AValue.CheckState(FLua) then
  begin
    // Push the table to the stack
    PushToStack;

    // Push the new key and value to the stack
    FLua.Stack.PushVariant(AKey);
    AValue.PushToStack;
    FLua.Stack.SetTable(-3);

    // Remove the table from stack
    PopFromStack;

    // Update table
    Update;
  end;
end;

procedure TLuaTable.BeginUpdate;
begin
  AtomicIncrement(FUpdating);
end;

procedure TLuaTable.EndUpdate;
begin
  if FUpdating > 0 then
  begin
    AtomicDecrement(FUpdating);
    if FUpdating = 0 then
    begin
      Iterate;
    end;
  end;
end;

procedure TLuaTable.Clear;
var
  I: Integer;
begin
  try
    for I:=0 to FRecords.Count - 1 do
    begin
      FRecords[I].V.Free;
    end;
  finally
    FRecords.Clear;
  end;
end;

destructor TLuaTable.Destroy;
begin
  // Remove this table from the TLua cleanup list
  FLua.NotifyCleanup(Self, opRemove);

  // Clear the table records
  Clear;

  // Free objects
  FreeAndNil(FRecords);

  inherited;
end;

function TLuaTable.ToString(const AVarName: String = ''): String;

  function EscapeToLuaStr(Subject: String): String;
  begin
    Result := Subject;
    Result := StringReplace(Result, '\', '\\', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, #13, '\r', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, #09, '\t', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '"', '\"', [rfReplaceAll, rfIgnoreCase]);
  end;

  procedure WriteIdentToStr(IdentDepth: Integer);
  begin
    Result := Result + StringOfChar(' ', IdentDepth);
  end;

  procedure WriteTableValueToStr(TableRecord: TLuaTableRecord);
  begin
    if TableRecord.IsNil then
    begin
      Result:=Result + 'nil';
    end else
    if TableRecord.IsBool then
    begin
      if Boolean(TableRecord.AsBool) then
      begin
        Result:=Result + 'true';
      end else
      begin
        Result:=Result + 'false';
      end;
    end else
    if TableRecord.IsInt then
    begin
      Result:=Result + IntToStr(TableRecord.AsInt);
    end else
    if TableRecord.IsFloat then
    begin
      Result:=Result + FloatToStr(TableRecord.AsFloat);
    end else
    if TableRecord.IsStr then
    begin
      Result:=Result + '"' + EscapeToLuaStr(TableRecord.AsStr) + '"';
    end else
    begin
      Result:=Result + '0x' + IntToHex(NativeInt(TableRecord.AsPtr), SizeOf(NativeInt) * 2);
    end;
  end;

  procedure WriteTableToStr(Table: TLuaTable; var IdentDepth: Integer);
  var
    I: Integer;
    TableRecord: TLuaTableRecord;
  begin
    Inc(IdentDepth, 2);

    for I:=0 to Table.GetCount - 1 do
    begin
      TableRecord:=Table.GetRecords(I);
      if Assigned(TableRecord) then
      begin
        WriteIdentToStr(IdentDepth);

        if TableRecord.IsTable then
        begin
          if TableRecord.IsIndexKey then
          begin
            Result:=Result + '[' + IntToStr(TableRecord.Key.AsInt) + ']' + ' = {' + #13#10;
          end else
          begin
            Result:=Result + '["' + EscapeToLuaStr(TableRecord.Key.AsStr) + '"]' + ' = {' + #13#10;
          end;

          WriteTableToStr(TableRecord.AsTable, IdentDepth);
          WriteIdentToStr(IdentDepth);

          Result:=Result + '},' + #13#10;
        end else
        begin
          if TableRecord.IsIndexKey then
          begin
            Result:=Result + '[' + IntToStr(TableRecord.Key.AsInt) + ']' + ' = ';
          end else
          begin
            Result:=Result + '["' + EscapeToLuaStr(TableRecord.Key.AsStr) + '"]' + ' = ';
          end;

          WriteTableValueToStr(TableRecord);

          Result:=Result + ',' + #13#10;
        end;
      end;
    end;

    Dec(IdentDepth, 2);
  end;

var
  Ident: Integer;
begin
  Ident:=0;

  if Trim(AVarName) <> '' then
  begin
    Result:=Trim(AVarName) + ' = {' + #13#10;
  end else
  begin
    Result:='{' + #13#10;
  end;

  WriteTableToStr(Self, Ident);

  Result:=Result + '};' + #13#10;
end;

procedure TLuaTable.Update;
begin
  // Reiterate the table
  if FUpdating = 0 then
  begin
    Iterate;
  end;
end;

function TLuaTable.GetAsBlueprint(Key: Variant): TLuaClassBlueprint;
var
  Entry: TLuaTableRecord;
begin
  Result:=nil;

  if GetRecordByKey(Key, Entry) then
  begin
    Result:=Entry.AsBlueprint;
  end;
end;

function TLuaTable.GetAsBool(Key: Variant): Boolean;
var
  Entry: TLuaTableRecord;
begin
  Result:=False;

  if GetRecordByKey(Key, Entry) then
  begin
    Result:=Entry.AsBool;
  end;
end;

function TLuaTable.GetAsClass(Key: Variant): TLuaClass;
var
  Entry: TLuaTableRecord;
begin
  Result:=nil;

  if GetRecordByKey(Key, Entry) then
  begin
    Result:=Entry.AsClass;
  end;
end;

function TLuaTable.GetAsFloat(Key: Variant): Double;
var
  Entry: TLuaTableRecord;
begin
  Result:=0.0;

  if GetRecordByKey(Key, Entry) then
  begin
    Result:=Entry.AsFloat;
  end;
end;

function TLuaTable.GetAsFunc(Key: Variant): TLuaFunction;
var
  Entry: TLuaTableRecord;
begin
  Result:=nil;

  if GetRecordByKey(Key, Entry) then
  begin
    Result:=Entry.AsFunc;
  end;
end;

function TLuaTable.GetAsInt(Key: Variant): Int64;
var
  Entry: TLuaTableRecord;
begin
  Result:=0;

  if GetRecordByKey(Key, Entry) then
  begin
    Result:=Entry.AsInt;
  end;
end;

function TLuaTable.GetAsPtr(Key: Variant): Pointer;
var
  Entry: TLuaTableRecord;
begin
  Result:=nil;

  if GetRecordByKey(Key, Entry) then
  begin
    Result:=Entry.AsPtr;
  end;
end;

function TLuaTable.GetAsStr(Key: Variant): String;
var
  Entry: TLuaTableRecord;
begin
  Result:='';

  if GetRecordByKey(Key, Entry) then
  begin
    Result:=Entry.AsStr;
  end;
end;

function TLuaTable.GetAsTable(Key: Variant): TLuaTable;
var
  Entry: TLuaTableRecord;
begin
  Result:=nil;

  if GetRecordByKey(Key, Entry) then
  begin
    Result:=Entry.AsTable;
  end;
end;

function TLuaTable.GetCount: Integer;
begin
  Result:=FRecords.Count;
end;

function TLuaTable.GetRecordByIndex(AIndex: Integer): TLuaTableRecord;
begin
  Result:=nil;

  if (AIndex >= 0) AND (AIndex < FRecords.Count) then
  begin
    Result:=FRecords[AIndex].V;
  end;
end;

function TLuaTable.GetRecordByKey(AKey: Variant): TLuaTableRecord;
var
  I: Integer;
begin
  Result:=nil;

  for I:=0 to FRecords.Count - 1 do
  begin
    if VarSameValue(FRecords[I].K, AKey) then
    begin
      Result:=FRecords[I].V;
      Break;
    end;
  end;
end;

function TLuaTable.GetRecordByIndex(AIndex: Integer; var ARecord: TLuaTableRecord): Boolean;
begin
  ARecord:=GetRecordByIndex(AIndex);
  Result:=Assigned(ARecord);
end;

function TLuaTable.GetRecordByKey(AKey: Variant; var ARecord: TLuaTableRecord): Boolean;
begin
  ARecord:=GetRecordByKey(AKey);
  Result:=Assigned(ARecord);
end;

function TLuaTable.GetRecords(Index: Integer): TLuaTableRecord;
begin
  Result:=nil;

  if (Index >= 0) AND (Index < FRecords.Count) then
  begin
    Result:=TLuaTableRecord(FRecords[Index].V);
  end;
end;

procedure TLuaTable.Initialize;
begin
  // Create stuff'n'shit
  FUpdating:=0;
  FRecords:=TList<KeyValue>.Create;

  // Try iterate the refed table
  Iterate;
end;

procedure TLuaTable.Iterate;
var
  TableRecord: TLuaTableRecord;
begin
  // Clear the table before iteration
  Clear;

  // Push it to the stack
  PushToStack;
  try
    if FLua.Stack.IsTable(-1) then
    begin
      FLua.Stack.PushNil;
      while FLua.Stack.Next(-2) do
      begin
        TableRecord:=TLuaTableRecord.Create(Self, -2, -1);

        // Check if the key is numeric
        if TableRecord.Key.IsInt then
        begin
          FLastNumIndex:=Max(FLastNumIndex, TableRecord.Key.AsInt);
        end;

        // Add record to list
        FRecords.Add(KeyValue.New(TableRecord.Key.AsStr, TableRecord));

        // Pop the value
        PopFromStack;
      end;
    end;
  finally
    // Pop table from stack
    PopFromStack;
  end;
end;

{ TLuaFunction }

class function TLuaFunction.New(ALua: TLua; AIndex: Integer): TLuaFunction;
begin
  Result:=TLuaFunction.Create(ALua, oaStackIndex, AIndex);
end;

destructor TLuaFunction.Destroy;
begin
  FreeAndNil(FArgs);
  FreeAndNil(FResults);

  inherited;
end;

function TLuaFunction.Execute: Boolean;
var
  Res, I: Integer;
begin
  Res:=0;
  Result:=False;

  // Top Index before call
  I:=FLua.Stack.Top;

  // Push function
  PushToStack;

  // Push args
  FArgs.ApplyToStack;

  try
    // Execute the function
    Res:=lua_pcall(FLua.State, FArgs.Count, LUA_MULTRET, 0);
    case Res of
      LUA_ERRRUN: raise ELuaExecuteException.Create('Runtime error');
      LUA_ERRMEM: raise ELuaExecuteException.Create('Memory allocation error');
      LUA_ERRSYNTAX: raise ELuaExecuteException.Create('Syntax error');
      LUA_ERRERR: raise ELuaExecuteException.Create('Error handling function failed');
    end;

    // Set result
    Result:=Res = LUA_OK;
  except
    on E: ELuaException do
    begin
      // Set exception props
      E.FName:='<anonymous function>';
      E.FCode:=Res;
      E.FLuaMessage:=FLua.Stack.ToString(-1);

      // Handle the error
      FLua.HandleLuaError(E);

      // Pop the message from stack
      FLua.Stack.Pop;
    end;
  end;

  // Clear results
  FResults.Update(0);

  // Set the new count
  FResults.FCount:=Abs(FLua.Stack.Top - I);

  // Manual fetch results
  for I:=FResults.FCount - 1 downto 0 do
  begin
    FResults.FValues.Add(TLuaValue.New(FLua, -(I + 1)));
  end;

  // Remove the results from the stack
  FLua.Stack.Pop(FResults.FCount);
end;

procedure TLuaFunction.Initialize;
begin
  FResults:=TLuaFunctionResults.Create(FLua, 0);
  FArgs:=TLuaFunctionArgs.Create(FLua);
end;

{ TLuaClassMethod }

constructor TLuaClassMethod.Create(AClass: TLuaClass; AName: String; ACallback: TLuaClassMethodEvent);
begin
  inherited Create;

  FClass:=AClass;
  FName:=AName;
  FOnCallback:=ACallback;
end;

destructor TLuaClassMethod.Destroy;
begin
  inherited;
end;

{ TLuaClassProperty }

constructor TLuaClassProperty.Create(AClass: TLuaClass; AName: String; AGetCallback, ASetCallback: TLuaClassPropertyEvent);
begin
  inherited Create;

  FClass:=AClass;
  FName:=AName;
  FOnGetCallback:=AGetCallback;
  FOnSetCallback:=ASetCallback;
end;

destructor TLuaClassProperty.Destroy;
begin
  inherited;
end;

{ TLuaClassIndexProperty }

constructor TLuaClassIndexProperty.Create(AClass: TLuaClass; AName: String; AGetCallback, ASetCallback: TLuaClassIndexPropertyEvent);
begin
  inherited Create;

  FClass:=AClass;
  FName:=AName;
  FOnGetCallback:=AGetCallback;
  FOnSetCallback:=ASetCallback;
end;

destructor TLuaClassIndexProperty.Destroy;
begin
  inherited;
end;

{ TLuaClassBlueprint }

constructor TLuaClassBlueprint.Create(ALua: TLua; AName: String);
begin
  inherited Create(ALua, oaNone);

  // Create the method table we are using to bind it to the reg index
  FLua.Stack.NewTable;
  FromStack(-1);
  PopFromStack;

  // Default init stuff
  FIsInherited:=False;
  FLua:=ALua;
  FName:=AName;
  FParent:=nil;
  FAllowConstruct:=True;
end;

constructor TLuaClassBlueprint.Create(ABlueprint: TLuaClassBlueprint; AName: String);
begin
  inherited Create(ABlueprint.Lua, oaNone);

  // Create the method table we are using to bind it to the reg index
  FLua.Stack.NewTable;
  FromStack(-1);
  PopFromStack;

  // Default init stuff for inheritance
  FIsInherited:=True;
  FLua:=ABlueprint.FLua;
  FName:=AName;
  FParent:=ABlueprint;
  FAllowConstruct:=True;
end;

destructor TLuaClassBlueprint.Destroy;
begin
  Finalize;

  FreeAndNil(FInheritances);
  FreeAndNil(FInstances);
  FreeAndNil(FIndexProperties);
  FreeAndNil(FProperties);
  FreeAndNil(FMethods);

  TLuaCallbackWrapper.PrepareRelease(FLuaIndexHandler);
  TLuaCallbackWrapper.PrepareRelease(FLuaCallHandler);
  TLuaCallbackWrapper.PrepareRelease(FLuaNewHandler);
  TLuaCallbackWrapper.PrepareRelease(FLuaGCHandler);

  inherited;
end;

procedure TLuaClassBlueprint.Initialize;
begin
  // Create all the good stuff
  FMethods:=TList<MethodEntry>.Create;
  FProperties:=TList<PropertyEntry>.Create;
  FIndexProperties:=TList<IndexPropertyEntry>.Create;
  FInstances:=TObjectList<TLuaClass>.Create;
  FInheritances:=TObjectList<TLuaClassBlueprint>.Create;

  // Properties
  FLuaIndexHandler:=TLuaCallbackWrapper.New(LuaIndexHandler, LuaNewIndexHandler).Callback;
  // Constructor
  FLuaCallHandler:=TLuaCallbackWrapper.New(LuaCallHandler).Callback;
  FLuaNewHandler:=TLuaCallbackWrapper.New(LuaNewHandler).Callback;
  // 'Destructor'
  FLuaGCHandler:=TLuaCallbackWrapper.New(LuaGCHandler).Callback;

  // Copy methods from parent class, if we have a parent
  if Assigned(FParent) then
  begin
    CopyMethods(FParent.FMethods);
    CopyProperties(FParent.FProperties);
    CopyIndexProperties(FParent.FIndexProperties);
  end;
end;

procedure TLuaClassBlueprint.Finalize;
var
  I: Integer;
begin
  for I:=0 to FInheritances.Count - 1 do
    FInheritances[I].Free;

  FInheritances.OwnsObjects:=False;
  FInheritances.Clear;

  for I:=0 to FInstances.Count - 1 do
    FInstances[I].Free;

  FInstances.OwnsObjects:=False;
  FInstances.Clear;
end;

class function TLuaClassBlueprint.FromGlobal(ALua: TLua; AName: String): TLuaObject;
begin
  ALua.Stack.GetGlobal(AName);
  Result:=Acquire(ALua.State);
end;

class function TLuaClassBlueprint.FromTable(ATable: TLuaTable): TLuaClassBlueprint;
begin
  ATable.PushToStack;
  try
    Result:=Acquire(ATable.Lua.State, True);
  finally
    ATable.Lua.Stack.Pop;
  end;
end;

class function TLuaClassBlueprint.Acquire(AState: TLuaState; AKeepOnStack: Boolean = False): TLuaClassBlueprint;
var
  Name: AnsiString;
  Blueprint: TLuaClassBlueprint;
begin
  Result:=nil;
  Blueprint:=nil;

  (**********************************************************************)
  (* Low level Lua API call because we dont have a TLua instance here *)
  (**********************************************************************)

  // Is the current call a blueprint table stack?
  if (lua_gettop(AState) > 0) AND (lua_type(AState, 1) = LUA_TTABLE) then
  begin
    // Get the blueprint name
    lua_pushstring(AState, '_classname');
    lua_rawget(AState, 1);
    if lua_isstring(AState, -1) then
    begin
      Name:=System.AnsiStrings.StrPas(lua_tostring(AState, -1));
    end;
    lua_pop(AState, 1);

    // Get the blueprint object
    lua_pushstring(AState, '_blueprint');
    lua_rawget(AState, 1);
    if lua_islightuserdata(AState, -1) then
    begin
      Blueprint:=lua_topointer(AState, -1);
    end;
    lua_pop(AState, 1);

    // Does it match? Return it!
    if Assigned(Blueprint) AND AnsiSameStr(String(Name), Blueprint.Name) then
    begin
      if NOT AKeepOnStack then
      begin
        lua_remove(AState, 1);
      end;

      Result:=Blueprint;
    end;
  end;
end;


function TLuaClassBlueprint.GetIndexProperties(Index: Integer): String;
begin
  Result:='';

  if (Index >= 0) AND (Index < FIndexProperties.Count) then
  begin
    Result:=FIndexProperties[Index].N;
  end;
end;

function TLuaClassBlueprint.GetMethod(AName: String; var AMethod: MethodEntry): Boolean;
var
  I: Integer;
begin
  Result:=False;

  for I:=0 to FMethods.Count - 1 do
  begin
    if AnsiSameText(AName, FMethods[I].N) then
    begin
      Result:=True;
      AMethod:=FMethods[I];
      Break;
    end;
  end;
end;

function TLuaClassBlueprint.GetMethodCount: Integer;
begin
  Result:=FMethods.Count;
end;

function TLuaClassBlueprint.GetMethods(Index: Integer): String;
begin
  Result:='';

  if (Index >= 0) AND (Index < FMethods.Count) then
  begin
    Result:=FMethods[Index].N;
  end;
end;

function TLuaClassBlueprint.GetProperties(Index: Integer): String;
begin
  Result:='';

  if (Index >= 0) AND (Index < FProperties.Count) then
  begin
    Result:=FProperties[Index].N;
  end;
end;

function TLuaClassBlueprint.GetIndexPropertyCount: Integer;
begin
  Result:=FIndexProperties.Count;
end;

function TLuaClassBlueprint.GetPropertyCount: Integer;
begin
  Result:=FProperties.Count;
end;

function TLuaClassBlueprint.HasMethod(AName: String): Boolean;
var
  I: Integer;
begin
  Result:=False;

  for I:=0 to FMethods.Count - 1 do
  begin
    if AnsiSameText(FMethods[I].N, AName) then
    begin
      Result:=True;

      Break;
    end;
  end;
end;

function TLuaClassBlueprint.HasProperty(AName: String): Boolean;
var
  I: Integer;
begin
  Result:=False;

  try
    for I:=0 to FProperties.Count - 1 do
    begin
      if AnsiSameText(FProperties[I].N, AName) then
      begin
        Result:=True;

        Abort;
      end;
    end;

    for I:=0 to FIndexProperties.Count - 1 do
    begin
      if AnsiSameText(FIndexProperties[I].N, AName) then
      begin
        Result:=True;

        Abort;
      end;
    end;
  except
  end;
end;

function TLuaClassBlueprint.Inherit(AName: String = ''): TLuaClassBlueprint;

  function CreateUID: String;
  var
    GUID: TGUID;
    StrUID: String;
  begin
    StrUID:=StringOfChar('0', SizeOf(TGUID) * 2);

    if CoCreateGuid(GUID) = S_OK then
      BinToHex(@GUID, PChar(StrUID), SizeOf(GUID));

    Result:=AnsiUpperCase(StrUID);
  end;


begin
  // If no name is supplied, generate one
  if Trim(AName) = '' then
  begin
    AName:=FName + '_' + CreateUID;
  end;

  // Create new instance of the inherited blueprint
  Result:=TLuaClassBlueprint.Create(Self, AName);

  // Apply existing events
  Result.OnDefaultPropertyGet:=FOnDefaultPropertyGet;
  Result.OnDefaultPropertySet:=FOnDefaultPropertySet;
  Result.OnConstructon:=FOnConstructon;
  Result.OnRelease:=FOnRelease;
  Result.OnGarbageCollection:=FOnGarbageCollection;

  // Add to inheritances list
  FInheritances.Add(Result);
end;

class function TLuaClassBlueprint.IsRegistered(ALua: TLua; AName: String): Boolean;
begin
  Result:=False;

  if Assigned(ALua) then
  begin
    // Retrive value from global and check if it is a table
    Result:=ALua.Stack.IsTable(ALua.Stack.GetGlobal(AName));

    // Pop the retrived value
    ALua.Stack.Pop;
  end;
end;

procedure TLuaClassBlueprint.CleanupInstances;
var
  I: Integer;
begin
  for I:=0 to FInheritances.Count - 1 do
    FInheritances[I].Finalize;

  for I:=0 to FInstances.Count - 1 do
    FInstances[I].ReleaseCleanup;
end;

function TLuaClassBlueprint.Construct(AData: Pointer = nil): TLuaClass;
begin
  Result:=nil;

  if IsRegistered(FLua, FName) then
  begin
    // Create the class
    Result:=TLuaClass.New(FLua, Self, AData);

    // Add the new class to the instance list
    FInstances.Add(Result);
  end;
end;

procedure TLuaClassBlueprint.CopyIndexProperties(ASource: TList<IndexPropertyEntry>);
var
  Source: IndexPropertyEntry;
begin
  for Source in ASource do
  begin
    FIndexProperties.Add(Source);
  end;
end;

procedure TLuaClassBlueprint.CopyMethods(ASource: TList<MethodEntry>);
var
  Source: MethodEntry;
begin
  for Source in ASource do
  begin
    FMethods.Add(Source);
  end;
end;

procedure TLuaClassBlueprint.CopyProperties(ASource: TList<PropertyEntry>);
var
  Source: PropertyEntry;
begin
  for Source in ASource do
  begin
    FProperties.Add(Source);
  end;
end;

procedure TLuaClassBlueprint.LuaCallHandler(Sender: TLua; Blueprint: TLuaClassBlueprint; Args: TLuaArgs; var UserClass: TObject; var Allow: Boolean);
begin
  Allow:=False;
end;

procedure TLuaClassBlueprint.LuaNewHandler(Sender: TLua; Blueprint: TLuaClassBlueprint; Args: TLuaArgs; var UserClass: TObject; var Allow: Boolean);
begin
  if Assigned(FOnConstructon) then
  begin
    FOnConstructon(Sender, Blueprint, Args, UserClass, Allow);
  end;
end;

procedure TLuaClassBlueprint.LuaGCHandler(Sender: TLua; Clazz: TLuaClass);
begin
  if Assigned(FOnGarbageCollection) then
  begin
    FOnGarbageCollection(Sender, Clazz);
  end;
end;

procedure TLuaClassBlueprint.LuaIndexHandler(Sender: TLua; Clazz: TLuaClass; Access, Value: TLuaValue);
begin
  if Assigned(FOnDefaultPropertyGet) then
  begin
    FOnDefaultPropertyGet(Sender, Clazz, Access, Value);
  end;
end;

procedure TLuaClassBlueprint.LuaNewIndexHandler(Sender: TLua; Clazz: TLuaClass; Access, Value: TLuaValue);
begin
  if Assigned(FOnDefaultPropertySet) then
  begin
    FOnDefaultPropertySet(Sender, Clazz, Access, Value);
  end;
end;

function TLuaClassBlueprint.NewMethodInvoker(AMethodName: String; AClass: TLuaClass): TLuaClassMethodInvoker;
var
  Method: MethodEntry;
begin
  Result:=nil;

  // Get the method
  if GetMethod(AMethodName, Method) then
  begin
    case Method.T of // Check method type, native or plain lua?
      mtNative:
      begin
        Result:=TLuaClassMethodInvoker.NewNative(AMethodName, AClass, Method.D);
      end;
      mtLua:
      begin
        Result:=TLuaClassMethodInvoker.NewLua(AMethodName, AClass, Method.L);
      end;
    end;
  end;
end;

procedure TLuaClassBlueprint.Register;
var
  Methods, MetaTable, MethodTable: Integer;
begin
  if NOT IsRegistered(FLua, FName) then
  begin
    Methods:=PushToStack;
    MetaTable:=FLua.Stack.NewMetaTable(FName);
    MethodTable:=FLua.Stack.NewTable;

    FLua.Stack.PushString('__metatable');
    FLua.Stack.PushValue(Methods);
    FLua.Stack.SetTable(MetaTable);

    FLua.Stack.PushString('__index');
    FLua.Stack.PushFunction(FLuaIndexHandler);
    FLua.Stack.SetTable(MetaTable);

    FLua.Stack.PushString('__newindex');
    FLua.Stack.PushFunction(FLuaIndexHandler);
    FLua.Stack.SetTable(MetaTable);

    FLua.Stack.PushString('__gc');
    FLua.Stack.PushFunction(FLuaGCHandler);
    FLua.Stack.SetTable(MetaTable);

    if FIsInherited then
    begin
      FLua.Stack.PushString('__newindex');
      FLua.Stack.PushFunction(FLuaIndexHandler);
      FLua.Stack.SetTable(MethodTable);
    end;

    if FAllowConstruct then
    begin
      FLua.Stack.PushString('__call');
      FLua.Stack.PushFunction(FLuaCallHandler);
      FLua.Stack.SetTable(MethodTable);

      FLua.Stack.PushString('new');
      FLua.Stack.PushFunction(FLuaNewHandler);
      FLua.Stack.SetTable(Methods);
    end else
    begin
      FLua.Stack.PushString('__call');
      FLua.Stack.PushNil;
      FLua.Stack.SetTable(MethodTable);

      FLua.Stack.PushString('new');
      FLua.Stack.PushNil;
      FLua.Stack.SetTable(Methods);
    end;

    FLua.Stack.SetMetaTable(Methods);

    // Set the blueprint class
    FLua.Stack.PushString('_blueprint');
    FLua.Stack.PushPointer(Self);
    FLua.Stack.RawSet(Methods);

    // Set the class name
    FLua.Stack.PushString('_classname');
    FLua.Stack.PushString(FName);
    FLua.Stack.RawSet(Methods);

    // Make this class accessable to global name
    FLua.Stack.PushValue(Methods);
    FLua.Stack.SetGlobal(FName);

    FLua.Stack.Pop(2);
  end;
end;

procedure TLuaClassBlueprint.AddMethod(AName: String; ACallback: TLuaClassMethodEvent);
var
  Item: MethodEntry;
begin
  Item.N:=AName;
  Item.T:=mtNative;
  Item.D:=ACallback;

  AddOrReplaceMethod(Item);
end;

procedure TLuaClassBlueprint.AddMethod(AName: String; AFunc: TLuaFunction);
var
  Item: MethodEntry;
begin
  Item.N:=AName;
  Item.T:=mtLua;
  Item.L:=AFunc.CopyToRef;

  AddOrReplaceMethod(Item);
end;

procedure TLuaClassBlueprint.AddOrReplaceIndexProperty(AIndexPropertyEntry: IndexPropertyEntry);
var
  I: Integer;
begin
  try
    for I:=0 to FIndexProperties.Count - 1 do
    begin
      if AnsiSameText(FIndexProperties[I].N, AIndexPropertyEntry.N) then
      begin
        FIndexProperties[I]:=AIndexPropertyEntry;

        Abort;
      end;
    end;

    FIndexProperties.Add(AIndexPropertyEntry);
  except
  end;
end;

procedure TLuaClassBlueprint.AddOrReplaceMethod(AMethodEntry: MethodEntry);
var
  I: Integer;
begin
  try
    for I:=0 to FMethods.Count - 1 do
    begin
      if AnsiSameText(FMethods[I].N, AMethodEntry.N) then
      begin
        FMethods[I]:=AMethodEntry;

        Abort;
      end;
    end;

    FMethods.Add(AMethodEntry);
  except
  end;
end;

procedure TLuaClassBlueprint.AddOrReplaceProperty(APropertyEntry: PropertyEntry);
var
  I: Integer;
begin
  try
    for I:=0 to FProperties.Count - 1 do
    begin
      if AnsiSameText(FProperties[I].N, APropertyEntry.N) then
      begin
        FProperties[I]:=APropertyEntry;

        Abort;
      end;
    end;

    FProperties.Add(APropertyEntry);
  except
  end;
end;

procedure TLuaClassBlueprint.AddProperty(AName: String; AGet, ASet: TLuaClassPropertyEvent);
var
  Item: PropertyEntry;
begin
  Item.N:=AName;
  Item.G:=AGet;
  Item.S:=ASet;

  AddOrReplaceProperty(Item);
end;

procedure TLuaClassBlueprint.AddIndexProperty(AName: String; AGet, ASet: TLuaClassIndexPropertyEvent);
var
  Item: IndexPropertyEntry;
begin
  Item.N:=AName;
  Item.G:=AGet;
  Item.S:=ASet;

  AddOrReplaceIndexProperty(Item);
end;

procedure TLuaClassBlueprint.DeleteIndexProperty(AIndex: Integer);
begin
  if (AIndex >= 0) AND (AIndex < FIndexProperties.Count) then
  begin
    FIndexProperties.Delete(AIndex);
  end;
end;

procedure TLuaClassBlueprint.DeleteMethod(AIndex: Integer);
begin
  if (AIndex >= 0) AND (AIndex < FMethods.Count) then
  begin
    FMethods.Delete(AIndex);
  end;
end;

procedure TLuaClassBlueprint.DeleteProperty(AIndex: Integer);
begin
  if (AIndex >= 0) AND (AIndex < FProperties.Count) then
  begin
    FProperties.Delete(AIndex);
  end;
end;

{ TLuaClass }

destructor TLuaClass.Destroy;
var
  I: Integer;
begin
  // Remove this instance from the blueprint list
  FBlueprint.FInstances.Extract(Self);

  // Cleanup the remaining stuff...
  ReleaseCleanup;

  // Free the callback wrappers...
  TLuaCallbackWrapper.PrepareRelease(FLuaReleaseHandler);
  TLuaCallbackWrapper.PrepareRelease(FLuaInheritedHandler);

  try
    for I:=0 to FIndexProperties.Count - 1 do
    begin
      FIndexProperties.Objects[I].Free;
    end;
  finally
    FreeAndNil(FIndexProperties);
  end;

  try
    for I:=0 to FProperties.Count - 1 do
    begin
      FProperties.Objects[I].Free;
    end;
  finally
    FreeAndNil(FProperties);
  end;

  try
    for I:=0 to FMethods.Count - 1 do
    begin
      FMethods.Objects[I].Free;
    end;
  finally
    FreeAndNil(FMethods);
  end;

  FreeAndNil(FCleanupList);

  inherited;
end;

class function TLuaClass.FromGlobal(ALua: TLua; AName: String): TLuaObject;
begin
  ALua.Stack.GetGlobal(AName);
  Result:=Acquire(ALua.State);
end;

class function TLuaClass.Acquire(AState: TLuaState; AKeepOnStack: Boolean = False): TLuaClass;
begin
  Result:=nil;

  if (lua_gettop(AState) > 0) AND (lua_type(AState, 1) = LUA_TTABLE) then
  begin
    // Get class object
    lua_pushstring(AState, '_self');
    lua_rawget(AState, 1);
    if lua_islightuserdata(AState, -1) then
    begin
      Result:=lua_topointer(AState, -1);
    end;
    lua_pop(AState, 1);

    // Get and check RefId
    lua_pushstring(AState, '_refid');
    lua_rawget(AState, 1);
    if lua_isinteger(AState, -1) then
    begin
      if Result.RefId <> lua_tointeger(AState, -1) then
      begin
        Result:=nil;
      end;
    end;
    lua_pop(AState, 1);

    // Is this a property index?
    lua_pushstring(AState, '_idxpropname');
    lua_rawget(AState, 1);
    if lua_isstring(AState, -1) then
    begin
//      FIsIndexProp := True;
//      FName := String(LuaToString(AState, -1));
    end;
    lua_pop(AState, 1);

    // Remove the table object from the top of the stack
    if Assigned(Result) AND NOT AKeepOnStack then
    begin
      lua_remove(AState, 1);
    end;
  end;
end;

function TLuaClass.GetIndexProperties(Name: String): TLuaClassIndexProperty;
var
  Idx: Integer;
begin
  Result:=nil;

  Idx:=FIndexProperties.IndexOf(Name);
  if Idx >= 0 then
  begin
    Result:=TLuaClassIndexProperty(FIndexProperties.Objects[Idx]);
  end;
end;

function TLuaClass.GetMethods(Name: String): TLuaClassMethod;
var
  Idx: Integer;
begin
  Result:=nil;

  Idx:=FMethods.IndexOf(Name);
  if Idx >= 0 then
  begin
    Result:=TLuaClassMethod(FMethods.Objects[Idx]);
  end;
end;

function TLuaClass.GetProperties(Name: String): TLuaClassProperty;
var
  Idx: Integer;
begin
  Result:=nil;

  Idx:=FProperties.IndexOf(Name);
  if Idx >= 0 then
  begin
    Result:=TLuaClassProperty(FProperties.Objects[Idx]);
  end;
end;

function TLuaClass.Inherit(AMethod: String): TLuaClassMethodInvoker;
begin
  Result:=nil;

  if Assigned(FBlueprint.Parent) AND FBlueprint.Parent.HasMethod(AMethod) then
  begin
    Result:=FBlueprint.Parent.NewMethodInvoker(AMethod, Self);
  end;
end;

function TLuaClass.Invoke(AMethod: String): TLuaClassMethodInvoker;
begin
  Result:=nil;

  if FBlueprint.HasMethod(AMethod) then
  begin
    Result:=FBlueprint.NewMethodInvoker(AMethod, Self);
  end;
end;

procedure TLuaClass.LuaInheritedHandler(Sender: TLua; Clazz: TLuaClass; Args: TLuaArgs; Results: TLuaResults);
var
  LuaInfo: lua_Debug;
begin
  if (lua_getstack(Lua.State, 1, @LuaInfo) <> 0) AND (lua_getinfo(Lua.State, 'nSl', @LuaInfo) <> 0) then
  begin
    if Trim(String(AnsiString(LuaInfo.name))) <> '' then
    begin
      with Clazz.Inherit(String(AnsiString(LuaInfo.name))) do
      begin
        try
          Execute;
        finally
          Free;
        end;
      end;
    end;
  end;
end;

procedure TLuaClass.LuaReleaseHandler(Sender: TLua; Clazz: TLuaClass);
begin
  // Redirect release call to the class blueprint
  if Assigned(Clazz) AND Assigned(Clazz.Blueprint.OnRelease) then
  begin
    Clazz.Blueprint.OnRelease(Sender, Clazz, Clazz.FUserClass);
  end;
end;

class function TLuaClass.New(ALua: TLua; ABlueprint: TLuaClassBlueprint; AData: Pointer = nil): TLuaClass;
var
  Wrap: TLuaCallbackWrapper;
  Method: TLuaClassMethod;
  I, Table, MetaTable, MethodTable, IndexTable: Integer;
begin
  // Create the new table
  ALua.Stack.NewTable;

  // Construct the class using the new table
  Result:=TLuaClass.Create(ALua, oaDefault);

  with Result do
  begin
    // Set internal fields
    FBlueprint:=ABlueprint;
    FData:=AData;
    FMethods:=THashedStringList.Create;
    FProperties:=THashedStringList.Create;
    FIndexProperties:=THashedStringList.Create;
    FCleanupList:=TObjectList.Create;
    FLuaReleaseHandler:=TLuaCallbackWrapper.New(LuaReleaseHandler).Callback;
    FLuaInheritedHandler:=TLuaCallbackWrapper.New(LuaInheritedHandler).Callback;

    // Create props
    for I:=0 to FBlueprint.FProperties.Count - 1 do
    begin
      FProperties.AddObject(FBlueprint.FProperties[I].N, TLuaClassProperty.Create(Result, FBlueprint.FProperties[I].N, FBlueprint.FProperties[I].G, FBlueprint.FProperties[I].S));
    end;

    // Create index props
    for I:=0 to FBlueprint.FIndexProperties.Count - 1 do
    begin
      FIndexProperties.AddObject(FBlueprint.FIndexProperties[I].N, TLuaClassIndexProperty.Create(Result, FBlueprint.FIndexProperties[I].N, FBlueprint.FIndexProperties[I].G, FBlueprint.FIndexProperties[I].S));
    end;

    // Push to stack
    Table:=Result.PushToStack;

    // Push self into the table
    FLua.Stack.PushString('_self');
    FLua.Stack.PushPointer(Result);
    FLua.Stack.RawSet(Table);

    // Push the RefId into the table
    FLua.Stack.PushString('_refid');
    FLua.Stack.PushInteger(Result.RefId);
    FLua.Stack.RawSet(Table);

    // Kind of destructor to release the assoc class
    FLua.Stack.PushString('release');
    FLua.Stack.PushFunction(FLuaReleaseHandler);
    FLua.Stack.SetTable(Table);

    // Class inherited method a direct alternative to the gloabal inherited function
    FLua.Stack.PushString('inherited');
    FLua.Stack.PushFunction(FLuaInheritedHandler);
    FLua.Stack.SetTable(Table);

    // Create methods
    for I:=0 to FBlueprint.FMethods.Count - 1 do
    begin
      Method:=nil;

      case FBlueprint.FMethods[I].T of
        mtNative:
        begin
          Method:=TLuaClassMethod.Create(Result, FBlueprint.FMethods[I].N, FBlueprint.FMethods[I].D);

          // Create the method callback wrapper
          Wrap:=TLuaCallbackWrapper.New(Method);

          // Register the method
          FLua.Stack.PushString(Method.Name);
          FLua.Stack.PushFunction(Wrap.Callback);
          FLua.Stack.SetTable(Table);

          FCleanupList.Add(Wrap);
        end;
        mtLua:
        begin
          // Register the method
          FLua.Stack.PushString(FBlueprint.FMethods[I].N);
          FLua.PushRef(FBlueprint.FMethods[I].L);
          FLua.Stack.SetTable(Table);
        end;
      end;

      if Assigned(Method) then
      begin
        FMethods.AddObject(FBlueprint.FMethods[I].N, Method);
      end;
    end;

    MetaTable:=FLua.Stack.NewTable;
    MethodTable:=FLua.Stack.GetMetaTable(ABlueprint.Name);

    // Copy the method table to the new table
    FLua.Stack.CopyTable(MethodTable, Table, MetaTable);

    // Remove the method table from the stack, we dont need it anymore!
    FLua.Stack.Remove(MethodTable);

    // Set the meta table to the table
    FLua.Stack.SetMetaTable(Table);

    for I:=0 to FIndexProperties.Count - 1 do
    begin
      // Create the property callback wrapper
      Wrap:=TLuaCallbackWrapper.New(TLuaClassIndexProperty(FIndexProperties.Objects[I]));

      IndexTable:=FLua.Stack.NewTable;

      FLua.Stack.PushString('_self');
      FLua.Stack.PushPointer(Result);
      lua_rawset(FLua.State, IndexTable);

      FLua.Stack.PushString('_refid');
      FLua.Stack.PushInteger(Result.RefId);
      lua_rawset(FLua.State, IndexTable);

      FLua.Stack.PushString('_idxpropname');
      FLua.Stack.PushString(FBlueprint.FIndexProperties[I].N);
      lua_rawset(FLua.State, IndexTable);

      MetaTable:=FLua.Stack.NewTable;

      // Add the getter
      FLua.Stack.PushString('__index');
      FLua.Stack.PushFunction(Wrap.Callback);
      lua_settable(FLua.State, MetaTable);

      // Add the setter
      FLua.Stack.PushString('__newindex');
      FLua.Stack.PushFunction(Wrap.Callback);
      lua_settable(FLua.State, MetaTable);

      lua_setmetatable(FLua.State, IndexTable);

      FLua.Stack.PushString(FBlueprint.FIndexProperties[I].N);
      lua_pushvalue(FLua.State, IndexTable);
      lua_rawset(FLua.State, Table);

      FCleanupList.Add(Wrap);

      FLua.Stack.Pop;
    end;

    FLua.Stack.Pop;
  end;
end;

procedure TLuaClass.ReleaseCleanup;
var
  Table, I: Integer;

  procedure ClearField(Name: String);
  begin
    FLua.Stack.PushString(Name);
    FLua.Stack.PushNil;
    FLua.Stack.RawSet(Table);
  end;

begin
  if Assigned(FBlueprint.OnRelease) then
  begin
    FBlueprint.OnRelease(FLua, Self, FUserClass);
  end;

  Table:=PushToStack;
  try
    if (lssDestroy <> FLua.FStatus) AND FLua.Stack.IsTable(Table) then
    begin
      ClearField('_self');
      ClearField('_refid');

      ClearField('__metatable');
      ClearField('__index');
      ClearField('__newindex');
      ClearField('__gc');
      ClearField('__call');

      for I:=0 to FMethods.Count - 1 do
      begin
        ClearField(FMethods[I]);
      end;

      ClearField('new');
    end;
  finally
    PopFromStack;
  end;
end;

{ TLuaClassMethodInvoker }

class function TLuaClassMethodInvoker.NewLua(AMethodName: String; AClass: TLuaClass; ARefId: Integer): TLuaClassMethodInvoker;
begin
  Result:=TLuaClassMethodInvoker.Create(AClass.Lua, oaNone);
  with Result do
  begin
    FInvoker.N:=AMethodName;
    FInvoker.C:=AClass;
    FInvoker.T:=mtLua;
    FInvoker.R:=ARefId;

    FLua.PushRef(ARefId);
    FromStack(-1);
    PopFromStack;
  end;
end;

class function TLuaClassMethodInvoker.NewNative(AMethodName: String; AClass: TLuaClass; ACallback: TLuaClassMethodEvent): TLuaClassMethodInvoker;
begin
  Result:=TLuaClassMethodInvoker.Create(AClass.Lua, oaNone);
  with Result do
  begin
    FInvoker.N:=AMethodName;
    FInvoker.C:=AClass;
    FInvoker.T:=mtNative;
    FInvoker.E:=ACallback;
    FInvoker.F:=TLuaCallbackWrapper.New(LuaInvokeHandler).Callback;

    FLua.Stack.PushFunction(FInvoker.F);
    FromStack(-1);
    PopFromStack;
  end;
end;

procedure TLuaClassMethodInvoker.LuaInvokeHandler(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
var
  Method: TLuaClassMethod;
begin
  if FInvoker.T = mtNative then
  begin
    // First arg should be the class, we dont want it!
    Args.Purge(0);

    // Create a pseudo method, because mostly the class will not have it when the func is overwritten in lua!
    Method:=TLuaClassMethod.Create(FInvoker.C, FInvoker.N, FInvoker.E);
    try
      // Call diz shit
      FInvoker.E(Sender, FInvoker.C, Method, Args, Results);
    finally
      Method.Free;
    end;
  end;
end;

function TLuaClassMethodInvoker.Execute: Boolean;
var
  Res: Integer;
begin
  Res:=0;
  Result:=False;

  // **                                            ** //
  // Do we need all this shit if it is a native call? //
  // **                                            ** //

  // Push the function to stack
  PushToStack;

  // Push the class table to stack
  FInvoker.C.PushToStack;

  // Push the args to the stack
  FArgs.ApplyToStack;

  try
    // Execute the function
    Res:=FLua.Stack.PCall(FArgs.Count + 1, LUA_MULTRET, 0);
    case Res of
      LUA_ERRRUN: raise ELuaExecuteException.Create('Runtime error');
      LUA_ERRMEM: raise ELuaExecuteException.Create('Memory allocation error');
      LUA_ERRSYNTAX: raise ELuaExecuteException.Create('Syntax error');
      LUA_ERRERR: raise ELuaExecuteException.Create('Error handling function failed');
    end;

    // Set result
    Result:=Res = LUA_OK;
  except
    on E: ELuaException do
    begin
      // Set exception props
      E.FName:=FInvoker.C.Blueprint.Name + ':' + FInvoker.N;
      E.FCode:=Res;
      E.FLuaMessage:=FLua.Stack.ToString(-1);

      // Handle the error
      FLua.HandleLuaError(E);

      // Pop the message from stack
      FLua.Stack.Pop;
    end;
  end;
end;

destructor TLuaClassMethodInvoker.Destroy;
begin
  if FInvoker.T = mtNative then
  begin
    TLuaCallbackWrapper.Release(FInvoker.F);
  end;

  inherited;
end;

{ TLuaClassInheritor }

constructor TLuaClassInheritor.Create(ALua: TLua);
begin
  inherited Create;

  FLua:=ALua;
end;

destructor TLuaClassInheritor.Destroy;
begin
  inherited;
end;

procedure TLuaClassInheritor.Activate;
begin
  // Check if a class inheritor is already active in this state
  FLua.Stack.PushString('__TLuaClassInheritor');
  if NOT FLua.Stack.IsPointer(FLua.Stack.RawGet(LUA_REGISTRYINDEX)) then
  begin
    FLua.Stack.PushString('__TLuaClassInheritor');
    FLua.Stack.PushPointer(Self);
    FLua.Stack.RawSet(LUA_REGISTRYINDEX);

    // Register the class method
    FLua.RegisterMethod('class', LuaInheritClass);
    // Register the inherit method
    FLua.RegisterMethod('inherited', LuaInheritMethod);
  end;
  FLua.Stack.Pop;
end;

procedure TLuaClassInheritor.LuaInheritClass(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
var
  Blueprint: TLuaClassBlueprint;
begin
  if Args.Check([ltBlueprint]) then
  begin
    Blueprint:=Args[0].AsBlueprint;
    if Assigned(Blueprint) then
    begin
      Blueprint:=Blueprint.Inherit;
      Blueprint.Register;

      Sender.Stack.GetGlobal(Blueprint.Name);
      Results.PushByStack(-1);
      Sender.Stack.Pop;
    end;
  end;
end;

procedure TLuaClassInheritor.LuaInheritMethod(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
begin
  if Args.Check([ltClass, ltString], False) then
  begin
    with Args[0].AsClass.Inherit(Args[1].AsStr) do
    begin
      try
        Execute;
      finally
        Free;
      end;
    end;
  end;
end;

{ TLuaLibrary }

constructor TLuaLibrary.Create(ALua: TLua; ALibName: String);
var
  I: Integer;
begin
  inherited Create;

  FLua:=ALua;
  FName:=ALibName;
  FFunctions:=TList<LibFunction>.Create;

  FLuaIndexHandler:=TLuaCallbackWrapper.New(LuaIndexHandler, LuaNewIndexHandler).Callback;

  SetLength(FConstants, HASH_BUCKET_SIZE);
  for I:=0 to HASH_BUCKET_SIZE - 1 do
  begin
    FConstants[I]:=TList<LibConstant>.Create;
  end;
end;

destructor TLuaLibrary.Destroy;
var
  I: Integer;
begin
  TLuaCallbackWrapper.Release(FLuaIndexHandler);

  FLua.NotifyCleanup(Self, opRemove);

  for I:=0 to FFunctions.Count - 1 do
  begin
    TLuaCallbackWrapper.Release(FFunctions[I].C);
  end;

  for I:=0 to Length(FConstants) - 1 do
  begin
    FConstants[I].Free;
  end;

  FreeAndNil(FFunctions);

  inherited;
end;

function TLuaLibrary.HashOf(const AName: String): Cardinal;
var
  I: Integer;
begin
  Result:=0;

  for I:=0 to AName.Length - 1 do
  begin
    Result:=((Result SHL 2) OR (Result SHR (SizeOf(Result) * 8 - 2))) XOR Ord(AName.Chars[I]);
  end;
end;

procedure TLuaLibrary.LuaIndexHandler(Sender: TLua; Clazz: TLuaClass; Access, Value: TLuaValue);
var
  Item: LibConstant;
begin
  if Access.IsStr then
  begin
    if Find(Access.AsStr, Item) >= 0 then
    begin
      Value.SetAsVariant(Item.V);
    end;
  end;
end;

procedure TLuaLibrary.LuaNewIndexHandler(Sender: TLua; Clazz: TLuaClass; Access, Value: TLuaValue);
begin

end;

function TLuaLibrary.Find(const AName: String; var AItem: LibConstant): Integer;
var
  Hash, Pos: Integer;
begin
  Result:=-1;
  Hash:=HashOf(AName) MOD Cardinal(Length(FConstants));

  if Assigned(FConstants[Hash]) then
  begin
    Pos:=0;

    while Pos < FConstants[Hash].Count do
    begin
      if AnsiSameText(AName, FConstants[Hash][Pos].N) then
      begin
        AItem:=FConstants[Hash][Pos];
        Result:=Pos;

        Break;
      end;

      Inc(Pos, 1);
    end;
  end;
end;

procedure TLuaLibrary.Register;
var
  I, Table, MetaTable: Integer;
begin
  Table:=FLua.Stack.NewTable;

  for I:=0 to FFunctions.Count - 1 do
  begin
    FLua.Stack.PushFunction(FFunctions[I].C);
    FLua.Stack.SetField(Table, FFunctions[I].N);
  end;

  MetaTable:=FLua.Stack.NewTable;
  FLua.Stack.PushFunction(FLuaIndexHandler);
  FLua.Stack.SetField(MetaTable, '__index');
  FLua.Stack.PushFunction(FLuaIndexHandler);
  FLua.Stack.SetField(MetaTable, '__newindex');
  FLua.Stack.SetMetaTable(Table);

  FLua.Stack.SetGlobal(FName);
end;

procedure TLuaLibrary.AddConstant(AName: String; AValue: Variant);
var
  Entry: LibConstant;
begin
  Entry.N:=AName;
  Entry.V:=AValue;

  FConstants[HashOf(AName) MOD Cardinal(Length(FConstants))].Add(Entry);
end;

procedure TLuaLibrary.AddFunction(AName: String; ACallback: TLuaMethodEvent);
var
  Entry: LibFunction;
begin
  Entry.N:=AName;
  Entry.C:=TLuaCallbackWrapper.New(ACallback).Callback;

  FFunctions.Add(Entry);
end;

{ TLua }

constructor TLua.Create;
begin
  inherited Create;

  // Initialize
  FCleanupList:=TObjectList.Create;
  FScriptSource:=TStringList.Create;
  FClassInheritor:=TLuaClassInheritor.Create(Self);
  FClassBlueprints:=TObjectList<TLuaClassBlueprint>.Create;
  FErrorHandlers:=TInterfaceList.Create;
  FFunctions:=THashedStringList.Create(dupError, True, False);

  // Create lua state and open default libs
  FState:=lua_newstate(LuaDefaultAllocator, Self);
  luaL_openlibs(FState);

  // Store this instance to the state
  lua_pushliteral(FState, '__TLua');
  lua_pushlightuserdata(FState, Self);
  lua_rawset(FState, LUA_REGISTRYINDEX);

  // Create the stack class
  FStack:=TLuaStack.Create(FState);

  // Activate the class inheritor
  FClassInheritor.Activate;
end;

constructor TLua.Create(AState: TLuaState);
begin
  inherited Create;

  FStatus:=lssInitialize;

  // Initialize
  FCleanupList:=TObjectList.Create;
  FScriptSource:=TStringList.Create;
  FStack:=TLuaStack.Create(AState);
  FClassInheritor:=TLuaClassInheritor.Create(Self);
  FClassBlueprints:=TObjectList<TLuaClassBlueprint>.Create;
  FErrorHandlers:=TInterfaceList.Create;
  FFunctions:=THashedStringList.Create;

  // Save state
  FState:=AState;

  // Flag this as volatile, we do not want to do our stuff with this state
  FVolatile:=True;

  // Activate the class inheritor
  FClassInheritor.Activate;

  FStatus:=lssReady;
end;

destructor TLua.Destroy;
begin
  FStatus:=lssDestroy;

  // Execute pre-cleanup actions
  PreCleanup;

  // Free objects that depend on the state
  FreeAndNil(FCleanupList);
  FreeAndNil(FClassInheritor);
  FreeAndNil(FClassBlueprints);
  FreeAndNil(FFunctions);

  // Free the state
  if NOT FVolatile then
  begin
    try
      lua_close(FState);
    finally
      FState:=nil;
    end;

    // Cleanup the callback wrappers that where still requierd by the state...
    TLuaCallbackWrapper.CleanupRelease;
  end;

  // Free the other stuff
  FreeAndNil(FStack);
  FreeAndNil(FScriptSource);
  FreeAndNil(FErrorHandlers);

  inherited;
end;

class function TLua.Acquire(AState: TLuaState): TLua;
begin
  // Get the TLua instace from the state
  lua_pushliteral(AState, '__TLua');
  lua_rawget(AState, LUA_REGISTRYINDEX);
  Result:=Pointer(lua_topointer(AState, -1));

  // Pop the value from the stack
  lua_pop(AState, 1);

  // Is the acquired state the same we are awaiting
  if Result.State <> AState then
  begin
    Result:=TLua.Volatile(AState);
  end;
end;

function TLua.GetClasses(AName: String): TLuaClassBlueprint;
var
  I: Integer;
begin
  Result:=nil;

  for I:=0 to FClassBlueprints.Count - 1 do
  begin
    if AnsiSameText(FClassBlueprints[I].FName, AName) then
    begin
      Exit(FClassBlueprints[I]);
    end;
  end;
end;

class function TLua.Volatile(AState: TLuaState): TLua;
begin
  Result:=TLua.Create(AState);
end;

function TLua.Execute: Boolean;
var
  Res: Integer;
begin
  Res:=0;
  Result:=False;

  try
    Res:=luaL_loadbuffer(FState, PAnsiChar(AnsiString(Trim(FScriptSource.Text))), Trim(FScriptSource.Text).Length, PAnsiChar(AnsiString(FScriptName)));

    case Res of
      LUA_ERRSYNTAX: raise ELuaLoadException.Create('Syntax error during precompilation');
      LUA_ERRMEM: raise ELuaLoadException.Create('Memory allocation error');
    end;

    if Res <> LUA_OK then
    begin
      raise ELuaLoadException.Create('Cannot load buffer for script');
    end;

    Res:=lua_pcall(FState, 0, LUA_MULTRET, 0);

    case Res of
      LUA_ERRRUN: raise ELuaExecuteException.Create('Runtime error');
      LUA_ERRMEM: raise ELuaExecuteException.Create('Memory allocation error');
      LUA_ERRSYNTAX: raise ELuaExecuteException.Create('Syntax error');
      LUA_ERRERR: raise ELuaExecuteException.Create('Error handling function failed');
    end;

    Result:=Res = LUA_OK;
  except
    on E: ELuaException do
    begin
      // Set exception props
      E.FName:=FScriptName;
      E.FCode:=Res;
      E.FLuaMessage:=FStack.ToString(-1);

      // Process the error
      HandleLuaError(E);

      // Remove the error msg from the stack
      FStack.Pop;
    end;
  end;
end;

function TLua.ExecuteDirect(ASource: String): Boolean;
var
  State: TLuaState;
  RefId: Integer;
begin
  State:=lua_newthread(FState);

  RefId:=luaL_ref(FState, LUA_REGISTRYINDEX);
  try
    Result:=luaL_dostring(State, PAnsiChar(AnsiString(ASource))) = LUA_OK;
  finally
    luaL_unref(FState, LUA_REGISTRYINDEX, RefId);
  end;
end;

function TLua.GetFunctions(Name: String): TLuaFunction;
var
  Idx: Integer;
begin
  Result:=nil;

  Idx:=FFunctions.IndexOf(Name);
  if Idx >= 0 then
    Result:=TLuaFunction(FFunctions.Objects[Idx]);
end;

function TLua.GetGlobals(Name: String): Variant;
begin
  FStack.GetGlobal(Name);
  Result:=FStack.ToVariant(-1);
end;

function TLua.GetTables(Name: String): TLuaTable;
begin
  Result:=nil;

  FStack.GetGlobal(Name);
  try
    if FStack.IsTable(-1) then
    begin
      Result:=TLuaTable.New(Self, -1);
      Result.FName:=Name;

      FCleanupList.Add(Result);
    end;
  finally
    FStack.Pop;
  end;
end;

function TLua.LoadSource(AFileName: String): Boolean;
begin
  Result:=False;

  if FileExists(AFileName) then
  begin
    FScriptName:=ExtractFileName(AFileName);
    FScriptSource.LoadFromFile(AFileName);

    Result:=True;
  end;
end;

function TLua.NewValue(AValue: Variant; AName: String = ''): TLuaValue;
begin
  Result:=nil;

  try
    Result:=TLuaValue.Clear(Self);
    Result.AsVariant:=AValue;

    if AName <> '' then
    begin
      Result.PushToStack;
      FStack.SetGlobal(AName);
    end;
  finally
    FCleanupList.Add(Result);
  end;
end;

function TLua.NewClass(AName: String): TLuaClassBlueprint;
begin
  Result:=nil;

  try
    Result:=TLuaClassBlueprint.Create(Self, AName);
  finally
    FClassBlueprints.Add(Result);
  end;
end;

function TLua.NewLibrary(AName: String): TLuaLibrary;
begin
  Result:=nil;

  try
    Result:=TLuaLibrary.Create(Self, AName);
  finally
    FCleanupList.Add(Result);
  end;
end;

function TLua.NewTable(AName: String = ''): TLuaTable;
begin
  Result:=nil;

  try
    Result:=TLuaTable.New(Self, AName);
  finally
    FCleanupList.Add(Result);
  end;
end;

function TLua.NewThread: TLuaThread;
begin
  Result:=TLuaThread.Create(Self);

  TLuaInternalCore.GetInstance.RegisterThread(Result);
end;

procedure TLua.NotifyCleanup(ASubject: TObject; AOperation: TOperation);
begin
  if Assigned(FCleanupList) then
  begin
    case AOperation of
      opInsert:
      begin
        if FCleanupList.IndexOf(ASubject) < 0 then
        begin
          FCleanupList.Add(ASubject);
        end;
      end;
      opRemove:
      begin
        if FCleanupList.IndexOf(ASubject) >= 0 then
        begin
          FCleanupList.Extract(ASubject);
        end;
      end;
    end;
  end;
end;

procedure TLua.PreCleanup;
var
  I: Integer;
begin
  for I:=0 to FFunctions.Count - 1 do
    FFunctions.Objects[I].Free;

  for I:=0 to FClassBlueprints.Count - 1 do
    FClassBlueprints[I].CleanupInstances;
end;

function TLua.PushAndUnref(ARefId: Integer): Integer;
begin
  if TLuaInternalCore.GetInstance.HasRef(ARefId, Self) then
  begin
    Result:=FStack.RawGetI(LUA_REGISTRYINDEX, ARefId);

    TLuaInternalCore.GetInstance.RemoveRef(Result, Self);

    FStack.UnRef(LUA_REGISTRYINDEX, ARefId);
  end else
  begin
    Result:=FStack.PushNil;
  end;
end;

function TLua.PushRef(ARefId: Integer): Integer;
begin
  if TLuaInternalCore.GetInstance.HasRef(ARefId, Self) then
  begin
    Result:=FStack.RawGetI(LUA_REGISTRYINDEX, ARefId);
  end else
  begin
    Result:=FStack.PushNil;
  end;
end;

procedure TLua.RegisterCFunction(AName: String; AFunc: TLuaCFunctionEvent);
begin
  lua_register(FState, PAnsiChar(AnsiString(AName)), AFunc);
end;

procedure TLua.RegisterFunction(AName: String; AFunc: TLuaFunctionEvent);
var
  Wrapper: TLuaCallbackWrapper;
begin
  Wrapper:=TLuaCallbackWrapper.New(AFunc);
  try
    lua_register(FState, PAnsiChar(AnsiString(AName)), Wrapper.Callback);
  finally
    FCleanupList.Add(Wrapper);
  end;
end;

procedure TLua.RegisterMethod(AName: String; AMethod: TLuaMethodEvent);
var
  Wrapper: TLuaCallbackWrapper;
begin
  Wrapper:=TLuaCallbackWrapper.New(AMethod);
  try
    lua_register(FState, PAnsiChar(AnsiString(AName)), Wrapper.Callback);
  finally
    FCleanupList.Add(Wrapper);
  end;
end;

procedure TLua.RegisterProcedure(AName: String; AProc: TLuaProcedure);
var
  Wrapper: TLuaCallbackWrapper;
begin
  Wrapper:=TLuaCallbackWrapper.New(AProc);
  try
    lua_register(FState, PAnsiChar(AnsiString(AName)), Wrapper.Callback);
  finally
    FCleanupList.Add(Wrapper);
  end;
end;

procedure TLua.SetGlobals(Name: String; const Value: Variant);
begin
  FStack.PushVariant(Value);
  FStack.SetGlobal(Name);
end;

procedure TLua.SetScriptSource(const Value: TStrings);
begin
  FScriptSource.Assign(Value);
end;

procedure TLua.SetTables(Name: String; const Value: TLuaTable);
begin
  // Push, if assigned, the table! Otherwise nil
  if Assigned(Value) then
  begin
    Value.PushToStack;
  end else
  begin
    FStack.PushNil;
  end;

  FStack.SetGlobal(Name);
end;

procedure TLua.HandleLuaError(AException: ELuaException);
var
  I: Integer;
begin
  for I:=0 to FErrorHandlers.Count - 1 do
  begin
    if AException IS ELuaLoadException then
    begin
      ILuaErrorHandler(FErrorHandlers[I]).OnScriptLoadError(AException.Name, AException.Message, AException.Code, AException.LuaMessage);
    end else
    if AException IS ELuaExecuteException then
    begin
      ILuaErrorHandler(FErrorHandlers[I]).OnScriptExecutionError(AException.Name, AException.Message, AException.Code, AException.LuaMessage);
    end;
  end;
end;

function TLua.IntroduceFunction(AName: String): Boolean;
begin
  Result:=False;

  FStack.GetGlobal(AName);
  try
    if FStack.IsFunction(-1) AND (FFunctions.IndexOf(AName) <= 0) then
    begin
      FFunctions.AddObject(AName, TLuaFunction.New(Self, -1));

      Result:=True;
    end;
  finally
    FStack.Pop;
  end;
end;

procedure TLua.RegisterErrorHandler(const AErrorHandler: ILuaErrorHandler);
begin
  if FErrorHandlers.IndexOf(AErrorHandler) < 0 then
  begin
    FErrorHandlers.Add(AErrorHandler);
  end;
end;

procedure TLua.UnregisterErrorHandler(const AErrorHandler: ILuaErrorHandler);
begin
  if FErrorHandlers.IndexOf(AErrorHandler) >= 0 then
  begin
    FErrorHandlers.Remove(AErrorHandler);
  end;
end;

{ TLuaThread }

constructor TLuaThread.Create(ALua: TLua);
begin
  inherited Create;

  FLua:=ALua;
  FOwned:=True;
  FThread:=lua_newthread(FLua.State);
  FRefId:=luaL_ref(FLua.State, LUA_REGISTRYINDEX);

  FStack:=TLuaStack.Create(FThread);
end;

constructor TLuaThread.Create(ALua: TLua; AStackIdx: Integer);
begin
  inherited Create;

  FLua:=ALua;
  FOwned:=False;
  FRefId:=LUA_NOREF;

  if FLua.Stack.IsThread(AStackIdx) then
  begin
    FThread:=FLua.Stack.ToThread(AStackIdx);
  end;

  FStack:=TLuaStack.Create(FThread);
end;

destructor TLuaThread.Destroy;
begin
  FreeAndNil(FStack);

  if FOwned AND Assigned(FThread) then
  begin
    // Unref the thread
    luaL_unref(FLua.State, LUA_REGISTRYINDEX, FRefId);

    // Force GC, this will cleanup the thread
    lua_gc(FLua.State, LUA_GCCOLLECT, 0);
  end;

  inherited;
end;

function TLuaThread.Execute(AString: String): Boolean;
var
  Count: Integer;
begin
  Result:=False;

  if luaL_loadstring(FThread, PAnsiChar(AnsiString(AString))) = 0 then
  begin
    Count:=lua_gettop(FThread);
    if lua_pcallk(FThread, 0, LUA_MULTRET, 0, nil, nil) = 0 then
    begin
      Count:=Abs(lua_gettop(FThread) - Count);

      // Process results?
      if Count > 0 then
      begin

      end;

      Result:=True;
    end else
    begin

    end;
  end else
  begin
    FLastErrorMessage:=String(System.AnsiStrings.StrPas(lua_tostring(FThread, -1)));
  end;
end;

{ TLuaStack }

constructor TLuaStack.Create(AState: TLuaState);
begin
  inherited Create;

  FState:=AState;
end;

procedure TLuaStack.DumpToStdOut;
var
  I: Integer;
begin
  for I:=1 to Top do
  begin
    WriteLn(String(IntToStr(I) + '# (' + LUA_TYPES[lua_type(FState, I)] + '): ' + VarToStrDef(luaL_tovariant(FState, I), '')));
  end;
end;

procedure TLuaStack.CopyTable(ASource, ADest, AMeta: Integer);
begin
  luaL_copytable(FState, ASource, ADest, AMeta);
end;

function TLuaStack.GetGlobal(AName: String): Integer;
begin
  lua_getglobal(FState, PAnsiChar(AnsiString(AName)));
  Result:=lua_gettop(FState);
end;

function TLuaStack.GetMetaTable(AName: String): Integer;
begin
  luaL_getmetatable(FState, PAnsiChar(AnsiString(AName)));
  Result:=lua_gettop(FState);
end;

function TLuaStack.IsBoolean(AIndex: Integer): Boolean;
begin
  Result:=lua_isboolean(FState, AIndex);
end;

function TLuaStack.IsNumber(AIndex: Integer): Boolean;
begin
  Result:=lua_isnumber(FState, AIndex);
end;

function TLuaStack.IsFunction(AIndex: Integer): Boolean;
begin
  Result:=lua_isfunction(FState, AIndex);
end;

function TLuaStack.IsInteger(AIndex: Integer): Boolean;
begin
  Result:=lua_isinteger(FState, AIndex);
end;

function TLuaStack.IsNil(AIndex: Integer): Boolean;
begin
  Result:=lua_isnil(FState, AIndex);
end;

function TLuaStack.IsNone(AIndex: Integer): Boolean;
begin
  Result:=lua_isnone(FState, AIndex);
end;

function TLuaStack.IsPointer(AIndex: Integer): Boolean;
begin
  Result:=lua_islightuserdata(FState, AIndex);
end;

function TLuaStack.IsString(AIndex: Integer): Boolean;
begin
  Result:=lua_isstring(FState, AIndex);
end;

function TLuaStack.IsTable(AIndex: Integer): Boolean;
begin
  Result:=lua_istable(FState, AIndex);
end;

function TLuaStack.IsThread(AIndex: Integer): Boolean;
begin
  Result:=lua_isthread(FState, AIndex);
end;

function TLuaStack.NewMetaTable(AName: String): Integer;
begin
  Result:=-1;

  if luaL_newmetatable(FState, PAnsiChar(AnsiString(AName))) <> 0 then
  begin
    Result:=lua_gettop(FState);
  end;
end;

function TLuaStack.NewTable: Integer;
begin
  lua_newtable(FState);
  Result:=lua_gettop(FState);
end;

function TLuaStack.Next(AIndex: Integer): Boolean;
begin
  Result:=lua_next(FState, AIndex) <> 0;
end;

procedure TLuaStack.DumpToDebugOut;
var
  I: Integer;
begin
  for I:=1 to Top do
  begin
    if lua_type(FState, I) IN [LUA_TNIL, LUA_TBOOLEAN, LUA_TNUMBER, LUA_TSTRING] then
    begin
      OutputDebugString(PChar(String(IntToStr(I) + '# (' + LUA_TYPES[lua_type(FState, I)] + '): ' + VarToStrDef(luaL_tovariant(FState, I), ''))));
    end else
    begin
      OutputDebugString(PChar(String(IntToStr(I) + '# (' + LUA_TYPES[lua_type(FState, I)] + '): <can not dump>')));
    end;
  end;
end;

function TLuaStack.PCall(AArgsCount, AResultCount, AErrorFuncIdx: Integer): Integer;
begin
  Result:=lua_pcall(FState, AArgsCount, AResultCount, AErrorFuncIdx);
end;

procedure TLuaStack.Pop(ACount: Integer);
begin
  lua_pop(FState, ACount);
end;

function TLuaStack.PushBoolean(AValue: Boolean): Integer;
begin
  lua_pushboolean(FState, AValue);
  Result:=lua_gettop(FState);
end;

function TLuaStack.PushNumber(AValue: Double): Integer;
begin
  lua_pushnumber(FState, AValue);
  Result:=lua_gettop(FState);
end;

function TLuaStack.PushFunction(AValue: TLuaCFunctionEvent): Integer;
begin
  lua_pushcfunction(FState, AValue);
  Result:=lua_gettop(FState);
end;

function TLuaStack.PushInteger(AValue: Int64): Integer;
begin
  lua_pushinteger(FState, AValue);
  Result:=lua_gettop(FState);
end;

function TLuaStack.PushNil: Integer;
begin
  lua_pushnil(FState);
  Result:=lua_gettop(FState);
end;

function TLuaStack.PushPointer(AValue: Pointer): Integer;
begin
  lua_pushlightuserdata(FState, AValue);
  Result:=lua_gettop(FState);
end;

function TLuaStack.PushString(AValue: String): Integer;
begin
  lua_pushliteral(FState, PAnsiChar(AnsiString(AValue)));
  Result:=lua_gettop(FState);
end;

function TLuaStack.PushValue(AIndex: Integer): Integer;
begin
  lua_pushvalue(FState, AIndex);
  Result:=lua_gettop(FState);
end;

function TLuaStack.PushVariant(AValue: Variant): Integer;
begin
  luaL_pushvariant(FState, AValue);
  Result:=lua_gettop(FState);
end;

function TLuaStack.RawGet(AIndex: Integer): Integer;
begin
  lua_rawget(FState, AIndex);
  Result:=lua_gettop(FState);
end;

function TLuaStack.RawGetI(AIndex: Integer; AKey: NativeInt): Integer;
begin
  lua_rawgeti(FState, AIndex, AKey);
  Result:=lua_gettop(FState);
end;

procedure TLuaStack.RawSet(AIndex: Integer);
begin
  lua_rawset(FState, AIndex);
end;

procedure TLuaStack.RawSetI(AIndex: Integer; AKey: NativeInt);
begin
  lua_rawseti(FState, AIndex, AKey);
end;

function TLuaStack.Ref(ATable: Integer): Integer;
begin
  Result:=luaL_ref(FState, ATable);
end;

procedure TLuaStack.Remove(AIndex: Integer);
begin
  lua_remove(FState, AIndex);
end;

procedure TLuaStack.SetField(ATableIndex: Integer; AName: String);
begin
  lua_setfield(FState, ATableIndex, PAnsiChar(AnsiString(AName)));
end;

procedure TLuaStack.SetFuncs(AFuncs: TLuaReg; ANumUpvalues: Integer);
begin
  luaL_setfuncs(FState, AFuncs, ANumUpvalues);
end;

procedure TLuaStack.SetGlobal(AName: String);
begin
  lua_setglobal(FState, PAnsiChar(AnsiString(AName)));
end;

procedure TLuaStack.SetMetaTable(AObjectIndex: Integer);
begin
  lua_setmetatable(FState, AObjectIndex);
end;

procedure TLuaStack.SetTable(AIndex: Integer);
begin
  lua_settable(FState, AIndex);
end;

function TLuaStack.ToBoolean(AIndex: Integer): Boolean;
begin
  Result:=lua_toboolean(FState, AIndex);
end;

function TLuaStack.ToNumber(AIndex: Integer): Double;
begin
  Result:=lua_tonumber(FState, AIndex);
end;

function TLuaStack.ToFunction(AIndex: Integer): TLuaCFunctionEvent;
begin
  Result:=lua_tocfunction(FState, AIndex);
end;

function TLuaStack.ToInteger(AIndex: Integer): Int64;
begin
  Result:=lua_tointeger(FState, AIndex);
end;

function TLuaStack.ToPointer(AIndex: Integer): Pointer;
begin
  Result:=lua_topointer(FState, AIndex);
end;

function TLuaStack.ToString(AIndex: Integer): String;
begin
  Result:=String(System.AnsiStrings.StrPas(lua_tostring(FState, AIndex)));
end;

function TLuaStack.ToThread(AIndex: Integer): TLuaState;
begin
  Result:=lua_tothread(FState, AIndex);
end;

function TLuaStack.ToVariant(AIndex: Integer): Variant;
begin
  Result:=luaL_tovariant(FState, AIndex);
end;

function TLuaStack.Top: Integer;
begin
  Result:=lua_gettop(FState);
end;

function TLuaStack.Typ(AIndex: Integer): Integer;
begin
  Result:=lua_type(FState, AIndex);
end;

function TLuaStack.TypAsName(AIndex: Integer): String;
begin
  Result:=LUA_TYPES[lua_type(FState, AIndex)];
end;

function TLuaStack.TypName(AIndex: Integer): String;
begin
  Result:=String(System.AnsiStrings.StrPas(lua_typename(FState, Typ(AIndex))));
end;

procedure TLuaStack.UnRef(ATable, ARefId: Integer);
begin
  luaL_unref(FState, ATable, ARefId);
end;

{ TLuaArgs }

constructor TLuaArgs.Create(ALua: TLua; ACount: Integer = -1);
begin
  inherited Create;

  FLua:=ALua;
  FValues:=TObjectList<TLuaValue>.Create;
  FCount:=ACount;

  GetArgs;
end;

destructor TLuaArgs.Destroy;
begin
  FreeAndNil(FValues);

  inherited;
end;

procedure TLuaArgs.GetArgs;
var
  I: Integer;
begin
  if FCount >= 0 then
  begin
    FCount:=Min(FCount, FLua.Stack.Top);
  end else
  begin
    FCount:=FLua.Stack.Top;
  end;

  for I:=1 to FCount do
  begin
    FValues.Add(TLuaValue.New(FLua, I));
    if FCount <> FLua.Stack.Top then
    begin
      raise ELuaExecuteException.Create('Stack manipulation error triggered, was ' + IntToStr(FCount) + ' now is ' + IntToStr(FLua.Stack.Top) + '.'#13#10'Inconsistent data, abort!');
    end;
  end;

  if FCount > 0 then
  begin
    FLua.Stack.Pop(FCount);
  end;
end;

function TLuaArgs.GetValue(Index: Integer): TLuaValue;
begin
  Result:=nil;

  if (Index >= 0) AND (Index < FValues.Count) then
  begin
    Result:=FValues[Index];
  end;
end;

procedure TLuaArgs.Purge(AIndex: Integer);
begin
  if (AIndex >= 0) AND (AIndex < FValues.Count) then
  begin
    try
      FValues.Delete(AIndex);
    finally
      Dec(FCount);
    end;
  end;
end;

procedure TLuaArgs.Update(ACount: Integer = -1);
begin
  // Set new limit
  FCount:=ACount;

  // Clear existing
  FValues.Clear;

  // Fetch new
  GetArgs;
end;

function TLuaArgs.Check(ATypes: Array of TLuaType; AExactLength: Boolean = True): Boolean;
var
  I: Integer;
begin
  Result:=False;

  if (AExactLength AND (FCount = Length(ATypes))) OR (NOT AExactLength AND (Length(ATypes) >= FCount)) then
  begin
    Result:=True;

    for I:=Low(ATypes) to High(ATypes) do
    begin
      if ATypes[I] = ltBlueprint then
      begin
        Result:=FValues[I].IsBlueprint;
      end else
      if ATypes[I] = ltClass then
      begin
        Result:=FValues[I].IsClass;
      end else
      if ATypes[I] = ltTable then
      begin
        Result:=FValues[I].IsTable;
      end else
      begin
        Result:=Result AND (FValues[I].Typ = ATypes[I]);
      end;

      if NOT Result then Break;
    end;
  end;
end;

{ TLuaResults }

constructor TLuaResults.Create(ALua: TLua);
begin
  inherited Create;

  FLua:=ALua;
  FValues:=TObjectList<TLuaValue>.Create;
end;

destructor TLuaResults.Destroy;
begin
  FreeAndNil(FValues);

  inherited;
end;

procedure TLuaResults.Clear;
begin
  FValues.Clear;
end;

procedure TLuaResults.ApplyToStack;
var
  I: Integer;
begin
  for I:=0 to FValues.Count - 1 do
  begin
    case FValues[I].Typ of
      ltNone: FLua.Stack.PushNil;
      ltNil: FLua.Stack.PushNil;
      ltBoolean: FLua.Stack.PushBoolean(FValues[I].AsBool);
      ltLightUserdata: FLua.Stack.PushPointer(FValues[I].AsPtr);
      ltNumber: if FValues[I].IsInt then FLua.Stack.PushInteger(FValues[I].AsInt) else FLua.Stack.PushNumber(FValues[I].AsFloat);
      ltString: FLua.Stack.PushString(FValues[I].AsStr);
      ltTable: FValues[I].PushToStack;
      ltFunction: FValues[I].PushToStack;
      ltUserdata: FValues[I].PushToStack;
      ltThread: FValues[I].PushToStack;
    end;
  end;
end;

function TLuaResults.GetCount: Integer;
begin
  Result:=FValues.Count;
end;

procedure TLuaResults.PushBool(AValue: Boolean);
begin
  FValues.Add(TLuaValue.FromValue(FLua, AValue));
end;

procedure TLuaResults.PushByRefId(ARefId: Integer);
begin
  FValues.Add(TLuaValue.FromRefId(FLua, ARefId));
end;

procedure TLuaResults.PushByStack(AIndex: Integer);
begin
  FValues.Add(TLuaValue.New(FLua, AIndex));
end;

procedure TLuaResults.PushClass(AValue: TLuaClass);
begin
  FValues.Add(TLuaValue.FromRefId(FLua, AValue.RefId));
end;

procedure TLuaResults.PushFloat(AValue: Double);
begin
  FValues.Add(TLuaValue.FromValue(FLua, AValue));
end;

procedure TLuaResults.PushInt(AValue: Int64);
begin
  FValues.Add(TLuaValue.FromValue(FLua, AValue));
end;

procedure TLuaResults.PushNil;
begin
  FValues.Add(TLuaValue.FromValue(FLua, nil));
end;

procedure TLuaResults.PushStr(AValue: String);
begin
  FValues.Add(TLuaValue.FromValue(FLua, AValue));
end;

procedure TLuaResults.PushValue(AValue: TLuaValue);
begin
  FValues.Add(TLuaValue.FromRefId(AValue.Lua, AValue.RefId));
end;

initialization
  TLuaInternalCore.GetInstance;

finalization
  TLuaInternalCore.GetInstance.Free;

end.
