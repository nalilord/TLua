unit LuaTestCompat;

{$I ../Source/LuaCompiler.inc}

interface

uses
  {$IFDEF FPC}
  cwstring,
  {$ENDIF}
  SysUtils, Classes;

function TempFilePath(const AFileName: string): string;
procedure WriteAllText(const AFileName, AText: string);
function FileExistsCompat(const AFileName: string): Boolean;
procedure DeleteFileCompat(const AFileName: string);

implementation

{$IFNDEF FPC}
uses
  IOUtils;
{$ENDIF}

{$IFDEF FPC}

function TempFilePath(const AFileName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) + AFileName;
end;

procedure WriteAllText(const AFileName, AText: string);
var
  Bytes: RawByteString;
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    Bytes := UTF8Encode(AText);
    if Bytes <> '' then
      Stream.WriteBuffer(Pointer(Bytes)^, Length(Bytes));
  finally
    Stream.Free;
  end;
end;

function FileExistsCompat(const AFileName: string): Boolean;
begin
  Result := FileExists(AFileName);
end;

procedure DeleteFileCompat(const AFileName: string);
begin
  DeleteFile(AFileName);
end;

{$ELSE}

function TempFilePath(const AFileName: string): string;
begin
  Result := TPath.Combine(TPath.GetTempPath, AFileName);
end;

procedure WriteAllText(const AFileName, AText: string);
begin
  TFile.WriteAllText(AFileName, AText);
end;

function FileExistsCompat(const AFileName: string): Boolean;
begin
  Result := TFile.Exists(AFileName);
end;

procedure DeleteFileCompat(const AFileName: string);
begin
  TFile.Delete(AFileName);
end;

{$ENDIF}

end.
