{
Global application helpers.
}
unit app_helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{ Save string contents to a file }
function FilePutContents(const filename, contents: ansistring): Boolean;
{ Load string contents from a file }
function FileGetContents(const filename: ansistring; out buffer: ansistring): Boolean;
{ Execute an app and don't wait when it exits. }
procedure AppExec(const exename: string);

implementation

uses process;

function FilePutContents(const filename, contents: string): Boolean;
var
  fs: TFileStream;
begin
  Result := False;
  try
    fs := TFileStream.Create(filename, fmCreate);
    fs.Write(contents[1], length(contents));
    Result := True;
  finally
    fs.Free;
  end;
end;

function FileGetContents(const filename: ansistring; out buffer: ansistring): Boolean;
var
  str: TStringList;
begin
  Result := False;
  try
    str := TStringList.Create;
    str.LoadFromFile(filename);
    buffer := str.Text;
    Result := True;
  finally
    str.Free;
  end;
end;

procedure AppExec(const exename: string);
begin
  with TProcess.Create(nil) do begin
    Executable := exename;
    Execute;
    Free;
  end;
end;

end.

