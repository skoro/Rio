{
Global application helpers.
}
unit app_helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{
  Save string contents to a file.
}
function FilePutContents(const filename, contents: ansistring): Boolean;

implementation

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

end.

