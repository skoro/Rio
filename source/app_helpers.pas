{
=============================================================================
Global application helpers.
=============================================================================
}
unit app_helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ValEdit, Dialogs;

type

  TFindPos = record
    Pos: Integer;
    SelStart: Integer;
    SelLength: Integer;
  end;

{ Save string contents to a file }
function FilePutContents(const filename, contents: ansistring): Boolean;
{ Load string contents from a file }
function FileGetContents(const filename: ansistring; out buffer: ansistring): Boolean;
{ Execute an app and don't wait when it exits. }
procedure AppExec(const exename: string);
{ Split input string to list of strings delimited by character }
procedure SplitStrings(const Input: string; const Delim: char; Strings: TStringList);
function SplitKV(const Input: string; const Delim: char): TKeyValuePair;
{ Approximate milliseconds }
function FormatMsApprox(ms: Int64): string;
{ Separate number parts by dot. }
function NumberFormat(num: Int64; dot: string = '.'): string;
{ Finds all the named controls in an owner control. }
procedure EnumControls(const Owner: TWinControl; const ControlName: string; Controls: TList);

function FindInText(AText, Search: string; Options: TFindOptions; FromPos: Integer = 0): TFindPos;

implementation

uses process, LazUTF8, strutils;

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

procedure SplitStrings(const Input: string; const Delim: char; Strings: TStringList);
begin
  Strings.Clear;
  Strings.Delimiter := Delim;
  Strings.StrictDelimiter := True;
  Strings.DelimitedText := Input;
end;

function SplitKV(const Input: string; const Delim: char): TKeyValuePair;
var
  p: Integer;
begin
  p := Pos(delim, Input);
  if p = 0 then
  begin
    Result.Key := Input;
    Result.Value := '';
    Exit; // =>
  end;
  Result.Key := LeftStr(Input, p - 1);
  Result.Value := Trim(RightStr(Input, Length(Input) - p));
end;

function FormatMsApprox(ms: Int64): string;
var
  seconds, minutes, hours: Integer;
begin
  Result := Format('%d ms', [ms]);
  if ms >= 1000 then begin
    seconds := round(ms / 1000);
    Result := Format('~ %d sec', [seconds]);
    if seconds >= 60 then begin
      minutes := round(seconds / 60);
      seconds := seconds mod 60;
      Result := Format('~ %d min %d sec', [minutes, seconds]);
      if minutes >= 60 then begin
        hours := round(minutes / 60);
        minutes := minutes mod 60;
        Result := Format('%d h %d min', [hours, minutes]);
      end;
    end;
  end;
end;

function NumberFormat(num: Int64; dot: string): string;
var
  s: string;
  i, n: Integer;
begin
  n := 1;
  s := IntToStr(num);
  Result := '';
  for i := Length(s) downto 1 do begin
    Result := s[i] + result;
    if (i <> 1) and (n mod 3 = 0) then
      Result := dot + result;
    Inc(n);
  end;
end;

procedure EnumControls(const Owner: TWinControl; const ControlName: string; Controls: TList);
var
  cnt: integer;
  ctrl: TControl;
begin
  for cnt := 0 to Owner.ControlCount - 1 do begin
    ctrl := Owner.Controls[cnt];
    if ctrl is TWinControl then
      EnumControls(TWinControl(ctrl), ControlName, Controls);
    if ctrl.ClassName = ControlName then begin
      Controls.Add(ctrl);
    end;
  end;
end;

// http://wiki.freepascal.org/TMemo#Search_text
function FindInText(AText, Search: string; Options: TFindOptions; FromPos: Integer): TFindPos;
var
  p: Integer;
  StrRes: string;
  done: Boolean;
begin
  done := False;

  while not done do begin
    if not (frMatchCase in Options) then begin
      AText := Utf8LowerCase(AText);
      Search := Utf8LowerCase(Search);
    end;

    if frDown in Options then
      p := PosEx(Search, AText, FromPos)
    else begin
      if FromPos = 0 then
        FromPos := UTF8Length(AText);
      p := RPosex(Search, AText, FromPos);
    end;

    Result.Pos := -1;
    Result.SelStart := -1;

    if p = 0 then
      Exit;

    Result.Pos := p;
    Result.SelStart := UTF8Length(PChar(AText), p - 1);
    Result.SelLength := UTF8Length(Search);

    if frWholeWord in Options then begin
      StrRes := MidBStr(AText, p - 1, Length(Search) + 2);
      if ((p = 1) or (StrRes[1] in StdWordDelims)) and
          (StrRes[Length(StrRes)] in StdWordDelims) then
        Done := True;
      FromPos := p + Length(Search);
    end
    else
      done := True;
  end;
end;

end.

