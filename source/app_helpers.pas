{
=============================================================================
Global application helpers.
=============================================================================
}
unit app_helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ValEdit, Dialogs, Forms;

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
procedure AppExec(const exename: string; params: array of string);
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
procedure Tokenize(txt: string; Tokens: TStrings);

// Show the confirmation dialog.
// Returns mrOK on Yes and mrCancel on No.
function ConfirmDlg(Caption, Txt: string): TModalResult;

// Dialogs.
procedure OKMsg(Caption, Txt: string);
procedure ERRMsg(Caption, Txt: string);
procedure WarnMsg(Caption, Txt: string);

implementation

uses process, LazUTF8, SynEditTypes, strutils;

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

procedure AppExec(const exename: string; params: array of string);
var
  par: string;
begin
  with TProcess.Create(nil) do begin
    Executable := exename;
    for par in params do
      Parameters.Add(par);
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
const
  WordDelims = TSynWordBreakChars + TSynWhiteChars;
var
  p, wlen: Integer;
  StrRes: string;
  done: Boolean;
begin
  done := False;

  while not done do begin
    if not (frMatchCase in Options) then begin
      AText := Utf8LowerCase(AText);
      Search := Utf8LowerCase(Search);
    end;

    if frDown in Options then begin
      if FromPos = 0 then
        FromPos := 1;
      p := PosEx(Search, AText, FromPos);
    end
    else begin
      if FromPos = 0 then
        FromPos := UTF8Length(AText);
      if FromPos < 0 then
        p := 0
      else
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
      if (p = 1) then begin
        if AText = Search then
          Done := True
        else
          if AText[Length(Search) + 1] in WordDelims then
            Done := True;
      end
      else begin
        StrRes := MidBStr(AText, p - 1, Length(Search) + 2);
        wlen := Length(StrRes);
        if (wlen = Length(Search) + 2) then
          if (StrRes[1] in WordDelims) and (StrRes[wlen] in WordDelims) then
            Done := True;
        if (p = Length(AText)) and (StrRes[1] in WordDelims) then
          Done := True;
      end;
      if frDown in Options then
        FromPos := p + Length(Search)
      else
        FromPos := p - Length(Search);
    end
    else
      done := True;
  end;
end;

procedure Tokenize(txt: string; Tokens: TStrings);
var
  Items: TStringArray;
  Token, buf: string;
  Quote: char;
  isString: Boolean;

  procedure AddStr(s: string);
  begin
    Tokens.Add(Copy(s, 2, s.Length - 2));
  end;

begin
  Tokens.Clear;
  Items := AnsiString(txt).Split([' ']);
  isString := False;
  for Token in Items do begin
    if not isString and Token.IsEmpty then
      Continue;
    if (not isString) and (not Token.IsEmpty) and ((Token[1] = '"') or (Token[1] = '''')) then begin
      Quote := Token[1];
      Buf := Token.TrimRight;
      if (Buf.Length > 1) and (Buf.EndsWith(Quote)) then begin
        AddStr(Buf);
        Continue;
      end;
      isString := True;
      Buf := Token + ' ';
      Continue;
    end;
    if isString then begin
      Buf := Buf + IfThen(Token.IsEmpty, ' ', Token + ' ');
      if Token.TrimRight.EndsWith(Quote) then begin
        AddStr(Buf.TrimRight);
        isString := False;
      end;
    end
    else begin
      if not Token.Trim.IsEmpty then
        Tokens.Add(Token.TrimRight);
    end;
  end;
end;

function ConfirmDlg(Caption, Txt: string): TModalResult;
begin
  Result := QuestionDlg(Caption, Txt, mtConfirmation, [mrOK, 'Yes', mrCancel, 'No', 'IsDefault'], 0);
end;

procedure OKMsg(Caption, Txt: string);
begin
  MessageDlg(Caption, Txt, mtInformation, [mbOK], 0);
end;

procedure ERRMsg(Caption, Txt: string);
begin
  MessageDlg(Caption, Txt, mtError, [mbOK], 0);
end;

procedure WarnMsg(Caption, Txt: string);
begin
  MessageDlg(Caption, Txt, mtWarning, [mbOK], 0);
end;

end.

