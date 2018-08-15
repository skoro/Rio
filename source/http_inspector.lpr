program http_inspector;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  cmem,
  {$ENDIF}
  Forms, ValEdit, sysutils, Classes, Interfaces, main, options, app_helpers;

{$R *.res}

const CRLF = #13#10;

function Usage: string;
begin
  Result := Format(
    crlf +
    '%s [options] [URL]' + crlf +
    '' + crlf +
    '-h,--help' + crlf +
    '    This help.' + crlf +
    '-X,--request=<command>' + crlf +
    '    Custom request method: PUT, DELETE, etc.' + crlf +
    '-H,--header=<name:value>' + crlf +
    '    Extra header to include in the request.' + crlf +
    '    You may specify any number of extra headers.' + crlf +
    '-F,--form=<name=value>' + crlf +
    '    Post a form data using the Content-Type application/x-www-form-urlencoded.' + crlf +
    '    This implies that it will be a POST request but can be overrided' + crlf +
    '    by -X option. When a value prefixed by @ then it treated as file upload.' + crlf +
    '-C,--cookie=<name=value>' + crlf +
    '    Include a cookie to the request.' + crlf +
    '-d,--data=<data>' + crlf +
    '    Send the specified data in a POST request (may be overrided by -X option).' + crlf +
    '    Compare to the -F option you will need to specify Content-Type header.' + crlf +
    '    Data from this option will appear in "Other" view of "Body" tab.' + crlf +
    '-j,--json=<json>' + crlf +
    '    Like -d but appends Content-Type: application/json header.' + crlf +
    '    Data from this option will appear in "Json" view of "Body" tab.' + crlf +
    '-f,--file=<file>' + crlf +
    '    Open request data from the specified file.' + crlf +
    crlf,
    [ExtractFileName(Application.ExeName)]
  );
end;

procedure HandleCommandLine;
const
  LongOpts: array [1..7] of string = ('request:', 'header:', 'form:',
            'cookie:', 'data:', 'json:', 'file:'
  );
  ShortOpts: string = 'X:H:F:C:d:j:f:';
var
  ErrorMsg, ReqMethod, Value, FileName: string;
  I: Integer;
  Values: TStringArray;
  KV: TKeyValuePair;
begin
  if Application.HasOption('h', 'help') then
    raise Exception.Create(Usage);
  ErrorMsg := Application.CheckOptions(ShortOpts, LongOpts);
  if ErrorMsg <> '' then
    raise Exception.Create(ErrorMsg);
  ReqMethod := '';
  // A request method.
  if Application.HasOption('X', 'request') then
    ReqMethod := Application.GetOptionValue('X', 'request');
  // Request headers.
  if Application.HasOption('H', 'header') then begin
    Values := Application.GetOptionValues('H', 'header');
    for I := Length(Values) - 1 downto 0 do begin
      KV := SplitKV(Values[I], ':');
      Form1.AddRequestHeader(KV.Key, KV.Value);
    end;
  end;
  // Form data.
  if Application.HasOption('F', 'form') then begin
    Values := Application.GetOptionValues('F', 'form');
    if ReqMethod = '' then
      ReqMethod := 'POST';
    for I := Length(Values) - 1 downto 0 do begin
      KV := SplitKV(Values[I], '=');
      if LeftStr(KV.Value, 1) = '@' then
        Form1.AddFormData(KV.Key, RightStr(KV.Value, Length(KV.Value) - 1), True)
      else
        Form1.AddFormData(KV.Key, KV.Value);
    end;
  end;
  // Cookies.
  if Application.HasOption('C', 'cookie') then begin
    Values := Application.GetOptionValues('C', 'cookie');
    for I := Length(Values) - 1 downto 0 do
      Form1.SetRowKV(Form1.gridReqCookie, SplitKV(Values[I], '='));
  end;
  // Body data.
  if Application.HasOption('d', 'data') then
    Form1.editOther.Text := Application.GetOptionValue('d', 'data');
  // Json data.
  if Application.HasOption('j', 'json') then
    Form1.editJson.Text := Application.GetOptionValue('j', 'json');
  // Open request file.
  if Application.HasOption('f', 'file') then begin
    FileName := Application.GetOptionValue('f', 'file');
    if not FileGetContents(FileName, Value) then
      raise Exception.Create('cannot read file: ' + FileName);
    Form1.OpenRequestFile(Value);
  end;
  // Non options values are for url but we need only one url so use
  // the first value.
  Values := Application.GetNonOptions(ShortOpts, LongOpts);
  if Length(Values) > 0 then
    Form1.cbUrl.Text := Values[0];
  if ReqMethod <> '' then
    Form1.cbMethod.Text := UpperCase(ReqMethod);
end;

begin
  Application.Title := 'HTTP request inspector';
  RequireDerivedFormResource := True;
  Application.Initialize;

  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TOptionsForm, OptionsForm);

  try
     HandleCommandLine;
  except
    on E: Exception do begin
      WriteLn(Format('%s: %s', [ExtractFileName(Application.ExeName), E.Message]));
      Application.Terminate;
      Exit;
    end;
  end;

  Form1.ApplyOptions;

  Application.Run;
end.

