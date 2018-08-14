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
    crlf,
    [ExtractFileName(Application.ExeName)]
  );
end;

procedure HandleCommandLine;
var
  ErrorMsg, ReqMethod: string;
  I: Integer;
  Values: TStringArray;
  KV: TKeyValuePair;
begin
  if Application.HasOption('h', 'help') then
    raise Exception.Create(Usage);
  ErrorMsg := Application.CheckOptions('X:H:F:C:', 'request: header: form: cookie:');
  if ErrorMsg <> '' then
    raise Exception.Create(ErrorMsg);
  ReqMethod := '';
  if Application.HasOption('X', 'request') then
    ReqMethod := Application.GetOptionValue('X', 'request');
  if Application.HasOption('H', 'header') then begin
    Values := Application.GetOptionValues('H', 'header');
    for I := Length(Values) - 1 downto 0 do begin
      KV := SplitKV(Values[I], ':');
      Form1.AddRequestHeader(KV.Key, KV.Value);
    end;
  end;
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
  if Application.HasOption('C', 'cookie') then begin
    Values := Application.GetOptionValues('C', 'cookie');
    for I := Length(Values) - 1 downto 0 do
      Form1.SetRowKV(Form1.gridReqCookie, SplitKV(Values[I], '='));
  end;
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

