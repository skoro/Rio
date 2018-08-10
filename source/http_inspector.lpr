program http_inspector;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  cmem,
  {$ENDIF}
  Forms, sysutils, Classes, Interfaces, main, options;

{$R *.res}

const CRLF = #13#10;

function Usage: string;
begin
  Result := Format(
    crlf +
    '%s [options] [URL]' + crlf +
    '' + crlf +
    '-h,--help               This help.' + crlf +
    '-X,--request <command>  Custom request method: PUT, DELETE, etc.' + crlf +
    '-H,--header <header>    Extra header to include in the request.' + crlf +
    '                        You may specify any number of extra headers.' + crlf +
    '-F,--form <name=value>  Post a form data using the Content-Type application/x-www-form-urlencoded' + crlf +
    crlf,
    [ExtractFileName(Application.ExeName)]
  );
end;

procedure HandleCommandLine;
var
  ErrorMsg, Value: string;
  Values: TStringArray;
begin
  if Application.HasOption('h', 'help') then
    raise Exception.Create(Usage);
  ErrorMsg := Application.CheckOptions('X:H:F:', 'request: header: form:');
  if ErrorMsg <> '' then
    raise Exception.Create(ErrorMsg);
  if Application.HasOption('X', 'request') then begin
    Value := Application.GetOptionValue('X', 'request');
    Form1.cbMethod.Text := UpperCase(Value);
  end;
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

