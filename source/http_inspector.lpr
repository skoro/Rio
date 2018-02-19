program http_inspector;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Forms, datetimectrls, Interfaces, main, cookie_form;

{$R *.res}

begin
  Application.Title:='HTTP request inspector';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TCookieForm, CookieForm);
  Application.Run;
end.

