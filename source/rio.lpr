program rio;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  cmem,
  {$ENDIF}
  Forms, sysutils, Interfaces, cmdline, main,
  tachartlazaruspkg // Don't remove! Chart initialization.
  ;

{$R *.res}

begin
  Application.Title:='Rio';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);

  try
     HandleCommandLine;
  except
    on E: Exception do begin
      WriteLn(Format('%s: %s', [ExtractFileName(Application.ExeName), E.Message]));
      Application.Terminate;
      Exit;
    end;
  end;

  MainForm.ApplyOptions;
  Application.Run;
end.

