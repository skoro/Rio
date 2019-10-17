program rio;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  cmem,
  {$ENDIF}
  LazFileUtils, Forms, sysutils, Interfaces, cmdline, main,
  tachartlazaruspkg // Don't remove! Chart initialization.
  ;

{$R *.res}

{$IFDEF DEBUGLEAKS}
var
  trcfile: string;
{$ENDIF DEBUGLEAKS}

begin
  {$IFDEF DEBUGLEAKS}
  trcfile := ExtractFilePath(Application.ExeName) + ExtractFileName(Application.ExeName) + '.trc';
  if FileExistsUTF8(trcfile) then
    DeleteFileUTF8(trcfile);
  SetHeapTraceOutput(trcfile);
  {$ENDIF DEBUGLEAKS}

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

