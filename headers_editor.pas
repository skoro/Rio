unit headers_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids;

type

  { THeadersEditorForm }

  THeadersEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    StringGrid1: TStringGrid;
  private

  public
    procedure FillHeaderValues(header: string; Buf: TStrings);
  end;

var
  HeadersEditorForm: THeadersEditorForm;

implementation

{$R *.lfm}

{ THeadersEditorForm }

procedure THeadersEditorForm.FillHeaderValues(header: string; Buf: TStrings);
var
  i: integer;
begin
  header := LowerCase(header);
  Buf.Clear;
  for i := 1 to StringGrid1.RowCount - 1 do
  begin
    if LowerCase(StringGrid1.Cells[0, i]) = header then Buf.Add(StringGrid1.Cells[1, i]);
  end;
end;

end.

