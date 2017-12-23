unit headers_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids, ComCtrls;

type

  { THeadersEditorForm }

  THeadersEditorForm = class(TForm)
    BtnClose: TButton;
    ImageList1: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    gridHeaders: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
  private

  public
    procedure FillHeaderValues(header: string; Buf: TStrings);
    procedure FillHeaders(Buf: TStrings);
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
  for i := 1 to gridHeaders.RowCount - 1 do
  begin
    if LowerCase(gridHeaders.Cells[0, i]) = header then Buf.Add(gridHeaders.Cells[1, i]);
  end;
end;

procedure THeadersEditorForm.FillHeaders(Buf: TStrings);
var
  i: integer;
  s: string;
begin
  Buf.Clear;
  for i := 1 to gridHeaders.RowCount - 1 do
  begin
    s := Trim(gridHeaders.Cells[0, i]);
    if Length(s) = 0 then continue;
    if Buf.IndexOf(s) = -1 then Buf.Add(s);
  end;
end;

end.

