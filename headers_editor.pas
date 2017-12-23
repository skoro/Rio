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
    btnAddRow: TToolButton;
    btnRemoveRow: TToolButton;
    ToolButton3: TToolButton;
    btnMoveUp: TToolButton;
    btnMoveDown: TToolButton;
    procedure btnAddRowClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnRemoveRowClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

procedure THeadersEditorForm.btnAddRowClick(Sender: TObject);
begin
  with gridHeaders do
    InsertRowWithValues(RowCount, ['', '']);
end;

procedure THeadersEditorForm.btnMoveDownClick(Sender: TObject);
begin
  with gridHeaders do
    if Row < RowCount - 1 then MoveColRow(False, Row, Row + 1);
end;

procedure THeadersEditorForm.btnMoveUpClick(Sender: TObject);
begin
  with gridHeaders do
    if Row > 1 then MoveColRow(False, Row, Row - 1);
end;

procedure THeadersEditorForm.btnRemoveRowClick(Sender: TObject);
begin
  with gridHeaders do
    DeleteRow(Row);
end;

procedure THeadersEditorForm.FormShow(Sender: TObject);
begin
  gridHeaders.SetFocus;
end;

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

