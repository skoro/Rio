unit headers_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,
  ExtCtrls, Grids, ComCtrls, JSONPropStorage;

type

  { THeadersEditorForm }

  THeadersEditorForm = class(TForm)
    BtnClose: TButton;
    ImageList1: TImageList;
    Props: TJSONPropStorage;
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
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure PropsRestoringProperties(Sender: TObject);
    procedure PropsSavingProperties(Sender: TObject);
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

procedure THeadersEditorForm.FormCreate(Sender: TObject);
var
  C: string;
begin
  C := GetAppConfigDir(False);
  if DirectoryExists(C) then
    begin
      C := C + DirectorySeparator + 'HeadersEditor' + ConfigExtension;
      Props.JSONFileName := C;
      Props.Active := True;
    end;
end;

procedure THeadersEditorForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  oldRow: Integer;
begin
  if Shift = [ssCtrl] then
    case Key of
      // Control-W
      87: Close;
      // Control-D
      68: btnRemoveRowClick(Sender);
      // Control-Up
      38: begin
        oldRow := gridHeaders.Row; // restore selected row after moving.
        btnMoveUpClick(Sender);
        gridHeaders.Row := oldRow;
      end;
      // Control-Down
      40: begin
        oldRow := gridHeaders.Row;
        btnMoveDownClick(Sender);
        gridHeaders.Row := oldRow;
      end;
    end;
end;

procedure THeadersEditorForm.FormShow(Sender: TObject);
begin
  gridHeaders.SetFocus;
end;

procedure THeadersEditorForm.PropsRestoringProperties(Sender: TObject);
var
  S: TStringList;
  i, p: integer;
  h: string;
begin
  S := TStringList.Create;
  try
    Props.ReadStrings('Rows', S);
    gridHeaders.RowCount := S.Count + 1;
    for i := 0 to S.Count - 1 do
    begin
      p := Pos(':', S.Strings[i]);
      h := LeftStr(S.Strings[i], p - 1); // minus colon
      gridHeaders.Cells[0, i + 1] := h;
      gridHeaders.Cells[1, i + 1] := RightStr(S.Strings[i], Length(S.Strings[i]) - p);
    end;
    p := Props.ReadInteger('Width_Col1', 0);
    if p <> 0 then gridHeaders.Columns.Items[0].Width := p;
    p := Props.ReadInteger('Width_Col2', 0);
    if p <> 0 then gridHeaders.Columns.Items[1].Width := p;
  finally
    FreeAndNil(S);
  end;
end;

procedure THeadersEditorForm.PropsSavingProperties(Sender: TObject);
var
  S: TStringList;
  i: Integer;
  h: string;
begin
  Props.WriteInteger('Width_Col1', gridHeaders.Columns.Items[0].Width);
  Props.WriteInteger('Width_Col2', gridHeaders.Columns.Items[1].Width);
  S := TStringList.Create;
  for i := 1 to gridHeaders.RowCount - 1 do
  begin
    h := trim(gridHeaders.Cells[0, i]);
    if h <> '' then S.Add(h + ':' + trim(gridHeaders.Cells[1, i]));
  end;
  Props.WriteStrings('Rows', S);
  FreeAndNil(S);
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

