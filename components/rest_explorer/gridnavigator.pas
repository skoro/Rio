unit GridNavigator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids;

type

  TGridChangeRowEvent = procedure (Sender: TObject; Grid: TStringGrid;
    const aRow: Integer) of object;

  TGridDeleteRowEvent = procedure (Sender: TObject; Grid: TStringGrid) of object;

  { TGridNavigator }

  TGridNavigator = class(TToolBar)
  private
    FGrid: TStringGrid;
    FBtnEdit: TToolButton;
    FBtnNew: TToolButton;
    FBtnDelete: TToolButton;
    FBtnClear: TToolButton;
    FOnNewRow: TGridChangeRowEvent;
    FOnEditRow: TGridChangeRowEvent;
    FOnDeleteRow: TGridDeleteRowEvent;
    FOnClearRows: TGridDeleteRowEvent;
    FShowNavButtons: Boolean;
    procedure OnButtonNewClick(Sender: TObject);
    procedure OnButtonEditClick(Sender: TObject);
    procedure OnButtonDeleteClick(Sender: TObject);
    procedure OnButtonClearClick(Sender: TObject);
    procedure SetShowNavButtons(AValue: Boolean);
  protected

  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetButtonsOrder;
  published
    property Grid: TStringGrid read FGrid write FGrid;
    property NewButton: TToolButton read FBtnNew write FBtnNew;
    property EditButton: TToolButton read FBtnEdit write FBtnEdit;
    property DeleteButton: TToolButton read FBtnDelete write FBtnDelete;
    property ClearButton: TToolButton read FBtnClear write FBtnClear;
    property OnNewRow: TGridChangeRowEvent read FOnNewRow write FOnNewRow;
    property OnEditRow: TGridChangeRowEvent read FOnEditRow write FOnEditRow;
    property OnDeleteRow: TGridDeleteRowEvent read FOnDeleteRow write FOnDeleteRow;
    property OnGridClear: TGridDeleteRowEvent read FOnClearRows write FOnClearRows;
    property ShowNavButtons: Boolean read FShowNavButtons write SetShowNavButtons;
  end;

procedure Register;

implementation

uses LCLType;

procedure Register;
begin
  RegisterComponents('Misc',[TGridNavigator]);
end;

{ TGridNavigator }

procedure TGridNavigator.OnButtonNewClick(Sender: TObject);
begin
  if Assigned(FGrid) then begin
    FGrid.RowCount := FGrid.RowCount + 1;
    FGrid.Row := FGrid.RowCount - 1; // Set selection to inserted row.
    if Assigned(FOnNewRow) then FOnNewRow(Self, FGrid, FGrid.RowCount - 1);
  end;
end;

procedure TGridNavigator.OnButtonEditClick(Sender: TObject);
begin
  if Assigned(FGrid) then begin
    if (FGrid.RowCount > 0) and (FGrid.RowCount <> FGrid.FixedRows) then
      if Assigned(FOnEditRow) then
        FOnEditRow(Self, FGrid, FGrid.Row);
  end;
end;

procedure TGridNavigator.OnButtonDeleteClick(Sender: TObject);
var
  Answer: TModalResult;
  ColName: string;
begin
  if Assigned(FGrid) then begin
    if (FGrid.FixedRows > 0) and (FGrid.RowCount = 1) then
      Exit; // =>
    ColName := FGrid.Cells[1, FGrid.Row]; // "Name" column.
    Answer := QuestionDlg('Delete ?', Format('Are you sure to delete "%s" ?', [ColName]), mtConfirmation, [mrOK, 'Yes', mrCancel, 'No', 'IsDefault'], 0);
    if Answer = mrCancel then
      Exit; // =>
    FGrid.DeleteRow(FGrid.Row);
    if Assigned(FOnDeleteRow) then
      FOnDeleteRow(Self, FGrid);
  end;
end;

procedure TGridNavigator.OnButtonClearClick(Sender: TObject);
var
  Answer: TModalResult;
begin
  if Assigned(FGrid) then begin
    if (FGrid.FixedRows > 0) and (FGrid.RowCount = 1) then Exit;
    Answer := QuestionDlg('Clear rows', 'Are you sure you want to clear all the rows ?', mtConfirmation, [mrOK, 'Yes', mrCancel, 'No', 'IsDefault'], 0);
    if Answer = mrCancel then
      Exit; // =>
    if FGrid.FixedRows > 0 then
      FGrid.RowCount := FGrid.FixedRows
    else
      FGrid.RowCount := 0;
    if Assigned(FOnClearRows) then FOnClearRows(Self, FGrid);
  end;
end;

procedure TGridNavigator.SetShowNavButtons(AValue: Boolean);
begin
  if FShowNavButtons = AValue then Exit;
  FShowNavButtons := AValue;
  FBtnNew.Visible := AValue;
  FBtnEdit.Visible := AValue;
  FBtnDelete.Visible := AValue;
  FBtnClear.Visible := AValue;
end;

constructor TGridNavigator.Create(TheOwner: TComponent);
begin
  inherited;

  FGrid := nil;
  FShowNavButtons := True;

  FBtnNew := TToolButton.Create(Self);
  FBtnNew.Caption := 'New';
  FBtnNew.Parent := Self;
  FBtnNew.OnClick := @OnButtonNewClick;

  FBtnEdit := TToolButton.Create(Self);
  FBtnEdit.Caption := 'Edit';
  FBtnEdit.Parent := Self;
  FBtnEdit.OnClick := @OnButtonEditClick;

  FBtnDelete := TToolButton.Create(Self);
  FBtnDelete.Caption := 'Delete';
  FBtnDelete.Parent := Self;
  FBtnDelete.OnClick := @OnButtonDeleteClick;

  FBtnClear := TToolButton.Create(Self);
  FBtnClear.Caption := 'Clear';
  FBtnClear.Parent := Self;
  FBtnClear.OnClick := @OnButtonClearClick;

  // Some defaults properties.
  ShowCaptions := True;
  EdgeBorders := [];
end;

procedure TGridNavigator.SetButtonsOrder;
var
  i: Integer;

  // http://forum.lazarus.freepascal.org/index.php?topic=22863.0
  procedure SetToolButtonIndex(AToolbutton: TToolButton; Value: Integer);
  var
    btns: array of TToolbutton;
    i, currentIdx : integer;
    toolbar: TToolbar;
  begin
    if not AToolbutton.Visible then
      exit;

    currentIdx := AToolButton.Index;
    //toolbar := TMyToolbutton(AToolButton).FToolbar;
    toolbar := Self;

    // Create a separate list of current toolbuttons, invisible ones marked by nil
    SetLength(btns, toolbar.ButtonCount);
    for i:=0 to toolbar.ButtonCount-1 do
      if toolbar.Buttons[i].Visible then
        btns[i] := toolbar.Buttons[i]
      else
        btns[i] := nil;

    // Swap "AToolButton" with the button currently at index "Value"
    btns[currentIdx] := btns[Value];
    btns[Value] := AToolButton;

    // Now, going from the last to the first button, move each visible button to
    // the left of the toolbar. This reorders the buttons in the requested order.
    for i:=Length(btns)-1 downto 0 do
      if btns[i] <> nil then
        btns[i].Left := -1;
  end;

begin
  for i := 0 to ButtonCount - 1 do begin
    SetToolButtonIndex(Buttons[i], i);
  end;
end;

end.
