unit frmKeyValue;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, ExtCtrls,
  StdCtrls, ButtonPanel, ComCtrls, Grids, AppHelpers,
  main {don't delete main it's used for access to toolbar icons};

type

  TOnGetKeyValue =
    procedure(Sender: TObject; Increment: Integer; var AKey, AValue: string;
      var ACurrent, ATotal: Integer) of object;

type

  { TKeyValueForm }

  TKeyValueForm = class(TForm)
    ButtonPanel: TButtonPanel;
    cbEnabled: TCheckBox;
    editName: TEdit;
    LabelName: TLabel;
    LabelValue: TLabel;
    textValue: TMemo;
    PanelMain: TPanel;
    tbGridControl: TToolBar;
    tbNextRow: TToolButton;
    tbPrevRow: TToolButton;
    tbSaveRow: TToolButton;
    tbDeleteRow: TToolButton;
    tbInsertRow: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure OnChangeValue(Sender: TObject);
    procedure OnNextPrevRowClick(Sender: TObject);
    procedure tbDeleteRowClick(Sender: TObject);
    procedure tbInsertRowClick(Sender: TObject);
    procedure tbSaveRowClick(Sender: TObject);
  private
    FFocusedComponent: TWinControl;
    FGrid: TCustomStringGrid;
    FValEnabled: boolean; // Initial enabled value (for IsChanged func).
    FViewOnly: boolean;
    FTitle: string;
    FOnGetKeyValue: TOnGetKeyValue;

    function GetKey: string;
    function GetValEnabled: boolean;
    function GetValue: string;
    procedure SetKey(AValue: string);
    procedure SetValEnabled(AValue: boolean);
    procedure SetValue(AValue: string);
    function IsChanged: boolean;
    procedure SetViewOnly(AValue: boolean);
    procedure UpdateGridNavButtons;
    procedure SetFormTitle;
    procedure GridSaveValue;
    procedure GridConfirmSaveValue;
    procedure GridMoveRow(const step: integer);
    procedure GridActionButtons;
    procedure ExternalKeyValue(Increment: integer);
    procedure ShowView;

  public
    property Key: string read GetKey write SetKey;
    property Value: string read GetValue write SetValue;
    property ValEnabled: boolean read GetValEnabled write SetValEnabled;
    property Grid: TCustomStringGrid read FGrid write FGrid;
    property ViewOnly: boolean read FViewOnly write SetViewOnly;
    property OnGetKeyValue: TOnGetKeyValue read FOnGetKeyValue write FOnGetKeyValue;

  public
    function Edit(const AKey, AValue, ATitle: string;
      AEnabled: boolean = False;
      const FocusVal: boolean = False): TKeyValue;
    function Edit(const KV: TKeyValue; const ATitle: string;
      const FocusVal: boolean = False): TKeyValue;
    function EditGrid(const AGrid: TCustomStringGrid; const ATitle: string): TModalResult;
    procedure View(const AKey, AValue, ATitle: string);
    procedure View(const KV: TKeyValue; const ATitle: string);
    procedure ViewGrid(const AGrid: TCustomStringGrid; const ATitle: string);
    procedure ViewNav(const ATitle: string);
    function IsGridControl: Boolean;
  end;

var
  KeyValueForm: TKeyValueForm;

implementation

uses Clipbrd, strutils;

{$R *.lfm}

{ TKeyValueForm }

procedure TKeyValueForm.FormShow(Sender: TObject);
begin
  if Assigned(FFocusedComponent) then
    FFocusedComponent.SetFocus;
end;

procedure TKeyValueForm.OnChangeValue(Sender: TObject);
begin
  tbSaveRow.Enabled := True;
end;

procedure TKeyValueForm.OnNextPrevRowClick(Sender: TObject);
begin
  if Assigned(FGrid) then
  begin
    GridConfirmSaveValue;
    if Sender = tbNextRow then
      GridMoveRow(1);
    if Sender = tbPrevRow then
      GridMoveRow(-1);
  end
  else
    if Assigned(FOnGetKeyValue) then
    begin
      if Sender = tbNextRow then
        ExternalKeyValue(1)
      else
        if Sender = tbPrevRow then
          ExternalKeyValue(-1);
    end;
end;

procedure TKeyValueForm.tbDeleteRowClick(Sender: TObject);
var
  s, title: string;
begin
  s := FGrid.Cells[1, FGrid.Row];
  if s = '' then
    s := FGrid.Cells[2, FGrid.Row];
  title := '';
  if s <> '' then
    title := 'Do you want to delete: ' + s;
  if (title <> '') and (ConfirmDlg('Delete ?', title) <> mrOK) then
    Exit; // =>
  FGrid.DeleteRow(FGrid.Row);
  if FGrid.RowCount > 1 then
    GridMoveRow(0)
  else
    begin
      // Last value is deleted.
      SetKey('');
      SetValue('');
      SetValEnabled(False);
      tbNextRow.Enabled := False;
      tbPrevRow.Enabled := False;
      tbSaveRow.Enabled := False;
      tbDeleteRow.Enabled := False;
      // Append an empty row, so we can insert a new value if the user
      // is pressed OK form's button.
      FGrid.RowCount := 2;
      FGrid.Row := 1;
    end
end;

procedure TKeyValueForm.tbInsertRowClick(Sender: TObject);
begin
  GridConfirmSaveValue;
  SetKey('');
  SetValue('');
  SetValEnabled(True);
  FGrid.RowCount := FGrid.RowCount + 1;
  FGrid.Row := FGrid.RowCount - 1;
  UpdateGridNavButtons;
end;

procedure TKeyValueForm.tbSaveRowClick(Sender: TObject);
begin
  GridSaveValue;
end;

procedure TKeyValueForm.FormCreate(Sender: TObject);
begin
  FFocusedComponent := nil;
  FGrid := nil;
  FOnGetKeyValue := nil;
end;

procedure TKeyValueForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    ModalResult := mrCancel;
end;

function TKeyValueForm.GetKey: string;
begin
  Result := editName.Text;
end;

function TKeyValueForm.GetValEnabled: boolean;
begin
  Result := cbEnabled.Checked;
end;

function TKeyValueForm.GetValue: string;
begin
  Result := TrimRight(textValue.Text);
end;

procedure TKeyValueForm.SetKey(AValue: string);
begin
  if editName.Text = AValue then
    Exit;
  editName.Text := AValue;
  editName.Modified := False;
end;

procedure TKeyValueForm.SetValEnabled(AValue: boolean);
begin
  FValEnabled := AValue;
  cbEnabled.Checked := AValue;
end;

procedure TKeyValueForm.SetValue(AValue: string);
begin
  if textValue.Text = AValue then
    Exit;
  textValue.Text := AValue;
  textValue.Modified := False;
end;

function TKeyValueForm.IsChanged: boolean;
begin
  Result := textValue.Modified or editName.Modified
                               or (cbEnabled.Checked <> FValEnabled);
end;

procedure TKeyValueForm.SetViewOnly(AValue: boolean);
begin
  if FViewOnly = AValue then
    Exit; // =>
  FViewOnly := AValue;
  editName.ReadOnly := AValue;
  textValue.ReadOnly := AValue;
  cbEnabled.Enabled := not AValue;
  if Assigned(FGrid) then
    GridActionButtons;
end;

procedure TKeyValueForm.UpdateGridNavButtons;
begin
  if not Assigned(FGrid) then
    Exit; // =>
  tbNextRow.Enabled := True;
  tbPrevRow.Enabled := True;
  if FGrid.Row <= 1 then
    tbPrevRow.Enabled := False;
  if FGrid.Row >= FGrid.RowCount - 1 then
    tbNextRow.Enabled := False;
  GridActionButtons;
  SetFormTitle;
end;

procedure TKeyValueForm.SetFormTitle;
begin
  Caption := FTitle;
  if GetKey <> '' then
    Caption := Caption + ': ' + GetKey;
end;

procedure TKeyValueForm.GridSaveValue;
begin
  if not Assigned(FGrid) then
    Exit; // =>
  with FGrid do
  begin
    Cells[0, Row] := IfThen(ValEnabled, '1', '0');
    Cells[1, Row] := Key;
    Cells[2, Row] := Value;
  end;
  FValEnabled := cbEnabled.Checked;
  editName.Modified := False;
  textValue.Modified := False;
  tbSaveRow.Enabled := False;
  tbDeleteRow.Enabled := True;
end;

procedure TKeyValueForm.GridConfirmSaveValue;
begin
  if IsChanged and (ConfirmDlg('Confirm', 'Value has been changed. Do you want to save it ?') = mrOK) then
    GridSaveValue;
end;

procedure TKeyValueForm.GridMoveRow(const step: integer);
var
  KV: TKeyValue;
begin
  FGrid.Row := FGrid.Row + Step;
  KV := GetRowKV(FGrid);
  SetKey(KV.Key);
  SetValue(KV.Value);
  SetValEnabled(IsRowEnabled(FGrid));
  UpdateGridNavButtons;
  SetFormTitle;
end;

procedure TKeyValueForm.GridActionButtons;
begin
  if ViewOnly then
  begin
    tbSaveRow.Visible := False;
    tbDeleteRow.Visible := False;
    tbInsertRow.Visible := False;
  end
  else begin
    tbSaveRow.Visible := True;
    tbSaveRow.Enabled := IsChanged;
    tbDeleteRow.Visible := True;
    tbInsertRow.Visible := True;
  end;
end;

procedure TKeyValueForm.ExternalKeyValue(Increment: integer);
var
  AKey, AValue: string;
  ACurrent, ATotal: integer;
begin
  FOnGetKeyValue(Self, Increment, AKey, AValue, ACurrent, ATotal);
  SetKey(AKey);
  SetValue(AValue);
  SetFormTitle;
  if ATotal = 1 then
  begin
    tbNextRow.Enabled := False;
    tbPrevRow.Enabled := False;
  end
  else
    if (ACurrent > 0) and (ACurrent < (ATotal - 1)) then
    begin
      tbNextRow.Enabled := True;
      tbPrevRow.Enabled := True;
    end
    else
      if ACurrent <= 0 then
      begin
        tbNextRow.Enabled := True;
        tbPrevRow.Enabled := False;
      end
      else
        if ACurrent >= ATotal - 1 then
        begin
          tbNextRow.Enabled := False;
          tbPrevRow.Enabled := True;
        end;
end;

procedure TKeyValueForm.ShowView;
begin
  ViewOnly := True;
  cbEnabled.Visible := False;
  ButtonPanel.OKButton.Caption := 'C&opy and Close';
  FFocusedComponent := textValue;
  UpdateGridNavButtons;
  if ShowModal = mrOk then
  begin
    Clipboard.AsText := textValue.Text;
  end;
  FGrid := nil;
end;

function TKeyValueForm.Edit(const AKey, AValue, ATitle: string;
  AEnabled: boolean = False; const FocusVal: boolean = False): TKeyValue;
begin
  tbGridControl.Visible := IsGridControl;
  FTitle := ATitle;
  SetKey(AKey);
  SetValue(AValue);
  SetFormTitle;
  ViewOnly := False;
  cbEnabled.Visible := True;
  SetValEnabled(AEnabled);
  ButtonPanel.OKButton.Caption := '&OK';
  if FocusVal then
    FFocusedComponent := textValue
  else
    FFocusedComponent := editName;
  UpdateGridNavButtons;
  if ShowModal = mrOk then
  begin
    Result.Key := GetKey;
    Result.Value := GetValue;
    Result.Enabled := GetValEnabled;
    // Fix #146: don't allow to add empty rows.
    if (Trim(AKey) = '') and (Trim(AValue) = '') and
      (Trim(Result.Key) = '') and (Trim(Result.Value) = '') then
      ModalResult := mrCancel;
    // When a grid is attached save the changed value when closing the form.
    if (ModalResult = mrOK) and Assigned(FGrid) and IsChanged then
      GridSaveValue;
  end
  else
  begin
    Result.Key := akey;
    Result.Value := avalue;
  end;
  FGrid := nil;
end;

function TKeyValueForm.Edit(const KV: TKeyValue; const ATitle: string; const FocusVal: boolean): TKeyValue;
begin
  Result := Edit(KV.Key, KV.Value, ATitle, KV.Enabled, FocusVal);
end;

function TKeyValueForm.EditGrid(const AGrid: TCustomStringGrid; const ATitle: string): TModalResult;
begin
  FGrid := AGrid;
  Edit(GetRowKV(AGrid), ATitle, AGrid.Col = 2);
  Result := ModalResult;
end;

procedure TKeyValueForm.View(const AKey, AValue, ATitle: string);
begin
  tbGridControl.Visible := IsGridControl;
  FTitle := ATitle;
  SetKey(AKey);
  SetValue(AValue);
  SetFormTitle;
  ShowView;
end;

procedure TKeyValueForm.View(const KV: TKeyValue; const ATitle: string);
begin
  View(KV.Key, KV.Value, ATitle);
end;

procedure TKeyValueForm.ViewGrid(const AGrid: TCustomStringGrid; const ATitle: string);
begin
  FGrid := AGrid;
  View(GetRowKV(AGrid), ATitle);
end;

procedure TKeyValueForm.ViewNav(const ATitle: string);
begin
  if Assigned(FGrid) then
    raise Exception.Create('Cannot use this method with Grid property.');
  if not Assigned(FOnGetKeyValue) then
    raise Exception.Create('Invoke this method only with OnGetKeyValue properties.');
  tbGridControl.Visible := True;
  FTitle := ATitle;
  ViewOnly := True; // Initialize earlier for GridActionButtons.
  GridActionButtons;
  ExternalKeyValue(0);
  ShowView;
end;

function TKeyValueForm.IsGridControl: Boolean;
begin
  Result := (Assigned(FGrid) or Assigned(FOnGetKeyValue));
end;

end.
