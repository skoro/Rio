unit frmKeyValue;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, ExtCtrls,
  StdCtrls, ButtonPanel, ComCtrls, Grids, Classes;

type

  { TKeyValue }

  TKeyValue = record
    Key: string;
    Value: string;
    Enabled: boolean;
  end;

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
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure OnChangeValue(Sender: TObject);
    procedure OnNextPrevRowClick(Sender: TObject);
    procedure tbSaveRowClick(Sender: TObject);
  private
    FFocusedComponent: TWinControl;
    FGrid: TCustomStringGrid;
    FValEnabled: boolean; // Initial enabled value (for IsChanged func).
    FViewOnly: boolean;

    function GetKey: string;
    function GetValEnabled: boolean;
    function GetValue: string;
    procedure SetKey(AValue: string);
    procedure SetValEnabled(AValue: boolean);
    procedure SetValue(AValue: string);
    function IsChanged: boolean;
    procedure SetViewOnly(AValue: boolean);
    procedure UpdateGridNavButtons;
    procedure GridSaveValue;

  public
    property Key: string read GetKey write SetKey;
    property Value: string read GetValue write SetValue;
    property ValEnabled: boolean read GetValEnabled write SetValEnabled;
    property Grid: TCustomStringGrid read FGrid;
    property ViewOnly: boolean read FViewOnly write SetViewOnly;
    function Edit(const AKey, AValue, title: string;
      AEnabled: boolean = False;
      const FocusVal: boolean = False): TKeyValue;
    function Edit(const KV: TKeyValue; const title: string;
      const FocusVal: boolean = False): TKeyValue;
    function EditGrid(const AGrid: TCustomStringGrid): TModalResult;
    procedure View(const AKey, AValue, Title: string);
    procedure View(const KV: TKeyValue; const title: string);
  end;

var
  KeyValueForm: TKeyValueForm;

implementation

uses Clipbrd, strutils, AppHelpers;

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
var
  KV: TKeyValue;
begin
  if not Assigned(FGrid) then
    Exit; // =>
  if IsChanged and (ConfirmDlg('Confirm', 'Value has been changed. Do you want to save it ?') = mrOK) then
    GridSaveValue;
  if Sender = tbNextRow then
    FGrid.Row := FGrid.Row + 1;
  if Sender = tbPrevRow then
    FGrid.Row := FGrid.Row - 1;
  KV := GetRowKV(FGrid);
  SetKey(KV.Key);
  SetValue(KV.Value);
  SetValEnabled(IsRowEnabled(FGrid));
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
  begin
    tbSaveRow.Enabled := not AValue;
  end;
end;

procedure TKeyValueForm.UpdateGridNavButtons;
begin
  tbNextRow.Enabled := True;
  tbPrevRow.Enabled := True;
  if FGrid.Row <= 1 then
    tbPrevRow.Enabled := False;
  if FGrid.Row >= FGrid.RowCount - 1 then
    tbNextRow.Enabled := False;
  tbSaveRow.Enabled := IsChanged;
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
end;

function TKeyValueForm.Edit(const AKey, AValue, title: string;
  AEnabled: boolean = False; const FocusVal: boolean = False): TKeyValue;
begin
  tbGridControl.Visible := Assigned(FGrid);
  SetKey(AKey);
  SetValue(AValue);
  ViewOnly := False;
  Caption := title;
  cbEnabled.Visible := True;
  SetValEnabled(AEnabled);
  ButtonPanel.OKButton.Caption := '&OK';
  if FocusVal then
    FFocusedComponent := textValue
  else
    FFocusedComponent := editName;
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
end;

function TKeyValueForm.Edit(const KV: TKeyValue; const title: string;
  const FocusVal: boolean): TKeyValue;
begin
  Result := Edit(KV.Key, KV.Value, title, KV.Enabled, FocusVal);
end;

function TKeyValueForm.EditGrid(const AGrid: TCustomStringGrid): TModalResult;
begin
  FGrid := AGrid;
  UpdateGridNavButtons;
  Edit(GetRowKV(AGrid), '', AGrid.Col = 2);
  Result := ModalResult;
end;

procedure TKeyValueForm.View(const AKey, AValue, Title: string);
begin
  SetKey(AKey);
  SetValue(AValue);
  ViewOnly := True;
  cbEnabled.Visible := False;
  Caption := title;
  ButtonPanel.OKButton.Caption := 'C&opy and Close';
  FFocusedComponent := textValue;
  if ShowModal = mrOk then
  begin
    Clipboard.AsText := textValue.Text;
  end;
end;

procedure TKeyValueForm.View(const KV: TKeyValue; const title: string);
begin
  View(KV.Key, KV.Value, title);
end;

end.
