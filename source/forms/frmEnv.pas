unit frmEnv;

{$mode objfpc}{$H+}

interface

uses
  DividerBevel, Forms,
  ButtonPanel, ExtCtrls, Grids, StdCtrls, ComCtrls, Menus,
  GridNavigator, Env, Controls;

type

  { TEnvForm }

  TEnvForm = class(TForm)
    btnSave: TButton;
    ButtonPanel: TButtonPanel;
    cbParent: TComboBox;
    labParent: TLabel;
    dbVars: TDividerBevel;
    dbEnv: TDividerBevel;
    editName: TEdit;
    navVars: TGridNavigator;
    labName: TLabel;
    panEnv: TPanel;
    panName: TPanel;
    panVars: TPanel;
    panOpts: TPanel;
    menuEnv: TPopupMenu;
    gridVars: TStringGrid;
    TBControls: TToolBar;
    tbAdd: TToolButton;
    tbEdit: TToolButton;
    tbDelete: TToolButton;
    tbEnv: TToolButton;
    procedure btnSaveClick(Sender: TObject);
    procedure editNameChange(Sender: TObject);
    procedure editNameKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gridVarsEditingDone(Sender: TObject);
    procedure navVarsDeleteRow(Sender: TObject; Grid: TStringGrid);
    procedure navVarsGridClear(Sender: TObject; Grid: TStringGrid);
    procedure tbAddClick(Sender: TObject);
    procedure tbDeleteClick(Sender: TObject);
    procedure tbEditClick(Sender: TObject);
  private
    type
    TOpState = (opNone, opAdd, opEdit);
  private
    FOpState: TOpState;
    FEnvManager: TEnvManager;
    FCurrentEnv: TEnvironment;
    FCurrentMenu: TMenuItem;
    function CreateMenuItem(const EnvName: string): TMenuItem;
    procedure OnSelectEnv(Sender: TObject);
    procedure SetCurrentEnv(AValue: TEnvironment);
    procedure SetEnvManager(AValue: TEnvManager);
    procedure SetParentsForEnv(const Env: TEnvironment);
    procedure PrepareEditEnv;
    procedure DisableIfEnvEmpty;
    procedure FillEnvVars;
  public
    procedure ShowPanelEnv;
    procedure HidePanelEnv;
    function ShowModal(const Env: TEnvironment): TModalResult; overload;
    property EnvManager: TEnvManager read FEnvManager write SetEnvManager;
    property CurrentEnv: TEnvironment read FCurrentEnv write SetCurrentEnv;
  end;

implementation

uses SysUtils, Graphics, AppHelpers;

{$R *.lfm}

{ TEnvForm }

procedure TEnvForm.FormCreate(Sender: TObject);
begin
  tbEnv.Font.Style := [fsBold];
  navVars.NavButtons := [nbNew, nbDelete, nbClear];
  FCurrentEnv := nil;
end;

procedure TEnvForm.FormShow(Sender: TObject);
begin
  HidePanelEnv;
  DisableIfEnvEmpty;
end;

procedure TEnvForm.gridVarsEditingDone(Sender: TObject);
var
  R: integer;
  VarName, VarVal: string;
  V: TVariable;
begin
  if not Assigned(FCurrentEnv) then
    Exit; // =>
  R := gridVars.Row;
  VarName := gridVars.Cells[0, R];
  if VarName = '' then
    Exit; // =>
  VarVal := gridVars.Cells[1, R];
  V := FCurrentEnv.FindVar(VarName);
  if Assigned(V) then
     V.Value := VarVal
  else
     FCurrentEnv.Add(VarName, VarVal);
end;

procedure TEnvForm.navVarsDeleteRow(Sender: TObject; Grid: TStringGrid);
var
  VarName: string;
begin
  if not Assigned(FCurrentEnv) then
    Exit; // =>
  VarName := Grid.Cells[0, Grid.Row];
  if VarName = '' then
    Exit; // =>
  FCurrentEnv.DeleteVar(VarName);
end;

procedure TEnvForm.navVarsGridClear(Sender: TObject; Grid: TStringGrid);
begin
  if Assigned(FCurrentEnv) then
    FCurrentEnv.DeleteAllVars;
end;

procedure TEnvForm.editNameChange(Sender: TObject);
begin
  btnSave.Enabled := not (Length(Trim(editName.Text)) = 0);
end;

procedure TEnvForm.editNameKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    #27: HidePanelEnv;
    #13: if btnSave.Enabled then
            btnSaveClick(Sender);
  end;
end;

procedure TEnvForm.btnSaveClick(Sender: TObject);
var
  Env, EnvParent: TEnvironment;
  MI: TMenuItem;
begin
  EnvParent := nil;
  if cbParent.ItemIndex > 0 then
    EnvParent := FEnvManager.Env[cbParent.Items[cbParent.ItemIndex]];

  case FOpState of
    opAdd:
    begin
      Env := TEnvironment.Create(editName.Text, EnvParent);
      try
        FEnvManager.Add(Env);
        MI := CreateMenuItem(Env.Name);
        OnSelectEnv(MI); // Set selection on the current menu.
        tbEnv.Caption := Env.Name;
      except
        on E: Exception do
        begin
          ERRMsg('Error', E.Message);
          FreeAndNil(Env);
        end;
      end;
    end;

    opEdit:
    begin
      try
        if (editName.Text <> FCurrentEnv.Name) then
        begin
          FEnvManager.Rename(FCurrentEnv, editName.Text);
          FCurrentMenu.Caption := FCurrentEnv.Name;
          tbEnv.Caption := FCurrentEnv.Name;
        end;
        FCurrentEnv.Parent := EnvParent;
        FillEnvVars;
      except
        on E: Exception do
        begin
          ERRMsg('Error', E.Message);
          Exit; // =>
        end;
      end;
    end;
  end; // case

  HidePanelEnv;
end;

procedure TEnvForm.tbAddClick(Sender: TObject);
begin
  if panEnv.Visible and (FOpState = opAdd) then
  begin
    HidePanelEnv;
    Exit; // =>
  end;
  dbEnv.Caption := 'New environment';
  editName.Text := '';
  SetParentsForEnv(nil);
  btnSave.Enabled := False;
  ShowPanelEnv;
  FOpState := opAdd;
end;

procedure TEnvForm.tbDeleteClick(Sender: TObject);
var
  Mi: TMenuItem;
begin
  if not Assigned(FCurrentEnv) then
    Exit; // When invoked via shortcut =>
  if ConfirmDlg('Delete', 'Are you sure you want to delete: ' + FCurrentEnv.Name + ' ?') = mrCancel then
    Exit; // =>

  MI := menuEnv.Items.Find(FCurrentEnv.Name);
  if Assigned(MI) then
    menuEnv.Items.Remove(MI);

  FEnvManager.Delete(FCurrentEnv.Name);
  FCurrentEnv := nil;
  if FOpState = opAdd then
    SetParentsForEnv(nil);
  DisableIfEnvEmpty;
end;

procedure TEnvForm.tbEditClick(Sender: TObject);
begin
  if not Assigned(FCurrentEnv) then
    Exit; // When invoked via shortcut =>
  if PanEnv.Visible and (FOpState = opEdit) then
  begin
    HidePanelEnv;
    Exit; // =>
  end;
  PrepareEditEnv;
  ShowPanelEnv;
  FOpState := opEdit;
end;

function TEnvForm.CreateMenuItem(const EnvName: string): TMenuItem;
begin
  Result := TMenuItem.Create(menuEnv);
  Result.Caption := EnvName;
  Result.OnClick := @onSelectEnv;
  menuEnv.Items.Add(Result);
end;

procedure TEnvForm.OnSelectEnv(Sender: TObject);
begin
  if (Sender is TMenuItem) then
    SetCurrentEnv(FEnvManager.EnvIndex[TMenuItem(Sender).MenuIndex]);
end;

procedure TEnvForm.SetCurrentEnv(AValue: TEnvironment);
var
  MI: TMenuItem;
begin
  if FCurrentEnv = AValue then
    Exit; // =>
  FCurrentEnv := AValue;
  tbEnv.Enabled := True;
  tbEnv.Caption := AValue.Name;
  FCurrentMenu := nil;
  // Something strange on RadioItem, emulate Radio by using checked.
  for MI in menuEnv.Items do
  begin
    MI.Checked := (MI.Caption = AValue.Name);
    if MI.Caption = AValue.Name then
      FCurrentMenu := MI;
  end;
  tbEdit.Enabled := True;
  tbDelete.Enabled := True;
  if FOpState = opEdit then
    PrepareEditEnv;
  FillEnvVars;
end;

procedure TEnvForm.SetEnvManager(AValue: TEnvManager);
var
  EnvName: string;
begin
  if FEnvManager = AValue then
    Exit; // =>
  menuEnv.Items.Clear;
  for EnvName in AValue.EnvNames do
    CreateMenuItem(EnvName);
  FEnvManager := AValue;
end;

procedure TEnvForm.SetParentsForEnv(const Env: TEnvironment);
var
  EnvList: TEnvList;
  Iter: TEnvironment;
begin
  cbParent.Items.Clear;
  cbParent.Items.Add(' '); // No Parent choice.
  EnvList := FEnvManager.FindAvailParents(Env);
  for Iter in EnvList do
  begin
    cbParent.Items.Add(Iter.Name);
    if (Env <> nil) and (Env.Parent = Iter) then
      cbParent.ItemIndex := cbParent.Items.Count - 1;
  end;
  FreeAndNil(EnvList);
  cbParent.Enabled := (cbParent.Items.Count > 1);
end;

procedure TEnvForm.PrepareEditEnv;
begin
  dbEnv.Caption := 'Edit environment: ' + FCurrentEnv.Name;
  editName.Text := FCurrentEnv.Name;
  SetParentsForEnv(FCurrentEnv);
end;

procedure TEnvForm.DisableIfEnvEmpty;
var
  EnableSwitch: Boolean;
begin
  if FEnvManager.Count = 0 then
  begin
    EnableSwitch := False;
    tbEnv.Caption := 'No environments!';
    if FOpState = opEdit then
      HidePanelEnv;
    FCurrentEnv := nil;
    gridVars.RowCount := 1;
  end
  else begin
    EnableSwitch := True;
    if not Assigned(FCurrentEnv) then
      CurrentEnv := FEnvManager.First;
  end;
  tbEdit.Enabled := EnableSwitch;
  tbDelete.Enabled := EnableSwitch;
  tbEnv.Enabled := EnableSwitch;
end;

procedure TEnvForm.FillEnvVars;
var
  VL: TVarList;
  R: integer;
begin
  VL := FCurrentEnv.Vars;
  gridVars.Clear;
  gridVars.RowCount := VL.Count + 1;
  try
    for R := 0 to VL.Count - 1 do
    begin
      gridVars.Cells[0, R + 1] := VL[R].Name;
      gridVars.Cells[1, R + 1] := VL[R].Value;
    end;
  finally
    FreeAndNil(VL);
  end;
end;

procedure TEnvForm.ShowPanelEnv;
begin
  PanEnv.Visible := True;
  editName.SetFocus;
end;

procedure TEnvForm.HidePanelEnv;
begin
  PanEnv.Visible := False;
  FOpState := opNone;
end;

function TEnvForm.ShowModal(const Env: TEnvironment): TModalResult;
begin
  if Assigned(Env) then
    CurrentEnv := Env;
  Result := inherited ShowModal;
end;

end.
