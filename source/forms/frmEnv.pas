unit frmEnv;

{$mode objfpc}{$H+}

interface

uses
  DividerBevel, Forms,
  ButtonPanel, ExtCtrls, Grids, StdCtrls, ComCtrls, Menus,
  GridNavigator, Env, Classes;

type

  { TEnvForm }

  TEnvForm = class(TForm)
    btnSave: TButton;
    ButtonPanel: TButtonPanel;
    chkParent: TCheckBox;
    cbParent: TComboBox;
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
    procedure chkParentClick(Sender: TObject);
    procedure editNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
    function CreateMenuItem(const EnvName: string): TMenuItem;
    procedure OnSelectEnv(Sender: TObject);
    procedure SetCurrentEnv(AValue: TEnvironment);
    procedure SetEnvManager(AValue: TEnvManager);
    procedure SetParentsForEnv(const Env: TEnvironment);
    procedure PrepareEditEnv;
    procedure DisableIfEnvEmpty;
  public
    procedure ShowPanelEnv;
    procedure HidePanelEnv;
    function ShowModal(const EnvName: string): TModalResult;
    property EnvManager: TEnvManager read FEnvManager write SetEnvManager;
    property CurrentEnv: TEnvironment read FCurrentEnv write SetCurrentEnv;
  end;

implementation

uses SysUtils, Controls, AppHelpers;

{$R *.lfm}

{ TEnvForm }

procedure TEnvForm.FormCreate(Sender: TObject);
begin
  HidePanelEnv;
  DisableIfEnvEmpty;
end;

procedure TEnvForm.editNameChange(Sender: TObject);
begin
  btnSave.Enabled := not (Length(Trim(editName.Text)) = 0);
end;

procedure TEnvForm.chkParentClick(Sender: TObject);
begin
  cbParent.Enabled := chkParent.Checked;
end;

procedure TEnvForm.btnSaveClick(Sender: TObject);
var
  Env, EnvParent: TEnvironment;
  MI: TMenuItem;
begin
  EnvParent := nil;
  if chkParent.Checked and (cbParent.ItemIndex > -1) then
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
  Env: TEnvironment;
  Mi: TMenuItem;
begin
  Env := FEnvManager.Env[tbEnv.Caption];
  if ConfirmDlg('Delete', 'Are you sure you want to delete: ' + Env.Name + ' ?') = mrCancel then
    Exit; // =>

  for MI in menuEnv.Items do
    if MI.Caption = Env.Name then
    begin
      menuEnv.Items.Remove(MI);
      break;
    end;

  FEnvManager.Delete(Env.Name);
  if FOpState = opAdd then
    SetParentsForEnv(nil);
  DisableIfEnvEmpty;
end;

procedure TEnvForm.tbEditClick(Sender: TObject);
begin
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
  tbEnv.Caption := AValue.Name;
  // Something strange on RadioItem, emulate Radio by using checked.
  for MI in menuEnv.Items do
    MI.Checked := (MI.Caption = AValue.Name);
  tbEdit.Enabled := True;
  tbDelete.Enabled := True;
  if FOpState = opEdit then
    PrepareEditEnv;
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
  ParentEnabled: Boolean;
begin
  cbParent.Items.Clear;
  ParentEnabled := (FEnvManager.Count = 0);
  chkParent.Checked := ParentEnabled;
  cbParent.Enabled := ParentEnabled;
  if ParentEnabled then
    cbParent.Items.AddStrings(FEnvManager.EnvNames);
end;

procedure TEnvForm.PrepareEditEnv;
var
  EnvName: string;
  PIdx: integer;
begin
  dbEnv.Caption := 'Edit environment: ' + FCurrentEnv.Name;
  editName.Text := FCurrentEnv.Name;
  chkParent.Checked := False;
  cbParent.Items.Clear;
  for EnvName in FEnvManager.EnvNames do
    if EnvName <> FCurrentEnv.Name then
    begin
      cbParent.Items.Add(EnvName);
      if Assigned(FCurrentEnv.Parent) and (FCurrentEnv.Parent.Name = EnvName) then
        PIdx := cbParent.Items.Count - 1;
    end;
  if Assigned(FCurrentEnv.Parent) then
  begin
    chkParent.Checked := True;
    cbParent.ItemIndex := PIdx;
  end;
end;

procedure TEnvForm.DisableIfEnvEmpty;
var
  EnableSwitch: Boolean;
begin
  if FEnvManager.Count = 0 then
  begin
    EnableSwitch := False;
    tbEnv.Caption := 'No enviroments!';
    if FOpState = opEdit then
      HidePanelEnv;
    FCurrentEnv := nil;
  end
  else begin
    EnableSwitch := True;
    CurrentEnv := FEnvManager.First;
  end;
  tbEdit.Enabled := EnableSwitch;
  tbDelete.Enabled := EnableSwitch;
  tbEnv.Enabled := EnableSwitch;
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

function TEnvForm.ShowModal(const EnvName: string): TModalResult;
begin
  Result := inherited ShowModal;
end;

end.
