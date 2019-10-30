unit frmEnv;

{$mode objfpc}{$H+}

interface

uses
  DividerBevel, Forms,
  ButtonPanel, ExtCtrls, Grids, StdCtrls, ComCtrls, Menus,
  GridNavigator, Env;

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
    procedure tbEditClick(Sender: TObject);
  private
    type
    TOpState = (opNone, opAdd, opEdit);
  private
    FOpState: TOpState;
    FEnvManager: TEnvManager;
    function CreateMenuItem(const EnvName: string): TMenuItem;
    procedure OnSelectEnv(Sender: TObject);
    procedure SetEnvManager(AValue: TEnvManager);
    procedure PrepareEditEnv;
  public
    procedure ShowEnv;
    procedure HideEnv;
    function ShowModal(const EnvName: string): TModalResult;
    property EnvManager: TEnvManager read FEnvManager write SetEnvManager;
  end;

implementation

uses SysUtils, AppHelpers;

{$R *.lfm}

{ TEnvForm }

procedure TEnvForm.FormCreate(Sender: TObject);
begin
  HideEnv;
  tbEnv.Caption := '------';
  tbEdit.Enabled := False;
  tbDelete.Enabled := False;
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

  HideEnv;
end;

procedure TEnvForm.tbAddClick(Sender: TObject);
begin
  if panEnv.Visible and (FOpState = opAdd) then
  begin
    HideEnv;
    Exit; // =>
  end;
  dbEnv.Caption := 'New environment';
  editName.Text := '';
  chkParent.Checked := False;
  cbParent.Enabled := False;
  cbParent.Items.AddStrings(FEnvManager.EnvNames, True);
  btnSave.Enabled := False;
  ShowEnv;
  FOpState := opAdd;
end;

procedure TEnvForm.tbEditClick(Sender: TObject);
begin
  if PanEnv.Visible and (FOpState = opEdit) then
  begin
    HideEnv;
    Exit; // =>
  end;
  PrepareEditEnv;
  ShowEnv;
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
var
  Sel, MI: TMenuItem;
begin
  if (Sender is TMenuItem) then
  begin
    Sel := TMenuItem(Sender);
    tbEnv.Caption := Sel.Caption;
    // Something strange on RadioItem, emulate Radio by using checked.
    for MI in menuEnv.Items do
      MI.Checked := False;
    Sel.Checked := True;
    tbEdit.Enabled := True;
    tbDelete.Enabled := True;
    if FOpState = opEdit then
      PrepareEditEnv;
  end;
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

procedure TEnvForm.PrepareEditEnv;
var
  Env: TEnvironment;
  EnvName: string;
  PIdx: integer;
begin
  Env := FEnvManager.Env[tbEnv.Caption];
  dbEnv.Caption := 'Edit environment: ' + Env.Name;
  editName.Text := Env.Name;
  chkParent.Checked := False;
  cbParent.Items.Clear;
  for EnvName in FEnvManager.EnvNames do
    if EnvName <> Env.Name then
    begin
      cbParent.Items.Add(EnvName);
      if Assigned(Env.Parent) and (Env.Parent.Name = EnvName) then
        PIdx := cbParent.Items.Count - 1;
    end;
  if Assigned(Env.Parent) then
  begin
    chkParent.Checked := True;
    cbParent.ItemIndex := PIdx;
  end;
end;

procedure TEnvForm.ShowEnv;
begin
  PanEnv.Visible := True;
  editName.SetFocus;
end;

procedure TEnvForm.HideEnv;
begin
  PanEnv.Visible := False;
  FOpState := opNone;
end;

function TEnvForm.ShowModal(const EnvName: string): TModalResult;
begin
  Result := inherited ShowModal;
end;

end.
