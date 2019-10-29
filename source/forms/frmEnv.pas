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
  public
    procedure ShowEnv;
    procedure HideEnv;
    function ShowModal(const EnvName: string): TModalResult;
    property EnvManager: TEnvManager read FEnvManager write SetEnvManager;
  end;

implementation

uses sysutils, AppHelpers;

{$R *.lfm}

{ TEnvForm }

procedure TEnvForm.FormCreate(Sender: TObject);
begin
  HideEnv;
  tbEnv.Caption := '------';
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
begin
  EnvParent := nil;
  if chkParent.Checked and (cbParent.ItemIndex > -1) then
    EnvParent := FEnvManager.Env[cbParent.Items[cbParent.ItemIndex]];

  case FOpState of
    opAdd: begin
      Env := TEnvironment.Create(editName.Text, EnvParent);
      try
        FEnvManager.Add(Env);
        CreateMenuItem(Env.Name);
        tbEnv.Caption := Env.Name;
      except
        on E: Exception do begin
          ERRMsg('Error', E.Message);
          FreeAndNil(Env);
        end;
      end;
    end;
    opEdit: begin

    end;
  end;

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
  dbEnv.Caption := 'Edit environment: ';
  ShowEnv;
  FOpState := opEdit;
end;

function TEnvForm.CreateMenuItem(const EnvName: string): TMenuItem;
begin
  Result := TMenuItem.Create(menuEnv);
  Result.Caption := EnvName;
  Result.RadioItem := True;
  Result.OnClick := @onSelectEnv;
  menuEnv.Items.Add(Result);
end;

procedure TEnvForm.OnSelectEnv(Sender: TObject);
var
  MI: TMenuItem;
begin
  if (Sender is TMenuItem) then
  begin
    MI := TMenuItem(Sender);
    tbEnv.Caption := MI.Caption;
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

