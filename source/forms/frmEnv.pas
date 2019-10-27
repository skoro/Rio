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
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
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
  public
    procedure ShowEnv;
    procedure HideEnv;
    property EnvManager: TEnvManager read FEnvManager write FEnvManager;
  end;

implementation

uses sysutils;

{$R *.lfm}

{ TEnvForm }

procedure TEnvForm.FormCreate(Sender: TObject);
begin
  HideEnv;
end;

procedure TEnvForm.editNameChange(Sender: TObject);
begin
  btnSave.Enabled := not (Length(Trim(editName.Text)) = 0);
end;

procedure TEnvForm.chkParentClick(Sender: TObject);
begin
  cbParent.Enabled := chkParent.Checked;
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

procedure TEnvForm.ShowEnv;
begin
  PanEnv.Visible := True;
end;

procedure TEnvForm.HideEnv;
begin
  PanEnv.Visible := False;
  FOpState := opNone;
end;

end.

