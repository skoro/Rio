unit frmEnv;

{$mode objfpc}{$H+}

interface

uses
  DividerBevel, Forms,
  ButtonPanel, ExtCtrls, Grids, StdCtrls, ComCtrls, Menus,
  GridNavigator, Classes;

type

  { TEnvForm }

  TEnvForm = class(TForm)
    btnSave: TButton;
    ButtonPanel: TButtonPanel;
    chkInherit: TCheckBox;
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
    procedure editNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbAddClick(Sender: TObject);
    procedure tbEditClick(Sender: TObject);
  private

  public
    procedure ShowEnv;
    procedure HideEnv;
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

procedure TEnvForm.tbAddClick(Sender: TObject);
begin
  if panEnv.Visible then
    HideEnv
  else begin
    dbEnv.Caption := 'New environment';
    editName.Text := '';
    chkInherit.Checked := False;
    btnSave.Enabled := False;
    ShowEnv;
  end;
end;

procedure TEnvForm.tbEditClick(Sender: TObject);
begin
  if PanEnv.Visible then
    HideEnv
  else begin
    dbEnv.Caption := 'Edit environment: ';
    ShowEnv;
  end;
end;

procedure TEnvForm.ShowEnv;
begin
  PanEnv.Visible := True;
end;

procedure TEnvForm.HideEnv;
begin
  PanEnv.Visible := False;
end;

end.

