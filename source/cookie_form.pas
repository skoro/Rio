unit cookie_form;

{$mode objfpc}{$H+}

interface

uses
  Forms,
  ExtCtrls, StdCtrls, Classes, Grids;

type

  { TCookieForm }

  TCookieForm = class(TForm)
    btnOK: TButton;
    cbHttp: TCheckBox;
    cbSecure: TCheckBox;
    expiresValue: TEdit;
    editDomain: TLabeledEdit;
    editName: TLabeledEdit;
    editPath: TLabeledEdit;
    Label1: TLabel;
    labelExpires: TLabel;
    memoValue: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure InitValuesFromGrid(grid: TStringGrid);
  public
    procedure View(grid: TStringGrid);
  end;

var
  CookieForm: TCookieForm;

implementation

uses SysUtils, strutils;

{$R *.lfm}

{ TCookieForm }

procedure TCookieForm.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TCookieForm.FormCreate(Sender: TObject);
begin
  editName.ReadOnly:=True;
  editDomain.ReadOnly:=True;
  editPath.ReadOnly:=True;
  memoValue.ReadOnly:=True;
  expiresValue.ReadOnly:=True;
  cbHttp.Enabled:=False;
  cbSecure.Enabled:=False;
end;

procedure TCookieForm.InitValuesFromGrid(grid: TStringGrid);
var
  I: Integer;
  data: TStrings;
begin
  cbHttp.Checked := False;
  cbSecure.Checked := False;
  data := grid.Rows[grid.Row];

  for I := 0 to grid.Columns.Count - 1 do
    case LowerCase(grid.Columns.Items[I].Title.Caption) of
      'name'   : editName.Text := data[I];
      'domain' : editDomain.Text := data[I];
      'path'   : editPath.Text := data[I];
      'value'  : memoValue.Text := data[I];
      'http'   : if data[I] = '1' then cbHttp.Checked := True;
      'secure' : if data[I] = '1' then cbSecure.Checked := True;
      'expires': begin
        expiresValue.Text := IfThen(data[I] = '', '', data[I]);
        expiresValue.Enabled := data[I] <> '';
      end;
    end;
end;

procedure TCookieForm.View(grid: TStringGrid);
begin
  InitValuesFromGrid(grid);
  Show;
end;

end.

