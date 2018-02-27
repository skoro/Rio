unit cookie_form;

{$mode objfpc}{$H+}

interface

uses
  Forms,
  ExtCtrls, StdCtrls, Classes, Grids, Buttons, Controls;

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
    btnPrev: TSpeedButton;
    btnNext: TSpeedButton;
    procedure btnNextClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FResponseGrid: TStringGrid;
    FRequestGrid: TStringGrid;
    procedure InitValuesFromGrid;
    procedure EnableNextPrevButtons;
  public
    property ResponseGrid: TStringGrid read FResponseGrid write FResponseGrid;
    property RequestGrid: TStringGrid read FRequestGrid write FRequestGrid;
    procedure View;
  end;

var
  CookieForm: TCookieForm;

implementation

uses SysUtils, strutils, dialogs;

{$R *.lfm}

{ TCookieForm }

procedure TCookieForm.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TCookieForm.btnNextClick(Sender: TObject);
var
  btn: TSpeedButton;
begin
  btn := (Sender as TSpeedButton);
  if btn = btnNext then
    FResponseGrid.Row := FResponseGrid.Row + 1
  else
    if btn = btnPrev then FResponseGrid.Row := FResponseGrid.Row - 1;
  InitValuesFromGrid;
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

procedure TCookieForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 27 then // Escape
    Close
  else // Alt - Left
    if (Shift = [ssAlt]) and (key = 37) then btnNextClick(btnPrev)
  else // Alt - Right
  if (Shift = [ssAlt]) and (key = 39) then btnNextClick(btnNext);
end;

procedure TCookieForm.InitValuesFromGrid;
var
  I: Integer;
  data: TStrings;
begin
  cbHttp.Checked := False;
  cbSecure.Checked := False;
  data := FResponseGrid.Rows[FResponseGrid.Row];

  for I := 0 to FResponseGrid.Columns.Count - 1 do
    case LowerCase(FResponseGrid.Columns.Items[I].Title.Caption) of
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

  EnableNextPrevButtons;
end;

procedure TCookieForm.EnableNextPrevButtons;
begin
  btnPrev.Enabled := True;
  btnNext.Enabled := True;
  if FResponseGrid.Row <= 1 then
    btnPrev.Enabled := False;
  if FResponseGrid.Row >= FResponseGrid.RowCount - 1 then
    btnNext.Enabled := False;
end;

procedure TCookieForm.View;
begin
  InitValuesFromGrid;
  Show;
end;

end.

