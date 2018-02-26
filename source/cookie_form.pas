unit cookie_form;

{$mode objfpc}{$H+}

interface

uses
  DateTimePicker, Forms,
  ExtCtrls, StdCtrls, Classes, Grids;

type

  { TCookieForm }

  TCookieForm = class(TForm)
    btnOK: TButton;
    cbHttp: TCheckBox;
    cbSecure: TCheckBox;
    dateExpires: TDateTimePicker;
    editDomain: TLabeledEdit;
    editName: TLabeledEdit;
    editPath: TLabeledEdit;
    Label1: TLabel;
    labelExpires: TLabel;
    memoValue: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelTop: TPanel;
    PanelClient: TPanel;
    procedure btnOKClick(Sender: TObject);
  private
    procedure ShowExpires(AVisible: Boolean = True);
    function FormatExpiresDate: string;
    procedure InitValuesFromGrid(grid: TStringGrid);
  public
    procedure View(grid: TStringGrid);
    procedure SetExpiresDateTime(const value: String);
  end;

var
  CookieForm: TCookieForm;

implementation

uses DateUtils, SysUtils, RegExpr, httpprotocol, Controls, strutils;

{$R *.lfm}

{ TCookieForm }

procedure TCookieForm.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TCookieForm.ShowExpires(AVisible: Boolean);
begin
  if dateExpires.Visible = AVisible then Exit; //=>
  dateExpires.Visible := AVisible;
  labelExpires.Visible := AVisible;
  // Adjust panel size depends of expires components
  if AVisible then
    PanelTop.Height := PanelTop.Height + dateExpires.Height + labelExpires.Height
  else
    PanelTop.Height := PanelTop.Height - dateExpires.Height - labelExpires.Height
end;

function TCookieForm.FormatExpiresDate: string;
var
  day, month: string;
begin
  day := HTTPDays[DayOfWeek(dateExpires.DateTime)];
  month := HTTPMonths[MonthOf(dateExpires.DateTime)];
  Result := day + FormatDateTime('"," dd"-mm-"yyyy hh":"nn":"ss "GMT"', dateExpires.DateTime);
  Result := ReplaceStr(Result, 'mm', month);
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
      'expires': SetExpiresDateTime(data[I]);
    end;
end;

procedure TCookieForm.View(grid: TStringGrid);
begin
  InitValuesFromGrid(grid);
  Show;
end;

{
  https://tools.ietf.org/html/rfc2616#section-3.3.1
  Date parser.
  ScanDateTime from DateUtils cannot parse those dates.
}
procedure TCookieForm.SetExpiresDateTime(const value: String);
var
  date: tdatetime;
  Reg: TRegExpr;
  day, month, year, hour, minute, second, i: integer;
begin
  Reg := TRegExpr.Create;
  Reg.Expression := '([A-Z][a-z]{2}), ([0-9]{2})-([A-Z][a-z]{2})-([0-9]{4}) ([0-9]{2}):([0-9]{2}):([0-9]{2}) GMT';
  if Reg.Exec(value) then begin
    day := StrToInt(Reg.Match[2]);
    year := StrToInt(Reg.Match[4]);
    hour := StrToInt(Reg.Match[5]);
    minute := StrToInt(Reg.Match[6]);
    second := StrToInt(Reg.Match[7]);
    for I := 1 to Length(HTTPMonths) do
      if HTTPMonths[I] = Reg.Match[3] then begin
        month := I;
        break;
      end;
    date := EncodeDateTime(year, month, day, hour, minute, second, 0);
    dateExpires.DateTime := date;
    ShowExpires;
  end
  else
    ShowExpires(False);
  Reg.Free;
end;

end.

