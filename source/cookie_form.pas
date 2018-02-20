unit cookie_form;

{$mode objfpc}{$H+}

interface

uses
  DateTimePicker, Forms,
  ExtCtrls, StdCtrls, Classes, Grids;

type

  { TCookieForm }

  TCookieForm = class(TForm)
    Button1: TButton;
    cbHttp: TCheckBox;
    cbSecure: TCheckBox;
    dateExpires: TDateTimePicker;
    Label1: TLabel;
    Label2: TLabel;
    editName: TLabeledEdit;
    editDomain: TLabeledEdit;
    editPath: TLabeledEdit;
    memoValue: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
  private

  public
    procedure Edit(Columns: TGridColumns; Cookie: TStrings);
    procedure Insert;
    procedure SetExpiresDateTime(const value: String);
  end;

var
  CookieForm: TCookieForm;

implementation

uses DateUtils, SysUtils, RegExpr, httpprotocol;

{$R *.lfm}

{ TCookieForm }

procedure TCookieForm.Edit(Columns: TGridColumns; Cookie: TStrings);
var
  I: Integer;
begin
  cbHttp.Checked := False;
  cbSecure.Checked := False;

  for I := 0 to Columns.Count - 1 do
    case LowerCase(Columns.Items[I].Title.Caption) of
      'name'   : editName.Text := Cookie[I];
      'domain' : editDomain.Text := Cookie[I];
      'path'   : editPath.Text := Cookie[I];
      'value'  : memoValue.Text := Cookie[I];
      'http'   : if Cookie[I] = '1' then cbHttp.Checked := True;
      'secure' : if Cookie[I] = '1' then cbSecure.Checked := True;
      'expires': SetExpiresDateTime(Cookie[I]);
    end;

  Show;
end;

procedure TCookieForm.Insert;
begin
  cbHttp.Checked := False;
  cbSecure.Checked := False;
  editName.Text := '';
  editDomain.Text := '';
  editPath.Text := '';
  memoValue.Text := '';
  dateExpires.DateTime := IncHour(Now, 1);

  Show;
end;

{
https://tools.ietf.org/html/rfc2616#section-3.3.1
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
    dateExpires.Enabled := True;
  end
  else
    dateExpires.Enabled := False;
  Reg.Free;
end;

end.

