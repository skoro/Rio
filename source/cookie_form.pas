unit cookie_form;

{$mode objfpc}{$H+}

interface

uses
  DateTimePicker, Forms,
  ExtCtrls, StdCtrls, Classes, Grids;

type

  { TViewState }

  TViewState = (vsView, vsEdit, vsNew);

  { TCookieForm }

  TCookieForm = class(TForm)
    btnOK: TButton;
    btnSave: TButton;
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
    FViewState: TViewState;
    procedure ShowExpires(AVisible: Boolean = True);
    procedure SetViewState(AState: TViewState);
  public
    property ViewState: TViewState read FViewState write SetViewState;
    procedure View(Columns: TGridColumns; Cookie: TStrings);
    procedure Insert;
    procedure Edit;
    procedure SetExpiresDateTime(const value: String);
  end;

var
  CookieForm: TCookieForm;

implementation

uses DateUtils, SysUtils, RegExpr, httpprotocol, Controls;

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

procedure TCookieForm.SetViewState(AState: TViewState);
begin
  FViewState := AState;
  if AState = vsView then begin
    editName.ReadOnly := True;
    editDomain.ReadOnly := True;
    editPath.ReadOnly := True;
    memoValue.ReadOnly := True;
    cbHttp.Enabled := False;
    cbSecure.Enabled := False;
    dateExpires.ReadOnly := True;
    btnSave.Visible := False;
  end
  else if (AState = vsNew) or (AState = vsEdit) then begin
    editName.ReadOnly := False;
    editDomain.ReadOnly := False;
    editPath.ReadOnly := False;
    memoValue.ReadOnly := False;
    cbHttp.Enabled := True;
    cbSecure.Enabled := True;
    dateExpires.ReadOnly := False;
    btnSave.Visible := True;
    ShowExpires;
    if Visible then Close;
  end;
end;

procedure TCookieForm.View(Columns: TGridColumns; Cookie: TStrings);
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

  SetViewState(vsView);
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

  SetViewState(vsNew);
  ShowModal;
end;

procedure TCookieForm.Edit;
begin
  SetViewState(vsEdit);
  ShowModal;
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

