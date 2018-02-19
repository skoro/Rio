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
  end;

var
  CookieForm: TCookieForm;

implementation

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
      'name'  : editName.Text := Cookie[I];
      'domain': editDomain.Text := Cookie[I];
      'path'  : editPath.Text := Cookie[I];
      'value' : memoValue.Text := Cookie[I];
      'http'  : if Cookie[I] = '1' then cbHttp.Checked := True;
      'secure': if Cookie[I] = '1' then cbSecure.Checked := True;
    end;

  Show;
end;

end.

