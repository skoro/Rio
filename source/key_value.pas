unit key_value;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ValEdit;

type

  { TKeyValueForm }

  TKeyValueForm = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    editName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    textValue: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormShow(Sender: TObject);
  private
    function GetKey: string;
    function GetValue: string;
    procedure SetKey(AValue: string);
    procedure SetValue(AValue: string);

  public
    property Key: string read GetKey write SetKey;
    property Value: string read GetValue write SetValue;
    function Edit(const AKey, AValue, title: string): TKeyValuePair;
  end;

var
  KeyValueForm: TKeyValueForm;

implementation

{$R *.lfm}

{ TKeyValueForm }

procedure TKeyValueForm.FormShow(Sender: TObject);
begin
  editName.SetFocus;
end;

function TKeyValueForm.GetKey: string;
begin
  Result := editName.Text;
end;

function TKeyValueForm.GetValue: string;
begin
  Result := textValue.Text;
end;

procedure TKeyValueForm.SetKey(AValue: string);
begin
  if editName.Text = AValue then Exit;
  editName.Text := AValue;
end;

procedure TKeyValueForm.SetValue(AValue: string);
begin
  if textValue.Text = AValue then Exit;
  textValue.Text := AValue;
end;

function TKeyValueForm.Edit(const AKey, AValue, title: string): TKeyValuePair;
begin
  SetKey(AKey);
  SetValue(AValue);
  Caption := title;
  if ShowModal = mrOK then begin
    Result.Key := GetKey;
    Result.Value := GetValue;
  end
  else begin
    Result.Key := akey;
    Result.Value := avalue;
  end;
end;

end.

