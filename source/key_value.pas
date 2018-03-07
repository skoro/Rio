unit key_value;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

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
  private
    function GetKey: string;
    function GetValue: string;
    procedure SetKey(AValue: string);
    procedure SetValue(AValue: string);

  public
    property Key: string read GetKey write SetKey;
    property Value: string read GetValue write SetValue;
  end;

var
  KeyValueForm: TKeyValueForm;

implementation

{$R *.lfm}

{ TKeyValueForm }

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

end.

