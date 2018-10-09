unit key_value;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, ExtCtrls,
  StdCtrls, ValEdit, ButtonPanel;

const
  FocusKey = 1;
  FocusVal = 2;

type

  { TKeyValueForm }

  TKeyValueForm = class(TForm)
    ButtonPanel: TButtonPanel;
    cbEnabled: TCheckBox;
    editName: TEdit;
    LabelName: TLabel;
    LabelValue: TLabel;
    textValue: TMemo;
    PanelMain: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure textValueKeyPress(Sender: TObject; var Key: char);
  private
    FFocusedComponent: TWinControl;

    function GetKey: string;
    function GetValue: string;
    procedure SetKey(AValue: string);
    procedure SetValue(AValue: string);

  public
    property Key: string read GetKey write SetKey;
    property Value: string read GetValue write SetValue;
    function Edit(const AKey, AValue, title: string;
                        const Focus: Integer = FocusKey): TKeyValuePair;
    function Edit(const KV: TKeyValuePair; const title: string;
                        const Focus: Integer = FocusKey): TKeyValuePair;
    procedure View(const AKey, AValue, Title: string);
    procedure View(const KV: TKeyValuePair; const title: string);
  end;

var
  KeyValueForm: TKeyValueForm;

implementation

uses Clipbrd;

{$R *.lfm}

{ TKeyValueForm }

procedure TKeyValueForm.FormShow(Sender: TObject);
begin
  if Assigned(FFocusedComponent) then
    FFocusedComponent.SetFocus;
end;

procedure TKeyValueForm.textValueKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    ModalResult := mrCancel;
end;

procedure TKeyValueForm.FormCreate(Sender: TObject);
begin
  FFocusedComponent := nil;
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
  if editName.Text = AValue then
    Exit;
  editName.Text := AValue;
end;

procedure TKeyValueForm.SetValue(AValue: string);
begin
  if textValue.Text = AValue then
    Exit;
  textValue.Text := AValue;
end;

function TKeyValueForm.Edit(const AKey, AValue, title: string;
  const Focus: Integer = FocusKey): TKeyValuePair;
begin
  SetKey(AKey);
  SetValue(AValue);
  Caption := title;
  cbEnabled.Visible := True;
  ButtonPanel.OKButton.Caption := '&OK';
  case Focus of
    FocusKey: FFocusedComponent := editName;
    FocusVal: FFocusedComponent := textValue;
  end;
  if ShowModal = mrOk then
  begin
    Result.Key := GetKey;
    Result.Value := GetValue;
  end
  else
  begin
    Result.Key := akey;
    Result.Value := avalue;
  end;
end;

function TKeyValueForm.Edit(const KV: TKeyValuePair; const title: string;
  const Focus: Integer = FocusKey): TKeyValuePair;
begin
  Result := Edit(KV.Key, KV.Value, title, Focus);
end;

procedure TKeyValueForm.View(const AKey, AValue, Title: string);
begin
  SetKey(AKey);
  SetValue(AValue);
  cbEnabled.Visible := False;
  Caption := title;
  ButtonPanel.OKButton.Caption := 'C&opy and Close';
  FFocusedComponent := textValue;
  if ShowModal = mrOk then
  begin
    Clipboard.AsText := textValue.Text;
  end;
end;

procedure TKeyValueForm.View(const KV: TKeyValuePair; const title: string);
begin
  View(KV.Key, KV.Value, title);
end;

end.
