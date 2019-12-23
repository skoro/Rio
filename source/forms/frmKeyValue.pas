unit frmKeyValue;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, ExtCtrls,
  StdCtrls, ButtonPanel;

const
  FocusKey = 1;
  FocusVal = 2;

type

  { TKeyValue }

  TKeyValue = record
    Key: string;
    Value: string;
    Enabled: Boolean;
  end;

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
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
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
                        AEnabled: Boolean = False;
                        const Focus: Integer = FocusKey): TKeyValue;
    function Edit(const KV: TKeyValue; const title: string;
                        const Focus: Integer = FocusKey): TKeyValue;
    procedure View(const AKey, AValue, Title: string);
    procedure View(const KV: TKeyValue; const title: string);
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

procedure TKeyValueForm.FormCreate(Sender: TObject);
begin
  FFocusedComponent := nil;
end;

procedure TKeyValueForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    ModalResult := mrCancel;
end;

function TKeyValueForm.GetKey: string;
begin
  Result := editName.Text;
end;

function TKeyValueForm.GetValue: string;
begin
  Result := TrimRight(textValue.Text);
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
  AEnabled: Boolean = False;
  const Focus: Integer = FocusKey): TKeyValue;
begin
  SetKey(AKey);
  SetValue(AValue);
  Caption := title;
  cbEnabled.Visible := True;
  cbEnabled.Checked := AEnabled;
  ButtonPanel.OKButton.Caption := '&OK';
  case Focus of
    FocusKey: FFocusedComponent := editName;
    FocusVal: FFocusedComponent := textValue;
  end;
  if ShowModal = mrOk then
  begin
    Result.Key := GetKey;
    Result.Value := GetValue;
    Result.Enabled := cbEnabled.Checked;
    // Fix #146: don't allow to add empty rows.
    if (Trim(AKey) = '') and (Trim(AValue) = '')
       and (Trim(Result.Key) = '') and (Trim(Result.Value) = '') then
      ModalResult := mrCancel;
  end
  else
  begin
    Result.Key := akey;
    Result.Value := avalue;
  end;
end;

function TKeyValueForm.Edit(const KV: TKeyValue; const title: string;
  const Focus: Integer = FocusKey): TKeyValue;
begin
  Result := Edit(KV.Key, KV.Value, title, KV.Enabled, Focus);
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

procedure TKeyValueForm.View(const KV: TKeyValue; const title: string);
begin
  View(KV.Key, KV.Value, title);
end;

end.
