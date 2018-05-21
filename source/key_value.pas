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
    procedure FormCreate(Sender: TObject);
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
    function Edit(const AKey, AValue, title: string): TKeyValuePair;
    function Edit(const KV: TKeyValuePair; const title: string): TKeyValuePair;
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

function TKeyValueForm.Edit(const AKey, AValue, title: string): TKeyValuePair;
begin
  SetKey(AKey);
  SetValue(AValue);
  Caption := title;
  btnOK.AutoSize := False;
  btnOK.Width := 67;
  btnOK.Caption := '&OK';
  FFocusedComponent := editName;
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

function TKeyValueForm.Edit(const KV: TKeyValuePair;
  const title: string): TKeyValuePair;
begin
  Result := Edit(KV.Key, KV.Value, title);
end;

procedure TKeyValueForm.View(const AKey, AValue, Title: string);
begin
  SetKey(AKey);
  SetValue(AValue);
  Caption := title;
  btnOK.Caption := 'C&opy and Close';
  btnOK.AutoSize := True;
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
