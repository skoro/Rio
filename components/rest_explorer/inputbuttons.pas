unit inputbuttons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls;

type
  TInputButtons = class(TCustomPanel)
  private
    FResetButton: TSpeedButton;
    FExecButton: TSpeedButton;
    FInput: TComboBox;
    FOnResetClick: TNotifyEvent;
    FOnExecClick: TNotifyEvent;
    FOnInputChange: TNotifyEvent;
    FOnInputKeyPress: TKeyPressEvent;
    function GetText: string;
    procedure InternalOnResetClick(Sender: TObject);
    procedure InternalOnExecClick(Sender: TObject);
    procedure InternalOnInputChange(Sender: TObject);
    procedure InternalOnInputKeyPress(Sender: TObject; var Key: char);
    procedure SetText(AValue: string);
  protected
    function CreateResetButton: TSpeedButton; virtual;
    function CreateExecButton: TSpeedButton; virtual;
    function CreateInput: TComboBox; virtual;
    procedure InputChange; virtual;
    procedure InputKeyPress(var Key: char); virtual;
    procedure ExecClick; virtual;
    procedure ResetClick; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ExecButton: TSpeedButton read FExecButton;
    property ResetButton: TSpeedButton read FResetButton;
    property Input: TComboBox read FInput;
    property OnResetClick: TNotifyEvent read FOnResetClick write FOnResetClick;
    property OnExecClick: TNotifyEvent read FOnExecClick write FOnExecClick;
    property OnInputChange: TNotifyEvent read FOnInputChange write FOnInputChange;
    property OnInputKeyPress: TKeyPressEvent read FOnInputKeyPress write FOnInputKeyPress;
    property Text: string read GetText write SetText;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc',[TInputButtons]);
end;

{ TInputButtons }

procedure TInputButtons.InternalOnResetClick(Sender: TObject);
begin
  ResetClick;
end;

function TInputButtons.GetText: string;
begin
  Result := FInput.Text;
end;

procedure TInputButtons.InternalOnExecClick(Sender: TObject);
begin
  ExecClick;
end;

procedure TInputButtons.InternalOnInputChange(Sender: TObject);
begin
  InputChange;
end;

procedure TInputButtons.InternalOnInputKeyPress(Sender: TObject; var Key: char);
begin
  InputKeyPress(Key);
end;

procedure TInputButtons.SetText(AValue: string);
begin
  if GetText = AValue then
    Exit;
  FInput.Text := AValue;
  InputChange;
end;

function TInputButtons.CreateResetButton: TSpeedButton;
begin
  Result := TSpeedButton.Create(Self);
  Result.Caption := 'Reset';
  Result.AutoSize := True;
  Result.Align := alRight;
  Result.Parent := Self;
end;

function TInputButtons.CreateExecButton: TSpeedButton;
begin
  Result := TSpeedButton.Create(Self);
  Result.Caption := 'Filter';
  Result.AutoSize := True;
  Result.Align := alRight;
  Result.Parent := Self;
end;

function TInputButtons.CreateInput: TComboBox;
begin
  Result := TComboBox.Create(Self);
  Result.Text := '';
  Result.Align := alClient;
  Result.Parent := Self;
  Result.AutoComplete := True;
end;

procedure TInputButtons.InputChange;
var
  IsBtnEnabled: Boolean;
begin
  if FInput.Text = '' then
    IsBtnEnabled := False
  else
    IsBtnEnabled := True;
  FExecButton.Enabled := IsBtnEnabled;
  FResetButton.Enabled := IsBtnEnabled;
  if Assigned(FOnInputChange) then
    FOnInputChange(Self);
end;

procedure TInputButtons.InputKeyPress(var Key: char);
begin
  case Key of
    #13: ExecClick;
    #27: ResetClick;
  end;
end;

procedure TInputButtons.ExecClick;
begin
  if FInput.Text = '' then
    Exit;
  FInput.AddHistoryItem(FInput.Text, 10, True, True);
  if Assigned(FOnExecClick) then
    FOnExecClick(Self);
end;

procedure TInputButtons.ResetClick;
begin
  FInput.Text := '';
  FResetButton.Enabled := False;
  FExecButton.Enabled := False;
  if Assigned(FOnResetClick) then
    FOnResetClick(Self);
end;

constructor TInputButtons.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResetButton := CreateResetButton;
  FResetButton.OnClick := @InternalOnResetClick;
  FResetButton.Enabled := False;
  FExecButton := CreateExecButton;
  FExecButton.OnClick := @InternalOnExecClick;
  FExecButton.Enabled := False;
  FInput := CreateInput;
  FInput.OnChange := @InternalOnInputChange;
  FInput.OnKeyPress := @InternalOnInputKeyPress;
  AutoSize := True;
  BevelOuter := bvNone;
end;

destructor TInputButtons.Destroy;
begin
  inherited Destroy;
end;

end.
