unit help_form;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel,
  StdCtrls, Classes, Controls;

type

  { THelpForm }

  THelpForm = class(TForm)
    bpButtons: TButtonPanel;
    helpText: TMemo;
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private

  public
    function ShowModal(ACaption, AHelpText: string): TModalResult; overload;
  end;

implementation

{$R *.lfm}

{ THelpForm }

procedure THelpForm.FormShow(Sender: TObject);
begin
  helpText.SetFocus;
end;

procedure THelpForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    Close;
end;

function THelpForm.ShowModal(ACaption, AHelpText: string): TModalResult;
begin
  Caption := ACaption;
  helpText.Text := AHelpText;
  Result := inherited ShowModal;
end;

end.

