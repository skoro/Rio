unit help_form;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel,
  StdCtrls;

type

  { THelpForm }

  THelpForm = class(TForm)
    bpButtons: TButtonPanel;
    helpText: TMemo;
  private

  public
    function ShowModal(ACaption, AHelpText: string): TModalResult; overload;
  end;

implementation

{$R *.lfm}

{ THelpForm }

function THelpForm.ShowModal(ACaption, AHelpText: string): TModalResult;
begin
  Caption := ACaption;
  helpText.Text := AHelpText;
  Result := inherited ShowModal;
end;

end.

