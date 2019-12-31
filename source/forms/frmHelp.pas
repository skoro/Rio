unit frmHelp;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel,
  StdCtrls, Classes;

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
    class procedure HelpModal(TheOwner: TComponent; ACaption, AHelpText: string);
  end;

implementation

uses options;

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

class procedure THelpForm.HelpModal(TheOwner: TComponent; ACaption, AHelpText: string);
begin
  with THelpForm.Create(TheOwner) do begin
    helpText.Font := OptionsForm.GetFontItem(fiHelp);
    ShowModal(ACaption, AHelpText);
    Free;
  end;
end;

end.

