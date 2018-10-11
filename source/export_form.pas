unit export_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, StdCtrls;

type

  TExportType = (etCurl, etPHPCurl, etPHPSocket);

  { TExportForm }

  TExportForm = class(TForm)
    ButtonPanel: TButtonPanel;
    cbExport: TComboBox;
    lExport: TLabel;
    MemoResult: TMemo;
    PanelMain: TPanel;
    TopPanel: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

implementation

uses options;

{$R *.lfm}

{ TExportForm }

procedure TExportForm.FormCreate(Sender: TObject);
begin
  ButtonPanel.HelpButton.Enabled := False;
  ButtonPanel.OKButton.ModalResult := mrNone;
  cbExport.Items.Add('Curl');
  cbExport.ItemIndex := 0;
  MemoResult.Font := OptionsForm.GetFontItem(fiValue);
end;

end.

