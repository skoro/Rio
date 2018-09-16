unit import_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, StdCtrls;

type

  TImportFrom = (ifCurl);

  { TImportForm }

  TImportForm = class(TForm)
    ButtonPanel: TButtonPanel;
    cbImportFrom: TComboBox;
    linfo: TLabel;
    lImport: TLabel;
    input: TMemo;
    MainPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    procedure ImportData;
  public

  end;

implementation

uses import, LCLType;

{$R *.lfm}

{ TImportForm }

procedure TImportForm.FormCreate(Sender: TObject);
begin
  ButtonPanel.OKButton.ModalResult := mrNone;
  cbImportFrom.Items.Add('Curl');
  cbImportFrom.ItemIndex := 0;
  linfo.Caption := 'Curl command line:';
end;

procedure TImportForm.OKButtonClick(Sender: TObject);
begin
  ImportData;
end;

procedure TImportForm.ImportData;
var
  Engine: TImport;
begin
  try
    case TImportFrom(cbImportFrom.ItemIndex) of
      ifCurl: Engine := TCurlImport.Create;
    end;
    Engine.Input := input.Text;
  except on E: Exception do
    Application.MessageBox(PChar(E.Message), 'Import error', MB_ICONERROR + MB_OK);
  end;
end;

end.

