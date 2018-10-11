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
    SaveDialog: TSaveDialog;
    TopPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    procedure ExportRequest;
  public

  end;

implementation

uses options, export_req, request_object, main, LCLType;

{$R *.lfm}

{ TExportForm }

procedure TExportForm.FormCreate(Sender: TObject);
begin
  ButtonPanel.HelpButton.Enabled := False; // Help button acts as Save button.
  ButtonPanel.OKButton.ModalResult := mrNone;
  cbExport.Items.Add('Curl');
  cbExport.ItemIndex := 0;
  MemoResult.Font := OptionsForm.GetFontItem(fiValue);
end;

procedure TExportForm.HelpButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    // TODO: generate file name depending on export type.
    MemoResult.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TExportForm.OKButtonClick(Sender: TObject);
begin
  try
    ExportRequest;
    ButtonPanel.HelpButton.Enabled := True;
  except on E: Exception do
    Application.MessageBox(PChar(E.Message), 'Export error', MB_ICONERROR + MB_OK);
  end;
end;

procedure TExportForm.ExportRequest;
var
  exp: TExport;
  req: TRequestObject;
begin
  try
    case TExportType(cbExport.ItemIndex) of
      etCurl: exp := TCurlExport.Create;
    end;

    req := TRequestObject.Create(Form1);
    try
      exp.RequestObject := req;
      MemoResult.Text := exp.Output;
    except on E: Exception do
      begin
        FreeAndNil(exp);
        FreeAndNil(req);
        raise E;
      end;
    end;
  finally
    FreeAndNil(exp);
    FreeAndNil(req);
  end;
end;

end.

