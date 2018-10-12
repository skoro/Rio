unit export_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, ButtonPanel,
  ExtCtrls, StdCtrls;

type

  TExportType = (etCurl, etPHPCurl, etPHPSocket);

  { TExportForm }

  TExportForm = class(TForm)
    btnCopy: TButton;
    btnSave: TButton;
    ButtonPanel: TButtonPanel;
    cbExport: TComboBox;
    lExport: TLabel;
    MemoResult: TMemo;
    PanelMain: TPanel;
    SaveDialog: TSaveDialog;
    TopPanel: TPanel;
    procedure btnCopyClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cbExportChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
  cbExport.Items.Add('Curl');
  cbExport.Items.Add('PHP (curl)');
  cbExport.ItemIndex := 0;
  MemoResult.Font := OptionsForm.GetFontItem(fiValue);
end;

procedure TExportForm.btnSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    // TODO: generate file name depending on export type.
    MemoResult.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TExportForm.btnCopyClick(Sender: TObject);
begin
  //
end;

procedure TExportForm.cbExportChange(Sender: TObject);
begin
  ExportRequest;
end;

procedure TExportForm.FormShow(Sender: TObject);
begin
  ExportRequest;
end;

procedure TExportForm.ExportRequest;
var
  exp: TExport;
  req: TRequestObject;
begin
  btnCopy.Enabled := True;
  btnSave.Enabled := True;

  try
    case TExportType(cbExport.ItemIndex) of
      etCurl:    exp := TCurlExport.Create;
      etPHPCurl: exp := TExport.Create;
    end;

    req := TRequestObject.Create(Form1);
    try
      exp.RequestObject := req;
      MemoResult.Text := exp.Output;
    except on E: Exception do
      begin
        Application.MessageBox(PChar(E.Message), 'Export error', MB_ICONERROR + MB_OK);
        btnCopy.Enabled := False;
        btnSave.Enabled := False;
      end;
    end;

  finally
    FreeAndNil(exp);
    FreeAndNil(req);
  end;
end;

end.

