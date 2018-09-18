unit import_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, StdCtrls, import;

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
    procedure FormDestroy(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FImport: TImport;
    function GetRequestObjects: TRequestObjectList;
    procedure ImportData;
  public
    property RequestObjects: TRequestObjectList read GetRequestObjects;
  end;

implementation

uses LCLType;

{$R *.lfm}

{ TImportForm }

procedure TImportForm.FormCreate(Sender: TObject);
begin
  ButtonPanel.OKButton.ModalResult := mrNone;
  cbImportFrom.Items.Add('Curl');
  cbImportFrom.ItemIndex := 0;
  linfo.Caption := 'Put Curl command line here:'#13#10'(only limited set of options supported)';
  FImport := nil;
end;

procedure TImportForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FImport) then
    FreeAndNil(FImport);
end;

procedure TImportForm.OKButtonClick(Sender: TObject);
begin
  try
    ImportData;
    ModalResult := mrOK;
  except on E: Exception do
    Application.MessageBox(PChar(E.Message), 'Import error', MB_ICONERROR + MB_OK);
  end;
end;

procedure TImportForm.ImportData;
begin
  if Assigned(FImport) then
    FreeAndNil(FImport);
  case TImportFrom(cbImportFrom.ItemIndex) of
    ifCurl: FImport := TCurlImport.Create;
  end;
  FImport.Input := input.Text;
end;

function TImportForm.GetRequestObjects: TRequestObjectList;
begin
  if not Assigned(FImport) then
    raise Exception.Create('Data not imported.');
  Result := FImport.RequestObjects;
end;

end.

