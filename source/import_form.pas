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
    lImport: TLabel;
    memoData: TMemo;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

{ TImportForm }

procedure TImportForm.FormCreate(Sender: TObject);
begin
  cbImportFrom.Items.Add('Curl command line');
  cbImportFrom.ItemIndex := 0;
end;

end.

