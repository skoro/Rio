unit options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, JSONPropStorage;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbJsonExpanded: TCheckBox;
    Props: TJSONPropStorage;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    function GetJsonExpanded: Boolean;

  public
    property JsonExpanded: Boolean read GetJsonExpanded;
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

{ TOptionsForm }

procedure TOptionsForm.FormCreate(Sender: TObject);
var
  CF: String;
begin
  CF := GetAppConfigDir(False) + DirectorySeparator + 'Options' + ConfigExtension;
  Props.JSONFileName := CF;
  Props.Active := True;
end;

function TOptionsForm.GetJsonExpanded: Boolean;
begin
  Result := cbJsonExpanded.Checked;
end;

end.

