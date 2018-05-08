unit options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls,
  StdCtrls, JSONPropStorage, Spin;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    Button1: TButton;
    cbJsonExpanded: TCheckBox;
    cbJsonSaveFmt: TCheckBox;
    editIndentSize: TSpinEdit;
    Label1: TLabel;
    PanelIndent: TPanel;
    Props: TJSONPropStorage;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure cbJsonSaveFmtChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetFmtIndentSize: Integer;
    function GetJsonExpanded: Boolean;
    function GetJsonSaveFmt: Boolean;

  public
    property JsonExpanded: Boolean read GetJsonExpanded;
    property JsonSaveFormatted: Boolean read GetJsonSaveFmt;
    property FmtIndentSize: Integer read GetFmtIndentSize;
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
  cbJsonSaveFmtChange(Sender);
end;

function TOptionsForm.GetFmtIndentSize: Integer;
begin
  Result := editIndentSize.Value;
end;

procedure TOptionsForm.cbJsonSaveFmtChange(Sender: TObject);
begin
  PanelIndent.Enabled := cbJsonSaveFmt.Checked;
end;

function TOptionsForm.GetJsonExpanded: Boolean;
begin
  Result := cbJsonExpanded.Checked;
end;

function TOptionsForm.GetJsonSaveFmt: Boolean;
begin
  Result := cbJsonSaveFmt.Checked;
end;

end.

