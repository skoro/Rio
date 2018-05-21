unit options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, ExtCtrls,
  StdCtrls, JSONPropStorage, Spin, ComCtrls, fpjson;

type

  { TPanelsLayout }

  TPanelsLayout = (plVertical, plHorizontal);

  { TOptionsForm }

  TOptionsForm = class(TForm)
    Button1: TButton;
    cbJsonExpanded: TCheckBox;
    cbJsonSaveFmt: TCheckBox;
    cbJsonFmtArray: TCheckBox;
    editIndentSize: TSpinEdit;
    GroupBox1: TGroupBox;
    gbLayout: TGroupBox;
    Label1: TLabel;
    pagesOptions: TPageControl;
    Panel1: TPanel;
    Props: TJSONPropStorage;
    rbLayoutVert: TRadioButton;
    rbLayoutHor: TRadioButton;
    tabJson: TTabSheet;
    tabAppearance: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    function GetJsonFormatOptions: TFormatOptions;
    function GetJsonIndentSize: Integer;
    function GetJsonExpanded: Boolean;
    function GetJsonSaveFmt: Boolean;
    function GetPanelsLayout: TPanelsLayout;

  public
    property JsonExpanded: Boolean read GetJsonExpanded;
    property JsonSaveFormatted: Boolean read GetJsonSaveFmt;
    property JsonIndentSize: Integer read GetJsonIndentSize;
    property JsonFormat: TFormatOptions read GetJsonFormatOptions;
    property PanelsLayout: TPanelsLayout read GetPanelsLayout;
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
  pagesOptions.ActivePage := tabAppearance;
end;

function TOptionsForm.GetJsonFormatOptions: TFormatOptions;
begin
  Result := DefaultFormat;
  if cbJsonFmtArray.Checked then
    Result := Result + [foSingleLineArray];
end;

function TOptionsForm.GetJsonIndentSize: Integer;
begin
  Result := editIndentSize.Value;
end;

function TOptionsForm.GetJsonExpanded: Boolean;
begin
  Result := cbJsonExpanded.Checked;
end;

function TOptionsForm.GetJsonSaveFmt: Boolean;
begin
  Result := cbJsonSaveFmt.Checked;
end;

function TOptionsForm.GetPanelsLayout: TPanelsLayout;
begin
  if rbLayoutHor.Checked then
    Result := plHorizontal
  else
    Result := plVertical;
end;

end.

