unit options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, ExtCtrls, StdCtrls, JSONPropStorage, Spin, ComCtrls, fpjson,
  PairSplitter, Dialogs, Graphics, response_tabs, Classes;

type

  TUIFontItem = (fiGrids, fiEditor, fiJson, fiContent, fiValue);
  TFontItemList = array of TFont;

  { TOptionsPage }

  TOptionsPage = (opAppearance, opJson);

  { TOptionsForm }

  TOptionsForm = class(TForm)
    Button1: TButton;
    btnSelectFont: TButton;
    cbJsonExpanded: TCheckBox;
    cbJsonSaveFmt: TCheckBox;
    cbJsonFmtArray: TCheckBox;
    cbHideGridButtons: TCheckBox;
    cboxFontItem: TComboBox;
    editIndentSize: TSpinEdit;
    dlgFont: TFontDialog;
    GroupBox1: TGroupBox;
    gbLayout: TGroupBox;
    GroupBox2: TGroupBox;
    gbFonts: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lFontDemo: TLabel;
    pagesOptions: TPageControl;
    Panel1: TPanel;
    Props: TJSONPropStorage;
    rbJsonTree: TRadioButton;
    rbJsonFormatted: TRadioButton;
    rbLayoutVert: TRadioButton;
    rbLayoutHor: TRadioButton;
    tabJson: TTabSheet;
    tabAppearance: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btnSelectFontClick(Sender: TObject);
    procedure cboxFontItemChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFontItemList: TFontItemList;
    function GetGridButtonsHidden: Boolean;
    function GetJsonFormatOptions: TFormatOptions;
    function GetJsonIndentSize: Integer;
    function GetJsonExpanded: Boolean;
    function GetJsonSaveFmt: Boolean;
    function GetJsonView: TViewPage;
    function GetPanelsLayout: TPairSplitterType;
    procedure SetFontDemo;
    procedure InitFonts;

  public
    function ShowModalPage(page: TOptionsPage): TModalResult;
    function GetFontItem(AFontItem: TUIFontItem): TFont;
    procedure SetFontItem(AFontItem: TUIFontItem; AFont: TFont);
    property JsonExpanded: Boolean read GetJsonExpanded;
    property JsonSaveFormatted: Boolean read GetJsonSaveFmt;
    property JsonIndentSize: Integer read GetJsonIndentSize;
    property JsonView: TViewPage read GetJsonView;
    property JsonFormat: TFormatOptions read GetJsonFormatOptions;
    property PanelsLayout: TPairSplitterType read GetPanelsLayout;
    property GridButtonsHidden: Boolean read GetGridButtonsHidden;
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
  InitFonts;
end;

function TOptionsForm.GetFontItem(AFontItem: TUIFontItem): TFont;
begin
  Result := FFontItemList[Ord(AFontItem)];
end;

procedure TOptionsForm.btnSelectFontClick(Sender: TObject);
begin
  if dlgFont.Execute then begin
    FFontItemList[cboxFontItem.ItemIndex] := dlgFont.Font;
    SetFontDemo;
  end;
end;

procedure TOptionsForm.cboxFontItemChange(Sender: TObject);
begin
  SetFontDemo;
end;

function TOptionsForm.GetGridButtonsHidden: Boolean;
begin
  Result := cbHideGridButtons.Checked;
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

function TOptionsForm.GetJsonView: TViewPage;
begin
  if rbJsonTree.Checked then
    Result := vpTree
  else if rbJsonFormatted.Checked then
    Result := vpFormatted;
end;

function TOptionsForm.GetPanelsLayout: TPairSplitterType;
begin
  if rbLayoutHor.Checked then
    Result := pstHorizontal
  else
    Result := pstVertical;
end;

procedure TOptionsForm.SetFontDemo;
var
  FontObj: TFont;
  StyleName: string;
begin
  FontObj := FFontItemList[cboxFontItem.ItemIndex];
  StyleName := 'Regular';
  if FontObj.Bold then StyleName := 'Bold';
  if FontObj.Italic then StyleName := StyleName + ' Italic';
  if FontObj.Underline then StyleName := StyleName + 'Underline';
  if FontObj.StrikeThrough then StyleName := StyleName + 'StrikeThrough';
  lFontDemo.Font := FontObj;
  lFontDemo.Caption := Format('%s %d %s', [
    FontObj.Name, FontObj.Size, StyleName
  ]);
end;

procedure TOptionsForm.SetFontItem(AFontItem: TUIFontItem; AFont: TFont);
begin
  if FFontItemList[Ord(AFontItem)] <> AFont then
    FFontItemList[Ord(AFontItem)] := AFont;
end;

procedure TOptionsForm.InitFonts;
begin
  SetLength(FFontItemList, Ord(High(TUIFontItem)) + 1);
  FFontItemList[Ord(fiGrids)]   := TFont.Create;
  FFontItemList[Ord(fiEditor)]  := TFont.Create;
  FFontItemList[Ord(fiJson)]    := TFont.Create;
  FFontItemList[Ord(fiContent)] := TFont.Create;
  FFontItemList[Ord(fiValue)]   := TFont.Create;
  cboxFontItem.Items.AddStrings([
    'Grids', 'Editor', 'Json tree', 'Response content', 'Value editor'
  ]);
  cboxFontItem.ItemIndex := 0;
  SetFontDemo;
end;

function TOptionsForm.ShowModalPage(page: TOptionsPage): TModalResult;
begin
  case page of
    opAppearance: pagesOptions.ActivePage := tabAppearance;
    opJson:       pagesOptions.ActivePage := tabJson;
  end;
  Result := ShowModal;
end;

end.

