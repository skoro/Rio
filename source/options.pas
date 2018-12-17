unit options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, ExtCtrls, StdCtrls, JSONPropStorage, Spin, ComCtrls, fpjson,
  PairSplitter, Dialogs, Graphics, Controls, response_tabs, Classes,
  PropertyStorage, Grids;

type

  TUIFontItem = (fiGrids, fiEditor, fiJson, fiContent, fiValue, fiHelp);

  // Keyboard shortcut item.
  TShortCutItem = (sciFocusUrl, sciFocusMethod, sciManageHeaders, sciSaveRequest,
    sciOptions, sciNewRequest, sciNewWindow, sciOpenRequest, sciFind, sciFindNext,
    sciJsonFilter, sciSaveBody, sciSwitchView, sciSubmit, sciQuit);

  TShortCut = record
    Shift: Boolean;
    Control: Boolean;
    Alt: Boolean;
    key: Word;
  end;

  TShortCuts = array of TShortCut;

  TFontItemList = array of TFont;

  { TOptionsPage }

  TOptionsPage = (opGeneral, opAppearance, opJson);

  { TOptionsForm }

  TOptionsForm = class(TForm)
    Button1: TButton;
    btnSelectFont: TButton;
    btnResetFont: TButton;
    cbJsonExpanded: TCheckBox;
    cbJsonSaveFmt: TCheckBox;
    cbJsonFmtArray: TCheckBox;
    cbHideGridButtons: TCheckBox;
    cboxFontItem: TComboBox;
    cbJsonLines: TCheckBox;
    cbEditMethods: TCheckBox;
    editIndentSize: TSpinEdit;
    dlgFont: TFontDialog;
    GroupBox1: TGroupBox;
    gbLayout: TGroupBox;
    GroupBox2: TGroupBox;
    gbFonts: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lTimeout: TLabel;
    lFontDemo: TLabel;
    pagesOptions: TPageControl;
    Panel1: TPanel;
    Props: TJSONPropStorage;
    rbJsonTree: TRadioButton;
    rbJsonFormatted: TRadioButton;
    rbLayoutVert: TRadioButton;
    rbLayoutHor: TRadioButton;
    seTimeout: TSpinEdit;
    gridShortcuts: TStringGrid;
    tabJson: TTabSheet;
    tabAppearance: TTabSheet;
    tabGeneral: TTabSheet;
    tabShortcuts: TTabSheet;
    //TabSheet2: TTabSheet;
    procedure btnResetFontClick(Sender: TObject);
    procedure btnSelectFontClick(Sender: TObject);
    procedure cboxFontItemChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure gridShortcutsButtonClick(Sender: TObject; aCol, aRow: Integer);
  private
    FFontItemList: TFontItemList;
    FKeyCatch: TPanel; // Panel for new shortcut.
    FKeySet: TShortCutItem; //
    FShortCuts: TShortCuts; // List of application shortcuts.
    function GetEditRequestMethods: Boolean;
    function GetGridButtonsHidden: Boolean;
    function GetJsonFormatOptions: TFormatOptions;
    function GetJsonIndentSize: Integer;
    function GetJsonExpanded: Boolean;
    function GetJsonLines: Boolean;
    function GetJsonSaveFmt: Boolean;
    function GetJsonView: TViewPage;
    function GetPanelsLayout: TPairSplitterType;
    function GetFontIndexFromKeyName(AKeyName: string): integer;
    function GetDefaultFont(FontItem: TUIFontItem): TFont;
    function GetRequestTimeout: Integer;
    procedure SetFontDemo;
    procedure InitFonts;
    procedure InitShortcuts;
    procedure SetShortCut(Item: TShortCutItem; AKey: Word; ShiftState: TShiftState);
    function GetKeyNameByCode(AKey: Word): string;
    procedure OnPropsFontSave(Sender: TStoredValue; var Value: TStoredType);
    procedure OnPropsFontRestore(Sender: TStoredValue; var Value: TStoredType);
  public
    function ShowModalPage(page: TOptionsPage): TModalResult;
    function GetFontItem(AFontItem: TUIFontItem): TFont;
    procedure SetFontItem(AFontItem: TUIFontItem; AFont: TFont);
    procedure ApplyControlFont(const ParentControl: TWinControl; const AClassName: string; AFontItem: TUIFontItem);
    property JsonExpanded: Boolean read GetJsonExpanded;
    property JsonSaveFormatted: Boolean read GetJsonSaveFmt;
    property JsonIndentSize: Integer read GetJsonIndentSize;
    property JsonView: TViewPage read GetJsonView;
    property JsonFormat: TFormatOptions read GetJsonFormatOptions;
    property JsonLines: Boolean read GetJsonLines;
    property PanelsLayout: TPairSplitterType read GetPanelsLayout;
    property GridButtonsHidden: Boolean read GetGridButtonsHidden;
    property Timeout: Integer read GetRequestTimeout;
    property EditRequestMethods: Boolean read GetEditRequestMethods;
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses app_helpers, SynEdit, fpjsonrtti;

{$R *.lfm}

{ TOptionsForm }

procedure TOptionsForm.FormCreate(Sender: TObject);
var
  CF: String;
  I: integer;
begin
  SetLength(FFontItemList, Ord(High(TUIFontItem)) + 1);
  InitFonts;

  InitShortcuts;
  FKeyCatch := nil;

  CF := GetAppConfigDir(False) + DirectorySeparator + 'Options' + ConfigExtension;
  Props.JSONFileName := CF;

  // Save/restore fonts.
  for I := Ord(Low(TUIFontItem)) to Ord(High(TUIFontItem)) do
    with TStoredValue(Props.StoredValues.Add) do begin
      // Should be named like these. In save/restore index will be parsed.
      Name := 'Font_' + IntToStr(I);
      KeyString := 'Font_' + IntToStr(I);
      OnSave := @OnPropsFontSave;
      OnRestore := @OnPropsFontRestore;
    end;

  Props.Active := True;
  pagesOptions.ActivePage := tabGeneral;
end;

procedure TOptionsForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FKeyCatch) then begin
    // Del key is pressed. Reset the current shortcut.
    if (Key = 46) and (Shift = []) then
      SetShortCut(FKeySet, 0, [])
    else
      // Skip empty combinations (only Ctrl, Alt or Shift pressed).
      if (Key <> 16) and (Key <> 17) and (Key <> 18) then
        SetShortCut(FKeySet, Key, Shift)
      else
        Exit; // =>
    FreeAndNil(FKeyCatch);
    tabShortcuts.Enabled := True;
  end;
end;

procedure TOptionsForm.FormShow(Sender: TObject);
begin
  SetFontDemo;
end;

procedure TOptionsForm.gridShortcutsButtonClick(Sender: TObject; aCol,
  aRow: Integer);
begin
  tabShortcuts.Enabled := False;
  FKeySet := TShortCutItem(aRow - 1);
  FKeyCatch := TPanel.Create(tabShortcuts);
  with FKeyCatch do begin
    Parent := tabShortcuts;
    Caption := 'Press key combination or Del to delete.';
    Height := tabShortcuts.Height div 2;
    Width := tabShortcuts.Width - 80;
    Top := (tabShortcuts.Height - Height) div 2;
    Left := (tabShortcuts.Width - Width) div 2;
  end;
end;

function TOptionsForm.GetFontItem(AFontItem: TUIFontItem): TFont;
begin
  Result := FFontItemList[Ord(AFontItem)];
end;

procedure TOptionsForm.btnSelectFontClick(Sender: TObject);
var
  fdo: TFontDialogOptions;
begin
  // FIXME: fdFixedPitchOnly doesn't work on GTK widgetset ?
  fdo := dlgFont.Options;
  Exclude(fdo, fdFixedPitchOnly);
  if cboxFontItem.ItemIndex = Ord(fiEditor) then
    Include(fdo, fdFixedPitchOnly);
  dlgFont.Options := fdo;

  dlgFont.Font := FFontItemList[cboxFontItem.ItemIndex];
  if dlgFont.Execute then begin
    FFontItemList[cboxFontItem.ItemIndex].Assign(dlgFont.Font);
    SetFontDemo;
  end;
end;

procedure TOptionsForm.btnResetFontClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := cboxFontItem.ItemIndex;
  FFontItemList[Idx] := GetDefaultFont(TUIFontItem(Idx));
  SetFontDemo;
end;

procedure TOptionsForm.cboxFontItemChange(Sender: TObject);
begin
  SetFontDemo;
end;

function TOptionsForm.GetGridButtonsHidden: Boolean;
begin
  Result := cbHideGridButtons.Checked;
end;

function TOptionsForm.GetEditRequestMethods: Boolean;
begin
  Result := cbEditMethods.Checked;
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

function TOptionsForm.GetJsonLines: Boolean;
begin
  Result := cbJsonLines.Checked;
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

function TOptionsForm.GetFontIndexFromKeyName(AKeyName: string): integer;
begin
  Result := StrToInt(StringReplace(AKeyName, 'Font_', '', []));
end;

function TOptionsForm.GetDefaultFont(FontItem: TUIFontItem): TFont;
begin
  Result := TFont.Create;
  if FontItem = fiEditor then
    with Result do begin
      Name    := SynDefaultFontName;
      Height  := SynDefaultFontHeight;
      Pitch   := SynDefaultFontPitch;
      Quality := SynDefaultFontQuality;
    end;
end;

function TOptionsForm.GetRequestTimeout: Integer;
begin
  Result := seTimeout.Value;
end;

procedure TOptionsForm.SetFontDemo;
var
  FontObj: TFont;
  StyleName: string;
begin
  FontObj := FFontItemList[cboxFontItem.ItemIndex];
  // Don't show LCL default font parameters: it's not actual.
  if FontObj.Name = 'default' then begin
    lFontDemo.Caption := '';
    btnResetFont.Enabled := False;
    Exit;
  end;
  StyleName := 'Regular';
  if FontObj.Bold then StyleName := 'Bold';
  if FontObj.Italic then StyleName := StyleName + ' Italic';
  if FontObj.Underline then StyleName := StyleName + 'Underline';
  if FontObj.StrikeThrough then StyleName := StyleName + 'StrikeThrough';
  lFontDemo.Font := FontObj;
  lFontDemo.Caption := Format('%s %d %s', [
    FontObj.Name, FontObj.Size, StyleName
  ]);
  btnResetFont.Enabled := True;
end;

procedure TOptionsForm.SetFontItem(AFontItem: TUIFontItem; AFont: TFont);
begin
  if FFontItemList[Ord(AFontItem)] <> AFont then
    FFontItemList[Ord(AFontItem)] := AFont;
end;

procedure TOptionsForm.ApplyControlFont(const ParentControl: TWinControl;
  const AClassName: string; AFontItem: TUIFontItem);
var
  ChildControls: TList;
  I: Integer;
begin
  ChildControls := TList.Create;
  try
    EnumControls(ParentControl, AClassName, ChildControls);
    if ChildControls.Count > 0 then
      for I := 0 to ChildControls.Count - 1 do
        TWinControl(ChildControls[I]).Font := GetFontItem(AFontItem);
  finally
    ChildControls.Free;
  end;
end;

procedure TOptionsForm.InitFonts;
var
  I: integer;
begin
  for I := Ord(Low(TUIFontItem)) to Ord(High(TUIFontItem)) do
    FFontItemList[I] := GetDefaultFont(TUIFontItem(I));

  cboxFontItem.Items.AddStrings([
    'Grids', 'Editor', 'Json tree', 'Response content', 'Value editor',
    'Help text'
  ]);
  cboxFontItem.ItemIndex := 0;
end;

procedure TOptionsForm.InitShortcuts;
var
  max: ShortInt;
begin
  max := Ord(High(TShortCutItem)) + 1;
  SetLength(FShortCuts, max);
  gridShortcuts.RowCount := max + 1;
  // For key scan codes see docs/key_codes.txt
  SetShortCut(sciFocusUrl,      76, [ssCtrl]); // L
  SetShortCut(sciFocusMethod,   80, [ssCtrl]); // P
  SetShortCut(sciManageHeaders, 73, [ssCtrl]); // I
  SetShortCut(sciSaveRequest,   83, [ssCtrl]); // S
  SetShortCut(sciOptions,       188, [ssCtrl]); // ,
  SetShortCut(sciNewRequest,    78, [ssCtrl]); // N
  SetShortCut(sciNewWindow,     78, [ssCtrl, ssShift]); // N
  SetShortCut(sciOpenRequest,   79, [ssCtrl]); // O
  SetShortCut(sciFind,          70, [ssCtrl]); // F
  SetShortCut(sciFindNext,      114, []); // F3
  SetShortCut(sciJsonFilter,    69, [ssCtrl]); // E
  SetShortCut(sciSaveBody,      113, []); // F2
  SetShortCut(sciSwitchView,    115, []); // F4
  SetShortCut(sciSubmit,        120, []); // F9
  SetShortCut(sciQuit,          81, [ssCtrl]); // Q
end;

procedure TOptionsForm.SetShortCut(Item: TShortCutItem; AKey: Word; ShiftState: TShiftState);
var
  txt: String;
  sc: TShortCut;
  idx: ShortInt;
begin
  with FShortCuts[Ord(Item)] do begin
    key := AKey;
    Alt := ssAlt in ShiftState;
    Control := ssCtrl in ShiftState;
    Shift := ssShift in ShiftState;
  end;

  idx := Ord(Item);

  case Item of
    sciFocusUrl:      txt := 'Go to URL field';
    sciFocusMethod:   txt := 'Go to methods list';
    sciManageHeaders: txt := 'Manage headers';
    sciSaveRequest:   txt := 'Save request';
    sciOptions:       txt := 'Options';
    sciNewRequest:    txt := 'New request';
    sciNewWindow:     txt := 'New window';
    sciOpenRequest:   txt := 'Open request';
    sciFind:          txt := 'Find text';
    sciFindNext:      txt := 'Find next';
    sciJsonFilter:    txt := 'Switch Json filter';
    sciSaveBody:      txt := 'Save response body';
    sciSwitchView:    txt := 'Switch views';
    sciSubmit:        txt := 'Submit the request';
    sciQuit:          txt := 'Quit';
  end;

  gridShortcuts.Cells[0, idx + 1] := txt;

  sc := FShortCuts[idx];
  txt := '';
  if sc.Control then
    txt := txt + 'Ctrl-';
  if sc.Shift then
    txt := txt + 'Shift-';
  if sc.Alt then
    txt := txt + 'Alt-';
  if sc.key <> 0 then
    txt := txt + UpperCase(GetKeyNameByCode(sc.Key));

  gridShortcuts.Cells[1, idx + 1] := txt;
end;

function TOptionsForm.GetKeyNameByCode(AKey: Word): string;
begin
  if (AKey >= 48) and (AKey <= 90) then
    Exit(chr(AKey));
  if (AKey >= 112) and (AKey <= 123) then
    Exit('F' + IntToStr(AKey - 111));
  if (AKey >= 96) and (AKey <= 105) then
    Exit('numpad ' + IntToStr(AKey - 96));
  case AKey of
    33:  Exit('PageUp');
    34:  Exit('PageDown');
    35:  Exit('End');
    36:  Exit('Home');
    45:  Exit('Insert');
    106: Exit('*');
    107: Exit('+');
    109: Exit('-');
    110: Exit('.');
    111: Exit('/');
    186: Exit(';');
    187: Exit('=');
    188: Exit(',');
    189: Exit('-');
    190: Exit('.');
    191: Exit('/');
    192: Exit('`');
    219: Exit('(');
    220: Exit('\');
    221: Exit(')');
    222: Exit('''');
  end;
  raise Exception.Create('Cannot use the key.');
end;

procedure TOptionsForm.OnPropsFontSave(Sender: TStoredValue;
  var Value: TStoredType);
var
  jStr: TJSONStreamer;
  Idx: integer;
begin
  jStr := TJSONStreamer.Create(nil);
  try
    Idx := GetFontIndexFromKeyName(Sender.KeyString);
    if FFontItemList[Idx].Name = 'default' then
      Value := ''
    else
      Value := jStr.ObjectToJSONString(FFontItemList[Idx]);
  finally
    jStr.Free;
  end;
end;

procedure TOptionsForm.OnPropsFontRestore(Sender: TStoredValue;
  var Value: TStoredType);
var
  jStr: TJSONDeStreamer;
  Idx: integer;
begin
  if Length(Trim(Value)) = 0 then
    Exit;
  jStr := TJSONDeStreamer.Create(nil);
  try
    Idx := GetFontIndexFromKeyName(Sender.KeyString);
    jStr.JSONToObject(Value, FFontItemList[Idx]);
  finally
    jStr.Free;
  end;
end;

function TOptionsForm.ShowModalPage(page: TOptionsPage): TModalResult;
begin
  case page of
    opAppearance: pagesOptions.ActivePage := tabAppearance;
    opJson:       pagesOptions.ActivePage := tabJson;
    opGeneral:    pagesOptions.ActivePage := tabGeneral;
  end;
  Result := ShowModal;
end;

end.

