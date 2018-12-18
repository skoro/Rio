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
  TShortCutItem = (sciNone, sciFocusUrl, sciFocusMethod, sciManageHeaders, sciSaveRequest,
    sciOptions, sciNewRequest, sciNewWindow, sciOpenRequest, sciFind, sciFindNext,
    sciJsonFilter, sciSaveBody, sciSwitchView, sciSubmit, sciQuit);

  TShortCut = record
    ShiftState: TShiftState;
    Key: Word;
  end;

  TShortCuts = array of TShortCut;

  TFontItemList = array of TFont;

  { TOptionsPage }

  TOptionsPage = (opGeneral, opAppearance, opJson);

  { TOptionsForm }

  TOptionsForm = class(TForm)
    btnClose: TButton;
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
    procedure SetShortCut(Item: TShortCutItem; AKey: Word; AShiftState: TShiftState);
    function GetKeyNameByCode(AKey: Word): string;
    function GetShortCutName(Item: TShortCutItem): string;
    procedure OnPropsFontSave(Sender: TStoredValue; var Value: TStoredType);
    procedure OnPropsFontRestore(Sender: TStoredValue; var Value: TStoredType);
  public
    function ShowModalPage(page: TOptionsPage): TModalResult;
    function GetFontItem(AFontItem: TUIFontItem): TFont;
    function GetShortCutItem(AKey: Word; AShiftState: TShiftState): TShortCutItem;
    // Convert our shortcut to menus.shortcut compatible.
    function GetShortCutValue(Item: TShortCutItem): Classes.TShortCut;
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

uses app_helpers, SynEdit, Menus, fpjsonrtti;

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
    try
      // Del key is pressed. Reset the current shortcut.
      if (Key = 46) and (Shift = []) then
        SetShortCut(FKeySet, 0, [])
      else
        // Skip empty combinations (only Ctrl, Alt or Shift pressed).
        if (Key <> 16) and (Key <> 17) and (Key <> 18) then begin
          // Escape cancels reading keys.
          if (Key <> 27) and (Shift <> []) then
            SetShortCut(FKeySet, Key, Shift)
        end
        else
          Exit; // =>
      FreeAndNil(FKeyCatch);
      tabShortcuts.Enabled := True;
      btnClose.Enabled := True;
    except on E: Exception do
      WarnMsg('Warning', E.Message);
    end;
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
  btnClose.Enabled := False;
  FKeySet := TShortCutItem(aRow);
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

function TOptionsForm.GetShortCutItem(AKey: Word; AShiftState: TShiftState
  ): TShortCutItem;
var
  idx: Integer;
begin
  Result := sciNone;
  if AKey < 48 then // one of control (ctrl,alt,shift,etc) key pressed.
    Exit;
  for idx := Ord(Low(TShortCutItem)) to Ord(High(TShortCutItem)) do
    if (FShortCuts[idx].Key = AKey) and (FShortCuts[idx].ShiftState = AShiftState) then
      Exit(TShortCutItem(idx));
end;

function TOptionsForm.GetShortCutValue(Item: TShortCutItem): Classes.TShortCut;
begin
  with FShortCuts[Ord(Item)] do
    Result := ShortCut(Key, ShiftState);
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
  gridShortcuts.RowCount := max; // without sciNone.
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

procedure TOptionsForm.SetShortCut(Item: TShortCutItem; AKey: Word; AShiftState: TShiftState);
var
  txt: String;
  sc: TShortCut;
  idx: ShortInt;
begin
  for idx := Ord(Low(TShortCutItem)) to Ord(High(TShortCutItem)) do
    if (idx <> 0) and (idx <> Ord(Item)) and (FShortCuts[idx].Key = AKey)
       and (FShortCuts[idx].ShiftState = AShiftState) then
      raise Exception.Create('The combination has been already assigned in: "' +
            GetShortCutName(TShortCutItem(idx)) + '"');

  with FShortCuts[Ord(Item)] do begin
    key := AKey;
    ShiftState := AShiftState;
  end;

  idx := Ord(Item);
  gridShortcuts.Cells[0, idx] := GetShortCutName(Item);

  sc := FShortCuts[idx];
  txt := '';
  if ssCtrl in sc.ShiftState then
    txt := txt + 'Ctrl-';
  if ssShift in sc.ShiftState then
    txt := txt + 'Shift-';
  if ssAlt in sc.ShiftState then
    txt := txt + 'Alt-';
  if sc.key <> 0 then
    txt := txt + UpperCase(GetKeyNameByCode(sc.Key));

  gridShortcuts.Cells[1, idx] := txt;
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
    32:  Exit('Space');
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

function TOptionsForm.GetShortCutName(Item: TShortCutItem): string;
begin
  case Item of
    sciFocusUrl:      Result := 'Go to URL field';
    sciFocusMethod:   Result := 'Go to methods list';
    sciManageHeaders: Result := 'Manage headers';
    sciSaveRequest:   Result := 'Save request';
    sciOptions:       Result := 'Options';
    sciNewRequest:    Result := 'New request';
    sciNewWindow:     Result := 'New window';
    sciOpenRequest:   Result := 'Open request';
    sciFind:          Result := 'Find text';
    sciFindNext:      Result := 'Find next';
    sciJsonFilter:    Result := 'Switch Json filter';
    sciSaveBody:      Result := 'Save response body';
    sciSwitchView:    Result := 'Switch views';
    sciSubmit:        Result := 'Submit the request';
    sciQuit:          Result := 'Quit';
  end;
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

