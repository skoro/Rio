unit ResponseTabs;

{$mode objfpc}{$H+}
{$Interfaces corba}

interface

uses
  Classes, SysUtils, fpjson, JsonParserMod, ComCtrls, ExtCtrls, Controls, Forms,
  StdCtrls, Dialogs, Grids, SynEdit, SynEditTypes, ThreadHttpClient, AppHelpers,
  inputbuttons;

type

  { TResponseTab }

  TResponseTab = class
  private
    FName: string;
    FTabSheet: TTabSheet;
  public
    constructor Create;
    destructor Destroy; override;
    function OpenOnMimeType(const MimeType: string): boolean; virtual; abstract;
    procedure CreateUI(ATabSheet: TTabSheet); virtual;
    procedure FreeTab; virtual;
    procedure OnHttpResponse(ResponseInfo: TResponseInfo); virtual; abstract;
    procedure Save(const AFileName: string); virtual;
    function CanFind: Boolean; virtual;
    procedure InitSearch(Search: string; Options: TFindOptions); virtual;
    function FindNext: Integer; virtual;
    property Name: string read FName;
    property TabSheet: TTabSheet read FTabSheet;
  end;

  { TOnOpenResponseTab }

  TOnOpenResponseTab = procedure(Tab: TResponseTab; ResponseInfo: TResponseInfo) of object;
  TOnSaveTab = procedure(const FileName: string; Tab: TResponseTab) of object;

  { TResponseTabManager }

  TResponseTabManager = class
  private
    FPageControl: TPageControl;
    { TODO : Convert FTabs and FOpenedTabs to TCollection }
    FTabs: TFPList;
    FOpenedTabs: TFPList;
    FOnOpenResponseTab: TOnOpenResponseTab;
    FOnSaveTab: TOnSaveTab;
    function GetActiveTab: TResponseTab;
  public
    constructor Create(APageControl: TPageControl);
    destructor Destroy; override;
    procedure RegisterTab(Tab: TResponseTab);
    procedure OpenTabs(ResponseInfo: TResponseInfo);
    procedure Save(const FileName: string);
    procedure CloseTabs;
    function CanFind: TResponseTab; virtual;
    property PageControl: TPageControl read FPageControl write FPageControl;
    property OnOpenResponseTab: TOnOpenResponseTab read FOnOpenResponseTab write FOnOpenResponseTab;
    property OnSaveTab: TOnSaveTab read FOnSaveTab write FOnSaveTab;
    property OpenedTabs: TFPList read FOpenedTabs;
    property ActiveTab: TResponseTab read GetActiveTab;
  end;

  { ETabException }

  ETabException = class(Exception)
  private
    FTabName: string;
    function GetTabMessage: string;
  public
    constructor Create(ATabName, Msg: string);
    property TabName: string read FTabName;
    property TabMessage: string read GetTabMessage;
  end;

  { TResponseImageTab }

  TResponseImageTab = class(TResponseTab)
  private
    FImage: TImage;
    FImageType: string;
    function GetImage: TImage;
    procedure OnDblClickResize(Sender: TObject);
    function ParseImageType(const ContentType: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    function OpenOnMimeType(const MimeType: string): boolean; override;
    procedure CreateUI(ATabSheet: TTabSheet); override;
    procedure OnHttpResponse(ResponseInfo: TResponseInfo); override;
    procedure Save(const AFileName: string); override;
    procedure ResizeImage(ToStretch: boolean);
    property Image: TImage read GetImage;
    property ImageType: string read FImageType;
  end;

  { ISelectedText }

  IEditorSelectedText = interface
    ['{A7059F84-814F-47CC-B38A-ED49DD8BAFA7}']
    function SelectedText: string;
  end;

  { TResponseFormattedTab }

  TResponseFormattedTab = class(TResponseTab, IEditorSelectedText)
  private
    FSynEdit: TSynEdit;
    FAutoCreate: Boolean; // Whether to create editor or it will be created
                          // in descent classes ?
  protected
    FSearchText: string;
    FSearchOptions: TSynSearchOptions;
    FSearchPos: TPoint;
    procedure InitSearchParams; virtual;
    // Search a text in the source (it allows to search in a plain text,
    // converting FSearchOptions to the TFindOptions).
    function FindText(const SrcText, SearchText: string; FromPos: integer): TFindPos;
  public
    constructor Create;
    procedure CreateUI(ATabSheet: TTabSheet); override;
    procedure OnHttpResponse(ResponseInfo: TResponseInfo); override;
    function CanFind: Boolean; override;
    procedure InitSearch(Search: string; Options: TFindOptions); override;
    function FindNext: Integer; override;
    procedure FreeTab; override;
    property SynEdit: TSynEdit read FSynEdit;
    procedure Save(const AFileName: string); override;
    function SelectedText: string; virtual;
  end;

  { TViewPage }

  TViewPage = (vpTree, vpFormatted, vpTable);

  { TOnJsonFormat }

  TOnJsonFormat = procedure (JsonData: TJSONData; Editor: TSynEdit) of object;

  { TOnJsonData }

  TOnJsonData = procedure (RootJson, FilteredJson: TJSONData) of object;

  { TResponseJsonTab }

  TResponseJsonTab = class(TResponseFormattedTab)
  private type
    TSearchTable = record
      Col: Integer; // Cell column
      Row: Integer; // Cell row
      Pos: Integer; // Position in cell
    end;
  private var
    FLineNumbers: Boolean;
    FTreeView: TTreeView;
    FJsonRoot: TJSONData;
    FJsonParser: TJSONParser;
    FBtnOptions: TToolButton;
    FBtnFilter: TToolButton;
    FBtnTree: TToolButton;
    FBtnFormatted: TToolButton;
    FBtnTable: TToolButton;
    FPageControl: TPageControl;
    FTreeSheet: TTabSheet;
    FFormatSheet: TTabSheet;
    FTableSheet: TTabSheet;
    FFilter: TInputButtons;
    FOnJsonFormat: TOnJsonFormat;
    FTreeExpanded: Boolean;
    FOnJsonData: TOnJsonData;
    FSearchNode: TTreeNode;
    FSearchNodePos: Integer;
    FSearchTable: TSearchTable; // Current search position in the table.
    FToolbar: TToolbar;
    FGrid: TStringGrid;
    FTableDone: Boolean; // When the table is built and ready.
    FTableFallback: TViewPage;
    function GetTreeView: TTreeView;
    function GetViewPage: TViewPage;
    procedure LoadDocument(doc: string);
    procedure SetLineNumbers(AValue: Boolean);
    procedure SetOnJsonFormat(AValue: TOnJsonFormat);
    procedure SetViewPage(AValue: TViewPage);
    procedure ShowJsonData(AParent: TTreeNode; Data: TJSONData);
    procedure ClearJsonData;
    procedure ClearTable;
    procedure CreateToolbar(Parent: TWinControl);
    procedure ApplyFilter;
    procedure BuildTree(JsonData: TJSONData);
    procedure BuildTable(JsonData: TJSONData);
    procedure AdjustTableColumns;
    procedure SetFormattedText(JsonData: TJSONData);
    procedure InternalOnSwitchFilter(Sender: TObject);
    procedure OnChangeTreeMode(Sender: TObject);
    procedure OnChangeFormatMode(Sender: TObject);
    procedure OnChangeTableMode(Sender: TObject);
    procedure OnFilterClick(Sender: TObject);
    procedure OnFilterReset(Sender: TObject);
    procedure InternalOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure InternalOnTableDblClick(Sender: TObject);
    procedure InternalOnGetKeyValue(Sender: TObject; Increment: Integer;
      var AKey, AValue: string; var ACurrent, ATotal: Integer);
  protected
    procedure ToggleFilterPanel;
    procedure InitSearchParams; override;
    procedure ShowLineNumbers;
    procedure ActivateTable;
    function FindInNode(Node: TTreeNode): TTreeNode;
    function FindInTable: TSearchTable;
    function CanEnableTable(Json: TJSONData): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function OpenOnMimeType(const MimeType: string): boolean; override;
    procedure CreateUI(ATabSheet: TTabSheet); override;
    procedure OnHttpResponse(ResponseInfo: TResponseInfo); override;
    procedure FreeTab; override;
    procedure Filter(Node: TTreeNode); virtual;
    function IsFilterActive: Boolean;
    function FindNext: Integer; override;
    procedure ViewNextPage;
    procedure ExpandChildren(Node: TTreeNode; Collapse: Boolean = False);
    function SelectedText: string; override;
    property TreeView: TTreeView read GetTreeView;
    property JsonRoot: TJSONData read FJsonRoot;
    property ViewPage: TViewPage read GetViewPage write SetViewPage;
    property ButtonOptions: TToolButton read FBtnOptions;
    property TreeExpanded: Boolean read FTreeExpanded write FTreeExpanded default True;
    property OnJsonFormat: TOnJsonFormat read FOnJsonFormat write SetOnJsonFormat;
    property OnJsonData: TOnJsonData read FOnJsonData write FOnJsonData;
    property LineNumbers: Boolean read FLineNumbers write SetLineNumbers;
    property Toolbar: TToolbar read FToolbar write FToolbar;
    property TableFallback: TViewPage read FTableFallback write FTableFallback;
  end;

  { TResponseXMLTab }

  TResponseXMLTab = class(TResponseFormattedTab)
  public
    constructor Create;
    procedure CreateUI(ATabSheet: TTabSheet); override;
    function OpenOnMimeType(const MimeType: string): boolean; override;
  end;

implementation

uses options, frmKeyValue, strutils, SynHighlighterXML, LazUTF8, Graphics;

const
  ImageTypeMap: array[TJSONtype] of Integer =
  // (jtUnknown, jtNumber, jtString, jtBoolean, jtNull, jtArray, jtObject)
  (-1, 3, 2, 4, 5, 0, 1);

{ ETabException }

function ETabException.GetTabMessage: string;
begin
  Result := Format('%s: %s', [FTabName, Message]);
end;

constructor ETabException.Create(ATabName, Msg: string);
begin
  inherited Create(msg);
  FTabName := ATabName;
end;

{ TResponseFormattedTab }

procedure TResponseFormattedTab.InitSearchParams;
begin
  if ssoBackwards in FSearchOptions then begin
    FSearchPos.y := FSynEdit.Lines.Count;
    FSearchPos.x := FSynEdit.Lines[FSearchPos.y - 1].Length;
  end
  else
    FSearchPos := Point(0, 0);
end;

function TResponseFormattedTab.FindText(const SrcText, SearchText: string;
  FromPos: integer): TFindPos;
var
    Opts: TFindOptions;
begin
  Opts := [];
  if ssoMatchCase in FSearchOptions then
    Include(Opts, frMatchCase);
  if ssoWholeWord in FSearchOptions then
    Include(Opts, frWholeWord);
  if not (ssoBackwards in FSearchOptions) then
    Include(Opts, frDown);
  Result := FindInText(SrcText, SearchText, Opts, FromPos);
end;

constructor TResponseFormattedTab.Create;
begin
  FAutoCreate := True;
end;

procedure TResponseFormattedTab.CreateUI(ATabSheet: TTabSheet);
begin
  inherited CreateUI(ATabSheet);

  if not FAutoCreate then
    Exit; // =>

  // Init editor.
  FSynEdit := TSynEdit.Create(ATabSheet);
  FSynEdit.Parent := ATabSheet;
  FSynEdit.Align := alClient;
  FSynEdit.BorderStyle := bsNone;
  FSynEdit.ReadOnly := True;

  // Hide all the gutters except code folding.
  FSynEdit.Gutter.Parts.Part[0].Visible := False;
  FSynEdit.Gutter.Parts.Part[1].Visible := False;
  FSynEdit.Gutter.Parts.Part[2].Visible := False;
  FSynEdit.Gutter.Parts.Part[3].Visible := False;
end;

procedure TResponseFormattedTab.OnHttpResponse(ResponseInfo: TResponseInfo);
begin
  if Assigned(FSynEdit) then begin
    SynEdit.Text := ResponseInfo.Content.DataString;
    InitSearchParams;
  end;
end;

function TResponseFormattedTab.CanFind: Boolean;
begin
  Result := True;
end;

procedure TResponseFormattedTab.InitSearch(Search: string; Options: TFindOptions);
begin
  FSearchText := Search;
  FSearchOptions := [];

  if not (frDown in Options) then
    Include(FSearchOptions, ssoBackwards);
  if frMatchCase in Options then
    Include(FSearchOptions, ssoMatchCase);
  if frWholeWord in Options then
    Include(FSearchOptions, ssoWholeWord);

  InitSearchParams;
end;

function TResponseFormattedTab.FindNext: Integer;
var
  p, maxx, maxy: Integer;
begin
  p := FSynEdit.SearchReplaceEx(FSearchText, '', FSearchOptions, FSearchPos);
  if (p = 0) then begin
    if (ssoBackwards in FSearchOptions) then begin
      maxy := FSynEdit.Lines.Count;
      maxx := FSynEdit.Lines[maxy - 1].Length;
    end
    else begin
      maxy := 0;
      maxx := 0;
    end;
    // Not found at all.
    if (FSearchPos.x = maxx) and (FSearchPos.y = maxy) then
      Exit(-1);
  end;
  // Set position for the next search.
  if ssoBackwards in FSearchOptions then
    FSearchPos := FSynEdit.BlockBegin
  else
    FSearchPos := FSynEdit.BlockEnd;
  if p = 0 then
    Exit(0);
  Result := p;
end;

procedure TResponseFormattedTab.FreeTab;
begin
  if Assigned(FSynEdit) then
    FreeAndNil(FSynEdit);
  inherited FreeTab;
end;

procedure TResponseFormattedTab.Save(const AFileName: string);
begin
  if Assigned(FSynEdit) then
    FilePutContents(AFileName, FSynEdit.Text);
end;

function TResponseFormattedTab.SelectedText: string;
begin
  Result := '';
  if Assigned(FSynEdit) then
    Result := FSynEdit.SelText;
end;

{ TResponseXMLTab }

constructor TResponseXMLTab.Create;
begin
  inherited;
  FName := 'XML';
end;

procedure TResponseXMLTab.CreateUI(ATabSheet: TTabSheet);
begin
  inherited CreateUI(ATabSheet);
  // Init highlighter.
  SynEdit.Highlighter := TSynXMLSyn.Create(FSynEdit);
end;

function TResponseXMLTab.OpenOnMimeType(const MimeType: string): boolean;
begin
  Result := (MimeType = 'application/rss+xml') or
            (MimeType = 'application/atom+xml') or
            (MimeType = 'application/xml') or
            (MimeType = 'text/xml');
end;

{ TResponseJsonTab }

procedure TResponseJsonTab.LoadDocument(doc: string);
var
  S: TStringStream;
begin
  S := TStringStream.Create(doc);
  FJsonParser := TJSONParser.Create(S);
  with FTreeView.Items do begin
    BeginUpdate;
    try
      FJsonRoot := FJsonParser.Parse;
      ApplyFilter;
    finally
      EndUpdate;
    end;
  end;
  FreeAndNil(S);
end;

procedure TResponseJsonTab.SetLineNumbers(AValue: Boolean);
begin
  if FLineNumbers = AValue then Exit;
  FLineNumbers := AValue;
  ShowLineNumbers;
end;

procedure TResponseJsonTab.SetOnJsonFormat(AValue: TOnJsonFormat);
begin
  if FOnJsonFormat = AValue then
    Exit;
  FOnJsonFormat := AValue;
end;

procedure TResponseJsonTab.SetViewPage(AValue: TViewPage);
begin
  case AValue of
    vpTree: begin
      FPageControl.ActivePage := FTreeSheet;
      FBtnTree.Down := True;
      FBtnFormatted.Down := False;
      FBtnTable.Down := False;
    end;
    vpFormatted: begin
      FPageControl.ActivePage := FFormatSheet;
      FBtnTree.Down := False;
      FBtnFormatted.Down := True;
      FBtnTable.Down := False;
    end;
    vpTable: begin
      FPageControl.ActivePage := FTableSheet;
      FBtnTree.Down := False;
      FBtnFormatted.Down := False;
      FBtnTable.Down := True;
    end;
  end;
  // Force focus the control (this fixes focus lose on WIN).
  if TPageControl(FTabSheet.Parent).ActivePage = FTabSheet then
    if AValue = vpTree then
      FTreeView.SetFocus
    else if AValue = vpFormatted then
      FSynEdit.SetFocus;
  // Build the table with current json.
  if AValue = vpTable then
    ActivateTable;
end;

procedure TResponseJsonTab.ShowJsonData(AParent: TTreeNode; Data: TJSONData);
var
  N2: TTreeNode;
  I: Integer;
  D: TJSONData;
  C: String;
  S: TStringList;
begin
  if Not Assigned(Data) then
    exit;

  if not Assigned(AParent) then
  begin
    AParent := FTreeView.Items.AddChild(nil, '');
    AParent.ImageIndex := ImageTypeMap[Data.JSONType];
    AParent.SelectedIndex := ImageTypeMap[Data.JSONType];
    AParent.Data := Data;
  end;

  Case Data.JSONType of
    jtArray,
    jtObject:
      begin
        If (Data.JSONType = jtArray) then
          AParent.Text := AParent.Text + Format('[%d]', [Data.Count])
        else if (Data.JSONType = jtObject) and (AParent.Text = '') then
          AParent.Text := 'Object';
        S := TstringList.Create;
        try
          for I:=0 to Data.Count-1 do
            If Data.JSONtype = jtArray then
              S.AddObject(IntToStr(I), Data.items[i])
            else
              S.AddObject(TJSONObject(Data).Names[i], Data.items[i]);
          for I:=0 to S.Count-1 do
            begin
              N2 := FTreeView.Items.AddChild(AParent, S[i]);
              D := TJSONData(S.Objects[i]);
              N2.ImageIndex := ImageTypeMap[D.JSONType];
              N2.SelectedIndex := ImageTypeMap[D.JSONType];
              N2.Data := D;
              ShowJSONData(N2, D);
            end
        finally
          S.Free;
        end;
      end;
    jtNull:
      C := 'null';
  else
    C := Data.AsString;
    if (Data.JSONType = jtString) then
      C := '"'+C+'"';
    AParent.Text := AParent.Text + ': ' + C;
    AParent.Data := Data;
  end;
end;

procedure TResponseJsonTab.ClearJsonData;
begin
  if Assigned(FJsonRoot) then begin
    case FJsonRoot.JSONType of
      jtArray, jtObject: FJsonRoot.Clear;
    end;
    FreeAndNil(FJsonRoot);
  end;
  if Assigned(FTreeView) then
    FTreeView.Items.Clear;
  if Assigned(FJsonParser) then
    FreeAndNil(FJsonParser);
end;

procedure TResponseJsonTab.ClearTable;
begin
  if Assigned(FGrid) then
    FGrid.Clear;
  FTableDone := False;
end;

procedure TResponseJsonTab.CreateToolbar(Parent: TWinControl);
begin
  FToolbar := TToolBar.Create(Parent);
  FToolbar.Parent := Parent;
  FToolbar.EdgeBorders := [];
  FToolbar.ShowCaptions := True;
  FToolbar.List := True;

  FBtnTree := TToolButton.Create(FToolbar);
  FBtnTree.Parent := FToolbar;
  FBtnTree.Caption := 'Tree';
  FBtnTree.OnClick := @OnChangeTreeMode;

  FBtnFormatted := TToolButton.Create(FToolbar);
  FBtnFormatted.Parent := FToolbar;
  FBtnFormatted.Caption := 'Formatted';
  FBtnFormatted.OnClick := @OnChangeFormatMode;

  FBtnTable := TToolButton.Create(FToolbar);
  FBtnTable.Parent := FToolbar;
  FBtnTable.Caption := 'Table';
  FBtnTable.OnClick := @OnChangeTableMode;

  // Separator between view mode buttons and control buttons.
  with TToolButton.Create(FToolbar) do
  begin
    Parent := FToolbar;
    Style := tbsDivider;
  end;

  FBtnOptions := TToolButton.Create(FToolbar);
  FBtnOptions.Parent := FToolbar;
  FBtnOptions.Caption := 'Options';

  FBtnFilter := TToolButton.Create(FToolbar);
  with FBtnFilter do begin
    Parent  := FToolbar;
    Caption := 'Filter';
    Style   := tbsCheck;
    OnClick := @InternalOnSwitchFilter;
  end;
end;

procedure TResponseJsonTab.ApplyFilter;
var
  Filtered: TJSONData;
begin
  if IsFilterActive then
    Filtered := FJsonRoot.FindPath(FFilter.Text)
  else
    Filtered := FJsonRoot;

  if Assigned(FOnJsonData) then
    FOnJsonData(FJsonRoot, Filtered);

  if Assigned(Filtered) then
  begin
    SetFormattedText(Filtered);
    BuildTree(Filtered);
    if FTableDone then
      ClearTable;
    FBtnTable.Enabled := CanEnableTable(Filtered);
  end
  else begin
    FSynEdit.Text := '';
    BuildTree(nil);
  end;
end;

procedure TResponseJsonTab.BuildTree(JsonData: TJSONData);
begin
  FTreeView.Items.Clear;
  ShowJsonData(nil, JsonData);
  with FTreeView do
    if (Items.Count > 0) and Assigned(Items[0]) then begin
      Items[0].Expand(False);
      Selected := Items[0];
    end;
  if FTreeExpanded then
    FTreeView.FullExpand;
end;

procedure TResponseJsonTab.BuildTable(JsonData: TJSONData);
var
  i, k, colPos: integer;
  jsItem, jsData: TJSONData;
  header, dataValue: string;
  columnNames: TStringList;
begin
  columnNames := TStringList.Create;
  try
    with FGrid do
    begin
      BeginUpdate;
      Columns.Clear;
      RowCount := JsonData.Count + 1; // Total + Header.
      FixedRows := 1; // Header.
      for i := 0 to JsonData.Count - 1 do
      begin
        Application.ProcessMessages;
        jsItem := JsonData.Items[i];
        if (jsItem.JSONType = jtObject) then
        begin
          for k := 0 to jsItem.Count - 1 do // iterate over object.
          begin
            jsData := jsItem.Items[k];
            // Check column headers: add new ones dynamically.
            header := TJSONObject(jsItem).Names[k];
            colPos := columnNames.IndexOf(header); // data column index.
            if colPos = -1 then // new column.
            begin
              columnNames.Insert(k, header);
              colPos := k;
              with TGridColumn(Columns.Insert(k)).Title do
              begin
                Caption := header;
                Font.Style := [fsBold];
              end;
            end;
            // Complex types cannot be converted to a string.
            try
              if (jsData.JSONType = jtObject) or (jsData.JSONType = jtArray) then
                dataValue := jsData.FormatJSON([foSingleLineArray, foSingleLineObject, foSkipWhiteSpace])
              else if (jsData.JSONType = jtNull) then
                dataValue := 'NULL'
              else
                dataValue := jsData.AsString;
            except
              dataValue := 'Error';
            end;
            // insert data into a cell.
            Cells[colPos, i + 1] := dataValue;
          end; // for k
        end
        else begin
          try
            case jsItem.JSONType of
              jtArray: dataValue := jsItem.FormatJSON([foSingleLineArray, foSkipWhiteSpace]);
              jtNull:  dataValue := 'NULL';
              else     dataValue := jsItem.AsString;
            end;
          except
            dataValue := 'Error';
          end;
          if header = '' then // Only one column on a primitive type.
          begin
            header := 'Value';
            with Columns.Add.Title do
            begin
              Caption := header;
              Font.Style := [fsBold];
            end;
          end;
          Cells[0, i + 1] := dataValue;
        end;
      end; // for i
      // Don't show empty columns and rows when no data to show.
      if (RowCount - FixedRows) = 0 then
      begin
        FixedRows := 0;
        ColCount  := 1;
        RowCount  := 1;
        Cells[0, 0] := 'No data';
      end;
      EndUpdate;
    end; // with FGrid
    FTableDone := True; // Table build is done.
  finally
    FreeAndNil(columnNames);
  end;
end;

procedure TResponseJsonTab.AdjustTableColumns;
var
  i: Integer;
begin
  with FGrid do
  begin
    AutoSizeColumns;
    for i := 0 to ColCount - 1 do
      { TODO : Maximum column width should be configurable ? }
      if ColWidths[i] > 300 then
        ColWidths[i] := 300;
  end;
end;

procedure TResponseJsonTab.SetFormattedText(JsonData: TJSONData);
begin
  FSynEdit.BeginUpdate;
  if Assigned(FOnJsonFormat) then
    FOnJsonFormat(JsonData, FSynEdit)
  else
    FSynEdit.Text := JsonData.FormatJSON;
  FSynEdit.EndUpdate;
end;

procedure TResponseJsonTab.InternalOnSwitchFilter(Sender: TObject);
begin
  ToggleFilterPanel;
end;

procedure TResponseJsonTab.OnChangeTreeMode(Sender: TObject);
begin
  SetViewPage(vpTree);
end;

procedure TResponseJsonTab.OnChangeFormatMode(Sender: TObject);
begin
  SetViewPage(vpFormatted);
end;

procedure TResponseJsonTab.OnChangeTableMode(Sender: TObject);
begin
  SetViewPage(vpTable);
end;

procedure TResponseJsonTab.OnFilterClick(Sender: TObject);
begin
  ApplyFilter;
  if ViewPage = vpTable then
    ActivateTable;
end;

procedure TResponseJsonTab.OnFilterReset(Sender: TObject);
begin
  FFilter.Visible := False;
  FBtnFilter.Down := False;
  AppBusyCursor;
  ApplyFilter;
  AppResetCursor;
  if ViewPage = vpTable then
    ActivateTable;
end;

procedure TResponseJsonTab.InternalOnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case OptionsForm.GetShortCutItem(Key, Shift) of
    sciJsonFilter: InternalOnSwitchFilter(Sender);
    sciSwitchView: ViewNextPage;
  end;
end;

procedure TResponseJsonTab.ToggleFilterPanel;
begin
  FFilter.Visible := not FFilter.Visible;
  FBtnFilter.Down := FFilter.Visible;
  if FFilter.Visible then
    FFilter.Input.SetFocus;
end;

procedure TResponseJsonTab.InitSearchParams;
begin
  inherited;
  FSearchNode := nil;
  FSearchNodePos := 0;
  if Assigned(FGrid) then
  begin
    if ssoBackwards in FSearchOptions then
    begin
      FSearchTable.Row := FGrid.RowCount - 1;
      FSearchTable.Col := FGrid.ColCount - 1;
    end
    else
      begin
        FSearchTable.Row := FGrid.FixedRows;
        FSearchTable.Col := FGrid.FixedCols;
      end;
  end;
  FSearchTable.Pos := 0;
end;

procedure TResponseJsonTab.ShowLineNumbers;
begin
  if not Assigned(FSynEdit) then
    Exit; // =>
  FSynEdit.Gutter.Parts.Part[1].Visible := FLineNumbers;
end;

procedure TResponseJsonTab.ActivateTable;
var
  jsParser: TJSONParser;
  jsData: TJSONData;
begin
  // Build the table for the first click on the table view button.
  if not FTableDone then
  begin
    AppBusyCursor;
    jsData := nil;
    jsParser := TJSONParser.Create(FSynEdit.Text);
    try
      try
        jsData := jsParser.Parse;
      except
        on E: EJSONParser do
          jsData := nil;
      end;
      if CanEnableTable(jsData) then
      begin
        BuildTable(jsData);
        AdjustTableColumns;
      end
      else
        ViewPage := TableFallback;
    finally
      FreeAndNil(jsParser);
      if Assigned(jsData) then
        FreeAndNil(jsData);
      AppResetCursor;
    end;
  end;
end;

function TResponseJsonTab.FindInNode(Node: TTreeNode): TTreeNode;
var
  Next: TTreeNode;
  fp: TFindPos;
begin
  if Node = nil then
    Exit(nil); //=>
  fp := FindText(Node.Text, FSearchText, FSearchNodePos);
  if fp.Pos > 0 then begin
    if ssoBackwards in FSearchOptions then
    begin
      FSearchNodePos := fp.Pos - 1;
      // Don't reset search pos to 0 it will be looped.
      if FSearchNodePos = 0 then
        FSearchNodePos := 1;
    end
    else
      begin
        FSearchNodePos := fp.Pos + fp.SelLength
      end;
    Exit(Node);
  end;
  FSearchNodePos := 0;
  if ssoBackwards in FSearchOptions then
    Next := Node.GetPrev
  else
    Next := Node.GetNext;
  if Next <> nil then
    Exit(FindInNode(Next)); //=>
  Result := nil;
end;

function TResponseJsonTab.FindInTable: TSearchTable;
var
  txt: String;
  fp: TFindPos;
  Advance: Boolean;
begin
  Advance := False;
  while ((FSearchTable.Row > 0) and (FSearchTable.Row < FGrid.RowCount)) do
  begin
    txt := FGrid.Cells[FSearchTable.Col, FSearchTable.Row];
    fp := FindText(txt, FSearchText, FSearchTable.Pos);
    if (fp.Pos > 0) then
    begin
      if (ssoBackwards in FSearchOptions) then
      begin
        FSearchTable.Pos := fp.Pos - 1;
      end
      else
        FSearchTable.Pos := fp.Pos + fp.SelLength;
      Result.Row := FSearchTable.Row;
      Result.Col := FSearchTable.Col;
      Result.Pos := fp.Pos;
      if (FSearchTable.Pos = 0) or (FSearchTable.Pos > UTF8Length(txt)) then
        Advance := True // out of the cell, we need to advance a col/row and then exit.
      else
        Exit; // Match found =>
    end;
    with FSearchTable do
    begin
      Pos := 0;
      if (ssoBackwards in FSearchOptions) then
      begin
        Dec(Col);
        if (Col < 0) then
        begin
          Col := FGrid.ColCount - 1;
          Dec(Row);
        end;
      end
      else
        begin
          Inc(Col);
          if (Col > FGrid.ColCount - 1) then
          begin
            Col := 0;
            Inc(Row);
          end;
        end;
    end;
    if Advance then
      Exit; // =>
  end;
  Result.Row := -1;
  Result.Col := -1;
end;

function TResponseJsonTab.CanEnableTable(Json: TJSONData): Boolean;
begin
  Result := Assigned(Json) and (Json.JSONType = jtArray);
end;

procedure TResponseJsonTab.InternalOnTableDblClick(Sender: TObject);
begin
  with TKeyValueForm.Create(FTabSheet) do
  begin
    OnGetKeyValue := @InternalOnGetKeyValue;
    ViewNav('Json');
    Free;
  end;
end;

procedure TResponseJsonTab.InternalOnGetKeyValue(Sender: TObject;
  Increment: Integer; var AKey, AValue: string;
  var ACurrent, ATotal: Integer);
begin
  with FGrid do
  begin
    Row      := Row + Increment;
    ACurrent := Row - 1;
    ATotal   := RowCount - FixedRows;
    AKey     := Columns[Col].Title.Caption;
    AValue   := Cells[Col, Row];
  end;
end;

function TResponseJsonTab.GetTreeView: TTreeView;
begin
  if not Assigned(FTreeView) then
    raise Exception.Create('Tree view component is not initialized.');
  Result := FTreeView;
end;

function TResponseJsonTab.GetViewPage: TViewPage;
var
  aPage: TTabSheet;
begin
  aPage := FPageControl.ActivePage;
  if aPage = FTreeSheet then
    Result := vpTree
  else if aPage = FFormatSheet then
    Result := vpFormatted
  else if aPage = FTableSheet then
    Result := vpTable
  else
    raise Exception.Create('TResponseJsonTab.GetViewPage: ActivePage');
end;

constructor TResponseJsonTab.Create;
begin
  inherited;
  FName       := 'JSON';
  FTreeView   := nil;
  FJsonRoot   := nil;
  FJsonParser := nil;
  FSearchOptions := [];
  FLineNumbers := False;
  FAutoCreate  := False;
  FTableDone   := False;
  InitSearchParams;
end;

destructor TResponseJsonTab.Destroy;
begin
  ClearJsonData;
  inherited Destroy;
end;

function TResponseJsonTab.OpenOnMimeType(const MimeType: string): boolean;
begin
  Result := False;
  case MimeType of
    'application/json',
    'application/vnd.api+json':
       Result := True;
  end;
end;

procedure TResponseJsonTab.CreateUI(ATabSheet: TTabSheet);
begin
  inherited;

  CreateToolbar(ATabSheet);

  ATabSheet.OnKeyDown := @InternalOnKeyDown;

  FFilter := TInputButtons.Create(ATabSheet);
  FFilter.Parent := ATabSheet;
  FFilter.Align := alBottom;
  FFilter.ExecButton.Caption := 'Filter';
  FFilter.OnExecClick := @OnFilterClick;
  FFilter.OnResetClick := @OnFilterReset;
  FFilter.OnKeyDown := @InternalOnKeyDown;
  FFilter.Visible := False;

  FPageControl := TPageControl.Create(ATabSheet);
  with FPageControl do begin
    Parent := ATabSheet;
    Align := alClient;
    FTreeSheet := AddTabSheet;
    FFormatSheet := AddTabSheet;
    FTableSheet := AddTabSheet;
    ShowTabs := False;
  end;

  FTreeView := TTreeView.Create(FTreeSheet);
  FTreeView.Parent := FTreeSheet;
  FTreeView.Align := alClient;
  FTreeView.BorderStyle := bsNone;
  FTreeView.ReadOnly := True;
  FTreeView.RightClickSelect := True;
  FTreeView.ScrollBars := ssAutoBoth;
  FTreeView.ToolTips := False;
  FTreeView.OnKeyDown := @InternalOnKeyDown;

  FSynEdit := TSynEdit.Create(FFormatSheet);
  FSynEdit.Parent := FFormatSheet;
  FSynEdit.Align := alClient;
  FSynEdit.BorderStyle := bsNone;
  FSynEdit.ReadOnly := True;
  FSynEdit.OnKeyDown := @InternalOnKeyDown;

  // Hide all the gutters except code folding.
  FSynEdit.Gutter.Parts.Part[0].Visible := False;
  FSynEdit.Gutter.Parts.Part[1].Visible := False;
  FSynEdit.Gutter.Parts.Part[2].Visible := False;
  FSynEdit.Gutter.Parts.Part[3].Visible := False;

  FGrid := TStringGrid.Create(FTableSheet);
  with FGrid do
    begin
      Parent := FTableSheet;
      Align := alClient;
      BorderStyle := bsNone;
      TitleStyle := tsNative;
      FixedCols := 0;
      FixedRows := 1;
      Options := [goColSizing, goRowHighlight, goTabs, goFixedVertLine,
              goFixedHorzLine, goHorzLine, goVertLine, goSmoothScroll,
              goDrawFocusSelected];
      MouseWheelOption := mwGrid;
      ColumnClickSorts := True;
      OnKeyDown := @InternalOnKeyDown;
      OnDblClick := @InternalOnTableDblClick;
    end;

  ShowLineNumbers;
end;

procedure TResponseJsonTab.OnHttpResponse(ResponseInfo: TResponseInfo);
begin
  inherited;
  if Assigned(FTreeView) then begin
    ClearJsonData;
    LoadDocument(ResponseInfo.Content.DataString);
  end;
end;

procedure TResponseJsonTab.FreeTab;
begin
  ClearJsonData;
  ClearTable;
  FreeAndNil(FGrid);
  FreeAndNil(FTreeView);
  inherited;
end;

procedure TResponseJsonTab.Filter(Node: TTreeNode);
var
  FilterList: TStringList;
  childJson, parentJson: TJSONData;
  I: Integer;
  Key: string;
begin

  AppBusyCursor;
  FilterList := TStringList.Create;
  FilterList.LineBreak := '';

  // Find out a node path.
  try
    while Assigned(Node.Parent) do
    begin
      Application.ProcessMessages;
      childJson  := TJSONData(Node.Data);
      parentJson := TJSONData(Node.Parent.Data);
      Key := '';
      case parentJson.JSONType of
        jtObject:
          begin
            I := TJSONObject(parentJson).IndexOf(childJson);
            if I >= 0 then
              Key := '.' + TJSONObject(parentJson).Names[I];
          end;
        jtArray:
          begin
            I := TJSONArray(parentJson).IndexOf(childJson);
            if I >= 0 then
              Key := '[' + IntToStr(I) + ']';
          end;
      end;
      if Key <> '' then
        FilterList.Insert(0, Key);
      Node := Node.Parent;
    end;

    if FilterList.Count > 0 then begin
      if FFilter.Text.Trim.IsEmpty then
        FFilter.Text := FilterList.Text
      else
        // Already filtered, chain next filter.
        FFilter.Text := FFilter.Text + FilterList.Text;
      if not FFilter.Visible then
        ToggleFilterPanel;
      ApplyFilter;
      FFilter.AddHistory;
    end;

  finally
    AppResetCursor;
    FilterList.Free;
  end;
end;

function TResponseJsonTab.IsFilterActive: Boolean;
begin
  Result := FFilter.Visible and (Trim(FFilter.Text) <> '');
end;

function TResponseJsonTab.FindNext: Integer;
var
  tbl: TSearchTable;
begin
  if (FSearchNode = nil) and (FTreeView.Items.Count > 0) then
    if ssoBackwards in FSearchOptions then begin
      FSearchNode := FTreeView.BottomItem;
      // When the tree is collapsed the bottom item will be last collapsed item
      // and we should expand nodes until no children found.
      while FSearchNode.HasChildren do
        FSearchNode := FSearchNode.GetLastChild;
    end
    else
      FSearchNode := FTreeView.Items.GetFirstNode.GetNext;
  FSearchNode := FindInNode(FSearchNode);
  if FSearchNode <> nil then
    FSearchNode.Selected := True;
  // Now try searching in the table.
  if FTableDone then
  begin
    tbl := FindInTable;
    if (tbl.Row <> -1) and (tbl.Col <> -1) then
    begin
      FGrid.Row := tbl.Row;
      FGrid.Col := tbl.Col;
    end;
  end;
  Result := inherited;
end;

procedure TResponseJsonTab.ViewNextPage;
begin
  if ViewPage = vpTree then
    ViewPage := vpFormatted
  else
    if ViewPage = vpFormatted then
    begin
      // if canViewTable then viewPage := vpTable
      ViewPage := vpTable;
    end
  else
    if ViewPage = vpTable then
      ViewPage := vpTree;
end;

procedure TResponseJsonTab.ExpandChildren(Node: TTreeNode; Collapse: Boolean = False);
var
  N: Integer;
begin
  if not Node.HasChildren then
    Exit;
  // Expand root node.
  if (not Node.Expanded) and (not Collapse) then
    Node.Expanded := True;
  // Expand/collapse child nodes.
  for N := 0 to Node.Count - 1 do
    if Collapse then
      Node.Items[N].Collapse(True)
    else
      Node.Items[N].Expand(True);
end;

function TResponseJsonTab.SelectedText: string;
var
  Sel: TTreeNode;
  key: string;
begin
  if ViewPage = vpFormatted then
    Exit(inherited);
  key := '';
  Sel := FTreeView.Selected;
  if Sel <> Nil then begin
    // Use a node key as the selected value.
    case TJSONData(Sel.Data).JSONType of
      jtNumber,
      jtString,
      jtBoolean: key := LeftStr(Sel.Text, Pos(':', Sel.Text) - 1);
      jtNull:    key := Sel.Text;
      jtArray:   key := LeftStr(Sel.Text, Pos('[', Sel.Text) - 1);
      jtObject:  key := Sel.Text;
    end;
  end;
  Result := Key;
end;

{ TResponseTabManager }

function TResponseTabManager.GetActiveTab: TResponseTab;
var
  Tab: pointer;
begin
  for Tab in FOpenedTabs do
    if FPageControl.ActivePage = TResponseTab(Tab).TabSheet then
      Exit(TResponseTab(Tab));
end;

constructor TResponseTabManager.Create(APageControl: TPageControl);
begin
  FPageControl := APageControl;
  FTabs := TFPList.Create;
  FOpenedTabs := TFPList.Create;
  FOnOpenResponseTab := nil;
  FOnSaveTab := nil;
end;

destructor TResponseTabManager.Destroy;
var
  I: Integer;
begin
  for I := 0 to FTabs.Count - 1 do
    TResponseTab(FTabs.Items[I]).Free;
  FreeAndNil(FTabs);
  FreeAndNil(FOpenedTabs);
  FreeAndNil(FPageControl);
  inherited Destroy;
end;

procedure TResponseTabManager.RegisterTab(Tab: TResponseTab);
begin
  FTabs.Add(Tab);
end;

procedure TResponseTabManager.OpenTabs(ResponseInfo: TResponseInfo);
var
  I: integer;
  Tab: TResponseTab;
begin
  FOpenedTabs.Clear;
  for I := 0 to FTabs.Count - 1 do
  begin
    Tab := TResponseTab(FTabs.Items[I]);
    if Tab.OpenOnMimeType(ResponseInfo.ContentType) then
    begin
      if not Assigned(Tab.TabSheet) then
        Tab.CreateUI(FPageControl.AddTabSheet);
      try
        Tab.OnHttpResponse(ResponseInfo);
        if Assigned(FOnOpenResponseTab) then
          FOnOpenResponseTab(Tab, ResponseInfo);
        FOpenedTabs.Add(Tab);
      except on E: Exception do
        begin
          // On error don't open the tab at all.
          Tab.FreeTab;
          raise ETabException.Create(Tab.Name, E.Message);
        end;
      end; // try
    end
    else
      Tab.FreeTab;
  end;
end;

procedure TResponseTabManager.Save(const FileName: string);
var
  Tab: Pointer;
begin
  for Tab in FOpenedTabs do
    if Assigned(FOnSaveTab) then
      FOnSaveTab(FileName, TResponseTab(Tab));
end;

procedure TResponseTabManager.CloseTabs;
var
  Tab: Pointer;
begin
  for Tab in FOpenedTabs do
    TResponseTab(Tab).FreeTab;
  FOpenedTabs.Clear;
end;

function TResponseTabManager.CanFind: TResponseTab;
var
  Tab: Pointer;
  rt: TResponseTab;
begin
  Result := nil;
  for Tab in FOpenedTabs do begin
    rt := TResponseTab(Tab);
    if rt.CanFind then
      Exit(rt);
  end;
end;

{ TResponseImageTab }

function TResponseImageTab.GetImage: TImage;
begin
  if not Assigned(FImage) then
    raise Exception.Create('Image component is not initialized.');
  Result := FImage;
end;

procedure TResponseImageTab.OnDblClickResize(Sender: TObject);
begin
  ResizeImage(not FImage.Stretch);
end;

procedure TResponseImageTab.ResizeImage(ToStretch: boolean);
begin
  with FImage do
    if ToStretch then
    begin
      Width := Parent.ClientWidth;
      Height := Parent.ClientHeight;
      Proportional := True;
      Stretch := True;
    end
    else
    begin
      Width := Picture.Width;
      Height := Picture.Height;
      Proportional := False;
      Stretch := False;
    end;
end;

function TResponseImageTab.ParseImageType(const ContentType: string): string;
var
  P: integer;
begin
  P := Pos('/', ContentType);
  Result := UpperCase(RightStr(ContentType, Length(ContentType) - P));
  if Result = 'JPEG' then
    Result := 'JPG';
end;

constructor TResponseImageTab.Create;
begin
  inherited;
  FName := 'Image';
  FImage := nil;
end;

destructor TResponseImageTab.Destroy;
begin
  inherited Destroy;
end;

function TResponseImageTab.OpenOnMimeType(const MimeType: string): boolean;
begin
  case MimeType of
    'image/jpeg',
    'image/jpg',
    'image/png',
    'image/gif':
      Result := True;
    else
      Result := False;
  end;
end;

procedure TResponseImageTab.CreateUI(ATabSheet: TTabSheet);
var
  sb: TScrollBox;
begin
  inherited;
  sb := TScrollBox.Create(ATabSheet);
  sb.Parent := ATabSheet;
  sb.Align := alClient;
  FImage := TImage.Create(sb);
  FImage.Parent := sb;
  FImage.OnDblClick := @OnDblClickResize;
end;

procedure TResponseImageTab.OnHttpResponse(ResponseInfo: TResponseInfo);
begin
  if Assigned(FImage) then
  begin
    FImageType := ParseImageType(ResponseInfo.ContentType);
    FImage.Picture.LoadFromStream(ResponseInfo.Content);
    ResizeImage(False);
  end;
end;

procedure TResponseImageTab.Save(const AFileName: string);
begin
  if Assigned(FImage) then
    FImage.Picture.SaveToFile(AFileName);
end;

{ TResponseTab }

constructor TResponseTab.Create;
begin

end;

destructor TResponseTab.Destroy;
begin
  FreeTab;
  inherited Destroy;
end;

procedure TResponseTab.CreateUI(ATabSheet: TTabSheet);
begin
  FTabSheet := ATabSheet;
  FTabSheet.Caption := FName;
end;

procedure TResponseTab.FreeTab;
begin
  if Assigned(FTabSheet) then
    FreeAndNil(FTabSheet);
end;

procedure TResponseTab.Save(const AFileName: string);
begin
  raise Exception.Create('Save is not implemented.');
end;

function TResponseTab.CanFind: Boolean;
begin
  Result := False;
end;

procedure TResponseTab.InitSearch(Search: string; Options: TFindOptions);
begin
  raise Exception.Create('Tab does not support search');
end;

function TResponseTab.FindNext: Integer;
begin
  raise Exception.Create('Tab does not support search');
end;

end.
