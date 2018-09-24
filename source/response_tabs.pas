unit response_tabs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, json_parser, ComCtrls, ExtCtrls, Controls, Forms,
  StdCtrls, Dialogs, SynEdit, SynEditTypes, thread_http_client, inputbuttons;

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
    procedure CloseTab; virtual;
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
  end;

  { TResponseImageTab }

  TResponseImageTab = class(TResponseTab)
  private
    FImage: TImage;
    FImageType: string;
    function GetImage: TImage;
    procedure OnDblClickResize(Sender: TObject);
    procedure ResizeImage(ToStretch: boolean);
    function ParseImageType(const ContentType: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    function OpenOnMimeType(const MimeType: string): boolean; override;
    procedure CreateUI(ATabSheet: TTabSheet); override;
    procedure OnHttpResponse(ResponseInfo: TResponseInfo); override;
    procedure Save(const AFileName: string); override;
    property Image: TImage read GetImage;
    property ImageType: string read FImageType;
  end;

  { TViewPage }

  TViewPage = (vpTree, vpFormatted);

  { TOnJsonFormat }

  TOnJsonFormat = procedure (JsonData: TJSONData; Editor: TSynEdit) of object;

  { TOnJsonData }

  TOnJsonData = procedure (RootJson, FilteredJson: TJSONData) of object;

  { TResponseJsonTab }

  TResponseJsonTab = class(TResponseTab)
  private
    FTreeView: TTreeView;
    FJsonRoot: TJSONData;
    FJsonParser: TJSONParser;
    FBtnView: TToolButton;
    FBtnOptions: TToolButton;
    FBtnFilter: TToolButton;
    FPageControl: TPageControl;
    FTreeSheet: TTabSheet;
    FFormatSheet: TTabSheet;
    FSynEdit: TSynEdit;
    FFilter: TInputButtons;
    FOnJsonFormat: TOnJsonFormat;
    FTreeExpanded: Boolean;
    FOnJsonData: TOnJsonData;
    FSearchText: string;
    FSearchOptions: TSynSearchOptions;
    FSearchNode: TTreeNode;
    FSearchNodePos: Integer;
    FSearchPos: TPoint;
    function GetTreeView: TTreeView;
    function GetViewPage: TViewPage;
    procedure LoadDocument(doc: string);
    procedure SetOnJsonFormat(AValue: TOnJsonFormat);
    procedure SetViewPage(AValue: TViewPage);
    procedure ShowJsonData(AParent: TTreeNode; Data: TJSONData);
    procedure ClearJsonData;
    procedure CreateToolbar(Parent: TWinControl);
    procedure ApplyFilter;
    procedure BuildTree(JsonData: TJSONData);
    procedure SetFormattedText(JsonData: TJSONData);
    procedure InternalOnSwitchFilter(Sender: TObject);
    procedure OnChangeTreeMode(Sender: TObject);
    procedure OnChangeFormatMode(Sender: TObject);
    procedure OnFilterClick(Sender: TObject);
    procedure OnFilterReset(Sender: TObject);
    procedure InternalOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure ToggleFilterPanel;
    function FindInNode(Node: TTreeNode): TTreeNode;
  public
    constructor Create;
    destructor Destroy; override;
    function OpenOnMimeType(const MimeType: string): boolean; override;
    procedure CreateUI(ATabSheet: TTabSheet); override;
    procedure OnHttpResponse(ResponseInfo: TResponseInfo); override;
    procedure FreeTab; override;
    procedure Filter(Node: TTreeNode); virtual;
    function IsFilterActive: Boolean;
    function CanFind: Boolean; override;
    procedure InitSearch(Search: string; Options: TFindOptions); override;
    function FindNext: Integer; override;
    property TreeView: TTreeView read GetTreeView;
    property SynEdit: TSynEdit read FSynEdit;
    property JsonRoot: TJSONData read FJsonRoot;
    property ViewPage: TViewPage read GetViewPage write SetViewPage;
    property ButtonOptions: TToolButton read FBtnOptions;
    property TreeExpanded: Boolean read FTreeExpanded write FTreeExpanded default True;
    property OnJsonFormat: TOnJsonFormat read FOnJsonFormat write SetOnJsonFormat;
    property OnJsonData: TOnJsonData read FOnJsonData write FOnJsonData;
  end;

implementation

uses Menus, app_helpers, strutils;

const
  ImageTypeMap: array[TJSONtype] of Integer =
  // (jtUnknown, jtNumber, jtString, jtBoolean, jtNull, jtArray, jtObject)
  (-1, 3, 2, 4, 5, 0, 1);

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
      FBtnView.Caption := 'Tree';
    end;
    vpFormatted: begin
      FPageControl.ActivePage := FFormatSheet;
      FBtnView.Caption := 'Formatted';
    end;
  end;
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

procedure TResponseJsonTab.CreateToolbar(Parent: TWinControl);
var
  Toolbar: TToolBar;
  pm: TPopupMenu;
  itemTree: TMenuItem;
  itemFmt: TMenuItem;
begin
  Toolbar := TToolBar.Create(Parent);
  Toolbar.Parent := Parent;
  Toolbar.EdgeBorders := [];
  Toolbar.ShowCaptions := True;
  pm := TPopupMenu.Create(Parent);
  pm.Parent := Parent;
  itemTree := TMenuItem.Create(pm);
  itemTree.Caption := 'Tree';
  itemTree.OnClick := @OnChangeTreeMode;
  itemFmt := TMenuItem.Create(pm);
  itemFmt.Caption := 'Formatted';
  itemFmt.OnClick := @OnChangeFormatMode;
  pm.Items.Add(itemTree);
  pm.Items.Add(itemFmt);
  FBtnView := TToolButton.Create(Toolbar);
  with FBtnView do begin
    Parent := Toolbar;
    Style := tbsButtonDrop;
    DropdownMenu := pm;
  end;
  FBtnOptions := TToolButton.Create(Toolbar);
  FBtnOptions.Parent := Toolbar;
  FBtnOptions.Caption := 'Options';
  FBtnFilter := TToolButton.Create(Toolbar);
  with FBtnFilter do begin
    Parent  := Toolbar;
    Caption := 'Filter';
    Style   := tbsCheck;
    OnClick := @InternalOnSwitchFilter;
  end;
end;

procedure TResponseJsonTab.ApplyFilter;
var
  Filtered: TJSONData;
begin
  if not IsFilterActive then
    Filtered := FJsonRoot
  else
    Filtered := FJsonRoot.FindPath(FFilter.Text);

  if Assigned(FOnJsonData) then
    FOnJsonData(FJsonRoot, Filtered);

  if Assigned(Filtered) then begin
    SetFormattedText(Filtered);
    BuildTree(Filtered);
  end
  else
    FSynEdit.Text := '';
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

procedure TResponseJsonTab.SetFormattedText(JsonData: TJSONData);
begin
  if Assigned(FOnJsonFormat) then
    FOnJsonFormat(JsonData, FSynEdit)
  else
    FSynEdit.Text := JsonData.FormatJSON;
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

procedure TResponseJsonTab.OnFilterClick(Sender: TObject);
begin
  ApplyFilter;
end;

procedure TResponseJsonTab.OnFilterReset(Sender: TObject);
begin
  FFilter.Visible := False;
  FBtnFilter.Down := False;
  ApplyFilter;
end;

procedure TResponseJsonTab.InternalOnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Control-E show/hide filter panel.
  if (Shift = [ssCtrl]) and (Key = 69) then
     InternalOnSwitchFilter(Sender);
end;

procedure TResponseJsonTab.ToggleFilterPanel;
begin
  FFilter.Visible := not FFilter.Visible;
  FBtnFilter.Down := FFilter.Visible;
  if FFilter.Visible then
    FFilter.SetFocus;
end;

function TResponseJsonTab.FindInNode(Node: TTreeNode): TTreeNode;
var
  Next: TTreeNode;
  fp: TFindPos;
  Opts: TFindOptions;
begin
  if Node = nil then
    Exit(nil); //=>
  Opts := [];
  if ssoMatchCase in FSearchOptions then
    Include(Opts, frMatchCase);
  if ssoWholeWord in FSearchOptions then
    Include(Opts, frWholeWord);
  if not (ssoBackwards in FSearchOptions) then
    Include(Opts, frDown);
  fp := FindInText(Node.Text, FSearchText, Opts, FSearchNodePos);
  if fp.Pos > 0 then begin
    if frDown in Opts then
      FSearchNodePos := fp.Pos + fp.SelLength
    else begin
      FSearchNodePos := fp.Pos - 1;
      // Don't reset search pos to 0 it will be looped.
      if FSearchNodePos = 0 then
        FSearchNodePos := 1;
    end;
    Exit(Node);
  end;
  FSearchNodePos := 0;
  if frDown in Opts then
    Next := Node.GetNext
  else
    Next := Node.GetPrev;
  if Next <> nil then
    Exit(FindInNode(Next)); //=>
  Result := nil;
end;

function TResponseJsonTab.GetTreeView: TTreeView;
begin
  if not Assigned(FTreeView) then
    raise Exception.Create('Tree view component is not initialized.');
  Result := FTreeView;
end;

function TResponseJsonTab.GetViewPage: TViewPage;
begin
  if FPageControl.ActivePage = FTreeSheet then
    Result := vpTree
  else
    Result := vpFormatted;
end;

constructor TResponseJsonTab.Create;
begin
  inherited;
  FName       := 'JSON';
  FTreeView   := nil;
  FJsonRoot   := nil;
  FJsonParser := nil;
end;

destructor TResponseJsonTab.Destroy;
begin
  ClearJsonData;
  inherited Destroy;
end;

function TResponseJsonTab.OpenOnMimeType(const MimeType: string): boolean;
begin
  Result := MimeType = 'application/json';
end;

procedure TResponseJsonTab.CreateUI(ATabSheet: TTabSheet);
begin
  inherited CreateUI(ATabSheet);

  CreateToolbar(ATabSheet);

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
end;

procedure TResponseJsonTab.OnHttpResponse(ResponseInfo: TResponseInfo);
begin
  if Assigned(FTreeView) then begin
    ClearJsonData;
    LoadDocument(ResponseInfo.Content.DataString);
  end;
end;

procedure TResponseJsonTab.FreeTab;
begin
  ClearJsonData;
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

  FilterList := TStringList.Create;
  FilterList.LineBreak := '';

  try
    while Assigned(Node.Parent) do begin
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
      FFilter.Text := FilterList.Text;
      if not FFilter.Visible then
        ToggleFilterPanel;
      ApplyFilter;
      FFilter.AddHistory;
    end;

  finally
    FilterList.Free;
  end;
end;

function TResponseJsonTab.IsFilterActive: Boolean;
begin
  Result := FFilter.Visible and (Trim(FFilter.Text) <> '');
end;

function TResponseJsonTab.CanFind: Boolean;
begin
  Result := True;
end;

procedure TResponseJsonTab.InitSearch(Search: string; Options: TFindOptions);
begin
  FSearchText := Search;
  FSearchOptions := [];
  FSearchNodePos := 0;
  FSearchNode := nil;
  FSearchPos := Point(0, 0);
  if not (frDown in Options) then begin
    Include(FSearchOptions, ssoBackwards);
    FSearchPos.y := FSynEdit.Lines.Count;
    FSearchPos.x := FSynEdit.Lines[FSearchPos.y - 1].Length;
  end;
  if frMatchCase in Options then
    Include(FSearchOptions, ssoMatchCase);
  if frWholeWord in Options then
    Include(FSearchOptions, ssoWholeWord);
end;

function TResponseJsonTab.FindNext: Integer;
var
  p, maxx, maxy: Integer;
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
    if (FSearchPos.x = maxx) and (FSearchPos.y = maxy) and (FSearchNode = nil) then
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

{ TResponseTabManager }

constructor TResponseTabManager.Create(APageControl: TPageControl);
begin
  FPageControl := APageControl;
  FTabs := TFPList.Create;
  FOpenedTabs := TFPList.Create;
  FOnOpenResponseTab := nil;
  FOnSaveTab := nil;
end;

destructor TResponseTabManager.Destroy;
begin
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
      Tab.OnHttpResponse(ResponseInfo);
      if Assigned(FOnOpenResponseTab) then
        FOnOpenResponseTab(Tab, ResponseInfo);
      FOpenedTabs.Add(Tab);
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
    TResponseTab(Tab).CloseTab;
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
  inherited Destroy;
end;

procedure TResponseTab.CreateUI(ATabSheet: TTabSheet);
begin
  FTabSheet := ATabSheet;
  FTabSheet.Caption := FName;
end;

procedure TResponseTab.FreeTab;
begin
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

procedure TResponseTab.CloseTab;
begin
  FreeAndNil(FTabSheet);
end;

end.
