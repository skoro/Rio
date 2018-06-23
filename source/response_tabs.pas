unit response_tabs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, ComCtrls, ExtCtrls, Controls,
  Forms, StdCtrls, SynEdit, thread_http_client;

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
    property PageControl: TPageControl read FPageControl write FPageControl;
    property OnOpenResponseTab: TOnOpenResponseTab read FOnOpenResponseTab write FOnOpenResponseTab;
    property OnSaveTab: TOnSaveTab read FOnSaveTab write FOnSaveTab;
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

  { TResponseJsonTab }

  TResponseJsonTab = class(TResponseTab)
  private
    FTreeView: TTreeView;
    FJsonRoot: TJSONData;
    FJsonParser: TJSONParser;
    FBtnView: TToolButton;
    FPageControl: TPageControl;
    FTreeSheet: TTabSheet;
    FFormatSheet: TTabSheet;
    FSynEdit: TSynEdit;
    function GetTreeView: TTreeView;
    function GetViewPage: TViewPage;
    procedure LoadDocument(doc: string);
    procedure SetViewPage(AValue: TViewPage);
    procedure ShowJsonData(AParent: TTreeNode; Data: TJSONData);
    procedure ClearJsonData;
    procedure CreateToolbar(Parent: TWinControl);
    procedure OnChangeTreeMode(Sender: TObject);
    procedure OnChangeFormatMode(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function OpenOnMimeType(const MimeType: string): boolean; override;
    procedure CreateUI(ATabSheet: TTabSheet); override;
    procedure OnHttpResponse(ResponseInfo: TResponseInfo); override;
    procedure FreeTab; override;
    property TreeView: TTreeView read GetTreeView;
    property SynEdit: TSynEdit read FSynEdit;
    property JsonRoot: TJSONData read FJsonRoot;
    property ViewPage: TViewPage read GetViewPage write SetViewPage;
  end;

implementation

uses Menus;

const
  ImageTypeMap: array[TJSONtype] of Integer =
  // (jtUnknown, jtNumber, jtString, jtBoolean, jtNull, jtArray, jtObject)
  (-1, 3, 2, 4, 5, 0, 1);

{ TResponseJsonTab }

procedure TResponseJsonTab.LoadDocument(doc: string);
var
  S: TStringStream;
begin
  FSynEdit.Text := doc;
  S := TStringStream.Create(doc);
  FJsonParser := TJSONParser.Create(S);
  with FTreeView.Items do begin
    BeginUpdate;
    try
      FJsonRoot := FJsonParser.Parse;
      ShowJsonData(nil, FJsonRoot);
      with FTreeView do
        if (Items.Count > 0) and Assigned(Items[0]) then begin
          Items[0].Expand(False);
          Selected := Items[0];
        end;
    finally
      EndUpdate;
    end;
  end;
  FreeAndNil(S);
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
    if Data.JSONType = jtNumber then
      C := Data.AsFloat.ToString
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
end;

procedure TResponseJsonTab.OnChangeTreeMode(Sender: TObject);
begin
  SetViewPage(vpTree);
end;

procedure TResponseJsonTab.OnChangeFormatMode(Sender: TObject);
begin
  SetViewPage(vpFormatted);
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

  FSynEdit := TSynEdit.Create(FFormatSheet);
  FSynEdit.Parent := FFormatSheet;
  FSynEdit.Align := alClient;
  FSynEdit.BorderStyle := bsNone;
  FSynEdit.ReadOnly := True;

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

end.
