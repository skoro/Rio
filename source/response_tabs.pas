unit response_tabs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, fpjson, ComCtrls, ExtCtrls, Controls, Forms,
  StdCtrls, thread_http_client;

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
    procedure OnSaveResponse(const AFileName: string); virtual; abstract;
    property Name: string read FName;
    property TabSheet: TTabSheet read FTabSheet;
  end;

  { TResponseTabManager }

  TResponseTabManager = class
  private
    FPageControl: TPageControl;
    FTabs: TObjectList;
  public
    constructor Create(APageControl: TPageControl);
    destructor Destroy; override;
    procedure RegisterTab(Tab: TResponseTab);
    procedure OpenTabs(ResponseInfo: TResponseInfo);
    property PageControl: TPageControl read FPageControl write FPageControl;
  end;

  { TResponseImageTab }

  TResponseImageTab = class(TResponseTab)
  private
    FImage: TImage;
    function GetImage: TImage;
    procedure OnDblClickResize(Sender: TObject);
    procedure ResizeImage(ToStretch: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function OpenOnMimeType(const MimeType: string): boolean; override;
    procedure CreateUI(ATabSheet: TTabSheet); override;
    procedure OnHttpResponse(ResponseInfo: TResponseInfo); override;
    procedure OnSaveResponse(const AFileName: string); override;
    property Image: TImage read GetImage;
  end;

  { TResponseJsonTab }

  TResponseJsonTab = class(TResponseTab)
  private
    FTreeView: TTreeView;
    FJsonRoot: TJSONData;
    function GetTreeView: TTreeView;
    procedure LoadDocument(doc: string);
  public
    constructor Create;
    destructor Destroy; override;
    function OpenOnMimeType(const MimeType: string): boolean; override;
    procedure CreateUI(ATabSheet: TTabSheet); override;
    procedure OnHttpResponse(ResponseInfo: TResponseInfo); override;
    procedure OnSaveResponse(const AFileName: string); override;
    property TreeView: TTreeView read GetTreeView;
  end;

implementation


{ TResponseJsonTab }

procedure TResponseJsonTab.LoadDocument(doc: string);
begin

end;

function TResponseJsonTab.GetTreeView: TTreeView;
begin
  if not Assigned(FTreeView) then
    raise Exception.Create('Tree view component is not initialized.');
  Result := FTreeView;
end;

constructor TResponseJsonTab.Create;
begin
  inherited;
  FTreeView := nil;
  FJsonRoot := nil;
end;

destructor TResponseJsonTab.Destroy;
begin
  inherited Destroy;
end;

function TResponseJsonTab.OpenOnMimeType(const MimeType: string): boolean;
begin
  Result := MimeType = 'application/json';
end;

procedure TResponseJsonTab.CreateUI(ATabSheet: TTabSheet);
begin
  inherited CreateUI(ATabSheet);
  FTreeView := TTreeView.Create(ATabSheet);
  FTreeView.Parent := ATabSheet;
  FTreeView.Align := alClient;
  FTreeView.BorderStyle := bsNone;
  FTreeView.ReadOnly := True;
  FTreeView.RightClickSelect := True;
  FTreeView.ScrollBars := ssAutoBoth;
end;

procedure TResponseJsonTab.OnHttpResponse(ResponseInfo: TResponseInfo);
begin
  if Assigned(FTreeView) then begin
    LoadDocument(ResponseInfo.Content.DataString);
  end;
end;

procedure TResponseJsonTab.OnSaveResponse(const AFileName: string);
begin
  if Assigned(FTreeView) then begin

  end;
end;

{ TResponseTabManager }

constructor TResponseTabManager.Create(APageControl: TPageControl);
begin
  FPageControl := APageControl;
  FTabs := TObjectList.Create;
end;

destructor TResponseTabManager.Destroy;
begin
  FreeAndNil(FPageControl);
  FreeAndNil(FTabs);
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
  for I := 0 to FTabs.Count - 1 do
  begin
    Tab := TResponseTab(FTabs.Items[I]);
    if Tab.OpenOnMimeType(ResponseInfo.ContentType) then
    begin
      if not Assigned(Tab.TabSheet) then
        Tab.CreateUI(FPageControl.AddTabSheet);
      Tab.OnHttpResponse(ResponseInfo);
    end
    else
      Tab.FreeTab;
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

constructor TResponseImageTab.Create;
begin
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
    FImage.Picture.LoadFromStream(ResponseInfo.Content);
    ResizeImage(False);
  end;
end;

procedure TResponseImageTab.OnSaveResponse(const AFileName: string);
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

end.
