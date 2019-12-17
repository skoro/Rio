unit AppTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, ExtCtrls, Requests;

type

  { TAppTreeManager }

  TAppTreeManager = class(TPanel)
  private
    FTreeView: TTreeView;
    FBookmarkRoot: TTreeNode;
    FBookmarkSelected: TTreeNode;
    FBookmarkManager: TBookmarkManager;
    FBookmarkPopup: TBookmarkPopup;
  protected
    function CreateTreeView: TTreeView; virtual;
    function CreateBookmarkNode: TTreeNode; virtual;
    procedure InitTree; virtual;
    function IsBookmarkNode(ANode: TTreeNode): Boolean;
    procedure InternalTreeOnDblClick(Sender: TObject);
    procedure InternalTreeOnContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure InternalTreeOnSelectionChanged(Sender: TObject);
    procedure InternalTreePopupOnClose(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property TreeView: TTreeView read FTreeView;
    property BookmarkManager: TBookmarkManager read FBookmarkManager;
    property BookmarkRoot: TTreeNode read FBookmarkRoot;
    property BookmarkSelected: TTreeNode read FBookmarkSelected;
    property BookmarkPopup: TBookmarkPopup read FBookmarkPopup write FBookmarkPopup;
  end;

implementation

uses Controls, StdCtrls;

{ TAppTreeManager }

function TAppTreeManager.CreateTreeView: TTreeView;
begin
  Result := TTreeView.Create(Self);
  Result.Parent := Self;
  Result.Align := alClient;
  Result.ScrollBars := ssAutoBoth;
  Result.Options := [tvoReadOnly, tvoShowRoot, tvoShowLines, tvoShowButtons,
                 tvoAutoItemHeight, tvoKeepCollapsedNodes, tvoRightClickSelect];
  Result.ToolTips := True;
  // Event handlers.
  Result.OnDblClick := @InternalTreeOnDblClick;
  Result.OnContextPopup := @InternalTreeOnContextPopup;
  Result.OnSelectionChanged := @InternalTreeOnSelectionChanged;
end;

function TAppTreeManager.CreateBookmarkNode: TTreeNode;
begin
  with FTreeView.Items do begin
    Result := Add(NIL, 'My bookmarks');
    Result.Data := NIL;
    //FRootNode.StateIndex := FImgIdxRoot;
    Result.MakeVisible;
  end;
end;

procedure TAppTreeManager.InitTree;
begin
  FTreeView := CreateTreeView;
  FBookmarkRoot := CreateBookmarkNode;
end;

function TAppTreeManager.IsBookmarkNode(ANode: TTreeNode): Boolean;
begin
  while ANode.Parent <> NIL do
    ANode := ANode.Parent;
  Result := (ANode = FBookmarkRoot);
end;

procedure TAppTreeManager.InternalTreeOnDblClick(Sender: TObject);
var
  aNode: TTreeNode;
begin
  aNode := FTreeView.Selected;
  if not Assigned(aNode) then
    Exit; // =>
  if IsBookmarkNode(aNode) then
    FBookmarkManager.OpenNode(aNode);
end;

procedure TAppTreeManager.InternalTreeOnContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  aNode: TTreeNode;
begin
  aNode := FTreeView.GetNodeAt(MousePos.x, MousePos.y);
  if not Assigned(aNode) then
    Exit; // =>
  if IsBookmarkNode(aNode) then
  begin
    FTreeView.PopupMenu := FBookmarkPopup;
    FTreeView.PopupMenu.OnClose := @InternalTreePopupOnClose;
  end;
end;

procedure TAppTreeManager.InternalTreeOnSelectionChanged(Sender: TObject);
var
  aNode: TTreeNode;
begin
  aNode := FTreeView.Selected;
  FBookmarkSelected := NIL;
  if Assigned(aNode) then
  begin
    if IsBookmarkNode(aNode) then
      FBookmarkSelected := aNode;
  end;
  if Assigned(FBookmarkPopup) then
    FBookmarkPopup.SelectedNode := FBookmarkSelected;
end;

procedure TAppTreeManager.InternalTreePopupOnClose(Sender: TObject);
begin
  FTreeView.PopupMenu := nil;
end;

constructor TAppTreeManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Align := alClient;
  Caption := '';
  BevelOuter := bvNone;
  BorderSpacing.Left := 4;
  InitTree;
  FBookmarkManager := TBookmarkManager.Create(FBookmarkRoot);
  FBookmarkPopup := TBookmarkPopup.Create(Self);
  FBookmarkPopup.BookmarkManager := FBookmarkManager;
  FBookmarkPopup.RootNode := FBookmarkRoot;
end;

destructor TAppTreeManager.Destroy;
begin
  if Assigned(FBookmarkPopup) then
    FreeAndNil(FBookmarkPopup);
  FreeAndNil(FBookmarkManager);
  FreeAndNil(FTreeView);
  inherited Destroy;
end;

end.

