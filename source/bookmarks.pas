unit bookmarks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, ExtCtrls, Controls, request_object;

type
  // Forward declaration.
  TBookmark = class;

  // Callback when a new folder is created.
  TBookmarkNewFolder = procedure (Sender: TObject; FolderPath: string) of object;

  // When bookmark selected in the tree.
  TOnChangeBookmark = procedure (Previous, Selected: TBookmark) of object;

  { EFolderNode }

  EFolderNode = class(Exception)
  public
    constructor CreateDef;
  end;

  { TBookmark }

  TBookmark = class
  private
    FName: string;
    FRequest: TRequestObject;
    FTreeNode: TTreeNode;
    procedure SetName(AValue: string);
    procedure SetTreeNode(AValue: TTreeNode);
  public
    constructor Create(aName: string);
    destructor Destroy; override;
    property Name: string read FName write SetName;
    property Request: TRequestObject read FRequest write FRequest;
    property TreeNode: TTreeNode read FTreeNode write SetTreeNode;
  end;

  { TBookmarkManager }

  TBookmarkManager = class(TPanel)
  private
    FTreeView: TTreeView;
    FRootNode: TTreeNode;
    FCurrentNode: TTreeNode;
    FOnChangeBookmark: TOnChangeBookmark;

    function GetCurrentBookmark: TBookmark;
    function GetRootName: string;
    procedure SetRootName(AValue: string);

  protected
    function CreateTree: TTreeView; virtual;
    procedure CreateRootNode; virtual;
    // TreeView double click event handler.
    procedure InternalTreeOnDblClick(Sender: TObject); virtual;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destory;
    function NewFolder(FolderName: string; Edit: Boolean = True): TTreeNode;
    // Add a bookmark to the tree.
    // BM - the bookmark instance.
    // FolderPath - the bookmark folder path separated by /
    function AddBookmark(BM: TBookmark; FolderPath: string): TTreeNode;
    // Creates the folder tree items and attaches these items to a custom tree.
    procedure AttachFolderNodes(CustomTree: TCustomTreeView);
    procedure AddFolder(Sender: TObject; FolderPath: string);
    procedure ResetCurrent;
    // Check that the request object is matched with the current bookmark.
    function IsCurrentRequest(RO: TRequestObject): Boolean;

    property TreeView: TTreeView read FTreeView;
    property RootName: string read GetRootName write SetRootName;
    property CurrentNode: TTreeNode read FCurrentNode;
    property CurrentBookmark: TBookmark read GetCurrentBookmark;
    property OnChangeBookmark: TOnChangeBookmark read FOnChangeBookmark write FOnChangeBookmark;
  end;

  function IsFolderNode(Node: TTreeNode): Boolean;

implementation

uses StdCtrls, strutils;

function IsFolderNode(Node: TTreeNode): Boolean;
begin
  Result := Node.Data = NIL;
end;

{ EFolderNode }

constructor EFolderNode.CreateDef;
begin
  inherited Create('Node is a folder.');
end;


{ TBookmark }

procedure TBookmark.SetName(AValue: string);
begin
  if FName = AValue then
    Exit; // =>
  if Trim(FName) = '' then
    raise Exception.Create('Bookmark Name is required.');
  FName := AValue;
end;

procedure TBookmark.SetTreeNode(AValue: TTreeNode);
begin
  if FTreeNode = AValue then
    Exit; // =>
  if not Assigned(avalue) then
    raise Exception.Create('Bookmark must be assigned to tree node.');
  if IsFolderNode(AValue) then
    raise EFolderNode.CreateDef;
  FTreeNode := AValue;
end;

constructor TBookmark.Create(aName: string);
begin
  FName := aName;
  FRequest := nil;
end;

destructor TBookmark.Destroy;
begin
  if FRequest <> Nil then
    FreeAndNil(FRequest);
  inherited Destroy;
end;

{ TBookmarkManager }

function TBookmarkManager.GetRootName: string;
begin
  Result := FRootNode.Text;
end;

function TBookmarkManager.GetCurrentBookmark: TBookmark;
begin
  Result := NIL;
  if Assigned(FCurrentNode) then begin
    if FCurrentNode.Data = NIL then
      raise Exception.Create('Runtime error: node data is not bookmark.');
    Result := TBookmark(FCurrentNode.Data);
  end;
end;

procedure TBookmarkManager.SetRootName(AValue: string);
begin
  if RootName = AValue then
    Exit; // =>
  FRootNode.Text := AValue;
end;

function TBookmarkManager.CreateTree: TTreeView;
begin
  Result := TTreeView.Create(Self);
  Result.Parent := Self;
  Result.Align := alClient;
  Result.ScrollBars := ssAutoBoth;
  Result.Options := [tvoReadOnly, tvoShowRoot, tvoShowLines, tvoShowButtons,
                 tvoAutoItemHeight, tvoKeepCollapsedNodes, tvoRightClickSelect];
  // Event handlers.
  Result.OnDblClick := @InternalTreeOnDblClick;
end;

procedure TBookmarkManager.CreateRootNode;
begin
  with FTreeView.Items do begin
    Clear;
    FRootNode := Add(NIL, 'My bookmarks');
    FRootNode.Data := NIL;
    FRootNode.MakeVisible;
  end;
end;

procedure TBookmarkManager.InternalTreeOnDblClick(Sender: TObject);
var
  Selected: TTreeNode;
  Prev: TBookmark;
begin
  Selected := FTreeView.Selected;
  // The node must be selected and is not be a folder node.
  if not (Assigned(Selected) and (not IsFolderNode(Selected))) then
    Exit; // =>
  if Selected = FCurrentNode then
    Exit; // =>
  Prev := GetCurrentBookmark;
  FCurrentNode := Selected;
  if Assigned(FOnChangeBookmark) then begin
    FOnChangeBookmark(Prev, GetCurrentBookmark);
    FCurrentNode := Selected; // Preserve selected node after user callback.
  end;
  FTreeView.Selected := Selected;
end;

constructor TBookmarkManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Align := alClient;
  Caption := '';
  BevelOuter := bvNone;
  FTreeView := CreateTree;
  FCurrentNode := NIL;
  CreateRootNode;
end;

destructor TBookmarkManager.Destory;
begin
  FreeAndNil(FTreeView);
  inherited Destroy;
end;

function TBookmarkManager.NewFolder(FolderName: string; Edit: Boolean): TTreeNode;
var
  root: TTreeNode;
begin
  root := FTreeView.Selected;
  if root = NIL then
    root := FTreeView.Items.GetFirstNode;
  Result := FTreeView.Items.AddChild(root, FolderName);
  Result.MakeVisible;
  if Edit then
    Result.EditText;
  Result.Data := NIL;
end;

function TBookmarkManager.AddBookmark(BM: TBookmark; FolderPath: string
  ): TTreeNode;
var
  FolderNode: TTreeNode;
begin
  FolderNode := FTreeView.Items.FindNodeWithTextPath(FolderPath);
  if FolderNode = nil then
    raise Exception.CreateFmt('Folder %s not found.', [FolderPath]);
  if not IsFolderNode(FolderNode) then
    raise EFolderNode.Create('Node must be a folder.');
  Result := FTreeView.Items.AddChild(FolderNode, BM.Name);
  Result.MakeVisible;
  Result.Data := BM;
  Result.Selected := True;
  FCurrentNode := Result;
end;

procedure TBookmarkManager.AttachFolderNodes(CustomTree: TCustomTreeView);
  procedure WalkNodes(CurrentNode: TTreeNode; NewParent: TTreeNode = NIL);
  var
    Child: TTreeNode;
  begin
    Child := CurrentNode.GetFirstChild;
    while Child <> NIL do begin
      if IsFolderNode(Child) then
        WalkNodes(Child, CustomTree.Items.AddChild(NewParent, Child.Text));
      Child := Child.GetNextSibling;
    end;
  end;
var
  FirstNode, NewParent: TTreeNode;
begin
  FirstNode := FTreeView.Items.GetFirstNode;
  CustomTree.Items.Clear;
  NewParent := CustomTree.Items.AddChild(FirstNode.Parent, FirstNode.Text);
  WalkNodes(FirstNode, NewParent);
end;

procedure TBookmarkManager.AddFolder(Sender: TObject; FolderPath: string);
var
  p: SizeInt;
  FolderName, CurFolder: string;
  ParentNode: TTreeNode;
begin
  p := RPos('/', FolderPath);
  FolderName := RightStr(FolderPath, Length(FolderPath) - p);
  CurFolder := LeftStr(FolderPath, p - 1);
  ParentNode := FTreeView.Items.FindNodeWithTextPath(CurFolder);
  if ParentNode = NIL then
    raise Exception.CreateFmt('Folder %s not found.', [CurFolder]);
  with FTreeView.Items.AddChild(ParentNode, FolderName) do begin
    Data := NIL;
    MakeVisible;
  end;
end;

procedure TBookmarkManager.ResetCurrent;
begin
  if Assigned(FCurrentNode) then
    FCurrentNode.Selected := False;
  FCurrentNode := NIL;
end;

function TBookmarkManager.IsCurrentRequest(RO: TRequestObject): Boolean;
var
  BM: TBookmark;
begin
  BM := GetCurrentBookmark;
  if not Assigned(BM) then
    Exit(False);
  Result := BM.Request.Url = RO.Url;
end;

end.
