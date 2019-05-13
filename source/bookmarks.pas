unit bookmarks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, ExtCtrls, Controls, request_object;

type
  // Forward declaration.
  TBookmark = class;

  // Callback when a new folder is created.
  TOnNewFolderNode = function (Sender: TObject; FolderPath: string): TTreeNode of object;
  // Callback when a folder was renamed.
  TOnRenameFolderNode = procedure (Sender: TObject; FolderPath, NewName: string) of object;
  // When bookmark selected in the tree.
  TOnChangeBookmark = procedure (Previous, Selected: TBookmark) of object;

  { ENodeException }

  ENodeException = class(Exception)
  private
    FNode: TTreeNode;
  public
    constructor CreateNode(ANode: TTreeNode; const msg: string);
    property Node: TTreeNode read FNode write FNode;
  end;

  { ENodePathNotFound }

  ENodePathNotFound = class(Exception)
  private
    FPath: string;
  public
    constructor CreatePath(APath: string);
    property Path: string read FPath write FPath;
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
    // Add a bookmark to the tree.
    // BM - the bookmark instance.
    // FolderPath - the bookmark folder path separated by /
    function AddBookmark(BM: TBookmark; FolderPath: string): TTreeNode;
    // Creates the folder tree items and attaches these items to a custom tree.
    procedure AttachFolderNodes(CustomTree: TCustomTreeView);
    // Adds a new folder by its path to the tree.
    function AddFolder(Sender: TObject; FolderPath: string): TTreeNode;
    // Renames a folder.
    procedure RenameFolder(Sender: TObject; FolderPath, NewName: string);
    // Reset current bookmark and node.
    procedure ResetCurrent;
    // Update current bookmark: set a new name or/and move to another folder.
    // ANewName - a bookmark new name
    // TextPath - move the bookmark to another folder (optional).
    procedure UpdateCurrent(ANewName, TextPath: string);
    // Check that the request object is matched with the current bookmark.
    function IsCurrentRequest(RO: TRequestObject): Boolean;
    function FindFolder(TextPath: string): TTreeNode;

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

{ ENodePathNotFound }

constructor ENodePathNotFound.CreatePath(APath: string);
begin
  inherited CreateFmt('Folder path %s not found.', [APath]);
  FPath := APath;
end;

{ ENodeException }

constructor ENodeException.CreateNode(ANode: TTreeNode; const msg: string);
begin
  inherited Create(msg);
  FNode := ANode;
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
    raise ENodeException.CreateNode(AValue, 'The node is a folder node.');
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

function TBookmarkManager.AddBookmark(BM: TBookmark; FolderPath: string
  ): TTreeNode;
var
  FolderNode: TTreeNode;
begin
  FolderNode := FindFolder(FolderPath);
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

function TBookmarkManager.AddFolder(Sender: TObject; FolderPath: string): TTreeNode;
var
  p: SizeInt;
  FolderName, CurFolder: string;
  ParentNode: TTreeNode;
begin
  p := RPos('/', FolderPath);
  FolderName := RightStr(FolderPath, Length(FolderPath) - p);
  CurFolder := LeftStr(FolderPath, p - 1);
  ParentNode := FindFolder(CurFolder);
  Result := FTreeView.Items.AddChild(ParentNode, FolderName);
  Result.Data := NIL;
  Result.MakeVisible;
end;

procedure TBookmarkManager.RenameFolder(Sender: TObject; FolderPath,
  NewName: string);
var
  Folder: TTreeNode;
begin
  Folder := FindFolder(FolderPath);
  Folder.Text := NewName;
end;

procedure TBookmarkManager.ResetCurrent;
begin
  if Assigned(FCurrentNode) then
    FCurrentNode.Selected := False;
  FCurrentNode := NIL;
end;

procedure TBookmarkManager.UpdateCurrent(ANewName, TextPath: string);
var
  BM: TBookmark;
begin
  BM := CurrentBookmark;
  if not Assigned(BM) then
    Exit; // =>
  // Rename.
  if (ANewName <> '') and (BM.Name <> ANewName) then begin
    BM.Name := ANewName;
    FCurrentNode.Text := ANewName;
  end;
  // Move to another folder.
  if (TextPath <> '') and (FCurrentNode.GetTextPath <> TextPath) then begin
    FCurrentNode.Delete;
    AddBookmark(BM, TextPath); // current node will be updated here.
  end;
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

function TBookmarkManager.FindFolder(TextPath: string): TTreeNode;
begin
  Result := FTreeView.Items.FindNodeWithTextPath(TextPath);
  if Result = NIL then
    raise ENodePathNotFound.CreatePath(TextPath);
  if not IsFolderNode(Result) then
    raise ENodeException.CreateNode(Result, 'The node must be a folder.');
end;

end.
