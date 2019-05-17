unit bookmarks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, ExtCtrls, Controls, Menus, request_object;

type
  // Forward declarations.
  TBookmark = class;
  TBookmarkPopup = class;

  // When bookmark is changed (opened).
  TOnChangeBookmark = procedure (Previous, Selected: TBookmark) of object;
  // When 'Edit' item was selected in the popup menu.
  TOnEditBookmarkClick = procedure (Sender: TObject; BM: TBookmark) of object;

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

    function GetBookmarkPopup: TBookmarkPopup;
    function GetCurrentBookmark: TBookmark;
    function GetRootName: string;
    procedure SetBookmarkPopup(AValue: TBookmarkPopup);
    procedure SetCurrentNode(AValue: TTreeNode);
    procedure SetRootName(AValue: string);

  protected
    function CreateTree: TTreeView; virtual;
    procedure CreateRootNode; virtual;
    // TreeView double click event handler.
    procedure InternalTreeOnDblClick(Sender: TObject); virtual;
    function GetNodeFolderPath(aNode: TTreeNode): string;

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
    function AddFolder(FolderPath: string): TTreeNode;
    // Renames a folder.
    procedure RenameFolder(FolderPath, NewName: string);
    // Reset current bookmark and node.
    procedure ResetCurrent;
    // Update current bookmark: set a new name or/and move to another folder.
    // ANewName - a bookmark new name
    // FolderPath - move the bookmark to another folder (optional).
    function UpdateBookmark(BM: TBookmark; ANewName, FolderPath: string): Boolean;
    // Check that the request object is matched with the current bookmark.
    function IsCurrentRequest(RO: TRequestObject): Boolean;
    // Finds the folder node by path.
    function FindFolder(FolderPath: string): TTreeNode;
    // Deletes the bookmark from the tree.
    function DeleteBookmark(BM: TBookmark): Boolean;
    // Deletes the folder and all its children (folders, bookmarks).
    function DeleteFolder(FolderPath: string): Boolean;
    function DeleteFolderNode(FolderNode: TTreeNode): Boolean;
    // Finds a tree node by bookmark instance.
    function FindNode(BM: TBookmark): TTreeNode;
    // Select bookmark.
    procedure OpenBookmark(BM: TBookmark = NIL);

    property TreeView: TTreeView read FTreeView;
    property RootName: string read GetRootName write SetRootName;
    property CurrentNode: TTreeNode read FCurrentNode write SetCurrentNode;
    property CurrentBookmark: TBookmark read GetCurrentBookmark;
    property OnChangeBookmark: TOnChangeBookmark read FOnChangeBookmark write FOnChangeBookmark;
    property Popup: TBookmarkPopup read GetBookmarkPopup write SetBookmarkPopup;
  end;

  { TBookmarkPopup }

  TBookmarkPopup = class(TPopupMenu)
  private
    FBookmarkManager: TBookmarkManager;
    FOnEditClick: TOnEditBookmarkClick;
    function GetBookmarkManager: TBookmarkManager;
    function GetSelectedNode: TTreeNode;
  protected
    function CreateItem(const caption: string): TMenuItem; virtual;
    procedure CreateDefaultItems; virtual;
    procedure InternalOnClickOpen(Sender: TObject); virtual;
    procedure InternalOnClickEdit(Sender: TObject); virtual;
    procedure InternalOnClickDelete(Sender: TObject); virtual;
    procedure RenameFolder(FolderNode: TTreeNode); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function ConfirmDeleteFolder(FolderName: string): Boolean;
    function ConfirmDeleteBookmark(BM: TBookmark): Boolean;

    property BookmarkManager: TBookmarkManager read GetBookmarkManager write FBookmarkManager;
    property SelectedNode: TTreeNode read GetSelectedNode;
    property OnEditClick: TOnEditBookmarkClick read FOnEditClick write FOnEditClick;
  end;

  function IsFolderNode(Node: TTreeNode): Boolean;
  function NodeToBookmark(Node: TTreeNode): TBookmark;

implementation

uses StdCtrls, Dialogs, app_helpers, strutils;

function IsFolderNode(Node: TTreeNode): Boolean;
begin
  if not Assigned(Node) then
    raise Exception.Create('A node instance is required.');
  Result := Node.Data = NIL;
end;

function NodeToBookmark(Node: TTreeNode): TBookmark;
begin
  if IsFolderNode(Node) then
    raise ENodeException.CreateNode(Node, 'The node is a folder node but expected bookmark node.');
  Result := TBookmark(Node.Data);
end;

{ TBookmarkPopup }

function TBookmarkPopup.GetBookmarkManager: TBookmarkManager;
begin
  if not Assigned(FBookmarkManager) then
    raise Exception.Create('Bookmark manager is required for popup menu.');
  Result := FBookmarkManager;
end;

function TBookmarkPopup.GetSelectedNode: TTreeNode;
begin
  Result := FBookmarkManager.TreeView.Selected;
end;

function TBookmarkPopup.CreateItem(const caption: string): TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  Result.Caption := caption;
  Items.Add(Result);
end;

procedure TBookmarkPopup.CreateDefaultItems;
begin
  with CreateItem('Open') do
    OnClick := @InternalOnClickOpen;
  with CreateItem('Edit') do
    OnClick := @InternalOnClickEdit;
  with CreateItem('Delete') do
    OnClick := @InternalOnClickDelete;
end;

procedure TBookmarkPopup.InternalOnClickOpen(Sender: TObject);
var
  sNode: TTreeNode;
begin
  sNode := SelectedNode;
  if not Assigned(sNode) then
    Exit; // =>
  if IsFolderNode(sNode) then begin
    if sNode.Expanded then sNode.Collapse(True) else sNode.Expand(True);
  end
  else
    FBookmarkManager.OpenBookmark;
end;

procedure TBookmarkPopup.InternalOnClickEdit(Sender: TObject);
var
  sNode: TTreeNode;
begin
  sNode := SelectedNode;
  if not Assigned(sNode) then
    Exit; // =>
  if IsFolderNode(sNode) then begin
    RenameFolder(sNode);
  end
  else begin
    if Assigned(FOnEditClick) then
      FOnEditClick(Sender, NodeToBookmark(sNode));
  end;
end;

procedure TBookmarkPopup.InternalOnClickDelete(Sender: TObject);
var
  sNode: TTreeNode;
  BM: TBookmark;
begin
  sNode := SelectedNode;
  if not Assigned(sNode) then
    Exit; // =>
  if IsFolderNode(sNode) then begin
    // Don't allow to delete root node.
    if (sNode.Parent <> NIL) and ConfirmDeleteFolder(sNode.Text) then
      BookmarkManager.DeleteFolderNode(sNode);
  end
  else begin
    BM := NodeToBookmark(sNode);
    if ConfirmDeleteBookmark(BM) then begin
      BookmarkManager.DeleteBookmark(BM);
    end;
  end;
end;

procedure TBookmarkPopup.RenameFolder(FolderNode: TTreeNode);
var
  NewName: string;
begin
  NewName := InputBox('Edit folder', 'A new folder name:', FolderNode.Text);
  if (NewName = FolderNode.Text) or (Trim(NewName) = '') then
    Exit; // =>
  FolderNode.Text := NewName;
end;

function TBookmarkPopup.ConfirmDeleteFolder(FolderName: string): Boolean;
begin
  Result := ConfirmDlg('Delete folder ?', Format('Are you sure you want to delete folder "%s" and ALL its children ?', [FolderName])) = mrOK;
end;

function TBookmarkPopup.ConfirmDeleteBookmark(BM: TBookmark): Boolean;
begin
  Result := ConfirmDlg('Delete bookmark ?', Format('Are you sure you want to delete "%s" ?', [BM.Name])) = mrOK;
end;

constructor TBookmarkPopup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateDefaultItems;
  if AOwner is TBookmarkManager then
    FBookmarkManager := TBookmarkManager(AOwner);
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

procedure TBookmarkManager.SetBookmarkPopup(AValue: TBookmarkPopup);
begin
  if FTreeView.PopupMenu = AValue then
    Exit; // =>
  FTreeView.PopupMenu := TPopupMenu(AValue);
end;

procedure TBookmarkManager.SetCurrentNode(AValue: TTreeNode);
begin
  if FCurrentNode = AValue then
    Exit; // =>
  if AValue = NIL then
    ResetCurrent
  else begin
    if IsFolderNode(AValue) then
      raise ENodeException.CreateNode(AValue, 'Current node cannot be a folder node.');
    FCurrentNode := AValue;
    FCurrentNode.Selected := True;
    FCurrentNode.MakeVisible;
  end;
end;

function TBookmarkManager.GetCurrentBookmark: TBookmark;
begin
  Result := NIL;
  if Assigned(FCurrentNode) then begin
    if FCurrentNode.Data = NIL then
      raise ENodeException.CreateNode(FCurrentNode, 'Runtime error: node data is not bookmark.');
    Result := TBookmark(FCurrentNode.Data);
  end;
end;

function TBookmarkManager.GetBookmarkPopup: TBookmarkPopup;
begin
  Result := TBookmarkPopup(FTreeView.PopupMenu);
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
begin
  OpenBookmark;
end;

function TBookmarkManager.GetNodeFolderPath(aNode: TTreeNode): string;
var
  path: string;
  p: SizeInt;
begin
  path := aNode.GetTextPath;
  if IsFolderNode(aNode) then
    Exit(path);
  p := RPos('/', path);
  Result := LeftStr(path, p - 1);
end;

constructor TBookmarkManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Align := alClient;
  Caption := '';
  BevelOuter := bvNone;
  FTreeView := CreateTree;
  Popup := TBookmarkPopup.Create(Self);
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
  Result.Data := BM;
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

function TBookmarkManager.AddFolder(FolderPath: string): TTreeNode;
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

procedure TBookmarkManager.RenameFolder(FolderPath, NewName: string);
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

function TBookmarkManager.UpdateBookmark(BM: TBookmark; ANewName, FolderPath: string): Boolean;
var
  bNode: TTreeNode;
  isCurrent: Boolean;
begin
  bNode := FindNode(BM);
  if not Assigned(bNode) then
    Exit(False); // =>
  // Rename.
  if (ANewName <> '') and (BM.Name <> ANewName) then begin
    BM.Name := ANewName;
    bNode.Text := ANewName;
  end;
  // Move to another folder.
  if (FolderPath <> '') and (GetNodeFolderPath(bNode) <> FolderPath) then begin
    isCurrent := (bNode = FCurrentNode);
    bNode.Delete;
    bNode := AddBookmark(BM, FolderPath);
    if isCurrent then
      CurrentNode := bNode;
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

function TBookmarkManager.FindFolder(FolderPath: string): TTreeNode;
begin
  Result := FTreeView.Items.FindNodeWithTextPath(FolderPath);
  if Result = NIL then
    raise ENodePathNotFound.CreatePath(FolderPath);
  if not IsFolderNode(Result) then
    raise ENodeException.CreateNode(Result, 'The node must be a folder.');
end;

function TBookmarkManager.DeleteBookmark(BM: TBookmark): Boolean;
var
  bNode: TTreeNode;
begin
  bNode := FindNode(BM);
  if not Assigned(bNode) then
    Exit(False); // =>
  if bNode = FCurrentNode then begin
    ResetCurrent;
  end;
  FreeAndNil(BM);
  bNode.Delete;
  Result := True;
end;

function TBookmarkManager.DeleteFolder(FolderPath: string): Boolean;
var
  fNode: TTreeNode;
begin
  try
    fNode := FindFolder(FolderPath);
    Result := DeleteFolderNode(fNode);
  except on E: ENodePathNotFound do
    Exit(False); // =>
  end;
end;

function TBookmarkManager.DeleteFolderNode(FolderNode: TTreeNode): Boolean;
  // Recursive node delete.
  procedure DeleteNode(aNode: TTreeNode);
  begin
    if aNode = FCurrentNode then
      ResetCurrent;
    while aNode.HasChildren do
      DeleteNode(aNode.GetLastChild);
    if Assigned(aNode.Data) then
      TBookmark(aNode.Data).Free;
    aNode.Delete;
  end;

begin
  if not IsFolderNode(FolderNode) then
    Exit(False);
  DeleteNode(FolderNode);
  Result := True;
end;

function TBookmarkManager.FindNode(BM: TBookmark): TTreeNode;
  function InternalFind(aNode: TTreeNode): TTreeNode;
  var
    Child: TTreeNode;
  begin
    Result := NIL;
    if not Assigned(aNode) then
      Exit; // =>
    Child := aNode.GetFirstChild;
    while Child <> NIL do begin
      if IsFolderNode(Child) then begin
        Result := InternalFind(Child);
        if Result <> NIL then
          Exit; // =>
      end;
      if Child.Data = Pointer(BM) then
        Exit(Child);
      Child := Child.GetNextSibling;
    end;
  end;
begin
  Result := InternalFind(FTreeView.Items.GetFirstNode);
end;

procedure TBookmarkManager.OpenBookmark(BM: TBookmark = NIL);
var
  Selected: TTreeNode;
  Prev: TBookmark;
begin
  if BM = NIL then
    Selected := FTreeView.Selected
  else
    Selected := FindNode(BM);
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

end.
