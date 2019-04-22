unit bookmarks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, ExtCtrls, Controls, request_object;

type

  // Callback when a new folder is created.
  TBookmarkNewFolder = procedure (Sender: TObject; FolderPath: string) of object;

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
    function GetRootName: string;
    procedure SetRootName(AValue: string);
  protected
    function CreateTree: TTreeView; virtual;
    procedure CreateRootNode;
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
    property TreeView: TTreeView read FTreeView;
    property RootName: string read GetRootName write SetRootName;
  end;

  function IsFolderNode(Node: TTreeNode): Boolean;

implementation

uses StdCtrls, strutils;

function IsFolderNode(Node: TTreeNode): Boolean;
begin
  Result := Node.Data = NIL;
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

constructor TBookmarkManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Align := alClient;
  Caption := '';
  BevelOuter := bvNone;
  FTreeView := CreateTree;
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
    raise Exception.Create(Format('Folder %s not found.', [FolderPath]));
  if FolderNode.Data <> NIL then
    raise Exception.Create('Bookmark can be attached only to a folder.');
  Result := FTreeView.Items.AddChild(FolderNode, BM.Name);
  Result.MakeVisible;
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
    raise Exception.Create(Format('Folder %s not found.', [CurFolder]));
  FTreeView.Items.AddChild(ParentNode, FolderName);
end;

end.
