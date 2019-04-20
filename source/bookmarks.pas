unit bookmarks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, ExtCtrls, Controls, request_object;

type

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
    function NewFolder(FolderName: string): TTreeNode;
    property TreeView: TTreeView read FTreeView;
    property RootName: string read GetRootName write SetRootName;
  end;

implementation

uses StdCtrls;


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
    FRootNode := AddChild(NIL, 'My bookmarks');
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

function TBookmarkManager.NewFolder(FolderName: string): TTreeNode;
begin
  // STUB
  Result := NIL;
end;

end.
