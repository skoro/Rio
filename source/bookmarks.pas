unit bookmarks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, request_object;

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
    constructor Create(aName: string; aRequest: TRequestObject);
    destructor Destroy; override;
    property Name: string read FName write SetName;
    property Request: TRequestObject read FRequest;
    property TreeNode: TTreeNode read FTreeNode write SetTreeNode;
  end;

  { TBookmarkContainer }

  TBookmarkContainer = class
  private
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

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

constructor TBookmark.Create(aName: string; aRequest: TRequestObject);
begin
  FName := aName;
  FRequest := aRequest;
end;

destructor TBookmark.Destroy;
begin
  inherited Destroy;
end;

{ TBookmarkContainer }

constructor TBookmarkContainer.Create;
begin

end;

destructor TBookmarkContainer.Destroy;
begin
  inherited Destroy;
end;

end.
