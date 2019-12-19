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
    FRequestRoot: TTreeNode;
    FRequestSelected: TTreeNode;
    FRequestManager: TRequestManager;
    FRequestPopup: TSavedRequestPopup;
  protected
    function CreateTreeView: TTreeView; virtual;
    function CreateRequestNode: TTreeNode; virtual;
    procedure InitTree; virtual;
    function IsRequestNode(ANode: TTreeNode): Boolean;
    procedure InternalTreeOnDblClick(Sender: TObject);
    procedure InternalTreeOnContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure InternalTreeOnSelectionChanged(Sender: TObject);
    procedure InternalTreePopupOnClose(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property TreeView: TTreeView read FTreeView;
    property RequestManager: TRequestManager read FRequestManager;
    property RequestRoot: TTreeNode read FRequestRoot;
    property RequestSelected: TTreeNode read FRequestSelected;
    property RequestPopup: TSavedRequestPopup read FRequestPopup write FRequestPopup;
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

function TAppTreeManager.CreateRequestNode: TTreeNode;
begin
  with FTreeView.Items do begin
    Result := Add(NIL, 'Saved requests');
    Result.Data := NIL;
    //FRootNode.StateIndex := FImgIdxRoot;
    Result.MakeVisible;
  end;
end;

procedure TAppTreeManager.InitTree;
begin
  FTreeView := CreateTreeView;
  FRequestRoot := CreateRequestNode;
end;

function TAppTreeManager.IsRequestNode(ANode: TTreeNode): Boolean;
begin
  while ANode.Parent <> NIL do
    ANode := ANode.Parent;
  Result := (ANode = FRequestRoot);
end;

procedure TAppTreeManager.InternalTreeOnDblClick(Sender: TObject);
var
  aNode: TTreeNode;
begin
  aNode := FTreeView.Selected;
  if not Assigned(aNode) then
    Exit; // =>
  if IsRequestNode(aNode) then
    FRequestManager.OpenNode(aNode);
end;

procedure TAppTreeManager.InternalTreeOnContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  aNode: TTreeNode;
begin
  aNode := FTreeView.GetNodeAt(MousePos.x, MousePos.y);
  if not Assigned(aNode) then
    Exit; // =>
  if IsRequestNode(aNode) then
  begin
    FTreeView.PopupMenu := FRequestPopup;
    FTreeView.PopupMenu.OnClose := @InternalTreePopupOnClose;
  end;
end;

procedure TAppTreeManager.InternalTreeOnSelectionChanged(Sender: TObject);
var
  aNode: TTreeNode;
begin
  aNode := FTreeView.Selected;
  FRequestSelected := NIL;
  if Assigned(aNode) then
  begin
    if IsRequestNode(aNode) then
      FRequestSelected := aNode;
  end;
  if Assigned(FRequestPopup) then
    FRequestPopup.SelectedNode := FRequestSelected;
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
  FRequestManager := TRequestManager.Create(FRequestRoot);
  FRequestPopup := TSavedRequestPopup.Create(Self);
  FRequestPopup.RequestManager := FRequestManager;
  FRequestPopup.RootNode := FRequestRoot;
end;

destructor TAppTreeManager.Destroy;
begin
  if Assigned(FRequestPopup) then
    FreeAndNil(FRequestPopup);
  FreeAndNil(FRequestManager);
  FreeAndNil(FTreeView);
  inherited Destroy;
end;

end.

