unit Requests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, ComCtrls, Controls, Menus,
  RequestObject;

type
  // Forward declarations.
  TSavedRequest = class;
  TSavedRequestPopup = class;

  // When a request is changed (opened).
  TOnChangeRequest = procedure (Previous, Selected: TSavedRequest) of object;
  // When the request menu item was selected in the popup menu.
  TOnRequestClick = procedure (Sender: TObject; SR: TSavedRequest) of object;
  // When the request is deleted.
  TOnDeleteRequest = procedure (const ARequest: TSavedRequest) of object;
  // When the request is renamed or moved to another tree node.
  TOnMoveRequest = procedure (const SR: TSavedRequest; const OldPath: string) of object;
  // When the new request is added.
  TOnAddRequest = procedure (const SR: TSavedRequest; const FolderPath: string) of object;

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

  { TSavedRequest }

  TSavedRequest = class
  private
    FName: string;
    FRequest: TRequestObject;
    FTreeNode: TTreeNode;
    FLocked: Boolean;
    FTabRequest: string;
    FTabResponse: string;
    function GetPath: string;
    procedure SetName(AValue: string);
    procedure SetTreeNode(AValue: TTreeNode);
  public
    constructor Create(aName: string); virtual;
    destructor Destroy; override;
    procedure UpdateRequest(ANewRequest: TRequestObject);
    property Request: TRequestObject read FRequest write FRequest;
    property TreeNode: TTreeNode read FTreeNode write SetTreeNode;
    property Name: string read FName write SetName;
    property Path: string read GetPath;
    property Locked: Boolean read FLocked write FLocked;
    property TabRequest: string read FTabRequest write FTabRequest;
    property TabResponse: string read FTabResponse write FTabResponse;
  end;

  { TNodeView }

  TRequestNodeStyle = (bnsNone, bnsText, bnsIcon);

  { TRequestManager }

  TRequestManager = class
  private
    FRootNode: TTreeNode;
    FCurrentNode: TTreeNode;
    FOnChangeRequest: TOnChangeRequest;
    FOnDeleteRequest: TOnDeleteRequest;
    FOnMoveRequest: TOnMoveRequest;
    FOnAddRequest: TOnAddRequest;
    FImgIdxFolder: Integer;
    FImgIdxSelected: Integer;
    FImgIdxRoot: Integer;
    FRequestNodeStyle: TRequestNodeStyle;

    function GetCurrentRequest: TSavedRequest;
    function GetRootName: string;
    procedure SetCurrentNode(AValue: TTreeNode);
    procedure SetRequestNodeStyle(AValue: TRequestNodeStyle);
    procedure SetImgIdxRoot(AValue: Integer);
    procedure SetRootName(AValue: string);

  protected
    function InternalAddFolder(ParentNode: TTreeNode; FolderName: string): TTreeNode; virtual;
    // Walk around the TreeView and save its content to the xml document.
    procedure InternalSaveToXml(Doc: TXMLDocument; XmlRoot: TDOMNode; aNode: TTreeNode); virtual;
    // Populate the tree view from the XML.
    procedure InternalLoadFromXml(XmlNode: TDOMNode; aNode: TTreeNode); virtual;
    // Set or reset the selected image on the node.
    procedure SetNodeSelectedImage(aNode: TTreeNode; NeedReset: Boolean = False); virtual;
    // Sort nodes by specific criteria.
    procedure SortNodes(ParentNode: TTreeNode); virtual;
    // Sort comparator.
    function SortNodeCompare(Node1, Node2: TTreeNode): integer; virtual;
    // Apply node styles (TRequestNodeStyle).
    procedure UpdateRequestNodeStyle; virtual;
    // Adds a child node to the requests subtree.
    function AddChildNode(ParentNode: TTreeNode; const NodeText: string): TTreeNode;

  public
    constructor Create(ARootNode: TTreeNode); virtual;
    destructor Destroy; override;
    // Add a request to the tree.
    // SR - the saved request instance.
    // FolderPath - the requests folder path separated by /
    function AddRequest(SR: TSavedRequest; FolderPath: string): TTreeNode;
    // Creates the folder tree items and attaches these items to a custom tree.
    procedure AttachFolderNodes(CustomTree: TCustomTreeView);
    // Adds a new folder by its path to the tree.
    function AddFolder(FolderPath: string): TTreeNode;
    // Renames a folder.
    function RenameFolder(FolderPath, NewName: string): Boolean;
    // Reset current request and node.
    procedure ResetCurrent;
    // Update current request: set a new name or/and move to another folder.
    // ANewName - a request new name
    // FolderPath - move the request to another folder (optional).
    function UpdateRequest(SR: TSavedRequest; ANewName, FolderPath: string): Boolean;
    // Check that the request object is matched with the current request.
    function IsCurrentRequest(RO: TRequestObject): Boolean;
    // Finds the folder node by path.
    function FindFolder(FolderPath: string): TTreeNode;
    // Deletes the request from the tree.
    function DeleteRequest(SR: TSavedRequest): Boolean;
    // Deletes the folder and all its children (folders, requests).
    function DeleteFolder(FolderPath: string): Boolean;
    function DeleteFolderNode(FolderNode: TTreeNode): Boolean;
    // Finds a tree node by request instance.
    function FindNode(SR: TSavedRequest): TTreeNode;
    // Select a request.
    procedure OpenRequest(SR: TSavedRequest);
    // Select a request by its node path.
    function OpenRequestPath(Path: string): TSavedRequest;
    // Select a request by the node.
    procedure OpenNode(aNode: TTreeNode);
    // Get the node folder path (this is like dirname for files).
    function GetNodeFolderPath(aNode: TTreeNode): string;
    // Save the requests content to a string buffer.
    procedure SaveXmlToStream(S: TStream); virtual;
    // Load requests from the stream.
    procedure LoadXmlFromStream(S: TStream); virtual;
    // Returns a request tree path.
    function GetRequestPath(SR: TSavedRequest): string; virtual;
    // Set a node style: icon or text presentation.
    procedure SetNodeStyle(aNode: TTreeNode); virtual;

    //property TreeView: TTreeView read FTreeView;
    property RootNode: TTreeNode read FRootNode;
    property RootName: string read GetRootName write SetRootName;
    property CurrentNode: TTreeNode read FCurrentNode write SetCurrentNode;
    property CurrentRequest: TSavedRequest read GetCurrentRequest;
    property OnChangeRequest: TOnChangeRequest read FOnChangeRequest write FOnChangeRequest;
    property OnDeleteRequest: TOnDeleteRequest read FOnDeleteRequest write FOnDeleteRequest;
    property OnMoveRequest: TOnMoveRequest read FOnMoveRequest write FOnMoveRequest;
    property OnAddRequest: TOnAddRequest read FOnAddRequest write FOnAddRequest;
    property ImageIndexFolder: Integer read FImgIdxFolder write FImgIdxFolder;
    property ImageIndexSelected: Integer read FImgIdxSelected write FImgIdxSelected;
    property ImageIndexRoot: Integer read FImgIdxRoot write SetImgIdxRoot;
    property RequestNodeStyle: TRequestNodeStyle read FRequestNodeStyle write SetRequestNodeStyle;
  end;

  { TSavedRequestPopup }

  TSavedRequestPopup = class(TPopupMenu)
  private
    FRequestManager: TRequestManager;
    FOnEditClick: TOnRequestClick;
    FOnDeleteClick: TOnRequestClick;
    FSelectedNode: TTreeNode;
    FRootNode: TTreeNode;
    function GetRequestManager: TRequestManager;
  protected
    function CreateItem(const caption: string): TMenuItem; virtual;
    procedure CreateDefaultItems; virtual;
    procedure InternalOnClickOpen(Sender: TObject); virtual;
    procedure InternalOnClickEdit(Sender: TObject); virtual;
    procedure InternalOnClickDelete(Sender: TObject); virtual;
    procedure InternalOnClickNewFolder(Sender: TObject); virtual;
    procedure RenameFolder(FolderNode: TTreeNode); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function ConfirmDeleteFolder(FolderName: string): Boolean;
    function ConfirmDeleteRequest(SR: TSavedRequest): Boolean;

    property RequestManager: TRequestManager read GetRequestManager write FRequestManager;
    property SelectedNode: TTreeNode read FSelectedNode write FSelectedNode;
    property RootNode: TTreeNode read FRootNode write FRootNode;
    property OnEditClick: TOnRequestClick read FOnEditClick write FOnEditClick;
    property OnDeleteClick: TOnRequestClick read FOnDeleteClick write FOnDeleteClick;
  end;

  // Globals section.
  // ===================================
const
  // Application requests filename.
  REQUESTS_FILENAME = 'Requests.cfg';

  function IsFolderNode(Node: TTreeNode): Boolean;
  function NodeToRequest(Node: TTreeNode): TSavedRequest;

  { TODO : See Env.pas as the example for Load/Save data into/from class.
   Also there will be needed an interface for load and save data:
    interface
      procedure LoadFromStream
      procedure SaveToStream
  }

  // Save the application requests.
  procedure SaveAppRequests(RM: TRequestManager; Filename: string = REQUESTS_FILENAME);
  // Load the application requests.
  function LoadAppRequests(RM: TRequestManager; Filename: string = REQUESTS_FILENAME): Boolean;
  // Get the full path name of the application requests.
  function GetAppRequestsFilename(Basename: string = REQUESTS_FILENAME): string;

  // These functions must be used instead of Node.GetTextPath and
  // Tree.Items.FindNodeWithTextPath. This is because a forward slash can be
  // inside Node.Text which is used as a path separator between tree nodes
  // in the above functions.
  // Returns a safe node path (must be used instead of Node.GetTextPath).
  function GetNodePath(aNode: TTreeNode): string;
  // Returns a node by the path (must be used instead of Tree.Items.FindNodeWithTextPath).
  function FindNodePath(StartNode: TTreeNode; TextPath: string): TTreeNode;

implementation

uses Dialogs, AppHelpers, strutils, XMLWrite, XMLRead;

const
  // What symbol to use as forward slash in the node text.
  // See GetNodePath, FindNodePath.
  NODE_TEXT_SLASH = #9;

function IsFolderNode(Node: TTreeNode): Boolean;
begin
  if not Assigned(Node) then
    raise Exception.Create('A node instance is required.');
  Result := Node.Data = NIL;
end;

function NodeToRequest(Node: TTreeNode): TSavedRequest;
begin
  if IsFolderNode(Node) then
    raise ENodeException.CreateNode(Node, 'The node is a folder node but expected a request node.');
  Result := TSavedRequest(Node.Data);
end;

procedure SaveAppRequests(RM: TRequestManager; Filename: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(GetAppRequestsFilename(Filename), fmCreate);
  try
    RM.SaveXmlToStream(FS);
  finally
    FS.Free;
  end;
end;

function LoadAppRequests(RM: TRequestManager; Filename: string): Boolean;
var
  FS: TFileStream;
  Path: string;
begin
  Path := GetAppRequestsFilename(Filename);
  if not FileExists(Path) then
    Exit(False); // =>
  try
    FS := TFileStream.Create(Path, fmOpenRead);
    RM.LoadXmlFromStream(FS);
    Result := True;
  finally
    FS.Free;
  end;
end;

function GetAppRequestsFilename(Basename: string): string;
begin
  Result := GetAppConfigDir(False) + Basename;
end;

function GetNodePath(aNode: TTreeNode): string;
var
  Node: TTreeNode;
  Name: string;
begin
  Result := '';
  Node := aNode;
  while Assigned(Node) do
  begin
    if Result <> '' then
      Result := '/' + Result;
    // When finding a node we must treat the node name without a request method.
    // Folder node name is untouched.
    if IsFolderNode(Node) then
      Name := Node.Text
    else
      Name := NodeToRequest(Node).Name;
    Result := ReplaceStr(Name, '/', NODE_TEXT_SLASH) + Result;
    Node := Node.Parent;
  end;
end;

function FindNodePath(StartNode: TTreeNode; TextPath: string): TTreeNode;
var
  p: SizeInt;
  CurText: String;
  function FindNode(Root: TTreeNode; Find: string): TTreeNode;
  var
    NodeName: string;
  begin
    Result := Root;
    while Assigned(Result) do begin
      if IsFolderNode(Result) then
        NodeName := Result.Text
      else
        NodeName := NodeToRequest(Result).Name;
      if Find = NodeName then
        Exit; //=>
      Result := Result.GetNextSibling;
    end;
  end;
begin
  Result := NIL;
  repeat
    p := System.Pos('/', TextPath);
    if p > 0 then begin
      CurText := LeftStr(TextPath, p-1);
      System.Delete(TextPath, 1, p);
    end else begin
      CurText := TextPath;
      TextPath := '';
    end;
    CurText := ReplaceStr(CurText, NODE_TEXT_SLASH, '/');
    if Result = NIL then
    begin
      if CurText = StartNode.Text then
        Result := StartNode;
    end
    else
      Result := FindNode(Result.GetFirstChild, CurText);
  until (Result = NIL) or (TextPath = '');
end;

{ TSavedRequestPopup }

function TSavedRequestPopup.GetRequestManager: TRequestManager;
begin
  if not Assigned(FRequestManager) then
    raise Exception.Create('Request manager is required for popup menu.');
  Result := FRequestManager;
end;

function TSavedRequestPopup.CreateItem(const caption: string): TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  Result.Caption := caption;
  Items.Add(Result);
end;

procedure TSavedRequestPopup.CreateDefaultItems;
begin
  with CreateItem('Open') do
    OnClick := @InternalOnClickOpen;
  with CreateItem('New folder') do
    OnClick := @InternalOnClickNewFolder;
  with CreateItem('Edit') do
    OnClick := @InternalOnClickEdit;
  with CreateItem('Delete') do
    OnClick := @InternalOnClickDelete;
end;

procedure TSavedRequestPopup.InternalOnClickOpen(Sender: TObject);
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
    FRequestManager.OpenNode(sNode);
end;

procedure TSavedRequestPopup.InternalOnClickEdit(Sender: TObject);
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
      FOnEditClick(Sender, NodeToRequest(sNode));
  end;
end;

procedure TSavedRequestPopup.InternalOnClickDelete(Sender: TObject);
var
  sNode: TTreeNode;
  BM: TSavedRequest;
begin
  sNode := SelectedNode;
  if not Assigned(sNode) then
    Exit; // =>
  if IsFolderNode(sNode) then begin
    // Don't allow to delete root node.
    if (sNode.Parent <> NIL) and ConfirmDeleteFolder(GetNodePath(sNode)) then
    begin
      RequestManager.DeleteFolderNode(sNode);
      if Assigned(FOnDeleteClick) then
        FOnDeleteClick(Self, NIL);
    end;
  end
  else begin
    BM := NodeToRequest(sNode);
    if ConfirmDeleteRequest(BM) then
    begin
      if Assigned(FOnDeleteClick) then
        FOnDeleteClick(Self, BM);
      RequestManager.DeleteRequest(BM);
    end;
  end;
end;

procedure TSavedRequestPopup.InternalOnClickNewFolder(Sender: TObject);
var
  fName: string;
  sNode: TTreeNode;
begin
  fName := InputBox('New folder', 'Folder name (or folder path separated by /):', '');
  if Trim(fName) = '' then
    Exit; // =>
  sNode := SelectedNode;
  if not Assigned(sNode) then // No selected - attach to the root node.
    sNode := FRootNode;
  if not IsFolderNode(sNode) then
    sNode := sNode.Parent;
  if (not Assigned(sNode)) or (not IsFolderNode(sNode)) then
    Exit; // =>
  FRequestManager.AddFolder(GetNodePath(sNode) + '/' + fName);
end;

procedure TSavedRequestPopup.RenameFolder(FolderNode: TTreeNode);
var
  NewName: string;
begin
  NewName := InputBox('Edit folder', 'A new folder name:', FolderNode.Text);
  if (NewName = FolderNode.Text) or (Trim(NewName) = '') then
    Exit; // =>
  if not FRequestManager.RenameFolder(GetNodePath(FolderNode), NewName) then
    ERRMsg('Error', 'Cannot rename folder. Folder is already exist ?');
end;

function TSavedRequestPopup.ConfirmDeleteFolder(FolderName: string): Boolean;
begin
  Result := ConfirmDlg('Delete folder ?',
         Format('Are you sure you want to delete folder "%s" and ALL its children ?', [FolderName])) = mrOK;
end;

function TSavedRequestPopup.ConfirmDeleteRequest(SR: TSavedRequest): Boolean;
begin
  Result := ConfirmDlg('Delete request ?',
         Format('Are you sure you want to delete:'+#13#10+'"%s" ?', [SR.Path])) = mrOK;
end;

constructor TSavedRequestPopup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateDefaultItems;
  FSelectedNode := nil;
  FRootNode := nil;
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

{ TSavedRequest }

procedure TSavedRequest.SetName(AValue: string);
begin
  if FName = AValue then
    Exit; // =>
  if Trim(FName) = '' then
    raise Exception.Create('Request Name is required.');
  FName := AValue;
end;

function TSavedRequest.GetPath: string;
begin
  Result := '';
  if Assigned(FTreeNode) then
    Result := GetNodePath(FTreeNode);
end;

procedure TSavedRequest.SetTreeNode(AValue: TTreeNode);
begin
  if FTreeNode = AValue then
    Exit; // =>
  if not Assigned(AValue) then
    raise Exception.Create('Request must be assigned to tree node.');
  if IsFolderNode(AValue) then
    raise ENodeException.CreateNode(AValue, 'The node is a folder node.');
  FTreeNode := AValue;
end;

constructor TSavedRequest.Create(aName: string);
begin
  { TODO : Check that name is not empty. }
  FName := aName;
  FRequest := nil;
  FLocked := False;
end;

destructor TSavedRequest.Destroy;
begin
  if FRequest <> Nil then
    FreeAndNil(FRequest);
  inherited Destroy;
end;

procedure TSavedRequest.UpdateRequest(ANewRequest: TRequestObject);
begin
  if Assigned(FRequest) and not FLocked then
    FreeAndNil(FRequest);
  //if not FLocked then
  FRequest := ANewRequest;
end;

{ TRequestManager }

function TRequestManager.GetRootName: string;
begin
  Result := FRootNode.Text;
end;

procedure TRequestManager.SetCurrentNode(AValue: TTreeNode);
begin
  if FCurrentNode = AValue then
    Exit; // =>
  if AValue = NIL then
    ResetCurrent
  else begin
    if IsFolderNode(AValue) then
      raise ENodeException.CreateNode(AValue, 'Current node cannot be a folder node.');
    // Reset the selected image on the previous node.
    if Assigned(FCurrentNode) then
      SetNodeSelectedImage(FCurrentNode, True);
    FCurrentNode := AValue;
    FCurrentNode.Selected := True;
    SetNodeSelectedImage(FCurrentNode);
    FCurrentNode.MakeVisible;
  end;
end;

procedure TRequestManager.SetRequestNodeStyle(AValue: TRequestNodeStyle);
begin
  if FRequestNodeStyle = AValue then
    Exit; //=>
  FRequestNodeStyle := AValue;
  UpdateRequestNodeStyle;
end;

procedure TRequestManager.SetImgIdxRoot(AValue: Integer);
begin
  if FImgIdxRoot = AValue then
    Exit; // =>
  FImgIdxRoot := AValue;
  FRootNode.StateIndex := AValue;
end;

function TRequestManager.GetCurrentRequest: TSavedRequest;
begin
  Result := NIL;
  if Assigned(FCurrentNode) then begin
    if FCurrentNode.Data = NIL then
      raise ENodeException.CreateNode(FCurrentNode, 'Runtime error: node data is not request.');
    Result := TSavedRequest(FCurrentNode.Data);
  end;
end;

procedure TRequestManager.SetRootName(AValue: string);
begin
  if RootName = AValue then
    Exit; // =>
  FRootNode.Text := AValue;
end;

function TRequestManager.InternalAddFolder(ParentNode: TTreeNode;
  FolderName: string): TTreeNode;
begin
  Result := AddChildNode(ParentNode, FolderName);
  Result.Data := NIL;
  Result.MakeVisible;
  Result.StateIndex := FImgIdxFolder;
  SortNodes(ParentNode);
end;

procedure TRequestManager.InternalSaveToXml(Doc: TXMLDocument; XmlRoot: TDOMNode; aNode: TTreeNode);
var
  Child: TTreeNode;
  SR: TSavedRequest;
  XmlNode: TDOMNode;
begin
  if not Assigned(aNode) then
    Exit; // =>
  Child := aNode.GetFirstChild;
  while Child <> NIL do begin
    if IsFolderNode(Child) then begin // Folder node.
      XmlNode := Doc.CreateElement('Folder');
      TDOMElement(XmlNode).SetAttribute('name', Child.Text);
      TDOMElement(XmlNode).SetAttribute('expanded', IfThen(Child.Expanded, '1', '0'));
      XmlRoot.AppendChild(XmlNode);
      InternalSaveToXml(Doc, XmlNode, Child);
    end
    else begin // Request node.
      XmlNode := Doc.CreateElement('Request');
      SR := TSavedRequest(Child.Data);
      with TDOMElement(XmlNode) do begin
        SetAttribute('name', SR.Name);
        SetAttribute('locked', IfThen(SR.Locked, '1', '0'));
        SetAttribute('tabRequest', SR.TabRequest);
        SetAttribute('tabResponse', SR.TabResponse);
      end;
      XmlNode.AppendChild(Doc.CreateCDATASection(SR.Request.ToJson));
      XmlRoot.AppendChild(XmlNode);
    end;
    Child := Child.GetNextSibling;
  end; // while
end;

procedure TRequestManager.InternalLoadFromXml(XmlNode: TDOMNode; aNode: TTreeNode);
var
  Elem: TDOMElement;
  Child: TDOMNode;
  SR: TSavedRequest;
  fNode: TTreeNode;
  ExpandedState: Boolean; // preserve a node expanded state when adding the request
  NodePath: string;
begin
  if XmlNode = NIL then // Stops if reached a leaf.
    Exit; // =>
  Child := XmlNode.FirstChild;
  NodePath := GetNodePath(aNode);
  // save the expanded state and restore it after adding a request.
  ExpandedState := aNode.Expanded;
  while Child <> NIL do begin
    Elem := TDOMElement(Child);
    case LowerCase(Child.NodeName) of
      'folder': begin
        fNode := InternalAddFolder(aNode, Elem.GetAttribute('name'));
        InternalLoadFromXml(Child, fNode);
        fNode.Expanded := (Elem.GetAttribute('expanded') = '1');
      end;
      'request': begin
        SR := TSavedRequest.Create(Elem.GetAttribute('name'));
        SR.Locked := Elem.GetAttribute('locked') = '1';
        SR.Request := TRequestObject.CreateFromJson(Child.TextContent);
        SR.TabRequest := Elem.GetAttribute('tabRequest');
        SR.TabResponse := Elem.GetAttribute('tabResponse');
        fNode := AddRequest(SR, NodePath);
        // preserve the request parent folder collapsed/expanded state
        fNode.Parent.Expanded := ExpandedState;
      end;
    end;
    Child := Child.NextSibling;
  end;
end;

procedure TRequestManager.SetNodeSelectedImage(aNode: TTreeNode;
  NeedReset: Boolean);
var
  i: Integer;
begin
  if NeedReset then i := -1 else i := FImgIdxSelected;
  aNode.SelectedIndex := i;
  aNode.ImageIndex := i;
end;

procedure TRequestManager.SortNodes(ParentNode: TTreeNode);
begin
  ParentNode.CustomSort(@SortNodeCompare);
end;

function TRequestManager.SortNodeCompare(Node1, Node2: TTreeNode): integer;
begin
  // Folders come first.
  if (Node1.Data = NIL) and (Node2.Data <> NIL) then Exit(-1);
  if (Node1.Data <> NIL) and (Node2.Data = NIL) then Exit(1);
  // Compare folders or requests by name.
  if ((Node1.Data = NIL) and (Node2.Data = NIL))
     or ((Node1.Data <> NIL) and (Node2.Data <> NIL)) then
    Exit(AnsiCompareStr(Node1.Text, Node2.Text));
end;

procedure TRequestManager.SetNodeStyle(aNode: TTreeNode);
var
  Prefix: string;
  Idx: SmallInt;
  SR: TSavedRequest;
begin
  SR := NodeToRequest(aNode);
  case FRequestNodeStyle of
    bnsText: begin
      case SR.Request.Method of
        'DELETE':  Prefix := 'DEL';
        'OPTIONS': Prefix := 'OPT';
        'PATCH':   Prefix := 'PAT';
        else       Prefix := SR.Request.Method;
      end;
      aNode.Text := Format('%s %s', [Prefix, SR.Name]);
      aNode.StateIndex := -1;
    end;
    bnsIcon: begin
      case SR.Request.Method of
        'GET':     Idx := 1;
        'POST':    Idx := 2;
        'PUT':     Idx := 3;
        'OPTIONS': Idx := 4;
        'DELETE':  Idx := 5;
        'PATCH':   Idx := 6;
        'HEAD':    Idx := 7;
        else       Idx := -1;
      end;
      aNode.Text := SR.Name;
      aNode.StateIndex := Idx;
    end;
    else begin
      aNode.Text := SR.Name;
      aNode.StateIndex := -1;
    end;
  end;
end;

procedure TRequestManager.UpdateRequestNodeStyle;
  procedure InternalIterate(aNode: TTreeNode);
  var
    Child: TTreeNode;
  begin
    if not Assigned(aNode) then
      Exit; //=>
    Child := aNode.GetFirstChild;
    while Child <> NIL do begin
      if IsFolderNode(Child) then
        InternalIterate(Child)
      else
        SetNodeStyle(Child);
      Child := Child.GetNextSibling;
    end;
  end;
begin
  InternalIterate(FRootNode);
end;

function TRequestManager.AddChildNode(ParentNode: TTreeNode;
  const NodeText: string): TTreeNode;
begin
  Result := FRootNode.TreeNodes.AddChild(ParentNode, NodeText);
end;

function TRequestManager.GetNodeFolderPath(aNode: TTreeNode): string;
var
  path: string;
  p: SizeInt;
begin
  path := GetNodePath(aNode);
  if IsFolderNode(aNode) then
    Exit(path);
  p := RPos('/', path);
  Result := LeftStr(path, p - 1);
end;

procedure TRequestManager.SaveXmlToStream(S: TStream);
var
  Doc: TXMLDocument;
  XmlRoot: TDOMNode;
begin
  try
    Doc := TXMLDocument.Create;
    XmlRoot := Doc.CreateElement('Requests');
    TDOMElement(XmlRoot).SetAttribute('name', RootName);
    Doc.AppendChild(XmlRoot);
    InternalSaveToXml(Doc, XmlRoot, FRootNode);
    WriteXML(Doc, S);
  finally
    Doc.Free;
  end;
end;

procedure TRequestManager.LoadXmlFromStream(S: TStream);
var
  Doc: TXMLDocument;
  XmlRoot: TDOMNode;
begin
  try
    ReadXMLFile(Doc, S);
    XmlRoot := Doc.FindNode('Requests');
    if not Assigned(XmlRoot) then
      raise Exception.Create('Malformed xml input.');
    FRootNode.Text := TDOMElement(XmlRoot).GetAttribute('name');
    while XmlRoot <> NIL do begin
      InternalLoadFromXml(XmlRoot, FRootNode);
      XmlRoot := XmlRoot.NextSibling;
    end;
  finally
    Doc.Free;
  end;
end;

function TRequestManager.GetRequestPath(SR: TSavedRequest): string;
var
  bNode: TTreeNode;
begin
  Result := '';
  bNode := FindNode(SR);
  if bNode = NIL then
    Exit; // =>
  Result := GetNodePath(bNode);
end;

constructor TRequestManager.Create(ARootNode: TTreeNode);
begin
  inherited Create;
  FCurrentNode := nil;
  FRequestNodeStyle := bnsNone;
  FRootNode := ARootNode;
end;

destructor TRequestManager.Destroy;
begin
  FOnDeleteRequest := nil; // Don't emit the event on the component destroy.
  DeleteFolderNode(FRootNode);
  inherited Destroy;
end;

function TRequestManager.AddRequest(SR: TSavedRequest; FolderPath: string
  ): TTreeNode;
var
  FolderNode: TTreeNode;
begin
  FolderNode := FindFolder(FolderPath);
  if FolderNode = NIL then
    raise ENodePathNotFound.CreatePath(FolderPath);
  if FolderNode.FindNode(SR.Name) <> NIL then
    raise ENodeException.CreateNode(FolderNode, Format('Name "%s" already exists.', [SR.Name]));
  Result := AddChildNode(FolderNode, '');
  Result.Data := SR;
  Result.Parent.Expanded := True;
  SR.TreeNode := Result;
  SetNodeStyle(Result);
  SortNodes(FolderNode);
  if Assigned(FOnAddRequest) then
    FOnAddRequest(SR, FolderPath);
end;

procedure TRequestManager.AttachFolderNodes(CustomTree: TCustomTreeView);
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
  NewParent: TTreeNode;
begin
  CustomTree.Items.Clear;
  NewParent := CustomTree.Items.AddChild(NIL, FRootNode.Text);
  WalkNodes(FRootNode, NewParent);
end;

function TRequestManager.AddFolder(FolderPath: string): TTreeNode;
var
  p: SizeInt;
  CurFolder: string;
  NextNode: TTreeNode;
begin
  Result := NIL;
  repeat
    p := system.Pos('/', FolderPath);
    if p > 0 then begin
      CurFolder := LeftStr(FolderPath, p-1);
      system.Delete(FolderPath, 1, p);
    end
    else begin
      CurFolder := FolderPath;
      FolderPath := '';
    end;
    if Result = NIL then begin
      if CurFolder <> FRootNode.Text then
        raise ENodePathNotFound.CreatePath(CurFolder);
      Result := FRootNode;
    end
    else begin
      NextNode := Result.FindNode(CurFolder);
      if NextNode = NIL then
        NextNode := InternalAddFolder(Result, CurFolder);
      Result := NextNode;
    end;
  until FolderPath = '';
end;

function TRequestManager.RenameFolder(FolderPath, NewName: string): Boolean;
var
  Folder: TTreeNode;
  p, posCount: SizeInt;
  newPath, OldName: string;
  // Iterate over the folder's child nodes and call OnMove event
  // for the request nodes.
  procedure MoveNodes(AFolder: TTreeNode);
  var
    Child: TTreeNode;
    SR: TSavedRequest;
    NodeOldPath: string;
  begin
    Child := AFolder.GetFirstChild;
    while Child <> NIL do
    begin
      if IsFolderNode(Child) then
        MoveNodes(Child)
      else
        begin
          SR := NodeToRequest(Child);
          NodeOldPath := LeftStr(SR.Path, p) + OldName + Copy(SR.Path, posCount);
          FOnMoveRequest(SR, NodeOldPath);
        end;
      Child := Child.GetNextSibling;
    end;
  end;

begin
  if Length(Trim(NewName)) = 0 then // is empty name ?
    Exit(False); // =>
  if system.Pos('/', NewName) <> 0 then // no slashes in the new name.
    Exit(False); // =>
  p := RPos('/', FolderPath);
  if p = 0 then
    NewPath := NewName // It's a root node.
  else
    newPath := LeftStr(FolderPath, p - 1) + '/' + NewName;
  posCount := p + Length(NewName) + 1;
  if FindNodePath(FRootNode, newPath) <> NIL then
    Exit(False);
  Folder := FindFolder(FolderPath);
  if Folder = NIL then
    Exit(False); // =>
  OldName := Folder.Text;
  Folder.Text := NewName;
  // Request nodes are moved too. So expose the move event on them.
  if Assigned(FOnMoveRequest) then
    MoveNodes(Folder);
  Result := True;
end;

procedure TRequestManager.ResetCurrent;
begin
  if Assigned(FCurrentNode) then begin
    SetNodeSelectedImage(FCurrentNode, True);
  end;
  FCurrentNode := NIL;
end;

function TRequestManager.UpdateRequest(SR: TSavedRequest; ANewName, FolderPath: string): Boolean;
var
  bNode, newNode: TTreeNode;
  isCurrent: Boolean;
  oldPath: string;
begin
  bNode := FindNode(SR);
  if not Assigned(bNode) then
    Exit(False); // =>
  oldPath := SR.Path;
  // Rename.
  if (ANewName <> '') and (SR.Name <> ANewName) then begin
    SR.Name := ANewName;
    SetNodeStyle(bNode);
  end;
  // Move to another folder.
  if (FolderPath <> '') and (GetNodeFolderPath(bNode) <> FolderPath) then begin
    isCurrent := (bNode = FCurrentNode);
    newNode := AddRequest(SR, FolderPath);
    FRootNode.TreeNodes.Delete(bNode);
    bNode := newNode;
    if isCurrent then
    begin
      CurrentNode := bNode;
      SetNodeSelectedImage(bNode);
    end;
  end;
  if (oldPath <> SR.Path) and (Assigned(FOnMoveRequest)) then
    FOnMoveRequest(SR, OldPath);
end;

function TRequestManager.IsCurrentRequest(RO: TRequestObject): Boolean;
var
  SR: TSavedRequest;
begin
  SR := GetCurrentRequest;
  if not Assigned(SR) then
    Exit(False);
  Result := SR.Request.UrlPath = RO.UrlPath;
end;

function TRequestManager.FindFolder(FolderPath: string): TTreeNode;
begin
  Result := FindNodePath(FRootNode, FolderPath);
  if (Result <> NIL) and IsFolderNode(Result) then
    Exit;
  Result := NIL;
end;

function TRequestManager.DeleteRequest(SR: TSavedRequest): Boolean;
var
  bNode: TTreeNode;
begin
  bNode := FindNode(SR);
  if not Assigned(bNode) then
    Exit(False); // =>
  if bNode = FCurrentNode then
    ResetCurrent;
  if Assigned(FOnDeleteRequest) then
    FOnDeleteRequest(SR);
  FreeAndNil(SR);
  bNode.Delete;
  Result := True;
end;

function TRequestManager.DeleteFolder(FolderPath: string): Boolean;
var
  fNode: TTreeNode;
begin
  fNode := FindFolder(FolderPath);
  if fNode = NIL then
    Exit(False);
  Result := DeleteFolderNode(fNode);
end;

function TRequestManager.DeleteFolderNode(FolderNode: TTreeNode): Boolean;
  // Recursive node delete.
  procedure DeleteNode(aNode: TTreeNode);
  var
    SR: TSavedRequest;
  begin
    if aNode = FCurrentNode then
      ResetCurrent;
    while aNode.HasChildren do
      DeleteNode(aNode.GetLastChild);
    if Assigned(aNode.Data) then
    begin
      SR := TSavedRequest(aNode.Data);
      if Assigned(FOnDeleteRequest) then
        FOnDeleteRequest(SR);
      FreeAndNil(SR);
    end;
    aNode.Delete;
  end;

begin
  if not IsFolderNode(FolderNode) then
    Exit(False);
  DeleteNode(FolderNode);
  Result := True;
end;

function TRequestManager.FindNode(SR: TSavedRequest): TTreeNode;
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
      if Child.Data = Pointer(SR) then
        Exit(Child);
      Child := Child.GetNextSibling;
    end;
  end;
begin
  Result := InternalFind(FRootNode);
end;

procedure TRequestManager.OpenRequest(SR: TSavedRequest);
var
  Selected: TTreeNode;
  Prev: TSavedRequest;
begin
  Selected := FindNode(SR);
  // The node must be selected and is not be a folder node.
  if not (Assigned(Selected) and (not IsFolderNode(Selected))) then
    Exit; // =>
  if Selected = FCurrentNode then
    Exit; // =>
  Prev := GetCurrentRequest;
  CurrentNode := Selected;
  if Assigned(FOnChangeRequest) then begin
    FOnChangeRequest(Prev, GetCurrentRequest);
    CurrentNode := Selected; // Preserve selected node after user callback.
  end;
  CurrentNode.Selected := True;
end;

function TRequestManager.OpenRequestPath(Path: string): TSavedRequest;
var
  bNode: TTreeNode;
begin
  Result := NIL;
  Path := Trim(Path);
  if Length(Path) = 0 then
    Exit; // =>
  bNode := FindNodePath(FRootNode, Path);
  if (bNode = NIL) or (IsFolderNode(bNode)) then
    Exit; // =>
  Result := NodeToRequest(bNode);
  OpenRequest(Result);
end;

procedure TRequestManager.OpenNode(aNode: TTreeNode);
begin
  if IsFolderNode(aNode) then
    Exit; // =>
  OpenRequest(NodeToRequest(aNode));
end;

end.
