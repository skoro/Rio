unit bookmark_form;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel,
  ExtCtrls, StdCtrls, ComCtrls, bookmarks, request_object, Controls, Classes;

const
  mrAdded = mrLast + 1; // A new bookmark was added.
  mrDeleted = mrLast + 2; // Modal result for delete operation.

type

  { TBookmarkForm }

  TBookmarkForm = class(TForm)
    btnNewFolder: TButton;
    ButtonPanel: TButtonPanel;
    cbLock: TCheckBox;
    edName: TEdit;
    lFolder: TLabel;
    lName: TLabel;
    pOptions: TPanel;
    pFolders: TPanel;
    pFolderBtn: TPanel;
    pName: TPanel;
    tvFolders: TTreeView;
    procedure btnNewFolderClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure tvFoldersEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure tvFoldersEditingEnd(Sender: TObject; Node: TTreeNode;
      Cancel: Boolean);
  private
    FIsNewNode: Boolean; // A new folder was added.
    FBookmarkManager: TBookmarkManager;
    FPrevPath: string; // Keep an original node path before editing node.
    FPrevName: string; // Keep an source node name before editing node.
    FRequestObject: TRequestObject;
    FBookmark: TBookmark; // Edited bookmark.
    function GetBookmarkManager: TBookmarkManager;
    function GetBookmarkName: string;
    function GetDeleteEnabled: Boolean;
    function GetFolderPath: string;
    function GetRequestObject: TRequestObject;
    procedure SetDeleteEnabled(AValue: Boolean);
  public
    function ShowModal: TModalResult; override;
    function ShowModal(BM: TBookmark; RO: TRequestObject): TModalResult; overload;
    procedure PrepareEditForm; virtual;
    procedure PrepareAddForm; virtual;
    procedure AddBookmark; virtual;
    procedure UpdateBookmark; virtual;
    procedure DeleteBookmark; virtual;

    property Bookmark: TBookmark read FBookmark write FBookmark;
    property BookmarkManager: TBookmarkManager read GetBookmarkManager write FBookmarkManager;
    property RequestObject: TRequestObject read GetRequestObject write FRequestObject;
    property DeleteEnabled: Boolean read GetDeleteEnabled write SetDeleteEnabled;
    property FolderPath: string read GetFolderPath;
    property BookmarkName: string read GetBookmarkName;
  end;

var
  BookmarkForm: TBookmarkForm;

implementation

uses thread_http_client, app_helpers, sysutils;

{$R *.lfm}

{ TBookmarkForm }

procedure TBookmarkForm.OKButtonClick(Sender: TObject);
begin
  // Don't allow to close form without a bookmark name.
  if Trim(edName.Text) = '' then
  begin
    edName.SetFocus;
    Exit; //=>
  end;
  ModalResult := mrOK;
end;

procedure TBookmarkForm.tvFoldersEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  // Don't allow to edit the root node.
  if Node.Parent = NIL then begin
    AllowEdit := False;
    Exit; // =>
  end;
  // Start editing the existing node.
  FPrevName := Node.Text;
  FPrevPath := Node.GetTextPath;
  FIsNewNode := False;
end;

procedure TBookmarkForm.tvFoldersEditingEnd(Sender: TObject; Node: TTreeNode;
  Cancel: Boolean);
begin
  // A new node has been added.
  if FIsNewNode then begin
    // Don't add a cancelled node or node without name.
    if Cancel or (Trim(Node.Text) = '') then
      Node.Delete
    else begin
      Node.Selected := True;
      if BookmarkManager.AddFolder(Node.GetTextPath) = NIL then begin
        ERRMsg('Error', 'Cannot add folder: ' + Node.Text);
        Node.Delete;
      end;
      FIsNewNode := False;
    end;
  end
  // An existing node has been edited.
  else
    if FPrevPath <> '' then begin
      // Revert the previous name on cancel or empty name.
      if Cancel or (Trim(Node.Text) = '') or (FPrevName = Node.Text) then
        Node.Text := FPrevName
      else
        if not BookmarkManager.RenameFolder(FPrevPath, Node.Text) then begin
          ERRMsg('Error', 'Cannot rename folder to: ' + Node.Text);
          Node.Text := FPrevName;
        end;
      FPrevName := '';
      FPrevPath := '';
    end;
end;

function TBookmarkForm.GetFolderPath: string;
var
  fNode: TTreeNode;
begin
  fNode := tvFolders.Selected;
  if not Assigned(fNode) then
    fNode := tvFolders.Items.GetFirstNode;
  if not Assigned(fNode) then
    raise Exception.Create('Cannot get a folder node.');
  Result := fNode.GetTextPath;
end;

function TBookmarkForm.GetRequestObject: TRequestObject;
begin
  if not Assigned(FRequestObject) then
    raise Exception.Create('Request object is required.');
  Result := FRequestObject;
end;

procedure TBookmarkForm.SetDeleteEnabled(AValue: Boolean);
begin
  ButtonPanel.CloseButton.Visible := AValue;
end;

function TBookmarkForm.ShowModal: TModalResult;
begin
  if (not Assigned(FBookmark)) or (not Assigned(FRequestObject)) then
    raise Exception.Create('Bookmark or request object is required.');
  Result := inherited ShowModal;
end;

function TBookmarkForm.GetBookmarkManager: TBookmarkManager;
begin
  if not Assigned(FBookmarkManager) then
    raise Exception.Create('Bookmark manager is required.');
  Result := FBookmarkManager;
end;

function TBookmarkForm.GetBookmarkName: string;
begin
  Result := edName.Text;
end;

function TBookmarkForm.GetDeleteEnabled: Boolean;
begin
  Result := ButtonPanel.CloseButton.Visible;
end;

function TBookmarkForm.ShowModal(BM: TBookmark; RO: TRequestObject
  ): TModalResult;
var
  IsNew: Boolean;
begin
  FBookmark := BM;
  FRequestObject := RO;

  IsNew := (not Assigned(BM)) and Assigned(RO);

  BookmarkManager.AttachFolderNodes(tvFolders);

  if IsNew then
    PrepareAddForm
  else
    PrepareEditForm;

  Result := inherited ShowModal;
  if IsNew and (Result = mrOK) then begin
    AddBookmark;
    Result := mrAdded;
  end;
  if not IsNew and (Result = mrOK) then begin
    UpdateBookmark;
  end;
end;

procedure TBookmarkForm.PrepareEditForm;
var
  srcNode, dstNode: TTreeNode;
  path: string;
begin
  DeleteEnabled := True;
  edName.Text := FBookmark.Name;
  // Select the bookmark node by default.
  srcNode := FBookmarkManager.FindNode(FBookmark);
  if not Assigned(srcNode) then
    { TODO : should be logged ? }
    Exit; // =>
  path := FBookmarkManager.GetNodeFolderPath(srcNode);
  dstNode := tvFolders.Items.FindNodeWithTextPath(path);
  if Assigned(dstNode) then
    with dstNode do begin
      Selected := True;
      MakeVisible;
    end;
  { TODO : path not found. Should be logged ? }
end;

procedure TBookmarkForm.PrepareAddForm;
begin
  DeleteEnabled := False;
  edName.Text := GetRequestFilename(RequestObject.Url);
end;

procedure TBookmarkForm.AddBookmark;
var
  BM: TBookmark;
  NewNode: TTreeNode;
begin
  BM := TBookmark.Create(BookmarkName);
  BM.Request := RequestObject;
  NewNode := BookmarkManager.AddBookmark(BM, FolderPath);
  BookmarkManager.CurrentNode := NewNode;
end;

procedure TBookmarkForm.UpdateBookmark;
begin
  BookmarkManager.UpdateBookmark(FBookmark, BookmarkName, FolderPath);
end;

procedure TBookmarkForm.DeleteBookmark;
begin
  BookmarkManager.DeleteBookmark(FBookmark);
end;

procedure TBookmarkForm.FormCreate(Sender: TObject);
begin
  ButtonPanel.OKButton.ModalResult := mrNone;
  FIsNewNode := False;
end;

procedure TBookmarkForm.FormShow(Sender: TObject);
begin
  edName.SetFocus;
end;

procedure TBookmarkForm.btnNewFolderClick(Sender: TObject);
var
  root: TTreeNode;
begin
  root := tvFolders.Selected;
  if root = NIL then
    root := tvFolders.Items.GetFirstNode;
  with tvFolders.Items.AddChild(root, '') do begin
    MakeVisible;
    EditText;
  end;
  FIsNewNode := True;
end;

procedure TBookmarkForm.CloseButtonClick(Sender: TObject);
begin
  ModalResult := mrNone;
  if not BookmarkManager.Popup.ConfirmDeleteBookmark(FBookmark) then
    Exit; // =>
  DeleteBookmark;
  ModalResult := mrDeleted;
end;

end.

