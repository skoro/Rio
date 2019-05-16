unit bookmark_form;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel,
  ExtCtrls, StdCtrls, ComCtrls, bookmarks, request_object, Controls;

const
  mrAdded = mrLast + 1; // A new bookmark was added.
  mrDeleted = mrLast + 2; // Modal result for delete operation.

type

  { TBookmarkForm }

  TBookmarkForm = class(TForm)
    btnNewFolder: TButton;
    ButtonPanel: TButtonPanel;
    chkSaveResponse: TCheckBox;
    chkSyncResp: TCheckBox;
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
    function GetBookmarkManager: TBookmarkManager;
    function GetBookmarkName: string;
    function GetDeleteEnabled: Boolean;
    function GetFolderPath: string;
    function GetRequestObject: TRequestObject;
    procedure SetDeleteEnabled(AValue: Boolean);
  public
    function ShowModal: TModalResult; override;
    procedure PrepareEditForm; virtual;
    procedure PrepareAddForm; virtual;
    procedure AddBookmark; virtual;
    procedure UpdateBookmark; virtual;
    procedure DeleteBookmark; virtual;

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
      BookmarkManager.AddFolder(Node.GetTextPath);
      FIsNewNode := False;
    end;
  end
  // An existing node has been edited.
  else
    if FPrevPath <> '' then begin
      // Revert the previous name on cancel or empty name.
      if Cancel or (Trim(Node.Text) = '') then
        Node.Text := FPrevName
      else
        BookmarkManager.RenameFolder(FPrevPath, Node.Text);
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

function TBookmarkForm.ShowModal: TModalResult;
var
  IsNew: Boolean;
begin
  IsNew := not Assigned(BookmarkManager.CurrentBookmark);

  if IsNew then
    PrepareAddForm
  else
    PrepareEditForm;

  BookmarkManager.AttachFolderNodes(tvFolders);

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
begin
  DeleteEnabled := True;
  edName.Text := BookmarkManager.CurrentBookmark.Name;
end;

procedure TBookmarkForm.PrepareAddForm;
begin
  DeleteEnabled := False;
  edName.Text := GetRequestFilename(RequestObject.Url);
end;

procedure TBookmarkForm.AddBookmark;
var
  BM: TBookmark;
begin
  BM := TBookmark.Create(edName.Text);
  BM.Request := RequestObject;
  BookmarkManager.AddBookmark(BM, FolderPath);
end;

procedure TBookmarkForm.UpdateBookmark;
begin
  BookmarkManager.UpdateCurrent(BookmarkName, FolderPath);
end;

procedure TBookmarkForm.DeleteBookmark;
begin
  with BookmarkManager do
    DeleteBookmark(CurrentBookmark);
end;

procedure TBookmarkForm.FormCreate(Sender: TObject);
begin
  ButtonPanel.OKButton.ModalResult := mrNone;
  FIsNewNode := False;
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
var
  mr: TModalResult;
begin
  ModalResult := mrNone;
  mr := ConfirmDlg('Delete bookmark',
    Format('Are you sure you want to delete "%s" bookmark ?', [
      BookmarkManager.CurrentBookmark.Name
    ])
  );
  if mr <> mrOK then
    Exit; // =>
  DeleteBookmark;
  ModalResult := mrDeleted;
end;

end.

