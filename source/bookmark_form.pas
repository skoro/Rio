unit bookmark_form;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel,
  ExtCtrls, StdCtrls, ComCtrls, bookmarks, request_object, Controls;

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
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure tvFoldersEditingEnd(Sender: TObject; Node: TTreeNode;
      Cancel: Boolean);
  private
    FNewNode: TTreeNode;
    FOnNewFolder: TBookmarkNewFolder;
    function GetFolderNode: TTreeNode;
  public
    property TreeView: TTreeView read tvFolders;
    property FolderNode: TTreeNode read GetFolderNode;
    function CreateBookmark(RO: TRequestObject): TBookmark;
    property OnNewFolder: TBookmarkNewFolder read FOnNewFolder write FOnNewFolder;
  end;

var
  BookmarkForm: TBookmarkForm;

implementation

uses thread_http_client, sysutils;

{$R *.lfm}

{ TBookmarkForm }

procedure TBookmarkForm.OKButtonClick(Sender: TObject);
begin
  if Trim(edName.Text) = '' then
  begin
    edName.SetFocus;
    Exit; //=>
  end;
  ModalResult := mrOK;
end;

procedure TBookmarkForm.tvFoldersEditingEnd(Sender: TObject; Node: TTreeNode;
  Cancel: Boolean);
begin
  // Delete currently inserted node on canceling edit.
  if Cancel and Assigned(FNewNode) and (FNewNode = Node) then
    FNewNode.Delete;
  if not Cancel then begin
    Node.Selected := True;
    if Assigned(FOnNewFolder) then
      FOnNewFolder(Self, Node.GetTextPath);
  end;
  FNewNode := Nil;
end;

function TBookmarkForm.GetFolderNode: TTreeNode;
begin
  Result := tvFolders.Selected;
  if Result = NIL then
    Result := tvFolders.Items.GetFirstNode;
end;

function TBookmarkForm.CreateBookmark(RO: TRequestObject): TBookmark;
begin
  ButtonPanel.CloseButton.Visible := False;
  edName.Text := GetRequestFilename(RO.Url);
  if ShowModal <> mrOK then
    Exit(NIL); //=>
  Result := TBookmark.Create(edName.Text);
  Result.Request := RO;
end;

procedure TBookmarkForm.FormCreate(Sender: TObject);
begin
  ButtonPanel.OKButton.ModalResult := mrNone;
  FNewNode := NIL;
end;

procedure TBookmarkForm.btnNewFolderClick(Sender: TObject);
var
  root: TTreeNode;
begin
  root := tvFolders.Selected;
  if root = NIL then
    root := tvFolders.Items.GetFirstNode;
  FNewNode := tvFolders.Items.AddChild(root, '');
  FNewNode.MakeVisible;
  FNewNode.EditText;
end;

end.

