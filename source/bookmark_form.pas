unit bookmark_form;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel,
  ExtCtrls, StdCtrls, ComCtrls, bookmarks, request_object, Classes;

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
  private

  public
    property TreeView: TTreeView read tvFolders;
    function CreateBookmark(RO: TRequestObject): TBookmark;
    function EditBookmark(Bookmark: TBookmark): TModalResult;
  end;

var
  BookmarkForm: TBookmarkForm;

implementation

uses Controls, sysutils;

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

function TBookmarkForm.CreateBookmark(RO: TRequestObject): TBookmark;
begin
  ButtonPanel.CloseButton.Visible := False;
  if ShowModal <> mrOK then
    Exit(NIL); //=>
  Result := TBookmark.Create(edName.Text);
  Result.Request := RO;
end;

function TBookmarkForm.EditBookmark(Bookmark: TBookmark): TModalResult;
begin

end;

procedure TBookmarkForm.FormCreate(Sender: TObject);
begin
  ButtonPanel.OKButton.ModalResult := mrNone;
end;

procedure TBookmarkForm.btnNewFolderClick(Sender: TObject);
begin

end;

end.

