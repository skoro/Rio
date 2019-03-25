unit bookmark_form;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel,
  ExtCtrls, StdCtrls, ComCtrls;

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
  private

  public

  end;

var
  BookmarkForm: TBookmarkForm;

implementation

{$R *.lfm}

end.

