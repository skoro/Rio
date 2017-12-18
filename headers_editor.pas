unit headers_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids;

type

  { THeadersEditorForm }

  THeadersEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    StringGrid1: TStringGrid;
  private

  public

  end;

var
  HeadersEditorForm: THeadersEditorForm;

implementation

{$R *.lfm}

end.

