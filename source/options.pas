unit options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbJsonExpanded: TCheckBox;
    Panel1: TPanel;
    Panel2: TPanel;
  private

  public

  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

end.

