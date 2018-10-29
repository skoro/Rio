unit about;

{$mode objfpc}{$H+}

interface

uses
  Forms, ExtCtrls,
  StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Image1: TImage;
    LabelVersion: TLabel;
    Panel1: TPanel;
    StaticText1: TStaticText;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}
const {$I version.inc};

{ TAboutForm }

procedure TAboutForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  LabelVersion.Caption := 'Version: ' + APP_VER;
end;

end.

