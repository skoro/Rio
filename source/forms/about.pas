unit about;

{$mode objfpc}{$H+}

interface

uses
  Forms, ExtCtrls,
  StdCtrls, ButtonPanel;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    FormButtons: TButtonPanel;
    ImgLogo: TImage;
    LblAppName: TLabel;
    LblAppVer: TLabel;
    LblBuidDate: TLabel;
    LblTargetCPU: TLabel;
    LblTargetOS: TLabel;
    LblWidgetSet: TLabel;
    InfoPanel: TPanel;
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
  //LabelVersion.Caption := 'Version: ' + APP_VER;
end;

end.

