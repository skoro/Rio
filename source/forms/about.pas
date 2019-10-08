unit about;

{$mode objfpc}{$H+}

interface

uses
  Forms, ExtCtrls,
  StdCtrls, ButtonPanel, DividerBevel, Classes;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    BtnCopy: TButton;
    DivInfo: TDividerBevel;
    EditUrl: TEdit;
    FormButtons: TButtonPanel;
    ImgLogo: TImage;
    LblHomeUrl: TLabel;
    LblLCLVer: TLabel;
    LblAppName: TLabel;
    LblAppVer: TLabel;
    LblBuidDate: TLabel;
    InfoPanel: TPanel;
    LblFPCVer: TLabel;
    LblTargetCPU: TLabel;
    LblTargetOS: TLabel;
    LblWidgetSet: TLabel;
    LinksPanel: TPanel;
    procedure BtnCopyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

uses InterfaceBase, LCLPlatformDef, Clipbrd;

{$R *.lfm}
const {$I version.inc};
  appBuildDate = {$I %DATE%};
  TargetOS = {$I %FPCTARGETOS%};
  fpcVersion = {$I %FPCVERSION%};
  TargetCPU = {$I %FPCTARGETCPU%};

resourcestring
  sAbout               = 'About...';
  sBuildDate           = 'Build date: ';
  sTargetOS            = 'Target OS: ';
  sTargetCPU           = 'Target CPU: ';
  sWidgetSet           = 'Widget set: ';
  sAppVersion          = 'Version: ';
  sAppName             = 'RIO - REST Api Client';
  sFPCVer              = 'FPC version: ';
  sLCLVer              = 'LCL version: ';

  sHomePage            = 'Home page: ';

{ TAboutForm }

procedure TAboutForm.BtnCopyClick(Sender: TObject);
var
  txt: string;
begin
  txt := LblAppVer.Caption + #13#10 +
         LblBuidDate.Caption + #13#10 +
         LblTargetCPU.Caption + #13#10 +
         LblTargetOS.Caption + #13#10 +
         LblWidgetSet.Caption + #13#10 +
         LblFPCVer.Caption + #13#10 +
         LblLCLVer.Caption + #13#10;
  Clipboard.AsText := txt;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Caption := sAbout;
  LblAppName.Caption := sAppName;
  LblBuidDate.Caption := sBuildDate + appBuildDate;
  LblTargetCPU.Caption := sTargetCPU + TargetCPU;
  LblTargetOS.Caption := sTargetOS + TargetOS;
  LblAppVer.Caption := sAppVersion + APP_VER;
  LblWidgetSet.Caption := sWidgetSet + LCLPlatformDisplayNames[WidgetSet.LCLPlatform];
  LblFPCVer.Caption := sFPCVer + fpcVersion;
  LblLCLVer.Caption := sLCLVer + LCLVersion;
  LblHomeUrl.Caption := sHomePage;
  EditUrl.Text := 'https://github.com/skoro/rio';
end;

end.

