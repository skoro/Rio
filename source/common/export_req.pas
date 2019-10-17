unit export_req;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RequestObject;

type

  { TExport }

  TExport = class
  private
    FOutput: string;
    FRequestObject: TRequestObject;
    procedure SetRequestObject(AValue: TRequestObject);
  protected
    function GetOutput: string; virtual;
    procedure ExportRequestObject; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsGet: Boolean;
    // Compact json string by removing new lines chars.
    function MinimizeJson: string;
    // Format auth bearer header.
    function FormatAuthBearer: string;
    property RequestObject: TRequestObject read FRequestObject write SetRequestObject;
    property Output: string read GetOutput;
  end;

  { TExportException }

  TExportException = class(Exception)

  end;

  { TCurlExport }

  TCurlExport = class(TExport)
  private
    FCurlName: string;
    FOptions: TStrings;
  protected
    procedure ExportRequestObject; override;
    procedure AddOption(Option: string; Value: string = '');
    procedure AddSingleOption(Option: string);
    function GetOptionsStr: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    property CurlName: string read FCurlName write FCurlName;
    property Options: TStrings read FOptions;
  end;

  { TTextExport }

  TTextExport = class(TExport)
  private
    FLines: TStrings;
  protected
    procedure ExportRequestObject; override;
    function GetOutput: string; override;
    procedure Initialize; virtual;
    procedure RenderHeaders; virtual;
    procedure RenderCookies; virtual;
    procedure RenderForm; virtual;
    procedure RenderAuth; virtual;
    procedure Finish; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Lines: TStrings read FLines;
  end;

  { TPHPCurlExport }

  TPHPCurlExport = class(TTextExport)
  private
    FCurlVar: string;
  protected
    procedure Initialize; override;
    procedure Finish; override;
    procedure RenderHeaders; override;
    procedure RenderCookies; override;
    procedure RenderForm; override;
    procedure RenderAuth; override;
    procedure SetCurlOpt(Option, Value: string; QuoteVal: Boolean = True);
  public
    constructor Create; override;
    property CurlVar: string read FCurlVar write FCurlVar;
  end;

implementation

uses strutils, fphttpclient;

{ TTextExport }

procedure TTextExport.ExportRequestObject;
begin
  Initialize;
  RenderHeaders;
  RenderCookies;
  RenderForm;
  RenderAuth;
  Finish;
end;

function TTextExport.GetOutput: string;
begin
  Result := FLines.Text;
end;

procedure TTextExport.Initialize;
begin
  FLines.Clear;
end;

procedure TTextExport.RenderHeaders;
begin
  //
end;

procedure TTextExport.RenderCookies;
begin
  //
end;

procedure TTextExport.RenderForm;
begin
  //
end;

procedure TTextExport.RenderAuth;
begin
  //
end;

procedure TTextExport.Finish;
begin
  //
end;

constructor TTextExport.Create;
begin
  inherited Create;
  FLines := TStringList.Create;
end;

destructor TTextExport.Destroy;
begin
  FreeAndNil(FLines);
  inherited Destroy;
end;

{ TPHPCurlExport }

procedure TPHPCurlExport.Initialize;
begin
  Lines.Add('<?php');
  Lines.Add(Format('%s = curl_init();', [FCurlVar]));

  SetCurlOpt('CURLOPT_URL', FRequestObject.Url);

  if not IsGet then
    SetCurlOpt('CURLOPT_CUSTOMREQUEST', FRequestObject.Method);
end;

procedure TPHPCurlExport.Finish;
begin
  SetCurlOpt('CURLOPT_RETURNTRANSFER', 'true', False);
  Lines.Add(Format('echo curl_exec(%s);', [FCurlVar]));
  Lines.Add(Format('curl_close(%s);', [FCurlVar]));
end;

procedure TPHPCurlExport.RenderHeaders;
var
  Header: TRequestParamItem;
  Buf: TStrings;
begin
  try
    Buf := TStringList.Create;
    for Header in FRequestObject.Headers do
      if Header.Enabled then begin
        // Don't add content-type for GET requests.
        if IsGet and (LowerCase(Header.Name) = 'content-type') then
          Continue;
        Buf.Add(Format('%s: %s', [Header.Name, Header.Value]));
      end;
    // Automatically add content type for json if it missed.
    if (not IsGet) and (not FRequestObject.IsJson) and (FRequestObject.DataType = btJson) then
      Buf.Add('Content-Type: application/json');
    // Expose auth bearer as header.
    if FRequestObject.AuthType = atBearer then
      Buf.Add(FormatAuthBearer);
    if Buf.Count > 0 then
      SetCurlOpt('CURLOPT_HTTPHEADER', Format('array(%s)', [Buf.CommaText]), False);
  finally
    FreeAndNil(Buf);
  end;
end;

procedure TPHPCurlExport.RenderCookies;
var
  Cookie: TRequestParamItem;
  Buf: string;
begin
  if FRequestObject.Cookies.Count = 0 then
    Exit; // =>
  Buf := '';
  for Cookie in FRequestObject.Cookies do
    if Cookie.Enabled then
      Buf := Buf + Format('%s=%s; ', [Cookie.Name, Cookie.Value]);
  if Length(Buf) > 0 then
    SetCurlOpt('CURLOPT_COOKIE', Trim(Buf));
end;

procedure TPHPCurlExport.RenderForm;
var
  FormParam: TFormParamItem;
  Buf: TStrings;
  Val: string;
begin
  if isGet then // Don't render form for a GET request.
    Exit; // =>
  Val := '';
  case FRequestObject.DataType of
    btForm: begin
      try
        Buf := TStringList.Create;
        for FormParam in FRequestObject.Form do
          if FormParam.Enabled then
            Buf.Add(
              EncodeURLElement(FormParam.Name) + '=' +
              IfThen(FormParam.ElemType = ftiFile, '@' + FormParam.Value, EncodeURLElement(FormParam.Value))
            );
        Buf.Delimiter := '&';
        Val := Buf.DelimitedText;
      finally
        FreeAndNil(Buf);
      end;
    end;
    btJson:  Val := MinimizeJson;
    btOther: Val := FRequestObject.Body;
  end;
  if Val <> '' then
    SetCurlOpt('CURLOPT_POSTFIELDS', Val);
end;

procedure TPHPCurlExport.RenderAuth;
begin
  inherited;
  case FRequestObject.AuthType of
    atBasic:
      with FRequestObject do
        SetCurlOpt('CURLOPT_USERPWD', AuthBasic.Login + ':' + AuthBasic.Password);
    atBearer: begin
      // it should be added in headers.
    end;
  end;
end;

procedure TPHPCurlExport.SetCurlOpt(Option, Value: string; QuoteVal: Boolean = True);
begin
  if QuoteVal then
    Value := '''' + Value + '''';
  Lines.Add(Format('curl_setopt(%s, %s, %s);', [FCurlVar, Option, Value]));
end;

constructor TPHPCurlExport.Create;
begin
  inherited Create;
  FCurlVar := '$ch';
end;

{ TCurlExport }

procedure TCurlExport.ExportRequestObject;
var
  Param: TRequestParamItem;
  FormParam: TFormParamItem;
  cookies, formValue: string;
begin
  FOptions.Clear;

  // Request method.
  if not isGet then
    AddOption('-X', UpperCase(FRequestObject.Method));

  // Create curl header options.
  for Param in FRequestObject.Headers do
    if Param.Enabled then begin
      // Don't add content-type for GET requests.
      if (IsGet) and (LowerCase(Param.Name) = 'content-type') then
        Continue;
      // Don't expose 'application/x-www-form-urlencoded' for a form.
      // It'll be added by curl -F option automatically.
      if (FRequestObject.Method = 'POST') and (LowerCase(Param.Name) = 'content-type')
          and (LowerCase(Param.Value) = 'application/x-www-form-urlencoded')
      then
        Continue;
      AddOption('-H', Format('%s: %s', [Param.Name, Param.Value]));
    end;
  if (not IsGet) and (not FRequestObject.IsJson) and (FRequestObject.DataType = btJson) then
    AddOption('-H', 'Content-Type: application/json');

  // Cookie option.
  cookies := '';
  for Param in FRequestObject.Cookies do
    if Param.Enabled then
      cookies := cookies + Format('%s=%s; ', [Param.Name, Param.Value]);
  if cookies <> '' then
    AddOption('--cookie', cookies);

  // Form data: form, json, other.
  if not isGet then // Passing the data except GET requests.
    case FRequestObject.DataType of
      btForm: begin
        for FormParam in FRequestObject.Form do
          if FormParam.Enabled then begin
            case FormParam.ElemType of
              ftiText: formValue := FormParam.Value;
              ftiFile: formValue := '@' + FormParam.Value;
            end;
            AddOption('-F', Format('%s=%s', [FormParam.Name, formValue]));
          end;
      end;
      btJson:  AddOption('--data', MinimizeJson);
      btOther: AddOption('--data', FRequestObject.Body);
    end;

  // Auth options.
  case FRequestObject.AuthType of
    atBasic: begin
      AddSingleOption('--basic');
      AddOption('--user', Format('%s:%s', [FRequestObject.AuthBasic.Login, FRequestObject.AuthBasic.Password]));
    end;
    atBearer: AddOption('-H', FormatAuthBearer);
  end;

  // Format the command line.
  FOutput := Format('%s %s %s', [
          FCurlName, GetOptionsStr, QuotedStr(FRequestObject.Url)
  ]);
end;

procedure TCurlExport.AddOption(Option: string; Value: string = '');
begin
  FOptions.AddStrings([Option, QuotedStr(Value)]);
end;

procedure TCurlExport.AddSingleOption(Option: string);
begin
  FOptions.Add(Option);
end;

function TCurlExport.GetOptionsStr: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FOptions.Count - 1 do
    Result := Result + FOptions[i] + ' ';
end;

constructor TCurlExport.Create;
begin
  inherited Create;
  FCurlName := 'curl';
  FOptions := TStringList.Create;
end;

destructor TCurlExport.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

{ TExport }

procedure TExport.SetRequestObject(AValue: TRequestObject);
begin
  if FRequestObject = AValue then Exit;
  FRequestObject := AValue;
  ExportRequestObject;
end;

function TExport.GetOutput: string;
begin
  Result := FOutput;
end;

procedure TExport.ExportRequestObject;
begin
  raise TExportException.Create('Should be implemented in descent classes');
end;

constructor TExport.Create;
begin
  FRequestObject := nil;
  FOutput := '';
end;

destructor TExport.Destroy;
begin
  inherited Destroy;
end;

function TExport.IsGet: Boolean;
begin
  if not Assigned(FRequestObject) then
    raise TExportException.Create('Request object is not assigned.');
  Result := FRequestObject.Method = 'GET';
end;

function TExport.MinimizeJson: string;
begin
  Result := AnsiReplaceStr(FRequestObject.Json, #13, '');
  Result := AnsiReplaceStr(FRequestObject.Json, #10, '');
  Result := Trim(Result);
end;

function TExport.FormatAuthBearer: string;
begin
  Result := '';
  with FRequestObject do
    Result := 'Authorization: ' + Format('%s %s', [AuthBearer.Prefix, AuthBearer.Token]);
end;

end.

