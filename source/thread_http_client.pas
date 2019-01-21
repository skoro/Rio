unit thread_http_client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fgl;

type

  { TCustomHttpClient }

  TCustomHttpClient = class(TFPHTTPClient)
  public
    procedure MultiFileStreamFormPost(FormData, FileNames: TStrings);
  end;

  { TQueryParams }

  TQueryParams = specialize TFPGMap<string, string>;

  { TRequestInfo }

  TResponseInfo = class
  private
    FContent: TStringStream;
    FContentType: string;
    FHttpVersion: string;
    FMethod: string;
    FRequestHeaders: TStrings;
    FResponseHeaders: TStrings;
    FStatusCode: Integer;
    FStatusText: string;
    FTime: Int64;
    FUrl: string;
  public
    constructor Create;
    destructor Destroy; override;
    property StatusCode: Integer read FStatusCode write FStatusCode;
    property StatusText: string read FStatusText write FStatusText;
    property HttpVersion: string read FHttpVersion write FHttpVersion;
    property Url: string read FUrl write FUrl;
    property Method: string read FMethod write FMethod;
    property RequestHeaders: TStrings read FRequestHeaders;
    property ResponseHeaders: TStrings read FResponseHeaders;
    property Content: TStringStream read FContent;
    property ContentType: string read FContentType write FContentType;
    property Time: Int64 read FTime write FTime;
  end;

  TOnRequestComplete = procedure(ResponseInfo: TResponseInfo) of object;
  TOnException = procedure(Url, Method: string; E: Exception) of object;

  { TThreadHttpClient }

  TThreadHttpClient = class(TThread)
  private
    FHttpClient: TCustomHttpClient;
    FHttpMethod: string;
    FUrl: string;
    FOnRequestComplete: TOnRequestComplete;
    FResponseData: TStringStream;
    FOnClientException: TOnException;
    FException: Exception;
    FStartTime: TDateTime;
    FFinishTime: TDateTime;
    FCookies: TStrings;
    function GetRequestBody: TStream;
    procedure SetHttpMethod(AValue: string);
    procedure SetRequestBody(AValue: TStream);
    procedure SetUrl(AValue: string);
    procedure RequestComplete;
    procedure OnClientException;
  protected
    procedure Execute; override;
    function ParseContentType: string;
  public
    constructor Create(CreateSuspened: boolean);
    destructor Destroy; override;
    procedure AddHeader(const AHeader, AValue: string);
    procedure AddCookie(const AName, AValue: string; EncodeValue: boolean = True);
    property Client: TCustomHttpClient read FHttpClient;
    property Method: string read FHttpMethod write SetHttpMethod;
    property Url: string read FUrl write SetUrl;
    property RequestBody: TStream read GetRequestBody write SetRequestBody;
    property OnRequestComplete: TOnRequestComplete
      read FOnRequestComplete write FOnRequestComplete;
    property OnException: TOnException read FOnClientException write FOnClientException;
  end;

  { TMimeType }

  TMimeType = record
    MimeType: string;
    Subtype: string;
  end;

function DecodeUrl(const url: string): string;
function GetURLQueryParams(const url: string): TQueryParams;
function ReplaceURLQueryParams(const url: string; Params: TQueryParams): string;
function SplitMimeType(const ContentType: string): TMimeType;

implementation

uses dateutils, strutils, URIParser, app_helpers;

const
  CRLF = #13#10;

function DecodeUrl(const url: string): string;
begin
  Result := ReplaceStr(url, '+', ' ');
  Result := DecodeURLElement(Result);
end;

function GetURLQueryParams(const url: string): TQueryParams;
var
  URI: TURI;
  Params, KV: TStringList;
  I: integer;
begin
  Result := TQueryParams.Create;
  Params := TStringList.Create;
  KV := TStringList.Create;
  try
    URI := ParseURI(url);
    SplitStrings(URI.Params, '&', Params);
    for I := 0 to Params.Count - 1 do
    begin
      SplitStrings(Params[I], '=', KV);
      case KV.Count of
        1: Result.Add(DecodeURLElement(KV[0]), '');
        // Fix: DecodeURLElement on empty string leads to use the value from the previous iteration.
        2: Result.Add(DecodeURLElement(KV[0]), IfThen(KV[1] = '', '',
            DecodeURLElement(KV[1])));
      end;
    end;
  finally
    FreeAndNil(KV);
    FreeAndNil(Params);
  end;
end;

function ReplaceURLQueryParams(const url: string; Params: TQueryParams): string;
var
  URI: TURI;
  ParamStr, Key, Data: string;
  I: integer;
begin
  ParamStr := '';
  for I := 0 to Params.Count - 1 do
  begin
    Key := Trim(Params.Keys[I]);
    Data := Params.Data[I];
    if Key <> '' then
      ParamStr := ParamStr + IfThen(Data = '', Key, Format('%s=%s', [Key, Data])) + '&';
  end;
  ParamStr := TrimRightSet(ParamStr, ['&']);

  URI := ParseURI(url);
  URI.Params := ParamStr;
  Result := EncodeURI(URI);
end;

function SplitMimeType(const ContentType: string): TMimeType;
var
  P: Word;
begin
  Result.MimeType := '';
  Result.Subtype  := '';
  P := Pos('/', ContentType);
  if P = 0 then
    Exit;
  Result.MimeType := LowerCase(LeftStr(ContentType, P - 1));
  Result.Subtype  := LowerCase(RightStr(ContentType, Length(ContentType) - P));
end;

{ TResponseInfo }

constructor TResponseInfo.Create;
begin
  FContent := TStringStream.Create('');
  FRequestHeaders := TStringList.Create;
  FResponseHeaders := TStringList.Create;
end;

destructor TResponseInfo.Destroy;
begin
  FreeAndNil(FContent);
  FreeAndNil(FRequestHeaders);
  FreeAndNil(FResponseHeaders);
  inherited Destroy;
end;

{ TCustomHttpClient }

procedure TCustomHttpClient.MultiFileStreamFormPost(FormData, FileNames: TStrings);
var
  N, V, S, Sep: string;
  SS: TStringStream;
  I: integer;
  FS: TFileStream;
begin
  Sep := Format('-----%.8x-%.8x_multipart_boundary', [Random($ffffff), Random($ffffff)]);
  AddHeader('Content-Type', 'multipart/form-data; boundary=' + Sep);
  SS := TStringStream.Create('');
  if (FormData <> nil) then
    for I := 0 to FormData.Count - 1 do
    begin
      FormData.GetNameValue(I, N, V);
      S := '--' + Sep + CRLF;
      S := S + Format('Content-Disposition: form-data; name="%s"' +
        CRLF + CRLF + '%s' + CRLF, [N, V]);
      SS.WriteBuffer(S[1], Length(S));
    end;
  for I := 0 to FileNames.Count - 1 do
  begin
    S := '--' + Sep + CRLF;
    FileNames.GetNameValue(I, N, V);
    S := S + Format('Content-Disposition: form-data; name="%s"; filename="%s"' + CRLF,
      [N, ExtractFileName(V)]);
    S := S + 'Content-Type: application/octet-string' + CRLF + CRLF;
    SS.WriteBuffer(S[1], Length(S));
    try
      FS := TFileStream.Create(V, fmOpenRead);
      FS.Seek(0, soFromBeginning);
      SS.CopyFrom(FS, FS.Size);
      FreeAndNil(FS);
    except
      on E: Exception do
      begin
        //FreeAndNil(FS);
        FreeAndNil(SS);
        raise Exception.Create('Couldn''t read file: ' + V);
      end;
    end;
  end;
  S := CRLF + '--' + Sep + '--' + CRLF;
  SS.WriteBuffer(S[1], Length(S));
  SS.Position := 0;
  RequestBody := SS;
  AddHeader('Content-Length', IntToStr(SS.Size));
end;

{ TThreadHttpClient }

function TThreadHttpClient.GetRequestBody: TStream;
begin
  Result := FHttpClient.RequestBody;
end;

procedure TThreadHttpClient.SetHttpMethod(AValue: string);
begin
  if FHttpMethod = AValue then
    Exit; // =>
  AValue := UpperCase(Trim(AValue));
  if Length(AValue) = 0 then
    AValue := 'GET'; // GET by default.
  FHttpMethod := AValue;
end;

procedure TThreadHttpClient.SetRequestBody(AValue: TStream);
begin
  FHttpClient.RequestBody := AValue;
end;

procedure TThreadHttpClient.SetUrl(AValue: string);
begin
  if FUrl = AValue then
    Exit; // =>
  // TODO: need better protocol parser.
  if Pos('http', AValue) = 0 then
    AValue := 'http://' + AValue;
  FUrl := AValue;
end;

procedure TThreadHttpClient.RequestComplete;
var
  info: TResponseInfo;
begin
  if Assigned(FOnRequestComplete) then
  begin
    info := TResponseInfo.Create;
    info.Method := FHttpMethod;
    info.Url := FUrl;
    info.RequestHeaders.Assign(FHttpClient.RequestHeaders);
    info.ResponseHeaders.Assign(FHttpClient.ResponseHeaders);
    info.StatusCode := FHttpClient.ResponseStatusCode;
    info.StatusText := FHttpClient.ResponseStatusText;
    info.HttpVersion := FHttpClient.ServerHTTPVersion;
    info.Content.WriteString(FResponseData.DataString);
    info.Time := MilliSecondsBetween(FFinishTime, FStartTime);
    info.ContentType := ParseContentType;
    FOnRequestComplete(info);
  end;
end;

procedure TThreadHttpClient.OnClientException;
begin
  if Assigned(FOnClientException) then
    FOnClientException(Url, Method, FException);
end;

procedure TThreadHttpClient.Execute;
begin
  try
    if Assigned(FCookies) then
      FHttpClient.Cookies := FCookies;
    FStartTime := Now;
    FHttpClient.HTTPMethod(FHttpMethod, FUrl, FResponseData, []);
    FFinishTime := Now;
    Synchronize(@RequestComplete);
  except
    on E: Exception do
    begin
      FException := E;
      Synchronize(@OnClientException);
    end;
  end;
end;

function TThreadHttpClient.ParseContentType: string;
var
  I, P: integer;
  Line, Header: string;
begin
  Result := '';
  for I := 0 to FHttpClient.ResponseHeaders.Count - 1 do
  begin
    Line := FHttpClient.ResponseHeaders.Strings[I];
    P := Pos(':', Line);
    if p <> 0 then
    begin
      Header := LeftStr(Line, P - 1);
      if LowerCase(Header) = 'content-type' then
      begin
        Result := Trim(RightStr(Line, Length(Line) - P));
        P := Pos(';', Result);
        if P <> 0 then
          Result := Trim(LeftStr(Result, P - 1));
      end;
    end;
  end;
end;

constructor TThreadHttpClient.Create(CreateSuspened: boolean);
begin
  inherited Create(CreateSuspened);
  FreeOnTerminate := True;
  FHttpClient := TCustomHttpClient.Create(nil);
  FResponseData := TStringStream.Create('');
  FOnClientException := nil;
  FOnRequestComplete := nil;
  FCookies := nil;
end;

destructor TThreadHttpClient.Destroy;
begin
  if Assigned(FCookies) then
    FCookies.Free;
  FHttpClient.RequestBody.Free;
  FreeAndNil(FHttpClient);
  FreeAndNil(FResponseData);
  inherited Destroy;
end;

procedure TThreadHttpClient.AddHeader(const AHeader, AValue: string);
begin
  FHttpClient.AddHeader(AHeader, AValue);
end;

procedure TThreadHttpClient.AddCookie(const AName, AValue: string;
  EncodeValue: boolean = True);
begin
  if not Assigned(FCookies) then
    FCookies := TStringList.Create;
  FCookies.Add(Format('%s=%s',
    [AName, IfThen(EncodeValue, EncodeURLElement(AValue), AValue)]));
end;

end.
