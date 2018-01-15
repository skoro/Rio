unit thread_http_client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient;

type

  { TRequestInfo }

  TResponseInfo = record
    StatusCode: integer;
    StatusText: string;
    HttpVersion: string;
    Url: string;
    Method: string;
    RequestHeaders: TStrings;
    ResponseHeaders: TStrings;
    Content: TStringStream;
  end;

  TOnRequestComplete = procedure (ResponseInfo: TResponseInfo) of object;
  TOnHeaders = procedure (Headers: TStrings) of object;
  TOnException = procedure (Url, Method: string; E: Exception) of object;

  { TThreadHttpClient }

  TThreadHttpClient = class(TThread)
  private
    FHttpClient: TFPHTTPClient;
    FHttpMethod: string;
    FOnHeaders: TOnHeaders;
    FUrl: string;
    FOnRequestComplete: TOnRequestComplete;
    FResponseData: TStringStream;
    FOnClientException: TOnException;
    FException: Exception;
    function GetRequestBody: TStream;
    procedure SetHttpMethod(AValue: string);
    procedure SetRequestBody(AValue: TStream);
    procedure SetUrl(AValue: string);
    procedure RequestComplete;
    procedure OnClientException;
    procedure InternalOnHeaders(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspened: Boolean);
    destructor Destroy; override;
    procedure AddHeader(const AHeader,AValue : String);
    property Method: string read FHttpMethod write SetHttpMethod;
    property Url: string read FUrl write SetUrl;
    property RequestBody: TStream read GetRequestBody write SetRequestBody;
    property OnRequestComplete: TOnRequestComplete read FOnRequestComplete write FOnRequestComplete;
    property OnHeaders: TOnHeaders read FOnHeaders write FOnHeaders;
    property OnException: TOnException read FOnClientException write FOnClientException;
  end;

implementation

{ TThreadHttpClient }

function TThreadHttpClient.GetRequestBody: TStream;
begin
  Result := FHttpClient.RequestBody;
end;

procedure TThreadHttpClient.SetHttpMethod(AValue: string);
begin
  if FHttpMethod = AValue then Exit; // =>
  AValue := UpperCase(Trim(AValue));
  if Length(AValue) = 0 then AValue := 'GET'; // GET by default.
  FHttpMethod := AValue;
end;

procedure TThreadHttpClient.SetRequestBody(AValue: TStream);
begin
  FHttpClient.RequestBody := AValue;
end;

procedure TThreadHttpClient.SetUrl(AValue: string);
begin
  if FUrl = AValue then Exit; // =>
  // TODO: need better protocol parser.
  if Pos('http', AValue) = 0 then AValue := 'http://' + AValue;
  FUrl := AValue;
end;

procedure TThreadHttpClient.RequestComplete;
var
  info: TResponseInfo;
begin
  if Assigned(FOnRequestComplete) then
  begin
    info.Method:=FHttpMethod;
    info.Url:=FUrl;
    info.RequestHeaders:=FHttpClient.RequestHeaders;
    info.ResponseHeaders:=FHttpClient.ResponseHeaders;
    info.StatusCode:=FHttpClient.ResponseStatusCode;
    info.StatusText:=FHttpClient.ResponseStatusText;
    info.HttpVersion:=FHttpClient.ServerHTTPVersion;
    info.Content:=FResponseData;
    FOnRequestComplete(info);
  end;
end;

procedure TThreadHttpClient.OnClientException;
begin
  if Assigned(FOnClientException) then FOnClientException(Url, Method, FException);
end;

procedure TThreadHttpClient.InternalOnHeaders(Sender: TObject);
begin
  if Assigned(FOnHeaders) then
  begin
    FOnHeaders(FHttpClient.ResponseHeaders);
  end;
end;

procedure TThreadHttpClient.Execute;
begin
  try
    FHttpClient.HTTPMethod(FHttpMethod, FUrl, FResponseData, []);
    Synchronize(@RequestComplete);
  except
    on E: Exception do
    begin
      FException := E;
      Synchronize(@OnClientException);
    end;
  end;
end;

constructor TThreadHttpClient.Create(CreateSuspened: Boolean);
begin
  inherited Create(CreateSuspened);
  FreeOnTerminate := True;
  FHttpClient := TFPHTTPClient.Create(nil);
  FHttpClient.OnHeaders := @InternalOnHeaders;
  FResponseData := TStringStream.Create('');
  FOnClientException:=nil;
  FOnHeaders:=nil;
  FOnRequestComplete:=nil;
end;

destructor TThreadHttpClient.Destroy;
begin
  FHttpClient.RequestBody.Free;
  FreeAndNil(FHttpClient);
  FreeAndNil(FResponseData);
  inherited Destroy;
end;

procedure TThreadHttpClient.AddHeader(const AHeader, AValue: String);
begin
  FHttpClient.AddHeader(AHeader, AValue);
end;

end.

