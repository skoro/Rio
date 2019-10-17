unit import;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, request_object;

type

  { TRequestObjectItem }

  TRequestObjectItem = class(TCollectionItem)
  private
    FRequestObject: TRequestObject;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property RequestObject: TRequestObject read FRequestObject;
  end;

  { TRequestObjectList }

  TRequestObjectList = class(TCollection)
  private
    function GetItems(Index: integer): TRequestObjectItem;
    procedure SetItems(Index: integer; AValue: TRequestObjectItem);
  public
    constructor Create;
    function Add: TRequestObjectItem;
    property Items[Index: integer]: TRequestObjectItem read GetItems write SetItems; default;
  end;

  { TImport }

  TImport = class
  private
    FRequestObjectList: TRequestObjectList;
    FInput: string;
  protected
    function GetRequestObjects: TRequestObjectList; virtual;
    procedure SetInput(AValue: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Input: string read FInput write SetInput;
    property RequestObjects: TRequestObjectList read GetRequestObjects;
  end;

  { TImportException }

  TImportException = class(Exception)
  end;

  { TCurlImport }

  TCurlImport = class(TImport)
  private
  protected
    procedure SetInput(AValue: string); override;
    procedure ParseCommandLine;
  public
  end;

implementation

uses strutils, ValEdit, AppHelpers;

{ TRequestObjectItem }

constructor TRequestObjectItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FRequestObject := TRequestObject.Create;
end;

destructor TRequestObjectItem.Destroy;
begin
  FRequestObject.Free;
  inherited Destroy;
end;

{ TRequestObjectList }

function TRequestObjectList.GetItems(Index: integer): TRequestObjectItem;
begin
  Result := TRequestObjectItem(inherited Items[Index]);
end;

procedure TRequestObjectList.SetItems(Index: integer; AValue: TRequestObjectItem
  );
begin
  Items[Index].Assign(AValue);
end;

constructor TRequestObjectList.Create;
begin
  inherited Create(TRequestObjectItem);
end;

function TRequestObjectList.Add: TRequestObjectItem;
begin
  Result := inherited Add as TRequestObjectItem;
end;

{ TCurlImport }

procedure TCurlImport.SetInput(AValue: string);
begin
  inherited;
  ParseCommandLine;
end;

procedure TCurlImport.ParseCommandLine;
var
  Buf, tokens, data: TStringList;
  line: string;
  RO: TRequestObject;
  KV: TKeyValuePair;
  Auth: TAuthTab;

  // Get a next token from the tokens list.
  function NextTok: string;
  begin
    if Tokens.Count = 0 then
      raise TImportException.Create('Command line error.');
    Result := Tokens.Strings[0];
    Tokens.Delete(0);
  end;

begin
  Buf := TStringList.Create; // Temporary buffer.
  tokens := TStringList.Create; // Tokens list.
  data := TStringList.Create; // Form data.
  Auth := atNone;
  try
    Tokenize(Input, Tokens);
    if Tokens.Strings[0] <> 'curl' then
      raise TImportException.Create('Not a curl command line.');
    Tokens.Delete(0);
    RO := RequestObjects.Add.RequestObject;
    while Tokens.Count > 0 do begin
      line := NextTok;
      case line of
        '\': Continue;
        '-X', '--request': RO.Method := NextTok;
        '-H', '--header': begin
          KV := SplitKV(NextTok, ':');
          if LowerCase(KV.Key) = 'cookie' then begin // Cookie header.
            KV := SplitKV(KV.Value, '=');
            RO.AddCookie(KV.Key, KV.Value)
          end
          else
            RO.AddHeader(KV.Key, KV.Value);
        end;
        '-F', '--form': begin
          KV := SplitKV(NextTok, '=');
          if KV.Value[1] = '@' then
            RO.AddForm(KV.Key, MidStr(KV.Value, 2, Length(KV.Value) - 1), True, ftiFile)
          else
            RO.AddForm(KV.Key, KV.Value);
        end;
        '-d', '--data': begin
          data.Add(NextTok);
        end;
        '-b', '--cookie': begin
          // The data should be in the format "NAME1=VALUE1; NAME2=VALUE2".
          SplitStrings(NextTok, ';', buf);
          for line in Tokens do begin
            KV := SplitKV(Trim(line), '=');
            RO.AddCookie(KV.Key, KV.Value);
          end;
        end;
        '--compressed': begin
          // Just ignore this option.
        end;
        '--basic': auth := atBasic;
        '--user': begin
          if Auth = atNone then
            Auth := atBasic;
          // This is pointless because no other auth methods are parsed.
          if Auth <> atBasic then
            raise TImportException.Create('Only Basic auth is supported.');
          KV := SplitKV(NextTok, ':');
          RO.AuthType := Auth;
          RO.AuthBasic.Login := KV.Key;
          RO.AuthBasic.Password := KV.Value;
        end
        else begin
          if line[1] = '-' then
            raise TImportException.Create(Format('Option "%s" is not supported.', [line]));
          RO.Url := line;
        end;
      end; // while
    end;
    if data.Count > 0 then begin
      // the data always send by POST method.
      if UpperCase(RO.Method) = 'GET' then
        RO.Method := 'POST';
      if RO.IsJson then RO.Json := data.Text else RO.Body := data.Text;
    end;
  finally
    Buf.Free;
    tokens.Free;
    data.Free;
  end;
end;

{ TImport }

procedure TImport.SetInput(AValue: string);
begin
  if (FInput <> '') and (FInput = AValue) then Exit;
  AValue := Trim(AValue);
  if Length(AValue) = 0 then
    raise TImportException.Create('Input is empty.');
  FInput := AValue;
end;

function TImport.GetRequestObjects: TRequestObjectList;
begin
  Result := FRequestObjectList;
end;

constructor TImport.Create;
begin
  inherited;
  FRequestObjectList := TRequestObjectList.Create;
end;

destructor TImport.Destroy;
begin
  FRequestObjectList.Free;
  inherited Destroy;
end;

end.

