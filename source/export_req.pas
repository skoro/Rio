unit export_req;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, request_object;

type

  { TExport }

  TExport = class
  private
    FOutput: string;
    FRequestObject: TRequestObject;
    procedure SetRequestObject(AValue: TRequestObject);
  protected
    procedure ExportRequestObject; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property RequestObject: TRequestObject read FRequestObject write SetRequestObject;
    property Output: string read FOutput;
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
    function GetOptionsStr: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    property CurlName: string read FCurlName write FCurlName;
    property Options: TStrings read FOptions;
  end;
implementation

uses main, strutils;

{ TCurlExport }

procedure TCurlExport.ExportRequestObject;
var
  Param: TRequestParamItem;
  FormParam: TFormParamItem;
  cookies, formValue: string;
  isGet: Boolean;
begin
  FOptions.Clear;
  isGet := FRequestObject.Method = 'GET';

  // Request method.
  if not isGet then
    AddOption('-X', UpperCase(FRequestObject.Method));

  // Create curl header options.
  for Param in FRequestObject.Headers do
    if Param.Enabled then begin
      // Don't add content-type for GET requests.
      if (isGet) and (LowerCase(Param.Name) = 'content-type') then
        continue
      else
        AddOption('-H', Format('%s: %s', [Param.Name, Param.Value]));
    end;
  if (not isGet) and (not FRequestObject.IsJson) and (FRequestObject.DataType = btJson) then
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
      btJson: begin
        // Compact json data by removing line breaks.
        formValue := AnsiReplaceStr(FRequestObject.Json, #13, '');
        formValue := AnsiReplaceStr(FRequestObject.Json, #10, '');
        AddOption('--data', formValue);
      end;
      btOther: AddOption('--data', FRequestObject.Body);
    end;

  // Auth options.
  case FRequestObject.AuthType of
    atBasic: begin
      AddOption('--basic');
      AddOption('--user', Format('%s:%s', [FRequestObject.AuthBasic.Login, FRequestObject.AuthBasic.Password]));
    end;
    atBearer: with FRequestObject.AuthBearer do begin
      AddOption('-H', 'Authorization ' + Format('%s %s', [Prefix, Token]));
    end;
  end;

  // Format the command line.
  FOutput := Format('%s %s %s', [
          FCurlName, GetOptionsStr, QuotedStr(FRequestObject.Url)
  ]);
end;

procedure TCurlExport.AddOption(Option: string; Value: string = '');
begin
  if Value = '' then
    FOptions.Add(Option)
  else
    FOptions.AddStrings([Option, QuotedStr(Value)]);
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

end.

