unit import;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, request_object;

type

  TRequestObjectItem = class(TCollectionItem)

  end;

  { TRequestObjectList }

  TRequestObjectList = class(TCollection)
  private
    function GetItems(Index: integer): TRequestObjectItem;
    procedure SetItems(Index: integer; AValue: TRequestObjectItem);

  public
    constructor Create;
    function Add: TRequestObjectItem;
    property Items[Index: integer]: TRequestObjectItem read GetItems write SetItems;
  end;

  { TImport }

  TImport = class
  private
  protected
    FRequestObjectList: TRequestObjectList;
    function GetRequestObjects: TRequestObjectList; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property RequestObjects: TRequestObjectList read GetRequestObjects;
  end;

  { TCurlImport }

  TCurlImport = class(TImport)
  private
    FCommandLine: string;
    procedure SetCommandLine(AValue: string);
  public
    property CommandLine: string read FCommandLine write SetCommandLine;
  end;

implementation

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

procedure TCurlImport.SetCommandLine(AValue: string);
begin
  if FCommandLine = AValue then Exit;
  FCommandLine := AValue;
end;

{ TImport }

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

