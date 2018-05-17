unit request_object;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids;

type

  { TRequestParamItem }

  TRequestParamItem = class(TCollectionItem)
  private
    FEnabled: boolean;
    FName: string;
    FValue: string;
  protected
  public
  published
    property Enabled: boolean read FEnabled write FEnabled;
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
  end;

  { TRequestObject }

  TRequestObject = class(TPersistent)
  private
    FMethod: string;
    FUrl: string;
    FBody: string;
    FJson: string;
    FHeaders: TCollection;
    FForm: TCollection;
    FCookies: TCollection;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetCollectionFromGrid(Grid: TStringGrid; coll: TCollection);
    procedure SetCollectionToGrid(coll: TCollection; Grid: TStringGrid);
  published
    property Method: string read FMethod write FMethod;
    property Url: string read FUrl write FUrl;
    property Body: string read FBody write FBody;
    property Json: string read FJson write FJson;
    property Headers: TCollection read FHeaders;
    property Form: TCollection read FForm;
    property Cookies: TCollection read FCookies;
  end;

implementation

uses strutils;

{ TRequestParamItem }

{ TRequestObject }

procedure TRequestObject.SetCollectionFromGrid(Grid: TStringGrid;
  coll: TCollection);
var
  I: Integer;
  name: string;
  item: TRequestParamItem;
begin
  for I := 1 to grid.RowCount - 1 do begin
    name := trim(grid.Cells[1, I]);
    if name = '' then continue;
    item := TRequestParamItem(coll.Add);
    item.Enabled := grid.cells[0, i] = '1';
    item.Name := grid.cells[1, i];
    item.Value := grid.cells[2, i];
  end;
end;

constructor TRequestObject.Create;
begin
  inherited Create;
  FHeaders := TCollection.Create(TRequestParamItem);
  FForm := TCollection.Create(TRequestParamItem);
  FCookies := TCollection.Create(TRequestParamItem);
end;

destructor TRequestObject.Destroy;
begin
  FHeaders.Free;
  FForm.Free;
  FCookies.Free;
  inherited Destroy;
end;

procedure TRequestObject.SetCollectionToGrid(coll: TCollection;
  Grid: TStringGrid);
var
  item: TRequestParamItem;
  I: Integer;
begin
  Grid.RowCount := coll.Count + 1;
  for I := 0 to coll.Count - 1 do begin
    item := TRequestParamItem(coll.Items[I]);
    Grid.Cells[0, I + 1] := IfThen(item.Enabled, '1', '0');
    Grid.Cells[1, I + 1] := item.Name;
    Grid.Cells[2, I + 1] := item.Value;
  end;
end;

end.

