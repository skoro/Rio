unit request_object;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, main;

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

  { TRequestParamList }

  TRequestParamList = class(TCollection)
  private
    function GetItems(Index: integer): TRequestParamItem;
    procedure SetItems(Index: integer; AValue: TRequestParamItem);
  public
    constructor Create;
    function Add: TRequestParamItem;
    property Items[Index: integer]: TRequestParamItem read GetItems write SetItems; default;
  end;

  { TFormTypeItem }

  TFormTypeItem = (ftiText, ftiFile);

  { TFormParamItem }

  TFormParamItem = class(TRequestParamItem)
  private
    FElemType: TFormTypeItem;
  published
    property ElemType: TFormTypeItem read FElemType write FElemType;
  end;

  { TFormParamList }

  TFormParamList = class(TCollection)
  private
    function GetItems(Index: integer): TFormParamItem;
    procedure SetItems(Index: integer; AValue: TFormParamItem);
  public
    constructor Create;
    function Add: TFormParamItem;
    property Items[Index: integer]: TFormParamItem read GetItems write SetItems;
  end;

  { TAuthBasic }

  TAuthBasic = class
  private
    FLogin: string;
    FPassword: string;
  published
    property Login: string read FLogin write FLogin;
    property Password: string read FPassword write FPassword;
  end;

  { TAuthBearer }

  TAuthBearer = class
  private
    FPrefix: string;
    FToken: string;
  public
    constructor Create;
  published
    property Prefix: string read FPrefix write FPrefix;
    property Token: string read FToken write FToken;
  end;

  { TRequestObject }

  TRequestObject = class(TPersistent)
  private
    FMethod: string;
    FUrl: string;
    FBody: string;
    FJson: string;
    FHeaders: TRequestParamList;
    FCookies: TRequestParamList;
    FParams: TRequestParamList; // GET params
    FForm: TFormParamList;
    FAuthBasic: TAuthBasic;
    FAuthBearer: TAuthBearer;
    FAuthType: TAuthTab; // TODO: should be TAuthType
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetCollectionFromGrid(Grid: TStringGrid; coll: TCollection);
    procedure SetCollectionToGrid(coll: TCollection; Grid: TStringGrid);
    procedure SetFormToGrid(FormGrid: TStringGrid);
    procedure GetFormFromGrid(FormGrid: TStringGrid);
    procedure AddHeader(AName, AValue: string; IsEnabled: Boolean = True);
    procedure AddCookie(AName, AValue: string; IsEnabled: Boolean = True);
    procedure AddForm(AName, AValue: string; IsEnabled: Boolean = True;
      AElemType: TFormTypeItem = ftiText);
    procedure LoadToForm(form: TForm1);
    function IsJson: Boolean;
  published
    property Method: string read FMethod write FMethod;
    property Url: string read FUrl write FUrl;
    property Body: string read FBody write FBody;
    property Json: string read FJson write FJson;
    property Headers: TRequestParamList read FHeaders;
    property Form: TFormParamList read FForm;
    property Cookies: TRequestParamList read FCookies;
    property Params: TRequestParamList read FParams;
    property AuthType: TAuthTab read FAuthType write FAuthType;
    property AuthBasic: TAuthBasic read FAuthBasic;
    property AuthBearer: TAuthBearer read FAuthBearer;
  end;

implementation

uses strutils;

{ TAuthBearer }

constructor TAuthBearer.Create;
begin
  FPrefix := 'Bearer';
end;

{ TFormParamList }

function TFormParamList.GetItems(Index: integer): TFormParamItem;
begin
  Result := TFormParamItem(inherited Items[Index]);
end;

procedure TFormParamList.SetItems(Index: integer; AValue: TFormParamItem);
begin
  Items[Index].Assign(AValue);
end;

constructor TFormParamList.Create;
begin
  inherited Create(TFormParamItem);
end;

function TFormParamList.Add: TFormParamItem;
begin
  Result := inherited Add as TFormParamItem;
end;

{ TRequestParamList }

function TRequestParamList.GetItems(Index: integer): TRequestParamItem;
begin
  Result := TRequestParamItem(inherited Items[Index]);
end;

procedure TRequestParamList.SetItems(Index: integer; AValue: TRequestParamItem);
begin
  Items[Index].Assign(AValue);
end;

constructor TRequestParamList.Create;
begin
  inherited Create(TRequestParamItem);
end;

function TRequestParamList.Add: TRequestParamItem;
begin
  Result := inherited Add as TRequestParamItem;
end;

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
  FHeaders    := TRequestParamList.Create;
  FCookies    := TRequestParamList.Create;
  FParams     := TRequestParamList.Create;
  FForm       := TFormParamList.Create;
  FAuthType   := atNone;
  FAuthBasic  := TAuthBasic.Create;
  FAuthBearer := TAuthBearer.Create;
  FMethod     := 'GET';
end;

destructor TRequestObject.Destroy;
begin
  FHeaders.Free;
  FForm.Free;
  FCookies.Free;
  FParams.Free;
  FAuthBasic.Free;
  FAuthBearer.Free;
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

procedure TRequestObject.GetFormFromGrid(FormGrid: TStringGrid);
var
  Item: TFormParamItem;
  I: Integer;
  Name: String;
begin
  for I := 1 to FormGrid.RowCount - 1 do begin
    Name := Trim(FormGrid.Cells[1, I]);
    if Name = '' then
      Continue;
    Item := TFormParamItem(FForm.Add);
    Item.Enabled  := FormGrid.Cells[0, I] = '1';
    Item.Name     := FormGrid.Cells[1, I];
    Item.Value    := FormGrid.Cells[2, I];
    if FormGrid.Cells[3, I] = 'File' then
      Item.ElemType := ftiFile
    else
      Item.ElemType := ftiText;
  end;
end;

procedure TRequestObject.SetFormToGrid(FormGrid: TStringGrid);
var
  Item: TFormParamItem;
  I: Integer;
begin
  FormGrid.RowCount := FForm.Count + 1;
  for I := 0 to FForm.Count - 1 do begin
    Item := TFormParamItem(FForm.Items[I]);
    FormGrid.Cells[0, I + 1] := IfThen(Item.Enabled, '1', '0');
    FormGrid.Cells[1, I + 1] := Item.Name;
    FormGrid.Cells[2, I + 1] := Item.Value;
    case Item.ElemType of
      ftiText: FormGrid.Cells[3, I + 1] := '';
      ftiFile: FormGrid.Cells[3, I + 1] := 'File';
    end;
  end;
end;

procedure TRequestObject.AddHeader(AName, AValue: string; IsEnabled: Boolean);
begin
  with Headers.Add do begin
    Enabled := IsEnabled;
    Name := AName;
    Value := AValue;
  end;
end;

procedure TRequestObject.AddCookie(AName, AValue: string; IsEnabled: Boolean);
begin
  with Cookies.Add do begin
    Enabled := IsEnabled;
    Name := AName;
    Value := AValue;
  end;
end;

procedure TRequestObject.AddForm(AName, AValue: string; IsEnabled: Boolean;
  AElemType: TFormTypeItem);
begin
  with Form.Add do begin
    Enabled := IsEnabled;
    Name := AName;
    Value := AValue;
    ElemType := AElemType;
  end;
end;

procedure TRequestObject.LoadToForm(form: TForm1);
begin
  with form do begin
    cbUrl.Text     := Url;
    cbMethod.Text  := Method;
    editOther.Text := Body;
    editJson.Text  := Json;

    SetCollectionToGrid(Headers, requestHeaders);
    SetCollectionToGrid(Cookies, gridReqCookie);
    SetCollectionToGrid(Params, gridParams);
    SetFormToGrid(gridForm);

    SelectAuthTab(AuthType);
    editBasicLogin.Text    := AuthBasic.Login;
    editBasicPassword.Text := AuthBasic.Password;
    editBearerPrefix.Text  := AuthBearer.Prefix;
    editBearerToken.Text   := AuthBearer.Token;
  end;
end;

function TRequestObject.IsJson: Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to FHeaders.Count - 1 do
    if (LowerCase(FHeaders[i].Name) = 'content-type')
       and (AnsiStartsText('application/json', FHeaders[i].Value)) then
      Exit(True);
end;

end.

