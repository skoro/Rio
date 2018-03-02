unit request_object;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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
    FHeaders: TCollection;
    FForm: TCollection;
    FCookies: TCollection;
  protected

  public
    constructor Create;
    destructor Destroy; override;
  published
    property Method: string read FMethod write FMethod;
    property Url: string read FUrl write FUrl;
    property Body: string read FBody write FBody;
    property Headers: TCollection read FHeaders;
    property Form: TCollection read FForm;
    property Cookies: TCollection read FCookies;
  end;

implementation

{ TRequestParamItem }

{ TRequestObject }

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

end.

