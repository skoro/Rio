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
    constructor Create;
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
  protected
    procedure ExportRequestObject; override;
  public
  end;
implementation

{ TCurlExport }

procedure TCurlExport.ExportRequestObject;
begin
  inherited ExportRequestObject;
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

end;

destructor TExport.Destroy;
begin
  inherited Destroy;
end;

end.

