unit Env;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { TEnvironment }

  TEnvironment = class
  private
    type
      TVarList = specialize TFPGMap<string, string>;
  private
    FParent: TEnvironment;
    FName: string;
    FVarList: TVarList;
  protected
    procedure SetName(const AValue: string);
    procedure SetParent(const AValue: TEnvironment);
    procedure SetValue(const VarName, AValue: string);
    function GetValue(VarName: string): string;
    function GetVarNames: TStringArray;
  public
    constructor Create(const AName: string; const AParent: TEnvironment); virtual;
    destructor Destroy; override;
    procedure DeleteVar(const VarName: string);
    property Parent: TEnvironment read FParent write SetParent;
    property Name: string read FName write SetName;
    property VarNames: TStringArray read GetVarNames;
    property Value[VarName: string]: string read GetValue write SetValue; default;
  end;

  { EVariableNotFound }

  EVariableNotFound = class(Exception)
  public
    constructor Create(const VarName: string);
  end;

  { TEnvManager }

  TEnvManager = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TEnvManager }

constructor TEnvManager.Create;
begin
  inherited;
end;

destructor TEnvManager.Destroy;
begin
  inherited Destroy;
end;

{ EVariableNotFound }

constructor EVariableNotFound.Create(const VarName: string);
begin
  inherited CreateFmt('Variable "%s" not found.', [VarName]);
end;

{ TEnvironment }

procedure TEnvironment.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit; // =>
  if Length(Trim(AValue)) = 0 then
    raise Exception.Create('Name cannot be an empty string.');
  FName := AValue;
end;

function TEnvironment.GetValue(VarName: string): string;
var
  KeyIdx: integer;
begin
  if not FVarList.Find(VarName, KeyIdx) then
    if Assigned(FParent) then
      Exit(FParent[VarName])
    else
      raise EVariableNotFound.Create(VarName);
  Result := FVarList.Data[KeyIdx];
end;

procedure TEnvironment.SetParent(const AValue: TEnvironment);
begin
  if FParent = AValue then
    Exit; // =>
  if AValue = Self then
    raise Exception.Create('Cannot set self to parent.');
  FParent := AValue;
end;

procedure TEnvironment.SetValue(const VarName, AValue: string);
var
  KeyIdx: integer;
begin
  if not FVarList.Find(VarName, KeyIdx) then
    FVarList.Add(VarName, AValue)
  else
    FVarList.Data[KeyIdx] := AValue;
end;

constructor TEnvironment.Create(const AName: string; const AParent: TEnvironment);
begin
  Name := AName;
  FParent := AParent;
  FVarList := TVarList.Create;
end;

destructor TEnvironment.Destroy;
begin
  FreeAndNil(FVarList);
  inherited Destroy;
end;

procedure TEnvironment.DeleteVar(const VarName: string);
var
  KeyIdx: integer;
begin
  if not FVarList.Find(VarName, KeyIdx) then
    raise EVariableNotFound.Create(VarName);
  FVarList.Delete(KeyIdx);
end;

function TEnvironment.GetVarNames: TStringArray;
var
  i: integer;
begin
  SetLength(Result, FVarList.Count);
  for i := 0 to FVarList.Count - 1 do
    Result[i] := FVarList.Data[i];
end;

end.
