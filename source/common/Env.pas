unit Env;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { TVariable }

  TVariable = class
  private
    FName: string;
    FValue: string;
  public
    constructor Create(const VarName: string); virtual;
    constructor Create(const VarName, VarValue: string); overload;
    function Replace(const Txt: string): string; virtual;
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
  end;

  { TEnvironment }

  TEnvironment = class
  private
    type
    TVarList = specialize TFPGObjectList<TVariable>;
  private
    FParent: TEnvironment;
    FName: string;
    FVarList: TVarList;
  protected
    procedure SetName(const AValue: string);
    procedure SetParent(const AValue: TEnvironment);
    function GetVariable(VarName: string): TVariable;
    function GetVarNames: TStringArray;
    function FindVar(const VarName: string): TVariable;
  public
    constructor Create(const AName: string; const AParent: TEnvironment); virtual;
    destructor Destroy; override;
    procedure DeleteVar(const VarName: string);
    procedure Add(const Variable: TVariable);
    function Apply(const Txt: string): string;
    property Parent: TEnvironment read FParent write SetParent;
    property Name: string read FName write SetName;
    property VarNames: TStringArray read GetVarNames;
    property Variable[VarName: string]: TVariable read GetVariable; default;
  end;

  { EVariableBase }

  EVariableBase = class(Exception);

  { EVariableNotFound }

  EVariableNotFound = class(EVariableBase)
  public
    constructor Create(const VarName: string);
  end;

  { EEnvironmentBase }

  EEnvironmentBase = class(Exception);

  { EEnvironmentNotFound }

  EEnvironmentNotFound = class(EEnvironmentBase)
  public
    constructor Create(const EnvName: string);
  end;

  { EEnvironmentExists }

  EEnvironmentExists = class(EEnvironmentBase)
  public
    constructor Create(const EnvName: string);
  end;

  { TEnvList }

  TEnvList = specialize TFPGObjectList<TEnvironment>;

  { TEnvManager }

  TEnvManager = class
  private
    FEnvList: TEnvList;
    function GetCount: Integer;
    function GetEnv(EnvName: string): TEnvironment;
    function GetEnvIndex(Index: integer): TEnvironment;
    function GetEnvNames: TStringArray;
    function GetFirst: TEnvironment;
  protected
    function FindEnv(const EnvName: string): TEnvironment;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(const Env: TEnvironment);
    procedure Delete(const EnvName: string);
    procedure Rename(const Env:TEnvironment; const NewName: string);
    function FindAvailParents(const Env: TEnvironment): TEnvList;
    property EnvNames: TStringArray read GetEnvNames;
    property Env[EnvName: string]: TEnvironment read GetEnv; default;
    property EnvIndex[Index: integer]: TEnvironment read GetEnvIndex;
    property Count: Integer read GetCount;
    property First: TEnvironment read GetFirst;
  end;

implementation

uses strutils;

{ EEnvironmentExists }

constructor EEnvironmentExists.Create(const EnvName: string);
begin
  inherited CreateFmt('"%s" already exists.', [EnvName]);
end;

{ EEnvironmentNotFound }

constructor EEnvironmentNotFound.Create(const EnvName: string);
begin
  inherited CreateFmt('"%s" not found.', [EnvName]);
end;

{ TVariable }

constructor TVariable.Create(const VarName: string);
begin
  inherited Create;
  FName := VarName;
end;

constructor TVariable.Create(const VarName, VarValue: string);
begin
  inherited Create;
  FName := VarName;
  FValue := VarValue;
end;

function TVariable.Replace(const Txt: string): string;
var
  start: SizeInt;
  LName, LTxt: string;
begin
  Result := Txt;
  LTxt := LowerCase(Txt);
  LName := LowerCase(FName);
  repeat
    start := PosEx('{{' + LName + '}}', LTxt);
    if start = 0 then
      Exit; // =>
    Result := ReplaceText(Txt, '{{' + FName + '}}', FValue);
  until True;
end;

{ TEnvManager }

function TEnvManager.GetEnv(EnvName: string): TEnvironment;
begin
  Result := FindEnv(EnvName);
  if not Assigned(Result) then
    raise EEnvironmentNotFound.Create(EnvName);
end;

function TEnvManager.GetEnvIndex(Index: integer): TEnvironment;
begin
  Result := FEnvList.Items[Index];
end;

function TEnvManager.GetCount: Integer;
begin
  Result := FEnvList.Count;
end;

function TEnvManager.GetEnvNames: TStringArray;
var
  i: integer;
begin
  SetLength(Result, FEnvList.Count);
  for i := 0 to FEnvList.Count - 1 do
    Result[i] := FEnvList[i].Name;
end;

function TEnvManager.GetFirst: TEnvironment;
begin
  Result := FEnvList.First;
end;

function TEnvManager.FindEnv(const EnvName: string): TEnvironment;
begin
  for Result in FEnvList do
    if Result.Name = EnvName then
      Exit;
  Result := nil;
end;

constructor TEnvManager.Create;
begin
  inherited;
  FEnvList := TEnvList.Create(True);
end;

destructor TEnvManager.Destroy;
begin
  FreeAndNil(FEnvList);
  inherited Destroy;
end;

procedure TEnvManager.Add(const Env: TEnvironment);
begin
  if FindEnv(Env.Name) <> nil then
    raise EEnvironmentExists.Create(Env.Name);
  FEnvList.Add(Env);
end;

procedure TEnvManager.Delete(const EnvName: string);
var
  i: integer;
  e, x: TEnvironment;
begin
  E := nil;
  // Get the index of the deleted env.
  for i := 0 to FEnvList.Count - 1 do
    if FEnvList[i].Name = EnvName then
    begin
      E := FEnvList[i];
      break;
    end;
  // Not found.
  if E = nil then
    raise EEnvironmentNotFound.Create(EnvName);
  // Detach the env from the parents.
  for x in FEnvList do
    if x.Parent = E then
      x.Parent := nil;
  // Delete.
  FEnvList.Delete(i);
end;

procedure TEnvManager.Rename(const Env: TEnvironment; const NewName: string);
begin
  if FindEnv(NewName) <> nil then
    raise EEnvironmentExists.Create(NewName);
  if FindEnv(Env.Name) = nil then
    raise EEnvironmentNotFound.Create(Env.Name);
  Env.Name := NewName;
end;

function TEnvManager.FindAvailParents(const Env: TEnvironment): TEnvList;
var
  Iter: TEnvironment;
  function IsMyParent(Check, Base: TEnvironment): Boolean;
  begin
    if Check.Parent = nil then
      Exit(False); // =>
    if Check.Parent = Base then
      Exit(True); // =>
    Result := IsMyParent(Check.Parent, Base);
  end;

begin
  Result := TEnvList.Create(False);
  // nil - passed - all available envs can be parents.
  if Env = nil then
  begin
    Result.Assign(FEnvList);
    Exit; // =>
  end;
  // Add only environment which is not parent (and via parents, etc...) for Env.
  for Iter in FEnvList do
    if (Env <> Iter) and (not IsMyParent(Iter, Env)) then
      Result.Add(Iter);
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

function TEnvironment.GetVariable(VarName: string): TVariable;
begin
  Result := FindVar(VarName);
  if not Assigned(Result) then
    if not Assigned(FParent) then
      raise EVariableNotFound.Create(VarName)
    else
      Result := FParent.Variable[VarName];
end;

procedure TEnvironment.SetParent(const AValue: TEnvironment);
begin
  if FParent = AValue then
    Exit; // =>
  if AValue = Self then
    raise Exception.Create('Cannot set self to parent.');
  FParent := AValue;
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
  i: integer;
  V: TVariable;
begin
  V := nil;
  for i := 0 to FVarList.Count - 1 do
    if FVarList[i].Name = VarName then
    begin
      V := FVarList[i];
      break;
    end;
  if V = nil then
    raise EVariableNotFound.Create(VarName);
  FVarList.Delete(i);
end;

procedure TEnvironment.Add(const Variable: TVariable);
begin
  if FindVar(Variable.Name) <> nil then
    raise Exception.CreateFmt('Variable "%s" is already exist.', [Variable.Name]);
  FVarList.Add(Variable);
end;

function TEnvironment.Apply(const Txt: string): string;
var
  start, stop: SizeInt;
  VarName: string;
  V: TVariable;
begin
  Result := Txt;
  start := 1;
  repeat
    start := PosEx('{{', Txt, start);
    if start = 0 then
      Exit; // =>
    Inc(start, 2);
    stop := PosEx('}}', Txt, start);
    if stop = 0 then
      Exit; // =>
    VarName := MidStr(Txt, start, stop - start);
    if VarName = '' then
      Exit; // =>
    V := GetVariable(VarName);
    Result := LeftStr(Txt, start - 2) + V.Value + RightStr(Txt, Length(Txt) - stop);
  until True;
end;

function TEnvironment.GetVarNames: TStringArray;
var
  i: integer;
begin
  SetLength(Result, FVarList.Count);
  for i := 0 to FVarList.Count - 1 do
    Result[i] := FVarList[i].Name;
end;

function TEnvironment.FindVar(const VarName: string): TVariable;
begin
  for Result in FVarList do
    if Result.Name = VarName then
      Exit; // =>
  Result := nil;
end;

end.
