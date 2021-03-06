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

  { TVarList }

  TVarList = specialize TFPGObjectList<TVariable>;

  { TEnvironment }

  TEnvironment = class
  private
    FParent: TEnvironment;
    FName: string;
    FVarList: TVarList;
  protected
    procedure SetName(const AValue: string);
    procedure SetParent(const AValue: TEnvironment);
    function GetVariable(VarName: string): TVariable;
    function GetVars: TVarList;
    function GetOwnVars: TVarList;
    function GetVarIndex(Index: integer): TVariable;
    function GetVarsCount: integer;
  public
    constructor Create(const AName: string; const AParent: TEnvironment); virtual;
    destructor Destroy; override;
    procedure DeleteVar(const VarName: string);
    procedure DeleteAllVars;
    function FindVar(const VarName: string): TVariable;
    function Add(const Variable: TVariable): TVariable;
    function Add(const VarName, VarVal: string): TVariable; overload;
    function Apply(const Txt: string): string;
    property Parent: TEnvironment read FParent write SetParent;
    property Name: string read FName write SetName;
    property Variable[VarName: string]: TVariable read GetVariable; default;
    property VarIndex[Index: integer]: TVariable read GetVarIndex;
    property Vars: TVarList read GetVars;
    property OwnVars: TVarList read GetOwnVars;
    property Count: integer read GetVarsCount;
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
    FCurrent: TEnvironment;
    FExtList: TStrings; // Env names external list pointer.
    function GetCount: Integer;
    function GetEnv(EnvName: string): TEnvironment;
    function GetEnvIndex(Index: integer): TEnvironment;
    function GetEnvNames: TStringArray;
    function GetFirst: TEnvironment;
    procedure SetCurrent(AValue: TEnvironment);
    procedure SetExtList(AValue: TStrings);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(const Env: TEnvironment);
    procedure Delete(const EnvName: string);
    procedure Rename(const Env:TEnvironment; const NewName: string);
    // Finds the env name in the external list.
    function FindExt(const EnvName: string; out Index: integer): Boolean;
    // Finds the available parents for the specified env.
    function FindAvailParents(const Env: TEnvironment): TEnvList;
    // Finds env by name.
    function FindEnv(const EnvName: string): TEnvironment;
    procedure SaveToStream(const AStream: TStream);
    procedure SaveToFile(const FileName: string);
    // Loads the environments data from the stream.
    procedure LoadFromStream(const AStream: TStream);
    // Loads the environments data from the file.
    // False - when file isn't existing.
    function LoadFromFile(const FileName: string): Boolean;
    // Apply the current environment to the text.
    function Apply(const Txt: string): string;
    // Check whether the string has vars.
    function HasVars(const Txt: string): Boolean;
    // Gets the array list of the env names.
    property EnvNames: TStringArray read GetEnvNames;
    // Gets the env by name.
    property Env[EnvName: string]: TEnvironment read GetEnv; default;
    // Gets the env by index (from 0).
    property EnvIndex[Index: integer]: TEnvironment read GetEnvIndex;
    // The total count of environments.
    property Count: Integer read GetCount;
    // The first environment.
    property First: TEnvironment read GetFirst;
    // Selected environment (in the owner).
    property Current: TEnvironment read FCurrent write SetCurrent;
    // Synchronize the env names with an external string list.
    property ExtList: TStrings read FExtList write SetExtList;
  end;

implementation

uses strutils, XMLRead, DOM, XMLWrite;

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

procedure TEnvManager.SetCurrent(AValue: TEnvironment);
begin
  if FCurrent = AValue then
    Exit; // =>
  if FindEnv(AValue.Name) = NIL then
    raise EEnvironmentNotFound.Create(AValue.Name);
  FCurrent := AValue;
end;

procedure TEnvManager.SetExtList(AValue: TStrings);
begin
  if FExtList = AValue then
    Exit; // =>
  FExtList := AValue;
  if Assigned(FExtList) then
    FExtList.AddStrings(EnvNames, True);
end;

function TEnvManager.FindExt(const EnvName: string; out Index: integer): Boolean;
var
  i: integer;
begin
  Result := false;
  Index := -1;
  for i := 0 to FExtList.Count - 1 do
    if AnsiCompareText(FExtList[i], EnvName) = 0 then
    begin
      Index := i;
      Exit(True);
    end;
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
  FCurrent := nil;
end;

destructor TEnvManager.Destroy;
begin
  FreeAndNil(FEnvList);
  inherited Destroy;
end;

procedure TEnvManager.Add(const Env: TEnvironment);
begin
  // Env must belong to our container.
  if FindEnv(Env.Name) <> nil then
    raise EEnvironmentExists.Create(Env.Name);
  FEnvList.Add(Env);
  // Sync the external list.
  if Assigned(FExtList) then
    FExtList.Add(Env.Name);
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
  // If deleted env is current - detach it.
  if FCurrent = E then
    FCurrent := nil;
  // Delete.
  FEnvList.Delete(i);
  // Sync the external list.
  if Assigned(FExtList) and FindExt(EnvName, i) then
    FExtList.Delete(i);
end;

procedure TEnvManager.Rename(const Env: TEnvironment; const NewName: string);
var
  i: integer;
begin
  if FindEnv(NewName) <> nil then
    raise EEnvironmentExists.Create(NewName);
  if FindEnv(Env.Name) = nil then
    raise EEnvironmentNotFound.Create(Env.Name);
  // Sync the external list.
  if Assigned(FExtList) and FindExt(Env.Name, i) then
    FExtList[i] := NewName;
  // Rename.
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

procedure TEnvManager.SaveToStream(const AStream: TStream);
var
  Doc: TXMLDocument;
  XmlRoot, EnvElem, VarElem: TDOMNode;
  EnvIter: TEnvironment;
  EnvOwnVars: TVarList;
  VarIter: TVariable;
begin
  try
    Doc := TXMLDocument.Create;
    XmlRoot := Doc.CreateElement('Environments');
    Doc.AppendChild(XmlRoot);
    for EnvIter in FEnvList do
    begin
      EnvElem := Doc.CreateElement('Env');
      TDOMElement(EnvElem).SetAttribute('name', EnvIter.Name);
      if FCurrent = EnvIter then
        TDOMElement(EnvElem).SetAttribute('current', '1');
      if Assigned(EnvIter.Parent) then
        TDOMElement(EnvElem).SetAttribute('parent', EnvIter.Parent.Name);
      EnvOwnVars := EnvIter.OwnVars;
      for VarIter in EnvOwnVars do
      begin
        VarElem := Doc.CreateElement('Var');
        TDOMElement(VarElem).SetAttribute('name', VarIter.Name);
        TDOMElement(VarElem).SetAttribute('value', VarIter.Value);
        EnvElem.AppendChild(VarElem);
      end;
      FreeAndNil(EnvOwnVars);
      XmlRoot.AppendChild(EnvElem);
    end;
    WriteXML(Doc, AStream);
  finally
    Doc.Free;
  end;
end;

procedure TEnvManager.SaveToFile(const FileName: string);
var
  FS: TFileStream;
begin
  if FEnvList.Count = 0 then
    Exit; // =>
  FS := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FS);
  finally
    FreeAndNil(FS);
  end;
end;

procedure TEnvManager.LoadFromStream(const AStream: TStream);
var
  Doc: TXMLDocument;
  RootNode, EnvNode, VarNode: TDOMNode;
  EnvName, ParentName: string;
  EnvObj: TEnvironment;
begin
  try
    ReadXMLFile(Doc, AStream);
    RootNode := Doc.FindNode('Environments');
    if not Assigned(RootNode) then
      raise Exception.Create('Cannot read env xml data from stream.');
    EnvNode := RootNode.FirstChild;
    while EnvNode <> NIL do
    begin
      if EnvNode.NodeName <> 'Env' then
        Continue; { TODO : May be log needed ? }
      EnvName := TDOMElement(EnvNode).AttribStrings['name'];
      if EnvName = '' then
        Continue;
      EnvObj := TEnvironment.Create(EnvName, NIL);
      // Read variables for the environment.
      VarNode := EnvNode.FirstChild;
      while VarNode <> NIL do
      begin
        if VarNode.NodeName <> 'Var' then
          Continue; { TODO : May be log needed ? }
        with TDOMElement(VarNode) do
          EnvObj.Add(AttribStrings['name'], AttribStrings['value']);
        VarNode := VarNode.NextSibling;
      end;
      Add(EnvObj);
      if TDOMElement(EnvNode).AttribStrings['current'] = '1' then
        Current := EnvObj;
      EnvNode := EnvNode.NextSibling;
    end;
    // Rescan tree again and set parents to the environments.
    EnvNode := RootNode.FirstChild;
    while EnvNode <> NIL do
    begin
      EnvName := TDOMElement(EnvNode).AttribStrings['name'];
      EnvObj := FindEnv(EnvName);
      if EnvObj = Nil then
        Continue;
      ParentName := TDOMElement(EnvNode).AttribStrings['parent'];
      EnvObj.Parent := FindEnv(ParentName);
      EnvNode := EnvNode.NextSibling;
    end;
  finally
    FreeAndNil(Doc);
  end;
end;

function TEnvManager.LoadFromFile(const FileName: string): Boolean;
var
  FS: TFileStream;
begin
  Result := False;
  if not FileExists(FileName) then
    Exit; // =>
  FS := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(FS);
    Result := True;
  finally
    FreeAndNil(FS);
  end;
end;

function TEnvManager.Apply(const Txt: string): string;
begin
  if not Assigned(FCurrent) then
    Exit(Txt); // Nothing to apply.
  Result := FCurrent.Apply(Txt);
end;

function TEnvManager.HasVars(const Txt: string): Boolean;
var
  p1, p2: integer;
begin
  p1 := PosEx('{{', Txt);
  if p1 = 0 then
    Exit(False); // =>
  p2 := PosEx('}}', Txt, p1 + 2);
  Result := p2 > p1;
end;

{ EVariableNotFound }

constructor EVariableNotFound.Create(const VarName: string);
begin
  inherited CreateFmt('Variable "%s" not found.', [VarName]);
end;

{ TEnvironment }

function TEnvironment.GetVars: TVarList;
var
  V: TVariable;
  ParVL: TVarList;
  EnvParent: TEnvironment;
begin
  Result := GetOwnVars;
  EnvParent := FParent;
  while Assigned(EnvParent) do
  begin
    ParVL := EnvParent.Vars;
    for V in ParVL do
      if FindVar(V.Name) = nil then
        Result.Add(V);
    FreeAndNil(ParVL);
    EnvParent := EnvParent.Parent;
  end;
end;

function TEnvironment.GetOwnVars: TVarList;
var
  V: TVariable;
begin
  Result := TVarList.Create(False); // Don't let free objects outside.
  for V in FVarList do
    Result.Add(V);
end;

function TEnvironment.GetVarIndex(Index: integer): TVariable;
begin
  Result := FVarList[Index];
end;

function TEnvironment.GetVarsCount: integer;
begin
  Result := FVarList.Count;
  if Assigned(FParent) then
    Result := Result + FParent.Count;
end;

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
  FVarList := TVarList.Create(True);
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

procedure TEnvironment.DeleteAllVars;
begin
  FVarList.Clear;
end;

function TEnvironment.Add(const Variable: TVariable): TVariable;
var
  ParentEnv: TEnvironment;
  PV: TVariable;
begin
  if FindVar(Variable.Name) <> nil then
    raise Exception.CreateFmt('Variable "%s" is already exist.', [Variable.Name]);
  // Search if the variable exists in the parent.
  ParentEnv := FParent;
  while Assigned(ParentEnv) do
  begin
    PV := ParentEnv.FindVar(Variable.Name);
    if Assigned(PV) then
    begin
      // The variable exists and the value equals. No need to add a new one.
      if PV.Value = Variable.Value then
        Exit(PV); // =>
      break; // The value differs, override the parent variable.
    end;
    ParentEnv := ParentEnv.Parent;
  end;
  FVarList.Add(Variable);
  Result := Variable;
end;

function TEnvironment.Add(const VarName, VarVal: string): TVariable;
begin
  Result := Add(TVariable.Create(VarName, VarVal));
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
    start := PosEx('{{', Result, start);
    if start = 0 then
      Exit; // =>
    Inc(start, 2);
    stop := PosEx('}}', Result, start);
    if stop = 0 then
      Exit; // =>
    VarName := MidStr(Result, start, stop - start);
    if VarName = '' then
      Exit; // =>
    try
      V := GetVariable(VarName);
      Result := LeftStr(Result, start - 3) + V.Value + RightStr(Result, Length(Result) - stop - 1);
    except
      // Nothing to do. Just skip.
    end;
  until False;
end;

function TEnvironment.FindVar(const VarName: string): TVariable;
begin
  for Result in FVarList do
    if Result.Name = VarName then
      Exit; // =>
  Result := nil;
end;

end.
