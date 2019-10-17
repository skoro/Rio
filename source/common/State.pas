// Global application state.

unit State;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { TAppState }

  TAppState = class
  private
    type
    TStateValue = record
      ValString: string;
      ValInt: integer;
      ValBool: boolean;
    end;
    TStateMap = specialize TFPGMap<string, TStateValue>;
  var
    FStateMap: TStateMap;
  public
    constructor Create;
    destructor Destroy; override;
    function ReadString(State: string; DefaultValue: string = ''): string;
    function ReadInteger(State: string; DefaultValue: integer = 0): integer;
    function ReadBoolean(State: string; DefaultValue: boolean = False): boolean;
    procedure WriteString(State: string; Value: string);
    procedure WriteInteger(State: string; Value: integer);
    procedure WriteBoolean(State: string; Value: boolean);
  end;

var
  AppState: TAppState = nil;

implementation

{ TAppState }

constructor TAppState.Create;
begin
  inherited;
  FStateMap := TStateMap.Create;
end;

destructor TAppState.Destroy;
begin
  FStateMap.Free;
  inherited Destroy;
end;

function TAppState.ReadString(State: string; DefaultValue: string = ''): string;
begin
  try
    Result := FStateMap[State].ValString;
  except
    on EListError do
      Result := DefaultValue;
  end;
end;

function TAppState.ReadInteger(State: string; DefaultValue: integer = 0): integer;
begin
  try
    Result := FStateMap[State].ValInt;
  except
    on EListError do
      Result := DefaultValue;
  end;
end;

function TAppState.ReadBoolean(State: string; DefaultValue: boolean): boolean;
begin
  try
    Result := FStateMap[State].ValBool;
  except
    on EListError do
      Result := DefaultValue;
  end;
end;

procedure TAppState.WriteString(State: string; Value: string);
var
  SV: TStateValue;
begin
  SV.ValString := Value;
  FStateMap[State] := SV;
end;

procedure TAppState.WriteInteger(State: string; Value: integer);
var
  SV: TStateValue;
begin
  SV.ValInt := Value;
  FStateMap[State] := SV;
end;

procedure TAppState.WriteBoolean(State: string; Value: boolean);
var
  SV: TStateValue;
begin
  SV.ValBool := Value;
  FStateMap[State] := SV;
end;

initialization
  AppState := TAppState.Create;

finalization
  FreeAndNil(AppState);

end.
