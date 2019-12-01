{
  TODO : No locking is implemented !
  TODO : add crc for cache data for avoid rewriting data on the file system.
}

unit Cache;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type

  { TCacheAbstract }

  TCacheAbstract = class
  private
  protected
    function GetKeyValue(const Key: string): string; virtual; abstract;
    procedure SetKeyValue(const Key, Value: string); virtual; abstract;
  public
    procedure Put(const Key, Value: string); virtual;
    procedure Put(const Key, Value: string; const Expired: TDateTime);
      virtual; abstract; overload;
    procedure Put(const Key, Value: string; const Minutes: int64); virtual; overload;
    function Delete(const Key: string): boolean; virtual; abstract;
    function Exists(const Key: string): boolean; virtual; abstract;
    property KeyValue[const Key: string]: string read GetKeyValue write SetKeyValue;
      default;
  end;

  { TFileMeta }

  TFileMeta = class
  private
    FKey: string;
    FHash: string;
    FBaseDir: string;
    FExpired: TDateTime;
    FDataSize: SizeInt;
    function GetDataFile: string;
    function GetHash: string;
    function GetMetaFile: string;
  protected
    function GenId(const Val: string): string; virtual;
  public
    constructor Create(const AKey, ABaseDir: string);
    procedure Save;
    procedure Load;
    function Delete: boolean;
    function Exists: boolean;
    function IsExpired: boolean;
    property Key: string read FKey;
    property Hash: string read GetHash;
    property BaseDir: string read FBaseDir write FBaseDir;
    property MetaFile: string read GetMetaFile;
    property DataFile: string read GetDataFile;
    property Expired: TDateTime read FExpired write FExpired;
    property DataSize: SizeInt read FDataSize write FDataSize;
  end;

  { ECacheKeyNotFound }

  ECacheKeyNotFound = class(Exception);

  { TFileCache }

  TFileCache = class(TCacheAbstract)
  private
    procedure SetCacheDir(AValue: string);
  protected
    FCacheDir: string;
    function GetKeyValue(const Key: string): string; override;
    procedure SetKeyValue(const Key, Value: string); override;
    function CreateMeta(const Key: string): TFileMeta; virtual;
    function LoadValue(const Key: string): string; virtual;
  public
    constructor Create(const ACacheDir: string);
    destructor Destroy; override;
    procedure Put(const Key, Value: string; const Expired: TDateTime);
      override; overload;
    function Delete(const Key: string): boolean; override;
    function Exists(const Key: string): boolean; override;
    property CacheDir: string read FCacheDir write SetCacheDir;
  end;

  { TNullCache }

  TNullCache = class(TCacheAbstract)
  protected
    function GetKeyValue(const Key: string): string; override;
    procedure SetKeyValue(const Key, Value: string); override;
  public
    procedure Put(const Key, Value: string; const Expired: TDateTime);
      override; overload;
    function Delete(const Key: string): boolean; override;
    function Exists(const Key: string): boolean; override;
  end;

// Application cache directory.
//
// Parameter 'Create' will create as the base directory and the additional
// 'aDir' directory. If one of these directories cannot be created then
// the exception EInOutError will be thrown.
function AppCacheDir(const aDir: string = ''; Create: boolean = false): string;

implementation

uses md5, IniFiles, dateutils;

function AppCacheDir(const aDir: string; Create: boolean = false): string;
begin
  { TODO : How about Windows ? }
  Result := GetEnvironmentVariable('XDG_CACHE_HOME');
  if Result = '' then
    Result := IncludeTrailingPathDelimiter(GetUserDir + '.cache')
  else
    Result := IncludeTrailingPathDelimiter(Result);
  Result := IncludeTrailingPathDelimiter(Result + ApplicationName);
  if Create and (not DirectoryExists(Result)) and (not CreateDir(Result)) then
    raise EInOutError.CreateFmt('Could not create cache directory: %s', [Result]);
  if aDir <> '' then
  begin
    Result := IncludeTrailingPathDelimiter(Result + aDir);
    if Create and (not DirectoryExists(Result)) and (not CreateDir(Result)) then
      raise EInOutError.CreateFmt('Could not create cache directory: %s', [Result]);
  end;
end;

{ TNullCache }

function TNullCache.GetKeyValue(const Key: string): string;
begin
  Result := '';
end;

procedure TNullCache.SetKeyValue(const Key, Value: string);
begin
  // It does nothing.
end;

procedure TNullCache.Put(const Key, Value: string; const Expired: TDateTime);
begin
  // It does nothing.
end;

function TNullCache.Delete(const Key: string): boolean;
begin
  Result := True;
end;

function TNullCache.Exists(const Key: string): boolean;
begin
  Result := False;
end;

{ TCacheAbstract }

procedure TCacheAbstract.Put(const Key, Value: string);
begin
  Put(Key, Value, TDateTime(0));
end;

procedure TCacheAbstract.Put(const Key, Value: string; const Minutes: int64);
begin
  Put(Key, Value, IncMinute(Now, Minutes));
end;

{ TFileMeta }

function TFileMeta.GetHash: string;
begin
  if FHash = '' then
    FHash := GenId(FKey);
  Result := FHash;
end;

function TFileMeta.GetDataFile: string;
begin
  Result := IncludeTrailingPathDelimiter(FBaseDir) + Hash + '.data';
end;

function TFileMeta.GetMetaFile: string;
begin
  Result := IncludeTrailingPathDelimiter(FBaseDir) + Hash + '.meta';
end;

function TFileMeta.GenId(const Val: string): string;
begin
  Result := MDPrint(MD5String(Val));
end;

constructor TFileMeta.Create(const AKey, ABaseDir: string);
begin
  FKey := AKey;
  FHash := '';
  FExpired := 0;
  FDataSize := 0;
  FBaseDir := ABaseDir;
end;

procedure TFileMeta.Save;
var
  Ini: TIniFile;
begin
  Ini := nil;
  try
    Ini := TIniFile.Create(MetaFile);
    Ini.WriteString(ClassName, 'Key', FKey);
    // Just for information.
    Ini.WriteString(ClassName, 'Created', DateTimeToStr(Now));
    // Expired time.
    if FExpired = 0 then
      Ini.WriteInteger(ClassName, 'Expired', 0)
    else
      Ini.WriteString(ClassName, 'Expired', DateTimeToStr(FExpired));
    Ini.WriteInt64(ClassName, 'DataSize', FDataSize);
  finally
    if Assigned(Ini) then
      Ini.Free;
  end;
end;

procedure TFileMeta.Load;
var
  Ini: TIniFile;
  Buf: string;
begin
  Ini := nil;
  try
    if not Exists then
      raise EFileNotFoundException.CreateFmt('Cache meta file not found: %s',
        [MetaFile]);
    Ini := TIniFile.Create(MetaFile);
    Buf := Ini.ReadString(ClassName, 'Key', '');
    Buf := GenId(Buf);
    if Buf <> FHash then
      raise Exception.CreateFmt('Cache meta file integrity failed: %s', [MetaFile]);
    FDataSize := Ini.ReadInt64(ClassName, 'DataSize', 0);
    Buf := Ini.ReadString(ClassName, 'Expired', '0');
    if Buf = '0' then
      FExpired := 0
    else
      FExpired := StrToDateTime(Buf);
  finally
    if Assigned(Ini) then
      Ini.Free;
  end;
end;

function TFileMeta.Delete: boolean;
begin
  Result := DeleteFile(MetaFile);
end;

function TFileMeta.Exists: boolean;
begin
  Result := FileExists(MetaFile);
end;

function TFileMeta.IsExpired: boolean;
begin
  Result := (FExpired <> 0) and (Now > FExpired);
end;

{ TFileCache }

procedure TFileCache.SetCacheDir(AValue: string);
begin
  if FCacheDir = AValue then
    Exit;
  if (not DirectoryExists(AValue)) and (not CreateDir(AValue)) then
    raise EInOutError.CreateFmt('Cannot create cache directory: %s', [AValue]);
  FCacheDir := AValue;
end;

function TFileCache.GetKeyValue(const Key: string): string;
begin
  Result := LoadValue(Key);
end;

procedure TFileCache.SetKeyValue(const Key, Value: string);
begin
  Put(Key, Value);
end;

function TFileCache.CreateMeta(const Key: string): TFileMeta;
begin
  Result := TFileMeta.Create(Key, FCacheDir);
end;

function TFileCache.LoadValue(const Key: string): string;
var
  Meta: TFileMeta;
  Buf: string;
  H, S: longint;
begin
  Meta := CreateMeta(Key);
  try
    Meta.Load;
    if Meta.IsExpired then
      raise ECacheKeyNotFound.CreateFmt('Cache key "%s" is not exists.', [Key]);
    if not FileExists(Meta.DataFile) then
      raise EFileNotFoundException.CreateFmt('Cache data file not found: %s',
        [Meta.DataFile]);
    H := FileOpen(Meta.DataFile, fmOpenRead);
    if H = -1 then
      raise EInOutError.CreateFmt('Cannot open cache data: %s', [Meta.DataFile]);
    SetLength(Buf, Meta.DataSize);
    S := FileRead(H, Buf[1], Meta.DataSize);
    if S <> Meta.DataSize then
      raise EInOutError.CreateFmt('Cannot read cache data: %s', [Meta.DataSize]);
    FileClose(H);
    Result := Buf;
  finally
    FreeAndNil(Meta);
  end;
end;

constructor TFileCache.Create(const ACacheDir: string);
begin
  inherited Create;
  CacheDir := ACacheDir;
end;

destructor TFileCache.Destroy;
begin
  inherited Destroy;
end;

procedure TFileCache.Put(const Key, Value: string; const Expired: TDateTime);
var
  Meta: TFileMeta;
  H, S: longint;
begin
  Meta := CreateMeta(Key);
  S := Length(Value);
  try
    Meta.Expired := Expired;
    Meta.DataSize := S;
    Meta.Save;
    H := FileCreate(Meta.DataFile);
    if H = -1 then
      raise EInOutError.CreateFmt('Cannot create cache data file: %s', [Meta.DataFile]);
    if FileWrite(H, Value[1], S) <> S then
    begin
      Meta.Delete;
      DeleteFile(Meta.DataFile);
      raise EInOutError.CreateFmt('Failed to write to cache data file: %s',
        [Meta.DataFile]);
    end;
    FileClose(H);
  finally
    FreeAndNil(Meta);
  end;
end;

function TFileCache.Delete(const Key: string): boolean;
var
  Meta: TFileMeta;
begin
  Result := False;
  Meta := CreateMeta(Key);
  try
    Meta.Delete;
    if FileExists(Meta.DataFile) then
      Result := DeleteFile(Meta.DataFile);
  finally
    FreeAndNil(Meta);
  end;
end;

function TFileCache.Exists(const Key: string): boolean;
var
  Meta: TFileMeta;
begin
  Result := False;
  Meta := CreateMeta(Key);
  try
    if Meta.IsExpired then
      Delete(Key)
    else
      Result := Meta.Exists and FileExists(Meta.DataFile);
  finally
    FreeAndNil(Meta);
  end;
end;

end.
