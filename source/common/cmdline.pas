unit cmdline;

{$mode objfpc}{$H+}

interface

function Usage: string;
procedure HandleCommandLine;

implementation

uses sysutils, strutils, Forms, ValEdit, AppHelpers, main, RequestObject;

const CRLF = #13#10;

function Usage: string;
begin
  Result := Format(
    crlf +
    '%s [options] [URL]' + crlf +
    '' + crlf +
    '-h,--help' + crlf +
    '    This help.' + crlf +
    '-X,--request=<command>' + crlf +
    '    Custom request method: PUT, DELETE, etc.' + crlf +
    '-H,--header=<name:value>' + crlf +
    '    Extra header to include in the request.' + crlf +
    '    You may specify any number of extra headers.' + crlf +
    '-F,--form=<name=value>' + crlf +
    '    Post a form data using the Content-Type application/x-www-form-urlencoded.' + crlf +
    '    This implies that it will be a POST request but can be overrided' + crlf +
    '    by -X option. When a value prefixed by @ then it treated as file upload.' + crlf +
    '-C,--cookie=<name=value>' + crlf +
    '    Include a cookie to the request.' + crlf +
    '-d,--data=<data>' + crlf +
    '    Send the specified data in a POST request (may be overrided by -X option).' + crlf +
    '    Compare to the -F option you will need to specify Content-Type header.' + crlf +
    '    Data from this option will appear in "Other" view of "Body" tab.' + crlf +
    '-j,--json=<json>' + crlf +
    '    Like -d but appends Content-Type: application/json header.' + crlf +
    '    Data from this option will appear in "Json" view of "Body" tab.' + crlf +
    '-f,--file=<file>' + crlf +
    '    Open request data from the specified file.' + crlf +
    '-b,--bookmark=<name>' + crlf +
    '    Load a bookmark. <name> is the bookmark name separated by "/".' + crlf +
    '    For example, a value /Service/users loads the bookmark "users"' + crlf +
    '    from the folder "Service".' + crlf +
    '--new' + crlf +
    '    Start a new request. Empty all the request fields and grids.' + crlf +
    crlf,
    [ExtractFileName(Application.ExeName)]
  );
end;

procedure HandleCommandLine;
const
  LongOpts: array [1..9] of string = ('request:', 'header:', 'form:',
            'cookie:', 'data:', 'json:', 'file:', 'new', 'bookmark:'
  );
  ShortOpts: string = 'X:H:F:C:d:j:f:b:';
var
  ErrorMsg, ReqMethod, Value, FileName: string;
  I: Integer;
  Values: TStringArray;
  KV: TKeyValuePair;
begin
  if Application.HasOption('h', 'help') then
    raise Exception.Create(Usage);
  ErrorMsg := Application.CheckOptions(ShortOpts, LongOpts);
  if ErrorMsg <> '' then
    raise Exception.Create(ErrorMsg);
  // Starts a new request.
  if Application.HasOption('new') then begin
    MainForm.StartNewRequest;
    Exit;
  end;
  // A request method.
  ReqMethod := '';
  if Application.HasOption('X', 'request') then
    ReqMethod := Application.GetOptionValue('X', 'request');
  // Request headers.
  if Application.HasOption('H', 'header') then begin
    Values := Application.GetOptionValues('H', 'header');
    for I := Length(Values) - 1 downto 0 do begin
      KV := SplitKV(Values[I], ':');
      MainForm.AddRequestHeader(KV.Key, KV.Value);
    end;
  end;
  // Form data.
  if Application.HasOption('F', 'form') then begin
    Values := Application.GetOptionValues('F', 'form');
    if ReqMethod = '' then
      ReqMethod := 'POST';
    for I := Length(Values) - 1 downto 0 do begin
      KV := SplitKV(Values[I], '=');
      if LeftStr(KV.Value, 1) = '@' then
        MainForm.AddFormData(KV.Key, RightStr(KV.Value, Length(KV.Value) - 1), True)
      else
        MainForm.AddFormData(KV.Key, KV.Value);
    end;
    MainForm.SelectBodyTab(btForm);
  end;
  // Cookies.
  if Application.HasOption('C', 'cookie') then begin
    Values := Application.GetOptionValues('C', 'cookie');
    for I := Length(Values) - 1 downto 0 do
      SetRowKV(MainForm.gridReqCookie, SplitKV(Values[I], '='));
  end;
  // Body data.
  if Application.HasOption('d', 'data') then
    with MainForm do begin
      editOther.Text := Application.GetOptionValue('d', 'data');
      SelectBodyTab(btOther);
    end;
  // Json data.
  if Application.HasOption('j', 'json') then
    with MainForm do begin
      if not SetJsonBody(Application.GetOptionValue('j', 'json'), ErrorMsg) then
        raise Exception.Create(ErrorMsg);
      SelectBodyTab(btJson);
    end;
  // Open request file.
  if Application.HasOption('f', 'file') then begin
    FileName := Application.GetOptionValue('f', 'file');
    if not FileGetContents(FileName, Value) then
      raise Exception.Create('cannot read file: ' + FileName);
    MainForm.OpenRequestFile(Value);
  end;
  // Load a bookmark.
  if Application.HasOption('b', 'bookmark') then begin
    Value := Trim(Application.GetOptionValue('b', 'bookmark'));
    Value := MainForm.BookmarkManager.RootName + '/' + TrimLeftSet(Value, ['/']);
    if MainForm.BookmarkManager.OpenBookmarkPath(Value) = NIL then
      raise Exception.CreateFmt('Could not open bookmark: %s', [Value]);
  end;
  // Set request method.
  if ReqMethod <> '' then
    MainForm.cbMethod.Text := UpperCase(ReqMethod);
  // Non options values are for url but we need only one url so use
  // the first value.
  Values := Application.GetNonOptions(ShortOpts, LongOpts);
  if Length(Values) > 0 then
    with MainForm do begin
      cbUrl.Text := Values[0];
      SubmitRequest;
    end;
end;

end.

