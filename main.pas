unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Dialogs, StdCtrls,
  ComCtrls, ValEdit, ExtCtrls, Grids, Menus,
  fphttpclient, fpjson, Controls, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnSubmit: TButton;
    Button2: TButton;
    cbMethod: TComboBox;
    cbUrl: TComboBox;
    GroupBox1: TGroupBox;
    boxResponse: TGroupBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    responseRaw: TMemo;
    PostText: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Splitter1: TSplitter;
    responseHeaders: TStringGrid;
    requestHeaders: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    JsonTree: TTreeView;
    procedure btnSubmitClick(Sender: TObject);
    procedure cbUrlKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure requestHeadersBeforeSelection(Sender: TObject; aCol, aRow: Integer
      );
    procedure requestHeadersPickListSelect(Sender: TObject);
  private
    FContentType: string;
    FJsonRoot: TJSONData;
    procedure HttpClientOnHeaders(Sender: TObject);
    procedure DoneResponse(httpClient: TFPHTTPClient; S: TStream);
    procedure ParseContentType(Headers: TStrings);
    procedure JsonDocument(json: string);
    procedure ShowJsonDocument;
    procedure ShowJsonData(AParent: TTreeNode; Data: TJSONData);
    function ParseHeaderLine(line: string): TKeyValuePair;
  public

  end;

var
  Form1: TForm1;

implementation

uses sysutils, jsonparser, about, headers_editor;

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnSubmitClick(Sender: TObject);
var
  url, method, key: string;
  httpClient: TFPHTTPClient;
  SS, Body: TStringStream;
  i: integer;
begin
  url := Trim(cbUrl.Text);
  if url = '' then
  begin
    cbUrl.SetFocus;
    exit;
  end;
  method := UpperCase(Trim(cbMethod.Text));
  if method = '' then method := 'GET';
  if Pos('http', url) = 0 then url := 'http://' + url;
  FContentType := '';
  httpClient := TFPHTTPClient.Create(nil);
  httpClient.OnHeaders := @HttpClientOnHeaders;
  if (method = 'POST') or (method = 'PUT') then
    httpClient.RequestBody := TStringStream.Create(PostText.Text);
  try
    btnSubmit.Enabled := False;
    for i:=1 to requestHeaders.RowCount-1 do
    begin
      key := trim(requestHeaders.Cells[0, i]);
      if key = '' then continue;
      httpClient.AddHeader(key, trim(requestHeaders.Cells[1, i]));
    end;
    SS := TStringStream.Create('');
    httpClient.HTTPMethod(method, url, SS, []);
    responseRaw.Clear;
    responseRaw.Append(SS.DataString);
    responseRaw.CaretPos := Point(0, 0);
    DoneResponse(httpClient, SS);
    if cbUrl.Items.IndexOf(url) = -1 then cbUrl.Items.Add(url);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
  httpClient.RequestBody.Free;
  FreeAndNil(httpClient);
  FreeAndNil(SS);
  btnSubmit.Enabled := True;
end;

procedure TForm1.cbUrlKeyPress(Sender: TObject; var Key: char);
begin
  if key = #13 then btnSubmitClick(Sender);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := ApplicationName;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //showmessage(inttostr(key));
  // F9 submit request
  if (key = 120) and (shift = []) then btnSubmitClick(Sender);
  if Shift = [ssCtrl] then
  begin
    case Key of
      // Control-L focus to url field.
      76: cbUrl.SetFocus;
      // Control-P focus to methods field.
      80: cbMethod.SetFocus;
    end;
  end;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TForm1.requestHeadersBeforeSelection(Sender: TObject; aCol,
  aRow: Integer);
var
  header: string;
begin
  header := Trim(requestHeaders.Cells[0, aRow]);
  if header <> '' then
  begin
    HeadersEditorForm.FillHeaderValues(header, requestHeaders.Columns.Items[1].PickList);
  end;
end;

procedure TForm1.requestHeadersPickListSelect(Sender: TObject);
begin
end;

procedure TForm1.HttpClientOnHeaders(Sender: TObject);
var
  client: TFPHTTPClient;
  i, p: integer;
  h: string;
begin
  client := TFPHTTPClient(Sender);
  responseHeaders.RowCount := client.ResponseHeaders.Count + 1;
  for i:=0 to client.ResponseHeaders.Count-1 do
  begin
    h := client.ResponseHeaders.Strings[i];
    p := Pos(':', h);
    responseHeaders.Cells[0, i + 1] := LeftStr(h, p - 1);
    responseHeaders.Cells[1, i + 1] := trim(RightStr(h, Length(h) - p));
  end;
  ParseContentType(client.ResponseHeaders);
end;

procedure TForm1.DoneResponse(httpClient: TFPHTTPClient; S: TStream);
begin
  boxResponse.Caption := Format('Response: HTTP/%s %d %s', [
    httpClient.ServerHTTPVersion,
    httpClient.ResponseStatusCode,
    httpClient.ResponseStatusText
  ]);
  if FContentType = 'application/json' then JsonDocument(responseRaw.Text);
end;

procedure TForm1.ParseContentType(Headers: TStrings);
var
  i: integer;
  kv: TKeyValuePair;
begin
  for i := 0 to headers.count - 1 do
  begin
    kv := ParseHeaderLine(Headers.Strings[i]);
    if LowerCase(kv.Key) = 'content-type' then
    begin
      FContentType := LowerCase(kv.Value);
      Exit; // =>
    end;
  end;
end;

procedure TForm1.JsonDocument(json: string);
var
  D: TJSONData;
  P: TJSONParser;
  S: TStringStream;
begin
  S := TStringStream.Create(json);
  P := TJSONParser.Create(S);
  D := P.Parse;
  FJsonRoot := D;
  ShowJsonDocument;
  FreeAndNil(P);
  FreeAndNil(S);
  FreeAndNil(D);
  FJsonRoot := nil;
end;

procedure TForm1.ShowJsonDocument;
begin
  with JsonTree.Items do begin
    BeginUpdate;
    try
      JsonTree.Items.Clear;
      ShowJsonData(Nil, FJsonRoot);
      with JsonTree do
        if (Items.Count > 0) and Assigned(Items[0]) then
        begin
          Items[0].Expand(False);
          Selected := Items[0];
        end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TForm1.ShowJsonData(AParent: TTreeNode; Data: TJSONData);
var
  N,N2: TTreeNode;
  I: Integer;
  D: TJSONData;
  C: String;
  S: TStringList;
begin
  if Not Assigned(Data) then
    exit;

  N := JsonTree.Items.AddChild(AParent,'');

  Case Data.JSONType of
    jtArray,
    jtObject:
      begin
      If (Data.JSONType = jtArray) then
        C := 'Array (%d elements)'
      else
        C := 'Object (%d members)';
      C := Format(C, [Data.Count]);
      S := TstringList.Create;
      try
        For I:=0 to Data.Count-1 do
          If Data.JSONtype=jtArray then
            S.AddObject(IntToStr(I), Data.items[i])
          else
            S.AddObject(TJSONObject(Data).Names[i], Data.items[i]);
        For I:=0 to S.Count-1 do
          begin
          N2 := JsonTree.Items.AddChild(N, S[i]);
          D := TJSONData(S.Objects[i]);
          //N2.ImageIndex := ImageTypeMap[D.JSONType];
          //N2.SelectedIndex := ImageTypeMap[D.JSONType];
          ShowJSONData(N2,D);
          end
      finally
        S.Free;
      end;
      end;
    jtNull:
      C := 'null';
  else
    C := Data.AsString;
    if (Data.JSONType = jtString) then
      C := '"'+C+'"';
  end;
  If Assigned(N) then
    begin
    If N.Text = '' then
      N.Text := C
    else
      N.Text := N.Text + ': ' + C;
    //N.ImageIndex := ImageTypeMap[Data.JSONType];
    //N.SelectedIndex := ImageTypeMap[Data.JSONType];
    N.Data := Data;
    end;
end;

function TForm1.ParseHeaderLine(line: string): TKeyValuePair;
var
  p: integer;
begin
  p := Pos(':', line);
  if p = 0 then
  begin
    Result.Key := line;
    Result.Value := '';
    Exit; // =>
  end;
  Result.Key := LeftStr(line, p - 1);
  Result.Value := Trim(RightStr(line, Length(line) - p));
  p := Pos(';', Result.Value);
  if p <> 0 then Result.Value := Trim(LeftStr(Result.Value, p - 1));
end;

end.

