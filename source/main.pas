unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Dialogs, StdCtrls,
  ComCtrls, ValEdit, ExtCtrls, Grids, Menus,
  fphttpclient, fpjson, Controls, JSONPropStorage, thread_http_client,
  SysUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnSubmit: TButton;
    cbMethod: TComboBox;
    cbUrl: TComboBox;
    gridForm: TStringGrid;
    gaInsertRow: TMenuItem;
    gaEditRow: TMenuItem;
    gaSaveHeader: TMenuItem;
    miOptions: TMenuItem;
    StatusText3: TLabel;
    respImg: TImage;
    miOpenRequest: TMenuItem;
    miSaveRequest: TMenuItem;
    miSaveResponse: TMenuItem;
    miNew: TMenuItem;
    dlgOpen: TOpenDialog;
    Panel1: TPanel;
    pagesRequest: TPageControl;
    PostText: TMemo;
    requestHeaders: TStringGrid;
    dlgSave: TSaveDialog;
    scrollImage: TScrollBox;
    Splitter1: TSplitter;
    StatusImage1: TImage;
    jsImages: TImageList;
    JsonTree: TTreeView;
    StatusText1: TLabel;
    StatusText2: TLabel;
    miEdit: TMenuItem;
    gaClearRows: TMenuItem;
    miInsertHeader: TMenuItem;
    gaDeleteRow: TMenuItem;
    pagesResponse: TPageControl;
    StatusPanel: TPanel;
    Panel2: TPanel;
    popupGridActions: TPopupMenu;
    PSMAIN: TJSONPropStorage;
    AppMenu: TMainMenu;
    miFile: TMenuItem;
    miHelp: TMenuItem;
    miView: TMenuItem;
    miTreeExpand: TMenuItem;
    miQuit: TMenuItem;
    miAbout: TMenuItem;
    MenuItem6: TMenuItem;
    responseHeaders: TStringGrid;
    responseRaw: TMemo;
    gridRespCookie: TStringGrid;
    gridReqCookie: TStringGrid;
    tabContent: TTabSheet;
    tabForm: TTabSheet;
    tabJson: TTabSheet;
    tabResponse: TTabSheet;
    tabHeaders: TTabSheet;
    tabBody: TTabSheet;
    tabRespCookie: TTabSheet;
    tabReqCookie: TTabSheet;
    tabImage: TTabSheet;
    procedure btnSubmitClick(Sender: TObject);
    procedure cbUrlKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure gaClearRowsClick(Sender: TObject);
    procedure gaEditRowClick(Sender: TObject);
    procedure gaInsertRowClick(Sender: TObject);
    procedure gaSaveHeaderClick(Sender: TObject);
    procedure gridColRowInserted(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure gridEditDblClick(Sender: TObject);
    procedure gridRespCookieDblClick(Sender: TObject);
    procedure JsonTreeClick(Sender: TObject);
    procedure miInsertHeaderClick(Sender: TObject);
    procedure miNewClick(Sender: TObject);
    procedure miOpenRequestClick(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure miQuitClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure gaDeleteRowClick(Sender: TObject);
    procedure miSaveRequestClick(Sender: TObject);
    procedure miSaveResponseClick(Sender: TObject);
    procedure miTreeExpandClick(Sender: TObject);
    procedure popupGridActionsPopup(Sender: TObject);
    procedure PSMAINRestoringProperties(Sender: TObject);
    procedure PSMAINSavingProperties(Sender: TObject);
    procedure requestHeadersBeforeSelection(Sender: TObject; aCol, aRow: Integer
      );
    procedure respImgDblClick(Sender: TObject);
  private
    FContentType: string;
    FJsonRoot: TJSONData;
    FHttpClient: TThreadHttpClient;
    procedure OnHttpException(Url, Method: string; E: Exception);
    procedure ParseContentType(Headers: TStrings);
    procedure JsonDocument(json: string);
    procedure ShowJsonDocument;
    procedure ShowJsonData(AParent: TTreeNode; Data: TJSONData);
    function ParseHeaderLine(line: string; delim: char = ':'; all: Boolean = False): TKeyValuePair;
    procedure UpdateHeadersPickList;
    function EncodeFormData: string;
    procedure OnRequestComplete(Info: TResponseInfo);
    procedure UpdateStatusLine(Text1: string = ''; Text2: string = '');
    procedure ShowResponseCookie(Headers: TStrings);
    function GetRequestFilename(ext: string = ''): string;
    function PromptNewRequest(const prompt: string; const promptTitle: string = 'New request'): Boolean;
    procedure StartNewRequest;
    function GetPopupSenderAsStringGrid(Sender: TObject): TStringGrid;
    procedure EditGridRow(Grid: TStringGrid);
    function NormalizeUrl: string;
    procedure SetAppCaption(const AValue: String = '');
    procedure ShowHideResponseTabs(Info: TResponseInfo);
    procedure ImageResize(ToStretch: Boolean = True);
    function GetContentSubtype: string;
  public

  end;

var
  Form1: TForm1;

implementation

uses lcltype, jsonparser, about, headers_editor, cookie_form, uriparser,
  request_object, app_helpers, fpjsonrtti, key_value, strutils, options;

const
  ImageTypeMap: array[TJSONtype] of Integer =
  // (jtUnknown, jtNumber, jtString, jtBoolean, jtNull, jtArray, jtObject)
  (-1, 3, 2, 4, 5, 0, 1);

  JSONTypeNames: array[TJSONtype] of string =
  ('Unknown', 'Number', 'String', 'Boolean', 'Null', 'Array', 'Object');

  MAX_URLS = 15; // How much urls we can store in url dropdown history.

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnSubmitClick(Sender: TObject);
var
  url, method, key, value, formData: string;
  i: integer;
  isForm: boolean; // is it form submit request
  ctForm: boolean; // append form content type to headers grid
begin
  try
    url := NormalizeUrl;
  except on E: Exception do
    begin
      cbUrl.SetFocus;
      Exit;
    end;
  end;

  FContentType := '';
  isForm := False;
  ctForm := False;

  FHttpClient := TThreadHttpClient.Create(true);
  FHttpClient.OnRequestComplete := @OnRequestComplete;
  FHttpClient.OnException := @OnHttpException;

  method := UpperCase(Trim(cbMethod.Text));
  if method = '' then method := 'GET';

  // Do submit form ?
  formData := EncodeFormData;
  if (method = 'POST') and (Length(formData) > 0) then begin
    isForm := True;
    FHttpClient.RequestBody := TStringStream.Create(formData);
  end;

  if (not isForm) and (Length(Trim(PostText.Text)) > 0) then begin
    FHttpClient.RequestBody := TStringStream.Create(PostText.Text);
  end;

  btnSubmit.Enabled := False;
  miTreeExpand.Enabled := False;

  // Assign request headers to the client.
  for i:=1 to requestHeaders.RowCount-1 do
  begin
    if requestHeaders.Cells[0, i] = '0' then continue; // Skip disabled headers.
    key := trim(requestHeaders.Cells[1, i]);
    if key = '' then continue;
    value := trim(requestHeaders.Cells[2, i]);
    if isForm and (LowerCase(key) = 'content-type') then
      // Forms must be with appropriate content type.
      if LowerCase(value) = 'application/x-www-form-urlencoded' then ctForm := True
      else begin
        value := 'application/x-www-form-urlencoded';
        requestHeaders.Cells[2, i] := value;
        ctForm := True
      end;
    FHttpClient.AddHeader(key, value);
  end;
  // It's a form submit request but there is no form content type in the
  // headers grid. Append one to the grid.
  if isForm and not ctForm then
  begin
    key := 'Content-Type';
    value := 'application/x-www-form-urlencoded';
    FHttpClient.AddHeader(key, value);
    requestHeaders.InsertRowWithValues(requestHeaders.RowCount, ['1', key, value]);
  end;

  // Set request cookies
  for I := 1 to gridReqCookie.RowCount - 1 do
  begin
    if gridReqCookie.Cells[0, I] = '0' then continue;
    key := Trim(gridReqCookie.Cells[1, I]);
    if key = '' then continue;
    value := gridReqCookie.Cells[2, I];
    FHttpClient.AddCookie(key, value);
  end;

  UpdateStatusLine('Waiting for the response...');

  FHttpClient.Url := url;
  FHttpClient.Method := method;
  FHttpClient.Start;
end;

procedure TForm1.cbUrlKeyPress(Sender: TObject; var Key: char);
begin
  if key = #13 then btnSubmitClick(Sender);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  C: string;
begin
  inherited;
  SetAppCaption;

  // Init app configuration.
  C := GetAppConfigFile(False, True);
  PSMAIN.JSONFileName := C;
  C := ExtractFilePath(C);
  if not ForceDirectories(C) then ShowMessage(Format('Cannot create directory "%s"', [C]));
  PSMAIN.Active := True;

  // Form components defaults.
  StatusText2.Caption := '';
  StatusText3.Caption := '';
  miSaveResponse.Enabled := False;

  HeadersEditorForm := THeadersEditorForm.Create(Application);

  UpdateHeadersPickList;

  gridForm.Cells[0, 1] := '1';
  gridReqCookie.Cells[0, 1] := '1';

  // Init cookie form.
  CookieForm := TCookieForm.Create(Application);
  CookieForm.ResponseGrid := gridRespCookie;
  CookieForm.RequestGrid := gridReqCookie;

  KeyValueForm := TKeyValueForm.Create(Application);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Если не освободить, то когда активна вкладка JSON при выходе
  // из приложения возникают исключения что память не освобождена.
  jsImages.Free;

  if Assigned(FHttpClient) then FHttpClient.Terminate;
  inherited;
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

procedure TForm1.gaClearRowsClick(Sender: TObject);
var
  Answer: Integer;
  Grid: TStringGrid;
begin
  Answer := Application.MessageBox('Are you sure to clear content ?', 'Clear rows', MB_ICONQUESTION + MB_YESNO);
  if Answer = IDNO then Exit; // =>
  Grid := GetPopupSenderAsStringGrid(Sender);
  if Grid <> nil then
    with Grid do begin
      RowCount := 2;
      Cells[0, 1] := '1';
      Cells[1, 1] := '';
      Cells[2, 1] := '';
    end;
end;

procedure TForm1.gaEditRowClick(Sender: TObject);
var
  Grid: TStringGrid;
begin
  Grid := GetPopupSenderAsStringGrid(Sender);
  if Grid <> nil then EditGridRow(Grid);
end;

procedure TForm1.gaInsertRowClick(Sender: TObject);
var
  Grid: TStringGrid;
begin
  Grid := GetPopupSenderAsStringGrid(Sender);
  if Grid = nil then Exit;
  if Grid = requestHeaders then
    miInsertHeaderClick(Grid)
  else
    if (Grid = gridForm) or (Grid = gridReqCookie) then
      Grid.InsertRowWithValues(Grid.RowCount, ['1', '', '']);
end;

procedure TForm1.gaSaveHeaderClick(Sender: TObject);
var
  Grid: TStringGrid;
  Header: String;
begin
  Grid := GetPopupSenderAsStringGrid(Sender);
  if Grid = requestHeaders then begin
    Header := Trim(Grid.Cells[1, Grid.Row]);
    if Header <> '' then
      HeadersEditorForm.Add(Header, Grid.Cells[2, Grid.Row]);
  end;
end;

procedure TForm1.gridColRowInserted(Sender: TObject; IsColumn: Boolean; sIndex,
  tIndex: Integer);
begin
  // New inserted columns with "On" checked by default.
  (Sender as TStringGrid).Cells[0, sIndex] := '1';
end;

procedure TForm1.gridEditDblClick(Sender: TObject);
var
  grid: TStringGrid;
begin
  if Sender is TStringGrid then begin
    grid := TStringGrid(Sender);
    if grid = responseHeaders then
    begin
      if grid.RowCount > 1 then with grid do
        KeyValueForm.View(Cells[0, Row], Cells[1, Row], 'View: ' + Cells[0, Row])
    end
    else
      EditGridRow(grid);
  end;
end;

procedure TForm1.gridRespCookieDblClick(Sender: TObject);
begin
  CookieForm.View;
end;

procedure TForm1.JsonTreeClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TTreeView(Sender).Selected;
  if Assigned(Node) then miTreeExpand.Enabled := True else miTreeExpand.Enabled := False;
end;

procedure TForm1.miInsertHeaderClick(Sender: TObject);
var
  i, row: integer;
  gridHeaders: TStringGrid;
begin
  gridHeaders := HeadersEditorForm.gridHeaders;
  // Button "Insert" pressed.
  if (HeadersEditorForm.ShowModal = mrOK) and
     (gridHeaders.SelectedRangeCount > 0) then
  begin
    for i := 0 to gridHeaders.SelectedRangeCount - 1 do
    begin
      Row := gridHeaders.SelectedRange[i].Top;
      requestHeaders.InsertRowWithValues(requestHeaders.Row, [
        '1', // Checked by default
        gridHeaders.Cells[0, Row],
        gridHeaders.Cells[1, Row]
      ]);
    end;
  end;
  UpdateHeadersPickList;
end;

procedure TForm1.miNewClick(Sender: TObject);
begin
  if PromptNewRequest('Are you sure you want to start a new request ?') then
    StartNewRequest;
end;

procedure TForm1.miOpenRequestClick(Sender: TObject);
var
  jsonStr: string;
  streamer: TJSONDeStreamer;
  obj: TRequestObject;
begin
  if not PromptNewRequest('Do you want to open request file ?', 'Open request file') then Exit;

  dlgOpen.DefaultExt := 'json';
  if dlgOpen.Execute then begin

    if not FileGetContents(dlgOpen.FileName, jsonStr) then begin
      ShowMessage('Cannot read file ' + dlgOpen.FileName);
      Exit;
    end;

    streamer := TJSONDeStreamer.Create(nil);
    obj := TRequestObject.Create;

    try
      streamer.JSONToObject(jsonStr, obj);
      StartNewRequest;
      cbUrl.Text := obj.Url;
      cbMethod.Text := obj.Method;
      PostText.Text := obj.Body;
      obj.SetCollectionToGrid(obj.Headers, requestHeaders);
      obj.SetCollectionToGrid(obj.Form, gridForm);
      obj.SetCollectionToGrid(obj.Cookies, gridReqCookie);
    except on E: Exception do
        ShowMessage(E.Message);
    end;

    streamer.Free;
    obj.Free;
  end;
end;

procedure TForm1.miOptionsClick(Sender: TObject);
begin
  OptionsForm.ShowModal;
end;

procedure TForm1.miQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.miAboutClick(Sender: TObject);
begin
  with TAboutForm.Create(Self) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TForm1.gaDeleteRowClick(Sender: TObject);
var
  Component: TComponent;
begin
  Component := TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent;
  if Component is TStringGrid then
    with TStringGrid(Component) do
      if RowCount > 2 then DeleteRow(Row)
      else begin
        // Don't delete last row (user cannot add one) just empty it.
        Cells[0, 1] := '1';
        Cells[1, 1] := '';
        Cells[2, 1] := '';
      end;
end;

procedure TForm1.miSaveRequestClick(Sender: TObject);
var
  obj: TRequestObject;
  streamer: TJSONStreamer;
  json: string;
begin
  obj := TRequestObject.Create;
  streamer := TJSONStreamer.Create(nil);

  try
    obj.Url := cbUrl.Text;
    obj.Method := cbMethod.Text;
    obj.Body := PostText.Text;
    obj.SetCollectionFromGrid(requestHeaders, obj.Headers);
    obj.SetCollectionFromGrid(gridForm, obj.Form);
    obj.SetCollectionFromGrid(gridReqCookie, obj.Cookies);
    json := streamer.ObjectToJSONString(obj);
    try
      dlgSave.FileName := GetRequestFilename('request.json');
      dlgSave.Title := 'Save the request to a file';
      if dlgSave.Execute then
        if not FilePutContents(dlgSave.Filename, json) then
          ShowMessage('Cannot create file ' + dlgSave.FileName);
    except on E: Exception do
      ShowMessage(E.Message);
    end;
  finally
    obj.Free;
    streamer.Free;
  end;
end;

procedure TForm1.miSaveResponseClick(Sender: TObject);
begin
  try
    dlgSave.FileName := GetRequestFilename;
    dlgSave.Title := 'Save the response to a file';
    if dlgSave.Execute then begin
      if tabContent.TabVisible then
        responseRaw.Lines.SaveToFile(dlgSave.FileName)
      else if tabImage.TabVisible then
        respImg.Picture.SaveToFile(dlgSave.FileName);
    end;
  except on E: Exception do
    ShowMessage(E.Message);
  end;
end;

procedure TForm1.miTreeExpandClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := JsonTree.Selected;
  if Assigned(Node) then
  begin
    Node.Expanded := not Node.Expanded;
    if Node.Expanded then Node.Expand(True) else Node.Collapse(True);
  end;
end;

// Show/hide some items in Grid's popup menu.
// Depending on grid popup menu can show or hide some menu items for specific
// grid.
procedure TForm1.popupGridActionsPopup(Sender: TObject);
begin
  gaSaveHeader.Visible := False;
  if GetPopupSenderAsStringGrid(Sender) = requestHeaders then
    gaSaveHeader.Visible := True;
end;

procedure TForm1.PSMAINRestoringProperties(Sender: TObject);
  procedure SetColumns(grid: TStringGrid);
  var
    Val, col: Integer;
  begin
    for col := 1 to grid.ColCount do begin
      Val := PSMAIN.ReadInteger(grid.Name + 'Col' + IntToStr(col), 0);
      if Val > 0 then grid.Columns.Items[col - 1].Width := Val;
    end;
  end;
begin
  SetColumns(requestHeaders);
  SetColumns(responseHeaders);
  SetColumns(gridForm);
  SetColumns(gridReqCookie);
  SetColumns(gridRespCookie);
end;

procedure TForm1.PSMAINSavingProperties(Sender: TObject);
  procedure SaveColumns(grid: TStringGrid);
  var
    I: Integer;
  begin
    for I := 0 to grid.Columns.Count - 1 do
      PSMAIN.WriteInteger(grid.Name + 'Col' + IntToStr(I + 1), grid.Columns.Items[I].Width);
  end;
begin
  SaveColumns(requestHeaders);
  SaveColumns(gridForm);
  SaveColumns(responseHeaders);
  SaveColumns(gridReqCookie);
  SaveColumns(gridRespCookie);
end;

procedure TForm1.requestHeadersBeforeSelection(Sender: TObject; aCol,
  aRow: Integer);
var
  header: string;
begin
  header := Trim(requestHeaders.Cells[1, aRow]);
  if header <> '' then
    HeadersEditorForm.FillHeaderValues(header, requestHeaders.Columns.Items[2].PickList);
end;

procedure TForm1.respImgDblClick(Sender: TObject);
begin
  ImageResize(not respImg.Stretch);
end;

procedure TForm1.OnHttpException(Url, Method: string; E: Exception);
begin
  UpdateStatusLine;
  ShowMessage(E.Message);
  btnSubmit.Enabled := True;
end;

procedure TForm1.ParseContentType(Headers: TStrings);
var
  i: integer;
  kv: TKeyValuePair;
begin
  FContentType := '';
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
  if OptionsForm.JsonExpanded then JsonTree.FullExpand;
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

  if not Assigned(AParent) then
  begin
    AParent := JsonTree.Items.AddChild(nil, '');
    AParent.ImageIndex := ImageTypeMap[Data.JSONType];
    AParent.SelectedIndex := ImageTypeMap[Data.JSONType];
  end;

  Case Data.JSONType of
    jtArray,
    jtObject:
      begin
      If (Data.JSONType = jtArray) then
        AParent.Text := AParent.Text + Format('[%d]', [Data.Count])
      else if (Data.JSONType = jtObject) and (AParent.Text = '') then
        AParent.Text := 'Object';
      S := TstringList.Create;
      try
        For I:=0 to Data.Count-1 do
          If Data.JSONtype = jtArray then
            S.AddObject(IntToStr(I), Data.items[i])
          else
            S.AddObject(TJSONObject(Data).Names[i], Data.items[i]);
        For I:=0 to S.Count-1 do
          begin
          N2 := JsonTree.Items.AddChild(AParent, S[i]);
          D := TJSONData(S.Objects[i]);
          N2.ImageIndex := ImageTypeMap[D.JSONType];
          N2.SelectedIndex := ImageTypeMap[D.JSONType];
          ShowJSONData(N2, D);
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
    AParent.Text := AParent.Text + ': ' + C;
    AParent.Data := Data;
  end;
end;

function TForm1.ParseHeaderLine(line: string; delim: char = ':'; all: Boolean = False): TKeyValuePair;
var
  p: integer;
begin
  p := Pos(delim, line);
  if p = 0 then
  begin
    Result.Key := line;
    Result.Value := '';
    Exit; // =>
  end;
  Result.Key := LeftStr(line, p - 1);
  Result.Value := Trim(RightStr(line, Length(line) - p));
  p := Pos(';', Result.Value);
  if (not all) and (p <> 0) then Result.Value := Trim(LeftStr(Result.Value, p - 1));
end;

procedure TForm1.UpdateHeadersPickList;
begin
  HeadersEditorForm.FillHeaders(requestHeaders.Columns.Items[1].PickList);
  requestHeaders.Cells[0, 1] := '1';
end;

function TForm1.EncodeFormData: string;
var
  i: integer;
  n, v: string;
begin
  Result := '';
  for i:=0 to gridForm.RowCount-1 do
  begin
    if gridForm.Cells[0, i] = '0' then continue;
    n := Trim(gridForm.Cells[1, i]); // Name
    v := Trim(gridForm.Cells[2, i]); // Value
    if n = '' then continue; // Skip empty names
    if Result <> '' then Result := Result + '&';
    Result := Result + EncodeURLElement(n) + '=' + EncodeURLElement(v);
  end;
end;

procedure TForm1.OnRequestComplete(Info: TResponseInfo);
var
  i, p: integer;
  h: string;
begin
  btnSubmit.Enabled := True;
  SetAppCaption(cbUrl.Text);

  responseHeaders.RowCount := Info.ResponseHeaders.Count + 1;
  for i := 0 to Info.ResponseHeaders.Count - 1 do
  begin
    h := Info.ResponseHeaders.Strings[i];
    p := Pos(':', h);
    responseHeaders.Cells[0, i + 1] := LeftStr(h, p - 1);
    responseHeaders.Cells[1, i + 1] := trim(RightStr(h, Length(h) - p));
  end;
  ParseContentType(Info.ResponseHeaders);

  UpdateStatusLine(
    Format('HTTP/%s %d %s', [Info.HttpVersion, Info.StatusCode, Info.StatusText]),
    Format('%d ms', [Info.Time])
  );

  if (cbUrl.Items.IndexOf(Info.Url) = -1) and (Info.StatusCode <> 404) then
  begin
    if cbUrl.Items.Count >= MAX_URLS then
    begin
      cbUrl.Items.Delete(cbUrl.Items.Count - 1);
      // FIXME: delete last item also deletes and text ?
      cbUrl.Text := Info.Url;
    end;
    cbUrl.Items.Insert(0, Info.Url);
  end;

  // Fill response cookie grid or hide it.
  ShowResponseCookie(Info.ResponseHeaders);

  // Get the response content - enable menu item.
  //miSaveResponse.Enabled := Length(responseRaw.Text) > 0;
  miSaveResponse.Enabled := True;

  ShowHideResponseTabs(Info);
end;

procedure TForm1.UpdateStatusLine(Text1: string = ''; Text2: string = '');
var
  w: Integer;
begin
  StatusText1.Caption := Text1;
  if Text2 = '' then
  begin
    StatusText2.Caption := '';
    StatusImage1.Visible := False;
    StatusText2.Visible := False;
  end
  else begin
    // Manual place components. Align property in the some cases can
    // lead to exchange of order of the image and text.
    w := StatusText1.Left + StatusText1.Width + 8;
    StatusImage1.Left := w;
    StatusText2.Left := w + StatusImage1.Width + 2;
    StatusImage1.Visible := True;
    StatusText2.Visible := True;
    StatusText2.Caption := Text2;
  end;
end;

procedure TForm1.ShowResponseCookie(Headers: TStrings);
var
  I, J, Row, Size: Integer;
  kv: TKeyValuePair;
  tokens: TStringList;
  tok: String;
begin
  Row := 1;
  tokens := TStringList.Create;
  tokens.Delimiter := ';';
  tokens.StrictDelimiter := True;

  try
    for I := 0 to Headers.Count - 1 do begin
      kv := ParseHeaderLine(Headers.Strings[I], ':', True);
      if LowerCase(kv.Key) = 'set-cookie' then begin
        gridRespCookie.RowCount := Row + 1;
        tokens.DelimitedText := kv.Value;
        Size := 0;
        // Reset grid.
        for J := 0 to gridRespCookie.ColCount - 1 do
          if (J = 6) or (J = 7) then
            gridRespCookie.Cells[J, Row] := '0'
          else
            gridRespCookie.Cells[J, Row] := '';
        // Fill grid.
        for J := 0 to tokens.Count - 1 do begin
          tok := Trim(tokens.Strings[J]);
          kv := ParseHeaderLine(tok, '=');
          // Cookie name and value
          if J = 0 then begin
            gridRespCookie.Cells[0, Row] := kv.Key;
            gridRespCookie.Cells[1, Row] := DecodeURL(kv.Value);
            Size := Length(kv.Value); // Cookie size
          end
          else
            case LowerCase(kv.Key) of
              'domain'  : gridRespCookie.Cells[2, Row] := kv.Value;
              'path'    : gridRespCookie.Cells[3, Row] := kv.Value;
              'expires' : gridRespCookie.Cells[4, Row] := kv.Value;
              'httponly': gridRespCookie.Cells[6, Row] := '1';
              'secure'  : gridRespCookie.Cells[7, Row] := '1';
              'samesite': gridRespCookie.Cells[8, Row] := kv.Value;
            end;
        end; // for
        // Cookie size
        gridRespCookie.Cells[5, Row] := IntToStr(Size);
        Inc(Row);
      end;
    end; // for
  finally
    tokens.Free;
  end;

  if Row > 1 then tabRespCookie.TabVisible := True
    else tabRespCookie.TabVisible := False;
end;

// Creates a filename based on a request.
// If parameter 'ext' is empty then extension will be detected depending on
// the document.
function TForm1.GetRequestFilename(ext: string): string;
var
  uri: TURI;
  basename: string;
begin
  uri := ParseURI(NormalizeUrl);
  if ext = '' then
    case FContentType of
      'text/html': ext := 'html';
      'text/plain': ext := 'txt';
      'application/json': ext := 'json';
      'application/javascript': ext := 'js';
      'application/xml': ext := 'xml';
      'image/png': ext := 'png';
      'image/jpg', 'image/jpeg': ext := 'jpg';
      else ext := 'data';
    end;
  basename := TrimSet(uri.Document, ['/']);
  if basename = '' then
    basename := uri.Host;
  // Strip extension from a document name (mainly for images).
  if RightStr(basename, Length(ext) + 1) = '.' + ext then
    basename := LeftStr(basename, Length(basename) - Length(ext) - 1);
  Result := Format('%s.%s', [basename, ext]);
end;

function TForm1.PromptNewRequest(const prompt: string; const promptTitle: string = 'New request'): Boolean;
var
  NeedConfirm: Boolean;
  I: Integer;
  function IsGridFilled(grid: TStringGrid): Boolean;
  var I: Integer;
  begin
    Result := False;
    for I := 1 to grid.RowCount - 1 do
      if (Trim(grid.Cells[1, I]) <> '') or (Trim(grid.Cells[2, I]) <> '') then Exit(True);
  end;
begin
  // Is confirmation needed ?
  NeedConfirm := False;
  // Check body post data.
  if Trim(PostText.Text) <> '' then NeedConfirm := True;
  // Check grids.
  if not NeedConfirm then NeedConfirm := IsGridFilled(requestHeaders);
  if not NeedConfirm then NeedConfirm := IsGridFilled(gridForm);

  if NeedConfirm then
  begin
    I := Application.MessageBox(PChar(prompt), PChar(promptTitle), MB_ICONQUESTION + MB_YESNO);
    if I <> IDYES then Exit(False); // =>
  end;

  Result := True;
end;

procedure TForm1.StartNewRequest;
begin
  // Request fields.
  cbUrl.Text := '';
  cbMethod.Text := 'GET';
  PostText.Text := '';
  requestHeaders.RowCount := 2;
  requestHeaders.Cells[0, 1] := '1';
  requestHeaders.Cells[1, 1] := '';
  requestHeaders.Cells[2, 1] := '';
  gridForm.RowCount := 2;
  gridForm.Cells[0, 1] := '1';
  gridForm.Cells[1, 1] := '';
  gridForm.Cells[2, 1] := '';
  gridReqCookie.RowCount := 2;
  gridReqCookie.Cells[0, 1] := '1';
  gridReqCookie.Cells[1, 1] := '';
  gridReqCookie.Cells[2, 1] := '';

  // Response fields.
  responseHeaders.RowCount := 1;
  responseRaw.Text := '';
  if tabJson.TabVisible then
  begin
    JsonTree.Items.Clear;
    tabJson.TabVisible := False;
  end;
  pagesResponse.ActivePage := tabResponse;
  miSaveResponse.Enabled := False;

  SetAppCaption;
end;

// Get StringGrid instance from Popup sender.
// The sender can be a Popup menu or popup menu item.
// Returns nil when popup doesn't belongs to grid.
// Raises an exception when sender isn't popup or menu item.
function TForm1.GetPopupSenderAsStringGrid(Sender: TObject): TStringGrid;
var
  Component: TComponent;
begin
  // What the sender is: popup or menu item ?
  if Sender is TMenuItem then
    Component := TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent
  else
    if Sender is TPopupMenu then
      Component := TPopupMenu(Sender).PopupComponent
  else
    raise Exception.Create('Sender is not Popup or MenuItem.');

  if Component is TStringGrid then
    Result := TStringGrid(Component)
  else if Component is TStringCellEditor then
    Result := TStringCellEditor(Component).Parent as TStringGrid
  else
    Result := nil;
end;

procedure TForm1.EditGridRow(Grid: TStringGrid);
var
  kv: TKeyValuePair;
begin
  with Grid do begin
    kv := KeyValueForm.Edit(Cells[1, Row], Cells[2, Row], 'Edit...');
    Cells[1, Row] := kv.Key;
    Cells[2, Row] := kv.Value;
  end;
end;

function TForm1.NormalizeUrl: string;
begin
  Result := LowerCase(Trim(cbUrl.Text));
  if Result = '' then
    raise Exception.Create('Url is empty.');
  if Pos('http://', Result) = 0 then
    if Pos('https://', Result) = 0 then
      Result := 'http://' + Result;
end;

procedure TForm1.SetAppCaption(const AValue: String);
begin
  Caption := ApplicationName;
  if AValue <> '' then Caption := Caption + ': ' + AValue;
end;

// Depending on content type - show/hide response tabs.
procedure TForm1.ShowHideResponseTabs(Info: TResponseInfo);
var
  subtype: string;
begin
  Info.Content.Position := 0;

  responseRaw.Clear;
  respImg.Picture.Clear;
  StatusText3.Caption := '';
  subtype := UpperCase(GetContentSubtype);

  case FContentType of
    'application/json':
      begin
        JsonDocument(Info.Content.DataString);
        tabImage.TabVisible := False;
        tabContent.TabVisible := True;
        tabJson.TabVisible := True;
      end;

    'image/jpeg',
    'image/jpg',
    'image/png',
    'image/gif':
      begin
        respImg.Picture.LoadFromStream(Info.Content);
        ImageResize(False);
        tabContent.TabVisible := False;
        tabJson.TabVisible := False;
        tabImage.TabVisible := True;
        if subtype = 'JPEG' then subtype := 'JPG';
        StatusText3.Caption := Format('%s: %d x %d', [subtype, respImg.Picture.Width, respImg.Picture.Height]);
      end;

    else
      begin
        tabImage.TabVisible := False;
        tabJson.TabVisible := False;
        tabContent.TabVisible := True;
      end;
  end;

  if tabContent.TabVisible then begin
    responseRaw.Append(Info.Content.DataString);
    responseRaw.CaretPos := Point(0, 0);
  end;
end;

// Resize response image.
// ToStretch = True stretch image.
// ToStretch = False set image original size.
procedure TForm1.ImageResize(ToStretch: Boolean);
begin
  with respImg do
    if ToStretch then begin
      Width := scrollImage.ClientWidth;
      Height := scrollImage.ClientHeight;
      Proportional := True;
      Stretch := True;
    end
    else begin
      Width := Picture.Width;
      Height := Picture.Height;
      Proportional := False;
      Stretch := False;
    end;
end;

// Returns subtype from content type property.
// For example, for 'application/json' it returns 'json'.
function TForm1.GetContentSubtype: string;
var
  p: integer;
begin
  if FContentType = '' then
    raise Exception.Create('Cannot get subtype. Content type is empty');
  p := Pos('/', FContentType);
  Result := RightStr(FContentType, Length(FContentType) - p);
end;

end.

