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
    miNew: TMenuItem;
    Panel1: TPanel;
    pagesRequest: TPageControl;
    PostText: TMemo;
    requestHeaders: TStringGrid;
    Splitter1: TSplitter;
    StatusImage1: TImage;
    jsImages: TImageList;
    JsonTree: TTreeView;
    StatusText1: TLabel;
    StatusText2: TLabel;
    MenuItem4: TMenuItem;
    gaClearRows: TMenuItem;
    miInsertHeader: TMenuItem;
    gaDeleteRow: TMenuItem;
    pagesResponse: TPageControl;
    StatusPanel: TPanel;
    Panel2: TPanel;
    popupGridActions: TPopupMenu;
    PSMAIN: TJSONPropStorage;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miTreeExpand: TMenuItem;
    miQuit: TMenuItem;
    miAbout: TMenuItem;
    MenuItem6: TMenuItem;
    responseHeaders: TStringGrid;
    responseRaw: TMemo;
    tabContent: TTabSheet;
    tabForm: TTabSheet;
    tabJson: TTabSheet;
    tabResponse: TTabSheet;
    tabHeaders: TTabSheet;
    tabBody: TTabSheet;
    procedure btnSubmitClick(Sender: TObject);
    procedure cbUrlKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure gaClearRowsClick(Sender: TObject);
    procedure JsonTreeClick(Sender: TObject);
    procedure miInsertHeaderClick(Sender: TObject);
    procedure miNewClick(Sender: TObject);
    procedure miQuitClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure gaDeleteRowClick(Sender: TObject);
    procedure miTreeExpandClick(Sender: TObject);
    procedure PSMAINRestoringProperties(Sender: TObject);
    procedure PSMAINSavingProperties(Sender: TObject);
    procedure requestHeadersBeforeSelection(Sender: TObject; aCol, aRow: Integer
      );
  private
    FContentType: string;
    FJsonRoot: TJSONData;
    FHttpClient: TThreadHttpClient;
    procedure OnHttpException(Url, Method: string; E: Exception);
    procedure ParseContentType(Headers: TStrings);
    procedure JsonDocument(json: string);
    procedure ShowJsonDocument;
    procedure ShowJsonData(AParent: TTreeNode; Data: TJSONData);
    function ParseHeaderLine(line: string): TKeyValuePair;
    procedure UpdateHeadersPickList;
    function EncodeFormData: string;
    procedure OnRequestComplete(Info: TResponseInfo);
    procedure UpdateStatusLine(Text1: string = ''; Text2: string = '');
  public

  end;

var
  Form1: TForm1;

implementation

uses lcltype, jsonparser, about, headers_editor;

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
  url := Trim(cbUrl.Text);
  if url = '' then
  begin
    cbUrl.SetFocus;
    exit;
  end;

  if Pos('http', url) = 0 then url := 'http://' + url;

  FContentType := '';
  isForm := False;
  ctForm := False;

  FHttpClient := TThreadHttpClient.Create(true);
  FHttpClient.OnRequestComplete := @OnRequestComplete;
  FHttpClient.OnException := @OnHttpException;

  method := UpperCase(Trim(cbMethod.Text));
  if method = '' then method := 'GET';
  if (method = 'POST') or (method = 'PUT') then
  begin
    formData := EncodeFormData;
    // form and body filled, what to submit ?
    if (Length(formData) <> 0) and (Length(Trim(PostText.Text)) <> 0) then
    begin
      i := QuestionDlg(
        'Choose what to submit',
        'Do you want to submit form data or body ?',
        mtCustom,
        [mrYes, 'Form', mrNo, 'Body', mrCancel, 'Cancel'],
        ''
      );
      if i = mrNo then formData := PostText.Text
      else if i = mrYes then isForm := True
      else begin
        FreeAndNil(FHttpClient);
        Exit; // =>
      end;
    end
    else if Length(formData) > 0 then isForm := True;
    FHttpClient.RequestBody := TStringStream.Create(formData);
  end;

  btnSubmit.Enabled := False;
  miTreeExpand.Enabled := False;
  // Assign request headers to the client.
  for i:=1 to requestHeaders.RowCount-1 do
  begin
    key := trim(requestHeaders.Cells[0, i]);
    if key = '' then continue;
    value := trim(requestHeaders.Cells[1, i]);
    if isForm and (LowerCase(key) = 'content-type') then
      if LowerCase(value) <> 'application/x-www-form-urlencoded' then
      begin
        value := 'application/x-www-form-urlencoded';
        requestHeaders.Cells[1, i] := value;
        ctForm := True
      end;
    FHttpClient.AddHeader(key, value);
  end;
  // It's a form submit request but there is no form content type in the
  // headers grid. Append one to the grid.
  if isForm and not ctForm then
  begin
    key := 'Content-Type'; value := 'application/x-www-form-urlencoded';
    FHttpClient.AddHeader(key, value);
    with requestHeaders do begin
      Cells[0, RowCount - 1] := key;
      Cells[1, RowCount - 1] := value;
    end;
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
  Caption := ApplicationName;
  C := GetAppConfigFile(False, True);
  PSMAIN.JSONFileName := C;
  C := ExtractFilePath(C);
  if not ForceDirectories(C) then ShowMessage(Format('Cannot create directory "%s"', [C]));
  PSMAIN.Active := True;
  StatusText2.Caption := '';
  HeadersEditorForm := THeadersEditorForm.Create(Application);
  UpdateHeadersPickList;
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
  Component: TComponent;
  Answer: Integer;
begin
  Answer := Application.MessageBox('Are you sure to clear content ?', 'Clear rows', MB_ICONQUESTION + MB_YESNO);
  if Answer = IDNO then Exit; // =>
  Component := TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent;
  if Component is TStringGrid then
    with TStringGrid(Component) do begin
      RowCount := 2;
      Cells[0, 1] := '';
      Cells[1, 1] := '';
    end;
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
        gridHeaders.Cells[0, Row],
        gridHeaders.Cells[1, Row]
      ]);
    end;
    requestHeaders.SetFocus;
  end;
  UpdateHeadersPickList;
end;

procedure TForm1.miNewClick(Sender: TObject);
var
  NeedConfirm: Boolean;
  I: Integer;
  function IsGridFilled(grid: TStringGrid): Boolean;
  var I: Integer;
  begin
    Result := False;
    for I := 1 to grid.RowCount - 1 do
      if (Trim(grid.Cells[0, I]) <> '') or (Trim(grid.Cells[1, I]) <> '') then Exit(True);
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
    I := Application.MessageBox('Are you sure to start a new request ?', 'New', MB_ICONQUESTION + MB_YESNO);
    if I = IDNO then Exit; // =>
  end;

  // Reset fields to start a new request.
  // Request fields.
  cbUrl.Text := '';
  cbMethod.Text := 'GET';
  PostText.Text := '';
  requestHeaders.RowCount := 2;
  requestHeaders.Cells[0, 1] := ''; requestHeaders.Cells[1, 1] := '';
  gridForm.RowCount := 2;
  gridForm.Cells[0, 1] := ''; gridForm.Cells[1, 1] := '';

  // Response fields.
  responseHeaders.RowCount := 1;
  responseRaw.Text := '';
  if tabJson.TabVisible then
  begin
    JsonTree.Items.Clear;
    tabJson.TabVisible := False;
  end;
  pagesResponse.ActivePage := tabResponse;
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
        Cells[0, 1] := '';
        Cells[1, 1] := '';
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

procedure TForm1.PSMAINRestoringProperties(Sender: TObject);
  procedure SetColumn(grid: TStringGrid; col: Integer);
  var
    Val: Integer;
  begin
    Val := PSMAIN.ReadInteger(grid.Name + 'Col' + IntToStr(col), 0);
    if Val > 0 then grid.Columns.Items[col - 1].Width := Val;
  end;
begin
  SetColumn(requestHeaders, 1);
  SetColumn(requestHeaders, 2);
  SetColumn(responseHeaders, 1);
  SetColumn(responseHeaders, 2);
end;

procedure TForm1.PSMAINSavingProperties(Sender: TObject);
begin
  PSMAIN.WriteInteger(requestHeaders.Name + 'Col1', requestHeaders.Columns.Items[0].Width);
  PSMAIN.WriteInteger(requestHeaders.Name + 'Col2', requestHeaders.Columns.Items[1].Width);
  PSMAIN.WriteInteger(responseHeaders.Name + 'Col1', responseHeaders.Columns.Items[0].Width);
  PSMAIN.WriteInteger(responseHeaders.Name + 'Col2', responseHeaders.Columns.Items[1].Width);
end;

procedure TForm1.requestHeadersBeforeSelection(Sender: TObject; aCol,
  aRow: Integer);
var
  header: string;
begin
  header := Trim(requestHeaders.Cells[0, aRow]);
  if header <> '' then
    HeadersEditorForm.FillHeaderValues(header, requestHeaders.Columns.Items[1].PickList);
end;

procedure TForm1.OnHttpException(Url, Method: string; E: Exception);
begin
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
  tabJson.TabVisible := True;
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

procedure TForm1.UpdateHeadersPickList;
begin
  HeadersEditorForm.FillHeaders(requestHeaders.Columns.Items[0].PickList);
end;

function TForm1.EncodeFormData: string;
var
  i: integer;
  n, v: string;
begin
  Result := '';
  for i:=0 to gridForm.RowCount-1 do
  begin
    n := Trim(gridForm.Cells[0, i]); // Name
    v := Trim(gridForm.Cells[1, i]); // Value
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

  responseHeaders.RowCount := Info.ResponseHeaders.Count + 1;
  for i := 0 to Info.ResponseHeaders.Count - 1 do
  begin
    h := Info.ResponseHeaders.Strings[i];
    p := Pos(':', h);
    responseHeaders.Cells[0, i + 1] := LeftStr(h, p - 1);
    responseHeaders.Cells[1, i + 1] := trim(RightStr(h, Length(h) - p));
  end;
  ParseContentType(Info.ResponseHeaders);

  responseRaw.Clear;
  responseRaw.Append(Info.Content.DataString);
  responseRaw.CaretPos := Point(0, 0);

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

  if FContentType = 'application/json' then JsonDocument(responseRaw.Text)
  else begin
    tabJson.TabVisible := False;
  end;
end;

procedure TForm1.UpdateStatusLine(Text1: string = ''; Text2: string = '');
begin
  StatusText1.Caption := Text1;
  if Text2 = '' then
  begin
    StatusText2.Caption := '';
    StatusImage1.Visible := False;
    StatusText2.Visible := False;
    StatusText2.Align := alNone;
  end
  else begin
    StatusImage1.Visible := True; // Must be first otherwise will be after label
    StatusText2.Visible := True;
    StatusText2.Caption := Text2;
    StatusText2.Align := alLeft;
  end;
end;

end.

