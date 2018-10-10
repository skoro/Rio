unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Dialogs, StdCtrls, ComCtrls, ValEdit, ExtCtrls, Grids, Menus,
  fphttpclient, fpjson, Controls, JSONPropStorage, PairSplitter, SynEdit,
  SynHighlighterJScript, thread_http_client, response_tabs, key_value,
  GridNavigator, SysUtils, jsonparser;

type

  TBodyTab = (btForm, btJson, btOther);
  TAuthTab = (atNone = -1, atBasic, atBearer);
  TGridOperation = (goNew, goEdit, goDelete, goClear);

  { TForm1 }

  TForm1 = class(TForm)
    btnSubmit: TButton;
    cbBasicShowPassword: TCheckBox;
    cbMethod: TComboBox;
    cbUrl: TComboBox;
    editBasicLogin: TLabeledEdit;
    editBasicPassword: TLabeledEdit;
    editBearerPrefix: TLabeledEdit;
    editBearerToken: TLabeledEdit;
    editJson: TSynEdit;
    editOther: TMemo;
    dlgFind: TFindDialog;
    gaInsertRow: TMenuItem;
    gaEditRow: TMenuItem;
    gridForm: TStringGrid;
    gnavParams: TGridNavigator;
    gnavHeaders: TGridNavigator;
    gnavCookie: TGridNavigator;
    gnavBody: TGridNavigator;
    gridParams: TStringGrid;
    gridReqCookie: TStringGrid;
    gridRespCookie: TStringGrid;
    gaManageHeaders: TMenuItem;
    gaSaveHeader: TMenuItem;
    gaSeparator: TMenuItem;
    miImport: TMenuItem;
    miFindNext: TMenuItem;
    miSep1: TMenuItem;
    miFind: TMenuItem;
    miHelpCmd: TMenuItem;
    miJsonFilter: TMenuItem;
    StatusImageSize: TImage;
    StatusTextInfo: TLabel;
    StatusTextSize: TLabel;
    miAuthNone: TMenuItem;
    miAuthBasic: TMenuItem;
    miAuthBearer: TMenuItem;
    miJsonView: TMenuItem;
    miJsonCopyValue: TMenuItem;
    miJsonCopyKey: TMenuItem;
    miJsonCopyValueKey: TMenuItem;
    miBodyForm: TMenuItem;
    miBodyJson: TMenuItem;
    miBodyOther: TMenuItem;
    miNewWindow: TMenuItem;
    miOptions: TMenuItem;
    pagesAuth: TPageControl;
    pagesBody: TPageControl;
    pagesRequest: TPageControl;
    pagesResponse: TPageControl;
    LayoutSplitter: TPairSplitter;
    splitterSideRequest: TPairSplitterSide;
    splitterSideResponse: TPairSplitterSide;
    panelUrl: TPanel;
    panelRequest: TPanel;
    panelResponse: TPanel;
    popupJsonTree: TPopupMenu;
    pmBodyType: TPopupMenu;
    pmAuthType: TPopupMenu;
    requestHeaders: TStringGrid;
    miOpenRequest: TMenuItem;
    miSaveRequest: TMenuItem;
    miSaveResponse: TMenuItem;
    miNew: TMenuItem;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    jsImages: TImageList;
    miEdit: TMenuItem;
    gaClearRows: TMenuItem;
    miManageHeaders: TMenuItem;
    gaDeleteRow: TMenuItem;
    popupGridActions: TPopupMenu;
    PSMAIN: TJSONPropStorage;
    AppMenu: TMainMenu;
    miFile: TMenuItem;
    miHelp: TMenuItem;
    miQuit: TMenuItem;
    miAbout: TMenuItem;
    MenuItem6: TMenuItem;
    responseHeaders: TStringGrid;
    responseRaw: TMemo;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    StatusImageTime: TImage;
    StatusPanel: TPanel;
    StatusTextMain: TLabel;
    StatusTextTime: TLabel;
    synJS: TSynJScriptSyn;
    tabBody: TTabSheet;
    tabBodyForm: TTabSheet;
    tabBodyJson: TTabSheet;
    tabBodyOther: TTabSheet;
    tabContent: TTabSheet;
    tabHeaders: TTabSheet;
    tabQuery: TTabSheet;
    tabReqCookie: TTabSheet;
    tabRespCookie: TTabSheet;
    tabResponse: TTabSheet;
    tabAuth: TTabSheet;
    tabAuthBasic: TTabSheet;
    tabAuthBearer: TTabSheet;
    TimerRequest: TTimer;
    toolbarAuth: TToolBar;
    ToolButton1: TToolButton;
    tbtnManageHeaders: TToolButton;
    tbtnSaveHeader: TToolButton;
    tbtnBodyType: TToolButton;
    tbtnBodyFormat: TToolButton;
    tbtnFormUpload: TToolButton;
    tbtnAuthType: TToolButton;
    procedure btnSubmitClick(Sender: TObject);
    procedure cbBasicShowPasswordClick(Sender: TObject);
    procedure cbUrlChange(Sender: TObject);
    procedure cbUrlKeyPress(Sender: TObject; var Key: char);
    procedure dlgFindFind(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure gaClearRowsClick(Sender: TObject);
    procedure gaEditRowClick(Sender: TObject);
    procedure gaInsertRowClick(Sender: TObject);
    procedure gridColRowInserted(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure gridEditDblClick(Sender: TObject);
    procedure gridFormSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure gridParamsCheckboxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure gridParamsEditingDone(Sender: TObject);
    procedure gridRespCookieDblClick(Sender: TObject);
    procedure miFindClick(Sender: TObject);
    procedure miFindNextClick(Sender: TObject);
    procedure miHelpCmdClick(Sender: TObject);
    procedure miImportClick(Sender: TObject);
    procedure miManageHeadersClick(Sender: TObject);
    procedure JsonTreeDblClick(Sender: TObject);
    procedure JsonTreePopupMenuClick(Sender: TObject);
    procedure miNewClick(Sender: TObject);
    procedure miNewWindowClick(Sender: TObject);
    procedure miOpenRequestClick(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure miQuitClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure gaDeleteRowClick(Sender: TObject);
    procedure miSaveRequestClick(Sender: TObject);
    procedure miSaveResponseClick(Sender: TObject);
    procedure OnGridClear(Sender: TObject; Grid: TStringGrid);
    procedure OnGridDeleteRow(Sender: TObject; Grid: TStringGrid);
    procedure OnGridEditRow(Sender: TObject; Grid: TStringGrid;
      const aRow: Integer);
    procedure OnGridNewRow(Sender: TObject; Grid: TStringGrid;
      const aRow: Integer);
    procedure pmAuthTypeClick(Sender: TObject);
    procedure pmBodyTypeClick(Sender: TObject);
    procedure popupGridActionsPopup(Sender: TObject);
    procedure PSMAINRestoreProperties(Sender: TObject);
    procedure PSMAINRestoringProperties(Sender: TObject);
    procedure PSMAINSavingProperties(Sender: TObject);
    procedure requestHeadersBeforeSelection(Sender: TObject; aCol, aRow: Integer
      );
    procedure tbtnFormUploadClick(Sender: TObject);
    procedure tbtnManageHeadersClick(Sender: TObject);
    procedure tbtnBodyFormatClick(Sender: TObject);
    procedure tbtnSaveHeaderClick(Sender: TObject);
    procedure TimerRequestTimer(Sender: TObject);
  private
    FContentType: string;
    FHttpClient: TThreadHttpClient;
    FResponseTabManager: TResponseTabManager;
    FResponseJsonTab: TResponseJsonTab;
    FFindTextPos: Integer;
    FRequestSeconds: Integer;
    procedure OnHttpException(Url, Method: string; E: Exception);
    procedure ParseContentType(Headers: TStrings);
    function ParseHeaderLine(line: string; delim: char = ':'; all: Boolean = False): TKeyValuePair;
    procedure UpdateHeadersPickList;
    function EncodeFormData: string;
    procedure OnRequestComplete(Info: TResponseInfo);
    procedure UpdateStatusLine(Main: string = '');
    procedure UpdateStatusLine(Info: TResponseInfo);
    procedure ShowResponseCookie(Headers: TStrings);
    function GetRequestFilename(ext: string = ''): string;
    function PromptNewRequest(const prompt: string; const promptTitle: string = 'New request'): Boolean;
    function GetPopupSenderAsStringGrid(Sender: TObject): TStringGrid;
    function EditGridRow(Grid: TStringGrid;
      const ValueFocused: Boolean = False): TModalResult;
    function NormalizeUrl: string;
    procedure SetAppCaption(const AValue: String = '');
    procedure SyncURLQueryParams;
    procedure SyncGridQueryParams;
    function FormatJson(json: TJSONData): string;
    function IsRowEnabled(const grid: TStringGrid; aRow: Integer = -1): Boolean;
    function GetRowKV(const grid: TStringGrid; aRow: Integer = -1): TKeyValue;
    procedure DoGridOperation(Grid: TStringGrid; const op: TGridOperation);
    procedure OnOpenResponseTab(Tab: TResponseTab; ResponseInfo: TResponseInfo);
    procedure OnSaveResponseTab(const FileName: string; Tab: TResponseTab);
    procedure OnJsonTabButtonOptionsClick(Sender: TObject);
    procedure JsonTab_OnJsonFormat(JsonData: TJSONData; Editor: TSynEdit);
    procedure FindStart(Search: Boolean = True);
  public
    procedure ApplyOptions;
    function SetRowKV(AGrid: TStringGrid; KV: TKeyValuePair;
      aRow: Integer = -1; isUnique: Boolean = True): Integer;
    procedure AddRequestHeader(AHeader, AValue: string);
    procedure AddFormData(AName, AValue: string; isFile: Boolean = False);
    procedure OpenRequestFile(jsonStr: string);
    procedure SelectBodyTab(const tab: tbodytab);
    procedure SelectAuthTab(const tab: TAuthTab);
    function GetSelectedBodyTab: TBodyTab;
    function GetSelectedAuthTab: TAuthTab;
    procedure StartNewRequest;
    function SetJsonBody(jsonStr: string; var ErrMsg: string): Boolean;
    procedure SubmitRequest;
    procedure FindText;
  end;

var
  Form1: TForm1;

implementation

uses lcltype, about, headers_editor, cookie_form, uriparser, request_object,
  app_helpers, fpjsonrtti, strutils, help_form, cmdline, options,
  import_form, Clipbrd;

const
  MAX_URLS = 15; // How much urls we can store in url dropdown history.

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnSubmitClick(Sender: TObject);
begin
  SubmitRequest;
  FRequestSeconds := 0;
  TimerRequest.Enabled := True;
end;

procedure TForm1.SubmitRequest;
var
  url, method, formData, contentType: string;
  i: integer;
  KV: TKeyValue;
  FileNames, FormValues: TStringList;
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
  formData := '';
  contentType := '';

  FHttpClient := TThreadHttpClient.Create(true);
  FHttpClient.OnRequestComplete := @OnRequestComplete;
  FHttpClient.OnException := @OnHttpException;
  FHttpClient.Client.IOTimeout := OptionsForm.Timeout * TimerRequest.Interval;

  method := UpperCase(Trim(cbMethod.Text));
  if method = '' then method := 'GET';

  // Post the form data.
  if (method = 'POST') and (GetSelectedBodyTab = btForm) then begin
    if gridForm.Cols[3].IndexOf('File') = -1 then begin
      formData := EncodeFormData;
      contentType := 'application/x-www-form-urlencoded';
    end
    else begin
      // Form multi file upload.
      FileNames := TStringList.Create;
      FormValues := TStringList.Create;
      try
        for I := 1 to gridForm.RowCount - 1 do begin
          if IsRowEnabled(gridForm, I) then begin
            KV := GetRowKV(gridForm, I);
            if gridForm.Cells[3, I] = 'File' then
              FileNames.Values[KV.Key] := KV.Value
            else
              FormValues.Values[KV.Key] := KV.Value;
          end;
        end;
        try
          FHttpClient.Client.MultiFileStreamFormPost(FormValues, FileNames);
          // Content-type header is already added to the http client.
          // Remove any content-type from request grid.
          for I := 1 to requestHeaders.RowCount - 1 do
            if IsRowEnabled(requestHeaders, I) then begin
              KV := GetRowKV(requestHeaders, I);
              if LowerCase(KV.Key) = 'content-type' then begin
                requestHeaders.DeleteRow(I);
                Break;
              end;
            end;
        except on E: Exception do
          begin
            FreeAndNil(FHttpClient);
            ShowMessage(E.Message);
            Exit;
          end;
        end;
      finally
        FreeAndNil(FileNames);
        FreeAndNil(FormValues);
      end;
    end;
  end;

  // Post, put, delete, etc other data.
  if (method <> 'GET') and (GetSelectedBodyTab <> btForm) then begin
    case GetSelectedBodyTab of
      btJson : begin
        formData := editJson.Text;
        contentType := 'application/json';
      end;
      btOther: formData:=editOther.Text;
    end;
  end;

  if (method <> 'GET') and (Length(formData) > 0) then
    FHttpClient.RequestBody := TStringStream.Create(formData);

  btnSubmit.Enabled := False;

  // Assign request headers to the client.
  for i:=1 to requestHeaders.RowCount-1 do
  begin
    KV := GetRowKV(requestHeaders, i);
    if (LowerCase(KV.Key) = 'content-type') and (contentType <> '') then begin
      if LowerCase(KV.Value) <> contentType then begin
        requestHeaders.Cells[2, i] := contentType;
        KV.Value := contentType;
        if not IsRowEnabled(requestHeaders, i) then
          requestHeaders.Cells[0, i] := '1';
      end;
      contentType := ''; // No need to insert content type below.
    end;
    if (IsRowEnabled(requestHeaders, i)) and (Trim(KV.Key) <> '') then
      FHttpClient.AddHeader(KV.key, KV.value);
  end;

  if contentType <> '' then begin
    requestHeaders.InsertRowWithValues(1, ['1', 'Content-Type', contentType]);
    FHttpClient.AddHeader('Content-Type', contentType);
  end;

  // Set request cookies
  for I := 1 to gridReqCookie.RowCount - 1 do
  begin
    if not IsRowEnabled(gridReqCookie, I) then continue;
    kv := GetRowKV(gridReqCookie, I);
    if kv.key = '' then continue;
    FHttpClient.AddCookie(kv.key, kv.value);
  end;

  // Process auth tab
  case GetSelectedAuthTab of
    atBearer: begin
      if (Length(editBearerPrefix.Text) > 0)
           and (Length(editBearerToken.Text) > 0)
      then
        FHttpClient.AddHeader('Authorization', Format('%s %s', [
          editBearerPrefix.Text,
          editBearerToken.Text
        ]));
    end;
    atBasic: begin
      FHttpClient.Client.UserName := editBasicLogin.Text;
      FHttpClient.Client.Password := editBasicPassword.Text;
    end;
  end;

  SyncURLQueryParams;
  UpdateStatusLine('Waiting for the response...');

  FHttpClient.Url := url;
  FHttpClient.Method := method;
  FHttpClient.Start;
end;

procedure TForm1.FindText;
var
  fp: TFindPos;
  tab: TResponseTab;
  FindSucc: Integer;
  Ans: Integer;
begin
  tab := FResponseTabManager.CanFind;
  if (tab <> nil) and (pagesResponse.ActivePage <> tabContent) then begin
    pagesResponse.ActivePage := tab.TabSheet;
    FindSucc := tab.FindNext;
  end
  else
    if tabContent.TabVisible then begin
      pagesResponse.ActivePage := tabContent;
      fp := FindInText(responseRaw.Text, dlgFind.FindText, dlgFind.Options, FFindTextPos);
      if (fp.Pos = -1) and (FFindTextPos = 0) then
        FindSucc := -1 // Not found at all.
      else
        FindSucc := fp.Pos + 1;
      if fp.Pos > 0 then begin
        responseRaw.SelStart  := fp.SelStart;
        responseRaw.SelLength := fp.SelLength;
        responseRaw.SetFocus;
        if frDown in dlgFind.Options then
          FFindTextPos := fp.Pos + responseRaw.SelLength
        else
          FFindTextPos := fp.Pos - responseRaw.SelLength;
      end;
    end;

  case FindSucc of
    // Not found at all.
    -1: begin
      miFindNext.Enabled := False;
      Application.MessageBox(PChar('Search string "' + dlgFind.FindText + '" not found.'), 'Not found', MB_ICONERROR + MB_OK);
    end;
    // Not found but previous search was successful.
    0: begin
      Ans := Application.MessageBox(PChar('Search string "' + dlgFind.FindText + '" not found.'#13'Continue search from the beginning ?'), 'Not found', MB_ICONQUESTION + MB_YESNO);
      if Ans = ID_YES then
        FindStart
      else
        miFindNext.Enabled := False;
    end;
  end;
end;

procedure TForm1.cbBasicShowPasswordClick(Sender: TObject);
begin
  if cbBasicShowPassword.Checked then
    editBasicPassword.EchoMode := emNormal
  else
    editBasicPassword.EchoMode := emPassword;
end;

procedure TForm1.cbUrlChange(Sender: TObject);
begin
  SyncURLQueryParams;
end;

procedure TForm1.cbUrlKeyPress(Sender: TObject; var Key: char);
begin
  if key = #13 then btnSubmitClick(Sender);
end;

procedure TForm1.dlgFindFind(Sender: TObject);
begin
  dlgFind.CloseDialog;
  FindStart;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  C: string;
begin
  inherited;

  // Init app configuration.
  C := GetAppConfigFile(False, True);
  PSMAIN.JSONFileName := C;
  C := ExtractFilePath(C);
  if not ForceDirectories(C) then
    ShowMessage(Format('Cannot create directory "%s"', [C]));
  PSMAIN.Active := True;

  // Form components defaults.
  StatusTextTime.Caption := '';
  StatusTextSize.Caption := '';
  StatusTextInfo.Caption := '';
  miSaveResponse.Enabled := False;

  HeadersEditorForm := THeadersEditorForm.Create(Application);

  // Initialize and register response tabs.
  FResponseTabManager := TResponseTabManager.Create(pagesResponse);
  FResponseTabManager.RegisterTab(TResponseImageTab.Create);
  FResponseJsonTab := TResponseJsonTab.Create;
  FResponseJsonTab.OnJsonFormat := @JsonTab_OnJsonFormat;
  FResponseTabManager.RegisterTab(FResponseJsonTab);
  FResponseTabManager.OnOpenResponseTab := @OnOpenResponseTab;
  FResponseTabManager.OnSaveTab := @OnSaveResponseTab;

  UpdateHeadersPickList;

  gridForm.Cells[0, 1] := '1';
  gridReqCookie.Cells[0, 1] := '1';
  gridParams.Cells[0, 1] := '1';

  // Init cookie form.
  CookieForm := TCookieForm.Create(Application);
  CookieForm.ResponseGrid := gridRespCookie;
  CookieForm.RequestGrid := gridReqCookie;

  KeyValueForm := TKeyValueForm.Create(Application);

  SelectBodyTab(btForm);
  SelectAuthTab(atNone);
  pagesRequest.ActivePage := tabHeaders;

  StartNewRequest;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FResponseTabManager);

  // Если не освободить, то когда активна вкладка JSON при выходе
  // из приложения возникают исключения что память не освобождена.
  jsImages.Free;

  // In some rare situations Terminate leads to access violation error.
  {if Assigned(FHttpClient) then begin
    FHttpClient.Terminate;
  end;}
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
  Grid: TStringGrid;
begin
  Grid := GetPopupSenderAsStringGrid(Sender);
  DoGridOperation(Grid, goClear);
end;

procedure TForm1.gaEditRowClick(Sender: TObject);
var
  Grid: TStringGrid;
begin
  Grid := GetPopupSenderAsStringGrid(Sender);
  DoGridOperation(Grid, goEdit);
end;

procedure TForm1.gaInsertRowClick(Sender: TObject);
var
  Grid: TStringGrid;
begin
  Grid := GetPopupSenderAsStringGrid(Sender);
  DoGridOperation(Grid, goNew);
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
      if grid.RowCount > 1 then
        KeyValueForm.View(GetRowKV(grid), 'View: ' + grid.Cells[0, grid.Row])
    end
    else
      EditGridRow(grid);
  end;
end;

procedure TForm1.gridFormSelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
  if aCol = 3 then
    if Editor is TCustomComboBox then
      with Editor as TCustomComboBox do begin
        Style := csDropDownList;
        Items.CommaText := 'Text,File';
      end;
end;

procedure TForm1.gridParamsCheckboxToggled(sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
var
  Params: TQueryParams;
  KV: TKeyValue;
  I: Integer;
begin
  Params:=nil;
  try
    KV:=GetRowKV(gridParams, aRow);
    if KV.Key = '' then Exit;
    Params := GetURLQueryParams(cbUrl.Text);
    I := Params.IndexOf(KV.Key);
    // Add param to the url.
    if (IsRowEnabled(gridParams, aRow)) and (I = -1) then
      Params.AddOrSetData(KV.Key, KV.Value);
    // Remove param from the url
    if (not IsRowEnabled(gridParams, aRow)) and (I >= 0) then
      Params.Remove(KV.Key);
    // Construct a new url.
    cbUrl.Text:=ReplaceURLQueryParams(cbUrl.Text, Params);
  finally
    FreeAndNil(Params);
  end;
end;

procedure TForm1.gridParamsEditingDone(Sender: TObject);
begin
  SyncGridQueryParams;
end;

procedure TForm1.gridRespCookieDblClick(Sender: TObject);
begin
  CookieForm.View;
end;

procedure TForm1.miFindClick(Sender: TObject);
begin
  if pagesResponse.ActivePage = tabContent then begin
    if responseRaw.SelText <> '' then
      dlgFind.FindText := responseRaw.SelText;
  end;
  dlgFind.Execute;
end;

procedure TForm1.miFindNextClick(Sender: TObject);
begin
  FindText;
end;

procedure TForm1.miHelpCmdClick(Sender: TObject);
begin
  with THelpForm.Create(Self) do begin
    helpText.Font := OptionsForm.GetFontItem(fiHelp);
    ShowModal('Command line help', Trim(Usage));
    Free;
  end;
end;

procedure TForm1.miImportClick(Sender: TObject);
begin
  with TImportForm.Create(Self) do begin
    if ShowModal = mrOK then begin
      if RequestObjects.Count = 0 then
        Application.MessageBox('Data not imported.', 'Error', MB_ICONERROR )
      else
        RequestObjects.Items[0].RequestObject.LoadToForm(Self);
    end;
    Free;
  end;
end;

procedure TForm1.JsonTreeDblClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  if Sender is TTreeView then begin
    Node := TTreeView(Sender).Selected;
    if not Assigned(Node) then
      Exit;
    // Double click on parent nodes expand/collapse children.
    // But on child node shows KeyValue modal.
    if not Node.HasChildren then
      JsonTreePopupMenuClick(miJsonView);
  end;
end;

// Various copy operations on Json tree node.
// Popup menu handler for the following operations:
// 1. Copy node value. For root values (object, array) it will copy the whole
//    child tree.
// 2. Copy node key.
// 3. Copy node key + value.
procedure TForm1.JsonTreePopupMenuClick(Sender: TObject);
var
  Node, ParentNode: TTreeNode;
  MenuItem: TMenuItem;
  JsonData, ParentData: TJSONData;
  I: Integer;
  Value, Key: String;
begin
  Node := FResponseJsonTab.TreeView.Selected;
  if not Assigned(Node) then Exit;

  // Get json node value.
  JsonData := TJSONData(Node.Data);
  case JsonData.JSONType of
    jtNumber, jtString, jtBoolean: Value := JsonData.AsString;
    jtArray, jtObject:             Value := FormatJson(JsonData);
    jtNull:                        Value := '';
  end;

  MenuItem := Sender as TMenuItem;
  if MenuItem = miJsonCopyValue then
  begin
    Clipboard.AsText := Value;
    Exit;
  end;

  // Get json node key.
  Key := '';
  ParentNode := Node.Parent;
  if not Assigned(ParentNode) then Exit;
  ParentData := TJSONData(ParentNode.Data);
  case ParentData.JSONType of
    jtObject:
      begin
        I := TJSONObject(ParentData).IndexOf(JsonData);
        if I >= 0 then
          Key := TJSONObject(ParentData).Names[I];
      end;
    jtArray:
      begin
        I := TJSONArray(ParentData).IndexOf(JsonData);
        if I >= 0 then
          Key := IntToStr(I);
      end;
  end;

  if MenuItem = miJsonCopyKey then
    Clipboard.AsText := Key
  else if MenuItem = miJsonCopyValueKey then
    Clipboard.AsText := IfThen(Key = '', Value, Format('"%s": %s', [Key, FormatJson(JsonData)]));

  if MenuItem = miJsonView then
    KeyValueForm.View(Key, Value, Key);

  if MenuItem = miJsonFilter then
    FResponseJsonTab.Filter(Node);
end;

procedure TForm1.miManageHeadersClick(Sender: TObject);
var
  i, aRow, p1, p2: integer;
  gridHeaders: TStringGrid;
  hname, hvalue: string;
begin
  gridHeaders := HeadersEditorForm.gridHeaders;
  // Button "Insert" pressed.
  if (HeadersEditorForm.ShowModal = mrOK) and
     (gridHeaders.SelectedRangeCount > 0) then
  begin
    for i := 0 to gridHeaders.SelectedRangeCount - 1 do
    begin
      aRow := gridHeaders.SelectedRange[i].Top;
      hname := gridHeaders.Cells[0, aRow];
      hvalue := gridHeaders.Cells[1, aRow];
      with requestHeaders do begin
        p1 := Cols[1].IndexOf(hname);
        p2 := Cols[2].IndexOf(hvalue);
        // A new header-value pair.
        if (p1 = -1) then begin
          RowCount := RowCount + 1;
          Row := RowCount - 1;
          Cells[0, Row] := '1';
          Cells[1, Row] := hname;
          Cells[2, Row] := hvalue;
        end;
        // Update a header value.
        if (p1 <> -1) and (p1 <> p2) then begin
          Cells[2, p1] := hvalue;
        end;
      end;
    end;
  end;
  UpdateHeadersPickList;
end;

procedure TForm1.miNewClick(Sender: TObject);
begin
  if PromptNewRequest('Are you sure you want to start a new request ?') then
    StartNewRequest;
end;

procedure TForm1.miNewWindowClick(Sender: TObject);
begin
  AppExec(Application.ExeName, ['--new']);
end;

procedure TForm1.miOpenRequestClick(Sender: TObject);
var
  jsonStr: string;
begin
  if not PromptNewRequest('Do you want to open a request file ?', 'Open request file') then Exit;

  dlgOpen.DefaultExt := 'json';
  if dlgOpen.Execute then begin

    if not FileGetContents(dlgOpen.FileName, jsonStr) then begin
      ShowMessage('Cannot read file ' + dlgOpen.FileName);
      Exit;
    end;

    OpenRequestFile(jsonStr);
  end;
end;

procedure TForm1.miOptionsClick(Sender: TObject);
begin
  if OptionsForm.ShowModal = mrOK then ApplyOptions;
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
  Grid: TStringGrid;
begin
  Grid := GetPopupSenderAsStringGrid(Sender);
  DoGridOperation(Grid, goDelete);
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
    obj.Body := editOther.Text;
    obj.Json := editJson.Text;

    obj.SetCollectionFromGrid(requestHeaders, obj.Headers);
    obj.SetCollectionFromGrid(gridReqCookie, obj.Cookies);
    obj.SetCollectionFromGrid(gridParams, obj.Params);
    obj.GetFormFromGrid(gridForm);

    obj.AuthType := GetSelectedAuthTab;
    obj.AuthBasic.Login    := editBasicLogin.Text;
    obj.AuthBasic.Password := editBasicPassword.Text;
    obj.AuthBearer.Prefix  := editBearerPrefix.Text;
    obj.AuthBearer.Token   := editBearerToken.Text;

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
      if (FResponseTabManager.OpenedTabs.Count = 0) and (tabContent.TabVisible) then
        responseRaw.Lines.SaveToFile(dlgSave.FileName)
      else
        FResponseTabManager.Save(dlgSave.FileName);
    end;
  except on E: Exception do
    ShowMessage(E.Message);
  end;
end;

procedure TForm1.OnGridClear(Sender: TObject; Grid: TStringGrid);
begin
  // Force to update url query params.
  if Grid = gridParams then SyncGridQueryParams;
end;

procedure TForm1.OnGridDeleteRow(Sender: TObject; Grid: TStringGrid);
begin
  // Force to update url query params.
  if Grid = gridParams then SyncGridQueryParams;
end;

procedure TForm1.OnGridEditRow(Sender: TObject; Grid: TStringGrid;
  const aRow: Integer);
begin
  EditGridRow(Grid);
end;

procedure TForm1.OnGridNewRow(Sender: TObject; Grid: TStringGrid;
  const aRow: Integer);
begin
  // New inserted columns with "On" checked by default.
  Grid.Cells[0, aRow] := '1';
  if EditGridRow(Grid) <> mrOK then
    Grid.DeleteRow(aRow);
end;

procedure TForm1.pmAuthTypeClick(Sender: TObject);
var
  mi: TMenuItem;
begin
  mi := Sender as TMenuItem;
  if mi = miAuthNone then SelectAuthTab(atNone)
  else if mi = miAuthBasic then SelectAuthTab(atBasic)
  else if mi = miAuthBearer then SelectAuthTab(atBearer);
end;

procedure TForm1.pmBodyTypeClick(Sender: TObject);
var
  mi: TMenuItem;
begin
  mi := Sender as TMenuItem;
  if mi = miBodyForm then SelectBodyTab(btForm)
  else if mi = miBodyJson then SelectBodyTab(btJson)
  else if mi = miBodyOther then SelectBodyTab(btOther);
end;

// Show/hide some items in Grid's popup menu.
// Depending on grid popup menu can show or hide some menu items for specific
// grid.
procedure TForm1.popupGridActionsPopup(Sender: TObject);
begin
  gaSaveHeader.Visible := False;
  gaManageHeaders.Visible := False;
  if GetPopupSenderAsStringGrid(Sender) = requestHeaders then begin
    gaManageHeaders.Visible := True;
    gaSaveHeader.Visible := True;
  end;
end;

procedure TForm1.PSMAINRestoreProperties(Sender: TObject);
begin
  // Update Query tab and app title.
  SetAppCaption(cbUrl.Text);
  SyncURLQueryParams;
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
  SetColumns(gridParams);
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
  SaveColumns(gridParams);
end;

procedure TForm1.requestHeadersBeforeSelection(Sender: TObject; aCol,
  aRow: Integer);
var
  header: string;
begin
  header := Trim(GetRowKV(requestHeaders, aRow).Key);
  if header <> '' then
    HeadersEditorForm.FillHeaderValues(header, requestHeaders.Columns.Items[2].PickList);
end;

procedure TForm1.tbtnFormUploadClick(Sender: TObject);
var
  Row, A: Integer;
begin
  if gridForm.RowCount = 1 then Exit;
  Row := gridForm.Row;

  if gridForm.Cells[3, Row] <> 'File' then begin
    A := Application.MessageBox('To upload a file the form element must be the file type.'#13'Do you want to change it to the file type ?', 'Upload', MB_ICONQUESTION + MB_YESNO);
    if A = IDYES then
      gridForm.Cells[3, Row] := 'File';
  end;

  if gridForm.Cells[3, Row] = 'File' then begin
    dlgOpen.Title := 'Upload a file';
    if dlgOpen.Execute then
      gridForm.Cells[2, Row] := dlgOpen.FileName;
  end;
end;

procedure TForm1.tbtnManageHeadersClick(Sender: TObject);
begin
  miManageHeadersClick(requestHeaders);
end;

procedure TForm1.tbtnBodyFormatClick(Sender: TObject);
var
  ErrMsg: string;
begin
  if Length(Trim(editJson.Text)) = 0 then
    Exit;
  if not SetJsonBody(editJson.Text, ErrMsg) then
    ShowMessage(ErrMsg);
end;

procedure TForm1.tbtnSaveHeaderClick(Sender: TObject);
var
  KV: TKeyValue;
begin
  KV := GetRowKV(requestHeaders);
  if KV.Key <> '' then
    HeadersEditorForm.Add(KV.Key, KV.Value);
end;

procedure TForm1.TimerRequestTimer(Sender: TObject);
var
  Min, Sec: Integer;
begin
  Inc(FRequestSeconds);
  Min := FRequestSeconds div 60;
  Sec := FRequestSeconds mod 60;
  StatusTextInfo.Caption := Format('%.2d:%.2d', [Min, Sec]);
end;

// Synchronizes query parameters from the url.
procedure TForm1.SyncURLQueryParams;
var
  Params, Keep: TQueryParams;
  I, Idx: Integer;
  KV: TKeyValue;
begin
  Params:=nil;
  try
    Keep:=TQueryParams.Create;
    Params := GetURLQueryParams(cbUrl.Text);
    // Preserve disabled params.
    for I:=1 to gridParams.RowCount-1 do
      if not IsRowEnabled(gridParams, I) then begin
        KV:=GetRowKV(gridParams, I);
        if KV.Key='' then Continue;
        Keep.Add(KV.Key, KV.Value);
      end;
    gridParams.RowCount:=1;
    for I:=0 to Params.Count-1 do
      gridParams.InsertRowWithValues(I+1, ['1', Params.Keys[I], Params.Data[I]]);
    // Return disabled params to the grid.
    for I:=0 to Keep.Count-1 do begin
      Idx:=gridParams.Cols[1].IndexOf(Keep.Keys[I]);
      if Idx = -1 then
        gridParams.InsertRowWithValues(gridParams.RowCount, ['0', Keep.Keys[I], Keep.Data[I]]);
    end;
  finally
    FreeAndNil(Params);
    FreeAndNil(Keep);
  end;
end;

// Synchronizes query parameters from the grid.
procedure TForm1.SyncGridQueryParams;
var
  Params: TQueryParams;
  KV: TKeyValue;
  I: Integer;
begin
  if not IsRowEnabled(gridParams) then Exit;
  Params := TQueryParams.Create;
  try
    for I:=1 to gridParams.RowCount-1 do begin
      if not IsRowEnabled(gridParams, I) then Continue;
      KV:=GetRowKV(gridParams, I);
      if KV.Key = '' then Continue;
      Params.AddOrSetData(KV.Key, KV.Value);
    end;
    cbUrl.Text:=ReplaceURLQueryParams(cbUrl.Text, Params);
  finally
    FreeAndNil(Params);
  end;
end;

function TForm1.IsRowEnabled(const grid: TStringGrid; aRow: Integer): Boolean;
begin
  if aRow = -1 then aRow := grid.Row;
  Result := grid.Cells[0, aRow] = '1';
end;

function TForm1.GetRowKV(const grid: TStringGrid; aRow: Integer): TKeyValue;
var
  Offset: ShortInt;
begin
  if aRow = -1 then aRow := grid.Row;
  // Grids with more two columns should have first column with checkboxes.
  if grid.ColCount = 2 then
    Offset := 0
  else begin
    Offset := 1;
    Result.Enabled := (grid.Cells[0, aRow] = '1');
  end;
  Result.Key:=Trim(grid.Cells[Offset, aRow]); // Key cannot be whitespaced.
  Result.Value:=grid.Cells[Offset+1, aRow];
end;

function TForm1.FormatJson(json: TJSONData): string;
begin
  Result := json.FormatJSON(OptionsForm.JsonFormat, OptionsForm.JsonIndentSize);
end;

procedure TForm1.SelectBodyTab(const tab: tbodytab);
begin
  tbtnFormUpload.Visible  := False;
  tbtnBodyFormat.Visible  := False;
  tbtnBodyType.Visible    := False;
  gnavBody.ShowNavButtons := False;

  case tab of
    btForm:
      begin
        pagesBody.ActivePage := tabBodyForm;
        // This procedure is invoked in Form's Create method when
        // options form is not initalized yet. So, for the first time
        // we always show toolbar.
        if Assigned(OptionsForm) then
          gnavBody.ShowNavButtons := not OptionsForm.GridButtonsHidden
        else
          gnavBody.ShowNavButtons := True;
        tbtnBodyType.Visible := True;
        tbtnFormUpload.Visible := True;
        tbtnBodyType.Caption := 'Form';
      end;
    btJson:
      begin
        pagesBody.ActivePage:=tabBodyJson;
        tbtnBodyType.Visible:=True;
        tbtnBodyType.Caption:='JSON';
        tbtnBodyFormat.Visible:=True;
      end;
    btOther:
      begin
        pagesBody.ActivePage:=tabBodyOther;
        tbtnBodyType.Visible:=True;
        tbtnBodyType.Caption:='Other';
      end;
  end;

  tabBody.Caption := 'Body: ' + tbtnBodyType.Caption;

  // Keep buttons order after some buttons are hidden.
  gnavBody.SetButtonsOrder;
end;

procedure TForm1.SelectAuthTab(const tab: TAuthTab);
begin
  if tab = atNone then
    pagesAuth.Visible := False
  else begin
    pagesAuth.Visible := True;
    pagesAuth.ActivePageIndex := LongInt(tab);
  end;

  case tab of
    atNone:   tbtnAuthType.Caption := 'None';
    atBasic:  tbtnAuthType.Caption := 'Basic';
    atBearer: tbtnAuthType.Caption := 'Bearer token';
  end;

  tabAuth.Caption := 'Auth: ' + tbtnAuthType.Caption;
end;

function TForm1.GetSelectedBodyTab: TBodyTab;
begin
  case pagesBody.ActivePageIndex of
    0: Result:=btJson;
    1: Result:=btForm;
    2: Result:=btOther;
    else raise Exception.Create('No value for active tab.');
  end;
end;

function TForm1.GetSelectedAuthTab: TAuthTab;
begin
  if pagesAuth.Visible then
    case pagesAuth.ActivePageIndex of
      0: Result := atBasic;
      1: Result := atBearer;
      else raise Exception.Create('No value for active tab');
    end
  else
    Result := atNone;
end;

procedure TForm1.DoGridOperation(Grid: TStringGrid; const op: TGridOperation);
var
  toolbar: TGridNavigator;
begin
  if Grid = requestHeaders then
    toolbar := gnavHeaders
  else if Grid = gridParams then
    toolbar := gnavParams
  else if Grid = gridForm then
    toolbar := gnavBody
  else if Grid = gridReqCookie then
    toolbar := gnavCookie
  else
    Exit;

  case op of
    goNew:    toolbar.NewButton.Click;
    goEdit:   toolbar.EditButton.Click;
    goDelete: toolbar.DeleteButton.Click;
    goClear:  toolbar.ClearButton.Click;
  end;
end;

procedure TForm1.OnOpenResponseTab(Tab: TResponseTab;
  ResponseInfo: TResponseInfo);
var
  ImageTab: TResponseImageTab;
begin
  if Tab = FResponseJsonTab then begin
    with FResponseJsonTab do begin
      TreeView.Images := jsImages;
      TreeView.PopupMenu := popupJsonTree;
      TreeView.OnDblClick := @JsonTreeDblClick;
      TreeView.Font := OptionsForm.GetFontItem(fiJson);
      SynEdit.Highlighter := synJS;
      SynEdit.Font := OptionsForm.GetFontItem(fiEditor);
      ButtonOptions.OnClick := @OnJsonTabButtonOptionsClick;
      ViewPage := OptionsForm.JsonView;
      LineNumbers := OptionsForm.JsonLines;
    end;
  end

  else if Tab is TResponseImageTab then begin
    ImageTab := TResponseImageTab(Tab);
    StatusTextInfo.Caption := Format('%s: %d x %d', [
      ImageTab.ImageType,
      ImageTab.Image.Picture.Width,
      ImageTab.Image.Picture.Height
    ]);
  end;
end;

procedure TForm1.OnSaveResponseTab(const FileName: string; Tab: TResponseTab);
begin
  if Tab is TResponseImageTab then begin
    TResponseImageTab(Tab).Save(FileName);
  end

  else if Tab is TResponseJsonTab then begin
    if OptionsForm.JsonSaveFormatted then
      FilePutContents(FileName, FormatJson(TResponseJsonTab(Tab).JsonRoot))
    else
      responseRaw.Lines.SaveToFile(FileName)
  end;
end;

procedure TForm1.OnJsonTabButtonOptionsClick(Sender: TObject);
begin
  if OptionsForm.ShowModalPage(opJson) = mrOK then begin
    ApplyOptions;
    FResponseJsonTab.LineNumbers := OptionsForm.JsonLines;
  end;
end;

procedure TForm1.JsonTab_OnJsonFormat(JsonData: TJSONData; Editor: TSynEdit);
begin
  Editor.Text := FormatJson(JsonData);
end;

procedure TForm1.FindStart(Search: Boolean = True);
var
  tab: TResponseTab;
  IsText: Boolean;
begin
  FFindTextPos := 0;
  IsText := not dlgFind.FindText.Trim.IsEmpty;
  tab := FResponseTabManager.CanFind;
  if (tab <> nil) and (IsText) then
    tab.InitSearch(dlgFind.FindText, dlgFind.Options);
  if IsText then
    miFindNext.Enabled := True;
  if Search then
    FindText;
end;

procedure TForm1.ApplyOptions;
begin
  editJson.TabWidth := OptionsForm.JsonIndentSize;
  FResponseJsonTab.TreeExpanded := OptionsForm.JsonExpanded;

  // Adjust window size before change layout.
  if (LayoutSplitter.SplitterType = pstVertical) and
       (OptionsForm.PanelsLayout = pstHorizontal) and
       (Width < 460) then
    Width := 600;
  if (LayoutSplitter.SplitterType = pstHorizontal) and
       (OptionsForm.PanelsLayout = pstVertical) and
       (Height < 400) then
    Height := 600;
  LayoutSplitter.SplitterType := OptionsForm.PanelsLayout;

  if GetSelectedBodyTab = btForm then
    gnavBody.ShowNavButtons := not OptionsForm.GridButtonsHidden;
  gnavBody.SetButtonsOrder;
  gnavHeaders.Visible := not OptionsForm.GridButtonsHidden;
  // Ugly hack to hide toolbar.
  // Когда тулбар скрыт при запуске приложения он всё равно отображается как
  // пустая панель. (When toolbar is hidden it always shows like empty panel)
  if OptionsForm.GridButtonsHidden then
    gnavHeaders.Height := 0
  else
    gnavHeaders.Height := gnavBody.Height;
  gnavParams.Visible := not OptionsForm.GridButtonsHidden;
  gnavCookie.Visible := not OptionsForm.GridButtonsHidden;

  // Apply fonts
  OptionsForm.ApplyControlFont(Self, 'TStringGrid', fiGrids);
  OptionsForm.ApplyControlFont(Self, 'TSynEdit', fiEditor);
  OptionsForm.ApplyControlFont(Self, 'TTreeView', fiJson);
  responseRaw.Font := OptionsForm.GetFontItem(fiContent);
  KeyValueForm.textValue.Font := OptionsForm.GetFontItem(fiValue);

  cbMethod.ReadOnly := not OptionsForm.EditRequestMethods;
end;

function TForm1.SetRowKV(AGrid: TStringGrid; KV: TKeyValuePair;
  aRow: Integer; isUnique: Boolean): Integer;
var
  Start: SmallInt;
begin
  Start := 0;
  with AGrid do begin
    if Columns[0].ButtonStyle <> cbsAuto then
      Start := 1;
    if isUnique and (Cols[Start].IndexOf(KV.Key) > 0) then
      Exit;
    if aRow = -1 then begin
      RowCount := RowCount + 1;
      ARow := RowCount - 1;
    end;
    if Columns[0].ButtonStyle = cbsCheckboxColumn then
      Cells[0, ARow] := '1';
    Cells[Start, ARow] := KV.Key;
    Cells[Start + 1, ARow] := KV.Value;
  end;
  Result := ARow;
end;

// Add values to request grid.
// Adjust header name from the predefined name from the headers editor form.
procedure TForm1.AddRequestHeader(AHeader, AValue: string);
var
  headers: TStringList;
  iter: string;
  kv: TKeyValuePair;
begin
  headers := TStringList.Create;
  try
    HeadersEditorForm.FillHeaders(headers);
    for iter in headers do
      if LowerCase(iter) = LowerCase(AHeader) then begin
        AHeader := iter;
        break;
      end;
    kv.Key := AHeader;
    kv.Value := AValue;
    SetRowKV(requestHeaders, kv);
  finally
    headers.Free;
  end;
end;

procedure TForm1.AddFormData(AName, AValue: string; isFile: Boolean);
var
  isUnique: Boolean = False;
  KV: TKeyValuePair;
  Added: Integer;
begin
  if AnsiContainsStr(AValue, '[]') then
    isUnique := False;
  KV.Key := AName;
  KV.Value := AValue;
  Added := SetRowKV(gridForm, KV, -1, isUnique);
  if isFile then
    gridForm.Cells[3, Added] := 'File';
end;

procedure TForm1.OpenRequestFile(jsonStr: string);
var
  streamer: TJSONDeStreamer;
  obj: TRequestObject;
begin
  streamer := TJSONDeStreamer.Create(nil);
  obj := TRequestObject.Create;

  try
    streamer.JSONToObject(jsonStr, obj);
    StartNewRequest;
    obj.LoadToForm(Self);
  except on E: Exception do
      ShowMessage(E.Message);
  end;

  streamer.Free;
  obj.Free;
end;

procedure TForm1.OnHttpException(Url, Method: string; E: Exception);
begin
  TimerRequest.Enabled := False;
  UpdateStatusLine;
  StatusTextInfo.Caption := '';
  // Is this a timeout exception ?
  if OptionsForm.Timeout = FRequestSeconds then
    ShowMessage('Request is timeout.')
  else
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

function TForm1.ParseHeaderLine(line: string; delim: char = ':'; all: Boolean = False): TKeyValuePair;
var
  p: integer;
begin
  Result := SplitKV(line, delim);
  p := Pos(';', Result.Value);
  if (not all) and (p <> 0) then Result.Value := Trim(LeftStr(Result.Value, p - 1));
end;

procedure TForm1.UpdateHeadersPickList;
begin
  with requestHeaders do begin
    HeadersEditorForm.FillHeaders(Columns.Items[1].PickList);
    if RowCount > 1 then
      Cells[0, 1] := '1';
  end;
end;

function TForm1.EncodeFormData: string;
var
  i: integer;
  KV: TKeyValue;
begin
  Result := '';
  for i := 1 to gridForm.RowCount - 1 do
  begin
    if not IsRowEnabled(gridForm, i) then continue;
    KV := GetRowKV(gridForm, i);
    if KV.Key = '' then continue; // Skip empty names
    if Result <> '' then Result := Result + '&';
    Result := Result + EncodeURLElement(KV.Key) + '=' + EncodeURLElement(KV.Value);
  end;
end;

procedure TForm1.OnRequestComplete(Info: TResponseInfo);
var
  i, p: integer;
  h: string;
  mime: TMimeType;
begin
  btnSubmit.Enabled := True;
  TimerRequest.Enabled := False;
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

  UpdateStatusLine(Info);


  if (Info.StatusCode <> 404) then
  begin
    p := cbUrl.Items.IndexOf(Info.Url);
    if p = -1 then begin // Add a new url.
      if cbUrl.Items.Count >= MAX_URLS then
        cbUrl.Items.Delete(cbUrl.Items.Count - 1);
      cbUrl.Items.Insert(0, Info.Url);
    end
    else // Move an existing url to the top of the list.
      cbUrl.Items.Move(p, 0);
    cbUrl.Text := Info.Url;
  end;

  // Fill response cookie grid or hide it.
  ShowResponseCookie(Info.ResponseHeaders);

  // Get the response content - enable menu item.
  miSaveResponse.Enabled := True;

  Info.Content.Position := 0;
  StatusTextInfo.Caption := '';
  responseRaw.Clear;
  mime := SplitMimeType(Info.ContentType);

  FResponseTabManager.OpenTabs(Info);

  if (mime.MimeType = 'text') or ((mime.MimeType = 'application') and (mime.Subtype <> 'octet-stream')) then
    tabContent.TabVisible := True
  else
    tabContent.TabVisible := False;

  // Find always enabled when Content tab is visible.
  miFind.Enabled := tabContent.TabVisible;
  if not miFind.Enabled then
    miFindNext.Enabled := False;

  // Reset search: Find Next will search from start or from the end data with
  // the parameters from the previous search (if it was happen).
  FindStart(False);

  if tabContent.TabVisible then begin
    with responseRaw.Lines do begin
      BeginUpdate;
      try
        Add(Info.Content.DataString);
      finally
        EndUpdate;
      end;
    end;
    responseRaw.CaretPos := Point(0, 0);
  end;
end;

procedure TForm1.UpdateStatusLine(Main: string);
begin
  StatusTextMain.Caption  := Main;
  StatusTextTime.Caption := '';
  StatusTextSize.Caption := '';
  StatusImageTime.Visible := False;
  StatusImageSize.Visible := False;
end;

procedure TForm1.UpdateStatusLine(Info: TResponseInfo);
var
  w: Integer;
begin
  StatusTextMain.Caption := Format('HTTP/%s %d %s', [Info.HttpVersion, Info.StatusCode, Info.StatusText]);

  StatusImageTime.Visible := True;
  StatusImageSize.Visible := True;

  StatusTextTime.Caption := IfThen(Info.Time > 1000,
      Format('%d ms (%s)', [Info.Time, FormatMsApprox(Info.Time)]),
      Format('%d ms',      [Info.Time]));

  StatusTextSize.Caption := NumberFormat(Info.Content.Size) + ' bytes';

  // Manual place components. Align property in the some cases can
  // lead to exchange of order of the image and text.
  w := StatusTextMain.Left + StatusTextMain.Width;

  // Place components in proper order.
  StatusImageTime.Left  := w + 16;
  StatusTextTime.Left   := StatusImageTime.Left + StatusImageTime.Width + 4;
  StatusTextTime.Height := StatusTextMain.Height;
  StatusImageSize.Left  := StatusTextTime.Left + StatusTextTime.Width + 8;
  StatusTextSize.Left   := StatusImageSize.Left + StatusImageSize.Width + 4;
  StatusTextSize.Height := StatusTextMain.Height;
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
    FreeAndNil(tokens);
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
    ext := RightStr(FContentType, Length(FContentType) - Pos('/', FContentType));
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
  I: Integer;
  Need: Boolean = False;

  function IsEmpty(Grid: TStringGrid): Boolean;
  var
    I: Integer;
  begin
    for I := 1 to Grid.RowCount - 1 do
      if (Grid.Cells[1, I] <> '') or (Grid.Cells[2, I] <> '') then
        Exit(False);
    Result := True;
  end;

begin
  Need := Length(cbUrl.Text) > 0;
  if not Need then
    Need := not IsEmpty(requestHeaders);
  if not Need then
    Need := not IsEmpty(gridReqCookie);
  if not Need then
    Need := not IsEmpty(gridForm);
  if not Need then
    Need := not IsEmpty(gridParams);
  if not Need then
    Need := GetSelectedAuthTab <> atNone;
  if not Need then
    Need := Length(editOther.Text) > 0;
  if not Need then
    Need := Length(Trim(editJson.Text)) > 0;

  if Need then begin
    I := Application.MessageBox(PChar(prompt), PChar(promptTitle), MB_ICONQUESTION + MB_YESNO);
    if I <> IDYES then Exit(False); // =>
  end;
  Result := True;
end;

procedure TForm1.StartNewRequest;
  procedure ResetGrid(grid: TStringGrid);
  begin
    grid.RowCount := 2;
    grid.Cells[0, 1] := '1';
    grid.Cells[1, 1] := '';
    grid.Cells[2, 1] := '';
  end;

begin
  // Request fields.
  cbUrl.Text := '';
  cbMethod.Text := 'GET';
  editOther.Text := '';
  editJson.Text := '';
  ResetGrid(requestHeaders);
  ResetGrid(gridForm);
  ResetGrid(gridReqCookie);
  ResetGrid(gridParams);
  SelectBodyTab(btForm);

  // Response fields.
  responseHeaders.RowCount := 1;
  responseRaw.Text := '';
  FContentType := '';
  tabContent.TabVisible := False;
  tabRespCookie.TabVisible := False;
  pagesResponse.ActivePage := tabResponse;

  // Menu items.
  miSaveResponse.Enabled := False;
  miFind.Enabled := False;
  miFindNext.Enabled := False;

  // Reset auth
  editBasicLogin.Text := '';
  editBasicPassword.Text := '';
  cbBasicShowPassword.Checked := False;
  editBearerPrefix.Text := 'Bearer';
  editBearerToken.Text := '';
  SelectAuthTab(atNone);

  FResponseTabManager.CloseTabs;
  SetAppCaption;
end;

function TForm1.SetJsonBody(jsonStr: string; var ErrMsg: string): Boolean;
var
  ss: TStringStream;
  parser: TJSONParser;
  data: TJSONData;
begin
  data := nil;
  Result := True;
  try
    ss := TStringStream.Create(jsonStr);
    Parser := TJSONParser.Create(ss);
    try
      Data := Parser.Parse;
      editJson.Text := FormatJson(Data);
    except
      on E: Exception do begin
        Result := False;
        ErrMsg := E.Message;
      end;
    end;
  finally
    FreeAndNil(ss);
    FreeAndNil(parser);
    FreeAndNil(data);
  end;
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
  else if Component is TStringCellEditor then begin
    Component := TStringCellEditor(Component).Parent;
    if Component is TCompositeCellEditor then
      Result := TCompositeCellEditor(Component).Parent as TStringGrid
    else
      Result := Component as TStringGrid;
  end
  else if Component is TPickListCellEditor then begin
    Result := TPickListCellEditor(Component).Parent as TStringGrid;
  end
  else
    Result := nil;
end;

function TForm1.EditGridRow(Grid: TStringGrid;
  const ValueFocused: Boolean): TModalResult;
var
  kv: TKeyValue;
  focus: Integer = FocusKey;
begin
  if ValueFocused then
    focus := FocusVal;
  with Grid do begin
    kv := KeyValueForm.Edit(GetRowKV(Grid), 'Edit...', focus);
    Result := KeyValueForm.ModalResult;
    if Result = mrOK then begin
      Cells[0, Row] := IfThen(kv.Enabled, '1', '0');
      Cells[1, Row] := kv.Key;
      Cells[2, Row] := kv.Value;
      // Force to update url after editing query params.
      if Grid = gridParams then
        SyncGridQueryParams;
    end;
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

end.

