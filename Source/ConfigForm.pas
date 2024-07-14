{$WARN UNIT_PLATFORM OFF}

unit ConfigForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, System.Contnrs,
  StdCtrls, ExtCtrls, ComCtrls, Spin, ActnList, Actions, Menus, CommDrv, FileCtrl,
  DeMetadata, DSMeta, ListFormUnit, uMapUtils, uMapLayers;

type

  TComboCoxHelper = class helper for TCombobox
  private
    procedure SetMappedValue(const ID: integer);
    function  GetMappedValue: Integer;
  public
    property MappedID: Integer read GetMappedValue write SetMappedValue;
    procedure MappedLoad(const aTableName: string);
  end;

  TForm_Da_Parameters = class(TForm)
    Btn_Da_Apply: TButton;
    Btn_Da_Cancel: TButton;
    Btn_Ok: TButton;
    PageControl: TPageControl;
    tab_Dl_Device: TTabSheet;
    L_Dl_Port: TLabel;
    L_Dl_PortSpeed: TLabel;
    L_Dl_PortParity: TLabel;
    L_Dl_PortStop: TLabel;
    L_Dl_PortStopByte: TLabel;
    L_Dl_PortBits: TLabel;
    L_Dl_PortTimeout: TLabel;
    Box_dL_uSeBarcode: TCheckBox;
    PortBox: TComboBox;
    Btn_Da_Find: TButton;
    BtnStartStop: TButton;
    SpeedBox: TComboBox;
    ParityBox: TComboBox;
    EndBox: TComboBox;
    TimeEdit: TSpinEdit;
    BitEdit: TSpinEdit;
    StopEdit: TSpinEdit;
    Btn_Df_Default: TButton;
    Panel1: TPanel;
    Im: TImage;
    TypeCodeBox: TComboBox;
    AEdit: TEdit;
    BEdit: TEdit;
    Btn_Dl_Random: TButton;
    Lb_Dl_Check: TLabel;
    Bevel3: TBevel;
    Lb_dF_barcode: TLabel;
    Tab_Dl_Other: TTabSheet;
    Box_Dl_Totray: TCheckBox;
    L_Dl_OutputFiles: TLabel;
    Edit2: TEdit;
    Btn2_Da_Browse: TButton;
    Tab_Dl_Configuration: TTabSheet;
    bvlMetadata: TBevel;
    lb2_Dt_Database: TLabel;
    lbl_Df_Type: TLabel;
    cbxDBType: TComboBox;
    lbl_Df_Login: TLabel;
    edLogin: TEdit;
    lbl_Df_Connection: TLabel;
    lbl_Df_Password: TLabel;
    edPassword: TEdit;
    btn_Da_Browse: TButton;
    edName: TEdit;
    Lb_Df_Name: TLabel;
    lbSolutions: TListBox;
    btnDaAdd: TButton;
    btnDaDelete: TButton;
    Bevel4: TBevel;
    Lb_Dl_Configuration: TLabel;
    edConnectString: TEdit;
    menuConfig: TPopupMenu;
    miAdd: TMenuItem;
    miCopy: TMenuItem;
    miSeparator: TMenuItem;
    miDelete: TMenuItem;
    alConfig: TActionList;
    action_Da_Add: TAction;
    action_Da_Copy: TAction;
    action_Da_Delete: TAction;
    lb_Df_Server: TLabel;
    lb_Dt_Database: TLabel;
    edServer: TEdit;
    edDatabase: TEdit;
    Tab_Dl_Interface: TTabSheet;
    bvlInterface: TBevel;
    Lb_Da_ViewList: TLabel;
    LB_Dl_Animation: TLabel;
    Lb_dl_slower: TLabel;
    tbAnimation: TTrackBar;
    Lb_dl_faster: TLabel;
    Lb_Dl_Other: TLabel;
    Bevel8: TBevel;
    Cb_Dl_GridLines: TCheckBox;
    Box_Dl_RememberStartup: TCheckBox;
    Box_Dl_SoundOn: TCheckBox;
    eSoundFilePath: TEdit;
    Btn3_Da_Browse: TButton;
    lb_Da_Remember: TLabel;
    btnDaCopy: TButton;
    Bevel5: TBevel;
    Lb2_Dl_Other: TLabel;
    Box_Da_StatusBar: TCheckBox;
    Box_Da_ShowHint: TCheckBox;
    Box_Da_AutoSaveChanges: TCheckBox;
    tab_dF_Value: TTabSheet;
    action_Da_Test: TAction;
    btnDaTestconnect: TButton;
    Box_Dl_DragDrop: TCheckBox;
    action_Da_CreateShortcut: TAction;
    btnDaShortcut: TButton;
    Bevel13: TBevel;
    Bevel14: TBevel;
    Box_Da_StandardMenu: TCheckBox;
    Tab_Dl_Logging: TTabSheet;
    Box_dL_EnableLog: TCheckBox;
    Btn1_Da_Browse: TButton;
    Box_dL_FullDateTimeLog: TCheckBox;
    btn_Da_Clear: TButton;
    la_Dl_Logging: TLabel;
    Bevel15: TBevel;
    Box_dL_EnableQueryLog: TCheckBox;
    Box_dL_MaxSizeLog: TCheckBox;
    seMaxSizeLog: TSpinEdit;
    Box_Dl_ImageOldStoreMode: TCheckBox;
    Tab_Dl_Map: TTabSheet;
    la_Dl_Map: TLabel;
    Bevel16: TBevel;
    edMapBaseDirectory: TEdit;
    lbMapLayers: TListBox;
    edMapTemlplateURL: TEdit;
    Lm_Dl_Properties: TLabel;
    Bevel17: TBevel;
    Lm_Df_Template: TLabel;
    Lm_Df_Name: TLabel;
    edMapLayerName: TEdit;
    Bnm_Da_Default: TButton;
    Box_Dl_MapTileCacheEnabled: TCheckBox;
    Btn4_Da_Browse: TButton;
    btm_Da_Add: TButton;
    btm_Da_Copy: TButton;
    acm_Da_Add: TAction;
    acm_Da_Copy: TAction;
    btm_Da_Delete: TButton;
    acm_Da_Delete: TAction;
    LbWeb: TLabel;
    Bevel9: TBevel;
    box_dL_LockSession: TCheckBox;
    LockSpin: TSpinEdit;
    Bevel11: TBevel;
    Lb2_Df_User: TLabel;
    edDefaultUser: TEdit;
    edDefaultPassword: TEdit;
    Lb2_Df_Password: TLabel;
    cbDatabaseDriver: TComboBox;
    la_Dl_DatabaseDriver: TLabel;
    box_Dl_CheckUpdates: TCheckBox;
    box_dL_HTTPAccess: TCheckBox;
    HTTPSpin: TSpinEdit;
    Lb_dV_clipboard: TLabel;
    Bevel7: TBevel;
    Box_Dl_CopyHeaderField: TCheckBox;
    Box_Dl_CopyAllField: TCheckBox;
    Bevel6: TBevel;
    Bevel12: TBevel;
    Label1: TLabel;
    Bevel18: TBevel;
    lb3_Dt_Database: TLabel;
    lbl_Df_Person: TLabel;
    pmMapLayers: TPopupMenu;
    Lm_dL_brightness: TLabel;
    tbBrightness: TTrackBar;
    lb_da_agrmax: TLabel;
    lb_da_agrmin: TLabel;
    Cb_Dl_GridBrush: TCheckBox;
    cbxDBCodePage: TComboBox;
    lb_Dt_Charsets: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Btn_Dl_RandomClick(Sender: TObject);
    procedure AEditChange(Sender: TObject);
    procedure Btn_Df_DefaultClick(Sender: TObject);
    procedure Btn_Da_FindClick(Sender: TObject);
    procedure Change(Sender: TObject);
    procedure TypeCodeBoxChange(Sender: TObject);
    procedure BtnStartStopClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure btn_Da_BrowseClick(Sender: TObject);
    procedure Btn_Da_ApplyClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Btn2_Da_BrowseClick(Sender: TObject);
    procedure Btn_OkClick(Sender: TObject);
    procedure CommPortReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tab_Dl_DeviceShow(Sender: TObject);
    procedure lbSolutionsClick(Sender: TObject);
    procedure btnDaAddClick(Sender: TObject);
    procedure btnDaDeleteClick(Sender: TObject);
    procedure SolutionDataChange(Sender : TObject);
    procedure action_Da_CopyExecute(Sender: TObject);
    procedure action_Da_TestExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Box_Dl_SoundOnClick(Sender: TObject);
    procedure Btn3_Da_BrowseClick(Sender: TObject);
    procedure btn_Da_LoadFromClick(Sender: TObject);
    procedure btn_Da_LoadToClick(Sender: TObject);
    procedure btn2_Da_DeleteClick(Sender: TObject);
    procedure Btn1_Da_BrowseClick(Sender: TObject);
    procedure action_Da_CreateShortcutExecute(Sender: TObject);
    procedure btn_Da_ClearClick(Sender: TObject);
    procedure lbMapLayersClick(Sender: TObject);
    procedure Bnm_Da_DefaultClick(Sender: TObject);
    procedure lbMapLayersDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure Btn4_Da_BrowseClick(Sender: TObject);
    procedure acm_Da_AddExecute(Sender: TObject);
    procedure acm_Da_CopyExecute(Sender: TObject);
    procedure acm_Da_DeleteExecute(Sender: TObject);
    procedure edMapLayerNameChange(Sender: TObject);
    procedure edMapTemlplateURLChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FStarted        : boolean;
    FSolutionsForm  : TListForm;
    FParametersForm : TListForm;
    FSolutionsDSM   : TDataSetMeta;
    FOldPortEvent   : TPortReceiveDataEvent;
    FMapLayers: TMapLayers;
    FDefaultLayers: TMapLayers;
    procedure RefreshTables;
    procedure ApplyPortSettings;
    function ApplySettings : boolean;
    procedure SetStartStopCaption;
    procedure SetStarted(const aStarted : boolean);
    procedure CheckSolutionControls;
    function SettingsValid : boolean;
    procedure ExpandName(aItem : TDeConfigItem);
    procedure AppendFoldersSheet;
    procedure FolderBrowseClick(Sender: TObject);
    procedure BuildProviderList;
    procedure BuildMapLayerList;
    procedure ExpandLayerName(MapLayer: TMapLayer);
    procedure BuildPopupLayerList;
    procedure DefaultMapLayerClick(Sender: TObject);
  public
    { Public declarations }
    property Started : boolean read FStarted write SetStarted;
  end;

const
  smNotShowWithoutLogin     = $0001;
  smNotShowInDeveloperMode  = $0002;
  smNotShowInUserMode       = $0004;

implementation

uses Dialogs, ShellAPI, Buttons, System.IOUtils, ComObj, ActiveX, ShlObj, Registry, Math,
  DeLog, DeTypes, Funcs, codescan, Dictionary, DeDB, Security, DeMeta, Main, DataUnit, QueryDescriptor,
  DataManager, HintForm, DeSettings, DeActions, DeControls, ConnectOperationsUnit;

{$R *.dfm}

const MinAnimation       = 1;
      MaxAnimation       = 30;

      ConnectionsSection = 'Connections';
      ConnectionsCount   = 'Count';
      ConnectionItem     = 'Item';
      HistoryDepth       = 8;

procedure TForm_Da_Parameters.FormCreate(Sender: TObject);
var I, AnimPosition : Integer;
  LogFullDateTime: Boolean;
  LogMaxSize: Int64;
begin
  { определяем необходимость показа закладок }
  for I := 0 to PageControl.PageCount-1 do
    PageControl.Pages[I].TabVisible := PageControl.Pages[I].TabVisible and
      not (
      (
      (UserSession <> nil) and
      (((UserSession.IsAdmin) and (PageControl.Pages[I].Tag and smNotShowInDeveloperMode <> 0)) or
      ((Not UserSession.IsAdmin) and (PageControl.Pages[I].Tag and smNotShowInUserMode <> 0)))
      )
      or
      (CurrentConfig = nil) and
      (PageControl.Pages[I].Tag and smNotShowWithoutLogin <> 0));

  { установка прочих параметров }
  cbxDBType.MappedLoad(tblDDatabaseType);
  cbxDBCodePage.MappedLoad(tblDCharSets);

  SetStartStopCaption;
  FOldPortEvent := DM.CommPort.OnReceiveData;
  DM.CommPort.OnReceiveData:=CommPortReceiveData;
  Started := DM.CommPort.Connected;
  Btn_Dl_Random.Click;
  for i:=ord(br110) to ord(br115200) do
     SpeedBox.Items.Add(IntToStr(Win32BaudRates[TPortBaudRate(i)]));
  for i:=0 to bc_TypeCodeCount-1 do TypeCodeBox.Items.Add(bc_CodeName[i]);
  TypeCodeBox.ItemIndex:=0;
  tab_Dl_DeviceShow(Self);
  for I := 0 to PortBox.Items.Count-1 do
    if integer(PortBox.Items.Objects[I]) = Variables.AsInteger[RegPort] then
    begin
      PortBox.ItemIndex := I;
      break;
    end;
  BuildProviderList;
  Box_Dl_RememberStartup.Checked := Variables.AsBoolean[RegRemStartup];
  eSoundFilePath.Text := Variables.AsString[RegRemSoundPath];
  Box_Dl_SoundOn.Checked := Variables.AsBoolean[RegSoundOn];
  eSoundFilePath.Enabled := Box_Dl_SoundOn.Checked;
  Btn3_Da_Browse.Enabled := Box_Dl_SoundOn.Checked;

  SpeedBox.ItemIndex         := Variables.AsInteger[RegSpeed];
  ParityBox.ItemIndex        := Variables.AsInteger[RegOdd];
  EndBox.ItemIndex           := Variables.AsInteger[RegEndBits];
  TimeEdit.Value             := Variables.AsInteger[RegTime];
  BitEdit.Value              := Variables.AsInteger[RegBit];
  StopEdit.Value             := Variables.AsInteger[RegStop];
  Box_Dl_UseBarcode.Checked  := Variables.AsBoolean[RegStart];
  Box_Dl_ToTray.Checked      := Variables.AsBoolean[RegTray];
  {$IFDEF DBCO_HTTP}
  Box_dL_HTTPAccess.Checked  := Variables.AsBoolean[RegHTTP];
  HTTPSpin.Value             := Variables.AsInteger[RegPortHTTP];
  {$ENDIF}
  box_dL_LockSession.Checked := Variables.AsBoolean[RegLockSession];
  Box_Dl_CheckUpdates.Checked  := Variables.AsBoolean[RegCheckUpdates];
  Box_Dl_CopyAllField.Checked  := Variables.AsBoolean[RegCopyAllField];
  Box_Dl_CopyHeaderField.Checked  := Variables.AsBoolean[RegCopyHeaderField];
  Box_Dl_DragDrop.Checked := Variables.AsBoolean[RegDragDrop];
  Box_dL_EnableLog.Checked     := Variables.AsBoolean[RegLogFile];
  Box_dL_EnableQueryLog.Checked := Variables.AsBoolean[RegLogQuery];
  Box_Da_ShowHint.Checked      := Variables.AsBoolean[RegShowHint];
  Box_Da_StatusBar.Checked := Variables.AsBoolean[RegShowStatus];
  Box_Da_StandardMenu.Checked := Variables.AsBoolean[RegMenuS];
  Box_Da_AutoSaveChanges.Checked := Variables.AsBoolean[RegAutoSaveChanges];
  Box_Dl_ImageOldStoreMode.Checked := Variables.AsInteger[RegImageStoreMode] = 0;

  LockSpin.Value             := Variables.AsInteger[RegLockTime];
  AnimPosition := (MaxAnimation - Variables.AsInteger[RegAnim] + 1) * (tbAnimation.Max - tbAnimation.Min) div
    (MaxAnimation - MinAnimation + 1);
  if AnimPosition < tbAnimation.Min then
    tbAnimation.Position := tbAnimation.Min
  else if AnimPosition > tbAnimation.Max then
    tbAnimation.Position := tbAnimation.Max
  else
    tbAnimation.Position := AnimPosition;

  Edit2.Text:=Variables.AsString[RegDirPath];
  cb_Dl_GridBrush.Checked := Variables.AsBoolean[RegGridBrush];
  cb_Dl_GridLines.Checked := Variables.AsBoolean[RegGridLines];

  { заполнение списка решений }
  for I := 0 to ConfigList.Count-1 do
    lbSolutions.Items.Add(ConfigList[I].Name);
  if lbSolutions.Items.Count > 0 then
  begin
    lbSolutions.ItemIndex := 0;
    lbSolutionsClick(Self);
  end;
  action_Da_Delete.Enabled := (lbSolutions.Items.Count > 0) and not ConfigList.ReadOnly;
  action_Da_Copy.Enabled   := action_Da_Delete.Enabled;
  action_Da_Test.Enabled   := lbSolutions.Items.Count > 0;
  action_Da_CreateShortcut.Enabled := action_Da_Delete.Enabled;
  btn_Da_Clear.Enabled := not IsEmptyDirectory(LogDirectory);

  PageControl.ActivePageIndex := 0;
  {$IFDEF SQLNCLI}
  case VarToInt(Variables.AsWord[RegDriverMSSQL]) of
    Ord(ddOleDB): cbDatabaseDriver.ItemIndex := cbDatabaseDriver.Items.IndexOfObject(Pointer(ddOleDB));
    Ord(ddNativeClient): cbDatabaseDriver.ItemIndex := cbDatabaseDriver.Items.IndexOfObject(Pointer(ddNativeClient));
  else
    cbDatabaseDriver.ItemIndex := cbDatabaseDriver.Items.IndexOfObject(Pointer(ddAuto));
  end;
  cbDatabaseDriver.Enabled := True;
  {$ELSE}
  cbDatabaseDriver.ItemIndex := cbDatabaseDriver.Items.IndexOfObject(Pointer(ddOleDB));
  cbDatabaseDriver.Enabled := False;
  {$ENDIF}

  FMapLayers := TMapLayers.Create;
  if Assigned(CurrentConfig) then
    begin
      if Assigned(UserSession) then
        FMapLayers.LoadFromDatabase(UserSession.ID, 0)
      else
        FMapLayers.LoadFromDatabase(0, 0);
      Tab_Dl_Map.TabVisible := True;
    end
  else
    Tab_Dl_Map.TabVisible := False;
  acm_Da_Add.Enabled := Tab_Dl_Map.TabVisible;
  tbBrightness.Position:= Min(255, Max(0,Variables.AsInteger[RegMapBrightness]));
  Box_Dl_MapTileCacheEnabled.Checked := Variables.AsBoolean[RegMapCacheEnabled];
  edMapBaseDirectory.Text := Variables.AsString[RegMapBaseDirectory];
  BuildMapLayerList;
  acm_Da_Copy.Enabled := acm_Da_Add.Enabled and (lbMapLayers.ItemIndex <> -1);
  AppendFoldersSheet;
  ReadParameterLog(LogFullDateTime, LogMaxSize);
  Box_dL_FullDateTimeLog.Checked := LogFullDateTime;
  if LogMaxSize > 0 then
    begin
      Box_dL_MaxSizeLog.Checked := True;
      seMaxSizeLog.Value := LogMaxSize;
    end
  else
    Box_dL_MaxSizeLog.Checked := False;
  btn_Da_Apply.Enabled := false;
end;

procedure TForm_Da_Parameters.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult = mrCancel then
    if not ConfigList.ReadOnly then ConfigList.LoadFromXML(ConfigXML);
end;

procedure TForm_Da_Parameters.Btn_Dl_RandomClick(Sender: TObject);
var n,CodeType: Integer;
    t         : String;
    ch        : Char;
begin
  CodeType:=TypeCodeBox.ItemIndex;
  t:= EmptyStr;
  if CodeType in [0..bc_TypeCodeCount-1] then
  begin
    if 0<bc_CodeSize[CodeType] then n:=bc_CodeSize[CodeType]-bc_CodeControl[CodeType]
                               else n:=3+random(10);

    if Length(bc_CodeChar[CodeType])=0 then
      While Length(t)< n do
        t:=t + AnsiChar(32+Random(255-32))
    else
      While Length(t)< n do
        begin
          ch:=bc_CodeChar[CodeType][Random(Length(bc_CodeChar[CodeType]))+1];
          if ORD(ch)>31 then t:=t+ch;
        end;
  end;
  AEdit.Text:=t;
end;

procedure TForm_Da_Parameters.AEditChange(Sender: TObject);
var t: string;
    BarCode: TBarCode;
begin
  Im.Canvas.Pen.Color:= clWhite;
  Im.Canvas.Brush.Color:= Im.Canvas.Pen.Color;
  Im.Canvas.Rectangle(0,0,Im.Width,Im.Height);
  Im.Canvas.Pen.Color:=clBlack;

  BEdit.Text:=bc_GetControlChar(TypeCodeBox.ItemIndex, AEdit.Text);

  if bc_IsCodeType(TypeCodeBox.ItemIndex,AEdit.Text+BEdit.Text,t)
  then begin AEdit.Font.Color:=clBlack;
             BarCode:= TBarCode.Create;
             BarCode.SetCode(TypeCodeBox.ItemIndex, AEdit.Text+BEdit.Text);
             BarCode.PaintBarCode(Im.Canvas, 10);
             BarCode.Free;
       end
  else begin AEdit.Font.Color:=clRed;
       end;
end;

procedure TForm_Da_Parameters.Btn_Df_DefaultClick(Sender: TObject);
begin
  SpeedBox.ItemIndex:=6;
  TimeEdit.Value:=25;
  ParityBox.ItemIndex:=0;
  BitEdit.Value:=8;
  EndBox.ItemIndex:=0;
  StopEdit.Value:=13;
end;

procedure TForm_Da_Parameters.Btn_Da_FindClick(Sender: TObject);
var n,q,i:Integer;
begin
  Q:=-1;
  // перебор портов и поиск работающего сканера
  for i:=0 to PortBox.Items.Count-1 do
  begin
    Variables.AsInteger[RegPort] := integer(PortBox.Items.Objects[I]);
    DM.CommStart(Sender);
    n:=DM.CommPort.GetState;
    DM.CommStop(Sender);
    if (n=112) or ((n= 32)and(Q=-1)) then Q:=i;
  end;
  // запуск обнаруженного сканера
  if Q>=0 then
  begin
    Change(Self);
    PortBox.ItemIndex:=Q;
    Variables.AsInteger[RegPort] := integer(PortBox.Items.Objects[Q]);
    DM.CommStart(Sender);
    Started := true;
  end;
end;

procedure TForm_Da_Parameters.Change(Sender: TObject);
begin
  Btn_Da_Apply.Enabled := True;
  {$IFDEF DBCO_HTTP}
  HTTPSpin.Enabled := box_Dl_HTTPAccess.Checked;
  HTTPSpin.Color   := DisableColor(HTTPSpin.Enabled);
  {$ENDIF}
  LockSpin.Enabled := box_dL_LockSession.Checked;
  LockSpin.Color   := DisableColor(LockSpin.Enabled);
  edMapBaseDirectory.Enabled := Box_Dl_MapTileCacheEnabled.Checked;
  Btn4_Da_Browse.Enabled := edMapBaseDirectory.Enabled;
  Box_dL_FullDateTimeLog.Enabled := Box_dL_EnableLog.Checked or Box_dL_EnableQueryLog.Checked;
  Box_dL_MaxSizeLog.Enabled := Box_dL_FullDateTimeLog.Enabled;
  seMaxSizeLog.Enabled := Box_dL_MaxSizeLog.Checked and Box_dL_MaxSizeLog.Enabled;
  seMaxSizeLog.Color := DisableColor(seMaxSizeLog.Enabled);
end;

procedure TForm_Da_Parameters.TypeCodeBoxChange(Sender: TObject);
var n  : Integer;
begin
  n:=TypeCodeBox.itemIndex;
  AEdit.OnChange:=nil;
  BEdit.Color:=DisableColor(bc_CodeControl[n]=1);
  Btn_Dl_Random.click;
  AEdit.OnChange:=AEditChange;
  AEditChange(Sender);
end;

procedure TForm_Da_Parameters.BtnStartStopClick(Sender: TObject);
begin
  Started := not Started;
  if Started then
  begin
    ApplyPortSettings;
    DM.CommStart(Self);
  end
  else
    DM.CommStop(Self);
end;

procedure TForm_Da_Parameters.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage=tab_Dl_Device then
    Btn_Dl_Random.Click else
  if PageControl.ActivePage=Tab_Dl_Interface then
    begin
      {$IFDEF DBCO_HTTP}
      box_dL_HTTPAccess.Enabled := True;
      HTTPSpin.Enabled := box_dL_HTTPAccess.Checked;
      HTTPSpin.Color := DisableColor(HTTPSpin.Enabled);
      {$ELSE}
      box_dL_HTTPAccess.Enabled := False;
      HTTPSpin.Enabled := False;
      HTTPSpin.Color := DisableColor(HTTPSpin.Enabled);
      {$ENDIF}
    end;
  Btn1_Da_Browse.Enabled := SysUtils.DirectoryExists(LogDirectory);
end;

procedure TForm_Da_Parameters.btn_Da_BrowseClick(Sender: TObject);
var S           : string;
    OpenDlg     : TOpenDialog;
    PathChanged : boolean;
begin
  PathChanged := false;

  case TDatabaseType(cbxDBType.MappedID) of
    dtBDE: { Borland Database Engine }
      begin
        s := edConnectString.Text;
        if SelectDirectory(GetTitle('_Dl.Location'), EmptyStr, S) then
        begin
          PathChanged := S <> edConnectString.Text;
          edConnectString.Text := S;
        end;
      end;
    dtInterbase: { Interbase or Firebird }
      begin
        OpenDlg := TOpenDialog.Create(Self);
        try
          OpenDlg.Title       := GetTitle('_Dl.Location');
          OpenDlg.InitialDir  := edConnectString.Text;
          OpenDlg.Filter      := GetTitle(dlgFilters);
          OpenDlg.FilterIndex := dlgFilterIB;
          if OpenDlg.Execute then
            begin
              PathChanged := OpenDlg.FileName <> edConnectString.Text;
              edConnectString.Text:=OpenDlg.FileName;
            end;
        finally
          OpenDlg.Free;
        end;
      end;
    dtADOXML: { ADO XML Database }
      begin
        OpenDlg := TOpenDialog.Create(Self);
        try
          OpenDlg.Title      := GetTitle('_Dl.Location');
          OpenDlg.DefaultExt := sExtensionXML;
          OpenDlg.Filter := 'ADO XML Files|*.xml';
          OpenDlg.FileName := ExtractFileName(edConnectString.Text);
          OpenDlg.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableIncludeNotify, ofEnableSizing, ofDontAddToRecent];
          OpenDlg.InitialDir := ExtractFilePath(edConnectString.Text);
          if OpenDlg.Execute then
            begin
              PathChanged := OpenDlg.FileName <> edConnectString.Text;
              edConnectString.Text := OpenDlg.FileName;
            end;
        finally
          OpenDlg.Free;
        end;
      end;
    dtDBCOXML: { ADO XML Database }
      begin
        OpenDlg := TOpenDialog.Create(Self);
        try
          OpenDlg.Title      := GetTitle('_Dl.Location');
          OpenDlg.DefaultExt := sExtensionXMLX;
          OpenDlg.Filter := 'DBCO XML Files|*.xml;*.xmlx;*.zip';
          OpenDlg.FileName := ExtractFileName(edConnectString.Text);
          OpenDlg.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableIncludeNotify, ofEnableSizing, ofDontAddToRecent];
          OpenDlg.InitialDir := ExtractFilePath(edConnectString.Text);
          if OpenDlg.Execute then
            begin
              PathChanged := OpenDlg.FileName <> edConnectString.Text;
              edConnectString.Text := OpenDlg.FileName;
            end;
        finally
          OpenDlg.Free;
        end;
      end;
    { TODO -oКуфенко П. Ю. -cДоработка : Здесь необходимо правильно сделать обзор для остальных типов баз данных! }
  end;
  if PathChanged then
    SolutionDataChange(edConnectString);
end;

procedure TForm_Da_Parameters.btn_Da_ClearClick(Sender: TObject);
begin
  ClearDirectory(LogDirectory);
  btn_Da_Clear.Enabled := not IsEmptyDirectory(LogDirectory);
end;

procedure TForm_Da_Parameters.Btn1_Da_BrowseClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, nil, PChar(LogDirectory), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm_Da_Parameters.Btn2_Da_BrowseClick(Sender: TObject);
var s : string;
begin
  s:= Edit2.Text;
  if SelectDirectory( L_Dl_OutputFiles.Caption, EmptyStr, S) then
    Edit2.Text := S;
end;

procedure TForm_Da_Parameters.RefreshTables;
begin
  //
end;

procedure TForm_Da_Parameters.ApplyPortSettings;
begin
  if PortBox.ItemIndex >= 0 then
    Variables.AsInteger[RegPort]     := integer(PortBox.Items.Objects[PortBox.ItemIndex])
  else
    Variables.AsInteger[RegPort] := 0;
  Variables.AsInteger[RegSpeed]    := SpeedBox.ItemIndex;
  Variables.AsInteger[RegOdd]      := ParityBox.ItemIndex;
  Variables.AsInteger[RegEndBits]  := EndBox.ItemIndex;
  Variables.AsInteger[RegTime]     := TimeEdit.Value;
  Variables.AsInteger[RegBit]      := BitEdit.Value;
  Variables.AsInteger[RegStop]     := StopEdit.Value;
  Variables.AsBoolean[RegSoundOn]  := Box_Dl_SoundOn.Checked;
  Variables.AsString[RegRemSoundPath] := eSoundFilePath.Text;
end;

function TForm_Da_Parameters.ApplySettings : boolean;
var i: integer;
    NeedUpdate: Boolean;
begin
  result := SettingsValid;
  if result then
  begin
    Variables.AsBoolean[RegRemStartup] := Box_Dl_RememberStartup.Checked;
    Variables.AsString[RegDirPath]  := Edit2.Text;
    ApplyPortSettings;
    Variables.AsInteger[RegLockTime] := LockSpin.Value;
    Variables.AsInteger[RegStart] :=Integer( Box_Dl_UseBarcode.Checked);
    Variables.AsInteger[RegTray]  :=Integer( Box_Dl_ToTray.Checked);
    {$IFDEF DBCO_HTTP}
    Variables.AsInteger[RegHTTP]  :=Integer( Box_Dl_HTTPAccess.Checked);
    Variables.AsInteger[RegPortHTTP] := HTTPSpin.Value;
    {$ENDIF}
    Variables.AsInteger[RegLockSession] :=Integer( box_dL_LockSession.Checked);
    Variables.AsInteger[RegAnim]  := MaxAnimation + 1 -
      tbAnimation.Position * (MaxAnimation - MinAnimation + 1) div
      (tbAnimation.Max - tbAnimation.Min);
    Variables.AsBoolean[RegCheckUpdates]  := box_Dl_CheckUpdates.Checked;
    Variables.AsBoolean[RegCopyAllField]  := box_Dl_CopyAllField.Checked;
    Variables.AsBoolean[RegCopyHeaderField] := Box_Dl_CopyHeaderField.Checked;

    Variables.AsBoolean[RegDragDrop] := Box_Dl_DragDrop.Checked;

    Btn_Da_Apply.Enabled:=False;
    NeedUpdate:= False;

    if (Variables.AsBoolean[RegGridBrush] <> cb_Dl_GridBrush.Checked) then
      begin
        Variables.AsBoolean[RegGridBrush] := cb_Dl_GridBrush.Checked;
        NeedUpdate:= True;
      end;

    if (Variables.AsBoolean[RegGridLines] <> cb_Dl_GridLines.Checked) then
      begin
        Variables.AsBoolean[RegGridLines] := cb_Dl_GridLines.Checked;
        NeedUpdate:= True;
      end;

    if NeedUpdate and assigned(MForm) then
      for i:= 0 to Pred(MForm.BaseFormsCount) do
        MForm.BaseForms[i].ReCreateView;

    Variables.AsBoolean[RegLogFile]        := Box_dL_EnableLog.Checked;
    {$IFDEF SQLNCLI}
    if cbDatabaseDriver.ItemIndex <> -1 then
      Variables.AsWord[RegDriverMSSQL] := Integer(cbDatabaseDriver.Items.Objects[cbDatabaseDriver.ItemIndex]);
    {$ENDIF}
    Variables.AsBoolean[RegLogQuery]       := Box_dL_EnableQueryLog.Checked;
    Variables.AsBoolean[RegShowHint]       := Box_Da_ShowHint.Checked;
    Variables.AsBoolean[RegShowStatus]     := Box_Da_StatusBar.Checked;
    Variables.AsBoolean[RegMenuS] := Box_Da_StandardMenu.Checked;
    Variables.AsBoolean[RegAutoSaveChanges]:= Box_Da_AutoSaveChanges.Checked;

    if Box_Dl_ImageOldStoreMode.Checked then
      Variables.AsInteger[RegImageStoreMode] := 0
    else
      Variables.AsInteger[RegImageStoreMode] := 1;

    if Box_dL_MaxSizeLog.Checked then
      WriteParameterLog(Box_dL_FullDateTimeLog.Checked, seMaxSizeLog.Value)
    else
      WriteParameterLog(Box_dL_FullDateTimeLog.Checked, 0);

    Variables.AsBoolean[RegMapCacheEnabled] := Box_Dl_MapTileCacheEnabled.Checked;
    Variables.AsInteger[RegMapBrightness] := tbBrightness.Position;
    Variables.AsString[RegMapBaseDirectory] := edMapBaseDirectory.Text;
    if Assigned(FMapLayers) and FMapLayers.Modified then
      FMapLayers.SaveToDatabase;

    if not ConfigList.ReadOnly then
      begin
        // сохранение списка решений
        ConfigList.SaveToXML(ConfigXML);
      end;
  end
  else
    ModalResult := mrNone;
end;

procedure TForm_Da_Parameters.SetStartStopCaption;
begin
  if Started then
    BtnStartStop.Caption := GetTitle('_Dl.Stop')
  else
    BtnStartStop.Caption := GetTitle('_Dl.Start');
end;

procedure TForm_Da_Parameters.SetStarted(const aStarted : boolean);
begin
  if aStarted <> Started then
  begin
    FStarted := aStarted;
    Btn_Da_Find.Enabled := not Started;
    SetStartStopCaption;
  end;
end;

procedure TForm_Da_Parameters.Btn_Da_ApplyClick(Sender: TObject);
begin
  if ApplySettings then
  begin
    MForm.SBar.Visible := Variables.AsBoolean[RegShowStatus];
    MForm.ShowHint     := Variables.AsBoolean[RegShowHint];
    LangFormUpdate(Self);
    RefreshTables;
    SetStartStopCaption;
    BringToFront;
  end;
end;

procedure TForm_Da_Parameters.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key)=27 then Btn_Da_Cancel.Click;
  if Ord(Key)=13 then Btn_Ok.Click;
end;

procedure TForm_Da_Parameters.FormShow(Sender: TObject);
begin
  if screen.Width < 900 then Width:= 720;
end;

procedure TForm_Da_Parameters.Btn_OkClick(Sender: TObject);
begin
  if ApplySettings then
  begin
    MForm.SBar.Visible := Variables.AsBoolean[RegShowStatus];
    MForm.ShowHint     := Variables.AsBoolean[RegShowHint];

    RefreshTables;
    SetStartStopCaption;
  end;
end;

procedure TForm_Da_Parameters.CommPortReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: Integer);
var s: String;
    i: Integer;
begin
  for i:=0 to DataSize-2 do s:= s + Chr(Byte((Pointer(NativeInt(DataPtr)+i))^));

  for i:= Length(s) downto 1 do
    if s[i]='&' then Insert('&',s,i);
  SendMessage( Application.MainForm.Handle, DM_INFONOTIFY, 23, NativeInt('_Dl.check : '#13+s));
end;

procedure TForm_Da_Parameters.tab_Dl_DeviceShow(Sender: TObject);
const PortPrefix = 'COM';
var Reg : TRegistry;
    I, PortNumber, PrefixPos : integer;
    PortName : string;
begin
  SetStartStopCaption;
  if PortBox.Items.Count = 0 then
    begin
      Reg := TRegistry.Create;
      try
        Reg.Access := KEY_READ;
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey('Hardware\DeviceMap\SerialComm',False) then
        begin
          Reg.GetValueNames(PortBox.Items);
          for i := 0 to PortBox.Items.Count-1 do
          begin
            PortName := reg.ReadString(PortBox.Items[i]);
            PrefixPos := Pos(PortPrefix, PortName);
            if PrefixPos > 0 then
              try
                PortNumber := StrToInt(Copy(PortName,
                  PrefixPos + Length(PortPrefix), MaxInt));
              except
                PortNumber := 0;
              end
            else
                PortNumber := 0;
            PortBox.Items[i] := PortName;
            PortBox.Items.Objects[i] := pointer(PortNumber);
          end;
          Reg.CloseKey;
        end;
      finally
        Reg.Free;
      end;
        if PortBox.Items.Count > 0 then PortBox.ItemIndex := 0;
    end;
end;

procedure TForm_Da_Parameters.CheckSolutionControls;
var Enabled : boolean;
begin
  Enabled := (lbSolutions.ItemIndex >= 0) and not ConfigList.ReadOnly;
  action_Da_Add.Enabled := not ConfigList.ReadOnly;
  edName.Enabled := Enabled;
  cbxDBType.Enabled := Enabled;
  cbxDBCodePage.Enabled := Enabled;
  edServer.Enabled := Enabled;
  edDatabase.Enabled := Enabled;
  edLogin.Enabled := Enabled;
  edPassword.Enabled := Enabled;
  edConnectString.Enabled := Enabled;
  edDefaultUser.Enabled := Enabled;
  edDefaultPassword.Enabled := Enabled;
  btn_Da_Browse.Enabled := Enabled and (TDatabaseType(cbxDBType.MappedID) in [dtBDE, dtInterbase, dtADOXML, dtDBCOXML]);
  action_Da_Test.Enabled := (cbxDBType.MappedID >= 0) and (lbSolutions.ItemIndex <> -1);
end;

procedure TForm_Da_Parameters.lbMapLayersClick(Sender: TObject);
var
  MapLayer: TMapLayer;
begin
  if lbMapLayers.ItemIndex <> -1 then
    MapLayer := lbMapLayers.Items.Objects[lbMapLayers.ItemIndex] as TMapLayer
  else
    MapLayer := nil;
  if Assigned(MapLayer) then
    begin
      Lm_Df_Name.Enabled := True;
      edMapLayerName.Enabled := True;
      edMapLayerName.Text := MapLayer.Name;
      Lm_Df_Template.Enabled := True;
      edMapTemlplateURL.Enabled := True;
      edMapTemlplateURL.Text := MapLayer.MP.Source;
      acm_Da_Delete.Enabled := acm_Da_Add.Enabled and Assigned(UserSession) and (UserSession.IsAdmin or (MapLayer.UserID = UserSession.ID));
    end
  else
    begin
      Lm_Df_Name.Enabled := False;
      edMapLayerName.Enabled := False;
      edMapLayerName.Text := EmptyStr;
      Lm_Df_Template.Enabled := False;
      edMapTemlplateURL.Enabled := False;
      edMapTemlplateURL.Text := EmptyStr;
      acm_Da_Delete.Enabled := False;
    end;
  acm_Da_Copy.Enabled := acm_Da_Add.Enabled and (lbMapLayers.ItemIndex <> -1);
  edMapLayerName.ReadOnly := acm_Da_Add.Enabled and not acm_Da_Delete.Enabled;
  edMapTemlplateURL.ReadOnly := edMapLayerName.ReadOnly;
end;

procedure TForm_Da_Parameters.lbSolutionsClick(Sender: TObject);
var i: Integer;
begin
  CheckSolutionControls;
  if lbSolutions.ItemIndex >= 0 then
    with ConfigList[lbSolutions.ItemIndex] do
    begin
      edName.Text := Name;
      cbxDBType.MappedID := Ord(DBType);
      cbxDBCodePage.MappedID:= CodePage;

      edServer.Text := Server;
      edDatabase.Text := Database;
      edLogin.Text := Login;
      edPassword.Text := Password;
      edConnectString.Text := ConnectString;
      edDefaultUser.Text := DefaultLogin;
      edDefaultPassword.Text := DefaultPassword;
    end
  else
    begin
      edName.Text := EmptyStr;
      cbxDBType.MappedID := unassigned;
      cbxDBCodePage.MappedID := unassigned;
      edServer.Text := EmptyStr;
      edDatabase.Text := EmptyStr;
      edLogin.Text := EmptyStr;
      edPassword.Text := EmptyStr;
      edConnectString.Text := EmptyStr;
      edDefaultUser.Text := EmptyStr;
      edDefaultPassword.Text := EmptyStr;
    end
end;

procedure TForm_Da_Parameters.btnDaAddClick(Sender: TObject);
var Item : TDeConfigItem;
begin
  Item := TDeConfigItem.Create;
  Item.Name := GetTitle('_Solution');
  ExpandName(Item);
  ConfigList.Add(Item);
  lbSolutions.Items.Add(Item.Name);
  lbSolutions.ItemIndex := Pred(lbSolutions.Items.Count);
  lbSolutionsClick(Self);
  Change(Sender);
  action_Da_Delete.Enabled := not ConfigList.ReadOnly;
  action_Da_Copy.Enabled := action_Da_Delete.Enabled;
  action_Da_CreateShortcut.Enabled := (cbxDBType.MappedID > 0) and (lbSolutions.ItemIndex <> -1) and not ConfigList.ReadOnly;
end;

procedure TForm_Da_Parameters.action_Da_CopyExecute(Sender: TObject);
var Item : TDeConfigItem;
begin
  Item := TDeConfigItem.Create;
  with lbSolutions do
    Item.Assign(ConfigList[ItemIndex]);
  ExpandName(Item);
  ConfigList.Add(Item);
  lbSolutions.Items.Add(Item.Name);
  lbSolutions.ItemIndex := lbSolutions.Items.Count-1;
  lbSolutionsClick(Self);
  Change(Sender);
end;

procedure TForm_Da_Parameters.btnDaDeleteClick(Sender: TObject);
var Index : integer;
begin
  Index := lbSolutions.ItemIndex;
  if Index >= 0 then
  begin
    ConfigList.Delete(Index);
    lbSolutions.Items.Delete(Index);
  end;
  if Index >= lbSolutions.Items.Count then
    dec(Index);
  lbSolutions.ItemIndex := Index;
  action_Da_Delete.Enabled := (lbSolutions.Items.Count > 0) and not ConfigList.ReadOnly;
  action_Da_Copy.Enabled := action_Da_Delete.Enabled;
  action_Da_CreateShortcut.Enabled := (cbxDBType.MappedID >= 0) and (lbSolutions.ItemIndex <> -1) and not ConfigList.ReadOnly;
  lbSolutionsClick(Self);
  Change(Sender);
end;

procedure TForm_Da_Parameters.action_Da_TestExecute( Sender: TObject);
var DBItem : TDeCustomDatabase;
    ErrorCode: Integer;
    ErrorText: String;
begin
    DBItem := nil;
  try
    DBItem:= MetaData.CreateDatabase(ConfigList[lbSolutions.ItemIndex], False);

    if DBItem.CheckConnection then
      begin
        if DBItem.CheckMetaConnection(ErrorCode, ErrorText) then
          case ErrorCode of
            meOk:
              begin
                SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar('_De.ConnectOk')));
              end;
{
            meConnect, meConnectDB, meConnectServer, meConnectPassword, meConnectNetwork, meConnectLibrary:
              begin
                SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
                    NativeInt(PChar(Format(GetTitle('_De.ConnectEr'),[ErrorText]))));
              end;
{}
            meEmpty, meMetaEmpty, meMetaBroken, meMetaVersion:
              begin
                SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
                  NativeInt(PChar('_eRror.MetabaseError'+#10+ ErrorText)))
              end;
          end
        else
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar('_eRror.connectmeta')));
      end
    else
      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
          NativeInt(PChar('_De.ConnectEr'#10+DBItem.ConnectErrorMessage)));
  finally
    if assigned(DBItem) then DBItem.Free;
  end;
end;

procedure TForm_Da_Parameters.SolutionDataChange(Sender : TObject);
var Changed : boolean;
begin
  Changed := false;
  if lbSolutions.ItemIndex >= 0 then
    with ConfigList[lbSolutions.ItemIndex] do
    begin
      if Sender = edName then
      begin
        Changed := edName.Text <> Name;
        Name := edName.Text;
        lbSolutions.Items[lbSolutions.ItemIndex] := Name;
      end
      else if Sender = cbxDBType then
      begin
        Changed := cbxDBType.MappedID <> ord(DBType);
        DBType := TDatabaseType(cbxDBType.MappedID);
      end
      else if Sender = cbxDBCodePage then
      begin
        Changed := cbxDBType.MappedID <> CodePage;
        CodePage := cbxDBCodePage.MappedID;
      end
      else if Sender = edServer then
      begin
        Changed := edServer.Text <> Server;
        Server := edServer.Text;
      end
      else if Sender = edDatabase then
      begin
        Changed := edDatabase.Text <> Database;
        Database := edDatabase.Text;
      end
      else if Sender = edLogin then
      begin
        Changed := edLogin.Text <> Login;
        Login := edLogin.Text;
      end
      else if Sender = edPassword then
      begin
        Changed := edPassword.Text <> Password;
        Password := edPassword.Text;
      end
      else if Sender = edConnectString then
      begin
        Changed := edConnectString.Text <> ConnectString;
        ConnectString := edConnectString.Text;
      end
      else if Sender = edDefaultUser then
      begin
        Changed := edDefaultUser.Text <> DefaultLogin;
        DefaultLogin := edDefaultUser.Text;
      end
      else if Sender = edDefaultPassword then
      begin
        Changed := edDefaultPassword.Text <> DefaultPassword;
        DefaultPassword := edDefaultPassword.Text;
      end;
    end;
  if Changed then
    Change(Sender);
end;

function TForm_Da_Parameters.SettingsValid : boolean;
var I : integer;
begin
  result := true;
  for I := 0 to ConfigList.Count-1 do
    if Length(Trim(ConfigList[I].Name)) = 0 then
    begin
      result := false;
      PageControl.ActivePage := Tab_Dl_Configuration;
      lbSolutions.ItemIndex := I;
      edName.SetFocus;
      ShowHintWindow(
        edName,
        Format(GetTitle('_dE.empty'),[GetTitle('_Df.Name')]),
        GetTitle('_dE.error'),
        icoError);
    end;
end;

procedure TForm_Da_Parameters.ExpandName(aItem : TDeConfigItem);
var I, Index : integer;
    BaseName : string;
begin
  BaseName := aItem.Name;
  Index := lbSolutions.Items.IndexOf(aItem.Name);
  I := 1;
  while Index >= 0 do
  begin
    inc(I);
    aItem.Name := BaseName + IntToStr(I);
    Index := lbSolutions.Items.IndexOf(aItem.Name);
  end;
end;

procedure TForm_Da_Parameters.FormDestroy(Sender: TObject);
begin
  DM.CommPort.OnReceiveData := FOldPortEvent;
  FSolutionsForm.Free;
  FParametersForm.Free;
  FSolutionsDSM.Free;
  FreeAndNil(FDefaultLayers);
  FreeAndNil(FMapLayers);
end;

procedure TForm_Da_Parameters.Bnm_Da_DefaultClick(Sender: TObject);
var
  P: TPoint;
begin
  if not Assigned(FDefaultLayers) then
    begin
      FDefaultLayers := TMapLayers.Create;
      FDefaultLayers.LoadSystemDefaults;
      BuildPopupLayerList;
    end;
  P := Bnm_Da_Default.ClientToScreen(Point(0, Bnm_Da_Default.Height));
  pmMapLayers.Popup(P.X, P.Y);
//  FMapLayers.LoadSystemDefaults;
//  FMapLayers.SaveToDatabase;
//  BuildMapLayerList;
end;

procedure TForm_Da_Parameters.Box_Dl_SoundOnClick(Sender: TObject);
begin
  Btn3_Da_Browse.Enabled := Box_Dl_SoundOn.Checked;
  eSoundFilePath.Enabled := Box_Dl_SoundOn.Checked;
end;

procedure TForm_Da_Parameters.Btn3_Da_BrowseClick(Sender: TObject);
resourcestring
  sSoundFileFilter = 'Sound files (*.wav)|*.wav';
var
   OpenDlg : TOpenDialog;
begin
  OpenDlg := TOpenDialog.Create(Self);
  try
    OpenDlg.InitialDir := ExtractFilePath(eSoundFilePath.Text);
    OpenDlg.FileName := ExtractFileName(eSoundFilePath.Text);
    OpenDlg.Filter := sSoundFileFilter;
    OpenDlg.DefaultExt := sExtensionWAV;
    OpenDlg.Options := [ofPathMustExist, ofFileMustExist];
    if OpenDlg.Execute then
      eSoundFilePath.Text := OpenDlg.FileName;
  finally
    OpenDlg.Free;
  end;
end;

procedure TForm_Da_Parameters.btn_Da_LoadFromClick(Sender: TObject);
begin
  MetaDataOperations.LoadSolution(Sender{, Integer(FSolutionsDSM.Cache)});
end;

procedure TForm_Da_Parameters.btn_Da_LoadToClick(Sender: TObject);
begin
  MetaDataOperations.ExtractSolution(Sender{, Integer(FSolutionsDSM.Cache)});
end;

procedure TForm_Da_Parameters.btn2_Da_DeleteClick(Sender: TObject);
begin
  MetaDataOperations.DeleteSolution(Sender{, Integer(FSolutionsDSM.Cache)});
end;

procedure TForm_Da_Parameters.action_Da_CreateShortcutExecute(Sender: TObject);
var
  ConfigItem: TDeConfigItem;
  FileName, Parameters: string;
  Unknown: IUnknown;
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
begin
  ConfigItem := ConfigList[lbSolutions.ItemIndex];
  FileName := GetDesktopDirectory + NormalizeFileName(ConfigItem.Name) + sExtensionLNK;
  Parameters := ConfigItem.PrepareCommandLineParameters;
  Parameters := Trim(Parameters + Variables.PrepareCommandLineParameters);

  Unknown := CreateComObject(CLSID_ShellLink);
  try
    ShellLink := Unknown as IShellLink;
    try
      ShellLink.SetPath(PChar(Application.ExeName));
      ShellLink.SetArguments(PChar(Parameters));
      ShellLink.SetWorkingDirectory(PChar(ExtractFilePath(Application.ExeName)));
      ShellLink.SetDescription(PChar(ConfigItem.Name));
      ShellLink.SetIconLocation(PChar(Application.ExeName), 0);
      PersistFile := Unknown as IPersistFile;
      try
        PersistFile.Save(PChar(FileName), True);
      finally
        PersistFile := nil;
      end;
    finally
      ShellLink := nil;
    end;
  finally
    Unknown := nil;
  end;
  SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo,
    NativeInt(PChar(GetTitle('_Da.CreateDesktopShortcutCompleted'))));
end;

procedure TForm_Da_Parameters.AppendFoldersSheet;
var
  TabSheet: TTabSheet;
  FolderTop: Integer;
  procedure AppendFolder(const Caption, Hint, Directory: string);
  var
    CaptionLabel: TLabel;
    Edit: TEdit;
    Button: TSpeedButton;
  begin
    if Length(Directory) <> 0 then
      begin
        CaptionLabel := TLabel.Create(TabSheet);
        try
          CaptionLabel.Parent := TabSheet;
          CaptionLabel.Layout := tlCenter;
          CaptionLabel.Caption := Caption;
          if (Length(Caption) <> 0) and (Caption[Length(Caption)] <> ':') then
            CaptionLabel.Caption := CaptionLabel.Caption + ':';
          CaptionLabel.Hint := Hint;
          CaptionLabel.AutoSize := False;
          Edit := TEdit.Create(TabSheet);
          try
            Edit.Parent := TabSheet;
            Edit.SetBounds(8 * 2 + 120, FolderTop,
              TabSheet.ClientWidth - (8 * 2 + 120) - Edit.Height - 8 - 1, Edit.Height);
            CaptionLabel.SetBounds(8, FolderTop, 120, Edit.Height);
            CaptionLabel.FocusControl := Edit;
            Edit.Anchors := [akTop, akLeft, akRight];
            Edit.ReadOnly := True;
            Edit.ParentColor := True;
            Edit.Text := ExcludeTrailingBackslash(Directory);
            Button := TSpeedButton.Create(TabSheet);
            try
              Button.Parent := TabSheet;
              Button.SetBounds(Edit.Left + Edit.Width, FolderTop, Succ(Edit.Height), Edit.Height);
              Button.Anchors := [akTop, akRight];
              Button.Caption := '...';
              Button.Hint := 'Обзор папки';
              Button.Tag := NativeInt(Edit);
              Button.OnClick := FolderBrowseClick;
              Button.Enabled := SysUtils.DirectoryExists(Directory);
            except
              Button.Free;
              raise;
            end;
          except
            Edit.Free;
            raise;
          end;
        except
          CaptionLabel.Free;
          raise;
        end;
        Inc(FolderTop, Edit.Height + 4);
      end;
  end;
begin
  if Assigned(UserSession) and (UserSession.IsAdmin) then
    begin
      FolderTop := 8;
      TabSheet := TTabSheet.Create(PageControl);
      try
        TabSheet.PageControl := PageControl;
        TabSheet.ShowHint := True;
        TabSheet.Caption := 'Каталоги';
        AppendFolder('Временные файлы', 'DeTempDir', DeTempDir);
        AppendFolder(EmptyStr, 'TPath.GetTempPath', TPath.GetTempPath);
        AppendFolder(EmptyStr, 'DeControls.GetTempFile', ExtractFilePath(DeControls.GetTempFile));
        AppendFolder(EmptyStr, 'Funcs.DeGetTempFileName', ExtractFilePath(Funcs.DeGetTempFileName(EmptyStr, EmptyStr, EmptyStr)));
        AppendFolder('Персональные файлы', 'Variables.AsString[RegDirPath]', Variables.AsString[RegDirPath]);
        AppendFolder('Файлы логирования', 'LogDirectory', LogDirectory);
      except
        TabSheet.Free;
        raise;
      end;
    end;
end;

procedure TForm_Da_Parameters.FolderBrowseClick(Sender: TObject);
begin
  if Assigned(Sender) and (Sender is TSpeedButton) then
    ShellExecute(Application.Handle, nil, PChar(IncludeTrailingBackslash(TEdit((Sender as TSpeedButton).Tag).Text)), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm_Da_Parameters.BuildProviderList;
  function AppendDatabaseDriver(const Text: string; const DatabaseDriver: TADODatabaseDriver): Integer;
  begin
    Result := cbDatabaseDriver.Items.AddObject(Text, Pointer(DatabaseDriver));
  end;
  function AppendProvider(const ProviderName: string; const DatabaseDriver: TADODatabaseDriver): Integer;
  var
    Registry: TRegistry;
    Description: string;
  begin
    Registry := TRegistry.Create;
    try
      Registry.RootKey := HKEY_CLASSES_ROOT;
      if Registry.OpenKeyReadOnly(ProviderName) then
        Description := Trim(Registry.ReadString(EmptyStr))
      else
        Description := EmptyStr;
    finally
      Registry.Free;
    end;
    if Length(Description) = 0 then Description := ProviderName;
    Result := AppendDatabaseDriver(Description, DatabaseDriver);
  end;
begin
  cbDatabaseDriver.Items.BeginUpdate;
  try
    cbDatabaseDriver.Items.Clear;
    AppendDatabaseDriver(GetTitle('_Dl.DatabaseDriverAuto'), ddAuto);
    AppendProvider(cProviderSQLOLEDB, ddOleDB);
    {$IFDEF SQLNCLI}
    case HiWord(GetNativeClientVersion) of
      10: AppendProvider(cProviderSQLNCLI10, ddNativeClient);
      11: AppendProvider(cProviderSQLNCLI11, ddNativeClient);
    else
      AppendProvider(cProviderSQLNCLI, ddNativeClient);
    end;
    {$ENDIF}
  finally
    cbDatabaseDriver.Items.EndUpdate;
  end;
end;

procedure TForm_Da_Parameters.BuildMapLayerList;
var
  LayerID, Index, NewIndex: Integer;
begin
  lbMapLayers.Items.BeginUpdate;
  try
    if lbMapLayers.ItemIndex <> -1 then
      LayerID := (lbMapLayers.Items.Objects[lbMapLayers.ItemIndex] as TMapLayer).LayerID
    else
      LayerID := 0;
    lbMapLayers.Items.Clear;
    if Assigned(FMapLayers) then
      for Index := 0 to Pred(FMapLayers.Count) do
        if FMapLayers[Index].Stage <> mlsDeleted then
          begin
            NewIndex := lbMapLayers.Items.AddObject(FMapLayers[Index].Name, FMapLayers[Index]);
            if FMapLayers[Index].LayerID = LayerID then
              lbMapLayers.ItemIndex := NewIndex;
          end;
  finally
    lbMapLayers.Items.EndUpdate;
    //Bnm_Da_Default.Enabled := lbMapLayers.Items.Count = 0;
  end;
  lbMapLayersClick(lbMapLayers);
end;

procedure TForm_Da_Parameters.lbMapLayersDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  MapLayer: TMapLayer;
  OffsetX, ImageIndex: Integer;
  TableMeta: TTableMeta;
begin
  (Control as TListBox).Canvas.FillRect(Rect);
  if Assigned(FMapLayers) then
    MapLayer := (Control as TListBox).Items.Objects[Index] as TMapLayer
  else
    MapLayer := nil;
  OffsetX := 1;
  if Assigned(MapLayer) then
    begin
      ImageIndex := $9030; // Метка на карте ...
      if MapLayer.UserID <> 0 then
        ImageIndex := $00040000 or ImageIndex // Пользователь на метке ...
      else
        if MapLayer.DatasetID <> 0 then
          if Assigned(MetaData) then
            begin
              TableMeta := MetaData.GetTableMeta(MapLayer.DatasetID);
              if Assigned(TableMeta) then
                if TableMeta.ICO <> -1 then
                  ImageIndex := ((LoWord(TableMeta.ICO) and $0FFF) shl 8) or ImageIndex; // Иконка набора данных на метке ...
            end;
      DM.ilIcon16.Draw((Control as TListBox).Canvas, Rect.Left + OffsetX, Rect.Top + 1, DM.MapIconIndex(ImageIndex));
      OffsetX := OffsetX + DM.ilIcon16.Width + 2;
    end;
  (Control as TListBox).Canvas.TextOut(Rect.Left + OffsetX,
    Rect.Top + ((Rect.Bottom - Rect.Top - (Control as TListBox).Canvas.TextHeight((Control as TListBox).Items[Index])) div 2),
    (Control as TListBox).Items[Index]);
end;

procedure TForm_Da_Parameters.ExpandLayerName(MapLayer: TMapLayer);
var
  NewIndex, Index: Integer;
  BaseName: string;
begin
  if Assigned(MapLayer) then
    begin
      BaseName := MapLayer.Name;
      Index := lbMapLayers.Items.IndexOf(BaseName);
      NewIndex := 1;
      while Index <> -1 do
        begin
          Inc(NewIndex);
          MapLayer.Name := BaseName + IntToStr(NewIndex);
          Index := lbMapLayers.Items.IndexOf(MapLayer.Name);
        end;
    end;
end;

resourcestring
  sNewLayerName = 'Новый слой';

procedure TForm_Da_Parameters.acm_Da_AddExecute(Sender: TObject);
var
  MapLayer: TMapLayer;
begin
  MapLayer := FMapLayers.NewLayer;
  if Assigned(MapLayer) then
    begin
      MapLayer.Name := sNewLayerName;
      if Assigned(UserSession) then MapLayer.UserID := UserSession.ID;
      ExpandLayerName(MapLayer);
      lbMapLayers.ItemIndex := lbMapLayers.Items.AddObject(MapLayer.Name, MapLayer);
      lbMapLayersClick(lbMapLayers);
      Change(Sender);
    end;
end;

procedure TForm_Da_Parameters.acm_Da_CopyExecute(Sender: TObject);
var
  SourceLayer, MapLayer: TMapLayer;
begin
  if lbMapLayers.ItemIndex <> -1 then
    SourceLayer := lbMapLayers.Items.Objects[lbMapLayers.ItemIndex] as TMapLayer
  else
    SourceLayer := nil;
  MapLayer := FMapLayers.NewLayer(SourceLayer);
  if Assigned(MapLayer) then
    begin
      if Length(MapLayer.Name) = 0 then MapLayer.Name := sNewLayerName;
      ExpandLayerName(MapLayer);
      if Assigned(UserSession) then
        if not UserSession.IsAdmin then
          MapLayer.UserID := UserSession.ID;
      lbMapLayers.ItemIndex := lbMapLayers.Items.AddObject(MapLayer.Name, MapLayer);
      lbMapLayersClick(lbMapLayers);
      Change(Sender);
    end;
end;

procedure TForm_Da_Parameters.acm_Da_DeleteExecute(Sender: TObject);
var
  Index: Integer;
  MapLayer: TMapLayer;
begin
  Index := lbMapLayers.ItemIndex;
  if Index <> -1 then
    begin
      MapLayer := lbMapLayers.Items.Objects[Index] as TMapLayer;
      MapLayer.MarkDeleted;
      lbMapLayers.Items.Delete(Index);
      if lbMapLayers.Items.Count <> 0 then
        if Index > Pred(lbMapLayers.Items.Count) then
          lbMapLayers.ItemIndex := Pred(lbMapLayers.Items.Count)
        else
          lbMapLayers.ItemIndex := Index;
      lbMapLayersClick(lbMapLayers);
      Change(Sender);
    end;
end;

procedure TForm_Da_Parameters.Btn4_Da_BrowseClick(Sender: TObject);
resourcestring
  sMapBaseDirectoryCaption = 'Выберите каталог кэширования слоёв карты ...';
var
  Directory: string;
begin
  Directory := Trim(edMapBaseDirectory.Text);
  if Length(Directory) = 0 then
    Directory := MapDirectory;
  if SelectDirectory(sMapBaseDirectoryCaption, EmptyStr, Directory) then
    edMapBaseDirectory.Text := Directory;
end;

procedure TForm_Da_Parameters.edMapLayerNameChange(Sender: TObject);
var
  Index: Integer;
  MapLayer: TMapLayer;
begin
  Index := lbMapLayers.ItemIndex;
  if Index <> -1 then
    begin
      MapLayer := lbMapLayers.Items.Objects[Index] as TMapLayer;
      if not SameStr(MapLayer.Name, (Sender as TEdit).Text) then
        begin
          MapLayer.Name := (Sender as TEdit).Text;
          lbMapLayers.Items[Index] := MapLayer.Name;
          Change(Sender);
        end;
    end;
end;

procedure TForm_Da_Parameters.edMapTemlplateURLChange(Sender: TObject);
var
  Index: Integer;
  MapLayer: TMapLayer;
begin
  Index := lbMapLayers.ItemIndex;
  if Index <> -1 then
    begin
      MapLayer := lbMapLayers.Items.Objects[Index] as TMapLayer;
      if not SameStr(MapLayer.MP.Source, (Sender as TEdit).Text) then
        begin
          MapLayer.MP.Source := (Sender as TEdit).Text;
          Change(Sender);
        end;
    end;
end;

procedure TForm_Da_Parameters.BuildPopupLayerList;
var
  Index: Integer;
  MenuItem: TMenuItem;
begin
  pmMapLayers.Items.Clear;
  if Assigned(FDefaultLayers) then
    for Index := 0 to Pred(FDefaultLayers.Count) do
      begin
        MenuItem := TMenuItem.Create(pmMapLayers);
        MenuItem.Tag := NativeInt(FDefaultLayers[Index]);
        MenuItem.Caption := FDefaultLayers[Index].Name;
        MenuItem.OnClick := DefaultMapLayerClick;
        pmMapLayers.Items.Add(MenuItem);
      end;
  if pmMapLayers.Items.Count = 0 then
    begin
      MenuItem := TMenuItem.Create(pmMapLayers);
      MenuItem.Caption := GetTitle('_dA.emptylist');
      MenuItem.Enabled := False;
      pmMapLayers.Items.Add(MenuItem);
    end;
end;

procedure TForm_Da_Parameters.DefaultMapLayerClick(Sender: TObject);
var
  DefaultLayer, NewLayer: TMapLayer;
begin
  if Assigned(Sender) and (Sender is TMenuItem) then
    begin
      DefaultLayer := Pointer((Sender as TMenuItem).Tag);
      if Assigned(DefaultLayer) then
        begin
          NewLayer := FMapLayers.NewLayer(DefaultLayer);
          if Assigned(NewLayer) then
            begin
              ExpandLayerName(NewLayer);
              lbMapLayers.ItemIndex := lbMapLayers.Items.AddObject(NewLayer.Name, NewLayer);
              lbMapLayersClick(lbMapLayers);
              Change(Sender);
            end;
        end;
    end;
end;

{ TComboCoxHelper }

function TComboCoxHelper.GetMappedValue: Integer;
begin
  if ItemIndex = -1 then Result:= 0  //unassigned
                    else Result:= Integer(Items.Objects[ItemIndex]);
end;

procedure TComboCoxHelper.SetMappedValue(const ID: integer);
begin
  ItemIndex:= Items.IndexOfObject(TObject(ID));
end;

procedure TComboCoxHelper.MappedLoad(const aTableName: string);
var fID,R : Integer;
    Q : TDeDataset;
begin
  Items.Clear;
  try
    Metadata.CatalogsDB.Connected:= True;
    Q:= MetaData.CatalogsDB.CreateQuery(qtSelect);
    Q.Descr.Table:= aTableName;
    Q.Descr.AddSortField(fldName, sdAscending);
    Q.Open;
    for R:=0 to Pred(Q.RecordCount) do
      begin
        Q.RecNo:= R;
        fID:= Q.IntValueByNameDef(fldID);
        if fID = 0 then Items.InsertObject(0, GetTitle(Q.StringValueByName(fldName)), TObject(fID) )
                   else Items.AddObject( GetTitle(Q.StringValueByName(fldName)), TObject(fID) );
      end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('ConfigForm unit initialization ...');

finalization
  DebugLog('ConfigForm unit finalization ...');
{$ENDIF}

end.

