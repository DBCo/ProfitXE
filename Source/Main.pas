unit Main;

interface

// Некорректно работает сохранение файлов из TImageEdit
// Если запись только чтение, то выгружать файл тоже только чтение

uses Windows, System.Types, SysUtils, Messages, Classes, Controls, Graphics, Forms, StdCtrls,
  ComCtrls, ExtCtrls, Menus, Buttons, ToolWin, ActnList, DB, Contnrs, Spin,
  Dialogs, Actions, Vcl.ActnMan, Vcl.ActnCtrls, Vcl.PlatformDefaultStyleActnCtrls,
  FireDAC.UI.Intf, FireDAC.VCLUI.Wait, FireDAC.Stan.Intf, FireDAC.Comp.UI,
  {CommDrv,} DeTypes, DeMeta, MainMenu, BaseMainFormUnit, DataCacheUnit, DataUnit,
  DeMetadata, DeToolbars, DeVariable, DeParser;

const
  LastMessID = 1009;
  ProgressID = 1015;
  ConnectID = 1038;

type
  TCharUpCaseTable = array [Char] of Char;

  TDeMenuItem = class(TMenuItem)
  private
  public
    // constructor Create(AOwner: TComponent); override;
  end;

type
  TMForm = class(TBaseMainForm)
    SBar: TStatusBar;
    MMParameters: TMenuItem;
    MM_Da_Help: TMenuItem;
    MMFind: TMenuItem;
    MMAbout: TMenuItem;
    miCloseSession: TMenuItem;
    MMHomePage: TMenuItem;
    N4: TMenuItem;
    MMHelp: TMenuItem;
    BackMenu: TPopupMenu;
    ForwardMenu: TPopupMenu;
    MMLastMess: TMenuItem;
    MMRemoved: TMenuItem;
    MMGoBack: TMenuItem;
    MMGoForward: TMenuItem;
    MMMenu: TMenuItem;
    pmTask: TPopupMenu;
    Tray_Da_Open: TMenuItem;
    Item_Da_Parameters: TMenuItem;
    N2: TMenuItem;
    Item_Da_Exit: TMenuItem;
    act_Da_Create: TAction;
    MMExit: TMenuItem;
    CreatePopup: TPopupMenu;
    MM_Dl_Directories: TMenuItem;
    MMOpen: TMenuItem;
    MMMailAs: TMenuItem;
    N3: TMenuItem;
    MMShowCardArea: TMenuItem;
    act_Da_ShowCardArea: TAction;
    MM_Da_Access: TMenuItem;
    miDictDeliniter: TMenuItem;
    MM_Da_Dictionary: TMenuItem;
    miOptionsSeparator: TMenuItem;
    pmToolBars: TPopupMenu;
    alPanels: TActionList;
    MM_Dl_ToolBars: TMenuItem;
    Reports1: TMenuItem;
    pnlClientArea: TPanel;
    APanel: TPanel;
    MainPanel: TPanel;
    TopPanel2: TPanel;
    MainGridPanel: TPanel;
    DockPanelLeft: TPanel;
    DockSplitter: TSplitter;
    DockPanelRight: TPanel;
    N1: TMenuItem;
    N6: TMenuItem;
    MMRemember: TMenuItem;
    act_Da_Sorting: TAction;
    mmiEditBDeleteDelimiter: TMenuItem;
    miSettings: TMenuItem;
    actSettings3: TMenuItem;
    NewHelpSeparator: TMenuItem;
    Mm_Da_DataImport: TMenuItem;
    N5: TMenuItem;
    MM_Da_DataExport: TMenuItem;
    Undo1: TMenuItem;
    Save1: TMenuItem;
    BottomBarPanel: TControlBar;
    MM_Dl_CDirectories: TMenuItem;
    AttentionPanel: TPanel;
    tiTask: TTrayIcon;
    OpenDialog1: TOpenDialog;
    PanelScreen: TPanel;
    MMArchived: TMenuItem;
    pmMenuToolBars: TPopupMenu;
    AttentionLabel: TLabel;
    PanelTopTop: TPanel;
    PanelM: TPanel;
    PanelL: TPanel;
    L_dL_rEgistration: TLabel;
    Bevel: TBevel;
    PanelR: TPanel;
    Password_Ok: TSpeedButton;
    PanelC: TPanel;
    L_Df_User: TLabel;
    L_Df_Password: TLabel;
    LoginEdit: TEdit;
    PassEdit: TEdit;
    PanelLL: TPanel;
    lbSolutionValue: TLabel;
    L_Dt_Solution: TLabel;
    edSolution: TComboBox;
    FindPanel: TPanel;
    PanelL_F: TPanel;
    Panel6: TPanel;
    FindBox: TComboBox;
    FieldBox: TComboBox;
    PanelR_F: TPanel;
    L_Dl_FindText: TLabel;
    L_Dl_FindPath: TLabel;
    PanelLL_F: TPanel;
    Bevel1: TBevel;
    LblFind: TLabel;
    Do_Da_Find: TSpeedButton;
    SB7: TSpeedButton;
    FindPanelSep: TPanel;
    MM_Dt_DataSet: TMenuItem;
    Go_Da_Find: TAction;
    Go_Da_Forward: TAction;
    Go_Da_Back: TAction;
    Go_Da_MainMenu: TAction;
    Go_Da_Help: TAction;
    act_Da_MailAs: TAction;
    Go_Da_Report: TAction;
    Go_dA_HomePage: TAction;
    Go_Da_LastMess: TAction;
    Go_Da_About: TAction;
    Go_Da_ShowRemoved: TAction;
    Go_Da_UnregUser: TAction;
    Go_Da_Exit: TAction;
    Go_Da_ShowArchived: TAction;
    Go_dA_DataImport: TAction;
    Go_dA_DataExport: TAction;
    Go_Da_Remember: TAction;
    Go_Da_Settings: TAction;
    Go_Da_Parameters: TAction;
    MM_Da_Report: TMenuItem;
    tiStarup: TTimer;
    act_Da_Update: TAction;
    Go_Da_Update: TAction;
    MMUpdate: TMenuItem;
    Go_Da_SplitBy: TAction;
    Go_Da_Presentation: TAction;
    Go_Da_Order: TAction;
    Go_Da_View: TAction;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    LanguagePanel: TPanel;
    LanguageSelectPanel: TPanel;
    Panel1: TPanel;
    BarPanel: TControlBar;
    Go_Da_Consistency: TAction;

    procedure CommPortReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: Integer);
    procedure RestoreItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormClose2(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure DeIdle(Sender: TObject; var Done: Boolean);
    procedure Go_Da_FindExecute(Sender: TObject);
    procedure Go_Da_ExitExecute(Sender: TObject);
    procedure act_Da_UpdateExecute(Sender: TObject);
    procedure Do_UnregUser;
    procedure Go_Da_UnregUserExecute(Sender: TObject);
    procedure Go_Da_AboutExecute(Sender: TObject);
    procedure Go_Da_ParametersExecute(Sender: TObject);
    procedure Do_Da_FindClick(Sender: TObject);
    procedure Password_Show(const FullShow: Boolean = True);
    procedure Password_Hide;
    procedure AfterChangeHistory;
    procedure AfterRegUser(Sender: TObject);
    procedure GoStartWorkExecute(Sender: TObject);
    procedure FormLangPaint(Sender: TObject);
    procedure PassEditKeyPress(Sender: TObject; var Key: Char);
    procedure LoginEditKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    function LoginIn : Boolean;
    procedure Pass_OkClick(Sender: TObject);
    procedure Go_dA_HomePageExecute(Sender: TObject);
    procedure NewHelpClick(Sender: TObject);
    procedure Go_Da_HelpExecute(Sender: TObject);
    procedure Go_Da_BackExecute(Sender: TObject);
    procedure Go_Da_ForwardExecute(Sender: TObject);
    procedure HistoryClick(Sender: TObject);
    procedure Go_Da_MainMenuExecute(Sender: TObject);
    procedure Go_Da_LastMessExecute(Sender: TObject);
    procedure Go_Da_ShowRemovedExecute(Sender: TObject);
    procedure DockPanelLeftDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
    procedure DockPanelUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
    procedure DockPanelRightDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
    procedure DockPanelLeftGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure DockPanelLeftDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DockPanelRightDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DockPanelRightGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure SBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure SBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure edSolutionChange(Sender: TObject);
    procedure BackMenuPopup(Sender: TObject);
    procedure ForwardMenuPopup(Sender: TObject);
    procedure act_Da_MailAsExecute(Sender: TObject);
    procedure act_Da_ShowCardAreaExecute(Sender: TObject);
    procedure Go_Da_ReportExecute(Sender: TObject);
    procedure pmToolBarsPopup(Sender: TObject);
    procedure BarPanelGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure tb_AdditionalToolbarStartDock(Sender: TObject; var DragObject: TDragDockObject);
    procedure tb_AdditionalToolbarEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure Go_Da_SettingsExecute(Sender: TObject);
    procedure Go_Da_RememberExecute(Sender: TObject);
    procedure setToolBarStyle(aTB: TToolBar; aImageSize, aCaption: Integer);
    procedure MM_ViewClick(Sender: TObject);
    procedure tbAllCommandsGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure tbAllCommandsDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tbAllCommandsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BackBtnStartDock(Sender: TObject; var DragObject: TDragDockObject);
    procedure BackBtnEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure tbAllCommandsResize(Sender: TObject);
    procedure MM_dTgoDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure MM_dTgoClick(Sender: TObject);
    procedure Go_dA_DataExportExecute(Sender: TObject);
    procedure Go_dA_DataImportExecute(Sender: TObject);
    procedure UpdateToolBars(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FindBoxKeyPress(Sender: TObject; var Key: Char);
    procedure FindBoxChange(Sender: TObject);
    procedure edSolutionKeyPress(Sender: TObject; var Key: Char);
    procedure MM_Da_ActionsClick(Sender: TObject);
    procedure Go_Da_ShowArchivedExecute(Sender: TObject);
    procedure pmMenuToolBarsPopup(Sender: TObject);
    procedure PanelTopTopResize(Sender: TObject);
    procedure FindPanelResize(Sender: TObject);
    procedure tiStarupTimer(Sender: TObject);
    procedure LoginOrPassEditChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Go_Da_SplitByExecute(Sender: TObject);
    procedure Go_Da_PresentationExecute(Sender: TObject);
    procedure Go_Da_OrderExecute(Sender: TObject);
    procedure Go_Da_ViewExecute(Sender: TObject);
    procedure LangButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MM_Da_HelpClick(Sender: TObject);
    procedure Go_Da_ConsistencyExecute(Sender: TObject);
  private
    FCtrlPress: Boolean;
    FAltPress: Boolean;
    FDefaultLogin: Boolean;
    FStatusItem: TStatusItem;
    FLastMessageForm: TForm;
    FWindowState: TWindowState;
    FOldBeforeSyncronize: TSyncronizeEvent;
    FSwapMenuList: TList;
    FCommandLineParameters: TDeVariableList;
    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); // message WM_GETMINMAXINFO;
    Procedure LastFocus(var Mess: TMessage); message WM_NCACTIVATE;
    procedure DeShowHint(Sender: TObject);
    // procedure DoMetaPrepare(ID:Integer);
    // procedure DoMetaWait;
    procedure LoadSolutionsList;
    procedure OnAddClick(Sender: TObject);
    procedure OnMoveToClick(Sender: TObject);
    procedure OnDatasetClick(Sender: TObject);
    procedure OnDatasetExecute(Sender: TObject);

    procedure OnDictionaryClick(Sender: TObject);
    procedure OnRightsClick(Sender: TObject);
    procedure CreateDirectoriesMenu;

    procedure UpdateCreateMenu(ParentItem: TMenuItem);
    procedure StoreEnvironment(aHistoryData: TContextMeta);
    procedure MainBeforeSyncronize(var aCacheItem: TCacheItem; AIndex : Integer);
    procedure LoadHotKeys;
    procedure SaveToolBars;
    procedure SaveHotKeys;
    procedure CreateToolBarsMenu(AMenuItem: TMenuItem);
    procedure ToolBarMenuClick(Sender: TObject);
    procedure ApplicationMinimize(Sender: TObject);
    procedure ApplicationRestore(Sender: TObject);
    procedure BuildGlobalFilterMenu;
    procedure CheckGlobalFilter(Sender: TObject);
    procedure ClearSwapMenu;
    procedure ReAlignSwapMenu(M: TMainMenu);
    procedure BuildSwapMenu;
    procedure MetaMenuItemClick(Sender: TObject);
    procedure ClearMainMenuList;
    procedure BuildMainMenuList;
    procedure ClearDatasetsList;
    procedure BuildDatasetsList;
    function GetCommandLineIdentType(const aIdent: string; var aType: TFieldType; aDataObject: pointer): Boolean;
    function GetCommandLineIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
    function ExecuteCommandLineActions: Boolean;
    procedure SetIsDefaultLogin(aValue: Boolean);
    property IsDefaultLogin: Boolean read FDefaultLogin write SetIsDefaultLogin;
  protected
    procedure InitForm; override;
    procedure Afterinit; override;
    procedure DoAfterBaseFormsChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoData(Sender: TObject; aContext: TContextMeta);

    procedure NewUserMessage;
    procedure StartEditToolBars;
    procedure EndEditToolBars;
    function CreateButton(Source: TAction): TDeToolButton;
    procedure InitToolButton(B: TDeToolButton);
    function CreateButtonByName(const aName: string): TDeToolButton;
    function CreateSeparator: TDeToolButton;
    procedure WMDESTROY1(var Msg: TMessage); message WM_QUERYENDSESSION;
    // WM_DESTROY;
    procedure WMDESTROY2(var Msg: TMessage); message WM_ENDSESSION;
    // WM_SYSCOMMAND;
    function GetHintPosition: TRect;
    // procedure WMICON (var msg: TMessage);    message DM_ICONNOTIFY;
    procedure WMINFO(var Msg: TMessage); message DM_INFONOTIFY;
    procedure WMINFO2(var Msg: TMessage); message DM_INFONOTIFY2;
    procedure WMERROR(var Msg: TMessage); message DM_ERRORNOTIFY;
    procedure WMSTATUS(var Msg: TMessage); message DM_STATUSNOTIFY;
    procedure WMSTATUS2(var Msg: TMessage); message DM_STATUS2NOTIFY;
    procedure WMATTENTION(var Msg: TMessage); message DM_ATTENTION;
    procedure WMACTION(var Msg: TMessage); message DM_ACTIONNOTIFY;
    procedure WMEXTSENDMESSAGE(var Msg: TMessage); message DM_EXTSENDMESSAGE;
    procedure WMCloseSession(var Msg: TMessage); message DM_CLOSESESSION;
    procedure WMFavoritesUpdate(var Msg: TMessage); message DM_FAVORITES;
    procedure WMPROGRESSNOTIFY(var Msg: TMessage); message DM_PROGRESSNOTIFY;
    procedure WHCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
  end;

  // ------------------------------------------------------------------------------
var
  MForm: TMForm;
  MainMenuForm: TForm_Da_MainMenu;
  CharUpCaseTable: TCharUpCaseTable;

  flagPaste: Boolean = False;
  flagInfo: Boolean = True;
  flagIgnoreActive: Boolean = False;
  LastSearchArea: TSearchArea;
  _AnimStart: Integer = 1;

  DeHistory: THistoryList;
  RECYCLER: TObjectList;

implementation

uses Variants, Math, ImgList, ShellAPI,
  DeLog, Funcs, Security, Dictionary, About, DSMeta, ConnectOperationsUnit,
  DataManager, BaseGridFormUnit, DeDB, DeSettings, BaseDataFormUnit, ConfigForm,
  UnitA, DeReport {, DePrintDlg} , TBConfigForm, HintForm, StructureUnit,
  {FileMonitorThreadUnit,} DeActions, LogoForm, DeMasterFormUnit, BaseFormUnit,
  ActionUnit, DeCommandDlg, DeCalculator, DeScript, DeFunctions,
  ChartFormUnit, DeFilters;

{$R *.DFM}
{ .$R Images.res }

function MakeHint(const aName, aValue: string): string;
begin
  if Length(aValue) > 0 then Result:= #10 + GetTitle(aName) + ': ' + aValue
                        else Result:= EmptyStr;
end;

// ==============================================================================

constructor TMForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DM.MapActionListIcons(MainActionList);
  TopPanel2.Height := 0;
end;

procedure TMForm.DockPanelLeftGetSiteInfo(Sender: TObject; DockClient: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin // Зона реакции мыши
  InfluenceRect.TopLeft := APanel.ClientToScreen(Point(0, 0));
  InfluenceRect.BottomRight := APanel.ClientToScreen(Point(64, APanel.Height));
  CanDock := (DockClient = MainMenuForm);
end;

// ------------------------------------------------------------------------------
procedure TMForm.DockPanelRightGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin // Зона реакции мыши
  InfluenceRect.TopLeft := APanel.ClientToScreen
    (Point(APanel.ClientWidth - 64, 0));
  InfluenceRect.BottomRight := APanel.ClientToScreen
    (Point(APanel.Width, APanel.Height));
  CanDock := (DockClient = MainMenuForm);
end;

// ------------------------------------------------------------------------------
procedure TMForm.DockPanelLeftDockOver(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  R: TRect;
begin // Будущая зона дока окна
  R.TopLeft := APanel.ClientToScreen(Point(0, 0));
  R.BottomRight := APanel.ClientToScreen(Point(Source.Control.Width,
    APanel.Height));
  Source.DockRect := R;
  Accept := (Source.Control = MainMenuForm);
end;

// ------------------------------------------------------------------------------
procedure TMForm.DockPanelRightDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  R: TRect;
begin // Будущая зона дока окна
  R.TopLeft := APanel.ClientToScreen
    (Point(APanel.ClientWidth - Source.Control.Width, 0));
  R.BottomRight := APanel.ClientToScreen(Point(APanel.Width, APanel.Height));
  Source.DockRect := R;
  Accept := (Source.Control = MainMenuForm);
end;

// ------------------------------------------------------------------------------
procedure TMForm.DockPanelLeftDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
begin
  APanel.DisableAlign;

  DockPanelLeft.Width := (Source.DockRect.Right - Source.DockRect.Left);
  DockPanelLeft.Align := alLeft;
  DockSplitter.Align := alLeft;

  APanel.EnableAlign;
  APanel.Invalidate;
end;

// ------------------------------------------------------------------------------
procedure TMForm.DockPanelRightDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
begin
  APanel.DisableAlign;

  DockPanelRight.Width := (Source.DockRect.Right - Source.DockRect.Left);
  DockPanelRight.Align := alRight;
  DockSplitter.Align := alRight;

  APanel.EnableAlign;
  APanel.Invalidate;
end;

// ------------------------------------------------------------------------------
procedure TMForm.DockPanelUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
begin
  inherited;
  if TWinControl(Sender).DockClientCount < 2 then
  begin
    TWinControl(Sender).Align := alNone;
    TWinControl(Sender).Height := 0;
    DockSplitter.Align := alNone;
    DockSplitter.Left := MaxInt;
  end;
end;

// ==============================================================================
procedure TMForm.NewUserMessage;
begin
  Go_Da_LastMess.Enabled := True;

  if DeStatus.StatusItemByID(LastMessID) = nil then
    DeStatus.Add(LastMessID, 1139 { LastMessID mod 100 } , '_Da.LastMess', Go_Da_LastMessExecute);
end;

function FindTaskControl(Tray: hWnd; aName: String): hWnd;
var
  Child: hWnd;
  C: Array [0 .. 127] of Char;
begin
  Result := 0;
  Child := GetWindow(Tray, GW_CHILD);
  while Child <> 0 do
  begin
    If GetClassName(Child, C, SizeOf(C)) > 0 then
    begin
      if UpperCase(StrPAS(C)) = aName then
        Result := Child
      else
        Result := FindTaskControl(Child, aName);

      if Result > 0 then
        Break;
    end;

    Child := GetWindow(Child, GW_HWNDNEXT);
  end;
end;

function TMForm.GetHintPosition: TRect;
// var
// hTaskBar, hControl: hWnd;
begin
  Result.TopLeft := SBar.ClientToScreen(Point(4 + 16 + 8, 4));
  Result.BottomRight := Result.TopLeft;

  {
    if (Not Application.MainForm.Active) then
    begin
    hTaskBar:= FindWindow('Shell_TrayWnd', nil);
    hControl:=FindTaskControl(hTaskBar,'TRAYCLOCKWCLASS');
    if hControl<=0 then
    hControl:=FindTaskControl(hTaskBar,'MSTASKSWWCLASS');
    if hControl<=0 then
    hControl:=FindTaskControl(hTaskBar,'TRAYNOTIFYWND');
    if hControl>0 then
    GetWindowRect(hControl, Result);
    end;
    { }
end;

procedure TMForm.WMINFO(var Msg: TMessage);
var
  TR: TRect;
  DMes: TDeMessage;
  MsgText: string;
  IsDouble: Boolean;
begin
  SBar.Update;
  TR := GetHintPosition;

  if Msg.LParam = 0 then
    MsgText := ErrorMessage
  else
    MsgText := PChar(Msg.LParam);

  if Length(MsgText) = 0 then Exit;

  IsDouble := False;
  try
    if (DeMessages.Count > 0) and (DeMessages[Pred(DeMessages.Count)].Text = MsgText) and Assigned(FLastMessageForm) then
      IsDouble := FLastMessageForm.Visible;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog(ClassName + '.WMINFO error: ' + E.Message);
    {$ENDIF}
  end;

  NewUserMessage;

  if IsDouble then
    begin
      DMes := DeMessages[Pred(DeMessages.Count)];
      DMes.Count := Succ(DMes.Count);
      TfrmHint(FLastMessageForm).CaptionLabel.Caption := IntToStr(DMes.Count) + ' * ' + GetTitle(DMes.Caption);
      FLastMessageForm.Repaint;
    end
  else
    begin
      DMes := DeMessages.AddMessage(Msg.WParam, '_Dl.Information', MsgText, mtInformation);
      FLastMessageForm := ShowHintWindow(TR, GetTitle(DMes.Text), DMes.Caption, DMes.Ico, DMes.MsgType);
    end;
end;

procedure TMForm.WMINFO2(var Msg: TMessage);
var
  TR: TRect;
  DMes: TDeMessage;
  MsgText: string;
  IsDouble: Boolean;
begin
  SBar.Update;

  if (Msg.WParamLo > 0) and (Msg.WParamHi > 0) then
    begin
      TR.Top := Msg.WParamHi;
      TR.Left := Msg.WParamLo;
      TR.BottomRight := TR.TopLeft;
    end
  else
    TR := GetHintPosition;

  if Msg.LParam = 0 then
    MsgText := ErrorMessage
  else
    MsgText := PChar(Msg.LParam);

  if Length(MsgText) = 0 then Exit;

  IsDouble := False;
  try
    if (DeMessages.Count > 0) and (DeMessages[Pred(DeMessages.Count)].Text = MsgText) and Assigned(FLastMessageForm) then
      IsDouble := FLastMessageForm.Visible;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog(ClassName + '.WMINFO error: ' + E.Message);
    {$ENDIF}
  end;

  NewUserMessage;

  if IsDouble then
  begin
    DMes := DeMessages[Pred(DeMessages.Count)];
    DMes.Count := Succ(DMes.Count);
    TfrmHint(FLastMessageForm).CaptionLabel.Caption := IntToStr(DMes.Count) + ' * ' + GetTitle(DMes.Caption);
    FLastMessageForm.Repaint;
  end
  else
  begin
    DMes := DeMessages.AddMessage(icoInfo, '_Dl.Information', MsgText, mtInformation);
    FLastMessageForm := ShowHintWindow(TR, GetTitle(DMes.Text), DMes.Caption, DMes.Ico, DMes.MsgType);
  end;
end;

// ..............................................................................
procedure TMForm.WMERROR(var Msg: TMessage);
var
  TR: TRect;
  DMes: TDeMessage;
  MsgText: string;
begin
  SBar.Update;
  TR := GetHintPosition;

  if Msg.LParam = 0 then
    MsgText := ErrorMessage
  else
    MsgText := PChar(Msg.LParam);

  Funcs.WriteLog('wmERROR - ' + MsgText);

  DMes := DeMessages.AddMessage(icoError, '_De.Error', MsgText, mtError);

  NewUserMessage;

  ShowHintWindow(TR, GetTitle(DMes.Text), DMes.Caption, DMes.Ico, DMes.MsgType);
end;
// ..............................................................................

procedure TMForm.WMPROGRESSNOTIFY(var Msg: TMessage);
begin
  if Assigned(FStatusItem) then
    if Msg.LParam > 0 then
    begin
      // Если ещё нет уровней прогресс бара, то ...
      if not Assigned(FStatusItem.Last) then
      begin
        SBar.Panels[0].Width := 128 + 256;
        SBar.Panels[1].Width := 0;
      end;
      // Создадим новый уровень прогресс база и укажем количество шагов и сам шаг ...
      FStatusItem.BeginProgress(Msg.WParam, Msg.LParam);
    end

    else if Msg.LParam = 0 then
    begin
      FStatusItem.Progress := Msg.WParam;
    end

    else if Msg.LParam = -1 then
    begin
      // Если это последний или больше нет уровней прогресс бара
      if not Assigned(FStatusItem.Last) then
      begin
        SBar.Panels[0].Width := 128;
        SBar.Panels[1].Width := 256;
      end
      else if FStatusItem.LevelCount = 1 then
      begin
        SBar.Panels[0].Width := 128;
        SBar.Panels[1].Width := 256;
      end;
      // Удаляем текущий уровень
      FStatusItem.EndProgress;
    end

    else if Msg.LParam = -2 then
    begin
      FStatusItem.Hint := PChar(Msg.WParam);
    end;
end;

// ==============================================================================
procedure TMForm.WMSTATUS(var Msg: TMessage);
var s: String;
begin
  if Msg.LParam = CurrentBaseForm.Handle then
    begin
      s:= GetTitle(PChar(Msg.WParam));
      if not (SBar.Panels[1].Text = s) then
        begin
          SBar.Panels[1].Text := s;
          SBar.Invalidate;
        end;
    end;
end;

procedure TMForm.WMSTATUS2(var Msg: TMessage);
begin
  if (Msg.LParam = 0) or (Msg.LParam = CurrentBaseForm.Handle) then
    if not (SBar.Panels[2].Text = GetTitle(PChar(Msg.WParam))) then
      begin
        SBar.Panels[2].Text := GetTitle(PChar(Msg.WParam));
        SBar.Invalidate;
      end;
end;

procedure TMForm.WMATTENTION(var Msg: TMessage);
var
  MsgText: string;
begin
  MsgText := PChar(Msg.LParam);

  AttentionPanel.Visible:= (0 < Length(MsgText));
  AttentionLabel.Caption:= MsgText;
end;

// ------------------------------------------------------------------------------
procedure TMForm.WMEXTSENDMESSAGE(var Msg: TMessage);
var
  s, a, b: string;
  i, p: Integer;
  TM: TTableMeta;
  Variables: TDeVariableList;
begin
  s := Trim(PChar(Msg.WParam));
  TM := nil;

  if AnsiCompareText(copy(s, 1, 6), 'Update') = 0 then
  begin
    TM := metadata.GetTableByName(Trim(copy(s, 7, MaxInt)));

    if Assigned(TM) then
    begin
      {
        for i:=0 to MetaData.DataSetsCount-1 do
        if MetaData.DataSets[i].Table=TM then
        MetaData.DataSets[i].Cache.Update(mcUpdate, null);
      }
      metadata.UpdateDataSets(TM.ID, mcUpdate, null);
      metadata.UpdateLibrary(TM.ID);
    end
    else

      if Assigned(ActiveMeta) then
    begin
      ActiveMeta.Cache.Update(mcUpdate, null);
      metadata.UpdateLibrary(ActiveMeta.Table.ID);
    end;

    for i := 0 to BaseFormsCount - 1 do
      if BaseForms[i].DataSetMeta.Table = TM then
        BaseForms[i].DataSetMeta.UpdateVisualizationForm;
  end;

  if AnsiCompareText(copy(s, 1, 5), 'Open ') = 0 then
  begin
    s := Trim(copy(s, 6, MaxInt));
    p := Pos(' ', s);
    if p > 0 then
    begin
      a := Trim(copy(s, 1, p));
      b := Trim(copy(s, p, MaxInt));
      TM := metadata.GetTableByNameAndID(a, nil, StrToIntDef(a, -1));
      if Assigned(TM) then
        EditRecord(TM, StrToIntDef(b, -1));
    end;

    if Assigned(TM) then
    begin
      for i := 0 to metadata.LibraryCount - 1 do
        if metadata.LibraryData[i].TableMeta = TM then
          metadata.LibraryData[i].Update(mcUpdate, null);
      {
        for i:=0 to MetaData.DataSetsCount-1 do
        if  MetaData.DataSets[i].Table = TM then
        MetaData.DataSets[i].Cache.Update(mcUpdate,  null);
      }
      metadata.UpdateDataSets(TM.ID, mcUpdate, null);
    end;
  end;

  if SameText(copy(s, 1, 5), 'Exec ') and Assigned(ProfitActions) then
  begin
    s := Trim(copy(s, 6, MaxInt));
    p := Pos(' ', s);
    if p <> 0 then
    begin
      a := copy(s, 1, Pred(p));
      s := Trim(copy(s, Succ(p), Length(s)));
      if (Copy(s,1,1)='(') and (Copy(s,Length(s),1)=')') then s:=Copy(s,2,Length(s)-2);

    end
    else
    begin
      a := s;
      s := EmptyStr;
    end;
    i := ProfitActions.IndexByOriginal(a);
    if i <> -1 then
    begin
      Variables := TDeVariableList.Create;
      try
        while Length(s) <> 0 do
        begin
          p := Pos('=', s);
          if p = 0 then
          begin
            Variables.SetByName(s, Unassigned);
            s := EmptyStr;
          end
          else
          begin
            a := Trim(copy(s, 1, Pred(p)));
            if copy(a,Length(a),1)=':' then Delete(a,Length(a),1); // отождествляем "=" и ":="

            s := Trim(copy(s, Succ(p), Length(s)));
            p := Pos(',', s);
            if p = 0 then
            begin
              Variables.SetByName(a, s);
              s := EmptyStr;
            end
            else
            begin
              b := Trim(copy(s, 1, Pred(p)));
              s := Trim(copy(s, Succ(p), Length(s)));
              Variables.SetByName(a, b);
            end;
          end;
        end;
        ProfitActions[i].Execute(Variables, False);
      finally
        Variables.Free;
      end;
    end;
  end;
end;


procedure TMForm.WHCopyData(var Msg: TWMCopyData);
begin
  if Assigned(Msg.CopyDataStruct) then
    case Msg.CopyDataStruct^.dwData of
      DM_EXTSENDMESSAGE: { Нужно вызвать событие DM_EXTSENDMESSAGE }
        SendMessage(Handle, DM_EXTSENDMESSAGE, NativeInt(Msg.CopyDataStruct^.lpData), 0);
      DM_INFONOTIFY: { Нужно вызвать событие DM_INFONOTIFY }
        SendMessage(Handle, DM_INFONOTIFY, 0, NativeInt(Msg.CopyDataStruct^.lpData));
      DM_ERRORNOTIFY: { Нужно вызвать событие DM_ERRORNOTIFY }
        SendMessage(Handle, DM_ERRORNOTIFY, 0, NativeInt(Msg.CopyDataStruct^.lpData));
      DM_PROGRESSNOTIFY: { Нужно вызвать событие DM_PROGRESSNOTIFY }
        SendMessage(Handle, DM_PROGRESSNOTIFY, NativeInt(Msg.CopyDataStruct^.lpData), -2);
      DM_STATUS2NOTIFY: { Нужно вызвать событие DM_STATUS2NOTIFY }
        SendMessage(Handle, DM_STATUS2NOTIFY, 0, NativeInt(Msg.CopyDataStruct^.lpData));
    end;
end;

// ------------------------------------------------------------------------------
procedure TMForm.WMCloseSession(var Msg: TMessage);
begin
  Do_UnregUser;
  Password_Show;
end;

// ------------------------------------------------------------------------------

procedure TMForm.WMACTION(var Msg: TMessage);
begin
  case Msg.WParam of
    DM_SHOWREMOVED:
      Go_Da_ShowRemoved.Checked := (Msg.LParam = 1);
    DM_SHOWARCHIVED:
      Go_Da_ShowArchived.Checked := (Msg.LParam = 1);
  end;
end;

// ------------------------------------------------------------------------------
procedure TMForm.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
  inherited;

  if WindowState = wsNormal then
    with Msg.MinMaxInfo^ do
    begin
      ptMaxTrackSize.X := Screen.Width;
      ptMaxTrackSize.Y := Screen.Height;
      if ptMaxTrackSize.X >= ptMaxTrackSize.Y then
        begin
          ptMinTrackSize.X := 480;
          ptMinTrackSize.Y := 360;
        end
      else
        begin
          ptMinTrackSize.X := 360;
          ptMinTrackSize.Y := 480;
        end;

    end;
end;

procedure TMForm.WMFavoritesUpdate(var Msg: TMessage);
begin
  if Assigned(MainMenuForm) then
    SendMessage(MainMenuForm.Handle, DM_FAVORITES, Msg.WParam, Msg.LParam);
end;

// ------------------------------------------------------------------------------
procedure TMForm.DeShowHint(Sender: TObject);
var
  s: String;
begin
  s := GetTitle(Application.Hint);
  s := StringReplace(s, #10, ' ', [rfReplaceAll]);
  s := StringReplace(s, #13, ' ', [rfReplaceAll]);
  SBar.Panels[2].Text := sp + s;
end;

// ==============================================================================
procedure TMForm.FormLangPaint(Sender: TObject);
begin
  Application.Title := dicAppFullName;
  tiTask.Hint := Application.Title;
  // TrayIcon(tioUpdate, Application.Handle, DM_ICONNOTIFY, 'MAINICON', Application.Title);
  if Assigned(UserSession) then Caption := dicAppName + ' - [ ' + UserSession.Name + ' ]'
                           else Caption := dicAppName + ' - [ * ]';

  LangFormUpdate(Self, Caption);

  if MainMenuForm <> nil then
    LangFormUpdate(MainMenuForm);
  metadata.UpdateCardForms;
end;

// ==============================================================================
procedure TMForm.CommPortReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: Integer);
var
  s: string;
  i, bcField: Integer;
  sDigit: Boolean;
begin
  RestoreItemClick(Sender);

  for i := 0 to DataSize - 2 do
    s := s + Chr(Byte((Pointer(NativeInt(DataPtr) + i))^));

  // ............................................................................
  if not Assigned(UserSession) then
  begin
    SendMessage(Handle, DM_INFONOTIFY, icoError, NativeInt(PChar('_Error.Start' + #10 + '_RegistrationText')));
    Exit;
  end;
  sDigit := isDigit(s);

  if Screen.ActiveControl is TEdit then
  begin
    TEdit(Screen.ActiveControl).Text := s;
    Exit;
  end;
  if (Screen.ActiveControl is TComboBox) and
    (TComboBox(Screen.ActiveControl).Style = csDropDown) then
  begin
    TComboBox(Screen.ActiveControl).Text := s;
    Exit;
  end;
  if (Screen.ActiveControl is TSpinEdit) and (sDigit) then
  begin
    TSpinEdit(Screen.ActiveControl).Text := s;
    Exit;
  end;
  // ............................................................................
  bcField := -1;
  for i := 0 to ActiveMeta.Cache.Fields.Count - 1 do
    if ActiveMeta.Cache.Fields[i].ShowType = stBarCode then
      bcField := i;

  if bcField > -1 then
  begin
    for i := 0 to ActiveMeta.Cache.Count - 1 do
      if ActiveMeta.Cache[i].FieldText(bcField) = s then
      begin
        ActiveMeta.Cache.ClearSelection;
        ActiveMeta.Cache[i].SetFocus;
        ActiveMeta.Cache.ItemSelect(i);
        Exit;
      end;
    SendMessage(Handle, DM_INFONOTIFY, 31, NativeInt(PChar('_De.barcode')));
    Exit;
  end;
  // ............................................................................
  if Assigned(UserSession) and (DefaultMeta.Table.ID = 16) then
  begin
    if (Screen.ActiveControl is TEdit) then
    begin
      TEdit(Screen.ActiveControl).Text := s;
      Exit
    end;
    if (Screen.ActiveControl is TComboBox) then
    begin
      TComboBox(Screen.ActiveControl).Text := s;
      Exit
    end;
  end;
  // ............................................................................
  SendMessage(Handle, DM_INFONOTIFY, 31, NativeInt(PChar('_De.barcode')));
  // ............................................................................
end;

/// /////////////////////////////////////////////////////////////////////////////
procedure TMForm.WMDESTROY1(var Msg: TMessage);
begin
  Msg.Result := 1;
  onClose := nil;
  Close;
end;

// ------------------------------------------------------------------------------
procedure TMForm.WMDESTROY2(var Msg: TMessage);
begin
  Msg.Result := 0;
  onClose := nil;
  Close;
end;

// ------------------------------------------------------------------------------
{
  procedure TMForm.WMICON(var msg: TMessage);
  var P : TPoint;
  begin
  case msg.LParam of     // обработка обратных сообщений по нажатию клавиши
  WM_RBUTTONUP: begin GetCursorPos(p);
  //   SetForegroundWindow(Application.Handle);
  SendMessage(Application.Handle, WM_ACTIVATEAPP, 0, 0);
  SendMessage(Application.Handle, WM_SETFOCUS, 0, 0);
  PopupMenu1.Popup(P.X, P.Y);
  end;
  WM_LBUTTONUP: begin RestoreItemClick(Self);
  end;
  end;
  end; }
// ------------------------------------------------------------------------------
procedure TMForm.RestoreItemClick(Sender: TObject);
begin
  Application.ShowMainForm := True;
  ShowWindow(Application.Handle, SW_SHOW);
  ShowWindow(Application.MainForm.Handle, SW_SHOW);

  SendMessage(Application.Handle, wm_SysCommand, SC_RESTORE, 0);
  SendMessage(Application.Handle, wm_ACTIVATEAPP, 1, 0);

  SetForegroundWindow(Application.Handle);
  Application.BringToFront;
end;

// ------------------------------------------------------------------------------
procedure TMForm.FormActivate(Sender: TObject);
var H: Integer;
begin
  inherited;
  H:= Max(edSolution.Height, LoginEdit.Height);
  case H mod 3 of
    0: begin YStep:= H div 3 + 1; YSpace:= 3; end;
    1: begin YStep:= H div 3 + 1; YSpace:= 1; end;
    2: begin YStep:= H div 3 + 1; YSpace:= 2; end;
  end;
end;

procedure TMForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FWindowState := Application.MainForm.WindowState;

  onResize := nil;
  if Assigned(MainMenuForm) then
    MainMenuForm.SavePosition;

  // Минимизация вместо закрытия, если это не выключение компа
  if ((Application.ShowMainForm) and (Variables.AsBoolean[RegTray])) then
  begin
    Action := caNone;

    Application.ShowMainForm := False;
    ShowWindow(Application.Handle, SW_HIDE);
    ShowWindow(Application.MainForm.Handle, SW_HIDE);
  end
  else
  begin
    {
      if Assigned(FileManager)and(FileManager.Count>0) then
      if MessageBox(Handle,
      'Имеются открытые файлы, которые используются до сих пор.'+
      ' При выходе изменения сделанные в этих файлах будут утеряны.'+
      ' Все равно выйти?', PChar(dicAppName),
      MB_YESNO or MB_ICONWARNING)<>IDYES
      then
      begin
      Action := caNone;
      Exit;
      end
      else
      FileManager.AskBeforeTerminate := False;
      //для того,чтобы при вызове Go_UnregUserExecute не спрашивать повторно
    }
//    SaveToolBars;
    Do_UnregUser;
    Password_Show(False);
    FreeAndNil(GlobalActionConditionList);
    // после Logout'a пользователя, чтобы его отработать
  end;
end;

// ------------------------------------------------------------------------------
procedure TMForm.FormClose2(Sender: TObject; var Action: TCloseAction);
begin
  onResize := nil;
  SaveToolBars;
  Do_UnregUser;
  Password_Show(False);

  if Assigned(MainMenuForm) then
    MainMenuForm.SavePosition;
  FreeAndNil(GlobalActionConditionList);
  // после Logout'a пользователя, чтобы его отработать

  {
    Application.ShowMainForm := False;
    ShowWindow(Application.Handle, SW_HIDE);
    ShowWindow(Application.MainForm.Handle, SW_HIDE);
    { }
end;

// ------------------------------------------------------------------------------
procedure TMForm.DeIdle(Sender: TObject; var Done: Boolean);
begin
{$IFDEF DeDEBUG}
  Funcs.WriteLog('Idle start');
{$ENDIF}
  { try
    RECYCLER.Clear;
    finally
    end;
    Application.OnIdle:=nil;
  }
{$IFDEF DeDEBUG}
  Funcs.WriteLog('Idle end');
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TMForm.FormDestroy(Sender: TObject);
begin
  tiTask.Visible := False;
  FreeAndNil(FSwapMenuList);
  // TrayIcon( tioDelete, Self.Handle, DM_ICONNOTIFY, EmptyStr,EmptyStr);
  // try
  RECYCLER.Clear;
  // finally
  // end;
end;
// ------------------------------------------------------------------------------
procedure TMForm.DoData(Sender: TObject; aContext: TContextMeta);
var
  OldDataSetMeta: TDataSetMeta;
  OldGridForm, MainGrid: TBaseGridForm;
  Table: TTableMeta;
  FShowArea: Boolean;
begin
  if Not ( SecuritySystem.CheckPolicyDataSet(aContext.DatasetId, spSelect) and
           SecuritySystem.CheckPolicyMenu(aContext.ID, spSelect) ) then
    begin
      if Sender is TControl then ShowHintWindow(TControl(Sender), '_eRror.right', '_dE.error', icoError, mtError)
                            else ShowHintWindow(nil, '_eRror.right', '_dE.error', icoError, mtError);
      Exit;
    end;

{$IFDEF DeDEBUG}
  if aContext.Name <> EmptyStr then
    Funcs.WriteLog('Start open ' + aContext.Name);
{$ENDIF}

  OldDataSetMeta:= DefaultMeta;
  PanelScreen.Visible:= not Assigned(OldDataSetMeta);
  // PanelScreen нужна только для "прикрытия" построения первой формы

  FShowArea:= Variables.AsBoolean[RegShowCardArea];
  if Assigned(OldDataSetMeta) then
    begin
      // Закрываем карточку, .т.к. может потребоваться подтверждения сохранения изменений при при наличии фокуса
      if OldDataSetMeta.GridForm.HandleAllocated  then
        if OldDataSetMeta.GridForm is TBaseGridForm then
          begin
            FShowArea:= TBaseGridForm(OldDataSetMeta.GridForm).ViewArea;
            if TBaseGridForm(OldDataSetMeta.GridForm).ViewArea then
              if not TBaseGridForm(OldDataSetMeta.GridForm).HideViewArea(False) then Exit;
          end;

      if assigned(OldDataSetMeta.CardForm) then
        OldDataSetMeta.CardForm.Enabled:= False;

      if Assigned(OldDataSetMeta.GridForm) then
        OldDataSetMeta.GridForm.SendToBack;
    end;

  if (aContext <> DeHistory.CurrentItem) and (DeHistory.IndexOf(aContext) < 0) then
    begin
      StoreEnvironment(DeHistory.CurrentItem);  //  запоминаем данные группировок
      DeHistory.NewItem.Assign(aContext);       //  создаем новую запись для History
      if (DeHistory.CurrentIndex = -1) or (DeHistory.CurrentIndex = (DeHistory.Count - 2)) then
        DeHistory.CurrentIndex := Pred(DeHistory.Count)
      else
        begin
          DeHistory.Move(Pred(DeHistory.Count), Succ(DeHistory.CurrentIndex));
          DeHistory.CurrentIndex := Succ(DeHistory.CurrentIndex);
        end;
    end;

  LastErrorDBType := dtNone;

  if Go_Da_Find.Checked then
    Go_Da_FindExecute(Sender);

  Table:= aContext.Dataset;

  if Assigned(Table) then
    begin
      Application.Title:= GetTitle(aContext.Dataset.Name, ttSecondName) + ' - ' + dicAppName;

      DefaultMeta:= TDataSetMeta.Create(utForm, aContext);
      FOldBeforeSyncronize:= DefaultMeta.Cache.OnBeforeSyncronize;
      DefaultMeta.Cache.OnBeforeSyncronize:= MainBeforeSyncronize;
      DefaultMeta.Context.ViewType:= aContext.ViewType;
      // DoMetaWait;

      DefaultMeta.Cache.PrepareData(False);  // похоже операция лишняя, поскольку  PrepareData вызывается при
      CreateDirectoriesMenu;                 // MainGrid.ReInitForm(DefaultMeta);  десятью строками ниже    777777777777

      MainGrid:= TBaseGridForm(ViewTypeToClass(DefaultMeta.Context.ViewType).Create(Application));
      MainGrid.ManualDock(MainGridPanel);
      // пытаемся уменьшить количество срабатываний onResize и задаем будущий размер для процедура Init
      MainGrid.SetBounds(0, 0, MainGridPanel.ClientWidth, MainGridPanel.ClientHeight);

      MainGrid.Align:= alClient;
      MainGrid.ReInitForm(DefaultMeta);
      MainGrid.SendToBack;
      SendMessage(MainGrid.Handle, WM_BF_CHANGEACTIVE, 1, 0);
      if MainGrid.ViewParams.TestByName('ShowCard','False') then FShowArea:= False else
      if MainGrid.ViewParams.TestByName('ShowCard','True') then  FShowArea:= MainGrid.CanShowCard else
                                                                 FShowArea:= FShowArea and MainGrid.CanShowCard;
      act_Da_ShowCardArea.Checked:= FShowArea;
      MainGrid.Show;
      if PanelTopTop.Visible then
        begin
          PanelTopTop.Visible := False;
          Refresh;
        end;
      if FShowArea then MainGrid.ShowViewArea(False);
    end
  else
    begin
      Application.Title:= dicAppFullName;
      DefaultMeta:= nil;
      CreateDirectoriesMenu;
      MainGrid:= nil;
    end;

  if Assigned(OldDataSetMeta) then
    begin
      if Assigned(OldDataSetMeta.GridForm) then
        begin
         // Запоминаем, т.к. в DeInitForm связь рвется
         OldGridForm:= TBaseGridForm(OldDataSetMeta.GridForm);
         OldGridForm.DeInitForm;
         OldGridForm.Free;
        end;

      OldDataSetMeta.DestroyRecursive;
    end;

  //перед последним изменением фокуса, чтобы меню было наполнено
  UpdateCreateMenu(MM_Da_Create);
  UpdateCreateMenu(CreatePopup.Items);

  if Assigned(Table) and Assigned(MainGrid.MainControl) then
    MainGrid.MainControl.SetFocus;

  // сообщения об ошибках версий клиентов СУБД (Interbase, BDE)
  metadata.DisplayVersionError;
{$IFDEF DeDEBUG}
  if aContext.Name <> EmptyStr then
    Funcs.WriteLog('End open ' + aContext.Name);
{$ENDIF}
  AfterChangeHistory;
  act_Da_ShowCardArea.Enabled:= assigned(MainGrid) and Not MainGrid.IsMultiWIndow;
end;

procedure TMForm.MetaMenuItemClick(Sender: TObject);
begin
  if Assigned(Sender) and ((Sender is TMenuItem) or (Sender is TAction)) then
    if Sender is TAction then DoData(Self, TContextMeta((Sender as TAction).Tag))
                         else DoData(Self, TContextMeta((Sender as TMenuItem).Tag));
end;
// ------------------------------------------------------------------------------
procedure TMForm.Go_Da_FindExecute(Sender: TObject);
var
  i, n, x1, x2: Integer;
  BackViewArea: Boolean;
  FName: string;
begin
  DisableAlign;
  BackViewArea := TBaseGridForm(DefaultMeta.GridForm).ViewArea;

  Go_Da_Find.Checked := not Go_Da_Find.Checked;
  if (Go_Da_Find.Checked) then
    begin
      if BackViewArea then TBaseGridForm(DefaultMeta.GridForm).HideViewArea(False);
      LblFind.Caption := GetTitle('_Dl.FindIn', ttSecondName)+ ' "' + GetTitle(DefaultMeta.Table.Name, ttSecondName) + '"';
      FieldBox.onDrawItem := DM.ComboBoxDrawItem;
      FieldBox.OnKeyPress := DM.ComboBoxKeyPress;

      while FieldBox.Items.Count > 0 do
        FieldBox.Items.Delete(0);
      n := 0;
      for i := 0 to DefaultMeta.Table.Fields.Count - 1 do
        with DefaultMeta.Table.Fields[i] do
          if (Not IsLookup) and (ord(VisibleLevel) > 0) and (PolicyShow) then
          begin
            if LookupPair <> nil then
              FName := LookupPair.Original
            else
              FName := Original;
            FieldBox.Items.Add(AnsiQuotedStr(FName, DeDelimeter) + '=' +
              AnsiQuotedStr(Name, DeDelimeter));
          end;
      FieldBox.ItemIndex := n;
      FindPanel.Visible := True;

      Do_Da_Find.Enabled := DefaultMeta.Cache.Active and
        (Length(Trim(FindBox.Text)) > 0);
      FieldBox.Enabled := DefaultMeta.Cache.Active;
      FindBox.Enabled := DefaultMeta.Cache.Active;

      x1 := 0;
      x2 := FindPanel.Height;
      for i := _AnimStart to Variables.AsInteger[RegAnim] do
      begin
        TopPanel2.Height := LiteResize(x1, x2, Variables.AsInteger[RegAnim], i, 10);
        UpdateWindow(TopPanel2.Handle);
        UpdateWindow(MainGridPanel.Handle);
      end;

      MainGridPanel.Align := alClient;
      if BackViewArea then TBaseGridForm(DefaultMeta.GridForm).ShowViewArea(False);
      FindBox.SetFocus;
    end
  else
    begin
      if BackViewArea then TBaseGridForm(DefaultMeta.GridForm).HideViewArea(False);

      x1 := FindPanel.Height; // Hide
      x2 := 0;

      for i := _AnimStart to Variables.AsInteger[RegAnim] do
      begin
        TopPanel2.Height := LiteResize(x1, x2, Variables.AsInteger[RegAnim], i, 10);
        UpdateWindow(TopPanel2.Handle);
        UpdateWindow(MainGridPanel.Handle);
      end;

      if BackViewArea then TBaseGridForm(DefaultMeta.GridForm).ShowViewArea(False);
      FindPanel.Visible := False;
    end;

  EnableAlign;
end;

// ------------------------------------------------------------------------------
procedure TMForm.Go_Da_BackExecute(Sender: TObject);
begin
{$IFDEF DEBUG}
  DebugLog('Before backward history ...');
{$ENDIF}
  { запоминаем старое значение группировок }
  StoreEnvironment(DeHistory.CurrentItem);
  { переходим на новую запись }
  with DeHistory do
    if CurrentIndex > 0 then
    begin
      CurrentIndex := CurrentIndex - 1;
      DoData(Sender, DeHistory.CurrentItem);
    end;
{$IFDEF DEBUG}
  DebugLog('After backward history ...');
{$ENDIF}
end;

procedure TMForm.Go_Da_ConsistencyExecute(Sender: TObject);
var
  Form: TForm_Da_Consistency;
begin
  Form := TForm_Da_Consistency.Create(Application);
  try
    if Form.Init(nil) then
      Form.ShowModal;
  finally
    Form.Free;
  end;
end;
// ------------------------------------------------------------- -----------------
procedure TMForm.Go_Da_ForwardExecute(Sender: TObject);
begin
{$IFDEF DEBUG}
  DebugLog('Before forward history ...');
{$ENDIF}
  { запоминаем старое значение группировок }
  StoreEnvironment(DeHistory.CurrentItem);
  { переходим на новую запись }
  with DeHistory do
    if CurrentIndex < (Count - 1) then
    begin
      CurrentIndex := CurrentIndex + 1;
      DoData(Sender, DeHistory.CurrentItem);
    end;
{$IFDEF DEBUG}
  DebugLog('After forward history ...');
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TMForm.Go_Da_MainMenuExecute(Sender: TObject);
var
  i: Integer;
begin
  // Если разворачивать меню, то ...
  if Variables.AsBoolean[RegMenuS] then
    begin
      if Assigned(ToolbarList) then
        for i := 0 to Pred(ToolbarList.Count) do
          if ToolbarList.Items[i].Origin = tbMeta then
            begin  // скрываем и отображаем только ПЕРВУЮ панель
              ToolbarList.Items[i].Visible := not ToolbarList.Items[i].Visible;
              Break;
            end;
    end
  else if Assigned(MainMenuForm) then
    begin
      if MainMenuForm.Visible then MainMenuForm.Close
                              else MainMenuForm.Show;
    end
  else
    case Variables.AsByte[RegMenuA] of
      2: MainMenuForm := TForm_Da_MainMenu.Create(DockPanelLeft);
      3: MainMenuForm := TForm_Da_MainMenu.Create(DockPanelRight);
    else MainMenuForm := TForm_Da_MainMenu.Create(nil);
    end;
end;
// ------------------------------------------------------------------------------

procedure TMForm.Do_Da_FindClick(Sender: TObject);
var
  C, R, cur, FN, Index: Integer;
  pStr: PChar;
  Founded: Boolean;
  UpperText: String;
begin
  R := FindBox.Items.IndexOf(FindBox.Text);
  if R > 0 then
    FindBox.Items.Delete(R);
  FindBox.Items.Insert(0, FindBox.Text);
  if FindBox.Items.Count > 8 then
    FindBox.Items.Delete(FindBox.Items.Count - 1);

  C := DefaultMeta.Cache.Count;
  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, C, 1);
  try
    pStr := PChar(FieldBox.Items[FieldBox.ItemIndex]);
    Founded := False;
    FN := DefaultMeta.Cache.Fields.IndexByName(AnsiExtractQuotedStr(pStr, DeDelimeter));

    if DefaultMeta.Table.UserValues.GetFirstIndex(uvFindLast, Index) then
      begin
        DefaultMeta.Table.UserValues[Index].Name:= DefaultMeta.Cache.Fields[FN].Original;
        DefaultMeta.Table.UserValues[Index].XML:= FindBox.Text;
      end
    else
      begin
        DefaultMeta.Table.UserValues.New(uvFindLast, DefaultMeta.Table.ID, UserSession.ID,
          DefaultMeta.Cache.Fields[FN].Original, FindBox.Text )
      end;

    if Assigned(DefaultMeta.Cache.FocusedItem) then
      cur := DefaultMeta.Cache.FocusedIndex
    else
      cur := -1;

    R := C;
    UpperText:= AnsiUpperCase(FindBox.Text);
    while (R > 0) do
    begin
      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, C - R, 0);
      Dec(R);
      cur := (cur + 1) mod C;

      if (isNotInitialized in DefaultMeta.Cache.Items[cur].State) then
        DefaultMeta.Cache.Items[cur].FillItem;
      if 0 < Pos(UpperText, AnsiUpperCase(DefaultMeta.Cache[cur].FieldText(FN)))
      then
      begin
        DefaultMeta.Cache.ClearSelection;
        DefaultMeta.Cache[cur].Selected := True;
        DefaultMeta.Cache.FocusedItem := DefaultMeta.Cache[cur];
        // DefaultMeta.Cache.FocusedItem.Selected := True;

        if Assigned(DefaultMeta.Cache.OnSelectedChange) then
          DefaultMeta.Cache.OnSelectedChange(DefaultMeta.Cache.FocusedItem);
        if Assigned(DefaultMeta.GridForm.ActiveControl) then
          DefaultMeta.GridForm.ActiveControl.SetFocus;

        Founded := True;
        Break;
      end;
    end;

  finally
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  end;
  if Not Founded then
    ShowHintWindow(FindBox, '_eRror.find', '_Dl.information', icoInfo);
end;

procedure TMForm.FindBoxKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if ord(Key) = 13 then
    if Do_Da_Find.Enabled then
      Do_Da_FindClick(Sender);
end;

procedure TMForm.FindBoxChange(Sender: TObject);
begin
  inherited;
  Do_Da_Find.Enabled := DefaultMeta.Cache.Active and (Length(Trim(FindBox.Text)) > 0);
end;

// ------------------------------------------------------------------------------
procedure TMForm.Go_Da_ShowArchivedExecute(Sender: TObject);
begin
  // MetaData.ShowArchived := Not MetaData.ShowArchived;
end;

procedure TMForm.Go_Da_ShowRemovedExecute(Sender: TObject);
begin
  metadata.ShowRemoved := Not metadata.ShowRemoved;
end;

// ------------------------------------------------------------------------------
procedure TMForm.Go_Da_ExitExecute(Sender: TObject);
begin
  onClose := FormClose2;
  Close;
end;

// ------------------------------------------------------------------------------
procedure TMForm.Go_Da_ReportExecute(Sender: TObject);
var
  Variables: TDeVariableList;
  ActionName: string;
  ActionIndex: Integer;
begin
  if Assigned(metadata.MetaTables[idxCommands]) then
  begin
    Variables := TDeVariableList.Create;
    try
      if TDeCommandDialog.Execute(0, Variables) = mrOK then
      begin
        if Assigned(Variables.GetByName(varActionID, False)) then
        begin
          try
            ActionIndex := VarToInt(Variables.GetValueByName(varActionID));
          except
            on E: Exception do
            begin
{$IFDEF DEBUG}
              DebugLog(ClassName + '.Go_Da_ReportExecute: Variables.GetValueByName(%s) skip error: %s',
                [QuotedStr(varActionID), E.Message]);
{$ENDIF}
              ActionIndex := -2;
            end;
          end;
          ActionName := IntToStr(ActionIndex);
          ActionIndex := ProfitActions.IndexByID(ActionIndex);
        end
        else
        begin
          ActionName := Variables.GetValueByName(varActionOriginal);
          if Length(ActionName) = 0 then
            ActionIndex := -1
          else
          begin
            ActionIndex := ProfitActions.IndexByOriginal(ActionName);
            ActionName := QuotedStr(ActionName);
          end;
        end;
        if ActionIndex >= 0 then
          ProfitActions[ActionIndex].Execute(Variables, False)
        else
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
            NativeInt(PChar(Format(GetTitle('_eRror.actionnotfound'),
            [ActionName]))));
      end;
    finally
      Variables.Free;
    end;
  end
  // else
  // TDePrintDialog.Execute;
end;

// ------------------------------------------------------------------------------
procedure TMForm.act_Da_UpdateExecute(Sender: TObject);
var
  State : TKeyboardState;
begin
  GetKeyboardState(State);
  if ((State[vk_Control] And 128) <> 0 ) then
    begin
     MetaData.UpdateLibrary(-2);
      if Assigned(UserSession) and Assigned(DefaultMeta) and Assigned(DefaultMeta.GridForm) then
        TBaseGridForm(DefaultMeta.GridForm).DoUpdate(nil);
    end
  else
    begin
      if Assigned(UserSession) and Assigned(ActiveMeta) and Assigned(ActiveMeta.GridForm) then
        TBaseGridForm(ActiveMeta.GridForm).DoUpdate(Sender);
    end;
end;

// ------------------------------------------------------------------------------
procedure TMForm.SetIsDefaultLogin(aValue: Boolean);
begin
  FDefaultLogin:= aValue;
  if aValue then
    begin
      LoginEdit.Font.Color := clGrayText;
      PassEdit.Font.Color := clGrayText;
    end
  else
    begin
      LoginEdit.Font.Color := clWindowText;
      PassEdit.Font.Color := clWindowText;
    end;
end;

// ------------------------------------------------------------------------------
procedure TMForm.Password_Show(const FullShow: Boolean);
var
  i, xx, v: Integer;
begin
  PanelM.Top := -PanelM.Height;
  PanelTopTop.Height := ClientHeight;
  PanelTopTop.Visible := True;
  BarPanel.Visible := False;
  DisableAlign;

  if FullShow then
    begin
      // загрузка списка решений
      LoadSolutionsList;

      IsDefaultLogin:= true;

      edSolution.ItemIndex := edSolution.Items.IndexOf(Variables.AsString[RegLastSolution]);
      if (edSolution.ItemIndex < 0) and (edSolution.Items.Count > 0) then
        edSolution.ItemIndex := 0;

      edSolutionChange(Self);

      lbSolutionValue.Top := L_Dt_Solution.Top;
      lbSolutionValue.Visible := (ConfigList.Count <= 1);
      L_Dt_Solution.Visible := (1 < ConfigList.Count);
      edSolution.Visible := (1 < ConfigList.Count);
      LoginEdit.Enabled := (0 < ConfigList.Count);
      PassEdit.Enabled := (0 < ConfigList.Count);

      xx := (ClientHeight + PanelM.Height  - LanguagePanel.Height) div 2;

      PanelM.DoubleBuffered:=true;
      for i := 1 to 15 do
        begin
          v := LiteResize(0, xx, 15, i, 10) - PanelM.Height;
          PanelM.Top := v;
          if i = 1 then
            PanelM.Visible := True;
          PanelM.Repaint;
        end;
      PanelTopTop.Repaint;

      if PassEdit.Enabled then
        if PassEdit.HandleAllocated then
          PassEdit.SetFocus;
    end;

  EnableAlign;
  LanguagePanel.Visible:= True;
  BarPanel.Update;
end;

// ------------------------------------------------------------------------------
procedure TMForm.Password_Hide;
var
  i, v, xx: Integer;
begin
  DisableAlign;
  LanguagePanel.Visible:= False;
  xx := PanelM.Top + PanelM.Height;
  if (0 < xx) then
  begin
    for i := 1 to 15 do
    begin
      v := LiteResize(xx, 0, 15, i, 10) - PanelM.Height;
      PanelM.Top := v;
      if i = 1 then
        PanelM.Visible := True;
      PanelM.Repaint;
    end;
    PanelTopTop.Repaint;
  end
  else
  begin
    PanelTopTop.Repaint;
  end;

  BarPanel.Visible := True;
  EnableAlign;

  // меняем местами SBar и BottomBarPanel, если они при алигне "уехали"
  if SBar.Visible and BottomBarPanel.Visible and (SBar.Top < BottomBarPanel.Top)
  then
    BottomBarPanel.Top := SBar.Top - BottomBarPanel.Height;
end;

procedure TMForm.LangButtonClick(Sender: TObject);
var
  L, i, x1, x2: Integer;
begin
  inherited;
  x1:= LanguageSelectPanel.Left;
  x2:= TControl(Sender).Left;
  for i := 1 to 15 do
    LanguageSelectPanel.Left := LiteResize( x1, x2, 15, i, 10);

  L:= TControl(Sender).Tag;
  if Dict.ActiveLang <> Dict.Languages[L].LngID  then
    begin
      if Dict.SetActiveLang(Dict.Languages[L].LngID) then
        begin
          MForm.FormLangPaint(Self);
        end
      else
        begin
          for i := 1 to 15 do
            LanguageSelectPanel.Left := LiteResize( x2, x1, 15, i, 10);

          ShowHintWindow( TControl(Sender), Format(GetTitle('_de.file'), [Dict.Languages[L].FileName]),
                                                                             GetTitle('_dE.error'), icoError, mtError);
        end;

      case ConfigList.Count of
       0: lbSolutionValue.Caption := GetTitle('_dA.emptylist');
       1: lbSolutionValue.Caption := '[ ' + ConfigList.Items[0].Name + ' ]';
      end
    end;
end;

// ------------------------------------------------------------------------------
procedure TMForm.PanelTopTopResize(Sender: TObject);
const
  wLow = 600;
  wHi = 1000;
var
  Z : Word;
begin
  inherited;
  PanelM.Color := clActiveCaption;

  PanelTopTop.Height := ClientHeight;
  PanelM.Width := PanelM.Parent.Width;
  Z := (PanelTopTop.Width - PanelR.Width - PanelLL.Width);

  if Z < wLow then
    PanelL.Width := 0
  else if wHi < Z then
    begin
      L_dL_rEgistration.Font.Color := clWhite;
      PanelL.Width := Round((PanelTopTop.Width - PanelR.Width) * (0.4))
    end
  else
    begin
      L_dL_rEgistration.Font.Color :=
        RGB(LiteResize(GetRValue(ColorToRGB(PanelL.Color)), 255, wHi - wLow, Z - wLow, 0),
            LiteResize(GetGValue(ColorToRGB(PanelL.Color)), 255, wHi - wLow, Z - wLow, 0),
            LiteResize(GetBValue(ColorToRGB(PanelL.Color)), 255, wHi - wLow, Z - wLow, 0));
      PanelL.Width := Z - wLow;
    end;

  PanelM.Top := (ClientHeight - PanelM.Height - LanguagePanel.Height) div 2;
end;


procedure TMForm.FormResize(Sender: TObject);
begin
  inherited;
  if PanelTopTop.Visible then
    PanelTopTopResize(Sender);
end;

// ------------------------------------------------------------------------------

procedure TMForm.FindPanelResize(Sender: TObject);
const
  wLow = 600;
  wHi = 1000;
var
  Z: Word;
begin
  inherited;

  inherited;
  FindPanel.Color := clActiveCaption;

  FindPanel.Width := FindPanel.Parent.Width;
  Z := (FindPanel.Parent.Width - PanelR_F.Width - PanelLL_F.Width);

  if Z < wLow then
    PanelL_F.Width := 0
  else if wHi < Z then
  begin
    LblFind.Font.Color := clWhite;
    PanelL_F.Width := Round((FindPanel.Width - PanelR_F.Width) * (0.4))
  end
  else
  begin
    LblFind.Font.Color := RGB(LiteResize(GetRValue(ColorToRGB(PanelL.Color)),
      GetRValue(ColorToRGB(clWhite)), wHi - wLow, Z - wLow, 0),
      LiteResize(GetGValue(ColorToRGB(PanelL.Color)),
      GetGValue(ColorToRGB(clWhite)), wHi - wLow, Z - wLow, 0),
      LiteResize(GetBValue(ColorToRGB(PanelL.Color)),
      GetBValue(ColorToRGB(clWhite)), wHi - wLow, Z - wLow, 0));
    PanelL_F.Width := Z - wLow;
  end;

end;

// ------------------------------------------------------------------------------
procedure TMForm.BackMenuPopup(Sender: TObject);
var
  i: Integer;
  TMI: TMenuItem;
begin
  BackMenu.Items.Clear;
  for i := (DeHistory.CurrentIndex - 1) downto max(0, DeHistory.CurrentIndex - 8) do
  begin
    TMI := TMenuItem.Create(BackMenu);
    TMI.Caption := GetTitle(DeHistory.Items[i].Caption, ttSecondName);
    TMI.Hint := GetTitle(DeHistory.Items[i].Hint);
    if Assigned(DeHistory.Items[i].Dataset) then
      TMI.ImageIndex := DM.MapIconIndex(DeHistory.Items[i].Dataset.Ico)
    else
      TMI.ImageIndex := -1;
    TMI.OnClick := HistoryClick;
    TMI.Tag := i;
    BackMenu.Items.Add(TMI);
  end;
end;

// ------------------------------------------------------------------------------
procedure TMForm.ForwardMenuPopup(Sender: TObject);
var
  i: Integer;
  TMI: TMenuItem;
begin
  ForwardMenu.Items.Clear;
  for i := (DeHistory.CurrentIndex + 1) to min(DeHistory.CurrentIndex + 8,
    DeHistory.Count - 1) do
  begin
    TMI:= TMenuItem.Create(ForwardMenu);
    TMI.Caption:= GetTitle(DeHistory.Items[i].Caption, ttSecondName);
    TMI.Hint:= GetTitle(DeHistory.Items[i].Hint);
    if Assigned(DeHistory.Items[i].Dataset) then
      TMI.ImageIndex := DM.MapIconIndex(DeHistory.Items[i].Dataset.Ico)
    else
      TMI.ImageIndex := -1;
    TMI.OnClick := HistoryClick;
    TMI.Tag := i;
    ForwardMenu.Items.Add(TMI);
  end;
end;

// ------------------------------------------------------------------------------
procedure TMForm.AfterChangeHistory;
begin
  Go_Da_Update.Enabled := Assigned(UserSession) and Assigned(DefaultMeta) and Assigned(DefaultMeta.GridForm);
  act_Da_Update.Enabled := Go_Da_Update.Enabled;
  Go_Da_Find.Enabled := Assigned(UserSession) and Assigned(DefaultMeta) and Assigned(DefaultMeta.GridForm) and
                        not(DefaultMeta.GridForm is TDeChartForm);
  Go_Da_Report.Enabled := Assigned(UserSession) and Assigned(DefaultMeta) and (flagOffice);
  Go_Da_Back.Hint := GetTitle('_Da.Back', ttSecondName);
  Go_Da_Back.Enabled := (DeHistory.CurrentIndex > 0) and (DeHistory.Count > 0);
  if Go_Da_Back.Enabled then Go_Da_Back.Hint := Go_Da_Back.Hint + sp + '''' +
                                       GetTitle(DeHistory.Items[DeHistory.CurrentIndex - 1].Name, ttSecondName) + '''';

  Go_Da_Forward.Hint := GetTitle('_Da.Forward', ttSecondName);
  Go_Da_Forward.Enabled := (DeHistory.CurrentIndex < (DeHistory.Count - 1));
  if Go_Da_Forward.Enabled then Go_Da_Forward.Hint := Go_Da_Forward.Hint + sp + '''' +
                                      GetTitle(DeHistory.Items[DeHistory.CurrentIndex + 1].Name, ttSecondName) + '''';
end;

// ------------------------------------------------------------------------------
procedure TMForm.AfterRegUser(Sender: TObject);
begin
  // MM_Service.Visible    := Assigned(UserSession);
  MM_Da_Edit.Visible := Assigned(UserSession);
  MM_Da_View.Visible := Assigned(UserSession);
  MM_Da_Create.Visible := Assigned(UserSession);
  MM_da_Go.Visible := Assigned(UserSession);
  MM_Da_Service.Visible := Assigned(UserSession);
  MM_da_Actions.Visible := Assigned(UserSession);
  MM_da_Commands.Visible := Assigned(UserSession);
  MM_Da_Operations.Visible := Assigned(UserSession);
  Go_Da_Remember.Visible := Assigned(UserSession);

  Go_Da_MainMenu.Enabled := Assigned(UserSession) and Go_Da_MainMenu.Visible;
  Go_Da_Report.Enabled := Assigned(UserSession) and Assigned(DefaultMeta) and (flagOffice);
  Go_Da_Find.Enabled := Assigned(UserSession) and Assigned(DefaultMeta);
  //Go_Da_Backup.Enabled := Assigned(UserSession) and (UserSession.IsAdmin);
  Go_Da_Consistency.VISIBLE := Assigned(UserSession) and UserSession.IsAdmin;
  Go_dA_DataImport.VISIBLE := Assigned(UserSession);
  Go_dA_DataExport.Enabled := Assigned(UserSession);
  Go_Da_UnregUser.Enabled := Assigned(UserSession);

  Go_Da_Settings.Enabled := Assigned(UserSession);

  act_Da_ShowCardArea.Checked := Variables.AsBoolean[RegShowCardArea];

  // BarPanel.Visible := Assigned(ConfigList) and ConfigList.ToolBarsVisible;
  // BarPanel.Visible := Variables.AsBoolean[RegToolBars_Visible];
  Go_Da_Back.Enabled := False;
  Go_Da_Back.Hint := GetTitle('_Da.Back', ttSecondName);
  Go_Da_Forward.Enabled := False;
  Go_Da_Forward.Hint := GetTitle('_Da.Forward', ttSecondName);

  // Создадим меню глобальных фильтров ...
  BuildGlobalFilterMenu;

  if Assigned(UserSession) then
  begin
    DeStatus.StatusItemByID(ConnectID).Hint := MakeHint('_Df.Server ', CurrentConfig.Server) +
      MakeHint('_Dt.Database', CurrentConfig.Database) + MakeHint('_Df.Connection', CurrentConfig.ConnectString);
    DeStatus.StatusItemByID(ConnectID).Ico := DM.MapIconIndex(23);
  end
  else
  begin
    DeStatus.StatusItemByID(ConnectID).Hint := GetTitle('_Df.Connection: _dl.no');
    DeStatus.StatusItemByID(ConnectID).Ico := DM.MapIconIndex(38);
  end;

  { пользователи }
  { MM_Users.Visible :=
    Assigned(UserSession)
    and SecuritySystem.CheckDatasetPolicy(MetaData.MetaTables[idxUsers].ID, spSelect);
    if MM_Users.Visible then
    begin
    MM_Users.Caption := GetTitle(MetaData.MetaTables[idxUsers].Name);
    MM_Users.Tag := NativeInt(MetaData.MetaTables[idxUsers]);
    MM_Users.OnClick := OnDatasetClick;
    end;
  }
  { права }
  MM_Da_Access.Visible := Assigned(UserSession) and (UserSession.IsAdmin)
  // TODO: Убрать после создание нормальных прав
    and SecuritySystem.CheckPolicyDataSet(metadata.MetaTables[idxRights].ID, spSelect);
  MM_Da_Access.ImageIndex:= DM.MapIconIndex(112);
  if MM_Da_Access.Visible then
  begin
    MM_Da_Access.Tag := NativeInt(metadata.MetaTables[idxRights]);
    MM_Da_Access.OnClick := OnRightsClick;
  end;
  { словарь }
  MM_Da_Dictionary.Visible := Assigned(UserSession) and
    (metadata.MetaTables[idxDictionary].IsDirectory in [idLocal, idGlobal]) and
    (SecuritySystem.CheckPolicyDataSet(metadata.MetaTables[idxLang].ID, spSelect));
  if MM_Da_Dictionary.Visible then
  begin
    MM_Da_Dictionary.Tag := NativeInt(metadata.MetaTables[idxLang]);
    MM_Da_Dictionary.OnClick := OnDictionaryClick;
  end;
end;

// ------------------------------------------------------------------------------
procedure TMForm.Do_UnregUser;
var
  BackupCursor: TCursor;
  EmptyData: TContextMeta;
begin

  SaveToolBars;
  {
    if Assigned(FileManager)and(FileManager.Count>0)and(FileManager.AskBeforeTerminate) then
    if MessageBox(Handle,
    'Имеются открытые файлы, которые используются до сих пор.'+
    ' При выходе изменения сделанные в этих файлах будут утеряны.'+
    ' Все равно выйти?', PChar(dicAppName),
    MB_YESNO or MB_ICONWARNING)<>IDYES then Exit;

    if Assigned(FileManager) then FreeAndNil(FileManager);
  }
  Rem.Enabled := False;
  Rem.Clear;

  if Assigned(UserSession) then
    begin
      EmptyData := TContextMeta.Create(nil);
      DoData(Self, EmptyData);
      EmptyData.Free;
    end;

  DeHistory.Clear;

  DeMessages.Clear;
  Go_Da_LastMess.Enabled:= False;

  if DeStatus.StatusItemByID(LastMessID) <> nil then
    DeStatus.Remove(LastMessID);

  if Assigned(UserSession) then
    DoAct('OnLogout', nil);

  SecuritySystem.ClearPolicy;
  FreeAndNil(UserSession);

  NeedLogout := False;

  Application.Title := dicAppFullName;
  Caption := dicAppName + ' - [ * ]';
  AttentionPanel.Visible := False;

  if Assigned(MainMenuForm) then
  begin
    MainMenuForm.SavePosition;
    FreeAndNil(MainMenuForm);
  end;

  APanel.Visible := False;

  AfterRegUser(nil);

  // удаляем метаструктуру
  UpdateWindow(Handle);
  BackupCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    metadata.Disconnect;
    FreeAndNil(CurrentConfig);
  finally
    Screen.Cursor := BackupCursor;
  end;

  Update;

  while BaseFormsCount > 0 do
    BaseForms[0].Free;

  // Сбрасываем контекст всех функций при необходимости ...
  if Assigned(FunctionList) then FunctionList.ResetContents;
end;
// ------------------------------------------------------------------------------
procedure TMForm.edSolutionChange(Sender: TObject);
var
  Enabled: Boolean;
begin
  Enabled := edSolution.ItemIndex >= 0;

  LoginEdit.Enabled := Enabled;
  PassEdit.Enabled := Enabled;
  Password_Ok.Enabled := Enabled;

  if edSolution.Items.Count = 0 then
    edSolution.Hint := EmptyStr
  else
    edSolution.Hint := ConfigList[edSolution.ItemIndex].Name +
//    MakeHint('_Df.DatabaseType', ConfigList[edSolution.ItemIndex].DBType) +
      MakeHint('_Df.Server', ConfigList[edSolution.ItemIndex].Server) +
      MakeHint('_Dt.Database', ConfigList[edSolution.ItemIndex].Database) +
      MakeHint('_Df.Connection', ConfigList[edSolution.ItemIndex].ConnectString);


  if ConfigList.Count = 0 then
    begin
      lbSolutionValue.Caption := GetTitle('_dA.emptylist');
      LoginEdit.Text := EmptyStr;
      PassEdit.Text := EmptyStr;
    end
  else
    begin
      lbSolutionValue.Caption := '[ ' + ConfigList.Items[edSolution.ItemIndex].Name + ' ]';

      if IsDefaultLogin then
        begin
          LoginEdit.OnChange:= nil;
          PassEdit.OnChange:= nil;

          if Length(Trim(ConfigList[edSolution.ItemIndex].DefaultLogin)) = 0 then
            begin
              LoginEdit.Text := GetWindowsUserName;
              PassEdit.Text := EmptyStr;
            end else

            begin
              LoginEdit.Text := ConfigList[edSolution.ItemIndex].DefaultLogin;
              PassEdit.Text := ConfigList[edSolution.ItemIndex].DefaultPassword;
            end;

          LoginEdit.OnChange:= LoginOrPassEditChange;
          PassEdit.OnChange:= LoginOrPassEditChange;
        end;
    end;
end;

procedure TMForm.edSolutionKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
    LoginEdit.SetFocus;
end;

procedure TMForm.LoginEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
    PassEdit.SetFocus;
end;

procedure TMForm.PassEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
    Pass_OkClick(Sender);
end;

procedure TMForm.LoginOrPassEditChange(Sender: TObject);
begin
  IsDefaultLogin:= False;
  inherited;
end;

procedure TMForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_CONTROL then
    FCtrlPress:= True;
  if Key = VK_MENU then
    FAltPress:= True;
end;

procedure TMForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_CONTROL then
    FCtrlPress:= False;
  if Key = VK_MENU then
    FAltPress:= False;

  if FCtrlPress and (Key = VK_RETURN) then
    if not Assigned(UserSession) then // только для экрана логина
      Pass_OkClick(Sender);
end;

// ------------------------------------------------------------------------------
procedure TMForm.GoStartWorkExecute(Sender: TObject);
var
  AttentionValue: Variant;
  MenuNode: TContextMeta;

  function GetFirstContext(Level: Integer; aMenuContext: TContextMeta): TContextMeta;
  var i: Integer;
  begin
    Result:= nil;
    for i:= 0 to Pred(aMenuContext.Count) do
      begin
        if (0 < Level) and Assigned(aMenuContext.Items[i].Dataset) and
           (Not aMenuContext.Items[i].IsSeparator) and (Not aMenuContext.Items[i].Deleted)
           then if SecuritySystem.CheckPolicyMenu(aMenuContext.Items[i].ID, spSelect) and
                   SecuritySystem.CheckPolicyDataSet(aMenuContext.Items[i].Dataset.ID, spSelect)
                   then Exit(aMenuContext.Items[i]);

        Result:= GetFirstContext( Succ(Level), aMenuContext[i]);
        if Assigned(Result) then Exit;
      end;
  end;

begin
  if not Assigned(UserSession) then
    Exit;

  Caption := dicAppName + ' - [ ' + UserSession.Name + ' ]';

  AfterRegUser(Sender);

  if MainMenuMeta = nil then
    MainMenuMeta := TContextMeta.Create(nil, True)
  else
    MainMenuMeta.LoadMenu;

  if not Variables.AsBoolean[RegMenuS] then
    Go_Da_MainMenu.Execute;

  // Заменил, т.к. здесь автоматически не расчитывались значения!!!
  AttentionValue := metadata.ParamValue['Attention'];
  if MetaData.SimpleMode then AttentionValue:= 'Simple Mode';

  AttentionPanel.Visible := Length(VarToStr(AttentionValue)) <> 0;

  if AttentionPanel.Visible then
    AttentionLabel.Caption := AttentionValue;

  if Variables.AsBoolean[RegMenuS] then BuildSwapMenu;

  BuildCommands;
  BuildMainMenuList;
  BuildDatasetsList;

  ToolbarList.Show;
  Password_Hide;
  Refresh;

  CreateDirectoriesMenu; // иначе не происходит очистки

  APanel.Visible := True;
  EndEditToolBars;
  PanelTopTop.Visible := False;
  Refresh;

  if not(FAltPress) then
    begin
      MenuNode:= GetFirstContext(0, MainMenuMeta);
      if Assigned(MenuNode) then DoData(Sender, MenuNode );
    end;

  // 32-х битная версия больше не поддерживается и
  // соответственно отключаем для неё автоматическое обновление!
  {$IFDEF WIN64}
  // проверка обновлений на сайте
  if Variables.AsBoolean[RegCheckUpdates] and
    (Date > Variables.AsDateTime[RegLastUpdateDate]) then
      DM.GetUpdateInfo(Self.Handle);
  {$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TMForm.CreateDirectoriesMenu;
var
  i, j, n: Integer;
  t: TTableMeta;
  MMD, Item: TMenuItem;
begin
  MM_Dl_Directories.Clear;
  MM_Dl_CDirectories.Clear;
  if Assigned(CurrentConfig) and Assigned(DefaultMeta) then
  begin
    for i:= 0 to metadata.TableCount - 1 do
      begin
        if metadata.Tables[i].SolutionID <> DefaultMeta.Table.SolutionID then
          Continue;
        t:= metadata.Tables[i];
        if (t.IsDirectory in [idLocal, idGlobal]) and (SecuritySystem.CheckPolicyDataSet(t.ID, spSelect)) then
          begin
            Item:= TMenuItem.Create(Self);
            Item.Caption:= GetTitle(t.Name, ttSecondName);
            Item.ImageIndex:= DM.MapIconIndex(t.Ico);
            Item.Tag:= NativeInt(t);
            Item.OnClick:= OnDatasetClick;
            n:= 0;

            If (t.Links.Count < 2) then MMD:= MM_Dl_Directories
                                   else MMD:= MM_Dl_CDirectories;

            Item.Hint:= Item.Caption;
            for j:= 0 to t.Links.Count - 1 do
              if j = 0 then Item.Hint:= Item.Hint + ' => ' + GetTitle(t.Links[j].Owner.Name, ttSecondName)
                       else Item.Hint:= Item.Hint + '; ' + GetTitle(t.Links[j].Owner.Name, ttSecondName);

            while (n < (MMD.Count)) and (MMD.Items[n].Caption < Item.Caption) do
              inc(n);
            MMD.Insert(n, Item);
          end;
      end;

    if (MM_Dl_Directories.Count < 4) or (MM_Dl_CDirectories.Count < 4) then
      begin
        Item := TMenuItem.Create(Self);
        Item.Caption := cLineCaption;
        MM_Dl_Directories.Insert(0, Item);

        while MM_Dl_CDirectories.Count > 0 do
          begin
            Item := MM_Dl_CDirectories.Items[MM_Dl_CDirectories.Count - 1];
            MM_Dl_CDirectories.Remove(Item);
            MM_Dl_Directories.Insert(0, Item);
          end;
      end;
  end;

  MM_Dl_Directories.Visible:= MM_Dl_Directories.Count > 0;
  MM_Dl_CDirectories.Visible:= MM_Dl_CDirectories.Count > 0;
end;

// ------------------------------------------------------------------------------
function TMForm.LoginIn: Boolean;
var
  Disabled: Boolean;
begin
  {$IFDEF DEBUG}
  DebugLog(ClassName + '.LoginIn start ...');
  {$ENDIF}
  Result := False;

  if not metadata.Connect then
  begin
    {$IFDEF DEBUG}
    DebugLog(ClassName + '.LoginIn: connect metadata error: ' + ErrorMessage);
    {$ENDIF}
    FreeAndNil(CurrentConfig);
    metadata.Disconnect;
    if Length(ErrorMessage) = 0 then
      ErrorMessage := '_Error.connectmeta';
    PostMessage(Handle, DM_ERRORNOTIFY, 23, 0);
  end;

  // Exception in 'Gift' database!
  if Assigned(GlobalActionConditionList) then
    GlobalActionConditionList.TableMetaID := EmptyTableMetaID;

  // вход в систему
  if Assigned(CurrentConfig) then
    try
      Disabled := False;
      try
        if Metadata.MetadataDB = Metadata.SampleMTDB
          then UserSession := TUserSession.Create('Administrator', EmptyStr)
          else UserSession := TUserSession.Create(CurrentConfig.SessionLogin, CurrentConfig.SessionPassword);
      except
        on E: ESecurityDisabledError do
        begin
          {$IFDEF DEBUG}
          DebugLog(ClassName + '.LoginIn: TUserSession.Create error: ' + E.Message);
          {$ENDIF}
          FreeAndNil(CurrentConfig);
          Disabled := True;
        end;
        on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog(ClassName + '.LoginIn: TUserSession.Create error: ' + E.Message);
          {$ENDIF}
          FreeAndNil(CurrentConfig);
        end;
      end;

      if Not(Assigned(UserSession)) then
      begin
        if Disabled then
          SendMessage(Handle, DM_INFONOTIFY, icoLock, NativeInt(PChar('_eRror.useraccountdisabled')))
        else if GetKeyState(VK_CAPITAL) and 1 = 1 then
          SendMessage(Handle, DM_INFONOTIFY, icoLock, NativeInt(PChar('_dE.password'#10'_dE.passwordcapslock')))
        else
          SendMessage(Handle, DM_INFONOTIFY, icoLock, NativeInt(PChar('_dE.password')));
      end
      else
      begin
        SecuritySystem.LoadPolicy;
      end;
    except
      on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog(ClassName + '.LoginIn: error: ' + E.Message);
        {$ENDIF}
        FreeAndNil(UserSession);
        SendMessage(Handle, DM_INFONOTIFY, icoLock, NativeInt(PChar('_eRror.connectmeta')))
      end;
    end;

  if Assigned(UserSession) then
  begin
    Variables.AsString[RegLastSolution] := CurrentConfig.Name;
    if Length(CurrentConfig.DefaultLogin) = 0 then
    begin
      CurrentConfig.DefaultLogin := UserSession.Login;
      CurrentConfig.DefaultPassword := PassEdit.Text;
      ConfigList.SaveToXML(ConfigXML);
    end;
    DoAct('OnLogin', nil);

    GoStartWorkExecute(nil);
    Rem.Enabled := Variables.AsBoolean[RegRemStartup];
    Result := True;
  end;

  // Если есть прогресс бар, то удалим все открытые уровни ...
  if Assigned(FStatusItem) then
    FStatusItem.ClearProgress;
  {$IFDEF DEBUG}
  DebugLog(ClassName + '.LoginIn finish ...');
  {$ENDIF}
end;

procedure TMForm.Pass_OkClick(Sender: TObject);
begin
  {$IFDEF DeDEBUG}
  Funcs.WriteLog('Password Enter');
  {$ENDIF}
  if edSolution.ItemIndex >= 0 then
  begin
    // подключение к метаструктуре
    metadata.Disconnect;
    CurrentConfig := TDeConfigItem.Create;
    CurrentConfig.Assign(ConfigList[edSolution.ItemIndex]);
    CurrentConfig.SessionLogin:= Trim(LoginEdit.Text);
    CurrentConfig.SessionPassword:= Trim(PassEdit.Text);

    if LoginIn then
      begin
      end;
  end;
end;
// ------------------------------------------------------------------------------
procedure TMForm.Go_Da_UnregUserExecute(Sender: TObject);
begin
  Do_UnregUser;
  Password_Show;

  ClearDatasetsList;
  ClearMainMenuList;
  ClearCommands;

  if Assigned(FSwapMenuList) then
  begin
    ClearSwapMenu;
    ReAlignSwapMenu(MainMenu);
    DrawMenuBar(Handle);
  end;
end;

procedure TMForm.Go_Da_ViewExecute(Sender: TObject);
begin
  inherited;
  //
end;

procedure TMForm.Go_Da_SplitByExecute(Sender: TObject);
begin
  inherited;
  //
end;

procedure TMForm.Go_Da_PresentationExecute(Sender: TObject);
begin
  inherited;
  //
end;

procedure TMForm.Go_Da_OrderExecute(Sender: TObject);
var ADS: TDataSetMeta;
begin
  inherited;
  ADS:=ActiveMeta;
  if assigned(ADS) then
    TBaseGridForm(ADS.GridForm).act_Da_Sorting.Execute;
end;
// ------------------------------------------------------------------------------
procedure TMForm.MM_Da_HelpClick(Sender: TObject);
begin
  inherited;
  Go_Da_Help.Enabled:= FileExists(Application.HelpFile);
end;

procedure TMForm.NewHelpClick(Sender: TObject);
var
  s: String;
begin
  if Not(Sender is TMenuItem) then
    Exit;
  s := TMenuItem(Sender).Hint;
  s := copy(s, 1, Pos('|', s) - 1);
  if 32 >= Shellexecute(Handle, nil, PChar(s), nil, nil, sw_shownormal) then
    SendMessage(Handle, DM_INFONOTIFY, icoError, NativeInt(Format(GetTitle('_De.File'), [s])));
end;

procedure TMForm.Go_Da_HelpExecute(Sender: TObject);
begin
  if 32 >= Shellexecute(Handle, nil, PChar(Application.HelpFile), nil, nil, sw_shownormal) then
    SendMessage(Handle, DM_INFONOTIFY, icoError, NativeInt(Format(GetTitle('_De.File'), [Application.HelpFile])));
end;

// ------------------------------------------------------------------------------
procedure TMForm.Go_dA_HomePageExecute(Sender: TObject);
begin
  Shellexecute(Application.MainForm.Handle, nil, PWideChar(dicAppHomeURL), nil, nil, sw_shownormal);
end;

// ------------------------------------------------------------------------------
procedure TMForm.Go_Da_AboutExecute(Sender: TObject);
begin
  TForm_Da_About.Execute;
end;
// ==============================================================================

procedure TMForm.LoadSolutionsList;
var
  i: Integer;
begin
  // загрузка списка решений
  edSolution.Items.Clear;
  for i := 0 to ConfigList.Count - 1 do
    edSolution.AddItem(ConfigList[i].Name, ConfigList[i]);
end;

procedure TMForm.Go_Da_ParametersExecute(Sender: TObject);
var
  frmConfig: TForm_Da_Parameters;
  SolutionName: string;
  SelectedSolution: TDeConfigItem;
  Index: Integer;
begin
  if not Assigned(UserSession) then
    metadata.Disconnect;
  frmConfig := TForm_Da_Parameters.Create(Self);
  LangFormUpdate(frmConfig);
  with frmConfig do
  begin
    if ShowModal = mrOK then
    begin
      // запоминаем выбранный элемент в списке решений
      SelectedSolution := CurrentConfig;
      if Assigned(SelectedSolution) then
        SolutionName := SelectedSolution.Name
      else if edSolution.ItemIndex >= 0 then
        SolutionName := edSolution.Items[edSolution.ItemIndex]
      else
        SolutionName := EmptyStr;
      // загружаем список
      LoadSolutionsList;
      // определяем выбранный элемент
      Index := edSolution.Items.IndexOf(SolutionName);
      if (Index < 0) and (edSolution.Items.Count > 0) then
        Index := 0;
      edSolution.ItemIndex := Index;
      edSolutionChange(Self);

      if not Assigned(UserSession) then
        begin
          Password_Hide;
          Password_Show;
        end;
    end;
    Free;
  end;
  if NeedLogout then
  begin
    Do_UnregUser;
    Password_Show;
  end;
end;

// ------------------------------------------------------------------------------
procedure TMForm.HistoryClick(Sender: TObject);
begin
  { запоминаем старое значение группировок }
  StoreEnvironment(DeHistory.CurrentItem);
  { переходим на новую запись }
  DeHistory.CurrentIndex := TMenuItem(Sender).Tag;
  DoData(Sender, DeHistory.CurrentItem)
end;

// ------------------------------------------------------------------------------
procedure TMForm.Go_Da_LastMessExecute(Sender: TObject);
var TSI: TStatusItem;
begin
  SBar.Update;
  if DeMessages.Count <> 0 then
    begin
      TSI := DeStatus.StatusItemByID(LastMessID);
      if Assigned(TSI) then
        ShowHintWindow(SBar.ClientToScreen(Point(4 + 8 + TSI.Position, 4)), DeMessages.Items[Pred(DeMessages.Count)])
      else
        ShowHintWindow(SBar.ClientToScreen(Point(4, 4)), DeMessages.Items[Pred(DeMessages.Count)]);
    end;
end;

Procedure TMForm.LastFocus(var Mess: TMessage);
begin
  if flagIgnoreActive then
    Mess.Result := 1
  else
    inherited;
end;

procedure TMForm.SBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
begin
  DeStatus.Draw(SBar.Canvas, Rect);
end;

procedure TMForm.SBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SI: TStatusItem;
  x1, x2: Integer;
  p: TPoint;
begin
  case Button of
    mbLeft:
      begin
        SI := DeStatus.StatusItemByPosition(X);
        if Assigned(SI) then
          if Assigned(SI.OnClick) then
            SI.OnClick(Sender);
      end;
    mbRight:
      begin
        x1 := SBar.Panels[0].Width;
        x2 := x1 + SBar.Panels[1].Width;
        if (x1 < X) and (X < x2) and (DefaultMeta.GridForm is TBaseGridForm)
        then
        begin
          p := SBar.ClientToScreen(Point(X, Y));
          TBaseGridForm(DefaultMeta.GridForm).StatusPopupMenu.Popup(p.X, p.Y);
        end;
      end;
  end;
end;

procedure TMForm.SBarMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  SI: TStatusItem;
begin
  SI := DeStatus.StatusItemByPosition(X);

  if Assigned(SI) then
    SBar.Hint := SI.Hint
  else
    SBar.Hint := EmptyStr;
end;

procedure TMForm.FormCreate(Sender: TObject);
var
  temp: Boolean;
  SR: TSearchRec;
  TMI: TMenuItem;
  n: String;
  i, j: Integer;
  TB: TToolButton;
  SB: TSpeedButton;
begin
  inherited;
  tiTask.Icon.Handle := LoadIcon(hInstance, 'MAINICON');
  Application.OnMinimize := ApplicationMinimize;
  Application.OnRestore := ApplicationRestore;
  tiTask.Hint := dicAppName;
  // tiTask.Visible := True;
  // TrayIcon(tioInsert, Handle, DM_ICONNOTIFY, 'MAINICON', dicAppName);
  Application.OnHint := DeShowHint;
  // Application.OnIdle := DeIdle;
  if FileExists( ExtractFilePath(Application.ExeName) + dicAppName + sExtensionCHM ) then
    Application.HelpFile := ExtractFilePath(Application.ExeName) + dicAppName + sExtensionCHM else
  if FileExists( ExtractFilePath(Application.ExeName) + dicAppName + sExtensionPDF ) then
    Application.HelpFile := ExtractFilePath(Application.ExeName) + dicAppName + sExtensionPDF else
    Application.HelpFile := GetTitle('_Da.Help');


  DockPanelLeft.Alignment := TAlignment(alLeft);
  DockPanelRight.Alignment := TAlignment(alRight);
  DockPanelUnDock(DockPanelLeft, nil, nil, temp);
  DockPanelUnDock(DockPanelRight, nil, nil, temp);

  DM.ilIcon32.GetBitmap(DM.MapIconIndex(117), Password_Ok.Glyph);
  PanelM.Top := -PanelM.Height;
  PanelTopTop.Height := 2000;
  PanelTopTop.Visible := True;

  DeStatus.ParentControl := SBar;
  DeStatus.Add(ConnectID, 39 { ConnectID mod 100 } , '_Df.Connection', Go_Da_AboutExecute);

  FStatusItem := DeStatus.Add(ProgressID, 15 { ProgressID mod 100 } , EmptyStr, nil, False);
  {
    if Assigned(PluginsManager) then
    for i:=0 to PluginsManager.PluginsCount-1 do
    for j:=0 to PluginsManager.Plugins[i].MethodsCount-1 do
    begin
    TB:=TToolButton.Create(MForm);
    TB.Parent:=tbAllCommands;

    TB.Name:=GenerateName(PluginsManager.Plugins[i].FileName)+
    PluginsManager.Plugins[i].Methods[j].InternalName+'Btn';
    TB.Action:=TAction(PluginsManager.Plugins[i].Methods[j]);
    TB.AutoSize:=True;
    end;
    { }

  ToolbarList.Load;
  LoadHotKeys;

  AfterRegUser(Self);
  FormLangPaint(Self);
  GlobalActionConditionList := TEventMetaList.Create(True);

  if FindFirst('*' + sExtensionPDF, faAnyFile, SR) = 0 then
    repeat
      n := ExtractFileName(SR.Name);
      // добавляем в меню все справки, кроме основной

      if SameText(n, dicAppName + sExtensionPDF) then
        Continue;

      TMI := TMenuItem.Create(Self);
      TMI.Caption := ChangeFileExt(n, EmptyStr);
      TMI.Hint := SR.Name + '|' + GetTitle('_Da.Help', ttSecondName) + ' ' + ChangeFileExt(n, EmptyStr);
      TMI.OnClick := NewHelpClick;
      MM_Da_Help.Insert(MM_Da_Help.IndexOf(NewHelpSeparator), TMI);
    until FindNext(SR) <> 0;
  FindClose(SR);

  if FindFirst('*' + sExtensionCHM, faAnyFile, SR) = 0 then
    repeat
      n := ExtractFileName(SR.Name);
      // добавляем в меню все справки, кроме основной

      if SameText(n, dicAppName + sExtensionCHM) then
        Continue;

      TMI := TMenuItem.Create(Self);
      TMI.Caption := ChangeFileExt(n, EmptyStr);
      TMI.Hint := SR.Name + '|' + GetTitle('_Da.Help', ttSecondName) + ' ' + ChangeFileExt(n, EmptyStr);
      TMI.OnClick := NewHelpClick;
      MM_Da_Help.Insert(MM_Da_Help.IndexOf(NewHelpSeparator), TMI);
    until FindNext(SR) <> 0;
  FindClose(SR);

{$IF DEFINED(DEBUG)}
  BarPanel.Color := clPurple;;
{$IFEND}

{$IF DEFINED(PRERELEASE)}
  BarPanel.Color := clGray;
{$IFEND}

  tiStarup.Enabled := True;

  for I := 0 to Pred(Dict.LanguageCount) do
  begin
    SB:= TSpeedButton.Create(self);
    SB.Parent:= LanguagePanel;
    SB.Flat:= True;
    SB.Caption:= ReduceCaption(Dict.Languages[I].Caption, 20);
    if SB.Caption <> Dict.Languages[I].Caption then SB.Hint:= Dict.Languages[I].Caption;
    SB.Tag:= I;
    SB.OnClick:= LangButtonClick;
    SB.SetBounds(22 + i * 120, 0, 120, 24);

    if ((Dict.ActiveLang = 0) and (Dict.DefaultLanguageID = Dict.Languages[I].LngID)) or (Dict.ActiveLang = Dict.Languages[I].LngID)
      then LanguageSelectPanel.SetBounds(SB.Left, 0, SB.Width, 4);
  end;

  I:= Variables.AsInteger[RegHintTime];
  if 0 < I then Application.HintHidePause := I;
end;

procedure TMForm.InitForm;
begin
  FLastMessageForm := nil;

  FWindowState := wsNormal;
  // screen.cursors[1] := LoadCursor(hInstance, pChar('CURSOR_DB'));
  ControlState := [];

  flagInfo := Variables.AsBoolean[RegDForm];
  ShowHint := Variables.AsBoolean[RegShowHint];

  SBar.Visible := Variables.AsBoolean[RegShowStatus];
  _AnimStart := Variables.AsInteger[RegAnim];

  Password_Show;
  _AnimStart := 1;

  {$IFDEF DeDEBUG}
  Funcs.WriteLog('MainForm create completed');
  {$ENDIF}
  inherited;

  UpdateWindow(Handle);
  DM.CommPort.OnReceiveData := CommPortReceiveData;
end;

procedure TMForm.Afterinit;
begin
  // inherited Afterinit;
end;

procedure TMForm.DoAfterBaseFormsChanged;
begin
  inherited;
  if Not MenuDesign then
    EndEditToolBars;
end;

procedure TMForm.OnAddClick(Sender: TObject);
var
  RecordEditor: TRecordEditor;
  DMan: TDataManager;
  TMeta: TTableMeta;
  i: Integer;
begin
  TMeta := TTableMeta(TMenuItem(Sender).Tag);

  DMan := TDataManager.Create;
  DMan.Table := TMeta;

  RecordEditor := TRecordEditor.Create(TMeta, null);
  DMan.PrepareRecord(RecordEditor.CacheItem);
  for i := 0 to TMeta.Fields.Count - 1 do
    if TMeta.Fields[i].LinkTable = TBaseDataForm(CurrentBaseForm).DataSetMeta.Table then
      if Not TMeta.Fields[i].IsLookup then
        if Assigned(TBaseDataForm(CurrentBaseForm).DataSetMeta.Cache.FocusedItem) then
          RecordEditor.CacheItem.FieldValue[i] := TBaseDataForm(CurrentBaseForm).DataSetMeta.Cache.FocusedItem.ID;

  if RecordEditor.Execute then
  begin
    DMan.SetPrimaryKey(RecordEditor.CacheItem);
    DMan.InsertRecord(RecordEditor.CacheItem);

    {
      for i:=0 to MetaData.DataSetsCount-1 do
      if MetaData.DataSets[i].Table=TMeta then
      MetaData.DataSets[i].Cache.Update(mcInsert, RecordEditor.CacheItem.ID);
    }
    metadata.UpdateDataSets(TMeta.ID, mcInsert, RecordEditor.CacheItem.ID);
    metadata.UpdateLibrary(TMeta.ID);
  end;

  RecordEditor.Free;
  DMan.Free;
end;

procedure TMForm.OnMoveToClick(Sender: TObject);
var
  aContext: TContextMeta;
begin
  aContext:= TContextMeta.Create(nil);
  aContext.Dataset:= TTableMeta(TMenuItem(Sender).Tag);
  if Assigned(aContext.Dataset) then
    DoData(nil, aContext);
  aContext.Free;
end;

procedure TMForm.OnDatasetClick(Sender: TObject);
var
  aContext: TContextMeta;
begin
  aContext:= TContextMeta.Create(nil);
  aContext.Dataset:= TTableMeta(TMenuItem(Sender).Tag);
  DoData(nil, aContext);
  aContext.Free;
end;

procedure TMForm.OnDatasetExecute(Sender: TObject);
var
  aContext: TContextMeta;
begin
  inherited;
  if Not(Sender is TGUIDAction) then Exit;
  aContext:= TContextMeta.Create(nil);
  aContext.Dataset:= Metadata.TablesList.FindByGUID( TGUIDAction(Sender).GUID);
  DoData(nil, aContext);
  aContext.Free;
end;

procedure TMForm.OnDictionaryClick(Sender: TObject);
var
  aContext: TContextMeta;
begin
  aContext:= TContextMeta.Create(nil);
  aContext.Dataset:= Metadata.MetaTables[idxLang];
  aContext.AddGroupField( Metadata.MetaTables[idxLang].Fields.FindByName(fldDictionaryLangID), null);
  DoData(nil, aContext);
  aContext.Free;
end;

procedure TMForm.OnRightsClick(Sender: TObject);
var
  aContext: TContextMeta;
begin
  aContext:= TContextMeta.Create(nil);
  aContext.Dataset:= Metadata.MetaTables[idxRights];
  aContext.AddGroupField( Metadata.MetaTables[idxRights].Fields.FindByName(fldRightsSubjectId), null);
  DoData(nil, aContext);
  aContext.Free;
end;

procedure TMForm.UpdateCreateMenu(ParentItem: TMenuItem);
var
  i: Integer;
  Item: TMenuItem;
  CBF: TBaseForm;
  t: TTableMeta;
begin
  ParentItem.Clear;
  CBF := CurrentBaseForm;

  if Assigned(CBF) and (CBF is TBaseDataForm) then
  begin
    t := TBaseDataForm(CBF).DataSetMeta.Table;

    Item := TMenuItem.Create(ParentItem);
    Item.Caption := GetTitle(t.Name);
    Item.Tag := NativeInt(t);
    Item.OnClick := OnAddClick;
    Item.Default := True;
    Item.Enabled := (t <> metadata.MetaTables[idxInterrelations]);
    ParentItem.Add(Item);

    Item := TMenuItem.Create(ParentItem);
    Item.Caption := cLineCaption;
    ParentItem.Add(Item);

    for i := 0 to t.Links.Count - 1 do
      if (t.Links[i].Owner.ID <> metadata.MetaTables[idxInterrelations].ID) and
        (t.Links[i].Owner.ID <> metadata.MetaTables[idxNotes].ID) then
      begin
        Item := TMenuItem.Create(ParentItem);
        Item.Caption := GetTitle(t.Links[i].Owner.Name);
        Item.Tag := NativeInt(t.Links[i].Owner);
        Item.OnClick := OnAddClick;
        ParentItem.Add(Item);
      end;

    Item := TMenuItem.Create(ParentItem);
    Item.Caption := cLineCaption;
    ParentItem.Add(Item);

    Item := TMenuItem.Create(ParentItem);
    Item.Action := TBaseDataForm(CBF).actCreate_Da_Shortcut;
    ParentItem.Add(Item);

    Item := TMenuItem.Create(ParentItem);
    Item.Action := TBaseDataForm(CBF).actCreate_Da_Note;
    ParentItem.Add(Item);
  end;
end;

procedure TMForm.MM_dTgoClick(Sender: TObject);
var
  Item: TMenuItem;
  i, CurrentID: Integer;
begin
  for i := MM_da_Go.Count - 1 downto 0 do
    if (MM_da_Go.Items[i].GroupIndex = 99) and
      (MM_da_Go.Items[i] <> miMoveToDelimiter) then
      MM_da_Go.Delete(i);

  if (Not Assigned(UserSession)) or (Not Assigned(CurrentBaseForm)) or
    (Not(CurrentBaseForm is TBaseDataForm)) then
    Exit;

  CurrentID := TBaseDataForm(CurrentBaseForm).DataSetMeta.Table.ID;

  for i := 0 to metadata.TableCount - 1 do
    if metadata.Tables[i].OnAddMenu and (CurrentID <> metadata.Tables[i].ID) then
      if SecuritySystem.CheckPolicyDataset(metadata.Tables[i].ID, spSelect) then
        begin
          Item := TMenuItem.Create(MM_da_Go);
          Item.Caption := GetTitle(metadata.Tables[i].Name, ttSecondName);
          Item.Tag := NativeInt(metadata.Tables[i]);
          Item.OnClick := OnMoveToClick;
          Item.GroupIndex := 99;
          Item.ImageIndex := DM.MapIconIndex(metadata.Tables[i].Ico);
          MM_da_Go.Add(Item);
        end;
end;

procedure TMForm.act_Da_MailAsExecute(Sender: TObject);
begin
  Shellexecute(0, 'open', PChar(dicAppMailURL), nil, nil, sw_shownormal);
end;

procedure TMForm.act_Da_ShowCardAreaExecute(Sender: TObject);
var
  B: Boolean;
begin
  B := not act_Da_ShowCardArea.Checked;
  act_Da_ShowCardArea.Checked := B;

  if Assigned(ActiveMeta) then
    if B then TBaseGridForm(ActiveMeta.GridForm).ShowViewArea(True)
         else TBaseGridForm(ActiveMeta.GridForm).HideViewArea(True);
end;

procedure TMForm.StoreEnvironment(aHistoryData: TContextMeta);
var
  Index: Integer;
  XML: string;
begin
  if Assigned(aHistoryData) and Assigned(DefaultMeta) then
  begin
    if Assigned(DefaultMeta) and Assigned(DefaultMeta.Cache.FocusedItem) then
      aHistoryData.RecordKey := DefaultMeta.Cache.FocusedItem.ID
    else
      aHistoryData.RecordKey := null;

    aHistoryData.SelectedKeys:= DefaultMeta.Cache.SelectedKeys ;
    {
      aHistoryData.ClearSelectedKeys;
      if DefaultMeta.Cache.SelectedCount<DefaultMeta.Cache.Count then
      aHistoryData.AddSelectedKey(DefaultMeta.Cache);
      // А теперь представь 100 000 выделенных записей будет перераспределение памяти?
      // Жесть, комментирую и вызываю перегруженный метод...
      //for Index := 0 to DefaultMeta.Cache.SelectedCount-1 do
      //  aHistoryData.AddSelectedKey(DefaultMeta.Cache.SelectedItems[Index].ID);
    }
    aHistoryData.Groups.Clear;

    if Assigned(DefaultMeta) and assigned(DefaultMeta.GridForm) then
    begin
      for Index := 0 to Pred(DefaultMeta.OwnerLinks.Count) do
        if Assigned(DefaultMeta.OwnerLinks[Index].DataSetMeta.Cache.FocusedItem) then
          aHistoryData.AddGroupField(DefaultMeta.OwnerLinks[Index].LinkField,
            DefaultMeta.OwnerLinks[Index].DataSetMeta.Cache.FocusedItem.ID);
      aHistoryData.ViewParams := TBaseGridForm(DefaultMeta.GridForm).GetPropertiesString;
      XML := EmptyStr;
      if Assigned(TBaseGridForm(DefaultMeta.GridForm).FilterControl) then
        if not TBaseGridForm(DefaultMeta.GridForm).FilterControl.WriteFilterToXML(XML) then
          XML := EmptyStr;
      aHistoryData.FilterXML := XML;
      aHistoryData.FilterActive := TBaseGridForm(DefaultMeta.GridForm).pnlFilter.Visible;
      aHistoryData.FilterVisible := TBaseGridForm(DefaultMeta.GridForm).act_Da_Filter.Checked;
    end;
  end;
end;

procedure TMForm.MainBeforeSyncronize(var aCacheItem: TCacheItem; AIndex : Integer);
// var NewCacheItem : TCacheItem;
begin
  if Assigned(FOldBeforeSyncronize) then
    FOldBeforeSyncronize(aCacheItem, AIndex);
  {
    if Assigned(aCacheItem)
    and (not VarIsEmpty(DeHistory.CurrentItem.RecordKey))
    and (not VarIsNull(DeHistory.CurrentItem.RecordKey)) then
    begin
    NewCacheItem :=
    aCacheItem.Owner.FindByID(DeHistory.CurrentItem.RecordKey);
    if Assigned(NewCacheItem) then
    aCacheItem := NewCacheItem;
    end;
    { }
end;

procedure TMForm.ToolBarMenuClick(Sender: TObject);
var
  MI: TMenuItem;
begin
  MI := TMenuItem(Sender);

  ToolbarList[MI.Tag].Visible := MI.Checked;
end;

procedure TMForm.StartEditToolBars;
var
  i: Integer;
begin
  pnlClientArea.Enabled := False;
  BarPanel.PopupMenu := nil;
  BottomBarPanel.PopupMenu := nil;
//  LeftBarPanel.PopupMenu := nil;
//  RightBarPanel.PopupMenu := nil;

  for i := 0 to ToolbarList.Count - 1 do
    ToolbarList[i].EditBegin;

  MenuDesign := True;
  pnlClientArea.Enabled := False;
end;

procedure TMForm.EndEditToolBars;
var
  i: Integer;
begin
  MenuDesign := False;
  pnlClientArea.Enabled := True;

  for i := 0 to ToolbarList.Count - 1 do
    ToolbarList[i].EditEnd;

  pnlClientArea.Enabled := True;
  BarPanel.PopupMenu := pmToolBars;
  BottomBarPanel.PopupMenu := pmToolBars;
//  LeftBarPanel.PopupMenu := pmToolBars;
//  RightBarPanel.PopupMenu := pmToolBars;

  MainMenu.Items.Enabled := True;
end;

function TMForm.CreateSeparator: TDeToolButton;
begin
  Result := TDeToolButton.Create(Application);
  Result.ActionName:= '|';
  Result.Style := tbsSeparator;
  Result.DragKind := dkDock;
  Result.DragMode := dmManual;
  Result.OnStartDock := BackBtnStartDock;
  Result.OnEndDock := BackBtnEndDock;
  Result.Action := nil;
  Result.Width  := 6;
  Result.Height := 16;
  Result.AutoSize := False;
  Result.FloatingDockSiteClass := TDeToolBtnDockForm;
end;

function TMForm.CreateButton(Source: TAction): TDeToolButton;
begin
  if Assigned(Source) then
  begin
    Result := TDeToolButton.Create(Application);
    if Source is TGUIDAction then
      Result.ActionName := GUIDtoString(TGUIDAction(Source).GUID)
    else
      Result.ActionName := Source.Name;

    Result.Style := tbsButton;
    Result.DragKind := dkDock;
    Result.DragMode := dmManual;
    Result.OnStartDock := BackBtnStartDock;
    Result.OnEndDock := BackBtnEndDock;
    Result.Action := Source;
    Result.AutoSize := True;
    Result.FloatingDockSiteClass := TDeToolBtnDockForm;
  end
  else
    Result := nil;
end;

procedure TMForm.InitToolButton(B: TDeToolButton);
var j: Integer;
    ADS: TDataSetMeta;
  procedure PrepareButtonMenu(B: TDeToolButton; aSource: TActionList; aCategory: String; aRadio: Boolean = False);
  var i: Integer;
  begin
    if not Assigned(B.DropDownMenu) then B.DropDownMenu:= TPopupMenu.Create(B)
                                    else B.DropDownMenu.Items.Clear;
    B.Style:= tbsDropDown;
    B.DropDownMenu.Images:= DM.ilIcon16;

    for i:=Pred(aSource.ActionCount) downto 0 do
      if SameText(aSource.Actions[i].Category, aCategory ) then
        begin
          B.DropDownMenu.Items.Insert(0, TMenuItem.Create(B.DropDownMenu));
          B.DropDownMenu.Items[0].Action:= aSource.Actions[i];
          B.DropDownMenu.Items[0].RadioItem:= aRadio;
        end;
    B.Enabled:= (0 < B.DropDownMenu.Items.Count);
  end;
begin
  ADS:=ActiveMeta;

  if CompareText(B.ActionName, '|') = 0 then
    begin
      B.style:= tbsSeparator;
    end else

  if CompareText(B.ActionName, act_Da_Create.Name ) = 0 then
    begin
      B.style:= tbsDropDown;
      B.DropDownMenu := CreatePopup;

      ADS:=ActiveMeta;
      if assigned(ADS) then
        for j := 0 to ADS.GridForm.ActionList.ActionCount - 1 do
          if CompareText(B.ActionName, ADS.GridForm.ActionList[j].Name) = 0 then
             begin B.Action := ADS.GridForm.ActionList[j]; Exit; end;
    end else

  if CompareText(B.ActionName, 'act_Da_Filter') = 0 then
    begin
      if assigned(ADS) then begin
                              B.Action:= TBaseGridForm(ADS.GridForm).act_Da_Filter;
                              B.DropDownMenu:= TBaseGridForm(ADS.GridForm).FilterPopupMenu;
                              B.Style:= tbsDropDown;
                              B.Enabled:= True;
                            end
                       else B.Enabled:= False;
    end else

  if CompareText(B.ActionName, 'Go_Da_Presentation') = 0 then
    begin
      if assigned(ADS) then PrepareButtonMenu(B, TBaseGridForm(ADS.GridForm).ActionList, CategoryPresent, True)
                       else B.Enabled:= False;
    end else

  if CompareText(B.ActionName, 'Go_Da_View') = 0 then
    begin
      if assigned(ADS) then PrepareButtonMenu(B, TBaseGridForm(ADS.GridForm).ActionList, CategoryView, True)
                       else B.Enabled:= False;
    end else

  if CompareText(B.ActionName, 'Go_Da_Order') = 0 then
    begin
      if assigned(ADS) then PrepareButtonMenu(B, TBaseGridForm(ADS.GridForm).TempActionList, CategorySort, False)
                       else B.Enabled:= False
    end else

  if CompareText(B.ActionName, 'Go_Da_SplitBy') = 0 then
    begin
      if assigned(ADS) then PrepareButtonMenu(B, TBaseGridForm(ADS.GridForm).TempActionList, CategorySplit, True)
                       else B.Enabled:= False;
    end else

  if CompareText(B.ActionName, Go_Da_Back.Name ) = 0    then
    begin
      B.style:= tbsDropDown;
      B.DropDownMenu := BackMenu;
    end else

  if CompareText(B.ActionName, Go_Da_Forward.Name ) = 0 then
    begin
      B.style:= tbsDropDown;
      B.DropDownMenu := ForwardMenu;
    end else

    begin
      B.style:= tbsButton;

      ADS:=ActiveMeta;
      if assigned(ADS) then
        for j := 0 to ADS.GridForm.ActionList.ActionCount - 1 do
          if CompareText(B.ActionName, ADS.GridForm.ActionList[j].Name) = 0 then
             begin B.Action := ADS.GridForm.ActionList[j]; Exit; end;

      if not Assigned(B.Action) then
        for j := 0 to MainActionList.ActionCount - 1 do
          if CompareText(B.ActionName, MainActionList[j].Name) = 0 then
             begin B.Action := MainActionList[j]; Exit; end;

      if not Assigned(B.Action) and (Copy(B.ActionName,1,1) = '{') then
        for j:=0 to Pred(MainActionList.ActionCount) do
          if MainActionList[j] is TGUIDAction then
            if GUIDToString(TGUIDAction(MainActionList[j]).GUID) = B.ActionName then
              begin B.Action := MainActionList[j]; Exit; end;
    end;
end;

function TMForm.CreateButtonByName(const aName: string): TDeToolButton;
var
  i: Integer;
  Source: TComponent;
  MenuMeta: TContextMeta;
  MenuItem: TMenuItem;
  MenuAction: TComponent;
  MenuID, MenuIndex: Integer;
begin
  Result := nil;
  if aName = '|' then           Exit(CreateSeparator);

  Source := FindComponent(aName);
  if not Assigned(Source) then Source := ActionList.FindComponent(aName);
  if not Assigned(Source) then Source := MainActionList.FindComponent(aName);
  if not Assigned(Source) then
    for i:=0 to Pred(MainActionList.ActionCount) do
      if MainActionList[i] is TGUIDAction then
        if GUIDToString(TGUIDAction(MainActionList[i]).GUID) = aName then
          begin
           Source := MainActionList[i];
           Break;
          end;


  if Source is TAction then     Result := CreateButton(TAction(Source));

  if Assigned(Result) then
  if Assigned(SecuritySystem) and Assigned(Result.Action) and
          (pos('acMainMenuMeta', Result.Action.Name) = 1) and
          (TObject(Result.Action.Tag) is TContextMeta) then
        begin
          MenuMeta := TContextMeta(Result.Action.Tag);
          if MenuMeta.Count <> 0 then
            for MenuIndex := 0 to Pred(MenuMeta.Count) do
            begin
              MenuID := MenuMeta[MenuIndex].ID;
              if SecuritySystem.CheckPolicyMenu(MenuID, spSelect) then
              begin
                MenuAction := MainActionList.FindComponent(Format('acMainMenuMeta%u', [MenuID]));
                if Assigned(MenuAction) and (MenuAction is TAction) then
                  begin
                    if not Assigned(Result.DropdownMenu) then
                      begin
                        Result.DropdownMenu := TPopupMenu.Create(MForm);
                        Result.Style := tbsDropDown;
                        Result.DropdownMenu.Images := MainActionList.Images;
                      end;
                    MenuItem := TMenuItem.Create(Result.DropdownMenu);
                    try
                      MenuItem.Action := MenuAction as TAction;
                      Result.DropdownMenu.Items.Add(MenuItem);
                    except
                      MenuItem.Free;
                      raise;
                    end;
                  end
                else
                  begin
                    ///
                    Break;
                  end;
              end;
            end;
        end;

end;

procedure TMForm.LoadHotKeys;
var
  i: Integer;
  aCmp: TComponent;
  L: tStringList;
begin
  L := tStringList.Create;
  try
    L.CommaText := Variables.AsString[RegToolBars_ShortCuts];
    for i := 0 to L.Count - 1 do
    begin
      aCmp := ActionList.FindComponent(L.Names[i]);

      if Not Assigned(aCmp) then
        aCmp := MainActionList.FindComponent(L.Names[i]);

      if Assigned(aCmp) and (aCmp is TAction) then
        TAction(aCmp).ShortCut := TextToShortCut(L.Values[aCmp.Name]);
    end;
  finally
    L.Free;
  end;
end;

procedure TMForm.SaveHotKeys;
var
  i: Integer;
  L: tStringList;
begin
  L := tStringList.Create;
  try
    for i := 0 to ActionList.ActionCount - 1 do
      L.Values[ActionList.Actions[i].Name] :=
        ShortCutToText(ActionList.Actions[i].ShortCut);

    for i := 0 to MainActionList.ActionCount - 1 do
      L.Values[MainActionList.Actions[i].Name] :=
        ShortCutToText(MainActionList.Actions[i].ShortCut);

    Variables.AsString[RegToolBars_ShortCuts] := L.CommaText;
  finally
    L.Free;
  end;
end;

procedure TMForm.SaveToolBars;
begin
  ToolbarList.Save;
  SaveHotKeys;
end;

procedure TMForm.CreateToolBarsMenu(AMenuItem: TMenuItem);
var
  Index, MenuIndex: Integer;
  MenuItem: TMenuItem;
begin
  for Index := Pred(AMenuItem.Count) downto 0 do
    if AMenuItem.Items[Index].Tag >= 0 then
      AMenuItem.Delete(Index);

  if Assigned(ToolbarList) then
    begin
      MenuIndex := 0;
      for Index := 0 to Pred(ToolbarList.Count) do
        begin
          MenuItem := TMenuItem.Create(AMenuItem);
          try
            MenuItem.Caption := GetTitle(ToolbarList[Index].DisplayName);
            MenuItem.Hint := EmptyStr;
            MenuItem.AutoCheck := True;
            MenuItem.Checked := ToolbarList[Index].Visible;
            MenuItem.OnClick := ToolBarMenuClick;
            MenuItem.Tag := Index;
            AMenuItem.Insert(MenuIndex, MenuItem);
          except
            MenuItem.Free;
            raise;
          end;
          inc(MenuIndex);
        end;
    end;
end;

procedure TMForm.pmMenuToolBarsPopup(Sender: TObject);
begin
  CreateToolBarsMenu(pmMenuToolBars.Items);
end;

procedure TMForm.pmToolBarsPopup(Sender: TObject);
begin
  CreateToolBarsMenu(pmToolBars.Items);
end;

procedure TMForm.MM_ViewClick(Sender: TObject);
begin
  inherited;
  CreateToolBarsMenu(MM_Dl_ToolBars);
  if Assigned(ActiveMeta) then
    act_Da_ShowCardArea.Checked := TBaseGridForm(ActiveMeta.GridForm).ViewArea;
end;

procedure TMForm.BarPanelGetSiteInfo(Sender: TObject; DockClient: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  inherited;
  CanDock := (DockClient is TToolBar);
end;

var
  _DragObject: TDragDockObject;

procedure TMForm.tb_AdditionalToolbarStartDock(Sender: TObject; var DragObject: TDragDockObject);
begin
  _DragObject := TToolDockObject.Create(TControl(Sender));
  DragObject := _DragObject;
end;

procedure TMForm.tiStarupTimer(Sender: TObject);
begin
  with Sender as TTimer do Enabled := False;
  // Проверка на запуск в командном режиме ...
  ExecuteCommandLineActions;
end;

procedure TMForm.tb_AdditionalToolbarEndDock(Sender, Target: TObject; X, Y: Integer);
begin
  _DragObject.Free;
end;

procedure TMForm.Go_Da_SettingsExecute(Sender: TObject);
begin
  ShowToolBarsSettings;
end;

procedure TMForm.Go_Da_RememberExecute(Sender: TObject);
begin
  Rem.Show;
end;

procedure TMForm.setToolBarStyle(aTB: TToolBar; aImageSize, aCaption: Integer);
begin
  aTB.DisableAlign;
  aTB.AutoSize := False;

  aTB.Images:= DM.GetEImageList(aImageSize);
  aTB.HotImages:= DM.GetHImageList(aImageSize);
  aTB.DisabledImages:= DM.GetDImageList(aImageSize);

  aTB.ShowCaptions := aCaption in [1, 2];
  aTB.List := aCaption in [2];

  aTB.Width := aTB.ButtonWidth * aTB.ButtonCount;
  aTB.Constraints.MinHeight := aTB.ButtonHeight;
  aTB.Constraints.MinWidth := aTB.ButtonWidth;

  aTB.AutoSize := True;
  aTB.EnableAlign;
end;

procedure TMForm.UpdateToolBars(Sender: TObject);
var
  i: Integer;
  Rect: TRect;
begin
  BarPanel.DisableAlign;
  for i := 0 to ToolbarList.Count - 1 do
    if Assigned(ToolbarList[i].ToolBar) then
      setToolBarStyle(ToolbarList[i].ToolBar, MenuIcoSize, MenuCaption);

  // Rect := BarPanel.ClientRect;// .GetClientRect;
  // AlignControls(BarPanel, Rect);
  // BarPanel.ControlState := BarPanel.ControlState - [csAlignmentNeeded];

  BarPanel.EnableAlign;

  for i := 0 to metadata.DataSetsCount - 1 do
    if metadata.DataSets[i].CardForm <> nil then
      TAForm(metadata.DataSets[i].CardForm).ReloadImages;
end;

procedure TMForm.tbAllCommandsGetSiteInfo(Sender: TObject; DockClient: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  inherited;
  CanDock := (DockClient is TListBox) or (DockClient is TToolButton);
  InfluenceRect := TToolBar(Sender).ClientRect;
  MapWindowPoints(TToolBar(Sender).Handle, 0, InfluenceRect, 2);
end;

function CalcDockRect(aTB: TWinControl; X, Y: Integer): TRect;
var
  aCtrl: TControl;
  ARect: TRect;
  midX: Integer;
begin
  aCtrl := aTB.ControlAtPos(Point(X, Y), True);
  if aCtrl <> nil then
  begin
    ARect := aCtrl.BoundsRect;
    midX := (ARect.Left + ARect.Right) div 2;
    if (X < midX) then
    begin
      ARect.Right := ARect.Left;
      ARect.Left := ARect.Left - 5;
    end
    else
    begin
      ARect.Left := ARect.Right - 5;
    end;
  end
  else
  begin
    ARect.TopLeft := Point(0, 0);
    ARect.BottomRight := Point(5, aTB.ClientHeight);
  end;
  Result := ARect;
end;

procedure TMForm.tbAllCommandsDockOver(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  aTB: TToolBar;
  ARect: TRect;
begin
  Accept := Source.Control is TToolButton;
  if not Accept then
    Exit;
  aTB := TToolBar(Sender);
  if State = dsDragLeave then
    Exit;
  ARect := CalcDockRect(aTB, X, Y);
  MapWindowPoints(aTB.Handle, 0, ARect, 2);
  Source.DockRect := ARect;
end;

procedure TMForm.BackBtnStartDock(Sender: TObject;
  var DragObject: TDragDockObject);
var
  aBtn: TToolButton;
begin
  aBtn := TToolButton(Sender);
  DragObject := TDeToolBarDockObject.Create(aBtn);
end;

procedure TMForm.BackBtnEndDock(Sender, Target: TObject; X, Y: Integer);
var
  aTB: TToolBar;
  ARect: TRect;
begin
  // Screen.Cursor := crDefault;
  if (Target = nil) or (Target is TControl(Sender).FloatingDockSiteClass) then
    Sender.Free
  else if Target is TToolBar then
  begin
    aTB := TToolBar(Target);
    ARect := CalcDockRect(aTB, X, Y);
    aTB.DisableAlign;
    TToolButton(Sender).Left := ARect.Left;
    aTB.EnableAlign;
    aTB.Invalidate;
  end;
end;

procedure TMForm.tbAllCommandsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aTB: TToolBar;
  aCtrl: TControl;
begin
  aTB := TToolBar(Sender);
  aCtrl := aTB.ControlAtPos(Point(X, Y), True);
  if aCtrl = nil then
    aTB.BeginDrag(False);
end;

procedure TMForm.tbAllCommandsResize(Sender: TObject);
var
  aTB: TToolBar;
  aBtn: TToolButton;
  i: Integer;
  f: Boolean;
begin
  aTB := TToolBar(Sender);
  f := True;
  for i := 0 to aTB.ButtonCount - 1 do
  begin
    aBtn := aTB.Buttons[i];
    if aBtn.Style = tbsSeparator then
    begin
      aBtn.Visible := not(f);
      f := True;
    end
    else
      f := f and not(aBtn.Visible);
  end;
end;

procedure TMForm.MM_dTgoDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
begin
  // inherited;
end;

procedure TMForm.Go_dA_DataExportExecute(Sender: TObject);
begin
  with TDeMaster.Create(Application) do
    try
      Reinit(moExport);
      ShowModal;
    finally;
      Free;
    end;
end;

procedure TMForm.Go_dA_DataImportExecute(Sender: TObject);
begin
  with TDeMaster.Create(Application) do
    try
      Reinit(moImport);
      ShowModal;
    finally;
      Free;
    end;
end;

procedure TMForm.MM_Da_ActionsClick(Sender: TObject);
begin
  inherited;
  if Assigned(ActiveMeta) then
    ActiveMeta.UpdateActions;
end;

/// //////////////////////////// процедуры автоподстановки значений для TEdit ///

procedure TMForm.Edit1Change(Sender: TObject);
const
  Z: array [0 .. 7] of string = ('Андрей', 'Евгений', 'Иван', 'Юрий', 'Ольга', 'Екатерина', 'Александр', 'Алексей');
var
  TE: TEdit;
  s: string;
  i, L: Integer;
begin
  inherited;
  TE := TEdit(Sender);
  s := TE.Text;
  L := Length(s);
  if (L > 1) and (TE.SelStart = L) and (TE.SelLength = 0) then
    for i := 0 to 7 do
      if s = copy(Z[i], 1, L) then
      begin
        TE.OnChange := nil;

        TE.SelText := copy(Z[i], L + 1, MaxInt);
        TE.SelStart := Length(s);
        TE.SelLength := MaxInt;

        TE.OnChange := Edit1Change;
        Break;
      end;
end;

procedure TMForm.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key In [8, 46] then
    TEdit(Sender).OnChange := nil
  else
    TEdit(Sender).OnChange := Edit1Change;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMForm.ApplicationMinimize(Sender: TObject);
begin
  tiTask.Visible := True;
end;

procedure TMForm.ApplicationRestore(Sender: TObject);
begin
  tiTask.Visible := False;
end;

procedure TMForm.BuildGlobalFilterMenu;
var
  Position, Index: Integer;
  MenuItem: TMenuItem;
begin
  {$IFDEF DEBUG}
  DebugLog(ClassName + '.BuildGlobalFilterMenu start ...');
  {$ENDIF}
  Position := MM_Da_Representation.IndexOf(MMRemoved);
  for Index := Pred(MM_Da_Representation.Count) downto Succ(Position) do
    MM_Da_Representation.Delete(Index);
  if Assigned(GroupFilters) then
    for Index := 0 to Pred(GroupFilters.Count) do
      if GroupFilters[Index].IsGlobal and GroupFilters[Index].Visible then
      begin
        MenuItem := TMenuItem.Create(MM_Da_Representation);
        try
          MenuItem.Caption := GetTitle(GroupFilters.Names[Index], ttFirstName);
          MenuItem.Hint := GetTitle(GroupFilters.Names[Index], ttSecondName);
          MenuItem.Tag := Index;
          MenuItem.Checked := GroupFilters[Index].Active;
          MenuItem.OnClick := CheckGlobalFilter;
          MM_Da_Representation.Add(MenuItem);
        except
          MenuItem.Free;
          raise;
        end;
      end;
  if MM_Da_Representation.Count > Succ(Position) then
  begin
    MenuItem := TMenuItem.Create(MM_Da_Representation);
    try
      MenuItem.Caption := '-';
      MM_Da_Representation.Insert(Succ(Position), MenuItem);
    except
      MenuItem.Free;
      raise;
    end;
  end;
  {$IFDEF DEBUG}
  DebugLog(ClassName + '.BuildGlobalFilterMenu finish ...');
  {$ENDIF}
end;

procedure TMForm.CheckGlobalFilter(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    if Assigned(GroupFilters) then
    begin
      GroupFilters[Tag].Active := Checked;
      GroupFilters[Tag].UpdateFilters;
    end;
  end;
end;

procedure TMForm.ClearSwapMenu;
var
  MenuItem: TMenuItem;
begin
  if Assigned(FSwapMenuList) then
    while FSwapMenuList.Count <> 0 do
    begin
      MenuItem := FSwapMenuList[0];
      MenuItem.Visible := False;
      MenuItem.Free;
      FSwapMenuList.Delete(0);
    end;
end;

procedure TMForm.ReAlignSwapMenu(M: TMainMenu);
var
  MenuItemInfo: TMenuItemInfo;
  Buffer: array [0 .. 255] of Char;
  Index, N: Integer;
begin
  if Not Assigned(M) then
    Exit;

  N:=0;  // Считаем пункты пользовательского меню, которые надо перенести направо
  for Index := Pred(M.Items.Count) downto 0 do
    if M.Items[Index] is TDeMenuItem then Inc(N);

  // Перенесим направо N последних пунктов
  for Index := Pred(M.Items.Count) downto 0 do
    begin
      MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
      MenuItemInfo.fMask := MIIM_TYPE; // or MIIM_STATE;
      MenuItemInfo.dwTypeData := Buffer;
      MenuItemInfo.cch := SizeOf(Buffer);

      if GetMenuItemInfo(M.Handle, Index, True, MenuItemInfo) then
        begin
          MenuItemInfo.fType := MenuItemInfo.fType or MFT_RIGHTJUSTIFY;
          SetMenuItemInfo(M.Handle, Index, True, MenuItemInfo);
          Dec(N);
          if N=0 then Break;
        end;
    end;
end;

procedure TMForm.BuildSwapMenu;
var
  TempMainMenu: TMainMenu;
  MenuItem: TDeMenuItem;
  Index, Position: Integer;

  procedure BuildSubMenu(MainMenuMeta: TContextMeta; RootItem: TMenuItem);
  var
    MenuItem: TMenuItem;
    Index: Integer;
  begin
    for Index := Pred(MainMenuMeta.Count) downto 0 do
      if SecuritySystem.CheckPolicyMenu(MainMenuMeta[Index].ID, spSelect) or
        (Assigned(UserSession) and UserSession.IsAdmin) then
        if (Assigned(UserSession) and
          (MainMenuMeta[Index].SubjectID = UserSession.ID)) or
          (MainMenuMeta[Index].SubjectID < 1) then
        begin
          MenuItem := TMenuItem.Create(MainMenu);
          try
            MenuItem.Tag := NativeInt(MainMenuMeta[Index]);
            if (Length(Trim(MainMenuMeta[Index].Caption)) = 0) and
              (Assigned(MainMenuMeta[Index].Dataset)) then
              MenuItem.Caption := MainMenuMeta[Index].Dataset.Name
            else
              MenuItem.Caption := MainMenuMeta[Index].Caption;
            MenuItem.Hint := MainMenuMeta[Index].Hint;
            MenuItem.ImageIndex := -1;
            // Index;//DM.MapIconIndex(MainMenuMeta[Index].ICO);
            MenuItem.OnClick := MetaMenuItemClick;
            RootItem.Insert(0, MenuItem);
            FSwapMenuList.Insert(0, MenuItem);
          except
            MenuItem.Free;
            raise;
          end;
        end;
  end;

begin
  TempMainMenu := TMainMenu.Create(Self);
  for Index := 0 to MainMenu.Items.Count - 1 do
  begin
    MenuItem := TDeMenuItem.Create(TempMainMenu);
    MenuItem.Caption := MainMenu.Items[Index].Caption;
    MenuItem.Visible := MainMenu.Items[Index].Visible;
    MenuItem.Enabled := MainMenu.Items[Index].Enabled;
    MenuItem.ImageIndex := -1;
    TempMainMenu.Items.Add(MenuItem);
  end;

  Menu := TempMainMenu;
  ClearSwapMenu;
  if Assigned(MainMenuMeta) and Assigned(SecuritySystem) then
  begin
    if not Assigned(FSwapMenuList) then
      FSwapMenuList := TList.Create;
    Position := MainMenu.Items.Count;
    for Index := Pred(MainMenuMeta.Count) downto 0 do
      if SecuritySystem.CheckPolicyMenu(MainMenuMeta[Index].ID, spSelect) or
        (Assigned(UserSession) and UserSession.IsAdmin) then
      begin
        MenuItem := TDeMenuItem.Create(MainMenu);
        try
          MenuItem.Caption := GetTitle(MainMenuMeta[Index].Name);
          MenuItem.Hint := GetTitle(MainMenuMeta[Index].Name, ttSecondName);
          MenuItem.Tag := NativeInt(MainMenuMeta[Index]);
          MenuItem.ImageIndex := -1;

          if MainMenuMeta[Index].Count <> 0 then
            BuildSubMenu(MainMenuMeta[Index], MenuItem)
          else
            MenuItem.Enabled := False;
          FSwapMenuList.Add(MenuItem);

          MainMenu.Items.Insert(Position, MenuItem);
        except
          MenuItem.Free;
          raise;
        end;
      end;
  end;

  ReAlignSwapMenu(MainMenu);
  Menu := MainMenu;
  DrawMenuBar(Handle);
  TempMainMenu.Free;
end;

procedure TMForm.ClearDatasetsList;
begin
  ClearCategoryActionList(MainActionList, categoryDatasets);
end;

procedure TMForm.BuildDatasetsList;
var I:Integer;
    Action: TGUIDAction;
begin
   for I := 0 to Pred(MetaData.TablesList.Count) do
    if SecuritySystem.CheckPolicyDataset(MetaData.TablesList[I].ID, spSelect) or (Assigned(UserSession) and UserSession.IsAdmin) then
    if not VarisNull(MetaData.TablesList[I].VariantGUID) then
      begin
        Action := TGUIDAction.Create(MainActionList);
        try
          Action.ActionList := MainActionList;
          Action.GUID := MetaData.TablesList[I].GUID;
          Action.Caption := GetTitle(MetaData.TablesList[I].Name);
          Action.Hint := GetTitle(MetaData.TablesList[I].Name, ttSecondName);
          Action.MapImageIndex:=  EncodeIcon( 224, 0, 0, (MetaData.TablesList[I].Ico and $0FFF) mod 256,
                                                         (MetaData.TablesList[I].Ico and $0FFF) div 256, 0, 12, 0 );
          Action.OnExecute := OnDatasetExecute;
          Action.Category := categoryDatasets;
        except
          Action.Free;
          raise;
        end;
      end;
end;

procedure TMForm.ClearMainMenuList;
begin
  if Assigned(ToolbarList) then
    ToolbarList.ClearFromMainMenu;

  ClearCategoryActionList(MainActionList, CategoryMenuItems);
end;

procedure TMForm.BuildMainMenuList;
var
  Index: Integer;

  procedure BuildSubMenuActions(const RootMenuMeta: TContextMeta);
  var
    Index: Integer;
    Action: TAction;
  begin
    if Assigned(RootMenuMeta) and Assigned(SecuritySystem) then
      for Index := 0 to Pred(RootMenuMeta.Count) do
        if SecuritySystem.CheckPolicyMenu(RootMenuMeta[Index].ID, spSelect) or
          (Assigned(UserSession) and UserSession.IsAdmin) then
        begin
          Action := TAction.Create(MainActionList);
          try
            Action.ActionList := MainActionList;
            Action.Tag := NativeInt(RootMenuMeta[Index]);
            Action.Name := Format('acMainMenuMeta%u', [VarToInt(RootMenuMeta[Index].ID)]);
            Action.Caption := RootMenuMeta[Index].Caption;
            Action.Hint := GetTitle(RootMenuMeta[Index].Name, ttSecondName);
            Action.ImageIndex := DM.MapIconIndex(RootMenuMeta[Index].Ico);
            Action.OnExecute := MetaMenuItemClick;
            Action.Category := CategoryMenuItems;
          except
            Action.Free;
            raise;
          end;
          if RootMenuMeta[Index].Count <> 0 then
            BuildSubMenuActions( RootMenuMeta[Index]);
        end;

//    DM.MapActionListIcons(MainActionList);
  end;

begin
  if Assigned(MainMenuMeta) and Assigned(SecuritySystem) then
    for Index := 0 to Pred(MainMenuMeta.Count) do
      if SecuritySystem.CheckPolicyMenu(MainMenuMeta[Index].ID, spSelect) or
        (Assigned(UserSession) and UserSession.IsAdmin) then
        BuildSubMenuActions(MainMenuMeta[Index]);

  if Assigned(ToolbarList) then
    begin
      ToolbarList.ClearFromMainMenu;
      ToolbarList.LoadFromMainMenu;
    end;
end;

function TMForm.GetCommandLineIdentType(const aIdent: string; var aType: TFieldType; aDataObject: pointer): Boolean;
var VariableItem: TVariableItem;
    N: Integer;
begin
  Result:= False;

  if Assigned(FCommandLineParameters) then
    begin
      N:=FCommandLineParameters.IndexByName(aIdent);
      if -1 < N then
        begin
          aType := ftUnknown;
          Exit(True);
        end;
    end;

  if Assigned(GlobalVariables) then
    begin
      VariableItem := GlobalVariables.FindVariable(aIdent);
      if Assigned(VariableItem) then
        begin
          aType:= VariableItem.DataType;
          Exit(True);
        end;
    end;
end;

function TMForm.GetCommandLineIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
var VariableItem: TVariableItem;
    N: Integer;
begin
  Result:= False;

  if Assigned(FCommandLineParameters) then
    begin
      N:=FCommandLineParameters.IndexByName(aIdent);
      if -1 < N then
        begin
          aValue := FCommandLineParameters.Items[N].Value;
          Exit(True);
        end;
    end;

  if Assigned(GlobalVariables) then
    begin
      VariableItem := GlobalVariables.FindVariable(aIdent);
      if Assigned(VariableItem) then
        begin
          aValue:= VariableItem.Value;
          Exit(True);
        end;
    end;
end;

function TMForm.ExecuteCommandLineActions: Boolean;
var
  Parser: TDeParser;
  Calculator: TDeCalculator;
  P, Index, Position, ActionID: Integer;
  Value, ActionName, ParamName, ParamValue : string;
  IntValue: Integer;
  ExtValue: Extended;
  BoolValue: Boolean;
  DateValue: TDateTime;
  VarValue: Variant;
  CalculateEnabled: Boolean;
  ExpressionItem: TExpressionItem;
begin
  Result := False;
  if Assigned(ProfitActions) then
    begin
      Parser := nil;
      Calculator := nil;
      try
        // читаем параметры
        {
        FCommandLineParameters := TDeVariableList.Create;
        for Index:= 1 to ParamCount do
          if ParamStr(Index)[1] in ['-','/'] then
            begin
              P:= Pos(':', StringReplace(ParamStr(Index),'=',':',[]));
              if 0 < P then
                begin
                  ParamName:= Copy(ParamStr(Index), 2, P-2);
                  ParamValue:= Copy(ParamStr(Index), P+1, MaxInt);
                end
              else
                begin
                  ParamName:= Copy(ParamStr(Index), 1, MaxInt);
                  ParamValue:= EmptyStr;
                end;

              FCommandLineParameters.GetByName(ParamName, True).Value:= ParamValue;
            end;

        ActionName:= FCommandLineParameters.GetValueByName('command');
        {}

        for Index := 1 to ParamCount do
          begin
            Value := ParamStr(Index);
            if (Pos('-', Value) = 1) or (Pos('/', Value) = 1) then
              begin
                System.Delete(Value, 1, 1);
                if Pos('command:', LowerCase(Value)) = 1 then
                  begin
                    System.Delete(Value, 1, 8);
                    Position := Pos(' ', Value);
                    if Position = 0 then
                      begin
                        ActionName := Trim(Value);
                        Value := EmptyStr;
                      end
                    else
                      begin
                        ActionName := Trim(Copy(Value, 1, Pred(Position)));
                        Value := Trim(Copy(Value, Succ(Position), Length(Value)));
                      end;
                    // Если пользователь не авторизирован, то ...
                    if not Assigned(UserSession) then
                      begin
                        LoadSolutionsList;
                        edSolution.ItemIndex := 0;
                        edSolutionChange(Self);
                        if ConfigList.ReadOnly and (ConfigList.Count = 1) then
                          begin
                            LoginEdit.Text := ConfigList.Items[0].DefaultLogin;
                            PassEdit.Text := ConfigList.Items[0].DefaultPassword;
                          end;
                        Pass_OkClick(Password_Ok); // Пытаемся авторизовать ...
                      end;
                    if TryStrToInt(ActionName, ActionID) then
                      ActionID := ProfitActions.IndexByID(ActionID)
                    else
                      ActionID := ProfitActions.IndexByOriginal(ActionName);
                    if ActionID <> -1 then
                      if Assigned(UserSession) then
                        begin
                          FCommandLineParameters := TDeVariableList.Create;
                          try
                            while Length(Value) <> 0 do
                              begin
                                CalculateEnabled := False;
                                Position := Pos('=', Value);
                                if Position = 0 then
                                  begin
                                    Position := Pos(' ', Value);
                                    if Position = 0 then
                                      begin
                                        ParamName := Value;
                                        Value := EmptyStr;
                                      end
                                    else
                                      begin
                                        ParamName := Trim(Copy(Value, 1, Pred(Position)));
                                        Value := Trim(Copy(Value, Succ(Position), Length(Value)));
                                      end;
                                    WriteLog('Parameter ''%s'' for action ''%s'' unassigned', [ParamName, ActionName], False, 'CommandLine');
                                    FCommandLineParameters.SetByName(ParamName, Unassigned);
                                  end
                                else
                                  begin
                                    if (Position > 1) and (Value[Pred(Position)] = ':') then
                                      begin
                                        ParamName := Trim(Copy(Value, 1, Position - 2));
                                        CalculateEnabled := True;
                                      end
                                    else
                                      ParamName := Trim(Copy(Value, 1, Pred(Position)));
                                    Value := Trim(Copy(Value, Succ(Position), Length(Value)));
                                    if (Length(Value) <> 0) and (Value[1] = '''') then
                                      begin
                                        ParamValue := EmptyStr;
                                        while Length(Value) <> 0 do
                                          if Value[1] = '''' then
                                            if (Length(Value) > 1) and (Value[2] = '''') then
                                              begin
                                                ParamValue := ParamValue + Value[1];
                                                Delete(Value, 1, 2);
                                              end
                                            else
                                              begin
                                                Delete(Value, 1, 1);
                                                Break;
                                              end
                                          else
                                            begin
                                              ParamValue := ParamValue + Value[1];
                                              Delete(Value, 1, 1);
                                            end;
                                      end
                                    else
                                      begin
                                        Position := Pos(' ', Value);
                                        if Position = 0 then
                                          begin
                                            ParamValue := Value;
                                            Value := EmptyStr;
                                          end
                                        else
                                          begin
                                            ParamValue := Trim(Copy(Value, 1, Pred(Position)));
                                            Value := Trim(Copy(Value, Succ(Position), Length(Value)));
                                          end;
                                      end;
                                    if CalculateEnabled then
                                      begin
                                        ExpressionItem := TExpressionItem.Create;
                                        try
                                          VarValue := Unassigned;
                                          try
                                            if not Assigned(Parser) then
                                              begin
                                                Parser := TDeParser.Create;
                                                Parser.onGetIdentType:= GetCommandLineIdentType;
                                              end;
                                            Parser.Parse(ParamValue, ExpressionItem);
                                          except
                                            on E: Exception do
                                              begin
                                                WriteLog('Parse parameter ''%s'' for action ''%s'' error: %s', [ParamName, ActionName, E.Message], True, 'CommandLine');
                                                ExpressionItem.Clear;
                                              end;
                                          end;
                                          try
                                            if not Assigned(Calculator) then
                                              begin
                                                Calculator := TDeCalculator.Create;
                                                Calculator.OnGetIdentValue:= GetCommandLineIdentValue;
                                              end;
                                            VarValue := Calculator.Calculate(ExpressionItem);
                                          except
                                            on E: Exception do
                                              begin
                                                WriteLog('Calculate parameter ''%s'' for action ''%s'' error: %s', [ParamName, ActionName, E.Message], True, 'CommandLine');
                                                VarValue := Unassigned;
                                              end;
                                          end;
                                        finally
                                          ExpressionItem.Free;
                                        end;
                                        FCommandLineParameters.SetByName(ParamName, VarValue);
                                      end
                                    else if TryStrToInt(ParamValue, IntValue) then
                                      FCommandLineParameters.SetByName(ParamName, IntValue)
                                    else if DeTryStrToBoolean(ParamValue, BoolValue) then
                                      FCommandLineParameters.SetByName(ParamName, BoolValue)
                                    else if TryStrToFloat(ParamValue, ExtValue) then
                                      FCommandLineParameters.SetByName(ParamName, ExtValue)
                                    else if TryStrToDateTime(ParamValue, DateValue) then
                                      FCommandLineParameters.SetByName(ParamName, DateValue)
                                    else
                                      FCommandLineParameters.SetByName(ParamName, ParamValue);
                                  end;
                              end;
                            ProfitActions[ActionID].Execute(FCommandLineParameters, False);
                            Result := True;
                          finally
                            FreeAndNil(FCommandLineParameters);
                          end;
                        end
                      else
                        WriteLog('User unauthorized for execute action ''%s''', [ActionName], True, 'CommandLine')
                    else
                      WriteLog('Action ''%s'' not found', [ActionName], True, 'CommandLine');
                  end
                else if SameText('quit', Value) then
                  begin
                    OnClose := FormClose2;
                    PostMessage(Application.Handle, WM_CLOSE, 0, 0);
                    Result := True;
                  end;
              end;
          end;
      finally
        Calculator.Free;
        Parser.Free;
      end;
    end;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}

procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('Main unit initialization ...');
  {$ENDIF}
  DeHistory := THistoryList.Create;
  RECYCLER := TObjectList.Create;
  LastSearchArea := saDataset;
end;

procedure Shutdown;
begin
  FreeAndNil(DeHistory);
  FreeAndNil(RECYCLER);
  {$IFDEF DEBUG}
  DebugLog('Main unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization

Startup;

finalization

Shutdown;

end.

