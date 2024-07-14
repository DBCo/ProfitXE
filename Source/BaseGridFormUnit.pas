unit BaseGridFormUnit;

interface

uses
  Windows, SysUtils, Classes, Contnrs, Messages, Controls, Menus, ActnList, Actions, Vcl.Graphics,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, ToolWin, ShellAPI, System.IOUtils, ImgList,
  ExtSplitter, DeTypes, DataCacheUnit, DeMeta, DSMeta, DeFilters,
  DeReport, DePrintStyles, BaseDataFormUnit, DeVariable, DeParser;

const
  WM_BGF_MESSAGE = WM_USER + 21; // сообщение базовой табличной формы
  WM_BGF_GRPCHANGE = WM_BGF_MESSAGE + 01;
  // сообщение базовой табличной формы  GROUPCHANGE;

type
  TTMenuItemHelper = class helper for TMenuItem
  private
    function GetThis: TMenuItem; Inline;
  public
    property This: TMenuItem read GetThis;
  end;

  TCaptionDirection = (cdNone, cdAuto, cdHorizontal, cdVertical);

  TBaseGridForm = class;

  TDragType = (dtSelf,     // перетаскиваем внутри одного экземпляра DS или между разными экземплярами одного DS
               dtParent,   // перетаскиваем из дочернего в родительский
               dtChild,    // перетаскиваем из родительского в дочерний
               dtOrder);   // принимающее окно учитывает порядок записей: нужны пересчеты номера по порядку


  TDragInfo = class
    Control: TWinControl;
    DragTypes: set of TDragType;
    Source: TDataSetMeta;
    Color: TColor;
    canNull: Boolean;
    // разрешено класть просто в окно, нужно для создания или перетасивания в null группу

    IField, XField, YField, OField: Integer;
    IValue, XValue, YValue        : Variant;
                            OValue: Integer;
    Rect: TRect;
    Align: TAlign;
    function CanItem: Boolean;
    function CanXY: Boolean;
    function CanOrder: Boolean;
    function СanAny: Boolean;
  end;

  TSelectRect = class(TObject)
  private type
    TLineGraphicControl = class(TGraphicControl)
    protected
      procedure Paint; override;
      procedure CMHitTest(var Message: TWMNCHitTest); message CM_HITTEST;
    public
      constructor Create(AOwner: TComponent); override;
    end;

  var
    FParent: TWinControl;
    FRect: TRect;
    FAlign: TAlign;
    FVisible: Boolean;
    FLLine: TLineGraphicControl;
    FTLine: TLineGraphicControl;
    FRLine: TLineGraphicControl;
    FBLine: TLineGraphicControl;
    FColor: TColor;
    procedure SetParent(aParent: TWinControl);
    procedure SetVisible(aVisible: Boolean);
    procedure SetColor(aColor: TColor);
  protected
    procedure ListViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
  public
    constructor Create(aParent: TWinControl); overload;
    destructor Destroy; override;
    property Parent: TWinControl read FParent write SetParent;
    property Visible: Boolean read FVisible write SetVisible;
    property Color: TColor read FColor write SetColor;
    property Align: TAlign read FAlign write FAlign;
    procedure SetBounds(aL, aT, aW, aH: Integer);
    procedure Show(aL, aT, aW, aH: Integer; aColor: TColor; aAlign: TAlign; aParent: TWinControl = nil); overload;
    procedure Show(aRect: TRect; aColor: TColor; aAlign: TAlign; aParent: TWinControl = nil); overload;
    procedure Hide;
    property Left: Integer read FRect.Left;
    property Top: Integer read FRect.Top;
  end;

  TBaseGridForm = class(TBaseDataForm)
    MM_Da_OrderBy: TMenuItem;
    MM_Da_Groupby: TMenuItem;
    Item_Da_Create: TMenuItem;
    PMDelete: TMenuItem;
    miGrpBeginDelimiter: TMenuItem;
    miGrpSEndDelimiter: TMenuItem;
    Item_Da_Groupby: TMenuItem;
    Item_Da_Orderby: TMenuItem;
    Item_Da_View: TMenuItem;
    pnlGroup: TPanel;
    pnlCard: TPanel;
    GroupSplitter: TSplitter;
    pnlMain: TPanel;
    act_Da_Restore: TAction;
    PMRestore: TMenuItem;
    MMRestore: TMenuItem;
    miCreateSortcuts: TMenuItem;
    Item_Da_Show: TMenuItem;
    act_Da_Sorting: TAction;
    PanelPopupMenu: TPopupMenu;
    PMDaOpenInNewWindow: TMenuItem;
    act_Da_OpenInNewWindow: TAction;
    N1: TMenuItem;
    N2: TMenuItem;
    act_Da_ViewChart: TAction;
    actDiagram1: TMenuItem;
    act_Da_ViewList: TAction;
    act_Da_ViewTree: TAction;
    act_Da_ViewMap: TAction;
    actChart1: TMenuItem;
    actChart2: TMenuItem;
    actChart3: TMenuItem;
    act_Da_ViewCalendar: TAction;
    actvCalendar1: TMenuItem;
    N3: TMenuItem;
    PM_Dl_Directories: TMenuItem;
    Item_Da_Splitby: TMenuItem;
    MM_Da_Splitby: TMenuItem;
    N8: TMenuItem;
    Open1: TMenuItem;
    N9: TMenuItem;
    actDaSorting1: TMenuItem;
    Item_Da_Action: TMenuItem;
    act_Da_TableProp: TAction;
    ItemTableProperty: TMenuItem;
    TableProperty1: TMenuItem;
    actvList2: TMenuItem;
    actvTree1: TMenuItem;
    actvChart1: TMenuItem;
    actvCalendar2: TMenuItem;
    actvMap1: TMenuItem;
    TempActionList: TActionList;
    GoToWindow: TMenuItem;
    act_Da_OpenInMain: TAction;
    StatusPopupMenu: TPopupMenu;
    pnlMainR: TPanel;
    pnlGrid: TPanel;
    pnlFilter: TPanel;
    pnlClient: TPanel;
    pnlFilterBase: TPanel;
    BGFEditPanel: TPanel;
    act_Da_ViewPivot: TAction;
    ViewPivot1: TMenuItem;
    actvPivot2: TMenuItem;
    pnlFilterTop: TPanel;
    act_dA_Favorites: TAction;
    actDaFavorites1: TMenuItem;
    N18: TMenuItem;
    GradientPaint: TPaintBox;
    pnlFilterR: TPanel;
    PanelExitBtn: TPanel;
    AreaBtnClose: TSpeedButton;
    PanelFilterMenu: TPanel;
    sbFindPrev: TSpeedButton;
    sbFindNext: TSpeedButton;
    sbFilter: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnClose: TSpeedButton;
    Item_Da_Print: TMenuItem;
    miOptionsSeparator: TMenuItem;
    MM_Da_Help: TMenuItem;
    NewHelpSeparator: TMenuItem;
    act_Da_FindNext: TAction;
    act_Da_FindPrev: TAction;
    act_Da_FilterApply: TAction;
    FilterPopupMenu: TPopupMenu;
    act_Da_FilterSave: TAction;
    mi_Da_Filter: TMenuItem;
    LocalFilterActionList: TActionList;
    btnFetchAll: TSpeedButton;
    act_Da_ViewTileMap: TAction;
    actvTileMap: TMenuItem;
    actvTileMap1: TMenuItem;
    FastPrint2: TMenuItem;
    Print2: TMenuItem;
    act_Da_AgrNo: TAction;
    act_Da_AgrCount: TAction;
    act_Da_AgrValue: TAction;
    act_Da_AgrSum: TAction;
    act_Da_AgrMin: TAction;
    act_Da_AgrMax: TAction;
    act_Da_AgrAve: TAction;
    btnMenu: TSpeedButton;
    actClose: TAction;
    actMaximize: TAction;
    actFetchAll: TAction;
    actFlip: TAction;
    btnMaximize: TSpeedButton;
    btnFlip: TSpeedButton;
    miFilter: TMenuItem;
    MMDefault: TMenuItem;
    pnlBounds: TPanel;
    pnlAttentionSolution: TPanel;
    Thumbnail: TImageList;
    miFilterLineSelectEnd: TMenuItem;
    mI_Da_FilterUser: TMenuItem;
    sbFindMenu: TSpeedButton;
    mI_Da_FilterNo: TMenuItem;
    miFilterLine1: TMenuItem;
    mi_dA_FilterSave: TMenuItem;
    N21: TMenuItem;
    miFilterOn: TMenuItem;
    miFilterOff: TMenuItem;
    act_Da_FilterOff: TAction;
    act_Da_FilterOn: TAction;
    procedure act_CutExecute(Sender: TObject);
    procedure act_CopyExecute(Sender: TObject);
    procedure act_PasteExecute(Sender: TObject);
    procedure act_CreateShortcutExecute(Sender: TObject);
    procedure act_CreateNoteExecute(Sender: TObject);
    procedure act_PropertiesExecute(Sender: TObject);
    procedure act_AddExecute(Sender: TObject);
    procedure act_DeleteExecute(Sender: TObject);
    procedure pnlGroupResize(Sender: TObject);
    procedure act_Da_RestoreExecute(Sender: TObject);
    procedure act_PrintExecute(Sender: TObject);
    procedure SetupSort(Sender: TObject);
    procedure act_Da_OpenInNewWindowExecute(Sender: TObject);
    procedure PanelPopupMenuPopup(Sender: TObject);
    procedure act_DirectoriesExecute(Sender: TObject);
    procedure actv_XXXExecute(Sender: TObject);
    procedure act_Da_TablePropExecute(Sender: TObject);
    procedure Act_Da_DefaultExecute(Sender: TObject);
    procedure act_Da_OpenInMainExecute(Sender: TObject);
    procedure act_Da_FilterExecute(Sender: TObject);
    procedure NewFilterSize;
    procedure ShowFilterClick(Sender: TObject);
    procedure HideFilterClick(Sender: TObject);
    procedure FilterMenu(Sender: TObject);
    procedure FilterDo(Sender: TObject);
    procedure FilterClear(Sender: TObject);
    procedure pnlFilterBaseResize(Sender: TObject);
    procedure act_dA_FavoritesExecute(Sender: TObject);
    procedure GradientPaintPaint(Sender: TObject);
    procedure sbFindPrevClick(Sender: TObject);
    procedure sbFindNextClick(Sender: TObject);
    procedure pnlGridEnter(Sender: TObject);
    procedure pnlGridExit(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure act_Da_FastPrintExecute(Sender: TObject);
    procedure act_Da_FindNextExecute(Sender: TObject);
    procedure act_Da_FindPrevExecute(Sender: TObject);
    procedure act_Da_FilterApplyExecute(Sender: TObject);
    procedure DoAgrigate(Sender: TObject);
    procedure act_Da_AgrNoExecute(Sender: TObject);
    procedure act_Da_AgrCountExecute(Sender: TObject);
    procedure act_Da_AgrValueExecute(Sender: TObject);
    procedure act_Da_AgrSumExecute(Sender: TObject);
    procedure act_Da_AgrMinExecute(Sender: TObject);
    procedure act_Da_AgrMaxExecute(Sender: TObject);
    procedure act_Da_AgrAveExecute(Sender: TObject);
    procedure GroupSplitterCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure actCloseExecute(Sender: TObject);
    procedure actFetchAllExecute(Sender: TObject);
    procedure actFlipExecute(Sender: TObject);
    procedure pnlClientMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer; var MouseActivate: TMouseActivate);
    procedure btnMenuClick(Sender: TObject);
    procedure pnlMainRResize(Sender: TObject);
    procedure FilterPopupMenuPopup(Sender: TObject);
    procedure mi_dA_FilterSaveClick(Sender: TObject);
    procedure act_Da_FilterOffExecute(Sender: TObject);
    procedure act_Da_FilterOnExecute(Sender: TObject);
  private
    FWindowActive: Boolean;
    FCanGrouping: Boolean;
    FCanFind: Boolean;
    FCanSorting: Boolean;
    FCanShowCard: Boolean;
    FViewArea: Boolean;
    FViewParams: TDeVariableList;
    FLockForm: Boolean;
    FFilterControl: TDeFilterControl;
    FFormCaption: String;
    FCaptionDirection: TCaptionDirection;
    FTemporaryMenuItems: TObjectList;
    // Список временных пунктов меню (для быстрой очистки), которые создаются в динамике ...
    FRefreshTimerUpdating: Boolean;

    procedure OnGroupClick(Sender: TObject);
    procedure ShowGroupArea(const isShow: Boolean);
    procedure SetCanGrouping(const Value: Boolean);
    procedure SetCanSorting(const Value: Boolean);
    procedure WMBGFGrpChange(var Message: TMessage); message WM_BGF_GRPCHANGE;
    procedure WMChangeActive(var Message: TMessage); message WM_BF_CHANGEACTIVE;
    procedure WMFilterChange(var Message: TMessage); message DM_FILTERCHANGE;

    procedure onPrintCardPreview(Sender: TObject);
    procedure DisplayMenuClick(Sender: TObject);
    // procedure FillCreateMenu;
    procedure DeleteSelectedItems;
    procedure SelectFilterClick(Sender: TObject);
    procedure RenameFilterClick(Sender: TObject);
    procedure DeleteFilterClick(Sender: TObject);
    procedure FilterAllUsersClick(Sender: TObject);
    procedure FilterOneUserClick(Sender: TObject);
    procedure FillLocalFilters;
    procedure LocalFilterExecute(Sender: TObject);
    procedure SetCaptionDirection(const Value: TCaptionDirection = cdAuto);
    function GetControlFilter: TFilterItem;
    procedure SetFormCaption(const Value: String);
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
  protected
    FFieldX, FFieldY: TFieldMeta;
    FIndexDField: Integer;
    FValueDField: Variant;
    FCanDrag: Boolean;
    FDragInfo: TDragInfo;
    FSelectRect: TSelectRect;
    FRefreshTimer: TTimer;
    FCaptionHeight: Integer;
    FCaptionBorder: Integer;
    FThumbNailSizeW: Integer;
    FThumbNailSizeH: Integer;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure OnSortClick(Sender: TObject); virtual;
    procedure SetCanDrag(Value: Boolean); virtual;
    function getCaptionText: string; virtual;
    procedure SetCanShowCard(const Value: Boolean);
    function GetCanShowCard: Boolean; virtual;
    procedure GroupCreate(AFieldMeta: TFieldMeta; aParentKeyValue: Variant);
    procedure GroupDelete(DeleteMeta: TDataSetMeta);
    procedure GroupDeleteAll(isClose: Boolean = False);
    procedure SubFormCreate(aContext: TContextMeta);
    // procedure CreateParams(var Params: TCreateParams); override;
    procedure GroupMenuCreate(GM_Item: TMenuItem); virtual;
    procedure SortMenuCreate; virtual;
    procedure DisplayMenuCreate(aItem: TMenuItem);
    function FindMenuItem(const aName: string): TMenuItem;
    // procedure UpdateActionMenu(aParent : TMenuItem);
    // procedure UpdatePrintMenu(ParentItem: TMenuItem);
    procedure UpdateActionsMenu;
    procedure InitForm; override;
    procedure AfterInit; override;
    procedure AfterDataOpen(Sender: TObject); override;
    procedure SetDataSetMeta(const Value: TDataSetMeta); override;
    procedure DoDragStart(Sender: TObject; var DragObject: TDragObject); virtual;
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); virtual;
    procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer); virtual;
    procedure CacheFocusedChange(Item: TCacheItem); override;
    procedure CacheSelectedChange(Item: TCacheItem); override;
    procedure CacheItemInsert(Item: TCacheItem); override;
    procedure CacheAllowFocusedChange(var NewItem: TCacheItem; var Allow: Boolean); override;
    function GetKeyValueAt(X, Y: Integer): Variant; virtual;
    function GetCacheItemAt(X, Y: Integer): TCacheItem; virtual;
    procedure SetZOrder(TopMost: Boolean); override;
    function getPrintReport(aStyle: TDePrintStyle): TDeReport; virtual;
    function getEnabledPrintStyles: TDePrintStyleTypes; virtual;
    procedure doPrintReport(aStyle: TDePrintStyle; const PrinterName: string); virtual;
    function GridMinHeight: Integer; virtual;
    procedure NotifyRefreshTimer(Sender: TObject); virtual;
    // Дополнительный фильтр контрола, v.16.12
    property ControlFilter: TFilterItem read GetControlFilter;
    procedure ActionVisible(const aCategory: String; aValue: Boolean);
    procedure ActionEnabled(const aCategory: String; aValue: Boolean);
    procedure ActionChecked(const aCategory: String; AFieldMeta: TFieldMeta);
    procedure ShowAttentionSolution;
    property ThumbNailSizeW: Integer read FThumbNailSizeW;
    property ThumbNailSizeH: Integer read FThumbNailSizeH;
    procedure SetThumbNailSize(const ValueW: Integer; ValueH: Integer = 0);
    procedure PrepareThumbNail(const aIndex: Integer);
  public
    AFormSplitter: TExtSplitter;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; Override;
    procedure DoUpdate(Sender: TObject); virtual;
    procedure OpenCard;
    procedure ChangeDataSetStatus; override;
    procedure DeInitForm; override;
    property ViewParams: TDeVariableList read FViewParams;
    property CanFind: Boolean read FCanFind write FCanFind;
    property CanGrouping: Boolean read FCanGrouping write SetCanGrouping;
    property CanSorting: Boolean read FCanSorting write SetCanSorting;
    property CanShowCard: Boolean read GetCanShowCard write SetCanShowCard;
    property CanDrag: Boolean read FCanDrag write SetCanDrag;
    property FormCaption: String read FFormCaption write SetFormCaption;
    property CaptionDirection: TCaptionDirection read FCaptionDirection write SetCaptionDirection;
    procedure MakeVisibleItem(Item: TCacheItem); virtual;
    function ShowViewArea(Manual: Boolean = False): Boolean;
    function HideViewArea(Manual: Boolean = False): Boolean;
    property ViewArea: Boolean read FViewArea;
    procedure ReCreateView; override;
    procedure ReInitForm(AParam: TObject); override;
    function GetPropertiesString: string; virtual;
    function DragInfoTarget(Sender, Source: TObject; X, Y: Integer; aDragInfo: TDragInfo): Boolean; virtual;

    property FilterControl: TDeFilterControl read FFilterControl;
    property FieldX: TFieldMeta read FFieldX;
    property FieldY: TFieldMeta read FFieldY;
    // v.18.11
    // Запустить/остановить таймер автоматического обновления грида ...
    function StartRefreshTimer: Boolean; dynamic;
    function StopRefreshTimer: Boolean; dynamic;
    function ViewType: TDSViewType;
    function IsMultiWIndow: Boolean;
  end;

function GetParentBaseGridForm(aObject: TObject): TBaseGridForm;
function ViewTypeToClass(const aViewType: TDSViewType): TControlClass;

implementation

uses Variants, ShlObj, DB, Forms, Math, Types,
  Vcl.Imaging.GIFImg, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage,
  DeLog, Dictionary, Funcs, Security, DeSettings, DeMetadata, DeActions,
  DataManager, HintForm, QueryDescriptor, DeDB, DataUnit, BaseFormUnit,
  TreeFormUnit, CalendarFormUnit, ChartFormUnit, PivotFormUnit, MapFormUnit,
  ListFormUnit, BaseMainFormUnit, UnitA, DePrintDlg2, DeStdReport, DeSortDialog, Main,
  DeControls, DeCommandDlg, DlgRename;

{$R *.dfm}

function ViewTypeToClass(const aViewType: TDSViewType): TControlClass;
begin
  case aViewType of
    vtTree:
      Result := TTreeForm;
{$IFDEF VECTOR_MAP}
    vtVectorMap:
      Result := TVectorViewForm;
{$ENDIF}
    vtCalendar:
      Result := TCalendarForm;
    vtChart:
      Result := TDeChartForm;
    vtPivot:
      Result := TPivotForm;
    vtTileMap:
      Result := TMapForm;
  else
    Result := TListForm;
  end;
end;

{ TSelectRect }

constructor TSelectRect.TLineGraphicControl.Create(AOwner: TComponent);
begin
  inherited;
  Canvas.Brush.Style := bsSolid;
end;

procedure TSelectRect.TLineGraphicControl.Paint;
begin
  Canvas.FillRect(System.Classes.Rect(0, 0, Width, Height));
end;

procedure TSelectRect.TLineGraphicControl.CMHitTest(var Message: TWMNCHitTest);
begin
  // чтобы контрол был прозрачен для мыши и не лоил на себя события
  Message.Result := 0;
end;

constructor TSelectRect.Create(aParent: TWinControl);
begin
  FParent := aParent;
  FVisible := False;
  FAlign := alClient;

  FLLine := TLineGraphicControl.Create(nil);
  FLLine.Visible := FVisible;
  FLLine.Parent := FParent;

  FTLine := TLineGraphicControl.Create(nil);
  FTLine.Visible := FVisible;
  FTLine.Parent := FParent;

  FRLine := TLineGraphicControl.Create(nil);
  FRLine.Visible := FVisible;
  FRLine.Parent := FParent;

  FBLine := TLineGraphicControl.Create(nil);
  FBLine.Visible := FVisible;
  FBLine.Parent := FParent;

  Color := clActiveCaption;
end;

destructor TSelectRect.Destroy;
begin
  FLLine.Free;
  FTLine.Free;
  FRLine.Free;
  FBLine.Free;
  inherited;
end;

procedure TSelectRect.ListViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Sender is TControl then
    TListView(FParent).OnDragOver(Sender, Source, X + TControl(Sender).Left, Y + TControl(Sender).Top, State, Accept);
end;

procedure TSelectRect.SetParent(aParent: TWinControl);
begin
  FParent := aParent;
  FLLine.Parent := aParent;
  FTLine.Parent := aParent;
  FRLine.Parent := aParent;
  FBLine.Parent := aParent;
  if aParent is TListView then
  begin
    FLLine.OnDragOver := ListViewDragOver;
    FTLine.OnDragOver := ListViewDragOver;
    FRLine.OnDragOver := ListViewDragOver;
    FBLine.OnDragOver := ListViewDragOver;
  end;
end;

procedure TSelectRect.SetColor(aColor: TColor);
begin
  FColor := aColor;
  FLLine.Canvas.Brush.Color := FColor;
  FLLine.Repaint;
  FTLine.Canvas.Brush.Color := FColor;
  FTLine.Repaint;
  FRLine.Canvas.Brush.Color := FColor;
  FRLine.Repaint;
  FBLine.Canvas.Brush.Color := FColor;
  FBLine.Repaint;
end;

procedure TSelectRect.SetBounds(aL, aT, aW, aH: Integer);
var
  C,R: TRect;
begin
  case FAlign of
    alTop:    R:= Rect(aL,      aT - 1,     aL + aW,     aT);
    alBottom: R:= Rect(aL,      aT + aH -1, aL + aW,     aT + aH - 1 + 1);
    alLeft:   R:= Rect(aL - 1,  aT,         aL,          aT + aH);
    alRight:  R:= Rect(aL + aW, aT,         aL + aW + 1, aT + aH);
  else        R:= Rect(aL,      aT,         aL + aW - 1, aT + aH - 1);
  end;

//if FParent.ParentToClient(R.TopLeft).Y < 0 then R.Top := R.Top + 1;
  if R.Top < 0  then R.Top:= 0;
  if R.Left < 0 then R.Left:= 0;

  C:= Parent.ClientRect;
  if FAlign in [alTop, alBottom] then
    if C.Width < R.Right then R.Right:= C.Width;
  if FAlign in [alLeft, alRight] then
    if C.Height < R.Top then R.Top:= C.Height;

  FLLine.SetBounds(R.Left, R.Top, 1, R.Height);
  FRLine.SetBounds(R.Right, R.Top, 1, R.Height);
  FTLine.SetBounds(R.Left, R.Top, R.Width, 1);
  FBLine.SetBounds(R.Left, R.Bottom, R.Width, 1);
end;

procedure TSelectRect.SetVisible(aVisible: Boolean);
begin
  FVisible := aVisible;
  FLLine.Visible := FVisible and (FAlign in [alClient, alLeft, alRight]);
  FRLine.Visible := FVisible and (FAlign in [alClient, alLeft, alRight]);
  FTLine.Visible := FVisible and (FAlign in [alClient, alTop, alBottom]);
  FBLine.Visible := FVisible and (FAlign in [alClient, alTop, alBottom]);
end;

procedure TSelectRect.Show(aL, aT, aW, aH: Integer; aColor: TColor; aAlign: TAlign; aParent: TWinControl = nil);
begin
  if Assigned(aParent) and (Parent <> aParent) then
    begin
      SetVisible(False);
      Parent:= aParent;
    end;
  Align := aAlign;
  SetBounds(aL, aT, aW, aH);
  SetColor(aColor);
  SetVisible(True);
end;

procedure TSelectRect.Show(aRect: TRect; aColor: TColor; aAlign: TAlign; aParent: TWinControl = nil);
begin
  Show(aRect.Left, aRect.Top, aRect.Width, aRect.Height, aColor, aAlign, aParent);
end;

procedure TSelectRect.Hide;
begin
  SetVisible(False);
end;

const
  MM_NONEGROUP = 'MM_NONEGROUP';
  MM_SELF = 'MM_SELF';

  { ------------ }

function GetParentBaseGridForm(aObject: TObject): TBaseGridForm;
var
  aControl: TControl;
begin
  if aObject is TControl then
    aControl := TControl(aObject)
  else
    Exit(nil);

  while (aControl.Parent <> nil) and (not(aControl is TBaseGridForm)) do
    aControl := aControl.Parent;

  if aControl is TBaseGridForm then
    Result := TBaseGridForm(aControl)
  else
    Result := nil;
end;

{ TBaseGridForm }

constructor TBaseGridForm.Create(AOwner: TComponent);
const
  cCrossBitmap = 'CROSS';
var
  NonClientMetrics: TNonClientMetrics;
begin
  inherited Create(AOwner);

  SetThumbNailSize(0);
  FCaptionHeight := 24;
  NonClientMetrics.cbSize := sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    FCaptionHeight := Min(FCaptionHeight, Max(18, NonClientMetrics.iCaptionHeight));

  FLockForm := False;
  FWindowActive := False;
  FCanFind := True;
  FCanGrouping := True;
  FCanSorting := True;
  FCanShowCard := True;
  FCaptionDirection := cdNone;
  FViewArea := False;
  FViewParams := TDeVariableList.Create;

  act_Da_ViewList.Tag := Ord(vtList);
  act_Da_ViewTree.Tag := Ord(vtTree);
  act_Da_ViewCalendar.Tag := Ord(vtCalendar);
  act_Da_ViewChart.Tag := Ord(vtChart);
{$IFDEF VECTOR_MAP}
  act_Da_ViewMap.Tag := Ord(vtVectorMap);
{$ELSE}
  act_Da_ViewMap.Enabled := False;
  act_Da_ViewMap.Visible := False;
{$ENDIF}
  act_Da_ViewPivot.Tag := Ord(vtPivot);
  act_Da_ViewTileMap.Tag := Ord(vtTileMap);

  btnClose.Glyph.LoadFromResourceName(hInstance, cCrossBitmap);
  // btnFlip.Glyph.LoadIconFromResource(hInstance, 'P01');
  // LoadIconFromResource
  AreaBtnClose.Glyph.LoadFromResourceName(hInstance, cCrossBitmap);
  sbFindMenu.Glyph.LoadFromResourceName(hInstance, '3DOT');
  sbFindPrev.Glyph.LoadFromResourceName(hInstance, 'FINDPREV');
  sbFindNext.Glyph.LoadFromResourceName(hInstance, 'FINDNEXT');
  sbFilter.Glyph.LoadFromResourceName(hInstance, 'FILTER');
  // sbFindMenu.Hint := GetTitle('', ttSecondName);
  sbFindPrev.Hint := GetTitle('_dA.FindPrev', ttSecondName);
  sbFindNext.Hint := GetTitle('_dA.FindNext', ttSecondName);
  sbFilter.Hint := GetTitle('_dA.FilterOn', ttSecondName);
  FFilterControl := TDeFilterControl.Create(self);
  FFilterControl.Parent := BGFEditPanel;
  FFilterControl.Align := alClient;
  // PrepareCaption;
  FDragInfo := TDragInfo.Create;
  FSelectRect := TSelectRect.Create(self);
end;

destructor TBaseGridForm.Destroy;
  procedure DestroyControls(aParent: TWinControl);
  var
    i: Integer;
  begin
    for i := Pred(aParent.ControlCount) downto 0 do
      if aParent.Controls[i] is TBaseDataForm then
        with TBaseDataForm(aParent.Controls[i]) do
        begin
          DeInitForm;
          Free;
        end;
  end;

begin
  FreeAndNil(FDragInfo);
  FreeAndNil(FTemporaryMenuItems);
  if Assigned(FRefreshTimer) then
  begin
    FRefreshTimer.Enabled := False;
    FreeAndNil(FRefreshTimer);
  end;
  FViewParams.Free;
  FSelectRect.Free;
  DestroyControls(PanelL);
  DestroyControls(PanelR);
  DestroyControls(PanelT);
  DestroyControls(PanelB);
  Thumbnail.Clear;
  inherited;
end;

function TBaseGridForm.ViewType: TDSViewType;
begin
{$IFDEF VECTOR_MAP} .if ClassType = TVectorViewForm then Result := vtVectorMap
else
{$ENDIF}
  if ClassType = TListForm then
    Result := vtList
  else if ClassType = TTreeForm then
    Result := vtTree
  else if ClassType = TMapForm then
    Result := vtTileMap
  else if ClassType = TCalendarForm then
    Result := vtCalendar
  else if ClassType = TDeChartForm then
    Result := vtChart
  else if ClassType = TPivotForm then
    Result := vtPivot
  else
    raise Exception.Create('Error in TBaseGridForm.ViewType');
end;

procedure TBaseGridForm.InitForm;
var
  i, Index: Integer;
  TWC: TWinControl;
  FilterItem: TFilterItem;
begin
  FSelectRect.Parent := FMainControl;

  if DataSetMeta = nil then
    Exit;

  for i := 0 to DataSetMeta.ChildrenCount - 1 do
    if Assigned(DataSetMeta.Children[i].LockField) and (DataSetMeta.Children[i].LockField.Link = DataSetMeta.Table.ID) then
      FLockForm := True;

  FViewParams.LoadCommaText(DataSetMeta.Table.GroupViewParams);
  FViewParams.LoadCommaText(DataSetMeta.Context.ViewParams);

  // pnlGroup.Width := 0; Здесь нужно грузить размер pnlGroup из реестра;
  if (DataSetMeta.Role = drMain) then
  begin
    if FCanGrouping and (DataSetMeta.GroupFields.Count > 0) then
    begin
      DataSetMeta.Cache.BeginUpdate;
      // если групп несколько, то блокируем обновления до создания всех групп
      for i := 0 to DataSetMeta.GroupFields.Count - 1 do
        GroupCreate(DataSetMeta.GroupFields.Items[i], unassigned);
      DataSetMeta.Cache.EndUpdate;
      ShowGroupArea(True);
      DataSetMeta.Cache.PrepareData(False);
      if not DataSetMeta.Cache.Active then
        DataSetMeta.Cache.OpenData;
    end
    else
    begin
      DataSetMeta.Cache.PrepareData(False);
      if not DataSetMeta.Cache.Active then
        DataSetMeta.Cache.OpenData;
      GroupMenuCreate(self.MM_Da_Groupby);
      GroupMenuCreate(self.Item_Da_Groupby);
      DisplayMenuCreate(self.Item_Da_Show);
    end;
  end
  else

    if DataSetMeta.Role = drParent then
  begin
    if not DataSetMeta.Cache.Active then
      DataSetMeta.Cache.OpenData;
    GroupMenuCreate(self.MM_Da_Groupby);
    GroupMenuCreate(self.Item_Da_Groupby);
    DisplayMenuCreate(self.Item_Da_Show);
  end;

  if (DataSetMeta.Role in [drMain, drParent]) then
    FCaptionBorder := 3
  else
    FCaptionBorder := 1;

  inherited InitForm;

  TWC := self;
  While TWC.Parent <> nil do
    TWC := TWC.Parent;

  act_Da_TableProp.Visible := Assigned(UserSession) and (UserSession.IsAdmin);
  act_Da_OpenInMain.Visible := (DataSetMeta.Role in [drParent, drChild]);
  act_dA_Favorites.Visible := (DataSetMeta.Role in [drMain]);
  act_Da_OpenInMain.Enabled := (Not DataSetMeta.Table.IsInterrelations) and
    (Not DataSetMeta.Table.IsNotes) and (TWC = Application.MainForm);

  actClose.Visible := (DataSetMeta.Role in [drParent]) and Not FLockForm;
  actFlip.Visible := DataSetMeta.Role in [drMain];
  actMaximize.Visible := act_Da_OpenInMain.Visible and act_Da_OpenInMain.Enabled;

  TableProperty1.Default := Not act_Da_OpenInMain.Visible;


  if Assigned(FFilterControl) then
    FFilterControl.Free;

  FFilterControl := TDeFilterControl.Create(self);
  FFilterControl.Parent := BGFEditPanel;
  FFilterControl.Align := alClient;
  FFilterControl.Init(DataSetMeta.Table.Fields, False); // !!! FALSE, т.е. не содаем ни одного поля

  while (FilterControl.Count = 0) and DataSetMeta.Table.UserValues.GetNextIndex(uvFilterCommon, Index) do
    if DataSetMeta.Table.UserValues[Index].Order = -1 then
      FilterControl.ReadFilterFromXML(DataSetMeta.Table.UserValues[Index].XML);

  if (FilterControl.Count = 0) and DataSetMeta.Table.UserValues.GetFirstIndex(uvFilterLast, Index) then
    FilterControl.ReadFilterFromXML(DataSetMeta.Table.UserValues[Index].XML);

  if (FilterControl.Count = 0) and DataSetMeta.Table.UserValues.GetFirstIndex(uvFilterCommon, Index) then
    FilterControl.ReadFilterFromXML(DataSetMeta.Table.UserValues[Index].XML);

  if (FilterControl.Count = 0) then
    FilterControl.InitDefaultFilter;                    // !!! тут создастся дефолтный фильтр, если нет других настроек

  if FilterControl.ReadFilterFromXML(DataSetMeta.Context.FilterXML) then
    if DataSetMeta.Context.FilterActive then
    begin
      FilterItem := TFilterItem.Create;
      try
        FilterControl.WriteFilterPostfixTo(FilterItem);
        DataSetMeta.UserFilterPostfix.CopyFrom(FilterItem);
      finally
        FilterItem.Free;
      end;
    end;

  act_Da_Filter.Enabled := (FilterControl.FieldsMeta.Count > 0);

  if DataSetMeta.Context.FilterActive then
  begin
    act_Da_Filter.Checked := False;
    ShowFilterClick(act_Da_Filter);
    PostMessage(Handle, DM_FILTERCHANGE, DM_pnCONDITION, 0);
    // MainGrid.act_Da_SetFilterExecute(act_Da_SetFilter);
  end
  else if DataSetMeta.Context.FilterVisible then
  begin
    act_Da_Filter.Checked := False;
    ShowFilterClick(act_Da_Filter);
  end;

  if not(VarIsNull(DataSetMeta.Context.SelectedKeys) or VarIsEmpty(DataSetMeta.Context.SelectedKeys)) then
    DataSetMeta.Cache.SelectedKeys := DataSetMeta.Context.SelectedKeys;

  if not(VarIsNull(DataSetMeta.Context.RecordKey) or VarIsEmpty(DataSetMeta.Context.RecordKey)) then
    DefaultMeta.Cache.SyncronizeOnData(DataSetMeta.Context.RecordKey);
  {
    if DataSetMeta.Context.ItemType = itAttribute then
    FCanShowCard:= False;
    { }
  StartRefreshTimer;
  for i := 0 to Pred(DataSetMeta.Context.Count) do
    if DataSetMeta.Context.Items[i].ItemType = itAttribute then
      SubFormCreate(DataSetMeta.Context.Items[i]);
end;

function TBaseGridForm.IsMultiWIndow: Boolean;
begin
  Result := (0 < PanelL.ControlCount) or (0 < PanelR.ControlCount) or (0 < PanelT.ControlCount) or (0 < PanelB.ControlCount);
end;

procedure TBaseGridForm.DeInitForm;
var
  i: Integer;
begin
  if Assigned(DataSetMeta) then
  begin
    if assigned(DataSetMeta.Table) then
      if (Not DataSetMeta.Table.IsReadOnly) and (Not DataSetMeta.Table.Database.IsReadOnly) then
        DataSetMeta.Table.UserValues.Commit;

    if DataSetMeta.Role = drMain then
      GroupDeleteAll(True);

    if Assigned(DataSetMeta.CardForm) then
    begin
      DataSetMeta.CardForm.DeInitForm;
      DataSetMeta.CardForm.Free;
    end;

    DataSetMeta.GridForm := nil;
  end;
  inherited DeInitForm;

  for i := 0 to Pred(ActionList.ActionCount) do
    ActionList.Actions[i].Visible := False;
end;

procedure TBaseGridForm.AfterInit;
var
  ok, ok_date // , ok_group
{$IFDEF VECTOR_MAP}, ok_map{$ENDIF}: Boolean;
  i: Integer;
  VI: TVariableItem;
  S: String;
begin
  inherited AfterInit;

  ok := Assigned(DataSetMeta) and (DataSetMeta.Role = drMain) and (DataSetMeta.Table.Fields.Count > 0);
  MM_Da_Representation.Visible := ok;

  ok_date := False;
  for i := 0 to DataSetMeta.Table.Fields.Count - 1 do
    if (DataSetMeta.Table.Fields[i].DataType in DateTypes) then
      if fvService < DataSetMeta.Table.Fields[i].VisibleLevel then
        begin
          ok_date:= True;
          Break;
        end;

  // 05.02.2016 * Карты отключены, а проверка осталась! Убрал проверку...
{$IFDEF VECTOR_MAP}
  ok_map := False; (*
    (DataSetMeta.Table.Fields.FindByName('OBJID')    <> nil)
    and (DataSetMeta.Table.Fields.FindByName('OBJOWNER') <> nil)
    and (DataSetMeta.Table.Fields.FindByName('OBJCLASS') <> nil)
    and (DataSetMeta.Table.Fields.FindByName('OBJPARENT')<> nil)
    and (DataSetMeta.Table.Fields.FindByName('OBJZORDER')<> nil)
    and (DataSetMeta.Table.Fields.FindByName('OBJDATA')  <> nil); *)
{$ENDIF}
  // 05.02.2016 -

  act_Da_ViewList.Visible := ok;
  act_Da_ViewTree.Visible := ok and Assigned(DataSetMeta.Table.PField);
  act_Da_ViewChart.Visible := ok { and ok_group };
  act_Da_ViewCalendar.Visible := ok and ok_date;
{$IFDEF VECTOR_MAP}
  act_Da_ViewMap.Visible := ok and ok_map;
{$ENDIF}
  act_Da_ViewPivot.Visible := ok;
  act_Da_ViewPivot.Enabled := DataSetMeta.Table.Database.CanODBC and (GetMicrosoftChartVersion = 11);
  act_Da_ViewTileMap.Visible := ok and (not Assigned(DataSetMeta.Table)) or
    (Assigned(DataSetMeta.Table.LatitudeField) and Assigned(DataSetMeta.Table.LongitudeField));
  act_Da_ViewTileMap.Enabled := act_Da_ViewTileMap.Visible;

  VI := ViewParams.GetByName(cViewTypes, False);
  if Assigned(VI) then
  begin
    S := Uppercase(VarToStr(VI.Value));
    act_Da_ViewList.Visible := act_Da_ViewList.Visible and (0 < Pos(Uppercase(cViewTypeList), S));
    act_Da_ViewTree.Visible := act_Da_ViewTree.Visible and (0 < Pos(Uppercase(cViewTypeTree), S));
    act_Da_ViewCalendar.Visible := act_Da_ViewCalendar.Visible and (0 < Pos(Uppercase(cViewTypeCalendar), S));
    act_Da_ViewChart.Visible := act_Da_ViewChart.Visible and (0 < Pos(Uppercase(cViewTypeChart), S));
    act_Da_ViewTileMap.Visible := act_Da_ViewTileMap.Visible and (0 < Pos(Uppercase(cViewTypeMap), S));
    act_Da_ViewPivot.Visible := act_Da_ViewPivot.Visible and (0 < Pos(Uppercase(cViewTypePivot), S));
  end;

  // -------------------

  SortMenuCreate;
  ReCreateView;

  // UpdateActionMenu(MM_Da_Actions);
  // UpdateActionMenu(Item_Da_Action);

  // UpdatePrintMenu(Item_Da_Print); // Настройка контекстного меню печати ...
  UpdateActionsMenu; // Полная настройка POPUP и основного меню ...

  ok := True;
  for i := 0 to MM_Da_Actions.Count - 1 do
    if (MM_Da_Actions[i].Visible) and not(MM_Da_Actions[i].IsLine) and
      not(MM_Da_Actions[i].Action = act_dA_emptylist) then
    begin
      ok := False;
      Break;
    end;

  act_dA_emptylist.Visible := ok;

  // FillCreateMenu; // Реализовано в UpdateActionsMenu!!!
 // FillUserFilters;
  FillLocalFilters;

  pnlGrid.DoubleBuffered := True;

  // GroupMenuCreate(self.MM_Group);
  // GroupMenuCreate(self.Item_Group);
  CanDrag := Variables.AsBoolean[RegDragDrop];
end;

procedure TBaseGridForm.btnMenuClick(Sender: TObject);
var
  P: TPoint;
begin
  P := pnlClient.ClientToScreen(Point(0, pnlClient.Height));
  PanelPopupMenu.Popup(P.X, P.Y);
end;

procedure TBaseGridForm.ReCreateView;
var
  i: Integer;
begin
  for i := 0 to TempActionList.ActionCount - 1 do
    if TempActionList.Actions[i] is TMetaAction then
      if TempActionList.Actions[i].Category = CategorySort then
      begin
        TempActionList.Actions[i].Checked := (DataSetMeta.Cache.SortList.Count = 1) and
          (DataSetMeta.Cache.SortList[0].FieldID = TMetaAction(TempActionList.Actions[i]).FieldMeta.ID);

        TempActionList.Actions[i].Visible := (TempActionList.Actions[i].Checked)
          or (TMetaAction(TempActionList.Actions[i]).FieldMeta.VisibleLevel >= fvLevel2);
      end;
  // FFilterControl.ReadFilterFromXML(ApplyFilter, '<filters operation="AND" apply="yes"><filter field="CCUSFIO" operation="LIKE" value="z&lt;1&gt;"/><filter field="DCUSBIRTH" operation="is not null"/></filters>')
  ShowAttentionSolution;
  StartRefreshTimer;
  inherited;
end;

procedure TBaseGridForm.ReInitForm(AParam: TObject);
begin
  inherited;
  FormResize(self);
end;

procedure TBaseGridForm.AfterDataOpen(Sender: TObject);
begin
  inherited;
  Thumbnail.Clear;
  ReCreateView;
end;

procedure TBaseGridForm.CacheFocusedChange(Item: TCacheItem);
begin
  ChangeDataSetStatus;
  inherited;
end;

procedure TBaseGridForm.CacheSelectedChange(Item: TCacheItem);
begin
  inherited;
end;

procedure TBaseGridForm.ChangeDataSetStatus;
begin
  inherited ChangeDataSetStatus;

  if DataSetMeta = nil then
    Exit;

  act_Da_FastPrint.Enabled := Assigned(DataSetMeta) and DataSetMeta.Cache.Active and (CacheItems.Count > 0);
  act_Da_Restore.Visible := MetaData.ShowRemoved and Assigned(CacheItems.TableMeta.DField);
  if act_Da_Restore.Visible and Assigned(CacheItems.FocusedItem) then
    if CacheItems.TableMeta.DField.DataType in DateTypes then
      act_Da_Restore.Enabled := not VarIsNullOrEmpty(CacheItems.FocusedItem.ValueByName[CacheItems.TableMeta.DField.Original])
    else
      act_Da_Restore.Enabled :=
        VarSameValue(CacheItems.FocusedItem.ValueByName[CacheItems.TableMeta.DField.Original], CacheItems.TableMeta.DField.Value2);

  if Assigned(DataSetMeta) then
    if DataSetMeta.AllFetched then
      btnFetchAll.Visible := False
    else
    begin
      if not btnFetchAll.Visible then
      begin
        if btnClose.Visible then
          btnFetchAll.SetBounds(btnClose.Left - btnFetchAll.Width - 2, btnFetchAll.Top, btnFetchAll.Width, btnFetchAll.Height)
        else
          btnFetchAll.SetBounds(pnlClient.Width - btnFetchAll.Width - 2, btnFetchAll.Top, btnFetchAll.Width, btnFetchAll.Height);
        btnFetchAll.Visible := True;
        btnFetchAll.Enabled := True;
        ShowHintWindow(btnFetchAll, 'Не все записи начитаны.');
      end;
    end
  else
    btnFetchAll.Visible := False;

  TBaseMainForm(MainBaseForm).RefreshMenu;
end;

procedure TBaseGridForm.SortMenuCreate;
var
  i: Integer;
  AC: TAction;
begin
  for i := Pred(TempActionList.ActionCount) downto 0 do
    if TempActionList.Actions[i].Category = CategorySort then
      TempActionList.Actions[i].Free;

  for i := 0 to DataSetMeta.Table.Fields.Count - 1 do
    if DataSetMeta.Table.Fields[i].IsLookup or
      Not Assigned(DataSetMeta.Table.Fields[i].LookupPair) then
      if DataSetMeta.Table.Fields[i].PolicyShow then
      begin
        AC := TMetaAction.Create(self, CategorySort, DataSetMeta.Table.Fields[i], OnSortClick, [Item_Da_Orderby, MM_Da_OrderBy]);
        AC.ActionList := TempActionList;
        if DataSetMeta.Cache.SortList.Count = 1 then
          if DataSetMeta.Table.Fields[i].ID = DataSetMeta.Cache.SortList[0].FieldID then
            AC.Checked := True;
      end;

  Item_Da_Orderby.Visible := True;
  MM_Da_OrderBy.Visible := True;
end;

procedure TBaseGridForm.DisplayMenuCreate(aItem: TMenuItem);
var
  i: Integer;
  NewItem: TMenuItem;
begin
  for i := Pred(TempActionList.ActionCount) downto 0 do
    if TempActionList.Actions[i].Category = CategoryDisplay then
      TempActionList.Actions[i].Free;

  Item_Da_Show.Clear;

  with DataSetMeta.Table do
    for i := 0 to Fields.Count - 1 do
      if (Fields[i].Link > 0) and (not Fields[i].IsLookup) and
        (Fields[i].LinkTable.IsDirectory = idPrimary) then
        TMetaAction.Create(self, CategoryDisplay, Fields[i], DisplayMenuClick, [Item_Da_Show]).ActionList := TempActionList;

  NewItem := TMenuItem.Create(aItem);
  NewItem.Caption := cLineCaption;
  Item_Da_Show.Add(NewItem);

  with DataSetMeta.Table do
    for i := 0 to Fields.Count - 1 do
      if (Fields[i].Link > 0) and (not Fields[i].IsLookup) and (Fields[i].LinkTable.IsDirectory in [idLocal, idGlobal]) then
        TMetaAction.Create(self, CategoryDisplay, Fields[i], DisplayMenuClick, [Item_Da_Show]).ActionList := TempActionList;
end;

procedure TBaseGridForm.GroupMenuCreate(GM_Item: TMenuItem);
var
  i, j, N: Integer;
  NewMenu: TMenuItem;
  FNoneGrp: Boolean;
begin
  GM_Item.Clear;
  GM_Item.Visible := False;

  If (DataSetMeta = nil) or (not FCanGrouping) then
    Exit;

  NewMenu := TMenuItem.Create(GM_Item);
  NewMenu.Caption := cLineCaption;
  GM_Item.Add(NewMenu);

  FNoneGrp := True;

  N := -1;
  if DataSetMeta.Role = drMain then
  begin
    if (DataSetMeta.Context.ViewType = vtTileMap) then
    begin
      NewMenu := TMenuItem.Create(GM_Item);
      NewMenu.Name := MM_SELF;
      NewMenu.Tag := NativeInt(DataSetMeta.Table.KField[0]);
      NewMenu.Caption := GetTitle('_dA.iconslist');
      NewMenu.Hint := GetTitle('_dA.iconslist', ttSecondName);
      NewMenu.OnClick := OnGroupClick;
      GM_Item.Add(NewMenu);
      Inc(N);
    end;

    for i := 0 to DataSetMeta.Table.Fields.Count - 1 do
      if (DataSetMeta.Table.Fields[i].IsLookup) and (DataSetMeta.Table.Fields[i].LookupPair.PolicySelect) then
        if DataSetMeta.Table.Fields[i].LookupPair.IsStored then
        begin
          NewMenu := TMenuItem.Create(GM_Item);
          NewMenu.Name := 'MM_' + IntToStr(NativeInt(DataSetMeta.Table.Fields[i].LookupPair));
          NewMenu.Tag := NativeInt(DataSetMeta.Table.Fields[i].LookupPair);
          NewMenu.Caption := DataSetMeta.Table.Fields[i].Native;
          NewMenu.Hint := NewMenu.Caption + ' [' + DataSetMeta.Table.Fields[i].LookupPair.Original + ']';
          NewMenu.OnClick := OnGroupClick;
          NewMenu.Enabled := (DataSetMeta.Table.Fields[i].LookupPair <> DataSetMeta.LockField);

          for j := DataSetMeta.GroupFields.Count - 1 downto 0 do
            if DataSetMeta.GroupFields[j] = DataSetMeta.Table.Fields[i].LookupPair then
            begin
              NewMenu.Checked := True;
              FNoneGrp := False;
              Break;
            end;

          if DataSetMeta.Table.Fields[i].LinkTable.IsDirectory in [idLocal, idGlobal] then
            GM_Item.Add(NewMenu)
          else
          begin
            Inc(N);
            GM_Item.Insert(N, NewMenu);
          end;
        end;
  end
  else if (DataSetMeta.Role = drParent) and (Not FLockForm) then
    for i := 0 to DataSetMeta.ChildrenCount - 1 do
      if DataSetMeta.Children[i].Role = drMain then
      begin
        NewMenu := TMenuItem.Create(GM_Item);
        // NewMenu.Name := 'MM_'+DataSetMeta.Table.Table;
        NewMenu.Caption := GetTitle(DataSetMeta.Children[i].Table.Name);
        NewMenu.Checked := True;
        NewMenu.OnClick := OnGroupClick;

        if DataSetMeta.Children[i].Table.IsDirectory in [idLocal, idGlobal] then
          GM_Item.Add(NewMenu)
        else
        begin
          Inc(N);
          GM_Item.Insert(N, NewMenu);
        end;

        FNoneGrp := False;
        Break;
      end;

  GM_Item.Visible := (GM_Item.Count > 0);
  if GM_Item.Visible then
  begin
    NewMenu := TMenuItem.Create(GM_Item);
    NewMenu.Caption := cLineCaption;
    GM_Item.Add(NewMenu);

    NewMenu := TMenuItem.Create(GM_Item);
    NewMenu.Name := MM_NONEGROUP;
    NewMenu.Caption := GetTitle('_Dl.NoGroup');
    NewMenu.Checked := FNoneGrp;
    NewMenu.OnClick := OnGroupClick;
    NewMenu.Enabled := Not Assigned(DataSetMeta.LockField);
    GM_Item.Add(NewMenu);
  end;
end;

procedure TBaseGridForm.GroupSplitterCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
begin
  inherited;
  if NewSize > 500 then
    NewSize := 500
  else if NewSize < FCaptionHeight + (132 - FCaptionHeight) div 2 then
    NewSize := FCaptionHeight
  else if NewSize < 132 then
    NewSize := 132;

  Accept := True;
end;

procedure TBaseGridForm.actFlipExecute(Sender: TObject);
begin
  inherited;
  if pnlGroup.Width > 132 then
  begin
    pnlGroup.Tag := pnlGroup.Width;
    pnlGroup.Width := FCaptionHeight;
  end
  else
  begin
    pnlGroup.Width := Min(500, Max(132, pnlGroup.Tag));
  end;
end;

function TBaseGridForm.FindMenuItem(const aName: string): TMenuItem;
var
  Component: TComponent;
begin
  Component := FindComponent(aName);
  if Assigned(Component) and (Component is TMenuItem) then
    Result := TMenuItem(Component)
  else
    Result := nil;
end;

function TBaseGridForm.GridMinHeight: Integer;
begin
  Result := Round(ClientHeight * 1 / 4);
end;

procedure TBaseGridForm.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
var
  H: Integer;
begin
  if Assigned(DataSetMeta) then
    if Assigned(DataSetMeta.CardForm) then
      if NewHeight <> Height then
      begin
        H := pnlCard.ClientHeight;

        if (NewHeight >= Height) and (H < DataSetMeta.CardForm.Tag) then
          H := DataSetMeta.CardForm.Tag;

        pnlCard.ClientHeight :=
          Min(H, ClientHeight - (pnlClient.Height + pnlFilter.Height + GridMinHeight + 2 * pnlGrid.BevelWidth + 6));
      end;

  Resize := True;
end;

procedure TBaseGridForm.FormResize(Sender: TObject);
var
  L, R, T, B: Integer;
begin
  inherited;

  if PanelL.ControlCount > 0 then
    L := (PanelL.Controls[0] as TBaseGridForm).DataSetMeta.Context.Size
  else
    L := 0;
  if PanelR.ControlCount > 0 then
    R := (PanelR.Controls[0] as TBaseGridForm).DataSetMeta.Context.Size
  else
    R := 0;
  if PanelT.ControlCount > 0 then
    T := (PanelT.Controls[0] as TBaseGridForm).DataSetMeta.Context.Size
  else
    T := 0;
  if PanelB.ControlCount > 0 then
    B := (PanelB.Controls[0] as TBaseGridForm).DataSetMeta.Context.Size
  else
    B := 0;

  if PanelL.ControlCount > 0 then
    PanelL.Width := (L * Width) div (200 + L + R);
  if PanelR.ControlCount > 0 then
    PanelR.Width := (R * Width) div (200 + L + R);

  if PanelT.ControlCount > 0 then
    PanelT.Height := (T * Height) div (200 + T + B);
  if PanelB.ControlCount > 0 then
    PanelB.Height := (B * Height) div (200 + T + B);
end;

{
  procedure TBaseGridForm.UpdateActionMenu(aParent : TMenuItem);
  var I         : integer;
  NewMenu   : TMenuItem;
  HaveEmpty : Boolean;
  begin
  HaveEmpty:= False;
  for i := aParent.Count-1 downto 0 do
  if (aParent.Items[i].Action = act_dA_emptylist) then HaveEmpty:=True else
  if not (aParent.Items[i].IsLine) then aParent.Delete(i);

  for I := Pred(DataSetMeta.UserActionList.ActionCount) downto 0 do
  if TDeActionData(DataSetMeta.UserActionList[i].Tag).ActionType <> atReport then
  begin
  NewMenu := TMenuItem.Create(aParent);
  //NewMenu.Name :=  aParent.Name+IntToStr(i);
  NewMenu.Action:=DataSetMeta.UserActionList.Actions[i];
  aParent.Insert(0, NewMenu);
  end;

  if (not HaveEmpty) then
  aParent.Visible:= ( 0 < DataSetMeta.UserActionList.ActionCount );
  end;
}
{
  procedure TBaseGridForm.UpdatePrintMenu(ParentItem: TMenuItem);
  var
  Index, Count: Integer;
  Strings: TStrings;
  MenuItem: TMenuItem;
  begin
  if Assigned(ParentItem) then
  begin
  ParentItem.Clear;
  if Assigned(DataSetMeta) and Assigned(DataSetMeta.UserActionList) and Assigned(DataSetMeta.Table) then
  begin
  Strings := TStringList.Create;
  try
  for Index := Pred(DataSetMeta.UserActionList.ActionCount) downto 0 do
  if TDeActionData(DataSetMeta.UserActionList[Index].Tag).ActionType = atReport then
  if TDeActionData(DataSetMeta.UserActionList[Index].Tag).DataSet = DataSetMeta.Table.ID then
  begin
  MenuItem := TMenuItem.Create(ParentItem);
  try
  MenuItem.Action := DataSetMeta.UserActionList[Index];
  ParentItem.Insert(0, MenuItem);
  except
  MenuItem.Free;
  raise;
  end;
  end
  else
  Strings.Append(IntToStr(Index));
  Count := ParentItem.Count;
  for Index := Pred(Strings.Count) downto 0 do
  begin
  MenuItem := TMenuItem.Create(ParentItem);
  try
  MenuItem.Action := DataSetMeta.UserActionList[StrToInt(Strings[Index])];
  ParentItem.Insert(Count, MenuItem);
  except
  MenuItem.Free;
  raise;
  end;
  end;
  if (Count > 0) and (ParentItem.Count <> Count) then
  begin
  MenuItem := TMenuItem.Create(ParentItem);
  try
  MenuItem.Caption := '-';
  ParentItem.Insert(Count, MenuItem);
  except
  MenuItem.Free;
  raise;
  end;
  end;
  finally
  Strings.Free;
  end;
  end;
  if ParentItem.Count = 0 then
  begin
  MenuItem := TMenuItem.Create(ParentItem);
  try
  MenuItem.Caption := act_dA_emptylist.Caption;
  MenuItem.Enabled := False;
  ParentItem.Add(MenuItem);
  except
  MenuItem.Free;
  raise;
  end;
  end;
  end;
  end;
}

procedure TBaseGridForm.UpdateActionsMenu;
type
  TMenuType = (mtMain, mtPopup);

  TRootMap = packed record
    Item: TMenuItem; // Корневой пункт меню
    Index: Integer; // Индекс вставки новых пунктов меню
    Count: Integer; // Количество вставленных новых пунктов меню
    SubCount: Integer; // Количество вставленных новых пунктов подменю
{$IFDEF DEBUG}
    Names: string;
{$ENDIF}
  end;

  TRootMaps = array [TActionCategory, TMenuType] of TRootMap;
var
  RootMaps: TRootMaps;
  MenuItem, SeparatorItem: TMenuItem;
  Index, ItemIndex: Integer;
  ItemList: TStrings;
  procedure AppendTemporaryMenuItems(MenuItem: TMenuItem);
  begin
    if Assigned(MenuItem) then
    begin
      if not Assigned(FTemporaryMenuItems) then
        FTemporaryMenuItems := TObjectList.Create(True);
      FTemporaryMenuItems.Add(MenuItem);
    end;
  end;
  procedure AppendAction(Action: TAction; const MenuType: TMenuType);
  var
    ActionCategory: TActionCategory;
    MenuItem, RootItem: TMenuItem;
    Categories, Category, PathCategory: string;
    MenuIndex: Integer;
  begin
    ActionCategory := TDeActionData(Action.Tag).Category;
    RootItem := RootMaps[ActionCategory, MenuType].Item;
    if Assigned(RootItem) and TDeActionData(Action.Tag).Active then
    begin
      MenuIndex := RootMaps[ActionCategory, MenuType].Index;
      Categories := TDeActionData(Action.Tag).SubCategories;
      PathCategory := IntToStr(Ord(MenuType));
      while Length(Categories) <> 0 do
      begin
        Category := Trim(CutTextValue(Categories, '\'));
        PathCategory := PathCategory + '\' + Category;
        ItemIndex := ItemList.IndexOf(PathCategory);
        if ItemIndex <> -1 then
          RootItem := ItemList.Objects[ItemIndex] as TMenuItem
        else
        begin
          MenuItem := TMenuItem.Create(RootItem);
          try
            MenuItem.Caption := GetTitle(Category);
            MenuItem.Hint := GetTitle(Category, ttSecondName);
            RootItem.Insert(MenuIndex, MenuItem);
          except
            MenuItem.Free;
            raise;
          end;
          AppendTemporaryMenuItems(MenuItem);
          Inc(RootMaps[ActionCategory, MenuType].SubCount);
          RootItem := MenuItem;
          ItemList.AddObject(PathCategory, RootItem);
        end;
        MenuIndex := 0;
      end;
      MenuItem := TMenuItem.Create(RootItem);
      MenuItem.Default := (Action.ShortCut = VK_RETURN);
      try
        MenuItem.Action := Action;
        RootItem.Insert(MenuIndex, MenuItem);
      except
        MenuItem.Free;
        raise;
      end;
      AppendTemporaryMenuItems(MenuItem);
      if TDeActionData(Action.Tag).BreakLine then
      begin
        MenuItem := TMenuItem.Create(RootItem);
        try
          MenuItem.Caption := '-';
          RootItem.Insert(MenuIndex, MenuItem);
        except
          MenuItem.Free;
          raise;
        end;
        AppendTemporaryMenuItems(MenuItem);
      end;
      Inc(RootMaps[ActionCategory, MenuType].Count);
{$IFDEF DEBUG}
      with RootMaps[ActionCategory, MenuType] do
      begin
        if Length(Names) <> 0 then
          Names := Names + ', ';
        Names := Names + QuotedStr(Action.Caption);
      end;
{$ENDIF}
    end;
  end;
{$IFDEF DEBUG}
  procedure WriteRootMaps(const Text: string);
  const
    MenuTypes: array [TMenuType] of PChar = ('mtMain', 'mtPopup');
    function PrepareRootMap(const RootMap: TRootMap): string;
    begin
      if Assigned(RootMap.Item) then
        if Length(RootMap.Item.Caption) = 0 then
          Result := Format('$%p', [Pointer(RootMap.Item)])
        else
          Result := QuotedStr(RootMap.Item.Caption)
      else
        Result := 'nil';
      Result := Format('{Item: %s, Index: %d, Count: %d {%s}, SubCount: %d}',
        [Result, RootMap.Index, RootMap.Count, RootMap.Names, RootMap.SubCount]);
    end;

  var
    Index: TActionCategory;
    Value: string;
  begin
    Value := EmptyStr;
    for Index := Low(Index) to High(Index) do
    begin
      Value := Value + Format(#13#10'                        [%s, mtMain]: %s',
        [StrPas(ActionCategories[Index]), PrepareRootMap(RootMaps[Index, mtMain])])
        + Format(#13#10'                        [%s, mtPopup]: %s',
        [StrPas(ActionCategories[Index]), PrepareRootMap(RootMaps[Index, mtPopup])]);
    end;
    WriteLog(Text + Value, True, 'RootMap');
  end;
{$ENDIF}

begin
  // Очистим список временных пунктов меню если они есть ...
  if Assigned(FTemporaryMenuItems) then
    FreeAndNil(FTemporaryMenuItems);
  if Assigned(DataSetMeta) and Assigned(DataSetMeta.Table) then
  begin
    if Assigned(DataSetMeta.UserActionList) then
    begin
      ZeroMemory(@RootMaps, sizeof(RootMaps));
      with RootMaps[acRoot, mtMain] do
      begin
        Item := MainMenu.Items;
        Index := MM_Da_Help.MenuIndex;
      end;
      with RootMaps[acFile, mtMain] do
      begin
        Item := MM_Da_File;
        Index := afterPrintSep.MenuIndex;
      end;
      RootMaps[acCreate, mtPopup].Item := Item_Da_Create;
      RootMaps[acPrint, mtPopup].Item := Item_Da_Print;
      with RootMaps[acEdit, mtMain] do
      begin
        Item := MM_Da_Edit;
        Index := mmiEditBAddDeleteDelimiter.MenuIndex;
      end;
      with RootMaps[acService, mtMain] do
      begin
        Item := MM_Da_Service;
        Index := miOptionsSeparator.MenuIndex;
      end;
      RootMaps[acOperation, mtMain].Item := MM_Da_Operations;
      RootMaps[acAction, mtMain].Item := MM_Da_Actions;
      RootMaps[acAction, mtPopup].Item := Item_Da_Action;
      with RootMaps[acHelp, mtMain] do
      begin
        Item := MM_Da_Help;
        Index := NewHelpSeparator.MenuIndex;
      end;
{$IFDEF DEBUG}
      WriteRootMaps('UpdateActionsMenu start ...');
{$ENDIF}
      ItemList := TStringList.Create;
      try
        for Index := Pred(DataSetMeta.UserActionList.ActionCount) downto 0 do
          if not TDeActionData(TAction(DataSetMeta.UserActionList[Index]).Tag).IsContextDataSet(DataSetMeta.Table.ID) then
          begin
            AppendAction(DataSetMeta.UserActionList[Index] as TAction, mtMain);
          end;

        SeparatorItem := TMenuItem.Create(MM_Da_Actions);
        SeparatorItem.Caption := '-';
        MM_Da_Actions.Insert(0, SeparatorItem);

        for Index := Pred(DataSetMeta.UserActionList.ActionCount) downto 0 do
          if TDeActionData(TAction(DataSetMeta.UserActionList[Index]).Tag).IsContextDataSet(DataSetMeta.Table.ID) then
          begin
            AppendAction(DataSetMeta.UserActionList[Index] as TAction, mtMain);
            AppendAction(DataSetMeta.UserActionList[Index] as TAction, mtPopup);
          end;
      finally
        ItemList.Free;
      end;
    end;
    if DataSetMeta.Table.IsNotes or DataSetMeta.Table.IsInterrelations then
    begin
      MenuItem := TMenuItem.Create(Item_Da_Create);
      MenuItem.Caption := cLineCaption;
      Item_Da_Create.Add(MenuItem);
      AppendTemporaryMenuItems(MenuItem);
      Inc(RootMaps[acCreate, mtPopup].Count);

      MenuItem := TMenuItem.Create(Item_Da_Create);
      MenuItem.Action := actCreate_Da_Shortcut;
      Item_Da_Create.Add(MenuItem);
      AppendTemporaryMenuItems(MenuItem);
      Inc(RootMaps[acCreate, mtPopup].Count);

      MenuItem := TMenuItem.Create(Item_Da_Create);
      MenuItem.Action := actCreate_Da_Note;
      Item_Da_Create.Add(MenuItem);
      AppendTemporaryMenuItems(MenuItem);
      Inc(RootMaps[acCreate, mtPopup].Count);
    end;
  end;
  MenuItem := TMenuItem.Create(Item_Da_Create);
  MenuItem.Action := Act_Da_Create;
  MenuItem.Default := True;
  if Assigned(DataSetMeta) and Assigned(DataSetMeta.Table) then
    MenuItem.Caption := GetTitle(DataSetMeta.Table.Name);
  Item_Da_Create.Insert(0, MenuItem);
  AppendTemporaryMenuItems(MenuItem);
  Inc(RootMaps[acCreate, mtPopup].Count);
  if Item_Da_Create.Count > 1 then
  begin
    MenuItem := TMenuItem.Create(Item_Da_Create);
    MenuItem.Caption := cLineCaption;
    Item_Da_Create.Insert(1, MenuItem);
    AppendTemporaryMenuItems(MenuItem);
    Inc(RootMaps[acCreate, mtPopup].Count);
  end;
  if Item_Da_Action.Count = 0 then
  begin
    MenuItem := TMenuItem.Create(Item_Da_Action);
    try
      MenuItem.Caption := act_dA_emptylist.Caption;
      MenuItem.Enabled := False;
      Item_Da_Action.Add(MenuItem);
    except
      MenuItem.Free;
      raise;
    end;
    AppendTemporaryMenuItems(MenuItem);
    Inc(RootMaps[acAction, mtPopup].Count);
  end;
  if Item_Da_Print.Count = 0 then
  begin
    MenuItem := TMenuItem.Create(Item_Da_Print);
    try
      MenuItem.Caption := act_dA_emptylist.Caption;
      MenuItem.Enabled := False;
      Item_Da_Print.Add(MenuItem);
    except
      MenuItem.Free;
      raise;
    end;
    AppendTemporaryMenuItems(MenuItem);
    Inc(RootMaps[acPrint, mtPopup].Count);
  end;
{$IFDEF DEBUG}
  WriteRootMaps('UpdateActionsMenu finish ...');
{$ENDIF}
end;

// TODO 5: Здесь проблема загрузки проца при простое = TComponent.UpdateAction

procedure TuneCheckedState(Sender: TMenuItem);
var
  i: Integer;
  FNoneGrp: Boolean;
  NoneMenu: TMenuItem;
begin
  if SameText(Sender.Name, MM_NONEGROUP) then
  begin
    for i := 0 to Sender.Parent.Count - 1 do
      if Sender.Parent.Items[i] <> Sender then
        Sender.Parent.Items[i].Checked := False;
  end
  else
  begin
    if Sender.Checked then
    begin
      for i := Sender.Parent.Count - 1 downto 0 do
        if SameText(Sender.Parent.Items[i].Name, MM_NONEGROUP) then
        begin
          Sender.Parent.Items[i].Checked := False;
          Break;
        end;
    end
    else
    begin
      FNoneGrp := True;
      NoneMenu := nil;

      for i := 0 to Sender.Parent.Count - 1 do
        if SameText(Sender.Parent.Items[i].Name, MM_NONEGROUP) then
          NoneMenu := Sender.Parent.Items[i]
        else if Sender.Parent.Items[i].Checked then
          FNoneGrp := False;

      if Assigned(NoneMenu) then
        NoneMenu.Checked := FNoneGrp;
    end;
  end;
end;

procedure TBaseGridForm.OnGroupClick(Sender: TObject);
var
  FM: TFieldMeta;
  i, N: Integer;
  OwnerMeta: TDataSetMeta;
  MainForm: TBaseMainForm;
begin
  MainForm := TBaseMainForm(MainBaseForm);
  if DataSetMeta.Role = drMain then
  begin
    if SameText(TMenuItem(Sender).Name, MM_NONEGROUP) then
    begin
      TMenuItem(Sender).Checked := True;
      TuneCheckedState(TMenuItem(Sender));
      GroupDeleteAll;
    end
    else

    begin
      // FM:= TFieldMeta(StrToInt(Copy(TMenuItem(Sender).Name,4,MaxInt)));
      FM := TFieldMeta(TMenuItem(Sender).Tag);
      if not Assigned(FM) then
        Exit;

      OwnerMeta := nil;
      for i := DataSetMeta.OwnerLinks.Count - 1 downto 0 do
      begin
        N := DataSetMeta.OwnerLinks[i].DataSetMeta.GroupFields.IndexByID(FM.ID);
        if N > -1 then
        begin
          OwnerMeta := DataSetMeta.OwnerLinks[i].DataSetMeta;
          Break;
        end;
      end;

      // Если нашли родительский TDataSetMeta - убиваем
      if OwnerMeta <> nil then
      begin
        TMenuItem(Sender).Checked := False;
        TuneCheckedState(TMenuItem(Sender));
        GroupDelete(OwnerMeta);
      end
      else
      begin
        // Если не нашли родительский TDataSetMeta - создаем
        TMenuItem(Sender).Checked := True;
        TuneCheckedState(TMenuItem(Sender));
        if Assigned(DataSetMeta.Cache.FocusedItem) then
          GroupCreate(FM, DataSetMeta.Cache.FocusedItem.ValueByName[FM.Original])
        else
          GroupCreate(FM, unassigned);
      end;
    end;
  end
  else if DataSetMeta.Role = drParent then
  begin
    GroupDelete(DataSetMeta);
    Close;
  end;
  if Assigned(MainForm) then
    MainForm.RefreshMenu;
end;

procedure TBaseGridForm.GroupCreate(AFieldMeta: TFieldMeta; aParentKeyValue: Variant);
var
  NewMeta: TDataSetMeta;
  BaseForm: TBaseDataForm;
  i, N: Integer;
begin
  // если пришел не пустой aFocusedID, то позиционируемся на него, иначе на первую запись
  if not SecuritySystem.CheckPolicyDataSet(AFieldMeta.Owner.ID, spSelect) then
    Exit;
  for i := DataSetMeta.OwnerLinks.Count - 1 downto 0 do
  begin
    N := DataSetMeta.OwnerLinks[i].DataSetMeta.GroupFields.IndexByID
      (AFieldMeta.ID);
    if N > -1 then
      Exit;
  end;

  DataSetMeta.Cache.CloseData;
  if not Assigned(DataSetMeta.GroupFields.FindByID(AFieldMeta.ID)) then
    DataSetMeta.GroupFields.Add(AFieldMeta);

  NewMeta := DataSetMeta.CreateParent(AFieldMeta);
  NewMeta.GroupFields.Add(AFieldMeta);
  NewMeta.Context.ViewType := NewMeta.Table.GroupViewType;
  // NewMeta.ViewParams := NewMeta.Table.GroupViewParams;
  BaseForm := TBaseGridForm(ViewTypeToClass(NewMeta.Context.ViewType).Create(Application));

  if pnlGroup.ControlCount > 0 then
  begin
    pnlGroup.Controls[pnlGroup.ControlCount - 1].Align := alTop;

    with TSplitter.Create(pnlGroup) do
    begin
      Parent := pnlGroup;
      Align := alTop;
      Color := clBtnFace;
      AutoSnap := False;
      MinSize := 17;
    end;
  end;

  if (0 < NewMeta.Cache.Count) then
  begin
    N := 0;

    if not VarIsEmpty(aParentKeyValue) then
      for i := 0 to NewMeta.Cache.Count - 1 do
        if VarSameValue(NewMeta.Cache.Items[i].ID, aParentKeyValue) then
        begin
          N := i;
          Break;
        end;

    NewMeta.Cache.ClearSelection;
    NewMeta.Cache.Items[N].SetFocus;
    NewMeta.Cache.Items[N].Selected := True;
  end;

  BaseForm.ManualDock(pnlGroup);
  BaseForm.ReInitForm(NewMeta);

  BaseForm.Align := alClient;
  BaseForm.Visible := True;
  Perform(WM_BGF_GRPCHANGE, 0, 0);

  if (DataSetMeta.Role = drMain) then
    DataSetMeta.OwnersWedding;
end;

procedure TBaseGridForm.GroupDelete(DeleteMeta: TDataSetMeta);
var
  i, j: Integer;
  ChildMeta: TDataSetMeta;
  GridForm: TBaseForm;
  FM: TFieldMeta;
begin
  for i := DeleteMeta.ChildrenCount - 1 downto 0 do
  begin
    ChildMeta := DeleteMeta.Children[i];

    for j := ChildMeta.OwnerLinks.Count - 1 downto 0 do
      if ChildMeta.OwnerLinks[j].DataSetMeta = DeleteMeta then
      begin
        ChildMeta.OwnerLinks.RemoveDataSet(DeleteMeta);
        DeleteMeta.Children.Extract(ChildMeta);
        Break;
      end;

    for j := 0 to DeleteMeta.GroupFields.Count - 1 do
    begin
      FM := ChildMeta.GroupFields.FindByID(DeleteMeta.GroupFields[j].ID);
      if Assigned(FM) then
        ChildMeta.GroupFields.Delete(ChildMeta.GroupFields.IndexByID(FM.ID));
    end;

    GridForm := DeleteMeta.GridForm;

    if Assigned(GridForm) then
    begin
      if (GridForm <> self) then
      begin
        GridForm.DeInitForm;
        GridForm.Free;
        Perform(WM_BGF_GRPCHANGE, 0, 0);
        DataSetMeta.Cache.Update(mcNone, null);
        // DataSetMeta.GroupFields
      end
      else
      begin
        GridForm.DeInitForm;
        Visible := False;
        Parent := nil;
        ChildMeta.GridForm.Perform(WM_BGF_GRPCHANGE, 0, 0);
        ChildMeta.Cache.Update(mcNone, null);
      end;
    end;
  end;
  DeleteMeta.Free;
end;

procedure TBaseGridForm.GroupDeleteAll(isClose: Boolean);
var
  i: Integer;
  DeleteMeta: TDataSetMeta;
  GridForm: TBaseForm;
begin
  for i := DataSetMeta.OwnerLinks.Count - 1 downto 0 do
  begin
    DeleteMeta := DataSetMeta.OwnerLinks[i].DataSetMeta;
    DataSetMeta.OwnerLinks.RemoveDataSet(DeleteMeta);
    DeleteMeta.Children.Extract(DataSetMeta);
    GridForm := DeleteMeta.GridForm;
    if Assigned(GridForm) then
    begin
      GridForm.DeInitForm;
      GridForm.Free;
    end;
    DeleteMeta.Free;
  end;
  if not isClose then
  begin
    Perform(WM_BGF_GRPCHANGE, 0, 0);
    DataSetMeta.GroupFields.Clear;
    DataSetMeta.Cache.Update(mcNone, null);
  end;
end;

procedure TBaseGridForm.WMBGFGrpChange(var Message: TMessage);
begin
  pnlGroup.Perform(WM_SIZE, 0, 0);
  GroupMenuCreate(self.MM_Da_Groupby);
  GroupMenuCreate(self.Item_Da_Groupby);
  DisplayMenuCreate(self.Item_Da_Show);
end;

procedure TBaseGridForm.OnSortClick(Sender: TObject);
var
  SD: TDSSortDirection;
begin
  if Sender is TMetaAction then
  begin
    if not Assigned(TMetaAction(Sender).FieldMeta) then
      Exit;
    if (CacheItems.SortList.Count = 1) and
      (CacheItems.SortList[0].FieldID = TMetaAction(Sender).FieldMeta.ID) then
    begin
      SD := CacheItems.SortList[0].NextDirection;
      if SD = sdNone then
        SD := sdAscending;
      CacheItems.SortList[0].Direction := SD;
    end
    else
    begin
      CacheItems.ClearSortList;
      CacheItems.SortList.Add(TMetaAction(Sender).FieldMeta.ID, sdAscending);
    end;
  end;

  DataSetMeta.Cache.Update(mcUpdate, null);
end;

procedure TBaseGridForm.DeleteSelectedItems;
var
  mes: string;
  i, C: Integer;

  procedure qsort(L, R: Integer);
  var
    i, j, m: Integer;
    Fi, Fj, FM: Variant;
  begin
    i := L;
    j := R;
    m := (R + L) div 2;
    FM := DataSetMeta.Cache.SelectedItems[m].ID;
    repeat

      Fi := DataSetMeta.Cache.SelectedItems[i].ID;
      while (Fi > FM) do
      begin
        i := i + 1;
        Fi := DataSetMeta.Cache.SelectedItems[i].ID;
      end;

      Fj := DataSetMeta.Cache.SelectedItems[j].ID;
      while (FM > Fj) do
      begin
        j := j - 1;
        Fj := DataSetMeta.Cache.SelectedItems[j].ID;
      end;

      if (i <= j) then
      begin
        if (Fi < Fj) then
          DataSetMeta.Cache.CacheSelectedItems.Exchange(i, j);
        i := i + 1;
        j := j - 1;
      end;
    until i > j;
    if L < j then
      qsort(L, j);
    if i < R then
      qsort(i, R);
  end;

begin
  C := DataSetMeta.Cache.SelectedCount;

  if C = 0 then
    Exit;

  if C = 1 then
  begin
    mes := DataSetMeta.Cache.SelectedItems[0].Caption;
    if mes = EmptyStr then
      mes := GetTitle('_Dv.Noname');
    if DataSetMeta.Cache.SelectedItems[0].Deleted or (not Assigned(DataSetMeta.Table.DField)) then
      mes := Format(GetTitle('_qUerypermdelete'), [mes])
    else
      mes := Format(GetTitle('_qUerydelete'), [mes]);
  end
  else
    mes := Format(GetTitle('_qUerypermdeleteall'), [IntToStr(C)]);

  if Application.MessageBox(PChar(mes), PChar(GetTitle('_Dl.Confirmation')),
    MB_YESNO or MB_ICONQUESTION) = idYes then
  begin
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, C, 1);
    try
      MetaData.BeginUpdate;
      DataSetMeta.Cache.BeginUpdate;

      // Сортируем по убыванию ключа, чтобы снизить вероятность удаления родителя
      // и сообщения объект удален другим пользователем
      if (C > 1) and (Assigned(DataSetMeta.Table.PField)) then
        qsort(0, DataSetMeta.Cache.SelectedCount - 1);

      for i := 0 to Pred(C) { DataSetMeta.Cache.SelectedCount - 1 } do
      begin
        DataSetMeta.Cache.DeleteRecord(DataSetMeta.Cache.SelectedItems[i]);
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, i, 0);
        DataSetMeta.Cache.Update(mcDelete, DataSetMeta.Cache.SelectedItems[i].ID)
      end;

      DataSetMeta.Cache.EndUpdate;
      MetaData.EndUpdate;

    finally
      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
    end;
  end;
end;

procedure TBaseGridForm.act_CutExecute(Sender: TObject);
begin
  if Assigned(DataSetMeta) and (DataSetMeta.Cache.Count > 0) then
  begin
    DataSetMeta.Cache.CopyToClipboard;
    DeleteSelectedItems;
  end;
end;

procedure TBaseGridForm.DoAgrigate(Sender: TObject);
begin
  SendMessage(Application.MainForm.Handle, DM_STATUSNOTIFY, NativeInt(StatusText), Handle);

  act_Da_AgrNo.Checked := (MetaData.AgregateType = atNo);
  act_Da_AgrCount.Checked := (MetaData.AgregateType = atCount);
  act_Da_AgrValue.Checked := (MetaData.AgregateType = atValue);
  act_Da_AgrSum.Checked := (MetaData.AgregateType = atSum);
  act_Da_AgrMin.Checked := (MetaData.AgregateType = atMin);
  act_Da_AgrMax.Checked := (MetaData.AgregateType = atMax);
  act_Da_AgrAve.Checked := (MetaData.AgregateType = atAverage);
end;

procedure TBaseGridForm.act_Da_AgrNoExecute(Sender: TObject);
begin
  inherited;
  MetaData.AgregateType := atNo;
  DoAgrigate(Sender);
end;

procedure TBaseGridForm.act_Da_AgrCountExecute(Sender: TObject);
begin
  inherited;
  MetaData.AgregateType := atCount;
  DoAgrigate(Sender);
end;

procedure TBaseGridForm.act_Da_AgrValueExecute(Sender: TObject);
begin
  inherited;
  MetaData.AgregateType := atValue;
  DoAgrigate(Sender);
end;

procedure TBaseGridForm.act_Da_AgrSumExecute(Sender: TObject);
begin
  inherited;
  MetaData.AgregateType := atSum;
  DoAgrigate(Sender);
end;

procedure TBaseGridForm.act_Da_AgrMinExecute(Sender: TObject);
begin
  inherited;
  MetaData.AgregateType := atMin;
  DoAgrigate(Sender);
end;

procedure TBaseGridForm.act_Da_AgrMaxExecute(Sender: TObject);
begin
  inherited;
  MetaData.AgregateType := atMax;
  DoAgrigate(Sender);
end;

procedure TBaseGridForm.act_Da_AgrAveExecute(Sender: TObject);
begin
  inherited;
  MetaData.AgregateType := atAverage;
  DoAgrigate(Sender);
end;

procedure TBaseGridForm.act_CopyExecute(Sender: TObject);
begin
  if Assigned(DataSetMeta) and (DataSetMeta.Cache.Count > 0) then
  begin
    DataSetMeta.Cache.CopyToClipboard;
    ChangeDataSetStatus;
  end;
end;

procedure TBaseGridForm.act_PasteExecute(Sender: TObject);
begin
  if Assigned(DataSetMeta) then
    if DataSetMeta.Cache.PasteRecords then
      DataSetMeta.Cache.Update(mcUpdate, null);;
end;

procedure TBaseGridForm.Act_Da_DefaultExecute(Sender: TObject);
begin
  DataSetMeta.Cache.FocusedItem.Default := Not Act_Da_Default.Checked;
  if DataSetMeta.Cache.UpdateRecord(DataSetMeta.Cache.FocusedItem) then
    DataSetMeta.Cache.Update(mcUpdate, null);
end;

procedure TBaseGridForm.act_CreateShortcutExecute(Sender: TObject);
var
  RecordEditor: TRecordEditor;
  DMan: TDataManager;
  i: Integer;

  // StringData   : TStringList;
  // TMeta        : TTableMeta;
  ClipID, FCount { ,IDindex } : Integer;
  RecordID: Variant;
begin
  if DataSetMeta.Table.IsNotes or DataSetMeta.Table.IsInterrelations then
    Exit;
  if DataSetMeta.Cache.SelectedCount = 0 then
    Exit;
  if not DeClipboard.GetInfo(ClipID, FCount, RecordID) then
    Exit;
  // if not DeClipboard.ContainsDeData then Exit;

  RecordEditor := TRecordEditor.Create(MetaData.MetaTables[idxInterRelations], null);
  RecordEditor.CacheItem.BeginInit;

  RecordEditor.CacheItem.InitFieldValueByNameExternal(fldIRChildTable, DataSetMeta.Table.ID);
  RecordEditor.CacheItem.InitFieldValueByNameExternal(fldIRChildKey, DataSetMeta.Cache.SelectedItems[0].ID);

  RecordEditor.CacheItem.InitFieldValueByNameExternal(fldIRParentTable, ClipID);
  RecordEditor.CacheItem.InitFieldValueByNameExternal(fldIRParentKey, RecordID { StringData[0*FCount+IDindex+1] } );

  // StringData.Free;
  RecordEditor.CacheItem.EndInit;

  if RecordEditor.Execute then
  begin
    DMan := TDataManager.Create;
    DMan.Table := MetaData.MetaTables[idxInterRelations];
    // DMan.PrepareRecord(RecordEditor.CacheItem); // проставляем значения вручную

    DMan.SetPrimaryKey(RecordEditor.CacheItem);
    DMan.InsertRecord(RecordEditor.CacheItem);

    for i := 0 to MetaData.DataSetsCount - 1 do
      if MetaData.DataSets[i].Table.IsInterrelations then
        MetaData.DataSets[i].Cache.Update(mcInsert, RecordEditor.CacheItem.ID);

    DMan.Free;
  end;

  RecordEditor.Free;
end;

procedure TBaseGridForm.act_CreateNoteExecute(Sender: TObject);
var
  RecordEditor: TRecordEditor;
  DMan: TDataManager;
  i: Integer;
begin
  if DataSetMeta.Table.IsNotes or DataSetMeta.Table.IsInterrelations then
    Exit;
  if DataSetMeta.Cache.SelectedCount = 0 then
    Exit;

  RecordEditor := TRecordEditor.Create(MetaData.MetaTables[idxNotes], null);
  RecordEditor.CacheItem.BeginInit;
  DMan := TDataManager.Create;
  DMan.Table := MetaData.MetaTables[idxNotes];
  DMan.PrepareRecord(RecordEditor.CacheItem);

  RecordEditor.CacheItem.InitFieldValueByNameExternal(fldNotesTable, DataSetMeta.Table.ID);
  RecordEditor.CacheItem.InitFieldValueByNameExternal(fldNotesKey, DataSetMeta.Cache.SelectedItems[0].ID);
  RecordEditor.CacheItem.EndInit;

  if RecordEditor.Execute then
  begin
    DMan.SetPrimaryKey(RecordEditor.CacheItem);
    DMan.InsertRecord(RecordEditor.CacheItem);

    for i := 0 to MetaData.DataSetsCount - 1 do
      if MetaData.DataSets[i].Table.IsNotes then
        MetaData.DataSets[i].Cache.Update(mcInsert, RecordEditor.CacheItem.ID);
  end;

  DMan.Free;
  RecordEditor.Free;
end;

procedure TBaseGridForm.act_PropertiesExecute(Sender: TObject);
var
  TableMeta: TTableMeta;
  ItemID: Variant;
  RecordEditor: TRecordEditor;
begin
  if not DataSetMeta.Table.IsInterrelations then
    OpenCard
  else
  begin
    DataSetMeta.GetLinkedData(DataSetMeta.Cache.FocusedIndex,
      TableMeta, ItemID);
    if Assigned(TableMeta) then
    begin
      RecordEditor := TRecordEditor.Create(TableMeta, ItemID);
      RecordEditor.Caption := GetTitle('_Dl.Properties ') + ' ''' + GetTitle(TableMeta.Name) + '''';
      RecordEditor.Execute;
      RecordEditor.Free;
    end
    else
    begin
      if DataSetMeta.CardForm = nil then
      begin
        TAForm.Create(Application).ReInitForm(DataSetMeta);
        TAForm(DataSetMeta.CardForm).CacheItem := DataSetMeta.Cache.FocusedItem;
        DataSetMeta.CardForm.Caption := GetTitle('_Dl.Properties ') + ' ''' + GetTitle(DataSetMeta.Table.Name) + '''';
      end;
      DataSetMeta.CardForm.ShowModal;
    end;
  end;
end;

procedure TBaseGridForm.OpenCard;
var
  RecordEditor: TRecordEditor;
  FilterItem: TFilterItem;
  Hidden: Boolean;
begin
  if CanShowCard and Assigned(DataSetMeta.Cache.FocusedItem) then
  begin
    pnlGridExit(pnlGrid);

    RecordEditor := TRecordEditor.Create(DataSetMeta.Table, CacheItems.FocusedItem.ID);
    try
      if RecordEditor.Execute then
      begin
        // Hidden:= False;

        FilterItem := TFilterItem.Create;
        try
          DataSetMeta.ReadAllFilters(FilterItem);
          Hidden := FilterItem.Active and not RecordEditor.CacheItem.Calculate(FilterItem, False)
        finally
          FilterItem.Free;
        end;

        DataSetMeta.Cache.Update(mcNone, null);
        MetaData.UpdateLibrary(DataSetMeta.Table.ID);

        if Hidden then
          SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo,
            NativeInt(PChar(GetTitle('_De.HideSaved', ttFull))));
      end;
    finally
      RecordEditor.Free;
    end;

    pnlGridEnter(pnlGrid);
  end;
end;

procedure TBaseGridForm.CacheItemInsert(Item: TCacheItem);
var
  ICount: Integer;
begin
  inherited;
  DataSetMeta.SetReferenceKeys(Item);

  { установка значений определенных текущим фильтром "=" }
  ICount := DataSetMeta.FilterPostfix.Count - 1;
  if DataSetMeta.SetFilterKeys(Item, ICount) and (ICount = -1) then
  begin
    ICount := DataSetMeta.FilterPostfix.Count - 1;
    DataSetMeta.SetFilterKeys(Item, ICount, True);
  end;

  { установка значений определенных текущим пользовательским фильтром "=" }
  {
    ICount :=DataSetMeta.UserFilterPostfix.Count-1;
    if DataSetMeta.SetFilterKeys(Item, ICount) and (ICount=-1) then
    begin
    ICount :=DataSetMeta.UserFilterPostfix.Count-1;
    DataSetMeta.SetFilterKeys(Item, ICount, True);
    end;
    { }
end;

procedure TBaseGridForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_ACCEPTFILES;
end;

procedure TBaseGridForm.WMDropFiles(var Message: TWMDropFiles);
var
  aFile: string;
  RecordEditor: TRecordEditor;
  DMan: TDataManager;
  FilterItem: TFilterItem;
  FileControl: TDeImage;
begin
  inherited;
  if not Assigned(DataSetMeta.Table) then
    Exit;
  if not DataSetMeta.Table.CanDropFiles then
    Exit;

  SetLength(aFile, MAX_PATH);
  SetLength(aFile, DragQueryFile(Message.drop, 0, PChar(aFile), MAX_PATH));
  if SameText(ExtractFileExt(aFile), sExtensionLNK) then
    Exit;

  if Not(DataSetMeta.Table.NField.DataType in StringTypes) then
    Exit;

  DMan := TDataManager.Create;
  DMan.Table := DataSetMeta.Table;

  RecordEditor := TRecordEditor.Create(DataSetMeta.Table, null);
  RecordEditor.CacheItem.BeginInit;
  DMan.PrepareRecord(RecordEditor.CacheItem);

  if Assigned(DataSetMeta.Table.DropBlobField) then
  begin
    FileControl := TDeImage.Create(nil);
    FileControl.LoadFromFile(aFile);
    RecordEditor.CacheItem.InitFieldValueByNameExternal(DataSetMeta.Table.DropBlobField.Original, FileControl.Value);
    FileControl.Free;
  end;

  if Assigned(DataSetMeta.Table.DropNameField) then
    RecordEditor.CacheItem.InitFieldValueByNameExternal(DataSetMeta.Table.DropNameField.Original, ExtractFileName(aFile));

  CacheItemInsert(RecordEditor.CacheItem);

  // *****
  FilterItem := TFilterItem.Create;
  try
    // TDataSetMeta(DMan.DataSetMeta) := DataSetMeta;
    DataSetMeta.ReadAllFilters(FilterItem);
    RecordEditor.DataSetMeta.FilterPostfix.Assign(FilterItem);
  finally
    FilterItem.Free;
  end;
  // *****

  RecordEditor.CacheItem.EndInit;

  if RecordEditor.Execute then
  begin
    DMan.SetPrimaryKey(RecordEditor.CacheItem);
    if DMan.InsertRecord(RecordEditor.CacheItem) then
      DataSetMeta.Cache.Update(mcInsert, RecordEditor.CacheItem.ID)
    else
      SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoError,
        NativeInt(PChar(GetTitle('_dL.procedureerror', ttFull))));

    MetaData.UpdateLibrary(DataSetMeta.Table.ID);
  end;

  RecordEditor.Free;
  DMan.Free;

  DragFinish(Message.drop);
end;

procedure TBaseGridForm.act_AddExecute(Sender: TObject);
var
  RecordEditor: TRecordEditor;
  DMan: TDataManager;
  // StringData   : TStringList;
  // TMeta        : TTableMeta;
  ClipID, FCount { ,IDindex } : Integer;
  FilterItem: TFilterItem;
  RecordID: Variant;
begin
  DMan := TDataManager.Create;
  DMan.Table := DataSetMeta.Table;

  RecordEditor := TRecordEditor.Create(DataSetMeta.Table, null);
  RecordEditor.CacheItem.BeginInit;
  DMan.PrepareRecord(RecordEditor.CacheItem);
  CacheItemInsert(RecordEditor.CacheItem);

  // *****
  FilterItem := TFilterItem.Create;
  try
    // TDataSetMeta(DMan.DataSetMeta) := DataSetMeta;
    DataSetMeta.ReadAllFilters(FilterItem);
    RecordEditor.DataSetMeta.FilterPostfix.Assign(FilterItem);
  finally
    FilterItem.Free;
  end;
  // *****

  if DataSetMeta.Table.IsInterrelations and DeClipboard.GetInfo(ClipID, FCount, RecordID) then
    try
      RecordEditor.CacheItem.InitFieldValueByNameExternal(fldIRChildTable, ClipID);
      RecordEditor.CacheItem.InitFieldValueByNameExternal(fldIRChildKey, RecordID);
    except
{$IFDEF DEBUG}
      on E: Exception do
        DebugLog('TBaseGridForm.act_AddExecute error: ' + E.Message);
{$ENDIF}
    end;

  RecordEditor.CacheItem.EndInit;

  if RecordEditor.Execute then
  begin
    DMan.SetPrimaryKey(RecordEditor.CacheItem);
    if DMan.InsertRecord(RecordEditor.CacheItem) then
      DataSetMeta.Cache.Update(mcInsert, RecordEditor.CacheItem.ID)
    else
      SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoError,
        NativeInt(PChar(GetTitle('_dL.procedureerror', ttFull))));

    MetaData.UpdateLibrary(DataSetMeta.Table.ID);
  end;

  RecordEditor.Free;
  DMan.Free;
end;

procedure TBaseGridForm.act_DeleteExecute(Sender: TObject);
begin
  if Assigned(DataSetMeta) and (DataSetMeta.Cache.Count > 0) then
    DeleteSelectedItems;
end;

procedure TBaseGridForm.act_Da_RestoreExecute(Sender: TObject);
var
  i, C, N: Integer;
begin
  C := DataSetMeta.Cache.SelectedCount;
  N := CacheItems.Fields.IndexByName(CacheItems.TableMeta.DField.Original);
  {
    CacheItems.FocusedItem.FieldValue[CacheItems.Fields.IndexByName(
    CacheItems.TableMeta.SField.Original)] := CacheItems.TableMeta.SField.Value1;
    DataSetMeta.Cache.UpdateRecord(DataSetMeta.Cache.FocusedItem.ItemIndex);
    { }

  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, C, 1);
  try
    MetaData.BeginUpdate;
    DataSetMeta.Cache.BeginUpdate;
    for i :=0 to { DataSetMeta.Cache.SelectedCount - 1 } Pred(C) do
      if DataSetMeta.Cache.SelectedItems[i].Deleted then
      begin
        DataSetMeta.Cache.SelectedItems[i].FillItem(fsMax);
        if CacheItems.TableMeta.DField.DataType in DateTypes then
          DataSetMeta.Cache.SelectedItems[i].FieldValue[N] := null
        else
          DataSetMeta.Cache.SelectedItems[i].FieldValue[N] := CacheItems.TableMeta.DField.Value1;
        DataSetMeta.Cache.UpdateRecord(DataSetMeta.Cache.SelectedItems[i]);
        DataSetMeta.Cache.Update(mcNone, DataSetMeta.Cache.SelectedItems[i].ID);
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, i, 0);
      end;
    DataSetMeta.Cache.EndUpdate;
    MetaData.EndUpdate;
  finally
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  end;
end;

function TBaseGridForm.DragInfoTarget(Sender, Source: TObject; X, Y: Integer;
  aDragInfo: TDragInfo): Boolean;
var
  N: Integer;
begin
  aDragInfo.DragTypes := [];
  if not Assigned(DataSetMeta) then
    Exit(False);

  aDragInfo.Source := GetParentBaseGridForm(Source).DataSetMeta;
  aDragInfo.Color := clActiveCaption;

  if Assigned(FieldX) then aDragInfo.XField := DataSetMeta.Cache.Fields.IndexByID(FieldX.ID)
                      else aDragInfo.XField := -1;
  if -1 < aDragInfo.XField then
    if FDragInfo.Source.Table.ID = DataSetMeta.Table.ID then
      include(aDragInfo.DragTypes, dtSelf);

  if Assigned(FieldY) then aDragInfo.YField := DataSetMeta.Cache.Fields.IndexByID(FieldY.ID)
                      else aDragInfo.YField := -1;
  if -1 < aDragInfo.YField then
    if FDragInfo.Source.Table.ID = DataSetMeta.Table.ID then
      include(aDragInfo.DragTypes, dtSelf);

  aDragInfo.OField := -1;
  if Assigned(DataSetMeta.Table.OField) then
    if 0 < DataSetMeta.Cache.SortList.Count then
      if DataSetMeta.Cache.SortList[0].FieldID = DataSetMeta.Table.OField.ID then
        if DataSetMeta.Cache.SortList[0].Direction <> sdNone then
          if FDragInfo.Source.Table.ID = DataSetMeta.Table.ID then
            begin
              aDragInfo.OField:= DataSetMeta.Cache.Fields.IndexByID(DataSetMeta.Table.OField.ID);
              include(aDragInfo.DragTypes, dtSelf);
            end;

  if assigned(DataSetMeta) then
    if DataSetMeta = aDragInfo.Source then
    begin
      aDragInfo.IField := aDragInfo.Source.Table.Fields.IndexByLink(DataSetMeta.Table.ID);
      if -1 < aDragInfo.IField then
        include(aDragInfo.DragTypes, dtSelf);
    end;

  if aDragInfo.DragTypes = [] then
  begin
    N := FDragInfo.Source.GroupFields.IndexByLink(DataSetMeta.Table.ID);
    if -1 < N then
      aDragInfo.IField := FDragInfo.Source.Table.Fields.IndexByID(FDragInfo.Source.GroupFields[N].ID)
    else
      aDragInfo.IField := FDragInfo.Source.Table.Fields.IndexByLink(DataSetMeta.Table.ID);
    if -1 < aDragInfo.IField then
      include(aDragInfo.DragTypes, dtParent);
  end;

  if aDragInfo.DragTypes = [] then
  begin
    N := DataSetMeta.GroupFields.IndexByLink(FDragInfo.Source.Table.ID);
    if -1 < N then
      aDragInfo.IField := DataSetMeta.Table.Fields.IndexByID(DataSetMeta.GroupFields[N].ID)
    else
      aDragInfo.IField := DataSetMeta.Table.Fields.IndexByLink(FDragInfo.Source.Table.ID);
    if -1 < aDragInfo.IField then
      include(aDragInfo.DragTypes, dtChild);
  end;

  Result := Not(aDragInfo.DragTypes = []);
end;

procedure TBaseGridForm.DoDragStart(Sender: TObject; var DragObject: TDragObject);
begin
  inherited;
end;

procedure TBaseGridForm.DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if State = dsDragLeave then FSelectRect.Hide;
end;

procedure TBaseGridForm.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  aItem: TCacheItem;
  i, j, N, R, G, UCount: Integer;
  B: Boolean;
  OrderDirection: TDSSortDirection;

  GroupCount: Integer;
  GroupListIndex: array of Integer;
  GroupListValue: array of Variant;

  function NextValue(Var aVar: Integer; const aInc: Integer): Integer;
  begin
    Result:= aVar;
    aVar:= aVar + aInc;
  end;

begin
  FSelectRect.Hide;

  DragInfoTarget(Sender, Source, X, Y, FDragInfo);
  if not Assigned(FDragInfo.Source) or (FDragInfo.DragTypes = []) then
    Exit;

  if FDragInfo.DragTypes * [dtSelf, dtParent] <> []then
  try
    DataSetMeta.Cache.BeginUpdate;

    // Меняем поля затрагивающие только выделенные записи: IValue, XValue, YValue
    for i := 0 to Pred(FDragInfo.Source.Cache.SelectedCount) do
      begin
        aItem := FDragInfo.Source.Cache.SelectedItems[i];
        if -1 < FDragInfo.IField then
          aItem.FieldValue[FDragInfo.IField]:= FDragInfo.IValue;
        if -1 < FDragInfo.XField then
          aItem.FieldValue[FDragInfo.XField]:= FDragInfo.XValue;
        if -1 < FDragInfo.YField then
          aItem.FieldValue[FDragInfo.YField]:= FDragInfo.YValue;
      end;

    // Меняем поле затрагивающее другие записи: OValue - номер по порядку
    if -1 < FDragInfo.OField then
      begin
        OrderDirection:= DataSetMeta.Cache.SortList.FirstFieldDirestion(FDragInfo.Source.Table.OField);
        // заполняем группы, внутри которых определяется порядок
        GroupCount:= 0;
        for i:=0 to Pred(FDragInfo.Source.Table.Fields.Count) do
          if frGroupDefault in FDragInfo.Source.Table.Fields[i].Role then
            begin
              SetLength(GroupListIndex, Succ(GroupCount));
              SetLength(GroupListValue, Succ(GroupCount));
              GroupListIndex[GroupCount]:= FDragInfo.Source.Table.Fields.IndexByID(FDragInfo.Source.Table.Fields[i].ID);
              GroupListValue[GroupCount]:= FDragInfo.Source.Cache[0].ValueByName[FDragInfo.Source.Table.Fields[i].Original];
              Inc(GroupCount);
            end;

        FDragInfo.OValue:= Min( Max(FDragInfo.OValue, -1), DataSetMeta.Cache.Count);

        case OrderDirection of
          sdAscending: // принимающий набор данных по возрастанию поля порядок
            begin
              R:= 0;
              N:= 0;
              for i:= 0 to DataSetMeta.Cache.Count do
                begin
                  B:= True;
                  for G:= 0 to Pred(GroupCount) do
                    B:= B and (DataSetMeta.Cache[i].FieldValue[GroupListIndex[G]] = GroupListValue[G]);

                  if B then
                    begin
                      if R = FDragInfo.OValue then
                        for j:= 0 to Pred(FDragInfo.Source.Cache.SelectedCount) do
                          FDragInfo.Source.Cache.SelectedItems[j].FieldValue[FDragInfo.OField]:= NextValue(N, 1);

                      if (-1 < R) and (R < DataSetMeta.Cache.Count) then
                        if Not ((DataSetMeta.Cache = FDragInfo.Source.Cache) and DataSetMeta.Cache[R].Selected) then
                          DataSetMeta.Cache[R].FieldValue[FDragInfo.OField]:= NextValue(N, 1);

                      Inc(R);
                    end;
                end;
            end;
          sdDescending: // принимающий набор данных по убыванию поля порядок
            begin
              R:= DataSetMeta.Cache.Count;

            end;
          sdNone:      // принимающий набор данных не отсортирован по полю порядок
            begin

            end;
        end;
      end;

    // Считаем количество измененных записей для прогресса
    UCount:= 0;
    for i:= 0 to Pred(FDragInfo.Source.Cache.Count) do
      if isModified in FDragInfo.Source.Cache.Items[i].State then Inc(UCount);

    // Сохраняем в базу
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, UCount, 1);
    UCount:= 0;
    for i:= 0 to Pred(FDragInfo.Source.Cache.Count) do
      if isModified in FDragInfo.Source.Cache.Items[i].State then
      begin
        if FDragInfo.Source.Cache.DataManager.CanUpdateRecord(FDragInfo.Source.Cache[i]) and
          FDragInfo.Source.Cache.DataManager.UpdateRecord(FDragInfo.Source.Cache[i]) then
        else
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
            NativeInt(PChar(DataSetMeta.Cache.DataManager.Errors.GetMessage)));
        Inc(UCount);
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, UCount, 0);
      end;

  finally
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
    DataSetMeta.Cache.EndUpdate;
  end;

  if dtChild in FDragInfo.DragTypes then
  begin
    DataSetMeta.Cache.BeginUpdate;
    DataSetMeta.Cache.ClearSelection;
    for i := 0 to Pred(FDragInfo.Source.Cache.SelectedCount) do
    begin
      aItem := DataSetMeta.Cache.InsertNewItem(DataSetMeta.Cache.Count);
      DataSetMeta.Cache.DataManager.PrepareRecord(aItem);

      B := True;
      if Assigned(DataSetMeta.Cache.OnInsertRecord) then
        DataSetMeta.Cache.OnInsertRecord(aItem, B);

      if -1 < FDragInfo.IField then
        aItem.FieldValue[FDragInfo.IField] :=
          FDragInfo.Source.Cache.SelectedItems[i].ID;
      if -1 < FDragInfo.XField then
        aItem.FieldValue[FDragInfo.XField] := FDragInfo.XValue;
      if -1 < FDragInfo.YField then
        aItem.FieldValue[FDragInfo.YField] := FDragInfo.YValue;
      if -1 < FDragInfo.OField then
        aItem.FieldValue[FDragInfo.OField] := FDragInfo.OValue;

      if DataSetMeta.Cache.DataManager.CanInsertRecord(aItem) and
        DataSetMeta.Cache.DataManager.InsertRecord(aItem)
      then { DataSetMeta.Cache.FocusedItem:= NewCacheItem }
      else
        SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
          NativeInt(PChar(DataSetMeta.Cache.DataManager.Errors.GetMessage)));
      aItem.Selected := True;
    end;
    DataSetMeta.Cache.EndUpdate;
  end;
end;

function TBaseGridForm.GetKeyValueAt(X, Y: Integer): Variant;
begin
  Result := unassigned;
end;

function TBaseGridForm.GetCacheItemAt(X, Y: Integer): TCacheItem;
begin
  Result := nil;
end;

function TBaseGridForm.getCaptionText: string;
var
  CaptionText, GroupText, CommaText: string;
  i: Integer;
begin
  Result := EmptyStr;
  if DataSetMeta = nil then
    Exit;
  GroupText := EmptyStr;
  if (DataSetMeta.Role = drParent) and (DataSetMeta.GroupFields.Count > 0) then
    CaptionText := GetTitle(DataSetMeta.GroupFields[0].Name, ttSecondName)
  else
    CaptionText := GetTitle(DataSetMeta.Table.Name, ttSecondName);

  if DataSetMeta.Role = drMain then
  begin
    for i := 0 to DataSetMeta.OwnerLinks.Count - 1 do
    begin
      if i > 0 then
        CommaText := ', ';
      if DataSetMeta.OwnerLinks[i].DataSetMeta.GroupFields.Count > 0 then
        GroupText := GroupText + CommaText + GetTitle(DataSetMeta.OwnerLinks[i].DataSetMeta.GroupFields[0].Name, ttSecondName)
      else
        GroupText := GroupText + CommaText + GetTitle(DataSetMeta.OwnerLinks[i].DataSetMeta.Table.Name, ttSecondName);
    end;
    if Length(GroupText) > 0 then
      GroupText := GroupText + ' - ';

  end;
  Result := GroupText + CaptionText;
end;

procedure TBaseGridForm.SetCaptionDirection(const Value: TCaptionDirection = cdAuto);
begin
  FormCaption := getCaptionText;
  FCaptionDirection := Value;

  { Спрятать заголовок }
  if Value = cdNone then
  begin
    pnlClient.Visible := False;
  end
  else

    { Горизонтальный заголовок }
    if (Value = cdHorizontal) or
      ((Value = cdAuto) and (pnlMainR.Width > 2 * FCaptionHeight)) then
    begin
      pnlClient.Top := 0;
      pnlClient.Align := alTop;
      pnlClient.Height := FCaptionHeight + FCaptionBorder;
      pnlClient.Visible := True;
    end
    else

    { Вертикальный заголовок }
    begin
      pnlClient.Left := 0;
      pnlClient.Align := alLeft;
      pnlClient.Width := FCaptionHeight + FCaptionBorder;
      pnlClient.Visible := True;
    end;
end;

procedure TBaseGridForm.pnlClientMouseActivate(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  inherited;
  if Assigned(MainControl) then
    MainControl.SetFocus;
end;

procedure TBaseGridForm.pnlMainRResize(Sender: TObject);
var
  N, W: Integer;
begin
  inherited;
  actFlip.Visible := (0 < DataSetMeta.OwnerLinks.Count) and (DataSetMeta.Role = drMain);
  SetCaptionDirection;

  N := 0;
  W := FCaptionHeight;

  if pnlClient.Align = alTop then
  begin
    GradientPaint.SetBounds(0, 0, pnlClient.Width, FCaptionHeight);

    btnMenu.SetBounds(2, 0, (3 * W) div 2 - 2, FCaptionHeight);
    if DataSetMeta.Role in [drMain, drChild] then
      W := (3 * W) div 2;

    if btnClose.Visible then
    begin
      btnClose.SetBounds(pnlClient.Width - N - W, 0, W - 1, FCaptionHeight);
      Inc(N, W);
    end;
    if actMaximize.Visible then
    begin
      btnMaximize.SetBounds(pnlClient.Width - N - W, 0, W - 1, FCaptionHeight);
      Inc(N, W);
    end;
    if actFlip.Visible then
    begin
      btnFlip.SetBounds(pnlClient.Width - N - W, 0, W - 1, FCaptionHeight);
      Inc(N, W);
    end;
    if btnFetchAll.Visible then
    begin
      btnFetchAll.SetBounds(pnlClient.Width - N - W, 0, W - 1, FCaptionHeight);
    end;
  end
  else
  begin
    GradientPaint.SetBounds(0, 0, FCaptionHeight, pnlClient.Height);

    if btnClose.Visible then
    begin
      btnClose.SetBounds(0, N, FCaptionHeight, W - 1);
      Inc(N, W);
    end;
    if actMaximize.Visible then
    begin
      btnMaximize.SetBounds(0, N, FCaptionHeight, W - 1);
      Inc(N, W);
    end;
    if actFlip.Visible then
    begin
      btnFlip.SetBounds(0, N, FCaptionHeight, W - 1);
      Inc(N, W);
    end;
    if btnFetchAll.Visible then
    begin
      btnFetchAll.SetBounds(0, N, FCaptionHeight, W - 1);
    end;

    W := (3 * W) div 2;
    btnMenu.SetBounds(0, pnlClient.Height - W, FCaptionHeight, W - 1);
  end;
end;

procedure TBaseGridForm.ShowGroupArea(const isShow: Boolean);
begin
  pnlMain.DisableAlign; // Выключаем алигн - Убираем двойное изменение ширины

  if pnlGroup.Width = 0 then
    pnlGroup.Width := 224 * Integer(isShow);
  pnlGroup.Visible := isShow;
  GroupSplitter.Left := pnlGroup.Width;
  GroupSplitter.Visible := isShow;

  pnlMain.EnableAlign;
end;

procedure TBaseGridForm.pnlGroupResize(Sender: TObject);
var
  i, j, k, spltCount: Integer;
  PredCotrol: TControl;
  AControls: TList;
begin
  pnlGroup.DisableAlign;

  PredCotrol := nil;
  AControls := TList.Create;

  for i := 0 to pnlGroup.ControlCount - 1 do
  begin
    k := 0;
    for j := 0 to AControls.Count - 1 do
      if pnlGroup.Controls[i].Top < TControl(AControls[j]).Top then
        Break
      else
        Inc(k);
    AControls.Insert(k, pnlGroup.Controls[i]);
  end;

  for i := AControls.Count - 1 downto 0 do
    if TControl(AControls[i]).ClassType = TSplitter then
    begin
      if (i in [0, AControls.Count - 1]) or
        (Assigned(PredCotrol) and (PredCotrol.ClassType = TSplitter)) then
        TControl(AControls[i]).Free
      else
        PredCotrol := TControl(AControls[i]);
    end
    else
      PredCotrol := TControl(AControls[i]);

  spltCount := (AControls.Count - 1) div 2;

  AControls.Free;

  for i := 0 to pnlGroup.ControlCount - 1 do
  begin
    if pnlGroup.Controls[i].ClassType = TSplitter then
    begin
      pnlGroup.Controls[i].Height := 4;
      pnlGroup.Controls[i].Align := alTop;
    end
    else
    begin
      pnlGroup.Controls[i].Height :=
        ((pnlGroup.ClientHeight - (spltCount * 4)) div (pnlGroup.ControlCount - spltCount));
      if i < pnlGroup.ControlCount - 1 then
        pnlGroup.Controls[i].Align := alTop
      else
        pnlGroup.Controls[i].Align := alClient;
    end;

  end;

  pnlGroup.EnableAlign;
  ShowGroupArea(pnlGroup.ControlCount > 0);
  // UpdateWindow(pnlGrid.Handle);
end;

procedure TBaseGridForm.actCloseExecute(Sender: TObject);
var
  MainForm: TBaseMainForm;
begin
  MainForm := TBaseMainForm(MainBaseForm);
  GroupDelete(DataSetMeta);
  Close;
  if Assigned(MainForm) then
    MainForm.RefreshMenu;
end;

procedure TBaseGridForm.SetCanDrag(Value: Boolean);
begin
  FCanDrag := Value;
end;

procedure TBaseGridForm.SetCanGrouping(const Value: Boolean);
begin
  FCanGrouping := Value;
end;

procedure TBaseGridForm.SetCanShowCard(const Value: Boolean);
begin
  FCanShowCard := Value;
  if FViewArea and not CanShowCard then
    HideViewArea(False);
end;

function TBaseGridForm.GetCanShowCard: Boolean;
begin
  Result := FCanShowCard;
end;

procedure TBaseGridForm.SetCanSorting(const Value: Boolean);
begin
  FCanSorting := Value;
end;

{
  procedure TBaseGridForm.CreateParams(var Params: TCreateParams);
  begin
  inherited;
  //  Params.Style := Params.Style and (not WS_CAPTION);
  end;
}

procedure TBaseGridForm.WMChangeActive(var Message: TMessage);
begin
  FWindowActive := (Message.WParam = 1);

  // Обновим локальные фильтры на кнопке главной формы ...
  if FWindowActive then
    if Assigned(MForm) then
      { UpdateLocalFilterMenu(MForm.LocalFilterPopupMenu.Items) };

  pnlClient.Repaint;
end;

procedure TBaseGridForm.GradientPaintPaint(Sender: TObject);
var
  TH: Integer;
  NonClientMetrics: TNonClientMetrics;
  LogFont: tagLOGFONT;
  OldFontHandle, NewFontHandle: HFONT;
  Icon: TIcon;
begin
  Icon := TIcon.Create;
  if pnlClient.Align = alTop then
    if FWindowActive then
      GradientPaint.Canvas.StretchDraw(GradientPaint.ClientRect, DM.PatternHCaptionActive)
    else
      GradientPaint.Canvas.StretchDraw(GradientPaint.ClientRect, DM.PatternHCaptionInactive)
  else if FWindowActive then
    GradientPaint.Canvas.StretchDraw(GradientPaint.ClientRect, DM.PatternVCaptionActive)
  else
    GradientPaint.Canvas.StretchDraw(GradientPaint.ClientRect, DM.PatternVCaptionInactive);

  if Assigned(DataSetMeta) and Assigned(DataSetMeta.Table) then
    DM.ilIcon16.GetIcon(DM.MapIconIndex(DataSetMeta.Ico), Icon)
  else
    DM.ilIcon16.GetIcon(DM.MapIconIndex(96), Icon);

  inherited;
  NonClientMetrics.cbSize := sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    GradientPaint.Canvas.Font.Handle :=
      CreateFontIndirect(NonClientMetrics.lfCaptionFont)
  else
    GradientPaint.Canvas.Font.Handle := GetStockObject(SYSTEM_FONT);

  TH := GradientPaint.Canvas.TextHeight('Yy');

  if FWindowActive then
  begin
    GradientPaint.Canvas.Font.Color := clCaptionText;
    GradientPaint.Canvas.Font.Style := [fsBold];
  end
  else
  begin
    GradientPaint.Canvas.Font.Color := clInactiveCaptionText;
    GradientPaint.Canvas.Font.Style := [];
    GradientPaint.Canvas.Brush.Style
  end;

  GradientPaint.Canvas.Brush.Style := bsClear;

  if pnlClient.Align = alTop then
  // GradientPaint.Height < GradientPaint.Width then
  begin
    GradientPaint.Canvas.TextOut((3 * FCaptionHeight) div 2 + 3, (GradientPaint.Height - TH) div 2, FFormCaption);
    GradientPaint.Canvas.Draw(2 + ((3 * FCaptionHeight) div 2 - Icon.Width)  div 2, (FCaptionHeight - Icon.Height) div 2, Icon);
  end
  else
  begin
    if GetObject(GradientPaint.Canvas.Font.Handle, sizeof(LogFont), @LogFont) = 0
    then
      Exit;

    LogFont.lfEscapement := 90 * 10;
    LogFont.lfCharset := 1;
    LogFont.lfOutPrecision := OUT_TT_PRECIS;
    LogFont.lfHeight := -11;
    LogFont.lfQuality := DEFAULT_QUALITY; // PROOF_QUALITY;

    NewFontHandle := CreateFontIndirect(LogFont);
    try
      OldFontHandle := SelectObject(GradientPaint.Canvas.Handle, NewFontHandle);
      try
        SetBKMode(GradientPaint.Canvas.Handle, TRANSPARENT);
        if Assigned(DataSetMeta.Cache.FocusedItem) then
          GradientPaint.Canvas.TextOut((FCaptionHeight - TH) div 2, GradientPaint.Height - (3 * FCaptionHeight) div 2 - 3,
                                       FFormCaption + ' : ' + DataSetMeta.Cache.FocusedItem.Caption)
        else
          GradientPaint.Canvas.TextOut((FCaptionHeight - TH) div 2, GradientPaint.Height - (3 * FCaptionHeight) div 2 - 3,
                                       FFormCaption);
      finally
        NewFontHandle := SelectObject(GradientPaint.Canvas.Handle, OldFontHandle);
      end;
    finally
      DeleteObject(NewFontHandle);
    end;
    GradientPaint.Canvas.Draw((FCaptionHeight - Icon.Width) div 2,
      GradientPaint.Height - Icon.Height - 2 - ((3 * FCaptionHeight) div 2 - Icon.Height) div 2, Icon);
  end;

  Icon.Free;
end;

procedure TBaseGridForm.SetZOrder(TopMost: Boolean);
begin
  if TopMost = False then
    inherited SetZOrder(TopMost);

  UpdateWindow(Handle);

  if TopMost = True then
    inherited SetZOrder(TopMost);
end;

function TBaseGridForm.getPrintReport(aStyle: TDePrintStyle): TDeReport;
begin
  Result := nil;
  if aStyle.TypeID = PS_Table_TypeID then
  begin
    Result := TDeStdWordReport.Create(DataSetMeta);
    TDeStdWordReport(Result).Style := aStyle;
  end
  else if aStyle.TypeID = PS_Card_TypeID then
  begin
    Result := TDeStdCardReport.Create(DataSetMeta);
    TDeStdCardReport(Result).Style := aStyle;
  end;
end;

procedure TBaseGridForm.doPrintReport(aStyle: TDePrintStyle;
  const PrinterName: string);
var
  aReport: TDeReport;
begin
  aReport := getPrintReport(aStyle);
  if Assigned(aReport) then
    with aReport do
      try
        if PrinterName = EmptyStr then
          Preview
        else
          Print(PrinterName);
      finally
        Free;
      end;
end;

function TBaseGridForm.getEnabledPrintStyles: TDePrintStyleTypes;
begin
  Result := [PS_Table_TypeID, PS_Card_TypeID];
end;

procedure TBaseGridForm.onPrintCardPreview(Sender: TObject);
var
  PrintDlg: TDePrintDialog2;
  aStyle: TDePrintStyle;
begin
  if Assigned(Sender) and (Sender is TDePrintDialog2) then
  begin
    PrintDlg := Sender as TDePrintDialog2;
    if Length(PrintDlg.StyleName) <> 0 then
    begin
      aStyle := PrintDlg.Styles[PrintDlg.StyleName];
      doPrintReport(aStyle, EmptyStr);
    end;
  end;
end;

procedure TBaseGridForm.act_PrintExecute(Sender: TObject);
var
  Variables: TDeVariableList;
  ActionName: string;
  ActionIndex: Integer;
begin
  Variables := TDeVariableList.Create;
  try
    if TDeCommandDialog.Execute(CacheItems.TableMeta.ID, Variables) = mrOK then
    begin
      if Assigned(Variables.GetByName(varActionID, False)) then
      begin
        try
          ActionIndex := VarToInt(Variables.GetValueByName(varActionID));
        except
          on E: Exception do
          begin
{$IFDEF DEBUG}
            DebugLog('TBaseGridForm.act_PrintExecute: Variables.GetValueByName(%s) skip error: %s',
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
          NativeInt(PChar(Format(GetTitle('_eRror.actionnotfound'), [ActionName]))));
    end;
  finally
    Variables.Free;
  end;
end;

procedure TBaseGridForm.MakeVisibleItem(Item: TCacheItem);
begin
  if Assigned(Item) then
    inherited;
end;

function TBaseGridForm.ShowViewArea(Manual: Boolean = False): Boolean;
var
  i, H: Integer;
  CardForm: TAForm;
begin
  if FViewArea then
    Exit(False)
  else
    Result := True;

  if (DataSetMeta.Role = drMain) and (CanShowCard) and
    (DataSetMeta.Context.ItemType <> itAttribute) and Not IsMultiWIndow then
  begin
    if not Assigned(DataSetMeta.CardForm) then
    begin
      CardForm := TAForm.Create(Application);
      CardForm.ManualDock(pnlCard);
      CardForm.ReInitForm(DataSetMeta);
    end
    else
    begin
      CardForm := TAForm(DataSetMeta.CardForm);
      CardForm.ManualDock(pnlCard);
    end;

    if not Assigned(CardForm) then
      Exit(False);

    if Not Assigned(AFormSplitter) then
    begin
      AFormSplitter := TExtSplitter.Create(self);
      AFormSplitter.Visible := False;
      AFormSplitter.Parent := self;
      AFormSplitter.Align := alBottom;
      AFormSplitter.Height := 4;
      AFormSplitter.Color := clBtnFace;
      AFormSplitter.StepSize := 7;
    end;

    H := Min(CardForm.Tag, Round(ClientHeight * 2 / 3));

    pnlCard.Align := alNone;
    pnlCard.SetBounds(0, ClientHeight, ClientWidth, H);

    CardForm.Align := alClient;
    CardForm.Show;
    DisableAlign;
    AFormSplitter.Visible := True;
    pnlCard.Visible := True;
    pnlCard.BringToFront;
    if PanelB.Visible then
      PanelB.Align := alNone;

    for i := 1 to Variables.AsInteger[RegAnim] do
    begin
      pnlCard.Top := ClientHeight - LiteResize(0, H, Variables.AsInteger[RegAnim], i, 10);
      pnlCard.Repaint;
    end;

    pnlCard.Align := alBottom;
    PanelB.Align := alBottom;
    EnableAlign;

    if Assigned(DataSetMeta.Cache) then
      MakeVisibleItem(DataSetMeta.Cache.FocusedItem);

    Repaint;
    FViewArea := True;
    if Manual then
      Variables.AsBoolean[RegShowCardArea] := True;
  end
  else
  begin
    OpenCard;
  end;
end;

function TBaseGridForm.HideViewArea(Manual: Boolean = False): Boolean;
var
  i, H: Integer;
  CardForm: TAForm;
begin
  if not FViewArea then
    Exit(False);

  CardForm := TAForm(DataSetMeta.CardForm);
  if not Assigned(CardForm) then
    Exit(False);

  Result := TAForm(CardForm).CanClose;

  if Result then
  begin
    CardForm.CheckDesignModeClosed;
    DisableAlign;
    pnlMain.Height := ClientHeight;

    H := pnlCard.ClientHeight;
    for i := 1 to Variables.AsInteger[RegAnim] do
    begin
      pnlCard.Top := ClientHeight - LiteResize(H, 0, Variables.AsInteger[RegAnim], i, 10);
      Repaint;
    end;
    pnlCard.Visible := False;

    CardForm.Hide;
    CardForm.Align := alNone;
    if Assigned(AFormSplitter) then
      AFormSplitter.Visible := False;

    EnableAlign;

    Repaint;
    FViewArea := False;
    if Manual then
      Variables.AsBoolean[RegShowCardArea] := False;
  end;
end;

procedure TBaseGridForm.SetupSort(Sender: TObject);
var
  Dlg: Tfrm_Da_Order;
begin
  Dlg := Tfrm_Da_Order.Create(Application);
  Dlg.ReInitForm(DataSetMeta);
  if Dlg.ShowModal = mrOK then
    DataSetMeta.Cache.Update(mcUpdate, null);
  Dlg.Free;
end;

procedure TBaseGridForm.DisplayMenuClick(Sender: TObject);
begin
  if (Sender is TMetaAction) and Assigned(DataSetMeta.Cache.FocusedItem) then
    if Assigned(TMetaAction(Sender).FieldMeta) and Assigned(TMetaAction(Sender).FieldMeta.LinkTable) then
      if EditRecord(TMetaAction(Sender).FieldMeta.LinkTable,
        DataSetMeta.Cache.FocusedItem.ValueByName[TMetaAction(Sender).FieldMeta.Original]) then
        DataSetMeta.Cache.Update(mcUpdate, null);
end;

procedure TBaseGridForm.CacheAllowFocusedChange(var NewItem: TCacheItem;
  var Allow: Boolean);
begin
  inherited CacheAllowFocusedChange(NewItem, Allow);
  { if Assigned(Datasetmeta.CardForm) then
    Allow := TAForm(Datasetmeta.CardForm).CanClose; }
end;

procedure TBaseGridForm.act_Da_OpenInNewWindowExecute(Sender: TObject);
// var CurGroupFields : array of TFieldMeta;
// I              : integer;
// V              : Variant;
begin
  {
    if Assigned(DataSetMeta) then
    begin
    SetLength(CurGroupFields, DataSetMeta.GroupFields.Count);
    for I := 0 to DataSetMeta.GroupFields.Count-1 do
    CurGroupFields[I] := DataSetMeta.GroupFields[I];

    if DataSetMeta.Cache.FocusedItem<>nil then
    V:=DataSetMeta.Cache.FocusedItem.ID
    else
    V:=null;

    MForm . Display Dataset(        //
    DataSetMeta.Table,
    CurGroupFields,
    V, null,
    DataSetMeta.ViewType,
    DataSetMeta.ViewParams,
    true);
    end;
    { }
end;

procedure TBaseGridForm.PanelPopupMenuPopup(Sender: TObject);
var
  ex: Boolean;
  i, j, N: Integer;
  NewMenu: TMenuItem;
begin

  PM_Dl_Directories.Clear;

  if DataSetMeta.Role = drMain then
    for i := 0 to DataSetMeta.Table.Fields.Count - 1 do
      if DataSetMeta.Table.Fields[i].IsLookup and
        SecuritySystem.CheckPolicyDataSet(DataSetMeta.Table.Fields[i].Owner.ID, spSelect) then
      begin
        ex := False;
        for j := 0 to PM_Dl_Directories.Count - 1 do
          if PM_Dl_Directories.Items[j].Tag = DataSetMeta.Table.Fields[i].Link then
            ex := True;
        if ex then
          Continue;

        NewMenu := TMenuItem.Create(PM_Dl_Directories);
        NewMenu.Tag := DataSetMeta.Table.Fields[i].Link;
        NewMenu.OnClick := act_DirectoriesExecute;
        NewMenu.Caption := GetTitle(DataSetMeta.Table.Fields[i].LookupPair.LinkTable.Name, ttSecondName);
        NewMenu.ImageIndex := DM.MapIconIndex(DataSetMeta.Table.Fields[i].LookupPair.LinkTable.Ico);

        N := 0;
        while (N < (PM_Dl_Directories.Count)) and
          (PM_Dl_Directories.Items[N].Caption < NewMenu.Caption) do
          Inc(N);

        PM_Dl_Directories.Insert(N, NewMenu);
      end;
  PM_Dl_Directories.Visible := (PM_Dl_Directories.Count > 0);
end;

procedure TBaseGridForm.act_DirectoriesExecute(Sender: TObject);
var
  aContext: TContextMeta;
begin
  if Not(Sender is TMenuItem) then
    Exit;

  aContext := TContextMeta.Create(nil);
  aContext.Dataset := MetaData.GetTableMeta(TMenuItem(Sender).Tag);
  if Assigned(aContext.Dataset) then
    MForm.DoData(nil, aContext);
  aContext.Free;
end;

procedure TBaseGridForm.actv_XXXExecute(Sender: TObject);
var
  aContext: TContextMeta;
  VariableList: TDeVariableList;
  i: Integer;
begin
  if Not(Sender is TAction) then
    Exit;

  aContext := TContextMeta.Create(nil);
  aContext.Assign(DataSetMeta.Context);
  aContext.ViewType := TDSViewType(TAction(Sender).Tag);

  VariableList := TDeVariableList.Create;
  try
    aContext.ViewParams := GetPropertiesString;
    VariableList.LoadCommaText(DataSetMeta.Context.ViewParams);
    for i := Pred(VariableList.Count) downto 0 do
      if not SameText(VariableList[i].Name, 'GroupBy') and not SameText(VariableList[i].Name, 'SortBy') then
        aContext.ViewParams := aContext.ViewParams + ' ' + VariableList[i].Name + '=' + VariableList[i].Value + ';';
  finally
    VariableList.Free;
  end;

  MForm.DoData(nil, aContext);
  aContext.Free;
end;

{
  procedure TBaseGridForm.FillCreateMenu;
  var MItem : TMenuItem;
  begin
  Item_Da_Create.Clear;
  MItem := TMenuItem.Create(Item_Da_Create);
  MItem.Action := Act_Da_Create;
  MItem.Caption := GetTitle(DataSetMeta.Table.Name);
  MItem.Default := True;
  Item_Da_Create.Add(MItem);

  if DataSetMeta.Table.IsNotes or DataSetMeta.Table.IsInterrelations then Exit;

  MItem := TMenuItem.Create(Item_Da_Create);
  MItem.Caption := cLineCaption;
  Item_Da_Create.Add(MItem);

  MItem := TMenuItem.Create(Item_Da_Create);
  MItem.Action := self.actCreate_Da_Shortcut;
  Item_Da_Create.Add(MItem);

  MItem := TMenuItem.Create(Item_Da_Create);
  MItem.Action := self.actCreate_Da_Note;
  Item_Da_Create.Add(MItem);
  end;
}

(*
  procedure TBaseGridForm.NewCaptWndProc(var Message: TMessage);
  var
  i: Integer;
  m, P: TPoint;
  begin
  case Message.Msg of
  WM_RBUTTONDOWN:
  begin
  SleepTimer.Enabled := False;
  if Assigned(MainControl) then
  MainControl.SetFocus;
  end;
  WM_LBUTTONDOWN:
  begin
  if Assigned(MainControl) then
  MainControl.SetFocus;
  SleepTimer.Interval := GetDoubleClickTime;
  m := ClientToScreen(TWMMouse(Message).Pos);
  { P:= imgIcon.ClientToScreen(Point(0,0));
  SleepTimer.Enabled := (P.X <= M.X) and (M.X <= P.X + imgIcon.Width)
  and (P.Y <= M.Y) and (M.Y <= P.Y + imgIcon.Height)
  { } end;
  WM_LBUTTONDBLCLK:
  begin
  SleepTimer.Enabled := False;
  if Assigned(MainControl) then
  MainControl.SetFocus;

  for i := 0 to PanelPopupMenu.Items.Count - 1 do
  if PanelPopupMenu.Items[i].Default then
  if PanelPopupMenu.Items[i].Enabled then
  if PanelPopupMenu.Items[i].Action <> nil then
  begin
  // мешает при переходе из грида в грид
  // pnlClient.WindowProc := OldCaptWndProc;
  // OldCaptWndProc       := nil;

  PanelPopupMenu.Items[i].Action.Execute;

  // if Assigned(pnlClient) then
  // begin
  // OldCaptWndProc := pnlClient.WindowProc;
  // pnlClient.WindowProc := NewCaptWndProc;
  // end;
  Exit;
  end;
  Exit;
  end;
  end;
  OldCaptWndProc(Message);
  end;
*)
procedure TBaseGridForm.act_Da_TablePropExecute(Sender: TObject);
var
  newTable: TTableMeta;
  BackupCursor: TCursor;
  RecordEditor: TRecordEditor;
begin
  inherited;
  BackupCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  RecordEditor := nil;
  try
    RecordEditor := TRecordEditor.Create(MetaData.MetaTables[idxDataset], DataSetMeta.Table.ID);

    if Assigned(RecordEditor.CacheItem) then
    begin
      RecordEditor.Caption := StripHotKey((Sender as TAction).Caption);
      Screen.Cursor := BackupCursor;
      if RecordEditor.Execute then
      begin
        newTable := nil;
        try
          newTable := TTableMeta.Create;
          newTable.Assign(RecordEditor.CacheItem);
        finally
          newTable.Free;
        end;
      end;
    end;
  finally
    if Assigned(RecordEditor) then
      RecordEditor.Free;
  end;
  Screen.Cursor := BackupCursor;
end;

procedure TBaseGridForm.act_Da_OpenInMainExecute(Sender: TObject);
var
  i: Integer;
  aContext: TContextMeta;
begin
  inherited;

  if Assigned(DataSetMeta) then
  begin
    aContext := TContextMeta.Create(nil);
    aContext.Assign(DataSetMeta.Context);
    aContext.Groups.Clear;

    case DataSetMeta.Role of
      drParent:
        begin
          aContext.SelectedKeys := DataSetMeta.GetKeyValue(gkmFocused);
          MForm.DoData(nil, aContext);
        end;
      drChild:
        begin
          for i := 0 to Pred(DataSetMeta.OwnerLinks.Count) do
            if Assigned(DataSetMeta.OwnerLinks[i].LinkField) then
              aContext.AddGroupField(DataSetMeta.OwnerLinks[i].LinkField, DataSetMeta.OwnerLinks[i].DataSetMeta.GetKeyValue);
          MForm.DoData(nil, aContext);
        end;
    end;

    aContext.Free;
  end;
end;

// ------------------------------------------------------------------------------

procedure TBaseGridForm.act_Da_FilterExecute(Sender: TObject);
begin
  if act_Da_Filter.Checked then
    HideFilterClick(Sender)
  else
    ShowFilterClick(Sender);
end;

procedure TBaseGridForm.NewFilterSize;
var
  i, OldSize, NewSize: Integer;
begin
  OldSize := pnlFilter.Height;

  pnlFilterBase.OnResize := nil;

  if act_Da_Filter.Checked then
  begin
    NewSize := FFilterControl.NeedHeight;
    pnlFilterBase.Height := NewSize;
    Inc(NewSize, 2);

    if pnlMainR.Width < 400 then
    begin
      Bevel1.Visible := False;
      pnlFilterTop.Height := 24;
      pnlFilterR.Parent := nil;//pnlFilterTop;
    end
    else
    begin
      Bevel1.Visible := True;
      pnlFilterTop.Height := 0;
      pnlFilterR.Parent := nil;//pnlFilterBase;
    end;

    Inc(NewSize, pnlFilterTop.Height);
    pnlFilter.Visible := True;
  end
  else
  begin
    NewSize := 0;
  end;

  if NewSize <> OldSize then
  begin
    if pnlFilter.Visible then
      for i := 0 to Variables.AsInteger[RegAnim] do
      begin
        pnlFilter.Height := LiteResize(OldSize, NewSize,
          Variables.AsInteger[RegAnim], i, 10);
        pnlFilter.Invalidate;
        pnlGrid.Invalidate
      end
    else
    begin
      pnlFilter.Height := NewSize;
      pnlGrid.Invalidate
    end;
  end;

  if NewSize > 0 then
    pnlFilterBase.OnResize := pnlFilterBaseResize
  else
  begin
    if Visible and Assigned(MainControl) and MainControl.Visible then
      MainControl.SetFocus;
    pnlFilter.Visible := False;
  end
end;

procedure TBaseGridForm.WMFilterChange(var Message: TMessage);
var
  DownButton: Boolean;
  aFilterItem: TFilterItem;
begin
  if not(Message.WParam = DM_pnCONDITION) then
    NewFilterSize;

  aFilterItem := TFilterItem.Create;
  FFilterControl.WriteFilterPostfixTo(aFilterItem);

  if Not Assigned(DataSetMeta) then
    begin
      DownButton := False;

      sbFilter.Enabled := False;
      sbFilter.Down := False;
      sbFilter.OnClick := nil;
      sbFilter.Tag := -1;
    end else
  if Not act_Da_Filter.Checked then
    begin
      if (DataSetMeta.UserFilterPostfix.Count > 0) then
        FilterClear(sbFilter);
    end else
  if (aFilterItem.Count = 0) // фильтр в контролах пуст
  then
    begin
      DownButton := False;

      sbFilter.Enabled := False;
      sbFilter.Down := False;
      sbFilter.OnClick := nil;
      sbFilter.Tag := 0;
    end else
  if (DataSetMeta.UserFilterPostfix.Count = 0)
  then
    begin
      DownButton := True;

      sbFilter.Enabled := False;
      sbFilter.Down := True;
      sbFilter.OnClick := FilterClear;
      sbFilter.Tag := 1;
      if True then

      FilterDo(sbFilter);
    end else
    begin
      DownButton := True;

      sbFilter.Enabled := False;
      sbFilter.Down := True;
      sbFilter.OnClick := FilterClear;
      sbFilter.Tag := 2;
      if not DataSetMeta.UserFilterPostfix.IdenticalTo(aFilterItem) then
        FilterDo(sbFilter);
    end; { else

      begin
        DownButton := DataSetMeta.UserFilterPostfix.IdenticalTo(aFilterItem);

        if (sbFilter.Down) then // фильтр включен, сразу перезапрашиваем
          begin
            if DownButton then
              begin
                sbFilter.Enabled := True;
                sbFilter.Down := False;
                sbFilter.OnClick := FilterDo;
              end
            else
              FilterDo(sbFilter);
          end
        else
          begin
            sbFilter.Enabled := True;
            sbFilter.Down := DownButton;
            if DownButton then
              sbFilter.OnClick := FilterClear
            else
              sbFilter.OnClick := FilterDo;
          end;
      end
  else
  begin

  end;
   {}

  sbFindPrev.Enabled := sbFilter.Enabled and (not DownButton) and CanFind;
  sbFindNext.Enabled := sbFilter.Enabled and (not DownButton) and CanFind;

  act_Da_FilterApply.Enabled := sbFilter.Enabled;
  act_Da_FilterApply.Checked := sbFilter.Down;
  act_Da_FindPrev.Enabled := sbFindPrev.Enabled;
  act_Da_FindNext.Enabled := sbFindNext.Enabled;

  act_Da_FilterOn.Visible := not act_Da_FilterApply.Checked;
  act_Da_FilterOff.Visible := act_Da_FilterApply.Checked;
  aFilterItem.Free;
end;

procedure TBaseGridForm.pnlFilterBaseResize(Sender: TObject);
begin
  inherited;
  NewFilterSize;
end;

procedure TBaseGridForm.ShowFilterClick(Sender: TObject);
begin
  PanelFilterMenu.Width:= 16;
  act_Da_Filter.Checked := True;
  NewFilterSize;
  FFilterControl.Start;
  PostMessage(self.Handle, DM_FILTERCHANGE, DM_pnCONDITION, 0);
end;

procedure TBaseGridForm.HideFilterClick(Sender: TObject);
begin
  act_Da_Filter.Checked := False;
  NewFilterSize;
  FilterClear(Sender);
end;

procedure TBaseGridForm.FilterDo(Sender: TObject);
var N: Integer;
    sXML: String;
begin
  if Not Assigned(DataSetMeta) then Exit;
  if Not Assigned(DataSetMeta.Table) then Exit;

  DataSetMeta.UserFilterPostfix.Clear;
  FFilterControl.WriteFilterPostfixTo(DataSetMeta.UserFilterPostfix);
  DataSetMeta.Cache.Update(mcUpdate, null, ltFirst);

  FFilterControl.WriteFilterToXML(sXML);
  if DataSetMeta.Table.UserValues.GetFirstIndex(uvFilterLast, N)
    then DataSetMeta.Table.UserValues[N].XML:= sXML
    else DataSetMeta.Table.UserValues.New(uvFilterLast, DataSetMeta.Table.ID, UserSession.ID, FFilterControl.FilterAsString, sXML);

  PostMessage(self.Handle, DM_FILTERCHANGE, DM_pnCONDITION, 0);
end;

procedure TBaseGridForm.FilterMenu(Sender: TObject);
var
  P: TPoint;
begin
  with TWinControl(Sender) do
    P := ClientToScreen(Point(0, BoundsRect.Bottom));

  FilterPopupMenu.Popup(P.X, P.Y);
  inherited;
end;

procedure TBaseGridForm.FilterClear(Sender: TObject);
begin
  if DataSetMeta.UserFilterPostfix.Count > 0 then
  begin
    DataSetMeta.UserFilterPostfix.Clear;
    DataSetMeta.Cache.Update(mcUpdate, null);
  end;

  PostMessage(self.Handle, DM_FILTERCHANGE, DM_pnCONDITION, 0);
end;

procedure TBaseGridForm.sbFindPrevClick(Sender: TObject);
var
  aFilterItem: TFilterItem;
  ii, i, N, C: Integer;
  ok: Boolean;
begin
  ok := False;
  aFilterItem := TFilterItem.Create;
  FFilterControl.WriteFilterPostfixTo(aFilterItem);

  N := DataSetMeta.Cache.FocusedIndex;
  C := DataSetMeta.Cache.Count - 1;
  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, C, 1);
  try
    for ii := Pred(DataSetMeta.Cache.Count) downto 0 do
    begin
      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, C - ii, 0);
      i := (ii + N) mod DataSetMeta.Cache.Count;

      if (isNotInitialized in DataSetMeta.Cache.Items[i].State) then
        DataSetMeta.Cache.Items[i].FillItem;

      if DataSetMeta.Cache.Items[i].Calculate(aFilterItem, False) then
      begin
        DataSetMeta.Cache.ClearSelection;

        DataSetMeta.Cache.Items[i].SetFocus;
        DataSetMeta.Cache.Items[i].Selected := True;
        CacheSelectedChange(DataSetMeta.Cache.Items[i]);
        ChangeDataSetStatus;

        MainControl.SetFocus;
        ok := True;
        Break;
      end;
    end;
  finally
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  end;

  aFilterItem.Free;
  TWinControl(Sender).Repaint;
  if ok = False then
    ShowHintWindow(sbFindPrev, '_eRror.find', '_Dl.information', icoInfo);
end;

procedure TBaseGridForm.sbFindNextClick(Sender: TObject);
var
  aFilterItem: TFilterItem;
  ii, i, N: Integer;
  ok: Boolean;
begin
  ok := False;
  aFilterItem := TFilterItem.Create;
  FFilterControl.WriteFilterPostfixTo(aFilterItem);

  N := DataSetMeta.Cache.FocusedIndex;
  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, DataSetMeta.Cache.Count - 1, 1);
  try
    for ii := 1 to DataSetMeta.Cache.Count do
    begin
      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, ii - 1, 0);
      i := (ii + N) mod DataSetMeta.Cache.Count;

      if (isNotInitialized in DataSetMeta.Cache.Items[i].State) then
        DataSetMeta.Cache.Items[i].FillItem;

      if DataSetMeta.Cache.Items[i].Calculate(aFilterItem, False) then
      begin
        DataSetMeta.Cache.ClearSelection;

        DataSetMeta.Cache.Items[i].SetFocus;
        DataSetMeta.Cache.Items[i].Selected := True;
        CacheSelectedChange(DataSetMeta.Cache.Items[i]);
        ChangeDataSetStatus;

        ok := True;
        MainControl.SetFocus;
        Break;
      end;
    end;
  finally
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  end;

  aFilterItem.Free;
  TWinControl(Sender).Repaint;
  if ok = False then
    ShowHintWindow(sbFindNext, '_eRror.find', '_Dl.information', icoInfo);
end;

// ------------------------------------------------------------------------------

function TBaseGridForm.GetPropertiesString: string;
var
  i: Integer;
  FM: TFieldMeta;
begin
  Result := EmptyStr;
  for i := 0 to Pred(DataSetMeta.Cache.SortList.Count) do
  begin
    if Length(Result) <> 0 then
      Result := Result + '|';
    FM := DataSetMeta.Table.Fields.FindByID
      (DataSetMeta.Cache.SortList[i].FieldID);
    if FM.IsLookup then
      Result := Result + FM.LookupPair.Original
    else
      Result := Result + FM.Original;
    case DataSetMeta.Cache.SortList[i].Direction of
      sdAscending: Result := Result + '+';
      sdDescending: Result := Result + '-';
    end;
  end;
  if Length(Result) <> 0 then
    Result := ' SortBy=' + Result + ';';

  if DataSetMeta.GroupFields.Count > 0 then
  begin
    Result := Result + ' GroupBy=';
    for i := 0 to Pred(DataSetMeta.GroupFields.Count) do
    begin
      if i > 0 then
        Result := Result + '|';
      Result := Result + DataSetMeta.GroupFields[i].Original;
    end;
    Result := Result + ';';
  end;
end;

procedure TBaseGridForm.DoUpdate(Sender: TObject);
begin
  StopRefreshTimer;
  // Если Sender=nil то главная форма сама обновляет ВСЕ открытые наборы данных
  if Assigned(Sender) then
    MetaData.UpdateLibrary;

  if Assigned(DataSetMeta) then
    DataSetMeta.Cache.Update(mcUpdate, null);
  StartRefreshTimer;
end;

procedure TBaseGridForm.act_Da_FastPrintExecute(Sender: TObject);
var
  aStyle: TDePrintStyle;
  Dlg: TDePrintDialog2;
  S: string;
begin
  Dlg := TDePrintDialog2.Create(Application);
  try
    S := GetTitle(DataSetMeta.Table.Name, ttSecondName);
    LangFormUpdate(Dlg, '_Da.Print "' + S + '"');
    Dlg.Styles := PrintStyles;
    Dlg.EnabledStyles := getEnabledPrintStyles;
    Dlg.onPreview := onPrintCardPreview;
    if Dlg.Execute then
    begin
      aStyle := Dlg.Styles[Dlg.StyleName];
      doPrintReport(aStyle, Dlg.PrinterName);
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TBaseGridForm.act_dA_FavoritesExecute(Sender: TObject);
var
  DM: TDataManager;
  Cache: TDataCache;
  CI: TCacheItem;
  MMM: TContextMeta;
  a, B, C, MenuCaption: string;
  N: Integer;
begin
  MenuCaption := DataSetMeta.Table.Name;
  if DlgRename.EditName('_dA.Favorites', '_dF.name:', MenuCaption) then
    try
      DM := TDataManager.Create;
      DM.Table := MetaData.GetSystemTableByName(tblMenu);
      Cache := TDataCache.Create(MetaData.GetSystemTableByName(tblMenu));

      // объединение базового и пользовательского фильтров

      a := DataSetMeta.Filter;
      if DataSetMeta.UserFilterPostfix.Count > 0 then
        B := FFilterControl.FilterAsString
      else
        B := EmptyStr;

      if (Length(Trim(a)) = 0) then
        C := B
      else if (Length(Trim(B)) = 0) then
        C := a
      else
        C := '( ' + a + ' ) and ( ' + B + ' )';

      CI := Cache.AddNewItem;
      CI.ValueByName[fldMenuOwner] := 1;
      CI.ValueByName[fldMenuIco] := DataSetMeta.Ico;
      CI.ValueNativeByName[fldMenuName] := MenuCaption;
      // DataSetMeta.Table.Name;
      CI.ValueByName[fldMenuDataSet] := DataSetMeta.Table.ID;
      CI.ValueByName[fldMenuType] := 2; // Группа
      CI.ValueByName[fldMenuDeleted] := 0;
      CI.ValueByName[fldMenuDataSetView] := DataSetMeta.Context.ViewType;
      CI.ValueNativeByName[fldMenuDataFilter] := C;
      CI.ValueNativeByName[fldMenuDataSetPars] := GetPropertiesString;
      CI.ValueByName[fldMenuOrder] := 1;
      N := TCacheItem(CI).Owner.Fields.IndexByName(fldMenuSubjectID);
      if -1 < N then
        CI.FieldValue[N] := UserSession.ID;

      DM.InsertRecord(CI);

      MMM := TContextMeta.Create(nil);
      MMM.Assign(CI);
      SendMessage(Application.MainForm.Handle, DM_FAVORITES, NativeInt(MMM), 0);

      Cache.Free;
      DM.Free;
    except
{$IFDEF DEBUG}
      on E: Exception do
        DebugLog('TBaseGridForm.act_dA_FavoritesExecute error: ' + E.Message);
{$ENDIF}
    end;
end;

procedure TBaseGridForm.pnlGridEnter(Sender: TObject);
begin
  inherited;

  act_Da_Cut.ShortCut := ShortCut(Word('X'), [ssCtrl]);
  act_Da_Copy.ShortCut := ShortCut(Word('C'), [ssCtrl]);
  act_Da_Paste.ShortCut := ShortCut(Word('V'), [ssCtrl]);
  act_Da_Undo.ShortCut := ShortCut(Word('Z'), [ssCtrl]);
  act_Da_SelectAll.ShortCut := ShortCut(Word('A'), [ssCtrl]);
end;

procedure TBaseGridForm.pnlGridExit(Sender: TObject);
begin
  inherited;

  act_Da_Cut.ShortCut := ShortCut(Word('X'), [ssCtrl, ssShift]);
  act_Da_Copy.ShortCut := ShortCut(Word('C'), [ssCtrl, ssShift]);
  act_Da_Paste.ShortCut := ShortCut(Word('V'), [ssCtrl, ssShift]);
  act_Da_Undo.ShortCut := ShortCut(Word('Z'), [ssCtrl, ssShift]);
  act_Da_SelectAll.ShortCut := ShortCut(Word('A'), [ssCtrl, ssShift]);
end;

procedure TBaseGridForm.PopupMenuPopup(Sender: TObject);
var
  i, j: Integer;
  V: Variant;
begin
  inherited;
  if Assigned(DataSetMeta) then
    DataSetMeta.UpdateActions;

  Item_Da_Show.Visible := Item_Da_Show.Count > 0;
  Item_Da_Show.Enabled := Assigned(DataSetMeta.Cache.FocusedItem);

  if Item_Da_Show.Enabled then
    for i := Pred(TempActionList.ActionCount) downto 0 do
      if TempActionList.Actions[i].Category = CategoryDisplay then
        if Assigned(TMetaAction(TempActionList.Actions[i]).FieldMeta) then
        begin
          V := DataSetMeta.Cache.FocusedItem.ValueByName[TMetaAction(TempActionList.Actions[i]).FieldMeta.Original];
          TempActionList.Actions[i].Visible :=
            Not Assigned(DataSetMeta.GroupFields.FindByID(TMetaAction(TempActionList.Actions[i]).FieldMeta.ID));
          TempActionList.Actions[i].Enabled := (not VarIsEmpty(V)) and (not VarIsNull(V));

          if TempActionList.Actions[i].Visible and TempActionList.Actions[i].Enabled
          then
            for j := 0 to Pred(MetaData.LibraryCount) do
              if MetaData.LibraryData[j].TableMeta.ID = TMetaAction
                (TempActionList.Actions[i]).FieldMeta.Link then
              begin
                TempActionList.Actions[i].Enabled := (-1 < MetaData.LibraryData[j].IndexByID(V));
                Break;
              end;
        end;

  for i := Pred(TempActionList.ActionCount) downto 0 do
    if TempActionList.Actions[i].Category = CategorySort then
      if Assigned(TMetaAction(TempActionList.Actions[i]).FieldMeta) then
      begin
        if DataSetMeta.Cache.SortList.Count = 0 then
          TempActionList.Actions[i].Checked := False
        else
          TempActionList.Actions[i].Checked :=
            (DataSetMeta.Cache.SortList[0].FieldID = TMetaAction(TempActionList.Actions[i]).FieldMeta.ID);
      end;
end;

procedure TBaseGridForm.act_Da_FindNextExecute(Sender: TObject);
begin
  sbFindNextClick(sbFindNext);
end;

procedure TBaseGridForm.act_Da_FindPrevExecute(Sender: TObject);
begin
  sbFindPrevClick(sbFindPrev);
end;

procedure TBaseGridForm.act_Da_FilterApplyExecute(Sender: TObject);
begin
  if Assigned(sbFilter.OnClick) then
    sbFilter.OnClick(sbFilter);
end;

procedure TBaseGridForm.SelectFilterClick(Sender: TObject);
var Index: Integer;
begin
  if Assigned(Sender) and (Sender is TMenuItem) then
    if Assigned(DataSetMeta) and Assigned(DataSetMeta.Table) then
      begin
        if DataSetMeta.Table.UserValues.GetIndexByID((Sender as TMenuItem).Tag, Index) then
          begin
            FilterControl.ReadFilterFromXML(DataSetMeta.Table.UserValues[Index].XML);
            ShowFilterClick(Sender);
            FilterDo(Sender);
          end;
      end;
end;

procedure TBaseGridForm.act_Da_FilterOnExecute(Sender: TObject);
begin
  if Assigned(DataSetMeta) and Assigned(DataSetMeta.Table) then
    begin
      ShowFilterClick(Sender);
      FilterDo(Sender);
    end;
end;

procedure TBaseGridForm.act_Da_FilterOffExecute(Sender: TObject);
begin
  if Assigned(DataSetMeta) and Assigned(DataSetMeta.Table) then
    FilterClear(Sender);
end;

procedure TBaseGridForm.mi_dA_FilterSaveClick(Sender: TObject);
var Index: Integer;
    FilterName, sXML: string;
begin
  if Assigned(DataSetMeta) and Assigned(DataSetMeta.Table) then
    begin
      FilterName:= FFilterControl.GenerateName;

      if DlgRename.EditName((Sender as TMenuItem).Caption, '_dA.FilterName:', FilterName) then
        begin
          FFilterControl.WriteFilterToXML(sXML);
          Index:= DataSetMeta.Table.UserValues.New(uvFilterPersonal, DataSetMeta.Table.ID, UserSession.ID, FilterName, sXML);
          DataSetMeta.Table.UserValues.Commit;
        end;
    end;
end;

procedure TBaseGridForm.RenameFilterClick(Sender: TObject);
var Index: Integer;
    FName: String;
begin
  if Assigned(Sender) and (Sender is TMenuItem) then
    if Assigned(DataSetMeta) and Assigned(DataSetMeta.Table) then
      begin
        if DataSetMeta.Table.UserValues.GetIndexByID((Sender as TMenuItem).Tag, Index) then
          begin
            FName:= DataSetMeta.Table.UserValues[Index].Name;
            if DlgRename.EditName((Sender as TMenuItem).Caption, '_dA.FilterName:', FName) then
              DataSetMeta.Table.UserValues[Index].Name:= FName;
            DataSetMeta.Table.UserValues.Commit;
          end;
      end;
end;

procedure TBaseGridForm.DeleteFilterClick(Sender: TObject);
var Index: Integer;
begin
  if Assigned(Sender) and (Sender is TMenuItem) then
    if Assigned(DataSetMeta) and Assigned(DataSetMeta.Table) then
      if DataSetMeta.Table.UserValues.GetIndexByID((Sender as TMenuItem).Tag, Index) then
        if Application.MessageBox( pChar(GetTitle('_dA.deletefilter')+' "'+DataSetMeta.Table.UserValues[Index].Name+'"'),
                                   pChar(GetTitle('_dL.confirmation')),
                                   MB_ICONQUESTION or MB_YESNO ) = idYes then
          begin
            DataSetMeta.Table.UserValues[Index].Status:= [mcDelete];
            DataSetMeta.Table.UserValues.Commit;
          end;
end;

procedure TBaseGridForm.FilterAllUsersClick(Sender: TObject);
var Index: Integer;
begin
  if Assigned(Sender) and (Sender is TMenuItem) then
    if Assigned(DataSetMeta) and Assigned(DataSetMeta.Table) then
      begin
        if DataSetMeta.Table.UserValues.GetIndexByID((Sender as TMenuItem).Tag, Index) then
          begin
            DataSetMeta.Table.UserValues[Index].TypeValue:= uvFilterCommon;
            DataSetMeta.Table.UserValues.Commit;
          end;
      end;
end;

procedure TBaseGridForm.FilterOneUserClick(Sender: TObject);
var Index: Integer;
begin
  if Assigned(Sender) and (Sender is TMenuItem) then
    if Assigned(DataSetMeta) and Assigned(DataSetMeta.Table) then
      begin
        if DataSetMeta.Table.UserValues.GetIndexByID((Sender as TMenuItem).Tag, Index) then
          begin
            DataSetMeta.Table.UserValues[Index].Subject:= UserSession.ID;
            DataSetMeta.Table.UserValues[Index].TypeValue:= uvFilterPersonal;
            DataSetMeta.Table.UserValues.Commit;
          end;
      end;
end;

procedure TBaseGridForm.FilterPopupMenuPopup(Sender: TObject);
var
  i, PosIndex, FCurrentID: Integer;
  sFilter: String;
  aMenu: TPopUpMenu;
  UserItem: TMenuItemDinamic;

  procedure FillMenu(aFilterType: Integer);
  var Index: Integer;
  begin
    Index:= -1;
    while DataSetMeta.Table.UserValues.GetNextIndex(aFilterType, Index) do
      begin
        UserItem := TMenuItemDinamic.Create(aMenu);
        UserItem.Caption:= DataSetMeta.Table.UserValues[Index].Name;
        UserItem.Tag:= DataSetMeta.Table.UserValues[Index].ID;
        aMenu.Items.Insert(PosIndex, UserItem);

        // сравниваем по простому - строковое выражение фильтра
        if (DataSetMeta.Table.UserValues[Index].XML = sFilter) then
          FCurrentID:= UserItem.Tag;

        if UserItem.Tag = FCurrentID then
          begin
            if Not act_Da_FilterApply.Checked then
              with TMenuItem.Create(UserItem) do
                begin
                  Action:= act_da_filterOn;
                  UserItem.Add(This);
                end;

            if act_Da_FilterApply.Checked then
              with TMenuItem.Create(UserItem) do
                begin
                  Action:= act_da_filterOff;
                  UserItem.Add(This);
                end;

            with TMenuItem.Create(UserItem) do
              begin
                Caption:= '-';
                UserItem.Add(This);
              end;

            if UserSession.IsAdmin or (DataSetMeta.Table.UserValues[Index].Subject = UserSession.ID) then
            begin
              with TMenuItem.Create(UserItem) do
                begin
                  Caption:= GetTitle('_dA.Rename');
                  OnClick:= RenameFilterClick;
                  Tag:= UserItem.Tag;
                  UserItem.Add(This);
                end;

              with TMenuItem.Create(UserItem) do
                begin
                  Caption:= GetTitle('_dA.deletefilter');
                  OnClick:= DeleteFilterClick;
                  Tag:= UserItem.Tag;
                  UserItem.Add(This);
                end;
            end;

            if UserSession.IsAdmin then
              begin
                with TMenuItem.Create(UserItem) do
                  begin
                    Caption:= '-';
                    UserItem.Add(This);
                  end;

                with TMenuItem.Create(UserItem) do
                  begin
                    Caption:= GetTitle('_dA.filteroneuser');
                    OnClick:= FilterOneUserClick;
                    Tag:= UserItem.Tag;
                    RadioItem:= True;
                    Checked:= (DataSetMeta.Table.UserValues[Index].TypeValue = uvFilterPersonal);
                    UserItem.Add(This);
                  end;

                with TMenuItem.Create(UserItem) do
                  begin
                    Caption:= GetTitle('_dA.filterallusers');
                    OnClick:= FilterAllUsersClick;
                    Tag:= UserItem.Tag;
                    RadioItem:= True;
                    Checked:= (DataSetMeta.Table.UserValues[Index].TypeValue = uvFilterCommon);
                    UserItem.Add(This);
                  end;
              end;

            UserItem.ImageIndex:= DM.MapIconIndex( iif(act_Da_FilterApply.Checked, 76, 77 ) );
          end
        else
          UserItem.OnClick:= SelectFilterClick;

        Inc(PosIndex);
      end;
  end;

begin
  inherited;

  if not Assigned(DataSetMeta) then Exit;
  if not Assigned(DataSetMeta.Table) then Exit;
  if not Assigned(UserSession) then Exit;
  if not (Sender is TPopupMenu) then Exit;
  aMenu:= TPopupMenu(Sender);

  // удаляем старые и запоминаем положение разделителя после группы
  for I := Pred(aMenu.Items.Count) downto 0 do
    if aMenu.Items[i].ClassType = TMenuItemDinamic then aMenu.Items.Delete(i);

  if act_Da_Filter.Checked then FilterControl.WriteFilterToXML(sFilter)
                           else sFilter:= EmptyStr;

  FCurrentID:= -1;

  PosIndex:= aMenu.Items.IndexOf(miFilterLineSelectEnd);
  FillMenu(uvFilterPersonal);

  PosIndex:= aMenu.Items.IndexOf(miFilterLineSelectEnd) + 1;
  FillMenu(uvFilterCommon);

  // Текущий/пустой фильтр
  mI_Da_FilterNo.Visible:= (FCurrentID = -1) and (0 = Length(sFilter));
  mI_Da_FilterNo.ImageIndex:= DM.MapIconIndex( 116 );

  mI_Da_FilterUser.Visible:= (FCurrentID = -1) and (0 < Length(sFilter));
  mI_Da_FilterUser.ImageIndex:= DM.MapIconIndex( iif(act_Da_FilterApply.Checked, 76, 77 ) );
end;

procedure TBaseGridForm.FillLocalFilters;
var
  Index: Integer;
  Action: TAction;
  MenuItem: TMenuItem;
begin
{$IFDEF DEBUG}
  DebugLog(ClassName + '.FillLocalFilters start ...');
{$ENDIF}
  while LocalFilterActionList.ActionCount <> 0 do
    LocalFilterActionList[0].Free;
  mi_Da_Filter.Clear;
  if Assigned(GroupFilters) and Assigned(DataSetMeta) and
    Assigned(DataSetMeta.Table) then
    for Index := 0 to Pred(GroupFilters.Count) do
      if GroupFilters[Index].IsLocal(DataSetMeta.Table.ID) then
      begin
        Action := TAction.Create(LocalFilterActionList);
        try
          Action.Caption := GetTitle(GroupFilters.Names[Index], ttFirstName);
          Action.Hint := GetTitle(GroupFilters.Names[Index], ttSecondName);
          Action.Tag := Index;
          Action.Checked := GroupFilters[Index].Active;
          Action.OnExecute := LocalFilterExecute;
          Action.ActionList := LocalFilterActionList;
          MenuItem := TMenuItem.Create(mi_Da_Filter);
          try
            MenuItem.Action := Action;
            mi_Da_Filter.Add(MenuItem);
          except
            MenuItem.Free;
            raise;
          end;
        except
          Action.Free;
          raise;
        end;
      end;
  if mi_Da_Filter.Count = 0 then
  begin
    MenuItem := TMenuItem.Create(mi_Da_Filter);
    try
      MenuItem.Caption := act_dA_emptylist.Caption;
      MenuItem.Enabled := False;
      mi_Da_Filter.Add(MenuItem);
    except
      MenuItem.Free;
      raise;
    end;
  end;
{$IFDEF DEBUG}
  DebugLog(ClassName + '.FillLocalFilters finish ...');
{$ENDIF}
end;

procedure TBaseGridForm.LocalFilterExecute(Sender: TObject);
begin
  if Assigned(DataSetMeta) and Assigned(DataSetMeta.Table) and
    Assigned(DataSetMeta.Cache) and Assigned(GroupFilters) then
    with Sender as TAction do
    begin
      Checked := not Checked;
      GroupFilters[Tag].Active := Checked;
      DataSetMeta.Cache.Update(mcUpdate, null);
    end;
end;

procedure TBaseGridForm.ActionEnabled(const aCategory: String; aValue: Boolean);
var
  i: Integer;
begin
  for i := 0 to Pred(TempActionList.ActionCount) do
    if SameText(TempActionList.Actions[i].Category, aCategory) then
      TempActionList.Actions[i].Enabled := aValue;
end;

procedure TBaseGridForm.ActionVisible(const aCategory: String; aValue: Boolean);
var
  i: Integer;
begin
  for i := 0 to Pred(TempActionList.ActionCount) do
    if SameText(TempActionList.Actions[i].Category, aCategory) then
      TempActionList.Actions[i].Enabled := aValue;
end;

procedure TBaseGridForm.ActionChecked(const aCategory: String; AFieldMeta: TFieldMeta);
var
  i: Integer;
begin
  for i := 0 to Pred(TempActionList.ActionCount) do
    if SameText(TempActionList.Actions[i].Category, aCategory) and
      (TempActionList.Actions[i] is TMetaAction) then
      TempActionList.Actions[i].Checked := (TMetaAction(TempActionList.Actions[i]).FieldMeta = AFieldMeta);
end;

procedure TBaseGridForm.SetDataSetMeta(const Value: TDataSetMeta);
begin
  if Assigned(Value) and Assigned(Value.Cache) and Assigned(Value.Table) then
    Value.Cache.IncludeDependedFields;

  if FDataSetMeta = Value then
    Exit;

  if Assigned(FDataSetMeta) then
    FDataSetMeta.GridForm := nil;

  FDataSetMeta := Value;

  if Assigned(FDataSetMeta) then
  begin
    FDataSetMeta.GridForm := self;

    Icon.SetSize(DM.ilIcon16.Width, DM.ilIcon16.Height);
    DM.ilIcon16.GetIcon(DM.MapIconIndex(FDataSetMeta.Ico), Icon);

    if Assigned(FDataSetMeta.Table.OField) then
      FIndexDField := Value.Cache.Fields.IndexByID(FDataSetMeta.Table.OField.ID)
    else
      FIndexDField := -1;
    if -1 < FIndexDField then
      FValueDField := VarToType(FDataSetMeta.Table.OField.Value2, FDataSetMeta.Table.OField.DataType)
    else
      FValueDField := unassigned;

    Act_Da_Default.Visible := (FDataSetMeta.Table.OField <> nil);
  end;
end;

procedure TBaseGridForm.SetFormCaption(const Value: String);
begin
  FFormCaption := Value;
  Caption := Value;
  GradientPaint.Repaint;
end;

procedure TBaseGridForm.SetThumbNailSize(const ValueW: Integer;
  ValueH: Integer = 0);
begin
  FThumbNailSizeW := ValueW;
  if ValueH = 0 then
    FThumbNailSizeH := ValueW
  else
    FThumbNailSizeH := ValueH;
  Thumbnail.Clear;
  if (0 < FThumbNailSizeW) and (0 < FThumbNailSizeH) then
    Thumbnail.SetSize(FThumbNailSizeW, FThumbNailSizeH);
end;

procedure TBaseGridForm.PrepareThumbNail(const aIndex: Integer);
var
  i: Integer;
  k: Double;
  BM, BMR: TBitmap;
  Icon: TIcon;
  XtractImage: IExtractImage;
  RT: IRunnableTask;
  Flags: DWORD;
  FileName: String;

  aValue: Variant;
  P: Pointer;
  W,H,BlockSize: Integer;
  AGraphicClass: TGraphicClass;
  FGraphic: TGraphic;
  AFileData:TByteDynArray;
  Stream: TStream;

  function CheckSignature(const Signature: TByteDynArray): Boolean;
  begin
    Result := Length(AFileData) > Length(Signature);
    if Result then
      Result := CompareMem(@AFileData[0], @Signature[0], Length(Signature));
  end;
begin
  for i := Thumbnail.Count to aIndex do
    try
      BM := nil;

      if Assigned(DataSetMeta.Table.PreviewField) then
        begin
          if DataSetMeta.Table.PreviewField.DataType in StringTypes then
          try
            FileName := DataSetMeta.Cache.Items[i].ValueByName[DataSetMeta.Table.PreviewField.Original];
            Flags := IEIFLAG_SCREEN or IEIFLAG_QUALITY;
            if FileExists(FileName) then
              if GetExtractImageItfPtr(FileName, XtractImage) then
                ExtractImageGetFileThumbnail(XtractImage, Thumbnail.Width, Thumbnail.Height, 32, Flags, RT, BM)
              else
              begin
                BM := TBitmap.Create;
                BM.PixelFormat := pf32Bit;
                BM.SetSize(Thumbnail.Width, Thumbnail.Height);
                Icon := TIcon.Create;
                Icon.Handle := GetRegistryIconHandle(FileName);
                BM.Assign(Icon);
                Icon.Free;
              end;
          except
            BM := nil;
          end;

          if DataSetMeta.Table.PreviewField.DataType in BinaryTypes then
          try
            FileName := '';//DataSetMeta.Cache.Items[i].ValueByName[DataSetMeta.Table.PreviewField.Original];
            aValue:= DataSetMeta.Cache.Items[i].ValueByName[DataSetMeta.Table.PreviewField.Original];
            BlockSize := VarArrayHighBound(aValue, 1) - VarArrayLowBound(aValue, 1);

            SetLength(AFileData, BlockSize);
            P := VarArrayLock(aValue);
            try
              Move(P^, AFileData[0], BlockSize);
            finally
              VarArrayUnlock(aValue);
            end;

            if CheckSignature([$47, $49, $46]) then AGraphicClass:= TGIFImage else
            if CheckSignature([$FF, $D8, $FF]) then AGraphicClass:= TJPEGImage else
            if CheckSignature([$89, $50, $4E, $47]) then AGraphicClass:= TPngImage else
            if CheckSignature([$42, $4D]) then AGraphicClass:= VCL.Graphics.TBitmap else
            if CheckSignature([$49, $49, $2A]) then AGraphicClass:= TWICImage { else
            if SameText(ExtractFileExt(FFileName), sExtensionEMF) or SameText(ExtractFileExt(FFileName), sExtensionWMF)
              then AGraphicClass:= TMetafile {}
              else AGraphicClass:= nil;

            if Assigned(AGraphicClass) then
              begin
                FGraphic := AGraphicClass.Create;
                try
                  Stream := TMemoryStream.Create;
                  try
                    Stream.Write(AFileData[0], Length(AFileData));
                    Stream.Position := 0;
                    FGraphic.LoadFromStream(Stream);
                  finally
                    Stream.Free;
                  end;

                  BM := TBitmap.Create;
                  BM.PixelFormat := pf24Bit;
                  BM.SetSize(Thumbnail.Width, Thumbnail.Height);
                  if (FGraphic.Width > Thumbnail.Width) or (FGraphic.Height > Thumbnail.Height) then
                    begin
                      k:= max(FGraphic.Width / Thumbnail.Width, FGraphic.Height / Thumbnail.Height);
                      W:= trunc(FGraphic.Width / k);
                      H:= trunc(FGraphic.Height / k);
                      BM.Canvas.StretchDraw( Rect((Thumbnail.Width - W) div 2, Thumbnail.Height - H,
                                                  W + (Thumbnail.Width - W) div 2, Thumbnail.Height),
                                             FGraphic);
                    end
                  else
                    begin
                      BM.Canvas.Draw((Thumbnail.Width - FGraphic.Width) div 2, Thumbnail.Height - FGraphic.Height, FGraphic);
                    end;
                except
                  FreeAndNil(FGraphic);
                  raise;
                end;
              end
          except
            BM:= nil;
          end;
        end;

      if not Assigned(BM) then
      begin
        BMR := TBitmap.Create;
        BMR.PixelFormat := pf32Bit;
        BMR.SetSize(Thumbnail.Width, Thumbnail.Height);
        BMR.Canvas.Pen.Color := clSilver;
        BMR.Canvas.Pen.Style := psDot;
        BMR.Canvas.Rectangle(0, 0, Thumbnail.Width, Thumbnail.Height);
        Thumbnail.Add(BMR, nil);
        BMR.FreeImage;
      end
      else if (BM.Height = Thumbnail.Height) and (BM.Width = Thumbnail.Width)
      then
      begin
        Thumbnail.Add(BM, nil);
      end
      else if (BM.Height > Thumbnail.Height) or (BM.Width > Thumbnail.Width)
      then
      begin
        BMR := TBitmap.Create;
        BMR.PixelFormat := pf32Bit;
        BMR.SetSize(Thumbnail.Width, Thumbnail.Height);
        k := 1 / Max(BM.Width / Thumbnail.Width, BM.Height / Thumbnail.Height);
        BMR.Canvas.StretchDraw
          (Rect(Round(Thumbnail.Width / 2 - k * BM.Width / 2),
          Round(Thumbnail.Height - k * BM.Height),
          Round(Thumbnail.Width / 2 + k * BM.Width / 2), Thumbnail.Height), BM);

        Thumbnail.Add(BMR, nil);
        BMR.FreeImage;
      end
      else
      begin
        BMR := TBitmap.Create;
        BMR.PixelFormat := pf32Bit;
        BMR.SetSize(Thumbnail.Width, Thumbnail.Height);
        BMR.Canvas.Draw((Thumbnail.Width - BM.Width) div 2,
          (Thumbnail.Height - BM.Height), BM);
        Thumbnail.Add(BMR, nil);
        BMR.FreeImage;
      end;
    finally
      BM.Free;
    end;
end;

procedure TBaseGridForm.actFetchAllExecute(Sender: TObject);
begin
  btnFetchAll.Enabled := False;
  if Assigned(DataSetMeta) and Assigned(DataSetMeta.Cache) and not DataSetMeta.AllFetched
  then
    try
      DataSetMeta.Cache.FetchAll(DataSetMeta.PrepareFetchLinkQueryDescr);
    finally
      btnFetchAll.Enabled := not DataSetMeta.AllFetched;
    end;
end;

function TBaseGridForm.GetControlFilter: TFilterItem;
begin
  if Assigned(DataSetMeta) and Assigned(DataSetMeta.Cache) then
    Result := DataSetMeta.Cache.ControlFilter
  else
    Result := nil;
end;

function TBaseGridForm.StartRefreshTimer: Boolean;
var
  Value: Variant;
  Seconds: Integer;
begin
  Result := False;
  if FRefreshTimerUpdating then
    Exit;
  if not Assigned(FRefreshTimer) then
  begin
    FRefreshTimer := TTimer.Create(self);
    FRefreshTimer.OnTimer := NotifyRefreshTimer;
  end;
  FRefreshTimer.Enabled := False;
  Value := ViewParams.GetValueByName('RefreshTime');
  if VarIsNumeric(Value) then
    Seconds := Value
  else
    Seconds := StrToIntDef(Value, 0);
  if Seconds > 0 then
  begin
    Result := True;
    FRefreshTimer.Interval := Seconds * 1000;
    FRefreshTimer.Enabled := True;
  end
  else
    FreeAndNil(FRefreshTimer);
end;

function TBaseGridForm.StopRefreshTimer: Boolean;
begin
  if FRefreshTimerUpdating then
    Exit(False);
  Result := Assigned(FRefreshTimer);
  if Result then
  begin
    FRefreshTimer.Enabled := False;
    FreeAndNil(FRefreshTimer);
  end;
end;

procedure TBaseGridForm.SubFormCreate(aContext: TContextMeta);
var
  NewMeta: TDataSetMeta;
  aPanel: TPanel;
begin
  NewMeta := TDataSetMeta.Create(utForm, aContext);
  // FOldBeforeSyncronize:= ChildMeta.Cache.OnBeforeSyncronize;
  // ChildMeta.Cache.OnBeforeSyncronize:= MainBeforeSyncronize;
  NewMeta.Cache.PrepareData(False);

  NewMeta.GridForm := TBaseGridForm(ViewTypeToClass(NewMeta.Context.ViewType).Create(Application));
  case NewMeta.Context.Align of
    alTop:
      begin
        aPanel := PanelT;
      end;
    alBottom:
      begin
        aPanel := PanelB;
      end;
    alLeft:
      begin
        aPanel := PanelL;
      end;
    alRight:
      begin
        aPanel := PanelR;
      end;
  else
    begin
      aPanel := PanelL;
    end;
  end;
  NewMeta.GridForm.ManualDock(aPanel);

  NewMeta.GridForm.Align := alClient;
  aPanel.Visible := True;
  NewMeta.GridForm.ReInitForm(NewMeta);
  NewMeta.GridForm.Show;
  (NewMeta.GridForm as TBaseGridForm).StartRefreshTimer;

  SplitterL.Visible := (PanelL.ControlCount > 0);
  SplitterR.Visible := (PanelR.ControlCount > 0);
  SplitterT.Visible := (PanelT.ControlCount > 0);
  SplitterB.Visible := (PanelB.ControlCount > 0);
end;

procedure TBaseGridForm.NotifyRefreshTimer(Sender: TObject);
var
  Updating: Boolean;
begin
  with Sender as TTimer do
    Enabled := False;
  try
    FRefreshTimerUpdating := True;
    try
      if Assigned(DataSetMeta) and Assigned(DataSetMeta.Cache) then
      begin
        Updating := True;
        if Assigned(DataSetMeta.CardForm) and (DataSetMeta.CardForm is TAForm)
        then
          if Assigned((DataSetMeta.CardForm as TAForm).CacheItem) then
            if (isInserted in (DataSetMeta.CardForm as TAForm).CacheItem.State)
              or (isModified in (DataSetMeta.CardForm as TAForm).CacheItem.State) or (DataSetMeta.CardForm as TAForm).Modified
            then
              Updating := False;
        if Updating then
          DataSetMeta.Cache.Update(mcUpdate, null);
      end;
    finally
      FRefreshTimerUpdating := False;
    end;
  finally
    with Sender as TTimer do
      Enabled := not Application.Terminated;
  end;
end;

procedure TBaseGridForm.ShowAttentionSolution;
var
  AttentionText: string;
begin
  pnlAttentionSolution.Visible := False;
  if Assigned(DataSetMeta) and Assigned(DataSetMeta.Table) then
  begin
    AttentionText := DataSetMeta.Table.VariantSolutionGUID;
    if Length(AttentionText) <> 0 then
    begin
      AttentionText := 'Attention_' + AttentionText;
      AttentionText := MetaData.ParamValue[AttentionText];
      if Length(AttentionText) <> 0 then
      begin
        pnlAttentionSolution.Caption := AttentionText;
        pnlAttentionSolution.Visible := True;
      end;
    end;
  end;
end;

{ TDragInfo }

function TDragInfo.CanItem: Boolean;
begin
  Result := (-1 < IField) and (DragTypes * [dtSelf, dtParent] <> []);
end;

function TDragInfo.CanXY: Boolean;
begin
  Result := ((-1 < XField) or (-1 < YField));
end;

function TDragInfo.CanOrder: Boolean;
begin
  Result := (-1 < OField);
end;

function TDragInfo.СanAny: Boolean;
begin
  Result := CanItem or CanXY or CanOrder;
end;

{ TTMenuItemHelper }

function TTMenuItemHelper.GetThis: TMenuItem;
begin
  Result := Self;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}

procedure Startup;
begin
{$IFDEF DEBUG}
  DebugLog('BaseGridFormUnit unit initialization ...');
{$ENDIF}
  RegisterClass(TBaseGridForm);
end;

procedure Shutdown;
begin
{$IFDEF DEBUG}
  DebugLog('BaseGridFormUnit unit finalization ...');
{$ENDIF}
end;
{$ENDREGION}

initialization

Startup;

finalization

Shutdown;

end.
