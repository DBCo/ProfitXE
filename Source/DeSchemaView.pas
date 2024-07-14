unit DeSchemaView;

interface

uses
  Types, Windows, Messages, Classes, Controls, Graphics, {StdCtrls, }ExtCtrls, {ComCtrls, }Forms,
  G2D, deSchema2D;

const
  DE_UPDATESCROLLBARS = WM_USER+140;
  DE_ENHANCEDVIEW     = WM_USER+141;

const
  cRulerSize    = 24;
  cMouseDelta   = 9;

  cMoveMapDelay = 50;
  cMoveObjDelay = 50;

const
  crAddPoint    = 500;
  crMovePoint   = 501;
  crRotate      = 502;
  crSpecial     = 503;
  crCanvas      = 504;

type
  TdeRulerStyle = (rsNone,rsWordlike,rsScholl);

type
  TDeShiftState = set of (ssSpace, ssTab, ssDeShift, ssDeCtrl);

type
  TDeSelectMode = (smInvert, smSelect, smClear, smUnique);

type
  TdeSchemaViewOption  = (voGrid,voSmoothing);
  TdeSchemaViewOptions = set of TdeSchemaViewOption;

type
  TdeSchemaViewScrollbars  = (svsNone,svsHorz,svsVert,svsBoth,svsAuto);

type
  TdeSchemaViewMode  = (vmBrowse,vmEdit);

type
  TdeSchemaView = class;

  TdeSchemaViewHashCell = class
  private
    FItems    : TList;
    function    getCount:integer;
    function    getItem(index:integer):TdeSchemaObject;
  public
    constructor Create;
    destructor  Destroy;override;
    procedure   Clear;
    procedure   Add(anItem:TdeSchemaObject);
    procedure   Delete(index:integer);
    function    indexOf(anItem:TdeSchemaObject):integer;
    property    Count:integer read getCount;
    property    Item[index:integer]:TdeSchemaObject read getItem;
  end;

  TdeSchemaViewHash = class
  private
    FCells    : TList;
    FBounds   : TRect;
    FCellW    : integer;
    FCellH    : integer;
    FColCount : integer;
    FRowCount : integer;
    function    getCell(aCol,aRow:integer):TdeSchemaViewHashCell;
    function    getCellXY(X,Y:integer):TdeSchemaViewHashCell;
    function    getCellRect(aCol,aRow:integer):TRect;
  public
    constructor Create(aBounds:TRect;aColCount,aRowCount:integer);
    destructor  Destroy;override;
    function    CellIDbyXY(X,Y:integer):integer;
    function    ColToX(aCol:integer):integer;
    function    RowToY(aRow:integer):integer;
    function    XToCol(X:integer):integer;
    function    YToRow(Y:integer):integer;
    procedure   ClearItems;
    procedure   Add(aRect:TRect;anItem:TdeSchemaObject);
    property    Bounds:TRect read FBounds;
    property    ColCount:integer read FColCount;
    property    RowCount:integer read FRowCount;
    property    Cell[aCol,aRow:integer]:TdeSchemaViewHashCell read getCell;
    property    CellXY[X,Y:integer]:TdeSchemaViewHashCell read getCellXY;
    property    CellRect[aCol,aRow:integer]:TRect read getCellRect;
  end;

  TdeSchemaViewController = class
  protected
    FOwner : TdeSchemaView;
    procedure    MouseDown(Button:TMouseButton;
                            Shift:TShiftState; X,Y:Integer);virtual;abstract;
    procedure    MouseMove( Shift:TShiftState; X,Y:Integer);virtual;abstract;
    procedure    MouseUp(  Button:TMouseButton;
                            Shift:TShiftState; X,Y:Integer);virtual;abstract;
  public
    constructor  Create(anOwner:TdeSchemaView);
    destructor   Destroy;override;
  end;

  TdeSchemaObjectNotify = procedure(Sender:TObject;
                                              anItem:TdeSchemaObject) of object;

  TdeSchemaView = class(TCustomControl)
  private
    FViewRect      : TRect;
    FBuffer1       : tBitmap; //в девять раз больше размеров
                              //экрана. Сюда отрисовывается увеличинная карта
    FBuffer3       : tBitmap; //в три раза больше размеров
                              //экрана. Сюда делается BitBlt из FBuffer2 и
                              //StretchBlt из FBuffer1 для улучшения качества картинки
                              //а также здесь отрисовываются рамки и сетка
    FTopRulerBuf   : tBitmap;
    FLeftRulerBuf  : tBitmap;
    FTopLeftBuf    : tBitmap;
    FSBTracking    : boolean;
    FOldVSBPos     : integer;
    FOldHSBPos     : integer;
    FLMMove_Timer  : boolean;
    FLMMove_Pos    : TPoint;
    FLMMove_Shift  : TShiftState;
    FViewCoorninates : TdeViewCoorninates;
    FConvertion    : TMatrix2D;
    FRConvertion   : TMatrix2D;
    FConvToBuffer  : TMatrix2D;
    FSelected      : TdeSchemaObjectList;
    FChangesShown  : boolean;
    FSelectRect    : TRect;
    FFocused       : TdeSchemaObject;
    FEditItem      : TdeSimpleCurve;
    FController    : TdeSchemaViewController;
    FBrowseCtlr    : TdeSchemaViewController;
    FEditCtlr      : TdeSchemaViewController;
    FCustomObjCtlr : TdeSchemaViewController;
    FChangeOptions : boolean;
    FActiveShortcut: TdeSchemaShortcut;
    FPrepareTick   : integer;
    FDeShiftState  : TDeShiftState;
    FApplicationOnMessage : TMessageEvent;
    procedure    CreateBuffers;
    procedure    DestroyBuffers;
    procedure    ClearBuffer(aBuffer:TBitmap);
    procedure    UpdateHash;
    procedure    PrepareBuffers;
    procedure    PrepareWordlikeRullers;
    procedure    PrepareSchollRullers;
    procedure    PrepareTopLeftPanel;
    procedure    RefreshEnhancedBuffer;
    procedure    ApplyEnhancedBuffer;
    procedure    RefreshViewBuffer;
    procedure    CalcGridParams(var Xdown,Xup,XFirst,XStep : double;
                                var Ydown,Yup,YFirst,YStep : double);
    procedure    doDrawGrid(aBuffer:TBitmap;StoC:TMatrix2D);
    procedure    UpdateObjectView(anItem:TdeSchemaObject);
    procedure    UpdateViewRect;
    procedure    UpdateConvertion;
    procedure    UpdateConvToBuffer;

    procedure    KeyRepeat(Sender: TObject);
    procedure    KeyMessages(var Msg: tMsg; var Handled: Boolean);
    procedure    WMKeyClear(var Message: TMessage); message WM_KILLFOCUS;
    procedure    WMKeyStart(var Message: TMessage); message WM_SETFOCUS;

    procedure    WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure    WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure    WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure    WMMouseWheel(var Message: TMSHMouseWheel); message WM_MOUSEWHEEL;
    procedure    WMContextMenu(var Message: TMessage); message WM_ContextMenu;
    procedure    DEUpdateScrollBars(var Message:TMessage); message DE_UPDATESCROLLBARS;
    procedure    DEEnhancedView(var Message:TMessage);message DE_ENHANCEDVIEW;  
    procedure    DelayTimerOnTimer(Sender: TObject);
    function     getSelectedCount:integer;
    function     getSelected(index:integer):TdeSchemaObject;
    function     getFocusedItem:TdeSchemaObject;
    procedure    setFocusedItem(aValue:TdeSchemaObject);
    procedure    LocateFocusedItem;
    procedure    setMode(aValue:TdeSchemaViewMode);
    function     CalcCursor(anObject:TdeSchemaObject; aPC: TdeBoundsPointClass):TCursor;
  protected
    FOptions       : TdeSchemaViewOptions;
    FRulerStyle    : TdeRulerStyle;
    FScrollbars    : TdeSchemaViewScrollbars;
    FSchema        : TdeSchema;
    FOffset        : TPoint2D;
    FScale         : double;
    FIntZoom       : integer;
    FViewDelta     : TPoint;
    FDelayTimer    : TTimer;
    FKeyTimer      : TTimer;
    FKeyCode       : Integer;
    FUpdateLock    : integer;
    FNeedUpdate    : boolean;
    FNeedRefresh   : boolean;
    FDeleting      : boolean;
    FHash          : TdeSchemaViewHash;
    FMode          : TdeSchemaViewMode;
    FOnSelectObject: TdeSchemaObjectNotify;
    FOnFocusObject : TdeSchemaObjectNotify;
    FOnNewObject   : TdeSchemaObjectNotify;
    FOnDeleteObject: TdeSchemaObjectNotify;
    FOnObjectChanged:TdeSchemaObjectNotify;
    procedure    CreateParams(var Params: TCreateParams); override;
    procedure    setOptions(aValue:TdeSchemaViewOptions);
    function     getOptions:TdeSchemaViewOptions;
    procedure    setRulerStyle(aValue:TdeRulerStyle);
    procedure    setScrollbars(aValue:TdeSchemaViewScrollbars);
    procedure    setOffset(aValue:TPoint2D);
    procedure    OffsetChanged;
    procedure    setScale(aValue:double);
    function     getZoom:single;
    procedure    ScaleChanged;
    procedure    SchemaNewLayer(aLayer:TdeSchemaLayer);
    procedure    SchemaDelLayer(aLayer:TdeSchemaLayer);
    procedure    SchemaLayerChanged(aLayer:TdeSchemaLayer);
    procedure    SchemaNewShortcut(aShortcut:TdeSchemaShortcut);
    procedure    SchemaDelShortcut(aShortcut:TdeSchemaShortcut);
    procedure    SchemaShortcutChanged(aShortcut:TdeSchemaShortcut);
    procedure    CheckItemNotActive(anItem:TdeSchemaObject);
    procedure    SchemaNewItem(anItem:TdeSchemaObject);
    procedure    SchemaDelItem(anItem:TdeSchemaObject);
    procedure    SchemaItemChanging(anItem:TdeSchemaObject);
    procedure    SchemaItemChanged(anItem:TdeSchemaObject;
                             ChangedProps:TdeObjectChangedProps);
    procedure    SchemaChanged;
    procedure    DrawRulers;
    procedure    Paint; override;
    procedure    DrawChanges;
    procedure    HideChanges;
    procedure    ShowChanges;//вызывается из DrawChanges/HideChanges
    procedure    HideSelectRect;
    procedure    DrawSelectRect(aRect:TRect);
    procedure    MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);override;
    procedure    MouseMove(Shift:TShiftState;X,Y:Integer);override;
    procedure    MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);override;
    procedure    UpdateScrollBars;
    procedure    MoveDisplay(dX,dY:integer; Limited: Boolean = False);
    procedure    MoveSelected(dX,dY:integer);
    procedure    ResizeSelected(Direction:TdeBoundsPointClass;
                                dX,dY:integer; Proportional:boolean);
    procedure    RotateSelected(Angle:double);
    function     getShortcutAt(X,Y:integer):TdeSchemaShortcut;
    function     getItemAt(X,Y:integer):TdeSchemaObject;
    function     getRootItemAt(X,Y:integer):TdeSchemaObject;
    procedure    SelectObjectNotify(anItem:TdeSchemaObject);
    procedure    FocusObjectNotify(anItem:TdeSchemaObject);
    procedure    NewObjectNotify(anItem:TdeSchemaObject);
    procedure    DeleteObjectNotify(anItem:TdeSchemaObject);
    procedure    ObjectChangedNotify(anItem:TdeSchemaObject);
  public
    constructor  Create(AOwner: TComponent); override;
    destructor   Destroy; override;
    procedure    SetBounds(ALeft, ATop, AWidth, AHeight: Integer);override;
    procedure    NormalizeByRect(R:TRect2D);
    procedure    Normalize;
    procedure    Centeralize;
    procedure    ZoomIn;
    procedure    ZoomOut;
    function     SchemaToView(aPoint:TPoint2D):TPoint;
    function     ViewToSchema(aPoint:TPoint):TPoint2D;
    function     ViewToSchemaRound(aPoint:TPoint):TPoint2D;
    procedure    BeginUpdate;
    procedure    EndUpdate;
    function     IsUpdating:boolean;
    procedure    UpdateView(Forced:boolean = false);
    procedure    RefreshView(Forced:boolean = false);
    procedure    Select(Item:TdeSchemaObject; SelectMode: TDeSelectMode = smInvert);
    function     SelectedIndex(Item:TdeSchemaObject):integer;
    procedure    SelectByRect(aRect:TRect);
    procedure    ClearSelection;
    function     canGroup:boolean;
    function     canUnGroup:boolean;
    procedure    Group;
    procedure    unGroup;
    function     canEdit:boolean;
    procedure    Edit;
    procedure    EndEdit;
    function     IsEditing:boolean;
    function     IsFocuseRectPoint(X,Y:integer;
                                   var selObj:TdeSchemaObject;
                                   var pntID:integer):TdeBoundsPointClass;
    property     Options:TdeSchemaViewOptions read getOptions write setOptions;
    property     RulerStyle:TdeRulerStyle read FRulerStyle write setRulerStyle;
    property     Schema:TdeSchema read FSchema;
    property     Zoom:single read getZoom;
    property     Offset:TPoint2D read FOffset write setOffset;
    property     Scale:double read FScale write setScale;
    property     ShortcutAt[X,Y:integer]:TdeSchemaShortcut read getShortcutAt;
    property     ItemAt[X,Y:integer]:TdeSchemaObject read getItemAt;
    property     RootItemAt[X,Y:integer]:TdeSchemaObject read getRootItemAt;
    property     SelectedCount:integer read getSelectedCount;
    property     Selected[index:integer]:TdeSchemaObject read getSelected;
    property     FocusedItem:TdeSchemaObject read getFocusedItem write setFocusedItem;
    property     EditedItem:TdeSimpleCurve read FEditItem;
    property     ActiveShortcut: TdeSchemaShortcut read FActiveShortcut;
    property     ShowCoordinates: TdeViewCoorninates read FViewCoorninates write FViewCoorninates;
    property     Mode:TdeSchemaViewMode read FMode write setMode;
    property     ViewRect:TRect read FViewRect;
    property     Scrollbars:TdeSchemaViewScrollbars read FScrollbars write setScrollbars;
  published
    property     DeShiftState : TDeShiftState read FDeShiftState;
    property     ParentColor;
    property     onMouseDown;
    property     onMouseMove;
    property     onMouseUp;
    property     onDblClick;
    property     onSelectObject:TdeSchemaObjectNotify read FOnSelectObject write FOnSelectObject;
    property     onFocusObject:TdeSchemaObjectNotify read FOnFocusObject write FOnFocusObject;
    property     onNewObject:TdeSchemaObjectNotify read FOnNewObject write FOnNewObject;
    property     onDeleteObject:TdeSchemaObjectNotify read FOnDeleteObject write FOnDeleteObject;
    property     onObjectChanged:TdeSchemaObjectNotify read FOnObjectChanged write FOnObjectChanged;
    property     PopupMenu;
  end;

implementation

uses SysUtils, Math,
     DeLog, Funcs;

const
  _ZoomTickCount = 6;
  _ZoomTicks : array[0.._ZoomTickCount-1] of integer = (10,15,20,30,50,75);

const
  _ScaleK = 2;

const
  ENHANCEDVIEWDELAY = 300;

{TdeSchemaViewHashCell}
constructor TdeSchemaViewHashCell.Create;
begin
  inherited Create;
  FItems    := TList.Create;
end;

destructor  TdeSchemaViewHashCell.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function    TdeSchemaViewHashCell.getCount:integer;
begin
  Result := FItems.Count;
end;

function    TdeSchemaViewHashCell.getItem(index:integer):TdeSchemaObject;
begin
  Result := TdeSchemaObject(FItems[index]);
end;

procedure   TdeSchemaViewHashCell.Clear;
begin
  FItems.Clear;
end;

procedure   TdeSchemaViewHashCell.Add(anItem:TdeSchemaObject);
begin
  FItems.Add(anItem);
end;

procedure   TdeSchemaViewHashCell.Delete(index:integer);
begin
  FItems.Delete(index);
end;

function    TdeSchemaViewHashCell.indexOf(anItem:TdeSchemaObject):integer;
begin
  Result := FItems.IndexOf(anItem);
end;



{TdeSchemaViewHash}
constructor TdeSchemaViewHash.Create(aBounds:TRect;aColCount,aRowCount:integer);
var
  X,Y : integer;
begin
  inherited Create;
  FCells    := TList.Create;
  FBounds   := aBounds;
  FColCount := aColCount;
  FRowCount := aRowCount;
  FCellW := (FBounds.Right-FBounds.Left) div ColCount;
  FCellH := (FBounds.Bottom-FBounds.Top) div RowCount;
  for Y:=0 to RowCount-1 do begin
    for X := 0 to ColCount-1 do
      FCells.Add(TdeSchemaViewHashCell.Create);
  end;
end;

destructor  TdeSchemaViewHash.Destroy;
begin
  while (FCells.Count>0) do begin
    TdeSchemaViewHashCell(FCells[0]).Free;
    FCells.Delete(0);
  end;
  FCells.Free;
  inherited Destroy;
end;

function    TdeSchemaViewHash.getCell(aCol,aRow:integer):TdeSchemaViewHashCell;
begin
  Result := TdeSchemaViewHashCell(FCells[aRow*ColCount+aCol]);
end;

function    TdeSchemaViewHash.getCellXY(X,Y:integer):TdeSchemaViewHashCell;
var
  i : integer;
begin
  i := CellIDbyXY(X,Y);
  if i>=0 then
    Result := TdeSchemaViewHashCell(FCells[i])
  else
    Result := nil;
end;

function    TdeSchemaViewHash.getCellRect(aCol,aRow:integer):TRect;
begin
  Result.TopLeft := Point(ColToX(aCol),RowToY(aRow));
  Result.BottomRight := Point(Result.Left+FCellW,Result.Top+FCellH);
end;

function    TdeSchemaViewHash.CellIDbyXY(X,Y:integer):integer;
begin
  if PtInRect(FBounds,Point(X,Y)) then begin
    Result := YToRow(Y)*ColCount + XToCol(X);
  end
  else
    Result :=-1
end;

function    TdeSchemaViewHash.ColtoX(aCol:integer):integer;
begin
  Result := FBounds.Left+aCol*FCellW;
end;

function    TdeSchemaViewHash.RowtoY(aRow:integer):integer;
begin
  Result := FBounds.Top+aRow*FCellH;
end;

function    TdeSchemaViewHash.XtoCol(X:integer):integer;
begin
  Result := (X-FBounds.Left) div FCellW;
end;

function    TdeSchemaViewHash.YtoRow(Y:integer):integer;
begin
  Result := (Y-FBounds.Top) div FCellH;
end;

procedure   TdeSchemaViewHash.ClearItems;
var
  i : integer;
begin
  for i:=0 to FCells.Count-1 do
    TdeSchemaViewHashCell(FCells[i]).Clear;
end;

procedure   TdeSchemaViewHash.Add(aRect:TRect;anItem:TdeSchemaObject);
var
  X,Y : integer;
  R   : TRect;
begin
  R.Left   := XtoCol(aRect.Left);
  if (R.Left>=ColCount) then exit;
  R.Top    := YtoRow(aRect.Top);
  if (R.Top>=RowCount) then exit;
  R.Right  := XtoCol(aRect.Right);
  if (R.Right<0) then exit;
  R.Bottom := YtoRow(aRect.Bottom);
  if (R.Bottom<0) then exit;

  if (R.Top<0) then R.Top:=0;
  if (R.Left<0) then R.Left:=0;
  if (R.Bottom>=RowCount) then R.Bottom:=RowCount-1;
  if (R.Right>=ColCount) then R.Right:=ColCount-1;

  for Y := R.Top to R.Bottom do
    for X := R.Left to R.Right do
      Cell[X,Y].Add(anItem);
end;

{TdeSchemaViewController}
constructor TdeSchemaViewController.Create(anOwner:TdeSchemaView);
begin
  inherited Create;
  FOwner := anOwner;
end;

destructor  TdeSchemaViewController.Destroy;
begin
  inherited Destroy;
end;


type
  TdeSchemaMouseMode =  (emNone,emBrowse,emSelect,emUnSelect,emMove,emRect,
                         emResize,emRotate,emSpecial,emEdit,
                         emMoveVertex,emAddVertex,emMoveLeftCP,emMoveRightCP,
                         emShortcut);

{TdeBrowseController}
type
  TdeBrowseController = class(TdeSchemaViewController)
  protected
    FMouseMode : TdeSchemaMouseMode;
    FMouseMoved: boolean;
    FMouseDbl  : boolean;
    FLastPos   : TPoint;
    FRectStart : TPoint;
    FButton    : TMouseButton;
    procedure    MouseDown(Button:TMouseButton;
                            Shift:TShiftState;
                              X,Y:Integer);override;
    procedure    MouseMove( Shift:TShiftState;
                              X,Y:Integer);override;
    procedure    MouseUp(  Button:TMouseButton;
                            Shift:TShiftState;
                              X,Y:Integer);override;
  public
    constructor  Create(anOwner:TdeSchemaView);
    destructor   Destroy;override;
  end;


constructor  TdeBrowseController.Create(anOwner:TdeSchemaView);
begin
  inherited Create(anOwner);
  FMouseMode := emNone;
  FMouseMoved:= false;
  FMouseDbl  := false;
  FLastPos   := Point(-1,-1);
  FRectStart := Point(0,0);
end;

destructor   TdeBrowseController.Destroy;
begin
  inherited Destroy;
end;

procedure    TdeBrowseController.MouseDown(Button:TMouseButton;
                                            Shift:TShiftState;
                                              X,Y:Integer);
var
  selSC  : TdeSchemaShortcut;
  anItem : TdeSchemaObject;
begin
  if (FMouseMode<>emNone) then exit;
  FButton     := Button;
  FMouseMoved := false;
  FMouseMode  := emNone;
  FLastPos    := Point(X,Y);
  FMouseDbl   := (ssDouble in Shift);

  with FOwner do
  begin
    selSC  := ShortcutAt[X,Y];
    if (selSC <> nil) then
      begin
        ClearSelection;
        FActiveShortcut  := selSC;
        if (Button = mbLeft) then
          FMouseMode := emShortcut;
        FocusedItem := nil;
      end
    else
      begin
        anItem := RootItemAt[X,Y];
        if (anItem<>nil) then
          begin
            if SelectedIndex(anItem) < 0 then
              begin
                FMouseMode := emNone;
                BeginUpdate;
                if not(ssShift in Shift) then
                  ClearSelection;
                Select(anItem);
                EndUpdate;
                if (Button = mbLeft)and(ssSpace in FDeShiftState) then
                  FMouseMode := emBrowse;
                FocusedItem := anItem;
              end
            else
              begin
                if (Button = mbLeft) then
                    begin
                      FMouseMode := emUnSelect;
                      Select(anItem);
                    end;
                  LocateFocusedItem;
              end;
          end
        else
          begin
            if (Button = mbLeft) and not (ssSpace in FDeShiftState) then
              begin
                if not(ssShift in Shift) then
                  ClearSelection;
                FMouseMode := emRect;
                FRectStart := Point(X,Y);
              end
            else
              begin
                ClearSelection;
                if (Button = mbLeft)and(ssSpace in FDeShiftState) then
                  FMouseMode := emBrowse;
                FocusedItem := nil;
              end;
          end;
      end;
    if (FMouseMode <> emNone) then
      BeginUpdate;
  end;
end;

procedure    TdeBrowseController.MouseMove( Shift:TShiftState;
                                              X,Y:Integer);
var
  selSC     : TdeSchemaShortcut;
  selObj    : TdeSchemaObject;
begin
  if (abs(X-FLastPos.X)=0)and(abs(Y-FLastPos.Y)=0) then exit;
  FMouseMoved := true;
  with FOwner do begin
    case FMouseMode of
      emNone : begin
        selSC  := ShortcutAt[X,Y];
        if selSC <> nil then
          Cursor := crHandPoint
        else begin
          selObj := RootItemAt[X,Y];
          if (selObj<>nil)and not(selObj.ReadOnly)
          then
            Cursor := crHandPoint
          else
            Cursor := crDefault;
        end;
      end;
      emBrowse : begin
          MoveDisplay(X-FLastPos.X,Y-FLastPos.Y);
      end;
      emUnSelect : begin
        FMouseMode := emBrowse;
        MouseMove(Shift,X,Y);
      end;
      emRect : begin
        HideSelectRect;
        DrawSelectRect(Rect(FRectStart,Point(X,Y)));
      end;
      emShortcut : begin
        HideChanges;
        FActiveShortcut.Offset := ViewToSchema(Point(X,Y));
        DrawChanges;
      end;
    end;//case Mode
  end;
  FLastPos := Point(X,Y);
end;

procedure    TdeBrowseController.MouseUp(  Button:TMouseButton;
                                            Shift:TShiftState;
                                              X,Y:Integer);
begin
  if (Button<>FButton) then exit;
  with FOwner do begin
    HideSelectRect;
    case FMouseMode of
      emNone : begin
      end;
      emBrowse : begin
        if FMouseDbl then
          DblClick;
        RefreshView;
      end;
      emUnSelect : begin
        if FMouseDbl then
          DblClick;
      end;
      emRect : begin
        SelectByRect(Rect(FRectStart,Point(X,Y)));
      end;
      emShortcut : begin
        if FMouseDbl then
          DblClick;
        FActiveShortcut := nil;
        RefreshView;
      end;
    end; //case Mode
    if (FMouseMode <> emNone) then
      EndUpdate;
  end;
  FLastPos    := Point(X,Y);
  FMouseMoved := false;
  FMouseDbl   := false;
  FMouseMode  := emNone;
end;

{TdeEditController}
type
  TdeEditController   = class(TdeSchemaViewController)
  protected
    FMouseMode : TdeSchemaMouseMode;
    FMouseMoved: boolean;
    FMouseDbl  : boolean;
    FLastPos   : TPoint;
    FRectStart : TPoint;
    FButton    : TMouseButton;
    FBndsPoint : TdeBoundsPointClass;
    FBndsPntID : integer;
    procedure    MouseDown(Button:TMouseButton;
                            Shift:TShiftState;
                              X,Y:Integer);override;
    procedure    MouseMove( Shift:TShiftState;
                              X,Y:Integer);override;
    procedure    MouseUp(  Button:TMouseButton;
                            Shift:TShiftState;
                              X,Y:Integer);override;
  public
    constructor  Create(anOwner:TdeSchemaView);
    destructor   Destroy;override;
  end;


constructor  TdeEditController.Create(anOwner:TdeSchemaView);
begin
  inherited Create(anOwner);
  FMouseMode  := emNone;
  FMouseMoved := false;
  FMouseDbl   := false;
  FLastPos    := Point(-1,-1);
  FRectStart  := Point(0,0);
  FBndsPoint  := bpcUnknown;
  FBndsPntID  :=-1;
end;

destructor   TdeEditController.Destroy;
begin
  inherited Destroy;
end;

procedure    TdeEditController.MouseDown(Button:TMouseButton;
                                          Shift:TShiftState;
                                            X,Y:Integer);
var
  selSC  : TdeSchemaShortcut;
  selObj : TdeSchemaObject;
  i      : integer;
begin
  if (FMouseMode <> emNone) then exit;
  FButton     := Button;
  FMouseMoved := false;
  FMouseMode  := emNone;
  FLastPos    := Point(X,Y);
  FMouseDbl   := (ssDouble in Shift);

  if (Button = mbLeft)and(ssSpace in FOwner.FDeShiftState) then
    begin
      FMouseMode := emBrowse;
      Exit;
    end;

  with FOwner do begin
    FBndsPoint := IsFocuseRectPoint(X,Y,selObj,FBndsPntID);
    if (FBndsPoint = bpcUnknown) then begin
      FMouseMode := emNone;
      selSC  := ShortcutAt[X,Y];
      if (selSC <> nil) then begin
        ClearSelection;
        FActiveShortcut  := selSC;
        FocusedItem := FActiveShortcut.LinkedObject;
        if (Button = mbLeft) then
          FMouseMode := emShortcut;
      end
      else begin
        FActiveShortcut  := nil;
        selObj := RootItemAt[X,Y];
        if (selObj <> nil) then
          begin
            if (SelectedIndex(selObj) < 0) then
              begin
                FMouseMode := emNone;
                BeginUpdate;
                if not(ssShift in Shift) then
                  ClearSelection;
                Select(selObj);
                EndUpdate;
                if (Button = mbLeft) then begin
                  FMouseMode := emMove;
                  for i := 0 to SelectedCount-1 do
                    Selected[i].Lock;
                end;
                FocusedItem := selObj;
              end
            else
              begin
                if (Button = mbLeft) then
                  begin
//DONE: RA Было     FMouseMode := emUnSelect;
//                  Select(selObj);
// Стало
                    BeginUpdate;
                    if not(ssShift in Shift) then
                      ClearSelection;
                    Select(selObj);
                    EndUpdate;
                    if (Button = mbLeft) then begin
                      FMouseMode := emMove;
                      for i := 0 to SelectedCount-1 do
                        Selected[i].Lock;
                    end;

                  end;
                LocateFocusedItem;
              end;
          end
        else
          begin
            if (Button = mbLeft)and Not(ssSpace in FDeShiftState) then
              begin
                if not(ssShift in Shift) then
                  ClearSelection;
                FMouseMode := emRect;
                FRectStart := Point(X,Y);
              end
            else
              begin
                ClearSelection;
                if (Button = mbLeft)and(ssSpace in FDeShiftState) then
                  FMouseMode := emBrowse;
                FocusedItem := nil;
              end;
          end;
      end;
    end
    else if (Button = mbLeft) then
    begin
      FocusedItem := selObj;
      case FBndsPoint of
        bpcRotation : FMouseMode := emRotate;
        bpcSpecial  : FMouseMode := emSpecial;
        else          FMouseMode := emResize;
      end;
      for i := 0 to SelectedCount-1 do
        Selected[i].Lock;
    end;
    if (FMouseMode <> emNone)
    then
      BeginUpdate;
  end;
end;

procedure    TdeEditController.MouseMove( Shift:TShiftState; X,Y:Integer);
var
  selSC     : TdeSchemaShortcut;
  selObj    : TdeSchemaObject;
  pntID     : integer;
  BPC       : TdeBoundsPointClass;
  angle,fangle : double;
  i         : integer;
begin
  if (abs(X-FLastPos.X) = 0) and (abs(Y-FLastPos.Y) = 0) then exit;
  FMouseMoved := true;
  
  if (ssSpace in FOwner.DeShiftState) then
     begin
       if (FMouseMode <> emNone) then
          FOwner.MoveDisplay(X-FLastPos.X,Y-FLastPos.Y);
       //FOwner.Cursor := crCanvas
     end
  else

  with FOwner do begin
    case FMouseMode of
      emNone : begin
        BPC := IsFocuseRectPoint(X,Y,selObj,PntID);
        if (BPC <> bpcUnknown)
        then begin
          case BPC of
            bpcRotation :
              Cursor := crRotate;
            bpcSpecial  :
              Cursor := crSpecial;
            else
              Cursor := CalcCursor(selObj,BPC);
          end;
        end
        else begin
          selSC  := ShortcutAt[X,Y];
          if selSC <> nil then
            Cursor := crHandPoint
          else begin
            selObj := RootItemAt[X,Y];
            if (selObj <> nil)and not(selObj.ReadOnly)
            then
              Cursor := crSizeAll
            else
              Cursor := crDefault;
          end;
        end;
      end;
      emBrowse : begin
          MoveDisplay(X-FLastPos.X,Y-FLastPos.Y);
      end;
      emUnSelect : begin
        FMouseMode := emMove;
        for i := 0 to SelectedCount-1 do
          Selected[i].Lock;
        MouseMove(Shift,X,Y);
      end;
      emMove : begin
        HideChanges;
        MoveSelected(X-FLastPos.X,Y-FLastPos.Y);
        DrawChanges;
      end;
      emRect : begin
        HideSelectRect;
        DrawSelectRect(Rect(FRectStart,Point(X,Y)));
      end;
      emResize : begin
        HideChanges;
        ResizeSelected(FBndsPoint,X-FLastPos.X,Y-FLastPos.Y,(ssCtrl in Shift));
        DrawChanges;
      end;
      emRotate : begin
        HideChanges;
        Angle :=-FocusedItem.AngleToPoint(ViewToSchema(Point(X,Y)));
        if (ssDeCtrl in FOwner.FDeShiftState) then
          begin
            fangle:= FocusedItem.Rotation;
            Angle := Round((Angle+Fangle)*12/Pi)*Pi/12-Fangle; // 180 = 15*12
          end;
        RotateSelected(Angle);
        DrawChanges;
      end;
      emShortcut : begin
        HideChanges;
        FActiveShortcut.Offset := ViewToSchema(Point(X,Y));
        DrawChanges;
      end;
    end;//case Mode
  end;
  FLastPos    := Point(X,Y);
end;

procedure    TdeEditController.MouseUp(  Button:TMouseButton;
                                          Shift:TShiftState;
                                              X,Y:Integer);
var
  i : integer;
begin
  if (Button <> FButton) then exit;
  with FOwner do begin
    HideChanges;
    HideSelectRect;
    case FMouseMode of
      emNone : begin
      end;
      emBrowse : begin
        RefreshView;
      end;
      emUnSelect : begin
        if FMouseDbl then
          DblClick;
      end;
      emMove : begin
        for i := 0 to SelectedCount-1 do
          Selected[i].unLock;
        if FMouseDbl then
          DblClick;
      end;
      emRect : begin
        SelectByRect(Rect(FRectStart,Point(X,Y)));
      end;
      emResize : begin
        for i := 0 to SelectedCount-1 do
          Selected[i].unLock;
      end;
      emRotate : begin
        for i := 0 to SelectedCount-1 do
          Selected[i].unLock;
      end;
      emSpecial : begin
        for i := 0 to SelectedCount-1 do
          Selected[i].unLock;
      end;
      emShortcut : begin
        if FMouseDbl then
          DblClick;
        //FActiveShortcut := nil;
        RefreshView;
      end;
    end;
    if (FMouseMode <> emNone) then
      EndUpdate;
  end;
  FMouseMoved := false;
  FMouseDbl   := false;
  FMouseMode  := emNone;
  FLastPos    := Point(X,Y);
end;


{TdeCustomObjController}
type
  TdeCustomObjController   = class(TdeSchemaViewController)
  protected
    FMouseMode    : TdeSchemaMouseMode;
    FMouseMoved   : boolean;
    FLastPos      : TPoint;
    FButton       : TMouseButton;
    FObjectPoint  : TdeVertexesPointClass;
    FNewVtx       : integer;
    FNewVtxPos    : TPoint;
    procedure    MouseDown(Button:TMouseButton;
                            Shift:TShiftState;
                              X,Y:Integer);override;
    procedure    MouseMove( Shift:TShiftState;
                              X,Y:Integer);override;
    procedure    MouseUp(  Button:TMouseButton;
                            Shift:TShiftState;
                              X,Y:Integer);override;
  public
    constructor  Create(anOwner:TdeSchemaView);
    destructor   Destroy;override;
  end;


constructor  TdeCustomObjController.Create(anOwner:TdeSchemaView);
begin
  inherited Create(anOwner);
  FMouseMode    := emNone;
  FMouseMoved   := false;
  FLastPos      := Point(-1,-1);
  FObjectPoint  := vpcUnknown;
end;

destructor   TdeCustomObjController.Destroy;
begin
  inherited Destroy;
end;

procedure    TdeCustomObjController.MouseDown(Button:TMouseButton;
                                          Shift:TShiftState;
                                            X,Y:Integer);
var
  pntID   : integer;
begin
  if (FMouseMode <> emNone) then exit;
  FButton     := Button;
  FMouseMoved := false;
  FMouseMode  := emNone;
  FLastPos    := Point(X,Y);
  if (ssDouble in Shift)then exit;

  with FOwner do begin
    with EditedItem do begin
      pntID   := ActiveVertex;
      FObjectPoint  := Classify(ViewToSchema(Point(X,Y)),
                                sqrt(2)*cPointSize/FOwner.Scale,
                                pntID);
    end;
    if (FObjectPoint = vpcUnknown) then begin
      MouseUp(Button,Shift,X,Y);
      EndEdit;
      FOwner.MouseDown(Button,Shift,X,Y);
    end
    else if ((FObjectPoint = vpcBody)and(Button = mbRight)) then begin
    end
    else if (Button = mbLeft) then begin
      case FObjectPoint of
        vpcVertex : begin
          EditedItem.ActiveVertex := pntID;
          UpdateObjectView(EditedItem);
          FEditItem.PaintChanges(Canvas,FConvertion,FViewCoorninates);

          FOwner.Schema.NotifyItemChanged(FEditItem,[]);

          FMouseMode := emMoveVertex;
        end;
        vpcEdge   : begin
          FNewVtx    := pntID;
          FNewVtxPos := Point(X,Y);
          EditedItem.ActiveVertex :=-1;
          UpdateObjectView(EditedItem);
          FEditItem.PaintChanges(Canvas,FConvertion,FViewCoorninates);
          FMouseMode  := emAddVertex;
        end;
        vpcLeftCP   : begin
          FMouseMode := emMoveLeftCP;
        end;
        vpcRightCP   : begin
          FMouseMode := emMoveRightCP;
        end;
      end;
    end;
    if (FMouseMode <> emNone) then
      FOwner.BeginUpdate;
  end;
end;

procedure    TdeCustomObjController.MouseMove( Shift:TShiftState;
                                            X,Y:Integer);
var
  selSC           : TdeSchemaShortcut;
  selObj          : TdeSchemaObject;
  i,iX,iY,D,pntID : integer;
  mX,mY           : double;
  OPC             : TdeVertexesPointClass;
  S               : String;
  P,Pt            : TPoint2D;
begin
  if (abs(X-FLastPos.X) = 0) and (abs(Y-FLastPos.Y) = 0) then exit;
  if ssSpace in FOwner.FDeShiftState then
    begin
      Exit;
    end;

  s:='';

  FMouseMoved := true;
  with FOwner do begin
    case FMouseMode of
      emBrowse : begin
          MoveDisplay(X-FLastPos.X,Y-FLastPos.Y);
      end;
      emNone : begin
        with EditedItem do
          begin
            pntID := ActiveVertex;
            OPC := Classify(ViewToSchema(Point(X,Y)),sqrt(2)*cPointSize/FOwner.Scale,pntID);
          end;
        if (OPC <> vpcUnknown) then
          begin
            case OPC of
              vpcVertex,
              vpcLeftCP,
              vpcRightCP : with EditedItem do
                           begin
                             Cursor := crMovePoint;
                             case VertexType[pntID] of
                               vtxDirect:  s:='|_Da.pointdirect ' +IntToName(pntID);
                               vtxAngular: s:='|_Da.pointangular '+IntToName(pntID);
                               vtxSmooth:  s:='|_Da.pointsmooth ' +IntToName(pntID);
                             end;
                           end;
              vpcEdge    : with EditedItem do
                           begin
                             Cursor := crAddPoint;
                             
                             D:=(pntID+1) mod SegmentsCount;
                             if SegmentType[pntID] = sgmBezier then
                               begin
                                 s:='|_Da.pointbezier '+IntToName(pntID)+IntToName(D);
                                 //TODO: Сделать вычисление длины искривленного сегмента
                               end
                             else
                               s:='|_Da.pointlinear '+IntToName(pntID)+IntToName(D)+
                                 Format(' = %3.3f',[SQRT(
                                   SQR(OtoS(Vertex[pntID]).X - OtoS(Vertex[D]).X)+
                                   SQR(OtoS(Vertex[pntID]).Y - OtoS(Vertex[D]).Y))]);
                           end;
              vpcBody    : Cursor := crSizeAll;
              else         Cursor := crDefault;
            end;
          end
        else
          begin
          selSC  := ShortcutAt[X,Y];
          if selSC <> nil then
            Cursor := crHandPoint
          else
            begin
              selObj := RootItemAt[X,Y];
              if (selObj <> nil)and not(selObj.ReadOnly) then
                Cursor := crSizeAll
              else
                Cursor := crDefault;
            end;
          end;
      end;
      emMoveVertex : begin
        HideChanges;
        if not(EditedItem.IsUpdating) then EditedItem.BeginUpdate;
        P:=ViewToSchema(Point(X,Y));

        with FEditItem do
          begin
            iX:=-1;  mX:=0; // ищем ближайшие точки
            iY:=-1;  mY:=0; 
            for i:=0 to VertexesCount-1 do
              if i<>ActiveVertex then
                begin
                  Pt:= OtoS(Vertex[i]);
                  if (iX<0) or (abs(Pt.X-P.X)<mX) then
                    begin iX:=i; mx:= abs(Pt.X-P.X); end;
                  if (iY<0) or (abs(Pt.Y-P.Y)<mY) then
                    begin iY:=i; my:= abs(Pt.Y-P.Y);  end;
                end;

            if (ssDeCtrl in FOwner.FDeShiftState) then D:= cMouseDelta
                                                  else D:= 0; 

            if (iX>-1) and (abs(X-SchemaToView(OtoS(Vertex[iX])).X)<=D ) then
              P.X:=FEditItem.OtoS(Vertex[iX]).X;
            if (iY>-1) and (abs(Y-SchemaToView(OtoS(Vertex[iY])).Y)<=D ) then
              P.Y:=FEditItem.OtoS(Vertex[iY]).Y;
          end;

        if D=0 then s:='|_dL.movebygrid';
        EditedItem.Vertex[EditedItem.ActiveVertex]:=FEditItem.StoO(P);
        DrawChanges;
      end;
      emAddVertex  : begin
        HideChanges;
        with EditedItem do begin
          EditedItem.AddVertex(FNewVtx,ViewToSchema(FNewVtxPos));
          EditedItem.ActiveVertex := FNewVtx+1;
          if not(IsUpdating) then BeginUpdate;
            Vertex[ActiveVertex] := StoO(ViewToSchemaRound(Point(X,Y)));
        end;
        DrawChanges;
        FMouseMode := emMoveVertex;
      end;
      emMoveLeftCP : begin
        HideChanges;
        with EditedItem do begin
          if not(IsUpdating) then BeginUpdate;
          setLeftCP(ActiveVertex,StoO(ViewToSchema(Point(X,Y))));
        end;
        DrawChanges;
      end;
      emMoveRightCP : begin
        HideChanges;
        with EditedItem do begin
          if not(IsUpdating) then BeginUpdate;
          setRightCP(ActiveVertex,StoO(ViewToSchema(Point(X,Y))));
        end;
        DrawChanges;
      end;
    end;//case Mode
  end;

  FLastPos    := Point(X,Y);
  if s<>TWinControl(FOwner).Hint then
    TWinControl(FOwner).Hint:=s;
end;

procedure    TdeCustomObjController.MouseUp(  Button:TMouseButton;
                                          Shift:TShiftState;
                                              X,Y:Integer);
begin
  if Button<>FButton then exit;
  with FOwner do begin
    case FMouseMode of
      emNone : begin
      end;
      emAddVertex,
      emMoveVertex,
      emMoveLeftCP,
      emMoveRightCP : begin
        if (EditedItem.IsUpdating) then begin
          EditedItem.EndUpdate;
          UpdateObjectView(FEditItem);
          DrawChanges;
        end;
      end;
    end;
    if (FMouseMode <> emNone) then
      FOwner.EndUpdate;
  end;
  FMouseMoved := false;
  FMouseMode  := emNone;
  FLastPos    := Point(X,Y);
end;


type
  TViewSchema = class(TdeSchema)
  private
    FOwner     : TdeSchemaView;
  protected
    procedure    NotifyNewLayer(aLayer:TdeSchemaLayer);override;
    procedure    NotifyDeleteLayer(aLayer:TdeSchemaLayer);override;
    procedure    NotifyLayerChanged(aLayer:TdeSchemaLayer);override;
    procedure    NotifyNewShortcut(aShortcut:TdeSchemaShortcut);override;
    procedure    NotifyDeleteShortcut(aShortcut:TdeSchemaShortcut);override;
    procedure    NotifyShortcutChanged(aShortcut:TdeSchemaShortcut);override;
    procedure    NotifyNewItem(anItem:TdeSchemaObject);override;
    procedure    NotifyDeleteItem(anItem:TdeSchemaObject);override;
    procedure    NotifyItemChanging(anItem:TdeSchemaObject);override;
    procedure    NotifyItemChanged(anItem:TdeSchemaObject;
                                ChangedProps:TdeObjectChangedProps);override;
    procedure    NotifyChanged;override;
  public
    constructor  Create(anOwner:TdeSchemaView);
    destructor   Destroy;override;
  end;


{TViewSchema}
constructor  TViewSchema.Create(anOwner:TdeSchemaView);
begin
  inherited Create;
  FOwner   := anOwner;
end;

destructor   TViewSchema.Destroy;
begin
  inherited Destroy;
end;

procedure    TViewSchema.NotifyNewLayer(aLayer:TdeSchemaLayer);
begin
  FOwner.SchemaNewLayer(aLayer);
end;

procedure    TViewSchema.NotifyDeleteLayer(aLayer:TdeSchemaLayer);
begin
  FOwner.SchemaDelLayer(aLayer);
end;

procedure    TViewSchema.NotifyLayerChanged(aLayer:TdeSchemaLayer);
begin
  FOwner.SchemaLayerChanged(aLayer);
end;

procedure    TViewSchema.NotifyNewShortcut(aShortcut:TdeSchemaShortcut);
begin
  FOwner.SchemaNewShortcut(aShortcut);
end;

procedure    TViewSchema.NotifyDeleteShortcut(aShortcut:TdeSchemaShortcut);
begin
  FOwner.SchemaDelShortcut(aShortcut);
end;

procedure    TViewSchema.NotifyShortcutChanged(aShortcut:TdeSchemaShortcut);
begin
  FOwner.SchemaShortcutChanged(aShortcut);
end;

procedure    TViewSchema.NotifyNewItem(anItem:TdeSchemaObject);
begin
  FOwner.SchemaNewItem(anItem);
end;

procedure    TViewSchema.NotifyDeleteItem(anItem:TdeSchemaObject);
begin
  FOwner.SchemaDelItem(anItem);
end;

procedure    TViewSchema.NotifyItemChanging(anItem:TdeSchemaObject);
begin
  FOwner.SchemaItemChanging(anItem);
end;

procedure    TViewSchema.NotifyItemChanged(anItem:TdeSchemaObject;
                                     ChangedProps:TdeObjectChangedProps);
begin
  FOwner.SchemaItemChanged(anItem,ChangedProps);
end;

procedure    TViewSchema.NotifyChanged;
begin
  FOwner.SchemaChanged;
end;


type
  TdeRefreshThread = class(tThread)
  protected
    FWnd  : hWnd;
    FPrepareTick : integer;
    procedure   Execute;override;
  public
    constructor Create(aWnd : hWnd;
                       aPrepareTick : integer);
    destructor  Destroy;override;
  end;


constructor TdeRefreshThread.Create(aWnd : hWnd;aPrepareTick : integer);
begin
  FWnd  := aWnd;
  FPrepareTick := aPrepareTick;
  inherited Create(false);
end;

destructor  TdeRefreshThread.Destroy;
begin
  inherited Destroy;
end;

procedure   TdeRefreshThread.Execute;
begin
  Priority := tpLowest;
  FreeOnTerminate := true;
  sleep(ENHANCEDVIEWDELAY);
  PostMessage(FWnd,DE_ENHANCEDVIEW,0,FPrepareTick);
end;




{TdeSchemaView}

constructor  TdeSchemaView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDeShiftState     := [];

  ParentColor       := false;
  Color             := clWindow;
  DoubleBuffered    := false;
  M2D_Identity(FConvertion);
  M2D_Identity(FConvToBuffer);

  FOptions          := [voGrid,voSmoothing];
  FRulerStyle       := rsWordlike;
  FScrollbars       := svsAuto;
  FViewCoorninates  := vcNone;

  ControlStyle      := ControlStyle + [csReplicatable,csOpaque];
  ControlStyle      := ControlStyle - [csClickEvents];
  
  FDeleting         := false;

  FSBTracking       := false;
  //
  FScale            := 1.0;
  
  CreateBuffers;

  FSchema           := TViewSchema.Create(Self);
  //
  FDelayTimer          := TTimer.Create(nil);
  FDelayTimer.Interval := cMoveMapDelay;
  FDelayTimer.OnTimer  := DelayTimerOnTimer;
  FDelayTimer.Enabled  := true;

  FKeyTimer         := TTimer.Create(nil);
  FKeyTimer.OnTimer := KeyRepeat;
  FKeyTimer.Enabled := false;
  FKeyCode          := 0;
  //
  FOffset           := Point2D(0,0);
  //
  FUpdateLock       := 0;
  FNeedUpdate       := false;
  FNeedRefresh      := false;
  //
  FChangesShown     := false;
  FSelected         := TdeSchemaObjectList.Create;
  FFocused          := nil;
  FEditItem         := nil;
  FActiveShortcut   := nil;

  //
  FMode             := vmBrowse;
  FBrowseCtlr       := TdeBrowseController.Create(Self);
  FEditCtlr         := TdeEditController.Create(Self);
  FCustomObjCtlr    := TdeCustomObjController.Create(Self);
  FController       := FBrowseCtlr;
  //
  UpdateConvertion;
  UpdateConvToBuffer;

  FOnFocusObject  := nil;
  FOnDeleteObject := nil;
  FOnObjectChanged:= nil;
end;

destructor   TdeSchemaView.Destroy;
begin
  FDeleting := true;
  FBrowseCtlr.Free;
  FEditCtlr.Free;
  FCustomObjCtlr.Free;
  FDelayTimer.Free;
  FKeyTimer.Free;
  FSchema.Free;
  DestroyBuffers;
  FSelected.Free;
  inherited Destroy;
end;

procedure    TdeSchemaView.CreateBuffers;
begin
  if (voSmoothing in Options) then begin
    FBuffer1               := tBitmap.Create;
    FBuffer1.PixelFormat   := pf24bit;
    FBuffer1.IgnorePalette := true;
    FBuffer1.HandleType    := bmDIB;
    FBuffer1.Width         := _ScaleK * Screen.Width;
    FBuffer1.Height        := _ScaleK * Screen.Height;
  end
  else
    FBuffer1               := nil;

  FPrepareTick           := 0;

  FBuffer3               := tBitmap.Create;
  FBuffer3.PixelFormat   := pf24bit;
  FBuffer3.IgnorePalette := true;
  FBuffer3.HandleType    := bmDIB;
  FBuffer3.Width         := 3*Screen.Width;
  FBuffer3.Height        := 3*Screen.Height;

  FHash                  := TdeSchemaViewHash.Create(Rect(0,0,FBuffer3.Width,FBuffer3.Height),
                                                     30,30);

  FTopRulerBuf           := tBitmap.Create;
  FTopRulerBuf.PixelFormat   := pf8bit;
  FTopRulerBuf.IgnorePalette := true;
  FTopRulerBuf.HandleType    := bmDIB;
  FTopRulerBuf.Width   := FBuffer3.Width;
  FTopRulerBuf.Height  := cRulerSize;

  FLeftRulerBuf        := tBitmap.Create;
  FLeftRulerBuf.PixelFormat   := pf8bit;
  FLeftRulerBuf.IgnorePalette := true;
  FLeftRulerBuf.HandleType    := bmDIB;
  FLeftRulerBuf.Width  := cRulerSize;
  FLeftRulerBuf.Height := FBuffer3.Height;

  FTopLeftBuf          := tBitmap.Create;
  FTopLeftBuf.PixelFormat   := pf8bit;
  FTopLeftBuf.IgnorePalette := true;
  FTopLeftBuf.HandleType    := bmDIB;
  FTopLeftBuf.Height   := cRulerSize;
  FTopLeftBuf.Width    := cRulerSize;
end;

procedure    TdeSchemaView.DestroyBuffers;
begin
  FHash.Free;
  if FBuffer1 <> nil then
    FBuffer1.Free;
  FBuffer3.Free;
  FTopRulerBuf.Free;
  FLeftRulerBuf.Free;
  FTopLeftBuf.Free;
end;

procedure    TdeSchemaView.ClearBuffer(aBuffer:TBitmap);
var
  Y     : integer;
  pSL   : pByteArray;
begin
  for y := 0 to aBuffer.Height-1 do begin
    pSL := aBuffer.ScanLine[y];
    fillChar(pSL^,aBuffer.Width*3,255);
  end;
end;

procedure RScaleSchedule(Ymin,Ymax : double;
                         var Yup,Ydown,YFirst,Step : double; MultiPlay: Integer=1);
var
  Range : double;
  K     : integer;
  NR    : double;
begin
  Range := Ymax-Ymin;
  if (Range < 0) then exit;
  if (Range = 0) then begin
    Ymin  := Ymax-1;
    Ymax  := Ymax+1;
    Range := Ymax-Ymin;
  end;
  K := 1;
  NR := Range;
  while (NR > 10) do begin
    NR := NR/10;
    K  := K+1;
  end;
  while (NR <= 1) do begin
    NR := NR*10;
    K  := K-1;
  end;

  if (NR > 1)and(NR <= 2) then
    Step := 0.5/MultiPlay
  else if (NR > 2)and(NR <= 5) then
    Step := 1/MultiPlay
  else if (NR > 5)and(NR <= 10) then
    Step := 2/MultiPlay;

  while (K > 1) do begin
    Step := Step*10;
    K := K-1;
  end;
  while (K < 1) do begin
    Step := Step/10;
    K := K+1;
  end;

  if (Ymin < 0)and(Ymax > 0) then
    YFirst := 0
  else if (Ymin >= 0) then
    YFirst := (trunc(Ymin/Step)+1)*Step
  else
    YFirst := (trunc(Ymax/Step)-1)*Step;
  Ydown := YFirst;
  while (Ydown > Ymin) do
    Ydown := Ydown-Step;
  Yup := YFirst;
  while (Yup < Ymax) do
    Yup := Yup+Step;
end;

procedure    TdeSchemaView.UpdateHash;
var
  anItem : TdeSchemaObject;
  R      : TRect;
begin
  FHash.ClearItems;
  anItem := FSchema.First;
  while (anItem <> nil) do
  begin
    if (anItem.IGroup=nil)
       and anItem.Visible
    then
    begin
      R := anItem.getViewRect(FBuffer3.Canvas,FConvToBuffer);
      FHash.Add(R,anItem);
    end;
    anItem := FSchema.Next(anItem);
  end;
end;



var
  StTick : integer;


procedure    TdeSchemaView.PrepareBuffers;
var
  _Conv  : TMatrix2D;
begin
  //отрисовать схему в буфер
  StTick := integer(getTickCount);

  // очистить буфер 3
  ClearBuffer(FBuffer3);  

  //отрисовать нормальную схему в буфер 3
  M2D_Identity(_Conv);
  _Conv := M2D_Shift(_Conv,-Offset.X,-Offset.Y);
  _Conv := M2D_Scale(_Conv,Scale,-Scale);
  _Conv := M2D_Shift(_Conv,FBuffer3.Width/2,FBuffer3.Height/2);
  if (voGrid in Options) then begin
    FBuffer3.Canvas.Pen.Color := clLtGray;
    FBuffer3.Canvas.Pen.Style := psSolid;
    FBuffer3.Canvas.Pen.Width := 1;
    doDrawGrid(FBuffer3,_Conv);
  end;
  FSchema.PaintTo(FBuffer3.Canvas,_Conv);

  StTick := integer(getTickCount)-StTick;
  StTick := integer(getTickCount);

  // отрисовка буферов линеек
  if (FRulerStyle = rsWordlike) then
  begin
    PrepareWordlikeRullers;
    PrepareTopLeftPanel;
  end
  else if (FRulerStyle = rsScholl) then
  begin
    PrepareSchollRullers;
    PrepareTopLeftPanel;
  end;

  //UpdateHash;

end;

procedure    DrawFocuseRect(anObject:TdeSchemaObject;aCanvas:tCanvas;StoC:TMatrix2D);
var
  P1,P2,P3,P4         : TPoint2D;
  iP1,iP2,iP3,iP4     : TPoint;
  iP12,iP23,iP34,iP41 : TPoint;
  iPR                 : TPoint;
  d                   : integer;
begin
  if getGraphicsMode(aCanvas.Handle) = GM_ADVANCED
  then
    d := 0
  else
    d := 1;

  anObject.getBoundsPoints(aCanvas,StoC,P1,P2,P3,P4);
  iP1 := P2DToP(P1);
  iP2 := P2DToP(P2);
  iP3 := P2DToP(P3);
  iP4 := P2DToP(P4);

  iP12:= Point((iP1.X+iP2.X)div 2,(iP1.Y+iP2.Y)div 2);
  iP23:= Point((iP2.X+iP3.X)div 2,(iP2.Y+iP3.Y)div 2);
  iP34:= Point((iP3.X+iP4.X)div 2,(iP3.Y+iP4.Y)div 2);
  iP41:= Point((iP4.X+iP1.X)div 2,(iP4.Y+iP1.Y)div 2);
  //по углам
  aCanvas.Pen.Color   := clBlack;
  aCanvas.Pen.Width   := 1;
  aCanvas.Pen.Style   := psSolid;
  aCanvas.Pen.Mode    := pmCOPY;
  aCanvas.Brush.Color := clWhite;
  aCanvas.Brush.Style := bsSolid;

  aCanvas.Ellipse(iP1.X - cPointSize,iP1.Y - cPointSize,
                  iP1.X + cPointSize + d,iP1.Y + cPointSize + d);
  aCanvas.Ellipse(iP2.X - cPointSize,iP2.Y - cPointSize,
                  iP2.X + cPointSize + d,iP2.Y + cPointSize + d);
  aCanvas.Ellipse(iP3.X - cPointSize,iP3.Y - cPointSize,
                  iP3.X + cPointSize + d,iP3.Y + cPointSize + d);
  aCanvas.Ellipse(iP4.X - cPointSize,iP4.Y - cPointSize,
                  iP4.X + cPointSize + d,iP4.Y + cPointSize + d);

  if not(ocoResizable in anObject.ClassOptions)
  then begin
    aCanvas.MoveTo(iP1.X - cPointSize + 1,      iP1.Y - cPointSize + 1);
    aCanvas.LineTo(iP1.X + cPointSize - 1 + d,  iP1.Y + cPointSize - 1 + d);
    aCanvas.MoveTo(iP1.X + cPointSize - 2 + d,  iP1.Y - cPointSize + 1);
    aCanvas.LineTo(iP1.X - cPointSize + 0,      iP1.Y + cPointSize - 1 + d);

    aCanvas.MoveTo(iP2.X - cPointSize + 1,      iP2.Y - cPointSize + 1);
    aCanvas.LineTo(iP2.X + cPointSize - 1 + d,  iP2.Y + cPointSize - 1 + d);
    aCanvas.MoveTo(iP2.X + cPointSize - 2 + d,  iP2.Y - cPointSize + 1);
    aCanvas.LineTo(iP2.X - cPointSize + 0,      iP2.Y + cPointSize - 1 + d);

    aCanvas.MoveTo(iP3.X - cPointSize + 1,      iP3.Y - cPointSize + 1);
    aCanvas.LineTo(iP3.X + cPointSize - 1 + d,  iP3.Y + cPointSize - 1 + d);
    aCanvas.MoveTo(iP3.X + cPointSize - 2 + d,  iP3.Y - cPointSize + 1);
    aCanvas.LineTo(iP3.X - cPointSize + 0,      iP3.Y + cPointSize - 1 + d);

    aCanvas.MoveTo(iP4.X - cPointSize + 1,      iP4.Y - cPointSize + 1);
    aCanvas.LineTo(iP4.X + cPointSize - 1 + d,  iP4.Y + cPointSize - 1 + d);
    aCanvas.MoveTo(iP4.X + cPointSize - 2 + d,  iP4.Y - cPointSize + 1);
    aCanvas.LineTo(iP4.X - cPointSize + 0,      iP4.Y + cPointSize - 1 + d);
  end;
  // поворот
  if (ocoRotatable in anObject.ClassOptions)
     and getRotationMark(iP12,iP23,iP34,iP41,iPR)
  then begin
    aCanvas.Pen.Color   := clBlack;
    aCanvas.Pen.Width   := 1;
    aCanvas.Pen.Style   := psSolid;
    aCanvas.Brush.Color := clLime;
    aCanvas.MoveTo(iP12.X,iP12.Y);
    aCanvas.LineTo(iPR.X,iPR.Y);
    aCanvas.Ellipse(iPR.X - cPointSize,iPR.Y - cPointSize,
                    iPR.X + cPointSize + d,iPR.Y + cPointSize + d);
  end;
  if (ocoResizable in anObject.ClassOptions)
  then begin
    // по центру сторон
    aCanvas.Pen.Color   := clBlack;
    aCanvas.Pen.Width   := 1;
    aCanvas.Pen.Style   := psSolid;
    aCanvas.Brush.Color := clWhite;
    aCanvas.Ellipse(iP12.X - cPointSize, iP12.Y - cPointSize,
                    iP12.X + cPointSize + d, iP12.Y + cPointSize + d);
    aCanvas.Ellipse(iP23.X - cPointSize, iP23.Y - cPointSize,
                    iP23.X + cPointSize + d, iP23.Y + cPointSize + d);
    aCanvas.Ellipse(iP34.X - cPointSize, iP34.Y - cPointSize,
                    iP34.X + cPointSize + d, iP34.Y + cPointSize + d);
    aCanvas.Ellipse(iP41.X - cPointSize, iP41.Y - cPointSize,
                    iP41.X + cPointSize + d, iP41.Y + cPointSize + d);
  end;
end;

function     _IsFocuseRectPoint(anObject:TdeSchemaObject;
                                  aPoint:TPoint;
                               var pntID:integer;
                                 aCanvas:TCanvas;
                                    StoC:TMatrix2D):TdeBoundsPointClass;
var
  P1,P2,P3,P4         : TPoint2D;
  iP1,iP2,iP3,iP4     : TPoint;
  iP12,iP23,iP34,iP41 : TPoint;
  iPR     : TPoint;
  function Equal(P1,P2:TPoint):boolean;
  begin
    Result := (abs(P2.X-P1.X)<cPointSize)and(abs(P2.Y-P1.Y)<cPointSize);
  end;
begin
  Result := bpcUnknown;
  if not(ocoResizable in anObject.ClassOptions)
    and not(ocoRotatable in anObject.ClassOptions)
  then
    exit;

  anObject.getBoundsPoints(aCanvas,StoC,P1,P2,P3,P4);
  iP1 := P2DtoP(P1);
  iP2 := P2DtoP(P2);
  iP3 := P2DtoP(P3);
  iP4 := P2DtoP(P4);
  iP12 := P2DtoP(Point2D((P1.X+P2.X)/2,(P1.Y+P2.Y)/2));
  iP23 := P2DtoP(Point2D((P2.X+P3.X)/2,(P2.Y+P3.Y)/2));
  iP34 := P2DtoP(Point2D((P3.X+P4.X)/2,(P3.Y+P4.Y)/2));
  iP41 := P2DtoP(Point2D((P4.X+P1.X)/2,(P4.Y+P1.Y)/2));
  if (ocoResizable in anObject.ClassOptions)
  then begin
    pntID  :=-1;
    if Equal(aPoint,iP1) then
      Result := bpcTopLeft
    else if Equal(aPoint,iP12)  then
      Result := bpcTop
    else if Equal(aPoint,iP2)   then
      Result := bpcTopRight
    else if Equal(aPoint,iP23)  then
      Result := bpcRight
    else if Equal(aPoint,iP3)   then
      Result := bpcBottomRight
    else if Equal(aPoint,iP34)  then
      Result := bpcBottom
    else if Equal(aPoint,iP4)   then
      Result := bpcBottomLeft
    else if Equal(aPoint,iP41)  then
      Result := bpcLeft;
  end;
  if (ocoRotatable in anObject.ClassOptions)
     and getRotationMark(iP12,iP23,iP34,iP41,iPR)
     and (Equal(aPoint,iPR))
  then
    Result := bpcRotation;
end;

procedure    TdeSchemaView.RefreshEnhancedBuffer;
var
  anItem    : TdeSchemaObject;
  _Conv     : TMatrix2D;
begin
  UpdateConvertion;
  UpdateConvToBuffer;

  ClearBuffer(FBuffer1);
  //отрисовать увеличенную схему в буфер 1
  M2D_Identity(_Conv);
  _Conv := M2D_Shift(_Conv,-Offset.X,-Offset.Y);
  _Conv := M2D_Scale(_Conv,_ScaleK * Scale,-_ScaleK * Scale);
  _Conv := M2D_Shift(_Conv,FBuffer1.Width/2,FBuffer1.Height/2);

  if (voGrid in Options) then begin
    FBuffer1.Canvas.Pen.Color := clLtGray;
    FBuffer1.Canvas.Pen.Style := psSolid;
    FBuffer1.Canvas.Pen.Width := _ScaleK;
    doDrawGrid(FBuffer1,_Conv);
  end;

  anItem := FSchema.First;
  while anItem <> nil do begin
    if (anItem.IGroup = nil)
       and anItem.Visible
    then
      anItem.PaintTo(FBuffer1.Canvas,_Conv,2);
    anItem := FSchema.Next(anItem);
  end;
end;

procedure rStretchBlt(bmpDest:TBitmap;
                      nXOriginDest:integer; // x-coordinate of upper-left corner of dest. rect.
                      nYOriginDest:integer; // y-coordinate of upper-left corner of dest. rect.
                      nWidthDest:integer;   // width of destination rectangle
                      nHeightDest:integer;  // height of destination rectangle
                      bmpSrc:TBitmap;       // handle of source device context
                      nXOriginSrc:integer;  // x-coordinate of upper-left corner of source rectangle
                      nYOriginSrc:integer);  // y-coordinate of upper-left corner of source rectangle
var
  x,y            : integer;
  i,j1,j2        : integer;
  ScL1,ScL2,rScL : pByteArray;
begin
//DONE: Исправление проблем с крайним пикселом на карте       
//  y := 1;
  y := 0;
  while y < nHeightDest  do
  begin
    ScL1 := bmpSrc.ScanLine[nYOriginSrc+2*y];
    ScL2 := bmpSrc.ScanLine[nYOriginSrc+2*y+1];
    rScL := bmpDest.ScanLine[nYOriginDest+y+1];
//    x := 1;
    x := 0;
    while x < nWidthDest  do
    begin
      j1 := nXOriginSrc*3+x*6;
      j2 := (nXOriginDest+x)*3;
      for i := 0 to 2 do        
        rScL[j2+i] :=(ScL1[j1+i+3]+ScL1[j1+i]+ScL2[j1+i+3]+ScL2[j1+i]) div 4;
      inc(x,1);
    end;
    inc(y,1);
  end;
end;

procedure    TdeSchemaView.ApplyEnhancedBuffer;
var
  VR1,VR3   : TRect;
  VR3Offset : TPoint;
begin
  VR3 := ViewRect;
  VR3Offset := Point(((FBuffer3.Width-(VR3.Right+VR3.Left))div 2)+FViewDelta.X,
                     ((FBuffer3.Height-(VR3.Bottom+VR3.Top))div 2)+FViewDelta.Y);
  OffsetRect(VR3,VR3Offset.X,VR3Offset.Y);
  VR1 := VR3;

  OffsetRect(VR1,-((VR3.Right+VR3.Left) div 2),-((VR3.Bottom+VR3.Top) div 2));
  VR1.Left   := VR1.Left * _ScaleK;
  VR1.Top    := VR1.Top * _ScaleK;
  VR1.Right  := VR1.Right * _ScaleK;
  VR1.Bottom := VR1.Bottom * _ScaleK;
  OffsetRect(VR1,(FBuffer1.Width div 2),(FBuffer1.Height div 2));
  {
  //TODO: Заменить на новую процедуру
  setStretchBltMode(FBuffer3.Canvas.Handle,HALFTONE);
  setBrushOrgEx(FBuffer3.Canvas.Handle,0,0,nil);
  StretchBlt(FBuffer3.Canvas.Handle,
             VR3.Left+1,VR3.Top+1,VR3.Right-VR3.Left,VR3.Bottom-VR3.Top,
             FBuffer1.Canvas.Handle,
             VR1.Left,VR1.Top,VR1.Right-VR1.Left,VR1.Bottom-VR1.Top,
             SRCCOPY);
  {}
  rStretchBlt(FBuffer3,
              VR3.Left+1,VR3.Top+1,VR3.Right-VR3.Left,VR3.Bottom-VR3.Top,
//              VR3.Left,VR3.Top,VR3.Right-VR3.Left,VR3.Bottom-VR3.Top,
              FBuffer1,
              VR1.Left,VR1.Top);
  {}
end;

procedure    TdeSchemaView.RefreshViewBuffer;
var
  aShortcut : TdeSchemaShortcut;
  i         : integer;
begin
  UpdateConvertion;
  UpdateConvToBuffer;
  UpdateHash;
  {}
  for i := 0 to FSchema.ShortCutsCount-1 do
  begin
    aShortcut := FSchema.Shortcut[i];
    if (aShortcut.LinkedObject <> nil)
       and(aShortcut.LinkedObject.Visible)
    then
      aShortcut.PaintTo(FBuffer3.Canvas,FConvToBuffer);
  end;
  //отрисовать выделенные в буфер 3
  if IsEditing then begin
    //отрисовать редактируемый объект
  end
  else begin
    //отрисовать рамки для выделенных объектов
    if (Mode = vmEdit) then
    begin
      for i := 0 to SelectedCount-1 do
        DrawFocuseRect(Selected[i],FBuffer3.Canvas,FConvToBuffer);

      if (SelectedCount=1)and(FViewCoorninates in [vcPoint,vcBoth]) then
        if Selected[0] is TdeSimpleCurve then
            TdeSimpleCurve(Selected[0]).PaintChanges(FBuffer3.Canvas,FConvToBuffer,vcBoth);
    end
    else
      DrawChanges;
  end;
end;

procedure    TdeSchemaView.CalcGridParams(var Xdown,Xup,XFirst,XStep : double;
                                          var Ydown,Yup,YFirst,YStep : double);
var
  R                 : TRect;
  CX,CY             : integer;
  H,W               : integer;
  P1,P2             : TPoint2D;
begin
  R  := ViewRect;
  CX := (R.Right + R.Left)div 2;
  CY := (R.Bottom + R.Top)div 2;
  W  := Screen.Width;
  H  := Screen.Height;
  P1 := PtoP2DEx(Point(Cx-(W div 6),CY -(2*H div 9)),FRConvertion);
  P2 := PtoP2DEx(Point(Cx+(W div 6),CY +(2*H div 9)),FRConvertion);
  RScaleSchedule(P1.X,P2.X,Xup,Xdown,XFirst,XStep,4);
  RScaleSchedule(P2.Y,P1.Y,Yup,Ydown,YFirst,YStep,4);
end;

procedure    TdeSchemaView.doDrawGrid(aBuffer:TBitmap;StoC:TMatrix2D);
var
  R                 : TRect;
  Xdown,Xup,XFirst  : double;
  XStep             : double;
  Ydown,Yup,YFirst  : double;
  YStep             : double;
  P1                : TPoint2D;
  iP1,iP2           : TPoint;
  CtoS              : TMatrix2D;
begin
  CtoS := M2D_Reverse(StoC);
  with aBuffer,Canvas do
  begin
    //отрисовать сетку
    CalcGridParams(Xdown,Xup,XFirst,XStep,Ydown,Yup,YFirst,YStep);

    R := Rect(0,0,aBuffer.Width,aBuffer.Height);

    P1  := Point2D(XFirst,0);
    iP1 := P2DtoPEx(P1,StoC);
    while (iP1.X > 0) do
    begin
      iP1.Y := R.Top;
      iP2 := Point(iP1.X,R.Bottom);
      MoveTo(iP1.X,iP1.Y);
      LineTo(iP2.X,iP2.Y);
      P1.X := P1.X-XStep;
      iP1 := P2DtoPEx(P1,StoC);
    end;
    P1  := Point2D(XFirst+XStep,0);
    iP1 := P2DtoPEx(P1,StoC);
    while (iP1.X < Width) do
    begin
      iP1.Y := R.Top;
      iP2 := Point(iP1.X,R.Bottom);
      MoveTo(iP1.X,iP1.Y);
      LineTo(iP2.X,iP2.Y);
      P1.X := P1.X+XStep;
      iP1 := P2DtoPEx(P1,StoC);
    end;
    P1  := Point2D(0,YFirst);
    iP1 := P2DtoPEx(P1,StoC);
    while (iP1.Y > 0) do
    begin
      iP1.X := R.Left;
      iP2 := Point(R.Right,iP1.Y);
      MoveTo(iP1.X,iP1.Y);
      LineTo(iP2.X,iP2.Y);
      P1.Y := P1.Y+YStep;
      iP1 := P2DtoPEx(P1,StoC);
    end;
    P1  := Point2D(0,YFirst+YStep);
    iP1 := P2DtoPEx(P1,StoC);
    while (iP1.Y < Height) do
    begin
      iP1.X := R.Left;
      iP2 := Point(R.Right,iP1.Y);
      MoveTo(iP1.X,iP1.Y);
      LineTo(iP2.X,iP2.Y);
      P1.Y := P1.Y-YStep;
      iP1 := P2DtoPEx(P1,StoC);
    end;
  end;
end;

procedure    TdeSchemaView.UpdateObjectView(anItem:TdeSchemaObject);
begin
  //TODO: додумать алгоритм обновления отдельного объекта
  UpdateView;
end;

procedure    TdeSchemaView.PrepareWordlikeRullers;
var
  R                 : TRect;
  Xdown,Xup,XFirst  : double;
  XStep,XStepic     : double;
  Ydown,Yup,YFirst  : double;
  YStep,YStepic     : double;
  P1,P2             : TPoint2D;
  iP1,iP2           : TPoint;
  H,W               : integer;
  CX,CY             : integer;
  TS                : TSize;
  s                 : string;
  L,D               : integer;
  X                 : double;
  dX,dY             : integer;
begin
  R  := ViewRect;
  CX := (R.Right + R.Left) div 2;
  CY := (R.Bottom + R.Top) div 2;
  W  := Screen.Width;
  H  := Screen.Height;
  P1 := ViewToSchema(Point(Cx-(W div 6),CY -(2*H div 9)));
  P2 := ViewToSchema(Point(Cx+(W div 6),CY +(2*H div 9)));
  RScaleSchedule(P1.X,P2.X,Xup,Xdown,XFirst,XStep);
  RScaleSchedule(P2.Y,P1.Y,Yup,Ydown,YFirst,YStep);
  XStepic := XStep/2;
  YStepic := YStep/2;
  with FTopRulerBuf do
  begin
    R := Rect(0,0,Width,Height);
    dX := (Width div 2) - CX;
    dY := (Height div 2) - CY;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clBtnFace;
    Canvas.Pen.Width := 1;
    Canvas.Rectangle(R);
    InflateRect(R,0,-(cRulerSize*9 div 40));
    Canvas.Brush.Color := Self.Color;
    Canvas.Pen.Color := Self.Color;
    Canvas.Rectangle(R);

    Canvas.Pen.Color := Self.Font.Color;
    Canvas.Font.Name  := 'Times New Roman';
    Canvas.Font.Color := Self.Font.Color;
    Canvas.Font.Height:=-10;

    D := 0;
    X := XStep;
    while (X < 1) do begin
      X := X*10;
      D := D+1;
    end;
    L := D+6;

    P1  := Point2D(XFirst,0);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.X > 0) do
    begin
      iP1.Y := R.Top;
      iP2 := Point(iP1.X,R.Bottom);
      Str(P1.X:L:D,s);
      s := trim(s);
      TS:=Canvas.TextExtent(s);
      Canvas.TextOut(iP1.X-(TS.cx div 2),(iP2.Y+iP1.Y-TS.cy)div 2,s);
      P1.X := P1.X-XStep;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    P1  := Point2D(XFirst+XStep,0);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.X < Width) do
    begin
      iP1.Y := R.Top;
      iP2 := Point(iP1.X,R.Bottom);
      Str(P1.X:L:D,s);
      s := trim(s);
      TS:=Canvas.TextExtent(s);
      Canvas.TextOut(iP1.X-(TS.cx div 2),(iP2.Y+iP1.Y-TS.cy)div 2,s);
      P1.X := P1.X+XStep;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    H   := (R.Bottom-R.Top) div 2;
    P1  := Point2D(XFirst-XStepic,0);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.X > 0) do
    begin
      iP1.Y := R.Top+((R.Bottom-R.Top-H) div 2);
      iP2 := Point(iP1.X,iP1.Y+H);
      Canvas.MoveTo(iP1.X,iP1.Y);
      Canvas.LineTo(iP2.X,iP2.Y);
      P1.X := P1.X-2*XStepic;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    P1  := Point2D(XFirst+XStepic,0);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.X < Width) do
    begin
      iP1.Y := R.Top+((R.Bottom-R.Top-H) div 2);
      iP2 := Point(iP1.X,iP1.Y+H);
      Canvas.MoveTo(iP1.X,iP1.Y);
      Canvas.LineTo(iP2.X,iP2.Y);
      P1.X := P1.X+2*XStepic;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    XStepic := XStepic/2;
    H   := (R.Bottom-R.Top) div 4;
    P1  := Point2D(XFirst-XStepic,0);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.X > 0) do
    begin
      iP1.Y := R.Top+((R.Bottom-R.Top-H) div 2);
      iP2 := Point(iP1.X,iP1.Y+H);
      Canvas.MoveTo(iP1.X,iP1.Y);
      Canvas.LineTo(iP2.X,iP2.Y);
      P1.X := P1.X-2*XStepic;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    P1  := Point2D(XFirst+XStepic,0);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.X < Width) do
    begin
      iP1.Y := R.Top+((R.Bottom-R.Top-H) div 2);
      iP2 := Point(iP1.X,iP1.Y+H);
      Canvas.MoveTo(iP1.X,iP1.Y);
      Canvas.LineTo(iP2.X,iP2.Y);
      P1.X := P1.X+2*XStepic;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
  end;

  with FLeftRulerBuf do
  begin
    R := Rect(0,0,Width,Height);
    dX := (Width div 2) - CX;
    dY := (Height div 2) - CY;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clBtnFace;
    Canvas.Pen.Width := 1;
    Canvas.Rectangle(R);
    InflateRect(R,-(cRulerSize*9 div 40),0);
    Canvas.Brush.Color := Self.Color;
    Canvas.Pen.Color := Self.Color;
    Canvas.Rectangle(R);

    D := 0;
    X := YStep;
    while (X < 1) do begin
      X := X*10;
      D := D+1;
    end;
    L := D+6;

    Canvas.Pen.Color   := clBlack;
    Canvas.Font.Name   := 'Times New Roman';
    Canvas.Font.Color  := clBlack;
    Canvas.Font.Height := -10;
    {}
    P1  := Point2D(0,YFirst);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.Y > 0) do
    begin
      iP1.X := R.Left;
      iP2 := Point(R.Right,iP1.Y);
      Str(P1.Y:L:D,s);
      s := trim(s);
      TS:=Canvas.TextExtent(s);
      DrawRotatedText(Canvas,s,(iP1.X+iP2.X-TS.cy)div 2,iP1.Y+(TS.cx div 2),90);
      P1.Y := P1.Y+YStep;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    P1  := Point2D(0,YFirst-YStep);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.Y < Height) do
    begin
      iP1.X := R.Left;
      iP2 := Point(R.Right,iP1.Y);
      Str(P1.Y:L:D,s);
      s := trim(s);
      TS:=Canvas.TextExtent(s);
      DrawRotatedText(Canvas,s,(iP1.X+iP2.X-TS.cy)div 2,iP1.Y+(TS.cx div 2),90);
      P1.Y := P1.Y-YStep;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    W   := (R.Right-R.Left) div 2;
    P1  := Point2D(0,YFirst+YStepic);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.Y > 0) do
    begin
      iP1.X := R.Left+((R.Right-R.Left-W) div 2);
      iP2 := Point(iP1.X+W,iP1.Y);
      Canvas.MoveTo(iP1.X,iP1.Y);
      Canvas.LineTo(iP2.X,iP2.Y);
      P1.Y := P1.Y+2*YStepic;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    P1  := Point2D(0,YFirst-YStepic);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.Y < Height) do
    begin
      iP1.X := R.Left+((R.Right-R.Left-W) div 2);
      iP2 := Point(iP1.X+W,iP1.Y);
      Canvas.MoveTo(iP1.X,iP1.Y);
      Canvas.LineTo(iP2.X,iP2.Y);
      P1.Y := P1.Y-2*YStepic;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    YStepic := YStepic/2;
    W   := (R.Right-R.Left) div 4;
    P1  := Point2D(0,YFirst+YStepic);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.Y > 0) do
    begin
      iP1.X := R.Left+((R.Right-R.Left-W) div 2);
      iP2 := Point(iP1.X+W,iP1.Y);
      Canvas.MoveTo(iP1.X,iP1.Y);
      Canvas.LineTo(iP2.X,iP2.Y);
      P1.Y := P1.Y+2*YStepic;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    P1  := Point2D(0,YFirst-YStepic);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.Y < Height) do
    begin
      iP1.X := R.Left+((R.Right-R.Left-W) div 2);
      iP2 := Point(iP1.X+W,iP1.Y);
      Canvas.MoveTo(iP1.X,iP1.Y);
      Canvas.LineTo(iP2.X,iP2.Y);
      P1.Y := P1.Y-2*YStepic;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
  end;
end;

procedure    TdeSchemaView.PrepareSchollRullers;
var
  R                 : TRect;
  Xdown,Xup,XFirst  : double;
  XStep,XStepic     : double;
  Ydown,Yup,YFirst  : double;
  YStep,YStepic     : double;
  P1,P2             : TPoint2D;
  iP1,iP2           : TPoint;
  H,W               : integer;
  CX,CY             : integer;
  TS                : TSize;
  s                 : string;
  L,D               : integer;
  X                 : double;
  dX,dY             : integer;
  CStepic           : integer;
begin
  R  := ViewRect;
  CX := ((R.Right + R.Left) div 2);
  CY := ((R.Bottom + R.Top) div 2);
  W  := Screen.Width;
  H  := Screen.Height;
  P1 := ViewToSchema(Point(Cx-(W div 6),CY -(2*H div 9)));
  P2 := ViewToSchema(Point(Cx+(W div 6),CY +(2*H div 9)));
  RScaleSchedule(P1.X,P2.X,Xup,Xdown,XFirst,XStep);
  RScaleSchedule(P2.Y,P1.Y,Yup,Ydown,YFirst,YStep);
  with FTopRulerBuf do
  begin
    R := Rect(0,0,Width,Height);
    dX := (Width div 2) - CX;
    dY := (Height div 2) - CY;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clBtnFace;
    Canvas.Rectangle(R);
    InflateRect(R,0,-3);
    Canvas.Brush.Color := Self.Color;
    Canvas.Pen.Color := Self.Color;
    Canvas.Rectangle(R);

    Canvas.Pen.Color := clBlack;
    Canvas.Font.Name  := 'Times New Roman';
    Canvas.Font.Color := clBlack;
    Canvas.Font.Height:=-10;

    D := 0;
    X := XStep;
    while (X < 1) do begin
      X := X*10;
      D := D+1;
    end;
    L := D+6;

    P1  := Point2D(XFirst,0);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.X > 0) do
    begin
      iP1.Y := R.Top;
      iP2 := Point(iP1.X,R.Bottom);
      Str(P1.X:L:D,s);
      s := trim(s);
      TS:=Canvas.TextExtent(s);
      Canvas.TextOut(iP1.X-(TS.cx div 2),iP1.Y,s);
      P1.X := P1.X-XStep;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    P1  := Point2D(XFirst+XStep,0);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.X < Width) do
    begin
      iP1.Y := R.Top;
      iP2 := Point(iP1.X,R.Bottom);
      Str(P1.X:L:D,s);
      s := trim(s);
      TS:=Canvas.TextExtent(s);
      Canvas.TextOut(iP1.X-(TS.cx div 2),iP1.Y,s);
      P1.X := P1.X+XStep;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    XStepic := XStep/10;
    CStepic := 0;
    P1  := Point2D(XFirst,0);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.X > 0) do
    begin
      H := (R.Bottom-R.Top);
      case (CStepic mod 10) of
        0    : H   := 5*H div 12;
        5    : H   := 9*H div 24;
        else   H   := 3*H div 12;
      end;
      iP1.Y := R.Bottom-H;
      iP2 := Point(iP1.X,iP1.Y+H);
      Canvas.MoveTo(iP1.X,iP1.Y);
      Canvas.LineTo(iP2.X,iP2.Y);
      P1.X := P1.X-XStepic;
      inc(CStepic);
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    P1  := Point2D(XFirst+XStepic,0);
    CStepic := 1;
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.X < Width) do
    begin
      H := (R.Bottom-R.Top);
      case (CStepic mod 10) of
        0    : H   := 5*H div 12;
        5    : H   := 9*H div 24;
        else   H   := 3*H div 12;
      end;
      iP1.Y := R.Bottom-H;
      iP2 := Point(iP1.X,iP1.Y+H);
      Canvas.MoveTo(iP1.X,iP1.Y);
      Canvas.LineTo(iP2.X,iP2.Y);
      P1.X := P1.X+XStepic;
      inc(CStepic);
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
  end;

  with FLeftRulerBuf do
  begin
    R := Rect(0,0,Width,Height);
    dX := (Width div 2) - CX;
    dY := (Height div 2) - CY;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clBtnFace;
    Canvas.Rectangle(R);
    InflateRect(R,-3,0);
    Canvas.Brush.Color := Self.Color;
    Canvas.Pen.Color := Self.Color;
    Canvas.Rectangle(R);

    D := 0;
    X := YStep;
    while (X < 1) do begin
      X := X*10;
      D := D+1;
    end;
    L := D+6;

    Canvas.Pen.Color := clBlack;
    Canvas.Font.Name  := 'Times New Roman';
    Canvas.Font.Color := clBlack;
    Canvas.Font.Height:= -10;
    {}
    P1  := Point2D(0,YFirst);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.Y > 0) do
    begin
      iP1.X := R.Left;
      iP2 := Point(R.Right,iP1.Y);
      Str(P1.Y:L:D,s);
      s := trim(s);
      TS:=Canvas.TextExtent(s);
      DrawRotatedText(Canvas,s,iP1.X,iP1.Y+(TS.cx div 2),90);
      P1.Y := P1.Y+YStep;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    P1  := Point2D(0,YFirst-YStep);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.Y < Height) do
    begin
      iP1.X := R.Left;
      iP2 := Point(R.Right,iP1.Y);
      Str(P1.Y:L:D,s);
      s := trim(s);
      TS:=Canvas.TextExtent(s);
      DrawRotatedText(Canvas,s,iP1.X,iP1.Y+(TS.cx div 2),90);
      P1.Y := P1.Y-YStep;
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    YStepic := YStep/10;
    CStepic := 0;
    P1  := Point2D(0,YFirst);
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.Y > 0) do
    begin
      W := (R.Right-R.Left);
      case (CStepic mod 10) of
        0    : W   := 5*W div 12;
        5    : W   := 9*W div 24;
        else   W   := 3*W div 12;
      end;
      iP1.X := R.Right-W;
      iP2 := Point(iP1.X+W,iP1.Y);
      Canvas.MoveTo(iP1.X,iP1.Y);
      Canvas.LineTo(iP2.X,iP2.Y);
      P1.Y := P1.Y+YStepic;
      inc(CStepic);
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
    P1  := Point2D(0,YFirst-YStepic);
    CStepic := 1;
    iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    while (iP1.Y < Height) do
    begin
      W := (R.Right-R.Left);
      case (CStepic mod 10) of
        0    : W   := 5*W div 12;
        5    : W   := 9*W div 24;
        else   W   := 3*W div 12;
      end;
      iP1.X := R.Right-W;
      iP2 := Point(iP1.X+W,iP1.Y);
      Canvas.MoveTo(iP1.X,iP1.Y);
      Canvas.LineTo(iP2.X,iP2.Y);
      P1.Y := P1.Y-YStepic;
      inc(CStepic);
      iP1 := OffsetPoint(SchemaToView(P1),dX,dY);
    end;
  end;
end;

procedure    TdeSchemaView.PrepareTopLeftPanel;
var
  R : TRect;
begin
  with FTopLeftBuf do
  begin
    R := Rect(0,0,Width,Height);
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clBtnFace;
    Canvas.Rectangle(R);
    InflateRect(R,-(cRulerSize div 5),-(cRulerSize div 5));
    Frame3D(Canvas,R,clBtnShadow,clBtnHighlight,1);
    Frame3D(Canvas,R,clBtnHighlight,clBtnShadow,1);
  end;
end;

procedure    TdeSchemaView.UpdateViewRect;
begin
  FViewRect := ClientRect;

  if (FRulerStyle <> rsNone)
  then begin
    FViewRect.Left   := FViewRect.Left+cRulerSize;
    FViewRect.Top    := FViewRect.Top+cRulerSize;
  end;
  if (FViewRect.Left > FViewRect.Right)
     or(FViewRect.Top > FViewRect.Bottom)
  then
    FViewRect := Rect(0,0,0,0)
end;

procedure    TdeSchemaView.UpdateConvertion;
var
  R : TRect;
begin
  M2D_Identity(FConvertion);
  FConvertion := M2D_Shift(FConvertion,-Offset.X,-Offset.Y);
  FConvertion := M2D_Scale(FConvertion,Scale,-Scale);
  R           := ViewRect;
  FConvertion := M2D_Shift(FConvertion,(R.Left+R.Right)/2,
                                       (R.Top+R.Bottom)/2);
  FRConvertion:= M2D_Reverse(FConvertion);
end;

procedure    TdeSchemaView.UpdateConvToBuffer;
begin
  M2D_Identity(FConvToBuffer);
  FConvToBuffer := M2D_Shift(FConvToBuffer,-Offset.X,-Offset.Y);
  FConvToBuffer := M2D_Scale(FConvToBuffer,Scale,-Scale);
  FConvToBuffer := M2D_Shift(FConvToBuffer,FBuffer3.Width/2 + FViewDelta.X,
                                           FBuffer3.Height/2 + FViewDelta.Y);
end;

procedure TdeSchemaView.KeyRepeat(Sender: TObject);
var i    : Integer;
    V    : TPoint2D;
    Obj  : TDeSchemaObject;
    IPar : Integer;
    WPar : DWORD;
begin
  //............................................................................
  if Sender=nil then
    begin
      if SystemParametersInfo(SPI_GETKEYBOARDSPEED, 0, @WPar, 0) then
        FKeyTimer.Interval:=(1000 div WPar);
    end
  else
    begin
      if SystemParametersInfo(SPI_GETKEYBOARDDELAY, 0, @IPar, 0) then
        FKeyTimer.Interval:=IPar;
    end;
  //............................................................................
  if FDeShiftState = [] then
    begin
      case FKeyCode of
        VK_LEFT    : Select(Schema.NextLeft  (Selected[0]), smUnique);
        VK_RIGHT   : Select(Schema.NextRight (Selected[0]), smUnique);
        VK_UP      : Select(Schema.NextTop   (Selected[0]), smUnique);
        VK_DOWN    : Select(Schema.NextBottom(Selected[0]), smUnique);
      end;
      Paint;
    end;
  //............................................................................
  if (FDeShiftState = [ssDeShift]) then
    begin
      case FKeyCode of
        VK_LEFT    : Obj:= Schema.NextLeft  (Selected[SelectedCount-1]);
        VK_RIGHT   : Obj:= Schema.NextRight (Selected[SelectedCount-1]);
        VK_UP      : Obj:= Schema.NextTop   (Selected[SelectedCount-1]);
       else{VK_DOWN:}Obj:= Schema.NextBottom(Selected[SelectedCount-1]);
      end;

      if SelectedCount<2 then
        Select(Obj, smInvert)
      else
        if Obj = Selected[SelectedCount-2] then
          Select(Selected[SelectedCount-1], smInvert)
        else
          Select(Obj, smInvert);
      Paint;
    end;
  //............................................................................
  if (FDeShiftState = [ssDeCtrl]) then
    begin
      case FKeyCode of
        VK_LEFT    : V:= Point2D(-1/Scale, 0);
        VK_RIGHT   : V:= Point2D(+1/Scale, 0);
        VK_UP      : V:= Point2D(0, +1/Scale);
       else{VK_DOWN:}V:= Point2D(0, -1/Scale);
      end;

      for i:=0 to SelectedCount-1 do
        Selected[i].MoveBy(V);
      Paint;
    end;
  //............................................................................
  if (FDeShiftState = [ssDeCtrl, ssDeShift]) then
    begin
      case FKeyCode of
        VK_LEFT    : V:= Point2D(-10/Scale, 0);
        VK_RIGHT   : V:= Point2D(+10/Scale, 0);
        VK_UP      : V:= Point2D(0, +10/Scale);
       else{VK_DOWN:}V:= Point2D(0, -10/Scale);
      end;

      for i:=0 to SelectedCount-1 do
        Selected[i].MoveBy(V);

      Paint;
    end;
  //............................................................................
end;

procedure TdeSchemaView.KeyMessages(var Msg: tMsg; var Handled: Boolean);
begin
  //............................................................................
  if (Msg.Message = WM_KeyDown) then
    begin
      if (Msg.wParam in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) then
        begin
          Handled := True;
        end;
      case Msg.wParam of
        VK_SPACE    : begin
                        Include(FDeShiftState, ssSpace);
                        Cursor:=crCanvas;
                      end;
        VK_CONTROL  : Include(FDeShiftState, ssDeCtrl);
        VK_SHIFT    : Include(FDeShiftState, ssDeShift);

        VK_ADD      : ZoomIn;
        VK_SUBTRACT : ZoomOut;
        VK_MULTIPLY : Normalize;

        VK_HOME     : if FDeShiftState = [] then Select(Schema.NextLeft  (nil), smUnique);
        VK_END      : if FDeShiftState = [] then Select(Schema.NextRight (nil), smUnique);
        VK_PRIOR    : if FDeShiftState = [] then Select(Schema.NextTop   (nil), smUnique);
        VK_NEXT     : if FDeShiftState = [] then Select(Schema.NextBottom(nil), smUnique);
      end;

      if Msg.wParam in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
        begin
          FKeyCode := Msg.wParam;
          FKeyTimer.Enabled:=True;
          FKeyTimer.OnTimer(nil);
        end;
    end;
  //............................................................................
  if (Msg.Message = WM_KeyUp) then
    begin
      FKeyTimer.Enabled:=False;

      case Msg.wParam of
        VK_SPACE   :  begin
                        Exclude(FDeShiftState, ssSpace);
                        Cursor:=crDefault;
                      end;

        VK_CONTROL : Exclude(FDeShiftState, ssDeCtrl);
        VK_SHIFT   : Exclude(FDeShiftState, ssDeShift);
      end;
    end;
  //............................................................................
end;

procedure    TdeSchemaView.WMKeyStart(var Message: TMessage);
begin  //DOWN: Возможно из-за этого будут крупные проблемы
  FDeShiftState := [];
  FApplicationOnMessage := Application.OnMessage;
  Application.OnMessage := KeyMessages;
  FKeyTimer.Enabled     := False;
end;

procedure    TdeSchemaView.WMKeyClear(var Message: TMessage);
begin
  FDeShiftState := [];
  Application.OnMessage := FApplicationOnMessage;
  FKeyTimer.Enabled     := False;
end;

procedure    TdeSchemaView.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure    TdeSchemaView.WMHScroll(var Message: TWMHScroll);
var
  SI : TScrollInfo;
begin
  inherited;
  SI.cbSize := sizeOf(SI);
  SI.fMask  := SIF_ALL;
  getScrollInfo(Handle,SB_HORZ,SI);
  case Message.ScrollCode of
    SB_LINEUP   : MoveDisplay(SI.nPage div 50,0);
    SB_LINEDOWN : MoveDisplay(-(SI.nPage div 50),0);
    SB_PAGEUP   : MoveDisplay(SI.nPage,0,True);
    SB_PAGEDOWN : MoveDisplay(-SI.nPage,0,True);
    SB_THUMBTRACK : begin
      if not(FSBTracking) then
        BeginUpdate;
      FSBTracking := true;
      if SI.nTrackPos=SI.nPos then
        FOldHSBPos := SI.nTrackPos
      else
        MoveDisplay(-SI.nTrackPos+FOldHSBPos,0);
      FOldHSBPos := SI.nTrackPos;
    end;
    SB_THUMBPOSITION
{    SB_ENDSCROLL{} : begin
      if (FSBTracking) then
        EndUpdate;
      PostMessage(Handle,DE_UPDATESCROLLBARS,0,0);
      FSBTracking := false;
    end;
  end;
  if not(Message.ScrollCode in [SB_THUMBPOSITION,SB_THUMBTRACK,SB_ENDSCROLL]) then
    RefreshView;
end;

procedure    TdeSchemaView.WMVScroll(var Message: TWMVScroll);
var
  SI : TScrollInfo;
begin
  inherited;
  SI.cbSize := sizeOf(SI);
  SI.fMask  := SIF_ALL;
  getScrollINfo(Handle,SB_VERT,SI);
  case Message.ScrollCode of
    SB_LINEUP   : MoveDisplay(0,SI.nPage div 50);
    SB_LINEDOWN : MoveDisplay(0,-(SI.nPage div 50));
    SB_PAGEUP   : MoveDisplay(0,SI.nPage,True);
    SB_PAGEDOWN : MoveDisplay(0,-SI.nPage,True);
    SB_THUMBTRACK : begin
      if not(FSBTracking) then
        BeginUpdate;
      FSBTracking := true;
      if SI.nTrackPos=SI.nPos then
        FOldVSBPos := SI.nTrackPos
      else
        MoveDisplay(0,-SI.nTrackPos+FOldVSBPos);
      FOldVSBPos := SI.nTrackPos;
    end;
    SB_THUMBPOSITION
{    SB_ENDSCROLL{} : begin
      if (FSBTracking) then
        EndUpdate;
      PostMessage(Handle,DE_UPDATESCROLLBARS,0,0);
      FSBTracking := false;
    end;
  end;
  if not(Message.ScrollCode in [SB_THUMBPOSITION,SB_THUMBTRACK,SB_ENDSCROLL]) then
    RefreshView;
end;

procedure    TdeSchemaView.WMMouseWheel(var Message: TMSHMouseWheel);
begin
  inherited;
end;

procedure    TdeSchemaView.WMContextMenu(var Message: TMessage);
begin
  if not(IsUpdating) then
    inherited;
end;

procedure    TdeSchemaView.DEUpdateScrollBars(var Message:TMessage);
begin
  if Not FSBTracking then
    UpdateScrollBars;
  RefreshView;
  inherited;
end;

procedure    TdeSchemaView.DEEnhancedView(var Message:TMessage);
begin
  if (voSmoothing in Options)
     and(Message.LParam = FPrepareTick)
     and not IsEditing
  then begin
    RefreshEnhancedBuffer;
    ApplyEnhancedBuffer;
    RefreshViewBuffer;
    Invalidate;
  end;
end;

procedure    TdeSchemaView.DelayTimerOnTimer(Sender: TObject);
begin
  if (FLMMove_Timer) then begin
    FLMMove_Timer := false;
    if PtInRect(ClientRect{ViewRect},FLMMove_Pos)
    then begin
      if (FController <> nil) then
        FController.MouseMove(FLMMove_Shift,FLMMove_Pos.X,FLMMove_Pos.Y);
    end
    else
      begin
        Cursor := crDefault;
        if FLMMove_Pos.X<0            then MoveDisplay( 10,0);
        if FLMMove_Pos.X>ClientWidth  then MoveDisplay(-10,0);

        if FLMMove_Pos.Y<0            then MoveDisplay(0, 10);
        if FLMMove_Pos.Y>ClientHeight then MoveDisplay(0,-10);
      end;
    inherited MouseMove(FLMMove_Shift,FLMMove_Pos.X,FLMMove_Pos.Y);
  end;
end;

procedure    TdeSchemaView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style
                  or WS_HSCROLL
                  or WS_VSCROLL;
  Params.WindowClass.Style := Params.WindowClass.Style
                              and not(CS_VREDRAW)
                              and not(CS_HREDRAW);

end;

procedure    TdeSchemaView.setOptions(aValue:TdeSchemaViewOptions);
var
  F : boolean;
begin
  FChangeOptions := true;

  F := (voSmoothing in ((FOptions-aValue)+(aValue-FOptions)));
  if F then
    DestroyBuffers;

  FOptions := aValue;

  if F then
    CreateBuffers;

  UpdateViewRect;
  UpdateConvertion;

  FChangeOptions := false;
  if Visible then
    UpdateView;
end;

function     TdeSchemaView.getOptions:TdeSchemaViewOptions;
begin
  Result := FOptions;
end;

procedure    TdeSchemaView.setRulerStyle(aValue:TdeRulerStyle);
begin
  if RulerStyle=aValue then exit;
  FRulerStyle := aValue;
  UpdateViewRect;
  UpdateConvertion;
  if Visible then
    UpdateView;
end;

procedure    TdeSchemaView.setScrollbars(aValue:TdeSchemaViewScrollbars);
begin
  if aValue = FScrollbars then exit;
  FScrollBars := aValue;
  UpdateView;
end;

procedure    TdeSchemaView.setOffset(aValue:TPoint2D);
begin
  if (FOffset.X<>aValue.X)or(FOffset.Y<>aValue.Y)
  then begin
    FOffset := aValue;
    OffsetChanged;
  end;
end;

procedure    TdeSchemaView.OffsetChanged;
begin
  UpdateConvertion;
  UpdateConvToBuffer;
  if (abs(FViewDelta.X)>Screen.Width)
     or(abs(FViewDelta.Y)>Screen.Height)
  then
    UpdateView(true)
  else begin
    RefreshViewBuffer;
    UpdateScrollBars;
    Invalidate;
    RefreshView(true);
  end;
end;

procedure    TdeSchemaView.setScale(aValue:double);
var
  i : integer;
  iValue : integer;
begin
  if (FScale<>aValue) then begin
    FScale := aValue;

    aValue := aValue * 100;
    i := 0;
    while (round(aValue)<10) do begin
      aValue := aValue*10;
      i := i-1;
    end;
    while (round(aValue)>=100) do begin
      aValue := aValue/10;
      i := i+1;
    end;
    iValue := round(aValue);
    FIntZoom := i*_ZoomTickCount;
    i := _ZoomTickCount-1;
    while (i>=0)and(_ZoomTicks[i]>iValue) do dec(i);
    FIntZoom := FIntZoom + i;

    FScale := Zoom/100;
    ScaleChanged;
  end;
end;

procedure    TdeSchemaView.ScaleChanged;
begin
  UpdateConvertion;
  UpdateConvToBuffer;
  UpdateView;
end;

function     TdeSchemaView.getZoom:single;
var
  i : integer;
begin
  Result := 1;
  i := FIntZoom;
  while (i>=_ZoomTickCount) do begin
    Result := Result * 10;
    dec(i,_ZoomTickCount);
  end;
  while (i<0) do begin
    Result := Result / 10;
    inc(i,_ZoomTickCount);
  end;
  Result := Result * _ZoomTicks[i];
end;

procedure    TdeSchemaView.SchemaNewLayer(aLayer:TdeSchemaLayer);
begin
end;

procedure    TdeSchemaView.SchemaDelLayer(aLayer:TdeSchemaLayer);
var
  i : integer;
begin
  BeginUpdate;
  try
    if (FEditItem <> nil) and (FEditItem.Layer = aLayer) then
      EndEdit;
    for i := 0 to SelectedCount-1 do begin
      if (Selected[i].Layer = aLayer) then
        Select(Selected[i]);
    end;
    if (FFocused <> nil)
        and(TdeSchemaObject(FFocused).Layer = aLayer)
    then
      FocusedItem := nil;
    UpdateView;
  finally
    EndUpdate;
  end;
end;

procedure    TdeSchemaView.SchemaLayerChanged(aLayer:TdeSchemaLayer);
begin
  if aLayer.ReadOnly
     or not(aLayer.Visible)
  then
    SchemaDelLayer(aLayer)
  else
    UpdateView;
end;

procedure    TdeSchemaView.SchemaNewShortcut(aShortcut:TdeSchemaShortcut);
begin
end;

procedure    TdeSchemaView.SchemaDelShortcut(aShortcut:TdeSchemaShortcut);
begin
  if FActiveShortcut = aShortcut then
    FActiveShortcut := nil;
end;

procedure    TdeSchemaView.SchemaShortcutChanged(aShortcut:TdeSchemaShortcut);
begin
end;

procedure    TdeSchemaView.CheckItemNotActive(anItem:TdeSchemaObject);
begin
  BeginUpdate;
  try
    if (FEditItem = anItem) then
      EndEdit;
    if SelectedIndex(anItem) >= 0 then
      Select(anItem);
    if (FFocused = anItem) then
      FocusedItem := nil;
    UpdateView;
  finally
    EndUpdate;
  end;
end;

procedure    TdeSchemaView.SchemaNewItem(anItem:TdeSchemaObject);
begin
  NewObjectNotify(anItem);
end;

procedure    TdeSchemaView.SchemaDelItem(anItem:TdeSchemaObject);
begin
  CheckItemNotActive(anItem);
  DeleteObjectNotify(anItem);
end;

procedure    TdeSchemaView.SchemaItemChanging(anItem:TdeSchemaObject);
begin
end;

procedure    TdeSchemaView.SchemaItemChanged(anItem:TdeSchemaObject;
                                       ChangedProps:TdeObjectChangedProps);
begin
  if (ocpParent in ChangedProps)
     and(anItem.Parent<>nil)
  then begin
    CheckItemNotActive(anItem);
  end;
  ObjectChangedNotify(anItem);
end;

procedure    TdeSchemaView.SchemaChanged;
begin
  UpdateView;
end;

procedure    TdeSchemaView.DrawRulers;
var
  RTL,RT,RL : TRect;
  R,R0,R1   : TRect;
begin
  if (FRulerStyle=rsNone) then exit;
  R := ViewRect;
  RTL.BottomRight := R.TopLeft;
  RTL.Left  := RTL.Right  - cRulerSize;
  RTL.Top   := RTL.Bottom - cRulerSize;
  RT := Rect(RTL.Right,RTL.Top,R.Right,RTL.Bottom);
  RL := Rect(RTL.Left,RTL.Bottom,RTL.Right,R.Bottom);

  with Canvas do begin
    Lock;
    try
      R1 := ClipRect;
      IntersectRect(R,R1,RT);
      R0 := R;
      OffsetRect(R0,((FTopRulerBuf.Width-(RT.Right+RT.Left))div 2)+FViewDelta.X,
                     (FTopRulerBuf.Height-(RT.Bottom+RT.Top))div 2);
      BitBlt(Self.Canvas.Handle,
             R.Left,R.Top,R.Right-R.Left,R.Bottom-R.Top,
             FTopRulerBuf.Canvas.Handle,
             R0.Left,R0.Top,SRCCOPY);
      R1 := ClipRect;
      IntersectRect(R,R1,RL);
      R0 := R;
      OffsetRect(R0,(FLeftRulerBuf.Width-(RL.Right+RL.Left))div 2,
                    ((FLeftRulerBuf.Height-(RL.Bottom+RL.Top))div 2)+FViewDelta.Y);
      BitBlt(Self.Canvas.Handle,
             R.Left,R.Top,R.Right-R.Left,R.Bottom-R.Top,
             FLeftRulerBuf.Canvas.Handle,
             R0.Left,R0.Top,SRCCOPY);
      R1 := ClipRect;
      IntersectRect(R,R1,RTL);
      R0 := R;
      OffsetRect(R0,(FTopLeftBuf.Width-(RTL.Right+RTL.Left))div 2,
                    (FTopLeftBuf.Height-(RTL.Bottom+RTL.Top))div 2);
      BitBlt(Self.Canvas.Handle,
             R.Left,R.Top,R.Right-R.Left,R.Bottom-R.Top,
             FTopLeftBuf.Canvas.Handle,
             R0.Left,R0.Top,SRCCOPY);
    finally
      unLock;
    end;
  end;
end;

procedure    TdeSchemaView.Paint;
var
  R,R0,R1  : TRect;
  OffsetDB : TPoint;
begin
  R := ViewRect;

  StTick := integer(getTickCount);

  DrawRulers;

  R1 := Canvas.ClipRect;
  OffsetDB := Point(((FBuffer3.Width-(R.Right+R.Left))div 2)+FViewDelta.X,
                    ((FBuffer3.Height-(R.Bottom+R.Top))div 2)+FViewDelta.Y);
  IntersectRect(R,R1,R);
  R0 := R;
  OffsetRect(R0,OffsetDB.X,OffsetDB.Y);

  BitBlt(Self.Canvas.Handle,
         R.Left,R.Top,R.Right-R.Left,R.Bottom-R.Top,
         FBuffer3.Canvas.Handle,
         R0.Left,R0.Top,SRCCOPY);

  if FChangesShown then
    ShowChanges;

  StTick := integer(getTickCount)-StTick;
  StTick := integer(getTickCount);

end;

procedure    TdeSchemaView.DrawChanges;
begin
  if FChangesShown
     or ((SelectedCount=0)and(FActiveShortcut=nil))
  then
    exit;
  FChangesShown := true;
  //ShowChanges;
  Invalidate;
end;

procedure    TdeSchemaView.HideChanges;
begin
  if FChangesShown
    and ((SelectedCount>0)or(FActiveShortcut<>nil))
  then
    {ShowChanges};
  FChangesShown := false;
  Invalidate;
end;


procedure    TdeSchemaView.ShowChanges;
var
  i       : integer;
  anItem  : TdeSchemaObject;
  DplRect : TRect;
  SCList  : tList;
  SC      : TdeSchemaShortcut;
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Style := psDot;
  Canvas.Pen.Mode  := pmXOR;
  Canvas.Pen.Width := 1;
  Canvas.Brush.Color := clWhite;
  Canvas.Brush.Style := bsSolid;
  if IsEditing then
  begin
    EditedItem.PaintChanges(Canvas,FConvertion,FViewCoorninates);
  end
  else begin
    if FActiveShortcut <> nil then
    begin
      IntersectClipRect(Canvas.Handle,ViewRect.Left, ViewRect.Top,
                                      ViewRect.Right,ViewRect.Bottom);
      FActiveShortcut.PaintFrame(Canvas,FConvertion);
      SelectClipRgn(Canvas.Handle,0);
    end
    else begin
      if (Mode=vmEdit)and(SelectedCount>1) then
      begin
        DplRect := Selected[0].getViewRect(Canvas,FConvertion);
        for i:=1 to SelectedCount-1 do
          UnionRect(DplRect,DplRect,Selected[i].getViewRect(Canvas,FConvertion));
      end
      else
        DplRect := Rect(0,0,0,0);
      IntersectClipRect(Canvas.Handle,ViewRect.Left, ViewRect.Top,
                                      ViewRect.Right,ViewRect.Bottom);
      if not(IsRectEmpty(DplRect)) then begin
        anItem := FocusedItem;
        if anItem<>nil then
          anItem.PaintFrame(Canvas,FConvertion);
        Canvas.Pen.Color := clBlack;
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Mode  := pmCOPY;
        Canvas.Brush.Color := clWhite;
        Canvas.Brush.Style := bsSolid;
        Canvas.DrawFocusRect(DplRect);
      end
      else begin
        SCList := TList.Create;
        try
          for i:=0 to SelectedCount-1 do begin
            Selected[i].PaintFrame(Canvas,FConvertion);
            FSchema.getObjectShortcuts(Selected[i],SCList);
            while SCList.Count > 0 do begin
              SC := TdeSchemaShortcut(SCList[SCList.Count-1]);
              SC.PaintFrame(Canvas,FConvertion);
              SCList.Delete(SCList.Count-1);
            end;
          end;
        finally
          SCList.Free;
        end;
      end;
      SelectClipRgn(Canvas.Handle,0);
    end;
  end;
end;

procedure    TdeSchemaView.HideSelectRect;
begin
  if (FSelectRect.Left<0) then exit;
  IntersectClipRect(Canvas.Handle,ViewRect.Left,ViewRect.Top,
                                  ViewRect.Right,ViewRect.Bottom);
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Mode  := pmCOPY;
  Canvas.Brush.Style := bsSolid;
  Canvas.DrawFocusRect(FSelectRect);
  SelectClipRgn(Canvas.Handle,0);
  FSelectRect.Left := -10;
end;

procedure    TdeSchemaView.DrawSelectRect(aRect:TRect);
begin
  if (FSelectRect.Left>=0) then HideSelectRect;
  if (aRect.Left>aRect.Right) then begin
    FSelectRect.Left  := aRect.Right;
    FSelectRect.Right := aRect.Left;
  end
  else begin
    FSelectRect.Left  := aRect.Left;
    FSelectRect.Right := aRect.Right;
  end;
  if (aRect.Top>aRect.Bottom) then begin
    FSelectRect.Top  := aRect.Bottom;
    FSelectRect.Bottom := aRect.Top;
  end
  else begin
    FSelectRect.Top  := aRect.Top;
    FSelectRect.Bottom := aRect.Bottom;
  end;
  IntersectClipRect(Canvas.Handle,ViewRect.Left,ViewRect.Top,
                                  ViewRect.Right,ViewRect.Bottom);
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Mode  := pmCOPY;
  Canvas.Brush.Color := clWhite;
  Canvas.Brush.Style := bsSolid;
  Canvas.DrawFocusRect(FSelectRect);
  SelectClipRgn(Canvas.Handle,0);
end;

procedure    TdeSchemaView.MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
  if not(Focused)and canFocus then setFocus;
  if not PtInRect(ViewRect,Point(X,Y)) then exit;
  FLMMove_Pos   := Point(X,Y);
  if (FController<>nil)then
    FController.MouseDown(Button,Shift,X,Y);
  inherited MouseDown(Button,Shift,X,Y);
end;

procedure    TdeSchemaView.MouseMove(Shift:TShiftState;X,Y:Integer);
begin
  FLMMove_Timer := (FLMMove_Pos.X<>X)or(FLMMove_Pos.Y<>Y)or(FLMMove_Shift<>Shift);
  FLMMove_Pos   := Point(X,Y);
  FLMMove_Shift := Shift;
end;

procedure    TdeSchemaView.MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
  delayTimerOnTimer(nil);
  FLMMove_Pos   := Point(X,Y);
  if (FController<>nil) then
    FController.MouseUp(Button,Shift,X,Y);
  inherited MouseUp(Button,Shift,X,Y);
end;

procedure    TdeSchemaView.UpdateScrollBars;
var
  VR    : TRect;
  VH,VW : integer;
  Center: TPoint;
  SR    : TRect;
  SH,SW : integer;
  Bnds  : TRect2D;
  R1    : TRect;
  SI    : TScrollInfo;
  bV,bH : boolean;
begin
  if FSBTracking then exit;

  UpdateConvertion;
  VR := ViewRect;
  VH := VR.Bottom - VR.Top;
  VW := VR.Right  - VR.Left;

  Bnds := Schema.Bounds;
  SR.TopLeft     := SchemaToView(RectTopLeft(Bnds));
  SR.BottomRight := SR.TopLeft;
  InflateRect(SR,1,1);
  R1.TopLeft     := SchemaToView(RectTopRight(Bnds));
  R1.BottomRight := R1.TopLeft;
  InflateRect(R1,1,1);
  UnionRect(SR,SR,R1);
  R1.TopLeft     := SchemaToView(RectBottomRight(Bnds));
  R1.BottomRight := R1.TopLeft;
  InflateRect(R1,1,1);
  UnionRect(SR,SR,R1);
  R1.TopLeft     := SchemaToView(RectBottomLeft(Bnds));
  R1.BottomRight := R1.TopLeft;
  InflateRect(R1,1,1);
  UnionRect(SR,SR,R1);
  SH := SR.Bottom - SR.Top;
  SW := SR.Right  - SR.Left;
  Center := SchemaToView(Offset);

  bV := false;
  bH := false;
  case FScrollbars of
    svsNone : begin
    end;
    svsHorz : begin
      bH := true;
    end;
    svsVert : begin
      bV := true;
    end;
    svsBoth : begin
      bH := true;
      bV := true;
    end;
    svsAuto : begin
      bH := (SR.Left<VR.Left)or(SR.Right>VR.Right);
      bV := (SR.Top<VR.Top)or(SR.Bottom>VR.Bottom);
    end;
  end;

  if bH=bV then
    begin
      ShowScrollBar(Handle,SB_BOTH,bH);
    end  
  else
    begin
      ShowScrollBar(Handle,SB_HORZ,bH);
      ShowScrollBar(Handle,SB_Vert,bV);
    end;

  if (SH-VH) > 0 then
    InflateRect(SR,0,-VH div 2)
  else begin
    SR.Bottom:=(SR.Bottom+SR.Top) div 2;
    SR.Top   :=SR.Bottom-1;
    SR.Bottom:=SR.Bottom+1;
  end;
  if (SW-VW) > 0 then
    InflateRect(SR,-VW div 2,0)
  else begin
    SR.Left  := (SR.Left+SR.Right) div 2;
    SR.Right := SR.Left+1;
    SR.Left  := SR.Left-1;
  end;

  if (SR.Right < Center.X) then
    SR.Right:=Center.X
  else if (SR.Left > Center.X) then
    SR.Left:=Center.X;
  if (SR.Top > Center.Y) then
    SR.Top:=Center.Y
  else if (SR.Bottom < Center.Y) then
    SR.Bottom:=Center.Y;

  SI.cbSize := sizeOf(SI);
  SI.fMask  := SIF_PAGE or SIF_POS or SIF_RANGE;
  SI.nMin   := SR.Top;
  SI.nMax   := SR.Bottom + VH;
  SI.nPos   := Center.Y;
  SI.nPage  := VH;
  if bV then setScrollInfo(Handle,SB_VERT,SI,true);
  FOldHSBPos := SI.nPos;

  SI.cbSize := sizeOf(SI);
  SI.fMask  := SIF_PAGE or SIF_POS or SIF_RANGE;
  SI.nMin   := SR.Left;
  SI.nMax   := SR.Right + VW;
  SI.nPos   := Center.X;
  SI.nPage  := VW;
  if bH then setScrollInfo(Handle,SB_HORZ,SI,true);
  FOldVSBPos := SI.nPos;
end;

procedure    TdeSchemaView.MoveDisplay(dX,dY:integer; Limited: Boolean = False);
var
  P1,P2,V : TPoint2D;
  P       : TPoint;
begin
  P1 := ViewToSchema(Point( 0, 0));
  P2 := ViewToSchema(Point(dX,dY));
  V  := Vector2D(P2,P1);

  if (Limited) and (Offset.X + V.X < FSchema.Bounds.Left) and (V.X<0) then
    V.X := FSchema.Bounds.Left - Offset.X;

  if (Limited) and (Offset.X + V.X > FSchema.Bounds.Right) and (V.X>0) then
    V.X := FSchema.Bounds.Right - Offset.X;

  if (Limited) and (Offset.Y + V.Y < FSchema.Bounds.Bottom) and (V.Y<0) then
    V.Y := FSchema.Bounds.Bottom - Offset.Y;

  if (Limited) and (Offset.Y + V.Y > FSchema.Bounds.Top) and (V.Y>0) then
    V.Y := FSchema.Bounds.Top - Offset.Y;

  P  := SchemaToView(OffsetPoint2D(V,P1.X,P1.Y));

  FViewDelta := OffsetPoint  (FViewDelta, P.X, P.Y);
  Offset     := OffsetPoint2D(Offset,     V.X, V.Y);
end;

procedure    TdeSchemaView.MoveSelected(dX,dY:integer);
var
  P1,P2 : TPoint2D;
  i     : integer;
begin
  if (SelectedCount = 0) then exit;
  P1 := ViewToSchema(Point(0,0));
  P2 := ViewToSchema(Point(dX,dY));
  for i := 0 to SelectedCount-1 do
    Selected[i].MoveBy(Vector2D(P1,P2));
end;

procedure    TdeSchemaView.ResizeSelected(Direction:TdeBoundsPointClass;
                                           dX,dY:integer; Proportional:boolean);
var
  P1,P2 : TPoint2D;
  i     : integer;
begin
  if (SelectedCount = 0) then exit;
  P1 := ViewToSchema(Point(0,0));
  P2 := ViewToSchema(Point(dX,dY));
  for i := 0 to SelectedCount-1 do
    Selected[i].Resize(Direction,Vector2D(P1,P2),Proportional);
end;

procedure    TdeSchemaView.RotateSelected(Angle:double);
var
  i     : integer;
begin
  if (SelectedCount = 0) then exit;
  for i := 0 to SelectedCount-1 do
    Selected[i].Rotate(Angle);
end;

function     TdeSchemaView.getShortcutAt(X,Y:integer):TdeSchemaShortcut;
var
  i      : integer;
  anSC   : TdeSchemaShortcut;
  CX,CY  : integer;
begin
  Result := nil;
  CX := X-ViewRect.Left+(FBuffer3.Width-ViewRect.Right+ViewRect.Left)div 2 + FViewDelta.X;
  CY := Y-ViewRect.Top+(FBuffer3.Height-ViewRect.Bottom+ViewRect.Top)div 2 + FViewDelta.Y;
  i:=FSchema.ShortcutsCount-1;
  while (i >= 0) and (Result = nil) do
    begin
     anSC := FSchema.Shortcut[i];
     if anSC.Contain(FBuffer3.Canvas,FConvToBuffer,CX,CY) then
       Result := anSC;
      dec(i);
    end;
end;

function     TdeSchemaView.getItemAt(X,Y:integer):TdeSchemaObject;
var
  i      : integer;
  anItem : TdeSchemaObject;
  aCell  : TdeSchemaViewHashCell;
  CX,CY  : integer;
begin
  Result := nil;
  CX := X-ViewRect.Left+(FBuffer3.Width-ViewRect.Right+ViewRect.Left)div 2 + FViewDelta.X;
  CY := Y-ViewRect.Top+(FBuffer3.Height-ViewRect.Bottom+ViewRect.Top)div 2 + FViewDelta.Y;
  aCell := FHash.CellXY[CX,CY];
  if aCell<>nil then
    begin
      i:=aCell.Count-1;
      while (i >= 0) and (Result = nil) do
        begin
          anItem := aCell.Item[i];
          if (anItem.IGroup=nil) and anItem.Visible
                and anItem.Contain(FBuffer3.Canvas,FConvToBuffer,CX,CY) then
            Result := anItem;
          dec(i);
        end;
    end;
end;

function     TdeSchemaView.getRootItemAt(X,Y:integer):TdeSchemaObject;
begin
  Result := ItemAt[X,Y];
  if (Result<>nil) then
    Result := Result.TopParent;
end;

procedure    TdeSchemaView.SelectObjectNotify(anItem:TdeSchemaObject);
begin
  if not(FDeleting) and Assigned(FOnSelectObject) then
    FOnSelectObject(Self,anItem);
end;

procedure    TdeSchemaView.FocusObjectNotify(anItem:TdeSchemaObject);
begin
  if not(FDeleting) and Assigned(FOnFocusObject) then
    FOnFocusObject(Self,anItem);
end;

procedure    TdeSchemaView.NewObjectNotify(anItem:TdeSchemaObject);
begin
  if not(FDeleting) and Assigned(FOnNewObject) then
    FOnNewObject(Self,anItem);
end;

procedure    TdeSchemaView.DeleteObjectNotify(anItem:TdeSchemaObject);
begin
  if not(FDeleting) and Assigned(FOnDeleteObject) then
    FOnDeleteObject(Self,anItem);
end;

procedure    TdeSchemaView.ObjectChangedNotify(anItem:TdeSchemaObject);
begin
  if not(FDeleting) and Assigned(FOnObjectChanged) then
    FOnObjectChanged(Self,anItem);
end;

function     TdeSchemaView.IsFocuseRectPoint(X,Y:integer;
                                      var selObj:TdeSchemaObject;
                                       var pntID:integer):TdeBoundsPointClass;
var
  i     : integer;
  P     : TPoint;
  anObj : TdeSchemaObject;
  function Equal(P1,P2:TPoint):boolean;
  begin
    Result := (abs(P2.X-P1.X)<cPointSize)and(abs(P2.Y-P1.Y)<cPointSize);
  end;
begin
  Result := bpcUnknown;
  i := SelectedCount-1;
  P := Point(X,Y);
  while (i>=0)and(Result=bpcUnknown) do
    begin
      anObj := Selected[i];
      Result := _IsFocuseRectPoint(anObj,P,pntID,Canvas,FConvertion);
      if (Result<>bpcUnknown) then
        selObj := Selected[i];
      dec(i);
    end;
end;

function     TdeSchemaView.getSelectedCount:integer;
begin
  Result := FSelected.Count;
end;

function     TdeSchemaView.getSelected(index:integer):TdeSchemaObject;
begin
  if (Index<0) or (FSelected.Count<=Index) then
    Result:=nil
  else
    Result := FSelected[index];
end;

function     TdeSchemaView.getFocusedItem:TdeSchemaObject;
begin
  Result := FFocused;
end;

procedure    TdeSchemaView.setFocusedItem(aValue:TdeSchemaObject);
begin
  if (aValue<>nil) then
    begin
      if (aValue.Level=0)and(aValue.Visible)and not(aValue.ReadOnly)
      then
        FFocused := aValue
      else
        FFocused := nil;
    end
  else
    FFocused := nil;
  FocusObjectNotify(FFocused);
end;

procedure    TdeSchemaView.LocateFocusedItem;
var i       : integer;
    G       : boolean;
    anItem  : TdeSchemaObject;
begin
  G:= True;

  for i:=0 to FSchema.RootCount-1 do
    begin
      anItem := Schema.RootItem[i];

      if (G) and (FSelected.indexOf(anItem)>=0) then
        begin
          FocusedItem := anItem;
          G:= False;
        end;
    end;

  if G then FocusedItem := nil;
end;

procedure    TdeSchemaView.setMode(aValue:TdeSchemaViewMode);
begin
  if FMode=aValue then exit;
  FMode := aValue;
  EndEdit;
  if (FMode=vmEdit) then
    FController := FEditCtlr
  else
    FController := FBrowseCtlr;
end;

function     TdeSchemaView.CalcCursor(anObject:TdeSchemaObject;
                                           aPC: TdeBoundsPointClass):TCursor;
const
  _Cursors1 : array[0..7] of TCursor = (crSizeNESW,
                                        crSizeNS,
                                        crSizeNWSE,
                                        crSizeWE,
                                        crSizeNESW,
                                        crSizeNS,
                                        crSizeNWSE,
                                        crSizeWE);
  _Cursors2 : array[0..7] of TCursor = (crSizeNWSE,
                                        crSizeNS,
                                        crSizeNESW,
                                        crSizeWE,
                                        crSizeNWSE,
                                        crSizeNS,
                                        crSizeNESW,
                                        crSizeWE);
var
  angle      : double;
  shift      : integer;
  index      : integer;
  B          : TRect2D;
  V1,V2      : TPoint2D;
begin
  angle := anObject.Rotation*180/Pi;
  angle := (trunc(angle) + 360) mod 360;
  B     := anObject.Bounds;
  V1 := anObject.OToS(rectCenter(B));
  V2 := Vector2D(V1,anObject.OToS(rectRight(B)));
  V1 := Vector2D(V1,anObject.OToS(rectTop(B)));
  if (VectorVV2D(V1,V2)>0) then
    begin
      shift := trunc(360-angle+(45/2)) div 45;
      index := ((ord(aPC)-ord(bpcTopLeft)+shift) mod 8);
      Result := _Cursors1[index];
    end
  else
    begin
      shift := trunc(angle+(45/2)) div 45;
      index := ((ord(aPC)-ord(bpcTopLeft)+shift) mod 8);
      Result := _Cursors2[index];
    end;
end;

procedure    TdeSchemaView.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  C1,C2     : TPoint;
  P1,P2,V   : TPoint2D;
  Delta     : TPoint;
begin
  if not FChangeOptions then
    begin
      C1 := Point(round((ViewRect.Left+ViewRect.Right)/2),
                  round((ViewRect.Top+ViewRect.Bottom)/2));
    end;
  inherited setBounds(aLeft,aTop,aWidth,aHeight);


  UpdateViewRect;
  UpdateConvertion;
{
  UpdateScrollBars;
}
  if not FChangeOptions then
    begin
      C2 := Point(round((ViewRect.Left+ViewRect.Right)/2),
                  round((ViewRect.Top+ViewRect.Bottom)/2));
      //MoveDisplay((C1.X-C2.X),(C1.Y-C2.Y));
      Delta := Point((C1.X-C2.X),(C1.Y-C2.Y));
      P1 := ViewToSchema(Point(0,0));
      P2 := ViewToSchema(Delta);
      V := Vector2D(P2,P1);
      FViewDelta := Point(FViewDelta.X-Delta.X,FViewDelta.Y-Delta.Y);
      Offset := OffsetPoint2D(Offset,V.X,V.Y);
      RefreshView;
    end
  else
    begin
      UpdateScrollBars;
    end;
end;

procedure    TdeSchemaView.NormalizeByRect(R:TRect2D);
var
  H,W   : double;
  Kx,Ky : double;
  VR    : TRect;
begin
  H := abs(R.Top-R.Bottom);
  W := abs(R.Right-R.Left);
  if (H<=0) and (W<=0) then Exit;

  BeginUpdate;
  try
    VR := ViewRect;
    if (H>0) then
      Ky := (VR.Bottom-VR.Top)/H
    else
      Ky := 1/0.95;
    if (W>0) then
      Kx := (VR.Right-VR.Left)/W
    else
      Kx := 1/0.95;

    Scale  := 0.95*min(Kx,Ky);
    Offset := Point2D((R.Left+R.Right)/2, (R.Top+R.Bottom)/2);
  finally
    EndUpdate;
  end;
end;

procedure    TdeSchemaView.Normalize;
var
  R     : TRect2D;
  H,W   : double;
  Kx,Ky : double;
  VR    : TRect;
begin
  try
    R := Schema.Bounds;
    H := abs(R.Top-R.Bottom);
    W := abs(R.Right-R.Left);
  except
    H:=0;
    W:=0;
  end;
  if (H=0) and (W=0) then Exit;

  BeginUpdate;
  try
    VR := ViewRect;
    if (H>0) then
      Ky := (VR.Bottom-VR.Top)/H
    else
      Ky := 1/0.95;
    if (W>0) then
      Kx := (VR.Right-VR.Left)/W
    else
      Kx := 1/0.95;

    Scale  := 0.95*min(Kx,Ky);
    Offset := Point2D((R.Left+R.Right)/2, (R.Top+R.Bottom)/2);
  finally
    EndUpdate;
  end;
end;

procedure    TdeSchemaView.Centeralize;
var
  R     : TRect2D;
  H,W   : double;
begin
  try
    R := Schema.Bounds;
    H := abs(R.Top-R.Bottom);
    W := abs(R.Right-R.Left);
  except
    H:=0;
    W:=0;
  end;
  if (H=0) and (W=0) then Exit;

  BeginUpdate;
  try
    Offset := Point2D((R.Left+R.Right)/2, (R.Top+R.Bottom)/2);
  finally
    EndUpdate;
  end;
end;

procedure    TdeSchemaView.ZoomIn;
begin
  if FIntZoom>=18 then exit;
  inc(FIntZoom);
  Scale := Zoom/100;
end;

procedure    TdeSchemaView.ZoomOut;
begin
  if FIntZoom<=-18 then exit;
  dec(FIntZoom);
  Scale := Zoom/100;
end;

function     TdeSchemaView.SchemaToView(aPoint:TPoint2D):TPoint;
begin
  Result := P2dToP(MultVM2D(aPoint,FConvertion));
end;

function     TdeSchemaView.ViewToSchema(aPoint:TPoint):TPoint2D;
begin
  Result := MultVM2D(PToP2D(aPoint),FRConvertion);
end;

function     TdeSchemaView.ViewToSchemaRound(aPoint:TPoint):TPoint2D;
begin
  //TODO -oRA: Доделать округление
  Result := ViewToSchema(aPoint);
  Result.X:=(Round(Result.X*Scale))/Scale;
  Result.Y:=(Round(Result.Y*Scale))/Scale;
end;

procedure    TdeSchemaView.BeginUpdate;
begin
  inc(FUpdateLock);
  if (FUpdateLock=1) then
    FSchema.Lock;
end;

procedure    TdeSchemaView.EndUpdate;
begin
  if (FUpdateLock=1) then
    FSchema.unLock;
  dec(FUpdateLock);
  if (FUpdateLock<=0) then
    begin
      FUpdateLock := 0;
      if FNeedUpdate then
        UpdateView
      else
        if FNeedRefresh then
          begin
            RefreshView;
          end;
    end;
end;

function     TdeSchemaView.IsUpdating:boolean;
begin
  Result := (FUpdateLock>0);
end;

procedure    TdeSchemaView.UpdateView(Forced:boolean = false);
var OldCursor : TCursor;
begin
  FPrepareTick := integer(getTickCount);
  if FDeleting then exit;
  if not(Forced) and IsUpdating then
    FNeedUpdate := true
  else
    begin
      OldCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      FViewDelta := Point(0,0);
      UpdateConvertion;
      PrepareBuffers;
      UpdateScrollBars;
      if (voSmoothing in Options) and (not IsEditing) and (Mode = vmBrowse) then
        begin
          UpdateHash;
          TdeRefreshThread.Create(Handle,FPrepareTick);
        end
      else
        begin
          RefreshViewBuffer;
          Invalidate;
        end;

      FNeedUpdate := false;
      Screen.Cursor := OldCursor;
    end;
end;

procedure    TdeSchemaView.RefreshView(Forced:boolean = false);
begin
  FPrepareTick := integer(getTickCount);
  if FDeleting then exit;
  if not(Forced) and IsUpdating then
    FNeedRefresh := true
  else
    begin
//      RefreshViewBuffer;
//      UpdateScrollBars;
//      Invalidate;

      if (voSmoothing in Options) and (not IsEditing) and (Mode = vmBrowse) then
        TdeRefreshThread.Create(Handle,FPrepareTick)
      else
        UpdateView;

      FNeedRefresh := false;
    end;
end;

procedure    TdeSchemaView.Select(Item:TdeSchemaObject; SelectMode: TDeSelectMode = smInvert);
var i,n    : integer;
    aItem  : TdeSchemaObject;
    doEdit : Boolean;
begin
  if (Item=nil)then exit;
  n := SelectedIndex(Item);
  doEdit:= (FSelected.Count=1) and (FSelected[0]=FEditItem);

  //smUnique
  if SelectMode = smUnique then
    begin
      doEdit:= (FSelected.Count=1) and (FSelected[0]=FEditItem);

      if (n<0)and(Item.Level=0)and(Item.Visible)and not(Item.ReadOnly) then
      begin
        FSelected.Add(Item);
        FocusObjectNotify(Item);
      end;

      for i:= FSelected.Count-1 downto 0 do
        begin
          aItem:=FSelected[i];
          if aItem<>Item then
            begin
              if FEditItem=FSelected[i] then
                EndEdit;
              FSelected.Delete(i);
              //UpdateObjectView(Item);
              SelectObjectNotify(aItem);
            end;
        end;
      if doEdit then Edit;
      UpdateView;
      Exit;
    end;

  //smInvert
  if (n>=0) then
    begin
      if FEditItem=Item then
        EndEdit;
      FSelected.Delete(n);
      //UpdateObjectView(Item);
      SelectObjectNotify(Item);
      UpdateView;
    end
  else if (Item.Level=0)and(Item.Visible)and not(Item.ReadOnly) then
    begin
      if doEdit then EndEdit;
      FSelected.Add(Item);
      //UpdateObjectView(Item);
      SelectObjectNotify(Item);
      UpdateView;
    end;
end;

function     TdeSchemaView.SelectedIndex(Item:TdeSchemaObject):integer;
begin
  Result := FSelected.IndexOf(Item);
end;

procedure    TdeSchemaView.SelectByRect(aRect:TRect);
var
  i       : integer;
  VR,IR   : TRect2D;
  F,G     : boolean;
  anItem  : TdeSchemaObject;
begin
  if (aRect.Left>aRect.Right) then
    begin
      i := aRect.Left;
      aRect.Left  := aRect.Right;
      aRect.Right :=i;
    end;
  if (aRect.Top>aRect.Bottom) then
    begin
      i := aRect.Top;
      aRect.Top  := aRect.Bottom;
      aRect.Bottom := i;
    end;

  VR.TopLeft     := ViewToSchema(aRect.TopLeft);
  VR.BottomRight := ViewToSchema(aRect.BottomRight);
  BeginUpdate;
  try
    F := True;
    G := True;

    for i:=0 to FSchema.RootCount-1 do
      begin
        anItem := Schema.RootItem[i];
        IR := anItem.SchemaBounds;
        if anItem.Visible
            and not(anItem.ReadOnly)
            and PointInRect2D(IR.TopLeft,VR)
            and PointInRect2D(IR.BottomRight,VR) then
          begin
            F := False;
            Select(anItem);
          end;

        if (G) and (FSelected.indexOf(anItem)>=0) then
          begin
            FocusedItem := anItem;
            G:= False;
          end;
      end;

    if G then FocusedItem := nil;
    if F then Invalidate;
  finally
    EndUpdate;
  end;
end;

procedure    TdeSchemaView.ClearSelection;
var
  Item : TDeSchemaObject;
begin
  if (SelectedCount=0) then exit;
  while FSelected.Count > 0 do
  begin
    Item := FSelected[FSelected.Count-1];
    FSelected.Delete(FSelected.Count-1);
    SelectObjectNotify(Item);
  end;
  FocusObjectNotify(nil);
  RefreshView;
end;

function     TdeSchemaView.canGroup:boolean;
begin
  Result := (Mode=vmEdit)and(SelectedCount>1);
end;

function     TdeSchemaView.canUnGroup:boolean;
begin
  Result := (Mode=vmEdit)and(SelectedCount=1)and(Selected[0].IGroup<>nil);
end;

procedure    TdeSchemaView.Group;
var
  newGroup : TdeSchemaGroup;
begin
  if not(canGroup) then exit;
  BeginUpdate;
  try
    newGroup := FSchema.NewGroup(nil,Selected[0].LayerName);
    newGroup.Lock;
    while (SelectedCount>0) do
      Selected[0].Parent := newGroup;
    newGroup.unLock;
    Select(newGroup);
  finally
    EndUpdate;
  end;  
end;

procedure    TdeSchemaView.unGroup;
var
  aGroup : TdeSchemaGroup;
  anItem : TdeSchemaObject;
begin
  if not(canUnGroup) then exit;
  BeginUpdate;
  try
    aGroup := Selected[0].IGroup;
    ClearSelection;
    aGroup.Lock;
    while (aGroup.Count>0) do begin
      anItem := aGroup.Item[0];
      anItem.Lock;
      try
        anItem.Parent := aGroup.Parent;
        anItem.Index := aGroup.Index;
      finally
        anItem.unLock;
      end;
      Select(anItem);
    end;
    aGroup.unLock;
    aGroup.Delete;
  finally
    EndUpdate;
  end;  
end;

function     TdeSchemaView.canEdit:boolean;
begin
  Result := not(IsEditing)
            and(Mode=vmEdit)
            and(SelectedCount=1)
            and(Selected[0] is TdeSimpleCurve);
end;

procedure    TdeSchemaView.Edit;
begin
  if IsEditing then exit;
  if canEdit then begin
    FEditItem := TdeSimpleCurve(Selected[0]);
    FEditItem.ActiveVertex :=-1;
    UpdateObjectView(FEditItem);
    DrawChanges;
    FController := FCustomObjCtlr;
  end
  else
    FEditItem := nil;
end;

procedure    TdeSchemaView.EndEdit;
begin
  if not(IsEditing) then exit;
  HideChanges;
  FEditItem.ActiveVertex :=-1;
  UpdateObjectView(FEditItem);
  FEditItem := nil;
  if (Mode=vmBrowse) then
    FController := FBrowseCtlr
  else
    FController := FEditCtlr;
end;

function     TdeSchemaView.IsEditing:boolean;
begin
  Result := (FEditItem<>nil);
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('DeSchemaView unit initialization ...');
  {$ENDIF}
  Screen.Cursors[crAddPoint]  := LoadCursor(hInstance,'crAddPoint');
  Screen.Cursors[crMovePoint] := LoadCursor(hInstance,'crMovePoint');
  Screen.Cursors[crRotate]    := LoadCursor(hInstance,'crRotate');
  Screen.Cursors[crSpecial]   := LoadCursor(hInstance,'crSpecial');
  Screen.Cursors[crCanvas]    := LoadCursor(hInstance,'crCanvas');
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DeSchemaView unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

