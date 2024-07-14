unit DECalendar;

interface

uses
  System.Types, Windows, SysUtils, Messages, Graphics, Classes, StdCtrls, Controls, ExtCtrls,
  Vcl.ComCtrls, CommCtrl, DateUtils, Math, Forms, GraphUtil;

const

  WM_CONTOUR        = WM_USER + 1100;
  WM_DROPPED        = WM_CONTOUR + 1;
  WM_DOBUTTONRESIZE = WM_CONTOUR + 2;
  WM_AUTOPLACE      = WM_CONTOUR + 3;

  DATE_NONE         = -99999;

  RCoeff    = 5;
  TextSpase = 3;

  FirstTimerInterval = 500;
  NextTimerInterval = 120;

type
  TBtnArrowDirection = (adUp, adDown, adLeft, adRight);

  TCalendarCellKind = (ckNone, ckPredMonth, ckCurrentMonth, ckNextMonth, ckItem);

  TCalendarViewType = (cvWeeks, cvDays, cvMonths, cvCells);

  TGetLanguageType = (glSystemAuto, glDecarteAuto);

  TDropControlKind = (dcNone, dcCell, dcContainObject);

  TCalendarCellContainer = class;

  TCanSelectEvent  = procedure(Sender: TObject; var CanSelect: Boolean) of object;
  TDateChangeEvent = procedure(Sender: TObject; ADate : TDateTime) of object;
  TRangeChangeEvent = procedure(Sender: TObject; ADate : TDateTime; BDate : TDateTime) of object;

  TContainerEvent  = procedure(Sender: TCalendarCellContainer;AContainerObject : TWinControl)of object;
  TCreateContainerEvent  = procedure(Sender: TCalendarCellContainer;Var AContainerObject : TWinControl)of object;
  TContainerDragDropEvent  = procedure(DropControl : TControl; DropControlKind : TDropControlKind)of object;
  TFillingCellEvent = procedure (Sender: TCalendarCellContainer) of object;
  TResizingCellEvent = procedure (Sender: TCalendarCellContainer; var Width,Height:integer) of object;


  TContourControl = class(TCustomControl)
  private
    FContourColor: TColor;
    FCanAdjust : Boolean;
    FCanDraw: Boolean;
    FRoundedCorners: Boolean;
    FDesignDraw : Boolean;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure SetContourColor(const Value: TColor);
    procedure SetCanDraw(const Value: Boolean);
    function IsMouseOn: Boolean;
    function GetCanDraw: Boolean;
    procedure SetRoundedCorners(const Value: Boolean);
    function GetRoundedCoeff: integer;
    function GetDesignDraw: Boolean;
    procedure SetDesignDraw(const Value: Boolean);
  protected
    procedure CreateWnd;override;
    procedure Paint;override;
    procedure AdjustBounds;virtual;
    function  GetMinBounds:TPoint;virtual;
    procedure DoAdjustBounds;virtual;
    procedure DoOnBoundsChanged;virtual;
    procedure DrawControl;virtual;
    procedure DoOnCanDrawChanged;virtual;
    procedure DoOnSetContourColor;virtual;
    procedure DoOnResize;virtual;
    procedure DoOnMouseEnter;virtual;
    procedure DoOnMouseLeave;virtual;
    procedure DisableAdjust;
    procedure EnableAdjust;
    procedure ChangingCanDraw(CanDraw : Boolean; var CanChange: Boolean);virtual;
  public
    constructor Create(AOwner : TComponent);override;
    property CanDrawContour : Boolean read GetCanDraw write SetCanDraw ;
    property RoundedCorners : Boolean read FRoundedCorners write SetRoundedCorners;
    property DesignDraw : Boolean read GetDesignDraw write SetDesignDraw;
  published
    property ContourColor : TColor read FContourColor write SetContourColor;
    property Color default clNone;
    property ParentColor;
  end;

  TContourTextControl = class(TContourControl)
  private
    FAlignment: TAlignment;
    FOnChange: TNotifyEvent;
    procedure SetAlignment(const Value: TAlignment);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    function  GetMinBounds:TPoint;override;
    procedure Paint;override;
    function  GetTextRect: TRect; virtual;
    function  GetDrawText: String; virtual;
    function  GetTextAlignment: TAlignment; virtual;
    procedure DrawControlText(ATextRect : TRect; AText:TCaption; ATextAlignment : TAlignment);virtual;
    procedure DoOnChange;virtual;
  public
    constructor Create(AOwner:TComponent);override;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Text;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  published
    property Font;
  end;


  TWeekDaysPanel = class(TContourTextControl)
  private
    function GetItemBounds: TPoint;
    function GetDayName(DayIndex : integer):string;
    procedure DrawItem(ATextRect: TRect; DayIndex : integer);
  protected
    function  GetMinBounds:TPoint;override;
    procedure Paint; override;
    procedure DrawControlText(ATextRect : TRect; AText:TCaption; ATextAlignment : TAlignment);override;
  public
    constructor Create(AOwner : TComponent); override;
  published
    property Align;
    property Anchors;
    property Font;
    property CanDrawContour;
    property RoundedCorners;
    property ParentColor default False;
  end;

  TDraggedControl = class(TControl)
  public
    property DragMode;
  end;

  TCalendarCellContainer = class(TContourTextControl)
  private
    FMinBoundsX, FMinBoundsY: integer;
    FCellIndex : integer;
    FDate: TDateTime;
    //FIsCurrentMonthDay: Boolean;
    FCanSelectEvent: TCanSelectEvent;
    FSelected : Boolean;
    FCellKind: TCalendarCellKind;
    FContainObject: TWinControl;

    FOldContainerWndProc : TWndMethod;
    FDropTarget: TControl;
    FOnDropTarget: TContainerDragDropEvent;
    FDragging : Boolean;
    FOnDblClickContent: TNotifyEvent;

    procedure WMAutoPlace(var Message: TMessage); message WM_AUTOPLACE;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    procedure SetCellKind(const Value: TCalendarCellKind);
    function GetSelected: Boolean;
    procedure SetSelected(const Value: Boolean);
    function GetIsContainer: Boolean;
    function GetText: string;
    procedure UpdateTracking(aControl : TControl);
    procedure NewContainerWndProc(var Message : TMessage);
    procedure SetDropTarget(const Value: TControl);
    procedure SetText(const Value: string);
  protected
    procedure CreateWnd;override;
    procedure CreateParams(var Params: TCreateParams); override;
  //  function GetDragImages: TDragImageList; override;
    procedure AdjustBounds;override;
    function  GetMinBounds:TPoint;override;
    function  GetTextRect: TRect;override;
    function  GetDrawText: String; override;
    function  GetTextAlignment: TAlignment; override;
    procedure Paint; override;
    procedure DrawControl;override;
    procedure DrawControlText(ATextRect : TRect; AText:TCaption; ATextAlignment : TAlignment);override;
    procedure AutoPlaceControl(AWCoeff,AHCoeff:integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;X, Y: Integer); override;
    procedure SetDateValue(const Value: TDateTime);virtual;
    property CellKind : TCalendarCellKind read FCellKind  write SetCellKind;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy;override;
    property CellIndex : integer read FCellIndex;
    property CurrentDate : TDateTime read FDate write SetDateValue;
    property IsContainer : Boolean read GetIsContainer;
    property ContainObject : TWinControl read FContainObject;
    property Selected : Boolean read GetSelected write SetSelected;
    property DropTarget : TControl read FDropTarget write SetDropTarget;
    property Text : string read GetText write SetText;
    property OnCanSelect: TCanSelectEvent read FCanSelectEvent write FCanSelectEvent;
    property OnDropTarget: TContainerDragDropEvent read FOnDropTarget write FOnDropTarget;
    property OnDblClickContent: TNotifyEvent read FOnDblClickContent write FOnDblClickContent;
    property DragMode;
    property OnDragDrop;
    property OnDragOver;
  end;

  TCustomCalendar = class(TContourControl)
  private
    WeekDays  : TWeekDaysPanel;
    FFirstCellDate: TDate;
    FWCoeff,FHCoeff : integer ;
    FSelectedIndex: Integer;
    FCells: array of TCalendarCellContainer;
    FCurrentDate: TDateTime;
    FOnDateChange: TDateChangeEvent;
    FIsContainer: Boolean;
    FOnFillingContainer: TContainerEvent;
    FOnCreatingContainer: TCreateContainerEvent;
    FVCellsCount: integer;
    FHCellsCount: integer;
    FRowCount : integer;
    FViewType: TCalendarViewType;
    FOnActivateCell: TContainerEvent;
    FOnDeactivateCell: TContainerEvent;
    FOnClearContainer: TContainerEvent;
    FOnDestroingContainer: TContainerEvent;
    FOnDropTargetChange: TContainerDragDropEvent;
    FOnFillingCell : TFillingCellEvent;

    FScrollPos : Integer;
    FScrollUnit : integer;
    FColCount: integer;
    FOnDblClickCellCaption: TNotifyEvent;
    FOnDblClickCellContent: TNotifyEvent;
    FOnRangeChange: TRangeChangeEvent;
    FLockCount: Integer;
    FLockInitCount: Integer;
    FSelectFrom: TDate;
    FSelectTo: TDate;
    procedure ReInitScrollBar;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure IncCell(const Coeff: integer);
    procedure IncLine(const Coeff:integer);
    procedure IncPage(const Coeff:integer);
    procedure IncThumb(const Coeff:integer);
    procedure CreateCells;
    procedure SetCurrentDate(const Value: TDateTime);
    procedure SetIsContainer(const Value: Boolean);
    procedure SetViewType(const Value: TCalendarViewType);
    procedure SetFirstCellDate(const Value: TDate);
    procedure SetIsLoading(const Value: Boolean);
    procedure SetVCellsCount(const Value: integer);
    procedure SetHCellsCount(const Value: integer);
    procedure SetRowCount(const Value: integer);
    procedure SetColCount(const Value: integer);
    procedure SetScrollUnit(const Value: integer);
  private
    function GetVCellsCount: integer;
    function GetEndCellDate: TDate;
    function GetCell(const Index: Integer): TCalendarCellContainer;
    property  ScrollUnit : integer read  FScrollUnit write SetScrollUnit;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd;override;
    procedure AdjustBounds;override;
    function  GetMinBounds:TPoint;override;
    procedure SetCellsCount(ACount : integer);virtual;
    procedure SetCellsValues;
    procedure ReInitControl;
    procedure DoOnSetContourColor;override;
    procedure DoOnCanSelect(Sender: TObject; var CanSelect: Boolean);
    procedure DoDropTarget(DropControl : TControl; DropControlKind : TDropControlKind);
    procedure DoOnCreatingContainer(Sender: TCalendarCellContainer; var AContainerObject : TWinControl);
    procedure DoOnDestroingContainer(Sender: TCalendarCellContainer;AContainerObject : TWinControl);
    procedure DoOnFillingContainer(Sender: TCalendarCellContainer;AContainerObject : TWinControl);
    procedure DoOnClearContainer(Sender: TCalendarCellContainer;AContainerObject : TWinControl);
    procedure DoOnActivateCell;
    procedure DoOnDeactivateCell(Sender: TCalendarCellContainer);
    property  HCellsCount : integer read  FHCellsCount write  SetHCellsCount;
    property  VCellsCount : integer read  GetVCellsCount write  SetVCellsCount;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure CreateCellsContainers;
    procedure ClearCellContainers;
    procedure RefreshContent;
    procedure ClearContent;
    procedure SetParams(AViewType:TCalendarViewType; ARowCount: integer = 1; AColCount:integer = 7);
    function SelectedWinControl : TWinControl;
    property SelectedIndex: Integer read FSelectedIndex;
    procedure BeginUpdate;
    procedure EndUpdate(DoReInitControl: Boolean = True);
    property Cells[const Index: Integer]: TCalendarCellContainer read GetCell; default;
  published
    procedure CalcFirstCellDate;
    property Align;
    property Anchors;
    property Font;
    property CanDrawContour;
    property RoundedCorners;
    property ParentColor default False;
    property RowCount : integer read FRowCount write SetRowCount;
    property ColCount : integer read FColCount write SetColCount;
    property CurrentDate : TDateTime read FCurrentDate write SetCurrentDate;
    property FirstCellDate : TDate read FFirstCellDate write SetFirstCellDate;
    property EndCellDate: TDate read GetEndCellDate;
    property IsContainer : Boolean read FIsContainer write SetIsContainer;
    property ViewType : TCalendarViewType read FViewType write SetViewType;
    property OnDateChange: TDateChangeEvent read FOnDateChange write FOnDateChange;
    property OnCreatingContainer : TCreateContainerEvent read FOnCreatingContainer write FOnCreatingContainer;
    property OnDestroingContainer : TContainerEvent read FOnDestroingContainer write FOnDestroingContainer;
    property OnFillingContainer : TContainerEvent read FOnFillingContainer write FOnFillingContainer;
    property OnClearContainer : TContainerEvent read FOnClearContainer write FOnClearContainer;
    property OnActivateCell : TContainerEvent read FOnActivateCell write FOnActivateCell;
    property OnDeactivateCell : TContainerEvent read FOnDeactivateCell write FOnDeactivateCell;
    property OnDropTargetChange : TContainerDragDropEvent read FOnDropTargetChange write FOnDropTargetChange;
    property OnFillingCell : TFillingCellEvent read FOnFillingCell write FOnFillingCell;
    property OnDblClickCellCaption: TNotifyEvent read FOnDblClickCellCaption write FOnDblClickCellCaption;
    property OnDblClickCellContent: TNotifyEvent read FOnDblClickCellContent write FOnDblClickCellContent;
    property OnRangeChange: TRangeChangeEvent read FOnRangeChange write FOnRangeChange;
  end;


  TCellsScrollBox = class(TScrollBox)
  private
    WeekDays  : TWeekDaysPanel;
    FFirstCellDate: TDateTime;
    FIsLoading: Boolean;
    FWCoeff,FHCoeff : integer ;
    FSelectedIndex: Integer;
    FCells: array of TCalendarCellContainer;
    FCurrentDate: TDateTime;
    FOnDateChange: TDateChangeEvent;
    FIsContainer: Boolean;
    FOnFillingContainer: TContainerEvent;
    FOnCreatingContainer: TCreateContainerEvent;
    FVCellsCount: integer;
    FHCellsCount: integer;
    FRowCount : integer;
    FViewType: TCalendarViewType;
    FOnActivateCell: TContainerEvent;
    FOnDeactivateCell: TContainerEvent;
    FOnClearContainer: TContainerEvent;
    FOnDestroingContainer: TContainerEvent;
    FOnDropTargetChange: TContainerDragDropEvent;
    FOnFillingCell : TFillingCellEvent;

    FScrollPos : Integer;
    FScrollUnit : integer;
    FColCount: integer;

    FOnDblClickCellCaption: TNotifyEvent;
    FOnDblClickCellContent: TNotifyEvent;
    FOnRangeChange: TRangeChangeEvent;
    FLockCount: Integer;
    FLockInitCount: Integer;

    procedure ReInitScrollBar;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure IncLine(const Coeff:integer);
    procedure IncPage(const Coeff:integer);
    procedure IncThumb(const Coeff:integer);
    procedure CreateCells;
    procedure SetCurrentDate(const Value: TDateTime);
    procedure SetIsContainer(const Value: Boolean);
    procedure SetViewType(const Value: TCalendarViewType);
    procedure SetFirstCellDate(const Value: TDateTime);
    procedure SetIsLoading(const Value: Boolean);
    procedure CalcFirstCellDate;
    procedure SetVHCellsCount(const VValue, HValue: integer);
    procedure SetVCellsCount(const Value: integer);
    procedure SetHCellsCount(const Value: integer);
    procedure SetRowCount(const Value: integer);
    procedure SetColCount(const Value: integer);
    procedure SetScrollUnit(const Value: integer);
  private
    property  IsLoading : Boolean read FIsLoading write SetIsLoading;
    property  FirstCellDate : TDateTime read FFirstCellDate write SetFirstCellDate;
    property  ScrollUnit : integer read  FScrollUnit write SetScrollUnit;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd;override;
    procedure AdjustBounds;virtual;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure AutoScrollInView(AControl: TControl); override;
    procedure SetCellsCount(ACount : integer);virtual;
    procedure SetCellsValues;
    procedure ReInitControl;
    procedure DoOnCanSelect(Sender: TObject; var CanSelect: Boolean);
    procedure DoDropTarget(DropControl : TControl; DropControlKind : TDropControlKind);
    procedure DoOnCreatingContainer(Sender: TCalendarCellContainer; var AContainerObject : TWinControl);
    procedure DoOnDestroingContainer(Sender: TCalendarCellContainer;AContainerObject : TWinControl);
    procedure DoOnFillingContainer(Sender: TCalendarCellContainer;AContainerObject : TWinControl);
    procedure DoOnClearContainer(Sender: TCalendarCellContainer;AContainerObject : TWinControl);
    procedure DoOnActivateCell;
    procedure DoOnDeactivateCell(Sender: TCalendarCellContainer);
    property  HCellsCount : integer read  FHCellsCount write  SetHCellsCount;
    property  VCellsCount : integer read  FVCellsCount write  SetVCellsCount;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy;override;
    procedure CreateCellsContainers;
    procedure ClearCellContainers;
    procedure RefreshContent;
    procedure ClearContent;
    procedure SetRowColCount(const aRow, aCol: integer);
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Align;
    property Anchors;
    property Font;
    property ParentColor default False;
    property RowCount : integer read FRowCount write SetRowCount;
    property ColCount : integer read FColCount write SetColCount;
    property CurrentDate : TDateTime read FCurrentDate write SetCurrentDate;
    property CurrentIndex: integer read FSelectedIndex;
    property IsContainer : Boolean read FIsContainer write SetIsContainer;
    property ViewType : TCalendarViewType read FViewType write SetViewType;
    property OnDateChange: TDateChangeEvent read FOnDateChange write FOnDateChange;
    property OnCreatingContainer : TCreateContainerEvent read FOnCreatingContainer write FOnCreatingContainer;
    property OnDestroingContainer : TContainerEvent read FOnDestroingContainer write FOnDestroingContainer;
    property OnFillingContainer : TContainerEvent read FOnFillingContainer write FOnFillingContainer;
    property OnClearContainer : TContainerEvent read FOnClearContainer write FOnClearContainer;
    property OnActivateCell : TContainerEvent read FOnActivateCell write FOnActivateCell;
    property OnDeactivateCell : TContainerEvent read FOnDeactivateCell write FOnDeactivateCell;
    property OnDropTargetChange : TContainerDragDropEvent read FOnDropTargetChange write FOnDropTargetChange;
    property OnFillingCell : TFillingCellEvent read FOnFillingCell write FOnFillingCell;
    property OnDblClickCellCaption: TNotifyEvent read FOnDblClickCellCaption write FOnDblClickCellCaption;
    property OnDblClickCellContent: TNotifyEvent read FOnDblClickCellContent write FOnDblClickCellContent;
  end;

  procedure Register;

  function GetAltMonthNames (Locale: LCID; var ALTMontNames): HRESULT; stdcall;
  external 'oleaut32.dll' name 'GetAltMonthNames';

  procedure GetLocalParams;

const
  HLSMAX = 240;
  RGBMAX = 255;
  UNDEFINED = (HLSMAX*2) div 3;

type
  TALTMontNames = array[1..12] of PWideChar;
  PALTMontNames = ^TALTMontNames;

var
  AltMonthNames: PALTMontNames;
  LongMonthNames: array[1..12] of string;
  ShortDayNames: array[1..7] of string;

  FirstDayOfWeek : integer;
implementation

{ Service Functions }

procedure Register;
begin
  RegisterComponents('Profit', [TCalendarCellContainer,{ TMonthsComboBox, TYearSpinBox,
                                TMYPanel, TCustomCalendarGrid, }TCustomCalendar,
                                TCellsScrollBox]);
end;

procedure GetLocalParams;
var
  UserLCID : LCID;
  UserLang : LANGID;
  i, Day : integer;

  function GetLocaleStr(LocaleType: Integer): string;
  var
    L: Integer;
    Buffer: array[0..255] of Char;
  begin
    L := GetLocaleInfo(UserLCID, LocaleType, Buffer, SizeOf(Buffer));
    if L > 0 then
      SetString(Result, Buffer, L - 1)
    else
      Result := '*';
  end;

begin
  UserLCID := GetThreadLocale;
  UserLang := Word(UserLCID);

  GetLocaleStr(LOCALE_IFIRSTDAYOFWEEK);
  FirstDayOfWeek := StrToInt(Copy(GetLocaleStr(LOCALE_IFIRSTDAYOFWEEK),0,1));

  for I := 1 to 12 do
    DECalendar.LongMonthNames[I] := GetLocaleStr(LOCALE_SMONTHNAME1 + I - 1);

  for I := 1 to 7 do
  begin
    Day := (I + 5) mod 7;
    DECalendar.ShortDayNames[I] := GetLocaleStr(LOCALE_SABBREVDAYNAME1 + Day);
  end;

  if (not Succeeded(GetAltMonthNames(UserLang, AltMonthNames)))
  or (not Assigned(AltMonthNames))then
  begin
    GetMem(AltMonthNames, 12*30);
    for i:=1 to 12 do
    begin
      GetMem(AltMonthNames^[i],(Length(DECalendar.LongMonthNames[i])+1)* sizeof(WideChar));
      StringToWideChar(DECalendar.LongMonthNames[i],AltMonthNames^[i],(Length(DECalendar.LongMonthNames[i])+1));
    end;
  end;
end;

procedure RGBtoHLS(R,G,B : LongInt; var H,L,S : LongInt);
Var
 cMax,cMin  : integer;
 Rdelta,Gdelta,Bdelta : single;
Begin
   cMax := max( max(R,G), B);
   cMin := min( min(R,G), B);
   L := round( ( ((cMax+cMin)*HLSMAX) + RGBMAX )/(2*RGBMAX) );

   if (cMax = cMin) then begin
      S := 0; H := UNDEFINED;
   end else begin
      if (L <= (HLSMAX/2)) then
         S := round( ( ((cMax-cMin)*HLSMAX) + ((cMax+cMin)/2) ) / (cMax+cMin) )
      else
         S := round( ( ((cMax-cMin)*HLSMAX) + ((2*RGBMAX-cMax-cMin)/2) )
            / (2*RGBMAX-cMax-cMin) );
      Rdelta := ( ((cMax-R)*(HLSMAX/6)) + ((cMax-cMin)/2) ) / (cMax-cMin);
      Gdelta := ( ((cMax-G)*(HLSMAX/6)) + ((cMax-cMin)/2) ) / (cMax-cMin);
      Bdelta := ( ((cMax-B)*(HLSMAX/6)) + ((cMax-cMin)/2) ) / (cMax-cMin);
      if (R = cMax) then H := round(Bdelta - Gdelta)
      else if (G = cMax) then H := round( (HLSMAX/3) + Rdelta - Bdelta)
      else H := round( ((2*HLSMAX)/3) + Gdelta - Rdelta );
      if (H < 0) then H:=H + HLSMAX;
      if (H > HLSMAX) then H:= H - HLSMAX;
   end;
   if S<0 then S:=0; if S>HLSMAX then S:=HLSMAX;
   if L<0 then L:=0; if L>HLSMAX then L:=HLSMAX;
end;

procedure HLStoRGB(H,L,S : LongInt; var R,G,B : LongInt);
Var
 Magic1,Magic2 : single;

  function HueToRGB(n1,n2,hue : single) : single;
  begin
     if (hue < 0) then hue := hue+HLSMAX;
     if (hue > HLSMAX) then hue:=hue -HLSMAX;
     if (hue < (HLSMAX/6)) then
        result:= ( n1 + (((n2-n1)*hue+(HLSMAX/12))/(HLSMAX/6)) )
     else
     if (hue < (HLSMAX/2)) then result:=n2 else
     if (hue < ((HLSMAX*2)/3)) then
        result:= ( n1 + (((n2-n1)*(((HLSMAX*2)/3)-hue)+(HLSMAX/12))/(HLSMAX/6)))
     else result:= ( n1 );
  end;

begin
   if (S = 0) then begin
      B:=round( (L*RGBMAX)/HLSMAX ); R:=B; G:=B;
   end else begin
      if (L <= (HLSMAX/2)) then Magic2 := (L*(HLSMAX + S) + (HLSMAX/2))/HLSMAX
      else Magic2 := L + S - ((L*S) + (HLSMAX/2))/HLSMAX;
      Magic1 := 2*L-Magic2;
      R := round( (HueToRGB(Magic1,Magic2,H+(HLSMAX/3))*RGBMAX + (HLSMAX/2))/HLSMAX );
      G := round( (HueToRGB(Magic1,Magic2,H)*RGBMAX + (HLSMAX/2)) / HLSMAX );
      B := round( (HueToRGB(Magic1,Magic2,H-(HLSMAX/3))*RGBMAX + (HLSMAX/2))/HLSMAX );
   end;
   if R<0 then R:=0; if R>RGBMAX then R:=RGBMAX;
   if G<0 then G:=0; if G>RGBMAX then G:=RGBMAX;
   if B<0 then B:=0; if B>RGBMAX then B:=RGBMAX;
end;

function ChangeBrightnessByValue(AColor : TColor; Value : Byte):TColor;
var
  V : Byte;
  r,g,b,h,l,s : LongInt;
begin
  V := Min(Value,HLSMAX);
  r := GetRValue(ColorToRGB(AColor));
  g := GetGValue(ColorToRGB(AColor));
  b := GetBValue(ColorToRGB(AColor));
  RGBtoHLS(r,g,b,h,l,s);
  l := V;
  HLStoRGB(h,l,s,r,g,b);
  result := RGB(r, g, b);
end;

function ChangeBrightnessByDelta(AColor : TColor; Delta :integer):TColor;
var
  r,g,b,h,l,s : LongInt;
begin
  r := GetRValue(ColorToRGB(AColor));
  g := GetGValue(ColorToRGB(AColor));
  b := GetBValue(ColorToRGB(AColor));
  RGBtoHLS(r,g,b,h,l,s);
  l := l + Delta;
  l:= Max(Min(l,HLSMAX),0);
  HLStoRGB(h,l,s,r,g,b);
  result := RGB(r, g, b);
end;

{ TContourControl }

constructor TContourControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption, {csOpaque, }csDoubleClicks];
 // DoubleBuffered := True;
  FContourColor   := clBlack;
  FCanDraw        := False;
  FRoundedCorners := False;
  FCanAdjust      := True;
end;

procedure TContourControl.CreateWnd;
begin
  inherited CreateWnd;
  DisableAlign;
end;


procedure TContourControl.DrawControl;
begin
  if (FCanDraw)or(DesignDraw) then
  begin
    Canvas.Pen.Color := FContourColor;
    Canvas.RoundRect(0,0,ClientWidth,ClientHeight,GetRoundedCoeff,GetRoundedCoeff);
  end
  else
  begin
    Canvas.Pen.Color := Canvas.Brush.Color;
    Canvas.RoundRect(0,0,ClientWidth,ClientHeight,GetRoundedCoeff,GetRoundedCoeff);
  end;
end;

procedure TContourControl.Paint;
begin
  Canvas.Brush.Style := bsClear;//bsSolid;
  Brush.Color := Color;
  DrawControl;
end;

procedure TContourControl.AdjustBounds;
var
  NewWidth, NewHeight: integer;
  MinBounds : TPoint;
  OldCanAdjust : Boolean;
begin
  OldCanAdjust := FCanAdjust;
  FCanAdjust   := False;

  MinBounds := GetMinBounds;
  if csFixedWidth in ControlStyle then
    NewWidth := MinBounds.X
  else
    NewWidth := Width;
  if csFixedHeight in ControlStyle then
    NewHeight := MinBounds.Y
  else
    NewHeight := Height;

  if NewWidth < MinBounds.X then
     NewWidth := MinBounds.X;
  if NewHeight < MinBounds.Y then
     NewHeight := MinBounds.Y;
  if (NewWidth <> Width) or (NewHeight <> Height) then
    SetBounds(Left,Top,NewWidth,NewHeight);
  FCanAdjust := OldCanAdjust;
end;


function TContourControl.GetMinBounds: TPoint;
begin
  result := Point(0,0);
end;

procedure TContourControl.DisableAdjust;
begin
  FCanAdjust := False;
end;

procedure TContourControl.EnableAdjust;
begin
  FCanAdjust := True;
end;

procedure TContourControl.DoAdjustBounds;
begin
  if (not HandleAllocated)or(not FCanAdjust)then Exit;
  Canvas.Font.Assign(Self.Font);
  AdjustBounds;
  DoOnBoundsChanged;
end;

procedure TContourControl.DoOnBoundsChanged;
begin
end;

procedure TContourControl.DoOnMouseEnter;
begin
end;

procedure TContourControl.DoOnMouseLeave;
begin
end;

procedure TContourControl.ChangingCanDraw(CanDraw : Boolean; var CanChange: Boolean);
begin
  CanChange := True;
end;

procedure TContourControl.DoOnCanDrawChanged;
begin
  Invalidate;
end;

function TContourControl.GetCanDraw: Boolean;
begin
  result := FCanDraw;
end;

procedure TContourControl.SetCanDraw(const Value: Boolean);
var
  CanChange : Boolean;
begin
  if FCanDraw = Value then Exit;
  ChangingCanDraw(Value, CanChange);
  if not CanChange then Exit;
  FCanDraw := Value;
  DoOnCanDrawChanged;
end;

procedure TContourControl.SetContourColor(const Value: TColor);
begin
  if FContourColor = Value then Exit;
  FContourColor := Value;
  DoOnSetContourColor;
end;

procedure TContourControl.DoOnSetContourColor;
begin
  Invalidate;
end;

procedure TContourControl.SetRoundedCorners(const Value: Boolean);
begin
  if  FRoundedCorners = Value then Exit;
  FRoundedCorners := Value;
  DoOnResize;
end;

procedure TContourControl.DoOnResize;
var
  Rgn  : HRGN;
begin
  if not HandleAllocated then Exit;
  DoAdjustBounds;
  Rgn := CreateRoundRectRgn(0,0,Width+1,Height+1,GetRoundedCoeff,GetRoundedCoeff);
  SetWindowRgn(Handle, Rgn, True);
  DeleteObject(Rgn);
end;

function TContourControl.GetRoundedCoeff: integer;
begin
  result := 0 + (RCoeff * ord(RoundedCorners));
end;

function TContourControl.IsMouseOn: Boolean;
var
  Rgn  : HRGN;
  MPos : TPoint;
begin
  MPos := ScreenToClient(Mouse.CursorPos);
  Rgn := CreateRectRgn(0,0,0,0);
  GetWindowRgn(Handle,Rgn);
  Result :=PtInRegion(Rgn,MPos.X,MPos.Y);
end;

function TContourControl.GetDesignDraw: Boolean;
begin
  result := FDesignDraw and (csDesigning in ComponentState)
end;

procedure TContourControl.SetDesignDraw(const Value: Boolean);
begin
  if FDesignDraw = Value then Exit;
  FDesignDraw := Value;
  Invalidate;
end;

procedure TContourControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  DoAdjustBounds;
end;

procedure TContourControl.WMSize(var Message: TMessage);
begin
  inherited ;
  //if ((csDestroyingHandle in ControlState)or(csLoading in ComponentState)) then
  DoOnResize;
end;

{procedure TContourControl.WMERASEBKGND(var Message: TMessage);
begin
  Message.Result := 1;
end;}


{ TContourTextControl }

constructor TContourTextControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFixedHeight];
  FAlignment := taLeftJustify;
end;

function TContourTextControl.GetMinBounds: TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  result := inherited GetMinBounds;
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then I := Metrics.tmHeight;
    I := I div 8 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  result.Y := Metrics.tmHeight + I;
end;

procedure TContourTextControl.DrawControlText(ATextRect : TRect; AText:TCaption; ATextAlignment : TAlignment);
const
  Alignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Flags : LongInt;
begin
  if csLoading in ComponentState then Exit;
  with Canvas do
  begin
    Flags := DT_EXPANDTABS or DT_VCENTER or Alignments[ATextAlignment];
    Flags := DrawTextBiDiModeFlags(Flags);
    DrawText(Handle, PChar(AText), -1, ATextRect, Flags);
  end;
end;

function TContourTextControl.GetTextRect: TRect;
var
  FontHeight : integer;
begin
  Result := GetClientRect;
  FontHeight := Canvas.TextHeight('W');
  with Result do
  begin
    Top    := ((ClientRect.Bottom + ClientRect.Top) - FontHeight) div 2;
    Left   := ClientRect.Left  + TextSpase;
    Right  := (ClientRect.Right - TextSpase);
    Bottom := (((ClientRect.Bottom + ClientRect.Top) + FontHeight) div 2);
  end;
end;

function TContourTextControl.GetDrawText: String;
begin
  result := Text;
end;

function TContourTextControl.GetTextAlignment: TAlignment;
begin
  result := FAlignment;
end;

procedure TContourTextControl.Paint;
begin
  inherited Paint;
  DrawControlText(GetTextRect, GetDrawText, GetTextAlignment);
end;

procedure TContourTextControl.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  invalidate;
end;

procedure TContourTextControl.DoOnChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TContourTextControl.CMTextChanged(var Message: TMessage);
var
  R : TRect;
begin
  if not HandleAllocated then Exit;
  R:= GetTextRect;
  InvalidateRect(Self.Handle,@R ,True);
  DoOnChange;
end;

{ TWeekDaysPanel }

constructor TWeekDaysPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csFixedHeight];
  DoubleBuffered := True;
  Alignment := taCenter;
  RoundedCorners := False;
  ParentColor := False;
end;

procedure TWeekDaysPanel.DrawControlText(ATextRect: TRect; AText: TCaption; ATextAlignment: TAlignment);
var
  i : integer;
begin
  for i := 1 to 7 do
    DrawItem(ATextRect, i);
end;

procedure TWeekDaysPanel.DrawItem(ATextRect: TRect; DayIndex: integer);
var
  ItemRect : TRect;
  ItemBounds : TPoint;
  IncCoeff: integer;
begin
  ItemRect   := ATextRect;
  ItemBounds := GetItemBounds;
  IncCoeff   := (ClientWidth - GetMinBounds.X) div 7;
  with ItemRect do
  begin
    Top   := Top - 1;
    Bottom := Bottom -1;
    Left  := (ItemBounds.X + IncCoeff)*(DayIndex-1)+2;
    Right := Left + ItemBounds.X+IncCoeff;
  end;
  inherited DrawControlText(ItemRect,GetDayName(DayIndex),Alignment);
end;

procedure TWeekDaysPanel.Paint;
begin
  inherited Paint;
//  Canvas.Pen.Color := Font.Color;
//  Canvas.Polyline([Point(TextSpase,Height-2),Point(Width-TextSpase,Height-2)]);
end;

function TWeekDaysPanel.GetDayName(DayIndex: integer): string;
var
  di : integer;
begin
  di := ((((FirstDayOfWeek +1)mod 7)+DayIndex-1) mod 7) +1;
  result := Copy(ShortDayNames[di],0,2); 
end;

function TWeekDaysPanel.GetItemBounds: TPoint;
begin
  with result do
  begin
    x := Canvas.TextWidth('88')+(TextSpase*2);
    y := Canvas.TextHeight('8')+(TextSpase div 2);
  end;
end;

function TWeekDaysPanel.GetMinBounds: TPoint;
begin
  result := inherited GetMinBounds;
  result.X := (GetItemBounds.X*7)+3;
end;

{ TCalendarCellContainer }

constructor TCalendarCellContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csDisplayDragImage] - [csFixedHeight];
  DoubleBuffered := True;
 // DragMode := dmAutomatic;
  FMinBoundsX:= 0;
  FMinBoundsY:= 0;
  FAlignment  := taCenter;
  FCellKind := ckNone;
  FContainObject := nil;
  FSelected := False;
  FDragging := False;
  FOldContainerWndProc:=nil;
end;

destructor TCalendarCellContainer.Destroy;
begin
  if Assigned(FContainObject) then FContainObject.Free;
  inherited;
end;

procedure TCalendarCellContainer.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style + WS_CLIPCHILDREN;
end;

procedure TCalendarCellContainer.CreateWnd;
begin
  if(Parent is TCustomCalendar) then DisableAdjust;
  inherited CreateWnd;
  EnableAdjust;
end;

procedure TCalendarCellContainer.AdjustBounds;
begin
  inherited AdjustBounds;
  if IsContainer and Assigned(FContainObject) then
    with FContainObject do
    begin
      Top := GetTextRect.Bottom +2;
      Left :=  2;
      Width  := Self.ClientWidth  - Left -2;
      Height := Self.ClientHeight - Top - 2;
    end;
end;

function TCalendarCellContainer.GetMinBounds: TPoint;
begin
  // запоминаем, чтобы не дергать канву попусту
  if FMinBoundsX = 0 then
    FMinBoundsX:= Canvas.TextWidth('88')+(TextSpase*2);
  if FMinBoundsY = 0 then
    FMinBoundsY:= Canvas.TextHeight('8')+(TextSpase div 2);

  result.x := FMinBoundsX;
  result.y := FMinBoundsY;
end;

procedure TCalendarCellContainer.SetDateValue(const Value: TDateTime);
begin
//  if(Value < 1)or(Value>31) then Exit;
  if (FDate = Value) then Exit;
  FDate := Value;
  inherited Text := IntToStr(DayOf(FDate));
end;

procedure TCalendarCellContainer.AutoPlaceControl(AWCoeff,AHCoeff:integer);
var
   NewWidth, NewHeight, NewLeft, NewTop : integer;
   CellX, CellY : integer;
   bWCoeff : Double;

   MinBounds : TPoint;
   CalendarGrid : TCustomCalendar;
begin
  if not(Assigned(Parent))and(Parent is TCustomCalendar) then exit;
  CalendarGrid := TCustomCalendar(Parent);

 // DisableAdjust;
  MinBounds := GetMinBounds;

  CellX := FCellIndex-((FCellIndex div CalendarGrid.HCellsCount)*CalendarGrid.HCellsCount);
  CellY := FCellIndex div CalendarGrid.HCellsCount;

  NewTop   := (CellY)*(MinBounds.Y+AHCoeff)+2+
              (CalendarGrid.WeekDays.Height*integer(CalendarGrid.WeekDays.Visible));
  NewHeight:= (MinBounds.Y+AHCoeff-ord(AHCoeff>0));

//RAVINSKY 
//NewLeft  := (CellX)*(MinBounds.X+AWCoeff)+2;
//NewWidth := (MinBounds.X+AWCoeff-ord(AWCoeff>0));

  bWCoeff  := ((CalendarGrid.ClientWidth-3) / CalendarGrid.HCellsCount)-MinBounds.X;
  NewLeft  := Trunc(( CellX )*(MinBounds.X+bWCoeff))+2;
  NewWidth := Trunc((CellX+1)*(MinBounds.X+bWCoeff))+1-NewLeft;

  SetBounds(NewLeft,NewTop,NewWidth,NewHeight);
//  EnableAdjust;
end;

function TCalendarCellContainer.GetTextRect: TRect;
begin
  if IsContainer then
    with Result do
    begin
      Top    := TextSpase div 2;
      Left   := TextSpase;
      Right  := Self.ClientWidth - Left - TextSpase;// Left + Canvas.TextWidth('00');
      Bottom := Top  + Canvas.TextHeight('0');
    end
  else
    result := inherited GetTextRect;
 // result := Rect(0,0,0,0);
end;


function TCalendarCellContainer.GetText: string;
var
  ATextRect : TRect;
  aResult : String;
begin
  result := Inherited Text;
  if IsContainer
    and (CellKind<>ckItem)
    then
  begin
    aResult := result +' '+ AltMonthNames[MonthOf(FDate)];
    ATextRect := GetTextRect;
    if Canvas.TextWidth(aResult) > (ATextRect.Right - ATextRect.Left) then
    begin
      aResult := AltMonthNames[MonthOf(FDate)];
      result:= result +' '+ Copy(aResult,1,Min(3,Length(aResult)));
    end
    else
    begin
      if Selected and (Canvas.TextWidth(aResult+' 2000') < (ATextRect.Right - ATextRect.Left))
        then result:= aResult + ' ' +  IntToStr(YearOf(FDate))
        else result:= aResult;
    end;
  end;
end;

function TCalendarCellContainer.GetDrawText: String;
begin
  result := Text;
end;

function TCalendarCellContainer.GetTextAlignment: TAlignment;
var
  ATextRect : TRect;
begin
  ATextRect := GetTextRect;
  if Canvas.TextWidth(Text) > (ATextRect.Right - ATextRect.Left) then
    result := taLeftJustify
  else
     result := inherited GetTextAlignment;
end;


procedure TCalendarCellContainer.Paint;
begin
  inherited Paint;
  if IsContainer then FContainObject.Invalidate;
end;

procedure TCalendarCellContainer.DrawControl;
begin
  if Selected then
  begin
    //Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clHighlight
  end
  else
  begin
    //Canvas.Brush.Style := bsClear;
    Canvas.Brush.Color :=  Color;
  end;
  inherited DrawControl;
end;

procedure TCalendarCellContainer.DrawControlText(ATextRect: TRect; AText: TCaption; ATextAlignment: TAlignment);
begin
  if CellKind = ckNone then Exit;
  // Canvas.Font.Color := ColorToRGB(Font.Color)+ColorToRGB(($7F7F7F))*ord(CellKind<>ckCurrentMonth);
  if (CellKind<>ckCurrentMonth) and (CellKind<>ckItem) then
    Canvas.Font.Color := ChangeBrightnessByValue(Canvas.Font.Color,215)
  else
  if Selected then
    Canvas.Font.Color := clHighlightText
  else
    Canvas.Font.Color := Font.Color;

  inherited DrawControlText(ATextRect, Text, ATextAlignment);
end;

procedure TCalendarCellContainer.SetCellKind(const Value: TCalendarCellKind);
begin
  FCellKind := Value;
  Invalidate;
end;

function TCalendarCellContainer.GetSelected: Boolean;
begin
  result := FSelected;
end;

procedure TCalendarCellContainer.SetSelected(const Value: Boolean);
var
  CanSelect : Boolean;
begin
  if Selected = Value then Exit;
  CanSelect := True;
  if (Value)and(Assigned(FCanSelectEvent))then FCanSelectEvent(Self,CanSelect);
  FSelected := CanSelect and Value;
  if IsContainer then
  begin
    if Selected then
      begin
        ContainObject.Perform(CM_ENTER,0,0);
        TCustomCalendar(Owner).DoOnActivateCell;
      end
    else
      begin
        ContainObject.Perform(CM_EXIT,0,0);
        TCustomCalendar(Owner).DoOnDeactivateCell(self);
      end;
  end;
  Invalidate;
{  if IsContainer then
  begin
    if CanDrawContour then
      FContainObject.SetFocus
    else
      Self.Parent.SetFocus;
  end;}
end;

procedure TCalendarCellContainer.SetDropTarget(const Value: TControl);
var
  DropControlKind : TDropControlKind;
begin
  if FDropTarget = Value then Exit;
  FDropTarget := Value;
  if Assigned(FDropTarget) then
  begin
  if FDropTarget = Self then
      DropControlKind := dcCell
    else
      DropControlKind := dcContainObject;
  end    
  else
    DropControlKind := dcNone;
  if Assigned(OnDropTarget) then OnDropTarget(FDropTarget, DropControlKind);
end;

function TCalendarCellContainer.GetIsContainer: Boolean;
begin
  Result := Assigned(ContainObject) and ContainObject.Visible;
end;

procedure TCalendarCellContainer.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button= mbLeft) and IsContainer then
    ContainObject.SetFocus;
end;

procedure TCalendarCellContainer.UpdateTracking(aControl : TControl);
var
  FMouseInControl : Boolean;
begin
  if Enabled then
  begin
    FMouseInControl := (AControl = Self) or (ContainsControl(AControl));
    CanDrawContour := FMouseInControl and (not FSelected);
    UpdateWindow(Handle);
    if FDragging then
      DropTarget := AControl;
  end;
end;

procedure TCalendarCellContainer.WMAutoPlace(var Message: TMessage);
begin
  with Message do
    AutoPlaceControl(WParam,LParam);
end;

procedure TCalendarCellContainer.WMMouseMove(var Message: TWMMouseMove);
var
  P: TPoint;
  aControl : TControl;
begin
  inherited;
  GetCursorPos(P);
  aControl := FindDragTarget(P, True);
  UpdateTracking(aControl);
  MouseCapture := ((aControl = Self)or(ContainsControl(aControl)))and (not FSelected);
end;

procedure TCalendarCellContainer.WMLButtonDown( var Message: TWMLButtonDown);
var
  P: TPoint;
  aControl : TControl;
begin
  P:= Point(Message.XPos, Message.YPos);
  P:= ClientToScreen(P);

  aControl := FindDragTarget(P, True);

  if isContainer and Assigned(aControl)and(aControl.Parent = Self) then
  begin
    // по этому флагу блокируем ранюю смену €чейки
    // чтобы не выдел€ть дважды
    MouseCapture:= False; // по этому флагу блокируем ранюю смену €чейки
//   TDraggedControl(Control).DragMode := dmAutomatic;
    P:= aControl.ScreenToClient(P);
    Message.Pos:= SmallPoint(P.X, P.Y);
    aControl.WindowProc(TMessage(Message));
    MouseCapture:= True;

    Self.Selected := True;
    inherited;
  end
  else
    begin
      inherited;
      Selected := True;
    end;
end;

procedure TCalendarCellContainer.WMCaptureChanged(var Message: TMessage);
begin
  inherited;
  CanDrawContour := False;
end;

procedure TCalendarCellContainer.CMDrag(var Message: TCMDrag);
var
  P: TPoint;
  Control : TControl;
//  IsDropTarget : Boolean;
begin
  FDragging := True;
  GetCursorPos(P);
  Control := FindDragTarget(P, True);
//  IsDropTarget := (Control = Self)or(Assigned(Control)and(Control.Parent = Self));
  with Message, DragRec^ do
  begin
    case DragMessage of
      dmDragEnter:begin
                    Source.HideDragImage;
                    UpdateTracking(Control);
                  end;
      dmDragMove :begin
                    Source.ShowDragImage;
                  end;
      dmDragLeave:begin
                    Source.HideDragImage;
                    UpdateTracking(Control);
                  end;
      dmDragDrop, dmDragCancel : ;
      dmFindTarget: Result := LongInt(Control);
    end;
  end;
  if Control = Self then Inherited;
  if Message.DragMessage in [dmDragDrop, dmDragCancel] then
    DropTarget := nil;
  FDragging := False;
end;

procedure TCalendarCellContainer.NewContainerWndProc(var Message : TMessage) ;
begin
  case Message.Msg of
    WM_CREATE    :begin
                    FOldContainerWndProc(Message);
                    InitializeFlatSB(Self.ContainObject.Handle);
                    FlatSB_ShowScrollBar(Self.ContainObject.Handle, SB_HORZ, False);
                 end;
    WM_NCHITTEST : begin
                     FOldContainerWndProc(Message);
                     // корректируем зону горизонтального скроллинга и зону в правом нижнем углу
                     case message.Result of
                       HTHSCROLL: message.Result:= HTCLIENT;
                       HTGROWBOX: message.Result:= HTVSCROLL;
                     end;
                   end;
    WM_NCCALCSIZE: begin
                    FlatSB_ShowScrollBar(Self.ContainObject.Handle, SB_HORZ, False);
                    {ƒобавлено 2010, толщина нижнего скроллинга вли€ет на вертикальный скроллинг}
                    FlatSB_SetScrollProp(Self.ContainObject.Handle, WSB_PROP_CYHSCROLL, 0, True);

                    FOldContainerWndProc(Message);  // тут проблемы. имеенно в конце вызывваем
                                                    // вроде так лучше отрисовыввсааетс€ скроллинг
                   end;
    WM_LBUTTONDOWN: begin
                      TDraggedControl(Self.ContainObject).DragMode := dmAutomatic;
                      FOldContainerWndProc(Message);
                     // Self.WndProc(Message);
                    end;
    CM_DRAG      : begin
                     FOldContainerWndProc(Message);
                     Self.WndProc(Message);
                   end;
    WM_KEYDOWN:    begin
                     if Message.WParamLo = VK_LEFT then
                       SendMessage(TWinControl(Owner).Handle, WM_VSCROLL, SB_LEFT, 0) else
                     if Message.WParamLo = VK_RIGHT then
                       SendMessage(TWinControl(Owner).Handle, WM_VSCROLL, SB_RIGHT, 0) else

                       FOldContainerWndProc(Message);
                   end;
    WM_MOUSEMOVE : begin
                     FOldContainerWndProc(Message);
                     Self.WndProc(Message);
                   end;
    WM_LBUTTONDBLCLK: begin
                        FOldContainerWndProc(Message);
                        if Assigned(OnDblClickContent) then OnDblClickContent(Self);
                      end;
    CM_ENTER      :begin
                     FOldContainerWndProc(Message);
                     if MouseCapture then
                       begin
                         Self.Selected := True;
                       end;
                 //  никак не получает отрисовать нижнюю кнопку вертикального скроллинга
                 //  FlatSB_SetScrollProp(Self.ContainObject.Handle, WSB_PROP_CYHSCROLL, 0, True);
                 //  FlatSB_ShowScrollBar(Self.ContainObject.Handle,SB_VERT, True);
                 //  FlatSB_EnableScrollBar( Self.ContainObject.Handle, SB_VERT, ESB_ENABLE_BOTH);
                   end;
   CM_EXIT      :  begin
                     Selected := False;
                     FOldContainerWndProc(Message);
//                   FlatSB_EnableScrollBar( Self.ContainObject.Handle, SB_VERT, 4{ESB_DISABLE_BOTH});
                   end
    else           FOldContainerWndProc(Message);
  end;
end;

procedure TCalendarCellContainer.SetText(const Value: string);
begin
  inherited Text := Value;
end;

{ TCustomCalendar }

constructor TCustomCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GetLocalParams;
  DoubleBuffered := True;
  FSelectedIndex := -1;
  FFirstCellDate := DATE_NONE;
  FWCoeff :=-1;
  FHCoeff :=-1;
  FRowCount := 1;
  FColCount := 7;
  FVCellsCount := 1;
  FHCellsCount := 7;
  FLockCount := 0;
  ParentColor := False;
  RoundedCorners := False;
  WeekDays    := TWeekDaysPanel.Create(Self);
  with WeekDays do
  begin
    ParentFont := False;
    ParentColor := True;
    Parent := Self;
  end;
  FCurrentDate := Today;
  FViewType := cvWeeks;
end;

destructor TCustomCalendar.Destroy;
begin
  SetCellsCount(0);
  WeekDays.Free;
  inherited;
end;

procedure TCustomCalendar.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style +  WS_VSCROLL;
end;

procedure TCustomCalendar.CreateWnd;
begin
  inherited CreateWnd;
  ReInitScrollBar;
end;

procedure TCustomCalendar.ReInitScrollBar;
begin
  SetScrollRange(Handle, SB_VERT,-FScrollUnit ,FScrollUnit, True);
  SetScrollPos(Handle, SB_VERT,0, False );
  FScrollPos := 0;
end;

procedure TCustomCalendar.SetCellsCount(ACount : integer);
var
  i: integer;
  OldCount : integer;
begin
  OldCount := Length(FCells);
  if OldCount = ACount then Exit;
  if OldCount > ACount then
  for i:= ACount to  Length(FCells)-1 do
    if Assigned(FCells[i]) then
    begin
      if FCells[i].IsContainer then
      begin
        DoOnDestroingContainer(FCells[i],FCells[i].ContainObject);
      end;
      FreeAndNil(FCells[i]);
    end;
  SetLength(FCells,ACount);
 // FSelectedIndex := -1;
end;

procedure TCustomCalendar.CreateCells;
var
  i:integer;
begin
{
  SetCellsCount(VCellsCount*HCellsCount);
  for i:= Low(FCells) to High(FCells) do
  begin
    if not Assigned(FCells[i]) then
    begin
      FCells[i]:= TCalendarCellContainer.Create(Self);
      FCells[i].FCellIndex := i;
      FCells[i].ContourColor := clHighlight	;
      FCells[i].OnCanSelect:= DoOnCanSelect;
      FCells[i].OnDropTarget:= DoDropTarget;
      if Assigned(OnDblClickCellCaption) then
        FCells[i].OnDblClick:=OnDblClickCellCaption;
      if Assigned(OnDblClickCellContent) then
        FCells[i].OnDblClickContent:=OnDblClickCellContent;
      FCells[i].Parent := Self;
    end;
    FCells[i].FSelected := False;
    FCells[i].CellKind := ckNone;
  end;
  FSelectedIndex := -1;
  {}
end;

procedure TCustomCalendar.CalcFirstCellDate;
var
  RI, SDWI, SDAI: integer;
 // NewFirstDate : TDateTime;
begin
   case FViewType of
    cvWeeks:begin
              SDWI := DayOfTheWeek(FCurrentDate);
              SDAI := ((((SDWI+6)mod 7)-FirstDayOfWeek +7 )mod 7);
              if FSelectedIndex > -1 then
                RI := FSelectedIndex - SDAI
              else
                RI := ((VCellsCount div 2)*HCellsCount);

//            FirstCellDate := IncDay(FCurrentDate,-(SDAI+RI));

              FirstCellDate := IncDay(FCurrentDate,-(SDAI));
              while FCurrentDate < FirstCellDate do
                FirstCellDate:= IncDay(-7);
              while IncDay(FirstCellDate, 7*VCellsCount) <= FCurrentDate do
                FirstCellDate:= IncDay(+7);

              FSelectFrom:= FirstCellDate;
              FSelectTo:= IncDay(FSelectFrom, 7*VCellsCount);
            end;
    cvMonths:begin
              SDWI := DayOfTheWeek(StartOfTheMonth(FCurrentDate));
              SDAI := ((((SDWI+6)mod 7)-FirstDayOfWeek +7 )mod 7);
              RI := trunc(FCurrentDate-StartOfTheMonth(FCurrentDate));

              FirstCellDate  := IncDay(FCurrentDate,-(SDAI+RI));
              FSelectFrom:= StartOfTheMonth(FCurrentDate);
              FSelectTo:= EndOfTheMonth(FCurrentDate);
            end;
    cvDays,cvCells  :
            begin
              SDAI := (HCellsCount div 2)+(HCellsCount*(FVCellsCount div 2));

              FirstCellDate  := IncDay(FCurrentDate,-(SDAI));
              FSelectFrom:= FirstCellDate;
              FSelectTo:= IncDay(FSelectFrom, HCellsCount*FVCellsCount);
            end;
   end;
end;

procedure TCustomCalendar.SetCellsValues;
var
  i,N : integer;
begin
  N:= -1;
  SetCellsCount(VCellsCount*HCellsCount);

  for i:= Low(FCells) to High(FCells) do
  begin

    if not Assigned(FCells[i]) then
    begin
      FCells[i]:= TCalendarCellContainer.Create(Self);
      FCells[i].FCellIndex := i;
      FCells[i].ContourColor := clHighlight	;
      FCells[i].OnCanSelect:= DoOnCanSelect;
      FCells[i].OnDropTarget:= DoDropTarget;
      if Assigned(OnDblClickCellCaption) then
        FCells[i].OnDblClick:=OnDblClickCellCaption;
      if Assigned(OnDblClickCellContent) then
        FCells[i].OnDblClickContent:=OnDblClickCellContent;
      FCells[i].Parent := Self;
    end;

    FCells[i].CurrentDate := IncDay(FFirstCellDate,i);

    if FViewType = cvCells then begin
      if Assigned(FOnFillingCell) then
        FOnFillingCell(FCells[i]);
      FCells[i].CellKind := ckItem;
    end else
      if (FCells[i].CurrentDate< StartOfTheMonth(FCurrentDate)) and(FViewType = cvMonths) then
        FCells[i].CellKind := ckPredMonth
      else
      if (FCells[i].CurrentDate > EndOfTheMonth(FCurrentDate)) and(FViewType = cvMonths) then
        FCells[i].CellKind := ckNextMonth
      else
        FCells[i].CellKind := ckCurrentMonth;
   
    if ViewType = cvWeeks then
    begin
      if Odd(MonthOf(FCells[i].CurrentDate)) then
        FCells[i].Color := ChangeBrightnessByDelta(Color,-15)
      else
        FCells[i].Color := ChangeBrightnessByDelta(Color,-30);
    end
    else
    if isContainer then
    begin
      if (FCells[i].CellKind = ckCurrentMonth)
         or (FCells[i].CellKind = ckItem) then
        FCells[i].Color := ChangeBrightnessByDelta(Color,-30)
      else
        FCells[i].Color := Color;
    end;

    if (FCells[i].CurrentDate = FCurrentDate) then
      begin
        FCells[i].FSelected:= True;
        N:= i;
      end else
      begin
        FCells[i].FSelected:= False;
      end;
  end;
  FSelectedIndex := N;
end;

procedure TCustomCalendar.AdjustBounds;
var
  WCoeff, HCoeff : integer;
  MinBounds : TPoint;
  i: integer;
begin
  inherited AdjustBounds;
  MinBounds := GetMinBounds;
  if WeekDays.Visible then
   WeekDays.SetBounds(0,0,ClientWidth,WeekDays.Height);
  WCoeff := (ClientWidth - MinBounds.X )div FHCellsCount;
  HCoeff := (ClientHeight - MinBounds.Y)div VCellsCount;
  FWCoeff := WCoeff;
  FHCoeff := HCoeff;
  for i:= Low(FCells) to High(FCells) do
    SendMessage(FCells[i].Handle,WM_AUTOPLACE,FWCoeff,FHCoeff);
end;

function TCustomCalendar.GetMinBounds: TPoint;
begin
  with result do
  begin
    x := (Canvas.TextWidth('88')+(TextSpase*2))*FHCellsCount + 3;
    y := (Canvas.TextHeight('8')+(TextSpase div 2))*VCellsCount + 2;
  end;
  result.y := result.y + WeekDays.GetMinBounds.Y*integer(WeekDays.Visible);
  if (result.X < WeekDays.GetMinBounds.X)and WeekDays.Visible then
    result.X := WeekDays.GetMinBounds.X;
end;

function TCustomCalendar.GetVCellsCount: integer;
var D,M,Y: Word;
    D1,D2: TDateTime;
begin
  if FViewType = cvMonths then
    begin
      DecodeDate(CurrentDate, Y, M, D);
      D1:= EncodeDate(Y, M, 1);
      DecodeDate(IncMonth(D1,1), Y, M, D);
      D2:= IncDay(EncodeDate(Y, M, 1),-1);

      D:= DaysBetween(D2,D1) + 1 + (((DayOfTheWeek(D1)+6)mod 7)-FirstDayOfWeek +7 ) mod 7;
      if D Mod 7 = 0 then Result:= D div 7
                     else Result:= D div 7 +1;
    end
  else
    Result:= FVCellsCount;
end;

function TCustomCalendar.GetCell(const Index: Integer): TCalendarCellContainer;
begin
  Result:= FCells[Index];
end;

function TCustomCalendar.GetEndCellDate: TDate;
begin
  Result:= IncDay(FFirstCellDate, FColCount*FRowCount);
end;

procedure TCustomCalendar.DoOnCanSelect(Sender: TObject; var CanSelect: Boolean);
var
  Container :  TCalendarCellContainer;
begin
  CanSelect := False;
  Container := TCalendarCellContainer(Sender);
  case Container.CellKind of
  ckPredMonth: begin
                 if Assigned(Parent)and(Parent is TCustomCalendar)then
                    IncLine(-1)
                 else
                 begin
                    CurrentDate:= IncMonth(CurrentDate,-1);
                    ReInitControl;
                 end;
               end;
  ckCurrentMonth,ckItem:begin
                   if FSelectedIndex >-1 then Self.FCells[FSelectedIndex].Selected := False;
                   CanSelect := True;
                   if FSelectedIndex > -1 then DoOnDeactivateCell(FCells[FSelectedIndex]);
                   FSelectedIndex := Container.FCellIndex;
                   CurrentDate:= Container.CurrentDate;
                 end;
  ckNextMonth: begin
                 if Assigned(Parent)and(Parent is TCustomCalendar)then
                    IncLine(1)
                 else
                 begin
                    CurrentDate:= IncMonth(CurrentDate,1);
                    ReInitControl;
                 end;
               end;
  end;
end;

procedure TCustomCalendar.DoDropTarget(DropControl : TControl; DropControlKind : TDropControlKind);
begin
  if Assigned(OnDropTargetChange) then OnDropTargetChange(DropControl, DropControlKind );
end;

procedure TCustomCalendar.DoOnSetContourColor;
var
  i : integer;
begin
  for i:= Low(FCells) to High(FCells) do
    FCells[i].ContourColor := ContourColor;
  inherited DoOnSetContourColor;
end;

procedure TCustomCalendar.DoOnCreatingContainer(Sender : TCalendarCellContainer; var AContainerObject: TWinControl);
begin
  AContainerObject := nil;
  if Assigned(FOnCreatingContainer) then FOnCreatingContainer(Sender,AContainerObject);
  if not Assigned(AContainerObject) then Exit;
  AContainerObject.Parent := Sender;
  Sender.FOldContainerWndProc  := AContainerObject.WindowProc;
  AContainerObject.WindowProc := Sender.NewContainerWndProc;
end;

procedure TCustomCalendar.DoOnDestroingContainer(Sender: TCalendarCellContainer; AContainerObject: TWinControl);
begin
  if Assigned(FOnDestroingContainer) then
  begin
    if Assigned(AContainerObject) then
    begin
      AContainerObject.WindowProc := Sender.FOldContainerWndProc;
      Sender.FOldContainerWndProc := nil;
    end;
    FOnDestroingContainer(Sender,AContainerObject);
  end;
end;

procedure TCustomCalendar.DoOnFillingContainer(Sender: TCalendarCellContainer; AContainerObject: TWinControl);
begin
  if Assigned(FOnFillingContainer) then FOnFillingContainer(Sender,AContainerObject);
end;

procedure TCustomCalendar.DoOnClearContainer(Sender: TCalendarCellContainer; AContainerObject: TWinControl);
begin
   if Assigned(FOnClearContainer) then FOnClearContainer(Sender,AContainerObject);
end;

procedure TCustomCalendar.DoOnActivateCell;
var
  Cell : TCalendarCellContainer;
  CObject : TWinControl;
begin
  if not(IsContainer and HandleAllocated) then  Exit;
  Cell := nil;
  CObject := nil;
  if FSelectedIndex > -1 then
  begin
    Cell := Self.FCells[FSelectedIndex];
    if Assigned(Cell)then
      CObject := Cell.FContainObject;
  end;
  if Assigned(FOnActivateCell) then FOnActivateCell(Cell,CObject);
end;

procedure TCustomCalendar.DoOnDeactivateCell(Sender: TCalendarCellContainer);
var
  CObject : TWinControl;
begin
  if not Sender.IsContainer then Exit;
  CObject := Sender.ContainObject;
  if Assigned(FOnDeactivateCell) then FOnDeactivateCell(Sender,CObject);
end;


procedure TCustomCalendar.SetCurrentDate(const Value: TDateTime);

begin
  if FCurrentDate = Trunc(Value) then Exit;
  FCurrentDate := Trunc(Value);
  if Assigned(FOnDateChange) then FOnDateChange(Self, FCurrentDate);
  DoOnActivateCell;
 // if not IsLoading then ReInitControl;
end;

procedure TCustomCalendar.SetFirstCellDate(const Value: TDate);
begin
  if (FFirstCellDate = Value)then Exit;
  FFirstCellDate := Value;
end;

procedure TCustomCalendar.SetIsContainer(const Value: Boolean);
begin
  if FIsContainer = Value then Exit;
  FIsContainer := Value;
end;

procedure TCustomCalendar.SetIsLoading(const Value: Boolean);
begin

end;

procedure TCustomCalendar.SetViewType(const Value: TCalendarViewType);
begin
  FViewType := Value;
  case FViewType of
   cvMonths:begin
              FHCellsCount := 7;
              FVCellsCount := 6;
              ScrollUnit := 12;
            end;
   cvWeeks :begin
              FHCellsCount := 7;
              FVCellsCount := RowCount;
              ScrollUnit := 52;
            end;
   cvDays  :begin
              FHCellsCount := ColCount;
              FVCellsCount := RowCount;
              ScrollUnit := (366 div ColCount)+1;
            end;
   cvCells :begin
              FHCellsCount := ColCount;
              FVCellsCount := RowCount;
              ScrollUnit := trunc(RowCount/2)
            end;
  end;
  WeekDays.Visible := (FViewType in [cvWeeks, cvMonths]);
  FirstCellDate := DATE_NONE;

  ReInitControl;
end;

procedure TCustomCalendar.SetParams(AViewType:TCalendarViewType; ARowCount: integer = 1; AColCount:integer = 7);
var NeedReInit: Boolean;
    aRow, aCol: Integer;
begin
  NeedReInit:= False;

  if (FViewType <> AViewType) then
    begin
      FViewType:= AViewType;
      NeedReInit:= True;
    end;

  case AViewType of
    cvMonths: begin
                aRow:= 6;
                aCol:= 7;
                ScrollUnit := 12;
              end;
    cvWeeks:  begin
                aRow:= ARowCount;
                aCol:= 7;
                ScrollUnit := 52;
              end;
    cvDays:   begin
                aRow:= 1;
                aCol:= 1;
                ScrollUnit := (366 div ColCount)+1;
              end;
    cvCells:  begin
                aRow:= ARowCount;
                aCol:= AColCount;
                ScrollUnit := trunc(RowCount/2)
              end;
  end;

  aCol:= Max(1, Min(7, aCol));
  if FHCellsCount <> aCol then
    begin
      FHCellsCount:= aCol;
      NeedReInit:= True;
    end;

  aRow:= Max(1, Min(14, aRow));
  if FVCellsCount <> aRow then
    begin
      FVCellsCount:= aRow;
      NeedReInit:= True;
    end;

  WeekDays.Visible := (FViewType in [cvWeeks, cvMonths]);

  if NeedReInit then
    ReInitControl;
end;

procedure TCustomCalendar.SetVCellsCount(const Value: integer);
begin
  if FVCellsCount = Value then Exit;
  FVCellsCount := Value;
  ReInitControl;
end;

procedure TCustomCalendar.SetHCellsCount(const Value: integer);
begin
  if FHCellsCount = Value then Exit;
  FHCellsCount := Value;
  ReInitControl;
end;

procedure TCustomCalendar.SetRowCount(const Value: integer);
var
  V : integer;
begin
  if FRowCount = Value then Exit;
  if Value < 1  then
    V := 1
  else
  if Value > 14 then
    V := 14
  else
    V := Value;
  FRowCount := V;
  VCellsCount := V;
end;

procedure TCustomCalendar.SetColCount(const Value: integer);
var
  V : integer;
begin
  if FColCount = Value then Exit;
  if Value < 1  then
    V := 1
  else
  if Value > 7 then
    V := 7
  else
    V := Value;
  FColCount := V;
  HCellsCount := V;
end;

procedure TCustomCalendar.SetScrollUnit(const Value: integer);
begin
  FScrollUnit := Value;
  if HandleAllocated then ReInitScrollBar;
end;

procedure TCustomCalendar.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TCustomCalendar.EndUpdate(DoReInitControl: Boolean = True);
begin
  Dec(FLockCount);
  if (FLockCount = 0) and (DoReinitControl) then ReInitControl;
end;

procedure TCustomCalendar.ReInitControl;
var i: Integer;
begin
  if 0 < FLockCount then
    begin
      Inc(FLockInitCount);
      Exit;
    end;

  BeginUpdate;
  if isContainer then
    ClearCellContainers;

  CalcFirstCellDate;
  SetCellsValues;

  if Assigned(FOnRangeChange) then
    FOnRangeChange(self, FSelectFrom, FSelectTo);

  if isContainer then
    CreateCellsContainers;

  if HandleAllocated then
    AdjustBounds;

  EndUpdate(False);
  FLockInitCount:= 0;
end;

procedure TCustomCalendar.CreateCellsContainers;
var
  i : integer;
begin
  if (IsContainer)and(not (csDesigning in ComponentState))then
  begin
    for i:= Low(FCells) to High(FCells) do
    begin
        if not Assigned(FCells[i].FContainObject) then
          DoOnCreatingContainer(FCells[i],FCells[i].FContainObject);
        if not Assigned(FCells[i].FContainObject) then Continue;
        FCells[i].FContainObject.Visible := (FCells[i].CellKind = ckCurrentMonth) or (FCells[i].CellKind = ckItem);
        if FCells[i].HandleAllocated then FCells[i].AdjustBounds;
        if FCells[i].FContainObject.Visible then
        begin
          DoOnFillingContainer(FCells[i],FCells[i].FContainObject);
          FCells[i].Invalidate;
        end;  {}
    end;
  //  DoOnActivateCell;
  end;
end;

procedure TCustomCalendar.ClearCellContainers;
var
  i: integer;
begin
  if (IsContainer)and(not (csDesigning in ComponentState))then
    begin
      // ќбработчик с пустым указателем выполн€ет очистку общую дл€ всех €чеек (рвет св€зи кеша с графикой)
      DoOnClearContainer(nil, nil);

      for i:= Low(FCells) to High(FCells) do
        if assigned(FCells[i].FContainObject) then
          DoOnClearContainer(FCells[i], FCells[i].FContainObject);
    end;
end;

procedure TCustomCalendar.RefreshContent;
begin
  GetLocalParams;
  ReInitControl;
end;

procedure TCustomCalendar.ClearContent;
begin
  SetCellsCount(0);
  FSelectedIndex := -1;
end;

procedure TCustomCalendar.CMColorChanged(var Message: TMessage);
var
  i : integer;
begin
  inherited;
  for i:= Low(FCells) to High(FCells) do
   if ViewType = cvWeeks then
    begin
      if Odd(MonthOf(FCells[i].CurrentDate)) then
        FCells[i].Color := ChangeBrightnessByDelta(Color,-10)
      else
        FCells[i].Color := ChangeBrightnessByDelta(Color,-30);
    end
    else
    if isContainer then
    begin
      if (FCells[i].CellKind = ckCurrentMonth)
         or (FCells[i].CellKind = ckItem)
         then
        FCells[i].Color := ChangeBrightnessByDelta(Color,-20)
      else
        FCells[i].Color := Color;
    end;
end;

procedure TCustomCalendar.IncCell(const Coeff: integer);
begin
//TODO: хорошо бы сделать листание
//TODO: если указатель мыши над соседним днем - клавиатура не работает (((
  // пока сделал движение только по отображаемым дн€м
    begin
      FCells[FSelectedIndex].Selected := False;
      sendMessage(FCells[FSelectedIndex].Handle, CM_ENTER, 0 , 0);

      CurrentDate := IncDay(FCurrentDate,Coeff);

      if (CurrentDate<FirstCellDate) or (EndCellDate<=CurrentDate) then
        case FViewType of
        cvMonths:begin
                   //CurrentDate := IncMonth(FCurrentDate,Coeff);
                 end;
        cvWeeks :begin
                   //CurrentDate := IncWeek(CurrentDate,Coeff);
                   if (FCurrentDate < FirstCellDate)
                   or (FCurrentDate > IncDay(FirstCellDate,(FVCellsCount*FHCellsCount)-1)) then
                     FirstCellDate := IncWeek(FirstCellDate,Coeff);
                 end;
        cvDays,cvCells  :begin
                   //CurrentDate := IncDay(CurrentDate,Coeff*ColCount);
                   if (FCurrentDate < FirstCellDate)
                   or (FCurrentDate > IncDay(FirstCellDate,(FVCellsCount*FHCellsCount)-1)) then
                     FirstCellDate := IncDay(FirstCellDate,Coeff*ColCount);
                 end;
        end;

      ReInitControl;
      FCells[FSelectedIndex].FContainObject.SetFocus;

      sendMessage(FCells[FSelectedIndex].Handle, CM_ENTER, 0 , 0);

      FScrollPos := FScrollPos + Coeff;
      SetScrollPos(Handle, SB_VERT,FScrollPos, True);
    end;
end;

procedure TCustomCalendar.IncLine(const Coeff: integer);
begin
  case FViewType of
  cvMonths:begin
             CurrentDate := IncMonth(FCurrentDate,Coeff);
           end;
  cvWeeks :begin
             CurrentDate := IncWeek(CurrentDate,Coeff);
             if (FCurrentDate < FirstCellDate)
             or (FCurrentDate > IncDay(FirstCellDate,(FVCellsCount*FHCellsCount)-1)) then
               FirstCellDate := IncWeek(FirstCellDate,Coeff);
           end;
  cvDays,cvCells  :begin
             CurrentDate := IncDay(CurrentDate,Coeff*ColCount);
             if (FCurrentDate < FirstCellDate)
             or (FCurrentDate > IncDay(FirstCellDate,(FVCellsCount*FHCellsCount)-1)) then
               FirstCellDate := IncDay(FirstCellDate,Coeff*ColCount);
           end;
  end;
  ReInitControl;
  FScrollPos := FScrollPos + Coeff;
  SetScrollPos(Handle, SB_VERT,FScrollPos, True);
end;

procedure TCustomCalendar.IncPage(const Coeff: integer);
begin
  case FViewType of
  cvMonths:begin
             SetScrollPos(Handle, SB_VERT, FScrollPos + Coeff, True);
             CurrentDate:= IncMonth(FCurrentDate,Coeff);
           end;
  cvWeeks :begin
             SetScrollPos(Handle, SB_VERT, FScrollPos + Max(VCellsCount,1)*Coeff, True);
             CurrentDate:= IncWeek(CurrentDate, Coeff);
           end;
  cvDays,cvCells  :begin
             SetScrollPos(Handle, SB_VERT, FScrollPos + Max(VCellsCount,1)*Max(HCellsCount,1)*Coeff, True);
             CurrentDate:= IncDay(CurrentDate,Coeff*Max(VCellsCount,1)*Max(HCellsCount,1));
           end;
  end;
  ReInitControl;
end;

procedure TCustomCalendar.IncThumb(const Coeff: integer);
var
 Delta : integer;
begin
  Delta := Coeff - FScrollPos;
  case FViewType of
  cvMonths:begin
            CurrentDate := IncMonth(FCurrentDate,Delta);
           end;
  cvWeeks :begin
             CurrentDate := IncWeek(CurrentDate,Delta);
             if (FCurrentDate < FirstCellDate)
             or (FCurrentDate > IncDay(FirstCellDate,(FVCellsCount*FHCellsCount)-1)) then
               FirstCellDate := IncWeek(FirstCellDate,Delta);
           end;
  cvDays,cvCells :begin
             CurrentDate := IncDay(CurrentDate,Delta*ColCount*Max(RowCount-1,1));
             if (FCurrentDate < FirstCellDate)
             or (FCurrentDate > IncDay(FirstCellDate,(FVCellsCount*FHCellsCount)-1)) then
               FirstCellDate := IncDay(FirstCellDate,Delta*ColCount*Max(RowCount-1,1));
           end;
  end;
  ReInitControl;
  FScrollPos := Coeff;
  SetScrollPos(Handle, SB_VERT,FScrollPos, True);
end;

procedure TCustomCalendar.WMVScroll(var Message: TWMVScroll);
begin
  case Message.ScrollCode of
  SB_ENDSCROLL:;
//  SB_LEFT: IncCell(-1);
//  SB_RIGHT:IncCell(+1);
  SB_LINEUP:IncLine(-1);
  SB_LINEDOWN:IncLine(+1);
  SB_PAGEUP:IncPage(-1);
  SB_PAGEDOWN:IncPage(+1);
  SB_THUMBPOSITION:;
  SB_THUMBTRACK:IncThumb(Message.Pos);
  end;
end;

{ TCellsScrollCalendar }

constructor TCellsScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  IsLoading := True;
  FSelectedIndex := -1;
  FFirstCellDate := DATE_NONE;
  FWCoeff :=-1;
  FHCoeff :=-1;
  FRowCount := 5;
  FColCount := 1;
  FLockCount := 0;
  ParentColor := False;
  WeekDays    := TWeekDaysPanel.Create(Self);
  with WeekDays do
  begin
    ParentFont := False;
    ParentColor := True;
    Parent := Self;
  end;
  FCurrentDate := Today;
  FViewType := cvCells;
  HorzScrollBar.Visible:=false;
end;

destructor TCellsScrollBox.Destroy;
begin
  SetCellsCount(0);
  WeekDays.Free;
  inherited;
end;

procedure TCellsScrollBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style +  WS_VSCROLL;
end;

procedure TCellsScrollBox.CreateWnd;
begin
  GetLocalParams;
  inherited CreateWnd;
  UnInitializeFlatSB(Handle);
  ReInitScrollBar;
  IsLoading := False;
end;

procedure TCellsScrollBox.ReInitScrollBar;
begin
//  SetScrollRange(Handle, SB_VERT,-FScrollUnit ,FScrollUnit, True);
  SetScrollPos(Handle, SB_VERT,0, True);
  FScrollPos := 0;
end;

procedure TCellsScrollBox.SetCellsCount(ACount : integer);
var
  i: integer;
  OldCount : integer;
begin
  OldCount := Length(FCells);
  if OldCount = ACount then Exit;
  if OldCount > ACount then
  for i:= ACount to  Length(FCells)-1 do
    if Assigned(FCells[i]) then
    begin
      if FCells[i].IsContainer then
      begin
        DoOnDestroingContainer(FCells[i],FCells[i].ContainObject);
      end;
      FreeAndNil(FCells[i]);
    end;
  SetLength(FCells,ACount);
 // FSelectedIndex := -1;
end;

procedure TCellsScrollBox.CreateCells;
var
  i:integer;
begin
  SetCellsCount(VCellsCount*HCellsCount);
  for i:= Low(FCells) to High(FCells) do
  begin
    if not Assigned(FCells[i]) then
    begin
      FCells[i]:= TCalendarCellContainer.Create(Self);
      FCells[i].FCellIndex := i;
      FCells[i].ContourColor := clHighlight	;
      FCells[i].OnCanSelect:= DoOnCanSelect;
      FCells[i].OnDropTarget:= DoDropTarget;
      if Assigned(OnDblClickCellCaption) then
        FCells[i].OnDblClick:=OnDblClickCellCaption;
      if Assigned(OnDblClickCellContent) then
        FCells[i].OnDblClickContent:=OnDblClickCellContent;
      FCells[i].Parent := Self;
    end;
    FCells[i].FSelected := False;
    FCells[i].CellKind := ckNone;
  end;
  FSelectedIndex := -1;
end;

procedure TCellsScrollBox.CalcFirstCellDate;
var
  RI, SDWI, SDAI: integer;
 // NewFirstDate : TDateTime;
begin
   case FViewType of
    cvWeeks:if  FirstCellDate= DATE_NONE then
            begin
              SDWI := DayOfTheWeek(FCurrentDate);
              SDAI := ((((SDWI+6)mod 7)-FirstDayOfWeek +7 )mod 7);
              if FSelectedIndex > -1 then
                RI := FSelectedIndex - SDAI
              else
                RI := ((VCellsCount div 2)*HCellsCount);
              FirstCellDate  := IncDay(FCurrentDate,-(SDAI+RI));
            end;
    cvMonths:begin
              SDWI := DayOfTheWeek(StartOfTheMonth(FCurrentDate));
              SDAI := ((((SDWI+6)mod 7)-FirstDayOfWeek +7 )mod 7);
              if SDAI=0 then SDAI := 7;
              RI := trunc(FCurrentDate-StartOfTheMonth(FCurrentDate));
              FirstCellDate  := IncDay(FCurrentDate,-(SDAI+RI));
            end;
    cvDays,cvCells  :if  FirstCellDate= DATE_NONE then
            begin
              SDAI := (HCellsCount div 2)+(HCellsCount*(FVCellsCount div 2));
              FirstCellDate  := IncDay(FCurrentDate,-(SDAI));
            end;
   end;
end;


procedure TCellsScrollBox.SetCellsValues;
var
  i : integer;
begin
  for i:= Low(FCells) to High(FCells) do
  begin
    FCells[i].CurrentDate := IncDay(FFirstCellDate,i);

    if FViewType = cvCells then begin
      if Assigned(FOnFillingCell) then
        FOnFillingCell(FCells[i]);
      FCells[i].CellKind := ckItem;
    end else
      if (FCells[i].CurrentDate< StartOfTheMonth(FCurrentDate))
      and(FViewType = cvMonths) then
        FCells[i].CellKind := ckPredMonth
      else
      if (FCells[i].CurrentDate > EndOfTheMonth(FCurrentDate))
      and(FViewType = cvMonths) then
        FCells[i].CellKind := ckNextMonth
      else
        FCells[i].CellKind := ckCurrentMonth;

    if ViewType = cvWeeks then
    begin
      if Odd(MonthOf(FCells[i].CurrentDate)) then
        FCells[i].Color := ChangeBrightnessByDelta(Color,-15)
      else
        FCells[i].Color := ChangeBrightnessByDelta(Color,-30);
    end
    else
    if isContainer then
    begin
      if (FCells[i].CellKind = ckCurrentMonth)
         or (FCells[i].CellKind = ckItem) then
        FCells[i].Color := ChangeBrightnessByDelta(Color,-30)
      else
        FCells[i].Color := Color;
    end;

    FCells[i].Selected := (FCells[i].CurrentDate = FCurrentDate);
   // if (FCells[i].CurrentDate = FCurrentDate) then FSelectedIndex := i;

  end;
end;

procedure TCellsScrollBox.AdjustBounds;
var
  WCoeff, HCoeff : integer;
  i: integer;
begin
  if WeekDays.Visible then
   WeekDays.SetBounds(0,0,ClientWidth,WeekDays.Height);
  WCoeff := (ClientWidth)div FHCellsCount;
  if FVCellsCount<>0 then
    HCoeff := (ClientHeight)div FVCellsCount
  else
    HCoeff := 0;
  FWCoeff := WCoeff;
  FHCoeff := HCoeff;
  for i:= Low(FCells) to High(FCells) do begin
    if Assigned(FCells[i].FContainObject) then
      if i>Low(FCells) then
        FCells[i].SetBounds(2,FCells[i-1].Top+FCells[i-1].Height+2,FWCoeff-4,FCells[i].FContainObject.Height+FCells[i].GetMinBounds.Y+4)
      else
        FCells[i].SetBounds(2,2,FWCoeff-4,FCells[i].FContainObject.Height+FCells[i].GetMinBounds.Y+4)
    else
      FCells[i].SetBounds(0,i*FHCoeff,FWCoeff,FHCoeff);
  end;
end;

procedure TCellsScrollBox.DoOnCanSelect(Sender: TObject;
  var CanSelect: Boolean);
var
  Container :  TCalendarCellContainer;
begin
  CanSelect := False;
  Container := TCalendarCellContainer(Sender);
  case Container.CellKind of
  ckPredMonth: begin
                 if Assigned(Parent)and(Parent is TCustomCalendar)then
                    IncLine(-1)
                 else
                 begin
                    CurrentDate:= IncMonth(CurrentDate,-1);
                    ReInitControl;
                 end;
               end;
  ckCurrentMonth,ckItem:begin
                   if FSelectedIndex >-1 then Self.FCells[FSelectedIndex].Selected := False;
                   CanSelect := True;
                   if FSelectedIndex > -1 then DoOnDeactivateCell(FCells[FSelectedIndex]);
                   FSelectedIndex := Container.FCellIndex;
                   CurrentDate:= Container.CurrentDate;
                 end;
  ckNextMonth: begin
                 if Assigned(Parent)and(Parent is TCustomCalendar)then
                    IncLine(1)
                 else
                 begin
                    CurrentDate:= IncMonth(CurrentDate,1);
                    ReInitControl;
                 end;
               end;
  end;
end;

procedure TCellsScrollBox.DoDropTarget(DropControl : TControl; DropControlKind : TDropControlKind);
begin
  if Assigned(OnDropTargetChange) then OnDropTargetChange(DropControl, DropControlKind );
end;


procedure TCellsScrollBox.DoOnCreatingContainer(Sender : TCalendarCellContainer;
  var AContainerObject: TWinControl);
begin
  AContainerObject := nil;
  if Assigned(FOnCreatingContainer) then FOnCreatingContainer(Sender,AContainerObject);
  if not Assigned(AContainerObject) then Exit;
  AContainerObject.Parent := Sender;
  Sender.FOldContainerWndProc  := AContainerObject.WindowProc;
  AContainerObject.WindowProc := Sender.NewContainerWndProc;
end;

procedure TCellsScrollBox.DoOnDestroingContainer(
  Sender: TCalendarCellContainer; AContainerObject: TWinControl);
begin
  if Assigned(FOnDestroingContainer) then
  begin
    if Assigned(AContainerObject) then
    begin
      AContainerObject.WindowProc := Sender.FOldContainerWndProc;
      Sender.FOldContainerWndProc := nil;
    end;
    FOnDestroingContainer(Sender,AContainerObject);
  end;
end;

procedure TCellsScrollBox.DoOnFillingContainer(Sender: TCalendarCellContainer;
   AContainerObject: TWinControl);
begin
  if Assigned(FOnFillingContainer) then FOnFillingContainer(Sender,AContainerObject);
end;

procedure TCellsScrollBox.DoOnClearContainer(
  Sender: TCalendarCellContainer; AContainerObject: TWinControl);
begin
   if Assigned(FOnClearContainer) then FOnClearContainer(Sender,AContainerObject);
end;

procedure TCellsScrollBox.DoOnActivateCell;
var
  Cell : TCalendarCellContainer;
  CObject : TWinControl;
begin
  if not(IsContainer and HandleAllocated) then  Exit;
  Cell := nil;
  CObject := nil;
  if FSelectedIndex > -1 then
  begin
    Cell := Self.FCells[FSelectedIndex];
    if Assigned(Cell)then
      CObject := Cell.FContainObject;
  end;
  if Assigned(FOnActivateCell) then FOnActivateCell(Cell,CObject);
end;

procedure TCellsScrollBox.DoOnDeactivateCell(
  Sender: TCalendarCellContainer);
var
  CObject : TWinControl;
begin
  if not Sender.IsContainer then Exit;
  CObject := Sender.ContainObject;
  if Assigned(FOnDeactivateCell) then FOnDeactivateCell(Sender,CObject);
end;

procedure TCellsScrollBox.SetCurrentDate(const Value: TDateTime);
begin
  if FCurrentDate = Trunc(Value) then Exit;
  FCurrentDate := Trunc(Value);
  if Assigned(FOnDateChange) then  FOnDateChange(Self,FCurrentDate);
  DoOnActivateCell;
 // if not IsLoading then ReInitControl;
end;

procedure TCellsScrollBox.SetFirstCellDate(const Value: TDateTime);
begin
  if (FFirstCellDate = Value)then Exit;
  FFirstCellDate := Value;
 //SetCellsValues;
end;

procedure TCellsScrollBox.SetIsLoading(const Value: Boolean);
begin
  if FIsLoading = Value then Exit;
  FIsLoading := Value;
  if not FIsLoading then ReInitControl;
end;

procedure TCellsScrollBox.SetIsContainer(const Value: Boolean);
begin
  if FIsContainer = Value then Exit;
  FIsContainer := Value;
end;

procedure TCellsScrollBox.SetViewType(const Value: TCalendarViewType);
begin
  FViewType := Value;
  case FViewType of
   cvMonths:begin
              FHCellsCount := 7;
              FVCellsCount := 6;
              ScrollUnit := 12;
            end;
   cvWeeks :begin
              FHCellsCount := 7;
              FVCellsCount := RowCount;
              ScrollUnit := 52;
            end;
   cvDays  :begin
              FHCellsCount := ColCount;
              FVCellsCount := RowCount;
              ScrollUnit := (366 div ColCount)+1;
            end;
   cvCells :begin
              FHCellsCount := ColCount;
              FVCellsCount := RowCount;
              ScrollUnit := trunc(RowCount/2)
            end;
  end;
  WeekDays.Visible := (FViewType<>cvDays) and (FViewType<>cvCells);
  FirstCellDate := DATE_NONE;
  if not IsLoading then ReInitControl;
end;

procedure TCellsScrollBox.SetVHCellsCount(const VValue, HValue: integer);
begin
  FVCellsCount := VValue;
  FHCellsCount := HValue;
  if not IsLoading then ReInitControl;
end;

procedure TCellsScrollBox.SetRowColCount(const aRow, aCol: integer);
var
  RV, CV : integer;
begin
  if (FRowCount = aRow) and (FColCount = aCol) then Exit;

  if aRow < 1  then
    RV := 1
  else
  if aRow > 14 then
    RV := 14
  else
    RV := aRow;
  FRowCount := RV;

  if aCol < 1  then
    CV := 1
  else
  if aCol > 7 then
    CV := 7
  else
    CV := aCol;
  FColCount := CV;

  SetVHCellsCount(CV, RV);
end;

procedure TCellsScrollBox.SetVCellsCount(const Value: integer);
begin
  FVCellsCount := Value;
  if not IsLoading then ReInitControl;
end;

procedure TCellsScrollBox.SetHCellsCount(const Value: integer);
begin
  FHCellsCount := Value;
  if not IsLoading then ReInitControl;
end;

procedure TCellsScrollBox.SetRowCount(const Value: integer);
var
  V : integer;
begin
  if FRowCount = Value then Exit;
  if (Value < 1) and (ViewType<>cvCells) then
    V := 1
  else
  if (Value > 14) and (ViewType<>cvCells) then
    V := 14
  else
    V := Value;
  FRowCount := V;
  VCellsCount := V;
end;

procedure TCellsScrollBox.SetColCount(const Value: integer);
var
  V : integer;
begin
  if FColCount = Value then Exit;
  if Value < 1  then
    V := 1
  else
  if Value > 7 then
    V := 7
  else
    V := Value;
  FColCount := V;
  HCellsCount := V;
end;

procedure TCellsScrollBox.SetScrollUnit(const Value: integer);
begin
  FScrollUnit := Value;
  if HandleAllocated then ReInitScrollBar;
end;

procedure TCellsScrollBox.BeginUpdate;
begin
  if (FLockCount = 0) then FLockInitCount:= 0;
  Inc(FLockCount);
end;

procedure TCellsScrollBox.EndUpdate;
begin
  Dec(FLockCount);
  if (FLockCount = 0) and (0 < FLockInitCount) then ReInitControl;
end;

procedure TCellsScrollBox.ReInitControl;
begin
  if 0 < FLockCount then
    begin
      Inc(FLockInitCount);
      Exit;
    end;

  if isContainer then
   ClearCellContainers;
  CreateCells;
  CalcFirstCellDate;
  SetCellsValues;

  if Assigned(FOnRangeChange) then
    FOnRangeChange(self,  FFirstCellDate, IncDay(FFirstCellDate, FColCount*FRowCount) );

  if HandleAllocated then
  begin
//    AdjustBounds;
    if isContainer then
    begin
      CreateCellsContainers;
    end;
    ReInitScrollBar;
    AdjustBounds;
  end;
end;

procedure TCellsScrollBox.CreateCellsContainers;
var
  i : integer;
begin
  if (IsContainer)and(not (csDesigning in ComponentState))then
  begin
    for i:= Low(FCells) to High(FCells) do
    begin
        if not Assigned(FCells[i].FContainObject) then
          DoOnCreatingContainer(FCells[i],FCells[i].FContainObject);
        if not Assigned(FCells[i].FContainObject) then Continue;
        FCells[i].FContainObject.Visible := (FCells[i].CellKind = ckCurrentMonth) or (FCells[i].CellKind = ckItem);
        if FCells[i].HandleAllocated then FCells[i].AdjustBounds;
        if FCells[i].FContainObject.Visible then
        begin
          DoOnFillingContainer(FCells[i],FCells[i].FContainObject);
        end;
    end;
    DoOnActivateCell;
  end;
end;

procedure TCellsScrollBox.ClearCellContainers;
var
  i: integer;
begin
  if (IsContainer)and(not (csDesigning in ComponentState)) then
    for i:= Low(FCells) to High(FCells) do
      DoOnClearContainer(FCells[i],FCells[i].FContainObject);
end;

procedure TCellsScrollBox.RefreshContent;
begin
  GetLocalParams;
  ReInitControl;
end;


procedure TCellsScrollBox.ClearContent;
begin
  SetCellsCount(0);
  FSelectedIndex := -1;
end;

procedure TCellsScrollBox.CMColorChanged(var Message: TMessage);
var
  i : integer;
begin
  inherited;
  for i:= Low(FCells) to High(FCells) do
   if ViewType = cvWeeks then
    begin
      if Odd(MonthOf(FCells[i].CurrentDate)) then
        FCells[i].Color := ChangeBrightnessByDelta(Color,-10)
      else
        FCells[i].Color := ChangeBrightnessByDelta(Color,-30);
    end
    else
    if isContainer then
    begin
      if (FCells[i].CellKind = ckCurrentMonth) or (FCells[i].CellKind = ckItem)
         then
        FCells[i].Color := ChangeBrightnessByDelta(Color,-20)
      else
        FCells[i].Color := Color;
    end;
end;

procedure TCellsScrollBox.IncLine(const Coeff: integer);
begin
  case FViewType of
  cvMonths:begin
             CurrentDate := IncMonth(FCurrentDate,Coeff);
           end;
  cvWeeks :begin
             CurrentDate := IncWeek(CurrentDate,Coeff);
             if (FCurrentDate < FirstCellDate) or (FCurrentDate > IncDay(FirstCellDate,(FVCellsCount*FHCellsCount)-1)) then
               FirstCellDate := IncWeek(FirstCellDate,Coeff);;
           end;
  cvDays,cvCells  :begin
             CurrentDate := IncDay(CurrentDate,Coeff*ColCount);
             if (FCurrentDate < FirstCellDate) or (FCurrentDate > IncDay(FirstCellDate,(FVCellsCount*FHCellsCount)-1)) then
               FirstCellDate := IncDay(FirstCellDate,Coeff*ColCount);
           end;
  end;
  ReInitControl;
  FScrollPos := FScrollPos + Coeff;
  SetScrollPos(Handle, SB_VERT,FScrollPos, True);
end;

procedure TCellsScrollBox.IncPage(const Coeff: integer);
begin
  case FViewType of
  cvMonths:begin
             CurrentDate := IncMonth(FCurrentDate,Coeff);
             FScrollPos := FScrollPos + Coeff;
           end;
  cvWeeks :begin
             CurrentDate := IncWeek(CurrentDate,Max((VCellsCount-1),1)*Coeff);
             if (FCurrentDate < FirstCellDate) or (FCurrentDate > IncDay(FirstCellDate,(FVCellsCount*FHCellsCount)-1)) then
              FirstCellDate := IncWeek(FirstCellDate,Max((VCellsCount-1),1)*Coeff);
             FScrollPos := FScrollPos + Max((VCellsCount-1),1)*Coeff;
           end;
  cvDays,cvCells  :begin
             CurrentDate := IncDay(CurrentDate,Coeff*ColCount*Max(RowCount-1,1));
             if (FCurrentDate < FirstCellDate) or (FCurrentDate > IncDay(FirstCellDate,(FVCellsCount*FHCellsCount)-1)) then
               FirstCellDate := IncDay(FirstCellDate,Coeff*ColCount*Max(RowCount-1,1));
             FScrollPos := FScrollPos + Max((VCellsCount-1),1)*Coeff;
           end;
  end;
  ReInitControl;
  SetScrollPos(Handle, SB_VERT,FScrollPos, True);
end;

procedure TCellsScrollBox.IncThumb(const Coeff: integer);
var
  Delta : integer;
begin
  Delta := Coeff - FScrollPos;
  case FViewType of
  cvMonths:begin
            CurrentDate := IncMonth(FCurrentDate,Delta);
           end;
  cvWeeks :begin
             CurrentDate := IncWeek(CurrentDate,Delta);
             if (FCurrentDate < FirstCellDate) or (FCurrentDate > IncDay(FirstCellDate,(FVCellsCount*FHCellsCount)-1)) then
               FirstCellDate := IncWeek(FirstCellDate,Delta);
           end;
  cvDays,cvCells :begin
             CurrentDate := IncDay(CurrentDate,Delta*ColCount*Max(RowCount-1,1));
             if (FCurrentDate < FirstCellDate) or (FCurrentDate > IncDay(FirstCellDate,(FVCellsCount*FHCellsCount)-1)) then
               FirstCellDate := IncDay(FirstCellDate,Delta*ColCount*Max(RowCount-1,1));
           end;
  end;
  ReInitControl;
  FScrollPos := Coeff;
  SetScrollPos(Handle, SB_VERT,FScrollPos, True);
end;

procedure TCellsScrollBox.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
{  case Message.ScrollCode of
  SB_BOTTOM: ;
  SB_ENDSCROLL:;
  SB_LINEDOWN:IncLine(1);
  SB_LINEUP:IncLine(-1);
  SB_PAGEDOWN:IncPage(1);
  SB_PAGEUP:IncPage(-1);
  SB_THUMBPOSITION:;
  SB_THUMBTRACK:IncThumb(Message.Pos);
  SB_TOP:;
  end;}
end;

procedure TCellsScrollBox.AdjustClientRect(var Rect: TRect);
var
  WCoeff : integer;
  i: integer;
begin
  inherited;
  if (not (csDesigning in ComponentState))then
  begin
   if WeekDays.Visible then
     WeekDays.SetBounds(0,0,ClientWidth,WeekDays.Height);
    WCoeff := (ClientWidth) div FHCellsCount;
    FWCoeff := WCoeff;
    for i:= Low(FCells) to High(FCells) do
      if Assigned(FCells[i]) then FCells[i].Width:=FWCoeff-4;
  end;
end;

procedure TCellsScrollBox.AutoScrollInView(AControl: TControl);
begin
  inherited;
  ScrollInView(AControl.Parent);
end;

function TCustomCalendar.SelectedWinControl : TWinControl;
begin
  if (0<=FSelectedIndex) and (FSelectedIndex<Length(FCells)) then
    result:= FCells[FSelectedIndex].FContainObject
  else
    result:=nil;
end;

end.




