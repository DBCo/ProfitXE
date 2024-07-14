unit DeSchema2D;

interface

uses Types, SysUtils, Windows, {Messages, }Classes, Controls, Graphics, {StdCtrls, }{ExtCtrls, }
     G2D;

const
  cPointSize    = 3;
  cRotationMark = 18;
  
const
  DE_EPSILON = 1e-5;

  MAX_SCHEMALAYERNAME_SIZE = 100;
  MAX_SHORTCUTCAPTION_SIZE = 100;

type
  TdeViewCoorninates  = (vcNone,vcPoint,vcLine,vcBoth);

type
  TdeFlipDirection  = (fdVertical, fdHorizontal);

type
  TdeBoundsPointClass = (bpcUnknown,
                         bpcTopLeft,bpcTop,bpcTopRight,bpcRight,
                         bpcBottomRight,bpcBottom,bpcBottomLeft,bpcLeft,
                         bpcRotation,bpcSpecial);
type
  TdeObjectChangedProp = (ocpBounds,ocpParent,ocpLayer,ocpParams,ocpSubItems);
  TdeObjectChangedProps = set of TdeObjectChangedProp;

type
  TdeObjectClassOption  = (ocoResizable,ocoRotatable);
  TdeObjectClassOptions = set of TdeObjectClassOption;
{type
  TdeParamDataType = (pdtUnknown,pdtString,pdtInteger,pdtFloat,pdtBool);}

type
  TdeAFScaleMode = (smStretch,     //масштабироваться под размер Rect
                    smProportional,//wmStretch + сохранив пропорции
                    smAutoSize);   //wmProportional + изменить размер Rect


type
  TdeSchema        = class;
  TdeSchemaGroup   = class;
  TdeSchemaLayer   = class;
  TdeSchemaObject  = class;

  TdeSchemaObjectMemento = class
  protected
    ClassID    : integer; //class ID of the originator object!
    Layer      : string;  //Layer of the originator object!
    Convertion : TMatrix2D;
    constructor Create(anOrigin:TdeSchemaObject);
  public
    function    CloneObject(aSchema:TdeSchema;
                            aParent:TdeSchemaGroup = nil;
                            aLayer:string = ''):TdeSchemaObject;virtual;
    procedure   Restore(anObject:TdeSchemaObject);virtual;
    destructor  Destroy;override;
  end;

  TdeObjectDataHeader = packed record
    nSize          : dword;
    pInheritedData : dword;
    pInheritedSize : dword;
  end;

  TdeSchemaObjectData = packed record
    Header         : TdeObjectDataHeader;
    Matrix         : TMatrix2D;
  end;
  PdeSchemaObjectData  = ^TdeSchemaObjectData;

  TdeSchemaObject = class
  private
    FOwner           : TdeSchema;
    FParent          : TdeSchemaGroup;
    FLayer           : TdeSchemaLayer;
    FBounds          : TRect2D;
    FDeleting        : boolean;
    FWaitDelete      : boolean;
    FChangeParent    : boolean;
    FChangeLayer     : boolean;
    FLocked          : integer;
    FChanging        : integer;
    FChanged         : boolean;
    FBndsChanged     : boolean;
    FModified        : boolean;
    function     getBounds:TRect2D;
    function     getLevel:integer;
    function     getSchemaBounds:TRect2D;
    procedure    setParent(aValue:TdeSchemaGroup);
    function     getTopParent:TdeSchemaObject;
    function     getLayerName:string;
    function     getIndex:integer;
    procedure    setIndex(newIndex:integer);
    procedure    setConvertion(aValue:TMatrix2D);
    function     getTopLeft:TPoint2D;
    procedure    setTopLeft(aValue:TPoint2D);
    function     getOffset:TPoint2D;
    procedure    setOffset(aValue:TPoint2D);
    function     getWidth:double;
    procedure    setWidth(aValue:double);
    function     getHeight:double;
    procedure    setHeight(aValue:double);
    function     getRotation:double;
    procedure    setRotation(aValue:double);
    function     getReadOnly:boolean;
    function     getVisible:boolean;
  protected
    FConvertion     : TMatrix2D;
    FOToSConv       : TMatrix2D;
    FRConvertion    : TMatrix2D;
    FROToSConv      : TMatrix2D;
    FChangedProps   : TdeObjectChangedProps;
    function     getLayer:TdeSchemaLayer;virtual;
    procedure    setLayer(aValue:TdeSchemaLayer);virtual;
    function     RecalcBounds:TRect2D;virtual;abstract;
    procedure    Changing;// вызывается объектом перед само-изменением
    procedure    doChanging;virtual;// вызывается объектом перед само-изменением
    procedure    Changed(ChangedProps:TdeObjectChangedProps);virtual;
    procedure    doChanged;virtual; // -/- объектом после само-изменения
    procedure    UpdateBounds;
    procedure    UpdateConvertion;virtual;
    procedure    Update;virtual;  // -/- извне чтобы объект мог учесть изменения
  public
    class function ClassID:integer;virtual;abstract;
    class function ClassOptions:TdeObjectClassOptions;virtual;
    constructor  Create(anOwner:TdeSchema);virtual;
    destructor   Destroy;override;
    function     getData(pData:pointer;var iSize:integer):boolean;virtual;
    procedure    setData(pData:pointer;iSize:integer);virtual;
    function     CreateMemento:TdeSchemaObjectMemento;virtual;
    function     IGroup:TdeSchemaGroup;virtual;
    procedure    Delete;
    function     OToS(aPoint:TPoint2D):TPoint2D;//ObjectToSchema
    function     SToO(aPoint:TPoint2D):TPoint2D;//SchemaToObject
    function     OToP(aPoint:TPoint2D):TPoint2D;//ObjectToParent
    function     PToO(aPoint:TPoint2D):TPoint2D;//ParentToObject
    procedure    Lock;
    procedure    unLock;
    procedure    ApplyConvertion(aConvertion:TMatrix2D);
    procedure    DropConvertion;virtual;
    procedure    MoveTo(aPoint:TPoint2D);
    procedure    MoveBy(aVector:TPoint2D);
    function     AngleToPoint(aPoint:TPoint2D):double;
    procedure    Rotate(Angle:double);
    procedure    Flip(Direction: TdeFlipDirection);
    procedure    Resize(BoundPoint:TdeBoundsPointClass;
                            Vector:TPoint2D;
                      Proportional:boolean = false);
    procedure    getBoundsPoints(
                                            Canvas : TCanvas;
                                              StoV : TMatrix2D;
                                 var P_TopLeft     : TPoint2D;
                                 var P_TopRight    : TPoint2D;
                                 var P_BottomRight : TPoint2D;
                                 var P_BottomLeft  : TPoint2D);virtual;
    function     getViewRect(Canvas:TCanvas;StoV:TMatrix2D):TRect;virtual;
    procedure    PaintTo(Canvas:TCanvas;StoC:TMatrix2D;WidthLine:Integer=1);virtual;
    procedure    PaintFrame(Canvas:TCanvas;StoC:TMatrix2D);virtual;
    function     Contain(Canvas:TCanvas;StoC:TMatrix2D;X,Y:integer):boolean;virtual;
    property     Owner:TdeSchema read FOwner;
    property     Parent:TdeSchemaGroup read FParent write setParent;
    property     TopParent:TdeSchemaObject read getTopParent;
    property     Level:integer read getLevel;
    property     Layer:TdeSchemaLayer read getLayer write setLayer;
    property     LayerName:string read getLayerName;
    property     Bounds:TRect2D read getBounds;               //собственные границы
    property     SchemaBounds:TRect2D read getSchemaBounds; //границы на схеме
    property     Deleting:boolean read FDeleting;
    property     Index:integer read getIndex write setIndex;
    property     Convertion:TMatrix2D read FConvertion write setConvertion;
    property     TopLeft:TPoint2D read getTopLeft write setTopLeft;
    property     Offset:TPoint2D read getOffset write setOffset;
    property     Width:double read getWidth write setWidth;
    property     Height:double read getHeight write setHeight;
    property     Rotation:double read getRotation write setRotation;
    property     ReadOnly:boolean read getReadOnly;
    property     Visible:boolean read getVisible;
    property     Modified:boolean read FModified write FModified;
  end;

  TdeSchemaObjectClass = class of TdeSchemaObject;

  TdeSchemaObjectList = class
  private
    FCount    : integer;
    FCapacity : integer;
    FItems    : array of TdeSchemaObject;
    FIndex    : array of packed record
      Key     : integer;
      Index   : integer;
    end;
    FOnChange : TNotifyEvent;
    function    getItem(index:integer):TdeSchemaObject;
    function    find(anItem:TdeSchemaObject;var index:integer):boolean;
    procedure   Grow;
    procedure   NotifyChanged;
  public
    constructor Create;
    destructor  Destroy;override;
    procedure   Clear;
    function    Add(anItem:TdeSchemaObject):integer;
    procedure   Insert(index:integer;anItem:TdeSchemaObject);
    procedure   Remove(anItem:TdeSchemaObject);
    procedure   Delete(index:integer);
    procedure   Move(curIndex,newIndex:integer);
    function    indexOf(anItem:TdeSchemaObject):integer;
    property    Count:integer read FCount;
    property    Items[index:integer]:TdeSchemaObject read getItem; default;
    property    onChange:TNotifyEvent read FOnChange write FOnChange;
  end;

  TdeSchemaGroup  = class(TdeSchemaObject)
  private
    FItems     : TdeSchemaObjectList;
    function     getCount:integer;
    function     getItem(index:integer):TdeSchemaObject;
  protected
    function     RecalcBounds:TRect2D;override;
    procedure    doChanging;override; //вызывается объектом перед само-изменением
    procedure    doChanged;override;  //-/- объектом после само-изменением
    procedure    UpdateConvertion;override;
    procedure    Update;override;   //-/- извне чтобы объект мог учесть изменения
    procedure    UpdateItems;virtual;
    procedure    ItemChanging(anItem:TdeSchemaObject);virtual;
    procedure    ItemChanged(anItem:TdeSchemaObject;
                             ChangedProps:TdeObjectChangedProps);virtual;
  public
    class function ClassID:integer;override;
    constructor  Create(anOwner:TdeSchema);override;
    destructor   Destroy;override;
    function     CreateMemento:TdeSchemaObjectMemento;override;
    function     IGroup:TdeSchemaGroup;override;
    procedure    PaintTo(Canvas:TCanvas;StoC:TMatrix2D;WidthLine:Integer=1);override;
    procedure    PaintFrame(Canvas:TCanvas;StoC:TMatrix2D);override;
    function     Contain(Canvas:TCanvas;StoC:TMatrix2D;X,Y:integer):boolean;override;
    procedure    Clear;virtual;
    procedure    DropConvertion;override;
    function     indexOf(anItem:TdeSchemaObject):integer;
    procedure    Add(anItem:TdeSchemaObject);
    procedure    Insert(index:integer;anItem:TdeSchemaObject);
    procedure    Remove(anItem:TdeSchemaObject);
    procedure    ChangeChildIndex(curIndex,newIndex:integer);
    property     Count:integer read getCount;
    property     Item[index:integer]:TdeSchemaObject read getItem;
  end;

  TdeSchemaLayer  = class
  private
    FDeleting  : boolean;
  protected
    FOwner     : TdeSchema;
    FItems     : TdeSchemaObjectList;
    FVisible   : boolean;
    FReadOnly  : boolean;
    FName      : string;
    FModified  : boolean;
    function     getCount:integer;
    function     getItem(index:integer):TdeSchemaObject;
    function     getVisible:boolean;
    procedure    setVisible(aValue:boolean);
    function     getReadOnly:boolean;
    procedure    setReadOnly(aValue:boolean);
  public
    constructor  Create(anOwner:TdeSchema;aName:string);
    destructor   Destroy;override;
    procedure    Delete;
    procedure    Clear;
    function     IndexOf(anItem:TdeSchemaObject):integer;
    procedure    Add(anItem:TdeSchemaObject);
    procedure    Remove(anItem:TdeSchemaObject);
    property     Owner:TdeSchema read FOwner;
    property     Name:string read FName write FName;
    property     Count:integer read getCount;
    property     Item[index:integer]:TdeSchemaObject read getItem;
    property     Visible:boolean read getVisible write setVisible;
    property     ReadOnly:boolean read getReadOnly write setReadOnly;
    property     Modified:boolean read FModified write FModified;
  end;

  TdeShortcutOption  = (scoShowCaption,scoShowImage);
  TdeShortcutOptions = set of TdeShortcutOption;

  TdeShortcutObjectData = packed record
    pOffset        : TPoint2D;
    cCaption       : array[0..MAX_SHORTCUTCAPTION_SIZE] of char;
    reserved       : dword;
    iImageIndex    : integer;
    sOptions       : TdeShortcutOptions;
    iShortcutID    : integer;
  end;
  PdeShortcutObjectData  = ^TdeShortcutObjectData;

  TdeSchemaShortcut = class
  private
    FOwner       : TdeSchema;
    FDeleting    : boolean;
    FOffset      : TPoint2D;
    FBitmap      : TBitmap;
    FCaption     : string;
    FImageIndex  : integer;
    FOptions     : TdeShortcutOptions;
    FModified    : boolean;
    FLObject     : TdeSchemaObject;
    FShortcutID  : integer;
    procedure    UpdateBitmap;
    procedure    setOffset(aValue:TPoint2D);
    procedure    setCaption(aValue:string);
    procedure    setImageIndex(aValue:integer);
    procedure    setOptions(aValue:TdeShortcutOptions);
    procedure    setLObject(aValue:TdeSchemaObject);
    function     getViewRect(Canvas:TCanvas;StoV:TMatrix2D):TRect;
  public
    constructor  Create(anOwner:TdeSchema);
    destructor   Destroy;override;
    procedure    Delete;
    function     getData(pData:pointer;var iSize:integer):boolean;
    procedure    setData(pData:pointer;    iSize:integer);
    procedure    PaintTo(   Canvas:TCanvas;StoC:TMatrix2D);
    procedure    PaintFrame(Canvas:TCanvas;StoC:TMatrix2D);
    function     Contain(   Canvas:TCanvas;StoC:TMatrix2D;X,Y:integer):boolean;
    property     Offset:TPoint2D read FOffset write setOffset;
    property     Caption:string read FCaption write setCaption;
    property     ImageIndex:integer read FImageIndex write setImageIndex;
    property     Options:TdeShortcutOptions read FOptions write setOptions;
    property     Modified:boolean read FModified write FModified;
    property     LinkedObject:TdeSchemaObject read FLObject write setLObject;
    property     ShortcutID:integer read FShortcutID write FShortcutID;
  end;

  TdeSchema       = class
  private
    FBounds     : TRect2D;
    FLockCount  : integer;
    FChanged    : boolean;
    FDeleting   : boolean;
    FClearing   : boolean;
    function     recalcBounds:TRect2D;
  protected
    FRootItems : TdeSchemaObjectList;
    FLayers    : TStringList;
    FShortcuts : TList;
    FSCImages  : TImageList;
    FSCFont    : TFont;
    function     getBounds:TRect2D;
    function     getItemsCount:integer;
    function     getItem(index:integer):TdeSchemaObject;
    function     getRootCount:integer;
    function     getRootItem(index:integer):TdeSchemaObject;
    procedure    RootAdd(anItem:TdeSchemaObject);
    procedure    RootInsert(Index:integer;anItem:TdeSchemaObject);
    procedure    RootRemove(anItem:TdeSchemaObject);
    function     getLayersCount:integer;
    function     getLayer(index:integer):TdeSchemaLayer;
    function     getLayerByName(aName:string):TdeSchemaLayer;
    function     getShortcutsCount:integer;
    function     getShortcut(index:integer):TdeSchemaShortcut;
    procedure    setShortcutImages(anImageList:TImageList);
    procedure    setShortcutFont(aFont:TFont);
    procedure    DeleteObjectShortcuts(anObject:TdeSchemaObject);
    procedure    MoveObjectShortcutsBy(anObject:TdeSchemaObject;
                                        aVector:TPoint2D);
    procedure    ItemChanging(anItem:TdeSchemaObject);
    procedure    ItemChanged(anItem:TdeSchemaObject;
                             ChangedProps:TdeObjectChangedProps);
    procedure    Changed;
    procedure    NotifyNewLayer(aLayer:TdeSchemaLayer);virtual;
    procedure    NotifyDeleteLayer(aLayer:TdeSchemaLayer);virtual;
    procedure    NotifyLayerChanged(aLayer:TdeSchemaLayer);virtual;
    procedure    NotifyNewShortcut(aShortcut:TdeSchemaShortcut);virtual;
    procedure    NotifyDeleteShortcut(aShortcut:TdeSchemaShortcut);virtual;
    procedure    NotifyShortcutChanged(aShortcut:TdeSchemaShortcut);virtual;
    procedure    NotifyNewItem(anItem:TdeSchemaObject);virtual;
    procedure    NotifyDeleteItem(anItem:TdeSchemaObject);virtual;
    procedure    NotifyItemChanging(anItem:TdeSchemaObject);virtual;
    procedure    NotifyChanged;virtual;
  public
    procedure    NotifyItemChanged(anItem:TdeSchemaObject;
                             ChangedProps:TdeObjectChangedProps);virtual;

    constructor  Create;
    destructor   Destroy;override;
    procedure    Lock;
    procedure    unLock;
    function     IsLocked:boolean;
    procedure    Clear;
    function     First:TdeSchemaObject;
    function     Next(anItem:TdeSchemaObject):TdeSchemaObject;
    function     Prev(anItem:TdeSchemaObject):TdeSchemaObject;
    function     Last:TdeSchemaObject;
    function     NextLeft(anItem:TdeSchemaObject):TdeSchemaObject;
    function     NextRight(anItem:TdeSchemaObject):TdeSchemaObject;
    function     NextTop(anItem:TdeSchemaObject):TdeSchemaObject;
    function     NextBottom(anItem:TdeSchemaObject):TdeSchemaObject;
    function     IndexOf(anItem:TdeSchemaObject):integer;
    procedure    ChangeRootItemIndex(Index,aValue:integer);
    function     RootItemIndex(anItem:TdeSchemaObject):integer;
    function     New(aClassID:integer;aParent:TdeSchemaGroup;aLayer:string=''):TdeSchemaObject;overload;
    function     New(aClass:TdeSchemaObjectClass;aParent:TdeSchemaGroup;aLayer:string=''):TdeSchemaObject;overload;
    function     NewGroup(aParent:TdeSchemaGroup;aLayer:string=''):TdeSchemaGroup;
    function     NewShortcut:TdeSchemaShortcut;
    procedure    Delete(anItem:TdeSchemaObject);
    function     LayerIndex(aLayerName:string):integer;
    function     ShortcutIndex(aShortcut:TdeSchemaShortcut):integer;
    procedure    getObjectShortcuts(anObject:TdeSchemaObject;ShortcutsList:TList);
    procedure    PaintTo(Canvas:TCanvas;StoC:TMatrix2D;WidthLine:Integer=1);
    procedure    AddFrom(aSchema:TdeSchema;var Rect:TRect2D;ScaleMode:TdeAFScaleMode);
    procedure    PrepareInfo;
    property     Bounds:TRect2D read getBounds;
    property     ItemsCount:integer read getItemsCount;
    property     Item[index:integer]:TdeSchemaObject read getItem;
    property     RootCount:integer read getRootCount;
    property     RootItem[index:integer]:TdeSchemaObject read getRootItem;
    property     LayersCount:integer read getLayersCount;
    property     Layer[index:integer]:TdeSchemaLayer read getLayer;
    property     LayerByName[Name:string]:TdeSchemaLayer read getLayerByName;
    property     ShortCutsCount:integer read getShortcutsCount;
    property     ShortCut[index:integer]:TdeSchemaShortcut read getShortCut;
    property     ShortcutImages:TImageList read FSCImages write setShortcutImages;
    property     ShortcutFont:TFont read FSCFont write setShortcutFont;
  end;

type
  TdeCustomCurve = class(TdeSchemaObject)
  protected
    FDrawPath  : array of TPoint2D;
    FBrdColor  : TColor;
    FBrdStyle  : TPenStyle;
    FBrdWidth  : integer;
    FColor     : TColor;
    FTransparent : boolean;
    function     PointsCount:integer;
    function     getPoint(index:integer):TPoint2D;
    function     getPT(index:integer):integer;
    function     RecalcBounds:TRect2D;override;
    procedure    setBrdColor(aValue:TColor);virtual;
    procedure    setBrdStyle(aValue:TPenStyle);virtual;
    procedure    setBrdWidth(aValue:integer);virtual;
    procedure    setColor(aValue:TColor);virtual;
    function     getTransparent:boolean;virtual;
    procedure    setTransparent(aValue:boolean);virtual;
    procedure    IntPaint(Canvas:TCanvas;const iPoints:array of TPoint);
    property     Point[index:integer]:TPoint2D read getPoint;
    property     PntType[index:integer]:integer read getPT;
  public
    constructor  Create(anOwner:TdeSchema);override;
    destructor   Destroy;override;
    function     getData(pData:pointer;var iSize:integer):boolean;override;
    procedure    setData(pData:pointer;iSize:integer);override;
    function     CreateMemento:TdeSchemaObjectMemento;override;
    procedure    PaintTo( Canvas:TCanvas;StoC:TMatrix2D;WidthLine:Integer=1);override;
    procedure    PaintFrame(Canvas:TCanvas;StoC:TMatrix2D);override;
    function     Contain(  Canvas:TCanvas;StoC:TMatrix2D;X,Y:integer):boolean;override;
    procedure    DropConvertion;override;
    function     getDrawPath(var Points:array of TPoint2D;
                                var Cnt:integer;
                                  pConv:pMatrix2D = nil):boolean;virtual;
    procedure    setDrawPath(const Points:array of TPoint2D;
                                      Cnt:integer;
                                    pConv:pMatrix2D = nil);virtual;
    property     BorderColor:TColor read FBrdColor write setBrdColor;
    property     BorderStyle:TPenStyle read FBrdStyle write setBrdStyle;
    property     BorderWidth:integer read FBrdWidth write setBrdWidth;
    property     Color:tColor read FColor write setColor;
    property     Transparent:boolean read getTransparent write setTransparent;
  end;


const
  MAX_BEZIER_POINTS     = 20;

type
  TdeVertexType         = (vtxAngular,vtxDirect,vtxSmooth);
  TdeSegmentType        = (sgmLinear,sgmBezier);
  TdeVertexesPointClass = (vpcUnknown,vpcVertex,vpcEdge,vpcBody,vpcLeftCP,vpcRightCP);

type
  EVertexException = class(Exception);


type
  TdeDPVertex = packed record
    Point  : integer;
    VType  : TdeVertexType;
    LCP    : integer;
    RCP    : integer;
  end;

type
  TdeSimpleCurve = class(TdeCustomCurve)
  protected
    FClosed    : boolean;
    FCycled    : boolean;
    FVertexes  : array of TdeDPVertex;
    FActiveVtx : integer;
    FUpdating  : integer;
    procedure    setPoint(index:integer;aValue:TPoint2D);
    procedure    setPT(index:integer;aValue:integer);
    procedure    DelPoints(index,count:integer);
    procedure    AddPoints(index,count:integer);
    function     getClosed:boolean;virtual;
    procedure    setClosed(aValue:boolean);virtual;
    function     nextVertex(vtxIndex:integer):integer;
    function     prevVertex(vtxIndex:integer):integer;
    procedure    DrawCP(Canvas:TCanvas;iCP,iP:TPoint);
    procedure    DrawVertex(Canvas:TCanvas;aVtx:TPoint;Selected: Boolean = False);
    procedure    UpdateVertexes;
    procedure    RecalcVertexType(vtxIndex:integer);
    procedure    RecalcVertexesTypes;
    function     getVertexesCount:integer;
    function     getVertex(vtxIndex:integer):TPoint2D;
    procedure    setVertex(vtxIndex:integer;aValue:TPoint2D);
    function     getVtxType(vtxIndex:integer):TdeVertexType;
    procedure    setVtxType(vtxIndex:integer;aValue:TdeVertexType);
    function     getSegmentsCount:integer;
    function     getSgmType(sgmIndex:integer):TdeSegmentType;
    procedure    setSgmType(sgmIndex:integer;aValue:TdeSegmentType);
    procedure    setActiveVertex(vertexID:integer);
  public
    class function ClassID:integer;override;
    constructor  Create(anOwner:TdeSchema);override;
    destructor   Destroy;override;
    function     CreateMemento:TdeSchemaObjectMemento;override;
    function     getDrawPath(var Points:array of TPoint2D;
                                var Cnt:integer;
                                  pConv:pMatrix2D = nil):boolean;override;
    procedure    setDrawPath(const Points:array of TPoint2D;
                                      Cnt:integer;
                                    pConv:pMatrix2D = nil);override;
    procedure    PaintChanges(Canvas:TCanvas;StoC:TMatrix2D;ShowInfo:TdeViewCoorninates = vcNone);virtual;
    procedure    BeginUpdate;
    procedure    EndUpdate;
    function     IsUpdating:boolean;
    function     Classify(      aPoint:TPoint2D;
                             PointSize:double;
                          var vtxIndex:integer):TdeVertexesPointClass;
    procedure    AddVertex(sgmIndex:integer;Value:TPoint2D);
    procedure    DelVertex(vtxIndex:integer);
    function     getLeftCP(vtxIndex:integer;var aValue:TPoint2D):boolean;
    function     getRightCP(vtxIndex:integer;var aValue:TPoint2D):boolean;
    procedure    setLeftCP(vtxIndex:integer;aValue:TPoint2D);
    procedure    setRightCP(vtxIndex:integer;aValue:TPoint2D);
    procedure    getSegmentPoints(sgmIndex:integer;var P0,P1,P2,P3:TPoint2D);
    function     getLinearPath(var Points:array of TPoint2D;
                                  var Cnt:integer;
                                    pConv:pMatrix2D = nil):boolean;
    property     VertexesCount:integer read getVertexesCount;
    property     Vertex[vtxIndex:integer]:TPoint2D read getVertex write setVertex;
    property     VertexType[vtxIndex:integer]:TdeVertexType read getVtxType
                                                           write setVtxType;
    property     SegmentsCount:integer read getSegmentsCount;
    property     SegmentType[sgmIndex:integer]:TdeSegmentType read getSgmType
                                                             write setSgmType;
    property     Closed:boolean read getClosed write setClosed;
    property     ActiveVertex:integer read FActiveVtx write setActiveVertex;
  end;

type
  TdeComplexCurve = class({TdeSimpleCurve{}TdeCustomCurve{})
  public
    class function ClassID:integer;override;
    constructor  Create(anOwner:TdeSchema);override;
    destructor   Destroy;override;
    procedure    Split(newObjects:tList; SetIndex:Boolean = True);
                                          //в параметре newObjects возвращается
                                          //список созданных объектов
    procedure    CombineWith(aCurve:TdeCustomCurve);
  end;


type
  TdeMemoryStream = class(TMemoryStream)
  public
    destructor Destroy; override;
    function   Write(const Buffer; Count: Longint): Longint; override;
  end;



type
  TdeSchemaStorage = class
  protected
    procedure    DoNotifyProgress(Progress:integer);virtual;
  public
    constructor  Create;
    destructor   Destroy;override;
    procedure    Load(aShema:TdeSchema);virtual;
    procedure    Save(aShema:TdeSchema);virtual;
  end;


function  P2DToP(const aPoint:TPoint2D):tPoint;
function  PToP2D(const aPoint:TPoint):tPoint2D;
function  P2DToPEx(const aPoint:TPoint2D;const StoC:TMatrix2D):tPoint;
function  PToP2DEx(const aPoint:TPoint;const CtoS:TMatrix2D):tPoint2D;
function  OffsetPoint(aPoint:TPoint;dX,dY:integer):TPoint;
function  ExpandRect(const R:TRect;const P:TPoint):TRect;
function  getRotationMark(P_Top,P_Right,P_Bottom,P_Left : TPoint;
                         var aPoint:TPoint):boolean;
procedure DrawRotatedText(CV : TCanvas; sText:string; X,Y,Angle:integer);


procedure deRegisterObjectClass(aClass:TdeSchemaObjectClass);


implementation

uses Math,
     DeLog, Funcs;

{$R MapCursors.res}

function OffsetPoint(aPoint:TPoint;dX,dY:integer):TPoint;
begin
  Result := Point(aPoint.X+dX,aPoint.Y+dY);
end;

function P2DToP(const aPoint:TPoint2D):tPoint;
begin
  Result := Point(round(aPoint.X),round(aPoint.Y));
end;

function PToP2D(const aPoint:TPoint):tPoint2D;
begin
  Result := Point2D(aPoint.X,aPoint.Y);
end;

function P2DToPEx(const aPoint:TPoint2D;const StoC:TMatrix2D):tPoint;
begin
  Result := P2DToP(MultVM2D(aPoint,StoC));
end;

function PToP2DEx(const aPoint:TPoint;const CtoS:TMatrix2D):tPoint2D;
begin
  Result := MultVM2D(Point2D(aPoint.X,aPoint.Y),CtoS);
end;

function  ExpandRect(const R:TRect;const P:TPoint):TRect;
begin
  Result := R;
  if Result.Left>P.X then
    Result.Left := P.X
  else if Result.Right<P.X then
    Result.Right := P.X;
  if Result.Top>P.Y then
    Result.Top := P.Y
  else if Result.Bottom<P.Y then
    Result.Bottom := P.Y;
end;


function  getRotationMark(P_Top,P_Right,P_Bottom,P_Left : TPoint;
                          var aPoint:TPoint):boolean;
var
  V  : TPoint;
  d  : double;
begin
  Result := false;
  // поворот
  V := Point(P_Top.X-P_Bottom.X,P_Top.Y-P_Bottom.Y);
  d := sqrt(sqr(V.X)+sqr(V.Y));
  if (d > 0) then begin
    aPoint := Point(trunc(P_Top.X+V.X/d*cRotationMark),
                    trunc(P_Top.Y+V.Y/d*cRotationMark));
    Result := true;
  end
  else begin
    V := Point(P_Right.X-P_Left.X,P_Right.Y-P_Left.Y);
    d := sqrt(sqr(V.X)+sqr(V.Y));
    if (d > 0) then begin
      aPoint := Point(trunc(P_Top.X+V.Y/d*cRotationMark),
                      trunc(P_Top.Y-V.X/d*cRotationMark));
      Result := true;
    end;
  end;
end;


procedure   DrawRotatedText(CV : TCanvas; sText:string; X,Y,Angle:integer);
var
  LogFont : TLogFont;
begin
  getObject(CV.Font.Handle,sizeOf(TLogFont),@LogFont);
  LogFont.lfEscapement := Angle*10;
  LogFont.lfOrientation := Angle*10;
  CV.Font.Handle := CreateFontIndirect(LogFont);
  CV.TextOut(X,Y,sText);
end;


var
  IntObjectClassList : TStringList;

procedure deRegisterObjectClass(aClass:TdeSchemaObjectClass);
var
  aName : string;
  i     : integer;
begin
  aName := aClass.ClassName;
  i :=IntObjectClassList.IndexOf(aName);
  if i>=0 then
    IntObjectClassList.Objects[i]:=tObject(aClass)
  else
    IntObjectClassList.AddObject(aName,tObject(aClass));
end;

function  deCreateObject(aClassName:string;anOwner:TdeSchema):TdeSchemaObject;overload;
var
  i : integer;
begin
  Result := nil;
  i :=IntObjectClassList.IndexOf(aClassName);
  if i>=0 then
    Result := TdeSchemaObjectClass(IntObjectClassList.Objects[i]).Create(anOwner);
end;

function  deCreateObject(aClassID:integer;anOwner:TdeSchema):TdeSchemaObject;overload;
var
  i : integer;
  aClass : TdeSchemaObjectClass;
begin
  i := 0;
  Result := nil;
  while (i<IntObjectClassList.Count)and(Result=nil) do begin
    aClass := TdeSchemaObjectClass(IntObjectClassList.Objects[i]);
    if (aClass.ClassID=aClassID) then
      Result := aClass.Create(anOwner);
    inc(i);
  end;
end;





{TdeSchemaObjectMemento}
constructor TdeSchemaObjectMemento.Create(anOrigin:TdeSchemaObject);
begin
  inherited Create;
  ClassID := anOrigin.ClassID;
  Layer   := anOrigin.LayerName;
  Convertion := anOrigin.Convertion;
end;

function    TdeSchemaObjectMemento.CloneObject(aSchema:TdeSchema;
                                    aParent:TdeSchemaGroup = nil;
                                    aLayer:string = ''):TdeSchemaObject;
begin
  if (aLayer<>'') then
    Layer := aLayer;
  if (ClassID<0) then
    Result := aSchema.NewGroup(aParent)
  else
    Result := aSchema.New(ClassID,aParent,aLayer);
  if Result=nil then exit;
  Restore(Result);
end;

procedure   TdeSchemaObjectMemento.Restore(anObject:TdeSchemaObject);
begin
  if (anObject.ClassID<>ClassID) then
    raise Exception.Create('Memento:Couldn''t restore object.');
  anObject.Lock;
  anObject.Convertion := Convertion;
  anObject.unLock;
end;

destructor  TdeSchemaObjectMemento.Destroy;
begin
  inherited Destroy;
end;


type
  TdeGroupMemento = class(TdeschemaObjectMemento)
  protected
    Items : TList;
    constructor Create(anOrigin:TdeSchemaGroup);
  public
    procedure   Restore(anObject:TdeSchemaObject);override;
    destructor  Destroy;override;
  end;

constructor TdeGroupMemento.Create(anOrigin:TdeSchemaGroup);
var
  i : integer;
begin
  inherited Create(anOrigin);
  Items := TList.Create;
  for i:= 0 to anOrigin.Count-1 do
    Items.Add(anOrigin.Item[i].CreateMemento);
end;

procedure   TdeGroupMemento.Restore(anObject:TdeSchemaObject);
var
  Grp  : TdeSchemaGroup;
  aMem : TdeSchemaObjectMemento;
  i    : integer;
begin
  anObject.Lock;
  inherited Restore(anObject);
  Grp := TdeSchemaGroup(anObject);
  Grp.Clear;
  for i := 0 to Items.Count-1 do begin
    aMem := TdeSchemaObjectMemento(Items[i]);
    aMem.CloneObject(anObject.Owner,Grp,Layer);
  end;
  anObject.unLock;
end;

destructor  TdeGroupMemento.Destroy;
begin
  while (Items.Count>0) do begin
    TObject(Items[0]).Free;
    Items.Delete(0);
  end;
  inherited Destroy;
end;


type
  TdeCustomCurveMemento = class(TdeSchemaObjectMemento)
  protected
    Points        : array of TPoint2D;
    Color         : TColor;
    BorderColor   : TColor;
    BorderStyle   : TPenStyle;
    BorderWidth   : integer;
    Transparent   : boolean;
    constructor Create(anOrigin:TdeCustomCurve);
  public
    procedure   Restore(anObject:TdeSchemaObject);override;
    destructor  Destroy;override;
  end;

constructor TdeCustomCurveMemento.Create(anOrigin:TdeCustomCurve);
var
  Cnt : integer;
begin
  inherited Create(anOrigin);
  Cnt := high(anOrigin.FDrawPath)+1;
  if (Cnt>0) then begin
    setLength(Points,Cnt);
    system.Move(anOrigin.FDrawPath[0],Points[0],Cnt*sizeOf(TPoint2D));
  end
  else
    Points := nil;
  {}
  Color       := anOrigin.Color;
  BorderColor := anOrigin.BorderColor;
  BorderStyle := anOrigin.BorderStyle;
  BorderWidth := anOrigin.BorderWidth;
  Transparent := anOrigin.Transparent;
end;

procedure   TdeCustomCurveMemento.Restore(anObject:TdeSchemaObject);
begin
  anObject.Lock;
  inherited Restore(anObject);
  with TdeCustomCurve(anObject) do begin
    setDrawPath(Points,High(Points)+1);
    Color       := Self.Color;
    BorderColor := Self.BorderColor;
    BorderStyle := Self.BorderStyle;
    BorderWidth := Self.BorderWidth;
    Transparent := Self.Transparent;
  end;
  anObject.unLock;
end;

destructor  TdeCustomCurveMemento.Destroy;
begin
  Points := nil;
  inherited Destroy;
end;



{TdeSchemaObject}
class function TdeSchemaObject.ClassOptions:TdeObjectClassOptions;
begin
  Result := [ocoResizable,ocoRotatable];
end;

constructor  TdeSchemaObject.Create(anOwner:TdeSchema);
begin
  inherited Create;
  M2D_Identity(FConvertion);
  M2D_Identity(FOtoSConv);
  M2D_Identity(FRConvertion);
  M2D_Identity(FROToSConv);
  FOwner      := anOwner;
  FParent     := nil;
  FDeleting   := false;
  FWaitDelete := false;
  FChangeParent := false;
  FChangeLayer  := false;
  FLocked     := 0;
  FChanging   := 0;
  FModified   := false;
  FChangedProps := [];
end;

destructor   TdeSchemaObject.Destroy;
begin
  FDeleting := true;
  if (Layer<>nil) then
    Layer.Remove(Self);
  if (Parent<>nil) then
    Parent.Remove(Self);
  Owner.Delete(Self);
  inherited Destroy;
end;

function     TdeSchemaObject.getBounds:TRect2D;
begin
  if FBndsChanged then
    FBounds := recalcBounds;
  Result := FBounds;
end;

function     TdeSchemaObject.getLevel:integer;
var
  anItem : TdeSchemaObject;
begin
  Result := 0;
  anItem := Parent;
  while (anItem<>nil) do begin
    Result := Result + 1;
    anItem := anItem.Parent;
  end;
end;

function     TdeSchemaObject.getSchemaBounds:TRect2D;
begin
  Result.TopLeft := OtoS(RectTopLeft(Bounds));
  Result.BottomRight := Result.TopLeft;
  Result := ExpandRect2D(Result,OtoS(RectTopRight(Bounds)));
  Result := ExpandRect2D(Result,OtoS(RectBottomRight(Bounds)));
  Result := ExpandRect2D(Result,OtoS(RectBottomLeft(Bounds)));
end;

procedure    TdeSchemaObject.Changing;
begin
  {inc(FChanging);
  if FChanging>1 then exit;
  if FLocked>0 then exit;
  doChanging;}
end;

procedure    TdeSchemaObject.doChanging;
begin
  if (Owner<>nil) then
    Owner.ItemChanging(Self);
  if (Parent<>nil) then
    Parent.ItemChanging(Self);
end;

procedure    TdeSchemaObject.Changed(ChangedProps:TdeObjectChangedProps);
begin
  //dec(FChanging);
  FModified := true;
  FChangedProps := FChangedProps+ChangedProps;
  {if (FChanging<=0) then begin
    FChanging := 0;}
    FChanged := (FLocked>0);
    if not FChanged and (FChangedProps <> []) then begin
      doChanged;
      FChangedProps := [];
    end;
  //end;
end;

procedure    TdeSchemaObject.doChanged;
begin
  if (Parent<>nil) then
    Parent.ItemChanged(Self,FChangedProps);
  if (Owner<>nil) then
    Owner.ItemChanged(Self,FChangedProps);
end;

procedure    TdeSchemaObject.UpdateBounds;
var
  R : TRect2D;
begin
  FBndsChanged := (FLocked>0);
  if not(FBndsChanged) then begin
    R := recalcBounds;
    if not(RectsEqual2D(R,FBounds)) then begin
      FChangedProps := FChangedProps + [ocpBounds];
      FBounds       := R;
    end;
  end;
end;

procedure    TdeSchemaObject.UpdateConvertion;
var
  Prnt : TdeSchemaGroup;
begin
  Prnt := Parent;
  if (Prnt<>nil) then
    FOtoSConv:=MultMM2D(FConvertion,Prnt.FOtoSConv)
  else
    FOtoSConv:=FConvertion;
  FRConvertion := M2D_Reverse(FConvertion);
  FROToSConv   := M2D_Reverse(FOtoSConv);
end;

procedure    TdeSchemaObject.Update;
begin
  if FChangeParent then exit;
  UpdateConvertion;
end;

{function     TdeSchemaObject.getAsString:string;
var
  s,n : string;
  L   : tStringList;
begin
  L   := tStringList.Create;
  with tStringList.Create do try
    Text := '';
    if not M2D_IsIdentity(FConvertion) then begin
      Str(FConvertion.M11:10:5,n);
      Str(FConvertion.M12:10:5,s);
      n := n+','+s;
      Str(FConvertion.M21:10:5,s);
      n := n+','+s;
      Str(FConvertion.M22:10:5,s);
      n := n+','+s;
      Str(FConvertion.M31:10:5,s);
      n := n+','+s;
      Str(FConvertion.M32:10:5,s);
      n := n+','+s;
      Values['Matrix'] := n;
    end;
    Result := Text;
  finally
    Free;
    L.Free;
  end;
end;

procedure    TdeSchemaObject.setAsString(aValue:string);
var
  k   : integer;
  L   : TStringList;
begin
  Changing;
  Lock;
  L := TStringList.Create;
  with tStringList.Create do try
    Text := aValue;
    L.CommaText := trim(Values['Matrix']);
    if L.Count>0 then begin
      Val(trim(L[0]),FConvertion.M11,k);
      Val(trim(L[1]),FConvertion.M12,k);
      Val(trim(L[2]),FConvertion.M21,k);
      Val(trim(L[3]),FConvertion.M22,k);
      Val(trim(L[4]),FConvertion.M31,k);
      Val(trim(L[5]),FConvertion.M32,k);
    end
    else
      M2D_Identity(FConvertion);
  finally
    Free;
    L.Free;
  end;
  unLock;
  Changed([ocpParams,ocpParams,ocpLayer]);
end;
}

function     TdeSchemaObject.getData(pData:pointer;var iSize:integer):boolean;
var nSize : integer;
begin
  nSize := sizeOf(TdeSchemaObjectData)+Length(LayerName);
  Result := (nSize<=iSize);
  if Result then
    begin
      with PdeSchemaObjectData(pData)^ do
        begin
          Header.nSize := nSize;
          Header.pInheritedData := 0;
          Header.pInheritedSize := 0;
          Matrix := FConvertion;
        end;
    end;
  iSize := nSize;
end;

procedure    TdeSchemaObject.setData(pData:pointer;iSize:integer);
begin
  if iSize<sizeOf(TdeSchemaObjectData) then exit;
  Lock;
  try
    setConvertion(PdeSchemaObjectData(pData)^.Matrix);
  finally
    unLock;
  end;
end;

procedure    TdeSchemaObject.PaintTo(Canvas:TCanvas;StoC:TMatrix2D;WidthLine:Integer=1);
begin
end;

procedure    TdeSchemaObject.PaintFrame(Canvas:TCanvas;StoC:TMatrix2D);
var iP0,iP : TPoint;
begin
  iP0 := P2DtoP(MultVM2D(OtoS(rectTopLeft(Bounds)),StoC));
  Canvas.MoveTo(iP0.X,iP0.Y);
  iP := P2DtoP(MultVM2D(OtoS(rectTopRight(Bounds)),StoC));
  Canvas.LineTo(iP.X,iP.Y);
  iP := P2DtoP(MultVM2D(OtoS(rectBottomRight(Bounds)),StoC));
  Canvas.LineTo(iP.X,iP.Y);
  iP := P2DtoP(MultVM2D(OtoS(rectBottomLeft(Bounds)),StoC));
  Canvas.LineTo(iP.X,iP.Y);
  Canvas.LineTo(iP0.X,iP0.Y);
end;

function     TdeSchemaObject.Contain(Canvas:TCanvas;StoC:TMatrix2D;X,Y:integer):boolean;
begin
  Result := false;
end;

procedure    TdeSchemaObject.setParent(aValue:TdeSchemaGroup);
begin
  if Deleting or (Parent=aValue) or FChangeParent then exit;

  FChangeParent    := true;
  Changing;
  if (Parent<>nil) then
    begin
      FConvertion := FOtoSConv;
      Parent.Remove(Self);
    end
  else
    Owner.RootRemove(Self);

  FParent := aValue;

  if (Parent<>nil) then
    begin
      FConvertion := MultMM2D(FOtoSConv,Parent.FROtoSConv);
      Parent.Add(Self);
      Layer := Parent.Layer;
    end
  else
    Owner.RootAdd(Self);
  Changed([ocpParent]);
  FChangeParent := false;
end;

function     TdeSchemaObject.getTopParent:TdeSchemaObject;
begin
  Result := Self;
  while (Result.Parent<>nil) do
    Result := Result.Parent;
end;

function     TdeSchemaObject.getLayer:TdeSchemaLayer;
begin
  if Parent <> nil then
    Result := Parent.Layer
  else
    Result := FLayer;
end;

procedure    TdeSchemaObject.setLayer(aValue:TdeSchemaLayer);
begin
  if (Parent<>nil)or Deleting or (FLayer=aValue) or FChangeLayer then exit;
  FChangeLayer    := true;
  Changing;

  if (FLayer<>nil) then
    FLayer.Remove(Self);

  FLayer := aValue;

  if (FLayer<>nil) then
    FLayer.Add(Self);
  FChangeLayer := false;
  Changed([ocpLayer]);
end;

function     TdeSchemaObject.getLayerName:string;
begin
  if (Layer=nil) then Result := ''
                 else Result := Layer.Name;
end;

function     TdeSchemaObject.getIndex:integer;
begin
  if (Parent <> nil) then
    Result := Parent.indexOf(Self)
  else
    Result := Owner.RootItemIndex(Self);
end;

procedure    TdeSchemaObject.setIndex(newIndex:integer);
begin
  if Index = newIndex then exit;
  if (Parent <> nil) then
    Parent.ChangeChildIndex(Index,newIndex)
  else
    Owner.ChangeRootItemIndex(Index,newIndex);
end;

procedure    TdeSchemaObject.setConvertion(aValue:TMatrix2D);
begin
  if M2D_IsEqual(FConvertion,aValue) then exit;;
  Changing;
  FConvertion := aValue;
  Update;
  UpdateBounds;
  Changed([]);
end;

function     TdeSchemaObject.getTopLeft:TPoint2D;
begin
  Result := SchemaBounds.TopLeft;
end;

procedure    TdeSchemaObject.setTopLeft(aValue:TPoint2D);
var
  P,C : TPoint2D;
begin
  C := OtoS(RectCenter(Bounds));
  P := TopLeft;
  MoveTo(OffsetPoint2D(aValue,C.X-P.X,C.Y-P.Y));
end;

function     TdeSchemaObject.getOffset:TPoint2D;
begin
  Result := RectCenter(SchemaBounds);
end;

procedure    TdeSchemaObject.setOffset(aValue:TPoint2D);
begin
  MoveTo(aValue);
end;

function     TdeSchemaObject.getWidth:double;
begin
  Result := Distance2D(OtoS(RectTopLeft(Bounds)),
                       OtoS(RectTopRight(Bounds)));
end;

procedure    TdeSchemaObject.setWidth(aValue:double);
var
  W,A : double;
  V   : TPoint2D;
begin
  W := Width;
  A := Rotation*Pi/180;
  V := Point2D((aValue-W)*cos(A),(aValue-W)*sin(A));
  Resize(bpcRight,V,false);
end;

function     TdeSchemaObject.getHeight:double;
begin
  Result := Distance2D(OtoS(RectTopLeft(Bounds)),
                       OtoS(RectBottomLeft(Bounds)));
end;

procedure    TdeSchemaObject.setHeight(aValue:double);
var
  H,A : double;
  V   : TPoint2D;
begin
  H := Height;
  A := Rotation*Pi/180;
  V := Point2D(-(aValue-H)*sin(A),-(aValue-H)*cos(A));
  Resize(bpcBottom,V,false);
end;

function     TdeSchemaObject.getRotation:double;
begin
  Result := AngleToPoint(Point2D(Offset.X,Offset.Y+100));
end;

procedure    TdeSchemaObject.setRotation(aValue:double);
begin
  Rotate(aValue-Rotation);
end;

function     TdeSchemaObject.getReadOnly:boolean;
begin
  Result := (Layer<>nil)and(Layer.ReadOnly);
end;

function     TdeSchemaObject.getVisible:boolean;
begin
  Result := (Layer=nil)or(Layer.Visible);
end;

function     TdeSchemaObject.CreateMemento:TdeSchemaObjectMemento;
begin
  Result := TdeSchemaObjectMemento.Create(Self);
end;

function     TdeSchemaObject.IGroup:TdeSchemaGroup;
begin
  Result := nil;
end;

procedure    TdeSchemaObject.Delete;
begin
  FWaitDelete := (FLocked>0);
  if not(FWaitDelete or Deleting) then
    Free;
end;

function     TdeSchemaObject.OToS(aPoint:TPoint2D):TPoint2D;
begin
  Result := MultVM2D(aPoint,FOToSConv);
end;

function     TdeSchemaObject.SToO(aPoint:TPoint2D):TPoint2D;
begin
  Result := MultVM2D(aPoint,FROToSConv);
end;

function     TdeSchemaObject.OToP(aPoint:TPoint2D):TPoint2D;
begin
  Result := MultVM2D(aPoint,FConvertion);
end;

function     TdeSchemaObject.PToO(aPoint:TPoint2D):TPoint2D;
begin
  Result := MultVM2D(aPoint,FRConvertion);
end;

procedure    TdeSchemaObject.Lock;
begin
  if (FLocked<0) then
    FLocked := 0;
  inc(FLocked);
end;

procedure    TdeSchemaObject.unLock;
begin
  dec(FLocked);
  if (FLocked=0) then
    begin
      if not(FDeleting) then
        begin
          if FBndsChanged then
            UpdateBounds;
          if FChanged then
            Changed([]);
        end;
      if FWaitDelete then
        Delete;
    end
  else if (FLocked<0) then
    FLocked := 0;
end;

procedure    TdeSchemaObject.ApplyConvertion(aConvertion:TMatrix2D);
begin
  if M2D_IsIdentity(aConvertion) then exit;
  Changing;
  FConvertion := MultMM2D(FOtoSConv,aConvertion);
  if (Parent<>nil) then
    FConvertion := MultMM2D(Parent.FROtoSConv,FConvertion);
  UpdateConvertion;
  Changed([ocpBounds]);
end;

procedure    TdeSchemaObject.DropConvertion;
begin
  M2D_Identity(FConvertion);
  UpdateConvertion;
end;

procedure    TdeSchemaObject.MoveTo(aPoint:TPoint2D);
var
  V : TPoint2D;
begin
  Changing;
  FOwner.MoveObjectShortcutsBy(Self,Vector2D(Offset,aPoint));
  V := Vector2D(OtoS(RectCenter(Bounds)),aPoint);
  FConvertion := M2D_Shift(FOtoSConv,V.X,V.Y);
  if (Parent<>nil) then
    FConvertion := MultMM2D(Parent.FROtoSConv,FConvertion);
  UpdateConvertion;
  Changed([ocpBounds]);
end;

procedure    TdeSchemaObject.MoveBy(aVector:TPoint2D);
begin
  Changing;
  FOwner.MoveObjectShortcutsBy(Self,aVector);
  FConvertion := M2D_Shift(FOtoSConv,aVector.X,aVector.Y);
  if (Parent <> nil) then
    FConvertion := MultMM2D(Parent.FROtoSConv,FConvertion);
  UpdateConvertion;
  Changed([ocpBounds]);
end;

function     TdeSchemaObject.AngleToPoint(aPoint:TPoint2D):double;
var
  R      : TRect2D;
  P      : TPoint2D;
  V,V0   : TPoint2D;
  P00,PR : TPoint2D;
  dV,dV0 : double;
  cosA   : double;
  sinA   : double;
begin
  R := Bounds;
  P := aPoint;
  P00 := OtoS(RectCenter(R));
  PR  := OtoS(RectTop(R));
  V0  := Vector2D(P00,PR);
  V   := Vector2D(P00,P);
  dV  := sqrt(sqr(V.X)+sqr(V.Y));
  dV0 := sqrt(sqr(V0.X)+sqr(V0.Y));
  if (abs(dV*dV0) < DE_EPSILON) then
    Result := 0
  else begin
    cosA := ((V.X*V0.X)+(V.Y*V0.Y))/(dV*dV0);
    sinA := ((V.X*V0.Y)-(V.Y*V0.X))/(dV*dV0);
    if (sinA > 0.0) then
      Result :=-arccos(cosA*0.99999999)
    else
      Result :=arccos(cosA*0.99999999);
  end;
  if Result >= 0 then
    Result := trunc(Result*180/Pi+0.5)*Pi/180
  else
    Result := (360+trunc(Result*180/Pi-0.5))*Pi/180;
end;

procedure    TdeSchemaObject.Rotate(Angle:double);
var
  Fix : TPoint2D;
begin
  Changing;
  Fix := OtoP(RectCenter(Bounds));
  FConvertion:=M2D_Shift(FConvertion,-Fix.X,-Fix.Y);
  FConvertion:=M2D_Rotate(FConvertion,Angle);
  FConvertion:=M2D_Shift(FConvertion,Fix.X,Fix.Y);
  UpdateConvertion;
  Changed([ocpBounds]);
end;

procedure    TdeSchemaObject.Flip(Direction: TdeFlipDirection);
var
  Fix : TPoint2D;
begin
  Changing;
  Fix := OtoP(RectCenter(Bounds));
  FConvertion:=M2D_Shift(FConvertion,-Fix.X,-Fix.Y);
  if Direction = fdVertical then
    FConvertion:=M2D_Scale(FConvertion,1,-1)
  else
    FConvertion:=M2D_Scale(FConvertion,-1,1);

  FConvertion:=M2D_Shift(FConvertion,Fix.X,Fix.Y);
  UpdateConvertion;
  Changed([ocpBounds]);
end;

procedure    TdeSchemaObject.Resize(BoundPoint:TdeBoundsPointClass;
                                        Vector:TPoint2D;
                                  Proportional:boolean = false);
var
  P1,P2          : TPoint2D;
  Kx,Ky          : double;
  fixX,fixY      : double;
  W0,H0,W,H      : double;
  R0             : TRect2D;
  Vx,Vy          : TPoint2D;
  P_TL,P_TR,P_BL : TPoint2D;
  deM            : TMatrix2D;
begin
  Changing;
  P1 := MultVM2D(Point2D(0,0),FROtoSConv);
  P2 := MultVM2D(Vector,FROtoSConv);
  Vector := Vector2D(P1,P2);

  R0 := Bounds;
  W0 := R0.Right-R0.Left;
  H0 := R0.Top-R0.Bottom;

  P_TL := RectTopLeft(R0);
  P_TR := RectTopRight(R0);
  P_BL := RectBottomLeft(R0);
  Vx := Vector2D(P_TL,P_TR);
  Vy := Vector2D(P_TL,P_BL);
  W := VLength2D(Vx);
  H := VLength2D(Vy);

  case BoundPoint of
    bpcTopLeft       : begin
      fixX := R0.Right;
      fixY := R0.Bottom;
      if (abs(W0) < DE_EPSILON) then
        Kx := 1
      else if (abs(W) < DE_EPSILON) then
        Kx := VLength2D(Vector)/W0
      else
        Kx := 1-ScalarVV2D(Vector,Vx)/(W*W);
      if (abs(H0) < DE_EPSILON) then
        Ky := 1
      else if (abs(H) < DE_EPSILON) then
        Ky := VLength2D(Vector)/H0
      else
        Ky := 1-ScalarVV2D(Vector,Vy)/(H*H);
    end;
    bpcTop           : begin
      fixX := (R0.Right+R0.Left)/2;
      fixY := R0.Bottom;
      if (abs(H0) < DE_EPSILON) then
        Ky := 1
      else if (abs(H) < DE_EPSILON) then
        Ky := VLength2D(Vector)/H0
      else
        Ky := 1-ScalarVV2D(Vector,Vy)/(H*H);
      Kx := 1;
    end;
    bpcTopRight      : begin
      fixX := R0.Left;
      fixY := R0.Bottom;
      if (abs(W0) < DE_EPSILON) then
        Kx := 1
      else if (abs(W) < DE_EPSILON) then
        Kx := VLength2D(Vector)/W0
      else
        Kx := 1+ScalarVV2D(Vector,Vx)/(W*W);
      if (abs(H0) < DE_EPSILON) then
        Ky := 1
      else if (abs(H) < DE_EPSILON) then
        Ky := VLength2D(Vector)/H0
      else
        Ky := 1-ScalarVV2D(Vector,Vy)/(H*H);
    end;
    bpcRight         : begin
      fixX := R0.Left;
      fixY := (R0.Top+R0.Bottom)/2;
      if (abs(W0) < DE_EPSILON) then
        Kx := 1
      else if (abs(W) < DE_EPSILON) then
        Kx := VLength2D(Vector)/W0
      else
        Kx := 1+ScalarVV2D(Vector,Vx)/(W*W);
      Ky := 1;
    end;
    bpcBottomRight   : begin
      fixX := R0.Left;
      fixY := R0.Top;
      if (abs(W0) < DE_EPSILON) then
        Kx := 1
      else if (abs(W) < DE_EPSILON) then
        Kx := VLength2D(Vector)/W0
      else
        Kx := 1+ScalarVV2D(Vector,Vx)/(W*W);
      if (abs(H0) < DE_EPSILON) then
        Ky := 1
      else if (abs(H) < DE_EPSILON) then
        Ky := VLength2D(Vector)/H0
      else
        Ky := 1+ScalarVV2D(Vector,Vy)/(H*H);
    end;
    bpcBottom        : begin
      fixX := (R0.Right+R0.Left)/2;
      fixY := R0.Top;
      if (abs(H0) < DE_EPSILON) then
        Ky := 1
      else if (abs(H) < DE_EPSILON) then
        Ky := VLength2D(Vector)/H0
      else
        Ky := 1+ScalarVV2D(Vector,Vy)/(H*H);
      Kx := 1;
    end;
    bpcBottomLeft    : begin
      fixX := R0.Right;
      fixY := R0.Top;
      if (abs(W0) < DE_EPSILON) then
        Kx := 1
      else if (abs(W) < DE_EPSILON) then
        Kx := VLength2D(Vector)/W0
      else
        Kx := 1-ScalarVV2D(Vector,Vx)/(W*W);
      if (abs(H0) < DE_EPSILON) then
        Ky := 1
      else if (abs(H) < DE_EPSILON) then
        Ky := VLength2D(Vector)/H0
      else
        Ky := 1+ScalarVV2D(Vector,Vy)/(H*H);
    end;
    bpcLeft          : begin
      fixX := R0.Right;
      fixY := (R0.Top+R0.Bottom)/2;
      if (abs(W0) < DE_EPSILON) then
        Kx := 1
      else if (abs(W) < DE_EPSILON) then
        Kx := VLength2D(Vector)/W0
      else
        Kx := 1-ScalarVV2D(Vector,Vx)/(W*W);
      Ky := 1;
    end;
    else begin
      fixX := 0;
      fixY := 0;
      Kx := 1;
      Ky := 1;
    end;
  end;
  if Proportional then begin
    if Kx = 1 then
      Kx := Ky
    else
      Ky := Kx;  
  end;
  M2D_Identity(deM);
  deM := M2D_Shift(deM,-fixX,-fixY);
  deM := M2D_Scale(deM,Kx,Ky);
  deM := M2D_Shift(deM,fixX,fixY);
  FConvertion := MultMM2D(deM,FConvertion);
  UpdateConvertion;
  Changed([ocpBounds]);
end;

procedure     TdeSchemaObject.getBoundsPoints(
                                                     Canvas : TCanvas;
                                                       StoV : TMatrix2D;
                                          var P_TopLeft     : TPoint2D;
                                          var P_TopRight    : TPoint2D;
                                          var P_BottomRight : TPoint2D;
                                          var P_BottomLeft  : TPoint2D);
var
  B : TRect2D;
  M : TMatrix2D;
begin
  B := Bounds;
  M := MultMM2D(FOtoSConv,StoV);
  P_TopLeft  := MultVM2D(RectTopLeft(B),M);
  P_TopRight := MultVM2D(RectTopRight(B),M);
  P_BottomRight := MultVM2D(RectBottomRight(B),M);
  P_BottomLeft := MultVM2D(RectBottomLeft(B),M);
end;

function      TdeSchemaObject.getViewRect(Canvas:TCanvas;StoV:TMatrix2D):TRect;
var
  PTL,PTR,PBR,PBL : TPoint2D;
begin
  getBoundsPoints(Canvas,StoV,PTL,PTR,PBR,PBL);
  Result.TopLeft := P2DtoP(PTL);
  Result.BottomRight := Result.TopLeft;
  Result := ExpandRect(Result,P2DtoP(PTR));
  Result := ExpandRect(Result,P2DtoP(PBR));
  Result := ExpandRect(Result,P2DtoP(PBL));
end;




{TdeSchemaObjectList}
constructor TdeSchemaObjectList.Create;
begin
  inherited Create;
  Clear;
  FOnChange := nil;
end;

destructor  TdeSchemaObjectList.Destroy;
begin
  FOnChange := nil;
  Clear;
  inherited Destroy;
end;

procedure   TdeSchemaObjectList.Clear;
begin
  FCount    := 0;
  FCapacity := 0;
  FItems    := nil;
  FIndex    := nil;
end;

function    TdeSchemaObjectList.getItem(index:integer):TdeSchemaObject;
begin
  if (index >= 0)
     and(index < FCount)
  then
    Result := FItems[index]
  else
    raise EListError.Create(Format('Index is out of bounds (%d)',[index]));
end;

function    TdeSchemaObjectList.find(anItem:TdeSchemaObject;var index:integer):boolean;
var
  iMin,iMax,iMid,C : integer;
begin
  Result := false;
  iMin := 0;
  iMax := FCount-1;
  while (iMin <= iMax) do begin
    iMid := (iMin+iMax) shr 1;
    C := FIndex[iMid].Key - integer(anItem);
    if (C < 0) then
      iMin := iMid+1
    else begin
      iMax := iMid-1;
      Result := (C=0);
    end;
  end;
  index := iMin;
end;

procedure   TdeSchemaObjectList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  FCapacity := FCapacity+Delta;
  setLength(FItems,FCapacity);
  setLength(FIndex,FCapacity);
end;

procedure   TdeSchemaObjectList.NotifyChanged;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function    TdeSchemaObjectList.Add(anItem:TdeSchemaObject):integer;
var
  index,i:integer;
begin
  if FCount = FCapacity then Grow;
  if find(anItem,index) then begin
    Result := FIndex[index].Index;
  end
  else begin
    FItems[FCount] := anItem;
    for i := FCount downto index+1 do
      FIndex[i]:=FIndex[i-1];
    FIndex[index].Key   := integer(anItem);
    FIndex[index].Index := FCount;
    Result := FCount;
    FCount := FCount + 1;
    NotifyChanged;
  end;
end;

procedure   TdeSchemaObjectList.Insert(index:integer;anItem:TdeSchemaObject);
var
  curIndex,i:integer;
begin
  if (index >= 0)
     and(index <= FCount) then begin
    if FCount = FCapacity then Grow;
    if find(anItem,curIndex) then begin
    end
    else begin
      for i := FCount downto index+1 do
        FItems[i]:=FItems[i-1];
      FItems[index] := anItem;
      for i := FCount downto curIndex+1 do
        FIndex[i]:=FIndex[i-1];
      for i := 0 to FCount do begin
        if (FIndex[i].Index>=index) then
          inc(FIndex[i].Index);
      end;
      FIndex[curIndex].Key   := integer(anItem);
      FIndex[curIndex].Index := index;
      FCount := FCount + 1;
      NotifyChanged;
    end;
  end
  else
    raise EListError.Create(Format('Index is out of bounds (%d)',[index]));
end;

procedure   TdeSchemaObjectList.Remove(anItem:TdeSchemaObject);
var
  index : integer;
begin
  index := indexOf(anItem);
  if index >= 0 then
    Delete(index);
end;

procedure   TdeSchemaObjectList.Delete(index:integer);
var
  curIndex,i : integer;
begin
  if (index >= 0) and(index < FCount) then
    begin
      if find(FItems[index],curIndex) then
        begin
          for i := curIndex to FCount-2 do
            FIndex[i] := FIndex[i+1];
          for i := 0 to FCount-2 do
            begin
              if (FIndex[i].Index >= index) then
                dec(FIndex[i].Index);
            end;
        end;
      for i := Index to FCount-2 do
        FItems[i] := FItems[i+1];
      FCount := FCount - 1;
      NotifyChanged;
    end
  else
    raise EListError.Create(Format('Index is out of bounds (%d)',[index]));
end;

procedure   TdeSchemaObjectList.Move(curIndex,newIndex:integer);
var anItem   : TdeSchemaObject;
    _ChgHdlr : TNotifyEvent;
    i        : Integer;
begin
  if (curIndex >= 0)and(curIndex < FCount)
  then begin
    if (newIndex >= 0)and(newIndex < FCount)
    then begin
      if curIndex<newIndex then
        for i:=curIndex to newIndex do
          Items[i].FModified:=True
      else
        for i:=newIndex to curIndex do
          Items[i].FModified:=True;

      _ChgHdlr := FOnChange;
      FOnChange := nil;
      anItem := Items[curIndex];
      Delete(curIndex);
      Insert(newIndex,anItem);
      FOnChange := _ChgHdlr;
      NotifyChanged;
    end
    else
      raise EListError.Create(Format('Index is out of bounds (%d)',[newIndex]));
  end
  else
    raise EListError.Create(Format('Index is out of bounds (%d)',[curIndex]));
end;

function    TdeSchemaObjectList.indexOf(anItem:TdeSchemaObject):integer;
var
  index:integer;
begin
  if find(anItem,index) then
    Result := FIndex[index].Index
  else
    Result :=-1;
end;


{TdeSchemaGroup}
class function TdeSchemaGroup.ClassID:integer;
begin
  Result :=-1;
end;

constructor  TdeSchemaGroup.Create(anOwner:TdeSchema);
begin
  inherited Create(anOwner);
  FItems  := TdeSchemaObjectList.Create;
end;

destructor   TdeSchemaGroup.Destroy;
begin
  FDeleting := true;
  Clear;
  FItems.Free;
  inherited Destroy;
end;

function     TdeSchemaGroup.RecalcBounds:TRect2D;
var
  i   : integer;
  Bnd : TRect2D;
  Obj : TdeSchemaObject;
begin
  if Count = 0 then
    Result := Rect2D(0,0,0,0)
  else begin
    Obj := Item[0];
    Bnd := Obj.Bounds;
    Result.TopLeft := Obj.OToP(RectTopLeft(Bnd));
    Result.BottomRight := Result.TopLeft;
    Result := ExpandRect2D(Result,Obj.OtoP(RectTopRight(Bnd)));
    Result := ExpandRect2D(Result,Obj.OtoP(RectBottomRight(Bnd)));
    Result := ExpandRect2D(Result,Obj.OtoP(RectBottomLeft(Bnd)));
    for i := 1 to Count-1 do begin
      Obj := Item[i];
      Bnd := Obj.Bounds;
      Result := ExpandRect2D(Result,Obj.OtoP(RectTopLeft(Bnd)));
      Result := ExpandRect2D(Result,Obj.OtoP(RectTopRight(Bnd)));
      Result := ExpandRect2D(Result,Obj.OtoP(RectBottomRight(Bnd)));
      Result := ExpandRect2D(Result,Obj.OtoP(RectBottomLeft(Bnd)));
    end;
  end;
end;

procedure    TdeSchemaGroup.doChanging;
begin
  inherited doChanging;
end;

procedure    TdeSchemaGroup.doChanged;
begin
  UpdateItems;
  inherited doChanged;
end;

procedure    TdeSchemaGroup.UpdateConvertion;
begin
  inherited updateConvertion;
  UpdateItems;
end;

procedure    TdeSchemaGroup.Update;
begin
  inherited Update;
end;

procedure    TdeSchemaGroup.UpdateItems;
var i:integer;
begin
  for i := 0 to Count-1 do
    Item[i].Update;
end;

procedure    TdeSchemaGroup.ItemChanging(anItem:TdeSchemaObject);
begin
  if Parent <> nil then
    Parent.ItemChanging(Self);
end;


procedure    TdeSchemaGroup.ItemChanged(anItem:TdeSchemaObject;
                                  ChangedProps:TdeObjectChangedProps);
begin
  UpdateBounds;
  if Parent <> nil then
    Parent.ItemChanged(Self,[ocpSubItems]);
end;

procedure    TdeSchemaGroup.PaintTo(Canvas:TCanvas;StoC:TMatrix2D;WidthLine:Integer=1);
var
  i : integer;
  //VR : TRect;
begin
  {VR := getViewRect(StoC);
  if (abs(VR.Right-VR.Left)<=10)and(abs(VR.Bottom-VR.Top)<=10) then exit;
  if not IntersectRect(VR,VR,Canvas.ClipRect) then exit;}
  for i := 0 to Count-1 do
    Item[i].PaintTo(Canvas,StoC,WidthLine);
end;

procedure    TdeSchemaGroup.PaintFrame(Canvas:TCanvas;StoC:TMatrix2D);
var
  i      : integer;
  //VR : TRect;
begin
  {VR := getViewRect(StoC);
  if (abs(VR.Right-VR.Left)<=10)and(abs(VR.Bottom-VR.Top)<=10) then exit;
  if not IntersectRect(VR,VR,Canvas.ClipRect) then exit;}
  if (Count < 10) then begin
    for i:=0 to Count-1 do
      Item[i].PaintFrame(Canvas,StoC);
  end;
  inherited PaintFrame(Canvas,StoC);
end;

function     TdeSchemaGroup.Contain(Canvas:TCanvas;StoC:TMatrix2D;X,Y:integer):boolean;
var
  i      : integer;
  anItem : TdeSchemaObject;
begin
  Result := false;
  i:=Count-1;
  while (i >= 0)and not(Result) do begin
    anItem := Item[i];
    Result := anItem.Contain(Canvas,StoC,X,Y);
    dec(i);
  end;
end;

function     TdeSchemaGroup.getCount:integer;
begin
  Result := FItems.Count;
end;

function     TdeSchemaGroup.getItem(index:integer):TdeSchemaObject;
begin
  Result := FItems[index];
end;

function     TdeSchemaGroup.CreateMemento:TdeSchemaObjectMemento;
begin
  Result := TdeGroupMemento.Create(Self);
end;

function     TdeSchemaGroup.IGroup:TdeSchemaGroup;
begin
  Result := Self;
end;

procedure    TdeSchemaGroup.Clear;
begin
  Lock;
  try
    while (Count > 0) do
      Item[Count-1].Delete;
  finally
    unLock;
  end;
end;

procedure    TdeSchemaGroup.DropConvertion;
var i : integer;
begin
  for i:=0 to Count-1 do
    Item[i].DropConvertion;
  inherited DropConvertion;
end;

function     TdeSchemaGroup.indexOf(anItem:TdeSchemaObject):integer;
begin
  Result := FItems.IndexOf(anItem);
end;

procedure    TdeSchemaGroup.Add(anItem:TdeSchemaObject);
begin
  Changing;
  if (anItem = nil)or(indexOf(anItem) >= 0) then exit;
  FItems.Add(anItem);
  anItem.Parent := Self;
  UpdateBounds;
  Changed([ocpSubItems]);
end;

procedure    TdeSchemaGroup.Insert(index:integer;anItem:TdeSchemaObject);
begin
  Changing;
  if (anItem = nil) or (indexOf(anItem) >= 0) then exit;
  FItems.Insert(index,anItem);
  anItem.Parent := Self;
  UpdateBounds;
  Changed([ocpSubItems]);
end;

procedure    TdeSchemaGroup.Remove(anItem:TdeSchemaObject);
var i : integer;
begin
  if not Deleting then
    Changing;
  i := indexOf(anItem);
  if (i<0) then exit;
  FItems.Delete(i);
  if not anItem.Deleting then
    anItem.Parent := nil;
  if not Deleting then begin
    UpdateBounds;
    Changed([ocpSubItems]);
  end;
end;

procedure    TdeSchemaGroup.ChangeChildIndex(curIndex,newIndex:integer);
begin
  if not Deleting then
    Changing;
  FItems.Move(curIndex,newIndex);
  if not Deleting then
    Changed([ocpSubItems]);
end;




{TdeSchemaLayer}
constructor  TdeSchemaLayer.Create(anOwner:TdeSchema;aName:string);
begin
  inherited Create;
  FOwner      := anOwner;
  FName       := aName;
  FItems      := TdeSchemaObjectList.Create;
  FVisible    := true;
  FReadOnly   := false;
  FDeleting   := false;
  FModified   := false;
end;

destructor   TdeSchemaLayer.Destroy;
begin
  FDeleting   := true;
  Clear;
  FItems.Free;
  FOwner.FLayers.Delete(FOwner.LayerIndex(Name));
  FOwner.NotifyDeleteLayer(Self);
  inherited Destroy;
end;

function     TdeSchemaLayer.getCount:integer;
begin
  Result := FItems.Count;
end;

function     TdeSchemaLayer.getItem(index:integer):TdeSchemaObject;
begin
  Result := FItems[index];
end;

function     TdeSchemaLayer.getVisible:boolean;
begin
  Result := FVisible;
end;

procedure    TdeSchemaLayer.setVisible(aValue:boolean);
begin
  if (Visible = aValue) then exit;
  FVisible := aValue;
  Owner.NotifyLayerChanged(Self);
end;

function     TdeSchemaLayer.getReadOnly:boolean;
begin
  Result := FReadOnly;
end;

procedure    TdeSchemaLayer.setReadOnly(aValue:boolean);
begin
  if (ReadOnly = aValue) then exit;
  FReadOnly := aValue;
  Owner.NotifyLayerChanged(Self);
end;

procedure    TdeSchemaLayer.Delete;
begin
  if not(FDeleting) then
    Free;
end;

procedure    TdeSchemaLayer.Clear;
begin
  if Count = 0 then exit;
  Owner.Lock;
  try
    while (Count > 0) do
      Item[0].Delete;
  finally
    Owner.unLock;
  end;
end;

function     TdeSchemaLayer.IndexOf(anItem:TdeSchemaObject):integer;
begin
  Result := FItems.IndexOf(anItem);
end;

procedure    TdeSchemaLayer.Add(anItem:TdeSchemaObject);
begin
  if indexOf(anItem) >= 0 then exit;
  FItems.Add(anItem);
  FModified   := true;
  anItem.Layer := Self;
end;

procedure    TdeSchemaLayer.Remove(anItem:TdeSchemaObject);
begin
  if indexOf(anItem) < 0 then exit;
  FItems.Remove(anItem);
  if not FDeleting then begin
    FModified   := true;
    anItem.Layer := nil;
  end;
end;




{TdeSchemaShortcut}
constructor  TdeSchemaShortcut.Create(anOwner:TdeSchema);
begin
  inherited Create;
  FOwner       := anOwner;
  FDeleting    := false;
  FOffset      := Point2D(0.0,0.0);
  FBitmap      := nil;
  FCaption     := '';
  FImageIndex  := 0;
  FOptions     := [scoShowCaption,scoShowImage];
  FModified    := false;
  FOwner.FShortcuts.Add(Self);
end;

destructor   TdeSchemaShortcut.Destroy;
var i : integer;
begin
  FDeleting := true;
  if FBitmap<>nil then
    FBitmap.Free;
  i := FOwner.ShortcutIndex(Self);
  if i>=0 then
    FOwner.FShortcuts.Delete(i);
  FOwner.NotifyDeleteShortcut(Self);
  inherited Destroy;
end;

procedure    TdeSchemaShortcut.UpdateBitmap;
begin
  if (FOwner.FSCImages <> nil)
     and (FImageIndex >= 0)
     and (FImageIndex < FOwner.FSCImages.Count)
  then begin
    if FBitmap = nil then
      FBitmap := TBitmap.Create;
    FOwner.FSCImages.getBitmap(FImageIndex,FBitmap);
    FBitmap.Transparent := true;
    FBitmap.TransparentMode := tmAuto;
  end
  else begin
    if FBitmap<>nil then
      FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure    TdeSchemaShortcut.setOffset(aValue:TPoint2D);
begin
  FOffset   := aValue;
  FModified := true;
end;

procedure    TdeSchemaShortcut.setCaption(aValue:string);
begin
  if FCaption = aValue then exit;
  FCaption := aValue;
  FModified := true;
end;

procedure    TdeSchemaShortcut.setImageIndex(aValue:integer);
begin
  if FImageIndex = aValue then exit;
  FImageIndex := aValue;
  FModified := true;
  UpdateBitmap;
end;

procedure    TdeSchemaShortcut.setOptions(aValue:TdeShortcutOptions);
begin
  if FOptions = aValue then exit;
  FOptions := aValue;
  FModified := true;
  UpdateBitmap;
end;

procedure    TdeSchemaShortcut.setLObject(aValue:TdeSchemaObject);
begin
  if FLObject = aValue then exit;
  FModified := true;
  FLObject := aValue;
end;

function     TdeSchemaShortcut.getViewRect(Canvas:TCanvas;StoV:TMatrix2D):TRect;
var
  TWB,TWT : integer;
  TW      : integer;
  TH      : integer;
  P       : TPoint;
  Font    : TFont;
begin
  P := P2DToPEx(Offset,StoV);
  TH := 0;
  if (FBitmap <> nil)
     and (scoShowImage in Options)
  then begin
    TWB := FBitmap.Width;
    TH := TH + FBitmap.Height;
  end
  else
    TWB := 0;
  if (Caption <> '')
     and (scoShowCaption in Options)
  then begin
    Font := FOwner.ShortcutFont;
    if Font<>nil then
      Canvas.Font := Font;
    //Canvas.Font.Name := 'MS Sans Serif';
    //Canvas.Font.Size := 10;
    TWT := Canvas.TextWidth(Caption) + 1;
    TH := TH + Canvas.TextHeight(Caption) + 5;
  end
  else
    TWT := 0;
  if TWT>TWB then
    TW := TWT
  else
    TW := TWB;
  if TW > 64 then
    TW := 64;
  TW := TW div 2;
  TH := TH div 2;
  Result := Rect(P.X - TW,P.Y - TH,P.X + TW,P.Y + TH);
end;

procedure    TdeSchemaShortcut.Delete;
begin
  if not FDeleting then
    Free;
end;

function     TdeSchemaShortcut.getData(pData:pointer;var iSize:integer):boolean;
var
  nSize : integer;
begin
  nSize := sizeOf(TdeShortcutObjectData);
  Result := (nSize<=iSize);
  if Result then begin
    with PdeShortcutObjectData(pData)^ do begin
      pOffset      := Offset;
      StrLCopy(@(cCaption[0]),pChar(Caption),MAX_SHORTCUTCAPTION_SIZE);
      reserved     := 0;
      iImageIndex  := ImageIndex;
      sOptions     := Options;
      iShortcutID  := ShortcutID;
    end;
  end;
  iSize := nSize;
end;

procedure    TdeSchemaShortcut.setData(pData:pointer;    iSize:integer);
begin
  if iSize < sizeOf(TdeShortcutObjectData) then exit;
  with PdeShortcutObjectData(pData)^ do begin
    FOffset        := pOffset;
    FCaption       := cCaption;
    FImageIndex    := iImageIndex;
    FOptions       := sOptions;
    FShortcutID    := iShortcutID;
    UpdateBitmap;
  end;
end;

procedure    TdeSchemaShortcut.PaintTo(   Canvas:TCanvas;StoC:TMatrix2D);
var
  R : TRect;
  X : integer;
  Font : TFont;
begin
  Font := FOwner.ShortcutFont;
  if Font <> nil then
    Canvas.Font := Font;
  Canvas.Pen.Color   := Canvas.Font.Color;
  Canvas.Pen.Style   := psSolid;
  Canvas.Pen.Width   := 1;
  Canvas.Brush.Color := clWhite;
  Canvas.Brush.Style := bsSolid;//bsClear;
  {Canvas.Font.Name   := 'Times New Roman';
  Canvas.Font.Color  := clRed;
  Canvas.Font.Size   := 20;
  Canvas.Font.Style  := Canvas.Font.Style + [fsBold];}

  R := getViewRect(Canvas,StoC);

  if (FBitmap <> nil)
     and (scoShowImage in Options)
  then
    Canvas.Draw((R.Right + R.Left - FBitmap.Width) div 2,R.Top,FBitmap);
  if (Caption <> '')
     and (scoShowCaption in Options)
  then begin
    R.Top := R.Bottom - Canvas.TextHeight(Caption);
    X := (R.Right - R.Left - Canvas.TextWidth(Caption)) div 2;
    if X < 0 then
      X := 0;
//    Canvas.Rectangle(R);
    Canvas.TextRect(R,
                    R.Left + X,
                    R.Top,
                    Caption);
  end;
end;

procedure    TdeSchemaShortcut.PaintFrame(Canvas:TCanvas;StoC:TMatrix2D);
var
  R : TRect;
begin
  R := getViewRect(Canvas,StoC);
  Canvas.MoveTo(R.Left,R.Top);
  Canvas.LineTo(R.Right,R.Top);
  Canvas.LineTo(R.Right,R.Bottom);
  Canvas.LineTo(R.Left,R.Bottom);
  Canvas.LineTo(R.Left,R.Top);
end;

function     TdeSchemaShortcut.Contain(   Canvas:TCanvas;StoC:TMatrix2D;X,Y:integer):boolean;
var
  R : TRect;
begin
  R := getViewRect(Canvas,StoC);
  Result := PtInRect(R,Point(X,Y));
end;






constructor  TdeSchema.Create;
begin
  inherited Create;
  FRootItems     := TdeSchemaObjectList.Create;
  FLayers        := TStringList.Create;
  FLayers.Sorted := true;
  FLayers.Duplicates := dupError;
  FShortcuts     := TList.Create;
  FSCImages      := nil;
  FSCFont        := nil;
  FBounds        := Rect2D(0,0,0,0);
  FDeleting      := false;
  FClearing      := false;
end;

destructor   TdeSchema.Destroy;
begin
  FDeleting      := true;
  Clear;
  FRootItems.Free;
  FLayers.Free;
  FShortCuts.Free;
  inherited Destroy;
end;

function     TdeSchema.recalcBounds:TRect2D;
var
  i      : integer;
begin
  Result := Rect2D(0,0,0,0);
  if (RootCount = 0) then exit;
  Result := RootItem[0].SchemaBounds;
  for i := 1 to RootCount-1 do
    UnionRect2D(Result,Result,RootItem[i].SchemaBounds);
end;

procedure    TdeSchema.Lock;
begin
  if (FLockCount <= 0) then begin
    FLockCount := 0;
  end;
  inc(FLockCount);
end;

procedure    TdeSchema.unLock;
begin
  dec(FLockCount);
  if (FLockCount = 0) then begin
    if FChanged then
      Changed;
  end
  else if (FLockCount < 0) then
    FLockCount := 0;
end;

function     TdeSchema.IsLocked:boolean;
begin
  Result := (FLockCount > 0);
end;

function     TdeSchema.First:TdeSchemaObject;
begin
  Result := nil;
  if (RootCount > 0) then
    Result := RootItem[0];
end;

function     TdeSchema.Next(anItem:TdeSchemaObject):TdeSchemaObject;
var
  i : integer;
begin
  if (anItem = nil) then
    Result := First
  else if (anItem.IGroup <> nil)and(anItem.IGroup.Count > 0)then
    Result := anItem.IGroup.Item[0]
  else begin
    i := anItem.Index;
    while (anItem.Parent <> nil)and(i = (anItem.Parent.Count-1)) do begin
      anItem := anItem.Parent;
      i := anItem.Index;
    end;
    if anItem.Parent <> nil then
      Result := anItem.Parent.Item[i+1]
    else begin
      if i < (RootCount-1) then
        Result := RootItem[i+1]
      else
        Result := nil;
    end;
  end;
end;

function     TdeSchema.Prev(anItem:TdeSchemaObject):TdeSchemaObject;
var
  i : integer;
begin
  if (anItem = nil) then
    Result := Last
  else if (anItem.Parent <> nil) then begin
    i := anItem.Index;
    if i>0 then
      Result := anItem.Parent.Item[i-1]
    else
      Result := anItem.Parent;
  end
  else begin
    i := anItem.Index;
    if i>0 then
      Result := RootItem[i-1]
    else
      Result := nil;
  end;
  if (anItem <> nil)and(Result <> nil)
     and(anItem.Parent <> Result)
     and(Result.IGroup <> nil)
  then begin
    while (Result.IGroup <> nil)and(Result.IGroup.Count > 0) do
      Result := Result.IGroup.Item[Result.IGroup.Count-1];
  end;
end;

function     TdeSchema.Last:TdeSchemaObject;
begin
  Result := nil;
  if (RootCount > 0) then begin
    Result := RootItem[RootCount-1];
    while (Result.IGroup <> nil)and(Result.IGroup.Count > 0) do
      Result := Result.IGroup.Item[Result.IGroup.Count-1];
  end;
end;

function     TdeSchema.NextLeft(anItem:TdeSchemaObject):TdeSchemaObject;
var i         : Integer;
    aP,bP,tP  : TPoint2D;
    a,b,t     : TdeSchemaObject;
begin
  if anItem=nil then
    begin
      b := Item[0];
      bP:= b.Offset;

      for i:=1 to ItemsCount-1 do
        begin
          t := Item[i];
          tP:= t.Offset;
          if   ( ((tP.X<bP.X)                                  ) or
                 ((tP.X=bP.X)and(tP.Y<bP.Y)                    ) or
                 ((tP.X=bP.X)and(tP.Y=bP.Y)and(t.Index<b.Index)) )
                 and (t.Level=0) and (t.Visible) then
              begin
                b  := Item[i];
                bP := Item[i].Offset;
              end;
        end;

      Result:=b;
    end
  else
    begin
      a := anItem;
      aP:= a.Offset;
      b := nil;
      bP:= aP;

      for i:=0 to ItemsCount-1 do
        begin
          t := Item[i];
          tP:= t.Offset;
          if   ( ((tP.X<aP.X)                                  ) or
                 ((tP.X=aP.X)and(tP.Y<aP.Y)                    ) or
                 ((tP.X=aP.X)and(tP.Y=aP.Y)and(t.Index<a.Index)) )
                 and (t.Level=0) and (t.Visible) then
            if ( ((tP.X>bP.X)                                  ) or
                 ((tP.X=bP.X)and(tP.Y>bP.Y)                    ) or
                 ((tP.X=bP.X)and(tP.Y=bP.Y)and(t.Index>b.Index)) ) or (b=nil) then
              begin
                b  := Item[i];
                bP := Item[i].Offset;
              end;
        end;

      Result:=b;
    end;
end;

function     TdeSchema.NextRight(anItem:TdeSchemaObject):TdeSchemaObject;
var i         : Integer;
    aP,bP,tP  : TPoint2D;
    a,b,t     : TdeSchemaObject;
begin
  if anItem=nil then
    begin
      b := Item[0];
      bP:= b.Offset;

      for i:=1 to ItemsCount-1 do
        begin
          t := Item[i];
          tP:= t.Offset;
          if   ( ((tP.X>bP.X)                                  ) or
                 ((tP.X=bP.X)and(tP.Y>bP.Y)                    ) or
                 ((tP.X=bP.X)and(tP.Y=bP.Y)and(t.Index>b.Index)) )
                 and (t.Level=0) and (t.Visible) then
              begin
                b  := Item[i];
                bP := Item[i].Offset;
              end;
        end;

      Result:=b;
    end
  else
    begin
      a := anItem;
      aP:= a.Offset;
      b := nil;
      bP:= aP;

      for i:=0 to ItemsCount-1 do
        begin
          t := Item[i];
          tP:= t.Offset;
          if   ( ((tP.X>aP.X)                                  ) or
                 ((tP.X=aP.X)and(tP.Y>aP.Y)                    ) or
                 ((tP.X=aP.X)and(tP.Y=aP.Y)and(t.Index>a.Index)) )
                 and (t.Level=0) and (t.Visible) then
            if ( ((tP.X<bP.X)                                  ) or
                 ((tP.X=bP.X)and(tP.Y<bP.Y)                    ) or
                 ((tP.X=bP.X)and(tP.Y=bP.Y)and(t.Index<b.Index)) ) or (b=nil) then
              begin
                b  := Item[i];
                bP := Item[i].Offset;
              end;
        end;

      Result:=b;
    end;
end;

function     TdeSchema.NextTop(anItem:TdeSchemaObject):TdeSchemaObject;
var i         : Integer;
    aP,bP,tP  : TPoint2D;
    a,b,t     : TdeSchemaObject;
begin
  if anItem=nil then
    begin
      b := Item[0];
      bP:= b.Offset;

      for i:=1 to ItemsCount-1 do
        begin
          t := Item[i];
          tP:= t.Offset;
          if   ( ((tP.Y>bP.Y)                                  ) or
                 ((tP.Y=bP.Y)and(tP.X>bP.X)                    ) or
                 ((tP.Y=bP.Y)and(tP.X=bP.X)and(t.Index>b.Index)) )
                 and (t.Level=0) and (t.Visible) then
              begin
                b  := Item[i];
                bP := Item[i].Offset;
              end;
        end;

      Result:=b;
    end
  else
    begin
      a := anItem;
      aP:= a.Offset;
      b := nil;
      bP:= aP;

      for i:=0 to ItemsCount-1 do
        begin
          t := Item[i];
          tP:= t.Offset;
          if   ( ((tP.Y>aP.Y)                                  ) or
                 ((tP.Y=aP.Y)and(tP.X>aP.X)                    ) or
                 ((tP.Y=aP.Y)and(tP.X=aP.X)and(t.Index>a.Index)) )
                 and (t.Level=0) and (t.Visible) then
            if ( ((tP.Y<bP.Y)                                  ) or
                 ((tP.Y=bP.Y)and(tP.X<bP.X)                    ) or
                 ((tP.Y=bP.Y)and(tP.X=bP.X)and(t.Index<b.Index)) ) or (b=nil) then
              begin
                b  := Item[i];
                bP := Item[i].Offset;
              end;
        end;

      Result:=b;
    end;
end;

function     TdeSchema.NextBottom(anItem:TdeSchemaObject):TdeSchemaObject;
var i         : Integer;
    aP,bP,tP  : TPoint2D;
    a,b,t     : TdeSchemaObject;
begin
  if anItem=nil then
    begin
      b := Item[0];
      bP:= b.Offset;

      for i:=1 to ItemsCount-1 do
        begin
          t := Item[i];
          tP:= t.Offset;
          if   ( ((tP.Y<bP.Y)                                  ) or
                 ((tP.Y=bP.Y)and(tP.X<bP.X)                    ) or
                 ((tP.Y=bP.Y)and(tP.X=bP.X)and(t.Index<b.Index)) )
                 and (t.Level=0) and (t.Visible) then
              begin
                b  := Item[i];
                bP := Item[i].Offset;
              end;
        end;

      Result:=b;
    end
  else
    begin
      a := anItem;
      aP:= a.Offset;
      b := nil;
      bP:= aP;

      for i:=0 to ItemsCount-1 do
        begin
          t := Item[i];
          tP:= t.Offset;
          if   ( ((tP.Y<aP.Y)                                  ) or
                 ((tP.Y=aP.Y)and(tP.X<aP.X)                    ) or
                 ((tP.Y=aP.Y)and(tP.X=aP.X)and(t.Index<a.Index)) )
                 and (t.Level=0) and (t.Visible) then
            if ( ((tP.Y>bP.Y)                                  ) or
                 ((tP.Y=bP.Y)and(tP.X>bP.X)                    ) or
                 ((tP.Y=bP.Y)and(tP.X=bP.X)and(t.Index>b.Index)) ) or (b=nil) then
              begin
                b  := Item[i];
                bP := Item[i].Offset;
              end;
        end;

      Result:=b;
    end;
end;

function     TdeSchema.getBounds:TRect2D;
begin
  if FChanged then
    FBounds := recalcBounds;
  Result := FBounds;  
end;

function     TdeSchema.getItemsCount:integer;
var
  anItem : TdeSchemaObject;
begin
  Result := 0;
  anItem := First;
  while anItem <> nil do begin
    inc(Result);
    anItem := Next(anItem);
  end;
end;

function     TdeSchema.getItem(index:integer):TdeSchemaObject;
var
  i      : integer;
begin
  i := 0;
  Result := First;
  while (Result <> nil)and(i < index) do begin
    Result := Next(Result);
    inc(i);
  end;
end;

function     TdeSchema.getRootCount:integer;
begin
  Result := FRootItems.Count;
end;

function     TdeSchema.getRootItem(index:integer):TdeSchemaObject;
begin
  Result := FRootItems[index];
end;

procedure    TdeSchema.RootAdd(anItem:TdeSchemaObject);
begin
  FRootItems.Add(anItem);
  anItem.Update;
end;

procedure    TdeSchema.RootInsert(Index:integer;anItem:TdeSchemaObject);
begin
  FRootItems.Insert(Index,anItem);
  anItem.Update;
end;

procedure    TdeSchema.RootRemove(anItem:TdeSchemaObject);
begin
  FRootItems.Remove(anItem);
end;

function     TdeSchema.getLayersCount:integer;
begin
  Result := FLayers.Count;
end;

function     TdeSchema.getLayer(index:integer):TdeSchemaLayer;
begin
  if (0<=index) and (index<FLayers.Count) then
    Result := TdeSchemaLayer(FLayers.Objects[index])
  else
    Result:=nil;
end;

function     TdeSchema.getLayerByName(aName:string):TdeSchemaLayer;
var
  i : integer;
begin
  if (aName = '') then
    Result := nil
  else begin
    i := FLayers.IndexOf(aName);
    if (i < 0) then begin
      i := FLayers.AddObject(aName,TdeSchemaLayer.Create(Self,aName));
      NotifyNewLayer(Layer[i]);
    end;
    Result := Layer[i];
  end;
end;

function     TdeSchema.getShortcutsCount:integer;
begin
  Result := FShortcuts.Count;
end;

function     TdeSchema.getShortcut(index:integer):TdeSchemaShortcut;
begin
  Result := TdeSchemaShortcut(FShortcuts[index]);
end;

procedure    TdeSchema.setShortcutImages(anImageList:TImageList);
var
  i : integer;
begin
  FSCImages := anImageList;
  for i := 0 to ShortcutsCount - 1 do
    Shortcut[i].UpdateBitmap;
end;

procedure    TdeSchema.setShortcutFont(aFont:TFont);
begin
  FSCFont := aFont;
end;

procedure    TdeSchema.getObjectShortcuts(anObject:TdeSchemaObject;
                                     ShortcutsList:TList);
var
  i : integer;
begin
  ShortcutsList.Clear;
  for i := 0 to ShortcutsCount - 1 do
  begin
    if (Shortcut[i].LinkedObject = anObject) then
      ShortcutsList.Add(Shortcut[i]);
  end;
end;

procedure    TdeSchema.DeleteObjectShortcuts(anObject:TdeSchemaObject);
var
  L : TList;
begin
  Lock;
  try
    L := TList.Create;
    try
      getObjectShortcuts(anObject,L);
      while L.Count>0 do begin
        TdeSchemaShortcut(L[L.Count-1]).Delete;
        L.Delete(L.Count-1);
      end;
    finally
      L.Free;
    end;
  finally
    unLock;
  end;
end;

procedure    TdeSchema.MoveObjectShortcutsBy(anObject:TdeSchemaObject;
                                              aVector:TPoint2D);
var
  L  : TList;
  SC : TdeSchemaShortcut;
begin
  Lock;
  try
    L := TList.Create;
    try
      getObjectShortcuts(anObject,L);
      while L.Count>0 do begin
        SC := TdeSchemaShortcut(L[L.Count-1]);
        SC.Offset := OffsetPoint2D(SC.Offset,aVector.X,aVector.Y);
        L.Delete(L.Count-1);
      end;
    finally
      L.Free;
    end;
  finally
    unLock;
  end;
end;

procedure    TdeSchema.ItemChanging(anItem:TdeSchemaObject);
begin
  NotifyItemChanging(anItem);
end;

procedure    TdeSchema.ItemChanged(anItem:TdeSchemaObject;
                             ChangedProps:TdeObjectChangedProps);
begin
  NotifyItemChanged(anItem,ChangedProps);
  Changed;
end;

procedure    TdeSchema.Changed;
begin
  if FDeleting then exit;
  FChanged := (FLockCount > 0);
  if not FChanged then begin
    FBounds := recalcBounds;
    NotifyChanged;
  end;
end;

procedure    TdeSchema.NotifyNewLayer(aLayer:TdeSchemaLayer);
begin
end;

procedure    TdeSchema.NotifyDeleteLayer(aLayer:TdeSchemaLayer);
begin
end;

procedure    TdeSchema.NotifyLayerChanged(aLayer:TdeSchemaLayer);
begin
end;

procedure    TdeSchema.NotifyNewShortcut(aShortcut:TdeSchemaShortcut);
begin
end;

procedure    TdeSchema.NotifyDeleteShortcut(aShortcut:TdeSchemaShortcut);
begin
end;

procedure    TdeSchema.NotifyShortcutChanged(aShortcut:TdeSchemaShortcut);
begin
end;

procedure    TdeSchema.NotifyNewItem(anItem:TdeSchemaObject);
begin
end;

procedure    TdeSchema.NotifyDeleteItem(anItem:TdeSchemaObject);
begin
end;

procedure    TdeSchema.NotifyItemChanging(anItem:TdeSchemaObject);
begin
end;

procedure    TdeSchema.NotifyItemChanged(anItem:TdeSchemaObject;
                                   ChangedProps:TdeObjectChangedProps);
begin
end;

procedure    TdeSchema.NotifyChanged;
begin
end;

procedure    TdeSchema.Clear;
begin
  FClearing   := true;
  Lock;
  try
    while (RootCount > 0) do
      RootItem[RootCount-1].Delete;
    while (ShortcutsCount > 0) do
      Shortcut[ShortcutsCount-1].Delete;
  finally
    unLock;
  end;
  FClearing   := false;
end;

function     TdeSchema.IndexOf(anItem:TdeSchemaObject):integer;
var
  _Item : TdeSchemaObject;
begin
  Result := 0;
  _Item := First;
  while (_Item <> nil)and(_Item <> anItem) do begin
    _Item := Next(_Item);
    inc(Result);
  end;
  if (_Item = nil) then
    Result :=-1;
end;

procedure    TdeSchema.ChangeRootItemIndex(Index,aValue:integer);
begin
  FRootItems.Move(Index,aValue);
  Changed;
end;

function     TdeSchema.RootItemIndex(anItem:TdeSchemaObject):integer;
begin
  Result := FRootItems.IndexOf(anItem);
end;

function     TdeSchema.New(aClassID:integer;aParent:TdeSchemaGroup;aLayer:string=''):TdeSchemaObject;
begin
  Result := deCreateObject(aClassID,Self);
  if (Result = nil) then exit;
  Result.Lock;
  try
    if aParent = nil then
      RootAdd(Result)
    else
      Result.Parent := aParent;
    Result.Layer  := LayerByName[aLayer];
    Result.FChanged := false;
    NotifyNewItem(Result);
  finally
    Result.unLock;
  end;
  Changed;
end;

function     TdeSchema.New(aClass:TdeSchemaObjectClass;aParent:TdeSchemaGroup;aLayer:string=''):TdeSchemaObject;
begin
  Result := aClass.Create(Self);
  if (Result = nil) then exit;
  Result.Lock;
  try
    if aParent = nil then
      RootAdd(Result)
    else
      Result.Parent := aParent;
    Result.Layer  := LayerByName[aLayer];
    Result.FChanged := false;
    NotifyNewItem(Result);
  finally
    Result.unLock;
  end;
  Changed;
end;

function     TdeSchema.NewGroup(aParent:TdeSchemaGroup;aLayer:string=''):TdeSchemaGroup;
begin
  Result := TdeSchemaGroup.Create(Self);
  Result.Lock;
  try
    if aParent = nil then
      RootAdd(Result)
    else
      Result.Parent := aParent;
    Result.Layer  := LayerByName[aLayer];
    Result.FChanged := false;
    NotifyNewItem(Result);
  finally
    Result.unLock;
  end;
  Changed;
end;

function     TdeSchema.NewShortcut:TdeSchemaShortcut;
begin
  Result := TdeSchemaShortcut.Create(Self);
  try
    NotifyNewShortcut(Result);
  finally
  end;
  Changed;
end;

procedure    TdeSchema.Delete(anItem:TdeSchemaObject);
begin
  if not(anItem.Deleting) then
    anItem.Delete
  else begin
    //удалить объект из списка корневых
    RootRemove(anItem);

    //удалить ссылки на объект
    DeleteObjectShortcuts(anItem);

    //известить об удалении объекта
    if not(FDeleting) then
      NotifyDeleteItem(anItem);
  end;
  Changed;
end;

function     TdeSchema.LayerIndex(aLayerName:string):integer;
begin
  Result := FLayers.IndexOf(aLayerName);
end;

function     TdeSchema.ShortcutIndex(aShortcut:TdeSchemaShortcut):integer;
begin
  Result := FShortcuts.IndexOf(aShortcut);
end;

procedure    TdeSchema.PaintTo(Canvas:TCanvas;StoC:TMatrix2D;WidthLine:Integer=1);
var
  anItem : TdeSchemaObject;
  i      : integer;
begin
  anItem := First;
  while anItem <> nil do
  begin
    if (anItem.IGroup = nil)
       and anItem.Visible
    then
      anItem.PaintTo(Canvas,StoC,WidthLine);
    anItem := Next(anItem);
  end;
  for i := 0 to ShortCutsCount-1 do
  begin
    if (Shortcut[i].LinkedObject <> nil)
       and(Shortcut[i].LinkedObject.Visible)
    then
      Shortcut[i].PaintTo(Canvas,StoC);
  end;
end;

procedure    TdeSchema.PrepareInfo;
var R      : TRect2D;
    anItem : TdeSimpleCurve;
    xx,yy  : single;
    P      : TPoint2D;
    DP     : array[0..255] of TPoint2D;

begin
  R:=getBounds;

    anItem := TdeSimpleCurve(New(TdeSimpleCurve,nil,''));

    anItem.Lock;
    try
      anItem.Color       := clGray;
      anItem.Transparent := true;
      anItem.BorderColor:=clGreen;
      anItem.BorderWidth:=8;

      xx:=abs(R.Right-R.Left)/2;
      yy:=abs(R.Top-R.bottom)/2;
      P := Point2D((R.Right+R.Left)/2,(R.Top+R.bottom)/2);

      DP[0] := Point2D(P.X-xx,P.Y+yy);
      DP[0].W := PT_MOVETO;
      DP[1] := Point2D(P.X+xx,P.Y+yy);
      DP[1].W := PT_LINETO;
      DP[2] := Point2D(P.X+xx,P.Y-yy);
      DP[2].W := PT_LINETO;
      DP[3] := Point2D(P.X-xx,P.Y-yy);
      DP[3].W := PT_LINETO or PT_CLOSEFIGURE;
      anItem.setDrawPath(DP,4,nil);
    finally
      anItem.unLock;
    end;

end;

procedure    TdeSchema.AddFrom(aSchema:TdeSchema;
                              var Rect:TRect2D;
                             ScaleMode:TdeAFScaleMode);
var
  NRect           : TRect2D;
  NSize,ASize     : TPoint2D;
  NCenter,ACenter : TPoint2D;
  Kx,Ky           : double;
  Conv            : TMatrix2D;
  i               : integer;
  aLayer          : TdeSchemaLayer;
  anItem          : TdeSchemaObject;
  aMemo           : TdeSchemaObjectMemento;
begin
  if (aSchema = nil) or (aSchema.RootCount = 0) then exit;

  //рассчитать матрицу преобразования для новых объектов
  M2D_Identity(Conv);
  if IsEmptyRect2D(Rect) then
    Rect := aSchema.Bounds
  else begin
    //координаты центра Rect в пикселях
    NRect := aSchema.Bounds;
    NSize   := RectSizes(NRect);
    NCenter := RectCenter(NRect);
    ASize   := RectSizes(Rect);
    ACenter := RectCenter(Rect);
    //коэффициенты масштабирования новых объектов
    if abs(NSize.X) < DE_EPSILON then
      Kx := 1
    else
      Kx := ASize.X/NSize.X;
    if abs(NSize.Y) < DE_EPSILON then
      Ky := 1
    else
      Ky := ASize.Y/NSize.Y;

    if (ScaleMode = smAutoSize) then begin
      Kx := 1;
      Ky := 1;
    end
    else if (ScaleMode = smProportional) then begin
      if Kx > Ky then Kx := Ky else Ky := Kx;
    end;
    Rect := Rect2D(ACenter.X-Kx*ASize.X,ACenter.Y+Ky*ASize.Y,
                   ACenter.X+Kx*ASize.X,ACenter.Y-Ky*ASize.Y);

    Conv := M2D_Shift(Conv,-NCenter.X,-NCenter.Y);
    Conv := M2D_Scale(Conv,Kx,Ky);
    Conv := M2D_Shift(Conv,ACenter.X,ACenter.Y);
  end;
  //объединить схемы
  for i := 0 to aSchema.LayersCount-1 do
  begin
    aLayer := LayerByName[aSchema.Layer[i].Name];
    aLayer.ReadOnly := aSchema.Layer[i].ReadOnly;
  end;
  for i := 0 to aSchema.RootCount-1 do
  begin
    aMemo := aSchema.RootItem[i].CreateMemento;
    try
      anItem := aMemo.CloneObject(Self,nil,aSchema.RootItem[i].LayerName);
      anItem.ApplyConvertion(Conv);
      
    finally
      aMemo.Free;
    end;
  end;
end;



{TdeMemoryStream}
destructor TdeMemoryStream.Destroy;
begin
  inherited Destroy;
end;

function   TdeMemoryStream.Write(const Buffer; Count: Longint): Longint;
var
  Pos: Longint;
begin
  if (Position >= 0) and (Count >= 0) then
  begin
    Pos := Position + Count;
    if Pos > Capacity then
      Capacity := (5*Pos div 4);
    Result := inherited Write(Buffer,Count);
  end
  else
    Result := 0;
end;




{TdeSchemaStorage}
constructor TdeSchemaStorage.Create;
begin
  inherited Create;
end;

destructor  TdeSchemaStorage.Destroy;
begin
  inherited Destroy;
end;

procedure   TdeSchemaStorage.DoNotifyProgress(Progress:integer);
begin
end;

procedure   TdeSchemaStorage.Load(aShema:TdeSchema);
begin
end;

procedure   TdeSchemaStorage.Save(aShema:TdeSchema);
begin
end;



type
  TdeCustomCurveData = packed record
    Header         : TdeObjectDataHeader;
    cBorderColor   : TColor;
    iBorderStyle   : TPenStyle;
    iBorderWidth   : integer;
    cFillColor     : TColor;
    bTransparent   : boolean;
    cbPoints       : integer;
    dPoints        : array[0..0] of TPoint2D;
  end;
  PdeCustomCurveData  = ^TdeCustomCurveData;


{TdeCustomCurve}
constructor  TdeCustomCurve.Create(anOwner:TdeSchema);
begin
  inherited Create(anOwner);
  FDrawPath := nil;
  FBrdColor := clBlack;
  FBrdStyle := psSolid;
  FBrdWidth := 1;
  FColor    := clWhite;
  FTransparent := false;
end;

destructor   TdeCustomCurve.Destroy;
begin
  FDeleting := true;
  FDrawPath := nil;
  inherited Destroy;
end;

function     TdeCustomCurve.RecalcBounds:TRect2D;
var
  i,j : integer;
  t   : double;
begin
  Result := Rect2D(0,0,0,0);
  i := 0;
  while i < PointsCount do begin
    case PntType[i] of
      PT_MOVETO   : begin
        if i = 0 then begin
          Result.TopLeft     := Point[i];
          Result.BottomRight := Result.TopLeft;
        end
        else begin
          Result := ExpandRect2D(Result,Point[i]);
        end;
      end;
      PT_LINETO   : begin
        Result := ExpandRect2D(Result,Point[i]);
      end;
      PT_BEZIERTO : begin
        t := 1/MAX_BEZIER_POINTS;
        for j:=1 to MAX_BEZIER_POINTS do
          Result := ExpandRect2D(Result,Bezier2D(Point[i-1],
                                                 Point[i],
                                                 Point[i+1],
                                                 Point[i+2],
                                                 t*j));
        inc(i,2);
      end;
    end;
    inc(i);
  end;
end;

{
function     TdeCustomCurve.getAsString:string;
var
  n,s : string;
  i   : integer;
begin
  with tStringList.Create do try
    Text := inherited getAsString;
    Values['Color'] := IntToStr(integer(FColor));
    Values['BorderColor'] := IntToStr(integer(FBrdColor));
    Values['BorderStyle'] := IntToStr(integer(FBrdStyle));
    Values['BorderWidth'] := IntToStr(FBrdWidth);
    Values['Transparent'] := IntToStr(ord(FTransparent));
    n := '';
    for i := 0 to PointsCount-1 do begin
      Str(FDrawPath[i].X:10:5,s);
      n := n+','+trim(s);
      Str(FDrawPath[i].Y:10:5,s);
      n := n+','+trim(s);
      Str(trunc(FDrawPath[i].W):5,s);
      n := n+','+trim(s);
    end;
    Values['Points.Count']:=IntToStr(PointsCount);
    Values['Points']:=copy(n,2,length(n)-1);
    Result := Text;
  finally
    Free;
  end;
end;

procedure    TdeCustomCurve.setAsString(aValue:string);
var
  n,s   : string;
  i,c   : integer;
  k,l,j : integer;
  P     : TPoint2D;
  Pnts  : array of TPoint2D;
begin
  Changing;
  Lock;
  inherited setAsString(aValue);
  with TStringList.Create do try
    Text := aValue;
    FColor       := TColor(StrToIntDef(Values['Color'],0));
    FBrdColor    := TColor(StrToIntDef(Values['BorderColor'],0));
    FBrdStyle    := TPenStyle(StrToIntDef(Values['BorderStyle'],0));
    FBrdWidth    := StrToIntDef(Values['BorderWidth'],1);
    FTransparent := (StrToIntDef(Values['Transparent'],1)=1);
    i := StrToIntDef(Values['Points.Count'],0);
    setLength(Pnts,i);
    if i > 0 then begin
      i := 0;
      s := Values['Points'];
      l := Length(s);
      k := 1;
      while (k <= l) do begin
        j := k;
        while (j < l)and(s[j] <> ',')do inc(j);
        n := copy(s,k,j-k);
        k := j+1;
        Val(trim(n),P.X,c);
        j := k;
        while (j < l)and(s[j] <> ',')do inc(j);
        n := copy(s,k,j-k);
        k := j+1;
        Val(trim(n),P.Y,c);
        j := k;
        while (j <= l)and(s[j] <> ',')do inc(j);
        n := copy(s,k,j-k);
        k := j+1;
        Val(trim(n),P.W,c);
        Pnts[i] := P;
        inc(i);
      end;
    end;
    setDrawPath(Pnts,i);
    Pnts := nil;
  finally
    Free;
  end;
  unLock;
  Changed([ocpParams]);
end;
}

function     TdeCustomCurve.getData(pData:pointer;var iSize:integer):boolean;
var
  nSize,inhSize : integer;
  inhData       : dword;
  pInhData      : pointer;
begin
  inhSize := 0;
  inherited getData(nil,inhSize);
  inhData := sizeOf(TdeCustomCurveData)+PointsCount*sizeOf(TPoint2D);
  nSize := integer(inhData)+inhSize;
  Result := (nSize<=iSize);
  if Result then begin
    with PdeCustomCurveData(pData)^ do begin
      Header.nSize := nSize;
      Header.pInheritedData := inhData;
      Header.pInheritedSize := inhSize;
      pInhData := pointer(dword(pData)+inhData);
      inherited getData(pInhData,inhSize);
      cBorderColor   := BorderColor;
      iBorderStyle   := BorderStyle;
      iBorderWidth   := BorderWidth;
      cFillColor     := Color;
      bTransparent   := Transparent;
      cbPoints       := PointsCount;
      {
      system.Move(FDrawPath[0],dPoints[0],cbPoints*sizeOf(TPoint2D));
      }
      getDrawPath(dPoints,cbPoints);
      {}
    end;
  end;
  iSize := nSize;
end;

procedure    TdeCustomCurve.setData(pData:pointer;iSize:integer);
var
  inhSize   : integer;
  inhData   : dword;
  pInhData  : pointer;
begin
  if iSize < sizeOf(TdeCustomCurveData) then exit;
  Lock;
  Changing;
  with PdeCustomCurveData(pData)^ do begin
    inhSize := Header.pInheritedSize;
    if (inhSize > 0) then begin
      inhData := Header.pInheritedData;
      pInhData := pointer(dword(pData)+inhData);
      inherited setData(pInhData,inhSize);
    end;
    BorderColor   := cBorderColor;
    BorderStyle   := iBorderStyle;
    BorderWidth   := iBorderWidth;
    Color         := cFillColor;
    Transparent   := bTransparent;
    setDrawPath(dPoints,cbPoints);
  end;
  Changed([ocpParams]);
  unLock;
end;

function     TdeCustomCurve.getPT(index:integer):integer;
begin
  Result := trunc(FDrawPath[index].W)and not(PT_CLOSEFIGURE);
end;

function     TdeCustomCurve.PointsCount:integer;
begin
  Result := High(FDrawPath)+1;
end;

function     TdeCustomCurve.getPoint(index:integer):TPoint2D;
begin
  Result := Point2D(FDrawPath[index].X,FDrawPath[index].Y);
end;

function     TdeCustomCurve.CreateMemento:TdeSchemaObjectMemento;
begin
  Result := TdeCustomCurveMemento.Create(Self);
end;

function     TdeCustomCurve.getDrawPath(var Points:array of TPoint2D;
                                           var Cnt:integer;
                                             pConv:pMatrix2D = nil):boolean;
var
  resCount : integer;
  i        : integer;
begin
  resCount := PointsCount;
  Result := (resCount<=Cnt);
  if Result then begin
    if (pConv = nil) or M2D_IsIdentity(pConv^) then
      System.Move(FDrawPath[0],Points[0],resCount*sizeOf(TPoint2D))
    else begin
      for i := 0 to resCount-1 do begin
        Points[i]   := MultVM2D(FDrawPath[i],pConv^);
        Points[i].W := FDrawPath[i].W;
      end;
    end;
  end;
  Cnt := resCount;
end;

procedure    TdeCustomCurve.setDrawPath(const Points:array of TPoint2D;
                                                 Cnt:integer;
                                               pConv:pMatrix2D = nil);
var
  i : integer;
begin
  Changing;
  Lock;
  try
    setLength(FDrawPath,Cnt);
    if (pConv = nil) or M2D_IsIdentity(pConv^) then
      System.Move(Points[0],FDrawPath[0],Cnt*sizeOf(TPoint2D))
    else begin
      for i := 0 to Cnt-1 do begin
        FDrawPath[i] := MultVM2D(Points[i],pConv^);
        FDrawPath[i].W := Points[i].W;
      end;
    end;
  finally
    UpdateBounds;
    unLock;
    Changed([]);
  end;
end;

procedure    TdeCustomCurve.setBrdColor(aValue:TColor);
begin
  if aValue = BorderColor then exit;
  Changing;
  FBrdColor := aValue;
  Changed([ocpParams]);
end;

procedure    TdeCustomCurve.setBrdStyle(aValue:TPenStyle);
begin
  if aValue = BorderStyle then exit;
  Changing;
  FBrdStyle := aValue;
  Changed([ocpParams]);
end;

procedure    TdeCustomCurve.setBrdWidth(aValue:integer);
begin
  if aValue = BorderWidth then exit;
  Changing;
  FBrdWidth := aValue;
  Changed([ocpParams]);
end;

procedure    TdeCustomCurve.setColor(aValue:TColor);
begin
  if aValue = Color then exit;
  Changing;
  FColor := aValue;
  Changed([ocpParams]);
end;

function     TdeCustomCurve.getTransparent:boolean;
begin
  Result := FTransparent;
end;

procedure    TdeCustomCurve.setTransparent(aValue:boolean);
begin
  if aValue = Transparent then exit;
  Changing;
  FTransparent := aValue;
  Changed([ocpParams]);
end;

procedure    TdeCustomCurve.IntPaint(Canvas:TCanvas;
                              const iPoints:array of TPoint);
var
  i   : integer;
  Cnt : integer;
  MT  : TPoint;
  Cl  : boolean;
begin
  Cnt := High(iPoints)+1;
  if Cnt <= 0 then exit;
  i := 0;
  while (i < Cnt) do begin
    Cl := (trunc(FDrawPath[i].W)and PT_CLOSEFIGURE) = PT_CLOSEFIGURE;
    case PntType[i] of
      PT_MOVETO : begin
        MT := iPoints[i];
        Canvas.MoveTo(iPoints[i].X,iPoints[i].Y);
      end;
      PT_LINETO : Canvas.LineTo(iPoints[i].X,iPoints[i].Y);
      PT_BEZIERTO : begin
        PolyBezierTo(Canvas.Handle,iPoints[i],3);
        inc(i,2);
        Cl := (trunc(FDrawPath[i].W)and PT_CLOSEFIGURE) = PT_CLOSEFIGURE;
      end;
    end;
    if Cl then
      Canvas.LineTo(MT.X,MT.Y);
    inc(i);
  end;
end;

procedure    TdeCustomCurve.PaintTo(Canvas:TCanvas;StoC:TMatrix2D;WidthLine:Integer=1);
var
  i       : integer;
  Drawn   : boolean;
  //VR      : TRect;
  iPoints : array of TPoint;
  Conv    : TMatrix2D;

  lb : TLogBrush;
  OldPen,Pen: HPen;
begin
  {VR := getViewRect(StoC);
  if (abs(VR.Right-VR.Left)<=10)and(abs(VR.Bottom-VR.Top)<=10) then exit;
  if not IntersectRect(VR,VR,Canvas.ClipRect) then exit;}
  Conv := MultMM2D(FOtoSConv,StoC);
  setLength(iPoints,PointsCount);
  for i:=0 to PointsCount-1 do
    iPoints[i] := P2DToP(MultVM2D(Point[i],Conv));
  Drawn := false;
  Canvas.Pen.Color   := BorderColor;
  Canvas.Pen.Style   := BorderStyle;
  Canvas.Pen.Width   := BorderWidth*WidthLine;
  Canvas.Brush.Color := Color;                      
  Canvas.Brush.Style := bsSolid;

  if not Transparent then
    begin
      BeginPath(Canvas.Handle);
      IntPaint(Canvas,iPoints);
      EndPath(Canvas.Handle);
      FillPath(Canvas.Handle);
      Drawn := true;
    end;

  if not(Drawn) or (BorderColor <> Color) or (BorderStyle <> psSolid) then
    begin
      OldPen:=Canvas.Pen.Handle;

      if (BorderStyle <> psSolid) then
      begin
        if (BorderWidth*WidthLine>1) then
          begin
            lb.lbStyle := BS_SOLID;
            lb.lbHatch := 0;
            lb.lbColor := BorderColor;
            Pen:= ExtCreatePen(PS_GEOMETRIC or Integer(BorderStyle) or PS_JOIN_ROUND,
                               BorderWidth*WidthLine, lb, 0, nil);
            Canvas.Pen.Handle :=Pen;
          end
        else
          begin
            Canvas.Brush.Color:=clWhite;
          end;
      end;
      
      BeginPath(Canvas.Handle);
      IntPaint(Canvas,iPoints);
      EndPath(Canvas.Handle);
      StrokePath(Canvas.Handle);

      if (BorderStyle <> psSolid)and(BorderWidth*WidthLine>1) then
        begin
          Canvas.Pen.Handle := OldPen;
          DeleteObject(Pen);
        end;
    end;
  iPoints := nil;
end;

procedure    TdeCustomCurve.PaintFrame(Canvas:TCanvas;StoC:TMatrix2D);
var
  //VR      : TRect;
  i       : integer;
  iPoints : array of TPoint;
  Conv    : TMatrix2D;
begin
  {VR := getViewRect(StoC);
  if (abs(VR.Right-VR.Left)<=10)and(abs(VR.Bottom-VR.Top)<=10) then exit;
  if not IntersectRect(VR,VR,Canvas.ClipRect) then exit;}
  Conv := MultMM2D(FOtoSConv,StoC);
  setLength(iPoints,PointsCount);
  for i:=0 to PointsCount-1 do
    iPoints[i] := P2DToP(MultVM2D(Point[i],Conv));
  IntPaint(Canvas,iPoints);
  iPoints := nil;
end;

function     TdeCustomCurve.Contain(  Canvas:TCanvas;
                                       StoC:TMatrix2D;
                                        X,Y:integer):boolean;
var
  aRgn    : hRGN;
  i       : integer;
  iPoints : array of TPoint;
begin
  Canvas.Pen.Width := 5*cPointSize;
  setLength(iPoints,PointsCount);
  for i:=0 to PointsCount-1 do
    iPoints[i] := P2DToP(MultVM2D(OtoS(Point[i]),StoC));
  BeginPath(Canvas.Handle);
  IntPaint(Canvas,iPoints);
  EndPath(Canvas.Handle);
  if Transparent then
    WidenPath(Canvas.Handle);
  aRgn := PathToRegion(Canvas.Handle);
  Result := PtInRegion(aRgn,X,Y);
  DeleteObject(aRgn);
  iPoints := nil;
end;

procedure    TdeCustomCurve.DropConvertion;
var
  i : integer;
  W : single;
begin
  for i:=0 to high(FDrawPath) do begin
    W := FDrawPath[i].W;
    FDrawPath[i] := OtoS(FDrawPath[i]);
    FDrawPath[i].W := W;
  end;
  inherited DropConvertion;
end;



{TdeComplexCurve}
class function TdeComplexCurve.ClassID:integer;
begin
  Result := 0;
end;

constructor  TdeComplexCurve.Create(anOwner:TdeSchema);
begin
  inherited Create(anOwner);
end;

destructor   TdeComplexCurve.Destroy;
begin
  inherited Destroy;
end;

procedure    TdeComplexCurve.Split(newObjects:tList; SetIndex:Boolean = True);
          //в параметре newObjects возвращается
          //список созданных объектов
var
  i, Start  : integer;
  Buffer    : array of TPoint2D;
  BufCount  : integer;
  Points    : array of TPoint2D;
  Cnt       : integer;
  newObject : TdeSimpleCurve;
begin
  if newObjects <> nil  then newObjects.Clear;
  Buffer   := nil;
  BufCount := 0;
  getDrawPath(Buffer,BufCount);
  setLength(Buffer,BufCount);
  getDrawPath(Buffer,BufCount);
  i := 1;
  Start := 0;
  while (i <= BufCount) do begin
    if (i = BufCount)
       or ((trunc(Buffer[i].W) and not PT_CLOSEFIGURE) = PT_MOVETO)
    then begin
      Cnt := i-Start;
      setLength(Points,Cnt);
      system.Move(Buffer[Start],Points[0],Cnt*sizeOf(TPoint2D));
      newObject := TdeSimpleCurve(FOwner.New(TdeSimpleCurve,Parent,LayerName));
      if newObject <> nil then begin
        newObject.Lock;
        try
          newObject.setDrawPath(Points,Cnt);
          newObject.Convertion := FConvertion;
          newObject.Color := Color;
          newObject.BorderColor := BorderColor;
          newObject.BorderStyle := BorderStyle;
          newObject.BorderWidth := BorderWidth;
          newObject.Transparent := Transparent;
          if SetIndex then
          newObject.Index := Index;
        finally
          newObject.unLock;
        end;
        if (newObjects<>nil) then
          newObjects.Add(newObject);
      end;
      Start := i;
    end;
    inc(i);
  end;
end;

procedure    TdeComplexCurve.CombineWith(aCurve:TdeCustomCurve);
var
  Buffer      : array of TPoint2D;
  BufCount    : integer;
  Points      : array of TPoint2D;
  Cnt         : integer;
begin
  if (aCurve = nil) or (aCurve.PointsCount = 0) then exit;
  Buffer := nil;
  BufCount := 0;
  getDrawPath(Buffer,BufCount);
  Points := nil;
  Cnt    := 0;
  aCurve.getDrawPath(Points,Cnt);
  setLength(Points,Cnt);
  aCurve.getDrawPath(Points,Cnt,@aCurve.FOtoSConv);
  setLength(Buffer,BufCount+Cnt);
  getDrawPath(Buffer,BufCount,@FOtoSConv);
  system.Move(Points[0],Buffer[BufCount],Cnt*sizeOf(TPoint2D));
  setDrawPath(Buffer,BufCount + Cnt,@FROToSConv);
end;


type
  TdeSimpleCurveMemento = class(TdeCustomCurveMemento)
  protected
    MMClosed   : Boolean;
    MMCycled   : Boolean;
    Vertexes   : array of TdeDPVertex;
    constructor Create(anOrigin:TdeSimpleCurve);
  public
    procedure   Restore(anObject:TdeSchemaObject);override;
    destructor  Destroy;override;
  end;

{TdeSimpleCurveMemento}
constructor TdeSimpleCurveMemento.Create(anOrigin:TdeSimpleCurve);
var
  Cnt : integer;
begin
  inherited Create(anOrigin);
  Cnt := high(anOrigin.FVertexes);
  if (Cnt > 0) then begin
    setLength(Vertexes,Cnt);
    system.Move(anOrigin.FVertexes[0],Vertexes[0],Cnt*sizeOf(TdeDPVertex));
    MMClosed := anOrigin.FClosed;
    MMCycled := anOrigin.FCycled;
  end
  else
    Vertexes := nil;
end;

procedure   TdeSimpleCurveMemento.Restore(anObject:TdeSchemaObject);
var
  Cnt : integer;
begin
  anObject.Lock;
  with TdeSimpleCurve(anObject) do
    begin
      Cnt := high(Vertexes);
      if (Cnt > 0) then
        begin
          setLength(FVertexes,Cnt);
          system.Move(Vertexes[0],FVertexes[0],Cnt*sizeOf(TdeDPVertex));
        end
      else
        FVertexes := nil;
    end;

  inherited Restore(anObject);

  with TdeSimpleCurve(anObject) do
    begin
      FClosed:=MMClosed;
      FCycled:=MMCycled;
    end;

  anObject.unLock;
end;

destructor  TdeSimpleCurveMemento.Destroy;
begin
  Vertexes := nil;
  inherited Destroy;
end;





{TdeSimpleCurve}
class function TdeSimpleCurve.ClassID:integer;
begin
  Result := 1;
end;

constructor    TdeSimpleCurve.Create(anOwner:TdeSchema);
begin
  inherited Create(anOwner);
  FActiveVtx :=-1;
  FUpdating  := 0;
  FVertexes  := nil;
end;

destructor     TdeSimpleCurve.Destroy;
begin
  FDeleting := true;
  FVertexes  := nil;
  inherited Destroy;
end;

function       TdeSimpleCurve.nextVertex(vtxIndex:integer):integer;
begin
  if (vtxIndex < 0)or(vtxIndex >= VertexesCount) then
    Result :=-1
  else begin
    Result := vtxIndex+1;
    if (Result = VertexesCount) then begin
      if Closed then Result := 0
                else Result :=-1;
    end;
  end;
end;

procedure      TdeSimpleCurve.setPoint(index:integer;aValue:TPoint2D);
var
  W : double;
begin
  W := FDrawPath[index].W;
  FDrawPath[index] := Point2D(aValue.X,aValue.Y);
  FDrawPath[index].W := W;
end;

procedure      TdeSimpleCurve.setPT(index:integer;aValue:integer);
begin
  FDrawPath[index].W := (aValue and not(PT_CLOSEFIGURE));
end;

procedure      TdeSimpleCurve.DelPoints(index,count:integer);
var
  cnt,last,i : integer;
begin
  last := index+count-1;
  cnt  := PointsCount;
  if (index >= cnt)or(last < 0) then exit;
  if (index < 0) then index:=0;
  if (last >= cnt) then last := cnt-1;
  count := last-index+1;
  if (index = 0) then begin
    if FCycled and Closed then begin
      setPoint(PointsCount-1,Point[count]);
      setPT(count,PT_MOVETO);
    end;
  end;
  for i := index to cnt-count-1 do
    FDrawPath[i]:=FDrawPath[i+Count];
  setLength(FDrawPath,cnt-count);
end;

procedure      TdeSimpleCurve.AddPoints(index,count:integer);
var
  cnt,i : integer;
begin
  cnt  := PointsCount;
  if (index >= 0)and(index <= cnt) then begin
    setLength(FDrawPath,cnt+count);
    for i := High(FDrawPath) downto index+count do
      FDrawPath[i]:=FDrawPath[i-count];
  end
  else
    raise EVertexException.Create(Format('Invalid point (%d)',[index]));
end;

function       TdeSimpleCurve.getClosed:boolean;
begin
  Result := FClosed;
end;

procedure      TdeSimpleCurve.setClosed(aValue:boolean);
begin
  if Closed=aValue then exit;
  Changing;
  Lock;
  FClosed := aValue;
  if FCycled
  then begin
    if Closed
    then begin
      FVertexes[0].LCP := FVertexes[High(FVertexes)].LCP;
    end
    else begin
      FVertexes[High(FVertexes)].LCP := FVertexes[0].LCP;
      FVertexes[0].LCP :=-1;
      FVertexes[High(FVertexes)].RCP :=-1;
    end;
  end
  else begin
    if Closed
    then begin
      AddPoints(PointsCount,1);
      setPoint(PointsCount-1,Vertex[0]);
      setPT(PointsCount-1,PT_LINETO);
      setLength(FVertexes,High(FVertexes)+2);
      FVertexes[0].LCP := 0;
      FVertexes[High(FVertexes)].Point := PointsCount-1;
      FVertexes[High(FVertexes)].LCP :=-1;
      FVertexes[High(FVertexes)].RCP := FVertexes[0].RCP;
      FCycled := true;
    end
    else begin
      FVertexes[0].LCP :=-1;
      FVertexes[High(FVertexes)].RCP :=-1;
    end;
  end;
  UpdateBounds;
  unLock;
  Changed([ocpBounds]);
end;

function       TdeSimpleCurve.prevVertex(vtxIndex:integer):integer;
begin
  if (vtxIndex < 0)or(vtxIndex >= VertexesCount) then
    Result :=-1
  else begin
    Result := vtxIndex-1;
    if (Result=-1)
    then begin
      if Closed then Result := VertexesCount-1
                else Result :=-1;
    end;
  end;
end;

procedure      TdeSimpleCurve.DrawCP(Canvas:TCanvas;iCP,iP:TPoint);
begin
  with Canvas do begin
    Pen.Mode    := pmCOPY;
    Pen.Style   := psSolid;
    Brush.Style := bsSolid;

    Pen.Width   := 3;
    Pen.Color   := clWhite;
    MoveTo(iCP.X,iCP.Y);
    LineTo(iP.X,iP.Y);
    Pen.Width   := 1;
    Pen.Color   := clBlue;
    MoveTo(iCP.X,iCP.Y);
    LineTo(iP.X,iP.Y);

    Pen.Color   := clBlack;
    Brush.Color := clWhite;
    Rectangle(iCP.X-cPointSize,iCP.Y-cPointSize,
              iCP.X+cPointSize,iCP.Y+cPointSize);
  end;
end;

procedure     TdeSimpleCurve.DrawVertex(Canvas:TCanvas;aVtx:TPoint;Selected: Boolean = False);
begin
  with Canvas do begin
    Pen.Color   := clWhite;
    Pen.Mode    := pmCOPY;
    Pen.Style   := psSolid;
    Pen.Width   := 1;
    Brush.Color := clBlack;
    Brush.Style := bsSolid;

    if Selected then
      begin
        Rectangle(aVtx.X-cPointSize-2,aVtx.Y-cPointSize-2,
                  aVtx.X+cPointSize+2,aVtx.Y+cPointSize+2);
        Pen.Color   := clLime;
      end;

    Rectangle(aVtx.X-cPointSize,aVtx.Y-cPointSize,
              aVtx.X+cPointSize,aVtx.Y+cPointSize);
  end;
end;

procedure     TdeSimpleCurve.UpdateVertexes;
var
  Cnt,i             : integer;
  vtxCount,vtxIndex : integer;
begin
  Cnt := PointsCount;
  vtxCount := 0;
  i := 0;
  while (i < Cnt) do begin
    case PntType[i] of
      PT_MOVETO   : ;
      PT_LINETO   : ;
      PT_BEZIERTO : inc(i,2);
    end;
    inc(vtxCount);
    inc(i);
  end;
  setLength(FVertexes,vtxCount);
  vtxIndex := 0;
  i        := 0;
  while (i < PointsCount) do begin
    case PntType[i] of
      PT_MOVETO   : begin
        FVertexes[vtxIndex].Point := i;
        FVertexes[vtxIndex].LCP   :=-1;
        FVertexes[vtxIndex].RCP   :=-1;
      end;
      PT_LINETO   : begin
        FVertexes[vtxIndex].Point := i;
        FVertexes[vtxIndex].LCP   :=-1;
        FVertexes[vtxIndex].RCP   :=-1;
        FVertexes[vtxIndex-1].RCP :=-1;
      end;
      PT_BEZIERTO : begin
        inc(i,2);
        FVertexes[vtxIndex].Point  := i;
        FVertexes[vtxIndex].LCP    := i-1;
        FVertexes[vtxIndex].RCP   :=-1;
        FVertexes[vtxIndex-1].RCP  := i-2;
      end;
    end;
    inc(vtxIndex);
    inc(i);
  end;
  if Closed
  then begin
    if (FCycled)
    then begin
      FVertexes[0].LCP := FVertexes[High(FVertexes)].LCP;
      FVertexes[High(FVertexes)].RCP := FVertexes[0].RCP;
    end
    else begin
      AddPoints(PointsCount,1);
      setPoint(PointsCount-1,Vertex[0]);
      setPT(PointsCount-1,PT_LINETO);
      setLength(FVertexes,High(FVertexes)+2);
      FVertexes[High(FVertexes)].Point := PointsCount-1;
      FVertexes[High(FVertexes)].LCP :=-1;
      FVertexes[High(FVertexes)].RCP := FVertexes[0].RCP;
      FCycled := true;
    end;
  end;
end;

procedure     TdeSimpleCurve.RecalcVertexType(vtxIndex:integer);
var
  P,LCP,RCP : TPoint2D;
  VL,VR     : TPoint2D;
begin
  if getLeftCP(vtxIndex,LCP)and getRightCP(vtxIndex,RCP)
  then begin
    P   := Vertex[vtxIndex];
    VL  := Vector2D(LCP,P);
    VR  := Vector2D(P,RCP);
    if (VLength2D(VL) > DE_EPSILON)
       and(VLength2D(VR) > DE_EPSILON)
    then begin
      if (Distance2D(VL,VR) < DE_EPSILON) then
        FVertexes[vtxIndex].VType := vtxSmooth
      else if (Distance2D(Norm2D(VL),Norm2D(VR)) < DE_EPSILON) then
        FVertexes[vtxIndex].VType := vtxDirect
      else
        FVertexes[vtxIndex].VType := vtxAngular;
    end
    else
      FVertexes[vtxIndex].VType := vtxAngular;
  end
  else
    FVertexes[vtxIndex].VType := vtxAngular;
  if (vtxIndex = 0)and Closed and FCycled then
    FVertexes[High(FVertexes)].VType := FVertexes[0].VType;
end;

procedure     TdeSimpleCurve.RecalcVertexesTypes;
var
  vtxIndex : integer;
begin
  vtxIndex := 0;
  while (vtxIndex < VertexesCount) do begin
    recalcVertexType(vtxIndex);
    inc(vtxIndex);
  end;
end;

function      TdeSimpleCurve.getVertexesCount:integer;
begin
  Result := High(FVertexes)+1;
  if Closed and FCycled then
    Result := Result-1;
end;

function      TdeSimpleCurve.getVertex(vtxIndex:integer):TPoint2D;
begin
  Result := Point[FVertexes[vtxIndex].Point];
end;

procedure     TdeSimpleCurve.setVertex(vtxIndex:integer;aValue:TPoint2D);
var
  i   : integer;
  V,P : TPoint2D;
  VT  : TdeVertexType;
begin
  Changing;
  i := FVertexes[vtxIndex].Point;
  V := Vector2D(Point[i],aValue);
  if (vtxIndex = 0)and(FCycled)
  then begin
    if Closed then
      setPoint(PointsCount-1,aValue)
    else
      FCycled := false;
  end;
  setPoint(i,aValue);
  VT := FVertexes[vtxIndex].VType;
  FVertexes[vtxIndex].VType := vtxAngular;
  if getLeftCP(vtxIndex,P) then
    setLeftCP(vtxIndex,OffsetPoint2D(P,V.X,V.Y));
  if getRightCP(vtxIndex,P) then
    setRightCP(vtxIndex,OffsetPoint2D(P,V.X,V.Y));
  FVertexes[vtxIndex].VType := VT;
  UpdateBounds;
  Changed([]);
end;

function      TdeSimpleCurve.getVtxType(vtxIndex:integer):TdeVertexType;
begin
  Result := FVertexes[vtxIndex].VType;
end;

procedure     TdeSimpleCurve.setVtxType(vtxIndex:integer;aValue:TdeVertexType);
var
  iP         : integer;
  P,LCP,RCP  : TPoint2D;
  V          : TPoint2D;
  dL,dR      : double;
begin
  if (VertexType[vtxIndex] = aValue) then exit;
  Changing;
  FVertexes[vtxIndex].VType := aValue;
  iP   := FVertexes[vtxIndex].Point;
  if (aValue <> vtxAngular)
     and(getLeftCP(vtxIndex,LCP))
     and(getRightCP(vtxIndex,RCP))
  then begin
    P := Point[iP];
    V := Vector2D(LCP,P);
    case aValue of
      vtxDirect  : begin
        dL := Distance2D(LCP,P);
        dR := Distance2D(RCP,P);
        if dL > 0 then
          setRightCP(vtxIndex,OffsetPoint2D(P,V.X/dL*dR,V.Y/dL*dR));
      end;
      vtxSmooth  : begin
        setRightCP(vtxIndex,OffsetPoint2D(P,V.X,V.Y));
      end;
    end;
  end;
  UpdateBounds;
  Changed([]);
end;

function      TdeSimpleCurve.getSegmentsCount:integer;
begin
  Result := VertexesCount-1;
  if (Result > 0)and Closed then
    Result := Result+1;
end;

function      TdeSimpleCurve.getSgmType(sgmIndex:integer):TdeSegmentType;
begin
  if (sgmIndex >= 0)and(sgmIndex < SegmentsCount)
  then begin
    if (FVertexes[sgmIndex].RCP > 0) then
      Result := sgmBezier
    else
     Result := sgmLinear;
  end
  else
    raise EVertexException.Create(Format('Invalid segment (%d)',[sgmIndex]));
end;

procedure     TdeSimpleCurve.setSgmType(sgmIndex:integer;aValue:TdeSegmentType);
var
  i,iP  : integer;
  dX,dY : single;
begin
  if (SegmentType[sgmIndex] = aValue) then exit;
  Changing;
  if (aValue = sgmLinear) then begin
    //удаляем контрольные точки
    iP := FVertexes[sgmIndex].Point;
    DelPoints(iP+1,2);
    setPT(iP+1,PT_LINETO);
    //перестраиваем индекс
    iP := iP+2;
    for i := 0 to High(FVertexes) do begin
      if (FVertexes[i].Point > iP) then dec(FVertexes[i].Point,2);
      if (FVertexes[i].LCP > iP)   then dec(FVertexes[i].LCP,2);
      if (FVertexes[i].RCP > iP)   then dec(FVertexes[i].RCP,2);
    end;
    FVertexes[sgmIndex].RCP   :=-1;
    FVertexes[NextVertex(sgmIndex)].LCP :=-1;
  end
  else begin
    //добавляем контрольные точки
    iP := FVertexes[sgmIndex].Point;
    AddPoints(iP,2);
    setPT(iP+3,PT_BEZIERTO);
    //перестраиваем индекс
    for i := 0 to High(FVertexes) do begin
      if (FVertexes[i].Point > iP) then inc(FVertexes[i].Point,2);
      if (FVertexes[i].LCP > iP)   then inc(FVertexes[i].LCP,2);
      if (FVertexes[i].RCP > iP)   then inc(FVertexes[i].RCP,2);
    end;
    FVertexes[sgmIndex].RCP   := iP+1;
    FVertexes[NextVertex(sgmIndex)].LCP := iP+2;
    //TODO -cMap: выставляем значения созданных контрольных точек
    {
    setPoint(iP+1,Point[iP+3]);
    setPT(iP+1,PT_BEZIERTO);
    setPoint(iP+2,Point[iP]);
    setPT(iP+2,PT_BEZIERTO);
    }
    dX := (Point[iP+3].X - Point[iP].X)/3;
    dY := (Point[iP+3].Y - Point[iP].Y)/3;
    setPoint(iP+1,OffsetPoint2D(Point[iP],dX,dY));
    setPT(iP+1,PT_BEZIERTO);
    setPoint(iP+2,OffsetPoint2D(Point[iP+3],-dX,-dY));
    setPT(iP+2,PT_BEZIERTO);
    {}
  end;
  UpdateBounds;
  Changed([]);
end;

function      TdeSimpleCurve.CreateMemento:TdeSchemaObjectMemento;
begin
  Result := TdeSimpleCurveMemento.Create(Self);
end;

function      TdeSimpleCurve.getDrawPath(var Points:array of TPoint2D;
                                         var Cnt:integer;
                                           pConv:pMatrix2D = nil):boolean;
begin
  Result := inherited getDrawPath(Points,Cnt,pConv);
  if Result then begin
    if FClosed then
      Points[Cnt-1].W := trunc(Points[Cnt-1].W)or PT_CLOSEFIGURE;
  end;
end;

procedure     TdeSimpleCurve.setDrawPath(const Points:array of TPoint2D;
                                                  Cnt:integer;
                                                pConv:pMatrix2D = nil);
begin
  Changing;
  Lock;
  try
    inherited setDrawPath(Points,Cnt,pConv);
    FClosed := (Cnt > 1)
               and((trunc(FDrawPath[Cnt-1].W)and PT_CLOSEFIGURE) = PT_CLOSEFIGURE);
    FDrawPath[Cnt-1].W := trunc(FDrawPath[Cnt-1].W)and not PT_CLOSEFIGURE;
    FCycled := (Cnt > 1)
               and(Distance2D(Point[0],Point[PointsCount-1]) < DE_EPSILON);
    FVertexes := nil;
    if (Cnt = 0) then exit;
    UpdateVertexes;
    recalcVertexesTypes;
  finally
    UpdateBounds;
    unLock;
    Changed([]);
  end;
end;

function PPToV2D(const P1,P2:TPoint):tPoint2D;
begin
  Result := Point2D(P2.X-P1.X,P2.Y-P1.Y)
end;

procedure     TdeSimpleCurve.PaintChanges(Canvas:TCanvas;StoC:TMatrix2D;ShowInfo:TdeViewCoorninates=vcNone);
const D = 16;
var
  i,iP,iPL    : integer;
  P0,P1,P2,P3 : TPoint2D;
  iPt         : array[0..2] of TPoint;
  StoV        : TMatrix2D;

    Procedure PaintPointAbout(aI:Integer);
    var
      pT2D,pT2D1,pT2D2,pT2DR : TPoint2D;
      pT,pT1,pT2        : TPoint;
      s                 : String;
      sW,sH,a_i,b_i     : Integer;
      sS                : Single;
    begin

      Canvas.Brush.Style := bsClear;
      Canvas.Font.Color  := clBlack;
      Canvas.Font.Style  := [];

      s    := IntToName(ai);//+' '+IntToStr(Round(pT2D1.X))+':'+IntToStr(Round(pT2D1.Y));
      sW   := Canvas.TextWidth (s);
      sH   := Canvas.TextHeight(s);
      sS   := Max(sH,sW)/Sqrt(2);

      pT2D := OToS(Vertex[ai]);
      pT   := P2DtoP(MultVM2D(Vertex[ai],StoV));

      a_i:=prevVertex(ai);
      b_i:=nextVertex(ai);
      if a_i<0 then a_i:=ai;
      if b_i<0 then b_i:=ai;

      pT1  := P2DtoP(MultVM2D(Vertex[a_i],StoV));
      pT2  := P2DtoP(MultVM2D(Vertex[b_i],StoV));

      pT2D1:=Norm2Dalt(Point2D(pT.X-pT1.X,pT.Y-pT1.Y));
      pT2D2:=Norm2Dalt(Point2D(pT.X-pT2.X,pT.Y-pT2.Y));
      pT2DR:=Norm2Dalt(Point2D(pT2D1.X+pT2D2.X,pT2D1.Y+pT2D2.Y),sS);

      if VLength2D(pT2DR)=0 then
        begin
            pT2DR:=Norm2Dalt(Point2D(pT2D1.Y,-pT2D1.X),sS);
          if VLength2D(pT2DR)=0 then
            pT2DR:=Norm2Dalt(Point2D(pT2D2.Y,-pT2D2.X),sS);
          if VLength2D(pT2DR)=0 then
            pT2DR.X:=sS;
        end;

      Canvas.TextOut(pT.X+Round(pT2DR.X)-(sW div 2),
                     pT.Y+Round(pT2DR.Y)-(sH div 2), s);
    end;

    Procedure PaintSizeAbout(aI,bI:Integer);
    var
      LogFont         : TLogFont;
      pT2D1,pT2D2     : TPoint2D;
      pT1,pT2,pTA,pT3,pT4 : TPoint;
      a_i,b_i,i1,i2,sw,L,LT,i   : Integer;
      s               : String;
      BreakX,BreakY   : Boolean;
      r1,r2,r3        : Single;
    begin
      if (aI<0) or (bI<0) then Exit;

      a_i:= prevVertex(ai);
      b_i:= NextVertex(bi);
      if a_i<0 then a_i:=ai;
      if b_i<0 then b_i:=bi;

      pT2D1 := OToS(Vertex[ai]);
      pT2D2 := OToS(Vertex[bi]);

      pT1   := P2DtoP(MultVM2D(Vertex[ai ],StoV));
      pT2   := P2DtoP(MultVM2D(Vertex[bi ],StoV));
      pT3   := P2DtoP(MultVM2D(Vertex[a_i],StoV));
      pT4   := P2DtoP(MultVM2D(Vertex[b_i],StoV));

      Canvas.Font.Name  := 'Default';//Times New Roman';
      Canvas.Font.Style := [];
      Canvas.Font.Size  := 7;
      Canvas.Font.Color := clBlack;
      Canvas.Brush.Style:= bsClear;

      BreakX:=False;
      BreakY:=False;

      for i:=bi+1 to VertexesCount-2 do
        begin
          i1:=(i+  VertexesCount) mod VertexesCount;
          i2:=(i+1+VertexesCount) mod VertexesCount;
          BreakX:=BreakX or
                  ((Vertex[i1].X=Vertex[ai].X) and (Vertex[i2].X=Vertex[bi].X)) or
                  ((Vertex[i1].X=Vertex[bi].X) and (Vertex[i2].X=Vertex[ai].X));
          BreakY:=BreakY or
                  ((Vertex[i1].Y=Vertex[ai].Y) and (Vertex[i2].Y=Vertex[bi].Y)) or
                  ((Vertex[i1].Y=Vertex[bi].Y) and (Vertex[i2].Y=Vertex[ai].Y));
        end; 

      if (pT1.Y<>pT2.Y)and(Not BreakY) then
        begin
          getObject(Canvas.Font.Handle,sizeOf(TLogFont),@LogFont);
          LogFont.lfEscapement  := 900;
          LogFont.lfOrientation := 900;
          Canvas.Font.Handle := CreateFontIndirect(LogFont);

          // меняем точки между собой
          if pT1.Y>pT2.Y then
            begin pTA:=pT1; pT1:=pT2; pT2:=pTA;
                  pTA:=pT3; pT3:=pT4; pT4:=pTA;  end;
          
          // определяем сторону, с которой наносить надпись
          r1:=DistancePL(PToP2D(pT3),PToP2D(pT1),PToP2D(pT2));
          r2:=DistancePL(PToP2D(pT4),PToP2D(pT1),PToP2D(pT2));
          r3:=DistancePL(Point2D(Max(pT1.X,pT2.X)+D,(pT1.Y+pT2.Y)/2),
                                     PToP2D(pT1),PToP2D(pT2));

          // выпуклый отрезок, однозначно определяется
          if (r1*r2>=0) then
            if (r1*r3<=0) then begin L:=Max(pT1.X,pT2.X)+D; LT:=L;    end
                          else begin L:=Min(pT1.X,pT2.X)-D; LT:=L-12; end
          else

          // соседи не мешают, выбираем лучший
          if (pT3.Y<=pT1.Y) and (pT2.Y<=pT4.Y) then
            if abs(Norm2Dalt(PPToV2D(pT3,pT1)).Y) <
               abs(Norm2Dalt(PPToV2D(pT4,pT2)).Y)
                          then begin L:=Max(pT1.X,pT2.X)+D; LT:=L;    end
                          else begin L:=Min(pT1.X,pT2.X)-D; LT:=L-12; end
          else

          // один сосед мешает, однозначно определяется
          if (pT3.Y<=pT1.Y) and Not(pT2.Y<=pT4.Y) then
            if (r1*r3>=0) then begin L:=Max(pT1.X,pT2.X)+D; LT:=L;    end
                          else begin L:=Min(pT1.X,pT2.X)-D; LT:=L-12; end
          else

          // друшой сосед мешает, однозначно определяется
          if Not(pT3.Y<=pT1.Y) and (pT2.Y<=pT4.Y) then
            if (r1*r3<=0) then begin L:=Max(pT1.X,pT2.X)+D; LT:=L;    end
                          else begin L:=Min(pT1.X,pT2.X)-D; LT:=L-12; end
          else

          // оба соседа мешают, выбираем кто мешает меньше
          begin
            if (abs(Norm2Dalt(PPToV2D(pT3,pT1)).Y)>
                abs(Norm2Dalt(PPToV2D(pT4,pT2)).Y) ) xor (pT1.X<pT2.X)
                         then begin L:=Min(pT1.X,pT2.X)+D; LT:=L;    end
                         else begin L:=Max(pT1.X,pT2.X)-D; LT:=L-12; end
          end;

          // наносим линии и надпись
            Canvas.Pen.Color:=clGray;
            Canvas.Pen.Style:=psDot;
            Canvas.Brush.Style:=bsClear;
          Canvas.MoveTo(pT1.X,pT1.Y); Canvas.LineTo(L,pT1.Y);
          Canvas.MoveTo(pT2.X,pT2.Y); Canvas.LineTo(L,pT2.Y);
            Canvas.Pen.Color:=clBlack;
            Canvas.Pen.Style:=psSolid;

          s:= IntToStr(Round(abs(pT2D1.Y-pT2D2.Y)));
          sw:=Canvas.TextWidth(s);

          if abs(pT1.Y-pT2.Y)<10+sw then
            begin
              pTA:=pT1; pT1:=pT2; pT2:=pTA;
              Canvas.TextOut(LT, pT2.Y-5, s);
              Canvas.MoveTo(L,pT1.Y); Canvas.LineTo(L,pT2.Y-5-sw);
            end
          else
            begin
              Canvas.TextOut(LT, pT2.Y-((abs(pT1.Y-pT2.Y)-sw) div 2), s);
              Canvas.MoveTo(L,pT1.Y); Canvas.LineTo(L,pT2.Y+1);
            end;

          // рисуем стрелки
          Canvas.MoveTo(L-1,pT1.Y+1); Canvas.LineTo(L+2,pT1.Y+1);
          Canvas.MoveTo(L-2,pT1.Y+2); Canvas.LineTo(L+3,pT1.Y+2);
          Canvas.MoveTo(L-3,pT1.Y+3); Canvas.LineTo(L+4,pT1.Y+3);

          Canvas.MoveTo(L-1,pT2.Y-1); Canvas.LineTo(L+2,pT2.Y-1);
          Canvas.MoveTo(L-2,pT2.Y-2); Canvas.LineTo(L+3,pT2.Y-2);
          Canvas.MoveTo(L-3,pT2.Y-3); Canvas.LineTo(L+4,pT2.Y-3);
        end;
      
      if (pT1.X<>pT2.X) and (Not BreakX) then
        begin
          getObject(Canvas.Font.Handle,sizeOf(TLogFont),@LogFont);
          LogFont.lfEscapement := 0;
          LogFont.lfOrientation := 0;
          Canvas.Font.Handle := CreateFontIndirect(LogFont);

          // меняем точки между собой
          if pT1.X>pT2.X then
            begin pTA:=pT1; pT1:=pT2; pT2:=pTA;
                  pTA:=pT3; pT3:=pT4; pT4:=pTA;  end;

          // определяем сторону, с которой наносить надпись
          r1:=DistancePL(PToP2D(pT3),PToP2D(pT1),PToP2D(pT2));
          r2:=DistancePL(PToP2D(pT4),PToP2D(pT1),PToP2D(pT2));
          r3:=DistancePL(Point2D((pT1.X+pT2.X)/2,Max(pT1.Y,pT2.Y)+D),
                                     PToP2D(pT1),PToP2D(pT2));

          // выпуклый отрезок, однозначно определяется
          if (r1*r2>=0) then
            if (r1*r3<=0) then begin L:=Max(pT1.Y,pT2.Y)+D; LT:=L+1;  end
                          else begin L:=Min(pT1.Y,pT2.Y)-D; LT:=L-11; end
          else

          // соседи не мешают, выбираем лучший
          if (pT3.X<=pT1.X) and (pT2.X<=pT4.X) then
            if abs(Norm2Dalt(PPToV2D(pT3,pT1)).X)<
               abs(Norm2Dalt(PPToV2D(pT4,pT2)).X)
                          then begin L:=Max(pT1.Y,pT2.Y)+D; LT:=L+1;  end
                          else begin L:=Min(pT1.Y,pT2.Y)-D; LT:=L-11; end
          else

          // один сосед мешает, однозначно определяется
          if (pT3.X<=pT1.X) and Not(pT2.X<=pT4.X) then
            if (r1*r3>=0) then begin L:=Max(pT1.Y,pT2.Y)+D; LT:=L+1;  end
                          else begin L:=Min(pT1.Y,pT2.Y)-D; LT:=L-11; end
          else

          // друшой сосед мешает, однозначно определяется
          if Not(pT3.X<=pT1.X) and (pT2.X<=pT4.X) then
            if (r1*r3<=0) then begin L:=Max(pT1.Y,pT2.Y)+D; LT:=L+1;  end
                          else begin L:=Min(pT1.Y,pT2.Y)-D; LT:=L-11; end
          else

          // оба соседа мешают, выбираем кто мешает меньше
          begin
            if ( abs(Norm2Dalt(PPToV2D(pT3,pT1)).X)<
                 abs(Norm2Dalt(PPToV2D(pT4,pT2)).X)  ) xor (pT1.Y<pT2.Y)
                             then begin L:=Min(pT1.Y,pT2.Y)+D; LT:=L+1;  end
                             else begin L:=Max(pT1.Y,pT2.Y)-D; LT:=L-11; end
          end;

          // наносим линии и надпись
            Canvas.Pen.Color:=clGray;
            Canvas.Pen.Style:=psDot;
          Canvas.MoveTo(pT1.X,pT1.Y); Canvas.LineTo(pT1.X,L);
          Canvas.MoveTo(pT2.X,pT2.Y); Canvas.LineTo(pT2.X,L);
            Canvas.Pen.Color:=clBlack;
            Canvas.Pen.Style:=psSolid;
          s:= IntToStr(Round(abs(pT2D1.X-pT2D2.X)));
          sw:=Canvas.TextWidth(s);
          if abs(pT1.X-pT2.X)<9+sw then
            begin
              pTA:=pT1; pT1:=pT2; pT2:=pTA;
              Canvas.TextOut(pT1.X+5, LT, s );
              Canvas.MoveTo(pT1.X+5+sw,L); Canvas.LineTo(pT2.X,L);
            end
          else
            begin
              Canvas.TextOut(1+(pT1.X+pT2.X-sw) div 2, LT, s );
              Canvas.MoveTo(pT1.X,L); Canvas.LineTo(pT2.X+1,L);
            end;

          // рисуем стрелки
          Canvas.MoveTo(pT1.X+1,L-1); Canvas.LineTo(pT1.X+1,L+2);
          Canvas.MoveTo(pT1.X+2,L-2); Canvas.LineTo(pT1.X+2,L+3);
          Canvas.MoveTo(pT1.X+3,L-3); Canvas.LineTo(pT1.X+3,L+4);

          Canvas.MoveTo(pT2.X-1,L-1); Canvas.LineTo(pT2.X-1,L+2);
          Canvas.MoveTo(pT2.X-2,L-2); Canvas.LineTo(pT2.X-2,L+3);
          Canvas.MoveTo(pT2.X-3,L-3); Canvas.LineTo(pT2.X-3,L+4);
        end;  
    end;

begin
  StoV := MultMM2D(FOtoSConv,StoC);
  if (FUpdating > 0) then begin
    if (FActiveVtx >= 0)and(FActiveVtx < VertexesCount) then begin
      Canvas.Pen.Color   := clWhite;
      Canvas.Pen.Style   := psDot;
      Canvas.Pen.Mode    := pmXOR;
      Canvas.Brush.Style := bsClear;
      iP := FActiveVtx;

      iPL := prevVertex(iP);
      if (iPL >= 0) then begin
        P0 := Vertex[iPL];
        P3 := Vertex[iP];
        iPt[0] := P2DtoP(MultVM2D(P0,StoV));
        Canvas.MoveTo(iPt[0].X,iPt[0].Y);
        if (SegmentType[iPL] = sgmLinear) then begin
          iPt[0] := P2DtoP(MultVM2D(P3,StoV));
          Canvas.LineTo(iPt[0].X,iPt[0].Y);
        end
        else begin
          getRightCP(iPL,P1);
          getLeftCP(iP,P2);

          iPt[0] := P2DtoP(MultVM2D(P1,StoV));
          iPt[1] := P2DtoP(MultVM2D(P2,StoV));
          iPt[2] := P2DtoP(MultVM2D(P3,StoV));
          Canvas.PolyBezierTo(iPt);
          Canvas.LineTo(iPt[1].X,iPt[1].Y);
        end;
      end;
      iPL := nextVertex(iP);
      if (iPL >= 0) then begin
        P0 := Vertex[iP];
        P3 := Vertex[iPL];
        iPt[0] := P2DtoP(MultVM2D(P3,StoV));
        Canvas.MoveTo(iPt[0].X,iPt[0].Y);
        if (SegmentType[iP] = sgmLinear) then begin
          iPt[0] := P2DtoP(MultVM2D(P0,StoV));
          Canvas.LineTo(iPt[0].X,iPt[0].Y);
        end
        else begin
          getRightCP(iP,P1);
          getLeftCP(iPL,P2);
          iPt[0] := P2DtoP(MultVM2D(P2,StoV));
          iPt[1] := P2DtoP(MultVM2D(P1,StoV));
          iPt[2] := P2DtoP(MultVM2D(P0,StoV));
          Canvas.PolyBezierTo(iPt);
          Canvas.LineTo(iPt[1].X,iPt[1].Y);
        end;
      end;

      if ShowInfo in [vcPoint,vcBoth] then
        PaintPointAbout(ip);

      if ShowInfo in [vcLine,vcBoth] then
        begin
          PaintSizeAbout(PrevVertex(ip),ip);
          PaintSizeAbout(ip,NextVertex(ip));
        end;
    end
  end
  else begin
    i := 0;
    while (i < VertexesCount) do begin
      iPt[0] := P2DtoP(MultVM2D(Vertex[i],StoV));

      if (i = FActiveVtx) then begin
        if getLeftCP(i,P0) then begin
          iPt[1] := P2DtoP(MultVM2D(P0,StoV));
          DrawCP(Canvas,iPt[1],iPt[0]);
        end;
        if getRightCP(i,P0) then begin
          P0 := Point[FVertexes[i].RCP];
          iPt[1] := P2DtoP(MultVM2D(P0,StoV));
          DrawCP(Canvas,iPt[1],iPt[0]);
        end;
      end;

      DrawVertex(Canvas,iPt[0],i=ActiveVertex);

      if ShowInfo in [vcPoint,vcBoth] then
        PaintPointAbout(i);

      if (ShowInfo in [vcLine,vcBoth]) then
        PaintSizeAbout(i,NextVertex(i));
      inc(i);
    end;
  end;
end;

procedure     TdeSimpleCurve.setActiveVertex(vertexID:integer);
begin
  FActiveVtx := vertexID;
end;

procedure     TdeSimpleCurve.getSegmentPoints(sgmIndex:integer;
                                        var P0,P1,P2,P3:TPoint2D);
var
  i : integer;
begin
  if (sgmIndex >= 0)and(sgmIndex < SegmentsCount)
  then begin
    P0 := Point[FVertexes[sgmIndex].Point];
    i  := nextVertex(sgmIndex);
    P3 := Point[FVertexes[i].Point];
    getRightCP(sgmIndex,P1);
    getLeftCP(i,P2);
  end
  else
    raise EVertexException.Create(Format('Invalid segment (%d)',[sgmIndex]));
end;

procedure     TdeSimpleCurve.BeginUpdate;
begin
  if (FUpdating < 0) then
    FUpdating := 0;
  inc(FUpdating);
end;

procedure     TdeSimpleCurve.EndUpdate;
begin
  dec(FUpdating);
  if (FUpdating < 0) then
    FUpdating := 0;
end;

function      TdeSimpleCurve.IsUpdating:boolean;
begin
  Result := (FUpdating > 0);
end;

function      TdeSimpleCurve.Classify( aPoint:TPoint2D;
                                    PointSize:double;
                                 var vtxIndex:integer):TdeVertexesPointClass;
var
  Cross        : TPoint2D;
  Parity       : boolean;
  P0,P1,P2,P3  : TPoint2D;
  X1,X2        : TPoint2D;
  i,j          : integer;
  t            : double;
  procedure _CheckSegment(X1,X2 : TPoint2D);
  begin
    if (DistancePV2D(aPoint,X1,X2) <= PointSize) then begin
      Result := vpcEdge;
      vtxIndex := i;
    end
    else if (X1.Y <= Cross.Y)and(X2.Y > Cross.Y)
            or(X1.Y > Cross.Y)and(X2.Y <= Cross.Y)
    then begin //отрезок пересекает прямую Y=Cross.Y
      Cross.X := X1.X+(X2.X-X1.X)*(Cross.Y-X1.Y)/(X2.Y-X1.Y);
      if (Cross.X > aPoint.X) then
        Parity := not(Parity);
    end;
  end;
begin
  Result := vpcUnknown;
  if (VertexesCount = 0) then exit;
  //если vtxIndex>=0 то проверим на попадание в ее контрольные точки
  if (vtxIndex >= 0)and(vtxIndex < VertexesCount) then begin
    if getLeftCP(vtxIndex,X1)
       and(Distance2D(aPoint,X1) <= PointSize)
    then
      Result := vpcLeftCP
    else begin
      if getRightCP(vtxIndex,X1)
         and(Distance2D(aPoint,X1) <= PointSize)
      then
        Result := vpcRightCP;
    end;
  end;
  if Result <> vpcUnknown then exit;

  //далее проверим на попадание в вершину
  vtxIndex := -1;
  i := 0;
  while(i < VertexesCount)and(Result = vpcUnknown)
  do begin
    if (Distance2D(aPoint,Vertex[i]) <= PointSize)
    then begin
      vtxIndex := i;
      Result   := vpcVertex;
    end
    else
      inc(i);
  end;
  if Result <> vpcUnknown then exit;

  //далее проверим на попадание в "ребро"
  Cross.Y := aPoint.Y;
  Parity:=true;
  X2 := Vertex[0];
  i  := 0;
  while (i < SegmentsCount) and (Result = vpcUnknown)
  do begin
    getSegmentPoints(i,P0,P1,P2,P3);
    if (SegmentType[i] = sgmLinear) then
      _CheckSegment(P0,P3)
    else begin
      j := 1;
      t := 1/MAX_BEZIER_POINTS;
      X2 := P0;
      while (j <= MAX_BEZIER_POINTS) and (Result = vpcUnknown)
      do begin
        X1 := X2;
        X2 := Bezier2D(P0,P1,P2,P3,j*t);
        _CheckSegment(X1,X2);
        inc(j);
      end;
    end;
    inc(i);
  end;
  if (Result = vpcUnknown)and not(Parity)
  then
    Result := vpcBody;
end;

procedure     TdeSimpleCurve.AddVertex(sgmIndex:integer;Value:TPoint2D);
var
  i             : integer;
  iSP,iNP,iRP   : integer;
  NewVI         : integer;
  iShift        : integer;
  dX,dY         : single;
begin
  //добавить вершину в сегмент sgmIndex
  if (sgmIndex >= 0)and(sgmIndex < SegmentsCount) then begin
    Changing;
    //запоминаем начальную точку сегмента
    iSP := FVertexes[sgmIndex].Point;
    NewVI := sgmIndex+1;
    //добавляем вершину в список вершин
    setLength(FVertexes,High(FVertexes)+2);
    for i := High(FVertexes) downto sgmIndex+2 do
      FVertexes[i] := FVertexes[i-1];

    //добавить точку в массив точек
    if (FVertexes[sgmIndex].RCP > 0) then begin //сегмент искривленный!
      iNP := iSP+3;
      AddPoints(iNP-1,3);//AddPoints(iSP-1,3);
      setPoint(iNP, Value);
      setPT(iNP, PT_BEZIERTO);
      //TODO -cMap: рассчитать значения контрольных точек
      {
      setPoint(iNP-1, Value);
      setPT(iNP-1, PT_BEZIERTO);
      setPoint(iNP+1, Value);
      setPT(iNP+1, PT_BEZIERTO);
      }
      iRP := iNP + 3;
      dX := (Point[iRP-1].X - Point[iSP+1].X) / 3;
      dY := (Point[iRP-1].Y - Point[iSP+1].Y) / 3;
      //setPoint(iSP+1, OffsetPoint2D(Point[iSP],dX,dY));
      //setPT(iSP+1, PT_BEZIERTO);
      setPoint(iNP-1, OffsetPoint2D(Value,-dX,-dY));
      setPT(iNP-1, PT_BEZIERTO);
      setPoint(iNP+1, OffsetPoint2D(Value,dX,dY));
      setPT(iNP+1, PT_BEZIERTO);
      //setPoint(iRP-1, OffsetPoint2D(Point[iRP],-dX,-dY));
      //setPT(iRP-1, PT_BEZIERTO);
      {}
      FVertexes[NewVI].Point := iNP;
      FVertexes[NewVI].LCP   := iNP-1;
      FVertexes[NewVI].RCP   := iNP+1;
      iShift := 3;
    end
    else begin //сегмент линейный!
      AddPoints(iSP+1,1);
      iNP := iSP+1;
      setPoint(iNP, Value);
      setPT(iNP, PT_LINETO);
      FVertexes[NewVI].Point := iNP;
      FVertexes[NewVI].LCP   :=-1;
      FVertexes[NewVI].RCP   :=-1;
      iShift := 1;
    end;
    i := NewVI+1;
    while (i <= High(FVertexes)) do begin
      if (FVertexes[i].Point > iSP) then
        FVertexes[i].Point := FVertexes[i].Point+iShift;
      if (FVertexes[i].LCP > iSP) then
        FVertexes[i].LCP := FVertexes[i].LCP+iShift;
      if (FVertexes[i].RCP > iSP) then
        FVertexes[i].RCP := FVertexes[i].RCP+iShift;
      i := i + 1;
    end;
    if (newVI = VertexesCount-1)and Closed and FCycled then begin
      FVertexes[High(FVertexes)].RCP := FVertexes[0].RCP;
      FVertexes[0].LCP := FVertexes[High(FVertexes)].LCP;
    end;
    UpdateBounds;
    Changed([ocpBounds]);
  end
  else
    raise EVertexException.Create(Format('Invalid segment (%d)',[sgmIndex]));
end;

procedure     TdeSimpleCurve.DelVertex(vtxIndex:integer);
var
  i             : integer;
  prevVI,nextVI : integer;
  iShift,iBase  : integer;
  vtxCount      : integer;
begin
  // удалить вершину vtxIndex
  if (vtxIndex >= 0)and(vtxIndex < VertexesCount) then
  begin
    if (VertexesCount = 2) or ((FClosed) and (VertexesCount = 3)) then
      begin
        Delete;
        exit;
      end;
    Changing;
    prevVI := prevVertex(vtxIndex);
    if (prevVI < 0) then begin //удаляем первую вершину в незамкнутой фигуре
      //вычисляем число удаляемых точек
      if (segmentType[vtxIndex] = sgmLinear) then iShift := 1
                                             else iShift := 3;
      //удаляем точки
      DelPoints(0,iShift);
      setPT(0, PT_MOVETO);
      //удаляем вершину из списка и перестраиваем ссылки на точки
      vtxCount := VertexesCount;
      for i := 0 to vtxCount-2 do begin
        FVertexes[i]:=FVertexes[i+1];
        FVertexes[i].Point := FVertexes[i].Point-iShift;
        if FVertexes[i].LCP > 0 then
          FVertexes[i].LCP := FVertexes[i].LCP-iShift;
        if FVertexes[i].RCP > 0 then
          FVertexes[i].RCP := FVertexes[i].RCP-iShift;
      end;
      setLength(FVertexes,vtxCount-1);
    end
    else begin
      nextVI := nextVertex(vtxIndex);
      if (nextVI < 0) then begin //удаляем последнюю вершину в незамкнутой фигуре
        //вычисляем число удаляемых точек
        if (segmentType[prevVI] = sgmLinear) then iShift := 1
                                             else iShift := 3;
        //удаляем точки
        DelPoints(PointsCount-iShift,iShift);
        //удаляем вершину из списка и перестраиваем ссылки на точки
        vtxCount := VertexesCount;
        setLength(FVertexes,vtxCount-1);
        if (VertexesCount > 0)
           and(FVertexes[VertexesCount-1].RCP > 0)
        then
          FVertexes[VertexesCount-1].RCP :=-1;
      end
      else begin //удаляем промежуточную вершину
        //вычисляем число и базу удаляемых точек
        if (segmentType[prevVI] = sgmLinear)
           and(segmentType[vtxIndex] = sgmLinear)
        then begin
          iShift := 1;
          iBase  := FVertexes[vtxIndex].Point;
        end
        else if (segmentType[prevVI] = sgmLinear) then begin
          iShift := 1;
          iBase  := FVertexes[vtxIndex].Point;
          FVertexes[prevVI].RCP   := FVertexes[vtxIndex].RCP;
          FVertexes[prevVI].VType := vtxAngular;
        end
        else if (segmentType[vtxIndex] = sgmLinear) then begin
          iShift := 1;
          iBase  := FVertexes[vtxIndex].Point;
          if FVertexes[nextVI].Point <> 0 then
            setPT(FVertexes[nextVI].Point, PT_BEZIERTO);
          FVertexes[nextVI].LCP   := FVertexes[vtxIndex].LCP;
          FVertexes[nextVI].VType := vtxAngular;
        end
        else begin
          iShift := 3;
          iBase  := FVertexes[vtxIndex].Point-1;
        end;
        //удаляем точки
        DelPoints(iBase,iShift);
        //удаляем вершину из списка и перестраиваем ссылки на точки
        if (Closed) and FCycled
           and ((vtxIndex = 0) or (vtxIndex = (VertexesCount-1)))
        then begin
          FVertexes[High(FVertexes)].LCP := FVertexes[0].LCP;
          FVertexes[High(FVertexes)].RCP := FVertexes[0].RCP;
        end;
        vtxCount := High(FVertexes)+1;
        for i := vtxIndex to vtxCount-2 do
          FVertexes[i]:=FVertexes[i+1];
        for i := 0 to vtxCount-2 do begin
          if (FVertexes[i].Point > iBase) then
            FVertexes[i].Point := FVertexes[i].Point-iShift;
          if (FVertexes[i].LCP > iBase) then
            FVertexes[i].LCP := FVertexes[i].LCP-iShift;
          if (FVertexes[i].RCP > iBase) then
            FVertexes[i].RCP := FVertexes[i].RCP-iShift;
        end;
        setLength(FVertexes,vtxCount-1);
        if (Closed) and FCycled
           and ((vtxIndex = 0) or (vtxIndex = VertexesCount))
        then begin
          FVertexes[0].LCP := FVertexes[High(FVertexes)].LCP;
          FVertexes[High(FVertexes)].RCP := FVertexes[0].RCP;
        end;
      end;
    end;
    UpdateBounds;
    FActiveVtx :=-1;
    Changed([ocpBounds]);
  end
  else
    raise EVertexException.Create(Format('Invalid vertex (%d)',[vtxIndex]));
end;

function      TdeSimpleCurve.getLeftCP(vtxIndex:integer;var aValue:TPoint2D):boolean;
var
  i : integer;
begin
  i := FVertexes[vtxIndex].LCP;
  Result := (i > 0);
  if Result then
    aValue := Point[i];

  Result := (i > 0);
  if Result then
    aValue := Point[i];
end;

function      TdeSimpleCurve.getRightCP(vtxIndex:integer;var aValue:TPoint2D):boolean;
var
  i : integer;
begin
  i := FVertexes[vtxIndex].RCP;
  Result := (i > 0);
  if Result then
    aValue := Point[i];
end;

procedure     TdeSimpleCurve.setLeftCP(vtxIndex:integer;aValue:TPoint2D);
var
  iL,iR   : integer;
  P,L,R,V : TPoint2D;
  dR       : double;
begin
  iL := FVertexes[vtxIndex].LCP;
  if (iL > 0) then begin
    Changing;
    setPoint(iL, aValue);
    iR := FVertexes[vtxIndex].RCP;
    if (iR > 0) then begin
      P := Point[FVertexes[vtxIndex].Point];
      L := Point[iL];
      R := Point[iR];
      V := Vector2D(L,P);
      if (FVertexes[vtxIndex].VType = vtxDirect) then begin
        dR := VLength2D(Vector2D(P,R));
        try
          V := Norm2D(V);
        except
          V := Point2D(1,1);
        end;
        setPoint(iR, offsetPoint2D(P,V.X*dR,V.Y*dR));
      end
      else if (FVertexes[vtxIndex].VType = vtxSmooth) then begin
        setPoint(iR, offsetPoint2D(P,V.X,V.Y));
      end;
    end;
    UpdateBounds;
    Changed([ocpBounds]);
  end
  else
    raise EVertexException.Create(Format('Vertex has no left CP (%d)',[vtxIndex]));
end;

procedure     TdeSimpleCurve.setRightCP(vtxIndex:integer;aValue:TPoint2D);
var
  iL,iR   : integer;
  P,L,R,V : TPoint2D;
  dR      : double;
begin
  iR := FVertexes[vtxIndex].RCP;
  if (iR > 0) then begin
    Changing;
    setPoint(iR, aValue);
    iL := FVertexes[vtxIndex].LCP;
    if (iL > 0) then begin
      P := point[FVertexes[vtxIndex].Point];
      L := Point[iL];
      R := Point[iR];
      V := Vector2D(R,P);
      if (FVertexes[vtxIndex].VType = vtxDirect) then begin
        dR := VLength2D(Vector2D(P,L));
        try
          V  := Norm2D(V);
          setPoint(iL, offsetPoint2D(P,V.X*dR,V.Y*dR));
        except
        end;
      end
      else if (FVertexes[vtxIndex].VType = vtxSmooth) then begin
        setPoint(iL, offsetPoint2D(P,V.X,V.Y));
      end;
    end;
    UpdateBounds;
    Changed([ocpBounds]);
  end
  else
    raise EVertexException.Create(Format('Vertex has no right CP (%d)',[vtxIndex]));
end;

function      TdeSimpleCurve.getLinearPath(var Points:array of TPoint2D;
                                              var Cnt:integer;
                                                pConv:pMatrix2D = nil):boolean;
var
  P0,P1,P2,P3  : TPoint2D;
  newCnt,i,j   : integer;
  t            : double;
begin
  newCnt := 0;
  if (VertexesCount > 0) then begin
    i  := 0;
    newCnt := 1;
    while (i < SegmentsCount) do begin
      if SegmentType[i] = sgmLinear then
        newCnt := newCnt + 1
      else
        newCnt := newCnt + MAX_BEZIER_POINTS;
      inc(i);
    end;
    Result := (Cnt >= newCnt);

    if Result then
      begin
        Points[0]   := Vertex[0];
        if pConv <> nil then
          Points[0] := MultVM2D(Points[0],pConv^);
        Points[0].W := PT_MOVETO;
        i  := 0;
        newCnt  := 1;
        while (i < SegmentsCount) do
          begin
            getSegmentPoints(i,P0,P1,P2,P3);
            if (SegmentType[i] = sgmLinear) then
              begin
                Points[newCnt] := P3;
                if pConv <> nil then
                  Points[newCnt] := MultVM2D(Points[newCnt],pConv^);
                Points[newCnt].W := FDrawPath[i+1].W;
                inc(newCnt);
              end
            else
              begin
                t := 1/MAX_BEZIER_POINTS;
                for j := 1 to MAX_BEZIER_POINTS do
                  begin
                    Points[newCnt] := Bezier2D(P0,P1,P2,P3,j*t);
                    if pConv <> nil then
                      Points[newCnt] := MultVM2D(Points[newCnt],pConv^);
                    Points[newCnt].W := FDrawPath[i+1].W;
                    inc(newCnt);
                  end;
              end;
            inc(i);
          end;
    end;
  end;
  Result := (Cnt >= newCnt);
  Cnt := newCnt;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('DeSchema2D unit initialization ...');
  {$ENDIF}
  IntObjectClassList := TStringList.Create;
  IntObjectClassList.Sorted := True;
  IntObjectClassList.Duplicates := dupIgnore;
  deRegisterObjectClass(TdeComplexCurve);
  deRegisterObjectClass(TdeSimpleCurve);
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DeSchema2D unit finalization ...');
  {$ENDIF}
  IntObjectClassList.Free;
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

