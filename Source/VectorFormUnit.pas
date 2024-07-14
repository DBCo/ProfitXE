unit VectorFormUnit;

interface

uses Windows, SysUtils, Messages, Classes, Graphics, Controls, ActnList, Actions,
     Buttons, Menus, StdCtrls, ExtCtrls, ComCtrls, ToolWin,
     DeMeta, DSMeta, DataCacheUnit, DataManager, DeDBStorage, DeSchema2D,
     DeSchemaView, BaseGridFormUnit, DePrintStyles, DeReport;

const
  MF_LOADMAP = WM_USER+145;

type
  TVectorViewForm = class(TBaseGridForm)
    LayersActionList: TActionList;
    act_Da_Grid: TAction;
    act_Da_Rulers: TAction;
    act_Da_EditMode: TAction;
    Grid1            : TMenuItem;
    Rulers1          : TMenuItem;
    mi_Dl_Layers: TMenuItem;
    act_Da_ZoomIn: TAction;
    act_Da_ZoomOut: TAction;
    act_Da_Grouping: TAction;
    act_Da_Ungrouping: TAction;
    actGroup1        : TMenuItem;
    actUngroup1      : TMenuItem;
    mmGrid: TMenuItem;
    mmRulers: TMenuItem;
    mmEditMode: TMenuItem;
    actLayer_Da_Create: TAction;
    mm_Dl_ActiveLayer: TMenuItem;
    act_Da_ClearSelection: TAction;
    ItemEditmode: TMenuItem;
    Item_Dl_Scale: TMenuItem;
    ZoomIn1          : TMenuItem;
    ZoomOut2         : TMenuItem;
    act_Da_ZoomNorm: TAction;
    actNormalize1    : TMenuItem;
    actLayer_Da_Delete: TAction;
    act_Da_PointBegin: TAction;
    act_Da_PointEnd: TAction;
    act_Da_PointInsert: TAction;
    act_Da_PointDelete: TAction;
    mdEditMode       : TMenuItem;
    actAddPoint1     : TMenuItem;
    actDeletePoint1  : TMenuItem;
    actEndEditPoints1: TMenuItem;
    act_Da_PointAngular: TAction;
    act_Da_PointDirect: TAction;
    act_Da_PointSmooth: TAction;
    mdEditVertex     : TMenuItem;
    actEditPointsAngularVertex1: TMenuItem;
    actEditPointsDirectVertex1: TMenuItem;
    actEditPointsSmoothVertex1: TMenuItem;
    act_Da_PointLinear: TAction;
    act_Da_PointBezier: TAction;
    actEditPointsLinearSgmnt1: TMenuItem;
    actEditPointsBezierSgmnt1: TMenuItem;
    act_Da_PointClose: TAction;
    act_Da_PointUnclose: TAction;
    actEditPointsClose1: TMenuItem;
    actEditPointsUnclose1: TMenuItem;
    act_Da_MapToFront: TAction;
    act_Da_MapToBack: TAction;
    act_Da_MapCombine: TAction;
    act_Da_MapBreakCurve: TAction;
    actCombine1      : TMenuItem;
    actBreakeCurveApart1: TMenuItem;
    act_Da_Iconssmall: TAction;
    actSmallIcons1   : TMenuItem;
    act_Da_MapEnhanced: TAction;
    Enhanced1        : TMenuItem;
    EnhancedView1    : TMenuItem;
    SmallIcons1      : TMenuItem;
    N4               : TMenuItem;
    N5               : TMenuItem;
    N6               : TMenuItem;
    act_Da_MapCoord: TAction;
    ShowCoordinates1: TMenuItem;
    actLayer_Da_Rename: TAction;
    actBeginEditPoints1: TMenuItem;
    N10: TMenuItem;
    Default1: TMenuItem;
    N11: TMenuItem;
    ClearSelection1: TMenuItem;
    act_dA_RotateFlipH: TAction;
    RotateFlipH1: TMenuItem;
    act_dA_RotateFlipV: TAction;
    RotateFlipV1: TMenuItem;
    act_dA_Rotate90: TAction;
    act_dA_Rotate270: TAction;
    Rotate901: TMenuItem;
    Rotate2701: TMenuItem;
    Item_Da_Rotate: TMenuItem;
    N12: TMenuItem;
    Item_Da_GroupActions: TMenuItem;
    N13: TMenuItem;
    MM_Da_GroupActions: TMenuItem;
    MM_Da_Rotate: TMenuItem;
    RotateFlipH2: TMenuItem;
    Rotate2702: TMenuItem;
    N14: TMenuItem;
    RotateFlipV2: TMenuItem;
    Rotate902: TMenuItem;
    Group1: TMenuItem;
    Ungroup1: TMenuItem;
    N15: TMenuItem;
    actCombine2: TMenuItem;
    actBreakeCurveApart2: TMenuItem;
    MM_Da_Order: TMenuItem;
    BringToFront1: TMenuItem;
    SendToBack1: TMenuItem;
    Item_Da_Order: TMenuItem;
    mm_Dl_Scale: TMenuItem;
    ZoomNorm1: TMenuItem;
    N16: TMenuItem;
    ZoomOut1: TMenuItem;
    ZoomIn2: TMenuItem;
    ClearSelection2: TMenuItem;
    ShowCoordinates2: TMenuItem;
    act_Da_ZoomSelect: TAction;
    ZoomNormSel: TMenuItem;
    actNormSelect: TMenuItem;
    act_Da_ZoomDefault: TAction;
    ZoomDefault1: TMenuItem;
    procedure   miCheckLayerClick(Sender: TObject);
    procedure   miActiveLayerClick(Sender: TObject);
    procedure   miDeleteLayerClick(Sender: TObject);
    procedure   act_Da_ZoomInExecute(Sender: TObject);
    procedure   act_Da_ZoomOutExecute(Sender: TObject);
    procedure   deMapperMouseMove(Sender:TObject;Shift:TShiftState;X,Y:Integer);
    procedure   deMapperDblClick(Sender:TObject);
    procedure   deMapperSelectObject(Sender:tObject;anItem:TdeSchemaObject);
    procedure   deMapperFocusObject(Sender:tObject;anItem:TdeSchemaObject);
    procedure   deMapperDeleteObject(Sender:tObject;anItem:TdeSchemaObject);
    procedure   deMapperObjectChanged(Sender:tObject;anItem:TdeSchemaObject);
    procedure   deMapperObjectNew(Sender:tObject;anItem:TdeSchemaObject);
    
    procedure   act_Da_GridExecute(Sender: TObject);
    procedure   act_Da_RulersExecute(Sender: TObject);
    procedure   act_Da_EditModeExecute(Sender: TObject);
    procedure   act_Da_IconssmallExecute(Sender: TObject);
    procedure   act_Da_MapEnhancedExecute(Sender: TObject);
    procedure   act_CopyExecute(Sender: TObject);
    procedure   act_PasteExecute(Sender: TObject);
    procedure   act_CutExecute(Sender: TObject);
    procedure   act_DeleteExecute(Sender: TObject);
    procedure   act_Da_GroupingExecute(Sender: TObject);
    procedure   act_Da_UngroupingExecute(Sender: TObject);
    procedure   act_SaveExecute(Sender: TObject);
    procedure   actLayer_Da_CreateExecute(Sender: TObject);
    procedure   actLayer_Da_RenameExecute(Sender: TObject);
    procedure   actLayer_Da_DeleteExecute(Sender: TObject);
    procedure   act_AddExecute(Sender: TObject);
    procedure   act_Da_ClearSelectionExecute(Sender: TObject);
    procedure   act_Da_ZoomNormExecute(Sender: TObject);
    procedure   act_Da_PointEndExecute(Sender: TObject);
    procedure   act_Da_PointInsertExecute(Sender: TObject);
    procedure   act_Da_PointDeleteExecute(Sender: TObject);
    procedure   act_Da_PointBeginExecute(Sender: TObject);
    procedure   PopupMenuPopup(Sender: TObject);
    procedure   act_PropertiesExecute(Sender: TObject);
    procedure   act_Da_PointAngularExecute(Sender: TObject);
    procedure   act_Da_PointDirectExecute(Sender: TObject);
    procedure   act_Da_PointSmoothExecute(Sender: TObject);
    procedure   act_Da_PointLinearExecute(Sender: TObject);
    procedure   act_Da_PointBezierExecute(Sender: TObject);
    procedure   act_Da_PointCloseExecute(Sender: TObject);
    procedure   act_Da_PointUncloseExecute(Sender: TObject);
    procedure   act_Da_MapToBackExecute(Sender: TObject);
    procedure   act_Da_MapToFrontExecute(Sender: TObject);
    procedure   act_Da_MapCombineExecute(Sender: TObject);
    procedure   act_Da_MapBreakCurveExecute(Sender: TObject);
    procedure   act_CreateShortcutExecute(Sender: TObject);
    procedure   act_DtInvertselectionExecute(Sender: TObject);
    procedure   act_Da_MapCoordExecute(Sender: TObject);
    procedure   act_dA_RotateFlipHExecute(Sender: TObject);
    procedure   act_dA_RotateFlipVExecute(Sender: TObject);
    procedure   act_dA_Rotate90Execute(Sender: TObject);
    procedure   act_dA_Rotate270Execute(Sender: TObject);
    procedure act_Da_SelectallExecute(Sender: TObject);
    procedure act_Da_ZoomSelectExecute(Sender: TObject);
    procedure act_Da_ZoomDefaultExecute(Sender: TObject);
  private
    { Private declarations }
    FdeMapper     : TdeSchemaView;
    FRulerStyle   : TdeRulerStyle;
    FModeStr      : string;
    FCoordsStr    : string;
    FZoomStr      : string;
    FStatusStr    : string;
    FCalledFromPP : boolean;
    FShortcutImages : TImageList;
    FWhileSave    : boolean;

    FActiveLayer  : integer;
    //FMenuObject   : TObject;
    //
    FOld_Add_Hdlr    : TNotifyEvent;
    FOld_Copy_Hdlr   : TNotifyEvent;
    FOld_Paste_Hdlr  : TNotifyEvent;
    FOld_Cut_Hdlr    : TNotifyEvent;
    FOld_Delete_Hdlr : TNotifyEvent;
    FOld_Properties_Hdlr : TNotifyEvent;
    FOld_Save_Hdlr   : TNotifyEvent;
    FOld_CrShortcut_Hdlr : TNotifyEvent;

    procedure   UpdateLayers;
    function    getMode:TdeSchemaViewMode;
    procedure   setMode(aValue:TdeSchemaViewMode);
    procedure   setMouseCoordsStr(aValue:string);
    procedure   UpdateStatus;
    procedure   ActivateLayer(Index:integer);
    procedure   DeleteLayer(Index:integer);
    procedure   NormalizeBySelected;
    procedure   Normalize;
    procedure   CopySelected;
    procedure   DeleteSelected;
    function    canLoadFromClipboard:boolean;
    procedure   LoadFromClipboard;
    procedure   SelectAll;
    procedure   InvertSelection;
    function    FindShortcut(anObject:TdeSchemaObject;  ShortcutID:integer;
      var ShortcutDSM : TDataSetMeta):TCacheItem;
    function    getShortcutInfo(anObject:TdeSchemaObject;
                                ShortcutID:integer;
                                var aItemID : Variant):TTableMeta;
    procedure   ShowShortcutProps(aShortcut:TdeSchemaShortcut);
  protected
    { Protected declarations }
    FStorage  : TdeSchemaStorage;
    procedure   WM_MouseWheel(var Message: TMSHMouseWheel);  message WM_MOUSEWHEEL;
    procedure   WM_EraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure   InitForm;override;
    procedure   AfterInit;override;
    function    getEnabledPrintStyles:TDePrintStyleTypes;override;
    function    getPrintReport(aStyle : TDePrintStyle):TDeReport;override;
    procedure   MFLoadMap(var Msg: TMessage); message MF_LOADMAP;
    procedure   UpdateShortcuts;
    function    MapChanged:boolean;
  public
    { Public declarations }
    constructor Create(aOwner : TComponent);  override;
    destructor  Destroy;override;
    procedure   DeInitForm;override;
    procedure   ReCreateView; override;
    procedure   ChangeDataSetStatus;override;
    procedure   ZoomIn;
    procedure   ZoomOut;
    procedure   UpdateZoom;
    property    Mode:TdeSchemaViewMode read getMode write setMode;
    property    MouseCoordsStr:string write setMouseCoordsStr;
    property    SchemaView: TdeSchemaView read FdeMapper;
  end;

type
  TdeDSMetaStorage = class(TdeDBStorage)
  protected
    FProgress       : integer;
    FRegistered     : boolean;
    //
    FObjsDSMeta     : TDatasetMeta;
    FObjsDSCache    : TDataCache;
    FObjsIMan       : TDataManager;
    FObjsUMan       : TDataManager;
    FObjsDMan       : TDataManager;
    //
    FObjIndex       : integer;
    //
    FObjs_objID     : integer;
    FObjs_objOwner  : integer;
    FObjs_objClass  : integer;
    FObjs_objParent : integer;
    FObjs_objData   : integer;
    FObjs_objZOrder : integer;
    //
    procedure   setStepsCount(aValue:integer);override;
    procedure   setStep(aValue:integer);override;
    function    OpenDatabase(forWrite:boolean):boolean;override;
    procedure   CloseDatabase;override;
    function    getLoadingObjectsCount:integer;override;
    procedure   InsertObject(var objID:integer;
                             objOwner,objClass,objParent,objZOrder:integer;
                             objData:string);override;
    procedure   UpdateObject(objID,objOwner,objClass,objParent,objZOrder:integer;
                             objData:string);override;
    procedure   DeleteObject(objID:integer);override;
    function    FirstObject(var objID:integer):boolean;override;
    function    NextObject(var objID:integer):boolean;override;
    function    getObjectData(objID:integer;
                              var objOwner,objClass,objParent,objZOrder:integer;
                              var objData:string):integer;override;
    function    getObjectLevel(objID:integer):integer;override;
    //
    procedure   RegisterProgressNotify;
    procedure   unRegisterProgressNotify;
    procedure   doNotifyProgress(Progress:integer);override;
  public
    constructor Create(MapObjects: TDatasetMeta); reintroduce; overload;
    constructor Create(MapObjects: TDataCache); reintroduce; overload;
    destructor  Destroy;override;
    procedure   Load(aSchema:TdeSchema);override;
    procedure   Save(aSchema:TdeSchema);override;
    function    StoreObject(anObject:TObject):integer;
    procedure   DelObject(anObject:TObject);
    function    getObjectID(anObject:TObject):integer;
  end;

implementation

{$R *.dfm}

uses Variants, Forms, {Math, }ClipBrd, {DB, } NetEncoding,
     DeLog, DeTypes, Dictionary, Funcs, G2D, DeSettings, DeMetadata, DeClpBrdStorage, DataUnit, VectorPntPropForm,
     {UnitA, }DeStdReport, DlgRename, DeEMFStorage, BaseMainFormUnit, UnitA;

procedure Error(const sMessage: string);
begin
  SendMessage(Application.MainForm.Handle,
              DM_ERRORNOTIFY,
              icoError,
              NativeInt(PChar(sMessage)));
end;

constructor TdeDSMetaStorage.Create(MapObjects:TDatasetMeta);
begin
  inherited Create;
  FObjsDSMeta  := MapObjects;
  FObjsDSCache := MapObjects.Cache;

  FObjsIMan := nil;
  FObjsUMan   := nil;
  FObjsDMan   := nil;
end;

constructor TdeDSMetaStorage.Create(MapObjects: TDataCache);
begin
  inherited Create;
  FObjsDSMeta  := nil;
  FObjsDSCache := MapObjects;

  FObjsIMan := nil;
  FObjsUMan   := nil;
  FObjsDMan   := nil;
end;

destructor  TdeDSMetaStorage.Destroy;
begin
  inherited Destroy;
end;

function    TdeDSMetaStorage.FirstObject(var objID:integer):boolean;
begin
  FObjIndex := 0;
  Result := (FObjIndex<FObjsDSCache.Count);
  if Result then
    objID  := FObjsDSCache.Items[FObjIndex].FieldValue[FObjs_objID];
end;

function    TdeDSMetaStorage.NextObject(var objID:integer):boolean;
begin
  inc(FObjIndex);
  Result := (FObjIndex<FObjsDSCache.Count);
  if Result then
    objID  := FObjsDSCache.Items[FObjIndex].FieldValue[FObjs_objID];
end;

function    TdeDSMetaStorage.getObjectData(objID:integer;
                                           var objOwner,objClass,objParent,objZOrder:integer;
                                           var objData:string):integer;
var
  CI     : TCacheItem;
  sData  : string;
//  Buffer : string;
//  l      : integer;
begin
  Result := -1;
  if (FObjsDSCache=nil) then exit;
  CI := FObjsDSCache.Items[FObjIndex];
  if (CI.FieldValue[FObjs_objID] <> objID) then
    CI := FObjsDSCache.FindById(objID);
  if CI = nil then
    Result :=-1
  else begin
    if (CI.FieldValue[FObjs_objOwner] = NULL) then
      objOwner  :=-1
    else
      objOwner  := CI.FieldValue[FObjs_objOwner];
    if (CI.FieldValue[FObjs_objClass] = NULL) then
      objClass  :=-3
    else
      objClass  := CI.FieldValue[FObjs_objClass];
    if (CI.FieldValue[FObjs_objParent] = NULL) then
      objParent  :=-1
    else
      objParent := CI.FieldValue[FObjs_objParent];
    if (CI.FieldValue[FObjs_objZOrder] = NULL) then
      objZOrder  := 0
    else
      objZOrder := CI.FieldValue[FObjs_objZOrder];
    sData   := CI.FieldValue[FObjs_objData];
    if (sData <> '')and(
    sData[1] = 'A')
    then begin
      system.Delete(sData,1,1);
      ObjData := TBase64Encoding.Base64.Decode(sData);
      {
      Buffer := ' ';
      l := 0;
      if not Base64Decode(sData,@(Buffer[1]),l)
      then begin
        setLength(Buffer,l+1);
        Base64Decode(sData,@(Buffer[1]),l);
      end;
      objData := copy(Buffer,1,l);
      }
    end
    else
      Result :=-1;
  end;
end;

function    TdeDSMetaStorage.getObjectLevel(objID:integer):integer;
var
  objOwner,objClass,objParent,objZOrder : integer;
  objData : string;
begin
  if (objID >= 0)
     and(getObjectData(objID,objOwner,objClass,objParent,objZOrder,objData) >= 0)
  then
    Result := getObjectLevel(objParent)+1
  else
    Result :=-3;
end;

procedure   TdeDSMetaStorage.UpdateObject(objID,objOwner,objClass,objParent,objZOrder:integer;
                                          objData:string);
var CI : TCacheItem;
    s  : string;
begin
  if (FObjsDSCache=nil) then exit;

  CI := FObjsDSCache.FindById(objID);
  if CI=nil then
    CI := FObjsDSCache.AddNewItem;
  if (objData <> '') then
    s := 'A' + TBase64Encoding.Base64.Encode(ObjData)//Base64Encode(@(objData[1]),length(objData))
  else
    s := '';
  {
  try
    F := varSameValue(CI.FieldValue[FObjs_objParent],objParent)
         or(varSameValue(CI.FieldValue[FObjs_objZOrder],objZOrder))
         or(varSameValue(CI.FieldValue[FObjs_objData],s));
  except
    F := false;
  end;
  }
  if true{not F} then
  begin
    CI.FieldValue[FObjs_objClass]  := objClass;
    CI.FieldValue[FObjs_objParent] := objParent;
    CI.FieldValue[FObjs_objZOrder] := objZOrder;
    CI.FieldValue[FObjs_objData]   := s;
    if FObjsDSMeta<>nil then
      FObjsDSMeta.SetReferenceKeys(CI);
    if not FObjsUMan.UpdateRecord(CI)
    then
      Error(FObjsUMan.Errors.GetMessage);
  end;
end;

procedure   TdeDSMetaStorage.InsertObject(var objID:integer;
                                          objOwner,objClass,objParent,objZOrder:integer;
                                          objData:string);
var
  CI   : TCacheItem;
  s    : string;
begin
  if (FObjsDSCache=nil) then exit;

  CI := FObjsDSCache.AddNewItem;
  if (objData <> '') then
    s := 'A' + TBase64Encoding.Base64.Encode(ObjData) //Base64Encode(@(objData[1]),length(objData))
  else
    s := EmptyStr;
  CI.FieldValue[FObjs_objData]   := s;
  CI.FieldValue[FObjs_objID]     := objID;
  CI.FieldValue[FObjs_objClass]  := objClass;
  CI.FieldValue[FObjs_objParent] := objParent;
  CI.FieldValue[FObjs_objZOrder] := objZOrder;
  if (FObjsDSMeta<>nil) then
    FObjsDSMeta.SetReferenceKeys(CI);
  FObjsIMan.PrepareRecord(CI);
  if FObjsIMan.CanInsertRecord(CI)
     and FObjsIMan.InsertRecord(CI)
  then
    objID := VarToInt(CI.FieldValue[CI.Owner.Fields.IndexByID(CI.Owner.TableMeta.KField[0].ID)])
  else
    Error(FObjsIMan.Errors.GetMessage);
end;

procedure   TdeDSMetaStorage.DeleteObject(objID:integer);
var
  CI   : TCacheItem;
begin
  if (FObjsDSCache=nil) then exit;

  CI := FObjsDSCache.FindById(objID);
  if CI=nil then exit;

  if FObjsDMan.canDelete(CI) and FObjsDMan.DeleteRecord(CI)
  then
  else
    Error(FObjsDMan.Errors.GetMessage);
end;

procedure   TdeDSMetaStorage.setStepsCount(aValue:integer);
begin
  if aValue<=0 then
    aValue := 1;
  inherited setStepsCount(aValue);
end;

procedure   TdeDSMetaStorage.setStep(aValue:integer);
var
  newProgress : integer;
begin
  inherited setStep(aValue);
  newProgress := (Step * 100 div StepsCount) mod 100;
  if newProgress<>FProgress then
    doNotifyProgress(newProgress);
  FProgress := newProgress;
end;

function    TdeDSMetaStorage.OpenDatabase(forWrite:boolean):boolean;
begin
  Result := inherited OpenDatabase(forWrite);
  if not Result then exit;
  try
    with FObjsDSCache do
    begin
      FObjs_objID     := Fields.IndexByName('objID');
      FObjs_objOwner  := Fields.IndexByName('objOwner');
      FObjs_objClass  := Fields.IndexByName('objClass');
      FObjs_objParent := Fields.IndexByName('objParent');
      FObjs_objZOrder := Fields.IndexByName('objZOrder');
      FObjs_objData   := Fields.IndexByName('objData');
    end;
    if forWrite then
    begin
      if (FObjsDSCache<>nil) then
      begin
        FObjsIMan := CreateDataManager(FObjsDSCache.TableMeta);
        FObjsUMan := CreateDataManager(FObjsDSCache.TableMeta);
        FObjsDMan := CreateDataManager(FObjsDSCache.TableMeta);
      end;
    end;
    Result := true;
  except
    Result := false;
  end;
end;

procedure   TdeDSMetaStorage.CloseDatabase;
begin
  if FObjsIMan<>nil then FObjsIMan.Free;
  FObjsIMan   := nil;
  if FObjsUMan<>nil then FObjsUMan.Free;
  FObjsUMan   := nil;
  if FObjsDMan<>nil then FObjsDMan.Free;
  FObjsDMan := nil;
  inherited CloseDatabase;
end;

function    TdeDSMetaStorage.getLoadingObjectsCount:integer;
begin
  Result := FObjsDSCache.Count;
end;

procedure   TdeDSMetaStorage.RegisterProgressNotify;
begin
  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY,100,1);
  FRegistered := true;
end;

procedure   TdeDSMetaStorage.unRegisterProgressNotify;
begin
  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, -1, -1);
  FRegistered := false;
end;

procedure   TdeDSMetaStorage.DoNotifyProgress(Progress:integer);
begin
  if FRegistered then
    SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY,Progress,0);
end;

procedure   TdeDSMetaStorage.Load(aSchema:TdeSchema);
begin
  RegisterProgressNotify;
  inherited Load(aSchema);
  unRegisterProgressNotify;
end;

procedure   TdeDSMetaStorage.Save(aSchema:TdeSchema);
begin
  RegisterProgressNotify;
  MetaData.BeginUpdate;
  FObjsDSCache.BeginUpdate;
  inherited Save(aSchema);
  FObjsDSCache.EndUpdate;
  MetaData.EndUpdate;
  unRegisterProgressNotify;
end;

function    TdeDSMetaStorage.StoreObject(anObject:TObject):integer;
var
  objID : integer;
begin
  objID := getObjectID(anObject);
  if objID >=0 then
    MarkSaved(objID,false);
  Result := inherited SaveObject(anObject);
end;

procedure   TdeDSMetaStorage.DelObject(anObject:TObject);
var
  objID,ownerID : integer;
begin
  objID := getIndex2(anObject,ownerID);
  if objID >= 0 then begin
    DeleteObject(objID);
    delIndexes(objID);
  end;
end;

function    TdeDSMetaStorage.getObjectID(anObject:TObject):integer;
var
  ownerID : integer;
begin
  Result := getIndex2(anObject,ownerID);
end;
     


constructor TVectorViewForm.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  //
  FOld_Add_Hdlr        := act_Da_Create.OnExecute;
  FOld_Copy_Hdlr       := act_Da_Copy.OnExecute;
  FOld_Paste_Hdlr      := act_Da_Paste.OnExecute;
  FOld_Cut_Hdlr        := act_Da_Cut.OnExecute;
  FOld_Delete_Hdlr     := act_Da_Delete.OnExecute;
  FOld_Properties_Hdlr := act_Da_Open.OnExecute;
  FOld_Save_Hdlr       := act_Da_Save.OnExecute;
  FOld_CrShortcut_Hdlr := actCreate_Da_Shortcut.OnExecute;
  act_Da_Create.OnExecute := act_AddExecute;
  act_Da_Copy.OnExecute   := act_CopyExecute;
  act_Da_Paste.OnExecute  := act_PasteExecute;
  act_Da_Cut.OnExecute    := act_CutExecute;
  act_Da_Delete.OnExecute := act_DeleteExecute;
  act_Da_Open.OnExecute   := act_PropertiesExecute;
  act_Da_Save.OnExecute   := act_SaveExecute;
  actCreate_Da_Shortcut.OnExecute := act_CreateShortcutExecute;
  //
  FRulerStyle := rsWordlike;
  FdeMapper := TdeSchemaView.Create(Self);
  FdeMapper.BeginUpdate;
  try
    FdeMapper.Parent        := pnlGrid;
    FdeMapper.Align         := alClient;
    FdeMapper.ParentColor   := true;
    FdeMapper.RulerStyle    := FRulerStyle;
    FdeMapper.Schema.ShortcutFont := Font;
    FdeMapper.Options       := [voGrid,voSmoothing];
    FdeMapper.OnMouseMove   := deMapperMouseMove;
    FdeMapper.OnDblClick    := deMapperDblClick;
    FdeMapper.OnFocusObject   := deMapperFocusObject;
    FdeMapper.OnSelectObject  := deMapperSelectObject;

    FdeMapper.onNewObject     := deMapperObjectNew;
    FdeMapper.OnDeleteObject  := deMapperDeleteObject;
    FdeMapper.OnObjectChanged := deMapperObjectChanged;
    FdeMapper.PopupMenu       := PopupMenu;
    Mode := vmBrowse;
    UpdateLayers;
    UpdateZoom;
  finally
    FdeMapper.EndUpdate;
  end;

  //FMenuObject := nil;
  
  act_Da_Grid.Checked     := (voGrid in FdeMapper.Options);
  act_Da_Rulers.Checked   := (FdeMapper.RulerStyle<>deSchemaView.rsNone);
  act_Da_EditMode.Checked := (Mode=vmEdit);
  FStorage      := nil;
  FCalledFromPP := false;
  ActiveControl := FdeMapper;
end;

destructor  TVectorViewForm.Destroy;
begin
  //FMenuObject := nil;
  if (FStorage<>nil) then
    FStorage.Free;
  act_Da_Create.OnExecute   := FOld_Add_Hdlr;
  act_Da_Copy.OnExecute     := FOld_Copy_Hdlr;
  act_Da_Paste.OnExecute    := FOld_Paste_Hdlr;
  act_Da_Cut.OnExecute      := FOld_Cut_Hdlr;
  act_Da_Delete.OnExecute   := FOld_Delete_Hdlr;
  act_Da_Open.OnExecute     := FOld_Properties_Hdlr;
  act_Da_Save.OnExecute     := FOld_Save_Hdlr;
  actCreate_Da_Shortcut.OnExecute := FOld_CrShortcut_Hdlr;
  inherited Destroy;
end;

procedure   TVectorViewForm.WM_EraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1000;
end;

procedure   TVectorViewForm.InitForm;
begin
  act_Da_ZoomIn.ShortCut := VK_ADD;
  act_Da_ZoomOut.ShortCut := VK_SUBTRACT;
  act_Da_ZoomNorm.ShortCut := VK_MULTIPLY;
//  act_Da_ZoomSelect.ShortCut := VK_SEPARATOR;

  //FMenuObject := nil;
  if DataSetMeta=nil then exit;
  CanGrouping := true;
  CanSorting  := false;
  ViewParams.SetByName('VectorScale','Auto');

  inherited;

  CanShowCard := False;
  act_Da_ZoomDefault.Visible:=Not ViewParams.TestByName('VectorScale','Auto');
  act_Da_ZoomDefault.Caption:=GetTitle('_Da.Default') + ' ('+ViewParams.GetValueByName('VectorScale') + '%)';

  if not DataSetMeta.Cache.Active then DataSetMeta.Cache.OpenData;

  FdeMapper.BeginUpdate;
  try
    FdeMapper.Schema.Clear;

    //..........................................................................
    act_Da_Rulers.Visible := true;
    if ViewParams.TestByName('VectorRulesType','Adobe') then
      FRulerStyle := rsScholl
    else
      FRulerStyle := rsWordlike;

    if ViewParams.TestByName('VectorRules','False') then
      FdeMapper.RulerStyle := deSchemaView.rsNone
    else
      FdeMapper.RulerStyle := FRulerStyle;
    act_Da_Rulers.Checked := (FdeMapper.RulerStyle <> deSchemaView.rsNone);

    //..........................................................................
    act_Da_Grid.Visible := true;
    if ViewParams.TestByName('VectorGrid','False') then
      FdeMapper.Options := FdeMapper.Options - [voGrid]
    else
      FdeMapper.Options := FdeMapper.Options + [voGrid];
    act_Da_Grid.Checked := (voGrid in FdeMapper.Options);

    //..........................................................................
    act_Da_Iconssmall.Visible := true;
    act_Da_Iconssmall.Checked := ViewParams.TestByName('VectorIcon','Small');
    if act_Da_Iconssmall.Checked then
      FShortcutImages := DataUnit.DM.ilIcon16{swImages}
    else
      FShortcutImages := DataUnit.DM.ilIcon32{ImagesWhite};

    //..........................................................................
    act_Da_MapCoord.Visible := true;
    act_Da_MapCoord.Checked := ViewParams.TestByName('VectorCoord','True');
    if act_Da_MapCoord.Checked then
      FdeMapper.ShowCoordinates:=vcBoth
    else
      FdeMapper.ShowCoordinates:=vcNone;

    //..........................................................................
    act_Da_MapEnhanced.Visible := true;
    act_Da_MapEnhanced.Checked := Not ViewParams.TestByName('VectorBest','False');
    if act_Da_MapEnhanced.Checked then
      FdeMapper.Options := FdeMapper.Options + [voSmoothing]
    else
      FdeMapper.Options := FdeMapper.Options - [voSmoothing];

    //..........................................................................
{    if ViewParams.TestByName('VectorScroll','False') then
      FdeMapper.Options := FdeMapper.Options - [voScrollBars]
    else
      FdeMapper.Options := FdeMapper.Options + [voScrollBars];  {}

    //..........................................................................
   act_Da_EditMode.Enabled := not(DataSetMeta.Table.IsReadOnly);
    {    //TODO: Пришлось вынести в AfterInit т.к. нельзя вызывать
         ChangeDatasetStatus из InitForm - неверно обновляется главное меню!!!
    if DataSetMeta.Table.IsReadOnly then
      setMode(vmBrowse)
    else if ViewParams.TestByName('VectorEdit','True') then
      setMode(vmEdit)
    else
      setMode(vmBrowse);  {}
  finally
    FdeMapper.EndUpdate;
  end;
end;

procedure   TVectorViewForm.AfterInit;
begin
  inherited;
  {
  FdeMapper.BeginUpdate;
  try
    if DataSetMeta.Table.IsReadOnly then
      setMode(vmBrowse) else
    if ViewParams.TestByName('VectorEdit','True') then
      setMode(vmEdit)
    else
      setMode(vmBrowse);
  finally
    FdeMapper.EndUpdate;
  end;
  {}
end;

procedure   TVectorViewForm.DeInitForm;
begin
  { //TODO:
  if (DatasetMeta <> nil) then
  begin
    if MapChanged
       and act_Save.Enabled
       and (Application.MessageBox(PChar(GetTitle('_qUery.savechanges')),
                                   PChar(GetTitle('_Dl.Confirmation')),
                                   MB_ICONEXCLAMATION or MB_YESNO) = IDYES)
    then
      act_Save.Execute;
  end;
  }
  inherited DeInitForm;
end;

procedure   TVectorViewForm.ReCreateView;
begin
  if Assigned(DataSetMeta) then
    PostMessage(Handle,MF_LOADMAP,0,0);
  inherited;
end;

function    TVectorViewForm.getEnabledPrintStyles:TDePrintStyleTypes;
begin
  Result := [PS_Chart_TypeID];
end;

function    TVectorViewForm.getPrintReport(aStyle : TDePrintStyle):TDeReport;
var sFile: String;
    FMetaFile : TMetaFile;
begin
  if aStyle.TypeID = PS_Chart_TypeID then
  begin
    sFile:= DeGetTempFileName('','DEI', sExtensionWMF);
    DM.RegisterTemporaryFile(sFile);
    FMetaFile := TMetaFile.Create;

    with TdeEMFStorage.Create(FMetaFile) do try
      Save(FDeMapper.Schema);
    finally
      Free;
    end;
    FMetaFile.SaveToFile(sFile);
    FMetaFile.Free;

    Result := TDeStdGraphicReport.Create;
    TDeStdGraphicReport(Result).Style:= aStyle;
    TDeStdGraphicReport(Result).Title:= getCaptionText;
    TDeStdGraphicReport(Result).FileName:= sFile;
  end
  else
    Result := inherited getPrintReport(aStyle);
end;

procedure   TVectorViewForm.MFLoadMap(var Msg: TMessage);
var aScale : Double;
begin
  if Not Assigned(DatasetMeta) then Exit;

  FdeMapper.BeginUpdate;
  try
    FdeMapper.onNewObject     := nil;
    FdeMapper.onDeleteObject  := nil;
    FdeMapper.onObjectChanged := nil;
    FdeMapper.Schema.Clear;

    if (FStorage<>nil) then
      FStorage.Free;

    DatasetMeta.Cache.ClearSelection;
    FStorage  := TdeDSMetaStorage.Create(DatasetMeta);
    try
      FStorage.Load(FdeMapper.Schema);
    except
    end;
    DatasetMeta.Cache.ClearSelection;
    UpdateShortcuts;

    if not FWhileSave then
    begin
      if ViewParams.TestByName('VectorScale','Auto') then
        begin
          FdeMapper.Normalize;
          UpdateZoom;
        end
      else
        begin
          aScale:=StrToFloatDef(ViewParams.GetValueByName('VectorScale'),-1);
          if aScale>=0 then
            begin
              FdeMapper.Scale:=aScale/100;
              UpdateZoom;
              FdeMapper.Centeralize;
            end;
        end;

      if DataSetMeta.Table.IsReadOnly then
        setMode(vmBrowse)
      else if ViewParams.TestByName('VectorEdit','True') then
        setMode(vmEdit)
      else
        setMode(vmBrowse);
    end;
    FWhileSave := false;

  finally
    FdeMapper.EndUpdate;
  end;

  UpdateLayers;

  CacheItems.FocusedItem := nil;

  FdeMapper.onNewObject     := deMApperObjectNew;
  FdeMapper.onDeleteObject  := deMapperDeleteObject;
  FdeMapper.onObjectChanged := deMApperObjectChanged;

//  if (Visible and Enabled) then setFocus;
  ChangeDataSetStatus;
end;

procedure   TVectorViewForm.UpdateShortcuts;
begin
  FdeMapper.BeginUpdate;
  try
    FdeMapper.Schema.ShortcutImages := FShortcutImages;
    FdeMapper.RefreshView;
  finally
    FdeMapper.EndUpdate;
  end;
end;

function    TVectorViewForm.MapChanged:boolean;
begin
  Result := true;
end;

procedure   TVectorViewForm.ChangeDataSetStatus;
var IsEdit, IsSelect, IsSingle : Boolean;
    I : Integer;
begin
  inherited;
  IsEdit   := (Mode = vmEdit);
  IsSelect := (FdeMapper.SelectedCount > 0);
  IsSingle := (FdeMapper.SelectedCount = 1);

  act_Da_Create.Enabled := IsEdit {and(FActiveLayer >= 0)};
  act_Da_Copy.Enabled   := IsSelect;
  act_Da_Paste.Enabled  := IsEdit and canLoadFromClipBoard;
  act_Da_Cut.Enabled    := IsEdit and IsSelect;
  act_Da_Delete.Enabled := (IsEdit and IsSelect) or
                           (FdeMapper.ActiveShortcut <> nil);
  act_Da_Open.Enabled   := (FdeMapper.FocusedItem <> nil) or
                           (FdeMapper.ActiveShortcut <> nil);

  act_Da_RotateFlipH.Enabled := IsEdit and IsSelect;
  act_Da_RotateFlipV.Enabled := IsEdit and IsSelect;
  act_Da_Rotate90.Enabled    := IsEdit and IsSelect;
  act_Da_Rotate270.Enabled   := IsEdit and IsSelect;

  act_Da_Selectall.Enabled       := true;
  act_Da_Invertselection.Enabled := true;
  act_Da_Clearselection.Visible  := true;
  
  act_Da_Print.Enabled := true;
  act_Da_FastPrint.Enabled := True;

  act_Da_MapBreakCurve.Enabled := IsEdit and IsSingle and (FdeMapper.Selected[0] is TdeComplexCurve);

  act_Da_MapCombine.Enabled := IsEdit and (FdeMapper.SelectedCount > 1);
  i := 0;
  while act_Da_MapCombine.Enabled and (i<FdeMapper.SelectedCount) do
  begin
    act_Da_MapCombine.Enabled := (FdeMapper.Selected[i] is TdeCustomCurve);
    inc(i);
  end;

  act_Da_MapToFront.Enabled := IsEdit and IsSingle and (FdeMapper.Selected[0].Index<(FdeMapper.Schema.RootCount-1));
  act_Da_MapToBack.Enabled  := IsEdit and IsSingle and (FdeMapper.Selected[0].Index>0);

  act_Da_Grouping.Enabled   := IsEdit and (FdeMapper.canGroup);
  act_Da_UnGrouping.Enabled := IsEdit and (FdeMapper.canUngroup);
end;

procedure   TVectorViewForm.deMapperSelectObject(Sender:tObject; anItem:TdeSchemaObject);
var objID : integer;
    CI    : TCacheItem;
begin
  objID := TdeDSMetaStorage(FStorage).getObjectID(anItem);
  CI := CacheItems.FindById(objID);
  if Assigned(CI) then
    CI.Selected := (FdeMapper.SelectedIndex(anItem) >= 0)
  else
    CacheItems.ClearSelection;
end;

procedure   TVectorViewForm.deMapperFocusObject(Sender:TObject; anItem:TdeSchemaObject);
var objID : integer;
begin
  if FStorage=nil then exit;
  objID := TdeDSMetaStorage(FStorage).getObjectID(anItem);
  CacheItems.FocusedItem := CacheItems.FindById(objID);
end;

procedure   TVectorViewForm.deMapperDeleteObject(Sender:tObject;anItem:TdeSchemaObject);
var objID : integer;
    CI    : TCacheItem;
begin
  if FStorage=nil then exit;
  objID := TdeDSMetaStorage(FStorage).getObjectID(anItem);
  CI := CacheItems.FindById(objID);
  if (CI<>nil) and CI.Focused then
    CacheItems.FocusedItem := nil;

  if Variables.AsBoolean[RegAutoSaveChanges] then
    with TdeDSMetaStorage(FStorage) do
      begin
        OpenDatabase(true);
        DelObject(anItem);
        CloseDatabase;
      end;

  DataSetMeta.UpdateVisualizationForm;
end;

procedure   TVectorViewForm.deMapperObjectChanged(Sender:tObject;anItem:TdeSchemaObject);
var  objID : integer;
begin
  if FStorage=nil then exit;

  if Variables.AsBoolean[RegAutoSaveChanges] then
    with TdeDSMetaStorage(FStorage) do
      begin
        OpenDatabase(true);
        objID := StoreObject(anItem);
        CloseDatabase;
      end;

  DataSetMeta.UpdateVisualizationForm;
end;

procedure   TVectorViewForm.deMapperObjectNew(Sender:tObject;anItem:TdeSchemaObject);
{
var
  objID : integer; {}
begin
  {
  if FStorage=nil then exit;
  with TdeDSMetaStorage(FStorage) do begin
    OpenDatabase(true);
    objID := StoreObject(anItem);
    CloseDatabase;
  end; {}
end;

procedure   TVectorViewForm.miCheckLayerClick(Sender: TObject);
var L  : TdeSchemaLayer;
    AC : TAction;
begin
  if Sender is TAction then begin
    AC := TAction(Sender);
    AC.Checked := not(AC.Checked);
    L := FdeMapper.Schema.Layer[AC.Tag];
    if (L<>nil) then begin
      L.Visible := AC.Checked;
      if (AC.Tag = FActiveLayer) then
        setMode(Mode)
      else
        UpdateLayers;
    end;
    FdeMapper.UpdateView;
  end;
end;

procedure   TVectorViewForm.miActiveLayerClick(Sender: TObject);
var AC : TAction;
begin
  if Sender is TAction then begin
    AC := TAction(Sender);
    ActivateLayer(AC.Tag);
  end;
end;

procedure   TVectorViewForm.miDeleteLayerClick(Sender: TObject);
var AC : TAction;
begin
  if Sender is TAction then begin
    AC := TAction(Sender);
    DeleteLayer(AC.Tag);
  end;
end;


procedure   TVectorViewForm.UpdateLayers;
var
  i   : integer;
  AC  : TAction;
  MI  : TMenuItem;
  L   : TdeSchemaLayer;
  LName : string;
begin
  while LayersActionList.ActionCount>0 do
    LayersActionList.Actions[0].Free;
  mi_Dl_Layers.Clear;
  mm_Dl_ActiveLayer.Clear;
  i := 0;
  while i < FdeMapper.Schema.LayersCount do
    begin
      L := FdeMapper.Schema.Layer[i];
      LName := L.Name;
      L.ReadOnly := (Mode=vmEdit)and(i<>FActiveLayer);
      AC := TAction.Create(Self);
      AC.Category  := 'Layers';
      AC.Name      := 'act_Layer'+trim(IntToStr(i));
      AC.Caption   := GetTitle(LName);
      AC.Hint      := AC.Caption;
      AC.Checked   := L.Visible;
      AC.Tag       := i;
      AC.OnExecute := miCheckLayerClick;
      AC.ActionList := LayersActionList;
      AC.ImageIndex := i;
      MI := TMenuItem.Create(Self);
      MI.Action := AC;
      mi_Dl_Layers.Add(MI);
      //
      AC := TAction.Create(Self);
      AC.Category  := 'Layers';
      AC.Name      := 'act_ActLayer'+trim(IntToStr(i));
      AC.Caption   := GetTitle(LName);
      AC.Hint      := AC.Caption;
      AC.Checked   := (i=FActiveLayer);
      AC.GroupIndex:= 1;
      AC.Tag       := i;
      AC.Enabled   := L.Visible;
      AC.OnExecute := miActiveLayerClick;
      AC.ActionList := LayersActionList;
      AC.ImageIndex := i;
      MI := TMenuItem.Create(Self);
      MI.Action := AC;
      mm_Dl_ActiveLayer.Add(MI);
      //
      inc(i);
    end;
  MI := TMenuItem.Create(Self);
  MI.Caption := cLineCaption;
  mm_Dl_ActiveLayer.Add(MI);
  MI := TMenuItem.Create(Self);
  MI.Action := actLayer_Da_Create;
  mm_Dl_ActiveLayer.Add(MI);

  MI := TMenuItem.Create(Self);
  MI.Action := actLayer_Da_Rename;
  mm_Dl_ActiveLayer.Add(MI);

  MI := TMenuItem.Create(Self);
  MI.Action := actLayer_Da_Delete;
  mm_Dl_ActiveLayer.Add(MI);
end;

procedure   TVectorViewForm.UpdateZoom;
var ds,ts : char;
begin
  ds := FormatSettings.DecimalSeparator;
  ts := FormatSettings.ThousandSeparator;
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := #0;
  FZoomStr := FormatFloat('0.###',FdeMapper.Zoom)+'%';
  FormatSettings.DecimalSeparator := ds;
  FormatSettings.ThousandSeparator := ts;
  UpdateStatus;
end;

function    TVectorViewForm.getMode:TdeSchemaViewMode;
begin
  Result := FdeMapper.Mode;
end;

procedure   TVectorViewForm.setMode(aValue:TdeSchemaViewMode);
var i : integer;
begin
  FdeMapper.Mode := aValue;
  FActiveLayer := -1;
  mm_Dl_ActiveLayer.Visible := (FdeMapper.Mode = vmEdit);
  if (FdeMapper.Mode = vmEdit) then begin
    FModeStr       := getTitle('_Da.edit');
    with FdeMapper do begin
      for i:=0 to Schema.LayersCount-1 do begin
        if FActiveLayer<0 then
          FActiveLayer := i;
        Schema.Layer[i].ReadOnly := (i <> FActiveLayer);
      end;
    end;
  end
  else begin
    FModeStr       := getTitle('_Da.preview');
  end;
  FdeMapper.UpdateView;
  UpdateLayers;
  UpdateStatus;
  act_Da_EditMode.Checked := (FdeMapper.Mode = vmEdit);
  ChangeDataSetStatus;
end;
  
procedure   TVectorViewForm.setMouseCoordsStr(aValue:string);
begin
  FCoordsStr := aValue;
  UpdateStatus;
end;

procedure   TVectorViewForm.UpdateStatus;
begin
  FStatusStr := FModeStr+' '+FZoomStr+' '+FCoordsStr;

  if FdeMapper.FocusedItem<>nil then
    if FdeMapper.IsUpdating then
    begin
      if assigned(FdeMapper.EditedItem) then
        FStatusStr:= IntToName(FdeMapper.EditedItem.ActiveVertex)+' '+FCoordsStr
      else
        with FdeMapper.FocusedItem do
        FStatusStr:= Format('w:%1.1f h:%1.1f a:%1.0f',[Width,Height,Rotation*180/Pi]);
    end;

  SendMessage(Application.MainForm.Handle, DM_STATUSNOTIFY, NativeInt(FStatusStr), Handle);
end;

procedure   TVectorViewForm.ActivateLayer(Index:integer);
var
  L  : TdeSchemaLayer;
begin
  L := FdeMapper.Schema.Layer[Index];
  if (L<>nil) then begin
    FdeMapper.ClearSelection;
    FActiveLayer := Index;
    UpdateLayers;
  end;
  FdeMapper.UpdateView;
  PopupMenu.OnPopup(nil);
end;

procedure   TVectorViewForm.DeleteLayer(Index:integer);
var
  L  : TdeSchemaLayer;
begin
  if Index<0 then exit;
  with FdeMapper do begin
    BeginUpdate;
    try
      L := Schema.Layer[Index];
      if (L<>nil) then begin
        L.Delete;
        if (Index=FActiveLayer) then
          setMode(Mode)
        else
          UpdateLayers;
      end;
    finally
      UpdateView;
      EndUpdate;
    end;
  end;
end;

procedure   TVectorViewForm.NormalizeBySelected;
var i   : Integer;
    R,R2: TRect2D;
begin
  if FdeMapper.SelectedCount=0 then
    FdeMapper.Normalize
  else
    begin
      R:=FdeMapper.Selected[0].SchemaBounds;
      for i:=1 to FdeMapper.SelectedCount-1 do
        begin
          R2 := FdeMapper.Selected[i].SchemaBounds;
          R  := ExpandRect2D(R,R2.TopLeft);
          R  := ExpandRect2D(R,R2.BottomRight);
        end;
      FdeMapper.NormalizeByRect(R);
    end;
  UpdateZoom;
end;

procedure   TVectorViewForm.Normalize;
begin
  FdeMapper.Normalize;
  UpdateZoom;
end;

procedure   TVectorViewForm.CopySelected;
var
  aSchema  : TdeSchema;
  anItem   : TdeSchemaObject;
  aMemento : TdeSchemaObjectMemento;
begin
  aSchema := TdeSchema.Create;
  try
    //создаем временную схему и копируем в нее выделенные элементы
    anItem := FdeMapper.Schema.First;
    while (anItem<>nil) do begin
      if (FdeMapper.SelectedIndex(anItem) >= 0)
      then begin
        aMemento := anItem.CreateMemento;
        try
          aMemento.CloneObject(aSchema,nil,anItem.LayerName);
        finally
          aMemento.Free;
        end;
      end;
      anItem := FdeMapper.Schema.Next(anItem);
    end;
    //"отрисовываем" временную схему в буфере обмена
    with TdeClipboardStorage.Create do try
      Save(aSchema);
    finally
      Free;
    end;
  finally
    aSchema.Free;
  end;
end;

procedure   TVectorViewForm.DeleteSelected;
begin
  with FdeMapper do begin
    BeginUpdate;
    try
      while SelectedCount > 0 do
        Selected[0].Delete;
    finally
      EndUpdate;
    end;
  end;
end;

function    TVectorViewForm.canLoadFromClipboard:boolean;
begin
  Result := Clipboard.HasFormat(CF_ENHMETAFILE)
            or Clipboard.HasFormat(CF_PROFITSCHEMA);
end;

procedure   TVectorViewForm.LoadFromClipboard;
var
  aSchema  : TdeSchema;
  VR       : TRect;
  R        : TRect2D;
  W,H      : integer;
begin
  FdeMapper.BeginUpdate;
  try
    VR := FdeMapper.ViewRect;
    W := VR.Right-VR.Left;
    H := VR.Bottom-VR.Top;
    InflateRect(VR,-(W div 3),-(H div 3));
    R.TopLeft := FdeMapper.ViewToSchema(VR.TopLeft);
    R.BottomRight := FdeMapper.ViewToSchema(VR.BottomRight);
    aSchema  := TdeSchema.Create;
    try
      with TdeClipboardStorage.Create do try
        Load(aSchema);
      finally
        Free;
      end;
      if Clipboard.HasFormat(CF_PROFITSCHEMA) then
        FdeMapper.Schema.AddFrom(aSchema,R,smAutoSize)
      else
        FdeMapper.Schema.AddFrom(aSchema,R,smProportional);
    finally
      aSchema.Free;
    end;
  finally
    FdeMapper.EndUpdate;
  end;
end;

procedure   TVectorViewForm.SelectAll;
var
  anItem : TdeSchemaObject;
begin
  with FdeMapper do begin
    BeginUpdate;
    try
      anItem := Schema.First;
      while (anItem<>nil) do begin
        if (SelectedIndex(anItem) < 0) then
          Select(anItem);
        anItem := Schema.Next(anItem);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure   TVectorViewForm.InvertSelection;
var
  anItem : TdeSchemaObject;
begin
  with FdeMapper do begin
    BeginUpdate;
    try
      anItem := Schema.First;
      while (anItem<>nil) do
        begin
          Select(anItem);
          anItem := Schema.Next(anItem);
        end;
    finally
      EndUpdate;
    end;
  end;
end;

function TVectorViewForm.FindShortcut(anObject:TdeSchemaObject;
  ShortcutID:integer;  var ShortcutDSM : TDataSetMeta):TCacheItem;
var i           : integer;
    SCLinkField : TFieldMeta;
    objID       : integer;
begin
  Result := nil;
  ShortcutDSM := nil;
  if (DataSetMeta = nil) or (FStorage = nil) then exit;
  objID := TdeDSMetaStorage(FStorage).getObjectID(anObject);
  if objID < 0 then exit;

  DataSetMeta.Cache.FocusedItem := DataSetMeta.Cache.FindById(objID);

  for i := 0 to DataSetMeta.ChildrenCount - 1 do
    begin
      if DataSetMeta.Children[i].Table.IsInterrelations then
        begin
          ShortcutDSM := DataSetMeta.Children[i];
          break;
        end;
    end;
  if ShortcutDSM = nil then
    begin
      SCLinkField := MetaData.MetaTables[idxInterrelations].Fields.FindByName(fldIRParentKey);
      ShortcutDSM := DataSetMeta.CreateChild(MetaData.MetaTables[idxInterrelations].ID, SCLinkField);
      if not ShortcutDSM.Cache.Active then
        ShortcutDSM.Cache.OpenData;
    end;
  Result := ShortcutDSM.Cache.FindById(ShortcutID);
end;

function    TVectorViewForm.getShortcutInfo(anObject:TdeSchemaObject;
                                         ShortcutID:integer;
                                         var aItemID : Variant):TTableMeta;
var
  SC_DSM       : TDatasetMeta;
  SC_CI        : TCacheItem;
begin
  Result := nil;
  SC_CI  := FindShortcut(anObject, ShortcutID, SC_DSM);
  if SC_CI <> nil then
  begin
    SC_DSM.Cache.FocusedItem := SC_CI;
    SC_DSM.GetLinkedData(SC_DSM.Cache.FocusedIndex, Result, aItemID);
  end;
end;

procedure   TVectorViewForm.ShowShortcutProps(aShortcut:TdeSchemaShortcut);
var
  TableMeta    : TTableMeta;
  varRefKey    : variant;
  sError       : string;
begin
  sError := '';

  TableMeta := getShortCutInfo(aShortcut.LinkedObject, aShortcut.ShortcutID, varRefKey);
  if TableMeta = nil then
  begin
    Error('_dE.linkabsent');
    exit;
  end;

  if not(varIsNull(varRefKey)) then
    EditRecord(TableMeta, varRefKey)
  else
    Error('_dE.linkabsent');
end;

procedure   TVectorViewForm.act_Da_ZoomInExecute(Sender: TObject);
begin
  ZoomIn;
end;

procedure   TVectorViewForm.act_Da_ZoomOutExecute(Sender: TObject);
begin
  ZoomOut;
end;

procedure   TVectorViewForm.deMapperMouseMove(Sender:TObject;Shift:TShiftState;X,Y:Integer);
var
  P : TPoint2D;
begin
  P := FdeMapper.ViewToSchema(Point(X,Y));
  MouseCoordsStr := Format('[%-d,%-d]',[trunc(P.X),trunc(P.Y)]);
end;

procedure TVectorViewForm.deMapperDblClick(Sender:TObject);
begin
  if FdeMapper.canEdit then
//    act_EditPointsBegin.Execute
    act_Da_PointBeginExecute(Sender)
  else begin
    ChangeDataSetStatus;
    if act_Da_Open.Enabled then
      act_Da_Open.Execute;
  end;
end;

procedure TVectorViewForm.act_Da_GridExecute(Sender: TObject);
begin
  act_Da_Grid.Checked := not(act_Da_Grid.Checked);
  if act_Da_Grid.Checked then
    FdeMapper.Options := FdeMapper.Options + [voGrid]
  else
    FdeMapper.Options := FdeMapper.Options - [voGrid];
end;

procedure TVectorViewForm.act_Da_RulersExecute(Sender: TObject);
begin
  act_Da_Rulers.Checked := not(act_Da_Rulers.Checked);
  if act_Da_Rulers.Checked then
    FdeMapper.RulerStyle := FRulerStyle
  else
    FdeMapper.RulerStyle := deSchemaView.rsNone;
end;

procedure TVectorViewForm.act_Da_MapCoordExecute(Sender: TObject);
begin
  if FdeMapper.ShowCoordinates=vcNone then
    FdeMapper.ShowCoordinates:=vcBoth
  else
    FdeMapper.ShowCoordinates:=vcNone;
  act_Da_MapCoord.Checked:= (FdeMapper.ShowCoordinates=vcBoth);
  FdeMapper.RefreshView(False);
end;

procedure TVectorViewForm.act_Da_EditModeExecute(Sender: TObject);
begin
  act_Da_EditMode.Checked := not(act_Da_EditMode.Checked);
  if act_Da_EditMode.Checked then
    Mode := vmEdit
  else
    Mode := vmBrowse;
end;

procedure TVectorViewForm.act_Da_IconssmallExecute(Sender: TObject);
begin
  act_Da_Iconssmall.Checked := not(act_Da_Iconssmall.Checked);
  if act_Da_Iconssmall.Checked then
    FShortcutImages := DataUnit.DM.ilIcon16{swImages}
  else
    FShortcutImages := DataUnit.DM.ilIcon32{ImagesWhite};
  UpdateShortcuts;
end;

procedure TVectorViewForm.act_Da_MapEnhancedExecute(Sender: TObject);
begin
  act_Da_MapEnhanced.Checked := not(act_Da_MapEnhanced.Checked);
  if act_Da_MapEnhanced.Checked then
    FdeMapper.Options := FdeMapper.Options + [voSmoothing]
  else
    FdeMapper.Options := FdeMapper.Options - [voSmoothing];
end;


procedure TVectorViewForm.act_CopyExecute(Sender: TObject);
begin
  CopySelected;
end;

procedure TVectorViewForm.act_PasteExecute(Sender: TObject);
begin
  DeleteSelected;
  LoadFromClipboard;
end;

procedure TVectorViewForm.act_CutExecute(Sender: TObject);
begin
  CopySelected;
  DeleteSelected;
end;

procedure TVectorViewForm.act_DeleteExecute(Sender: TObject);
var
  aShortcut : TdeSchemaShortcut;
  CI : TCacheItem;
  SC_DSM : TDatasetMeta;
begin
  if FdeMapper.ActiveShortcut <> nil
  then begin
    aShortcut := FdeMapper.ActiveShortcut;
    CI := FindShortcut(aShortcut.LinkedObject, aShortcut.ShortcutID, SC_DSM);
    if (CI <> nil) and (CI.Owner <> nil) then
      SC_DSM.Cache.DeleteRecord(CI);
    with TdeDSMetaStorage(FStorage) do begin
      OpenDatabase(true);
      DelObject(aShortcut);
      CloseDatabase;
    end;
    aShortcut.Delete;
    FdeMapper.RefreshView;
  end
  else
    DeleteSelected;
  ChangeDataSetStatus;
end;

procedure TVectorViewForm.act_Da_GroupingExecute(Sender: TObject);
begin
  FdeMapper.Group;
end;

procedure TVectorViewForm.act_Da_UngroupingExecute(Sender: TObject);
begin
  FdeMapper.unGroup;
end;

procedure TVectorViewForm.act_SaveExecute(Sender: TObject);
begin
  MetaData.BeginUpdate;
  if FDeMapper.IsEditing then
    FDeMapper.EndEdit;
  FStorage.Save(FdeMapper.Schema);
  FWhileSave := true;
  DataSetMeta.Cache.Update(mcNone,NULL);
  MetaData.EndUpdate;
end;

procedure TVectorViewForm.actLayer_Da_CreateExecute(Sender: TObject);
var
  LN      : string;
  i,j     : integer;
  MaxRoof : integer;
  CurRoof : integer;
begin
  //вычислить максимальный индекс из имен слоев
  if FdeMapper.Schema.LayersCount=0 then
    begin
      LN := '_Df.default';
    end
  else
    begin
      MaxRoof := 0;
      for i := 0 to FdeMapper.Schema.LayersCount-1 do
        begin
          LN := FdeMapper.Schema.Layer[i].Name;
          for j:=Length(LN) downTo 1 do
            if Not (LN[j] in ['0'..'9']) then Delete(LN,j,1);
          CurRoof := StrToIntDef(LN, MaxRoof);
          if MaxRoof<CurRoof then
            MaxRoof:=CurRoof;
        end;
      LN := '_Df.Layer '+ trim(IntToStr(MaxRoof+1));
    end;
  //создать новый слой
  FdeMapper.Schema.LayerByName[LN];
  ActivateLayer(FdeMapper.Schema.LayerIndex(LN));
  FdeMapper.Schema.LayerByName[LN].Modified:=True;
end;

procedure TVectorViewForm.act_AddExecute(Sender: TObject);
var
  anItem : TdeSimpleCurve;
  K      : double;
  P      : TPoint2D;
  DP     : array[0..3] of TPoint2D;
  CreateAt : TPoint2D;
begin
  {}
  with FdeMapper do begin
    if FCalledFromPP then
      CreateAt := ViewToSchema(ScreenToClient(PopupMenu.PopupPoint))
    else
      CreateAt := Offset;
  end;
  FCalledFromPP := false;
  {}
  FdeMapper.BeginUpdate;
  try
    with FdeMapper.Schema do begin
      if FActiveLayer>=0 then
        anItem := TdeSimpleCurve(New(TdeSimpleCurve,nil,Layer[FActiveLayer].Name))
      else
        anItem := TdeSimpleCurve(New(TdeSimpleCurve,nil,''));
    end;
    anItem.Lock;
    try
      anItem.Color       := clGray;
      anItem.BorderColor := clBlack;
      anItem.Transparent := true;
      anItem.BorderWidth := 2;
      K := 100/FdeMapper.Scale/2;
      P := CreateAt;
      DP[0] := Point2D(P.X-K,P.Y+K);
      DP[0].W := PT_MOVETO;
      DP[1] := Point2D(P.X+K,P.Y+K);
      DP[1].W := PT_LINETO;
      DP[2] := Point2D(P.X+K,P.Y-K);
      DP[2].W := PT_LINETO;
      DP[3] := Point2D(P.X-K,P.Y-K);
      DP[3].W := PT_LINETO or PT_CLOSEFIGURE;
      anItem.setDrawPath(DP,4,nil);
    finally
      anItem.unLock;
    end;
    
    if Variables.AsBoolean[RegAutoSaveChanges] then
      with TdeDSMetaStorage(FStorage) do
        begin
          OpenDatabase(true);
          StoreObject(anItem);
          CloseDatabase;
        end;
    {}
  finally
    FdeMapper.EndUpdate;
  end;
  UpdateLayers;
end;

procedure TVectorViewForm.act_Da_ClearSelectionExecute(Sender: TObject);
begin
  inherited;
  FdeMapper.ClearSelection;
  ChangeDataSetStatus;
end;

procedure TVectorViewForm.act_Da_ZoomNormExecute(Sender: TObject);
begin
  inherited;
  Normalize;
end;

procedure TVectorViewForm.act_Da_ZoomSelectExecute(Sender: TObject);
begin
  inherited;
  NormalizeBySelected;
end;

procedure TVectorViewForm.act_Da_ZoomDefaultExecute(Sender: TObject);
var aScale : Double;
begin
  aScale:=StrToFloatDef(ViewParams.GetValueByName('VectorScale'),-1);
  if aScale<=0 then exit;

  FdeMapper.Scale:=aScale/100;
  UpdateZoom;
end;

procedure TVectorViewForm.WM_MouseWheel(var Message: TMSHMouseWheel);
begin
  if Message.WheelDelta<0 then
    ZoomIn
  else
    ZoomOut;
  UpdateZoom;
end;

procedure TVectorViewForm.actLayer_Da_DeleteExecute(Sender: TObject);
begin
  inherited;
  DeleteLayer(FActiveLayer);
end;

procedure TVectorViewForm.actLayer_Da_RenameExecute(Sender: TObject);
var s:String;
begin
  s:=FdeMapper.Schema.Layer[FActiveLayer].Name;
  if EditName('_Dl.Activelayer','_Df.Name',s) then
    begin
      FdeMapper.Schema.Layer[FActiveLayer].Name:=s;
      FdeMapper.Schema.Layer[FActiveLayer].Modified:=True;
    end;
  UpdateLayers;
end;

procedure TVectorViewForm.act_Da_PointBeginExecute(Sender: TObject);
begin
  inherited;
  FdeMapper.Edit;
end;

procedure TVectorViewForm.act_Da_PointEndExecute(Sender: TObject);
begin
  inherited;
  FdeMapper.EndEdit;
end;

procedure TVectorViewForm.act_Da_PointInsertExecute(Sender: TObject);
var
  aPoint  : TPoint2D;
  iPoint  : TPoint;
begin
  inherited;
  iPoint  := FdeMapper.ScreenToClient(PopupMenu.PopupPoint);
  aPoint  := FdeMapper.ViewToSchema(iPoint);
  with FdeMapper.EditedItem do
    AddVertex(ActiveVertex,StoO(aPoint));
end;

procedure TVectorViewForm.act_Da_PointDeleteExecute(Sender: TObject);
begin
  inherited;
  with FdeMapper.EditedItem do
    DelVertex(ActiveVertex);
end;

procedure TVectorViewForm.PopupMenuPopup(Sender: TObject);
var
  iPoint    : TPoint;
  iPntClass : TdeVertexesPointClass;
  vertexID  : integer;
  vtxType   : TdeVertexType;
  sgmType   : TdeSegmentType;
  vtxClosed : boolean;
begin
  FCalledFromPP := true;

  inherited;
  Item_Da_Rotate.Visible       := (FdeMapper.Mode=vmEdit);
  Item_Da_GroupActions.Visible := (FdeMapper.Mode=vmEdit);
  Item_Da_Order.Visible        := (FdeMapper.Mode=vmEdit);

  act_Da_PointBegin.Visible := (FdeMapper.canEdit);
  act_Da_PointBegin.Enabled := act_Da_PointBegin.Visible;
  act_Da_PointEnd.Visible   := FdeMapper.IsEditing;
  act_Da_PointEnd.Enabled   := act_Da_PointEnd.Visible;

  actLayer_Da_Create.Enabled  := (FdeMapper.Mode=vmEdit);
  actLayer_Da_Rename.Enabled  := (FdeMapper.Mode=vmEdit) and(FActiveLayer>=0)
                                 and(FdeMapper.Schema.Layer[FActiveLayer]<>nil);
  actLayer_Da_Delete.Enabled  := (FdeMapper.Mode=vmEdit)
                                 and(FActiveLayer>=0)
                                 and(FdeMapper.Schema.Layer[FActiveLayer]<>nil);

  iPoint    := FdeMapper.ScreenToClient(PopupMenu.PopupPoint);

  vertexID  :=-1;
  iPntClass := vpcUnknown;
  vtxClosed := true;
  vtxType   := vtxAngular;
  sgmType   := sgmLinear;
  if (FdeMapper.IsEditing) then with FdeMapper,EditedItem do begin
    vertexID := ActiveVertex;
    iPntClass := Classify(ViewToSchema(iPoint),
                          sqrt(2)*cPointSize/Scale,
                          vertexID);
    ActiveVertex := vertexID;
    if (vertexID>=0) then begin
      if (iPntClass=vpcVertex) then
        vtxType   := VertexType[vertexID];
      if (iPntClass=vpcEdge) then
        sgmType   := SegmentType[vertexID];
    end;
    vtxClosed := Closed;
  end;
  act_Da_PointAngular.Visible := (FdeMapper.IsEditing) and(iPntClass=vpcVertex);
  act_Da_PointAngular.Enabled := act_Da_PointAngular.Visible;
  act_Da_PointDirect.Visible  := act_Da_PointAngular.Visible;
  act_Da_PointDirect.Enabled  := act_Da_PointAngular.Visible;
  act_Da_PointSmooth.Visible  := act_Da_PointAngular.Visible;
  act_Da_PointSmooth.Enabled  := act_Da_PointAngular.Visible;

  if act_Da_PointAngular.Visible then
  begin
    act_Da_PointAngular.Checked := (vtxType=vtxAngular);
    act_Da_PointDirect.Checked  := (vtxType=vtxDirect);
    act_Da_PointSmooth.Checked  := (vtxType=vtxSmooth);
  end;

  act_Da_PointInsert.Visible  := (FdeMapper.IsEditing)and(iPntClass=vpcEdge);
  act_Da_PointInsert.Enabled  := act_Da_PointInsert.Visible;
  act_Da_PointDelete.Visible  := (FdeMapper.IsEditing)and(iPntClass=vpcVertex);
  act_Da_PointDelete.Enabled  := act_Da_PointDelete.Visible;
  act_Da_PointClose.Visible   := (FdeMapper.IsEditing)and not(vtxClosed);
  act_Da_PointClose.Enabled   := act_Da_PointClose.Visible;
  act_Da_PointUnClose.Visible := (FdeMapper.IsEditing)and (vtxClosed);
  act_Da_PointUnClose.Enabled := act_Da_PointUnClose.Visible;

  act_Da_PointLinear.Visible := (FdeMapper.IsEditing) and(iPntClass=vpcEdge);
  act_Da_PointLinear.Enabled := act_Da_PointLinear.Visible;
  act_Da_PointBezier.Visible := act_Da_PointLinear.Visible;
  act_Da_PointBezier.Enabled := act_Da_PointLinear.Visible;
  if act_Da_PointLinear.Visible then
  begin
    act_Da_PointLinear.Checked := (SgmType=sgmLinear);
    act_Da_PointBezier.Checked := (SgmType=sgmBezier);
  end;
  mdEditVertex.Visible := act_Da_PointAngular.Visible or act_Da_PointBezier.Visible;

  ChangeDataSetStatus;
end;

procedure TVectorViewForm.act_PropertiesExecute(Sender: TObject);
var
  iPoint    : TPoint;
  iPntClass : TdeVertexesPointClass;
  vertexID  : integer;
  xPoint    : TPoint2D;
  F         : boolean;
begin
  if (FdeMapper.ActiveShortcut <> nil) then
    ShowShortcutProps(FdeMapper.ActiveShortcut)
  else begin
  {}
    F := true;
    if FdeMapper.IsEditing then
    begin
      iPoint    := FdeMapper.ScreenToClient(PopupMenu.PopupPoint);
      with FdeMapper,EditedItem do begin
        vertexID := ActiveVertex;
        iPntClass := Classify(ViewToSchema(iPoint),
                              sqrt(2)*cPointSize/Scale,
                              vertexID);
        case iPntClass of
          vpcVertex : begin
            xPoint := OtoS(Vertex[vertexID]);
            if EditObjectPointProps(xPoint,(Mode=vmEdit))
            then
              Vertex[vertexID]:=StoO(xPoint);
            F := false;
          end;
          vpcLeftCP : begin
            getLeftCP(vertexID,xPoint);
            xPoint := OtoS(xPoint);
            if EditObjectPointProps(xPoint,(Mode=vmEdit)) then
              setLeftCP(vertexID,StoO(xPoint));
            F := false;
          end;
          vpcRightCP : begin
            getRightCP(vertexID,xPoint);
            xPoint := OtoS(xPoint);
            if EditObjectPointProps(xPoint,(Mode=vmEdit)) then
              setRightCP(vertexID,StoO(xPoint));
            F := false;
          end;
        end;
      end;
    end;
    
    if F and (FdeMapper.FocusedItem <> nil) then
      begin
        inherited;
      end;
  end;
  ChangeDataSetStatus;
end;

procedure   TVectorViewForm.ZoomIn;
begin
  FdeMapper.ZoomIn;
  UpdateZoom;
end;

procedure   TVectorViewForm.ZoomOut;
begin
  FdeMapper.ZoomOut;
  UpdateZoom;
end;

procedure TVectorViewForm.act_Da_PointAngularExecute(Sender: TObject);
begin
  inherited;
  with FdeMapper.EditedItem do
    VertexType[ActiveVertex]:=vtxAngular;
end;

procedure TVectorViewForm.act_Da_PointDirectExecute(Sender: TObject);
begin
  inherited;
  with FdeMapper.EditedItem do
    VertexType[ActiveVertex]:=vtxDirect;
end;

procedure TVectorViewForm.act_Da_PointSmoothExecute(Sender: TObject);
begin
  inherited;
  with FdeMapper.EditedItem do
    VertexType[ActiveVertex]:=vtxSmooth;
end;

procedure TVectorViewForm.act_Da_PointLinearExecute(Sender: TObject);
begin
  inherited;
  with FdeMapper.EditedItem do
    SegmentType[ActiveVertex]:=sgmLinear;
end;

procedure TVectorViewForm.act_Da_PointBezierExecute(Sender: TObject);
begin
  inherited;
  with FdeMapper.EditedItem do
    SegmentType[ActiveVertex]:=sgmBezier;
end;

procedure TVectorViewForm.act_Da_PointCloseExecute(Sender: TObject);
begin
  inherited;
  with FdeMapper.EditedItem do
    Closed := true;
end;

procedure TVectorViewForm.act_Da_PointUncloseExecute(Sender: TObject);
begin
  inherited;
  with FdeMapper.EditedItem do
    Closed := false;
end;

procedure TVectorViewForm.act_Da_MapToFrontExecute(Sender: TObject);
begin
  inherited;
  with FdeMapper do
    Selected[0].Index := Schema.RootCount-1;
end;

procedure TVectorViewForm.act_Da_MapToBackExecute(Sender: TObject);
begin
  inherited;
  with FdeMapper do
    Selected[0].Index := 0;
end;

procedure TVectorViewForm.act_Da_MapCombineExecute(Sender: TObject);
var
  firstObject : TdeCustomCurve;
  newObject   : TdeComplexCurve;
begin
  inherited;
  FdeMapper.BeginUpdate;
  try
    firstObject := TdeCustomCurve(FdeMapper.Selected[0]);
    newObject := TdeComplexCurve(FdeMapper.Schema.New(TdeComplexCurve,
                                                      firstObject.Parent,
                                                      firstObject.LayerName));
    newObject.Lock;
    try
      newObject.Color       := firstObject.Color;
      newObject.BorderColor := firstObject.BorderColor;
      newObject.BorderStyle := firstObject.BorderStyle;
      newObject.BorderWidth := firstObject.BorderWidth;
      newObject.Transparent := firstObject.Transparent;
      while FdeMapper.SelectedCount > 0 do
        begin
          newObject.CombineWith(TdeCustomCurve(FdeMapper.Selected[0]));
          FdeMapper.Selected[0].Delete;
        end;
    finally
      newObject.unLock;
    end;
  finally
    FdeMapper.EndUpdate;
  end;
end;

procedure TVectorViewForm.act_Da_MapBreakCurveExecute(Sender: TObject);
var
  newObjects : tList;
  i          : integer;
begin
  inherited;
  if (FdeMapper.SelectedCount <> 1)
     or not(FdeMapper.Selected[0] is TdeComplexCurve) then exit;

  FdeMapper.BeginUpdate;
  newObjects := tList.Create;
  try
    TdeComplexCurve(FdeMapper.Selected[0]).Split(newObjects);
    FdeMapper.Selected[0].Delete;
    for i := 0 to newObjects.Count-1 do
      FdeMapper.Select(TdeSchemaObject(newObjects[i]));
  finally
    newObjects.Free;
    FdeMapper.EndUpdate;
  end;
end;

procedure TVectorViewForm.act_CreateShortcutExecute(Sender: TObject);
var
  newShortcut : TdeSchemaShortcut;
  SCKey       : Variant;
  CreateAt    : TPoint2D;
  CI          : TCacheItem;
  ScTbMeta    : TTableMeta;
  ScItemID    : variant;
  Cache       : TDataCache;
  ShortcutsData : TDataSetMeta;
begin
  if DataSetMeta=nil then exit;

  if DataSetMeta.Table <> MetaData.MetaTables[idxInterrelations] then
  begin
    ShortcutsData := DataSetMeta.FindChild(MetaData.MetaTables[idxInterrelations]);
    if not Assigned(ShortcutsData) then
      ShortcutsData := DataSetMeta.CreateChild(MetaData.MetaTables[idxInterrelations].ID, nil);
  end
  else
    ShortcutsData := DataSetMeta;
  if not ShortcutsData.Cache.AppendRecord(SCKey) then exit;

  ScTbMeta := getShortcutInfo(FdeMapper.FocusedItem,SCKey,ScItemID);
  if ScTbMeta = nil then exit;

  Cache := TDataCache.Create(ScTbMeta);
  try
    Cache.Filters.NewFilter(ScTbMeta.KField[0], opEQ, ScItemID);
    Cache.PrepareData;
    CI := Cache.FocusedItem;

    with FdeMapper do begin
      if FCalledFromPP then
        CreateAt := ViewToSchema(ScreenToClient(PopupMenu.PopupPoint))
      else
        CreateAt := Offset;
      FCalledFromPP := false;
      BeginUpdate;
     try
        newShortcut := Schema.NewShortcut;
        newShortcut.Offset       := CreateAt;
        newShortcut.Caption      := CI.Caption;
        newShortcut.Options      := [scoShowImage,scoShowCaption];
        newShortcut.LinkedObject := FocusedItem;
        newShortcut.ImageIndex   := CI.ImageIndex;
        newShortcut.ShortcutID   := SCKey;

        with TdeDSMetaStorage(FStorage) do begin
          OpenDatabase(true);
          StoreObject(newShortcut);
          CloseDatabase;
        end;
      finally
        EndUpdate;
      end;
    end;
  finally
    Cache.Free;
  end;
end;

procedure TVectorViewForm.act_Da_SelectallExecute(Sender: TObject);
begin
  SelectAll;
  ChangeDataSetStatus;
end;

procedure TVectorViewForm.act_DtInvertselectionExecute(Sender: TObject);
begin
  InvertSelection;
  ChangeDataSetStatus;
end;

procedure TVectorViewForm.act_dA_RotateFlipHExecute(Sender: TObject);
var i:Integer;
begin
  for i:=0 to FdeMapper.SelectedCount-1 do
   FdeMapper.Selected[i].Flip(fdHorizontal);
end;

procedure TVectorViewForm.act_dA_RotateFlipVExecute(Sender: TObject);
var i:Integer;
begin
  for i:=0 to FdeMapper.SelectedCount-1 do
   FdeMapper.Selected[i].Flip(fdVertical);
end;

procedure TVectorViewForm.act_dA_Rotate90Execute(Sender: TObject);
var i:Integer;
begin
  for i:=0 to FdeMapper.SelectedCount-1 do
   FdeMapper.Selected[i].Rotate(Pi/2);
end;

procedure TVectorViewForm.act_dA_Rotate270Execute(Sender: TObject);
var i:Integer;
begin
  for i:=0 to FdeMapper.SelectedCount-1 do
   FdeMapper.Selected[i].Rotate(-Pi/2);
end;

{$IFDEF DEBUG}
initialization
  DebugLog('VectorFormUnit unit initialization ...');

finalization
  DebugLog('VectorFormUnit unit finalization ...');
{$ENDIF}

end.

