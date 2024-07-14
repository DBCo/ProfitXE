unit ChartFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Menus, ActnList, ComCtrls, System.Generics.Collections,
  ToolWin, ExtCtrls, Buttons, StdCtrls, Contnrs, Dialogs, TeeProcs, TeEngine, TeeGDIPlus, Chart, Series, Actions,
  DeTypes, DeMeta, BaseGridFormUnit, DePrintStyles, DeReport, Vcl.ImgList;

const
    {
    TimeArray  : array[0..25] of Double =
    ( 1/(24*60*60), 2/(24*60*60), 5/(24*60*60), 10/(24*60*60), 20/(24*60*60), // секунды s
      1/(24*60), 2/(24*60), 5/(24*60), 10/(24*60), 20/(24*60), // минуты m
      1/(24), 2/(24), 4/(24), 12/(24), // часы h
      1, 2, 4, 7, 15, // дни D
      1*30, 2*30, 6*30, // месяцы M
      1*365, 2*365, 5*365, 10*365); //годы Y
    {}
    ChartTypes : array[0..5] of TChartSeriesClass =
                                      (TPieSeries, TBarSeries, THorizBarSeries, TLineSeries, TAreaSeries, TPointSeries);
    maxGroupX = 39;
    maxGroupY = 12;

    giID = 0;
    giCaption = 1;
    giCount = 2;
    giNNCount = 3;
    giSum = 4;
    giMin = 5;
    giMax = 6;

type
  TChartType = (
    ChartTypePie    = 0,
    ChartTypeBarV   = 1,
    ChartTypeBarH   = 2,
    ChartTypeLine   = 3,
    ChartTypeArea   = 4,
    ChartTypePoint  = 5
    );

  TValuesItem = Record
    FSum: Variant;            // Descrete - Sum           // Continue - Value
    FMin: Variant;            // Descrete - Min           // Continue - X
    FMax: Variant;            // Descrete - Max           // Continue - ID записи в Cache
    FCount: Integer;          // Descrete - Count         // Continue - номер записи в Cache
    FNotNullCount: Integer;   // Descrete - NotNull (0<=) // Continue - -1 флаг Continue
  private
  public
    procedure ContinueItem(const aValue: Variant; const aX: Variant; const aID: Variant; const aNumber: Integer);
    procedure DescreteItem;
    procedure DescreteValue(const aValue: Variant);
    function Value(aType: TAgregateType): Variant;
    function V: Variant;
    function X: Variant;
    function ID: Variant;
    function N: Integer;
    function IsDesrete: Boolean; inline;
    procedure Join(const aItem: TValuesItem);
  end;

  TGroupItem = class
  private
    FHeader: array[giID..giMAX] of variant;
    function GetHeaderItem(const Index: Integer): variant;
    procedure SetHeaderItem(const Index: Integer; const Value: variant);
  public
    Data: Array of TValuesItem;
    constructor Create;
    destructor Destroy; override;
    procedure Join(const aGroupItem: TGroupItem);
    property ID: variant index giID read GetHeaderItem write SetHeaderItem;
    property Caption: variant index giCaption read GetHeaderItem write SetHeaderItem;
    property Count: variant index giCount read GetHeaderItem write SetHeaderItem;
    property Sum: variant index giSum read GetHeaderItem write SetHeaderItem;
    property Min: variant index giMin read GetHeaderItem write SetHeaderItem;
    property Max: variant index giMax read GetHeaderItem write SetHeaderItem;
    function Length: Integer;
  end;

  TListGroup  =  class(TObjectList)
  private
    FFieldMeta: TFieldMeta;
    function GetItem(const Index: Integer): TGroupItem;
    procedure PutItem(const Index: Integer; const Value: TGroupItem);
  public
    function IndexByID(aValue: Variant): Integer;
    function AddNew(aValue, aCaption: Variant): Integer;
    property FieldMeta: TFieldMeta read FFieldMeta write FFieldMeta;
    property Items[const Index: Integer]: TGroupItem read GetItem write PutItem; default;
  end;

  TDeChartForm = class(TBaseGridForm)
    ChartPieChart: TMenuItem;
    ChartBarChart: TMenuItem;
    act_Da_ChartPie: TAction;
    act_Da_ChartBarV: TAction;
    actPieChart1: TMenuItem;
    actBarChart1: TMenuItem;
    act_Da_ChartLine: TAction;
    actBarChart2: TMenuItem;
    actBarChart3: TMenuItem;
    N4: TMenuItem;
    act_Da_ChartBarH: TAction;
    act_Da_ChartArea: TAction;
    act_Da_ChartPoint: TAction;
    BarH1: TMenuItem;
    Area1: TMenuItem;
    Point1: TMenuItem;
    N5: TMenuItem;
    Line1: TMenuItem;
    Area2: TMenuItem;
    Point2: TMenuItem;
    act_Da_Chart3D: TAction;
    act_Da_ChartStacked: TAction;
    act_Da_ChartLog: TAction;
    act_Da_ChartHide: TAction;
    Chart3D1: TMenuItem;
    ChartStacked1: TMenuItem;
    ChartStacked2: TMenuItem;
    ChartLog1: TMenuItem;
    act_dA_ChartCurve: TAction;
    ChartCurve1: TMenuItem;
    TeeGDIPlus1: TTeeGDIPlus;
    crtChart: TChart;
    mi_dL_results: TMenuItem;
    Item_dL_metrics: TMenuItem;
    MM_dL_Metrics: TMenuItem;
    MM_dL_results: TMenuItem;
    Chart3D2: TMenuItem;
    ChartCurve2: TMenuItem;
    ChartLog2: TMenuItem;
    ChartHide1: TMenuItem;
    ChartStacked3: TMenuItem;
    actDaAgrCount1: TMenuItem;
    actDaAgrValue1: TMenuItem;
    actDaAgrMin1: TMenuItem;
    actDaAgrMax1: TMenuItem;
    actDaAgrAve1: TMenuItem;
    actDaAgrValue2: TMenuItem;
    Count1: TMenuItem;
    Sum1: TMenuItem;
    Min1: TMenuItem;
    Min2: TMenuItem;
    Max1: TMenuItem;
    Value1: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    Savetofile1: TMenuItem;
    function ClearIndex: Boolean;
    procedure ChartTypeClick(Sender: TObject);
    procedure crtChartMouseMove(Sender:TObject;Shift:TShiftState;X,Y:Integer);
    procedure crtChartDblClick(Sender: TObject);
    procedure act_Da_Chart3DExecute(Sender: TObject);
    procedure act_Da_ChartLogExecute(Sender: TObject);
    procedure act_Da_ChartStackedExecute(Sender: TObject);
    procedure act_Da_ChartHideExecute(Sender: TObject);
    procedure act_CopyExecute(Sender: TObject);
    procedure act_dA_ChartCurveExecute(Sender: TObject);
    procedure crtChartMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure crtChartMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure crtChartMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMetrics(Sender: TObject);
    procedure DoSplit(Sender: TObject);
    procedure DoResult(Sender: TObject);
    procedure act_Da_AgrNoExecute(Sender: TObject);
    procedure act_Da_AgrCountExecute(Sender: TObject);
    procedure act_Da_AgrMinExecute(Sender: TObject);
    procedure act_Da_AgrAveExecute(Sender: TObject);
    procedure act_Da_AgrMaxExecute(Sender: TObject);
    procedure act_Da_AgrSumExecute(Sender: TObject);
    procedure act_Da_AgrValueExecute(Sender: TObject);
    procedure crtChartClick(Sender: TObject);
    procedure act_Da_SavetofileExecute(Sender: TObject);
    procedure crtChartAfterDraw(Sender: TObject);
  private
    { Private declarations }
    FOperation        : TOperationType;
    FSumFields        : TFieldsMeta;
    FSortFields       : TFieldsMeta;
    FSortByNo, FSortByName, FSortByValue : TFieldMeta;
    FChartType        : TChartType;
    FFieldX: TFieldMeta;
    FFieldA: TFieldMeta;
    FSortBy: TFieldMeta;
    FChart3D, FChartHide, FChartStacked, FChartLog, FChartCurve: Boolean;
    FX, FY: TListGroup;
    FLastInfo: string;
    FStepX: Double;
    FSizeX: Double;
    FPositionX: Double;
    FPositionMinX: Boolean;
    FPositionMaxX: Boolean;
    FMouseDown: Boolean;
    FMouseDownX, FMouseDownY: Integer;
    FMouseDownMinX, FMouseDownMaxX: Double;
    FMouseDownMinY, FMouseDownMaxY: Double;
    procedure SetChartType(aChartType : TChartType);
    procedure SetChart3D(aValue : Boolean);
    procedure SetChartHide(aValue : Boolean);
    procedure SetChartStacked(aValue : Boolean);
    procedure SetChartLog(aValue : Boolean);
    procedure SetChartCurve(aValue : Boolean);
    procedure SetSortBy(aFieldMeta: TFieldMeta);
    procedure SetFieldA(aFieldMeta: TFieldMeta);
    procedure SetFieldX(aFieldMeta: TFieldMeta);
    procedure SetFieldY(aFieldMeta: TFieldMeta);
    function GetFieldX: TFieldMeta;
    function GetFieldY: TFieldMeta;
    function SeriesByXY(X,Y:integer;var Index:integer):TChartSeries;
    procedure SetStepX(const Value: Double);
    procedure SetSizeX(const Value: Double);
    procedure SetPositionX(const Value: Double);
    function GetLeftAxisTitle: String;
  protected
    procedure   InitForm; override;
    procedure SetCanDrag(Value: Boolean); override;
    procedure   SortMenuCreate; override;
    procedure   OnSortClick(Sender: TObject); override;
    function    getEnabledPrintStyles:TDePrintStyleTypes; override;
    function    getPrintReport(aStyle : TDePrintStyle):TDeReport; override;
    procedure   doPrintReport(aStyle: TDePrintStyle; const PrinterName: string); override;
    function    getCaptionText:string; override;
    function GetCanShowCard: Boolean; override;
    procedure   AfterInit; override;
  public
    { Public declarations }
    constructor Create(aOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   DeInitForm; override;
    procedure   ReCreateView; override;
    procedure   ChangeDataSetStatus; override;
    function GetPropertiesString: string; override;
    property ChartType : TChartType read FChartType write SetChartType;
    property Chart3D : Boolean read FChart3D write SetChart3D;
    property ChartHide : Boolean read FChartHide write SetChartHide;
    property ChartStacked : Boolean read FChartStacked write SetChartStacked;
    property ChartLog : Boolean read FChartLog write SetChartLog;
    property ChartCurve : Boolean read FChartCurve write SetChartCurve;
    property SortBy: TFieldMeta read FSortBy write SetSortBy;
    property FieldX: TFieldMeta read GetFieldX write SetFieldX;
    property FieldY: TFieldMeta read GetFieldY write SetFieldY;
    property FieldA: TFieldMeta read FFieldA write SetFieldA;
    property StepX: Double read FStepX write SetStepX;
    property SizeX: Double read FSizeX write SetSizeX;
    property PositionX: Double read FPositionX write SetPositionX;
    function DescreteX: Boolean;
  end;

implementation

uses Math, ClipBrd, Variants, Forms, uIconUtils, DeLog, Funcs, Security,
     Dictionary, DeMetadata, DSMeta, DeSettings, DataCacheUnit, UnitA,
     DeStdReport, DataUnit;

{$R *.dfm}

constructor TDeChartForm.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
//FOperation:= opNone;
  FOperation:= opCOUNT;
  FMainControl:= crtChart;
  FSumFields:= TFieldsMeta.Create(False);
  FSortFields:= TFieldsMeta.Create(True);

  FSortByNo:= FSortFields.New('No','_dL.no');
  FSortByName:= FSortFields.New('Name','_dF.name');
  FSortByValue:= FSortFields.New('Value','_dL.results');
  FSortBy:= FSortByName;

  CanFind:= false;
  CanSorting:= false;
  CanGrouping:= true;
  FChartCurve:= false;

  FX:= TListGroup.Create;
  FY:= TListGroup.Create;
  ClearIndex;
end;

destructor  TDeChartForm.Destroy;
begin
  ClearIndex;
  FSumFields.Free;
  FSortFields.Free;
  FX.Free;
  FY.Free;
  inherited Destroy;
end;

function TDeChartForm.GetPropertiesString: string;
begin
  Result := inherited GetPropertiesString;

  case FChartType of
      ChartTypePie : Result:=Result+' ChartType=Pie;';
     ChartTypeBarV : Result:=Result+' ChartType=BarV;';
     ChartTypeBarH : Result:=Result+' ChartType=BarH;';
     ChartTypeLine : Result:=Result+' ChartType=Line;';
     ChartTypeArea : Result:=Result+' ChartType=Area;';
    ChartTypePoint : Result:=Result+' ChartType=Point;';
  end;

  Result:=Result+' ChartFieldX='+FieldX.Original+';';
  if Assigned(FFieldY) then
  Result:=Result+' ChartFieldY='+FFieldY.Original+';';
  if Assigned(FFieldA) then
  Result:=Result+' ChartFieldA='+FFieldA.Original+';';

  Result:=Result+' Chart3D='+BoolToStr(FChart3D)+';';
  Result:=Result+' ChartHide='+BoolToStr(FChartHide)+';';
  Result:=Result+' ChartStacked='+BoolToStr(FChartStacked)+';';
  Result:=Result+' ChartLog='+BoolToStr(FChartLog)+';';
  Result:=Result+' ChartSort='+FSortBy.Original+';';
  Result:=Result+' ChartCurve='+BoolToStr(FChartCurve)+';';
  if not crtChart.BottomAxis.Automatic then
    Result:=Result+' ChartXSize='+IntToStr(crtChart.BottomAxis.IAxisSize )+';';

end;

procedure   TDeChartForm.InitForm;
var i: integer;
    F: TFieldMeta;
begin
  if DataSetMeta=nil then exit;
  act_Da_Print.Enabled := False;
  inherited;

  if ViewParams.TestByName('ChartType','Pie')   then ChartType := ChartTypePie else
  if ViewParams.TestByName('ChartType','BarH')  then ChartType := ChartTypeBarH else
  if ViewParams.TestByName('ChartType','Line')  then ChartType := ChartTypeLine else
  if ViewParams.TestByName('ChartType','Area')  then ChartType := ChartTypeArea else
  if ViewParams.TestByName('ChartType','Point') then ChartType := ChartTypePoint else
                                                     ChartType := ChartTypeBarV;

  SortBy:= FSortFields.FindByName(ViewParams.GetValueByName('ChartSort'));
  if Not Assigned(FSortBy) then FSortBy:= FSortFields[0];

  Chart3D      := ViewParams.Calculate('Chart3D', True, varBoolean);
  ChartHide    := ViewParams.Calculate('ChartHide', True, varBoolean);
  ChartStacked := ViewParams.Calculate('ChartStacked', False, varBoolean);
  ChartLog     := ViewParams.Calculate('ChartLog', False, varBoolean);
  ChartCurve   := ViewParams.Calculate('ChartCurve', False, varBoolean);
  SizeX        := ViewParams.Calculate('ChartXSize', 0, varDouble);
  PositionX    := ViewParams.Calculate('ChartXPos', unassigned, varDouble);

  if not DataSetMeta.Cache.Active then
    DataSetMeta.Cache.OpenData;

  FSumFields.Clear;

  if DataSetMeta.Role <> drMain then exit;

  MM_Da_Splitby.Clear;
  Item_Da_Splitby.Clear;

  for i := Pred(TempActionList.ActionCount) downto 0 do
    if SameText(TempActionList[i].Category, CategoryMetrics) or
       SameText(TempActionList[i].Category, CategorySplit) or
       SameText(TempActionList[i].Category, CategoryResults) then TempActionList[i].Free;

  TMetaAction.Create(self, CategoryMetrics, nil, DoMetrics, [Item_dL_metrics, MM_dL_Metrics]).ActionList:= TempActionList;
  TMetaAction.Create(self, CategorySplit, nil, DoSplit, [Item_Da_Splitby, MM_Da_Splitby]).ActionList:= TempActionList;
  FieldX:= nil;
  FieldY:= nil;
  FieldA:= nil;

  for i:=0 to DataSetMeta.Table.Fields.Count-1 do
    begin
      F := DataSetMeta.Table.Fields[i];
      if (fvLevel1 <= F.VisibleLevel) and SecuritySystem.CheckPolicyDataSet(F.Owner.ID, spSelect)
          and F.PolicySelect and F.PolicyShow and (Not (F.ShowType in [stPassword, stEncryptedPassword])) then
        begin
          if (Not F.IsLookUp) and ((F.LookupPair<>nil) or (F.DataType in NumericTypes + DateTimeTypes) ) then
            begin
              TMetaAction.Create(self, CategoryMetrics, F, DoMetrics, [Item_dL_metrics, MM_dL_Metrics]).ActionList:= TempActionList;
              if Sametext(F.Original, ViewParams.GetByName('ChartFieldX').Value) then FieldX:= F;
            end;

          if (Not F.IsLookUp) and (F.LookupPair<>nil) then
            begin
              TMetaAction.Create(self, CategorySplit, F, DoSplit, [Item_Da_Splitby, MM_Da_Splitby]).ActionList:= TempActionList;
              if Not Assigned(FFieldY) and Sametext(F.Original, ViewParams.GetByName('ChartFieldY').Value) then FieldY:= F;
            end;

          if (F.LookupPair=nil) and (F.DataType in NumericTypes) and (Not F.Key) { and (F.ShowType <> stYesNo)} then
            begin
              TMetaAction.Create(self, CategoryResults, F, DoResult, [mi_dL_results, MM_dL_results]).ActionList:= TempActionList;
              if Not Assigned(FFieldA) or Sametext(F.Original, ViewParams.GetByName('ChartFieldA').Value) then FieldA:= F;
            end;
        end;
    end;

  if not Assigned(FFieldX) then
    for i:=0 to DataSetMeta.Table.Fields.Count-1 do
      begin
        F := DataSetMeta.Table.Fields[i];
        if -1 < DataSetMeta.GroupFields.IndexByID(F.ID) then Continue;

        if not F.IsLookUp then
          if not Assigned(FFieldX) or (FFieldX.VisibleLevel < F.VisibleLevel) or
            ((FFieldX.VisibleLevel = F.VisibleLevel) and (FFieldX.LookupPair=nil) and (F.LookupPair<>nil)) then
            if (F.LookupPair<>nil) or (F.DataType in NumericTypes + DateTimeTypes) then
              begin
                FieldX:= F;
              end;
      end;

  Item_Da_OrderBy.Visible := True;
  MM_Da_OrderBy.Visible   := True;

  act_Da_Copy.Hint := GetTitle('_dA.CopyChart', ttFull);
end;

procedure TDeChartForm.SetStepX(const Value: Double);
begin
  if Assigned(FieldX.LookupPair) then FStepX:= 0
                                 else FStepX:= Value;
end;

procedure TDeChartForm.SetSizeX(const Value: Double);
begin
  FSizeX:= Value;
  crtChart.BottomAxis.Automatic:= (Value = 0);
end;

procedure TDeChartForm.SetPositionX(const Value: Double);
begin
  FPositionX:= Value;
  FPositionMinX:= False;
  FPositionMaxX:= False;
end;

function TDeChartForm.DescreteX: Boolean;
begin
  if Assigned(FieldX.LookupPair) then Result:= True
                                 else Result:= FChartType in [ChartTypePie, ChartTypeBarV, ChartTypeBarH];
end;

procedure TDeChartForm.SetFieldX(aFieldMeta: TFieldMeta);
var V: Boolean;
begin
  if Not Assigned(aFieldMeta) then StepX:= 1 else
  if Assigned(aFieldMeta.LookupPair) then StepX:=0 else
    if StepX=0 then StepX:=1
               else if assigned(aFieldMeta) and assigned(FFieldX) then
                      if aFieldMeta.DataType <> FFieldX.DataType then StepX:=1;

  FFieldX:= aFieldMeta;
  ActionChecked(CategoryMetrics, aFieldMeta);

  V:= DescreteX;
  act_Da_AgrNo.Visible:= V;
  act_Da_AgrCount.Visible:= V;
  act_Da_AgrValue.Visible:= V;
  act_Da_AgrSum.Visible:= V;
  act_Da_AgrMin.Visible:= V;
  act_Da_AgrMax.Visible:= V;
  act_Da_AgrAve.Visible:= V;
end;

procedure TDeChartForm.SetFieldY(aFieldMeta: TFieldMeta);
begin
  FFieldY:= aFieldMeta;
  ActionChecked(CategorySplit, aFieldMeta);
end;

function TDeChartForm.GetFieldX: TFieldMeta;
begin
  if Assigned(FFieldX) then Result:= FFieldX
                       else Result:= DataSetMeta.Table.KField[0];
end;

function TDeChartForm.GetFieldY: TFieldMeta;
begin
  if (FieldX = FFieldY) then Result:= nil
                        else Result:= FFieldY;
end;

procedure TDeChartForm.SetFieldA(aFieldMeta: TFieldMeta);
begin
  FFieldA:= aFieldMeta;
  ActionChecked(CategoryResults, aFieldMeta);
end;

procedure TDeChartForm.SetSortBy(aFieldMeta: TFieldMeta);
begin
  FSortBy:= aFieldMeta;
  ActionChecked(CategorySort, aFieldMeta);
end;

procedure   TDeChartForm.SortMenuCreate;
var i: Integer;
begin
  act_Da_Sorting.Visible:= False;

  for i := Pred(TempActionList.ActionCount) downto 0 do
    if TempActionList[i].Category = CategorySort then
      TempActionList[i].Free;

  for i := 0 to Pred(FSortFields.Count) do
    TMetaAction.Create(self, CategorySort, FSortFields[i], OnSortClick, [Item_Da_OrderBy, MM_Da_OrderBy]).ActionList:= TempActionList;

  SortBy:= FSortBy;
end;

procedure   TDeChartForm.OnSortClick(Sender: TObject);
begin
  if Sender is TMetaAction then
    begin
      SortBy:= TMetaAction(Sender).FieldMeta;
      ReCreateView;
    end;
end;

procedure TDeChartForm.ReCreateView;
var i,j,L : integer;
    s: string;
    aCache  : TDataCache;
    aLookupItem: TCacheItem;
    aSeries : TChartSeries;
    iX, iY, iFldX, iFldY, iFldA: integer;
    V, vXID, vYID, vAID: Variant;
    VItem: TValuesItem;
    P, PMin, PMax: Double;
  //----------------------------------------------------------------------------------------------------- сортировки ---
  procedure QsortX(l, r: integer; aSortBy: integer; aRelation: TVariantRelationship = vrLessThan);
  var i, j, k: integer;
      m: Variant;
      T: TValuesItem;
  begin
    if l>=r then Exit;
    i:=l;
    j:=r;
    M:=FX[(r+l) div 2].GetHeaderItem(aSortBy);
    while i<=j do
      begin
        while VarCompareValue(FX[i].GetHeaderItem(aSortBy), M) = aRelation do i:=i+1;
        while VarCompareValue(M, FX[j].GetHeaderItem(aSortBy)) = aRelation do j:=j-1;

        if (i<=j) then
          begin
            if VarCompareValue(FX[j].GetHeaderItem(aSortBy), FX[i].GetHeaderItem(aSortBy)) = aRelation then
              begin
                FX.Exchange(i,j);
                for k:=0 to Pred(FY.Count) do
                  begin
                    T:= FY[k].Data[i];
                        FY[k].Data[i]:= FY[k].Data[j];
                                        FY[k].Data[j]:= T;
                  end;
              end;
            i:=i+1;
            j:=j-1;
          end;
      end;

    if l<j then QsortX(l, j, aSortBy, aRelation);
    if i<r then QsortX(i, r, aSortBy, aRelation);
  end;

  procedure QsortY(l, r: integer; aSortBy: integer; aRelation: TVariantRelationship = vrLessThan);
  var i, j: integer;
      m: Variant;
  begin
    if l>=r then Exit;
    i:=l;
    j:=r;
    M:=FY[(r+l) div 2].GetHeaderItem(aSortBy);
    while i<=j do
      begin
        while VarCompareValue(FY[i].GetHeaderItem(aSortBy), M) = aRelation do i:=i+1;
        while VarCompareValue(M, FY[j].GetHeaderItem(aSortBy)) = aRelation do j:=j-1;

        if (i<=j) then
          begin
            if VarCompareValue(FY[j].GetHeaderItem(aSortBy), FY[i].GetHeaderItem(aSortBy)) = aRelation then
              begin
                FY.Exchange(i,j);
              end;
            i:=i+1;
            j:=j-1;
          end;
      end;

    if l<j then QsortY(l, j, aSortBy, aRelation);
    if i<r then QsortY(i, r, aSortBy, aRelation);
  end;
  //--------------------------------------------------------------------------------------------------------------------

begin
  ClearIndex;

  for i:= crtChart.SeriesCount-1 downto 0 do
    crtChart.Series[i].Free;
  if DataSetMeta=nil then exit;

  // ...................................................................................................................
  aCache := DataSetMeta.Cache;//CacheItems;
  if assigned(FieldX) then iFldX:= aCache.Fields.IndexByID(FieldX.ID)
                      else Exit;
  if assigned(FieldY) then iFldY:= aCache.Fields.IndexByID(FieldY.ID)
                      else iFldY:= -1;
  if Assigned(FieldA) then iFldA:= aCache.Fields.IndexByID(FFieldA.ID)
                      else iFldA:= -1;

  FX.FieldMeta:= FieldX;
  FY.FieldMeta:= FieldY;

  if iFldY<0 then
    FY.AddNew(-1, '-');

  if not DataSetMeta.Cache.Active then
    DataSetMeta.Cache.OpenData;

  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, NativeInt(PChar('_dL.reading')), -2);
  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, aCache.Count, 1);
  try

  if DescreteX then // дискретная ось X, заполняем FX, раскладываем по ячейкам, считаем count, min, max, sum, avg
    for i:= 0 to aCache.Count - 1 do
      begin
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, i, 0);

        if iFldY=-1 then
          iY:=0
        else
          begin
            vYID:= aCache.Items[i].FieldValue[iFldY];
            iY:= FY.IndexByID(vYID);
            if iY=-1 then // добавляем и запоминаем строку
              begin
                iY:= FY.AddNew(vYID, unassigned);
                SetLength( FY[iY].Data, Length(FY[0].Data));
                for j:= Low(FY[iY].Data) to High(FY[iY].Data) do
                  FY[iY].Data[j].DescreteItem;
              end;
          end;

        vXID:= aCache.Items[i].FieldValue[iFldX];
        if aCache.Fields[iFldX].Key then
          begin
            iX:= FX.AddNew(vXID, aCache.Items[i].Caption);

            L:= Length(FY[iY].Data);
            for j:= 0 to Pred(FY.Count) do
              begin
                SetLength( FY[j].Data, L+1);
                FY[j].Data[L].DescreteItem;
              end;
          end
        else
          begin
            iX:= FX.IndexByID(vXID);
            if iX=-1 then
              begin
                iX:= FX.AddNew(vXID, unassigned);

                L:= Length(FY[iY].Data);
                for j:= 0 to Pred(FY.Count) do
                  begin
                    SetLength( FY[j].Data, L+1);
                    FY[j].Data[L].DescreteItem;
                  end;
              end;
          end;

        if -1 < iFldA then vAID:= aCache.Items[i].FieldValue[iFldA]
                      else vAID:= 1;
        FY[iY].Data[iX].DescreteValue(vAID);
      end

  else  // непрерывная ось X, игнорируем FX, просто набиваем значения
    for i:= 0 to aCache.Count - 1 do
      begin
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, i, 0);

        vXID:= aCache.Items[i].FieldValue[iFldX];
        if VarIsNull(vXID) or VarIsEmpty(vXID) then Continue;

        if iFldY=-1 then
          iY:=0
        else
          begin
            vYID:= aCache.Items[i].FieldValue[iFldY];
            iY:= FY.IndexByID(vYID);
            if iY=-1 then
              iY := FY.AddNew(vYID, unassigned);
          end;


        if -1 < iFldA then vAID:= aCache.Items[i].FieldValue[iFldA]
                      else vAID:= 1;

        L:= Length(FY[iY].Data);
        SetLength( FY[iY].Data, L+1);
        FY[iY].Data[L].ContinueItem(vAID, vXID, aCache.Items[i].ID, i);
      end;


  // Агрегация для сортировки по результату и/или скрытия мелких значений
  if DescreteX and FChartHide then
    begin
      for i:=0 to Pred(FY.Count) do FY[i].Sum:=0;
      for j:=0 to Pred(FX.Count) do FX[j].Sum:=0;

      for i:=0 to Pred(FY.Count) do
        for j:=0 to Pred(FX.Count) do
          begin
            v:= FY[i].Data[j].Value(MetaData.AgregateType);
            if Not VarIsEmpty(V) then
              begin
                FY[i].Sum:= FY[i].Sum + V;
                FX[j].Sum:= FX[j].Sum + V;
              end;
          end;

      if maxGroupY < FY.Count then
        begin
          QsortY(0, Pred(FY.Count), Integer(MetaData.AgregateType));

          for iY:=Pred(FY.Count) downto maxGroupY do
            begin
              for iX:=0 to Pred(Min(FY[maxGroupY-1].Length, FY[iY].Length)) do
                FY[maxGroupY-1].Data[iX].Join(FY[iY].Data[iX]);

              FY[maxGroupY-1].Join(FY[iY]);
              FY.Delete(iY);
            end;

          FY[maxGroupY-1].Caption:= getTitle('_dL.other');;
          FY[maxGroupY-1].ID:= unassigned;
        end;

      if maxGroupX < FX.Count then
        begin
          QsortX(0, Pred(FX.Count), Integer(MetaData.AgregateType));

          for iY:=0 to Pred(FY.Count) do
            begin
              for iX:=maxGroupX to Pred(FX.Count) do
                FY[iY].Data[maxGroupX-1].Join(FY[iY].Data[iX]);
              SetLength(FY[iY].Data, maxGroupX);
            end;

          for iX:=Pred(FX.Count) downto maxGroupX do
            begin
              FX[maxGroupX-1].Join(FX[iX]);
              FX.Delete(iX);
            end;

          FX[maxGroupX-1].ID:= unassigned;
          FX[maxGroupX-1].Caption := getTitle('_dL.other');
        end;
    end;

  if DescreteX and (aCache.Count>1) then
    begin
      if (FSortBy = FSortByName)  then QsortX(0, Pred(FX.Count), giCaption) else
      if (FSortBy = FSortByValue) then QsortX(0, Pred(FX.Count), giSum) else
                                       QsortX(0, Pred(FX.Count), giID);

      if (FSortBy = FSortByName) and assigned(FieldY) then
                      if FieldY.LookupPair<>nil       then QsortY(0, Pred(FY.Count), giCaption)
                                                      else QsortY(0, Pred(FY.Count), giID);

      if (FSortBy = FSortByNo)                        then QsortX(0, Pred(FX.Count), giID);
      if (FSortBy = FSortByNo) and assigned(FieldY)   then QsortY(0, Pred(FY.Count), giID);

      if (FSortBy = FSortByValue)                     then QsortX(0, Pred(FX.Count), giSum, vrGreaterThan);

      if (FSortBy = FSortByValue) and assigned(FieldY) then QsortY(0, Pred(FY.Count), giSum);
    end;

  {}
  finally
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  end;
  // ...................................................................................................................
  if (FChartType=ChartTypePie) or (1 < FY.Count) then
    begin
      SetLength(crtChart.ColorPalette,13);
      for i:=0 to 12 do
        crtChart.ColorPalette[i]:= SchemaColors[3+ (5*(i+1)) mod 13]; // перемешиваем 12 цветов + серый последний,
    end
  else
    begin
      SetLength(crtChart.ColorPalette,1);
      crtChart.ColorPalette[0]:= SchemaColors[15]; // только последний цвет,
    end;

  crtChart.Legend.CheckBoxes:= (1 < FY.Count) and Not DescreteX and not assigned(FRefreshTimer);

  if (FChartType=ChartTypePie) then
      begin
        crtChart.View3D :=FChart3D;
        crtChart.View3DOptions.Orthogonal := false;

        for i:=0 to Pred(FY.Count) do
          begin
            aSeries:=  TPieSeries.Create(Self);
            aSeries.Marks.Arrow.Color:= clBlack;
            aSeries.Marks.Visible:= true;
            TPieSeries(aSeries).Circled:= true;

            for j:=0 to Pred(Length(FY[i].Data)) do
              aSeries.Add(FY[i].Data[j].Value(MetaData.AgregateType), ReduceCaption(FX[j].Caption, 24));

            crtChart.AddSeries(aSeries);
          end;

        crtChart.BottomAxis.Labels:= false;
        crtChart.Legend.LegendStyle:= lsValues;
        crtChart.Title.Text.Text := FieldX.Native;
      end
   else
      begin
        crtChart.View3D :=FChart3D;

        if (FieldY <> nil) and (not FChartHide) and ( FY.Count > maxGroupY )
          then crtChart.Chart3DPercent:= Max ( 2, 128 div (FY.Count ))
          else crtChart.Chart3DPercent:= 8;

        crtChart.View3DOptions.Orthogonal := FChart3D;

        for i:=Pred(FY.Count) downto 0 do
          begin
            aSeries := TChartSeries(ChartTypes[Ord(FChartType)].Create(Self));
            aSeries.Title:= FY[i].Caption;
            if FChart3D then
                          begin
                            aSeries.Pen.Width:=1
                          end
                        else
                          begin
                            aSeries.Pen.Width:=2;
                            if (aSeries is TLineSeries) and ChartCurve  then
                              TLineSeries(aSeries).DrawStyle:= dsCurve;
                          end;

            aSeries.Marks.Visible:= False;
            aSeries.XValues.DateTime:= (FieldX.DataType in DateTimeTypes);
            if FChartStacked then TBarSeries(aSeries).MultiBar := mbStacked
                             else TBarSeries(aSeries).MultiBar := mbNone;
            TBarSeries(aSeries).ColorEachPoint := not (FieldY <> nil);

            for j:=Low(FY[i].Data) to High(FY[i].Data) do
              if FY[i].Data[j].IsDesrete
                then begin
                       V:= FY[i].Data[j].Value(MetaData.AgregateType);
                       if VarIsEmpty(V)
                         then aSeries.AddNull
                         else aSeries.Add(V, ReduceCaption(FX[j].Caption, 24))
                     end
                else if FY[i].Data[j].V = unassigned
                       then aSeries.AddNull
                       else begin
                              if Length(FieldX.Template)=0 then        s:= VarToStr(FY[i].Data[j].X) else
                              if FieldX.DataType in DateTimeTypes then s:= FormatDateTime(FieldX.Template, FY[i].Data[j].X) else
                                                                       s:= Format(FieldX.Template, [FY[i].Data[j].X]);
                              aSeries.AddXY(FY[i].Data[j].X, FY[i].Data[j].V, s);
                            end;

            crtChart.AddSeries(aSeries);
          end;

        crtChart.BottomAxis.Labels := True;
        crtChart.Legend.LegendStyle := lsSeries;

        crtChart.Title.Text.Text := FieldX.Native;
        if (FieldY <> nil) then
          crtChart.Title.Text.Text := crtChart.Title.Text.Text + ' * ' + FFieldY.Native;
        crtChart.Legend.Visible:=(FieldY <> nil);
        crtChart.DepthAxis.Visible:=(FieldY <> nil) and (not FChartStacked);

        crtChart.LeftAxis.Title.Caption:= GetLeftAxisTitle;
      end;

  if assigned(FieldY) then crtChart.Title.Text.Text:= FieldX.Native + ' + ' + FieldY.Native
                      else crtChart.Title.Text.Text:= FieldX.Native;

//  crtChart.BottomAxis.LabelsSeparation:=10;
  if (Not DescreteX) and (1<FY.Count) then crtChart.BottomAxis.LabelStyle:=talValue
                                      else crtChart.BottomAxis.LabelStyle:=talAuto;

  crtChart.BottomAxis.Logarithmic:= FChartLog and (FChartType = ChartTypeBarH);
  if DescreteX or (SizeX = 0) or  (FY.Count = 0) or (Length(FY[0].Data) = 0) then
    begin
      crtChart.BottomAxis.Automatic:= True;
    end
  else
    begin
      crtChart.BottomAxis.Automatic:= True;

    //  crtChart.BottomAxis.Automatic:= False;
      PMin:= FY[0].Data[0].X;
      PMax:= FY[0].Data[Pred(Length(FY[0].Data))].X;
      for i:= 1 to Pred(FY.Count) do
        begin
          if FY[i].Data[0].X < PMin then PMin:= FY[i].Data[0].X;
          if pMax < FY[i].Data[Pred(Length(FY[i].Data))].X then PMax:= FY[i].Data[Pred(Length(FY[i].Data))].X;
          if pMin > FY[i].Data[Pred(Length(FY[i].Data))].X then PMin:= FY[i].Data[Pred(Length(FY[i].Data))].X;
        end;

      if VarIsEmpty(FPositionX) then
        begin
          P:= (PMin + PMaX) / 2;
          crtChart.BottomAxis.SetMinMax(P - SizeX / 2, P + SizeX / 2);
        end
      else
        begin
          P:= PositionX;

          if FPositionMaxX or (P > (PMax - SizeX / 2)) then
            begin
              FPositionMaxX:= True;
              P:= PMax - SizeX / 2;
            end;

          if FPositionMinX or (P < (PMin + SizeX / 2)) then
            begin
              FPositionMinX:= True;
              P:= PMin + SizeX / 2;
            end;

          crtChart.BottomAxis.SetMinMax(P - SizeX / 2, P + SizeX / 2);
        end;
    end;

  crtChart.LeftAxis.Logarithmic:= FChartLog and (FChartType in [ChartTypeBarV, ChartTypeLine, ChartTypeArea, ChartTypePoint]);
  crtChart.LeftAxis.Automatic:= True;

  crtChart.AllowZoom:= false;
  crtChart.AllowPanning := TeeProcs.pmNone;
end;

function TDeChartForm.GetLeftAxisTitle: String;
begin
  if Not DescreteX
    then if Assigned(FieldA) then Result:= getTitle(FieldA.Name)
                             else Result:= EmptyStr
  else
    if (not Assigned(FieldA)) or (MetaData.AgregateType = atCount)
      then Result:= getTitle(TAgregateTypeNames[MetaData.AgregateType])
      else if (MetaData.AgregateType = atNo)
             then Result:= getTitle(FieldA.Name)
             else Result:= getTitle(TAgregateTypeNames[MetaData.AgregateType]+' ( '+FieldA.Name+' )');
end;

function    TDeChartForm.getCaptionText:string;
//var LeftTitle: String;
begin
  Result := inherited getCaptionText;
  {
  if assigned(FieldX) and Not(FieldX.Key)
    then Result:= Result + '   X: ' + FieldX.Native;

  if assigned(FieldY)
    then Result:= Result + '   Y: ' + FieldY.Native;
  {
  LeftTitle:= GetLeftAxisTitle;
  if 0 < Length(LeftTitle) then
    Result:= Result + '   ' + GetTitle('_Dl.results: ') + LeftTitle;
  {}
end;

function    TDeChartForm.getEnabledPrintStyles:TDePrintStyleTypes;
begin
  Result := [PS_Chart_TypeID];
end;

function    TDeChartForm.getPrintReport(aStyle : TDePrintStyle):TDeReport;
var sFile: String;
begin
  if aStyle.TypeID = PS_Chart_TypeID then
    try
//      sFile:= DeGetTempFileName(EmptyStr, 'DEI', sExtensionWMF);
//      crtChart.SaveToMetafileEnh(sFile);

      sFile:= DeGetTempFileName(EmptyStr, 'DEI', sExtensionBMP);
      crtChart.SaveToBitmapFile(sFile);

      DM.RegisterTemporaryFile(sFile);

      Result := TDeStdGraphicReport.Create;
      TDeStdGraphicReport(Result).Style:= aStyle;
      TDeStdGraphicReport(Result).Title:= GetCaptionText;
      TDeStdGraphicReport(Result).FileName:= sFile;
    finally
    end
  else
    Result := inherited getPrintReport(aStyle);
end;

procedure TDeChartForm.doPrintReport(aStyle: TDePrintStyle; const PrinterName : string);
begin
  inherited doPrintReport(aStyle, PrinterName);
end;

procedure TDeChartForm.act_Da_SavetofileExecute(Sender: TObject);
var SaveDlg: TSaveDialog;
    s: string;
begin
  SaveDlg:=TSaveDialog.Create(nil);
  SaveDlg.InitialDir  := Variables.AsString[RegDirPath];
  SaveDlg.Filter      := GetTitle('_Df.file BMP|*'+sExtensionBMP+
                                 '|_Df.file WMF|*'+sExtensionWMF);
  s:= GetCaptionText;
  if not DescreteX then
    if crtChart.BottomAxis.IsDateTime
      then if (crtChart.BottomAxis.Minimum-crtChart.BottomAxis.Maximum) < 1
             then s:= s + ' ('+  DateToStr(crtChart.BottomAxis.Minimum) + ' ' + TimeToStr(crtChart.BottomAxis.Minimum) + '-' +
                                                                                TimeToStr(crtChart.BottomAxis.Maximum) + ')'
             else s:= s + ' ('+  DateToStr(crtChart.BottomAxis.Minimum) + ' ' + TimeToStr(crtChart.BottomAxis.Minimum) + '-' +
                                 DateToStr(crtChart.BottomAxis.Maximum) + ' ' + TimeToStr(crtChart.BottomAxis.Maximum) + ')'
      else s:= s + ' ('+ FloatToStr(crtChart.BottomAxis.Minimum) + '-' + FloatToStr(crtChart.BottomAxis.Maximum) + ')';

  SaveDlg.FilterIndex := 0;
  SaveDlg.FileName    := NormalizeFileName(s, False);
  if SaveDlg.Execute then
    case SaveDlg.FilterIndex of
      1: crtChart.SaveToBitmapFile(SaveDlg.FileName + sExtensionBMP);
      2: crtChart.SaveToMetafileEnh(SaveDlg.FileName + sExtensionWMF);
    end;
  SaveDlg.Free;
end;

procedure TDeChartForm.DoMetrics(Sender: TObject);
begin
  if assigned(Sender) then
    if Sender is TMetaAction then
      begin
        if Assigned(TMetaAction(Sender).FieldMeta)
          then FieldX:= TMetaAction(Sender).FieldMeta
          else FieldX:= DataSetMeta.Table.KField[0];

        ReCreateView;
        TMetaAction(Sender).Checked:= true;
        FormCaption := getCaptionText;
      end;
end;

procedure TDeChartForm.DoResult(Sender: TObject);
begin
  if assigned(Sender) then
    if Sender is TMetaAction then
      begin
        FieldA:= TMetaAction(Sender).FieldMeta;
        ReCreateView;
        TMetaAction(Sender).Checked:= true;
        FormCaption := getCaptionText;
      end;
end;

procedure TDeChartForm.DoSplit(Sender: TObject);
begin
  if assigned(Sender) then
    if Sender is TMetaAction then
      begin
        FieldY:= TMetaAction(Sender).FieldMeta;
        ReCreateView;
        TMetaAction(Sender).Checked:= true;
        FormCaption := getCaptionText;
      end;
end;

function TDeChartForm.GetCanShowCard: Boolean;
begin
  Result:= inherited;
  Result:= Result and ((Not DescreteX) or (FieldX.Key));
end;

function TDeChartForm.ClearIndex;
var i:Integer;
begin
  for i:=0 to Pred(FY.Count) do
    SetLength(FY[i].Data,0);
  FY.Clear;
  FX.Clear;
  result:=True;
end;

procedure TDeChartForm.SetChartType(aChartType : TChartType);
begin
  if FChartType <> aChartType then
    begin
      FChartType := aChartType;

      act_Da_ChartPie.Checked:= FChartType = ChartTypePie;
      act_Da_ChartBarV.Checked:= FChartType = ChartTypeBarV;
      act_Da_ChartBarH.Checked:= FChartType = ChartTypeBarH;
      act_Da_ChartLine.Checked:= FChartType = ChartTypeLine;
      act_Da_ChartArea.Checked:= FChartType = ChartTypeArea;
      act_Da_ChartPoint.Checked:= FChartType = ChartTypePoint;
    end;
end;

procedure TDeChartForm.SetCanDrag(Value: Boolean);
begin
  FCanDrag:= False;
end;

procedure TDeChartForm.SetChart3D(aValue : Boolean);
begin
  FChart3D:=aValue;
  act_Da_Chart3D.Checked := FChart3D;
  act_Da_ChartCurve.Enabled:= not FChart3D;
end;

procedure TDeChartForm.SetChartHide(aValue : Boolean);
begin
  FChartHide:=aValue;
  act_Da_ChartHide.Checked := FChartHide;
end;

procedure TDeChartForm.SetChartStacked(aValue : Boolean);
begin
  FChartStacked:=aValue;
  act_Da_ChartStacked.Checked := FChartStacked;
end;

procedure TDeChartForm.SetChartLog(aValue : Boolean);
begin
  FChartLog:=aValue;
  act_Da_ChartLog.Checked := FChartLog;
end;

procedure TDeChartForm.SetChartCurve(aValue : Boolean);
begin
  FChartCurve:=aValue;
  act_Da_ChartCurve.Checked := FChartCurve;
end;

procedure   TDeChartForm.DeInitForm;
begin
  inherited DeInitForm;
end;

procedure   TDeChartForm.ChangeDataSetStatus;
begin
  act_Da_FastPrint.Enabled := Assigned(DataSetMeta)
            and DataSetMeta.Cache.Active
            and (CacheItems.Count > 0);
  act_Da_Copy.Enabled := True;
end;

procedure TDeChartForm.ChartTypeClick(Sender: TObject);
begin
  inherited;
  ChartType := TChartType(TAction(Sender).Tag);
  ReCreateView;
end;

procedure TDeChartForm.act_CopyExecute(Sender: TObject);
var
  Bitmap: TBitmap;
  OldStartColor: TColor;
  OldEndColor: TColor;
  hGlobal: THandle;
  Format: Word;
  Pallete: HPALETTE;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := crtChart.Width;
    Bitmap.Height := crtChart.Height;
    OldStartColor := crtChart.Gradient.StartColor;
    OldEndColor := crtChart.Gradient.EndColor;
    try
      crtChart.Gradient.StartColor := clWhite;
      crtChart.Gradient.EndColor := clWhite;
      crtChart.Draw(Bitmap.Canvas, Rect(0, 0, Bitmap.Width, Bitmap.Height));
      Clipboard.Open;
      try
        Format := CF_BITMAP;
        Bitmap.SaveToClipboardFormat(Format, hGlobal, Pallete);
        Clipboard.SetAsHandle(Format, hGlobal);
      finally
        Clipboard.Close;
      end;
    finally
      crtChart.Gradient.StartColor := OldStartColor;
      crtChart.Gradient.EndColor := OldEndColor;
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure TDeChartForm.act_Da_AgrNoExecute(Sender: TObject);
begin
  inherited;
  ActionEnabled(CategoryResults, not DescreteX);
  RecreateView;
end;

procedure TDeChartForm.act_Da_AgrCountExecute(Sender: TObject);
begin
  inherited;
  ActionEnabled(CategoryResults, not DescreteX);
  RecreateView;
end;

procedure TDeChartForm.act_Da_AgrAveExecute(Sender: TObject);
begin
  inherited;
  ActionEnabled(CategoryResults, True);
  RecreateView;
end;

procedure TDeChartForm.act_Da_AgrMinExecute(Sender: TObject);
begin
  inherited;
  ActionEnabled(CategoryResults, True);
  RecreateView;
end;

procedure TDeChartForm.act_Da_AgrMaxExecute(Sender: TObject);
begin
  inherited;
  ActionEnabled(CategoryResults, True);
  RecreateView;
end;

procedure TDeChartForm.act_Da_AgrSumExecute(Sender: TObject);
begin
  inherited;
  ActionEnabled(CategoryResults, True);
  RecreateView;
end;

procedure TDeChartForm.act_Da_AgrValueExecute(Sender: TObject);
begin
  inherited;
  ActionEnabled(CategoryResults, True);
  RecreateView;
end;

procedure TDeChartForm.act_Da_Chart3DExecute(Sender: TObject);
begin
  inherited;
  Chart3D := Not Chart3D;
  ReCreateView;
end;

procedure TDeChartForm.act_dA_ChartCurveExecute(Sender: TObject);
begin
  inherited;
  ChartCurve := Not ChartCurve;
  ReCreateView;
end;

procedure TDeChartForm.act_Da_ChartHideExecute(Sender: TObject);
begin
  inherited;
  ChartHide := Not ChartHide;
  ReCreateView;
end;

procedure TDeChartForm.act_Da_ChartStackedExecute(Sender: TObject);
begin
  inherited;
  ChartStacked := Not ChartStacked;
  ReCreateView;
end;

procedure TDeChartForm.AfterInit;
begin
  inherited;
end;

procedure TDeChartForm.act_Da_ChartLogExecute(Sender: TObject);
begin
  inherited;
  ChartLog := Not ChartLog;
  ReCreateView;
end;

function  TDeChartForm.SeriesByXY(X,Y:integer;var Index:integer):TChartSeries;
var i : integer;
begin
  Result := nil;
  Index  := -1;
  i := crtChart.SeriesCount - 1;
  while (i >= 0) and (Index < 0) do
    begin
      Result := crtChart.Series[i];
      Index := Result.Clicked(X,Y);
      dec(i);
    end;
  if Index < 0 then
    Result := nil;
end;

procedure TDeChartForm.crtChartMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseDownX:= X;
  FMouseDownY:= Y;
  FMouseDownMinX:= crtChart.BottomAxis.Minimum;
  FMouseDownMaxX:= crtChart.BottomAxis.Maximum;
  FMouseDownMinY:= crtChart.LeftAxis.Minimum;
  FMouseDownMaxY:= crtChart.LeftAxis.Maximum;
  if DescreteX then Exit;
  FMouseDown:= True;
end;

procedure TDeChartForm.crtChartMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseDown:= False;
end;

procedure TDeChartForm.crtChartMouseMove( Sender:TObject; Shift:TShiftState; X,Y:Integer);
var
  index   : integer;
  aSeries : TChartSeries;
  s       : string;
  f       : Double;
  R       : TRect;
begin
  if FMouseDown then
    begin

      if ([ssLeft] = Shift) or ([ssLeft, ssCtrl] = Shift) or ([ssLeft, ssCtrl, ssShift] = Shift) then
        begin
          F:= (FMouseDownX-X) * (FMouseDownMaxX - FMouseDownMinX) / (crtChart.BottomAxis.IAxisSize);
          PositionX:= (FMouseDownMinX + FMouseDownMaxX) / 2 + F;

          crtChart.BottomAxis.SetMinMax(FMouseDownMinX + F, FMouseDownMaxX + F);
        end;

      if ([ssLeft, ssShift] = Shift) or ([ssLeft, ssCtrl] = Shift) or ([ssLeft, ssCtrl, ssShift] = Shift)  then
        begin
          F:= (Y-FMouseDownY) * (FMouseDownMaxY - FMouseDownMinY) / (crtChart.LeftAxis.IAxisSize);
          crtChart.LeftAxis.SetMinMax(FMouseDownMinY + F, FMouseDownMaxY + F);
        end;

      crtChart.Invalidate;
    end
  else
    begin
      aSeries := SeriesByXY(X, Y, index);
      if DescreteX then
        begin
          if index >= 0 then
            begin
              if crtChart.SeriesCount > 1 then
                s := Format('%s - %s (%s)',[aSeries.XLabel[index], aSeries.Title, aSeries.YValueToText(aSeries.YValue[index])])
              else
                s := Format('%s (%s)',     [aSeries.XLabel[index],                aSeries.YValueToText(aSeries.YValue[index])])
            end
          else
                s:= EmptyStr;
        end
      else

        begin
          R:=crtChart.ChartRect;
          if (R.Left<=X) and (X<=R.Right) then
            begin
              f:= crtChart.BottomAxis.Minimum +
                 (crtChart.BottomAxis.Maximum - crtChart.BottomAxis.Minimum)*(X-R.Left)/R.Width;
              if crtChart.BottomAxis.IsDateTime then s:= DateToStr(f)+' '+TimeToStr(f)
                                                else s:= FloatToStr(f);
              if index >= 0 then
                s:= s + ' - ' + aSeries.Title;
            end
          else
                s:= EmptyStr;
        end;

      if FLastInfo <> s then
        begin
          SendMessage(Application.MainForm.Handle, DM_STATUSNOTIFY, NativeInt(s), Handle);
          FLastInfo := s;
        end;
    end;
end;

procedure TDeChartForm.crtChartAfterDraw(Sender: TObject);
begin
  inherited;

  if (FChartType=ChartTypePie) then
    begin
      crtChart.Canvas.Font.Assign(crtChart.LeftAxis.Title.Font);
      crtChart.Canvas.TextOut(crtChart.BorderWidth, crtChart.BorderWidth, GetLeftAxisTitle);
    end;
end;

procedure TDeChartForm.crtChartClick(Sender: TObject);
var
  i, index: integer;
  P: TPoint;
  S: TChartSeries;
  vID: Variant;
begin
  inherited;
  P:= crtChart.ScreenToClient(Mouse.CursorPos);
  S:= SeriesByXY(P.X, P.Y, index);

  if (Index < 0) or Not Assigned(S) then
    begin
      DataSetMeta.Cache.FocusedItem:= nil;
    end
  else
    begin
      vID:= unassigned;
      if DescreteX then
        vID:= FX[Index].ID
      else
        begin
          for i:=0 to Pred(crtChart.SeriesCount) do
            if crtChart.SeriesList[i]=S then
              if Index < Length(FY[i].Data) then
                begin
                  vID:= FY[i].Data[Index].ID;
                  Break;
                end;
        end;
      DataSetMeta.Cache.FocusedItem:= DataSetMeta.Cache[ DataSetMeta.Cache.IndexByID(vID) ]
    end;
end;

procedure TDeChartForm.crtChartDblClick(Sender: TObject);
var
  index   : integer;
  P       : TPoint;
begin
  P := crtChart.ScreenToClient(Mouse.CursorPos);
  SeriesByXY(P.X, P.Y, index);

  if Not DescreteX then
    begin
      if index >= 0 then DataSetMeta.Cache.FocusedItem:= DataSetMeta.Cache[Index]
                    else DataSetMeta.Cache.FocusedItem:= nil;
      if ViewArea then HideViewArea(True)
                  else ShowViewArea(True);
    end else

  if FieldX.Key then
    begin
      if index >= 0 then DataSetMeta.Cache.FocusedItem:= DataSetMeta.Cache[ DataSetMeta.Cache.IndexByID(FX[Index].ID) ]
                    else DataSetMeta.Cache.FocusedItem:= nil;
      if ViewArea then HideViewArea(True)
                  else ShowViewArea(True);
    end else

    begin
      if (index >= 0) and assigned(FieldX.LookupPair) then
          begin
            if EditRecord(FieldX.LookupPair.Owner, FX.Items[index].ID) then
              DataSetMeta.Cache.Update(mcUpdate, Null);
          end
        else
          sleep(9);
    end;

end;

procedure TDeChartForm.crtChartMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Axis: TChartAxis;
  P, d: Double;
begin
  inherited;
  if DescreteX then Exit;

  crtChart.BottomAxis.Automatic:= False;
  if WheelDelta<0 then d:= power(  2, 1/4)
                  else d:= power(1/2, 1/4);

  if (([ssShift] = Shift) or ([ssCtrl] = Shift)) or ([ssShift, ssCtrl] = Shift) then
    begin
      Axis:= crtChart.LeftAxis;
      P:= Axis.IAxisSize - (crtChart.ScreenToClient(MousePos).Y - Axis.IStartPos);

      if P < 0 then P:= 0;
      if P > Axis.IAxisSize then P:= Axis.IAxisSize;

      P:= Axis.Minimum + P * (Axis.Maximum - Axis.Minimum) / (Axis.IAxisSize);
      Axis.SetMinMax(P + (Axis.Minimum - P)*d, P + (Axis.Maximum - P)*d );
      Handled:= True;
    end;

  if (([] = Shift) or ([ssCtrl] = Shift)) or ([ssShift, ssCtrl] = Shift) then
    begin
      Axis:= crtChart.BottomAxis;
      P:= crtChart.ScreenToClient(MousePos).X - Axis.IStartPos;

      if P < 0 then P:= 0;
      if P > Axis.IAxisSize then P:= Axis.IAxisSize;
      P:= Axis.Minimum + P * (Axis.Maximum - Axis.Minimum) / (Axis.IAxisSize);

      SizeX:= SizeX * d;
      PositionX:= P + ( (Axis.Minimum + Axis.Maximum) / 2 - P ) * d;

      Axis.SetMinMax(P + (Axis.Minimum - P)*d, P + (Axis.Maximum - P)*d );
      Handled:= True;
    end;
end;

{ TGroupItem }

constructor TGroupItem.Create;
begin
  inherited;
end;

destructor TGroupItem.Destroy;
begin
  inherited;
end;

function TGroupItem.GetHeaderItem(const Index: Integer): variant;
begin       {
  if Index = giCaption then
    begin
      if VarIsEmpty(FHeader[giCaption]) then Result:= VarToStr(FHeader[giID])
                                        else Result:= VarToStr(FHeader[giCaption])
    end
  else    {}
    Result:= FHeader[Index];
end;

procedure TGroupItem.SetHeaderItem(const Index: Integer; const Value: variant);
begin
  FHeader[Index]:= Value;
end;

procedure TGroupItem.Join(const aGroupItem: TGroupItem);
begin
  FHeader[giID]:= unassigned;
//FHeader[giCaption]:= getTitle('_dL.other');
  FHeader[giCount]:= FHeader[giCount] + aGroupItem.FHeader[giCount];
  FHeader[giNNCount]:= FHeader[giNNCount] + aGroupItem.FHeader[giNNCount];

  if VarIsEmpty(FHeader[giMin]) or aGroupItem.FHeader[giMin]<FHeader[giMin] then FHeader[giMin]:= aGroupItem.FHeader[giMin];
  if VarIsEmpty(FHeader[giMax]) or aGroupItem.FHeader[giMax]>FHeader[giMax] then FHeader[giMax]:= aGroupItem.FHeader[giMax];

  if VarIsEmpty(FHeader[giSum]) then FHeader[giSum]:= aGroupItem.FHeader[giSum] else
  if Not VarIsEmpty(aGroupItem.FHeader[giSum]) then FHeader[giSum]:= FHeader[giSum] + aGroupItem.FHeader[giSum];
end;

function TGroupItem.Length: Integer;
begin
  Result:= System.Length(Data);
end;

{ TListGroup }

function TListGroup.GetItem(const Index: Integer): TGroupItem;
begin
  Result := TGroupItem(inherited Items[Index]);
end;

procedure TListGroup.PutItem(const Index: Integer; const Value: TGroupItem);
begin
  inherited Items[Index] := Value;
end;

function TListGroup.IndexByID(aValue: Variant): Integer;
var i: Integer;
begin
  for i:=0 to Pred(Count) do
    if Items[i].ID = aValue then
      Exit(i);
  result:=-1;
end;

function TListGroup.AddNew(aValue, aCaption: Variant): Integer;
var aLookupItem: TCacheItem;
begin
  Result:= Add(TGroupItem.Create);
  Items[Result].ID:= aValue;

  if VarIsEmpty(aCaption) and assigned(FieldMeta) and assigned(FieldMeta.LookupPair) then
    begin
      aLookupItem := TDataCache(FieldMeta.LookupPair.Lookup).FindByID(aValue);
      if Assigned(aLookupItem) then
        Items[Result].Caption := getTitle(aLookupItem.Caption)
      else
        begin
          if Length(VarToStr(aValue))=0 then Items[Result].Caption := getTitle('_Dv.Noname')
                                        else Items[Result].Caption := '[# '+VarToStr(aValue)+' ]';
        end;
    end

  else
     Items[Result].Caption:= VarToStr(aCaption);

  Items[Result].Count:= 0;
end;

{ TValuesItem }

procedure TValuesItem.ContinueItem(const aValue: Variant; const aX: Variant; const aID: Variant; const aNumber: Integer);
begin
  FSum:= aValue;
  FMin:= aX;
  FMax:= aID;
  FCount:= aNumber;
  FNotNullCount:= -1;
end;

procedure TValuesItem.DescreteItem;
begin
  FSum:= unassigned;
  FMin:= unassigned;
  FMax:= unassigned;
  FCount:= 0;
  FNotNullCount:= 0;
end;

procedure TValuesItem.DescreteValue(const aValue: Variant);
begin
  if not VarIsEmpty(aValue) and Not VarIsNull(aValue) then
    begin
      if FNotNullCount = 0 then
        begin
          FSum:= aValue;
          FMin:= aValue;
          FMax:= aValue;
        end
      else
        begin
          case VarCompareValue(aValue, FMin) of
            vrLessThan:    FMin:= aValue;
            vrGreaterThan: FMax:= aValue;
          end;
          FSum:= FSum + aValue;
        end;
      Inc(FNotNullCount);
    end;

  Inc(FCount);
end;

function TValuesItem.IsDesrete: Boolean;
begin
  Result:= (-1 < FNotNullCount);
end;

function TValuesItem.V: Variant;
begin
  Assert(FNotNullCount=-1, 'Item not Continue type');
  Result:= FSum;
end;

function TValuesItem.X: Variant;
begin
  Assert(FNotNullCount=-1, 'Item not Continue type');
  Result:= FMin;
end;

function TValuesItem.ID: Variant;
begin
  Assert(FNotNullCount=-1, 'Item not Continue type');
  Result:= FMax;
end;

function TValuesItem.N: Integer;
begin
  Assert(FNotNullCount=-1, 'Item not Continue type');
  Result:= FCount;
end;

function TValuesItem.Value(aType: TAgregateType): Variant;
begin
  if Not IsDesrete then Exit(FSum);

  case aType of
    atNo    : Result:= 1;
    atCount : if FCount = 0 then Result:= unassigned
                            else Result:= FCount;
    atValue : result:= FNotNullCount;
    atSum   : result:= FSum;
    atMin   : result:= FMin;
    atMax   : result:= FMax;
    atAverage: if FNotNullCount <= 0 then Result:= unassigned
                                     else Result:= FSum/FNotNullCount;
    else      result:= unassigned;
  end;
end;

procedure TValuesItem.Join(const aItem: TValuesItem);
begin
  FCount:= FCount + aItem.FCount;
  FNotNullCount:= FNotNullCount + aItem.FNotNullCount;

  if VarIsEmpty(FMin) or (aItem.FMin < FMin) then FMin:= aItem.FMin;
  if VarIsEmpty(FMax) or (aItem.FMax > FMax) then FMax:= aItem.FMax;

  if VarIsEmpty(FSum) then FSum:= aItem.FSum else
  if not VarIsEmpty(aItem.FSum) then FSum := FSum + aItem.FSum;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('ChartFormUnit unit initialization ...');

finalization
  DebugLog('ChartFormUnit unit finalization ...');
{$ENDIF}

end.

