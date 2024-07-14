unit DeStdReport;

interface

uses
  Windows, Contnrs, Classes, Graphics, Generics.Collections,
  DeTypes, DeReport, DSMeta, DeMeta, DePrintStyles;

const
  clExcelGray = $00E0E0E0;

type
  TDeStyledReport = class(TDeReport)
  private
    FDSMeta   : TDatasetMeta;
    FStyle    : TDePrintStyle;
    FList     : TFieldsMeta;
    FDinamic  : Boolean;
    FXML      : string;
    function GetStyle: TDePrintStyle;
    procedure SetStyle(aValue: TDePrintStyle);
    procedure PrepareList;
  public
    constructor Create;
    destructor Destroy; override;
    property Style: TDePrintStyle read GetStyle write SetStyle;
    property FieldsList: TFieldsMeta read FList;
    property Dinamic: Boolean read FDinamic write FDinamic;
    property XML: string read FXML write FXML;
  end;

type
  TDeStdExcelReport = class(TDeStyledReport)
  private
    FCurSheet: Integer;
    FSheetCount: Integer;
    function OpenExcel: OleVariant;
    procedure doProcess(ExcelApp: OleVariant);
    procedure SetCurSheet(const Value: Integer);
  protected
    procedure doPreview; override;
    procedure doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); override;
    procedure doPrintToFile(const aFileName: string); override;
    procedure doSaveToFile(const aFileName: string); override;
  public
    ExcelApp: Variant;
    Comment2: string;
    constructor Create(aDatasetMeta: TDatasetMeta);
    destructor Destroy;override;
    procedure CloseApp;
    procedure GetRecordFromFile(RecNo : integer; SheetNum: integer;  var sl: TStringList);
    function OpenFile(const FilePath: string): Boolean;
    function CloseAllFiles: Boolean;
    function SheetName(const Index: Integer): string;
    function SheetVisible(const Index: Integer): Boolean;
    function Comment: string;
    procedure GetContent(var R : OLEVariant; var ColCount:integer; var RowCount : integer);
    property CurSheet: Integer read FCurSheet write SetCurSheet;//текущий лист
    property SheetCount: Integer read FSheetCount;
  end;

type
  TDeCustomWordReport = class(TDeStyledReport)
  private
    function ProcessReport: OleVariant;
  protected
    function CheckDataReady: Boolean; virtual;
    function OpenWord: OleVariant; virtual;
    procedure doProcess(WordApp: OleVariant);virtual;
    procedure SetPageStyle(varDoc: OleVariant); virtual;
    procedure SetFontStyle(varFont: OleVariant; const FontStyle: string); virtual;
    procedure doPreview; override;
    procedure doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); override;
    procedure doPrintToFile(const aFileName: string); override;
    procedure doSaveToFile(const aFileName: string); override;
  public
//    constructor Create;
//    destructor Destroy; override;
  end;

type
  TDeStdWordReport = class(TDeCustomWordReport)
  private
  protected
    function CheckDataReady: Boolean; override;
    procedure doProcess(WordApp: OleVariant); override;
  public
    constructor Create(aDatasetMeta: TDatasetMeta);
//    destructor Destroy; override;
  end;

type
  TDeStdCardReport = class(TDeCustomWordReport)
  private
    procedure InsertGrid(wCell: OleVariant; DM, OwnerDM: TDatasetMeta);
  protected
    function CheckDataReady: Boolean; override;
    procedure doProcess(WordApp: OleVariant); override;
  public
    constructor Create(aDatasetMeta: TDatasetMeta);
//    destructor Destroy; override;
  end;

type
  TDeStdGraphicReport = class(TDeCustomWordReport)
  private
    FTitle: string;
    FFileName: String;
  protected
    function CheckDataReady: Boolean; override;
    procedure doProcess(WordApp: OleVariant); override;
  public
    property Title: string read FTitle write FTitle;
    property FileName: string read FFileName write FFileName;
  end;

implementation

uses UITypes, SysUtils, ComObj, Variants, DB, Math, XMLDoc, XMLIntf, Forms,
     DeLog, DataUnit, Funcs, Dictionary, DeSettings, Security, ElementsMeta, DeParser, DataCacheUnit, DeControls;

const
  msBorderLeft        =  1;
  msBorderRight       =  2;
  msBorderTop         =  3;
  msBorderBottom      =  4;
  msBorderLT_RB       =  5;
  msBorderLB_RT       =  6;
  msBorderRangeLeft   =  7;
  msBorderRangeRight  =  8;
  msBorderRangeTop    =  9;
  msBorderRangeBottom = 10;
  msBorderInnerV      = 11;
  msBorderInnerH      = 12;

  xlLandscape         = $00000002;
  xlPortrait          = $00000001;

const
  xlValidateInputOnly   = $00000000;
  xlValidateWholeNumber = $00000001;
  xlValidateDecimal     = $00000002;
  xlValidateList        = $00000003;
  xlValidateDate        = $00000004;
  xlValidateTime        = $00000005;
  xlValidateTextLength  = $00000006;
  xlValidateCustom      = $00000007;

  xlValidAlertStop      = $00000001;
  xlValidAlertWarning   = $00000002;
  xlValidAlertInformation = $00000003;

  xlBetween             = $00000001;
  xlHAlignCenter        = $FFFFEFF4;

  wdPropertyComments    = $00000005;
  wdPropertyTemplate = $00000006;

const
  wdBorderTop = $FFFFFFFF;
  wdBorderLeft = $FFFFFFFE;
  wdBorderBottom = $FFFFFFFD;
  wdBorderRight = $FFFFFFFC;
  wdBorderHorizontal = $FFFFFFFB;
  wdBorderVertical = $FFFFFFFA;

const
  wdLineStyleNone = $00000000;
  wdLineStyleSingle = $00000001;
  wdLineStyleDot = $00000002;
  wdLineStyleDashSmallGap = $00000003;
  wdLineStyleDashLargeGap = $00000004;
  wdLineStyleDashDot = $00000005;
  wdLineStyleDashDotDot = $00000006;
  wdLineStyleDouble = $00000007;
  wdLineStyleTriple = $00000008;
  wdLineStyleThinThickSmallGap = $00000009;
  wdLineStyleThickThinSmallGap = $0000000A;
  wdLineStyleThinThickThinSmallGap = $0000000B;
  wdLineStyleThinThickMedGap = $0000000C;
  wdLineStyleThickThinMedGap = $0000000D;
  wdLineStyleThinThickThinMedGap = $0000000E;
  wdLineStyleThinThickLargeGap = $0000000F;
  wdLineStyleThickThinLargeGap = $00000010;
  wdLineStyleThinThickThinLargeGap = $00000011;
  wdLineStyleSingleWavy = $00000012;
  wdLineStyleDoubleWavy = $00000013;
  wdLineStyleDashDotStroked = $00000014;
  wdLineStyleEmboss3D = $00000015;
  wdLineStyleEngrave3D = $00000016;

// Constants for enum WdLineWidth
const
  wdLineWidth025pt = $00000002;
  wdLineWidth050pt = $00000004;
  wdLineWidth075pt = $00000006;
  wdLineWidth100pt = $00000008;
  wdLineWidth150pt = $0000000C;
  wdLineWidth225pt = $00000012;
  wdLineWidth300pt = $00000018;
  wdLineWidth450pt = $00000024;
  wdLineWidth600pt = $00000030;

// Constants for enum WdRowAlignment
const
  wdAlignRowLeft = $00000000;
  wdAlignRowCenter = $00000001;
  wdAlignRowRight = $00000002;

// Constants for enum WdRulerStyle
const
  wdAdjustNone = $00000000;
  wdAdjustProportional = $00000001;
  wdAdjustFirstColumn = $00000002;
  wdAdjustSameWidth = $00000003;

// Constants for enum WdParagraphAlignment
const
  wdAlignParagraphLeft = $00000000;
  wdAlignParagraphCenter = $00000001;
  wdAlignParagraphRight = $00000002;
  wdAlignParagraphJustify = $00000003;


const
  wdColorBlack  = 0;
  wdColorGray50 = $808080;

// Constants for enum WdTextureIndex
const
  wdTextureNone = $00000000;
  wdTexture2Pt5Percent = $00000019;
  wdTexture5Percent = $00000032;
  wdTexture7Pt5Percent = $0000004B;
  wdTexture10Percent = $00000064;
  wdTexture12Pt5Percent = $0000007D;
  wdTexture15Percent = $00000096;
  wdTexture17Pt5Percent = $000000AF;
  wdTexture20Percent = $000000C8;
  wdTexture22Pt5Percent = $000000E1;
  wdTexture25Percent = $000000FA;
  wdTexture27Pt5Percent = $00000113;
  wdTexture30Percent = $0000012C;
  wdTexture32Pt5Percent = $00000145;
  wdTexture35Percent = $0000015E;
  wdTexture37Pt5Percent = $00000177;
  wdTexture40Percent = $00000190;
  wdTexture42Pt5Percent = $000001A9;
  wdTexture45Percent = $000001C2;
  wdTexture47Pt5Percent = $000001DB;
  wdTexture50Percent = $000001F4;
  wdTexture52Pt5Percent = $0000020D;
  wdTexture55Percent = $00000226;
  wdTexture57Pt5Percent = $0000023F;
  wdTexture60Percent = $00000258;
  wdTexture62Pt5Percent = $00000271;
  wdTexture65Percent = $0000028A;
  wdTexture67Pt5Percent = $000002A3;
  wdTexture70Percent = $000002BC;
  wdTexture72Pt5Percent = $000002D5;
  wdTexture75Percent = $000002EE;
  wdTexture77Pt5Percent = $00000307;
  wdTexture80Percent = $00000320;
  wdTexture82Pt5Percent = $00000339;
  wdTexture85Percent = $00000352;
  wdTexture87Pt5Percent = $0000036B;
  wdTexture90Percent = $00000384;
  wdTexture92Pt5Percent = $0000039D;
  wdTexture95Percent = $000003B6;
  wdTexture97Pt5Percent = $000003CF;
  wdTextureSolid = $000003E8;
  wdTextureDarkHorizontal = $FFFFFFFF;
  wdTextureDarkVertical = $FFFFFFFE;
  wdTextureDarkDiagonalDown = $FFFFFFFD;
  wdTextureDarkDiagonalUp = $FFFFFFFC;
  wdTextureDarkCross = $FFFFFFFB;
  wdTextureDarkDiagonalCross = $FFFFFFFA;
  wdTextureHorizontal = $FFFFFFF9;
  wdTextureVertical = $FFFFFFF8;
  wdTextureDiagonalDown = $FFFFFFF7;
  wdTextureDiagonalUp = $FFFFFFF6;
  wdTextureCross = $FFFFFFF5;
  wdTextureDiagonalCross = $FFFFFFF4;

const
  wdOrientPortrait = $00000000;
  wdOrientLandscape = $00000001;

const
  xlLocalSessionChanges = $00000002;
  xlOtherSessionChanges = $00000003;
  xlUserResolution = $00000001;

{TDeStyledReport}
constructor TDeStyledReport.Create;
begin
  inherited Create;
  FStyle    := nil;
  FDinamic  := False;
  FXML      := EmptyStr;
  FList:=TFieldsMeta.Create(False);
end;

destructor  TDeStyledReport.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function    TDeStyledReport.getStyle:TDePrintStyle;
begin
  Result := FStyle;
end;

procedure   TDeStyledReport.setStyle(aValue:TDePrintStyle);
begin
  FStyle := aValue;
end;

procedure   TDeStyledReport.PrepareList;
var i: Integer;
begin
  if FList.Count>0 then Exit;
  for i:=0 to FDSMeta.Table.Fields.Count-1 do
    FList.Add(FDSMeta.Table.Fields[i]);
end;

function Cell(x,y:Integer):String;
begin
  if x>26 then result:=CHR(64+(x-1)div(26))+CHR(65+(x-1)mod(26))+IntToStr(y)
          else result:=CHR(64+x)+IntToStr(y);
end;

function MillimetersToPoints(Millimiters:single):single;
begin
  Result := 2.85*Millimiters;
end;

function PointsToMillimeters(Points:single):single;
begin
  Result := Points / 2.85;
end;

{TDeStdExcelReport}
constructor TDeStdExcelReport.Create(aDatasetMeta:TDatasetMeta);
begin
  inherited Create;
  FDSMeta := aDatasetMeta;
  Comment2:= EmptyStr;
end;

destructor  TDeStdExcelReport.Destroy;
begin
  inherited Destroy;
end;

function    TDeStdExcelReport.OpenExcel:OLEVariant;
begin
  Result := unAssigned;
  {
  try
    Result := GetActiveOleObject(ExcelName);
  except
    Result := CreateOleObject(ExcelName);
  end;
  {}
  Result := CreateOleObject(ExcelName);

  if varIsEmpty(Result) then exit;
  Result.Visible := False;
  Result.DisplayAlerts := false;
end;

procedure   TDeStdExcelReport.doProcess(ExcelApp:OLEVariant);
var
  Sheet       : variant;
  FieldList   : TList<Integer>;
  R           : OleVariant;
  bm          : TBitMap;
  sFile       : String;
  i,NN,LL, X,Y,W, J : Integer;
begin

  if FDSMeta = nil then exit;
  ExcelApp.Workbooks.Add;
  Sheet := ExcelApp.ActiveSheet;
  //... Определение видимых колонок и ширин, подготовка массива ...
  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, 100, 1);
  try
  W := 0;
  NN:= 0;
  FieldList:= TList<Integer>.Create;
  for i:=0 to FDSMeta.Table.Fields.Count-1 do
  begin
    if (FDSMeta.Table.Fields[i].VisibleLevel >= fvLevel2) then
    begin
      Inc(NN);
      Inc(W, FDSMeta.Table.Fields[i].Width);
      FieldList.Add(I);
    end;
  end;

  R := VarArrayCreate([ 0, FDSMeta.Cache.Count, 0, NN], varOleStr);

  //... Читаем данные таблицы в массив ...
  LL:=0;
  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, 10, 0);
  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, FDSMeta.Cache.Count, 80);
  try
  for I := 0 to FDSMeta.Cache.Count-1 do
  begin
    SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, I, 0);
    Inc(LL);
    R[LL, 0]:=IntToStr(LL);
    for J:=0 to FieldList.Count-1 do
      R[LL, J+1]:= FDSMeta.Cache[I].FieldText(FieldList[J]);
  end;
  finally
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  end;

  //... Выводим данные в Excel ...
  // Заголовок
  Sheet.Cells[1,3].Font.Italic:= True;
  //Sheet.Cells[1,3].HorizontalAlignment := 4;
  Sheet.Cells[1,3]:=GetTitle('_date')+': '+DateToStr(Date);
  Sheet.Cells[2,3].Font.Bold  := True;
  Sheet.Cells[2,3]:=GetTitle(FDSMeta.Table.Name);
  // Рисунок
  sFile := DeGetTempFileName(EmptyStr, 'DEI', sExtensionBMP);
  if sFile <> EmptyStr then
  begin
    BM := TBitMap.Create;
    try
      BM.PixelFormat := pf24bit;
      BM.Width := DM.ilIcon32.Width;
      BM.Height := DM.ilIcon32.Height;
      BM.Canvas.Brush.Color := clWhite;
      BM.Canvas.FillRect(Rect(0, 0, BM.Width, BM.Height));
      DM.ilIcon32.Draw(BM.Canvas, 0, 0, DM.MapIconIndex(FDSMeta.ICO));
      //DM.ImagesWhite.GetBitmap(FDSMeta.Table.ICO , BM);
      BM.SaveToFile(sFile);
      //BM.FreeImage;
    finally
      BM.Free;
    end;
  end;

  // Столбцы
  R[0,0]:='№';
  Sheet.Columns[1].ColumnWidth:=2;
  Sheet.Columns[2].ColumnWidth:=6;
  NN:=0;
  for i:=0 to FDSMeta.Table.Fields.Count-1 do
  begin
    if (FDSMeta.Table.Fields[i].VisibleLevel >= fvLevel2) then
    begin
      Inc(NN);
      R[0,NN] := FDSMeta.Table.Fields[i].Native;
      Sheet.Columns[NN+2].ColumnWidth := (72*FDSMeta.Table.Fields[i].Width) div W;
    end;
  end;
  FieldList.Free;
  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, 90, 0);

  if sFile <> EmptyStr then
  begin
    X := 14; // MSS.Cells[ 2, 2].MergeArea.Left;
    Y := 13; // MSS.Cells[ 2, 2].MergeArea.Top;
    Sheet.Shapes.AddPicture(sFile, True, True, X, Y, True, True);
  end;
  DeleteFile(sFile);

  // Данные
  Sheet.Range[Cell( 2, 4  )+':'+Cell(NN+2,4+LL)].WrapText := True;
  Sheet.Range[Cell( 2, 4  )+':'+Cell(NN+2,4+LL)].Value := R;

  // Рамки
  Sheet.Range[Cell( 3, 1  )+':'+Cell(NN+2, 1  )].Merge;
  Sheet.Range[Cell( 3, 2  )+':'+Cell(NN+2, 2  )].Merge;

  Sheet.Range[Cell( 3, 1  )+':'+Cell(NN+2, 1  )].Borders[msBorderBottom].Weight   :=3;
  Sheet.Range[Cell( 2, 4  )+':'+Cell(NN+2,4+LL)].Borders.Linestyle:=1;

  R:= Sheet.Range[Cell( 2, 4  )+':'+Cell(NN+2, 4  )];
  R.Interior.Color := clSilver;
  R.Borders[msBorderTop]   .Weight :=3;
  R.Borders[msBorderBottom].Weight :=3;
  Sheet.Range[Cell( 2,4+LL)+':'+Cell(NN+2,4+LL)].Borders[msBorderBottom].Weight :=3;

  finally
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  end;
end;

procedure   TDeStdExcelReport.doPreview;
var
  ExcelApp : OLEVariant;
begin
  if FDSMeta = nil then exit;
  ExcelApp := OpenExcel;
  if varIsEmpty(ExcelApp) then exit;
  doProcess(ExcelApp);
  ExcelApp.DisplayAlerts:=true;
  ExcelApp.Visible := True;
  ExcelApp  := unAssigned;
end;

procedure TDeStdExcelReport.doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean);
var
  ExcelApp    : variant;
begin
  if FDSMeta = nil then exit;
  ExcelApp := OpenExcel;
  if varIsEmpty(ExcelApp) then exit;
  doProcess(ExcelApp);

  ExcelApp.ActiveSheet.PrintOut(From:=1,
                                To:=9999,
                                Copies:=CopyCount,
                                Preview:=False,
                                ActivePrinter:=PrinterName,
                                Collate:=SplitCopies);
  ExcelApp.Quit;
  ExcelApp  := unAssigned;
end;

procedure TDeStdExcelReport.doPrintToFile(const aFileName: string);
var
  ExcelApp    : variant;
begin
  if FDSMeta = nil then exit;
  ExcelApp := OpenExcel;
  if varIsEmpty(ExcelApp) then exit;
  doProcess(ExcelApp);
  ExcelApp.ActiveSheet.PrintOut(PrintToFile := true,PrToFileName := aFileName);
  ExcelApp.Quit;
  ExcelApp  := unAssigned;
end;


procedure TDeStdExcelReport.doSaveToFile(const aFileName: string);
var
  ExcelApp    : variant;
  Sheet       : variant;
  V,WB        : variant;
  R,R2        : OleVariant;
  i,ii,iiCount,NN,LibCount,LL,W,J : Integer;
  s,tt        : string;
  sFile       : string;
  f           : TextFile;
  LibCache    : TDataCache;
  Index       : Array of Integer;

  sGuard, sAutoFilter, sPasswordEdit, sScenarios, sFormattingCells, sFormattingColumns, sFormattingRows,
  sInsertingColumns, sInsertingRows, sInsertingHyperlinks, sDeletingColumns,
  sDeletingRows, sSorting, sFiltering, sUsingPivotTables, sValidateList: OleVariant;

Function ReadComment(aComment: string): Boolean;
var XMLDoc   : TXMLDocument;
    Node     : IXMLNode;
    NodeName : String;
    ii,i     : Integer;
    s        : String;
begin
  Result:=False;
  sGuard             := True;
  sAutoFilter        := True;
  sPasswordEdit      := EmptyStr;
  sScenarios         := True;
  sFormattingCells   := True;
  sFormattingColumns := True;
  sFormattingRows    := True;
  sInsertingColumns  := False;
  sInsertingRows     := False;
  sInsertingHyperlinks:=False;
  sDeletingColumns   := False;
  sDeletingRows      := False;
  sSorting           := True;
  sFiltering         := True;
  sUsingPivotTables  := True;
  sValidateList      := True;

  XMLDoc:= TXMLDocument.Create(Application.MainForm);

  try
    XMLDoc.XML.Text:=aComment;
    XMLDoc.Active:=True;
	except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('ReadComment skip error: ' + E.Message);
    {$ENDIF}
  end;

  if XMLDoc.Active then
    for ii := 0 to XMLDoc.ChildNodes.Count-1 do
      if AnsiCompareStr(XMLDoc.ChildNodes[ii].NodeName,'IMPORT')=0 then
        for i := 0 to XMLDoc.ChildNodes[ii].ChildNodes.Count-1 do
          begin
            Node:=XMLDoc.ChildNodes[ii].ChildNodes[i];
            NodeName:=Node.NodeName;
            if AnsiCompareStr(NodeName,'AUTOFILTER')=0 then sAutoFilter     := (Trim(Node.Text)='1') else
            if AnsiCompareStr(NodeName,'GUARD')=0 then Result               := (Trim(Node.Text)='1') else
            if AnsiCompareStr(NodeName,'GPE' )=0  then
              begin s :=  XMLDecode(Node.Text); sPasswordEdit := s; end else
            if AnsiCompareStr(NodeName,'GS'  )=0  then sScenarios           := (Trim(Node.Text)='1') else
            if AnsiCompareStr(NodeName,'GAFS')=0  then sFormattingCells     := (Trim(Node.Text)='1') else
            if AnsiCompareStr(NodeName,'GAFC')=0  then sFormattingColumns   := (Trim(Node.Text)='1') else
            if AnsiCompareStr(NodeName,'GAFR')=0  then sFormattingRows      := (Trim(Node.Text)='1') else
            if AnsiCompareStr(NodeName,'GAIC')=0  then sInsertingColumns    := (Trim(Node.Text)='1') else
            if AnsiCompareStr(NodeName,'GAIR')=0  then sInsertingRows       := (Trim(Node.Text)='1') else
            if AnsiCompareStr(NodeName,'GAIH')=0  then sInsertingHyperlinks := (Trim(Node.Text)='1') else
            if AnsiCompareStr(NodeName,'GADC')=0  then sDeletingColumns     := (Trim(Node.Text)='1') else
            if AnsiCompareStr(NodeName,'GADR')=0  then sDeletingRows        := (Trim(Node.Text)='1') else
            if AnsiCompareStr(NodeName,'GAS' )=0  then sSorting             := (Trim(Node.Text)='1') else
            if AnsiCompareStr(NodeName,'GAF' )=0  then sFiltering           := (Trim(Node.Text)='1') else
            if AnsiCompareStr(NodeName,'GAUP')=0  then sUsingPivotTables    := (Trim(Node.Text)='1') else
            if AnsiCompareStr(NodeName,'FKV' )=0  then sValidateList        := (Trim(Node.Text)='1');
          end;

  XMLDoc.Free;
end;

function PrepareExcelStringValue( v: string) : string;
begin
  result:=v;
//  while Pos('"',result)>0 do  Delete(result,Pos('"',result),1);

  if Length(result)>900 then result:=Copy(result,1,900)+'...';

  // переводим перевод строки к формату Эксель
  result:=StringReplace(result, #13#10, #10, [rfReplaceAll]);

  // заменяем табуляции на пробел
  result:=StringReplace(result, #9, #32, [rfReplaceAll]);

  // если строка начинается с ковычки или знака равно, то добавляем  символ одиночной ковычки
  if (Copy(result,1,1)='''') then result := ''''+result;
  if (Copy(result,1,1)='=')  then result := ''''+result;
end;

begin
  if FDSMeta = nil then exit;
  PrepareList;

  ExcelApp := OpenExcel;
  if varIsEmpty(ExcelApp) then exit;
//  doProcess(ExcelApp);
////////////////////////////////////////////////////////////////////////////////

  WB:=ExcelApp.Workbooks.Add;

  Sheet := WB.ActiveSheet ;//ExcelApp.ActiveSheet;
  //... Определение ширин столбцов, подготовка массива ...

  NN:=FieldsList.Count;
  SetLength(Index, NN);
  LL:=FDSMeta.Cache.Count;

  for i:=0 to FieldsList.Count-1 do
  begin
    Index[i]:= FDSMeta.Table.Fields.IndexByID(FieldsList[i].ID);
    if (FieldsList[i].Link>0) then
      begin
        if FieldsList[i].IsLookup then W:= FieldsList[i].LookupPair.Width div 4
                                  else W:= 10;
      end
    else
      W:= FieldsList[i].Width div 4;

    if W<10  then W:=10;
    if W>150 then W:=150;
    Sheet.Columns[i+1].ColumnWidth := W;
    Sheet.Cells[ 1,i+1].Value := FieldsList[i].Native;

    Sheet.Cells[ 1,i+1].AddComment;
    Sheet.Cells[ 1,i+1].Comment.Text( Text:=FieldsList[i].Original );

    R:=Sheet.Range[Cell(1+I,2)+':'+Cell(1+I,1+LL)];

    if FieldsList[i].DataType in StringTypes then R.NumberFormat := '@';

    if not FieldsList[i].IsReadOnly then R.Locked:=False
                                    else R.Interior.Color := clExcelGray;
  end;

// закрепляем область до выгрузки данных и прокрутки листа, т.к. закрепляет верхнюю ВИДИМУЮ строку
  Sheet.Range[Cell( 1,2)+':'+Cell(1,2)].Select;
  Sheet.Application.ActiveWindow.FreezePanes:= True;

//... Читаем данные таблицы в массив ...
  iiCount:=( 65535 div (NN+1) );
//  iiCount:=500;
  ii:=0;
  if iiCount>LL then iiCount:=LL;

  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, FDSMeta.Cache.Count, 1);
  try
  for I := 0 to LL-1 do
  begin
    if (I mod iiCount) = 0 then
      begin
        //создаем новый буфер
        R2 := VarArrayCreate([ 0, iiCount-1, 0, NN-1], varOleStr);
        II := 0;
      end;

    FDSMeta.Cache.FillItems(I,I); // сильно ускоряет начитывавание

    for J:=0 to NN-1 do
      begin
        V:=FDSMeta.Cache[I].FieldValue[Index[J]];
        if not VarIsEmpty(V) then
        begin
          if VarCompareValue(V, Null)=vrEqual then
           //  R2[II, J]:=EmptyStr
          else

            if FieldsList[J].DataType in StringTypes then
              begin
                R2[II, J] := PrepareExcelStringValue(FDSMeta.Cache[I].FieldText(Index[J]));
              end else

            if FieldsList[J].DataType in BinaryTypes then
              begin
                tt :=  VarAsType(V,varString);

  {              resBLOB := FDSMeta.Cache[I].FieldValue[integer(FieldList[J])];
                setLength(sBlob,2*Length(resBLOB));
                BinToHex(PChar(resBlob),PChar(sBlob),2*Length(resBLOB));
                for k := 1 to Length(resBLOB) do
                begin
                  s :=IntToHex(Ord(sBlob[k]),2);
                  sBLOB[2*k-1] := s[1];
                  sBLOB[2*k] := s[2];
                end;
  }
                if Length(tt)>0 then
                  begin
                    sFile := DeGetTempFileName(EmptyStr,'DEI','.txt');
                    AssignFile(f, sFile);
                    Rewrite(f);
                    Write(f,tt);
                    CloseFile(f);
                    Sheet.OLEObjects.Add(EmptyParam, sFile);
  //                  Sheet.OLEObjects.Add(EmptyParam, sFile, DisplayAsIcon:=true, Left:=LL, Top:=J);
  //                  Sheet.OLEObjects.Add(EmptyParam, 'sFile', DisplayAsIcon := true, Left := LL, Top := J);
                    R2[II, J]:= ExtractFileName(sFile)
                  end
                else
                  begin
                    R2[II, J]:= EmptyStr
                  end;
              end else

          {if FieldsList[J].DataType = остальные типы  }
              begin
                R2[II, J]:= V;//VarAsType(V,varString);
              end;
        end;
      end;
    SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, i, 0);
    Inc(ii);

    //пишем в файл наполненый буфер
    if ((I+1) mod iiCount) = 0  then
      begin
        Sheet.Range[Cell( 1, 2+I-(iiCount-1) )              +':'+Cell(NN, I+2)].Value := R2;
        Sheet.Range[Cell( 1, 2+I-(iiCount-1) )              +':'+Cell(NN, I+2)].WrapText := True;
        Sheet.Range[Cell( 1, 2+I-(iiCount-1) )              +':'+Cell(NN, I+2)].Borders.Linestyle:=1;
      end else

    // пишем в файл последний неполный буфер
    if  (I+1)              = LL then
      begin
        Sheet.Range[Cell( 1, 2+((I+1) div iiCount)*iiCount )+':'+Cell(NN, I+2)].Value := R2;
        Sheet.Range[Cell( 1, 2+((I+1) div iiCount)*iiCount )+':'+Cell(NN, I+2)].WrapText := True;
        Sheet.Range[Cell( 1, 2+((I+1) div iiCount)*iiCount )+':'+Cell(NN, I+2)].Borders.Linestyle:=1;
      end;
  end;

  finally
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  end;

  DeleteFile(sFile);

  R:= Sheet.Range[Cell( 1, 1 )+':'+Cell(NN, 1)];
  R.Interior.Color := clExcelGray;
  R.Font.Bold := True;
  //R.HorizontalAlignment := xlHAlignCenter;
  R.Borders[msBorderBottom].Weight :=3;

  R:= Sheet.Range[Cell( 1,1+LL)+':'+Cell(NN,1+LL)];
  R.Borders[msBorderBottom].Weight :=3;

  if Length(Comment2)>0 then
    WB.BuiltInDocumentProperties(wdPropertyComments):=Comment2;
   // ExcelApp.ActiveWorkbook.BuiltInDocumentProperties(wdPropertyComments):=Comment2;

  sGuard := ReadComment(Comment2);

  // Выгружаем справочники
  if Dinamic then
    begin
      while ExcelApp.Sheets.Count>1 do
        ExcelApp.Sheets[2].Delete;

      LibCount:=0;
      for i:=0 to FieldsList.Count-1 do
        if FieldsList[i].IsLookup then
           Inc(LibCount);

      SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, LibCount, 1);
      try
      NN:=0;
      for i:=0 to FieldsList.Count-1 do
        if FieldsList[i].IsLookup then
          begin
            SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY,
              NativeInt(PChar('_dL.directories '+IntToStr(NN+1)+'/'+IntToStr(LibCount)+' : '+FieldsList[i].Native)), -2);
            SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, NN, 0);

            Inc(NN);
            s:=FieldsList[i].LookupPair.Original;

            LibCache :=TDataCache.Create( FieldsList[i].LinkTable);
            LibCache.SortList.Add(LibCache.TableMeta.NField.ID);
            LibCache.PrepareData;

            ExcelApp.Sheets[1].Activate;
            ExcelApp.Sheets.Add( After:=ExcelApp.Sheets[NN] );
            Sheet:=ExcelApp.ActiveSheet;
            Sheet.Name := 'Lib'+s;
            Sheet.Columns[1].ColumnWidth := 50;
            Sheet.Visible := False;

            if LibCache.Count>0 then
              begin
                SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, LibCache.Count, 1);
                try
                R2 := VarArrayCreate([ 0, LibCache.Count-1, 0, 1], varOleStr);
                for j:=0 to LibCache.Count-1 do
                  begin
                    R2[ j, 0 ]:= PrepareExcelStringValue(LibCache.Items[j].Caption);
                    R2[ j, 1 ]:= LibCache.Items[j].ID;
                    SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, j, 0);
                  end;
                Sheet.Range[Cell( 1, 1  )+':'+Cell(2,LibCache.Count)].Value := R2;
                finally
                  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
                end;
              end;

            WB.Names.Add( Name:='Reg_'+s,
            RefersToR1C1:='=Lib'+s+'!R1C1:R'+IntTostr(LibCache.Count)+'C1' );

            ExcelApp.Sheets[1].Activate;

            R:= ExcelApp.Sheets[1].Range[Cell( i+1, 2 )+':'+Cell(i+1, FDSMeta.Cache.Count+1)];
            R.Interior.Color := RGB(255,255,204);

            if LibCache.Count>0 then
              begin
                R.Interior.Color :=RGB(255,255,204);
                R.Validation.Delete;

                if sValidateList then
                  begin
                    R.Validation.Add( Type:=xlValidateList, AlertStyle:=xlValidAlertStop,
                                    Operator:= xlBetween, Formula1:='='+'Reg_'+s);
                    R.Validation.ShowError := True;
                  end
                else
                  begin
                    R.Validation.Add( Type:=xlValidateList, AlertStyle:=xlValidAlertInformation,
                                    Operator:= xlBetween, Formula1:='='+'Reg_'+s);
                    R.Validation.ShowError := False;
                  end;

                R.Validation.IgnoreBlank := True ;
                R.Validation.InCellDropdown := True;
                R.Validation.ShowInput := True;
              end;

            LibCache.Free;
          end;
      finally
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
      end;
    end;

// Устанавливаем автофильтр
  if sAutoFilter=True then
    begin
      ExcelApp.Sheets[1].Range[Cell(1,1)+':'+Cell(FieldsList.Count,1)].Select;
      ExcelApp.Selection.AutoFilter;
    end;

// http://msdn.microsoft.com/ru-ru/library/microsoft.office.tools.excel.worksheet.protect.aspx?cs-save-lang=1&cs-lang=csharp#code-snippet-1
   if sGuard=True then ExcelApp.ActiveSheet.Protect(
                                    Password              := sPasswordEdit,          //Пароль
                                    Scenarios             := sScenarios,             //Изменение сценариев
                                    AllowFormattingCells  := sFormattingCells,       //Форматирование ячеек
                                    AllowFormattingColumns:= sFormattingColumns,     //Форматирование столбцов
                                    AllowFormattingRows   := sFormattingRows,        //Форматирование строк
                                    AllowInsertingColumns := sInsertingColumns,      //Вставка столбцов
                                    AllowInsertingRows    := sInsertingRows,         //Вставка строк
                                    AllowInsertingHyperlinks:=sInsertingHyperlinks,  //Вставка гиперссылок
                                    AllowDeletingColumns  := sDeletingColumns,       //Удаление столбцов
                                    AllowDeletingRows     := sDeletingRows,          //Удаление строк
                                    AllowSorting          := sSorting,               //Сортировка
                                    AllowFiltering        := sFiltering,             //Использование автофильтра
                                    AllowUsingPivotTables := sUsingPivotTables       //Использование сводных таблиц
                                                     )
                  else ExcelApp.ActiveSheet.UnProtect;

//вставляем BLOB поля как OLE объекты
{  if prBlob then
    begin
      LL:=0;
      for I := 0 to DatasetMeta.Cache.Count-1 do
      begin
        Inc(LL);
        for J:=0 to FieldList.Count-1 do
          if DatasetMeta.Fields[integer(FieldList[J])].DataType in BinaryTypes then
            begin
              if (not VarIsEmpty(DatasetMeta.Cache[I].FieldValue[integer(FieldList[J])]))
                 and (not (VarCompareValue(DatasetMeta.Cache[I].FieldValue[integer(FieldList[J])], Null)=vrEqual))
              then
              begin
                sBlob:=VarToStr(DatasetMeta.Cache[I].FieldValue[integer(FieldList[J])]);
                if sBlob<>EmptyStr then
                  begin
                    prBlob := true;
                    sFile := DeGetTempFileName(EmptyStr,'DEI','.txt');
                    AssignFile(f, sFile);
                    Rewrite(f);
                    Write(f,sBlob);
                    CloseFile(f);
                    Sheet.OLEObjects.Add(EmptyParam, sFile);
//                  Sheet.OLEObjects.Add(EmptyParam, sFile, DisplayAsIcon:=true, Left:=LL, Top:=J);
//                  Sheet.OLEObjects.Add(EmptyParam, 'sFile', DisplayAsIcon := true, Left := LL, Top := J);
                  end;
              end;
            end;
      end;
    end;}
////////////////////////////////////////////////////////////////////////////////
  ExcelApp.DisplayAlerts := false;
  ExcelApp.ActiveWorkbook.SaveAs(FileName := aFileName,
                       ConflictResolution := xlLocalSessionChanges);
  ExcelApp.Quit;
  ExcelApp := unAssigned;
end;


procedure TDeStdExcelReport.GetRecordFromFile(RecNo: integer; SheetNum: integer; var sl: TStringList);
var
  R           : OLEVariant;
  UsedRange   : OLEVariant;
  Sheet       : variant;
  ColCount    : integer;
  i           : integer;
begin
  if RecNo<0 then Exit;

  ExcelApp.ActiveWorkbook.Sheets[SheetNum].Activate;
  CurSheet := SheetNum;
  Sheet := ExcelApp.ActiveSheet;
  UsedRange := Sheet.UsedRange;

  ColCount := UsedRange.Columns.Count;
  R := Sheet.Range[Cell( 1, RecNo+1  )+':'+Cell(ColCount+1,RecNo+1)].Value;
  for i:=1 to ColCount do
  begin
    sl.Add(VarToStr(R[1,i]));
  end;
end;

function TDeStdExcelReport.OpenFile(const FilePath: string): Boolean;
begin
  Result := true;
  ExcelApp := OpenExcel;
  if varIsEmpty(ExcelApp) then
                            begin
                              FSheetCount:=-1;
                              Result := false;
                              Exit;
                            end;
  ExcelApp.Workbooks.Open(FilePath);
  FSheetCount := ExcelApp.Sheets.Count;
  CurSheet := ExcelApp.ActiveSheet.Index;
end;

function TDeStdExcelReport.CloseAllFiles : boolean;
begin
  Result:=False;
  while ExcelApp.Workbooks.count>0 do
    begin
      ExcelApp.ActiveWorkbook.close;
      Result:=True;
    end;
end;

function TDeStdExcelReport.SheetName(const Index: Integer): string;
begin
  if (Index < 1) or (FSheetCount < Index) then
    Result := Unassigned
  else
    Result := ExcelApp.Sheets[Index].Name;
end;

function TDeStdExcelReport.SheetVisible(const Index: Integer): Boolean;
begin
  if (Index < 1) or (FSheetCount < Index) then
    Result := Unassigned
  else
    Result := ExcelApp.Sheets[Index].Visible;
end;

function TDeStdExcelReport.Comment: string;
begin
  try
    Result:= ExcelApp.ActiveWorkbook.BuiltInDocumentProperties[wdPropertyComments].Value;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TDeStdExcelReport.Comment skip error: ' + E.Message);
        {$ENDIF}
        Result:= EmptyStr;
      end;
  end;
end;

procedure TDeStdExcelReport.CloseApp;
begin
  if VarIsEmpty(ExcelApp) then Exit;
  ExcelApp.Quit;
  ExcelApp := Unassigned;
end;

procedure TDeStdExcelReport.GetContent(var R : OLEVariant; var ColCount:integer; var RowCount : integer);
var
  UsedRange   : OLEVariant;
  Sheet       : variant;
begin
  ExcelApp.ActiveWorkbook.Sheets[CurSheet].Activate;
  Sheet := ExcelApp.ActiveSheet;
  UsedRange := Sheet.UsedRange;
  ColCount := UsedRange.Columns.Count;
  RowCount := UsedRange.Rows.Count;
  R := Sheet.Range[Cell( 1, 1  )+':'+Cell(ColCount+1,RowCount+1)].Value;
end;

procedure TDeStdExcelReport.SetCurSheet(const Value: Integer);
begin
  if Value > 0 then FCurSheet := Value;
end;

{ TDeCustomWordReport }

{
constructor TDeCustomWordReport.Create;
begin
  inherited Create;
end;

destructor  TDeCustomWordReport.Destroy;
begin
  inherited Destroy;
end;
}

function TDeCustomWordReport.ProcessReport: OleVariant;
begin
  if CheckDataReady then
    begin
      Result := OpenWord;
      if not varIsEmpty(Result) then
        doProcess(Result);
    end
  else
    Result := unAssigned;
end;

function TDeCustomWordReport.CheckDataReady: Boolean;
begin
  Result := True;
end;

function TDeCustomWordReport.OpenWord: OleVariant;
var
  Doc : OLEVariant;
begin
  Result := CreateOleObject(WordName);
  if varIsEmpty(Result) then exit;
  Result.Application.Visible := False;
  Result.DisplayAlerts := false;
  Result.Options.CheckSpellingAsYouType:=False;
  Result.Options.CheckGrammarAsYouType:=False;

  Doc := Result.Documents.Add;
  Doc.PageSetup.Orientation := wdOrientLandscape;
  SetPageStyle(Doc);

end;

procedure TDeCustomWordReport.doProcess(WordApp: OleVariant);
begin
  {$IFDEF DEBUG}
  DebugLog('TDeCustomWordReport.doProcess ...');
  {$ENDIF}
end;

procedure TDeCustomWordReport.SetPageStyle(varDoc: OleVariant);
var
  PS : OLEVariant;
begin
  if Style = nil then exit;
  PS := varDoc.PageSetup;
  PS.Orientation  := Ord(Style.Orientation);
  PS.LeftMargin   := MillimetersToPoints(Style.Fields.Left/10);
  PS.RightMargin  := MillimetersToPoints(Style.Fields.Right/10);
  PS.TopMargin    := MillimetersToPoints(Style.Fields.Top/10);
  PS.BottomMargin := MillimetersToPoints(Style.Fields.Bottom/10);
end;

procedure TDeCustomWordReport.SetFontStyle(varFont: OleVariant; const FontStyle: string);
var
  aFont : TFont;
begin
  if Style = nil then exit;
  aFont := Style.FontByName(FontStyle);
  if aFont = nil then exit;
  varFont.Name   := aFont.Name;
  varFont.Size   := aFont.Size;
  varFont.Color  := aFont.Color;
  varFont.Bold   := (fsBold in aFont.Style);
  varFont.Italic := (fsItalic in aFont.Style);
  varFont.Underline := (fsUnderline in aFont.Style);
  varFont.StrikeThrough := (fsStrikeOut in aFont.Style);
end;

procedure TDeCustomWordReport.doPreview;
var
  WordApp : OLEVariant;
begin
  WordApp := ProcessReport;
  if not varIsEmpty(WordApp) then try
    WordApp.DisplayAlerts := true;
    WordApp.Application.Visible := True;
  finally
    WordApp := unAssigned;
  end;
end;

procedure TDeCustomWordReport.doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean);
var
  WordApp : OLEVariant;
begin
  WordApp := ProcessReport;
  if not varIsEmpty(WordApp) then try
    WordApp.ActivePrinter := PrinterName;
    WordApp.PrintOut(Background:=False,
                     Copies:=CopyCount,
                     Collate:=SplitCopies);
    WordApp.Quit(False);
  finally
    WordApp := unAssigned;
  end;
end;

procedure TDeCustomWordReport.doPrintToFile(const aFileName: string);
var
  WordApp : OLEVariant;
begin
  WordApp := ProcessReport;
  if not varIsEmpty(WordApp) then try
    WordApp.PrintOut(BackGround := false, OutputFileName := aFileName, PrintToFile := true);
    WordApp.Quit(False);
  finally
    WordApp := unAssigned;
  end;
end;

procedure TDeCustomWordReport.doSaveToFile(const aFileName: string);
var
  WordApp : OLEVariant;
  Doc     : OLEVariant;
begin
  WordApp := ProcessReport;
  if not varIsEmpty(WordApp) then try
    Doc := WordApp.ActiveDocument;
    if not varIsEmpty(Doc) then
      Doc.SaveAs(aFileName);
    WordApp.Quit(False);
  finally
    WordApp := unAssigned;
  end;
end;

{ TDeStdWordReport }

constructor TDeStdWordReport.Create(aDatasetMeta: TDatasetMeta);
begin
  inherited Create;
  FDSMeta := aDatasetMeta;
end;

{
destructor  TDeStdWordReport.Destroy;
begin
  inherited Destroy;
end;
}

function TDeStdWordReport.CheckDataReady: Boolean;
begin
  Result := Assigned(FDSMeta);
  if Result and not(FDSMeta.Cache.Active) then
    FDSMeta.Cache.PrepareData;
end;

procedure TDeStdWordReport.doProcess(WordApp: OleVariant);
var
  Doc                : OLEVariant;
  Table              : OLEVariant;
  Rng                : OLEVariant;
  i,NN,l,w,AWidth, J : Integer;
  newW               : integer;
  FM                 : TFieldMeta;
begin
  Doc := WordApp.ActiveDocument;

  Rng := Doc.Paragraphs.Add.Range;
  setFontStyle(Rng.Font,PFS_Title);
  Rng.Text := GetTitle(FDSMeta.Table.Name);

  Rng := Doc.Paragraphs.Add.Range;
  Rng := Doc.Paragraphs.Add.Range;

  NN     :=0;
  aWidth :=0;
  for i:=0 to FDSMeta.Table.Fields.Count-1 do
    begin
      FM:=FDSMeta.Table.Fields[i];
      if (Not FM.IsLookup) and (FM.VisibleLevel >= fvLevel2 ) then
        begin
          Inc(NN);
          Inc(aWidth, FM.Width);
        end;
    end;

  Rng := Doc.Paragraphs.Add.Range;
  Doc.Tables.Add(Rng, FDSMeta.Cache.Count+1, NN);
  Table:=Doc.Tables.Item(1);

  w:=0;
  for i:=1 to NN do
    w:=w+Table.Columns.Item(i).Width;

  setFontStyle(Table.Range.Font,PFS_Data);
  setFontStyle(Table.Rows.Item(1).Range.Font,PFS_ColumnHeader);

  Table.Rows.Item(1).Range.Shading.Texture := wdTexture20Percent;

  NN:=0;
  for i:=0 to FDSMeta.Table.Fields.Count-1 do
    begin
      if (Not FDSMeta.Table.Fields[i].IsLookup) and
         (FDSMeta.Table.Fields[i].VisibleLevel >= fvLevel2) then
        begin
          Inc(NN);
          newW := (w*FDSMeta.Table.Fields[i].Width) div aWidth;
          if newW < 20 then
            newW := 20;
          Table.Columns.Item(NN).Width := newW;
          Table.Cell(1,NN).Range.Text:=FDSMeta.Table.Fields[i].Native;
        end;
    end;

  l:=0;
  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, FDSMeta.Cache.Count, 1);
  try
  for I := 0 to FDSMeta.Cache.Count-1 do
    begin
      SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, i, 0);
      Inc(l);
      NN:=0;
      for J:=0 to FDSMeta.Table.Fields.Count-1 do
        begin
          if (Not FDSMeta.Table.Fields[J].IsLookup) and (FDSMeta.Table.Fields[J].VisibleLevel >= fvLevel2) then
            begin
              Inc(NN);
              Rng := Table.Cell(L+1,NN).Range;

              if Assigned(FDSMeta.Table.Fields[J].LookupPair) then
                Rng.Text:=FDSMeta.Cache[I].FieldText
                   (FDSMeta.Cache.Fields.IndexByName(FDSMeta.Table.Fields[J].LookupPair.Original))
              else
                Rng.Text:=FDSMeta.Cache[I].FieldText(J);
            end;
        end;
    end;
  finally
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  end;
end;


type
  tPages = class;
  tPage  = class;
  tRow   = class;

  tCell = class
  protected
    FRow      : TRow;
    FElement  : TElementMeta;
    FLeft     : integer;
    FWidth    : integer;
    FHeight   : integer;
    function    getCellIndex:integer;
    function    getFullWidth:integer;
    function    getText:string;
    function    getNext:TCell;
    function    getPrev:TCell;
    function    getElementType:TElementType;
    function    getIsDynamicHeight:boolean;
  public
    constructor Create(aRow : TRow; anElement : TElementMeta);
    destructor  Destroy;override;
    property    Row:TRow read FRow;
    property    CellIndex:integer read getCellIndex;
    property    Element:TElementMeta read FElement;
    property    Left:integer read FLeft{ write FLeft};
    property    Width:integer read FWidth;
    property    Height:integer read FHeight;
    property    Text:string read getText;
    property    Next:TCell read getNext;
    property    Prev:TCell read getPrev;
    property    ElementType:TElementType read getElementType;
    property    IsDynamicHeight:boolean read getIsDynamicHeight;
  end;

  TRow = class
  private
    FPage     : TPage;
    FTop      : integer;
    FCells    : TList;
    function    getRowIndex:integer;
    function    getCount:integer;
    function    getCell(index:integer):TCell;
  public
    constructor Create(aPage : TPage; aTop : integer);
    destructor  Destroy;override;
    function    Add(anElement:TElementMeta):TCell;
    function    AddEmpty(aLeft,aRight:integer):TCell;
    procedure   Delete(index:integer);
    property    Page:TPage read FPage;
    property    RowIndex:integer read getRowIndex;
    property    Count:integer read getCount;
    property    Cells[index:integer]:TCell read getCell;
    property    Top:integer read FTop write FTop;
  end;

  TPage = class
  private
    FPages    : TPages;
    FRows     : TList;
    FName     : string;
    function    getPageIndex:integer;
    function    getCount:integer;
    function    getRow(index:integer):TRow;
  public
    constructor Create(aPages:TPages;aName:string);
    destructor  Destroy;override;
    function    RowByTop(aTop:integer):TRow;
    procedure   Delete(index:integer);
    property    Pages:TPages read FPages;
    property    Name:string read FName write FName;
    property    PageIndex:integer read getPageIndex;
    property    Count:integer read getCount;
    property    Rows[index:integer]:TRow read getRow;
  end;

  TPages = class
  private
    FPages    : TList;
    FWidth    : integer;
    FDMeta    : TDataSetMeta;
    procedure   setWidth(aValue:integer);
    function    getCount:integer;
    function    getPage(index:integer):TPage;
  public
    constructor Create;
    destructor  Destroy;override;
    function    Add(PageName:string):TPage;
    procedure   Delete(index:integer);
    property    Width:integer read FWidth write setWidth;
    property    DataSetMeta:TDatasetMeta read FDMeta write FDMeta;
    property    Count:integer read getCount;
    property    Page[index:integer]:TPage read getPage;
  end;


{tCell}
constructor tCell.Create(aRow : TRow; anElement : TElementMeta);
var
  aTop : integer;
begin
  inherited Create;
  FRow      := aRow;
  FElement  := anElement;
  if FElement <> nil then
  begin
    FElement.GetBounds(getFullWidth,0,FLeft,aTop,FWidth,FHeight);
    if ElementType = etBevel then
      FHeight := 1
    else
      FHeight := FElement.EH;
  end
  else begin
    FHeight := 1;
    FLeft   := 0;
    FWidth  := 0;
  end;
end;

destructor  tCell.Destroy;
begin
  inherited Destroy;
end;

function    tCell.getCellIndex:integer;
begin
  Result := FRow.FCells.IndexOf(Self);
end;

function    tCell.getFullWidth:integer;
begin
  Result := FRow.FPage.FPages.Width;
end;

function    tCell.getText:string;
var
  DM   : TDatasetMeta;
  iFld : integer;
  iLkp : integer;
begin
  DM := FRow.FPage.FPages.DataSetMeta;

  case FElement.ElementType of
    etNone,
    etForm,
    etBevel,
    etCheckBox,
    etBarCode,
    etIconBox,
    etCheckListBox,
    etTreeBox,
    etPanel,
    etGrid         : begin
      Result := EmptyStr;
    end;
    etTabSheet,
    etListTabSheet,
    etLabel        : begin
      Result := getTitle(FElement.Name);
    end;
    etDateTime,
    etTime,
    etLinkedCombo,
    etDefault : begin
      iFld := DM.Cache.Fields.IndexByID(FElement.Field.ID);
      if (iFld >= 0) then
      begin
        iLkp := DM.Cache.Fields.IndexByID(DM.Cache.Fields[iFld].LookupPair.ID);
        if iLkp >= 0 then
          Result := DM.Cache.FocusedItem.FieldText(iLkp)
        else
          Result := DM.Cache.FocusedItem.FieldText(iFld);
      end
      else
        Result := getTitle(FElement.Name);
    end;
  end; //case
end;

function    TCell.getNext:TCell;
begin
  if CellIndex < (Row.Count - 1) then
    Result := Row.Cells[CellIndex + 1]
  else
    Result := nil;
end;

function    TCell.getPrev:TCell;
begin
  if CellIndex > 0 then
    Result := Row.Cells[CellIndex - 1]
  else
    Result := nil;
end;

function    TCell.getElementType:TElementType;
begin
  if FElement = nil then
    Result := etNone
  else
    Result := FElement.ElementType;
end;

function    TCell.getIsDynamicHeight:boolean;
begin
  case ElementType of
    etDefault      : begin
      Result := (Element.Field.DataType in BinaryTypes)
                or((Element.Field.DataType = ftString)and(Element.EH>3))
    end;
    etGrid         : Result := true;
    etListTabSheet : Result := true;
    etBarCode      : Result := true;
    etBrowser      : Result := true;
    etIconBox      : Result := true;
    else
      Result := false;
  end;
end;

{TRow}
constructor TRow.Create(aPage : TPage; aTop : integer);
begin
  inherited Create;
  FPage     := aPage;
  FTop      := aTop;
  FCells    := TList.Create;
end;

destructor  TRow.Destroy;
begin
  while (Count > 0) do
    Delete(Count - 1);
  FCells.Free;
  inherited Destroy;
end;

function    TRow.getRowIndex:integer;
begin
  Result := FPage.FRows.IndexOf(Self);
end;

function    TRow.getCount:integer;
begin
  Result := FCells.Count;
end;

function    TRow.getCell(index:integer):TCell;
begin
  Result := TCell(FCells[index]);
end;

function    TRow.Add(anElement:TElementMeta):TCell;
var
  i : integer;
begin
  Result := TCell.Create(Self,anElement);
  i := 0;
  while (i < Count) and (Cells[i].Left < Result.Left) do
    inc(i);
  FCells.Insert(i,Result);
end;

function    TRow.AddEmpty(aLeft,aRight:integer):TCell;
var
  i : integer;
begin
  Result := TCell.Create(Self,nil);
  Result.FLeft   := aLeft;
  Result.FWidth  := aRight - aLeft;
  Result.FHeight := 1;
  i := 0;
  while (i < Count) and (Cells[i].Left < Result.Left) do
    inc(i);
  FCells.Insert(i,Result);
end;

procedure   TRow.Delete(index:integer);
begin
  Cells[index].Free;
  FCells.Delete(index);
end;

{TPage}
constructor TPage.Create(aPages:TPages;aName:string);
begin
  inherited Create;
  FPages    := aPages;
  FName     := aName;
  FRows     := TList.Create;
end;

destructor  TPage.Destroy;
begin
  while (Count > 0) do
    Delete(Count - 1);
  FRows.Free;
  inherited Destroy;
end;

function    TPage.getPageIndex:integer;
begin
  Result := FPages.FPages.IndexOf(Self);
end;

function    TPage.getCount:integer;
begin
  Result := FRows.Count;
end;

function    TPage.getRow(index:integer):TRow;
begin
  Result := TRow(FRows[index]);
end;

function    TPage.RowByTop(aTop:integer):TRow;
var
  i : integer;
begin
  i := 0;
  while (i < Count) and (aTop > Rows[i].Top) do inc(i);
  if (i < Count) and (aTop = Rows[i].Top) then
    Result := TRow(FRows[i])
  else begin
    Result := TRow.Create(Self,aTop);
    FRows.Insert(i,Result);
  end;
end;

procedure   TPage.Delete(index:integer);
begin
  Rows[index].Free;
  FRows.Delete(index);
end;

{TPages}
constructor TPages.Create;
begin
  inherited Create;
  FPages := TList.Create;
end;

destructor  TPages.Destroy;
begin
  while (Count > 0) do
    Delete(Count - 1);
  FPages.Free;
  inherited Destroy;
end;

procedure   TPages.setWidth(aValue:integer);
begin
  FWidth := aValue;
end;

function    TPages.getCount:integer;
begin
  Result := FPages.Count;
end;

function    TPages.getPage(index:integer):TPage;
begin
  Result := TPage(FPages[index]);
end;

function    TPages.Add(PageName:string):TPage;
begin
  Result := TPage.Create(Self,PageName);
  FPages.Add(Result);
end;

procedure   TPages.Delete(index:integer);
begin
  Page[index].Free;
  FPages.Delete(index);
end;

{TCellHolder}
type
  TCellHolder = class
  private
    FCell : OLEVariant;
  public
    constructor Create(aCell:OLEVariant);
    destructor  Destroy;override;
    property    Cell:OLEVariant read FCell;
  end;

constructor TCellHolder.Create(aCell:OLEVariant);
begin
  inherited Create;
  FCell := aCell;
end;

destructor  TCellHolder.Destroy;
begin
  FCell := unAssigned;
  inherited Destroy;
end;

{TCellHolders}
type
  TCellHolders = class
  private
    FItems : tStringList;
    function    getCell(anElement:TElementMeta):OLEVariant;
  public
    constructor Create;
    destructor  Destroy;override;
    procedure   Add(anElement:TElementMeta;aCell:OLEVariant);
    property    Cell[anElement:TElementMeta]:OLEVariant read getCell;
  end;


constructor TCellHolders.Create;
begin
  inherited Create;
  FItems := tStringList.Create;
end;

destructor  TCellHolders.Destroy;
begin
  while FItems.Count > 0 do
  begin
    TCellHolder(FItems.Objects[FItems.Count-1]).Free;
    FItems.Delete(FItems.Count-1);
  end;
  inherited Destroy;
end;

function    TCellHolders.getCell(anElement:TElementMeta):OLEVariant;
var
  sKey : string;
  iKey : integer;
begin
  sKey := IntToHex(integer(anElement),8);
  iKey := FItems.IndexOf(sKey);
  if  iKey >= 0 then
    Result := TCellHolder(FItems.Objects[iKey]).Cell
  else
    Result := unAssigned;
end;

procedure   TCellHolders.Add(anElement:TElementMeta;aCell:OLEVariant);
var
  sKey : string;
begin
  sKey := IntToHex(integer(anElement),8);
  if FItems.IndexOf(sKey) < 0 then
    FItems.AddObject(sKey,TCellHolder.Create(aCell));
end;

{TDeStdCardReport}
constructor TDeStdCardReport.Create(aDatasetMeta:TDatasetMeta);
begin
  inherited Create;
  FDSMeta := aDatasetMeta;
end;

{
destructor  TDeStdCardReport.Destroy;
begin
  inherited Destroy;
end;
}

// Вычисление абсолютной (с учетом всех владельцев) строки элемента
// в единицах ElementMeta.ET
// Эта процедура сделана локальной, чтобы не вытаскивать в интерфейс
// классы TCell,TRow,TPage
function  AbsoluteRow(anElement:TElementMeta):integer;
var
  aNode : TTreeMeta;
begin
  Result := anElement.ET;
  aNode  := anElement.Owner;
  while (aNode <> nil)
        and(aNode is TElementMeta)
        and not(TElementMeta(aNode).ElementType in [etForm,etTabSheet])
  do begin
    Result := Result + TElementMeta(aNode).ET;
    aNode := aNode.Owner;
  end;
end;

// Построение списка страниц по описанию формы карточки (UnitA)
// Каждая страница содержит строки, строки содержат элементы описания
// полей ElementMeta.ET которых, лежит в данной строке.
// Эта процедура сделана локальной, чтобы не вытаскивать в интерфейс
// классы TCell,TRow,TPage
procedure BuildElement(Pages: TPages; anElement: TElementMeta);
var
  aTop : integer;
  procedure BuildChildElements(Pages:TPages;anElement:TElementMeta);
  var
    i : integer;
  begin
    for i := 0 to anElement.Count - 1 do
      BuildElement(Pages,anElement.Items[i]);
  end;
begin
  case anElement.ElementType of
    etNone         : begin
    end;
    etForm         : begin
      BuildChildElements(Pages,anElement);
    end;
    etPanel        : begin
      BuildChildElements(Pages,anElement);
    end;
    etBevel,
    etLabel,
    etDateTime,
    etCheckBox,
    etTime,
    etLinkedCombo,
    etBarCode,
    etIconBox,
    etCheckListBox,
    etTreeBox,
    etGrid,
    etDefault      : begin
      aTop := AbsoluteRow(anElement);
      if Pages.Count = 0 then
        Pages.Add(EmptyStr);
      Pages.Page[Pages.Count-1].RowByTop(aTop).Add(anElement);
    end;
    etListTabSheet : begin
      Pages.Add(getTitle(anElement.Name,ttSecondName));
      aTop := AbsoluteRow(anElement);
      Pages.Page[Pages.Count-1].RowByTop(aTop).Add(anElement);
    end;
    etTabSheet : begin
      Pages.Add(getTitle(anElement.Name,ttSecondName));
      BuildChildElements(Pages,anElement);
    end;
  end; //case
end;

// Построение списка страниц по описанию формы карточки (UnitA)
// После выполнения процедуры BuildElement производится дополнительная
// обработка списка страниц:
//  1. для элементов перекрывающих несколько строк создаются
//     дополнительные ячейки в перекрываемых строках.
//  2. ячейки с таблицами выносятся в отдельные строки
//     (отказались)
// эта процедура сделана локальной, чтобы не вытаскивать в интерфейс
// классы TCell,TRow,TPage
procedure BuildPageLayout(Pages: TPages; anElement: TElementMeta);
var
  Page                     : TPage;
  Row                      : TRow;
  Cell                     : TCell;
  iPage,iRow,iCell,jRow    : integer;
  yBottom                  : integer;
  xLeft,xRight             : integer;
  newCell                  : TCell;
  NeedEmptyRow             : boolean;
begin
  // построить разбивку элементов по строкам и столбцам
  BuildElement(Pages,anElement);

  // добавляем ячейки для "многострочных" элементов
  for iPage := 0 to Pages.Count - 1 do
  begin
    Page := Pages.Page[iPage];
    for iRow := Page.Count - 1 downto 0 do
    begin
      Row := Page.Rows[iRow];
      for iCell := 0 to Row.Count - 1 do
      begin
        Cell := Row.Cells[iCell];
        if (Cell.Height > 1)
        then begin
          jRow := iRow + 1;
          yBottom := Row.Top + Cell.Height;
          if Cell.IsDynamicHeight then
          begin
            NeedEmptyRow := false;
            while (jRow<Page.Count) and (Page.Rows[jRow].Top < yBottom)
            do begin
              inc(jRow);
              NeedEmptyRow := true;
            end;
            if NeedEmptyRow then
              Page.RowByTop(yBottom-1);
          end;    
          jRow := iRow + 1;
          yBottom := Row.Top + Cell.Height;
          xRight := Pages.Width;
          if Cell.Next <> nil then
            xRight := Cell.Next.Left;
          while (jRow<Page.Count) and (Page.Rows[jRow].Top < yBottom)
          do begin
            newCell := Page.Rows[jRow].Add(Cell.Element);
            if (newCell.Next = nil)
                and(Cell.Next<>nil)
            then begin
              xLeft := Cell.Next.Left;
              if (xLeft+XStep) < Pages.Width then
                Page.Rows[jRow].AddEmpty(xLeft,Pages.Width-xLeft);
            end;
            xLeft := 0;
            if (newCell.Prev <> nil) then
            begin
              xLeft := newCell.Prev.Left + newCell.Prev.Width;
              if (Cell.Prev <> nil)and(xLeft<Cell.Prev.Left) then
                xLeft := Cell.Prev.Left;
            end;
            if (newCell.Left > xLeft+XStep)
            then
              Page.Rows[jRow].AddEmpty(xLeft,newCell.Left);
            if (newCell.Next <> nil)
               and(newCell.Next.Left < xRight)
            then
              xRight := newCell.Next.Left;
            inc(jRow);
          end;
          if (Cell.Next = nil)and(xRight < Pages.Width)
          then
            Row.AddEmpty(xRight,Pages.Width - xRight);
        end;
      end;
    end;
  end;
end;

procedure DrawBorder(Range: OleVariant; aBorder, aLineStyle, aLineWidth, aColor: DWORD);
var
  wBrdr : OLEVariant;
begin
  wBrdr := Range.Borders.Item(aBorder);
  wBrdr.LineStyle := aLineStyle;
  wBrdr.LineWidth := aLineWidth;
  wBrdr.Color     := aColor;
end;

function FindChildDM(aDM: TDatasetMeta; aChildID: Integer): TDatasetMeta;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to aDM.ChildrenCount - 1 do
    if aDM.Children.Items[i].Table.ID = aChildID then
    begin
      Result := aDM.Children.Items[i];
      break;
    end;
end;

procedure TDeStdCardReport.InsertGrid(wCell: OleVariant; DM, OwnerDM: TDatasetMeta);
var
  WordApp            : OLEVariant;
  Doc                : OLEVariant;
  cTable             : OLEVariant;
  cRng               : OLEVariant;
  iRow,iCol          : integer;
  cCell              : OLEVariant;
  i                  : integer;
  Flds               : TFieldsMeta;
  FullWidth,TblWidth : integer;
  aWidth             : integer;
  Column             : OLEVariant;
  function IsGrouppedBy(LF : TFieldMeta):boolean;
  var i:integer;
  begin
    Result := false;
    for i := 0 to DM.OwnerLinks.Count - 1 do
      if DM.OwnerLinks[i].DataSetMeta = OwnerDM then
        if assigned(DM.OwnerLinks[i].LinkField) then
          if DM.OwnerLinks[i].LinkField = LF then Exit(True);
  end;
begin
  Flds := TFieldsMeta.Create(False);
  try
    FullWidth := 0;
    for i := 0 to DM.Table.Fields.Count-1 do
    begin
      if (DM.Table.Fields[i].VisibleLevel >= fvLevel2) and (Not DM.Table.Fields[i].IsLookup)
      //   and not(IsGrouppedBy(DM.Table.Fields[i]))
      //   and not((DM.Table.Fields[i].IsLookup) and(IsGrouppedBy(DM.Table.Fields[i].LookupPair)))
      then begin
        Flds.Add(DM.Table.Fields[i]);
        Inc(FullWidth, DM.Table.Fields[i].Width);
      end;
    end;

    WordApp := wCell.Application;
    Doc     := WordApp.ActiveDocument;

    cRng := wCell.Range;
    cTable := wCell.Tables.Add(cRng,1,Flds.Count);
    cTable.Rows.Alignment := wdAlignRowLeft;
    cTable.LeftPadding := 5;
    cTable.TopPadding := 2;
    cTable.RightPadding := 5;
    cTable.Select;
    DrawBorder(WordApp.Selection, wdBorderBottom, wdLineStyleSingle, wdLineWidth050pt, wdColorBlack);
    DrawBorder(WordApp.Selection, wdBorderTop, wdLineStyleSingle, wdLineWidth050pt, wdColorBlack);
    DrawBorder(WordApp.Selection, wdBorderLeft, wdLineStyleSingle, wdLineWidth050pt, wdColorBlack);
    DrawBorder(WordApp.Selection, wdBorderRight, wdLineStyleSingle, wdLineWidth050pt, wdColorBlack);
    {}
    TblWidth := 0;
    for i := 1 to cTable.Columns.Count do
      TblWidth := TblWidth + cTable.Columns.Item(i).Width;
    {}
    for i := 1 to cTable.Columns.Count - 1 do
    begin
      Column := cTable.Columns.Item(i);
      aWidth := Flds[i-1].Width;
      if aWidth < 20 then aWidth := 20;
      Column.setWidth((TblWidth * aWidth) div FullWidth, wdAdjustProportional);
    end;
    {}
    for iRow := 0 to DM.Cache.Count - 1 do
    begin
      cTable.Rows.Add;
      for iCol := 0 to Flds.Count - 1 do
        begin
          cCell := cTable.Cell(iRow + 2,iCol + 1);
          if Assigned(Flds[iCol].LookupPair) then
            cCell.Range.Text := DM.Cache[iRow].FieldText(DM.Table.Fields.IndexByID(Flds[iCol].LookupPair.ID))
          else
            cCell.Range.Text := DM.Cache[iRow].FieldText(DM.Table.Fields.IndexByID(Flds[iCol].ID));
        end;
    end;
    {}
    cTable.Rows.Item(1).Select;
    DrawBorder(WordApp.Selection,
               wdBorderBottom,wdLineStyleSingle,wdLineWidth050pt,wdColorBlack);
    WordApp.Selection.Shading.Texture := wdTexture30Percent;
    for i := 1 to cTable.Columns.Count  do
    begin
      cTable.Columns.Item(i).Select;
      DrawBorder(WordApp.Selection,
                 wdBorderRight,wdLineStyleSingle,wdLineWidth050pt,wdColorBlack);
      cRng := cTable.Cell(1,i).Range;
      cRng.Text := Flds[i-1].Native;
      setFontStyle(cRng.Font,PFS_ColumnHeader);
    end;
  finally
    Flds.Free;
  end;
end;

function TDeStdCardReport.CheckDataReady: Boolean;
begin
  Result := Assigned(FDSMeta);
  if Result and not(FDSMeta.Cache.Active) then
    FDSMeta.Cache.PrepareData;
  Result := Result and Assigned(FDSMeta.Cache.FocusedItem);
end;

procedure TDeStdCardReport.doProcess(WordApp: OleVariant);
var
  Doc                : OLEVariant;
  wTable             : OLEVariant;
  wRng               : OLEVariant;
  wCell              : OLEVariant;
  wBrdr              : OLEVariant;
var
  Pages              : TPages;
  iPage              : integer;
  Page               : TPage;
  iRow               : integer;
  Row                : TRow;
  iCell              : integer;
  Cell,nCell         : TCell;
  ChildDM            : TDatasetMeta;
  iFld,iLkp          : integer;
  UsedCells          : TCellHolders;
  mergeCell          : OLEVariant;
  sFile              : string;
  bBottomBrdr        : boolean;
begin

  Pages     := TPages.Create;
  try
    Pages.DatasetMeta := FDSMeta;

    UsedCells := TCellHolders.Create;
    try

      Doc := WordApp.ActiveDocument;

      Pages.Width := Doc.PageSetup.PageWidth
                     - Doc.PageSetup.LeftMargin
                     - Doc.PageSetup.RightMargin;
      // подготовить разметку страницы
      BuildPageLayout(Pages,FDSMeta.Table.FirstForm);

      // вставить строку заголовка
      wRng := Doc.Paragraphs.Add.Range;
      wRng.Text := FDSMeta.Cache.FocusedItem.Caption;
      setFontStyle(wRng.Font,PFS_Title);
      wRng.Collapse(0);
      wRng.Select;
      wBrdr := WordApp.Selection.Borders.Item(wdBorderBottom);
      wRng  := Doc.Paragraphs.Add.Range;
      wRng.Collapse(0);
      wBrdr.LineStyle := wdLineStyleSingle;
      wBrdr.LineWidth := wdLineWidth150pt;
      wBrdr.Color := 0{wdColorBlack};

      //вставить закладки карточной формы
      for iPage := 0 to Pages.Count - 1 do
      begin
        // вставить заголовок закладки
        Page := Pages.Page[iPage];
        wRng := Doc.Paragraphs.Add.Range;
        wRng.Collapse(0);
        wRng := Doc.Paragraphs.Add.Range;
        wRng.Text := Page.Name;
        setFontStyle(wRng.Font,PFS_Section);

        //вставить таблицу из одной колонки и требуемого
        //количества строк в которую будем размещать поля
        wRng := Doc.Paragraphs.Add.Range;
        wRng.Collapse(0);
        wTable := Doc.Tables.Add(wRng,Page.Count,1);
        wTable.Rows.Alignment := wdAlignRowLeft;
        wTable.LeftPadding := 5;
        wTable.RightPadding := 5;
        wTable.TopPadding := 5;

        //для каждой строки закладки
        for iRow := 0 to Page.Count -1 do
        begin
          Row := Page.Rows[iRow];
          // разбить колонку на требуемое число ячеек
          wCell := wTable.Cell(iRow+1,1);
          if Row.Count > 1 then
            wCell.Split(NumColumns:=Row.Count);
          // для каждой ячейки задать форматирование и вставить данные
          for iCell := 0 to Row.Count-1 do
          begin
            Cell := Row.Cells[iCell];
            wCell := wTable.Cell(iRow+1,iCell+1);
            // установить ширину ячейки
            if iCell<(Row.Count-1) then
              wCell.setWidth(Row.Cells[iCell+1].Left - Cell.Left,
                             wdAdjustProportional);

            mergeCell := UsedCells.Cell[Cell.Element];
            if (Cell.Element <> nil) then
            begin
              if varIsEmpty(mergeCell)or varIsNull(mergeCell) then
              begin
                UsedCells.Add(Cell.Element,wCell);
                // вставить данные
                setFontStyle(wCell.Range.Font,PFS_Data);
                case Cell.ElementType of
                  etListTabSheet,
                  etGrid         : begin
                    ChildDM := FindChildDM(FDSMeta,Cell.Element.Link);
                    if ChildDM <> nil then
                      InsertGrid(wCell,ChildDM,FDSMeta);
                  end;
                  etLabel        : begin
                    wCell.Range.Text := getTitle(Cell.Element.Name);
                    setFontStyle(wCell.Range.Font,PFS_Label);
                    if Cell.CellIndex > 0 then
                    begin
                      nCell := Cell.Next;
                      if (nCell <> nil)
                         and (nCell.ElementType in [etDateTime,etTime,etLinkedCombo,etDefault])
                      then
                        wCell.Range.Paragraphs.Item(1).Alignment := wdAlignParagraphRight;
                    end;
                  end;
                  etDateTime,
                  etTime,
                  etLinkedCombo,
                  etDefault      : begin
                    bBottomBrdr := true;
                    with FDSMeta.Cache do
                    begin
                      iFld := Fields.IndexByID(Cell.Element.Field.ID);
                      if (iFld >= 0) then
                      begin
                        if (Cell.ElementType = etDefault)
                           and(Cell.Element.Field.DataType in BinaryTypes)
                        then begin
                          bBottomBrdr := false;
                          with TDeImage.Create(nil) do try
                            Value := FocusedItem.FieldValue[iFld];
                            if Assigned(Graphic) and not Graphic.Empty
                            then begin
                              sFile := DeGetTempFileName(EmptyStr,'DEI', sExtensionBMP);
                              SaveToBitmap(sFile);
                              Doc.InlineShapes.AddPicture(sFile,False,true,wCell.Range);
                              DeleteFile(sFile);
                            end;
                          finally
                            Free;
                          end;
                        end
                        else begin
                          // 29.10.2015 Добавил проверку на наличие LookupPair, т.к. при его отсутствии падало а AV!
                          if Assigned(Fields[iFld].LookupPair) then
                            iLkp := Fields.IndexByID(Fields[iFld].LookupPair.ID)
                          else
                            iLkp := -1;
                          if iLkp >= 0 then
                            wCell.Range.Text := FocusedItem.FieldText(iLkp)
                          else begin
                            wCell.Range.Text := FocusedItem.FieldText(iFld);
                            bBottomBrdr := Cell.Element.EH <= 3;
                          end;
                        end;
                      end
                      else
                        wCell.Range.Text := getTitle(Cell.Element.Name);
                    end;

                    if bBottomBrdr then
                    begin
                      wCell.Range.Select;
                      DrawBorder(WordApp.Selection,
                                 wdBorderBottom,wdLineStyleSingle,wdLineWidth050pt,
                                 wdColorBlack);
                    end;
                  end;
                end; //case
              end
              else begin
                try
                  mergeCell.Merge(wCell);
                except
                  {$IFDEF DEBUG}
                  on E: Exception do
                    DebugLog('TDeStdCardReport.doProcess skip error: ' + E.Message);
                  {$ENDIF}
                end;
              end;
            end;
            (**)
          end;
        end;
      end;
    finally
      UsedCells.Free;
    end;

  finally
    Pages.Free;
  end;
end;


{ TDeStdGraphicReport }

function TDeStdGraphicReport.CheckDataReady: Boolean;
begin
  Result := (0<Length(FFileName)) and FileExists(FFileName);
end;

procedure TDeStdGraphicReport.doProcess(WordApp: OleVariant);
var
  Doc    : OLEVariant;
  Rng    : OLEVariant;
  Brdr   : OLEVariant;
begin
  Doc := WordApp.ActiveDocument;

  Rng := Doc.Paragraphs.Add.Range;
  setFontStyle(Rng.Font,PFS_Title);
  Rng.Text := Title;
  Rng.Collapse(0);
  Rng.Select;
  Brdr := WordApp.Selection.Borders.Item(wdBorderBottom);
  Rng  := Doc.Paragraphs.Add.Range;
  Rng.Collapse(0);
  Brdr.LineStyle := wdLineStyleSingle;
  Brdr.LineWidth := wdLineWidth150pt;
  Brdr.Color := 0{wdColorBlack};

  Rng := Doc.Paragraphs.Add.Range;
  Rng := Doc.Paragraphs.Add.Range;
  Doc.InlineShapes.AddPicture(FFileName, False, true, Rng);
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DeStdReport unit initialization ...');

finalization
  DebugLog('DeStdReport unit finalization ...');
{$ENDIF}

end.

