unit DeEMFStorage;

interface

uses Windows, {Classes, }Graphics,
     deSchema2D;

const
  GDICOMMENT_PROFIT    : dword = $41434544; //'DECA'
  GDICOMMENT_PROFITMAP : dword = $2050414D; //'MAP '

type
  TdeEMFStorage = class(TdeSchemaStorage)
  protected
    FMetaFile   : TMetaFile;
    FScaleMode  : TdeAFScaleMode;
  public
    constructor Create(aMetaFile:TMetaFile);
    //destructor  Destroy;override;
    procedure   Load(aSchema:TdeSchema);override;
    procedure   Save(aSchema:TdeSchema);override;
    property    ScaleMode:TdeAFScaleMode read FScaleMode write FScaleMode;
  end;

implementation

uses Types, SysUtils,
     DeLog, G2D, deStdPalette;

const
  Eps = 1e-20;

type
  TSmallPointArray = array[0..0] of TSmallPoint;
  pSmallPointArray = ^TSmallPointArray;

type
  TLongPointArray = array[0..0] of TPoint;
  pLongPointArray = ^TLongPointArray;

type
   TEMFReader = class
   protected
     FMetaFile   : TMetaFile;
     FSchema     : TdeSchema;
     //
     FEMFGroup   : TdeSchemaGroup;
     FInObject   : boolean;
     FPnts       : array of TPoint2D;
     FCapacity   : integer;
     FSize       : integer;
     FPrevSize   : integer;
     FIsSamePnts : boolean;
     FCmplxCurve : boolean;
     FLastObj    : TdeSchemaObject;
     FIsSameObj  : boolean;
     FConv       : TMatrix2D;
     //
     FPen        : TLogPen;
     FBrush      : TLogBrush32;
     //
     procedure   BuildObject(Closed:boolean);
     procedure   StartNewObject(P:TPoint);
     procedure   AddObjectPoint(P:TPoint;PT:integer);
     procedure   setTransparent(anItem:TdeSchemaObject;OnOff:boolean);
     procedure   setBrdrColor(anItem:TdeSchemaObject;aColor:TColor);
     procedure   setBrdrWidth(anItem:TdeSchemaObject;aWidth:integer);
     procedure   setBrdrStyle(anItem:TdeSchemaObject;aStyle:integer);
     procedure   setFillColor(anItem:TdeSchemaObject;aColor:TColor);
     procedure   BuildSchema;
     //
     function    doHeader(lpHeader : pENHMETAHEADER):integer;
     function    doEof:integer;
     function    doMoveToEx(P:TPoint):integer;
     function    doLineTo(P:TPoint):integer;
     function    doPolyXXXXTo(cPts:dword;aPts:pLongPointArray;PT:integer):integer;
     function    doPolyXXXX(cPts:dword;aPts:pLongPointArray;PT:integer;Closed:boolean):integer;
     function    doPolyXXXXTo16(cPts:dword;aPts:pSmallPointArray;PT:integer):integer;
     function    doPolyXXXX16(cPts:dword;aPts:pSmallPointArray;PT:integer;Closed:boolean):integer;
     function    doRectangle(aRect:TRect):integer;

     function    doCreatePen(lpEMRCreatePen:pEMRCreatePen):integer;
     function    doCreateBrush(lpEMRCreateBrush:pEMRCreateBrushIndirect):integer;

     function    doBitBlt(lpEMRBitBlt:pEMRBitBlt):integer;
     function    doStretchBlt(lpEMRStretchBlt:pEMRStretchBlt):integer;
     function    doStretchDIBits(lpEMRStretchDIBits:pEMRStretchDIBits):integer;

     function    doBeginPath:integer;
     function    doEndPath:integer;
     function    doCloseFigure:integer;
     function    doFillPath:integer;
     function    doStrokeAndFillPath:integer;
     function    doStrokePath:integer;
     //
     function    doGDIComment(lpEMR:PEMRGDIComment):integer;
   public
     constructor Create(  aMetaFile:TMetaFile;
                            aSchema:TdeSchema);
     destructor  Destroy;override;
   end;


{TdeEMFStorage}

constructor TdeEMFStorage.Create(aMetaFile:TMetaFile);
begin
  inherited Create;
  FMetaFile   := aMetaFile;
  FScaleMode  := smAutoSize;
end;

{
destructor  TdeEMFStorage.Destroy;
begin
  inherited Destroy;
end;
}

procedure   TdeEMFStorage.Load(aSchema:TdeSchema);
var
  Rdr  : TEMFReader;
begin
  Rdr  := TEMFReader.Create(FMetaFile,aSchema);
  try
    Rdr.BuildSchema;
  finally
    Rdr.Free;
  end;
end;

procedure   TdeEMFStorage.Save(aSchema:TdeSchema);
var
  EMFCanvas : TMetaFileCanvas;
  emfSize   : TSize;
  scmSize   : TPoint2D;
  scmCenter : TPoint2D;
  Kx,Ky     : double;
  aDC       : hDC;
  C         : TMatrix2D;
  Bnds      : TRect2D;
begin
  Bnds := aSchema.Bounds;
  if IsEmptyRect2D(Bnds) then exit;

  emfSize.cx := FMetaFile.MMWidth;
  emfSize.cy := FMetaFile.MMHeight;
  if (emfSize.cx = 0) then begin
    emfSize.cx := 14000;
    FMetaFile.MMWidth := emfSize.cx;
  end;
  if (emfSize.cy = 0) then begin
    emfSize.cy := 20000;
    FMetaFile.MMHeight := emfSize.cy;
  end;  

  scmSize   := RectSizes(Bnds);
  scmCenter := RectCenter(Bnds);
  //коэффициенты пересчета точек схемы в миллиметры EMF
  Kx := emfSize.cx/scmSize.X;
  Ky := emfSize.cy/scmSize.Y;
  if (ScaleMode >= smProportional) then begin
    if Kx>Ky then Kx := Ky else Ky := Kx;
    if (ScaleMode = smAutoSize) then begin
      FMetaFile.MMWidth  := round(scmSize.X*Kx);
      FMetaFile.MMHeight := round(scmSize.Y*Ky);
    end;
  end;
  aDC := getDC(0);
  try
    FMetaFile.Width  := MulDiv(FMetaFile.MMWidth,
                               GetDeviceCaps(aDC,HORZRES),
                               GetDeviceCaps(aDC,HORZSIZE)*100);
    Kx := Kx*GetDeviceCaps(aDC,HORZRES)/GetDeviceCaps(aDC,HORZSIZE)/100;
    FMetaFile.Height := MulDiv(FMetaFile.MMHeight,
                               GetDeviceCaps(aDC,VERTRES),
                               GetDeviceCaps(aDC,VERTSIZE)*100);
    Ky := Ky*GetDeviceCaps(aDC,VERTRES)/GetDeviceCaps(aDC,VERTSIZE)/100;
  finally
    releaseDC(0,aDC);
  end;
  EMFCanvas := TMetaFileCanvas.Create(FMetaFile,0);
  try
    //сохранить "картинку"
    M2D_Identity(C);
    C := M2D_Shift(C,-scmCenter.X,-scmCenter.Y);
    C := M2D_Scale(C,Kx,-Ky);
    C := M2D_Shift(C,FMetaFile.Width/2,FMetaFile.Height/2);

    aSchema.PaintTo(EMFCanvas,C);

  finally
    EMFCanvas.Free;
  end;
end;

type
  TEMFRecordProc  = function(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;

const
  MAXPROCINDEX = 255;
type
  TProcTable = array[0..MAXPROCINDEX] of TEMFRecordProc;
var
  ProcTable : TProcTable;

function      EnhMetaFileProc(aDC : hDC;            // handle to device context
                         lpHTable : PHandleTable;   // pointer to metafile handle table
                         lpEMFR   : PEnhMetaRecord; // pointer to metafile record
                         nObj     : integer;        // count of objects
                         lpData   : pointer         // pointer to optional data
                                 ): integer;stdcall;
begin
  if (lpEMFR.iType<=MAXPROCINDEX)
     and Assigned(ProcTable[lpEMFR.iType])
  then
    Result := ProcTable[lpEMFR.iType](TEMFReader(lpData),pEMR(lpEMFR))
  else
    Result :=-1;
end;

constructor TEMFReader.Create(  aMetaFile:TMetaFile;
                                  aSchema:TdeSchema);
begin
  inherited Create;
  FMetaFile   := aMetaFile;
  FSchema     := aSchema;
  //
  FEMFGroup := nil;
  FInObject := false;
  FPnts     := nil;
  FCapacity := 0;
  FSize     := 0;
  FPrevSize := 0;
  FIsSamePnts := false;
  M2D_Identity(FConv);
  FConv     := M2D_Scale(FConv,1,-1);
  //
end;

destructor  TEMFReader.Destroy;
begin
  FPnts     := nil;
  inherited Destroy;
end;

procedure   TEMFReader.BuildObject(Closed:boolean);
var
  anItem    : TdeCustomCurve;
begin
  if not(FInObject) then exit;
  FInObject := false;
  if (FSize<2) then exit;

  FIsSameObj := (FPrevSize = FSize) and FIsSamePnts;
  if FIsSameObj then exit;

  if FCmplxCurve then
    anItem := TdeCustomCurve(FSchema.New(TdeComplexCurve,FEMFGroup))
  else
    anItem := TdeCustomCurve(FSchema.New(TdeSimpleCurve,FEMFGroup));
  anItem.setDrawPath(FPnts,FSize);
  FLastObj := anItem;
end;

procedure   TEMFReader.StartNewObject(P:TPoint);
var
  NP : TPoint2D;
begin
  if (FCapacity=0) then begin
    FCapacity := 16;
    setLength(FPnts,FCapacity);
  end;
  FInObject   := true;
  FCmplxCurve := false;
  FPrevSize := FSize;
  FSize := 0;
  NP   := MultVM2D(PtoP2D(P),FConv);
  NP.W := PT_MOVETO;
  FIsSamePnts := (FPrevSize > 0) and PointsEqual2D(NP,FPnts[FSize]);
  FPnts[FSize]   := NP;
  inc(FSize);
end;

procedure   TEMFReader.AddObjectPoint(P:TPoint;PT:integer);
var
  NP : TPoint2D;
begin
  if not(FInObject) then exit;
  if (FSize=FCapacity) then begin
    FCapacity := (FCapacity * 5) div 4;
    setLength(FPnts,FCapacity);
  end;
  NP   := MultVM2D(PtoP2D(P),FConv);
  NP.W := PT;
  FIsSamePnts := FIsSamePnts
                 and (FPrevSize >= FSize)
                 and PointsEqual2D(NP,FPnts[FSize]);
  FPnts[FSize]   := NP;
  if (PT and not PT_CLOSEFIGURE) = PT_MOVETO then
    FCmplxCurve := true;
  inc(FSize);
end;

procedure   TEMFReader.setTransparent(anItem:TdeSchemaObject;OnOff:boolean);
var
  i : integer;
begin
  if (anItem=nil) then exit;
  if (anItem.IGroup <> nil) then begin
    for i:=0 to anItem.IGroup.Count-1 do
      setTransparent(anItem.IGroup.Item[i],OnOff);
  end
  else if (anItem is TdeCustomCurve) then
    TdeCustomCurve(anItem).Transparent := OnOff;
end;

procedure   TEMFReader.setBrdrColor(anItem:TdeSchemaObject;aColor:TColor);
var
  i : integer;
begin
  if (anItem=nil) then exit;
  if (anItem.IGroup <> nil) then begin
    for i:=0 to anItem.IGroup.Count-1 do
      setBrdrColor(anItem.IGroup.Item[i],aColor);
  end
  else if (anItem is TdeCustomCurve) then
    TdeCustomCurve(anItem).BorderColor := aColor;
end;

procedure   TEMFReader.setBrdrWidth(anItem:TdeSchemaObject;aWidth:integer);
var
  i : integer;
begin
  if (anItem=nil) then exit;
  if (anItem.IGroup <> nil) then begin
    for i:=0 to anItem.IGroup.Count-1 do
      setBrdrWidth(anItem.IGroup.Item[i],aWidth);
  end
  else if (anItem is TdeCustomCurve) then
    TdeCustomCurve(anItem).BorderWidth := aWidth;
end;

procedure   TEMFReader.setBrdrStyle(anItem:TdeSchemaObject;aStyle:integer);
var
  i : integer;
begin
  if (anItem=nil) then exit;
  if (anItem.IGroup <> nil) then begin
    for i:=0 to anItem.IGroup.Count-1 do
      setBrdrStyle(anItem.IGroup.Item[i],aStyle);
  end
  else if (anItem is TdeCustomCurve) then
    TdeCustomCurve(anItem).BorderStyle := TPenStyle(aStyle);
end;

procedure   TEMFReader.setFillColor(anItem:TdeSchemaObject;aColor:TColor);
var
  i : integer;
begin
  if (anItem=nil) then exit;
  if (anItem.IGroup <> nil) then begin
    for i:=0 to anItem.IGroup.Count-1 do
      setFillColor(anItem.IGroup.Item[i],aColor);
  end
  else if (anItem is TdeCustomCurve) then
    TdeCustomCurve(anItem).Color := aColor;
end;

procedure   TEMFReader.BuildSchema;
begin
  FSchema.Lock;
  try
    FEMFGroup := FSchema.NewGroup(nil);
    FEMFGroup.Lock;
    try
      EnumEnhMetaFile(0,FMetaFile.Handle,@EnhMetaFileProc,Self,Rect(0,0,MaxInt-1,MaxInt-1));
    finally
      FEMFGroup.unLock;
    end;
    if (FEMFGroup.Count = 1) then
      FEMFGroup.Item[0].Parent := nil;
    if (FEMFGroup.Count = 0) then
      FEMFGroup.Delete;
  finally
    FSchema.unLock;
  end;
end;

function    TEMFReader.doHeader(lpHeader : pENHMETAHEADER):integer;
begin
  Result := 1;
end;

function    TEMFReader.doEof:integer;
begin
  BuildObject(false);
  Result := 1;
end;

function    TEMFReader.doMoveToEx(P:TPoint):integer;
begin
  Result := 1;
  if FInObject then
    AddObjectPoint(P,PT_MOVETO)
  else
    StartNewObject(P);
end;

function    TEMFReader.doLineTo(P:TPoint):integer;
begin
  Result := 0;
  if FInObject then begin
    AddObjectPoint(P,PT_LINETO);
    Result := 1;
  end;
end;

function    TEMFReader.doPolyXXXXTo(cPts:dword;aPts:pLongPointArray;PT:integer):integer;
var
  i : integer;
begin
  Result := 0;
  if FInObject then begin
    for i := 0 to cPts-1 do
      AddObjectPoint(aPts[i],PT);
    Result := 1;
  end;
end;

function    TEMFReader.doPolyXXXX(cPts:dword;aPts:pLongPointArray;PT:integer;Closed:boolean):integer;
var
  i : integer;
begin
  doMoveToEx(aPts[0]);
  for i := 1 to cPts-1 do
    AddObjectPoint(aPts[i],PT);
  if Closed then
    doCloseFigure;
  Result := 1;
end;

function    TEMFReader.doPolyXXXXTo16(cPts:dword;aPts:pSmallPointArray;PT:integer):integer;
var
  i : integer;
begin
  Result := 0;
  if FInObject then begin
    for i := 0 to cPts-1 do
      AddObjectPoint(Point(aPts[i].X,aPts[i].Y),PT);
    Result := 1;
  end;
end;

function    TEMFReader.doPolyXXXX16(cPts:dword;aPts:pSmallPointArray;
                                    PT:integer;Closed:boolean):integer;
var
  i : integer;
begin
  doMoveToEx(Point(aPts[0].X,aPts[0].Y));
  for i := 1 to cPts-1 do
    AddObjectPoint(Point(aPts[i].X,aPts[i].Y),PT);
  if Closed then
    doCloseFigure;
  Result := 1;
end;

function    TEMFReader.doRectangle(aRect:TRect):integer;
begin
  doMoveToEx(aRect.TopLeft);
  AddObjectPoint(Point(aRect.Right,aRect.Top),PT_LINETO);
  AddObjectPoint(Point(aRect.Right,aRect.Bottom),PT_LINETO);
  AddObjectPoint(Point(aRect.Left,aRect.Bottom),PT_LINETO);
  doCloseFigure;
  Result := 1;
end;

function    TEMFReader.doCreatePen(lpEMRCreatePen:pEMRCreatePen):integer;
begin
  Result := 1;
  with lpEMRCreatePen^ do
    FPen := lopn;
end;

function    TEMFReader.doCreateBrush(lpEMRCreateBrush:pEMRCreateBrushIndirect):integer;
begin
  Result := 1;
  with lpEMRCreateBrush^ do
    FBrush := lb;
end;

function    TEMFReader.doBitBlt(lpEMRBitBlt:pEMRBitBlt):integer;
var
  aBMP        : TBitmap;
  HandleTable : THandleTable;
  P           : TPoint2D;
  anItem      : TdePictureObject;
begin
  Result    := 1;
  aBMP := TBitmap.Create;
  try
    aBMP.Width  := lpEMRBitBlt.cxDest;
    aBMP.Height := lpEMRBitBlt.cyDest;
    HandleTable.objectHandle[0] := FMetafile.Handle;
    PlayEnhMetafileRecord(aBMP.Handle,HandleTable,PEnhMetaRecord(lpEMRBitBlt)^,1);
    P   := MultVM2D(PtoP2D(Point(lpEMRBitBlt.xDest,lpEMRBitBlt.yDest)),FConv);
    anItem := TdePictureObject(FSchema.New(TdePictureObject,FEMFGroup));
    anItem.TopLeft := P;
    anItem.Picture.Graphic := aBMP;
    FLastObj := anItem;
  finally
    aBMP.Free;
  end;
end;

function    TEMFReader.doStretchBlt(lpEMRStretchBlt:pEMRStretchBlt):integer;
var
  aBMP        : TBitmap;
  HandleTable : THandleTable;
  P           : TPoint2D;
  anItem      : TdePictureObject;
begin
  Result    := 1;
  aBMP := TBitmap.Create;
  try
    aBMP.Width  := lpEMRStretchBlt.cxDest;
    aBMP.Height := lpEMRStretchBlt.cyDest;
    HandleTable.objectHandle[0] := FMetafile.Handle;
    PlayEnhMetafileRecord(aBMP.Handle,HandleTable,PEnhMetaRecord(lpEMRStretchBlt)^,1);
    P   := MultVM2D(PtoP2D(Point(lpEMRStretchBlt.xDest,lpEMRStretchBlt.yDest)),FConv);
    anItem := TdePictureObject(FSchema.New(TdePictureObject,FEMFGroup));
    anItem.TopLeft := P;
    anItem.Picture.Graphic := aBMP;
    FLastObj := anItem;
  finally
    aBMP.Free;
  end;
end;

(*
function    TEMFReader.doStretchDIBits(lpEMRStretchDIBits:pEMRStretchDIBits):integer;
var
  aBMP        : TBitmap;
  HandleTable : THandleTable;
  iDestP      : TPoint;
  P           : TPoint2D;
  anItem      : TdePictureObject;
begin
  Result    := 1;
  aBMP := TBitmap.Create;
  try
    aBMP.Transparent     := true;
    aBMP.TransparentMode := tmAuto;
    aBMP.Width  := lpEMRStretchDIBits.cxDest;
    aBMP.Height := lpEMRStretchDIBits.cyDest;
    iDestP      := Point(lpEMRStretchDIBits.xDest,lpEMRStretchDIBits.yDest);
    lpEMRStretchDIBits.xDest := 0;
    lpEMRStretchDIBits.yDest := 0;
    HandleTable.objectHandle[0] := FMetafile.Handle;
    if PlayEnhMetafileRecord(aBMP.Canvas.Handle,HandleTable,PEnhMetaRecord(lpEMRStretchDIBits)^,1)
    then begin
      P   := MultVM2D(PtoP2D(iDestP),FConv);
      anItem := TdePictureObject(FSchema.New(TdePictureObject,FEMFGroup));
      anItem.TopLeft := P;
      P   := MultVM2D(PtoP2D(Point(iDestP.X+lpEMRStretchDIBits.cxDest,
                                   iDestP.Y+lpEMRStretchDIBits.cyDest)),FConv);
      anItem.Width   := (P.X - anItem.TopLeft.X);
      anItem.Height  :=-(P.Y - anItem.TopLeft.Y);
      anItem.Picture.Graphic := aBMP;
      FLastObj := anItem;
    end;
  finally
    aBMP.Free;
  end;
end;
*)
function    TEMFReader.doStretchDIBits(lpEMRStretchDIBits:pEMRStretchDIBits):integer;
var
  aBMP        : TBitmap;
  iDestP      : TPoint;
  P           : TPoint2D;
  anItem      : TdePictureObject;
begin
  Result    := 1;
  aBMP := TBitmap.Create;
  try
    {aBMP.Width  := lpEMRStretchDIBits.cxSrc;
    aBMP.Height := lpEMRStretchDIBits.cySrc;}
    iDestP      := Point(lpEMRStretchDIBits.xDest,lpEMRStretchDIBits.yDest);
    if setDIBits(aBMP.Canvas.Handle,
                aBMP.Handle,
                0,lpEMRStretchDIBits.cySrc,
                pointer(dword(lpEMRStretchDIBits)+lpEMRStretchDIBits.offBitsSrc),
                pBitmapInfo(dword(lpEMRStretchDIBits)+lpEMRStretchDIBits.offBmiSrc)^,
                lpEMRStretchDIBits.iUsageSrc) > 0
    then begin
      P   := MultVM2D(PtoP2D(iDestP),FConv);
      anItem := TdePictureObject(FSchema.New(TdePictureObject,FEMFGroup));
      anItem.TopLeft := P;
      P   := MultVM2D(PtoP2D(Point(iDestP.X+lpEMRStretchDIBits.cxDest,
                                   iDestP.Y+lpEMRStretchDIBits.cyDest)),
                      FConv);
      anItem.Width   := (P.X - anItem.TopLeft.X);
      anItem.Height  :=-(P.Y - anItem.TopLeft.Y);
      anItem.Picture.Graphic := aBMP;
      FLastObj := anItem;
    end;
  finally
    aBMP.Free;
  end;
end;
(**)

function    TEMFReader.doBeginPath:integer;
begin
  Result    := 1;
end;

function    TEMFReader.doEndPath:integer;
begin
  Result := 1;
  BuildObject(false);
end;

function    TEMFReader.doCloseFigure:integer;
begin
  Result := 1;
  if FSize > 0 then
    FPnts[FSize-1].W := trunc(FPnts[FSize-1].W) or PT_CLOSEFIGURE;
end;

function    TEMFReader.doFillPath:integer;
begin
  Result := 1;
  setTransparent(FLastObj,false);
  setFillColor(FLastObj,FBrush.lbColor);
  if FIsSameObj then exit;
  setBrdrColor(FLastObj,FBrush.lbColor);
  setBrdrStyle(FLastObj,Ord(psSolid));
  setBrdrWidth(FLastObj,1);
end;

function    TEMFReader.doStrokePath:integer;
begin
  Result := 1;
  setBrdrColor(FLastObj,FPen.lopnColor);
  setBrdrStyle(FLastObj,FPen.lopnStyle);
  setBrdrWidth(FLastObj,FPen.lopnWidth.X);
  if FIsSameObj then exit;
  setTransparent(FLastObj,true);
end;


function    TEMFReader.doStrokeAndFillPath:integer;
begin
  Result := 1;
  setTransparent(FLastObj,false);
  setBrdrColor(FLastObj,FPen.lopnColor);
  setBrdrStyle(FLastObj,FPen.lopnStyle);
  setBrdrWidth(FLastObj,FPen.lopnWidth.X);
  setFillColor(FLastObj,FBrush.lbColor);
end;

function    TEMFReader.doGDIComment(lpEMR:PEMRGDIComment):integer;
begin
  Result :=-1;
end;


function  EMR_HEADER_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doHeader(pENHMETAHEADER(lpEMR));
end;

function  EMR_POLYBEZIER_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doPolyXXXX(PEMRPolyline(lpEMR).cPtl,
                             @(PEMRPolyline(lpEMR).aPtl[0]),
                              PT_BEZIERTO,
                              false);
end;

function  EMR_POLYGON_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doPolyXXXX(PEMRPolyline(lpEMR).cPtl,
                             @(PEMRPolyline(lpEMR).aPtl[0]),
                              PT_LINETO,
                              true);
end;

function  EMR_POLYLINE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doPolyXXXX(PEMRPolyline(lpEMR).cPtl,
                             @(PEMRPolyline(lpEMR).aPtl[0]),
                              PT_LINETO,
                              false);
end;

function  EMR_POLYBEZIERTO_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doPolyXXXXTo(PEMRPolyline(lpEMR).cPtl,
                               @(PEMRPolyline(lpEMR).aPtl[0]),
                                PT_BEZIERTO);
end;

function  EMR_POLYLINETO_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doPolyXXXXTo(PEMRPolyline(lpEMR).cPtl,
                               @(PEMRPolyline(lpEMR).aPtl[0]),
                                PT_LINETO);
end;

function  EMR_POLYPOLYLINE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_POLYPOLYGON_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETWINDOWEXTEX_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETWINDOWORGEX_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETVIEWPORTEXTEX_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETVIEWPORTORGEX_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETBRUSHORGEX_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_EOF_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doEof;
end;

function  EMR_SETPIXELV_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETMAPPERFLAGS_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETMAPMODE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETBKMODE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETPOLYFILLMODE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETROP2_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETSTRETCHBLTMODE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETTEXTALIGN_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETCOLORADJUSTMENT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETTEXTCOLOR_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETBKCOLOR_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_OFFSETCLIPRGN_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_MOVETOEX_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doMoveToEx(pEMRLineTo(lpEMR).ptl);
end;

function  EMR_SETMETARGN_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_EXCLUDECLIPRECT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_INTERSECTCLIPRECT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SCALEVIEWPORTEXTEX_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SCALEWINDOWEXTEX_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SAVEDC_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_RESTOREDC_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETWORLDTRANSFORM_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_MODIFYWORLDTRANSFORM_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SELECTOBJECT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_CREATEPEN_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doCreatePen(pEMRCreatePen(lpEMR));
end;

function  EMR_CREATEBRUSHINDIRECT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doCreateBrush(pEMRCreateBrushIndirect(lpEMR));
end;

function  EMR_DELETEOBJECT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_ANGLEARC_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_ELLIPSE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_RECTANGLE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doRectangle(PEMRRectangle(lpEMR).rclBox);
end;

function  EMR_ROUNDRECT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_ARC_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_CHORD_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_PIE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SELECTPALETTE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_CREATEPALETTE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETPALETTEENTRIES_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_RESIZEPALETTE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_REALIZEPALETTE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_EXTFLOODFILL_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_LINETO_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doLineTo(pEMRLineTo(lpEMR).ptl);
end;

function  EMR_ARCTO_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_POLYDRAW_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETARCDIRECTION_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETMITERLIMIT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_BEGINPATH_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doBeginPath;
end;

function  EMR_ENDPATH_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doEndPath;
end;

function  EMR_CLOSEFIGURE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doCloseFigure;
end;

function  EMR_FILLPATH_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doFillPath;
end;

function  EMR_STROKEANDFILLPATH_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doStrokeAndFillPath;
end;

function  EMR_STROKEPATH_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doStrokePath;
end;

function  EMR_FLATTENPATH_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_WIDENPATH_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SELECTCLIPPATH_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_ABORTPATH_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_GDICOMMENT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doGDIComment(PEMRGDIComment(lpEMR));
end;

function  EMR_FILLRGN_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_FRAMERGN_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_INVERTRGN_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_PAINTRGN_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_EXTSELECTCLIPRGN_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_BITBLT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doBitBlt(pEMRBitBlt(lpEMR));
end;

function  EMR_STRETCHBLT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doStretchBlt(pEMRStretchBlt(lpEMR));
end;

function  EMR_MASKBLT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_PLGBLT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETDIBITSTODEVICE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_STRETCHDIBITS_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doStretchDIBits(pEMRStretchDIBits(lpEMR));
end;

function  EMR_EXTCREATEFONTINDIRECTW_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_EXTTEXTOUTA_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_EXTTEXTOUTW_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_POLYBEZIER16_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doPolyXXXX16(PEMRPolyBezier16(lpEMR).cPts,
                                @(PEMRPolyBezier16(lpEMR).aPts[0]),
                                PT_BEZIERTO,
                                false);
end;

function  EMR_POLYGON16_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doPolyXXXX16(PEMRPolygon16(lpEMR).cPts,
                              @(PEMRPolygon16(lpEMR).aPts[0]),
                                PT_LINETO,
                                true);
end;

function  EMR_POLYLINE16_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doPolyXXXX16(PEMRPolyLine16(lpEMR).cPts,
                              @(PEMRPolyLine16(lpEMR).aPts[0]),
                                PT_LINETO,
                                false);
end;

function  EMR_POLYBEZIERTO16_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doPolyXXXXTo16(PEMRPolyline16(lpEMR).cPts,
                                  @(PEMRPolyline16(lpEMR).aPts[0]),
                                  PT_BEZIERTO);
end;

function  EMR_POLYLINETO16_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result := Sender.doPolyXXXXTo16(PEMRPolyline16(lpEMR).cPts,
                                  @(PEMRPolyline16(lpEMR).aPts[0]),
                                  PT_LINETO);
end;

function  EMR_POLYPOLYLINE16_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_POLYPOLYGON16_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_POLYDRAW16_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_CREATEMONOBRUSH_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_CREATEDIBPATTERNBRUSHPT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_EXTCREATEPEN_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_POLYTEXTOUTA_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_POLYTEXTOUTW_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETICMMODE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_CREATECOLORSPACE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETCOLORSPACE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_DELETECOLORSPACE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_GLSRECORD_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_GLSBOUNDEDRECORD_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_PIXELFORMAT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_DRAWESCAPE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_EXTESCAPE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_STARTDOC_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SMALLTEXTOUT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_FORCEUFIMAPPING_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_NAMEDESCAPE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_COLORCORRECTPALETTE_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETICMPROFILEA_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETICMPROFILEW_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_ALPHABLEND_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_ALPHADIBBLEND_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_TRANSPARENTBLT_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_TRANSPARENTDIB_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_GRADIENTFILL_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETLINKEDUFIS_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;

function  EMR_SETTEXTJUSTIFICATION_Proc(Sender:TEMFReader;lpEMR:pEMR):integer;stdcall;
begin
  Result :=-1;
end;


procedure setHandler(iType:integer;aHandler:pointer);
begin
  @ProcTable[iType] := aHandler;
end;

procedure InitProcTable;
var
  i : integer;
begin
  for i := 0 to MAXPROCINDEX do
    setHandler(i,nil);

  setHandler(EMR_HEADER,@EMR_HEADER_Proc);
  setHandler(EMR_POLYBEZIER,@EMR_POLYBEZIER_Proc);
  setHandler(EMR_POLYGON,@EMR_POLYGON_Proc);
  setHandler(EMR_POLYLINE,@EMR_POLYLINE_Proc);
  setHandler(EMR_POLYBEZIERTO,@EMR_POLYBEZIERTO_Proc);
  setHandler(EMR_POLYLINETO,@EMR_POLYLINETO_Proc);
  setHandler(EMR_POLYPOLYLINE,@EMR_POLYPOLYLINE_Proc);
  setHandler(EMR_POLYPOLYGON,@EMR_POLYPOLYGON_Proc);
  setHandler(EMR_SETWINDOWEXTEX,@EMR_SETWINDOWEXTEX_Proc);
  setHandler(EMR_SETWINDOWORGEX,@EMR_SETWINDOWORGEX_Proc);
  setHandler(EMR_SETVIEWPORTEXTEX,@EMR_SETVIEWPORTEXTEX_Proc);
  setHandler(EMR_SETVIEWPORTORGEX,@EMR_SETVIEWPORTORGEX_Proc);
  setHandler(EMR_SETBRUSHORGEX,@EMR_SETBRUSHORGEX_Proc);
  setHandler(EMR_EOF,@EMR_EOF_Proc);
  setHandler(EMR_SETPIXELV,@EMR_SETPIXELV_Proc);
  setHandler(EMR_SETMAPPERFLAGS,@EMR_SETMAPPERFLAGS_Proc);
  setHandler(EMR_SETMAPMODE,@EMR_SETMAPMODE_Proc);
  setHandler(EMR_SETBKMODE,@EMR_SETBKMODE_Proc);
  setHandler(EMR_SETPOLYFILLMODE,@EMR_SETPOLYFILLMODE_Proc);
  setHandler(EMR_SETROP2,@EMR_SETROP2_Proc);
  setHandler(EMR_SETSTRETCHBLTMODE,@EMR_SETSTRETCHBLTMODE_Proc);
  setHandler(EMR_SETTEXTALIGN,@EMR_SETTEXTALIGN_Proc);
  setHandler(EMR_SETCOLORADJUSTMENT,@EMR_SETCOLORADJUSTMENT_Proc);
  setHandler(EMR_SETTEXTCOLOR,@EMR_SETTEXTCOLOR_Proc);
  setHandler(EMR_SETBKCOLOR,@EMR_SETBKCOLOR_Proc);
  setHandler(EMR_OFFSETCLIPRGN,@EMR_OFFSETCLIPRGN_Proc);
  setHandler(EMR_MOVETOEX,@EMR_MOVETOEX_Proc);
  setHandler(EMR_SETMETARGN,@EMR_SETMETARGN_Proc);
  setHandler(EMR_EXCLUDECLIPRECT,@EMR_EXCLUDECLIPRECT_Proc);
  setHandler(EMR_INTERSECTCLIPRECT,@EMR_INTERSECTCLIPRECT_Proc);
  setHandler(EMR_SCALEVIEWPORTEXTEX,@EMR_SCALEVIEWPORTEXTEX_Proc);
  setHandler(EMR_SCALEWINDOWEXTEX,@EMR_SCALEWINDOWEXTEX_Proc);
  setHandler(EMR_SAVEDC,@EMR_SAVEDC_Proc);
  setHandler(EMR_RESTOREDC,@EMR_RESTOREDC_Proc);
  setHandler(EMR_SETWORLDTRANSFORM,@EMR_SETWORLDTRANSFORM_Proc);
  setHandler(EMR_MODIFYWORLDTRANSFORM,@EMR_MODIFYWORLDTRANSFORM_Proc);
  setHandler(EMR_SELECTOBJECT,@EMR_SELECTOBJECT_Proc);
  setHandler(EMR_CREATEPEN,@EMR_CREATEPEN_Proc);
  setHandler(EMR_CREATEBRUSHINDIRECT,@EMR_CREATEBRUSHINDIRECT_Proc);
  setHandler(EMR_DELETEOBJECT,@EMR_DELETEOBJECT_Proc);
  setHandler(EMR_ANGLEARC,@EMR_ANGLEARC_Proc);
  setHandler(EMR_ELLIPSE,@EMR_ELLIPSE_Proc);
  setHandler(EMR_RECTANGLE,@EMR_RECTANGLE_Proc);
  setHandler(EMR_ROUNDRECT,@EMR_ROUNDRECT_Proc);
  setHandler(EMR_ARC,@EMR_ARC_Proc);
  setHandler(EMR_CHORD,@EMR_CHORD_Proc);
  setHandler(EMR_PIE,@EMR_PIE_Proc);
  setHandler(EMR_SELECTPALETTE,@EMR_SELECTPALETTE_Proc);
  setHandler(EMR_CREATEPALETTE,@EMR_CREATEPALETTE_Proc);
  setHandler(EMR_SETPALETTEENTRIES,@EMR_SETPALETTEENTRIES_Proc);
  setHandler(EMR_RESIZEPALETTE,@EMR_RESIZEPALETTE_Proc);
  setHandler(EMR_REALIZEPALETTE,@EMR_REALIZEPALETTE_Proc);
  setHandler(EMR_EXTFLOODFILL,@EMR_EXTFLOODFILL_Proc);
  setHandler(EMR_LINETO,@EMR_LINETO_Proc);
  setHandler(EMR_ARCTO,@EMR_ARCTO_Proc);
  setHandler(EMR_POLYDRAW,@EMR_POLYDRAW_Proc);
  setHandler(EMR_SETARCDIRECTION,@EMR_SETARCDIRECTION_Proc);
  setHandler(EMR_SETMITERLIMIT,@EMR_SETMITERLIMIT_Proc);
  setHandler(EMR_BEGINPATH,@EMR_BEGINPATH_Proc);
  setHandler(EMR_ENDPATH,@EMR_ENDPATH_Proc);
  setHandler(EMR_CLOSEFIGURE,@EMR_CLOSEFIGURE_Proc);
  setHandler(EMR_FILLPATH,@EMR_FILLPATH_Proc);
  setHandler(EMR_STROKEANDFILLPATH,@EMR_STROKEANDFILLPATH_Proc);
  setHandler(EMR_STROKEPATH,@EMR_STROKEPATH_Proc);
  setHandler(EMR_FLATTENPATH,@EMR_FLATTENPATH_Proc);
  setHandler(EMR_WIDENPATH,@EMR_WIDENPATH_Proc);
  setHandler(EMR_SELECTCLIPPATH,@EMR_SELECTCLIPPATH_Proc);
  setHandler(EMR_ABORTPATH,@EMR_ABORTPATH_Proc);
  setHandler(EMR_GDICOMMENT,@EMR_GDICOMMENT_Proc);
  setHandler(EMR_FILLRGN,@EMR_FILLRGN_Proc);
  setHandler(EMR_FRAMERGN,@EMR_FRAMERGN_Proc);
  setHandler(EMR_INVERTRGN,@EMR_INVERTRGN_Proc);
  setHandler(EMR_PAINTRGN,@EMR_PAINTRGN_Proc);
  setHandler(EMR_EXTSELECTCLIPRGN,@EMR_EXTSELECTCLIPRGN_Proc);
  setHandler(EMR_BITBLT,@EMR_BITBLT_Proc);
  setHandler(EMR_STRETCHBLT,@EMR_STRETCHBLT_Proc);
  setHandler(EMR_MASKBLT,@EMR_MASKBLT_Proc);
  setHandler(EMR_PLGBLT,@EMR_PLGBLT_Proc);
  setHandler(EMR_SETDIBITSTODEVICE,@EMR_SETDIBITSTODEVICE_Proc);
  setHandler(EMR_STRETCHDIBITS,@EMR_STRETCHDIBITS_Proc);
  setHandler(EMR_EXTCREATEFONTINDIRECTW,@EMR_EXTCREATEFONTINDIRECTW_Proc);
  setHandler(EMR_EXTTEXTOUTA,@EMR_EXTTEXTOUTA_Proc);
  setHandler(EMR_EXTTEXTOUTW,@EMR_EXTTEXTOUTW_Proc);
  setHandler(EMR_POLYBEZIER16,@EMR_POLYBEZIER16_Proc);
  setHandler(EMR_POLYGON16,@EMR_POLYGON16_Proc);
  setHandler(EMR_POLYLINE16,@EMR_POLYLINE16_Proc);
  setHandler(EMR_POLYBEZIERTO16,@EMR_POLYBEZIERTO16_Proc);
  setHandler(EMR_POLYLINETO16,@EMR_POLYLINETO16_Proc);
  setHandler(EMR_POLYPOLYLINE16,@EMR_POLYPOLYLINE16_Proc);
  setHandler(EMR_POLYPOLYGON16,@EMR_POLYPOLYGON16_Proc);
  setHandler(EMR_POLYDRAW16,@EMR_POLYDRAW16_Proc);
  setHandler(EMR_CREATEMONOBRUSH,@EMR_CREATEMONOBRUSH_Proc);
  setHandler(EMR_CREATEDIBPATTERNBRUSHPT,@EMR_CREATEDIBPATTERNBRUSHPT_Proc);
  setHandler(EMR_EXTCREATEPEN,@EMR_EXTCREATEPEN_Proc);
  setHandler(EMR_POLYTEXTOUTA,@EMR_POLYTEXTOUTA_Proc);
  setHandler(EMR_POLYTEXTOUTW,@EMR_POLYTEXTOUTW_Proc);
  setHandler(EMR_SETICMMODE,@EMR_SETICMMODE_Proc);
  setHandler(EMR_CREATECOLORSPACE,@EMR_CREATECOLORSPACE_Proc);
  setHandler(EMR_SETCOLORSPACE,@EMR_SETCOLORSPACE_Proc);
  setHandler(EMR_DELETECOLORSPACE,@EMR_DELETECOLORSPACE_Proc);
  setHandler(EMR_GLSRECORD,@EMR_GLSRECORD_Proc);
  setHandler(EMR_GLSBOUNDEDRECORD,@EMR_GLSBOUNDEDRECORD_Proc);
  setHandler(EMR_PIXELFORMAT,@EMR_PIXELFORMAT_Proc);
  setHandler(EMR_DRAWESCAPE,@EMR_DRAWESCAPE_Proc);
  setHandler(EMR_EXTESCAPE,@EMR_EXTESCAPE_Proc);
  setHandler(EMR_STARTDOC,@EMR_STARTDOC_Proc);
  setHandler(EMR_SMALLTEXTOUT,@EMR_SMALLTEXTOUT_Proc);
  setHandler(EMR_FORCEUFIMAPPING,@EMR_FORCEUFIMAPPING_Proc);
  setHandler(EMR_NAMEDESCAPE,@EMR_NAMEDESCAPE_Proc);
  setHandler(EMR_COLORCORRECTPALETTE,@EMR_COLORCORRECTPALETTE_Proc);
  setHandler(EMR_SETICMPROFILEA,@EMR_SETICMPROFILEA_Proc);
  setHandler(EMR_SETICMPROFILEW,@EMR_SETICMPROFILEW_Proc);
  setHandler(EMR_ALPHABLEND,@EMR_ALPHABLEND_Proc);
  setHandler(EMR_ALPHADIBBLEND,@EMR_ALPHADIBBLEND_Proc);
  setHandler(EMR_TRANSPARENTBLT,@EMR_TRANSPARENTBLT_Proc);
  setHandler(EMR_TRANSPARENTDIB,@EMR_TRANSPARENTDIB_Proc);
  setHandler(EMR_GRADIENTFILL,@EMR_GRADIENTFILL_Proc);
  setHandler(EMR_SETLINKEDUFIS,@EMR_SETLINKEDUFIS_Proc);
  setHandler(EMR_SETTEXTJUSTIFICATION,@EMR_SETTEXTJUSTIFICATION_Proc);
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('DeEMFStorage unit initialization ...');
  {$ENDIF}
  InitProcTable;
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DeEMFStorage unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

