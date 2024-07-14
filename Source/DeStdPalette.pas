unit DeStdPalette;

interface

uses
  Windows, Graphics, {Controls, }
  G2D, deSchema2D;

type
  TdeTextObject = class(TdeSchemaObject)
  protected
    FFontName   : string;
    FFontStyle  : TFontStyles;
    FFontCharset:TFontCharSet;
    FText       : string;
    FColor      : TColor;
    function     RecalcBounds:TRect2D;override;
    procedure    setFontName(aValue:string);
    procedure    setFontStyle(aValue:TFontStyles);
    procedure    setFontCharset(aValue:TFontCharSet);
    procedure    setText(aValue:string);virtual;
    procedure    setColor(aValue:TColor);virtual;
  public
    class function ClassID:integer;override;
    constructor  Create(anOwner:TdeSchema);override;
    destructor   Destroy;override;
    function     getData(pData:pointer;var iSize:integer):boolean;override;
    procedure    setData(pData:pointer;iSize:integer);override;
    function     CreateMemento:TdeSchemaObjectMemento;override;
    procedure    PaintTo( Canvas:TCanvas;StoC:TMatrix2D;WidthLine:Integer=1);override;
    procedure    PaintFrame(Canvas:TCanvas;StoC:TMatrix2D);override;
    function     Contain(   Canvas:TCanvas;StoC:TMatrix2D;X,Y:integer):boolean;override;
    property     FontName:string read FFontName write setFontName;
    property     FontStyle:TFontStyles read FFontStyle write setFontStyle;
    property     FontCharset:TFontCharSet read FFontCharSet write setFontCharSet;
    property     Text:string read FText write setText;
    property     Color:tColor read FColor write setColor;
  end;


type
  TdePictureObject = class(TdeSchemaObject)
  protected
    FPicture   : TPicture;
    function     RecalcBounds:TRect2D;override;
  public
    class function ClassID:integer;override;
    constructor  Create(anOwner:TdeSchema);override;
    destructor   Destroy;override;
    function     getData(pData:pointer;var iSize:integer):boolean;override;
    procedure    setData(pData:pointer;iSize:integer);override;
    function     CreateMemento:TdeSchemaObjectMemento;override;
    procedure    PaintTo( Canvas:TCanvas;StoC:TMatrix2D;WidthLine:Integer=1);override;
    procedure    PaintFrame(Canvas:TCanvas;StoC:TMatrix2D);override;
    function     Contain(   Canvas:TCanvas;StoC:TMatrix2D;X,Y:integer):boolean;override;
    property     Picture:TPicture read FPicture;
  end;

implementation

uses SysUtils, Classes,
     DeLog;

const
  cTextSize = 20;

type
  TdeTextObjectMemento = class(TdeSchemaObjectMemento)
  protected
    FontName    : string;
    FontStyle   : TFontStyles;
    FontCharset : TFontCharset;
    Color       : TColor;
    Text        : string;
    constructor Create(anOrigin:TdeTextObject);
  public
    procedure   Restore(anObject:TdeSchemaObject);override;
    destructor  Destroy;override;
  end;

constructor TdeTextObjectMemento.Create(anOrigin:TdeTextObject);
begin
  inherited Create(anOrigin);
  FontName    := anOrigin.FontName;
  FontStyle   := anOrigin.FontStyle;
  FontCharset := anOrigin.FontCharset;
  Color       := anOrigin.Color;
  Text        := anOrigin.Text;
end;

procedure   TdeTextObjectMemento.Restore(anObject:TdeSchemaObject);
begin
  anObject.Lock;
  inherited Restore(anObject);
  with TdeTextObject(anObject) do begin
    FontName    := Self.FontName;
    FontStyle   := Self.FontStyle;
    FontCharset := Self.FontCharset;
    Color       := Self.Color;
    Text        := Self.Text;
  end;
  anObject.unLock;
end;

destructor  TdeTextObjectMemento.Destroy;
begin
  inherited Destroy;
end;

const
  MAX_FONTNAME_SIZE = 50;

type
  TdeTextObjectData = packed record
    Header         : TdeObjectDataHeader;
    cFontName      : array[0..MAX_FONTNAME_SIZE-1] of char;
    reserved       : dword;
    iFontStyle     : TFontStyles;
    iFontCharset   : TFontCharset;
    cColor         : TColor;
    cbLength       : dword;
    cText          : array[0..0] of char;
  end;
  PdeTextObjectData  = ^TdeTextObjectData;


  
{TdeTextObject}
class function TdeTextObject.ClassID:integer;
begin
  Result := 5;
end;

constructor  TdeTextObject.Create(anOwner:TdeSchema);
begin
  inherited Create(anOwner);
  FFontName    := 'Times New Roman';
  FFontStyle   := [];
  FFontCharset := DEFAULT_CHARSET;
  FText        := EmptyStr;
  FColor       := clBlack;
  updateBounds;
end;

destructor   TdeTextObject.Destroy;
begin
  inherited Destroy;
end;

function     TdeTextObject.RecalcBounds:TRect2D;
begin
  Result := Rect2D(0.0,1.0,1.0,0.0);
end;

procedure    TdeTextObject.setFontName(aValue:string);
begin
  aValue := ANSIUpperCase(aValue);
  if FFontName = aValue then exit;
  Changing;
  FFontName := aValue;
  Changed([ocpParams]);
end;

procedure    TdeTextObject.setFontStyle(aValue:TFontStyles);
begin
  if FFontStyle = aValue then exit;
  Changing;
  FFontStyle := aValue;
  Changed([ocpParams]);
end;

procedure    TdeTextObject.setFontCharset(aValue:TFontCharSet);
begin
  if FFontCharset = aValue then exit;
  Changing;
  FFontCharset := aValue;
  Changed([ocpParams]);
end;

procedure    TdeTextObject.setText(aValue:string);
begin
  if FText = aValue then exit;
  Changing;
  FText := aValue;
  Changed([ocpParams]);
end;

procedure    TdeTextObject.setColor(aValue:TColor);
begin
  if FColor = aValue then exit;
  Changing;
  FColor := aValue;
  Changed([ocpParams]);
end;

function     TdeTextObject.getData(pData:pointer;var iSize:integer):boolean;
var
  nSize,inhSize : integer;
  inhData       : dword;
  pInhData      : pointer;
begin
  inhSize := 0;
  inherited getData(nil,inhSize);
  inhData := sizeOf(TdeTextObjectData)+Length(Text)+1;
  nSize := integer(inhData)+inhSize;
  Result := (nSize<=iSize);
  if Result then begin
    with PdeTextObjectData(pData)^ do begin
      Header.nSize := nSize;
      Header.pInheritedData := inhData;
      Header.pInheritedSize := inhSize;
      pInhData := pointer(dword(pData)+inhData);
      inherited getData(pInhData,inhSize);
      StrLCopy(@(cFontName[0]),pChar(FontName),MAX_FONTNAME_SIZE);
      reserved     := 0;
      iFontStyle   := FontStyle;
      iFontCharset := FontCharset;
      cColor       := Color;
      cbLength     := Length(Text);
      cText[0]     := #0;
      StrLCopy(@(cText[0]),pChar(FText),cbLength);
    end;
  end;
  iSize := nSize;
end;

procedure    TdeTextObject.setData(pData:pointer;iSize:integer);
var
  inhSize   : integer;
  inhData   : dword;
  pInhData  : pointer;
begin
  if iSize < sizeOf(TdeTextObjectData) then exit;
  Lock;
  Changing;
  try
    with PdeTextObjectData(pData)^ do begin
      inhSize := Header.pInheritedSize;
      if (inhSize > 0) then begin
        inhData := Header.pInheritedData;
        pInhData := pointer(dword(pData)+inhData);
        inherited setData(pInhData,inhSize);
      end;
      FontName      := cFontName;
      FontStyle     := iFontStyle;
      FontCharset   := iFontCharset;
      Color         := cColor;
      if cbLength > 0 then
        Text := pChar(@(cText[0]))
      else
        Text := EmptyStr;
    end;
  finally
    Changed([ocpParams]);
    unLock;
  end;
end;

function     TdeTextObject.CreateMemento:TdeSchemaObjectMemento;
begin
  Result := TdeTextObjectMemento.Create(Self);
end;

procedure    TdeTextObject.PaintTo( Canvas:TCanvas;StoC:TMatrix2D;WidthLine:Integer=1);
var
  P1,P2,P3       : TPoint2D;
  iTL            : TPoint;
  Kx,Ky          : double;
  oldXFORM,XFORM : tagXFORM;
  OtoC,Cnv       : TMatrix2D;
  V1,V2          : TPoint2D;
  Alpha,W,H      : double;
  oldGM          : integer;
begin
  if FText = EmptyStr then exit;

  Canvas.Font.Name    := FontName;
  Canvas.Font.Size    := cTextSize;
  Canvas.Font.Color   := Color;
  Canvas.Font.Style   := FontStyle;
  Canvas.Font.Charset := FontCharset;
  Canvas.Brush.Style  := bsClear;

  OtoC := MultMM2D(FOtoSConv,StoC);
  P1 := MultVM2D(rectTopLeft(Bounds),OtoC);
  P2 := MultVM2D(rectTopRight(Bounds),OtoC);
  P3 := MultVM2D(rectBottomLeft(Bounds),OtoC);
  iTL := P2DtoP(P1);
  Alpha := Rotation;
  V1 := Vector2D(OToS(rectCenter(Bounds)),OToS(rectRight(Bounds)));
  V2 := Vector2D(OToS(rectCenter(Bounds)),OToS(rectTop(Bounds)));
  if VectorVV2D(V1,V2)>=0 then Kx := 1 else Kx :=-1;
  Ky := 1;
  W := Distance2D(P1,P2);
  H := Distance2D(P1,P3);

  if W<>0 then
    Kx := Kx*W/Canvas.TextWidth(FText);
  if H<>0 then
    Ky := Ky*H/Canvas.TextHeight(FText);

  M2D_Identity(Cnv);
  Cnv := M2D_Shift(Cnv,-iTL.X,-iTL.Y);
  Cnv := M2D_Scale(Cnv,Kx,Ky);
  Cnv := M2D_Rotate(Cnv,-Alpha);
  Cnv := M2D_Shift(Cnv,iTL.X,iTL.Y);

  XFORM.eM11 := Cnv.M11;
  XFORM.eM12 := Cnv.M12;
  XFORM.eM21 := Cnv.M21;
  XFORM.eM22 := Cnv.M22;
  XFORM.eDx  := Cnv.M31;
  XFORM.eDy  := Cnv.M32;

  oldGM := SetGraphicsMode(Canvas.Handle,GM_ADVANCED);
  getWorldTransform(Canvas.Handle,oldXFORM);
  modifyWorldTransform(Canvas.Handle,XFORM,MWT_LEFTMULTIPLY);

  Canvas.TextOut(iTL.X,iTL.Y,Text);

  setWorldTransform(Canvas.Handle,oldXFORM);
  SetGraphicsMode(Canvas.Handle,oldGM);
end;

procedure    TdeTextObject.PaintFrame(Canvas:TCanvas;
                                        StoC:TMatrix2D);
begin
  inherited PaintFrame(Canvas,StoC);
end;

function     TdeTextObject.Contain(Canvas:TCanvas; StoC:TMatrix2D; X,Y:integer):boolean;
var
  aRgn : hRGN;
begin
  Canvas.Pen.Width := 1;
  BeginPath(Canvas.Handle);
  PaintTo(Canvas,StoC);
  EndPath(Canvas.Handle);
  aRgn := PathToRegion(Canvas.Handle);
  Result := PtInRegion(aRgn,X,Y);
  DeleteObject(aRgn);
end;






type
  TdePictureObjectMemento = class(TdeSchemaObjectMemento)
  protected
    Picture : TPicture;
    constructor Create(anOrigin:TdePictureObject);
  public
    procedure   Restore(anObject:TdeSchemaObject);override;
    destructor  Destroy;override;
  end;

constructor TdePictureObjectMemento.Create(anOrigin:TdePictureObject);
begin
  inherited Create(anOrigin);
  Picture := TPicture.Create;
  Picture.Assign(anOrigin.Picture);
end;

procedure   TdePictureObjectMemento.Restore(anObject:TdeSchemaObject);
begin
  anObject.Lock;
  try
    inherited Restore(anObject);
    with TdePictureObject(anObject) do
      Picture.Assign(Self.Picture);
  finally    
    anObject.unLock;
  end;
end;

destructor  TdePictureObjectMemento.Destroy;
begin
  Picture.Free;
  inherited Destroy;
end;


type
  TdePictureObjectData = packed record
    Header         : TdeObjectDataHeader;
    GraphicType    : integer;
    cbData         : dword;
    cData          : array[0..0] of char;
  end;
  PdePictureObjectData  = ^TdePictureObjectData;


{TdePictureObject}
class function TdePictureObject.ClassID:integer;
begin
  Result := 6;
end;

constructor  TdePictureObject.Create(anOwner:TdeSchema);
begin
  inherited Create(anOwner);
  FPicture  := TPicture.Create;
  updateBounds;
  update;
end;

destructor   TdePictureObject.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

function     TdePictureObject.RecalcBounds:TRect2D;
begin
  Result := Rect2D(0.0,1.0,1.0,0.0);
end;

function     TdePictureObject.getData(pData:pointer;var iSize:integer):boolean;
var
  nSize,inhSize : integer;
  inhData       : dword;
  pInhData      : pointer;
  aStrm         : TdeMemoryStream;
  cbDataSize    : dword;
  iDataType     : integer;
begin
  inhSize := 0;
  inherited getData(nil,inhSize);
  aStrm         := TdeMemoryStream.Create;
  try
    if Picture.Graphic is TIcon then begin
      Picture.Icon.SaveToStream(aStrm);
      cbDataSize := aStrm.Size;
      iDataType  := 2;
    end
    else if Picture.Graphic is TMetafile then begin
      Picture.Metafile.SaveToStream(aStrm);
      cbDataSize := aStrm.Size;
      iDataType  := 1;
    end
    else if Picture.Graphic is TBitmap then begin
      Picture.Bitmap.SaveToStream(aStrm);
      cbDataSize := aStrm.Size;
      iDataType  := 3;
    end
    else begin
      cbDataSize := 0;
      iDataType  := 0;
    end;
    inhData := sizeOf(TdePictureObjectData)+cbDataSize+1;
    nSize := integer(inhData)+inhSize;
    Result := (nSize<=iSize);
    if Result then begin
      with PdePictureObjectData(pData)^ do begin
        Header.nSize := nSize;
        Header.pInheritedData := inhData;
        Header.pInheritedSize := inhSize;
        pInhData := pointer(dword(pData)+inhData);
        inherited getData(pInhData,inhSize);
        cbData      := cbDataSize;
        GraphicType := iDataType;
        aStrm.Seek(0,soFromBeginning);
        aStrm.Read(cData[0],cbDataSize);
      end;
    end;
  finally
    aStrm.Free;
  end;
  iSize := nSize;
end;

procedure    TdePictureObject.setData(pData:pointer;iSize:integer);
var
  inhSize    : integer;
  inhData    : dword;
  pInhData   : pointer;
  aStrm      : TdeMemoryStream;
  aGraphic   : TGraphic;
begin
  if iSize < sizeOf(TdePictureObjectData) then exit;
  Lock;
  Changing;
  try
    with PdePictureObjectData(pData)^ do begin
      inhSize := Header.pInheritedSize;
      if (inhSize > 0) then begin
        inhData := Header.pInheritedData;
        pInhData := pointer(dword(pData)+inhData);
        inherited setData(pInhData,inhSize);
      end;
      if GraphicType = 1 then
        aGraphic := TMetafile.Create
      else if GraphicType = 2 then
        aGraphic := TIcon.Create
      else if GraphicType = 3 then
        aGraphic := TBitmap.Create
      else
        aGraphic := nil;
      if aGraphic <> nil then begin
        aStrm := TdeMemoryStream.Create;
        try
          aStrm.Write(cData,cbData);
          aStrm.Seek(0,soFromBeginning);
          aGraphic.LoadFromStream(aStrm);
        finally
          aStrm.Free;
        end;
      end;
      Picture.Graphic := aGraphic;
    end;
  finally
    Changed([ocpParams]);
    unLock;
  end;
end;

function     TdePictureObject.CreateMemento:TdeSchemaObjectMemento;
begin
  Result := TdePictureObjectMemento.Create(Self);
end;

procedure    TdePictureObject.PaintTo( Canvas:TCanvas;StoC:TMatrix2D;WidthLine:Integer=1);
var
  P1,P2,P3,P4    : TPoint2D;
  iTL,iBR        : TPoint;
  Kx,Ky          : double;
  oldXFORM,XFORM : tagXFORM;
  OtoC,Cnv       : TMatrix2D;
  V1,V2          : TPoint2D;
  Alpha,W,H      : double;
  OldGM          : integer;
begin
  if FPicture.Graphic = nil then exit;
  OtoC := MultMM2D(FOtoSConv,StoC);
  P1 := MultVM2D(rectTopLeft(Bounds),OtoC);
  P2 := MultVM2D(rectTopRight(Bounds),OtoC);
  P3 := MultVM2D(rectBottomLeft(Bounds),OtoC);
  P4 := MultVM2D(rectBottomRight(Bounds),OtoC);
  iTL := P2DtoP(P1);
  iBR := P2DtoP(P4);
  Alpha := Rotation;

  V1 := Vector2D(OToS(rectCenter(Bounds)),OToS(rectRight(Bounds)));
  V2 := Vector2D(OToS(rectCenter(Bounds)),OToS(rectTop(Bounds)));
  if VectorVV2D(V1,V2)>=0 then Kx := 1 else Kx :=-1;
  Ky := 1;
  W := Distance2D(P1,P2);
  H := Distance2D(P1,P3);
  if W<>0 then
    Kx := Kx * W / FPicture.Width;
  if H<>0 then
    Ky := Ky * H / FPicture.Height;

  M2D_Identity(Cnv);
  Cnv := M2D_Shift(Cnv,-iTL.X,-iTL.Y);
  Cnv := M2D_Scale(Cnv,Kx,Ky);
  Cnv := M2D_Rotate(Cnv,-Alpha);
  Cnv := M2D_Shift(Cnv,iTL.X,iTL.Y);

  XFORM.eM11 := Cnv.M11;
  XFORM.eM12 := Cnv.M12;
  XFORM.eM21 := Cnv.M21;
  XFORM.eM22 := Cnv.M22;
  XFORM.eDx  := Cnv.M31;
  XFORM.eDy  := Cnv.M32;


  oldGM := SetGraphicsMode(Canvas.Handle,GM_ADVANCED);
  getWorldTransform(Canvas.Handle,oldXFORM);
  modifyWorldTransform(Canvas.Handle,XFORM,MWT_LEFTMULTIPLY);

  Canvas.Draw(iTL.X,iTL.Y,FPicture.Graphic);

  setWorldTransform(Canvas.Handle,oldXFORM);
  SetGraphicsMode(Canvas.Handle,oldGM);
end;

procedure    TdePictureObject.PaintFrame(Canvas:TCanvas;StoC:TMatrix2D);
begin
  inherited PaintFrame(Canvas,StoC);
end;

function     TdePictureObject.Contain(   Canvas:TCanvas;StoC:TMatrix2D;X,Y:integer):boolean;
var
  CtoO : TMatrix2D;
  P    : TPoint2D;
begin
  CtoO := MultMM2D(M2D_Reverse(StoC),FROToSConv);
  P := MultVM2D(Point2D(X,Y),CtoO);
  Result := PointInRect2D(P,Bounds);
end;


(*
type
  TdeShortcutObjectData = packed record
    Header         : TdeObjectDataHeader;
    cCaption       : array[0..MAX_SHORTCUTCAPTION_SIZE] of char;
    reserved       : dword;
    iImageIndex    : integer;
    sOptions       : TdeShortcutOptions;
  end;
  PdeShortcutObjectData  = ^TdeShortcutObjectData;

{TdeShortcutObject}
class function TdeShortcutObject.ClassID:integer;
begin
  Result := 7;
end;

class function TdeShortcutObject.ClassOptions:TdeObjectClassOptions;
begin
  Result := [];
end;

constructor    TdeShortcutObject.Create(anOwner:TdeSchema);
begin
  inherited Create(anOwner);
  FBitmap      := nil;

  FCaption     := EmptyStr;
  FImages      := nil;
  FImageIndex  :=-1;
  FOptions     := [scoShowCaption,scoShowImage];

  updateBounds;
end;

destructor     TdeShortcutObject.Destroy;
begin
  if FBitmap<>nil then
    FBitmap.Free;
  inherited Destroy;
end;

procedure      TdeShortcutObject.UpdateBitmap;
begin
  if (FImages <> nil)
     and (FImageIndex >= 0)
     and (FImageIndex < FImages.Count)
  then begin
    if FBitmap = nil then
      FBitmap := TBitmap.Create;
    FImages.getBitmap(FImageIndex,FBitmap);
    FBitmap.Transparent := true;
    FBitmap.TransparentMode := tmAuto;
  end
  else begin
    if FBitmap<>nil then
      FBitmap.Free;
    FBitmap := nil;
  end;
end;

function       TdeShortcutObject.RecalcBounds:TRect2D;
begin
  Result := Rect2D(0.0,0.0,0.0,0.0);
end;

procedure      TdeShortcutObject.setCaption(aValue:string);
begin
  if FCaption = aValue then exit;
  Changing;
  FCaption := aValue;
  Changed([ocpParams]);
end;

procedure      TdeShortcutObject.setImages(aValue:TImageList);
begin
  if FImages = aValue then exit;
  Changing;
  FImages := aValue;
  UpdateBitmap;
  Changed([ocpParams]);
end;

procedure      TdeShortcutObject.setImageIndex(aValue:integer);
begin
  if FImageIndex = aValue then exit;
  Changing;
  FImageIndex := aValue;
  UpdateBitmap;
  Changed([ocpParams]);
end;

procedure      TdeShortcutObject.setOptions(aValue:TdeShortcutOptions);
begin
  if FOptions = aValue then exit;
  Changing;
  FOptions := aValue;
  UpdateBitmap;
  Changed([ocpParams]);
end;

function       TdeShortcutObject.getData(pData:pointer;var iSize:integer):boolean;
var
  nSize,inhSize : integer;
  inhData       : dword;
  pInhData      : pointer;
begin
  inhSize := 0;
  inherited getData(nil,inhSize);
  inhData := sizeOf(TdeShortcutObjectData);
  nSize := integer(inhData)+inhSize;
  Result := (nSize<=iSize);
  if Result then begin
    with PdeShortcutObjectData(pData)^ do begin
      Header.nSize := nSize;
      Header.pInheritedData := inhData;
      Header.pInheritedSize := inhSize;
      pInhData := pointer(dword(pData)+inhData);
      inherited getData(pInhData,inhSize);
      StrLCopy(@(cCaption[0]),pChar(Caption),MAX_SHORTCUTCAPTION_SIZE);
      reserved     := 0;
      iImageIndex  := ImageIndex;
      sOptions     := Options;
    end;
  end;
  iSize := nSize;
end;

procedure      TdeShortcutObject.setData(pData:pointer;    iSize:integer);
var
  inhSize   : integer;
  inhData   : dword;
  pInhData  : pointer;
begin
  if iSize < sizeOf(TdeShortcutObjectData) then exit;
  Lock;
  Changing;
  try
    with PdeShortcutObjectData(pData)^ do begin
      inhSize := Header.pInheritedSize;
      if (inhSize > 0) then begin
        inhData := Header.pInheritedData;
        pInhData := pointer(dword(pData)+inhData);
        inherited setData(pInhData,inhSize);
      end;
      FCaption       := cCaption;
      FImageIndex    := iImageIndex;
      FOptions       := sOptions;
    end;
  finally
    Changed([ocpParams]);
    unLock;
  end;
end;

function       TdeShortcutObject.CreateMemento:TdeSchemaObjectMemento;
begin
  {!TODO}
  Result := nil;
end;

procedure      TdeShortcutObject.getBoundsPoints(
                                                Canvas:TCanvas;
                                                 StoV : TMatrix2D;
                                    var P_TopLeft     : TPoint2D;
                                    var P_TopRight    : TPoint2D;
                                    var P_BottomRight : TPoint2D;
                                    var P_BottomLeft  : TPoint2D);
var
  R : TRect;
  VtoS : TMatrix2D;
  TWB,TWT : integer;
  TW      : integer;
  P  : TPoint;
begin
  VtoS := M2D_Reverse(StoV);

  P := P2DToPEx(Offset,StoV);

  if (FBitmap <> nil)
     and (scoShowImage in Options)
  then
    TWB := FBitmap.Width
  else
    TWB := 0;
  if (Caption <> EmptyStr)
     and (scoShowCaption in Options)
  then begin
    Canvas.Font.Name := 'MS Sans Serif';
    Canvas.Font.Size := 10;
    TWT := Canvas.TextWidth(Caption) + 1;
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
  R := Rect(P.X - TW,P.Y - TW,P.X + TW,P.Y + TW);

  P_TopLeft     := PtoP2D(Point(R.Left, R.Top));
  P_TopRight    := PtoP2D(Point(R.Right,R.Top));
  P_BottomRight := PtoP2D(Point(R.Right,R.Bottom));
  P_BottomLeft  := PtoP2D(Point(R.Left, R.Bottom));
end;

function       TdeShortcutObject.getViewRect(Canvas:TCanvas;StoV:TMatrix2D):TRect;
begin
  Result := inherited getViewRect(Canvas,StoV);
end;

procedure      TdeShortcutObject.PaintTo(   Canvas:TCanvas;StoC:TMatrix2D);
var
  R : TRect;
  X : integer;
begin
  Canvas.Pen.Color   := clBlack;
  Canvas.Pen.Style   := psSolid;
  Canvas.Pen.Width   := 1;
  Canvas.Brush.Color := clWhite;
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Size   := 10;
  Canvas.Font.Color  := clBlack;
  Canvas.Font.Name   := 'MS Sans Serif';

  R := getViewRect(Canvas,StoC);

  if (FBitmap <> nil)
     and (scoShowImage in Options)
  then
    Canvas.Draw((R.Right + R.Left - FBitmap.Width) div 2,R.Top,FBitmap);
  if (Caption <> EmptyStr)
     and (scoShowCaption in Options)
  then begin
    Canvas.Font.Name := 'MS Sans Serif';
    Canvas.Font.Size := 10;
    R.Top := R.Bottom - 16;
    X := (R.Right - R.Left - Canvas.TextWidth(Caption)) div 2;
    if X < 0 then
      X := 0;
    Canvas.TextRect(R,
                    R.Left + X,
                    R.Top,
                    Caption);
  end;
end;

procedure      TdeShortcutObject.PaintFrame(Canvas:TCanvas;StoC:TMatrix2D);
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


function       TdeShortcutObject.Contain(   Canvas:TCanvas;StoC:TMatrix2D;X,Y:integer):boolean;
var
  R : TRect;
begin
  R := getViewRect(Canvas,StoC);
  Result := PtInRect(R,Point(X,Y));
end;
*)

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('DeStdPalette unit initialization ...');
  {$ENDIF}
  deRegisterObjectClass(TdeTextObject);
  deRegisterObjectClass(TdePictureObject);
  //deRegisterObjectClass(TdeShortcutObject);
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DeStdPalette unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

