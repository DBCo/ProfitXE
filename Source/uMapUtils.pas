{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}

unit uMapUtils;

interface

uses System.Types, System.Classes, WinAPI.Messages, Vcl.Graphics;

const
  cLongitude = '{longitude}';
  cLatitude = '{latitude}';

  cDefaultTileSize = 256;         // Размер стороны тайла (https://tech.yandex.ru/maps/doc/theory/concepts/coordinates-docpage/#tile_coordinates)

  cStaticTileDeadHours = 30 * 24; // Время жизни обычного статичного тайла месяц (map, sat и т.п.)
  cDynamicTileDeadHours = 1;      // Время жизни динамичного тайла 1 час (trf и т.п.)
  cInfinityTileDeadHours = 100 * 30 * 24; // ~100 лет для тайлов из файла!

type
  /// <summary>Тип описания масштаба</summary>
  /// <remarks>https://tech.yandex.ru/maps/doc/staticapi/1.x/dg/concepts/map_scale-docpage/</remarks>
  TMapScale = 0..21;
  TMapScalesEnabled = array[TMapScale] of Boolean;
  TMapScalesCaption = array[TMapScale] of string;

Const
  MapDecarteScale: array[TMapScale] of Integer =
        ( 025, 028, 033, 037, 043,
          050, 057, 066, 075, 087,
          100, 115, 133, 150, 175,
          200, 230, 265, 300, 350,
          400, 460  );

type

  TTileDrawEvent = procedure(Sender: TObject; const Col, Row: Integer; const Scale: TMapScale; const Rect: TRect; Canvas: TCanvas) of object;
  TTileLoadEvent = procedure(Sender: TObject; const Col, Row, Priority: Integer; const Scale: TMapScale; const BackgroundColor: TColor; var FileName: string) of object;

  TMapCoordType =
    (
      mctDefault, // По умолчанию
      mctEllipse, // Эллипс
      mctSphere,  // Сфера
      mctDecarte  // Декартовы
    );

      TMapSourceType =
    (
      mstNull,
      mstURL,
      mstDatabase,
      mstFile
    );

  TCoordGeo = record
    X: Extended;
    Y: Extended;
    procedure SetPeople(const aX, aY: Extended); overload;
    procedure SetPeople(const aValue: TCoordGeo); overload;
    function GetPeople: TCoordGeo;
    function GetPeopleX: Extended;
    function GetPeopleY: Extended;

    procedure SetMercatorS(const aValue: TCoordGeo);
    function GetMercatorS: TCoordGeo;

    procedure SetMercatorE(const aValue: TCoordGeo);
    function GetMercatorE: TCoordGeo;

    function GetStringX: string;
    function GetStringY: string;
  end;

  TCoordPix = record
    X: LongInt;
    Y: LongInt;
    procedure SetValue(const aX, aY: LongInt);

    procedure SetMercator(const aValue: TCoordGeo; const WorldSizeX, WorldSizeY: LongInt);
    function GetMercator(const WorldSizeX, WorldSizeY: LongInt): TCoordGeo;
  end;

  TMapProperties = record
    Source: String;
    SourceType: TMapSourceType;
    Coord: TMapCoordType;

    Scale: TMapScale;
    Enabled: TMapScalesEnabled;
    Caption: TMapScalesCaption;
    OriginalX: Integer;
    OriginalY: Integer;
    MapSizeX: Integer;
    MapSizeY: Integer;
    TileSizeX: Integer;
    TileSizeY: Integer;
    TileDirectory: String;
    TileDeadInHours: Integer;
  procedure AssignMap(aMap: TMapProperties; AssignScale: Boolean = true);
  function NormalizeTileX(const TileNumber: Integer): Integer;
  function NormalizeTileY(const TileNumber: Integer): Integer;
  function GetCoordName: String;
  function SetCoordName(Value: String): Boolean;
  function HighX: Integer;
  function HighY: Integer;
  procedure SetScale(Value: Integer);
  procedure InitializeMapScales(const MinScale, MaxScale: TMapScale);
  procedure Geo2Pixel(const Geo: TCoordGeo; var Pixel:TCoordPix);
  procedure Pixel2Geo(const Pixel:TCoordPix; var Geo: TCoordGeo);
  end;

  PMapProperties = ^TMapProperties;
const
  EquatorLength = 40075016.685578488; // Длина экватора в метрах
  Rn = 6378137; // Экваториальный радиус  - Большая полуось
  Pn = 6356752.3142; // Малая полуось
  En = 0.0818191908426; // Эксцентриситет
//En = 0.081819190929; // Эксцентриситет v2

var
  MapDirectory: string;        // C:\Documents and Settings\All Users\Application Data\Prifit\MAP

function CreateTilesBitmap(Sender: TObject; const MP: TMapProperties;
                           const MinCol, MinRow, MaxCol, MaxRow: Integer; const Transparent: Byte;
                           OnTileDraw: TTileDrawEvent; OnTileLoad: TTileLoadEvent; const BackgroundColor: TColor): TBitmap;

const
  cDefaultTemplateMapYandexURL = 'https://static-maps.yandex.ru/1.x/?l=map&ll={longitude},{latitude}&z={zoom}&size=256,256&lang=ru_RU';
  cDefaultTemplateMapGoogleURL = 'https://maps.googleapis.com/maps/api/staticmap?maptype=roadmap&center={latitude},{longitude}&zoom={zoom}&size=256x256&format=PNG&language=ru&region=RU';
  cDefaultTemplateOpenStreetMapURL = 'http://a.tile.openstreetmap.org/{zoom}/{col}/{row}.png';
  cDefaultTemplate2GISURL = 'http://static.maps.2gis.com/1.0?center={longitude},{latitude}&zoom={zoom}&size=256,256';

function InitializeMapEnvirovment(var MP: TMapProperties; LayerID: Integer): Boolean;

function PrepareTileURL(const TileRow, TileCol: Integer; MP: TMapProperties): string;
function IsLocalTileFile(const MP: TMapProperties; const TileRow, TileCol: Integer; const MapScale: TMapScale; var FileName: string; var NeedReload: Boolean): Boolean;

type
  TGraphicFileType =
    (
      gftUnknown,
      gftPNG,
      gftJPG,
      gftGIF,
      gftBMP
    );

function DetectingGraphicFileType(const FileName: string): TGraphicFileType;
function DetectingGraphicFileClass(const FileName: string; var GraphicClass: TGraphicClass; var GraphicExtension: string): Boolean;

function GraphicFileTypeToExtension(const GraphicFileType: TGraphicFileType): string;
function GraphicFileTypeToGraphic(const GraphicFileType: TGraphicFileType): TGraphicClass;

function GraphicSizeToMapScale(const Width, Height: Integer): TMapScale;

function Graphic_LoadFromFile_Resize(Graphic: TGraphic; const FileName: String; const aWidth, aHeight: Integer;
                                     const BackgroundColor: TColor = clWindow): Boolean;

procedure BuildFilesInDirectory(const RootDirectory: string; Files: TStrings; const Recursived: Boolean = True);
(*
type
  TBackupProgressEvent = procedure(Sender: TObject; const FileName: string; const Index, Count: Integer; var Canceled: Boolean) of object;

function CreateBackupMapTileFiles(const FileName: string; OnProgress: TBackupProgressEvent = nil; Sender: TObject = nil): Boolean;
function RestoreBackupMapTileFiles(const FileName: string; OnProgress: TBackupProgressEvent = nil; Sender: TObject = nil): Boolean;

function DifferenceBackupMapTileFiles(const SourceFileName, TargetFileName: string; OnProgress: TBackupProgressEvent = nil; Sender: TObject = nil): Boolean;

function GetCommonMapDirectory: string;
*)
implementation

uses
  Winapi.UrlMon, System.SysUtils, System.StrUtils, System.DateUtils, System.Math,
  System.IOUtils, System.Zip, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg,
  Vcl.Imaging.GIFImg, MD5,
  DeLog, Funcs;

function GetCommonMapDirectory: string;
begin
  Result := GetApplicationDataDirectory(aftCommon);
  if Length(Result) = 0 then Result := GetApplicationDataDirectory;
  if Length(Result) <> 0 then Result := IncludeTrailingPathDelimiter(Result);
  Result := IncludeTrailingPathDelimiter(Result + cApplicationName);
  Result := IncludeTrailingPathDelimiter(Result + 'MAP');
end;

function CoordToString(const Value: Extended): string;
begin
  Result := ReplaceStr(FormatFloat('#0.000000000000', Value), FormatSettings.DecimalSeparator, '.');
  Result := Copy(Result, 1, Pos('.', Result) + 6);
end;

{ TCoordGeo }

procedure TCoordGeo.SetPeople(const aValue: TCoordGeo);
begin
  SetPeople(aValue.X, aValue.Y);
end;

function TCoordGeo.GetPeople: TCoordGeo;
begin
  Result.X := GetPeopleX;
  Result.Y := GetPeopleY;
end;

procedure TCoordGeo.SetPeople(const aX, aY: Extended);
begin
  X := Pi * aX / 180;
  Y := Pi * aY / 180;
end;

function TCoordGeo.GetPeopleX: Extended;
begin
  Result := 180 * X / Pi;
end;

function TCoordGeo.GetPeopleY: Extended;
begin
  Result := 180 * Y / Pi;
end;

function MercatorYS(y: Extended): Extended; inline;
begin
  Result:= Rn * Ln( tan(Pi / 4 + y / 2)  );
end;

procedure TCoordGeo.SetMercatorS(const aValue: TCoordGeo);
begin
  x:= aValue.X / Rn;
  y:= Pi/2 -2 * ArcTan( exp(- aValue.Y / Rn) );
end;

function TCoordGeo.GetMercatorS: TCoordGeo;
begin
  Result.X := Rn * X;
  Result.Y := MercatorYS(Y);
end;

function MercatorYE(y: Extended): Extended; inline;
begin
  Result:= Rn * Ln( tan(Pi / 4 + y / 2) * power( (1 - En * sin(y)) / (1 + En * sin(y)), En/2) );
end;

procedure TCoordGeo.SetMercatorE(const aValue: TCoordGeo);
var M,gL,gM,gH  : Extended;
begin
  x:= aValue.X / Rn;

  gL:= -Pi/2;
  gH:= +Pi/2;
  gM:=(gL+gH)/2;
  while gH-gL > 0.0000000001 do // 6 нулей для яндекса + 90 = 100 + 2 погрешность
    begin
      M:= MercatorYE(gM);
      if M > aValue.Y then gH:= gM else
      if M < aValue.Y then gL:= gM else
                           Break;
      gM:=(gL+gH)/2;
    end;
  y:= gM;
end;

function TCoordGeo.GetMercatorE: TCoordGeo;
begin
  Result.X := Rn * X;
  Result.Y := MercatorYE(Y);
end;

function TCoordGeo.GetStringX: string;
begin
  Result := CoordToString(X);
end;

function TCoordGeo.GetStringY: string;
begin
  Result := CoordToString(Y);
end;

{ TCoordPix }

procedure TCoordPix.SetValue(const aX, aY: Integer);
begin
  X := aX;
  Y := aY;
end;

procedure TCoordPix.SetMercator(const aValue: TCoordGeo; const WorldSizeX, WorldSizeY : Integer);
begin
  X := Round(worldSizeX * (1/2 + aValue.X / equatorLength));
  Y := Round(worldSizeY * (1/2 - aValue.Y / equatorLength));
end;

function TCoordPix.GetMercator(const WorldSizeX, WorldSizeY: Integer): TCoordGeo;
begin
  Result.X :=   equatorLength * ( X / WorldSizeX - 1/2);
  Result.Y := - equatorLength * ( Y / WorldSizeY - 1/2);
end;

function DetectingGraphicFileType(const FileName: string): TGraphicFileType;
var
  Stream: TStream;
  Signature: Cardinal;
begin
  Result := gftUnknown;
  try
    if FileExists(FileName) then
      begin
        Stream := TFileStream.Create(FileName, fmOpenRead);
        try
          if Stream.Size > 3 then
            begin
              Stream.ReadBuffer(Signature, SizeOf(Signature));
              // Если это PNG файл, то ...
              if Signature = $474E5089 then
                Result := gftPNG
              else
                begin
                  Signature := Signature and $00FFFFFF;
                  // Если это GIF файл, то ...
                  if Signature = $464947 then
                    Result := gftGIF
                  else
                    // Если это JPEG файл, то ...
                    if Signature = $FFD8FF then
                        Result := gftJPG
                    else
                      begin
                        Signature := Signature and $0000FFFF;
                        // Если это BMP файл, то ..
                        if Signature = $4D42 then
                          Result := gftBMP;
                      end;
                end;
            end;
        finally
          Stream.Free;
        end;
      end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('DetectingGraphicFileType(%s) skip error: %s', [QuotedStr(FileName), E.Message]);
        {$ENDIF}
        Result := gftUnknown;
      end;
  end;
end;

function GraphicFileTypeToExtension(const GraphicFileType: TGraphicFileType): string;
begin
  case GraphicFileType of
    gftJPG: Result := '.jpg';
    gftPNG: Result := '.png';
    gftGIF: Result := '.gif';
    gftBMP: Result := '.bmp';
  else
    Result := EmptyStr;
  end;
end;

function GraphicFileTypeToGraphic(const GraphicFileType: TGraphicFileType): TGraphicClass;
begin
  case GraphicFileType of
    gftJPG: Result := TJPEGImage;
    gftPNG: Result := TPngImage;
    gftGIF: Result := TGIFImage;
    gftBMP: Result := TBitmap;
  else
    Result := nil;
  end;
end;

function DetectingGraphicFileClass(const FileName: string; var GraphicClass: TGraphicClass; var GraphicExtension: string): Boolean;
begin
  try
    case DetectingGraphicFileType(FileName) of
      gftJPG: { JPEG }
        begin
          GraphicClass := TJPEGImage;
          GraphicExtension := '.jpg';
          Result := True;
        end;
      gftPNG: { PNG }
        begin
          GraphicClass := TPngImage;
          GraphicExtension := '.png';
          Result := True;
        end;
      gftGIF: { GIF }
        begin
          GraphicClass := TGIFImage;
          GraphicExtension := '.gif';
          Result := True;
        end;
      gftBMP: { BMP }
        begin
          GraphicClass := TBitmap;
          GraphicExtension := '.bmp';
          Result := True;
        end;
    else
      GraphicClass := nil;
      GraphicExtension := EmptyStr;
      Result := False;
    end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('DetectingGraphicFileClass(%s) skip error: %s', [QuotedStr(FileName), E.Message]);
        {$ENDIF}
        Result := False;
      end;
  end;
end;

function GraphicSizeToMapScale(const Width, Height: Integer): TMapScale;
var
  Size: Integer;
  Index: TMapScale;
begin
  Result:= Low(Index);
  Size := Max(Width, Height);
  // Начнём искать масштаб для картинки с 0-го ...
  for Index := Low(TMapScale) to High(TMapScale) do
    if Size <= (1 shl Index) * cDefaultTileSize then
      Exit(Index);
end;

function CreateTilesBitmap(Sender: TObject; const MP: TMapProperties;
                           const MinCol, MinRow, MaxCol, MaxRow: Integer; const Transparent: Byte;
                           OnTileDraw: TTileDrawEvent; OnTileLoad: TTileLoadEvent; const BackgroundColor: TColor): TBitmap;
var
  Y, X, Row, Col, nRow, nCol: Integer;
  FileName: string;
  FileLoaded, NeedReload: Boolean;

  function DownloadTileFile(const TileRow, TileCol: Integer; const MapScale: TMapScale; var FileName: string; const BackgroundColor: TColor): Boolean;
  var
    URL: string;
    TempFileName, Extension: string;
    GraphicClass: TGraphicClass;
    Graphic: TGraphic;
  begin
    Result := False;
    try
      FileName := EmptyStr;
      URL := PrepareTileURL(TileRow, TileCol, MP);
      TempFileName := TPath.GetTempFileName;
      if FileExists(TempFileName) then DeleteFile(TempFileName);
      try
        if URLDownloadToFile(nil, PChar(URL), PChar(TempFileName), 0, nil) = S_OK then
          if DetectingGraphicFileClass(TempFileName, GraphicClass, Extension) then
            begin
              Graphic := GraphicClass.Create;
              try
                Graphic.LoadFromFile(TempFileName);
                FileName := IncludeTrailingBackslash(MP.TileDirectory + IntToStr(MapScale)) + Format('%d_%d%s', [TileRow, TileCol, Extension]);
                ForceDirectories(ExtractFilePath(FileName));
                Graphic.SaveToFile(FileName);

                DeleteFile(TempFileName);
                Result := True;
              finally
                Graphic.Free;
              end;
            end
          else
            begin
              {$IFDEF DeDEBUG}
              WriteLog('Error: Download tile %s unknown format', [QuotedStr(URL)], 'MAP');
              {$ENDIF}
              {$IFDEF DEBUG}
              DebugLog('DownloadTileFile(%d, %d, %d) skip error: Unknown downloaded tile file format', [TileRow, TileCol, MapScale]);
              {$ENDIF}
            end
        else
          begin
            {$IFDEF DeDEBUG}
            WriteLog('Error: Download tile %s failed', [QuotedStr(URL)], 'MAP');
            {$ENDIF}
          end;
      finally
        if FileExists(TempFileName) then DeleteFile(TempFileName);
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog('DownloadTileFile(%d, %d, %d) skip error: %s', [TileRow, TileCol, MapScale, E.Message]);
          {$ENDIF}
          Result := False;
        end;
    end;
  end;

  procedure DefaultDraw;
  var
  {$IFDEF RELEASE}
    i,j: Integer;
  {$ELSE}
    Text: string;
  {$ENDIF}
  begin

  {$IFDEF RELEASE}
    for i:=0 to 15 do
      for j:=0 to 15 do
        begin
          Result.Canvas.Pixels[X + i*16 + 7, Y + j*16 + 7]:= $00E0E0E0;
          Result.Canvas.Pixels[X + i*16 + 7, Y + j*16 + 8]:= $00E0E0E0;
          Result.Canvas.Pixels[X + i*16 + 8, Y + j*16 + 7]:= $00E0E0E0;
          Result.Canvas.Pixels[X + i*16 + 8, Y + j*16 + 8]:= $00E0E0E0;
        end;
  {$ELSE}
    Result.Canvas.Pen.Color := $00D0D0D0;
    Result.Canvas.Rectangle(X, Y, X + MP.TileSizeX, Y + MP.TileSizeY);
    Text := Format('%d  x:%d y:%d', [MP.Scale, nCol, nRow]);
    Result.Canvas.Font.Color := clSilver;
    Result.Canvas.TextOut(X + ((MP.TileSizeX - Result.Canvas.TextWidth(Text)) div 2),
                          Y + ((MP.TileSizeY - Result.Canvas.TextHeight(Text)) div 2), Text);
  {$ENDIF}
  end;

  procedure ExceptionDraw(const E: Exception);
  var
    Text: string;
    Bottom: Integer;
  begin
    Result.Canvas.Pen.Color := $00D0D0D0;
    Result.Canvas.Rectangle(X, Y, X + MP.TileSizeX, Y + MP.TileSizeY);
    Text := Format('%d  x:%d y:%d', [MP.Scale, nCol, nRow]);
    Result.Canvas.Font.Color := clSilver;
    Bottom := Y + MP.TileSizeY - Result.Canvas.TextHeight(Text);
    Result.Canvas.TextOut(X + MP.TileSizeX - Result.Canvas.TextWidth(Text), Bottom, Text);
    if Assigned(E) then
      begin
        Text := ReplaceText(ReplaceText(E.Message, #13, ' '), #10, EmptyStr);
        Result.Canvas.Font.Color := clBlack;
        Result.Canvas.TextRect(Rect(X, Y, X + MP.TileSizeX, Bottom), X, Y, Text);
      end;
  end;

  procedure FileDrawZoom(_2: Integer = 2);
  var
    GraphicClass: TGraphicClass;
    Graphic: TGraphic;
    FBitMap: TBitmap;
    i,j,dX,DY: Integer;
    iBA, oBA: PByteArray;
  begin
    try
      GraphicClass := GraphicFileTypeToGraphic(DetectingGraphicFileType(FileName));
      if Assigned(GraphicClass) then
        begin
          Graphic := GraphicClass.Create;
          try
            Graphic_LoadFromFile_Resize( Graphic, FileName, MP.TileSizeX, MP.TileSizeY, BackgroundColor);
            FBitMap:= TBitmap.Create;
            try
              FBitMap.PixelFormat:= pf24bit;
              FBitMap.SetSize(Graphic.Width, Graphic.Height);
              FBitMap.Canvas.Draw(0, 0, Graphic);

              dX:= (nCol mod _2)*(MP.TileSizeX div _2);
              dY:= (nRow mod _2)*(MP.TileSizeY div _2);
              for j:=0 to Pred(MP.TileSizeY div _2) do
                begin
                  iBA := FBitMap.ScanLine[j+dY];
                  oBA := Result.ScanLine[Y+_2*j];
                  for i:=0 to Pred(MP.TileSizeX div _2) do
                    Move(iBA[(dX+i)*3], oBA[(X+i*_2)*3], 3);
                end;
            finally
              FBitMap.Free;
            end;
          finally
            Graphic.Free;
          end;
        end;
    except
      on E: Exception do ExceptionDraw(E);
    end;
  end;

  procedure FileDrawStretch;
  var
    GraphicClass: TGraphicClass;
    Graphic: TGraphic;
  begin
    try
      GraphicClass := GraphicFileTypeToGraphic(DetectingGraphicFileType(FileName));
      if Assigned(GraphicClass) then
        begin
          Graphic := GraphicClass.Create;
          try
            Graphic_LoadFromFile_Resize( Graphic, FileName, MP.OriginalX, MP.OriginalY, BackgroundColor);
            try
              Result.Canvas.StretchDraw( Rect(0, 0, Result.Width, Result.Height), Graphic);
            finally
            end;
          finally
            Graphic.Free;
          end;
        end;
    except
      on E: Exception do ExceptionDraw(E);
    end;
  end;

  procedure FileDraw;
  var
    GraphicClass: TGraphicClass;
    Graphic: TGraphic;
    Bitmap: TBitmap;
  begin
    try
      GraphicClass := GraphicFileTypeToGraphic(DetectingGraphicFileType(FileName));
      if Assigned(GraphicClass) then
        begin
          Graphic := GraphicClass.Create;
          try
            Graphic_LoadFromFile_Resize( Graphic, FileName, MP.TileSizeX, MP.TileSizeY, BackgroundColor);
            if Transparent < 255 then
              begin
                Bitmap := TBitmap.Create;
                Bitmap.SetSize(MP.TileSizeX, MP.TileSizeY);
                try
                  Bitmap.Canvas.Draw(0, 0, Graphic);
                  Result.Canvas.Brush.Color := BackgroundColor;
                  Result.Canvas.FillRect(Rect( X, Y, X + MP.TileSizeX, Y + MP.TileSizeY));
                  Result.Canvas.Draw(X, Y, Bitmap, Transparent);
                finally
                  Bitmap.Free;
                end;
              end
            else
              begin
                Result.Canvas.Draw(X, Y, Graphic);
              end;
          finally
            Graphic.Free;
          end;
        end;
    except
      on E: Exception do ExceptionDraw(E);
    end;
  end;
begin
  Result := TBitmap.Create;
  try
    Result.PixelFormat := pf24bit;
    Result.SetSize( (1+MaxCol-MinCol) * MP.TileSizeX, (1+MaxRow-MinRow) * MP.TileSizeY);
    Result.Canvas.Brush.Color := BackgroundColor;
    Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));

    Y := 0;
    for Row := MinRow to MaxRow do
      begin
        nRow:= MP.NormalizeTileY(Row);
        X := 0;
        for Col := MinCol to MaxCol do
          begin
            nCol := MP.NormalizeTileX(Col);
            // Если файл тайла есть в кэше и он не старый, то ...

            if MP.SourceType in [mstFile, mstDatabase] then
              begin
                FileName:= MP.Source;
                FileDrawStretch;
              end
            else
              begin // Иначе если есть внешний загрузчик тайлов, то используем его ...
                FileLoaded:= IsLocalTileFile(MP, nRow, nCol, MP.Scale, FileName, NeedReload);

                if NeedReload and (MP.SourceType = mstURL) then
                  if Assigned(OnTileLoad) then
                    OnTileLoad(Sender, Col, Row, Sqr(2*Col - (MinCol+MaxCol)) + Sqr(2*Row - (MinRow+MaxRow)),
                               MP.Scale, BackgroundColor, FileName)
                  else
                    FileLoaded:= DownloadTileFile( nRow, nCol, MP.Scale, FileName, BackgroundColor);

                if FileLoaded
                  then FileDraw
                  else if IsLocalTileFile(MP, nRow div 2, nCol div 2, MP.Scale-1, FileName, NeedReload)
                         then FileDrawZoom(2)
                         else if IsLocalTileFile(MP, nRow div 4, nCol div 4, MP.Scale-2, FileName, NeedReload)
                              then FileDrawZoom(4)
                              else if IsLocalTileFile(MP, nRow div 8, nCol div 8, MP.Scale-3, FileName, NeedReload)
                                   then FileDrawZoom(8)
                                   else DefaultDraw;
              end;

            // Если есть обработчик дополнительной отрисовки поверх тайла, то используем его ...
            if Assigned(OnTileDraw) then
              OnTileDraw(Sender, nCol, nRow, MP.Scale, Rect(X, Y, X + MP.TileSizeX, Y + MP.TileSizeY), Result.Canvas);
            Inc(X, MP.TileSizeX);
          end;
        Inc(Y, MP.TileSizeY);
      end;
  except
    Result.Free;
    raise;
  end;
end;

function PictureFileToTileMap(const MapScale: TMapScale; const FileName: string; const TemplateFileName: string): Boolean;
var
  GraphicClass: TGraphicClass;
  GraphicExtension, TileFileName: string;
  Graphic: TGraphic;
  Picture: TPicture;
  SquareBitmap, TileBitmap: TBitmap;
  X, Y, Count, Row, Col, Age: Integer;
begin
  Result := DetectingGraphicFileClass(FileName, GraphicClass, GraphicExtension);
  try
    if Result then
      begin
        SquareBitmap := TBitmap.Create;
        try
          Picture:= TPicture.Create;
          try
            Picture.LoadFromFile(FileName);
            Age := FileAge(FileName);
            SquareBitmap.PixelFormat := pf24bit;
            Count := 1 shl MapScale; // Получаем количество тайлов ...
            SquareBitmap.SetSize(Count * cDefaultTileSize, Count * cDefaultTileSize);
            SquareBitmap.Canvas.Brush.Color := clWhite;
            SquareBitmap.Canvas.FillRect(Rect(0, 0, SquareBitmap.Width, SquareBitmap.Height));
            SquareBitmap.Canvas.Draw((SquareBitmap.Width - Picture.Width) div 2, (SquareBitmap.Height - Picture.Height) div 2, Picture.Graphic);
            //SquareBitmap.SaveToFile('D:\map.bmp');
          finally
            Picture.Free;
          end;
          TileBitmap := TBitmap.Create;
          try
            TileBitmap.PixelFormat := pf24bit;
            TileBitmap.SetSize(cDefaultTileSize, cDefaultTileSize);
            Count := 1 shl MapScale; // Получаем количество тайлов ...
            Y := 0;
            for Row := 0 to Pred(Count) do
              begin
                X := 0;
                for Col := 0 to Pred(Count) do
                  begin
                    TileFileName := ReplaceText(ReplaceText(ReplaceText(TemplateFileName, '{zoom}', IntToStr(MapScale)), '{row}', IntToStr(Row)), '{col}', IntToStr(Col));
                    TileBitmap.Canvas.CopyRect(Rect(0, 0, TileBitmap.Width, TileBitmap.Height), SquareBitmap.Canvas, Rect(X, Y, X + cDefaultTileSize, Y + cDefaultTileSize));
                    Graphic := GraphicClass.Create;
                    try
                      Graphic.Assign(TileBitmap);
                      ForceDirectories(ExtractFilePath(TileFileName));
                      Graphic.SaveToFile(TileFileName);
                      FileSetDate(TileFileName, Age)
                    finally
                      Graphic.Free;
                    end;
                    Inc(X, cDefaultTileSize);
                  end;
                Inc(Y, cDefaultTileSize);
              end;
          finally
            TileBitmap.Free;
          end;
        finally
          SquareBitmap.Free;
        end;
      end;
  except
    on E: Exception do
      begin
        Result := False;
        {$IFDEF DeDEBUG}
        WriteLog('Cropping file %s error: %s', [QuotedStr(FileName), E.Message], 'MAP');
        {$ENDIF}
        {$IFDEF DEBUG}
        DebugLog('PictureFileToTileMap(' + IntToStr(MapScale) + ', ' + QuotedStr(FileName) + ', ' + QuotedStr(TemplateFileName) + ') error: ' + E.Message);
        {$ENDIF}
      end;
  end;
end;

type
  PDWORD = ^DWORD;

function SetErrorMode(uMode: Cardinal): Cardinal; stdcall; external 'kernel32.dll' name 'SetErrorMode';
function GetVolumeInformation(lpRootPathName: PWideChar; lpVolumeNameBuffer: PWideChar; nVolumeNameSize: DWORD; lpVolumeSerialNumber: PDWORD; var lpMaximumComponentLength, lpFileSystemFlags: DWORD; lpFileSystemNameBuffer: PWideChar; nFileSystemNameSize: DWORD): LongBool; stdcall; external 'kernel32.dll' name 'GetVolumeInformationW';

function GetSerialNumerDrive(const DriveName: string): Integer;
const
  MAX_PATH = 260;
  SEM_FAILCRITICALERRORS = 1;
var
  ErrorMode: Cardinal;
  Buffer: array[0..MAX_PATH] of Char;
  MaximumComponentLength, FileSystemFlags: Cardinal;
begin
  ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    if not GetVolumeInformation(PChar(DriveName), Buffer, SizeOf(Buffer), @Result, MaximumComponentLength, FileSystemFlags, nil, 0) then
      Result := 0;
  finally
    SetErrorMode(ErrorMode);
  end;
end;

function InitializeMapFileDirectoryEnvirovment(const BaseDirectory: string; const DeadInHours: Integer; var MP: TMapProperties): Boolean;
var
  Value, FullDirectory, Directory, SubDirectory, FileName, SourceFileName: string;
  Index, Count: Integer;
begin
  Directory := EmptyStr;
  SubDirectory := EmptyStr;
  FileName := EmptyStr;
  Value := Trim(ReplaceText(BaseDirectory, '/', '\'));
  Index := LastDelimiter('\', Value); // Поиск разделителя для имени файла и каталога ...
  Result := Index <> 0;
  if Result then
    begin
      FullDirectory := Trim(Copy(Value, 1, Pred(Index)));
      if Length(FullDirectory) <> 0 then FullDirectory := IncludeTrailingBackslash(FullDirectory);
      FileName := Trim(Copy(Value, Succ(Index), Length(Value)));
      Result := Pos('{row}', LowerCase(FileName)) <> 0;
      if Result then
        begin
          Result := Pos('{col}', LowerCase(FileName)) <> 0;
          if Result then
            begin
              Index := Pos('{zoom}', LowerCase(FullDirectory));
              Result := Index <> 0;
              if Result then
                begin
                  SubDirectory := Copy(FullDirectory, Index, Length(FullDirectory));
                  Directory := Copy(FullDirectory, 1, Pred(Index));
                  Index := LastDelimiter('\', Directory);
                  if Index = 0 then
                    begin
                      SubDirectory := Directory + SubDirectory;
                      Directory := EmptyStr;
                    end
                  else
                    begin
                      SubDirectory := Copy(Directory, Succ(Index), Length(Directory)) + SubDirectory;
                      Directory := Copy(Directory, 1, Index);
                    end;
                  MP.InitializeMapScales(0, 20);
                  if Length(Directory) = 0 then
                    begin
                      {$IFDEF DeDEBUG}
                      WriteLog('Warning: Automatic cutting of files is disabled because of the empty name of the base foldier.', 'MAP');
                      {$ENDIF}
                    end
                  else
                    begin
                      Count := 0;
                      for Index := 0 to 6 do
                        begin
                          Value := ReplaceText(FullDirectory, '{zoom}', IntToStr(Index));
                          // При отсутствии каталога масштаба пытаемся создать из картинки ...
                          if DirectoryExists(Value) then
                            Inc(Count)
                          else
                            begin
                              SourceFileName := Directory + IntToStr(Index) + ExtractFileExt(FileName);
                              if FileExists(SourceFileName) then
                                if PictureFileToTileMap(Index, SourceFileName, FullDirectory + FileName) then
                                  Inc(Count)
                                else
                                  MP.Enabled[Index] := False // Если не удалось нарезать картику на тайлы, то не даём выбрать этот масштаб!!!
                              else
                                MP.Enabled[Index] := False; // Ничего нет - не даём выбрать этот масштаб!!!
                            end;
                        end;
                      Result := Count <> 0; // Инициализировано только при наличии хотя бы одного масштаба!!!
                    end;
                end
              else
                begin
                  {$IFDEF DeDEBUG}
                  WriteLog('Error: Macros ''{zoom}'' in template map foldier name %s not detected', [QuotedStr(BaseDirectory)], 'MAP');
                  {$ENDIF}
                end;
            end
          else
            begin
              {$IFDEF DeDEBUG}
              WriteLog('Error: Macros ''{col}'' in template map file name %s not detected', [QuotedStr(BaseDirectory)], 'MAP');
              {$ENDIF}
            end;
        end
      else
        begin
          {$IFDEF DeDEBUG}
          WriteLog('Error: Macros ''{row}'' in template map file name %s not detected', [QuotedStr(BaseDirectory)], 'MAP');
          {$ENDIF}
        end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      WriteLog('Error: Directory separator in template map %s not detected', [QuotedStr(BaseDirectory)], 'MAP');
      {$ENDIF}
    end;
  if Result then
    begin
      //Value := ReplaceText(ReplaceText(ReplaceText(ExtractFileDrive(FullDirectory), '\\', EmptyStr), '\', '_'), ':', '$');
      Value := IntToHex(GetSerialNumerDrive(IncludeTrailingBackslash(ExtractFileDrive(FullDirectory))), 8) + '\' +
               ReplaceText(ReplaceText(GUIDToString(MD5String(BaseDirectory).G), '{', EmptyStr), '}', EmptyStr);
      MP.TileDirectory := IncludeTrailingBackslash(MapDirectory + Value);
      MP.Source:= 'file://' + BaseDirectory;
      MP.Coord:= mctDecarte;
      if DeadInHours < 0 then MP.TileDeadInHours := cInfinityTileDeadHours
                         else MP.TileDeadInHours := DeadInHours;
    end;
end;

function InitializeMapEnvirovment(var MP: TMapProperties; LayerID: Integer): Boolean;
var
  Host, Value: string;
  Index: Integer;
  Parameters: TStrings;
begin
  MP.TileDeadInHours := -1;

  if Length(MP.Source)=0 then
    begin
      MP.Coord:= mctDecarte;
      MP.SourceType:= mstNull;
      MP.TileSizeX:= cDefaultTileSize;
      MP.TileSizeY:= cDefaultTileSize;
      MP.MapSizeX:= 4096;
      MP.MapSizeY:= 4096;
      Exit;
    end;

  MP.TileSizeX:= cDefaultTileSize;
  MP.TileSizeY:= cDefaultTileSize;

  Result := True;
  if Length(MapDirectory) = 0 then
    MapDirectory := GetCommonMapDirectory
  else
    MapDirectory := IncludeTrailingBackslash(MapDirectory);

  Value := Trim(MP.Source);
  if Pos('HTTPS://', UpperCase(Value)) = 1 then
    begin
      Delete(Value, 1, 8);
      MP.SourceType:= mstURL;
    end
  else if Pos('HTTP://', UpperCase(Value)) = 1 then
    begin
      Delete(Value, 1, 7);
      MP.SourceType:= mstURL;
    end
  else if Pos('FILE://', UpperCase(Value)) = 1 then
    begin
      Delete(Value, 1, 7);
      MP.SourceType:= mstFile;
      Result := InitializeMapFileDirectoryEnvirovment(Value, MP.TileDeadInHours, MP);
      Exit;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      WriteLog('Error: Unknown template map URL protocol %s', [QuotedStr(MP.Source)], 'MAP');
      {$ENDIF}
      Result := False;
    end;

  if Result then
    begin
      Parameters := TStringList.Create;
      try
        Index := Pos('?', Value);
        if Index <> 0 then
          begin
            Parameters.Text := ReplaceText(Copy(Value, Succ(Index), Length(Value)), '&', #13);
            Value := Trim(Copy(Value, 1, Pred(Index)));
          end;
        Index := Pos('/', Value);
        if Index <> 0 then
          Value := Trim(Copy(Value, 1, Pred(Index)));
        Host := EmptyStr;
        if MP.TileDeadInHours < 0 then
          MP.TileDeadInHours := cStaticTileDeadHours;
        Index := LastDelimiter('.', Value);
        if Index <> 0 then
          begin
            Host := Copy(Value, Index, Length(Value));
            Delete(Value, Index, Length(Value));
            Index := LastDelimiter('.', Value);
            if Index = 0 then
              Host := Value + Host
            else
              Host := Copy(Value, Succ(Index), Length(Value)) + Host;
          end;
        if Length(Host) = 0 then
          begin
           {$IFDEF DeDEBUG}
           WriteLog('Warning: Second level domain in template map URL %s not detected. Used ''Unknown'' folder', [QuotedStr(MP.Source)], 'MAP');
           {$ENDIF}
           Host := 'Unknown';
          end;

        Host:= IncludeTrailingBackslash(Host)+'Id'+IntToStr(LayerID);

        if Pos('YANDEX', UpperCase(Host)) <> 0 then
          begin
            // Настройки Yandex карт ...
            if MP.Coord = mctDefault then MP.Coord:= mctEllipse;

            Value := Trim(Parameters.Values['lang']);
            if Length(Value) > 0 then
              Host:= Host + '_' + NormalizeFileName(Value, True);

            Value := Trim(Parameters.Values['l']);
            if Length(Value) > 0 then
              Host:= Host + '_' + NormalizeFileName(Value, True);

            if (Pos('trf', LowerCase(Value)) <> 0) and (MP.TileDeadInHours < 0) then
              MP.TileDeadInHours := cDynamicTileDeadHours;

            MP.InitializeMapScales(0, 21);
          end

        else if Pos('GOOGLE', UpperCase(Host)) <> 0 then
          begin
            // Настройки Google карт ...
            if MP.Coord = mctDefault then MP.Coord:= mctEllipse;

            Value := Trim(Parameters.Values['language']);
            if Length(Value) > 0 then
              Host:= Host + '_' + NormalizeFileName(Value, True);

            Value := Trim(Parameters.Values['region']);
            if Length(Value) > 0 then
              Host:= Host + '_' + NormalizeFileName(Value, True);

            Value := Trim(Parameters.Values['maptype']);
            if Length(Value) > 0 then
              Host:= Host + '_' + NormalizeFileName(Value, True);

            MP.InitializeMapScales(0, 20);
          end

        else if Pos('OPENSTREETMAP', UpperCase(Host)) <> 0 then
          begin
            // Настройки OpenStreetMap карт ...
            if MP.Coord = mctDefault then MP.Coord:=mctSphere;
            MP.InitializeMapScales(0, 19);
          end

        else if Pos('2GIS', UpperCase(Host)) <> 0 then
          begin
            // Настройки 2GIS карт ...
            if MP.Coord = mctDefault then MP.Coord:=mctSphere;
            MP.InitializeMapScales(1, 18);
          end

        else if Pos('VIRTUALEARTH.NET', UpperCase(Host)) <> 0 then
          begin
            // Настройки Microsoft Bing карт ...
            if MP.Coord = mctDefault then MP.Coord:=mctSphere;

            Value := Trim(Parameters.Values['mapLayer']);
            if Length(Value) > 0 then
              Host:= Host + '_' + NormalizeFileName(Value, True);

            MP.InitializeMapScales(1, 20);
          end

        else
          begin
            MP.InitializeMapScales(0, 17);
          end;

        Host:= IncludeTrailingBackslash(Host);

        Value:= LowerCase(MP.Source);
        Result:= ((Pos(cLongitude, Value) <> 0) and (Pos(cLatitude, Value) <> 0)) or
                 ((Pos('{row}', Value) <> 0) and (Pos('{col}', Value) <> 0));

        if result then
          begin
            MP.TileDirectory := IncludeTrailingBackslash(MapDirectory + Host);
            if (Pos('{zoom}', Value) = 0) then
              MP.InitializeMapScales(0, 0); // Если не используется "zoom" оставляем один масштаб
            if MP.Coord = mctDefault then
              MP.Coord:= mctEllipse; // По умолчанию тип координат карты - Эллипс!
          end
        else
          WriteLog('Error: ''+cLatitude+''/''+cLongitude+'' or ''{col}''/''{row}'' in template map URL %s not detected', [QuotedStr(MP.Source)], 'MAP');

      finally
        Parameters.Free;
      end;
    end;
end;

function PrepareTileURL(const TileRow, TileCol: Integer; MP: TMapProperties): string;
var
  Pixel: TCoordPix;
  Mercator, People: TCoordGeo;
begin
  Result := ReplaceText(ReplaceText(ReplaceText(MP.Source, '{zoom}', IntToStr(MP.Scale)), '{row}', IntToStr(TileRow)), '{col}', IntToStr(TileCol));

  // Если в запросе есть широта или долгота, то ...
  if (Pos(cLongitude, LowerCase(Result)) <> 0) or (Pos(cLatitude, LowerCase(Result)) <> 0) then
    begin
      Pixel.SetValue(TileCol * MP.TileSizeX + MP.TileSizeX div 2, TileRow * MP.TileSizeY + MP.TileSizeY div 2);
      // Если тип координат карты "Сфера", то ...
      if MP.Coord = mctSphere then
        Mercator.SetMercatorS(Pixel.GetMercator((1 shl MP.Scale) * MP.TileSizeX, (1 shl MP.Scale) * MP.TileSizeY))
      else
        Mercator.SetMercatorE(Pixel.GetMercator((1 shl MP.Scale) * MP.TileSizeX, (1 shl MP.Scale) * MP.TileSizeY));
      People := Mercator.GetPeople;
      Result := ReplaceText(ReplaceText(Result, cLongitude, People.GetStringX), cLatitude, People.GetStringY);
    end;
end;

function IsLocalTileFile(const MP: TMapProperties; const TileRow, TileCol: Integer; const MapScale: TMapScale; var FileName: string; var NeedReload: Boolean): Boolean;
var
  DateTime, TileDateTime: TDateTime;
begin
  FileName := IncludeTrailingBackslash(MP.TileDirectory + IntToStr(MapScale)) + Format('%d_%d.png', [TileRow, TileCol]);
  Result := DirectoryExists(ExtractFilePath(FileName));
  if Result then
    begin
      // Если нет PNG, то ...
      if not FileExists(FileName) then
        begin
          FileName := ChangeFileExt(FileName, '.jpg');
          // Если нет JPG, то ...
          if not FileExists(FileName) then
            begin
              FileName := ChangeFileExt(FileName, '.gif');
              // Если нет GIF, то ...
              if not FileExists(FileName) then
                begin
                  FileName := ChangeFileExt(FileName, '.bmp');
                  Result := FileExists(FileName);
                end;
            end;
        end;
    end;

  // Если есть файла тайла, то вернём результат по дате актуальности данных в тайле ...
  if Result then
    begin
      TileDateTime := IncHour(FileDateToDateTime(FileAge(FileName)), MP.TileDeadInHours);
      DateTime := Now;
      NeedReload := TileDateTime < DateTime;
      {$IFDEF DEBUG}
      if NeedReload then
        DebugLog('Need reaload %s: %s < %s = True', [QuotedStr(FileName),
          FormatDateTime('DD"."MM"."YYYY" "HH":"NN":"SS', TileDateTime),
          FormatDateTime('DD"."MM"."YYYY" "HH":"NN":"SS', DateTime)
          ]);
      {$ENDIF}
    end
  else
    NeedReload := True;
end;


function Graphic_LoadFromFile_Resize(Graphic: TGraphic; const FileName: String; const aWidth, aHeight: Integer;
                                     const BackgroundColor: TColor = clWindow): Boolean;
var
  Age: Integer;
  aBitmap: TBitmap;
begin
  try
    Graphic.LoadFromFile(FileName);
  except
    Exit(False);
  end;

  Result:= False;
  if assigned(Graphic) then
    if (Graphic.Width = aWidth) and (Graphic.Height = aHeight) then
      Result:= True
    else
      begin
        aBitmap := TBitmap.Create;
        try
          aBitmap.PixelFormat := pf24bit;
          aBitmap.SetSize(aWidth, aHeight);
          aBitmap.Canvas.Brush.Color := BackgroundColor;
          aBitmap.Canvas.FillRect(Rect(0, 0, aBitmap.Width, aBitmap.Height));
          aBitmap.Canvas.Draw((aBitmap.Width - Graphic.Width) div 2, (aBitmap.Height - Graphic.Height) div 2, Graphic);
          Graphic.Assign(aBitmap);
          Result:= True;
        finally
          aBitmap.Free;
        end;

        // Cохраняем файл с новым размером и с датой модификации исходного файла для правильного времени жизни тайла ...
        Age := FileAge(FileName);
        Graphic.SaveToFile(FileName);
        FileSetDate(FileName, Age);
      end;
end;

procedure BuildFilesInDirectory(const RootDirectory: string; Files: TStrings; const Recursived: Boolean);
var
  MainDirectory: string;
  procedure ScanDirectory(const SubDirectory: string);
  const
    faExclude = faDirectory or faVolumeID;
  var
    Directory: string;
    ErrorCode: Integer;
    SearchRec: TSearchRec;
  begin
    Directory := SubDirectory;
    if Length(Directory) <> 0 then
      Directory := IncludeTrailingBackslash(Directory);
    ErrorCode := FindFirst(MainDirectory + Directory + '*.*', faAnyFile, SearchRec);
    if ErrorCode = 0 then
      try
        while ErrorCode = 0 do
          begin
            if (SearchRec.Attr and faExclude) = 0 then
              Files.Append(Directory + SearchRec.Name)
            else
              if Recursived and ((SearchRec.Attr and faDirectory) <> 0) and (LeftStr(SearchRec.Name, 1) <> '.') then
                ScanDirectory(Directory + SearchRec.Name);
            ErrorCode := FindNext(SearchRec);
          end;
      finally
        FindClose(SearchRec);
      end;
  end;
begin
  if Assigned(Files) then
    begin
      MainDirectory := RootDirectory;
      if Length(MainDirectory) <> 0 then MainDirectory := IncludeTrailingPathDelimiter(MainDirectory);
      ScanDirectory(EmptyStr);
    end;
end;

(*
function IsNoPackFileName(const FileName: string): Boolean;
var
  Extension: string;
begin
  Extension := ExtractFileExt(FileName);
  Result := SameText(Extension, '.jpg') or SameText(Extension, '.jpeg') or SameText(Extension, '.png') or SameText(Extension, '.gif') or SameText(Extension, '.bmp');
end;

function CreateBackupMapTileFiles(const FileName: string; OnProgress: TBackupProgressEvent; Sender: TObject): Boolean;
var
  Files: TStrings;
  ZipFile: TZipFile;
  Index: Integer;
  Canceled: Boolean;
begin
  Result := True;
  try
    Files := TStringList.Create;
    try
      BuildFilesInDirectory(MapDirectory, Files);
      Result := Files.Count <> 0;
      if Result then
        begin
          if FileExists(FileName) then DeleteFile(FileName);
          ZipFile := TZipFile.Create;
          try
            ZipFile.Open(FileName, zmWrite);
            try
              for Index := 0 to Pred(Files.Count) do
                begin
                  if IsNoPackFileName(Files[Index]) then
                    ZipFile.Add(MapDirectory + Files[Index], Files[Index], zcStored)
                  else
                    ZipFile.Add(MapDirectory + Files[Index], Files[Index]);
                  Canceled := False;
                  if Assigned(OnProgress) then OnProgress(Sender, MapDirectory + Files[Index], Index, Files.Count, Canceled);
                  if Canceled then Break;
                end;
            finally
              ZipFile.Close;
            end;
          finally
            ZipFile.Free;
          end;
        end;
    finally
      Files.Free;
    end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('CreateBackupMapTileFiles(%s) skip error: %s', [QuotedStr(FileName), E.Message]);
        {$ENDIF}
        Result := False;
      end;
  end;
end;

function RestoreBackupMapTileFiles(const FileName: string; OnProgress: TBackupProgressEvent; Sender: TObject): Boolean;
var
  ZipFile: TZipFile;
  Index: Integer;
  TargetFileName: string;
  Canceled: Boolean;
begin
  try
    Result := FileExists(FileName) and TZipFile.IsValid(FileName);
    if Result then
      begin
        ZipFile := TZipFile.Create;
        try
          ZipFile.Open(FileName, zmRead);
          try
            Result := ZipFile.FileCount <> 0;
            if Result then
              for Index := 0 to Pred(ZipFile.FileCount) do
                begin
                  TargetFileName := MapDirectory + ZipFile.FileName[Index];
                  if FileExists(TargetFileName) then
                    begin
                      if FileDateToDateTime(FileAge(TargetFileName)) < FileDateToDateTime(ZipFile.FileInfo[Index].ModifiedDateTime) then
                        begin
                          DeleteFile(TargetFileName);
                          ZipFile.Extract(ZipFile.FileName[Index], MapDirectory);
                          FileSetDate(TargetFileName, ZipFile.FileInfo[Index].ModifiedDateTime);
                        end;
                    end
                  else
                    begin
                      ZipFile.Extract(ZipFile.FileName[Index], MapDirectory);
                      FileSetDate(TargetFileName, ZipFile.FileInfo[Index].ModifiedDateTime);
                    end;
                  Canceled := False;
                  if Assigned(OnProgress) then OnProgress(Sender, MapDirectory + ZipFile.FileName[Index], Index, ZipFile.FileCount, Canceled);
                  if Canceled then Break;
                end;
          finally
            ZipFile.Close;
          end;
        finally
          ZipFile.Free;
        end;
      end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('RestoreBackupMapTileFiles(%s) skip error: %s', [QuotedStr(FileName), E.Message]);
        {$ENDIF}
        Result := False;
      end;
  end;
end;

function DifferenceBackupMapTileFiles(const SourceFileName, TargetFileName: string; OnProgress: TBackupProgressEvent; Sender: TObject): Boolean;
var
  SourceZipFile, TargetZipFile: TZipFile;
  MapFiles: TStrings;
  Index, FileIndex: Integer;
  Canceled: Boolean;
begin
  try
    Result := FileExists(SourceFileName) and TZipFile.IsValid(SourceFileName) and not SameFileName(SourceFileName, TargetFileName);
    if Result then
      begin
        SourceZipFile := TZipFile.Create;
        try
          SourceZipFile.Open(SourceFileName, zmRead);
          try
            TargetZipFile := TZipFile.Create;
            try
              MapFiles := TStringList.Create;
              try
                BuildFilesInDirectory(MapDirectory, MapFiles);
                TargetZipFile.Open(TargetFileName, zmWrite);
                try
                  Canceled := False;
                  for Index := 0 to Pred(MapFiles.Count) do
                    begin
                      FileIndex := SourceZipFile.IndexOf(MapFiles[Index]);
                      if FileIndex <> -1 then
                        begin
                          if IncSecond(FileDateToDateTime(FileAge(MapDirectory + MapFiles[Index])), -2) > FileDateToDateTime(SourceZipFile.FileInfo[FileIndex].ModifiedDateTime) then
                            begin
                              if IsNoPackFileName(MapFiles[Index]) then
                                TargetZipFile.Add(MapDirectory + MapFiles[Index], MapFiles[Index], zcStored)
                              else
                                TargetZipFile.Add(MapDirectory + MapFiles[Index], MapFiles[Index]);
                            end;
                        end
                      else
                        begin
                          if IsNoPackFileName(MapFiles[Index]) then
                            TargetZipFile.Add(MapDirectory + MapFiles[Index], MapFiles[Index], zcStored)
                          else
                            TargetZipFile.Add(MapDirectory + MapFiles[Index], MapFiles[Index]);
                        end;
                      if Assigned(OnProgress) then OnProgress(Sender, MapDirectory + MapFiles[Index], Index, MapFiles.Count, Canceled);
                      if Canceled then Break;
                    end;
                finally
                  TargetZipFile.Close;
                end;
              finally
                MapFiles.Free;
              end;
            finally
              TargetZipFile.Free;
            end;
          finally
            SourceZipFile.Close;
          end;
        finally
          SourceZipFile.Free;
        end;
      end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('DifferenceBackupMapTileFiles(%s, %s) skip error: %s', [QuotedStr(SourceFileName), QuotedStr(TargetFileName), E.Message]);
        {$ENDIF}
        Result := False;
      end;
  end;
end;
*)
{ TMapProperties }

procedure TMapProperties.AssignMap(aMap: TMapProperties; AssignScale: Boolean = true);
begin
    SourceType:= aMap.SourceType;
    Coord:= aMap.Coord;
    Source:= aMap.Source;
    TileDeadInHours:= aMap.TileDeadInHours;

    Enabled:= aMap.Enabled;
    Caption:= aMap.Caption;
    TileDirectory:= aMap.TileDirectory;

    TileSizeX:= aMap.TileSizeX;
    TileSizeY:= aMap.TileSizeY;
    if AssignScale then SetScale(aMap.Scale)
                   else SetScale(Scale);
end;

function TMapProperties.GetCoordName: String;
const
  MapCoordTypeNames: array[TMapCoordType] of PChar = ('', 'ellipse', 'sphere', 'decarte');
begin
  Result:= MapCoordTypeNames[Coord];
end;

function TMapProperties.SetCoordName(Value: String): Boolean;
begin
  Result:= True;
  if SameText(Value, 'ELLIPSE') then Coord:= mctEllipse else
  if SameText(Value, 'SPHERE') then  Coord:= mctSphere else
  if SameText(Value, 'DECARTE') then Coord:= mctDecarte else
    begin
      Coord:= mctDefault;
      Result:= False;
    end;
end;

procedure TMapProperties.SetScale(Value: Integer);
var i, V: Integer;
begin
  V:= Value;
  if V < Low(TMapScale) then  V:= Low(TMapScale);
  if V > High(TMapScale) then V:= High(TMapScale);

  if Not Enabled[V] then
    for i:= Succ(V) to High(TMapScale) do
      if Enabled[i] then
        begin
          V:= i;
          Break;
        end;

  if Not Enabled[V] then
    for i:= Pred(V) downto Low(TMapScale)  do
      if Enabled[i] then
        begin
          V:= i;
          Break;
        end;

  Scale:= V;

  if Coord = mctDecarte then
    begin
      TileSizeX:= Round(MapDecarteScale[Scale] * OriginalX / 100);
      TileSizeY:= Round(MapDecarteScale[Scale] * OriginalY / 100);
      MapSizeX:= TileSizeX;
      MapSizeY:= TileSizeY;
    end
  else
    begin
      MapSizeX:= (1 shl Scale) * TileSizeX;
      MapSizeY:= (1 shl Scale) * TileSizeY;
    end;
end;

function TMapProperties.HighX: Integer;
begin
  if Coord = mctDecarte then Result:= 0
                        else Result:= Pred(MapSizeX div TileSizeX);
end;

function TMapProperties.HighY: Integer;
begin
  if Coord = mctDecarte then Result:= 0
                        else Result:= Pred(MapSizeY div TileSizeY);
end;

procedure TMapProperties.InitializeMapScales(const MinScale, MaxScale: TMapScale);
var
  Index: TMapScale;
begin
  for Index := Low(TMapScalesEnabled) to High(TMapScalesEnabled) do
    begin
      Enabled[Index] := (Index >= MinScale) and (Index <= MaxScale);
      if Coord = mctDecarte then Caption[Index]:= IntToStr(MapDecarteScale[Index])+'%'
                            else Caption[Index]:= Format('1 : _mAp.Scale%u', [Index]);
    end;
end;

function TMapProperties.NormalizeTileX(const TileNumber: Integer): Integer;
var
  Count: Integer;
begin
  Result := TileNumber;
  if Coord = mctDecarte then Exit;

  Count :=  MapSizeX div TileSizeX;
  while Result < 0 do Inc(Result, Count);
  while Result > Pred(Count) do Dec(Result, Count);
end;

function TMapProperties.NormalizeTileY(const TileNumber: Integer): Integer;
var
  Count: Integer;
begin
  Result := TileNumber;
  if Coord = mctDecarte then Exit;

  Count :=  MapSizeY div TileSizeY;
  while Result < 0 do Inc(Result, Count);
  while Result > Pred(Count) do Dec(Result, Count);
end;

procedure TMapProperties.Geo2Pixel(const Geo: TCoordGeo; var Pixel: TCoordPix);
begin
  case Coord of
    mctSphere:  Pixel.SetMercator(Geo.GetMercatorS, MapSizeX, MapSizeY);
    mctEllipse: Pixel.SetMercator(Geo.GetMercatorE, MapSizeX, MapSizeY);
    mctDecarte: begin
                  Pixel.X:= Round( MapSizeX * (1/2 + Geo.X/ OriginalX));
                  Pixel.Y:= Round( MapSizeY * (1/2 - Geo.Y/ OriginalY));
                end;
  end;
end;

procedure TMapProperties.Pixel2Geo(const Pixel: TCoordPix; var Geo: TCoordGeo);
begin
  case Coord of
    mctSphere:  Geo.SetMercatorS(Pixel.GetMercator(MapSizeX, MapSizeY));
    mctEllipse: Geo.SetMercatorE(Pixel.GetMercator(MapSizeX, MapSizeY));
    mctDecarte: begin
                  Geo.X:=   OriginalX * ( Pixel.X / MapSizeX - 1/2);
                  Geo.Y:= - OriginalY * ( Pixel.Y / MapSizeY - 1/2);
                end;
  end;
end;

procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('uMapUtils unit initialization ...');
  {$ENDIF}
  MapDirectory := GetCommonMapDirectory;
//  InitializeMapEnvirovment(cDefaultTemplateMapYandexURL);
  // HACKED size!!!
//  InitializeMapEnvirovment(FMP,'https://static-maps.yandex.ru/1.x/?l=map&ll={longitude},{latitude}&z={zoom}&size=256,340&lang=ru_RU', -1, mctDefault);
  // InitializeMapEnvirovment('https://maps.googleapis.com/maps/api/staticmap?maptype=hybrid&center={latitude},{longitude}&zoom={zoom}&size=256x340&format=PNG&language=ru&region=RU');
//  InitializeMapEnvirovment(cDefaultTemplateMapGoogleURL);
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('uMapUtils unit finalization ...');
  {$ENDIF}
end;

initialization
  Startup;

finalization
  Shutdown;

end.

