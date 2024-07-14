{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}

unit MapFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Contnrs, System.Actions, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.Menus,  Vcl.ActnList, Vcl.StdCtrls, Vcl.Buttons,
  DeMeta, DSMeta, BaseGridFormUnit, DataCacheUnit, uMapUtils, uMapLayers, Vcl.ImgList;

const
  EmptyHint: string = #01;
  PreparedMove: double = 0.75;
  // 0   только центральный тайл
  // 0.5 пол экрана от центральной точки              - минимальное значение по умолчанию
  // 1.0 пол экрана от центральной точки + пол экрана - по пол экрана создается "про запас"
  // 1.5 пол экрана от центральной точки + экрана     - по экрану создается "про запас",
  //                                                    гаратирует наличие картинки при любом сдвиге

type
  TLabelAlign = (laDefault, laRight, laLeft);
  TMapCacheList = class;

  TMapCacheItem = class
  private
    { Private declarations }
    FCaptions: TStringList;
    FOwner: TMapCacheList;
    FChildItems: TMapCacheList;
    FMainItem: TMapCacheItem;
    FCacheItem: TCacheItem;
    FGeo: TCoordGeo;
    FImage: TImage;
    FLabelAlign: TLabelAlign;
    FH, FW: Integer;
    FIDX, FIDY: Integer;
    FLDX, FLDY, FLH, FLW: Integer;
    FMoving: Boolean;
    FMovingX: Integer;
    FMovingY: Integer;
    FImageX: Integer;
    FImageY: Integer;
    FIconSize: Integer;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TMapCacheItem;
    procedure InvalidateFocused;
    procedure RegisterChildCacheItem(CacheItem: TMapCacheItem);
    procedure UnRegisterChildCacheItem(CacheItem: TMapCacheItem);
    function GetHint: string;
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetCaption(const Value: string);
    procedure ReLabelAlign;
    procedure SetLabelAlign(const Value: TLabelAlign);
  public
    { Public declarations }
    constructor Create(AOwner: TMapCacheList; ACacheItem: TCacheItem; const AGeo: TCoordGeo);
    destructor Destroy; override;
    procedure Show(Parent: TWinControl; const X, Y: Integer);
    procedure Hide;
    function IndexOf(const Item: TMapCacheItem): Integer;
    function IsShowChildFocused: Boolean;
    function Select: Integer;
    function UnSelect: Integer;
    property MainItem: TMapCacheItem read FMainItem;
    property CacheItem: TCacheItem read FCacheItem;
    property Geo: TCoordGeo read FGeo write FGeo;
    property Image: TImage read FImage;
    property IconSize: Integer read FIconSize write FIconSize;
    // Вложенные в группу кэши ...
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TMapCacheItem read GetItem; default;
    property LabelAlign: TLabelAlign read FLabelAlign write SetLabelAlign;
    property Hint: string read GetHint;
  end;

  TMapCacheList = class(TObjectList)
  private
    { Private declarations }
    FFocused: TMapCacheItem;
    FOnDblClick: TNotifyEvent;
    FOnMoved: TNotifyEvent;
    PMP: PMapProperties;
    function GetItem(const Index: Integer): TMapCacheItem;
    procedure PutItem(const Index: Integer; const Value: TMapCacheItem);
  public
    { Public declarations }
    procedure ShowControls(Parent: TWinControl; const TileLeftTopCol, TileLeftTopRow, TileRightBottomCol, TileRightBottomRow: Integer);
    procedure DestroyControls;
    property Items[const Index: Integer]: TMapCacheItem read GetItem write PutItem; default;
    property Focused: TMapCacheItem read FFocused;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    property MapProperties: PMapProperties read PMP write PMP;
  end;

  TMapForm = class(TBaseGridForm)
    pnlMap: TPanel;
    imgMap: TImage;
    mi_Dl_Scale: TMenuItem;
    mm_Dl_Scale: TMenuItem;
    PMPosition: TMenuItem;
    act_Da_AutoPosition: TAction;
    act_Da_maphideingroup: TAction;
    actDamaphideingroup1: TMenuItem;
    procedure pnlGridResize(Sender: TObject);
    procedure imgMapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgMapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure WM_MouseWheel(var Message: TMSHMouseWheel); message WM_MOUSEWHEEL;
    procedure act_Da_AutoPositionExecute(Sender: TObject);
    procedure imgMapDblClick(Sender: TObject);
    procedure act_Da_maphideingroupExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FThreads: TThreadList;
    FMapCacheList: TMapCacheList;
    FMapLayers: TMapLayers;
    FMapScaleActions: array[TMapScale] of TAction;
    FMapTransparent: Byte;
    FTileLeft, FTileRight, FTileTop, FTileBottom: Integer;
    FMapCenter: TCoordGeo;
    FMoving: Boolean;
    FHoldUpdate: Boolean;
    FMovingX: Integer;
    FMovingY: Integer;
    FImageX: Integer;
    FImageY: Integer;
    FMaxExecutingThreadCount: Integer;
    FExecutingThreadCount: Integer;
    FMP: TMapProperties;
    procedure SetMapScale(const Value: TMapScale);
    procedure UpdateTilesBitmap(const FullRecreated: Boolean);
    procedure SetMapPosition(nX, nY: Integer);
    procedure TileDraw(Sender: TObject; const Col, Row: Integer; const Scale: TMapScale; const Rect: TRect; Canvas: TCanvas);
    procedure TileLoad(Sender: TObject; const Col, Row, Priority: Integer; const Scale: TMapScale; const BackgroundColor: TColor; var FileName: string);
    procedure MapScaleUpdate(Sender: TObject);
    procedure MapScaleExecute(Sender: TObject);
    procedure BuildMapScaleActions;
    procedure BuildMapScaleMenu(RootItem: TMenuItem);
    procedure ClearThreads;
    procedure NewThread(const Col, Row, Priority: Integer; const MP: TMapProperties; const BackgroundColor: TColor; const Running: Boolean = False);
    procedure DoneThread(Sender: TObject);
    procedure RunThreads;
    procedure BuildLabels;
    procedure MapSelectUpdate(Sender: TObject);
    procedure MapSelectExecute(Sender: TObject);
    function FindMapLayerAction(const LayerID: Integer; const AutoCreated: Boolean = True): TAction;
    procedure BuildMapLayerMenu;
    procedure BuildMapLayerMainMenu;
    procedure ImageDblClick(Sender: TObject);
    procedure ImageMoved(Sender: TObject);
    procedure SetMapTransparent(const Value: Byte);
    function GetMapScale: TMapScale;
  protected
    { Protected declarations }
    procedure InitForm; override;
    procedure AfterInit; override;
    procedure SetDataSetMeta(const Value: TDataSetMeta); override;
    procedure CacheFocusedChange(Item: TCacheItem); override;
    procedure CacheSelectedChange(Item: TCacheItem); override;
    procedure CacheItemInsert(Item : TCacheItem);override;
    procedure NotifyRefreshTimer(Sender: TObject); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetPropertiesString: string; override;
    procedure DeInitForm; override;
    procedure ReCreateView; override;
    procedure ChangeDataSetStatus; override;
    procedure DoUpdate(Sender: TObject); override;
 //   property MapScale: TMapScale read GetMapScale write SetMapScale;
    property MapTransparent: Byte read FMapTransparent write SetMapTransparent;
  end;

var
   FMapLabelBackGround: Boolean;

implementation

uses Math, StrUtils, Types, DB, DeTypes, DeSettings, DeLog, Dictionary,
     Funcs, DataUnit, Security, DeMetadata, Main;

{$R *.dfm}

{ TMapCacheItem }

constructor TMapCacheItem.Create(AOwner: TMapCacheList; ACacheItem: TCacheItem; const AGeo: TCoordGeo);
begin
  FOwner := AOwner;
  FCacheItem := ACacheItem;
  FGeo:= AGeo;
  FIconSize:= 24;
  FLabelAlign:= laRight;

  FImage := TImage.Create(nil);
  FImage.Transparent:= True;
  FImage.AutoSize := True;
  FImage.Tag := NativeInt(Self);
  FImage.OnMouseDown := ImageMouseDown;
  FImage.OnMouseMove := ImageMouseMove;
  FImage.OnMouseUp := ImageMouseUp;
  FImage.Hint := EmptyHint;
  if Assigned(FOwner) then FImage.OnDblClick := FOwner.OnDblClick;
  FCaptions:= TStringList.Create;
  SetCaption( ACacheItem.Caption );
end;

destructor TMapCacheItem.Destroy;
begin
  FreeAndNil(FChildItems);
  FCaptions.Free;
  if Assigned(FImage) then FImage.Tag := 0;
  if Assigned(FOwner) and (FOwner.FFocused = Self) then
    FOwner.FFocused := nil;
  inherited Destroy;
end;

function TMapCacheItem.GetCount: Integer;
begin
  if Assigned(FChildItems) then
    Result := FChildItems.Count
  else
    Result := 0;
end;

function TMapCacheItem.GetItem(const Index: Integer): TMapCacheItem;
begin
  if Assigned(FChildItems) then
    Result := FChildItems[Index]
  else
    Result := nil;
end;

function TMapCacheItem.GetHint: string;
var
  Index: Integer;
begin
  Result := EmptyStr;

  if Count = 0 then
    begin
      if Assigned(FCacheItem) then Result:= FCacheItem.Hint;
    end
  else
    begin
      if Assigned(FCacheItem) then Result:= FCacheItem.Caption;
      for Index := 0 to Pred(Count) do
        begin
          if Assigned(Items[Index].CacheItem) then StrAdd(Result, #10, Items[Index].CacheItem.Caption);
          if Index = 11 then
            begin
              Result := Result + #10 + '...';
              Break;
            end;
        end;
      Result:= Format( GetTitle('_dV.selectedcount'), [Succ(Count)]) + #10#10 + Result;
    end;
end;

procedure TMapCacheItem.Show(Parent: TWinControl; const X, Y: Integer);
var
  i: Integer;
  Icon: TIcon;
  Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse: Byte;
begin
  Assert(Assigned(Parent), 'Parent is nil!');
  FImage.Parent := Parent;

  Icon := TIcon.Create;
  try
    if Assigned(FCacheItem) then
      begin
        if Count = 0 then
          begin
            DM.ilIcon16.GetIcon(DM.MapIconIndex(FCacheItem.ImageIndex), Icon)
          end
        else
          begin
            DecodeIcon(FCacheItem.ImageIndex, Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse);
            DM.ilIcon24.GetIcon(DM.MapIconIndex(
                              EncodeIcon(Icon1, Color1, IsSymbol1, Icon1, Color1, IsSymbol1, 14, Inverse)), Icon);
          end;

        if FCacheItem.Focused then
          begin
            if Assigned(FOwner) then FOwner.FFocused := Self;
          end
        else
          begin
            if Assigned(FOwner) and (FOwner.FFocused = Self) then
              FOwner.FFocused := nil;
          end;
      end;

      ReLabelAlign;
      FImage.Picture.Graphic.SetSize(FW, FH);

      if FCacheItem.Focused then
        begin
          FImage.Transparent:= False;
          FImage.Canvas.Brush.Style:= bsSolid;
          FImage.Canvas.Brush.Color:= clWhite;
          FImage.Canvas.Pen.Color:= clMenuHighlight;
          FImage.Canvas.Rectangle(0, 0, FW, FH);
          FImage.BringToFront;
        end
      else
        begin
          if FMapLabelBackGround then
            begin
              FImage.Transparent:= False;
              FImage.Canvas.Brush.Style:= bsSolid;
              FImage.Canvas.Brush.Color:= clWhite;
              FImage.Canvas.Pen.Color:= clSilver;
              FImage.Canvas.Rectangle(0, 0, FW, FH);
            end
          else
            begin
              FImage.Transparent:= True;
              FImage.Canvas.Brush.Style:= bsClear;
              FImage.Canvas.Brush.Color:= clWhite;
              FImage.Canvas.Pen.Color:= clWhite;
              FImage.Canvas.Rectangle(0, 0, FW, FH);
            end;
        end;

    FImage.Canvas.Draw(FIDX + (FIconSize - Icon.Width) div 2, FIDY + (FIconSize - Icon.Height) div 2, Icon);
    for i := 0 to Pred(FCaptions.Count) do
      FImage.Canvas.TextOut(FLDX, FLDY + i*(FLH div FCaptions.Count), FCaptions[i]);

    FImage.SetBounds( X - FIconSize div 2 - FIDX, Y - FIconSize div 2 - FIDY, FW, FH);
    FImage.Visible := True;
  finally
    Icon.Free;
  end;
end;

procedure TMapCacheItem.Hide;
begin
  if Assigned(FImage) then
    begin
      FImage.Visible := False;
      if Assigned(FOwner) and (FOwner.FFocused = Self) then
        FOwner.FFocused := nil;
    end;
end;

function TMapCacheItem.IndexOf(const Item: TMapCacheItem): Integer;
begin
  if Assigned(FChildItems) then
    Result := FChildItems.IndexOf(Item)
  else
    Result := -1;
end;

function TMapCacheItem.IsShowChildFocused: Boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to Pred(Count) do
    if Assigned(Items[Index].CacheItem) and Items[Index].CacheItem.Focused then
      begin
        Result := True;
        Break;
      end;
end;

procedure TMapCacheItem.InvalidateFocused;
var
  X, Y: Integer;
  P: TWinControl;
begin
  if Assigned(FImage) and FImage.Visible then
    if Assigned(FImage.Parent) then
      begin
        X:= FImage.Left + FIDX + (IconSize div 2);
        Y:= FImage.Top + FIDY + (IconSize div 2);
        P:= FImage.Parent;
        Hide;
        Show(P, X, Y);
      end
    else // перенаправляем на группу (родителя)
      begin
        Assert(assigned(FMainItem), 'TMapCacheItem: FImage.Parent and FMainItem are nil!');
        FMainItem.InvalidateFocused
      end
end;

procedure TMapCacheItem.RegisterChildCacheItem(CacheItem: TMapCacheItem);
begin
  if not Assigned(FChildItems) then
    begin
      FChildItems := TMapCacheList.Create(False);
      FChildItems.PMP := FOwner.PMP;
    end;
  if FChildItems.Add(CacheItem) <> -1 then
    if Assigned(CacheItem) then
      CacheItem.FMainItem := Self;

  SetCaption( Format(GetTitle('_dV.MapSelectedCount'), [Succ(Count)]) );
  ReLabelAlign;
end;

procedure TMapCacheItem.UnRegisterChildCacheItem(CacheItem: TMapCacheItem);
begin
  if Assigned(FChildItems) then
    begin
      FChildItems.Remove(CacheItem);
      if FChildItems.Count = 0 then FreeAndNil(FChildItems);
    end;
  if Assigned(CacheItem) then CacheItem.FMainItem := nil;

  if Count = 0 then SetCaption( FCacheItem.Caption )
               else SetCaption( Format(GetTitle('_dV.MapSelectedCount'), [Succ(Count)]));
  ReLabelAlign;
end;

function TMapCacheItem.Select: Integer;
var
  Index: Integer;
begin
  Result := 0;
  if Assigned(FCacheItem) then
    begin
      FCacheItem.Selected := True;
      Inc(Result);
    end;
  if Assigned(FChildItems) then
    for Index := 0 to Pred(FChildItems.Count) do
      if Assigned(FChildItems[Index].CacheItem) then
        begin
          FChildItems[Index].CacheItem.Selected := True;
          Inc(Result);
        end;
end;

procedure TMapCacheItem.ReLabelAlign;
var i: Integer;
begin
  try
    if (FLW = -1) and assigned(FImage) then
      for i:=0 to Pred(FCaptions.Count) do
        FLW:= Max(FLW, FImage.Canvas.TextWidth(FCaptions[i]));
    if (FLH = -1) and assigned(FImage) then
      FLH:= FCaptions.Count * FImage.Canvas.TextHeight('Yy');
  except
    FLW:= 100;
    FLH:= 22;
  end;

  case FLabelAlign of
    laRight: begin
              FW:= 3 + FIconSize + 6 + FLW + 3;
              FH:= 2 + max(FIconSize, FLH) + 2;
              FIDX:= 3;
              FIDY:= 2;
              FLDX:= 3 + FIconSize + 6;
              FLDY:= (FH - FLH) div 2;
            end;
    laLeft: begin
              FW:= 3 + FLW + 6 + FIconSize + 3;
              FH:= 2 + max(FIconSize, FLH) + 2;
              FIDX:= FW - FIconSize - 3;
              FIDY:= 2;
              FLDX:= 3 + 1;
              FLDY:= (FH - FLH) div 2;
            end;
  end;
end;

procedure TMapCacheItem.SetCaption(const Value: string);
var i: Integer;
    s: String;
begin
  FCaptions.Clear;
  s:= EmptyStr;
  for i:=1 to Length(Value) do
    if (Value[i]<>#10) and (Value[i]<>#13)
      then s:=s + Value[i]
      else if 0<Length(s)
             then begin
                    FCaptions.Add(s);
                    s:= EmptyStr;
                  end;

  if 0<Length(s) then FCaptions.Add(s);

  FLW:= -1;
  FLH:= -1;
end;

procedure TMapCacheItem.SetLabelAlign(const Value: TLabelAlign);
begin
  if Value = laDefault then
    begin
     FLabelAlign:= laRight
    end
  else
    begin
      FLabelAlign:= Value;
      ReLabelAlign;
    end;
end;

function TMapCacheItem.UnSelect: Integer;
var
  Index: Integer;
begin
  Result := 0;
  if Assigned(FCacheItem) then
    begin
      FCacheItem.Selected := False;
      Inc(Result);
    end;
  if Assigned(FChildItems) then
    for Index := 0 to Pred(FChildItems.Count) do
      if Assigned(FChildItems[Index].CacheItem) then
        begin
          FChildItems[Index].CacheItem.Selected := False;
          Inc(Result);
        end;
end;

procedure TMapCacheItem.ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(Sender) and (Sender is TImage) and Assigned(FOwner) and  Assigned(TMapCacheItem((Sender as TImage).Tag).CacheItem) then
    if TMapCacheItem((Sender as TImage).Tag).CacheItem.Focused and Assigned(FOwner.OnMoved) then
      begin
        FMoving := True;
        FMovingX := X;
        FMovingY := Y;
        FImageX := (Sender as TImage).Left;
        FImageY := (Sender as TImage).Top;
      end
    else
      begin
        if Assigned(FOwner) and Assigned(FOwner.FFocused) then FOwner.FFocused.UnSelect;
        FCacheItem.SetFocus;
        if Assigned(FOwner) and Assigned(FOwner.FFocused) then FOwner.FFocused.InvalidateFocused;
        InvalidateFocused;
        Select;
      end;
end;

procedure TMapCacheItem.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Not Assigned(Sender) then Exit;

  if (Sender is TImage) then
    with Sender as TImage do
      begin
        if FMoving then
          SetBounds(Left - (FMovingX - X), Top - (FMovingY - Y), Width, Height)
        else if TGraphicControl(Sender).Hint = EmptyHint then
               TGraphicControl(Sender).Hint:= TMapCacheItem(TGraphicControl(Sender).Tag).Hint;
      end;
end;

procedure TMapCacheItem.ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Pixel: TCoordPix;
  MCI: TMapCacheItem;
  Geo: TCoordGeo;
begin
  if Assigned(Sender) and (Sender is TImage) and FMoving then
    begin
      FMoving := False;
      MCI:= TMapCacheItem((Sender as TImage).Tag);
      MCI.FOwner.PMP.Geo2Pixel(MCI.Geo, Pixel);
      Pixel.X := Pixel.X + ((Sender as TImage).Left - FImageX);
      Pixel.Y := Pixel.Y + ((Sender as TImage).Top  - FImageY);
      MCI.FOwner.PMP.Pixel2Geo(Pixel, Geo);
      MCI.Geo:= Geo;
      FOwner.OnMoved(Sender);
    end;
  inherited;
end;

{ TMapCacheList }

function TMapCacheList.GetItem(const Index: Integer): TMapCacheItem;
begin
  Result := inherited Items[Index] as TMapCacheItem;
end;

procedure TMapCacheList.PutItem(const Index: Integer; const Value: TMapCacheItem);
begin
  inherited Items[Index] := Value;
end;

procedure TMapCacheList.ShowControls(Parent: TWinControl; const TileLeftTopCol, TileLeftTopRow, TileRightBottomCol, TileRightBottomRow: Integer);
const
  cMaxControlCount = 1000;
var
  Index, GroupCount, DrawCount: Integer;
  Pixel: TCoordPix;
  TileX, TileY: Integer;
  DrawList: TMapCacheList;
begin
  DrawList := TMapCacheList.Create(False);
  DrawList.PMP:= PMP;
  try
    GroupCount := 0;
    // Получим счисок всех объектов, входящих в видимую зону экрана ...
    for Index := 0 to Pred(Count) do
      begin
        PMP.Geo2Pixel(Items[Index].FGeo, Pixel);
        TileX := Pixel.X div PMP.TileSizeX;
        TileY := Pixel.Y div PMP.TileSizeY;
        if (TileLeftTopCol <= TileX) and (TileX <= TileRightBottomCol) and
           (TileLeftTopRow <= TileY) and (TileY <= TileRightBottomRow) then
          if Items[Index].Count = 0 then
            DrawList.Add(Items[Index])
          else
            begin
              DrawList.Insert(GroupCount, Items[Index]);
              Inc(GroupCount);
            end
        else
          Items[Index].Hide;
      end;
    DrawCount := 0;

    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, DrawList.Count + GroupCount, 1);

    // Отрисуем все сгруппированные объекты ...
    for Index := 0 to Pred(GroupCount) do
      begin
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Index, 0);
        if DrawCount < cMaxControlCount then
          begin
            PMP.Geo2Pixel(DrawList[Index].FGeo, Pixel);
            Pixel.X := Pixel.X - (TileLeftTopCol * PMP.TileSizeX);
            Pixel.Y := Pixel.Y - (TileLeftTopRow * PMP.TileSizeY);
            // Саму группу ...
            DrawList[Index].Show(Parent, Pixel.X, Pixel.Y);
            Inc(DrawCount);
            // И вложенные объекты в случае фокусировки...
            {
            if (DrawList[Index] = Focused) or DrawList[Index].IsShowChildFocused then
              for ChildIndex := 0 to Pred(DrawList[Index].Count) do
                if DrawCount < cMaxControlCount then
                  begin
                    DrawList[Index][ChildIndex].Show(Parent, Pixel.X + (DM.ilIcon16.Width div 2), Pixel.Y + Succ(DM.ilIcon24.Height) + (Succ(DM.ilIcon16.Height) * ChildIndex));
                    Inc(DrawCount);
                  end
                else
                  DrawList[Index][ChildIndex].Hide
            else
              for ChildIndex := 0 to Pred(DrawList[Index].Count) do
                DrawList[Index][ChildIndex].Hide;
            }
          end
        else
          DrawList[Index].Hide;
      end;
    // Отрисуем объекты без групп ...


    for Index := GroupCount to Pred(DrawList.Count) do
      begin
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, GroupCount + Index, 0);
        if not Assigned(DrawList[Index].MainItem) then
          if DrawCount < cMaxControlCount then
            begin
              PMP.Geo2Pixel(DrawList[Index].FGeo, Pixel);
              DrawList[Index].Show(Parent, Pixel.X - (TileLeftTopCol * PMP.TileSizeX), Pixel.Y - (TileLeftTopRow * PMP.TileSizeY));
              Inc(DrawCount);
            end
          else
            DrawList[Index].Hide;
      end;

    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);

  finally
    DrawList.Free;
  end;
end;

procedure TMapCacheList.DestroyControls;
var
  Index: Integer;
begin
  for Index := 0 to Pred(Count) do
    FreeAndNil(Items[Index].FImage);
  FFocused := nil;
end;

{ TMapForm }

constructor TMapForm.Create(AOwner: TComponent);
var
  SystemInfo: TSystemInfo;
begin
  inherited Create(AOwner);
  FMainControl := pnlGrid;
  FThreads := TThreadList.Create;
  FMapCacheList := TMapCacheList.Create;
  FMapCacheList.PMP:= @FMP;
  FMapLayers := TMapLayers.Create;
  FMapTransparent := 255;
  FTileLeft := -1;
  FTileRight := -1;
  FTileTop := -1;
  FTileBottom := -1;
  FMaxExecutingThreadCount := 4; // Для терминального режима максимум 4 нитки!!!
  FMapCacheList.OnDblClick := ImageDblClick;
  // Если не териминальный режим, то ...
  if GetSystemMetrics(SM_REMOTESESSION) = 0 then
    begin
      {$IFDEF WIN32}
      GetNativeSystemInfo(SystemInfo);
      {$ELSE}
      GetSystemInfo(SystemInfo);
      {$ENDIF}
      // Если количество процессоров больше одного, то берем по их количеству количество ниток!
      if Min(16, 4*SystemInfo.dwNumberOfProcessors) > FMaxExecutingThreadCount then
        FMaxExecutingThreadCount := Min(16, 4*SystemInfo.dwNumberOfProcessors);
    end;
end;

destructor TMapForm.Destroy;
begin
  ClearThreads;
  FreeAndNil(FThreads);
  FreeAndNil(FMapLayers);
  FreeAndNil(FMapCacheList);
  inherited Destroy;
end;

procedure TMapForm.SetDataSetMeta(const Value: TDataSetMeta);
var
  Index: Integer;
  Field: TFieldMeta;
begin
  if Assigned(Value) and Assigned(Value.Cache) and not Value.Cache.Active then
    if Assigned(Value.Table) then
      begin
        if Assigned(Value.Cache.Fields) and (Value.Cache.Fields.Count <> 0) then
          for Index := 0 to Pred(Value.Cache.Fields.Count) do
            begin
              Field := Value.Cache.Fields[Index];
              if Assigned(Field) then
                if Field.Key then
                  Field.Stage := fsKey
                // + Для карты это ключевые поля!!!
                else if Assigned(Value.Table.LatitudeField) and (Value.Table.LatitudeField.ID = Field.ID) then
                  Field.Stage := fsKey
                else if Assigned(Value.Table.LongitudeField) and (Value.Table.LongitudeField.ID = Field.ID) then
                  Field.Stage := fsKey
                // - Для карты это ключевые поля!!!
                else if Assigned(Value.Table.OField) and (Value.Table.OField.ID = Field.ID) then
                  Field.Stage := fsBase
                else if Assigned(Value.Table.GField) and (Value.Table.GField.ID = Field.ID) then
                  Field.Stage := fsBase
                else if Assigned(Value.Table.NField) and (Value.Table.NField.ID = Field.ID) then
                  Field.Stage := fsBase
                else if Assigned(Value.Table.PField) and (Value.Table.PField.ID = Field.ID) then
                  Field.Stage := fsBase
                else if Assigned(Value.Table.DField) and (Value.Table.DField.ID = Field.ID) then
                  Field.Stage := fsBase
                else if Assigned(Value.Table.IconField) and (Value.Table.IconField.ID = Field.ID) then
                  Field.Stage := fsBase
                else if Assigned(Value.Table.ColorField) and (Value.Table.ColorField.ID = Field.ID) then
                  Field.Stage := fsBase
                else if Field.DataType in BinaryTypes then
                  Field.Stage := fsBlob
                else if Field.DataType in [ftMemo, ftWideMemo] then
                  Field.Stage := fsBlob
                else if Assigned(Value.Cache.SortList) and (Value.Cache.SortList.IndexByID(Field.ID) <> -1) then
                  Field.Stage := fsBase
                else
                  Field.Stage := fsFull;
            end;
      end;
  inherited SetDataSetMeta(Value);
end;

function TMapForm.GetMapScale: TMapScale;
begin
  Result:= FMP.Scale;
end;

procedure TMapForm.SetMapScale(const Value: TMapScale);
begin
  FMP.SetScale(Value);
end;

procedure TMapForm.SetMapTransparent(const Value: Byte);
begin
  FMapTransparent := Value;
end;

procedure TMapForm.FormShow(Sender: TObject);
begin
  inherited;
  if FHoldUpdate then UpdateTilesBitmap(True);
end;

procedure TMapForm.UpdateTilesBitmap(const FullRecreated: Boolean);
var
  TL, TR, TT, TB, BackUpValue: Integer;
  Bitmap: TBitmap;
  P: TCoordPix;
begin
  if Not Visible then
    begin
      FHoldUpdate:= True;
      Exit;
    end;

  // Получаем центральную точку ...
  FMP.Geo2Pixel(FMapCenter, P);

  // Находим крайние тайлы ...

  if FMP.Coord = mctDecarte then
    begin
      TL := 0;
      TR := 0;
      TT := 0;
      TB := 0;
    end
  else
    begin
      TL :=     (P.X - Round(pnlGrid.ClientWidth * PreparedMove))  div FMP.TileSizeX;
      TR :=     (P.X + Round(pnlGrid.ClientWidth * PreparedMove))  div FMP.TileSizeX;
      TT := Max((P.Y - Round(pnlGrid.ClientHeight * PreparedMove)) div FMP.TileSizeY, 0);
      TB := Min((P.Y + Round(pnlGrid.ClientHeight * PreparedMove)) div FMP.TileSizeY, FMP.HighY);
    end;

  if FHoldUpdate or FullRecreated or (FMP.Coord = mctDecarte) or
     (TL<>FTileLeft) or (TR<>FTileRight) or (TT<>FTileTop) or (TB<>FTileBottom) then
    try
      FTileLeft:= TL;
      FTileRight:= TR;
      FTileTop:= TT;
      FTileBottom:= TB;

      BackUpValue:= FMaxExecutingThreadCount;
      FMaxExecutingThreadCount:= 1;
      Bitmap := CreateTilesBitmap(Self, FMP, TL, TT, TR, TB, MapTransparent, nil{TileDraw}, TileLoad, clWindow);
      FMaxExecutingThreadCount:= BackUpValue;

      try
        imgMap.Picture.Assign(Bitmap);
      finally
        Bitmap.Free;
      end;

      SetMapPosition( (pnlGrid.ClientWidth div 2) + (FTileLeft * FMP.TileSizeX - P.X),
                      (pnlGrid.ClientHeight div 2) + (FTileTop * FMP.TileSizeY - P.Y) );

      BuildLabels;
      FMapCacheList.ShowControls(pnlMap, FTileLeft, FTileTop, FTileRight, FTileBottom);
    finally
      RunThreads;
    end
  else
    begin
      SetMapPosition( (pnlGrid.ClientWidth div 2) + (FTileLeft * FMP.TileSizeX - P.X),
                      (pnlGrid.ClientHeight div 2) + (FTileTop * FMP.TileSizeY - P.Y) );
    end;

  FHoldUpdate:= False;
end;

procedure TMapForm.SetMapPosition(nX, nY: Integer);
begin
  // Контроль только для "конечных" по Х координате
  if (FMP.Coord in [mctDecarte]) then
    if (pnlGrid.ClientWidth  > FMP.MapSizeX)
      then begin
             nX:= (pnlGrid.ClientWidth  - FMP.MapSizeX) div 2;
             FMapCenter.X:= 0;
           end
      else if nX > FTileLeft * FMP.TileSizeX
             then nX:= FTileLeft * FMP.TileSizeX
             else if nX < pnlGrid.ClientWidth - imgMap.Picture.Width - (FMP.HighX - FTileRight) * FMP.TileSizeX
                    then nX:= pnlGrid.ClientWidth - imgMap.Picture.Width - (FMP.HighX - FTileRight) * FMP.TileSizeX;

  // Контроль для всех по Y координате
  if pnlGrid.ClientHeight > FMP.MapSizeY
    then begin
           nY:= (pnlGrid.ClientHeight - FMP.MapSizeY) div 2;
           FMapCenter.Y:= 0;
         end
    else if nY > FTileTop * FMP.TileSizeY
           then nY:= FTileTop * FMP.TileSizeY
           else if nY < pnlGrid.ClientHeight - imgMap.Picture.Height - (FMP.HighY - FTileBottom) * FMP.TileSizeY
                  then nY:= pnlGrid.ClientHeight - imgMap.Picture.Height - (FMP.HighY - FTileBottom) * FMP.TileSizeY;

  pnlMap.SetBounds( nX, nY, imgMap.Picture.Width, imgMap.Picture.Height);
end;

procedure TMapForm.TileDraw(Sender: TObject; const Col, Row: Integer; const Scale: TMapScale; const Rect: TRect; Canvas: TCanvas);
begin
  // Этот обработчик события используется для рисования поверх тайла каких-то объектов!!!
end;

procedure TMapForm.TileLoad(Sender: TObject; const Col, Row, Priority: Integer; const Scale: TMapScale; const BackgroundColor: TColor; var FileName: string);
begin
  FileName := EmptyStr; // Нитка сама отрисует!!!
  NewThread(Col, Row, Priority, FMP, BackgroundColor);
end;

procedure TMapForm.pnlGridResize(Sender: TObject);
begin
  inherited;
  UpdateTilesBitmap(False);
end;

procedure TMapForm.imgMapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    begin
      FMoving := True;
      FMovingX := X;
      FMovingY := Y;
      FImageX:= pnlMap.Left;
      FImageY:= pnlMap.Top;
      pnlGrid.SetFocus;
    end;
end;

procedure TMapForm.imgMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var s: String;
    Geo: TCoordGeo;
    Pixel: TCoordPix;
begin
  if FMoving then
    begin
      SetMapPosition( pnlMap.Left + (X - FMovingX), pnlMap.Top + (Y - FMovingY) );
    end
  else
    begin
      FMP.Geo2Pixel(FMapCenter, Pixel);
      Pixel.X := Pixel.X + (X + pnlMap.Left - pnlGrid.ClientWidth div 2);
      Pixel.Y := Pixel.Y + (Y + pnlMap.Top  - pnlGrid.ClientHeight div 2);
      FMP.Pixel2Geo(Pixel, Geo);

      s:= FMP.Caption[FMP.Scale];
      if Sender = imgMap then
        if FMP.Coord = mctDecarte then
          begin
            s:= s + '   ' + 'X: ' + IntToStr(Round(Geo.X)) + '  ' + 'Y: ' + IntToStr(Round(Geo.Y))
          end
        else
          begin
            while Pi  < Geo.X do Geo.X:= Geo.X - 2 * Pi;
            while Geo.X < -Pi do Geo.X:= Geo.X + 2 * Pi;

            if Geo.GetPeopleX<0 then s:= s + '   ' + Format(GetTitle('_mAp.longitude.west'),    [-Geo.GetPeopleX])
                                else s:= s + '   ' + Format(GetTitle('_mAp.longitude.eastern'), [ Geo.GetPeopleX]);
            if Geo.GetPeopleY<0 then s:= s + '   ' + Format(GetTitle('_mAp.latitude.south'),    [-Geo.GetPeopleY])
                                else s:= s + '   ' + Format(GetTitle('_mAp.latitude.north'),    [ Geo.GetPeopleY]);
          end;
      SendMessage(Application.MainForm.Handle, DM_STATUSNOTIFY, NativeInt(s), Handle);
    end;
end;

procedure TMapForm.imgMapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Pixel: TCoordPix;
begin
  if FMoving and (Button = mbLeft) then
    begin
      FMP.Geo2Pixel(FMapCenter, Pixel);
      Pixel.X := Pixel.X + (FImageX - pnlMap.Left);
      Pixel.Y := Pixel.Y + (FImageY - pnlMap.Top);
      FMP.Pixel2Geo(Pixel, FMapCenter);
      UpdateTilesBitmap(False);
    end;
  FMoving := False;
end;

procedure TMapForm.InitForm;
var
  vLat, vLon: Variant;
  i, N, UserID, DatasetID: Integer;
  s, MapName: string;
  MapTable: TTableMeta;
  MapField: TFieldMeta;
  GUID: TGUID;
begin
  inherited InitForm;
  {$IFDEF DEBUG}
  DebugLog('%s.InitForm begin ...', [ClassName]);
  ViewParams.DebugVariablesLog(ClassName + '.InitForm:');
  {$ENDIF}
  MapName := ViewParams.GetValueByName('Map');
  MapTable := nil;
  MapField := nil;
  // Формат имени слоя:
  // {DatasetGUID}.{FieldGUID}
  // {DatasetGUID}.{FieldName}
  // {DatasetName}.{FieldGUID}
  // {DatasetName}.{FieldName}
  // {DatasetGUID}.
  // {DatasetName}.
  // .{FieldGUID}
  // {Имя обычного слоя}
  // Примечание: По возможности указывать не имя таблицы, а ИМЕНННО GUID,
  // т.к. таблицы могут наследоваться (иметь одинаковые имена) и быть в разных БД!
  i := Pos('.', MapName);
  if i <> 0 then
    begin
      s := Trim(Copy(MapName, 1, Pred(i)));
      if Length(s) <> 0 then
        if TryStringToGUID(s, GUID) then
          MapTable := MetaData.GetTableMeta(GUID)
        else
          MapTable := MetaData.GetTableByName(s);
      s := Trim(Copy(MapName, Succ(i), Length(MapName)));
      if Length(s) <> 0 then
        if TryStringToGUID(s, GUID) then
          if Assigned(MapTable) then
            MapField := MapTable.Fields.FindByGUID(GUID)
          else
            MapField := MetaData.TablesList.FindFieldByGUID(GUID)
        else
          if Assigned(MapTable) then
            MapField := MapTable.Fields.FindByName(s);
    end;

  if Variables.AsBoolean[RegMapCacheEnabled] then
    MapDirectory := Variables.AsString[RegMapBaseDirectory]
  else
    begin
      MapDirectory := IncludeTrailingBackslash(LogDirectory + 'MAP');
      DM.RegisterTemporaryFile(MapDirectory);
    end;
  if Assigned(UserSession) then UserID:= UserSession.ID
                           else UserID:= 0;
  if Assigned(DataSetMeta) and Assigned(DataSetMeta.Table) then DatasetID:= DataSetMeta.Table.ID
                                                           else DatasetID:= 0;
  FMapLayers.LoadFromDatabase(UserID, DatasetID);

  if (Assigned(MapTable) or Assigned(MapField)) and
      FMapLayers.LoadLayer(ViewParams.GetValueByName('Key'), MapTable, MapField, FMP)
    then
      begin
        N:= StrToIntDef(ViewParams.GetValueByName('Zoom'), -1);
        if N=-1 then
          begin
            N:=0;
            for i:= Succ(Low(MapDecarteScale)) to High(MapDecarteScale) do
              if (FMP.OriginalX * MapDecarteScale[i] / 100 < pnlGrid.ClientWidth) and
                 (FMP.OriginalY * MapDecarteScale[i] / 100 < pnlGrid.ClientHeight)
               then N:= i
               else Break;
          end;

        FMP.SetScale( N );  // Масштаб: Картинка в размер экрана
        FMapCenter.X:= 0;
        FMapCenter.Y:= 0;
      end
    else
      begin
        if FMapLayers.Count = 0 then
          begin
            FMapLayers.CreateDefaultLayer;
            FMapLayers.SelectLayer(0);
          end
        else
          begin
            N:= FMapLayers.IndexByName(MapName);
            if N=-1 then FMapLayers.SelectLayer(0)
                    else FMapLayers.SelectLayer(N);
          end;
        FMP:= FMapLayers.Selected.MP;

        vLon := StrToFloatDef(StringReplace(StringReplace(ViewParams.GetValueByName('Longitude'),
                                                          ',', FormatSettings.DecimalSeparator, []),
                                                          '.', FormatSettings.DecimalSeparator, []), 83); // Центр России
        vLat := StrToFloatDef(StringReplace(StringReplace(ViewParams.GetValueByName('Latitude'),
                                                          ',', FormatSettings.DecimalSeparator, []),
                                                          '.', FormatSettings.DecimalSeparator, []), 60); // Центр России
        FMapCenter.SetPeople(vLon, vLat);
        FMP.SetScale( StrToIntDef(ViewParams.GetValueByName('Zoom'), 4));  // Масштаб: 4 ~ Россия
      end;

  if -1 < ViewParams.IndexByName('LabelBackGround') then
    FMapLabelBackGround := (0 <> StrToIntDef(ViewParams.GetValueByName('LabelBackGround'), 0));

  MapTransparent := Variables.AsInteger[RegMapBrightness];
  if -1 < ViewParams.IndexByName('Transparent') then
    MapTransparent := Max(0, Min(255, StrToIntDef(ViewParams.GetValueByName('Transparent'), MapTransparent)));

  BuildMapScaleActions;
  BuildMapScaleMenu(mm_Dl_Scale);           // Основное меню ...
  BuildMapScaleMenu(mi_Dl_Scale);           // Всплывающее меню ...
  BuildMapScaleMenu(StatusPopupMenu.Items); // Меню в строке состояния ...
  BuildMapLayerMenu;     // Строим всплывающее меню ...
  BuildMapLayerMainMenu; // Строим основное меню на основе всплывающего ...
//  InitializeMapEnvirovment( FMP, EmptyStr);       // Инициализируем пустое полотно
  UpdateTilesBitmap(True);

  Item_Da_Orderby.Enabled:=False;
  MM_Da_OrderBy.Enabled:=False;
  {$IFDEF DEBUG}
  DebugLog('%s.InitForm variables:'#13#10#9#9#9'MapScale=%d'#13#10#9#9#9'MapDirectory=%s'#13#10#9#9#9'TileDirectory=%s'#13#10#9#9#9'TileSourceURL=%s',
  [ClassName, Ord(FMP.Scale), MapDirectory, FMP.TileDirectory, FMP.Source]);
  DebugLog('%s.InitForm end ...', [ClassName]);
  {$ENDIF}
end;

procedure TMapForm.DeInitForm;
begin
  ClearThreads;
  inherited DeInitForm;
end;

procedure TMapForm.CacheFocusedChange(Item: TCacheItem);
var
  Index: Integer;
begin
  inherited CacheFocusedChange(Item);
  if Assigned(FMapCacheList) and Assigned(Item) and not (DataSetMeta = MForm.ActiveMeta) then
    for Index := 0 to Pred(FMapCacheList.Count) do
      if FMapCacheList.Items[Index].CacheItem = Item then
        begin
          FMapCacheList.Items[Index].InvalidateFocused;
          if act_Da_autoposition.Checked then
            FMapCenter := FMapCacheList.Items[Index].FGeo;
          UpdateTilesBitmap(False);
          Break;
        end;
end;

procedure TMapForm.CacheItemInsert(Item: TCacheItem);
begin
  inherited;
  if assigned(DataSetMeta) then
    if assigned(DataSetMeta.Table) then
      if assigned(DataSetMeta.Table.LatitudeField) then
        if assigned(DataSetMeta.Table.LongitudeField) then
          begin
            Item.FieldValue[DataSetMeta.Table.Fields.IndexByID(DataSetMeta.Table.LatitudeField.ID)]:=  FMapCenter.GetPeopleY;
            Item.FieldValue[DataSetMeta.Table.Fields.IndexByID(DataSetMeta.Table.LongitudeField.ID)]:= FMapCenter.GetPeopleX;
          end;
end;

procedure TMapForm.CacheSelectedChange(Item: TCacheItem);
var
  Index: Integer;
begin
  inherited CacheSelectedChange(Item);
  if Assigned(FMapCacheList) and Assigned(Item) then
    for Index := 0 to Pred(FMapCacheList.Count) do
      if FMapCacheList.Items[Index].CacheItem = Item then
        FMapCacheList.Items[Index].InvalidateFocused;
end;

procedure TMapForm.ChangeDataSetStatus;
begin
  inherited ChangeDataSetStatus;
  /// Здесь перерисовываем выделенный объект!!! (FocusetItem в кэше)
end;

procedure TMapForm.MapScaleUpdate(Sender: TObject);
begin
  if Assigned(Sender) and (Sender is TAction) then
    with Sender as TAction do
      begin
        Visible:= FMP.Enabled[TMapScale(Tag)];
        Enabled:= FMP.Enabled[TMapScale(Tag)];
        Checked:= Tag = Integer(FMP.Scale);
      end;
end;

procedure TMapForm.MapScaleExecute(Sender: TObject);
begin
  if Assigned(Sender) and (Sender is TAction) then
    begin
      FMP.SetScale( TMapScale((Sender as TAction).Tag));
      UpdateTilesBitmap(True);
    end;
end;

procedure TMapForm.BuildMapScaleActions;
var
  Index: TMapScale;
  Action: TAction;
  ScaleName: string;
begin
  for Index := Low(FMapScaleActions) to High(FMapScaleActions) do
    begin
      FreeAndNil(FMapScaleActions[Index]);
      ScaleName := FMP.Caption[Index]; // Format('_mAp.Scale%u', [Index]);
      Action := TAction.Create(ActionList);
      try
        Action.ActionList := ActionList;
        Action.Caption := GetTitle(ScaleName);
        Action.Hint := GetTitle(ScaleName, ttSecondName);
        Action.Tag := Index;
        Action.Category := CategoryMetrics;
        Action.OnUpdate := MapScaleUpdate;
        Action.OnExecute := MapScaleExecute;
      except
        Action.Free;
        raise;
      end;
      FMapScaleActions[Index] := Action;
    end;
end;

procedure TMapForm.BuildMapScaleMenu(RootItem: TMenuItem);
var
  Index: TMapScale;
  MenuItem: TMenuItem;
begin
  RootItem.Clear;
  for Index :=Low(FMapScaleActions) to High(FMapScaleActions) do
    if Assigned(FMapScaleActions[Index]) then
    begin
      MenuItem := TMenuItem.Create(RootItem);
      try
        MenuItem.Action := FMapScaleActions[Index];
        MenuItem.RadioItem := True;
        RootItem.Add(MenuItem);
      except
        MenuItem.Free;
        raise;
      end;
    end;
end;

procedure TMapForm.ClearThreads;
var
  List: TList;
  Job: TMapTilePoolJob;
begin
  if Assigned(FThreads) then
    begin
      List := FThreads.LockList;
      try
        while List.Count <> 0 do
          begin
            Job := List[0];
            if Assigned(Job.Thread) then Job.Thread.Terminate;
            List.Delete(0);
          end;
      finally
        FThreads.UnlockList;
      end;
    end;
end;

procedure TMapForm.NewThread(const Col, Row, Priority: Integer; const MP: TMapProperties; const BackgroundColor: TColor; const Running: Boolean);
var
  List: TList;
  Job: TMapTilePoolJob;
  Thread: TMapTilePoolThread;
  Index, NewIndex: Integer;
  nCol, nRow: Integer;
begin
  List := FThreads.LockList;
  try
    NewIndex := 0;
    nCol := MP.NormalizeTileX(Col);
    nRow := MP.NormalizeTileY(Row);

    // Ищем такую же нитку, если нитка для тайла уже есть, то Выходим не добавляя нитки!
      for Index := 0 to Pred(List.Count) do
        begin
          Job := List[Index];
          if (Job.MapScale = MP.Scale) and (Job.Source = MP.Source) and
             (Job.NormalizeTileRow = nRow) and (Job.NormalizeTileCol = nCol) then
              begin
                NewIndex := -1;
                Break;
              end;
        end;

    // Определяем место в очереди
    if NewIndex = 0 then
      begin
        for Index := 0 to Pred(List.Count) do
          begin
            Job := List[Index];
            if Job.MapScale <> MP.Scale then
              Break
            else
              if Priority > Job.TilePriority then // Если приоритет добавляемого тайла в очередь больше, то ..
                NewIndex := Succ(Index)           // Будем добавлять после существующей нитки ...
              else                                // Если приоритет добавляемого тайла в очередь меньше, то ..
                begin                             // Будем добавлять перед существующей ниткой ...
                  NewIndex := Max(0, Pred(Index));
                  Break;
                end;
          end;

        Job := TMapTilePoolJob.Create(MP.Source, Col, Row, MP.NormalizeTileX(Col), MP.NormalizeTileY(Row),
                                      Priority, MP.Scale, BackgroundColor,
                                      PrepareTileURL(nRow, nCol, MP), MP.TileDirectory);
        List.Insert(NewIndex, Job);
        if Running and (FExecutingThreadCount < FMaxExecutingThreadCount) then
          begin
            Thread := TMapTilePoolThread.Create(Job);
            Thread.OnTerminate := DoneThread;
            Job.Thread := Thread;
            Thread.Resume;
            Inc(FExecutingThreadCount);
          end;
      end;

  finally
    FThreads.UnlockList;
  end;
end;

procedure TMapForm.NotifyRefreshTimer(Sender: TObject);
begin
  FMapCacheList.DestroyControls;
  FMapCacheList.Clear;
  inherited;
  BuildLabels;
  FMapCacheList.ShowControls(pnlMap, FTileLeft, FTileTop, FTileRight, FTileBottom);
end;

procedure TMapForm.DoneThread(Sender: TObject);
var
  List: TList;
  Thread, NextThread: TMapTilePoolThread;
  NextJob: TMapTilePoolJob;
  Bitmap: TBitmap;
  GraphicClass: TGraphicClass;
  Graphic: TGraphic;
  X, Y, Index: Integer;
begin
  if Assigned(FThreads) and Assigned(Sender) and (Sender is TMapTilePoolThread) and HandleAllocated then
    begin
      Thread := Sender as TMapTilePoolThread;
      List := FThreads.LockList;
      try
        for Index := 0 to Pred(List.Count) do
          if TMapTilePoolJob(List[Index]).Thread = Thread then
            begin
              List.Delete(Index);
              Break;
            end;
        Dec(FExecutingThreadCount);
        {$IFDEF DEBUG}
        DebugLog('%s.DoneThread[%d: %d, %d]: executing %d task(s), total %d job(s) ...', [ClassName, Thread.TilePriority, Thread.TileCol, Thread.TileRow, FExecutingThreadCount, List.Count]);
        {$ENDIF}
        if Assigned(imgMap) then
          begin
            Index := 0;
            // Запустим следующую нитку если она есть, которая ещё не запущена ...
            while List.Count > Index do
              if Application.Terminated then
                Break
              else
                begin
                  NextJob := TMapTilePoolJob(List[Index]);
                  if Assigned(NextJob.Thread) then
                    Inc(Index)
                  else
                    begin
                      NextThread := TMapTilePoolThread.Create(NextJob);
                      NextThread.OnTerminate := DoneThread;
                      NextJob.Thread := NextThread;
                      {$IFDEF DEBUG}
                      DebugLog('%s.NextThread[%d: %d, %d]: executing %d task(s), total %d job(s) ...', [ClassName, NextThread.TilePriority, NextThread.TileCol, NextThread.TileRow, FExecutingThreadCount, List.Count]);
                      {$ENDIF}
                      NextThread.Resume;
                      Inc(FExecutingThreadCount);
                      Break;
                    end
                end;
            if not Application.Terminated and
               (FMP.Scale = Thread.MapScale) and (FMP.Source = Thread.Source) and
               (FTileLeft <= Thread.TileCol) and (Thread.TileCol <= FTileRight) and
               (FTileTop <= Thread.TileRow) and (Thread.TileRow <= FTileBottom) then
              begin
                GraphicClass := GraphicFileTypeToGraphic(DetectingGraphicFileType(Thread.FileName));
                if Assigned(GraphicClass) then
                  begin
                    Graphic := GraphicClass.Create;
                    try
                      Graphic_LoadFromFile_Resize(Graphic, Thread.FileName, FMP.TileSizeX, FMP.TileSizeY, Thread.BackgroundColor);
                      X:= (Thread.TileCol - FTileLeft) * FMP.TileSizeX;
                      Y:= (Thread.TileRow - FTileTop) * FMP.TileSizeY;
                      if MapTransparent < 255 then
                        begin
                          Bitmap := TBitmap.Create;
                          Bitmap.SetSize(FMP.TileSizeX, FMP.TileSizeY);
                          try
                            Bitmap.Canvas.Draw(0, 0, Graphic);
                            imgMap.Canvas.Brush.Color := Thread.BackgroundColor;
                            imgMap.Canvas.FillRect(Rect( X, Y, X + FMP.TileSizeX, Y + FMP.TileSizeY));
                            imgMap.Canvas.Draw(X, Y, Bitmap, MapTransparent);
                          finally
                            Bitmap.Free;
                          end;
                        end
                      else
                        begin
                          imgMap.Canvas.Draw(X, Y, Graphic);
                        end;
                      imgMap.Update;
                    finally
                      Graphic.Free;
                    end;
                  end;
              end;
          end;
        {$IFDEF DeDEBUG}
        if Length(Thread.ErrorMessage) <> 0 then
          Funcs.WriteLog('Downloaded map tile file %s skip error: %', [ExtractFileName(Thread.FileName), Thread.ErrorMessage], False, 'MAP');
        {$ENDIF}
      finally
        FThreads.UnlockList;
      end;
    end;
end;

procedure TMapForm.DoUpdate(Sender: TObject);
begin
  inherited DoUpdate(Sender);
  UpdateTilesBitmap(True);
end;

procedure TMapForm.RunThreads;
var
  List: TList;
  Job: TMapTilePoolJob;
  Thread: TMapTilePoolThread;
  Index: Integer;
begin
  if Assigned(FThreads) and Assigned(imgMap) and (FExecutingThreadCount < FMaxExecutingThreadCount) and not Application.Terminated then
    begin
      List := FThreads.LockList;
      try
        {$IFDEF DEBUG}
        DebugLog('%s.RunThreads: executing %d task(s), total %d job(s) ...', [ClassName, FExecutingThreadCount, List.Count]);
        {$ENDIF}
        Index := 0;
        // Запустим следующую нитку если она есть, которая ещё не запущена ...
        while List.Count > Index do
          if Application.Terminated then
            Break
          else
            begin
              Job := TMapTilePoolJob(List[Index]);
              if not Assigned(Job.Thread) then
                begin
                  Thread := TMapTilePoolThread.Create(Job);
                  Thread.OnTerminate := DoneThread;
                  Job.Thread := Thread;
                  {$IFDEF DEBUG}
                  DebugLog('%s.StartThread[%d: %d, %d]: executing %d task(s), total %d job(s) ...', [ClassName, Thread.TilePriority, Thread.TileCol, Thread.TileRow, FExecutingThreadCount, List.Count]);
                  {$ENDIF}
                  Thread.Resume;
                  Inc(FExecutingThreadCount);
                  if FExecutingThreadCount >= FMaxExecutingThreadCount then
                    Break;
                end;
              Inc(Index);
            end;
      finally
        FThreads.UnlockList;
      end;
    end;
end;

procedure TMapForm.WM_MouseWheel(var Message: TMSHMouseWheel);
var P: TPoint;
    Pixel: TCoordPix;
begin
  P:= pnlGrid.ScreenToClient(Point(Message.XPos, Message.YPos));
  FMP.Geo2Pixel(FMapCenter, Pixel);

  if (Message.WheelDelta < 0) and (FMP.Scale > Low(TMapScale)) and FMP.Enabled[FMP.Scale - 1] then
    begin
      Pixel.X := Pixel.X - (P.X - pnlGrid.ClientWidth div 2);
      Pixel.Y := Pixel.Y - (P.Y - pnlGrid.ClientHeight div 2);
      FMP.Pixel2Geo(Pixel, FMapCenter);
      FMP.SetScale(Pred(FMP.Scale));
      UpdateTilesBitmap(True);
    end else

  if (Message.WheelDelta > 0) and (FMP.Scale < High(TMapScale)) and FMP.Enabled[FMP.Scale + 1] then
    begin
      Pixel.X := Pixel.X + (P.X - pnlGrid.ClientWidth div 2) div 2;
      Pixel.Y := Pixel.Y + (P.Y - pnlGrid.ClientHeight div 2) div 2;
      FMP.Pixel2Geo(Pixel, FMapCenter);
      FMP.SetScale(Succ(FMP.Scale));
      UpdateTilesBitmap(True);
    end;

  imgMapMouseMove( nil, [], Message.XPos , Message.YPos);
end;

procedure TMapForm.ReCreateView;
begin
  DataSetMeta.Cache.FillAll;
  UpdateTilesBitmap(True);
  inherited ReCreateView;
end;

procedure TMapForm.act_Da_AutoPositionExecute(Sender: TObject);
begin
  inherited;
  //
end;

procedure TMapForm.act_Da_maphideingroupExecute(Sender: TObject);
begin
  inherited;
  UpdateTilesBitmap(True);
end;

procedure TMapForm.AfterInit;
var i, N, LatitudeFieldIndex, LongitudeFieldIndex : Integer;
    LatitudeValue, LongitudeValue, LatitudeMin, LongitudeMin, LatitudeMax, LongitudeMax: Variant;
    tDist, bestDist: Double;
begin
  inherited;
  if Not assigned(DatasetMeta) then Exit;
  if not Assigned(FMapCacheList) then Exit;
  if not Assigned(DataSetMeta.Cache) then Exit;
  if not DataSetMeta.Cache.Active then Exit;

  if not Assigned(DataSetMeta.Cache.TableMeta) then Exit;
  if not Assigned(DataSetMeta.Cache.Fields) then Exit;
  if not Assigned(DataSetMeta.Cache.TableMeta.LatitudeField) or
     not Assigned(DataSetMeta.Cache.TableMeta.LongitudeField) then Exit;

  LatitudeFieldIndex := DataSetMeta.Cache.Fields.IndexByID(DataSetMeta.Cache.TableMeta.LatitudeField.ID);
  LongitudeFieldIndex := DataSetMeta.Cache.Fields.IndexByID(DataSetMeta.Cache.TableMeta.LongitudeField.ID);

  if not Assigned(SecuritySystem) then Exit;
  if not SecuritySystem.CheckPolicyDataset(DataSetMeta.Cache.TableMeta.ID, spSelect) or
     not SecuritySystem.CheckPolicyField(DataSetMeta.Cache.Fields[LatitudeFieldIndex].ID, sdSelect) or
     not SecuritySystem.CheckPolicyField(DataSetMeta.Cache.Fields[LongitudeFieldIndex].ID, sdSelect) then Exit;

  LatitudeMin:= 200;
  LongitudeMin:= 200;
  LatitudeMax:= -200;
  LongitudeMax:= -200;
  for i:= 0 to Pred(DataSetMeta.Cache.Count) do
    begin
      LatitudeValue := DataSetMeta.Cache[i].FieldValue[LatitudeFieldIndex];
      LongitudeValue := DataSetMeta.Cache[i].FieldValue[LongitudeFieldIndex];

      DeLog.writeLog(IntToStr(Integer(VarType(LatitudeValue))));
      if VarIsType(LatitudeValue, [varSingle, varDouble, varCurrency, varFmtBCD]) and
         VarIsType(LongitudeValue, [varSingle, varDouble, varCurrency, varFmtBCD]) then
        begin
          if LatitudeValue < LatitudeMin then LatitudeMin:= LatitudeValue;
          if LatitudeValue > LatitudeMax then LatitudeMax:= LatitudeValue;
          if LongitudeValue < LongitudeMin then LongitudeMin:= LongitudeValue;
          if LongitudeValue > LongitudeMax then LongitudeMax:= LongitudeValue;
        end;
    end;

  if (LatitudeMin < 200) and (LongitudeMin < 200) then
    begin
      FMapCenter.SetPeople(VarAsType((LongitudeMin + LongitudeMax)/2, varDouble),
                           VarAsType((LatitudeMin + LatitudeMax)/2, varDouble));

    end;

{
  if Not assigned(DatasetMeta.Cache.FocusedItem) then
    if 0 < DatasetMeta.Cache.Count then
      DatasetMeta.Cache.FocusedItem:=  DatasetMeta.Cache[0];
}

  // ищем ближайший к центру элемент
  if (0 < FMapCacheList.Count) and not assigned(DatasetMeta.Cache.FocusedItem) then
    begin
      N:=0;
      bestDist:= SQR(FMapCacheList[0].FGeo.X - FMapCenter.X) + SQR(FMapCacheList[0].FGeo.Y - FMapCenter.Y);
      for i:= 1 to Pred(FMapCacheList.Count) do
        begin
          tDist:= SQR(FMapCacheList[i].FGeo.X - FMapCenter.X) + SQR(FMapCacheList[i].FGeo.Y - FMapCenter.Y);
          if tDist<bestDist then
            begin
              bestDist:= tDist;
              N:=i;
            end;
        end;

      if Assigned(FMapCacheList[N].FCacheItem) then
        begin
          FMapCacheList[N].FCacheItem.SetFocus;
          FMapCacheList[N].Select;
          if Assigned(FMapCacheList.FFocused) then FMapCacheList.FFocused.InvalidateFocused;
        end;
    end;
end;

procedure TMapForm.BuildLabels;
var
  MapCacheItem: TMapCacheItem;
  LatitudeFieldIndex, LongitudeFieldIndex, Index: Integer;
  LatitudeValue, LongitudeValue: Variant;
  People: TCoordGeo;
  Center, Pixel: TCoordPix;
  L, T, B, R: Extended;

  I, J: Integer;
  P: TCoordPix;
  LT: TCoordGeo;
  dX, dY : Extended;
  Pi, Pj: TCoordPix;
begin
  FMapCacheList.DestroyControls;
  FMapCacheList.Clear;
  if Assigned(DataSetMeta) and Assigned(DataSetMeta.Cache) and DataSetMeta.Cache.Active then
    begin
      DataSetMeta.Cache.ClearSelection;
      if Assigned(DataSetMeta.Cache.TableMeta) and Assigned(DataSetMeta.Cache.Fields) then
        if Assigned(DataSetMeta.Cache.TableMeta.LatitudeField) and Assigned(DataSetMeta.Cache.TableMeta.LongitudeField) then
          begin
            LatitudeFieldIndex := DataSetMeta.Cache.Fields.IndexByID(DataSetMeta.Cache.TableMeta.LatitudeField.ID);
            LongitudeFieldIndex := DataSetMeta.Cache.Fields.IndexByID(DataSetMeta.Cache.TableMeta.LongitudeField.ID);

            if Assigned(SecuritySystem) and SecuritySystem.CheckPolicyDataset(DataSetMeta.Cache.TableMeta.ID, spUpdate) and
               SecuritySystem.CheckPolicyField(DataSetMeta.Cache.Fields[LatitudeFieldIndex].ID, sdUpdate) and
               SecuritySystem.CheckPolicyField(DataSetMeta.Cache.Fields[LongitudeFieldIndex].ID, sdUpdate) then
              FMapCacheList.OnMoved := ImageMoved
            else
              FMapCacheList.OnMoved := nil;

            // определяем границы и создаем только те объекты, которые видны с запасом на размер самой иконки
            FMP.Geo2Pixel(FMapCenter, Center);

            Pixel.X := Center.X - (pnlGrid.ClientWidth  div 2) - 16;
            Pixel.Y := Center.Y - (pnlGrid.ClientHeight div 2) - 16 ;
            FMP.Pixel2Geo(Pixel, People);
            L:= People.GetPeopleX;
            T:= People.GetPeopleY;

            Pixel.X := Center.X + (pnlGrid.ClientWidth  div 2) + 16;
            Pixel.Y := Center.Y + (pnlGrid.ClientHeight div 2) + 16;
            FMP.Pixel2Geo(Pixel, People);
            R:= People.GetPeopleX;
            B:= People.GetPeopleY;

            for Index :=0 to Pred(DataSetMeta.Cache.Count) do
              begin
                LatitudeValue := DataSetMeta.Cache[Index].FieldValue[LatitudeFieldIndex];
                LongitudeValue := DataSetMeta.Cache[Index].FieldValue[LongitudeFieldIndex];
                if VarIsType(LatitudeValue, [varSingle, varDouble, varCurrency, varFmtBCD]) and
                   VarIsType(LongitudeValue, [varSingle, varDouble, varCurrency, varFmtBCD]) then
                  if (L<=LongitudeValue) and (LongitudeValue<=R) and (B<=LatitudeValue) and (LatitudeValue<=T) then
                    begin
                      People.SetPeople(VarAsType(LongitudeValue, varDouble), VarAsType(LatitudeValue, varDouble));
                      MapCacheItem := TMapCacheItem.Create(FMapCacheList, DataSetMeta.Cache[Index], People);
                      if FMapCacheList.Add(MapCacheItem) = -1 then MapCacheItem.Free;
                    end;
              end;
          end;
    end;

  if act_Da_maphideingroup.Checked then // Группируем при необходимости ...
    begin
      for I:= 0 to Pred(FMapCacheList.Count) do
        begin
          FMapCacheList.Items[I].LabelAlign:= laRight;
          // Отвяжем себя от группирующего кэша при необходимости ...
          if Assigned(FMapCacheList.Items[I].MainItem) then
            FMapCacheList.Items[I].MainItem.UnRegisterChildCacheItem(FMapCacheList.Items[I]);
          // Отвяжем детей от себя при необходимости ...
          while FMapCacheList.Items[I].Count <> 0 do
            FMapCacheList.Items[I].UnRegisterChildCacheItem(FMapCacheList.Items[I][0]);
        end;

      // определяем границы "прилипания" к родительскому элементу в ОБЩИХ координатах
      FMP.Geo2Pixel(FMapCenter, P);
      P.SetValue( P.X - DM.ilIcon24.Width, P.Y - DM.ilIcon24.Height);
      FMP.Pixel2Geo(P, LT);
      dX:= abs(LT.X - FMapCenter.X);
      dY:= abs(LT.Y - FMapCenter.Y);

      for I:= 0 to Pred(FMapCacheList.Count) do
        begin

          if not Assigned(FMapCacheList.Items[I].MainItem) then
            for J:= Succ(I) to Pred(FMapCacheList.Count) do
              if (abs(FMapCacheList.Items[I].Geo.X - FMapCacheList.Items[J].Geo.X) < dX) and
                 (abs(FMapCacheList.Items[I].Geo.Y - FMapCacheList.Items[J].Geo.Y) < dY) then
                FMapCacheList.Items[I].RegisterChildCacheItem(FMapCacheList.Items[J]);
        end;

      // Устанавливаем текст слева, если есть наложение на соседние итемы
      for i:= 0 to Pred(FMapCacheList.Count) do
        if not Assigned(FMapCacheList.Items[i].MainItem) then
          if FMapCacheList.Items[i].LabelAlign = laRight then
            begin
              FMP.Geo2Pixel(FMapCacheList.Items[i].Geo, Pi);
              for j:= 0 to Pred(FMapCacheList.Count) do
                if (i<>j) and not Assigned(FMapCacheList.Items[j].MainItem) then
                  begin
                    FMP.Geo2Pixel(FMapCacheList.Items[j].Geo, Pj);
                    if (Pi.X - FMapCacheList.Items[i].FIDX  <= Pj.X - FMapCacheList.Items[j].FIDX) and
                       (Pj.X - FMapCacheList.Items[j].FIDX <= Pi.X - FMapCacheList.Items[i].FIDX + FMapCacheList.Items[i].FW) and
                       (abs(Pi.Y - Pj.Y) <= FMapCacheList.Items[i].FH) then
                       begin
                         FMapCacheList.Items[i].LabelAlign:= laLeft;
                         Break;
                       end;
                  end;
            end;
    end;
end;

procedure TMapForm.MapSelectUpdate(Sender: TObject);
begin
  if Assigned(Sender) and (Sender is TAction) then
    with Sender as TAction do
      Checked := Assigned(FMapLayers) and Assigned(FMapLayers.Selected) and (FMapLayers.Selected.LayerID = Tag);
end;

procedure TMapForm.MapSelectExecute(Sender: TObject);
var Index: Integer;
begin
  if Assigned(FMapLayers) and Assigned(Sender) and (Sender is TAction) then
    begin
      Index:= FMapLayers.IndexByID((Sender as TAction).Tag);
      if FMapLayers.SelectLayer(Index) then
        begin
          FMP.AssignMap( FMapLayers.Selected.MP, False);
          UpdateTilesBitmap(True);
        end;
    end;
end;

function TMapForm.FindMapLayerAction(const LayerID: Integer; const AutoCreated: Boolean): TAction;
var
  Index: Integer;
  LayerName: string;
begin
  Result := nil;
  for Index := Pred(ActionList.ActionCount) downto 0 do
    if (ActionList[Index] is TAction) and SameText(ActionList[Index].Category, CategoryView) and (ActionList[Index].Tag = LayerID) then
      begin
        Result := ActionList[Index] as TAction;
        Break;
      end;
  if AutoCreated and not Assigned(Result) then
    begin
      Result := TAction.Create(ActionList);
      try
        Result.ActionList := ActionList;
        Result.Category := CategoryView;
        Result.Tag := LayerID;
        Result.OnUpdate := MapSelectUpdate;
        Result.OnExecute := MapSelectExecute;
        LayerName := CategoryView + ' ' + IntToStr(LayerID);
        if Assigned(FMapLayers) then
          begin
            Index:= FMapLayers.IndexByID(LayerID);
            if -1 < Index then
              LayerName := FMapLayers[Index].Name;
          end;
        Result.Caption := LayerName;
      except
        Result.Free;
        raise;
      end;
    end;
end;

function TMapForm.GetPropertiesString: string;
var
  S: string;
begin
  Result := inherited GetPropertiesString;
  if Assigned(FMapLayers.Selected) then
    Result := Result + ' Map=' + FMapLayers.Selected.Name + ';'
  else
    begin
      S := ViewParams.GetValueByName('Map');
      if Length(S) <> 0 then
        Result := Result + ' Map=' + S + ';';
    end;
  Result:=Result+' Zoom='+IntToStr(FMP.Scale) + ';';
  Result:=Result+' Longitude='+FloatToStr(FMapCenter.GetPeopleX) + ';';
  Result:=Result+' Latitude=' +FloatToStr(FMapCenter.GetPeopleY) + ';';
  Result:=Result+' Transparent='+IntToStr(MapTransparent) + ';';
end;

procedure TMapForm.BuildMapLayerMenu;
var
  Index, MenuPosition: Integer;
  MenuItem: TMenuItem;
  //              User   , Dataset
  MenuMaps: array[Boolean, Boolean] of Integer;
  procedure InserSeparator(const MenuPosition: Integer);
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(Item_Da_View);
    try
      MenuItem.Caption := '-';
      Item_Da_View.Insert(MenuPosition, MenuItem);
    except
      MenuItem.Free;
      raise;
    end;
  end;
begin
  Item_Da_View.Clear;
  if Assigned(FMapLayers) then
    begin
      ZeroMemory(@MenuMaps, SizeOf(MenuMaps));
      for Index := 0 to Pred(FMapLayers.Count) do
        begin
          MenuItem := TMenuItem.Create(Item_Da_View);
          try
            MenuItem.Action := FindMapLayerAction(FMapLayers[Index].LayerID);
            MenuItem.RadioItem := True;
            if FMapLayers[Index].UserID = 0 then
              begin
                //                    User, Dataset  +     User, No Dataset  +      No User, Dataset
                MenuPosition := MenuMaps[True, True] + MenuMaps[True, False] +  MenuMaps[False, True];
                if FMapLayers[Index].DatasetID = 0 then
                  begin
                    //                           +   No User, No Dataset
                    MenuPosition := MenuPosition + MenuMaps[False, False];
                    Inc(MenuMaps[False, False]);
                  end
                else
                  Inc(MenuMaps[False, True]);
                Item_Da_View.Insert(MenuPosition, MenuItem);
              end
            else
              begin
                //                    User, Dataset
                MenuPosition := MenuMaps[True, True];
                if FMapLayers[Index].DatasetID = 0 then
                  begin
                    //                           +     User, No Dataset
                    MenuPosition := MenuPosition + MenuMaps[True, False];
                    Inc(MenuMaps[True, False]);
                  end
                else
                  Inc(MenuMaps[True, True]);
                Item_Da_View.Insert(MenuPosition, MenuItem);
              end
          except
            MenuItem.Free;
            raise;
          end;
        end;
      if (MenuMaps[False, True] <> 0) and (MenuMaps[False, False] <> 0) then
        begin
          //                    User, Dataset  +     User, No Dataset  +      No User, Dataset
          MenuPosition := MenuMaps[True, True] + MenuMaps[True, False] +  MenuMaps[False, True];
          InserSeparator(MenuPosition);
        end;
      if ((MenuMaps[True, True] + MenuMaps[True, False]) <> 0) and ((MenuMaps[False, True] + MenuMaps[False, False]) <> 0) then
        begin
          //                    User, Dataset  +     User, No Dataset
          MenuPosition := MenuMaps[True, True] + MenuMaps[True, False];
          InserSeparator(MenuPosition);
        end;
      if (MenuMaps[True, True] <> 0) and (MenuMaps[True, False] <> 0) then
        begin
          //                    User, Dataset
          MenuPosition := MenuMaps[True, True];
          InserSeparator(MenuPosition);
        end;
    end;
  if Item_Da_View.Count = 0 then
    begin
      MenuItem := TMenuItem.Create(Item_Da_View);
      try
        MenuItem.Caption := GetTitle('_dA.Default');
        MenuItem.RadioItem := True;
        MenuItem.Checked := True;
        MenuItem.Enabled := False;
        Item_Da_View.Add(MenuItem);
      except
        MenuItem.Free;
        raise;
      end;
    end;
end;

procedure TMapForm.BuildMapLayerMainMenu;
var
  Index, Position: Integer;
  MenuItem: TMenuItem;
begin
  for Index := Pred(miViewRadioZone.MenuIndex) downto Succ(miSortGrpZone.MenuIndex) do
    MM_Da_View[Index].Free;
  Position := Succ(miSortGrpZone.MenuIndex);
  for Index := 0 to Pred(Item_Da_View.Count) do
    begin
      MenuItem := TMenuItem.Create(MM_Da_View);
      try
        MenuItem.Action := Item_Da_View[Index].Action;
        MenuItem.Caption := Item_Da_View[Index].Caption;
        MenuItem.RadioItem := Item_Da_View[Index].RadioItem;
        MM_Da_View.Insert(Position + Index, MenuItem);
      except
        MenuItem.Free;
        raise;
      end;
    end;
end;

procedure TMapForm.ImageDblClick(Sender: TObject);
var
  Processing: Boolean;
begin
  Processing := True;
  if Assigned(Sender) and (Sender is TControl) then
    try
      // Если кликнули на сгруппированных объектах, то ...
      if TMapCacheItem((Sender as TControl).Tag).Count <> 0 then
        try
          if FMP.Scale < High(TMapScale) then
            begin
              FMapCenter := TMapCacheItem((Sender as TControl).Tag).Geo;
              FMP.SetScale(Succ(FMP.Scale));
              UpdateTilesBitmap(True);
            end;
        finally
          Processing := False;
        end;
    except
      on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog('%s.ImageDblClick() skip error: %s', [ClassName, E.Message]);
          {$ENDIF}
          Processing := True;
        end;
    end;
  if Processing and Assigned(CacheItems.FocusedItem) then
    if (DataSetMeta.Role = drMain) then
      begin
        if not ViewArea then
          MForm.act_Da_ShowCardArea.Execute
      end
    else
      OpenCard;
end;

procedure TMapForm.imgMapDblClick(Sender: TObject);
var
  SP, CP: TPoint;
  TX, TY: Integer;
begin
  if GetCursorPos(SP) then
    begin
      CP := (Sender as TImage).ScreenToClient(SP);
      TX := FTileLeft + (CP.X div FMP.TileSizeX);
      TY := FTileTop + (CP.Y div FMP.TileSizeY);
      NewThread(TX, TY, 0, FMP, clWindow, True);
    end;
end;

procedure TMapForm.ImageMoved(Sender: TObject);
var
  LatitudeFieldIndex, LongitudeFieldIndex: Integer;
begin
  if Assigned(Sender) and (Sender is TImage) and Assigned(TMapCacheItem((Sender as TImage).Tag).CacheItem) then
    if Assigned(TMapCacheItem((Sender as TImage).Tag).CacheItem.Owner) then
      if Assigned(TMapCacheItem((Sender as TImage).Tag).CacheItem.Owner.TableMeta) and Assigned(TMapCacheItem((Sender as TImage).Tag).CacheItem.Owner.Fields) then
        if Assigned(TMapCacheItem((Sender as TImage).Tag).CacheItem.Owner.TableMeta.LatitudeField) and Assigned(TMapCacheItem((Sender as TImage).Tag).CacheItem.Owner.TableMeta.LongitudeField) then
          begin
            LatitudeFieldIndex := TMapCacheItem((Sender as TImage).Tag).CacheItem.Owner.Fields.IndexByID(TMapCacheItem((Sender as TImage).Tag).CacheItem.Owner.TableMeta.LatitudeField.ID);
            LongitudeFieldIndex := TMapCacheItem((Sender as TImage).Tag).CacheItem.Owner.Fields.IndexByID(TMapCacheItem((Sender as TImage).Tag).CacheItem.Owner.TableMeta.LongitudeField.ID);

            TMapCacheItem((Sender as TImage).Tag).CacheItem.FieldValue[LatitudeFieldIndex] := TMapCacheItem((Sender as TImage).Tag).Geo.GetPeopleY;
            TMapCacheItem((Sender as TImage).Tag).CacheItem.FieldValue[LongitudeFieldIndex] := TMapCacheItem((Sender as TImage).Tag).Geo.GetPeopleX;
            TMapCacheItem((Sender as TImage).Tag).CacheItem.Owner.UpdateRecord(TMapCacheItem((Sender as TImage).Tag).CacheItem);
          end;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('MapFormUnit unit initialization ...');
  {$ENDIF}
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('MapFormUnit unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

