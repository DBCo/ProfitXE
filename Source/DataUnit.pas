{$WARN SYMBOL_PLATFORM OFF}

unit DataUnit;

interface

uses
  Windows, SysUtils, Classes, Forms, Graphics, Controls, ImgList, Contnrs,
  System.Actions, Vcl.ActnList, ExtCtrls, XMLDoc, XMLIntf,
  CommDrv, uIconManager, DeCalculator,  URemember, System.Sensors, System.Sensors.Components, System.Win.Sensors,
  FireDAC.Phys.MySQLDef, FireDAC.Stan.Intf, FireDAC.Phys, FireDAC.Phys.MySQL, FireDAC.Phys.PGDef, FireDAC.Phys.PG
  {$IFDEF OLDPLUGIONS}, PluginsManagerUnit{$ENDIF}, Funcs, uOpenSSL;

type
  {$IFDEF WIN64}
  TUpdateThread = class(TThread)
  private
    FHWND: HWND;
  public
    constructor Create(const AHandle: HWND);
  protected
    procedure Execute; override;
  end;
  {$ENDIF}

  TDM = class(TDataModule)
    PortTimer: TTimer;
    CheckImages: TImageList;
    ilIcon08: TImageList;
    ilIcon08d: TImageList;
    ilIcon08h: TImageList;
    ilIcon16: TImageList;
    ilIcon16d: TImageList;
    ilIcon16h: TImageList;
    ilIcon24: TImageList;
    ilIcon24d: TImageList;
    ilIcon24h: TImageList;
    ilIcon32: TImageList;
    ilIcon32d: TImageList;
    ilIcon32h: TImageList;
    ilIcon48: TImageList;
    ilIcon48d: TImageList;
    ilIcon48h: TImageList;
    ilIcon64: TImageList;
    ilIcon64d: TImageList;
    ilIcon64h: TImageList;
    LocationSensor: TLocationSensor;
    ilIcon12: TImageList;
    ilIcon12d: TImageList;
    ilIcon12h: TImageList;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    procedure ShowAllert(Sender: TObject);

    //Обработчик события ComboBoxDrawItem для отображения Items
    procedure ComboBoxKeyPress(Sender: TObject; var Key: Char);
    procedure ComboBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);

    procedure GetStatus(Sender: TObject);
    procedure CommStart(Sender: TObject);
    procedure CommStop(Sender: TObject);
    procedure RegisterTemporaryFile(const aFileName: string);
//    function LoadResourceIcon(const ResourceName: string; const ImageIndex: Integer): Integer;
    procedure LoadResourceIcons;
  private
    { Private declarations }
    FLibrarySSL: TOpenSSL;
    fBaseIconCount : Integer;
    fCalculator: TDeCalculator;
    FTemporaryFiles: TStrings;
//    FIconIndexes: TStrings;
    FAscendingImageIndex: Integer;
    FDescendingImageIndex: Integer;
    FPatternHCaptionActive: Graphics.TBitmap;
    FPatternHCaptionInactive: Graphics.TBitmap;
    FPatternVCaptionActive: Graphics.TBitmap;
    FPatternVCaptionInactive: Graphics.TBitmap;
    FIconManager: TIconManager;
    FLatitude, FLongitude: Variant;
    {$IFDEF WIN64}
    FAppUpdateThread: TUpdateThread;
    procedure DoneAppUpdateThread(Sender: TObject);
    {$ENDIF}
    procedure CoordinateChanged(Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D);
  public
    FPGDriverLink: TFDPhysPgDriverLink;
    FMySQLDriverLink: TFDPhysMySQLDriverLink;
    CommPort : TCommPortDriver;
    procedure AfterConnect;
    procedure BeforeUnconnect;
    function LoadResourceImages(ImageList: TImageList; const ResourceName: string): Integer;
    function MapIconIndex(const ImageIndex: Integer): Integer;
    function UnMapIconIndex(const ImageIndex: Integer): Integer;
    procedure MapActionListIcons(ActionList: TActionList);
    property Calculator : TDeCalculator read fCalculator; // калькулятор для общих нужд
    property AscendingImageIndex: Integer read FAscendingImageIndex;
    property DescendingImageIndex: Integer read FDescendingImageIndex;
    property BaseIconCount : Integer read FBaseIconCount;
    function GetIconSizeForSize(aSize: Integer): Integer;
    function GetEImageList(aSize: Integer = -1): TImageList;
    function GetHImageList(aSize: Integer = -1): TImageList;
    function GetDImageList(aSize: Integer = -1): TImageList;
    {$IFDEF DEBUG}
    procedure DebugIconIndexesLog(const Text: string);
    procedure DebugDumpIconIndexes;
    {$ENDIF}
    property IconManager: TIconManager read FIconManager;
    property PatternHCaptionActive: Graphics.TBitmap read FPatternHCaptionActive;
    property PatternHCaptionInactive: Graphics.TBitmap read FPatternHCaptionInactive;
    property PatternVCaptionActive: Graphics.TBitmap read FPatternVCaptionActive;
    property PatternVCaptionInactive: Graphics.TBitmap read FPatternVCaptionInactive;
    function MyCoordinate(var aLatitude: Double; var aLongitude: Double): Boolean;
    function OpenSSL: Boolean;

    {$IFDEF WIN64}
    procedure GetUpdateInfo(aHandle: HWND);
    {$ENDIF}
    function AppUpdateStatus: string;
    function AppCurVersionText: string;
    function AppNewVersionText: string;
    function DoUpdateApplication(const aQuery: Boolean): Boolean;
  end;

  TStatusList = class;
  TStatusItem = class;

  TStatusProgress = class
  private
    FOwner: TStatusItem;
    FMin: Integer;
    FMax: Integer;
    FProgress: Integer;
    FLeft: Extended;
    FRight: Extended;
    FA: Extended;
    FB: Extended;
    //FAutoLevel: Boolean;
  public
    constructor Create(AOwner: TStatusItem; const AMin: Integer = 0; const AMax: Integer = 100);
    property Owner: TStatusItem read FOwner;
    property Min: Integer read FMin;
    property Max: Integer read FMax;
    property Progress: Integer read FProgress;
    property Left: Extended read FLeft;
    property Right: Extended read FRight;
    property A: Extended read FA;
    property B: Extended read FB;
  end;

  TStatusItem = class
  private
    FList     : TObjectList;
    FOwner    : TStatusList;
    FID       : Integer;
    FICO      : Integer;
    FHint     : string;
    FReadyHint : Boolean;
    FVisible  : Boolean;
    FOnClick  : TNotifyEvent;
    FFirst    : TStatusProgress;
    FLast     : TStatusProgress;
    FProgressX: Integer;
    FProgressW: Integer;
    FVisibleAuto: Boolean;
    //procedure SetProgress(const Value: Integer);
    procedure SetICO(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    function GetProgress: Integer;
    procedure SetProgress(const Progress: Integer);
    function GetHint: string;
    procedure SetHint(const Value: string);
  public
    constructor Create(AOwner: TStatusList);
    destructor Destroy; override;
    function Position: Integer;
    /// <summary>Процедура запуска прогресс бара с новыми значениями</summary>
    procedure BeginProgress(const ACount: Integer = 100; const ASteps: Integer = 1);
    /// <summary>Процедура завершения уровня прогресс бара</summary>
    procedure EndProgress;
    /// <summary>Метод прорисовки элемента</summary>
    procedure ItemDraw(Canvas: TCanvas; const Rect: TRect; const DrawIco: Boolean = True);
    /// <summary>Метод удаления всех уровней прогресс бара</summary>
    procedure ClearProgress;
    {$IFDEF DEBUG}
    procedure DebugProgressLog(const Text: string);
    {$ENDIF}
    property Owner: TStatusList read FOwner;
    property ID: Integer read FID;
    property ICO: Integer read FICO write SetICO;
    property Visible: Boolean read FVisible write SetVisible;
    property Hint: string read GetHint write SetHint;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Progress: Integer read GetProgress write SetProgress;
    property First: TStatusProgress read FFirst;
    property Last: TStatusProgress read FLast;
    function LevelCount: Integer;
  end;

  TStatusList = Class(TObjectList)
  private
    FParentControl: TControl;
    FImageList: TImageList;
    FProgressTime: TDateTime;
    function GetStatusItem(const Index: Integer): TStatusItem;
    function GetProgressIndex: Integer;
    procedure Update(const FullRepaint: Boolean = True);
  public
    function Add(const ID, ICO: Integer; const Hint: string; OnClick: TNotifyEvent = nil; const Visible: Boolean = True): TStatusItem;
    function StatusItemByID(const ID: Integer): TStatusItem;
    function StatusItemByPosition(const X: Integer): TStatusItem;
    function Remove(const ID: Integer): Boolean;
    /// <summary>Метод прорисовки элемента</summary>
    procedure Draw(Canvas: TCanvas; const Rect: TRect);
    property ParentControl: TControl read FParentControl write FParentControl;
    property ImageList: TImageList read FImageList write FImageList;
    property StatusItems[const Index: Integer]: TStatusItem read GetStatusItem; default;
    property ProgressIndex: Integer read GetProgressIndex;
  end;

type
  TShowActionMessage = (amWait, amMessage, amLock);

const
  auNull = 0;
  auProgress = 1;
  auCurrent = 2;
  auNew = 3;
  auOld = 4;
  auError = 32; // или больше, будем при необходимости классифицировать ошибки

var
  DM: TDM;
  dicAppName: String;
  dicAppFullName: String;
  dicAppHomeURL: String;
  dicAppMailURL: String;
  XStep: Integer = 8;
  YStep: Integer = 7;
  YSpace: Integer = 2;

  AppVersion: TFileVersion;
  AppUpdate:  TFileVersion;

  AppUpdateCode: Integer = auNull;
  AppUpdateURL: string = '';

  LastActionTime        : TTime;
  ShowActionMessage     : TShowActionMessage = amWait;
  flagOffice            : Boolean = False;
  _Hist                 : Integer;
  {$IFDEF OLDPLUGIONS}
  PluginsManager        : TPluginsManager;
  {$ENDIF}
  Rem                   : TAllerts;
  RemForm               : TForm;
  //_TemporaryFileList    : tStringList = nil;

  ImageCount{, UpImageIndex, DownImageIndex} : Integer;
  DeStatus : TStatusList;
  CommPortUsed : Boolean = False;
  clWindowSecond : TColor;

  DeCurrentDir : string;
    // C:\Program Files\Profit
  DeTempDir : string;
    // C:\Documents and Settings\andrey\Local Settings\Temp\Profit\
    // Временные файлы - Общая папка, все файлы удаляются автоматически при выходе
  //DeTempDirBDE : string;
    // C:\Documents and Settings\andrey\Application Data\Temp\Profit\~de00017.tmp
    // Временные файлы BDE, создается новый каталог, каталог удаляется при выходе
  DeDataDir : string;
    // C:\Documents and Settings\andrey\Application Data\Profit\
    // (генерируемые и лог файлы, файлы автоматически не удаляются)

implementation

uses Variants, Messages, Menus, StdCtrls, CommCtrl, ComCtrls, Masks, MMSystem,
     ShlObj, ShellAPI, Math, StrUtils, Types, Vcl.Dialogs,
     DeLog, DeTypes, Dictionary, DeSettings, Remember, Security, Main,
     DeActions, HintForm, uIconUtils;

{$R *.dfm}

{ TStatusProgress }

constructor TStatusProgress.Create(AOwner: TStatusItem; const AMin: Integer; const AMax: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FMin := AMin;
  FMax := math.Max(AMax, AMin);
  FProgress := AMin;
  FLeft := 0;
  FRight := 1;
  FA := 1;
  FB := 0;
end;

{ TStatusItem }

const
  ProgressWait: Double = 0.1 / (24 * 60 * 60);

constructor TStatusItem.Create(AOwner: TStatusList);
begin
  inherited Create;
  FList := TObjectList.Create;
  FOwner := AOwner;
  FProgressX := -1;
  FProgressW := MaxInt;
end;

destructor TStatusItem.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

{
procedure TStatusItem.SetProgress(const Value: Integer);
var
  Rect: TRect;
  Full: Boolean;
begin
  if FProgress <> Value then
    begin
      Full := (FProgress > Value) or (Value < 1);
      FProgress := Value;
      if Full then
        FOwner.Update
      else
        if Assigned(FOwner.ParentControl) and (FOwner.ParentControl is TStatusBar) then
          if ((FOwner.ParentControl as TStatusBar).Panels.Count <> 0) and Assigned((FOwner.ParentControl as TStatusBar).OnDrawPanel) then
            (FOwner.ParentControl as TStatusBar).OnDrawPanel(FOwner.ParentControl as TStatusBar,
            (FOwner.ParentControl as TStatusBar).Panels[0], Rect);
    end;
end;
}

procedure TStatusItem.SetICO(const Value: Integer);
begin
  if Value <> FICO then
    begin
      FICO := Value;
      FOwner.Update;
    end;
end;

procedure TStatusItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
    begin
      FVisible := Value;
      FOwner.Update;
    end;
end;

function TStatusItem.Position: Integer;
var
 Index: Integer;
begin
  Result := 0;
  for Index := 0 to Pred(FOwner.Count) do
    begin
      if FOwner.StatusItems[Index] = Self then
        Break
      else
        if FOwner.StatusItems[Index].Visible then
          Inc(Result, 18);
    end;
end;

procedure TStatusItem.BeginProgress(const ACount: Integer; const ASteps: Integer);
var
  Pre_, New_: TStatusProgress;
begin

  Assert(ASteps > 0, 'Step must be greater than zero');

  New_ := TStatusProgress.Create(Self, 0, Pred(ACount));
  try
    if (FList.Count = 0) and not FVisible then
      begin
        FProgressX :=-1;
        FProgressW := MaxInt;

        FVisibleAuto := False;
        FOwner.FProgressTime := Time + ProgressWait;
      end;

    if 0 < FList.Count then
      begin
        Pre_:=FList[0] as TStatusProgress;

        New_.FLeft  := Pre_.FLeft + (Pre_.FRight - Pre_.FLeft) * (          (Pre_.FProgress  - Pre_.FMin) / Succ(Pre_.FMax - Pre_.FMin) );
        New_.FRight := Pre_.FLeft + (Pre_.FRight - Pre_.FLeft) * (((Pre_.FProgress + ASteps) - Pre_.FMin) / Succ(Pre_.FMax - Pre_.FMin) );
      end;

    if New_.FMin <= New_.FMax then
      begin
        New_.FA := (New_.FRight-New_.FLeft) / Succ(New_.FMax - New_.FMin);
        New_.FB := New_.FLeft - New_.FA * New_.FMin;
      end;

    FList.Insert(0, New_);

    FLast := New_;
    if not Assigned(FFirst) then FFirst := FLast;
    {
    if (1 = FList.Count) then
      FOwner.Update;
    {}
  except
    New_.Free;
    raise;
  end;
end;

procedure TStatusItem.EndProgress;
begin
  if Assigned(FList) and (FList.Count <> 0) then
    begin
      FList.Delete(0);
      if FList.Count <> 0 then
        FLast := FList[0] as TStatusProgress
      else
        begin
          if FVisible then FOwner.Update;
          if FVisibleAuto then FVisible := False;
          FLast := nil;
          FFirst := nil;
        end;
    end;
end;

procedure TStatusItem.ClearProgress;
begin
  if Assigned(FList) then
    while FList.Count <> 0 do
      EndProgress;
end;

procedure TStatusItem.SetHint(const Value: string);
begin
  FReadyHint:=False;
  FHint:= Value;
end;

function TStatusItem.GetHint: string;
begin
  if not FReadyHint then
    begin
      FReadyHint:= True;
      FHint:= GetTitle(FHint);
    end;

  Result:= FHint;
end;

function TStatusItem.GetProgress: Integer;
begin
  if Assigned(Last) then
    Result := Last.Progress
  else
    Result := 0;
end;

procedure TStatusItem.SetProgress(const Progress: Integer);
var PX: Integer;
    Rect: TRect;
begin
  if Assigned(Last) then
    begin
      Last.FProgress:=Progress;
      if not FVisible then
        begin
          if Assigned(FOwner) and (FOwner.FProgressTime < Time) then
            begin
              FVisibleAuto := True;
              FVisible := True;
              FOwner.Update;
            end;
        end
      else
        begin
          PX:=Last.FProgress;
          if PX < Last.Min then PX:=Last.Min;
          if Last.Max-1 < PX then PX:=Last.Max-1;
          PX := Round(FProgressW * (PX * Last.A + Last.B));
          if not( PX = FProgressX) then
            begin
              //перерисовываем только прогресс бар
              SendGetStructMessage((FOwner.ParentControl as TStatusBar).Handle, SB_GETRECT, 0, Rect);
              InflateRect(Rect, -1, -1);
              ItemDraw((FOwner.ParentControl as TStatusBar).Canvas, Rect, False);
        //    Application.ProcessMessages;       // из-за него "залипает" курсор в режим драг-энд-дроп
            end;
        end;
    end;
end;

procedure TStatusItem.ItemDraw(Canvas: TCanvas; const Rect: TRect; const DrawIco: Boolean = True);
var
  WorkRect: TRect;
  dY: Integer;
//  Width, Index: Integer;
//  StatusProgress: TStatusProgress;
begin
  if Assigned(Canvas) then
    begin
      WorkRect := Rect;
      Inc(WorkRect.Left);
      Inc(WorkRect.Top);
      dY:= (WorkRect.Height - 16) div 2;
      if Visible and Assigned(FList) and (FList.Count <> 0) then
        begin
          // Отрисуем прогресс бар ...
          if Assigned(FOwner) and Assigned(FOwner.ImageList) and (ICO <> -1) then
            begin
              if DrawIco then
                FOwner.ImageList.Draw(Canvas, WorkRect.Left, WorkRect.Top + dY, ICO);
              Inc(WorkRect.Left, Succ(FOwner.ImageList.Width));
            end;
          Dec(WorkRect.Bottom, 2);
          Dec(WorkRect.Right, 2);
          Canvas.Pen.Color := clActiveCaption;
          Canvas.Brush.Color := clActiveCaption;
          Canvas.MoveTo(WorkRect.Left, WorkRect.Top);
          Canvas.LineTo(WorkRect.Left, WorkRect.Bottom);  // |
          Canvas.LineTo(WorkRect.Right, WorkRect.Bottom); // _
          Canvas.LineTo(WorkRect.Right, WorkRect.Top);    // |
          Canvas.LineTo(WorkRect.Left, WorkRect.Top);     // _
          Inc(WorkRect.Top, 2);
          Dec(WorkRect.Bottom);
          Inc(WorkRect.Left, 2);
          Dec(WorkRect.Right);

          FProgressW := WorkRect.Right - WorkRect.Left;
          FProgressX := Round(FProgressW * (Last.FProgress * Last.A + Last.B));
          Canvas.Rectangle(WorkRect.Left, WorkRect.Top, WorkRect.Left + FProgressX, WorkRect.Bottom);
        end
      else
        if Visible and Assigned(FOwner) and Assigned(FOwner.ImageList) and (ICO <> -1) then
          FOwner.ImageList.Draw(Canvas, WorkRect.Left + Position, WorkRect.Top + dY , ICO);
    end;
end;

function TStatusItem.LevelCount: Integer;
begin
  Result:= FList.Count;
end;

{$IFDEF DEBUG}
procedure TStatusItem.DebugProgressLog(const Text: string);
  function PrepareStatusProgressLog(StatusProgress: TStatusProgress): string;
  begin
    if Assigned(StatusProgress) then
      Result := Format(' %10d | %10d | %10d | %20s | %20s | %20s | %20s |', [StatusProgress.Progress,
        StatusProgress.Min, StatusProgress.Max,
        FloatToStrF(StatusProgress.Left, ffFixed, 10, 9), FloatToStrF(StatusProgress.Right, ffFixed, 10, 9),
        FloatToStrF(StatusProgress.A, ffFixed, 10, 9), FloatToStrF(StatusProgress.B, ffFixed, 10, 9)
        ])
    else
      Result := Format('%12s|%12s|%12s|%22s|%22s|%22s|%22s|', [' ', ' ', ' ', ' ', ' ', ' ', ' ']);
  end;
  function PrepareStatusProgressItemsLog: string;
  var
    IndexSize, Index: Integer;
    Value: string;
  begin
    Result := EmptyStr;
    IndexSize := Length(IntToStr(FList.Count));
    for Index := 0 to Pred(FList.Count) do
      Result := Result + Format(#13#10'                        | %*d. |%s', [
        IndexSize, Index, PrepareStatusProgressLog(FList[Index] as TStatusProgress)]);
    if Length(Result) <> 0 then
      begin
        Value := Format(#13#10'                        +-%s--+%s+%s+%s+%s+%s+%s+%s+', [DupeString('-', IndexSize),
          DupeString('-', 12), DupeString('-', 12), DupeString('-', 12),
          DupeString('-', 22), DupeString('-', 22), DupeString('-', 22), DupeString('-', 22)]);
        Result := Value +
          Format(#13#10'                        | %*s | %-10s | %-10s | %-10s | %-20s | %-20s | %-20s | %-20s |',
            [-IndexSize, 'No', 'Progress', 'Min', 'Max', 'Left', 'Right', 'A', 'B']) +
          Value + Result + Value;
      end;
  end;
begin
  DebugLog(Text + PrepareStatusProgressItemsLog);
end;
{$ENDIF}

{ TStatusList }

function TStatusList.Add(const ID, ICO: Integer; const Hint: string; OnClick: TNotifyEvent; const Visible: Boolean): TStatusItem;
begin
  Result := TStatusItem.Create(Self);
  Result.FID       := ID;
  Result.FICO      := DM.MapIconIndex(ICO);
  Result.FOnClick  := OnClick;
  Result.Hint      := Hint;
  Result.FVisible  := Visible;
  if inherited Add(Result) <> -1 then
    begin
      if Visible then Update;
    end
  else
    FreeAndNil(Result);
end;

function TStatusList.GetStatusItem(const Index: Integer): TStatusItem;
begin
  if (Index < 0) or (Index >= Count) then
    Result := nil
  else
    Result := TStatusItem(Items[Index]);
end;

function TStatusList.GetProgressIndex: Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Pred(Count) do
    with StatusItems[Index] do
      if Visible and Assigned(FList) and (FList.Count <> 0) then
        begin
          Result := Index;
          Break;
        end;
end;

function TStatusList.StatusItemByID(const ID: Integer): TStatusItem;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to Pred(Count) do
    if StatusItems[Index].ID = ID then
      begin
        Result := StatusItems[Index];
        Break;
      end;
end;

function TStatusList.StatusItemByPosition(const X: Integer): TStatusItem;
var
  Index, Left: Integer;
begin
  Result := nil;
  Left := 0;
  for Index := 0 to Pred(Count) do
    if StatusItems[Index].Visible then
      begin
        if (Left <= X) and (X < Left + 18) then
          begin
            Result := StatusItems[Index];
            Break;
          end;
        Inc(Left, 18);
      end;
end;

function TStatusList.Remove(const ID: Integer): Boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to Pred(Count) do
    if StatusItems[Index].ID = ID then
      begin
        inherited Remove(StatusItems[Index]);
        Result := True;
        Break;
      end;
  Update;
end;

procedure TStatusList.Update(const FullRepaint: Boolean);
var
  Rect: TRect;
begin
  if Assigned(ParentControl) then
    if FullRepaint then
      ParentControl.Repaint
    else
      if (ParentControl is TStatusBar) and ((ParentControl as TStatusBar).Panels.Count > 0) then
        begin
          SendGetStructMessage((ParentControl as TStatusBar).Handle, SB_GETRECT, 0, Rect);
          InflateRect(Rect, -1, -1);
          Draw((ParentControl as TStatusBar).Canvas, Rect);
        end
      else
       ParentControl.Invalidate;
end;

procedure TStatusList.Draw(Canvas: TCanvas; const Rect: TRect);
var
  Index: Integer;
begin
  if Assigned(Canvas) then
    begin
      Index := ProgressIndex;
      if Index <> -1 then
        StatusItems[Index].ItemDraw(Canvas, Rect)
      else
        for Index := 0 to Pred(Count) do
          StatusItems[Index].ItemDraw(Canvas, Rect);
    end;
end;

{$IFDEF WIN64}

{ TUpdateThread }

constructor TUpdateThread.Create(const AHandle: HWND);
begin
  inherited Create(True);
  Priority := tpLowest;
  FHWND := AHandle;
  FreeOnTerminate := True;
end;

procedure TUpdateThread.Execute;
var Res : Boolean;
    P: Integer;
    sl: TStringList;
    T: String;
    Document: TXMLDocument;
    RootNode, ChildNode: IXMLNode;
begin
  AppUpdateCode:= auProgress;
  SendMessage(FHWND, DM_DOWNLOADNOTIFY, AppUpdateCode, NativeInt(PChar(DM.AppUpdateStatus)));
  { TODO -oKruglov -cBug : Ещё лучше! Создаём КОМПОНЕНТ в нитке! Овнера нет, разрушать тоже не надо. Подумаешь утечка памяти! }
  Document := TXMLDocument.Create(nil);
  try
   { TODO -oKruglov -cBug : Я конечно же всё понимаю, но Dict является объектом основного потока! И если нужен CurrentLang, то надо его передавать в качестве параметра нитке! }
    Res:= GetInetString(Format(urlUpdate, [ Dict.CurrentLang ]), T);
    AppUpdateURL:= EmptyStr;

    if Res then
      begin
        P:= Pos('?>', T);                /// Фигня <? ... ?> называется "Преамблуа" )))
        if 0<P then Delete(T, 1, P+1);   /// Че за фигня, заголовок мешает парсить

        Document.LoadFromXML(T);
        Document.Active:= True;
        Res:= Document.Active;
//      RootNode:= Document.ChildNodes.FindNode('update');
        RootNode:= Document.ChildNodes[0];
        if Res and assigned(RootNode) then
          begin
            ChildNode:= RootNode.ChildNodes.FindNode('version');
            if assigned(ChildNode) then
              TryStringToFileVersion(ChildNode.Text, AppUpdate);

            ChildNode:= RootNode.ChildNodes.FindNode('url');
            if assigned(ChildNode) then AppUpdateURL:= ChildNode.Text;
          end;
      end;

    Document.Active:= false;
  except
    Res:= False;
  end;

  // не удалось получить данные
  if (Not Res) then
    AppUpdateCode:= auError else

  // не удалось разобрать данные
  if (AppUpdate.Major = 0) or (Length(AppUpdateURL) = 0) then
    AppUpdateCode:= auError else

  case CompareFileVersion(AppVersion, AppUpdate) of
    0: AppUpdateCode:= auCurrent; // версия совпадает
    -1: AppUpdateCode:= auNew;    // доступно обновление
  else
    AppUpdateCode:= auOld;
  end;

  SendMessage(FHWND, DM_DOWNLOADNOTIFY, AppUpdateCode, NativeInt(PChar(DM.AppUpdateStatus)));
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure TDM.DataModuleCreate(Sender: TObject);
var
   i, BeginRGB, EndRGB: Integer;
   S    : String;
   Id   : TItemIDList;
   PId  : PItemIDList;
// A,B,C,Binmap : TBitmap;
// C1,C2        : TColor;
begin
  LastActionTime := Time;

  try
    LocationSensor.active:= true;
    LocationSensor.OnLocationChanged:= CoordinateChanged;
  Except
  end;
  FLatitude:= unassigned;
  FLongitude:= unassigned;

  S := Variables.AsString[RegApplicationTitle];
  i := Pos('|', S);
  if i = 0 then
    begin
      dicAppName := S;
      dicAppFullName := S;
    end
  else
    begin
      dicAppName := Copy(S, 1, Pred(i));
      dicAppFullName := Copy(S, Succ(i), Length(S));
    end;

  dicAppHomeURL := Variables.AsString[RegApplicationHomeURL];
  dicAppMailURL := Variables.AsString[RegApplicationMailURL];

  // определение каталога программы
  DeCurrentDir := GetCurrentDir;

  // определение временного каталога
  DeTempDir := GetCommonTempFolder;

  // определение каталога для "полу-нужных" данных
  DeDataDir := GetAppDataFolder + '\' + cApplicationName; // dicAppName;
  ForceDirectories(DeDataDir);

  // создание временного каталога BDE
  // Не нужен, т.к. BDE более не поддерживается в XE7
  {
  i := 0;
  while DirectoryExists(DeTempDir+Format(BDEDirPattern, [i])) do
    inc(i);
  DeTempDirBDE := DeTempDir+Format(BDEDirPattern, [i]);
  if not ForceDirectories(DeTempDirBDE) then
    Application.MessageBox(
       pChar(Format('Error creating temporary subfolder in the folder ''%s''.', [DeTempDir])),
         'Warning', MB_OK or MB_ICONEXCLAMATION);
  }
  // проверка наличия каталога для выходных файллов
  if Not SysUtils.DirectoryExists(Variables.AsString[RegDirPath]) then
    begin
      SetLength(S, MAX_PATH);
      PId := @Id;
      SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, PId);
      SHGetPathFromIDList(PId, PChar(S));
      Variables.AsString[RegDirPath] := IncludeTrailingPathDelimiter(Trim(S));
    end;

  // ----------------------------------------------
  {$IFDEF DeDEBUG}
  Funcs.WriteLog('Start DataUnit');
  {$ENDIF}
  // ----------------------------------------------

  clWindowSecond:=ChangeBrightnessByDelta(clInfoBK,-2);
  {
  BitMapCheck1:=TBitMap.Create;
  BitMapCheck1.LoadFromResourceName(hInstance,'BITCHECK1');
  BitMapCheck1.Transparent := True;
  BitMapCheck1.TransParentColor := clFuchsia;

  BitMapCheck2:=TBitMap.Create;
  BitMapCheck2.LoadFromResourceName(hInstance,'BITCHECK2');
  BitMapCheck2.Transparent := True;
  BitMapCheck2.TransParentColor := clFuchsia;

  BitMapBlob1:=TBitMap.Create;
  BitMapBlob1.LoadFromResourceName(hInstance,'BITBLOB1');
  BitMapBlob1.Transparent := True;
  BitMapBlob1.TransParentColor := clFuchsia;

  BitMapBlob2:=TBitMap.Create;
  BitMapBlob2.LoadFromResourceName(hInstance,'BITBLOB2');
  BitMapBlob2.Transparent := True;
  BitMapBlob2.TransParentColor := clFuchsia;
  }
  LoadResourceIcons;
  {
  LoadResourceImages(mImagesE, 'MENU'); // Выкинул картинки в файл MENU.bmp (линкуется в ресурс приложения).
  rgb2light( mImagesE,  mImagesH, True);
  rgb2bw   ( mImagesE,  mImagesD);

  rgb2varsmall( mImagesE,  smImagesE);
  rgb2bw   (smImagesE,  smImagesD);
  rgb2light(smImagesE,  smImagesH, True);
  LoadResourceImages(TImages, 'ICONS'); // Выкинул картинки в файл ICONS.bmp (линкуется в ресурс приложения).
  A:=TBitMap.Create;
  A.PixelFormat := pf24Bit;
  A.Height:=TImages.Height;
  A.Width :=TImages.Width*TImages.Count;

  B:=TBitMap.Create;
  B.PixelFormat := pf24Bit;
  B.Height:=  TImages.Height;
  B.Width :=2*TImages.Width*TImages.Count;

  C:=TBitMap.Create;
  C.PixelFormat := pf24Bit;
  C.Height:= TImages.Height div 2;
  C.Width := TImages.Width*TImages.Count;
  ImageCount := TImages.Count;
  }
  ImageCount := ilIcon32.Count;
  {
  for i:=0 to TImages.Count-1 do
    TImages.Draw( A.Canvas, i*(TImages.Width), 0, i);

  Transp2color_bw(clWhite,   A, B, False);    ImagesWhite.AddMasked(B,clFuchsia);
    big2small(B,C);                           swImages.AddMasked(C,clFuchsia);
  Transp2color_bw(clWindow,  A, B, False);    ImagesWindow.AddMasked(B,clFuchsia);
    big2small(B,C);                           sImages.AddMasked(C,clFuchsia);
  Transp2color_bw(clBtnFace, A, B, False);    ImagesBtnFace.AddMasked(B,clFuchsia);
    big2small(B,C);                           ImagesBFS.AddMasked(C,clFuchsia);
  Transp2color_bw(clActiveCaption,A,B,False); ImagesActive.AddMasked(B,clFuchsia);
    big2small(B,C);                           sActiveCaptionImages.AddMasked(C,clFuchsia);
  Transp2color_bw(clInactiveCaption,A,B,False);
    big2small(B,C);                           sInactiveCaptionImages.AddMasked(C,clFuchsia);

  A.Free;
  B.Free;
  C.Free;
  }

  //LoadResourceIcon('MAINICON', 21); // Убрал по распоряжению БОССа ;)

  (*
  Binmap := TBitmap.Create;
  Binmap.LoadFromResourceName(hInstance,{'BTNUP'} 'ASCENDING');
    rgb2transparent(clFuchsia,clBtnFace,Binmap);
    //UpImageIndex := sImages.AddMasked(Binmap, clFuchsia);
    SortDirectionImages[sdAscending] := sImages.AddMasked(Binmap, clFuchsia);
  Binmap.LoadFromResourceName(hInstance,{'BTNDOWN'} 'DESCENDING');
    rgb2transparent(clFuchsia,clBtnFace,Binmap);
    //DownImageIndex := sImages.AddMasked(Binmap,clFuchsia);
    SortDirectionImages[sdDescending] := sImages.AddMasked(Binmap,clFuchsia);
  Binmap.Free;
  *)
  CommPort := TCommPortDriver.Create(Self);
  CommPort.EndByte := 13;

  Randomize;
  DeStatus := TStatusList.Create;
  DeStatus.ImageList := ilIcon16; // ImagesBFS;

  flagOffice:=IsOLEObjectInstalled(ExcelName);

  {$IFDEF OLDPLUGIONS}
  PluginsManager := TPluginsManager.Create(Self);
  PluginsManager.RegisterPlugins;
  {$ENDIF}

  if Variables.AsBoolean[RegStart] then
    CommStart(Sender)
  else
    GetStatus(Sender);

  Rem:= TAllerts.Create;
  Rem.onAllert:= ShowAllert;

  // готовим градиент для заголовков
  BeginRGB:= ColorToRGB(clActiveCaption);
  EndRGB:= ColorToRGB(clGradientActiveCaption);

  FPatternHCaptionActive:= Graphics.TBitmap.Create;
  FPatternHCaptionActive.SetSize(256, 1);
  for i:= 0 to 255 do
    FPatternHCaptionActive.Canvas.Pixels[i, 0]:=
      RGB( (GetRValue(BeginRGB) * (255 - i) + GetRValue(EndRGB) * i) div 255,
           (GetGValue(BeginRGB) * (255 - i) + GetGValue(EndRGB) * i) div 255,
           (GetBValue(BeginRGB) * (255 - i) + GetBValue(EndRGB) * i) div 255   );

  FPatternVCaptionActive:= Graphics.TBitmap.Create;
  FPatternVCaptionActive.SetSize(1, 256);
  for i:= 0 to 255 do
    FPatternVCaptionActive.Canvas.Pixels[0, i]:= FPatternHCaptionActive.Canvas.Pixels[255 - i, 0];


  BeginRGB:= ColorToRGB(clInactiveCaption);
  EndRGB:= ColorToRGB(clGradientInactiveCaption);

  FPatternHCaptionInactive:= Graphics.TBitmap.Create;
  FPatternHCaptionInactive.SetSize(256, 1);
  for i:= 0 to 255 do
    FPatternHCaptionInactive.Canvas.Pixels[i, 0]:=
      RGB( (GetRValue(BeginRGB) * (255 - i) + GetRValue(EndRGB) * i) div 255,
           (GetGValue(BeginRGB) * (255 - i) + GetGValue(EndRGB) * i) div 255,
           (GetBValue(BeginRGB) * (255 - i) + GetBValue(EndRGB) * i) div 255   );

  FPatternVCaptionInactive:= Graphics.TBitmap.Create;
  FPatternVCaptionInactive.SetSize(1, 256);
  for i:= 0 to 255 do
    FPatternVCaptionInactive.Canvas.Pixels[0, i]:= FPatternHCaptionInactive.Canvas.Pixels[255 - i, 0];

  {$IFDEF DeDEBUG}
  Funcs.WriteLog('Finish DataUnit');
  {$ENDIF}

  fCalculator := TDeCalculator.Create;
  Application.OnMessage := AppMessage;
end;
//------------------------------------------------------------------------------
procedure TDM.DataModuleDestroy(Sender: TObject);
var SearchRec, SearchRecDir: TSearchRec;
    i,FindResult,FindResultDir: Integer;
begin
  fCalculator.Free;

  //PatternCaptionActive.Free;
  //PatternCaptionInactive.Free;
  FreeAndNil(FLibrarySSL);
  FreeAndNil(FPatternHCaptionActive);
  FreeAndNil(FPatternHCaptionInactive);

  Rem.Enabled:=False;
  Rem.Free;

  CommStop(Sender);
  {$IFDEF OLDPLUGIONS}
  PluginsManager.Free;
  {$ENDIF}
  DeStatus.Free;

  // Удаление временных файлов
  FindResult:=FindFirst(DeTempDir+'\'+AnyFileMask, faAnyFile, SearchRec);
  try
    while FindResult = 0 do
    begin
      if ((SearchRec.Attr and faDirectory) = 0) and MatchesMask(SearchRec.Name, AnyFileMask)
      then DeleteFile(DeTempDir+'\'+SearchRec.Name);
      FindResult:=FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;

  // Удаление временных каталогов
  FindResultDir:=FindFirst(DeTempDir+'\~de*.tmp', faDirectory, SearchRecDir);
  try
    while FindResultDir = 0 do
      begin
        FindResult:=FindFirst(DeTempDir+'\'+SearchRecDir.Name+'\*.*', faAnyFile, SearchRec);
        try
          while FindResult = 0 do
          begin
            if ((SearchRec.Attr and faDirectory) = 0) then
              DeleteFile(DeTempDir+'\'+SearchRecDir.Name+'\'+SearchRec.Name);
            FindResult:=FindNext(SearchRec);
          end;
        finally
          FindClose(SearchRec);
        end;

        RMDir(DeTempDir+'\'+SearchRecDir.Name);
        FindResultDir:=FindNext(SearchRecDir);
      end;
  finally
    FindClose(SearchRecDir);
  end;

 {$IFDEF WIN64}
 if Assigned(FAppUpdateThread) then
   try
     FAppUpdateThread.Terminate;
     //FAppUpdateThread.Priority:=tpNormal;
     //FreeAndNil(FAppUpdateThread);
     FAppUpdateThread := nil;
   except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('TDM.DataModuleDestroy error: ' + E.Message);
    {$ENDIF}
   end;
 {$ENDIF}

  {
  FreeAndNil(BitMapCheck1);
  FreeAndNil(BitMapBlob1);
  FreeAndNil(BitMapCheck2);
  FreeAndNil(BitMapBlob2);
  }

  if Assigned(FTemporaryFiles) then
    begin
      for I := Pred(FTemporaryFiles.Count) downto 0 do
        begin
          if FileExists(FTemporaryFiles[I]) then
            begin
              if DeleteFile(FTemporaryFiles[I]) then
                FTemporaryFiles.Delete(I);
            end
          else if SysUtils.DirectoryExists(FTemporaryFiles[I]) then
            begin
              ClearDirectory(FTemporaryFiles[I], True);
              if RemoveDir(FTemporaryFiles[I]) then
                FTemporaryFiles.Delete(I);
            end;
        end;
      FreeAndNil(FTemporaryFiles);
    end;
  FreeAndNil(FIconManager);
//  FreeAndNil(FIconIndexes);
end;

procedure TDM.AppMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if (Msg.message = WM_KEYDOWN) or
     (Msg.message = WM_MOUSEMOVE) then
  begin
    LastActionTime := Time;
  end;
end;

//------------------------------------------------------------------------------
procedure TDM.RegisterTemporaryFile(const aFileName: string);
begin
  if not Assigned(FTemporaryFiles) then
    FTemporaryFiles := TStringList.Create;
  FTemporaryFiles.Add(aFileName);
end;

//------------------------------------------------------------------------------
procedure TDM.ShowAllert(Sender: TObject);
begin
  if Not Assigned(RemForm) then
    begin
      RemForm:=TForm_Da_Remember.Create(Application);
      RemForm.Visible:=False;

      if (Variables.AsBoolean[RegSoundOn]) and
         (FileExists(Variables.AsString[RegRemSoundPath])) and
         (waveOutGetNumDevs<>0) then
        sndPlaySound(PChar(Variables.AsString[RegRemSoundPath]), SND_ASYNC);
    end;

  TForm_Da_Remember(RemForm).UpdateAllerts(Sender);

  with RemForm do
    if Visible or not Enabled or (fsModal in FormState) or (FormStyle = fsMDIChild) then
    // Ничего не делаем, форма уже на экране, смотри реализацию ShowModal
  else
    RemForm.ShowModal;
end;

//------------------------------------------------------------------------------

function TDM.GetIconSizeForSize(aSize: Integer): Integer;
begin
  case aSize of
     0.. 7: Result:=  0;
     8..11: Result:=  8;
    12..15: Result:= 12;
    16..23: Result:= 16;
    24..31: Result:= 24;
    32..47: Result:= 32;
    48..63: Result:= 48;
    else    Result:= 64;
  end;
end;

function TDM.GetEImageList(aSize: Integer = -1): TImageList;
begin
  case iif(aSize < 0, MForm.MenuIcoSize, aSize) of
       0: Result := nil;
       8: Result := ilIcon08;
      12: Result := ilIcon12;
      16: Result := ilIcon16;
      24: Result := ilIcon24;
      48: Result := ilIcon48;
      64: Result := ilIcon64;
    else  Result := ilIcon32;
  end;
end;

function TDM.GetHImageList(aSize: Integer = -1): TImageList;
begin
  case iif(aSize < 0, MForm.MenuIcoSize, aSize) of
       0: Result := nil;
       8: Result := ilIcon08h;
      12: Result := ilIcon12h;
      16: Result := ilIcon16h;
      24: Result := ilIcon24h;
      48: Result := ilIcon48h;
      64: Result := ilIcon64h
    else  Result := ilIcon32h;
  end;
end;

function TDM.GetDImageList(aSize: Integer = -1): TImageList;
begin
  case iif(aSize < 0, MForm.MenuIcoSize, aSize) of
       0: Result := nil;
       8: Result := ilIcon08d;
      12: Result := ilIcon12d;
      16: Result := ilIcon16d;
      24: Result := ilIcon24d;
      48: Result := ilIcon48d;
      64: Result := ilIcon64d;
    else  Result := ilIcon32d;
  end;
end;

procedure TDM.GetStatus(Sender: TObject);
const AtMinute = 24*60{*60{};

var q: byte;
    s: string;
    t: Integer;
begin
  if CommPortUsed then
    begin
      if CommPort.Connected then q:=CommPort.GetState
                            else q:=10;
      if (CommPort.ErrorConnected=0)
               then s:=GetTitle('_dE.port1') else
      if (CommPort.ErrorConnected<0) or
        (q=10) then s:=GetTitle('_dE.port2') else
      if q=112 then s:=GetTitle('_dE.port0') else
      if q=96  then s:=GetTitle('_dE.port3') else
      if q=32  then s:=GetTitle('_dE.port4') else
      if q=0   then s:=GetTitle('_dE.port5') else
                    s:=GetTitle('_dE.error')+Format(' #%d/%d',[q,CommPort.ErrorConnected]);
//      SBar.Panels[0].Text:=s;
    end;


  if Assigned(UserSession) and (Variables.AsInteger[RegLockSession] = 1) then
    begin
      t:=Round((Time-LastActionTime)*AtMinute);

      // период ожидания не превышен
      if (t<Variables.AsInteger[RegLockTime]*0.75) then
        begin
          ShowActionMessage := amWait;
        end;

      // период ожидания почти превышен - выводим сообщение
      if (t>Variables.AsInteger[RegLockTime]*0.75) and
         (ShowActionMessage=amWait) then
        begin
          SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, 38, NativeInt(PChar('_Dl.LockSession')));
          ShowActionMessage := amMessage;
        end;

      // период ожидания превышен - закрываем сессию
      if (t>Variables.AsInteger[RegLockTime])and(ShowActionMessage <> amLock) then
        begin
          ShowActionMessage:=amLock;
          MForm.Go_Da_UnregUser.Execute;
        end;
      // Application.MainForm.Caption:=IntToStr(Round(AtMinute*(Time-LastActionTime)));
    end
  else
    ShowActionMessage := amWait;     
end;
//------------------------------------------------------------------------------
procedure TDM.CommStart(Sender: TObject);
begin
  CommPort.Port         := Variables.AsByte[RegPort];
  CommPort.PortSpeed    := TPortBaudRate(Variables.AsByte[RegSpeed]);
  CommPort.PortParity   := TPortParity(Variables.AsByte[RegOdd]);
  CommPort.PortStopBits := TPortStopBits(Variables.AsByte[RegEndBits]);
  CommPort.PortDataBits := Variables.AsByte[RegBit];
  CommPort.EndByte      := Variables.AsByte[RegStop];
  CommPort.Connect;

  CommPortUsed := True;
  GetStatus(Sender);
end;
//------------------------------------------------------------------------------
procedure TDM.CommStop(Sender: TObject);
begin
  CommPort.Disconnect;

  CommPortUsed := False;
  GetStatus(Sender);
end;

//------------------------------------------------------------------------------

procedure TDM.AfterConnect;
begin

end;

procedure TDM.BeforeUnconnect;
begin

end;

//------------------------------------------------------------------------------
procedure TDM.ComboBoxKeyPress(Sender: TObject; var Key: Char);
var i : Integer;
    s : String;
begin
  if not (Sender is TComboBox) then Exit;

  for i:=1 to TComboBox(Sender).Items.Count-1 do
  begin s:=TComboBox(Sender).Items[i];
        if {Ansi}UpperCase(s[Pos('=',s)+1])>={Ansi}UpperCase(Key)
        then begin TComboBox(Sender).ItemIndex:=i;
                   Break;
             end;
  end;
end;
//------------------------------------------------------------------------------
procedure TDM.ComboBoxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var x   : Integer;
    Box : TComboBox;
    s, tmpS   : string;
    pStr : pChar;
begin
  Box:=TComboBox(Control);

  if odComboBoxEdit in State then x:=Rect.Left+2 else x:=Rect.Left+1;

  s:=Box.Items[Index];
//  s:=Copy(s,Pos('=',s)+1,MaxInt);
  pStr := pChar(s);
  tmpS := AnsiExtractQuotedStr(pStr, DeDelimeter);
  pStr := CharNext(pStr);
  tmpS := AnsiExtractQuotedStr(pStr, DeDelimeter);
  if Copy(tmpS,1,1)='_' then
    tmpS:=GetTitle(tmpS);

  if (Index=0) and (tmpS=' ***') then Box.Canvas.Font.Color:=clRed;
  Box.Canvas.TextRect(Rect, x, Rect.Top, tmpS);
end;
//------------------------------------------------------------------------------
function GetDOSEnvVar(const VarName: string): string;
var
  i: integer;
begin
  Result := EmptyStr;
  try
    i := GetEnvironmentVariable(PChar(VarName), nil, 0);
    if i > 0 then
    begin
      SetLength(Result, i);
      GetEnvironmentVariable(Pchar(VarName), PChar(Result), i);
    end;
  except
    Result := EmptyStr;
  end;
end;

{$IFDEF WIN64}
procedure TDM.GetUpdateInfo(aHandle: HWND);
begin
  FAppUpdateThread := TUpdateThread.Create(AHandle);
  FAppUpdateThread.OnTerminate := DoneAppUpdateThread;
  FAppUpdateThread.Start;
end;

procedure TDM.DoneAppUpdateThread(Sender: TObject);
begin
  FAppUpdateThread := nil;
end;
{$ENDIF}

function TDM.AppCurVersionText: string;
begin
  Result := FileVersionToString(AppVersion, True);
end;

function TDM.AppNewVersionText: string;
begin
  Result:= FileVersionToString(AppUpdate, True);
end;

function TDM.AppUpdateStatus: string;
begin
  case AppUpdateCode of
    auNull: Result:= EmptyStr;
    auProgress: Result:= Format(GetTitle('_dL.receivingdata'), [urlHome]);
    auCurrent: Result:= GetTitle('_dM.updateprono');
    auNew:  Result:= Format(GetTitle('_dM.updateproavaible'), [AppNewVersionText]);
    auOld:  Result:= GetTitle('_dM.updateproold');
  else
    Result:= GetTitle('_eRror.updateitem');
  end;
end;

function TDM.DoUpdateApplication(const aQuery: Boolean): Boolean;
var sDir, sUpdate, sBack, sAppName : String;
    ErrorCode: Integer;
begin
  Result:= True;
  case AppUpdateCode of
    auNull:;
    auProgress:
      SendMessage( Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(AppUpdateStatus)));
    auCurrent:
      SendMessage( Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(AppUpdateStatus)));
    auNew:
      begin
        Variables.AsString[RegLastUpdateMess]:= AppNewVersionText;

        if (aQuery = False) or
           (Application.MessageBox( pChar(GetTitle('_dM.queryupdateapp')),
                                    pChar(Format(GetTitle('_dM.updateproavaible'), [AppNewVersionText])),
                                    MB_ICONEXCLAMATION or MB_YESNO) = idYes) then
          try
            sAppName:= ExtractFileName(Application.ExeName);
            sDir:= ExtractFileDir(Application.ExeName);

            sBack:=    sDir + PathDelim + AppCurVersionText + '_' + sAppName;
            sUpdate:=  sDir + PathDelim + 'update_' + sAppName;
            if fileExists(sUpdate) then DeleteFile(sUpdate);

            if GetInetFile(AppUpdateURL, sUpdate) then
              begin

                ErrorCode:= ShellExecute(0, nil, PChar(sUpdate), '', nil, SW_SHOWNORMAL);

                if HINSTANCE_ERROR < ErrorCode then
                  begin
                    Application.Minimize;
                    MoveFile(PChar(Application.ExeName), PChar(sBack));
                    MoveFile(PChar(sUpdate), PChar(Application.ExeName));
                    //Synchronize(
                    Application.MainForm.Close
                    //);
                  end
                else
                  begin
                    if fileExists(sUpdate) then DeleteFile(sUpdate);
                  end;
              end
            else
              begin
                if fileExists(sUpdate) then DeleteFile(sUpdate);
              end;

          except
            SendMessage( Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar('_dL.procedureerror')));
          end;

      end
    else
      SendMessage( Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(AppUpdateStatus)));
  end;
end;

function TDM.LoadResourceImages(ImageList: TImageList; const ResourceName: string): Integer;
var
  Icon: TIcon;
begin
  Result := -1;
  if Assigned(ImageList) then
    begin
      Icon := TIcon.Create;
      try
        Icon.Handle := LoadImage(hInstance, PChar(ResourceName),  IMAGE_ICON, ImageList.Width, ImageList.Height, LR_CREATEDIBSECTION or LR_SHARED);
        if Icon.Handle <> 0 then
          Result := ImageList.AddIcon(Icon);
      finally
        Icon.Free;
      end;
    end;
end;

function EnumIconGroupResources(Module: HMODULE; ResType, ResName: PChar; Data: Pointer): BOOL; stdcall;
var
  Index: Integer;
  Name: string;
begin
  Result := True;
  if Integer(Pointer(ResName)) > 65535  then
    begin
      Name := StrPas(ResName);
      if Assigned(Data) and (Length(Name) = 3) and (Name[1] = 'P') then
        if TryStrToInt('$' + Copy(Name, 2, 2), Index) then
          TStrings(Data).AddObject(ResName, Pointer(Index));
    end;
end;

(*
function TDM.LoadResourceIcon(const ResourceName: string; const ImageIndex: Integer): Integer;
begin
  Result := LoadIconGroupToImageLists(ResourceName,
    [ilIcon16, ilIcon32, ilIcon48, ilIcon64],
    [ilIcon16h, ilIcon32h, ilIcon48h, ilIcon64h],
    [ilIcon16d, ilIcon32d, ilIcon48d, ilIcon64d]);
  if Result <> -1 then
    begin
      if ImageIndex <> -1 then
        FIconIndexes.Append(Format('%d=%d', [ImageIndex, Result]));
    end
  else
    begin
      {$IFDEF DEBUG}
      Funcs.WriteLog('Не удалось добавить иконку из ресурса %s с индексом %d.', [QuotedStr(ResourceName), ImageIndex], True, 'icons');
      {$ENDIF}
    end;
end;
*)

procedure TDM.LoadResourceIcons;
//var
//  Strings: TStrings;
//  Index, ImageIndex: Integer;
//  ResourceName: string;
begin
//  Strings := TStringList.Create;
//  try
//    Strings.Capacity := 256;
//    Win32Check(EnumResourceNames(hInstance, RT_GROUP_ICON, @EnumIconGroupResources, NativeInt(Strings)));
//    if not Assigned(FIconIndexes) then FIconIndexes := TStringList.Create;
    if not Assigned(FIconManager) then
      FIconManager := TIconManager.Create
        (
          [ilIcon08,  ilIcon12,  ilIcon16,  ilIcon24,  ilIcon32,  ilIcon48,  ilIcon64],
          [ilIcon08h, ilIcon12h, ilIcon16h, ilIcon24h, ilIcon32h, ilIcon48h, ilIcon64h],
          [ilIcon08d, ilIcon12d, ilIcon16d, ilIcon24d, ilIcon32d, ilIcon48d, ilIcon64d],
          SchemaColors
        );
    //FBaseIconCount := Strings.Count;
    FBaseIconCount := FIconManager.LoadIcons;
//    for Index := 0 to Pred(Strings.Count) do
//      begin
//        ResourceName := Strings[Index];
//        ImageIndex := Integer(Strings.Objects[Index]);
//        LoadResourceIcon(ResourceName, ImageIndex);
//      end;
//  finally
//    Strings.Free;
//  end;
end;

(*
function TDM.MapIconIndex(const ImageIndex: Integer): Integer;
const
  errorIndex = 111+256*3;  // серая буква 'i'
var
  OverlayPosition: Byte;
  OverlayIndex: Integer;
  function MapOneIconIndex(const ImageIndex: Word): Integer;
  var
  IconIndex, RealIndex: Integer;
  SchemaColor: TSchemaColor;
  begin
    Result := StrToIntDef(FIconIndexes.Values[IntToStr(ImageIndex)], -1);
    if Result = -1 then
      begin
        SchemaColor := HiByte(ImageIndex) and MaskSchemaColor;
        // Если не базовая (не из ресурсов), то ...
        if SchemaColor <> 0 then
          begin
            IconIndex := LoByte(ImageIndex);
            RealIndex := StrToIntDef(FIconIndexes.Values[IntToStr(IconIndex)], -1);
            if RealIndex <> -1 then
              begin
                Result := CopyShemaIconToImageLists(RealIndex, SchemaColor,
                  [ilIcon16, ilIcon32, ilIcon48, ilIcon64],
                  [ilIcon16h, ilIcon32h, ilIcon48h, ilIcon64h],
                  [ilIcon16d, ilIcon32d, ilIcon48d, ilIcon64d]);
                FIconIndexes.Values[IntToStr(IconIndex + SchemaColor * 256)] := IntToStr(Result);
                {$IFDEF DEBUG}
                //DebugIconIndexesLog('CopyShemaIconToImageLists for ' + IntToStr(ImageIndex) + '%s ...');
                {$ENDIF}
              end;
          end;
      end;
  end;
begin
  if Assigned(FIconIndexes) and (ImageIndex <> -1) then
    begin
      Result := StrToIntDef(FIconIndexes.Values[IntToStr(ImageIndex)], -1);

      if ImageIndex<0 then
        Exit(MapIconIndex(errorIndex));

      if Result = -1 then
        begin
          Result := MapOneIconIndex(LoWord(ImageIndex));
          if Result <> -1 then
            begin
              // Если нужна инверсия базовой иконки, то ...
              if (HiWord(ImageIndex) and $4000) <> 0 then
                begin
                  // Признак инверсии + номер базовой иконки + цветовая схема базовой иконки
                  OverlayIndex := ImageIndex and $40000FFF;
                  OverlayIndex := StrToIntDef(FIconIndexes.Values[IntToStr(OverlayIndex)], -1);
                  if OverlayIndex <> -1 then
                    Result := OverlayIndex
                  else
                    begin
                      Result := CopyInvertIconToImageLists(Result,
                        [ilIcon16, ilIcon32, ilIcon48, ilIcon64],
                        [ilIcon16h, ilIcon32h, ilIcon48h, ilIcon64h],
                        [ilIcon16d, ilIcon32d, ilIcon48d, ilIcon64d]);
                      FIconIndexes.Values[IntToStr(ImageIndex and $40000FFF)] := IntToStr(Result);
                     {$IFDEF DEBUG}
                     //DebugIconIndexesLog('CopyInvertIconToImageLists for ' + IntToStr(ImageIndex) + '%s ...');
                     {$ENDIF}
                    end;
                end;
              if (Result <> -1) and ((HiWord(ImageIndex) and $0FFF) <> 0) then
                begin
                  OverlayIndex := MapOneIconIndex(HiWord(ImageIndex));
                  if OverlayIndex <> -1 then
                    begin
                      OverlayPosition := HiByte(LoWord(ImageIndex)) shr 4;
                      if OverlayPosition in [0..12] then
                         begin
                           Result := CopyOverlayIconToImageLists(Result, OverlayIndex,
                            [ilIcon16, ilIcon32, ilIcon48, ilIcon64],
                            [ilIcon16h, ilIcon32h, ilIcon48h, ilIcon64h],
                            [ilIcon16d, ilIcon32d, ilIcon48d, ilIcon64d],
                            OverlayPosition);
                           FIconIndexes.Values[IntToStr(ImageIndex)] := IntToStr(Result);
                           {$IFDEF DEBUG}
                           //DebugIconIndexesLog('CopyOverlayIconToImageLists for ' + IntToStr(ImageIndex) + '%s ...');
                           {$ENDIF}
                         end
                      else
                        Result := -1;
                    end
                  else
                    Result := -1;
                end
              else
                FIconIndexes.Values[IntToStr(ImageIndex)] := IntToStr(Result);

            end;
          if (Result = -1) and (ImageIndex <> errorIndex) then
            Result := MapIconIndex(errorIndex);
        end;
    end
  else
    Result := -1;
end;
*)

function TDM.MapIconIndex(const ImageIndex: Integer): Integer;
const
  errorIndex = 111+256*3;  // серая буква 'i'
begin
  if Assigned(FIconManager) and (ImageIndex <> -1) then
    try
     {$IFDEF DEBUG}
     //DebugIconIndexesLog(Format('%s.MapIconIndex(%d) before mapping', [ClassName, ImageIndex]) + '%s ...');
     {$ENDIF}
      if ImageIndex  < 0 then
        Result := FIconManager.MapIcon(errorIndex)
      else
        Result := FIconManager.MapIcon(ImageIndex);
     {$IFDEF DEBUG}
     //DebugIconIndexesLog(Format('%s.MapIconIndex(%d) after mapping and return %d', [ClassName, ImageIndex, Result]) + '%s ...');
     {$ENDIF}
    except
     on E: Exception do
       begin
         Result := -1;
         {$IFDEF DEBUG}
         DebugLog('%s.MapIconIndex(%d) skip mapping exception: %s', [ClassName, ImageIndex, E.Message]);
         {$ENDIF}
       end;
    end
  else
    Result := -1;
end;

procedure TDM.CoordinateChanged(Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D);
begin
  FLatitude:= NewLocation.Latitude;
  FLongitude:= NewLocation.Latitude;
end;

function TDM.MyCoordinate(var aLatitude, aLongitude: Double): Boolean;
begin
  if LocationSensor.active and not varIsEmpty(FLatitude) and not varIsEmpty(FLongitude) then
    begin
      aLatitude:= FLatitude;
      aLongitude:= FLongitude;
      Result:= True;
    end
  else
    result:= False;
end;

function TDM.OpenSSL: Boolean;
var
  Handle: THandle;
begin
  // Если нет детектора библиотек SSL, то создадим его ...
  if not Assigned(FLibrarySSL) then
    FLibrarySSL := TOpenSSL.Create;

  // Если все библиотеки есть и их можно загрузить, то вернём True ...
  Result := FLibrarySSL.Loaded;

  // Если библиотеки не загружаются, то попытаемся их установить ...
  if not Result then
    if FLibrarySSL.Install then
      Result := True
    else
      FreeAndNil(FLibrarySSL); // Если не удалось установить - разрушим детектор до следующего вызова данной функции!!!
end;

function TDM.UnMapIconIndex(const ImageIndex: Integer): Integer;
//var
//  Index: Integer;
begin
  if Assigned(FIconManager) and (ImageIndex <> -1) then
    Result := FIconManager.UnMapIcon(ImageIndex)
  else
    Result := -1;
  {
  Result := -1;
  if Assigned(FIconIndexes) and (ImageIndex <> -1) then
    for Index := 0 to Pred(FIconIndexes.Count) do
      if StrToIntDef(FIconIndexes.ValueFromIndex[Index], -1) = ImageIndex then
        begin
          Result := StrToIntDef(FIconIndexes.Names[Index], -1);
          Break;
        end;
  }
end;

procedure TDM.MapActionListIcons(ActionList: TActionList);
var
  Index, FromIndex, ToIndex: Integer;
  {$IFDEF DEBUG}
  Value, ActionName, ActionCaption: string;
  {$ENDIF}
begin
  if Assigned(ActionList) then
    begin
      {$IFDEF DEBUG}
      Value := EmptyStr;
      {$ENDIF}
      for Index := 0 to Pred(ActionList.ActionCount) do
        begin
          FromIndex := ActionList[Index].ImageIndex;
          ToIndex := MapIconIndex(FromIndex);
          ActionList[Index].ImageIndex := ToIndex;
          {$IFDEF DEBUG}
          ActionName := Trim(Copy(ActionList[Index].Name, 1, 64));
          if Length(ActionName) = 0 then ActionName := '$' + IntToHex(Integer(ActionList[Index]), 8);
          ActionCaption := Trim(Copy(ActionList[Index].Caption, 1, 80));
          Value := Value + Format(#13#10'                        ¦ %3d. ¦ %11d ¦ %11d ¦ %-64s ¦ %-80s ¦',
            [Index, FromIndex, ToIndex, ActionName, ActionCaption]);
          {$ENDIF}
        end;
      {$IFDEF DEBUG}
      if Length(Value) <> 0 then
        begin
          Value := Format(#13#10'                        -%sT%sT%sT%sT%s¬',
            [
            DupeString('-', 6),DupeString('-', 13), DupeString('-', 13),
            DupeString('-', 66), DupeString('-', 82)
            ]) +
            Format(#13#10'                        ¦  No  ¦ %-11s ¦ %-11s ¦ %-64s ¦ %-80s ¦',
            [
            'Old Index', 'New Index', 'Name', 'Caption'
            ]) +
            Format(#13#10'                        +%s+%s+%s+%s+%s+',
            [
            DupeString('-', 6),DupeString('-', 13), DupeString('-', 13),
            DupeString('-', 66), DupeString('-', 82)
            ]) + Value +
            Format(#13#10'                        L%s+%s+%s+%s+%s-',
            [
            DupeString('-', 6),DupeString('-', 13), DupeString('-', 13),
            DupeString('-', 66), DupeString('-', 82)
            ]);
          ActionName := ActionList.Name;
          if Length(ActionName) = 0 then ActionName := '$' + IntToHex(Integer(ActionList), 8);
          if Assigned(ActionList.Images) then
            begin
              ActionCaption := ActionList.Images.Name;
              if Length(ActionCaption) = 0 then ActionCaption := '$' + IntToHex(Integer(ActionList.Images), 8);
              ActionName := ActionName + ' linked image list ' + ActionCaption;
            end;
          DebugLog('Action list ' + ActionName + ' icon mapped ...' + Value);
        end;
      {$ENDIF}
    end;
end;

{$IFDEF DEBUG}
procedure TDM.DebugIconIndexesLog(const Text: string);
const
  cImageIndexesFile = 'ImageIndexes';
  function PrepareIconIndexes: string;
  const
    Colors: array[TSchemaColor] of PChar =
      (
        'None',
        'Black',
        'White',
        'Gray',
        '30°',
        '60°',
        '90°',
        '120°',
        '150°',
        '180°',
        '210°',
        '240°',
        '270°',
        '300°',
        '330°',
        '360°'
      );
  var
    Index, IndexSize: Integer;
    OriginalIndex, MappedIndex: Integer;
    BaseIndex, AddIndex: Byte;
    OriginalString, MappedString, InverseString, BaseColorString: string;
    BaseIndexString, AddIndexString, AddColorString, OverlayString: string;
  begin
    Result := EmptyStr;
    if Assigned(FIconManager) then
      begin
        IndexSize := Length(IntToStr(FIconManager.MapList.Count));
        for Index := 0 to Pred(FIconManager.MapList.Count) do
          begin
            InverseString := EmptyStr;
            BaseColorString := EmptyStr;
            BaseIndexString := EmptyStr;
            AddColorString := EmptyStr;
            AddIndexString := EmptyStr;
            OverlayString := EmptyStr;
            OriginalIndex := FIconManager.MapList[Index].LogicalIndex;
            OriginalString := IntToStr(OriginalIndex);
            if (HiWord(OriginalIndex) and $4000) <> 0 then
              InverseString := '+';
            BaseIndex := LoByte(LoWord(OriginalIndex));
            BaseIndexString := IntToStr(BaseIndex);
            BaseColorString := StrPas(Colors[HiByte(LoWord(OriginalIndex)) and $0F]);
            AddIndex := LoByte(HiWord(OriginalIndex));
            if AddIndex <> 0 then
              begin
                AddIndexString := IntToStr(AddIndex);
                AddColorString := StrPas(Colors[HiByte(HiWord(OriginalIndex)) and $0F]);
                OverlayString := IntToStr((HiByte(LoWord(OriginalIndex)) shr 4) and $0F);
              end;
            MappedIndex := FIconManager.MapList[Index].RealIndex;
            MappedString := IntToStr(MappedIndex);
            Result := Result + Format(#13#10'                        ¦ %*d. ¦ %11s ¦ %11s ¦ %1s ¦ %5s ¦ %3s ¦ %5s ¦ %3s ¦ %2s ¦',
              [
                IndexSize, Index, MappedString, OriginalString,
                InverseString, BaseColorString, BaseIndexString,
                AddColorString, AddIndexString, OverlayString
              ]);
          end;
        if Length(Result) <> 0 then
          begin
            Result :=
              Format(#13#10'                        -%sT%sT%sT---T%sT%sT%sT%sT%s¬', [
                DupeString('-', IndexSize + 3),
                DupeString('-', 13),
                DupeString('-', 13),
                DupeString('-', 7),
                DupeString('-', 5),
                DupeString('-', 7),
                DupeString('-', 5),
                DupeString('-', 4)
                ]) +
              Format(#13#10'                        ¦ %*s ¦ %-11s ¦ %-11s ¦ I ¦ %-5s ¦ %3s ¦ %-5s ¦ %3s ¦ %2s ¦', [
                Succ(IndexSize), 'No',
                'Mapped',
                'Original',
                'BC',
                'BI',
                'AC',
                'AI',
                'OP'
                ]) +
              Format(#13#10'                        +%s+%s+%s+---+%s+%s+%s+%s+%s+', [
                DupeString('-', IndexSize + 3),
                DupeString('-', 13),
                DupeString('-', 13),
                DupeString('-', 7),
                DupeString('-', 5),
                DupeString('-', 7),
                DupeString('-', 5),
                DupeString('-', 4)
                ]) +
              Result +
              Format(#13#10'                        L%s+%s+%s+---+%s+%s+%s+%s+%s-', [
                DupeString('-', IndexSize + 3),
                DupeString('-', 13),
                DupeString('-', 13),
                DupeString('-', 7),
                DupeString('-', 5),
                DupeString('-', 7),
                DupeString('-', 5),
                DupeString('-', 4)
                ]);
          end;
      end;
  end;
begin
  WriteLog(Format(Text, [EmptyStr]) + PrepareIconIndexes, True, cImageIndexesFile);
  DebugLog(Text, [' saved to file ' + QuotedStr(cImageIndexesFile + sExtensionLog)]);
end;

procedure TDM.DebugDumpIconIndexes;
  procedure DumpIconIndexes;
  var
    FileName: string;
    Strings: TStrings;
    Index: Integer;
  begin
    FileName := LogDirectory + 'MapIconIndex.lst';
    if Assigned(FIconManager) then
      begin
        Strings := TStringList.Create;
        try
          for Index := 0 to Pred(FIconManager.MapList.Count) do
            Strings.Append(Format('%d=%d', [FIconManager.MapList[Index].LogicalIndex, FIconManager.MapList[Index].RealIndex]));
          Strings.SaveToFile(FileName);
        finally
          Strings.Free;
        end;
      end
    else
      if FileExists(FileName) then
        DeleteFile(FileName);
  end;
  procedure DumpImageList(ImageList: TImageList);
  var
    FileName: string;
    ColCount, RowCount, MaxColCount, ColIndex, RowIndex: Integer;
    Bitmap: TBitmap;
  begin
    if Assigned(ImageList) then
      begin
        FileName := ImageList.Name;
        if Length(FileName) = 0 then FileName := IntToHex(NativeInt(ImageList), SizeOf(NativeInt) * 2);
        FileName := LogDirectory + FileName + '.bmp';
        if FileExists(FileName) then DeleteFile(FileName);
        MaxColCount := 16384 div ImageList.Width;
        if ImageList.Count > MaxColCount then
          begin
            RowCount := ImageList.Count div MaxColCount;
            if (ImageList.Count mod MaxColCount) <> 0 then
              Inc(RowCount);
            ColCount := MaxColCount;
          end
        else
          begin
            ColCount := ImageList.Count;
            RowCount := 1;
          end;
        Bitmap := TBitmap.Create;
        try
          case ImageList.ColorDepth of
            cd4Bit: Bitmap.PixelFormat := pf4bit;
            cd8Bit: Bitmap.PixelFormat := pf8bit;
            cd16Bit: Bitmap.PixelFormat := pf16bit;
            cd24Bit: Bitmap.PixelFormat := pf24bit;
            cd32Bit: Bitmap.PixelFormat := pf32bit;
          end;
          Bitmap.Width := ColCount * ImageList.Width;
          Bitmap.Height := RowCount * ImageList.Height;
          Bitmap.Canvas.Brush.Color := clFuchsia; //ImageList.BkColor;
          Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));
          for RowIndex := 0 to Pred(RowCount) do
            for ColIndex := 0 to Pred(ColCount) do
              if (RowIndex * ColCount + ColIndex) >= ImageList.Count then
                Break
              else
                ImageList.Draw(Bitmap.Canvas, ColIndex * ImageList.Width, RowIndex * ImageList.Height, RowIndex * ColCount + ColIndex);
          Bitmap.SaveToFile(FileName);
        finally
          Bitmap.Free;
        end;
      end;
  end;
begin
  DumpIconIndexes;
  DumpImageList(ilIcon08);
  DumpImageList(ilIcon08d);
  DumpImageList(ilIcon08h);
  DumpImageList(ilIcon12);
  DumpImageList(ilIcon12d);
  DumpImageList(ilIcon12h);
  DumpImageList(ilIcon16);
  DumpImageList(ilIcon16d);
  DumpImageList(ilIcon16h);
  DumpImageList(ilIcon24);
  DumpImageList(ilIcon24d);
  DumpImageList(ilIcon24h);
  DumpImageList(ilIcon32);
  DumpImageList(ilIcon32d);
  DumpImageList(ilIcon32h);
  DumpImageList(ilIcon48);
  DumpImageList(ilIcon48d);
  DumpImageList(ilIcon48h);
  DumpImageList(ilIcon64);
  DumpImageList(ilIcon64d);
  DumpImageList(ilIcon64h);
  ShellExecute(0, nil, PChar(LogDirectory), nil, nil, SW_SHOWNORMAL);
end;
{$ENDIF}

{$REGION 'Startup & Shutdown Unit Runtime ...'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('DataUnit unit initialization ...');
  {$ENDIF}

  GetFileVersion(GetModuleName(hInstance), AppVersion);

  NullStrictConvert := False;
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DataUnit unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

