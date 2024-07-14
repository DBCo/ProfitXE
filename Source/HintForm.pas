unit HintForm;

interface

uses
  System.Types,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ExtCtrls, Buttons, StdCtrls, DeMetadata,
  ComCtrls, Math, Dialogs, DeTypes;

const
  defaultICO = icoInfo;
  NullDate = 1899-01-01;
type
  THFormStyle = (
    hsNone,          // нет маркера элемента управления
    hsTopLeft,       // маркер на верхней границе слева
    hsTopRight,      // маркер на верхней границе справа
    hsBottomLeft,    // маркер на нижней границе слева
    hsBottomRight,   // маркер на нижней границе справа
    hsLeftTop,       // маркер на левой границе сверху
    hsLeftBottom,    // маркер на левой границе снизу
    hsRightTop,      // маркер на правой границе сверху
    hsRightBottom    // маркер на правой границе снизу
  );

  TfrmHint = class(TForm)
    ShowPanel: TPanel;
    TextPanel: TPanel;
    TextLabel: TLabel;
    TimeLabel: TLabel;
    PanelT: TPanel;
    CaptionLabel: TLabel;
    CloseButton: TSpeedButton;
    PanelL: TPanel;
    Image: TImage;
    Bevel1: TBevel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure TextLabelDblClick(Sender: TObject);
  private
    { Private declarations }
    FHStyle   : THFormStyle;
    FRect     : TRect;
    FIco      : Integer;
    FMsgType  : TMsgDlgType;
    FTime     : TDateTime;
    FBndsRect : TRect;
    FBndsPnts : array[0..4] of TPoint;
    FRgnPnts  : array[0..2] of TPoint;
    procedure SetScreenBounds;
    function CalcRealWidth(const aStyle: THFormStyle): Integer;
    function CalcRealHeight(const aStyle: THFormStyle): Integer;
    function HCheck(const aStyle: THFormStyle; const aX: Integer): Boolean;
    function VCheck(const aStyle: THFormStyle; const aY: Integer): Boolean;
    procedure InitHint(const aRect: TRect; const aText, aCaption: string; const aIco: Integer = defaultICO; const aMsgType: TMsgDlgType = mtInformation; const aTime: TDateTime = NullDate);
    procedure SoftShow;
  public
    { Public declarations }
    procedure CopyToClipboard;
  //  property HStyle : THFormStyle read FHStyle write FHStyle;
  end;

  function ShowHintWindow(const aRect: TRect; const aText: string; const aCaption: string = ''; const aIco: Integer = 0; const aMsgType: TMsgDlgType = mtInformation): TfrmHint; overload;
  function ShowHintWindow(const aControl: TControl; const aText: string; const aCaption: string = ''; const aIco: Integer = icoInfo; const aMsgType: TMsgDlgType = mtInformation): TfrmHint; overload;
  function ShowHintWindow(const aPoint: TPoint; aMessage: TDeMessage): TfrmHint; overload;



////////////////////////////////////////////////////////////////////////////////////////////////////////

implementation

uses ClipBrd,
     DataUnit, Dictionary;

{$R *.dfm}

const
  cGap       = 9;   // зазор между каймой и центральной панелью
  cHeight    = 25;  // размер панели каймы (высота гор. и ширина верт.)
  cShift     = 15;  // отступ от края каймы до начала выступа
  cLength    = 15;  // длина основания выступа
  cMarker    = cHeight - cGap + cShift;  // расстояние от края формы до носика
  cBoundGap  = 4;   // минимально допустимый зазор между формой и краем экрана
  cFillColor = $00E1FFFF;  // цвет заливки формы

//==============================================================================

function ShowHintWindow(const aControl: TControl; const aText: string; const aCaption: string; const aIco: Integer; const aMsgType: TMsgDlgType) : TfrmHint;
var aRect : TRect;
    bControl: TControl;
begin
  // Если Control вложен на закладка, то переходим на закладку содержащую Control
  if Assigned(aControl) then
    begin
      bControl:= aControl;
      aRect.TopLeft     := bControl.ClientToScreen( Point(0,0) );
      aRect.BottomRight := bControl.ClientToScreen( Point(bControl.Width-1, bControl.Height-1) );

      while Assigned(bControl.Parent) do
        begin
          if bControl.Parent is TPageControl then
            begin
              TPageControl(bControl.Parent).ActivePage:= TTabSheet(bControl);
              Break;
            end;
          bControl:= bControl.Parent;
        end;
    end
  else
    begin
      aRect.TopLeft     := Application.MainForm.ClientToScreen( Point(0,0) );
      aRect.BottomRight := aRect.TopLeft;
    end;


  Result := TfrmHint.Create(Application.MainForm);
  Result.InitHint(aRect, aText, aCaption, aIco, aMsgType, NullDate);
  Result.SoftShow;
end;

function ShowHintWindow(const aRect: TRect; const aText: string; const aCaption: string; const aIco: Integer; const aMsgType: TMsgDlgType): TfrmHint;
begin
  Result := TfrmHint.Create(Application.MainForm);
  Result.InitHint(aRect, aText, aCaption, aIco, aMsgType, NullDate);
  Result.SoftShow;
end;

function ShowHintWindow(const aPoint: TPoint; aMessage: TDeMessage): TfrmHint;
begin
  Result := TfrmHint.Create(Application.MainForm);
  Result.InitHint(Rect(aPoint,aPoint), aMessage.Text, aMessage.Caption, aMessage.Ico, aMessage.MsgType, aMessage.Time);
  Result.SoftShow;
end;

//==============================================================================

function TfrmHint.CalcRealWidth(const aStyle: THFormStyle): Integer;
begin
  Result := ShowPanel.Width + 2 * ShowPanel.BorderWidth;
  if aStyle in [hsLeftTop, hsLeftBottom, hsRightTop, hsRightBottom] then
    Inc(Result, cHeight);
end;

function TfrmHint.CalcRealHeight(const aStyle: THFormStyle): Integer;
begin
  Result := ShowPanel.Height + 2 * ShowPanel.BorderWidth;
  if aStyle in [hsTopLeft, hsTopRight, hsBottomLeft, hsBottomRight] then
    Inc(Result, cHeight);
end;

function TfrmHint.HCheck(const aStyle: THFormStyle; const aX: Integer): Boolean;
begin
  result := True;
  // проверяет, влезает ли форма на экран по горизонтали
  case aStyle of
    hsTopLeft, hsBottomLeft:
      result := ((aX-cShift{cMarker}) > cBoundGap) and
                ((aX-{cMarker}cShift+{Width}CalcRealWidth(aStyle)) < (Screen.Width-cBoundGap));
    hsTopRight, hsBottomRight:
      result := ((aX-{Width}CalcRealWidth(aStyle)+{cMarker}cShift) > cBoundGap) and
                ((aX+{cMarker}cShift) < (Screen.Width-cBoundGap));
    hsLeftTop, hsLeftBottom:
      result := (aX + {Width}CalcRealWidth(aStyle)) > (Screen.Width-cBoundGap);
    hsRightTop, hsRightBottom:
      result := (aX - {Width}CalcRealWidth(aStyle)) > cBoundGap;
  end;
end;

function TfrmHint.VCheck(const aStyle: THFormStyle; const aY: Integer): Boolean;
begin
  result := True;
  // проверяет, влезает ли форма на экран по вертикали
  case aStyle of
    hsTopLeft, hsTopRight:
      result := (aY - {Height}CalcRealHeight(aStyle)) > cBoundGap;
    hsBottomLeft, hsBottomRight:
      result := (aY + {Height}CalcRealHeight(aStyle)) < (Screen.Height - cBoundGap);
    hsLeftTop, hsRightTop:
      result := ((aY-{cMarker}cShift) > cBoundGap) and
                ((aY-{cMarker}cShift+{Height}CalcRealHeight(aStyle)) < (Screen.Height - cBoundGap));
    hsLeftBottom, hsRightBottom:
      result := ((aY+{cMarker}cShift) < (Screen.Height - cBoundGap)) and
                ((aY+{cMarker}cShift-{Height}CalcRealHeight(aStyle)) > cBoundGap);
  end;
end;

procedure TfrmHint.SetScreenBounds;
var
  aLeft, aTop, aWidth, aHeight : integer;
  RGN1,RGN2 : hRGN;
begin
  // позиционирование формы относительно элемента редактирования
  aLeft := 0;  aTop := 0;
  aHeight := FRect.Bottom-FRect.Top+1;
  aWidth := FRect.Right-FRect.Left+1;
  // выбор положения формы относительно элемента редактирования
  FHStyle := hsNone;
  if (FRect.Top+3-{Height}CalcRealHeight(hsBottomRight)) > cBoundGap then
    begin
      // форму можно расположить выше элемента
      aTop := FRect.Top+3-Height;
      if HCheck(hsBottomRight, FRect.Left+3) or
         HCheck(hsBottomRight, FRect.Left+aWidth-3) then
        begin
          // уголок можно расположить снизу справа
          FHStyle := hsBottomRight;
          if HCheck(hsBottomRight, FRect.Left+3) then
            aLeft := FRect.Left+3-Width+cMarker
          else
            aLeft := {cBoundGap;}FRect.Left+aWidth-3-Width+cMarker;
        end
      else if HCheck(hsBottomLeft, FRect.Left+3) or
              HCheck(hsBottomLeft, FRect.Left+aWidth-3) then
        begin
          // уголок можно расположить снизу слева
          FHStyle := hsBottomLeft;
          if HCheck(hsBottomLeft, FRect.Left+3) then
            aLeft := FRect.Left-cMarker
          else
            aLeft := {cBoundGap;}FRect.Left+aWidth-3-cMarker-3;
        end
    end;
  if (FHStyle = hsNone) and
     ((FRect.Top+aHeight-1+{Height}CalcRealHeight(hsTopRight)) < (Screen.Height-cBoundGap)) then
    begin
      // форму можно расположить ниже элемента
      aTop := FRect.Top+aHeight-4;
      if HCheck(hsTopRight, FRect.Left+3) or
         HCheck(hsTopRight, FRect.Left+aWidth-3) then
        begin
          // уголок можно расположить сверху справа
          FHStyle := hsTopRight;
          if HCheck(hsTopRight, FRect.Left+3) then
            aLeft := FRect.Left+3-Width+cMarker
          else
            aLeft := {cBoundGap;}FRect.Left+aWidth-3-Width+cMarker;
        end
      else if HCheck(hsTopLeft, FRect.Left+3) or
              HCheck(hsTopLeft, FRect.Left+aWidth-3) then
        begin
          // уголок можно расположить сверху слева
          FHStyle := hsTopLeft;
          if HCheck(hsTopLeft, FRect.Left+3) then
            aLeft := FRect.Left-cMarker
          else
            aLeft := {cBoundGap;}FRect.Left+aWidth-3-cMarker-3;
        end
    end;
  if (FHStyle = hsNone) and ((FRect.Left+3-{Width}CalcRealWidth(hsRightBottom)) > cBoundGap) then
    begin
      // форму можно расположить слева от элемента
      aLeft := FRect.Left+3-Width;
      if VCheck(hsRightBottom, FRect.Top+3) or
         VCheck(hsRightBottom, FRect.Top+aHeight-3)
        then
          begin
            // уголок можно расположить справа снизу
            FHStyle := hsRightBottom;
            if VCheck(hsRightBottom, FRect.Top+3) then
              aTop := FRect.Top+3-Height+cMarker
            else
              aTop := {cBoundGap;}FRect.Top+aHeight-3-Height+cMarker;
          end
      else if VCheck(hsRightTop, FRect.Top+3) or
              VCheck(hsRightTop, FRect.Top+aHeight-3) then
        begin
          // уголок можно расположить справа сверху
          FHStyle := hsRightTop;
          if VCheck(hsRightTop, FRect.Top+3) then
            aTop := FRect.Top+3-cMarker
          else
            aTop := {cBoundGap;}FRect.Top+aHeight-3-cMarker;
        end
    end;
  if (FHStyle = hsNone) and
     ((FRect.Left+aWidth-1+{Width}CalcRealWidth(hsLeftBottom)) < (Screen.Width-cBoundGap)) then
    begin
      // форму можно расположить справа от элемента
      aLeft := FRect.Left+aWidth-4;
      if VCheck(hsLeftBottom, FRect.Top+3) or
         VCheck(hsLeftBottom, FRect.Top+aHeight-3) then
        begin
          FHStyle := hsLeftBottom;
          if VCheck(hsLeftBottom, FRect.Top+3) then
            aTop := FRect.Top+3-Height+cMarker
          else
            aTop := {cBoundGap;}FRect.Top+aHeight-3-Height+cMarker;
        end
      else if VCheck(hsLeftTop, FRect.Top+3) or
              VCheck(hsLeftTop, FRect.Top+aHeight-3) then
        begin
          FHStyle := hsLeftTop;
          if VCheck(hsLeftTop, FRect.Top+3) then
            aTop := FRect.Top+3-cMarker
          else
            aTop := {cBoundGap;}FRect.Top+aHeight-3-cMarker;
        end;
    end;
  if FHStyle = hsNone then
    begin
      // установки границ по умолчанию
      FHStyle := hsTopLeft;
      aLeft := FRect.Left+cBoundGap;
      aTop := FRect.Top+cBoundGap;
    end;
  SetBounds(aLeft, aTop, Width, Height);
  {}
  FBndsRect := Classes.Rect(cHeight-cGap-1,
                    cHeight-cGap-1,
                    ClientWidth-cHeight+cGap+1,
                    ClientHeight-cHeight+cGap+1);
  case FHStyle of
    hsNone        : begin // нет маркера элемента управления
    end;
    hsTopLeft     : begin // маркер на верхней границе слева
      FBndsPnts[0] := Point(cHeight-1-cGap+cShift, cHeight-1-cGap);
      FBndsPnts[1] := Point(cHeight-1-cGap+cShift, 0);
      FBndsPnts[2] := Point(cHeight-1-cGap+cShift+cLength, cHeight-1-cGap);
      FBndsPnts[3] := Point(FBndsPnts[2].X-1, FBndsPnts[2].Y);
      FBndsPnts[4] := Point(FBndsPnts[0].X, FBndsPnts[0].Y);
      FRgnPnts[0]  := Point(FBndsPnts[0].X-1,FBndsPnts[0].Y+1);
      FRgnPnts[1]  := Point(FBndsPnts[1].X-1,FBndsPnts[1].Y-2);
      FRgnPnts[2]  := Point(FBndsPnts[2].X+1,FBndsPnts[2].Y+1);
    end;
    hsTopRight    : begin // маркер на верхней границе справа
      FBndsPnts[0] := Point(ClientWidth-(cHeight-1-cGap+cShift), cHeight-1-cGap);
      FBndsPnts[1] := Point(ClientWidth-(cHeight-1-cGap+cShift), 0);
      FBndsPnts[2] := Point(ClientWidth-(cHeight-1-cGap+cShift+cLength), cHeight-1-cGap);
      FBndsPnts[3] := Point(FBndsPnts[2].X+1, FBndsPnts[2].Y);
      FBndsPnts[4] := Point(FBndsPnts[0].X, FBndsPnts[0].Y);
      FRgnPnts[0]  := Point(FBndsPnts[0].X+1,FBndsPnts[0].Y+1);
      FRgnPnts[1]  := Point(FBndsPnts[1].X+1,  FBndsPnts[1].Y-1);
      FRgnPnts[2]  := Point(FBndsPnts[2].X-1,FBndsPnts[2].Y+1);
    end;
    hsBottomLeft  : begin // маркер на нижней границе слева
      FBndsPnts[0] := Point(cHeight-1-cGap+cShift, ClientHeight-(cHeight-1-cGap)-1);
      FBndsPnts[1] := Point(cHeight-1-cGap+cShift, ClientHeight-1);
      FBndsPnts[2] := Point(cHeight-1-cGap+cShift+cLength, ClientHeight-(cHeight-1-cGap)-1);
      FBndsPnts[3] := Point(FBndsPnts[2].X-1, FBndsPnts[2].Y);
      FBndsPnts[4] := Point(FBndsPnts[0].X, FBndsPnts[0].Y);
      FRgnPnts[0]  := Point(FBndsPnts[0].X, FBndsPnts[0].Y-1);
      FRgnPnts[1]  := Point(FBndsPnts[1].X, FBndsPnts[1].Y+1);
      FRgnPnts[2]  := Point(FBndsPnts[2].X+1,FBndsPnts[2].Y-1);
    end;
    hsBottomRight : begin // маркер на нижней границе справа
      FBndsPnts[0] := Point(ClientWidth-(cHeight-1-cGap+cShift), ClientHeight-(cHeight-1-cGap)-1);
      FBndsPnts[1] := Point(ClientWidth-(cHeight-1-cGap+cShift), ClientHeight-1);
      FBndsPnts[2] := Point(ClientWidth-(cHeight-1-cGap+cShift+cLength), ClientHeight-(cHeight-1-cGap)-1);
      FBndsPnts[3] := Point(FBndsPnts[2].X+1, FBndsPnts[2].Y);
      FBndsPnts[4] := Point(FBndsPnts[0].X, FBndsPnts[0].Y);
      FRgnPnts[0]  := Point(FBndsPnts[0].X+1,FBndsPnts[0].Y-1);
      FRgnPnts[1]  := Point(FBndsPnts[1].X+1,FBndsPnts[1].Y+1);
      FRgnPnts[2]  := Point(FBndsPnts[2].X-1,FBndsPnts[2].Y-1);
    end;
    hsLeftTop     : begin // маркер на левой границе сверху
      FBndsPnts[0] := Point(cHeight-1-cGap, cHeight-1-cGap+cShift);
      FBndsPnts[1] := Point(0,
                            cHeight-1-cGap+cShift);
      FBndsPnts[2] := Point(cHeight-1-cGap, cHeight-1-cGap+cShift+cLength);
      FBndsPnts[3] := Point(FBndsPnts[2].X, FBndsPnts[2].Y-1);
      FBndsPnts[4] := Point(FBndsPnts[0].X, FBndsPnts[0].Y);
      FRgnPnts[0]  := Point(FBndsPnts[0].X+1,FBndsPnts[0].Y-1);
      FRgnPnts[1]  := Point(FBndsPnts[1].X-1,FBndsPnts[1].Y-1);
      FRgnPnts[2]  := Point(FBndsPnts[2].X+1,FBndsPnts[2].Y+1);
    end;
    hsLeftBottom  : begin // маркер на левой границе снизу
      FBndsPnts[0] := Point(cHeight-1-cGap, ClientHeight-(cHeight-1-cGap+cShift));
      FBndsPnts[1] := Point(0, ClientHeight-(cHeight-1-cGap+cShift));
      FBndsPnts[2] := Point(cHeight-1-cGap, ClientHeight-(cHeight-1-cGap+cShift+cLength));
      FBndsPnts[3] := Point(FBndsPnts[2].X, FBndsPnts[2].Y+1);
      FBndsPnts[4] := Point(FBndsPnts[0].X, FBndsPnts[0].Y);
      FRgnPnts[0]  := Point(FBndsPnts[0].X+1,FBndsPnts[0].Y+1);
      FRgnPnts[1]  := Point(FBndsPnts[1].X-1,FBndsPnts[1].Y+1);
      FRgnPnts[2]  := Point(FBndsPnts[2].X+1,FBndsPnts[2].Y-1);
    end;
    hsRightTop    : begin // маркер на правой границе сверху
      FBndsPnts[0] := Point(ClientWidth-(cHeight-1-cGap)-1, cHeight-1-cGap+cShift);
      FBndsPnts[1] := Point(ClientWidth-1, cHeight-1-cGap+cShift);
      FBndsPnts[2] := Point(ClientWidth-(cHeight-1-cGap)-1, cHeight-1-cGap+cShift+cLength);
      FBndsPnts[3] := Point(FBndsPnts[2].X, FBndsPnts[2].Y-1);
      FBndsPnts[4] := Point(FBndsPnts[0].X, FBndsPnts[0].Y);
      FRgnPnts[0]  := Point(FBndsPnts[0].X-1,FBndsPnts[0].Y-1);
      FRgnPnts[1]  := Point(FBndsPnts[1].X+2,FBndsPnts[1].Y-1);
      FRgnPnts[2]  := Point(FBndsPnts[2].X-1,FBndsPnts[2].Y+1);
    end;
    hsRightBottom : begin // маркер на правой границе снизу
      FBndsPnts[0] := Point(ClientWidth-(cHeight-1-cGap)-1, ClientHeight-(cHeight-1-cGap+cShift));
      FBndsPnts[1] := Point(ClientWidth-1, ClientHeight-(cHeight-1-cGap+cShift));
      FBndsPnts[2] := Point(ClientWidth-(cHeight-1-cGap)-1, ClientHeight-(cHeight-1-cGap+cShift+cLength));
      FBndsPnts[3] := Point(FBndsPnts[2].X, FBndsPnts[2].Y+1);
      FBndsPnts[4] := Point(FBndsPnts[0].X, FBndsPnts[0].Y);
      FRgnPnts[0]  := Point(FBndsPnts[0].X-1,FBndsPnts[0].Y+1);
      FRgnPnts[1]  := Point(FBndsPnts[1].X+2,FBndsPnts[1].Y+1);
      FRgnPnts[2]  := Point(FBndsPnts[2].X-1,FBndsPnts[2].Y-1);
    end;
  end;
  RGN1 := CreateRoundRectRgn(FBndsRect.Left, FBndsRect.Top,
                             FBndsRect.Right+1,FBndsRect.Bottom+1,
                             cGap,cGap);
  RGN2 := CreatePolygonRgn(FRgnPnts,3,WINDING);
  CombineRgn(RGN1,RGN1,RGN2,RGN_OR);
  setWindowRgn(Handle,RGN1,true);
  {}
end;

procedure TfrmHint.InitHint(const aRect: TRect; const aText, aCaption: string; const aIco: Integer; const aMsgType: TMsgDlgType; const aTime: TDateTime);
var
  s        : String;
  p,W,WWW  : Integer;
  sText    : string;
  Icon     : TIcon;
begin
  FIco     := aIco;
  FMsgType := aMsgType;
  FRect    := aRect;
  FTime    := aTime;

  Icon := TIcon.Create;
  try
    DM.ilIcon32.GetIcon(DM.MapIconIndex(FIco), Icon);
    Image.Picture.Assign(Icon);
  finally
    Icon.Free;
  end;

  if Length(aCaption) = 0 then CaptionLabel.Caption := GetTitle('_Dl.information')
                          else CaptionLabel.Caption := GetTitle(aCaption);

  if FTime > NullDate then
                        begin
                          if ( Trunc(FTime) = Trunc(Date) )  then
                            TimeLabel.Caption := FormatDateTime('hh:mm:ss', FTime)
                          else
                            TimeLabel.Caption := FormatDateTime('dd-MM-YYYY', FTime)+' '+FormatDateTime('hh:mm:ss', FTime)
                        end
                      else TimeLabel.Caption := EmptyStr;

  TimeLabel.Visible := (FTime > NullDate);
  Bevel1.   Visible := (FTime > NullDate);

  sText := GetTitle(aText);
  s:= sText;
  WWW:=0;
  repeat
    p:=pos(#10,s);
    if p = 0 then W := TextLabel.Canvas.TextWidth(s)
             else W := TextLabel.Canvas.TextWidth(Copy(s,1,p-1));
    if WWW < W then WWW:=W;
    Delete(s,1,p);
  until P=0;

  WWW := WWW + TextPanel.Left + (2*cHeight)+64;
  WWW := Min( WWW, (Screen.Width div 2));  // не более 1/2 ширины экрана
  WWW := Max( WWW, Width);
  Width:=WWW;

  TextLabel.Caption := sText;
  TextLabel.Width := TextPanel.ClientWidth;
  TextLabel.AutoSize := False;
  TextLabel.Align := alTop;
  TextLabel.AutoSize := True;
  WWW := TextLabel.Canvas.TextHeight('Xy') div 2; // Отступ по высоте в пол строки!
  if Bevel1.Visible then
    Height := max(TextLabel.Height, 2*Image.Top + Image.Height) + PanelT.Height + Bevel1.Height + TimeLabel.Height + (2*cHeight) + 8 + WWW
  else
    Height := max(TextLabel.Height, 2*Image.Top + Image.Height) + PanelT.Height                                    + (2*cHeight) + 8 + WWW;

  TextLabel.Align := alClient;
  SetScreenBounds;
end;

procedure TfrmHint.SoftShow;
var I : Integer;
begin
  if AlphaBlend then
    for I := 1 to 22 do
      begin
        AlphaBlendValue := I*10;
        BringToFront;
        visible:=True;
       if i=1 then repaint
               else update;

        sleep(15);
      end;
end;

procedure TfrmHint.FormCreate(Sender: TObject);
begin
  CloseButton.Glyph.LoadFromResourceName(hInstance, 'CROSS');
  Left:=MaxInt;
  FHStyle := hsNone;
  CaptionLabel.Font.Style:= [fsBold];
  TimeLabel.Font.Color:= clGrayText;
end;

procedure TfrmHint.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TfrmHint.FormPaint(Sender: TObject);
var
  BrdrColor : TColor;
  Kx,Ky : integer;
begin
  //почему-то слетают на Windows7
  ShowPanel.Left := 21;

  with Canvas do
  begin
    if FMsgType = mtError then BrdrColor := clRed
                          else BrdrColor := clDkGray;
    Pen.Width := 3;
    Pen.Color := BrdrColor;
    RoundRect(FBndsRect.Left,FBndsRect.Top,
              FBndsRect.Right,FBndsRect.Bottom,
              cGap,cGap);
    Pen.Color := Color;
    if FBndsPnts[4].X > FBndsPnts[3].X then Kx := 1
    else if FBndsPnts[4].X < FBndsPnts[3].X then Kx :=-1
    else Kx := 0;
    if FBndsPnts[4].Y > FBndsPnts[3].Y then Ky := 1
    else if FBndsPnts[4].Y < FBndsPnts[3].Y then Ky :=-1
    else Ky := 0;
    MoveTo(FBndsPnts[3].X + Kx,FBndsPnts[3].Y + Ky);
    LineTo(FBndsPnts[4].X - Kx,FBndsPnts[4].Y - Ky);

    Pen.Color := BrdrColor;
    MoveTo(FBndsPnts[0].X,FBndsPnts[0].Y);
    LineTo(FBndsPnts[1].X,FBndsPnts[1].Y);
    LineTo(FBndsPnts[2].X,FBndsPnts[2].Y);
  end;
end;

procedure TfrmHint.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) in [13,27,32] then Close;
end;

procedure TfrmHint.TextLabelDblClick(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TfrmHint.CopyToClipboard;
var
  Bitmap: TBitmap;
  Text: string;
  BitmapFormat: Word;
  DataHandle: THandle;
  PalleteHandle: HPALETTE;
  DataPtr: PAnsiChar;
  LocaleData: LongInt;
  DataText: AnsiString;
begin
  Text := TextLabel.Caption;
  if Length(Text) <> 0 then
    begin
      Clipboard.Open;
      try
        Clipboard.Clear;
        Bitmap := TBitmap.Create;
        try
          Bitmap.Width := Width;
          Bitmap.Height := Height;
          Bitmap.Canvas.Brush.Color := clWhite;
          Bitmap.Canvas.FillRect(Classes.Rect(0, 0, Bitmap.Width, Bitmap.Height));
          PaintTo(Bitmap.Canvas, 0, 0);
          Bitmap.SaveToClipboardFormat(BitmapFormat, DataHandle, PalleteHandle);
          Clipboard.SetAsHandle(BitmapFormat, DataHandle);
        finally
          Bitmap.Free;
        end;

        LocaleData := $0419;
        DataHandle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SizeOf(LocaleData));
        DataPtr := GlobalLock(DataHandle);
        Move(LocaleData, DataPtr^, SizeOf(LocaleData));
        GlobalUnlock(DataHandle);
        SetClipboardData(CF_LOCALE, DataHandle);

        DataText := Text;
        DataHandle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, Succ(Length(DataText)));
        DataPtr := GlobalLock(DataHandle);
        StrCopy(DataPtr, PAnsiChar(DataText));
        GlobalUnlock(DataHandle);
        SetClipboardData(CF_TEXT, DataHandle);

      finally
        Clipboard.Close;
      end;
    end;
end;

end.

