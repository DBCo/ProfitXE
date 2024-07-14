unit AnchorRuler;

interface

uses Controls, Classes, Graphics, Windows, Messages;

type
  TArrowDragged = (adNone, adLeft, adRight, adBoth);

  TAnchorRuler = class(TCustomControl)
  private
    FLeftAnchor: Integer;
    FRightAnchor: Integer;
    FRightAnchorBothDragged: Integer;
    FArrowDragged: TArrowDragged;
    FButtonPressed: Boolean;
    FLeftAnchorLowerLeftPoint: TPoint;
    FLeftAnchorLowerRightPoint: TPoint;
    FLeftAnchorArrowRect: TRect;
    FLeftAnchorArrowPoint: TPoint;
    FRightAnchorArrowRect: TRect;
    FRightAnchorArrowPoint: TPoint;
    FLeftMargin: Integer;
    FRightMargin: Integer;
    FOnChange: TNotifyEvent;
    procedure SetLeftAnchor(Value: Integer);
    procedure SetRightAnchor(Value: Integer);
    function GetAnchorArrowAreaNum(X: Integer): Integer;
    function GetAnchorArrowHorzPos(AnchorNum: Integer): Integer;
    procedure SetLeftAnchorArea;
    procedure SetRightAnchorArea;
    function PointInArea(P: TPoint): TArrowDragged;
    procedure CalcMargins;
  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Resized;
    procedure Update; override;
    property LeftAnchor: Integer read FLeftAnchor write SetLeftAnchor;
    property RightAnchor: Integer read FRightAnchor write SetRightAnchor;
    property onChange : TNotifyEvent read FOnChange write FOnChange;
    property ArrowDragged: TArrowDragged read FArrowDragged;
  end;

var
  AnchorRulerHeight: Integer;

implementation

uses
  System.Types,
  DeLog, Funcs, Meta, DeTypes, UnitA, Element;

const
  BarTopOffset = 4;
  BarBottomOffset = 6;
  BigBarTopOffset = 3;
  BigBarBottomOffset = 3;
  UpperStripHeight = 5;
  MainStripHeight = 13;
  LowerStripHeight = 6;
  AnchorArrowWidth = 8;
  AnchorArrowHeight = 7;
  LowerRectangleHeight = 6;
  MarginWidth = 2;

var
  AnchorArrowEdgeHeight: Integer;
  UpperArrowTop: Integer;

procedure TAnchorRuler.Paint;
var
  BarHorzPos: Integer;
//  AreaNum: Integer;
  BrightArrowColor, DarkArrowColor: TColor;
  i: Integer;
begin
  inherited;
  BrightArrowColor := ChangeBrightnessByDelta(clBtnFace,-90);
  DarkArrowColor := ChangeBrightnessByDelta(clBtnFace,-150);
  Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect(ClientRect);

  Canvas.Brush.Color := clAppWorkSpace;
  Canvas.FillRect(Rect(0,UpperStripHeight,
    FLeftMargin-MarginWidth,UpperStripHeight+MainStripHeight));
  Canvas.FillRect(Rect(FRightMargin+MarginWidth,UpperStripHeight, ClientWidth,UpperStripHeight+MainStripHeight));

  if Enabled then Canvas.Brush.Color := clWindow
  else Canvas.Brush.Color := ChangeBrightnessByDelta(clBtnFace,10);
  Canvas.FillRect(Rect(FLeftMargin,UpperStripHeight, FRightMargin,UpperStripHeight+MainStripHeight));

  Canvas.Pen.Color := clBlack;
//  AreaNum := GetAnchorArrowAreaNum(0);
//  BarHorzPos := GetAnchorArrowHorzPos(AreaNum);
//  while BarHorzPos < ClientWidth do
  for i := 1 to AnchorsCount - 2 do
  begin
//    if (AreaNum <> 0) and (AreaNum <> AnchorsCount - 1) then
    BarHorzPos := GetAnchorArrowHorzPos({AreaNum}i);
    if {AreaNum}i mod 3 = 0 then
    begin
      Canvas.MoveTo(BarHorzPos,UpperStripHeight+BigBarTopOffset);
      Canvas.LineTo(BarHorzPos,UpperStripHeight+MainStripHeight-BigBarBottomOffset);
    end
    else
    begin
      Canvas.MoveTo(BarHorzPos,UpperStripHeight+BarTopOffset);
      Canvas.LineTo(BarHorzPos,UpperStripHeight+MainStripHeight-BarBottomOffset);
    end;
//    Inc(AreaNum);
  end;

  Canvas.Brush.Color := clBtnFace;
  with FLeftAnchorArrowRect do
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.Pen.Color := DarkArrowColor;
    Canvas.Polygon([TopLeft,FLeftAnchorArrowPoint, Point(Right,Top),BottomRight,Point(Left,Bottom)]);
    Canvas.Polygon([Point(Left,Bottom),FLeftAnchorLowerLeftPoint, FLeftAnchorLowerRightPoint,BottomRight]);
    Canvas.Pen.Color := BrightArrowColor;
    if FButtonPressed and (FArrowDragged = adLeft) then
      Canvas.Polyline([FLeftAnchorLowerLeftPoint,Point(Left,Bottom), BottomRight,Point(Right,Top),FLeftAnchorArrowPoint])
    else if FButtonPressed and (FArrowDragged = adBoth) then
    begin
      Canvas.Polyline([FLeftAnchorLowerLeftPoint,FLeftAnchorLowerRightPoint, Point(Right,Top),FLeftAnchorArrowPoint]);
      Canvas.MoveTo(Right,Bottom);
      Canvas.LineTo(Left,Bottom);
    end
    else
      Canvas.Polyline([FLeftAnchorLowerLeftPoint,TopLeft, FLeftAnchorArrowPoint]);
  end;

  with FRightAnchorArrowRect do
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.Pen.Color := DarkArrowColor;
    Canvas.Polygon([TopLeft,Point(Right,Top),BottomRight, FRightAnchorArrowPoint,Point(Left,Bottom)]);
    Canvas.Pen.Color := BrightArrowColor;
    if FButtonPressed and (FArrowDragged = adRight) then
      Canvas.Polyline([Point(Right,Top),BottomRight, FRightAnchorArrowPoint])
    else
      Canvas.Polyline([FRightAnchorArrowPoint,Point(Left,Bottom), TopLeft,Point(Right,Top)]);
  end;
end;

procedure TAnchorRuler.Resized;
begin
  {if Assigned(Element) then }CalcMargins;
  SetLeftAnchorArea;
  SetRightAnchorArea;
end;

constructor TAnchorRuler.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := True;
  FLeftAnchor := -1;
  FRightAnchor := -1;
end;

procedure TAnchorRuler.SetLeftAnchor(Value: Integer);
begin
//  if Value = FLeftAnchor then Exit;
  if Value < 0 then Value := 0;
  if Value > AnchorsCount - 1 then Value := AnchorsCount - 1;
  if RightAnchor < Value then RightAnchor := Value;
  FLeftAnchor := Value;
  SetLeftAnchorArea;
  Invalidate;
end;

procedure TAnchorRuler.SetRightAnchor(Value: Integer);
begin
//  if Value = FRightAnchor then Exit;
  if Value < 0 then Value := 0;
  if Value > AnchorsCount - 1 then Value := AnchorsCount - 1;
  if LeftAnchor > Value then LeftAnchor := Value;
  FRightAnchor := Value;
  SetRightAnchorArea;
  Invalidate;
end;

function TAnchorRuler.GetAnchorArrowAreaNum(X: Integer): Integer;
var
  HalfStep: Integer;
//  LeftPoint, RightPoint: Integer;
begin
//  LeftPoint := FLeftMargin - 1;
//  RightPoint := FRightMargin;
  HalfStep := (FRightMargin - (FLeftMargin - 1)) div ((AnchorsCount - 1) * 2);
  Result := (X - (FLeftMargin - 1) + HalfStep) *  (AnchorsCount - 1) div (FRightMargin - (FLeftMargin - 1));
  if X - (FLeftMargin - 1) + HalfStep < 0 then Result := Result - 1;
end;

function TAnchorRuler.GetAnchorArrowHorzPos(AnchorNum: Integer): Integer;
//var
//  LeftPoint, RightPoint: Integer;
begin
//  LeftPoint := FLeftMargin - 1;
//  RightPoint := FRightMargin;
  Result := ((FLeftMargin - 1) * ((AnchorsCount - 1) - AnchorNum) + FRightMargin * AnchorNum) div (AnchorsCount - 1);
end;

procedure TAnchorRuler.SetLeftAnchorArea;
var
  ArrowHorzPos: Integer;
begin
  ArrowHorzPos := GetAnchorArrowHorzPos(FLeftAnchor);
  FLeftAnchorLowerLeftPoint := Point(ArrowHorzPos-AnchorArrowWidth div 2, AnchorRulerHeight-1);
  FLeftAnchorLowerRightPoint := Point(ArrowHorzPos+AnchorArrowWidth div 2, AnchorRulerHeight-1);
  FLeftAnchorArrowRect := Rect(ArrowHorzPos-AnchorArrowWidth div 2,
    AnchorRulerHeight-LowerRectangleHeight-AnchorArrowEdgeHeight-1,
    ArrowHorzPos+AnchorArrowWidth div 2,
    AnchorRulerHeight-LowerRectangleHeight-1);
  FLeftAnchorArrowPoint := Point(ArrowHorzPos, AnchorRulerHeight-LowerRectangleHeight-AnchorArrowHeight-1);
end;

procedure TAnchorRuler.SetRightAnchorArea;
var
  ArrowHorzPos: Integer;
begin
  ArrowHorzPos := GetAnchorArrowHorzPos(FRightAnchor);
  FRightAnchorArrowRect := Rect(ArrowHorzPos-AnchorArrowWidth div 2,
    UpperArrowTop,ArrowHorzPos+AnchorArrowWidth div 2,
    UpperArrowTop+AnchorArrowEdgeHeight);
  FRightAnchorArrowPoint := Point(ArrowHorzPos, UpperArrowTop+AnchorArrowHeight);
end;

function TAnchorRuler.PointInArea(P: TPoint): TArrowDragged;
begin
  Result := adNone;
  if (P.X >= FLeftAnchorArrowRect.Left) and
    (P.X <= FLeftAnchorArrowRect.Right) and
    (P.Y >= FLeftAnchorArrowRect.Top) and
    (P.Y <= FLeftAnchorArrowRect.Bottom) or
    (P.Y < FLeftAnchorArrowRect.Top) and
    (P.Y >= FLeftAnchorArrowPoint.Y) and
    (P.X >= FLeftAnchorArrowPoint.X - (P.Y - FLeftAnchorArrowPoint.Y) *
    AnchorArrowWidth div ((AnchorArrowHeight - AnchorArrowEdgeHeight) * 2)) and
    (P.X <= FLeftAnchorArrowPoint.X + (P.Y - FLeftAnchorArrowPoint.Y) *
    AnchorArrowWidth div ((AnchorArrowHeight - AnchorArrowEdgeHeight) * 2)) then
    Result := adLeft;
  if (P.X >= FRightAnchorArrowRect.Left) and
    (P.X <= FRightAnchorArrowRect.Right) and
    (P.Y >= FRightAnchorArrowRect.Top) and
    (P.Y <= FRightAnchorArrowRect.Bottom) or
    (P.Y > FRightAnchorArrowRect.Bottom) and
    (P.Y <= FRightAnchorArrowPoint.Y) and
    (P.X >= FRightAnchorArrowPoint.X - (FRightAnchorArrowPoint.Y - P.Y) *
    AnchorArrowWidth div ((AnchorArrowHeight - AnchorArrowEdgeHeight) * 2)) and
    (P.X <= FRightAnchorArrowPoint.X + (FRightAnchorArrowPoint.Y - P.Y) *
    AnchorArrowWidth div ((AnchorArrowHeight - AnchorArrowEdgeHeight) * 2)) then
    Result := adRight;
  if (P.X >= FLeftAnchorArrowRect.Left) and
    (P.X <= FLeftAnchorArrowRect.Right) and
    (P.Y > FLeftAnchorArrowRect.Bottom) and
    (P.Y <= FLeftAnchorLowerRightPoint.Y) then
    Result := adBoth;
end;

procedure TAnchorRuler.CalcMargins;
var
  P: TPoint;
  ElementParent: TControl;
begin
  ElementParent := TAForm(Owner).SizingRect.Element.Parent;
  P := Parent.ScreenToClient(ElementParent.ClientOrigin);
  FLeftMargin := P.X + MarginLeft + 1;
  FRightMargin := P.X + ElementParent.ClientWidth - MarginRight - 1;
end;

procedure TAnchorRuler.SetEnabled(Value: Boolean);
begin
  inherited;
  Invalidate;
end;

procedure TAnchorRuler.CreateParams(var Params: TCreateParams);
begin
  inherited;
//  Params.Style := Params.Style or WS_BORDER;
end;

procedure TAnchorRuler.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AnchorAreaNum: Integer;
  OldLeftAnchor: Integer;
begin
  inherited;
  if FButtonPressed then
  begin
    if FArrowDragged = adNone then Exit;
    AnchorAreaNum := GetAnchorArrowAreaNum(X);
    case FArrowDragged of
      adLeft:
      begin
        if AnchorAreaNum = LeftAnchor then Exit;
        LeftAnchor := AnchorAreaNum;
      end;
      adRight:
      begin
        if AnchorAreaNum = RightAnchor then Exit;
        RightAnchor := AnchorAreaNum;
      end;
      adBoth:
      begin
        if AnchorAreaNum = LeftAnchor then Exit;
        OldLeftAnchor := LeftAnchor;
        LeftAnchor := AnchorAreaNum;
        FRightAnchorBothDragged := FRightAnchorBothDragged +
          LeftAnchor - OldLeftAnchor;
        RightAnchor := FRightAnchorBothDragged;
      end;
    end;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else
  begin
 //   FArrowDragged := adNone;
    FArrowDragged := PointInArea(Point(X,Y));
    ShowHint := False;
  end;
end;

procedure TAnchorRuler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FButtonPressed := True;
  ShowHint := False;
  if FArrowDragged = adBoth then FRightAnchorBothDragged := FRightAnchor;
  Invalidate;
end;

procedure TAnchorRuler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FArrowDragged := adNone;
  FButtonPressed := False;
  Invalidate;
end;

procedure TAnchorRuler.Update;
begin
  CalcMargins;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('AnchorRuler unit initialization ...');
  {$ENDIF}
  AnchorArrowEdgeHeight := AnchorArrowHeight - AnchorArrowWidth div 2;
  AnchorRulerHeight := UpperStripHeight + MainStripHeight + LowerStripHeight;
  UpperArrowTop := AnchorRulerHeight - LowerRectangleHeight - 2 * AnchorArrowHeight - 2;
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('AnchorRuler unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.
