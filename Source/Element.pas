unit Element;

interface

uses Windows, System.Types, Classes, Controls, StdCtrls, ComCtrls, ExtCtrls, Messages, Graphics, SysUtils, DeMeta, ElementsMeta, Forms;

type
  TRectPoint = (
    ptNone,         // вне прямоугольника
    ptInner,        // внутри прямоугольника
    ptRight,        // справа посередине
    ptRightBottom,  // справа снизу
    ptBottom,       // снизу
    ptLeftBottom,   // слева снизу
    ptLeft,         // слева
    ptLeftTop,      // слева сверху
    ptTop,          // сверху
    ptRightTop      // справа сверху
  );

  TSizingRect = class(TCustomControl)
  private
//    FAnchorRuler  : TAnchorRuler;
    FElement      : TControl;
    FE,FL         : TElementMeta;
    PreStartMove  : TRectPoint;
    StartMove     : TRectPoint;
    StartX,StartY : Integer;
    FShift        : TShiftState;
    _T,_B,_X,_Y: Integer ;
    LastDownTime: TTime;
    LastDownX, LastDownY: Integer;
    FOnDblClick : TNotifyEvent;
    procedure SetElement(aElement : TControl);
    procedure WMPaint(var Message: TWmPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    function GetRectPoint(const X, Y : integer) : TRectPoint;
    function SelectControlAtPos(Sender: TControl; XY: TPoint) : TControl;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown (Button: TMouseButton;  Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState;  X,Y: Integer);  override;
    procedure MouseUp(Button: TMouseButton;  Shift: TShiftState; X,Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update; override;
    procedure PanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;  X, Y: Integer);
    function ElementIsSizable: Boolean;
  published
    property PopupMenu;
    property OnDblClick read FOnDblClick write FOnDblClick;
    property Element: TControl read FElement write SetElement;
    property Canvas;
//    property AnchorRuler  : TAnchorRuler read FAnchorRuler write FAnchorRuler;
  end;


implementation

uses Math, DeTypes, DataUnit, DeControls, UnitA;


{ --- TSizingRect ------------------------------------------------------------ }

constructor TSizingRect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  Brush.Style:=bsClear;

  Width  := 75;
  Height := 25;
  _X   := XStep;
  _Y   := YStep;
  _T   := 0;
  _B   := 0;
  FE   := nil;
  FL   := nil;

  LastDownTime := Time-1;
  LastDownX    := 0;
  LastDownY    := 0;
  FOnDblClick  := nil;
end;

procedure TSizingRect.CreateParams(var Params: TCreateParams);
begin inherited CreateParams(Params);
      Params.ExStyle := Params.ExStyle + WS_EX_TRANSPARENT;
      Canvas.Pen.Width:=2;
      Canvas.Pen.Color:=clActiveCaption;
//       Canvas.Pen.Mode := pmNot;
//      Canvas.Brush.Style := bsClear;
end;

procedure TSizingRect.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.FillRect(ClientRect);
  Message.Result := 1;
end;

{
procedure TSizingRect.WMCaptureChanged(var Message: TMessage);
begin
  inherited;
//  Screen.Cursor := crDefault;
end;
}

procedure TSizingRect.Update;
begin
  if Element = nil then Exit;
  Parent:=Element.Parent;

  Left  := FElement.Left - CubeSize div 2;
  Top   := FElement.Top - CubeSize div 2;
  Width := FElement.Width + CubeSize;
  Height:= FElement.Height+ CubeSize;
  Visible := True;
  BringToFront;
end;

procedure TSizingRect.SetElement(aElement : TControl);
begin
  FE:=nil;
  FL:=nil;

  FElement := aElement;
  FElement.BringToFront;
  Update;
  TAForm(Owner).AnchorRuler.Enabled := ElementIsSizable;
  TAForm(Owner).AnchorRuler.Update;

  if (FElement.Tag <> 0) and Assigned(TDeElement(FElement.Tag).ElementMeta) then
    begin
      FE := TDeElement(FElement.Tag).ElementMeta;
      if assigned(FE.Owner) and assigned(FE.Field) and (FE.ElementType<>etLabel) then
        FL := TElementMeta(FE.Owner).FindByFieldAndType(FE.Field, [etLabel]);

      TAForm(Owner).AnchorRuler.LeftAnchor  := FE.EAL;
      TAForm(Owner).AnchorRuler.RightAnchor := FE.EAR;
    end
  else
    SendMessage(Application.MainForm.Handle, DM_STATUS2NOTIFY, 0, 0);
end;

procedure TSizingRect.WMPaint(var Message: TWmPaint);
var
  x1, x2, x3, y1, y2, y3: Integer;
  DC: HDC;
  PS: TPaintStruct;
begin
//  inherited;
  DC:= BeginPaint(Handle, PS);

  with Canvas do
    if StartMove=ptNone then
      begin
        x1:= 0;
        y1:= 0;
        x2:= (Width - CubeSize) div 2;
        y2:= (Height - CubeSize) div 2;
        x3:= Width - CubeSize;
        y3:= Height - CubeSize;
        Brush.Style:= bsSolid;
        Brush.Color:= clBlack;

        FillRect(Rect(x1, y1, x1+CubeSize, y1+CubeSize));  // верний левый
        FillRect(Rect(x2, y1, x2+CubeSize, y1+CubeSize));  // верхний центральный
        FillRect(Rect(x3, y1, x3+CubeSize, y1+CubeSize));  // верхний правый

        if Not (FElement is TBevel) then
          begin
            FillRect(Rect(x1, y2, x1+CubeSize, y2+CubeSize));  // средний левый
            FillRect(Rect(x3, y2, x3+CubeSize, y2+CubeSize));  // средний правый

            FillRect(Rect(x1, y3, x1+CubeSize, y3+CubeSize));  // нижний левый
            FillRect(Rect(x2, y3, x2+CubeSize, y3+CubeSize));  // нижний центральный
            FillRect(Rect(x3, y3, x3+CubeSize, y3+CubeSize));  // нижний правый
          end;
      end
    else
      begin
        Brush.Style:= bsClear;
        if Not (FElement is TBevel) then
          Rectangle(CubeSize div 2, CubeSize div 2, Width - CubeSize div 2, Height - CubeSize div 2)
        else
          Rectangle(CubeSize div 2, CubeSize div 2, Width - CubeSize div 2, 0)
      end;

  EndPaint(Handle, PS);
end;

procedure TSizingRect.PanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  P : TPoint;
begin
  P := Point(X,Y);
  P := ScreenToClient(TControl(Sender).ClientToScreen(P));
  MouseMove(Shift,P.X,P.Y);
end;

procedure TSizingRect.PanelMouseDown(Sender: TObject;  Button : TMouseButton; Shift: TShiftState;  X, Y: Integer);
var
  P : TPoint;
begin
  P := Point(X,Y);
  P := ScreenToClient(TControl(Sender).ClientToScreen(P));
  MouseDown(Button, Shift, P.X, P.Y);
end;

function TSizingRect.GetRectPoint(const X, Y : integer) : TRectPoint;
var
  InnerArea: Boolean;
begin
  InnerArea := (X >= CubeSize div 2) and (X < Width - CubeSize div 2)
    and (Y >= CubeSize div 2) and (Y < Height - CubeSize div 2);
  if (X >= 0) and (X < CubeSize) then
  begin
    if (Y >= 0) and (Y < CubeSize) then Result := ptLeftTop
    else if (Y >= (Height - CubeSize) div 2) and
     (Y < (Height + CubeSize) div 2) then Result := ptLeft
    else if (Y >= Height - CubeSize) and (Y < Height) then Result := ptLeftBottom
    else if InnerArea then Result := ptInner
    else Result := ptNone;
  end
  else if (X >= (Width - CubeSize) div 2) and (X < (Width +CubeSize) div 2) then
  begin
    if (Y >= 0) and (Y < CubeSize) then Result := ptTop
    else if (Y >= Height - CubeSize) and (Y < Height) then Result := ptBottom
    else if InnerArea then Result := ptInner
    else Result := ptNone;
  end
  else if (X >= Width - CubeSize) and (X < Width) then
  begin
    if (Y >= 0) and (Y < CubeSize) then Result := ptRightTop
    else if (Y >= (Height - CubeSize) div 2) and
     (Y < (Height + CubeSize) div 2) then Result := ptRight
    else if (Y >= Height - CubeSize) and (Y < Height) then Result := ptRightBottom
    else if InnerArea then Result := ptInner
    else Result := ptNone;
  end
  else if InnerArea then Result := ptInner
  else Result := ptNone;
end;

function TSizingRect.SelectControlAtPos(Sender: TControl; XY: TPoint) : TControl;
var
  TC:  TControl;
  TP:  TPoint;
  sXY: TPoint;
begin
  Result := nil;
  sXY := Sender.ClientOrigin;
  if (XY.X < sXY.X) or (XY.Y < sXY.Y) or (XY.X >= sXY.X + Sender.Width) or
    (XY.Y >= sXY.Y + Sender.Height) then Exit;
  TC := Sender;
  repeat
    Result := TC;
    if (TC.Tag = 0) or not (TDeElement(TC.Tag).ElementMeta.ElementType in
      [etForm, etPanel, etTabSheet]) then Break;
    TP := TC.ScreenToClient(XY);
    TC := TWinControl(TC).ControlAtPos(TP,True,True);
  until TC = nil;
end;

function TSizingRect.ElementIsSizable: Boolean;
var
  FE: TDeElement;
begin
  FE := TDeElement(Element.Tag);
  Result := Assigned(FE) and ((FE.ElementMeta.ElementType <> etForm)
                              and (FE.ElementMeta.ElementType <> etListTabSheet)
                              and (FE.ElementMeta.ElementType <> etTabSheet));
end;

procedure TSizingRect.MouseDown(Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
var
  C: TControl;
  P: TPoint;
  Msg: TWMMouse;
  Ownr : TComponent;
  isDblClick : Boolean;
begin
  FShift := Shift;
  isDblClick:= ((Time - LastDownTime)<(1/(24*60*60*2))) and (Button = mbLeft)
               and (abs(X - LastDownX)<GetSystemMetrics(SM_CXDOUBLECLK))
               and (abs(Y - LastDownY)<GetSystemMetrics(SM_CYDOUBLECLK));

  LastDownTime := Time;
  LastDownX := X;
  LastDownY := Y;

  if (isDblClick) and Assigned(FOnDblClick) then
  begin
    StartMove := ptNone;
    FOnDblClick(self);
    Exit;
  end;

  if PreStartMove = ptInner then StartMove := ptNone
  else StartMove := PreStartMove;
  if StartMove <> ptNone then
  begin
    StartX := X;
    StartY := Y;
    Exit;
  end;
  P.X:=X;
  P.Y:=Y;
  P:=ClientToScreen(P);
  Visible := False;
  C := SelectControlAtPos(TAForm(Owner)._PageControl, P);
  if (C is TPageControl) and (Button = mbLeft) then
  begin
    P := TControl(C).ScreenToClient(P);
    Msg.Keys := MK_LBUTTON;
    Msg.XPos := P.X;
    Msg.YPos := P.Y;
    P := TPageControl(C).ClientToScreen(P);
    Ownr := TPageControl(C).Owner;
    while Assigned(Ownr) and (not (Owner is TAForm)) do
      Ownr := Ownr.Owner;
    if Assigned(Ownr) then
      SendMessage(TAForm(Ownr).Handle, WM_SELECTPAGE,
        TMessage(Msg).WParam,TMessage(Msg).LParam);
  end;
  if (C <> nil) and (TDeElement(C.Tag) <> nil) then
  begin
    Element := C;
//    AnchorRuler.Element := C;
    if Button = mbLeft then
    begin
      P := ScreenToClient(P);
      if ElementIsSizable then
        StartMove := ptInner
      else
        StartMove := ptNone;
      StartX := P.X;
      StartY := P.Y;
    end;
  end;
end;

procedure TSizingRect.MouseMove(Shift: TShiftState; X,Y: Integer);
var
  WW, dY, dX, a, l, t, w, h: Integer;
  P: TPoint;
  PageControl: TControl;
  DeElement: TDeElement;

  function Divide(A, B: Integer): Integer;
  begin
    Result := A div B;
    if A < 0 then Dec(Result);
  end;

begin
  PageControl := TAForm(Owner)._PageControl;
  P := PageControl.ScreenToClient(ClientToScreen(Point(X,Y)));
  if (P.X >= 0) and (P.X < PageControl.Width) and
    (P.Y >= 0) and (P.Y < PageControl.Height) then
    MouseCapture := True
  else if StartMove = ptNone then
  begin
    MouseCapture := False;
    Screen.Cursor := crDefault;
    Exit;
  end;

  if not ElementIsSizable then Exit;
  if StartMove = ptNone then
  begin
    PreStartMove := GetRectPoint(X,Y);
    case PreStartMove of
      ptLeft, ptRight:          Screen.Cursor := crSizeWE;
      ptTop, ptBottom:          Screen.Cursor := crSizeNS;
      ptLeftBottom, ptRightTop: Screen.Cursor := crSizeNESW;
      ptLeftTop, ptRightBottom: Screen.Cursor := crSizeNWSE;
      else                      Screen.Cursor := crArrow;
    end;
  end
  else
  begin
    WW:=TWinControl(Parent).ClientWidth - (MarginLeft+MarginRight);
    l:=Left;
    t:=Top;
    if (Parent is TDeTabSheet) then Inc(t, TDeTabSheet(Parent).Y);

    case StartMove of
      ptInner:       begin
                       a    := X + L - StartX - MarginLeft - ((WW * FE.EAL) div (AnchorsCount - 1));
                       dX   := Divide(a+_X div 2,_X) - FE.EL;
                       FE.EL := FE.EL + dX;
                       FE.ER := FE.ER + dX;

                       a   :=(T + Y - StartY)-_T;
                       dY  := Divide(a + _Y div 2,_Y) - FE.ET ;

                       if Assigned(FL) and not(ssShift in FShift) then
                         begin
                           if (FE.EAL = FL.EAL) then FL.EL := FL.EL + dX;
                           if (FE.EAL = FL.EAR) then FL.ER := FL.ER + dX;
                                                     FL.ET := FL.ET + dY;
                         end;

                       FE.ET:= FE.ET + dY;
                     end;
      ptTop:         begin
                       a:=(T+Y-StartY)-_T;
                       dY   := Divide(a+_Y div 2,_Y) - FE.ET;
                       FE.EH := FE.EH - dY;
                       FE.ET := FE.ET + dY;

                       if Assigned(FL) and not(ssShift in FShift) then  FL.ET := FL.ET + dY;
                     end;
      ptRightTop:    begin
                       a:=X+l-MarginLeft-((WW*FE.EAR) div (AnchorsCount-1));
                       FE.ER:=Divide(a+_X div 2,_X);
                       a:=(T+Y-StartY)-_T;
                       dY   := Divide(a+_Y div 2,_Y) - FE.ET;
                       FE.EH := FE.EH - dY;
                       FE.ET := FE.ET + dY;

                       if Assigned(FL) and not(ssShift in FShift) then  FL.ET := FL.ET + dY;
                     end;
      ptRight:       begin
                       a:=X+l-MarginLeft-((WW*FE.EAR) div (AnchorsCount-1));
                       FE.ER:=Divide(a+_X div 2,_X);
                     end;
      ptRightBottom: begin
                       a:=X+l-MarginLeft-((WW*FE.EAR) div (AnchorsCount-1));
                       FE.ER:=Divide(a+_X div 2,_X);
                       a:=Y;
                       FE.EH:=Divide(a-_Y div 2,_Y);
                     end;
      ptBottom:      begin
                       a:=Y;
                       FE.EH:=Divide(a-_Y div 2,_Y);
                     end;
      ptLeftTop:     begin
                       a:=X+L-StartX-MarginLeft-((WW*FE.EAL) div (AnchorsCount-1));
                       dX := Divide(a+_X div 2,_X) - FE.EL;
                       FE.EL:=FE.EL + dX;
                       a:=(T+Y-StartY)-_T;
                       dY    := Divide(a+_Y div 2,_Y) - FE.ET;
                       FE.EH := FE.EH - dY;
                       FE.ET := FE.ET + dY;

                       if Assigned(FL) and not(ssShift in FShift) then  FL.ET := FL.ET + dY;
                       if Assigned(FL) and not(ssShift in FShift) and (FE.EAL = FL.EAR) and (FE.EL - dX = FL.ER) then  FL.ER := FL.ER + dX;
                       if Assigned(FL) and not(ssShift in FShift) and (FE.EAL = FL.EAL) and (FE.EL - dX = FL.EL) then  FL.EL := FL.EL + dX;
                     end;
      ptLeft:        begin
                       a:=X+L-StartX-MarginLeft-((WW*FE.EAL) div (AnchorsCount-1));
                       dX := Divide(a+_X div 2,_X) - FE.EL;
                       FE.EL:=FE.EL + dX;

                       if Assigned(FL) and not(ssShift in FShift) and (FE.EAL = FL.EAR) and (FE.EL - dX = FL.ER) then  FL.ER := FL.ER + dX;
                       if Assigned(FL) and not(ssShift in FShift) and (FE.EAL = FL.EAL) and (FE.EL - dX = FL.EL) then  FL.EL := FL.EL + dX;
                     end;
      ptLeftBottom:  begin
                       a:=X+L-StartX-MarginLeft-((WW*FE.EAL) div (AnchorsCount-1));
                       dX := Divide(a+_X div 2,_X) - FE.EL;
                       FE.EL:=FE.EL + dX;
                       a:=Y;
                       FE.EH:=Divide(a-_Y div 2,_Y);

                       if Assigned(FL) and not(ssShift in FShift) and (FE.EAL = FL.EAR) and (FE.EL - dX = FL.ER) then  FL.ER := FL.ER + dX;
                       if Assigned(FL) and not(ssShift in FShift) and (FE.EAL = FL.EAL) and (FE.EL - dX = FL.EL) then  FL.EL := FL.EL + dX;
                     end;
    end;

    if Assigned(FL) then
      if FL.ET < 1 then FL.ET := 1;
    if FE.ET < 1 then FE.ET := 1;
    if FE.ElementType = etLabel then FE.EH := Max( 1, FE.EH)
                                else FE.EH := Max( 3, FE.EH);

    l:=(MarginLeft+((WW*FE.EAL) div (AnchorsCount-1))+XStep*FE.EL-(CubeSize div 2));
    w:=(MarginLeft+((WW*FE.EAR) div (AnchorsCount-1))+XStep*FE.ER+(CubeSize div 2))-l;
    t:=(_Y*FE.ET-(CubeSize div 2))+_T;
    h:=(_Y*FE.EH+(CubeSize div 2)*2)-YSPace;
    if (Parent is TDeTabSheet) then Dec(t,TDeTabSheet(Parent).Y);
    Invalidate;
    SetBounds(l,t,w,h);
    SendMessage(Application.MainForm.Handle, DM_STATUS2NOTIFY, NativeInt(PChar(Format('%s  L %d:%d / R %d:%d / T %d / H %d',
                                                        [FE.Caption, FE.EAL, FE.EL, FE.EAR, FE.ER, FE.ET, FE.ET]))), 0);

    // перемещаем заголовок вместе с контролом ввода если не нажат ssShift
    if Assigned(FL) then
      begin
        DeElement:=TAForm(Owner).FindByElement(FL);
        if Assigned(DeElement) then
          FL.SetBounds(DeElement.Control);
      end;
  end;
end;

procedure TSizingRect.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  E: TElementMeta;
begin
  if not ElementIsSizable then Exit;
  E := TDeElement(Element.Tag).ElementMeta;
  StartMove := ptNone;
  E.SetBounds(Element);
  Element.Repaint;
  Update;
end;

end.
