unit MainMenuList;

interface

uses Windows, Classes, Controls, Graphics, Messages, {Buttons, }ExtCtrls, Forms, SysUtils;

const
  icoFlipOpened = 1;
  icoFlipClosed = 2;

type
  TMenuOpt = class(TCustomControl)
  private
    FParent: TMenuOpt;

    FIsSeparator: Boolean;
    FIsFlip: Boolean;
    FIsFlipOpen: Boolean;

    FItemNum: Integer;
    FImageIndex: Integer;
    FCaption: string;
    FLevel: Integer;
    FPressed: Boolean;
    FHilited: Boolean;
    FStretchedImageRect: TRect;
    procedure SetCaption(aCaption: string);
    procedure RefreshCaption;
    procedure RefreshImage;
    procedure SetImage(Index: Integer);
    procedure WMRButtonUp(var Msg : TMessage);  message WM_RBUTTONUP;
    procedure WMLButtonDown(var Msg : TMessage);  message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg : TMessage);  message WM_LBUTTONUP;
  protected
    procedure Paint; override;
  public
    Data: Pointer;
    constructor Create(AOwner: TComponent; const ALevel: Integer); reintroduce;
    //destructor Destroy; override;
    procedure Flip  ;
    property Father: TMenuOpt read FParent write FParent;
    property IsSeparator : Boolean read FIsSeparator;
    property IsFlip: Boolean read FIsFlip write FIsFlip;
    property IsFlipOpen: Boolean read FIsFlipOpen;
    property Caption: string read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write SetImage;
    property ItemNum : integer read FItemNum;
    function IsVisible: Boolean;
  end;

  TMenuCont = class;
  TScrollState   = (ssNone,ssUp,ssDown);
  TPressState    = (psPressed,psReleased);

  TScrollBtn = class(TCustomControl)
  private
    FColor    : TColor;
    FColorAlt : TColor;
    FContainer: TMenuCont;
    FType: TScrollState;
    FPressState: TPressState;
    FMouseOnButton: Boolean;
//    FHotScroll: Boolean;
    procedure ScrollBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBtnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  protected
    constructor Create(AOwner: TMenuCont; const BtnType: TScrollState); reintroduce;
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  TGetImageIndexEvent = procedure(Sender: TObject; var ImageIndex: Integer) of object;

  TMenuCont = class(TWinControl)
  private
    FFont: TFont;
    FItemCount: Integer;
    FItemIndex: Integer;
    FTopScrollBtn: TScrollBtn;
    FBottomScrollBtn: TScrollBtn;
    FTopItemTop: Integer;
    FBottomItemBottom: Integer;
    FClientTop: Integer;
    FClientBottom: Integer;
    FTimer: TTimer;
    FScrollState: TScrollState;
    FOnItemSelect: TNotifyEvent;
    FScrollBtnsOn: Boolean;
    FHotScroll: Boolean;
    FScrollTimes: Integer;
    FMouseOnScrollButton: Boolean;
    FImages1: TImageList;
    FImages2: TImageList;
    FImages3: TImageList;
    FImages1H: TImageList;
    FImages2H: TImageList;
    FImages3H: TImageList;
    FOnGetImageIndex: TGetImageIndexEvent;
    procedure SelectItem(const ItemIndex: Integer);
    procedure ScrollItems(Step: Integer);
    procedure SetHotScrollState(const NewState: Boolean);
    procedure SetScrollBtnsState(const NewState: Boolean);
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure DoGetImageIndex(Sender: TObject; var ImageIndex: Integer);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Resize; override;
    procedure TimerTimer(Sender: TObject);
    procedure MenuOptMouseMove(Sender: TObject; Shift: TShiftState;  X, Y: Integer);
    procedure SetScrollState(const AScrollState: TScrollState);
  public
    Items: array[0..1024] of TMenuOpt;
    procedure MenuOptClick(Sender: TObject);
    property ScrollBtnsOn: Boolean read FScrollBtnsOn write SetScrollBtnsState;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(const ALevel: Integer): TMenuOpt;
    function Insert(const AIndex: integer;  const ALevel: Integer): TMenuOpt;
    procedure Move(const AFromIndex, AToIndex: Integer);
    procedure Delete(aMenuOpt : TMenuOpt);
    procedure Clear;
    property ItemIndex: Integer read FItemIndex write SelectItem default -1;
    property ItemCount: Integer read FItemCount;
    property ScrollState: TScrollState read FScrollState write SetScrollState;
    procedure FullRepaint;
    property Font Read FFont;
  published
    procedure RealignItems(SkipByHeight: boolean = False);

    property Images1: TImageList read FImages1 write FImages1;
    property Images2: TImageList read FImages2 write FImages2;
    property Images3: TImageList read FImages3 write FImages3;
    property Images1H: TImageList read FImages1H write FImages1H;
    property Images2H: TImageList read FImages2H write FImages2H;
    property Images3H: TImageList read FImages3H write FImages3H;
    property OnItemSelect: TNotifyEvent read FOnItemSelect write FOnItemSelect;
    property HotScroll: Boolean read FHotScroll write SetHotScrollState;
    // v.17.2
    property OnGetImageIndex: TGetImageIndexEvent read FOnGetImageIndex write FOnGetImageIndex;
  end;

function FindParentForm(Control: TControl): TForm;

implementation

uses Types, Menus,
     DeLog, Funcs;

const
  MenuItemHeight  = 32 + 8 {+ 4};
  MenuItemHeight2 = 16 + 8 {+ 4};
  MenuItemHeight3 = 16 + 8 {+ 4};
  TextLeftPos = 50;
  ScrollStep = 3;
  ScrollFrequency = 20;
  ScrollDelayTimes = 20;
  LeftStripWidth = 44;
  ImageRightPos = LeftStripWidth - 4 - 2;

var
  ScrollBtnHeight: Integer;

function FindParentForm(Control: TControl): TForm;
var
  ParControl: TControl;
begin
  Result := nil;
  ParControl := Control.Parent;
  while Assigned(ParControl) do
  begin
    if ParControl is TForm then
    begin
      Result := TForm(ParControl);
      Break;
    end;
    ParControl := ParControl.Parent;
  end;
end;

procedure TMenuOpt.SetCaption(aCaption: string);
var P,aHeight : Integer;
begin
  P := pos('|',aCaption);

  if 0 < P then FCaption := Copy(aCaption,P+1,MaxInt)
           else FCaption := aCaption;

  FIsSeparator := (FCaption = cLineCaption);

  if FIsSeparator then
    begin
      Hint:= EmptyStr;
      Cursor:= crDefault;
      aHeight:= 3;
    end
  else
    begin
      Cursor := crHandPoint;
      case FLevel of
         1: aHeight := MenuItemHeight;
         2: aHeight := MenuItemHeight2;
       else aHeight := MenuItemHeight3;
      end;
    end;

  if aHeight<>Height then
    begin
      Height:=aHeight;
      TMenuCont(parent).RealignItems;
    end;

  if IsVisible then
    begin
      RefreshCaption;
      Invalidate;
    end;
end;

procedure TMenuOpt.RefreshCaption;
var
  A,TxtHeight, TxtWidth: Integer;
  PrintedCaption: string;
  AddDots: Boolean;
begin
  ShowHint := False;
  Canvas.Brush.Color := Color;
  Canvas.Font.Color  := clHighlightText;

  PrintedCaption := FCaption;
  if FLevel in [1,2] then A:= 0
                     else A:= 8 * (FLevel-2);

  AddDots := True;
  TxtWidth := Canvas.TextWidth(PrintedCaption);
  while (TextLeftPos+A + TxtWidth > Width-4) and (Length(PrintedCaption) > 3) do
  begin
    ShowHint := True;
    if AddDots then
      begin
        Delete(PrintedCaption,Length(PrintedCaption),1);
        PrintedCaption := PrintedCaption + '...';
      end
    else
        Delete(PrintedCaption,Length(PrintedCaption)-3,1);
    TxtWidth := Canvas.TextWidth(PrintedCaption);
    AddDots := False;
  end;

  TxtHeight := Canvas.TextHeight(PrintedCaption);
  Canvas.TextOut(TextLeftPos+A,(Height - TxtHeight) div 2, PrintedCaption);
end;

procedure TMenuOpt.RefreshImage;
var
  ImageList: TImageList;
  TempIndex: Integer;
begin
  if Assigned(Parent) and (Parent is TMenuCont) and not FIsSeparator then
    begin
      if FLevel in [1,2] then
        begin
          case FLevel of
             1: if FHilited then ImageList:= TMenuCont(Parent).FImages1H
                            else ImageList:= TMenuCont(Parent).FImages1;
             2: if FHilited then ImageList:= TMenuCont(Parent).FImages2H
                            else ImageList:= TMenuCont(Parent).FImages2;
          end;
          TempIndex := FImageIndex;
          if FIsFlip then
            if FIsFlipOpen xor FHilited
              then TempIndex := icoFlipOpened
              else TempIndex := icoFlipClosed;
          TMenuCont(Parent).DoGetImageIndex(Self, TempIndex); // Маппинг иконок ...
          if (TempIndex >= 0) and (TempIndex < ImageList.Count) then
            ImageList.Draw(Canvas, ImageRightPos - ImageList.Width, (Height - ImageList.Height) div 2, TempIndex);
        end
      else
        begin
          if FHilited then ImageList:= TMenuCont(Parent).FImages3H
                      else ImageList:= TMenuCont(Parent).FImages3;
          TempIndex := 21384;
          TMenuCont(Parent).DoGetImageIndex(Self, TempIndex); // Маппинг иконок ...
          ImageList.Draw(Canvas, LeftStripWidth + 8*(FLevel - 3)- ImageList.Width div 2 ,
                                   (Height - ImageList.Height) div 2, TempIndex);
        end
    end;
end;

procedure TMenuOpt.SetImage(Index: Integer);
begin
  FImageIndex:= Index;
  if IsVisible then
    begin
      RefreshImage;
      Invalidate;
    end;
end;

procedure TMenuOpt.WMLButtonDown(var Msg : TMessage);
begin
  FPressed:= True;
  Invalidate;
  inherited;
end;

procedure TMenuOpt.WMLButtonUp(var Msg : TMessage);
begin
  FPressed:= False;
  Invalidate;
  inherited;
end;

procedure TMenuOpt.WMRButtonUp(var Msg : TMessage);
begin
  inherited;
  FHilited := False;
  FPressed := False;
  Invalidate;
end;

constructor TMenuOpt.Create(AOwner: TComponent; const ALevel: Integer);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  FIsSeparator := False;
  FIsFlip := False;
  FIsFlipOpen:= False;

  FLevel := ALevel;
  FPressed := False;
  FHilited := False;
  Cursor := crHandPoint;
  case aLevel of
      1: Height := MenuItemHeight;
      2: Height := MenuItemHeight2;
    else Height := MenuItemHeight3;
  end;
  if AOwner is TMenuCont then
    Canvas.Font.assign(TMenuCont(AOwner).Font);
end;

{
destructor TMenuOpt.Destroy;
begin
  inherited Destroy;
end;
}

procedure TMenuOpt.Paint;
var AP: array of TPoint;
    R: TRect;
begin
  if FHilited then Color:= clActiveCaption
              else Color:= clAppWorkSpace;
  inherited;

  if FIsSeparator then
    begin
      Canvas.Brush.Color:=clWhite;//clBtnFace;
      Canvas.FillRect(Rect(LeftStripWidth, 0, Width, Height));
      if FLevel in [1,2,3] then
        Canvas.Brush.Color:= clAppWorkspace;
      Canvas.FillRect(Rect(0, 0, LeftStripWidth, Height));
      Exit;
    end;

  Canvas.Brush.Color := clWhite;//BtnFace;
  Canvas.FillRect(Rect(0, 0, LeftStripWidth, Height));
 {
  if FLevel>2 then
    begin
      SetLength(AP,4);
      AP[0]:=Point(LeftStripWidth+(FLevel-3)*8, (Height div 2)-3);
      AP[1]:=Point(AP[0].X+3, (Height div 2));
      AP[2]:=Point(AP[0].X,   (Height div 2)+3);
      AP[3]:=Point(AP[0].X-3, (Height div 2));

      Canvas.Pen.Color:= clWhite;
      Canvas.Brush.Color:= clWhite;
      Canvas.Polygon(AP);
    end;   {}

  R:= Rect(0, 0, Width, Height);
  if FHilited then
    begin
      if FPressed then Canvas.Pen.Color:= clHighlight
                  else Canvas.Pen.Color:= clActiveCaption;
      Canvas.Brush.Color:= clActiveCaption;
      Canvas.Rectangle(R);
    end;

  RefreshCaption;
  RefreshImage;
end;

procedure TMenuOpt.Flip;
begin
  if not FIsFlip then Exit;
  FIsFlipOpen:= not FIsFlipOpen;
  Paint;
  TMenuCont(parent).RealignItems;
end;

function TMenuOpt.IsVisible: Boolean;
begin
  Result:= (0 < TMenuCont(Owner).Height);
end;

constructor TMenuCont.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := True;
 // DoubleBuffered := True;
  Color := clAppWorkspace;
  FScrollState := ssNone;
  FScrollBtnsOn := True;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := ScrollFrequency;
  FTimer.OnTimer := TimerTimer;
  FFont:= TFont.Create;

  FTopScrollBtn := TScrollBtn.Create(Self,ssUp);
  FTopScrollBtn.Left := LeftStripWidth;
  FTopScrollBtn.Height := ScrollBtnHeight;
  FTopScrollBtn.Visible := False;

  FBottomScrollBtn := TScrollBtn.Create(Self,ssDown);
  FBottomScrollBtn.Left := LeftStripWidth;
  FBottomScrollBtn.Height := ScrollBtnHeight;
  FBottomScrollBtn.Visible := False;

  HotScroll := True;
  FScrollTimes := 0;
end;

destructor TMenuCont.Destroy;
begin
  FFont.Free;
  inherited;
end;

function TMenuCont.Add(const ALevel: Integer): TMenuOpt;
begin
  Result := Insert(FItemCount, ALevel);
end;

function TMenuCont.Insert(const AIndex: Integer; const ALevel: Integer): TMenuOpt;
var I : integer;
begin
  result := TMenuOpt.Create(Self, aLevel);
  result.Parent := Self;
  result.Width := Width;
  if aIndex < FItemCount then
  begin
    result.Top := Items[aIndex].Top;
    result.FItemNum := aIndex;
  end
  else
  begin
    result.Top := FBottomItemBottom;
    result.FItemNum := FItemCount;
  end;
  result.SendToBack;
  result.OnClick := MenuOptClick;
  result.OnMouseMove := MenuOptMouseMove;
  FBottomItemBottom := FBottomItemBottom + result.Height;
  if aIndex < FItemCount then
  begin
    for I := FItemCount-1 downto aIndex do
    begin
      Items[I+1] := Items[I];
      Items[I+1].FItemNum := I+1;
      Items[I+1].Top := Items[I+1].Top + result.Height;
    end;
    Items[aIndex] := result;
  end
  else
    Items[FItemCount] := result;
  Inc(FItemCount);
  ScrollItems(0);
end;

procedure TMenuCont.Move(const AFromIndex, AToIndex: Integer);
var MenuOpt : TMenuOpt;
    I       : integer;
begin
  MenuOpt := Items[aFromIndex];
  if aToIndex > aFromIndex then
    for I := aFromIndex to aToIndex-1 do
    begin
      Items[I+1] := Items[I];
      Items[I].FItemNum := I;
      Items[I].Top := Items[I].Top - MenuOpt.Height;
      MenuOpt.Top := MenuOpt.Top + Items[I].Height;
    end
  else if aToIndex < aFromIndex then
    for I := aFromIndex-1  downto aToIndex do
    begin
      Items[I+1] := Items[I];
      Items[I+1].FItemNum := I+1;
      Items[I+1].Top := Items[I+1].Top + MenuOpt.Height;
      MenuOpt.Top := MenuOpt.Top - Items[I+1].Height;
    end;
  MenuOpt.FItemNum := aToIndex;
  Items[aToIndex] := MenuOpt;
end;

procedure TMenuCont.Delete(aMenuOpt : TMenuOpt);
var I, RemovedIndex : integer;
begin
  RemovedIndex := aMenuOpt.FItemNum;
  FBottomItemBottom := FBottomItemBottom - aMenuOpt.Height;
  if Assigned(aMenuOpt) then
  begin
    for I := aMenuOpt.FItemNum to FItemCount-2 do
    begin
      Items[I] := Items[I+1];
      Items[I].FItemNum := I;
      Items[I].Top := Items[I].Top - aMenuOpt.Height;
    end;
    aMenuOpt.Free;
    Dec(FItemCount);
    if RemovedIndex = ItemIndex then
    begin
      if RemovedIndex >= FItemCount then
        dec(RemovedIndex);
      if (RemovedIndex < FItemCount) and (RemovedIndex >= 0) then
        SelectItem(RemovedIndex);
    end;
    ScrollItems(0);
  end;
end;

procedure TMenuCont.RealignItems(SkipByHeight: boolean = False);
var I : integer;
    V: Boolean;
    P: TMenuOpt;
begin
  if (SkipByHeight) and (Height=0)  then Exit;

  FBottomItemBottom:=0;
  for i:=0 to FItemCount-1 do
    begin
      Items[I].Top:= FBottomItemBottom;

      V:=True;
      P:=Items[I].Father;
      while Assigned(P) do
        begin
          V:=V and not (P.IsFlip and not P.IsFlipOpen);
          P:=P.Father;
        end;

      Items[I].Visible:=V;
      if V then
        FBottomItemBottom:=FBottomItemBottom+ Items[I].Height;

      Items[I].Paint;
    end;
    ScrollItems(0);
end;

procedure TMenuCont.SelectItem(const ItemIndex: Integer);
begin
  FItemIndex := ItemIndex;
end;

procedure TMenuCont.SetScrollBtnsState(const NewState: Boolean);
begin
  FScrollBtnsOn := NewState;
  ScrollItems(0);
end;

procedure TMenuCont.SetHotScrollState(const NewState: Boolean);
begin
  FHotScroll := NewState;
end;

procedure TMenuCont.Resize;
var
  Ind: Integer;
begin
  inherited;
  if 0 = Height then Exit;

  for Ind := 0 to FItemCount - 1 do
    Items[Ind].Width := Width;

  FBottomScrollBtn.Top := Height - ScrollBtnHeight;
  FTopScrollBtn.Width := Width - LeftStripWidth;
  FBottomScrollBtn.Width := Width - LeftStripWidth;
  ScrollItems(0);

  invalidate;
end;

procedure TMenuCont.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  C: TControlCanvas;
begin
  Msg.Result := 1;
  C := TControlCanvas.Create;
  C.Control := Self;
  if assigned(Parent) then
    begin
      C.Brush.Color := clWhite;
      C.FillRect(Rect(0, 0, LeftStripWidth, Parent.ClientHeight));
      C.Brush.Color := Color;
      C.FillRect(Rect(LeftStripWidth, 0, Width, Parent.ClientHeight));
    end
  else
    begin
      C.Brush.Color := clWhite;
      C.FillRect(Rect(0, 0, LeftStripWidth, Height));
      C.Brush.Color := Color;
      C.FillRect(Rect(LeftStripWidth, 0, Width, Height));
    end;

  C.Free;
end;

procedure TMenuCont.ScrollItems(Step: Integer);
var
  Ind: Integer;
begin
  if (Step = 0) and (FBottomItemBottom < Height) and
    (FTopItemTop < 0) then
  begin
    Step := Height - FBottomItemBottom;
    if Step > -FTopItemTop then Step := -FTopItemTop;
  end;
  if Step <> 0 then
  begin
    FTopItemTop := FTopItemTop + Step;
    if (Step > 0) and (FTopItemTop > 0) then
    begin
      Step := Step - FTopItemTop;
      FTopItemTop := 0;
    end;
    FBottomItemBottom := FBottomItemBottom + Step;
    if (Step < 0) and (FBottomItemBottom < Height) then
    begin
      Step := Step + Height - FBottomItemBottom;
      FTopItemTop := FTopItemTop + Height - FBottomItemBottom;
      FBottomItemBottom := Height;
      if FTopItemTop > 0 then
      begin
        Step := Step - FTopItemTop;
        FBottomItemBottom := FBottomItemBottom - FTopItemTop;
        FTopItemTop := 0;
      end;
    end;

    if Step<0 then
      for Ind := 0 to FItemCount - 1 do
        SetWindowPos(Items[Ind].Handle, 0,
                     Items[Ind].Left, Items[Ind].Top + Step,
                     0, 0, SWP_NOSIZE + SWP_NOZORDER)
    else
      for Ind := FItemCount - 1 downto 0 do
        SetWindowPos(Items[Ind].Handle, 0,
                     Items[Ind].Left, Items[Ind].Top + Step,
                     0, 0, SWP_NOSIZE + SWP_NOZORDER);
  end;

  if (FTopItemTop < 0) and FScrollBtnsOn then
  begin
    FTopScrollBtn.Visible := True;
    FClientTop := ScrollBtnHeight;
  end
  else
  begin
    FTopScrollBtn.Visible := False;
    FClientTop := 0;
  end;

  if (FBottomItemBottom > Height) and FScrollBtnsOn then
  begin
    FBottomScrollBtn.Visible := True;
    FClientBottom := Height - ScrollBtnHeight;
  end
  else
  begin
    FBottomScrollBtn.Visible := False;
    FClientBottom := Height;
  end;
end;

procedure TMenuCont.TimerTimer(Sender: TObject);
begin
  if FHotScroll then
  begin
    Inc(FScrollTimes);
    if FScrollTimes <= ScrollDelayTimes then Exit;
  end;
  case ScrollState of
    ssUp:   ScrollItems(+ScrollStep);
    ssDown: ScrollItems(-ScrollStep);
  end;
end;

procedure TMenuCont.MenuOptClick(Sender: TObject);
var
  SelItem: TMenuOpt;
  ItemBottom: Integer;
begin
  if Parent is TCustomForm then
    TCustomForm(Parent).ActiveControl := Self;
  SelItem := TMenuOpt(Sender);
  if SelItem.Top < FClientTop then
    ScrollItems(-SelItem.Top + FClientTop);
  ItemBottom := SelItem.Top + SelItem.Height;
  if ItemBottom > FClientBottom then
    ScrollItems(-ItemBottom + FClientBottom);
  ItemIndex := SelItem.FItemNum;
//  for Ind := 0 to FItemCount - 1 do
//    SendMessage(Items[Ind].Handle,WM_PAINT,0,0);
  FullRepaint;
  
  if (SelItem.IsFlip) then
    SelItem.Flip
  else
    if (Not SelItem.IsSeparator) and (Assigned(FOnItemSelect)) then FOnItemSelect(Self);
  MenuOptMouseMove(Items[FItemIndex],[],-1,-1);
end;

procedure TMenuCont.Clear;
var
  Ind: Integer;
begin
  for Ind := 0 to FItemCount - 1 do Items[Ind].Free;
  FItemCount := 0;
  FTopItemTop := 0;
  FBottomItemBottom := 0;
  FItemIndex := -1;
  ScrollItems(0);
end;

procedure TMenuCont.FullRepaint;
var
  Ind: Integer;
begin
  for Ind := 0 to FItemCount - 1 do
    SendMessage(Items[Ind].Handle,WM_PAINT,0,0);
  SendMessage(Handle,WM_PAINT,0,0);
  invalidate;
end;

procedure TMenuCont.MenuOptMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TopEdge, BottomEdge: Integer;
  SelOption: TMenuOpt;
begin
  SelOption := TMenuOpt(Sender);
  if SelOption.Top < FClientTop then TopEdge := FClientTop - SelOption.Top
  else TopEdge := 0;

  if SelOption.Top + SelOption.Height > FClientBottom then
    BottomEdge := FClientBottom - SelOption.Top
  else BottomEdge := SelOption.Height;

  if (X < 0) or (X >= SelOption.Width) or
    (Y < TopEdge) or (Y >= BottomEdge) then
  begin
    SelOption.FHilited := False;
    SelOption.FPressed := False;
    SelOption.MouseCapture := False;
    SelOption.Invalidate;
  end
  else
  begin
    SelOption.FHilited := True;
    SelOption.MouseCapture := True;
    SelOption.Invalidate;
  end;
end;

procedure TMenuCont.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

procedure TMenuCont.WMMouseWheel(var Msg: TWMMouseWheel);
begin
  Msg.Result := 1;
  ScrollItems(Msg.WheelDelta div 10);
end;

procedure TMenuCont.SetScrollState(const AScrollState: TScrollState);
var ParentForm: TForm;
begin
  if FScrollState=aScrollState then Exit;

  ParentForm := FindParentForm(Self);

//  if FMouseOnScrollButton then
  if Assigned(ParentForm) then
  begin
    if aScrollState = ssNone then
      ParentForm.EnableAlign
    else
      ParentForm.DisableAlign;
  end;

  FScrollState:=aScrollState;
end;

procedure TMenuCont.DoGetImageIndex(Sender: TObject; var ImageIndex: Integer);
begin
  if Assigned(OnGetImageIndex) then OnGetImageIndex(Sender, ImageIndex);
end;

constructor TScrollBtn.Create(AOwner: TMenuCont; const BtnType: TScrollState);
begin
  FContainer := AOwner;
  inherited Create(FContainer);
  Parent := FContainer;

  FType  := BtnType;
  FPressState := psReleased;

  OnMouseUp   := self.ScrollBtnMouseUp;
  OnMouseDown := self.ScrollBtnMouseDown;
  OnMouseMove := self.ScrollBtnMouseMove;
  FColor    := ChangeBrightnessByDelta(FContainer.Color,+20);
  FColorAlt := ChangeBrightnessByDelta(FContainer.Color,-20);
  Color     := FColor; 
end;

procedure TScrollBtn.CreateParams(var Params: TCreateParams);
begin
  inherited;
 // Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TScrollBtn.Paint;
var
  HMd, VMd: Integer;
begin
  inherited;
  HMd := Width div 2;
  VMd := Height div 2;
  Canvas.Brush.Color := clBlack;
  Canvas.Pen.Color := clBlack;
  if FContainer.FHotScroll then
  begin
    if FType = ssUp then
      Canvas.Polygon([Point(HMd-3,VMd+1),Point(HMd+3,VMd+1),Point(HMd,VMd-2)])
    else
      Canvas.Polygon([Point(HMd-3,VMd-2),Point(HMd+3,VMd-2),Point(HMd,VMd+1)]);
  end
  else // not HotScroll
  begin
    if FPressState = psReleased then
    begin
      if FType = ssUp then
        Canvas.Polygon([Point(HMd-3,VMd+1),Point(HMd+3,VMd+1),Point(HMd,VMd-2)])
      else
        Canvas.Polygon([Point(HMd-3,VMd-2),Point(HMd+3,VMd-2),Point(HMd,VMd+1)]);
      if FMouseOnButton then
      begin
        Canvas.Pen.Color := clWhite;
        Canvas.Polyline([Point(0,Height-1),Point(0,0),Point(Width-1,0)]);
        Canvas.Pen.Color := clBlack;
        Canvas.Polyline([Point(Width-1,0),Point(Width-1,Height-1),Point(0,Height-1)]);
      end;
    end
    else
    begin
      if FType = ssUp then
        Canvas.Polygon([Point(HMd-2,VMd+2),Point(HMd+4,VMd+2),Point(HMd+1,VMd-1)])
      else
        Canvas.Polygon([Point(HMd-2,VMd-1),Point(HMd+4,VMd-1),Point(HMd+1,VMd+2)]);
      Canvas.Pen.Color := clBlack;
      Canvas.Polyline([Point(0,Height-1),Point(0,0),Point(Width-1,0)]);
      Canvas.Pen.Color := clWhite;
      Canvas.Polyline([Point(Width-1,0),Point(Width-1,Height-1),Point(0,Height-1)]);
    end;
  end;
end;

procedure TScrollBtn.ScrollBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not FContainer.HotScroll then
  begin
    FPressState := psReleased;
    Invalidate;
    FContainer.ScrollState := ssNone;
  end;
end;

procedure TScrollBtn.ScrollBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not FContainer.HotScroll then
  begin
    FPressState := psPressed;
    Invalidate;
    FContainer.ScrollState := FType;
  end;
end;

procedure TScrollBtn.ScrollBtnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (X >= 0) and (X < Width) and
    (Y >= 0) and (Y < Height) then
  begin
    MouseCapture   := True;
    FMouseOnButton := True;
    if FContainer.HotScroll then
    begin
      FContainer.ScrollState := FType;
      if not FContainer.FMouseOnScrollButton then
        FContainer.FScrollTimes := 0;
    end;
    FContainer.FTimer.Enabled := True;
    FContainer.FMouseOnScrollButton := True;
    Color := FColorAlt;
  end
  else
  begin
    MouseCapture   := False;
    FMouseOnButton := False;
    FPressState    := psReleased;

    FContainer.ScrollState    := ssNone;
    FContainer.FTimer.Enabled := False;
    FContainer.FMouseOnScrollButton := False;
    Color := FColor;
  end;
  Invalidate;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('MainMenuList unit initialization ...');
  {$ENDIF}
  ScrollBtnHeight := GetSystemMetrics(SM_CYHSCROLL);
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('MainMenuList unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

