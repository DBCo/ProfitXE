unit ColorComboBox;

interface

uses DeComboBox, Controls, Types, Classes, Windows, Graphics, StdCtrls, Commdlg, ActnList, messages, Vcl.Clipbrd,
     SysUtils, Contnrs, uIconManager, DeTypes;

type
  TDePalette = class;

  TLineWidthComboBox = class(TComboBox)
  private
    function getWidth: Integer;
    procedure setWidth(aWidth: Integer);
    procedure LComboBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  protected
    procedure CreateWnd; override;
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Value: Integer read getWidth write setWidth;
  end;

  TLineTypeComboBox = class(TComboBox)
  private
    function getType: Integer;
    procedure setType(aType: Integer);
    procedure LComboBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  protected
    procedure CreateWnd; override;
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Value: Integer read getType write setType;
  end;

  TPaletteItem = class;

  TDeComboEvent = procedure(Sender: TDePalette; withExtBorder: Boolean) of object;

  TPaletteItem = class(TObject)
  private
    FOwner: TObjectList;
    FRect: TRect;
    FCaption: String;
    FHint: String;
    FValue: Integer;
    FColor: TColor;
    FIndex: Integer;

    FHit: Boolean;
    FPressed: Boolean;
    FEnabled: Boolean;
    FOnPaint: TDeComboEvent;
  protected
    Procedure SetValue(aValue: Integer); virtual; //override;
    Procedure SetColor(aValue: TColor);
    Procedure SetHit(aValue: Boolean);
    Procedure SetPressed(aValue: Boolean);
    Procedure SetEnabled(aValue: Boolean);
  public
    constructor Create(AOwner: TObjectList); overload;
    procedure Repaint(withExtBorder: Boolean);
    procedure SetValueAndColor(aValue: Integer; aColor: TColor);
    procedure SetLTWH(aL, aT, aW, aH: Integer); virtual;
    procedure SetLTRB(aL, aT, aR, aB: Integer); virtual;

    property Index: Integer read FIndex write FIndex;
    property Rect: TRect read FRect write FRect;
    property Caption: string read FCaption write FCaption;
    property Hint: string read FHint write FHint;
    property Value: Integer read FValue write SetValue;
    property Color: TColor read FColor write SetColor;
    property Hit: Boolean read FHit write SetHit;
    property Pressed: Boolean read FPressed write SetPressed;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property onPaint: TDeComboEvent read FOnPaint write FOnPaint;
  end;

  TPaletteColorItem = class(TPaletteItem)
  protected
    procedure onPaintColor(Sender: TDePalette; withExtBorder: Boolean);
  public
    constructor Create(AOwner: TObjectList); overload;
  end;

  TPaletteTextItem = class(TPaletteItem)
  protected
    procedure onPaintText(Sender: TDePalette; withExtBorder: Boolean);
  public
    constructor Create(AOwner: TObjectList); overload;
  end;

  TPaletteLineItem = class(TPaletteItem)
  protected
    Procedure OnPaintLine(Sender: TDePalette; withExtBorder: Boolean);
  public
    constructor Create(AOwner: TObjectList); overload;
    constructor Create(AOwner: TObjectList; aValue: Integer; aColor: TColor; aL, aT, aW, aH: Integer); overload;
  end;

  TPaletteIconItem = class(TPaletteItem)
  private
    FIcon: TIcon;
    FIconSize: Integer;
    procedure SetIconSize(const Value: Integer);
  protected
    procedure PrepareIcon(aSize: Integer;  aCode: Integer; aIcon: TIcon);
    procedure SetLTWH(aL, aT, aW, aH: Integer); override;
    procedure SetLTRB(aL, aT, aR, aB: Integer); override;
    procedure SetValue(aValue: Integer); override;
    procedure onPaintIcon(Sender: TDePalette; withExtBorder: Boolean); virtual;
  public
    constructor Create(AOwner: TObjectList); overload;
    destructor Destroy; override;
    property IconSize: Integer read FIconSize write SetIconSize;
  end;

  TPaletteZoomIconItem = class(TPaletteIconItem)
  private
    FIconZoom: TIcon;
  protected
    procedure SetValue(aValue: Integer); override;
    procedure onPaintIcon(Sender: TDePalette; withExtBorder: Boolean); override;
  public
    constructor Create(AOwner: TObjectList); overload;
    destructor Destroy; override;
  end;

  TPaletteExampleItem = class(TPaletteIconItem)
  public
    constructor Create(AOwner: TObjectList); overload;
  end;

  TPalettePageItem = class(TPaletteIconItem)
  public
    constructor Create(AOwner: TObjectList); overload;
  end;

  TPaletteModeItem = class(TPaletteIconItem)
  public
    constructor Create(AOwner: TObjectList); overload;
  end;

  TPaletteItems = class(TObjectList)
  private
    FParentControl: TDePalette;
    function  GetItem(const Index: Integer): TPaletteItem;
    procedure PutItem(const Index: Integer; const Value: TPaletteItem);
  public
    constructor Create(aParentControl: TDePalette); overload;
    property Items[const Index : integer]: TPaletteItem read GetItem write PutItem;  default;
    property ParentControl: TDePalette read FParentControl;
  end;

  TDePalette = class(TCustomControl)
  protected
    FFocusedItem: TPaletteItem;
    FPaletteItems: TPaletteItems;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PaletteResize;

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    function PaintBoundsRect(Sender: TPaletteItem; withBounds: Boolean): TRect;
    procedure OnClear(Sender: TPaletteItem);
    procedure OnPaintButton(Sender: TPaletteItem);
    procedure OnClick(Sender: TPaletteItem); virtual;
  public
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

type
  TColorComboBox = class(TDeComboBox)
  private
    FColor: TColor;
    FUndoColor: TColor;
    FDefaultColor: TColor;
    FOnChange: TNotifyEvent;

    actChoose, actDefault, actCut, actCopy, actPaste, actUndo: TAction;
  protected
    procedure SetSubControlClasses; override;
    procedure InnerSetColor(Value: TColor); virtual;
    procedure SetColor(Value: TColor); virtual;
    procedure SetDefaultColor(Value: TColor); virtual;
    procedure SetCanTransparent(Value: Boolean); virtual;
    function GetCanTransparent: Boolean; virtual;

    procedure SetDefaultCaption(const Value: string);
    procedure SetAddColorCaption(const Value: string);
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure Change; override;

    procedure OnDSCChooseClick(Sender: TObject);
    procedure OnDSCDefaultClick(Sender: TObject);
    procedure OnDSCCutClick(Sender: TObject);
    procedure OnDSCCopyClick(Sender: TObject);
    procedure OnDSCPasteClick(Sender: TObject);
    procedure OnDSCUndoClick(Sender: TObject);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    property Color: TColor read FColor write SetColor;
    property DefaultColor: TColor read FDefaultColor write SetDefaultColor default clWindowText;
    property DefaultCaption: string write SetDefaultCaption;
    property AddColorCaption: string write SetAddColorCaption;
    property CanTransparent: Boolean read GetCanTransparent write SetCanTransparent;
  end;

  TColorIndicator = class(TCustomControl)
  protected
    procedure Paint; override;
  end;

  TColorPalette = class(TDePalette)
  private
    FDefault: TPaletteItem;
    FTransparent: TPaletteItem;
    FAdditional: TPaletteItem;
    FAdditionalButton: TPaletteItem;

    FCanTransparent: Boolean;
    FAddColorCaption: string;
  protected
    function SelectedColor: TColor;
    procedure OnClickAdditionColor(Sender: TPaletteItem);
    procedure OnClick(Sender: TPaletteItem); override;
  public
    constructor Create(AOwner: TComponent); override;
    property CanTransparent: Boolean read FCanTransparent write FCanTransparent;
  end;

  TIconComboBox = class(TDeComboBox)
  private
    FValue: Integer;
    FUndoValue: Integer;
    FOnChange: TNotifyEvent;
    FStackIcons: TStackIcons;
    actDelete, actCut, actCopy, actPaste, actUndo: TAction;
  protected
    procedure SetSubControlClasses; override;
    procedure InnerSetValue(aValue: Integer); virtual;
    procedure SetValue(aValue: Integer); virtual;

    procedure Change; override;
    procedure OnDSCDeleteClick(Sender: TObject);
    procedure OnDSCCutClick(Sender: TObject);
    procedure OnDSCCopyClick(Sender: TObject);
    procedure OnDSCPasteClick(Sender: TObject);
    procedure OnDSCUndoClick(Sender: TObject);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Value: Integer read FValue write SetValue;
    property StackIcons: TStackIcons read FStackIcons;
  end;

  TIconPalette = class(TDePalette)
  private
    CodeText             : TPaletteTextItem;
    TL,    T,   TR,   TT,
    L1, {Ico1}  R1,
    CL,    C,   CR,
    L2, {Ico2}  R2,
    BL,    B,   BR,
                R3,
                DR,   DD : TPaletteLineItem;
  protected
    FNumber: Integer;
    procedure OnClick(Sender: TPaletteItem); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TIconIndicator = class(TCustomControl)
  protected
    procedure Paint; override;
  end;


implementation

uses menus, Dialogs,
     DeMeta, funcs, uIconUtils, math, db, DeLog, DataUnit;
{ TLineWidthComboBox }

function TLineWidthComboBox.getWidth: Integer;
begin
  Result := ItemIndex + 1;
end;

procedure TLineWidthComboBox.setWidth(aWidth: Integer);
begin
  if aWidth in [1 .. 8] then
    ItemIndex := aWidth - 1;
end;

constructor TLineWidthComboBox.Create(AOwner: TComponent);
begin
  inherited;
  OnDrawItem := LComboBoxDrawItem;
  Style := csOwnerDrawFixed;
end;

procedure TLineWidthComboBox.CreateWnd;
var i:Integer;
begin
  inherited;
  for i := 1 to 8 do
    Items.Add(IntToStr(i));
end;

destructor TLineWidthComboBox.Destroy;
begin
  inherited;
end;

procedure TLineWidthComboBox.LComboBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var x   : Integer;
    Box : TComboBox;
begin
  Box:=TComboBox(Control);
  if odComboBoxEdit in State then x:=Rect.Left+2 else x:=Rect.Left+1;

  if Not Box.Enabled then
    Box.Canvas.Pen.Color:=clSilver
  else
  if odSelected in State then
    Box.Canvas.Pen.Color:=clWhite
  else
    Box.Canvas.Pen.Color:=clBlack;

  Box.Canvas.Font.Color :=Box.Canvas.Pen.Color;
  Box.Canvas.TextRect(Rect, Rect.Right-30+x, Rect.Top+1, IntToStr(Index+1)+' pt');

  Box.Canvas.Pen.Width:=1;
  Box.Canvas.Pen.Style:=psSolid;
  Box.Canvas.Brush.Color:=Box.Canvas.Pen.Color;
  Box.Canvas.Brush.Style:=bsSolid;
  Box.Canvas.Rectangle(Rect.Left+4,     Rect.Top+7-(Index div 2),
                       Rect.Right-33+x, Rect.Top+8-(Index div 2)+Index);
end;

{TLineTypeComboBox}

function TLineTypeComboBox.getType: Integer;
begin
  Result:=ItemIndex;
end;

procedure TLineTypeComboBox.setType(aType: Integer);
begin
  if aType in [0..4] then
    ItemIndex:=aType;
end;

constructor TLineTypeComboBox.Create(AOwner: TComponent);
begin
  inherited;
  OnDrawItem:=LComboBoxDrawItem;
  Style:=csOwnerDrawFixed;
end;

procedure TLineTypeComboBox.CreateWnd;
var i:Integer;
begin
  inherited;
  for i:=0 to 4 do
    Items.Add(IntToStr(i));
end;

destructor TLineTypeComboBox.Destroy;
begin
  inherited;
end;

procedure TLineTypeComboBox.LComboBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var Box : TComboBox;
    lb  : TLogBrush;
    OldPen,Pen: HPen;
begin
  Box:=TComboBox(Control);

  lb.lbStyle := BS_SOLID;
  lb.lbHatch := 0;

  if Not Box.Enabled then
    lb.lbColor := clSilver
  else
  if odSelected in State then
    lb.lbColor := clWhite
  else
    lb.lbColor := clBlack;

  OldPen:=Box.Canvas.Pen.Handle;

  Pen:= ExtCreatePen(PS_GEOMETRIC or (PS_SOLID+Index) or PS_JOIN_ROUND, 3, lb, 0, nil);

  Box.Canvas.Font.Color :=lb.lbColor;
  Box.Canvas.TextRect(Rect, Rect.Left, Rect.Top, EmptyStr);

  Box.Canvas.Pen.Handle :=Pen;
  Box.Canvas.MoveTo(Rect.Left+4, Rect.Top+7);
  Box.Canvas.LineTo(Rect.Right-4, Rect.Top+7);

  Box.Canvas.Pen.Handle := OldPen;
  DeleteObject(Pen);
end;

{ TColorComboBox }

const
  rDI            =  4;
  rStepY         =  8;
  rStepX         = 16;

  rColorHeight   = 15;
  rColorWidth    = 15;
  rColorInRow    = 16;

  rIconHeight    = 22;
  rIconWidth     = 22;

  rPartHeight    = 24;
  rPartWidth     = 64;

  rModeHeight    = 36;
  rModeWidth     = 28;

  rXCount        = 16;
  rYCount        = 15;

  OrderColor: array[0..15] of byte = ( 0, 1, 2, 3,11,12,13,14,15, 4, 5, 6, 7, 8, 9,10);
  OrderMode:  array[0..15] of byte = (15, 0,10, 5,11,12,13,14, 1, 2, 3, 4, 6, 7, 8, 9);
  ShiftMode:  array[0..15] of byte = ( 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3);

  rV: array[0..rYCount - 1,0..rXCount - 1] of byte

                 = ((000,001,002,151,101,153,154,152,019,074,150,039,075,190,029,157),
                    (127,135,136,137,030,021,026,032,145,144,146,112,111,113,114,160),
                    (005,003,004,141,175,044,142,058,059,037,041,057,010,017,128,155),

                    (006,040,033,035,170,115,147,009,007,053,045,134,133,130,031,132),
                    (043,179,178,051,052,047,173,174,176,177,180,055,171,018,014,172),
                    (027,109,098,110,103,097,049,100,050,139,104,105,140,023,159,016),

                    (013,054,191,163,099,158,038,131,129,008,046,048,184,138,182,025),
                    (034,162,183,181,024,020,149,015,093,094,148,011,042,012,076,077),
                    (213,214,215,216,217,000,000,000,000,000,161,156,028,102,078,079),
                    (068,069,185,186,187,065,000,061,060,062,080,081,072,073,082,083),
                    (064,091,092,088,089,090,036,084,085,086,087,168,167,165,166,022),

                    (122,123,120,121,096,056,117,116,108,107,119,106,118,124,095,125),
                    (210,209,211,212,207,126,218,219,220,221,222,223,000,000,000,208),
                    (224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239),
                    (240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255));

procedure TColorComboBox.SetSubControlClasses;
begin
  FEditClass := TColorIndicator;
  FListClass := TColorPalette;
end;

constructor TColorComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FColor := clWhite;
  SetCanTransparent(False);
  SetDefaultColor(clWindowText);
  
  actChoose := ActionList.CreateAction('_Da.Choose', OnDSCChooseClick, EmptyStr, True);
  actDefault := ActionList.CreateAction('_Da.Default', OnDSCDefaultClick);
  ActionList.CreateAction(cLineCaption, nil);
  actCut := ActionList.CreateAction('_Da.Cut', OnDSCCutClick, 'Ctrl+X');
  actCopy := ActionList.CreateAction('_Da.Copy', OnDSCCopyClick, 'Ctrl+C');
  actPaste := ActionList.CreateAction('_Da.Paste', OnDSCPasteClick, 'Ctrl+V');
  ActionList.CreateAction(cLineCaption, nil);
  actUndo := ActionList.CreateAction('_Da.Undo', OnDSCUndoClick, 'Ctrl+Z');
end;

procedure TColorComboBox.InnerSetColor(Value: TColor);
var ColorChanged : boolean;
    i,Ind : Integer;
begin
  ColorChanged := FColor <> Value;
  FColor := Value;
  FEdit.Invalidate;

  Ind:=-1;
  for i:=0 to Pred(TColorPalette(FList).FPaletteItems.Count) do
    if (TColorPalette(FList).FPaletteItems[i].FColor = FColor) then
      begin
        TColorPalette(FList).FPaletteItems[i].Pressed := True;
        Ind:=i;
      end
    else
      begin  
        TColorPalette(FList).FPaletteItems[i].Pressed := false;
      end;

  // не нашли устанавливаем добавочный цвет
  if Ind=-1 then
    begin
      TColorPalette(FList).FAdditional.Color:= FColor;
      TColorPalette(FList).FAdditional.Pressed:= True;
    end;

  if ColorChanged then
    Change;
end;

procedure TColorComboBox.SetColor(Value: TColor);
begin
  FUndoColor := Value;
  InnerSetColor(Value);
end;

procedure TColorComboBox.SetDefaultColor(Value: TColor);
begin
  TColorPalette(FList).FDefault.Color := Value;
end;

procedure TColorComboBox.SetCanTransparent(Value: Boolean);
begin
  TColorPalette(FList).CanTransparent := Value;
end;

function TColorComboBox.GetCanTransparent: Boolean;
begin
  Result := TColorPalette(FList).CanTransparent;
end;

procedure TColorComboBox.SetDefaultCaption(const Value: string);
begin
  TColorPalette(FList).FDefault.Caption := Value;
end;

procedure TColorComboBox.SetAddColorCaption(const Value: string);
begin
  TColorPalette(FList).FAdditionalButton.Caption := Value;
end;

procedure TColorComboBox.WMMouseWheel(var Msg: TWMMouseWheel);
// var I,Ind: Integer;
begin
{
  Ind:= -1;
  for i:=Low(ColArray) to High(ColArray) do
    if ColArray[i] = Color then
      begin
        Ind:=i;
        Break;
      end;

  if Msg.WheelDelta < 0 then
    Inc(Ind)
  else
    Dec(Ind);

  if (Low(ColArray)<=Ind) and (Ind<=High(ColArray)) then InnerSetColor(ColArray[Ind]);
  }
end;

procedure TColorComboBox.Change;
var
  v: Variant;
  c: TColor;
begin
  inherited;
  actCut.Enabled := False;
  actPaste.Enabled := DeClipboard.ReadValue(v, ftInteger, False) or StringToColor(DeClipboard.AsText, c);
  actUndo.Enabled := not(FUndoColor = FColor);

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TColorComboBox.OnDSCChooseClick(Sender: TObject);
var
  ChColStruct: TChooseColor;
  ColAr: array [0 .. 15] of COLORREF;
begin
  inherited;
  ZeroMemory(@ColAr[0], SizeOf(ColAr));
  ChColStruct.lStructSize := SizeOf(TChooseColor);
  ChColStruct.hWndOwner := Parent.Handle;
  ChColStruct.lpCustColors := @ColAr[0];
  ChColStruct.Flags := CC_FULLOPEN;
  if ChooseColor(ChColStruct) then
    InnerSetColor(ChColStruct.rgbResult);
end;

procedure TColorComboBox.OnDSCDefaultClick(Sender: TObject);
begin
  inherited;
  InnerSetColor(FDefaultColor);
  Change;
end;

procedure TColorComboBox.OnDSCCutClick(Sender: TObject);
begin
  inherited;
  //
end;

procedure TColorComboBox.OnDSCCopyClick(Sender: TObject);
begin
  inherited;
  DeClipboard.WriteValue(Color, ColorToString(Color));
  Change;
end;

procedure TColorComboBox.OnDSCPasteClick(Sender: TObject);
var
  v: Variant;
  C: TColor;
begin
  inherited;
  if DeClipboard.ReadValue(v, ftInteger, False) then InnerSetColor(v) else
  if StringToColor(DeClipBoard.AsText, C) then
   InnerSetColor(C);

  Change;
end;

procedure TColorComboBox.OnDSCUndoClick(Sender: TObject);
begin
  inherited;
  InnerSetColor(FUndoColor);
end;

{ TDePalette }

constructor TDePalette.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csNoDesignVisible] - [csCaptureMouse];
  Color := clWhite;
  FPaletteItems:= TPaletteItems.Create(self);
  FFocusedItem:= nil;
end;

destructor TDePalette.Destroy;
begin
  FPaletteItems.Free;
  inherited;
end;

procedure TDePalette.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_POPUP or WS_BORDER;
  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;
end;

procedure TDePalette.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not PtInRect(ClientRect, Point(X, Y)) then
    TDeComboBox(Parent).DroppedDown := False;
end;

procedure TDePalette.MouseMove(Shift: TShiftState; X, Y: Integer);
var i : Integer;
    FNewFocusedItem: TPaletteItem;
begin
  inherited;
  FNewFocusedItem:= nil;
  for i:= 0 to Pred(FPaletteItems.Count) do
    if (PtInRect(FPaletteItems[i].Rect, Point(X, Y))) then
      begin
        if FPaletteItems[i].Enabled then
          FNewFocusedItem:= FPaletteItems[i];
        Break;
      end;

  // убираем фокус-рамку с предыдущего фокусного элемента
  if assigned(FFocusedItem) then
    if not assigned(FNewFocusedItem) or (FFocusedItem <> FNewFocusedItem) then
        begin
          FFocusedItem.Hit:= False;
          FFocusedItem:= nil;
        end;

  if assigned(FNewFocusedItem) then
    begin
      FNewFocusedItem.Hit:= True;
      FFocusedItem:= FNewFocusedItem;
    end;
end;

procedure TDePalette.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i : Integer;
begin
  inherited;
  if PtInRect(ClientRect, Point(X, Y)) then
    for  i := 0 to Pred(FPaletteItems.Count) do
      if PtInRect(FPaletteItems[i].Rect, Point(X, Y)) then
        begin
          if FPaletteItems[i].Enabled then OnClick(FPaletteItems[i]);
          Break;
        end;
end;

procedure TDePalette.Paint;
var i: Integer;
begin
  inherited;

  for i := 0 to Pred(FPaletteItems.Count) do
    if assigned(FPaletteItems[i].FOnPaint) then FPaletteItems[i].FOnPaint(Self, False);
end;

procedure TDePalette.PaletteResize;
var i : Integer;
    R: TRect;
begin
  if 0 < FPaletteItems.Count then
    begin
      R:=FPaletteItems[0].Rect;
      for i:= 1 to Pred(FPaletteItems.Count) do
        R:= TRect.Union(R, FPaletteItems[i].Rect);

      Width  := R.Left + R.Right;
      Height := R.Top  + R.Bottom;
    end;
end;

function TDePalette.PaintBoundsRect(Sender: TPaletteItem; withBounds: Boolean): TRect;
var i: Integer;
begin
  if not assigned(Parent) then Exit;

  Result:= Sender.Rect;
  Canvas.Brush.Style:= bsSolid;

  if Sender.Hit and Sender.Pressed then
    begin
      InflateRect(Result, rDI, rDI);
      Canvas.Pen.Color:= clHighlight;
      Canvas.Brush.Color:= clActiveCaption;
      Canvas.Rectangle(Result);
    end else
  if Sender.Hit then
    begin
      InflateRect(Result, rDI, rDI);
      Canvas.Pen.Color:= clActiveCaption;
      Canvas.Brush.Color:= Color;
      Canvas.Rectangle(Result);
    end else
  if  Sender.Pressed then
    begin
      if withBounds then
        begin
          Canvas.Pen.Color:= Color;
          Canvas.Brush.Style:= bsClear;
          InflateRect(Result, rDI, rDI);
          for i:=1 to rDI do
              begin
                Canvas.Rectangle(Result);
                InflateRect(Result, -1, -1);
              end;
          Canvas.Brush.Style:= bsSolid;
        end;
      Canvas.Pen.Color:= clActiveCaption;
      Canvas.Brush.Color:= clActiveCaption;
      Canvas.Rectangle(Result);
    end else
    begin
      if withBounds then
        InflateRect(Result, rDI, rDI);
      Canvas.Pen.Color:= Color;
      Canvas.Brush.Color:= Color;
      Canvas.Rectangle(Result);
    end;
end;

procedure TDePalette.OnClear(Sender: TPaletteItem);
var R: TRect;
begin
  R := Sender.Rect;

  Canvas.Pen.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.Rectangle(Rect(R.Left, R.Top, R.Left + R.Height, R.Bottom));
end;

procedure TDePalette.OnPaintButton(Sender: TPaletteItem);
var R: TRect;
begin
  if not assigned(Parent) or not HandleAllocated then Exit;

  R:= PaintBoundsRect(Sender, False);
  InflateRect(R, -1, -1);

  Canvas.Brush.Style := bsSolid;
  Canvas.TextOut(R.Left + (R.Width  - Canvas.TextWidth(Sender.Caption)) div 2,
                 R.Top  + (R.Height - Canvas.TextHeight(Sender.Caption)) div 2, Sender.Caption);
end;

procedure TDePalette.OnClick(Sender: TPaletteItem);
begin
 //
end;

{ TColorPalette }

constructor TColorPalette.Create(AOwner: TComponent);
var  i,ind, R,G,B,H,L,S : Integer;
     c: TColor;
begin
  inherited;

  FDefault := TPaletteColorItem.Create(FPaletteItems);
  FDefault.SetLTWH( rStepX, rStepY, rColorInRow*rColorWidth, rColorHeight);
  FDefault.SetValueAndColor(-1, clNone);
  FDefault.FCaption:='Auto';

  ind:= 0;
  for i := 0 to rColorInRow - 1 do
    with TPaletteColorItem.Create(FPaletteItems) do
      begin
        FIndex:= Ind;
        C:= SchemaColors[i];
        RGBtoHLS(GetRValue(C),GetGValue(C),GetBValue(C),H,L,S);
        if S=0 then HLStoRGB(H,80,S,R,G,B)
               else HLStoRGB(H,100,100,R,G,B);
        SetValueAndColor(Ind, RGB(R,G,B) );
        SetLTWH(rStepX + i*rColorWidth, rStepY + 1*rColorHeight, rColorWidth, rColorHeight);
        Inc(Ind);
      end;

  for i := 0 to rColorInRow - 1 do
    with TPaletteColorItem.Create(FPaletteItems) do
      begin
        FIndex:= Ind;
        SetValueAndColor(Ind, SchemaColorsRGB[i]);
        SetLTWH(rStepX + i*rColorWidth, rStepY + 2*rColorHeight, rColorWidth, rColorHeight);
        Inc(Ind);
      end;

  for i := 0 to rColorInRow - 1 do
    with TPaletteColorItem.Create(FPaletteItems) do
      begin
        FIndex:= Ind;
        C:= SchemaColors[i];
        RGBtoHLS(GetRValue(C),GetGValue(C),GetBValue(C),H,L,S);
        if S=0 then HLStoRGB(H,180,S,R,G,B)
               else HLStoRGB(H,L,240,R,G,B);
        SetValueAndColor(Ind, RGB(R,G,B) );
        SetLTWH(rStepX + i*rColorWidth, rStepY + 3*rColorHeight, rColorWidth, rColorHeight);
        Inc(Ind);
      end;

    with TPaletteColorItem.Create(FPaletteItems) do
      begin
        FIndex:= Ind;
        SetValueAndColor(Ind, clWhite);
        SetLTWH(rStepX, rStepY + 4*rColorHeight, rColorWidth, rColorHeight);
        Inc(Ind);
      end;

  FAdditional := TPaletteColorItem.Create(FPaletteItems);
  FAdditionalButton := TPaletteTextItem.Create(FPaletteItems);
  FAdditionalButton.Caption:= 'Other colors...';

  if FCanTransparent then
    begin
      FTransparent := TPaletteColorItem.Create(FPaletteItems);
      FTransparent.SetLTWH( rStepX + 1*rColorWidth, rStepY + 4*rColorHeight, rColorWidth, rColorHeight);
      FTransparent.SetValueAndColor( -1, clNone);

      FAdditional.SetLTWH(rStepX + 2*rColorWidth, rStepY + 4*rColorHeight, rColorWidth, rColorHeight);
      FAdditionalButton.SetLTRB(rStepX + 3*rColorWidth, rStepY + 4*rColorHeight,
                      rStepX + rColorInRow*rColorWidth, rStepY + 5*rColorHeight);
    end
  else
    begin
      FAdditional.SetLTWH(rStepX + 1*rColorWidth, rStepY + 4*rColorHeight, rColorWidth, rColorHeight);
      FAdditionalButton.SetLTRB(rStepX + 2*rColorWidth, rStepY + 4*rColorHeight,
                                rStepX + rColorInRow*rColorWidth, rStepY + 5*rColorHeight);
    end;

  PaletteResize;
end;

function TColorPalette.SelectedColor: TColor;
var i: Integer;
begin
  Result := clNone;
  for i := 0 to Pred(FPaletteItems.Count) do
    if FPaletteItems[i].Pressed then
      begin
        Result:=FPaletteItems[i].Color;
        Break;
      end;
end;

procedure TColorPalette.OnClickAdditionColor(Sender: TPaletteItem);
var
  ChColStruct: TChooseColor;
  ColAr: array [0 .. 15] of COLORREF;
begin
  inherited;

  ZeroMemory(@ColAr[0], SizeOf(ColAr));
  TColorComboBox(Parent).CloseUp(False);
  ChColStruct.lStructSize := SizeOf(TChooseColor);
  ChColStruct.hWndOwner := Parent.Handle;
  ChColStruct.lpCustColors := @ColAr[0];
  ChColStruct.Flags := CC_FULLOPEN;
  if ChooseColor(ChColStruct) then
    begin
      FAdditional.Color := ChColStruct.rgbResult;
      FAdditional.Pressed:=True;
      TColorComboBox(Parent).Color := FAdditional.Color;
    end
  else
    TColorComboBox(Parent).CloseUp(False);
end;

procedure TColorPalette.OnClick(Sender: TPaletteItem);
begin
  inherited;
  if Not Sender.Enabled then {nothing}
    else
    
  if (Sender is TPaletteColorItem) then
    begin
      TColorComboBox(Parent).InnerSetColor(Sender.Color);
      TColorComboBox(Parent).CloseUp(True);
    end else

  if (Sender is TPaletteTextItem) then
    begin
      OnClickAdditionColor(Sender);
    end else
  
end;

{ TColorIndicator }

procedure TColorIndicator.Paint;
var
  R: TRect;
begin
  inherited;
  R := BoundsRect;
  Canvas.Brush.Style := bsClear;

  if Parent.Focused then
    begin
      Canvas.Pen.Color   := clHighlight;    Canvas.Rectangle(R);    InflateRect(R, -1, -1);
      Canvas.Pen.Color   := clHighlight;    Canvas.Rectangle(R);    InflateRect(R, -1, -1);
      Canvas.Pen.Color   := Color;
    end
  else
    begin
      Canvas.Pen.Color   := Color;          Canvas.Rectangle(R);     InflateRect(R, -1, -1);
      Canvas.Pen.Color   := Color;          Canvas.Rectangle(R);     InflateRect(R, -1, -1);
      Canvas.Pen.Color   := clGray;
    end;

  Canvas.Brush.Style := bsSolid;
  if TColorComboBox(Parent).Color = clNone then
    Canvas.Brush.Color := clWhite
  else
    Canvas.Brush.Color := TColorComboBox(Parent).Color;
  Canvas.Rectangle(R);
end;

{ TIconComboBox }

procedure TIconComboBox.Change;
begin
  inherited;

  actDelete.Enabled:= not (FValue = 0 );
  actCut.Enabled:= not (FValue = 0 );
  actCopy.Enabled:= True;
  actPaste.Enabled:= StringToIcon(DeClipboard.AsText, FValue);
  actUndo.Enabled:= not(FValue = FUndoValue);

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TIconComboBox.Create(AOwner: TComponent);
begin
  inherited;
  actDelete := ActionList.CreateAction('_Da.Delete', OnDSCDeleteClick);
  ActionList.CreateAction(cLineCaption, nil);
  actCut := ActionList.CreateAction('_Da.Cut', OnDSCCutClick, 'Ctrl+X');
  actCopy := ActionList.CreateAction('_Da.Copy', OnDSCCopyClick, 'Ctrl+C');
  actPaste := ActionList.CreateAction('_Da.Paste', OnDSCPasteClick, 'Ctrl+V');
  ActionList.CreateAction(cLineCaption, nil);
  actUndo := ActionList.CreateAction('_Da.Undo', OnDSCUndoClick, 'Ctrl+Z');
  FStackIcons:= DM.IconManager.CreateStackIcons(0);
end;

destructor TIconComboBox.Destroy;
begin
  if assigned(FStackIcons) then FStackIcons.Free;
  inherited;
end;

procedure TIconComboBox.SetSubControlClasses;
begin
  inherited;
  FEditClass := TIconIndicator;
  FListClass := TIconPalette;
end;

procedure TIconComboBox.InnerSetValue(aValue: Integer);
var aChanged : boolean;
    i : Integer;
    Icon_, Color_, IsSymbol_, Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse: Byte;
begin
  aChanged := FValue <> aValue;
  if aChanged then
    begin
      FreeAndNil(FStackIcons);
      FStackIcons:= DM.IconManager.CreateStackIcons(aValue);
    end;

  FValue := aValue;
  FEdit.Repaint;

  DecodeIcon(aValue ,Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse);
  if TIconPalette(FList).FNumber = 1 then
    begin
      Icon_    := Icon1;
      Color_   := Color1;
      IsSymbol_:= IsSymbol1;
      TIconPalette(FList).TL.SetValueAndColor(6, clGray);       TIconPalette(FList).TR.SetValueAndColor(10, clGray);
      TIconPalette(FList).L1.SetValueAndColor(5, clGray);       TIconPalette(FList).R1.SetValueAndColor( 0, clGray);
      TIconPalette(FList).CL.SetValueAndColor(3, clGray);       TIconPalette(FList).CR.SetValueAndColor(12, clGray);
      TIconPalette(FList).L2.SetValueAndColor(5, clSilver);     TIconPalette(FList).R2.SetValueAndColor( 5, clGray);
      TIconPalette(FList).BL.SetValueAndColor(3, clSilver);     TIconPalette(FList).BR.SetValueAndColor( 5, clGray);

      TIconPalette(FList).T.SetValueAndColor(10, clGray);       TIconPalette(FList).B.SetValueAndColor(10, clSilver);
    end
  else
    begin
      Icon_    := Icon2;
      Color_   := Color2;
      IsSymbol_:= IsSymbol2;
      TIconPalette(FList).TL.SetValueAndColor(6, clSilver);     TIconPalette(FList).TR.SetValueAndColor( 6, clGray);
      TIconPalette(FList).L1.SetValueAndColor(5, clSilver);     TIconPalette(FList).R1.SetValueAndColor( 5, clGray);
      TIconPalette(FList).CL.SetValueAndColor(6, clGray);       TIconPalette(FList).CR.SetValueAndColor( 9, clGray);
      TIconPalette(FList).L2.SetValueAndColor(5, clGray);       TIconPalette(FList).R2.SetValueAndColor( 0, clRed);
      TIconPalette(FList).BL.SetValueAndColor(3, clGray);       TIconPalette(FList).BR.SetValueAndColor(12, clGray);

      TIconPalette(FList).T.SetValueAndColor(10, clSilver);     TIconPalette(FList).B.SetValueAndColor(10, clGray);
    end;

  TIconPalette(FList).CodeText.FCaption:= IntToStr(aValue);
  TIconPalette(FList).CodeText.Repaint(False);

  for i:=0 to Pred(TIconPalette(FList).FPaletteItems.Count) do
    try
      if (TIconPalette(FList).FPaletteItems[i] is TPalettePageItem) then
        begin
          if (TIconPalette(FList).FPaletteItems[i].Index = 1) then
              if Icon1=0 then TIconPalette(FList).FPaletteItems[i].Value := EncodeIcon(Icon1, 2)
                         else TIconPalette(FList).FPaletteItems[i].Value := EncodeIcon(Icon1, Color1);
          if (TIconPalette(FList).FPaletteItems[i].Index = 2) then
              if Icon2=0 then TIconPalette(FList).FPaletteItems[i].Value := EncodeIcon(Icon2, 2)
                         else TIconPalette(FList).FPaletteItems[i].Value := EncodeIcon(Icon2, Color2);
        end else

      if (TIconPalette(FList).FPaletteItems[i] is TPaletteModeItem) then
        begin
          TIconPalette(FList).FPaletteItems[i].Pressed := (TIconPalette(FList).FPaletteItems[i].Index = Combination);
        end else

      if (TIconPalette(FList).FPaletteItems[i] is TPaletteColorItem) then
        begin
          TIconPalette(FList).FPaletteItems[i].Pressed := (TIconPalette(FList).FPaletteItems[i].Index = Color_)
        end else

     if (TIconPalette(FList).FPaletteItems[i] is TPaletteExampleItem) then
        begin
          TIconPalette(FList).FPaletteItems[i].Value := aValue;
        end else

      if (TIconPalette(FList).FPaletteItems[i] is TPaletteIconItem) then
        begin
          TIconPalette(FList).FPaletteItems[i].Value   :=  EncodeIcon(TIconPalette(FList).FPaletteItems[i].Index, Color_);
          TIconPalette(FList).FPaletteItems[i].Pressed := (TIconPalette(FList).FPaletteItems[i].Index = Icon_);
        end else
    except
      showmessage(intToStr(i));
    end;

  if aChanged then
    Change;
end;

procedure TIconComboBox.OnDSCDeleteClick(Sender: TObject);
begin
  InnerSetValue(0);
end;

procedure TIconComboBox.OnDSCCopyClick(Sender: TObject);
begin
  DeClipboard.WriteValue(FValue, IconToString(FValue));
end;

procedure TIconComboBox.OnDSCCutClick(Sender: TObject);
begin
  DeClipboard.WriteValue(FValue, IconToString(FValue));
  InnerSetValue(0);
end;

procedure TIconComboBox.OnDSCPasteClick(Sender: TObject);
var V: Integer;
begin
  if StringToIcon(DeClipboard.AsText, V) then
    InnerSetValue(V);
end;

procedure TIconComboBox.OnDSCUndoClick(Sender: TObject);
begin
  SetValue(FUndoValue);
end;

procedure TIconComboBox.SetValue(aValue: Integer);
begin
  FUndoValue := FValue;
  InnerSetValue(aValue);
end;

{ TIconIndicator }

procedure TIconIndicator.Paint;
var
  R: TRect;
  iSize: Integer;
  Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse: Byte;
  FStackIcon: TStackIcon;
begin
  inherited;
  R := BoundsRect;
  InflateRect(R, -1, -1);

  iSize:= DM.GetIconSizeForSize( min(R.Width, R.Height));
  DecodeIcon(TIconComboBox(Parent).Value ,Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse);

  if Parent.Focused then Canvas.Brush.Color := clHighlight
                    else Canvas.Brush.Color := Color;
  Canvas.FillRect(R);

  FStackIcon:= TIconComboBox(Parent).StackIcons.Find(iSize, iSize);
  if Assigned(FStackIcon) then
    Canvas.Draw(R.Left + (R.Width - iSize) div 2, R.Top + (R.Height - iSize) div 2, FStackIcon.Icon );
end;

{ TIconPalette }

procedure TIconPalette.OnClick(Sender: TPaletteItem);
var
  Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse: Byte;
  Clipboard: TClipboard;
begin


  inherited;
  if Not Sender.Enabled then {nothing}
    else
  if Sender is TPaletteTextItem then
    try
      Clipboard:= TClipboard.Create;
      Clipboard.Open;
      ClipBoard.AsText:= IntToStr(TIconComboBox(Parent).Value);
    finally
      Clipboard.Close;
    end else

  if (Sender is TPalettePageItem) then
    begin
      FNumber:= TPaletteItem(Sender).Index;
      TIconComboBox(Parent).Value:= TIconComboBox(Parent).Value;
    end else

  if (Sender is TPaletteModeItem) then
    begin
      DecodeIcon(TIconComboBox(Parent).Value ,Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse);
      Combination:= TPaletteItem(Sender).Index;
      TIconComboBox(Parent).Value:= EncodeIcon(Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse );
    end else

  if (Sender is TPaletteColorItem) then
    begin
      DecodeIcon(TIconComboBox(Parent).Value ,Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse);
      case FNumber of
        1:  Color1:= TPaletteItem(Sender).Index;
        2:  Color2:= TPaletteItem(Sender).Index;
      end;
      TIconComboBox(Parent).Value:= EncodeIcon(Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse );
    end else

  if (Sender is TPaletteIconItem) then
    begin
      DecodeIcon(TIconComboBox(Parent).Value ,Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse);
      case FNumber of
        1:  Icon1:= TPaletteItem(Sender).Index;
        2:  Icon2:= TPaletteItem(Sender).Index;
      end;
      TIconComboBox(Parent).Value:= EncodeIcon(Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse );
    end;
end;

constructor TIconPalette.Create(AOwner: TComponent);
const Z : array[0..4] of integer = (64, 48, 32, 24, 16);
var i, n, X, Y, ModeMaxX, ExampleMaxY: Integer;
    FCount: Integer;
begin
  inherited;

  for i:= 1 to 15 do
    with TPaletteModeItem.Create(FPaletteItems) do
      begin
        SetLTWH( 6 + rStepX + i * (rModeWidth + rDI) {+ 2*ShiftMode[i]}, rStepY, rModeWidth, rModeHeight);
        IconSize:= 24;
        ModeMaxX:= Rect.Right;
        FIndex:= OrderMode[i];
        Value:= EncodeIcon( 187,0,0,136,0,0,OrderMode[i],0 );
      end;

  FNumber:=1;

  for i:=0 to 1 do
    with TPalettePageItem.Create(FPaletteItems) do
      begin
        SetLTWH( rStepX + 6 + rDI, 3*rStepY + 2*i*rStepY + rModeHeight + i*rIconHeight - 1, rPartWidth, rPartHeight);
        FIndex:= Succ(i);
      end;

  Y:= 9*rStepY + 2*rPartHeight + rModeHeight;

  for i := Low(z) to High(z) do
    with TPaletteExampleItem.Create(FPaletteItems) do
      begin
        SetLTWH(rStepX + rDI, Y, 64+2*rDI,  z[i]+4 );
        ExampleMaxY:= Rect.Bottom;
        Value:= 7+256*15;
        Inc(Y, z[i] + rStepY);
      end;

  FCount:= rYCount * rXCount;
  for y := 0 to Pred(rYCount) do
    for x := 0 to Pred(rXCount) do
      begin
        n:= rV[ y, x ];
        if ( (0 < n) or (( y = 0 ) and (x = 0))) and ( 0 <= DM.IconManager.MapIcon(n) ) then
          with TPaletteZoomIconItem.Create(FPaletteItems) do
            begin
              SetLTWH(64 + 3*rStepX + x*(rIconWidth + rDI),
                           3*rStepY + rModeHeight + y*(rIconHeight + rDI), rIconWidth, rIconHeight);
              FIndex:= n;
           end;
      end;

  CodeText:= TPaletteTextItem.Create(FPaletteItems);
  CodeText.FCaption:= '';
  CodeText.SetValue(0);
  CodeText.SetLTWH(rStepX + rDI,
                   4*rStepY + rModeHeight + rYCount*(rIconHeight + rDI) + rDI,
                   64+2*rDI,
                   rIconHeight);

  for i:= 0 to 0 do
      with TPaletteColorItem.Create(FPaletteItems) do
        begin
          FIndex:= i;
          SetValueAndColor(I, SchemaColorsRGB[i]);

          SetLTWH(64 + 3*rStepX + OrderColor[i]*(rIconWidth + rDI),
                  4*rStepY + rModeHeight + rYCount*(rIconHeight + rDI) + rDI, rIconWidth {+rIconWidth+rDI}, rIconHeight);
        end;

  for i:= 1 to 15 do
      with TPaletteColorItem.Create(FPaletteItems) do
        begin
          FIndex:= i;
          SetValueAndColor(I, SchemaColorsRGB[i]);

          SetLTWH(64 + 3*rStepX + OrderColor[i]*(rIconWidth + rDI),
                  4*rStepY + rModeHeight + rYCount*(rIconHeight + rDI) + rDI, rIconWidth, rIconHeight);
        end;

//    TL,    T,   TR,   TT,
//    L1, {Ico1}  R1,                               3*rTopOffset + (rModeHeight + 2*rDY) + (j+1)*(rIconHeight + 2*rDY)+ rColorHeight + 2*rDY
//    CL,    C,   CR,
//    L2, {Ico2}  R2,
//    BL,    B,   BR,
//                R3,
//                DR,   DD : TPaletteLineItem;

  TL:= TPaletteLineItem.Create(FPaletteItems,  5, clGray, rStepX, rModeHeight + 2*rStepY - 3, 6, 6);
  T := TPaletteLineItem.Create(FPaletteItems, 10, clGray, TL.Rect.Right, TL.Rect.Top, 64 + rStepX  - 3, 6);
  TR:= TPaletteLineItem.Create(FPaletteItems, 10, clGray, T.Rect.Right, TL.Rect.Top, 6, 6);
  TT:= TPaletteLineItem.Create(FPaletteItems, 10, clGray, TR.Rect.Right, TL.Rect.Top,
                                                Max( ModeMaxX - TR.Rect.Right, rStepX + rXCount*(rIconWidth + rDI) - 3 ), 6);

  CL:= TPaletteLineItem.Create(FPaletteItems,  3, clGray, rStepX, TL.Rect.Bottom + 2*rStepY + rIconHeight - 6, 6, 6);
  C := TPaletteLineItem.Create(FPaletteItems, 10, clGray, CL.Rect.Right, CL.Rect.Top, T.Rect.Width, 6);
  CR:= TPaletteLineItem.Create(FPaletteItems, 12, clGray, C.Rect.Right, CL.Rect.Top, 6, 6);

  BL:= TPaletteLineItem.Create(FPaletteItems,  3, clSilver, rStepX, CL.Rect.Bottom + 2*rStepY + rIconHeight - 6, 6, 6);
  B := TPaletteLineItem.Create(FPaletteItems, 10, clSilver, BL.Rect.Right, BL.Rect.Top, T.Rect.Width, 6);
  BR:= TPaletteLineItem.Create(FPaletteItems,  5, clGray, B.Rect.Right, BL.Rect.Top, 6, 6);

  L1:= TPaletteLineItem.Create(FPaletteItems,  5, clGray, TL.Rect.Left, TL.Rect.Bottom, 6, CL.Rect.Top - TL.Rect.Bottom );
  L2:= TPaletteLineItem.Create(FPaletteItems,  5, clSilver, CL.Rect.Left, CL.Rect.Bottom, 6, CL.Rect.Top - TL.Rect.Bottom);

  R1:= TPaletteLineItem.Create(FPaletteItems,  0, clBlue{Silver}, TR.Rect.Left, TR.Rect.Bottom, 6, BL.Rect.Top - CL.Rect.Bottom + 1);
  R2:= TPaletteLineItem.Create(FPaletteItems,  5, clGray, CR.Rect.Left, CR.Rect.Bottom, 6, BL.Rect.Top - CL.Rect.Bottom + 1);
  R3:= TPaletteLineItem.Create(FPaletteItems,  5, clGray, BR.Rect.Left, BR.Rect.Bottom, 6,
       max( ExampleMaxY, 3*rStepY + rModeHeight + rYCount*(rIconHeight + rDI)) - (BR.Rect.Bottom) );

  DR:= TPaletteLineItem.Create(FPaletteItems,  3, clGray, R3.Rect.Left, R3.Rect.Bottom, 6, 6);
  DD:= TPaletteLineItem.Create(FPaletteItems, 10, clGray, DR.Rect.Right, R3.Rect.Bottom, TT.Rect.Width, 6);

  PaletteResize;
end;

{ TPaletteItems }

constructor TPaletteItems.Create(aParentControl: TDePalette);
begin
 inherited Create;
 FParentControl:= aParentControl;
end;

function TPaletteItems.GetItem(const Index: Integer): TPaletteItem;
begin
  Result:=  TPaletteItem(inherited Items[Index]);
end;

procedure TPaletteItems.PutItem(const Index: Integer; const Value: TPaletteItem);
begin
  items[Index]:= Value;
end;

{ TPaletteItem }

procedure TPaletteItem.SetValue(aValue: Integer);
begin
  if aValue=FValue then Exit;
  FValue:=aValue;
  Repaint(False);
end;

Procedure TPaletteItem.SetColor(aValue: TColor);
begin
  if aValue=FColor then Exit;
  FColor:=aValue;
  Repaint(False);
end;

procedure TPaletteItem.SetValueAndColor(aValue: Integer; aColor: TColor);
begin
  if (aValue=FValue) and (aColor=FColor) then Exit;

  FValue:=aValue;
  FColor:=aColor;
  Repaint(False);
end;

procedure TPaletteItem.SetHit(aValue: Boolean);
begin
  if aValue=FHit then Exit;
  FHit:= aValue;
  Repaint(True);
end;

procedure TPaletteItem.SetLTWH(aL, aT, aW, aH: Integer);
begin
  FRect.Left:= aL;
  FRect.Top:= aT;
  FRect.Width:= aW;
  FRect.Height:= aH;
end;

procedure TPaletteItem.SetLTRB(aL, aT, aR, aB: Integer);
begin
  FRect.Left:= aL;
  FRect.Top:= aT;
  FRect.Right:= aR;
  FRect.Bottom:= aB;
end;

procedure TPaletteItem.SetEnabled(aValue: Boolean);
begin
  if aValue= FEnabled then Exit;
  FEnabled:= aValue;
  Repaint(False);
end;

procedure TPaletteItem.SetPressed(aValue: Boolean);
begin

  if aValue=FPressed then Exit;
  FPressed:= aValue;
  Repaint(False);
end;

constructor TPaletteItem.Create(AOwner: TObjectList);
begin
  Create;
  FOwner:= aOwner;
  TPaletteItems(AOwner).add(self);

  FIndex    := -1;

  FColor    := clNone;;
  FCaption  := EmptyStr;
  FHint     := EmptyStr;

  FHit      := False;
  FPressed  := False;
  FEnabled  := True;
end;

procedure TPaletteItem.Repaint(withExtBorder: Boolean);
begin
  if assigned(onPaint) and assigned(FOwner) then
      if FOwner is TPaletteItems then
        if assigned(TPaletteItems(FOwner).FParentControl) then
          if TPaletteItems(FOwner).FParentControl is TDePalette then
             onPaint( TDePalette(TPaletteItems(FOwner).FParentControl), withExtBorder );
end;

{ TPaletteIconItem }

constructor TPaletteIconItem.Create(AOwner: TObjectList);
begin
  FIcon:=TIcon.Create;
  inherited;
  FonPaint:= OnPaintIcon;
  FEnabled := True;
end;

destructor TPaletteIconItem.Destroy;
begin
  FIcon.Free;
  inherited;
end;

procedure TPaletteIconItem.onPaintIcon(Sender: TDePalette; withExtBorder: Boolean);
begin
  if not assigned(Sender) or not Sender.HandleAllocated then Exit;

  Sender.PaintBoundsRect(Self, withExtBorder);
  Sender.Canvas.Draw(Rect.Left + (Rect.Width - FIcon.Width) div 2,
                     Rect.Top + (Rect.Height - FIcon.Height) div 2, FIcon);
end;

procedure TPaletteIconItem.PrepareIcon(aSize: Integer; aCode: Integer; aIcon: TIcon);
var I1: TIcon;
    Ind: Integer;
    Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse: Byte;
    aStackIcons: TStackIcons;
    aStackIcon: TStackIcon;
begin
  if Not Assigned(aIcon) then aIcon:= TIcon.Create;

  DecodeIcon(aCode ,Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse);

  if (Icon2 = 0) then
    begin
      Ind:= DM.MapIconIndex(Icon1);
      I1:= TIcon.Create;
      if 0<=Ind then
        DM.GetEImageList(aSize).GetIcon(Ind, I1);

      if (0=Color1) or (0=Icon1) then aIcon.Assign(I1)
                                 else UpdateColorIcon(I1, aIcon, SchemaColors[Color1]);
      I1.Free;
    end
  else
    begin
      aStackIcons:= DM.IconManager.CreateStackIcons(FValue);
      try
        aStackIcon := aStackIcons.Find(aSize, aSize);
        if Assigned(aStackIcon) then
          aIcon.Assign(aStackIcon.Icon);
      finally
        FreeAndNil(aStackIcons);
      end;
    end;
end;

procedure TPaletteIconItem.SetIconSize(const Value: Integer);
begin
  FIconSize := DM.GetIconSizeForSize( Value );
end;

procedure TPaletteIconItem.SetLTRB(aL, aT, aR, aB: Integer);
begin
  inherited;
  IconSize:= min(Rect.Width, Rect.Height);
end;

procedure TPaletteIconItem.SetLTWH(aL, aT, aW, aH: Integer);
begin
  inherited;
  IconSize:= min(Rect.Width, Rect.Height);
end;

procedure TPaletteIconItem.SetValue(aValue: Integer);
begin
  if aValue=FValue then Exit;

  FValue:=aValue;
  FIcon.Free;

  FIcon:= TIcon.Create;
  PrepareIcon(FIconSize, FValue, FIcon);

  Repaint(False);
end;

{ TPaletteZoomIconItem }

constructor TPaletteZoomIconItem.Create(AOwner: TObjectList);
begin
  inherited;
end;

destructor TPaletteZoomIconItem.Destroy;
begin
  if assigned(FIconZoom) then
    FIconZoom.Free;
  inherited;
end;


procedure TPaletteZoomIconItem.SetValue(aValue: Integer);
begin
  if not(aValue = FValue) then
    if assigned(FIconZoom) then FreeAndNil(FIconZoom);

  inherited SetValue(aValue);
end;

procedure TPaletteZoomIconItem.onPaintIcon(Sender: TDePalette; withExtBorder: Boolean);
var iSize: Integer;
begin
  if not assigned(Sender) or not Sender.HandleAllocated then Exit;

  if Hit then
    begin
      if not assigned(FIconZoom) then
        begin
          FIconZoom:= TIcon.Create;
          iSize:= DM.GetIconSizeForSize( min(Rect.Width, Rect.Height) + 2*rDI - 2 );
          PrepareIcon(iSize, FValue, FIconZoom);
        end;

      Sender.PaintBoundsRect(Self, withExtBorder);
      Sender.Canvas.Draw(Rect.Left + (Rect.Width - FIconZoom.Width) div 2,
                         Rect.Top + (Rect.Height - FIconZoom.Height) div 2, FIconZoom);
    end
  else
    inherited onPaintIcon(Sender, withExtBorder);
end;

{ TPaletteExampleItem }

constructor TPaletteExampleItem.Create(AOwner: TObjectList);
begin
  Inherited;
  FEnabled := False;
end;

{ TPaletteLineItem }

constructor TPaletteLineItem.Create(AOwner: TObjectList);
begin
  Inherited;
  FColor:= clGray;
  FEnabled := False;
  onPaint:= OnPaintLine;
end;

constructor TPaletteLineItem.Create(AOwner: TObjectList; aValue: Integer; aColor: TColor; aL, aT, aW, aH: Integer);
begin
  Create(AOwner);
  FColor:= aColor;
  FValue:= aValue;

  FRect.Left:= aL;
  FRect.Top:= aT;
  FRect.Width:= aW;
  FRect.Height:= aH;

  FEnabled := False;
  onPaint:= OnPaintLine;
end;

procedure TPaletteLineItem.OnPaintLine(Sender: TDePalette; withExtBorder: Boolean);
var X,Y: Integer;
begin
  if not assigned(Sender) or not Sender.HandleAllocated then Exit;

  Sender.Canvas.Brush.Style := bsSolid;
  Sender.Canvas.Brush.Color := Color;
  Sender.Canvas.Brush.Bitmap:= nil;
  Sender.Canvas.FillRect(Rect);

  Sender.Canvas.Pen.Color := Color;
  Sender.Canvas.Pen.Width:=1;

  X:= (Rect.Left + Rect.Right) div 2;
  Y:= (Rect.Top + Rect.Bottom) div 2;

  if 0 < (Value and $01) then Sender.Canvas.Rectangle(      X-1,   Rect.Top,        X+1,         Y+1);
  if 0 < (Value and $02) then Sender.Canvas.Rectangle(      X-1,        Y-1, Rect.Right,         Y+1);
  if 0 < (Value and $04) then Sender.Canvas.Rectangle(      X-1,        Y-1,        X+1, Rect.Bottom);
  if 0 < (Value and $08) then Sender.Canvas.Rectangle(Rect.Left,        Y-1,        X+1,         Y+1);
end;

{ TPaletteModeItem }

constructor TPaletteModeItem.Create(AOwner: TObjectList);
begin
  inherited;
  FonPaint:= OnPaintIcon;
end;

{ TPalettePageItem }

constructor TPalettePageItem.Create(AOwner: TObjectList);
begin
  inherited;
  FonPaint:= OnPaintIcon;
end;

{ TPaletteColorItem }

constructor TPaletteColorItem.Create(AOwner: TObjectList);
begin
  inherited;
  FonPaint:= OnPaintColor;
end;

procedure TPaletteColorItem.onPaintColor(Sender: TDePalette; withExtBorder: Boolean);
var R: TRect;
begin
  if not assigned(Sender) or not Sender.HandleAllocated then Exit;

  R:= Sender.PaintBoundsRect(Self, withExtBorder);
  R:= Rect;
  InflateRect(R, -3, -3);

  Sender.Canvas.Pen.Color:= clGray;
  Sender.Canvas.Brush.Style:= bsSolid;

  if Color = clNone then Sender.Canvas.Brush.Color := clWhite
                    else Sender.Canvas.Brush.Color := Color;

  if (0 = Length(Caption)) or (R.Width - R.Height < Sender.Canvas.TextWidth(Caption)) then
    begin
      Sender.Canvas.Rectangle(R);
    end
  else
    begin
      Sender.Canvas.Rectangle( R.Left, R.Top, R.Left+R.Height, R.Bottom );

      Sender.Canvas.Pen.Color := Sender.Color;
      Sender.Canvas.Brush.Style := bsSolid;
      Sender.Canvas.Brush.Color := Sender.Color;
      Sender.Canvas.Rectangle( R.Left+R.Height,R.Top,R.Right,R.Bottom );
      Sender.Canvas.TextOut(R.Left + (R.Width + R.Height - Sender.Canvas.TextWidth(Caption)) div 2,
                            R.Top  + (R.Height - Sender.Canvas.TextHeight(Caption)) div 2, Caption);
    end
end;

{ TPaletteTextItem }

constructor TPaletteTextItem.Create(AOwner: TObjectList);
begin
  inherited;
  FonPaint:= onPaintText;
end;

procedure TPaletteTextItem.onPaintText(Sender: TDePalette; withExtBorder: Boolean);
var R: TRect;
begin
  if not assigned(Sender) or not Sender.HandleAllocated then Exit;

  R:= Sender.PaintBoundsRect(Self, withExtBorder);
  InflateRect(R, -1, -1);

  Sender.Canvas.Pen.Color := Sender.Color;
  Sender.Canvas.Brush.Style := bsSolid;
  Sender.Canvas.Brush.Color := Sender.Color;
  Sender.Canvas.Rectangle( R.Left,R.Top,R.Right,R.Bottom );
  Sender.Canvas.TextOut(R.Left + (R.Width  - Sender.Canvas.TextWidth(Caption)) div 2,
                        R.Top  + (R.Height - Sender.Canvas.TextHeight(Caption)) div 2, Caption);
end;

initialization

end.
