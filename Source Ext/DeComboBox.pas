unit DeComboBox;

interface

uses System.UITypes, Controls, Windows, ActnList, Classes, Forms, StdCtrls, Graphics, Messages, UxTheme, Themes, Menus, Variants;

type
  TControlClass = class of TControl;

  TContextActionList = class(TActionList)
  private
    FPopupMenu: TPopupMenu;
  public
    constructor Create(AOwner: TComponent); override;
    function CreateAction(const aCaption: string; aEvent: TNotifyEvent; const aShort: String = '';
                          const aDefault: Boolean = False) : TAction;
    property PopupMenu: TPopupMenu read FPopupMenu;
  end;

  TDeEditBtn = class(TEdit)
  private
    FMouseDown: Boolean;
    FInButton, FLastInButton: Boolean;
    FShowButton: Boolean;
    FOnClick: TNotifyEvent;
    FEditing: Boolean;
    FDisplayFormat: string;
    procedure WMPaint(var Message: TWmPaint); message WM_PAINT;
    procedure WMSize(var Message: TWmPaint); message WM_SIZE;
    function ButtonWidth: Integer;
    function InButton(X,Y : Integer): Boolean;
    procedure SetDisplayFormat(const Value: string);
  protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure WMMButtonDblClk(var Message: TWMMouse); message WM_LBUTTONDBLCLK;

    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    property ShowButton: Boolean read FShowButton write FShowButton;
    property OnBtnClick: TNotifyEvent read FOnClick write FOnClick;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
  published
  end;

  TDeEditBtnPassword = class(TDeEditBtn)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TDeComboBox = class(TCustomEdit)
  private

    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
  protected
    FButtonWidth: Integer;
    FButtonRect: TRect;

    FListWidthFixed: Boolean;
    FListHeightFixed: Boolean;

    FButtonPressed: Boolean;
    FMouseOnButton: Boolean;
    FOnDropDown: TNotifyEvent;

    FEdit: TControl;
    FEditClass: TControlClass;
    FList: TControl;
    FListClass: TControlClass;

    FAutoDropDown: Boolean;
    FAutoShowButton: Boolean;
    FDroppedDown: Boolean;
    FActionList  : TContextActionList;
    FTrayBitmap: TBitmap;

    function GetClientRect: TRect; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DropDown; dynamic;
    procedure DeRealign;
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SetSubControlClasses; virtual; abstract;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CloseUp(Accept: Boolean); dynamic;
    procedure DoEnter; override;
    procedure DoExit; override;

    procedure SetDroppedDown(Value: Boolean);
    procedure SetButtonPressed(aValue: Boolean);
    procedure SetMouseOnButton(aValue: Boolean);

    property ButtonPressed: Boolean read FButtonPressed write setButtonPressed;
    property MouseOnButton: Boolean read FMouseOnButton write setMouseOnButton;
    property ButtonRect: TRect read FButtonRect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property DroppedDown: Boolean read FDroppedDown write SetDroppedDown;
    property AutoShowButton: Boolean read FAutoShowButton write FAutoShowButton;
  published
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property ActionList: TContextActionList read FActionList;
  end;

implementation

uses SysUtils, Dictionary, Funcs;

{ TContextActionList }

constructor TContextActionList.Create(AOwner: TComponent);
begin
  inherited;
  FPopupMenu := TPopupMenu.Create(AOwner);
end;

function TContextActionList.CreateAction(const aCaption: string; aEvent: TNotifyEvent; const aShort: String;
  const aDefault: Boolean): TAction;
  var aMenuItem: TMenuItem;
begin
  aMenuItem:=TMenuItem.Create(self);
  PopupMenu.Items.Add(aMenuItem);

  if aCaption= cLineCaption then
   aMenuItem.Caption:= cLineCaption
  else
    begin
      Result:= TAction.Create(self);
      Result.OnExecute := aEvent;
      Result.Caption := GetTitle(aCaption);
      Result.Hint    := GetTitle(aCaption, ttSecondName);

      if 0 < Length(aShort) then
        Result.ShortCut := TextToShortCut(aShort);
      aMenuItem.Action := Result;
      aMenuItem.Default := aDefault;
    end;
end;

{ TDeEditBtnPassword }

constructor TDeEditBtnPassword.Create(AOwner: TComponent);
begin
  inherited;
  PasswordChar := '*';
end;

{ TDeEditBtn }

constructor TDeEditBtn.Create(AOwner: TComponent);
begin
  inherited;
  FShowButton   := False;
  FMouseDown    := False;
  FInButton     := False;
  FLastInButton := False;
  FOnClick      := nil;
end;

function TDeEditBtn.ButtonWidth: Integer;
begin
  if (FShowButton) then
    Result:= GetSystemMetrics(SM_CXHTHUMB)
  else
    Result:= 0;
end;

procedure TDeEditBtn.WMPaint(var Message: TWmPaint);
var
  R : TRect;
  DC: HDC;
  bType,bState : Cardinal;
  Canvas: TControlCanvas;
  Value: string;
begin
  //TODO -2014 здесь надо подсмотреть отрисовку кнопки. очень похоже на контрол ввода чисел
  inherited;
  if (Length(DisplayFormat) <> 0) and not FEditing then
    begin
      Canvas := TControlCanvas.Create;
      try
        Canvas.Control := Self;
        R := ClientRect;
        if FShowButton then
          R.Right := R.Right - ButtonWidth;
        try
          Value := FormatString(DisplayFormat, Text);
        except
          Value := Text;
        end;
        Canvas.TextRect(R, R.Left, R.Top, Value);
      finally
        Canvas.Free;
      end;
    end;
  if Not FShowButton then Exit;

  DC := GetDC(Handle);
  R := ClientRect;
  R.Left := R.Right - ButtonWidth;

  bType:= DFC_CAPTION;  bState:=DFCS_CAPTIONMAX; // максимизаци€ окна

  if Not Enabled then  bState:=bState or DFCS_INACTIVE;
  if FMouseDown and FInButton then bState:= bState or DFCS_PUSHED or DFCS_FLAT;
  DrawFrameControl(DC, R, bType, bState);

  FLastInButton:=FInButton;
  ReleaseDC(Handle, DC);
end;

procedure TDeEditBtn.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style {or ES_MULTILINE or WS_CLIPCHILDREN };
     // в оригинале есть, но мешает при вставке из Excel и потребл€ет ресурсы
end;

procedure TDeEditBtn.CreateWnd;
var
  oRect: TRect;
begin
  inherited CreateWnd;
  SetRect(oRect, 0, 0, ClientWidth-ButtonWidth, ClientHeight);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@oRect));
end;

procedure TDeEditBtn.DoEnter;
begin
  inherited DoEnter;
  FEditing := True;
end;

procedure TDeEditBtn.DoExit;
begin
  inherited DoExit;
  FEditing := False;
  Invalidate;
end;

procedure TDeEditBtn.WMSize(var Message: TWmPaint);
var
  oRect: TRect;
begin
  inherited;
  SetRect(oRect, 0, 0, ClientWidth-ButtonWidth, ClientHeight);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@oRect));
end;

function TDeEditBtn.InButton(X,Y : Integer): Boolean;
begin
  Result:= ( ClientWidth-ButtonWidth <  X ) and ( X < ClientWidth ) and
                                 ( 0 <  Y ) and ( Y < ClientHeight );
end;

procedure TDeEditBtn.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  FInButton := InButton(X,Y);
  if FInButton then Cursor:=crArrow   // crHandPoint; //
               else Cursor:=crDefault;

  if FLastInButton<>FInButton then Invalidate;
end;

procedure TDeEditBtn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if InButton(X,Y) then
    begin
      FMouseDown:=True;
      Invalidate;
    end
  else
    inherited;
end;

procedure TDeEditBtn.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SavedFMouseDown : Boolean;
begin
  SavedFMouseDown:= FMouseDown;
  FMouseDown:=False;
  Invalidate;

  if InButton(X,Y) and (SavedFMouseDown) and Assigned(FOnClick) then
    FOnClick(self)
  else
    inherited;
end;

procedure TDeEditBtn.SetDisplayFormat(const Value: string);
begin
  if not SameStr(FDisplayFormat, Value) then
    begin
      FDisplayFormat := Value;
      Invalidate;
    end;
end;


procedure TDeEditBtn.WMMButtonDblClk(var Message: TWMMouse);
begin
  if Message.XPos<ClientWidth-ButtonWidth then
    inherited;
end;

{ TDeComboBox ---------------------------------------------------------------- }

function TDeComboBox.GetClientRect: TRect;
begin
  Result:= inherited GetClientRect;
  Result.Right:= Result.Right - FButtonWidth;
end;

Procedure TDeComboBox.DeRealign;
var R, SClient, SBound : TRect;
begin
  R:=inherited GetClientRect;
  SClient := Rect(ClientToScreen(R.TopLeft), ClientToScreen(R.BottomRight));
  SBound  := Rect(Parent.ClientToScreen(BoundsRect.TopLeft), Parent.ClientToScreen(BoundsRect.BottomRight));

  if AutoShowButton and (Width < 3 * GetSystemMetrics(SM_CXHTHUMB)) then
    begin
      FButtonWidth := 0;
      FButtonRect  := Rect(0, -1, 0, -1)
    end
  else
    begin
      FButtonWidth := GetSystemMetrics(SM_CXHTHUMB);
      FButtonRect  := Rect(Width- (SBound.Right-SClient.Right)- FButtonWidth, (SClient.Top-SBound.Top),
                           Width- (SBound.Right-SClient.Right),                Height- (SBound.Bottom-SClient.Bottom) );
    end;

  TWinControl(FEdit).SetBounds(0, 0, R.Right - FButtonWidth, R.Bottom);
end;

procedure TDeComboBox.CreateWnd;
begin
  inherited;
  DeRealign;
end;

procedure TDeComboBox.Resize;
begin
  inherited;
  DeRealign;
end;

procedure TDeComboBox.CloseUp(Accept: Boolean);
begin
  TDeComboBox(FList).MouseCapture := False;
  FList.Visible := False;
end;

constructor TDeComboBox.Create(AOwner: TComponent);
begin
  inherited;
  AutoSize := False; // »наче контрол подгон€етс€ под текущий шрифт!!!
  BorderStyle := bsSingle; //об€зательно, иначе не неклиентские сообщени€ не обрабатываютс€

  TabStop := True;
  Constraints.MinWidth := 8;
  FAutoShowButton := True;
  BevelKind := bkTile;
  Cursor:= crArrow;  // мен€ет текстовый курсор на стандартный
  ReadOnly :=True;   // отключает редактирование текста

  FAutoDropDown := True;
  FMouseOnButton := False;
  FButtonPressed :=False;
  FButtonWidth := 0;

  FListWidthFixed := True;
  FListHeightFixed := True;
  SetSubControlClasses;

  FEdit := FEditClass.Create(Self);
  FEdit.Parent := Self;
  if not (FEdit is TCustomEdit) then FEdit.Enabled := False;

  FList := FListClass.Create(Self);
  FList.Parent := Self;
  FList.Visible := False;

  FActionList := TContextActionList.Create(self);
  PopupMenu := FActionList.PopupMenu;

  FTrayBitmap := TBitmap.Create;
  FTrayBitmap.Height := 4;
  FTrayBitmap.Width := 4;
  with FTrayBitmap.Canvas do
  begin
    Brush.Color := clGray;
    FillRect(Rect(0,0,4,4));
    Brush.Color := clWhite;
    FillRect(Rect(0,0,2,2));
    FillRect(Rect(2,2,4,4));
  end;
end;

destructor TDeComboBox.Destroy;
begin
  FTrayBitmap.Free;
  inherited;
end;

procedure TDeComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TDeComboBox.DoEnter;
begin
  inherited;
  FEdit.repaint;
  Change;
end;

procedure TDeComboBox.DoExit;
begin
  inherited;
  CloseUp(False);;
  FEdit.repaint;
end;

function LiteResize(x1,x2,Step,i,SleepValue:Integer):Integer;
begin
  if i=Step then
    LiteResize:=x2
  else
    begin
      sleep(SleepValue);
      LiteResize:=x1+Round((x2-x1)*0.5*(1-Cos(i*pi/Step)));
    end;
end;   

procedure TDeComboBox.DropDown;
var
  TopLeft : TPoint;
  H,X,Y,i : Integer;
  aListWidth, aListHeight : Integer;
begin
  inherited;
  if Assigned(FOnDropDown) then FOnDropDown(Self);

  if Not assigned(FList) then Exit;

  aListWidth:= FList.Width;
  aListHeight:=FList.Height;

  TopLeft := Parent.ClientToScreen(BoundsRect.TopLeft);

  if TopLeft.X + aListWidth  >= Screen.WorkAreaWidth  then
    X:=TopLeft.X + Width - aListWidth
  else
    X:=TopLeft.X;

  if TopLeft.Y + Height + aListHeight >= Screen.WorkAreaHeight then
    begin
      Y:=TopLeft.Y - aListHeight;
      for i:=1 to 10 do
        begin
          H:=LiteResize(0, aListHeight, 10, i, 10);
          FList.SetBounds(X,  Y+aListHeight-H, aListWidth, H);
          FList.Visible := True;
          FList.Repaint;
        end;
    end
  else
    begin
      Y:=TopLeft.Y + Height;
      for i:=1 to 10 do // 0 не нужен, незачем рисовать то чего нет
        begin
          FList.SetBounds(X, Y, aListWidth, LiteResize(0, aListHeight, 10, i, 10));
          FList.Visible := True;
          FList.Repaint;
        end;
    end;

  FList.Update;
end;

procedure TDeComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (ssAlt in Shift) and ((Key = VK_DOWN) or (Key = VK_UP)) then
  begin
    DroppedDown := not DroppedDown;
  end;
end;

procedure TDeComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;

  SetFocus;

  if (FAutoDropDown) or PtInRect(ButtonRect, Point(X, Y)) then
    begin
      MouseOnButton := True;
      ButtonPressed := True;
      DropDown;
    end;
end;

procedure TDeComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ButtonPressed:= False;
end;

procedure TDeComboBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not ButtonPressed then
    MouseOnButton := PtInRect(ButtonRect, Point(X, Y));
end;

procedure TDeComboBox.WMPaint(var Message: TWMPaint);
var
  DC : HDC;
  T: TThemedElementDetails;
begin
  inherited;

  DC := GetWindowDC(Handle);
  try
    if StyleServices.Enabled then
      with StyleServices do
        begin
          T:=GetElementDetails(tcBorderNormal);
          if Not Enabled then    DrawElement(DC, GetElementDetails(tcDropDownButtonDisabled), ButtonRect) else
          if FMouseOnButton and FButtonPressed then
                                 DrawElement(DC, GetElementDetails(tcDropDownButtonPressed), ButtonRect) else
          if FMouseOnButton then DrawElement(DC, GetElementDetails(tcDropDownButtonHot), ButtonRect) else
                                 DrawElement(DC, GetElementDetails(tcDropDownButtonNormal), ButtonRect);
        end
    else
      begin
        if FMouseOnButton then   DrawFrameControl(DC, ButtonRect, DFC_SCROLL, DFCS_PUSHED or DFCS_SCROLLCOMBOBOX or DFCS_FLAT)
                          else   DrawFrameControl(DC, ButtonRect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX);
      end;
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TDeComboBox.SetDroppedDown(Value: Boolean);
begin
  if not FDroppedDown and Value then
    begin
      DropDown;
      TDeComboBox(FList).MouseCapture := True;
    end
  else
    begin
      CloseUp(False);
    end;
  FDroppedDown := Value;
end;

procedure TDeComboBox.SetMouseOnButton(aValue: Boolean);
var BR: TRect;
begin
  if not (aValue = FMouseOnButton) then
    begin
      FMouseOnButton:=aValue;
      BR:= ButtonRect;
      InvalidateRect(Handle, @BR, True);
    end;
end;

procedure TDeComboBox.SetButtonPressed(aValue: Boolean);
var BR: TRect;
begin
  if not (aValue = FMouseOnButton) then
    begin
      FButtonPressed:=aValue;
      BR:= ButtonRect;
      InvalidateRect(Handle, @BR, True);
    end;
end;

procedure TDeComboBox.WMCaptureChanged(var Message: TMessage);
var
  BR: TRect;
begin
  inherited;
  FButtonPressed := False;
  if FMouseOnButton then
  begin
    BR := ButtonRect;
    FMouseOnButton := False;
    InvalidateRect(Handle, @BR, True);
  end;
  TDeComboBox(FList).MouseCapture := True;
end;

procedure TDeComboBox.WMKillFocus(var Message: TMessage);
begin
  DroppedDown:=False;
end;

end.
