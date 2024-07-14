
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{  Copyright (c) 1995-2001 Borland Software Corporation }
{                                                       }
{*******************************************************}

unit ExtCheckLst;

{$T-,H+,X+}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Math, ExtCtrls,
  StdCtrls, CommCtrl, CheckLst, Imglist;

type
  TExtCheckListBox = class(TCheckListBox)
  private
    FDragImage : TDragImageList;
    FDragIndex : Integer;
    FDragRect: TRect;
    FAllowOrderFirst : boolean;
  protected
    procedure CreateDragImage;
    procedure DoStartDrag( var DragObject: TDragObject ); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AllowOrderFirst : boolean read FAllowOrderFirst write FAllowOrderFirst;
    function GetDragImages: TDragImageList; override;
    procedure DragDrop(Source: TObject; X, Y: Integer);override;
  end;

procedure Register;

implementation

uses Consts, RTLConsts;

procedure Register;
begin
  RegisterComponents('Profit', [TExtCheckListBox]);
end;

{ TExtCheckListBox }

constructor TExtCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:= ControlStyle+[csDisplayDragImage];
  FDragImage:= TImageList.CreateSize( 32, 32 );
  FDragIndex:=-1;
  FDragRect.Empty;
  BorderWidth:= 4
end;

destructor TExtCheckListBox.Destroy;
begin
  FDragImage.Free;
  inherited;
end;

function TExtCheckListBox.GetDragImages: TDragImageList;
begin
//  if FDragImage.Count > 0 then Result := FDragImage else Result := nil;
  Result:=nil;
end;

procedure TExtCheckListBox.CreateDragImage;
var
  HotSpotX, HotSpotY: integer;
  TranspColor: TColor;
  Bmp: TBitmap;
  pt: TPoint;
  R: TRect;
begin
  FDragImage.Clear;
  if ItemIndex = -1 then exit;
  R := ItemRect( ItemIndex );
  Bmp := TBitmap.Create;
  with Bmp do
  try
    GetCursorPos(pt);
    with ScreenToClient( pt ) do
    begin HotSpotX := X - R.Left; HotSpotY := Y - R.Top end;
//    if Assigned(OnGetDragImage) then
//      OnGetDragImage( self, Bmp, TranspColor, HotSpotX, HotSpotY )
//    else
    begin
      Width := R.Right - R.Left; Height := R.Bottom - R.Top;
      Canvas.Font:=Font;
      Canvas.DrawFocusRect( Rect( 0, 0, Width, Height ) );
      Canvas.Brush.Style := bsClear;
      Canvas.TextOut( 1, 1, Items[ItemIndex] );
      TranspColor := clWhite;
    end;
    FDragImage.Width := Width; FDragImage.Height := Height;
    FDragImage.AddMasked( Bmp, TranspColor );
    FDragImage.SetDragImage( 0, HotSpotX, HotSpotY );
  finally
    Bmp.Free;
  end;
end;

procedure TExtCheckListBox.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  CurrentIndex, F :integer;
begin
  inherited;
  if DragMode=dmAutomatic then
  begin
    Accept:=Self=Source;
    if Accept then
    begin
      // скроллинг
      if Y>Height-ItemHeight/2 then
        TopIndex:=TopIndex+1
      else if Y<ItemHeight/2 then
      begin
        FDragImage.HideDragImage;
        TopIndex:=TopIndex-1;
      end;
      CurrentIndex:=ItemAtPos(Point(x,y),False);

      // стираем старое
      Canvas.Pen.Mode:= pmNotCopy;
      if Not FDragRect.IsEmpty then
        begin
          Canvas.Rectangle(FDragRect);
          FDragRect.Empty;
        end;

      // Рисуем новое
      Canvas.Pen.Mode:= pmCopy;
      if (CurrentIndex<>-1) then
      begin
        if CurrentIndex=Items.Count then
          FDragIndex:= CurrentIndex
        else if Y > ItemRect(CurrentIndex).CenterPoint.Y  then
          FDragIndex:= CurrentIndex+1
        else
          FDragIndex:= CurrentIndex;

        if (State<>dsDragLeave)
           and (AllowOrderFirst or (CurrentIndex <> 0))
           and ((FDragIndex<ItemIndex) or (FDragIndex>ItemIndex+1)) then
        begin
          if FDragIndex = 0 then F:=0 else F:=-1;
          FDragRect:= Rect(ItemRect(FDragIndex).Left + 34, ItemRect(FDragIndex).Top + F,
                           ItemRect(FDragIndex).Right - 2, ItemRect(FDragIndex).Top + F + 2);
          Canvas.Rectangle(FDragRect);
        end;
      end;
    end;
  end;
end;

procedure TExtCheckListBox.DoStartDrag( var DragObject: TDragObject );
var
  P     : TPoint;
  Rect  : PRect;
  Index : integer;
begin
  inherited;
  if DragMode=dmAutomatic then
  begin
    GetCursorPos(P);
    P:=ScreenToClient(P);
    Index := ItemAtPos(P,True);
    if (Index <> -1) and (AllowOrderFirst or (Index > 0)) then
    begin
      CreateDragImage;
      New(Rect);
      Rect^:=ItemRect(ItemIndex);
      InvalidateRect(Handle, Rect, True);
      Dispose(Rect);
    end
    else
      CancelDrag;
  end;
end;

procedure TExtCheckListBox.DragDrop(Source: TObject; X, Y: Integer);
begin
  inherited;
  if (DragMode=dmAutomatic)
     and (AllowOrderFirst or (FDragIndex <> 0))
     and ((FDragIndex<ItemIndex) or (FDragIndex>ItemIndex+1))
    then begin
      if FDragIndex>ItemIndex then
        FDragIndex:=FDragIndex-1;
      Items.Move(ItemIndex,FDragIndex);
      ItemIndex:=FDragIndex;
    end;
end;

end.
