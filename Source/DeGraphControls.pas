unit DeGraphControls;

interface

uses Graphics, SysUtils, ExtCtrls, ComCtrls, Variants, Forms, Classes,
     Controls, StdCtrls, CheckLst, Menus, Windows, Dialogs, BaseGridFormUnit,
     DeMeta, DSMeta, Codescan, ShellAPI, DeDB, ListFormUnit, DataCacheUnit, Math,
     Messages, DeFileDropper, Types, DB, DeSchema2D, ColorComboBox, Funcs, DeTypes;
type
  { закладка для редактирования свойств графического объекта }
  TDeMapObjectPanel = class(TPanel)
  private
    FForm    : TForm;
    anObject : TdeSchemaObject;
    lbl_C  : TLabel;
    lbl_X  : TLabel;
    lbl_Y  : TLabel;
    lbl_W  : TLabel;
    lbl_H  : TLabel;
    lbl_R  : TLabel;
    lbl_BC : TLabel;
    lbl_FC : TLabel;
    lbl_L  : TLabel;
    lbl_T  : TLabel;
    ed_C   : TEdit;
    ed_X   : TEdit;
    ed_Y   : TEdit;
    ed_W   : TEdit;
    ed_H   : TEdit;
    ed_R   : TEdit;
    ed_L   : TLineWidthComboBox;
    ed_T   : TLineTypeComboBox;
    ed_BC  : TColorComboBox;
    ed_FC  : TColorComboBox;
    fInit    : Boolean;
    fSchema  : TdeSchema;
//    fItem    : TdeSchemaObject;
    fL,fT    : Integer;
    fX,fY,fH,fW,fR : Single;
    fBC,fFC  : TColor;
    fTC      : Boolean;
    procedure SetValue(aValue: Variant);
    function GetValue: Variant;
  protected
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure onChange(Sender: TObject);
  published
  public
    constructor Create(aOwner : TComponent);  override;
    destructor Destroy;  override;
    procedure SetData(aForm : TForm);
    property Value : Variant read GetValue write SetValue;
  end;

  { закладка для редактирования точек графического объекта }
  TDeMapPointObjectPanel = class(TPanel)
  private
    FForm    : TForm;
    anObject : TdeSimpleCurve;
 //   fVertex  : Integer;
    fInit    : Boolean;

    lbl_N   : TLabel;
    lbl_XY  : TLabel;
    lbl_0   : TLabel;
    lbl_1   : TLabel;
    lbl_2   : TLabel;
    lb_N    : TListBox;
    ed_X    : TEdit;
    ed_Y    : TEdit;
    ed_X1   : TEdit;
    ed_Y1   : TEdit;
    ed_X2   : TEdit;
    ed_Y2   : TEdit;
    procedure SetValue(aValue: Variant);
    function GetValue: Variant;
  protected
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure ClearControlValues(Sender: TObject);
    procedure LoadControlValuesA(Sender: TObject);
    procedure LoadControlValuesB(Sender: TObject);
    procedure LoadControlValuesC(Sender: TObject);
    procedure LoadControlValues(Sender: TObject);
    procedure onChange(Sender: TObject);
    procedure onComboChange(Sender: TObject);
    procedure UpdateView;
  published
  public
    constructor Create(aOwner : TComponent);  override;
    destructor Destroy;  override;
    procedure SetData(aForm : TForm);
    property Value : Variant read GetValue write SetValue;
  end;


implementation

uses Dictionary, VectorFormUnit, G2D, DataUnit{$IFDEF DEBUG}, DeLog{$ENDIF};

//------------------------------------------------------------------------------

procedure CreateLabel(var aLabel: TLabel; aOwner:TWinControl; aLeft,aTop: Integer; aCaption:String);
begin
  aLabel:=TLabel.Create(aOwner);
  aLabel.Parent := aOwner;
  aLabel.Left   := aLeft;
  aLabel.Top    := aTop;
  aLabel.Caption:= GetTitle(aCaption);
end;

procedure CreateEdit(var aEdit: TEdit; aOwner:TWinControl; aLeft,aTop: Integer;
                     aOnChange: TNotifyEvent);
begin
  aEdit:=TEdit.Create(aOwner);
  aEdit.Parent  := aOwner;
  aEdit.Left    := aLeft;
  aEdit.Top     := aTop;
  aEdit.OnExit {.OnChange}:= aOnChange;
end;

procedure CreateComboBox(var aCombo: TComboBox; aOwner:TWinControl; aLeft,aTop: Integer;
                     aOnChange: TNotifyEvent);
begin
  aCombo:=TComboBox.Create(aOwner);
  aCombo.Parent  := aOwner;
  aCombo.Left    := aLeft;
  aCombo.Top     := aTop;
  aCombo.Style   := csDropDownList;
  aCombo.OnChange:= aOnChange;
end;

procedure CreateListBox(var aCombo: TListBox; aOwner:TWinControl; aLeft,aTop: Integer;
                     aOnChange: TNotifyEvent);
begin
  aCombo:=TListBox.Create(aOwner);
  aCombo.Parent  := aOwner;
  aCombo.Left    := aLeft;
  aCombo.Top     := aTop;
  aCombo.OnClick := aOnChange;
end;

procedure CreateCEdit(var aEdit: TColorComboBox; aOwner:TWinControl;
                      aLeft,aTop: Integer; aOnChange: TNotifyEvent;
                      aCanTransparent: Boolean);
begin
  aEdit:=TColorComboBox.Create(aOwner);
  aEdit.Parent  := aOwner;
  aEdit.Left    := aLeft;
  aEdit.Top     := aTop;
  aEdit.Height  := 21;
  aEdit.CanTransparent := aCanTransparent;
  aEdit.DefaultCaption := GetTitle('_df.default');
  aEdit.AddColorCaption := GetTitle('_dl.morecolors');
  aEdit.OnChange:= aOnChange;
end;
          
procedure CreateLEdit(var aEdit: TLineWidthComboBox; aOwner:TWinControl;
                      aLeft,aTop: Integer; aOnChange: TNotifyEvent);
begin
  aEdit:=TLineWidthComboBox.Create(aOwner);
  aEdit.Parent  := aOwner;
  aEdit.Left    := aLeft;
  aEdit.Top     := aTop;
  aEdit.Height  := 21;
  aEdit.OnChange:= aOnChange;
end;

procedure CreateTEdit(var aEdit: TLineTypeComboBox; aOwner:TWinControl;
                      aLeft,aTop: Integer; aOnChange: TNotifyEvent);
begin
  aEdit:=TLineTypeComboBox.Create(aOwner);
  aEdit.Parent  := aOwner;
  aEdit.Left    := aLeft;
  aEdit.Top     := aTop;
  aEdit.Height  := 21;
  aEdit.OnChange:= aOnChange;
end;

//------------------------------------------------------------------------------

procedure TDeMapObjectPanel.onChange(Sender: TObject);
begin
  if fInit then Exit;

  anObject.Lock;

  if (Sender=ed_X)or(Sender=ed_Y) then
    if TEdit(Sender).Modified then
      anObject.Offset := Point2D(StrToFloatDef(ed_X.Text ,fX),
                                 StrToFloatDef(ed_Y.Text ,fY));
  if (Sender=ed_W) then
    if TEdit(Sender).Modified then
      anObject.Width  := StrToFloatDef(ed_W.Text, anObject.Width);
  if (Sender=ed_H) then
    if TEdit(Sender).Modified then
      anObject.Height := StrToFloatDef(ed_H.Text, anObject.Height);
  if (Sender=ed_R) then
    if TEdit(Sender).Modified then
      anObject.Rotation := StrToFloatDef(ed_R.Text,0)*Pi/180;

  if Not(anObject is TdeSchemaGroup) then
    begin {
      if (Sender=ed_C) then
        TdeCustomCurve(anObject).Caption     := ed_C.Text;
        {}
      if (Sender=ed_BC) then
        TdeCustomCurve(anObject).BorderColor := ed_BC.Color;
      if (Sender=ed_FC) then
        TdeCustomCurve(anObject).Color       := ed_FC.Color;
      if (Sender=ed_FC) then
        TdeCustomCurve(anObject).Transparent :=(ed_FC.Color = clNone);
      if (Sender=ed_L) then
        TdeCustomCurve(anObject).BorderWidth := ed_L.Value;
      if (Sender=ed_T) then
        TdeCustomCurve(anObject).BorderStyle := TPenStyle(ed_T.Value);
    end;
  anObject.unLock;
end;

procedure TDeMapObjectPanel.WMSize(var Message: TMessage);
var x: Integer;
begin
  x := ((Width-(16+225)) div 3);

  ed_X.Width := X;   ed_X.Left  := 75;
  ed_W.Width := X;   ed_W.Left  := 75;
  ed_R.Width := X;   ed_R.Left  := 75;

  lbl_Y.Left := X+75+8;
  lbl_H.Left := X+75+8;
  lbl_FC.Left:= X+75+8;

  ed_Y.Width := X;   ed_Y.Left  := X+150+8;
  ed_H.Width := X;   ed_H.Left  := X+150+8;
  ed_FC.Width:= X;   ed_FC.Left := X+150+8;

  lbl_L.Left := 2*X+150+16;
  lbl_T.Left := 2*X+150+16;
  lbl_BC.Left:= 2*X+150+16;

  ed_L.Width := X;   ed_L.Left  := 2*X+225+16;
  ed_T.Width := X;   ed_T.Left  := 2*X+225+16;
  ed_BC.Width:= X;   ed_BC.Left := 2*X+225+16;

  ed_C.Width := 3*X+150+16;   ed_C.Left  := 75;
end;

constructor TDeMapObjectPanel.Create(aOwner : TComponent);
begin
  fInit:=True;
  inherited;
  Caption:='';
  BevelOuter:=bvNone;
  fSchema:= TdeSchema.Create;

  CreateLabel(lbl_X,  self,   0,  6, 'X');
  CreateLabel(lbl_Y,  self, 200,  6, 'Y');

  CreateLabel(lbl_W,  self,   0, 28, '_Df.Width');
  CreateLabel(lbl_H,  self, 200, 28, '_Df.Height');

  CreateLabel(lbl_R,  self,   0, 56, '_Df.Rotate');
  CreateLabel(lbl_FC, self, 200, 56, '_Df.FillColor');

  CreateLabel(lbl_L,  self, 400,  6, '_Dl.Thickness');
  CreateLabel(lbl_T,  self, 400, 28, '_Df.Type');
  CreateLabel(lbl_BC, self, 400, 56, '_Df.BorderColor');

  CreateLabel(lbl_C, self, 0, 78, '_Df.Note');


  CreateEdit (ed_X, self, 80,  0, self.onChange);
  CreateEdit (ed_Y, self, 80,  0, self.onChange);
  CreateLEdit(ed_L,  self, 80,  0, self.onChange);

  CreateEdit(ed_W, self, 80, 22, self.onChange);
  CreateEdit(ed_H, self, 80, 22, self.onChange);
  CreateTEdit(ed_T,self, 80, 22, self.onChange);
  CreateEdit(ed_R, self, 80, 50, self.onChange);

  CreateCEdit( ed_FC, self, 80, 50, self.onChange, True);
  CreateCEdit( ed_BC, self, 80, 50, self.onChange, False);

  CreateEdit (ed_C,  self, 80,  72, self.onChange);
  fInit:=False;
end;

destructor TDeMapObjectPanel.Destroy;
begin
  fSchema.Free;
  inherited;
  //
end;

procedure TDeMapObjectPanel.SetData(aForm : TForm);
begin
  if aForm is TBaseGridForm then
    FForm:=aForm;
end;

procedure TDeMapObjectPanel.SetValue(aValue: Variant);
var i: Integer;
    s: String;
//    L:Integer;
//    TCI : TCacheItem;
//    S,Buffer,objData : string;
  procedure InitEditValue(aEdit: TEdit; aValue: string);
  begin
    if (aEdit.Text='') or (StrToFloatDef(aEdit.Text,0)<>StrToFloatDef(aValue,0)) then
      aEdit.Text:=aValue;
  end;
begin
  anObject:=nil;

  {
  TCI:=TBaseGridForm(FForm).DataSetMeta.Cache.FocusedItem;
  if Assigned(TCI) then
    if (TCI.ValueByName['OBJCLASS'] = 0) or
       (TCI.ValueByName['OBJCLASS'] = 1) then
    begin
  fItem:=fSchema.New(TCI.ValueByName['OBJCLASS'],nil,'');

  s:=TCI.ValueByName['OBJDATA'];
  Delete(s,1,1);
  l:=0;
  Buffer := ' ';
  if not Base64Decode(s,@(Buffer[1]),l)
  then begin
    setLength(Buffer,l+1);
    Base64Decode(s,@(Buffer[1]),l);
  end;
  objData := copy(Buffer,1,l);
  fItem.Lock;
  fItem.setData(@(objData[1]),length(objData));
  fItem.UnLock;
  anObject:=fItem;
    end;
    {}
  Caption:='';
//  if Not assigned(anObject) then
  if assigned(FForm) then
    if FForm is TVectorViewForm then
    if TVectorViewForm(FForm).SchemaView.SelectedCount>0 then
      anObject:= TVectorViewForm(FForm).SchemaView.Selected[0];

  for i:=0 to ControlCount-1 do
    Controls[i].Enabled:=(anObject<>nil);

  fInit:=True;
  if anObject=nil then
    begin
      ed_X.Text:='';
      ed_Y.Text:='';
      ed_W.Text:='';
      ed_H.Text:='';
      ed_R.Text:='';
      ed_L.Value:=1;
      ed_T.Value:=0;
      ed_BC.Color := clWhite;
      ed_FC.Color := clWhite;
      ed_C.Text:='';
    end
  else
    begin
      fX := anObject.Offset.X;
      fY := anObject.Offset.Y;
      fH := anObject.Height;
      fW := anObject.Width;
      fR := anObject.Rotation*180/Pi;

      if anObject is TdeSchemaGroup then
        begin
          ed_C.Text   := '';
          ed_BC.Color := clWhite;
          ed_FC.Color := clWhite;
          ed_BC.Enabled:=False;
          ed_FC.Enabled:=False;
          ed_L.Enabled:=False;
          ed_T.Enabled:=False;
        end
      else
        begin
//          ed_C.Text := TdeCustomCurve(anObject).Caption;

          fBC := TdeCustomCurve(anObject).BorderColor;
          fFC := TdeCustomCurve(anObject).Color;
          fTC := TdeCustomCurve(anObject).Transparent;

          fL  := TdeCustomCurve(anObject).BorderWidth;
          ed_L.Value:=fL;

          fT  := Integer(TdeCustomCurve(anObject).BorderStyle);
          ed_T.Value:=(fT);

          ed_BC.Color := fBC;
          if fTC then
            ed_FC.Color := clNone
          else
            ed_FC.Color := fFC;
        end;

      InitEditValue(ed_X, Format('%1.1f',[fX]));
      InitEditValue(ed_Y, Format('%1.1f',[fY]));
      InitEditValue(ed_H, Format('%1.1f',[fH]));
      InitEditValue(ed_W, Format('%1.1f',[fW]));

      s:=Format('%1.0f',[fR]);
      if s='360' then s:='0';

      if (ed_R.Text='') or
         (abs(StrToIntDef(ed_R.Text,0)-StrToIntDef(s,0)) mod 360 >0) then
        ed_R.Text:=s;
    end;

  FInit:=False;
end;

function TDeMapObjectPanel.GetValue: Variant;
begin
  result:=null;
end;

//------------------------------------------------------------------------------

procedure TDeMapPointObjectPanel.onChange(Sender: TObject);
var PointA,PointB, R : TPoint2D;
    A,B : Integer;
begin
  if fInit then Exit;
  if anObject=nil then Exit;

  if (Sender = ed_X) or (Sender = ed_Y) then
    begin
      A:=  LB_N.ItemIndex;
      PointA:=anObject.OToS(anObject.Vertex[A]);

      R   := anObject.Vertex[A];
      R.X := StrToFloatDef(ed_X.Text, Round(PointA.X));
      R.Y := StrToFloatDef(ed_Y.Text, Round(PointA.Y));

      anObject.Vertex[A] := anObject.SToO(R);

      UpdateView;
      LoadControlValuesB(Sender);
      LoadControlValuesC(Sender);
    end;

  if (Sender = ed_X1) or (Sender = ed_Y1) then
    begin
      A:=  LB_N.ItemIndex;
      B:= (A+anObject.VertexesCount-1) mod anObject.VertexesCount;
      PointA:=anObject.OToS(anObject.Vertex[A]);
      PointB:=anObject.OToS(anObject.Vertex[B]);

      R   := anObject.Vertex[A];
      R.X := PointB.X + StrToFloatDef(ed_X1.Text, Round(PointA.X-PointB.X));
      R.Y := PointB.Y + StrToFloatDef(ed_Y1.Text, Round(PointA.Y-PointB.Y));

      anObject.Vertex[A] := anObject.SToO(R);

      UpdateView;
      LoadControlValuesA(Sender);
      LoadControlValuesC(Sender);
    end;

  if (Sender = ed_X2) or (Sender = ed_Y2) then
    begin
      A:=  LB_N.ItemIndex;
      B:= (A+anObject.VertexesCount+1) mod anObject.VertexesCount;
      PointA:=anObject.OToS(anObject.Vertex[A]);
      PointB:=anObject.OToS(anObject.Vertex[B]);

      R   := anObject.Vertex[A];
      R.X := PointB.X + StrToFloatDef(ed_X2.Text, Round(PointA.X-PointB.X));
      R.Y := PointB.Y + StrToFloatDef(ed_Y2.Text, Round(PointA.Y-PointB.Y));

      anObject.Vertex[A] := anObject.SToO(R);

      UpdateView;
      LoadControlValuesA(Sender);
      LoadControlValuesB(Sender);
    end;
end;

procedure TDeMapPointObjectPanel.ClearControlValues(Sender: TObject);
begin
  FInit:=True;
  ed_X.Text:='';
  ed_Y.Text:='';

  Lbl_1.Caption:='';
  ed_X1.Text:='';
  ed_Y1.Text:='';

  Lbl_2.Caption:='';
  ed_X2.Text:='';
  ed_Y2.Text:='';
  FInit:=True;
end;

procedure TDeMapPointObjectPanel.LoadControlValuesA(Sender: TObject);
var PointA : TPoint2D;
    A      : Integer;
begin
  FInit:=True;

  A:=  LB_N.ItemIndex;
  anObject.ActiveVertex:=A;
  PointA:=anObject.OToS(anObject.Vertex[A]);
  ed_X.Text:=Format('%1.1f',[PointA.X]);
  ed_Y.Text:=Format('%1.1f',[PointA.Y]);

  FInit:=False;
end;

procedure TDeMapPointObjectPanel.LoadControlValuesB(Sender: TObject);
var PointA,PointB : TPoint2D;
    A,B : Integer;
begin
  FInit:=True;

  A:=  LB_N.ItemIndex;
  anObject.ActiveVertex:=A;
  PointA:=anObject.OToS(anObject.Vertex[A]);
  B:= (A+anObject.VertexesCount-1) mod anObject.VertexesCount;
  PointB:=anObject.OToS(anObject.Vertex[B]);

  ed_X1.Text:=Format('%1.1f',[PointA.X-PointB.X]);
  ed_Y1.Text:=Format('%1.1f',[PointA.Y-PointB.Y]);

  Lbl_1.Caption:=IntToName(B);
  FInit:=False;
end;

procedure TDeMapPointObjectPanel.LoadControlValuesC(Sender: TObject);
var PointA,PointB : TPoint2D;
    A,B : Integer;
begin
  FInit:=True;

  A:=  LB_N.ItemIndex;
  anObject.ActiveVertex:=A;
  PointA:=anObject.OToS(anObject.Vertex[A]);

  B:= (A+anObject.VertexesCount+1) mod anObject.VertexesCount;
  PointB:=anObject.OToS(anObject.Vertex[B]);

  ed_X2.Text:=Format('%1.1f',[PointA.X-PointB.X]);
  ed_Y2.Text:=Format('%1.1f',[PointA.Y-PointB.Y]);

  Lbl_2.Caption:=IntToName(B);
  FInit:=False;
end;

procedure TDeMapPointObjectPanel.LoadControlValues(Sender: TObject);
begin
  FInit:=True;
  LoadControlValuesA(Sender);
  LoadControlValuesB(Sender);
  LoadControlValuesC(Sender);
  FInit:=False;
end;

procedure TDeMapPointObjectPanel.UpdateView;
begin
  {$IFDEF VECTOR_MAP}
  if FForm is TVectorViewForm then
    TVectorViewForm(FForm).SchemaView.RefreshView(True);
  {$ENDIF}
end;

procedure TDeMapPointObjectPanel.onComboChange(Sender: TObject);
begin
  if fInit then Exit;
  LoadControlValues(Sender);
  UpdateView;
end;

procedure TDeMapPointObjectPanel.WMSize(var Message: TMessage);
var x : Integer;
begin
  x := ((Width-(16+3*75)) div 3);

  lb_N.Width  := x;    lb_N.Left  := 75;

  lbl_0.Left  := x+75+8;
  lbl_1.Left  := x+75+8;
  lbl_2.Left  := x+75+8;

  ed_X.Width  := x;   ed_X.Left  := x+225+8;
  ed_X1.Width := x;   ed_X1.Left := x+225+8;
  ed_X2.Width := x;   ed_X2.Left := x+225+8;

  lbl_XY.Left  := x+225-24;

  ed_Y.Width  := x;   ed_Y.Left  := 2*x+225+16;
  ed_Y1.Width := x;   ed_Y1.Left := 2*x+225+16;
  ed_Y2.Width := x;   ed_Y2.Left := 2*x+225+16;
end;

constructor TDeMapPointObjectPanel.Create(aOwner : TComponent);
begin
  fInit:=True;
  inherited;
  Caption:='';
  BevelOuter:=bvNone;

  CreateLabel (lbl_N, self,  0, 6, '_Df.Name');
  CreateListBox(lb_N, self, 80, 0, self.onComboChange);

  CreateLabel(lbl_0,  self, 200, 6, 'Координаты');
  CreateLabel(lbl_XY, self, 200, 6, 'X : Y'); lbl_XY.Font.Color:=clGray;

  CreateEdit(ed_X, self, 200, 0, self.onChange);
  CreateEdit(ed_Y, self, 200, 0, self.onChange);

  CreateLabel(lbl_1, self, 200, 34, '');
  CreateEdit (ed_X1, self, 200, 28, self.onChange);
  CreateEdit (ed_Y1, self, 200, 28, self.onChange);

  CreateLabel(lbl_2, self, 200, 56, '');
  CreateEdit (ed_X2, self, 200, 50, self.onChange);
  CreateEdit (ed_Y2, self, 200, 50, self.onChange);

  lb_N.Height := 50+2+ed_Y2.Height;

  fInit:=False;
end;

destructor TDeMapPointObjectPanel.Destroy;
begin
  inherited;
  //
end;

procedure TDeMapPointObjectPanel.SetData(aForm : TForm);
begin
  {$IFDEF VECTOR_MAP}
  if aForm is TVectorViewForm then
    FForm:=aForm;
  {$ENDIF}
end;

procedure TDeMapPointObjectPanel.SetValue(aValue: Variant);
var i,N: Integer;
begin
  Caption:='';
  anObject:=nil;
  {$IFDEF VECTOR_MAP}
  if assigned(FForm) then
    if TVectorViewForm(FForm).SchemaView.SelectedCount>0 then
      if TVectorViewForm(FForm).SchemaView.Selected[0] is TdeSimpleCurve then
         anObject := TdeSimpleCurve(TVectorViewForm(FForm).SchemaView.Selected[0]);
  {$ENDIF}

  for i:=0 to ControlCount-1 do
    Controls[i].Enabled:=(anObject<>nil);

  fInit:=True;
  lb_N.Items.Clear;

  if anObject=nil then
    begin
      ClearControlValues(lb_N);
    end
  else
    begin
      N:=anObject.VertexesCount;

      for i:=0 to N-1 do
        lb_N.Items.Add(IntToName(i));

      if anObject.ActiveVertex>-1 then
        lb_N.ItemIndex:=anObject.ActiveVertex
      else
        lb_N.ItemIndex:=0;

      LoadControlValues(lb_N);
    end;

  FInit:=False;
end;

function TDeMapPointObjectPanel.GetValue: Variant;
begin
  result:=null;
end;


{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('DeGraphControls unit initialization ...');
  {$ENDIF}
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DeGraphControls unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

