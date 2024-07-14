unit ItemsOrder;

interface

uses
  Windows, SysUtils, Variants, Classes, Controls, Dialogs, StdCtrls, ExtCtrls, Buttons,
  Forms, CheckLst, Menus, ActnList, Actions, ComCtrls, ToolWin, ImgList, ExtCheckLst,
  BaseFormUnit, DataCacheUnit, DeMeta;

type

  TItemsOrderForm = class(TBaseForm)
    ItemsBox: TExtCheckListBox;
    bt_Da_MoveUp: TButton;
    bt_Da_MoveDown: TButton;
    bt_Da_Show: TButton;
    bt_Da_Hide: TButton;
    bt_Da_reset: TButton;
    lbTitle: TLabel;
    pnlButtons: TPanel;
    ToolBar1: TToolBar;
    tb_Preview: TToolButton;
    tb_Cancel: TToolButton;
    Order_Da_Cancel: TAction;
    Order_Ok: TAction;
    bt_Da_Link: TButton;
    procedure ItemsBoxClick(Sender: TObject);
    procedure ItemsBoxClickCheck(Sender: TObject);
    procedure bt_Da_resetClick(Sender: TObject);
    procedure ItemsBoxEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure bt_Da_MoveUpClick(Sender: TObject);
    procedure bt_Da_MoveDownClick(Sender: TObject);
    procedure bt_Da_ShowClick(Sender: TObject);
    procedure Order_OkExecute(Sender: TObject);
    procedure Order_Da_CancelExecute(Sender: TObject);
    procedure bt_Da_LinkClick(Sender: TObject);
    procedure ItemsBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  private
    FTitle           : string;
    FItems           : TStringList;
    FAllowOrderFirst : boolean;
    FObject          : TObject;
    FFullWidth       : integer;
    FFieldsCache     : TFieldsCache;
    FIconIndex       : Integer;
    procedure SetTitle(const aTitle : string);
    procedure SetAllowOrderFirst(const aValue : boolean);
    function GetItemCount : integer;
    function GetChecked(const aIndex: Integer): Boolean;
    function GetData(const aIndex: Integer): Pointer;
    procedure ModerateButtons;
    function IsChecked: Boolean;
  public
    constructor Create(aOwner : TComponent);  override;
    destructor Destroy;  override;
    property Title : string read FTitle write SetTitle;
    property AllowOrderFirst: Boolean read FAllowOrderFirst write SetAllowOrderFirst;
    property ItemCount : integer read GetItemCount;
    property Checked[const Index: Integer]: Boolean read GetChecked;
    property Data[const Index: Integer]: Pointer read GetData;
    procedure AddItem(const aName: string; aData: Pointer; const aChecked, aAllowChangeChecked: Boolean);
    procedure ReInitForm(AParam : TObject);  override;
    class function Execute(DataCache: TDataCache): Boolean; overload;
    class function Execute(DataCache: TDataCache; ColumnList: TColumnList): Boolean; overload;
  end;

  PItemData = ^TItemData;
  TItemData = record
    Data               : pointer;
    Checked            : boolean;
    AllowChangeChecked : boolean;
  end;

implementation

uses Dictionary, DeTypes, DataUnit;

{$R *.dfm}

constructor TItemsOrderForm.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FItems := TStringList.Create;
  AllowOrderFirst := true;
  FFullWidth := Width;
  FIconIndex := DM.MapIconIndex(icoNext); // Константа для V
end;

destructor TItemsOrderForm.Destroy;
var I : integer;
begin
  for I := 0 to FItems.Count-1 do
    dispose(pointer(FItems.Objects[I]));
  inherited Destroy;
end;

procedure TItemsOrderForm.SetTitle(const aTitle : string);
begin
  FTitle := aTitle;
  lbTitle.Caption := GetTitle(FTitle);
end;

procedure TItemsOrderForm.SetAllowOrderFirst(const aValue : boolean);
begin
  FAllowOrderFirst := aValue;
  ItemsBox.AllowOrderFirst := FAllowOrderFirst;
end;

function TItemsOrderForm.GetItemCount : integer;
begin
  result := ItemsBox.Items.Count;
end;

function TItemsOrderForm.GetChecked(const aIndex: Integer): Boolean;
begin
  Result := ItemsBox.Checked[aIndex];
end;

function TItemsOrderForm.GetData(const aIndex: Integer): Pointer;
begin
  Result := ItemsBox.Items.Objects[aIndex];
end;

procedure TItemsOrderForm.ModerateButtons;
begin
  with ItemsBox do
  begin
    Bt_Da_MoveUp.Enabled :=
      (AllowOrderFirst and (ItemIndex > 0))
      or ((not AllowOrderFirst) and (ItemIndex > 1));
    Bt_Da_Movedown.Enabled :=
      (AllowOrderFirst or (ItemIndex > 0)) and
      (ItemIndex <> (Items.Count-1));
    bt_Da_Hide.Enabled := ItemEnabled[ItemIndex] and Checked[ItemIndex];
    bt_Da_Show.Enabled := ItemEnabled[ItemIndex] and (not Checked[ItemIndex]);
  end;
  Order_Ok.Enabled := IsChecked;
  bt_Da_Link.Enabled := Assigned(FFieldsCache) and
    (ItemsBox.ItemIndex <> -1) and Assigned(TFieldCache(Data[ItemsBox.ItemIndex]).DataCache) and
    Assigned(TFieldCache(Data[ItemsBox.ItemIndex]).DataCache.TableMeta) and
    Assigned(TFieldCache(Data[ItemsBox.ItemIndex]).DataCache.TableMeta.Database) and
    Assigned(TFieldCache(Data[ItemsBox.ItemIndex]).LinkTable) and
    Assigned(TFieldCache(Data[ItemsBox.ItemIndex]).LinkTable.Database) and
    (TFieldCache(Data[ItemsBox.ItemIndex]).DataCache.TableMeta.Database.ID = TFieldCache(Data[ItemsBox.ItemIndex]).LinkTable.Database.ID);
end;

procedure TItemsOrderForm.AddItem(const aName: string; aData: Pointer; const aChecked, aAllowChangeChecked: Boolean);
var PData : PItemData;
begin
  PData := new(PItemData);
  PData^.Data := aData;
  PData^.Checked := aChecked;
  PData^.AllowChangeChecked := aAllowChangeChecked;
  FItems.AddObject(aName, TObject(PData));
end;

procedure TItemsOrderForm.ReInitForm(AParam: TObject);
var I, Index : integer;
    s: String;
begin
  s:=Caption;
  inherited;
  Caption:=s;

  FObject := aParam;
  lbTitle.Caption := GetTitle(FTitle);
  with ItemsBox do
  begin
    Index := ItemIndex;
    Items.Clear;
    for I := 0 to FItems.Count-1 do
    begin
      Items.AddObject(FItems[I], PItemData(FItems.Objects[I])^.Data);
      Checked[I] := PItemData(FItems.Objects[I])^.Checked;
      ItemEnabled[I] := PItemData(FItems.Objects[I])^.AllowChangeChecked;
    end;
    if Index < 0 then
      ItemIndex := 0
    else
      ItemIndex := Index;
  end;
  ModerateButtons;
end;

procedure TItemsOrderForm.ItemsBoxClick(Sender: TObject);
begin
  inherited;
  ModerateButtons;
end;

procedure TItemsOrderForm.ItemsBoxClickCheck(Sender: TObject);
begin
  inherited;
  ModerateButtons;
end;

procedure TItemsOrderForm.ItemsBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  (Control as TCheckListBox).Canvas.FillRect(Rect);
  (Control as TCheckListBox).Canvas.TextOut(Rect.Left + 2 + 16 + 2, Rect.Top, (Control as TCheckListBox).Items[Index]);
  if Assigned(FFieldsCache) and Assigned(TFieldMeta(Data[Index]).LinkTable) then
    DM.ilIcon16.Draw((Control as TCheckListBox).Canvas, Rect.Left + 2 , (Rect.Top + Rect.Top + Rect.Height - 16) div 2, FIconIndex);
  if odSelected in State then (Control as TCheckListBox).Canvas.DrawFocusRect(Rect);
end;

procedure TItemsOrderForm.bt_Da_resetClick(Sender: TObject);
begin
  inherited;
  ReInitForm(FObject);
end;

procedure TItemsOrderForm.ItemsBoxEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  inherited;
  ModerateButtons;
end;

procedure TItemsOrderForm.bt_Da_MoveUpClick(Sender: TObject);
var NewItemIndex:integer;
begin
  inherited;
  with ItemsBox do
  begin
    NewItemIndex := ItemIndex-1;
    Items.Move(ItemIndex, ItemIndex-1);
    ItemIndex := NewItemIndex;
  end;
  ModerateButtons;
end;

procedure TItemsOrderForm.bt_Da_MoveDownClick(Sender: TObject);
var
  NewItemIndex:integer;
begin
  inherited;
  with ItemsBox do
  begin
    NewItemIndex := ItemIndex+1;
    Items.Move(ItemIndex, ItemIndex+1);
    ItemIndex := NewItemIndex;
  end;
  ModerateButtons;
end;

procedure TItemsOrderForm.bt_Da_ShowClick(Sender: TObject);
begin
  inherited;
  with ItemsBox do
    Checked[ItemIndex]:=not(Checked[ItemIndex]);
  ModerateButtons;
end;

procedure TItemsOrderForm.Order_OkExecute(Sender: TObject);
begin
  inherited;
  ModalResult:=mrOk;
end;

procedure TItemsOrderForm.Order_Da_CancelExecute(Sender: TObject);
begin
  inherited;
  ModalResult:=mrCancel;
end;

function TItemsOrderForm.IsChecked: Boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to Pred(ItemCount) do
    if Checked[Index] then
      begin
        Result := True;
        Break;
      end;
end;

class function TItemsOrderForm.Execute(DataCache: TDataCache): Boolean;
var
  Index, FieldPosition: Integer;
  Field: TFieldCache;
  FieldChecked, FieldAllowChangeChecked: Boolean;
  FieldName, FieldCaption: string;
begin
  Result := Assigned(DataCache) and Assigned(DataCache.Fields) and (DataCache.Fields.Count <> 0);
  if Result then
    with Self.Create(Application) do
      try
        FFieldsCache := DataCache.Fields;
        for Index := 0 to Pred(FFieldsCache.Count) do
          begin
            Field := FFieldsCache[Index];
            if Assigned(Field) and Field.PolicyShow then
              if not Field.IsLookUp then
                begin
                  FieldCaption := Field.Native;
                  FieldName := Field.Original;
                  FieldPosition := Pos('.', FieldName);
                  if Assigned(Field.Owner.Owner.TableMeta) and Assigned(DataCache.TableMeta) and (FieldPosition = 0) then
                    begin
                      FieldChecked := (Field.VisibleLevel >= fvLevel2) or (Field.VisibleLevel = fvCaption);
                      FieldAllowChangeChecked := Field.VisibleLevel <> fvCaption;
                    end
                  else
                    begin
                      FieldName := Copy(FieldName, 1, Pred(FieldPosition));
                      FieldPosition := FFieldsCache.IndexByName(FieldName);
                      if FieldPosition <> -1 then
                        if Assigned(FFieldsCache.Items[FieldPosition].LinkTable) then
                          FieldCaption := FFieldsCache.Items[FieldPosition].LinkTable.Name + '.' + FieldCaption;
                      FieldChecked := (Field.VisibleLevel >= fvLevel2);
                      FieldAllowChangeChecked := True;
                    end;
                  AddItem(FieldCaption, Field, FieldChecked, FieldAllowChangeChecked);
                end;
          end;
        ReInitForm(nil);
        Result := ShowModal = mrOK;
        if Result then
          for Index := 0 to Pred(ItemCount) do
            begin
              Field := Data[Index];
              Field.Order := Index;
              if Checked[Index] then Field.VisibleLevel := fvLevel2
                                else Field.VisibleLevel := fvLevel1;
            end;
      finally
        Free;
      end;
end;

class function TItemsOrderForm.Execute(DataCache: TDataCache; ColumnList: TColumnList): Boolean;
var
  Index, FieldPosition: Integer;
  Field: TFieldMeta;
  FieldChecked, FieldAllowChangeChecked: Boolean;
  FieldName, FieldCaption: string;
begin
  Result := Assigned(ColumnList) and (ColumnList.Count <> 0) and Assigned(DataCache) and Assigned(DataCache.Fields);
  if Result then
    with Self.Create(Application) do
      try
        Caption := GetTitle('_Di.column',ttSecondName);
        Title   := Format(GetTitle('_dM.Sequence'), [GetTitle('_di.column',ttSecondName)]);

        FFieldsCache := DataCache.Fields;
        for Index := 0 to Pred(ColumnList.Count) do
          begin
            Field := ColumnList[Index].Field;
            if Assigned(Field) and Field.PolicyShow then
              if not Field.IsLookUp then
                begin
                  FieldCaption := Field.Native;
                  FieldName := Field.Original;
                  FieldPosition := Pos('.', FieldName);
                  if Assigned(DataCache.TableMeta) and (FieldPosition = 0) then
                    begin
                      FieldChecked := (Field.VisibleLevel >= fvLevel2) or (Field.VisibleLevel = fvCaption);
                      FieldAllowChangeChecked := Field.VisibleLevel <> fvCaption;
                    end
                  else
                    begin
                      FieldName := Copy(FieldName, 1, Pred(FieldPosition));
                      FieldPosition := FFieldsCache.IndexByName(FieldName);
                      if FieldPosition <> -1 then
                        if Assigned(FFieldsCache.Items[FieldPosition].LinkTable) then
                          FieldCaption := FFieldsCache.Items[FieldPosition].LinkTable.Name + '.' + FieldCaption;
                      FieldChecked := (Field.VisibleLevel >= fvLevel2);
                      FieldAllowChangeChecked := True;
                    end;
                  AddItem(FieldCaption, Field, FieldChecked, FieldAllowChangeChecked);
                end;
          end;
        ReInitForm(nil);
        Result := ShowModal = mrOK;
        if Result then
          for Index := 0 to Pred(ItemCount) do
            begin
              Field := Data[Index];
              Field.Order := Index;
              if Checked[Index] then
                Field.VisibleLevel := fvLevel2
              else
                Field.VisibleLevel := fvLevel1;
            end;
      finally
        Free;
      end
  else
    Result := Execute(DataCache);
end;

procedure TItemsOrderForm.bt_Da_LinkClick(Sender: TObject);
var
  FieldCache, NewFieldCache: TFieldCache;
  FieldMeta: TFieldMeta;
  Index, FieldPosition, ExistPosition: Integer;
begin
  if ItemsBox.ItemIndex <> -1 then
    FieldCache := Data[ItemsBox.ItemIndex]
  else
    FieldCache := nil;
  if Assigned(FieldCache) and Assigned(FieldCache.LinkTable) then
    with TItemsOrderForm.Create(Application) do
      try
        for Index := 0 to Pred(FieldCache.LinkTable.Fields.Count) do
          begin
            FieldMeta := FieldCache.LinkTable.Fields[Index];
            if FieldMeta.PolicyShow then
            if not FieldMeta.IsLookUp then
              AddItem(FieldMeta.Native, FieldMeta, Self.FFieldsCache.IndexByID(FieldMeta.ID) <> -1, True);
          end;
        ReInitForm(nil);
        if ShowModal = mrOK then
          begin
            FieldPosition := Self.FFieldsCache.Count;
            for Index := Pred(Self.FFieldsCache.Count) downto 0 do
              if Self.FFieldsCache[Index].IsLookup then
                Dec(FieldPosition)
              else
                Break;
            for Index := 0 to Pred(ItemCount) do
              if Checked[Index] then
                begin
                  NewFieldCache := Self.FFieldsCache.FindByID(TFieldMeta(Data[Index]).ID) as TFieldCache;
                  if not Assigned(NewFieldCache) then
                    begin
                      NewFieldCache := TFieldCache.Create(Self.FFieldsCache);
                      NewFieldCache.Assign(Data[Index]);
                      NewFieldCache.Original := {FieldCache.Original + '.' +} TFieldMeta(Data[Index]).Original;
                      NewFieldCache.Order := Self.ItemCount + Index;
                      NewFieldCache.VisibleLevel := fvLevel2;
                      Self.FFieldsCache.Insert(FieldPosition, NewFieldCache);
                      Self.AddItem(FieldCache.Native + '.' + NewFieldCache.Native, NewFieldCache, True, True);
                      Inc(FieldPosition);
                    end;
                end;
            Self.ReInitForm(nil);
          end;
      finally
        Free;
      end;
end;

end.

