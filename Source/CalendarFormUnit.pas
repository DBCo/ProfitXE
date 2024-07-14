unit CalendarFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, Actions, ActnList, ComCtrls, DB, CommCtrl, StdCtrls, ExtCtrls,
  Buttons, ToolWin,
  DeMeta, DeCalendar, BaseGridFormUnit, DataCacheUnit, Vcl.ImgList;

type

   TCListview = Class(TListview)
   private
   end;

  TCalendarForm = class(TBaseGridForm)
    act_Da_Months: TAction;
    act_Da_Weeks: TAction;
    act_Da_IncLine: TAction;
    act_Da_DecLine: TAction;
    itemByMonths: TMenuItem;
    itemByWeeks: TMenuItem;
    ItemWeeks: TMenuItem;
    Incline1: TMenuItem;
    Decline1: TMenuItem;
    MIbymonths: TMenuItem;
    MIbyweeks: TMenuItem;
    act_Da_Days: TAction;
    itemByDays: TMenuItem;
    Bydays: TMenuItem;
    act2_Da_Default: TAction;
    itemByUser: TMenuItem;
    Calendar: TCustomCalendar;
    MiByDefault: TMenuItem;
    procedure CalendarCreatingContainer(Sender: TCalendarCellContainer;var AContainerObject: TWinControl);
    procedure CalendarFillingContainer(Sender: TCalendarCellContainer;AContainerObject: TWinControl);
    procedure CalendarActivateContainer(Sender: TCalendarCellContainer;AContainerObject: TWinControl);
    procedure CalendarDeactivateContainer(Sender: TCalendarCellContainer;AContainerObject: TWinControl);
    procedure CalendarDestroingContainer(Sender: TCalendarCellContainer;AContainerObject: TWinControl);
    procedure CalendarClearContainer(Sender: TCalendarCellContainer;AContainerObject: TWinControl);
    procedure CalendarDateChange(Sender: TObject; ADate: TDateTime);
    procedure act_Da_MonthsExecute(Sender: TObject);
    procedure act_Da_WeeksExecute(Sender: TObject);
    procedure act_Da_IncLineExecute(Sender: TObject);
    procedure act_Da_DecLineExecute(Sender: TObject);
    procedure CalendarDropTargetChange(DropControl: TControl; DropControlKind: TDropControlKind);
    procedure act_Da_DaysExecute(Sender: TObject);
    procedure act2_Da_DefaultExecute(Sender: TObject);
    procedure CalendarDblClickCaption(Sender: TObject);
    procedure WMCommand(var Msg: TWMCommand);message WM_COMMAND;
    procedure FormDblClick(Sender: TObject);
    procedure CalendarRangeChange(Sender: TObject; ADate, BDate: TDateTime);
  private
    FAddFields: Array of Integer;
    FViewType, FPrevViewType : integer;
    FieldXIndex : Integer;
    FDropControl: TControl;
    FDropControlKind: TDropControlKind;
    FFirstDate, FEndDate : TDate;
    FDefRowCount: Integer;
    FDefColCount: Integer;
    FDefViewType: TCalendarViewType;
    procedure ListViewAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure ListViewCustomDraw(Sender: TCustomListView; const ARect: TRect; var DefaultDraw: Boolean);
    procedure DoSplit(Sender:TObject);
    procedure SetFieldX(aField: TFieldMeta);
  protected
    procedure InitForm;override;
    procedure AfterInit;override;
    procedure ListViewInfoTip(Sender: TObject; Item: TListItem; var InfoTip: String);
    function GetStatusText : string; override;
    procedure ListItemChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure CacheSelectedChange(Item : TCacheItem);override;
    procedure CacheFocusedChange(Item : TCacheItem);override;
    procedure CacheSelectedChanging(OldItem : TCacheItem);override;
    procedure CacheFocusedChanging(OldItem : TCacheItem);override;
    procedure CacheItemInsert(Item : TCacheItem);override;
    procedure CacheSyncronize(var AFocusedItem : TCacheItem; AIndex : Integer = ltNone);override;
    function  GetCacheItemAt(X, Y: Integer):TCacheItem; override;
    function DragInfoTarget(Sender, Source: TObject; X, Y: Integer; aDragInfo: TDragInfo): Boolean; override;
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer);override;
  public
    constructor Create(aOwner : TComponent);  override;
    procedure DeInitForm; override;
    procedure ReCreateView;override;
    function GetPropertiesString: string; override;
    property FieldX: TFieldMeta read FFieldX write SetFieldX;
   // procedure ChangeDataSetStatus;override;
  end;

implementation

uses Variants, DateUtils, Math,
     DeLog, DataUnit, DSMeta, Funcs, DeTypes, DeSettings, Dictionary;

{$R *.dfm}

{ TCListview }

{procedure TCListview.CMDrag(var Message: TCMDrag);
begin
  Self.Parent.WindowProc(TMessage(Message));
  inherited;

end;}


{ TCalendarForm }

procedure TCalendarForm.InitForm;
var
  i: Integer;
  RowCount  : string;
  ColCount  : string;
  DateField : string;
  ViewType  : string;
begin
  FFirstDate:= DATE_NONE;
  FEndDate:= DATE_NONE;

  if DataSetMeta=nil then Exit;
  inherited InitForm;

  for i := Pred(TempActionList.ActionCount) downto 0 do
    if SameText(TempActionList[i].Category, CategorySplit) then
      TempActionList[i].Free;

  for i:=0 to DataSetMeta.Table.Fields.Count-1 do
    if DataSetMeta.Table.Fields[i].PolicySelect then
      if DataSetMeta.Table.Fields[i].DataType in DateTypes then
        TMetaAction.Create(self, CategorySplit, DataSetMeta.Table.Fields[i], DoSplit, [Item_Da_Splitby, MM_Da_Splitby]).ActionList:= TempActionList;

  Calendar.OnDblClickCellContent:= FormDblClick;
  FViewType := -1;

  RowCount := ViewParams.GetByName('CalendarROW').Value;
  ColCount := ViewParams.GetByName('CalendarCOL').Value;
  DateField:= ViewParams.GetByName('CalendarFIELD').Value;
  ViewType := ViewParams.GetByName('CalendarVIEW').Value;

  if ViewParams.TestByName('CalendarVIEW','Day')   then
    begin
      FDefRowCount:= 1;
      FDefColCount:= 1;
      FDefViewType:= cvDays;
      act_Da_Days.Checked := true;
    end else
  if ViewParams.TestByName('CalendarVIEW','Month') then
    begin
      FDefRowCount:= 6;
      FDefColCount:= 7;
      FDefViewType:= cvMonths;
      act_Da_Months.Checked := true;
    end else
  if (Length(RowCount) > 0) or (Length(ColCount) > 0) then
    begin
      FDefRowCount:= StrToIntDef(RowCount,1);
      FDefColCount:= StrToIntDef(ColCount,7);
      FDefViewType:= cvDays;
      act2_Da_Default.Checked := true;
    end
  else   // по умолчанию показываем календарь 'Week'
    begin
      FDefRowCount:= 1;
      FDefColCount:= 7;
      FDefViewType:= cvWeeks;
      act_Da_Weeks.Checked := true;
    end;

  FieldX := CacheItems.Fields.FindByName(DateField);
  if Not assigned(FieldX) or (Not (FieldX.DataType in DateTypes)) then
    for i:=0 to CacheItems.TableMeta.Fields.Count -1 do
      if CacheItems.TableMeta.Fields[i].DataType in DateTypes then
        begin
          FieldX:= CacheItems.TableMeta.Fields[i];
          Break;
        end;

  FPrevViewType:=-1;

  Calendar.CurrentDate := Date;
  Calendar.SetParams(FDefViewType, FDefRowCount, FDefColCount);
end;

procedure TCalendarForm.AfterInit;
begin
//  Calendar.BeginUpdate;
    inherited;
//  if Calendar.CurrentDate = DATE_NONE then
//    Calendar.CurrentDate := Date;
//    Calendar.SetParams(FDefViewType, FDefRowCount, FDefColCount);
//  Calendar.EndUpdate;
end;

procedure TCalendarForm.ReCreateView;
var i: Integer;
begin
  SetLength(FAddFields, 0);
  if assigned(DataSetMeta) then
   for i:=0 to Pred(DataSetMeta.Cache.FieldCount) do
     if fvLevel3 <= DataSetMeta.Cache.Fields[i].VisibleLevel then
     if DataSetMeta.Cache.Fields[i].ID <> DataSetMeta.Table.NField.ID then
     if not assigned(DataSetMeta.Cache.Fields[i].LookupPair) or (DataSetMeta.Cache.Fields[i].LookupPair.ID <> DataSetMeta.Table.NField.ID) then
     if DataSetMeta.Cache.Fields[i].ID <> FFieldX.ID then
     if DataSetMeta.Cache.Fields[i].IsLookup or (DataSetMeta.Cache.Fields[i].LinkTable = nil) then
       begin
         SetLength(FAddFields, Length(FAddFields)+1);
         FAddFields[Length(FAddFields)-1]:= i;
       end;

  if DataSetMeta.Cache.Active then
  begin
    CacheItems.FillAll;
    if HandleAllocated then Calendar.RefreshContent;
  end;
  inherited;
end;

procedure TCalendarForm.SetFieldX(aField: TFieldMeta);
begin
  if assigned(DataSetMeta) and assigned(aField) then FieldXIndex:= DataSetMeta.Cache.Fields.IndexByID(aField.ID)
                                                else FieldXIndex:= -1;
  if -1 < FieldXIndex then FFieldX:= aField
                      else FFieldX:= nil;
end;

procedure TCalendarForm.ListViewInfoTip(Sender: TObject; Item: TListItem; var InfoTip: String);
var N : Integer;
begin
  if (DataSetMeta=nil) then Exit;
  if (Assigned(Item))and(Item.Focused)and(DataSetMeta.Cache.Active) then
    begin
      N:=  DataSetMeta.Cache.IndexByItem(TCacheItem(Item.Data));
      InfoTip:=DataSetMeta.Cache.Hints[N];
    end;
end;

procedure TCalendarForm.CalendarCreatingContainer( Sender: TCalendarCellContainer; var AContainerObject: TWinControl);
var i: Integer;
begin
  AContainerObject := TCListview.Create(Self);
  with TCListview(AContainerObject) do
  begin
     DoubleBuffered:=True;
     OnInfoTip:= ListViewInfoTip;
     BorderStyle := bsNone;
     FlatScrollBars := True;
     SmallImages:= DM.ilIcon16; // DM.SImages;
     ViewStyle := vsReport;
     with Columns.Add do
        begin
          AutoSize := True;
          MinWidth := 100;
        end;
     for i:=0 to Pred(Length(FAddFields)) do
       with Columns.Add do
         begin
         //AutoSize := False;
           AutoSize := True;
           MinWidth := 50;
         end;
     ShowColumnHeaders := False;
     RowSelect := True;
     ReadOnly := True;
     HideSelection := False;
     MultiSelect := True;
     OnAdvancedCustomDrawItem := ListViewAdvancedCustomDrawItem;
     OnDblClick := FormDblClick;
     OnCustomDraw := ListViewCustomDraw;
     OnDragOver := Self.DoDragOver;
     OnDragDrop := Self.DoDragDrop;
  end;
end;

procedure TCalendarForm.CalendarDestroingContainer(Sender: TCalendarCellContainer; AContainerObject: TWinControl);
begin
  with TCListview(AContainerObject) do
  begin
    OnChange := nil;
  end;
end;

procedure TCalendarForm.CalendarRangeChange(Sender: TObject; ADate, BDate: TDateTime);
var Key: Variant;
begin
  inherited;
  if (ADate < FFirstDate) or (FEndDate < BDate) or (FFirstDate = DATE_NONE) or  (FEndDate = DATE_NONE) then
    begin
      FFirstDate:= ADate;
      FEndDate:= BDate;

      DataSetMeta.FormFilterPostfix.Clear;
      DataSetMeta.FormFilterPostfix.AddCondition(FieldX, opGE, FFirstDate);
      DataSetMeta.FormFilterPostfix.AddCondition(FieldX, opLT, FEndDate);
      DataSetMeta.FormFilterPostfix.AddOperation(opAnd);

      Key := Null;
      if DataSetMeta.Cache.Active then
        if Assigned(DataSetMeta.Cache.FocusedItem) then
          Key:= DataSetMeta.Cache.FocusedItem.ID;

      Calendar.ClearCellContainers;
      DataSetMeta.Cache.Update(mcUpdateLibrary, Key);
    end;
end;

procedure TCalendarForm.CalendarFillingContainer(Sender: TCalendarCellContainer; AContainerObject: TWinControl);
var
  i,j: integer;
  Item: TListItem;
  Key: Variant;
begin
  if (not Assigned(CacheItems)) or (not assigned(FieldX)) then Exit;
  with TCListview(AContainerObject) do
    begin
      OnChange := nil;
      Items.BeginUpdate;
      Items.Clear;
      for i:= 0 to Pred(CacheItems.Count) do
        begin
          Key:= CacheItems.Items[i].FieldValue[FieldXIndex];
          if (not VarIsNull(Key)) and (Trunc(Key) = Sender.CurrentDate) then
          begin
            Item:= Items.Add;
            Item.Data:=CacheItems.Items[i];
            CacheItems.Items[i].Data:= Item;
            Item.Caption:= CacheItems.Items[i].Caption;
            Item.ImageIndex:= DM.MapIconIndex(CacheItems.Items[i].ImageIndex);
            Item.Selected:= CacheItems.Items[i].Selected;
            for j:=0 to Pred(Length(FAddFields)) do
              Item.SubItems.add(CacheItems.Items[i].FieldText(FAddFields[j]));
          end;
        end;
      Items.EndUpdate;

      OnChange := ListItemChange;
    end;
end;

procedure TCalendarForm.CalendarClearContainer(Sender: TCalendarCellContainer; AContainerObject: TWinControl);
begin
  if not Assigned(AContainerObject) then
    CacheItems.ClearLinks
  else
    with TCListview(AContainerObject) do
      begin
        OnChange:= nil;
        Items.BeginUpdate;
        Items.Clear;
        Items.EndUpdate;
      end;
end;

procedure TCalendarForm.CalendarDateChange(Sender: TObject; ADate: TDateTime);
begin
// FormCaption:= getCaptionText + '  ['+DeCalendar.LongMonthNames[MonthOf(ADate)] + ' ' + IntToStr(YearOf(ADate)) + ']';
end;

procedure TCalendarForm.ListItemChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if (Assigned(Item))and(DataSetMeta.Cache.Active) then
  begin
    TCacheItem(Item.Data).Selected:= Item.Selected;
    if Item.Focused then
      TCacheItem(Item.Data).SetFocus;
  end;
  ChangeDataSetStatus;
end;

procedure TCalendarForm.CalendarActivateContainer( Sender: TCalendarCellContainer; AContainerObject: TWinControl);
var NewFocusedItem : TCacheItem;
    i,N: Integer;
begin
  if not (Assigned(CacheItems) and Assigned(AContainerObject)) then Exit;

  if (TListView(AContainerObject).Items.Count > 0) then
    begin
      N:= Max(0, TListView(AContainerObject).ItemIndex);

      if assigned(CacheItems.FocusedItem) then
        for i:=0 to Pred(TListView(AContainerObject).Items.Count) do
          if assigned(TListView(AContainerObject).Items[i].Data) then
            if TCacheItem(TListView(AContainerObject).Items[i].Data).ID = CacheItems.FocusedItem.ID then
              begin
                N:= i;
                Break;
              end;

      TListView(AContainerObject).Items[N].Focused:= True;
      for I := 0 to Pred(TListView(AContainerObject).Items.Count) do
         TListView(AContainerObject).Items[I].Selected := (i = N);

      NewFocusedItem:= TCacheItem(TListView(AContainerObject).Items[N].Data);
      NewFocusedItem.SetFocus;

      CacheItems.ClearSelection;
      NewFocusedItem.Selected:= True;
    end
  else
    begin
      CacheItems.ClearSelection;
      CacheItems.FocusedItem := nil;

      TListView(AContainerObject).ClearSelection;
      TListView(AContainerObject).ItemFocused:= nil;
    end;

  ChangeDataSetStatus;
end;

procedure TCalendarForm.CalendarDeactivateContainer( Sender: TCalendarCellContainer; AContainerObject: TWinControl);
begin
  if not (Assigned(CacheItems) and Assigned(AContainerObject)) then Exit;

//  CacheItems.ClearSelection;
//  CacheItems.FocusedItem:= Nil;

  TListView(AContainerObject).ClearSelection;
  TListView(AContainerObject).ItemFocused:= nil;

  ChangeDataSetStatus;
end;

procedure TCalendarForm.CacheFocusedChange(Item: TCacheItem);
begin
  if (Assigned(Item))and(Assigned(Item.Data)) then
  begin
    TListview(TListItem(Item.Data).ListView).OnChange := nil;
    TListItem(Item.Data).Focused := True;
    TListItem(Item.Data).MakeVisible(False);
    TListview(TListItem(Item.Data).ListView).OnChange := ListItemChange;
  end;
end;

procedure TCalendarForm.CacheSelectedChange(Item: TCacheItem);
begin
  if (Assigned(Item))and(Assigned(Item.Data)) then
  begin
    TListview(TListItem(Item.Data).ListView).OnChange := nil;
    TListItem(Item.Data).ListView.ClearSelection;
    TListItem(Item.Data).Selected:= True;
    TListview(TListItem(Item.Data).ListView).OnChange := ListItemChange;
  end;
end;

procedure TCalendarForm.CacheSelectedChanging(OldItem : TCacheItem);
begin
  if (Assigned(OldItem))and(Assigned(OldItem.Data)) then
  begin
    TListview(TListItem(OldItem.Data).ListView).OnChange := nil;
    TListItem(OldItem.Data).Selected:= False;
    TListview(TListItem(OldItem.Data).ListView).OnChange := ListItemChange;
  end;
end;

procedure TCalendarForm.CacheFocusedChanging(OldItem : TCacheItem);
begin
  if (Assigned(OldItem))and(Assigned(OldItem.Data)) then
  begin
    TListview(TListItem(OldItem.Data).ListView).OnChange := nil;
    TListItem(OldItem.Data).Focused:= False;
    TListview(TListItem(OldItem.Data).ListView).OnChange := ListItemChange;
  end;
end;

procedure TCalendarForm.CacheItemInsert(Item: TCacheItem);
begin
  inherited;
  Item.FieldValue[FieldXIndex]:= Calendar.CurrentDate;
end;

procedure TCalendarForm.CacheSyncronize(var AFocusedItem : TCacheItem; AIndex : Integer = ltNone);
var i,N: Integer;
begin
  if (aFocusedItem is TRootItem) then Exit;

  if Assigned(AFocusedItem) then
    if (AFocusedItem.FieldValue[FieldXIndex] <> Calendar.CurrentDate) then AFocusedItem:= nil;

  if Assigned(CacheItems.FocusedItem) then
    if CacheItems.FocusedItem.FieldValue[FieldXIndex] = Calendar.CurrentDate then
      AFocusedItem := CacheItems.FocusedItem;

  if (AIndex = ltNone) or (CacheItems.Count = 0) then
    begin
      AFocusedItem:= nil;
    end else

  if (AIndex = ltLast) or (Pred(CacheItems.Count) <= AIndex) then
    begin
      for i:= Pred(CacheItems.Count) downto 0 do
        if CacheItems.Items[i].FieldValue[FieldXIndex] = Calendar.CurrentDate then
          begin
            AFocusedItem:= CacheItems[i];
            Break;
          end
    end else

  if (0 <= AIndex) and (0 < CacheItems.Count) then
    begin
      N:= AIndex;
      for i:= 0 to Pred(CacheItems.Count) do
        if CacheItems.Items[i].FieldValue[FieldXIndex] = Calendar.CurrentDate then
          begin
            AFocusedItem := CacheItems[i];
            Dec(N);
            if N < 0 then Break;
          end;
    end;
end;

constructor TCalendarForm.Create(aOwner : TComponent);
begin
  inherited;
  FMainControl:= Calendar;
end;

procedure TCalendarForm.DeInitForm;
begin
  FSelectRect.Parent:= nil;
  Calendar.ClearContent;
  inherited DeInitForm;
end;

procedure TCalendarForm.act_Da_MonthsExecute(Sender: TObject);
begin
  act_Da_Months.Checked := True;
  Calendar.BeginUpdate;
  Calendar.SetParams(cvMonths);
  Calendar.EndUpdate;
  FPrevViewType:=FViewType;
  FViewType := integer(Calendar.ViewType);
 //Item_Weeks.Visible := False;
end;

procedure TCalendarForm.act_Da_WeeksExecute(Sender: TObject);
begin
  act_Da_Weeks.Checked := True;
  Calendar.SetParams(cvWeeks);
  FPrevViewType:=FViewType;
  FViewType := integer(Calendar.ViewType);
//  Item_Weeks.Visible := True;
end;

procedure TCalendarForm.act_Da_DaysExecute(Sender: TObject);
begin
  act_Da_Days.Checked := True;
  Calendar.SetParams(cvDays);
  FPrevViewType:=FViewType;
  FViewType := integer(Calendar.ViewType);
//  Item_Weeks.Visible := True;
end;

procedure TCalendarForm.act_Da_IncLineExecute(Sender: TObject);
begin
  Calendar.SetParams(Calendar.ViewType, Succ(Calendar.RowCount), Calendar.ColCount);
end;

procedure TCalendarForm.act_Da_DecLineExecute(Sender: TObject);
begin
  Calendar.SetParams(Calendar.ViewType, Pred(Calendar.RowCount), Calendar.ColCount);
end;

procedure TCalendarForm.CalendarDropTargetChange(DropControl: TControl; DropControlKind: TDropControlKind);
begin
  FDropControl := DropControl;
  FDropControlKind := DropControlKind;
  FMainControl:= TWinControl(FDropControl);
end;

function TCalendarForm.GetCacheItemAt(X, Y: Integer): TCacheItem;
var
  Item : TListItem;
begin
  if (FDropControlKind = dcContainObject)and(FDropControl is TListView) then
  begin
    Item   := TListView(MainControl).GetItemAt(X, Y);
    if Assigned(Item) then
      result :=TCacheItem(Item.Data)
    else
      result := inherited GetCacheItemAt(X, Y);
  end
  else
    result := inherited GetCacheItemAt(X, Y);
end;

procedure TCalendarForm.FormDblClick(Sender: TObject);
begin
  inherited;
  if ViewArea then HideViewArea(True)
              else ShowViewArea(True);
end;

function TCalendarForm.DragInfoTarget(Sender, Source: TObject; X, Y: Integer; aDragInfo: TDragInfo): Boolean;
var
  CurrentIndex: Integer;
  T: TRect;
  LI: TListItem;
  TLV: TListView;
begin
  inherited;  // Здесь определяются TDragInfo: XField, YField, canXY, Source

  if DataSetMeta.Table.ID = aDragInfo.Source.Table.ID then
  begin
    aDragInfo.IField := aDragInfo.Source.Table.Fields.IndexByLink(DataSetMeta.Table.ID);
    aDragInfo.DragTypes := [dtSelf];
  end;

  if Assigned(FDropControl) then
    begin
      aDragInfo.XField:= FieldXIndex;
      if FDropControl is TCalendarCellContainer then // мышь над заголовком дня
        begin
          aDragInfo.XValue:= TCalendarCellContainer(FDropControl).CurrentDate;
          aDragInfo.Control:= nil; //TListView(TCalendarCellContainer(FDropControl).ContainObject);
        //aDragInfo.Rect:= TCalendarCellContainer(FDropControl).ContainObject.ClientRect;
          aDragInfo.Align:= alNone;//Client;
          aDragInfo.OField:= -1;
        end else
      if FDropControl is TListView then // мышь над ListView
        begin
          aDragInfo.Control:= TListView(FDropControl);
          aDragInfo.XValue:= TCalendarCellContainer(aDragInfo.Control.Parent).CurrentDate;
        end else
{     if Calendar.FirstCellDate = Calendar.EndCellDate then // отображается единственный день - его и ставим
        begin
          aDragInfo.XValue:= Calendar.CurrentDate;
          aDragInfo.Area:= Calendar.Cells[0].ContainObject;
          aDragInfo.Align:= alClient;
          DoOrder:= True;
        end else  {}
        begin  // мышь над чем-то другим, например днями недели и дату определить невозможно
          aDragInfo.XValue:= unassigned;
          aDragInfo.Control:= nil;
          aDragInfo.Align:= alNone;
          aDragInfo.OField:= -1;
        end;

      if aDragInfo.CanOrder and assigned(aDragInfo.Control) and (aDragInfo.Control is TListView) then
        begin
          TLV:= TListView(aDragInfo.Control);
          // определяем элемент, перед которым вставляем и позицию указателя вставки
          LI:= TLV.GetItemAt(X,Y);
          if assigned(LI) then
              begin
                CurrentIndex:= LI.Index;
                T:= LI.DisplayRect(drSelectBounds);

                if (T.CenterPoint.Y < Y) then
                  begin
                    FDragInfo.Rect:= Rect(T.Left, T.Top, T.Right, T.Bottom);
                    FDragInfo.Align:= alBottom;
                    Inc(CurrentIndex);
                  end else
                  begin
                    FDragInfo.Rect:= Rect(T.Left, T.Top, T.Right, T.Bottom);
                    FDragInfo.Align:= alTop;
                  end;

                // скроллинг
                if (Y < T.Height) then
                  begin
                    TLV.Scroll(0, - T.Height);
                    TLV.Repaint;
                    sleep((Y*200) div T.Height); // До 200 мс на краю
                  end;
                if (TLV.ClientHeight - T.Height < Y) then
                  begin
                    TLV.Scroll(0, + T.Height);
                    TLV.Repaint;
                    sleep(((TLV.ClientHeight - Y)*200) div T.Height); // До 200 мс на краю
                  end;
              end else

            if TLV.Items.Count = 0 then
              begin
                CurrentIndex:= -1;
          //      FDragInfo.Rect:= Rect(0, 0, TListView(_Sender).ClientWidth, 10);

                SendMessage(TLV.Handle, LVM_GETITEMRECT, 0, LPARAM(@FDragInfo.Rect));
                FDragInfo.Align:= iif( Integer(TLV.ViewStyle) = LV_VIEW_DETAILS, alTop, alLeft);

                FDragInfo.Align:= alClient;//alTop;
              end else
          //if "After last item" then
              begin
                CurrentIndex:= TLV.Items.Count;
                T:= TLV.Items[Pred(CurrentIndex)].DisplayRect(drSelectBounds);
                FDragInfo.Rect:= Rect(T.Left, T.Bottom, T.Right, T.Bottom);
                FDragInfo.Align:= alBottom;
              end;
        end;

    end
  else
    begin
      aDragInfo.XField:= -1;
      aDragInfo.XValue:= unassigned;
    end;

  Result:= aDragInfo.СanAny;
end;

procedure TCalendarForm.DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  CurrentIndex :integer;
  T: TRect;
  LI: TListItem;
begin
  TListView(Sender).DragMode:= dmManual;//dmAutomatic ; .Dragging:= False;

  inherited;
//inherited DoDragOver(_Sender, Source, X, Y, State, Accept);
  if (State = dsDragEnter) and (Sender is TListView) then
    try // Мелочь, но может породить ошибки
      if 1 < GetParentBaseGridForm(Source).DataSetMeta.Cache.SelectedCount
        then TListView(Sender).DragCursor:= crMultiDrag
        else TListView(Sender).DragCursor:= crDrag;
    except
    end;

  DragInfoTarget(Sender, Source, X, Y, FDragInfo);

  Accept:= FDragInfo.СanAny;
  // позиционируем указатель вставки
  if Accept and not (State = dsDragLeave) {and (_Sender is TListView)}then
//    if (Sender <> Source) or ((Sender = Source) and True) then // (FDragIndex<TListView(Sender).ItemIndex) or (FDragIndex>TListView(Sender).ItemIndex+1) then
  //  if (FDragIndex<TListView(Sender).ItemIndex) or (FDragIndex>TListView(Sender).ItemIndex+1) then
      begin
        FDragInfo.Color:= clHighlight;//Black;
        FSelectRect.Show(FDragInfo.Rect, FDragInfo.Color, FDragInfo.Align, FDragInfo.Control);
      end
  else
      begin
        FSelectRect.Hide;
      end;
end;

procedure TCalendarForm.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if DragInfoTarget(Sender, Source, X, Y, FDragInfo) then
    begin
      inherited;
      Calendar.CurrentDate:= FDragInfo.XValue;
      if varIsEmpty(FDragInfo.IValue) then
        DataSetMeta.Cache.Update(mcInsert, unassigned)
      else
        DataSetMeta.Cache.Update(mcUpdate, unassigned);

      if Sender = Source then Calendar.RefreshContent
                         else ReCreateView;
    end;
end;

procedure TCalendarForm.ListViewAdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var FColor: TColor;
begin
  DefaultDraw := True;
  //DOWN: Сделана дополнительная проверка, иногда была ошибка

  FColor:= TCacheItem(Item.Data).FontColor;
  if Variables.AsBoolean[RegGridBrush] then
    if ColorToRGB(FColor) = clBlack then
      TListView(Sender).Canvas.Brush.Color:= TListView(Sender).Brush.Color
    else
      TListView(Sender).Canvas.Brush.Color:= FColor
  else
      TListView(Sender).Canvas.Font.Color:= FColor;

 if (-1 < FIndexDField) and Not (FValueDField = unassigned) then
   if TCacheItem(Item.Data).FieldValue[FIndexDField] = FValueDField then
     TListView(Sender).Canvas.Font.Style:= [fsBold];
end;

procedure TCalendarForm.ListViewCustomDraw(Sender: TCustomListView;
  const ARect: TRect; var DefaultDraw: Boolean);
begin
  TListView(Sender).GridLines := Variables.AsBoolean[RegGridLines];
end;

procedure TCalendarForm.act2_Da_DefaultExecute(Sender: TObject);
var RowCount,ColCount : string;
begin
  act2_Da_Default.Checked := True;
  if DataSetMeta=nil then Exit;

  RowCount := ViewParams.GetByName('CalendarROW').Value;
  ColCount := ViewParams.GetByName('CalendarCOL').Value;

  Calendar.SetParams(cvDays, StrToIntDef(RowCount,4),StrToIntDef(ColCount,7));
  FPrevViewType:=FViewType;
  FViewType:=4;
end;

procedure TCalendarForm.DoSplit(Sender: TObject);
begin
  if Sender is TMetaAction then
    FieldX:= TMetaAction(Sender).FieldMeta;

  (Sender as TMetaAction).Checked:=True;
  Calendar.RefreshContent;
end;

procedure TCalendarForm.CalendarDblClickCaption(Sender: TObject);
begin
  inherited;
  if FViewType<>integer(cvDays) then
      PostMessage(Handle,WM_COMMAND,ItemByDays.Command, Handle)
  else
    case FPrevViewType of
      0: PostMessage(Handle,WM_COMMAND,ItemByWeeks.Command, Handle);
      2: PostMessage(Handle,WM_COMMAND,ItemByMonths.Command, Handle);
      4: PostMessage(Handle,WM_COMMAND,ItemByUser.Command, Handle);
     -1: PostMessage(Handle,WM_COMMAND,ItemByWeeks.Command, Handle);
    end;
end;

function TCalendarForm.GetStatusText : string;
var TLV : TListView;
begin
  if (Calendar.SelectedWinControl <> nil) and
     (Calendar.SelectedWinControl is TListView) then
    begin
      TLV:=TListView(Calendar.SelectedWinControl);

      if (TLV.Selected=nil) or (TLV.Items.Count=0) then
        result:= Format('-/%d',[TLV.Items.Count])
      else
        result:= Format('%d/%d',[TLV.Selected.Index+1, TLV.Items.Count]);
    end;
end;

procedure TCalendarForm.WMCommand(var Msg: TWMCommand);
var
  i:integer;
begin
  with Item_Da_View do
    for i:=0 to Count-1 do
      if Items[i].Command=Msg.ItemID then
        Items[i].Action.Execute;
  Inherited;
end;

function TCalendarForm.GetPropertiesString: string;
begin
  Result := inherited GetPropertiesString;
  if assigned(FieldX) then
    Result:=Result+' CalendarField='+FieldX.Original+';';

  case Calendar.ViewType of
    cvMonths: Result:=Result+' CalendarView=Month;';
//   cvWeeks: Result:=Result+' CalendarView=Week;';
      cvDays: Result:=Result+' CalendarView=Day;';
     cvCells: Result:=Result+' CalendarCol='+IntToStr(Calendar.ColCount)+';'+
                             ' CalendarRow='+IntToStr(Calendar.RowCount)+';';
  end;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('CalendarFormUnit unit initialization ...');
  {$ENDIF}
  RegisterClass(TCalendarForm);
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('CalendarFormUnit unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

