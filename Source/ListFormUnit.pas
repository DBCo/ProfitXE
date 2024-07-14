unit ListFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Contnrs, Actions, Controls, ExtCtrls, Forms, ActnList, ComCtrls,
  System.Generics.Collections,
  Types, CommCtrl, DB, Menus, StdCtrls, Buttons, ToolWin, Graphics, Dialogs,
  DeTypes, BaseGridFormUnit, DataCacheUnit, DeMeta, DSMeta, Vcl.ImgList;

type
  TArray20uint = array[0..20] of UINT;
  PArray20uint = ^TArray20uint;

  TSplitGroup = Class(TObject)
  public
    GId: Integer;
    DBKey: Variant;
    DBIndex: Integer;
    GCount: Integer;
    constructor Create(aOwner: TObjectList<TSplitGroup>; aGID: Integer; aDBKey: Variant; aDBIndex: Integer);
    procedure IncCount;
  end;

  TEmptyListItem = class(TListItem)
  end;

  TDeListItem = class(TListItem)
  private
    FCacheItem: TCacheItem;
  public
    property CacheItem: TCacheItem read FCacheItem write FCacheItem;
  end;

  TListForm = class(TBaseGridForm)
    act_Da_IconsBig: TAction;
    act_Da_IconsSmall: TAction;
    act_Da_IconsList: TAction;
    PMIconsBig: TMenuItem;
    PMIconsTileSmall: TMenuItem;
    PMIconsList: TMenuItem;
    act_Da_IconsReport: TAction;
    PMIconsReport: TMenuItem;
    ColumnPopupMenu: TPopupMenu;
    LMIconsLarge: TMenuItem;
    LMIconsTileSmall: TMenuItem;
    LMIconsList: TMenuItem;
    LMIconsReport: TMenuItem;
    ListView: TListView;
    act_Da_ChooseCols: TAction;
    act_Da_ColumnSortUp: TAction;
    act_Da_ColumnSortDown: TAction;
    act_Da_FitWidth: TAction;
    act_Da_ColumnDelete: TAction;
    MMColumns: TMenuItem;
    MMFitWidth: TMenuItem;
    UpdateTimer: TTimer;
    cpmSortUp: TMenuItem;
    cpmSortDown: TMenuItem;
    N4: TMenuItem;
    cpm_Da_GroupByColumn: TMenuItem;
    N5: TMenuItem;
    cpmDeleteColumn: TMenuItem;
    cpmShowAll: TMenuItem;
    cpmChooseColumns: TMenuItem;
    N6: TMenuItem;
    cpmFitWidth: TMenuItem;
    N7: TMenuItem;
    cpmFormatofcolumn: TMenuItem;
    act_Da_AllColumns: TAction;
    act_Da_SelectColumn: TAction;
    cpmSelectColumn: TMenuItem;
    No1: TMenuItem;
    Count1: TMenuItem;
    N10: TMenuItem;
    Sum1: TMenuItem;
    Max1: TMenuItem;
    Min1: TMenuItem;
    Avg1: TMenuItem;
    Values1: TMenuItem;
    MM_Da_AutoCalc: TMenuItem;
    No2: TMenuItem;
    Count2: TMenuItem;
    N11: TMenuItem;
    Sum2: TMenuItem;
    Max2: TMenuItem;
    Min2: TMenuItem;
    Avg2: TMenuItem;
    Values2: TMenuItem;
    Act_Da_ColSave: TAction;
    Act_Da_ColSaveAll: TAction;
    Act_Da_ColClearAll: TAction;
    Act_Da_ColClear: TAction;
    cpm_Dl_View: TMenuItem;
    ActColumnDaSave1: TMenuItem;
    Restore2: TMenuItem;
    N12: TMenuItem;
    ActColumnDaDefault1: TMenuItem;
    ActColumnDaClear1: TMenuItem;
    N13: TMenuItem;
    PMIconsReportH: TMenuItem;
    PMIconsReportW: TMenuItem;
    act_Da_IconsReportWidth: TAction;
    act_Da_IconsReportHeight: TAction;
    act_Da_IconsLarge: TAction;
    act_Da_IconsListBig: TAction;
    act_Da_IconsNormal: TAction;
    LMIconsReportW: TMenuItem;
    LMIconsReportH: TMenuItem;
    LMIconsListBig: TMenuItem;
    LMIconsBig: TMenuItem;
    LMIconsNormal: TMenuItem;
    PMIconsListBig: TMenuItem;
    N15: TMenuItem;
    PMIconsLarge: TMenuItem;
    PMIconsTile: TMenuItem;
    act_Da_ShowEmpty: TAction;
    Showempty1: TMenuItem;
    N14: TMenuItem;
    Showempty2: TMenuItem;
    N16: TMenuItem;
    act_Da_IconsTile: TAction;
    actDaViewTile1: TMenuItem;
    LMIconsTile: TMenuItem;
    act_Da_IconsTileBig: TAction;
    actDaIconsTileBig1: TMenuItem;
    actDaViewTileBig1: TMenuItem;
    Savetofile1: TMenuItem;
    pnlShowLookUp: TPanel;
    btnShowLookUp: TSpeedButton;
    actDaColumnProperty: TAction;
    act_Da_IconsPreview64: TAction;
    act_Da_IconsPreview128: TAction;
    act_Da_IconsPreview256: TAction;
    actDaIconsPreviewSmall1: TMenuItem;
    actDaIconsPreview1: TMenuItem;
    actDaIconsPreviewBig1: TMenuItem;
    PreviewSmall1: TMenuItem;
    Preview1: TMenuItem;
    PreviewBig1: TMenuItem;
    N19: TMenuItem;
    N20: TMenuItem;
    act_Da_IconsPreview384: TAction;
    act_Da_IconsPreview512: TAction;
    act_Da_IconsPreviewMax: TAction;
    Preview3841: TMenuItem;
    Preview2561: TMenuItem;
    Preview2562: TMenuItem;
    Preview3842: TMenuItem;
    Preview5121: TMenuItem;
    Preview7681: TMenuItem;
    procedure act_Da_IconsExecute(Sender: TObject);
    procedure ColumnPopupMenuPopup(Sender: TObject);
    procedure ListViewInfoTip(Sender: TObject; Item: TListItem; var InfoTip: String);
//    procedure ListViewAdvancedCustomDrawItem(Sender: TCustomListView;
//      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure ColumnsOrderClick(Sender: TObject);
    procedure act_Da_ColumnSortXExecute(aField: TFieldMeta; const aDirection: TDSSortDirection);
    procedure act_Da_ColumnSortUpExecute(Sender: TObject);
    procedure act_Da_ColumnSortDownExecute(Sender: TObject);
    procedure act_Da_AllColumnsExecute(Sender: TObject);
    procedure act_Da_FitWidthExecute(Sender: TObject);
    procedure act_Da_ColumnDeleteExecute(Sender: TObject);

    procedure OnGroupColumnClick(Sender : TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure act_Da_SelectColumnExecute(Sender: TObject);
    procedure lbCaptionClick(Sender: TObject);
    procedure Act_Da_ColClearAllExecute(Sender: TObject);
    procedure Act_Da_ColSaveAllExecute(Sender: TObject);
    procedure Act_Da_ColClearExecute(Sender: TObject);
    procedure Act_Da_ColSaveExecute(Sender: TObject);
    procedure act_Da_ShowEmptyExecute(Sender: TObject);
    procedure ListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure ListViewCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ListViewDblClick(Sender: TObject);
    procedure act_Da_SavetofileExecute(Sender: TObject);
    procedure act_Da_SelectallExecute(Sender: TObject);
    procedure act_Da_InvertselectionExecute(Sender: TObject);
    procedure btnShowLookUpClick(Sender: TObject);
    procedure actDaColumnPropertyExecute(Sender: TObject);
  private
    FVirtualMode: Boolean;
    FStyle: TListViewStyle;
    FSelectedField: TFieldMeta;
    FLastLVHitInfo: tagLVHITTESTINFO;
    FSymbolWidth: Integer;
    ViewUpdateCallCount : integer;
    OnResizeHandler : Boolean;
    FCWKWidths : Double;
    FSumFieldsWidth : integer;
    FOldWidth : integer;
    FOldListViewChange : TLVChangeEvent;
    FOldWndProc : TWndMethod;
    FVisibleLevel : TFieldVisible;
    FUpdateDelay : integer;
    FSortClicking: Boolean;
    FColCount: Integer;
    FColumnList: TColumnList;
    FColumnsArray: array[0..255] of UINT;
    FFieldX: TFieldMeta;
    FSplitGroups: TObjectList<TSplitGroup>;
    procedure NewWndProc(var Message: TMessage);
    procedure ListViewDataHint(Sender: TObject; StartIndex, EndIndex: Integer);
    procedure SetStyle(aStyle: TListViewStyle);
    procedure DoSplit(Sender: TObject);
    procedure BeginViewUpdate(AUpdateItems:Boolean = True);
    procedure EndViewUpdate(AUpdateItems:Boolean = True);

    procedure ListItemChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure doListItemChange;

    procedure ListColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListColumnRightClick(Sender: TObject; Column: TListColumn; Point: TPoint);
    procedure SetVirtualMode(aValue: Boolean);
    procedure SetSplitField(aField: TFieldMeta);
    function GetSplitShowEmpty: Boolean;
    procedure SetSplitShowEmpty(aValue: Boolean);
    function GetSymbolWidth: Integer;
  protected
    function GetStatusText: string; override;
    procedure InitForm; override;
    procedure AfterInit; override;
    procedure CacheSelectedChange(Item : TCacheItem);override;
    procedure CacheFocusedChange(Item : TCacheItem); override;
    function GetCacheItemAt(X, Y: Integer):TCacheItem;override;
    procedure SetDataSetMeta(const Value: TDataSetMeta); override;
    function GridMinHeight : Integer; override;
    procedure SetZOrder(TopMost: Boolean); override;
    procedure SetCanDrag(value: Boolean); override;
    procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    procedure DoDragStart(Sender: TObject; var DragObject: TDragObject); override;
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer); override;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy;override;
    function DragInfoTarget(Sender, Source: TObject; X, Y: Integer; aDragInfo: TDragInfo): Boolean; override;
    procedure ReAlignColumns(const AWidth: Integer);
    procedure ReInitColumns;
    procedure FixColumns;
    procedure ReloadItems;
    procedure ReCreateView; override;
    procedure MakeVisibleItem(Item: TCacheItem); override;
    class function CreateNewForm(AParam : TObject; CloseAction : TCloseAction = caNone): TListForm;reintroduce;
    function GetPropertiesString: string; override;
    procedure DeInitForm; override;
    property Style: TListViewStyle read FStyle write SetStyle;
    property VirtualMode: Boolean read FVirtualMode write SetVirtualMode;
    property SplitField: TFieldMeta read FFieldX write SetSplitField;
    property SplitShowEmpty: Boolean read GetSplitShowEmpty write SetSplitShowEmpty;
  end;

implementation

uses ClipBrd, Math,
     DeLog, Dictionary, Security, Funcs, DataUnit, UnitA, HintForm, DataManager,
     DeMetadata, DeSettings, ItemsOrder, BaseMainFormUnit, DeParser, DeActions;

{$R *.dfm}

{ TListForm }

class function TListForm.CreateNewForm(AParam: TObject;
                                          CloseAction: TCloseAction): TListForm;
begin
  result := TListForm(inherited CreateNewForm('TListForm',AParam,CloseAction));
end;

constructor TListForm.Create(aOwner : TComponent);
begin
  inherited;
  FMainControl := ListView;
  FVirtualMode:= False;
  FVisibleLevel := fvLevel2;
  FColumnList := TColumnList.Create;
  FSplitGroups:= TObjectList<TSplitGroup>.Create;
  FLastLVHitInfo.iItem := -1;
//  Style:= lvsReportWidth;

  FUpdateDelay := 0;
  FOldWndProc := ListView.WindowProc;
  ListView.WindowProc := NewWndProc;
  FOldListViewChange := nil;
  ViewUpdateCallCount := 0;
  OnResizeHandler := False;
  ListView.DoubleBuffered := true;
  Act_Da_ColSaveAll.Visible := UserSession.IsAdmin;
  Act_Da_ColClearAll.Visible := UserSession.IsAdmin;
end;

destructor TListForm.Destroy;
begin
  FOldWndProc := nil;
  OnResizeHandler := False;
  FreeAndNil(FColumnList);
  FSplitGroups.Free;
  inherited;
end;

procedure TListForm.BeginViewUpdate;
begin
   pnlShowLookUp.Visible:= False;
   FSelectRect.Hide;

   if ViewUpdateCallCount = 0 then
   begin
     SendMessage(Application.MainForm.Handle, DM_STATUSNOTIFY, null, Handle );
     ListView.OnChange := nil;
     OnResizeHandler := False;
 //    ListView.Columns.BeginUpdate;
     if AUpdateItems then
     begin
       ListView.Items.BeginUpdate;
     end;
   end;
  Inc(ViewUpdateCallCount);
end;

procedure TListForm.EndViewUpdate;
var i: Integer;
    Header: HWND;
    Item: THDItem;
    SortInfo: TSortInfo;
begin
  Dec(ViewUpdateCallCount);
  if (ViewUpdateCallCount = 0) and (ListView.HandleAllocated) then
  begin
    ListView.OnChange := ListItemChange;
    OnResizeHandler := True;
//    ListView.Columns.EndUpdate;
    if AUpdateItems then
    begin
      ListView.Items.EndUpdate;
      ListView.Update;
    end;
    SendMessage(Application.MainForm.Handle, DM_STATUSNOTIFY, NativeInt(StatusText), Handle);

    if Style in lvsHeaderStyles then
      begin
        // подсвечиваем выделенный столбец
        if Assigned(FSelectedField) then
          for i:=0 to Pred(ListView.Columns.Count) do
            if DataSetMeta.Cache.Fields[FColumnList[i].TextIndex] = FSelectedField then
              begin
                SendMessage(ListView.Handle, LVM_SETSELECTEDCOLUMN, i, 0);
                Break;
              end;

        // вешаем значок сортировки на столбцах
        Header := ListView_GetHeader(ListView.Handle);
        ZeroMemory(@Item, SizeOf(Item));
        Item.Mask := HDI_FORMAT;

        for i:=0 to Pred(ListView.Columns.Count) do
          begin
            SortInfo := DataSetMeta.Cache.SortList.ItemByID(DataSetMeta.Cache.Fields[FColumnList[i].TextIndex].ID);

            Header_GetItem(Header, i, Item);
            Item.fmt := (Item.fmt and not (HDF_SORTUP or HDF_SORTDOWN));
            if Assigned(SortInfo) then
              case SortInfo.Direction of
                sdAscending: Item.fmt := Item.fmt or HDF_SORTUP;
                sdDescending: Item.fmt := Item.fmt or HDF_SORTDOWN;
              end;
            Header_SetItem(Header, i, Item);
          end;
      end;
  end;
  if ViewUpdateCallCount<0 then  ViewUpdateCallCount := 0;
end;

procedure TListForm.ReAlignColumns(const AWidth: Integer);
var
 i: integer;
 SumColWidth : integer;
 AClientWidth : Integer;
 FM : TFieldMeta;
begin
  if not ListView.HandleAllocated then Exit;

  if (not(FStyle in lvsResizeStyles)) or (ListView.Columns.Count = 0) then Exit;

  BeginViewUpdate;

  if Style in lvsResizeStyles then
    try
      AClientWidth := AWidth - GetSystemMetrics(SM_CXVSCROLL) ;

      SumColWidth := 0;
      FCWKWidths:=(AClientWidth- ListViewStyleSize[FStyle])/FSumFieldsWidth;

      for i:=0 to ListView.Columns.Count - 2 do
      begin
        FM:= FColumnList[i].Field;
        if FM.IsLookup then
          FM:=FM.LookupPair;

        if i=0 then
          ListView.Columns[i].Width := Max(Trunc( FM.Width * FCWKWidths), 1) + ListViewStyleSize[FStyle]
        else
          ListView.Columns[i].Width := Max(Trunc( FM.Width * FCWKWidths), 1);

        Inc(SumColWidth, ListView.Columns[i].Width);
      end;
      ListView.Columns[ListView.Columns.Count-1].Width :=AClientWidth - SumColWidth;
    finally
    end;

  if Style in [lvsReport] then
    try
      AClientWidth := AWidth - GetSystemMetrics(SM_CXVSCROLL);

      for i:=0 to ListView.Columns.Count - 1 do
      begin
        FM:= FColumnList[i].Field;
        if FM.IsLookup then
          FM:=FM.LookupPair;

        if i=0 then
          ListView.Columns[i].Width := Min( GetSymbolWidth * (FM.Width+2) + ListViewStyleSize[FStyle], AClientWidth)
        else
          ListView.Columns[i].Width := Min( GetSymbolWidth * (FM.Width+2) , AClientWidth);
      end;
    finally
    end;

  EndViewUpdate;
end;

procedure TListForm.ReInitColumns;
var
  i: integer;
  CaptionFM, FM : TFieldMeta;
begin
  FixColumns;

  for i:= low(FColumnsArray) to High(FColumnsArray) do
    FColumnsArray[i]:=i;

  BeginViewUpdate;
  FColCount := 0;
  FSumFieldsWidth:= 0;
  FColumnList.Clear;

  if Style in lvsTilesStyles then
    begin
      CaptionFM:= DataSetMeta.Table.NField;

      for i:= 0 to Pred(DataSetMeta.Cache.Fields.Count) do
        if CaptionFM.ID = DataSetMeta.Cache.Fields[i].ID then
          begin
            FColumnList.Add(DataSetMeta.Cache.Fields, i);
            Inc(FColCount);
            Break;
          end;

      for i:= 0 to Pred(DataSetMeta.Cache.Fields.Count) do
        if (DataSetMeta.Cache.Fields[i].VisibleLevel = fvLevel3) and (not DataSetMeta.Cache.Fields[i].IsLookup) and
           (DataSetMeta.Cache.Fields[i].PolicyShow) and (DataSetMeta.Cache.Fields[i].ID <> CaptionFM.ID) then
          begin
            FColumnList.Add(DataSetMeta.Cache.Fields, i);
            Inc(FColCount);
            if FColCount=20 then Break; // максимальное количество полей отображаемых в тайле
          end;

      if (FColCount<20) and (Style = lvsTileBig) then
      for i:= 0 to Pred(DataSetMeta.Cache.Fields.Count) do
        if (DataSetMeta.Cache.Fields[i].VisibleLevel = fvLevel2) and (not DataSetMeta.Cache.Fields[i].IsLookup) and
           (DataSetMeta.Cache.Fields[i].PolicyShow) and (DataSetMeta.Cache.Fields[i].ID <> CaptionFM.ID) then
          begin
            FColumnList.Add(DataSetMeta.Cache.Fields, i);
            Inc(FColCount);
            if FColCount=20 then Break; // максимальное количество полей отображаемых в тайле
          end;
    end else

  if Style in lvsReportStyles then
    begin
      for i := 0 to Pred(DataSetMeta.Cache.Fields.Count) do
        begin
          FM := DataSetMeta.Cache.Fields[i];
      //  if not(FM.PolicyShow) then Continue;  // проверяют в Get***VisibleLevel
          if (FM.IsLookup) then Continue;
          if (FM.VisibleLevel < FVisibleLevel) then Continue;
          if (DataSetMeta.Role = drParent) and (FM.VisibleLevel < fvLevel3) then Continue;
          if assigned(FieldX) and (FM.ID = FieldX.ID) then Continue;  // скрываем столбец, который определен группой
       // скрываем столбец, значение который определен фильтром-связью
       {  ColSkip := -1;
          for j := 0 to DataSetMeta.OwnerLinks.Count-1 do
            if (DataSetMeta.OwnerLinks[j].DataSetMeta.Table.ID = FM.Link) and Assigned(DataSetMeta.OwnerLinks[j].LinkField) then
              if (DataSetMeta.OwnerLinks[j].LinkField.ID = FM.ID) then
                begin
                  ColSkip:=j;
                  Break;
                end;
          if (-1<ColSkip) then Continue;  {}
          FColumnList.Add(DataSetMeta.Cache.Fields, i);
        end;
    end else

    begin
      for i := 0 to Pred(DataSetMeta.Cache.Fields.Count) do
        begin
          if DataSetMeta.Cache.Fields[i] = DataSetMeta.Table.NField then
            FColumnList.Add(DataSetMeta.Cache.Fields, i);
        end;
    end;

  FColumnList.SortByOrder;
  FColCount:= FColumnList.Count;
  FSumFieldsWidth:= FColumnList.FullWidth;

  ListView.Columns.Clear;
//  while ListView.Columns.Count > FColCount do ListView.Columns.Delete(0);

  for i := 0 to Pred(FColCount) do
    begin
      if ListView.Columns.Count <= i then ListView.Columns.Add;
      ListView.Columns[i].Caption:= FColumnList[i].Field.Native;
      ListView.Columns[i].Alignment:= DataSetMeta.Cache.Fields[FColumnList[i].TextIndex].Alignment;
    end;

  if Visible and (Style in lvsReportStyles) then ReAlignColumns(ListView.Width);

  EndViewUpdate;
end;

procedure TListForm.FixColumns;
var O,BackOrder: Array of Integer;
    i: Integer;
begin
  if Not (Style in lvsHeaderStyles) then Exit;
  if ListView.Columns.count = 0 then Exit;

  SetLength(BackOrder, ListView.Columns.count);
  for i := 0 to Pred(ListView.Columns.count) do
    BackOrder[i]:= DataSetMeta.Cache.Fields[FColumnList[i].ValueIndex].Order;

  SetLength(O, ListView.Columns.count);
  Header_GetOrderArray(ListView_GetHeader(ListView.Handle), ListView.Columns.count, PInteger(@O[0]));
  for i := 0 to Pred(ListView.Columns.count) do
    DataSetMeta.Cache.Fields[FColumnList[O[i]].ValueIndex].Order:= BackOrder[i];
end;

procedure TListForm.ReloadItems;
var r : TRect;
    v, vGroup : Variant;
    LI: TListItem;
    aLVItem: TLVItem;
    aLVGroup: TLVGROUP;
    i, j, p, iGroup, iIndex, iSplit, TopItm, CurItm, DivItm : Integer;
    FCache: TDataCache;
    sEmpty: String;
    icoEmpty: Integer;
begin
  //DONE -oRA: Определяем смещение для последующей прокрутки скроллинга
  CurItm := ListView_GetNextItem(ListView.Handle, -1, LVNI_SELECTED);
  TopItm := ListView_GetTopIndex(ListView.Handle);

  if (0<=CurItm) and (CurItm<CacheItems.Count) then
    begin
      v:= CacheItems[CurItm].ID;
      DivItm  := CurItm-TopItm;
      ListView_GetItemRect(ListView.Handle, TopItm, r, LVIR_BOUNDS);
    end
  else
    begin
      v:=null;
      DivItm := 0;
    end;

  if Assigned(SplitField) and assigned(SplitField.LinkTable) then
    begin
      VirtualMode:= False;
      ListView.Items.BeginUpdate;
      ListView.Items.Clear;
      iSplit:= CacheItems.Fields.IndexByName(SplitField.Original);

      FSplitGroups.Clear;
      ListView_RemoveAllGroups(ListView.Handle);

      FCache:= TDataCache.Create(SplitField.LinkTable);
      FCache.SortList.Add(SplitField.LinkTable.NField.ID);
      FCache.Fields.FindByID(SplitField.LinkTable.NField.ID).Stage:=fsKey;
      FCache.PrepareData(True, fsKey);

      SendMessage(ListView.Handle, LVM_DELETEALLITEMS, 0, 0);
      iGroup:= 0;
      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, 2*FCache.Count + CacheItems.Count, FCache.Count);

      // создаем группы
      for i:= 0 to Pred(FCache.Count) do
        begin
          TSplitGroup.Create(FSplitGroups, i, FCache[i].ID, i);

          aLVGroup.cbSize:= SizeOf(aLVGroup);
          aLVGroup.mask := LVGF_GROUPID;
          aLVGroup.iGroupID:= i;
          SendMessage(ListView.Handle, LVM_INSERTGROUP, WPARAM(i), LPARAM(@aLVGroup));

          SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, I, 0);
        end;

      SendMessage(ListView.Handle, LVM_ENABLEGROUPVIEW, 1, 0);

      // создаем и раскладываем по группам реальные элементы
      if 0<=iSplit then
        for i:= 0 to Pred(CacheItems.Count) do
          begin
            DataSetmeta.Cache.FillItems(i,i);
            LI:= TDeListItem.Create(ListView.Items);
            TDeListItem(LI).CacheItem:= CacheItems[i];

            iGroup:= -1;
            iIndex:= 0;
            vGroup:= CacheItems[i].FieldValue[iSplit];

            for j := 0 to Pred(FSplitGroups.Count) do
              if FSplitGroups[j].DBKey = vGroup then
                begin
                  iGroup:= FSplitGroups[j].GId;
                  FSplitGroups[j].IncCount;
                  iIndex:= FSplitGroups[j].GCount;
                  Break;
                end;

           { function TListItems.CreateItem(Index: Integer; ListItem: TListItem): TLVItem; }
           { begin }
            aLVItem.mask := LVIF_PARAM or LVIF_IMAGE or LVIF_GROUPID or LVIF_TEXT or LVIF_COLUMNS{ or LVIF_STATE  or LVIF_COLFMT};

            // выделить некоторые строчки TListView бледным (аналогично Ctrl+X в проводнике
            aLVItem.mask := LVIF_STATE or aLVItem.mask;
            aLVItem.stateMask := LVIS_CUT;
            if CacheItems[i].Deleted then aLVItem.state:= LVIS_CUT
                                     else aLVItem.state:= 0;

            CacheItems[i].Data:= Pointer(iIndex);
            aLVItem.iItem := i;
            aLVItem.iSubItem := 0;
            aLVItem.iImage := I_IMAGECALLBACK;
            aLVItem.pszText:= LPSTR_TEXTCALLBACK;
            aLVItem.iGroupId := iGroup;
            aLVItem.cColumns:= FColCount-1;
            aLVItem.puColumns:= @FColumnsArray[0];
            {$IFDEF CLR}
            aLVItem.lParam := DLI.GetHashCode;
            {$ELSE}
            aLVItem.lParam := LPARAM(LI);
            {$ENDIF}
           { end; }

            SendMessage(ListView.Handle, LVM_INSERTITEM, 0, LPARAM(@aLVItem));
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, FCache.Count + I, 0);
          end;

      // добавляем виртуальные элементы в пустые группы
      if SplitShowEmpty then
        begin
          i:= CacheItems.Count;
          sEmpty:= GetTitle('_dA.emptylist');
          icoEmpty:= DM.MapIconIndex(0);
          for j:= 0 to Pred(FSplitGroups.Count) do
            if FSplitGroups[j].GCount<=0 then
              begin
                LI:= TEmptyListItem.Create(ListView.Items);

               { function TListItems.CreateItem(Index: Integer; ListItem: TListItem): TLVItem; }
               { begin }
                aLVItem.mask := LVIF_PARAM or LVIF_IMAGE or LVIF_GROUPID or LVIF_NORECOMPUTE;
                aLVItem.iItem := i;
                aLVItem.iSubItem := 0;
                aLVItem.iImage := icoEmpty;
                aLVItem.pszText := PWideChar(sEmpty);
                aLVItem.iGroupId := FSplitGroups[j].GId;

                {$IFDEF CLR}
                aLVItem.lParam := DLI.GetHashCode;
                {$ELSE}
                aLVItem.lParam := LPARAM(LI);
                {$ENDIF}
               { end; }
                SendMessage(ListView.Handle, LVM_INSERTITEM, 0, LPARAM(@aLVItem));
              end;
        end
      else
        for j:= Pred(FSplitGroups.Count) downto 0 do
          if FSplitGroups[j].GCount=0 then
            begin
              SendMessage(ListView.Handle, LVM_REMOVEGROUP, FSplitGroups[j].GId, 0);
              FSplitGroups.Delete(j);
            end;

      // назначаем в ранее созданных группах свойства и заголовок
      for i:= 0 to Pred(FSplitGroups.Count) do
        with FSplitGroups[i] do
          begin
            FillChar(aLVGroup, SizeOf(aLVGroup), 0);
            aLVGroup.cbSize:= SizeOf(aLVGroup);
            aLVGroup.mask:= LVGF_HEADER or LVGF_STATE;
            aLVGroup.pszHeader:= PWChar( FCache.Items[DBIndex].ItemTitle + '    ('+IntToStr(GCount)+') ' );
            aLVGroup.cchHeader:= Length(aLVGroup.pszHeader);
            aLVGroup.iGroupId:= GId;

            aLVGroup.statemask:= LVGS_NORMAL or LVGS_COLLAPSIBLE or LVGS_COLLAPSED;
            if (0 < GCount)
              then aLVGroup.state:= LVGS_NORMAL or LVGS_COLLAPSIBLE
              else aLVGroup.state:= LVGS_NORMAL or LVGS_COLLAPSED;

            SendMessage(ListView.Handle, LVM_SETGROUPINFO, GId, LPARAM(@aLVGroup));
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, FCache.Count + CacheItems.Count + I, 0);
          end;

      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);

      ListView.Items.EndUpdate;

      FCache.Free;
    end
  else
    begin
      SendMessage(ListView.Handle, LVM_ENABLEGROUPVIEW, 0, 0);

      if 0 < FSplitGroups.Count then
        begin
          FSplitGroups.Clear;
          ListView_RemoveAllGroups(ListView.Handle);
        end;

      if VirtualMode then
        begin
          ListView.Items.Count := CacheItems.Count;
          Style:= FStyle; // требуется для пересчета под размер экрана в некоторых режимах
        end
      else
        begin
          VirtualMode:= True;
          // Style:= FStyle; устанавливается внутри VirtualMode
          ListView.Items.Count := CacheItems.Count;
        end;
    end;

  for i := 0 to Pred(CacheItems.SelectedCount) do
    CacheSelectedChange(CacheItems.SelectedItems[i]);
  CacheFocusedChange(CacheItems.FocusedItem);

  // подкручиваем скроллинг окна просмотра
  if v<>null then
    begin
      CurItm := CacheItems.IndexByID(v);
      if 0<=CurItm then
        begin
          TopItm:=CurItm-DivItm;
          if -1<CurItm then
          if -1<TopItm then
          if TopItm<ListView.Items.Count then
            begin
              if ListView.ViewStyle=vsList then
                begin
                  p:=ListView.Items.Item[TopItm].Position.x-r.Left;
                  ListView.Scroll(p, 0);
                end
              else
                begin
                  p:=ListView.Items.Item[TopItm].Position.y-r.Top;
                  // это помогает, но появляется мерцание
                  SendMessage(ListView.Handle, WM_SETREDRAW, 1, 0);
                  //TODO: не работает на прокрутку за половину высоты контрола ???
                  ListView.Scroll(0, p);
                end;
            end;
        end;
    end;
end;

procedure TListForm.InitForm;
{$J+}
const  FirstInit : Boolean = True;
{$J-}
var lvs: TListViewStyle;
    i: Integer;
begin
  inherited;
  if not Assigned(DataSetMeta) then  Exit;

  for i := Pred(TempActionList.ActionCount) downto 0 do
    if SameText(TempActionList[i].Category, CategorySplit) then
      TempActionList[i].Free;

  TMetaAction.Create(self, CategorySplit, nil, DoSplit, [Item_Da_Splitby, MM_Da_Splitby]).ActionList:= TempActionList;
  for i:=0 to DataSetMeta.Table.Fields.Count-1 do
    if (DataSetMeta.Table.Fields[i].PolicySelect) then
      if assigned(DataSetMeta.Table.Fields[i].LinkTable) then
        if not (DataSetMeta.Table.Fields[i].IsLookup) then
          TMetaAction.Create(self, CategorySplit, DataSetMeta.Table.Fields[i], DoSplit, [Item_Da_Splitby, MM_Da_Splitby]).ActionList:= TempActionList;

  ListView.MultiSelect := (DataSetMeta.UsageType = utForm);

  if FirstInit then
  begin
     FOldListViewChange := ListView.OnChange;
     FirstInit := False;
  end;

  FStyle:= lvsReportWidth; // дефолтный стиль
  for lvs:= Low(TListViewStyle) to High(TListViewStyle) do
    if ViewParams.TestByName(cListStyleParam, ListViewStyleNames[lvs]) then
      begin
        if lvs = lvsList then    FStyle:= lvsIconSmall else
        if lvs = lvsListBig then FStyle:= lvsIcon else
                                 FStyle:= lvs;
        Break;
      end;

  SplitShowEmpty:= ViewParams.TestByName(cListSplitEmpty, '1');
  SplitField:= DataSetMeta.Table.Fields.FindByName(ViewParams.GetValueByName(cListSplitField));

  act_Da_IconsPreview64.Visible:= Assigned(DataSetMeta.Table.PreviewField);
  act_Da_IconsPreview128.Visible:= Assigned(DataSetMeta.Table.PreviewField);
  act_Da_IconsPreview256.Visible:= Assigned(DataSetMeta.Table.PreviewField);
  act_Da_IconsPreview384.Visible:= Assigned(DataSetMeta.Table.PreviewField);
  act_Da_IconsPreview512.Visible:= Assigned(DataSetMeta.Table.PreviewField);
  act_Da_IconsPreviewmax.Visible:= Assigned(DataSetMeta.Table.PreviewField);
end;

procedure TListForm.AfterInit;
var i, iSplit: Integer;
    vGroup: Variant;
    f: string;
    ii: TAgregateType;
begin
  inherited;
  SetStyle(FStyle);
  if Not assigned(DatasetMeta) then Exit;

  f:= ViewParams.GetValueByName(cListAgregateFunc);
  for ii:= Low(TAgregateType) to High(TAgregateType) do
    if SameText(TAgregateTypeID[ii], f) then
      begin
        MetaData.AgregateType:= ii;
        Break;
      end;

  FSelectedField:= DatasetMeta.Cache.Fields.FindByName(ViewParams.GetValueByName(cListAgregateField));

  if assigned(DatasetMeta.Cache.FocusedItem) or (0 = DatasetMeta.Cache.Count) then Exit;

  // позиционируем на первую запись с учетом/без учета Split
  if assigned(SplitField) then
    begin
      vGroup:= Null;

      for i:=0 to Pred(FSplitGroups.Count) do
        if 0<FSplitGroups[i].GCount then
          begin
            vGroup:= FSplitGroups[i].DBKey;
            Break;
          end;

      if Not VarIsNull(vGroup) then
        begin
          iSplit:= DatasetMeta.Cache.Fields.IndexByName(SplitField.Original);
          i:= DatasetMeta.Cache.IndexByValues(iSplit, vGroup);
          if (-1 < i) and (i < ListView.Items.Count) then
              begin
                ListView.ItemFocused:= ListView.Items[i];
                ListView.Items[i].Selected:=True;
              end;
          end;
    end
  else
    begin
      ListView.ItemFocused:= ListView.Items[0];
      ListView.Items[0].Selected:=True;
    end;
end;

procedure TListForm.DeInitForm;
begin
  ListView.WindowProc:= FOldWndProc;
  ListView.OnDataHint := nil;

  BeginViewUpdate;
  if ListView.HandleAllocated then
    begin
      SendMessage(ListView.Handle, LVM_DELETEALLITEMS, 0, 0);
      SendMessage(ListView.Handle, LVM_REMOVEALLGROUPS, 0, 0);
    end;

  inherited DeInitForm;
  FSplitGroups.Clear;
end;

function TListForm.GetStatusText : string;
var  i,F,N     : Integer;
     T,S       : String;
     DT        : TFieldType;
     CI        : TCacheItem;
     v         : Variant;
     d,dMax,dMin,dSum : Double;
     isValue          : Boolean;
     dCount : Integer;
     aLVItem: TLVItem;
begin
  if Not Assigned(DataSetMeta) then Exit(EmptyStr);

  if not Assigned(ListView.ItemFocused) and not assigned(ListView.Selected) then
    if VirtualMode then result := Format('-/%d',[ DataSetMeta.Cache.Count ])
                   else result := Format('-/%d',[ FSplitGroups.Count ])
  else
    begin
      N:= 0;
      T:= EmptyStr;

      //TODO -o2010: Перенести обработку типа данных в обработчик кеша

      case MetaData.AgregateType of
        //............................................ без учета типа данных ...
        atNo:;
        atCount:
          begin
            if VirtualMode then N:= ListView.SelCount                    // работает on-line
                           else N:= DataSetmeta.Cache.SelectedCount;     // работает с задержкой
            T:=' ['+GetTitle('_Da.AgrCount')+Format(' = %d',[N])+']';
          end;
        atValue:
          if Not Assigned(FSelectedField) then
            begin
              T:=' [ ]';
            end
          else
            begin
              F:=DataSetMeta.Cache.Fields.IndexOf(FSelectedField);
              for i:=0 to ListView.Items.Count-1 do
                if ListView.Items[i].Selected then
                  begin
                    if VirtualMode then CI:= CacheItems.Items[i]
                                   else CI:= TCacheItem(ListView.Items[i].Data);
                  if assigned(CI) and (0 < Length(CI.FieldText(F))) then Inc(N);
                  end;
              T:=' ['+GetTitle('_Da.AgrValue')+Format(' = %d',[N])+']';
            end;
        //............................................. с учетом типа данных ...
        atMax,atMin,atSum,atAverage:
          if Not Assigned(FSelectedField) then
            begin
              T:=' [ ]';
            end
          else
            begin
              dCount:= 0;
              dSum  := 0;
              dMax  := unassigned;
              dMin  := unassigned;
              d     := unassigned;

              F  := DataSetMeta.Cache.Fields.IndexOf(FSelectedField);
              DT := DataSetMeta.Cache.Fields[F].DataType;

              for i:=0 to ListView.Items.Count-1 do
                if (ListView.Items[i].Selected) or (DataSetmeta.Cache.SelectedCount<2) then
                  begin
                    if VirtualMode then CI:= CacheItems.Items[i]
                                   else CI:= TCacheItem(ListView.Items[i].Data);
                    if Not Assigned(CI) then Continue;

                    V:=CI.FieldBaseValue[F];

                    // получаем значения для разных типов данных
                    if VarIsEmpty(V) or VarIsNull(V) then begin isValue:=False; d:=0; end else
                    if DT in NumericTypes then            begin isValue:=True;  d:=V; end else
                    if DT in StringTypes then             begin isValue:=True;  d:=Length(CI.FieldText(F)); end else
                    if DT in DateTimeTypes then           begin isValue:=True;  d:=V; end else
                    if DT in [ftBlob] then                begin isValue:=True;  d:=Length(V); end else
                                                          begin isValue:=False; d:=0; end;
                    if isValue then
                      begin
                        Inc(dCount);
                        if (dCount = 1) or (d > dMax) then dMax:=d;
                        if (dCount = 1) or (d < dMin) then dMin:=d;
                        dSum := dSum+d;
                      end;
                  end;

              case MetaData.AgregateType of
                atSum     : d:=dSum;
                atMax     : d:=dMax;
                atMin     : d:=dMin;
                atAverage : if dCount>0 then d:=dSum/dCount;
              end;

              if dCount=0 then            T:= '?' else
              if DT in DateTimeTypes then T:= DateToStr(TDate(d)) else
              if DT in StringTypes then   T:= Format('%1.1f',[d]) else
              if DT in IntegerTypes then  T:= Format('%1.1f',[d]) else
              if DT in FloatTypes then
                begin
                  if Length(FSelectedField.Template) > 0
                    then
                      try
                        if Pos('#', FSelectedField.Template) <> 0
                          then T:= FormatFloat(FSelectedField.Template, d)
                          else T:= Format(FSelectedField.Template, [d]);
                      except
                        T:= Format('%3.3f',[d]);
                      end
                    else
                        T:= Format('%3.3f',[d]);
                end else
              if DT in [ftBlob] then      T:= Format('%0.0f',[d]) else
                                          T:= Format('%6.6f',[d]);

              T:=' [' + GetTitle(TAgregateTypeNames[MetaData.AgregateType]) + ' = ' + T +']';
            end;
      end;
      if VirtualMode
        then begin
               if assigned(ListView.ItemFocused)
                 then result := Format('%d/%d',[ListView.ItemFocused.Index+1, DataSetMeta.Cache.Count]) + T
                 else result := Format('-/%d',[DataSetMeta.Cache.Count]) + T
             end
        else begin
               result:= '-/-' + T;

               if Assigned(ListView.ItemFocused) then
                 begin // есть выделенный Item
                   aLVItem.mask := LVIF_GROUPID;
                   aLVItem.iItem:= ListView.ItemFocused.Index;
                   SendMessage(ListView.Handle, LVM_GETITEM, 0, LPARAM(@aLVItem));

                   try
                     s:= IntToStr(Integer(DataSetMeta.Cache.Items[aLVItem.iItem].Data));
                   except
                     s:='-';
                   end;

                   for i:=0 to Pred(FSplitGroups.Count) do
                     if aLVItem.iGroupId = FSplitGroups[i].GId then
                       Exit(Format( '%s/%d %d/%d',[ s, FSplitGroups[i].GCount, succ(i), FSplitGroups.Count]) + T);
                 end

               else // выделена группа, нет выделенного Item
                 begin
                   aLVItem.mask := LVIF_GROUPID;
                   aLVItem.iItem:= ListView.Selected.Index;
                   SendMessage(ListView.Handle, LVM_GETITEM, 0, LPARAM(@aLVItem));

                   for i:=0 to Pred(FSplitGroups.Count) do
                     if aLVItem.iGroupId = FSplitGroups[i].GId then
                       Exit(Format( '-/%d %d/%d',[FSplitGroups[i].GCount, Succ(i), FSplitGroups.Count]) + T);
                 end
             end;
    end;
end;

function TListForm.GetSymbolWidth: Integer;
begin
  if 0 = FSymbolWidth then
    FSymbolWidth:= ListView.Canvas.TextWidth('00')-ListView.Canvas.TextWidth('0');
  Result:= FSymbolWidth;
end;

procedure TListForm.ReCreateView;
var HaveException: Boolean;
begin
  HaveException:= False;

  if not assigned(DataSetMeta) then Exit;

  BeginViewUpdate;

  try
    ReInitColumns;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TListForm.ReCreateView for ReInitColumns error: ' + E.Message);
        {$ENDIF}
        HaveException := True;
      end;
  end;

  if not HaveException then
  try
    ReloadItems;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('TListForm.ReCreateView for ReloadItems error: ' + E.Message);
    {$ENDIF}
  end;

  EndViewUpdate;

  inherited;
end;

procedure TListForm.doListItemChange;
var i,p,n : integer;
    Cache : TDataCache;
begin
  if not Assigned(DataSetMeta) then
    begin
      raise Exception.Create('doListItemChange.DataSetMeta error');
      Exit;
    end;

  Cache := DataSetMeta.Cache;
  if not Assigned(Cache) then
    begin
      raise Exception.Create('doListItemChange.DataSetMeta.Cache error');
      Exit;
    end;

  //установка необходимых признаков в кэше
  if (Cache.Active) then
  begin
    ListView.OnChange := nil;
    Cache.OffSelectedChange := true;

    {быстрая синхронизация без получения элемента ListView, только когда ListView и Cache синхронны }
    if VirtualMode then
      begin
        p:=-1;
        n:= ListView_GetNextItem(ListView.Handle, -1, LVNI_SELECTED);
        while N>-1 do
          begin
            for i:=p+1 to n-1 do
              Cache.ItemUnSelect(i);
            Cache.ItemSelect(n);
            p:=n;
            n:= ListView_GetNextItem(ListView.Handle, n, LVNI_SELECTED);
          end;

        for i:=p+1 to Cache.Count-1 do
          Cache.ItemUnSelect(i);


        n:= ListView_GetNextItem(ListView.Handle, -1, LVNI_FOCUSED);
        if N>-1 then
           Cache.Items[N].SetFocus;
      end
     else
    {медленная синхронизация с получением элемента ListView, когда ListView и Cache асинхронны }
      begin
        for i:=0 to ListView.Items.Count-1 do
          if ListView.Items[i] is TDeListItem then // вероятно лишняя проверка
            if assigned(TDeListItem(ListView.Items[i]).CacheItem) then
              TDeListItem(ListView.Items[i]).CacheItem.Selected := ListView.Items[i].Selected;

        if Assigned(ListView.ItemFocused) and (ListView.ItemFocused is TDeListItem) and
           assigned(TDeListItem(ListView.ItemFocused).CacheItem)
          then TDeListItem(ListView.ItemFocused).CacheItem.SetFocus
          else DataSetMeta.Cache.FocusedItem:= nil;
      end;

    Cache.OffSelectedChange := false;
    ListView.OnChange := ListItemChange;
  end;
end;

procedure TListForm.ListItemChange(Sender: TObject; Item: TListItem; Change: TItemChange);
var n : integer;
begin
  n:= ListView_GetNextItem(ListView.Handle, -1, LVNI_SELECTED);
  if Assigned(Item) and (Change=ctState) and ((-1<n) or (DataSetMeta.Cache.Count=0))then
  begin
    FUpdateDelay := 3;
    SendMessage(Application.MainForm.Handle, DM_STATUSNOTIFY, NativeInt(StatusText), Handle);
    //doListItemChange;
  end;
  (*
  if (not ListView.MultiSelect)and(Assigned(ListView.ItemFocused))and(DataSetMeta.Active) then
  begin
    //ListView.Selected := ListView.ItemFocused;
    ListView.OnChange := nil;
    CacheItems.Items[CacheItems.SelectedItem.ItemIndex].Selected := false;
    CacheItems.Items[ListView.ItemFocused.Index].Focused := True;
    CacheItems.Items[ListView.ItemFocused.Index].Selected := true;
    ListView.OnChange := ListItemChange;
  end;
  //установка необходимых признаков в кэше
  if (ListView.MultiSelect)and(Assigned(ListView.Selected{ItemFocused}))and(Assigned(Item))and(DataSetMeta.Active) then
  begin
    ListView.OnChange := nil;
    CacheItems.OffSelectedChange := true;
    //первые выбранные
    if Assigned(ListView.ItemFocused) then  CacheItems.Items[ListView.ItemFocused.Index].Focused := True;
    CacheItems.Items[ListView.Selected.Index].Selected := True;
    //все остальные
    for i:=0 to ListView.Items.Count-1 do
    begin
      if CacheItems.Items[i].Selected <> ListView.Items[i].Selected then
        CacheItems.Items[i].Selected := ListView.Items[i].Selected;
    end;
    CacheItems.OffSelectedChange := false;

    ListView.OnChange := ListItemChange;
  end;
  *)
  if Assigned(FOldListViewChange) then
    FOldListViewChange(Sender,Item,Change);
end;

procedure TListForm.CacheFocusedChange(Item : TCacheItem);
var N: Integer;
begin
  if Assigned(Item) then
  begin
    ListView.OnChange := nil;
    N:= CacheItems.IndexByItem(Item);
    if -1 < N then
      begin
        ListView.Items[N].Focused := True;
        ListView.Items[N].MakeVisible(True);
        ListView.OnChange := ListItemChange;
      end;
  end;
  inherited;
end;

procedure TListForm.CacheSelectedChange(Item : TCacheItem);
var i: integer;
    CI: TCacheItem;
    CItems: TCacheItemList;
begin
  if Assigned(Item) then
    begin
      ListView.OnChange := nil;

//    i:= CacheItems.IndexByItem(Item);
//    if i>-1 then
//      ListView.Items[i].Selected := Item.Owner.ItemSelected(i);

//    for i:=0 to Item.Owner.Count-1 do
//      ListView.Items[i].Selected := Item.Owner.ItemSelected(i);
      CItems:= Item.Owner.CacheItems;
      for i:=0 to Item.Owner.Count-1 do
        begin
           CI := CItems.Items[I];
           if Assigned(CI) then ListView.Items[i].Selected := CI.Selected
        end;

      ListView.OnChange := ListItemChange;
    end;
  inherited;
end;

function TListForm.GridMinHeight: Integer;
var C,R,xx : Integer;
    DW : DWORD;
    Rect: TRect;
begin
  Result:=Inherited GridMinHeight;
  C:= ListView.Items.Count;

  case ListView.ViewStyle of
    vsIcon :  begin
                DW:=ListView_GetItemSpacing(ListView.Handle, 0);
                xx:= Max(1, ListView.ClientWidth  div LoWord(DW));
                if (C mod xx) = 0 then R:= (C div xx)
                                  else R:= (C div xx) + 1;

                Result := Min(2, R) * HiWord(DW);
              end;
    vsSmallIcon
            : begin
                DW:=ListView_GetItemSpacing(ListView.Handle, 1);
                xx:= Max(1, ListView.ClientWidth  div LoWord(DW));
                if (C mod xx) = 0 then R:= (C div xx)
                                  else R:= (C div xx) + 1;

                Result := Min(7, R) * HiWord(DW);
              end;
    vsList:   begin
                DW:=ListView_GetItemSpacing(ListView.Handle, 1);
                Result := Min(7, C) * HiWord(DW);
              end;
    vsReport: begin
                DW:=ListView_GetItemSpacing(ListView.Handle, 1);
                GetWindowRect(SendMessage(Listview.Handle, LVM_GETHEADER, 0, 0), Rect);
                Result := Min(7, C) * HiWord(DW) + Rect.Height;
              end;
  end;
end;

procedure TListForm.ListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var CI: TCacheItem;
    FColor: TColor;
begin
  CI:= DataSetMeta.Cache.Items[Item.Index];
  FColor:= CI.FontColor;

  if Variables.AsBoolean[RegGridBrush] then
    if ColorToRGB(FColor) = clBlack then
      ListView.Canvas.Brush.Color:= ListView.Brush.Color
    else
      ListView.Canvas.Brush.Color:= FColor
  else
      ListView.Canvas.Font.Color:= FColor;
end;

procedure TListForm.ListViewCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; var DefaultDraw: Boolean);
var CI: TCacheItem;
    FColor: TColor;
begin
  CI:= DataSetMeta.Cache.Items[Item.Index];
  FColor:= CI.FontColor;

  if Variables.AsBoolean[RegGridBrush] then
    if ColorToRGB(FColor) = clBlack then
      ListView.Canvas.Brush.Color:= ListView.Brush.Color
    else
      ListView.Canvas.Brush.Color:= FColor
  else
      ListView.Canvas.Font.Color:= FColor;
end;

procedure TListForm.ListViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var aLVHitInfo: tagLVHITTESTINFO;
  R: TRect;
begin
  inherited;
  aLVHitInfo.pt:=Point(X,Y);
  ListView_SubItemHitTest(ListView.Handle, @aLVHitInfo);
  if (FLastLVHitInfo.iItem <> aLVHitInfo.iItem) or (FLastLVHitInfo.iSubItem <> aLVHitInfo.iSubItem) then
    begin
      if (-1 < aLVHitInfo.iItem) and (-1 < aLVHitInfo.iSubItem) and (0 < FColumnList.Count) then
        begin
          if assigned(FColumnList[aLVHitInfo.iSubItem].Field.LinkTable)
            and Not VarIsNull(DataSetMeta.Cache.Items[aLVHitInfo.iItem].FieldValue[FColumnList[aLVHitInfo.iSubItem].ValueIndex]) then
            begin
              if (aLVHitInfo.iSubItem = 0) and (1 < ListView.Columns.Count) then
                begin
                  ListView_GetSubItemRect(ListView.Handle, aLVHitInfo.iItem, 1, LVIR_BOUNDS, PRect(@R));
                  pnlShowLookUp.SetBounds(R.Left-20-1, R.Top+1, 20, R.Height-2);
                end
              else
                begin
                  ListView_GetSubItemRect(ListView.Handle, aLVHitInfo.iItem, aLVHitInfo.iSubItem, LVIR_BOUNDS, PRect(@R));
                  pnlShowLookUp.SetBounds(R.Right-20-1, R.Top+1, 20, R.Height-2);
                end;
              pnlShowLookUp.Visible:= True;
            end
          else
            begin
              pnlShowLookUp.Visible:= False;
            end;
        end;

      if (-1< FLastLVHitInfo.iItem) and (FLastLVHitInfo.iItem < listView.Items.count) then
        listView.Items[FLastLVHitInfo.iItem].Update;

      if FLastLVHitInfo.iItem <> aLVHitInfo.iItem then
        if (-1< aLVHitInfo.iItem) and (aLVHitInfo.iItem < listView.Items.count) then
          listView.Items[aLVHitInfo.iItem].Update;
    end;

  FLastLVHitInfo:= aLVHitInfo;

  if (aLVHitInfo.iItem<0) or (aLVHitInfo.iSubItem<0) then pnlShowLookUp.Visible:= False;
end;

procedure TListForm.MakeVisibleItem(Item: TCacheItem);
var N: Integer;
begin
  if assigned(Item) then
    begin
      N:= CacheItems.IndexByItem(Item);
      if -1 < N then
        ListView.Items[N].MakeVisible(True);
    end;
end;

procedure TListForm.ListViewDataHint(Sender: TObject; StartIndex, EndIndex: Integer);
begin
  CacheItems.FillItems(StartIndex,EndIndex);
end;

procedure TListForm.ListViewDblClick(Sender: TObject);
var i: Integer;
begin
  inherited;
 // DoListItemChange;   // зачем это делается????

  for i:= Pred(DataSetMeta.UserActionList.ActionCount) downto 0 do
    if TDeActionData(TAction(DataSetMeta.UserActionList[i]).Tag).Active then
      if TAction(DataSetMeta.UserActionList[i]).ShortCut = VK_RETURN then
        begin
          TDeActionData(TAction(DataSetMeta.UserActionList[i]).Tag).Execute;
          Exit;
        end;

  if ViewArea then HideViewArea(True)
              else ShowViewArea(True);
end;

procedure TListForm.btnShowLookUpClick(Sender: TObject);
var v: Variant;
begin
  inherited;

  if (-1 < FLastLVHitInfo.iItem) and (-1 < FLastLVHitInfo.iSubItem) then
    if assigned(FColumnList[FLastLVHitInfo.iSubItem].Field.LinkTable) then
      begin
        v:= DataSetMeta.Cache.Items[FLastLVHitInfo.iItem].FieldValue[FColumnList[FLastLVHitInfo.iSubItem].ValueIndex];
        if not(VarIsNull(v) or VarIsEmpty(v)) then
          begin
            if EditRecord(FColumnList[FLastLVHitInfo.iSubItem].Field.LinkTable, v) then
              DataSetMeta.Cache.Update(mcUpdate, Null);
            Exit;
          end;
      end;
end;

procedure TListForm.act_Da_SavetofileExecute(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.InitialDir := Variables.AsString[RegDirPath];
    SaveDialog.DefaultExt := sExtensionCSV;
    SaveDialog.Filter := GetTitle('_Df.file CSV') + '|*'+sExtensionCSV;
    SaveDialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableIncludeNotify, ofEnableSizing, ofDontAddToRecent];
    SaveDialog.FileName := NormalizeFileName(FormCaption);
    if SaveDialog.Execute then
      DataSetMeta.Cache.SaveToFileCSV(SaveDialog.FileName, FVisibleLevel);
  finally
    SaveDialog.Free;
  end;
end;

procedure TListForm.act_Da_SelectColumnExecute(Sender: TObject);
begin
  FSelectedField:= TFieldMeta((Sender as TAction).Tag);

  BeginViewUpdate;
  Application.MainForm.Perform(DM_STATUSNOTIFY, NativeInt(StatusText), Handle);
  EndViewUpdate;
end;

procedure TListForm.ListColumnClick(Sender: TObject; Column: TListColumn);
var dx : integer;
    FM  : TFieldMeta;
    SortItem : Integer;
    KeyboardState : TKeyboardState;
    O: Array of Integer;
begin
  if FSortClicking then Exit;
  FSortClicking := True;
  {$IFDEF DEBUG}
  DebugLog('TListForm.ListColumnClick start ...');
  {$ENDIF}

  SetLength(O, ListView.Columns.count);
  Header_GetOrderArray(ListView_GetHeader(ListView.Handle), ListView.Columns.count, PInteger(@O[0]));

  FM:= DataSetMeta.Cache.Fields[FColumnList[O[Column.Index]].TextIndex];

  if FM=nil then Exit;
  GetKeyboardState(KeyboardState);

  if AltDown(KeyboardState) then
    begin
      FSelectedField:= FM;
      BeginViewUpdate;
      Application.MainForm.Perform(DM_STATUSNOTIFY, NativeInt(StatusText), Handle);
      EndViewUpdate;
      FSortClicking := False;
      Exit;
    end;

  if ShiftDown(KeyboardState) then
    begin
      SortItem := DataSetMeta.Cache.SortList.IndexByID(FM.ID);
      if -1 < SortItem then
        begin
          if DataSetMeta.Cache.SortList[SortItem].NextDirection = sdNone then
            DataSetMeta.Cache.SortList.Delete(SortItem);
        end
      else
        begin
          DataSetMeta.Cache.SortList.add(FM.ID);
        end;
    end
  else
    if (DataSetMeta.Cache.SortList.Count = 1) and (DataSetMeta.Cache.SortList[0].FieldID = FM.ID) then
      begin
        if DataSetMeta.Cache.SortList[0].NextDirection = sdNone then
          DataSetMeta.Cache.ClearSortList;
      end
    else
      begin
        DataSetMeta.Cache.ClearSortList;
        DataSetMeta.Cache.SortList.Add(FM.ID);
      end;

  dx:=GetScrollPos(ListView.Handle, sb_Horz);
  if dx=0 then
      DataSetMeta.Cache.Update(mcUpdate, Null)
  else
    begin
      LockWindowUpdate(ListView.Handle);
      DataSetMeta.Cache.Update(mcUpdate, Null);
      ListView.Scroll(dx,0);
      LockWindowUpdate(null);
    end;

  {$IFDEF DEBUG}
  DebugLog('TListForm.ListColumnClick finish ...');
  {$ENDIF}
  FSortClicking := False;
end;

procedure TListForm.SetStyle(aStyle: TListViewStyle);
var LVTileViewInfo: tagLVTILEVIEWINFO;
    P, W, H: NativeInt;
begin
  FStyle:= aStyle;
  if not(aStyle in lvsReportStyles) then
    ListView.Columns.Clear;

  act_Da_IconsReport.Checked:= (FStyle = lvsReport);
  act_Da_IconsReportWidth.Checked:= (FStyle = lvsReportWidth) or (FStyle = lvsColumn);
  act_Da_IconsReportHeight.Checked:= (FStyle = lvsReportHeight) or (FStyle = lvsColumnHeight);

  act_Da_IconsSmall.Checked := (FStyle = lvsIconSmall);
  act_Da_IconsNormal.Checked := (FStyle = lvsIcon);
  act_Da_IconsBig.Checked := (FStyle = lvsIconBig);
  act_Da_IconsLarge.Checked := (FStyle = lvsIconLarge);

  act_Da_IconsPreview64.Checked := (FStyle = lvsPreview64);
  act_Da_IconsPreview128.Checked := (FStyle = lvsPreview128);
  act_Da_IconsPreview256.Checked := (FStyle = lvsPreview256);
  act_Da_IconsPreview384.Checked := (FStyle = lvsPreview384);
  act_Da_IconsPreview512.Checked := (FStyle = lvsPreview512);
  act_Da_IconsPreviewmax.Checked := (FStyle = lvsPreviewmax);

  act_Da_IconsList.Checked:= (FStyle = lvsList);
  act_Da_IconsListBig.Checked:= (FStyle = lvsListBig);

  act_Da_IconsTile.Checked:= (FStyle = lvsTile);
  act_Da_IconsTileBig.Checked:= (FStyle = lvsTileBig);

  act_Da_ChooseCols.Visible := (FStyle in lvsReportStyles);
  act_Da_FitWidth.Visible := (FStyle in lvsReportStyles);

  ViewParams.SetByName(cListStyleParam, ListViewStyleNames[FStyle]);

  if ListViewStyleInt[FStyle] = LV_VIEW_TILE then
    begin
      LVTileViewInfo.cbSize:= SizeOf(LVTileViewInfo);
      LVTileViewInfo.dwMask:= LVTVIM_COLUMNS or LVTVIM_TILESIZE or LVTVIM_LABELMARGIN;
      LVTileViewInfo.dwFlags:= LVTVIF_FIXEDSIZE;
      if Fstyle = lvsIconSmall then LVTileViewInfo.cLines:= 1
                               else LVTileViewInfo.cLines:= ListView.Columns.Count;
      LVTileViewInfo.sizeTile.cx := ListViewStyleW[FStyle];
      if FStyle in lvsTilesStyles
        then LVTileViewInfo.sizeTile.cy := 8 + max(ListViewStyleSize[FStyle], 13*ListView.Columns.Count)
        else LVTileViewInfo.sizeTile.cy := 4 +     ListViewStyleSize[FStyle];
      LVTileViewInfo.rcLabelMargin:= Rect(2,2,2,2);
      SendMessage(ListView.Handle, LVM_SETTILEVIEWINFO, 0, LPARAM(@LVTileViewInfo));
    end;

  if FStyle in [lvsPreview64, lvsPreview128, lvsPreview256, lvsPreview384, lvsPreview512 ] then
    begin
      SetThumbNailSize(ListViewStyleSize[FStyle]);
      ListView.SmallImages:= Thumbnail;
    end else

  if FStyle in [lvsPreviewmax ] then
    begin
      // получаем максимально возможные размеры с учетом отступов, ширины скроллинга
      W:= ListView.Width  - (GetSystemMetrics(SM_CXICONSPACING) - GetSystemMetrics(SM_CXICON)) - GetSystemMetrics(SM_CXVSCROLL);
      H:= ListView.Height - (GetSystemMetrics(SM_CYICONSPACING) - GetSystemMetrics(SM_CYICON));
      // сильно маленькие размеры доводим до минимума превью 64
      W:= Max(W, 64);
      H:= Max(H, 64);
      // сильно вытянутые рамеры приводим до соотношения 3:4 или 4:3, т.е. соотношения стандартного фото
      W:= Min(W, Trunc(H * 4 / 3));
      H:= Min(H, Trunc(W * 4 / 3));
      SetThumbNailSize(W, H);
      ListView.SmallImages:= Thumbnail;
    end else

    begin
      SetThumbNailSize(0);
      ListView.SmallImages:= DM.GetEImageList(ListViewStyleSize[FStyle]);
    end;
  ListView.LargeImages := ListView.SmallImages;

  SendMessage( ListView.Handle, LVM_SETVIEW, ListViewStyleInt[FStyle], 0);

  if FStyle in lvsHeaderStyles then
    begin
      P:= LVS_EX_HEADERDRAGDROP;
      if Variables.AsBoolean[RegGridLines] then P:= P or LVS_EX_GRIDLINES;

      SendMessage(ListView.Handle, LVM_SETEXTENDEDLISTVIEWSTYLE, LVS_EX_HEADERDRAGDROP or LVS_EX_GRIDLINES, P);
      ListView.OnColumnClick      := ListColumnClick;
      ListView.OnColumnRightClick := ListColumnRightClick;
    end
 else
    begin
      P:=0;
      SendMessage(ListView.Handle, LVM_SETEXTENDEDLISTVIEWSTYLE, LVS_EX_HEADERDRAGDROP or LVS_EX_GRIDLINES, P);
      ListView.OnColumnClick      := nil;
      ListView.OnColumnRightClick := nil;
    end;

end;

procedure TListForm.DoSplit(Sender: TObject);
var N: Integer;
begin
  if assigned(Sender) then
    if Sender is TMetaAction then
      begin
        SplitField:= TMetaAction(Sender).FieldMeta;

        if assigned(SplitField) then
          begin
            N:=DataSetMeta.Cache.Fields.IndexByID(SplitField.ID);
            if (-1<N) and (fsBase < DataSetMeta.Cache.Fields[N].Stage) then
              begin
                DataSetMeta.Cache.Fields[N].Stage:= fsBase;
                DataSetMeta.Cache.Update(mcUpdate, Null);
              end
           else
              ReCreateView;

            VirtualMode:= False;//True;
          end
        else
          begin
           VirtualMode:= True;//False;
           ReCreateView;
          end;

        TMetaAction(Sender).Checked:= true;
      end;
end;

procedure TListForm.act_Da_ShowEmptyExecute(Sender: TObject);
begin
  ReCreateView;
end;

procedure TListForm.act_Da_IconsExecute(Sender: TObject);
begin
  BeginViewUpdate;

  ListView.Items.Clear;
  SendMessage(ListView.Handle, LVM_DELETEALLITEMS, 0, 0);

  Style:= TListViewStyle(TAction(Sender).Tag);

  ReCreateView;

  EndViewUpdate;
end;

procedure TListForm.NewWndProc(var Message: TMessage);
var FM : TFieldMeta;
    Column : TListColumn;
    i,N,ItemIndex : integer;
    nmlvcd: PNMLVCUSTOMDRAW;
    DispInfo: PLVDispInfo;
    CI: TCacheItem;
    s: String;
    O: array of Integer;
//    TmpItem: TLVItem;
//    SubItem: Boolean;
//    LVCustomDraw: PNMLVCustomDraw;
//    CustomDraw: PNMCustomDraw;
//    LVTileInfo: tagLVTILEINFO;
//    i, LSubItem: Integer;
begin
  if Message.Msg = CN_NOTIFY then
    begin
      case TWMNotify(Message).NMHdr.code of
       LVN_BEGINSCROLL:
         begin
           FSelectRect.Hide;

           pnlShowLookUp.Visible:= False;
           if (-1< FLastLVHitInfo.iItem) then
             begin
               listView.Items[FLastLVHitInfo.iItem].Update;
               FLastLVHitInfo.iItem:= -1;
             end;
         end;
       LVN_ENDSCROLL:
         begin
           if FStyle in [lvsPreview64, lvsPreview128, lvsPreview256, lvsPreview384, lvsPreview512 ] then
             begin

             end
           else
             begin
               ListView.Items.BeginUpdate;
               // без этого скроллинг назад к началу приводит к появлению пустоты перед первой записью
               ListView.Items.EndUpdate;

//             SendMessage(ListView.Handle, WM_SETREDRAW, 0, 0);
//             SendMessage( listView.Handle, WM_SETREDRAW, 1, 0);
             end;
         end;
       LVN_COLUMNDROPDOWN:
         begin
          // sleep(9);
         end;
       LVN_GETDISPINFOA, LVN_GETDISPINFOW:
         begin
          DispInfo := TWMNotifyLV(Message).LVDispInfo;

          if (0 < (DispInfo.item.mask and LVIF_COLUMNS)) then
            begin
              DispInfo.item.cColumns:= FColCount-1;
              for i:=0 to Pred(DispInfo.item.cColumns) do
                PArray20uint(DispInfo.item.puColumns)^[i]:= i+1;
            end;

          if DispInfo.item.iItem < DataSetMeta.Cache.Count then
            begin
              CI:= DataSetMeta.Cache.Items[DispInfo.item.iItem];

              if (0 < (DispInfo.item.mask and LVIF_IMAGE)) and (0 = DispInfo.item.iSubItem ) then
                begin
                  if ListView.SmallImages= Thumbnail
                    then begin
                           PrepareThumbNail(DispInfo.item.iItem);
                           DispInfo.item.iImage:= DispInfo.item.iItem
                         end
                    else DispInfo.item.iImage:= CI.MapImageIndex;
                end;

              if (0 < (DispInfo.item.mask and LVIF_TEXT)) then
                begin
                  if ListView.Columns.Count = 0 then
                    begin
                      s:= PWChar(CI.Caption);
                      DispInfo.item.pszText:= PWChar(CI.Caption);
                    end
                  else
                    begin
                      if Style in lvsTilesStyles then
                        begin
                          N:= DispInfo.item.iSubItem;
                          if (N < FColumnList.Count)
                            then s:= ReduceCaption(CI.FieldText(FColumnList[N].TextIndex), 255)
                            else s:= '<'+IntToStr(N)+'>';

                          if N=0 then
                            begin
                              if (0 = Length(s)) then s:= GetTitle('_Dv.Noname');
                              DispInfo.item.pszText:= PWChar(PWideChar(WideString(s)));
                            end
                          else
                            begin
                              if (0 < Length(s)) then s:= GetTitle(FColumnList[N].Field.Name) + ': ' + s;
                              DispInfo.item.pszText:= PWideChar(s);
                            end;
                        end else

                        begin
                          N:= DispInfo.item.iSubItem;
                          s:= ReduceCaption(CI.FieldText( FColumnList[N].TextIndex), 255 );
                          if (N=0) and (0 = Length(s)) then s:= GetTitle('_Dv.Noname');

                          DispInfo.item.pszText:= PWChar(PWideChar(WideString(s)));
                        end;
                    end;
                end;
            end
          else
            begin
              if (0 < (DispInfo.item.mask and LVIF_TEXT)) then
                DispInfo.item.pszText:= PWChar('Item:'+IntToStr(DispInfo.item.iItem)+'; subitem:'+IntToStr(DispInfo.item.iSubItem));

              if (0 < (DispInfo.item.mask and LVIF_IMAGE))  then
                DispInfo.item.iImage:= DM.MapIconIndex(112);

            end;

          message.Result:= 1;
          exit;
         end;
      end;
  end;

  FOldWndProc(Message);

  with  Message do
  case Msg of   {
    WM_MEASUREITEM:
      begin
            размер строки в листвью
      end; {}
  WM_MOUSEWHEEL:
    if assigned(ListView.OnMouseMove) then
      begin
        {
        FPoint:=ListView.ScreenToClient(Point(TWMMouseWheel(Message).XPos, TWMMouseWheel(Message).YPos));
        ListViewMouseMove(ListView, [], FPoint.X, FPoint.Y);
        {}
      end;
  WM_NOTIFY:
    begin
      if (TWMNotify(Message).NMHdr^.code=HDN_BEGINDRAG)and(OnResizeHandler) then
        begin
          pnlShowLookUp.Visible:= False;

          if pHDNotify(TWMNotify(Message).NMHdr)^.Item = 0 then Result:=Integer(True);
        end;

      if (TWMNotify(Message).NMHdr^.code=HDN_ENDDRAG)and(OnResizeHandler) then
        begin
          ItemIndex:= pHDNotify(TWMNotify(Message).NMHdr)^.Item;
          N:= PHDItemW(pHDNotify(TWMNotify(Message).NMHdr)^.PItem).iOrder;
          // запрещаем перетаскивать первый столбец
          if (N = 0) or (ItemIndex = 0) then Result:= 1;
        end;

      if (TWMNotify(Message).NMHdr^.code = HDN_BEGINTRACK)and(OnResizeHandler) then
        begin
          pnlShowLookUp.Visible:= False;
        end;

      if (TWMNotify(Message).NMHdr^.code = HDN_ENDTRACK)and(OnResizeHandler) then
      begin
        ItemIndex  := pHDNotify(TWMNotify(Message).NMHdr)^.Item;

        SetLength(O, ListView.Columns.count);
        for i:=0 to Pred(ListView.Columns.count) do O[i]:=i;
        Header_GetOrderArray(ListView_GetHeader(ListView.Handle), ListView.Columns.count, PInteger(@O[0]));

        Column:=ListView.Column[ItemIndex];
        FM:=  DataSetMeta.Cache.Fields[FColumnList[O[ItemIndex]].ValueIndex];

        Dec(FSumFieldsWidth, FM.Width);
        if FStyle=lvsReport then FM.Width:=Max(1,Trunc(Column.Width/GetSymbolWidth))
                            else FM.Width:=Max(1,Trunc(Column.Width/FCWKWidths));
        Inc(FSumFieldsWidth, FM.Width);
      end;

      {------------------------------------------------------------------------}
      if PNMLISTVIEW(lParam)^.hdr.code=NM_CUSTOMDRAW then
      begin
       nmlvcd:=PNMLVCUSTOMDRAW(lparam);
         case nmlvcd.nmcd.dwDrawStage of

           CDDS_PREPAINT:
                   begin
                     result:=CDRF_NOTIFYITEMDRAW;
                   end;
{
           CDDS_ITEMPREPAINT:
                   begin
                     N:=nmlvcd.nmcd.dwItemSpec;
                     FieldMeta:=TFieldMeta(ListView.Columns[N].Tag);

                     if Assigned(DataSetMeta) and (DataSetMeta.Cache.SelectList.IndexOf(FieldMeta)>-1) then
                       begin
                         SetTextColor(nmlvcd.nmcd.hdc, clWhite);    // цвет текста
                       end;
                  end;
                     {}
         end;
       end;
      {------------------------------------------------------------------------}
    end;
  WM_CREATE: FOldWidth := ListView.Width;

  WM_WINDOWPOSCHANGED:
      if (OnResizeHandler) and (TWMWindowPosChanging(Message).WindowPos^.flags and SWP_NOSIZE = 0)then
      begin
        if(FOldWidth <> TWMWindowPosChanging(Message).WindowPos^.cx) and (FStyle in lvsResizeStyles) then
          begin
            ReAlignColumns(TWMWindowPosChanging(Message).WindowPos^.cx);
            FOldWidth := TWMWindowPosChanging(Message).WindowPos^.cx;
          end;
     end;
  WM_WINDOWPOSCHANGING :
      if (OnResizeHandler) and (TWMWindowPosChanging(Message).WindowPos^.flags and SWP_NOSIZE = 0) then
      begin
        if(FOldWidth > TWMWindowPosChanging(Message).WindowPos^.cx) and (FStyle in lvsResizeStyles) then
          begin
            ReAlignColumns(TWMWindowPosChanging(Message).WindowPos^.cx);
            FOldWidth := TWMWindowPosChanging(Message).WindowPos^.cx;
          end;
      end
  end;
end;

function TListForm.DragInfoTarget(Sender, Source: TObject; X, Y: Integer; aDragInfo: TDragInfo): Boolean;
var i, TargetItem: Integer;
    H: HWND;
    aLVGroup: TLVGROUP;
    aRect: TRect;
    aLVHitInfo: tagLVHITTESTINFO;
begin
  Result:= inherited;  // Здесь определяются TDragInfo: DragLink, Source, IField, XField, YField, ...

//TargetItem:=SendMessage(ListView.Handle, LVM_GETHOTITEM, 0, 0);  // не всегда овозвращает hit элемент

  aLVHitInfo.pt := Point(X,Y);
  SendMessage(ListView.Handle, LVM_HITTEST, 0, LPARAM(@aLVHitInfo));
  TargetItem:= aLVHitInfo.iItem;
  if (-1 < TargetItem)
    then if VirtualMode then FDragInfo.IValue:= DataSetMeta.Cache[TargetItem].ID
                        else FDragInfo.IValue:= TDeListItem(ListView.Items[TargetItem]).CacheItem.ID
    else FDragInfo.IValue:= Unassigned;

  // перетаскивание пока не началось, мышь над перетаскиваемым элементом
  if (dtSelf in aDragInfo.DragTypes) and not varIsEmpty(FDragInfo.IValue) then
    for i:=0 to Pred(aDragInfo.Source.Cache.SelectedCount) do
      if aDragInfo.Source.Cache.SelectedItems[i].ID = FDragInfo.IValue then
        begin
          aDragInfo.DragTypes:= [];
          Exit(False);
        end;

  if aDragInfo.canItem and (-1 < TargetItem) then
    begin
      aDragInfo.Rect.Left:= LVIR_BOUNDS;
      SendMessage(ListView.Handle, LVM_GETITEMRECT, TargetItem, LPARAM(@aDragInfo.Rect));
      aDragInfo.Align:= alClient;
      aDragInfo.Color:= clActiveCaption;
      Exit(True);   // нашли - уходим
    end;

  if aDragInfo.canXY then
    begin
      Result:= false;
      for i:=0 to Pred(FSplitGroups.Count) do
        begin
          aLVGroup.cbSize:= SizeOf(aLVGroup);
          aLVGroup.iGroupId:= (FSplitGroups[i]).GId;
          aLVGroup.mask:= LVGF_STATE {or  LVGF_GROUPID {};
          aLVGroup.stateMask:= LVGS_COLLAPSED;
          SendMessage(ListView.Handle, LVM_GETGROUPINFO, aLVGroup.iGroupId, LPARAM(@aLVGroup));
          if 0 < (aLVGroup.state and LVGS_COLLAPSED) then aRect.Top:= LVGGR_HEADER
                                                     else aRect.Top:= LVGGR_GROUP;
          SendMessage(ListView.Handle, LVM_GETGROUPRECT, aLVGroup.iGroupId, LPARAM(@aRect));
          if (aRect.Left<=X) and (X<=aRect.Right) and (aRect.Top<=Y) and (Y<=aRect.Bottom) then
            begin
              aDragInfo.XValue:= FSplitGroups[i].DBKey;
              aDragInfo.Rect:= aRect;
              aDragInfo.Align:= alClient;
              aDragInfo.Color:= clActiveCaption;
              Result:= True;
              Break;   // нашли - уходим
            end;
        end;
    end;

  if aDragInfo.CanOrder then
    begin
      ListView.DragCursor:= crHandPoint;
      aDragInfo.Color:= clBlack;//Red;

      if (-1 < TargetItem) then
        begin // мышь над каким-то элементом
          aDragInfo.Rect.Left:= LVIR_BOUNDS;
          SendMessage(ListView.Handle, LVM_GETITEMRECT, TargetItem, LPARAM(@aDragInfo.Rect));
          aRect:= aDragInfo.Rect;
          if ListViewStyleInt[FStyle] = LV_VIEW_DETAILS
            then aDragInfo.Align:= iif(Y < aRect.CenterPoint.Y, alTop, alBottom)
            else aDragInfo.Align:= iif(X < aRect.CenterPoint.X, alLeft, alRight);

          if aDragInfo.Align in [alTop, alLeft] then aDragInfo.OValue := TargetItem
                                                else aDragInfo.OValue := Succ(TargetItem);
          Exit(True);   // нашли - уходим
        end
      else
        begin
          // мышь над заголовком, т.е. перед первым элементом
          H:= SendMessage(ListView.Handle, LVM_GETHEADER, 0, 0);
          if H>0 then
            begin
              GetWindowRect(H, aRect);
              if (Y <= aRect.Height) and (0 < ListView.Items.Count) then
                begin
                  aDragInfo.Rect.Left:= LVIR_BOUNDS;
                  SendMessage(ListView.Handle, LVM_GETITEMRECT, 0, LPARAM(@aDragInfo.Rect));
                  aDragInfo.Align:= iif(ListViewStyleInt[FStyle] = LV_VIEW_DETAILS, alTop, alLeft);
                  aDragInfo.OValue:= 0;
                  Exit(True);   // определили - уходим
                end;
            end;

          // мышь над пустым пространством за последним элементом
          TargetItem:= ListView.Items.Count;
          SendMessage(ListView.Handle, LVM_GETITEMRECT, TargetItem, LPARAM(@aDragInfo.Rect));
          aDragInfo.Align:= iif(ListViewStyleInt[FStyle] = LV_VIEW_DETAILS, alTop, alLeft);
          aDragInfo.OValue:= TargetItem;
          Exit(True);   // определили - уходим
        end;
    end;
end;

procedure TListForm.SetCanDrag(value: Boolean);
begin
  inherited;
  if value then
    begin
      ListView.DragMode := dmAutomatic;
      ListView.OnDragDrop  :=DoDragDrop;
      ListView.OnDragOver  :=DoDragOver;
      ListView.OnStartDrag :=DoDragStart;
    end
  else
    begin
      ListView.DragMode := dmManual;
      ListView.OnDragDrop :=nil;
      ListView.OnDragOver :=nil;
      ListView.OnStartDrag :=nil;
    end;
end;

procedure TListForm.CMDrag(var Message: TCMDrag);
begin
  with Message, DragRec^ do
  begin
    case DragMessage of
{      dmDragEnter:begin
                    //
                  end;
      dmDragMove :begin
                    //
                  end;{}
      dmDragLeave:begin
                    FSelectRect.Hide;
                  end;
      dmDragDrop, dmDragCancel:
                  begin
                    FSelectRect.Hide;
                  end;
{      dmFindTarget:
                     Result := LongInt(Control); {}
    end;
  end;

  inherited;
end;

procedure TListForm.DoDragStart(Sender: TObject; var DragObject: TDragObject);
begin
  inherited;
  if 1 < dataSetMeta.Cache.SelectedCount then ListView.DragCursor:= crMultiDrag
                                         else ListView.DragCursor:= crDrag;
end;

procedure TListForm.DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited;
  if Not Accept then Exit;

  Accept:= CanDrag and DragInfoTarget(Sender, Source, X, Y, FDragInfo);     // заполняем информацию о приемнике

  if Accept then FSelectRect.Show(FDragInfo.Rect, FDragInfo.Color, FDragInfo.Align)
            else FSelectRect.Hide;
end;

procedure TListForm.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if DragInfoTarget(Sender, Source, X, Y, FDragInfo) then
    begin
      inherited;
      DataSetMeta.Cache.Update(mcUpdate, unassigned);
      ReloadItems;
    end;
end;

procedure TListForm.act_Da_AllColumnsExecute(Sender: TObject);
begin
  act_Da_AllColumns.Checked := Not act_Da_AllColumns.Checked;
  if act_Da_AllColumns.Checked then FVisibleLevel := fvLevel1
                               else FVisibleLevel := fvLevel2;
  ReCreateView;
end;

procedure TListForm.actDaColumnPropertyExecute(Sender: TObject);
var FM, newField : TFieldMeta;
    BackupCursor       : TCursor;
    RecordEditor       : TRecordEditor;
begin
  BackupCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  RecordEditor  := nil;
  try
    FM:= TFieldMeta((Sender as TAction).Tag);
    if FM.IsLookup then
      FM := FM.LookupPair;
    RecordEditor := TRecordEditor.Create(MetaData.MetaTables[idxFields], FM.ID);
    if Assigned(RecordEditor.CacheItem) then
    begin
      RecordEditor.Caption := StripHotKey((Sender as TAction).Caption);
      Screen.Cursor := BackupCursor;
      if RecordEditor.Execute then
      begin
     {   newField := nil;
        try
          newField := TFieldMeta.Create;
          newField.Assign(RecordEditor.CacheItem);
        finally
          newField.Free;
        end; {}
        FM.Assign(RecordEditor.CacheItem);
        if Assigned(FM.LookupPair) then
          FM.LookupPair.Order := FM.Order;
        FColumnList.SortByOrder;

        DataSetMeta.Cache.PrepareData;
        ReCreateView;
      end;
    end;
  finally
    if Assigned(RecordEditor) then RecordEditor.Free;
  end;
  Screen.Cursor := BackupCursor;
end;

procedure TListForm.ColumnsOrderClick(Sender:TObject);
var
  Key: Variant;
  DColumnList: TColumnList;
begin
  // + Замена старой логики выбора столбцов ...
  FixColumns;
  DColumnList:= TColumnList.Create;
  DColumnList.InitColumnList(DataSetMeta.Cache.Fields);

  if TItemsOrderForm.Execute(DataSetMeta.Cache, DColumnList) then
    begin
      { сохраняем изменения }
      {$IFDEF DEBUG}
        FColumnList.DebugColumnsLog('Destroy columns ' + DataSetMeta.Table.Table + ' ...');
      {$ENDIF}
      //if Assigned(DataSetMeta.Table) then
      //  DataSetMeta.Cache.Fields.SynchronizeUserFields(DataSetMeta.Table.Fields);
      FColumnList.InitColumnList(DataSetMeta.Cache.Fields);
      {$IFDEF DEBUG}
        FColumnList.DebugColumnsLog('Create columns ' + DataSetMeta.Table.Table + ' ...');
      {$ENDIF}
      FColumnList.SortByOrder;
      BeginViewUpdate;
      try
        ReInitColumns;
      except
        {$IFDEF DEBUG}
        on E: Exception do
          DebugLog('TListForm.ColumnsOrderClick error: ' + E.Message);
        {$ENDIF}
      end;
      {$IFDEF DEBUG}
        FColumnList.DebugColumnsLog('After sorting columns ' + DataSetMeta.Table.Table + ' ...');
      {$ENDIF}
      if Assigned(DataSetMeta.Cache.FocusedItem) then
        Key := DataSetMeta.Cache.FocusedItem.ID
      else
        Key := Unassigned;
      DataSetMeta.Cache.ReInit(mcNone, Key);
      //DataSetMeta.Cache.Count := DataSetMeta.Cache.Count;
      //DataSetMeta.Cache.Update(mcNone, Key);
      // ОБЯЗАТЕЛЬНО ВЫЗЫВЕМ МЕТОД ПРЕДКА!!!
      // Иначе полное перестроение кэша!!!
      inherited ReCreateView;
      EndViewUpdate;
    end;

  FreeAndNil(DColumnList);
  TBaseMainForm(MainBaseForm).RefreshMenu;
end;

procedure TListForm.ListColumnRightClick(Sender: TObject; Column: TListColumn; Point: TPoint);
var
  FMT, FMV: TFieldMeta;
  NColumn:TListColumn;
  dx:Integer;
  hChildWnd: HWND;
  WndClass: string;
  hdhti: THDHitTestInfo;
  O: Array of Integer;
begin
  NColumn:=Column;
  // определяем горизонтальное смещение для правильного выбора столбца
  dx:=GetScrollPos(ListView.Handle, sb_Horz);

  if dx>0 then
    begin
      hChildWnd := ChildWindowFromPoint(ListView.Handle, Classes.Point(Point.X-dx, Point.Y));
      if (hChildWnd <> 0) and (hChildWnd <> Handle) then
        begin
          SetLength(WndClass, 80);
          SetLength(WndClass, GetClassName(hChildWnd, PChar(WndClass), Length(WndClass)));
          if WndClass = 'SysHeader32' then
            begin
              hdhti.Point:= (Point);
              if SendMessage(hChildWnd, HDM_HITTEST, 1, Longint(@hdhti)) >= 0 then
                NColumn:=ListView.Columns[hdhti.Item];
            end;
        end;
    end;


  SetLength(O, ListView.Columns.count);
  Header_GetOrderArray(ListView_GetHeader(ListView.Handle), ListView.Columns.count, PInteger(@O[0]));

  FMT:= DataSetMeta.Cache.Fields[FColumnList[O[NColumn.Index]].TextIndex];
  FMV:= DataSetMeta.Cache.Fields[FColumnList[O[NColumn.Index]].ValueIndex];

  //if Assigned(FM.LookupPair) then FM:=FM.LookupPair;

  cpmSortUp.Action.Tag       := NativeInt(FMT);
  cpmSortDown.Action.Tag     := NativeInt(FMT);

  cpmDeleteColumn.Action.Tag := NativeInt(FMV);
  cpmSelectColumn.Action.Tag := NativeInt(FMT);
  TAction(cpmDeleteColumn.Action).Visible := Not (FMV.ID = DataSetMeta.Table.NField.ID);

  cpm_Da_GroupByColumn.Tag      := NativeInt(FMT);
  cpm_Da_GroupByColumn.Visible  := (FMT<>FMV) and (FMT.PolicyShow);

  TAction(cpmFormatOfColumn.Action).Caption := GetTitle('_Da.FormatCol')+' '''+FMT.Native+'''';;
  cpmFormatOfColumn.Action.Tag := NativeInt(FMT);

  ListView.PopupMenu := ColumnPopupMenu;
end;

procedure TListForm.ColumnPopupMenuPopup(Sender: TObject);
begin
  ListView.PopupMenu := PopupMenu;
end;

function TListForm.GetCacheItemAt(X, Y: Integer): TCacheItem;
var
  Item : TListItem;
begin
  Item := ListView.GetItemAt(X, Y);
  if Assigned(Item) then
    begin
      if Item is TDeListItem then result := TDeListItem(Item).CacheItem
                             else result := CacheItems.Items[Item.Index];
    end
  else
    result := inherited GetCacheItemAt(X, Y);
end;

procedure TListForm.SetVirtualMode(aValue: Boolean);
begin
  if aValue<>FVirtualMode then
    begin
      ListView.Items.BeginUpdate;
      ListView.Items.Clear;
      ListView.Items.Count:=0;
      ListView.Items.EndUpdate;
    end;

  if aValue then
    begin
      ListView.OwnerData:= True;  // True - виртуальный режим
      // LVS_OWNERDATA
      //   SendMessage(ListView.Items.Handle, LVM_SETEXTENDEDLISTVIEWSTYLE, LVS_OWNERDATA, 0);
      ListView.OnDataHint := nil;//ListViewDataHint;
      Style:= FStyle;
    end
  else
    begin
      ListView.OwnerData:= False;
      ListView.OnDataHint := nil;
      Style:= FStyle;
   end;
  FVirtualMode:= aValue;
end;

function TListForm.GetSplitShowEmpty: Boolean;
begin
  Result:= act_Da_ShowEmpty.Checked;
end;

procedure TListForm.SetSplitShowEmpty(aValue: Boolean);
begin
  act_Da_ShowEmpty.Checked:= avalue;
end;

procedure TListForm.SetSplitField(aField: TFieldMeta);
begin
  if Assigned(aField) and (aField.Owner = DataSetMeta.Table) then
    begin
      VirtualMode:= False;
      FFieldX:= aField;
    end
  else
    begin
      VirtualMode:= True;
      FFieldX:= nil;
    end;
end;

procedure TListForm.ListViewInfoTip(Sender: TObject; Item: TListItem; var InfoTip: String);
begin
  if (DataSetMeta=nil) or (FStyle in lvsReportStyles) then Exit;
  if Item is TDeListItem
    then if assigned(TDeListItem(Item).CacheItem)
            then InfoTip:= TDeListItem(Item).CacheItem.Hint
            else InfoTip:= EmptyStr
    else if Item.Index <= CacheItems.Count
            then InfoTip:= CacheItems.Items[Item.Index].Hint
            else InfoTip:= EmptyStr;
end;
(*
procedure TListForm.ListViewAdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var IIndex:Integer;
    i, N : integer;
    CI: TCacheItem;
    FM : TFieldMeta;
begin
  DefaultDraw := True;

  if Not Assigned(Item) then
    Exit;
  //DOWN: Сделана дополнительная проверка, иногда была ошибка
  IIndex:=Item.Index;

  if Item is TDeListItem then
    begin
      if not assigned(TDeListItem(Item).CacheItem) then
        begin
          TListView(Sender).Canvas.Font.Color:= clSilver;
        end
      else
        begin
          CI:= CacheItems[IIndex];
          TListView(Sender).Canvas.Font.Color:= CI.FontColor;

          if not TDeListItem(Item).Loaded then
            begin
              TDeListItem(Item).Loaded:= True;
              Item.Data:= CI;
              Item.Caption:= CI.Caption;
              if CI.ImageIndex<1 then Item.ImageIndex:= DM.MapIconIndex(DataSetmeta.Table.Ico)
                                 else Item.ImageIndex:= DM.MapIconIndex(CI.ImageIndex);

              for i:=1 to ListView.Columns.Count-1 do
                begin
                  FM := TFieldMeta(ListView.Column[i].Tag);
                  if Assigned(FM) then
                    begin
                      N := CacheItems.Fields.IndexByID(FM.ID);
                      if N>=0 then Item.SubItems.Add(CI.FieldText[ N ])
                              else Item.SubItems.Add(EmptyStr);
                    end;
                end;
            end;
        end

     end
   else
     begin
       TListView(Sender).Canvas.Font.Color:=  CacheItems.Items[Item.Index].FontColor;
     end;
end;
*)
procedure TListForm.SetZOrder(TopMost: Boolean);
begin
  if TopMost = False then
    begin
      inherited SetZOrder(TopMost);
    end
  else
    begin
      if Visible then ReAlignColumns(ListView.Width);
      inherited SetZOrder(TopMost);
    end;
end;

procedure TListForm.act_Da_ColumnSortXExecute(aField: TFieldMeta; const aDirection: TDSSortDirection);
var dx: Integer;
{$IFDEF DEBUG}
const
  SortDirectionNames: array[TDSSortDirection] of PChar = ('sdNone', 'sdAscending', 'sdDescending');
{$ENDIF}
begin
  {$IFDEF DEBUG}
  if Assigned(aField) then
    DebugLog('TListForm.act_Da_ColumnSortXExecute({%s}, %s) start ...', [aField.Original, StrPas(SortDirectionNames[aDirection])])
  else
    DebugLog('TListForm.act_Da_ColumnSortXExecute(nil, %s) start ...', [StrPas(SortDirectionNames[aDirection])]);
  {$ENDIF}

  if (DataSetMeta.Cache.SortList.Count = 1)
    and (DataSetMeta.Cache.SortList[0].FieldID = aField.ID)
    and (DataSetMeta.Cache.SortList[0].Direction = aDirection) then Exit;

  DataSetMeta.Cache.ClearSortList;
  DataSetMeta.Cache.SortList.Add(aField.ID, aDirection);

  dx:=GetScrollPos(ListView.Handle, sb_Horz);
  if dx=0 then
      DataSetMeta.Cache.Update(mcUpdate, Null)
  else
    begin
      LockWindowUpdate(ListView.Handle);
      DataSetMeta.Cache.Update(mcUpdate, Null);
      ListView.Scroll(dx,0);
      LockWindowUpdate(null);
    end;

  {$IFDEF DEBUG}
  if Assigned(aField) then
    DebugLog('TListForm.act_Da_ColumnSortXExecute({%s}, %s) finish ...', [aField.Original, StrPas(SortDirectionNames[aDirection])])
  else
    DebugLog('TListForm.act_Da_ColumnSortXExecute(nil, %s) finish ...', [StrPas(SortDirectionNames[aDirection])]);
  {$ENDIF}
end;

procedure TListForm.act_Da_ColumnSortUpExecute(Sender: TObject);
begin
  BeginViewUpdate;
  act_Da_ColumnSortXExecute(TFieldMeta((Sender as TAction).Tag), sdAscending);
  EndViewUpdate;
end;

procedure TListForm.act_Da_ColumnSortDownExecute(Sender: TObject);
begin
  BeginViewUpdate;
  act_Da_ColumnSortXExecute(TFieldMeta((Sender as TAction).Tag), sdDescending);
  EndViewUpdate;
end;

procedure TListForm.act_Da_ColumnDeleteExecute(Sender: TObject);
var FM: TFieldMeta;
begin
  FM:= TFieldMeta((Sender as TAction).Tag);
  FM.VisibleLevel := fvLevel1;

  BeginViewUpdate;
  ReInitColumns;
  EndViewUpdate;

  TBaseMainForm(MainBaseForm).RefreshMenu;
end;

procedure TListForm.act_Da_FitWidthExecute(Sender: TObject);
var
  FM : TFieldMeta;
  j,i,w,AverageWidth,AverageCount,FieldIndex,FocusedIndex: Integer;
begin

  for j:=0 to DataSetMeta.Table.Fields.Count-1 do
  begin
    // из пары lookUp определяем по полю "название", а ставим полю "ключ"
    if DataSetMeta.Table.Fields[j].IsLookup then Continue;

    if assigned(DataSetMeta.Table.Fields[j].LookupPair) then
      FM:=DataSetMeta.Table.Fields[j].LookupPair
    else
      FM:=DataSetMeta.Table.Fields[j];

    if FM.ShowType = stYesNo then
      W:=3
    else
      case FM.DataType of
        ftString,ftWideString,ftFixedChar:
          begin
            if assigned(DataSetMeta.Cache.FocusedItem) then
              begin
                AverageCount := 1;
                AverageWidth := 12;

                FieldIndex   := DataSetMeta.Cache.Fields.IndexByID(FM.ID);
                FocusedIndex := DataSetMeta.Cache.FocusedIndex;

                for i:=Max(FocusedIndex-20,0) to Min(FocusedIndex+20,DataSetMeta.Cache.Count-1) do
                  begin
                    W:=Length(DataSetMeta.Cache[i].FieldText(FieldIndex));
                    Inc(AverageWidth, min(W, 256));
                    if W>0 then Inc(AverageCount);
                  end;

                W:=(2 * AverageWidth) div AverageCount;
              end
            else
              W:=12;
          end;
        ftBoolean,ftBlob:
          W:=3;
        ftInteger,ftSmallint,ftLargeint,ftWord:
          W:=6;
  //    ftFloat,ftCurrency,ftBCD,ftAutoInc:
  //      W:=9*SymbolWidth;
        ftDate,ftTime,ftDateTime,ftTimeStamp:
          if Length(Trim(FM.Template))>0 then
            W:=(Length(Trim(FM.Template))+1)
          else
            W:=9;
      else
        W:=9;
      end;

    if DataSetMeta.Table.Fields[j].Width<>W then
      begin
        DataSetMeta.Table.Fields[j].Width:=W;
//        DataSetMeta.Table.Fields[j].UserSave;
      end;
  end;
//  DataSetMeta.Cache.PrepareData(false);
  ReCreateView;
end;

procedure TListForm.Act_Da_ColSaveExecute(Sender: TObject);
begin
  if Assigned(DataSetMeta) then
    if Assigned(DataSetMeta.Cache) and Assigned(DataSetMeta.Cache.Fields) then
      DataSetMeta.Cache.Fields.UserSave(DataSetMeta.Table.ID)
    else
      if Assigned(DataSetMeta.Table) and Assigned(DataSetMeta.Table.Fields) then
        DataSetMeta.Table.Fields.UserSave(DataSetMeta.Table.ID);
end;

procedure TListForm.Act_Da_ColClearExecute(Sender: TObject);
begin
  DataSetMeta.Table.DeleteUserValues(uvFields, UserSession.ID);
end;

procedure TListForm.Act_Da_ColSaveAllExecute(Sender: TObject);
var
  Index: Integer;
  FieldsMeta: TFieldsMeta;
  FieldMeta: TFieldMeta;
begin
  Act_Da_ColSaveExecute(Sender);
  if Assigned(DataSetMeta.Cache) and Assigned(DataSetMeta.Cache.Fields) then
    FieldsMeta := DataSetMeta.Cache.Fields
  else if Assigned(DataSetMeta.Table) and Assigned(DataSetMeta.Table.Fields) then
    FieldsMeta := DataSetMeta.Table.Fields
  else
    FieldsMeta := nil;
  if Assigned(FieldsMeta) then
    for Index := 0 to Pred(FieldsMeta.Count) do
      begin
        FieldMeta := FieldsMeta[Index];
        if Assigned(FieldMeta) then
          begin
            if assigned(FieldMeta.LookupPair) then
              if FieldMeta.IsLookup then
                FieldMeta := FieldMeta.LookupPair
              else
                Continue;
            if FieldMeta.UserApply then FieldMeta.Save;
          end;
      end;
end;

procedure TListForm.Act_Da_ColClearAllExecute(Sender: TObject);
begin
  DataSetMeta.Table.DeleteUserValues(uvFields);
end;

procedure TListForm.OnGroupColumnClick(Sender: TObject);
var FM : TFieldMeta;
begin
  FM:=TFieldMeta(TControl(Sender).Tag);
  if Assigned(FM) then GroupCreate(FM, DataSetMeta.GetKeyValue);
end;

procedure TListForm.act_Da_SelectallExecute(Sender: TObject);
var i: Integer;
begin
  inherited;
  BeginViewUpdate;
  try
    if VirtualMode then
      try
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, DataSetMeta.Cache.Count, 1);
        for I:= 0 to Pred(DataSetMeta.Cache.Count) do
          begin
            ListView.Items[I].Selected := True;
            DataSetMeta.Cache.ItemSelect(i);
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, I, 0);
          end;
      finally
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
      end
    else
      try
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, ListView.Items.Count, 1);
        for I:= 0 to Pred(ListView.Items.Count) do
          begin
            if Assigned(TDeListItem(ListView.Items[i]).CacheItem) then
              begin
                ListView.Items[I].Selected := True;
                TDeListItem(ListView.Items[i]).CacheItem.Selected:=True;
              end;
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, I, 0);
          end;
      finally
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
      end;

    for I := 0 to Pred(DataSetMeta.Children.Count) do
      DataSetMeta.Children[i].Cache.Update(mcUpdate, null);
  finally
    EndViewUpdate;
  end;
end;

procedure TListForm.act_Da_InvertselectionExecute(Sender: TObject);
var i: integer;
begin
  inherited;
  BeginViewUpdate;
  try
    if VirtualMode then
      try
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, DataSetMeta.Cache.Count, 1);
        for I:= 0 to Pred(DataSetMeta.Cache.Count) do
          begin
            ListView.Items[i].Selected := not ListView.Items[i].Selected;
            DataSetMeta.Cache.Items[i].Selected := ListView.Items[i].Selected;
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, I, 0);
          end;
      finally
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
      end
    else
      try
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, ListView.Items.Count, 1);
        for I:= 0 to Pred(ListView.Items.Count) do
          begin
            if Assigned(TDeListItem(ListView.Items[i]).CacheItem) then
              begin
                ListView.Items[i].Selected := not ListView.Items[i].Selected;
                TDeListItem(ListView.Items[i]).CacheItem.Selected:= ListView.Items[i].Selected;
              end;
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, I, 0);
          end;
      finally
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
      end;

    for I := 0 to Pred(DataSetMeta.Children.Count) do
      DataSetMeta.Children[i].Cache.Update(mcUpdate, null);
  finally
    EndViewUpdate;
  end;
end;

procedure TListForm.UpdateTimerTimer(Sender: TObject);
begin
  if (FUpdateDelay <= 0) then exit;
  dec(FUpdateDelay);
  if (FUpdateDelay > 0) then exit;

  doListItemChange;
end;

procedure TListForm.lbCaptionClick(Sender: TObject);
const
  HDF_SORTUP = $0400;
  HDF_SORTDOWN = $0200;
var
 HeaderHndl: HWND;
 Item: THDItem;
begin
 {получаем дескриптор заголовка нашего ListView'а}
 HeaderHndl := ListView_GetHeader(ListView.Handle);
 FillChar(Item, SizeOf(Item), 0);
 {значение этого оператора см. в справке от Microsoft}
 Item.Mask := HDI_FORMAT;
 {получаем информацию об указанной колонке в заголовке для дальнейшего ее изменения}
 Header_GetItem(HeaderHndl, 1{ListView.ColumnIndex}, Item);
// if (1{ColumnIndex} = FArrowOptions.SortColumnIndex) then
 begin
  {предварительно убираем обе стрелки из флагов параметра fmt}
  Item.fmt:= Item.fmt and not (HDF_SORTDOWN or HDF_SORTUP);
  {указываем системе прорисовывать необходимую стрелку добавлением соотв. флага}

  if Random(2)=1 then
   Item.fmt:= Item.fmt or HDF_SORTUP
  else
   Item.fmt:= Item.fmt or HDF_SORTDOWN;
 end

 //на этой колонке не надо рисовать стрелку - сообщаем об этом системе
 {
 else
  Item.fmt := Item.fmt and not( HDF_BITMAP or HDF_BITMAP_ON_RIGHT or
    HDF_SORTDOWN
  or HDF_SORTUP);
 {}
 {применяем установленные атрибуты - система сама нарисует колонку}
 ;
 Header_SetItem(HeaderHndl, 0, Item);
end;

function TListForm.GetPropertiesString: string;
begin
  Result := inherited GetPropertiesString;
  Result := Result + ' ' + cListStyleParam + '=' + ListViewStyleNames[FStyle] + ';';

  if Assigned(SplitField) then
    begin
      Result := Result + ' ' + cListSplitField + '=' + SplitField.Original + ';'
                       + ' ' + cListSplitEmpty + '=' + IntToStr(Integer(SplitShowEmpty)) + ';';
    end;

  if assigned(FSelectedField) then
    begin
      Result := Result + ' ' + cListAgregateFunc + '=' + TAgregateTypeID[MetaData.AgregateType] + ';'
                       + ' ' + cListAgregateField + '=' + FSelectedField.Original + ';';
    end;
end;

procedure TListForm.SetDataSetMeta(const Value: TDataSetMeta);
var
  Index: Integer;
  Field: TFieldMeta;
begin
  if Assigned(Value) and Assigned(Value.Cache) {and not Value.Cache.Active} then
    begin
      if Assigned(Value.Table) then
        if Assigned(Value.Cache.Fields) and (Value.Cache.Fields.Count <> 0) then
          begin
            FFieldY:= Value.Table.PField;  //запоминаем для Drag-and-Drop

            for Index := 0 to Pred(Value.Cache.Fields.Count) do
              begin
                Field := Value.Cache.Fields[Index];
                if Assigned(Field) then
                  if Field.Key then
                    Field.Stage := fsKey
                  else if Assigned(Value.Table.OField) and (Value.Table.OField.ID = Field.ID) then
                    Field.Stage := fsBase
                  else if Assigned(Value.Table.GField) and (Value.Table.GField.ID = Field.ID) then
                    Field.Stage := fsBase
                  else if Assigned(Value.Table.NField) and (Value.Table.NField.ID = Field.ID) then
                    Field.Stage := fsBase
                  else if Assigned(Value.Table.PField) and (Value.Table.PField.ID = Field.ID) then
                    Field.Stage := fsBase
                  else if Assigned(Value.Table.DField) and (Value.Table.DField.ID = Field.ID) then
                    Field.Stage := fsBase
                  else if Assigned(Value.Table.IconField) and (Value.Table.IconField.ID = Field.ID) then
                    Field.Stage := fsBase
                  else if Assigned(Value.Table.ColorField) and (Value.Table.ColorField.ID = Field.ID) then
                    Field.Stage := fsBase
                  else if ((Field.OriginalVisibleLevel in [fvLevel2, fvLevel3]) or (Field.VisibleLevel in [fvLevel2, fvLevel3])) and not Field.IsLookup then
                    Field.Stage := fsBase
                  else if Field.DataType in [ftBlob, ftMemo, ftWideMemo] then
                    Field.Stage := fsBlob
                  else if Assigned(Value.Cache.SortList) and (Value.Cache.SortList.IndexByID(Field.ID) <> -1) then
                    Field.Stage := fsBase
                  else
                    Field.Stage := fsFull;
              end;
          end;
    end;

  inherited SetDataSetMeta(Value);
end;

{ TSplitGroup }

constructor TSplitGroup.Create(aOwner: TObjectList<TSplitGroup>; aGID: Integer; aDBKey: Variant; aDBIndex: Integer);
begin
  GID:= aGID;
  DBKey:= aDBKey;
  DBIndex:= aDBIndex;
  GCount:= 0;
  aOwner.Add(self)
end;

procedure TSplitGroup.IncCount;
begin
  GCount:= Succ(GCount);
end;

initialization
  RegisterClass(TListForm);

end.

