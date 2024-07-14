unit TreeFormUnit;

interface

uses
  Windows, SysUtils, Classes, Controls, ComCtrls, Buttons, Graphics, ExtCtrls, Menus, ActnList, StdCtrls,
  Messages, {Forms, } ToolWin, Actions, DeMeta, Contnrs,
  {Funcs, }BaseGridFormUnit, DataCacheUnit, DSMeta, Vcl.ImgList;

type
  TDeHeaderSection = class(THeaderSection)
  private
    FFieldMetaID: Variant;
  public
    property FieldMetaID: Variant read FFieldMetaID write FFieldMetaID;
  end;

  TTreeForm = class(TBaseGridForm)
    ItemCollapse: TMenuItem;
    ItemExpand: TMenuItem;
    ItemIconsSmall: TMenuItem;
    act_Da_IconsBig: TAction;
    ItemIconsBig: TMenuItem;
    act_Da_IconsSmall: TAction;
    act_Da_CollapseAll: TAction;
    act_Da_Expand: TAction;
    act_Da_Collapse: TAction;
    MMIconsBig: TMenuItem;
    MMIconsSmall: TMenuItem;
    TreeView: TTreeView;
    ItemCollapseAll: TMenuItem;
    N4: TMenuItem;
    Header: THeaderControl;
    procedure TreeMenuPopup(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure act_Da_IconsBigExecute(Sender: TObject);
    procedure act_Da_IconsSmallExecute(Sender: TObject);
    procedure act_RenameExecute(Sender: TObject);
    procedure act_Da_CollapseExecute(Sender: TObject);
    procedure act_Da_ExpandExecute(Sender: TObject);
    procedure act_Da_CollapseAllExecute(Sender: TObject);
    procedure TreeViewChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure TreeViewCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
    procedure act_PropertiesExecute(Sender: TObject);
    procedure TreeViewContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure TreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
      Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure HeaderSectionResize(HeaderControl: THeaderControl; Section: THeaderSection);
    procedure HeaderResize(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
    procedure TreeViewDblClick(Sender: TObject);
    procedure HeaderSectionClick(HeaderControl: THeaderControl; Section: THeaderSection);
  private
    FViewWidth: integer;
    FTextHeight: Integer;
    FColumnList: TColumnList;
//    FViewList  : TFieldsMeta;
    RootItem   : TCacheItem;
    FOnMouseNode : TTreeNode;
    procedure CreateTree(RootNode : TTreeNode; RootCacheItem: TCacheItem; Depth : integer = 2);
    procedure CatchGarbage;
  protected
    function GetStatusText : string; override;
    procedure InitForm;override;
    procedure AfterInit;override;
    procedure SetCanDrag(Value: Boolean); override;
    function GetCacheItemAt(X, Y: Integer):TCacheItem;override;
    procedure GroupMenuCreate(GM_Item : TMenuItem);override;
    procedure CacheSelectedChange(Item : TCacheItem);override;
    procedure CacheFocusedChange(Item : TCacheItem);override;
    procedure CacheItemInsert(Item : TCacheItem);override;
    procedure CacheSyncronize(var AFocusedItem : TCacheItem; AIndex : Integer = ltNone);override;
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure SetDataSetMeta(const Value: TDataSetMeta); override;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    procedure DeInitForm;override;
    procedure ReCreateView; override;
    procedure MakeVisibleItem(Item: TCacheItem); override;
    function GetPropertiesString: string; override;
  end;

//var
//  TreeForm : TTreeForm;

implementation

uses Variants, DB,
     DeLog, DeTypes, Dictionary, DataUnit, Funcs;

{$R *.dfm}

//..............................................................................
procedure TTreeForm.TreeMenuPopup(Sender: TObject);
var SNode:TTreeNode;
begin
  SNode:=TreeView.Selected;
  if SNode=nil then exit;

  Act_Da_Expand.Visible   := (Not SNode.Expanded) and (SNode.HasChildren);
  Act_Da_Collapse.Visible := (    SNode.Expanded) and (SNode.HasChildren);
  Act_Da_CollapseAll.Visible := Assigned(DataSetMeta.Table.PField);
end;

//..............................................................................
procedure TTreeForm.TreeViewEdited(Sender: TObject; Node: TTreeNode; var S: String);
var //Value   : Variant;
    NeedUpdateParent : Boolean;
    i : integer;
begin
  if Node<>nil then
  begin
    DataSetMeta.Cache.FocusedItem.FieldNativeValue
      [DataSetMeta.Cache.Fields.IndexByID(DataSetMeta.Table.NField.ID)] := s;

    if DataSetMeta.Cache.UpdateRecord(DataSetMeta.Cache.FocusedItem) then
    begin
      NeedUpdateParent := Assigned(DataSetMeta.Table.PField) and
      VarSameValue(CacheItems.FocusedItem.FieldValue[CacheItems.FolderSignIndex],
        CacheItems.TableMeta.GField.Value1);
      CacheItems.FocusedItem.ApplyChanges;
      if NeedUpdateParent then
      begin
        for I := 0 to DataSetMeta.OwnerLinks.Count-1 do
          if DataSetMeta.OwnerLinks[I].DataSetMeta.Table = DataSetMeta.Table then
          begin
            DataSetMeta.OwnerLinks[I].DataSetMeta.Cache.Update(mcUpdate, Null);
            Break;
          end;
      end;
    end;
  end;
end;
//..............................................................................
constructor TTreeForm.Create(aOwner : TComponent);
begin
  inherited;
  FMainControl := TreeView;
  FViewWidth := 0;
  //FViewList  := TFieldsMeta.create(False);
  FOnMouseNode := nil;
end;

destructor TTreeForm.Destroy;
begin
  FreeAndNil(FColumnList);
  //FViewList.Destroy;
  inherited;
end;

procedure TTreeForm.InitForm;
var i: Integer;
begin
  ViewParams.SetByName('TreeIcon', EmptyStr);
  inherited;

  if DataSetMeta=nil then Exit;
  FFieldX:= DataSetMeta.Table.PField;
  FreeAndNil(FColumnList);
  FColumnList:= TColumnList.Create;
  FTextHeight:= TreeView.Canvas.TextHeight('Yy');
  //FViewList.Clear;
  if DataSetMeta.Role = drParent then
    begin
      FColumnList.Add(DataSetMeta.Table.Fields, DataSetMeta.Table.NField);
    end
  else
    for i:=0 to DataSetMeta.Table.Fields.Count-1 do
      if ((DataSetMeta.Table.NField = DataSetMeta.Table.Fields[i]) or
          (DataSetMeta.Table.Fields[i].OriginalVisibleLevel >= fvLevel2)) and
          (Not DataSetMeta.Table.Fields[i].IsLookup)  then
        begin
          FColumnList.Add(DataSetMeta.Table.Fields, I);
        end;
  FColumnList.SortByOrder;
  FViewWidth:= FColumnList.FullWidth;

  if ViewParams.TestByName('TreeIcon','Big') then
      begin
        act_Da_IconsBig.Checked := true;
        TreeView.Images := DM.ilIcon32; // DM.ImagesWindow;
      end
    else
      begin
        act_Da_IconsSmall.Checked := true;
        TreeView.Images := DM.ilIcon16; // DM.SImages;
      end;
  TreeView.Indent:= TreeView.Images.Width ;
end;

procedure TTreeForm.MakeVisibleItem(Item: TCacheItem);
begin
  if assigned(Item) then
    if assigned(Item.Data) then
      try
        TTreeNode(Item.Data).MakeVisible;
      except
      end;
end;

procedure TTreeForm.AfterInit;
begin
  inherited AfterInit;
  if not assigned(DataSetMeta) then Exit;
  if DataSetMeta.Cache.Active then
    if Not Assigned(DataSetMeta.Cache.FocusedItem) and (0<DataSetMeta.Cache.Count) then
      begin
        // ставим фокус на первый невиртуальный элемент учетом иерархии дерева
        if 0<TreeView.Items.Count then
          if Assigned(TreeView.Items[0].Data) and not (TreeView.Items[0].Data = rootItem )
            then begin
                   TreeView.Items[0].Selected:= True;
                   Exit;
                 end;

        if 1<TreeView.Items.Count then
          if Assigned(TreeView.Items[1].Data)
            then begin
                   TreeView.Items[1].Selected:= True;
                   Exit;
                 end;
      end;
end;

procedure TTreeForm.DeInitForm;
begin
  TreeView.OnChange:=nil;
  TreeView.OnChanging := nil;
  TreeView.Items.BeginUpdate;
  TreeView.Items.Clear;
  inherited;
end;

//..............................................................................

procedure TTreeForm.TreeViewChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  inherited;
 // AllowChange := Assigned(Node) and Assigned(Node.Data);
end;

procedure TTreeForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
var CacheItem : TCacheItem;
begin
  if Assigned(Node) and Assigned(Node.Data) then
    begin
      CacheItem := TCacheItem(Node.Data);
      if Not CacheItem.Focused then
        begin
          Node.Focused:=True;
          CacheItem.Owner.ClearSelection;
          CacheItem.Selected := True;
          CacheItem.SetFocus;
        end;
    end;
end;

procedure TTreeForm.TreeViewClick(Sender: TObject);
begin 
  inherited;
  TreeViewChange(Sender, TreeView.Selected);
end;

function TTreeForm.GetCacheItemAt(X, Y: Integer): TCacheItem;
var
   TN : TTreeNode;
begin
  TN   := TreeView.GetNodeAt(X,Y);
  if Assigned(TN) then
    result := TCacheItem(TN.Data)
  else
    result := inherited GetCacheItemAt(X, Y);
end;

procedure TTreeForm.CreateTree(RootNode : TTreeNode; RootCacheItem: TCacheItem; Depth : integer);
{$J+}
const  Level : Integer = 0;
{$J-}
var Item : TCacheItem;
    NewNode : TTreeNode;
begin
  if Level = Depth then Exit;
  Item := RootCacheItem.GetFirstChild;
  if not Assigned(Item) then Exit;
  inc(Level);
  TreeView.Items.BeginUpdate;
  repeat
    if not Assigned(Item.Data) then
      begin
        NewNode := TreeView.Items.AddChild(RootNode,Item.Caption);
        NewNode.ImageIndex := DM.MapIconIndex(Item.ImageIndex);
        NewNode.SelectedIndex := DM.MapIconIndex(Item.SelectedImageIndex);
        NewNode.Data := Item;
        Item.Data := NewNode;
      end
    else
      NewNode := TTreeNode(Item.Data);
    CreateTree(NewNode,Item, Depth);
    Item := RootCacheItem.GetNextChild(Item);
  until not Assigned(Item);
  TreeView.Items.EndUpdate;
  Dec(Level);
 // if Level > 0 then Exit;
end;

procedure TTreeForm.CatchGarbage;
// var
//  i : integer;
begin
  {for i:=0 to CacheItems.Count-1 do
    if not Assigned(CacheItems.Items[i].Data)and(CacheItems.Items[i]) then
    begin
      NewNode := TreeView.Items.AddChild(RootNode,CacheItems.Items[i].Caption);
      NewNode.ImageIndex := CacheItems.Items[i].ImageIndex;
      NewNode.SelectedIndex := CacheItems.Items[i].SelectedImageIndex;
      NewNode.Data := CacheItems.Items[i];
      CacheItems.Items[i].Data := NewNode;
    end;}
end;

function TTreeForm.GetStatusText : string;
begin
  if TreeView.Selected=nil then
    result:='-'
  else
    if TreeView.Selected.Parent=nil then
      result:=format('%d [%d]',[TreeView.Selected.Index+1,
                                TreeView.Selected.Count])
    else
      result:=format('%d/%d [%d]',[TreeView.Selected.Index+1,
                                   TreeView.Selected.Parent.Count,
                                   TreeView.Selected.Count]);
end;

procedure TTreeForm.GroupMenuCreate(GM_Item : TMenuItem);
var
    i : integer;
begin
  inherited;
  for i:=GM_Item.Count-1 downto 0 do
    if TFieldMeta(StrToIntDef(Copy(TMenuItem(GM_Item.Items[i]).Name,4,MaxInt),-1)) = DataSetMeta.Table.PField then
      GM_Item.Items[i].Enabled:=False;
end;

procedure TTreeForm.HeaderResize(Sender: TObject);
var i,wIco,wScroll,P,W,ScreenW : Integer;
begin
  inherited;
  if TreeView.Images=nil then Exit;

  P:=0;
  wIco:= TreeView.Images.Width;
  wScroll:= GetSystemMetrics(SM_CXVSCROLL);
  ScreenW:= TreeView.Width-(wIco+3*wScroll+wScroll);

  if FColumnList.Count {FViewList.Count} = Header.Sections.Count then
    for i:=0 to Pred(FColumnList.Count{FViewList.Count}) do
      begin
        if i = Pred(FColumnList.Count{FViewList.Count}) then
          W:= Width-P
        else
          begin
            W:= (FColumnList[i].Field.Width {FViewList[i].Width} * ScreenW) div FViewWidth;
            if i=0 then Inc(W, wIco + 3*wScroll);

            P:=P+W;
          end;
         Header.Sections[i].Width:=W;
      end;
  TreeView.Refresh;
end;

procedure TTreeForm.HeaderSectionClick(HeaderControl: THeaderControl; Section: THeaderSection);
var FM: TFieldMeta;
    SortItem : Integer;
    KeyboardState : TKeyboardState;
begin
  inherited;
  FM:= nil;
  if Section is TDeHeaderSection then
    FM:= DataSetMeta.Table.Fields.FindByID(TDeHeaderSection(Section).FieldMetaID);

  if FM=nil then Exit;
  GetKeyboardState(KeyboardState);

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

  DataSetMeta.Cache.Update(mcUpdate, Null);
end;

procedure TTreeForm.HeaderSectionResize(HeaderControl: THeaderControl; Section: THeaderSection);
begin
  inherited;
  TreeView.Refresh;
end;

procedure TTreeForm.ReCreateView;
var RootNode : TTreeNode;
    ParentDS : TDataSetMeta;
    i        : integer;
    THS      : TDeHeaderSection;
begin
  TreeView.Items.BeginUpdate;
  TreeView.OnChange:=nil;
  CacheItems.ClearLinks;

  Header.Sections.BeginUpdate;
  Header.Sections.Clear;
  for i:=0 to Pred(FColumnList.Count{FViewList.Count}) do
    begin
      THS:= TDeHeaderSection.Create(Header.Sections);
      THS.Text:= GetTitle(FColumnList[i].Field.Name{TFieldMeta(FViewList[i]).Name});
      THS.Alignment:= FColumnList[i].Field.Alignment;
      THS.AllowClick:= True;
      THS.FieldMetaID:= FColumnList[i].Field.ID;
    end;

  HeaderResize(nil);
  Header.Sections.EndUpdate;
  Header.Visible:= (1<Header.Sections.Count);

  TreeView.Items.Clear;
  if DataSetMeta.Cache.Active then
  begin
    CacheItems.FillAll;
    RootNode := nil;
    RootItem := CacheItems.RootItem;
    if Assigned(DataSetMeta.Table.PField) then
      begin
        ParentDS := nil;

        for i := 0 to DataSetMeta.OwnerLinks.Count -1 do
          if DataSetMeta.OwnerLinks[I].DataSetMeta.Table.ID = DataSetMeta.Table.ID then
            begin
              ParentDS := DataSetMeta.OwnerLinks[I].DataSetMeta;
              Break;
            end;

        if Assigned(ParentDS)and Assigned(ParentDS.Cache.FocusedItem)
          and(not (ParentDS.Cache.FocusedItem is TRootItem)) then
            RootItem := CacheItems.InsertExistItem(ParentDS.Cache.FocusedItem);

        RootNode := TreeView.Items.Add(nil,RootItem.Caption);
        RootNode.ImageIndex := RootItem.MapImageIndex;
        RootNode.Data := RootItem;
        RootItem.Data := RootNode;

      end;
    CreateTree(RootNode,RootItem);
    CatchGarbage;
    CacheFocusedChange(CacheItems.FocusedItem);
    for i:=0 to CacheItems.SelectedCount-1 do
      CacheSelectedChange(CacheItems.SelectedItems[i]);
  end;
  TreeView.OnChange:=TreeViewChange;
  TreeView.Items.EndUpdate; 
  TreeView.Refresh;

  Inherited;
end;

procedure TTreeForm.SetCanDrag(Value: Boolean);
begin
  inherited;
  if Value then
    begin
      TreeView.DragMode := dmAutomatic;
      TreeView.OnDragOver:= DoDragOver;
      TreeView.OnDragDrop:= DoDragDrop;
    end
  else
    begin
      TreeView.DragMode := dmManual;
      TreeView.OnDragOver :=nil;
      TreeView.OnDragDrop :=nil;
    end;
end;

procedure TTreeForm.SetDataSetMeta(const Value: TDataSetMeta);
var
  Index: Integer;
  Field: TFieldMeta;
begin
  if Assigned(Value) and Assigned(Value.Cache) and not Value.Cache.Active then
    if Assigned(Value.Table) then
      begin
        if Assigned(Value.Cache.Fields) and (Value.Cache.Fields.Count <> 0) then
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
                else if Field.DataType in BinaryTypes then
                  Field.Stage := fsBase
                else if Field.DataType in [ftMemo, ftWideMemo] then
                  Field.Stage := fsBlob
                else if Assigned(Value.Cache.SortList) and (Value.Cache.SortList.IndexByID(Field.ID) <> -1) then
                  Field.Stage := fsBase
                else
                  Field.Stage := fsFull;
            end;
      end;

  inherited SetDataSetMeta(Value);
end;

procedure TTreeForm.CacheFocusedChange(Item: TCacheItem);
var
  Depth       : integer;
  AItem       : TCacheItem;
  ItemList    : TCacheItemList;
  Flag        : Boolean;
begin
  if Assigned(Item)then
    begin
      if (not Assigned(Item.Data))and(TreeView.Items.Count > 0)then
        begin
          AItem :=  Item;
  //      Flag := False;
          Depth := 0;
          ItemList := TCacheItemList.Create(False);

          repeat
            AItem := AItem.Owner.FindById(AItem.ParentID);
            if Assigned(AItem)and(ItemList.IndexOf(AItem)<0) then
              Flag := Assigned(AItem.Data)
            else
              Flag := True;
            ItemList.Add(AItem);
            inc(Depth);
          until Flag;

          ItemList.Free;

          if Depth<2 then Depth := 2;
          if Assigned(AItem) then
            CreateTree(TTreeNode(AItem.Data),AItem,Depth);
        end;
      if Assigned(Item.Data)then
        begin
          TTreeNode(Item.Data).Focused:=True;
          if TreeView.AutoExpand then
            if not TTreeNode(Item.Data).Expanded then
              if TTreeNode(Item.Data).Count>0 then
                TTreeNode(Item.Data).Expand(False);
  //      TTreeNode(Item.Data).MakeVisible
        end;
    end;
  inherited;
end;

procedure TTreeForm.CacheSelectedChange(Item: TCacheItem);
begin
  if Assigned(Item)and Assigned(Item.Data) then
   begin
    TTreeNode(Item.Data).Selected:=True;
   // TTreeNode(Item.Data).MakeVisible;
  end;
  inherited;
end;

procedure TTreeForm.CacheSyncronize(var AFocusedItem : TCacheItem; AIndex : Integer = ltNone);
begin
  case AIndex of
    ltRoot : AFocusedItem:= RootItem;
    ltNone : AFocusedItem:= nil;
  end;
end;

procedure TTreeForm.TreeViewExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
var CacheItem : TCacheItem;
begin
  CreateTree(Node,TCacheItem(Node.Data));
  AllowExpansion:= True;

  if Assigned(Node) and Assigned(Node.Data) then
    begin
      CacheItem := TCacheItem(Node.Data);
      if Not CacheItem.Focused then
        begin
          Node.Focused:=True;
          CacheItem.Owner.ClearSelection;
          CacheItem.Selected := True;
          CacheItem.SetFocus;
        end;
    end;
end;

procedure TTreeForm.TreeViewCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  if Assigned(Node.Data)and(TCacheItem(Node.Data) is TRootItem) then AllowCollapse := False;
end;

//..............................................................................
procedure TTreeForm.act_Da_IconsBigExecute(Sender: TObject);
begin
  act_Da_IconsBig.Checked := True;
  TreeView.Images := DM.ilIcon32;
  TreeView.Indent:= TreeView.Images.Width;
end;

procedure TTreeForm.act_Da_IconsSmallExecute(Sender: TObject);
begin
  act_Da_IconsSmall.Checked := True;
  TreeView.Images:=DM.ilIcon16;
  TreeView.Indent:= TreeView.Images.Width;
end;

procedure TTreeForm.act_RenameExecute(Sender: TObject);
begin
  inherited;
  TreeView.Selected.EditText;
end;

procedure TTreeForm.act_Da_CollapseExecute(Sender: TObject);
begin
  TreeView.Selected.Collapse(False);
end;

procedure TTreeForm.act_Da_ExpandExecute(Sender: TObject);
begin
  TreeView.Selected.Expand(False);
end;

procedure TTreeForm.act_Da_CollapseAllExecute(Sender: TObject);
begin
  TreeView.Selected.TreeView.FullCollapse
end;

procedure TTreeForm.act_PropertiesExecute(Sender: TObject);
begin
  inherited;
  //
end;

procedure TTreeForm.TreeViewDblClick(Sender: TObject);
begin
  inherited;
//if Not Assigned(CacheItems.FocusedItem) then Exit;
//if (CacheItems.FocusedItem is TRootItem) then Exit;
  if ViewArea then HideViewArea(True)
              else ShowViewArea(True);
end;

procedure TTreeForm.DoDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var ScrollPosY, ZoneY: Integer;
begin
  ScrollPosY:=GetScrollPos(TreeView.Handle, SB_VERT);

  if TreeView.Height>240 then ZoneY:=60 else ZoneY:=TreeView.Height div 4;

  if Y<ZoneY then
    SendMessage(TreeView.Handle, WM_VSCROLL, SB_LINEUP, 0);

  if Y>TTreeView(Sender).Height-ZoneY then
    SendMessage(TreeView.Handle, WM_VSCROLL, SB_LINEDOWN, 0);

  if ScrollPosY <> GetScrollPos(TreeView.Handle, SB_VERT) then
    InvalidateRect(TreeView.Handle, nil, True);

  inherited DoDragOver(Sender, Source, X, Y, State, Accept);
end;

procedure TTreeForm.TreeViewContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var tmpNode: TTreeNode;
begin
  tmpNode := (Sender as TTreeView).GetNodeAt(MousePos.X, MousePos.Y);
  if tmpNode <> nil then
  TTreeView(Sender).Selected := tmpNode;
end;

function TTreeForm.GetPropertiesString: string;
begin
  Result := inherited GetPropertiesString;

  if TreeView.Images=DM.ilIcon32{DM.ImagesWindow} then Result:=Result+' TreeIcon=Big;';
end;

procedure TTreeForm.CacheItemInsert(Item: TCacheItem);
begin
  inherited;
  if Assigned(DataSetMeta.Cache.FocusedItem) then
    Item.ParentID:=DataSetMeta.Cache.FocusedItem.ID;
end;

procedure TTreeForm.TreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var Canv : TCanvas;
    Rect : TRect;
    P    : TPoint;
    CacheItem : TCacheItem;
    i,n,w,TextX,TextY: Integer;
    ImageIndex: Integer;
    s: string;
begin
  CacheItem := TCacheItem(Node.Data);

  inherited;
  Canv:=Sender.Canvas;

 case Stage of
    cdPreErase, cdPostErase, cdPostPaint:
    begin
      DefaultDraw := False;
    end;

    cdPrePaint:
    if Node.IsVisible then
    begin
      if Assigned(CacheItem) then
        Canv.Font.Color := CacheItem.FontColor
      else
        Canv.Font.Color := clBlack;
      if ([cdsHot, cdsSelected] * State <> []) then
        begin
          Canv.Font.Color := clBlack;
          if cdsSelected in State then begin Canv.Brush.Color := clActiveCaption; Canv.Font.Color := clCaptionText; end else
          if cdsHot      in State then       Canv.Brush.Color := clWindowSecond;
        end;

      Rect := Node.DisplayRect(True);
      P:=Rect.TopLeft;
      Rect.Left:= 1;//Rect.Left- Succ(TreeView.Images.Width);
      Rect.Right := Sender.BoundsRect.Right;
      Canvas.Lock;
      Canv.FillRect(Rect);

      TextX:= P.X + ((Rect.Height-FTextHeight) div 2) + 1;
      TextY:= P.Y + ((Rect.Height-FTextHeight) div 2);
      Canv.TextOut(TextX, TextY, Node.Text);

      if Assigned(TreeView) and Assigned(TreeView.Images) then
        begin
          ImageIndex := Node.ImageIndex;
          if Node.Expanded and (ImageIndex = DM.MapIconIndex(2)) then
            ImageIndex := DM.MapIconIndex(1);
          if (ImageIndex >= 0) and (ImageIndex < TreeView.Images.Count) then
            TreeView.Images.Draw(Canv, P.X - TreeView.Images.Width, P.Y, ImageIndex);
        end;

      CacheItem := TCacheItem(Node.Data);
      if Assigned(CacheItem) then
        for i:=1 to Pred(FColumnList.Count{FViewList.Count}) do
          begin
            Rect := Node.DisplayRect(False);
            P.X:=Header.Sections[i].Left;
            P.Y:=Rect.Top;
            Rect.Left := Header.Sections[i].Left;
            Rect.Right:= Header.Sections[i].Right;
            Canv.FillRect(Rect);

            n:= FColumnList[I].TextIndex;
            s:= ReduceCaption(CacheItem.FieldText(n), -255);

            if 0 < length(s) then
              begin
                if FColumnList[I].Field.Alignment = taRightJustify then
                  begin
                    w:= Canv.TextWidth(s);
                    if  (w > Rect.Width - 6) { and (CacheItem.Owner.Fields[n].DataType in NumericTypes)}
                      then Canv.TextOut(P.X+3, TextY, '###')
                      else Canv.TextOut(P.X+Rect.Width-3-w, TextY, s)
                  end
                else
                  Canv.TextOut(P.X+3, TextY, s);
              end;
          end;
      Canvas.Unlock;

      PaintImages := False;//True;
      DefaultDraw := False;
    end;

  end;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('TreeFormUnit unit initialization ...');
  {$ENDIF}
  RegisterClass(TTreeForm);
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('TreeFormUnit unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

