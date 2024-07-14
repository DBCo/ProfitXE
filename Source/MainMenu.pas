unit MainMenu;

interface

uses
  Types, Messages, Classes, Graphics, Controls, ActnList, Menus, Buttons, ExtCtrls, {ComCtrls, }Forms, {DB, }
  DeTypes, DeMeta, MainMenuList;

Const
  PageHeigth = 24;

type
  TMenuSection = class(TComponent)
  private
    FSectionNum  : integer;
    FMenuItem    : TContextMeta;
    FMainPanel   : TPanel;
    FButtonPanel : TPanel;
    FButton      : TSpeedButton;
    FMenuControl : TMenuCont;
    FAction      : TAction;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure ProcessGetImageIcon(Sender: TObject; var ImageIndex: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);  override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;  override;
    property SectionNum : integer read FSectionNum write FSectionNum;
    property MenuItem : TContextMeta read FMenuItem write FMenuItem;
    property MainPanel : TPanel read FMainPanel write FMainPanel;
    property ButtonPanel : TPanel read FButtonPanel write FButtonPanel;
    property Button : TSpeedButton read FButton write FButton;
    property MenuControl : TMenuCont read FMenuControl write FMenuControl;
    property Action : TAction read FAction write FAction;
  end;

  TForm_Da_MainMenu = class(TForm)
    AllPanel: TPanel;
    PopupMenu: TPopupMenu;
    MN_Da_Open: TMenuItem;
    N1: TMenuItem;
    MN_Da_MenuGroup: TMenuItem;
    MN_Da_Delete: TMenuItem;
    MN_Da_MenuItem: TMenuItem;
    MN_Da_Properties: TMenuItem;
    N2: TMenuItem;
    MN_Da_Close: TMenuItem;
    N3: TMenuItem;
    MN_Da_MenuOrder: TMenuItem;
    MN_Da_Create: TMenuItem;
    MN_Da_MenuSubItem: TMenuItem;
    MN_Da_MenuMove: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    MN_Dl_Separator: TMenuItem;
    N6: TMenuItem;
    MN_Da_Copy: TMenuItem;
    MN_Da_Cut: TMenuItem;
    MN_Da_Paste: TMenuItem;
    procedure WListClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormEndDock(Sender, Target: TObject; X, Y: Integer);
    Procedure DirectoriesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure MenuOpen(Sender: TObject);
    procedure ShiftItems(aMenuMeta: TContextMeta; aIndex: Integer);
    procedure MenuCreateGroup(Sender: TObject);
    procedure MenuCreateItem(Sender: TObject);
    procedure MenuCreateSubItem(Sender: TObject);
    procedure MenuCreateSep(Sender: TObject);
    procedure MenuDelete(Sender: TObject);
    procedure MenuProperties(Sender: TObject);
    procedure MenuSetupOrder(Sender: TObject);
    procedure MN_Da_MenuMoveClick(Sender: TObject);
    procedure FormStartDock(Sender: TObject;
      var DragObject: TDragDockObject);
    procedure FormHide(Sender: TObject);
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMFavoritesUpdate(var msg: TMessage); message DM_FAVORITES;
    procedure MN_Da_CopyClick(Sender: TObject);
    procedure MN_Da_CutClick(Sender: TObject);
    procedure MN_Da_PasteClick(Sender: TObject);
  private
    { private declarations }
    FDDO          : TDragDockObject;
    FSectionIndex : integer;
    FSections     : TList;
    FPopupControl : TControl;
    FPopupNode    : TContextMeta;
    function GetSection(const Index: Integer): TMenuSection;
    function FindSection(aMenuItem : TContextMeta) : TMenuSection;
    function AddSection( _MMeta: TContextMeta) : TMenuSection;
    function InsertSection(aIndex: Integer; _MMeta: TContextMeta): TMenuSection;
    procedure MoveSection(const aFromIndex, aToIndex: Integer);
    procedure DeleteSection(const aSectionIndex: Integer);
    procedure FormMove(var msg: TMessage); message WM_MOVE;
    procedure SelectSection(const aIndex : integer);
    procedure ProcessSectionChanges(aMenuNode : TContextMeta;
      aSection : TMenuSection);
    procedure ProcessChanges(aParentItem : TContextMeta);
    property Sections[const Index: Integer]: TMenuSection read GetSection;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    destructor Destroy;  override;
    procedure SavePosition;
  end;

var
    FirstShow    : Boolean = True;

implementation

uses Windows, SysUtils, Variants, Contnrs, Math, ToolWin, Dialogs,
     DeLog, Funcs, Dictionary, {DSMeta, }DataUnit, DeSettings, DataManager, DeMetadata, DataCacheUnit,
     Main, DSMeta, BaseGridFormUnit, UnitA, Security, ItemsOrder, HintForm;

const MenuOptPrefix = 'menu';
      ButtonPrefix  = 'btn';

{$R *.dfm}

{ TMenuSection }

function LastOrder(aMeta: TContextMeta): Integer;
var
  Index: Integer;
begin
  Result:= 0;
  if Assigned(aMeta) then
    for index:= 0 to Pred(aMeta.Count) do
      if Result < Succ(aMeta[Index].Order) then
        Result:= Succ(aMeta[Index].Order);
end;

constructor TMenuSection.Create(AOwner: TComponent);
var Icon: TIcon;
begin
  inherited;

    MainPanel:=TPanel.Create(self);       ButtonPanel:=TPanel.Create(self);
{   MainPanel.Parent:=AllPanel; {}        ButtonPanel.Parent:=MainPanel;
    MainPanel.BevelOuter:=bvNone;         ButtonPanel.BevelOuter:=bvNone;
    MainPanel.Align:=alTop;               ButtonPanel.Align:=alTop;
    MainPanel.Height:=PageHeigth;         ButtonPanel.Height:=PageHeigth;
    MainPanel.ParentColor:=True;          ButtonPanel.ParentFont:= True;
    MainPanel.FullRepaint:= false;        ButtonPanel.FullRepaint:= false;
    MainPanel.FreeNotification(self);     ButtonPanel.FreeNotification(Self);
    MainPanel.StyleElements:= [seFont];   ButtonPanel.StyleElements:= [seFont];

    Action:=TAction.Create(self);
    Action.FreeNotification(self);

    Button:= TSpeedButton.Create(self);
    Button.Parent:=ButtonPanel;
    Button.Action:= Action;
    Button.Align:=alClient;
    Button.ParentFont:= True;
    Button.Font.Style:= [fsBold];
    Button.Font.Color:= clBlack;
    Button.Margin:= 12;
    Button.StyleElements:= [seFont];

    Icon:=TIcon.Create;
    DM.ilIcon12.GetIcon( DM.MapIconIndex( EncodeIcon( 96,(TMenuSection(self).ComponentIndex mod 12)+4,0,0,0,0,0,0) ), Icon);
    Button.Glyph.Assign(Icon);
    Icon.Free;
    Button.FreeNotification(self);

    MenuControl:= TMenuCont.Create(self);
    MenuControl.Parent:=  MainPanel;
    MenuControl.Align:= alClient;
    MenuControl.Images1:= DM.ilIcon32;
    MenuControl.Images2:= DM.ilIcon16;
    MenuControl.Images3:= DM.ilIcon08;
    MenuControl.Images1H:= DM.ilIcon32H;
    MenuControl.Images2H:= DM.ilIcon16H;
    MenuControl.Images3H:= DM.ilIcon08H;
    MenuControl.OnGetImageIndex:= ProcessGetImageIcon;
    MenuControl.FreeNotification(self);
    MenuControl.Font.Assign(MainPanel.Font);
end;

destructor TMenuSection.Destroy;
begin
  FMainPanel.Free;
  FButtonPanel.Free;
  FButton.Free;
  FMenuControl.Free;
  FAction.Free;
  inherited;
end;

procedure TMenuSection.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
    if aComponent = FMainPanel then
      FMainPanel:= nil
    else if aComponent = FButtonPanel then
      FButtonPanel:= nil
    else if aComponent = FButton then
      FButton:= nil
    else if aComponent = FMenuControl then
      FMenuControl:= nil
    else if aComponent = FAction then
      FAction:= nil;
  inherited;
end;

procedure TMenuSection.WMMouseWheel(var Msg: TWMMouseWheel);
begin
end;

procedure TMenuSection.ProcessGetImageIcon(Sender: TObject; var ImageIndex: Integer);
begin
  ImageIndex:= DM.MapIconIndex(ImageIndex);
end;

{ TMenuForm }

//..........................................................................
procedure RecurseCreate(MMM: TContextMeta; aSection: TMenuSection; aLevel: Integer; aFather: TMenuOpt = nil);
var TMO : TMenuOpt;
      j : Integer;
begin
  for j:=0 to MMM.Count-1 do
    if (MMM[j].ItemType in [itGroup, itElement]) then
      if SecuritySystem.CheckPolicyMenu (MMM[j].Id, spSelect) then
        if (MMM[j].SubjectID = UserSession.ID) or (MMM[j].SubjectID<1) then
          begin
            TMO:=aSection.MenuControl.Add(aLevel);
            TMO.Father:= aFather;
            TMO.Data:= MMM[j];
            if (Length(Trim(MMM[j].Caption))=0) and (assigned(MMM[j].Dataset)) then
              TMO.Caption:= MMM[j].Dataset.Name
            else
              TMO.Caption:= MMM[j].Caption;
            TMO.Hint:= MMM[j].Hint;
            TMO.ImageIndex:= MMM[j].Ico;
            TMO.IsFlip:= (Not Assigned (MMM[j].Dataset) );

            if MMM[j].Count>0 then
              RecurseCreate(MMM[j], aSection, aLevel+1, TMO);
          end;
  aSection.MenuControl.RealignItems;
end;
//..........................................................................
procedure RecurseClear(aSection:TMenuSection);
begin
   while aSection.MenuControl.ItemCount>0 do
     aSection.MenuControl.Delete(aSection.MenuControl.Items[0]);
end;
//..........................................................................

constructor TForm_Da_MainMenu.Create(AOwner: TComponent);
var i   : Integer;
    TMS : TMenuSection;
begin
  FirstShow:= True;

  inherited Create(Application.MainForm);
  FSections:= TList.Create;
  FSectionIndex:= -1;
  FPopupControl:= nil;
  FPopupNode:= nil;

  if AOwner<>nil then
  begin
    ManualDock(TWinControl(AOwner));
    TWinControl(AOwner).Realign;
  end
  else
  begin
    Left:=Variables.AsInteger[RegMenuL];
    Top:=Variables.AsInteger[RegMenuT];
    Height:=Variables.AsInteger[RegMenuH];
    Width:=Variables.AsInteger[RegMenuW];
  end;

  for i:=0 to MainMenuMeta.Count-1 do
    if SecuritySystem.CheckPolicyMenu (MainMenuMeta[i].Id, spSelect) then
      begin
        TMS:= AddSection(MainMenuMeta[i]);
        RecurseCreate(MainMenuMeta[i], TMS, 1);

        if TMS.MenuControl.ItemCount > 0 then TMS.MenuControl.ItemIndex:= 0;
        TMS.MainPanel.Visible:= {(MainMenuMeta[i].Count>0)}true;  { показ пустых закладок }
        TMS.MenuControl.OnItemSelect:=WListClick;
      end;

  AllPanel.Visible:=True;
  if FSections.Count > 0 then
    DirectoriesClick(Sections[0].Button);
  UpdateWindow(Handle);
  MN_Da_Close.OnClick:= MForm.Go_Da_MainMenu.OnExecute;
  FirstShow:=False;
end;

destructor TForm_Da_MainMenu.Destroy;
begin
  FSections.Free;
  inherited Destroy;
end;

procedure TForm_Da_MainMenu.DirectoriesClick(Sender: TObject);
var Num : Integer;
begin
  if ( TSpeedButton(Sender).Owner is TMenuSection) then
    begin
      Num:=  (TMenuSection(TSpeedButton(Sender).Owner)).SectionNum;
      if (Num < 0) or (AllPanel.ControlCount <= Num) then Exit;
      SelectSection(Num);
    end;
end;

function TForm_Da_MainMenu.GetSection(const Index: Integer): TMenuSection;
begin
  result:= TMenuSection(FSections[Index]);
end;

function TForm_Da_MainMenu.FindSection(aMenuItem: TContextMeta) : TMenuSection;
var I : integer;
begin
  result:= nil;
  for I:= 0 to FSections.Count-1 do
    if Sections[I].MenuItem = aMenuItem then
    begin
      result:= Sections[I];
      break;
    end;
end;

function TForm_Da_MainMenu.AddSection( _MMeta: TContextMeta) : TMenuSection;
begin
  result:= InsertSection(FSections.Count, _MMeta);
end;

function TForm_Da_MainMenu.InsertSection(aIndex: Integer; _MMeta: TContextMeta): TMenuSection;
var I : integer;
begin
  if FSectionIndex >= aIndex then
    inc(FSectionIndex);
  for I:= Pred(FSections.Count) downto aIndex do
    Sections[I].SectionNum:= Succ(I);

  result:= TMenuSection.Create(Self);

  if aIndex >= FSections.Count then
    aIndex:= FSections.Add(result)
  else
    FSections.Insert(aIndex, result);

  with result do
  begin
    MenuItem:= _MMeta;

    MainPanel.Parent:=AllPanel;

    Action.Caption:=GetTitle(_MMeta.Name, ttSecondName);
    Action.HelpKeyword:=_MMeta.Name;
    Action.OnExecute:=DirectoriesClick;

    SectionNum:= aIndex;
  end;
end;

procedure TForm_Da_MainMenu.MoveSection(const aFromIndex, aToIndex: Integer);
var I : integer;
    ActiveSection : TMenuSection;
begin
  if FSectionIndex >= 0 then
    ActiveSection:= Sections[FSectionIndex]
  else
    ActiveSection:= nil;
  FSections.Move(aFromIndex, aToIndex);
  if FSectionIndex >= 0 then
    FSectionIndex:= FSections.IndexOf(ActiveSection);
  for I:= Min(aFromIndex, aToIndex) to Pred(FSections.Count) do
    Sections[I].SectionNum:= I;
end;

procedure TForm_Da_MainMenu.DeleteSection(const aSectionIndex: Integer);
var Section : TMenuSection;
    I : integer;
begin
  { удаление закладки }
  if aSectionIndex < FSections.Count then
  begin
    for I:= Succ(aSectionIndex) to Pred(FSections.Count) do
      Sections[I].SectionNum:= Pred(Sections[I].SectionNum);
    Section:= FSections.Extract(FSections[aSectionIndex]);
    Section.Free;
    if FSectionIndex > aSectionIndex then
      dec(FSectionIndex)
    else if FSectionIndex = aSectionIndex then
      FSectionIndex:= -1;
  end;
end;

procedure TForm_Da_MainMenu.FormResize(Sender: TObject);
begin
  if Not FirstShow then
  begin if Floating then
          begin Variables.AsInteger[RegMenuL]:=Left; Variables.AsInteger[RegMenuT]:=Top; Variables.AsInteger[RegMenuH]:=Height; end;
                Variables.AsInteger[RegMenuW]:=Width;
  end;
end;

procedure TForm_Da_MainMenu.FormMove(var msg: TMessage);
begin
  inherited;
  if (Not FirstShow) and (Floating) then
    begin Variables.AsInteger[RegMenuL]:=Left; Variables.AsInteger[RegMenuT]:=Top; end;
end;

procedure TForm_Da_MainMenu.SelectSection(const aIndex : integer);
var I, H, NextTop, BottomTop : integer;
begin
  if Visible and Enabled then SetFocus;
  { выбор новой группы меню }
  if (aIndex >= 0) and (aIndex < FSections.Count) then
  begin
    AllPanel.DisableAlign;
    NextTop:= 1;
    for I:= 0 to aIndex-1 do
    begin
      Sections[I].MainPanel.Top:= NextTop;
      Sections[I].MainPanel.Align:= alTop;
      inc(NextTop, Sections[I].MainPanel.Height);
    end;
    BottomTop:= AllPanel.ClientHeight-1;
    for I:= FSections.Count-1 downto aIndex+1 do
    begin
      Sections[I].MainPanel.Top:= BottomTop-Sections[I].MainPanel.Height{+1};
      Sections[I].MainPanel.Align:= alBottom;
      dec(BottomTop, Sections[I].MainPanel.Height);
    end;
    Sections[aIndex].MainPanel.Top:= NextTop;
    Sections[aIndex].MainPanel.Align:= alClient;
    AllPanel.EnableAlign;
    if (FSectionIndex >= 0) and (aIndex <> FSectionIndex) then
    begin
      Sections[FSectionIndex].MainPanel.DisableAlign;
      Sections[aIndex].MainPanel.DisableAlign;
      H:= Sections[FSectionIndex].MainPanel.Height;
      for i:=1 to Variables.AsInteger[RegAnim] do
        Sections[FSectionIndex].MainPanel.Height:=
          LiteResize(H, PageHeigth, Variables.AsInteger[RegAnim], I, 10);
      Sections[aIndex].MainPanel.EnableAlign;
      Sections[FSectionIndex].MainPanel.EnableAlign;
    end;
    for I:= 0 to FSections.Count-1 do
      Sections[I].MainPanel.Repaint;
   FSectionIndex:= aIndex;
  end;
end;

procedure TForm_Da_MainMenu.WListClick(Sender: TObject);
begin
  if TMenuCont(Sender).ItemIndex<>-1 then
    MForm.DoData(             TMenuCont(Sender).Items[TMenuCont(Sender).ItemIndex],
                 TContextMeta(TMenuCont(Sender).Items[TMenuCont(Sender).ItemIndex].Data) );
end;

procedure TForm_Da_MainMenu.FormEndDock(Sender, Target: TObject; X, Y: Integer);
var xy : TPoint;
    lt : TPoint;
begin
  if Floating then
    begin
      lt:=self.ClientToScreen(Point(0,0));
      xy:=self.ScreenToClient(Point(X,Y));
      Left:=(Left-lt.X)+(X-xy.X);
      Top:=(Top -lt.Y)+(Y-xy.Y);
    end;
  inherited;
end;

procedure TForm_Da_MainMenu.SavePosition;
begin
  if Assigned(Parent) then
    case Parent.Align of
      alLeft: Variables.AsInteger[RegMenuA]:= 2;
      alRight: Variables.AsInteger[RegMenuA]:= 3;
    end
  else
    Variables.AsInteger[RegMenuA]:= 1;
end;

procedure TForm_Da_MainMenu.FormClose(Sender: TObject; var Action: TCloseAction);
var B : Boolean;
begin
  SavePosition;
  if Parent<>nil then
    begin
      B:=False;
      TPanel(Parent).OnUnDock(Parent,self,nil,B);
    end;
end;

procedure TForm_Da_MainMenu.FormShow(Sender: TObject);
begin
  FDDO:=TDragDockObject.Create(self);
  FDDO.DockRect:=self.GetClientRect;

  if Parent<>nil then
    TPanel(Parent).OnDockDrop(Parent,FDDO,Left,Top);

  LangFormUpdate(Self);
  MForm.Go_Da_MainMenu.Checked:= Visible;
end;

procedure TForm_Da_MainMenu.FormHide(Sender: TObject);
begin
  MForm.Go_Da_MainMenu.Checked:= Visible;
end;

procedure TForm_Da_MainMenu.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDDO);
end;

procedure TForm_Da_MainMenu.PopupMenuPopup(Sender: TObject);
type
  TPopupZone = (pzNone, pzActiveSection, pzSection, pzFreeZone,
                                                           pzItem, pzSeparator);
var
  ParentControl  : TControl;
  ClientPoint    : TPoint;
  PopupZone      : TPopupZone;
  inUse          : Boolean;
  inRight        : Boolean;
begin
  if Visible and Enabled then SetFocus;

  { находим Control, находящийся на верхнем уровне иерархии }
  ClientPoint:= AllPanel.ScreenToClient(TPopupMenu(Sender).PopupPoint);
  FPopupControl:= AllPanel.ControlAtPos(ClientPoint, false, true);

  ParentControl:= nil;
  while Assigned(FPopupControl) and (FPopupControl is TWinControl) do
  begin
    ParentControl:= FPopupControl;
    ClientPoint:= FPopupControl.ScreenToClient(TPopupMenu(Sender).PopupPoint);
    FPopupControl:= TWinControl(FPopupControl).ControlAtPos(ClientPoint, false, true);
  end;
  if not Assigned(FPopupControl) then
    FPopupControl:= ParentControl;

  { определяем TContextMeta, соответствующую Control'у }
  if FPopupControl is TMenuOpt then
  begin
    FPopupNode:= TContextMeta(TMenuOpt(FPopupControl).Data);
    if FPopupNode.IsSeparator then
      PopupZone:=  pzSeparator
    else
      PopupZone:= pzItem;
  end
  else if FPopupControl is TSpeedButton then
  begin
    FPopupNode:= Sections[(TMenuSection(TSpeedButton(FPopupControl).Owner)).SectionNum].MenuItem;

    if (FSectionIndex >= 0) and (Sections[FSectionIndex].MenuItem = FPopupNode) then
      PopupZone:= pzActiveSection
    else
      PopupZone:= pzSection;
  end
  else if FPopupControl is TMenuCont then
  begin
    FPopupNode:= Sections[FSectionIndex].MenuItem;
    PopupZone:= pzFreeZone;
  end
  else
  begin
    FPopupNode:= nil;
    PopupZone:= pzNone;
  end;

  inUse:= (FPopupNode.SolutionID <> MetaSolution);
  inRight:= SecuritySystem.CheckPolicyDataSet(MetaData.MetaTables[idxMenu].ID, spUpdate);

  { модифицируем главное меню в соответствии с типом контрола }
  MN_Da_Open.Enabled:= (PopupZone in [pzSection, pzItem]) and
     ((PopupZone = pzSection) or
     (SecuritySystem.CheckPolicyDataSet(FPopupNode.DatasetID, spSelect) and
      SecuritySystem.CheckPolicyMenu   (FPopupNode.Id, spExecute))  );

  MN_Da_MenuGroup.Enabled:= InUse and inRight;
  MN_Da_MenuItem.Enabled:= InUse and inRight and (PopupZone in [pzItem, pzActiveSection, pzFreeZone]);
  MN_Da_MenuSubItem.Enabled:= InUse and inRight and (PopupZone in [pzItem]) ;
  MN_Dl_Separator.Enabled:= InUse and inRight and (PopupZone in [pzItem, pzFreeZone]);

  MN_Da_Delete.Visible:= PopupZone in [pzItem, pzSection, pzActiveSection, pzFreeZone, pzSeparator];
  MN_Da_Delete.Enabled:= Assigned(FPopupNode) and InUse and InRight and (PopupZone in [pzItem, pzSection, pzActiveSection, pzSeparator]);

  MN_Da_Copy.Visible:= MN_Da_Delete.Visible;
  MN_Da_Copy.Enabled:= Assigned(FPopupNode) and InRight and (PopupZone in [pzItem, pzSection, pzActiveSection, pzSeparator]);;

  MN_Da_Cut.Visible:= MN_Da_Delete.Visible;
  MN_Da_Cut.Enabled:= MN_Da_Delete.Enabled;

  MN_Da_Paste.Visible:= PopupZone in [pzItem, pzSection, pzActiveSection, pzSeparator, pzFreeZone];
  MN_Da_Paste.Enabled:= Assigned(FPopupNode) and InUse and InRight
                         and (PopupZone in [pzItem, pzSection, pzActiveSection, pzFreeZone])
                         and (DeClipboard.ContainsDeData)
                         and (DeClipboard.DataID = MetaData.GetSystemTableByName(tblMenu).ID);

  MN_Da_MenuOrder.Visible:= PopupZone in [pzFreeZone, pzItem, pzSection, pzActiveSection, pzSeparator];

  MN_Da_Properties.Visible:= PopupZone in [pzItem, pzSection, pzActiveSection, pzSeparator];
  MN_Da_Properties.Enabled:= PopupZone in [pzItem, pzSection, pzActiveSection];

  MN_Da_MenuMove.Visible:= PopupZone in [pzFreeZone, pzSection, pzActiveSection];
  MN_Da_MenuMove.Enabled:= MN_Da_MenuMove.Visible and Assigned(HostDockSite);

  MN_Da_Close.Visible:= PopupZone in [pzFreeZone, pzSection, pzActiveSection];
end;

procedure TForm_Da_MainMenu.MenuOpen(Sender: TObject);
begin

  if FPopupControl is TSpeedButton then
    DirectoriesClick(TSpeedButton(FPopupControl))
  else if FPopupControl is TMenuOpt then
          TMenuCont(TMenuOpt(FPopupControl).Owner).MenuOptClick(FPopupControl);
end;

procedure TForm_Da_MainMenu.ShiftItems(aMenuMeta: TContextMeta; aIndex: Integer);
var i      : Integer;
    DoShift: Boolean;
    DMan   : TMenuDataManager;
begin
  DoShift:=False;

  for i:=0 to aMenuMeta.Count-1 do
    if aMenuMeta.Items[i].Order=aIndex then
      begin
        DoShift:=True;
        Break;
      end;

  if DoShift then
    begin
      DMan:= TMenuDataManager.Create;

      for i:=0 to aMenuMeta.Count-1 do
        if aMenuMeta.Items[i].Order>=aIndex then
          begin
            aMenuMeta.Items[i].Order:=aMenuMeta.Items[i].Order+1;
            DMan.UpdateItem(aMenuMeta.Items[i]);
          end;

      DMan.Free;
    end;
end;

procedure TForm_Da_MainMenu.MenuCreateGroup(Sender: TObject);
var NewItem      : TContextMeta;
    RecordEditor : TRecordEditor;
    DMan         : TMenuDataManager;
    aGrandPopupNode : TContextMeta;
begin
  aGrandPopupNode:=FPopupNode;
  while (aGrandPopupNode.Level>1) and Assigned(aGrandPopupNode.Owner) do
    aGrandPopupNode:= TContextMeta(aGrandPopupNode.Owner);

  NewItem:= TContextMeta.Create(MainMenuMeta);
  NewItem.Name:= GetTitle('_Da.MenuGroup');
  NewItem.ItemType:= itGroup;
  NewItem.Order:=aGrandPopupNode.Order+1;
  NewItem.OwnerID:=0;

  { редактирование пользователем и сохранение новой записи в базе данных }
  RecordEditor:= TRecordEditor.Create(MetaData.MetaTables[idxMenu], Null);
  RecordEditor.Caption:= GetTitle('_Da.MenuGroup');
  DMan:= TMenuDataManager.Create;
  DMan.SetItem(RecordEditor.CacheItem, NewItem);

  if RecordEditor.Execute then
    begin
      ShiftItems(MainMenuMeta,NewItem.Order); // сдвигаем нижестоящие пункты
      newItem.Assign(RecordEditor.CacheItem); // сохраняем
      DMan.InsertItem(NewItem);
      MainMenuMeta.SortByOrder; // упорядочиваем с учетом нового
      ProcessChanges(MainMenuMeta); // отображаем визуальные изменения
    end;

  DMan.Free;
  RecordEditor.Free;
end;

procedure TForm_Da_MainMenu.MenuCreateSep(Sender: TObject);
var NewItem      : TContextMeta;
    DMan         : TMenuDataManager;
    aPopupNode   : TContextMeta;
begin
  if Not assigned(FPopupNode) then Exit;
  if Not assigned(FPopupNode.Owner) then Exit;

  DMan:= TMenuDataManager.Create;

  if FPopupNode.Level=1 then {создаем разделитель в пустом месте раздела}
    begin
      NewItem:= TContextMeta.Create(FPopupNode);
      NewItem.Order:= LastOrder(FPopupNode);
    end
  else
    begin  {создаем разделитель под пунктом меню}
      aPopupNode:=FPopupNode;
      while (aPopupNode.Level>2) and Assigned(aPopupNode.Owner) do
        aPopupNode:= TContextMeta(aPopupNode.Owner);

      NewItem:= TContextMeta.Create(aPopupNode.Owner);
      NewItem.Order:=aPopupNode.Order+1;
      ShiftItems(TContextMeta(aPopupNode.Owner),NewItem.Order);
    end;

  NewItem.ItemType:= itElement;
  NewItem.Name:= cLineCaption;
  if NewItem.OwnerID=1 then NewItem.SubjectID:=UserSession.ID;
  DMan.InsertItem(NewItem);

  TContextMeta(NewItem.Owner).SortByOrder; // упорядочиваем с учетом нового
  ProcessChanges(NewItem); // отображаем визуальные изменения

  DMan.Free;
end;

procedure TForm_Da_MainMenu.MenuCreateItem(Sender: TObject);
var NewItem      : TContextMeta;
    DMan         : TMenuDataManager;
    RecordEditor : TRecordEditor;
    i, j: Integer;
    s: String;
begin
  if Not assigned(FPopupNode) then Exit;
  if Not assigned(FPopupNode.Owner) then Exit;

  if FPopupNode.Owner.ID=unassigned then NewItem:= TContextMeta.Create(FPopupNode)
                                    else NewItem:= TContextMeta.Create(FPopupNode.Owner);

  if Assigned(MForm.ActiveMeta) then
    begin
      s:= EmptyStr;
      if MForm.ActiveMeta.Role <> drMain then
        for i:= 0 to Pred(MForm.ActiveMeta.Table.Fields.Count) do
         if not MForm.ActiveMeta.Table.Fields[i].IsLookup then
           for j:= 0 to Pred(MForm.ActiveMeta.OwnerLinks.Count) do
             if MForm.ActiveMeta.Table.Fields[i].Link = MForm.ActiveMeta.OwnerLinks[j].DataSetMeta.Table.ID then
               if length(s) = 0 then s:= MForm.ActiveMeta.Table.Fields[i].Original
                                else s:= MForm.ActiveMeta.Table.Fields[i].Original + '|' + s;
      if 0 < Length(s) then s:=' GroupBy='+s+';';

      NewItem.Assign(MForm.ActiveMeta.Context);
      NewItem.ID:= unassigned;
      NewItem.ViewParams:= TBaseGridForm(MForm.ActiveMeta.GridForm).GetPropertiesString + s;
    end;

  NewItem.Order:= 1;
  for i:=0 to Pred(NewItem.Owner.Count) do
    if NewItem.Order <= TContextMeta(NewItem.Owner.Items[i]).Order then
      NewItem.Order:= TContextMeta(NewItem.Owner.Items[i]).Order + 1;

  NewItem.ItemType:= itElement;
  if NewItem.OwnerID = 1 then NewItem.SubjectID:= UserSession.ID;

  { редактирование пользователем и сохранение новой записи в базе данных }
  RecordEditor:= TRecordEditor.Create(MetaData.MetaTables[idxMenu], Null);
  DMan:= TMenuDataManager.Create;
  DMan.SetItem(RecordEditor.CacheItem, NewItem);
  RecordEditor.Caption:= GetTitle('_Da.MenuItem');

  if RecordEditor.Execute then
    begin
      ShiftItems(TContextMeta(NewItem.Owner),NewItem.Order);
      newItem.Assign(RecordEditor.CacheItem); // сохраняем
      DMan.InsertItem(NewItem);
      TContextMeta(NewItem.Owner).SortByOrder; // упорядочиваем с учетом нового
      ProcessChanges(NewItem); // отображаем визуальные изменения
    end;

  DMan.Free;
  RecordEditor.Free;
end;

procedure TForm_Da_MainMenu.MenuCreateSubItem(Sender: TObject);
var NewItem      : TContextMeta;
    DMan         : TMenuDataManager;
    RecordEditor : TRecordEditor;
begin
  if Not assigned(FPopupNode) then Exit;
  if Not assigned(FPopupNode.Owner) then Exit;

  NewItem:= TContextMeta.Create(FPopupNode);
  if FPopupNode.Count=0 then NewItem.Order:=1
                        else NewItem.Order:=FPopupNode.Items[FPopupNode.Count-1].Order+1;
  NewItem.ItemType:= itElement;
  if NewItem.OwnerID=1 then NewItem.SubjectID:=UserSession.ID;

  { редактирование пользователем и сохранение новой записи в базе данных }
  RecordEditor:= TRecordEditor.Create(MetaData.MetaTables[idxMenu], Null);
  DMan:= TMenuDataManager.Create;
  DMan.SetItem(RecordEditor.CacheItem, NewItem);
  RecordEditor.Caption:= GetTitle('_Da.MenuItem');

  if RecordEditor.Execute then
    begin
      ShiftItems(TContextMeta(NewItem.Owner),NewItem.Order);
      newItem.Assign(RecordEditor.CacheItem); // сохраняем
      DMan.InsertItem(NewItem);
      TContextMeta(NewItem.Owner).SortByOrder; // упорядочиваем с учетом нового
      ProcessChanges(NewItem); // отображаем визуальные изменения
    end;

  DMan.Free;
  RecordEditor.Free;
end;

procedure TForm_Da_MainMenu.WMMouseWheel(var Msg: TWMMouseWheel);
begin
  SendMessage(Sections[FSectionIndex].MenuControl.Handle,msg.Msg, TMessage(msg).WParam,TMessage(msg).LParam);
end;

procedure TForm_Da_MainMenu.ProcessSectionChanges(aMenuNode : TContextMeta; aSection : TMenuSection);
var Nodes            : TList;
    MenuOpt, NewOpt  : TMenuOpt;
    NextOption, i,j  : integer;
begin
  { определяем элемент, с которого начинаем проход }
  Nodes:= TList.Create;
  aSection.MenuItem.ConvertToList(Nodes);

 { проход, поиск и удаление отсутствующих элементов на странице }
  with aSection.MenuControl do
    for I:= 0 to ItemCount-1 do
      if not aSection.MenuItem.NodeFound(TContextMeta(Items[I].Data)) then
        Delete(Items[I]);

  { проход по списку и приведение одного списка в соответствие с другим }
  NextOption:= 0;
  for I:= 0 to Nodes.Count-1 do
    begin

      MenuOpt:= nil;
      for j:= 0 to aSection.MenuControl.ItemCount-1 do
        if aSection.MenuControl.Items[j].Data = Nodes[I] then
          begin
            MenuOpt:= aSection.MenuControl.Items[j];
            Break
          end;

      if TContextMeta(Nodes[I]).Changed = mcDelete then
        begin
          if Assigned(MenuOpt) then
            aSection.MenuControl.Delete(aSection.MenuControl.Items[MenuOpt.ItemNum]);
        end
      else
        begin
          if Assigned(MenuOpt) then
            begin
              if MenuOpt.ItemNum <> NextOption then
                aSection.MenuControl.Move(MenuOpt.ItemNum, NextOption);
            end
          else
            begin
              NewOpt:= aSection.MenuControl.Insert(NextOption, TContextMeta(Nodes[I]).Level-1);
              NewOpt.Data:= TContextMeta(Nodes[I]);
              NewOpt.Caption:= TContextMeta(Nodes[I]).Caption;
              NewOpt.Hint:= TContextMeta(Nodes[I]).Hint;
              NewOpt.ImageIndex:= TContextMeta(Nodes[I]).Ico;
            end;
          inc(NextOption);
        end;
    end;
  Nodes.Free;
end;

procedure TForm_Da_MainMenu.ProcessChanges(aParentItem : TContextMeta);
var StartSection, FinishSection, NextSection, I : integer;
    MenuNode{, ItemOwner} : TContextMeta;
    Section : TMenuSection;
begin
  // проходим древовидную структуру меню и приводим визуальную часть
  // в соответствие с метаструктурой
  if aParentItem = MainMenuMeta then
  begin
    { перебираем все группы }
    StartSection:= 0;
    FinishSection:= aParentItem.Count-1;
  end
  else
  begin
    { определяем группу, к которой относится элемент }
    MenuNode:= aParentItem;
    while Assigned(MenuNode.Owner) and (MenuNode.Owner <> MainMenuMeta) do
      MenuNode:= TContextMeta(MenuNode.Owner);
    StartSection:= MainMenuMeta.IndexOf(MenuNode);
    FinishSection:= StartSection;
  end;

  NextSection:= StartSection;
  for I:= StartSection to FinishSection do
  begin
    Section:= FindSection(MainMenuMeta[I]);
    if MainMenuMeta[I].Changed = mcDelete then
    begin
      if Assigned(Section) then
      begin
        { удаление группы }
        DeleteSection(NextSection);
        if FSectionIndex < 0 then  { удалили активную закладку }
          if NextSection < FSections.Count then
            SelectSection(NextSection)
          else if NextSection > 0 then
            SelectSection(NextSection-1)
          else
            SelectSection(-1)
        else
          SendMessage(Sections[FSectionIndex].MenuControl.Handle, WM_ERASEBKGND, 0, 0);
      end;
    end
    else
    begin
      if Assigned(Section) then
      begin
        { перемещение группы }
        if FSections.IndexOf(Section) <> NextSection then
        begin
          MoveSection(FSections.IndexOf(Section), NextSection);
          SelectSection(FSectionIndex);
        end;
      end
      else
      begin
        { добавление группы }
        InsertSection(NextSection, MainMenuMeta[I]).MenuControl.OnItemSelect:= WListClick;
        if FSections.Count = 1 then
          SelectSection(0)
        else
          SelectSection(FSectionIndex);
      end;
      if aParentItem = MainMenuMeta then
        ProcessSectionChanges(MainMenuMeta[I], Sections[NextSection])
      else
        ProcessSectionChanges(aParentItem, Sections[NextSection]);
      inc(NextSection);
    end;
  end;
end;

procedure TForm_Da_MainMenu.MenuDelete(Sender: TObject);
var I           : integer;
    Queue       : TQueue;
begin
  { запрос на удаление пункта меню }
  if Assigned(FPopupNode) and ((FPopupNode.Name='-') or
     (Application.MessageBox(
      pChar(Format(GetTitle('_qUerydelete'), [GetTitle(FPopupNode.Name)])),
      pChar(GetTitle('_Dl.Confirmation')),
      MB_YESNO or MB_ICONQUESTION) = idYes)) then
  begin
    { удаление разрешено }
    if FPopupNode.DeleteMenu then
    begin
      { помечаем флагом удаленные пункты меню }
      Queue:= TQueue.Create;
      Queue.Push(FPopupNode);
      while Queue.Count > 0 do
      begin
        for I:= 0 to TContextMeta(Queue.Peek).Count-1 do
          Queue.Push(TContextMeta(Queue.Peek)[I]);
        TContextMeta(Queue.Pop).Changed:= mcDelete;
      end;
      Queue.Free;
      { обрабатываем визуальные изменения }
      ProcessChanges(TContextMeta(FPopupNode.Owner));
      { удаляем пункт меню }
      FPopupNode.Free;
    end;
  end;
end;

procedure TForm_Da_MainMenu.MN_Da_CopyClick(Sender: TObject);
var aCache : TDataCache;
    aItem  : TCacheItem;
begin
  if Assigned(FPopupNode) then
    begin
      aCache:= TDataCache.Create(MetaData.MetaTables[idxMenu]);
   //   aCache.PrepareData(False);
      aItem:= aCache.AddNewItem;
      FPopupNode.SaveTo(aItem);
      aCache.SelectAll;
      aCache.CopyToClipboard;
      aCache.Free;
    end;
end;

procedure TForm_Da_MainMenu.MN_Da_CutClick(Sender: TObject);
begin
  MN_Da_CopyClick(Sender);
  MenuDelete(Sender);
end;

procedure TForm_Da_MainMenu.MN_Da_PasteClick(Sender: TObject);
var NewItem  : TContextMeta;
    DMan    : TMenuDataManager;
    aCache  : TDataCache;
    i : Integer;
begin
  if Not assigned(FPopupNode) then Exit;

  aCache:= TDataCache.Create(MetaData.MetaTables[idxMenu]);
  DMan:= TMenuDataManager.Create;
  aCache.PasteRecords;
  for i:=0 to aCache.Count-1 do
    begin
      NewItem:= TContextMeta.Create(FPopupNode);
      NewItem.Assign(aCache.Items[i]);
      NewItem.OwnerID:= FPopupNode.ID;
      if FPopupNode.Count=0 then NewItem.Order:=1
                            else NewItem.Order:=FPopupNode.Items[FPopupNode.Count-1].Order+1;
      DMan.UpdateItem(NewItem);
    end;

  TContextMeta(FPopupNode).SortByOrder; // упорядочиваем с учетом нового
  ProcessChanges(FPopupNode); // отображаем визуальные изменения

  DMan.Free;
  aCache.Free;
end;

procedure TForm_Da_MainMenu.WMFavoritesUpdate(var msg: TMessage);
var i   : Integer;
    TMO : TMenuOpt;
    MMM : TContextMeta;
begin
  { обрабатываем визуальные изменения }

  for i:=0 to FSections.Count-1 do
    if Sections[i].MenuItem.Id=1 then
      begin
        MMM:= TContextMeta(msg.WParam);
        MMM.Owner:= Sections[i].FMenuItem;

        TMO:=Sections[i].MenuControl.Add(1);
        TMO.Data:= MMM;
        TMO.Caption:= MMM.Caption;
        TMO.Hint:= MMM.Hint;
        TMO.ImageIndex:= MMM.Ico;
      end;
end;

procedure TForm_Da_MainMenu.MenuProperties(Sender: TObject);
var ChangesRootNode : TContextMeta;
    RecordEditor  : TRecordEditor;
begin
  { просмотр свойств пункта меню }
  if Assigned(FPopupNode) then
  begin
    RecordEditor:= TRecordEditor.Create(MetaData.MetaTables[idxMenu], FPopupNode.ID);
    RecordEditor.Caption:= GetTitle('_Dl.Properties ')+' '''+ GetTitle(MetaData.MetaTables[idxMenu].Name)+'''';
    if RecordEditor.Execute then
    begin
      FPopupNode.Assign(RecordEditor.CacheItem);
      ChangesRootNode:= TContextMeta(FPopupNode.Owner);
      if Assigned(FPopupNode.Owner) then
        if (not VarSameValue(FPopupNode.Owner.Id, FPopupNode.OwnerID)) then
        begin
          FPopupNode.Owner.Extract(FPopupNode);
          if VarToInt(FPopupNode.OwnerID) = 0 then
            MainMenuMeta.Add(FPopupNode)
          else
            MainMenuMeta.FindNode(FPopupNode.OwnerID).Add(FPopupNode);
          ChangesRootNode:= MainMenuMeta;
        end;
      if (FPopupControl) is TMenuOpt then
        begin
          if (FPopupNode.Ico<0) and (assigned(FPopupNode.Dataset))
            then TMenuOpt(FPopupControl).ImageIndex:= FPopupNode.Dataset.Ico
            else TMenuOpt(FPopupControl).ImageIndex:= FPopupNode.Ico;

          if (Length(Trim(FPopupNode.Caption))=0) and (assigned(FPopupNode.Dataset))
            then TMenuOpt(FPopupControl).Caption:= FPopupNode.Dataset.Name
            else TMenuOpt(FPopupControl).Caption:= FPopupNode.Caption;

          TMenuOpt(FPopupControl).Hint:= FPopupNode.Hint;
        end
      else
        TAction(TSpeedButton(FPopupControl).Action).Caption:= GetTitle(FPopupNode.Name, ttSecondName);
        
      if Assigned(FPopupNode.Owner) then
      begin
        TContextMeta(FPopupNode.Owner).SortByOrder;
        ProcessChanges(ChangesRootNode);
      end;
    end;
    RecordEditor.Free;
  end;
end;

procedure TForm_Da_MainMenu.MenuSetupOrder(Sender: TObject);
var MenuOwner : TContextMeta;
    OrderForm : TItemsOrderForm;
    I, MenuIndex : integer;
    Deleted : string;
begin
  if Assigned(FPopupNode) then
    MenuOwner:=TContextMeta(FPopupNode.Owner)
  else
    MenuOwner:=nil;

  if Assigned(MenuOwner) then
  begin
    OrderForm:= TItemsOrderForm.Create(Application);
    for I:= 0 to MenuOwner.Count-1 do
      OrderForm.AddItem(
        GetTitle(TContextMeta(MenuOwner[I]).Name, ttSecondName), TContextMeta(MenuOwner[I]), true, true);
    OrderForm.ReInitForm(nil);
    if MenuOwner.Level > 0 then
      begin
        OrderForm.Caption:= GetTitle('_Di.menuelem',ttSecondName);
        OrderForm.Title:= Format(GetTitle('_dM.Sequence'), [GetTitle('_di.menuelem',ttSecondName)]);
      end
    else
      begin
        OrderForm.Caption:= GetTitle('_Dt.menu',ttSecondName);
        OrderForm.Title:= Format(GetTitle('_dM.Sequence'), [GetTitle('_dt.menu',ttSecondName)]);
      end;

    if OrderForm.ShowModal = mrOK then
    begin
      Deleted:= EmptyStr;
      for I:= 0 to MenuOwner.Count-1 do
      begin
        if not OrderForm.Checked[I] then
        begin
          MenuIndex:= MenuOwner.IndexOf(OrderForm.Data[I]);
          StrAdd(Deleted, #13#10, GetTitle(MenuOwner[MenuIndex].Name, ttSecondName));
        end;
      end;
      if (Deleted = EmptyStr) or
         (Application.MessageBox( pChar(GetTitle('_dM.queryitemdelete') + #13#10#13#10 + Deleted),
                                  pChar(GetTitle('_Dl.Confirmation')), MB_YESNO or MB_ICONQUESTION) = idYes) then
        begin
          for I:= 0 to MenuOwner.Count-1 do
          begin
            MenuIndex:= MenuOwner.IndexOf(OrderForm.Data[I]);
            if not OrderForm.Checked[I] then
              TContextMeta(MenuOwner[MenuIndex]).SetChanged(mcDelete)
            else if TContextMeta(MenuOwner[MenuIndex]).Order <> I then
              begin
                TContextMeta(MenuOwner[MenuIndex]).Order:= I;
                TContextMeta(MenuOwner[MenuIndex]).Changed:= mcUpdate;
              end;
          end;
          TContextMeta(MenuOwner).SortByOrder;
          ProcessChanges(TContextMeta(MenuOwner));
          MainMenuMeta.SaveChanges;
        end;
    end;
    OrderForm.Free;
  end;
end;

procedure TForm_Da_MainMenu.MN_Da_MenuMoveClick(Sender: TObject);
var LeftTop, RightBottom : TPoint;
begin
  LeftTop:= HostDockSite.ClientToScreen(Point(Left, Top));
  RightBottom:= Point(LeftTop.X + Width - 1, LeftTop.Y + Height - 1);
  ManualFloat(Rect(LeftTop, RightBottom));
end;

procedure TForm_Da_MainMenu.FormStartDock(Sender: TObject; var DragObject: TDragDockObject);
begin
  DragObject:= TToolDockObject.Create(TControl(Sender));
end;

{$IFDEF DEBUG}
initialization
  DebugLog('MainMenu unit initialization ...');

finalization
  DebugLog('MainMenu unit finalization ...');
{$ENDIF}

end.

