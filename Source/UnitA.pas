unit UnitA;

interface

uses
  Messages, SysUtils, Contnrs, Classes, Controls, Forms, ActnList, Actions, StdCtrls, System.Generics.Collections,
  ExtCtrls, ComCtrls, Menus, Graphics, Types, Buttons, ToolWin, AnchorRuler,
  BaseFormUnit, Element, DeMeta, BaseCardFormUnit, DataCacheUnit, ElementsMeta, DSMeta, DeRight;

const
  WM_SELECTPAGE   = WM_USER + 500;
  TCM_GETITEMRECT = $130A;
  SpaseXY = 8;

type

  TEndEditAction = (
    smNo      = 0,  // Без действий
    smSave    = 1,  // Сохранение изменений
    smUndo    = 2,  // Откат изменений
    smQuery   = 3   // Запрос на сохранение
  );

  function EditRecord(TableMeta: TTableMeta; const ItemID: Variant): Boolean;

type
  TAForm = class(TBaseCardForm)
    DesignMenu: TPopupMenu;
    DM_Dl_Properties: TMenuItem;
    DMSave: TMenuItem;
    N57: TMenuItem;
    DM_Da_Delete: TMenuItem;
    DM_Da_Create: TMenuItem;
    GoSettings: TMenuItem;
    ActionList1: TActionList;
    Go_Ok: TAction;
    Go_Da_Cancel: TAction;
    Go_Da_DesignSettings: TAction;
    N1: TMenuItem;
    DM_Dl_Forms: TMenuItem;
    DF_Da_Create: TMenuItem;
    DF_Da_Delete: TMenuItem;
    N2: TMenuItem;
    UA_Da_Action: TAction;
    N3: TMenuItem;
    DM_Da_Go: TMenuItem;
    DM_Da_Hide: TMenuItem;
    DM_Dl_TabPages: TMenuItem;
    N5: TMenuItem;
    DM_Da_MoveTo: TMenuItem;
    PropPanel: TPanel;
    _PageControl: TPageControl;
    N6: TMenuItem;
    DMUndo: TMenuItem;
    ActionMenu: TPopupMenu;
    Go_Da_Undo: TAction;
    Go_Da_Save: TAction;
    Go_Da_AutoAnchor: TAction;
    DesignAuto1: TMenuItem;
    BtnPanel: TPanel;
    ToolBarL: TToolBar;
    ActionBtn: TToolButton;
    ToolBarR: TToolBar;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ToolButton3: TToolButton;
    Panel2: TPanel;
    Go_Da_ShowCardArea: TAction;
    CloseBtnPanel: TToolBar;
    ToolButton6: TToolButton;
    Panel3: TPanel;
    ToolButton1: TToolButton;
    procedure Go_Da_CancelExecute(Sender: TObject);
    procedure Go_OkExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditAllChange(Sender: TObject);
    procedure DesignPrepare(Sender: TObject; LockControls: Boolean);
    procedure Go_SettingsExit(aEditAction: TEndEditAction);
    procedure Go_Da_DesignSettingsExecute(Sender: TObject);
    procedure DM_Dl_PropertiesClick(Sender: TObject);
    procedure DM_Da_DeleteClick(Sender: TObject);
    procedure DM_Da_CreateClick(Sender: TObject);
    procedure CreateFieldControlExecute(Sender: TObject);
    procedure DF_Da_CreateClick(Sender: TObject);
    procedure DF_Da_DeleteClick(Sender: TObject);
    procedure DesignMenuPopup(Sender: TObject);
    procedure SelectNewForm(Sender : TObject);
    procedure BaseActionsExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure _PageControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure _PageControlMouseDown(Sender: TObject;  Button : TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure _PageControlResize(Sender: TObject);
    procedure PropPanelContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure UA_Da_ActionExecute(Sender: TObject);
    procedure SelectPage(Sender: TObject);
    procedure OnHidePage(Sender: TObject);
    procedure DM_Dl_TabPagesClick(Sender: TObject);
    procedure CloseAreaBtnClick(Sender: TObject);
    procedure SelectTabSheet(var Msg : TWMMouse);  message WM_SELECTPAGE;
    procedure Go_Da_SaveExecute(Sender: TObject);
    procedure Go_Da_UndoExecute(Sender: TObject);
    procedure _PageControlDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure _PageControlDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure Go_Da_AutoAnchorExecute(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure PropPanelResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure _PageControlChange(Sender: TObject);
    procedure ActionMenuPopup(Sender: TObject);
    procedure act_Da_UpdateExecute(Sender: TObject);
  private
    { Private declarations }
    FPopupPos        : TPoint;
    FFirstUpdate     : boolean;
    FCacheItem       : TCacheItem;
    FFormDispatcher  : TDeFormDispatcher;
    FNewEnabled      : boolean;
    FSizingRect      : TSizingRect;
    FAnchorRuler     : TAnchorRuler;
    FOldPageControlHeight: Integer;
    FOldPageControlWidth: Integer;
    FCheckPageControlResize: Boolean;
    FShowShortcuts : Boolean;
    FInDesign : Boolean;
    FInPage : Integer;
    FFormRight   : TDeUserRight;
    procedure SetCacheItem(aItem : TCacheItem);
    function AddPage(const aTableID : integer;  aLinkField : TFieldMeta;
      const aPageName : string = '';  aPageIndex : integer = -1) : TDeElement;
    procedure RemovePage(const aPage : TElementMeta);
    procedure MoveToPageClick(Sender : TObject);
    function GetModified: Boolean;
    function GetTrueTabIndex(aTabIndex : integer) : integer;
    procedure    WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure    WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  protected
    procedure InitForm;  override;
    procedure OnAnchorChange(Sender : TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure ReCreateView; override;
    property CacheItem : TCacheItem read FCacheItem write SetCacheItem;
    property SizingRect: TSizingRect read FSizingRect;
    property AnchorRuler: TAnchorRuler read FAnchorRuler;
    property Modified : Boolean  read GetModified;
    procedure CheckDesignModeClosed;
    procedure ReloadImages;
    property InDesign: Boolean read FInDesign;
    function  CanClose: boolean;
    function FindByElement(const aElement : TElementMeta) : TDeElement ;
  end;

  { класс для редактирования записи }
  TRecordEditor = class
  private
    FDataSetMeta : TDataSetMeta;
    function GetFocusedItem: TCacheItem;
    function GetCaption: String;
    procedure SetCaption(const aCaption : string);
  public
    constructor Create(const aTable: TTableMeta; const aRecordID: Variant);
    destructor Destroy;  override;
    function Execute : boolean;
    property DataSetMeta: TDataSetMeta read FDataSetMeta;
    property CacheItem : TCacheItem read GetFocusedItem;
    property Caption : string read GetCaption write SetCaption;
  end;

implementation

uses Variants, {DB, }Windows, Dialogs, Math,
     DeLog, DeTypes, Dictionary, DeMetadata, Security, Funcs, DataManager, DataUnit,
     HintForm, BaseDataFormUnit, BaseGridFormUnit, DeSettings, Main,
     ItemsOrder, DeControls, DeParser, DeActions;

{$R *.dfm}

const
  MinPageControlHeight = 60;

{ TAForm }

procedure TAForm.Go_Da_CancelExecute(Sender: TObject);
begin
  if Parent=nil then
    ModalResult:=mrCancel
  else
  begin
    CacheItem.Restore;
    SetCacheItem(CacheItem);
  end;
end;

procedure TAForm.Go_OkExecute(Sender: TObject);
const ImageID : array [boolean] of integer = (icoError, icoInfo);
var DlgResult, i, j, N, ErrorCount : integer;
    FormElement : TDeElement;
    StoreOk, NeedUpdateParent, Warnings, Hidden : boolean;
    DMes        : TDeMessage;
    DMan        : TDataManager;
    EL          : TDeElementsList;
    FilterItem  : TFilterItem;

    IsDouble      : Boolean;
    CI            : TCacheItem;
    ChangedList   : TList<TDeElement>;
    ChangedUnique : TList<TDeElement>;
    ChangedStage  : TFieldStage;

  function NameList(aList: TList<TDeElement>): String;
  var i: Integer;
  begin
    Result:= EmptyStr;
    for i:=0 to Pred(aList.Count) do
      if i = 0 then Result:= aList[i].ElementMeta.Field.Native
               else Result:= Result+'; '+aList[i].ElementMeta.Field.Native;
  end;

begin
  //------ определеяем измененые поля у нескольких выделенных записей ---
  if DataSetMeta.Cache.SelectedCount > 1 then
    begin
      EL:= FFormDispatcher.AllElements;
      ChangedList:= TList<TDeElement>.Create;
      ChangedUnique:= TList<TDeElement>.Create;

      for i:=Pred(EL.Count) downto 0 do
        if EL[i].UserModified or EL[i].UserAccessed then
          if assigned(EL[i].ElementMeta) then
            if assigned(EL[i].ElementMeta.Field) then
              if (EL[i].ElementMeta.Field.IsStored) and (Not EL[i].ElementMeta.Field.IsReadOnly) then
                begin
                  IsDouble:= False;
                  for j:=0 to Pred(ChangedList.Count) do
                    if ChangedList[j].ElementMeta.FieldID = EL[i].ElementMeta.FieldID then IsDouble:= True;
                  for j:=0 to Pred(ChangedUnique.Count) do
                    if ChangedUnique[j].ElementMeta.FieldID = EL[i].ElementMeta.FieldID then IsDouble:= True;

                  if Not IsDouble then
                    if Not EL[i].ElementMeta.Field.Unique then ChangedList.Add(EL[i])
                                                          else ChangedUnique.Add(EL[i]);
                end;

      if 0 < (ChangedList.Count + ChangedUnique.Count) then
        begin
          if 0 < ChangedList.Count
            then  DlgResult:= Application.MessageBox(
                  PChar('Да'+#9+'Измененить все поля только для текущей записи'+#10+
                        'Нет'+#9+'Измененить поля ['+NameList(ChangedList)+'] нескольких выделенных записей'+#10+
                        'Отмена'+#9+'Отказаться от изменений'),
                  PChar(GetTitle('_dL.confirmation')), MB_YESNOCANCEL or MB_ICONQUESTION)
            else  DlgResult:= Application.MessageBox(
                  PChar('Измененить все поля только для текущей записи?'),
                  PChar(GetTitle('_dL.confirmation')), MB_OKCANCEL or MB_ICONQUESTION);
          case DlgResult of
            IDCancel:
              begin
                Exit;
              end;
            IDNo:
              begin
                ErrorCount:=0;
                SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, CacheItem.Owner.SelectedCount, 1);
                try
                  CacheItem.Owner.BeginUpdate;
                  try
                    // пределяем максимальный stage, чтобы перед изменением начитать необходимые поля
                    ChangedStage:= fsKey;
                    for j:=0 to Pred(CacheItem.Owner.Fields.Count) do
                      if ChangedStage < CacheItem.Owner.Fields[j].Stage
                        then ChangedStage := CacheItem.Owner.Fields[j].Stage;

                    for i := 0 to Pred(CacheItem.Owner.SelectedCount) do
                      begin
                        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, i, 0);
                        CI := CacheItem.Owner.SelectedItems[i];

                        // дочитываем нужный уровень stage, т.к. часть записей может быть начитано с меньшим уровнем
                        if CI.Stage < ChangedStage then
                           CI.Owner.FillItem(CI, ChangedStage);

                        // меняем поля
                        for j:=0 to Pred(ChangedList.Count) do
                          begin
                            N:= CacheItem.Owner.Fields.IndexByID(ChangedList[j].ElementMeta.FieldID);
                            CI.FieldNativeValue[N] := ChangedList[j].Value;
                         end;

                        if not CacheItem.Owner.DataManager.CanUpdateRecord(CI) or
                           not CacheItem.Owner.DataManager.UpdateRecord(CI) then
                           begin
                             Inc(ErrorCount);
                             CI.Owner.FillItem(CI, CI.Stage);
                           end;
                      end;
                  finally
                    CacheItem.Owner.EndUpdate;
                  end;
                finally
                  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, -1, -1);
                end;

                if ErrorCount>0 then
                  SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo,
                    NativeInt(PChar(GetTitle('_iMp.resultskip')+' - '+IntToStr(ErrorCount))));

                DataSetMeta.Cache.Update(mcUpdate, Null);
                Exit;
              end;
            end;
        end;

      ChangedList.Free;
      ChangedUnique.Free;
    end;
  //----------------------------------------------------------------------------
  if Assigned(FFormRight) then
    FFormRight.Save;

  // читаем значения полей из формы в запись
  FFormDispatcher.GetCacheValues(CacheItem);
  if Not (isModified in FCacheItem.State) then
    begin
      ModalResult := mrOK;
      Exit;
    end;

  // обновляем запись
  DMan := CreateDataManager(DataSetMeta.Table);
  //  DMan.PrepareRecord(FCacheItem);
  //DONE: Вероятно это исправление ошибки при редактировании элемента формы
  if isInserted in FCacheItem.State then StoreOK := DMan.CanInsertRecord(FCacheItem) else
  if isModified in FCacheItem.State then StoreOK := DMan.CanUpdateRecord(FCacheItem);

  // Если запись не удовлетворяет условиям фильтрации, то потом об этом сообщим
  FilterItem := TFilterItem.Create;
  try
    DataSetMeta.ReadAllFilters(FilterItem);
    Hidden := FilterItem.Active and not CacheItem.Calculate(FilterItem, False)
  finally
    FilterItem.Free;
  end;

  Warnings := StoreOK and (DMan.Errors.Count > 0);
  // обрабатываем особые случаи, проявляющиеся в модальном режиме
//TODO: Есть сомнения, возможно нельзя так убирать
  if { (fsModal in FormState)and {}  Warnings and FFirstUpdate then
    begin
      StoreOK := false;
      FFirstUpdate := false;
    end
  else
    FFirstUpdate := true;
  // если повторный вход в модальный режим с одними и теми же предупреждениями,
  // то предупреждения подавляются
//TODO: Есть сомнения, возможно нельзя так убирать
  if {(fsModal in FormState) and {} StoreOK and Warnings then
    DMan.Errors.Clear;
  // попытка обновить запись
  if StoreOK then
    if not (isInserted in FCacheItem.State) then
      StoreOK :=DMan.UpdateRecord(FCacheItem);
  // обработка успешного сохранения
  if StoreOK then
    begin
      NeedUpdateParent := (not (isInserted in FCacheItem.State)) and
        Assigned(DataSetMeta.Table.PField) and
        Assigned(DataSetMeta.Table.GField) and
          VarSameValue(CacheItem.FieldValue[CacheItem.Owner.FolderSignIndex],
             CacheItem.Owner.TableMeta.GField.Value1);
       { (CacheItem.ValueChanged[DataSetMeta.Table.Fields.IndexOf(DataSetMeta.Table.NField)] or
         CacheItem.ValueChanged[DataSetMeta.Table.Fields.IndexOf(DataSetMeta.Table.OField)])};
      CacheItem.ApplyChanges;

      if NeedUpdateParent then
        for I := 0 to DataSetMeta.OwnerLinks.Count-1 do
          if DataSetMeta.OwnerLinks[I].DataSetMeta.Table = DataSetMeta.Table then
            begin
              DataSetMeta.OwnerLinks[I].DataSetMeta.Cache.Update(mcUpdate, Null);
              Break;
            end;
      if not (isInserted in FCacheItem.State) then
        if Hidden then DataSetMeta.Cache.Update(mcDelete, Null)
                  else DataSetMeta.Cache.Update(mcUpdate, Null);
      if fsModal in FormState then
        ModalResult := mrOK;
    end;

  // вывод сообщений об ошибках
  if DMan.Errors.Count > 0 then
    begin
      if PropPanel.Enabled then
        PropPanel.SetFocus;
      if DMan.Errors.Count > 0 then
        if DMan.Errors[0] is TFieldError then
          begin
            FormElement := FFormDispatcher.FindByField(TFieldError(DMan.Errors[0]).FieldName);
            if Assigned(FormElement) then
              begin
                DMes := DeMessages.AddMessage(ImageID[StoreOK], '_dE.error', DMan.Errors.GetMessage);
                MForm.NewUserMessage;

                if FormElement.Control is TWinControl then
                  if TWinControl(FormElement.Control).CanFocus then
                    TWinControl(FormElement.Control).SetFocus;

                //TODO: Здесь похоже криво выбирается цвет сообщения: ошибка или предепреждение
                if Warnings then
                  ShowHintWindow(FormElement.Control, DMes.Text, EmptyStr, ImageID[Warnings], mtInformation)
                else
                  ShowHintWindow(FormElement.Control, DMes.Text, EmptyStr, ImageID[StoreOK], mtError);
              end
            else
              if StoreOK then
                SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, ImageID[StoreOK], NativeInt(PChar(DMan.Errors.GetMessage)))
              else
                SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, ImageID[StoreOK], NativeInt(PChar(DMan.Errors.GetMessage)));
          end
        else
          if StoreOK then
            SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, ImageID[StoreOK], NativeInt(PChar(DMan.Errors.GetMessage)))
          else
            SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, ImageID[StoreOK], NativeInt(PChar(DMan.Errors.GetMessage)));
    end;

  if not StoreOK then
    CacheItem.RollbackChanges
  else
    if Hidden then
      SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(GetTitle('_De.HideSaved', ttFull))));
        //  DMan.Errors.Add(TAnyError.Create(GetTitle('_De.HideInserted', ttFull)));

  DMan.Free;
end;

procedure TAForm.FormShow(Sender: TObject);
var c : TWinControl;
    i : Integer;
begin
  CloseBtnPanel.Visible:= not Floating;
  if Floating then
    WindowToCenter(self);

  for i:=0 to _PageControl.PageCount-1 do
    SetTabStop(_PageControl.Pages[i]);

  if _PageControl.ActivePage=nil then Exit;
  if _PageControl.ActivePage.ControlCount=0 then Exit;

  inherited;
  if Floating then
    BringToFront;

  c:=GetFirstControl(_PageControl.ActivePage);
  for i:=0 to _PageControl.ActivePage.ControlCount-1 do
    begin
      if (_PageControl.ActivePage.Controls[i] is TWinControl) then
        if (_PageControl.ActivePage.Controls[i].Tag)>0 then
          if AnsiCompareText('focus', TDeElement(_PageControl.ActivePage.Controls[i].Tag).ElementMeta.Name)=0 then
            c:=TWinControl(_PageControl.ActivePage.Controls[i]);
    end;
  if (c<>nil) and (c.Enabled) and (c.Visible) and (Floating) then ActiveControl:=c;
end;

procedure TAForm.EditAllChange(Sender: TObject);
begin
  Go_Ok.Enabled:=True;
  Go_Da_Cancel.Enabled:=True;
  FFirstUpdate := true;
end;

procedure TAForm.DesignPrepare(Sender: TObject; LockControls: Boolean);
var i: Integer;
begin
  if Not Assigned(Sender) then Exit;

  if LockControls then
    begin
      TPanel(Sender).OnMouseMove := _PageControlMouseMove;
      TPanel(Sender).OnMouseDown := _PageControlMouseDown;
    end
  else
    begin
      TPanel(Sender).OnMouseMove := nil;
      TPanel(Sender).OnMouseDown := nil;
    end;

  if (Sender is TWinControl) then
    for i:=0 to TWinControl(Sender).ControlCount-1 do
      DesignPrepare(TWinControl(Sender).Controls[i], LockControls);
end;

procedure TAForm.Go_Da_DesignSettingsExecute(Sender: TObject);
var
  RectEl: TControl;
  i: Integer;
begin
  SetFocus;
  FInDesign:=True;
  FFormDispatcher.DesignVisible:=True;

  Go_Da_Save.Visible           := FInDesign;
  Go_Da_Undo.Visible           := FInDesign;
  Go_Da_Save.Enabled           := FInDesign;
  Go_Da_Undo.Enabled           := FInDesign;
  Go_Da_DesignSettings.Checked := FInDesign;
  Go_Da_AutoAnchor.Visible     := FInDesign;
  DM_Da_Create.Enabled         := FInDesign;
  DM_Da_Delete.Enabled         := FInDesign;
  DM_Dl_Properties.Enabled     := FInDesign;

  for i := 0 to _PageControl.PageCount - 1 do
    if _PageControl.Pages[i].Tag = 0 then
      _PageControl.Pages[i].TabVisible := False;

//  if Assigned(Parent) then
//    TBaseGridForm(Parent.Parent).AFormSplitter.Enabled := True;

  RectEl := _PageControl;

  FAnchorRuler := TAnchorRuler.Create(Self);
  FAnchorRuler.Parent   := PropPanel;
  FAnchorRuler.Align    := alTop;
  FAnchorRuler.onChange := OnAnchorChange;

  _PageControl.Anchors:=_PageControl.Anchors-[akTop];
  if Assigned(Parent) then
    Parent.Height := Parent.Height + AnchorRulerHeight
  else
    Height := Height + AnchorRulerHeight;
  _PageControl.Anchors:=_PageControl.Anchors+[akTop];

  for i:= 0 to Pred(_PageControl.PageCount) do
    FFormDispatcher.ElementPanelResize(_PageControl.Pages[i]);

  if assigned(_PageControl.ActivePage) then
    _PageControl.ActivePage.Repaint;

  FAnchorRuler.Height := AnchorRulerHeight;
  FSizingRect := TSizingRect.Create(Self);
  FSizingRect.PopupMenu  := DesignMenu;
  FSizingRect.Element    := RectEl;
  FSizingRect.OnDblClick := DM_Dl_Properties.OnClick;

  FOldPageControlHeight   := _PageControl.Height;
  FOldPageControlWidth    := _PageControl.Width;
  FCheckPageControlResize := True;

  DesignPrepare(_PageControl, True);  // служит для включения и отключения дизайн-режима

  Go_Da_DesignSettings.Visible:=False;
end;

procedure TAForm.Go_SettingsExit(aEditAction: TEndEditAction);
var
  i: Integer;
begin
  SetFocus;
  FInDesign:=False;
  FFormDispatcher.DesignVisible:=False;

  Go_Da_Save.Visible            := FInDesign;
  Go_Da_Undo.Visible            := FInDesign;
  Go_Da_Save.Enabled            := FInDesign;
  Go_Da_Undo.Enabled            := FInDesign;
  Go_Da_DesignSettings.Checked  := FInDesign;
  Go_Da_AutoAnchor.Visible      := FInDesign;
  DM_Da_Create.Enabled          := FInDesign;
  DM_Da_Delete.Enabled          := FInDesign;
  DM_Dl_Properties.Enabled      := FInDesign;

  for i := 0 to _PageControl.PageCount - 1 do
    if _PageControl.Pages[i].Tag = 0 then
      _PageControl.Pages[i].TabVisible := True;

//  if Assigned(Parent) then
//    TBaseGridForm(Parent.Parent).AFormSplitter.Enabled := True;//False;
  //--------
  FCheckPageControlResize := False;
  FreeAndNil(FSizingRect);
  FreeAndNil(FAnchorRuler);

  _PageControl.Anchors:=_PageControl.Anchors-[akTop];
  if Assigned(Parent) then
    Parent.Height := Parent.Height - AnchorRulerHeight
  else
    Height := Height - AnchorRulerHeight;
  _PageControl.Anchors:=_PageControl.Anchors+[akTop];

  case aEditAction of
  smNo  :  ;
  smSave:  begin
             MetaData.BeginUpdate;
             DataSetMeta.SelectedForm.SaveElements;
             MetaData.EndUpdate;
           end;
  smUndo:  ReCreateView;
  smQuery: if Assigned(DataSetMeta.SelectedForm) and DataSetMeta.SelectedForm.WasChanged then
            if Application.MessageBox(
              PChar(GetTitle('_qUery.savechanges')),
              PChar(GetTitle('_Dl.Confirmation')),
              MB_ICONEXCLAMATION or MB_YESNO) = IDYES then
                DataSetMeta.SelectedForm.SaveElements
            else
              ReCreateView; // откат изменений
  end;

  for i:= 0 to Pred(_PageControl.PageCount) do
    FFormDispatcher.ElementPanelResize(_PageControl.Pages[i]);

  DesignPrepare(_PageControl, False);  // служит для включения и отключения дизайн-режима
  Go_Da_DesignSettings.Visible:=True;
end;

procedure TAForm.DM_Dl_PropertiesClick(Sender: TObject);
var EM, NewOwner : TElementMeta;
    FE           : TDeElement;
    OwnerChanged : boolean;
    BackupCursor : TCursor;
    ElementsList : TList;
    ValuesArray  : array of Variant;
    I            : integer;
    OldField, NewField, Fld : TFieldMeta;
    RecordEditor : TRecordEditor;
    DMan         : TElementsDataManager;
begin
  if not Assigned(FSizingRect) then exit;
  RecordEditor := nil;  DMan := nil;
  FE := TDeElement(FSizingRect.Element.Tag);
  if Assigned(FE) then
    EM := FE.ElementMeta
  else
    EM := nil;
  BackupCursor := Screen.Cursor;
  if Assigned(EM) then
    try
      Screen.Cursor := crHourGlass;
      RecordEditor := TRecordEditor.Create(MetaData.MetaTables[idxElement], EM.Id);
      OldField := EM.Field;
      DMan := TElementsDataManager.Create;
      DMan.SetElement(RecordEditor.CacheItem, EM);

      if (Sender is TAction) then
        RecordEditor.Caption:= TAction(Sender).Caption
      else if (Sender is TMenuItem) then
        RecordEditor.Caption:= TMenuItem(Sender).Caption
      else
        RecordEditor.Caption:= GetTitle('_Dl.Properties');
        
      Screen.Cursor := BackupCursor;
      if RecordEditor.Execute then
      begin
        EM.Assign(RecordEditor.CacheItem);
        NewField := EM.Field;
        FE.ElementMeta := EM;
        // изменяем положение элемента в иерархии
        OwnerChanged :=
          (Assigned(EM.Owner) and (not VarSameValue(EM.OwnerID, EM.Owner.ID)))
           or ((not Assigned(EM.Owner)) and
             (not VarIsEmpty(EM.OwnerID)) and (not VarIsNull(EM.OwnerID)));
        if OwnerChanged then
        begin
          NewOwner := TElementMeta(FFormDispatcher.ElementsForm.FindNode(EM.OwnerID));
          if Assigned(NewOwner) then
          begin
            if Assigned(EM.Owner) then
              EM.Owner.Extract(EM);
            NewOwner.Add(EM);
          end;
        end;
        // определяем необходимость пересоздания Control'ов
        FFormDispatcher.SortElements;
        ElementsList := TList.Create;
        EM.ConvertToList(ElementsList);
        ElementsList.Insert(0, EM);
        SetLength(ValuesArray, ElementsList.Count);
        for I := 0 to ElementsList.Count-1 do
        begin
          FE := FFormDispatcher.FindByElement(ElementsList[I]);
          if Assigned(FE) then
          begin
            ElementsList[I] := FE;
            FE.DoCreateControl := true;
            if I = 0 then
              Fld := OldField
            else
              Fld := FE.ElementMeta.Field;
            if Assigned(Fld) then
              ValuesArray[I] := FE.Value
            else
              ValuesArray[I] := Unassigned;
          end
          else
            ElementsList[I] := nil;
        end;
        ElementsList.Pack;
        // пересоздаем Control'ы
        FFormDispatcher.CreateControls(FFormDispatcher.ControlsOwner, DataSetMeta);
        // возвращаем Control'ам ранее назначенные значения
        for I := 0 to ElementsList.Count-1 do
        begin
          if (I = 0) and (OldField <> NewField) then
          begin
            if Assigned(NewField) and Assigned(CacheItem) then
              TDeElement(ElementsList[I]).Value :=
                CacheItem.FieldValue[CacheItem.Owner.Fields.IndexByName(NewField.Original)]
            else
              TDeElement(ElementsList[I]).Value := Null;
          end
          else if (I > 0) or (OldField = NewField) then
            TDeElement(ElementsList[I]).Value := ValuesArray[I];
          DesignPrepare(TDeElement(ElementsList[I]).Control, True);
        end;
        if (not OwnerChanged) and (ElementsList.Count = 1) then
          FSizingRect.Element := TDeElement(ElementsList[0]).Control
        else
          FSizingRect.Element := _PageControl;
        ElementsList.Free;
      end;
    finally
      Screen.Cursor := BackupCursor;
      RecordEditor.Free;
      DMan.Free;
    end;
end;

procedure TAForm.DM_Da_DeleteClick(Sender: TObject);
var EM : TElementMeta;
    FE : TDeElement;
begin
  if Assigned(FSizingRect) then
    begin
      FE:=TDeElement(FSizingRect.Element.Tag);
      if Assigned(FE) then
        EM := FE.ElementMeta
      else
        EM := nil;
      if Assigned(EM) then
        begin
          EM.Changes := EM.Changes + [mcDelete];
          FE:= FFormDispatcher.FindByElement(TElementMeta(EM));
          if Assigned(FE) then FE.DestroyControl(True);

          FE:= FFormDispatcher.FindByElement(TElementMeta(EM.Owner));
          if assigned(FE.Control) then FSizingRect.Element:= FE.Control
                                  else FSizingRect.Element:= _PageControl;

          // ищем и удаляем надпись к контролу
          if assigned(EM.Field) and not(EM.ElementType = etLabel) then
            begin
              EM:=TElementMeta(EM.Owner).FindByFieldAndType(EM.Field, [etLabel]);
              if assigned(EM) then
                begin
                  EM.Changes := EM.Changes + [mcDelete];
                  FE:=FFormDispatcher.FindByElement(EM);
                  if Assigned(FE) then FE.DestroyControl(True);
                end;
            end;
        end
      else
        FSizingRect.Element := _PageControl;
    end;
end;

procedure TAForm.DM_Da_CreateClick(Sender: TObject);
var EF : TElementMeta;
    EM : TElementMeta;
    NewElement : TDeElement;
    ParentCont, ParentContRes: TControl;
    EType: TElementType;
    EField: TFieldMeta;
    EName: String ;
    XY: TPoint;
    TabIndex, _AL,_AR,_L,_R,_T : Integer;
begin
  ParentCont := FSizingRect.Element;
  ParentContRes := ParentCont;
  if ParentCont.Tag = 0 then
    begin
      // нет формы - необходимо создать первую форму
      EF := DataSetMeta.Table.Elements.AddBlankForm;
      FFormDispatcher.SetElementsForm(EF);
      FFormDispatcher.CreateControls(_PageControl, DataSetMeta);
    end
  else
    begin
      EM := TDeElement(ParentCont.Tag).ElementMeta;

      if EM.ElementType = etForm then
        begin
          EType := etTabSheet;
          TabIndex:=EM.Count+1;
          EM:=TElementMeta.Create(DataSetMeta.Table, Format('_Dl.Properties (%d)',
                                       [TabIndex]), nil, EType, 0, 0, 0, 0, 0, TabIndex, 0);
        end
      else
        begin
          XY:=TWinControl(ParentCont).ScreenToClient(FPopupPos);

          _AL := 0;     _L := XY.X div XStep;
          _AR := 0;     _R := XY.X div XStep + 10;
                        _T := XY.Y div YStep;

          EType := etPanel;
          EField:= nil;
          EName := GetTitle('_dT.formelement');

          if not (EM.ElementType in ContainerElements) then
            begin
              EField:=EM.Field;
              ParentContRes := ParentCont.Parent;
            end;

          if Sender is TMenuItem then
            begin
              EType := TElementType(TMenuItem(Sender).Tag);
              EName := TMenuItem(Sender).Caption;

              if (EType=etButton) then
                begin
                  _R:= _L + 4;
                  EName:= '...';
                end else

              if Assigned(EField) and (EType=etLabel) then
                begin
                  EName:= EmptyStr;
                  _AL := EM.EAL;   _L := EM.EL - LabelWidth;
                  _AR := EM.EAL;   _R := EM.EL;
                                   _T := EM.ET;

                  if (_AL=0) and (EM.EL<20) then _L := Min(0,_L);

                  // если левее левого края, то размещаем выше или ниже
                  if ((ClientWidth*_AL) div (AnchorsCount-1))+XStep*_L < 0 then
                    begin
                      _L := EM.EL;
                      _R := EM.EL + LabelWidth;
                      if _T>2 then _T :=EM.ET-3
                              else _T :=EM.ET+EM.EH;
                    end;
                end else

              if (EType = etBevel) then
                begin
                  _AL:= 0;
                  _AR:= 12;
                  _R:= 0;
                end;
            end;

          EM:=TElementMeta.Create(DataSetMeta.Table, EName, EField, EType, 0, _AL, _AR, _L, _R, _T, 3);
        end;

      TDeElement(ParentContRes.Tag).ElementMeta.Add(EM);
      NewElement := FFormDispatcher.AddElement(EM);
      NewElement.ElementMeta.ElementType := EType;
      NewElement.CreateControl(TWinControl(ParentContRes), DataSetMeta);

      TPanel(NewElement.Control).OnMouseMove:=_PageControlMouseMove;
      FSizingRect.Element    := NewElement.Control;
    end;
end;

procedure TAForm.CreateFieldControlExecute(Sender: TObject);
var
  ParentCont, ParentContRes: TControl;
  EM : TElementMeta;
  NewElement: TDeElement;
  FM: TFieldMeta;
  XY: TPoint;
  NL, NC: Integer;
begin
  if Not (Sender is TMenuItem) then Exit;

  ParentCont := FSizingRect.Element;
  ParentContRes := ParentCont;
  EM:= TDeElement(ParentCont.Tag).ElementMeta;
  FM:= TFieldMeta(TMenuItem(Sender).Tag);
  XY:=TWinControl(ParentCont).ScreenToClient(FPopupPos);

  NL:= EM.Add(TElementMeta.Create( FM.Owner, EmptyStr, FM, etLabel, 0, 0, 0, XY.X div XStep - LabelWidth, XY.X div XStep, XY.Y div YStep, 3));
  NewElement:= FFormDispatcher.AddElement(EM[NL]);
  NewElement.CreateControl(TWinControl(ParentContRes), DataSetMeta);

  NC:= EM.DesignElement(FM, XY.X div XStep, XY.Y div YStep);
  NewElement:= FFormDispatcher.AddElement(EM[NC]);
  NewElement.CreateControl(TWinControl(ParentContRes), DataSetMeta);
end;

procedure TAForm.DF_Da_CreateClick(Sender: TObject);
begin
  DataSetMeta.SelectedForm := DataSetMeta.Table.Elements.AddDefaultForm;
  SelectNewForm(nil);
end;

procedure TAForm.DF_Da_DeleteClick(Sender: TObject);
begin
  DataSetMeta.Table.Elements.DeleteForm(DataSetMeta.SelectedForm);
  FFormDispatcher.Clear;
  if DataSetMeta.Table.Elements.Count>0 then
    DataSetMeta.SelectedForm := DataSetMeta.Table.Elements[0]
  else
    DataSetMeta.SelectedForm:=nil;
  SelectNewForm(nil);
end;

procedure TAForm.DesignMenuPopup(Sender: TObject);
var i, TabIndex  : Integer;
    TMI, NewItem : TMenuItem;
    Cache        : TDataCache;
    E            : TElementMeta;
begin
  FPopupPos:=Mouse.CursorPos;

  DM_Da_Delete.Visible    := FInDesign;
  DM_Da_Create.Visible    := FInDesign;
  DM_Dl_Forms.Visible     := False;//FInDesign;
  DM_Dl_Properties.Visible:= FInDesign;
  DM_Da_Go.Visible        := not FInDesign;
  DM_Da_Hide.Visible      := not FInDesign;
  DM_Dl_TabPages.Visible  := not FInDesign;
  DM_Da_MoveTo.Visible    := FInDesign;

  if FInDesign then
  begin
    E:= TDeElement(FSizingRect.Element.Tag).ElementMeta;

    Go_Da_AutoAnchor.Enabled:= E.ElementType <> etForm;
    DM_Da_Delete.Enabled:= E.ElementType <> etForm;

    { меню "Переместить на закладку" }
    DM_Da_MoveTo.Clear;
    DM_Da_MoveTo.Visible :=  not (E.ElementType in [etForm, etTabSheet, etListTabSheet]);
    if DM_Da_MoveTo.Visible then
      for I := 0 to _PageControl.PageCount-1 do
        if (I <> _PageControl.ActivePageIndex) and (_PageControl.Pages[I].Tag <> 0)
          and (TDeElement(_PageControl.Pages[I].Tag).ElementMeta.ElementType in ContainerElements) then
            begin
              TMI := TMenuItem.Create(DM_Da_MoveTo);
              TMI.Tag := NativeInt(_PageControl.Pages[I]);
              TMI.Caption := _PageControl.Pages[I].Caption;
              TMI.OnClick := MoveToPageClick;
              DM_Da_MoveTo.Add(TMI);
            end;
    DM_Da_MoveTo.Visible := DM_Da_MoveTo.Count > 0;
    
    { меню "Создать" }
    DM_Da_Create.Clear;

    if FSizingRect.Element = _PageControl then
      begin
        TMI:= TMenuItem.Create(DM_Da_Create);
        TMI.Tag := ord(etTabSheet);
        TMI.Caption:= GetTitle('_dV.elempage');
        TMI.OnClick := DM_Da_CreateClick;
        DM_Da_Create.Add(TMI);
      end else

    if E.ElementType in (EditorElements+ViewerElements) then
      begin
        TMI:= TMenuItem.Create(DM_Da_Create);
        TMI.Tag:= ord(etLabel);
        if Assigned(E.Field) then TMI.Caption:= GetTitle('_dV.elemsign')+' "'+GetTitle(E.Field.Name)+'"'
                             else TMI.Caption:= GetTitle('_dV.elemsign');
        TMI.OnClick:= DM_Da_CreateClick;
        TMI.Enabled:= Assigned(E.Field);
        DM_Da_Create.Add(TMI);
      end else

      if E.ElementType in ContainerElements then
        begin
          // типы контролов
          Cache := TDataCache.Create(MetaData.GetSystemTableByName(tblDElementTypes));
          Cache.SortList.Add(Cache.TableMeta.NField.ID);
          Cache.PrepareData;
          DM_Da_Create.OnClick := nil;

          TMI:= TMenuItem.Create(DM_Da_Create);
          TMI.Tag:= ord(etDefault);
          TMI.Caption:= GetTitle('_dT.field');
          TMI.OnClick:= DM_Da_CreateClick;
          TMI.Default:= True;
          DM_Da_Create.Add(TMI);

          DM_Da_Create.Add(Menus.NewLine);

          // поля, которые отсутствуют на форме
          if assigned(DatasetMeta) then
            if assigned(DatasetMeta.Table) then
              for i := 0 to Pred(DatasetMeta.Table.Fields.Count) do
                if (not DatasetMeta.Table.Fields[i].IsLookup) and (fvService < DatasetMeta.Table.Fields[i].VisibleLevel) then
                  if not assigned(DataSetMeta.SelectedForm.FindByFieldAndType(DatasetMeta.Table.Fields[i], EditorElements)) then
                    begin
                      TMI:= TMenuItem.Create(DM_Da_Create);
                      TMI.Tag:= NativeInt(DatasetMeta.Table.Fields[i]);
                      TMI.Caption:= GetTitle(DatasetMeta.Table.Fields[i].Name);
                      TMI.Hint:= GetTitle(DatasetMeta.Table.Fields[i].Original);
                      TMI.OnClick:= CreateFieldControlExecute;
                      DM_Da_Create.Add(TMI);
                    end;

          DM_Da_Create.Add(Menus.NewLine);

          for I := 0 to Cache.Count-1 do
            if TElementType(VarToInt(Cache[I].ID)) in CreateElements then
              begin
                TMI:= TMenuItem.Create(DM_Da_Create);
                TMI.Tag:= VarToInt(Cache[I].ID);
                TMI.Caption:= GetTitle(Cache[I].Caption);
                TMI.OnClick:= DM_Da_CreateClick;
                DM_Da_Create.Add(TMI);
              end;
          Cache.Free;
        end;
      DM_Da_Create.Enabled:= (0 < DM_Da_Create.Count);
  end
  else
  begin

//TODO: падало из-за этого куска в режиме FInDesign, при исполнении в начале метода Странно!
(*
    While DM_Dl_Forms.Count>3 do
      DM_Dl_Forms.Delete(0);
    for i:=0 to DataSetMeta.Table.Elements.Count-1 do
    begin
      TMI:=TMenuItem.Create(DM_Dl_Forms);
      TMI.Tag       := NativeInt(DataSetMeta.Table.Elements[i]);
      TMI.RadioItem := True;
      TMI.Checked   := (DataSetMeta.Table.Elements[i]=DataSetMeta.SelectedForm);
      TMI.OnClick   := SelectNewForm;
      if Length(DataSetMeta.Table.Elements[i].Name) > 0
        then TMI.Caption := GetTitle(DataSetMeta.Table.Elements[i].Name)
        else TMI.Caption := GetTitle('_Dl.Form ')+IntToStr(i);
      DM_Dl_Forms.Insert(0,TMI);
    end;
    (* *)

    { опции меню "перейти" }
    DM_Da_Go.Clear;
    for I := 0 to _PageControl.PageCount-1 do
      begin
        NewItem := TMenuItem.Create(Self);
        NewItem.Caption := _PageControl.Pages[I].Caption;
        NewItem.Tag := I;
        NewItem.OnClick := SelectPage;
        DM_Da_Go.Add(NewItem);
      end;
    DM_Da_Go.Visible := DM_Da_Go.Count > 0;

    { опция меню "Скрыть" }
    TabIndex := GetTrueTabIndex(_PageControl.IndexOfTabAt(
      _PageControl.ScreenToClient(TPopupMenu(Sender).PopupPoint).X,
      _PageControl.ScreenToClient(TPopupMenu(Sender).PopupPoint).Y));
    DM_Da_Hide.OnClick := OnHidePage;
    DM_Da_Hide.Tag := TabIndex;
    DM_Da_Hide.Visible := (TabIndex >= 0)
      and Assigned(TDeElement(_PageControl.Pages[TabIndex].Tag));
    DM_Da_Hide.Enabled := DM_Da_Hide.Visible
      and (TDeElement(_PageControl.Pages[TabIndex].Tag).ElementMeta.ElementType = etListTabSheet);
  end;
end;

procedure TAForm.SelectNewForm(Sender : TObject);
begin
  Go_Da_DesignSettingsExecute(Self);
  if Assigned(Sender) then
    DataSetMeta.SelectedForm:= TElementMeta(TMenuItem(Sender).Tag);

  // удаляем элементы старой формы
  FFormDispatcher.DestroyControls;
  while _PageControl.ComponentCount > 0 do _PageControl.Components[0].Free;
  while _PageControl.ControlCount > 0 do _PageControl.Controls[0].Free;

  // создаем элементы новой формы
  TAForm(DataSetMeta.CardForm).ReinitForm(DataSetMeta);
  TAForm(DataSetMeta.CardForm).Enabled := DataSetMeta.DataEnabled;
  TAForm(DataSetMeta.CardForm).SetCacheItem(FCacheItem);
end;

procedure TAForm.BaseActionsExecute(Sender: TObject);
var
  i: integer;
  ParentForm : TBaseDataForm;
begin
  ParentForm := TBaseDataForm(DataSetMeta.GridForm);
  if not Assigned(ParentForm) then Exit;
  for i:= 0 to  ParentForm.ActionList.ActionCount -1 do
    if TCustomAction(Sender).Name = ParentForm.ActionList.Actions[i].Name then
      ParentForm.ActionList.Actions[i].Execute;
end;

procedure TAForm.FormActivate(Sender: TObject);
var
  i, j: integer;
  ParentForm : TBaseDataForm;
begin
  inherited;
  if not Assigned(DataSetMeta) then Exit;
  ParentForm := TBaseDataForm(DataSetMeta.GridForm);
  if not Assigned(ParentForm) then Exit;
  for i:= 0 to  ActionList.ActionCount -1 do
    for j := 0 to ParentForm.ActionList.ActionCount -1 do
      if ActionList.Actions[i].Name = ParentForm.ActionList.Actions[j].Name then
        begin
          ActionList.Actions[i] :=  ParentForm.ActionList.Actions[j];
          Break;
        end;
  _PageControlChange(_PageControl);

  if Assigned(MainBaseForm) then
     MainBaseForm.Perform(WM_BF_ACTIVE, NativeInt(Self), 0);
  {
  if _PageControl.ACtivePage is TDeListTabSheet then
    if _PageControl.ACtivePage.Enabled then
      if Assigned(TDeListTabSheet(_PageControl.ACtivePage).GridControl.ActiveControl) then
        if TDeListTabSheet(_PageControl.ACtivePage).GridControl.ActiveControl.CanFocus then
          TDeListTabSheet(_PageControl.ACtivePage).GridControl.ActiveControl.SetFocus;
  }
end;

procedure TAForm._PageControlChange(Sender: TObject);
begin
  inherited;
  if Assigned(_PageControl.ActivePage) then
    if _PageControl.ActivePage is TDeListTabSheet then
      begin
        if _PageControl.ActivePage.Enabled then
          if Assigned(TDeListTabSheet(_PageControl.ACtivePage).GridControl.ActiveControl) then
            if TDeListTabSheet(_PageControl.ACtivePage).GridControl.ActiveControl.CanFocus then
              TDeListTabSheet(_PageControl.ACtivePage).GridControl.ActiveControl.SetFocus;
      end
    else
      if (_PageControl.ActivePage is TDeTabSheet) then
        if (_PageControl.ActivePage.ControlCount = 1) and (_PageControl.ActivePage.Controls[0] is TBaseGridForm) then
          with _PageControl.ActivePage.Controls[0] as TBaseGridForm do
            DataSetMeta.Cache.Update(mcNone, Null);

end;

procedure TAForm.FormDestroy(Sender: TObject);
var i:Integer;
begin
  //отключаем закладки, чтобы не срабатывала активизация
  with FFormDispatcher do
    for I := 0 to ElementsForm.Count-1 do
      if ElementsForm[I].ElementType = etListTabSheet then
        TDeListTabSheet(FindByElement(ElementsForm[I]).Control).PrepareToDestroy;

  FFormDispatcher.Free;
  inherited;
end;

procedure TAForm.FormCreate(Sender: TObject);
begin
  FInDesign:=False;
  Go_Da_Save.Visible := FInDesign;
  Go_Da_Undo.Visible := FInDesign;
  Go_Da_Save.Enabled := FInDesign;
  Go_Da_Undo.Enabled := FInDesign;
  Go_Da_AutoAnchor.Visible := FInDesign;

  Width:= Min(MForm.MainGridPanel.Width, Round(0.75*Application.MainForm.ClientWidth));
  inherited;
  FNewEnabled := true;
  FShowShortcuts := Variables.AsBoolean[RegShowShortcuts];
end;

procedure TAForm._PageControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FInDesign then
    FSizingRect.PanelMouseMove(Sender,Shift,X,Y);
end;

procedure TAForm._PageControlMouseDown(Sender: TObject;  Button : TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
    begin
      if FInDesign then
        FSizingRect.PanelMouseDown(Sender, Button, Shift, X, Y)
      else
        begin
          inherited;
          FInPage:=_PageControl.ActivePage.PageIndex;
          _PageControl.Canvas.Pen.Color  :=clBlack;
          _PageControl.Canvas.Brush.Color:=clBlack;
          _PageControl.BeginDrag(False);
        end
    end    
  else
    Inherited;
end;

procedure TAForm._PageControlDragDrop(Sender, Source: TObject; X, Y: Integer);
var i,A,C,ind: Integer;
    r:  TRect;
begin
  if not (Sender is TPageControl) then Exit;
  A := _PageControl.ActivePage.TabIndex;

  for C := 0 to _PageControl.PageCount - 1 do
  begin
    _PageControl.Perform(TCM_GETITEMRECT, C, lParam(@r));
    if PtInRect(r, Point(X, Y)) and (C<>A) and
       Assigned(TDeElement(_PageControl.ActivePage.Tag)) and
       Assigned(TDeElement(_PageControl.Pages[C].Tag)) then
      begin
        FFormDispatcher.ElementsForm.Move(
          FFormDispatcher.ElementsForm.IndexOf(TDeElement(_PageControl.ActivePage.Tag).ElementMeta),
          FFormDispatcher.ElementsForm.IndexOf(TDeElement(_PageControl.Pages[C].Tag).ElementMeta));
        _PageControl.ActivePage.PageIndex := C;

        ind:=MaxInt;
        for i:=Min(C,A) to Max(C,A) do
          ind:=Min(ind,TDeElement(_PageControl.Pages[i].Tag).ElementMeta.ET);

        for i:=Min(C,A) to Max(C,A) do
        begin
          TDeElement(_PageControl.Pages[i].Tag).ElementMeta.ET:=ind;
          Inc(Ind);
        end;

        FFormDispatcher.ElementsForm.SaveElements;
      end;
  end;
end;

procedure TAForm._PageControlDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var i: Integer;
    r: TRect;
begin
  if State = dsDragLeave then
    InvalidateRect(_PageControl.Handle, nil, True);

  if Not (Sender is TPageControl) then Exit;

  Accept := True;
  for i := 0 to _PageControl.PageCount - 1 do
  begin
    _PageControl.Perform(TCM_GETITEMRECT, i, lParam(@r));

    if PtInRect(r, Point(X, Y)) then
    begin
      if FInPage<>i then
        begin
          InvalidateRect(_PageControl.Handle, nil, True);
          FInPage:=i;
          if i=_PageControl.ActivePage.PageIndex then
            _PageControl.DragCursor:=crNoDrop
          else
            _PageControl.DragCursor:=crDrag;
        end;

      if (i < _PageControl.ActivePage.PageIndex) then
        _PageControl.Canvas.Rectangle(r.Left-2, r.Top, r.Left+2, r.Bottom) else
      if (i > _PageControl.ActivePage.TabIndex) then
        _PageControl.Canvas.Rectangle(r.Right-2, r.Top, r.Right+2, r.Bottom);
      Break;
    end;
  end;
end;

procedure TAForm._PageControlResize(Sender: TObject);
begin
  if not FCheckPageControlResize then Exit;
  if Assigned(FSizingRect) and
    (FSizingRect.Element = Sender) then FSizingRect.Update;
  if _PageControl.Height <> FOldPageControlHeight then
    begin
      DataSetMeta.SelectedForm.EH := (ClientHeight - BtnPanel.Height -  8 - AnchorRulerHeight) div YStep;
      with DataSetMeta.SelectedForm do
        Changes := Changes + [mcUpdate];
      FOldPageControlHeight := _PageControl.Height;
    end;
  if _PageControl.Width <> FOldPageControlWidth then
    begin
      if Assigned(FAnchorRuler) then FAnchorRuler.Resized;
      FOldPageControlWidth := _PageControl.Width;
    end;
end;

procedure TAForm.PropPanelContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var Point      : TPoint;
    i,TopBound : integer;
    P          : TTabSheet;
begin
  Point:=MousePos;
  Point:=PropPanel.ClientToScreen(Point);

  TopBound := _PageControl.Top;
  if _PageControl.PageCount > 0 then
    inc(TopBound, _PageControl.Pages[0].Top)
  else
    inc(TopBound, _PageControl.Height);

  if MousePos.Y < TopBound then
    begin
      if UserSession.IsAdmin then
        DesignMenu.Popup(Point.X, Point.Y);
      Handled := true;
    end
  else
    begin
      //Принудительно посылаем команду задизабленным контролам TDeDSComboBox
      P:=_PageControl.ActivePage;
      for i := 0 to P.ControlCount - 1 do
        if P.Controls[i] is TWinControl then
          if (P.Controls[i] is TDeDSComboBox) and (not P.Controls[i].Enabled) then
            if PtInRect( P.Controls[i].BoundsRect, P.ScreenToClient(Point)) then
              begin
                TDeDSComboBox(P.Controls[i]).PopupMenu.Popup(Point.X, Point.Y);
                Handled := true;
              end;
    end;
end;

procedure TAForm.UA_Da_ActionExecute(Sender: TObject);
//var P : TPoint;
begin
  inherited;
  {
  P.X:=ActionBtn.Left;
  P.Y:=ActionBtn.Top;
  P:=ActionBtn.ClientToScreen(P);
  ActionBtn.DropdownMenu.Popup(P.X,P.Y);
  {}
end;

procedure TAForm.ActionMenuPopup(Sender: TObject);
begin
  DataSetMeta.UpdateActions;
  inherited;
end;

procedure TAForm.SelectPage(Sender: TObject);
begin
  if Sender is TMenuItem then
    _PageControl.ActivePageIndex := TMenuItem(Sender).Tag;
end;

procedure TAForm.OnHidePage(Sender: TObject);
var PageElem : TElementMeta;
begin
  PageElem := TDeElement(_PageControl.Pages[TMenuItem(Sender).Tag].Tag).ElementMeta;
  RemovePage(PageElem);
  FFormDispatcher.ElementsForm.SaveElements;
end;

procedure TAForm.DM_Dl_TabPagesClick(Sender: TObject);
var I, PageOldIndex, PageNewIndex : integer;
    OrderForm : TItemsOrderForm;
    ItemName  : string;
    Counter   : TCounter;
begin
  OrderForm := TItemsOrderForm.Create(Application);

  { определяем количество закладок для одного и того же набора данных }
  Counter := TCounter.Create;
  // виртуальные закладки
  for I := 0 to DataSetMeta.Table.Links.Count-1 do
    if    (DataSetMeta.Table.Links[I].Owner.ID <> MetaData.MetaTables[idxInterrelations].ID)
      and (DataSetMeta.Table.Links[I].Owner.ID <> MetaData.MetaTables[idxNotes].ID) then
        Counter.IncCount(DataSetMeta.Table.Links[I].Owner.ID);
  {   //DONE: Changed
  // уже созданные виртуальные закладки
  with FFormDispatcher do
    for I := 0 to ElementsForm.Count-1 do
      if ElementsForm[I].ElementType = etListTabSheet then
        Counter.IncCount(TDeListTabSheet(FindByElement(ElementsForm[I]).Control).DataID);
  // возможные виртуальные закладки
  for I := 0 to DataSetMeta.Table.Links.Count-1 do
    if not Assigned(FFormDispatcher.ElementsForm.FindDatasetPage(
      DataSetMeta.Table.Links[I].Owner.ID, DataSetMeta.Table.Links[I]))
      and (DataSetMeta.Table.Links[I].Owner.ID <> MetaData.MetaTables[idxInterrelations].ID)
      and (DataSetMeta.Table.Links[I].Owner.ID <> MetaData.MetaTables[idxNotes].ID) then
        Counter.IncCount(DataSetMeta.Table.Links[I].Owner.ID);
  {}
  { формируем список для диалога }
  // существующие закладки
  with FFormDispatcher do
    for I := 0 to ElementsForm.Count-1 do
      OrderForm.AddItem(
        GetTitle(ElementsForm[I].Name, ttSecondName),
        ElementsForm[I],
        true,
        ElementsForm[I].ElementType = etListTabSheet);
  // возможная закладка взаимосвязей
  if not Assigned(FFormDispatcher.ElementsForm.FindDatasetPage(MetaData.MetaTables[idxInterrelations].ID, nil)) then
    OrderForm.AddItem( GetTitle('_Dt.ShortCut', ttSecondName),  MetaData.MetaTables[idxInterrelations], false, true);
  // возможная закладка заметок
  if not Assigned(FFormDispatcher.ElementsForm.FindDatasetPage(MetaData.MetaTables[idxNotes].ID,
                                                 MetaData.MetaTables[idxNotes].Fields.FindByName(fldNotesKey))) then
    OrderForm.AddItem( GetTitle('_Dt.Note', ttSecondName), MetaData.MetaTables[idxNotes], false, true);
  // прочие возможные закладки
  for I := 0 to DataSetMeta.Table.Links.Count-1 do
    if not Assigned(FFormDispatcher.ElementsForm.FindDatasetPage(
      DataSetMeta.Table.Links[I].Owner.ID, DataSetMeta.Table.Links[I]))
      and (DataSetMeta.Table.Links[I].Owner.ID <> MetaData.MetaTables[idxInterrelations].ID)
      and (DataSetMeta.Table.Links[I].Owner.ID <> MetaData.MetaTables[idxNotes].ID)
      and (DataSetMeta.Table.Links[I].IsStored) then
    begin
      if Counter[DataSetMeta.Table.Links[I].Owner.ID] = 1 then
        ItemName := GetTitle(DataSetMeta.Table.Links[I].Owner.Name, ttSecondName)
      else
        ItemName := Format('%s (%s)',
                   [GetTitle(DataSetMeta.Table.Links[I].Owner.Name, ttSecondName), DataSetMeta.Table.Links[I].Native]);
      OrderForm.AddItem(ItemName, DataSetMeta.Table.Links[I], false, true);
    end;

  if (DataSetMeta.Context.ViewType<>vtTileMap) and Assigned(DataSetMeta.Table) and (tsGEO in DataSetMeta.Table.LoadingState) then
    begin
      OrderForm.AddItem( GetTitle('_Da.ViewTileMap'), DataSetMeta.Table.KField[0], false, true);
    end;

  OrderForm.ReInitForm(nil);
  OrderForm.Caption := GetTitle('_Dl.TabPages',ttSecondName);
  OrderForm.Title := Format(GetTitle('_dM.Sequence'), [GetTitle('_dl.TabPages',ttSecondName)]);

  if OrderForm.ShowModal = mrOK then
  begin
    PageNewIndex := 0;
    for I := 0 to OrderForm.ItemCount-1 do
      if OrderForm.Checked[I] then
        begin
          if TObject(OrderForm.Data[I]) is TElementMeta then
            with FFormDispatcher do
            begin
              PageOldIndex := ElementsForm.IndexOf(OrderForm.Data[I]);
              if ElementsForm[PageOldIndex].ET <> (PageNewIndex+1) then
                begin
                  ElementsForm[PageOldIndex].ET := PageNewIndex + 1;
                  with ElementsForm[PageOldIndex] do
                    Changes := Changes + [mcUpdate];
                  ElementsForm.Move(PageOldIndex, PageNewIndex);
                  TTabSheet(FindByElement(ElementsForm[PageNewIndex]).Control).PageIndex := PageNewIndex;
                end;
            end
          else if TObject(OrderForm.Data[I]) is TTableMeta then
            begin
              if TTableMeta(OrderForm.Data[I]).ID = MetaData.MetaTables[idxNotes].ID then
                AddPage(
                  TTableMeta(OrderForm.Data[I]).ID,
                  MetaData.MetaTables[idxNotes].Fields.FindByName(fldNotesKey),
                  TTableMeta(OrderForm.Data[I]).Name,
                  PageNewIndex) else

              if TTableMeta(OrderForm.Data[I]).ID = MetaData.MetaTables[idxInterRelations].ID then
                AddPage(
                  TTableMeta(OrderForm.Data[I]).ID,
                  nil,
                  TTableMeta(OrderForm.Data[I]).Name,
                  PageNewIndex);
            end
          else
            begin
              if Counter[TFieldMeta(OrderForm.Data[I]).Owner.ID] < 2 then
                ItemName := TFieldMeta(OrderForm.Data[I]).Owner.Name
              else
                ItemName := Format('%s (%s)',
                  [TFieldMeta(OrderForm.Data[I]).Owner.Name, TFieldMeta(OrderForm.Data[I]).Name]);
              AddPage(TFieldMeta(OrderForm.Data[I]).Owner.ID, TFieldMeta(OrderForm.Data[I]),
                ItemName, PageNewIndex);
            end;
          inc(PageNewIndex);
        end
      else if TObject(OrderForm.Data[I]) is TElementMeta then
        RemovePage(TElementMeta(OrderForm.Data[I]));
    FFormDispatcher.ElementsForm.SaveElements;
  end;
  OrderForm.Free;
  Counter.Free;
end;

procedure TAForm.CloseAreaBtnClick(Sender: TObject);
begin
  Variables.AsBoolean[RegShowCardArea] := False;
  MForm.act_Da_ShowCardArea.Checked    := False;
  TBaseGridForm(DataSetMeta.GridForm).HideViewArea(True);
end;

constructor TAForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMainControl:= _PageControl;
  DM.MapActionListIcons(ActionList1);
end;

function TAForm.GetTrueTabIndex(aTabIndex : integer) : integer;
var I : integer;
begin
  result := -1;
  if aTabIndex >= 0 then
    begin
      for I := 0 to _PageControl.PageCount-1 do
        begin
          if _PageControl.Pages[I].TabVisible then
            inc(result);
          if result = aTabIndex then
              Exit(I);
        end;
    end;
end;

procedure TAForm.SelectTabSheet(var Msg : TWMMouse);
var TabIdx : integer;
begin
  if FInDesign then
    begin
      TabIdx := GetTrueTabIndex(_PageControl.IndexOfTabAt(Msg.XPos, Msg.YPos));
      if TabIdx >= 0 then
        _PageControl.ActivePageIndex := TabIdx;
    end;
end;

procedure TAForm.SetCacheItem(aItem : TCacheItem);
var NewEnabled, CancelEnabled, OperationsEnabled : boolean;
begin
  if Not Assigned(DataSetMeta) then Exit;

  CancelEnabled:= (DataSetMeta.Cache.Count>0) and (not DataSetMeta.Table.ErrorsDetected) and Assigned(aItem);

  OperationsEnabled:= CancelEnabled and (not DataSetMeta.Table.IsReadOnly) and
                      SecuritySystem.CheckPolicyDataset( DataSetMeta.Table.ID, spUpdate) and
                     (DataSetMeta.DataEnabled) and (not (isReadOnly in aItem.State));

  NewEnabled:= OperationsEnabled;

  if FNewEnabled<>NewEnabled then
    begin
      FNewEnabled := NewEnabled;
      FFormDispatcher.Enabled := FNewEnabled;
      if FInDesign then
        begin
          DesignPrepare(_PageControl, True);
        end;
      Go_Ok.Enabled := FNewEnabled;
   end;

  Go_Da_Cancel.Enabled := CancelEnabled; //OperationsEnabled;


{$ifopt D+}
  Go_Da_DesignSettings.Enabled := UserSession.IsAdmin;
{$else}
  Go_Da_DesignSettings.Enabled := UserSession.IsAdmin and not (DataSetMeta.Table.SolutionID = MetaSolution);
{$endif}

  FCacheItem := aItem;
  FFormDispatcher.SetCacheValues(CacheItem);

  if Assigned(FFormRight) then
    begin
      if Assigned(FCacheItem) then FFormRight.ObjectID := FCacheItem.ID
                              else FFormRight.ObjectID := unAssigned;
    end;
end;

function TAForm.AddPage(const aTableID : integer;  aLinkField : TFieldMeta;
  const aPageName : string = '';  aPageIndex : integer = -1) : TDeElement;
var Page        : TElementMeta;
    I           : integer;
    PageCreated : boolean;
begin
  Page := FFormDispatcher.ElementsForm.GetDatasetPage(aTableID, aLinkField, PageCreated);
  if aPageName <> EmptyStr then
    Page.Name := aPageName;
  result := FFormDispatcher.FindByElement(Page);
  if not Assigned(result) then
    begin
      result := FFormDispatcher.AddElement(Page);
      result.CreateControl(FFormDispatcher.ControlsOwner, DataSetMeta);
      if aPageIndex >= 0 then
        TTabSheet(result.Control).PageIndex := aPageIndex;
      for I := 0 to Page.Count-1 do
        FFormDispatcher.AddElement(Page[I]).CreateControl( TWinControl(result.Control), DataSetMeta );
    end;
end;

procedure TAForm.RemovePage(const aPage : TElementMeta);
var I : integer;
    PageElements : TList;
begin
  if Assigned(aPage) then
    begin
      PageElements := TList.Create;
      aPage.ConvertToList(PageElements);
      for I := 0 to PageElements.Count-1 do
        FFormDispatcher.FindByElement(aPage[I]).Free;
      PageElements.Free;
      FFormDispatcher.FindByElement(aPage).Free;
      aPage.Changes := aPage.Changes + [mcDelete];
    end;
end;

procedure TAForm.MoveToPageClick(Sender : TObject);
var EC, EL, NewOwner : TElementMeta;
begin
  if Sender is TMenuItem then
    begin
      EC := TDeElement(FSizingRect.Element.Tag).ElementMeta;
      NewOwner := TDeElement(TTabSheet(TMenuItem(Sender).Tag).Tag).ElementMeta;
      if Assigned(NewOwner) then
        begin
          // ищем и перемещаем надпись к контролу
          if assigned(EC.Field) and not(EC.ElementType = etLabel)
            then EL:= TElementMeta(EC.Owner).FindByFieldAndType(EC.Field, [etLabel])
            else EL:= nil;

          if assigned(EL) then
            begin
              EL.Owner.Extract(EL);
              NewOwner.Add(EL);
              EL.Changes := EL.Changes + [mcUpdate];
              FFormDispatcher.FindByElement(El).Control.Parent:= TTabSheet(TMenuItem(Sender).Tag);
            end;

          EC.Owner.Extract(EC);
          NewOwner.Add(EC);
          EC.Changes:= EC.Changes + [mcUpdate];
          TDeElement(FSizingRect.Element.Tag).Control.Parent:= TTabSheet(TMenuItem(Sender).Tag);

          FSizingRect.Element := _PageControl;
          FFormDispatcher.SortElements;
        end;
    end;
end;

procedure TAForm.ReloadImages;
var OldHeight:Integer;
begin
  OldHeight:=BtnPanel.Height;

//  MForm.setToolBarStyle(ToolBarL, MForm.MenuIcoSize, 2);
//  MForm.setToolBarStyle(ToolBarR, MForm.MenuIcoSize, 2);

  CloseBtnPanel.DisableAlign;
  CloseBtnPanel.Visible:=False;
  CloseBtnPanel.Images         := DM.GetEImageList;
  CloseBtnPanel.DisabledImages := DM.GetDImageList;
  CloseBtnPanel.HotImages      := DM.GetHImageList;
  CloseBtnPanel.Visible:=True;
  if assigned(CloseBtnPanel.Images) then CloseBtnPanel.ButtonHeight:= CloseBtnPanel.Images.Height
                                    else CloseBtnPanel.ButtonHeight:= 16;
  ToolBarL.EnableAlign;

  ToolBarL.DisableAlign;
  ToolBarL.Visible:=False;
  ToolBarL.Images         := DM.GetEImageList;
  ToolBarL.DisabledImages := DM.GetDImageList;
  ToolBarL.HotImages      := DM.GetHImageList;
  ToolBarL.Visible:=True;
  if assigned(ToolBarL.Images) then ToolBarL.ButtonHeight:= ToolBarL.Images.Height
                               else ToolBarL.ButtonHeight:= 16;
  ToolBarL.EnableAlign;

  ToolBarR.DisableAlign;
  ToolBarR.Visible:=False;
  ToolBarR.Images         := DM.GetEImageList;
  ToolBarR.DisabledImages := DM.GetDImageList;
  ToolBarR.HotImages      := DM.GetHImageList;
  ToolBarR.Visible:=True;
  if assigned(ToolBarR.Images) then ToolBarR.ButtonHeight:= ToolBarR.Images.Height
                               else ToolBarR.ButtonHeight:= 16;
  ToolBarR.EnableAlign;

  BtnPanel.Height       := ToolBarL.ButtonHeight+2;

  Constraints.MinHeight := BtnPanel.Height + (Height - ClientHeight) +
    (PropPanel.ClientHeight - (_PageControl.Top + _PageControl.Height)) +
    MinPageControlHeight;
      {
  if Parent=nil then Height        := Height+BtnPanel.Height-OldHeight
                else Parent.Height := Parent.Height+BtnPanel.Height-OldHeight;
    {}
  BtnPanel.Realign;
end;

procedure TAForm.InitForm;
var i,NewHeight : Integer;
    TMI         : TMenuItem;
    TabRight    : TTabSheet;
    V           : Boolean;
begin

  ReloadImages;
  FPopupPos:= Point(0,0);

  if FInDesign then
    Go_SettingsExit(smNo);

  inherited InitForm;

  _PageControl.DoubleBuffered:=True;

  { удаляем классы элементов форм, не соответствующие выбранному набору данных }
  if not ValidFormDispatcher(FFormDispatcher, DataSetMeta.Table.Table) then
    FreeAndNil(FFormDispatcher);

  { создаем форму под требуемый набор данных }
  if not Assigned(FFormDispatcher) then
    begin
      FFormDispatcher:= CreateFormDispatcher(DataSetMeta.Table.Table);
      FFormDispatcher.OnChange:= EditAllChange;
    end;

  if not Assigned(DataSetMeta.SelectedForm) then
    DataSetMeta.SelectedForm:= DataSetMeta.Table.Elements.AddDefaultForm
  else
    if DataSetMeta.SelectedForm.Count = 0 then
      begin
        DataSetMeta.SelectedForm.CreateBlankPage;
//        DataSetMeta.SelectedForm.SaveElements;
      end;

  if Not Visible then // First Form Init
    if Floating then Constraints.MinWidth  := Max(400, MForm.Width div 2);
  Height:= Max(9, DataSetMeta.SelectedForm.EH )* YStep + 4 + BtnPanel.Height;

  FFormDispatcher.SetElementsForm(DataSetMeta.SelectedForm);

  if Assigned(DataSetMeta.SelectedForm) then
    FFormDispatcher.CreateControls(_PageControl, DataSetMeta);

  NewHeight:= Max(9, DataSetMeta.SelectedForm.EH) * YStep + 4 + BtnPanel.Height;

  if NewHeight<>Height then Height:= NewHeight;

  if (DataSetMeta.Role <> drMain) or (DataSetMeta.UsageType = utTemporary) or (not Variables.AsBoolean[RegShowCardArea])
    then Height := Height + (Height-ClientHeight);
  Tag := Height;

  Caption:= ' ' + GetTitle(DataSetMeta.Table.Name);
  DataSetMeta.UpdateVisualizationForm;
  FFirstUpdate:= true;

  ActionMenu.Items.Clear;
  // 23.11.2015 * Сделал отображение только тех действий, которые доступны в контекстном меню ...
  for I := 0 to Pred(DataSetMeta.UserActionList.ActionCount) do
    if TDeActionData(DataSetMeta.UserActionList.Actions[i].Tag).CheckPolicy(spSelect) and
      (TDeActionData(DataSetMeta.UserActionList.Actions[i].Tag).DataSet = DataSetMeta.Table.ID) and
      (TDeActionData(DataSetMeta.UserActionList.Actions[i].Tag).Active) and
      (TDeActionData(DataSetMeta.UserActionList.Actions[i].Tag).Category = acAction) then
      begin
        TMI:= TMenuItem.Create(ActionMenu);
        TMI.Action:=DataSetMeta.UserActionList.Actions[i];
        ActionMenu.Items.Add(TMI);
      end;
  // 23.11.2015 -
  V:=False;
  for i:=0 to ActionBtn.DropdownMenu.Items.Count-1 do
    if (not ActionBtn.DropdownMenu.Items[i].IsLine) and (ActionBtn.DropdownMenu.Items[i].Visible) then
        begin
          V:= True;
          Break;
        end;
  ActionBtn.ImageIndex:=DM.MapIconIndex(126);
  ActionBtn.Visible := V;

  if DataSetMeta.Table.isShowRight then
    begin
      TabRight:= TTabSheet.Create(_PageControl);
      TabRight.PageControl:= _PageControl;
      TabRight.Caption:= GetTitle('_Dt.Right',ttSecondName);

      FFormRight:=TDeUserRight.Create(TabRight, DataSetMeta.Table);
      LangFormUpdate(FFormRight);

      if not TabRight.HandleAllocated then TabRight.HandleNeeded;
      FFormRight.ManualDock(TabRight);
      FFormRight.Align:= alClient;
      FFormRight.Visible:= True;

      if Assigned(FCacheItem) then FFormRight.ObjectID:= FCacheItem.ID
                              else FFormRight.ObjectID:= unAssigned;
    end;
end;

procedure TAForm.ReCreateView;
var
   FForms: TList<TAForm>;
   i,FormID: Integer;
begin
  FForms:= TList<TAForm>.Create;
  _PageControl.Hide;

  // построение списка форм и сохранение актуальных значений в Control'ах
  FormID:= FFormDispatcher.ElementsForm.ID;
  for i:= 0 to MetaData.DataSetsCount-1 do
    if (MetaData.DataSets[i] <> DataSetMeta)
      and Assigned(MetaData.DataSets[I].CardForm)
      and (MetaData.DataSets[i].CardForm is TAForm)
      and (TAForm(MetaData.DataSets[i].CardForm).FFormDispatcher.ElementsForm.Id = FormID) then
        begin
          FForms.Add(TAForm(MetaData.DataSets[I].CardForm));
          TAForm(MetaData.DataSets[i].CardForm).FFormDispatcher.StoreValues(
            TAForm(MetaData.DataSets[i].CardForm).CacheItem);
        end;

  FFormDispatcher.StoreValues(CacheItem);
  FFormDispatcher.DestroyControls;
  FFormDispatcher.Clear;
  DataSetMeta.Table.ClearElements;
  DataSetMeta.Table.LoadElements;

  if DataSetMeta.Table.Elements.Count = 1 then
    if DataSetMeta.Table.Elements[0].count = 0 then
      DataSetMeta.SelectedForm:= DataSetMeta.Table.Elements.AddDefaultForm;

  FFormDispatcher.SetElementsForm(DataSetMeta.SelectedForm);
  FFormDispatcher.CreateControls(_PageControl, DataSetMeta);

  for i:= 0 to FForms.Count-1 do
    begin
      FForms[i].FFormDispatcher.SetElementsForm(DataSetMeta.SelectedForm);
      FForms[i].FFormDispatcher.CreateControls(_PageControl, DataSetMeta);
    end;

  if Floating then
    BringToFront;
  FForms.Free;

  FFormDispatcher.RestoreValues;
  if Assigned(CacheItem) and (not (CacheItem is TRootItem)) then
    FFormDispatcher.Initialize;

  _PageControl.Show;
  inherited;
end;

procedure TAForm.CheckDesignModeClosed;
begin
  if FInDesign then
    Go_SettingsExit(smQuery);
end;

function TAForm.GetModified: Boolean;
begin
  if Assigned(CacheItem) then
    begin
      FFormDispatcher.GetCacheValues(CacheItem);
      result := Assigned(CacheItem) and (isModified in CacheItem.State);
    end
  else
    result := false;
end;

function TAForm.CanClose: boolean;
var
  MesResult : Integer;
begin
  result := True;
  if GetModified then
    begin
      MesResult := Application.MessageBox(PChar(GetTitle('_query.savechanges')),
                                      PChar(GetTitle('_Dl.Confirmation')),
                                      MB_YESNOCANCEL or MB_ICONQUESTION or MB_APPLMODAL);
      case MesResult of
       IDNO     :begin
                   CacheItem.RollbackChanges;
                   FFormDispatcher.SetCacheValues(CacheItem);
                 end;
       IDYES    :begin
                   Go_Ok.Execute;
                 end;
       IDCANCEL :result := False;
      end;
    end;
end;

procedure TAForm.Go_Da_SaveExecute(Sender: TObject);
begin
  Go_SettingsExit(smSave);
end;

procedure TAForm.Go_Da_UndoExecute(Sender: TObject);
begin
  if DataSetMeta.SelectedForm.WasChanged then
    Go_SettingsExit(smUndo)
  else
    Go_SettingsExit(smNo);
end;

procedure TAForm.FormKeyPress(Sender: TObject; var Key: Char);
const keyESC: Char = Chr(27);
var FE : TDeElement;
begin
  if Not FInDesign then Exit;

  if (Key=keyESC) then
    if Assigned(FSizingRect) then
      begin
        FE:=TDeElement(FSizingRect.Element.Tag);
        if Assigned(FE) then
          begin
            if FE.ElementMeta.Owner<>nil then
              begin
                FE:=FFormDispatcher.FindByElement(TElementMeta(FE.ElementMeta.Owner));
                if assigned(FE.Control) then
                  FSizingRect.Element := FE.Control
                else
                  FSizingRect.Element := _PageControl;
              end;
          end
        else
          begin
            FSizingRect.Element := _PageControl;
          end;
      end;
end;

procedure TAForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var FE : TDeElement;
begin
  if (Not FInDesign) or (Not Assigned(FSizingRect))
   then
    begin
      inherited;
      Exit;
    end;

  FE:=TDeElement(FSizingRect.Element.Tag);
  if Shift=[] then
    case Key of
      VK_TAB : begin
                 FE.ElementMeta.EL:=FE.ElementMeta.EL-1;
                 FE.ElementMeta.ER:=FE.ElementMeta.ER-1;
               end;
      VK_HOME: begin
                 FE.ElementMeta.ER:=FE.ElementMeta.ER-FE.ElementMeta.EL;
                 FE.ElementMeta.EL:=0;
               end;
      VK_LEFT  : begin
                 FE.ElementMeta.EL:=FE.ElementMeta.EL-1;
                 FE.ElementMeta.ER:=FE.ElementMeta.ER-1;
               end;
      VK_RIGHT:begin
                 FE.ElementMeta.EL:=FE.ElementMeta.EL+1;
                 FE.ElementMeta.ER:=FE.ElementMeta.ER+1;
               end;
      VK_UP  : begin
                 FE.ElementMeta.ET:=Max(0, FE.ElementMeta.ET-1);
               end;
      VK_DOWN: begin
                 FE.ElementMeta.ET:=FE.ElementMeta.ET+1;
               end;
    end;

  if Shift=[ssShift] then
    case Key of
      VK_LEFT  : begin
                 FE.ElementMeta.ER:=FE.ElementMeta.ER-1;
               end;
      VK_RIGHT:begin
                 FE.ElementMeta.ER:=FE.ElementMeta.ER+1;
               end;
      VK_UP  : begin
                 FE.ElementMeta.EH:= Min(3, FE.ElementMeta.EH-1);
               end;
      VK_DOWN: begin
                 FE.ElementMeta.EH:=FE.ElementMeta.EH+1;
               end;
    end;

  if Shift=[ssCtrl] then
    case Key of
      VK_LEFT: begin
                 FE.ElementMeta.EAL:= Max( 0, FE.ElementMeta.EAL-1);
                 FE.ElementMeta.EAR:= Max( 0, FE.ElementMeta.EAR-1);
               end;
      VK_RIGHT:begin
                 FE.ElementMeta.EAL:= Min(12, FE.ElementMeta.EAL+1);
                 FE.ElementMeta.EAR:= Min(12, FE.ElementMeta.EAR+1);
               end;
    end;

  if Shift=[ssCtrl, ssShift] then
    case Key of
      VK_LEFT: begin
                 FE.ElementMeta.EAR:= Max( 0, FE.ElementMeta.EAR-1);
               end;
      VK_RIGHT:begin
                 FE.ElementMeta.EAR:= Min(12, FE.ElementMeta.EAR+1);
               end;
    end;

  FE.ElementMeta.SetBounds(FSizingRect.Element);
  FSizingRect.Element.Update;

  FSizingRect.SetBounds(FE.Control.BoundsRect.Left,FE.Control.BoundsRect.Top,FE.Control.BoundsRect.Right,FE.Control.BoundsRect.Bottom);
  FSizingRect.Update;

  FAnchorRuler.LeftAnchor := FE.ElementMeta.EAL;
  FAnchorRuler.RightAnchor:= FE.ElementMeta.EAR;
end;

procedure TAForm.Go_Da_AutoAnchorExecute(Sender: TObject);
  procedure AutoInner(DE: TDeElement);
  var
    EM,LEM: TElementMeta;
    CWidth,i,l,ll,r : Integer;
  begin
    EM:=DE.ElementMeta;

    CWidth:=DE.Control.Parent.ClientWidth;
    l := ((CWidth*EM.EAL) div (AnchorsCount-1))+XStep*EM.EL;
    r := ((CWidth*EM.EAR) div (AnchorsCount-1))+XStep*EM.ER;

    case EM.ElementType of
      etLabel: begin
                 EM.EAL:= Trunc(l / (CWidth / (AnchorsCount-1)));
                 EM.EL := Round( (l-((CWidth*EM.EAL)div(AnchorsCount-1)))/ XStep);

                 EM.EAR:=EM.EAL;
                 EM.ER := Round( (r-((CWidth*EM.EAR)div(AnchorsCount-1)))/ XStep);

                 EM.SetBounds(DE.Control);
               end;
     etDefault, etDateTime, etTime, etTreeBox, etIconBox,
     etCheckListBox, etFieldsBox, etTablesBox, etLinkedCombo, etColorComboBox,
     etInteger, etFloat, etString
             : begin
                 ll:=MaxInt;
                 for i:=0 to EM.Owner.Count-1 do
                   begin
                     LEM:=TElementMeta(EM.Owner.Items[i]);
                     if (LEM<>EM) and (LEM.ElementType=etLabel) and (LEM.Field=EM.Field) then
                       ll:=LEM.EAL;
                   end;
                   
                 EM.EAL:= Min(ll,Trunc(l / (CWidth/(AnchorsCount-1))));
                 EM.EL := Round( (l-((CWidth*EM.EAL) div (AnchorsCount-1)))/ XStep);

                 EM.EAR:= Round(      (r / (CWidth/(AnchorsCount-1))));
                 EM.ER := Round( (r-((CWidth*EM.EAR) div (AnchorsCount-1)))/ XStep);

                 EM.SetBounds(DE.Control);
               end;
    end;

    for i:=0 to EM.Count-1 do
      AutoInner(FFormDispatcher.FindByElement(TElementMeta(EM.Items[i])));
  end;
begin
  AutoInner(  TDeElement(FSizingRect.Element.Tag) );
  FSizingRect.Element := TDeElement(FSizingRect.Element.Tag).Control;
  FSizingRect.Update;
end;

procedure TAForm.OnAnchorChange(Sender : TObject);
var
  emC, emL : TElementMeta;
  elementL: TDeElement;
  W, X, AnchorL, AnchorR : Integer;
begin
  if FSizingRect.Element.Tag = 0 then Exit;
  emC := TDeElement(FSizingRect.Element.Tag).ElementMeta;
  if not Assigned(emC) then Exit;

  X:=0;
  W:= TWinControl(FSizingRect.Element).Parent.ClientWidth - (MarginLeft + MarginRight);

  while ((W * FAnchorRuler.LeftAnchor)  div (AnchorsCount - 1)) + XStep * (emC.EL+X) < (0 - XStep) do Inc(X);
  while ((W * FAnchorRuler.RightAnchor) div (AnchorsCount - 1)) + XStep * (emC.ER+X) > (W + XStep) do Dec(X);

  if assigned(emC.Field) and assigned(emC.Owner) then
    begin
      emL:= TElementMeta(emC.Owner).FindByFieldAndType(emC.Field, [etLabel]);
      if Assigned(emL) then
        begin
          AnchorL:= emL.EAL;
          AnchorR:= emL.EAR;

          if (emC.EAL <> FAnchorRuler.LeftAnchor) then
            begin
              if AnchorL = emC.EAL then emL.EAL:= FAnchorRuler.LeftAnchor;
              if AnchorR = emC.EAL then emL.EAR:= FAnchorRuler.LeftAnchor;
            end;

          if (emC.EAR <> FAnchorRuler.RightAnchor) and (AnchorL <> AnchorR) then
            begin
              if AnchorL = emC.EAR then emL.EAL:= FAnchorRuler.RightAnchor;
              if AnchorR = emC.EAR then emL.EAR:= FAnchorRuler.RightAnchor;
            end;

          if (AnchorL<>emL.EAL) or (AnchorR<>emL.EAR) then
            begin
              while ((W * FAnchorRuler.LeftAnchor)  div (AnchorsCount - 1)) + XStep * (emL.EL+X) < (0 - XStep) do Inc(X);
              while ((W * FAnchorRuler.RightAnchor) div (AnchorsCount - 1)) + XStep * (emL.ER+X) > (W + XStep) do Dec(X);

              emL.EL:= emL.EL + X;
              emL.ER:= emL.ER + X;

              elementL:= FindByElement(emL);
              if Assigned(elementL) then
                emL.SetBounds(elementL.Control, True);
            end;
        end;
    end;

  emC.EAL:= FAnchorRuler.LeftAnchor;
  emC.EAR:= FAnchorRuler.RightAnchor;

  if  (FAnchorRuler.ArrowDragged in [adLeft, adBoth]) or
     ((FAnchorRuler.ArrowDragged in [adRight]) and (X>0)) then emC.EL:= emC.EL + X;
  if  (FAnchorRuler.ArrowDragged in [adBoth, adRight]) or
     ((FAnchorRuler.ArrowDragged in [adLeft]) and (X<0))  then emC.ER:= emC.ER + X;

  emC.SetBounds(FSizingRect.Element, True);
  FSizingRect.Update;
end;

procedure TAForm.WMHScroll(var Message: TWMHScroll);
var
  SI : TScrollInfo;
begin
  inherited;
  SI.cbSize := sizeOf(SI);
  SI.fMask  := SIF_ALL;
  getScrollInfo(Handle,SB_HORZ,SI);
end;

procedure TAForm.WMVScroll(var Message: TWMVScroll);
var
  SI : TScrollInfo;
begin
  inherited;
  SI.cbSize := sizeOf(SI);
  SI.fMask  := SIF_ALL;
  getScrollINfo(Handle,SB_VERT,SI);
end;

procedure TAForm.PropPanelResize(Sender: TObject);
var W,H: Integer;
begin
  inherited;
  W:= ClientWidth;
  if PanelL.Visible then Dec(W, PanelL.Width + SplitterL.Width);
  if PanelR.Visible then Dec(W, PanelR.Width + SplitterR.Width);

  H:= ClientHeight - BtnPanel.Height;
  if PanelT.Visible then Dec(H, PanelT.Height + SplitterT.Height);
  if PanelB.Visible then Dec(H, PanelB.Height + SplitterB.Height);

  if FInDesign
    then _PageControl.SetBounds( SpaseXY, SpaseXY + AnchorRulerHeight, W - 2 * SpaseXY, H - (3 * SpaseXY) div 2 - AnchorRulerHeight)
    else _PageControl.SetBounds( SpaseXY, SpaseXY,                     W - 2 * SpaseXY, H - (3 * SpaseXY) div 2);
end;


function TAForm.FindByElement(const aElement : TElementMeta) : TDeElement ;
begin
  if Assigned(FFormDispatcher) then Result:=FFormDispatcher.FindByElement(aElement)
                               else Result:=nil;
end;


procedure TAForm.act_Da_UpdateExecute(Sender: TObject);
var
  State : TKeyboardState;

  procedure UpdateGridControl(Sender: TWinControl);
  var i: Integer;
  begin
    if Sender is TBaseGridForm then
      begin
        if TBaseGridForm(Sender).DataSetMeta.Cache.Active then
          TBaseGridForm(Sender).DataSetMeta.Cache.Update(mcUpdate, null)
      end
    else
      for i:=0 to Sender.ControlCount-1 do
        if Sender.Controls[i] is TWinControl then
          UpdateGridControl(TWinControl(Sender.Controls[i]));
  end;

begin
  GetKeyboardState(State);
  if ((State[vk_Control] And 128) <> 0 ) then
    begin
      MetaData.UpdateLibrary(-2);
      UpdateGridControl(_PageControl);
    end
  else
    begin
      UpdateGridControl(_PageControl.ActivePage);
    end;
end;

{ TRecordEditor }

constructor TRecordEditor.Create(const aTable: TTableMeta; const aRecordID: Variant);
begin
  if not assigned(aTable) then Exit;

  { создание набора данных }
  FDataSetMeta := TDataSetMeta.Create(utTemporary, aTable);
  FDataSetMeta.Cache.BeginUpdate; //защищаем от внешних изменений

  if VarIsEmpty(aRecordID) or VarIsNull(aRecordID) then
    begin
      FDataSetMeta.Cache.Filters.NewFilter(FDataSetMeta.Table.KField[0], opIs, Null);
      FDataSetMeta.Cache.PrepareData(True, fsMax);
      FDataSetMeta.Cache.FocusedItem:= FDataSetMeta.Cache.AddNewItem;
      FDataSetMeta.Cache.FocusedItem.Selected:= True;
    end
  else
    begin
      FDataSetMeta.Cache.Filters.NewFilter(FDataSetMeta.Table.KField[0], opEQ, ConvertValue(aRecordID, FDataSetMeta.Table.KField[0].DataType));
      FDataSetMeta.Cache.PrepareData(True, fsMax);
      if FDataSetMeta.Cache.Count = 1 then
        begin
          FDataSetMeta.Cache.FillItems(0, 0, fsMax);
          FDataSetMeta.Cache.FocusedItem:= FDataSetMeta.Cache[0];
          FDataSetMeta.Cache.FocusedItem.Selected:= True;
        end;
    end;
end;

destructor TRecordEditor.Destroy;
begin
  if assigned(FDataSetMeta) then
    begin
      if assigned(FDataSetMeta.CardForm) then FDataSetMeta.CardForm.DeInitForm;
      FDataSetMeta.DestroyRecursive;
    end;
  inherited Destroy;
end;

function TRecordEditor.GetFocusedItem: TCacheItem;
begin
  if Assigned(FDataSetMeta) and Assigned(FDataSetMeta.Cache) then Result:= FDataSetMeta.Cache.FocusedItem
                                                             else Result:= nil;
end;

function TRecordEditor.GetCaption: String;
begin
  if assigned(FDataSetmeta) and Assigned(FDataSetmeta.CardForm) then Result:= FDataSetmeta.CardForm.Caption
                                                                else Result:= EmptyStr;
end;

procedure TRecordEditor.SetCaption(const aCaption : string);
begin
  if assigned(FDataSetmeta) and Assigned(FDataSetmeta.CardForm) then FDataSetmeta.CardForm.Caption := aCaption;
end;

function TRecordEditor.Execute: boolean;
begin
  { если необходимо, то создаем форму визуализации единичной записи }
  if not Assigned(FDataSetMeta.CardForm) then
    begin
      FDataSetMeta.CardForm := TAForm.Create(Application);
      FDataSetMeta.CardForm.ReInitForm(FDataSetMeta);
      TAForm(FDataSetMeta.CardForm).CacheItem:= FDataSetMeta.Cache.FocusedItem;
    end;

  { показываем диалог }
  if FDataSetMeta.CardForm.Caption= EmptyStr then
    FDataSetMeta.CardForm.Caption:= GetTitle(FDataSetMeta.Table.Name);
  result:= (FDataSetMeta.CardForm.ShowModal = mrOK);
end;

function EditRecord(TableMeta: TTableMeta; const ItemID: Variant): Boolean;
var
  RecordEditor: TRecordEditor;
begin
  Result := False;
  if (not VarIsEmpty(ItemID)) and (not VarIsNull(ItemID)) and Assigned(TableMeta) and
      Assigned(SecuritySystem) and SecuritySystem.CheckPolicyDataSet(TableMeta.ID, spSelect) then
    begin
      RecordEditor := TRecordEditor.Create(TableMeta, ItemID);
      if Assigned(RecordEditor.CacheItem) then
        try
          Result := RecordEditor.Execute;
          if Result then
            MetaData.UpdateLibrary(TableMeta.ID);
         finally
         end
      else
        SendMessage( Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar('_dE.linkabsent')));

      RecordEditor.Free;
    end;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('UnitA unit initialization ...');

finalization
  DebugLog('UnitA unit finalization ...');
{$ENDIF}

end.

