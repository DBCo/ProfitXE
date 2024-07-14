unit BaseDataFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Forms, ActnList,
  Actions, Controls, Menus, DB, Graphics, Vcl.ComCtrls, Vcl.ExtCtrls,
  BaseFormUnit, DSMeta, DataCacheUnit
  {, DeDataset};

const
 WM_BF_CHANGEACTIVE     = WM_BF_MESSAGE + 06; //сообщение базовой формы CHANGEACTIVE

type
  TZone = class
    FParent: TWinControl;
    FSplitter: TSplitter;
    FPages: TPageControl;
  end;

type

  PArray =  array of Pointer;

  TBaseDataForm = class(TBaseForm)
    act_Da_Create: TAction;
    PopupMenu: TPopupMenu;
    PMCut: TMenuItem;
    PMCopy: TMenuItem;
    PMPaste: TMenuItem;
    PMProperties: TMenuItem;
    miCCPBeginDelimiter: TMenuItem;
    miCCPEndDelimiter: TMenuItem;
    miPropDelimiter: TMenuItem;
    actCreate_Da_Shortcut: TAction;
    actCreate_Da_Note: TAction;
    miSCNBeginDelimiter: TMenuItem;
    miSCNDelimeter: TMenuItem;
    miAddEndDelimeter: TMenuItem;
    PMSelectall: TMenuItem;
    Invertselection1: TMenuItem;
    ActDaDefault: TMenuItem;
    SplitterL: TSplitter;
    PanelL: TPanel;
    SplitterR: TSplitter;
    PanelR: TPanel;
    SplitterT: TSplitter;
    PanelT: TPanel;
    SplitterB: TSplitter;
    PanelB: TPanel;
    act_Da_Savetofile: TAction;
    MMSelectall: TMenuItem;
    MMInvertselection: TMenuItem;
    MMCut: TMenuItem;
    MMCopy: TMenuItem;
    MMPaste: TMenuItem;
    MMDelete: TMenuItem;
    act_Da_Filter: TAction;
  private
//    FZones: array[Low(TAlign).. High(TAlign)] of TZone;
    FCloseAction : TCloseAction;
    FOldDataOpen : TNotifyEvent;
 //   FOldDataScroll: TDataSetNotifyEvent;
    FOldSelectedChange: TItemEvent;
    FOldFocusedChange: TItemEvent;
    FOldSelectedChanging: TItemEvent;
    FOldFocusedChanging: TItemEvent;
    FOldItemInsert : TItemEvent;
    FOldAllowFocusedChange : TItemAllowEvent;
    FOldSyncronize : TSyncronizeEvent;
    FOnChangeDataSetStatus : TNotifyEvent;
    procedure DoMainFormMessage(Msg: Cardinal; WParam, LParam: NativeInt);
    function GetMainBaseForm: TBaseForm;
    function GetCacheItems: TDataCache;
  protected
    FMainControl: TWinControl;
    FDataSetMeta  : TDataSetMeta;
    procedure SetDataSetMeta(const Value: TDataSetMeta); virtual; abstract;
    procedure Activate; override;
    procedure Deactivate; override;
    procedure InitForm;override;
    procedure AfterInit;override;
    function GetStatusText : string; virtual;
   // procedure AfterDataScroll(DataSet: TDataSet);virtual;
    procedure AfterDataOpen(Sender : TObject);virtual;
    procedure CacheSelectedChanging(OldItem : TCacheItem);virtual;
    procedure CacheFocusedChanging(OldItem : TCacheItem);virtual;
    procedure CacheSelectedChange(Item : TCacheItem);virtual;
    procedure CacheFocusedChange(Item : TCacheItem);virtual;
    procedure CacheItemInsert(Item : TCacheItem);virtual;
    procedure CacheAllowFocusedChange(var NewItem : TCacheItem;var Allow : Boolean);virtual;
    procedure CacheSyncronize(var AFocusedItem: TCacheItem; AIndex: Integer);virtual;
    procedure DoClose(var Action: TCloseAction); override;
    class function CreateNewForm(FormClassName : String; AParam : TObject;
                   CloseAction : TCloseAction = caNone): TBaseDataForm;
    property  CacheItems : TDataCache Read GetCacheItems;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeInitForm; override;
    procedure ReInitForm(AParam : TObject);override;
    procedure AddUnit(aDataForm: TBaseDataForm; aAlign: TAlign = alClient);
    property MainControl : TWinControl read FMainControl;
    // возвращает строку, описывающую статус набора данных
    property StatusText : string read GetStatusText;
    procedure ChangeDataSetStatus;virtual;
    property MainBaseForm : TBaseForm read GetMainBaseForm;
    property DataSetMeta : TDataSetMeta read FDataSetMeta;
    property OnChangeDataSetStatus : TNotifyEvent read FOnChangeDataSetStatus write FOnChangeDataSetStatus;
  end;

  TBaseDataFormClass = class of TBaseDataForm;

//var
//  BaseDataForm: TBaseDataForm;

implementation

uses DeLog, DeTypes, Security, Dictionary, BaseCardFormUnit, DeMeta, DeMetaData, Funcs, DataUnit;

{$R *.dfm}

{ TBaseDataForm }

constructor TBaseDataForm.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FOldDataOpen  := nil;
//  FOldDataScroll:= nil;
  FOldFocusedChange := nil;
  FOldSelectedChange := nil;
  FOldFocusedChanging := nil;
  FOldSelectedChanging := nil;
  FOldItemInsert := nil;
  FOldSyncronize := nil;
  FOldAllowFocusedChange := nil;
  DoMainFormMessage(WM_BF_CREATE, NativeInt(Self), 0);
  Name:=ClassName + IntToStr(Handle);
end;

destructor TBaseDataForm.Destroy;
begin
  inherited;
  DoMainFormMessage(WM_BF_DESTROY, NativeInt(Self), 0);
end;

procedure TBaseDataForm.Activate;
begin
  DoMainFormMessage(WM_BF_ACTIVE, NativeInt(Self), 0);
  inherited; // именно здесь, в AUnit, перекрывается фокус на главную форму если выбрана закладка с таблицей
  ChangeDataSetStatus;
end;

procedure TBaseDataForm.Deactivate;
begin
  inherited;
  DoMainFormMessage(WM_BF_DEACTIVE, NativeInt(Self), 0);
end;

class function TBaseDataForm.CreateNewForm(FormClassName: String;
  AParam : TObject; CloseAction : TCloseAction): TBaseDataForm;
var
  FormClass : TBaseDataFormClass;
begin
  Result := nil;
  try
    FormClass := TBaseDataFormClass(FindClass(FormClassName));
    if not FormClass.InheritsFrom(TBaseForm) then
      raise Exception.Create('Не потомок TBaseForm');
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TBaseDataForm.CreateNewForm error: ' + E.Message);
        {$ENDIF}
        Exit;
      end;
  end;
  Result := FormClass.Create(Application);
  Result.FCloseAction := CloseAction;
  Result.ReinitForm(TDataSetMeta(AParam));
end;

procedure TBaseDataForm.DeInitForm;
begin
  if Assigned(DataSetMeta) then
  begin
  //  DataSetMeta.DataAfterScroll := FOldDataScroll;
    DataSetMeta.Cache.OnDataAfterOpen   := FOldDataOpen;
    DatasetMeta.OnCacheFocusedChange := FOldFocusedChange;
    DatasetMeta.OnCacheSelectedChange:= FOldSelectedChange;
    DatasetMeta.OnCacheFocusedChanging := FOldFocusedChanging;
    DatasetMeta.OnCacheSelectedChanging:= FOldSelectedChanging;
    DatasetMeta.OnAllowFocusedChanging := FOldAllowFocusedChange;
    DatasetMeta.OnInsertRecord := FOldItemInsert;
    DatasetMeta.Cache.OnBeforeSyncronize := FOldSyncronize;

    FDataSetMeta:= nil;
  end;
end;

procedure TBaseDataForm.InitForm;
begin
  inherited;
  //код инициализации формы в зависимости от набора параметров
end;

procedure TBaseDataForm.AfterInit;
begin
  inherited;
  if Not assigned(DataSetMeta) then Exit;

  // FOldDataScroll := DataSetMeta.DataAfterScroll;
  FOldDataOpen    := DataSetMeta.Cache.OnDataAfterOpen;
  // DataSetMeta.DataAfterScroll:=AfterDataScroll;
  DataSetMeta.Cache.OnDataAfterOpen := AfterDataOpen;
  FOldFocusedChange := DatasetMeta.OnCacheFocusedChange;
  DatasetMeta.OnCacheFocusedChange := CacheFocusedChange;
  FOldSelectedChange :=  DatasetMeta.OnCacheSelectedChange;
  DatasetMeta.OnCacheSelectedChange:= CacheSelectedChange;
  FOldFocusedChanging := DatasetMeta.OnCacheFocusedChanging;
  DatasetMeta.OnCacheFocusedChanging := CacheFocusedChanging;
  FOldSelectedChanging :=  DatasetMeta.OnCacheSelectedChanging;
  DatasetMeta.OnCacheSelectedChanging:= CacheSelectedChanging;
  FOldAllowFocusedChange := DatasetMeta.OnAllowFocusedChanging;
  DatasetMeta.OnAllowFocusedChanging := CacheAllowFocusedChange;
  FOldItemInsert := DatasetMeta.OnInsertRecord;
  DatasetMeta.OnInsertRecord := CacheItemInsert;
  FOldSyncronize := DatasetMeta.Cache.OnBeforeSyncronize;
  DatasetMeta.Cache.OnBeforeSyncronize := CacheSyncronize;
end;

procedure TBaseDataForm.ReInitForm(AParam: TObject);
begin
  if (AParam is TDataSetMeta) then
    begin
      SetDataSetMeta(TDataSetMeta(AParam));
      InitForm;
      AfterInit;
    end
  else
    begin // форма без указания датасета на время настройки UnitA
      SetDataSetMeta(nil);
      LangFormUpdate(Self);
    end;
end;

procedure TBaseDataForm.DoClose(var Action: TCloseAction);
begin
  //if DataSetMeta.Role = drParent then
  inherited ;
  Action := caFree;
  {if FCloseAction <> caNone then
    Action := FCloseAction;}
end;

procedure TBaseDataForm.AddUnit(aDataForm: TBaseDataForm; aAlign: TAlign);
begin

end;

procedure TBaseDataForm.DoMainFormMessage(Msg: Cardinal; WParam, LParam: NativeInt);
begin
  if Assigned(MainBaseForm) then
     MainBaseForm.Perform(Msg,WParam,LParam);
end;

function TBaseDataForm.GetMainBaseForm: TBaseForm;
begin
  If Assigned(Application.MainForm) and  (Application.MainForm is TBaseForm)then
    result := (Application.MainForm as TBaseForm)
  else
    result := nil;
end;

function TBaseDataForm.GetStatusText : string;
begin
  if not DataSetMeta.Cache.Active then
    result := '('+GetTitle('_dl.unavailable')+')'
  else
    result := GetTitle('_quantity: ')+Format('%d',[DataSetMeta.Cache.Count]);
end;

procedure TBaseDataForm.ChangeDataSetStatus;
var
  DSMeta, CanExecute, CanAdd, CanDelete : Boolean;
  tmp : Integer;
begin
  DSMeta :=Assigned(DataSetMeta);
  CanExecute := DSMeta
            and DataSetMeta.Cache.Active
            and(Assigned(CacheItems.FocusedItem))
            and (not (CacheItems.FocusedItem is TRootItem));
  CanAdd := DSMeta
            and DataSetMeta.Cache.Active
            and ((not Assigned(SecuritySystem))
                  or
                  SecuritySystem.CheckPolicyDataSet( DataSetMeta.Table.ID, spInsert))
            and (not CacheItems.TableMeta.IsReadOnly);
  CanDelete := DSMeta
            and DataSetMeta.Cache.Active
            and ((not Assigned(SecuritySystem))
                 or SecuritySystem.CheckPolicyDataSet(DataSetMeta.Table.ID, spDelete))
            and (not CacheItems.TableMeta.IsReadOnly)
            and (CacheItems.TableMeta <> MetaData.MetaTables[idxSolutions]);
  act_Da_Create.Enabled := DSMeta
            and ((CanAdd
                  and (not DataSetMeta.Table.IsInterrelations))
                 or(DataSetMeta.Table.IsInterrelations
                    and (DataSetMeta.Role = drChild)));
  act_Da_Delete.Enabled := DSMeta and CanDelete and CanExecute;
  act_Da_Cut.Enabled    := DSMeta and CanDelete and CanExecute;
  act_Da_Copy.Enabled   := DSMeta and CanExecute;
  act_Da_Paste.Enabled  := DSMeta
            and((CanAdd
                 and CacheItems.CanPasteFromClipboard(True,tmp)
                 and (not DataSetMeta.Table.IsInterrelations))
                or(DeClipboard.ContainsDeData
                   and DataSetMeta.Table.IsInterrelations
                   and (DataSetMeta.Role = drChild)));
  act_Da_Selectall.Enabled := DSMeta
            and CanExecute
{            and (DataSetMeta.Role <> drParent){};
  act_Da_Invertselection.Enabled := DSMeta
            and CanExecute
{           and (DataSetMeta.Role <> drParent) {};

  act_Da_Default.Visible := DSMeta
            and CanDelete and CanExecute
            and(CacheItems.SelectedCount=1)
            and(DataSetMeta.Table.OField<>nil);
  if Act_Da_Default.Visible then
    Act_Da_Default.Checked := CacheItems.FocusedItem.Default;

  actCreate_Da_Shortcut.Enabled := DSMeta
            and DeClipboard.ContainsDeData
            and ((CanExecute
                  and (not DataSetMeta.Table.IsInterrelations))
                 or(DataSetMeta.Table.IsInterrelations
                    and (DataSetMeta.Role = drChild)));
  if DSMeta and DataSetMeta.Table.IsInterrelations then
    act_Da_Create.Enabled := act_Da_Create.Enabled
                            and actCreate_Da_Shortcut.Enabled;
  actCreate_Da_Note.Enabled := DSMeta
            and((CanExecute
                 and (not DataSetMeta.Table.IsNotes))
                or(DataSetMeta.Table.IsNotes
                   and (DataSetMeta.Role = drChild)));

  act_Da_Open.Enabled  := DSMeta
            and DataSetMeta.Cache.Active
            and Assigned(CacheItems.FocusedItem)
            and not (CacheItems.FocusedItem is TRootItem);

  act_Da_Print.Enabled := DSMeta and Assigned(MetaData.MetaTables[idxCommands])
{           and DataSetMeta.Cache.Active
            and (CacheItems.Count > 0) {};
  act_Da_FastPrint.Enabled := DSMeta
            and DataSetMeta.Cache.Active
            and (CacheItems.Count > 0);

  if DSMeta then
  begin
    if Assigned(FOnChangeDataSetStatus) then
      FOnChangeDataSetStatus(Self);
    DoMainFormMessage(DM_STATUSNOTIFY, NativeInt(StatusText), Handle);
  end;
end; 

procedure TBaseDataForm.AfterDataOpen(Sender : TObject);
begin
  if Assigned(FOldDataOpen) then FOldDataOpen(Sender);
end;


{procedure TBaseDataForm.AfterDataScroll(DataSet: TDataSet);
begin
  if Assigned(FOldDataScroll) then FOldDataScroll(DataSet);
end;
{}

procedure TBaseDataForm.CacheFocusedChanging(OldItem : TCacheItem);
begin
  if Assigned(FOldFocusedChanging) then  FOldFocusedChanging(OldItem);
end;

procedure TBaseDataForm.CacheSelectedChanging(OldItem : TCacheItem);
begin
  if Assigned(FOldSelectedChanging) then  FOldSelectedChanging(OldItem);
end;

procedure TBaseDataForm.CacheAllowFocusedChange(var NewItem: TCacheItem;
var  Allow: Boolean);
begin
  if Assigned(FOldAllowFocusedChange) then  FOldAllowFocusedChange(NewItem,Allow);
end;

procedure TBaseDataForm.CacheFocusedChange(Item: TCacheItem);
begin
   if Assigned(FOldFocusedChange) then  FOldFocusedChange(Item);
end;

procedure TBaseDataForm.CacheSelectedChange(Item: TCacheItem);
begin
  if Assigned(FOldSelectedChange) then  FOldSelectedChange(Item);
end;

procedure TBaseDataForm.CacheItemInsert(Item: TCacheItem);
begin
  if Assigned(FOldItemInsert) then  FOldItemInsert(Item);
end;

procedure TBaseDataForm.CacheSyncronize(var AFocusedItem : TCacheItem; AIndex : Integer);
begin
  if Assigned(FOldSyncronize) then  FOldSyncronize(AFocusedItem, AIndex);
end;

function TBaseDataForm.GetCacheItems: TDataCache;
begin
  if Assigned(DataSetMeta) then
    result := DataSetMeta.Cache
  else
    result := nil;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('BaseDataFormUnit unit initialization ...');
  {$ENDIF}
  RegisterClass(TBaseDataForm);
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('BaseDataFormUnit unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

