unit DSMeta;

interface

uses DB, Contnrs, Controls, Classes, ActnList, Math,
     DeMeta, DeTypes, BaseFormUnit, DataCacheUnit, DeDB, DeScript, QueryDescriptor;

type
  /// <summary>способ использования наборов данных:
  /// <para><c>utForm</c> - для визуализации</para>
  /// <para><c>utTemporary</c> - временный</para>
  /// </summary>
  TDSUsageType = (utForm, utTemporary);

  TDSUsageTypes = set of TDSUsageType;

  /// <summary>роль набора данных:
  /// <para><c>drNone</c> - отношения с основным не определены</para>
  /// <para><c>drMain</c> - основной набор данных</para>
  /// <para><c>drParent</c> - родительский по отношению к основному</para>
  /// <para><c>drChild</c> - дочерний по отношению к основному</para>
  /// </summary>
  TDSRole = (drNone, drMain, drParent, drChild);

  TGetKeyMode = (gkmFocused, gkmSelected, gkmAll);

  // набор данных
  TDataSetMeta = class;

  /// <summary>связи набора данных с родителькими наборами данных</summary>
  TDSLink = class
  private
    FDataSetMeta : TDataSetMeta;
    FLinkField   : TFieldMeta;
  public
    constructor Create(aDataSetMeta : TDataSetMeta;  aLinkField : TFieldMeta);
    property DataSetMeta : TDataSetMeta read FDataSetMeta write FDataSetMeta;
    property LinkField : TFieldMeta read FLinkField write FLinkField;
  end;

  TDSLinksList = class(TObjectList)
  private
    function GetLink(const aIndex: Integer): TDSLink;
    procedure SetLink(const aIndex: Integer; const aLink: TDSLink);
  public
    property Items[const Index: Integer]: TDSLink read GetLink write SetLink; default;
    procedure RemoveDataSet(aDataSet : TDataSetMeta);
    {$IFDEF DEBUG}
    procedure DebugLinksLog(const Text: string);
    {$ENDIF}
  end;

  TDataSetsMeta = class;

  TDataSetMeta = class
  private
    FContext                 : TContextMeta;    // класс, содержащий параметры визуализации набора данных
    FFullDestroy             : Boolean;         // Флаг, отвечающий за способ уничтожения (только сама/сама и дети)
    FGridForm                : TBaseForm;       // Форма, отображающиая список записей (пока не используется)
    FCardForm                : TBaseForm;       // Форма, отображающая конкретную запись
    FSelectedForm            : TElementMeta;    // один из дочерних элементов первого уровня формы
    FOwnerLinks              : TDSLinksList;    // список связей с родительскими наборами данных
    FChildren                : TDataSetsMeta;   // список дочерних классов TDataSetMeta
    FTable                   : TTableMeta;      // метаописание таблицы, из которой читаются данные
    FUsageType               : TDSUsageType;    // способ использования набора данных
    FCache                   : TDataCache;      // записи набора данных
    FOnCacheFocusedChange    : TItemEvent;
    FOnCacheSelectedChange   : TItemEvent;
    FParser                  : TDeParser;
    FFilter                  : string;          // дополнительный фильтр
    FFilterPostfix           : TFilterItem;
    FUserFilterPostfix       : TFilterItem;     // фильтр который строит пользователь
    FFormFilterPostfix       : TFilterItem;     // фильтр из контекта окна (интервал дат на календаре, регион карты ...)
    FRole                    : TDSRole;         // роль в иерархии
    FOnInsertRecord          : TItemEvent;
    FOnCacheSelectedChanging : TItemEvent;
    FOnCacheFocusedChanging  : TItemEvent;
    FLockField               : TFieldMeta;
    FGroupFields             : TFieldsMeta;
    FGroupFieldsData         : array of Variant;
    FOnAllowFocusedChanging  : TItemAllowEvent;
    FUserActionList          : TActionList;
    FFenchLinkIndex          : Integer;         // <> -1: Признак того, что все записи начитаны
    FFetchLinkVaues          : Variant;         // Массив недочитанных записей
    function GetSelectedForm : TElementMeta;
    function GetChildrenCount : Integer;
    procedure InternalFocusedChange(Item : TCacheItem);
    procedure InternalSelectedChange(Item : TCacheItem);
    procedure InternalFocusedChanging(OldItem : TCacheItem);
    procedure InternalSelectedChanging(OldItem : TCacheItem);
    procedure InternalAllowFocusedChanging(var NewItem : TCacheItem;var Allow : Boolean);
    procedure UpdateChildren;
    function GetIsFoldersTree : boolean;
    procedure SetCardForm(const Value: TBaseForm);
    procedure SetGridForm(const Value: TBaseForm);
    function GetParentKey(const aIndex: Integer): Variant;
    procedure SetParentKey(const aIndex: Integer; aValue: Variant);
    procedure DataSetupFilter(aDescr : TCustomQueryDescr);
    function DataCanOpen(aDataSet : TDeDataSet) : boolean;
    function CanOpenData : boolean;
    procedure CacheInsertRecord(aCacheItem : TCacheItem;  var aCanInsert : boolean);
    procedure CacheInsertNote(aCacheItem : TCacheItem;  var aCanInsert : boolean);
    procedure CacheInsertInterrelation(aCacheItem : TCacheItem;  var aCanInsert : boolean);
    procedure SetFilter(const aFilter: string);
    {$IFDEF DEBUG}
    procedure DebugActionsLog(const Text: string);
    {$ENDIF}
    function GetAllFetched: Boolean;
    function GetIco: Integer;
  public
    constructor Create(const aUsageType: TDSUsageType; aTable: TTableMeta); overload;
    constructor Create(const aUsageType: TDSUsageType; aContext: TContextMeta); overload;
    destructor Destroy; override;
    procedure DestroyRecursive;
    procedure TableActionMenu(Sender: TObject);
    property GridForm   : TBaseForm read FGridForm write SetGridForm;   // Форма, отображающиая список записей
    property CardForm   : TBaseForm read FCardForm write SetCardForm;   // Форма, отображающая конкретную запись
    property Cache : TDataCache read FCache;
    property SelectedForm: TElementMeta read GetSelectedForm write FSelectedForm;
    property Children : TDataSetsMeta read FChildren;
    property OwnerLinks : TDSLinksList read FOwnerLinks;
    /// <summary>количество дочерних наборов данных</summary>
    property ChildrenCount: Integer read GetChildrenCount;
    /// <summary>метаописание таблицы</summary>
    property Table: TTableMeta read FTable;
    property Ico: Integer read GetIco;
    /// <summary>список действий для таблицы</summary>
    property UserActionList : TActionList read FUserActionList;
    /// <summary>способ использования набора данных</summary>
    property UsageType: TDSUsageType read FUsageType;
    // перехват событий кэша
    property OnCacheFocusedChanging  : TItemEvent read FOnCacheFocusedChanging write FOnCacheFocusedChanging;
    property OnCacheSelectedChanging  : TItemEvent read FOnCacheSelectedChanging write FOnCacheSelectedChanging;
    property OnCacheFocusedChange  : TItemEvent read FOnCacheFocusedChange write FOnCacheFocusedChange;
    property OnCacheSelectedChange  : TItemEvent read FOnCacheSelectedChange write FOnCacheSelectedChange;
    property OnAllowFocusedChanging  : TItemAllowEvent read FOnAllowFocusedChanging write FOnAllowFocusedChanging;
    property OnInsertRecord  : TItemEvent read FOnInsertRecord write FOnInsertRecord;
    property Context: TContextMeta read FContext;
    /// <summary>дополнительный фильтр</summary>
    property Parser : TDeParser read FParser;
    property Filter : string read FFilter write SetFilter;
    property FilterPostfix : TFilterItem read FFilterPostfix;
    property UserFilterPostfix : TFilterItem read FUserFilterPostfix;
    property FormFilterPostfix : TFilterItem read FFormFilterPostfix;
    // =true, если набор данных представляет собой дерево папок
    property IsFoldersTree : boolean read GetIsFoldersTree;
    /// <summary>роль набора данных</summary>
    property Role: TDSRole read FRole write FRole;
    property GroupFields : TFieldsMeta read FGroupFields;
    property LockField : TFieldMeta read FLockField write FLockField;
    property ParentKey[const Index: Integer]: Variant read GetParentKey write SetParentKey;
    /// <summary>Функция проверки доступности набора данных</summary>
    /// <returns>True - данные набора доступны</returns>
    function DataEnabled: Boolean;
    /// <summary>Функция получения значений ключевых полей</summary>
    /// <returns>Возвращает вариантный массив значений ключевых полей</returns>
    function GetKeyValue(const aGetMode: TGetKeyMode = gkmSelected): Variant;
    /// <summary>проставляет в запись значения внешних ключей</summary>
    procedure SetReferenceKeys(aCacheItem : TCacheItem);
    /// <summary>определяет и проставляет в запись значения определенные текущим фильтром "="</summary>
    function SetFilterKeys(aCacheItem : TCacheItem; var Ind: integer; DoValue: Boolean = False): Boolean;
    /// <summary>ищет набор данных с требуемой таблицей среди дочерних</summary>
    function FindChild(aTableMeta : TTableMeta) : TDataSetMeta;
    /// <summary>устанавливает дочерний набор данных;  инициализирует дочерний элемент</summary>
    procedure SetChild(aChildMeta : TDataSetMeta;  aLinkField : TFieldMeta = nil);
    /// <summary>создает дочерний набор данных</summary>
    function CreateChild(const aLinkID: Integer; aLinkField: TFieldMeta): TDataSetMeta;
    /// <summary>организует группировку набора данных по какому-либо поля</summary>
    function CreateParent(aGroupField : TFieldMeta) : TDataSetMeta;
    procedure UpdateVisualizationForm;
    // <summary>методы для обработки ярлыков</summary>
    procedure GetLinkedData(const aItemIndex: Integer;  var aTable: TTableMeta; var aItemID: Variant);
    // <summary>Ищет и формирует связи между родительскими наборами</summary>
    procedure OwnersWedding;
    // <summary>метод обновления списка действий</summary>
    procedure UpdateActions(const FullUpdate: Boolean = False);
    /// <summary>Метод получения всех фильтров набора данных</summary>
    procedure ReadAllFilters(FilterItem: TFilterItem);
    function PrepareFetchLinkQueryDescr(Descr: TCustomQueryDescr): Boolean;
    property AllFetched: Boolean read GetAllFetched;
  end;

  /// <summary>список наборов данных</summary>
  TDataSetsMeta = class(TObjectList)
  private
    function Get(const aIndex: Integer): TDataSetMeta;
    procedure Put(const aIndex: Integer; aDataSetMeta: TDataSetMeta);
  public
    destructor Destroy; override;
    property Items[const Index: integer]: TDatasetMeta read Get write Put; default;
  end;

var
  DefaultMeta : TDataSetMeta = nil;

implementation

uses SysUtils, StrUtils, Types, Windows, Variants, Menus, Forms, Actions,
     DeLog, Funcs, Dictionary, Security, DeVariable, DeParser, DataUnit, UnitA, DeMetadata,
     DeSettings, DeActions, DeCalculator, SolutionOperations, DeFunctions
     {$IFDEF DEBUG}, uTextTable{$ENDIF};

{ TDSLink }

constructor TDSLink.Create(aDataSetMeta : TDataSetMeta; aLinkField : TFieldMeta);
begin
  inherited Create;
  FDataSetMeta := aDataSetMeta;
  FLinkField := aLinkField;
end;

{ TDSLinksList }

function TDSLinksList.GetLink(const aIndex: Integer): TDSLink;
begin
  result := TDSLink(inherited Items[aIndex]);
end;

procedure TDSLinksList.SetLink(const aIndex: Integer; const aLink: TDSLink);
begin
  if aIndex >= Count then Count := aIndex + 1;
  inherited Items[aIndex] := aLink;
end;

procedure TDSLinksList.RemoveDataSet(aDataSet : TDataSetMeta);
var I : integer;
begin
  for I := Count-1 downto 0 do
    if Items[I].DataSetMeta = aDataSet then Delete(i);
end;

{$IFDEF DEBUG}
procedure TDSLinksList.DebugLinksLog(const Text: string);
  function PrepareLinksLog: string;
  const
    RoleNames: array[TDSRole] of PChar = ('drNone', 'drMain', 'drParent', 'drChild');
  var
    TextTable: TTextTable;
    Index: Integer;
    Value: string;
  begin
    if Count = 0 then
      Result := EmptyStr
    else
      begin
        TextTable := TTextTable.Create;
        try
          TextTable.Columns.Add('No', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Role', 8, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Table', 32, taLeftJustify, taLeftJustify);
          for Index := 0 to Pred(Count) do
            begin
              TextTable.Lines[Index][0] := IntToStr(Succ(Index));
              if Assigned(Items[Index]) and Assigned(Items[Index].DataSetMeta) then
                begin
                  TextTable.Lines[Index][1] := StrPas(RoleNames[Items[Index].DataSetMeta.Role]);
                  if Assigned(Items[Index].DataSetMeta.Table) then
                    TextTable.Lines[Index][2] := Items[Index].DataSetMeta.Table.Table;
                end;
            end;
            Result := TextTable.AsText(24);
        finally
          TextTable.Free;
        end;
      end;
    if Length(Result) <> 0 then
      Result := #13#10 + Result;
  end;
begin
  DebugLog(Text + PrepareLinksLog);
end;
{$ENDIF}

{ TDataSetMeta }

constructor TDataSetMeta.Create(const aUsageType: TDSUsageType; aTable: TTableMeta);
var I: Integer;
    S: string;
    DeVL : TDeVariableList;
    SupportActions, OtherActions: TList;
    procedure AppendAction(const Index: Integer);
    var
      TempAction: TAction;
      Category: string;
    begin
      TempAction := TAction.Create(FUserActionList);
      Category := ProfitActions[Index].SubCategories;
      if Length(Category) = 0 then
        if ProfitActions[Index].ActionType = atReport then
          Category := '_Print'
        else
          Category := '_Action';
      TempAction.Category := Category;
      Category := ProfitActions[Index].Caption;
      TempAction.Caption := CutTextValue(Category, '|');
      TempAction.Hint := Category;
      TempAction.ShortCut := ProfitActions[Index].ShortCut;
      TempAction.ActionList := FUserActionList;
      TempAction.OnExecute := TableActionMenu;
      TempAction.Enabled := ProfitActions[Index].CheckPolicy(spUpdate);
      TempAction.Tag := NativeInt(ProfitActions[Index]);
      TempAction.ImageIndex := DM.MapIconIndex(ProfitActions[Index].ICO);
    end;
begin
  FContext:= TContextMeta.Create(nil);
  FContext.Dataset:= aTable;

  FFullDestroy := False;

  MetaData.RegisterDataset(Self);
  FUsageType := aUsageType;
  FLockField := nil;
  FFilter    := EmptyStr;
  FFilterPostfix := TFilterItem.Create;
  FUserFilterPostfix := TFilterItem.Create;
  FFormFilterPostfix := TFilterItem.Create;
  FGroupFields   := TFieldsMeta.Create(false);
  FOwnerLinks    := TDSLinksList.Create;
  FChildren      := TDataSetsMeta.Create(false);
  FRole  := drMain;
  FTable := aTable;
  FParser := TDeParser.Create;
  FParser.Table := FTable;
  FCache := TDataCache.Create(FTable);
  FCache.OnDataSetupFilter := DataSetupFilter;
  FCache.OnQueryOpenData   := DataCanOpen;
  FCache.OnFocusedChange   := InternalFocusedChange;
  FCache.OnSelectedChange  := InternalSelectedChange;
  FCache.OnFocusedChanging := InternalFocusedChanging;
  FCache.OnSelectedChanging := InternalSelectedChanging;
  FCache.OnAllowFocusedChanging := InternalAllowFocusedChanging;
  FFenchLinkIndex := -1;
  FFetchLinkVaues := Unassigned;
  if FTable = MetaData.MetaTables[idxNotes] then
    FCache.OnInsertRecord := CacheInsertNote
  else if FTable = MetaData.MetaTables[idxInterrelations] then
    FCache.OnInsertRecord := CacheInsertInterrelation
  else
    FCache.OnInsertRecord := CacheInsertRecord;

  // добавляем сортировку по умолчанию

  DeVL:=TDeVariableList.Create;
  DeVL.LoadCommaText(aTable.GroupViewParams);
  S := ClearName(Trim(DeVL.GetValueByName('SortBy')));
  if 0< Length(S) then
    FCache.SortList.AddFields(FTable.Fields, S);

  DeVL.Free;

  // строим список всех дополнительных действий
  FUserActionList:=TActionList.Create(Application);
  SupportActions := TList.Create;
  OtherActions := TList.Create;
  try
    for I := 0 to Pred(ProfitActions.Count) do
      if ProfitActions[I].ActionType <> atDataSet then
        if ProfitActions[I].CheckPolicy(spSelect) then
          if ProfitActions[I].DataSet = FTable.ID then
            AppendAction(I)
          else if (ProfitActions[I].DataSet = 0) and (ProfitActions[I].ActionType <> atReport) then
            OtherActions.Add(Pointer(I))
          else if ProfitActions[I].IsSupportDataSet(FTable.ID) then
            SupportActions.Add(Pointer(I));
    for I := 0 to Pred(SupportActions.Count) do
      AppendAction(NativeInt(SupportActions[I]));
    for I := 0 to Pred(OtherActions.Count) do
      AppendAction(NativeInt(OtherActions[I]));
  finally
    OtherActions.Free;
    SupportActions.Free;
  end;
  {$IFDEF DEBUG}
  if Assigned(FTable) then
    DebugActionsLog(Format('TDataSetMeta %s [%d] created ...', [FTable.Table, VarToInt(FTable.ID)]))
  else
    DebugActionsLog('TDataSetMeta created ...');
  {$ENDIF}
end;

constructor TDataSetMeta.Create(const aUsageType: TDSUsageType; aContext: TContextMeta);
var VL: TDeVariableList;
    FM: TFieldMeta;
    s: string;
    P,i: Integer;
    LockReady: Boolean;
begin
  Create(aUsageType, aContext.Dataset);
  FContext.Assign(aContext);

  if 0 < Length(FContext.Filter) then
    Filter:= FContext.Filter;

  VL:= TDeVariableList.Create;
  VL.LoadCommaText(FContext.ViewParams);

  s:= ClearName(Trim(VL.GetValueByName('SortBy')));
  if 0 < Length(s) then
    begin
      Cache.SortList.Clear;
      Cache.SortList.AddFields(FTable.Fields, s);
    end;

  P:= Cache.SortList.Count;
  if (P=0) and Assigned(FTable.OField) then Cache.SortList.Add(FTable.OField.ID);
  if (P=0) and Assigned(FTable.NField) then Cache.SortList.Add(FTable.NField.ID);

  // добавляем группировки из строки параметров
  s := VL.GetValueByName('GroupBy');
  While Length(s) > 0 do
    begin
      p:= Pos('|', s);
      if p = 0 then P:= Length(s) + 1;

      FM:= Table.Fields.FindByName(copy(s, 1, p - 1));
      if Assigned(FM) then
        if FM.LookupPair <> nil then
          GroupFields.Add(FM);

      Delete(s, 1, p);
    end;

  LockReady:= False;
  for i:= 0 to FContext.Groups.Count - 1 do
    if Assigned(FContext.Groups[i].Field.LookupPair) then
      if SecuritySystem.CheckPolicyDataSet(FContext.Groups[i].Field.LookupPair.Owner.ID, spSelect) then
        begin
          GroupFields.Add(FContext.Groups[i].Field);
          ParentKey[GroupFields.Count - 1]:= FContext.Groups[i].Value;
          if (FContext.Groups[i].Field = LockField) then
            LockReady:= True;
        end;

  if Assigned(LockField) and (LockField.Link > 0) and (Not LockReady) then
    GroupFields.Add(LockField);

  FLockField:= Table.Fields.FindByName(VL.GetValueByName('GroupLock'));

  s:= VL.GetValueByName('Align');
  if SameText(s, 'Left')   then FContext.Align:= alLeft else
  if SameText(s, 'Right')  then FContext.Align:= alRight else
  if SameText(s, 'Top')    then FContext.Align:= alTop else
  if SameText(s, 'Bottom') then FContext.Align:= alBottom else
  if SameText(s, 'Center') then FContext.Align:= alClient;

  FContext.Size:= StrToIntDef(VL.GetValueByName('Size'), 100);
  VL.Free;
end;

procedure TDataSetMeta.TableActionMenu(Sender : TObject);
var AD: TDeActionData;
begin
  if Not (Sender is TAction) then Exit;
  if Not (TObject(TAction(Sender).Tag) is TDeActionData) then Exit;

  AD:= TDeActionData(TAction(Sender).Tag);
  if Length(AD.Question) <> 0 then
   if AD.ActionSource <> asCommands then
     if Application.MessageBox( PChar(Format(AD.Question, [Cache.SelectedItemsCaption])),
       PChar(GetTitle('_Dl.Confirmation')), MB_YESNO or MB_ICONQUESTION ) <> idYes then Exit;
  // 19.11.2015 Куфенко +
  // AD.Execute(NativeInt(Cache));
  // Раньше был только закомментированный выше код, но так делать нельзя в новой версии.
  // Соответственно теперь Action получает сам DataCache для своей работы.
  AD.Execute;
  // 19.11.2015 Куфенко -
end;

procedure TDataSetMeta.DestroyRecursive;
begin
  if Assigned(Cache) then Cache.CloseData;
  FFullDestroy:= True;
  Free;
end;

destructor TDataSetMeta.Destroy;
var I : Integer;
begin
  MetaData.UnregisterDataset(Self);
  for I := OwnerLinks.Count-1 downto 0 do
    begin
      OwnerLinks[I].DataSetMeta.Children.Extract(Self);
      if FFullDestroy then
      begin
        OwnerLinks[I].DataSetMeta.DestroyRecursive;
        OwnerLinks.Delete(I);
      end;
    end;
  for I := Children.Count-1 downto 0 do
    begin
      Children[I].OwnerLinks.RemoveDataSet(Self);
      if FFullDestroy then
      begin
        Children[I].DestroyRecursive;
        Children.Delete(I);
      end;
    end;
  FreeAndNil(FCache);
  FreeAndNil(FOwnerLinks);
  FreeAndNil(FChildren);
  FreeAndNil(FGroupFields);
  FParser.Free;
  FUserActionList.Free;
  FFilterPostfix.Free;
  FUserFilterPostfix.Free;
  FFormFilterPostfix.Free;
  FGroupFieldsData := nil;

  FContext.Free;
  inherited Destroy;
end;

procedure TDataSetMeta.UpdateVisualizationForm;
{$IFDEF DEBUG}
var
  Text, Value: string;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  Text := EmptyStr;
  if Assigned(GridForm) then
    begin
      Value := GridForm.Name;
      if Length(Value) = 0 then
        Value := '$' + IntToHex(NativeInt(GridForm), {$IFDEF WIN32}8{$ELSE}16{$ENDIF});
      Text := Text + #13#10 + DupeString(' ', 24) + 'Grid: ' + Value + ' as ' + GridForm.ClassName;
    end;
  if Assigned(CardForm) then
    begin
      Value := CardForm.Name;
      if Length(Value) = 0 then
        Value := '$' + IntToHex(NativeInt(CardForm), {$IFDEF WIN32}8{$ELSE}16{$ENDIF});
      Text := Text + #13#10 + DupeString(' ', 24) + 'Card: ' + Value + ' as ' + CardForm.ClassName;
    end;
  if Assigned(FOwnerLinks) then
    FOwnerLinks.DebugLinksLog(ClassName + '.UpdateVisualizationForm with owner links ...' + Text)
  else
    DebugLog(ClassName + '.UpdateVisualizationForm without owner links ...' + Text);
  {$ENDIF}
  if Assigned(CardForm) and Assigned(FCache) then
    TAForm(CardForm).CacheItem := FCache.FocusedItem;
end;

procedure TDataSetMeta.GetLinkedData(const aItemIndex: Integer; var aTable: TTableMeta;  var aItemID: Variant);
var ParentTableID, ChildTableID, I : integer;
    ParentItemID : Variant;
begin
  aTable := nil;
  aItemID := Null;
  if (Table = MetaData.MetaTables[idxInterrelations])
    and Assigned(Cache[aItemIndex]) then
      begin
        ChildTableID := VarToInt(Cache[aItemIndex].ValueByName[fldIRChildTable]);
        ParentTableID := VarToInt(Cache[aItemIndex].ValueByName[fldIRParentTable]);
        ParentItemID := Unassigned;
        for I := 0 to OwnerLinks.Count-1 do
        begin
          if OwnerLinks[I].DatasetMeta.Table.ID = ChildTableID then
            aTable := MetaData.GetTableMeta(ParentTableID)
          else if OwnerLinks[I].DatasetMeta.Table.ID = ParentTableID then
            aTable := MetaData.GetTableMeta(ChildTableID);
          if Assigned(aTable) then
          begin
            ParentItemID := OwnerLinks[I].DatasetMeta.Cache.FocusedItem.ID;
            break;
          end;
        end;
        if Assigned(aTable) then
        begin
          if ParentTableID = ChildTableID then
            if VarSameValue(
              Cache[aItemIndex].ValueByName[fldIRParentKey], ParentItemID) then
                aItemID := Cache[aItemIndex].ValueByName[fldIRChildKey]
            else
              aItemID := Cache[aItemIndex].ValueByName[fldIRParentKey]
          else if aTable.ID = ParentTableID then
            aItemID := Cache[aItemIndex].ValueByName[fldIRParentKey]
          else if aTable.ID = ChildTableID then
            aItemID := Cache[aItemIndex].ValueByName[fldIRChildKey];
        end;
      end;
end;

procedure TDataSetMeta.UpdateActions(const FullUpdate: Boolean);
var i: Integer;
begin
  for i:= 0 to UserActionList.ActionCount-1 do
    if (TObject(UserActionList[i].Tag) is TDeActionData) then
      with TDeActionData(TObject(UserActionList[i].Tag)) do
        begin
          if DataSet = 0 then
            try
              if EnabledPostfix.Count=0 then UserActionList[i].Enabled:= True
                                        else UserActionList[i].Enabled:= DM.Calculator.Calculate(EnabledPostfix);
            except
              on E: Exception do
                begin
                UserActionList[i].Enabled:= False;
                SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25,
                            NativeInt( PChar('_dA.action "'+UserActionList[i].Caption+'" '+#10+
                                             E.Message+' _inexpr '+EnableStr)));
                {$IFDEF DEBUG}
                DebugLog(ClassName + '.UpdateActions(%s) skip %d error: %s', [StrPas(BooleanNames[FullUpdate]), I, E.Message]);
                {$ENDIF}
                end;
            end else

          if DataSet = Cache.TableMeta.ID then
            begin
              if ActionType = atInsertValue then UserActionList[i].Enabled:= True else
              if Not Assigned(Cache.FocusedItem) then UserActionList[i].Enabled:= False else
              if Cache.SelectedCount > 1 then UserActionList[i].Enabled:= True else
              try
                UserActionList[i].Enabled:= Cache.FocusedItem.Calculate(EnabledPostfix, True);
              finally
              end
            end else

          UserActionList[i].Enabled:= False;
        end
      else
       UserActionList[i].Enabled:= True;
end;

procedure TDataSetMeta.InternalFocusedChange(Item : TCacheItem);
begin
  // изменяем состояние формы визуализации
  UpdateVisualizationForm;
  // устанавливаем правильное состояние дочерних наборов данных
  UpdateChildren;
  // вызываем назначенный обработчик
  if Assigned(FOnCacheFocusedChange) then
    FOnCacheFocusedChange(Item);
end;

procedure TDataSetMeta.InternalSelectedChange(Item : TCacheItem);
begin
  if Assigned(FOnCacheSelectedChange) then
    FOnCacheSelectedChange(Item);
end;

procedure TDataSetMeta.InternalFocusedChanging(OldItem : TCacheItem);
begin
  if Assigned(FOnCacheFocusedChanging) then
    FOnCacheFocusedChanging(OldItem);
end;

procedure TDataSetMeta.InternalSelectedChanging(OldItem : TCacheItem);
begin
  if Assigned(FOnCacheSelectedChanging) then
    FOnCacheSelectedChanging(OldItem);
end;

procedure TDataSetMeta.OwnersWedding;
var i,j,k : Integer;
begin
  for i:=0 to OwnerLinks.Count-1 do
    for j:=0 to OwnerLinks.Count-1 do
      if OwnerLinks[j].DataSetMeta.Table <> OwnerLinks[i].DataSetMeta.Table then
        for k:=0 to OwnerLinks[j].DataSetMeta.Table.Fields.Count-1 do
          if Not OwnerLinks[j].DataSetMeta.Table.Fields[k].IsLookup then
            if OwnerLinks[j].DataSetMeta.Table.Fields[k].LinkTable= OwnerLinks[i].DataSetMeta.Table then
              OwnerLinks[i].DataSetMeta.SetChild(OwnerLinks[j].DataSetMeta, OwnerLinks[j].DataSetMeta.Table.Fields[k]);
end;

procedure TDataSetMeta.InternalAllowFocusedChanging(var NewItem: TCacheItem;
var  Allow: Boolean);
begin
  if Assigned(FOnAllowFocusedChanging) then FOnAllowFocusedChanging(NewItem, Allow);
end;

procedure TDataSetMeta.UpdateChildren;
var I : integer;
begin
  if Cache.Active then
    for I := 0 to Children.Count-1 do
    //  if Children[I].Active then
    {  if assigned (Children[I].Cache.FocusedItem) then
        Children[I].Cache.Update(mcUpdate, Children[I].Cache.FocusedItem.ID, True)
      else }
        begin
          if Assigned(Cache.FocusedItem) and (Children[i].Context.ViewType=vtTileMap)
            then Children[I].Cache.Update(mcInsert{Update}, Cache.FocusedItem.ID, ltFirst)
            else Children[I].Cache.Update(mcUpdate, null, ltFirst);
        end;
end;

function TDataSetMeta.GetIco: Integer;
begin
  if Assigned(Table) then Result:= Table.Ico
                     else Result:= 0;
  if VarIsEmpty(Result) or (Result = 0) then Result:= 2;
end;

function TDataSetMeta.GetIsFoldersTree : boolean;
begin
   result :=
    Assigned(Table) and
    Assigned(Table.GField) and  // присутствует поле "признак папки"
    (Self <> DefaultMeta) and   // не основной набор данных
    Assigned(GridForm) and
    (Context.ViewType = vtTree);        // показан в древовидной форме
end;

procedure TDataSetMeta.SetCardForm(const Value: TBaseForm);
{$IFDEF DEBUG}
var
  FormName: string;
{$ENDIF}
begin
  FCardForm := Value;
  {$IFDEF DEBUG}
  if Assigned(FCardForm) then
    begin
      FormName := FCardForm.Name;
      if Length(FormName) = 0 then
        FormName := '$' + IntToHex(NativeInt(FCardForm), {$IFDEF WIN32}8{$ELSE}16{$ENDIF});
      DebugLog('%s.SetCardForm(%s(%s)) prepared ...', [ClassName, FCardForm.ClassName, FormName]);
    end
  else
    DebugLog('%s.SetCardForm(nil) prepared ...', [ClassName]);
  {$ENDIF}
end;

procedure TDataSetMeta.SetGridForm(const Value: TBaseForm);
{$IFDEF DEBUG}
var
  FormName: string;
{$ENDIF}
begin
  FGridForm := Value;
  {$IFDEF DEBUG}
  if Assigned(FGridForm) then
    begin
      FormName := FGridForm.Name;
      if Length(FormName) = 0 then
        FormName := '$' + IntToHex(NativeInt(FGridForm), {$IFDEF WIN32}8{$ELSE}16{$ENDIF});
      DebugLog('%s.SetCardForm(%s(%s)) prepared ...', [ClassName, FGridForm.ClassName, FormName]);
    end
  else
    DebugLog('%s.SetCardForm(nil) prepared ...', [ClassName]);
  {$ENDIF}
end;

function TDataSetMeta.CanOpenData : boolean;
var I : integer;
begin
  Table.Database.CheckConnection;
  result := Table.Database.Connected and Cache.Active;
  if result then
    for I := 0 to OwnerLinks.Count-1 do
      begin
        result := result and OwnerLinks[I].DataSetMeta.CanOpenData;
        if not result then
          break;
      end;
end;

function TDataSetMeta.GetSelectedForm : TElementMeta;
begin
  if Not Assigned(FSelectedForm) and Assigned(Table) then
    FSelectedForm := Table.FirstForm;
  result := FSelectedForm;
end;

function TDataSetMeta.GetChildrenCount : Integer;
begin
  result := Children.Count;
end;

function TDataSetMeta.DataEnabled: Boolean;
begin
  result := CanOpenData;
  result := result and Cache.Active;
end;

function TDataSetMeta.GetKeyValue(const aGetMode: TGetKeyMode): Variant;
var
  Index: Integer;
begin
  Result := Unassigned;
  if Assigned(FCache) and (FCache.Count <> 0) then
    begin
      if aGetMode = gkmFocused then
        begin
          if Assigned(FCache.FocusedItem) then
            Result := FCache.FocusedItem.ID;
        end else
      if aGetMode = gkmAll then
        begin
          if FCache.Count > 1 then
            begin
              Result := VarArrayCreate([0, Pred(FCache.Count)], varVariant);
              for Index := 0 to Pred(FCache.Count) do
                VarArrayPut(Result, FCache.Items[Index].ID, [Index]);
            end
          else
            begin
              Result := FCache.Items[0].ID;
            end
        end else
      {if aGetMode = gkmSelected then}
        begin
          if FCache.SelectedCount > 1 then
            begin
              Result := VarArrayCreate([0, Pred(FCache.SelectedCount)], varVariant);
              for Index := 0 to Pred(FCache.SelectedCount) do
                VarArrayPut(Result, FCache.SelectedItems[Index].ID, [Index]);
            end
          else
            if FCache.SelectedCount = 1 then
              Result := FCache.SelectedItems[0].ID;
        end;
    end;
end;

procedure TDataSetMeta.SetReferenceKeys(aCacheItem : TCacheItem);
var J : integer;
begin
  { связь с родительскими наборами данных }
  for J := 0 to OwnerLinks.Count-1 do
  begin
    if Table.IsInterrelations then
      begin
        aCacheItem.ValueByName[fldIRParentTable] := OwnerLinks[J].DataSetMeta.Table.ID;
        aCacheItem.ValueByName[fldIRParentKey] := OwnerLinks[J].DataSetMeta.GetKeyValue;
      end else
    if Table.IsNotes then
      begin
        aCacheItem.ValueByName[fldNotesTable] := OwnerLinks[J].DataSetMeta.Table.ID;
        aCacheItem.ValueByName[fldNotesKey] := OwnerLinks[J].DataSetMeta.GetKeyValue;
      end else
    // другие наборы данных
      begin
        if OwnerLinks[J].DataSetMeta.Cache.Active then
          begin
            if isInitializing in aCacheItem.State then
              aCacheItem.InitFieldValueByNameExternal
                (OwnerLinks[J].LinkField.Original, OwnerLinks[J].DataSetMeta.GetKeyValue)
             else
              aCacheItem.ValueByName[OwnerLinks[J].LinkField.Original] :=
                OwnerLinks[J].DataSetMeta.GetKeyValue(gkmFocused);
          end;
      end;
  end;
end;

function TDataSetMeta.SetFilterKeys(aCacheItem : TCacheItem; var Ind: integer; DoValue: Boolean = False): Boolean;
var f,c: Integer;
begin
  // пустое условие, ничего делать не надо
  if Ind=-1 then Exit(True);

  // короткое условие, заведомо некорректное
  if Ind<2 then Exit(False);

  // операция opAnd, после нее ищем дважды необходимые условия равенства
  if (FFilterPostfix.Items[Ind].ItemType=piBinary) and
     (FFilterPostfix.Items[Ind].BinaryOp=opAnd) then
    begin
      Dec(Ind);
      Result:=SetFilterKeys(aCacheItem, Ind, DoValue);
      Result:=Result and SetFilterKeys(aCacheItem, Ind, DoValue);
      Exit;
    end;

  if (FFilterPostfix.Items[Ind-1].ItemType = piIdent) then f:=Ind-1 else
  if (FFilterPostfix.Items[Ind-2].ItemType = piIdent) then f:=Ind-2 else f:= -1;
  if (FFilterPostfix.Items[Ind-1].ItemType = piConst) then c:=Ind-1 else
  if (FFilterPostfix.Items[Ind-2].ItemType = piConst) then c:=Ind-2 else c:= -1;

  // собственно условие равенства значения поля и константы
  if (c>=0) and (f>=0) then
    if (FFilterPostfix.Items[Ind].ItemType = piBinary) and
       (FFilterPostfix.Items[Ind].BinaryOp = opEQ) and
       (FFilterPostfix.Items[c].ResultType = FFilterPostfix.Items[f].ResultType) or
       ((FFilterPostfix.Items[c].ResultType = ftUnknown) and (FFilterPostfix.Items[c].Data = null)) then
    begin
      Dec(Ind,3);
      if DoValue then
        aCacheItem.ValueByName[FFilterPostfix.Items[f].Ident] := FFilterPostfix.Items[c].Data;
      Exit(True);
    end;

   // в остальных случаях значения полей проставить однозначно нельзя
   Exit(False);
end;

function TDataSetMeta.FindChild(aTableMeta : TTableMeta) : TDataSetMeta;
var I : integer;
begin
  result := nil;
  for I := 0 to Children.Count-1 do
    if Assigned(Children[I].Table) and (Children[I].Table = aTableMeta) then
    begin
      result := Children[I];
      break;
    end;
end;

procedure TDataSetMeta.SetChild(aChildMeta : TDataSetMeta; aLinkField : TFieldMeta = nil);
begin
  Children.Add(aChildMeta);
  aChildMeta.OwnerLinks.Add(TDSLink.Create(Self, aLinkField));
  // приводим дочерние элементы в соответствие с родительским
  if Assigned(aChildMeta){ and aChildMeta.Cache.Active }then  // иначе не работают группировки
    aChildMeta.Cache.Update(mcUpdate, null);
end;

function TDataSetMeta.CreateChild(const aLinkID: Integer; aLinkField: TFieldMeta): TDataSetMeta;
var I, InterrelationsID    : integer;
    TableMeta, LinkedTable : TTableMeta;
    LinkFounded: Boolean;
begin
  result := nil;
  if (aLinkID = null) then raise Exception.Create('CreateChild: aLinkID = null');

  if Assigned(MetaData.MetaTables[idxInterrelations]) then InterrelationsID := MetaData.MetaTables[idxInterrelations].ID
                                                      else InterrelationsID := null;

  TableMeta:= MetaData.GetTableMeta(aLinkID);

  // отрабатыыаем взаимосвязи
  if (aLinkID = InterrelationsID) then
    begin
      result := TDataSetMeta.Create(utForm, MetaData.MetaTables[idxInterrelations]);
      result.Role := drChild;
      SetChild(result, nil);
      result.Cache.PrepareData(false);
    end else


  //TODO: 2020 // пользовательская связь
  if not Assigned(aLinkField) then
    begin
      if TableMeta=nil then
        raise Exception.Create('CreateChild: aLink and aLinkField are unassigned')
      else
        begin
          result := TDataSetMeta.Create(utForm, TableMeta);
          result.Role := drNone;
          result.Cache.PrepareData(false);
				end;
    end else

  //TODO: 2020 // связь через все поля, ссылающиеся на таблицу
  if aLinkField = TableMeta.KField[0] then
    begin
      if TableMeta=nil then
        raise Exception.Create('CreateChild: aLink and aLinkField are unassigned')
      else
        begin
          result := TDataSetMeta.Create(utForm, TableMeta);
          // здесь надо отработать связь по одному из нескольких полей
          result.Role := drNone;
//          SetChild(result, aLinkField);

          LinkFounded:= False;
          for i := 0 to Pred(aLinkField.Owner.Fields.Count) do
            if not aLinkField.Owner.Fields[I].IsLookup then
              if aLinkField.Owner.Fields[I].Link =  TableMeta.ID then
                begin
                  SetChild(result, aLinkField.Owner.Fields[I]);
                  LinkFounded:= True;
                  Break;
                end;

           if Not LinkFounded and (aLinkField = aLinkField.Owner.KField[0]) then
              SetChild(result, aLinkField);

          //
          result.Cache.PrepareData(false);
				end;
    end else

  // прямой дочерний набор данных (включая Notes)
  if (aLinkField.Owner.ID = aLinkID) then
    begin
      result := TDataSetMeta.Create(utForm, aLinkField.Owner);
      result.Role := drChild;
      SetChild(result, aLinkField);
      result.Cache.PrepareData(false);
    end else

  // ищем дальний дочерний набор данных
  begin
    // поиск поля связи надо перенести в TElementMeta.Assign
    TableMeta := MetaData.GetTableMeta(aLinkID);

    for I := 0 to TableMeta.Fields.Count-1 do
      if TableMeta.Fields[I].Link > 0 then
        begin
          LinkedTable := MetaData.GetTableMeta(TableMeta.Fields[I].Link);
          while Assigned(LinkedTable)
            and (LinkedTable <> LinkedTable.OwnerTable)
            and (LinkedTable.ID <> Table.ID)
            and (LinkedTable.ID <> Table.OwnerTable.ID) do
              LinkedTable := LinkedTable.GrandOwnerTable;
          if Assigned(LinkedTable)
             and ((LinkedTable.ID = Table.ID) or
               (TableMeta.Fields[I].Link = Table.OwnerTable.ID)) then
            begin
              aLinkField := TableMeta.Fields[I];
              break;
            end;
        end;
    if Assigned(aLinkField) and aLinkField.IsLookup then
      aLinkField := aLinkField.LookupPair;

  // формируем дочерний набор данных
  if Assigned(aLinkField) then
      begin
        result := TDataSetMeta.Create(utForm, TableMeta);
        result.Role := drChild;
        SetChild(result, aLinkField);
        result.Cache.PrepareData(false);
      end;
  end;
end;

function TDataSetMeta.CreateParent(aGroupField: TFieldMeta) : TDataSetMeta;
var KeyVal : Variant;
begin
  result := nil;
  if not Assigned(aGroupField) then Exit;

  if (-1 < aGroupField.Link) or (aGroupField.Owner.KField[0]=aGroupField) then
    begin
      if Assigned(aGroupField.LinkTable) then result := TDataSetMeta.Create(utForm, aGroupField.LookupPair.Owner)
                                         else result := TDataSetMeta.Create(utForm, aGroupField.Owner);

      result.Role := drParent;
      // если это дерево самого набора данных по родительскому полю
      if (aGroupField.Owner.PField<>nil) and (aGroupField.Owner.GField<>nil)
        and(aGroupField.Original=aGroupField.Owner.PField.Original) then
          result.Cache.Filters.NewFilter( aGroupField.Owner.GField, opEQ, aGroupField.Owner.GField.Value1 );
      //TODO: Добавить сюда сортировку

      if Assigned(result.Table.NField) then
         Result.Cache.SortList.Add(Result.Table.NField.ID);

      Result.Cache.PrepareData(false);

      // запоминаем значение ключа, на который надо позиционировать запись
      if Cache.Active and Assigned(Cache.FocusedItem)  then
        KeyVal := Cache.FocusedItem.FieldValue[Cache.Fields.IndexByID(aGroupField.ID)]
      else
        KeyVal := Null;

      // открываем данные и позиционируемся на требуемую запись в родительском наборе данных
      result.Cache.OpenData;
      result.Cache.SyncronizeOnData(KeyVal, ltFirst);
      result.SetChild(Self, aGroupField);
    end;
end;

procedure TDataSetMeta.CacheInsertRecord(aCacheItem : TCacheItem;
  var aCanInsert : boolean);
var Key : Variant;
    ICount : Integer;
begin
  { запоминаем значение родительской папки }
  Key := Unassigned;
  if Context.ViewType = vtTree then
    if Assigned(Cache.FocusedItem) then
      if (Cache.FocusedItem.IsFolder) or (Cache.FocusedItem is TRootItem) then
        Key := Cache.FocusedItem.ID
      else
        Key := Cache.FocusedItem.ParentID;
  if IsFoldersTree and Assigned(Cache.FocusedItem) then
    Key := Cache.FocusedItem.FieldValue[Table.Fields.IndexByID(Table.KField[0].ID)];

  SetReferenceKeys(aCacheItem);

  { установка значений определенных текущим фильтром "=" }
     ICount :=FFilterPostfix.Count-1;
  if SetFilterKeys(aCacheItem, ICount) and (ICount=-1) then
    begin
      ICount :=FFilterPostfix.Count-1;
      SetFilterKeys(aCacheItem, ICount, True);
    end;

  { привязываемся к родительским элементам в дереве }
  if Context.ViewType = vtTree then
    if not VarIsEmpty(Key) then
      aCacheItem.FieldValue[Table.Fields.IndexByID(Table.PField.ID)] := Key;
  { если мы находимся в дереве, то создавать необходимо папку }
  if IsFoldersTree then
    aCacheItem.IsFolder := true;
  if Assigned(FOnInsertRecord)  then FOnInsertRecord(aCacheItem);
end;

procedure TDataSetMeta.CacheInsertNote(aCacheItem : TCacheItem;
  var aCanInsert : boolean);
var ParentItem : TCacheItem;
begin
  ParentItem := OwnerLinks[0].DataSetMeta.Cache.FocusedItem;
  if Assigned(ParentItem) then
    begin
      aCacheItem.ValueByName[fldNotesTable] := ParentItem.Owner.TableMeta.ID;
      aCacheItem.ValueByName[fldNotesKey] := ParentItem.ID;
      aCacheItem.ValueNativeByName[fldNotesNote] := GetTitle('_Note _for ') + ParentItem.Caption;
    end
  else
    aCanInsert := false;
end;

procedure TDataSetMeta.SetFilter(const aFilter: string);
var  s : String;
begin
  if (0 = Length(aFilter)) then
    begin
      FFilter:= EmptyStr;
      FFilterPostfix.Clear;
    end else

  if (FFilter <> aFilter) then
    begin
      FFilter:= aFilter;
      FFilterPostfix.Clear;
      try
        FParser.Parse(FFilter, FFilterPostfix);
      except
        on E: EDeParserError do
          begin
            {$IFDEF DEBUG}
            if Assigned(FParser) and Assigned(FParser.Table) and (Length(FParser.Table.Table) <> 0) then
              DebugLog('TDataSetMeta.SetFilter(%s) for %s skip error: %s', [QuotedStr(aFilter), FParser.Table.Table, E.Message])
            else
              DebugLog('TDataSetMeta.SetFilter(%s) skip error: %s', [QuotedStr(aFilter), E.Message]);
            {$ENDIF}
            FFilterPostfix.Clear;
            s:=E.Message+#10+FFilter;
            SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(pChar(s)));
          end;
      end;
    end;
end;

procedure TDataSetMeta.CacheInsertInterrelation(aCacheItem : TCacheItem; var aCanInsert : boolean);
var ParentItem, DataItem : TCacheItem;
    DataCache  : TDataCache;
begin
  ParentItem := OwnerLinks[0].DataSetMeta.Cache.FocusedItem;
  if Assigned(ParentItem) then
  begin
    DataCache := TDataCache.Create(MetaData.GetTableMeta(DeClipboard.DataID));
    try
      DataItem := DataCache.InsertFromClipboard;
      if (not VarSameValue(DataItem.ID, ParentItem.ID)) or
         (DataCache.TableMeta.ID <> ParentItem.Owner.TableMeta.ID) then
      begin
        aCacheItem.ValueByName[fldIRParentTable] := ParentItem.Owner.TableMeta.ID;
        aCacheItem.ValueByName[fldIRChildTable] := DataItem.Owner.TableMeta.ID;
        aCacheItem.ValueByName[fldIRParentKey] := ParentItem.ID;
        aCacheItem.ValueByName[fldIRChildKey] := DataItem.ID;
        aCacheItem.ValueByName[fldIRNote] := WideStringToUnicode(
          Format('%s %s --> %s', [GetTitle('_Dt.Shortcut'), ParentItem.Caption, DataItem.Caption]));

      end
      else
      begin
        aCanInsert := false;
        SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
          NativeInt(pChar(GetTitle('_dE.linkself'))));
      end;
    finally
      DataCache.Free;
    end;
  end
  else
    aCanInsert := false;
end;

{ --- TDataSetsMeta ---------------------------------------------------------- }

destructor TDataSetsMeta.Destroy;
begin
  while Count > 0 do Delete(0);
  inherited Destroy;
end;

function TDataSetsMeta.Get(const aIndex: Integer): TDataSetMeta;
begin
  if aIndex < Count then
    result := TDataSetMeta(inherited Items[aIndex])
  else
    result := nil;
end;

procedure TDataSetsMeta.Put(const aIndex: Integer; aDataSetMeta: TDataSetMeta);
begin
  if aIndex >= Count then Count := aIndex+1;
  inherited Items[aIndex] := aDataSetMeta;
end;

function TDataSetMeta.GetParentKey(const aIndex: Integer): Variant;
begin
  result := Null;
  if aIndex >= 0 then
    if Assigned(Cache.FocusedItem) then
      result := Cache.FocusedItem.FieldValue[Cache.Fields.IndexByID(GroupFields[aIndex].ID)]
    else if aIndex < Length(FGroupFieldsData) then
      result := FGroupFieldsData[aIndex];
end;

procedure TDataSetMeta.SetParentKey(const aIndex: Integer;  aValue: Variant);
begin
  if aIndex >= Length(FGroupFieldsData) then
    SetLength(FGroupFieldsData, aIndex+1);
  FGroupFieldsData[aIndex] := aValue;
end;

procedure TDataSetMeta.DataSetupFilter(aDescr : TCustomQueryDescr);
var I,j : integer;
    V, VKey: Variant;
    TM: TTableMeta;
    Added: integer;
begin
  Added := 0;
  FFenchLinkIndex := -1;
  FFetchLinkVaues := Unassigned;
  if Not Assigned(Table) then Exit;

  { условия связи с родительскими наборами данных }
  for I := 0 to OwnerLinks.Count-1 do
  begin
    if OwnerLinks[I].DataSetMeta.Cache.Active
      then if (Table.KField[0].LinkTable = OwnerLinks[I].DataSetMeta.Table) and (Table.IsDirectory = idExtention)
             //(OwnerLinks[I].LinkField.Key) and (OwnerLinks[I].DataSetMeta.Table.IsDirectory = idExtention)
             then V:= OwnerLinks[I].DataSetMeta.GetKeyValue(gkmAll)
             else V:= OwnerLinks[I].DataSetMeta.GetKeyValue(gkmSelected)
      else V:= Null;

    if not Table.IsInterrelations then
    begin

      if not VarIsClear(V) then
        begin
          if VarIsNull(V) then
            aDescr.AddCondition(OwnerLinks[I].LinkField.Original, OwnerLinks[I].LinkField.DataType, opIs, Null)
          else
            begin
              // добавляем на базовую таблицу, если это унаследованнная
              if SameText(fldFieldsTable, OwnerLinks[I].LinkField.Original) then
                begin
                  if VarIsArray(V) then
                    for j := VarArrayHighBound(V,1) downto VarArrayLowBound(V,1) do
                      begin
                        TM := MetaData.GetTableMeta(VarArrayGet(V, j));
                        if Assigned(TM) and Assigned(TM.OwnerTable) then
                          V:= IncludeExecute(nil,[V, TM.OwnerTable.Id]);
                      end
                  else
                    begin
                      TM := MetaData.GetTableMeta(VarToInt(V));
                      if Assigned(TM) and Assigned(TM.OwnerTable) then
                        V:= IncludeExecute(nil,[V, TM.OwnerTable.Id]);
                    end;
                end;

              VKey := DeVarArrayRange(V, 0, Variables.AsInteger[RegFetchRecord]);
              if VarIsArray(VKey) then
                begin
                  aDescr.AddCondition(OwnerLinks[I].LinkField.Original, OwnerLinks[I].LinkField.DataType, opIN, VKey);
                  // Установим признак начитанности всех записей ...
                  FFenchLinkIndex := I;
                  FFetchLinkVaues := DeVarArrayRange(V, Succ(Variables.AsInteger[RegFetchRecord]), VarArrayHighBound(V, 1));
                  if VarIsClear(FFetchLinkVaues) then FFenchLinkIndex := -1;
                end
              else
                begin
                  aDescr.AddCondition(OwnerLinks[I].LinkField.Original, OwnerLinks[I].LinkField.DataType, opEQ, VKey);
                end;
            end;

          Inc(Added);
        end
      else
        begin //родителя нет - возвращаем пустой набор данных
          aDescr.AddCondition(OwnerLinks[I].LinkField.Original, OwnerLinks[I].LinkField.DataType, opIs, Null);
          aDescr.AddCondition(OwnerLinks[I].LinkField.Original, OwnerLinks[I].LinkField.DataType, opIs, Null);
          aDescr.AddOperation(opNot);
          inc(Added,2);
        end;
    end
    else if not VarIsClear(V) then
      begin
        if VarIsArray(V) then VKey:= V[0]
                         else VKey:= V;
        aDescr.AddCondition(fldIRParentTable, ftInteger, opEQ, OwnerLinks[I].DataSetMeta.Table.ID);
        aDescr.AddCondition(fldIRParentKey, ftString, opEQ, VKey);
        aDescr.AddOperation(opAnd);
        aDescr.AddCondition(fldIRChildTable, ftInteger, opEQ, OwnerLinks[I].DataSetMeta.Table.ID);
        aDescr.AddCondition(fldIRChildKey, ftString, opEQ, VKey);
        aDescr.AddOperation(opAnd);
        aDescr.AddOperation(opOr);
        inc(Added);
      end;
  end;

  for I := 1 to Added-1 do
    aDescr.AddOperation(opAnd);

  aDescr.AssignFilter(FFilterPostfix);
  aDescr.AssignFilter(FUserFilterPostfix);
  aDescr.AssignFilter(FFormFilterPostfix);
end;

function TDataSetMeta.DataCanOpen(aDataSet : TDeDataSet) : boolean;
var I : integer;
begin
  result :=
    Assigned(aDataSet)
    and ((Role = drChild) or (UsageType = utTemporary)
     or SecuritySystem.CheckPolicyDataSet(Table.Id, spSelect));
  if result then
    for I := 0 to OwnerLinks.Count-1 do
      begin
        result := result and OwnerLinks[I].DataSetMeta.CanOpenData;
        if not result then
          break;
      end;
end;

procedure TDataSetMeta.ReadAllFilters(FilterItem: TFilterItem);
var
  Index, Count: Integer;
  Value: Variant;
begin
  if Assigned(FilterItem) then
    begin
      Count := 0;
      if Assigned(Table) then
        begin
          if Assigned(Table.DeletedFilter) and Table.DeletedFilter.Active then
            begin
              FilterItem.CopyFrom(Table.DeletedFilter);
              Inc(Count);
            end;
          if Assigned(Table.FilterPostfix) and Table.FilterPostfix.Active then
            begin
              FilterItem.CopyFrom(Table.FilterPostfix);
              Inc(Count);
            end;
          // Добавляем локальные/глобальные фильтры!!!
          for Index := 0 to Pred(Table.LinkFilters.Count) do
            if Table.LinkFilters[Index].Active then
              begin
                FilterItem.CopyFrom(Table.LinkFilters[Index]);
                Inc(Count);
              end;
        end;
      if Assigned(FilterPostfix) and FilterPostfix.Active then
        begin
          FilterItem.CopyFrom(FilterPostfix);
          Inc(Count);
        end;
      if Assigned(UserFilterPostfix) and UserFilterPostfix.Active then
        begin
          FilterItem.CopyFrom(UserFilterPostfix);
          Inc(Count);
        end;
      if Assigned(FormFilterPostfix) and FormFilterPostfix.Active then
        begin
          FilterItem.CopyFrom(FormFilterPostfix);
          Inc(Count);
        end;
      if Assigned(OwnerLinks) then
        for Index := 0 to Pred(OwnerLinks.Count) do
          if OwnerLinks[Index].DataSetMeta.Cache.Active then
            begin
              Value := OwnerLinks[Index].DataSetMeta.GetKeyValue;
              if VarIsNull(Value) then  FilterItem.AddCondition(OwnerLinks[Index].LinkField, opIs, Null) else
              if VarIsArray(Value) then FilterItem.AddCondition(OwnerLinks[Index].LinkField, opIn, Value) else
                                        FilterItem.AddCondition(OwnerLinks[Index].LinkField, opEQ, Value);
              Inc(Count);
            end;
      for Index := 1 to Pred(Count) do
        FilterItem.AddOperation(opAnd);
    end;
end;

{$IFDEF DEBUG}
procedure TDataSetMeta.DebugActionsLog(const Text: string);
  function PrepareActionsLog: string;
  const
    BooleanNames: array[Boolean] of Char = ('-', '+');
  var
    TextTable: TTextTable;
    ActionData: TDeActionData;
    Index: Integer;
  begin
    if Assigned(UserActionList) and (UserActionList.ActionCount <> 0) then
      begin
        TextTable := TTextTable.Create;
        try
          TextTable.Columns.Add('No', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('ID', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Source', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Type', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('DataSet', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Caption', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('ShortCut', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Category', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('SubCategories', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Original', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('ICO', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Order', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('B', 1, taCenter, taCenter);
          for Index := 0 to Pred(UserActionList.ActionCount) do
            begin
              TextTable.Lines[Index][0] := IntToStr(Succ(Index));
              ActionData := TDeActionData(UserActionList.Actions[Index].Tag);
              if Assigned(ActionData) then
                begin
                  TextTable.Lines[Index][1] := IntToStr(ActionData.ID);
                  TextTable.Lines[Index][2] := StrPas(ActionSources[ActionData.ActionSource]);
                  TextTable.Lines[Index][3] := ActionTypeToString(ActionData.ActionType);
                  TextTable.Lines[Index][4] := IntToStr(ActionData.DataSet);
                  TextTable.Lines[Index][5] := ActionData.Caption;
                  TextTable.Lines[Index][6] := ShortCutToText(ActionData.ShortCut);
                  TextTable.Lines[Index][7] := StrPas(ActionCategories[ActionData.Category]);
                  TextTable.Lines[Index][8] := ActionData.SubCategories;
                  TextTable.Lines[Index][9] := ActionData.Original;
                  TextTable.Lines[Index][10] := IntToStr(ActionData.ICO);
                  TextTable.Lines[Index][11] := IntToStr(ActionData.Order);
                  TextTable.Lines[Index][12] := BooleanNames[ActionData.BreakLine];
                end;
            end;
          Result := TextTable.AsText(24);
        finally
          TextTable.Free;
        end;
      end
    else
      Result := EmptyStr;
    if Length(Result) <> 0 then
      Result := #13#10 + Result;
  end;
begin
  DebugLog(Text + PrepareActionsLog);
end;
{$ENDIF}

function TDataSetMeta.GetAllFetched: Boolean;
begin
  Result := FFenchLinkIndex = -1;
end;

function TDataSetMeta.PrepareFetchLinkQueryDescr(Descr: TCustomQueryDescr): Boolean;
var
  Added: Integer;
  Value: Variant;
  Index: Integer;
begin
  Result := Assigned(Table) and Assigned(Descr) and (Descr.QueryType = qtSelect) and not AllFetched;
  if Result then
    begin
      Descr.BeginUpdate;
      try
        Added := 0;
        if VarIsClear(FFetchLinkVaues) then
          begin
            // родителя нет - возвращаем пустой набор данных
            Descr.AddCondition(OwnerLinks[FFenchLinkIndex].LinkField.Original, OwnerLinks[FFenchLinkIndex].LinkField.DataType, opIs, Null);
            Descr.AddCondition(OwnerLinks[FFenchLinkIndex].LinkField.Original, OwnerLinks[FFenchLinkIndex].LinkField.DataType, opIs, Null);
            Descr.AddOperation(opNot);
            Inc(Added, 2);
            FFenchLinkIndex := -1;
            FFetchLinkVaues := Unassigned;
          end
        else
          begin
            if VarIsNull(FFetchLinkVaues) then
              begin
                Descr.AddCondition(OwnerLinks[FFenchLinkIndex].LinkField.Original, OwnerLinks[FFenchLinkIndex].LinkField.DataType, opIs, Null);
                FFenchLinkIndex := -1;
                FFetchLinkVaues := Unassigned;
              end
            else
              if VarIsArray(FFetchLinkVaues) then
                begin
                  {$IFDEF DEBUG}
                  DebugLog('TDataSetMeta.PrepareFetchLinkQueryDescr: all = ' + VariantToString(FFetchLinkVaues));
                  {$ENDIF}
                  Value := DeVarArrayRange(FFetchLinkVaues, 0, Variables.AsInteger[RegFetchRecord]);
                  {$IFDEF DEBUG}
                  DebugLog('TDataSetMeta.PrepareFetchLinkQueryDescr: top(%d)  = %s ', [Variables.AsInteger[RegFetchRecord], VariantToString(Value)]);
                  {$ENDIF}
                  FFetchLinkVaues := DeVarArrayRange(FFetchLinkVaues, Succ(Variables.AsInteger[RegFetchRecord]), VarArrayHighBound(FFetchLinkVaues, 1));
                  {$IFDEF DEBUG}
                  DebugLog('TDataSetMeta.PrepareFetchLinkQueryDescr: rest = ' + VariantToString(FFetchLinkVaues));
                  {$ENDIF}
                  if VarIsArray(Value) then
                    Descr.AddCondition(OwnerLinks[FFenchLinkIndex].LinkField.Original, OwnerLinks[FFenchLinkIndex].LinkField.DataType, opIN, Value)
                  else
                    Descr.AddParamCondition(OwnerLinks[FFenchLinkIndex].LinkField.Original, OwnerLinks[FFenchLinkIndex].LinkField.DataType, opEQ, EmptyStr, Value);
                  if VarIsClear(FFetchLinkVaues) then FFenchLinkIndex := -1;
                end
              else
                begin
                  Descr.AddParamCondition(OwnerLinks[FFenchLinkIndex].LinkField.Original, OwnerLinks[FFenchLinkIndex].LinkField.DataType, opEQ, EmptyStr, FFetchLinkVaues);
                  FFenchLinkIndex := -1;
                  FFetchLinkVaues := Unassigned;
                end;
            Inc(Added);
          end;

        // Добавим операции AND при необходимости ...
        for Index := 1 to Pred(Added) do Descr.AddOperation(opAnd);

        // Добавим фильтры при необходимости ...
        Descr.AssignFilter(FFilterPostfix);
        Descr.AssignFilter(FUserFilterPostfix);
        Descr.AssignFilter(FFormFilterPostfix);
      finally
        Descr.EndUpdate;
      end;
    end;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DSMeta unit initialization ...');

finalization
  DebugLog('DSMeta unit finalization ...');
{$ENDIF}

end.

