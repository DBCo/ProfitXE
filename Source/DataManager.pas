unit DataManager;

interface

uses Contnrs, Classes, {ComCtrls, }{Menus, }DeTypes, DeMeta, DeDB;

const
  BooleanToInt: array[Boolean] of SmallInt = (0, 1);

type
  /// <summary>варианты исхода при проверке условия:
  /// <para><c>rsNotChecked</c> - условие не проверялось</para>
  /// <para><c>rsNoError</c> - при проверке не обнаружено ошибок</para>
  /// <para><c>rsWarning</c> - обнаружены некритические ошибки</para>
  /// <para><c>rsError</c> - обнаружены критические ошибки</para>
  /// </summary>
  TCheckResult = (rsNotChecked, rsNoError, rsWarning, rsError);

  /// <summary>информация об ошибках, возникших в ходе обработки данных</summary>
  TCustomError = class
  protected
    // возвращает строку, переданную в записи в открытом массиве
    function GetStringParam(aParam : TVarRec) : string;
    // возвращает Integer, переданный в открытом массиве
    function GetIntegerParam(aParam : TVarRec) : integer;
    // возвращает Boolean, переданный в открытом массиве
    function GetBooleanParam(aParam : TVarRec) : boolean;
    /// <summary>Функция получения текста сообщения об ошибке</summary>
    /// <returns>возврашает сообщение об ошибке</returns>
    function GetMessage: string; virtual; abstract;
    // сохраняет в поле какую-либо дополнительную текстовую информацию
    procedure SetEnvironment(const aInfo: array of const); virtual; abstract;
  end;

  /// <summary>папка имеет содержимое</summary>
  TContainError = class(TCustomError)
  private
    FFolderName   : string;
    FObjectsCount : integer;
  public
    constructor Create(const aCount: Integer);
    function GetMessage: string; override;
    procedure SetEnvironment(const aInfo: array of const); override;
  end;

  /// <summary>запись имеет дочерний элемент</summary>
  TChildError = class(TCustomError)
  private
    FParentObjectName : string;
    FPartName         : string;
  public
    constructor Create(const aPartName: string);
    function GetMessage: string; override;
    procedure SetEnvironment(const aInfo: array of const); override;
  end;

  /// <summary>запись имеет дочерние элементы</summary>
  TChildrenError = class(TCustomError)
  private
    FParentObjectName : string;
    FObjectName       : string;
    FObjectsCount     : integer;
  public
    constructor Create(const aName: string; const aCount: Integer);
    function GetMessage: string; override;
    procedure SetEnvironment(const aInfo: array of const); override;
  end;

  /// <summary>просто сообщение об ошибке</summary>
  TAnyError = class(TCustomError)
  private
    FErrorMessage : string;
  public
    constructor Create(const aMessage: string);
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    function GetMessage: string; override;
    procedure SetEnvironment(const aInfo: array of const); override;
  end;

  /// <summary>ошибка проверки условия при вставке/обновлении/удалении</summary>
  TFieldError = class(TAnyError)
  private
    FFieldName    : string;
  public
    constructor Create(const aField: string; const aMessage: string);
    property FieldName : string read FFieldName;
  end;

  // сообщение об ошибке, привязанной к какой-либо записи
  TIDError = class(TAnyError)
  private
    FID : integer;
  public
    constructor Create(const aID: Integer; const aMessage: string = '');
    property ID : integer read FID write FID;
  end;

  // сообщения об ошибках иерархии
  TUnknownOwnerError = class(TIDError);  // неизвестный владелец
  TRingError = class(TIDError);          // кольцо в иерархии
  TSelfRingError = class(TRingError);    // элемент является собственным владельцем

  /// <summary>список ошибок</summary>
  TErrorsList = class(TObjectList)
  private
    function Get(const Index: Integer): TCustomError;
    procedure Put(const Index: Integer; aData: TCustomError);
  public
    property Items[const Index: Integer]: TCustomError read Get write Put;
    function GetMessage: string;
    procedure SetEnvironment(const aInfo: array of const);
    procedure DeleteErrors(const aStartIndex, aFinishIndex: Integer);
  end;

  TDataChanges = (
    dcNoChanges,      // данные не изменены
    dcNotFound,       // данные не найдены
    dcChanged,        // данные изменены
    dcCalcChanged,    // изменены только вычисляемые данные
    dcDeleted         // данные удалены
    );

  /// <summary>менеджер записей</summary>
  TDataManager = class
  private
    FQuery           : TDeDataSet;
    FIQuery          : TDeDataSet;  // запрос на вставку
    FUQuery          : TDeDataSet;  // запрос на обновление
    FDQuery          : TDeDataSet;  // запрос на удаление
    FTable           : TTableMeta;
    FErrors          : TErrorsList;
    FDataSetMeta     : TObject;
    FCalculator      : TObject;
    // проверяет условие
    function GetFieldValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
    function CheckConstraint(aConstraint : TDeConstraint;
      aCacheItem : TObject;  aContexts : TConditionContexts) : TCheckResult;
    // перечисление условий для построения SQL-запроса
    function GetQuery(const aIndex: Integer): TDeDataset;
    property IQuery : TDeDataset index 1 read GetQuery;
    property UQuery : TDeDataset index 2 read GetQuery;
    property DQuery : TDeDataset index 3 read GetQuery;
//    procedure CheckChanges(aCacheItem : TObject;  var Changes : TDataChanges);
  protected
  public
    constructor Create;
    destructor Destroy;  override;
    property Table : TTableMeta read FTable write FTable;
    property Errors : TErrorsList read FErrors;
    property DataSetMeta : TObject read FDataSetMeta write FDataSetMeta;
    { операции вставки новой записи }
    /// <summary>заполняет значение первичного ключа</summary>
    procedure SetPrimaryKey(aCacheItem : TObject; InDS: TDeDataset = nil);
    /// <summary>заполняет запись значениями по умолчанию</summary>
    procedure PrepareRecord(aCacheItem : TObject);
    /// <summary>проверяет возможность вставки записи</summary>
    function CanInsertRecord(aCacheItem : TObject) : boolean;
    /// <summary>вставляет новую запись в набор</summary>
    function InsertRecord(aCacheItem : TObject) : boolean;  virtual;
    /// <summary>перечитывает запись из базы данных отдельным запросом</summary>
    function ReReadRecord(aCacheItem : TObject) : boolean;  virtual;
    /// <summary>проверяет возможность обновления записи в базе данных</summary>
    function CanUpdateRecord(aCacheItem : TObject) : boolean;  virtual;
    function UpdateNearRecords(aCacheItem : TObject) : boolean;
    function UpdateRecord(aCacheItem : TObject) : boolean;  virtual;
    function CheckRecord(aCacheItem: TObject; aContexts: TConditionContexts): Boolean; virtual;
    /// <summary>проверка возможности удаления записей</summary>
    function CanDelete(aCacheItem : TObject) : boolean;  virtual;
    // собственно удаление записей
    function CheckChildRecords(aLink: TFieldMeta; const aID: Variant): Integer;
    function DeleteChildRecords(aLink: TFieldMeta; const aID: Variant): Boolean;
    function UnlinkChildRecords(aLink: TFieldMeta; const aID: Variant): Boolean;
    function DeleteRecord(aCacheItem: TObject; const aProcessChildren: Boolean = True): Boolean; virtual;
    // удаляет несколько записей из базы данных (записи могут не быть начитаны)
    function DeleteRecords(aFilter: TFilterItem): Boolean;
  end;

  /// <summary>DataManager для главного меню</summary>
  TMenuDataManager = class(TDataManager)
  protected
  public
    constructor Create;
    procedure SetItem(aItem: TObject; aMenuItem: TContextMeta);
    procedure SetupNewMeta(aItem: TContextMeta);
    function InsertItem(aItem: TContextMeta): Boolean;
    function UpdateItem(aItem: TContextMeta): Boolean;
    function DeleteItem(aItem: TContextMeta): Boolean;
    function CheckRecord(aCacheItem: TObject; aContexts: TConditionContexts): Boolean; override;
  end;

  /// <summary>DataManager для таблицы полей</summary>
  TFieldsDataManager = class(TDataManager)
  private
    function CheckLinkSolution(aField: TFieldMeta): Boolean;
    function CheckExpression(aField: TFieldMeta): Boolean;
  protected
  public
    constructor Create;
    function InsertFieldMeta(aField: TFieldMeta): Boolean;
    function UpdateFieldMeta(aField: TFieldMeta): Boolean;
    function InsertRecord(aCacheItem: TObject): Boolean; override;
    function UpdateRecord(aCacheItem: TObject): Boolean; override;
    function DeleteRecord(aCacheItem: TObject; const aProcessChildren: Boolean = True): Boolean; override;
    function CheckRecord(aCacheItem: TObject; aContexts: TConditionContexts): Boolean; override;
  end;

  /// <summary>DataManager для таблицы полей</summary>
  TElementsDataManager = class(TDataManager)
  private
    function CheckLinkSolution(aCacheItem: TObject): Boolean;
  protected
  public
    constructor Create;
    function LoadElements(const aTable: TTableMeta): TElementMeta;
    procedure SetElement(aItem: TObject; aElement: TElementMeta);
    function InsertElement(aElement: TElementMeta): Boolean;
    function UpdateElement(aElement: TElementMeta): Boolean;
    function DeleteElement(aElement: TElementMeta): Boolean;
    function InsertRecord(aCacheItem: TObject): Boolean; override;
    function UpdateRecord(aCacheItem: TObject): Boolean; override;
    function DeleteRecord(aCacheItem: TObject; const aProcessChildren: Boolean = True): Boolean; override;
    function CheckRecord(aCacheItem: TObject; aContexts: TConditionContexts): Boolean; override;
  end;

  /// <summary>DataManager для баз данных</summary>
  TDatabaseDataManager = class(TDataManager)
  public
    constructor Create;
    function InsertRecord(aCacheItem: TObject): Boolean; override;
    function UpdateRecord(aCacheItem: TObject): Boolean; override;
    function DeleteRecord(aCacheItem: TObject; const aProcessChildren: Boolean = True): Boolean; override;
  end;

  /// <summary>DataManager для наборов данных</summary>
  TDatasetDataManager = class(TDataManager)
  private
    function CheckBasedOnSolution(aCacheItem: TObject): Boolean;
  protected
  public
    constructor Create;
    function UpdateTableMeta(aTable: TTableMeta): Boolean;
    function InsertRecord(aCacheItem: TObject): Boolean; override;
    function UpdateRecord(aCacheItem: TObject): Boolean; override;
    function DeleteRecord(aCacheItem: TObject; const aProcessChildren: Boolean = True): Boolean; override;
    function CheckRecord(aCacheItem : TObject; aContexts : TConditionContexts) : boolean;  override;
  end;

  /// <summary>DataManager для пользователей</summary>
  TUsersDataManager = class(TDataManager)
  protected
  public
    constructor Create;
    function CheckRecord(aCacheItem: TObject; aContexts: TConditionContexts): Boolean; override;
  end;

  /// <summary>DataManager для членства в группах</summary>
  TMembershipDataManager = class(TDataManager)
  protected
  public
    constructor Create;
    function CheckRecord(aCacheItem: TObject; aContexts: TConditionContexts): Boolean; override;
  end;

  // DataManager для таблицы ограничений
  TConstraintDataManager = class(TDataManager)
  protected
  public
    constructor Create;
    function CheckRecord(aCacheItem: TObject; aContexts: TConditionContexts): Boolean; override;
  end;

/// <summary>создает DataManager для указанной таблицы</summary>
function CreateDataManager(aTable: TTableMeta): TDataManager;

implementation

uses Types, SysUtils, Variants, DB, IBQuery, Windows, Forms, StrUtils, Controls, DCPmd5,
     DeLog, Dictionary, Funcs, Security, DeMetadata, DeParser, DeActions,
     DeScript, QueryDescriptor, DataCacheUnit, ActionUnit, DeCalculator;


function CreateDataManager(aTable: TTableMeta): TDataManager;
begin
  if Assigned(aTable) then
    if aTable = MetaData.MetaTables[idxFields] then
      Result := TFieldsDataManager.Create
    else if aTable = MetaData.MetaTables[idxMenu] then
      Result := TMenuDataManager.Create
    else if aTable = MetaData.MetaTables[idxElement] then
      Result := TElementsDataManager.Create
    else if aTable = MetaData.MetaTables[idxBase] then
      Result := TDatabaseDataManager.Create
    else if aTable = MetaData.MetaTables[idxDataset] then
      Result := TDatasetDataManager.Create
    else if aTable = MetaData.MetaTables[idxUsers] then
      Result := TUsersDataManager.Create
    else if aTable = MetaData.MetaTables[idxMembership] then
      Result := TMembershipDataManager.Create
    else if aTable = MetaData.MetaTables[idxConstraints] then
      Result := TConstraintDataManager.Create
    else
      begin
        Result := TDataManager.Create;
        Result.Table := aTable;
      end
  else
    Result := nil;
end;

{ TCustomError }

function TCustomError.GetStringParam(aParam : TVarRec) : string;
begin
  with aParam do
    case VType of
      vtString         : result := VString^;
      vtPChar          : result := VPChar;
      vtUnicodeString  : result := VPWideChar;
      vtAnsiString     : result := string(VAnsiString);
      vtVariant        : result := VarAsType(VVariant^, varString);
    end;
end;

function TCustomError.GetIntegerParam(aParam : TVarRec) : integer;
begin
  with aParam do
    case VType of
      vtInteger    : result := VInteger;
      vtVariant    : result := VarAsType(VVariant^, varInteger);
      else           result := 0;
    end;
end;

function TCustomError.GetBooleanParam(aParam : TVarRec) : boolean;
begin
  with aParam do
    case VType of
      vtBoolean    : result := VBoolean;
      vtVariant    : result := VarAsType(VVariant^, varBoolean);
      else           result := false;
    end;
end;

{ TContainError }

constructor TContainError.Create(const aCount: Integer);
begin
  inherited Create;
  FObjectsCount := aCount;
end;

function TContainError.GetMessage: string;
begin
  Result := Format(GetTitle('_De.Contain'), [FFolderName, FObjectsCount]);
end;

procedure TContainError.SetEnvironment(const aInfo: array of const);
begin
  FFolderName := GetStringParam(aInfo[0]);
end;

{ TChildError }

constructor TChildError.Create(const aPartName: string);
begin
  inherited Create;
  FPartName := aPartName;
end;

function TChildError.GetMessage: string;
begin
  Result := Format(GetTitle('_Error.Child'), [FParentObjectName, FPartName]);
end;

procedure TChildError.SetEnvironment(const aInfo: array of const);
begin
  FParentObjectName := GetStringParam(aInfo[0]);
end;

{ TChildrenError }

constructor TChildrenError.Create(const aName: string; const aCount: Integer);
begin
  inherited Create;
  FObjectName := aName;
  FObjectsCount := aCount;
end;

function TChildrenError.GetMessage: string;
begin
  Result := Format(GetTitle('_Error.Children'), [FParentObjectName, FObjectName, FObjectsCount]);
end;

procedure TChildrenError.SetEnvironment(const aInfo: array of const);
begin
  FParentObjectName := GetStringParam(aInfo[0]);
end;

{ TAnyError }

constructor TAnyError.Create(const aMessage: string);
begin
  inherited Create;
  FErrorMessage := GetTitle(aMessage);
end;

function TAnyError.GetMessage: string;
begin
  result := FErrorMessage;
end;

procedure TAnyError.SetEnvironment(const aInfo: array of const);
begin
  // пока начинки нет, но метод не должен оставаться абстрактным
end;

{ TFieldError }

constructor TFieldError.Create(const aField: string; const aMessage: string);
begin
  inherited Create(aMessage);
  FFieldName := aField;
end;

{ TIDError }

constructor TIDError.Create(const aID: Integer; const aMessage: string);
begin
  inherited Create(aMessage);
  FID := aID;
end;

{ TErrorsList }

function TErrorsList.Get(const Index: Integer): TCustomError;
begin
  if Index < Count then
    Result := TCustomError(inherited Items[Index])
  else
    Result := nil;
end;

procedure TErrorsList.Put(const Index: Integer; aData: TCustomError);
begin
  if Index >= Count then Count := Succ(Index);
  inherited Items[Index] := aData;
end;

function TErrorsList.GetMessage: string;
var I : integer;
begin
  result := EmptyStr;
  for I := 0 to Count-1 do
    StrAdd(result, '; '#10, Items[I].GetMessage);
end;

procedure TErrorsList.SetEnvironment(const aInfo: array of const);
var I : integer;
begin
  for I := 0 to Count-1 do
    Items[I].SetEnvironment(aInfo);
end;

procedure TErrorsList.DeleteErrors(const aStartIndex, aFinishIndex: Integer);
var I : integer;
begin
  for I := aStartIndex to aFinishIndex do
    Delete(aStartIndex);
end;

{ TDataManager }

constructor TDataManager.Create;
begin
  inherited Create;
  FErrors := TErrorsList.Create;
  FCalculator := TDeCalculator.Create;
  TDeCalculator(FCalculator).OnGetIdentValue := GetFieldValue;
end;

destructor TDataManager.Destroy;
begin
  FreeAndNil(FCalculator);
  FErrors.Free;
  FQuery.Free;
  FIQuery.Free;
  FUQuery.Free;
  FDQuery.Free;
  inherited Destroy;
end;

function TDataManager.GetFieldValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
begin
  Result:= TCacheItem(aDataObject).FieldValueRecurce(aIdent, aValue);
end;

function TDataManager.CheckConstraint(aConstraint : TDeConstraint;
 aCacheItem : TObject;  aContexts : TConditionContexts) : TCheckResult;
begin
  result := rsNoError;
  if aConstraint.Context * aContexts <> [] then
  begin
    if VarAsType(TDeCalculator(FCalculator).Calculate(aConstraint.ExprPostfix, TCacheItem(aCacheItem)), varBoolean) then
      case aConstraint.Event of
        ckError:
          begin
            Errors.Clear;
            if Assigned(aConstraint.ErrorField) then
              Errors.Add(TFieldError.Create(aConstraint.ErrorField.Original, aConstraint.StrMessage))
            else
              Errors.Add(TAnyError.Create(aConstraint.StrMessage));
            result := rsError;
          end;
        ckWarning:
          begin
            if Errors.Count = 0 then
              if Assigned(aConstraint.ErrorField) then
                Errors.Add(TFieldError.Create(aConstraint.ErrorField.Original, aConstraint.StrMessage))
              else
                Errors.Add(TAnyError.Create(aConstraint.StrMessage));
            result := rsWarning;
          end;
        else
          result := rsNoError;
      end;
  end;
end;

function TDataManager.CheckRecord(aCacheItem: TObject; aContexts: TConditionContexts): Boolean;
var I: integer;
    Q: TDeDataset;
begin
  result := true;
  if (ccInsert in aContexts) or (ccUpdate in aContexts)  then
    for I := 0 to TCacheItem(aCacheItem).Owner.FieldCount-1 do
      with TCacheItem(aCacheItem).Owner.Fields[I] do
      if (not IsReadOnly) and (not (IsLookup or (Calculated and (not IsStored)))) then
      begin
        // проверка прав
        if TCacheItem(aCacheItem).FieldIsNewValue(I) and Not SecuritySystem.CheckPolicyField(ID , sdUpdate) then
          begin
            Errors.Add(TFieldError.Create( Original, Format(GetTitle('_eRror.right'), [Native])));
            result := false;
          end;

        // проверка на NotNull
        if NotNull and VarIsNull(TCacheItem(aCacheItem).FieldValue[I]) then
          if not HaveDefaultDefDB then // база сама заполнит значение
            begin
              Errors.Add(TFieldError.Create( Original, Format(GetTitle('_dE.empty'), [Native])));
              result := false;
            end;

        // проверка на Unique
        if Unique and (ID <> TCacheItem(aCacheItem).Owner.TableMeta.OwnerTable.KField[0].ID) then
          // разрешаем несколько пустых значений, которые формально одинаковые
          if not VarIsNull(TCacheItem(aCacheItem).FieldValue[I]) then
          try
            Q:= Table.OwnerTable.Database.CreateQuery(qtSelect);
            Q.Descr.Table:= Table.OwnerTable.Table;
            Q.Descr.AddField(opCount);
            Q.Descr.AddCondition( Original, DataType, opEQ, TCacheItem(aCacheItem).FieldValue[I]);
            Q.Descr.AddCondition(TCacheItem(aCacheItem).Owner.TableMeta.OwnerTable.KField[0].Original,
              TCacheItem(aCacheItem).Owner.TableMeta.OwnerTable.KField[0].DataType, opNE, TCacheItem(aCacheItem).ID);
            Q.Descr.AddOperation(opAnd);
            Q.Open;
            if Q.Value[0] > 0 then
              begin
                result := false;
                Errors.Add(TFieldError.Create( Original, Format(GetTitle('_De.unique'), [Native])));
              end;
          finally
            Q.Free;
          end;

        // проверка ограничений длины строковых полей
        if (DataType = ftString) and (Length(VarToStr(TCacheItem(aCacheItem).FieldValue[I])) > DataSize) then
          begin
            Errors.Add(TFieldError.Create( Original, Format(GetTitle('_Error.strlen'), [Native])));
            result := false;
          end;

        if Not Result then Exit;
      end;

  // проверяем условия
  if result then
    for I := 0 to Table.Constraints.Count-1 do
      if Table.Constraints[I].Active and Table.Constraints[I].CheckPolicy(spExecute) then
        if rsError = CheckConstraint(Table.Constraints[I], aCacheItem, aContexts) then Exit(False) ;
end;

function TDataManager.GetQuery(const aIndex: Integer): TDeDataset;
begin
  case aIndex of
    1 : begin
          if not Assigned(FIQuery) then
          begin
            FIQuery := FTable.Database.CreateQuery(qtInsert);
            FIQuery.Descr.Table := Table.Table;
          end;
          result := FIQuery;
        end;
    2 : begin
          if not Assigned(FUQuery) then
          begin
            FUQuery := FTable.Database.CreateQuery(qtUpdate);
            FUQuery.Descr.Table := Table.Table;
          end;
          result := FUQuery;
        end;
    3 : begin
          if not Assigned(FDQuery) then
          begin
            FDQuery := FTable.Database.CreateQuery(qtDelete);
            FDQuery.Descr.Table := Table.Table;
          end;
          result := FDQuery;
        end;
    else
      begin
        if not Assigned(FQuery) then
        begin
          FQuery := FTable.Database.CreateQuery(qtSelect);
          FQuery.Descr.Table := Table.Table;
        end;
        result := FQuery;
      end;
    end;
end;

(*
procedure TDataManager.CheckChanges(aCacheItem : TObject; var Changes : TDataChanges);
var I                : integer;
    aValue1, aValue2 : Variant;
    Cache            : TDataCache;
begin
  // возвращает результат сравнения записи в базе данных с записью,
  // хранящейся в TCacheItem
  Cache := TDataCache.Create(Table);
  Cache.Filters.ParentManager:=nil;
  try
    Cache.LoadData(TFilterItem.Create(Table.OwnerTable.KField[0], opEQ, TCacheItem(aCacheItem).ID), TCacheItem(aCacheItem).Stage);
    if Cache.Count = 0 then
      begin
        Changes := dcNotFound;
      end
    else
      begin
        if (TCacheItem(aCacheItem).Owner.DeletedSignIndex >= 0) and
           (TCacheItem(aCacheItem).Deleted <> Cache[0].Deleted) then
          begin
            Changes := dcDeleted
          end
        else
          begin
            Changes := dcNoChanges;
            for I := 0 to Table.OwnerTable.Fields.Count-1 do
              if (not Table.OwnerTable.Fields[I].IsLookup) and
                 (not Table.OwnerTable.Fields[I].IsReadOnly) and
                 (Cache[0].Stage >= Table.OwnerTable.Fields[I].Stage) and
                 (Table.OwnerTable.Fields[I].IsStored) then
              begin
                aValue1 := TCacheItem(aCacheItem).FieldBaseValue[I];
                aValue2 :=               Cache[0].FieldBaseValue[I];
                if not DeVarSameValue(aValue1, aValue2) then
                  begin
                    Changes := dcChanged;
                    break;
                  end;
              end;
          end;
      end;
  finally
    Cache.Free;
  end;
end;
*)

procedure TDataManager.SetPrimaryKey(aCacheItem : TObject; InDS: TDeDataset = nil);
var V  : Variant;
    Q  : TDeDataset;
begin
  if (FTable.OwnerTable.KField[0].Link = 0) and
     not (FTable.OwnerTable.KField[0].ReadOnly ) then
  begin
    if FTable.OwnerTable.KField[0].DataType = ftGUID then
      begin
        TCacheItem(aCacheItem).ID := GUIDToString(NewGUID);
        Exit;
      end;
    V := Null;
    Q := FTable.OwnerTable.Database.CreateQuery(qtSelect, InDS);
    try
      Q.Descr.Table := FTable.OwnerTable.Table;
      V := Q.GetMaxKeyValue(FTable.OwnerTable.ID);
    finally
      Q.Free;
    end;

    if FTable.OwnerTable.KField[0].DataType in NumericTypes then
      begin
        if V = null then
          TCacheItem(aCacheItem).ID := 1
        else
          TCacheItem(aCacheItem).ID := VarToInt(V)+1
      end
    else
      begin
        if V = null then
          TCacheItem(aCacheItem).ID := 'a'
        else
          TCacheItem(aCacheItem).ID := SuccStr(V);
      end;
  end
end;

procedure TDataManager.PrepareRecord(aCacheItem : TObject);
type
  TValueState =
    (
      vsNone,         // Значение ещё не расчитано
      vsCalculating,  // Значение расчитывается
      vsCalculated    // Значение уже расчитано
    );
var
  CacheItem: TCacheItem;
  ValueStates: array of TValueState;
  FailFields: TStrings;
  Index: Integer;
  vDateTime: TDateTime;
  sText: String;

  {$IFDEF DEBUG}
  DebugString: string;
  DebugNoSize: Integer;
  {$ENDIF}

  {$IFDEF DEBUG}
  function DebugFooter: string;
  var
    Index: Integer;
  begin
    Result := EmptyStr;
    for Index := Low(ValueStates) to High(ValueStates) do
      Result := Result + DupeString('-', 15) + '+';
    if Length(Result) <> 0 then
      Result := #13#10'                        +' + DupeString('-', DebugNoSize + 2) + '+' + Result;
  end;
  function DebugHeader: string;
  var
    Index: Integer;
    Value: string;
  begin
    Result := EmptyStr;
    Value := DebugFooter;
    if Length(Value) <> 0 then
      for Index := Low(ValueStates) to High(ValueStates) do
        Result := Result + Format(' %13d |', [Index]);
    if Length(Result) <> 0 then
      Result := Value + #13#10'                        |' + DupeString(' ', DebugNoSize + 2) + '|' + Result + Value;
  end;
  function DebugValueStates(const FieldIndex: Integer): string;
  const
    ValueStateNames: array[TValueState] of PChar = ('vsNone', 'vsCalculating', 'vsCalculated');
  var
    Index: Integer;
  begin
    Result := EmptyStr;
    for Index := Low(ValueStates) to High(ValueStates) do
      Result := Result + Format(' %-13s |', [StrPas(ValueStateNames[ValueStates[Index]])]);
    if Length(Result) <> 0 then
      Result := Format(#13#10'                        | %*d |', [DebugNoSize, FieldIndex]) + Result;
  end;
  function DebugFailFields: string;
  var
    Index: Integer;
  begin
    Result := EmptyStr;
    for Index := 0 to Pred(FailFields.Count) do
      Result := Result + Format(#13#10'                        %*d: %s',
        [DebugNoSize, FTable.OwnerTable.Fields.IndexByName(FailFields[Index]), FailFields[Index]]);
    if Length(Result) <> 0 then
      Result := #13#10'                        FAIL FIELDS:' + Result;
  end;
  {$ENDIF}
  procedure CalculateItem(const FieldIndex: Integer);
  var
    FieldMeta: TFieldMeta;
    ExpressionItem: TExpressionItem;
    Text: AnsiString;
    function Calculate: Variant;
    var
      Index: Integer;
    begin
      { Здесь расчитываем сначала те поля, от которых зависит данное поле }
      for Index := Low(ValueStates) to High(ValueStates) do
        if ValueStates[Index] <> vsCalculated then
          if ExpressionItem.IndexOfIdent(FTable.OwnerTable.Fields[Index].Original) <> -1 then
            CalculateItem(Index);
      Result := TDeCalculator(FCalculator).Calculate(ExpressionItem, CacheItem);
    end;
  begin
    {$IFDEF DEBUG}
    DebugString := DebugString + DebugValueStates(FieldIndex);
    {$ENDIF}
    FieldMeta := FTable.OwnerTable.Fields[FieldIndex];
    case ValueStates[FieldIndex] of
      vsCalculating: { Циклическая ссылка }
        if FailFields.IndexOf(FieldMeta.Original) = -1 then
          FailFields.Append(FieldMeta.Original);
      vsNone: { Пока ещё не расчитано }
        begin
          ValueStates[FieldIndex] := vsCalculating;
          try
            if (not FieldMeta.Key) and FieldMeta.IsStored then
              begin
                ExpressionItem := FieldMeta.DefaultPostfix;
                if FieldMeta.NotNull and (VarIsNull(CacheItem.FieldValue[FieldIndex]) or VarIsEmpty(CacheItem.FieldValue[FieldIndex])) then
                  if ExpressionItem.Count <> 0 then
                    CacheItem.FieldValue[FieldIndex] := Calculate
                  else
                    if FieldMeta.DataType in StringTypes then
                      CacheItem.FieldNativeValue[FieldIndex] := FieldMeta.DefaultValue;

                if VarIsNull(CacheItem.FieldValue[FieldIndex]) or VarIsEmpty(CacheItem.FieldValue[FieldIndex]) then
                  if Length(FieldMeta.DefaultValue) = 0 then
                     if (FieldMeta.ShowType = stEncryptedPassword) and (FieldMeta.DataType in StringTypes) then
                       CacheItem.FieldValue[FieldIndex] := MD5Password(EmptyStr)
                     else
                       CacheItem.FieldValue[FieldIndex] := unassigned
                  else if FieldMeta.DataType in StringTypes then
                    begin
                      if ExpressionItem.Count <> 0 then
                        Text := Calculate
                      else
                        Text := FieldMeta.DefaultValue;
                      if (FieldMeta.ShowType = stEncryptedPassword) and (FieldMeta.DataType in StringTypes) then
                        CacheItem.FieldValue[FieldIndex] := MD5Password(WideStringToUnicode(Text))
                      else
                        CacheItem.FieldNativeValue[FieldIndex]:= Text;
                    end
                  else if FieldMeta.DataType in IntegerTypes then
                    if ExpressionItem.Count <> 0 then
                      CacheItem.FieldValue[FieldIndex] := Calculate
                    else
                      CacheItem.FieldValue[FieldIndex] := StrToInt64Def(FieldMeta.DefaultValue, 0)
                  else if FieldMeta.DataType in FloatTypes then
                    if ExpressionItem.Count <> 0 then
                      CacheItem.FieldValue[FieldIndex] := Calculate
                    else
                      CacheItem.FieldValue[FieldIndex] := StrToFloatDef(FieldMeta.DefaultValue, 0)
                  else if FieldMeta.DataType in DateTimeTypes then
                    if ExpressionItem.Count <> 0 then
                      CacheItem.FieldValue[FieldIndex] := Calculate
                    else
                      begin
                        if tryStrTodate(FieldMeta.DefaultValue, vDateTime)
                          then CacheItem.FieldValue[FieldIndex]:= vDateTime;
                      end
                  else if FieldMeta.DataType in [ftBoolean] then
                    if ExpressionItem.Count <> 0 then
                      CacheItem.FieldValue[FieldIndex] := Calculate
                    else
                      CacheItem.FieldValue[FieldIndex] := DeStrToBoolean(FieldMeta.DefaultValue, False)
                  else if FieldMeta.DataType in [ftGUID] then
                    if ExpressionItem.Count <> 0 then
                      CacheItem.FieldValue[FieldIndex] := Calculate
                    else
                      CacheItem.FieldValue[FieldIndex] := unassigned;

                //Если значение поля длиннее хранимого - то режем хвост
                if FieldMeta.DataType in StringTypes then
                  if (FieldMeta.DataSize > 0) and (Length(CacheItem.FieldValue[FieldIndex]) > FieldMeta.DataSize) then
                    CacheItem.FieldValue[FieldIndex] := Copy(CacheItem.FieldValue[FieldIndex], 1, FieldMeta.DataSize);
              end;
          finally
            ValueStates[FieldIndex] := vsCalculated;
          end;
        end;
    end;
  end;
  procedure CheckFailFields;
  var
    Text: string;
    Index: Integer;
  begin
    Text := EmptyStr;
    for Index := 0 to Pred(FailFields.Count) do
      begin
        if Length(Text) <> 0 then Text := Text + #13#10;
        Text := Text + Format(GetTitle('_Error.Field.Cycled'), [FailFields[Index]]);
      end;
    if Length(Text) <> 0 then
      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(Text)));
  end;
begin
//DONE: Возможно это ускорение ( отказ от назначения первичного ключа)
// приведет к проблемам при вставке записей
//  SetPrimaryKey(aCacheItem);
  (*
  CI := TCacheItem(aCacheItem);
  *)

  CacheItem := aCacheItem as TCacheItem;
  SetLength(ValueStates, FTable.OwnerTable.Fields.Count);
  {$IFDEF DEBUG}
  DebugString := EmptyStr;
  DebugNoSize := Length(IntToStr(FTable.OwnerTable.Fields.Count));
  {$ENDIF}
  for Index := Low(ValueStates) to High(ValueStates) do
    ValueStates[Index] := vsNone;
  FailFields := TStringList.Create;
  try
    for Index := Pred(FTable.OwnerTable.Fields.Count) downto 0 do
      CalculateItem(Index);

    {$IFDEF DEBUG}
    if Length(DebugString) <> 0 then
      DebugString := DebugHeader + DebugString + DebugFooter + DebugFailFields;
    {$ENDIF}
    CheckFailFields;
  finally
    FailFields.Free;
  end;
  {$IFDEF DEBUG}
  if Length(DebugString) <> 0 then
    DebugLog('PrepareRecord for %s ...%s', [Table.Table, DebugString]);
  {$ENDIF}
end;

function TDataManager.CanInsertRecord(aCacheItem : TObject) : boolean;
begin
  Errors.Clear;
  if Table.IsReadOnly then
  begin
    result := false;
    Errors.Add(TAnyError.Create('_De.readonly'));
  end
  else
    result := CheckRecord(aCacheItem, [ccInsert, ccUpdate]);
end;

function TDataManager.InsertRecord(aCacheItem : TObject) : boolean;
var AQuery: TDeDataset;
    i: Integer;
    TempDefault, GroupChanged: Boolean;
    V: Variant;
begin
  Errors.Clear;
  Result:= False;
  if Table.IsReadOnly then
    begin
      result := false;
      Errors.Add(TAnyError.Create('_De.readonly'))
    end else
  if (not Table.IsDynamic) and not SecuritySystem.CheckPolicyDataSet(FTable.ID, spUpdate) then
    begin
      result := false;
      Errors.Add(TAnyError.Create('_eRror.right'));
    end else
  begin
    TempDefault := False;
    if (0 < FTable.OwnerTable.KField.Count) and (FTable.OwnerTable.KField[0].ReadOnly ) then
      begin
        AQuery := FTable.Database.CreateQuery(qtInsert);
        try
          AQuery.Descr.Table := Table.Table;
          if Assigned(FTable.OField) then
            begin
              TempDefault := TCacheItem(aCacheItem).Default;
              if TempDefault then
                begin
                  GroupChanged:= False;
                  for i:=0 to Pred(TCacheItem(aCacheItem).Owner.FieldCount) do
                    if frGroupDefault in TCacheItem(aCacheItem).Owner.Fields[i].Role then
                      GroupChanged:= GroupChanged or
                        Not DeVarSameValue(TCacheItem(aCacheItem).FieldBaseValue[i], TCacheItem(aCacheItem).FieldValue[i]);
                  if Not GroupChanged then
                    TCacheItem(aCacheItem).Default := False;
                end;
            end;
          Result := AQuery.InsertRecord(aCacheItem);
        finally
          AQuery.Free;
        end;
      end
    else
      begin
        // Назначем ключ, если он не задан
        if (0 < FTable.OwnerTable.KField.Count) then
          begin
            V:= TCacheItem(aCacheItem).ID;
            if VarIsNull(V) or VarIsEmpty(V) then
              SetPrimaryKey(aCacheItem);
          end;

        if Assigned(FTable.OField) then
          begin
            TempDefault := TCacheItem(aCacheItem).Default;
            if TempDefault then
              TCacheItem(aCacheItem).Default := False;
          end;

        try
          result := IQuery.InsertRecord(aCacheItem);
        except
          On E: Exception do
            begin
              {$IFDEF DEBUG}
              DebugLog('TDataManager.InsertRecord error: ' + E.Message);
              {$ENDIF}
              SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
                                                                      NativeInt(pChar('_dL.procedureerror'+#10 + E.Message)));
            end;
        end;
      end;

    // проверка, присутствует ли в таблице запись по умолчанию
    if result then
      if Assigned(FTable.OField) then
        if TempDefault {TCacheItem(aCacheItem).Default} then Result := UpdateNearRecords(aCacheItem);

    if result then
    begin
      DoAct('OnInsert', TCacheItem(aCacheItem));
      DoAct('OnChange', TCacheItem(aCacheItem));
      MetaData.UpdateLibrary(Table.ID);
      if assigned(Table.NField) then
        Table.NField.UpdateControls;
      // Обновляем действия если надо ...
      if assigned(MetaData.MetaTables[idxCommands]) then
        if(Table.ID = MetaData.MetaTables[idxCommands].ID) and Assigned(ProfitActions) then
           ProfitActions.UpdateData(VarToInt(TCacheItem(aCacheItem).ID), mcInsert);
    end
    else
    begin
      result := false;
      Errors.Add(TAnyError.Create(GetTitle('_Error.Edit')))
    end;
  end;
end;

function TDataManager.ReReadRecord(aCacheItem : TObject) : boolean;
var Cache : TDataCache;
begin
  result := false;
  Errors.Clear;

  // в базе такой записи еще нет, сравнивать нечего.
  if isInserted in TCacheItem(aCacheItem).State then
    begin
      Errors.Add(TAnyError.Create(GetTitle('_Error.deletebyuser')+' [#1]'));
      Exit;
    end;

  Cache := TDataCache.Create(Table);
  Cache.Filters.ParentManager:=nil;
  try
    Cache.Filters.NewFilter(Table.OwnerTable.KField[0], opEQ, TCacheItem(aCacheItem).ID);
    Cache.LoadData(nil, TCacheItem(aCacheItem).Stage);
    if Cache.Count = 0 then
      begin
        Errors.Add(TAnyError.Create(GetTitle('_Error.deletebyuser')+' [#2]'));
      end
    else
      begin
        if (TCacheItem(aCacheItem).Owner.DeletedSignIndex >= 0) and
           (TCacheItem(aCacheItem).Deleted <> Cache[0].Deleted) then
          begin
            Errors.Add(TAnyError.Create(GetTitle('_Error.deletebyuser')+' [#3]'));
          end
        else
          begin
            Result := true;
            TCacheItem(aCacheItem).Assign(Cache[0]);
          end;
      end;
  finally
    Cache.Free;
  end;
end;

function TDataManager.CanUpdateRecord(aCacheItem : TObject) : boolean;
var FFlag: TChangedFlags;
begin
  result := false;
  Errors.Clear;

  if Table.IsReadOnly then
    Errors.Add(TAnyError.Create('_De.readonly')) else

  if Table.ObjectType = otView then
    Errors.Add(TAnyError.Create('_De.readonly'))
  else
    begin
      // проверяем наличие изменений в кеше записи
      result:= CheckRecord(aCacheItem, [ccUpdate]);

      if Result then
        begin
          // проверяем наличие изменений в БД за время редактирования
          TCacheItem(aCacheItem).Owner.CheckChanges(TCacheItem(aCacheItem), FFlag);

          if not(flExist in FFlag) then // Запись удалили
            begin
              Errors.Add(TAnyError.Create(GetTitle('_Error.deletebyuser')));
              Result:= False;
            end else

          if not (flBasketBase in FFlag) then // Запись пометили к удалению
            begin
              Errors.Add(TAnyError.Create(GetTitle('_Error.deletebyuser')));
              Result:= False;
            end;

          if not (flDataBase in FFlag) then // Запись изменили, спросим у пользователя
            begin
              Result:= Application.MessageBox(pChar(GetTitle('_eRror.update.changedbyuser')),
                                   pChar(GetTitle('_Dl.Confirmation')), MB_YESNO or MB_ICONEXCLAMATION) = idYes;
            end;
        end;
    end;
end;

function TDataManager.UpdateNearRecords(aCacheItem: TObject): boolean;
var Q, QUpd: TDeDataSet;
    R, I: Integer;
    FieldsMeta: TFieldsMeta;
begin
  Result:= True;
  FieldsMeta := TCacheItem(aCacheItem).Owner.Fields;
  Q := FTable.Database.CreateQuery(qtSelect);
  try
    Q.Descr.BeginUpdate;
    try
      Q.Descr.Table := FTable.Table;
      Q.Descr.AddField(FTable.KField[0].Original);
      Q.Descr.AddParamCondition(FTable.OField.Original, FTable.OField.DataType, opEQ, FTable.OField.Original,
                                ConvertValue(FTable.OField.Value2, FTable.OField.DataType));

      for I := 0 to Pred(FieldsMeta.Count) do
        if FieldsMeta[i].IsStored then  { Only for real-exists fields}
          if frGroupDefault in FieldsMeta[i].Role then {Default Group}
            begin
              Q.Descr.AddParamCondition(FieldsMeta[i].Original, FieldsMeta[i].DataType, opEQ, EmptyStr, TCacheItem(aCacheItem).FieldValue[I]);
              Q.Descr.AddOperation(opAnd);
            end;

      for I := 0 to Pred(FieldsMeta.Count) do
        if FieldsMeta[i].IsStored then  { Only for real-exists fields}
          if FieldsMeta[i].Key then {Key}
            begin
              Q.Descr.AddParamCondition(FieldsMeta[I].Original, FieldsMeta[I].DataType, opEQ, EmptyStr, TCacheItem(aCacheItem).FieldValue[I]);
              Q.Descr.AddOperation(opNot);
              Q.Descr.AddOperation(opAnd);
            end;
    finally
      Q.Descr.EndUpdate;
    end;
    Q.Open;

    // убираем старую запись по умолчанию
    for R:=0 to Pred(Q.RecordCount) do
      begin
        Q.RecNo:= R;
        try
          QUpd := FTable.Database.CreateQuery(qtUpdate);
          try
            QUpd.Descr.BeginUpdate;
            QUpd.Descr.Table := FTable.Table;
            QUpd.Descr.AddField(FTable.OField.Original, FTable.OField.DataType, FTable.OField.Value1);
            QUpd.Descr.AddParamCondition(FTable.KField[0].Original, FTable.KField[0].DataType, opEQ, FTable.KField[0].Original, Q.Value[0]);
            QUpd.Descr.EndUpdate;
            QUpd.ExecuteQuery;
          finally
            QUpd.Free;
          end;
        except
          on E: Exception do
            begin
              {$IFDEF DEBUG}
              DebugLog('%s.UpdateNearRecords for %s skip error: %s', [ClassName, Table.Table, E.Message]);
              {$ENDIF}
              Result := False;
            end;
        end;
      end;
  finally
    Q.Free;
  end;
  if not TCacheItem(aCacheItem).Default then
    try
      QUpd := FTable.Database.CreateQuery(qtUpdate);
      try
        QUpd.Descr.BeginUpdate;
        QUpd.Descr.Table := FTable.Table;
        QUpd.Descr.AddField(FTable.OField.Original, FTable.OField.DataType, FTable.OField.Value2);
        QUpd.Descr.AddParamCondition(FTable.KField[0].Original, FTable.KField[0].DataType, opEQ, FTable.KField[0].Original, TCacheItem(aCacheItem).ID);
        QUpd.Descr.EndUpdate;
        QUpd.ExecuteQuery;
      finally
        QUpd.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog('%s.UpdateNearRecords for %s skip error: %s', [ClassName, Table.Table, E.Message]);
          {$ENDIF}
          Result := False;
        end;
    end;
end;

function TDataManager.UpdateRecord(aCacheItem : TObject) : boolean;
var I: Integer;
    MustUpdate: boolean;
    TCI, PCI: TCacheItem;
begin
  if Table.IsReadOnly then
    begin
      Result := false;
      Errors.Add(TAnyError.Create('_De.readonly'));
    end
  else
    begin
      Result := True;
      TCI:= TCacheItem(aCacheItem);

      // проверка, присутствует ли в таблице запись по умолчанию
      if Assigned(FTable.OField) then
        if TCI.Default then
          Result := UpdateNearRecords(aCacheItem);

      // устанавливаем хранимые вычисляемые поля
      for I := 0 to TCI.Owner.FieldCount-1 do
        if TCI.Owner.Fields[I].Calculated then
          if TCI.Owner.Fields[I].IsStored then
            if TCI.Owner.Fields[I].DefaultPostfix.Count > 0 then
              TCI.FieldValue[I] := TDeCalculator(FCalculator).Calculate(TCI.Owner.Fields[I].DefaultPostfix, TCI);

      // проверяем вложение "живой" записи в удаленную папку
      if Assigned(FTable.DField) and Assigned(FTable.PField) then
        if FTable.PField.LinkType in [daRestrict, daCascade, daFullCascade] then
          if not VarIsNull(TCI.ParentID) then
            begin
              PCI:=TCI.Owner.FindById(TCI.ParentID);
              if Assigned(PCI) and PCI.Deleted and (Not TCI.Deleted) and
                 (TCI.ValueByName[FTable.PField.Original]<>
                  TCI.BaseValueByName[FTable.PField.Original]) then
                Errors.Add(TAnyError.Create('_De.ownerdeleted'));
            end;

      try
        Result := Result and (Errors.Count=0) and (UQuery.UpdateRecord(aCacheItem));

        if Result then
          begin
            //если изменили поле заглавное или fvLevel3, то обновляем control'ы
            MustUpdate:=False;
            for I := 0 to TCI.Owner.FieldCount-1 do
              if ((TCI.Owner.Fields[I] = Table.NField) or
                  (TCI.Owner.Fields[I].VisibleLevel = fvLevel3 ))
                 and
                 ( TCI.FieldIsNewValue(i) ) // с изменениями
              then
                begin
                  MustUpdate:=True;
                  Break
                end;

            if MustUpdate then  // ComboBox'ы
              if Assigned(Table.NField) then
                Table.NField.UpdateControls;

            // обновляем библиотечные наборы данных
            MetaData.UpdateLibrary(Table.ID);

            TCI.Owner.BeginUpdate;
            DoAct('OnUpdate', TCI);
            DoAct('OnChange', TCI);
            TCI.Owner.EndUpdate;
            // Обновляем действия если надо ...
            if (Table.ID = MetaData.MetaTables[idxCommands].ID) and Assigned(ProfitActions) then
               ProfitActions.UpdateData(VarToInt(TCI.ID), mcInsert);
          end
        else
          begin
            if Errors.Count=0 then
              Errors.Add(TAnyError.Create(GetTitle('_dL.procedureerror'+#10+'_Error.Edit')));
          end;
     except
        on E: Exception do
        if Length(E.Message)>0 then
          begin
            if SameText(Copy(E.Message, 1, 7), 'Script:') then
              MetaData.ExecuteScript(Copy(E.Message, 8, MaxInt))
            else
              if SameText(Copy(E.Message, 1, 11), 'SendMessage') then
                SendMessage(Application.MainForm.Handle, DM_EXTSENDMESSAGE, NativeInt(PChar(Copy(E.Message, 12, MaxInt))), 0) else

            if CompareText(copy(E.Message,1,5),'info:')=0 then
              SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(pChar(copy(E.Message,6,MaxInt)))) else

              SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(pChar(E.Message)))
          end
        else
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(pChar('_dL.procedureerror')));
      end;
    end;
end;

function TDataManager.CanDelete(aCacheItem : TObject) : boolean;
var I, R, J       : integer;
    Q             : TDeDataset;
    ChildCache    : TDataCache;
//  STable, NTable : TTableMeta;
    Filter        : TFilterItem;
    FFlag: TChangedFlags;
    DeleteIsUpdate: Boolean;
begin

  { проверка условия readonly }
  Errors.Clear;
   if Table.IsReadOnly then
     Errors.Add(TAnyError.Create('_dE.readonly'));

  { проверка прав на удаление записи }
  if Errors.Count=0 then
    if not SecuritySystem.CheckPolicyDataSet(FTable.ID, spUpdate) then
      Errors.Add(TAnyError.Create('_eRror.right'));

  { Начитаем при необходимости и проверим }
  TCacheItem(aCacheItem).Owner.CheckChanges(TCacheItem(aCacheItem), FFlag);

  { проверка условий на удаление записи, при наличии ошибок они попадают в массив Errors }
  if Errors.Count=0 then
    if DeleteIsUpdate then
      CheckRecord(aCacheItem, [ccBasket])
    else
      CheckRecord(aCacheItem, [ccDelete]);

  { саму запись удалить можно - проверяем, изменена ли запись }
  if Errors.Count=0 then
    begin
      if not (flExist in FFlag) then
        Errors.Add(TAnyError.Create(GetTitle('_Error.deletebyuser')));

      // TCacheItem(aCacheItem).Owner.DeletedSignIndex >= 0; // Наличие флага удаления
      // можно выделить обработку "уделения" записи удаленной другим пользователем
      if not (flDataBase in FFlag) or not (flBasketBase in FFlag) then
        if Application.MessageBox( pchar(GetTitle('_eRror.delete.changedbyuser')),
                                   pChar(GetTitle('_Dl.Confirmation')),
                                   MB_ICONEXCLAMATION or MB_YESNO) = idNo then
          begin
            Result := False;
            exit;
          end;
    end;

  { проверяем возможность удаления связанных записей - daRestrict }
  if Errors.Count=0 then
    for I := 0 to Table.OwnerTable.Links.Count-1 do
      if (Table.OwnerTable.Links[I].LinkType in [daNone, daRestrict]) and (Table.OwnerTable.Links[I].IsStored) then
        begin
          R:= CheckChildRecords(Table.OwnerTable.Links[I], TCacheItem(aCacheItem).ID);
          if R>0 then
            Errors.Add(TChildrenError.Create( GetTitle(Table.OwnerTable.Links[I].Owner.Name), R));
        end;

  // проверяем возможность удаления связанных записей - daCascade
  if Errors.Count=0 then
    for I := 0 to Table.OwnerTable.Links.Count-1 do
      if (Table.OwnerTable.Links[I].LinkType in [daCascade, daFullCascade]) and (Table.OwnerTable.Links[I].IsStored) then
        begin

            if Table.OwnerTable.Links[I].Owner.OwnerTable.Links.Count = 0 then
              begin
                { дочерние таблицы не имеют связей }
                if (Table.OwnerTable.Links[I].LinkType in [daCascade, daFullCascade]) and (Table.OwnerTable.Links[I].IsStored) then
                if Table.OwnerTable.Links[I].Owner.Constraints.Count>0 then
                begin
                  try
                    { условия запрета удаления }
                    for J := 0 to Table.OwnerTable.Links[I].Owner.Constraints.Count-1 do
                      if (Table.OwnerTable.Links[I].Owner.Constraints[J].Context * [ccDelete] <> [])
                        and (Table.OwnerTable.Links[I].Owner.Constraints[J].Event = ckError) then
                         begin
                           Q := Table.OwnerTable.Links[I].Owner.Database.CreateQuery(qtSelect);
                           Q.Descr.BeginUpdate;
                           try
                             Q.Descr.Table := Table.OwnerTable.Links[I].Owner.Table;
                             Q.Descr.AddField(opCount);
                             Q.Descr.AssignFilter(Table.OwnerTable.Links[I].Owner.Constraints[J].ExprPostfix, opOr);
                              { условия связи с родительской записью }
                             Q.Descr.AddParamCondition(
                                   Table.OwnerTable.Links[I].Original,
                                   Table.OwnerTable.Links[I].DataType,
                                   opEQ, EmptyStr,
                                   TCacheItem(aCacheItem).ID);
                              Q.Descr.AddOperation(opAnd);
                             { признак удаленности }
                             if Assigned(Table.OwnerTable.Links[I].Owner.DField) then
                               begin
                                 Q.Descr.AddParamCondition(
                                   Table.OwnerTable.Links[I].Owner.DField.Original,
                                   Table.OwnerTable.Links[I].Owner.DField.DataType,
                                   opEQ, EmptyStr,
                                   Table.OwnerTable.Links[I].Owner.DField.Value2);
                                 Q.Descr.AddOperation(opNot);
                                 Q.Descr.AddOperation(opAnd);
                             end;
                           finally
                             Q.Descr.EndUpdate;
                           end;
                           Q.Open;
                           if Q.Value[0] > 0 then
                             Errors.Add(TChildError.Create(GetTitle(Table.OwnerTable.Links[I].Owner.Name)+ ' : '+
                                        Table.OwnerTable.Links[I].Owner.Constraints[J].StrMessage +' - '+IntToStr(Q.Value[0])) );
                           Q.Free;
                         end;
                  finally
                  end;
                end;
              end
            else
              begin
                { дочерние записи имеют связи - загружаем записи и проверяем их поочередно }
                ChildCache := nil;
                if Table.OwnerTable.Links[I].IsStored then
                try
                  ChildCache := TDataCache.Create( MetaData.GetTableMeta(Table.OwnerTable.Links[I].Owner.ID) );
                  Filter := TFilterItem.Create;
                  Filter.AddCondition(Table.OwnerTable.Links[I], opEQ, TCacheItem(aCacheItem).ID);
                  if Assigned(ChildCache.TableMeta.DField) then
                    begin
                      Filter.AddCondition(ChildCache.TableMeta.DField, opEQ, ChildCache.TableMeta.DField.Value2);
                      Filter.AddOperation(opNot);
                      Filter.AddOperation(opAnd);
                    end;
                  ChildCache.LoadData(Filter);
                  for J := 0 to ChildCache.Count-1 do
                    if not ChildCache.CanDeleteRecord(ChildCache[J]) then
                      begin
                        Errors.Add(TChildError.Create(GetTitle(Table.OwnerTable.Links[I].Owner.Name)));
                        Break;
                      end;
                finally
                  ChildCache.Free;
                end;
              end;

        end;

        {
        //блок нужен только приналичии ограничений на удаление ярлыков и примечаний
        // проверяем возможность удаления ярлыков
        if not Table.IsInterrelations then
        begin
          STable := MetaData.MetaTables[idxInterrelations];
          if Assigned(STable) then
          if STable.Constraints.Count>0 then
          begin
            Q := STable.Database.CreateQuery(qtCount);
            Q.Descr.Table := STable.Table;
            for J := 0 to STable.Constraints.Count-1 do
              if (STable.Constraints[J].Context * [ccDelete] <> [])
                and (STable.Constraints[J].Event = ckError) then
              Q.Descr.AssignFilter(STable.Constraints[J].ExprPostfix, opOr);
            Q.Descr.AddCondition(
                  fldIRParentKey,
                  STable.Fields.FindByName(fldIRParentKey).DataType,
                  opEQ,
                  TCacheItem(aCacheItem).ID);
            Q.Descr.AddCondition(
                  fldIRChildKey,
                  STable.Fields.FindByName(fldIRChildKey).DataType,
                  opEQ,
                  TCacheItem(aCacheItem).ID);
            Q.Descr.AddOperation(opOr);
            Q.Descr.AddOperation(opAnd);
            Q.Open;
            if Q.Value[0] > 0 then
              Errors.Add(TChildError.Create(GetTitle(STable.Name)));
            Q.Free;
          end;
        end;

        // проверяем возможность удаления заметок
        if not Table.IsNotes then
        begin
          NTable := MetaData.MetaTables[idxNotes];
          if Assigned(NTable) then
          if NTable.Constraints.Count>0 then
          begin
            Q := NTable.Database.CreateQuery(qtCount);
            Q.Descr.Table := NTable.Table;
            for J := 0 to NTable.Constraints.Count-1 do
              if (NTable.Constraints[J].Context * [ccDelete] <> [])
                and (NTable.Constraints[J].Event = ckError) then
                Q.Descr.AssignFilter(NTable.Constraints[J].ExprPostfix, opOr);

            Q.Descr.AddCondition(
                  fldNotesKey,
                  NTable.Fields.FindByName(fldNotesKey).DataType,
                  opEQ,
                  TCacheItem(aCacheItem).ID);
              Q.Descr.AddOperation(opAnd);
              Q.Open;
              if Q.Value[0] > 0 then
                Errors.Add(TChildError.Create(GetTitle(NTable.Name)));
            Q.Free;
          end;
        end;
        {}
  result := Errors.Count = 0;
end;

function TDataManager.CheckChildRecords(aLink: TFieldMeta; const aID: Variant): Integer;
var Q: TDeDataSet;
begin
  Q := aLink.Owner.OwnerTable.Database.CreateQuery(qtSelect);
  try
    Result := -1;
    Q.Descr.BeginUpdate;
    try
      Q.Descr.Table := aLink.Owner.OwnerTable.Table;
      Q.Descr.AddField(opCount);
      Q.Descr.AddParamCondition(aLink.Original, aLink.DataType, opEQ, EmptyStr, aID);

      // дерево, разрешаем удаление записи, которая ссылается на себя
      if aLink.Link = aLink.Owner.OwnerTable.Id then
        begin
          Q.Descr.AddParamCondition(aLink.Owner.OwnerTable.KField[0].Original, aLink.DataType, opNE, EmptyStr, aID);
          Q.Descr.AddOperation(opAnd);
        end;

      if Assigned(aLink.Owner.OwnerTable.DField) then
        begin
          Q.Descr.AddParamCondition(
            aLink.Owner.OwnerTable.DField.Original,
            aLink.Owner.OwnerTable.DField.DataType,
            opEQ, EmptyStr,
            aLink.Owner.OwnerTable.DField.Value2);
          Q.Descr.AddOperation(opNot);
          Q.Descr.AddOperation(opAnd);
        end;
    finally
      Q.Descr.EndUpdate;
    end;
    Q.Open;
    Result := Q.Value[0];
  finally
    Q.Free;
  end;
end;

function TDataManager.DeleteChildRecords(aLink: TFieldMeta; const aID: Variant): Boolean;
var I     : integer;
    Cache : TDataCache;
begin
  result := true;
  Cache := nil;
  try
    Cache := TDataCache.Create(aLink.Owner);
    Cache.Filters.NewFilter( aLink, opEQ, aID);
    for I := 0 to Cache.Count-1 do
      if Assigned(aLink.Owner.DField) then
      begin
        Cache[I].Deleted := true;
        UQuery.UpdateRecord(Cache[I]);
      end
      else
        DQuery.DeleteRecord(Cache[I]);
  finally
    Cache.Free;
  end;
end;

function TDataManager.UnlinkChildRecords(aLink: TFieldMeta; const aID: Variant): Boolean;
var Q         : TDeDataSet;
    Cache     : TDataCache;
    I,LF      : integer;
    LinkField : TFieldMeta;
begin
  result := true;
  Cache := TDataCache.Create(aLink.Owner);
  try
    Cache.Filters.NewFilter(aLink, opEQ, aID);
    Cache.LoadData;
    if Cache.Count>0 then
      begin
        LinkField := Cache.TableMeta.Fields.FindByID(aLink.ID);
        LF := Cache.TableMeta.Fields.IndexByID(LinkField.ID);

        Q := aLink.Owner.Database.CreateQuery(qtUpdate);
        Q.Descr.Table := aLink.Owner.Table;
        for I := 0 to Cache.Count-1 do
        begin
          Cache[I].FieldValue[LF] := Null;
          result := result and Q.UpdateRecord(Cache[I]);
        end;
        Q.Free;
      end;
  finally
  end;
  Cache.Free;
end;

function TDataManager.DeleteRecord(aCacheItem: TObject; const aProcessChildren: Boolean): Boolean;
var I, J           : integer;
    Permanent      : boolean;
    Cache          : TDataCache;
    STable, NTable : TTableMeta;
    Q              : TDeDataset;
    Filter         : TFilterItem;
    IsEmptyExcept  : Boolean;
begin
  result := true;
  if Table.IsReadOnly then
  begin
    result := false;
    Errors.Add(TAnyError.Create('_De.readonly'));
  end
  else
  begin
    // проверяем, окончательно ли удаление записи
    Permanent := TCacheItem(aCacheItem).Deleted or (not Assigned(Table.DField));

    // обрабатываем связанные записи
    if aProcessChildren then
      for I := 0 to Table.OwnerTable.Links.Count-1 do
      if Table.OwnerTable.Links[I].IsStored then
      begin
        case  Table.OwnerTable.Links[I].LinkType of
          daNone:
              result := True;
          daSetNull:
              result := UnlinkChildRecords(Table.OwnerTable.Links[I], TCacheItem(aCacheItem).ID );
          daRestrict:
              Result := (CheckChildRecords(Table.OwnerTable.Links[I], TCacheItem(aCacheItem).ID )=0);
          daCascade, daFullCascade:
            begin
              // каскадное удаление
              if Table.OwnerTable.Links[I].Owner.OwnerTable.Links.Count > 0 then
              // обрабатываем записи по одной
              begin
                Cache := nil;
                try
                  // начитываем данные
                  Cache := TDataCache.Create(Table.OwnerTable.Links[I].Owner);
                  Filter := TFilterItem.Create;
                  Filter.AddCondition(Table.OwnerTable.Links[I], opEQ, TCacheItem(aCacheItem).ID);
                  if Assigned(Cache.TableMeta.DField) then
                    if Cache.TableMeta.DField.DataType in DateTypes then
                      begin
                        Filter.AddCondition(Cache.TableMeta.DField, opIs, Null);

                        Filter.AddIdent(Cache.TableMeta.DField.Original, Cache.TableMeta.DField.DataType, Cache.TableMeta.DField.CodePage);
                        Filter.AddConst(True, ftBoolean);
                        Filter.AddFunc('NOW', 1);
                        Filter.AddOperation(opGT);

                        Filter.AddOperation(opOr);
                        Filter.AddOperation(opAnd);
                      end
                    else
                      begin
                        Filter.AddCondition(Cache.TableMeta.DField, opEQ, Cache.TableMeta.DField.Value2);
                        Filter.AddOperation(opNot);
                        Filter.AddOperation(opAnd);
                      end;
                  Cache.LoadData(Filter);
                  // обрабатываем дочерние записи
                  for J := Cache.Count-1 downto 0  do
                    if not Cache.DeleteRecord(Cache[J]) then
                      break;
                finally
                  Cache.Free;
                end;
              end
              else
                result := DeleteChildRecords(Table.OwnerTable.Links[I], TCacheItem(aCacheItem).ID);
              if result and Permanent then
                result := UnlinkChildRecords(Table.OwnerTable.Links[I], TCacheItem(aCacheItem).ID);
            end;

        end;
        if not result then break;
      end;

    // удаляем ярлыки
    if aProcessChildren and Permanent and (not Table.IsInterrelations) then
    begin
      STable := MetaData.MetaTables[idxInterrelations];
      if Assigned(STable) then
      begin
        Q := STable.Database.CreateQuery(qtDelete);
        try
          Q.Descr.BeginUpdate;
          try
            Q.Descr.Table := STable.Table;
            Q.Descr.AddParamCondition(
                  fldIRParentKey,
                  STable.Fields.FindByName(fldIRParentKey).DataType,
                  opEQ, EmptyStr,
                  TCacheItem(aCacheItem).ID);
            Q.Descr.AddParamCondition(
                  fldIRParentTable,
                  STable.Fields.FindByName(fldIRParentTable).DataType,
                  opEQ, EmptyStr,
                  Table.ID);
            Q.Descr.AddOperation(opAnd);

            Q.Descr.AddParamCondition(
                  fldIRChildKey,
                  STable.Fields.FindByName(fldIRChildKey).DataType,
                  opEQ, EmptyStr,
                  TCacheItem(aCacheItem).ID);
            Q.Descr.AddParamCondition(
                  fldIRChildTable,
                  STable.Fields.FindByName(fldIRChildTable).DataType,
                  opEQ, EmptyStr,
                  Table.ID);
            Q.Descr.AddOperation(opAnd);

            Q.Descr.AddOperation(opOr);
          finally
            Q.Descr.EndUpdate;
          end;
          try
            Q.ExecuteQuery;
          except
            on E: Exception do
              begin
                result:= False;
                Funcs.WriteLog('TDataManager.DeleteRecord '+E.Message);
              end;
          end;
        finally
          Q.Free;
        end;
      end;
    end;

    // удаляем заметки
    if aProcessChildren and Permanent and (not Table.IsNotes) then
    begin
      NTable := MetaData.MetaTables[idxNotes];
      if Assigned(NTable) then
      begin
        Q := NTable.Database.CreateQuery(qtDelete);
        Q.Descr.BeginUpdate;
        try
          Q.Descr.Table := NTable.Table;
          Q.Descr.AddParamCondition(
                fldNotesTable,
                NTable.Fields.FindByName(fldNotesTable).DataType,
                opEQ, EmptyStr,
                Table.ID);
          Q.Descr.AddParamCondition(
                fldNotesKey,
                NTable.Fields.FindByName(fldNotesKey).DataType,
                opEQ, EmptyStr,
                TCacheItem(aCacheItem).ID);
          Q.Descr.AddOperation(opAnd);
        finally
          Q.Descr.EndUpdate;
        end;
        try
          Q.ExecuteQuery;
        except
          on E: Exception do
            begin
              result:= False;
              Funcs.WriteLog('TDataManager.DeleteRecord '+E.Message);
            end;
        end;
        Q.Free;
      end;
    end;

    // удаляем запись
    if result then
    begin
      TCacheItem(aCacheItem).FillItem(fsMax);
      TCacheItem(aCacheItem).Deleted := true;
      IsEmptyExcept:=False;

      try
        with Table do
          if not Permanent then
            result := UQuery.UpdateRecord(aCacheItem)
          else
            result := DQuery.DeleteRecord(aCacheItem);
      except
        on E: Exception do
            begin
              result:= False;
              if Length(E.Message)>0 then Errors.Add(TAnyError.Create(GetTitle(E.Message)))
                                     else IsEmptyExcept:=True;
            end;
      end;

      if (not result) and IsEmptyExcept then
        Errors.Add(TAnyError.Create(GetTitle('_Error.Edit')));

      if result then
      begin
        TCacheItem(aCacheItem).Owner.BeginUpdate;
        DoAct('OnDelete', TCacheItem(aCacheItem));
        DoAct('OnChange', TCacheItem(aCacheItem));
        TCacheItem(aCacheItem).Owner.EndUpdate;
        MetaData.UpdateLibrary(Table.ID);
        Table.NField.UpdateControls;
        // Обновляем действия если надо ...
        if assigned(MetaData.MetaTables[idxCommands]) then
          if (Table.ID = MetaData.MetaTables[idxCommands].ID) and Assigned(ProfitActions) then
            ProfitActions.UpdateData(VarToInt(TCacheItem(aCacheItem).ID), mcDelete);
      end;
    end;
  end;
end;

function TDataManager.DeleteRecords(aFilter: TFilterItem): Boolean;
begin
  Errors.Clear;
  if Table.IsReadOnly then
    begin
      Result := false;
      Errors.Add(TAnyError.Create('_De.readonly'))
    end
  else
    begin
      DQuery.Descr.BeginUpdate;
      try
        DQuery.Descr.Clear;
        DQuery.Descr.Table := FTable.Table;
        DQuery.Descr.AssignFilter(aFilter);
      finally
        DQuery.Descr.EndUpdate;
      end;
      try
        DQuery.ExecuteQuery;
        Result := true;
      except
        on E: Exception do
          begin
            result:= False;
            if Length(E.Message)>0 then Errors.Add(TAnyError.Create(GetTitle(E.Message)))
                                   else Errors.Add(TAnyError.Create( GetTitle('_dA.delete')+#10+GetTitle('_dL.procedureerror') ));
          end;
      end;
    end;
end;

{ TMenuDataManager }

constructor TMenuDataManager.Create;
begin
  inherited Create;
  Table := MetaData.MetaTables[idxMenu];
end;

function TMenuDataManager.CheckRecord(aCacheItem: TObject; aContexts: TConditionContexts): Boolean;
begin
  Result := inherited CheckRecord(aCacheItem, aContexts);
end;

procedure TMenuDataManager.SetItem(aItem: TObject; aMenuItem: TContextMeta);
const BoolToInt : array[boolean] of byte = (0, 1);
var   N: Integer;
begin
  TCacheItem(aItem).ValueByName[fldMenuID] := aMenuItem.ID;
  if Assigned(aMenuItem.Owner) then
    TCacheItem(aItem).ValueByName[fldMenuOwner] := aMenuItem.Owner.ID
  else
    TCacheItem(aItem).ValueByName[fldMenuOwner] := 0;
  TCacheItem(aItem).ValueNativeByName[fldMenuName] := aMenuItem.Name;
  TCacheItem(aItem).ValueByName[fldMenuOrder] := aMenuItem.Order;
  TCacheItem(aItem).ValueByName[fldMenuDataset] := aMenuItem.DatasetId;
  TCacheItem(aItem).ValueByName[fldMenuDatasetView] := aMenuItem.ViewType;
  TCacheItem(aItem).ValueByName[fldMenuDatasetPars] := aMenuItem.ViewParams;
  TCacheItem(aItem).ValueByName[fldMenuType] := aMenuItem.ItemType;
  TCacheItem(aItem).ValueByName[fldMenuDeleted] := BoolToInt[aMenuItem.Deleted];

  N:=TCacheItem(aItem).Owner.Fields.IndexByName(fldMenuBreak);
  if -1<N then
    TCacheItem(aItem).FieldValue[N] := False;

  N:=TCacheItem(aItem).Owner.Fields.IndexByName(fldMenuSubjectID);
  if -1<N then
    TCacheItem(aItem).FieldValue[N] := aMenuItem.SubjectID;
end;

procedure TMenuDataManager.SetupNewMeta(aItem: TContextMeta);
var MenuItem  : TCacheItem;
    MenuCache : TDataCache;
begin
  MenuCache := nil;
  try
    MenuCache := TDataCache.Create(Table);
    MenuItem := MenuCache.AddNewItem;
    SetItem(MenuItem, aItem);
    PrepareRecord(MenuItem);
    aItem.Assign(MenuItem);
    aItem.ID := 0;
  finally
    MenuCache.Free;
  end;
end;

function TMenuDataManager.InsertItem(aItem: TContextMeta): Boolean;
var MenuItem  : TCacheItem;
    MenuCache : TDataCache;
begin
  MenuCache := nil;
  try
    MenuCache := TDataCache.Create(Table);
    MenuItem := MenuCache.AddNewItem;
    SetItem(MenuItem, aItem);
    PrepareRecord(MenuItem);
    result := CanInsertRecord(MenuItem) and InsertRecord(MenuItem);
    if result then
    begin
      aItem.ID := VarToInt(MenuItem.ID);
      aItem.Changed := mcNone;
    end;
  finally
    MenuCache.Free;
  end;
end;

function TMenuDataManager.UpdateItem(aItem: TContextMeta): Boolean;
var MenuItem   : TCacheItem;
    MenuCache : TDataCache;
begin
  MenuCache := nil;
  try
    MenuCache := TDataCache.Create(Table);
    MenuItem := MenuCache.AddNewItem;
    SetItem(MenuItem, aItem);
    result := UpdateRecord(MenuItem);
    if result then
      aItem.Changed := mcNone;
  finally
    MenuCache.Free;
  end;
end;

function TMenuDataManager.DeleteItem(aItem: TContextMeta): Boolean;
var MenuItem   : TCacheItem;
    MenuCache : TDataCache;
begin
  MenuCache := nil;
  try
    MenuCache := TDataCache.Create(Table);
    MenuItem := MenuCache.AddNewItem;
    SetItem(MenuItem, aItem);
    result := DeleteRecord(MenuItem);
  finally
    MenuCache.Free;
  end;
end;  

{ TFieldsDataManager }

constructor TFieldsDataManager.Create;
begin
  inherited Create;
  Table := MetaData.MetaTables[idxFields];
end;

function TFieldsDataManager.CheckLinkSolution(aField: TFieldMeta): Boolean;
var LinkTable    : TTableMeta;
    SolutionName : string;
    SolutionItem : TCacheItem;
    Cache        : TDataCache;
begin
  result := true;
  LinkTable := MetaData.GetTableMeta(aField.Link);
  if Assigned(LinkTable) and (aField.Owner.SolutionID <> LinkTable.SolutionID) then
  begin
    result := false;
    Cache := MetaData.GetLibrary(MetaData.MetaTables[idxSolutions].ID);
    SolutionItem := Cache.FindByID(aField.Owner.SolutionID);
    if Assigned(SolutionItem) then
      SolutionName := Cache.FindByID(aField.Owner.SolutionID).Caption
    else
      SolutionName := EmptyStr;
    Errors.Clear;
    Errors.Add(TFieldError.Create(fldFieldsLink,
      Format(GetTitle('_eRror.datasetsolution'), [SolutionName])));
    MetaData.ReleaseCache(Cache);
  end;
end;

function TFieldsDataManager.CheckExpression(aField: TFieldMeta): Boolean;
var Parser  : TDeParser;
    Postfix : TExpressionItem;
begin
  // проверка колец в графе производится при начитывании списка полей
  result := true;
  if not aField.Key then
  begin
    Parser := TDeParser.Create;
    Postfix := TExpressionItem.Create;
    Parser.Table := aField.Owner;
    if Length(aField.DefaultValue) > 0 then
      try
        Parser.Parse(VarToStr(aField.DefaultValue), Postfix);
      except
        on E:EDeParserError do
        begin
          Errors.Clear;
          Errors.Add(TFieldError.Create(fldFieldsDefault, GetTitle(E.Message)));
          result := false;
        end;
      end;
    Postfix.Free;  
    Parser.Free;
  end;
end;

function TFieldsDataManager.CheckRecord(aCacheItem: TObject; aContexts: TConditionContexts): Boolean;
var RC, Roles, I : integer;
    LinkedTable  : TTableMeta;
    Cache        : TDataCache;
    Filter       : TFilterItem;
    Q            : TDeDataset;
    FM           : TFieldMeta;
  function CheckFlag(aFlag : TFieldRoleType;  aData : integer;
    const aErrorMessage : string) : boolean;
  begin
    if aFlag in FM.Role then
    begin
      result := (aData and RoleFlag[aFlag]) = 0;
      if not result then
      begin
        Errors.Clear;
        Errors.Add(TFieldError.Create(fldFieldsRole, aErrorMessage));
      end;
    end
    else
      result := true;
  end;
begin
  result := inherited CheckRecord(aCacheItem, aContexts);
  if (ccInsert in aContexts) or (ccUpdate in aContexts) then
  begin
    FM := TFieldMeta.Create;
    FM.Assign(aCacheItem);

    // ключевое поле может быть только одно
    if result then
    begin
      if FM.Key then
      begin
        Q := nil;
        try
          Q := Table.Database.CreateQuery(qtSelect);
          Q.Descr.BeginUpdate;
          try
            Q.Descr.Table := Table.Table;
            Q.Descr.AddField(opCount);
            Q.Descr.AddParamCondition( fldFieldsTable, Table.Fields.FindByName(fldFieldsTable).DataType, opEQ, EmptyStr, FM.Owner.ID);
            Q.Descr.AddParamCondition( fldFieldsId, Table.Fields.FindByName(fldFieldsId).DataType, opEQ, EmptyStr, FM.ID);
            Q.Descr.AddOperation(opNot);
            Q.Descr.AddOperation(opAnd);
            Q.Descr.AddParamCondition( Table.Fields.FindByName(fldFieldsKey).Original,
                                       Table.Fields.FindByName(fldFieldsKey).DataType, opEQ, EmptyStr, 1);
            Q.Descr.AddOperation(opAnd);
            Q.Descr.AddParamCondition( Table.Fields.FindByName(fldFieldsDeleted).Original,
                                       Table.Fields.FindByName(fldFieldsDeleted).DataType, opEQ, EmptyStr, 1);
            Q.Descr.AddOperation(opNot);
            Q.Descr.AddOperation(opAnd);
          finally
            Q.Descr.EndUpdate;
          end;
          Q.Open;
          RC := Q.Value[0];
          if RC > 0 then
          begin
            result := false;
            Errors.Clear;
            Errors.Add(TFieldError.Create(fldFieldsKey, '_eRror.fld.unique ''_Key'''));
          end;
        finally
          Q.Free;
        end;
      end;
    end;

    // проверяется уникальность флагов, описывающих роль поля
    if result then
    begin
      Cache := nil;
      try
        Cache := TDataCache.Create(Table);
        Filter := TFilterItem.Create;
        Filter.AddCondition(Table.Fields.FindByName(fldFieldsTable), opEQ, VarToInt(FM.Owner.ID));
        Filter.AddCondition(Table.Fields.FindByName(fldFieldsId), opEQ, VarToInt(FM.ID));
        Filter.AddOperation(opNot);
        Filter.AddOperation(opAnd);
        Cache.LoadData(Filter);
        Roles := 0;
        for I := 0 to Cache.Count-1 do
          Roles := Roles or VarToInt(Cache[I].ValueByName[fldFieldsRole]);
      finally
        Cache.Free;
      end;
         // поле имени может быть только одно
      if (not CheckFlag(frNameField, Roles, '_eRror.fld.unique ''_Df.Namefield''')) or
         // признак удаленности может быть только один
         (not CheckFlag(frDeleteSignField, Roles, '_eRror.fld.unique ''_Df.Deleted''')) or
         // признак "по умолчанию" может быть только один
         (not CheckFlag(frDefaultRecordSign, Roles, '_eRror.fld.unique ''_Df.Default''')) or
         // признак папки может быть только один
         (not CheckFlag(frFolderSign, Roles, '_eRror.fld.unique ''_Df.Folder''')) then
        result := false;
    end;

    // для Not Null-полей, не являющихся полем имени, необходимо указать Default-значение
    {
    if result then
      if FM.NotNull and (frNameField in FM.Role) and (not FM.Key) and (FM.DefValue = EmptyStr) then
      begin
        Errors.Clear;
        Errors.Add(TFieldError.Create(fldFieldsDefault, '_Error.fld.needdefault'));
        result := false;
      end;
    }

    // для признаков папки необходимо указать значение 1
    if result then
      if (frFolderSign in FM.Role) and (FM.Value1 = EmptyStr) then
      begin
        Errors.Clear;
        Errors.Add(TFieldError.Create(fldFieldsValue1, '_Error.fld.needvalue1'));
        result := false;
      end;

    // проверяем возможность связывания наборов данных
    if FM.Link > 0 then
    begin
      LinkedTable := MetaData.GetTableMeta(FM.Link);
      if Assigned(LinkedTable) and (FM.DataType <> LinkedTable.KField[0].DataType) and

         (Not ( (FM.DataType in StringTypes) and
                (LinkedTable.KField[0].DataType in StringTypes) )) and

         (Not ( (FM.DataType in IntegerTypes) and
                (LinkedTable.KField[0].DataType in IntegerTypes) )) then
      begin
        result := false;
        Errors.Clear;
        Errors.Add(TFieldError.Create(fldFieldsLink, GetTitle('_dE.fieldincorrectlink')));
      end;
    end;

    result := result and CheckLinkSolution(FM);
    result := result and CheckExpression(FM);
  end;
end;

function TFieldsDataManager.InsertFieldMeta(aField: TFieldMeta): Boolean;
var FieldItem   : TCacheItem;
    FieldsCache : TDataCache;
begin
  FieldsCache := nil;
  try
    FieldsCache := TDataCache.Create(Table);
    FieldItem := FieldsCache.AddNewItem;
    aField.AssignToCacheItem(FieldItem);
    PrepareRecord(FieldItem);
    Result := CanInsertRecord(FieldItem) and InsertRecord(FieldItem);
    aField.ID := VarToInt(FieldItem.ID);
  finally
    FieldsCache.Free;
  end;
end;

function TFieldsDataManager.UpdateFieldMeta(aField: TFieldMeta): Boolean;
var FieldItem   : TCacheItem;
    FieldsCache : TDataCache;
begin
  FieldsCache := nil;
  try
    FieldsCache := TDataCache.Create(Table);
    FieldItem := FieldsCache.AddNewItem;
    aField.AssignToCacheItem(FieldItem);
    Result := UpdateRecord(FieldItem);
  finally
    FieldsCache.Free;
  end;
end;

function TFieldsDataManager.DeleteRecord(aCacheItem: TObject; const aProcessChildren: Boolean): Boolean;
var TableID    : integer;
    FieldTable : TTableMeta;
begin
  TableID := VarToInt(TCacheItem(aCacheItem).ValueByName[fldFieldsTable]);
  FieldTable := MetaData.GetTableMeta(TableID);
  Result := inherited DeleteRecord(aCacheItem, aProcessChildren);
  if Result and
     (UserSession.IsAdmin) and  Assigned(FieldTable) and  (not FieldTable.IsSystem) then
    if Assigned(FieldTable) then
      FieldTable.ClearFields;
end;

function TFieldsDataManager.InsertRecord(aCacheItem: TObject): Boolean;
var TableID    : integer;
    FieldTable : TTableMeta;
begin
  TableID := VarToInt(TCacheItem(aCacheItem).ValueByName[fldFieldsTable]);
  FieldTable := MetaData.GetTableMeta(TableID);
  Result := inherited InsertRecord(aCacheItem);
  if Result and (UserSession.IsAdmin) and Assigned(FieldTable) and (not FieldTable.IsSystem) then
    if Assigned(FieldTable) then
      FieldTable.ClearFields;
end;

function TFieldsDataManager.UpdateRecord(aCacheItem: TObject): Boolean;
var SourceTableID, DestTableID       : integer;
    SourceFieldTable, DestFieldTable : TTableMeta;
begin
  SourceTableID := VarToInt(TCacheItem(aCacheItem).BaseValueByName[fldFieldsTable]);
  DestTableID := VarToInt(TCacheItem(aCacheItem).ValueByName[fldFieldsTable]);
  result := inherited UpdateRecord(aCacheItem);
  if result and (UserSession.IsAdmin) then
  begin
    SourceFieldTable := MetaData.GetTableMeta(SourceTableID);
    DestFieldTable := MetaData.GetTableMeta(DestTableID);
    if Assigned(SourceFieldTable) and (not SourceFieldTable.IsSystem) and
       (Not (SourceFieldTable.Database = MetaData.MetadataDB)) then
      SourceFieldTable.ClearFields;
    if Assigned(DestFieldTable) and (not DestFieldTable.IsSystem) and
       (Not (SourceFieldTable.Database = MetaData.MetadataDB)) then
      DestFieldTable.ClearFields;
  end;
end;

{ TElementsDataManager }

constructor TElementsDataManager.Create;
begin
  inherited Create;
  Table := MetaData.MetaTables[idxElement];
end;

function TElementsDataManager.CheckLinkSolution(aCacheItem: TObject): Boolean;
var TableIndex, LinkIndex : integer;
    ElemTable, LinkTable  : TTableMeta;
    SolutionName          : string;
    SolutionItem          : TCacheItem;
    Cache                 : TDataCache;
begin
  result := true;
  TableIndex := TCacheItem(aCacheItem).Owner.Fields.IndexByName(fldElemDataset);
  LinkIndex := TCacheItem(aCacheItem).Owner.Fields.IndexByName(fldElemLink);
  if (TableIndex > 0) and (LinkIndex > 0) then
  begin
    ElemTable := MetaData.GetTableMeta(
      VarToInt(TCacheItem(aCacheItem).FieldValue[TableIndex]));
    LinkTable := MetaData.GetTableMeta(
      VarToInt(TCacheItem(aCacheItem).FieldValue[LinkIndex]));
    if Assigned(ElemTable) and Assigned(LinkTable) and
      (ElemTable.SolutionID > 0) and (LinkTable.SolutionID > 0) and
      (ElemTable.SolutionID <> LinkTable.SolutionID) and
      (not LinkTable.IsSystem) then
    begin
      result := false;
      Cache := MetaData.GetLibrary(MetaData.MetaTables[idxSolutions].ID);
      SolutionItem := Cache.FindByID(ElemTable.SolutionID);
      if Assigned(SolutionItem) then
        SolutionName := Cache.FindByID(ElemTable.SolutionID).Caption
      else
        SolutionName := EmptyStr;
      Errors.Clear;
      Errors.Add(TFieldError.Create(fldElemLink,
        Format(GetTitle('_eRror.datasetsolution'), [SolutionName])));
      MetaData.ReleaseCache(Cache);  
    end;
  end;
end;

function TElementsDataManager.CheckRecord(aCacheItem: TObject; aContexts: TConditionContexts): Boolean;
var tmpElem, Elem, EditingElem : TElementMeta;
    FieldIndex, Index, I  : integer;
    LinkedTable : TTableMeta;
    Cache : TDataCache;
    Field : TFieldMeta;
    Filter : TFilterItem;
begin
  result := inherited CheckRecord(aCacheItem, aContexts);
  if result and ((ccInsert in aContexts) or (ccUpdate in aContexts)) then
    begin
      FieldIndex := TCacheItem(aCacheItem).Owner.Fields.IndexByName(fldElemDataSet);
      tmpElem := TElementMeta.Create(MetaData.GetTableMeta(TCacheItem(aCacheItem).FieldValue[FieldIndex]));
      tmpElem.Assign(aCacheItem);
      if Assigned(tmpElem.Table) then
        begin
          // проверяем соответствие типа поля типу элемента
          if (((tmpElem.ElementType = etCheckBox) or (tmpElem.ElementType = etTreeBox)) and
              ((not Assigned(tmpElem.Field)) or (tmpElem.Field.DataType in BinaryTypes)))
             or
             ((tmpElem.ElementType = etBarCode) and
             ((not Assigned(tmpElem.Field)) or Not (tmpElem.Field.DataType in StringTypes)))
             or
             ((tmpElem.ElementType = etExplorer) and
             ((not Assigned(tmpElem.Field)) or Not (tmpElem.Field.DataType in StringTypes)))
             or
             ((tmpElem.ElementType = etColorComboBox) and
             ((not Assigned(tmpElem.Field)) or (tmpElem.Field.DataType <> ftInteger)))
             or
             (((tmpElem.ElementType = etIconBox) or (tmpElem.ElementType = etCheckListBox)) and
              ((not Assigned(tmpElem.Field)) or (not (tmpElem.Field.DataType in [ftInteger, ftSmallInt]))))
             or
             (((tmpElem.ElementType = etFieldsBox) or (tmpElem.ElementType = etTablesBox)) and
              ((not Assigned(tmpElem.Field)) or Not (tmpElem.Field.DataType in StringTypes))) then
                  begin
                    result := false;
                    Errors.Clear;
                    Errors.Add(TFieldError.Create(fldElemType, '_Error.elem.type'));
                  end;
          // нельзя указывать связь для BLOB-поля
          if result and Assigned(tmpElem.Field) and
             (tmpElem.Field.DataType in BinaryTypes) and (tmpElem.Link > 0) then
            begin
              result := false;
              Errors.Clear;
              Errors.Add(TFieldError.Create(fldElemLink, '_Error.elem.linkpresent'));
            end;
          // проверяем возможность связывания
          if result
             and (tmpElem.Link > 0)
             and (not (tmpElem.ElementType in [etGrid, etListTabSheet]))
             and Assigned(tmpElem.Field) then
            begin
              LinkedTable := MetaData.GetTableMeta(tmpElem.Link);
              if Assigned(LinkedTable) and Assigned(LinkedTable.KField) then
                if not
                  (
                   ((tmpElem.ElementType <> etCheckListBox)
                   and
                   (((tmpElem.Field.DataType = LinkedTable.KField[0].DataType) and
                     (tmpElem.Field.DataSize = LinkedTable.KField[0].DataSize))
                     or
                     ((tmpElem.Field.DataType in IntegerTypes) and
                      (LinkedTable.KField[0].DataType in IntegerTypes))))
                   or
                   ((tmpElem.ElementType = etCheckListBox) and
                    (tmpElem.Field.DataType in IntegerTypes) and
                    (LinkedTable.KField[0].DataType in IntegerTypes))
                  )
                       then
                  begin
                    result := false;
                    Errors.Clear;
                    Errors.Add(TFieldError.Create(fldElemLink, '_Error.elem.incorrectlink'));
                  end;
            end;

          // проверяем, принадлежит ли связанный набор данных тому же решению
          result := result and CheckLinkSolution(aCacheItem);
          // проверяем иерархическую структуру элементов формы
          if result then
          begin
            Elem := LoadElements(tmpElem.Table);
            EditingElem := TElementMeta(Elem.FindNode(tmpElem.ID));
            if Assigned(EditingElem) then
            begin
              // запись не удалена, пока ее редактировали
              EditingElem.Assign(tmpElem);
              Errors.Clear;
              Elem.MakeTree(Errors);
              for I := 0  to Errors.Count-1 do
              begin
                if Errors[I] is TUnknownOwnerError then
                begin
                  // обнаружен элемент с неизвестным Owner'ом
                  result := false;
                  Errors.Clear;
                  Errors.Add(TFieldError.Create(fldElemOwner, '_dE.owner "'+ String(tmpElem.OwnerID)+'"'));
                end
                else if Errors[I] is TSelfRingError then
                begin
                  // элемент является собственным владельцем
                  Errors.Clear;
                  result := false;
                  Errors.Add(TFieldError.Create(fldElemOwner, '_dE.owner "'+ String(tmpElem.OwnerID)+'"'));

                end
                else if Errors[I] is TRingError then
                begin
                  // в иерархии обнаружено кольцо
                  result := false;
                  Errors.Clear;
                  Errors.Add(TFieldError.Create(fldElemOwner, '_dE.owner "'+ String(tmpElem.OwnerID)+'"'));
                end;
                if not result then break;
              end;
              if result then
              begin
                // владельцем элемента может быть только форма
                if result and (tmpElem.OwnerId > 0) then
                begin
                  Cache := nil;
                  try
                    Cache := TDataCache.Create(MetaData.MetaTables[idxElement]);
                    Filter := TFilterItem.Create;
                    Field := Cache.TableMeta.Fields.FindByName(fldElemId);
                    Filter.AddCondition(Field, opEQ, tmpElem.OwnerId);
                    Field := Cache.TableMeta.Fields.FindByName(fldElemDataset);
                    Filter.AddCondition(Field, opEQ, tmpElem.Table.ID);
                    Filter.AddOperation(opAnd);
                    Cache.LoadData(Filter);
                    Index := Cache.TableMeta.Fields.IndexByName(fldElemType);
                    if Cache.Count = 0 then
                    begin
                      result := false;
                      Errors.Clear;
                      Errors.Add(TFieldError.Create(fldElemOwner, '_Error.elem.unknownowner'));
                    end
                    else
                      if not(
                        ((TElementType(VarToInt(Cache[0].FieldValue[Index])) = etForm)
                         and (tmpElem.ElementType in [etTabSheet, etListTabSheet]))
                        or
                        ((TElementType(VarToInt(Cache[0].FieldValue[Index])) in [etPanel, etTabSheet])
                         and (not (tmpElem.ElementType in [etTabSheet, etListTabSheet, etForm])))) then
                      begin
                        result := false;
                        Errors.Clear;
                        Errors.Add(TFieldError.Create(fldElemOwner, '_Error.elem.owneriscontainer'));
                      end;
                  finally
                    Cache.Free;
                  end;
                end;
              end;
              Elem.Free;
            end;
          end;
        end;
    end;
end;

function TElementsDataManager.LoadElements(const aTable: TTableMeta): TElementMeta;
var R: Integer;
    Q: TDeDataset;
    EMeta : TElementMeta;
begin
  Result := TElementMeta.Create(aTable);
  Q := nil;
  try
    Q := MetaData.MetadataDB.CreateQuery(qtSelect);
    Q.Descr.BeginUpdate;
    try
      Q.Descr.Table := tblElement;
      Q.Descr.AddParamCondition(fldElemDeleted, ftSmallint, opNE, 'DID', 1);
      //Q.Descr.AddOperation(opNot);
      Q.Descr.AddParamCondition(fldElemDataset, ftInteger, opEQ, 'ID', aTable.ID);
      Q.Descr.AddOperation(opAnd);
      Q.Descr.AddSortField(fldElemT);
      // 27.08.2015 + Куфенко: Читаем только необходимые поля!
      Q.Descr.AddFields([fldElemId, fldElemType, fldElemLink, fldElemLinkField, fldElemOwner, fldElemName, fldElemField,
        fldElemFont, fldElemColor, fldElemVisible, fldElemReadOnly, fldElemAL, fldElemAR, fldElemL, fldElemR, fldElemT,
        fldElemH, fldElemExprName, fldElemExprFilter, fldElemExprVisible, fldElemExprReadOnly, fldElemExprValue
        {fldElemDeleted,}]);
    // 27.08.2015 -
    finally
      Q.Descr.EndUpdate;
    end;
    Q.Open;
    for R:=0 to Pred(Q.RecordCount) do
      begin
        Q.RecNo:= R;
        EMeta:=TElementMeta.Create(aTable, result);
        EMeta.Assign(Q);
      end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

procedure TElementsDataManager.SetElement(aItem: TObject; aElement: TElementMeta);
var FieldIndex : integer;
begin
  TCacheItem(aItem).ValueByName[fldElemId]   := aElement.ID;
  TCacheItem(aItem).ValueByName[fldElemType] := ord(aElement.ElementType);
  TCacheItem(aItem).ValueByName[fldElemLink] := aElement.Link;
  TCacheItem(aItem).ValueNativeByName[fldElemName] := aElement.Name;
  if Assigned(aElement.Field) then TCacheItem(aItem).ValueByName[fldElemField] := aElement.Field.ID
                              else TCacheItem(aItem).ValueByName[fldElemField] := 0;
  TCacheItem(aItem).ValueByName[fldElemFont] := aElement.FontCode;
  TCacheItem(aItem).ValueByName[fldElemAL]   := aElement.EAL;
  TCacheItem(aItem).ValueByName[fldElemAR]   := aElement.EAR;
  TCacheItem(aItem).ValueByName[fldElemL]    := aElement.EL;
  TCacheItem(aItem).ValueByName[fldElemR]    := aElement.ER;
  TCacheItem(aItem).ValueByName[fldElemT]    := aElement.ET;
  TCacheItem(aItem).ValueByName[fldElemH]    := aElement.EH;
  if Assigned(aElement.Owner) then TCacheItem(aItem).ValueByName[fldElemOwner] := aElement.Owner.Id
                              else TCacheItem(aItem).ValueByName[fldElemOwner] := aElement.OwnerId;
  TCacheItem(aItem).ValueByName[fldElemDataSet] := aElement.Table.ID;
  TCacheItem(aItem).ValueByName[fldElemColor]   := aElement.Color;
  TCacheItem(aItem).ValueByName[fldElemDeleted] := aElement.Deleted;
  FieldIndex := TCacheItem(aItem).Owner.Fields.IndexByName(fldElemLinkField);
  if FieldIndex >= 0 then
    if Assigned(aElement.LinkField) then TCacheItem(aItem).FieldValue[FieldIndex] := aElement.LinkField.ID
                                    else TCacheItem(aItem).FieldValue[FieldIndex] := 0;
  TCacheItem(aItem).ValueByName[fldElemVisible] := BooleanToInt[aElement.IsVisible];
  TCacheItem(aItem).ValueByName[fldElemReadOnly] := BooleanToInt[aElement.IsReadOnly];
  TCacheItem(aItem).ValueByName[fldElemExprName] := aElement.NameInExpr;
  TCacheItem(aItem).ValueByName[fldElemExprFilter] := aElement.ExprFilter;
  TCacheItem(aItem).ValueByName[fldElemExprVisible] := aElement.ExprVisible;
  TCacheItem(aItem).ValueByName[fldElemExprReadOnly] := aElement.ExprReadOnly;
  TCacheItem(aItem).ValueByName[fldElemExprValue] := aElement.ExprValue;
end;

function TElementsDataManager.InsertElement(aElement: TElementMeta): Boolean;
var ElementItem   : TCacheItem;
    ElementsCache : TDataCache;
begin
  ElementsCache := nil;
  try
    ElementsCache := TDataCache.Create(Table);
    ElementItem := ElementsCache.AddNewItem;
    SetElement(ElementItem, aElement);
    PrepareRecord(ElementItem);
    Result := CanInsertRecord(ElementItem) and InsertRecord(ElementItem);
    if Result then
    begin
      aElement.ID := VarToInt(ElementItem.ID);
      aElement.Changes := [];
    end;  
  finally
    ElementsCache.Free;
  end;
end;

function TElementsDataManager.UpdateElement(aElement: TElementMeta): Boolean;
var ElementItem   : TCacheItem;
    ElementsCache : TDataCache;
begin
  ElementsCache := nil;
  try
    ElementsCache := TDataCache.Create(Table);
    ElementItem := ElementsCache.AddNewItem;
    SetElement(ElementItem, aElement);
    Result := UpdateRecord(ElementItem);
    if Result then
      aElement.Changes := [];
  finally
    ElementsCache.Free;
  end;
end;

function TElementsDataManager.DeleteElement(aElement: TElementMeta): Boolean;
var ElementItem   : TCacheItem;
    ElementsCache : TDataCache;
begin
  ElementsCache := nil;
  try
    ElementsCache := TDataCache.Create(Table);
    ElementItem := ElementsCache.AddNewItem;
    SetElement(ElementItem, aElement);
    result := DeleteRecord(ElementItem);
  finally
    ElementsCache.Free;
  end;
end;

function TElementsDataManager.DeleteRecord(aCacheItem: TObject; const aProcessChildren: Boolean): Boolean;
var TableID   : integer;
    ElemTable : TTableMeta;
begin
  TableID := VarToInt(TCacheItem(aCacheItem).ValueByName[fldElemDataset]);
  Result := inherited DeleteRecord(aCacheItem, aProcessChildren);
  if Result and (UserSession.IsAdmin) then
  begin
    ElemTable := MetaData.GetTableMeta(TableID);
    if Assigned(ElemTable) and (not ElemTable.IsSystem) then
      ElemTable.ClearElements(False);
  end;
end;

function TElementsDataManager.InsertRecord(aCacheItem: TObject): Boolean;
var TableID   : integer;
    ElemTable : TTableMeta;
begin
  TableID := VarToInt(TCacheItem(aCacheItem).ValueByName[fldElemDataset]);
  Result := inherited InsertRecord(aCacheItem);
  if Result and (UserSession.IsAdmin) then
  begin
    ElemTable := MetaData.GetTableMeta(TableID);
    if Assigned(ElemTable) and (not ElemTable.IsSystem) then
      ElemTable.ClearElements(False);
  end;
end;

function TElementsDataManager.UpdateRecord(aCacheItem: TObject): Boolean;
var SourceTableID, DestTableID     : integer;
    ElemTable : TTableMeta;
begin
  SourceTableID := VarToInt(TCacheItem(aCacheItem).BaseValueByName[fldElemDataset]);
  DestTableID := VarToInt(TCacheItem(aCacheItem).ValueByName[fldElemDataset]);

  Result := inherited UpdateRecord(aCacheItem);

  if Result then
    begin
      ElemTable := MetaData.GetTableMeta(SourceTableID);
      if Assigned(ElemTable) then ElemTable.ClearElements(False);

      ElemTable := MetaData.GetTableMeta(DestTableID);
      if Assigned(ElemTable)   then ElemTable.ClearElements(False);
    end;
end;

{ TDatabaseDataManager }

constructor TDatabaseDataManager.Create;
begin
  inherited Create;
  Table := MetaData.MetaTables[idxBase];
end;

function TDatabaseDataManager.DeleteRecord(aCacheItem: TObject; const aProcessChildren: Boolean): Boolean;
var CI: TCacheItem;
    DBItem : TDeCustomDatabase;
begin
  CI:= TCacheItem(aCacheItem);
  DBItem:= MetaData.DatabaseByID(CI.ID);

  // редактировать мета-базу можно только в конфигураторе при подключении
  if DBItem.VariantGUID = MetadataDatabaseGUID then Exit(False);

  // редактировать хардкодные БД нельзя
  if DBItem.VariantGUID = CatalogsDatabaseGUID then Exit(False);
  if DBItem.VariantGUID = LanguageDatabaseGUID then Exit(False);
  if DBItem.VariantGUID = SampleMTDatabaseGUID then Exit(False);

  Result:= UserSession.IsAdmin and inherited DeleteRecord(aCacheItem, aProcessChildren);
  if Result then
    begin
      Metadata.NeedReloading:= True;
    end;
end;

function TDatabaseDataManager.InsertRecord(aCacheItem: TObject): Boolean;
begin
  Result := UserSession.IsAdmin and inherited InsertRecord(aCacheItem);
  if Result then
    begin
      MetaData.CreateDatabase(TCacheItem(aCacheItem), True);
    end;
end;

function TDatabaseDataManager.UpdateRecord(aCacheItem: TObject): Boolean;
var CI: TCacheItem;
    DBItem : TDeCustomDatabase;
    V: Variant;
begin
  CI:= TCacheItem(aCacheItem);
  DBItem:= MetaData.DatabaseByID(CI.ID);

  // редактировать мета-базу можно только в конфигураторе при подключении
  if DBItem.VariantGUID = MetadataDatabaseGUID then Exit(False);

  // редактировать мета-базу можно только в конфигураторе при подключении
  if DBItem.VariantGUID = CatalogsDatabaseGUID then Exit(False);

  Result := UserSession.IsAdmin and inherited UpdateRecord(aCacheItem);

  if Result then
    begin
      if CI.ValueChanged(fldBaseType, V) or
         CI.ValueChanged(fldBaseConnectStr, V) or
         CI.ValueChanged(fldBaseLogin, V) or
         CI.ValueChanged(fldBasePassword, V) or
         CI.ValueChanged(fldBaseDeleted, V) or
         CI.ValueChanged(fldBaseServer, V) or
         CI.ValueChanged(fldBaseDatabase, V) or
         CI.ValueChanged(fldBaseCharset, V) then Metadata.NeedReloading:= True;

      if CI.ValueChanged(fldBaseAlias,V)        then DBItem.Alias:= V;
    end;
end;

{ TDatasetDataManager }

function TDatasetDataManager.CheckBasedOnSolution(aCacheItem: TObject): Boolean;
var TableIndex, BasedOnIndex : integer;
    SelfTable, BasedOnTable  : TTableMeta;
//    SolutionName             : string;
//    SolutionItem             : TCacheItem;
//    Cache                    : TDataCache;
begin
  Result := true;
  TableIndex := TCacheItem(aCacheItem).Owner.Fields.IndexByName(fldDataSetID);
  BasedOnIndex := TCacheItem(aCacheItem).Owner.Fields.IndexByName(fldDataSetParent);
  if (TableIndex > 0) and (BasedOnIndex > 0) then
  begin
    SelfTable := TTableMeta.Create;
    SelfTable.Assign(aCacheItem);
    BasedOnTable := MetaData.GetTableMeta(
      VarToInt(TCacheItem(aCacheItem).FieldValue[BasedOnIndex]));
    {
    if Assigned(SelfTable) and Assigned(BasedOnTable) and
      (SelfTable.SolutionID > 0) and (BasedOnTable.SolutionID > 0) and
      ((SelfTable.SolutionID <> BasedOnTable.SolutionID)) then
    begin
      Result := false;
      Cache := MetaData.GetLibrary(MetaData.MetaTables[idxSolutions].ID);
      SolutionItem := Cache.FindByID(SelfTable.SolutionID);
      if Assigned(SolutionItem) then
        SolutionName := SolutionItem.Caption
      else
        SolutionName := EmptyStr;
      Errors.Clear;
      Errors.Add(TFieldError.Create(fldDataSetParent,
        Format(GetTitle('_eRror.datasetsolution'), [SolutionName])));
      MetaData.ReleaseCache(Cache);
    end;
    {}
    SelfTable.Free;
  end;
end;

function TDatasetDataManager.CheckRecord(aCacheItem: TObject; aContexts: TConditionContexts): Boolean;
begin
  Result := inherited CheckRecord(aCacheItem, aContexts);
  Result := Result and CheckbasedOnSolution(aCacheItem);
end;

constructor TDatasetDataManager.Create;
begin
  inherited Create;
  Table := MetaData.MetaTables[idxDataset];
end;

function TDatasetDataManager.DeleteRecord(aCacheItem: TObject; const aProcessChildren: Boolean): Boolean;
begin
  Result := UserSession.IsAdmin and inherited DeleteRecord(aCacheItem, aProcessChildren);
  if Result then
    begin
      Metadata.NeedReloading:= True;
    end;
end;

function TDatasetDataManager.UpdateTableMeta(aTable : TTableMeta) : boolean;
var TableItem   : TCacheItem;
    TablesCache : TDataCache;
begin
  TablesCache := nil;
  try
    TablesCache := TDataCache.Create(Table);
    TableItem := TablesCache.AddNewItem;
    aTable.AssignToCacheItem(TableItem);
    result := UpdateRecord(TableItem);
  finally
    TablesCache.Free;
  end;
end;

function TDatasetDataManager.InsertRecord(aCacheItem: TObject): boolean;
var TableItem : TTableMeta;
begin
  result:= UserSession.IsAdmin and inherited InsertRecord(aCacheItem);
  if result then
    begin
      TableItem := TTableMeta.Create;
      MetaData.AddTable(TableItem);
      TableItem.Assign(aCacheItem);
      TableItem.DefineTableLinks;
    end;
end;

function TDatasetDataManager.UpdateRecord(aCacheItem: TObject): boolean;
var CI: TCacheItem;
    TableItem: TTableMeta;
    V: Variant;
begin
  result := UserSession.IsAdmin and inherited UpdateRecord(aCacheItem);

  if result then
    begin
      CI:= TCacheItem(aCacheItem);
      TableItem:= MetaData.GetTableMeta(CI.ID);

      if CI.ValueChanged(fldDataSetTable, V) or
         CI.ValueChanged(fldDataSetSelect, V) or
         CI.ValueChanged(fldDataSetDatabase, V) or
         CI.ValueChanged(fldDataSetIsList, V) or
         CI.ValueChanged(fldDataSetDeleted, V) or
         CI.ValueChanged(fldDatasetParent, V) or
         CI.ValueChanged(fldDataSetSolution, V) then Metadata.NeedReloading:= True;

      if CI.ValueChanged(fldDataSetICO,V)       then TableItem.Ico:= V;
      if CI.ValueChanged(fldDataSetName,V)      then TableItem.Name:= V;
    //  if CI.ValueChanged(fldDataSetReadOnly,V)  then TableItem.IsReadOnly:= V;
      if CI.ValueChanged(fldDataSetGViewType,V) then TableItem.GroupViewType:= V;
      if CI.ValueChanged(fldDataSetGViewPars,V) then TableItem.GroupViewParams:= V;
      if CI.ValueChanged(fldDataSetFilter,V)    then TableItem.Filter:= V;
    end;
end;

{ TUsersDataManager }

function TUsersDataManager.CheckRecord(aCacheItem: TObject;
  aContexts: TConditionContexts): boolean;
var Q : TDeDataSet;
    Fld : TFieldMeta;
begin
  result := inherited CheckRecord(aCacheItem, aContexts);
  if result and (ccInsert in aContexts) or (ccUpdate in aContexts) then
  begin
    Fld := MetaData.MetaTables[idxUsers].Fields.FindByName(fldUsersType);
    if Assigned(Fld) then
    begin
      // проверяем возможность назначения признаков "Пользователь" и "Группа"
      if (VarToInt(TCacheItem(aCacheItem).ValueByName[fldUsersType]) > 0)
        and Assigned(MetaData.MetaTables[idxMembership]) then
          begin
            Q := MetaData.MetaTables[idxMembership].Database.CreateQuery(qtSelect);
            try
              Q.Descr.BeginUpdate;
              try
                Q.Descr.Table := tblMembership;
                Q.Descr.AddField(opCount);
                if VarSameValue(TCacheItem(aCacheItem).ValueByName[fldUsersType], Fld.Value2) then
                  Q.Descr.AddParamCondition(fldMSGroupID, ftInteger, opEQ, 'ID', TCacheItem(aCacheItem).ID)
                else
                  Q.Descr.AddParamCondition(fldMSMemberID, ftInteger, opEQ, 'ID', TCacheItem(aCacheItem).ID);
              finally
                Q.Descr.EndUpdate;
              end;
              Q.Open;
              if Q.Value[0] > 0 then
              begin
                result := false;
                Errors.Clear;
                Errors.Add(TFieldError.Create(fldUsersType, GetTitle('_eRror.invalidusertype')));
              end;
            finally
              Q.Free; // 21.09.2015 Утечка памяти! Данная Free ОТСУТСТВОВАЛА!!!
            end;
          end;
    end;
  end;
end;

constructor TUsersDataManager.Create;
begin
  inherited Create;
  Table := MetaData.MetaTables[idxUsers];
end;

{ TMembershipDataManager }

function TMembershipDataManager.CheckRecord(aCacheItem: TObject; aContexts: TConditionContexts): boolean;
var Q : TDeDataset;
begin
  result := inherited CheckRecord(aCacheItem, aContexts);
  if result and (ccInsert in aContexts) or (ccUpdate in aContexts) then
    if (VarToInt(TCacheItem(aCacheItem).ValueByName[fldMSGroupID]) > 0)
      and (VarToInt(TCacheItem(aCacheItem).ValueByName[fldMSMemberID]) > 0) then
        begin
          Q := MetaData.MetaTables[idxMembership].Database.CreateQuery(qtSelect);
          try
            Q.Descr.BeginUpdate;
            try
              Q.Descr.Table := tblMembership;
              Q.Descr.AddField(opCount);
              Q.Descr.AddParamCondition(fldMSGroupID, ftInteger, opEQ, 'GID',
                TCacheItem(aCacheItem).ValueByName[fldMSGroupID]);
              Q.Descr.AddParamCondition(fldMSMemberID, ftInteger, opEQ, 'MID',
                TCacheItem(aCacheItem).ValueByName[fldMSMemberID]);
              Q.Descr.AddOperation(opAnd);
              Q.Descr.AddParamCondition(fldMSID, ftInteger, opEQ, 'ID', TCacheItem(aCacheItem).ID);
              Q.Descr.AddOperation(opNot);
              Q.Descr.AddOperation(opAnd);
            finally
              Q.Descr.EndUpdate;
            end;
            Q.Open;
            if Q.Value[0] > 0 then
            begin
              result := false;
              Errors.Clear;
              Errors.Add(TFieldError.Create(fldMSGroupID, GetTitle('_eRror.uniquemembership')));
            end;
          finally
            Q.Free; // 21.09.2015 Утечка памяти! Данная Free ОТСУТСТВОВАЛА!!!
          end;
        end;
end;

constructor TMembershipDataManager.Create;
begin
  inherited Create;
  Table := MetaData.MetaTables[idxMembership];
end;

{ TConstraintDataManager }

function TConstraintDataManager.CheckRecord(aCacheItem: TObject; aContexts: TConditionContexts): Boolean;
var Parser : TDeParser;
    StrExpression : string;
    aTableID : integer;
    Postfix : TExpressionItem;
begin
  Result := inherited CheckRecord(aCacheItem, aContexts);
  if Result and ((ccInsert in aContexts) or (ccUpdate in aContexts)) then
  begin
    if mpDatabaseNewConstraints in MetaData.MetadataPresents then
      begin
        StrExpression := VarToStr(TCacheItem(aCacheItem).ValueByName[fldConstraintsExpression]);
        aTableID := VarToInt(TCacheItem(aCacheItem).ValueByName[fldConstraintsDataSet]);
      end
    else
      begin
        StrExpression := VarToStr(TCacheItem(aCacheItem).ValueByName[fldCSExpression]);
        aTableID := VarToInt(TCacheItem(aCacheItem).ValueByName[fldCSDataSet]);
      end;
    if Length(StrExpression) > 0 then
    begin
      Parser := TDeParser.Create;
      Postfix := TExpressionItem.Create;
      Parser.Table := MetaData.GetTableMeta(aTableID);
      try
        Parser.Parse(StrExpression, Postfix);
      except
        on E:EDeParserError do
        begin
          Errors.Clear;
          if mpDatabaseNewConstraints in MetaData.MetadataPresents then
            Errors.Add(TFieldError.Create(fldConstraintsExpression, GetTitle(E.Message)))
          else
            Errors.Add(TFieldError.Create(fldCSExpression, GetTitle(E.Message)));
          Result := false;
        end;
      end;
      Postfix.Free;
      Parser.Free;
    end;
  end;
end;

constructor TConstraintDataManager.Create;
begin
  inherited Create;
  Table := MetaData.MetaTables[idxConstraints];
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DataManager unit initialization ...');

finalization
  DebugLog('DataManager unit finalization ...');
{$ENDIF}

end.

