unit QueryDescriptor;

interface

uses
  Classes, DB,
  DeTypes, DeParser, DeCalculator;

const
  NullIndex = -32768;

type
  /// <summary>Тип запроса данных</summary>
  TDeQueryType = (
    qtSelect,             // выборка любого количества строк данных
    qtRow,                // выборка одной строки данных (аналог qtSelect для одной записи)
    qtInsert,             // добавление даннных
    qtUpdate,             // изменение данных
    qtDelete,             // удаление данных
    qtHole                // Поиск "дырки", v. 17.3
  );
  TDeQueryTypes = set of TDeQueryType;

  /// <summary>Тип операцими с Sequence</summary>
  TDeSequenceType = (
    stInsert,             // создание c 1
    stUpdate,             // установка очередного значения
    stDelete,             // удаление
    stHaving              // проверка существования
  );

  TParamEvent = procedure (aParam : TVariableItem) of object;

  TFieldData = record
  private
    FSourceName: string;   // Реальное имя поля в БД, v. 16.03 (Field дополняется префиксом таблицы для JOIN запросов)
    FSourceType: TFieldType;
    FSourceSize: Integer;
    FSourceOperation: TOperationType;
    FSourceIndex: Integer;
    function GetSourceName: string;
    function GetSourceOperation: TOperationType;
    function GetSourceSize: Integer;
    function GetSourceType: TFieldType;
  public
    TargetName: string;
    TargetType: TFieldType;
    TargetSize: Integer;  // Длина строкового поля, v. 16.12
    TargetIndex: Integer;  // Маппинг массива полей к потребителю, использует TCacheItem, когда нужны не все поля
    TargetOperation: TOperationType;

    Parameter: string;   // TVariableItem;
    Value: Variant;

    Source: string;   // Псевдоним таблицы, v. 16.03

    property SourceName: string read GetSourceName write FSourceName;
    property SourceType: TFieldType read GetSourceType write FSourceType;
    property SourceSize: Integer read GetSourceSize write FSourceSize;
    property SourceOperation: TOperationType read GetSourceOperation write FSourceOperation;
    property SourceIndex: Integer read FSourceIndex write FSourceIndex;
    function SourceAlias: string;
  end;

  TFieldsArray = array of TFieldData;

  /// <summary>Режим построений JOIN запросов:
  /// <para><c>lmReady</c> - допустимо использовать JOIN в одной базе данных</para>
  /// <para><c>lmUnknown</c> - связь не определена и недопустимо строить JOIN</para>
  /// <para><c>lmInvalidKey</c> - недопустимо строить JOIN, т.к. не определен ключ для связи</para>
  /// <para><c>lmDifferentServers</c> - недопустимо использовать JOIN на разных серверах баз данных</para>
  /// <para><c>lmDifferentDatabases</c> - допустимо/недопустимо использовать JOIN на разных базах данных</para>
  /// </summary>
  TLinkJoinMode = (lmReady, lmUnknown, lmInvalidKey, lmDifferentServers, lmDifferentDatabases);

  TLinkQueryDescrEvent = procedure(Sender: TObject; const aDataSet: TObject; const FieldName: string;
                       var LinkMode: TLinkJoinMode; var LinkDatasetName: TObject; var LinkFieldName: string) of object;

  TCustomQueryDescr = class;

  /// <summary>Класс-помощник построений JOIN запросов</summary>
  TJoinBuilder = class
  private
    FDescr: TCustomQueryDescr;
    FNodes: TStrings;
    FLinks: TStrings;
    FSQL: string;
    FSequenceID: Integer;
    FAllwaysJOIN: Boolean;
    function PrepareSequenceID(const SequenceID: Integer): string;
    procedure PrepareSelectFields;
    procedure PrepareSortFields;
    procedure PrepareFilterFields;
    procedure FinalizeFields;
  public
    FirstDataSet: TObject;
    constructor Create(ADescr: TCustomQueryDescr);
    destructor Destroy; override;
    property SQL: string read FSQL;
    property Descr: TCustomQueryDescr read FDescr;
  end;

  TQueryDescrUpdate = (duChanged, duFinished);
  TQueryDescrUpdateSet = set of TQueryDescrUpdate;

  /// <summary>Абстрактный класс-предок всех описателей запроса</summary>
  TCustomQueryDescr = class
  private
    FRecords         : TObject; // указатель на TCacheItem или TCacheItemList или nil для массовых insert/delete
    FTableMeta       : TObject;
    FTableName       : string;
    FSchema          : string; // v.19.08
    FSQL             : string;
    FParams          : TVariableList;
    FFilter          : TExpressionItem;
    FFields          : TFieldsArray;
    FSortFields      : TStringList;
    FOnChanged       : TNotifyEvent;
    FOnFinished      : TNotifyEvent;
    FOnSetParamValue : TParamEvent;
    FOnLinkField: TLinkQueryDescrEvent;
    FUpdateCount: Integer;
    FUpdateSet: TQueryDescrUpdateSet;
    FCalc : TDeSQLCalculator;
    FXML: string;
    FStaticSQL: Boolean;
    procedure SetTable(const aTable: string);
    function  GetTable: string;
    procedure SetTableMeta(const Value: TObject);
    function GetSQL: string;
    procedure SetSQL(const aSQL: string);
    function GetParamValueByName(const aParamName: string): Variant;
    procedure SetParamValueByName(const aParamName: string; const Value: Variant);
    function AddFieldData: Integer;
    function GetSortCount: Integer;
    function GetSortField(const Index: Integer): string;
    function GetSortDirection(const Index: Integer): TDSSortDirection;
    function GetFieldCount: Integer;
    function GetXML: string;
    function PrepareXML: string;
    function PrepareFieldsXML: string;
    function PrepareFieldXML(FieldData: TFieldData): string;
    function PrepareVariantXML(const Value: Variant; const SpaceLevel: Integer = 0): string;
    function PrepareSortFieldsXML: string;
    function PrepareParamsXML: string;
    function PrepareParamXML(VariableItem: TVariableItem): string;
    function GetSourceIndex(const Index: Integer): integer;
    function GetTargetIndex(const Index: Integer): integer;
    procedure SetSourceIndex(const Index, Value: integer);
    procedure SetTargetIndex(const Index, Value: integer);
  protected
    procedure DataChanged;
    procedure DataFinished;
    function SQLFilter: string;
    function GetSQLQuery: string; virtual;
    /// <summary>Абстрактная функция получения типа запроса данных, которая должна быть реализована в наследниках</summary>
    function GetQueryType: TDeQueryType; virtual; abstract;
    function DoLinkField(const aDataSet: TObject; const FieldName: string; var LinkMode: TLinkJoinMode; var LinkDataset: TObject; var LinkFieldName: string): TLinkJoinMode; dynamic;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Records: TObject read FRecords write FRecords;
    property Params: TVariableList read FParams;
    /// <summary>Свойство типа запроса данных</summary>
    /// <remarks>Зависит от наследника</remarks>
    property QueryType : TDeQueryType read GetQueryType;
    property Table: string read GetTable write SetTable;
    property TableMeta: TObject read FTableMeta write SetTableMeta;
    property SQL : string read GetSQL write SetSQL;
    property StaticSQL: Boolean read FStaticSQL;
    property ParamValueByName[const aParamName: string]: Variant read GetParamValueByName write SetParamValueByName;
    property Filter : TExpressionItem read FFilter;
    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
    property OnFinished : TNotifyEvent read FOnFinished write FOnFinished;
    property OnSetParamValue : TParamEvent read FOnSetParamValue write FOnSetParamValue;
    procedure Clear; virtual;
    procedure ClearFields; virtual;
    procedure Assign(Source: TCustomQueryDescr); virtual;
    procedure AddCondition(const aField: string; const aFieldType: TFieldType; const aCompare: TOperationType; const aValue: Variant); overload;
    procedure AddCondition(const aField: string; const aFieldType: TFieldType; const aUnary: TOperationType); overload;
    procedure AddOperation(const aOperation: TOperationType);
    procedure AddParamCondition(const aField: string; const aFieldType: TFieldType; const aCompare: TOperationType; const aParamName: string); overload;
    procedure AddParamCondition(const aField: string; const aFieldType: TFieldType; const aCompare: TOperationType; const aParamName: string; const aParamValue: Variant); overload;
    procedure AddConst(const aFieldType: TFieldType; const aValue: Variant);
    procedure AssignFilter(aFilter: TExpressionItem; const aOperation: TOperationType = opAnd); virtual;
    procedure AddSourceField(const aIndex: Integer; const aName: string; const aType: TFieldType; const aSize: Integer);
    procedure AddField(const aOperation: TOperationType; const aField: string = ''); overload;
    procedure AddField(const aField: string; const aFieldType: TFieldType = ftUnknown; const aDataSize: Integer = 0); overload;
    procedure AddField(const aTargetIndex: Integer; const aField: string; const aFieldType: TFieldType = ftUnknown; const aDataSize: Integer = 0); overload;
    procedure AddField(const aField: string; const aFieldType: TFieldType; const aValue: Variant); overload;
    procedure AddFields(const aFields: array of string);
    procedure AddParamField(const aField: string; const aFieldType: TFieldType);
    procedure AddParamValue(const aName: string; const aFieldType: TFieldType; const aValue: Variant);
    procedure AddSortField(const aField: string; const aSortDirection: TDSSortDirection = sdNone);
    procedure AddSortFields(const aFields: array of string; const aSortDirection: TDSSortDirection = sdNone);
    procedure BeginUpdate;
    procedure EndUpdate;
    property OnLinkField: TLinkQueryDescrEvent read FOnLinkField write FOnLinkField;
    property SortCount: Integer read GetSortCount;
    property SortField[const Index: Integer]: string read GetSortField;
    property SortFields: TStringList read FSortFields;
    property SortDirection[const Index: Integer]: TDSSortDirection read GetSortDirection;
    {$IFDEF DEBUG}
    procedure DebugQueryLog(const Text: string);
    {$ENDIF}
    // v. 18.02
    property Fields: TFieldsArray read FFields;
    property FieldCount: Integer read GetFieldCount;
    function IndexByName(const FieldName: string): Integer;
    property Calc: TDeSQLCalculator read FCalc write FCalc;
    // v. 18.10
    property XML: string read GetXML;
    // v.19.08
    property Schema: string read FSchema write FSchema;
    property SourceIndex[const Index: Integer]: integer read GetSourceIndex write SetSourceIndex;
    property TargetIndex[const Index: Integer]: integer read GetTargetIndex write SetTargetIndex;
  end;

  /// <summary>Класс описатель запроса, служащего для выборки множества строк данных</summary>
  TSelectQueryDescr = class(TCustomQueryDescr)
  protected
    function GetSQLQuery: string; override;
    function GetQueryType: TDeQueryType; override;
  end;

  /// <summary>Класс описатель запроса, служащего для выборки одной строки данных</summary>
  TRowQueryDescr = class(TSelectQueryDescr)
  protected
    function GetQueryType: TDeQueryType; override;
  end;

  /// <summary>Класс описатель запроса, служащего для удаления данных</summary>
  TDeleteQueryDescr = class(TCustomQueryDescr)
  protected
    function GetSQLQuery: string; override;
    function GetQueryType: TDeQueryType; override;
  end;

  /// <summary>Класс описатель запроса, служащего для добавления данных</summary>
  TInsertQueryDescr = class(TCustomQueryDescr)
  protected
    function GetSQLQuery: string; override;
    function GetQueryType: TDeQueryType; override;
  end;

  /// <summary>Класс описатель запроса, служащего для обновления данных</summary>
  TUpdateQueryDescr = class(TCustomQueryDescr)
  protected
    function GetSQLQuery: string; override;
    function GetQueryType: TDeQueryType; override;
  end;

  /// <summary>Класс описатель запроса, служащего для добавления таблицы</summary>
  {
  TMSSQLAddTableQueryDescr = class(TCustomQueryDescr)
  protected
    function GetSQLQuery: string; override;
    function GetQueryType: TDeQueryType; override;
  end;
  }

  /// <summary>Класс описатель запроса, служащего для добавления столбца в таблицу</summary>
  {
  TMSSQLAddFieldQueryDescr = class(TCustomQueryDescr)
  protected
    function GetSQLQuery: string; override;
    function GetQueryType: TDeQueryType; override;
  end;
  }

  /// <summary>Класс описатель запроса, служащего для изменения столбца в таблице</summary>
  {
  TMSSQLModifyFieldQueryDescr = class(TCustomQueryDescr)
  protected
    function GetSQLQuery: string; override;
    function GetQueryType: TDeQueryType; override;
  end;
  }

  /// <summary>Класс описатель запроса, служащего для удаления столбца из таблицы</summary>
  {
  TMSSQLDeleteFieldQueryDescr = class(TCustomQueryDescr)
  protected
    function GetSQLQuery: string; override;
    function GetQueryType: TDeQueryType; override;
  end;
  }

  /// <summary>Класс описатель запроса, служащего для добавления поиска "дырки" для целочисленного столбца</summary>
  TMSSQLHoleFieldQueryDescr = class(TCustomQueryDescr)
  protected
    function GetSQLQuery: string; override;
    function GetQueryType: TDeQueryType; override;
  end;

resourcestring
  sConditionNotSupported = 'Condition not supported for SQL property assigned';
  sUnknownParamName = 'Unknown param name ''%s''';
  sUnexpectedMethodCall = 'Unexpected method call';
  sInvalidComparisonOperation = 'Invalid comparison operation';
  sInvalidLogicalOperation = 'Invalid logical operation';

implementation

uses SysUtils, StrUtils, Contnrs, Variants,
     DeLog, DeMeta, DeMetadata, DataCacheUnit, Funcs;

{ TCustomQueryDescr }

constructor TCustomQueryDescr.Create;
begin
  inherited Create;
  FParams := TVariableList.Create;
  FFilter := TExpressionItem.Create;
  FSortFields := TStringList.Create;
end;

destructor TCustomQueryDescr.Destroy;
begin
  FreeAndNil(FCalc);

  FSortFields.Free;
  FFilter.Free;
  FParams.Free;
  FFields := nil;

  inherited Destroy;
end;

procedure TCustomQueryDescr.SetTable(const aTable: string);
begin
  FTableName := aTable;
  DataChanged;
end;

function TCustomQueryDescr.GetTable: string;
begin
  if Assigned(FTableMeta) then Result := TTableMeta(FTableMeta).Table
                          else Result := FTableName;
end;

procedure TCustomQueryDescr.SetTableMeta(const Value: TObject);
begin
  if Value is TTableMeta then
    begin
      FTableMeta := Value;
      DataChanged;
    end
  else
   raise Exception.Create('TCustomQueryDescr.SetTableMeta error');
end;

procedure TCustomQueryDescr.DataChanged;
begin
  if FUpdateCount = 0 then
    begin
      if Assigned(FOnChanged) then
        OnChanged(Self);
    end
  else
    Include(FUpdateSet, duChanged);
end;

procedure TCustomQueryDescr.DataFinished;
begin
  if FUpdateCount = 0 then
    begin
      if Assigned(OnFinished) then
        OnFinished(Self);
    end
  else
    Include(FUpdateSet, duFinished);
end;

function TCustomQueryDescr.GetSQL: string;
begin
  if (Length(FSQL) = 0) or (0 < FieldCount) or (0 < SortCount) or ((0<Filter.Count) and (Filter.Active)) then
    Result := GetSQLQuery
  else
    Result := FSQL;
end;

function TCustomQueryDescr.GetSQLQuery: string;
begin
  FStaticSQL:= False;
  Result:= EmptyStr;
end;

procedure TCustomQueryDescr.SetSQL(const aSQL: string);
begin
  FSQL := Trim(aSQL);
  FStaticSQL:= True;

  DataChanged;
  DataFinished;
  //if Assigned(FOnFinished) then
  //  OnFinished(Self);
end;

function TCustomQueryDescr.GetParamValueByName(const aParamName: string): Variant;
var VarItem : TVariableItem;
begin
  VarItem := FParams.FindVariable(aParamName);
  Assert(Assigned(VarItem), Format(SUnknownParamName, [aParamName]));
  Result := VarItem.Value;
end;

procedure TCustomQueryDescr.SetParamValueByName(const aParamName: string; const Value: Variant);
var VarItem : TVariableItem;
begin
  DataFinished;
  //if Assigned(FOnFinished) then
  //  OnFinished(Self);
  VarItem := FParams.FindVariable(aParamName);
  Assert(Assigned(VarItem), Format(SUnknownParamName, [aParamName]));
  VarItem.Value := Value;
  if Assigned(FOnSetParamValue) then
    FOnSetParamValue(VarItem);
end;

function TCustomQueryDescr.GetFieldCount: Integer;
begin
  Result := Length(FFields);
end;

function TCustomQueryDescr.AddFieldData: Integer;
begin
  Result:= Length(FFields);
  SetLength(FFields, Succ(Result));
  Initialize(FFields[Result]);

  FFields[Result].TargetName:= EmptyStr;
  FFields[Result].TargetOperation := opNone;
  FFields[Result].TargetType:= ftUnknown;
  FFields[Result].TargetSize:= 0;
  FFields[Result].FSourceIndex:= NullIndex;
end;

function TCustomQueryDescr.IndexByName(const FieldName: string): Integer;
var Index: Integer;
begin
  Result := -1;
  for Index := Low(FFields) to High(FFields) do
    if SameText(FFields[Index].SourceName, FieldName) then Exit(Index);
end;

function TCustomQueryDescr.SQLFilter: string;
var v: Variant;
begin
  if FFilter.Count=0 then Exit(EmptyStr);

  Assert(QueryType in [qtUpdate, qtDelete, qtSelect, qtRow], sUnexpectedMethodCall);
  Assert(Assigned(FCalc), ClassName + '.SQLFilter: TDeSQLCalculator is null');

  v:= FCalc.Calculate(FFilter);
  if varType(V) = varBoolean
    then if V then Result:= EmptyStr
              else Result:= '1=0'
    else           Result:= VarToStr(V);
end;

procedure TCustomQueryDescr.Clear;
begin
  FTableName := EmptyStr;
  FTableMeta := nil;
  FSQL := EmptyStr;
  FStaticSQl:= False;
  FFilter.Clear;
  FParams.Clear;
  FFields := nil;
  FSortFields.Clear;
  FXML := EmptyStr;
  FRecords:= nil;
  DataChanged;
end;

procedure TCustomQueryDescr.ClearFields;
begin
  FFields := nil;
  DataChanged;
end;

procedure TCustomQueryDescr.Assign(Source: TCustomQueryDescr);
var I : integer;
begin
  FTableName := Source.FTableName;
  FTableMeta := Source.FTableMeta;
  FSQL   := Source.FSQL;
  FStaticSQl:= Source.FStaticSQL;
  FParams.Assign(Source.FParams);
  FFilter.Assign(Source.FFilter);
  for I := 0 to Pred(FFilter.Count) do
    if FFilter[I].ItemType = piParameter then
      FFilter[I].Parameter := FParams[Source.FParams.IndexOf(FFilter[I].Parameter)];
  SetLength(FFields, Length(Source.FFields));
  for I := 0 to Pred(Length(Source.FFields)) do
    FFields[I] := Source.FFields[I];
  FSortFields.Assign(Source.FSortFields);
  FXML := Source.FXML;
  DataChanged;
end;

procedure TCustomQueryDescr.AddCondition(const aField: string; const aFieldType: TFieldType; const aCompare: TOperationType; const aValue: Variant);
var Item : TPostfixItem;
begin
  Assert(QueryType in [qtUpdate, qtDelete, qtSelect, qtRow], SUnexpectedMethodCall);
  Assert(aCompare in ComparisonOperations, sInvalidComparisonOperation);
  Assert(Length(FSQL) = 0, sConditionNotSupported);
  { поле }
  Item := FFilter.AddPostfixItem;
  Item.Ident := aField;
  Item.ResultType := aFieldType;
  { значение поля }
  Item := FFilter.AddPostfixItem;
  Item.SetData(aValue, aFieldType);
  Item.ResultType := aFieldType;
  { операция сравнения }
  Item := FFilter.AddPostfixItem;
  Item.BinaryOp := aCompare;
  Item.ResultType := ftBoolean;
  DataChanged;
end;

procedure TCustomQueryDescr.AddCondition(const aField: string; const aFieldType: TFieldType; const aUnary: TOperationType);
var Item : TPostfixItem;
begin
  Assert(QueryType in [qtUpdate, qtDelete, qtSelect, qtRow], sUnexpectedMethodCall);
  Assert(aUnary in ComparisonOperations*UnaryOperations, sInvalidComparisonOperation);
  Assert(Length(FSQL) = 0, sConditionNotSupported);
  { поле }
  Item := FFilter.AddPostfixItem;
  Item.Ident := aField;
  Item.ResultType := aFieldType;
  { операция }
  Item := FFilter.AddPostfixItem;
  Item.UnaryOp := aUnary;
  Item.ResultType := ftBoolean;
  DataChanged;
end;

procedure TCustomQueryDescr.AddConst(const aFieldType: TFieldType; const aValue: Variant);
var Item : TPostfixItem;
begin
  Assert(Length(FSQL) = 0, sConditionNotSupported);
  { значение }
  Item := FFilter.AddPostfixItem;
  Item.SetData(aValue, aFieldType);
  Item.ResultType := aFieldType;
  DataChanged;
end;

procedure TCustomQueryDescr.AddParamCondition(const aField: string; const aFieldType: TFieldType; const aCompare: TOperationType; const aParamName: string);
var Item    : TPostfixItem;
    VarItem : TVariableItem;
    NewParamName: string;
begin
  Assert(QueryType in [qtUpdate, qtDelete, qtSelect, qtRow], SUnexpectedMethodCall);
  Assert(aCompare in ComparisonOperations, sInvalidComparisonOperation);
  Assert(Length(FSQL) = 0, sConditionNotSupported);
  { поле }
  Item := FFilter.AddPostfixItem;
  Item.Ident := aField;
  Item.ResultType := aFieldType;
  { переменная }
  if Length(aParamName) = 0 then NewParamName := 'P_' + aField + '_' + IntToStr(Succ(FParams.Count))
                            else NewParamName := aParamName;
  VarItem := FParams.FindVariable(NewParamName);
  if not Assigned(VarItem) then
  begin
    VarItem := TVariableItem.Create(aFieldType, NewParamName, [amRead, amWrite]);
    FParams.Add(VarItem);
  end;
  { значение поля }
  Item := FFilter.AddPostfixItem;
  Item.Parameter := VarItem;
  { операция }
  Item := FFilter.AddPostfixItem;
  Item.BinaryOp := aCompare;
  Item.ResultType := ftBoolean;
  DataChanged;
end;

procedure TCustomQueryDescr.AddParamCondition(const aField: string; const aFieldType: TFieldType; const aCompare: TOperationType; const aParamName: string; const aParamValue: Variant);
var  NewParamName: string;
begin
  if Length(aParamName) = 0 then NewParamName := 'P_' + aField + '_' + IntToStr(Succ(FParams.Count))
                            else NewParamName := aParamName;
  AddParamCondition(aField, aFieldType, aCompare, aParamName);
  ParamValueByName[NewParamName] := aParamValue;
end;

procedure TCustomQueryDescr.AddOperation(const aOperation: TOperationType);
var Item : TPostfixItem;
begin
  Assert(QueryType in [qtUpdate, qtDelete, qtSelect, qtRow], sUnexpectedMethodCall);
  Assert(aOperation in LogicalOperations, sInvalidLogicalOperation);
  Assert(Length(FSQL) = 0, sConditionNotSupported);
  { операция }
  Item := FFilter.AddPostfixItem;
  if aOperation in BinaryOperations then
    Item.BinaryOp := aOperation
  else
    Item.UnaryOp := aOperation;
  Item.ResultType := ftBoolean;
  DataChanged;
end;

procedure TCustomQueryDescr.AssignFilter(aFilter: TExpressionItem; const aOperation: TOperationType = opAnd);
var
  i: Integer;
begin
  Assert(QueryType in [qtUpdate, qtDelete, qtSelect, qtRow], sUnexpectedMethodCall);
// Assert(Length(FSQL) = 0, sConditionNotSupported);
  if Assigned(aFilter) then
    if aFilter.Count>0 then
      begin
        BeginUpdate;

        for i := 0 to Pred(aFilter.Count) do
          FFilter.AddPostfixItem.Assign(aFilter[i]);

        if FFilter.Count > aFilter.Count then
          AddOperation(aOperation);
        EndUpdate;
      end;
end;

function TCustomQueryDescr.GetTargetIndex(const Index: Integer): integer;
begin
  if (-1 < Index) and (Index < Length(FFields)) then Result:= FFields[Index].TargetIndex
                                                else Result:= -1;
end;

procedure TCustomQueryDescr.SetTargetIndex(const Index, Value: integer);
begin
  if (-1 < Index) and (Index < Length(FFields)) then FFields[Index].TargetIndex:= Value
                                                else raise Exception.Create('Index ('+IntToStr(Index)+') out of bounds');
end;

function TCustomQueryDescr.GetSourceIndex(const Index: Integer): integer;
begin
  if (-1 < Index) and (Index < Length(FFields)) then Result:= FFields[Index].SourceIndex
                                                else Result:= -1;
end;

procedure TCustomQueryDescr.SetSourceIndex(const Index, Value: integer);
begin
  if (-1 < Index) and (Index < Length(FFields)) then FFields[Index].SourceIndex:= Value
                                                else raise Exception.Create('Index ('+IntToStr(Index)+') out of bounds');
end;

procedure TCustomQueryDescr.AddSourceField(const aIndex: Integer; const aName: string; const aType: TFieldType; const aSize: Integer);
var N: Integer;
begin
  Assert(QueryType in [qtSelect, qtRow], sUnexpectedMethodCall);
  N:= AddFieldData;

  FFields[N].TargetName:= aName;
  FFields[N].SourceName:= aName;
  FFields[N].SourceType:= aType;
  FFields[N].SourceSize:= aSize;
  FFields[N].SourceOperation:= opNone;
  FFields[N].SourceIndex:= aIndex;

//DataChanged; - не нужно !
end;

procedure TCustomQueryDescr.AddField(const aOperation: TOperationType; const aField: string = '');
var N: Integer;
begin
  Assert(QueryType in [qtSelect, qtRow, qtHole], sUnexpectedMethodCall);
  N:= AddFieldData;
  FFields[N].TargetOperation:= aOperation;
  FFields[N].TargetName:= aField;
  DataChanged;
end;

procedure TCustomQueryDescr.AddField(const aField: string; const aFieldType: TFieldType; const aDataSize: Integer);
var N: Integer;
begin
  Assert(QueryType in [qtSelect, qtRow, qtInsert, qtHole], sUnexpectedMethodCall);
  N:= AddFieldData;
  FFields[N].TargetName:= aField;
  FFields[N].TargetType:= aFieldType;
  FFields[N].TargetSize:= aDataSize;
  FFields[N].TargetIndex:= N;  // Индекс по порядку добавления полей
  DataChanged;
end;

procedure TCustomQueryDescr.AddField(const aTargetIndex: Integer; const aField: string; const aFieldType: TFieldType; const aDataSize: Integer);
var N: Integer;
begin
  Assert(QueryType in [qtSelect, qtRow, qtInsert, qtHole], sUnexpectedMethodCall);
  N:= AddFieldData;
  FFields[N].TargetName:= aField;
  FFields[N].TargetType:= aFieldType;
  FFields[N].TargetSize:= aDataSize;
  FFields[N].TargetIndex:= aTargetIndex;
  DataChanged;
end;

procedure TCustomQueryDescr.AddField(const aField: string; const aFieldType: TFieldType; const aValue: Variant);
var N: Integer;
    VariableItem: TVariableItem;
begin
  Assert(QueryType in [qtInsert, qtUpdate], sUnexpectedMethodCall);
  N:= AddFieldData;
  FFields[N].TargetName:= aField;
  FFields[N].TargetType:= aFieldType;
  FFields[N].Value:= aValue;

  // Багафикса не параметризованного поля! Делаю его по любому параметризованным!
  VariableItem := FParams.FindVariable(aField);
  if not Assigned(VariableItem) then
    begin
      VariableItem := TVariableItem.Create(aFieldType, aField, [amRead, amWrite]);
      FParams.Add(VariableItem);
      SetParamValueByName(aField, aValue);
    end;
  FFields[N].Parameter := VariableItem.Name;

  DataChanged;
end;

procedure TCustomQueryDescr.AddFields(const aFields: array of string);
var i: Integer;
begin
  for i := Low(aFields) to High(aFields) do
    AddField(aFields[i] );
end;

procedure TCustomQueryDescr.AddParamField(const aField: string; const aFieldType: TFieldType);
var VarItem : TVariableItem;
begin
  Assert(QueryType in [qtInsert, qtUpdate], sUnexpectedMethodCall);
//Assert(Length(FSQL) = 0, sConditionNotSupported); // Возможно надо всё же разрешить
  AddFieldData;
  with FFields[Length(FFields)-1] do
  begin
    TargetName := aField;
    TargetType := aFieldType;
    VarItem := FParams.FindVariable(aField);
    if not Assigned(VarItem) then
    begin
      VarItem := TVariableItem.Create(aFieldType, aField, [amRead, amWrite]);
      FParams.Add(VarItem);
    end;
    Parameter := VarItem.Name;
  end;
  DataChanged;
end;

procedure TCustomQueryDescr.AddParamValue(const aName: string; const aFieldType: TFieldType; const aValue: Variant);
var VarItem : TVariableItem;
begin
  Assert(QueryType in [qtInsert, qtUpdate], sUnexpectedMethodCall);
  VarItem := FParams.FindVariable(aName);
  if not Assigned(VarItem) then
    begin
      VarItem := TVariableItem.Create(aFieldType, aName, [amRead, amWrite]);
      VarItem.Value:= aValue;
      FParams.Add(VarItem);
    end;
  DataChanged;
end;

procedure TCustomQueryDescr.AddSortField(const aField: string; const aSortDirection: TDSSortDirection);
begin
  Assert(QueryType = qtSelect, sUnexpectedMethodCall);
//Assert(Length(FSQL) = 0, sConditionNotSupported);
  if FSortFields.IndexOfName(aField) = -1 then
    begin
      FSortFields.AddObject(aField + '=', Pointer(Ord(aSortDirection)));
      DataChanged;
    end;
end;

procedure TCustomQueryDescr.AddSortFields(const aFields: array of string; const aSortDirection: TDSSortDirection);
var
  Index: Integer;
begin
  Assert(QueryType = qtSelect, sUnexpectedMethodCall);
//Assert(Length(FSQL) = 0, sConditionNotSupported);
  if Length(aFields) <> 0 then
    begin
      for Index := Low(aFields) to High(aFields) do
        if FSortFields.IndexOfName(aFields[Index]) = -1 then
          FSortFields.AddObject(aFields[Index] + '=', Pointer(Ord(aSortDirection)));
      DataChanged;
    end;
end;

function TCustomQueryDescr.DoLinkField(const aDataSet: TObject; const FieldName: string; var LinkMode: TLinkJoinMode; var LinkDataset: TObject; var LinkFieldName: string): TLinkJoinMode;
begin
  LinkDataset := nil;
  LinkFieldName := EmptyStr;
  if Assigned(FOnLinkField) then
    FOnLinkField(Self, aDataSet, FieldName, Result, LinkDataset, LinkFieldName)
  else
    Result := lmUnknown;
end;

procedure TCustomQueryDescr.BeginUpdate;
begin
  if FUpdateCount = 0 then FUpdateSet := [];
  Inc(FUpdateCount);
end;

procedure TCustomQueryDescr.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 1 then
    begin
      FUpdateCount := 0;
      if duChanged in FUpdateSet then
        DataChanged;
      if duFinished in FUpdateSet then
        DataFinished;
      FUpdateSet := [];
    end;
end;

function TCustomQueryDescr.GetSortCount: Integer;
begin
  Result := FSortFields.Count;
end;

function TCustomQueryDescr.GetSortField(const Index: Integer): string;
begin
  Result := FSortFields.Names[Index];
end;

function TCustomQueryDescr.GetSortDirection(const Index: Integer): TDSSortDirection;
begin
  Result := TDSSortDirection(FSortFields.Objects[Index]);
end;

{$IFDEF DEBUG}
procedure TCustomQueryDescr.DebugQueryLog(const Text: string);
var
  Value: string;
  function PrepareFieldsLog: string;
  var
    Index, IndexSize: Integer;
    function PrepareFieldLog(const Field: TFieldData): string;
    const
      FieldTypeNames: array[TFieldType] of PChar = ('ftUnknown', 'ftString', 'ftSmallint', 'ftInteger', 'ftWord',
      'ftBoolean', 'ftFloat', 'ftCurrency', 'ftBCD', 'ftDate', 'ftTime', 'ftDateTime',
      'ftBytes', 'ftVarBytes', 'ftAutoInc', 'ftBlob', 'ftMemo', 'ftGraphic', 'ftFmtMemo',
      'ftParadoxOle', 'ftDBaseOle', 'ftTypedBinary', 'ftCursor', 'ftFixedChar', 'ftWideString',
      'ftLargeint', 'ftADT', 'ftArray', 'ftReference', 'ftDataSet', 'ftOraBlob', 'ftOraClob',
      'ftVariant', 'ftInterface', 'ftIDispatch', 'ftGuid', 'ftTimeStamp', 'ftFMTBcd',
      'ftFixedWideChar', 'ftWideMemo', 'ftOraTimeStamp', 'ftOraInterval',
      'ftLongWord', 'ftShortint', 'ftByte', 'ftExtended', 'ftConnection', 'ftParams', 'ftStream',
      'ftTimeStampOffset', 'ftObject', 'ftSingle'); //49..51
    const
      BoolNames: array[Boolean] of PChar = ('-', '+');
    var
      Value: string;
    begin
      Result := Field.Parameter;
      Value := VariantToString(Field.Value);
      if Length(Value) > 92 then Value := Copy(Value, 1, 92) + ' ...';
      Result := Format(#13#10'                        ¦ %*d ¦ %-96s ¦ %-24s ¦ %-96s ¦ %-24s ¦ %9d ¦ %9d ¦ %9d ¦ %s ¦ %-96s ¦ %-96s ¦',
        [
        IndexSize, Index,
        Field.TargetName,
        StrPas(FieldTypeNames[Field.SourceType]),
        Field.SourceName, Field.Source, Field.SourceSize,
        0, 0,
        '*', Result, Value
        ]);
    end;
  begin
    Result := EmptyStr;
    IndexSize := Length(IntToStr(High(FFields) - Low(FFields)));
    if IndexSize < 2 then IndexSize := 2;
    for Index := Low(FFields) to High(FFields) do
      Result := Result + PrepareFieldLog(FFields[Index]);
    if Length(Result) <> 0 then
      Result := #13#10'                        Request data fields:' +
        Format(#13#10'                        -%sT%sT%sT%sT%sT%sT%sT%sT%sT%sT%s¬',
          [
            DupeString('-', IndexSize + 2), DupeString('-', 98),
            DupeString('-', 26), DupeString('-', 98),
            DupeString('-', 26), DupeString('-', 11), DupeString('-', 11),
            DupeString('-', 11), DupeString('-', 3),
            DupeString('-', 98), DupeString('-', 98)
          ]) +
        Format(#13#10'                        ¦ %*s ¦ %-96s ¦ %-24s ¦ %-96s ¦ %-24s ¦ %-9s ¦ %-9s ¦ %-9s ¦ %s ¦ %-96s ¦ %-96s ¦',
          [
          -IndexSize, 'No', 'Field', 'Type', 'Original', 'Source', 'Size', 'Precision', 'Scale', 'N', 'Parameter', 'Value'
          ]) +
        Format(#13#10'                        +%s+%s+%s+%s+%s+%s+%s+%s+%s+%s+%s+',
          [
            DupeString('-', IndexSize + 2), DupeString('-', 98),
            DupeString('-', 26), DupeString('-', 98),
            DupeString('-', 26), DupeString('-', 11), DupeString('-', 11),
            DupeString('-', 11), DupeString('-', 3),
            DupeString('-', 98), DupeString('-', 98)
          ]) + Result +
        Format(#13#10'                        L%s+%s+%s+%s+%s+%s+%s+%s+%s+%s+%s-',
          [
            DupeString('-', IndexSize + 2), DupeString('-', 98),
            DupeString('-', 26), DupeString('-', 98),
            DupeString('-', 26), DupeString('-', 11), DupeString('-', 11),
            DupeString('-', 11), DupeString('-', 3),
            DupeString('-', 98), DupeString('-', 98)
          ]);
  end;
  function PrepareSortFieldsLog: string;
  const
    SortNames: array[TDSSortDirection] of PChar = ('sdNone', 'sdAscending', 'sdDescending');
  var
    Index, IndexSize: Integer;
  begin
    Result := EmptyStr;
    IndexSize := Length(IntToStr(SortCount));
    if IndexSize < 2 then IndexSize := 2;
    for Index := 0 to Pred(SortCount) do
      Result := Format(#13#10'                        ¦ %*d ¦ %-96s ¦ %-16s ¦',
        [
        IndexSize, Index,
        SortField[Index],
        StrPas(SortNames[SortDirection[Index]])
        ]);
    if Length(Result) <> 0 then
      Result := #13#10'                        Request sort fields:' +
        Format(#13#10'                        -%sT%sT%s¬',
          [
            DupeString('-', IndexSize + 2), DupeString('-', 98), DupeString('-', 18)
          ]) +
        Format(#13#10'                        ¦ %*s ¦ %-96s ¦ %-16s ¦',
          [
          -IndexSize, 'No', 'Field', 'Direction'
          ]) +
        Format(#13#10'                        +%s+%s+%s+',
          [
            DupeString('-', IndexSize + 2), DupeString('-', 98), DupeString('-', 18)
          ]) + Result +
        Format(#13#10'                        L%s+%s+%s-',
          [
            DupeString('-', IndexSize + 2), DupeString('-', 98), DupeString('-', 18)
          ]);
  end;
begin
  if Assigned(FFilter) then
    begin
      Value := FFilter.PrepareDebugPostfixLog;
      if Length(Value) <> 0 then
        Value := #13#10'                        Process filters:' + Value;
    end
  else
    Value := EmptyStr;
  DebugLog(Text + PrepareFieldsLog + PrepareSortFieldsLog + Value);
end;
{$ENDIF}

{ TSelectQueryDescr }

function TSelectQueryDescr.GetQueryType: TDeQueryType;
begin
  Result := qtSelect;
end;

function TSelectQueryDescr.GetSQLQuery: string;
const
  SortDirText: array[TDSSortDirection] of PChar = ('', ' ASC', ' DESC');
var
  Index: Integer;
  TName, Value: string;
  JoinBuilder: TJoinBuilder;
  HaveSimple, HaveAgregate: Boolean;
begin
  Result := inherited GetSQLQuery;
  JoinBuilder := TJoinBuilder.Create(Self);
  try
    HaveSimple:= false;
    HaveAgregate:= false;

    if (Length(FFields) = 1) and (Length(FFields[0].TargetName) = 0) and (FFields[0].TargetOperation = opNone) then
      begin
        Result:= Result + '*';
      end
    else
      for Index := Low(FFields) to High(FFields) do
        begin
          if Length(Result) <> 0 then Result := Result + ', ';

          if Length(FFields[Index].TargetName)=0
            then if FFields[Index].TargetOperation = opNone
                   then Result:= Result + '*'
                   else Result:= Result + OperationLexeme[FFields[Index].SourceOperation]+'(*)'
            else        Result:= Result + Calc.onFieldToStr(FFields[Index].TargetName,
                                                            FFields[Index].SourceAlias,
                                                            FFields[Index].Source,
                                                            FFields[Index].SourceOperation,
                                                            FFields[Index].SourceType,
                                                            FFields[Index].SourceSize);
          FFields[Index].SourceIndex:= Index;

          if FFields[Index].TargetOperation = opNone then HaveSimple:= True
                                                     else HaveAgregate:= True;
        end;

    if Length(Result) = 0 then Result := '*';

    Result := 'SELECT ' + Result + ' FROM ';
    Index := JoinBuilder.FNodes.IndexOfName(EmptyStr);
    if (Index > -1) or (JoinBuilder.FAllwaysJOIN)
      then begin
             TName:= Table;
             if assigned(JoinBuilder.FirstDataSet)
             then if Length(Trim(TTableMeta(JoinBuilder.FirstDataSet).SelectSQL)) = 0
                  then Result := Result + Calc.onTableToStr(Table, JoinBuilder.FNodes.ValueFromIndex[Index],
                                              TTableMeta(JoinBuilder.FirstDataSet).Database.Database{, LinkSchemaName})
                  else Result := Result + '('+TTableMeta(JoinBuilder.FirstDataSet).SelectSQL+') as '+ JoinBuilder.FNodes.ValueFromIndex[Index]
             else
                       Result := Result + Calc.onTableToStr(Table, JoinBuilder.FNodes.ValueFromIndex[Index], EmptyStr, Schema)
           end
      else     Result :=  Result + Calc.onTableToStr(Table, EmptyStr, EmptyStr, Schema);
    Result := Result + JoinBuilder.SQL;

    // Подготовим поля при необходимости в фильтрах ...
    // TDeCalculator использует     .Ident    = FIELD_NAME                     - сохраняется исходное
    // TDeSQLCalculator использует  .IdentSQL = A.[database_name].[FIELD_NAME] - меняется под запрос и тип БД
    for Index := 0 to Pred(JoinBuilder.FLinks.Count) do
      (JoinBuilder.FLinks.Objects[Index] as TPostfixItem).IdentSQL :=
        Calc.onFieldToStr(JoinBuilder.FLinks.Names[Index], EmptyStr, JoinBuilder.FLinks.ValueFromIndex[Index]);

    Value := SQLFilter;
    // Если нужна фильтрация, то ...
    if Length(Value) <> 0 then
      Result := Result + ' WHERE ' + Value;

    if HaveSimple and HaveAgregate then
      begin
        Value:= EmptyStr;
        for Index := Low(FFields) to High(FFields) do
          if FFields[Index].TargetOperation = opNone then
            Funcs.StrAdd(Value, ', ', Calc.onFieldToStr(FFields[Index].SourceName, EmptyStr, FFields[Index].Source));
        Result := Result + ' GROUP BY ' + Value;
      end;

    Value := EmptyStr;
    for Index := 0 to Pred(FSortFields.Count) do
      begin
        if 0 < Length(Value) then Value := Value + ', ';

        Value := Value + Calc.onFieldToStr(FSortFields.Names[Index], EmptyStr, FSortFields.ValueFromIndex[Index])
                       + StrPas(SortDirText[TDSSortDirection(FSortFields.Objects[Index])]);
      end;
    if Length(Value) <> 0 then Value := ' ORDER BY ' + Value;

    Result := Result + Value;
  finally
    JoinBuilder.Free;
  end;
end;

{ TRowQueryDescr }

function TRowQueryDescr.GetQueryType: TDeQueryType;
begin
  Result := qtRow;
end;

{ TDeleteQueryDescr }

function TDeleteQueryDescr.GetQueryType: TDeQueryType;
begin
  Result := qtDelete;
end;

function TDeleteQueryDescr.GetSQLQuery: string;
var
  TName, Value: string;
  Index: Integer;
  JoinBuilder: TJoinBuilder;
begin
  Result := inherited GetSQLQuery;

  JoinBuilder := TJoinBuilder.Create(Self);
  Index := JoinBuilder.FNodes.IndexOfName(EmptyStr);
    if (Index > -1) or (JoinBuilder.FAllwaysJOIN)
      then begin
             Result := 'DELETE A FROM ';
             TName:= Table;
             if assigned(JoinBuilder.FirstDataSet)
             then if Length(Trim(TTableMeta(JoinBuilder.FirstDataSet).SelectSQL)) = 0
                  then Result := Result + Calc.onTableToStr(Table, JoinBuilder.FNodes.ValueFromIndex[Index],
                                              TTableMeta(JoinBuilder.FirstDataSet).Database.Database{, LinkSchemaName})
                  else Result := Result + '('+TTableMeta(JoinBuilder.FirstDataSet).SelectSQL+') as '+ JoinBuilder.FNodes.ValueFromIndex[Index]
             else
                       Result := Result + Calc.onTableToStr(Table, JoinBuilder.FNodes.ValueFromIndex[Index], EmptyStr, Schema)
           end
      else begin
                       Result := 'DELETE FROM ' + Calc.onTableToStr(Table, EmptyStr, EmptyStr, Schema);
           end;
    Result := Result + JoinBuilder.SQL;

    // Подготовим поля при необходимости в фильтрах ...
    // TDeCalculator использует     .Ident    = FIELD_NAME                     - сохраняется исходное
    // TDeSQLCalculator использует  .IdentSQL = A.[database_name].[FIELD_NAME] - меняется под запрос и тип БД
    for Index := 0 to Pred(JoinBuilder.FLinks.Count) do
      (JoinBuilder.FLinks.Objects[Index] as TPostfixItem).IdentSQL :=
        Calc.onFieldToStr(JoinBuilder.FLinks.Names[Index], EmptyStr, JoinBuilder.FLinks.ValueFromIndex[Index]);

    Value := SQLFilter;
    // Должно быть условие, чтобы удалять не все записи
    if Length(Value) <> 0 then
      Result := Result + ' WHERE ' + Value;
end;

{ TInsertQueryDescr }

function TInsertQueryDescr.GetQueryType: TDeQueryType;
begin
  Result := qtInsert;
end;

function TInsertQueryDescr.GetSQLQuery: string;
var
  s, FieldsStr, ValuesStr, AllValuesStr: string;
  N, Index: Integer;
begin
  Result := inherited GetSQLQuery;

  FieldsStr:= EmptyStr;
  for Index:= 0 to Pred(Length(FFields)) do
    Funcs.StrAdd(FieldsStr, ', ', Calc.onFieldToStr(FFields[Index].SourceName) );

  AllValuesStr:= EmptyStr;
  if (FRecords is TCacheItemList) then
    for N:= 0 to Pred(TObjectList(FRecords).Count) do
      begin
        ValuesStr:= EmptyStr;
        for Index:= 0 to Pred(Length(FFields)) do
          begin
            if not assigned( Params.FindVariable(FFields[Index].SourceName + IntToStr(N)) )
              then Funcs.StrAdd(ValuesStr, ', ', Calc.onConstToStr(TCacheItemList(FRecords).Items[N].ValueByName[FFields[Index].SourceName],
                                                                   FFields[Index].SourceType))
              else Funcs.StrAdd(ValuesStr, ', ', ':' + FFields[Index].SourceName + IntToStr(N));
          end;
        Funcs.StrAdd( AllValuesStr, ', ', '(' + ValuesStr + ')' );
      end
  else
      begin
        ValuesStr:= EmptyStr;
        for Index:= 0 to Pred(Length(FFields)) do
          Funcs.StrAdd( ValuesStr, ',', ':' + FFields[Index].Parameter);
        AllValuesStr:= '('+ValuesStr+')';
      end;

  Result:= 'INSERT INTO ' + Calc.onTableToStr(Table, EmptyStr, EmptyStr, Schema) + ' (' + FieldsStr + ') VALUES '+ AllValuesStr;
end;

{ TUpdateQueryDescr }

function TUpdateQueryDescr.GetQueryType: TDeQueryType;
begin
  Result := qtUpdate;
end;

function TUpdateQueryDescr.GetSQLQuery: string;
var
  Index: Integer;
  ValuesStr, FilterString: string;
begin
  Result := inherited GetSQLQuery;

  ValuesStr:= EmptyStr;
  for Index := Low(FFields) to High(FFields) do
    Funcs.StrAdd(ValuesStr, ', ', Calc.onFieldToStr(FFields[Index].SourceName) + '=:' + FFields[Index].Parameter);

  Result := 'UPDATE ' + Calc.onTableToStr(Table, EmptyStr, EmptyStr, Schema)+ ' SET ' + ValuesStr;

  for Index := 0 to Pred(Filter.Count) do
    if Filter[Index].ItemType = piIdent then
      Filter[Index].IdentSQL:= Calc.onFieldToStr(Filter[Index].Ident);

  FilterString := SQLFilter;
  // Если нужна фильтрация, то ...
  if Length(FilterString) <> 0 then
    Result := Result + ' WHERE ' + FilterString;
end;

{ TMSSQLHoleFieldQueryDescr }

function TMSSQLHoleFieldQueryDescr.GetQueryType: TDeQueryType;
begin
  Result := qtHole;
end;

function TMSSQLHoleFieldQueryDescr.GetSQLQuery: string;
begin
  Result := inherited GetSQLQuery;

  case Length(FFields) of
    0: Result := '/* You must define one field */';
    1:
      begin
        Result := Calc.onFieldToStr(FFields[0].TargetName);
        Result := 'SELECT TOP 1 ' + Calc.onFieldToStr(FFields[0].TargetName, EmptyStr, 'A') + ' FROM ' +
                  '(SELECT 0 as ' + Calc.onFieldToStr(FFields[0].TargetName) +
                  ' UNION ALL ' +
                  'SELECT ' + Calc.onFieldToStr(FFields[0].TargetName) + ' FROM ' + Calc.onTableToStr(Table, EmptyStr, EmptyStr, Schema) + ') A' +
                  ' WHERE NOT EXISTS(SELECT * FROM ' + Calc.onTableToStr(Table, EmptyStr, EmptyStr, Schema) +
                  ' WHERE ' + Calc.onFieldToStr(FFields[0].TargetName) + ' = ' + Calc.onFieldToStr( FFields[0].TargetName, EmptyStr ,'A') + ' + 1)';
      end;
  else
    Result := '/* Invalid definition of more than one field */';
  end;
end;

{ TJoinBuilder }

constructor TJoinBuilder.Create(ADescr: TCustomQueryDescr);
begin
  FNodes := TStringList.Create;
  FLinks := TStringList.Create;
  FDescr := ADescr;
  FSequenceID := 1;
  FAllwaysJOIN := False;

  if Assigned(FDescr) and Assigned(FDescr.FTableMeta) then
    begin
      FirstDataSet:= FDescr.FTableMeta;
      if 0 < Length(Trim(TTableMeta(FDescr.FTableMeta).SelectSQL)) then
        FAllwaysJOIN:= True;
    end
  else
    begin
      FirstDataSet:= MetaData.GetTableByName(FDescr.Table, nil, True, FDescr.FSchema);
      FAllwaysJOIN:= (0 < Pos(' ', Trim(ADescr.Table)));
    end;

  PrepareSelectFields;
  PrepareFilterFields;
  PrepareSortFields;
  FinalizeFields;
end;

destructor TJoinBuilder.Destroy;
begin
  FLinks.Free;
  FNodes.Free;
  inherited Destroy;
end;

function TJoinBuilder.PrepareSequenceID(const SequenceID: Integer): string;
var
  Value: Integer;
begin
  Value := SequenceID;
  Result := Chr((Value mod 26) + 65);
  Value := Value div 26;
  if Value <> 0 then Result := Format('%s%u', [Result, Value]);
end;

procedure TJoinBuilder.PrepareSelectFields;
var
  Index, SourceIndex: Integer;
  Value, FieldName, JoinName, PriorJoinName, NewSourceName, SourceName: string;
  LinkMode: TLinkJoinMode;
  LinkFieldName: string;
  LinkDataSetObject: TObject;
  MainDataSet, LinkDataSet: TTableMeta;
begin
  if Assigned(FDescr) then
    for Index := Low(FDescr.FFields) to High(FDescr.FFields) do
      begin
        MainDataSet:= TTableMeta(FirstDataSet);
        SourceName := PrepareSequenceID(0);
        Value := FDescr.FFields[Index].TargetName;
        FieldName := CutTextValue(Value, '.');
        JoinName := EmptyStr;
        if FNodes.IndexOfName(JoinName) = -1 then FNodes.Append('=' + SourceName);
        while Length(Value) <> 0 do
          begin
            FDescr.DoLinkField( MainDataSet, FieldName, LinkMode, LinkDataSetObject, LinkFieldName);

            if assigned(LinkDataSetObject) and (LinkDataSetObject is TTableMeta) then
              LinkDataSet:= TTableMeta(LinkDataSetObject);

            if (LinkMode = lmReady) or ((LinkMode = lmDifferentDatabases) and (Length(LinkDataSet.Database.Database) <> 0)) then
              begin
                PriorJoinName := JoinName;
                if Length(JoinName) <> 0 then JoinName := JoinName + '.';
                JoinName := JoinName + FieldName;
                SourceIndex := FNodes.IndexOfName(JoinName);
                if SourceIndex = -1 then
                  begin
                    NewSourceName := PrepareSequenceID(FSequenceID);
                    SourceIndex := FNodes.Add(JoinName + '=' + NewSourceName);
                    if SourceIndex <> -1 then
                      begin
                                   FSQL := FSQL + ' LEFT JOIN ';

                            if Length(Trim(LinkDataSet.SelectSQL)) = 0
                              then FSQL := FSQL + FDescr.Calc.onTableToStr(LinkDataSet.Table, NewSourceName, LinkDataSet.Database.Database)
                              else FSQL := FSQL + '('+LinkDataSet.SelectSQL+') as '+ NewSourceName;

                                   FSQL := FSQL + ' ON ' + FDescr.Calc.onFieldToStr(LinkFieldName, EmptyStr, NewSourceName) + '=';

                            SourceIndex := FNodes.IndexOfName(PriorJoinName);
                            if SourceIndex <> -1
                              then FSQL := FSQL + FDescr.Calc.onFieldToStr(FieldName, EmptyStr, FNodes.ValueFromIndex[SourceIndex])
                              else FSQL := FSQL + FDescr.Calc.onFieldToStr(FieldName);


                        FSQL := FSQL + ' LEFT JOIN ' + FDescr.Calc.onTableToStr(LinkDataSet.Table, NewSourceName, LinkDataSet.Database.Database)
                                     + ' ON ' + FDescr.Calc.onFieldToStr(LinkFieldName, EmptyStr, NewSourceName) + '=';

                        Inc(FSequenceID);
                        SourceName := NewSourceName;
                      end
                    else
                      SourceName := EmptyStr;
                  end
                else
                  SourceName := FNodes.ValueFromIndex[SourceIndex];
                MainDataset := LinkDataset;
                FieldName := CutTextValue(Value, '.');
              end
            else
              begin
                FieldName := FDescr.FFields[Index].TargetName;
                SourceName := EmptyStr;
                Value := EmptyStr;
              end;
          end;
        FDescr.FFields[Index].SourceName := FieldName;
        FDescr.FFields[Index].Source := SourceName;
      end;
end;

procedure TJoinBuilder.PrepareSortFields;
var
  Index, SourceIndex: Integer;
  Value, FieldName, JoinName, PriorJoinName, NewSourceName, SourceName: string;
  LinkMode: TLinkJoinMode;
  LinkFieldName: string;
  LinkDataSetObject: TObject;
  MainDataSet, LinkDataSet: TTableMeta;
begin
  if Assigned(FDescr) then
    for Index := 0 to Pred(FDescr.FSortFields.Count) do
      begin
        MainDataSet:= TTableMeta(FirstDataSet);
        SourceName := PrepareSequenceID(0);
        Value := FDescr.FSortFields.Names[Index];
        FieldName := CutTextValue(Value, '.');
        JoinName := EmptyStr;
        if FNodes.IndexOfName(JoinName) = -1 then FNodes.Append('=' + SourceName);
        while Length(Value) <> 0 do
          begin
            FDescr.DoLinkField( MainDataSet, FieldName, LinkMode, LinkDataSetObject, LinkFieldName);

            if assigned(LinkDataSetObject) and (LinkDataSetObject is TTableMeta) then
              LinkDataSet:= TTableMeta(LinkDataSetObject);

            if (LinkMode = lmReady) or ((LinkMode = lmDifferentDatabases) and (Length(LinkDataSet.Database.Database) <> 0)) then
              begin
                PriorJoinName := JoinName;
                if Length(JoinName) <> 0 then JoinName := JoinName + '.';
                JoinName := JoinName + FieldName;
                SourceIndex := FNodes.IndexOfName(JoinName);
                if SourceIndex = -1 then
                  begin
                    NewSourceName := PrepareSequenceID(FSequenceID);
                    SourceIndex := FNodes.Add(JoinName + '=' + NewSourceName);
                    if SourceIndex <> -1 then
                      begin
                                   FSQL := FSQL + ' LEFT JOIN ';

                            if Length(Trim(LinkDataSet.SelectSQL)) = 0
                              then FSQL := FSQL + FDescr.Calc.onTableToStr(LinkDataSet.Table, NewSourceName, LinkDataSet.Database.Database)
                              else FSQL := FSQL + '('+LinkDataSet.SelectSQL+') as '+ NewSourceName;

                                   FSQL := FSQL + ' ON ' + FDescr.Calc.onFieldToStr(LinkFieldName, EmptyStr, NewSourceName) + '=';

                            SourceIndex := FNodes.IndexOfName(PriorJoinName);
                            if SourceIndex <> -1
                              then FSQL := FSQL + FDescr.Calc.onFieldToStr(FieldName, EmptyStr, FNodes.ValueFromIndex[SourceIndex])
                              else FSQL := FSQL + FDescr.Calc.onFieldToStr(FieldName);

                        Inc(FSequenceID);
                        SourceName := NewSourceName;
                      end
                    else
                      SourceName := EmptyStr;
                  end
                else
                  SourceName := FNodes.ValueFromIndex[SourceIndex];
                MainDataset := LinkDataset;
                FieldName := CutTextValue(Value, '.');
              end
            else
              begin
                FieldName := FDescr.FFields[Index].TargetName;
                SourceName := EmptyStr;
                Value := EmptyStr;
              end;
          end;
        FDescr.FSortFields[Index] := FieldName + '=' + SourceName;
      end;
end;

procedure TJoinBuilder.PrepareFilterFields;
var
  Index, SourceIndex: Integer;
  PostfixItem: TPostfixItem;
  Value, FieldName, JoinName, PriorJoinName, NewSourceName, SourceName: string;
  LinkMode: TLinkJoinMode;
  LinkFieldName: string;
  LinkDataSetObject: TObject;
  MainDataSet, LinkDataSet: TTableMeta;
begin
  {$IFDEF DEBUG}
  if Assigned(FDescr) and Assigned(FDescr.FFilter) then
    FDescr.FFilter.DebugPostfixLog(Format('TJoinBuilder.PrepareFilterFields for %s ...', [QuotedStr(FDescr.Table)]));
  {$ENDIF}
  if Assigned(FDescr) and Assigned(FDescr.FFilter) then
    for Index := 0 to Pred(FDescr.FFilter.Count) do
      begin
        PostfixItem := FDescr.FFilter[Index];
        if Assigned(PostfixItem) and (PostfixItem.ItemType = piIdent) then
          begin
            MainDataSet:= TTablemeta(FirstDataSet);
            SourceName := PrepareSequenceID(0);
            Value := PostfixItem.Ident;
            FieldName := CutTextValue(Value, '.');
            JoinName := EmptyStr;
            if FNodes.IndexOfName(JoinName) = -1 then FNodes.Append('=' + SourceName);
            while Length(Value) <> 0 do
              begin
                FDescr.DoLinkField( MainDataSet, FieldName, LinkMode, LinkDataSetObject, LinkFieldName);

                if assigned(LinkDataSetObject) and (LinkDataSetObject is TTableMeta) then
                  LinkDataSet:= TTableMeta(LinkDataSetObject);

                if (LinkMode = lmReady) or ((LinkMode = lmDifferentDatabases) and (Length(LinkDataSet.Database.Database) <> 0)) then
                  begin
                    PriorJoinName := JoinName;
                    if Length(JoinName) <> 0 then JoinName := JoinName + '.';
                    JoinName := JoinName + FieldName;
                    SourceIndex := FNodes.IndexOfName(JoinName);
                    if SourceIndex = -1 then
                      begin
                        NewSourceName := PrepareSequenceID(FSequenceID);
                        SourceIndex := FNodes.Add(JoinName + '=' + NewSourceName);
                        if SourceIndex <> -1 then
                          begin
                                   FSQL := FSQL + ' LEFT JOIN ';

                            if Length(Trim(LinkDataSet.SelectSQL)) = 0
                              then FSQL := FSQL + FDescr.Calc.onTableToStr(LinkDataSet.Table, NewSourceName, LinkDataSet.Database.Database)
                              else FSQL := FSQL + '('+LinkDataSet.SelectSQL+') as '+ NewSourceName;

                                   FSQL := FSQL + ' ON ' + FDescr.Calc.onFieldToStr(LinkFieldName, EmptyStr, NewSourceName) + '=';

                            SourceIndex := FNodes.IndexOfName(PriorJoinName);
                            if SourceIndex <> -1
                              then FSQL := FSQL + FDescr.Calc.onFieldToStr(FieldName, EmptyStr, FNodes.ValueFromIndex[SourceIndex])
                              else FSQL := FSQL + FDescr.Calc.onFieldToStr(FieldName);

                            Inc(FSequenceID);
                            SourceName := NewSourceName;
                          end
                        else
                          SourceName := EmptyStr;
                      end
                    else
                      SourceName := FNodes.ValueFromIndex[SourceIndex];
                    MainDataset := LinkDataset;
                    FieldName := CutTextValue(Value, '.');
                  end
                else
                  raise Exception.Create('Error in ' + ClassName + '.PrepareFilterFields(" '+Value+' ")');
              end;
            FLinks.AddObject(FieldName + '=' + SourceName, PostfixItem);
          end;
      end;
end;

procedure TJoinBuilder.FinalizeFields;
var
  Index: Integer;
begin
  if (FNodes.Count < 2) and (Not FAllwaysJOIN)then
    begin
      for Index := Low(FDescr.FFields) to High(FDescr.FFields) do
        FDescr.FFields[Index].Source := EmptyStr;
      for Index := 0 to Pred(FDescr.FSortFields.Count) do
        FDescr.FSortFields[Index] := FDescr.FSortFields.Names[Index] + '=';
      for Index := 0 to Pred(FLinks.Count) do
        FLinks[Index] := FLinks.Names[Index] + '=';
      FNodes.Clear;
    end;
end;

function TCustomQueryDescr.GetXML: string;
begin
  if Length(FXML) = 0 then FXML := PrepareXML;
  Result := FXML;
end;

function TCustomQueryDescr.PrepareXML: string;
const
  TypeNames: array[TDeQueryType] of PChar = ('select', 'row', 'insert', 'update', 'delete', 'hole');
var
  Value, XML: string;
begin
  Value := Table;
  if Length(Value) <> 0 then
    Value := ' table=' + QuotedStr(XMLEncode(Value));
  if Length(Schema) <> 0 then
    Value := Value + ' schema=' + QuotedStr(XMLEncode(Schema));
  Result := '<descriptor type="' + StrPas(TypeNames[QueryType]) + '"' + Value;
  Value := SQL;
  if Length(Value) <> 0 then Value := Value + '  <query>' + XMLEncode(Value) + '</query>';
  XML := PrepareFieldsXML;
  if Length(Value) <> 0 then
    begin
      if Length(XML) <> 0 then
        XML := XML + #13#10;
      XML := XML + Value;
    end;
  Value := PrepareSortFieldsXML;
  if Length(Value) <> 0 then
    begin
      if Length(XML) <> 0 then
        XML := XML + #13#10;
      XML := XML + Value;
    end;
  Value := PrepareParamsXML;
  if Length(Value) <> 0 then
    begin
      if Length(XML) <> 0 then
        XML := XML + #13#10;
      XML := XML + Value;
    end;
  if Length(XML) = 0 then
    Result := Result + ' />'
  else
    Result := Result + #13#10 + XML + '</descriptor>';
end;

function TCustomQueryDescr.PrepareFieldsXML: string;
var
  Index: Integer;
begin
  Result := EmptyStr;
  for Index := Low(FFields) to High(FFields) do
    begin
      if Length(Result) <> 0 then Result := Result + #13#10;
      Result := Result + PrepareFieldXML(FFields[Index]);
    end;
end;

function TCustomQueryDescr.PrepareFieldXML(FieldData: TFieldData): string;
var
  ValueStr: string;
begin
  Result := '  <field name=' + QuotedStr(XMLEncode(FieldData.TargetName));
  if FieldData.TargetType <> ftUnknown then
    Result := Result + ' type=' + QuotedStr(XMLEncode(StrPas(FieldTypeNames[FieldData.TargetType])));
  if FieldData.TargetSize <> 0 then
    Result := Result + ' size=' + QuotedStr(IntToStr(FieldData.TargetSize));
  if FieldData.TargetOperation <> opNone then
    Result := Result + ' operation=' + QuotedStr(XMLEncode(StrPas(OperationTypeNames[FieldData.TargetOperation])));
  if Not SameText(FieldData.SourceName, FieldData.TargetName) then
    Result := Result + ' original=' + QuotedStr(XMLEncode(FieldData.SourceName));
  if Length(FieldData.Source) <> 0 then
    Result := Result + ' source=' + QuotedStr(XMLEncode(FieldData.Source));
  if Length(FieldData.Parameter) <> 0 then
    Result := Result + ' parameter=' + QuotedStr(XMLEncode(FieldData.Parameter));
  ValueStr := PrepareVariantXML(FieldData.Value, 2);
  if Length(ValueStr) = 0 then
    Result := Result + ' />'
  else
    Result := Result + '>'#13#10 + ValueStr + #13#10'  </field>';
end;

function TCustomQueryDescr.PrepareVariantXML(const Value: Variant; const SpaceLevel: Integer): string;
var
  Index, LowIndex, HighIndex: Integer;
  ValueStr: string;
  ArrayType: Word;
begin
  if VarIsEmpty(Value) then
    Result := EmptyStr
  else if VarIsNull(Value) then
    Result := '<value type="null" />'
  else if VarIsType(Value, [varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord, varInt64]) then
    begin
      Result := '<value type="integer"';
      ValueStr := VarToStr(Value);
      if Length(ValueStr) = 0 then
        Result := Result + ' />'
      else
        Result := Result + '>' + XMLEncode(ValueStr) + '</value>';
    end
  else if VarIsType(Value, [varBoolean]) then
    Result := '<value type="boolean">' + XMLEncode(StrPas(BooleanNames[Value = True])) + '</value>'
  else if VarIsType(Value, [varDate]) then
    Result := '<value type="datetime">' + FormatDateTime('YYYYMMDD" "HH":"NN":"SS"."ZZZ', VarToDateTime(Value)) + '</value>'
  else if VarIsType(Value, [varSingle, varDouble]) then
    Result := '<value type="float">' + ReplaceText(FloatToStr(Value), FormatSettings.DecimalSeparator, '.') + '</value>'
  else if VarIsType(Value, varCurrency) then
    Result := '<value type="currency">' + ReplaceText(FormatFloat('#0.00', Value), FormatSettings.DecimalSeparator, '.') + '</value>'
  else if VarIsArray(Value) then
    begin
      LowIndex := VarArrayLowBound(Value, 1);
      HighIndex := VarArrayHighBound(Value, 1);
      Result := EmptyStr;
      for Index := LowIndex to HighIndex do
        begin
          ValueStr := PrepareVariantXML(VarArrayGet(Value, [Index]), SpaceLevel + 2);
          if Length(ValueStr) <> 0 then
            begin
              ValueStr := DupeString('  ', Succ(SpaceLevel)) + '<item index="' + IntToStr(Index) + '">'#13#10 + ValueStr + #13#10 + DupeString('  ', Succ(SpaceLevel)) + '</item>';
              Result := Result + ValueStr;
            end;
        end;
      ValueStr := EmptyStr;
      ArrayType := varUnknown;
      for Index := Succ(LowIndex) to HighIndex do
        if Length(ValueStr) = 0 then
          begin
            ArrayType := VarType(VarArrayGet(Value, [LowIndex]));
            case ArrayType of
              varBoolean: ValueStr := ' data="ftBoolean"';
              varByte: ValueStr := ' data="ftByte"';
              varShortInt: ValueStr := ' data="ftShortint"';
              varSmallint: ValueStr := ' data="ftSmallint"';
              varWord: ValueStr := ' data="ftWord"';
              varInteger: ValueStr := ' data="ftInteger"';
              varSingle: ValueStr := ' data="ftSingle"';
              varDouble: ValueStr := ' data="ftFloat"';
              varCurrency: ValueStr := ' data="ftCurrency"';
              varDate: ValueStr := ' data="ftDateTime"';
              varLongWord: ValueStr := ' data="ftLongWord"';
              varInt64,
              varUInt64: ValueStr := ' data="ftLargeint"';
              varUString: ValueStr := ' data="ftWideString"';
              varOleStr,
              varString: ValueStr := ' data="ftString"';
            else
              Break;
            end;
          end
        else
          if VarType(VarArrayGet(Value, [Index])) <> ArrayType then
            begin
              ValueStr := EmptyStr;
              Break;
            end;
      ValueStr := '<value type="array" low="' + IntToStr(LowIndex) + '" high="' + IntToStr(HighIndex) + '"' + ValueStr;
      if Length(Result) = 0 then
        Result := ValueStr + ' />'
      else
        Result := ValueStr + '>' + Result + '</value>';
    end
  else
    begin
      Result := '<value type="string"';
      ValueStr := VarToStr(Value);
      if Length(ValueStr) = 0 then
        Result := Result + ' />'
      else
        Result := Result + '>' + XMLEncode(ValueStr) + '</value>';
    end;
  if (SpaceLevel <> 0) and (Length(Result) <> 0) then
    Result := DupeString('  ', SpaceLevel) + Result;
end;

function TCustomQueryDescr.PrepareSortFieldsXML: string;
var
  Index: Integer;
begin
  Result := EmptyStr;
  for Index := 0 to Pred(SortCount) do
    begin
      if Length(Result) <> 0 then Result := Result + #13#10;
      Result := '  <sort field=' + QuotedStr(XMLEncode(SortField[Index]));
      case SortDirection[Index] of
        sdAscending: Result := Result + ' direction=' + QuotedStr('asc');
        sdDescending: Result := Result + ' direction=' + QuotedStr('desc');
      end;
      Result := Result + ' />'
    end;
end;

function TCustomQueryDescr.PrepareParamsXML: string;
var
  Index: Integer;
begin
  Result := EmptyStr;
  if Assigned(FParams) then
    for Index := 0 to Pred(FParams.Count) do
      begin
        if Length(Result) <> 0 then Result := Result + #13#10;
        Result := Result + PrepareParamXML(FParams[Index]);
      end;
end;

function TCustomQueryDescr.PrepareParamXML(VariableItem: TVariableItem): string;
var
  ValueStr: string;
begin
  if Assigned(VariableItem) then
    begin
      Result := '  <parameter name=' + QuotedStr(XMLEncode(VariableItem.Name));
      if VariableItem.DataType <> ftUnknown then
        Result := Result + ' type=' + QuotedStr(XMLEncode(StrPas(FieldTypeNames[VariableItem.DataType])));
      ValueStr := PrepareVariantXML(VariableItem.Value, 2);
      if Length(ValueStr) = 0 then
        Result := Result + ' />'
      else
        Result := Result + '>'#13#10 + ValueStr + #13#10'  </parameter>';
    end
  else
    Result := EmptyStr;
end;

{ TFieldData }

function TFieldData.GetSourceName: string;
begin
  if Length(FSourceName) = 0 then Result := TargetName
                             else Result := FSourceName;
end;

function TFieldData.GetSourceOperation: TOperationType;
begin
  if FSourceOperation = opNone then Result := TargetOperation
                               else Result := FSourceOperation;
end;

function TFieldData.GetSourceSize: Integer;
begin
  if FSourceSize = 0 then Result := TargetSize
                     else Result := FSourceSize;
end;

function TFieldData.GetSourceType: TFieldType;
begin
  if FSourceType = ftUnknown then Result := TargetType
                             else Result := FSourceType;
end;

function TFieldData.SourceAlias: string;
begin
  if (Length(FSourceName) = 0) and (TargetOperation = opNone) then Result:= EmptyStr
                                                              else Result:= SourceName;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('QueryDescriptor unit initialization ...');

finalization
  DebugLog('QueryDescriptor unit finalization ...');
{$ENDIF}

end.

