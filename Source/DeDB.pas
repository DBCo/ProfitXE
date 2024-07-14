unit DeDB;

interface

uses SysUtils, Classes, Variants, DB, Contnrs, {Windows, }
     DeTypes, QueryDescriptor, DeParser, DeVariable;

const
  meOk = 0;              // типы ошибок подключения к базе данных, включая ошибки метаструктуры
  meConnect = 1;
  meConnectDB = 2;
  meConnectServer = 3;
  meConnectPassword = 4;
  meConnectNetwork = 5;
  meConnectLibrary = 6;
  meEmpty = -7;
  meMetaEmpty = -8;
  meMetaBroken = -9;
  meMetaVersion = -10;

type
  // описание запроса для DeDataset'а (абстрагирование от SQL)
  TDeDataset = class;

  TObjectType = (otNone, otUnknown, otTable, otView, otStoredProc);

  TSupportDDL = (ddlCreateTable,
                 ddlCreateField,
                 ddlModifyField,
                 ddlDeleteField,
                 ddlSequence);
  TSupportDDLs = set of TSupportDDL;

  /// <summary>Права на операции с набором данных из базы данных</summary>
  TDeDatasetPermission =
    (
      dpSelect, // Разрешено чтение набора данных
      dpInsert, // Разрешено добавление строк в набор данных
      dpUpdate, // Разрешено обновление строк в наборе данных
      dpDelete  // Разрешено удаление строк в наборе данных
    );
  TDeDatasetPermissions = set of TDeDatasetPermission;

  TDeAsyncDatasetEvent = procedure(Sender: TObject; const RawValue: Variant) of object;
  TDeAsyncExecuteEvent = procedure(Sender: TObject; const ReturnCode: Integer) of object;
  TDeAsyncExceptionEvent = procedure(Sender: TObject; E: Exception) of object;

  // Абстрактый класс - предок всех типов баз данных Profite
  TDeCustomDataBase = class(TComponent)
  private
    FID                  : Variant;
    FAlias               : string;
    FConnectString       : string;
    FDatabaseType        : TDatabaseType;
    FReadOnly            : boolean;
    FServer              : string;
    FDatabase            : string;
    FLogin               : string;
    FPassword            : string;
    FConnectErrorMessage : string;
    FConnectErrorTime    : TDateTime;
    FQueriesList         : TComponentList;
    FCodePage            : Integer;
    FGUID                : Variant;
    function GetConnectString : string;
    procedure SetConnectString(const aConnectString : string);
    procedure SetDatabase(const aDatabase : string);
    procedure SetLogin(const aLogin : string);
    procedure SetPassword(const aPassword : string);
    function AddDataPath(const AddTo, GetFrom : string) : string;
  protected
    FCI                 : Boolean;
    FAI                 : Boolean;
    FDDL                : Boolean; // Флаг: можно ли выполнять скрипты создания/изменения таблиц/полей
    FCanODBC            : Boolean;
    FCanStoredProcedure : Boolean;
    function GetServer : string; virtual;
    function GetDatabase : string; virtual;
    function GetLogin : string; virtual;
    function GetPassword : string; virtual;
    procedure SetServer(const aServer : string);
    function GetReadOnly: boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation);  override;
    function BuildConnectString : string;  virtual;
    function GetConnected : boolean;  virtual;  abstract;
    procedure SetConnected(const aConnected : boolean);  virtual;
    function CreateQueryObject(const aQueryType: TDeQueryType; InDS: TDeDataset = nil): TDeDataset; virtual; abstract;
    function GetCI: Boolean; virtual;
    function GetAI: Boolean; virtual;
    function GetDDL: Boolean; virtual;
    function GetLoginSchema: string; virtual;
  public
    constructor Create(aOwner : TComponent);  override;
    destructor Destroy;  override;
    /// <summary>код базы данных</summary>
    property ID: Variant read FID write FID;
    /// <summary>псевдоним базы данных</summary>
    property Alias: string read FAlias write FAlias;
    /// <summary>путь к файлам базы данных</summary>
    property ConnectString: string read GetConnectString write SetConnectString;
    /// <summary>тип базы данных</summary>
    property DatabaseType : TDatabaseType read FDatabaseType write FDatabaseType;
    function isInternalDataBase: Boolean; Virtual;
    // =true, если база данных доступна только для чтения
    property IsReadOnly : boolean read GetReadOnly write FReadOnly;
    property CI: Boolean read GetCI;
    property AI: Boolean read GetAI;
    property DDL: Boolean read GetDDL;
    property Server : string read GetServer write SetServer;
    property Database : string read GetDatabase write SetDatabase;
    property Login : string read GetLogin write SetLogin;
    property Password : string read GetPassword write SetPassword;
    // =true, если класс подключен к реальному хранилищу
    property Connected : boolean read GetConnected write SetConnected;
    // текст сообщения об ошибке при подключении
    property ConnectErrorMessage : string read FConnectErrorMessage;
    // пробует подключиться к базе данных; возвращает true, если подключиться удалось
    function CheckConnection: Boolean;
    // начитывает заголовки Meta описаний таблиц
    procedure GetMetaTables(aObjectList: TObjectList);
    procedure GetMetaTableInfo(aTable : TObject);
    // выгружает содержимое BLOB-поля в поток
    procedure UploadBLOB(const aTableName, aKeyFieldName, aBlobFieldName : string;
      const aKeyValue:Variant;  aUploadTo:TStream);  virtual;  abstract;
    // загружает содержимое BLOB-поля из потока;  если передан NIL, то очищает
    // содержимое BLOB-поля
    procedure LoadBLOB(const aTableName, aKeyFieldName, aBlobFieldName : string;
      const aKeyValue:Variant;  aLoadFrom:TStream);  virtual;  abstract;
    function CreateQuery(aQueryType : TDeQueryType; InDS: TDeDataset = nil): TDeDataset; virtual;

    /// <summary>Функция создаёт таблицу в базе данных</summary>
    /// <param name="Source">объект с данными о создаваемой таблице</param>
    /// <returns>True - таблица создана в базе данных успешно</returns>
    function CreateTable(Source: TObject): Boolean; virtual;
    /// <summary>Функция создаёт поле для таблицы в базе данных</summary>
    /// <param name="Source">объект с данными о создаваемом столбце</param>
    /// <returns>True - поле для таблицы создано в базе данных успешно</returns>
    function CreateField(Source: TObject): Boolean; virtual;
    /// <summary>Функция изменяет поле для таблицы в базе данных</summary>
    /// <param name="Source">объект с данными о изменяемом столбце</param>
    /// <returns>True - поле для таблицы изменено в базе данных успешно</returns>
    function ModifyField(Source: TObject): Boolean; virtual;
    /// <summary>Функция удаляет поле из таблицы в базе данных</summary>
    /// <param name="Source">объект с данными о удаляемом столбце</param>
    /// <returns>True - поле из таблицы удалено в базе данных успешно</returns>
    function DeleteField(Source: TObject): Boolean; virtual;

    property CanODBC : Boolean read FCanODBC;
    property CanStoredProcedure : Boolean read FCanStoredProcedure;
    /// <summary>Функция проверяет доступность выполнения порционного запроса</summary>
    /// <returns>True - поддерживается частичная загрузка данных</returns>
    function CanPortionSelect: Boolean; dynamic;

    function ConnectStringODBC(const IncludeAutorisation: Boolean): string; virtual;
    function ExecuteStoredProcedure(const aProcedureName: string; aList: TDeVariableList): Integer; virtual;
    procedure RetrieveTableNames(aTablesList: TStringList); virtual; abstract;
    procedure RetrieveMetaTables(aTablesList: TObjectList); virtual; abstract;
    procedure RetrieveMetaTableInfo(aTable : TObject);  virtual;  abstract;
    procedure RetrieveProcedures(aProceduresList : TObjectList); virtual;
    procedure RetrieveProcedureInfo(aTable : TObject); virtual;
    /// <summary>Функция получает параметры для указанной хранимой процедуры</summary>
    /// <param name="ProcedureName">имя хранимой процедуры</param>
    /// <param name="ParameterList">список параметров хранимой процедуры</param>
    /// <returns>True - параметры процедуры получены в список</returns>
    function RetrieveProcedureParameters(const ProcedureName: string; ParameterList: TObject): Boolean; virtual;
    function RetrieveProcedureExecQuery(const ProcedureName: string; ParameterList: TObject): String; virtual;
    /// <summary>Функция проверяет наличие в базе данных указанной хранимой процедуры</summary>
    /// <param name="StoredProcedureName">имя хранимой процедуры</param>
    /// <returns>True - хранимая процедура есть в базе данных</returns>
    function StoredProcedureExists(const StoredProcedureName: string): Boolean; dynamic;
    /// <summary>Функция работает с Sequence</summary>
    /// <param>aQueryType = stInsert - создает Sequence </param>
    /// <param>aQueryType = stDelete - удаляет Sequence </param>
    /// <param>aQueryType = stUpdate - устанавливает новое значение, должно быть задано aValue </param>
    /// <param>aQueryType = stHaving - возвращает наличие Sequence с указанным именем </param>
    function Sequence(const aType: TDeSequenceType; const aTableName, aFieldName: String): Boolean; virtual; abstract;
    function Autoincrement(const aType: TDeSequenceType; const aTableName, aFieldName: String): Boolean; virtual; abstract;
    /// <summary>Функция получает тип объекта в базе данных</summary>
    /// <returns>
    /// <param>otNone - база данных не может определять тип объектов</param>
    /// <param>otUnknown - объект не найден</param>
    /// <param>otTable - таблица в базе данных</param>
    /// <param>otView - выборка в базе данных</param>
    /// <param>otStoredProc - хранимая процедура в базе данных</param>
    /// </returns>
    function RetrieveMetaObjectType(const ObjectName: string): TObjectType; dynamic;
    // v. 17.01
    property CodePage: Integer read FCodePage write FCodePage;
    /// <summary>Функция проверяет наличие поля у таблицы</summary>
    /// <param name="TableName">имя таблицы</param>
    /// <param name="FieldName">имя поля</param>
    /// <returns>True - Поле существует у таблицы</returns>
    function FieldExists(const TableName, FieldName: string): Boolean; dynamic;
    /// <summary>Функция возвращает множество поддерживаемых базой данных типов запросов</summary>
    class function SupportQueryTypes: TDeQueryTypes; dynamic;
    /// <summary>Функция проверяет базу данных на наличие метаструктурных таблиц</summary>
    /// <returns>True - база данных метаструктуры</returns>
    function CheckMetaConnection(var ErrorCode: Integer; var ErrorText: String): Boolean; dynamic;
    /// <summary>Функция проверяет тип поля на необходимость конвертации данных для работы с ними</summary>
    /// <param name="LocalType">тип проверяемых данных поля</param>
    /// <param name="RealType">новый тип для конвертации данных поля</param>
    /// <param name="RealSize">размерность нового типа поля для конвертации данных</param>
    /// <returns>True - нужна конвертация данных</returns>
    function IsFieldTypeConvert(const LocalType: TFieldType; const LocalSize: Integer;
                                var RealType: TFieldType; var RealSize: Integer): Boolean; dynamic;
    procedure TargetToSourceFieldType(var aFieldData: TFieldData); virtual;
    property InternalConnectString: string read FConnectString; // v. 17.07
    // v. 18.09
    /// <summary>Функция возвращает DDL (Data Definition Language), поддерживаемые базой данных</summary>
    function SupportDDLs: TSupportDDLs; dynamic;
    // v.19.06
    /// <summary>Получение прав на операции с набором данных из базы данных</summary>
    /// <param name="DatasetName">имя таблицы для получения прав</param>
    /// <returns>Функция возвращает множество из спика <c>TDeDatasetPermission</c></returns>
    function DatasetPermissions(const DatasetName: string): TDeDatasetPermissions; dynamic;
    // v.19.06
    /// <summary>Получение данных из базы данных асинхронно</summary>
    function AsyncOpen(const AQueryText: string; AOnDataset: TDeAsyncDatasetEvent; AOnException: TDeAsyncExceptionEvent = nil): Boolean; dynamic;
    /// <summary>Выполнение запроса в базе данных асинхронно</summary>
    function AsyncExecute(const AQueryText: string; AOnExecute: TDeAsyncExecuteEvent; AOnException: TDeAsyncExceptionEvent = nil): Boolean; dynamic;
    // v.19.07
    property VariantGUID: Variant read FGUID write FGUID;
    // v.19.08
    /// <summary>Функция проверяет доступность схем в базе данных</summary>
    /// <returns>True - поддерживаются схемы</returns>
    function CanSchema: Boolean; dynamic;
    /// <summary>Функция получения схемы по умолчанию для авторизированного пользователя в базе данных</summary>
    /// <returns>Схема по умолчанию для авторизированного пользователя</returns>
    property LoginSchema: string read GetLoginSchema;
    // v.21.12
    /// <summary>Функция получения даты и времени на сервере базе данных</summary>
    /// <param name="DateTime">Дата и время на сервере</param>
    /// <returns>True - сервер вернул данные</returns>
    function ReadServerDateTime(var DateTime: TDateTime): Boolean; dynamic;

    /// <summary>функция преобразования имени таблицы к строке по правилам БД</summary>
    function TableToStr(const tableName: String; const tableAlias: String = ''; const tableDatabase: String = '';
                        const tableSchema: String = ''): string; virtual;
    /// <summary>функция преобразования имени поля к строке по правилам БД</summary>
    function FieldToStr(const fieldName: String; const fieldAlias: String = ''; const tableAlias: String = '';
                        const fieldOperation: TOperationType = opNone; const DataType: TFieldType = ftUnknown; const DataSize: Integer = 0): string; virtual;
    /// <summary>функция преобразования значения к строке по правилам БД</summary>
    function ConstToStr(const aValue: Variant; const aType: TFieldType): string; virtual;
    /// <summary>функция преобразования фунции с параметрами к строке по правилам БД</summary>
    function FuncToStr(aFunction: string; aParams: array of variant): string; virtual;
  end;

  /// <summary>Класс пустой базы данных</summary>
  /// <remarks>В данной базе данных НИЧЕГО нет и если в метаструктуре описаны таблицы для такой базы данных - они будут пустыми!!!</remarks>
  TDeEmptyDatabase = class(TDeCustomDatabase)
  private
    FConnected: Boolean;
  protected
    function GetConnected: Boolean; override;
    procedure SetConnected(const Value: Boolean); override;
    procedure RetrieveTableNames(aTablesList: TStringList); override;
    procedure RetrieveMetaTables(aTablesList: TObjectList); override;
    procedure RetrieveMetaTableInfo(aTable : TObject);  override;
    function CreateQueryObject(const aQueryType: TDeQueryType; InDS: TDeDataset = nil): TDeDataset; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function SupportQueryTypes: TDeQueryTypes; override;
    function FieldExists(const TableName, FieldName: string): Boolean; override;
    procedure UploadBLOB(const TableName, KeyFieldName, BlobFieldName: string; const KeyValue: Variant; Stream: TStream); override;
    procedure LoadBLOB(const TableName, KeyFieldName, BlobFieldName: string; const KeyValue: Variant; Stream: TStream); override;
    // v.19.06
    function DatasetPermissions(const DatasetName: string): TDeDatasetPermissions; override;
  end;

  TDeQueryStateType = (qsInitialized, qsPrepared);
  TDeQueryState = set of TDeQueryStateType;

  // Абстрактный класс-предок для всех типов Dataset'ов Profite'a
  TDeDataset = class(TComponent)
  private
    FQueryDescr       : TCustomQueryDescr;
    FOnBeforeOpen     : TNotifyEvent;
    FOnAfterOpen      : TNotifyEvent;
    FState            : TDeQueryState;
    procedure SetQueryType(aQueryType : TDeQueryType);
    procedure QueryDescrChanged(Sender : TObject);
    procedure QueryDescrFinished(Sender : TObject);
    procedure SetDatabase(const Value: TDeCustomDatabase);
  protected
    FDatabase  : TDeCustomDatabase;
    FSourceSet : TComponent;
    FCurrentIndex : Integer;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function NewDescriptor(const aQueryType: TDeQueryType): TCustomQueryDescr; virtual;
    procedure CreateDescriptor(const aQueryType: TDeQueryType); virtual;
    function GetActive : boolean;  virtual;  abstract;
    function Initialize : boolean;  virtual;
    function GetRecordCount : integer; virtual;
    // возвращает значение поля с указанным номером
    function GetValue(const Index: Integer): Variant; virtual; abstract;
    // устанавливает значение поля с указанным номером
    procedure SetValue(const Index: Integer; const Value: Variant); virtual; abstract;
    // по имени поля возвращает его значение
    function GetValueByName(const aName : string) : Variant;
    procedure SetValueByName(const aName : string;  const aValue : Variant);
    function GetRecNo: Integer; virtual;
    procedure SetRecNo(const Value: Integer); virtual; abstract;
    procedure DoOpen;  virtual;
    procedure DoClose;  virtual;
    // закачивает записи на сторону клиента
    function FetchAll : boolean;  virtual; abstract;
    procedure BeforeOpen;  virtual;
    procedure AfterOpen;  virtual;
    procedure LinkField(Sender: TObject; const aDataSet: TObject; const FieldName: string; var LinkMode: TLinkJoinMode; var LinkDataset: TObject; var LinkFieldName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;  override;
    // нужны для datasets для построения индека фильтра и сортировки
    function Internal_Count: Integer; virtual; abstract;
    function Internal_Goto(Const aIndex: Integer): Boolean; virtual; abstract;
    function Internal_GetIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean; virtual; abstract;
    property QueryType : TDeQueryType write SetQueryType;
    property Database : TDeCustomDatabase read FDatabase write SetDatabase;
    property SourceSet: TComponent read FSourceSet;
    property Descr : TCustomQueryDescr read FQueryDescr;  // описание запроса
    property State : TDeQueryState read FState;             // состояние запроса
    property Active : boolean read GetActive;               // =true, если запрос активен
    property RecordCount : integer read GetRecordCount;     // количество записей набора
    // возвращает индех поля по имени
    function IndexByName(const Name: string): Integer; virtual; abstract;
    // доступ к значению поля по номеру
    property Value[const Index: Integer]: Variant read GetValue write SetValue;
    // доступ к значению поля по имени
    property ValueByName[const Name: string]: Variant read GetValueByName write SetValueByName;
    function ValueByNameDef(const aName : string; aDefaultValue: Variant) : Variant;
    function StringValue(const aIndex: Integer; aCodePage: Integer = -1): String; virtual;
    function StringValueByName(const aName : string; aCodePage: Integer = -1) : String;
    function StringValueByNameDef(const aName : string; aDefaultValue : String; aCodePage: Integer = -1) : String;
    function IntValueDef(const aIndex: Integer; aDefault: Int64 = 0): Int64;
    function IntValueByNameDef(const aName: string; aDefault: Int64 = 0): Int64;
    property OnBeforeOpen : TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnAfterOpen : TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    // номер активной записи набора данных
    property RecNo: Integer read GetRecNo write SetRecNo;
    // подготавливает запрос для выполнения
    procedure Prepare;  virtual;  abstract;
    // устанавливает режим блочного чтения
    procedure BlockReadMode;   virtual;  //abstract;
    // снимает режим блочного чтения
    procedure UnBlockReadMode;  virtual;  //abstract;
    // открывает набор данных
    procedure Open(const ARetrieveAll: Boolean = False);
    // закрывает набор данных
    procedure Close;
    // выполняет запрос
    function ExecuteQuery : boolean;  virtual;
    // вставляет запись в набор данных
    function InsertRecord(aRecord : TObject) : boolean;  virtual;  abstract;
    // обновляет запись в наборе данных
    function UpdateRecord(aRecord : TObject) : boolean;  virtual;  abstract;
    // удаляет запись из набора данных
    function DeleteRecord(aRecord : TObject) : boolean;  virtual;  abstract;
    // возвращает максимальное значение первичного ключа
    function GetMaxKeyValue(const MetaID: Integer = -1): Variant; virtual; abstract;
    // навигационные методы
    function Locate(const KeyFields: string; const KeyValues: Variant; const Options: TLocateOptions): Boolean; virtual; abstract;
    // получение метаописания полей набора данных
    procedure RetrieveFields(aFields: TObjectList; aTableMeta: TObject = nil);  virtual;  abstract;
    /// <summary>Наполнение набора данных полями на основании метаописания полей</summary>
    /// <param name="aFields">Список полей метаописания</param>
    /// <param name="Stage">Часть читаемых полей</param>
    procedure CreateFields(aFields: TObjectList; const Stage: TFieldStage); virtual; abstract;
    /// <summary>Получение типа данных для поля</summary>
    /// <param name="FieldName">Имя поля</param>
    /// <returns>Функция возвращает тип данных поля в наборе данных или ftUnknown в случае невозможности его определения</returns>
    function IndexTFieldByName(const FieldName: string; var aField: TField): Integer; virtual;
    {$IFDEF DEBUG}
    procedure DebugDumpLog(const FileName: string); dynamic;
    {$ENDIF}
    // v.19.06
    function AsyncOpen(AOnDataset: TDeAsyncDatasetEvent; AOnException: TDeAsyncExceptionEvent = nil): Boolean;
    function AsyncExecute(AOnExecute: TDeAsyncExecuteEvent; AOnException: TDeAsyncExceptionEvent = nil): Boolean;
  end;

  TDeIndexedDataset = class(TDeDataset)
  protected
    FInternalIndex: Integer;

    IND_UseIndex: Boolean;
    IND_Count: Integer;
    IND_Records: array of Integer;
    function IND_External2Internal(const aIndex: Integer): Integer; inline;
    function IND_Internal2External(const aIndex: Integer): Integer; inline;
    procedure IND_Clear;
    procedure IND_Build;

    function GetRecordCount : integer; override;
    function GetValue(const Index: Integer): Variant; override;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
  end;

  /// <summary>Класс пустого набора данных</summary>
  /// <remarks>Используется в <see cref="TDeEmptyDatabase">TDeEmptyDatabase</see>.</remarks>
  TDeEmptyDataset = class(TDeDataset)
  private
    FActive: Boolean;
    procedure LogAfterOpen(Descr: TCustomQueryDescr);
    //function GetIdentValue(const Ident: string; DataObject: Pointer): Variant;
  protected
    function GetActive: Boolean; override;
    function GetRecordCount: Integer; override;
    function GetValue(const Index: Integer): Variant; override;
    procedure SetValue(const Index: Integer; const Value: Variant); override;
    procedure DoOpen; override;
    procedure DoClose; override;
  public
    function ExecuteQuery: Boolean; override;
    function InsertRecord(ARecord: TObject): Boolean; override;
    function UpdateRecord(ARecord: TObject): Boolean; override;
    function DeleteRecord(ARecord: TObject): Boolean; override;
    function GetMaxKeyValue(const MetaID: Integer = -1): Variant; override;
    procedure Prepare; override;
    function FetchAll: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; const Options: TLocateOptions): Boolean; override;
    procedure RetrieveFields(Fields: TObjectList; TableMeta: TObject = nil); override;
    procedure CreateFields(Fields: TObjectList; const Stage: TFieldStage); override;
    // по имени поля возвращает его номер
    function IndexByName(const Name: string): Integer; override;
  end;

  // список баз данных вместе с подключениями
  TConnectionList = class(TComponentList)
  private
    // возвращает класс подключения в зависимости от типа базы данных
    function Get(const Index: Integer): TDeCustomDataBase;
    procedure Put(const Index: Integer; Database: TDeCustomDataBase);
  public
    //constructor Create;
    procedure Clear; override;
    property Items[const Index: Integer]: TDeCustomDataBase read Get write Put; default;
    {$IFDEF DEBUG}
    procedure DebugConnectionsLog(const Text: string);
    {$ENDIF}
  end;

  EDeDatasetError = class(Exception);

var
  LastErrorDBType   : TDatabaseType = dtNone;
  CurrentIBVersion  : Integer = 0;
  CurrentBDEVersion : Integer = 0;
  CurrentORAVersion : string = '';

implementation

uses Types, StrUtils, Windows, Registry, Math, DeLog, {DataUnit, } DeMeta,
  Funcs, DataCacheUnit, DeDataset, DeCalculator, DeMetaData
  {$IFDEF DEBUG}, uTextTable{$ENDIF};

{ TDeCustomDatabase }

constructor TDeCustomDatabase.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FQueriesList := TComponentList.Create;
  FConnectErrorMessage:= EmptyStr;
  FConnectErrorTime   := Null;
  FCanODBC            := False;
  FCanStoredProcedure := False;
  FCI                 := False;
  FAI                 := False;
end;

destructor TDeCustomDatabase.Destroy;
begin
  Connected := false;
  FreeAndNil(FQueriesList);
  inherited Destroy;
end;

function TDeCustomDatabase.GetConnectString : string;
begin
  if Length(FConnectString) = 0 then
    result := BuildConnectString
  else
    result := FConnectString;
end;

procedure TDeCustomDatabase.SetConnectString(const aConnectString : string);
begin
  if CompareText(FConnectString, aConnectString) <> 0 then
  begin
    Connected := false;
    FConnectString := aConnectString;
    FConnectErrorMessage:= EmptyStr;
    FConnectErrorTime:= Null;
  end;
end;

function TDeCustomDataBase.GetServer : string;
begin
  Result:=FServer;
  if Assigned(Metadata.MetadataDB) and (Metadata.MetadataDB <> Self) then
    Result := StringReplace(Result, '*', Metadata.MetadataDB.Server, [] );
end;


function TDeCustomDataBase.GetDatabase : string;
begin
  Result:=FDatabase;
  if Assigned(Metadata.MetadataDB) and (Metadata.MetadataDB <> Self) then
    Result := StringReplace(Result, '*', Metadata.MetadataDB.FDatabase, [] );
end;

function TDeCustomDataBase.GetLogin : string;
begin
  Result:=FLogin;
  if Assigned(Metadata.MetadataDB) and (Metadata.MetadataDB <> Self) then
    Result := StringReplace(Result, '*', Metadata.MetadataDB.FLogin, [] );
end;

function TDeCustomDataBase.GetLoginSchema: string;
begin
  Result := EmptyStr; // По умолчанию схема не определена!
end;

function TDeCustomDataBase.GetPassword : string;
begin
  Result:=FPassword;
  // если задан пользователь и не задан пароль, то подставляем пароль на мета-базу
  if Assigned(Metadata.MetadataDB) and (Metadata.MetadataDB <> Self) then
    if Not(Trim(FLogin)=EmptyStr) and ((Trim(FPassword)=EmptyStr) or (Trim(FPassword)='*')) then
      Result := Metadata.MetadataDB.FPassword;
end;

function TDeCustomDataBase.GetReadOnly: boolean;
begin
  Result := FReadOnly;
end;

procedure TDeCustomDataBase.SetServer(const aServer: string);
begin
  if FServer <> aServer then
  begin
    Connected := false;
    FServer := aServer;
    FConnectErrorMessage:= EmptyStr;
    FConnectErrorTime:= null;
  end;
end;

procedure TDeCustomDataBase.SetDatabase(const aDatabase: string);
begin
  if FDatabase <> aDatabase then
  begin
    Connected := false;
    FDatabase := aDatabase;
    FConnectErrorMessage:= EmptyStr;
    FConnectErrorTime:= null;
  end;
end;

procedure TDeCustomDatabase.SetLogin(const aLogin : string);
begin
  if FLogin <> aLogin then
  begin
    Connected := false;
    FLogin := aLogin;
    FConnectErrorMessage:= EmptyStr;
    FConnectErrorTime:= null;
  end;
end;

procedure TDeCustomDatabase.SetPassword(const aPassword : string);
begin
  if FPassword <> aPassword then
  begin
    Connected := false;
    FPassword := aPassword;
    FConnectErrorMessage:= EmptyStr;
    FConnectErrorTime:= null;
  end;
end;

function TDeCustomDatabase.AddDataPath(const AddTo, GetFrom : string) : string;
var PathSplitter : char;
    s            : string;
begin
  result := AddTo;
  PathSplitter := #0;
  if Pos('\', GetFrom) > 0 then
    PathSplitter := '\'
  else if Pos('/', GetFrom) > 0 then
    PathSplitter := '/';
  if PathSplitter <> #0 then
  begin
    s:= GetFrom;
    while (Length(s)>0) and (s[Length(s)]<>PathSplitter) do
      Delete(s,Length(s),1);
    while Pos(PathSplitter, result)>0 do
      Delete(result, 1, Pos(PathSplitter, result));
    result := s + result;
  end;
end;

procedure TDeCustomDatabase.SetConnected(const aConnected : boolean);
begin
  inherited;
end;

function TDeCustomDatabase.GetCI : boolean;
begin
  Result:=FCI;
end;

function TDeCustomDatabase.GetAI : boolean;
begin
  Result:=FAI;
end;

function TDeCustomDataBase.GetDDL: Boolean;
begin
  Result := FDDL;
end;

function TDeCustomDatabase.CheckConnection: boolean;
begin
  if Not VarIsNull(FConnectErrorTime) then
    if ((Now-FConnectErrorTime) < 20/(24*60*60)) and (0 < Length(FConnectErrorMessage)) then
      Exit(False);

  try
    Connected := True;
    FConnectErrorMessage := EmptyStr;
    FConnectErrorTime:= Null;
    Result := Connected;
  except
    on E:Exception do
      begin
        Connected := False;
        FConnectErrorMessage := E.Message;
        FConnectErrorTime:= Now;
        Result := False;
      end;
  end;
end;

procedure TDeCustomDatabase.GetMetaTables(aObjectList: TObjectList);
begin
  // Если есть список таблиц, то ...
  if Assigned(aObjectList) then
    RetrieveMetaTables(aObjectList);
end;

procedure TDeCustomDatabase.GetMetaTableInfo(aTable : TObject);
var i: Integer;
begin
  // Если передано не метатаблица, то ошибка
  if Not (aTable is TTableMeta) then
    raise Exception.Create('GetMetaTableInfo: ParamType not TTableMeta');
  if Connected then
    RetrieveMetaTableInfo(aTable);

  TTableMeta(aTable).Assign(self);
  TTableMeta(aTable).IsDynamic:= True;

  for i:= 0 to Pred(TTableMeta(aTable).Fields.Count) do
    if TTableMeta(aTable).Fields[i].Key then
      TTableMeta(aTable).KField.Add(TTableMeta(aTable).Fields[i]);
end;

procedure TDeCustomDataBase.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Assigned(FQueriesList) then
    FQueriesList.Extract(aComponent);
  inherited Notification(aComponent, Operation);
end;

function TDeCustomDataBase.BuildConnectString : string;
begin
  result := Database;
end;

function TDeCustomDataBase.CreateQuery(aQueryType: TDeQueryType; InDS: TDeDataset = nil): TDeDataset;
begin
  result := CreateQueryObject(aQueryType, InDS);
  result.QueryType := aQueryType;
  result.Database := Self;
  if Assigned(result) then
    FQueriesList.Add(result);
end;

function TDeCustomDataBase.ConnectStringODBC(const IncludeAutorisation: Boolean): string;
begin
  Result := EmptyStr;
end;

function TDeCustomDataBase.ExecuteStoredProcedure(const aProcedureName: string; aList: TDeVariableList): Integer;
begin
  raise Exception.Create('DataBase not support [ExecuteStoredProcedure]')
end;

function TDeCustomDataBase.FieldExists(const TableName, FieldName: string): Boolean;
begin
                   // Метод должен быть перекрыт в наследниках!!!
  Result := False; // По умолчению поля не существует
end;

function TDeCustomDataBase.CreateTable(Source: TObject): Boolean;
begin
  Result := False;
  // Зачем строка ниже - не понятно! Закомментил, т.к. сама функция возвращает True только когда объект создан!
  //raise Exception.Create('DataBase not support [CreateTable]')
end;

function TDeCustomDataBase.CreateField(Source: TObject): Boolean;
begin
  Result := False; // Метод должен перекрываться в наследниках!
end;

function TDeCustomDataBase.ModifyField(Source: TObject): Boolean;
begin
  Result := False; // Метод должен перекрываться в наследниках!
end;

function TDeCustomDataBase.DatasetPermissions(const DatasetName: string): TDeDatasetPermissions;
begin
  if Length(DatasetName) = 0 then
    Result := []
  else
    Result := [dpSelect, dpInsert, dpUpdate, dpDelete]; // Права по умолчанию для наборов данных. Можно переопределить в наследниках.
end;

function TDeCustomDataBase.DeleteField(Source: TObject): Boolean;
begin
  Result := False; // Метод должен перекрываться в наследниках!
end;

procedure TDeCustomDataBase.RetrieveProcedures(aProceduresList : TObjectList);
begin
  raise Exception.Create('DataBase not support procedure ExecuteStoredProcedure')
end;

procedure TDeCustomDataBase.RetrieveProcedureInfo(aTable : TObject);
begin
//
end;

function TDeCustomDataBase.RetrieveProcedureParameters(const ProcedureName: string; ParameterList: TObject): Boolean;
begin
  // Должна переопределяться в наследниках ...
  Result := False;
end;

function TDeCustomDataBase.RetrieveProcedureExecQuery(const ProcedureName: string; ParameterList: TObject): String;
begin
  // Может переопределяться в наследниках ...
  Result:= EmptyStr;
end;

function TDeCustomDataBase.StoredProcedureExists(const StoredProcedureName: string): Boolean;
begin
  // Должна переопределяться в наследниках ...
  Result := False;
end;

function TDeCustomDataBase.SupportDDLs: TSupportDDLs;
begin
  Result := []; // Базовый класс не поддерживает DDL!
end;

class function TDeCustomDataBase.SupportQueryTypes: TDeQueryTypes;
begin
  Result := [qtSelect, qtInsert, qtUpdate, qtDelete, qtRow];
end;

function TDeCustomDataBase.TableToStr(const tableName, tableAlias, tableDatabase, tableSchema: String): string;
begin
  if (Length(tableAlias) = 0) and (Length(tableDatabase) = 0) and (Length(tableSchema) = 0) then
    Result:= tableName
  else
    raise Exception.Create('Abstract Error in ' + ClassName + '.TableToStr');
end;

procedure TDeCustomDataBase.TargetToSourceFieldType(var aFieldData: TFieldData);
begin
  aFieldData.SourceType:= aFieldData.TargetType;
  aFieldData.SourceSize:= aFieldData.SourceSize;
end;

function TDeCustomDataBase.FieldToStr(const fieldName, fieldAlias, tableAlias: String;
  const fieldOperation: TOperationType; const DataType: TFieldType; const DataSize: Integer): string;
begin
  if (Length(fieldAlias) = 0) and (Length(tableAlias) = 0) and (fieldOperation = opNone) then
    Result:= fieldName
  else
    raise Exception.Create('Abstract Error in ' + ClassName + '.FieldToStr');
end;

function TDeCustomDataBase.ConstToStr(const aValue: Variant; const aType: TFieldType): string;
var H,M,S,X : Word;
begin
  if VarIsEmpty(aValue) then Result:='null' else
  if aType in NumericTypes then Result := VarToStr(aValue) else
  if aType in FloatTypes then Result := StringReplace(VarToStr(aValue),',','.',[]) else
  if aType in StringTypes then Result := ''''+VarToStr(aValue)+'''' else
  if aType in LogicalTypes then  Result := '''' + VarToStr(aValue) + '''' else
  if aType in [ftTime] then begin
                              DecodeTime(aValue, H, M, S, X);
                              if (X=0)
                                   then Result := FormatDateTime('hh:nn:ss', aValue)
                                   else Result := FormatDateTime('hh:nn:ss.zzz', aValue);
                            end else
  if aType in DateTimeTypes then begin
                                   DecodeTime(aValue, H, M, S, X);
                                   if (H=0) and (M=0) and (S=0) and (X=0) and false
                                     then Result := FormatDateTime('YYYYMMDD', aValue)
                                     else Result := FormatDateTime('YYYYMMDD hh:nn:ss', aValue);
                                 end else
                                 Result := '''' + VarToStr(aValue) + '''';
end;

function TDeCustomDataBase.FuncToStr(aFunction: string; aParams: array of variant): string;
begin
  Result:= EmptyStr;
  raise Exception.Create('Abstract Error in ' + ClassName + '.FuncToStr');
end;

function TDeCustomDataBase.CanPortionSelect: Boolean;
begin
  Result := False; // По умолчанию базы данных не могут читать данные порциями!
end;

function TDeCustomDataBase.CanSchema: Boolean;
begin
  Result := False; // По умолчанию у баз данных схема не поддерживается
end;

function TDeCustomDataBase.ReadServerDateTime(var DateTime: TDateTime): Boolean;
begin
  Result := True;
  DateTime := Now; // По умолчанию дата и время компьютера с запущенным приложением.
end;

function TDeCustomDataBase.RetrieveMetaObjectType(const ObjectName: string): TObjectType;
begin
  Result := otNone; // По умолчанию база не может определять типы объектов!
end;

function TDeCustomDataBase.IsFieldTypeConvert(const LocalType: TFieldType; const LocalSize: Integer;
                                              var RealType: TFieldType; var RealSize: Integer): Boolean;
begin
  RealType := ftUnknown;
  RealSize := 0;
  Result := False; // Конвертация не требуется по умолчанию ...
end;

function TDeCustomDataBase.isInternalDataBase: Boolean;
begin
  Result:= False;
end;

function TDeCustomDataBase.CheckMetaConnection(var ErrorCode: Integer; var ErrorText: String): Boolean;
const
  NeedTables: Array of string = [tblUsers, tblBase, tblDataset, tblFields, tblMenu];
var
  aTableList: TStringList;
  N, Index: Integer;
begin
  ErrorCode:= meOk;
  Result := True;
  try
    aTableList := TStringList.Create;
    try
    RetrieveTableNames(aTableList);

    if aTableList.Count = 0 then
      ErrorCode:= meEmpty
    else
      begin
        N:= 0;
        for Index := Low(NeedTables) to High(NeedTables) do
          if aTableList.IndexOf(NeedTables[Index]) = -1 then
            begin
              ErrorCode:= meMetaBroken;
              StrAdd(ErrorText, ', ', NeedTables[Index]);
            end;

        if N = Length(NeedTables) then
          ErrorCode:= meMetaEmpty;
      end;

    finally
      aTableList.Free;
    end;
  except
    on E: Exception do
      begin
        Funcs.WriteLog('TDeCustomDataBase.CheckConnectionInfo skip error: ' + E.Message);
        Result:= False;
      end;
  end;
end;

function TDeCustomDataBase.AsyncOpen(const AQueryText: string; AOnDataset: TDeAsyncDatasetEvent; AOnException: TDeAsyncExceptionEvent): Boolean;
begin
  Result := False; // Асинхронные операции должны быть реализованы в наследниках!
end;

function TDeCustomDataBase.AsyncExecute(const AQueryText: string; AOnExecute: TDeAsyncExecuteEvent; AOnException: TDeAsyncExceptionEvent): Boolean;
begin
  Result := False; // Асинхронные операции должны быть реализованы в наследниках!
end;

{ TDeDataset }

constructor TDeDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDeDataset.Destroy;
begin
  FreeAndNil(FQueryDescr);
  inherited Destroy;
end;

procedure TDeDataset.DoClose;
begin
  FCurrentIndex:= -1;
end;

procedure TDeDataset.DoOpen;
begin
  if GetRecordCount = 0 then FCurrentIndex:= -1
                        else FCurrentIndex:=  0;
end;

procedure TDeDataset.SetQueryType(aQueryType : TDeQueryType);
begin
  CreateDescriptor(aQueryType);
end;

procedure TDeDataset.QueryDescrChanged(Sender : TObject);
begin
  Exclude(FState, qsInitialized);
end;

procedure TDeDataset.QueryDescrFinished(Sender : TObject);
begin
  if not (qsInitialized in State) then
    Initialize;
end;

function TDeDataset.NewDescriptor(const aQueryType: TDeQueryType): TCustomQueryDescr;
begin
  case aQueryType of
    qtSelect: { Запрос выборки множества строк данных }
      begin
        Result := TSelectQueryDescr.Create;
        Result.OnLinkField := LinkField;
      end;
    qtRow: { Запрос выборки одной строки данных }
      begin
        Result := TRowQueryDescr.Create;
        Result.OnLinkField := LinkField;
      end;
    qtInsert: { Запрос на добавление данных }
      Result := TInsertQueryDescr.Create;
    qtUpdate: { Запрос на обновление данных }
      Result := TUpdateQueryDescr.Create;
    qtDelete: { Запрос на удаление данных }
      begin
        Result := TDeleteQueryDescr.Create;
        Result.OnLinkField := LinkField;
      end
  else
    Result := nil;
  end;
end;

procedure TDeDataset.CreateDescriptor(const aQueryType: TDeQueryType);
begin
  FreeAndNil(FQueryDescr);
  // Заменил логику для расширяемости!!! (см. закомментированный код ниже)
  FQueryDescr := NewDescriptor(aQueryType);
  {
  case aQueryType of
    qtCount:   FQueryDescr := TCountQueryDescr.Create;
    qtSelect:  begin
                 FQueryDescr := TSelectQueryDescr.Create;
                 FQueryDescr.OnLinkField := LinkField;
               end;
    qtInsert:  FQueryDescr := TInsertQueryDescr.Create;
    qtUpdate:  FQueryDescr := TUpdateQueryDescr.Create;
    qtDelete:  FQueryDescr := TDeleteQueryDescr.Create;
    qtRow:     begin
                 FQueryDescr := TRowQueryDescr.Create;
                 FQueryDescr.OnLinkField := LinkField;
               end;
    qtMin:     FQueryDescr := TMinQueryDescr.Create;
    qtMax:     FQueryDescr := TMaxQueryDescr.Create;
  end;
  }
  if Assigned(FQueryDescr) then
  begin
//    if Self is TDeADODataset then
//      FQueryDescr.QueryNames := qnBracket;
    {
    if Self is TDeBDEDataset then
      FQueryDescr.QueryNames := qnCommas2;
    }
    FQueryDescr.OnChanged := QueryDescrChanged;
    FQueryDescr.OnFinished := QueryDescrFinished;
  end;
end;

function TDeDataset.Initialize : boolean;
begin
  Include(FState, qsInitialized);
  result := true;
end;

function TDeDataset.StringValue(const aIndex: Integer; aCodePage: Integer = -1): String;
var resCodePage: Integer;
begin
  if aCodePage = -1 then resCodePage:= Database.CodePage
                    else resCodePage:= aCodePage;

  if resCodePage = cpUTF8 then
    result:= UTF8ToString(RawByteString(Trim(Value[aIndex])))
  else
    result:= Trim(Value[aIndex]);
end;

function TDeDataset.StringValueByName(const aName: string; aCodePage: Integer = -1): String;
var Index : integer;
begin
  Index := IndexByName(aName);
  if Index >= 0 then
    result := StringValue(Index, aCodePage)
  else
    raise EDeDataSetError.Create('Unknown field');
end;

function TDeDataset.StringValueByNameDef(const aName : string; aDefaultValue : String; aCodePage: Integer = -1) : String;
var Index : integer;
begin
  Index := IndexByName(aName);
  if Index >= 0 then
    result := StringValue(Index, aCodePage)
  else
    result := aDefaultValue;
end;

function TDeDataset.IntValueDef(const aIndex: Integer; aDefault: Int64 = 0): Int64;
var v: Variant;
begin
  try
    v:= Value[aIndex];
    if varIsNull(v) then Result:= aDefault
                    else Result:= v;
  except
    Result:= aDefault;
  end;
end;

function TDeDataset.IntValueByNameDef(const aName: string; aDefault: Int64 = 0): Int64;
var Index : integer;
begin
  Index := IndexByName(aName);
  if Index >= 0 then
    result := IntValueDef(Index, aDefault)
  else
    begin
      result := aDefault;
      raise EDeDataSetError.Create('Unknown field');
    end;
end;

function TDeDataset.GetRecNo: Integer;
begin
  Result:= FCurrentIndex;
end;

function TDeDataset.GetRecordCount: integer;
begin
  Result:= Internal_Count;
end;

function TDeDataset.GetValueByName(const aName : string) : Variant;
var Index : integer;
begin
  Index := IndexByName(aName);
  if Index >= 0 then
    result := Value[Index]
  else
    begin
      result := EmptyStr;
      raise EDeDataSetError.Create('Unknown field');
    end;
  if VarType(Result)= VarString  then
    Result:=Trim(Result);
end;

function TDeDataset.ValueByNameDef(const aName : string; aDefaultValue: Variant) : Variant;
var Index : integer;
begin
  Index:= IndexByName(aName);
  if Index >= 0 then
    begin
      result:= Value[Index];
      if VarType(Result)= VarString then Result:= Trim(Result);
    end
  else
    begin
      result:= aDefaultValue;
    end;
end;

procedure TDeDataset.SetValueByName(const aName : string;  const aValue : Variant);
var Index : integer;
begin
  Index := IndexByName(aName);
  if Index >= 0 then
    Value[Index] := aValue
  else
    raise EDeDataSetError.Create('Unknown field');
end;

procedure TDeDataset.BeforeOpen;
begin
  if Assigned(FOnBeforeOpen) then
    OnBeforeOpen(Self);
end;

procedure TDeDataset.AfterOpen;
begin
  if Assigned(FOnAfterOpen) then
    OnAfterOpen(Self);
end;

procedure TDeDataset.LinkField(Sender: TObject; const aDataSet: TObject; const FieldName: string; var LinkMode: TLinkJoinMode; var LinkDataset: TObject; var LinkFieldName: string);
var
  TableMeta, LinkTableMeta: TTableMeta;
  FieldMeta: TFieldMeta;
begin
  LinkFieldName := EmptyStr;
  LinkMode := lmUnknown;
  if Assigned(MetaData) then
    begin
      if aDataSet is TTableMeta then TableMeta:= TTableMeta(aDataSet)
                                else TableMeta:= nil;
      if Assigned(TableMeta) then
        if Assigned(TableMeta.Fields) then
          begin
            FieldMeta := TableMeta.Fields.FindByName(FieldName);
            if Assigned(FieldMeta) then
              begin
                LinkTableMeta := FieldMeta.LinkTable;
                if Assigned(LinkTableMeta) then
                  begin
                    if Assigned(TableMeta.Database) and Assigned(LinkTableMeta.Database) then
                      if SameText(TableMeta.Database.Server, LinkTableMeta.Database.Server) then
                        begin
                          LinkMode := lmReady;
                          if not SameText(LinkTableMeta.Database.Database, TableMeta.Database.Database) then
                            begin
  //                          if LinkTableMeta.Database.DatabaseType = dtMSSQL then LinkSchemaName := 'dbo';
                              LinkMode := lmDifferentDatabases; // Разные базы данных!!!
                            end;
                        end
                      else
                        LinkMode := lmDifferentServers; // Разные сервера!!!
                    if Assigned(LinkTableMeta.KField) and (LinkTableMeta.KField.Count = 1) then
                      begin
                        LinkFieldName := LinkTableMeta.KField[0].Original;
                      end
                    else
                      LinkMode := lmInvalidKey; // Нельзя построить связь из-за ключевого поля!
                  end;
                LinkDataset:= LinkTableMeta;
              end;
          end;
    end;
end;

procedure TDeDataset.BlockReadMode;  // virtual;  abstract;
begin
  //
end;

procedure TDeDataset.UnBlockReadMode; // virtual;  abstract;
begin
  //
end;

procedure TDeDataset.Open(const ARetrieveAll: Boolean);
begin
  if not (qsInitialized in State) then Initialize;
  BeforeOpen;
  DoOpen;
  if aRetrieveAll then FetchAll;
  AfterOpen;
end;

procedure TDeDataset.Close;
begin
  DoClose;
end;

function TDeDataset.ExecuteQuery : boolean;
begin
  result := true;
  if not (qsInitialized in State) then
    result := Initialize;
end;

procedure TDeDataset.SetDatabase(const Value: TDeCustomDatabase);
begin
  if Assigned(FDatabase) then
    FDatabase.RemoveFreeNotification(Self);
  FDatabase := Value;
  if Assigned(FDatabase) then
    FDatabase.FreeNotification(Self);
end;

procedure TDeDataset.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDatabase) then
    begin
      {$IFDEF DEBUG}
      if Assigned(Descr) then
        DebugLog('%s.Notification(%s, opRemove) for %s ...', [ClassName, QuotedStr(FDatabase.Alias), Descr.Table])
      else
        DebugLog('%s.Notification(%s, opRemove) for $%p ...', [ClassName, QuotedStr(FDatabase.Alias), Pointer(Self)]);
      {$ENDIF}
      FDatabase := nil;
    end;
end;

function TDeDataset.IndexTFieldByName(const FieldName: string; var aField: TField): Integer;
begin
  aField := nil;
  Result:= -1;    // Должено быть переопределён в наследниках!!!
end;

{$IFDEF DEBUG}
procedure TDeDataset.DebugDumpLog(const FileName: string);
begin
  { Ничего не делаем в базовом классе!!! }
end;
{$ENDIF}

function TDeDataset.AsyncOpen(AOnDataset: TDeAsyncDatasetEvent; AOnException: TDeAsyncExceptionEvent): Boolean;
begin
  Result := Assigned(AOnDataset) and Assigned(Database);
  if Result then
    Result := Database.AsyncOpen(Descr.SQL, AOnDataset, AOnException);
end;

function TDeDataset.AsyncExecute(AOnExecute: TDeAsyncExecuteEvent; AOnException: TDeAsyncExceptionEvent): Boolean;
begin
  Result := Assigned(AOnExecute) and Assigned(Database);
  if Result then
    Result := Database.AsyncExecute(Descr.SQL, AOnExecute, AOnException);
end;

{ TConnectionList }

procedure TConnectionList.Clear;
begin
  {$IFDEF DEBUG}
  DebugLog(ClassName + '.Clear before clear ...');
  {$ENDIF}
  try
    while Count <> 0 do
      begin
        {$IFDEF DEBUG}
        DebugLog('%s.Clear delete %s ...', [ClassName, (Items[Pred(Count)] as TDeCustomDataBase).Alias]);
        {$ENDIF}
        Delete(Pred(Count));
      end;
    //inherited Clear;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog(ClassName + '.Clear skip error: ' + E.Message);
    {$ENDIF}
  end;
  {$IFDEF DEBUG}
  DebugLog(ClassName + '.Clear after clear ...');
  {$ENDIF}
end;

{
constructor TConnectionList.Create;
begin
  inherited Create;
end;
}

function TConnectionList.Get(const Index: Integer): TDeCustomDataBase;
begin
  if Index < Count then
    Result := TDeCustomDataBase(inherited Items[Index])
  else
    Result := nil;
end;

procedure TConnectionList.Put(const Index: Integer; Database: TDeCustomDataBase);
begin
  if Index >= Count then Count := Succ(Index);
  inherited Items[Index] := Database;
end;

{$IFDEF DEBUG}
procedure TConnectionList.DebugConnectionsLog(const Text: string);
const
  DatabaseTypes: array[TDatabaseType] of PChar = ('dtNone', 'dtInterbase',
    'dtBDE', 'dtParadox', 'dtOracle', 'dt_5', 'dtFileDB', 'dtPostgreSQL',
    'dtMSSql', 'dtODBC', 'dtdBase', 'dtMSAccess', 'dtMySQL', 'dtSyBase',
    'dtInformix', 'dtDB2', 'dtDBCOXML', 'dtADOXML', 'dtFireBird', 'dtSQLite');
  function PrepareConnectionsLog: string;
  var
    TextTable: TTextTable;
    Index: Integer;
    Strings: TStrings;
    Database: TDeCustomDataBase;
    function PrepareAttributes: string;
    begin
      Result := EmptyStr;
      if Database.IsReadOnly then
        Result := Result + 'R';
      if Database.CI then
        Result := 'C' + Result;
      if Database.AI then
        Result := 'A' + Result;
    end;
  begin
    if Count = 0 then
      Result := EmptyStr
    else
      begin
        TextTable := TTextTable.Create;
        try
          TextTable.Columns.Add('No', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('ID', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Type', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Provider', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Server', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Database', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Login', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Alias', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Attr', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('CodePage', 0, taRightJustify, taCenter);
          if Assigned(MetaData) and (mpDatabaseDatabaseGUID in MetaData.MetadataPresents) then
            TextTable.Columns.Add('GUID', 38, taCenter, taCenter);
          Strings := TStringList.Create;
          try
            for Index := 0 to Pred(Count) do
              begin
                TextTable.Lines[Index][0] := IntToStr(Succ(Index));
                Database := Items[Index];
                if Assigned(Database) then
                  begin
                    TextTable.Lines[Index][1] := VarToStr(Database.ID);
                    TextTable.Lines[Index][2] := StrPas(DatabaseTypes[Database.DatabaseType]);
                    Strings.Text := ReplaceText(Database.ConnectString, ';', #13#10);
                    TextTable.Lines[Index][3] := Trim(Strings.Values['Provider']);
                    TextTable.Lines[Index][4] := Database.Server;
                    TextTable.Lines[Index][5] := Database.Database;
                    TextTable.Lines[Index][6] := Database.Login;
                    TextTable.Lines[Index][7] := Database.Alias;
                    TextTable.Lines[Index][8] := PrepareAttributes;
                    case Database.FCodePage of
                      - 1:
                        TextTable.Lines[Index][9] := 'Unknown';
                      cpNone:
                        TextTable.Lines[Index][9] := 'System';
                      cp866:
                        TextTable.Lines[Index][9] := 'DOS 866';
                      cpUTF8:
                        TextTable.Lines[Index][9] := 'UTF-8';
                    else
                      TextTable.Lines[Index][9] := IntToStr(Database.FCodePage);
                    end;
                    if Assigned(MetaData) and (mpDatabaseDatabaseGUID in MetaData.MetadataPresents) then
                      TextTable.Lines[Index][10] := VarToStr(Database.VariantGUID);
                  end;
              end;
          finally
            Strings.Free;
          end;
          Result := #13#10 + TextTable.AsText(24);
        finally
          TextTable.Free;
        end;
      end;
  end;
begin
  DebugLog(Text + PrepareConnectionsLog);
end;
{$ENDIF}

{ TDeEmptyDatabase }

constructor TDeEmptyDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DatabaseType := dtNone;
  FReadOnly := True;
  FCanStoredProcedure := False;
end;

class function TDeEmptyDatabase.SupportQueryTypes: TDeQueryTypes;
begin
  Result := [qtSelect, qtRow];
end;

function TDeEmptyDatabase.GetConnected: Boolean;
begin
  Result := FConnected;
end;

procedure TDeEmptyDatabase.SetConnected(const Value: Boolean);
begin
  FConnected := Value;
  inherited SetConnected(FConnected);
end;

procedure TDeEmptyDatabase.RetrieveTableNames(aTablesList: TStringList);
begin
  inherited;
  // Для данного типа базы данных нет таблиц вообще!!!
  aTablesList.Clear;
end;

procedure TDeEmptyDatabase.RetrieveMetaTableInfo(aTable: TObject);
begin
  ;
end;

procedure TDeEmptyDatabase.RetrieveMetaTables(aTablesList: TObjectList);
begin
  aTablesList.Clear;
end;

function TDeEmptyDatabase.CreateQueryObject(const aQueryType: TDeQueryType; InDS: TDeDataset): TDeDataset;
begin
  Result := TDeEmptyDataset.Create(Self);
  Result.Database := Self;
end;

function TDeEmptyDatabase.DatasetPermissions(const DatasetName: string): TDeDatasetPermissions;
begin
  if Length(DatasetName) = 0 then
    Result := []
  else
    Result := [dpSelect]; // Только чтение!!!
end;

function TDeEmptyDatabase.FieldExists(const TableName, FieldName: string): Boolean;
begin
  Result := True; // Все поля существуют в данном типе базы данных!!!
end;

procedure TDeEmptyDatabase.LoadBLOB(const TableName, KeyFieldName, BlobFieldName: string; const KeyValue: Variant; Stream: TStream);
begin
  raise Exception.Create('LoadBLOB not supported');
end;

procedure TDeEmptyDatabase.UploadBLOB(const TableName, KeyFieldName, BlobFieldName: string; const KeyValue: Variant; Stream: TStream);
begin
  raise Exception.Create('UploadBLOB not supported');
end;

{ TDeEmptyDataset }

function TDeEmptyDataset.GetActive: Boolean;
begin
  Result := FActive;
end;

function TDeEmptyDataset.GetRecordCount: Integer;
begin
  Result := 0; // Пустой набор данных ВСЕГДА!!!
end;

function TDeEmptyDataset.GetValue(const Index: Integer): Variant;
begin
  Result := Null; // Всегда ПУСТО!!!
end;

procedure TDeEmptyDataset.SetValue(const Index: Integer; const Value: Variant);
begin
  // А НИКУДА НЕ СОХРАНЯЕМ!!!
end;

procedure TDeEmptyDataset.DoOpen;
begin
  FActive := True;
  LogAfterOpen(Descr);
end;

procedure TDeEmptyDataset.DoClose;
begin
  FActive := False;
end;

function TDeEmptyDataset.ExecuteQuery: Boolean;
begin
  Result := False; // Не поддерживает выполнение запросов!!!
end;
{
procedure TDeEmptyDataset.LoadRecord(ARecord: TObject; const AStage: TFieldStage; aSourceIndex: Integer);
var
  CacheItem: TCacheItem;
  Index: Integer;
begin
  if aRecord is TCacheItem then
    begin
      CacheItem := TCacheItem(ARecord);
      for Index := 0 to Pred(CacheItem.Owner.FieldCount) do
        if not (CacheItem.Owner.Fields[Index].IsLookup or (not CacheItem.Owner.Fields[Index].IsStored)) then
          CacheItem.FieldValue[Index] := Null;
    end;
end;
{}
function TDeEmptyDataset.InsertRecord(ARecord: TObject): Boolean;
begin
  Result := False; // Не поддерживает вставку данных!!!
end;

function TDeEmptyDataset.UpdateRecord(ARecord: TObject): Boolean;
begin
  Result := False; // Не поддерживает обновление данных!!!
end;

function TDeEmptyDataset.DeleteRecord(ARecord: TObject): Boolean;
begin
  Result := False; // Не поддерживает удаление данных!!!
end;

function TDeEmptyDataset.GetMaxKeyValue(const MetaID: Integer): Variant;
begin
  Result := VarAsType(0, varInteger);
end;

procedure TDeEmptyDataset.Prepare;
begin
  { Ничего не делаем }
end;

function TDeEmptyDataset.FetchAll: Boolean;
begin
  Result := True; // Всё начитано!!!
end;

function TDeEmptyDataset.Locate(const KeyFields: string; const KeyValues: Variant; const Options: TLocateOptions): Boolean;
begin
  Result := False; // Позиционирование невозможно!!!
end;

procedure TDeEmptyDataset.LogAfterOpen(Descr: TCustomQueryDescr);
  function IsLogEnabled: Boolean;
  begin
    with TRegistry.Create do
      try
        RootKey := HKEY_CURRENT_USER;
        Result := OpenKeyReadOnly(RegKey);
        if Result then
          begin
            Result := ValueExists(RegLogQuery);
            if Result then
              try
                Result := Boolean(ReadInteger(RegLogQuery));
              except
                Result := False;
              end;
          end;
      finally
        Free;
      end;
  end;
  procedure WriteDescr(Descr: TCustomQueryDescr);
    function PrepareSQL: string;
    var
      Index: Integer;
    begin
      Result := EmptyStr;
      for Index := 0 to Pred(Descr.FieldCount) do
        Funcs.StrAdd(Result, ', ', Descr.Fields[Index].TargetName);

      if 0 < Length(Result) then
        Result := 'SELECT ' + Result + ' FROM ' + Descr.Table + ' -- source empty dataset';
    end;
  begin
    Funcs.WriteLog(PrepareSQL, False);
  end;
begin
  try
    if Assigned(Descr) then
      if IsLogEnabled then
        WriteDescr(Descr);
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog(ClassName + '.LogAfterOpen error: ' + E.Message);
    {$ENDIF}
  end;
end;

procedure TDeEmptyDataset.RetrieveFields(Fields: TObjectList; TableMeta: TObject);
begin
  { Ничего не делаем }
end;

procedure TDeEmptyDataset.CreateFields(Fields: TObjectList; const Stage: TFieldStage);
begin
  { Ничего не делаем }
end;

function TDeEmptyDataset.IndexByName(const Name: string): Integer;
begin
  Result := Descr.IndexByName(Name);
end;

{ TDeIndexedDataset }

constructor TDeIndexedDataset.Create(aOwner: TComponent);
begin
  inherited;
  IND_UseIndex:= False;
end;

destructor TDeIndexedDataset.Destroy;
begin
  IND_Clear;
  inherited;
end;

function TDeIndexedDataset.GetRecordCount: integer;
begin
  if IND_UseIndex then Result:= IND_Count
                  else Result:= Internal_Count;
end;

function TDeIndexedDataset.GetValue(const Index: Integer): Variant;
var i, PreIndex: Integer;
    v: Variant;
    Relation: TVariantRelationship;
begin
  Result:= Unassigned;

  if (Descr.QueryType in [qtRow]) then
    begin
      if (Descr.Fields[Index].TargetOperation = opCount) and (Descr.Fields[Index].TargetName = EmptyStr) then
        begin
          Result:= GetRecordCount;
        end else

      if (Descr.Fields[Index].TargetOperation in [opMin, opMax]) and (Descr.Fields[Index].SourceType in (NumericTypes + DateTimeTypes)) then
        begin
          if Descr.Fields[Index].TargetOperation = opMin then Relation:= vrLessThan
            {Descr.Fields[Index].TargetOperation = opMax}else Relation:= vrGreaterThan;

          PreIndex:= FInternalIndex;
          Result:= null;
          for i:=0 to Pred(GetRecordCount) do
            begin
              Internal_Goto(IND_External2Internal(i));
              if Internal_GetIdentValue( Descr.Fields[Index].SourceName, v, nil) then
                if not VarIsNull(v) then
                  if VarIsNull(Result) or (VarCompareValue(Result, v) = Relation) then Result:= v;
            end;
          Internal_Goto(PreIndex);
        end else

      if (Descr.Fields[Index].TargetOperation in [opSum]) and (Descr.Fields[Index].SourceType in (NumericTypes)) then
        begin
          PreIndex:= FInternalIndex;
          Result:= null;
          for i:=0 to Pred(GetRecordCount) do
            begin
              Internal_Goto(IND_External2Internal(i));
              if Internal_GetIdentValue( Descr.Fields[Index].SourceName, v, nil) then
                if not VarIsNull(v) then
                  if VarIsNull(Result) then Result:= v
                                       else Result:= Result + v;
            end;
          Internal_Goto(PreIndex);
        end;
    end;
end;

function TDeIndexedDataset.IND_External2Internal(const aIndex: Integer): Integer;
begin
  if not IND_UseIndex then Exit(aIndex);

  if (0 <= aIndex) and (aIndex < IND_Count) then Result:= IND_Records[aIndex]
                                            else Result:= -1;
end;

function TDeIndexedDataset.IND_Internal2External(const aIndex: Integer): Integer;
var i: Integer;
begin
  if not IND_UseIndex then Exit(aIndex);

  Result:= -1;
  for i:= Low(IND_Records) to High(IND_Records) do
    if IND_Records[i] = aIndex then Exit(i);

  raise Exception.Create('Internal2External. Index '+IntToStr(aIndex)+' out of bounds');
end;

procedure TDeIndexedDataset.IND_Clear;
begin
  IND_UseIndex:= False;
  SetLength(IND_Records, 0);
end;

procedure TDeIndexedDataset.IND_Build;
var
  Calculator: TDeCalculator;
  R, i, SortCount, FilterCount: Integer;
  v: Variant;
  FValues: array of Variant;

  function InnerCompare(A,B : Integer): Integer;
  const
    Directions: array[Boolean] of ShortInt = (-1, 1);
  var
    Index: Integer;
    vA, vB: Variant;
    sA, sB: string;
  begin
    Result := 0;
    for Index := 0 to Pred(SortCount) do
      begin
        vA := FValues[A][Index];
        vB := FValues[B][Index];

        if VarIsNull(vA) and VarIsNull(vB) then Continue else
        if VarIsNull(vA) and Not VarIsNull(vB) then Exit( Directions[Descr.SortDirection[Index] = sdDescending] ) else
        if Not VarIsNull(vA) and VarIsNull(vB) then Exit( Directions[Descr.SortDirection[Index] <> sdDescending] ) else
        if VarIsType(vA, varString ) or VarIsType(vA, varUString ) then
          begin
            sA := VarToStr(vA);
            sB := VarToStr(vB);
            case CompareString(LOCALE_USER_DEFAULT, 0, PChar(sA), Length(sA), PChar(sB), Length(sB)) of
              1:  Exit( Directions[Descr.SortDirection[Index] = sdDescending] ); { Первая строка меньше второй }
              3:  Exit( Directions[Descr.SortDirection[Index] <> sdDescending] ); { Первая строка больше второй }
            end;
          end
        else
          begin
            case VarCompareValue(vA, vB) of
              vrLessThan: Exit( Directions[Descr.SortDirection[Index] = sdDescending] ); { Первое значение меньше второго }
              vrGreaterThan: Exit( Directions[Descr.SortDirection[Index] <> sdDescending] ); { Втрое значение больше второго }
            end;
          end;
      end;
  end;

  procedure QuickSort(LowIndex, HighIndex: Integer);
  var
    I, J, P, V: Integer;
    VA: Variant;
  begin
    repeat
      I := LowIndex;
      J := HighIndex;
      P := (LowIndex + HighIndex) shr 1;
      repeat
        while InnerCompare(I, P) < 0 do Inc(I);
        while InnerCompare(J, P) > 0 do Dec(J);
        if I <= J then
        begin
          if I <> J then
            begin
              VA := FValues[I];
              FValues[I] := FValues[J];
              FValues[J] := VA;

              V := IND_Records[I];
              IND_Records[I] := IND_Records[J];
              IND_Records[J] := V;
            end;
          if P = I then
            P := J
          else if P = J then
            P := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;

      if (LowIndex=0) and (J=1) then
                          sleep(0);

      if LowIndex < J then QuickSort(LowIndex, J);
      LowIndex := I;
    until I >= HighIndex;
  end;

begin
  IND_UseIndex:= False;
  IND_Clear;

  if not Assigned(Descr) then Exit;
  if not Active          then Exit;
  if Internal_Count = 0  then Exit;

  SortCount:= Descr.SortCount;
  if not Assigned(Descr.Filter)
    then FilterCount:= 0
    else FilterCount:= Descr.Filter.Count;
  if ((SortCount = 0) and (FilterCount = 0)) then Exit;

  // надо строить индекс

  IND_UseIndex:= True;

  // Есть фильтр - отбираем записи
  if (0 < FilterCount) then
    begin
      SetLength(IND_Records, Internal_Count);
      IND_Count:= 0;
      Calculator:= TDeCalculator.Create;
      Calculator.OnGetIdentValue:= Internal_GetIdentValue;
      try
        for R:= 0 to Pred(Internal_Count) do
          begin
            Internal_Goto(R);
            if Calculator.Calculate(Descr.Filter, self) then
              begin
                IND_Records[IND_Count]:= R;
                Inc(IND_Count);
                // нужна одна запись - смысла рыть дальше нет
                // if (SortCount = 0) and (FDataSet.Descr.QueryType = qtRow) then Break;
              end;
          end;
      finally
        Calculator.Free;
      end;
      SetLength(IND_Records, IND_Count);
    end else

  // Фильтра нет, но есть сортировка - переносим все записи без отбора
  if (0 < SortCount) then
    begin
      IND_Count:= Internal_Count;
      SetLength(IND_Records, Internal_Count);
      for R:= 0 to Pred(Internal_Count) do
        IND_Records[R]:= R;
    end;

  // Сортируем, если надо
  if (0 < SortCount) and (0 < IND_Count) then
    try
      SetLength(FValues, IND_Count);
      for R:=0 to Pred(IND_Count) do
        begin
          FValues[R]:= VarArrayCreate([0, SortCount-1], varVariant);
          Internal_Goto(IND_Records[R]);
          for i:= 0 to Pred(SortCount) do
            if Internal_GetIdentValue(Descr.SortField[i], v, self) then FValues[R][i]:= v
                                                                   else FValues[R][i]:= Null;
        end;

       if (0 < SortCount ) and (1 < IND_Count) then
         QuickSort(0, Pred(IND_Count));
    finally
      SetLength(FValues, 0);
    end;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DeDB unit initialization ...');
finalization
  DebugLog('DeDB unit finalization ...');
{$ENDIF}

end.

