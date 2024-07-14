unit DeDB;

interface

uses SysUtils, Classes, Variants, DB, Contnrs, {Windows, }
     DeTypes, QueryDescriptor, DeParser, DeVariable;

const
  meOk = 0;              // ���� ������ ����������� � ���� ������, ������� ������ �������������
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
  // �������� ������� ��� DeDataset'� (��������������� �� SQL)
  TDeDataset = class;

  TObjectType = (otNone, otUnknown, otTable, otView, otStoredProc);

  TSupportDDL = (ddlCreateTable,
                 ddlCreateField,
                 ddlModifyField,
                 ddlDeleteField,
                 ddlSequence);
  TSupportDDLs = set of TSupportDDL;

  /// <summary>����� �� �������� � ������� ������ �� ���� ������</summary>
  TDeDatasetPermission =
    (
      dpSelect, // ��������� ������ ������ ������
      dpInsert, // ��������� ���������� ����� � ����� ������
      dpUpdate, // ��������� ���������� ����� � ������ ������
      dpDelete  // ��������� �������� ����� � ������ ������
    );
  TDeDatasetPermissions = set of TDeDatasetPermission;

  TDeAsyncDatasetEvent = procedure(Sender: TObject; const RawValue: Variant) of object;
  TDeAsyncExecuteEvent = procedure(Sender: TObject; const ReturnCode: Integer) of object;
  TDeAsyncExceptionEvent = procedure(Sender: TObject; E: Exception) of object;

  // ���������� ����� - ������ ���� ����� ��� ������ Profite
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
    FDDL                : Boolean; // ����: ����� �� ��������� ������� ��������/��������� ������/�����
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
    /// <summary>��� ���� ������</summary>
    property ID: Variant read FID write FID;
    /// <summary>��������� ���� ������</summary>
    property Alias: string read FAlias write FAlias;
    /// <summary>���� � ������ ���� ������</summary>
    property ConnectString: string read GetConnectString write SetConnectString;
    /// <summary>��� ���� ������</summary>
    property DatabaseType : TDatabaseType read FDatabaseType write FDatabaseType;
    function isInternalDataBase: Boolean; Virtual;
    // =true, ���� ���� ������ �������� ������ ��� ������
    property IsReadOnly : boolean read GetReadOnly write FReadOnly;
    property CI: Boolean read GetCI;
    property AI: Boolean read GetAI;
    property DDL: Boolean read GetDDL;
    property Server : string read GetServer write SetServer;
    property Database : string read GetDatabase write SetDatabase;
    property Login : string read GetLogin write SetLogin;
    property Password : string read GetPassword write SetPassword;
    // =true, ���� ����� ��������� � ��������� ���������
    property Connected : boolean read GetConnected write SetConnected;
    // ����� ��������� �� ������ ��� �����������
    property ConnectErrorMessage : string read FConnectErrorMessage;
    // ������� ������������ � ���� ������; ���������� true, ���� ������������ �������
    function CheckConnection: Boolean;
    // ���������� ��������� Meta �������� ������
    procedure GetMetaTables(aObjectList: TObjectList);
    procedure GetMetaTableInfo(aTable : TObject);
    // ��������� ���������� BLOB-���� � �����
    procedure UploadBLOB(const aTableName, aKeyFieldName, aBlobFieldName : string;
      const aKeyValue:Variant;  aUploadTo:TStream);  virtual;  abstract;
    // ��������� ���������� BLOB-���� �� ������;  ���� ������� NIL, �� �������
    // ���������� BLOB-����
    procedure LoadBLOB(const aTableName, aKeyFieldName, aBlobFieldName : string;
      const aKeyValue:Variant;  aLoadFrom:TStream);  virtual;  abstract;
    function CreateQuery(aQueryType : TDeQueryType; InDS: TDeDataset = nil): TDeDataset; virtual;

    /// <summary>������� ������ ������� � ���� ������</summary>
    /// <param name="Source">������ � ������� � ����������� �������</param>
    /// <returns>True - ������� ������� � ���� ������ �������</returns>
    function CreateTable(Source: TObject): Boolean; virtual;
    /// <summary>������� ������ ���� ��� ������� � ���� ������</summary>
    /// <param name="Source">������ � ������� � ����������� �������</param>
    /// <returns>True - ���� ��� ������� ������� � ���� ������ �������</returns>
    function CreateField(Source: TObject): Boolean; virtual;
    /// <summary>������� �������� ���� ��� ������� � ���� ������</summary>
    /// <param name="Source">������ � ������� � ���������� �������</param>
    /// <returns>True - ���� ��� ������� �������� � ���� ������ �������</returns>
    function ModifyField(Source: TObject): Boolean; virtual;
    /// <summary>������� ������� ���� �� ������� � ���� ������</summary>
    /// <param name="Source">������ � ������� � ��������� �������</param>
    /// <returns>True - ���� �� ������� ������� � ���� ������ �������</returns>
    function DeleteField(Source: TObject): Boolean; virtual;

    property CanODBC : Boolean read FCanODBC;
    property CanStoredProcedure : Boolean read FCanStoredProcedure;
    /// <summary>������� ��������� ����������� ���������� ����������� �������</summary>
    /// <returns>True - �������������� ��������� �������� ������</returns>
    function CanPortionSelect: Boolean; dynamic;

    function ConnectStringODBC(const IncludeAutorisation: Boolean): string; virtual;
    function ExecuteStoredProcedure(const aProcedureName: string; aList: TDeVariableList): Integer; virtual;
    procedure RetrieveTableNames(aTablesList: TStringList); virtual; abstract;
    procedure RetrieveMetaTables(aTablesList: TObjectList); virtual; abstract;
    procedure RetrieveMetaTableInfo(aTable : TObject);  virtual;  abstract;
    procedure RetrieveProcedures(aProceduresList : TObjectList); virtual;
    procedure RetrieveProcedureInfo(aTable : TObject); virtual;
    /// <summary>������� �������� ��������� ��� ��������� �������� ���������</summary>
    /// <param name="ProcedureName">��� �������� ���������</param>
    /// <param name="ParameterList">������ ���������� �������� ���������</param>
    /// <returns>True - ��������� ��������� �������� � ������</returns>
    function RetrieveProcedureParameters(const ProcedureName: string; ParameterList: TObject): Boolean; virtual;
    function RetrieveProcedureExecQuery(const ProcedureName: string; ParameterList: TObject): String; virtual;
    /// <summary>������� ��������� ������� � ���� ������ ��������� �������� ���������</summary>
    /// <param name="StoredProcedureName">��� �������� ���������</param>
    /// <returns>True - �������� ��������� ���� � ���� ������</returns>
    function StoredProcedureExists(const StoredProcedureName: string): Boolean; dynamic;
    /// <summary>������� �������� � Sequence</summary>
    /// <param>aQueryType = stInsert - ������� Sequence </param>
    /// <param>aQueryType = stDelete - ������� Sequence </param>
    /// <param>aQueryType = stUpdate - ������������� ����� ��������, ������ ���� ������ aValue </param>
    /// <param>aQueryType = stHaving - ���������� ������� Sequence � ��������� ������ </param>
    function Sequence(const aType: TDeSequenceType; const aTableName, aFieldName: String): Boolean; virtual; abstract;
    function Autoincrement(const aType: TDeSequenceType; const aTableName, aFieldName: String): Boolean; virtual; abstract;
    /// <summary>������� �������� ��� ������� � ���� ������</summary>
    /// <returns>
    /// <param>otNone - ���� ������ �� ����� ���������� ��� ��������</param>
    /// <param>otUnknown - ������ �� ������</param>
    /// <param>otTable - ������� � ���� ������</param>
    /// <param>otView - ������� � ���� ������</param>
    /// <param>otStoredProc - �������� ��������� � ���� ������</param>
    /// </returns>
    function RetrieveMetaObjectType(const ObjectName: string): TObjectType; dynamic;
    // v. 17.01
    property CodePage: Integer read FCodePage write FCodePage;
    /// <summary>������� ��������� ������� ���� � �������</summary>
    /// <param name="TableName">��� �������</param>
    /// <param name="FieldName">��� ����</param>
    /// <returns>True - ���� ���������� � �������</returns>
    function FieldExists(const TableName, FieldName: string): Boolean; dynamic;
    /// <summary>������� ���������� ��������� �������������� ����� ������ ����� ��������</summary>
    class function SupportQueryTypes: TDeQueryTypes; dynamic;
    /// <summary>������� ��������� ���� ������ �� ������� ��������������� ������</summary>
    /// <returns>True - ���� ������ �������������</returns>
    function CheckMetaConnection(var ErrorCode: Integer; var ErrorText: String): Boolean; dynamic;
    /// <summary>������� ��������� ��� ���� �� ������������� ����������� ������ ��� ������ � ����</summary>
    /// <param name="LocalType">��� ����������� ������ ����</param>
    /// <param name="RealType">����� ��� ��� ����������� ������ ����</param>
    /// <param name="RealSize">����������� ������ ���� ���� ��� ����������� ������</param>
    /// <returns>True - ����� ����������� ������</returns>
    function IsFieldTypeConvert(const LocalType: TFieldType; const LocalSize: Integer;
                                var RealType: TFieldType; var RealSize: Integer): Boolean; dynamic;
    procedure TargetToSourceFieldType(var aFieldData: TFieldData); virtual;
    property InternalConnectString: string read FConnectString; // v. 17.07
    // v. 18.09
    /// <summary>������� ���������� DDL (Data Definition Language), �������������� ����� ������</summary>
    function SupportDDLs: TSupportDDLs; dynamic;
    // v.19.06
    /// <summary>��������� ���� �� �������� � ������� ������ �� ���� ������</summary>
    /// <param name="DatasetName">��� ������� ��� ��������� ����</param>
    /// <returns>������� ���������� ��������� �� ����� <c>TDeDatasetPermission</c></returns>
    function DatasetPermissions(const DatasetName: string): TDeDatasetPermissions; dynamic;
    // v.19.06
    /// <summary>��������� ������ �� ���� ������ ����������</summary>
    function AsyncOpen(const AQueryText: string; AOnDataset: TDeAsyncDatasetEvent; AOnException: TDeAsyncExceptionEvent = nil): Boolean; dynamic;
    /// <summary>���������� ������� � ���� ������ ����������</summary>
    function AsyncExecute(const AQueryText: string; AOnExecute: TDeAsyncExecuteEvent; AOnException: TDeAsyncExceptionEvent = nil): Boolean; dynamic;
    // v.19.07
    property VariantGUID: Variant read FGUID write FGUID;
    // v.19.08
    /// <summary>������� ��������� ����������� ���� � ���� ������</summary>
    /// <returns>True - �������������� �����</returns>
    function CanSchema: Boolean; dynamic;
    /// <summary>������� ��������� ����� �� ��������� ��� ����������������� ������������ � ���� ������</summary>
    /// <returns>����� �� ��������� ��� ����������������� ������������</returns>
    property LoginSchema: string read GetLoginSchema;
    // v.21.12
    /// <summary>������� ��������� ���� � ������� �� ������� ���� ������</summary>
    /// <param name="DateTime">���� � ����� �� �������</param>
    /// <returns>True - ������ ������ ������</returns>
    function ReadServerDateTime(var DateTime: TDateTime): Boolean; dynamic;

    /// <summary>������� �������������� ����� ������� � ������ �� �������� ��</summary>
    function TableToStr(const tableName: String; const tableAlias: String = ''; const tableDatabase: String = '';
                        const tableSchema: String = ''): string; virtual;
    /// <summary>������� �������������� ����� ���� � ������ �� �������� ��</summary>
    function FieldToStr(const fieldName: String; const fieldAlias: String = ''; const tableAlias: String = '';
                        const fieldOperation: TOperationType = opNone; const DataType: TFieldType = ftUnknown; const DataSize: Integer = 0): string; virtual;
    /// <summary>������� �������������� �������� � ������ �� �������� ��</summary>
    function ConstToStr(const aValue: Variant; const aType: TFieldType): string; virtual;
    /// <summary>������� �������������� ������ � ����������� � ������ �� �������� ��</summary>
    function FuncToStr(aFunction: string; aParams: array of variant): string; virtual;
  end;

  /// <summary>����� ������ ���� ������</summary>
  /// <remarks>� ������ ���� ������ ������ ��� � ���� � ������������� ������� ������� ��� ����� ���� ������ - ��� ����� �������!!!</remarks>
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

  // ����������� �����-������ ��� ���� ����� Dataset'�� Profite'a
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
    // ���������� �������� ���� � ��������� �������
    function GetValue(const Index: Integer): Variant; virtual; abstract;
    // ������������� �������� ���� � ��������� �������
    procedure SetValue(const Index: Integer; const Value: Variant); virtual; abstract;
    // �� ����� ���� ���������� ��� ��������
    function GetValueByName(const aName : string) : Variant;
    procedure SetValueByName(const aName : string;  const aValue : Variant);
    function GetRecNo: Integer; virtual;
    procedure SetRecNo(const Value: Integer); virtual; abstract;
    procedure DoOpen;  virtual;
    procedure DoClose;  virtual;
    // ���������� ������ �� ������� �������
    function FetchAll : boolean;  virtual; abstract;
    procedure BeforeOpen;  virtual;
    procedure AfterOpen;  virtual;
    procedure LinkField(Sender: TObject; const aDataSet: TObject; const FieldName: string; var LinkMode: TLinkJoinMode; var LinkDataset: TObject; var LinkFieldName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;  override;
    // ����� ��� datasets ��� ���������� ������ ������� � ����������
    function Internal_Count: Integer; virtual; abstract;
    function Internal_Goto(Const aIndex: Integer): Boolean; virtual; abstract;
    function Internal_GetIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean; virtual; abstract;
    property QueryType : TDeQueryType write SetQueryType;
    property Database : TDeCustomDatabase read FDatabase write SetDatabase;
    property SourceSet: TComponent read FSourceSet;
    property Descr : TCustomQueryDescr read FQueryDescr;  // �������� �������
    property State : TDeQueryState read FState;             // ��������� �������
    property Active : boolean read GetActive;               // =true, ���� ������ �������
    property RecordCount : integer read GetRecordCount;     // ���������� ������� ������
    // ���������� ����� ���� �� �����
    function IndexByName(const Name: string): Integer; virtual; abstract;
    // ������ � �������� ���� �� ������
    property Value[const Index: Integer]: Variant read GetValue write SetValue;
    // ������ � �������� ���� �� �����
    property ValueByName[const Name: string]: Variant read GetValueByName write SetValueByName;
    function ValueByNameDef(const aName : string; aDefaultValue: Variant) : Variant;
    function StringValue(const aIndex: Integer; aCodePage: Integer = -1): String; virtual;
    function StringValueByName(const aName : string; aCodePage: Integer = -1) : String;
    function StringValueByNameDef(const aName : string; aDefaultValue : String; aCodePage: Integer = -1) : String;
    function IntValueDef(const aIndex: Integer; aDefault: Int64 = 0): Int64;
    function IntValueByNameDef(const aName: string; aDefault: Int64 = 0): Int64;
    property OnBeforeOpen : TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnAfterOpen : TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    // ����� �������� ������ ������ ������
    property RecNo: Integer read GetRecNo write SetRecNo;
    // �������������� ������ ��� ����������
    procedure Prepare;  virtual;  abstract;
    // ������������� ����� �������� ������
    procedure BlockReadMode;   virtual;  //abstract;
    // ������� ����� �������� ������
    procedure UnBlockReadMode;  virtual;  //abstract;
    // ��������� ����� ������
    procedure Open(const ARetrieveAll: Boolean = False);
    // ��������� ����� ������
    procedure Close;
    // ��������� ������
    function ExecuteQuery : boolean;  virtual;
    // ��������� ������ � ����� ������
    function InsertRecord(aRecord : TObject) : boolean;  virtual;  abstract;
    // ��������� ������ � ������ ������
    function UpdateRecord(aRecord : TObject) : boolean;  virtual;  abstract;
    // ������� ������ �� ������ ������
    function DeleteRecord(aRecord : TObject) : boolean;  virtual;  abstract;
    // ���������� ������������ �������� ���������� �����
    function GetMaxKeyValue(const MetaID: Integer = -1): Variant; virtual; abstract;
    // ������������� ������
    function Locate(const KeyFields: string; const KeyValues: Variant; const Options: TLocateOptions): Boolean; virtual; abstract;
    // ��������� ������������ ����� ������ ������
    procedure RetrieveFields(aFields: TObjectList; aTableMeta: TObject = nil);  virtual;  abstract;
    /// <summary>���������� ������ ������ ������ �� ��������� ������������ �����</summary>
    /// <param name="aFields">������ ����� ������������</param>
    /// <param name="Stage">����� �������� �����</param>
    procedure CreateFields(aFields: TObjectList; const Stage: TFieldStage); virtual; abstract;
    /// <summary>��������� ���� ������ ��� ����</summary>
    /// <param name="FieldName">��� ����</param>
    /// <returns>������� ���������� ��� ������ ���� � ������ ������ ��� ftUnknown � ������ ������������� ��� �����������</returns>
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

  /// <summary>����� ������� ������ ������</summary>
  /// <remarks>������������ � <see cref="TDeEmptyDatabase">TDeEmptyDatabase</see>.</remarks>
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
    // �� ����� ���� ���������� ��� �����
    function IndexByName(const Name: string): Integer; override;
  end;

  // ������ ��� ������ ������ � �������������
  TConnectionList = class(TComponentList)
  private
    // ���������� ����� ����������� � ����������� �� ���� ���� ������
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
  Result := EmptyStr; // �� ��������� ����� �� ����������!
end;

function TDeCustomDataBase.GetPassword : string;
begin
  Result:=FPassword;
  // ���� ����� ������������ � �� ����� ������, �� ����������� ������ �� ����-����
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
  // ���� ���� ������ ������, �� ...
  if Assigned(aObjectList) then
    RetrieveMetaTables(aObjectList);
end;

procedure TDeCustomDatabase.GetMetaTableInfo(aTable : TObject);
var i: Integer;
begin
  // ���� �������� �� �����������, �� ������
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
                   // ����� ������ ���� �������� � �����������!!!
  Result := False; // �� ��������� ���� �� ����������
end;

function TDeCustomDataBase.CreateTable(Source: TObject): Boolean;
begin
  Result := False;
  // ����� ������ ���� - �� �������! �����������, �.�. ���� ������� ���������� True ������ ����� ������ ������!
  //raise Exception.Create('DataBase not support [CreateTable]')
end;

function TDeCustomDataBase.CreateField(Source: TObject): Boolean;
begin
  Result := False; // ����� ������ ������������� � �����������!
end;

function TDeCustomDataBase.ModifyField(Source: TObject): Boolean;
begin
  Result := False; // ����� ������ ������������� � �����������!
end;

function TDeCustomDataBase.DatasetPermissions(const DatasetName: string): TDeDatasetPermissions;
begin
  if Length(DatasetName) = 0 then
    Result := []
  else
    Result := [dpSelect, dpInsert, dpUpdate, dpDelete]; // ����� �� ��������� ��� ������� ������. ����� �������������� � �����������.
end;

function TDeCustomDataBase.DeleteField(Source: TObject): Boolean;
begin
  Result := False; // ����� ������ ������������� � �����������!
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
  // ������ ���������������� � ����������� ...
  Result := False;
end;

function TDeCustomDataBase.RetrieveProcedureExecQuery(const ProcedureName: string; ParameterList: TObject): String;
begin
  // ����� ���������������� � ����������� ...
  Result:= EmptyStr;
end;

function TDeCustomDataBase.StoredProcedureExists(const StoredProcedureName: string): Boolean;
begin
  // ������ ���������������� � ����������� ...
  Result := False;
end;

function TDeCustomDataBase.SupportDDLs: TSupportDDLs;
begin
  Result := []; // ������� ����� �� ������������ DDL!
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
  Result := False; // �� ��������� ���� ������ �� ����� ������ ������ ��������!
end;

function TDeCustomDataBase.CanSchema: Boolean;
begin
  Result := False; // �� ��������� � ��� ������ ����� �� ��������������
end;

function TDeCustomDataBase.ReadServerDateTime(var DateTime: TDateTime): Boolean;
begin
  Result := True;
  DateTime := Now; // �� ��������� ���� � ����� ���������� � ���������� �����������.
end;

function TDeCustomDataBase.RetrieveMetaObjectType(const ObjectName: string): TObjectType;
begin
  Result := otNone; // �� ��������� ���� �� ����� ���������� ���� ��������!
end;

function TDeCustomDataBase.IsFieldTypeConvert(const LocalType: TFieldType; const LocalSize: Integer;
                                              var RealType: TFieldType; var RealSize: Integer): Boolean;
begin
  RealType := ftUnknown;
  RealSize := 0;
  Result := False; // ����������� �� ��������� �� ��������� ...
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
  Result := False; // ����������� �������� ������ ���� ����������� � �����������!
end;

function TDeCustomDataBase.AsyncExecute(const AQueryText: string; AOnExecute: TDeAsyncExecuteEvent; AOnException: TDeAsyncExceptionEvent): Boolean;
begin
  Result := False; // ����������� �������� ������ ���� ����������� � �����������!
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
    qtSelect: { ������ ������� ��������� ����� ������ }
      begin
        Result := TSelectQueryDescr.Create;
        Result.OnLinkField := LinkField;
      end;
    qtRow: { ������ ������� ����� ������ ������ }
      begin
        Result := TRowQueryDescr.Create;
        Result.OnLinkField := LinkField;
      end;
    qtInsert: { ������ �� ���������� ������ }
      Result := TInsertQueryDescr.Create;
    qtUpdate: { ������ �� ���������� ������ }
      Result := TUpdateQueryDescr.Create;
    qtDelete: { ������ �� �������� ������ }
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
  // ������� ������ ��� �������������!!! (��. ������������������ ��� ����)
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
                              LinkMode := lmDifferentDatabases; // ������ ���� ������!!!
                            end;
                        end
                      else
                        LinkMode := lmDifferentServers; // ������ �������!!!
                    if Assigned(LinkTableMeta.KField) and (LinkTableMeta.KField.Count = 1) then
                      begin
                        LinkFieldName := LinkTableMeta.KField[0].Original;
                      end
                    else
                      LinkMode := lmInvalidKey; // ������ ��������� ����� ��-�� ��������� ����!
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
  Result:= -1;    // ������� ���� ������������ � �����������!!!
end;

{$IFDEF DEBUG}
procedure TDeDataset.DebugDumpLog(const FileName: string);
begin
  { ������ �� ������ � ������� ������!!! }
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
  // ��� ������� ���� ���� ������ ��� ������ ������!!!
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
    Result := [dpSelect]; // ������ ������!!!
end;

function TDeEmptyDatabase.FieldExists(const TableName, FieldName: string): Boolean;
begin
  Result := True; // ��� ���� ���������� � ������ ���� ���� ������!!!
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
  Result := 0; // ������ ����� ������ ������!!!
end;

function TDeEmptyDataset.GetValue(const Index: Integer): Variant;
begin
  Result := Null; // ������ �����!!!
end;

procedure TDeEmptyDataset.SetValue(const Index: Integer; const Value: Variant);
begin
  // � ������ �� ���������!!!
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
  Result := False; // �� ������������ ���������� ��������!!!
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
  Result := False; // �� ������������ ������� ������!!!
end;

function TDeEmptyDataset.UpdateRecord(ARecord: TObject): Boolean;
begin
  Result := False; // �� ������������ ���������� ������!!!
end;

function TDeEmptyDataset.DeleteRecord(ARecord: TObject): Boolean;
begin
  Result := False; // �� ������������ �������� ������!!!
end;

function TDeEmptyDataset.GetMaxKeyValue(const MetaID: Integer): Variant;
begin
  Result := VarAsType(0, varInteger);
end;

procedure TDeEmptyDataset.Prepare;
begin
  { ������ �� ������ }
end;

function TDeEmptyDataset.FetchAll: Boolean;
begin
  Result := True; // �� ��������!!!
end;

function TDeEmptyDataset.Locate(const KeyFields: string; const KeyValues: Variant; const Options: TLocateOptions): Boolean;
begin
  Result := False; // ���������������� ����������!!!
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
  { ������ �� ������ }
end;

procedure TDeEmptyDataset.CreateFields(Fields: TObjectList; const Stage: TFieldStage);
begin
  { ������ �� ������ }
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
              1:  Exit( Directions[Descr.SortDirection[Index] = sdDescending] ); { ������ ������ ������ ������ }
              3:  Exit( Directions[Descr.SortDirection[Index] <> sdDescending] ); { ������ ������ ������ ������ }
            end;
          end
        else
          begin
            case VarCompareValue(vA, vB) of
              vrLessThan: Exit( Directions[Descr.SortDirection[Index] = sdDescending] ); { ������ �������� ������ ������� }
              vrGreaterThan: Exit( Directions[Descr.SortDirection[Index] <> sdDescending] ); { ����� �������� ������ ������� }
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

  // ���� ������� ������

  IND_UseIndex:= True;

  // ���� ������ - �������� ������
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
                // ����� ���� ������ - ������ ���� ������ ���
                // if (SortCount = 0) and (FDataSet.Descr.QueryType = qtRow) then Break;
              end;
          end;
      finally
        Calculator.Free;
      end;
      SetLength(IND_Records, IND_Count);
    end else

  // ������� ���, �� ���� ���������� - ��������� ��� ������ ��� ������
  if (0 < SortCount) then
    begin
      IND_Count:= Internal_Count;
      SetLength(IND_Records, Internal_Count);
      for R:= 0 to Pred(Internal_Count) do
        IND_Records[R]:= R;
    end;

  // ���������, ���� ����
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

