{$WARN SYMBOL_PLATFORM OFF}

unit ConnectOperationsUnit;

interface

uses Windows, SysUtils, Classes, DB, Variants, Contnrs, Zip, Generics.Collections,
     {$IFDEF INDYUSED}IdHTTP,{$ENDIF}
     Winapi.SHFolder, System.Win.Registry, System.Math, //Ora, OraClasses, SDEngine, DBTables, DBAccess, IBSQL,

     FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
     FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
     FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
     FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.UI, IBQuery, IBDataBase,
     FireDAC.Phys.PGDef, FireDAC.Phys.PG,
     FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef,
     FireDAC.Phys.FBDef, FireDAC.Phys.FB,
     Data.Win.ADODB,
     DeTypes, DeDataset, XMLDoc, XMLIntf, UFileDB, DeDB, DeVariable, DeParser, QueryDescriptor, XMLTable;

const
  // версии СУБД, необходимые для работы программы
  MinIBVersion  = 1;    // 6.0, 6.1, 6.5, ...
  MinBDEVersion = 100;  //  версия, входящая в поставку Delphi 6

type

  // Абстрактный класс-предок всех баз данных Profit, использующих язык SQL
  TDeSQLDatabase = class(TDeCustomDatabase)
  private
    FConnectionObject : TCustomConnection;  // подключение к базе данных
    // создает новое подключение к базе данных
    procedure CreateConnection;
    // закрывает подключение к базе данных
    procedure DestroyConnection;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // создает все необходимое для подключения к базе данных
    function CreateConnectionObject: TCustomConnection; virtual; abstract;
    function GetConnectionObject: TCustomConnection;
    function GetConnected: Boolean; override;
    procedure SetConnected(const aConnected: Boolean); override;
    function CreateTableDataset(const aTableName: string): TDataSet; virtual; abstract;
    procedure RetrieveMetaTableInfo(aTable : TObject); override;
  public
    property ConnectionObject: TCustomConnection read GetConnectionObject;
    procedure UploadBLOB(const aTableName, aKeyFieldName, aBlobFieldName: string; const aKeyValue:Variant; aUploadTo: TStream); override;
    procedure LoadBLOB(const aTableName, aKeyFieldName, aBlobFieldName : string; const aKeyValue:Variant; aLoadFrom:TStream ); override;
    function CanPortionSelect: Boolean; override;
  end;
  {
  // база данных BDE
  TDeBDEDatabase = class(TDeSQLDatabase)
  protected
    function CreateConnectionObject : TCustomConnection;  override;
    function CreateTableDataset(const aTableName : string) : TDataset;  override;
    procedure RetrieveTableNames;  override;
    procedure RetrieveMetaTables(aTablesList : TObjectList); override;
    procedure RetrieveMetaTableInfo(aTable : TObject); override;
    function CreateQueryObject(aQueryType : TDeQueryType; InDS:TDeDataset = nil) : TDeDataset;  override;
  public
    constructor Create(aOwner : TComponent);  override;
  end;
  {}
  // база данных Interbase
  TDeIBDatabase = class(TDeSQLDatabase)
  protected
    function BuildConnectString : string;  override;
    function CreateConnectionObject : TCustomConnection;  override;
    function CreateTableDataset(const aTableName : string) : TDataset;  override;
    function CreateQueryObject(const aQueryType: TDeQueryType; InDS: TDeDataset = nil): TDeDataset; override;
  private
  public
    constructor Create(aOwner : TComponent);  override;
    procedure RetrieveMetaTableInfo(aTable : TObject); override;
    procedure RetrieveTableNames(aTablesList: TStringList);  override;
    procedure RetrieveMetaTables(aTablesList : TObjectList); override;
  end;

  TDeFireDACDatabase = class(TDeSQLDatabase)
  protected
    function CreateTableDataset(const aTableName : string) : TDataset;  override;
    function CreateQueryObject(const aQueryType: TDeQueryType; InDS: TDeDataset = nil): TDeDataset; override;
    /// <summary>функция преобразования имени таблицы к строке по правилам БД</summary>
  private
  public
    function TableToStr(const tableName: String; const tableAlias: String = ''; const tableDatabase: String = '';
                        const tableSchema: String = ''): string; override;
    function FieldToStr(const fieldName: String; const fieldAlias: String = ''; const tableAlias: String = '';
                        const fieldOperation: TOperationType = opNone; const DataType: TFieldType = ftUnknown;
                        const DataSize: Integer = 0): string; override;
  end;

  TDeFBDatabase = class(TDeFireDACDatabase)
  protected
    function BuildConnectString : string;  override;
    function CreateConnectionObject : TCustomConnection;  override;
  private
  public
    constructor Create(aOwner : TComponent);  override;
    procedure RetrieveTableNames(aTablesList: TStringList);  override;
    procedure RetrieveMetaTables(aTablesList : TObjectList); override;
    procedure RetrieveMetaTableInfo(aTable : TObject); override;
  end;

  TDeMySQLDatabase = class(TDeFireDACDatabase)
  private
  protected
    function CreateConnectionObject : TCustomConnection;  override;
  public
    constructor Create(aOwner : TComponent);  override;
    destructor Destroy;  override;
    procedure RetrieveTableNames(aTablesList: TStringList);  override;
    procedure RetrieveMetaTables(aTablesList : TObjectList); override;
    procedure RetrieveMetaTableInfo(aTable : TObject);  override;
    function RetrieveProcedureParameters(const ProcedureName: string; ParameterList: TObject): Boolean; override;
    function TableToStr(const tableName: String; const tableAlias: String = ''; const tableDatabase: String = '';
                        const tableSchema: String = ''): string; override;
    function FieldToStr(const fieldName: String; const fieldAlias: String = ''; const tableAlias: String = '';
                        const fieldOperation: TOperationType = opNone; const DataType: TFieldType = ftUnknown;
                        const DataSize: Integer = 0): string; override;
    function FuncToStr(aFunction: string; aParams: array of variant): string; override;
    function StoredProcedureExists(const StoredProcedureName: string): Boolean; override;
    function ExecuteStoredProcedure(const aProcedureName: string; aList: TDeVariableList): Integer; override;
    function ToFieldType(aValue: Variant): TFieldType;
  end;

  TDePGDatabase = class(TDeFireDACDatabase)
  private
  protected
    function BuildConnectString : string;  override;
    function CreateConnectionObject : TCustomConnection;  override;
    function FieldMetaToDDL(Source: TObject; const KeyEnabled: Boolean): string;
  public
    constructor Create(aOwner : TComponent);  override;
    destructor Destroy;  override;
    procedure TargetToSourceFieldType(var aFieldData: TFieldData); override;
    function IsFieldTypeConvert(const LocalType: TFieldType; const LocalSize: Integer;
                                var RealType: TFieldType; var RealSize: Integer): Boolean; override;
    procedure RetrieveTableNames(aTablesList: TStringList);  override;
    procedure RetrieveMetaTables(aTablesList : TObjectList); override;
    procedure RetrieveMetaTableInfo(aTable : TObject);  override;
    function Sequence(const aType: TDeSequenceType; const aTableName, aFieldName: String): Boolean; override;
    function Autoincrement(const aType: TDeSequenceType; const aTableName, aFieldName: String): Boolean; override;
    function SupportDDLs: TSupportDDLs; override;
    function ExecuteStoredProcedure(const aProcedureName: string; aList: TDeVariableList): Integer; override;
    // v. 17.01
    function FieldExists(const TableName, FieldName: string): Boolean; override;
    function CreateTable(Source: TObject): Boolean; override;
    function CreateField(Source: TObject): Boolean;
    function DeleteField(Source: TObject): Boolean;
    function ModifyField(Source: TObject): Boolean;
  end;

  TADODatabaseDriver = (ddAuto, ddOleDB{$IFDEF SQLNCLI}, ddNativeClient{$ENDIF});

  // база данных MSSQL
  TDeADODatabase = class(TDeSQLDatabase)
  private
    FReady_CI_AI: Boolean;
    FReadyDDL: Boolean;
    FDatabaseDriver: TADODatabaseDriver;
    FObjectTypes: TStrings;
    FReadyLoginSchema: Boolean;
    FLoginSchema: string;
    class var
      FDefaultDatabaseDriver: TADODatabaseDriver;
      FDefaultApplicationName: string;
    procedure BeforeOpen(DataSet: TDataSet);
    type
      TAsyncQueryThread = class(TThread)
      private
        { Private declarations }
        FConnection: TADOConnection;
        FQuery: TADOQuery;
        FExecuting: Boolean;
        FReturnCode: Integer;
        FRawValue: Variant;
        FException: Exception;
        FOnExecute: TDeAsyncExecuteEvent;
        FOnError: TDeAsyncExceptionEvent;
        FOnDataset: TDeAsyncDatasetEvent;
        FOnException: TDeAsyncExceptionEvent;
        procedure NotifyReturnCode;
        procedure NotifyRawValue;
        procedure NotifyError;
        procedure NotifyException;
      protected
        procedure Execute; override;
      public
        constructor Create(const AConnectionString: string);
        destructor Destroy; override;
        procedure AsyncExecute(const AQueryText: string; AOnCompleted: TDeAsyncExecuteEvent; AOnFailed: TDeAsyncExceptionEvent = nil);
        procedure AsyncOpen(const AQueryText: string; AOnDataset: TDeAsyncDatasetEvent; AOnError: TDeAsyncExceptionEvent = nil);
        property OnException: TDeAsyncExceptionEvent read FOnException write FOnException;
      end;
      TAsyncThreadManager = class
      private
        { Private declarations }
        FConnectionString: string;
        FMaxThreadCount: Cardinal;
        FThreads: TThreadList;
        FOnException: TDeAsyncExceptionEvent;
        procedure ThreadDone(Sender: TObject);
      public
        { Public declarations }
        constructor Create(const AConnectionString: string; const AMaxThreadCount: Cardinal);
        destructor Destroy; override;
        function Execute(const QueryText: string; OnCompleted: TDeAsyncExecuteEvent; OnFailed: TDeAsyncExceptionEvent = nil): Boolean;
        function Open(const QueryText: string; OnDataset: TDeAsyncDatasetEvent; OnError: TDeAsyncExceptionEvent = nil): Boolean;
        property MaxThreadCount: Cardinal read FMaxThreadCount;
        property OnException: TDeAsyncExceptionEvent read FOnException write FOnException;
      end;
    var
      FAsyncThreads: TAsyncThreadManager; // Менеджер асинхронных ниток ..
  protected
    function GetServer : string; override;
    function BuildConnectString: string; override;
    procedure SetConnected(const aConnected: Boolean); override;
    function CreateConnectionObject : TCustomConnection;  override;
    function CreateTableDataset(const aTableName : string) : TDataset;  override;
    function CreateQueryObject(const aQueryType: TDeQueryType; InDS: TDeDataset = nil): TDeDataset; override;
    procedure Read_CI_AI;
    procedure ReadDDL;
    function GetCI: Boolean; override;
    function GetAI: Boolean; override;
    function GetDDL: Boolean; override;
    function FieldMetaToDDL(Source: TObject; const KeyEnabled: Boolean): string;
    procedure ReadLoginSchema;
    function GetLoginSchema: string; override;
  {$IFDEF DEBUG}
    procedure Async_Read_CI_AI(Sender: TObject; const RawValue: Variant);
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateTable(Source: TObject): Boolean; override;
    function ConnectStringODBC(const IncludeAuthorization: Boolean): string; override;
    function ExecuteStoredProcedure(const aProcedureName: string; aList: TDeVariableList): Integer; override;
    procedure RetrieveTableNames(aTablesList: TStringList);  override;
    procedure RetrieveMetaTables(aTablesList : TObjectList); override;
    procedure RetrieveMetaTableInfo(aTable : TObject); override;
    procedure RetrieveProcedures(aProceduresList : TObjectList); override;
    procedure RetrieveProcedureInfo(aTable : TObject); override;
    function RetrieveProcedureParameters(const ProcedureName: string; ParameterList: TObject): Boolean; override;
    function RetrieveProcedureExecQuery(const ProcedureName: string; ParameterList: TObject): String; override;
    function Sequence(const aType: TDeSequenceType; const aTableName, aFieldName: String): Boolean; override;
    /// <summary>функция преобразования имени таблицы к строке по правилам БД</summary>
    function TableToStr(const tableName: String; const tableAlias: String = ''; const tableDatabase: String = '';
                        const tableSchema: String = ''): string; override;
    /// <summary>функция преобразования имени поля к строке по правилам БД</summary>
    function FieldToStr(const fieldName: String; const fieldAlias: String = ''; const tableAlias: String = '';
                        const fieldOperation: TOperationType = opNone; const DataType: TFieldType = ftUnknown; const DataSize: Integer = 0): string; override;
    /// <summary>функция преобразования фунции с параметрами к строке по правилам БД</summary>
    function FuncToStr(aFunction: string; aParams: array of variant): string; override;

    /// <summary>Функция проверяет наличие в базе данных указанной хранимой процедуры</summary>
    /// <param name="StoredProcedureName">имя хранимой процедуры</param>
    /// <returns>True - хранимая процедура есть в базе данных</returns>
    function StoredProcedureExists(const StoredProcedureName: string): Boolean; override;
    function RetrieveMetaObjectType(const ObjectName: string): TObjectType; override;
    property DatabaseDriver: TADODatabaseDriver read FDatabaseDriver;
    // v. 17.01
    function FieldExists(const TableName, FieldName: string): Boolean; override;
    // v. 18.08
    procedure TargetToSourceFieldType(var aFieldData: TFieldData); override;
    function IsFieldTypeConvert(const LocalType: TFieldType; const LocalSize: Integer;
                                var RealType: TFieldType; var RealSize: Integer): Boolean; override;
    class function SupportQueryTypes: TDeQueryTypes; override;
    // v. 18.09
    function SupportDDLs: TSupportDDLs; override;
    function CreateField(Source: TObject): Boolean; override;
    function ModifyField(Source: TObject): Boolean; override;
    function DeleteField(Source: TObject): Boolean; override;
    // v.19.06
    function DatasetPermissions(const DatasetName: string): TDeDatasetPermissions; override;
    function AsyncOpen(const AQueryText: string; AOnDataset: TDeAsyncDatasetEvent; AOnException: TDeAsyncExceptionEvent = nil): Boolean; override;
    function AsyncExecute(const AQueryText: string; AOnExecute: TDeAsyncExecuteEvent; AOnException: TDeAsyncExceptionEvent = nil): Boolean; override;
    // v.19.08
    function CanSchema: Boolean; override;
    // v.21.12
    function ReadServerDateTime(var DateTime: TDateTime): Boolean; override;
  end;

  // база данных Oracle
  {
  TDeOracleDatabase = class(TDeCustomDatabase)
  private
   FConnectionObject : TOraSession;  // подключение к базе данных
   procedure CreateConnection;
   procedure DestroyConnection;
   function GetConnectionObject : TOraSession;
  protected
    function GetConnected : boolean; override;
    procedure SetConnected(const aConnected : boolean); override;
    procedure RetrieveTableNames(aTablesList: TStringList); override;
    function CreateQueryObject(aQueryType : TDeQueryType; InDS:TDeDataset = nil) : TDeDataset;  override;
  public
    property ConnectionObject : TOraSession read GetConnectionObject;
    constructor Create(aOwner : TComponent);  override;
    destructor Destroy;override;
    procedure UploadBLOB(const aTableName, aKeyFieldName, aBlobFieldName : string;
      const aKeyValue:Variant;  aUploadTo:TStream);  override;
    procedure LoadBLOB(const aTableName, aKeyFieldName, aBlobFieldName : string;
      const aKeyValue:Variant;  aLoadFrom:TStream);  override;
  end;
   {}
   {
  TXMLTableRec = record
                   tname : string;
                   tpath : string;
                 end;
  PXMLTableRec = ^TXMLTableRec;

  TXMLTables = array of TXMLTableRec;
  {}
type

  TDeFileDatabase = class(TDeCustomDatabase)
  private
   FConnectionObject : TFileDB;
   procedure CreateConnection;
   procedure DestroyConnection;
   function GetConnectionObject : TFileDB;
  protected
    function GetConnected : boolean; override;
    procedure SetConnected(const aConnected : boolean); override;
    function CreateQueryObject(const aQueryType: TDeQueryType; InDS: TDeDataset = nil): TDeDataset; override;
  public
    property ConnectionObject : TFileDB read GetConnectionObject;
    function GetTablePath(aTableName : string) : string;

    constructor Create(aOwner : TComponent);  override;
    destructor Destroy;override;
    procedure RetrieveTableNames(aTablesList: TStringList); override;
    procedure UploadBLOB(const aTableName, aKeyFieldName, aBlobFieldName : string;
      const aKeyValue:Variant;  aUploadTo:TStream);  override;
    procedure LoadBLOB(const aTableName, aKeyFieldName, aBlobFieldName : string;
      const aKeyValue:Variant;  aLoadFrom:TStream);  override;
  end;

  TXMLDatabaseMode = (xNotConnected, xModeFile, xModeDir, xModeZIP, xModeResource);

  // база данных DBCO XML
  TDBCOXMLDatabase = class(TDeCustomDatabase)
  private
    FMode: TXMLDatabaseMode;
    FConnectionObject: TZipFile;  // подключение к базе данных
    FConnectionDirectory: string;  // подключение к базе данных
    FXMLDocuments: Array of TXMLDocumentISO;
    function GetXMLDocument(Index: Integer): TXMLDocumentISO;
  protected
    function GetConnected: Boolean; override;
    procedure SetConnected(const Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateQueryObject(const aQueryType: TDeQueryType; InDS: TDeDataset = nil): TDeDataset; override;
    function isInternalDataBase: Boolean; override;
    function GetReadOnly: boolean; override;
    procedure RetrieveTableNames(aTablesList: TStringList); override;
    procedure RetrieveMetaTables(aTablesList : TObjectList); override;
    procedure RetrieveMetaTableInfo(aTable : TObject); override;
    function Get_XMLTable(const aTableName: string; const aShemaName: String = ''): TXMLTable;
    property XMLDocuments[Index: Integer]: TXMLDocumentISO read GetXMLDocument;
  //  property ConnectionObject: TZipFile read GetConnectionObject;
  end;

 // база данных ADO XML
  TDeADOXMLDatabase = class(TDeCustomDatabase)
  private
   FDirectory: string;
   FFiles: array of string;
  protected
    function GetConnected : boolean; override;
    procedure SetConnected(const aConnected : boolean); override;
  public
    constructor Create(aOwner : TComponent);  override;
    destructor Destroy;override;
    procedure RetrieveTableNames(aTablesList: TStringList); override;
    procedure RetrieveMetaTables(aTablesList: TObjectList); override;
    procedure RetrieveMetaTableInfo(aTable : TObject); override;

    function CreateQueryObject(const aQueryType: TDeQueryType; InDS: TDeDataset = nil): TDeDataset; override;

    function IndexByName(aName : string): Integer;
    function GetFileName(aIndex: Integer): String;
  end;

  TRegistryHelper = class helper for TRegistry
  public
    function ReadMultiSz(const aName: string; var Strings: TStrings): boolean;
    function WriteMultiSz(const aName: string; const value: TStrings): boolean;
    function ReadStringEx(const aName: string): String;
  end;

  // возвращает версию BDE;  =0, если DBE не установлен
//  function GetBDEVersion : Integer;
  // возвращает версию клиентской части Interbase;  =0, если Interbase не установлен
  function GetIBVersion : integer;
  // возвращает версию клиентской части Oracle;
  function GetOracleVersion: Integer;

type
  TInterbaseType = (itUnknown, itInterbase, itFirebird);

  TInterbaseInformation = packed record
    InterbaseType: TInterbaseType;
    MajorVersion: Word;
    MinorVersion: Word;
    ReleaseVersion: Word;
    BuildVersion: Word;
    ProductName: string;
    ProductAddition: string;
  end;

  TMSSQLInformation = packed record
    MajorVersion: Word;
    MinorVersion: Word;
    ReleaseVersion: Word;
    BuildVersion: Word;
    ProductName: string;
    ProductEdition: string;
  end;

/// <summary>Возвращает информацию о клиентской части Interbase</summary>
function GetInterbaseInformation(var Information: TInterbaseInformation): Boolean;

/// <summary>Возвращает информацию о клиентском ADO</summary>
function GetMDACVersion: Integer;

/// <summary>Возвращает информацию о Native Client</summary>
function GetNativeClientVersion: Integer;

/// <summary>Возвращает информацию о версии установленного Microsoft SQL Server`а</summary>
function GetMSSQLInformation(var Information: TMSSQLInformation): Boolean;

implementation

uses Forms, IBTable, IBHeader, StrUtils, System.Win.ComObj,
     {$IFDEF INDYUSED}IdCookieManager, {$ENDIF}DCPmd5,
     DeLog, DataCacheUnit, DeSettings, Funcs, {DataUnit, } DeMeta, Controls, DataUnit,
     DeMetadata, DeScript, DeCalculator;

function TRegistryHelper.ReadMultiSz(const aName: string; var Strings: TStrings): boolean;
var
  iSizeInByte: integer;
  Buffer: array of WChar;
  iWCharsInBuffer: integer;
  z: integer;
  sString: string;
begin
  iSizeInByte:= GetDataSize(aName);
  if iSizeInByte > 0 then
    begin
      SetLength(Buffer, Floor(iSizeInByte / sizeof(WChar)));
      iWCharsInBuffer := Floor(ReadBinaryData(aName, Buffer[0], iSizeInByte) / sizeof(WChar));
      sString:= EmptyStr;
      for z:= 0 to iWCharsInBuffer do
        begin
          if Buffer[z] <> #0 then
            begin
              sString:= sString + Buffer[z];
            end else
            begin
              if sString <> EmptyStr then
                begin
                  Strings.Append(sString);
                  sString:= EmptyStr;
                end;
            end;
        end;
      result:= true;
    end
  else
    begin
      result:= false;
    end;
end;

function TRegistryHelper.WriteMultiSz(const aName: string; const value: TStrings): boolean;
var
  sContent: string;
  x: integer;
begin
  sContent:= EmptyStr;
  for x:= 0 to pred(value.Count) do
    sContent:= sContent + value.Strings[x] + #0;
  sContent:= sContent + #0;
  result:= RegSetValueEx(CurrentKey, pchar(aName), 0, REG_MULTI_SZ, pointer(sContent), Length(sContent)*sizeof(Char)) = 0;
end;

function TRegistryHelper.ReadStringEx(const aName: string): String;
var
  TRI: TRegDataInfo;
  sList: TStrings;
begin
  try
    Result:= EmptyStr;
    GetDataInfo(aName, TRI);
    if TRI.RegData = rdString then
      Result:= ReadString(aName)
    else
      try
        sList:= TStringList.Create;
        if ReadMultiSz(aName, sList) then Result:= sList.GetText;
      finally
        sList.Free;
      end;
  except
    Result := EmptyStr;
  end;
end;

{
function GetBDEVersion : Integer;
  procedure Check(Status: DBIResult);
  begin
    if Status <> 0 then DbiError(Status);
  end;
var SV : SysVersion;
begin
  try
    Check(dbiInit(nil));
    Check(DbiGetSysVersion(SV));
    Check(DbiExit);
    result := SV.iVersion;
  except
    result := 0;
  end;
end;
{}

function GetInterbaseInformation(var Information: TInterbaseInformation): Boolean;
const
  GDS32 = 'gds32.dll'; //'FBClient.dll';
  cInterbase = 'Interbase';
  cFirebird = 'Firebird';
  cCompatibleIB = 'Compatible IB ';
var
  Handle: THandle;
  BufferSize, Size: Cardinal;
  Buffer: Pointer;
  FileInfo: PVSFixedFileInfo;
  Translation: PCardinal;
  Value: string;
  function ReadString(const Name: string): string;
  var
    StringBuffer: PChar;
  begin
    if VerQueryValue(Buffer, PChar(Format('\StringFileInfo\%.4x%.4x\%s', [LoWord(Translation^), HiWord(Translation^), Name])), Pointer(StringBuffer), BufferSize) then
      Result := StrPas(StringBuffer)
    else
      Result := EmptyStr;
  end;
  function IsStringReady(const Name: string): Boolean;
  begin
    Result := Pos(UpperCase(Name), UpperCase(Value)) <> 0;
  end;
  function PrepareVersion: string;
  begin
    Result := Format('%d.%d', [Information.MajorVersion, Information.MinorVersion]);
  end;
  procedure AppendProductAddition(const Value: string);
  begin
    if Length(Information.ProductAddition) <> 0 then
      Information.ProductAddition := Information.ProductAddition + ' ';
    Information.ProductAddition := Information.ProductAddition + Value;
  end;
  procedure AppendProductSP(const Number: Word);
  begin
    AppendProductAddition(Format('SP%u', [Number]));
  end;
  procedure AppendProductUpdate(const Number: Word);
  begin
    AppendProductAddition(Format('Update %u', [Number]));
  end;
  procedure AppendProductHotfix(const Number: Word);
  begin
    AppendProductAddition(Format('Hotfix %u', [Number]));
  end;
begin
  ZeroMemory(@Information, SizeOf(Information));
  Handle := LoadLibraryEx(GDS32, 0, LOAD_LIBRARY_AS_DATAFILE);
  Result := Handle <> 0;
  if Result then
    try
     BufferSize := GetFileVersionInfoSize(GDS32, Size);
     Result := BufferSize <> 0;
     if Result then
       begin
         GetMem(Buffer, BufferSize);
         try
           Result := GetFileVersionInfo(PChar(GDS32), Size, BufferSize, Buffer);
           if Result then
             begin
               Result := VerQueryValue(Buffer, '\', Pointer(FileInfo), BufferSize);
               if Result then
                 begin
                   Information.MajorVersion := HiWord(FileInfo^.dwFileVersionMS);
                   Information.MinorVersion := LoWord(FileInfo^.dwFileVersionMS);
                   Information.ReleaseVersion := HiWord(FileInfo^.dwFileVersionLS);
                   Information.BuildVersion := LoWord(FileInfo^.dwFileVersionLS);
                   if VerQueryValue(Buffer, '\VarFileInfo\Translation', Pointer(Translation), BufferSize) then
                     begin
                       Value := ReadString('ProductName');
                       if IsStringReady(cInterbase) then
                         Information.InterbaseType := itInterbase
                       else if IsStringReady(cFirebird) then
                         Information.InterbaseType := itFirebird
                       else
                         begin
                           Value := ReadString('FileDescription');
                           if IsStringReady(cInterbase) then
                             Information.InterbaseType := itInterbase
                           else if IsStringReady(cFirebird) then
                             Information.InterbaseType := itFirebird;
                         end;
                     end;
                   case Information.InterbaseType of
                     itInterbase:
                       case Information.MajorVersion of
                         13: { 2017 }
                           Information.ProductName := cInterbase + ' 2017';
                         12: { XE7 }
                           Information.ProductName := cInterbase + ' XE7';
                         11: { XE3 }
                           begin
                             Information.ProductName := cInterbase + ' XE3';
                             if Information.MinorVersion = 0 then
                               case Information.ReleaseVersion of
                                1: { Update 1 }
                                  AppendProductUpdate(1);
                                2: { Update 2 }
                                  begin
                                    AppendProductUpdate(2);
                                    AppendProductHotfix(1);
                                  end;
                                3: { Update 3 }
                                  AppendProductUpdate(3);
                                4: { Update 4 }
                                  begin
                                    AppendProductUpdate(4);
                                    AppendProductHotfix(2);
                                  end;
                                5: { Update 5 }
                                  AppendProductUpdate(5);
                                6: { Update 6 }
                                  AppendProductUpdate(6);
                               end;
                           end;
                         10: { XE }
                           Information.ProductName := cInterbase + ' XE';
                         9: { 2009 }
                           Information.ProductName := cInterbase + ' 2009';
                         8: { 2007 }
                           begin
                             Information.ProductName := cInterbase + ' 2007';
                             if Information.MinorVersion = 1 then
                               case Information.ReleaseVersion of
                                0: { SP2 }
                                  AppendProductSP(2);
                                1: { SP3 }
                                  AppendProductSP(3);
                               end;
                           end;
                       else
                         Information.ProductName := cInterbase + ' ' + PrepareVersion;
                       end;
                     itFirebird:
                       begin
                         Information.ProductName := cFirebird + ' ' + PrepareVersion;
                         if Information.MajorVersion = 1 then
                           case Information.MinorVersion of
                             0: { 1.0 }
                               case Information.ReleaseVersion of
                                 2: { SP1 }
                                   AppendProductSP(1);
                                 3: { SP2 }
                                   AppendProductSP(2);
                               end;
                             5: { 1.5 }
                               case Information.ReleaseVersion of
                                 1: { SP1 }
                                   AppendProductSP(1);
                                 2: { SP2 }
                                   AppendProductSP(2);
                                 3: { SP3 }
                                   AppendProductSP(3);
                                 4: { SP4 }
                                   AppendProductSP(4);
                                 5: { SP5 }
                                   AppendProductSP(5);
                                 6: { SP6 }
                                   AppendProductSP(6);
                               end;
                           end;
                       end;
                   else
                     Information.ProductName := cCompatibleIB + PrepareVersion;
                   end;
                 end;
             end;
         finally
           FreeMem(Buffer);
         end;
       end;
    finally
      FreeLibrary(Handle);
    end;
end;

function GetIBVersion: Integer;
var
  InterbaseInformation: TInterbaseInformation;
begin
  try
    //result := 6;// GetIBClientVersion;
    GetInterbaseInformation(InterbaseInformation);
    Result := InterbaseInformation.MajorVersion;
  except
    result := 0;
  end;
end;

function GetMDACVersion: Integer;
const
  cMDAC28 = '{2A75196C-D9EB-4129-B803-931327F72D5C}';
  iMDAC28 = $00020008;
  cMDAC27 = '{EF53050B-882E-4776-B643-EDA472E8E3F2}';
  iMDAC27 = $00020007;
  cMDAC26 = '{00000206-0000-0010-8000-00AA006D2EA4}';
  iMDAC26 = $00020006;
  cMDAC25 = '{00000205-0000-0010-8000-00AA006D2EA4}';
  iMDAC25 = $00020005;
  cMDAC21 = '{00000201-0000-0010-8000-00AA006D2EA4}';
  iMDAC21 = $00020001;
  cMDAC20 = '{00000200-0000-0010-8000-00AA006D2EA4}';
  iMDAC20 = $00020000;
  cMDAC15 = '{B691E011-1797-432E-907A-4D8C69339129}';
  iMDAC15 = $00010005;
var
  Registry: TRegistry;
  function IsSupport(const Name: string): Boolean;
  begin
    Result := Registry.OpenKeyReadOnly('\TypeLib\' + Name);
    Registry.CloseKey;
  end;
begin
  try
    Registry := TRegistry.Create;
    try
      Registry.RootKey := HKEY_CLASSES_ROOT;
        if IsSupport(cMDAC28) then
          Result := iMDAC28
        else if IsSupport(cMDAC27) then
          Result := iMDAC27
        else if IsSupport(cMDAC26) then
          Result := iMDAC26
        else if IsSupport(cMDAC25) then
          Result := iMDAC25
        else if IsSupport(cMDAC21) then
          Result := iMDAC21
        else if IsSupport(cMDAC20) then
          Result := iMDAC20
        else if IsSupport(cMDAC15) then
          Result := iMDAC15
        else
          Result := 0;
    finally
      Registry.Free;
    end;
  except
    Result := 0;
  end;
end;

function GetNativeClientVersion: Integer;
const
  cNCLI9 = 'SQLNCLI';
  iNCLI9 = $00090000;
  cNCLI10 = 'SQLNCLI10';
  iNCLI10 = $000A0000;
  cNCLI11 = 'SQLNCLI11';
  iNCLI11 = $000B0000;
var
  Registry: TRegistry;
  function IsSupport(const Name: string): Boolean;
  var
    Value: string;
  begin
    Result := Registry.OpenKeyReadOnly('\' + Name + '\Clsid');
    try
      if Result then
        begin
          Value := Trim(Registry.ReadString(EmptyStr));
          Registry.CloseKey;
          if (Length(Value) <> 0) and Registry.OpenKeyReadOnly('\CLSID\' + Value + '\InprocServer32') then
            begin
              Value := Trim(Registry.ReadString(EmptyStr));
              Result := (Length(Value) <> 0) and FileExists(Value);
            end
          else
            Result := False;
        end
    finally
      Registry.CloseKey;
    end;
  end;
begin
  try
    Registry := TRegistry.Create;
    try
      Registry.RootKey := HKEY_CLASSES_ROOT;
        if IsSupport(cNCLI11) then
          Result := iNCLI11
        else if IsSupport(cNCLI10) then
          Result := iNCLI10
        else if IsSupport(cNCLI9) then
          Result := iNCLI9
        else
          Result := 0;
    finally
      Registry.Free;
    end;
  except
    Result := 0;
  end;
end;

function GetOracleVersion: Integer;
var
  Registry: TRegistry;
  FileName: string;
  VersionMS, VersionLS: Cardinal;
begin
  Result := 0;
  {$IFDEF DEBUG}
  Funcs.WriteLog('GetOracleVersion begin ...', True, 'OraOLEDB');
  {$ENDIF}
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CLASSES_ROOT;
    if Registry.OpenKeyReadOnly('\OraOLEDB.Oracle\CLSID') then
      begin
        Funcs.WriteLog('\OraOLEDB.Oracle\CLSID open completed ...', True, 'OraOLEDB');
        FileName := Registry.ReadString(EmptyStr);
        Funcs.WriteLog('Read CLSID %s completed ...', [FileName], True, 'OraOLEDB');
        Registry.CloseKey;
        if Registry.OpenKeyReadOnly('\CLSID\' + FileName + '\InprocServer32') then
          begin
            Funcs.WriteLog('\CLSID\' + FileName + '\InprocServer32 open completed ...', True, 'OraOLEDB');
            FileName := Registry.ReadString(EmptyStr);
            Funcs.WriteLog('Read file name %s completed ...', [QuotedStr(FileName)], True, 'OraOLEDB');
            Registry.CloseKey;
          end
        else
          FileName := EmptyStr;
      end
    else
      FileName := EmptyStr;
  finally
    Registry.Free;
  end;
  if GetFileVersion(FileName, VersionMS, VersionLS) then
    Result := VersionMS;
  {$IFDEF DEBUG}
  Funcs.WriteLog('GetOracleVersion end and return %d ...', [Result], True, 'OraOLEDB');
  {$ENDIF}
end;

function GetMSSQLInformation(var Information: TMSSQLInformation): Boolean;
const
  cMicrosoftSQLServerSection = '\SOFTWARE\Microsoft\Microsoft SQL Server';
  cServiceSection = '\SYSTEM\CurrentControlSet\services\SQLWriter';
  cSqlWriterSection = '\SqlWriter\CurrentVersion';
  cInstanceNamesSection = '\Instance Names\SQL';
  cVersionIdent = 'Version';
  cInstalledInstancesIdent = 'InstalledInstances';
  cEditionIdent = 'Edition';
  cEditionTypeIdent = 'EditionType';
  cImagePathIdent = 'ImagePath';
var
  Value: string;
  Version: Integer;
  VersionMS, VersionLS: Cardinal;
begin
  ZeroMemory(@Information, SizeOf(Information));
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      Result := OpenKeyReadOnly(cMicrosoftSQLServerSection + cSqlWriterSection) and ValueExists(cVersionIdent);
      if Result then
        begin
          Value := ReadString(cVersionIdent);
          Result := TryStrToInt(CutTextValue(Value, '.'), Version);
          if Result then
            begin
              Information.MajorVersion := Version;
              Result := TryStrToInt(CutTextValue(Value, '.'), Version);
              if Result then
                begin
                  Information.MinorVersion := Version;
                  Information.ReleaseVersion := StrToIntDef(CutTextValue(Value, '.'), 0);
                  Information.BuildVersion := StrToIntDef(CutTextValue(Value, '.'), 0);
                end;
            end;
        end
      else
        begin
          Result := OpenKeyReadOnly(cServiceSection) and ValueExists(cImagePathIdent);
          if Result then
            begin
              Value := ReadString(cImagePathIdent);
              while (Length(Value) <> 0) and (Value[1] = '"') do Delete(Value, 1, 1);
              while (Length(Value) <> 0) and (Value[Length(Value)] = '"') do Delete(Value, Length(Value), 1);
              Result := GetFileVersion(Value, VersionMS, VersionLS, True);
              if Result then
                begin
                  Information.MajorVersion := HiWord(VersionMS);
                  Information.MinorVersion := LoWord(VersionMS);
                  Information.ReleaseVersion := HiWord(VersionLS);
                  Information.BuildVersion := LoWord(VersionLS);
                end;
            end;
        end;
      if Result then
        begin
          CloseKey;
          if OpenKeyReadOnly(cMicrosoftSQLServerSection) and ValueExists(cInstalledInstancesIdent) then
            begin
              try
                Value := Trim(ReadStringEx(cInstalledInstancesIdent));
              except
                Value := EmptyStr;
              end;
              CloseKey;
              if (Length(Value) <> 0) and OpenKeyReadOnly(cMicrosoftSQLServerSection + cInstanceNamesSection) and ValueExists(Value) then
                begin
                  Value := Trim(ReadString(Value));
                  CloseKey;
                  if (Length(Value) <> 0) and OpenKeyReadOnly(Format('%s\%s\Setup\', [cMicrosoftSQLServerSection, Value])) then
                    if ValueExists(cEditionIdent) then
                      Information.ProductEdition := Trim(ReadString(cEditionIdent))
                    else if ValueExists(cEditionTypeIdent) then
                      Information.ProductEdition := Trim(ReadString(cEditionTypeIdent));
                end;
            end;
        end;
    finally
      Free;
    end;
  if Result then
    case Information.MajorVersion of
      8: { 2000 }
        begin
          Information.ProductName := 'MSSQL 2000';
          if Information.MinorVersion = 0 then
            if Information.ReleaseVersion >= 2039 then
              Information.ProductName := Information.ProductName + ' SP4'
            else if Information.ReleaseVersion >= 760 then
              Information.ProductName := Information.ProductName + ' SP3'
            else if Information.ReleaseVersion >= 534 then
              Information.ProductName := Information.ProductName + ' SP2'
            else if Information.ReleaseVersion >= 384 then
              Information.ProductName := Information.ProductName + ' SP1';
        end;
      9: { 2005 }
        begin
          Information.ProductName := 'MSSQL 2005';
          if Information.MinorVersion = 0 then
            if Information.ReleaseVersion >= 5000 then
              Information.ProductName := Information.ProductName + ' SP4'
            else if Information.ReleaseVersion >= 4035 then
              Information.ProductName := Information.ProductName + ' SP3'
            else if Information.ReleaseVersion >= 3042 then
              Information.ProductName := Information.ProductName + ' SP2'
            else if Information.ReleaseVersion >= 2047 then
              Information.ProductName := Information.ProductName + ' SP1';
        end;
      10: { 2008 }
        begin
          Information.ProductName := 'MSSQL 2008';
          case Information.MinorVersion of
            0: { Обычный }
              if Information.ReleaseVersion >= 6000 then
                Information.ProductName := Information.ProductName + ' SP4'
              else if Information.ReleaseVersion >= 5500 then
                Information.ProductName := Information.ProductName + ' SP3'
              else if Information.ReleaseVersion >= 4000 then
                Information.ProductName := Information.ProductName + ' SP2'
              else if Information.ReleaseVersion >= 2531 then
                Information.ProductName := Information.ProductName + ' SP1';
            50: { R2 }
              begin
                Information.ProductName := Information.ProductName + ' R2';
                if Information.ReleaseVersion >= 6000 then
                  Information.ProductName := Information.ProductName + ' SP3'
                else if Information.ReleaseVersion >= 4000 then
                  Information.ProductName := Information.ProductName + ' SP2'
                else if Information.ReleaseVersion >= 2500 then
                  Information.ProductName := Information.ProductName + ' SP1';
              end;
          end;
        end;
      11: { 2012 }
        begin
          Information.ProductName := 'MSSQL 2012';
          if Information.MinorVersion = 0 then
            if Information.ReleaseVersion >= 7001 then
              Information.ProductName := Information.ProductName + ' SP4'
            else if Information.ReleaseVersion >= 6020 then
              Information.ProductName := Information.ProductName + ' SP3'
            else if Information.ReleaseVersion >= 5058 then
              Information.ProductName := Information.ProductName + ' SP2'
            else if Information.ReleaseVersion >= 3000 then
              Information.ProductName := Information.ProductName + ' SP1';
        end;
      12: { 2014 }
        begin
          Information.ProductName := 'MSSQL 2014';
          if Information.MinorVersion = 0 then
            if Information.ReleaseVersion >= 6000 then
              Information.ProductName := Information.ProductName + ' SP3'
            else if Information.ReleaseVersion >= 5000 then
              Information.ProductName := Information.ProductName + ' SP2'
            else if Information.ReleaseVersion >= 4100 then
              Information.ProductName := Information.ProductName + ' SP1';
        end;
      13: { 2016 }
        begin
          Information.ProductName := 'MSSQL 2016';
          if Information.MinorVersion = 0 then
            if Information.ReleaseVersion >= 6300 then
              Information.ProductName := Information.ProductName + ' SP3'
            else if Information.ReleaseVersion >= 5026 then
              Information.ProductName := Information.ProductName + ' SP2'
            else if Information.ReleaseVersion >= 4001 then
              Information.ProductName := Information.ProductName + ' SP1';
        end;
      14: { 2017 }
        Information.ProductName := 'MSSQL 2017';
      15: { 2019 }
        Information.ProductName := 'MSSQL 2019';
      16: { 2022 }
        Information.ProductName := 'MSSQL 2022';
    else
      if Information.MajorVersion < 8 then
        Information.ProductName := Format('MSSSQL %d.%d', [Information.MajorVersion, Information.MinorVersion]);
    end;
end;

{ -------------------------------------------------------------------------------------------------------------------- }
{ --- TDeSQLDatabase --------------------------------------------------------- }
{ -------------------------------------------------------------------------------------------------------------------- }

procedure TDeSQLDatabase.CreateConnection;
begin
  DestroyConnection;
  FConnectionObject := CreateConnectionObject;
  if Assigned(FConnectionObject) then
    FConnectionObject.FreeNotification(Self);
end;

procedure TDeSQLDatabase.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(aComponent, Operation);
  if Operation = opRemove then
    if aComponent = FCOnnectionObject then
      FConnectionObject := nil;
end;

procedure TDeSQLDatabase.RetrieveMetaTableInfo(aTable: TObject);
var Q: TDeDataset;
    I, P  : integer;
    FMeta : TFieldMeta;
    FName : String;
    TF:TField;
    TM: TTableMeta;
begin
  Assert( Assigned(aTable), 'RetrieveMetaTableInfo');
  Assert( aTable is TTableMeta, 'RetrieveMetaTableInfo');
  Assert( Assigned(FConnectionObject), ETableCantOpenError);

  if Not CheckConnection then Exit;

  TM:= TTableMeta(aTable);

  Q:= CreateQuery(qtSelect);
  try
    Q.Descr.Table:= TM.Table;
    try
    Q.Open(False);

    TM.Fields.Clear;

    for I := 0 to Pred(TDataSet(Q.SourceSet).FieldList.Count) do
    begin
       TF:= TDataSet(Q.SourceSet).Fields[i];
       FMeta := TFieldMeta.Create;
       FMeta.Owner := TM;
       FMeta.Original := TF.FieldName;
       P := Pos('_',FMeta.Original);
       if P = 0 then
         FName := '_'+FMeta.Original
       else
         FName := Copy(FMeta.Original, P, MaxInt);

       if Length(FName)>2 then
         FMeta.Name := UpperCase(Copy(FName,1,2)) + LowerCase(Copy(FName, 3, MaxInt))
       else
         FMeta.Name := FName;

       FMeta.IsReadOnly := TF.ReadOnly;
       FMeta.Key := TF.IsIndexField;
       FMeta.NotNull := TF.Required;
       FMeta.Unique := False;
       FMeta.Calculated := False;
       FMeta.IsStored := True;
       FMeta.DataType := TF.DataType;
       FMeta.DataSize := TF.Size;
       FMeta.Link := 0;
       FMeta.LinkType := daNone;
       FMeta.Role := [];
       FMeta.VisibleLevel  := fvLevel2;
       FMeta.ShowType := stNative;
       FMeta.Width := 20;
       FMeta.Order := I;
       FMeta.Template := EmptyStr;
       FMeta.DefaultValue := EmptyStr;
       FMeta.DefaultDefDB := EmptyStr;
       FMeta.Value1  := EmptyStr;
       FMeta.Value2  := EmptyStr;
       TM.Fields.Add(FMeta);
    end;

    except
      on E: Exception do
        begin
          Funcs.WriteLog(E.Message);
          raise Exception.Create(E.Message);
        end;
    end;
  finally
    Q.Free;
  end;
end;

procedure TDeSQLDatabase.DestroyConnection;
begin
  if Assigned(FConnectionObject) then
    begin
      FConnectionObject.RemoveFreeNotification(Self);
      FreeAndNil(FConnectionObject);
    end;
end;

function TDeSQLDatabase.GetConnectionObject : TCustomConnection;
begin
  if not Assigned(FConnectionObject) then
    CreateConnection;
  result := FConnectionObject;
end;

function TDeSQLDatabase.GetConnected : boolean;
begin
  result := Assigned(ConnectionObject) and ConnectionObject.Connected;
end;

procedure TDeSQLDatabase.SetConnected(const aConnected : boolean);
begin
  inherited SetConnected(aConnected);
  if aConnected then
    if Assigned(ConnectionObject) then
      FConnectionObject.Connected := aConnected
    else
      LastErrorDBType := DatabaseType  // проблемы с версией клиента
  else
    DestroyConnection;  // если класс для коннекта к базе больше не нужен, то удалем его
end;

procedure TDeSQLDatabase.UploadBLOB(const aTableName, aKeyFieldName,
  aBlobFieldName : string;  const aKeyValue:Variant;  aUploadTo:TStream);
var
  D: TDeDataset;
  BlockSize: Integer;
  aValue: Variant;
  P: Pointer;
begin
  D:= CreateQuery(qtSelect);
  try
    D.Descr.Table:= aTableName;
    D.Descr.AddCondition(aKeyFieldName, ftUnknown, opEQ, aKeyValue);
    D.Descr.AddField(aBlobFieldName);
    D.Open;
    if D.RecordCount = 1 then
    try
      aValue := D.Value[0];
      BlockSize := VarArrayHighBound(aValue, 1) - VarArrayLowBound(aValue, 1) + 1;
      P := VarArrayLock(aValue);
      try
        aUploadTo.Write(P, BlockSize);
      finally
        VarArrayUnlock(aValue);
      end;
    except
      {$IFDEF DEBUG}
      on E: Exception do
        DebugLog(ClassName + '.UploadBLOB error: ' + E.Message);
      {$ENDIF}
    end;
  finally
    D.Free;
  end;
end;

procedure TDeSQLDatabase.LoadBLOB(const aTableName, aKeyFieldName,
  aBlobFieldName : string;  const aKeyValue:Variant;  aLoadFrom:TStream);
begin
  with CreateTableDataSet(aTableName) do
  try
    Open;
    Locate(aKeyFieldName, aKeyValue, []);
    Edit;
    if Assigned(aLoadFrom) then
      TBLOBField(FieldByName(aBlobFieldName)).LoadFromStream(aLoadFrom)
    else
      TBLOBField(FieldByName(aBlobFieldName)).Clear;
    Post;
    Close;
  finally
    Free;
  end;
end;

function TDeSQLDatabase.CanPortionSelect: Boolean;
begin
  Result := True;
end;

{function TDeSQLDatabase.SQLExecuteResult(const SQLText : string) : Variant;
var DS : TDeDataset;
begin
  Result:=Unassigned;
  DS := CreateQuery(qtSelect);
  DS.Descr.SQL := SQLText;
  try
    try
      DS.Open;
      if DS.FieldsCount > 0 then
        result := DS.Value[0];
      DS.Close;
    except
    end
  finally
    DS.Free;
  end;
end;}

{ --- TDeBDEDatabase --------------------------------------------------------- }
{
constructor TDeBDEDatabase.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  DatabaseType := dtBDE;
end;

function TDeBDEDatabase.CreateConnectionObject : TCustomConnection;
var TempPath : string;
    ph       : PWideChar;
begin
  GetMem(ph, 255);
  GetTempPath(254, ph);
  TempPath := StrPas(ph);
  FreeMem(ph);
  if CurrentBDEVersion >= MinBDEVersion then
  begin
    result := TDatabase.Create(Application);
    result.LoginPrompt := False;
    with TDatabase(result) do
    begin
      Directory := TempPath;
      DatabaseName := ConnectString;
      Params.Add('user_name = ' + Login);
      Params.Add('password = ' + Password);
      KeepConnection:=True;
    end
  end
  else result:= nil;
end;

function TDeBDEDatabase.CreateTableDataset(const aTableName : string) : TDataset;
begin
  result := TTable.Create(ConnectionObject);
  TTable(result).SessionName  := TDatabase(ConnectionObject).Session.SessionName;
  TTable(result).DataBaseName := TDatabase(ConnectionObject).DatabaseName;
  TTable(result).TableName := aTableName;
end;

procedure TDeBDEDatabase.RetrieveTableNames(aTablesList: TStringList);
var BackupCursor  : TCursor;
begin
  BackupCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    TDatabase(ConnectionObject).GetTableNames(FTablesList);
  finally
    Screen.Cursor := BackupCursor;
  end;
end;

procedure TDeBDEDatabase.RetrieveMetaTables(aTablesList : TObjectList);
var TM  : TTableMeta;
    sr: TSearchRec;
begin
  if SysUtils.FindFirst(Database+'\'+'*.dbf', faAnyFile, sr) = 0 then
    begin
      repeat
        TM:=TTableMeta.Create;
        TM.SetTable    := sr.Name;
        TM.IsReadOnly  := ((FILE_ATTRIBUTE_READONLY and sr.Attr) > 0);
        TM.Description := EmptyStr;
        TM.SetName     := '_dF.table '+TM.Table;
        aTablesList.Add(TM);
      until SysUtils.FindNext(sr) <> 0;
      SysUtils.FindClose(sr);
    end;
end;

procedure TDeBDEDatabase.RetrieveMetaTableInfo(aTable : TObject);
var DS  : TTable;
    FM  : TFieldMeta;
    i,p : Integer;
    FL  : TFieldsMeta;
begin
  Assert( Assigned(aTable), 'RetrieveMetaTableInfo');
  Assert( aTable is TTableMeta, 'RetrieveMetaTableInfo');
  Assert( Assigned(FConnectionObject), ETableCantOpenError);

  if Not CheckConnection then Exit;

  try
    DS:=TTable.Create(ConnectionObject);
    TTable(DS).SessionName  := TDatabase(ConnectionObject).Session.SessionName;
    TTable(DS).DataBaseName := TDatabase(ConnectionObject).DatabaseName;
    TTable(DS).TableName := TTableMeta(aTable).Table;
    DS.Open;

    FL:=TFieldsMeta.create;
    for i:=0 to DS.Fields.Count-1 do
      begin
        FM:=TFieldMeta.Create;
        FM.Original := DS.Fields[i].FieldName;
        FM.NotNull  := False;
        FM.Order    := i;
        FM.IsStored := True;
        FM.DataType := DS.Fields[i].DataType;
        FM.DataSize := DS.Fields[i].DataSize;
        FM.Width    := DS.Fields[i].DisplayWidth;
        FM.Alignment := taLeftJustify;
        FM.Calculated := DS.Fields[i].Calculated;
        FM.IsReadOnly := not DS.Fields[i].CanModify;
        FM.Name     := DS.Fields[i].FieldName;
          p:=Pos('_',FM.Name); if p>0 then FM.Name:=Copy(FM.Name,p,MaxInt);

        FM.Owner:=TTableMeta(aTable);
        FL.Add(FM);
      end;
    DS.Close;
    DS.Free;

    TTableMeta(aTable).AssignFields(FL);
  except
  end;
end;

function TDeBDEDatabase.CreateQueryObject(aQueryType : TDeQueryType; InDS:TDeDataset = nil) : TDeDataset;
begin
  result := TDeBDEDataset.Create(Self);
  if Assigned(ConnectionObject) then
    with TQuery(TDeBDEDataset(result).Data) do
  begin
    SessionName  := TDatabase(ConnectionObject).Session.SessionName;
    DataBaseName := TDatabase(ConnectionObject).DatabaseName;
  //  RequestLive := true;
  end;
end;
}
{ -------------------------------------------------------------------------------------------------------------------- }
{ --- TDeIBDatabase ---------------------------------------------------------- }
{ -------------------------------------------------------------------------------------------------------------------- }

constructor TDeIBDatabase.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  DatabaseType := dtInterbase;
end;

function TDeIBDatabase.BuildConnectString : string;
begin
  if Length(Server) > 0 then
    result := Format('%s:%s', [Server, Database])
  else
    result := inherited BuildConnectString;

(*
  { определяем, содержит ли строка подключения полный путь к данных }
  if DataBaseType in [dtInterbase, dtBDE] then
    begin
                      p:= Pos('\', ConnectString);
      if (p = 0) then p:= Pos('/', ConnectString);
      if (p = 0) then ConnectString := AddDataPath(ConnectString, MetaData.MetadataDB.ConnectString);
    end;
*)
end;

function TDeIBDatabase.CreateConnectionObject : TCustomConnection;
begin
  if CurrentIBVersion >= MinIBVersion then
  begin
    result := TIBDatabase.Create(Application);
    result.LoginPrompt := False;
    with TIBDatabase(result) do
    begin
      DefaultTransaction := TIBTransaction.Create(result);
      DefaultTransaction.AutoStopAction := saCommit;
      DatabaseName := ConnectString;
      Params.Add('user_name=' + Login);
      Params.Add('password=' + Password);
    end;
  end
  else result:= nil;
end;

function TDeIBDatabase.CreateTableDataset(const aTableName : string) : TDataset;
begin
  result := TIBTable.Create(ConnectionObject);
  TIBTable(result).Database := TIBDataBase(ConnectionObject);
  TIBTable(result).TableName:= aTableName;
end;

procedure TDeIBDatabase.RetrieveTableNames(aTablesList: TStringList);
var BackupCursor  : TCursor;
begin
  BackupCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    TIBDatabase(ConnectionObject).GetTableNames(aTablesList);
  finally
    Screen.Cursor := BackupCursor;
  end;
end;

procedure TDeIBDatabase.RetrieveMetaTables(aTablesList : TObjectList);
var Q   : TIBQuery;
    TM  : TTableMeta;
    p   : Integer;
begin
  Q:=TIBQuery.Create(nil);
  try
    Q.Database:=TIBDataBase(ConnectionObject);
    Q.Transaction := Q.Database.InternalTransaction;
    Q.Transaction.AutoStopAction := saCommit;
    Q.SQL.Text :=
      'select RDB$RELATION_NAME, RDB$VIEW_BLR, RDB$DESCRIPTION ' +
      'from RDB$RELATIONS where RDB$SYSTEM_FLAG = 0';
    WriteQueryLog(Q);
    Q.Open;
    while Not Q.Eof do
      begin
        TM:= TTableMeta.Create(Q.Current.ByName('RDB$RELATION_NAME').AsString, self);
        TM.IsReadOnly  := Not  Q.Current.ByName('RDB$VIEW_BLR').IsNull;
        TM.Description := Trim(Q.Current.ByName('RDB$DESCRIPTION').AsString);

        p:=Pos(#13,TM.Description); //ищем разрыв строки
        TM.Name := Copy(TM.Description, 1, p-1);
        if Length(TM.Name)=0 then TM.Name := '_dF.table '+TM.Table;

        aTablesList.Add(TM);
        Q.Next;
      end;
    Q.Close;
  finally
  end;
  Q.Free;
end;

// почти на 100 содрано с procedure TIBTable.InitFieldDefs;
procedure TDeIBDatabase.RetrieveMetaTableInfo(aTable : TObject);
var Q       : TIBQuery;
    FL      : TFieldsMeta;
    FM      : TFieldMeta;
    p,scale : Integer;
begin
  Assert( Assigned(aTable), 'RetrieveMetaTableInfo');
  Assert( aTable is TTableMeta, 'RetrieveMetaTableInfo');
  Assert( Assigned(FConnectionObject), ETableCantOpenError);

  if Not CheckConnection then Exit;

  Q:=TIBQuery.Create(TIBDataBase(ConnectionObject));
  try
    FL:=TFieldsMeta.create;

    Q.Transaction := TIBDataBase(ConnectionObject).InternalTransaction;
    Q.Transaction.AutoStopAction := saCommit;

    // Читаем поля .............................................................
    Q.SQL.Text :=
      'select R.RDB$FIELD_NAME, F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE, '+
      'F.RDB$FIELD_LENGTH, F.RDB$FIELD_SCALE,  R.RDB$NULL_FLAG, '+
      'R.RDB$DESCRIPTION, R.RDB$DEFAULT_VALUE, R.RDB$FIELD_POSITION, '+
      'R.RDB$FIELD_ID '+
      'from RDB$RELATION_FIELDS R , RDB$FIELDS F '+
      'where F.RDB$FIELD_NAME = R.RDB$FIELD_SOURCE and R.RDB$SYSTEM_FLAG = 0 '+
                                       'and R.RDB$RELATION_NAME = :TABLENAME '+
      'order by R.RDB$FIELD_POSITION';

    Q.ParamByName('TABLENAME').Value := TTableMeta(aTable).Table;
    WriteQueryLog(Q);
    Q.Open;
    while Not Q.Eof do
      begin
        FM:=TFieldMeta.Create;
        FM.Original:= Trim(Q.Current.ByName('RDB$FIELD_NAME').AsString);
        FM.Description := Q.Current.ByName('RDB$DESCRIPTION').AsString;
        FM.Name:= FM.Original;
          p:=Pos('_',FM.Name);
          if p>0 then FM.Name:='_'+UpperCase(Copy(FM.Name,p+1,1))+LowerCase(Copy(FM.Name,p+2,MaxInt));
        FM.NotNull := (Q.Current.ByName('RDB$NULL_FLAG').AsInteger=1);
        FM.Order := Q.Current.ByName('RDB$FIELD_POSITION').AsInteger;
        FM.IsStored:=True;
        FM.Owner:=TTableMeta(aTable);

        scale := Q.Current.ByName('RDB$FIELD_SCALE').AsInteger;
        case Q.Current.ByName('RDB$FIELD_TYPE').AsInteger of
          blr_varying, blr_text:
            begin
              FM.DataType := ftString;
              FM.DataSize := Q.Current.ByName('RDB$FIELD_LENGTH').AsInteger;
            end; //.............................................................
          blr_float, blr_double, blr_d_float:
            begin
              FM.DataType := ftFloat;
            end; //.............................................................
          blr_short:
            begin
              if (scale = 0) then
                FM.DataType := ftSmallInt
              else
                begin
                  FM.DataType := ftBCD;
                  FM.Precision := 4;
                  FM.DataSize := -scale;
                end;
            end; //.............................................................
          blr_long:
            begin
              if (scale = 0) then
                FM.DataType := ftInteger
              else
                if (scale >= (-4)) then
                  begin
                    FM.DataType := ftBCD;
                    FM.Precision := 9;
                    FM.DataSize := -scale;
                  end
                else
                  FM.DataType := ftFloat;
            end; //.............................................................
          blr_int64:
            begin
              if (scale = 0) then
                FM.DataType := ftLargeInt
              else
                if (scale >= (-4)) then
                  begin
                    FM.DataType := ftBCD;
                    FM.Precision := 18;
                    FM.DataSize := -scale;
                  end
                else
                  FM.DataType := ftFloat;
            end; //.............................................................
          blr_timestamp: FM.DataType := ftDateTime;
          blr_sql_time: FM.DataType := ftTime;
          blr_sql_date: FM.DataType := ftDate;
          blr_blob:
            if (Q.Current.ByName('RDB$FIELD_SUB_TYPE').AsInteger = 1) then
              FM.DataType := ftMemo
            else
              FM.DataType := ftBlob;
          blr_quad:
            begin
              FM.DataType := ftUnknown;
              FM.DataSize := sizeof (TISC_QUAD);
            end;
          else
            FM.DataType := ftUnknown;
        end;

        FL.Add(FM);
        Q.Next;
      end;
    Q.Close;

    // Читаем ключи ............................................................
    Q.SQL.Text :=
      'select i.RDB$INDEX_NAME, s.RDB$FIELD_NAME, i.RDB$UNIQUE_FLAG, '+
                                                           'I.RDB$FOREIGN_KEY '+
      'from rdb$index_segments s, rdb$indices i '+
      'where s.rdb$index_name = i.rdb$index_name '+
                                         'and i.rdb$relation_name = :TABLENAME';

    Q.ParamByName('TABLENAME').Value := TTableMeta(aTable).Table;
    WriteQueryLog(Q);
    Q.Open;
    while Not Q.Eof do
      begin
        FM:=FL.FindByName(Trim(Q.Current.ByName('RDB$FIELD_NAME').AsString));
        if Assigned(FM) then
          begin
            if Q.Current.ByName('RDB$UNIQUE_FLAG').AsInteger=1 then
              FM.Unique:=True;
            if Q.Current.ByName('RDB$UNIQUE_FLAG').AsInteger=1 then
              FM.Key:= True;
            if Pos('RDB$PRIMARY',Q.Current.ByName('RDB$INDEX_NAME').AsString)=1 then
              FM.Key:=True;
          end;
        Q.Next;
      end;
    Q.Close;
    TTableMeta(aTable).AssignFields(FL);
  finally
  end;
  Q.Free;
end;

function TDeIBDatabase.CreateQueryObject(const aQueryType: TDeQueryType; InDS: TDeDataset): TDeDataset;
begin
  Result := TDeIBDataset.Create(Self);
  if Assigned(ConnectionObject) then
    with TIBQuery(TDeIBDataset(result).SourceSet) do
    begin
      if Assigned(InDS) and InDS.Active and (InDS is TDeIBDataset) then
        begin
          Transaction:=TIBQuery(TDeIBDataset(InDS).SourceSet).Transaction;
        end
      else
        begin
          Transaction := TIBTransaction.Create(ConnectionObject);
          Transaction.AddDatabase(TIBDatabase(ConnectionObject));
          Transaction.AutoStopAction := saCommit;
        end;
      DataBase := (TIBDatabase(ConnectionObject));
    end;
end;

{ --- TDeFireDACDatabase ---------------------------------------------------------- }

function TDeFireDACDatabase.TableToStr(const tableName, tableAlias, tableDatabase, tableSchema: String): string;
begin
  Result:= '"' + tableName + '"';

  if Length(tableAlias)>0 then
    Result:= Result+' as '+tableAlias;

  if Length(tableDatabase)>0 then
    Result:= '"' + tableDatabase + '"."' + iif(Length(tableSchema)>0, tableSchema, 'public') + '".'  + Result else
  if Length(tableSchema)>0 then
    Result:= '"' + tableSchema + '".' + Result
end;

function TDeFireDACDatabase.FieldToStr(const fieldName: String; const fieldAlias: String = ''; const tableAlias: String = '';
  const fieldOperation: TOperationType = opNone; const DataType: TFieldType = ftUnknown; const DataSize: Integer = 0): string;
begin
  if (Length(fieldName) = 0) then
    Result:='*' else
  if (FieldName[1] in ['"','''']) then
    Result:= fieldName else
    Result:= '"' + fieldName + '"';

  if (Length(tableAlias) > 0) then
    Result := tableAlias + '.' + Result;

  if fieldOperation in AgregateOperations then
    Result := OperationLexeme[fieldOperation] + '(' + Result + ')';

  if (Length(fieldAlias) <> 0) and (fieldName<>fieldAlias) then
    Result := Result + ' as "' + fieldAlias + '"';
end;

function TDeFireDACDatabase.CreateTableDataset(const aTableName : string) : TDataset;
begin
  result := TFDTable.Create(ConnectionObject);
  TFDTable(result).Connection := TFDConnection(ConnectionObject);
  TFDTable(result).TableName:= aTableName;
//  TFDTable(result).FetchOptions.RecordCountMode:= cmTotal;
//  TFDTable(result).FetchOptions.Unidirectional:= true;
end;

function TDeFireDACDatabase.CreateQueryObject(const aQueryType: TDeQueryType; InDS: TDeDataset): TDeDataset;
begin
  Result := TDeFireDACDataset.Create(Self);
  if Assigned(ConnectionObject) then
    with TFDQuery(TDeFireDACDataset(result).SourceSet) do
    begin
      Connection := (TFDConnection(ConnectionObject));
      Transaction := Connection.Transaction;
      // Это опция заставляет возвращать в качестве количества записей именно общее количество
      // по умолчанию стоит cmFetched - количество начитанных на клиента записей
//      FetchOptions.RecordCountMode:= cmTotal;
//      FetchOptions.Unidirectional:= true;
    end;
end;

{ --- TDeFBDatabase ---------------------------------------------------------- }

constructor TDeFBDatabase.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  DatabaseType := dtFireBird;
end;

function TDeFBDatabase.BuildConnectString : string;
begin
  if Length(Server) > 0 then
    result := Format('%s:%s', [Server, Database])
  else
    result := inherited BuildConnectString;
end;

function TDeFBDatabase.CreateConnectionObject : TCustomConnection;
begin
  if CurrentIBVersion >= MinIBVersion then
  begin
    result := TFDConnection.Create(Application);
    result.LoginPrompt := False;

    with TFDConnection(result) do
    begin
      DriverName:='FB';
//      DefaultTransaction := TFDTransaction.Create(result);
//      DefaultTransaction.AutoStopAction := saCommit;
      Params.Database := ConnectString;
      Params.UserName := Login;
      Params.Password := Password;
    end;
  end
  else result:= nil;
end;

procedure TDeFBDatabase.RetrieveTableNames(aTablesList: TStringList);
var BackupCursor  : TCursor;
begin
  BackupCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    TFDConnection(ConnectionObject).GetTableNames(EmptyStr,EmptyStr,EmptyStr,aTablesList,[osMy],[tkTable]);
  finally
    Screen.Cursor := BackupCursor;
  end;
end;

procedure TDeFBDatabase.RetrieveMetaTables(aTablesList : TObjectList);
var Q   : TFDQuery;
    TM  : TTableMeta;
    p   : Integer;
begin
  Q:=TFDQuery.Create(nil);
  try
    Q.Connection := (TFDConnection(ConnectionObject));
    Q.Transaction := Q.Connection.Transaction;
 //   Q.Transaction.AutoStopAction := saCommit;
    Q.SQL.Text :=
      'select RDB$RELATION_NAME, RDB$VIEW_BLR, RDB$DESCRIPTION '+
      'from RDB$RELATIONS where RDB$SYSTEM_FLAG = 0';
    Q.ResourceOptions.SilentMode:= True;
    Q.FetchOptions.Unidirectional:= true;
    WriteQueryLog(Q);
    Q.Open;
    while Not Q.Eof do
      begin
        TM:=TTableMeta.Create(Trim(Q.FieldByName('RDB$RELATION_NAME').AsString), self);
        TM.IsReadOnly  := Not  Q.FieldByName('RDB$VIEW_BLR').IsNull;
        TM.Description := Trim(Q.FieldByName('RDB$DESCRIPTION').AsString);

        p:=Pos(#13,TM.Description); //ищем разрыв строки
        TM.Name := Copy(TM.Description, 1, p-1);
        if Length(TM.Name)=0 then TM.Name := '_dF.table '+TM.Table;

        aTablesList.Add(TM);
        Q.Next;
      end;
    Q.Close;
  finally
  end;
  Q.Free;
end;

// почти на 100 содрано с procedure TIBTable.InitFieldDefs;
procedure TDeFBDatabase.RetrieveMetaTableInfo(aTable : TObject);
var Q       : TFDQuery;
    FL      : TFieldsMeta;
    FM      : TFieldMeta;
    s: String;
    p,scale : Integer;
begin
  Assert( Assigned(aTable), 'RetrieveMetaTableInfo');
  Assert( aTable is TTableMeta, 'RetrieveMetaTableInfo');
  Assert( Assigned(FConnectionObject), ETableCantOpenError);

  if Not CheckConnection then Exit;

  Q:=TFDQuery.Create(nil);
  try
    Q.Connection := (TFDConnection(ConnectionObject));
    Q.Transaction := Q.Connection.Transaction;
  //  Q.Transaction.AutoStopAction := saCommit;
    Q.ResourceOptions.SilentMode:= True;
    Q.FetchOptions.Unidirectional:= true;

    FL:=TFieldsMeta.create;

    // Читаем поля .............................................................
    Q.SQL.Text :=
      'select R.RDB$FIELD_NAME, F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE, '+
      'F.RDB$FIELD_LENGTH, F.RDB$FIELD_SCALE,  R.RDB$NULL_FLAG, '+
      'R.RDB$DESCRIPTION, R.RDB$DEFAULT_VALUE, R.RDB$FIELD_POSITION, F.RDB$CHARACTER_LENGTH, F.RDB$CHARACTER_SET_ID, '+
      'R.RDB$FIELD_ID '+
      'from RDB$RELATION_FIELDS R , RDB$FIELDS F '+
      'where F.RDB$FIELD_NAME = R.RDB$FIELD_SOURCE and R.RDB$SYSTEM_FLAG = 0 '+
                                       'and R.RDB$RELATION_NAME = :TABLENAME '+
      'order by R.RDB$FIELD_POSITION';

    Q.ParamByName('TABLENAME').Value := TTableMeta(aTable).Table;
    WriteQueryLog(Q);
    Q.Open;
    while Not Q.Eof do
      begin
        FM:=TFieldMeta.Create;
        FM.Original:= Trim(Q.FieldByName('RDB$FIELD_NAME').AsString);
        FM.Description := Q.FieldByName('RDB$DESCRIPTION').AsString;
        FM.Name:= FM.Original;
          p:=Pos('_',FM.Name);
          if p>0 then FM.Name:='_'+UpperCase(Copy(FM.Name,p+1,1))+LowerCase(Copy(FM.Name,p+2,MaxInt));
        FM.NotNull := (Q.FieldByName('RDB$NULL_FLAG').AsInteger=1);
        FM.Order := Q.FieldByName('RDB$FIELD_POSITION').AsInteger;
        FM.IsStored:=True;
        FM.Owner:=TTableMeta(aTable);

        scale := Q.FieldByName('RDB$FIELD_SCALE').AsInteger;
        case Q.FieldByName('RDB$FIELD_TYPE').AsInteger of

          blr_varying, blr_text:
            begin
              if Q.FieldByName('RDB$CHARACTER_SET_ID').AsInteger = 1 then
                begin
                  FM.DataType := ftBytes;
                end else
              if Q.FieldByName('RDB$CHARACTER_SET_ID').AsInteger = 4 then
                begin
                  FM.DataType := ftWideString;
                  FM.CodePage := cpUTF8;
                end else
              if Q.FieldByName('RDB$CHARACTER_SET_ID').AsInteger = 48 then
                begin
                  FM.DataType := ftWideString;
                  FM.CodePage := 866;
                end else
                begin
                  FM.DataType := ftString;
                  FM.CodePage := cpNone;
                end;
              FM.DataSize := Q.FieldByName('RDB$CHARACTER_LENGTH').AsInteger;
            end; //.............................................................
          blr_float:
            begin
              FM.DataType := ftSingle;
            end; //.............................................................
          blr_double, blr_d_float:
            begin
              FM.DataType := ftFloat;
            end; //.............................................................
          blr_short:
            begin
              if (scale = 0) then
                begin
                  FM.DataType := ftSmallInt
                end else
              if (Scale=-2) and (Q.FieldByName('RDB$FIELD_LENGTH').AsInteger = 2) then
                begin
                  FM.DataType:= ftCurrency;
                end else
                begin
                  FM.DataType := ftBCD;
                  FM.Precision := 4;
                  FM.DataSize := -scale;
                end;
            end; //.............................................................
          blr_long:
            begin
              if (scale = 0) then
                FM.DataType := ftInteger
              else
                if (scale >= (-4)) then
                  begin
                    FM.DataType := ftBCD;
                    FM.Precision := 9;
                    FM.DataSize := -scale;
                  end
                else
                  FM.DataType := ftFloat;
            end; //.............................................................
          blr_int64:
            begin
              if (scale = 0) then
                FM.DataType := ftLargeInt
              else
                if (scale >= (-4)) then
                  begin
                    FM.DataType := ftBCD;
                    FM.Precision := 18;
                    FM.DataSize := -scale;
                  end
                else
                  FM.DataType := ftFloat;
            end; //.............................................................
          blr_timestamp: FM.DataType := ftTimeStamp;
          blr_sql_time: FM.DataType := ftTime;
          blr_sql_date: FM.DataType := ftDate;
          blr_blob:
            if (Q.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger = 1) then
              FM.DataType := ftMemo
            else
              FM.DataType := ftBlob;
          blr_quad:
            begin
              FM.DataType := ftUnknown;
              FM.DataSize := sizeof (TISC_QUAD);
            end;
          else
            FM.DataType := ftUnknown;
        end;

        FL.Add(FM);
        Q.Next;
      end;
    Q.Close;

    // Читаем primary ключи ............................................................
    Q.SQL.Text:=
      'select i.RDB$INDEX_NAME, s.RDB$FIELD_NAME, i.RDB$UNIQUE_FLAG, I.RDB$FOREIGN_KEY, R.RDB$CONSTRAINT_TYPE, F.RDB$RELATION_NAME ' +
      'from rdb$indices I ' +
      'left join rdb$index_segments S on S.rdb$index_name = I.rdb$index_name ' +
      'left join rdb$relation_constraints R on R.rdb$index_name = I.rdb$index_name ' +
      'left join rdb$REF_CONSTRAINTS C on C.rdb$CONSTRAINT_NAME = R.rdb$CONSTRAINT_NAME ' +
      'left join rdb$RELATION_CONSTRAINTS F on F.rdb$CONSTRAINT_NAME = C.rdb$CONST_NAME_UQ ' +
      'where i.rdb$relation_name = :TABLENAME';

    Q.ParamByName('TABLENAME').Value:= TTableMeta(aTable).Table;
    WriteQueryLog(Q);
    Q.Open;
    while Not Q.Eof do
      begin
        FM:=FL.FindByName(Trim(Q.FieldByName('RDB$FIELD_NAME').AsString));
        if Assigned(FM) then
          begin
            if Q.FieldByName('RDB$UNIQUE_FLAG').AsInteger = 1 then
              FM.Unique:= True;
            if SameText('PRIMARY KEY', Q.FieldByName('RDB$CONSTRAINT_TYPE').AsString) then
              FM.Key:= True;

            if SameText('FOREIGN KEY', Q.FieldByName('RDB$CONSTRAINT_TYPE').AsString) then
              begin
                s:= Q.FieldByName('RDB$RELATION_NAME').AsString;
                if Length(s) > 0  then
                  FM.SetLinkByName(s);  // << [dcc32 Error] ConnectOperationsUnit.pas(1716): E2003 Undeclared identifier: 'SetLinkByName'
                //FM.
              end;

          end;
        Q.Next;
      end;
    Q.Close;
    TTableMeta(aTable).AssignFields(FL);
  finally
  end;
  Q.Free;
end;
{ -------------------------------------------------------------------------------------------------------------------- }
{ --- TDeMSSQLDatabase ---------------------------------------------------------- }
{ -------------------------------------------------------------------------------------------------------------------- }

constructor TDeADODatabase.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  DatabaseType := dtODBC;
  FDatabaseDriver := FDefaultDatabaseDriver;
  FCanODBC     := True;
  FCanStoredProcedure := True;
  FReady_CI_AI := False;
  FReadyDDL := False;
  FReadyLoginSchema := False;
end;

function TDeADODatabase.GetServer : string;
begin
  Result:=inherited GetServer;
  if Length(Result)=0 then
    Result:=GetComputerNetName;
end;

const
  cProviderParamADO = 'Provider';
  cPersistSecurityInfoParamADO = 'Persist Security Info';
  cDataSourceParamADO = 'Data Source';
  cInitialCatalogParamADO = 'Initial Catalog';
  cModeParamADO = 'Mode';
  cUserIDParamADO = 'User ID';
  cPasswordParamADO = 'Password';
  cIntegratedSecurityParamADO = 'Integrated Security';
  cApplicationNameParamADO = 'Application Name';
  cReadWriteParamValueADO = 'ReadWrite';
  cSSPIParamValueADO = 'SSPI';
  cProviderAccess2007 = 'Microsoft.ACE.OLEDB.12.0';
  cProviderAccess2016 = 'Microsoft.ACE.OLEDB.16.0';
  cProviderJetOLEDB = 'Microsoft.Jet.OLEDB.4.0';

function TDeADODatabase.BuildConnectString: string;
{$IFDEF SQLNCLI}
var
  NativeClientVersion: Integer;
  OldConnectionString: string;
{$ENDIF}
  procedure AppendParameter(const Name, Value: string);
  begin
    if Length(Result) <> 0 then
      Result := Result + ';';
    Result := Result + Name + '=' + Value;
  end;
  {$IFDEF SQLNCLI}
  procedure BuildNativeClientConnectionString(const NativeClientVersion: Integer);
  begin
    Result := EmptyStr;
    case HiWord(NativeClientVersion) of
      10: AppendParameter(cProviderParamADO, cProviderSQLNCLI10);
      11: AppendParameter(cProviderParamADO, cProviderSQLNCLI11);
    else
      AppendParameter(cProviderParamADO, cProviderSQLNCLI9);
    end;
    AppendParameter(cPersistSecurityInfoParamADO, BooleanNames[False]);
    AppendParameter(cInitialCatalogParamADO, Database);
    AppendParameter(cDataSourceParamADO, Server);
  end;
  {$ENDIF}
  procedure BuildOleDBConnectionString;
  begin
    Result := EmptyStr;
//    AppendParameter(cProviderParamADO, cProviderMSOLEDBSQL);  // возможно с новым двайвером быстрее
{

var Providers: TStringList;
    ConnectionString: String;
try
  Providers:= TStringList.Create;
  GetProviderNames(Providers);
  if Providers.IndexOf('MSOLEDBSQL') >= 0 then
    ConnectionString := 'Provider=MSOLEDBSQL.1;'
  else
    ConnectionString := 'Provider=SQLNCLI11.1;';
finally
  Providers.Free;
end;

}
    AppendParameter(cProviderParamADO, cProviderSQLOLEDB);
    AppendParameter(cPersistSecurityInfoParamADO, BooleanNames[False]);
    AppendParameter(cInitialCatalogParamADO, Database);
    AppendParameter(cDataSourceParamADO, Server);
  end;
begin
  Result := EmptyStr;
  case DatabaseType of
    dtMSAccess:
      (*
      case GetMicrosoftAccessVersion of
        12: { 2007 }
          begin
            AppendParameter(cProviderParamADO, cProviderAccess2007);
            AppendParameter(cPersistSecurityInfoParamADO, BooleanNames[False]);
            AppendParameter(cDataSourceParamADO, Database);
          end;
        16: { 2016 }
          begin
            AppendParameter(cProviderParamADO, cProviderAccess2016);
            AppendParameter(cPersistSecurityInfoParamADO, BooleanNames[False]);
            AppendParameter(cDataSourceParamADO, Database);
          end;
      else *)
       begin
        AppendParameter(cProviderParamADO, cProviderJetOLEDB);
        AppendParameter(cPersistSecurityInfoParamADO, BooleanNames[False]);
        AppendParameter(cModeParamADO, cReadWriteParamValueADO);
        AppendParameter(cDataSourceParamADO, Database);
      end;
    dtMSSQL:
      case FDatabaseDriver of
        ddAuto: { Пытаемся автоматически определить используемый драйвер }
          begin
            {$IFDEF SQLNCLI}
            NativeClientVersion := GetNativeClientVersion;
            // Если NativeCLient установлен, то ...
            if NativeClientVersion <> 0 then
              begin
                // Получаем строку подключения ...
                BuildNativeClientConnectionString(NativeClientVersion);
                OldConnectionString := Result;
                if Length(Trim(Login)) <> 0 then
                  begin
                    AppendParameter(cUserIDParamADO, Login);
                    AppendParameter(cPasswordParamADO, '"' + Password + '"');
                  end
                else
                  begin
                    AppendParameter(cUserIDParamADO, GetWindowsUserName);
                    AppendParameter(cIntegratedSecurityParamADO, cSSPIParamValueADO);
                  end;
                // Проверяем подключение ...
                with TADOConnection.Create(nil) do
                  try
                    ConnectionString := Result;
                    LoginPrompt := False;
                    try
                      Open;
                      // Тест пройден и можно использовать драйвер Native Client!!!
                      FDatabaseDriver := ddNativeClient;
                    except
                      on E: EOleException do
                        begin
                          {$IFDEF DeDEBUG}
                          Funcs.WriteLog('TDeADODatabase.BuildConnectString check native client skip error: ' + E.Message, False, 'databases');
                          {$ENDIF}
                          case E.ErrorCode of
                            // Оставляем Native Client т.к. ошибка не связана с типом драйвера
                            -2147217843:FDatabaseDriver := ddNativeClient; // Имя пользователя и пароль не опознаны сервером.
                            -2147467259:FDatabaseDriver := ddNativeClient; // Login Fails. Сервер не существует или отсутствует доступ.
                            // При прочих ошибках подключения используем стандартный OLE DB драйвер!!!
                            else
                              begin
                                FDatabaseDriver := ddOleDB;
                                BuildOleDBConnectionString;
                              end;
                          end;
                        end;
                      on E: Exception do
                        begin
                          {$IFDEF DeDEBUG}
                          Funcs.WriteLog('TDeADODatabase.BuildConnectString check native client skip error: ' + E.Message, False, 'databases');
                          {$ENDIF}
                          // При ошибке подключения используем стандартный OLE DB драйвер!!!
                          FDatabaseDriver := ddOleDB;
                          BuildOleDBConnectionString;
                        end;
                    end;
                  finally
                    Free;
                  end;
                Result := OldConnectionString;
              end
            else
              begin
                FDatabaseDriver := ddOleDB;
                BuildOleDBConnectionString;
              end;
            {$ELSE}
            FDatabaseDriver := ddOleDB;
            BuildOleDBConnectionString;
            {$ENDIF}
          end;
        ddOleDB: { Microsoft OLE DB Provider for SQL Server }
          BuildOleDBConnectionString;
        {$IFDEF SQLNCLI}
        ddNativeClient: { SQL Server Native Client }
          BuildNativeClientConnectionString(GetNativeClientVersion);
        {$ENDIF}
      end;
  else
    AppendParameter(cInitialCatalogParamADO, Database);
    AppendParameter(cDataSourceParamADO, Server);
  end;
end;

function TDeADODatabase.CanSchema: Boolean;
begin
  Result := DatabaseType = dtMSSQL; // Только для MS-SQL Server!
end;

function TDeADODatabase.ConnectStringODBC(const IncludeAuthorization: Boolean): string;
var
  NativeClientVersion: Integer;
  procedure AppendParameter(const Name, Value: string);
  begin
    if Length(Result) <> 0 then
      Result := Result + ';';
    Result := Result + Name + '=' + Value;
  end;
  procedure BuildNativeClientConnectionString(const NativeClientVersion: Integer);
  begin
    Result := EmptyStr;
    case HiWord(NativeClientVersion) of
      10: AppendParameter(cProviderParamADO, cProviderSQLNCLI10);
      11: AppendParameter(cProviderParamADO, cProviderSQLNCLI11);
    else
      AppendParameter(cProviderParamADO, cProviderSQLNCLI9);
    end;
    AppendParameter(cPersistSecurityInfoParamADO, BooleanNames[False]);
    AppendParameter(cInitialCatalogParamADO, Database);
    AppendParameter(cDataSourceParamADO, Server);
    if Length(Trim(Login)) <> 0 then
      begin
        AppendParameter(cUserIDParamADO, Login);
        AppendParameter(cPasswordParamADO, '"' + Password + '"');
      end
    else
      begin
        AppendParameter(cUserIDParamADO, GetWindowsUserName);
        AppendParameter(cIntegratedSecurityParamADO, cSSPIParamValueADO);
      end;
  end;
  procedure BuildOleDBConnectionString;
  begin
    Result := EmptyStr;
    AppendParameter(cProviderParamADO, cProviderSQLOLEDB);
    AppendParameter(cPersistSecurityInfoParamADO, BooleanNames[False]);
    AppendParameter(cInitialCatalogParamADO, Database);
    AppendParameter(cDataSourceParamADO, Server);
    if Length(Trim(Login)) <> 0 then
      begin
        AppendParameter(cUserIDParamADO, Login);
        AppendParameter(cPasswordParamADO, '"' + Password + '"');
      end
    else
      begin
        AppendParameter(cUserIDParamADO, GetWindowsUserName);
        AppendParameter(cIntegratedSecurityParamADO, cSSPIParamValueADO);
      end;
  end;
begin
  Result := EmptyStr;
  case DatabaseType of
    dtMSAccess:
      if 12 {2007} <= GetMicrosoftAccessVersion then
        begin
          AppendParameter(cProviderParamADO, cProviderAccess2007);
          AppendParameter(cPersistSecurityInfoParamADO, BooleanNames[False]);
          AppendParameter(cDataSourceParamADO, Database);
        end
      else
        begin
          AppendParameter(cProviderParamADO, cProviderJetOLEDB);
          AppendParameter(cPersistSecurityInfoParamADO, BooleanNames[False]);
          AppendParameter(cModeParamADO, cReadWriteParamValueADO);
          AppendParameter(cDataSourceParamADO, Database);
        end;
    dtMSSQL:
      case DatabaseDriver of
        ddAuto: { Пытаемся автоматически определить используемый драйвер }
          begin
            {$IFDEF SQLNCLI}
            NativeClientVersion := GetNativeClientVersion;
            if NativeClientVersion <> 0 then
              BuildNativeClientConnectionString(NativeClientVersion)
            else
              BuildOleDBConnectionString;
            {$ELSE}
            BuildOleDBConnectionString;
            {$ENDIF}
          end;
        ddOleDB: { Microsoft OLE DB Provider for SQL Server }
          BuildOleDBConnectionString;
        {$IFDEF SQLNCLI}
        ddNativeClient: { SQL Server Native Client }
          BuildNativeClientConnectionString(GetNativeClientVersion);
        {$ENDIF}
      end;
  end;
end;

function TDeADODatabase.CreateConnectionObject : TCustomConnection;
var cString : string;
    ParamList : TStringList;
  procedure AppendParameter(const Name, Value: string);
  begin
    if Length(cString) <> 0 then
      cString := cString + ';';
    cString := cString + Name + '=' + Value;
  end;
begin
  Result := TADOConnection.Create(Application);
  Result.LoginPrompt := False;
  ParamList := TStringList.Create;
  try
    ParamList.Delimiter := ';';
    ParamList.DelimitedText := ConnectString;

    cString := ConnectString;
    case DatabaseType of
      dtMSSQL:
        begin
          if Length(Trim(Login)) <> 0 then
            begin
              AppendParameter(cUserIDParamADO, Login);
              AppendParameter(cPasswordParamADO, Password);
            end
          else
            begin
              AppendParameter(cUserIDParamADO, GetWindowsUserName);
              AppendParameter(cIntegratedSecurityParamADO, cSSPIParamValueADO);
            end;
          if Length(FDefaultApplicationName) <> 0 then
            AppendParameter(cApplicationNameParamADO, '"' + FDefaultApplicationName + '"');
        end;
      dtMSAccess:
        begin
          if Length(Trim(Login)) <> 0 then
            AppendParameter(cUserIDParamADO, Login);
          if Length(Trim(Password)) <> 0 then
            AppendParameter(cPasswordParamADO, Password);
        end;
    end;

    TADOConnection(result).ConnectionString:= cString;
    TADOConnection(result).Provider := ParamList.Values[cProviderParamADO];
    if Variables.VarExists(RegConnectionTimeOut) then
      TADOConnection(result).ConnectionTimeout:= Variables.AsInteger[RegConnectionTimeOut];
  finally
    ParamList.Free;
  end;
end;

function TDeADODatabase.CreateTableDataset(const aTableName : string) : TDataset;
begin
  result := TADOTable.Create(ConnectionObject);
  TADOTable(result).Connection := TADOConnection(ConnectionObject);
  TADOTable(result).TableName := aTableName;
end;

destructor TDeADODatabase.Destroy;
begin
  FreeAndNil(FObjectTypes);
  inherited Destroy;
end;

procedure TDeADODatabase.RetrieveTableNames(aTablesList: TStringList);
var BackupTimeout : integer;
    BackupCursor  : TCursor;
    DisconnectAfterUse : boolean;
begin
  BackupCursor := Screen.Cursor;
  Assert(Assigned(FConnectionObject) and (FConnectionObject is TADOConnection), 'ConnectionObject is nil!!!');
  with TADOConnection(ConnectionObject) do
    try
      try
        Screen.Cursor := crHourglass;
        BackupTimeout := ConnectionTimeout;
        if not TADOConnection(FConnectionObject).Connected then
        begin
          TADOConnection(FConnectionObject).Open;
          DisconnectAfterUse := true;
        end
        else
          DisconnectAfterUse := false;
        TADOConnection(FConnectionObject).GetTableNames(aTablesList);
        if DisconnectAfterUse then
        begin
        TADOConnection(FConnectionObject).Close;
        ConnectionTimeout := BackupTimeout;
        end;
      except
        {$IFDEF DEBUG}
        on E: Exception do
          DebugLog(ClassName + '.RetrieveTableNames error: ' + E.Message);
        {$ENDIF}
      end;
    finally
      Screen.Cursor := BackupCursor;
    end;
end;

procedure TDeADODatabase.RetrieveMetaTables(aTablesList : TObjectList);
var Q: TADOQuery;
    TM: TTableMeta;
    Strings: TStrings;
    Index: Integer;
    MetaPresents: Boolean;
begin
  Assert(Assigned(FConnectionObject) and (FConnectionObject is TADOConnection), 'ConnectionObject is nil!!!');
  if DatabaseType = dtMSAccess then
    begin
      Strings := TStringList.Create;
      try
        (FConnectionObject as TADOConnection).GetTableNames(Strings);
        for Index := 0 to Pred(Strings.Count) do
          begin
            TM := TTableMeta.Create(Trim(Strings[Index]), self, [tsFields]);
            TM.SetTable := Trim(Strings[Index]);
            TM.Name := '_dF.table '+TM.Table;
            aTablesList.Add(TM);
          end;
      finally
        Strings.Free;
      end;
      Exit;
    end;

  Q:=TADOQuery.Create(Self);
  try
    Q.BeforeOpen:= BeforeOpen;
    Q.Connection:= TADOConnection(ConnectionObject);
    if DatabaseType = dtMSSQL then
      try
        MetaPresents := CanSchema;
        if MetaPresents then
          Q.SQL.Text := 'select s.[name] as [schema_name],'#13#10 +
                        '       d.[name],'#13#10 +
                        '       e.[value] as [description],'#13#10 +
                        '       d.[is_view]'#13#10 +
                        '  from ('#13#10 +
                        '         select [object_id],'#13#10 +
                        '                [schema_id],'#13#10 +
                        '                [name],'#13#10 +
                        '                cast (0 as bit) as [is_view]'#13#10 +
                        '           from sys.tables'#13#10 +
                        '          where [name] != ''sysdiagrams'''#13#10 +
                        '                 and [is_ms_shipped] = 0'#13#10 +
                        '          union all'#13#10 +
                        '         select v.[object_id],'#13#10 +
                        '                v.[schema_id],'#13#10 +
                        '                v.[name],'#13#10 +
                        '                case when exists(select * from sys.triggers where [parent_id] = v.[object_id] and [is_instead_of_trigger] = 1)'#13#10 +
                        '                     then cast (0 as bit)'#13#10 +
                        '                     else cast (1 as bit)'#13#10 +
                        '                end as [is_view]'#13#10 +
                        '           from sys.views as v'#13#10 +
                        '          where [is_ms_shipped] = 0'#13#10 +
                        '       ) d'#13#10 +
                        '  left join sys.schemas s'#13#10 +
                        '    on s.[schema_id] = d.[schema_id]'#13#10 +
                        '  left join sys.extended_properties e'#13#10 +
                        '    on e.[major_id] = d.[object_id]'#13#10 +
                        '   and e.[minor_id] = 0'#13#10 +
                        '   and e.[name] = ''MS_Description'''
        else
          Q.SQL.Text := 'select d.[name],'#13#10 +
                        '       e.[value] as [description],'#13#10 +
                        '       d.[is_view]'#13#10 +
                        '  from ('#13#10 +
                        '         select [object_id],'#13#10 +
                        '                [name],'#13#10 +
                        '                cast (0 as bit) as [is_view]'#13#10 +
                        '           from sys.tables'#13#10 +
                        '          where [name] != ''sysdiagrams'''#13#10 +
                        '                 and [is_ms_shipped] = 0'#13#10 +
                        '          union all'#13#10 +
                        '         select v.[object_id],'#13#10 +
                        '                v.[name],'#13#10 +
                        '                case when exists(select * from sys.triggers where [parent_id] = v.[object_id] and [is_instead_of_trigger] = 1)'#13#10 +
                        '                     then cast (0 as bit)'#13#10 +
                        '                     else cast (1 as bit)'#13#10 +
                        '                end as [is_view]'#13#10 +
                        '           from sys.views as v'#13#10 +
                        '          where [is_ms_shipped] = 0'#13#10 +
                        '       ) d'#13#10 +
                        '  left join sys.extended_properties e'#13#10 +
                        '    on e.[major_id] = d.[object_id]'#13#10 +
                        '   and e.[minor_id] = 0'#13#10 +
                        '   and e.[name] = ''MS_Description''';
        Q.Open;
        while not Q.Eof do
          begin
            TM := TTableMeta.Create(Trim(Q.FieldByName('name').AsString), self, [tsFields]);
            if MetaPresents then
              TM.Schema := Trim(Q.FieldByName('schema_name').AsString);
            TM.IsReadOnly := Q.FieldByName('is_view').AsBoolean;
            TM.Description := Trim(Q.FieldByName('description').AsString);
            TM.Name := '_dF.table ' + TM.Table;
            aTablesList.Add(TM);
            Q.Next;
          end;
      except
        on E: Exception do
          begin
            {$IFDEF DeDEBUG}
            Funcs.WriteLog('Failed to get MS-SQL table list: %s', [E.Message]);
            {$ENDIF}
          end;
      end
    else
      begin
        Q.SQL.Text := 'SELECT TABLE_NAME, TABLE_TYPE '+
                      'FROM INFORMATION_SCHEMA.TABLES '+
                      'WHERE not (TABLE_NAME = ''sysdiagrams'')';
        Q.Open;
        while not Q.Eof do
          begin
            TM := TTableMeta.Create(Trim(Q.FieldValues['TABLE_NAME']), self, [tsFields]);
            TM.IsReadOnly := Trim(Q.FieldValues['TABLE_TYPE']) = 'VIEW';
            TM.Name := '_dF.table ' + TM.Table;
            aTablesList.Add(TM);
            Q.Next;
          end;
      end;
  finally
    Q.Free;
  end;
end;

procedure TDeADODatabase.RetrieveMetaTableInfo(aTable : TObject);
var FL    : TFieldsMeta;
    Q     : TADOQuery;
    FM    : TFieldMeta;
    TM    : TTableMeta;
    p     : Integer;
    sType, sCharset: string;
    Index: Integer;
  function FieldTypeToScript(const FieldType: TFieldType): string; inline;
  begin
    Result := IntToStr(Ord(FieldType));
  end;
begin
  Assert( Assigned(aTable), 'RetrieveMetaTableInfo');
  Assert( aTable is TTableMeta, 'RetrieveMetaTableInfo');
  Assert( Assigned(FConnectionObject), ETableCantOpenError);

  if Not CheckConnection then Exit;

  FL := TFieldsMeta.Create;
  Q := TADOQuery.Create(Self);
  try
    Q.BeforeOpen := BeforeOpen;
    Q.Connection := TADOConnection(ConnectionObject);

    case DatabaseType of
      dtMSAccess: { Microsoft Access }
        begin
          Q.SQL.Text := 'select * from ' + TTableMeta(aTable).Table + ' where 1 = 0';
          Q.Open;
          try
            for Index := 0 to Pred(Q.Fields.Count) do
              begin
                FM := TFieldMeta.Create;
                FM.Original := Trim(Q.Fields[Index].FieldName);
                FM.Name := FM.Original;
                p:=Pos('_',FM.Name);
                if p > 0 then FM.Name := '_' + UpperCase(Copy(FM.Name, p + 1, 1)) + LowerCase(Copy(FM.Name, p + 2, MaxInt));
                FM.DataType := Q.Fields[Index].DataType;
                case FM.DataType of
                  ftString: FM.DataSize := Q.Fields[Index].DataSize;
                  ftWideString: FM.DataSize := Q.Fields[Index].DataSize;
                  ftGuid: FM.DataSize:= 38;
                end;
                FM.Key := pfInKey in Q.Fields[Index].ProviderFlags;
                if FM.Key then FM.VisibleLevel:= fvLevel1;
                FM.NotNull := not Q.Fields[Index].IsNull;
                FM.Order := Index;
                FM.IsStored := not Q.Fields[Index].Calculated;
                FM.ReadOnly := not Q.Fields[Index].CanModify;
                FM.Owner := TTableMeta(aTable);
                FL.Add(FM);
              end;
          finally
            Q.Close;
          end;
        end;
      dtMSSQL: { Microsoft SQL Server }
        begin
          if CanSchema then
            begin
              sType := Trim(TTableMeta(aTable).Schema);
              if Length(sType) <> 0 then
                sType := '[' + sType + '].';
            end
          else
            sType := EmptyStr;
          Q.SQL.Text := 'select c.[column_id],'#13#10 +
                        '       c.[name],'#13#10 +
                        '       case c.[system_type_id] when  34 then ' + FieldTypeToScript(ftBlob) + #13#10 +
                        '                               when  35 then ' + FieldTypeToScript(ftMemo) + #13#10 +
                        '                               when  36 then ' + FieldTypeToScript(ftGuid) + #13#10 +
                        '                               when  40 then ' + FieldTypeToScript(ftDate) + #13#10 +
                        '                               when  41 then ' + FieldTypeToScript(ftTime) + #13#10 +
                        '                               when  42 then ' + FieldTypeToScript(ftDateTime) + #13#10 +
                        '                               when  43 then ' + FieldTypeToScript(ftTimeStampOffset) + #13#10 +
                        '                               when  48 then ' + FieldTypeToScript(ftWord{ftShortInt}) + #13#10 +
                        '                               when  52 then ' + FieldTypeToScript(ftSmallint) + #13#10 +
                        '                               when  56 then ' + FieldTypeToScript(ftInteger) + #13#10 +
                        '                               when  58 then ' + FieldTypeToScript(ftDateTime) + #13#10 +
                        '                               when  59 then ' + FieldTypeToScript(ftSingle) + #13#10 +
                        '                               when  60 then ' + FieldTypeToScript(ftCurrency) + #13#10 +
                        '                               when  61 then ' + FieldTypeToScript(ftDateTime) + #13#10 +
                        '                               when  62 then ' + FieldTypeToScript(ftFloat) + #13#10 +
                        '                               when  98 then ' + FieldTypeToScript(ftVariant) + #13#10 +
                        '                               when  99 then ' + FieldTypeToScript(ftWideMemo) + #13#10 +
                        '                               when 104 then ' + FieldTypeToScript(ftBoolean) + #13#10 +
                        '                               when 106 then ' + FieldTypeToScript(ftBCD) + #13#10 +
                        '                               when 108 then ' + FieldTypeToScript(ftBCD) + #13#10 +
                        '                               when 122 then ' + FieldTypeToScript(ftCurrency) + #13#10 +
                        '                               when 127 then ' + FieldTypeToScript(ftLargeint) + #13#10 +
                        '                               when 165 then case when c.[max_length] = -1'#13#10 +
                        '                                                  then ' + FieldTypeToScript(ftBlob) + #13#10 +
                        '                                                  else ' + FieldTypeToScript(ftVarBytes) + #13#10 +
                        '                                             end'#13#10 +
                        '                               when 167 then case when c.[max_length] = -1'#13#10 +
                        '                                                  then ' + FieldTypeToScript(ftMemo) + #13#10 +
                        '                                                  else ' + FieldTypeToScript(ftString) + #13#10 +
                        '                                             end'#13#10 +
                        '                               when 173 then ' + FieldTypeToScript(ftBytes) + #13#10 +
                        '                               when 175 then case when c.[max_length] = -1'#13#10 +
                        '                                                  then ' + FieldTypeToScript(ftMemo) + #13#10 +
                        '                                                  else ' + FieldTypeToScript(ftString) + #13#10 +
                        '                                             end'#13#10 +
                        '                               when 189 then ' + FieldTypeToScript(ftTimeStamp) + #13#10 +
                        '                               when 231 then case when c.[max_length] = -1'#13#10 +
                        '                                                  then ' + FieldTypeToScript(ftWideMemo) + #13#10 +
                        '                                                  else ' + FieldTypeToScript(ftWideString) + #13#10 +
                        '                                             end'#13#10 +
                        '                               when 239 then case when c.[max_length] = -1'#13#10 +
                        '                                                  then ' + FieldTypeToScript(ftWideMemo) + #13#10 +
                        '                                                  else ' + FieldTypeToScript(ftWideString) + #13#10 +
                        '                                             end'#13#10 +
                        '                               when 241 then ' + FieldTypeToScript(ftWideMemo{ftXML}) + #13#10 +
                        '                                        else ' + FieldTypeToScript(ftUnknown) + #13#10 +
                        '       end as [type],'#13#10 +
                        '       case when c.[system_type_id] in (231, 239)'#13#10 +
                        '            then case when c.[max_length] = -1 then null'#13#10 +
                        '                      else c.[max_length] / 2'#13#10 +
                        '                 end'#13#10 +
                        '            else case when c.[system_type_id] in (165, 167, 173, 175) then c.[max_length]'#13#10 +
                        '                      when c.[system_type_id] = 189 then 8'#13#10 +
                        '                      when c.[system_type_id] = 36 then 38'#13#10 +
                        '                      when c.[system_type_id] in (60, 106, 108, 122) then c.[scale]'#13#10 +
                        '                      else null'#13#10 +
                        '                 end'#13#10 +
                        '       end as [data_size],'#13#10 +
                        '       case when c.[system_type_id] in (167, 175) then 866'#13#10 + // char/varchar
                        '            when c.[system_type_id] in (231, 239) then 0'#13#10 + // nchar/nvarchar
                        '            else null'#13#10 +
                        '       end as [charset],'#13#10 +
                        '       c.[is_nullable],'#13#10 +
                        '       c.[is_computed],'#13#10 +
                        '       c.[is_identity],'#13#10 +
                        '       case when p.column_id is null'#13#10 +
                        '            then cast(0 as bit)'#13#10 +
                        '            else cast(1 as bit)'#13#10 +
                        '       end as [is_primary_key],'#13#10 +
                        '       d.[definition] as [definition],'#13#10 +
                        '       e.[value] as [description]'#13#10 +
                        '  from sys.columns c'#13#10 +
                        '  left join sys.default_constraints d'#13#10 +
                        '    on d.[parent_object_id] = c.[object_id]'#13#10 +
                        '   and d.[parent_column_id] = c.[column_id]'#13#10 +
                        '  left join sys.extended_properties e'#13#10 +
                        '    on e.[major_id] = c.[object_id]'#13#10 +
                        '   and e.[minor_id] = c.[column_id]'#13#10 +
                        '   and e.[name] = ''MS_Description'''#13#10 +
                        '  left join ('#13#10 +
                        '              select i.[object_id],'#13#10 +
                        '                     c.[column_id]'#13#10 +
                        '                from sys.indexes i'#13#10 +
                        '                join sys.index_columns c'#13#10 +
                        '                  on c.[index_id] = i.[index_id]'#13#10 +
                        '                 and c.[object_id] = i.[object_id]'#13#10 +
                        '               where i.[is_primary_key] = 1'#13#10 +
                        '            ) p'#13#10 +
                        '    on p.[object_id] = c.[object_id]'#13#10 +
                        '   and p.[column_id] = c.[column_id]'#13#10 +
                        ' where c.[object_id] = Object_ID(' + QuotedStr(sType + '[' + TTableMeta(aTable).Table + ']') + ', ''U'')'#13#10 +
                        '    or c.[object_id] = Object_ID(' + QuotedStr(sType + '[' + TTableMeta(aTable).Table + ']') + ', ''V'')'#13#10 +
                        ' order by c.[column_id]'#13#10;
          Q.Open;
          while not Q.Eof do
            begin
              FM := TFieldMeta.Create;
              try
                FM.Owner := TTableMeta(aTable);
                FM.IsStored := True;
                FM.Original := Trim(Q.FieldByName('name').AsString);
                FM.Name := FM.Original;
                p := Pos('_',FM.Name);
                if p <> 0 then FM.Name := '_' + UpperCase(Copy(FM.Name, Succ(p), 1)) + LowerCase(Copy(FM.Name, p + 2, MaxInt));
                if Q.FieldByName('is_identity').AsBoolean and (TFieldType(Q.FieldByName('type').AsInteger) in IntegerTypes)
                  then FM.DataType := ftAutoinc
                  else FM.DataType := TFieldType(Q.FieldByName('type').AsInteger);
                if not Q.FieldByName('data_size').IsNull then
                  FM.DataSize := Q.FieldByName('data_size').AsInteger;
                if not Q.FieldByName('charset').IsNull then
                  FM.CodePage := Q.FieldByName('charset').AsInteger;
                FM.NotNull := not Q.FieldByName('is_nullable').AsBoolean;
                FM.ReadOnly := Q.FieldByName('is_computed').AsBoolean or Q.FieldByName('is_identity').AsBoolean;
                FM.Order := Q.FieldByName('column_id').AsInteger;
                FM.Key := Q.FieldByName('is_primary_key').AsBoolean;
                if not Q.FieldByName('definition').IsNull then
                  FM.DefaultDefDB:= TrimFormula(Q.FieldByName('definition').AsString);
                FM.Description := Trim(Q.FieldByName('description').AsString);
                FL.Add(FM);
              except
                FM.Free;
                raise;
              end;
              Q.Next;
            end;
          Q.Close;
          try
            // В принципе в этом запросе заложено использование нескольких столбцов в foreign ключей,
            // но Profit пока этого не понимает и ссылка только на primary ключ (причём только одно поле)!
            // Поэтому в запросе есть поле 'column_name', но ниже не используется пока.
            Q.SQL.Text := 'select p.[name] as source_name, ' +
                                  't.[name] as table_name, ' +
                                  'c.[name] as column_name, ' +
                                  'case f.[delete_referential_action] when 1 then ''CASCASE'' ' + // cascade
                                                                     'when 2 then ''SET NULL'' ' + // set null
                                                                            'else null ' +
                                  'end as delete_action ' +
                             'from sys.foreign_keys f ' +
                             'join sys.foreign_key_columns k ' +
                               'on k.[constraint_object_id] = f.[object_id] ' +
                             'join sys.tables t ' +
                               'on t.[object_id] = k.[referenced_object_id] ' +
                             'join sys.columns c ' +
                               'on c.[object_id] = k.[referenced_object_id] ' +
                              'and c.[column_id] = k.[referenced_column_id] ' +
                             'join sys.columns p ' +
                               'on p.[object_id] = k.[parent_object_id] ' +
                              'and p.[column_id] = k.[parent_column_id] ' +
                            'where k.[parent_object_id] = OBJECT_ID(' + QuotedStr(TTableMeta(aTable).Table) + ', ''U'') ' +
                            'order by k.[parent_column_id], k.[referenced_column_id]';
            Q.Open;
            try
              while not Q.Eof do
                begin
                  FM := FL.FindByName(Trim(Q.FieldByName('source_name').AsString));
                  if Assigned(FM) then
                    begin
                      TM := MetaData.GetTableByName(Trim(Q.FieldByName('table_name').AsString));
                      if Assigned(TM) then
                        begin
                          FM.Link := TM.ID;
                          sType := Q.FieldValues['delete_action'];
                          if SameText(sType, 'CASCASE') then
                            FM.LinkType := daCascade
                          else if SameText(sType, 'SET NULL') then
                            FM.LinkType := daSetNull;
                        end;
                    end;
                  Q.Next;
                end;
            finally
              Q.Close;
            end;
          except
            {$IFDEF DEBUG}
            on E: Exception do
              DebugLog(ClassName + '.RetrieveMetaTableInfo for foreign keys skip error: ' + E.Message);
            {$ENDIF}
          end;
        end;
    else
      raise Exception.Create('Unsupported database type for Retrieve table information!');
    end;
    TTableMeta(aTable).AssignFields(FL);
  finally
    Q.Free;
    FL.Free; // Багафикс: Объект до v. 17.06 не разрушался, что приводило к утечке памяти!
  end;
end;

procedure TDeADODatabase.RetrieveProcedures(aProceduresList : TObjectList);
const prefix : string = 'profit.';

var Q      : TADOQuery;
    TM,TM2 : TTableMeta;
    s      : string;
    p      : Integer;
begin
  if Pos( 'PROVIDER=MSOLAP', ConnectString ) > 0 then Exit;
  if Connected then
    begin
      Q:=TADOQuery.Create(self);
      try
        Q.BeforeOpen := BeforeOpen;
        Q.Connection:= TADOConnection(ConnectionObject);
        Q.SQL.Text:=
          'SELECT specific_Name FROM INFORMATION_SCHEMA.ROUTINES '+
          'where specific_Name like ''profit.%''';
        Q.Open;
        while Not Q.Eof do
          begin
            TM:=TTableMeta.Create;
            s:= Q.FieldValues['specific_Name'];
            TM.SetTable := s;

            if {Ansi}CompareText(Copy(s,1,Length(prefix)),prefix)=0 then Delete(s,1,Length(prefix));

            p:=Pos('.',s);
            if p>0 then
              begin
                TM.Name:=  '_'+Copy(s,p+1,MaxInt);
                TM2:=MetaData.TablesList.FindByTable(Copy(s,1,p-1));
                if Assigned(TM2) then TM.ID:=TM2.ID else TM.ID:=0;
              end
            else
              begin
                TM.Name:=  s;
                TM.ID:=  -1;
              end;

            aProceduresList.Add(TM);
            Q.Next;
          end;
        Q.Close;
      finally
      end;
      Q.Free;
    end;
end;

procedure TDeADODatabase.RetrieveProcedureInfo(aTable : TObject);
var FL    : TFieldsMeta;
    Q     : TADOQuery;
    FM    : TFieldMeta;
begin
  Q:=TADOQuery.Create(self);
  try
    Q.BeforeOpen := BeforeOpen;
    Q.Connection:= TADOConnection(ConnectionObject);
    FL:=TFieldsMeta.create;

    // Читаем поля .............................................................
    Q.SQL.Text :=
      'select SPECIFIC_CATALOG, SPECIFIC_NAME, PARAMETER_MODE, PARAMETER_NAME, '+
      'DATA_TYPE, CHARACTER_MAXIMUM_LENGTH LENGTH '+
      'from information_schema.PARAMETERS '+
      'where SPECIFIC_NAME = :TABLENAME '+
      'order by ORDINAL_POSITION';
    Q.Prepared:=True;
    Q.Parameters.ParamByName('TABLENAME').Value := TTableMeta(aTable).Table;
    Q.Open;
    while Not Q.Eof do
      begin
        FM:=TFieldMeta.Create;
        FM.Original:= Q.FieldValues['PARAMETER_NAME'];
        FL.Add(FM);
        Q.Next;
      end;
    Q.Close;

    TTableMeta(aTable).AssignFields(FL);
  finally
    Q.Free;
  end;
end;

function TDeADODatabase.RetrieveProcedureParameters(const ProcedureName: string; ParameterList: TObject): Boolean;
var
  Query: TADOQuery;
  VariableItem: TVariableItem;
  function PrepareFieldType(const FieldType: TFieldType): string; inline;
  begin
    Result := IntToStr(Ord(FieldType));
  end;
begin
  Result := False;
  if (DatabaseType = dtMSSQL) and Assigned(ParameterList) then
    if ParameterList is TStrings then
      begin
        TStrings(ParameterList).BeginUpdate;
        try
          TStrings(ParameterList).Clear;
          Query := TADOQuery.Create(Self);
          try
            Query.BeforeOpen := BeforeOpen;
            Query.Connection:= TADOConnection(ConnectionObject);
            Query.SQL.Text := 'select p.[name], case when p.[system_type_id] = 104 then 8 '+
                                                    'when p.[system_type_id] in (40, 58, 61) then 7 '+
                                                    'when p.[system_type_id] = 41 then 10 '+
                                                    'else 6 end as [type] '+
                              'from sys.parameters p '+
                              'where p.[object_id] = Object_ID(' + QuotedStr(ProcedureName) + ', ''P'')';
            Query.Open;
            while not Query.Eof do
              begin
                TStrings(ParameterList).AddObject(Query.Fields[0].AsString, Pointer(Query.Fields[1].AsInteger));
                Query.Next;
              end;
          finally
            Query.Free;
          end;
          Result := True;
        finally
          TStrings(ParameterList).EndUpdate;
        end;
      end
    else if ParameterList is TVariableList then
      begin
        TVariableList(ParameterList).Clear;
        Query := TADOQuery.Create(Self);
        try
          Query.BeforeOpen := BeforeOpen;
          Query.Connection:= TADOConnection(ConnectionObject);
          Query.SQL.Text := 'select p.[name], case p.[system_type_id]' +
            ' when 34 then ' + PrepareFieldType(ftBlob) +       // image
            ' when 35 then ' + PrepareFieldType(ftMemo) +       // text
            ' when 36 then ' + PrepareFieldType(ftGuid) +       // uniqueidentifier
            ' when 40 then ' + PrepareFieldType(ftDate) +       // date
            ' when 41 then ' + PrepareFieldType(ftTime) +       // time
            ' when 48 then ' + PrepareFieldType(ftByte) +       // tinyint !!!!
            ' when 52 then ' + PrepareFieldType(ftSmallint) +   // smallint
            ' when 56 then ' + PrepareFieldType(ftInteger) +    // int
            ' when 58 then ' + PrepareFieldType(ftDateTime) +   // smalldatetime
            ' when 59 then ' + PrepareFieldType(ftFloat) +      // real
            ' when 60 then ' + PrepareFieldType(ftCurrency) +   // money
            ' when 61 then ' + PrepareFieldType(ftDateTime) +   // datetime
            ' when 62 then ' + PrepareFieldType(ftFloat) +      // float
            ' when 104 then ' + PrepareFieldType(ftBoolean) +   // bit
            ' when 106 then ' + PrepareFieldType(ftFloat) +     // decimal
            ' when 108 then ' + PrepareFieldType(ftFloat) +     // numeric
            ' when 122 then ' + PrepareFieldType(ftCurrency) +  // smallmoney
            ' when 127 then ' + PrepareFieldType(ftLargeint) +  // bigint
            ' when 167 then ' + PrepareFieldType(ftString) +    // varchar
            ' when 175 then ' + PrepareFieldType(ftString) +    // char
            ' when 231 then ' + PrepareFieldType(ftString) +    // nvarchar
            ' else ' + PrepareFieldType(ftUnknown) +
            ' end as [type]'+
            ' from sys.parameters p'+
            ' where p.[object_id] = Object_ID(' + QuotedStr(ProcedureName) + ', ''P'')';
          Query.Open;
          while not Query.Eof do
            begin
              VariableItem := TVariableItem.Create(TFieldType(Query.Fields[1].AsInteger), Query.Fields[0].AsString, [amRead, amWrite]);
              if TVariableList(ParameterList).Add(VariableItem) = -1 then
                VariableItem.Free;
              Query.Next;
            end;
        finally
          Query.Free;
        end;
        Result := True;
      end;
end;

function TDeADODatabase.RetrieveProcedureExecQuery(const ProcedureName: string; ParameterList: TObject): String;
var i: Integer;
begin
  Result:= EmptyStr;
  if ParameterList is TStrings then
    begin
      for i := 0 to Pred(TStrings(ParameterList).Count) do
      begin
        if Length(Result) <> 0 then
          Result := Result + ',';
        Result := Result + ' :' + TStrings(ParameterList)[i];
      end;
    end else

  if ParameterList is TVariableList then
    begin
      for i := 0 to Pred(TVariableList(ParameterList).Count) do
      begin
        if Length(Result) <> 0 then
          Result := Result + ',';
        Result := Result + ' :' + TVariableList(ParameterList)[i].Name;
      end;
    end;

  Result := 'exec ' + ProcedureName + Result;
end;

function TDeADODatabase.StoredProcedureExists(const StoredProcedureName: string): Boolean;
var
  Query: TADOQuery;
begin
  if DatabaseType = dtMSSQL then
    begin
      Query := TADOQuery.Create(Self);
      try
        Query.BeforeOpen := BeforeOpen;
        Query.Connection:= TADOConnection(ConnectionObject);
        Query.SQL.Text := 'select [object_id] from [sys].[procedures] where [name] like :name';
        Query.Prepared := True;
        Query.Parameters[0].Value := StoredProcedureName;
        Query.Open;
        Result := not Query.Fields[0].IsNull;
      finally
        Query.Free;
      end;
    end
  else
    Result := False; // Здесь можно реализовать логику для других БД ...
end;

function TDeADODatabase.SupportDDLs: TSupportDDLs;
begin
  if DatabaseType = dtMSSQL then
    Result := [ddlCreateTable, ddlCreateField, ddlModifyField, ddlDeleteField]
  else
    Result := inherited SupportDDLs;
end;

function TDeADODatabase.DatasetPermissions(const DatasetName: string): TDeDatasetPermissions;
var
  Query: TADOQuery;
  Value: Integer;
  procedure CheckPermission(const Mask: Integer; const Permission: TDeDatasetPermission);
  begin
    if (Value and Mask) <> 0 then
      Include(Result, Permission);
  end;
begin
  if DatabaseType = dtMSSQL then
    begin
      Query := TADOQuery.Create(nil);
      try
        Query.Connection := TADOConnection(ConnectionObject);
        Query.BeforeOpen := BeforeOpen;
        Query.SQL.Text := 'select Permissions(Object_ID(' + QuotedStr(DatasetName) + ', ''U''))';
        Query.Open;
        Value := Query.Fields[0].AsInteger;
      finally
        Query.Free;
      end;
      Result := [];
      // https://docs.microsoft.com/en-us/sql/t-sql/functions/permissions-transact-sql?view=sql-server-2017
      CheckPermission($0001, dpSelect);
      CheckPermission($0008, dpInsert);
      CheckPermission($0002, dpUpdate);
      CheckPermission($0010, dpDelete);
    end
  else
    Result := inherited DatasetPermissions(DatasetName);
end;

class function TDeADODatabase.SupportQueryTypes: TDeQueryTypes;
begin
  Result := inherited SupportQueryTypes + [qtHole];
end;

function TDeADODatabase.TableToStr(const tableName, tableAlias, tableDatabase, tableSchema: String): string;
begin
  Result:= '['+tableName+']';
  if Length(tableAlias)>0 then Result:= Result+' as '+tableAlias;

  if Length(tableDatabase)>0
    then if Length(tableSchema)>0
           then Result:='[' + tableDatabase + '].[' + tableSchema + '].' + Result
           else Result:='[' + tableDatabase + '].[dbo].' + Result
    else if Length(tableSchema)>0
           then Result:= '['+tableSchema + '].' + Result
end;

function TDeADODatabase.FieldToStr(const fieldName, fieldAlias, tableAlias: String;
  const fieldOperation: TOperationType; const DataType: TFieldType; const DataSize: Integer): string;
begin
  if Length(tableAlias) = 0 then Result:=               '[' + fieldName + ']'
                            else Result:= tableAlias + '.[' + fieldName + ']';

  if fieldOperation in AgregateOperations then
    Result := OperationLexeme[fieldOperation] + '(' + Result + ')';

  if DataType in DateTimeTypes then
    begin
      if DataType in [ftDate] then Result := 'cast(' + Result + ' as date)'
                              else Result := 'cast(' + Result + ' as datetime)';
      Result:= Result + ' as [' + iif( Length(fieldAlias) = 0, FieldName, fieldAlias) + ']';
    end else
  if (Length(FieldAlias) <> 0) and (fieldName <> fieldAlias) then
    Result := Result + ' as [' + FieldAlias + ']';
end;

function TDeADODatabase.FuncToStr(aFunction: string; aParams: array of variant): string;
begin
  Result:= EmptyStr;
  if SameText(aFunction,'SUBSTRING') then
      Result:=Format('substring(%s,%d,%d)', [VarToStr(aParams[0]), VarToInt(aParams[1]), VarToInt(aParams[2])]) else

  if SameText(aFunction,'LENGTH') then
      Result:=Format('len(%s)', [VarToStr(aParams[0])]) else

  if SameText(aFunction,'INCDAY') then
      Result:=Format('DateAdd(day,%s,%s)', [aParams[1], VarToStr(aParams[0])]) else

  if SameText(aFunction,'NOW') then
      Result:='GETDATE()' else

  raise EDeDatasetError.Create('Function '+aFunction+' is not supported by your database');
end;

function TDeADODatabase.FieldExists(const TableName, FieldName: string): Boolean;
var
  Query: TADOQuery;
begin
  if DatabaseType = dtMSSQL then
    begin
      Query := TADOQuery.Create(Self);
      try
        Query.BeforeOpen := BeforeOpen;
        Query.Connection:= TADOConnection(ConnectionObject);
        Query.SQL.Text := 'select [column_id] from [sys].[columns] where [object_id] = Object_ID('
                                                 + QuotedStr(TableName) + ', ''U'') and [name] like '''+FieldName+'''';

  //    параметризация удваивает время обращения к серверу, что критично при удаленном подключении
  //                                                         + QuotedStr(TableName) + ', ''U'') and [name] like :name';
  //    Query.Prepared := True;
  //    Query.Parameters[0].Value := FieldName;
        Query.Open;
        Result := not Query.Fields[0].IsNull;
      finally
        Query.Free;
      end;
    end
  else
    Result := inherited FieldExists(TableName, FieldName); // Здесь можно реализовать логику для других БД ...
end;

procedure TDeADODatabase.TargetToSourceFieldType(var aFieldData: TFieldData);
begin
  if (aFieldData.TargetType in [ftTime, ftTimeStamp, ftTimeStampOffset]) then
    begin
      aFieldData.SourceType:= ftDateTime;
      aFieldData.SourceSize:= 0;
    end else

  if (aFieldData.TargetType = ftDate) and not (FDatabaseDriver = ddNativeClient) then
    begin
      aFieldData.SourceType:= ftDateTime;
      aFieldData.SourceSize:= 0;
    end else

  inherited TargetToSourceFieldType(aFieldData);
end;

function TDeADODatabase.IsFieldTypeConvert(const LocalType: TFieldType; const LocalSize: Integer;
                                           var RealType: TFieldType; var RealSize: Integer): Boolean;
begin
//TODO: Надо рабораться в необходимости подмены каждого типа для каждого провайдера
//TODO: Надо расширить модель полей, добавить Precision и Scale
  case LocalType of
    ftTime: { Время }
      begin
        // Преобразуем, т.к. не работает и в OleDB, и в NativeClient!
        RealType := ftDateTime;
        RealSize := 0;
        Result := True;
      end;
    ftDate: { Дата }
      // Работает только в NativeClient!
      if FDatabaseDriver = ddNativeClient then
        Result := inherited IsFieldTypeConvert(LocalType, LocalSize, RealType, RealSize)
      else
        begin
          // Преобразуем, т.к. не работает в OleDB!
          RealType := ftDateTime;
          RealSize := 0;
          Result := True;
        end;
    ftDateTime, ftTimeStamp, ftTimeStampOffset:
      begin
        RealType := ftDateTime;
        RealSize := 0;
        Result := True;
      end
    else
        Result := inherited IsFieldTypeConvert(LocalType, LocalSize, RealType, RealSize);
  end;
end;

function TDeADODatabase.Sequence(const aType: TDeSequenceType; const aTableName, aFieldName: String): Boolean;
{$IFDEF DEBUG}
const
  SequenceTypes: array[TDeSequenceType] of PChar = ('stInsert', 'stUpdate', 'stDelete', 'stHaving');
{$ENDIF}
var Q: TADOQuery;
    aValue: Int64;
    sSequenceName: String;
begin
  Result:= False;
  if Length(aFieldName) = 0 then sSequenceName:= aTableName
                            else sSequenceName:= aTableName + '_' + aFieldName + '_seq';
  try
    Q:= TADOQuery.Create(Self);
    Q.Connection := TADOConnection(ConnectionObject);
    Q.BeforeOpen := BeforeOpen;

    case aType of
      stInsert:
        try
          Q.SQL.Text:= 'SELECT isnull(max([' + aFieldName + ']), 0) + 1 FROM [' + aTableName + ']';
          Q.Open;
          aValue:= Q.Fields[0].Value;
          Q.Close;

          Q.SQL.Text:= 'CREATE sequence [' + sSequenceName + '] START WITH ' + IntToStr(aValue) + ' INCREMENT BY 1';
          Q.ExecSQL;
          Result:= True;
        except
          {$IFDEF DEBUG}
          on E: Exception do
            DebugLog('%s.Sequence(%s, %s, %s) skip error: %s', [StrPas(SequenceTypes[aType]), QuotedStr(aTableName), QuotedStr(aFieldName), E.Message]);
          {$ENDIF}
        end;
      stUpdate:
        try
          Q.SQL.Text:= 'SELECT isnull(max([' + aFieldName + ']), 0) + 1 FROM [' + aTableName + ']';
          Q.Open;
          aValue:= Q.Fields[0].Value;
          Q.Close;

          Q.SQL.Text:= 'ALTER sequence [' + sSequenceName + '] RESTART WITH ' + IntToStr(aValue) + ' INCREMENT BY 1';
          Q.ExecSQL;
          Result:= True;
        except
          {$IFDEF DEBUG}
          on E: Exception do
            DebugLog('%s.Sequence(%s, %s, %s) skip error: %s', [StrPas(SequenceTypes[aType]), QuotedStr(aTableName), QuotedStr(aFieldName), E.Message]);
          {$ENDIF}
        end;
      stDelete:
        try
          Q.SQl.Text:= 'DROP sequence [' + sSequenceName + ']';
          Q.ExecSQL;
          Result:= True;
        except
          {$IFDEF DEBUG}
          on E: Exception do
            DebugLog('%s.Sequence(%s, %s, %s) skip error: %s', [StrPas(SequenceTypes[aType]), QuotedStr(aTableName), QuotedStr(aFieldName), E.Message]);
          {$ENDIF}
        end;
      stHaving:
        try
//          Q.SQL.Text:= 'SELECT last_value FROM "' + sSequenceName + '"';
          Q.SQL.Text:= 'SELECT * FROM sys.sequences WHERE [name] = ''' + sSequenceName + '''';
          Q.Open;
          Result:= (Q.RecordCount = 1);
          Q.Close;
        except
          {$IFDEF DEBUG}
          on E: Exception do
            DebugLog('%s.Sequence(%s, %s, %s) skip error: %s', [StrPas(SequenceTypes[aType]), QuotedStr(aTableName), QuotedStr(aFieldName), E.Message]);
          {$ENDIF}
        end;
    end;
  finally
    Q.Free;
  end;
end;

procedure TDeADODatabase.SetConnected(const aConnected: Boolean);
begin
  inherited SetConnected(aConnected);
  {$IFDEF DEBUG}
  if aConnected then
    AsyncOpen('SELECT DATABASEPROPERTYEX('''+Database+''', ''Collation'')', Async_Read_CI_AI);
  {$ENDIF}
end;

procedure TDeADODatabase.Read_CI_AI;
var  Q: TADOQuery;
     s: string;
begin
  if not ConnectionObject.Connected then Connected:= True;

  if ConnectionObject.Connected then
    begin
      s:= EmptyStr;

      Q:= TADOQuery.Create(self);
      try
        Q.BeforeOpen:= BeforeOpen;
        Q.Connection:= TADOConnection(ConnectionObject);
        Q.SQL.Text:= 'SELECT DATABASEPROPERTYEX('''+Database+''', ''Collation'')';

//      Q.SQL.Text   := 'SELECT DATABASEPROPERTYEX(:BASENAME, ''Collation'')';
//      Q.Prepared   := True;
//      Q.Parameters.ParamByName('BASENAME').Value := Database;

        Q.Open;
        if Q.RecordCount = 1 then s:= Q.Fields[0].AsString;
        Q.Close;
      finally
        Q.Free;
      end;

      if Length(s) > 0 then
        begin
          FCI:= Pos('_CI', s) > 0;
          FAI:= Pos('_AI', s) > 0;
        end;

      FReady_CI_AI:= True;
    end;
end;

procedure TDeADODatabase.ReadDDL;
var
  Query: TADOQuery;
begin
  if not ConnectionObject.Connected then Connected := True;
  if ConnectionObject.Connected then
    begin
      Query := TADOQuery.Create(Self);
      try
        Query.BeforeOpen := BeforeOpen;
        Query.Connection := TADOConnection(ConnectionObject);
        Query.SQL.Text := 'SELECT IS_SRVROLEMEMBER(''sysadmin'')';
        Query.Open;
        FDDL := Query.Fields[0].AsInteger = 1;
      finally
        Query.Free;
      end;
      FReadyDDL := True;
    end;
end;

function TDeADODatabase.ReadServerDateTime(var DateTime: TDateTime): Boolean;
var
  Query: TADOQuery;
begin
  Result := ConnectionObject.Connected;
  if Result then
    begin
      Query := TADOQuery.Create(Self);
      try
        Query.BeforeOpen := BeforeOpen;
        Query.Connection := TADOConnection(ConnectionObject);
        Query.SQL.Text := 'SELECT GETDATE()';
        Query.Open;
        DateTime := Query.Fields[0].AsDateTime;
      finally
        Query.Free;
      end;
    end;
end;

function TDeADODatabase.GetCI: Boolean;
begin
  if connected and not FReady_CI_AI then Read_CI_AI;
  Result:=FCI;
end;

function TDeADODatabase.GetAI: Boolean;
begin
  if connected and not FReady_CI_AI then Read_CI_AI;
  Result:=FAI;
end;

function TDeADODatabase.GetDDL: Boolean;
begin
  if connected and not FReadyDDL then ReadDDL;
  Result := FDDL;
end;

procedure TDeADODatabase.ReadLoginSchema;
var
  Query: TADOQuery;
begin
  if not ConnectionObject.Connected then Connected := True;
  if ConnectionObject.Connected then
    begin
      Query := TADOQuery.Create(Self);
      try
        Query.BeforeOpen := BeforeOpen;
        Query.Connection := TADOConnection(ConnectionObject);
        Query.SQL.Text := 'SELECT SCHEMA_NAME()';
        Query.Open;
        FLoginSchema := Trim(Query.Fields[0].AsString);
      finally
        Query.Free;
      end;
      FReadyLoginSchema := True;
    end;
end;

function TDeADODatabase.GetLoginSchema: string;
begin
  if Connected and not FReadyLoginSchema then ReadLoginSchema;
  Result := FLoginSchema;
end;

function TDeADODatabase.CreateQueryObject(const aQueryType: TDeQueryType; InDS: TDeDataset): TDeDataset;
begin
  Result := TDeADODataset.Create(Self);
  if Assigned(ConnectionObject) then
    TADOQuery(TDeADODataset(Result).SourceSet).Connection := TADOConnection(ConnectionObject);
  if Assigned(TDeADODataset(Result).SourceSet) and (DatabaseType = dtMSSql) then
    (TDeADODataset(Result).SourceSet as TADOQuery).CacheSize := 1000;
end;

function TDeADODatabase.FieldMetaToDDL(Source: TObject; const KeyEnabled: Boolean): string;
var
  FieldMeta: TFieldMeta;
  FieldsMeta: TFieldsMeta;
  FieldIndex: Integer;
  function PrepareTypeSize(const TypeName: string; const MaxSize: Integer = 8000): string;
  begin
    Result := TypeName + '('
              + iif((FieldMeta.DataSize = -1) or (FieldMeta.DataSize <= MaxSize), IntToStr(FieldMeta.DataSize), 'max')
              + ')';
  end;
begin
  Result := EmptyStr;
  if Assigned(Source) then
    if Source is TFieldMeta then
      begin
        FieldMeta := Source as TFieldMeta;
        if FieldMeta.IsStored then
          begin
            Result := FieldToStr(FieldMeta.Original) + ' ';
            case FieldMeta.DataType of
              ftFixedChar:      Result:= Result + PrepareTypeSize('char');
              ftFixedWideChar:  Result:= Result + PrepareTypeSize('nchar');
              ftString:         Result:= Result + PrepareTypeSize('varchar');
              ftWideString:     Result:= Result + PrepareTypeSize('nvarchar');
              ftMemo:           Result:= Result + 'text';
              ftWideMemo:       Result:= Result + 'ntext';
              ftBoolean:        Result:= Result + 'bit';
              ftByte:           Result:= Result + 'tinyint';
              ftWord:           Result:= Result + 'int';
              ftSmallInt:       Result:= Result + 'smallint';
              ftShortInt:       Result:= Result + 'shortint';
              ftInteger:        Result:= Result + 'int';
              ftLargeInt:       Result:= Result + 'bigint';
              ftAutoInc:        Result:= Result + 'int';
              ftDate:           Result:= Result + 'date';
              ftTime:           Result:= Result + 'time';
              ftDateTime:       Result:= Result + 'datetime';
              ftTimeStamp:      Result:= Result + 'timestamp';
              ftTimeStampOffset:Result:= Result + 'datetimeoffset(7)';
              ftSingle:         Result:= Result + 'real';
              ftFloat:          Result:= Result + 'float';
              ftCurrency:       Result:= Result + 'money';
              ftBCD:            Result:= Result + 'decimal(18, 0)';
              ftGUID:           Result:= Result + 'uniqueidentifier';
              ftBytes:          Result:= Result + PrepareTypeSize('binary');
              ftVarBytes:       Result:= Result + PrepareTypeSize('varbinary');
              ftBlob:           Result:= Result + 'image';
            end;
            if FieldMeta.NotNull then Result := Result + ' NOT NULL';
            if KeyEnabled and FieldMeta.Key then Result := Result + ' PRIMARY KEY';
          end;
      end
    else if Source is TFieldsMeta then
      begin
        FieldsMeta := Source as TFieldsMeta;
        for FieldIndex := 0 to Pred(FieldsMeta.Count) do
          if FieldsMeta[FieldIndex].IsStored then
            funcs.StrAdd(Result, ', ', FieldMetaToDDL(FieldsMeta[FieldIndex], KeyEnabled));
      end;
end;

function TDeADODatabase.CreateTable(Source: TObject): Boolean;
var TM: TTableMeta;
    Q : TADOQuery;
begin
  try
    Result := Assigned(Source) and (Source is TTableMeta) and (DatabaseType = dtMSSQL);
    if Result then
      begin
        TM := TTableMeta(Source);
        Q := TADOQuery.Create(Self);
        try
          Q.Connection := TADOConnection(ConnectionObject);
          Q.SQL.Text := 'CREATE TABLE [' + TM.Table + '] (' + FieldMetaToDDL(TM.Fields, True) + ')';
          try
            Q.ExecSQL;
          finally
            WriteQueryLog(Q);
          end;
        finally
          Q.Free;
        end;
      end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog(ClassName + '.CreateTable skip error: ' + E.Message);
        {$ENDIF}
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Create table error: ' + E.Message);
        {$ENDIF}
        Result := False;
      end;
  end
end;

function TDeADODatabase.CreateField(Source: TObject): Boolean;
var
  FieldMeta: TFieldMeta;
  FieldsMeta: TFieldsMeta;
  Query: TADOQuery;
begin
  try
    Result := Assigned(Source) and (DatabaseType = dtMSSQL);
    if Result then
      if Source is TFieldMeta then
        begin
          FieldMeta := Source as TFieldMeta;
          Query := TADOQuery.Create(Self);
          try
            Query.Connection := TADOConnection(ConnectionObject);
            Query.SQL.Text := 'ALTER TABLE [' + FieldMeta.Owner.Table + '] ADD ' + FieldMetaToDDL(FieldMeta, False);
            try
              Query.ExecSQL;
            finally
              WriteQueryLog(Query);
            end;
          finally
            Query.Free;
          end;
        end
      else if Source is TFieldsMeta then
        begin
          FieldsMeta := Source as TFieldsMeta;
          Query := TADOQuery.Create(Self);
          try
            Query.Connection := TADOConnection(ConnectionObject);
            Query.SQL.Text := 'ALTER TABLE [' + FieldsMeta[0].Owner.Table + '] ADD ' + FieldMetaToDDL(FieldsMeta, False);
            try
              Query.ExecSQL;
            finally
              WriteQueryLog(Query);
            end;
          finally
            Query.Free;
          end;
        end
      else
        Result := False;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog(ClassName + '.CreateField skip error: ' + E.Message);
        {$ENDIF}
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Create field error: ' + E.Message);
        {$ENDIF}
        Result := False;
      end;
  end
end;

function TDeADODatabase.ModifyField(Source: TObject): Boolean;
var
  FieldMeta: TFieldMeta;
  FieldsMeta: TFieldsMeta;
  Query: TADOQuery;
begin
  try
    Result := Assigned(Source) and (DatabaseType = dtMSSQL);
    if Result then
      if Source is TFieldMeta then
        begin
          FieldMeta := Source as TFieldMeta;
          Query := TADOQuery.Create(Self);
          try
            Query.Connection := TADOConnection(ConnectionObject);
            Query.SQL.Text := 'ALTER TABLE [' + FieldMeta.Owner.Table + '] ALTER COLUMN ' + FieldMetaToDDL(FieldMeta, False);
            try
              Query.ExecSQL;
            finally
              WriteQueryLog(Query);
            end;
          finally
            Query.Free;
          end;
        end
      else if Source is TFieldsMeta then
        begin
          FieldsMeta := Source as TFieldsMeta;
          Query := TADOQuery.Create(Self);
          try
            Query.Connection := TADOConnection(ConnectionObject);
            Query.SQL.Text := 'ALTER TABLE [' + FieldsMeta[0].Owner.Table + '] ALTER COLUMN ' + FieldMetaToDDL(FieldsMeta, False);
            try
              Query.ExecSQL;
            finally
              WriteQueryLog(Query);
            end;
          finally
            Query.Free;
          end;
        end
      else
        Result := False;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog(ClassName + '.ModifyField skip error: ' + E.Message);
        {$ENDIF}
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Create field error: ' + E.Message);
        {$ENDIF}
        Result := False;
      end;
  end
end;

function TDeADODatabase.DeleteField(Source: TObject): Boolean;
var
  FieldMeta: TFieldMeta;
  Query: TADOQuery;
begin
  try
    Result := Assigned(Source);
    if Result then
      if Source is TFieldMeta then
        begin
          FieldMeta := Source as TFieldMeta;
          Query := TADOQuery.Create(Self);
          try
            Query.Connection := TADOConnection(ConnectionObject);
            Query.SQL.Text := 'ALTER TABLE [' + FieldMeta.Owner.Table + '] DROP COLUMN [' + FieldMeta.Original + ']';
            try
              Query.ExecSQL;
            finally
              WriteQueryLog(Query);
            end;
          finally
            Query.Free;
          end;
        end
      else
        Result := False;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog(ClassName + '.DeleteField skip error: ' + E.Message);
        {$ENDIF}
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Delete field error: ' + E.Message);
        {$ENDIF}
        Result := False;
      end;
  end
end;

function TDeADODatabase.ExecuteStoredProcedure(const aProcedureName: string; aList: TDeVariableList): Integer;
var Q: TADOStoredProc;
    ItemIndex, VariableIndex: Integer;
    ErrorText, LineText, RaiseText: string;
    FileStream: TFileStream;
    BinData: OleVariant;
    DataPtr: Pointer;
    FullFileName, FileName: AnsiString;
    NameSize: SmallInt;

begin
  Assert( aList is TDeVariableList, 'aList not TDeVariableList');

  try
    Q:=TADOStoredProc.Create(self);
    try
    Q.Connection:=TADOConnection(ConnectionObject);
    Q.ProcedureName:='['+aProcedureName+']';

    // 11.04.16 + Тайм-аут выполнения команды
    VariableIndex := aList.IndexByName(varActionTimeout);
    if VariableIndex <> -1 then
      Q.CommandTimeout := VarToInt(aList[VariableIndex].Value);
    // 11.04.16 -
    Q.Parameters.Refresh;

    // 11.04.16 + Тайм-аут выполнения команды
    VariableIndex := aList.IndexByName(varActionTimeout);
    if VariableIndex <> -1 then
      Q.CommandTimeout := VarToInt(aList[VariableIndex].Value);
    // 11.04.16 -
    Q.Parameters.Refresh;

    for ItemIndex := 0 to Pred(Q.Parameters.Count) do
      with Q.Parameters[ItemIndex] do
        if not (Q.Parameters[ItemIndex].Direction in [pdOutput, pdReturnValue]) then
          begin
            VariableIndex := aList.IndexByName(Name);
            if (VariableIndex = -1) and (Name[1] = '@') then
              VariableIndex := aList.IndexByName(Copy(Name, 2, MaxInt));

            if VariableIndex <> -1 then
              if DataType = ftVarBytes then
                begin
                  FullFileName:= aList[VariableIndex].Value;
                  if FileExists(FullFileName) then
                    begin
                      FileStream:= TFileStream.Create(FullFileName, fmOpenRead or fmShareDenyNone);
                      try
                        FileName:= ExtractFileName(FullFileName);
                        NameSize:= Length(FileName);

                        //TODO: Присохранении файлов мы ВСЕГДА в хвост добавляли один ненужный символ
                        //придется так и жить или менять сигнатуру для файлов сохраненных по правильному
                        BinData:= VarArrayCreate([0, DeSLength + SizeOf(Smallint) + NameSize + FileStream.Size {-1}], varByte);
                        DataPtr:= VarArrayLock(BinData);

                        Move(DeSignature[1], DataPtr^, DeSLength);
                        Move(NameSize, Ptr(cardinal(DataPtr) + DeSLength)^, SizeOf(Smallint));
                        Move(FileName[1], Ptr(cardinal(DataPtr) + DeSLength + SizeOf(Smallint))^, NameSize);
                        FileStream.ReadBuffer(Ptr(cardinal(DataPtr) + DeSLength + SizeOf(Smallint)  + NameSize)^, FileStream.Size);
                        Q.Parameters[ItemIndex].Value:= BinData;
                      finally
                        VarArrayUnlock(BinData);
                        FileStream.Free;
                      end;
                    end
                  else
                    begin
                      Q.Parameters[ItemIndex].Value:= unassigned;
                    end;
                end
              else
                begin
                  Value := aList[VariableIndex].Value;
                end;

            {
            else  //если параметр не задан и не определено NULL значение, то его и передаем NULL
            if not (paNullable in Q.Parameters[ItemIndex].Attributes) then
              Q.Parameters[ItemIndex].Value := null
            {}
        end;

    {$IFDEF DEBUG}
      aList.DebugVariablesLog(Format('%s: before execute stored procedure %s variables ...', [ClassName, QuotedStr(aProcedureName)]));
    {$ENDIF}

    WriteQueryLog(Q);
    Q.ExecProc;
    finally
      if aList is TDeVariableList then
        for ItemIndex := 0 to Pred(Q.Parameters.Count) do
          case Q.Parameters[ItemIndex].Direction of
            pdOutput, pdInputOutput: { Стандартные параметры }
              begin
                VariableIndex := (aList as TDeVariableList).IndexByName(Q.Parameters[ItemIndex].Name);
                if (VariableIndex = -1) and (Q.Parameters[ItemIndex].Name[1] = '@') then
                  VariableIndex := (aList as TDeVariableList).IndexByName(Copy(Q.Parameters[ItemIndex].Name, 2, MaxInt));
                if VariableIndex <> -1 then
                  with (aList as TDeVariableList)[VariableIndex] do
                    begin
                      IsOutput:= True;
                      Value:= Q.Parameters[ItemIndex].Value;
                    end;
              end;
            pdReturnValue: { Возможно результат работы хранимой процедуры }
              if SameText('@RETURN_VALUE', Q.Parameters[ItemIndex].Name) then
                begin
                  with (aList as TDeVariableList).GetByName(varActionResult) do
                    begin
                      IsOutput:= True;
                      Value:= Q.Parameters[ItemIndex].Value;
                    end;
                end;
          end;
      Q.Free;
    end;

    {$IFDEF DEBUG}
    if aList is TDeVariableList then
      (aList as TDeVariableList).DebugVariablesLog(Format('%s: after execute stored procedure %s variables ...', [ClassName, QuotedStr(aProcedureName)]));
    {$ENDIF}
    Result:=0;
  except
    on E: Exception do
      begin
        ErrorText := Trim(E.Message);
        RaiseText := EmptyStr;
        Result := 255;
        while Length(ErrorText) <> 0 do
          begin
            ItemIndex := Pos('||', ErrorText);
            if ItemIndex = 0 then
              begin
                LineText := ErrorText;
                ErrorText := EmptyStr;
              end
            else
              begin
                LineText := Trim(Copy(ErrorText, 1, Pred(ItemIndex)));
                ErrorText := Trim(Copy(ErrorText, ItemIndex + 2, Length(ErrorText)));
              end;
            if SameText(Copy(LineText, 1, 7), 'Script:') then
              begin
                Result := -1;
                MetaData.ExecuteScript(Copy(LineText, 8, MaxInt));
              end
            else if SameText(Copy(LineText, 1, 11), 'SendMessage') then
              begin
                Result:=-1;
                SendMessage(Application.MainForm.Handle, DM_EXTSENDMESSAGE, NativeInt(PChar(Copy(LineText, 12, MaxInt))), 0);
              end
            else
              begin
                if Length(RaiseText) <> 0 then RaiseText := RaiseText + '||';
                RaiseText := RaiseText + LineText;
              end;
          end;
        if Length(RaiseText) <> 0 then
          raise Exception.Create(RaiseText);
      end;
  end;
end;

function TDeADODatabase.RetrieveMetaObjectType(const ObjectName: string): TObjectType;
var
  Query: TADOQuery;
  Index: Integer;
  function PrepareObjectTypeString(const TypeName: string; const ObjectType: TObjectType): string; inline;
  begin
    if Length(TypeName) = 0 then
      Result := ' else '
    else
      Result := ' when ' + QuotedStr(TypeName) + ' then ';
    Result := Result + 'cast(' + IntToStr(Ord(ObjectType)) + ' as int)';
  end;
begin
  Result := inherited RetrieveMetaObjectType(ObjectName);
  if DatabaseType = dtMSSQL then
    begin
      if not Assigned(FObjectTypes) and CheckConnection then
        begin
          FObjectTypes := TStringList.Create;
          try
            Query := TADOQuery.Create(Self);
            try
              Query.BeforeOpen := BeforeOpen;
              Query.Connection:= TADOConnection(ConnectionObject);
              Query.SQL.Text := 'select [name], case [type]' +
                                PrepareObjectTypeString('P', otStoredProc) +
                                PrepareObjectTypeString('U', otTable) +
                                PrepareObjectTypeString('V', otView) +
                                PrepareObjectTypeString(EmptyStr, otUnknown) +
                                ' end as [type] from [sys].[all_objects] where ([object_id]>0) and ([type] in (''P'', ''U'', ''V'')) order by [name]';
              Query.Open;
              while not Query.Eof do
                begin
                  FObjectTypes.AddObject(Query.Fields[0].AsString, Pointer(Query.Fields[1].AsInteger));
                  Query.Next;
                end;
            finally
              Query.Free;
            end;
          except
            FreeAndNil(FObjectTypes);
            raise;
          end;
        end;
      if Assigned(FObjectTypes) then
        begin
          Index := FObjectTypes.IndexOf(ObjectName);
          if Index <> -1 then
            Result := TObjectType(FObjectTypes.Objects[Index])
          else
            Result := otUnknown;
        end
      else
        Result := otUnknown;
    end;
end;

function TDeADODatabase.AsyncOpen(const AQueryText: string; AOnDataset: TDeAsyncDatasetEvent; AOnException: TDeAsyncExceptionEvent): Boolean;
var
  TempConnectionString: string;
  procedure AppendParameter(const Name, Value: string);
  begin
    if Length(TempConnectionString) <> 0 then
      TempConnectionString := TempConnectionString + ';';
    TempConnectionString := TempConnectionString + Name + '=' + Value;
  end;
begin
  Result := (DatabaseType = dtMSSQL) and (Length(AQueryText) <> 0) and Assigned(AOnDataset);
  if Result then
    try
      if not Assigned(FAsyncThreads) then
        begin
          TempConnectionString := BuildConnectString;
          if Length(Trim(Login)) <> 0 then
            begin
              AppendParameter(cUserIDParamADO, Login);
              AppendParameter(cPasswordParamADO, Password);
            end
          else
            begin
              AppendParameter(cUserIDParamADO, GetWindowsUserName);
              AppendParameter(cIntegratedSecurityParamADO, cSSPIParamValueADO);
            end;
          FAsyncThreads := TAsyncThreadManager.Create(TempConnectionString, 2); // Пока ограничим двумя потоками ...
        end;
      Result := FAsyncThreads.Open(AQueryText, AOnDataset, AOnException);
    except
      on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog(ClassName + '.AsyncOpen skip error: ' + E.Message);
          {$ENDIF}
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Async query error: ' + E.Message);
          {$ENDIF}
          Result := False;
        end;
    end;
end;

{$IFDEF DEBUG}
procedure TDeADODatabase.Async_Read_CI_AI(Sender: TObject; const RawValue: Variant);
var
  S: string;
begin
  S := VarArrayGet(VarArrayGet(RawValue, [0]), [0]);
  if Length(S) <> 0 then
    begin
      FCI := Pos('_CI', S) <> 0;
      FAI := Pos('_AI', S) <> 0;
    end;
  FReady_CI_AI := True;
end;
{$ENDIF}

function TDeADODatabase.AsyncExecute(const AQueryText: string; AOnExecute: TDeAsyncExecuteEvent; AOnException: TDeAsyncExceptionEvent): Boolean;
var
  TempConnectionString: string;
  procedure AppendParameter(const Name, Value: string);
  begin
    if Length(TempConnectionString) <> 0 then
      TempConnectionString := TempConnectionString + ';';
    TempConnectionString := TempConnectionString + Name + '=' + Value;
  end;
begin
  Result := (DatabaseType = dtMSSQL) and (Length(AQueryText) <> 0) and Assigned(AOnExecute);
  if Result then
    try
      TempConnectionString := BuildConnectString;
      if Length(Trim(Login)) <> 0 then
        begin
          AppendParameter(cUserIDParamADO, Login);
          AppendParameter(cPasswordParamADO, Password);
        end
      else
        begin
          AppendParameter(cUserIDParamADO, GetWindowsUserName);
          AppendParameter(cIntegratedSecurityParamADO, cSSPIParamValueADO);
        end;
      if not Assigned(FAsyncThreads) then
        begin
          TempConnectionString := BuildConnectString;
          if Length(Trim(Login)) <> 0 then
            begin
              AppendParameter(cUserIDParamADO, Login);
              AppendParameter(cPasswordParamADO, Password);
            end
          else
            begin
              AppendParameter(cUserIDParamADO, GetWindowsUserName);
              AppendParameter(cIntegratedSecurityParamADO, cSSPIParamValueADO);
            end;
          FAsyncThreads := TAsyncThreadManager.Create(TempConnectionString, 2); // Пока ограничим двумя потоками ...
        end;
      Result := FAsyncThreads.Execute(AQueryText, AOnExecute, AOnException);
    except
      on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog(ClassName + '.AsyncExecute skip error: ' + E.Message);
          {$ENDIF}
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Async query error: ' + E.Message);
          {$ENDIF}
          Result := False;
        end;
    end;
end;

procedure TDeADODatabase.BeforeOpen(DataSet: TDataSet);
begin
  WriteQueryLog(DataSet);
end;

{ TDeOracleDatabase }
{ ---------------------------------------------------------------------------- }
 {
constructor TDeOracleDatabase.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  DatabaseType := dtOracle;
end;

destructor TDeOracleDatabase.Destroy;
begin
  DestroyConnection;
  inherited;
end;

procedure TDeOracleDatabase.CreateConnection;
begin
  FConnectionObject := TOraSession.Create(Self);
  FConnectionObject.AutoCommit    := True;
  FConnectionObject.ConnectPrompt := False;
  FConnectionObject.Server   := Database;
  FConnectionObject.Username := Login;
  FConnectionObject.Password := Password;
  if Login='SYS' then
    FConnectionObject.ConnectMode:=cmSysDBA;
  if Assigned(FConnectionObject) then
    ConnectionObject.FreeNotification(Self);
end;

procedure TDeOracleDatabase.DestroyConnection;
begin
  FreeAndNil(FConnectionObject);
end;

function TDeOracleDatabase.GetConnectionObject: TOraSession;
begin
  if not Assigned(FConnectionObject) then
    CreateConnection;
  result := FConnectionObject;
end;

function TDeOracleDatabase.GetConnected: boolean;
begin
  result := Assigned(ConnectionObject) and ConnectionObject.Connected;
end;

procedure TDeOracleDatabase.RetrieveTableNames(aTablesList: TStringList);
var Q: TOraQuery;
begin
  if Assigned(ConnectionObject)and(CheckConnection) then
    begin
        Q:=TOraQuery.Create(self);
      try
        Q.Session := ConnectionObject;
        Q.SQL.Text:=  'select o.name as tName, o.type#'+
                     ' from sys.obj$ o, sys.user$ u'+
                     ' where Upper(u.name) = :pName'+
                     ' and o.owner# = u.user#'+
                     ' and o.Type# = 2';
        Q.Prepare;
        Q.ParamByName('pName').Value:=UpperCase(ConnectionObject.Username);
        Q.Open;//Execute;

        while  not Q.Eof do
        begin
          FTablesList.Add(Q.FieldValues['tName']);
          Q.Next;
        end;
      finally
        Q.Free;
      end;
    end;
end;

function TDeOracleDatabase.CreateQueryObject(aQueryType : TDeQueryType; InDS:TDeDataset = nil) : TDeDataset;
begin
  result := TDeOracleDataset.Create(Self);
  if Assigned(ConnectionObject) then
    with TOraQuery(TDeOracleDataset(result).Data) do
    begin
      Session := ConnectionObject;
    end;
end;

procedure TDeOracleDatabase.SetConnected(const aConnected: boolean);
begin
  inherited SetConnected(aConnected);
  if aConnected then
  begin
    if Assigned(ConnectionObject) then
      FConnectionObject.Connected := aConnected
    else
      LastErrorDBType := DatabaseType  // проблемы с версией клиента
  end
  else
    DestroyConnection;  // если класс для коннекта к базе больше не нужен, то удалем его
end;
   }
{function TDeOracleDatabase.SQLExecuteResult(
  const SQLText: string): Variant;
begin
  result := Unassigned;
  with TOracleQuery.Create(Self)do
  try
    Session := ConnectionObject;
    SQL.Text := SQLText;
    Execute;
    result := Field(0);
    if VarIsEmpty(result)then
      result := Null;
  finally
    Free;
  end;
end;}
        {
procedure TDeOracleDatabase.UploadBLOB(const aTableName, aKeyFieldName,
  aBlobFieldName: string; const aKeyValue: Variant; aUploadTo: TStream);
begin

end;

procedure TDeOracleDatabase.LoadBLOB(const aTableName, aKeyFieldName,
  aBlobFieldName: string; const aKeyValue: Variant; aLoadFrom: TStream);
begin

end;
        {}

{ ---------------------------------------------------------------------------- }

{ TDeFileDatabase }

constructor TDeFileDatabase.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  DatabaseType := dtFileDB;
end;

procedure TDeFileDatabase.CreateConnection;
begin
  if Assigned(FConnectionObject) then Exit;
  FConnectionObject := TFileDB.Create(Self);
end;

function TDeFileDatabase.CreateQueryObject(const aQueryType: TDeQueryType; InDS: TDeDataset): TDeDataset;
begin
  if Assigned(ConnectionObject) then
  begin
    result := TFileDB_DEDS.Create(Self);
    TFileDB_DEDS(result).FileDB.Free;
    TFileDB_DEDS(result).FileDB:=nil;
    TFileDB_DEDS(result).FileDB := FConnectionObject;
  end
  else
    result := nil;
end;

destructor TDeFileDatabase.Destroy;
begin
  DestroyConnection;
  inherited Destroy;
end;

procedure TDeFileDatabase.DestroyConnection;
begin
  FConnectionObject.Free;
  FConnectionObject:=nil;
end;


function TDeFileDatabase.GetConnected: boolean;
begin
  Result:=False;
//  if not Assigned(FConnectionObject) then CreateConnection;
  if Assigned(ConnectionObject) then
    Result:=ConnectionObject.CurrentConnection.Connected;
end;

function TDeFileDatabase.GetConnectionObject: TFileDB;
begin
  if not Assigned(FConnectionObject) then
    CreateConnection;
  result := FConnectionObject;
end;

function TDeFileDatabase.GetTablePath(aTableName: string): string;
begin
  Result:='Test';
end;

procedure TDeFileDatabase.LoadBLOB(const aTableName, aKeyFieldName,
  aBlobFieldName: string; const aKeyValue: Variant; aLoadFrom: TStream);
begin
  inherited;
//
end;

procedure TDeFileDatabase.RetrieveTableNames(aTablesList: TStringList);
begin
  if CheckConnection then
  begin
    aTablesList.Clear;
    FConnectionObject.GetTableList(aTablesList);
  end;
end;

procedure TDeFileDatabase.SetConnected(const aConnected: boolean);
begin
  inherited SetConnected(aConnected);
  if aConnected then
    begin
      if Assigned(ConnectionObject)
        then ConnectionObject.ConnectToDB(True, 'localhost', 0, Login, Password, ConnectString)
        else LastErrorDBType:= DatabaseType  // проблемы с версией клиента
    end
  else
   DestroyConnection;
end;

procedure TDeFileDatabase.UploadBLOB(const aTableName, aKeyFieldName,
  aBlobFieldName: string; const aKeyValue: Variant; aUploadTo: TStream);
begin
  inherited;

end;

{ TDePGDatabase }

constructor TDePGDatabase.Create(aOwner: TComponent);
const LibName = 'libpq.dll';
var CurrentDir, VendorFile: string;
    Path: PChar;

  Function FindReсursive(StartDir: String; Mask: String = '*.*'): string;
  var SearchRec: TSearchRec;
  begin
    Result:= EmptyStr;
    if FileExists(StartDir+Mask) then Exit(StartDir+Mask);

    if FindFirst( StartDir+'*', faDirectory, SearchRec) = 0 then
      begin
        repeat
          Application.ProcessMessages;
          if (SearchRec.Attr and faDirectory) > 0 then
            if (SearchRec.Name <> '..') and (SearchRec.Name <> '.') then
              begin
                Result:= FindReсursive(StartDir+SearchRec.Name+'\', Mask);
                if Length(Result) > 0 then Exit;
              end;
        until FindNext(SearchRec) <> 0;
        FindClose(SearchRec);
      end;
  end;

begin
  if Not Assigned(DM.FPGDriverLink) then
    begin
      DM.FPGDriverLink:= TFDPhysPgDriverLink.Create(DM);
      with TFDPhysPgDriverLink(DM.FPGDriverLink) do
        begin
          // ищем в текущей папке или глубже
          CurrentDir:= ExtractFileDir(Application.ExeName)+'\';
          VendorFile:= FindReсursive(CurrentDir, LibName);

          // ищем на сайте dbco.ru
          if not SysUtils.FileExists(VendorFile) then
            if GetInetFile(urlDownload+LibName, CurrentDir+LibName) then
              VendorFile:= CurrentDir+LibName;

          // ищем в папке "program files"
          if not SysUtils.FileExists(VendorFile) then
            if SHGetFolderPath(0, CSIDL_PROGRAM_FILES, 0, 0, Path) = S_OK then
              begin
                VendorFile:= FindReсursive(StrPas(Path)+'\', LibName);
                if SysUtils.FileExists(VendorFile) then
                  if CopyFile(PChar(VendorFile), PChar(CurrentDir+LibName), False) then
                    VendorFile:= CurrentDir+LibName;
              end;

          if SysUtils.FileExists(VendorFile) then
            VendorLib:= VendorFile;
          Release;
        end;
    end;

  inherited Create(aOwner);
  FCanStoredProcedure:= True;
  DatabaseType:= dtPostgreSQL;
end;

destructor TDePGDatabase.Destroy;
begin
  //
  inherited;
end;

function TDePGDatabase.ExecuteStoredProcedure(const aProcedureName: string;
  aList: TDeVariableList): Integer;
var Q: TFDQuery;
    ItemIndex, VariableIndex: Integer;
    ErrorText, LineText, RaiseText: string;
    FileStream: TFileStream;
    BinData: OleVariant;
    DataPtr: Pointer;
    FullFileName, FileName: AnsiString;
    NameSize: SmallInt;
begin
  Assert( aList is TDeVariableList, 'aList not TDeVariableList');

  try
    Q:= TFDQuery.Create(self);
    try
    Q.Connection:= TFDConnection(ConnectionObject);
    Q.SQL.Add('call '+aProcedureName+'(1)');
    {
    for ItemIndex := 0 to Pred(Q.Params.Count) do
      begin
        Q.SQL.Add(Q.Params[ItemIndex].Name);
        if 0 < ItemIndex then Q.SQL.Add(',');
      end;
    Q.SQL.Add(')');
     {}
    Q.Prepared:= True;
{
    // 11.04.16 + Тайм-аут выполнения команды
    VariableIndex := aList.IndexByName(varActionTimeout);
    if VariableIndex <> -1 then
      Q.CommandTimeout := VarToInt(aList[VariableIndex].Value);
    // 11.04.16 -
    Q.Parameters.Refresh;

    // 11.04.16 + Тайм-аут выполнения команды
    VariableIndex := aList.IndexByName(varActionTimeout);
    if VariableIndex <> -1 then
      Q.CommandTimeout := VarToInt(aList[VariableIndex].Value);
    // 11.04.16 -
    Q.Parameters.Refresh;
{}
    for ItemIndex := 0 to Pred(Q.Params.Count) do
      with Q.Params[ItemIndex] do
        if not (Q.Params[ItemIndex].ParamType in [ptOutput, ptInputOutput]) then
          begin
            VariableIndex := aList.IndexByName(Name);
            if (VariableIndex = -1) and (Name[1] = '@') then
              VariableIndex := aList.IndexByName(Copy(Name, 2, MaxInt));

            if VariableIndex <> -1 then
              if DataType = ftVarBytes then
                begin
                  FullFileName:= aList[VariableIndex].Value;
                  if FileExists(FullFileName) then
                    begin
                      FileStream:= TFileStream.Create(FullFileName, fmOpenRead or fmShareDenyNone);
                      try
                        FileName:= ExtractFileName(FullFileName);
                        NameSize:= Length(FileName);

                        //TODO: Присохранении файлов мы ВСЕГДА в хвост добавляли один ненужный символ
                        //придется так и жить или менять сигнатуру для файлов сохраненных по правильному
                        BinData:= VarArrayCreate([0, DeSLength + SizeOf(Smallint) + NameSize + FileStream.Size {-1}], varByte);
                        DataPtr:= VarArrayLock(BinData);

                        Move(DeSignature[1], DataPtr^, DeSLength);
                        Move(NameSize, Ptr(cardinal(DataPtr) + DeSLength)^, SizeOf(Smallint));
                        Move(FileName[1], Ptr(cardinal(DataPtr) + DeSLength + SizeOf(Smallint))^, NameSize);
                        FileStream.ReadBuffer(Ptr(cardinal(DataPtr) + DeSLength + SizeOf(Smallint)  + NameSize)^, FileStream.Size);
                        Q.Params[ItemIndex].Value:= BinData;
                      finally
                        VarArrayUnlock(BinData);
                        FileStream.Free;
                      end;
                    end
                  else
                    begin
                      Q.Params[ItemIndex].Value:= unassigned;
                    end;
                end
              else
                begin
                  Value := aList[VariableIndex].Value;
                end;

            {
            else  //если параметр не задан и не определено NULL значение, то его и передаем NULL
            if not (paNullable in Q.Parameters[ItemIndex].Attributes) then
              Q.Parameters[ItemIndex].Value := null
            {}
        end;

    {$IFDEF DEBUG}
      aList.DebugVariablesLog(Format('%s: before execute stored procedure %s variables ...', [ClassName, QuotedStr(aProcedureName)]));
    {$ENDIF}

    WriteQueryLog(Q);
    Q.ExecSQL;
    finally
      if aList is TDeVariableList then
        for ItemIndex := 0 to Pred(Q.Params.Count) do
          case Q.Params[ItemIndex].ParamType of
            ptOutput, ptInputOutput: { Стандартные параметры }
              begin
                VariableIndex := (aList as TDeVariableList).IndexByName(Q.Params[ItemIndex].Name);
                if (VariableIndex = -1) and (Q.Params[ItemIndex].Name[1] = '@') then
                  VariableIndex := (aList as TDeVariableList).IndexByName(Copy(Q.Params[ItemIndex].Name, 2, MaxInt));
                if VariableIndex <> -1 then
                  with (aList as TDeVariableList)[VariableIndex] do
                    begin
                      IsOutput:= True;
                      Value:= Q.Params[ItemIndex].Value;
                    end;
              end;
            ptResult: { Возможно результат работы хранимой процедуры }
              if SameText('@RETURN_VALUE', Q.Params[ItemIndex].Name) then
                begin
                  with (aList as TDeVariableList).GetByName(varActionResult) do
                    begin
                      IsOutput:= True;
                      Value:= Q.Params[ItemIndex].Value;
                    end;
                end;
          end;
      Q.Free;
    end;

    {$IFDEF DEBUG}
    if aList is TDeVariableList then
      (aList as TDeVariableList).DebugVariablesLog(Format('%s: after execute stored procedure %s variables ...', [ClassName, QuotedStr(aProcedureName)]));
    {$ENDIF}
    Result:=0;
  except
    on E: Exception do
      begin
        ErrorText := Trim(E.Message);
        RaiseText := EmptyStr;
        Result := 255;
        while Length(ErrorText) <> 0 do
          begin
            ItemIndex := Pos('||', ErrorText);
            if ItemIndex = 0 then
              begin
                LineText := ErrorText;
                ErrorText := EmptyStr;
              end
            else
              begin
                LineText := Trim(Copy(ErrorText, 1, Pred(ItemIndex)));
                ErrorText := Trim(Copy(ErrorText, ItemIndex + 2, Length(ErrorText)));
              end;
            if SameText(Copy(LineText, 1, 7), 'Script:') then
              begin
                Result := -1;
                MetaData.ExecuteScript(Copy(LineText, 8, MaxInt));
              end
            else if SameText(Copy(LineText, 1, 11), 'SendMessage') then
              begin
                Result:=-1;
                SendMessage(Application.MainForm.Handle, DM_EXTSENDMESSAGE, NativeInt(PChar(Copy(LineText, 12, MaxInt))), 0);
              end
            else
              begin
                if Length(RaiseText) <> 0 then RaiseText := RaiseText + '||';
                RaiseText := RaiseText + LineText;
              end;
          end;
        if Length(RaiseText) <> 0 then
          raise Exception.Create(RaiseText);
      end;
  end;
end;

function TDePGDatabase.BuildConnectString : string;
begin
  if Length(Server) > 0 then
    result := Format('%s:%s', [Server, Database])
  else
    result := inherited BuildConnectString;
end;

function TDePGDatabase.CreateConnectionObject : TCustomConnection;
var P: Integer;
begin
  result := TFDConnection.Create(Application);
  result.LoginPrompt := False;

  with TFDConnection(result) do
  begin
    DriverName:='PG';

    P:=Pos(':',Server);
    if 1 < P then
      begin
        Params.Values['Server']:= Copy(Server, 1, P-1);
        Params.Values['Port']:= Copy(Server, P+1, MaxInt);
      end
    else
      begin
        Params.Values['Server']:= Server;
        Params.Values['Port']:= '5432';
      end;

    Params.Database := Database;
    Params.UserName := Login;
    Params.Password := Password;
  end;
end;

procedure TDePGDatabase.RetrieveTableNames(aTablesList: TStringList);
var Q   : TFDQuery;
begin
  if not Assigned(ConnectionObject) then Exit;

  Q:= TFDQuery.Create(Self);
  try
    Q.Connection := TFDConnection(ConnectionObject);
    Q.SQL.Text:=
      'SELECT table_name, table_type, table_schema '+
      'FROM information_schema.tables '+
      'WHERE table_schema NOT IN (''information_schema'',''pg_catalog'');';
    WriteQueryLog(Q);
    Q.Open;
    while Not Q.Eof do
      begin
        aTablesList.Add(Trim(Q.FieldValues['TABLE_NAME']));
        Q.Next;
      end;
    Q.Close;
  finally
  end;
  Q.Free;
//   TFDConnection(ConnectionObject).GetTableNames( (EmptyStr,EmptyStr,EmptyStr,aTablesList,[osMy],[tkTable]);
end;

function TDePGDatabase.SupportDDLs: TSupportDDLs;
begin
  if True {DatabaseType = dtPostgreSQL} then
    Result := [ddlCreateTable, ddlCreateField, ddlModifyField, ddlDeleteField, ddlSequence]
  else
    Result := inherited SupportDDLs;
end;

function TDePGDatabase.Sequence(const aType: TDeSequenceType; const aTableName, aFieldName: String): Boolean;
{$IFDEF DEBUG}
const
  SequenceTypes: array[TDeSequenceType] of PChar = ('stInsert', 'stUpdate', 'stDelete', 'stHaving');
{$ENDIF}
var Q: TFDQuery;
    aValue: Int64;
    sSequenceName: String;
begin
  Result:= False;
  if Length(aFieldName) = 0 then sSequenceName:= aTableName
                            else sSequenceName:= aTableName + '_' + aFieldName + '_seq';
  try
    Q:= TFDQuery.Create(Self);
    Q.Connection := TFDConnection(ConnectionObject);
    case aType of
      stInsert:
        try
          Q.SQL.Text:= 'SELECT COALESCE(max("' + aFieldName + '"), 0) + 1 FROM "' + aTableName + '"';
          WriteQueryLog(Q);
          Q.Open;
          aValue:= Q.Fields[0].Value;
          Q.Close;

          Q.ExecSQL('CREATE sequence "' + sSequenceName + '" RESTART WITH ' + IntToStr(aValue) + ' INCREMENT 1');
          Result:= True;
        except
          {$IFDEF DEBUG}
          on E: Exception do
            DebugLog('%s.Sequence(%s, %s, %s) skip error: %s', [StrPas(SequenceTypes[aType]), QuotedStr(aTableName), QuotedStr(aFieldName), E.Message]);
          {$ENDIF}
        end;
      stUpdate:
        try
          Q.SQL.Text:= 'SELECT COALESCE(max("' + aFieldName + '"), 0) + 1 FROM "' + aTableName + '"';
          WriteQueryLog(Q);
          Q.Open;
          aValue:= Q.Fields[0].Value;
          Q.Close;

          Q.ExecSQL('ALTER sequence "' + sSequenceName + '" RESTART WITH ' + IntToStr(aValue) + ' INCREMENT 1');
          Result:= True;
        except
          {$IFDEF DEBUG}
          on E: Exception do
            DebugLog('%s.Sequence(%s, %s, %s) skip error: %s', [StrPas(SequenceTypes[aType]), QuotedStr(aTableName), QuotedStr(aFieldName), E.Message]);
          {$ENDIF}
        end;
      stDelete:
        try
          Q.ExecSQL('DROP sequence "' + sSequenceName + '"');
          Result:= True;
        except
          {$IFDEF DEBUG}
          on E: Exception do
            DebugLog('%s.Sequence(%s, %s, %s) skip error: %s', [StrPas(SequenceTypes[aType]), QuotedStr(aTableName), QuotedStr(aFieldName), E.Message]);
          {$ENDIF}
        end;
      stHaving:
        try
//          Q.SQL.Text:= 'SELECT last_value FROM "' + sSequenceName + '"';
          Q.SQL.Text:= 'SELECT count(*) FROM pg_class WHERE relkind = ''S'' AND "relname" = ''' + sSequenceName + '''';
          WriteQueryLog(Q);
          Q.Open;
          Result:= (Q.RecordCount = 1);
          Q.Close;
        except
          {$IFDEF DEBUG}
          on E: Exception do
            DebugLog('%s.Sequence(%s, %s, %s) skip error: %s', [StrPas(SequenceTypes[aType]), QuotedStr(aTableName), QuotedStr(aFieldName), E.Message]);
          {$ENDIF}
        end;
    end;
  finally
    Q.Free;
  end;
end;

function TDePGDatabase.Autoincrement(const aType: TDeSequenceType; const aTableName, aFieldName: String): Boolean;
{$IFDEF DEBUG}
const
  SequenceTypes: array[TDeSequenceType] of PChar = ('stInsert', 'stUpdate', 'stDelete', 'stHaving');
{$ENDIF}
var Q: TFDQuery;
    aValue: Int64;
begin
  try
    Q:= TFDQuery.Create(Self);
    Q.Connection := TFDConnection(ConnectionObject);

    case aType of
      stInsert:
        try
          { TODO -oKruglov -cBug? : Не знаю как в постгресе, но в MSSQL если в таблице нет хотя бы одной записи - будет NULL даже при COALESCE! }
          Q.SQL.Text:= 'SELECT COALESCE(max("' + aFieldName + '"), 0) + 1 FROM "' + aTableName + '"';
          WriteQueryLog(Q);
          Q.Open;
          aValue:= Q.Fields[0].Value;
          Q.Close;

          Q.ExecSQL('ALTER TABLE "' + aTableName + '" ALTER COLUMN "' + aFieldName + '" ' +
                    'ADD GENERATED BY DEFAULT AS IDENTITY ( INCREMENT 1 START ' + IntToStr(aValue) + ' ');
          Result:= True;
        except
          {$IFDEF DEBUG}
          on E: Exception do
            DebugLog('%s.Autoincrement(%s, %s, %s) skip error: %s', [StrPas(SequenceTypes[aType]), QuotedStr(aTableName), QuotedStr(aFieldName), E.Message]);
          {$ENDIF}
        end;
      stUpdate:
        try
          Q.SQL.Text:= 'SELECT COALESCE(max("' + aFieldName + '"), 0) + 1 FROM "' + aTableName + '"';
          WriteQueryLog(Q);
          Q.Open;
          aValue:= Q.Fields[0].Value;
          Q.Close;

          Q.ExecSQL('ALTER TABLE "' + aTableName + '" ALTER COLUMN "' + aFieldName + '" ' +
                    'SET START ' + IntToStr(aValue) );
          Result:= True;
        except
          {$IFDEF DEBUG}
          on E: Exception do
            DebugLog('%s.Autoincrement(%s, %s, %s) skip error: %s', [StrPas(SequenceTypes[aType]), QuotedStr(aTableName), QuotedStr(aFieldName), E.Message]);
          {$ENDIF}
        end;
      stDelete:
        try
          Q.ExecSQL('ALTER TABLE "' + aTableName + '" ALTER COLUMN "' + aFieldName + '" ' +
                    'DROP IDENTITY');
          Result:= True;
        except
          {$IFDEF DEBUG}
          on E: Exception do
            DebugLog('%s.Autoincrement(%s, %s, %s) skip error: %s', [StrPas(SequenceTypes[aType]), QuotedStr(aTableName), QuotedStr(aFieldName), E.Message]);
          {$ENDIF}
        end;
      stHaving:
        try
        //
        except
          {$IFDEF DEBUG}
          on E: Exception do
            DebugLog('%s.Autoincrement(%s, %s, %s) skip error: %s', [StrPas(SequenceTypes[aType]), QuotedStr(aTableName), QuotedStr(aFieldName), E.Message]);
          {$ENDIF}
        end;
    end;
  finally
    Q.Free;
  end;
end;

procedure TDePGDatabase.RetrieveMetaTables(aTablesList : TObjectList);
var Q   : TFDQuery;
    TM  : TTableMeta;
begin
  if not Assigned(ConnectionObject) then Exit;

  Q:= TFDQuery.Create(Self);
  try
    Q.Connection := TFDConnection(ConnectionObject);
    Q.SQL.Text:=
      'SELECT table_name, table_type, table_schema '+
      'FROM information_schema.tables '+
      'WHERE table_schema NOT IN (''information_schema'',''pg_catalog'');';
    WriteQueryLog(Q);
    Q.Open;
    while Not Q.Eof do
      begin
        TM:=TTableMeta.Create(Trim(Q.FieldValues['TABLE_NAME']), self) ;
        TM.IsReadOnly  := Trim(Q.FieldValues['TABLE_TYPE'])='VIEW';
        TM.Schema      := Trim(Q.FieldValues['TABLE_SCHEMA']);
        TM.Description := EmptyStr;
        TM.Name        := '_dF.table '+TM.Table;
        aTablesList.Add(TM);
        Q.Next;
      end;
    Q.Close;
  finally
  end;
  Q.Free;
end;

procedure TDePGDatabase.RetrieveMetaTableInfo(aTable : TObject);
var FL    : TFieldsMeta;
    Q     : TFDQuery;
    FM    : TFieldMeta;
    p     : Integer;
    sType : String;
begin
  Assert( Assigned(aTable), 'RetrieveMetaTableInfo');
  Assert( aTable is TTableMeta, 'RetrieveMetaTableInfo');
  Assert( Assigned(FConnectionObject), ETableCantOpenError);

  if Not CheckConnection then Exit;

  Q:=TFDQuery.Create(self);
  try
    Q.Connection := TFDConnection(ConnectionObject);
    FL:=TFieldsMeta.create;

    // Читаем поля .............................................................
    Q.SQL.Text:=
      ' select C.COLUMN_NAME, C.DATA_TYPE, C.IS_NULLABLE, C.IS_IDENTITY, C.IS_UPDATABLE,'+
      '   case when ccu.column_name is null then ''NO'' else ''YES'' end as IS_PRIMARYKEY,'+
      '   C.CHARACTER_MAXIMUM_LENGTH, C.NUMERIC_PRECISION, C.NUMERIC_SCALE, C.COLUMN_DEFAULT, C.ORDINAL_POSITION'+
      ' from INFORMATION_SCHEMA.columns c'+
      ' FULL JOIN information_schema.table_constraints tc ON'+
      '   tc.constraint_schema = c.table_schema AND tc.table_catalog = c.table_catalog AND'+
      '   tc.table_name = c.table_name AND tc.constraint_type = ''PRIMARY KEY'''+
      ' LEFT JOIN information_schema.constraint_column_usage AS ccu ON'+
      '   ccu.constraint_schema = tc.constraint_schema AND ccu.constraint_name = tc.constraint_name AND'+
      '   ccu.column_name = c.column_name'+
      ' where c.TABLE_NAME = :TABLENAME and c.table_catalog = :BASENAME'+
      ' order by ORDINAL_POSITION';

    Q.ParamByName('TABLENAME').AsString:= TTableMeta(aTable).Table;
    Q.ParamByName('BASENAME').AsString:= GetDatabase;
    Q.Prepared:= True;
    WriteQueryLog(Q);
    Q.Open;
    while Not Q.Eof do
      begin
        FM:= TFieldMeta.Create;
        FM.Original:= Trim(Q.FieldValues['COLUMN_NAME']);
        FM.Description := EmptyStr;
        FM.Name:= FM.Original;
          p:=Pos('_', FM.Name); if p>0 then FM.Name:= Copy(FM.Name, p, MaxInt);
        FM.Key:= (Q.FieldValues['IS_PRIMARYKEY']='YES');
        FM.NotNull:= (Q.FieldValues['IS_NULLABLE']='NO');
        FM.ReadOnly:= (Q.FieldValues['IS_UPDATABLE']='NO');
        FM.Order := Q.FieldValues['ORDINAL_POSITION'];
        FM.IsStored:= True;
        FM.Owner:= TTableMeta(aTable);

        sType:= Q.FieldValues['DATA_TYPE'];
        p:=Pos('_', sType); if p>0 then sType:=Copy(sType, p, MaxInt);

        if SameText(sType, 'oid')               then begin FM.DataType:= ftLongWord; end else
        if SameText(sType, 'name')              then begin FM.DataType:= ftWideString; FM.DataSize:= 64; end else

        if SameText(sType, 'boolean')           then FM.DataType:= ftBoolean else
        if SameText(sType, 'uuid')              then begin FM.DataType:= ftGUID; FM.DataSize:= 38; end else
        if SameText(sType, 'anyarray')          then FM.DataType:= ftArray else

        if SameText(sType, 'tunyint')           then FM.DataType:= ftByte else
        if SameText(sType, 'smallint')          then FM.DataType:= ftSmallint else
        if SameText(sType, 'integer')           then FM.DataType:= ftInteger else
        if SameText(sType, 'bigint')            then FM.DataType:= ftLargeint else

        if SameText(sType, 'numeric')           then FM.DataType:= ftFMTBcd else
        if SameText(sType, 'real')              then FM.DataType:=ftSingle  else
        if SameText(sType, 'double precision')  then FM.DataType:=ftFloat else
        if SameText(sType, 'money')             then FM.DataType:=ftCurrency else

        if SameText(sType, 'character')         then FM.DataType:= ftFixedWideChar else
        if SameText(sType, 'character varying') then FM.DataType:= ftWideString else
        if SameText(sType, 'text')              then begin FM.DataType:= ftWideMemo; FM.DataSize:= 0; end else

        if SameText(sType, 'bytea')             then FM.DataType:= ftBlob else

        if SameText(sType, 'date')              then FM.DataType:= ftDate else
        if 1 = pos('timestamp', sType)          then begin
                                                       if 0 < pos('with time zone', sType)
                                                         then FM.DataType:= ftTimeStampOffset
                                                         else FM.DataType:= ftTimeStamp;
                                                     end else
        if 1 = pos('time', sType)               then FM.DataType:= ftTime else

                                                     FM.DataType:=ftUnknown;

        if FM.DataType in [ftString, ftWideString] then
          begin
            FM.DataSize:= Q.FieldValues['CHARACTER_MAXIMUM_LENGTH'];
          end;

        if FM.DataType in [ftMemo, ftWideMemo] then
          begin
            FM.DataSize:= 0;
          end;

        if FM.DataType in [ftBCD, ftFMTBcd] then
          begin
            FM.DataSize:= Q.FieldValues['NUMERIC_SCALE'];
          end;

        FM.DefaultDefDB:= Q.FieldValues['COLUMN_DEFAULT'];

        // проверяем наличие связанного счетчика для целочисленных полей notNull - вероятно это ключи
        {
        if (FM.DataType in [ftSmallint, ftInteger, ftLargeint]) and FM.NotNull then
          try
            Q2.SQL.Text := 'SELECT pg_get_serial_sequence('''+TTableMeta(aTable).Table+''', '''+FM.Original+''')';
            Q2.Open;
            if 0 < Q2.RecordCount then
              begin
                FM.ReadOnly:= True;
                FM.Key:= True;
              end;
            Q2.Close;
          except
          end;
       {}
        FL.Add(FM);

        Q.Next;
      end;
    Q.Close;

    TTableMeta(aTable).AssignFields(FL);
  finally
    Q.Free;
  end;
end;

function TDePGDatabase.FieldExists(const TableName, FieldName: string): Boolean;
var Q: TFDQuery;
begin
  if DatabaseType = dtPostgreSQL then
    try
      Q:= TFDQuery.Create(Self);
      Q.Connection:= TFDConnection(ConnectionObject);
      Q.SQL.Text:=
        ' select C.DATA_TYPE'+
        ' from INFORMATION_SCHEMA.columns c'+
        ' where C.COLUMN_NAME = :FIELDNAME and c.TABLE_NAME = :TABLENAME and c.table_catalog = :BASENAME';

      Q.ParamByName('FIELDNAME').AsString:= FieldName;
      Q.ParamByName('TABLENAME').AsString:= TableName;
      Q.ParamByName('BASENAME').AsString:= Database;
      Q.Prepared:= True;
      WriteQueryLog(Q);
      Q.Open;

      Result := (Q.RecordCount = 1);
    finally
      Q.Free;
    end
  else
    Result := inherited FieldExists(TableName, FieldName); // Здесь можно реализовать логику для других БД ...
end;

function TDePGDatabase.FieldMetaToDDL(Source: TObject; const KeyEnabled: Boolean): string;
var
  FieldMeta: TFieldMeta;
  FieldsMeta: TFieldsMeta;
  FieldIndex: Integer;
  SequenceValue: Int64;
  TrimDefaultValue: String;

  function PrepareTypeSize(const TypeName: string; const MaxSize: Integer = 8000): string;
  begin
    Result := TypeName + '(' + iif(FieldMeta.DataSize <= MaxSize, IntToStr(FieldMeta.DataSize), 'max') + ')';
  end;
begin
  Result := EmptyStr;
  if Assigned(Source) then
    if Source is TFieldMeta then
      begin
        FieldMeta := Source as TFieldMeta;
        if FieldMeta.IsStored then
          begin
            Result := FieldToStr(FieldMeta.Original) + ' ';
            case FieldMeta.DataType of
              ftFixedChar,
              ftFixedWideChar: Result := Result + PrepareTypeSize('character');
              ftString,
              ftWideString:    Result := Result + PrepareTypeSize('character varying');
              ftMemo,
              ftWideMemo:      Result := Result + 'text';
              ftBoolean:       Result := Result + 'boolean';
              ftSmallInt:      Result := Result + 'smallint';
              ftShortInt:      Result := Result + 'smallint';
              ftInteger:       Result := Result + 'integer';
              ftLargeInt:      Result := Result + 'bigint';
              ftByte:          Result := Result + 'smallint';
              ftWord:          Result := Result + 'integer';
              ftLongWord:      Result := Result + 'bigint';
              ftAutoInc:       case FieldMeta.DataSize of
                               1,2: Result := Result + 'smallint';
                                 8: Result := Result + 'bigint';
                                 else Result := Result + 'integer';
                               end;
              ftDate:          Result := Result + 'date';
              ftTime:          Result := Result + 'time';
              ftDateTime,
              ftTimeStamp:     Result := Result + 'timestamp';
              ftTimeStampOffset:Result:= Result + 'timestamp with time zone';

              ftFloat:         Result := Result + 'double precision';
              ftSingle:        Result := Result + 'real';
              ftCurrency:      Result := Result + 'money';
              ftBCD:           if FieldMeta.DataSize = 4 then Result := Result + 'money'
                                                         else Result := Result + 'double';
              ftGUID:          Result := Result + 'uuid';
              ftBlob:          Result := Result + 'bytea';
              ftBytes:         Result := Result + 'bytea';
              ftVarBytes:      Result := Result + 'bytea';
              else             Result := Result + '???';
            end;

            if KeyEnabled and ((FieldMeta.Key) or (FieldMeta.DataType = ftAutoInc)) then
              begin
                if (FieldMeta.Key) then
                  Result := Result + ' PRIMARY KEY';

                if FieldMeta.DataType = ftAutoInc then
                  begin
                    Result := Result + ' GENERATED BY DEFAULT AS IDENTITY (INCREMENT 1 START 1)';
                  end
                else
                  if Sequence(stHaving, FieldMeta.Owner.Table, FieldMeta.Original) then
                    if SequenceValue = 1 then
                      Result := Result + ' DEFAULT nextval(''"'+FieldMeta.Owner.Table+'_'+FieldMeta.Original+'_seq"''::regclass)';
              end
            else
              begin
              if FieldMeta.Unique then Result := Result + ' UNIQUE';
              if FieldMeta.NotNull then Result := Result + ' NOT NULL';

              if 0 < Length(FieldMeta.DefaultDefDB) then
                begin
                  TrimDefaultValue:= TrimFormula(FieldMeta.DefaultDefDB);

                  if FieldMeta.DataType in StringTypes then  Result := Result + ' DEFAULT ' + TrimDefaultValue else
                  if FieldMeta.DataType in IntegerTypes then Result := Result + ' DEFAULT ' + TrimDefaultValue else
                  if FieldMeta.DataType in FloatTypes then Result := Result + ' DEFAULT ' + TrimDefaultValue else
                  if FieldMeta.DataType in LogicalTypes then
                    begin
                     if (TrimDefaultValue = '0') or SameText(TrimDefaultValue, FalseLexeme) then
                       Result := Result + ' DEFAULT ' + FalseLexeme else
                     if (TrimDefaultValue = '1') or SameText(TrimDefaultValue, TrueLexeme) then
                       Result := Result + ' DEFAULT ' + TrueLexeme else
                       Result := Result + ' DEFAULT ' + TrimDefaultValue +'::boolean'
                    end else
                  if (FieldMeta.DataType in [ftDate]) and SameText(TrimDefaultValue, 'getdate()')
                                                                    then Result := Result + ' DEFAULT current_time' else
                  if (FieldMeta.DataType in [ftTime]) and SameText(TrimDefaultValue, 'getdate()')
                                                                    then Result := Result + ' DEFAULT current_date' else
                  if (FieldMeta.DataType in DateTypes) and SameText(TrimDefaultValue, 'getdate()')
                                                                    then Result := Result + ' DEFAULT current_timestamp' else
                  DeLog.WriteLog('Field ' + FieldMeta.Original+ ', default value skipped: "' + FieldMeta.DefaultDefDB + '"');
                        // FieldMeta.DefaultValue.DeQuotedString('''').QuotedString(''''); // Ковычки могут быть разные
                end;

              end;
          end;
      end
    else if Source is TFieldsMeta then
      begin
        FieldsMeta := Source as TFieldsMeta;
        for FieldIndex := 0 to Pred(FieldsMeta.Count) do
          if FieldsMeta[FieldIndex].IsStored then
            begin
              if Length(Result) <> 0 then
                Result := Result + ', ';
              Result := Result + FieldMetaToDDL(FieldsMeta[FieldIndex], KeyEnabled);
            end;
      end;
end;

procedure TDePGDatabase.TargetToSourceFieldType(var aFieldData: TFieldData);
begin
  if (aFieldData.TargetType = ftString) then
    begin
      aFieldData.SourceType:= ftWideString;
      aFieldData.SourceSize:= 0;
    end else

  if (aFieldData.TargetType = ftMemo) then
    begin
      aFieldData.SourceType:= ftWideMemo;
      aFieldData.SourceSize:= 0;
    end else

  if (aFieldData.TargetType = ftDateTime) then
    begin
      aFieldData.SourceType:= ftTimeStamp;
      aFieldData.SourceSize:= 0;
    end else

  if (aFieldData.TargetType = ftBCD) and (aFieldData.TargetSize = 4) then
    begin
      aFieldData.SourceType:= ftCurrency;
      aFieldData.SourceSize:= aFieldData.TargetSize;
    end else

  inherited TargetToSourceFieldType(aFieldData);
end;

function TDePGDatabase.IsFieldTypeConvert(const LocalType: TFieldType; const LocalSize: Integer;
                                          var RealType: TFieldType; var RealSize: Integer): Boolean;
begin
{
  if LocalType = ftBytes then
    begin
      RealType := ftBlob;
      RealSize := LocalSize;
      Result := True;
    end else {}

  if LocalType = ftString then
    begin
      RealType := ftWideString;
      RealSize := LocalSize;
      Result := True;
    end else
  if LocalType = ftMemo then
    begin
      RealType := ftWideMemo;
      RealSize := 0;
      Result := True;
    end else
  if LocalType = ftByte then
    begin
      RealType := ftShortInt;
      RealSize := 0;
      Result := True;
    end else
  if LocalType = ftWord then
    begin
      RealType := ftInteger;
      RealSize := 0;
      Result := True;
    end else
  if LocalType = ftDateTime then
    begin
      RealType := ftTimeStamp;
      RealSize := 0;
      Result := True;
    end else
  if (LocalType = ftBCD) and (LocalSize = 4) then
    begin
      RealType := ftCurrency;
      RealSize := 0;
      Result := True;
    end else
    begin
      Result := inherited IsFieldTypeConvert(LocalType, LocalSize, RealType, RealSize);
    end;
end;

function TDePGDatabase.CreateTable(Source: TObject): Boolean;
var TM: TTableMeta;
    Q : TFDQuery;
begin
  try
    Result := Assigned(Source) and (Source is TTableMeta);
    if Result then
      begin
        TM := TTableMeta(Source);
        Q := TFDQuery.Create(Self);
        try
          Q.Connection := TFDConnection(ConnectionObject);
          Q.SQL.Text := 'CREATE TABLE ' + TableToStr(TM.Table) + ' (' + FieldMetaToDDL(TM.Fields, True) + ')';
          WriteQueryLog(Q);
          Q.ExecSQL;
        finally
          Q.Free;
        end;
      end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog(ClassName + '.CreateTable skip error: ' + E.Message);
        {$ENDIF}
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Create table error: ' + E.Message);
        {$ENDIF}
        Result := False;
      end;
  end
end;

function TDePGDatabase.CreateField(Source: TObject): Boolean;
var
  TableMeta: TTableMeta;
  Query: TFDQuery;
begin
  try
    Result := Assigned(Source) and ((Source is TFieldMeta) or (Source is TFieldsMeta));
    if Result then
      begin
        if Source is TFieldMeta then TableMeta:= TFieldMeta(Source).Owner else
        if Source is TFieldsMeta then
          if 0 < TFieldsMeta(Source).Count then TableMeta:= TFieldsMeta(Source)[0].Owner;

        if Assigned(TableMeta) then
          begin
            Query := TFDQuery.Create(Self);
            try
              Query.Connection := TFDConnection(ConnectionObject);
              Query.SQL.Text := 'ALTER TABLE [' + TableToStr(TableMeta.Table) + '] ADD COLUMN ' + FieldMetaToDDL(Source, False);
              WriteQueryLog(Query);
              Query.ExecSQL;
            finally
              Query.Free;
            end;
          end;
      end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog(ClassName + '.CreateField skip error: ' + E.Message);
        {$ENDIF}
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Create field error: ' + E.Message);
        {$ENDIF}
        Result := False;
      end;
  end
end;

function TDePGDatabase.ModifyField(Source: TObject): Boolean;
var
  FieldMeta: TFieldMeta;
  FieldsMeta: TFieldsMeta;
  Query: TFDQuery;
begin
  try
    Result := Assigned(Source) and (DatabaseType = dtMSSQL);
    if Result then
      if Source is TFieldMeta then
        begin
          FieldMeta := Source as TFieldMeta;
          Query := TFDQuery.Create(Self);
          try
            Query.Connection := TFDConnection(ConnectionObject);
            Query.SQL.Text := 'ALTER TABLE [' + FieldMeta.Owner.Table + '] ALTER COLUMN ' + FieldMetaToDDL(FieldMeta, False);
            try
              Query.ExecSQL;
            finally
              WriteQueryLog(Query);
            end;
          finally
            Query.Free;
          end;
        end
      else if Source is TFieldsMeta then
        begin
          FieldsMeta := Source as TFieldsMeta;
          Query := TFDQuery.Create(Self);
          try
            Query.Connection := TFDConnection(ConnectionObject);
            Query.SQL.Text := 'ALTER TABLE [' + FieldsMeta[0].Owner.Table + '] ALTER COLUMN ' + FieldMetaToDDL(FieldsMeta, False);
            try
              Query.ExecSQL;
            finally
              WriteQueryLog(Query);
            end;
          finally
            Query.Free;
          end;
        end
      else
        Result := False;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog(ClassName + '.ModifyField skip error: ' + E.Message);
        {$ENDIF}
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Create field error: ' + E.Message);
        {$ENDIF}
        Result := False;
      end;
  end
end;

function TDePGDatabase.DeleteField(Source: TObject): Boolean;
var
  FieldMeta: TFieldMeta;
  Query: TFDQuery;
begin
  try
    Result := Assigned(Source);
    if Result then
      if Source is TFieldMeta then
        begin
          FieldMeta := Source as TFieldMeta;
          Query := TFDQuery.Create(Self);
          try
            Query.Connection := TFDConnection(ConnectionObject);
            Query.SQL.Text := 'ALTER TABLE [' + FieldMeta.Owner.Table + '] DROP COLUMN [' + FieldMeta.Original + ']';
            try
              Query.ExecSQL;
            finally
              WriteQueryLog(Query);
            end;
          finally
            Query.Free;
          end;
        end
      else
        Result := False;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog(ClassName + '.DeleteField skip error: ' + E.Message);
        {$ENDIF}
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Delete field error: ' + E.Message);
        {$ENDIF}
        Result := False;
      end;
  end
end;

{ TDeDBCOXMLDatabase }

constructor TDBCOXMLDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DatabaseType := dtDBCOXML;
  FMode:= xNotConnected;
end;

destructor TDBCOXMLDatabase.Destroy;
begin
  Connected:= False;
  inherited Destroy;
end;

function TDBCOXMLDatabase.GetReadOnly: boolean;
begin
  case FMode of
    xNotConnected: Result:= True;
    xModeFile:     Result:= False;
    xModeDir:      Result:= False;
    xModeZIP:      Result:= True;
    xModeResource: Result:= True;
  end;
end;

function TDBCOXMLDatabase.isInternalDataBase: Boolean;
begin
  Result:= (FMode = xModeResource);
end;

function TDBCOXMLDatabase.GetConnected: Boolean;
begin
  Result:= (Assigned(FConnectionObject) and (FConnectionObject.Mode <> zmClosed)) or (0 < Length(FConnectionDirectory));
end;

procedure TDBCOXMLDatabase.SetConnected(const Value: Boolean);
var i, N, ErrorCode: Integer;
    FDataStream: TStream;
    SearchRec: TSearchRec;
begin
  if Connected = Value then Exit;

  // DISCONNECT, удалем соединение
  if not Value then
    begin
      if Assigned(FConnectionObject) then
        FConnectionObject.Free;
      FConnectionDirectory:= EmptyStr;

      for i:=Low(FXMLDocuments) to High(FXMLDocuments) do
        FXMLDocuments[i].Free;

      setLength(FXMLDocuments, 0);

      FMode:= xNotConnected;
      Exit;
    end;

  // Базы, хранимые в РЕСУРСАХ EXE файла
  if (Server = InternalServerGUID) then
    begin
      FDataStream:= TResourceStream.Create(hInstance, Database, 'DB');
      try
        FConnectionObject:= TZipFile.Create;
        FConnectionObject.Open(FDataStream, zmRead);

        if (FConnectionObject.Mode <> zmClosed) then
          begin // все хорошо
            FMode:= xModeResource;
            SetLength(FXMLDocuments, FConnectionObject.FileCount);

            for i:= Low(FXMLDocuments) to High(FXMLDocuments) do
              begin
                FXMLDocuments[i]:= TXMLDocumentISO.Create(self);
                FXMLDocuments[i].ShemaName:= ChangeFileExt(FConnectionObject.FileName[i], EmptyStr);
              end;
          end else
          begin // архив не открылся/битый/нет прав
            FMode:= xNotConnected;
            FConnectionObject.Free;
            SetLength(FXMLDocuments, 0);
          end;
      except
        FMode:= xNotConnected;
        FreeAndNil(FDataStream);
        raise;
      end;
    end else

  // пользовательская база в АРХИВЕ на диске
  if FileExists(DataBase) and ( SameText(ExtractFileExt(DataBase), sExtensionZIP) or
                                SameText(ExtractFileExt(DataBase), sExtensionXMLX) ) then
    begin
      if Not IsReadOnly then
        IsReadOnly:= FileIsReadOnly(DataBase);

      try
        FConnectionObject:= TZipFile.Create;
        if IsReadOnly then FConnectionObject.Open(DataBase, zmRead)
                      else FConnectionObject.Open(DataBase, zmReadWrite);

        if (FConnectionObject.Mode <> zmClosed) then
          begin // все хорошо
            FMode:= xModeZIP;
            SetLength(FXMLDocuments, FConnectionObject.FileCount);

            for i:= Low(FXMLDocuments) to High(FXMLDocuments) do
              begin
                FXMLDocuments[i]:= TXMLDocumentISO.Create(self);
                FXMLDocuments[i].ShemaName:= ChangeFileExt(FConnectionObject.FileName[i], EmptyStr);
              end;
          end else
          begin // архив не открылся/битый/нет прав
            FMode:= xNotConnected;
            FConnectionObject.Free;
            SetLength(FXMLDocuments, 0);
          end;
      except
        raise;
      end;
    end else

  // пользовательская база В КАТАЛОГЕ на диске
  if DirectoryExists(ConnectString) then
    begin
      FConnectionDirectory:= IncludeTrailingPathDelimiter(DataBase) ;
      SetLength(FXMLDocuments, 0);

      try
        ErrorCode := FindFirst(FConnectionDirectory + '*' + sExtensionXML, faAnyFile, SearchRec);
        while ErrorCode = 0 do
          begin
            N:= Length(FXMLDocuments);
            SetLength(FXMLDocuments, Succ(N));
            FXMLDocuments[N]:= TXMLDocumentISO.Create(self);
            FXMLDocuments[N].ShemaName:= ChangeFileExt(SearchRec.Name, EmptyStr);
            ErrorCode := FindNext(SearchRec);
          end;
      finally
        FindClose(SearchRec);
      end;
      FMode:= xModeDir;
    end else

  if FileExists(DataBase) and SameText(ExtractFileExt(DataBase), sExtensionXML) then
    begin
      FConnectionDirectory:= IncludeTrailingPathDelimiter(ExtractFileDir(DataBase));
      SetLength(FXMLDocuments, 1);

      FXMLDocuments[0]:= TXMLDocumentISO.Create(self);
      FXMLDocuments[0].ShemaName:= ChangeFileExt(ExtractFileName(DataBase), EmptyStr);;
      FMode:= xModeFile;
    end else
  // Отсутствует подключение к базе
    begin
      raise Exception.Create('Отсутствует подключение к базе');
    end;
end;

procedure TDBCOXMLDatabase.RetrieveTableNames(aTablesList: TStringList);
var i: Integer;
begin
  aTablesList.Clear;
  for i:= Low(FXMLDocuments) to High(FXMLDocuments) do
    aTablesList.Add(FXMLDocuments[i].ShemaName);
end;

procedure TDBCOXMLDatabase.RetrieveMetaTables(aTablesList: TObjectList);
var TM : TTableMeta;
    TableName: string;
    i: Integer;
begin
  if not Connected then Exit;

  if (CheckConnection) then
    for i:= Low(FXMLDocuments) to High(FXMLDocuments) do
      begin
        TableName := FXMLDocuments[i].ShemaName;

        TM:= TTableMeta.Create(TableName, self);
        TM.ObjectType  := otTable;
        TM.SetTable    := TableName;
        TM.IsDynamic   := True;
        TM.IsReadOnly  := True;
        TM.Schema      := EmptyStr;
        TM.Description := EmptyStr;
        TM.Name        := '_dF.table '+TableName;
        aTablesList.Add(TM);
      end;
end;

function TDBCOXMLDatabase.GetXMLDocument(Index: Integer): TXMLDocumentISO;
var fZipHeader: TZipHeader;
    FStream: TStream;
begin
  if not FXMLDocuments[Index].Active then
    case FMode of
      xNotConnected: raise Exception.Create('XML database not connected');
      xModeFile, xModeDir:
        begin
          FXMLDocuments[Index].LoadFromFile(FConnectionDirectory + FXMLDocuments[Index].ShemaName + sExtensionXML);
          FXMLDocuments[Index].ReadHeader;
        end;
      xModeZIP, xModeResource:
        try
          FConnectionObject.Read(Index, FStream, fZipHeader);
          FStream.Position:= 0;
          FXMLDocuments[Index].LoadFromStream(FStream, xetUTF_8);
          FXMLDocuments[Index].ReadHeader;
        finally
          FStream.Free;
        end;
    end;

  Result:= FXMLDocuments[Index];
end;

procedure TDBCOXMLDatabase.RetrieveMetaTableInfo(aTable: TObject);
var i, M: Integer;
    FXMLTable: TXMLTable;
begin
  Assert( Assigned(aTable), 'RetrieveMetaTableInfo');
  Assert( aTable is TTableMeta, 'RetrieveMetaTableInfo');

  if Not CheckConnection then Exit;

  for i:= Low(FXMLDocuments) to High(FXMLDocuments) do
    if SameText(FXMLDocuments[i].ShemaName, TTableMeta(aTable).Table) then
      begin
        XMLDocuments[i].Active:= True;
        if XMLDocuments[i].ReadHeader
          then begin
                 M:= XMLDocuments[i].TableIndexByName(TTableMeta(aTable).Table);
                 if (-1 < M) then
                   begin
                    // TTableMeta(aTable).Assign(XMLDocuments[i].Items[M].TableMeta);
                     TTableMeta(aTable).Fields.AssignFields(XMLDocuments[i].Items[M].TableMeta.Fields);
                   end;
               end
          else Funcs.WriteLog('RetrieveMetaTableInfo Header');
      end;
end;

function TDBCOXMLDatabase.Get_XMLTable(const aTableName, aShemaName: String): TXMLTable;
var i, M: Integer;
begin
  Result:= nil;
  if not CheckConnection then Exit(nil);

  if (aShemaName = EmptyStr) then
    for i:= Low(FXMLDocuments) to High(FXMLDocuments) do
      if SameText(FXMLDocuments[i].ShemaName, aTableName) then
        if (-1 < i) then
          begin
            M:= XMLDocuments[i].TableIndexByName(aTableName);
            if (-1 < M) then
              Result:= XMLDocuments[i].Items[M];
          end;
end;

function TDBCOXMLDatabase.CreateQueryObject(const aQueryType: TDeQueryType; InDS: TDeDataset): TDeDataset;
begin
  Assert( aQueryType in [qtSelect, qtRow], 'XML Connector support only Select operation');

  if Not CheckConnection then Exit(nil);

  Result := TDBCOXMLDataset.Create(Self);
  TDBCOXMLDataset(Result).Database:= self;
end;

{ -------------------------------------------------------------------------------------------------------------------- }
{ TDeXMLDatabase }
{ -------------------------------------------------------------------------------------------------------------------- }

constructor TDeADOXMLDatabase.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  DatabaseType:= dtADOXML;
end;

destructor TDeADOXMLDatabase.Destroy;
begin
  SetLength(FFiles,0);
  inherited;
end;

function TDeADOXMLDatabase.GetConnected: boolean;
begin
  Result:= (0 < Length(FFiles));
end;

function TDeADOXMLDatabase.GetFileName(aIndex: Integer): String;
begin
  Result:= FDirectory + FFiles[aIndex] + sExtensionXML;
end;

procedure TDeADOXMLDatabase.SetConnected(const aConnected: boolean);
var SearchRec: TSearchRec;
    N, ErrorCode: Integer;
begin
  if GetConnected = aConnected then Exit;

  FDirectory:= EmptyStr;
  SetLength(FFiles, 0);

  if aConnected then
    begin
      if FileExists(Database) then  // только один XML файл
        begin
          FDirectory:= ExtractFilePath(Database);
          if 0 < Length(FDirectory) then FDirectory := IncludeTrailingPathDelimiter(FDirectory);

          if SameText(ExtractFileExt(Database), sExtensionXML) then
            begin
              N:= Length(FFiles);
              SetLength(FFiles, Succ(N));
              FFiles[N]:= ExtractFileName(Database);
            end;
        end else
      if DirectoryExists(Database) then  // все XML файлы из каталога
        begin
          FDirectory:= Database;
          if 0 < Length(FDirectory) then FDirectory := IncludeTrailingPathDelimiter(FDirectory);
          try
            ErrorCode := FindFirst(FDirectory + '*' + sExtensionXML, faAnyFile, SearchRec);
            while ErrorCode = 0 do
              begin
                N:= Length(FFiles);
                SetLength(FFiles, Succ(N));
                FFiles[N]:= ChangeFileExt(SearchRec.Name, EmptyStr);
                ErrorCode := FindNext(SearchRec);
              end;
          finally
            FindClose(SearchRec);
          end;
        end;
    end;
end;

function TDeADOXMLDatabase.IndexByName(aName: string): Integer;
var i: integer;
begin
  Result:= -1;
  for i:= 0 to Pred(Length(FFiles)) do
    if SameText(FFiles[i], aName) then Exit(i);
end;

procedure TDeADOXMLDatabase.RetrieveTableNames(aTablesList: TStringList);
var i: Integer;
begin
  aTablesList.Clear;
  for i:= Low(FFiles) to High(FFiles) do
    aTablesList.Add(FFiles[i]);
end;

procedure TDeADOXMLDatabase.RetrieveMetaTables(aTablesList: TObjectList);
var TM : TTableMeta;
    i: Integer;
begin
  if (CheckConnection) then
    for i:= Low(FFiles) to High(FFiles) do
      begin
        TM:= TTableMeta.Create(FFiles[i], self);
        TM.IsDynamic   := True;
        TM.IsReadOnly  := True;
        TM.Schema      := EmptyStr;
        TM.Description := EmptyStr;
        TM.Name        := '_dF.table ' + FFiles[i];
        aTablesList.Add(TM);
      end;
end;

procedure TDeADOXMLDatabase.RetrieveMetaTableInfo(aTable: TObject);
begin
  inherited;
  //
end;

function TDeADOXMLDatabase.CreateQueryObject(const aQueryType: TDeQueryType; InDS: TDeDataset = nil): TDeDataset;
begin
  if Connected then
    begin
      Result:= TDeADOXMLDataset.Create(Self);
      TDeADOXMLDataset(Result).Database:= Self;
    end
  else
    Result:= nil;
end;

{ -------------------------------------------------------------------------------------------------------------------- }
{ TDeADODatabase.TAsyncQueryThread }
{ -------------------------------------------------------------------------------------------------------------------- }

constructor TDeADODatabase.TAsyncQueryThread.Create(const AConnectionString: string);
begin
  inherited Create(True);
  FConnection := TADOConnection.Create(nil);
  FConnection.ConnectionString := AConnectionString;
  FConnection.LoginPrompt := False;
  FQuery := TADOQuery.Create(nil);
  FQuery.Connection := FConnection;
  FreeOnTerminate := True;
end;

destructor TDeADODatabase.TAsyncQueryThread.Destroy;
begin
  FQuery.Free;
  FConnection.Free;
  inherited Destroy;
end;

procedure TDeADODatabase.TAsyncQueryThread.AsyncExecute(const AQueryText: string; AOnCompleted: TDeAsyncExecuteEvent; AOnFailed: TDeAsyncExceptionEvent);
begin
  Assert(Suspended, 'Thread not suspended!');
  FOnExecute := AOnCompleted;
  FOnError := AOnFailed;
  FQuery.SQL.Text := AQueryText;
  FExecuting := True;
  Resume;
end;

procedure TDeADODatabase.TAsyncQueryThread.AsyncOpen(const AQueryText: string; AOnDataset: TDeAsyncDatasetEvent; AOnError: TDeAsyncExceptionEvent);
begin
  Assert(Suspended, 'Thread not suspended!');
  FOnDataset := AOnDataset;
  FOnError := AOnError;
  FQuery.SQL.Text := AQueryText;
  FExecuting := False;
  Resume;
end;

procedure TDeADODatabase.TAsyncQueryThread.Execute;
resourcestring
  sThreadException = 'Thread exception';
var
  RowIndex, ColIndex: Integer;
  RowValue: Variant;
begin
  try
    FConnection.Open;
    try
      while not Terminated do
        begin
          try
            if FExecuting then
              begin
                FReturnCode := FQuery.ExecSQL;
                if not Terminated then
                  Synchronize(NotifyReturnCode);
              end
            else
              begin
                FQuery.Open;
                try
                  if not Terminated then
                    begin
                      FRawValue := VarArrayCreate([0, Pred(FQuery.RecordCount)], varVariant);
                      RowIndex := VarArrayLowBound(FRawValue, 1);
                      while not FQuery.Eof do
                        begin
                          RowValue := VarArrayCreate([0, Pred(FQuery.Fields.Count)], varVariant);
                          for ColIndex := 0 to Pred(FQuery.Fields.Count) do
                            VarArrayPut(RowValue, FQuery.Fields[ColIndex].Value, [ColIndex]);
                          VarArrayPut(FRawValue, RowValue, [RowIndex]);
                          if Terminated then Break;
                          Inc(RowIndex);
                          FQuery.Next;
                        end;
                      RowValue := Unassigned;
                    end;
                finally
                  FQuery.Close;
                end;
                if not Terminated then
                  Synchronize(NotifyRawValue);
                FRawValue := Unassigned;
              end;
          except
            on E: Exception do
              begin
                FException := E;
                Synchronize(NotifyError);
              end;
          end;
          Suspend;
        end;
    finally
      FConnection.Close;
    end;
  except
    on E: Exception do
      begin
        FException := E;
        Synchronize(NotifyException);
      end;
  end;
end;

procedure TDeADODatabase.TAsyncQueryThread.NotifyReturnCode;
begin
  if Assigned(FOnExecute) then FOnExecute(Self, FReturnCode);
end;

procedure TDeADODatabase.TAsyncQueryThread.NotifyRawValue;
begin
  if Assigned(FOnDataset) then FOnDataset(Self, FRawValue);
end;

procedure TDeADODatabase.TAsyncQueryThread.NotifyError;
begin
  if Assigned(FOnError) then FOnError(Self, FException);
end;

procedure TDeADODatabase.TAsyncQueryThread.NotifyException;
resourcestring
  sThreadException = 'Thread exception';
  sNilThreadException = 'Unknown internal thread exception';
begin
  if Assigned(FOnException) then
    FOnException(Self, FException)
  else
    if Assigned(FException) then
      MessageBox(0, PChar(FException.Message), PChar(sThreadException), MB_OK or MB_ICONSTOP or MB_SYSTEMMODAL)
    else
      MessageBox(0, PChar(sNilThreadException), PChar(sThreadException), MB_OK or MB_ICONSTOP or MB_SYSTEMMODAL);
end;

{ TDeADODatabase.TAsyncThreadManager }

constructor TDeADODatabase.TAsyncThreadManager.Create(const AConnectionString: string; const AMaxThreadCount: Cardinal);
begin
  FThreads := TThreadList.Create;
  FConnectionString := AConnectionString;
  FMaxThreadCount := AMaxThreadCount;
end;

destructor TDeADODatabase.TAsyncThreadManager.Destroy;
var
  List: TList;
  Index: Integer;
begin
  if Assigned(FThreads) then
    begin
      List := FThreads.LockList;
      try
        for Index := Pred(List.Count) downto 0 do
          TThread(List).Terminate;
      finally
        FThreads.UnlockList;
      end;
      FreeAndNil(FThreads);
    end;
  inherited Destroy;
end;

function TDeADODatabase.TAsyncThreadManager.Execute(const QueryText: string; OnCompleted: TDeAsyncExecuteEvent; OnFailed: TDeAsyncExceptionEvent): Boolean;
var
  List: TList;
  Index: Integer;
  Thread: TAsyncQueryThread;
begin
  List := FThreads.LockList;
  try
    // Ищем "скучающую без дела" нитку ...
    Thread := nil;
    for Index := 0 to Pred(List.Count) do
      if TAsyncQueryThread(List[Index]).Suspended then
        begin
          Thread := TAsyncQueryThread(List[Index]);
          Break;
        end;
    // Если свободной не нашли, то ...
    if not Assigned(Thread) then
      if (FMaxThreadCount = 0) or (List.Count < FMaxThreadCount) then
        begin
          Thread := TAsyncQueryThread.Create(FConnectionString);
          if List.Add(Thread) <> -1 then
            begin
              Thread.OnTerminate := ThreadDone;
              Thread.OnException := FOnException;
            end
          else
            begin
              Thread.Free;
              Thread := nil;
            end;
        end;
    Result := Assigned(Thread);
    // Если нитка ждёт команды, то запускаем её ...
    if Result then
      Thread.AsyncExecute(QueryText, OnCompleted, OnFailed);
  finally
    FThreads.UnlockList;
  end;
end;

function TDeADODatabase.TAsyncThreadManager.Open(const QueryText: string; OnDataset: TDeAsyncDatasetEvent; OnError: TDeAsyncExceptionEvent): Boolean;
var
  List: TList;
  Index: Integer;
  Thread: TAsyncQueryThread;
begin
  List := FThreads.LockList;
  try
    // Ищем "скучающую без дела" нитку ...
    Thread := nil;
    for Index := 0 to Pred(List.Count) do
      if TAsyncQueryThread(List[Index]).Suspended then
        begin
          Thread := TAsyncQueryThread(List[Index]);
          Break;
        end;
    // Если свободной не нашли, то ...
    if not Assigned(Thread) then
      if (FMaxThreadCount = 0) or (List.Count < FMaxThreadCount) then
        begin
          Thread := TAsyncQueryThread.Create(FConnectionString);
          if List.Add(Thread) <> -1 then
            begin
              Thread.OnTerminate := ThreadDone;
              Thread.OnException := FOnException;
            end
          else
            begin
              Thread.Free;
              Thread := nil;
            end;
        end;
    Result := Assigned(Thread);
    // Если нитка ждёт команды, то запускаем её ...
    if Result then
      Thread.AsyncOpen(QueryText, OnDataset, OnError);
  finally
    FThreads.UnlockList;
  end;
end;

procedure TDeADODatabase.TAsyncThreadManager.ThreadDone(Sender: TObject);
var
  List: TList;
begin
  if Assigned(FThreads) then
    begin
      List := FThreads.LockList;
      try
        List.Remove(Sender);
      finally
        FThreads.UnlockList;
      end;
    end;
end;

{ TDeMySQLDatabase }

constructor TDeMySQLDatabase.Create(aOwner: TComponent);
const LibName = 'LIBMYSQL.dll';
var CurrentDir, VendorFile: string;
    Path: PChar;

  Function FindReсursive(StartDir: String; Mask: String = '*.*'): string;
  var SearchRec: TSearchRec;
  begin
    Result:= EmptyStr;
    if FileExists(StartDir+Mask) then Exit(StartDir+Mask);

    if FindFirst( StartDir+'*', faDirectory, SearchRec) = 0 then
      begin
        repeat
          Application.ProcessMessages;
          if (SearchRec.Attr and faDirectory) > 0 then
            if (SearchRec.Name <> '..') and (SearchRec.Name <> '.') then
              begin
                Result:= FindReсursive(StartDir+SearchRec.Name+'\', Mask);
                if Length(Result) > 0 then Exit;
              end;
        until FindNext(SearchRec) <> 0;
        FindClose(SearchRec);
      end;
  end;

begin
  if Not Assigned(DM.FMySQLDriverLink) then
    begin
      DM.FMySQLDriverLink:= TFDPhysMySQLDriverLink.Create(DM);
      with TFDPhysMySQLDriverLink(DM.FMySQLDriverLink) do
        begin
          // ищем в текущей папке или глубже
          CurrentDir:= ExtractFileDir(Application.ExeName)+'\';
          VendorFile:= FindReсursive(CurrentDir, LibName);

          // ищем на сайте dbco.ru
          if not SysUtils.FileExists(VendorFile) then
            if GetInetFile(urlDownload+LibName, CurrentDir+LibName) then
              VendorFile:= CurrentDir+LibName;

          // ищем в папке "program files"
          if not SysUtils.FileExists(VendorFile) then
            if SHGetFolderPath(0, CSIDL_PROGRAM_FILES, 0, 0, Path) = S_OK then
              begin
                VendorFile:= FindReсursive(StrPas(Path)+'\', LibName);
                if SysUtils.FileExists(VendorFile) then
                  if CopyFile(PChar(VendorFile), PChar(CurrentDir+LibName), False) then
                    VendorFile:= CurrentDir+LibName;
              end;

          if SysUtils.FileExists(VendorFile) then
            VendorLib:= VendorFile;
          Release;
        end;
    end;

  inherited Create(aOwner);
  FCanStoredProcedure:= True;
  DatabaseType:= dtMySQL;
end;

destructor TDeMySQLDatabase.Destroy;
begin

  inherited;
end;

function TDeMySQLDatabase.ExecuteStoredProcedure(const aProcedureName: string; aList: TDeVariableList): Integer;
var Q: TFDStoredProc;
    ItemIndex, VariableIndex: Integer;
    ErrorText, LineText, RaiseText: string;
    FileStream: TFileStream;
    BinData: OleVariant;
    DataPtr: Pointer;
    FullFileName, FileName: AnsiString;
    NameSize: SmallInt;
begin
  Assert( aList is TDeVariableList, 'aList not TDeVariableList');

  try
    Q:= TFDStoredProc.Create(self);
    try
    Q.Connection:= TFDConnection(ConnectionObject);
    Q.StoredProcName:= aProcedureName;
    Q.Prepared:= True;
{
    // 11.04.16 + Тайм-аут выполнения команды
    VariableIndex := aList.IndexByName(varActionTimeout);
    if VariableIndex <> -1 then
      Q.CommandTimeout := VarToInt(aList[VariableIndex].Value);
    // 11.04.16 -
    Q.Parameters.Refresh;

    // 11.04.16 + Тайм-аут выполнения команды
    VariableIndex := aList.IndexByName(varActionTimeout);
    if VariableIndex <> -1 then
      Q.CommandTimeout := VarToInt(aList[VariableIndex].Value);
    // 11.04.16 -
    Q.Parameters.Refresh;
{}
    for ItemIndex := 0 to Pred(Q.Params.Count) do
      with Q.Params[ItemIndex] do
        if not (Q.Params[ItemIndex].ParamType in [ptOutput, ptInputOutput]) then
          begin
            VariableIndex := aList.IndexByName(Name);
            if (VariableIndex = -1) and (Name[1] = '@') then
              VariableIndex := aList.IndexByName(Copy(Name, 2, MaxInt));

            if VariableIndex <> -1 then
              if DataType = ftVarBytes then
                begin
                  FullFileName:= aList[VariableIndex].Value;
                  if FileExists(FullFileName) then
                    begin
                      FileStream:= TFileStream.Create(FullFileName, fmOpenRead or fmShareDenyNone);
                      try
                        FileName:= ExtractFileName(FullFileName);
                        NameSize:= Length(FileName);

                        //TODO: Присохранении файлов мы ВСЕГДА в хвост добавляли один ненужный символ
                        //придется так и жить или менять сигнатуру для файлов сохраненных по правильному
                        BinData:= VarArrayCreate([0, DeSLength + SizeOf(Smallint) + NameSize + FileStream.Size {-1}], varByte);
                        DataPtr:= VarArrayLock(BinData);

                        Move(DeSignature[1], DataPtr^, DeSLength);
                        Move(NameSize, Ptr(cardinal(DataPtr) + DeSLength)^, SizeOf(Smallint));
                        Move(FileName[1], Ptr(cardinal(DataPtr) + DeSLength + SizeOf(Smallint))^, NameSize);
                        FileStream.ReadBuffer(Ptr(cardinal(DataPtr) + DeSLength + SizeOf(Smallint)  + NameSize)^, FileStream.Size);
                        Q.Params[ItemIndex].Value:= BinData;
                      finally
                        VarArrayUnlock(BinData);
                        FileStream.Free;
                      end;
                    end
                  else
                    begin
                      Q.Params[ItemIndex].Value:= unassigned;
                    end;
                end
              else
                begin
                  Value := aList[VariableIndex].Value;
                end;

            {
            else  //если параметр не задан и не определено NULL значение, то его и передаем NULL
            if not (paNullable in Q.Parameters[ItemIndex].Attributes) then
              Q.Parameters[ItemIndex].Value := null
            {}
        end;

    {$IFDEF DEBUG}
      aList.DebugVariablesLog(Format('%s: before execute stored procedure %s variables ...', [ClassName, QuotedStr(aProcedureName)]));
    {$ENDIF}

    WriteQueryLog(Q);
    Q.ExecProc;
    finally
      if aList is TDeVariableList then
        for ItemIndex := 0 to Pred(Q.Params.Count) do
          case Q.Params[ItemIndex].ParamType of
            ptOutput, ptInputOutput: { Стандартные параметры }
              begin
                VariableIndex := (aList as TDeVariableList).IndexByName(Q.Params[ItemIndex].Name);
                if (VariableIndex = -1) and (Q.Params[ItemIndex].Name[1] = '@') then
                  VariableIndex := (aList as TDeVariableList).IndexByName(Copy(Q.Params[ItemIndex].Name, 2, MaxInt));
                if VariableIndex <> -1 then
                  with (aList as TDeVariableList)[VariableIndex] do
                    begin
                      IsOutput:= True;
                      Value:= Q.Params[ItemIndex].Value;
                    end;
              end;
            ptResult: { Возможно результат работы хранимой процедуры }
              if SameText('@RETURN_VALUE', Q.Params[ItemIndex].Name) then
                begin
                  with (aList as TDeVariableList).GetByName(varActionResult) do
                    begin
                      IsOutput:= True;
                      Value:= Q.Params[ItemIndex].Value;
                    end;
                end;
          end;
      Q.Free;
    end;

    {$IFDEF DEBUG}
    if aList is TDeVariableList then
      (aList as TDeVariableList).DebugVariablesLog(Format('%s: after execute stored procedure %s variables ...', [ClassName, QuotedStr(aProcedureName)]));
    {$ENDIF}
    Result:=0;
  except
    on E: Exception do
      begin
        ErrorText := Trim(E.Message);
        RaiseText := EmptyStr;
        Result := 255;
        while Length(ErrorText) <> 0 do
          begin
            ItemIndex := Pos('||', ErrorText);
            if ItemIndex = 0 then
              begin
                LineText := ErrorText;
                ErrorText := EmptyStr;
              end
            else
              begin
                LineText := Trim(Copy(ErrorText, 1, Pred(ItemIndex)));
                ErrorText := Trim(Copy(ErrorText, ItemIndex + 2, Length(ErrorText)));
              end;
            if SameText(Copy(LineText, 1, 7), 'Script:') then
              begin
                Result := -1;
                MetaData.ExecuteScript(Copy(LineText, 8, MaxInt));
              end
            else if SameText(Copy(LineText, 1, 11), 'SendMessage') then
              begin
                Result:=-1;
                SendMessage(Application.MainForm.Handle, DM_EXTSENDMESSAGE, NativeInt(PChar(Copy(LineText, 12, MaxInt))), 0);
              end
            else
              begin
                if Length(RaiseText) <> 0 then RaiseText := RaiseText + '||';
                RaiseText := RaiseText + LineText;
              end;
          end;
        if Length(RaiseText) <> 0 then
          raise Exception.Create(RaiseText);
      end;
  end;
end;

function TDeMySQLDatabase.FieldToStr(const fieldName, fieldAlias, tableAlias: String;
  const fieldOperation: TOperationType; const DataType: TFieldType; const DataSize: Integer): string;
begin
  if (Length(fieldName) = 0) then
    Result:='*' else
  if (FieldName[1] in ['"','''','`']) then
    Result:= fieldName else
    Result:= '`' + fieldName + '`';

  if (Length(tableAlias) > 0) then
    Result := tableAlias + '.' + Result;

  if fieldOperation in AgregateOperations then
    Result := OperationLexeme[fieldOperation] + '(' + Result + ')';

  if (Length(fieldAlias) <> 0) and (fieldName<>fieldAlias) then
    Result := Result + ' as `' + fieldAlias + '`';
end;

function TDeMySQLDatabase.FuncToStr(aFunction: string; aParams: array of variant): string;
begin
  //
end;

function TDeMySQLDatabase.TableToStr(const tableName, tableAlias, tableDatabase, tableSchema: String): string;
begin
  Result:= '`' + tableName + '`';

  if Length(tableAlias)>0 then
    Result:= Result+' as '+tableAlias;

  if Length(tableDatabase)>0 then
    Result:= '`' + tableDatabase + '`.`' + iif(Length(tableSchema)>0, tableSchema, 'public') + '`.'  + Result else
  if Length(tableSchema)>0 then
    Result:= '`' + tableSchema + '`.' + Result
end;

procedure TDeMySQLDatabase.RetrieveTableNames(aTablesList: TStringList);
var Q   : TFDQuery;
begin
  if not Assigned(ConnectionObject) then Exit;

  Q:= TFDQuery.Create(Self);
  try
    Q.Connection := TFDConnection(ConnectionObject);
    Q.SQL.Text:=
      'SELECT table_name, table_type, table_schema '+
      'FROM information_schema.tables '+
      'WHERE table_schema = '''+ Database+'''' ;
    WriteQueryLog(Q);
    Q.Open;
    while Not Q.Eof do
      begin
        aTablesList.Add(Trim(Q.FieldValues['TABLE_NAME']));
        Q.Next;
      end;
    Q.Close;
  finally
  end;
  Q.Free;
end;

function TDeMySQLDatabase.StoredProcedureExists(const StoredProcedureName: string): Boolean;
var Q   : TFDQuery;
begin
  Q:= TFDQuery.Create(Self);
  try
    Q.Connection := TFDConnection(ConnectionObject);
    Q.SQL.Text:=
      'SELECT table_name, table_type, table_schema '+
      'FROM information_schema.ROUTINES '+
      'WHERE ROUTINE_NAME = '''+ StoredProcedureName+'''';
    WriteQueryLog(Q);
    Q.Open;
    Result:= (0 < Q.RecordCount);
    Q.Close;
  finally
  end;
  Q.Free;
end;

procedure TDeMySQLDatabase.RetrieveMetaTables(aTablesList: TObjectList);
var Q   : TFDQuery;
    TM  : TTableMeta;
begin
  if not Assigned(ConnectionObject) then Exit;

  Q:= TFDQuery.Create(Self);
  try
    Q.Connection := TFDConnection(ConnectionObject);
    Q.SQL.Text:=
      'SELECT table_name, table_type, table_schema '+
      'FROM information_schema.tables '+
      'WHERE table_schema = '''+ Database+'''';
    WriteQueryLog(Q);
    Q.Open;
    while Not Q.Eof do
      begin
        TM:=TTableMeta.Create(Trim(Q.FieldValues['TABLE_NAME']), self) ;
        TM.IsReadOnly  := not(Trim(Q.FieldValues['TABLE_TYPE'])='BASE TABLE');
        TM.Schema      := Trim(Q.FieldValues['TABLE_SCHEMA']);
        TM.Description := EmptyStr;
        TM.Name        := '_dF.table '+TM.Table;
        aTablesList.Add(TM);
        Q.Next;
      end;
    Q.Close;
  finally
  end;
  Q.Free;
end;

function TDeMySQLDatabase.ToFieldType(aValue: Variant): TFieldType;
begin
  Result:= ftUnknown;
  if not (VarIsType(aValue, VarString) or VarIsType(aValue, varUString)) then Exit;

  if SameText(aValue, 'date') then Result:= ftDate;

end;

function TDeMySQLDatabase.RetrieveProcedureParameters(const ProcedureName: string; ParameterList: TObject): Boolean;
var
  Query: TFDQuery;
  VariableItem: TVariableItem;
  function PrepareFieldType(const FieldType: TFieldType): string; inline;
  begin
    Result := IntToStr(Ord(FieldType));
  end;
begin
  Result := False;
  if (DatabaseType = dtMySQL) and Assigned(ParameterList) then
    if ParameterList is TStrings then
      begin
        TStrings(ParameterList).BeginUpdate;
        try
          TStrings(ParameterList).Clear;
          Query := TFDQuery.Create(Self);
          try
            Query.Connection:= TFDConnection(ConnectionObject);
            Query.SQL.Text := 'SELECT PARAMETER_NAME, DATA_TYPE, PARAMETER_MODE, ORDINAL_POSITION '+
                              'from INFORMATION_SCHEMA.PARAMETERS '+
                              'where SPECIFIC_NAME = '''+ProcedureName+'''';
            WriteQueryLog(Query);
            Query.Open;
            while not Query.Eof do
              begin
                TStrings(ParameterList).AddObject(Query.Fields[0].AsString, Pointer(ToFieldType(Query.Fields[1].AsString)));
                Query.Next;
              end;
          finally
            Query.Free;
          end;
          Result := True;
        finally
          TStrings(ParameterList).EndUpdate;
        end;
      end
end;

procedure TDeMySQLDatabase.RetrieveMetaTableInfo(aTable: TObject);
var FL    : TFieldsMeta;
    Q     : TFDQuery;
    FM    : TFieldMeta;
    p     : Integer;
    sType : String;
begin
  Assert( Assigned(aTable), 'RetrieveMetaTableInfo');
  Assert( aTable is TTableMeta, 'RetrieveMetaTableInfo');
  Assert( Assigned(FConnectionObject), ETableCantOpenError);

  if Not CheckConnection then Exit;

  Q:=TFDQuery.Create(self);
  try
    Q.Connection := TFDConnection(ConnectionObject);
    FL:=TFieldsMeta.create;

    // Читаем поля .............................................................
    Q.SQL.Text:=
      ' select  COLUMN_NAME, DATA_TYPE, IS_NULLABLE, CHARACTER_MAXIMUM_LENGTH, NUMERIC_PRECISION, DATETIME_PRECISION,'+
      ' NUMERIC_SCALE, COLUMN_KEY, COLLATION_NAME, COLUMN_TYPE, COLUMN_COMMENT, ORDINAL_POSITION, COLUMN_DEFAULT'+
      ' from INFORMATION_SCHEMA.columns'+
      ' where TABLE_NAME = :TABLENAME and TABLE_SCHEMA = :BASENAME'+
      ' order by ORDINAL_POSITION';

    Q.ParamByName('TABLENAME').AsString:= TTableMeta(aTable).Table;
    Q.ParamByName('BASENAME').AsString:= GetDatabase;
    Q.Prepared:= True;
    WriteQueryLog(Q);
    Q.Open;
    while Not Q.Eof do
      begin
        FM:= TFieldMeta.Create;
        FM.Original:= Trim(Q.FieldValues['COLUMN_NAME']);
        FM.Description := EmptyStr;
        FM.Name:= FM.Original;
          p:=Pos('_', FM.Name); if p>0 then FM.Name:= Copy(FM.Name, p, MaxInt);
        FM.Key:= (Q.FieldValues['COLUMN_KEY']='PRI');
        FM.NotNull:= (Q.FieldValues['IS_NULLABLE']='NO');
        FM.ReadOnly:= False; //(Q.FieldValues['IS_UPDATABLE']='NO');
        FM.Order := Q.FieldValues['ORDINAL_POSITION'];
        FM.IsStored:= True;
        FM.Owner:= TTableMeta(aTable);

        sType:= Q.FieldValues['DATA_TYPE'];
        p:=Pos('_', sType); if p>0 then sType:=Copy(sType, p, MaxInt);

        if SameText(sType, 'BOOL')              then begin FM.DataType:= ftBoolean;  FM.DataSize:= 1; end else
        if SameText(Q.FieldValues['COLUMN_TYPE'], 'TINYINT(1)')
                                                then begin FM.DataType:= ftBoolean;  FM.DataSize:= 1; end else

        if SameText(sType, 'TINYINT')           then begin FM.DataType:= ftShortint; FM.DataSize:= 1; end else
        if SameText(sType, 'TINYINT UNSIGNED')  then begin FM.DataType:= ftByte;     FM.DataSize:= 1; end else
        if SameText(sType, 'SMALLINT')          then begin FM.DataType:= ftSmallInt; FM.DataSize:= 2; end else
        if SameText(sType, 'SMALLINT UNSIGNED') then begin FM.DataType:= ftWord;     FM.DataSize:= 2; end else
        if SameText(sType, 'MEDIUMINT')         then begin FM.DataType:= ftInteger;  FM.DataSize:= 3; end else
        if SameText(sType, 'MEDIUMINT UNSIGNED')then begin FM.DataType:= ftLongWord; FM.DataSize:= 3; end else
        if SameText(sType, 'INT')               then begin FM.DataType:= ftInteger;  FM.DataSize:= 4; end else
        if SameText(sType, 'INT UNSIGNED')      then begin FM.DataType:= ftLongWord; FM.DataSize:= 4; end else
        if SameText(sType, 'BIGINT')            then begin FM.DataType:= ftLargeint; FM.DataSize:= 8; end else
        if SameText(sType, 'BIGINT UNSIGNED')   then begin FM.DataType:= ftLongWord; FM.DataSize:= 8; end else

        if SameText(sType, 'FLOAT')             then begin FM.DataType:=ftSingle;    FM.DataSize:= 4; end else
        if SameText(sType, 'DOUBLE') or SameText(sType, 'DOUBLE PRECISION') or SameText(sType, 'REAL')
                                                then begin FM.DataType:=ftFloat;     FM.DataSize:= 8; end else
        if SameText(sType, 'DECIMAL') or SameText(sType, 'NUMERIC') or SameText(sType, 'FIXED') or SameText(sType, 'DEC')
                                                then begin FM.DataType:= ftFMTBcd;
                                                           FM.DataSize:= Q.FieldValues['CHARACTER_MAXIMUM_LENGTH'];
                                                           FM.Precision:= Q.FieldValues['NUMERIC_PRECISION']; end else

        if SameText(sType, 'CHAR')              then begin FM.DataType:= ftFixedChar;
                                                           FM.DataSize:= Q.FieldValues['CHARACTER_MAXIMUM_LENGTH']; end else
        if SameText(sType, 'VARCHAR')           then begin FM.DataType:= ftString;
                                                           FM.DataSize:= Q.FieldValues['CHARACTER_MAXIMUM_LENGTH']; end else
        if SameText(sType, 'TINYTEXT')          then begin FM.DataType:= ftMemo; FM.DataSize:= Pred(256); end else
        if SameText(sType, 'TEXT')              then begin FM.DataType:= ftMemo; FM.DataSize:= Pred(256*256); end else
        if SameText(sType, 'MEDIUMTEXT')        then begin FM.DataType:= ftMemo; FM.DataSize:= Pred(256*256*256); end else
        if SameText(sType, 'LONGTEXT')          then begin FM.DataType:= ftMemo; FM.DataSize:= 0; end else

        if SameText(sType, 'TINYBLOB')          then begin FM.DataType:= ftBlob; FM.DataSize:= Pred(256); end  else
        if SameText(sType, 'BLOB')              then begin FM.DataType:= ftBlob; FM.DataSize:= Pred(256*256); end  else
        if SameText(sType, 'MEDIUMBLOB')        then begin FM.DataType:= ftBlob; FM.DataSize:= Pred(256*256*256); end  else
        if SameText(sType, 'LONGBLOB')          then begin FM.DataType:= ftBlob; FM.DataSize:= 0; end  else

        if SameText(sType, 'DATE')              then FM.DataType:= ftDate else
        if SameText(sType, 'DATETIME')          then FM.DataType:= ftDateTime else
        if SameText(sType, 'TIMESTAMP')         then FM.DataType:= ftTimeStamp else
        if SameText(sType, 'TIME')              then FM.DataType:= ftTime else

        if SameText(sType, 'YEAR')              then FM.DataType:= ftByte else
        if SameText(sType, 'JSON')              then begin FM.DataType:= ftStream; end else
        if SameText(sType, 'ENUM')              then begin FM.DataType:= ftReference; end else
        if SameText(sType, 'SET')               then begin FM.DataType:= ftArray; end else

                                                     FM.DataType:=ftUnknown;

        FM.DefaultDefDB:= Q.FieldValues['COLUMN_DEFAULT'];
        FL.Add(FM);
        Q.Next;
      end;
    Q.Close;

    TTableMeta(aTable).AssignFields(FL);
  finally
    Q.Free;
  end;
end;

function TDeMySQLDatabase.CreateConnectionObject: TCustomConnection;
var P: Integer;
begin
  result := TFDConnection.Create(Application);
  result.LoginPrompt := False;

  with TFDConnection(result) do
  begin
    DriverName:='MySQL';

    P:=Pos(':',Server);
    if 1 < P then
      begin
        Params.Values['Server']:= Copy(Server, 1, P-1);
        Params.Values['Port']:= Copy(Server, P+1, MaxInt);
      end
    else
      begin
        Params.Values['Server']:= Server;
        Params.Values['Port']:= '3306';
      end;

    Params.Database := Database;
    Params.UserName := Login;
    Params.Password := Password;
  end;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
var DrvParams    : TStringList;
    TempDir      : string;
    I            : integer;
    FileVersion  : TFileVersion;
    {$IFDEF DEBUG}
    //F: TFieldType;
    {$ENDIF}
  {$IFDEF SQLNCLI}
  function ReadDatabaseDriver(const Name: string; const Default: TADODatabaseDriver): TADODatabaseDriver;
  begin
    try
      with TRegistry.Create do
        try
          RootKey := HKEY_CURRENT_USER;
          if OpenKeyReadOnly(RegKey) and ValueExists(Name) then
            Result := TADODatabaseDriver(ReadInteger(Name))
          else
            Result := Default;
        finally
          Free;
        end;
    except
      on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog('ConnectOperationsUnit.Startup.ReadDatabaseDriver skip error: ' + E.Message);
          {$ENDIF}
          Result := Default;
        end;
    end;
  end;
  {$ENDIF}

begin
  {$IFDEF DEBUG}
  DebugLog('ConnectOperationsUnit unit initialization ...');
  {$ENDIF}
  try
    {$IFDEF DEBUG}
    //TempDir := EmptyStr;
    //for F := Low(F) to High(F) do
    //  TempDir := TempDir + IntToStr(Ord(F)) + '=' + StrPas(FieldTypeNames[F]) + #13#10;
    //DebugLog(#13#10 + TempDir);
    {$ENDIF}

    // выбор временного каталога
    TempDir := GetAppDataFolder;

    CurrentIBVersion  := GetIBVersion;
//    CurrentBDEVersion := GetBDEVersion;
    i := GetOracleVersion;
    if i = 0 then
      CurrentORAVersion := EmptyStr
    else
      CurrentORAVersion := Format('%d.%d', [HiWord(i), LoWord(i)]);
    {$IFDEF SQLNCLI}
    // Если есть Native Client, то ...
    if GetNativeClientVersion <> 0 then
      // для баз данных будем автоматически определять драйвер или по значению в реестре
      TDeADODatabase.FDefaultDatabaseDriver := ReadDatabaseDriver(RegDriverMSSQL, ddAuto)
    else
      // иначе для баз данных будем использовать стандартный драйвер
      TDeADODatabase.FDefaultDatabaseDriver := ddOleDB;
    {$ELSE}
    TDeADODatabase.FDefaultDatabaseDriver := ddOleDB;
    {$ENDIF}
    if GetFileVersion(GetModuleName(hInstance), FileVersion) then
      TDeADODatabase.FDefaultApplicationName := UpperCase(cApplicationName) +
        ', version ' + FileVersionToString(FileVersion) +
        IIF(fvfPreRelease in FileVersion.Flags, 'b', IIF(fvfDebug in FileVersion.Flags, 'd', EmptyStr))
        {$IFDEF WIN32} + ', x86'{$ELSE}{$IFDEF WIN64} + ', x64'{$ENDIF}{$ENDIF}
    else
      TDeADODatabase.FDefaultApplicationName := UpperCase(cApplicationName) + ', version 1.0a'
        {$IFDEF WIN32} + ', x86'{$ELSE}{$IFDEF WIN64} + ', x64'{$ENDIF}{$ENDIF};
    {
    if CurrentBDEVersion > 0 then
      begin
        // если установлен пакет BDE
        DrvParams := TStringList.Create;
        DrvParams.Values['LANGDRIVER'] := 'DBWINUS0';
        if Assigned(Sessions) then
          for I := 0 to Sessions.Count-1 do
            begin
              Sessions[I].ConfigMode := cmSession;
              try Sessions[I].ModifyDriver('DBASE', DrvParams); except end;
              try Sessions[I].ModifyDriver('PARADOX', DrvParams); except end;
              try Sessions[I].ModifyDriver('FOXPRO', DrvParams); except end;
              try Sessions[I].PrivateDir := DeTempDirBDE; except end;
            end;
        DrvParams.Free;
      end;
      {}
  except
    WriteLog('Error in ConnectOperationUnit initialization', True, 'Errors');
  end;
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('ConnectOperationsUnit unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

