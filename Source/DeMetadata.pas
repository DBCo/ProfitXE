unit DeMetadata;

interface

uses System.Types, SysUtils, Classes, Contnrs, Dialogs, DB, Zip, Generics.Collections,
     DeTypes, DeVariable, DeDB, DeMeta, DSMeta, DeParser, DataCacheUnit, DeActions, Security;

type
  // класс с информацией о конфигурации
  TDeConfigItem = class
  private
    FName            : string;
    FServer          : string;
    FDatabase        : string;
    FLogin           : string;
    FPassword        : string;
    FConnectString   : string;
    FType            : TDatabaseType;
    FDefaultLogin    : string;
    FDefaultPassword : string;
    FCodePage        : Integer;  // v. 17.01
    FSessionLogin    : String;
    FSessionPassword : String;
    //FStandardMainMenu: Boolean;
  public
    property Name : string read FName  write FName;
    property Server : string read FServer write FServer;
    property Database : string read FDatabase write FDatabase;
    property CodePage: Integer read FCodePage write FCodePage;
    property Login : string read FLogin write FLogin;
    property Password : string read FPassword write FPassword;
    property ConnectString : string read FConnectString write FConnectString;
    property DBType : TDatabaseType read FType write FType;
    property DefaultLogin : string read FDefaultLogin write FDefaultLogin;
    property DefaultPassword : string read FDefaultPassword write FDefaultPassword;
    //property StandardMainMenu: Boolean read FStandardMainMenu write FStandardMainMenu;
    procedure Assign(Source: TDeConfigItem);
    function PrepareCommandLineParameters: string;
    property SessionLogin: String read FSessionLogin write FSessionLogin;
    property SessionPassword : String read FSessionPassword write FSessionPassword;
  end;

  // список конфигураций
  TDeConfigList = class(TObjectList)
  private
//    FToolBarsVisible: Boolean;
    FDefineVariables: TDeVariableList;
    FOverrideVariables: TDeVariableList;
    FReadOnly: Boolean;
    function GetItem(const Index: Integer): TDeConfigItem;
    procedure SetItem(const Index: Integer; Value: TDeConfigItem);
  public
    constructor Create;
    destructor Destroy; override;
    property Items[const Index: Integer]: TDeConfigItem read GetItem write SetItem; default;
    procedure LoadFromXML(const aXMLName : String);
    procedure SaveToXML(const aXMLName : String);
    procedure LoadFromParameters;
    procedure LoadFromDirectory(const RootDirectory: string);
//    property ToolBarsVisible: Boolean read FToolBarsVisible write FToolBarsVisible;
    property DefineVariables: TDeVariableList read FDefineVariables;
    property OverrideVariables: TDeVariableList read FOverrideVariables;
    property ReadOnly: Boolean read FReadOnly;
  end;

  // список информации о кэшах
  TCacheData = class
  private
    FRefCount    : integer;
    FCache       : TDataCache;
  public
    constructor Create(aCache : TDataCache);
    destructor Destroy;  override;
    property Cache : TDataCache read FCache;
    property RefCount : integer read FRefCount;
    procedure IncRefCount;
    procedure DecRefCount;
  end;

  TMetadataPresent =
    (
      mpDatabaseDatabaseGUID,   // Есть GUID в базе данных для баз данных (v. 19.07)
      mpDatabaseDatasetGUID,    // Есть GUID в базе данных для наборов данных
      mpMetadataDatasetGUID,    // Есть GUID в метаданных для наборов данных
      mpDatabaseFieldGUID,      // Есть GUID в базе данных для списка полей
      mpMetadataFieldGUID,      // Есть GUID в метаданных для списка полей
      mpDatabaseSolutionGUID,   // Есть GUID в базе данных для списка решений
      mpMetadataSolutionGUID,   // Есть GUID в метаданных для списка решений
      mpDatabaseCommandGUID,    // Есть GUID в базе данных для списка команд
      mpDatabaseMenuGUID,       // Есть GUID в базе данных для меню
      mpDatabaseDataSetCaption, // Есть формула Заголовка в базе данных для наборов данных (v.16.10)
      mpDatabaseNewConstraints, // К полям таблицы M_CONSTRAINTS обращаться через новый префикс 'MCN_' (v. 18.12)
      mpCommandParamOrders,     // В параметрах команды есть поле MCP_ORDER - порядок параметра в списке (v. 19.04)
      mpDatabaseSchema,         // В наборах данных можно использовать схемы из баз данных (v. 19.08)
      mpFieldDefaultDB,         // Есть поле DefaultDB в списке полей
      mpFieldScale              // Есть поле Scale в списке полей
    );
  TMetadataPresents = set of TMetadataPresent;


  // метаданные (таблицы, наборы данных и т.п.)
  TDeMetadata = class
  private
    FLock        : Integer;
    FLockList    : Array of Variant;
    FNeedReloading : Boolean;
    FCatalogsDatabase,
    FSampleMTDatabase,
    FLanguageDatabase,
    FMetadataDatabase : TDeCustomDataBase;
    FDatabases   : TConnectionList;
    FTablesList  : TTablesList;
    FDataStorage : TDataSetsMeta;
    FShowRemoved : boolean;
    FParameters  : TParamList;
    FCacheList   : TObjectList;
    FAgregateType: TAgregateType;
    FMaxTableID  : Integer;
    FMaxFieldID  : Integer;
    FMetadataPresents: TMetadataPresents;
    function LockList_IndexByID(aID: Variant; const aAuto: Boolean = False): Integer;
    function InnerCreateDatabase(const aDatabaseType: TDatabaseType; const AddInList: Boolean): TDeCustomDatabase;
    function GetFieldMetaDataSet: TDeDataSet;
    procedure SetFieldMetaDataSet(const Value: TDeDataSet);
    function GetActionMetaDataSet: TDeDataSet;
    procedure SetActionMetaDataSet(const Value: TDeDataSet);
    function GetConfigDB: TDeCustomDatabase;
    procedure SetNeedReloading(const Value: Boolean);
    type
      /// <summary>
      ///   Компонент слежения за удалением FieldMetaDataSet и ActionMetaDataSet.
      /// </summary>
      TDeMetadataNotifier = class(TComponent)
      private
        FFieldMetaDataSet: TDeDataSet;
        FActionMetaDataSet: TDeDataSet;
        procedure SetFieldMetaDataSet(const Value: TDeDataSet);
        procedure SetActionMetaDataSet(const Value: TDeDataSet);
      protected
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      public
        destructor Destroy; override;
        property FieldMetaDataSet: TDeDataSet read FFieldMetaDataSet write SetFieldMetaDataSet;
        property ActionMetaDataSet: TDeDataSet read FActionMetaDataSet write SetActionMetaDataSet;
      end;
    var
      FNotifier: TDeMetadataNotifier;

    function GetDatabase(const Index: Integer): TDeCustomDatabase;
    function GetDatabasesCount: Integer;
    procedure SetShowRemoved(const aValue: Boolean);
    function GetDataSetsCount: Integer;
    function GetDataSetMeta(const Index: Integer): TDataSetMeta;
    procedure LoadParameters;
    function GetParameters: TParamList;
    procedure SetParamValue(const aParam : string; aValue: Variant);
    function GetParamValue(const aParam: string): Variant;
    procedure SetParamVarValue(aItem : TVariableItem;  const aValue : Variant);
    function GetParamVarValue(aItem : TVariableItem) : Variant;
    function LoadDatabases : boolean;
    procedure LoadTableHeaders;
    function GetTableByIndex(const Index: Integer): TTableMeta;
    function GetTableCount: Integer;
    procedure SetAgregateType(const aAgrigateType: TAgregateType);
    function GetLibraryCount: Integer;
    function GetLibraryData(const Index: Integer): TDataCache;
    function PrepareBooleanXML(const AttrName: string; const Value: Variant; const Inversed: Boolean = False): string;
    function PrepareMetadataDictionaryXML(const DatasetName: string; const Level: Integer): string;
    function PrepareMetadataLinkXML(const DatasetID, FieldID: Integer; const Level: Integer): string;
    function PrepareMetadataFieldXML(const DatasetID, FieldID: Integer; const Level: Integer): string;
    function PrepareMetadataElementXML(const DatasetID, ElementID: Integer; const Level: Integer): string;
    function PrepareMetadataCommandXML(const CommandID: Integer; const Level: Integer; ZipFile: TZipFile): string;
    function PrepareMetadataDatasetXML(const DatasetID: Integer; const Level: Integer; ZipFile: TZipFile): string;
    function PrepareMetadataDatabaseXML(const DatabaseID: Integer; const Level: Integer; ZipFile: TZipFile): string;
    function PrepareMetadataSolutionXML(const SolutionID: Integer; const Level: Integer; ZipFile: TZipFile): string;
    function TryDatasetNameToDatasetID(const DatasetName: string; var DatasetID: Integer; const FirstInMetadataSearch: Boolean = True): Boolean;
    function TryFieldNameToFieldID(const DatasetID: Integer; const FieldName: string; var FieldID: Integer; const FirstInMetadataSearch: Boolean = True): Boolean; overload;
    function TryFieldNameToFieldID(const DatasetName, FieldName: string; var DatasetID, FieldID: Integer; const FirstInMetadataSearch: Boolean = True): Boolean; overload;
    function CheckPresentFields: Boolean;
    procedure LoadMetadataFields;
  public
    MetaTables : array [1..CMetaTablesCount] of TTableMeta;
    property FieldMetaDataSet: TDeDataSet read GetFieldMetaDataSet write SetFieldMetaDataSet;
    property ActionMetaDataSet: TDeDataSet read GetActionMetaDataSet write SetActionMetaDataSet;
    constructor Create;
    destructor Destroy; override;
    property Config__DB: TDeCustomDatabase read GetConfigDB;
    property LanguageDB: TDeCustomDatabase read FLanguageDatabase;
    property SampleMTDB: TDeCustomDatabase read FSampleMTDatabase;
    property CatalogsDB: TDeCustomDatabase read FCatalogsDatabase;
    property MetadataDB: TDeCustomDatabase read FMetadataDatabase write FMetadataDatabase;
    property Databases[const Index: Integer]: TDeCustomDatabase read GetDatabase;
    property DatabasesCount: Integer read GetDatabasesCount;
    property ShowRemoved: Boolean read FShowRemoved write SetShowRemoved;
    property DataSetsCount: Integer read GetDataSetsCount;
    property DataSets[const Index: Integer]: TDataSetMeta read GetDataSetMeta;
    property LibraryCount: Integer read GetLibraryCount;
    property LibraryData[const Index: Integer]: TDataCache read GetLibraryData;
    property Parameters: TParamList read GetParameters;
    property ParamValue[const aParam: string]: Variant read GetParamValue;
    property Tables[const Index: Integer]: TTableMeta read GetTableByIndex;
    property TablesList: TTablesList read FTablesList;
    property TableCount: Integer read GetTableCount;
    property AgregateType: TAgregateType read FAgregateType write SetAgregateType;
    { общие методы }
    function ReadAndCheckMetadataTables: boolean;
    function ReadAndCheckMetadataFields: boolean;
    function ReadAndCheckMetadataActions: boolean;
    function CheckMetadataVersion : boolean;
    function Connect : boolean;
    function Disconnect : boolean;
    procedure CheckConnection;
    procedure DisplayVersionError;
    function GenerateDynamicFieldID: Integer;
    { методы работы со списком баз данных }
    function CreateDatabase(const aDeConfigItem: TDeConfigItem; const AddInList: Boolean): TDeCustomDatabase; overload;
    function CreateDatabase(const aCacheItem: TCacheItem; const AddInList: Boolean): TDeCustomDatabase; overload;
    function CreateDatabase(const aDeDataset: TDeDataset; const AddInList: Boolean): TDeCustomDatabase; overload;
    function CreateDatabase(const aDeCustomDatabase: TDeCustomDatabase; const AddInList: Boolean): TDeCustomDatabase; overload;

    procedure DeleteDatabase(aDatabase : TDeCustomDatabase);
    function DatabaseByID(const aDatabaseID: Variant): TDeCustomDataBase;
    function DatabaseByGUID(const aDatabaseGUID: String): TDeCustomDataBase;
    function DatabaseByAlias(const aDatabaseAlias: string): TDeCustomDataBase;
    { методы работы со списком наборов данных }
    function GetTableMeta(const ID: Integer): TTableMeta; overload;
    function GetTableMeta(const TableGUID: TGUID): TTableMeta; overload;
    function GetTableMeta(const Value: Variant): TTableMeta; overload;
    function GetTableByName(const aName: string; const aDatabase: TDeCustomDatabase = nil; const SkipChildDataSet: Boolean = True; const ASchemaName: string = ''): TTableMeta;
    function GetTableByNameAndID(const aName: string; const aDatabase: TDeCustomDatabase = nil; aID : Integer = -1): TTableMeta;
    function GetTableByCaption(const aName : string; const aDatabase : TDeCustomDatabase = nil) : TTableMeta;
    function GetTableByFullName(const aName : string; const aDatabase : TDeCustomDatabase = nil) : TTableMeta;
    function GetSystemTableByName(const aName : string) : TTableMeta;
    function GetTableByPath(const sPath: string): TTableMeta;
    procedure DefineTableLinks;
    procedure AddTable(aTableMeta : TTableMeta);
    procedure DeleteTable(aTableMeta : TTableMeta);
    function GetDynamicTableByName(const TableName: string; Database: TDeCustomDatabase): TTableMeta;
    { методы работы со списком экземпляров TDataSetMeta }
    function FindActiveDataSet(const ADS:Integer; const CanSelectItem: Boolean = False): TDataSetMeta;
    function FindActiveDataCache(const ADS:Integer; const CanSelectItem: Boolean = False): TDataCache;
    function FindDataSetContext(DS: Integer; var Key: Variant): Boolean;
    function FindDataSetDefault(DS: Integer; var Key: Variant): Boolean;
    procedure UpdateCardForms;
    procedure RegisterDataset(aDataSet : TDataSetMeta);
    procedure UnregisterDataSet(aDataSet : TDataSetMeta);
    function UpdateDataSets(const DataSetID: Variant; const Changed: TTypeChanged; const Key: Variant; const LocateIndex: Integer = ltNone): Integer; overload;
    function UpdateDataSets(const TableName: string; const Changed: TTypeChanged; const Key: Variant; const LocateIndex: Integer = ltNone): Integer; overload;
    function UpdateDataSets(const DataSetGUID: TGUID; const Changed: TTypeChanged; const Key: Variant; const LocateIndex: Integer = ltNone): Integer; overload;
    { методы работы со списком кэшей }
    function GetLibrary(const aID: Integer; autoCreate: Boolean = True): TDataCache;
    procedure CaptureCache(aCache : TDataCache);
    procedure ReleaseCache(aCache : TDataCache);
    procedure UpdateLibrary; overload;
    procedure UpdateLibrary(const aID: Variant); overload;
    procedure UpdateLibrary(const TableName: string); overload;
    procedure UpdateLibrary(const DataSetGUID: TGUID); overload;
    procedure BeginUpdate;
    procedure EndUpdate;
    function CheckSignatureFromClipboard: Boolean;
    function PrepareMetadataXML(const SolutionID: Integer = 0; const Level: Integer = -1; ZipFile: TZipFile = nil): string;
    class function TryDatasetToGUID(const DatasetName: string; var GUID: TGUID): Boolean;
    class function TryDatasetFieldToGUID(const DatasetName, FieldName: string; var DatasetGUID, FieldGUID: TGUID): Boolean;
    // v 17.11
    // Выполнение скрипта по SendMessage исключению в StoredProc базы данных ...
    function ExecuteScript(const Text: string): Boolean;
    // v 17.10
    property MetadataPresents: TMetadataPresents read FMetadataPresents;
    property NeedReloading: Boolean read FNeedReloading write SetNeedReloading;
    // v.19.05
    function ReadDefaultSolution(const AutoCreated: Boolean = True): Variant;
    function SimpleMode: Boolean;
  end;

  { информация о сообщении }
  TDeMessage = class
  private
    FIco     : Integer;
    FCount   : Integer;
    FCaption : string;
    FText    : string;
    FTime    : TDateTime;
    FMsgType : TMsgDlgType;
  public
    constructor Create(const aIco: Integer; const aCaption, aText: string; const aMsgType: TMsgDlgType = mtInformation);
    property MsgType: TMsgDlgType read FMsgType write FMsgType;
    property Ico: Integer read FIco write FIco;
    property Count: Integer read FCount write FCount;
    property Caption: string read FCaption write FCaption;
    property Text: string read FText write FText;
    property Time: TDateTime read FTime;
  end;

  { список сообщений }
  TDeMessageList = class(TObjectList)
  private
    function GetMessage(const Index: Integer): TDeMessage;
    procedure SetMessage(const Index: Integer; const Value: TDeMessage);
  public
    property Items[const Index: Integer]: TDeMessage read GetMessage write SetMessage; default;
    function AddMessage(const aIco: Integer; const aCaption, aText: string; const aMsgType: TMsgDlgType = mtInformation): TDeMessage;
  end;

var
  ErrorMessage  : string;
  MetaData      : TDeMetaData;
  ConfigList    : TDeConfigList;
  CurrentConfig : TDeConfigItem;
  DeMessages    : TDeMessageList;
  NeedLogout    : boolean = false;
  GlobalActionConditionList : TEventMetaList;

  indFieldsID,//         = 'MFL_ID';
  indFieldsOriginal,//   = 'MFL_ORIGINAL';
  indFieldsTable,//      = 'MFL_TABLE';
  indFieldsName,//       = 'MFL_NAME';
  indFieldsReadOnly,//   = 'MFL_READONLY';
  indFieldsKey,//        = 'MFL_KEY';
  indFieldsUnique,//     = 'MFL_UNIQUE';
  indFieldsNotNull,//    = 'MFL_NOTNULL';
  indFieldsCalculated,// = 'MFL_CALCULATED';
  indFieldsDataType,//   = 'MFL_DATATYPE';
  indFieldsDataSize,//   = 'MFL_DATASIZE';
  indFieldsLink,//       = 'MFL_LINK';
  indFieldsOnDelete,//   = 'MFL_ON_DELETE_ACTION';
  indFieldsRole,//       = 'MFL_ROLE';
  indFieldsVisible,//    = 'MFL_VISIBLE';
  indFieldsWidth,//      = 'MFL_WIDTH';
  indFieldsOrder,//      = 'MFL_ORDER';
  indFieldsTemplate,//   = 'MFL_TEMPLATE';
  indFieldsDefault,//    = 'MFL_DEFAULT';
  indFieldsDefaultDB,//  = 'MFL_DEFAULTDB';
  indFieldsValue1,//     = 'MFL_VALUE1';
  indFieldsValue2,//     = 'MFL_VALUE2';
  indFieldsShowType,//   = 'MFL_SHOWTYPE';
  indFieldsDeleted,//    = 'MFL_DELETED';
  indFieldsLinkRole,//   = 'MFL_LINKROLE';
  indFieldsCategory,//   = 'MFL_CATEGORY';
  indFieldsStored,//     = 'MFL_STORED';
  indFieldsDescription,//= 'MFL_DESCRIPTION'
  indFieldsCharset,//    = 'MFL_CHARSET'
  indFieldsGUID://       = 'MFL_GUID'
                         Integer;

implementation

uses Variants, Controls, Forms, XML.XMLDoc, XML.XMLIntf, Windows, ClipBrd, StrUtils,
     DeLog, Main, BaseGridFormUnit, Funcs,
     Dictionary, DeSettings, DataUnit, ConnectOperationsUnit, DeScript,
     QueryDescriptor, DeTemplate, DeControls, DeCalculator;

{ TDeConfigItem }

procedure TDeConfigItem.Assign(Source: TDeConfigItem);
begin
  FName := Source.FName;
  FServer := Source.FServer;
  FDatabase := Source.FDatabase;
  FLogin := Source.FLogin;
  FPassword := Source.FPassword;
  FConnectString := Source.FConnectString;
  FType := Source.FType;
  FDefaultLogin := Source.FDefaultLogin;
  FDefaultPassword := Source.FDefaultPassword;
  FCodePage := Source.FCodePage;
  FSessionLogin := Source.FSessionLogin;
  FSessionPassword := Source.FSessionPassword;
  //FStandardMainMenu := Source.FStandardMainMenu;
end;

function TDeConfigItem.PrepareCommandLineParameters: string;
begin
  Result := '-name:"' + Name + '" -type:' + IntToStr(Ord(DBType));
  if Length(Server) <> 0 then
    Result := Result + ' -server:"' + Server + '"';
  if Length(Database) <> 0 then
    Result := Result + ' -database:"' + Database + '"';
  if CodePage > 0 then
    Result := Result + ' -codepage:"' + IntToStr(CodePage) + '"';
  if Length(Login) <> 0 then
    Result := Result + ' -login:"' + Login + '"';
  if Length(Password) <> 0 then
    Result := Result + ' -hash:"' + AnsiStringToBase64(XorAnsiString(Password, Login), 0) + '"';
  if Length(ConnectString) <> 0 then
    Result := Result + ' -connectstring:"' + ConnectString + '"';
  if Length(DefaultLogin) <> 0 then
    Result := Result + ' -defaultuser:"' + DefaultLogin + '"';
  if Length(DefaultPassword) <> 0 then
    Result := Result + ' -defaulthash:"' + AnsiStringToBase64(XorAnsiString(DefaultPassword, DefaultLogin), 0) + '"';
end;

{ TDeConfigList }

constructor TDeConfigList.Create;
begin
  inherited Create;
  FReadOnly := False;
  FDefineVariables := TDeVariableList.Create;
  FOverrideVariables := TDeVariableList.Create;
end;

destructor TDeConfigList.Destroy;
begin
  FOverrideVariables.Free;
  FDefineVariables.Free;
  inherited Destroy;
end;

function TDeConfigList.GetItem(const Index: Integer): TDeConfigItem;
begin
  Result := TDeConfigItem(inherited Items[Index]);
end;

procedure TDeConfigList.SetItem(const Index: Integer; Value: TDeConfigItem);
begin
  if Index >= Count then Count := Succ(Index);
  inherited Items[Index] := Value;
end;

procedure TDeConfigList.LoadFromParameters;
var
  ConfigItem: TDeConfigItem;
  Index: Integer;
  Value: string;
  IntValue: Integer;
  NeedAppend: Boolean;
begin
  Clear;
  FReadOnly := False;
  ConfigItem := TDeConfigItem.Create;
  try
    NeedAppend := False;
    for Index := 1 to ParamCount do
      begin
        Value := ParamStr(Index);
        if (Pos('-', Value) = 1) or (Pos('/', Value) = 1) then
          begin
            System.Delete(Value, 1, 1);
            if Pos('name:', LowerCase(Value)) = 1 then
              begin
                System.Delete(Value, 1, 5);
                ConfigItem.Name := Value;
                NeedAppend := True;
              end
            else if Pos('type:', LowerCase(Value)) = 1 then
              begin
                System.Delete(Value, 1, 5);
                if TryStrToInt(Value, IntValue) then
                  if (IntValue >= Ord(Low(ConfigItem.FType))) and (IntValue <= Ord(High(ConfigItem.FType))) then
                    begin
                      ConfigItem.DBType := TDatabaseType(IntValue);
                      NeedAppend := True;
                    end
                  else
                    begin
                      {$IFDEF DeDEBUG}
                      Funcs.WriteLog('Command line parameter ''type'' value %d not between %d and %d!', [IntValue, Ord(Low(ConfigItem.FType)),  Ord(High(ConfigItem.FType))]);
                      {$ENDIF}
                    end
                else if SameText(Value, 'Interbase') then
                  begin
                    ConfigItem.DBType := dtInterbase;
                    NeedAppend := True;
                  end
                else if SameText(Value, 'MSSQL') then
                  begin
                    ConfigItem.DBType := dtMSSQL;
                    NeedAppend := True;
                  end
                else if SameText(Value, 'MSAccess') then
                  begin
                    ConfigItem.DBType := dtMSAccess;
                    NeedAppend := True;
                  end
                else if SameText(Value, 'Oracle') then
                  begin
                    ConfigItem.DBType := dtOracle;
                    NeedAppend := True;
                  end
                else if SameText(Value, 'PostgreSQL') then
                  begin
                    ConfigItem.DBType := dtPostgreSQL;
                    NeedAppend := True;
                  end
                else if SameText(Value, 'ODBC') then
                  begin
                    ConfigItem.DBType := dtODBC;
                    NeedAppend := True;
                  end
                else if SameText(Value, 'ADOXML') then
                  begin
                    ConfigItem.DBType := dtADOXML;
                    NeedAppend := True;
                  end
                else if SameText(Value, 'DBCOXML') then
                  begin
                    ConfigItem.DBType := dtDBCOXML;
                    NeedAppend := True;
                  end
                else
                  begin
                    {$IFDEF DeDEBUG}
                    Funcs.WriteLog('Command line parameter ''type'' unknown value %s!', [QuotedStr(Value)]);
                    {$ENDIF}
                  end;
              end
            else if Pos('server:', LowerCase(Value)) = 1 then
              begin
                System.Delete(Value, 1, 7);
                ConfigItem.Server := Value;
                NeedAppend := True;
              end
            else if Pos('database:', LowerCase(Value)) = 1 then
              begin
                System.Delete(Value, 1, 9);
                ConfigItem.Database := Value;
                NeedAppend := True;
              end
            else if Pos('login:', LowerCase(Value)) = 1 then
              begin
                System.Delete(Value, 1, 6);
                ConfigItem.Login := Value;
                NeedAppend := True;
              end
            else if Pos('hash:', LowerCase(Value)) = 1 then
              begin
                System.Delete(Value, 1, 5);
                ConfigItem.Password := XorAnsiString(AnsiStringFromBase64(Value), ConfigItem.Login);
                NeedAppend := True;
              end
            else if Pos('password:', LowerCase(Value)) = 1 then
              begin
                System.Delete(Value, 1, 9);
                ConfigItem.Password := Value;
                NeedAppend := True;
              end
            else if Pos('connectstring:', LowerCase(Value)) = 1 then
              begin
                System.Delete(Value, 1, 14);
                ConfigItem.ConnectString := Value;
                NeedAppend := True;
              end
            else if Pos('defaultuser:', LowerCase(Value)) = 1 then
              begin
                System.Delete(Value, 1, 12);
                ConfigItem.DefaultLogin := Value;
                NeedAppend := True;
              end
            else if Pos('defaulthash:', LowerCase(Value)) = 1 then
              begin
                System.Delete(Value, 1, 12);
                ConfigItem.DefaultPassword := XorAnsiString(AnsiStringFromBase64(Value), ConfigItem.DefaultLogin);
                NeedAppend := True;
              end
            else if Pos('defaultpassword:', LowerCase(Value)) = 1 then
              begin
                System.Delete(Value, 1, 16);
                ConfigItem.DefaultPassword := Value;
                NeedAppend := True;
              end
            else if Pos('codepage:', LowerCase(Value)) = 1 then
              begin
                System.Delete(Value, 1, 9);
                if TryStrToInt(Value, IntValue) then
                  begin
                    ConfigItem.CodePage := IntValue;
                    NeedAppend := True;
                  end
                else if SameText(Value, 'dos') or SameText(Value, 'dos866') or SameText(Value, 'dos-866') then
                  begin
                    ConfigItem.CodePage := cp866;
                    NeedAppend := True;
                  end
                else if SameText(Value, 'Utf8') or SameText(Value, 'Utf-8') then
                  begin
                    ConfigItem.CodePage := cpUTF8;
                    NeedAppend := True;
                  end
                else if SameText(Value, 'Ansi') or SameText(Value, 'System') then
                  begin
                    ConfigItem.CodePage := cpNone;
                    NeedAppend := True;
                  end
                else
                  begin
                    {$IFDEF DeDEBUG}
                    Funcs.WriteLog('Command line parameter ''CodePage'' unknown value %s!', [QuotedStr(Value)]);
                    {$ENDIF}
                  end;
              end
          end;
      end;
    if NeedAppend then
      begin
        if ConfigItem.DBType = dtNone then
          ConfigItem.DBType := dtMSSQL;

        if Length(ConfigItem.Name) = 0 then
          ConfigItem.Name := 'Default';

        if Add(ConfigItem) <> -1 then
          FReadOnly := True
        else
          begin
            {$IFDEF DeDEBUG}
            Funcs.WriteLog('Command line parameters skipped!!!');
            {$ENDIF}
          end;
      end;
  finally
    if not FReadOnly then ConfigItem.Free;
  end;
  {$IFDEF DeDEBUG}
  if FReadOnly then Funcs.WriteLog('Command line configuration loaded ...', False, 'CommandLine');
  {$ENDIF}
end;

procedure TDeConfigList.LoadFromDirectory(const RootDirectory: string);
const
  faExclude = faDirectory or faVolumeID;
var
  Directory: string;
  ErrorCode: Integer;
  SearchRec: TSearchRec;
  FileName: string;
  Index: Integer;
  ConfigItem: TDeConfigItem;
begin
  Clear;
  FReadOnly := False;
  Directory := RootDirectory;
  if Length(Directory) <> 0 then Directory := IncludeTrailingBackslash(Directory);
  ErrorCode := FindFirst(Directory + '*' + sExtensionXMLX, faAnyFile, SearchRec);
  if ErrorCode = 0 then
    try
      while ErrorCode = 0 do
        begin
          if (SearchRec.Attr and faExclude) = 0 then
            try
              FileName := Directory + SearchRec.Name;
              ConfigItem := TDeConfigItem.Create;
              ConfigItem.DBType := dtDBCOXML;
              ConfigItem.CodePage := 0;
              ConfigItem.ConnectString := FileName;
              ConfigItem.Name := ChangeFileExt(ExtractFileName(FileName), EmptyStr);
              if Add(ConfigItem) = -1 then ConfigItem.Free;
            except
              {$IFDEF DeDEBUG}
              on E: Exception do
                Funcs.WriteLog('Configuration file %s skipped because of an error: %s', [QuotedStr(SearchRec.Name), E.Message], True);
              {$ENDIF}
            end;
          ErrorCode := FindNext(SearchRec);
        end;
      FReadOnly := Count <> 0; // Запрещаем изменять конфигурацию!!!
    finally
      SysUtils.FindClose(SearchRec);
    end;
end;

procedure TDeConfigList.LoadFromXML(const aXMLName : String);
var
  XMLD   : TXMLDocument;
  ChNdL  : IXMLNodeList;
  Node   : IXMLNode;
  I, P   : Integer;
  Item   : TDeConfigItem;
  Comp   : TComponent;
  Value  : string;
  function FindNodeByName(const NodeName: string; const RootNodes: IXMLNodeList): IXMLNode;
  var
    Index: Integer;
  begin
    Result := nil;
    if Assigned(RootNodes) then
      for Index := 0 to Pred(RootNodes.Count) do
        if SameText(RootNodes.Nodes[Index].NodeName, NodeName) then
          begin
            Result := RootNodes.Nodes[Index];
            Break;
          end;
  end;
begin
  Clear;
  Comp := TComponent.Create(nil);
  try
    try
      XMLD := TXMLDocument.Create(Comp);
      XMLD.LoadFromFile(ExtractFilePath(ParamStr(0))+aXMLName);
      //Value := Trim(XMLD.ChildNodes.Last.ChildNodes.Last.Attributes['ToolBars']);
      //ToolBarsVisible := not ((Length(Value) = 0) or SameText(Value, 'hide'));
      //ChNdL := XMLD.ChildNodes.Last.ChildNodes.Last.ChildNodes;
      Node := FindNodeByName('ROWDATA', XMLD.ChildNodes.Last.ChildNodes);
      if Assigned(Node) then
        begin
          ChNdL := Node.ChildNodes;
          for I := 0 to ChNdL.Count-1 do
            begin
              Item := TDeConfigItem.Create;
              Item.Name := ChNdL.Nodes[I].Attributes['Name'];
              Item.DBType := TDatabaseType(ChNdL.Nodes[I].Attributes['Type']);
              Item.Server := VarToStr(ChNdL.Nodes[I].Attributes['Server']);
              Item.Database := VarToStr(ChNdL.Nodes[I].Attributes['Database']);
              if ChNdL.Nodes[I].HasAttribute('CodePage') then
                Item.CodePage:= VarToInt(ChNdL.Nodes[I].Attributes['CodePage'])
              else
                Item.CodePage:= 0;
              Item.Login := ChNdL.Nodes[I].Attributes['Login'];
              if ChNdL.Nodes[I].HasAttribute('Hash') then
                Item.Password := XorAnsiString(AnsiStringFromBase64(ChNdL.Nodes[I].Attributes['Hash']), Item.Login)
              else
                Item.Password := ChNdL.Nodes[I].Attributes['Password'];
              Item.ConnectString := Trim(VarToStr(ChNdL.Nodes[I].Attributes['ConnectString']));
              Item.DefaultLogin := ChNdL.Nodes[I].Attributes['DefaultUser'];
              if ChNdL.Nodes[I].HasAttribute('DefaultHash') then
                Item.DefaultPassword := XorAnsiString(AnsiStringFromBase64(ChNdL.Nodes[I].Attributes['DefaultHash']), Item.DefaultLogin)
              else
                Item.DefaultPassword := ChNdL.Nodes[I].Attributes['DefaultPassword'];

              //Value := Trim(ChNdL.Nodes[I].Attributes['MainMenu']);
              //Item.StandardMainMenu := (Length(Value) = 0) or SameText(Value, 'Standard');

              Add(Item);

              if (Item.DBType in [dtInterbase,dtBDE]) then
                if Item.ConnectString <> EmptyStr then
                begin
                  p := Pos('\', Item.ConnectString);
                  if p = 0 then
                    p := Pos('/', Item.ConnectString);
                  if p = 0 then
                    Item.ConnectString := Variables.AsString[RegDefaultDataPath] + Item.ConnectString;
                end
                else if Item.Database <> EmptyStr then
                begin
                  p := Pos('\', Item.Database);
                  if p = 0 then
                    p := Pos('/', Item.Database);
                  if p = 0 then
                    Item.Database := Variables.AsString[RegDefaultDataPath] + Item.Database;
                end;
            end;
        end
      else
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Tag ''ROWDATA'' not found in configuration file %s!', [QuotedStr(ExtractFileName(aXMLName))]);
          {$ENDIF}
        end;
    except
      {$IFDEF DEBUG}
      on E: Exception do
        DebugLog(ClassName + '.LoadFromXML error: ' + E.Message);
      {$ENDIF}
    end;
  finally
    Comp.Free;
  end;
  FReadOnly := False;
  FDefineVariables.LoadFromFile(ExtractFilePath(ParamStr(0)) + aXMLName, True);
  {$IFDEF DEBUG}
  FDefineVariables.DebugVariablesLog(ClassName + '.LoadFromXML define variables loaded ...');
  {$ENDIF}
  FOverrideVariables.LoadFromFile(ExtractFilePath(ParamStr(0)) + aXMLName, False);
  {$IFDEF DEBUG}
  FOverrideVariables.DebugVariablesLog(ClassName + '.LoadFromXML override variables loaded ...');
  {$ENDIF}
end;

procedure TDeConfigList.SaveToXML(const aXMLName : String);
var
  XMLD           : TXMLDocument;
  I              : Integer;
  Comp           : TComponent;
  RootNode, Node, ItemNode : IXMLNode;
  procedure SetItemNodeAttr(const Name, Value: string);
  begin
    if Length(Value) <> 0 then
      ItemNode.Attributes[Name] := Value;
  end;
begin
  Comp := TComponent.Create(nil);
  try
    try
      XMLD:= TXMLDocument.Create(Application.MainForm.Components[0]) ;
      XMLD.Options:= XMLD.Options+[doNodeAutoIndent];
      XMLD.Active := true;
      XMLD.StandAlone := 'yes';
      XMLD.Version := '1.0';
      RootNode := XMLD.AddChild('DATAPACKET');
      RootNode.Attributes['Version'] := '2.0';
      Node := RootNode.AddChild ('ROWDATA');
      {
      if ToolBarsVisible then
        Node.Attributes['ToolBars'] := 'show'
      else
        Node.Attributes['ToolBars'] := 'hide';
      }
      for I := 0 to Pred(Count) do
      begin
        ItemNode := Node.AddChild('ROW');
        ItemNode.Attributes['Name'] := Items[I].Name;
        ItemNode.Attributes['Type'] := ord(Items[I].DBType);
        ItemNode.Attributes['Server'] := Items[I].Server;
        ItemNode.Attributes['Database'] := Items[I].Database;
        if 0 < Items[I].CodePage then
          ItemNode.Attributes['CodePage'] := Items[I].CodePage;
        SetItemNodeAttr('Login', Items[I].Login);
        SetItemNodeAttr('Hash', AnsiStringToBase64(XorAnsiString(Items[I].Password, Items[I].Login), 0));
        //SetItemNodeAttr('Password', Items[I].Password);
        SetItemNodeAttr('ConnectString', Items[I].ConnectString);
        SetItemNodeAttr('DefaultUser', Items[I].DefaultLogin);
        SetItemNodeAttr('DefaultHash', AnsiStringToBase64(XorAnsiString(Items[I].DefaultPassword, Items[I].DefaultLogin), 0));
        //SetItemNodeAttr('DefaultPassword', Items[I].DefaultPassword);
        {
        if Items[I].StandardMainMenu then
          ItemNode.Attributes['MainMenu'] := 'standard'
        else
          ItemNode.Attributes['MainMenu'] := 'extended';
        }
      end;
      XMLD.SaveToFile(ExtractFilePath(ParamStr(0))+aXMLName);
    except
      {$IFDEF DEBUG}
      on E: Exception do
        DebugLog(ClassName + '.SaveToXML error: ' + E.Message);
      {$ENDIF}
    end;
  finally
    Comp.Free;
  end;
  FDefineVariables.SaveToFile(ExtractFilePath(ParamStr(0)) + aXMLName, True);
  {$IFDEF DEBUG}
  FDefineVariables.DebugVariablesLog(ClassName + '.SaveToXML define variables saved ...');
  {$ENDIF}
  FOverrideVariables.SaveToFile(ExtractFilePath(ParamStr(0)) + aXMLName, False);
  {$IFDEF DEBUG}
  FOverrideVariables.DebugVariablesLog(ClassName + '.LoadFromXML override variables saved ...');
  {$ENDIF}
end;


{ TCacheData }

constructor TCacheData.Create(aCache: TDataCache);
begin
  inherited Create;
  FCache := aCache;
end;

destructor TCacheData.Destroy;
begin
  FCache.Free;
  inherited Destroy;
end;

procedure TCacheData.IncRefCount;
begin
  inc(FRefCount);
end;

procedure TCacheData.DecRefCount;
begin
  dec(FRefCount);
end;

{ TDeMetadata }

procedure TDeMetadata.RegisterDataset(aDataSet: TDataSetMeta);
begin
  FDataStorage.Add(aDataSet);
end;

procedure TDeMetadata.AddTable(aTableMeta: TTableMeta);
begin
  FTablesList.Add(aTableMeta);
end;

procedure TDeMetadata.CheckConnection;
begin
  if not MetadataDB.Connected then
    if LastErrorDBType <> dtNone then
      DisplayVersionError
    else
      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(pChar('_Error.connectmeta')));
end;

function TDeMetaData.ReadAndCheckMetadataTables: boolean;
var DSL: TStringList;
    DlgResult,i,N: Integer;
begin
  DSL:= TStringList.Create;
  MetaData.MetadataDB.RetrieveTableNames(DSL);

  if DSL.Count = 0 then
    begin
      Result:= False;
      ErrorMessage:= 'Database haven''t any tables'
    end
  else
    begin
      Result:= True;
      ErrorMessage:= EmptyStr;
      N:=0;
      for i := Low(MetaTableCheck) to High(MetaTableCheck) do
        if DSL.IndexOf(MetaTableNames[i]) < 0 then
          begin
            if Result then ErrorMessage:= MetaTableNames[i]
                      else ErrorMessage:= ErrorMessage+', '+MetaTableNames[i];
            Result:= False;
            inc(N);
          end;

      if N = Length(MetaTableNames) then ErrorMessage:= 'Database haven''t any meta tables';
      if N > 0                      then ErrorMessage:= 'Can''t find table ' + ErrorMessage;
    end;
  DSL.Free;
end;

function TDeMetaData.ReadAndCheckMetadataFields: boolean;
var ErrorStr  : String;

    procedure TestField(const FieldName: string; var FieldIndex: Integer);
    begin
      FieldIndex := FieldMetaDataSet.IndexByName(FieldName);
      if FieldIndex < 0 then ErrorStr := ErrorStr + FieldName + ', ';
    end;
begin
  ErrorStr := EmptyStr;

  try
    FieldMetaDataSet := MetaData.MetadataDB.CreateQuery(qtSelect);
    FieldMetaDataSet.Descr.BeginUpdate;
    try
      FieldMetaDataSet.Descr.Table := tblFields;
      FieldMetaDataSet.Descr.AddParamCondition(fldFieldsTable, ftInteger, opEQ, 'ID', -1);
      FieldMetaDataSet.Descr.AddParamCondition(fldFieldsLink, ftInteger, opEQ, 'ID2', -1);
      FieldMetaDataSet.Descr.AddOperation(opOR);

      FieldMetaDataSet.Descr.AddParamCondition(fldFieldsDeleted, ftSmallint, opNE, 'DID', 1);
      //FieldMetaDataSet.Descr.AddOperation(opNot);

  //    видимость = 0
      FieldMetaDataSet.Descr.AddParamCondition(fldFieldsVisible, ftSmallint, opGT, 'VID', -1);
      FieldMetaDataSet.Descr.AddOperation(opAnd);

      FieldMetaDataSet.Descr.AddOperation(opAnd);

      // 31.08.2015 Куфенко + Добавляем только нужные поля!!!
      FieldMetaDataSet.Descr.AddFields([fldFieldsID, fldFieldsOriginal, fldFieldsTable, fldFieldsName, fldFieldsReadOnly,
        fldFieldsKey, fldFieldsUnique, fldFieldsNotNull, fldFieldsCalculated, fldFieldsDataType, fldFieldsDataSize,
        fldFieldsLink, fldFieldsOnDelete{, fldFieldsOnExecute}, fldFieldsRole, fldFieldsVisible, fldFieldsWidth,
        fldFieldsOrder, fldFieldsTemplate, fldFieldsDefault, fldFieldsValue1, fldFieldsValue2,
        fldFieldsShowType, fldFieldsDeleted, fldFieldsLinkRole, fldFieldsCategory, fldFieldsStored]);
      // 31.08.2015 -
      if MetaData.MetadataDB.FieldExists(tblFields, fldFieldsGUID) then
        begin
          FieldMetaDataSet.Descr.AddField(fldFieldsGUID);
          Include(FMetadataPresents, mpDatabaseFieldGUID); // В базе данных есть GUID`ы для полей
        end;
    finally
      FieldMetaDataSet.Descr.EndUpdate;
    end;
    FieldMetaDataSet.Prepare;
    FieldMetaDataSet.Open;
  except
    on E: Exception do
      Funcs.WriteLog(ClassName + '.ReadAndCheckMetadataFields for FieldMetaDataSet error: ' + E.Message);
  end;

  if FieldMetaDataSet.Active then
    begin
      TestField( fldFieldsID         , indFieldsID         );
      TestField( fldFieldsOriginal   , indFieldsOriginal   );
      TestField( fldFieldsTable      , indFieldsTable      );
      TestField( fldFieldsName       , indFieldsName       );
      TestField( fldFieldsReadOnly   , indFieldsReadOnly   );
      TestField( fldFieldsKey        , indFieldsKey        );
      TestField( fldFieldsUnique     , indFieldsUnique     );
      TestField( fldFieldsNotNull    , indFieldsNotNull    );
      TestField( fldFieldsCalculated , indFieldsCalculated );
      TestField( fldFieldsDataType   , indFieldsDataType   );
      TestField( fldFieldsDataSize   , indFieldsDataSize   );
      TestField( fldFieldsLink       , indFieldsLink       );
      TestField( fldFieldsOnDelete   , indFieldsOnDelete   );

      // В настоящее време не используется, хотя сохранено
      //TestField( fldFieldsOnExecute  , indFieldsOnExecute  );
      indFieldsCharset := FieldMetaDataSet.IndexByName(fldFieldsCharset);
      indFieldsGUID := FieldMetaDataSet.IndexByName(fldFieldsGUID);
      indFieldsDescription := FieldMetaDataSet.IndexByName(fldFieldsDescription);

      TestField( fldFieldsRole       , indFieldsRole       );
      TestField( fldFieldsVisible    , indFieldsVisible    );
      TestField( fldFieldsWidth      , indFieldsWidth      );
      TestField( fldFieldsOrder      , indFieldsOrder      );
      TestField( fldFieldsTemplate   , indFieldsTemplate   );
      TestField( fldFieldsDefault    , indFieldsDefault    );
      TestField( fldFieldsValue1     , indFieldsValue1     );
      TestField( fldFieldsValue2     , indFieldsValue2     );
      TestField( fldFieldsShowType   , indFieldsShowType   );
      TestField( fldFieldsDeleted    , indFieldsDeleted    );
      TestField( fldFieldsLinkRole   , indFieldsLinkRole   );
      TestField( fldFieldsCategory   , indFieldsCategory   );
      TestField( fldFieldsStored     , indFieldsStored     );
    end
  else
      ErrorStr:='Table "M_Fields" corrupted or not exist';

  FieldMetaDataSet.Close;

  result:= (0=Length(ErrorStr));
end;

function TDeMetaData.ReadAndCheckMetadataActions: boolean;
var ErrorStr  : String;
begin
  ErrorStr := EmptyStr;
  try
    ActionMetaDataSet := MetaData.MetadataDB.CreateQuery(qtSelect);
    ActionMetaDataSet.Descr.BeginUpdate;
    try
      ActionMetaDataSet.Descr.Table := tblActionConditions;
      // 27.11.2015 + Добавляем только нужные поля!
      ActionMetaDataSet.Descr.AddFields([fldACID, fldACActionName, fldACProcedureName, fldACProcParam, fldACCondition]);
      // 27.11.2015 -
      ActionMetaDataSet.Descr.AddParamCondition(fldACTableMetaID, ftInteger, opEQ, 'ID', -1);
      ActionMetaDataSet.Descr.AddParamCondition(fldACDeleted, ftInteger, opEQ, 'DID', 0);
      ActionMetaDataSet.Descr.AddOperation(opAnd);
    finally
      ActionMetaDataSet.Descr.EndUpdate;
    end;
    ActionMetaDataSet.Prepare;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog(ClassName + '.ReadAndCheckMetadataActions error: ' + E.Message);
        {$ENDIF}
        ActionMetaDataSet.Free;
        ActionMetaDataSet := nil;
        ErrorStr:='Table "M_ActionConditions" corrupted or not exist';
      end;
  end;

  result:= (0=Length(ErrorStr));
end;

function TDeMetaData.CheckMetadataVersion: boolean;
var PValue : Variant;
begin
  result:= ReadAndCheckMetadataTables;
  result:= Result and ReadAndCheckMetadataFields;
  result:= Result and ReadAndCheckMetadataActions;
  if result then
    begin
      PValue:= ParamValue[MetadataVersion];
      Result:= Result and (not VarIsEmpty(PValue)) and (PValue = CurrentVersion);
    end;
end;

function TDeMetaData.Connect: Boolean;
var BackupCursor : TCursor;
    ConfigDB     : TDeCustomDatabase;
    I            : integer;
    Vi           : integer;
    {$IFDEF DEBUG}
    function DebugMetadataPresentsString: string;
    const
      { TODO -oКуфенко -cMetadataPresent : Взял распаровал всё и добавил не парных. Ну тебе виднее, ты метаструктуру будешь сопровождать и за совместимостью смотреть. }
      Names: array[TMetadataPresent] of PChar = (
        'mpDatabaseDatabaseGUID',
        'mpDatabaseDatasetGUID', 'mpMetadataDatasetGUID', 'mpDatabaseFieldGUID',
        'mpMetadataFieldGUID', 'mpDatabaseSolutionGUID', 'mpMetadataSolutionGUID',
        'mpDatabaseCommandGUID', 'mpDatabaseMenuGUID', 'mpDatabaseDataSetCaption',
        'mpDatabaseNewConstraints', 'mpCommandParamOrders',
        'mpDatabaseSchema', 'mpFieldDefaultDB', 'mpFieldScale');
    var
      Index: TMetadataPresent;
    begin
      Result := EmptyStr;
      for Index := Low(Names) to High(Names) do
        if Index in FMetadataPresents then
          begin
            if Length(Result) <> 0 then Result := Result + ', ';
            Result := Result + StrPas(Names[Index]);
          end;
      Result := '[' + Result + ']';
    end;
    {$ENDIF}
begin
  {$IFDEF DEBUG}
  DebugLog(ClassName + '.Connect start ...');
  {$ENDIF}
  result := false;
  FMetadataPresents := [];
  ErrorMessage := EmptyStr;
  BackupCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    try
      // Если нет метабазы и есть текущая конфигурация, то ...
      if not Assigned(MetadataDB) and Assigned(CurrentConfig) then
        try
          {$IFDEF DEBUG}
          DebugLog('Metadata connect ask create metadata from %s ...', [QuotedStr(CurrentConfig.Name)]);
          {$ENDIF}
          // Создадим метабазу по текущей конфигурации ...
          MetadataDB:= MetaData.CreateDatabase(CurrentConfig, True);
        except
          MetaData.Disconnect;
          raise;
        end;

      MetadataDB.CheckConnection;  // подключать

      if MetadataDB.Connected then
        begin
          if CheckMetadataVersion then
            begin // метаструктура есть и подходит
              if LoadDatabases then
                begin
                  LoadTableHeaders;
                  CheckPresentFields; // v. 18.09: Перенёс из CheckMetadataVersion для оптимизации количества запросов!
                  ActivateAllActions;
                  //MetaActionList.Clear; дополняем таблицу
                  Dict.Load;
                  ProfitActions.LoadData;
                  {$IFDEF DEBUG}
                  //DebugLog(ClassName + '.Connect: GlobalVariables fill start ...');
                  {$ENDIF}
                  for I := 0 to Pred(Parameters.Count) do
                    begin
                      VI:=GlobalVariables.Add(
                        TVariableItem.Create(ftString, Parameters[I].Name, [amRead, amWrite]));
                      GlobalVariables[VI].OnGetValue := GetParamVarValue;
                      GlobalVariables[VI].OnSetValue := SetParamVarValue;
                      {$IFDEF DEBUG}
                      //DebugLog(ClassName + '.Connect: GlobalVariables append %s ...', [QuotedStr(Parameters[I].Name)]);
                      {$ENDIF}
                    end;
                  {$IFDEF DEBUG}
                  GlobalVariables.DebugVariablesLog(ClassName + '.Connect GlobalVariables initialized ...');
                  //DebugLog(ClassName + '.Connect: GlobalVariables fill finish ...');
                  {$ENDIF}
                  result := true;
                end;
              DM.AfterConnect;
            end;

        if (result = False) then
          begin // метаструктуры нет или битая или старая. ЗАХОДИМ В ОБРАЗЕЦ МЕТАСТРУКТУРЫ
            MetaData.Disconnect;
            MetadataDB:= FSampleMTDatabase;
            MetadataDB.Connected:= True;

            CheckMetadataVersion;

            LoadTableHeaders;
            ActivateAllActions;

            try
              ConfigDB:= MetaData.CreateDatabase(CurrentConfig, True);
              ConfigDB.Connected:= True;
            except
            end;

            ErrorMessage:= EmptyStr; // '_eRror.metadataversion'#10 + ErrorMessage;
            Result:= True;
          end;
        end;
    except
      on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog(ClassName + '.Connect error: ' + E.Message);
          {$ENDIF}
          Disconnect;
          ErrorMessage := '_Error.connectmeta'#10 + E.Message;
        end;
    end;
  finally
    Screen.Cursor := BackupCursor;
    if (not result) and (Length(ErrorMessage) = 0) then
      ErrorMessage := '_Error.connectmeta';

    if (not Result) and (Length(MetadataDB.ConnectErrorMessage)>0) then
      ErrorMessage:= ErrorMessage+#10+MetadataDB.ConnectErrorMessage;
  end;
  {$IFDEF DEBUG}
  DebugLog(ClassName + '.Connect finish %s ...', [DebugMetadataPresentsString]);
  {$ENDIF}
end;

constructor TDeMetaData.Create;
begin
  inherited Create;
  FLock      := 0;
  FNeedReloading:= False;

  FCatalogsDatabase:= TDBCOXMLDatabase.Create(nil);
  FCatalogsDatabase.ID:= CatalogsIndex;
  FCatalogsDatabase.Alias:= 'Catalogs';
  FCatalogsDatabase.Server:= InternalServerGUID;
  FCatalogsDatabase.Database:= 'META_CATALOGS';

  FSampleMTDatabase:= TDBCOXMLDatabase.Create(nil);
  FSampleMTDatabase.ID:= SampleMTIndex;
  FSampleMTDatabase.Alias:= 'SampleMT';
  FSampleMTDatabase.Server:= InternalServerGUID;
  FSampleMTDatabase.Database:= 'META_SAMPLE';

  FLanguageDatabase:= TDBCOXMLDatabase.Create(nil);
  FLanguageDatabase.ID:= LanguageIndex;
  FLanguageDatabase.Alias:= 'Language';
  FLanguageDatabase.Server:= InternalServerGUID;
  FLanguageDatabase.Database:= LanguageDatabaseGUID;

  FMetadataDatabase:= nil;

  FDatabases := TConnectionList.Create;
  FTablesList   := TTablesList.Create;
  FDataStorage  := TDataSetsMeta.Create;
  FCacheList    := TObjectList.Create;
  FAgregateType := atCount;
  FShowRemoved  := False;
  //FShowArchived := False;
  FNotifier := TDeMetadataNotifier.Create(nil);
end;

function TDeMetadata.InnerCreateDatabase(const aDatabaseType: TDatabaseType; const AddInList: Boolean): TDeCustomDatabase;
begin
  case aDatabaseType of
    dtInterbase:
      result:= TDeIBDatabase.Create(nil);
    dtFireBird:
      result:= TDeFBDatabase.Create(nil);
    dtMySql:
      result:= TDeMySQLDatabase.Create(nil);
    dtPostgreSQL:
      result:= TDePGDatabase.Create(nil);
    dtODBC, dtMSAccess, dtMSSql:
      result:= TDeADODatabase.Create(nil);
    dtADOXML:
      result:= TDeADOXMLDatabase.Create(nil);
    dtDBCOXML:
      result:= TDBCOXMLDatabase.Create(nil);
 // dtBDE, dtParadox, dtdBase : result := TDeBDEDatabase.Create(nil);
 // dtOracle : result := TDeOracleDataBase.Create(nil);
    dtFileDB :
      result := TDeFileDataBase.Create(nil);
 {   dtDB2, dtMySql, dtSyBase, dtInformix:
      result:= TDePGDataBase.Create(nil); {}
    else
      begin
        Result:= TDeEmptyDatabase.Create(nil); // nil; // v. 18.5 changed
        Exit;
      end;
  end;

  Result.DatabaseType:= aDatabaseType;

  if AddInList and Assigned(Result) then
    FDatabases.Add(Result);
end;

function TDeMetadata.CreateDatabase(const aDeConfigItem: TDeConfigItem; const AddInList: Boolean): TDeCustomDatabase;
begin
  Result:= InnerCreateDatabase(aDeConfigItem.DBType, AddInList);

  Result.Alias:= aDeConfigItem.Name;
  Result.DatabaseType:= aDeConfigItem.DBType;
  Result.Server:= aDeConfigItem.Server;
  Result.Database:= aDeConfigItem.Database;
  Result.ConnectString:= aDeConfigItem.ConnectString;
  Result.Login:= aDeConfigItem.Login;
  Result.Password:= aDeConfigItem.Password;
  Result.CodePage:= aDeConfigItem.CodePage;
  Result.IsReadOnly:= False;
  Result.VariantGUID:= MetadataDatabaseGUID;
  Result.ID:= 0;
end;

function TDeMetadata.CreateDatabase(const aCacheItem: TCacheItem; const AddInList: Boolean): TDeCustomDatabase;
begin
  Result:= InnerCreateDatabase(TDatabaseType(VarToInt(aCacheItem.ValueByName[fldBaseType])), AddInList);

  Result.Alias:= aCacheItem.ValueNativeByName[fldBaseAlias];
  Result.DatabaseType:= TDatabaseType(VarToInt(aCacheItem.ValueByName[fldBaseType]));
  Result.Server:= aCacheItem.ValueNativeByName[fldBaseServer];
  Result.Database:= aCacheItem.ValueNativeByName[fldBaseDatabase];
  Result.ConnectString:= aCacheItem.ValueNativeByName[fldBaseConnectStr];
  Result.Login:= aCacheItem.ValueNativeByName[fldBaseLogin];
  Result.Password:= aCacheItem.ValueNativeByName[fldBasePassword];
  Result.CodePage:= VarToInt(aCacheItem.ValueByName[fldBaseCharset]);
  Result.IsReadOnly:= VarToInt(aCacheItem.ValueByName[fldBaseReadOnly]) = 1;
  Result.VariantGUID:= VarToStr(aCacheItem.ValueByName[fldBaseGUID]);
  Result.ID:= VarToInt(aCacheItem.ValueByName[fldBaseId]);
end;

function TDeMetadata.CreateDatabase(const aDeDataset: TDeDataset; const AddInList: Boolean): TDeCustomDatabase;
begin
  Result:= InnerCreateDatabase(TDatabaseType(VarToInt(aDeDataset.ValueByName[fldBaseType])), AddInList);

  Result.Alias:= aDeDataset.StringValueByName(fldBaseAlias);
  Result.DatabaseType:= TDatabaseType(VarToInt(aDeDataset.ValueByName[fldBaseType]));
  Result.Server:= aDeDataset.StringValueByName(fldBaseServer);
  Result.Database:= aDeDataset.StringValueByName(fldBaseDatabase);
  Result.ConnectString:= aDeDataset.StringValueByName(fldBaseConnectStr);
  Result.Login:= aDeDataset.StringValueByName(fldBaseLogin);
  Result.Password:= aDeDataset.StringValueByName(fldBasePassword);
  Result.CodePage:= VarToInt(aDeDataset.ValueByNameDef(fldBaseCharset, 0));
  Result.IsReadOnly:= VarToInt(aDeDataset.ValueByNameDef(fldBaseReadOnly, 0)) = 1;
  Result.VariantGUID:= VarToStr(aDeDataset.ValueByNameDef(fldBaseGUID, EmptyStr));
  Result.ID:= VarToInt(aDeDataset.ValueByName[fldBaseId]);
end;

function TDeMetadata.CreateDatabase(const aDeCustomDatabase: TDeCustomDatabase; const AddInList: Boolean): TDeCustomDatabase;
begin
  Result:= InnerCreateDatabase(aDeCustomDatabase.DatabaseType, AddInList);

  Result.Alias:= aDeCustomDatabase.Alias;
  Result.DatabaseType:= aDeCustomDatabase.DatabaseType;
  Result.Server:= aDeCustomDatabase.Server;
  Result.Database:= aDeCustomDatabase.Database;
  Result.ConnectString:= aDeCustomDatabase.ConnectString;
  Result.Login:= aDeCustomDatabase.Login;
  Result.Password:= aDeCustomDatabase.Password;
  Result.CodePage:= aDeCustomDatabase.CodePage;
  Result.IsReadOnly:= aDeCustomDatabase.IsReadOnly;
  Result.VariantGUID:= aDeCustomDatabase.VariantGUID;
  Result.ID:= aDeCustomDatabase.ID;
end;

procedure TDeMetadata.DefineTableLinks;
var
  I, J: Integer;
  LoginSchema, TableSchema: string;
begin
  // Получаем схему авторизованного пользователя ...
  LoginSchema := MetadataDB.LoginSchema;
  // установка связей между таблицам и поиск метаструктурных таблиц
  for I := 0 to Pred(FTablesList.Count) do
  begin
    FTablesList[I].DefineTableLinks;
    // Получаем схему таблицы ...
    TableSchema := FTablesList[I].Schema;
    // Если схемы таблицы нет, то считаем её в схеме авторизованного пользоватея!
    if Length(TableSchema) = 0 then
      TableSchema := LoginSchema;
    for J := Low(MetaTables) to High(MetaTables) do
      if not Assigned(MetaTables[J]) then
        if SameText(FTablesList[I].Table, MetaTableNames[J]) and
           SameText(LoginSchema, TableSchema) and
           (FTablesList[I].OwnerTable = FTablesList[I]) then
        begin
          MetaTables[J] := FTablesList[I];
          FTablesList[I].MetaTableIndex := J; // Запоминаем индекс для определения метаструктурных таблиц!
          Break;
        end;
  end;
end;

procedure TDeMetadata.DeleteDatabase(aDatabase: TDeCustomDatabase);
begin
  FDatabases.Delete(FDatabases.IndexOf(aDatabase));
end;

procedure TDeMetadata.DeleteTable(aTableMeta: TTableMeta);
begin
  FTablesList.Delete(FTablesList.IndexOf(aTableMeta));
end;

destructor TDeMetadata.Destroy;
begin
  FreeAndNil(FLockList);
  FreeAndNil(FCacheList);
  FreeAndNil(FDataStorage);
  FreeAndNil(FTablesList);
  FreeAndNil(FDatabases);
  FreeAndNil(FParameters);
  FreeAndNil(FNotifier);
  inherited Destroy;
end;

function TDeMetaData.Disconnect : boolean;
var I : integer;
begin
  {$IFDEF DEBUG}
  DebugLog(ClassName + '.Disconnect start ...');
  {$ENDIF}
  try
    FCacheList.Clear;
    FDataStorage.Clear;
    DeactivateAllActions;
    ProfitActions.Clear;

    FTablesList.Clear;
    if Assigned(FParameters) then
      for I := 0 to Parameters.Count-1 do
        GlobalVariables.FindVariable(Parameters[I].Name).Free;
    FreeAndNil(FParameters);
    FreeAndNil(MainMenuMeta);
    for I := Low(MetaTables) to High(MetaTables) do
      MetaTables[I] := nil;

    DM.BeforeUnconnect;

    FDatabases.Clear;
    result := true;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog(ClassName + '.Disconnect skip error: ' + E.Message);
        {$ENDIF}
        Result := False;
      end;
  end;
  {$IFDEF DEBUG}
  DebugLog(ClassName + '.Disconnect finish ...');
  {$ENDIF}
  FMetadataDatabase:= nil;
end;

procedure TDeMetadata.DisplayVersionError;
begin
  if LastErrorDBType <> dtNone then
    begin
      case LastErrorDBType of
        dtInterbase:
          if CurrentIBVersion = 0 then
            ErrorMessage := pChar(Format('_dE.dbdriver',['Interbase']))
          else
            ErrorMessage := pChar(Format(GetTitle('_dE.dbversion'),
              ['Interbase',IntToStr(MinIBVersion), IntToStr(CurrentIBVersion)]));
        dtBDE:
          if CurrentBDEVersion = 0 then
            ErrorMessage := pChar(Format('_dE.dbdriver',['BDE']))
          else
            ErrorMessage := pChar(Format(GetTitle('_dE.dbversion'),
              ['BDE',IntToStr(MinBDEVersion div 100)+'.'+IntToStr(MinBDEVersion mod 100),
               IntToStr(CurrentBDEVersion div 100)+'.'+IntToStr(CurrentBDEVersion mod 100)]));
        dtOracle :
          if CurrentORAVersion = EmptyStr then
            ErrorMessage := pChar(Format('_dE.dbdriver',['Oracle']));
      end;
      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, 0);
    end;
end;

procedure TDeMetadata.UnregisterDataSet(aDataSet: TDataSetMeta);
begin
  FDataStorage.Extract(aDataSet);
end;

function TDeMetadata.GetLibrary(const aID: Integer; autoCreate: Boolean = True): TDataCache;
var
  Index: integer;
  TableMeta: TTableMeta;
  FilterItem: TFilterItem;
begin
  Result := nil;
  for Index := 0 to Pred(FCacheList.Count) do
    if (TCacheData(FCacheList[Index]).Cache.TableMeta.ID = aID) then
      begin
        Result := TCacheData(FCacheList[Index]).Cache;
        TCacheData(FCacheList[Index]).IncRefCount;
        Break;
      end;

  if not Assigned(Result) and autoCreate then
    begin
      TableMeta := GetTableMeta(aID);
      if Assigned(TableMeta) and Assigned(TableMeta.Database) then
        begin
          Result := TDataCache.Create(TableMeta, True);
          if Assigned(Result.TableMeta) then
            begin
              Result.SortList.Add(Result.TableMeta.KField[0].ID, sdAscending);

              for Index := 0 to Pred(TableMeta.Filters.Count) do
                begin
                  FilterItem := TFilterItem.Create;
                  FilterItem.Assign(TableMeta.Filters[Index]);
                  Result.Filters.AddFilter(FilterItem);
                end;

              try
                Result.PrepareData;
              except
                on E: Exception do
                  begin
                    {$IFDEF DEBUG}
                    DebugLog(ClassName + '.GetLibrary: ' + E.Message);
                    {$ENDIF}
                    FreeAndNil(result);
                  end;
              end;

              if assigned(Result) then
                TCacheData(FCacheList[FCacheList.Add(TCacheData.Create(Result))]).IncRefCount;
            end
          else
            FreeAndNil(result);
        end;
    end;
end;

procedure TDeMetadata.CaptureCache(aCache : TDataCache);
var I : integer;
begin
  for I := 0 to FCacheList.Count-1 do
    if TCacheData(FCacheList[i]).Cache = aCache then
    begin
      TCacheData(FCacheList[i]).IncRefCount;
      break;
    end;
end;

procedure TDeMetadata.ReleaseCache(aCache : TDataCache);
var I : integer;
begin
  for I := 0 to FCacheList.Count-1 do
    if TCacheData(FCacheList[i]).Cache = aCache then
    begin
      TCacheData(FCacheList[i]).DecRefCount;
      break;
    end;
end;

procedure TDeMetadata.UpdateLibrary;
begin
  UpdateLibrary(-1);
end;

procedure TDeMetadata.UpdateLibrary(const aID: Variant);
var I: integer;
begin
  if FLock>0 then
    begin
      LockList_IndexByID(aID, True);
    end
  else
    begin
      case aID of
       -1: for I := 0 to FCacheList.Count-1 do
             if TCacheData(FCacheList[I]).Cache.Active
               and ((not TCacheData(FCacheList[i]).Cache.TableMeta.IsSystem) or (UserSession.IsAdmin))
               and  (not TCacheData(FCacheList[i]).Cache.TableMeta.IsReadOnly) then
                 TCacheData(FCacheList[I]).Cache.Update(mcUpdateLibrary, Null);
       -2: for I := 0 to FCacheList.Count-1 do
             if TCacheData(FCacheList[I]).Cache.Active then
                 TCacheData(FCacheList[I]).Cache.Update(mcUpdateLibrary, Null);
      else for I := 0 to FCacheList.Count-1 do
             if TCacheData(FCacheList[I]).Cache.Active
                and (TCacheData(FCacheList[i]).Cache.TableMeta.ID = aID) then
                 TCacheData(FCacheList[I]).Cache.Update(mcUpdateLibrary, Null);
      end;
    end;
end;

procedure TDeMetadata.UpdateLibrary(const TableName: string);
var Index: Integer;
begin
  for Index := 0 to Pred(FCacheList.Count) do
    if TCacheData(FCacheList[Index]).Cache.Active then
      if SameText(TableName, TCacheData(FCacheList[Index]).Cache.TableMeta.Table) then
        if FLock > 0 then LockList_IndexByID(TCacheData(FCacheList[Index]).Cache.TableMeta.ID, True)
                     else TCacheData(FCacheList[Index]).Cache.Update(mcUpdateLibrary, Null);
end;

procedure TDeMetadata.UpdateLibrary(const DataSetGUID: TGUID);
var Index: Integer;
begin
  for Index := 0 to Pred(FCacheList.Count) do
    if TCacheData(FCacheList[Index]).Cache.Active then
      if tsGUID in TCacheData(FCacheList[Index]).Cache.TableMeta.LoadingState then
        if IsEqualGUID(TCacheData(FCacheList[Index]).Cache.TableMeta.GUID, DataSetGUID) then
          if FLock > 0 then LockList_IndexByID(TCacheData(FCacheList[Index]).Cache.TableMeta.ID, True)
                       else TCacheData(FCacheList[Index]).Cache.Update(mcUpdateLibrary, Null);
end;

procedure TDeMetadata.BeginUpdate;
begin
  if FLock = 0 then SetLength(FLockList, 0);
  Inc(FLock);
end;

procedure TDeMetadata.EndUpdate;
var
  Index: Integer;
begin
  Dec(FLock);
  if FLock = 0 then
    begin
      for Index:= Low(FLockList) to High(FLockList) do
        UpdateLibrary(FLockList[Index]);
      SetLength(FLockList, 0);
    end;
end;

function TDeMetadata.FindActiveDataSet(const ADS: Integer; const CanSelectItem: Boolean = False): TDataSetMeta;
var
  Index: Integer;
begin
  Result := nil;

  for Index := 0 to Pred(MForm.BaseFormsCount) do
   if (MForm.BaseForms[Index] is TBaseGridForm) and Assigned(MForm.BaseForms[Index].DataSetMeta) and Assigned(MForm.BaseForms[Index].DataSetMeta.Table) and (MForm.BaseForms[Index].DataSetMeta.Table.ID = ADS) then
     if ((not CanSelectItem) or TBaseGridForm(MForm.BaseForms[Index]).CanShowCard ) then
       begin
          Result := MForm.BaseForms[Index].DataSetMeta;
          Exit;
       end;

  for Index := 0 to Pred(FDataStorage.Count) do
    if (FDataStorage[Index].UsageType = utForm) and
       (FDataStorage[Index].Table.ID = ADS) and
       (FDataStorage[Index].Cache.Active) then
    begin
      Result := FDataStorage[Index];
      Break;
    end;
end;

function TDeMetadata.FindActiveDataCache(const ADS: Integer; const CanSelectItem: Boolean = False): TDataCache;
var
  DataSet: TDataSetMeta;
begin
  DataSet := FindActiveDataSet(ADS, CanSelectItem);
  if Assigned(DataSet) then
    Result := DataSet.Cache
  else
    Result := nil;
end;

function TDeMetadata.FindDataSetContext(DS: Integer; var Key: Variant): Boolean;
var i,j : Integer;
    DSKey : Variant;
    WTM: TTableMeta;
    FM : TFieldMeta;
    Q : TDeDataset;
begin
  Key:= unassigned;
  result:= false;

  //ищем в списке открытых таблиц
  for i := 0  to  FDataStorage.Count-1 do
    if (FDataStorage[i].UsageType = utForm) and
       (FDataStorage[i].Table.ID = DS) and
       (FDataStorage[i].Cache.Active) then
    begin
      DSKey := FDataStorage[i].GetKeyValue;
      if VarIsArray(DSKey) then
        Key := DSKey[0]
      else
        Key := DSKey;
      Result:=True;
      Exit;
    end;

  //ищем в списке полей-ссылок открытых таблиц
  for i := 0  to  FDataStorage.Count-1 do
    if (FDataStorage[i].UsageType = utForm) and
       (FDataStorage[i].Table.ID <> DS) and
       (FDataStorage[i].Cache.Active) then
    begin
      for j:=0 to FDataStorage[i].Table.Fields.Count-1 do
        if (FDataStorage[i].Table.Fields[j].IsLookup) and
           (FDataStorage[i].Table.Fields[j].Link = DS) and
            assigned(FDataStorage[i].Cache.FocusedItem) then
          begin
            Key:= FDataStorage[i].Cache.FocusedItem.ValueByName
                          [FDataStorage[i].Table.Fields[j].LookupPair.Original];
            Result:=True;
            Exit;
          end;
    end;

  //ищем в параметрах - текущий пользователь/офис
  WTM := MetaData.GetTableMeta(MetaData.ParamValue['WorkerTable']);
  //if DS = StrToIntDef(MetaData.ParamValue['WorkerTable'], -1) then
  if Assigned(WTM) and (DS = WTM.ID) then
    begin
      key:= UserSession.WorkerID;
      Result:=True;
      Exit;
    end;

  //ищем в параметрах - текущий офис
  if Assigned(MetaData.Parameters.FindParam('WorkerOffice')) then
      try
        if Assigned(WTM) then
          begin
            FM := WTM.Fields.FindByName(MetaData.ParamValue['WorkerOffice']);
            if Assigned(FM) and (FM.Link = DS) then
              begin
                Q := WTM.Database.CreateQuery(qtSelect);
                try
                  Q.Descr.BeginUpdate;
                  try
                    Q.Descr.Table := WTM.Table;
                    // 09.09.2015 Куфенко + Добавляем только нужные поля!!!
                    Q.Descr.AddField(String(MetaData.ParamValue['WorkerOffice']));
                    // 09.09.2015 -
                    Q.Descr.AddParamCondition(WTM.KField[0].Original, WTM.KField[0].DataType, opEQ, 'ID', UserSession.WorkerID);
                  finally
                    Q.Descr.EndUpdate;
                  end;
                  Q.Open;
                  if Q.RecordCount = 1 then
                    begin
                      Key := Q.Value[0]; // Q.ValueByName[MetaData.ParamValue['WorkerOffice']];
                      Result := True;
                    end;
                finally
                  Q.Free;
                end;
              end;
          end;
      except
        {$IFDEF DEBUG}
        on E: Exception do
          DebugLog(ClassName + '.FindContextDataSet error: ' + E.Message);
        {$ENDIF}
      end;

end;

function TDeMetadata.FindDataSetDefault(DS: Integer; var Key: Variant): Boolean;
var TM: TTableMeta;
    Q : TDeDataset;
begin
  TM:= MetaData.GetTableMeta(DS);
  Result:= False;
  if assigned(TM) and assigned(TM.OField) then
     begin
       Q := TM.Database.CreateQuery(qtSelect);
       try
         Q.Descr.Table := TM.Table;
         Q.Descr.AddCondition(TM.OField.Original, TM.OField.DataType, opEQ, TM.OField.Value2);
         Q.Open;
         if Q.RecordCount = 1 then
           begin
             Key:= Q.ValueByName[TM.KField[0].Original];
             Result:= True;
           end;
       finally
         Q.Free;
       end;
     end;
end;

function TDeMetadata.DatabaseByID(const aDatabaseID: Variant): TDeCustomDataBase;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to Pred(FDatabases.Count) do
    if FDatabases[Index].ID = aDatabaseID then
      Exit(FDatabases[Index]);

  if aDatabaseID =  0 then Exit(FMetadataDatabase);
  if aDatabaseID = -1 then Exit(FCatalogsDatabase);
  if aDatabaseID = -2 then Exit(FSampleMTDatabase);
  if aDatabaseID = -3 then Exit(FLanguageDatabase);
end;

function TDeMetadata.DatabaseByGUID(const aDatabaseGUID: String): TDeCustomDataBase;
var
  Index: Integer;
begin
  Result := nil;
  if 0 < Length(aDatabaseGUID) then
    for Index := 0 to Pred(FDatabases.Count) do
      if FDatabases[Index].VariantGUID = aDatabaseGUID then
        Exit(FDatabases[Index]);
end;

function TDeMetadata.DatabaseByAlias(const aDatabaseAlias: string): TDeCustomDataBase;
var
  Index: Integer;
begin
  Result := nil;

  if (aDatabaseAlias = '*') or SameText(MetadataDB.Alias, aDatabaseAlias) then  Exit(MetadataDB);

  for Index := 0 to Pred(FDatabases.Count) do
    if SameText(FDatabases[Index].Alias, StringReplace(aDatabaseAlias, '*', MetadataDB.Alias, [])) then
       Exit(FDatabases[Index]);
end;

function TDeMetadata.GetDatabase(const Index: Integer): TDeCustomDatabase;
begin
  if (0<= Index) and (Index < FDatabases.Count)
    then Result:= FDatabases[Index]
    else Result:= nil;
end;

function TDeMetadata.GetDatabasesCount : integer;
begin
  result := FDatabases.Count;
end;

function TDeMetadata.GetDataSetMeta(const Index: Integer): TDataSetMeta;
begin
  Result := FDataStorage[Index];
end;

procedure TDeMetadata.LoadParameters;
var R: Integer;
    ParamItem : TParamItem;
    Q         : TDeDataset;
begin
  FParameters.Free;
  FParameters:= TParamList.Create;
  Q := MetadataDB.CreateQuery(qtSelect);
  try
    Q.Descr.BeginUpdate;
    try
      Q.Descr.Table:= tblParameters;
      Q.Descr.AddCondition(fldParamDeleted, ftInteger, opNE, 1);
    finally
      Q.Descr.EndUpdate;
    end;
    Q.Open(true);
    for R:=0 to Pred(Q.RecordCount) do
      begin
        Q.RecNo:= R;
        ParamItem:= TParamItem.Create;
        FParameters.Add(ParamItem);
        ParamItem.Assign(Q);
      end;
  finally
    Q.Free;
  end;
  FParameters.MergeFromSystem;
  {$IFDEF DEBUG}
  FParameters.DebugParamsLog(ClassName + '.LoadParameters loaded ...');
  {$ENDIF}
end;

function TDeMetadata.GetParameters: TParamList;
begin
  if not Assigned(FParameters) then
    LoadParameters;
  Result := FParameters;
end;

procedure TDeMetadata.SetParamValue(const aParam : string; aValue: Variant);
var ParamItem : TParamItem;
    Q         : TDeDataset;
begin
  ParamItem := Parameters.FindParam(aParam);
  if Assigned(ParamItem) then
    try
      ParamItem.Value:=WideStringToUnicode(aValue);

      Q := MetadataDB.CreateQuery(qtUpdate);
      try
        Q.Descr.BeginUpdate;
        try
          Q.Descr.Table := tblParameters;
          Q.Descr.Assign(Q.Descr);
          Q.Descr.AddField(fldParamValue, ftString, ParamItem.Value);
          Q.Descr.AddCondition(fldParamName, ftString, opEQ, ParamItem.Name);
          Q.Descr.AddCondition(fldParamDeleted, ftSmallint, opNE, 1);
          Q.Descr.AddOperation(opAnd);
        finally
          Q.Descr.EndUpdate;
        end;
        Q.ExecuteQuery;
      finally
        Q.Free;
      end;
    except
      on E: Exception do
      if Length(E.Message)>0 then
        SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(pChar(E.Message)))
      else
        SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(pChar('_dL.procedureerror')));
    end;
end;

function TDeMetadata.GetParamValue(const aParam: string): Variant;
var
  ParamItem: TParamItem;
  FPostfix : TExpressionItem;
  FParser: TDeParser;
  FCalc: TDeCalculator;
begin
  ParamItem := Parameters.FindParam(aParam);
  if Assigned(ParamItem) then
    begin
      // Без расчета возвращаем wcNone
      if (ParamItem.WhenCalc = wcNone) then Exit( ConvertValue(ParamItem.AsString, ParamItem.DataType) );

      // Без расчета возвращаем wcNone (wcAuto not '=...')
      if (ParamItem.WhenCalc = wcAuto) then
        if Not (Copy(ParamItem.AsString, 1, 1) = '=') then
           Exit( ConvertValue(ParamItem.AsString, ParamItem.DataType) );

      // Считаем
      if (ParamItem.Value = Unassigned) or (ParamItem.WhenCalc = wcEvery) then
        begin
          FPostfix:= TExpressionItem.Create;

          FParser:= TDeParser.Create;
          try
            if (ParamItem.WhenCalc = wcAuto) then
              FParser.Parse( Copy(ParamItem.AsString, 2, MaxInt), FPostfix)
            else
              FParser.Parse(      ParamItem.AsString,             FPostfix);
          finally
            FParser.Free;
          end;

          if 0 < FPostfix.Count then
            begin
              FCalc:= TDeCalculator.Create;
              try
                ParamItem.Value:= FCalc.Calculate(FPostfix);
              except
                FPostfix.Clear;  // Считаем что выражение не определено
              end;
              FCalc.Free;
            end;

          if 0 = FPostfix.Count then // Не смогли посчитать - устанваливаем дефолтные значения
            if (ParamItem.DataType = ftAutoInc) then ParamItem.Value:= 1
                                                else ParamItem.Value:= ParamItem.AsString;

          FPostfix.Free;
        end
      else
        if ParamItem.DataType = ftAutoInc then
          ParamItem.Value := ParamItem.Value + 1;

      Result := ConvertValue(ParamItem.Value, ParamItem.DataType);
    end
  else
    Result := Unassigned;
end;

procedure TDeMetadata.SetParamVarValue(aItem : TVariableItem;  const aValue : Variant);
begin
  {$IFDEF DEBUG}
  if Assigned(aItem) then
    DebugLog('%s.SetParamVarValue(%s, {}) ...', [ClassName, QuotedStr(aItem.Name)])
  else
    DebugLog(ClassName + '.SetParamVarValue(nil, {}) ...');
  {$ENDIF}
  SetParamValue(aItem.Name, aValue);
end;

function TDeMetadata.GetParamVarValue(aItem : TVariableItem) : Variant;
begin
  {$IFDEF DEBUG}
  if Assigned(aItem) then
    DebugLog('%s.GetParamVarValue(%s) ...', [ClassName, QuotedStr(aItem.Name)])
  else
    DebugLog(ClassName + '.GetParamVarValue(nil) ...');
  {$ENDIF}
  result := GetParamValue(aItem.Name);
end;

function TDeMetadata.GetDataSetsCount: Integer;
begin
  Result := FDataStorage.Count;
end;

function TDeMetadata.GetTableByName(const aName: string; const aDatabase: TDeCustomDatabase; const SkipChildDataSet: Boolean; const ASchemaName: string): TTableMeta;
var I : integer;
begin
  result := nil;
  if Length(aName)=0 then Exit;

  for I := 0 to FTablesList.Count-1 do
    if (CompareText(FTablesList[I].Table, aName) = 0) and SameText(FTablesList[I].Schema, ASchemaName) and
       ((FTablesList[I].OwnerTable = FTablesList[I]) or
        (FTablesList[I].OwnerTable = nil) or (not SkipChildDataSet)) and
       ((not Assigned(aDatabase)) or (FTablesList[I].Database = aDatabase)) then
    begin
      result := FTablesList[I];
      break;
    end;
end;

function TDeMetadata.GetTableByNameAndID(const aName: string;
  const aDatabase: TDeCustomDatabase = nil; aID : Integer = -1): TTableMeta;
var I,N : integer;
begin
  result := nil;
  N:=0;
  for I := 0 to FTablesList.Count-1 do
    if (CompareText(FTablesList[I].Table, aName) = 0) and
       ((not Assigned(aDatabase)) or (FTablesList[I].Database = aDatabase)) then
    begin
      result := FTablesList[I];
      Inc(N);
    end;
  // не нашли или нашли единственный набор данных основанный на таблице
  if N<=1 then Exit;

  for I := 0 to FTablesList.Count-1 do
    if (CompareText(FTablesList[I].Table, aName) = 0) and
       (FTablesList[I].ID= aID) and
       ((not Assigned(aDatabase)) or (FTablesList[I].Database = aDatabase)) then
    begin
      result := FTablesList[I];
      Break;
    end;
end;

function TDeMetadata.GetSystemTableByName(const aName: string): TTableMeta;
var I : integer;
begin
  for i := 0 to FTablesList.Count-1 do
    if SameText(FTablesList[I].Table, aName) and (FTablesList[I].SolutionID = MetaSolution) then
      Exit(FTablesList[I]);

  result := nil;
end;

function TDeMetadata.GetTableByCaption(const aName: string; const aDatabase: TDeCustomDatabase = nil): TTableMeta;
var I : integer;
begin
  result := nil;
  for I := 0 to FTablesList.Count-1 do
    if (CompareText(FTablesList[I].Name, aName) = 0) and
       ((FTablesList[I].OwnerTable = FTablesList[I]) or
        (FTablesList[I].OwnerTable = nil)) and
       ((not Assigned(aDatabase)) or (FTablesList[I].Database = aDatabase)) then
    begin
      result := FTablesList[I];
      break;
    end;
end;

function TDeMetadata.GetTableByFullName(const aName: string; const aDatabase: TDeCustomDatabase = nil): TTableMeta;
var
  Index: Integer;
begin
  try
    Result := nil;
    for Index := 0 to Pred(FTablesList.Count) do
      if (CompareText(FTablesList[Index].FullTable, aName) = 0) and
         ((FTablesList[Index].OwnerTable = FTablesList[Index]) or (FTablesList[Index].OwnerTable = nil)) and
         ((not Assigned(aDatabase)) or (CompareText(FTablesList[Index].Database.Alias,aDatabase.Alias)=0)) then
        begin
          Result := FTablesList[Index];
          Break;
        end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('%s.GetTableByFullName(%s, %p) error: %s', [ClassName, aName, Pointer(aDatabase), E.Message]);
        {$ENDIF}
        raise;
      end;
  end;
end;

function TDeMetadata.GetTableByPath(const sPath: string): TTableMeta;
var
  i : integer;
  DBName,DSName : string;
begin
  try
    Result:=nil;
    i := pos('.',sPath);
    if i <= 0 then exit;
    DBName := Trim(copy(sPath,1,i-1));
    DSName := Trim(copy(sPath,i+1,Length(sPath)-i));
    Result := GetTableByFullName(DSName, MetaData.DatabaseByAlias(DBName));
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('%s.GetTableByPath(%s) error: %s', [ClassName, QuotedStr(sPath), E.Message]);
        {$ENDIF}
        raise;
      end;
  end;
end;

function TDeMetadata.GetTableMeta(const ID: Integer): TTableMeta;
//var
//  Index: Integer;
begin
  Result := FTablesList.FindByID(ID);
//  Result := nil;
//  for Index := 0 to Pred(FTablesList.Count) do
//    if FTablesList[Index].ID = ID then
//      begin
//        Result := TTableMeta(FTablesList[Index]);
//        Break;
//      end;
end;

function TDeMetadata.GetTableMeta(const TableGUID: TGUID): TTableMeta;
begin
  Result := FTablesList.FindByGUID(TableGUID);
end;

function TDeMetadata.LoadDatabases: Boolean;
var R: Integer;
    Q   : TDeDataSet;
    DBM : TDeCustomDatabase;
begin
  {$IFDEF DEBUG}
  DebugLog(ClassName + '.LoadDatabases start ...');
  {$ENDIF}

  Q := nil;
  result := false;
  try
    try
      Q := MetadataDB.CreateQuery(qtSelect);
      Q.Descr.BeginUpdate;
      try
        Q.Descr.Table := tblBase;
        Q.Descr.AddCondition(fldBaseDeleted, ftSmallint, opNE, 1);
        Q.Descr.AddCondition(fldBaseId, ftInteger, opNE, 0);
        Q.Descr.AddOperation(opAND);
      finally
        Q.Descr.EndUpdate;
      end;
      Q.Open;
      if Q.IndexByName(fldBaseGUID) <> -1 then
        Include(FMetadataPresents, mpDatabaseDatabaseGUID);
      for R:=0 to Pred(Q.RecordCount) do
      begin
        Q.RecNo:= R;
        DBM:= CreateDatabase(Q, True);
      end;

      if FDatabases.Count=1 then
        CreateDatabase(FDatabases[0], True);

      result := true;
    except
      {$IFDEF DEBUG}
      on E: Exception do
        DebugLog(ClassName + '.LoadDatabases error: ' + E.Message);
      {$ENDIF}
    end;
  finally
    Q.Free;
  end;
  {$IFDEF DEBUG}
  if Assigned(FDatabases) then
    FDatabases.DebugConnectionsLog(ClassName + '.LoadDatabases finish ...')
  else
    DebugLog(ClassName + '.LoadDatabases finish ...');
  {$ENDIF}
end;

procedure TDeMetadata.LoadTableHeaders;
var R: Integer;
    Q         : TDeDataSet;
    TableMeta : TTableMeta;
//    Parser    : TDeParser;
  procedure MainFormProgressNotify(const wParam, lParam: NativeInt);
  begin
    if Assigned(Application.MainForm) then
      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, wParam, lParam);
  end;
begin
  {$IFDEF DEBUG}
  DebugLog(ClassName + '.LoadTableHeaders start ...');
  {$ENDIF}
  Q := MetadataDB.CreateQuery(qtSelect);
  try
    Q.Descr.Table := tblDataset;
    Q.Descr.AddCondition(fldDatasetDeleted, ftSmallInt, opNE, 1);
    //Q.Descr.AddOperation(opNot);
    // 27.08.2015 + Куфенко: Читаем только необходимые поля!
    Q.Descr.AddFields([fldDataSetId, fldDatasetParent, fldDataSetDatabase, fldDataSetSolution, fldDataSetTable,
      fldDataSetName, fldDataSetSelect, fldDataSetUpdate, fldDataSetInsert, fldDataSetDelete, fldDataSetICO,
      fldDataSetReadOnly, fldDataSetIsList, fldDataSetGViewType, fldDataSetGViewPars, fldDataSetOnAddMenue,
      fldDataSetFilter {, fldDataSetDeleted,}]);
    // 27.08.2015 -
    if MetadataDB.FieldExists(tblDataset, fldDataSetGUID) then
      begin
        Q.Descr.AddField(fldDataSetGUID);
        Include(FMetadataPresents, mpDatabaseDatasetGUID); // В базе данных есть GUID`ы для наборов данных
      end;
    if MetadataDB.FieldExists(tblDataset, fldDataSetSchema) then
      begin
        Q.Descr.AddField(fldDataSetSchema);
        Include(FMetadataPresents, mpDatabaseSchema); // В базе данных есть поля со схемой для наборов данных
      end;
    Q.Open;
    MainFormProgressNotify(Q.RecordCount, 1);
    try
      for R:=0 to Pred(Q.RecordCount) do
        begin
          Q.RecNo:= R;
          MainFormProgressNotify(Q.RecNo, 0);
          TableMeta:= TTableMeta.Create;
          TableMeta.Assign(Q);
          FTablesList.Add(TableMeta);
          if FMaxTableID < TableMeta.ID then
            FMaxTableID := TableMeta.ID;
          {$IFDEF DEBUG}
          if tsGUID in TableMeta.LoadingState then
            DebugLog('%s.LoadTableHeaders loaded %s [%d:%s] ...', [ClassName, QuotedStr(TableMeta.Table), VarToInt(TableMeta.ID), QuotedStr(GUIDToString(TableMeta.GUID))])
          else
            DebugLog('%s.LoadTableHeaders loaded %s [%d] ...', [ClassName + QuotedStr(TableMeta.Table), VarToInt(TableMeta.ID)]);
          {$ENDIF}
        end;
    finally
      MainFormProgressNotify(-1, -1);
    end;
  finally
    Q.Free;
  end;
  DefineTableLinks;
  // Загружаем "скопом" все поля для метаструктуртых таблиц ...
  LoadMetadataFields;
  // Загружаем глобальные/локальные фильтры ...
  if Assigned(GroupFilters) then GroupFilters.LoadFromDatabase;
  {$IFDEF DEBUG}
  if Assigned(FTablesList) then
    FTablesList.DebugTablesLog(ClassName + '.LoadTableHeaders finish ...')
  else
    DebugLog(ClassName + '.LoadTableHeaders finish ...');
  {$ENDIF}
end;

function TDeMetadata.LockList_IndexByID(aID: Variant; const aAuto: Boolean): Integer;
var i: Integer;
begin
  Result:= -1;
  for i:= Low(FLockList) to High(FLockList) do
    if FLockList[i] = aID then
      begin
        Result:= i;
        Break;
      end;

  if (Result = -1) and (aAuto) then
    begin
      Result:= Length(FLockList);
      SetLength(FLockList, Succ(Result));
      FLockList[Result]:= aId;
    end;
end;

procedure TDeMetadata.LoadMetadataFields;
var
  Count, Index: Integer;
  TableIDs: array of Variant;
  VariantIDs: Variant;
  Operation: TOperationType;
  Dataset: TDeDataset;
  TableMeta: TTableMeta;
begin
  try
    SetLength(TableIDs, FTablesList.Count);
    Count := Low(TableIDs);
    for Index := 0 to Pred(FTablesList.Count) do
      if Assigned(FTablesList[Index]) and
        ( (Assigned(FTablesList[Index].Database) and FTablesList[Index].Database.isInternalDataBase) or
          (FTablesList[Index].IsMetabaseTable and Assigned(FTablesList[Index].Database) and (FTablesList[Index].Database.ID = MetadataDB.ID)) ) then
        begin
          TableIDs[Count] := FTablesList[Index].ID;
          Inc(Count);
        end;
    SetLength(TableIDs, Count);
    case Count of
      1: { Всего одна таблица }
        begin
          VariantIDs := TableIDs[Low(TableIDs)];
          Operation := opEQ;
        end;
    else
      VariantIDs := VarArrayOf(TableIDs);
      Operation := opIn;
    end;
    Dataset := MetaData.MetadataDB.CreateQuery(qtSelect);
    try
      Dataset.Descr.Table := tblFields;
      Dataset.Descr.AddCondition(fldFieldsVisible, ftSmallint, opGT, -1); // видимость >= 0
      Dataset.Descr.AddCondition(fldFieldsDeleted, ftSmallint, opNE, 1);  // не удалено
      Dataset.Descr.AddOperation(opAnd);
      Dataset.Descr.AddCondition(fldFieldsTable, ftInteger, Operation, VariantIDs);
      //Dataset.Descr.AddCondition(fldFieldsLink, ftInteger, Operation, VariantIDs);
      //Dataset.Descr.AddOperation(opOr);
      Dataset.Descr.AddOperation(opAnd);

      Dataset.Descr.AddFields([fldFieldsID, fldFieldsOriginal, fldFieldsTable, fldFieldsName, fldFieldsReadOnly,
        fldFieldsKey, fldFieldsUnique, fldFieldsNotNull, fldFieldsCalculated, fldFieldsDataType, fldFieldsDataSize,
        fldFieldsLink, fldFieldsOnDelete, fldFieldsRole, fldFieldsVisible, fldFieldsWidth,
        fldFieldsOrder, fldFieldsTemplate, fldFieldsDefault, fldFieldsValue1, fldFieldsValue2,
        fldFieldsShowType, fldFieldsDeleted, fldFieldsLinkRole, fldFieldsCategory, fldFieldsStored]);

      if (mpDatabaseFieldGUID in FMetadataPresents) then Dataset.Descr.AddField(fldFieldsGUID);

      Dataset.Descr.AddSortFields([fldFieldsTable, fldFieldsOrder]);
      Dataset.Open;
      for Index := Low(TableIDs) to High(TableIDs) do
        begin
          TableMeta := GetTableMeta(TableIDs[Index]);
          if Assigned(TableMeta) then
            if not (tsFields in TableMeta.LoadingState) then
              TableMeta.PreLoadFields(Dataset);
        end;
    finally
      Dataset.Free;
    end;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog(ClassName + '.LoadMetadataFields skip error: ' + E.Message);
    {$ENDIF}
  end;
end;

procedure TDeMetadata.SetShowRemoved(const aValue: boolean);
var I : integer;
begin
  if aValue <> FShowRemoved then
  begin
    FShowRemoved := aValue;
    SendMessage( Application.MainForm.Handle, DM_ACTIONNOTIFY, dm_SHOWREMOVED, Byte(FShowRemoved));

    { проходим по таблицам }
    for I := 0 to TableCount-1 do
      if Assigned(Tables[I].DeletedFilter) then
      begin
        Tables[I].DeletedFilter.Active := not FShowRemoved;
        if Assigned(Tables[I].NField) then
          Tables[I].NField.UpdateControls;
      end;
    { обновляем наборы данных }
    for I := 0 to FDataStorage.Count-1 do
      if FDataStorage[I].Cache.Active
        and Assigned(FDataStorage[I].Table.DeletedFilter) then
          FDataStorage[I].Cache.Update(mcUpdate, Null);
    { обновляем библиотечные списки }
    for I := 0 to LibraryCount-1 do
      if LibraryData[I].Active
        and Assigned(LibraryData[I].TableMeta.DeletedFilter) then
          LibraryData[I].Update(mcUpdate, Null);
  end;
end;

function TDeMetadata.SimpleMode: Boolean;
begin
  Result:= (MetadataDB = FSampleMTDatabase);
end;

procedure TDeMetadata.UpdateCardForms;
var
  Index: Integer;
begin
  for Index := 0 to Pred(FDataStorage.Count) do
    if (FDataStorage[Index].UsageType = utForm) and Assigned(FDataStorage[Index].CardForm) then
      LangFormUpdate(FDataStorage[Index].CardForm);
end;

function TDeMetadata.GetTableByIndex(const Index: Integer): TTableMeta;
begin
  Result := FTablesList.Items[Index];
end;

function TDeMetadata.GetTableCount: integer;
begin
  result := FTablesList.Count;
end;

function TDeMetadata.GetLibraryCount: Integer;
begin
  Result := FCacheList.Count;
end;

function TDeMetadata.GetLibraryData(const Index: Integer): TDataCache;
begin
  Result := TCacheData(FCacheList[Index]).Cache;
end;

procedure TDeMetadata.SetAgregateType(const aAgrigateType: TAgregateType);
begin
  FAgregateType := aAgrigateType;
end;

function TDeMetadata.GetFieldMetaDataSet: TDeDataSet;
begin
  Result := FNotifier.FieldMetaDataSet;
end;

procedure TDeMetadata.SetFieldMetaDataSet(const Value: TDeDataSet);
begin
  FNotifier.FieldMetaDataSet := Value;
end;

procedure TDeMetadata.SetNeedReloading(const Value: Boolean);
begin
  if Value and not FNeedReloading then
    begin
      FNeedReloading:= Value;
      SendMessage(Application.MainForm.Handle, DM_ATTENTION,
      NativeInt(VarToStr('Need Reloading Metadata')),
      NativeInt(VarToStr('Need Reloading Metadata')) );
    end;
end;

function TDeMetadata.GetActionMetaDataSet: TDeDataSet;
begin
  Result := FNotifier.ActionMetaDataSet;
end;

function TDeMetadata.GetConfigDB: TDeCustomDatabase;
begin
  if DatabasesCount = 0 then Result:= nil
                        else Result:= databases[0];
end;

procedure TDeMetadata.SetActionMetaDataSet(const Value: TDeDataSet);
begin
  FNotifier.ActionMetaDataSet := Value;
end;

function TDeMetadata.UpdateDataSets(const DataSetID: Variant; const Changed: TTypeChanged; const Key: Variant; const LocateIndex: Integer = ltNone): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to Pred(DataSetsCount) do
    if DataSets[Index].Table.ID = DataSetID then
      begin
        DataSets[Index].Cache.Update(Changed, Key, LocateIndex);
        Inc(Result);
      end;
  if Assigned(ProfitActions) and (MetaTables[idxCommands].ID = DataSetID) then
    begin
      Index := VarToInt(Key);
      ProfitActions.UpdateData(Index, Changed);
    end;
end;

function TDeMetadata.UpdateDataSets(const TableName: string; const Changed: TTypeChanged; const Key: Variant; const LocateIndex: Integer = ltNone): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to Pred(DataSetsCount) do
    if SameText(DataSets[Index].Table.Table, TableName) then
      begin
        DataSets[Index].Cache.Update(Changed, Key, LocateIndex);
        Inc(Result);
      end;
  if Assigned(ProfitActions) and SameText(MetaTables[idxCommands].Table, TableName) then
    begin
      Index := VarToInt(Key);
      ProfitActions.UpdateData(Index, Changed);
    end;
end;

function TDeMetadata.UpdateDataSets(const DataSetGUID: TGUID; const Changed: TTypeChanged; const Key: Variant; const LocateIndex: Integer = ltNone): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to Pred(DataSetsCount) do
    if tsGUID in DataSets[Index].Table.LoadingState then
      if IsEqualGUID(DataSets[Index].Table.GUID, DataSetGUID) then
        begin
          DataSets[Index].Cache.Update(Changed, Key, LocateIndex);
          Inc(Result);
        end;
  if Assigned(ProfitActions) and IsEqualGUID(MetaTables[idxCommands].GUID, DataSetGUID) then
    begin
      Index := VarToInt(Key);
      ProfitActions.UpdateData(Index, Changed);
    end;
end;

function TDeMetadata.GetDynamicTableByName(const TableName: string; Database: TDeCustomDatabase): TTableMeta;
var
  TableList: TStringList;
  TableIndex, FieldIndex, LeftIndex, RightIndex: Integer;
  Value, TagValue, ProcName, FieldName: string;
  FieldMeta: TFieldMeta;
  function CheckFieldTypeString(const Name: string): Boolean;
  var
    Value: string;
    Index: Integer;
  begin
    Value := TagValue;
    Result := Pos(Name, LowerCase(Value)) = 1;
    if Result then
      begin
        FieldMeta.DataType := ftString;
        Value := Trim(Copy(Value, Succ(Length(Name)), Length(Value)));
        Index := Length(Value);
        if Index <> 0 then
          if (Index > 2) and (Value[1] = '(') and (Value[Index] = ')') then
            begin
              Value := Trim(Copy(Value, 2, Length(Value) - 2));
              if TryStrToInt(Value, Index) then
                FieldMeta.DataSize := Index
              else
                begin
                  FieldMeta.DataSize := 255;
                  {$IFDEF DeDEBUG}
                  Funcs.WriteLog('String field ' + QuotedStr(FieldMeta.Original) + ' for stored procedure ' + QuotedStr(ProcName) + ': invalid data size definition ' + QuotedStr(TagValue) + '!!! Use default data size 255.');
                  {$ENDIF}
                end;
            end
          else
            begin
              FieldMeta.DataSize := 255;
              {$IFDEF DeDEBUG}
              Funcs.WriteLog('String field ' + QuotedStr(FieldMeta.Original) + ' for stored procedure ' + QuotedStr(ProcName) + ': incorrected data type and size definition ' + QuotedStr(TagValue) + '!!! Use default data size 255.');
              {$ENDIF}
            end
        else
          begin
            FieldMeta.DataSize := 255;
            {$IFDEF DeDEBUG}
            Funcs.WriteLog('String field ' + QuotedStr(FieldMeta.Original) + ' for stored procedure ' + QuotedStr(ProcName) + ': data size not defined!!! Use default data size 255.');
            {$ENDIF}
          end;
      end;
  end;
begin
  Result := nil;
  if Assigned(Database) then
    begin
      TableList := TStringList.Create;
      try
        Database.RetrieveTableNames(TableList);
        TableIndex := TableList.IndexOf(TableName);
      finally
        TableList.Free;
      end;
      if TableIndex <> -1 then
        begin
          Result := TTableMeta.Create(TableName, Database);
          try
            Result.IsDynamic := True;
            TableIndex := FTablesList.Add(Result);
            Inc(FMaxTableID);
            Result.ID := -FMaxTableID;
          finally
            if TableIndex = -1 then FreeAndNil(Result);
          end;
        end
      else if Database.CanStoredProcedure then
        begin
          Value := Trim(TableName);
          ProcName := EmptyStr;
          LeftIndex := Pos('(', Value);
          if LeftIndex <> 0 then
            begin
              RightIndex := Length(Value);
              if Value[RightIndex] = ')' then
                begin
                  ProcName := Trim(Copy(Value, 1, Pred(LeftIndex)));
                  if Length(ProcName) <> 0 then
                    begin
                      Value := Trim(Copy(Value, Succ(LeftIndex), Pred(RightIndex - LeftIndex)));
                      if Length(Value) = 0 then
                        begin
                          ProcName := EmptyStr;
                          {$IFDEF DeDEBUG}
                          Funcs.WriteLog('Skip find and check stored procedure ' + QuotedStr(TableName) + ': empty fields definition!!!');
                          {$ENDIF}
                        end;
                    end;
                end;
            end;
          if (Length(ProcName) <> 0) and Database.StoredProcedureExists(ProcName) then
            begin
              TableIndex := -1;
              Result := TTableMeta.Create(ProcName, Database);
              try
                Result.IsDynamic := True;
                Result.ObjectType := otStoredProc;

                Database.RetrieveProcedureParameters(ProcName, Result.Parameters);
                Result.SelectSQL := Database.RetrieveProcedureExecQuery(ProcName, Result.Parameters);

                while Length(Value) <> 0 do
                  begin
                    TagValue := Trim(CutTextValue(Value, ','));
                    FieldName := Trim(CutTextValue(TagValue, ' '));
                    if Length(FieldName) <> 0 then
                      begin
                        TagValue := Trim(TagValue);
                        FieldIndex := -1;
                        FieldMeta := TFieldMeta.Create;
                        try
                          FieldMeta.Original := FieldName;
                          FieldMeta.IsStored := True;
                          //if Result.Fields.Count = 0 then FieldMeta.Key := True;
                          if Length(TagValue) <> 0 then
                            if SameText(TagValue, 'int') then
                              FieldMeta.DataType := ftInteger
                            else if SameText(TagValue, 'date') then
                              FieldMeta.DataType := ftDate
                            else if SameText(TagValue, 'time') then
                              FieldMeta.DataType := ftTime
                            else if SameText(TagValue, 'datetime') then
                              FieldMeta.DataType := ftDateTime
                            else if SameText(TagValue, 'bigint') then
                              FieldMeta.DataType := ftLargeint
                            else if SameText(TagValue, 'float') then
                              FieldMeta.DataType := ftFloat
                            else if SameText(TagValue, 'money') then
                              FieldMeta.DataType := ftCurrency
                            else if CheckFieldTypeString('char') or CheckFieldTypeString('varchar') then
                              begin
                                // Ничего не делаем, т.к. всё уже сделано в функции!
                              end
                            else
                              begin
                                {$IFDEF DeDEBUG}
                                Funcs.WriteLog('Field ' + QuotedStr(FieldMeta.Original) +
                                  ' for stored procedure ' + QuotedStr(ProcName) +
                                  ': unknown type ' + QuotedStr(TagValue) + '!!!');
                                {$ENDIF}
                              end;
                          FieldIndex := Result.Fields.Add(FieldMeta);
                        finally
                          if FieldIndex = -1 then FieldMeta.Free;
                        end;
                      end;
                  end;
                TableIndex := FTablesList.Add(Result);
                Inc(FMaxTableID);
                Result.ID := -FMaxTableID;
              finally
                if TableIndex = -1 then FreeAndNil(Result);
              end;
            end;
        end;
    end;
end;

function TDeMetadata.GenerateDynamicFieldID: Integer;
var
  Query: TDeDataset;
begin
  if FMaxFieldID = 0 then
    try
      Query := MetadataDB.CreateQuery(qtSelect);
      try
        Query.Descr.BeginUpdate;
        try
          Query.Descr.Table := tblFields;
          Query.Descr.AddField(opMax, fldFieldsID);
        finally
          Query.Descr.EndUpdate;
        end;
        Query.Open;
        FMaxFieldID := Query.Value[0];
      finally
        Query.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Generate dynamic field ID skip error: ' + E.Message);
          {$ENDIF}
        end;
    end;
  Inc(FMaxFieldID);
  Result := FMaxFieldID;
end;

function TDeMetadata.CheckSignatureFromClipboard: Boolean;
var
  Handle: THandle;
  DataPtr: PAnsiChar;
  Template: TDeTemplate;
  Node: TDeNode;
begin
  Result := Clipboard.HasFormat(DeXML);
  if Result then
    begin
      Result := Assigned(MetadataDB);
      if Result then
        begin
          Template := TDeTemplate.Create;
          try
            Handle := Clipboard.GetAsHandle(DeXML);
            DataPtr := GlobalLock(Handle);
            try
              Template.Text := StrPas(DataPtr);
            finally
              GlobalUnlock(Handle);
            end;
            Result := Assigned(Template.Root);
            if Result then
              begin
                Node := Template.Root.FindNode('dataset');
                Result := Assigned(Node);
                if Result then
                  Result := SameText(Node.Attributes['server'], MetadataDB.Server) and
                    SameText(Node.Attributes['database'], MetadataDB.Database) and
                    SameText(Node.Attributes['login'], MetadataDB.Login);
              end;
          finally
            Template.Free;
          end;
        end;
    end;
end;

function TDeMetadata.PrepareBooleanXML(const AttrName: string; const Value: Variant; const Inversed: Boolean): string;
const
  Values: array[Boolean] of Char = ('N', 'Y');
var
  BoolValue: Boolean;
begin
  Result := EmptyStr;
  if not VarIsNull(Value) then
    case VarType(Value) of
      varShortInt, varSmallInt, varInteger, varByte, varWord, varLongWord, varInt64, varUInt64:
        begin
          BoolValue := Value = 1;
          if Inversed then BoolValue := not BoolValue;
          Result := Format(' %s="%s"', [AttrName, Values[BoolValue]]);
        end;
      varBoolean:
        begin
          BoolValue := Value;
          if Inversed then BoolValue := not BoolValue;
          Result := Format(' %s="%s"', [AttrName, Values[BoolValue]]);
        end;
    else
      if DeTryStrToBoolean(VarToStr(Value), BoolValue) then
        begin
          if Inversed then BoolValue := not BoolValue;
          Result := Format(' %s="%s"', [AttrName, Values[BoolValue]]);
        end;
    end;
end;

function TDeMetadata.PrepareMetadataDictionaryXML(const DatasetName: string; const Level: Integer): string;
var
  R, Index: Integer;
  SpaceString, LineString, AttrString, Value: string;
  Dataset: TDeDataset;
begin
  Result := EmptyStr;
  if Assigned(MetadataDB) then
    begin
      if Level >= 0 then
        begin
          if Level = 0 then
            SpaceString := EmptyStr
          else
            SpaceString := DupeString('  ', Level);
          LineString := #13#10;
        end
      else
        begin
          SpaceString := EmptyStr;
          LineString := EmptyStr;
        end;
      Dataset := MetadataDB.CreateQuery(qtSelect);
      try
        Dataset.Descr.BeginUpdate;
        try
          Dataset.Descr.Table := DatasetName;
        finally
          Dataset.Descr.EndUpdate;
        end;
        Dataset.Open;
        for R:= 0 to Pred(Dataset.RecordCount) do
          begin
            Dataset.RecNo:= R;
            Index := Dataset.IndexByName('ID');
            if (Index <> -1) and not VarIsNull(DataSet.Value[Index]) then
              begin
                Value := VarToStr(DataSet.Value[Index]);
                if Length(Value) <> 0 then
                  AttrString := Format(' identity="%s"', [XMLEncode(Value)]);
              end
            else
              AttrString := EmptyStr;
            Index := Dataset.IndexByName('NAME');
            if Index <> -1 then
              Value := Trim(UTF8ToString(RawByteString((DataSet.Value[Index]))))
            else
              Value := EmptyStr;
            if Length(Value) = 0 then
              begin
                if Length(AttrString) <> 0 then
                  Result := Result + LineString + SpaceString + '<record' + AttrString + ' />';
              end
            else
              Result := Result + LineString + SpaceString + '<record' + AttrString + '>' + XMLEncode(Value) + '</record>';
          end;
      finally
        Dataset.Free;
      end;
    end;
end;

function TDeMetadata.PrepareMetadataLinkXML(const DatasetID, FieldID, Level: Integer): string;
var
  Index: Integer;
  SpaceString, LineString, AttrString, Value: string;
  Dataset: TDeDataset;
begin
  Result := EmptyStr;
  if Assigned(MetadataDB) then
    begin
      if Level >= 0 then
        begin
          if Level = 0 then
            SpaceString := EmptyStr
          else
            SpaceString := DupeString('  ', Level);
          LineString := #13#10;
        end
      else
        begin
          SpaceString := EmptyStr;
          LineString := EmptyStr;
        end;
      Dataset := MetadataDB.CreateQuery(qtRow);
      try
        Dataset.Descr.BeginUpdate;
        try
          Dataset.Descr.Table := tblDataset;
          Dataset.Descr.AddParamCondition(fldDataSetID, ftInteger, opEQ, 'ID', DatasetID);
        finally
          Dataset.Descr.EndUpdate;
        end;
        Dataset.Open;
        Index := Dataset.IndexByName(fldDataSetGUID);
        if Index <> -1 then
          begin
            Value := VarToStr(DataSet.Value[Index]);
            if Length(Value) <> 0 then
              AttrString := Format(' id="%s"', [XMLEncode(Value)]);
          end
        else
          AttrString := EmptyStr;
        AttrString := AttrString + Format(' identity="%d"', [DatasetID]);
      finally
        Dataset.Free;
      end;
      if FieldID <> 0 then
        begin
          Dataset := MetadataDB.CreateQuery(qtRow);
          try
            Dataset.Descr.BeginUpdate;
            try
              Dataset.Descr.Table := tblFields;
              Dataset.Descr.AddParamCondition(fldFieldsID, ftInteger, opEQ, 'ID', FieldID);
            finally
              Dataset.Descr.EndUpdate;
            end;
            Dataset.Open;
            Index := Dataset.IndexByName(fldFieldsGUID);
            if Index <> -1 then
              begin
                Value := VarToStr(DataSet.Value[Index]);
                if Length(Value) <> 0 then
                  Value := Format(' id="%s"', [XMLEncode(Value)]);
              end
            else
              Value := EmptyStr;
            Value := '<field' + Value + Format(' identity="%d"', [FieldID]) + ' />';
            if Level >= 0 then
              Value := LineString + SpaceString + '  ' + Value;
            Result := Result + Value;
          finally
            Dataset.Free;
          end;
        end;
      if Length(Result) = 0 then
        Result := LineString + SpaceString + '<link' + AttrString + ' />'
      else
        Result := LineString + SpaceString + '<link' + AttrString + '>' + Result +
                  LineString + SpaceString + '</link>';
    end;
end;

function TDeMetadata.PrepareMetadataFieldXML(const DatasetID, FieldID, Level: Integer): string;
var
  Index, NextLevel: Integer;
  SpaceString, LineString, AttrString, Value: string;
  Dataset: TDeDataset;
  function PrepareViewXML: string;
  var
    Index: Integer;
    Value, AttrString: string;
  begin
    Result := EmptyStr;
    AttrString := EmptyStr;
    Index := Dataset.IndexByName(fldFieldsShowType);
    if (Index <> -1) and not VarIsNull(DataSet.Value[Index]) then
      begin
        Index := VarToInt(DataSet.Value[Index]);
        AttrString := Format(' type="%d"', [Index]);
      end;
    Index := Dataset.IndexByName(fldFieldsVisible);
    if (Index <> -1) and not VarIsNull(DataSet.Value[Index]) then
      begin
        Index := VarToInt(DataSet.Value[Index]);
        AttrString := AttrString + Format(' visible="%d"', [Index]);
      end;
    Index := Dataset.IndexByName(fldFieldsName);
    if Index <> -1 then
      begin
        Value := Trim(UTF8ToString(RawByteString((DataSet.Value[Index]))));
        if Length(Value) <> 0 then
          AttrString := AttrString + Format(' caption="%s"', [XMLEncode(Value)]);
      end;
    Index := Dataset.IndexByName(fldFieldsWidth);
    if (Index <> -1) and not VarIsNull(DataSet.Value[Index]) then
      begin
        Index := VarToInt(DataSet.Value[Index]);
        AttrString := AttrString + Format(' width="%d"', [Index]);
      end;
    Index := Dataset.IndexByName(fldFieldsMaxWidth);
    if (Index <> -1) and not VarIsNull(DataSet.Value[Index]) then
      begin
        Index := VarToInt(DataSet.Value[Index]);
        AttrString := AttrString + Format(' maxwidth="%d"', [Index]);
      end;
    Index := Dataset.IndexByName(fldFieldsTemplate);
    if Index <> -1 then
      begin
        Value := Trim(UTF8ToString(RawByteString((DataSet.Value[Index]))));
        if Length(Value) <> 0 then
          begin
            Value := '<template>' + XMLEncode(Value) + '</template>';
            if Level >= 0 then
              Result := LineString + SpaceString + '    ' + Value
            else
              Result := Value;
          end;
      end;
    if Length(Result) = 0 then
      begin
        if Length(AttrString) <> 0 then
          if Level >= 0 then
            Result := LineString + SpaceString + '  <view' + AttrString + ' />'
          else
            Result := '<view' + AttrString + ' />';
      end
    else
      if Level >= 0 then
        Result := LineString + SpaceString + '  <view' + AttrString + '>' + Result + '</view>'
      else
        Result := '<view' + AttrString + '>' + Result + '</view>';
  end;
  function PrepareValueXML(const FieldName, TypeName: string): string;
  var
    Index: Integer;
    Value: string;
  begin
    Result := EmptyStr;
    Index := Dataset.IndexByName(FieldName);
    if Index <> -1 then
      begin
        Value := UTF8ToString(RawByteString((DataSet.Value[Index])));
        if Length(Value) <> 0 then
          begin
            if Level >= 0 then
              Result := LineString + SpaceString + '  ';
            Result := Result + '<value type="' + XMLEncode(TypeName) + '">' + XMLEncode(Value) + '</value>';
          end;
      end;
  end;
begin
  Result := EmptyStr;
  if Assigned(MetadataDB) then
    begin
      NextLevel := Level;
      if Level >= 0 then
        begin
          Inc(NextLevel);
          if Level = 0 then
            SpaceString := EmptyStr
          else
            SpaceString := DupeString('  ', Level);
          LineString := #13#10;
        end
      else
        begin
          SpaceString := EmptyStr;
          LineString := EmptyStr;
        end;
      Dataset := MetadataDB.CreateQuery(qtRow);
      try
        Dataset.Descr.BeginUpdate;
        try
          Dataset.Descr.Table := tblFields;
          Dataset.Descr.AddParamCondition(fldFieldsID, ftInteger, opEQ, 'ID', FieldID);
        finally
          Dataset.Descr.EndUpdate;
        end;
        Dataset.Open;
        AttrString := EmptyStr;
        Index := Dataset.IndexByName(fldFieldsGUID);
        if Index <> -1 then
          begin
            Value := VarToStr(DataSet.Value[Index]);
            if Length(Value) <> 0 then
              AttrString := Format(' id="%s"', [XMLEncode(Value)]);
          end;
        Index := Dataset.IndexByName(fldFieldsID);
        if (Index <> -1) and not VarIsNull(DataSet.Value[Index]) then
          begin
            Value := VarToStr(DataSet.Value[Index]);
            if Length(Value) <> 0 then
              AttrString := AttrString + Format(' identity="%s"', [XMLEncode(Value)]);
          end;
        Index := Dataset.IndexByName(fldFieldsOriginal);
        if Index <> -1 then
          begin
            Value := Trim(UTF8ToString(RawByteString((DataSet.Value[Index]))));
            if Length(Value) <> 0 then
              AttrString := AttrString + Format(' name="%s"', [XMLEncode(Value)]);
          end;
        Index := Dataset.IndexByName(fldFieldsCharset);
        if (Index <> -1) and not VarIsNull(DataSet.Value[Index]) then
          begin
            Index := VarToInt(DataSet.Value[Index]);
            AttrString := AttrString + Format(' charset="%d"', [Index]);
          end;
        Index := Dataset.IndexByName(fldFieldsOrder);
        if (Index <> -1) and not VarIsNull(DataSet.Value[Index]) then
          begin
            Index := VarToInt(DataSet.Value[Index]);
            AttrString := AttrString + Format(' order="%d"', [Index]);
          end;
        Index := Dataset.IndexByName(fldFieldsRole);
        if Index <> -1 then
          begin
            Index := VarToInt(DataSet.Value[Index]);
            if Index <> 0 then
              AttrString := AttrString + Format(' role="%d"', [Index]);
          end;
        Index := Dataset.IndexByName(fldFieldsDataType);
        if Index <> -1 then
          begin
            Index := VarToInt(DataSet.Value[Index]);
            if Index <> 0 then
              AttrString := AttrString + Format(' datatype="%d"', [Index]);
          end;
        Index := Dataset.IndexByName(fldFieldsDataSize);
        if Index <> -1 then
          begin
            Index := VarToInt(DataSet.Value[Index]);
            if Index <> 0 then
              AttrString := AttrString + Format(' datasize="%d"', [Index]);
          end;
        Index := Dataset.IndexByName(fldFieldsCategory);
        if Index <> -1 then
          begin
            Index := VarToInt(DataSet.Value[Index]);
            if Index <> 0 then
              AttrString := AttrString + Format(' category="%d"', [Index]);
          end;

        //   *******

        Index := Dataset.IndexByName(fldFieldsKey);
        if Index <> -1 then
          AttrString := AttrString + PrepareBooleanXML('key', DataSet.Value[Index]);
        Index := Dataset.IndexByName(fldFieldsNotNull);
        if Index <> -1 then
          AttrString := AttrString + PrepareBooleanXML('nullable', DataSet.Value[Index], True);
        Index := Dataset.IndexByName(fldFieldsUnique);
        if Index <> -1 then
          AttrString := AttrString + PrepareBooleanXML('unique', DataSet.Value[Index]);
        Index := Dataset.IndexByName(fldFieldsReadOnly);
        if Index <> -1 then
          AttrString := AttrString + PrepareBooleanXML('readonly', DataSet.Value[Index]);
        Index := Dataset.IndexByName(fldFieldsCalculated);
        if Index <> -1 then
          AttrString := AttrString + PrepareBooleanXML('calculated', DataSet.Value[Index]);
        Index := Dataset.IndexByName(fldFieldsStored);
        if Index <> -1 then
          AttrString := AttrString + PrepareBooleanXML('stored', DataSet.Value[Index]);
        Index := Dataset.IndexByName(fldFieldsTable);
        if Index <> -1 then
          begin
            Index := VarToInt(DataSet.Value[Index]);
            if Index <> DatasetID then
               begin
                 if Level >= 0 then
                   Value := LineString + SpaceString + '  '
                 else
                   Value := EmptyStr;
                 Value := Value + Format('<!-- Conflict dataset in field %d and dataset in original field %d -->', [Index, DatasetID]);
                 Result := Value + Result;
               end;
          end;
        Index := Dataset.IndexByName(fldFieldsOnDelete);
        if Index <> -1 then
          begin
            Index := VarToInt(DataSet.Value[Index]);
            if Index <> 0 then
               begin
                 if Level >= 0 then
                   Value := LineString + SpaceString + '  '
                 else
                   Value := EmptyStr;
                 Value := Value + Format('<action type="delete" value="%d" />', [Index]);
                 Result := Value + Result;
               end;
          end;
        Index := Dataset.IndexByName(fldFieldsLinkRole);
        if Index <> -1 then
          begin
            Value := Trim(UTF8ToString(RawByteString(DataSet.Value[Index])));
            if Length(Value) <> 0 then
              begin
                Value := '<linkrole>' + XMLEncode(Value) + '</linkrole>';
                if Level >= 0 then
                  Result := Result + LineString + SpaceString + '  ' + Value
                else
                  Result := Result + Value;
              end;
          end;
        Result := Result + PrepareValueXML(fldFieldsValue1, 'false') +
                           PrepareValueXML(fldFieldsValue2, 'true') +
                           PrepareValueXML(fldFieldsDefault, 'default');
        Index := Dataset.IndexByName(fldFieldsLink);
        if Index <> -1 then
          begin
            Index := VarToInt(DataSet.Value[Index]);
            if Index <> 0 then
              Result := Result + PrepareMetadataLinkXML(Index, 0, NextLevel);
          end;
      finally
        Dataset.Free;
      end;
      if Length(Result) = 0 then
        Result := LineString + SpaceString + '<field' + AttrString + ' />'
      else
        Result := LineString + SpaceString + '<field' + AttrString + '>' + Result +
                  LineString + SpaceString + '</field>';
    end;
end;

function TDeMetadata.PrepareMetadataElementXML(const DatasetID, ElementID, Level: Integer): string;
var
  R, Index, NextLevel: Integer;
  SpaceString, LineString, AttrString, Value: string;
  ProcessElements: TStrings;
  Dataset: TDeDataset;
  function PrepareAnchorXML: string;
  var
    LeftIndex, RightIndex: Integer;
    Value: string;
  begin
    Result := EmptyStr;
    LeftIndex := Dataset.IndexByName(fldElemAL);
    if LeftIndex <> -1 then
      LeftIndex := VarToInt(Dataset.Value[LeftIndex])
    else
      LeftIndex := 0;
    RightIndex := Dataset.IndexByName(fldElemAR);
    if RightIndex <> -1 then
      RightIndex := VarToInt(Dataset.Value[RightIndex])
    else
      RightIndex := 0;
    if LeftIndex = 0 then
      Value := EmptyStr
    else
      Value := Format(' left="%d"', [LeftIndex]);
    if RightIndex <> 0 then
      Value := Value + Format(' right="%d"', [RightIndex]);
    if Length(Value) <> 0 then
      begin
        if Level >= 0 then
          Result := LineString + SpaceString + '  ';
        Result := Result + '<anchor' + Value + ' />';
      end;
  end;
  function PrepareExpressionXML(const FieldName, TypeName: string): string;
  var
    Index: Integer;
    Value: string;
  begin
    Result := EmptyStr;
    Index := Dataset.IndexByName(FieldName);
    if Index <> -1 then
      begin
        Value := UTF8ToString(RawByteString(DataSet.Value[Index]));
        if Length(Value) <> 0 then
          begin
            if Level >= 0 then
              Result := LineString + SpaceString + '  ';
            Result := Result + '<expression type="' + XMLEncode(TypeName) + '">' + XMLEncode(Value) + '</expression>';
          end;
      end;
  end;
  function PrepareSpecialXML: string;
  var
    Index: Integer;
    Value: string;
  begin
    Result := EmptyStr;
    Index := Dataset.IndexByName(fldElemXML);
    if Index <> -1 then
      begin
        Value := UTF8ToString(RawByteString(DataSet.Value[Index]));
        if Length(Value) <> 0 then
          begin
            if Level >= 0 then
              Result := LineString + SpaceString + '  ';
            Result := Result + '<xml>' + XMLEncode(Value) + '</xml>';
          end;
      end;
  end;
  function PrepareLinkXML: string;
  var
    DatasetIndex, FieldIndex: Integer;
  begin
    Result := EmptyStr;
    DatasetIndex := Dataset.IndexByName(fldElemLink);
    if DatasetIndex <> -1 then
      DatasetIndex := VarToInt(Dataset.Value[DatasetIndex])
    else
      DatasetIndex := 0;
    FieldIndex := Dataset.IndexByName(fldElemLinkField);
    if FieldIndex <> -1 then
      FieldIndex := VarToInt(Dataset.Value[FieldIndex])
    else
      FieldIndex := 0;
    if (DatasetIndex <> 0) or (FieldIndex <> 0) then
      Result := PrepareMetadataLinkXML(DatasetIndex, FieldIndex, NextLevel);
  end;
begin
  Result := EmptyStr;
  if Assigned(MetadataDB) then
    begin
      NextLevel := Level;
      if Level >= 0 then
        begin
          Inc(NextLevel);
          if Level = 0 then
            SpaceString := EmptyStr
          else
            SpaceString := DupeString('  ', Level);
          LineString := #13#10;
        end
      else
        begin
          SpaceString := EmptyStr;
          LineString := EmptyStr;
        end;
      ProcessElements := TStringList.Create;
      try
        Dataset := MetadataDB.CreateQuery(qtSelect);
        try
          Dataset.Descr.BeginUpdate;
          try
            Dataset.Descr.Table := tblElement;
            Dataset.Descr.AddField(fldElemID);
            Dataset.Descr.AddParamCondition(fldElemDeleted, ftInteger, opNE, 'DID', 1);
            Dataset.Descr.AddParamCondition(fldElemOwner, ftInteger, opEQ, 'ID', ElementID);
            Dataset.Descr.AddOperation(opAnd);
            Dataset.Descr.AddSortField(fldElemID);
          finally
            Dataset.Descr.EndUpdate;
          end;
          Dataset.Open(True);
          for R:=0 to Pred(Dataset.RecordCount) do
            begin
              Dataset.RecNo:= R;
              Value := IntToStr(VarToInt(Dataset.Value[0]));
              if ProcessElements.IndexOf(Value) = -1 then
                ProcessElements.Append(Value);
            end;
        finally
          Dataset.Free;
        end;
        for Index := 0 to Pred(ProcessElements.Count) do
          Result := Result + PrepareMetadataElementXML(DatasetID, StrToInt(ProcessElements[Index]), NextLevel);
        Dataset := MetadataDB.CreateQuery(qtRow);
        try
          Dataset.Descr.BeginUpdate;
          try
            Dataset.Descr.Table := tblElement;
            Dataset.Descr.AddParamCondition(fldElemID, ftInteger, opEQ, 'ID', ElementID);
          finally
            Dataset.Descr.EndUpdate;
          end;
          Dataset.Open;
          AttrString := EmptyStr;
          Index := Dataset.IndexByName(fldElemName);
          if Index <> -1 then
            begin
              Value := Trim(UTF8ToString(RawByteString(DataSet.Value[Index])));
              if Length(Value) <> 0 then
                AttrString := AttrString + Format(' name="%s"', [Value]);
            end;
          Index := Dataset.IndexByName(fldElemType);
          if Index <> -1 then
            begin
              Index := VarToInt(DataSet.Value[Index]);
              if Index <> 0 then
                AttrString := AttrString + Format(' control="%d"', [Index]);
            end;
          Index := Dataset.IndexByName(fldElemL);
          if Index <> -1 then
            AttrString := AttrString + Format(' left="%d"', [VarToInt(DataSet.Value[Index])]);
          Index := Dataset.IndexByName(fldElemT);
          if Index <> -1 then
            AttrString := AttrString + Format(' top="%d"', [VarToInt(DataSet.Value[Index])]);
          Index := Dataset.IndexByName(fldElemR);
          if Index <> -1 then
            AttrString := AttrString + Format(' right="%d"', [VarToInt(DataSet.Value[Index])]);
          Index := Dataset.IndexByName(fldElemH);
          if Index <> -1 then
            AttrString := AttrString + Format(' height="%d"', [VarToInt(DataSet.Value[Index])]);
          Index := Dataset.IndexByName(fldElemFont);
          if Index <> -1 then
            begin
              Index := VarToInt(DataSet.Value[Index]);
              if Index <> 0 then
                AttrString := AttrString + Format(' font="%d"', [Index]);
            end;
          Index := Dataset.IndexByName(fldElemColor);
          if Index <> -1 then
            begin
              Index := VarToInt(DataSet.Value[Index]);
              if Index <> 0 then
                AttrString := AttrString + Format(' color="%d"', [Index]);
            end;
          Index := Dataset.IndexByName(fldElemVisible);
          if Index <> -1 then
            AttrString := AttrString + PrepareBooleanXML('visible', DataSet.Value[Index]);
          Index := Dataset.IndexByName(fldElemReadOnly);
          if Index <> -1 then
            AttrString := AttrString + PrepareBooleanXML('readonly', DataSet.Value[Index]);
          Index := Dataset.IndexByName(fldElemDataSet);
          if Index <> -1 then
            begin
              Index := VarToInt(DataSet.Value[Index]);
              if Index <> DatasetID then
                 begin
                   if Level >= 0 then
                     Value := LineString + SpaceString + '  '
                   else
                     Value := EmptyStr;
                   Value := Value + Format('<!-- Conflict dataset in element %d and dataset in root element %d -->', [Index, DatasetID]);
                   Result := Value + Result;
                 end;
            end;
          Result := PrepareAnchorXML +
                    PrepareExpressionXML(fldElemExprName, 'name') +
                    PrepareExpressionXML(fldElemExprValue, 'value') +
                    PrepareExpressionXML(fldElemExprFilter, 'filter') +
                    PrepareExpressionXML(fldElemExprVisible, 'visible') +
                    PrepareExpressionXML(fldElemExprReadOnly, 'readonly') +
                    PrepareLinkXML +
                    PrepareSpecialXML +
                    Result;
        finally
          Dataset.Free;
        end;
        if Length(Result) = 0 then
          Result := LineString + SpaceString + '<element' + AttrString + ' />'
        else
          Result := LineString + SpaceString + '<element' + AttrString + '>' + Result +
                    LineString + SpaceString + '</element>';
      finally
        ProcessElements.Free;
      end;
    end;
end;

function TDeMetadata.PrepareMetadataCommandXML(const CommandID, Level: Integer; ZipFile: TZipFile): string;
var
  Index: Integer;
  SpaceString, LineString, AttrString, Value: string;
  Dataset: TDeDataset;
  function PrepareExpressionXML(const FieldName, TypeName: string): string;
  var
    Index: Integer;
    Value: string;
  begin
    Result := EmptyStr;
    Index := Dataset.IndexByName(FieldName);
    if Index <> -1 then
      begin
        Value := UTF8ToString(RawByteString(DataSet.Value[Index]));
        if Length(Value) <> 0 then
          begin
            if Level >= 0 then
              Result := LineString + SpaceString + '  ';
            Result := Result + '<expression type="' + XMLEncode(TypeName) + '">' + XMLEncode(Value) + '</expression>';
          end;
      end;
  end;
  function PrepareTextXML(const FieldName, TagName: string): string;
  var
    Index: Integer;
    Value: string;
  begin
    Result := EmptyStr;
    Index := Dataset.IndexByName(FieldName);
    if Index <> -1 then
      begin
        Value := UTF8ToString(RawByteString(DataSet.Value[Index]));
        if Length(Value) <> 0 then
          begin
            if Level >= 0 then
              Result := LineString + SpaceString + '  ';
            Result := Result + '<' + TagName + '>' + XMLEncode(Value) + '</' + TagName + '>';
          end;
      end;
  end;
  function PrepareImageXML: string;
  var
    Index: Integer;
    Image: TDeImage;
    FileName, RealName: string;
    Stream: TStream;
  begin
    Result := EmptyStr;
    Index := Dataset.IndexByName(fldCommandData);
    if (Index <> -1) and not VarIsNull(DataSet.Value[Index]) then
      begin
        Image := TDeImage.Create(nil);
        try
          Image.Value :=  DataSet.Value[Index];
          if Level >= 0 then
            Result := LineString + SpaceString + '  ';
          FileName := ExtractFileName(Image.OriginalFileName);
          Result := Result + '<file';
          if Length(Image.OriginalFileName) <> 0 then
            Result := Result + Format(' name="%s"', [XMLEncode(FileName)]);
          if Image.Version <> 0 then
            Result := Result + Format(' version="%u"', [Image.Version]);
          if Image.Modified <> 0 then
            Result := Result + Format(' modified="%s"', [FormatDateTime('YYYYMMDD" "HH":"NN":"SS"."ZZZ', Image.Modified)]);
          if Length(Image.Author) <> 0 then
            Result := Result + Format(' author="%s"', [XMLEncode(Image.Author)]);
          if Assigned(ZipFile) then
            begin
              FileName := GetApplicationDataDirectory + FileName;
              if Image.SaveToFile(FileName) then
                try
                  RealName := Format('%.4u\%s', [Succ(ZipFile.FileCount), ExtractFileName(FileName)]);
                  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
                  try
                    ZipFile.Add(Stream, RealName);
                  finally
                    Stream.Free;
                  end;
                  Result := Result + Format(' source="%s"', [XMLEncode(RealName)]);
                finally
                  DeleteFile(PChar(FileName));
                end;
            end;
          Result := Result + ' />';
        finally
          Image.Free;
        end;
      end;
  end;
begin
  Result := EmptyStr;
  if Assigned(MetadataDB) then
    begin
      if Level >= 0 then
        begin
          if Level = 0 then
            SpaceString := EmptyStr
          else
            SpaceString := DupeString('  ', Level);
          LineString := #13#10;
        end
      else
        begin
          SpaceString := EmptyStr;
          LineString := EmptyStr;
        end;
      Dataset := MetadataDB.CreateQuery(qtRow);
      try
        Dataset.Descr.BeginUpdate;
        try
          Dataset.Descr.Table := tblCommand;
          Dataset.Descr.AddParamCondition(fldCommandID, ftInteger, opEQ, 'ID', CommandID);
        finally
          Dataset.Descr.EndUpdate;
        end;
        Dataset.Open;
        AttrString := EmptyStr;
        Index := Dataset.IndexByName(fldCommandGUID);
        if Index <> -1 then
          begin
            Value := VarToStr(DataSet.Value[Index]);
            if Length(Value) <> 0 then
              AttrString := Format(' id="%s"', [XMLEncode(Value)]);
          end;
        Index := Dataset.IndexByName(fldCommandID);
        if (Index <> -1) and not VarIsNull(DataSet.Value[Index]) then
          begin
            Value := VarToStr(DataSet.Value[Index]);
            if Length(Value) <> 0 then
              AttrString := AttrString + Format(' identity="%s"', [XMLEncode(Value)]);
          end;
        Index := Dataset.IndexByName(fldCommandName);
        if Index <> -1 then
          begin
            Value := Trim(UTF8ToString(RawByteString(DataSet.Value[Index])));
            if Length(Value) <> 0 then
              AttrString := AttrString + Format(' name="%s"', [XMLEncode(Value)]);
          end;
        Index := Dataset.IndexByName(fldCommandOrder);
        if (Index <> -1) and not VarIsNull(DataSet.Value[Index]) then
          begin
            Index := VarToInt(DataSet.Value[Index]);
            AttrString := AttrString + Format(' order="%d"', [Index]);
          end;
        Index := Dataset.IndexByName(fldCommandType);
        if Index <> -1 then
          begin
            Index := VarToInt(DataSet.Value[Index]);
            if Index <> 0 then
              AttrString := AttrString + Format(' commandtype="%d"', [Index]);
          end;
        Index := Dataset.IndexByName(fldCommandActive);
        if Index <> -1 then
          AttrString := AttrString + PrepareBooleanXML('active', DataSet.Value[Index]);
        Index := Dataset.IndexByName(fldCommandBreak);
        if Index <> -1 then
          AttrString := AttrString + PrepareBooleanXML('break', DataSet.Value[Index]);
        Index := Dataset.IndexByName(fldCommandICO);
        if (Index <> -1) and not VarIsNull(DataSet.Value[Index]) then
          begin
            Index := VarToInt(DataSet.Value[Index]);
            AttrString := AttrString + Format(' icon="%d"', [Index]);
          end;
        Index := Dataset.IndexByName(fldCommandCategory);
        if Index <> -1 then
          begin
            Value := Trim(UTF8ToString(RawByteString(DataSet.Value[Index])));
            if Length(Value) <> 0 then
              AttrString := AttrString + Format(' category="%s"', [XMLEncode(Value)]);
          end;
        Index := Dataset.IndexByName(fldCommandShortCut);
        if Index <> -1 then
          begin
            Value := Trim(UTF8ToString(RawByteString(DataSet.Value[Index])));
            if Length(Value) <> 0 then
              AttrString := AttrString + Format(' shortcut="%s"', [XMLEncode(Value)]);
          end;
        Result := Result + PrepareExpressionXML(fldCommandOriginal, 'name') +
                           PrepareExpressionXML(fldCommandEnabled, 'enabled') +
                           PrepareExpressionXML(fldCommandVisible, 'visible') +
                           PrepareExpressionXML(fldCommandFile, 'file') +
                           PrepareExpressionXML(fldCommandParam, 'parameter') +
                           PrepareExpressionXML(fldCommandProcedure, 'procedure') +
                           PrepareImageXML +
                           PrepareTextXML(fldCommandQuestion, 'question') +
                           PrepareTextXML(fldCommandComment, 'comment');
      finally
        Dataset.Free;
      end;
      if Length(Result) = 0 then
        Result := LineString + SpaceString + '<command' + AttrString + ' />'
      else
        Result := LineString + SpaceString + '<command' + AttrString + '>' + Result +
                  LineString + SpaceString + '</command>';
    end;
end;

function TDeMetadata.PrepareMetadataDatasetXML(const DatasetID, Level: Integer; ZipFile: TZipFile): string;
var
  R, Index, NextLevel: Integer;
  SpaceString, LineString, AttrString, Value, DatasetName: string;
  ProcessDatasets: TStrings;
  Dataset: TDeDataset;
  function PrepareSQL(const FieldName, TypeName: string): string;
  var
    Index: Integer;
    Value: string;
  begin
    Result := EmptyStr;
    Index := Dataset.IndexByName(FieldName);
    if Index <> -1 then
      begin
        Value := UTF8ToString(RawByteString(DataSet.Value[Index]));
        if Length(Value) <> 0 then
          begin
            if Level >= 0 then
              Result := LineString + SpaceString + '  ';
            Result := Result + '<sql type="' + XMLEncode(TypeName) + '">' + XMLEncode(Value) + '</sql>';
          end;
      end;
  end;
  function PrepareViewXML: string;
  var
    TypeIndex, ValueIndex: Integer;
    Value: string;
  begin
    Result := EmptyStr;
    TypeIndex := Dataset.IndexByName(fldDataSetGViewType);
    ValueIndex := Dataset.IndexByName(fldDataSetGViewPars);
    if TypeIndex <> -1 then
      begin
        TypeIndex := VarToInt(DataSet.Value[TypeIndex]);
        if TypeIndex <> 0 then
          Result := Format(' type="%d"', [TypeIndex]);
        if ValueIndex <> -1 then
          begin
            Value := Trim(UTF8ToString(RawByteString(DataSet.Value[ValueIndex])));
            if Length(Value) <> 0 then
              begin
                if Level >= 0 then
                  Result := LineString + SpaceString + '  <view' + Result + '>'
                else
                  Result := '<view' + Result + '>';
                Result := Result + XMLEncode(Value) + '</view>';
              end
            else
              if Length(Result) <> 0 then
                if Level >= 0 then
                  Result := LineString + SpaceString + '  <view' + Result + ' />'
                else
                  Result := '<view' + Result + ' />';
          end
        else
          if Length(Result) <> 0 then
            if Level >= 0 then
              Result := LineString + SpaceString + '  <view' + Result + ' />'
            else
              Result := '<view' + Result + ' />';
      end
    else
      if ValueIndex <> -1 then
        begin
          Value := Trim(UTF8ToString(RawByteString(DataSet.Value[ValueIndex])));
          if Length(Value) <> 0 then
            begin
              if Level >= 0 then
                Result := LineString + SpaceString + '  ';
              Result := Result + '<view>' + XMLEncode(Value) + '</view>';
            end;
        end;
  end;
begin
  Result := EmptyStr;
  if Assigned(MetadataDB) then
    begin
      NextLevel := Level;
      if Level >= 0 then
        begin
          Inc(NextLevel);
          if Level = 0 then
            SpaceString := EmptyStr
          else
            SpaceString := DupeString('  ', Level);
          LineString := #13#10;
        end
      else
        begin
          SpaceString := EmptyStr;
          LineString := EmptyStr;
        end;
      ProcessDatasets := TStringList.Create;
      try
        Dataset := MetadataDB.CreateQuery(qtSelect);
        try
          Dataset.Descr.BeginUpdate;
          try
            Dataset.Descr.Table := tblDataset;
            Dataset.Descr.AddField(fldDataSetID);
            Dataset.Descr.AddParamCondition(fldDataSetDeleted, ftInteger, opNE, 'DID', 1);
            Dataset.Descr.AddParamCondition(fldDataSetParent, ftInteger, opEQ, 'ID', DatasetID);
            Dataset.Descr.AddOperation(opAnd);
            Dataset.Descr.AddSortField(fldDataSetID);
          finally
            Dataset.Descr.EndUpdate;
          end;
          Dataset.Open(True);
          for R:=0 to Pred(Dataset.RecordCount) do
            begin
              Dataset.RecNo:= R;
              Value := IntToStr(VarToInt(Dataset.Value[0]));
              if ProcessDatasets.IndexOf(Value) = -1 then
                ProcessDatasets.Append(Value);
            end;
        finally
          Dataset.Free;
        end;
        for Index := 0 to Pred(ProcessDatasets.Count) do
          Result := Result + PrepareMetadataDatasetXML(StrToInt(ProcessDatasets[Index]), NextLevel, ZipFile);
        Dataset := MetadataDB.CreateQuery(qtRow);
        try
          Dataset.Descr.BeginUpdate;
          try
            Dataset.Descr.Table := tblDataset;
            Dataset.Descr.AddParamCondition(fldDataSetID, ftInteger, opEQ, 'ID', DatasetID);
          finally
            Dataset.Descr.EndUpdate;
          end;
          Dataset.Open;
          Result := PrepareViewXML +
                    PrepareSQL(fldDataSetGener, 'sequence') +
                    PrepareSQL(fldDataSetSelect, 'select') +
                    PrepareSQL(fldDataSetInsert, 'insert') +
                    PrepareSQL(fldDataSetUpdate, 'update') +
                    PrepareSQL(fldDataSetDelete, 'delete') +
                    Result;
          AttrString := EmptyStr;
          Index := Dataset.IndexByName(fldDataSetGUID);
          if Index <> -1 then
            begin
              Value := VarToStr(DataSet.Value[Index]);
              if Length(Value) <> 0 then
                AttrString := Format(' id="%s"', [XMLEncode(Value)]);
            end;
          Index := Dataset.IndexByName(fldDataSetID);
          if (Index <> -1) and not VarIsNull(DataSet.Value[Index]) then
            begin
              Value := VarToStr(DataSet.Value[Index]);
              if Length(Value) <> 0 then
                AttrString := AttrString + Format(' identity="%s"', [XMLEncode(Value)]);
            end;
          Index := Dataset.IndexByName(fldDataSetTable);
          if Index <> -1 then
            begin
              Value := UTF8ToString(RawByteString(DataSet.Value[Index]));
              DatasetName := Value;
              if Length(Value) <> 0 then
                AttrString := AttrString + Format(' table="%s"', [XMLEncode(Value)]);
            end
          else
            DatasetName := EmptyStr;
          Index := Dataset.IndexByName(fldDataSetICO);
          if (Index <> -1) and not VarIsNull(DataSet.Value[Index]) then
            begin
              Index := VarToInt(DataSet.Value[Index]);
              AttrString := AttrString + Format(' icon="%d"', [Index]);
            end;
          Index := Dataset.IndexByName(fldDataSetReadOnly);
          if Index <> -1 then
            AttrString := AttrString + PrepareBooleanXML('readonly', DataSet.Value[Index]);
          Index := Dataset.IndexByName(fldDataSetIsList);
          if Index <> -1 then
            AttrString := AttrString + PrepareBooleanXML('list', DataSet.Value[Index]);
          Index := Dataset.IndexByName(fldDataSetOnAddMenue);
          if Index <> -1 then
            AttrString := AttrString + PrepareBooleanXML('menu', DataSet.Value[Index]);
          Index := Dataset.IndexByName(fldDataSetName);
          if Index <> -1 then
            begin
              Value := UTF8ToString(RawByteString(DataSet.Value[Index]));
              if Length(Value) <> 0 then
                AttrString := AttrString + Format(' name="%s"', [XMLEncode(Value)]);
            end;
          Index := Dataset.IndexByName(fldDataSetCaption);
          if Index <> -1 then
            begin
              Value := UTF8ToString(RawByteString(DataSet.Value[Index]));
              if Length(Value) <> 0 then
                AttrString := AttrString + Format(' caption="%s"', [XMLEncode(Value)]);
            end;
          Index := Dataset.IndexByName(fldDataSetDescription);
          if Index <> -1 then
            begin
              Value := UTF8ToString(RawByteString(DataSet.Value[Index]));
              if Length(Value) <> 0 then
                AttrString := AttrString + Format(' description="%s"', [XMLEncode(Value)]);
            end;
          Index := Dataset.IndexByName(fldDataSetFilter);
          if Index <> -1 then
            begin
              Value := UTF8ToString(RawByteString(DataSet.Value[Index]));
              if Length(Value) <> 0 then
                Result := LineString + SpaceString + '  <filter>' + XMLEncode(Value) + '</filter>' + Result;
            end;
        finally
          Dataset.Free;
        end;
        Dataset := MetadataDB.CreateQuery(qtSelect);
        try
          Dataset.Descr.BeginUpdate;
          try
            Dataset.Descr.Table := tblFields;
            Dataset.Descr.AddField(fldFieldsID);
            Dataset.Descr.AddParamCondition(fldFieldsDeleted, ftInteger, opNE, 'DID', 1);
            Dataset.Descr.AddParamCondition(fldFieldsTable, ftInteger, opEQ, 'TID', DatasetID);
            Dataset.Descr.AddOperation(opAnd);
            Dataset.Descr.AddSortFields([fldFieldsOrder, fldFieldsID]);
          finally
            Dataset.Descr.EndUpdate;
          end;
          Dataset.Open(True);
          for R:=0 to Pred(Dataset.RecordCount)do
            begin
              Dataset.RecNo:= R;
              Result := Result + PrepareMetadataFieldXML(DatasetID, VarToInt(Dataset.Value[0]), NextLevel);
            end;
        finally
          Dataset.Free;
        end;
        Dataset := MetadataDB.CreateQuery(qtSelect);
        try
          Dataset.Descr.BeginUpdate;
          try
            Dataset.Descr.Table := tblElement;
            Dataset.Descr.AddField(fldElemID);
            Dataset.Descr.AddParamCondition(fldElemDeleted, ftInteger, opNE, 'DID', 1);
            Dataset.Descr.AddParamCondition(fldElemDataSet, ftInteger, opEQ, 'TID', DatasetID);
            Dataset.Descr.AddCondition(fldElemOwner, ftInteger, opIs, Null);
            Dataset.Descr.AddParamCondition(fldElemOwner, ftInteger, opEQ, 'ID', 0);
            Dataset.Descr.AddOperation(opOr);
            Dataset.Descr.AddOperation(opAnd);
            Dataset.Descr.AddOperation(opAnd);
            Dataset.Descr.AddSortField(fldElemID);
          finally
            Dataset.Descr.EndUpdate;
          end;
          Dataset.Open(True);
          for R:=0 to Pred(Dataset.RecordCount) do
            begin
              Dataset.RecNo:= R;
              Result := Result + PrepareMetadataElementXML(DatasetID, VarToInt(Dataset.Value[0]), NextLevel);
            end;
        finally
          Dataset.Free;
        end;
        if (Length(DatasetName) <> 0) and (Dataset.Database = FCatalogsDatabase) then
            Result := Result + PrepareMetadataDictionaryXML(DatasetName, NextLevel);
        Dataset := MetadataDB.CreateQuery(qtSelect);
        try
          Dataset.Descr.BeginUpdate;
          try
            Dataset.Descr.Table := tblCommand;
            Dataset.Descr.AddField(fldCommandID);
            Dataset.Descr.AddParamCondition(fldCommandDeleted, ftInteger, opNE, 'DID', 1);
            Dataset.Descr.AddParamCondition(fldCommandDataSet, ftInteger, opEQ, 'TID', DatasetID);
            Dataset.Descr.AddOperation(opAnd);
            Dataset.Descr.AddSortFields([fldCommandOrder, fldCommandID]);
          finally
            Dataset.Descr.EndUpdate;
          end;
          Dataset.Open(True);
          for R:=0 to Pred(Dataset.RecordCount) do
            begin
              Dataset.RecNo:= R;
              Result := Result + PrepareMetadataCommandXML(VarToInt(Dataset.Value[0]), NextLevel, ZipFile);
            end;
        finally
          Dataset.Free;
        end;
        if Length(Result) = 0 then
          Result := LineString + SpaceString + '<dataset' + AttrString + ' />'
        else
          Result := LineString + SpaceString + '<dataset' + AttrString + '>' + Result +
                    LineString + SpaceString + '</dataset>';
      finally
        ProcessDatasets.Free;
      end;
    end;
end;

function TDeMetadata.PrepareMetadataDatabaseXML(const DatabaseID, Level: Integer; ZipFile: TZipFile): string;
var
  R, Index, NextLevel: Integer;
  SpaceString, LineString, AttrString, Value: string;
  ProcessDatasets: TStrings;
  Dataset: TDeDataset;
begin
  Result := EmptyStr;
  if Assigned(MetadataDB) then
    begin
      NextLevel := Level;
      if Level >= 0 then
        begin
          Inc(NextLevel);
          if Level = 0 then
            SpaceString := EmptyStr
          else
            SpaceString := DupeString('  ', Level);
          LineString := #13#10;
        end
      else
        begin
          SpaceString := EmptyStr;
          LineString := EmptyStr;
        end;
      ProcessDatasets := TStringList.Create;
      try
        Dataset := MetadataDB.CreateQuery(qtSelect);
        try
          Dataset.Descr.BeginUpdate;
          try
            Dataset.Descr.Table := tblDataset;
            Dataset.Descr.AddField(fldDataSetID);
            Dataset.Descr.AddParamCondition(fldDataSetDeleted, ftInteger, opNE, 'DID', 1);
            Dataset.Descr.AddParamCondition(fldDataSetDatabase, ftInteger, opEQ, 'BID', DatabaseID);
            Dataset.Descr.AddCondition(fldDataSetParent, ftInteger, opIs, Null);
            Dataset.Descr.AddParamCondition(fldDataSetParent, ftInteger, opEQ, 'PID', 0);
            Dataset.Descr.AddOperation(opOr);
            Dataset.Descr.AddOperation(opAnd);
            Dataset.Descr.AddOperation(opAnd);
            Dataset.Descr.AddSortField(fldDataSetID);
          finally
            Dataset.Descr.EndUpdate;
          end;
          Dataset.Open(True);
          for R:=0 to Pred(Dataset.RecordCount) do
            begin
              Dataset.RecNo:= R;
              Value := IntToStr(VarToInt(Dataset.Value[0]));
              if ProcessDatasets.IndexOf(Value) = -1 then
                ProcessDatasets.Append(Value);
            end;
        finally
          Dataset.Free;
        end;
        for Index := 0 to Pred(ProcessDatasets.Count) do
          Result := Result + PrepareMetadataDatasetXML(StrToInt(ProcessDatasets[Index]), NextLevel, ZipFile);
        Dataset := MetadataDB.CreateQuery(qtRow);
        try
          Dataset.Descr.BeginUpdate;
          try
            Dataset.Descr.Table := tblBase;
            Dataset.Descr.AddParamCondition(fldBaseID, ftInteger, opEQ, 'ID', DatabaseID);
          finally
            Dataset.Descr.EndUpdate;
          end;
          Dataset.Open;
          if MetadataDB.ID = DatabaseID then
            begin
              AttrString := Format(' type="system" charset="65001" base="%d"', [Ord(MetadataDB.DatabaseType)]);
              Value := MetadataDB.Server;
              if Length(Value) <> 0 then
                AttrString := AttrString + Format(' server="%s"', [XMLEncode(Value)]);
              Value := MetadataDB.Database;
              if Length(Value) <> 0 then
                AttrString := AttrString + Format(' database="%s"', [XMLEncode(Value)]);
              Value := MetadataDB.Login;
              if Length(Value) <> 0 then
                AttrString := AttrString + Format(' login="%s"', [XMLEncode(Value)]);
              Value := MetadataDB.InternalConnectString;
              if Length(Value) <> 0 then
                AttrString := AttrString + Format(' path="%s"', [XMLEncode(Value)]);
              Value := MetadataDB.Alias;
              if Length(Value) <> 0 then
                AttrString := AttrString + Format(' alias="%s"', [XMLEncode(Value)]);
            end
          else
            begin
              AttrString := EmptyStr;
              Index := Dataset.IndexByName(fldBaseCharset);
              if Index <> -1 then
                begin
                  Index := VarToInt(DataSet.Value[Index]);
                  if Index <> 0 then
                    AttrString := AttrString + Format(' charset="%d"', [Index]);
                end;
              Index := Dataset.IndexByName(fldBaseType);
              if Index <> -1 then
                begin
                  Index := VarToInt(DataSet.Value[Index]);
                  AttrString := AttrString + Format(' base="%d"', [Index]);
                end;
              Index := Dataset.IndexByName(fldBaseReadOnly);
              if Index <> -1 then
                AttrString := AttrString + PrepareBooleanXML('readonly', DataSet.Value[Index]);
              Index := Dataset.IndexByName(fldBaseDefault);
              if Index <> -1 then
                AttrString := AttrString + PrepareBooleanXML('default', DataSet.Value[Index]);
              Index := Dataset.IndexByName(fldBaseServer);
              if Index <> -1 then
                begin
                  Value := UTF8ToString(RawByteString(DataSet.Value[Index]));
                  if Length(Value) <> 0 then
                    AttrString := AttrString + Format(' server="%s"', [XMLEncode(Value)]);
                end;
              Index := Dataset.IndexByName(fldBaseDatabase);
              if Index <> -1 then
                begin
                  Value := UTF8ToString(RawByteString(DataSet.Value[Index]));
                  if Length(Value) <> 0 then
                    AttrString := AttrString + Format(' database="%s"', [XMLEncode(Value)]);
                end;
              Index := Dataset.IndexByName(fldBaseLogin);
              if Index <> -1 then
                begin
                  Value := UTF8ToString(RawByteString(DataSet.Value[Index]));
                  if Length(Value) <> 0 then
                    AttrString := AttrString + Format(' login="%s"', [XMLEncode(Value)]);
                end;
              Index := Dataset.IndexByName(fldBaseConnectStr);
              if Index <> -1 then
                begin
                  Value := UTF8ToString(RawByteString(DataSet.Value[Index]));
                  if Length(Value) <> 0 then
                    AttrString := AttrString + Format(' path="%s"', [XMLEncode(Value)]);
                end;
              Index := Dataset.IndexByName(fldBaseAlias);
              if Index <> -1 then
                begin
                  Value := UTF8ToString(RawByteString(DataSet.Value[Index]));
                  if Length(Value) <> 0 then
                    AttrString := AttrString + Format(' alias="%s"', [XMLEncode(Value)]);
                end;
            end;
        finally
          Dataset.Free;
        end;
        if Length(Result) = 0 then
          Result := LineString + SpaceString + '<database' + AttrString + ' />'
        else
          Result := LineString + SpaceString + '<database' + AttrString + '>' + Result +
                    LineString + SpaceString + '</database>';
      finally
        ProcessDatasets.Free;
      end;
    end;
end;

function TDeMetadata.PrepareMetadataSolutionXML(const SolutionID: Integer; const Level: Integer; ZipFile: TZipFile): string;
var
  R, Index, NextLevel: Integer;
  SpaceString, LineString, AttrString, Value: string;
  ProcessDatabases: TStrings;
  Dataset: TDeDataset;
begin
  Result := EmptyStr;
  if Assigned(MetadataDB) then
    begin
      NextLevel := Level;
      if Level >= 0 then
        begin
          Inc(NextLevel);
          if Level = 0 then
            SpaceString := EmptyStr
          else
            SpaceString := DupeString('  ', Level);
          LineString := #13#10;
        end
      else
        begin
          SpaceString := EmptyStr;
          LineString := EmptyStr;
        end;
      ProcessDatabases := TStringList.Create;
      try
        Dataset := MetadataDB.CreateQuery(qtSelect);
        try
          Dataset.Descr.BeginUpdate;
          try
            Dataset.Descr.Table := tblDataset;
            Dataset.Descr.AddField(fldDataSetDatabase);
            Dataset.Descr.AddParamCondition(fldDataSetDeleted, ftInteger, opNE, 'DID', 1);
            Dataset.Descr.AddParamCondition(fldDataSetSolution, ftInteger, opEQ, 'SID', SolutionID);
            Dataset.Descr.AddOperation(opAnd);
            Dataset.Descr.AddSortField(fldDataSetDatabase);
          finally
            Dataset.Descr.EndUpdate;
          end;
          Dataset.Open(True);
          for R:=0 to Pred(Dataset.RecordCount) do
            begin
              Dataset.RecNo:= R;
              Value := IntToStr(VarToInt(Dataset.Value[0]));
              if ProcessDatabases.IndexOf(Value) = -1 then
                ProcessDatabases.Append(Value);
            end;
        finally
          Dataset.Free;
        end;
        for Index := 0 to Pred(ProcessDatabases.Count) do
          Result := Result + PrepareMetadataDatabaseXML(StrToInt(ProcessDatabases[Index]), NextLevel, ZipFile);
        Dataset := MetadataDB.CreateQuery(qtSelect);
        try
          Dataset.Descr.BeginUpdate;
          try
            Dataset.Descr.Table := tblCommand;
            Dataset.Descr.AddField(fldCommandID);
            Dataset.Descr.AddParamCondition(fldCommandDeleted, ftInteger, opNE, 'DID', 1);
            Dataset.Descr.AddParamCondition(fldCommandSolution, ftInteger, opEQ, 'SID', SolutionID);
            Dataset.Descr.AddCondition(fldCommandDataSet, ftInteger, opIs, Null);
            Dataset.Descr.AddParamCondition(fldCommandDataSet, ftInteger, opEQ, 'ID', 0);
            Dataset.Descr.AddOperation(opOr);
            Dataset.Descr.AddOperation(opAnd);
            Dataset.Descr.AddOperation(opAnd);
            Dataset.Descr.AddSortFields([fldCommandOrder, fldCommandID]);
          finally
            Dataset.Descr.EndUpdate;
          end;
          Dataset.Open(True);
          for R:=0 to Pred(Dataset.RecordCount) do
            begin
              Dataset.RecNo:= R;
              Result := Result + PrepareMetadataCommandXML(VarToInt(Dataset.Value[0]), NextLevel, ZipFile);
            end;
        finally
          Dataset.Free;
        end;
        if Length(Result) <> 0 then
          begin
            Dataset := MetadataDB.CreateQuery(qtRow);
            try
              Dataset.Descr.BeginUpdate;
              try
                Dataset.Descr.Table := tblSolutions;
                Dataset.Descr.AddParamCondition(fldConfID, ftInteger, opEQ, 'ID', SolutionID);
              finally
                Dataset.Descr.EndUpdate;
              end;
              Dataset.Open;
              AttrString := EmptyStr;
              Index := Dataset.IndexByName(fldConfGUID);
              if Index <> -1 then
                begin
                  Value := Trim(VarToStr(DataSet.Value[Index]));
                  if Length(Value) <> 0 then
                    AttrString := Format(' id="%s"', [XMLEncode(Value)]);
                end;
              Index := Dataset.IndexByName(fldConfVersion);
              if Index <> -1 then
                begin
                  Index := VarToInt(DataSet.Value[Index]);
                  if Index <> 0 then
                    AttrString := AttrString + Format(' version="%d"', [Index]);
                end;
              Index := Dataset.IndexByName(fldConfName);
              if Index <> -1 then
                begin
                  Value := Trim(UTF8ToString(RawByteString(DataSet.Value[Index])));
                  if Length(Value) <> 0 then
                    AttrString := AttrString + Format(' name="%s"', [XMLEncode(Value)]);
                end;
              Index := Dataset.IndexByName(fldConfDeveloper);
              if Index <> -1 then
                begin
                  Value := Trim(UTF8ToString(RawByteString(DataSet.Value[Index])));
                  if Length(Value) <> 0 then
                    AttrString := AttrString + Format(' developer="%s"', [XMLEncode(Value)]);
                end;
              Index := Dataset.IndexByName(fldConfDescription);
              if Index <> -1 then
                begin
                  Value := Trim(UTF8ToString(RawByteString(DataSet.Value[Index])));
                  if Length(Value) <> 0 then
                    AttrString := AttrString + Format(' description="%s"', [XMLEncode(Value)]);
                end;
            finally
              Dataset.Free;
            end;
            Result := LineString + SpaceString + '<solution' + AttrString + '>' + Result +
                      LineString + SpaceString + '</solution>';
          end;
      finally
        ProcessDatabases.Free;
      end;
    end;
end;

function TDeMetadata.PrepareMetadataXML(const SolutionID: Integer; const Level: Integer; ZipFile: TZipFile): string;
var
  R, Index, NextLevel: Integer;
  SpaceString, LineString, Value: string;
  ProcessSolutions: TStrings;
  Dataset: TDeDataset;
  VersionMS, VersionLS: Cardinal;
begin
  Result := EmptyStr;
  if Assigned(MetadataDB) then
    begin
      NextLevel := Level;
      if Level >= 0 then
        begin
          Inc(NextLevel);
          if Level = 0 then
            SpaceString := EmptyStr
          else
            SpaceString := DupeString('  ', Succ(Level));
          LineString := #13#10;
        end
      else
        begin
          SpaceString := EmptyStr;
          LineString := EmptyStr;
        end;
      ProcessSolutions := TStringList.Create;
      try
        Dataset := MetadataDB.CreateQuery(qtSelect);
        try
          Dataset.Descr.BeginUpdate;
          try
            Dataset.Descr.Table := tblDataset;
            Dataset.Descr.AddField(fldDataSetSolution);
            Dataset.Descr.AddParamCondition(fldDataSetDeleted, ftInteger, opNE, 'DID', 1);
            if SolutionID <> 0 then
              begin
                Dataset.Descr.AddParamCondition(fldDataSetSolution, ftInteger, opEQ, 'SID', SolutionID);
                Dataset.Descr.AddOperation(opAnd);
              end;
            Dataset.Descr.AddSortField(fldDataSetSolution);
          finally
            Dataset.Descr.EndUpdate;
          end;
          Dataset.Open(True);
          for R:=0 to Pred(Dataset.RecordCount) do
            begin
              Dataset.RecNo:= R;
              Value := IntToStr(Dataset.Value[0]);
              if ProcessSolutions.IndexOf(Value) = -1 then
                ProcessSolutions.Append(Value);
            end;
        finally
          Dataset.Free;
        end;
        Dataset := MetadataDB.CreateQuery(qtSelect);
        try
          Dataset.Descr.BeginUpdate;
          try
            Dataset.Descr.Table := tblCommand;
            Dataset.Descr.AddField(fldCommandSolution);
            Dataset.Descr.AddParamCondition(fldCommandDeleted, ftInteger, opNE, 'DID', 1);
            if SolutionID <> 0 then
              begin
                Dataset.Descr.AddParamCondition(fldCommandSolution, ftInteger, opEQ, 'SID', SolutionID);
                Dataset.Descr.AddOperation(opAnd);
              end;
            Dataset.Descr.AddSortField(fldCommandSolution);
          finally
            Dataset.Descr.EndUpdate;
          end;
          Dataset.Open(True);
          for R:=0 to Pred(Dataset.RecordCount) do
            begin
              Dataset.RecNo:= R;
              Value := IntToStr(Dataset.Value[0]);
              if ProcessSolutions.IndexOf(Value) = -1 then
                ProcessSolutions.Append(Value);
            end;
        finally
          Dataset.Free;
        end;
        for Index := 0 to Pred(ProcessSolutions.Count) do
          Result := Result + PrepareMetadataSolutionXML(StrToInt(ProcessSolutions[Index]), NextLevel, ZipFile);
      finally
        ProcessSolutions.Free;
      end;
      if Level = 0 then
        Value := EmptyStr
      else
        Value := LineString;
      Value := Value + SpaceString + '<metadata';
      if GetFileVersion(Application.ExeName, VersionMS, VersionLS) then
        Value := Value + Format(' version="%d.%d"', [HiWord(VersionMS), LoWord(VersionMS)]);
      if Length(Result) = 0 then
        Result := Value + ' />'
      else
        Result := Value + '>' + Result + LineString + SpaceString + '</metadata>';
    end;
end;

const
  RT_GUIDS = 'GUIDS';

class function TDeMetadata.TryDatasetToGUID(const DatasetName: string; var GUID: TGUID): Boolean;
const
  cDatasetListResource = '__DATASETS';
var
  Stream: TStream;
  Strings: TStrings;
  Value: string;
begin
  Result := FindResource(hInstance, cDatasetListResource, RT_GUIDS) <> 0;
  if Result then
    begin
      Strings := TStringList.Create;
      try
        Stream := TResourceStream.Create(hInstance, cDatasetListResource, RT_GUIDS);
        try
          Strings.LoadFromStream(Stream);
        finally
          Stream.Free;
        end;
        Value := Trim(Strings.Values[DatasetName]);
      finally
        Strings.Free;
      end;
      Result := Length(Value) <> 0;
      if Result then GUID := StringToGUID(Value);
    end;
end;

class function TDeMetadata.TryDatasetFieldToGUID(const DatasetName, FieldName: string; var DatasetGUID, FieldGUID: TGUID): Boolean;
var
  Stream: TStream;
  Strings: TStrings;
  Value: string;
begin
  Result := TryDatasetToGUID(DatasetName, DatasetGUID) and (FindResource(hInstance, PChar(DatasetName), RT_GUIDS) <> 0);
  if Result then
    begin
      Strings := TStringList.Create;
      try
        Stream := TResourceStream.Create(hInstance, PChar(DatasetName), RT_GUIDS);
        try
          Strings.LoadFromStream(Stream);
        finally
          Stream.Free;
        end;
        Value := Trim(Strings.Values[FieldName]);
      finally
        Strings.Free;
      end;
      Result := Length(Value) <> 0;
      if Result then FieldGUID := StringToGUID(Value);
    end;
end;

function TDeMetadata.TryDatasetNameToDatasetID(const DatasetName: string; var DatasetID: Integer; const FirstInMetadataSearch: Boolean): Boolean;
var
  Dataset: TDeDataset;
  TableMeta: TTableMeta;
begin
  Result := False;
  if FirstInMetadataSearch and Assigned(TablesList) then
    begin
      TableMeta := TablesList.FindByTable(DatasetName);
      if Assigned(TableMeta) then
        begin
          DatasetID := TableMeta.ID;
          Result := True;
        end;
    end;
  if Assigned(MetadataDB) and not Result then
    begin
      Dataset := MetadataDB.CreateQuery(qtRow);
      try
        Dataset.Descr.BeginUpdate;
        try
          Dataset.Descr.Table := tblDataset;
          Dataset.Descr.AddField(fldDatasetID);
          Dataset.Descr.AddParamCondition(fldDataSetDeleted, ftSmallInt, opNE, 'DID', 1);
          Dataset.Descr.AddParamCondition(fldDataSetTable, ftString, opLike, 'NAME', DatasetName);
          Dataset.Descr.AddOperation(opAnd);
        finally
          Dataset.Descr.EndUpdate;
        end;
        Dataset.Open(True);
        Result := Dataset.RecordCount = 1;
        if Result then
          DatasetID := VarToInt(Dataset.Value[0]);
      finally
        Dataset.Free;
      end;
    end;
end;

function TDeMetadata.TryFieldNameToFieldID(const DatasetID: Integer; const FieldName: string; var FieldID: Integer; const FirstInMetadataSearch: Boolean): Boolean;
var
  Dataset: TDeDataset;
  TableMeta: TTableMeta;
  FieldMeta: TFieldMeta;
begin
  Result := False;
  if FirstInMetadataSearch and Assigned(TablesList) then
    begin
      TableMeta := TablesList.FindByID(DatasetID);
      if Assigned(TableMeta) then
        begin
          FieldMeta := TableMeta.Fields.FindByName(FieldName);
          if Assigned(FieldMeta) then
            begin
              FieldID := FieldMeta.ID;
              Result := True;
            end;
        end;
    end;
  if Assigned(MetadataDB) and not Result then
    begin
      Dataset := MetadataDB.CreateQuery(qtRow);
      try
        Dataset.Descr.BeginUpdate;
        try
          Dataset.Descr.Table := tblFields;
          Dataset.Descr.AddField(fldFieldsID);
          Dataset.Descr.AddParamCondition(fldFieldsDeleted, ftSmallInt, opNE, 'DID', 1);
          Dataset.Descr.AddParamCondition(fldFieldsTable, ftInteger, opEQ, 'TID', DatasetID);
          Dataset.Descr.AddParamCondition(fldFieldsOriginal, ftString, opLike, 'NAME', FieldName);
          Dataset.Descr.AddOperation(opAnd);
          Dataset.Descr.AddOperation(opAnd);
        finally
          Dataset.Descr.EndUpdate;
        end;
        Dataset.Open(True);
        Result := Dataset.RecordCount = 1;
        if Result then
          FieldID := VarToInt(Dataset.Value[0]);
      finally
        Dataset.Free;
      end;
    end;
end;

function TDeMetadata.TryFieldNameToFieldID(const DatasetName, FieldName: string; var DatasetID, FieldID: Integer; const FirstInMetadataSearch: Boolean): Boolean;
begin
  Result := TryDatasetNameToDatasetID(DatasetName, DatasetID, FirstInMetadataSearch);
  if Result then
    Result := TryFieldNameToFieldID(DatasetID, FieldName, FieldID, FirstInMetadataSearch);
end;

function TDeMetadata.CheckPresentFields: Boolean;
  procedure CheckMetadataPresents(const DatasetName, FieldName: string; const Present: TMetadataPresent);
  var
    DatasetID, FieldID: Integer;
  begin
    if TryFieldNameToFieldID(DatasetName, FieldName, DatasetID, FieldID) then
      Include(FMetadataPresents, Present)
    else
      Exclude(FMetadataPresents, Present);
  end;
  procedure CheckDatabasePresents(const DatasetName, FieldName: string; const Present: TMetadataPresent);
  begin
    if Assigned(MetadataDB) then
      if MetadataDB.FieldExists(DatasetName, FieldName) then
        Include(FMetadataPresents, Present)
      else
        Exclude(FMetadataPresents, Present);
  end;
begin
  try
    // Проверка основных GUID`ов ...
    CheckMetadataPresents(tblDataset, fldDataSetGUID, mpMetadataDatasetGUID);
    CheckMetadataPresents(tblFields, fldFieldsGUID, mpMetadataFieldGUID);
    // Проверка дополнительных GUID`ов ...
    CheckMetadataPresents(tblSolutions, fldConfGUID, mpMetadataSolutionGUID);
    CheckDatabasePresents(tblSolutions, fldConfGUID, mpDatabaseSolutionGUID);
    CheckDatabasePresents(tblCommand, fldCommandGUID, mpDatabaseCommandGUID);
    CheckDatabasePresents(tblMenu, fldMenuGUID, mpDatabaseMenuGUID);
    // Проверка дополнительных новых полей ...
    CheckDatabasePresents(tblDataset, fldDataSetCaption, mpDatabaseDataSetCaption);
    CheckDatabasePresents(tblFields, fldDataSetCaption, mpFieldDefaultDB);


    CheckDatabasePresents(tblConstraints, fldConstraintsID, mpDatabaseNewConstraints);
    CheckDatabasePresents(tblCommandParams, fldCommandParamOrder, mpCommandParamOrders); // v.19.04
    Result := True;
  except
    on E: Exception do
      begin
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Check present fields error: ' + E.Message);
        {$ENDIF}
        Result := False;
      end;
  end;
end;

function TDeMetadata.ExecuteScript(const Text: string): Boolean;
var
  Parser: TDeParser;
  ExpressionItem: TExpressionItem;
  Calculator: TDeCalculator;
begin
  try
    Parser := TDeParser.Create;
    try
      ExpressionItem := TExpressionItem.Create;
      try
        Parser.Parse(Text, ExpressionItem);
        Calculator := TDeCalculator.Create;
        try
          Calculator.Calculate(ExpressionItem);
          Result := True;
        finally
          Calculator.Free;
        end;
      finally
        ExpressionItem.Free;
      end;
    finally
      Parser.Free;
    end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('%s.ExecuteScript(%s) skip error: %s', [ClassName, QuotedStr(Text), E.Message]);
        {$ENDIF}
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Execute script %s error: %s', [QuotedStr(Text), E.Message]);
        {$ENDIF}
        Result := False;
      end;
  end;
end;

function TDeMetadata.GetTableMeta(const Value: Variant): TTableMeta;
var
  GUID: TGUID;
begin
  if VarIsStr(Value) then
    if TryStringToGUID(Value, GUID) then
      Result := MetaData.GetTableMeta(GUID)
    else
      Result := MetaData.GetTableByName(Value)
  else
    Result := MetaData.GetTableMeta(StrToIntDef(Value, -1));
end;

{ TDeMetadata.TDeMetadataNotifier }

destructor TDeMetadata.TDeMetadataNotifier.Destroy;
begin
  FieldMetaDataSet := nil;
  ActionMetaDataSet := nil;
  inherited Destroy;
end;

procedure TDeMetadata.TDeMetadataNotifier.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FFieldMetaDataSet then
      begin
        FFieldMetaDataSet := nil;
        {$IFDEF DEBUG}
        DebugLog('TDeMetadata.TDeMetadataNotifier.Notification(FieldMetaDataSet, opRemove) ...');
        {$ENDIF}
      end
    else if AComponent = FActionMetaDataSet then
      begin
        FActionMetaDataSet := nil;
        {$IFDEF DEBUG}
        DebugLog('TDeMetadata.TDeMetadataNotifier.Notification(ActionMetaDataSet, opRemove) ...');
        {$ENDIF}
      end;
end;

procedure TDeMetadata.TDeMetadataNotifier.SetFieldMetaDataSet(const Value: TDeDataSet);
begin
  if Assigned(FFieldMetaDataSet) then
    begin
      FFieldMetaDataSet.RemoveFreeNotification(Self);
      FreeAndNil(FFieldMetaDataSet);
    end;
  FFieldMetaDataSet := Value;
  if Assigned(FFieldMetaDataSet) then
    FFieldMetaDataSet.FreeNotification(Self);
end;

procedure TDeMetadata.TDeMetadataNotifier.SetActionMetaDataSet(const Value: TDeDataSet);
begin
  if Assigned(FActionMetaDataSet) then
    begin
      FActionMetaDataSet.RemoveFreeNotification(Self);
      FreeAndNil(FActionMetaDataSet);
    end;
  FActionMetaDataSet := Value;
  if Assigned(FActionMetaDataSet) then
    FActionMetaDataSet.FreeNotification(Self);
end;

function TDeMetadata.ReadDefaultSolution(const AutoCreated: Boolean): Variant;
resourcestring
  sDefaultSolutionName = 'Default solution';
  sDefaultSolutionDescription = 'Solution by default for user objects';
var
  Dataset: TDeDataset;
  Value: Variant;
begin
  Result := Unassigned;
  if Assigned(MetadataDB) then
    try
      Dataset := MetadataDB.CreateQuery(qtRow);
      try
        Dataset.Descr.BeginUpdate;
        try
          Dataset.Descr.Table := tblSolutions;
          Dataset.Descr.AddField(opMIN, fldConfID);
          Dataset.Descr.AddCondition(fldConfDeleted, ftSmallInt, opEQ, 0);
          Dataset.Descr.AddCondition(fldConfGUID, ftGUID, opIs, Null);
          Dataset.Descr.AddOperation(opAnd);
        finally
          Dataset.Descr.EndUpdate;
        end;
        Dataset.Open;
        Result := Dataset.Value[0];
      finally
        Dataset.Free;
      end;
      // Если нет решения по умолчанию и его надо создать, то ...
      if (VarIsEmpty(Result) or VarIsNull(Result)) and AutoCreated then
        begin
          Dataset := MetadataDB.CreateQuery(qtHole);
          try
            if Assigned(Dataset) then
              begin
                Dataset.Descr.BeginUpdate;
                try
                  Dataset.Descr.Table := tblSolutions;
                  Dataset.Descr.AddField(fldConfID);
                finally
                  Dataset.Descr.EndUpdate;
                end;
              end
            else
              begin
                Dataset := MetaData.MetadataDB.CreateQuery(qtRow);
                Dataset.Descr.BeginUpdate;
                try
                  Dataset.Descr.Table := tblSolutions;
                  Dataset.Descr.AddField(opMax, fldConfID);
                finally
                  Dataset.Descr.EndUpdate;
                end;
              end;
            Dataset.Open;
            Result := Dataset.IntValueDef(0) + 1;
          finally
            Dataset.Free;
          end;
          Dataset := MetadataDB.CreateQuery(qtInsert);
          try
            Dataset.Descr.BeginUpdate;
            try
              Dataset.Descr.Table := tblSolutions;
              Dataset.Descr.AddField(fldConfID, ftInteger, Result);
              Value := StrToInt(FormatDateTime('YYMM"00"', Now));
              Dataset.Descr.AddField(fldConfVersion, ftInteger, Value);
              Dataset.Descr.AddField(fldConfName, ftString, sDefaultSolutionName);
              Dataset.Descr.AddField(fldConfDescription, ftString, sDefaultSolutionDescription);
              Dataset.Descr.AddField(fldConfDeveloper, ftString, GetWindowsUserName);
              Value := 0;
              Dataset.Descr.AddField(fldConfDeleted, ftSmallInt, Value);
              Dataset.Descr.AddField(fldConfGUID, ftGUID, Null);
            finally
              Dataset.Descr.EndUpdate;
            end;
            Dataset.ExecuteQuery;
          finally
            Dataset.Free;
          end;
        end;
    except
      on E: Exception do
        begin
          Result := Unassigned;
          {$IFDEF DEBUG}
          if AutoCreated then
            DebugLog('%s.ReadDefaultSolution(True) skip error: %s', [ClassName, E.Message])
          else
            DebugLog('%s.ReadDefaultSolution(False) skip error: %s', [ClassName, E.Message]);
          {$ENDIF}
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Read default solution error: %s', [E.Message]);
          {$ENDIF}
        end;
    end;
end;

{ TDeMessage }

constructor TDeMessage.Create(const aIco: Integer; const aCaption, aText: string; const aMsgType: TMsgDlgType);
begin
  inherited Create;
  FIco := aIco;
  FCount := 1;
  FCaption := aCaption;
  FText := aText;
  FMsgType := aMsgType;
  FTime := Now;
end;

{ TDeMessageList }

function TDeMessageList.AddMessage(const aIco: Integer; const aCaption, aText: string; const aMsgType: TMsgDlgType = mtInformation): TDeMessage;
begin
  Result := TDeMessage.Create(aIco, aCaption, aText, aMsgType);
  Add(Result);
end;

function TDeMessageList.GetMessage(const Index: Integer): TDeMessage;
begin
  Result := TDeMessage(inherited Items[Index]);
end;

procedure TDeMessageList.SetMessage(const Index: Integer; const Value: TDeMessage);
begin
  if Index >= Count then Count := Succ(Index);
  inherited Items[Index] := Value;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('DeMetadata unit initialization ...');
  {$ENDIF}
  GlobalActionConditionList := nil;
  CurrentConfig := nil;
  DeMessages := TDeMessageList.Create;
  MetaData := TDeMetaData.Create;
  ConfigList := TDeConfigList.Create;
  ConfigList.LoadFromParameters;
  if not ConfigList.ReadOnly then
    if FileExists(ExtractFilePath(ParamStr(0))+ ConfigXML) then
      ConfigList.LoadFromXML(ConfigXML)
    else
      ConfigList.LoadFromDirectory(ExtractFilePath(ParamStr(0)));
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DeMetadata unit finalization ...');
  {$ENDIF}
  FreeAndNil(MetaData);
  FreeAndNil(ConfigList);
  FreeAndNil(DeMessages);
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

