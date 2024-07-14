{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

unit DeActions;

interface

uses Forms, SysUtils, Classes, Contnrs, ActnList, Variants, Dialogs, Controls, Windows, FileCtrl, Menus, DB, Math,
     Generics.Collections, DeTypes, DeVariable, DeParser, DeDB, DeReport, DeMeta, DeDataset, DataCacheUnit;

type
  /// <summary>Тип действия:
  /// <para><c>atGroup</c> - группа действий</para>
  /// <para><c>atInner</c> - встроенное в приложение</para>
  /// <para><c>atStoredProcedure</c> - вызов хранимой процедуры</para>
  /// <para><c>atShellExecute</c> - команда операционной системы</para>
  /// <para><c>atSetValue</c> - установка значения</para>
  /// <para><c>atPreview</c> - формирование отчёта с возможностью просмотра/печати/сохранения (в зависимости от состояния переменной окружения '__PRINTER_NAME')!</para>
  /// <para><c>atReport</c> - формирование отчёта с возможностью просмотра/печати/сохранения</para>
  /// </summary>
  TActionType =
    (
      atGroup = 0,
      atInner = 1,
      atStoredProcedure = 2,
      atShellExecute = 3,
      atSetValue = 4,
      atPreview = 5, // С v.18.3 тип просмотра отчёта // atDLL = 5,
      atReport = 6,
      atInsertValue = 7, // Новых два типа, которые должны прийти на смену atSetValue в версии 16.3 с поддержкой диалога параметров (имя параметра = имя поля).
      atUpdateValue = 8,
      atDataSet = 9,     // Открытие набора данных для v. 17.3
      atCalculator = 10  // Выполнения скрипта в калькуляторе для v. 19.2
    );

  TActionExecuteMethod = procedure(Sender: TObject) of object;

  /// <summary>Категория для действия:
  /// <para><c>acNone</c> - нет категории</para>
  /// <para><c>acRoot</c> - категория основного меню</para>
  /// <para><c>acFile</c> - категория файлов</para>
  /// <para><c>acCreate</c> - категория создания</para>
  /// <para><c>acPrint</c> - категория печати</para>
  /// <para><c>acEdit</c> - категория правки</para>
  /// <para><c>acService</c> - категория сервиса</para>
  /// <para><c>acOperation</c> - категория операций</para>
  /// <para><c>acAction</c> - категория действий</para>
  /// <para><c>acCommand</c> - категория команд</para>
  /// <para><c>acHelp</c> - категория помощи</para>
  /// </summary>
  TActionCategory = (acNone, acRoot, acFile, acCreate, acPrint, acEdit, acService, acOperation, acAction, acCommand, acHelp);

  /// <summary>Источник для действия:
  /// <para><c>asNone</c> - системные действия приложения</para>
  /// <para><c>asActions</c> - действия из таблицы M_ACTIONS</para>
  /// <para><c>asReports</c> - отчёты из таблицы M_REPORTS</para>
  /// <para><c>asCommands</c> - команды из таблицы M_COMMANDS</para>
  /// </summary>
  TActionSource = (asNone, {asActions, asReports, }asCommands);

  /// <summary>Режим выполнения для действия:
  /// <para><c>amDefault</c> - взять режим по умолчанию для действия, описанный в свойстве DefaultMode объекта класса TDeActionData (Только для первого уровня вызова при группе действий; внутри группы параметр игнорируется)</para>
  /// <para><c>amFocused</c> - только данные в фокусе и только одну запись</para>
  /// <para><c>amMultiIn</c> - все выбранные и только на входе в действие (до v.16.10 это стандартное поведение при выполнении действия)</para>
  /// <para><c>amMultiOut</c> - все выбранные и действие не разварачивает массивы, а передаёт их в обработчик</para>
  /// </summary>
  TActionMode = (amDefault, amFocused, amMultiIn, amMultiOut);

  TActionState = (alsGUID);
  TActionStates = set of TActionState;

{$IFDEF DEBUG}
const
  ActionCategories: array[TActionCategory] of PChar = ('acNone', 'acRoot', 'acFile', 'acCreate', 'acPrint', 'acEdit', 'acService', 'acOperation', 'acAction', 'acCommand', 'acHelp');
  ActionSources: array[TActionSource] of PChar = ('asNone', {'asActions', 'asReports', }'asCommands');
{$ENDIF}

type
   /// <summary>оболочка для действия</summary>
  TDeActionData = class
  private
    FActionSource: TActionSource;
    FType       : TActionType;
    FID         : Integer;
    FCaption    : string;
    FFile       : string;
    FProcedure  : string;
    FParams     : string;
    FOnExecute  : TActionExecuteMethod;
    FQuestion   : String;
    FDataSet    : Integer;
    FFields     : TObjectList;
    FEnabledStr : string;
    FVisibleStr : string;
    FShortCut   : TShortCut;
    FOrder      : Integer;
    FDataSetIDs : array of Integer;
    FCategory   : TActionCategory;
    FSubCategories: string;
    FOriginal: string;              // имя действия для вызова из скриптов; реализовано в v.15.10
    FICO: Integer;
    FBreakLine: Boolean;            // True - должен быть разделитель перед пунктом действия
    FEnabledPostfix : TExpressionItem;
    FVisiblePostfix : TExpressionItem;
    //FProgress: Integer;
    FActions: TStrings;
    FExecuteVaribales: TDeVariableList;
    FScriptVaribales: TDeVariableList;
    FActive: Boolean;              // True - отображать в группе действий
    FBeforeScript: string;
    FAfterScript: string;
    FStates: TActionStates;
    FGUID: TGUID;
    FDefaultMode: TActionMode;
    FSolution: Variant;
    function GetEnabledPostfix : TExpressionItem;
    function GetVisiblePostfix : TExpressionItem;
    procedure CheckCategory;
    function PrepareReportVariablesFileSuffix(Variables: TDeVariableList): string;
    function PrepareReport(Variables: TDeVariableList): TDeReport;
    function GetActionCount: Integer;
    function GetActions(const Index: Integer): TDeActionData;
    procedure CheckGroup;
    procedure ExecuteInner;
    procedure ExecuteSetValue;
    procedure ExecuteShellExecute(Variables: TDeVariableList);
    procedure ExecuteStoredProcedure(Variables: TDeVariableList);
    procedure ExecuteReport(Variables: TDeVariableList; const QuestionEnabled: Boolean; const Mode: TActionMode);
    procedure ExecuteGroup(Variables: TDeVariableList; const QuestionEnabled: Boolean; const Mode: TActionMode);
    procedure ExecuteUpdateValue(Variables: TDeVariableList; const QuestionEnabled: Boolean; const Mode: TActionMode);
    procedure ExecuteInsertValue(Variables: TDeVariableList; const QuestionEnabled: Boolean; const Mode: TActionMode);
    procedure ExecuteDataSet;
    procedure ExecuteCalculator(Variables: TDeVariableList; const QuestionEnabled: Boolean; const Mode: TActionMode);
    function GetIdentType(const aIdent: string; var aType: TFieldType; aDataObject: pointer): Boolean;
    function GetIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
    function GetActionNames(const Index: Integer): string;
    function GetVariantGUID: Variant;
    procedure CheckDefaultMode;
    function IsSupportLinkDataSet(const DataSetID: Integer): Boolean;
    function GetExecuteScriptIdentType(const aIdent: string; var aType: TFieldType; aDataObject: pointer): Boolean;
    function GetExecuteScriptIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
  public
    constructor Create; overload;
    constructor Create(const aActionName: string; aOnExecute: TActionExecuteMethod; const aTable: Integer = -1); overload;
    constructor Create(const aActionName: string; const aProcedureName: string; const aTable: Integer = -1); overload;
    destructor Destroy; override;
    procedure Assign(Source: TObject);
    /// <summary>Функция перечитывания параметров действия из базы данных</summary>
    /// <returns>True - параметры перечитались</returns>
    function Refresh: Boolean;
    /// <summary>проверка поддержки действием указанного датасета, включая общие</summary>
    /// <returns>True - поддерживает</returns>
    function IsSupportDataSet(const DataSetID: Integer): Boolean;
    /// <summary>проверка поддержки действием указанного датасета, исключая общие</summary>
    /// <returns>True - поддерживает</returns>
    function IsContextDataSet(const DataSetID: Integer): Boolean;
    /// <summary>проверка наличия в действии указанного типа действия</summary>
    /// <returns>True - есть такой тип действия</returns>
    function CheckActionType(const ActionType: TActionType): Boolean;

    function UpdateSort(const BreakLine: Boolean; const Order: Integer): Boolean;
    class function PrepareCommandLineParameters(const CommandID: Integer): string; overload;
    function PrepareCommandLineParameters: string; overload;
    /// <summary>Тип действия</summary>
    property ActionType: TActionType read FType;
    /// <summary>Источник действия</summary>
    property ActionSource: TActionSource read FActionSource;
    /// <summary>Идентификатор действия</summary>
    property ID: Integer read FID write FID;
    /// <summary>Заголовок действия</summary>
    property Caption: string read FCaption write FCaption;
    /// <summary>Текст запроса пользователю на подтверждение действия</summary>
    /// <remarks>Если пустая строка, то запрос не выводится пользователю</remarks>
    property Question: string read FQuestion;
    /// <summary>Имя действия для вызова из скриптов</summary>
    /// <remarks>реализовано в версии 15.10</remarks>
    property Original: string read FOriginal;

    property ActionFile: string read FFile;
    property ActionProcedure: string read FProcedure;
    property ActionParams: string read FParams;

    property DataSet: Integer read FDataSet write FDataSet;
    property Fields: TObjectList read FFields;
    property EnableStr: string read FEnabledStr;
    property EnabledPostfix: TExpressionItem read GetEnabledPostfix;
    property VisibleStr: string read FVisibleStr;
    property VisiblePostfix: TExpressionItem read GetVisiblePostfix;
    property ShortCut: TShortCut read FShortCut;
    property Order: Integer read FOrder;
    /// <summary>Индекс иконки для действия</summary>
    /// <remarks>реализовано в версии 15.10 и поддерживается M_ACTIONS.MAC_ICO</remarks>
    property ICO: Integer read FICO write FICO;
    /// <summary>Категория действия:
    /// <para>'_action' - категория действий по умолчанию</para>
    /// <para>'_print' - категория печати по умолчанию</para>
    /// <para>'_create' - категория создания по умолчанию</para>
    /// <para>'_root' - категория главного меню</para>
    /// <para>'_file' - категория меню работы с файлами</para>
    /// <para>'_edit' - категория меню правки</para>
    /// <para>'_service' - категория сервисного меню</para>
    /// <para>'_operation' - категория меню сервисных операций</para>
    /// <para>'_help' - категория меню помощи</para>
    /// <para>'_command' - категория меню команд</para>
    /// </summary>
    /// <remarks>Используется для раскидывания дествий по меню</remarks>
    property Category: TActionCategory read FCategory;
    /// <summary>Дерево для меню вложенных категорий</summary>
    property SubCategories: string read FSubCategories;
    /// <summary>Флаг разделителя перед пунктом действия</summary>
    /// <remarks>True - нужен разделитель!</remarks>
    property BreakLine: Boolean read FBreakLine;
    /// <summary>Метод разбора строки параметров</summary>
    /// <param name="aString">строка параметров для разбора</param>
    /// <param name="DoClear">флаг очистки списка полей перед разбором строки параметров</param>
    procedure ParseParams(const aString: string; const DoClear: Boolean = True);
    /// <summary>Метод выполнения действия</summary>
    /// <param name="Mode">Режим выполнения действия</param>
    procedure Execute(const Mode: TActionMode = amDefault); overload;
    /// <summary>Метод выполнения действия</summary>
    /// <param name="Variables">Список параметров</param>
    /// <param name="QuestionEnabled">Флаг отображения диалоговой формы</param>
    /// <param name="Mode">Режим выполнения действия</param>
    procedure Execute(Variables: TDeVariableList; const QuestionEnabled: Boolean; const Mode: TActionMode = amDefault); overload;
    /// <summary>Функция проверки доступности операции с данным действием</summary>
    /// <param name="Operation">операция с действием</param>
    /// <returns>True - операция с действием доступна (разрешено)</returns>
    function CheckPolicy(const Operation: TSecurityOperation): Boolean;
    function ExecuteScript(const Script: string; Variables: TDeVariableList): Boolean;
    /// <summary>Количество действий в группе</summary>
    /// <remarks>Если ActionType = atGroup, то это список действий для выполнения по группе</remarks>
    property ActionCount: Integer read GetActionCount;
    /// <summary>Массив действий в группе</summary>
    property Actions[const Index: Integer]: TDeActionData read GetActions;
    /// <summary>Массив имён действий в группе</summary>
    property ActionNames[const Index: Integer]: string read GetActionNames;
    /// <summary>Флаг отображения в группе</summary>
    /// <remarks>True - отображать в группе</remarks>
    property Active: Boolean read FActive;
    property BeforeScript: string read FBeforeScript;
    property AfterScript: string read FAfterScript;
    // v 17.10
    property States: TActionStates read FStates;
    property GUID: TGUID read FGUID;
    property VariantGUID: Variant read GetVariantGUID;
    property DefaultMode: TActionMode read FDefaultMode;
    property Solution: Variant read FSolution;
  end;

  TActionStatus = (asUnknown, asInformation, asResult, asError);

  TDeActionResult = class
  private
    FDateTime: TDateTime;
    FStatus: TActionStatus;
    FText: string;
  public
    constructor Create(const AText: string);
    property DateTime: TDateTime read FDateTime;
    property Status: TActionStatus read FStatus;
    property Text: string read FText;
  end;

  TDeActionResults = class
  private
    FList: TObjectList;
    FInfoList: TList;
    FErrorList: TList;
    FResultList: TList;
    FOwner: TDeActionData;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TDeActionResult;
    function GetInfoCount: Integer;
    function GetInfos(const Index: Integer): TDeActionResult;
    function GetResultCount: Integer;
    function GetResults(const Index: Integer): TDeActionResult;
    function GetErrorCount: Integer;
    function GetErrors(const Index: Integer): TDeActionResult;
  public
    constructor Create(AOwner: TDeActionData);
    destructor Destroy; override;
    procedure Clear;
    function Add(const Text: string): Integer;
    procedure Delete(const Index: Integer);
    {$IFDEF DEBUG}
    procedure DebugResultsLog(const Text: string);
    {$ENDIF}
    function PrepareText(const Status: TActionStatus; const MaxLength: Integer = 0; const Separator: string = #13#10): string;
    property Owner: TDeActionData read FOwner;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TDeActionResult read GetItem; default;
    property InfoCount: Integer read GetInfoCount;
    property Infos[const Index: Integer]: TDeActionResult read GetInfos;
    property ResultCount: Integer read GetResultCount;
    property Results[const Index: Integer]: TDeActionResult read GetResults;
    property ErrorCount: Integer read GetErrorCount;
    property Errors[const Index: Integer]: TDeActionResult read GetErrors;
  end;

  /// <summary>список действий</summary>
  TDeActionList = class(TObjectList)
  private
    FActionResults: TDeActionResults;
    function GetAction(const Index: Integer): TDeActionData;
    function GetExecutingAction: TDeActionData;
    function CreateCommandDataset(const ActionID: Integer = 0): TDeDataset;
    function CreateCommandLinkParametersDataset(const ActionID: Integer = 0): TDeDataset;
    procedure LoadCommands;
    procedure LoadCommandLinkParameters;
  public
    property Items[const Index: Integer]: TDeActionData read GetAction; default;
    /// <summary>Функция получения индекса действия в массиве по оригинальному имени</summary>
    /// <returns>-1 - действие не найдено</returns>
    function IndexByOriginal(const Original: string): Integer;
    /// <summary>Функция получения индекса действия в массиве по идентификатору</summary>
    /// <returns>-1 - действие не найдено</returns>
    function IndexByID(const ActionID: Integer): Integer;
    /// <summary>Метод загрузки списка действий из метаструктуры</summary>
    procedure LoadData;
    /// <summary>Метод обновления списка действий из метаструктуры</summary>
    procedure UpdateData(const ActionID: Integer = 0; const Changed: TTypeChanged = mcUpdate);
    {$IFDEF DEBUG}
    procedure DebugActionsLog(const Text: string);
    {$ENDIF}
    procedure BeforeExecute(Action: TDeActionData);
    procedure AfterExecute(Action: TDeActionData; Variables: TDeVariableList);
    /// <summary>Ссылка на выполняемое действие</summary>
    property ExecutingAction: TDeActionData read GetExecutingAction;
  end;

  /// <summary>контейнер для хранения действий</summary>
  TActionsContainer = class
  private
    FActions : TDeActionList;
    procedure UnactivateActions;
  protected
    procedure ActivateActions;  virtual;  abstract;
  public
    constructor Create;
    destructor Destroy;  override;
    property ContainerActions : TDeActionList read FActions;
    function ActivateAction(const Name: string; OnExecute: TActionExecuteMethod; const TableID: Integer = -1;
                            const BreakLine: Boolean = False; const Icon: Integer = -1;
                            const Category: TActionCategory = acAction; const SubCategories: string = ''): Integer;
  end;

  /// <summary>глобальный список контейнеров</summary>
  TActionContainerStorage = class(TObjectList)
  private
    function GetContainer(const Index: Integer): TActionsContainer;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    property Items[const Index: Integer]: TActionsContainer read GetContainer; default;
    procedure ActivateAll;
    procedure DeactivateAll;
  end;

type
  /// <summary>класс для хранения и регистрации дополнительных методов</summary>
  TMetaDataOperations = class(TActionsContainer)
  private
    FTempReportID: Integer;
    FTempReportLabel: string;
    FTempReportCaption: string;
    FTempReportParamDef: TDeReportParamDef;
    procedure ClearReportParameters(const ReportID: Integer);
    function PrepareTempReport(const ReportID: Integer): TDeReport;
    procedure TempCacheInsertReportParam(CacheItem: TCacheItem; var CanInsert: Boolean);
    procedure SaveReportParameters(const ReportID, CodePage: Integer; ParamDefs: TDeReportParamDefs);
    procedure UpdateCommandStoredProcedureParameters(const CommandID: Integer; const ProcedureName, DatabaseAlias: string);
    procedure UpdateCommandShellExecuteParameters(const CommandID: Integer);
    procedure UpdateCommandReportParameters(const CommandID: Integer; const ActionType: TActionType);
    procedure UpdateCommandValueParameters(const CommandID, TableID: Integer);
    function AppendCommandParameter(const CommandID: Integer; const ParamTypeID, ParamHiddenID: Integer; const ParamName, ParamValue: string; const ParamColumn: Integer = 1): Boolean;
  protected
    procedure ActivateActions; override;
  public
    procedure TestConnection(Sender: TObject);
    procedure RegTables(Sender: TObject);
    procedure Consistency(Sender: TObject);

    procedure OpenThisTable(Sender: TObject);
    procedure TableExportData(Sender: TObject);
    procedure TableImportData(Sender: TObject);
    procedure TableClearData(Sender: TObject);

    procedure LoadSolution(Sender: TObject);
    procedure ExtractSolution(Sender: TObject);
    procedure DeleteSolution(Sender: TObject);

    procedure UpdateCommandParameters(Sender: TObject);
    procedure ExecuteSelectCommand(Sender: TObject);

    procedure DatabaseExportData(Sender: TObject);
    procedure DatabaseImportData(Sender: TObject);
    procedure DatabaseImportMetaData(Sender: TObject);

    procedure CreateShortLink(Sender: TObject);

    procedure ExportSolution(Sender: TObject);

    procedure TablePurgeData(Sender: TObject);
  end;

/// <summary>регистрация всех действий в глобальном списке</summary>
/// <remarks>вызывается при подключении метаданных</remarks>
procedure ActivateAllActions;

function MigrationTable(const SourceTM: TTableMeta; const TargetDB: TDeCustomDataBase): Boolean;

/// <summary>очистка глобального списка действий</summary>
/// <remarks>вызывается при отключении от метаданных</remarks>
procedure DeactivateAllActions;

var ProfitActions      : TDeActionList;
var MetaDataOperations : TMetaDataOperations;

////////////////////////////////////////////////////////////////////////////////

procedure DoImportData(aCacheList: TObjectList<TDataCache>);
procedure FileImportData(Sender: TObject);
procedure ClipBoardImportData(Sender: TObject);


{$IFDEF DEBUG}
function ActionTypeToString(const ActionType: TActionType): string;
{$ENDIF}

implementation

uses StrUtils, ShellAPI, Types, Zip, ShlObj, ComObj, ActiveX, Data.Win.ADODB,
     DeLog, Funcs, Security, DeMetaData, Dictionary, DataManager, DeControls,
     QueryDescriptor, DeCalculator, Main, DeSettings, SolutionOperations, IndicatorForm, StructureUnit,
     DSMeta, DeScript, DeCommandDlg, DeXMLReport, DeTextReport, ItemsOrder, DataUnit, UnitA,
     {$IFDEF MICROSOFTCOM}DeMicrosoftOfficeReport{$ELSE}DeXLReport{$ENDIF}, ConnectOperationsUnit,
     uIconUtils;

procedure DoImportData(aCacheList: TObjectList<TDataCache>);
var CCC, CC, N, M, LN, i, ii, j, f, KeyIndex: Integer;
    DC: TDataCache;
    DM: TDataManager;
    TM: TTableMeta;
    V, PreValue, NewValue, PreTypeValue, NewTypeValue, PreObjValue, NewObjValue : Variant;
    InPlace: Boolean;
    Links: array of Integer;


  function GetCacheByID(const aID: Integer): Integer;
  var ii: Integer;
  begin
    result:= -1;
    for ii:= 0 to Pred(aCacheList.Count) do
      if aCacheList[ii].TableMeta.ID = aID then Exit(ii);
  end;

  function GetNewValue(const aCache: TDataCache; const aPreValue: Variant; var aNewValue: Variant;
                                                                           const aLastIndex: Integer = MaxInt): Boolean;
  var ii, KeyField: Integer;
  begin
    Result:= False;
    aNewValue:= unassigned;
    try
      if aPreValue = 0 then Exit(True);

      KeyField:= aCache.Fields.IndexKeyField;
      for ii:= 0 to Min(aLastIndex, Pred(aCache.Count)) do
          if aCache.Items[ii].FieldBaseValue[KeyField] = aPreValue then
            begin
              aNewValue:= aCache.Items[ii].FieldValue[KeyField];
              Result:= True;
              Break;
            end;
    except
      // ключ может не совпадать по типу
    end;
  end;

begin

  N:= 0;
  // Сортировка. Переносим вверх таблицы, которые не ссылаются на оставшиеся в несортированном списке
  for i:= N to Pred(aCacheList.Count) do
    begin
      InPlace:= True;
      for j:= N to Pred(aCacheList.Count) do
        if ( i<>j ) and (-1 < aCacheList[i].TableMeta.Fields.IndexByLink(aCacheList[j].TableMeta.ID)) then
          begin
            InPlace:= False;
            Break;
          end;

       if InPlace then
         begin
           aCacheList.Exchange(i, N);
           Inc(N);
         end;
    end;

  // Сортировка. Переносим права, примечания и ссылки вниз. т.к. они содержат косвенные ссылки
  N:= Pred(aCacheList.Count);
  for i:= N to Pred(aCacheList.Count) do
    if (aCacheList[i].TableMeta = Metadata.MetaTables[idxRights]) or
       (aCacheList[i].TableMeta = Metadata.MetaTables[idxNotes]) or
       (aCacheList[i].TableMeta = Metadata.MetaTables[idxInterRelations]) then
      begin
        aCacheList.Exchange(i, N);
        Dec(N);
      end;

  // Сортировка. Cортируем древовидные кэши, чтобы "родители" были вверху
  for N:= 0 to Pred(aCacheList.Count) do
    begin
      F:= aCacheList[N].TableMeta.Fields.IndexByLink(aCacheList[N].TableMeta.ID);
      if (-1 < F) then
        for i:=0 to Pred(aCacheList[N].Count) do
          for j:=i+1 to Pred(aCacheList[N].Count) do
            if aCacheList[N].Items[j].ID = aCacheList[N].Items[i].FieldValue[F] then
              aCacheList[N].CacheOwnedItems.Exchange(i,j);
    end;

  //....................................................................................
  // вставляем новые записи, с новыми ключами и ПО ВОЗМОЖНОСТИ с новыми ссылками
  CCC:= 0;
  cc:=  0;
  for i:= 0 to Pred(aCacheList.Count) do
    Inc(CCC, aCacheList[i].Count);
  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, CCC, 1);

  for N:= 0 to Pred(aCacheList.Count) do
    begin
      // получаем TableMeta куда грузим
      TM:= nil;
      for i:= Low(MetaData.MetaTables) to High(MetaData.MetaTables) do
        if SameText(MetaData.MetaTables[i].Table, aCacheList[N].TableMeta.Table) then
          begin
            TM:= MetaData.MetaTables[i];
            Break;
          end;
      if Not assigned(TM) then Continue;

      DM:= CreateDataManager(TM);
      if TM.IsDirectory = idGlobal then DC:= MetaData.GetLibrary(TM.ID)
                                   else DC:= nil;
      KeyIndex:= aCacheList[N].Fields.IndexKeyField;

      // строим массив ссылок на ЗАГРУЖАЕМЫЕ наборы данных
      Setlength(Links, aCacheList[N].Fields.Count);
      for f:= 0 to Pred(aCacheList[N].Fields.Count) do
        begin
          Links[f]:= -1;
          if (0 < aCacheList[N].Fields[f].Link) and aCacheList[N].Fields[f].IsStored and  Not aCacheList[N].Fields[f].IsLookup then
            for i:= 0 to Pred(aCacheList.Count) do
               if aCacheList[i].TableMeta.ID = aCacheList[N].Fields[f].Link then
                 begin
                   Links[f]:= i;
                   Break;
                 end;
        end;

      //................................................................................................................
      for i:=0 to Pred(aCacheList[N].Count) do
        begin
          for f:=0 to Pred(aCacheList[N].Fields.Count) do
            if aCacheList[N].Fields[f].IsStored then
              begin

                // набор данных, на который указывает ссылка - уже вставлен, подменяем значения
                if (-1 < Links[f]) and (Links[f] < N) then
                    begin
                      PreValue:= aCacheList[N].Items[i].FieldValue[f];
                      if Not VarIsNull(PreValue) then
                        begin
                          // косвенные ссылки код мета-таблицы + код записи в ней
                          if (aCacheList[N].TableMeta = Metadata.MetaTables[idxRights]) and
                             (aCacheList[N].Fields[f].Original = fldRightsObjectType {MRH_OBJECTTYPE} ) then
                            begin
                              // ссылка на мета-таблицу
                              NewValue:= -1;
                              PreTypeValue:= aCacheList[N].Items[i].ValueByName[fldRightsObjectType+'.'+fldDataSetTable];
                              for ii:= Low(Metadata.MetaTables) to High(Metadata.MetaTables) do
                                if SameText(Metadata.MetaTables[ii].Table, PreTypeValue) then
                                  begin
                                    NewValue:=Metadata.MetaTables[ii].ID;
                                    Break;
                                  end;

                              V:= VarArrayCreate([0, 1], varVariant);
                              VarArrayPut(V, PreValue, [0]);
                              VarArrayPut(V, NewValue, [1]);
                              aCacheList[N].Items[i].InitFieldValue(f, V);

                              // ссылка на запись
                              M:= aCacheList[N].Fields.IndexByName(fldRightsObjectId);
                              if -1 < M then
                                begin
                                  NewValue:= unassigned;
                                  PreObjValue:= aCacheList[N].Items[i].FieldValue[M];
                                  for ii:= 0 to Pred(aCacheList.Count) do
                                    if SameText(aCacheList[ii].TableMeta.Table, PreTypeValue) then
                                      begin
                                        GetNewValue(aCacheList[ii], PreObjValue, NewObjValue);
                                        Break;
                                      end;
                                  V:= VarArrayCreate([0, 1], varVariant);
                                  VarArrayPut(V, PreObjValue, [0]);
                                  VarArrayPut(V, NewObjValue, [1]);
                                  aCacheList[N].Items[i].InitFieldValue(M, V);
                                end;
                            end
                          else
                            begin
                              if not GetNewValue(aCacheList[Links[f]], PreValue, NewValue) then
                                DeLog.WriteLog('ImportKeyError'+
                                               ' TableName:'+aCacheList[N].TableMeta.Table+
                                               ' RecordKey:'+String(aCacheList[N].Items[i].ID)+
                                               ' FieldName:'+aCacheList[N].Fields[f].Original);

                              V:= VarArrayCreate([0, 1], varVariant);
                              VarArrayPut(V, PreValue, [0]);
                              VarArrayPut(V, NewValue, [1]);
                              aCacheList[N].Items[i].InitFieldValue(f, V);
                            end;
                        end;
                    end else

                // ссылки на текущий набор данных (дерево), если уже вставили родителя - подменяем ссылку
                if (Links[f] = N) then
                  begin
                    PreValue:= aCacheList[N].Items[i].FieldValue[f];
                    if Not varIsNull(PreValue) then
                      begin
                        if GetNewValue(aCacheList[Links[f]], PreValue, NewValue, Pred(i)) then
                          begin
                            V:= VarArrayCreate([0, 1], varVariant);
                            VarArrayPut(V, PreValue, [0]);
                            VarArrayPut(V, NewValue, [1]);
                            aCacheList[N].Items[i].InitFieldValue(f, V);
                          end
                        else
                          aCacheList[N].Items[i].State:= aCacheList[N].Items[i].State + [isWaitValue];
                      end;
                  end else

                // набор данных, на который указывает ссылка - еще не вставлен, дополнительный update
                if (N < Links[f]) then
                  begin
                    PreValue:= aCacheList[N].Items[i].FieldValue[f];
                    if Not VarIsNull(PreValue) then
                      begin
                        aCacheList[N].Items[i].State:= aCacheList[N].Items[i].State + [isWaitValue];
                      end;
                  end;
              end;

          // Глобальный справочник - при совпадении ключа обновляем
          if assigned(DC) then M:= -1//DC.IndexByID(aCacheList[N].Items[i].FieldBaseValue[KeyIndex])
                          else M:= -1;

          if -1 < M then
            begin
              aCacheList[N].Items[i].State:= aCacheList[N].Items[i].State + [isModified];
              DM.UpdateRecord(aCacheList[N].Items[i]);
            end
          else
            begin
              aCacheList[N].Items[i].State:= aCacheList[N].Items[i].State + [isInserted];
              V:= VarArrayCreate([0, 1], varVariant);
              VarArrayPut(V, aCacheList[N].Items[i].FieldValue[KeyIndex], [0]);
              VarArrayPut(V, unassigned, [1]);
              aCacheList[N].Items[i].InitFieldValue(KeyIndex, V);
              DM.InsertRecord(aCacheList[N].Items[i]);
            end;

          inc(cc, 1);
          SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, cc, 0);
        end;

      DM.Free;
    end;
  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);

  // проставляем значения ссылок, которые НЕ БЫЛИ ИЗВЕСТНЫ на момент вставки
  for N:= 0 to Pred(aCacheList.Count) do
      begin
        DeLog.WriteLog('Update '+IntToStr(N));
        TM:= nil;
        for i:= Low(MetaData.MetaTables) to High(MetaData.MetaTables) do
          if SameText(MetaData.MetaTables[i].Table, aCacheList[N].TableMeta.Table) then
            begin
              TM:= MetaData.MetaTables[i];
              Break;
            end;
        if Not assigned(TM) then Continue;

        DeLog.WriteLog('Update '+TM.Table);

        DM:= CreateDataManager(TM);
        // строим массив ссылок на ЗАГРУЖАЕМЫЕ наборы данных
        Setlength(Links, aCacheList[N].Fields.Count);
        for f:= 0 to Pred(aCacheList[N].Fields.Count) do
          begin
            Links[f]:= -1;
            if (0 < aCacheList[N].Fields[f].Link) and aCacheList[N].Fields[f].IsStored and  Not aCacheList[N].Fields[f].IsLookup then
              for i:= 0 to Pred(aCacheList.Count) do
                if aCacheList[i].TableMeta.ID = aCacheList[N].Fields[f].Link then
                  begin
                    Links[f]:= i;
                    Break;
                  end;
          end;

        DeLog.WriteLog('Update 2'+TM.Table);

        for i:= 0 to Pred(aCacheList[N].Count) do
          if isWaitValue in aCacheList[N].Items[i].State then
            begin
              DeLog.WriteLog('ImportKey'+
                             ' TableName:'+aCacheList[N].TableMeta.Table+
                             ' RecordKey:'+String(aCacheList[N].Items[i].ID), 'LoadAndUpdate');

              for f:=0 to Pred(aCacheList[N].Fields.Count) do
                if (-1 < Links[f]) then
                  begin
                    PreValue:= aCacheList[N].Items[i].FieldBaseValue[f];
                    if not VarIsNull(PreValue) then
                      begin
                        if not GetNewValue(aCacheList[Links[f]], PreValue, NewValue) then
                          DeLog.WriteLog('ImportKeyError'+
                                         ' TableName:'+aCacheList[N].TableMeta.Table+
                                         ' RecordKey:'+String(aCacheList[N].Items[i].ID)+
                                         ' FieldName:'+aCacheList[N].Fields[f].Original);

                          V:= VarArrayCreate([0, 1], varVariant);
                          VarArrayPut(V, PreValue, [0]);
                          VarArrayPut(V, NewValue, [1]);
                          aCacheList[N].Items[i].InitFieldValue(f, V);
                      end;
                  end;
              DM.UpdateRecord(aCacheList[N].Items[i]);
            end;
        DM.Free;
      end;
end;

procedure FileImportData(Sender: TObject);
begin
  //
end;

procedure ClipBoardImportData(Sender: TObject);
begin
  //
end;

{$IFDEF ACTIONICONTYPED}
{$R DeActions.res}
{$ENDIF}

{$IFDEF DEBUG}
function ActionTypeToString(const ActionType: TActionType): string;
begin
  case ActionType of
    atGroup: Result := 'atGroup';
    atInner: Result := 'atInner';
    atStoredProcedure: Result := 'atStoredProcedure';
    atShellExecute: Result := 'atShellExecute';
    atSetValue: Result := 'atSetValue';
    atPreview: Result := 'atPreview';
    //atDLL: Result := 'atDLL';
    atReport: Result := 'atReport';
    atInsertValue: Result := 'atInsertValue';
    atUpdateValue: Result := 'atUpdateValue';
    atDataSet: Result := 'atDataSet';
    atCalculator: Result := 'atCalculator';
  else
    Result := IntToStr(Ord(ActionType));
  end;
end;
{$ENDIF}

var ContainerStorage: TActionContainerStorage;

procedure ActivateAllActions;
begin
  {$IFDEF DEBUG}
  DebugLog('ActivateAllActions start ...');
  {$ENDIF}
  ContainerStorage.ActivateAll;
  {$IFDEF DEBUG}
  DebugLog('ActivateAllActions finish ...');
  {$ENDIF}
end;

procedure DeactivateAllActions;
begin
  {$IFDEF DEBUG}
  DebugLog('DeactivateAllActions start ...');
  {$ENDIF}
  ContainerStorage.DeactivateAll;
  {$IFDEF DEBUG}
  DebugLog('DeactivateAllActions finish ...');
  {$ENDIF}
end;

{$REGION 'Class: TActionData ...'}
constructor TDeActionData.Create;
begin
  inherited Create;
  FID         := -1;
  FICO        := -1;
  FFields     := TFieldsMeta.Create;
  FEnabledPostfix := nil;
  FVisiblePostfix := nil;
  FActive := True;
  FDefaultMode := amMultiIn; // до v.16.10 это стандартное поведение при выполнении действия
end;

constructor TDeActionData.Create(const aActionName: string; aOnExecute: TActionExecuteMethod; const aTable: Integer);
begin
  inherited Create;
  FID         := -1;
  FICO        := -1;
  FType       := atInner;
  FCaption    := GetTitle(aActionName, ttFull);
  FProcedure  := unassigned;
  FOnExecute  := aOnExecute;
  FDataSet    := aTable;
  FFields     := TFieldsMeta.Create;
  FEnabledPostfix := nil;
  FVisiblePostfix := nil;
  FActive := True;
  FDefaultMode := amMultiIn; // до v.16.10 это стандартное поведение при выполнении действия
end;

constructor TDeActionData.Create(const aActionName: string; const aProcedureName: string; const aTable: Integer);
begin
  inherited Create;
  FID         := -1;
  FICO        := -1;
  FType       := atStoredProcedure;
  FCaption    := GetTitle(aActionName, ttFull);
  FProcedure  := aProcedureName;
  FOnExecute  := nil;
  FDataSet    := aTable;
  FFields     := TFieldsMeta.Create;
  FEnabledPostfix := nil;
  FVisiblePostfix := nil;
  FActive := True;
  FDefaultMode := amMultiIn; // до v.16.10 это стандартное поведение при выполнении действия
end;

destructor TDeActionData.Destroy;
begin
  if Assigned(FEnabledPostfix) then FEnabledPostfix.Free;
  if Assigned(FVisiblePostfix) then FVisiblePostfix.Free;
  FFields.Free;
  inherited Destroy;
end;

procedure TDeActionData.CheckCategory;
var
  S: string;
begin
  S := CutTextValue(FSubCategories, '\');
  if SameText(S, '_root') then
    FCategory := acRoot
  else if SameText(S, '_file') then
    FCategory := acFile
  else if SameText(S, '_create') then
    FCategory := acCreate
  else if SameText(S, '_print') then
    FCategory := acPrint
  else if SameText(S, '_edit') then
    FCategory := acEdit
  else if SameText(S, '_service') then
    FCategory := acService
  else if SameText(S, '_operation') then
    FCategory := acOperation
  else if SameText(S, '_action') then
    FCategory := acAction
  else if SameText(S, '_command') then
    FCategory := acCommand
  else if SameText(S, '_help') then
    FCategory := acHelp
  else
    begin
      FCategory := acNone;
      if Length(S) <> 0 then
        begin
          if Length(FSubCategories) <> 0 then
            S := S + '\';
          FSubCategories := S + FSubCategories;
        end;
    end;
end;

procedure TDeActionData.CheckGroup;
var
  Index: Integer;
begin
  if (ActionType = atGroup) and (Length(Trim(ActionProcedure)) <> 0) then
    begin
      if not Assigned(FActions) then
        FActions := TStringList.Create;
      FActions.Text := FProcedure;
      for Index := Pred(FActions.Count) downto 0 do
        if Length(Trim(FActions[Index])) = 0 then
          FActions.Delete(Index);
    end
  else
    FreeAndNil(FActions);
end;

function TDeActionData.CheckPolicy(const Operation: TSecurityOperation): Boolean;
begin
  case FActionSource of
    asNone: { Системные действия приложения }
      Result := True;
    asCommands: { Команды из таблицы M_COMMANDS }
      Result := SecuritySystem.CheckPolicyCommand(ID, Operation);
  else
    Result := False;
  end;
end;

procedure TDeActionData.Assign(Source: TObject);
var
  N: Integer;
begin
  if Source is TDeDataSet then
    with TDeDataset(Source) do
      begin
        // Источником является M_COMMANDS ...
        FActionSource := asCommands;

        FID := VarToInt(ValueByName[fldCommandID]);
        FDataSet := VarToInt(ValueByName[fldCommandDataSet]);
        FCaption := GetTitle(StringValueByName(fldCommandName), ttFull){$IFDEF DEBUG} + ' [' + IntToStr(FID) + ']'{$ENDIF};
        FType := TActionType(ValueByName[fldCommandType]);
        FQuestion := StringValueByName(fldCommandQuestion);

        FICO := VarToInt(ValueByName[fldCommandICO]);
        FOriginal := ValueByName[fldCommandOriginal];
        FFile := ValueByName[fldCommandFile];
        FProcedure := StringValueByName(fldCommandProcedure);
        FParams := StringValueByName(fldCommandParam);
        FEnabledStr := StringValueByName(fldCommandEnabled);
        FVisibleStr := StringValueByName(fldCommandVisible);

        FBreakLine := VarToInt(ValueByName[fldCommandBreak]) <> 0;
        FActive := VarToInt(ValueByName[fldCommandActive]) <> 0;
        FShortCut := TextToShortCut(ValueByName[fldCommandShortCut]);
        FOrder := VarToInt(ValueByName[fldCommandOrder]);

        FSolution := TDeDataset(Source).ValueByNameDef(fldCommandSolution, null);
        FBeforeScript := TDeDataset(Source).StringValueByNameDef(fldCommandBefore, EmptyStr);
        FAfterScript := TDeDataset(Source).StringValueByNameDef(fldCommandAfter, EmptyStr);

        N:=TDeDataset(Source).IndexByName(fldCommandGUID);
        if -1 < N then
          try
            if not VarIsNull(Value[N]) then
              begin
                FGUID := StringToGUID(VarToStr(Value[N]));
                Include(FStates, alsGUID);
              end;
          except
            {$IFDEF DEBUG}
            on E: Exception do
              DebugLog('TDeActionData.Assign skip read GUID error: ' + E.Message);
            {$ENDIF}
          end;

        FSubCategories      := ReplaceText(ValueByName[fldCommandCategory], '/', '\');
        // Если не определена даже корневая категория, то "складываем всё в помойку ~Действия~" ...
        if Length(FSubCategories) = 0 then
          begin
            if FType in [atPreview, atReport]
              then FSubCategories := '_print'
              else FSubCategories := '_action';
          end;
        // Проверим корневую категорию ...
        CheckCategory;

        // Проверим группу действий ...
        CheckGroup;

        // Проверяем режим действия по умолчанию ...
        CheckDefaultMode;

        FreeAndNil(FEnabledPostfix);
        FreeAndNil(FVisiblePostfix);
      end
  else if Source is TCacheItem then
    with TCacheItem(Source) do
      begin
        // Источником является M_COMMANDS ...
        FActionSource := asCommands;

        FID                 := VarToInt(ValueByName[fldCommandID]);
        FDataSet            := VarToInt(ValueByName[fldCommandDataSet]);
        FCaption            := GetTitle(ValueNativeByName[ValueByName[fldCommandName]], ttFull);
        FType               := TActionType(ValueByName[fldCommandType]);
        FQuestion           := ValueNativeByName[fldCommandQuestion];

        FICO                := VarToInt(ValueByName[fldCommandICO]);
        FOriginal           := ValueByName[fldCommandOriginal];

        FFile               := ValueNativeByName[fldCommandFile];
        FProcedure          := ValueNativeByName[fldCommandProcedure];
        FParams             := ValueNativeByName[fldCommandParam];

        FEnabledStr         := ValueNativeByName[ValueByName[fldCommandEnabled]];
        FVisibleStr         := ValueNativeByName[ValueByName[fldCommandVisible]];
        FBreakLine          := VarToInt(ValueByName[fldCommandBreak]) <> 0;
        FActive             := VarToInt(ValueByName[fldCommandActive]) <> 0;

        FShortCut           := TextToShortCut(ValueNativeByName[ValueByName[fldCommandShortCut]]);
        FOrder              := VarToInt(ValueByName[fldCommandOrder]);

        FBeforeScript       := ValueNativeByName[fldCommandBefore];
        FAfterScript        := ValueNativeByName[fldCommandAfter];

        N:= TCacheItem(Source).Owner.Fields.IndexByName(fldCommandGUID);
        if N <> -1 then
          try
            if not VarIsNull(FieldValue[N]) then
              begin
                FGUID := StringToGUID(VarToStr(FieldValue[N]));
                Include(FStates, alsGUID);
              end;
          except
            {$IFDEF DEBUG}
            on E: Exception do
              DebugLog('TDeActionData.Assign skip read GUID error: ' + E.Message);
            {$ENDIF}
          end;

        FSubCategories := ReplaceText(ValueByName[fldCommandCategory], '/', '\');
        // Если не определена даже корневая категория, то "складываем всё в помойку ~Действия~" ...
        if Length(FSubCategories) = 0 then FSubCategories := '_action';
        // Проверим корневую категорию ...
        CheckCategory;

        // Проверим группу действий ...
        CheckGroup;

        // Проверяем режим действия по умолчанию ...
        CheckDefaultMode;

        FreeAndNil(FEnabledPostfix);
        FreeAndNil(FVisiblePostfix);
      end
  else if Source is TDeActionData then
    begin
      FActionSource       := TDeActionData(Source).FActionSource;
      FBreakLine          := TDeActionData(Source).BreakLine;
      FActive             := TDeActionData(Source).Active;
      FOriginal           := TDeActionData(Source).Original;
      FICO                := TDeActionData(Source).ICO;

      FID                 := TDeActionData(Source).ID;
      FDataSet            := TDeActionData(Source).DataSet;
      FCaption            := TDeActionData(Source).Caption;
      FType               := TDeActionData(Source).ActionType;
      FQuestion           := TDeActionData(Source).Question;

      FFile               := TDeActionData(Source).ActionFile;
      FProcedure          := TDeActionData(Source).ActionProcedure;
      FParams             := TDeActionData(Source).ActionParams;

      FEnabledStr         := TDeActionData(Source).EnableStr;
      FVisibleStr         := TDeActionData(Source).VisibleStr;
      FShortCut           := TDeActionData(Source).ShortCut;
      FOrder              := TDeActionData(Source).Order;

      FActionSource       := TDeActionData(Source).FActionSource;
      FCategory           := TDeActionData(Source).Category;
      FSubCategories      := TDeActionData(Source).SubCategories;

      FDataSetIDs         := TDeActionData(Source).FDataSetIDs;

      FBeforeScript       := TDeActionData(Source).BeforeScript;
      FAfterScript        := TDeActionData(Source).AfterScript;

      if alsGUID in TDeActionData(Source).States then
        begin
          FGUID := TDeActionData(Source).GUID;
          Include(FStates, alsGUID);
        end;

      // Проверим группу действий ...
      CheckGroup;

      // Проверяем режим действия по умолчанию ...
      CheckDefaultMode;

      FreeAndNil(FEnabledPostfix);
      FreeAndNil(FVisiblePostfix);
    end;
end;

function TDeActionData.Refresh: Boolean;
var
  CommandDataSet, ParamsDataSet: TDeDataSet;
  TableMeta: TTableMeta;
  R, T, DatasetIndex: Integer;
begin
  Result := False;
  if Assigned(MetaData.MetaTables[idxCommands]) then
    begin
      CommandDataSet := MetaData.MetadataDB.CreateQuery(qtSelect);
      try
        CommandDataSet.Descr.Table := tblCommand;
        CommandDataSet.Descr.AddCondition(fldCommandID, ftInteger, opEQ, Self.ID);
        CommandDataSet.Descr.AddFields([fldCommandID, fldCommandDataSet, fldCommandICO, fldCommandType, fldCommandOrder,
          fldCommandName, fldCommandCategory, fldCommandQuestion, fldCommandBreak, fldCommandActive, fldCommandFile,
          fldCommandProcedure, fldCommandParam, fldCommandEnabled, fldCommandVisible, fldCommandShortCut,
          fldCommandOriginal, fldCommandBefore, fldCommandAfter]);

        CommandDataSet.Open;
        for R:=0 to Pred(CommandDataSet.RecordCount) do
          begin
            CommandDataSet.RecNo:= R;
            Assign(CommandDataSet);
            DatasetIndex := 0;
            ParamsDataSet := MetaData.MetadataDB.CreateQuery(qtSelect);
            try
              ParamsDataSet.Descr.BeginUpdate;
              try
                ParamsDataSet.Descr.Table := tblCommandParams;
                ParamsDataSet.Descr.AddCondition(fldCommandParamCommand, ftInteger, opEQ, Self.ID);
                ParamsDataSet.Descr.AddCondition(fldCommandParamType, ftInteger, opEQ, Integer(etDefault)); // = 6
                ParamsDataSet.Descr.AddCondition(fldCommandParamHidden, ftInteger, opEQ, 0); // Только не скрытые!!!
                ParamsDataSet.Descr.AddOperation(opAnd);
                ParamsDataSet.Descr.AddOperation(opAnd);
                ParamsDataSet.Descr.AddSortField(fldCommandParamName);
                ParamsDataSet.Descr.AddField(fldCommandParamDataSet);
              finally
                ParamsDataSet.Descr.EndUpdate;
              end;
              ParamsDataSet.Open;
              SetLength(FDataSetIDs, ParamsDataSet.RecordCount);
              for T:=0 to Pred(ParamsDataSet.RecordCount) do
                begin
                  ParamsDataSet.RecNo:= T;
                  TableMeta := MetaData.GetTableMeta(ParamsDataSet.Value[0]); // fldCommandParamDataSet
                  if Assigned(TableMeta) then
                    begin
                      FDataSetIDs[DatasetIndex] := TableMeta.ID;
                      Inc(DatasetIndex);
                    end;
                end;
            finally
              ParamsDataSet.Free;
            end;
            SetLength(FDataSetIDs, DatasetIndex);
            Result := True;
          end;
      finally
        CommandDataSet.Free;
      end;
    end;
end;

function TDeActionData.GetActionCount: Integer;
begin
  if Assigned(FActions) then
    Result := FActions.Count
  else
    Result := 0;
end;

function TDeActionData.GetActions(const Index: Integer): TDeActionData;
var
  ActionIndex: Integer;
begin
  Result := nil;
  if Assigned(ProfitActions) and Assigned(FActions) and (Index >= 0) and (Index < FActions.Count) then
    begin
      Result := FActions.Objects[Index] as TDeActionData;
      if not Assigned(Result) then
        begin
          ActionIndex := ProfitActions.IndexByOriginal(FActions[Index]);
          if ActionIndex <> -1 then
            begin
              Result := ProfitActions[ActionIndex];
              FActions.Objects[Index] := Result;
            end;
        end;
    end;
end;

function TDeActionData.GetActionNames(const Index: Integer): string;
begin
  if Assigned(FActions) and (Index >= 0) and (Index < FActions.Count) then
    Result := FActions[Index]
  else
    Result := EmptyStr;
end;

function TDeActionData.GetEnabledPostfix : TExpressionItem;
var FParser: TDeParser;
begin
  if not Assigned(FEnabledPostfix) then
    begin
      FEnabledPostfix := TExpressionItem.Create;
      if Length(FEnabledStr) > 0 then
        begin
          FParser := TDeParser.Create;
          try
            if (0 < FDataSet) then
              FParser.Table := MetaData.GetTableMeta(FDataSet);
            FParser.onGetIdentType := GetIdentType;
            FParser.Parse(FEnabledStr, FEnabledPostfix);
          finally
            FParser.Free;
          end;
        end
    end;
  Result := FEnabledPostfix;
end;

function TDeActionData.GetVisiblePostfix : TExpressionItem;
var FParser     : TDeParser;
begin
  if not Assigned(FVisiblePostfix) then
    begin
      FVisiblePostfix := TExpressionItem.Create;
      if Length(FVisibleStr) > 0 then
        begin
          FParser := TDeParser.Create;
          if (0 < FDataSet) then
            FParser.Table := MetaData.GetTableMeta(FDataSet);
          FParser.Parse( FVisibleStr, FVisiblePostfix);
          FParser.Free;
        end
    end;
  Result:= FVisiblePostfix;
end;

procedure TDeActionData.ParseParams(const aString: string; const DoClear: Boolean);
var TF: TFieldMeta;
    s: string;
    p: Integer;
begin
  if DoClear then
    FFields.Clear;

  s:=Trim(aString);
  if Length(s)=0 then Exit;

  if (s[1]='(') and (s[Length(s)]=')') then s:=Copy(s,2,Length(s)-2);

  s:=Trim(s);
  if Length(s)=0 then Exit;

  p:=Pos(';',s);

  if p>0 then begin
                ParseParams(Copy(s,1,p-1), False);
                ParseParams(Copy(s,p+1,MaxInt), False);
              end
         else begin
                TF:=TFieldMeta.Create;
                p:=Pos('=',s);
                if p>0 then begin
                              TF.Original:= Trim(Copy(s,1,p-1));
                              TF.DefaultValue:= Trim(Copy(s,p+1,MaxInt));
                            end
                       else begin
                              TF.Original:= s;
                              TF.DefaultValue:= s;
                            end;
                Fields.Add(TF);
              end;
end;

procedure TDeActionData.Execute(const Mode: TActionMode);
var
  Variables: TDeVariableList;
  WorkMode: TActionMode;
begin
  if Mode = amDefault then WorkMode := FDefaultMode else WorkMode := Mode;
  Variables := TDeVariableList.Create;
  try
    if ActionType = atPreview then Variables.SetByName(varPrinterName, Null);
    if TDeCommandDialog.Execute(Self, Variables, True, WorkMode) = mrOK then
      begin
        ProfitActions.BeforeExecute(Self);
        try
          Execute(Variables, False, WorkMode);
        finally
          ProfitActions.AfterExecute(Self, Variables);
        end;
      end;
  finally
    Variables.Free;
  end;
end;

procedure TDeActionData.Execute(Variables: TDeVariableList; const QuestionEnabled: Boolean; const Mode: TActionMode);
var
  WorkMode: TActionMode;
  AM : TDataSetMeta;
begin
  if not CheckPolicy(spExecute) then
    begin
      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar('_eRror.right')));
      Exit;
    end;

  AM:=MForm.ActiveMeta;
  if Assigned(AM) then
    AM.Cache.BeginUpdate;

  try
    // Выполняем скрипт ДО выполнения действия ...
    ExecuteScript(FBeforeScript, Variables);
    {$IFDEF DEBUG}
    if Length(FBeforeScript) <> 0 then
      DebugLog('Action before execute script completed ...'#13#10'                        ' + FBeforeScript);
    {$ENDIF}
    if Mode = amDefault then WorkMode := FDefaultMode else WorkMode := Mode;

    with Variables.GetByName(varActionResult) do
      begin
        IsOutput:= True;
        Value:= 0; // По умолчанию ставим 0 результатом выполнения действия
      end;
    case ActionType of
      atGroup: { группа действий }
        ExecuteGroup(Variables, QuestionEnabled, WorkMode);
      atInner: { встроенное в приложение действие }
        ExecuteInner;
      atStoredProcedure: { вызов хранимой процедуры }
        ExecuteStoredProcedure(Variables);
      atShellExecute: { команда операционной системы }
        ExecuteShellExecute(Variables);
      atSetValue: { установка значения }
        ExecuteSetValue;
      //atDLL: { команда плагина }
      //  // Нет реализации даже в v.15.10!!! Тупо пропускаем ...
      //  WriteLog('DLL action %d not executed ...', [ID], 'Actions');
      atPreview,
      atReport: { формирование отчёта }
        ExecuteReport(Variables, QuestionEnabled, WorkMode);
      atInsertValue: { Вставка новой записи }
        if SecuritySystem.CheckPolicyDataset(DataSet, spInsert) then
          ExecuteInsertValue(Variables, QuestionEnabled, WorkMode)
        else
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar('_eRror.right')));
      atUpdateValue: { Обновление существующей записи }
        if SecuritySystem.CheckPolicyDataset(DataSet, spUpdate) then
          ExecuteUpdateValue(Variables, QuestionEnabled, WorkMode)
        else
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar('_eRror.right')));
      atDataSet: { Открытие набора данных }
        ExecuteDataSet;
      atCalculator: { Скрипт калькулятора }
        ExecuteCalculator(Variables, QuestionEnabled, WorkMode);
    end;
  finally
    if Assigned(AM) then
      if AM = MForm.ActiveMeta then
        AM.Cache.EndUpdate;
  end;
  // Выполняем скрипт ПОСЛЕ выполнения действия ...
  ExecuteScript(FAfterScript, Variables);
  {$IFDEF DEBUG}
  if Length(FAfterScript) <> 0 then
    DebugLog('Action after execute script completed ...'#13#10'                        ' + FAfterScript);
  {$ENDIF}
end;

procedure TDeActionData.ExecuteCalculator(Variables: TDeVariableList; const QuestionEnabled: Boolean; const Mode: TActionMode);
var
  VariableList: TDeVariableList;
  ListManager: TDeVariableListManager;
  Calculator: TDeCalculator;
//  VariableListHelper: TDeVariableListHelper;
  Parser: TDeParser;
  ExpressionItem: TExpressionItem;
  ItemIndex: Integer;
  function Calculate(aPostfix: TExpressionItem; const aDefaultResult: Variant): Variant;
  begin
    if Assigned(aPostfix) and (aPostfix.Count > 0) then
      Result := Calculator.Calculate(aPostfix)
    else
      Result := aDefaultResult;
  end;
begin
  VariableList := TDeVariableList.Create;
  try
    if Assigned(Variables) then
      VariableList.CopyFrom(Variables)
    else
      if TDeCommandDialog.Execute(Self, VariableList, QuestionEnabled, Mode) <> mrOK then
        Exit;
    // Разворачиваем массивы при необходимости в список для цикла ...
    ListManager := VariableList.ExtractArrayList;
    try
      {$IFDEF DEBUG}
      ListManager.DebugVariablesListLog('Extract array list completed ...');
      {$ENDIF}
      Calculator := TDeCalculator.Create;
      try
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, ListManager.Count, 1);
        try
          for ItemIndex := 0 to Pred(ListManager.Count) do
            begin
              SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, ItemIndex, 0);
              Calculator.OnGetIdentValue := ListManager[ItemIndex].GetIdentValue;
              if VarAsType(Calculate(EnabledPostfix, True), varBoolean) then
                begin
                  Parser := TDeParser.Create;
                  ExpressionItem := TExpressionItem.Create;
                  try
                    if FDataSet <> 0 then
                      Parser.Table := MetaData.GetTableMeta(FDataSet);
                    Parser.onGetIdentType := GetIdentType;
                    Parser.onGetVariable := ListManager[ItemIndex].GetVariableItem;
                    try
                      Parser.Parse(ActionProcedure, ExpressionItem);
                    except
                      on E: EDeParserError do
                        if E.Error <> Error_EmptyScript then
                          begin
                            if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                              ProfitActions.FActionResults.Add('error:' + E.Message)
                            else
                              SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
                                          NativeInt(PChar(Caption + #10#13 + E.Message + #10#13'_dL.procedureerror')));
                            Exit;
                          end;
                    end;
                    Calculator.Calculate(ExpressionItem);
                    {$IFDEF DEBUG}
                    ListManager[ItemIndex].DebugVariablesLog('Calculate variables executed ...');
                    {$ENDIF}
                  finally
                    ExpressionItem.Free;
                    Parser.Free;
                  end;
                end;
            end;
        finally
          SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
        end;
      finally
        Calculator.Free;
      end;
    finally
      ListManager.Free;
    end;
  finally
    VariableList.Free;
  end;
end;

function TDeActionData.IsSupportLinkDataSet(const DataSetID: Integer): Boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := Low(FDataSetIDs) to High(FDataSetIDs) do
    if FDataSetIDs[Index] = DataSetID then
      begin
        Result := True;
        Break;
      end;
end;

function TDeActionData.IsSupportDataSet(const DataSetID: Integer): Boolean;
begin
  Result := ((FDataSet = 0) and (not (ActionType in [atPreview, atReport]))) or
             (FDataSet = DataSetID) or
             ((FDataSet = 0) and (ActionType in [atPreview, atReport]) and (Category <> acPrint));
  if not Result then
    Result := IsSupportLinkDataSet(DataSetID);
end;

function TDeActionData.IsContextDataSet(const DataSetID: Integer): Boolean;
begin
  Result := ((FDataSet = DataSetID) or IsSupportLinkDataSet(DataSetID));
end;

function TDeActionData.CheckActionType(const ActionType: TActionType): Boolean;
var
  Index: Integer;
  ActionData: TDeActionData;
begin
  Result := ActionType = FType;
  if not Result then
    for Index := 0 to Pred(ActionCount) do
      begin
        ActionData := Actions[Index];
        if Assigned(ActionData) and ActionData.CheckActionType(ActionType) then
          begin
            Result := True;
            Break;
          end;
      end;
end;

function TDeActionData.PrepareReportVariablesFileSuffix(Variables: TDeVariableList): string;
var
//  NeedCaption : Boolean;
  {i, N, }Index: Integer;
  Name: string;
  Value: Variant;
//  Cache : TDataCache;
begin
  Result := EmptyStr;
  if Assigned(Variables) then
    for Index := 0 to Pred(Variables.Count) do
      begin
        Name := Variables[Index].Name;
        if not (SameText(Name, varSourceFileName) or SameText(Name, varTargetFileName) or
                SameText(Name, varShowResult) or SameText(Name, varActionResult) or SameText(Name, varPrinterName) or
                SameText(Name, varActionType) or SameText(Name, varActionID) or SameText(Name, varActionOriginal) or
                SameText(Name, varButtonPrint) or SameText(Name, varButtonPreview) or
                SameText(Name, varButtonExport) ) then
          begin
            Value := Variables[Index].Value;
            if Variables[Index].DataSetID > 0 then
              Value := DatasetRecordCaption(Variables[Index].DataSetID, Value);
            {
            // NeedCaption - надо подстваить заголовок по id
            NeedCaption:= (0 < Variables[Index].DataSetID);

            if NeedCaption then
              for i:=0 to Metadata.LibraryCount-1 do
                if Metadata.LibraryData[i].TableMeta.ID = Variables[Index].DataSetID then
                  begin
                    N:= Metadata.LibraryData[i].IndexByID(Value);
                    if -1 < N then
                      begin
                        NeedCaption := False;
                        Value := Metadata.LibraryData[i].Items[N].Caption;
                      end;
                    Break;
                  end;

            if NeedCaption then
              begin
                try
                  Cache := TDataCache.Create( Metadata.GetTableMeta(Variables[Index].DataSetID) );
                  Cache.Filters.AddFilter(TFilterItem.Create(Cache.TableMeta.KField[0], opEQ, Value));
                  Cache.PrepareData;

                  if Cache.Count=1 then
                     Value := Cache[0].Caption;
                finally
                  Cache.Free;
                end;
              end;
            }
            if VarIsType(Value, varBoolean) then
              if Value then Result := Result + ' V'
                       else Result := Result + ' -'
            else
                            Result := Result + ' ' + VarToStr(Value);
          end;
      end;
end;

function TDeActionData.PrepareReport(Variables: TDeVariableList): TDeReport;
var
  TableMeta: TTableMeta;
  Image: TDeImage;
  DataSet: TDeDataset;
  FilePath, FileNameOnly, FileExtension, FileName: string;
  Index: Integer;
  Text: AnsiString;
begin
  Result := nil;
  if FActionSource = asCommands then
    begin
      TableMeta := MetaData.MetaTables[idxCommands];
      if Assigned(TableMeta) and Assigned(TableMeta.Database) then
        begin
          Image := TDeImage.Create(nil);
          try
            DataSet := TableMeta.Database.CreateQuery(qtSelect);
            try
              DataSet.Descr.BeginUpdate;
              try
                DataSet.Descr.Table := TableMeta.Table;
                DataSet.Descr.AddField(fldCommandData);
                DataSet.Descr.AddParamCondition(fldCommandID, ftInteger, opEQ, 'ID', ID);
              finally
                DataSet.Descr.EndUpdate;
              end;
              DataSet.Open;
              Image.Value := DataSet.Value[0]; // fldCommandData
            finally
              DataSet.Free;
            end;
            FilePath := IncludeTrailingPathDelimiter(DeSettings.Variables.AsString[RegDirPath]);
            FileNameOnly := NormalizeFileName(Caption + PrepareReportVariablesFileSuffix(Variables), True);
            FileExtension := ExtractFileExt(Image.OriginalFileName);
            Index := 1;
            FileName := FilePath + FileNameOnly + FileExtension;
            while FileExists(FileName) and (Index < 1000) do
              begin
                Inc(Index);
                FileName := FilePath + FileNameOnly + ' ('+ Trim(Format('%3.3d', [Index])) + ')' + FileExtension;
              end;
            {$IFDEF MICROSOFTCOM}
            if TMicrosoftExcelReport.IsSupportExtension(FileExtension) then
              begin
                Image.SaveToFile(FileName);
                Result := TMicrosoftExcelReport.Create(FileName);
              end
            else if TMicrosoftWordReport.IsSupportExtension(FileExtension) then
              begin
                Image.SaveToFile(FileName);
                Result := TMicrosoftWordReport.Create(FileName);
              end
            {$ELSE}
            if TDeExcelReport.IsSupportExtension(FileExtension) then
              begin
                Image.SaveToFile(FileName);
                Result := TDeExcelReport.Create(FileName);
              end
            else if TDeWordReport.IsSupportExtension(FileExtension) then
              begin
                Image.SaveToFile(FileName);
                Result := TDeWordReport.Create(FileName);
              end
            {$ENDIF}
            else if TDeTextReport.IsSupportExtension(FileExtension) then
              begin
                Result := TDeTextReport.Create(FileName);
                Image.SaveToString(Text);
                (Result as TDeTextBasedReport).DeTemplate.Text := Text;
              end
            else
              begin
                Result := TDeSimpleReport.Create(FileName);
                Image.SaveToString(Text);
                (Result as TDeSimpleReport).DeTemplate.Text := Text;
              end;
            Result.ReportName := Caption;
            DM.RegisterTemporaryFile(FileName);
          finally
            Image.Free;
          end;
        end;
    end;
end;

procedure TDeActionData.ExecuteInner;
begin
  // передаем целиком aCache, перебор делается внутри метода
  if Assigned(FOnExecute) then FOnExecute(Self);
end;

procedure TDeActionData.ExecuteSetValue;
var
  DataCache: TDataCache;
  Parser: TDeParser;
  ExpressionItem: TExpressionItem;
  FieldIndex, CacheIndex: Integer;
  Value: Variant;
  ValueType: Word;
begin
  if DataSet <> -1 then
    begin
      DataCache := MetaData.FindActiveDataCache(DataSet, True);
      if Assigned(DataCache) then
        begin
          if DataCache.SelectedCount = 0 then
            begin
              if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                ProfitActions.FActionResults.Add('info:' + GetTitle('_eRror.noselectedobject'))
              else
                SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(GetTitle('_eRror.noselectedobject'))));
              Exit;
            end;

          FieldIndex := DataCache.TableMeta.Fields.IndexByName(ActionProcedure);
          if FieldIndex = -1 then
            begin
              if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                ProfitActions.FActionResults.Add('error:' + Format(GetTitle('_eRror.fieldnotexists'), [ActionProcedure]))
              else
                SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25,
                            NativeInt(PChar(Format(GetTitle('_eRror.fieldnotexists'), [ActionProcedure]))));
              Exit;
            end;

          MetaData.BeginUpdate;
          DataCache.BeginUpdate;

          Parser := TDeParser.Create;
          Parser.Table := DataCache.TableMeta;
          ExpressionItem := TExpressionItem.Create;

          try
            Parser.Parse(ActionParams, ExpressionItem);
          except
            on E: EDeParserError do
              begin
                if E.Error <> Error_EmptyScript then
                  begin
                    if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                      ProfitActions.FActionResults.Add('error:' + E.Message)
                    else
                      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
                                  NativeInt(PChar(Caption + #10#13 + E.Message + #10#13'_dL.procedureerror')));
                    Exit;
                  end;
              end;
          end;
          SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, DataCache.SelectedCount, 1);
          for CacheIndex := 0 to Pred(DataCache.SelectedCount) do
            try
              SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, CacheIndex, 0);
              if VarAsType(DataCache.SelectedItems[CacheIndex].Calculate(EnabledPostfix, True), varBoolean) then
                begin
                  Value := DataCache.SelectedItems[CacheIndex].Calculate(ExpressionItem, null);
                  ValueType := FieldTypeVarMap[DataCache.TableMeta.Fields[FieldIndex].DataType];
                  try
                    DataCache.SelectedItems[CacheIndex].FieldValue[FieldIndex] := VarAsType(Value, ValueType);
                    DataCache.UpdateRecord(DataCache.SelectedItems[CacheIndex]);
                  except
                    on E: Exception do
                      begin
                        if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                          ProfitActions.FActionResults.Add('error:_dL.procedureerror')
                        else
                          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(Caption + #10#13'_dL.procedureerror')));
                        {$IFDEF DEBUG}
                        DebugLog('TDeActionData.ExecuteSetValue error: ' + E.Message);
                        {$ENDIF}
                      end;
                  end;
                end;
            except
              {$IFDEF DEBUG}
              on E: Exception do
                DebugLog('TDeActionData.ExecuteSetValue error: ' + E.Message);
              {$ENDIF}
            end;
          SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
          ExpressionItem.Free;
          Parser.Free;
          DataCache.EndUpdate;
          MetaData.EndUpdate;
        end
      else
        Application.MessageBox(PChar(GetTitle('_eRror.notcacheforaction')),
                               PChar(GetTitle('_dE.error')), MB_OK or MB_ICONERROR or MB_APPLMODAL);
    end
  else
    Application.MessageBox(PChar(GetTitle('_eRror.notdatasetforaction')),
                           PChar(GetTitle('_dE.error')), MB_OK or MB_ICONERROR or MB_APPLMODAL);
end;

procedure TDeActionData.ExecuteShellExecute(Variables: TDeVariableList);
var
  ListManager: TDeVariableListManager;
  ItemIndex: Integer;
  Calculator: TDeCalculator;

procedure DeShellExecute(const AWnd: HWND; const AOperation, AFileName: String; const AParameters: String = ''; const ADirectory: String = ''; const AShowCmd: Integer = SW_SHOWNORMAL);
var
  ExecInfo: TShellExecuteInfo;
  NeedUnitialize: Boolean;
begin
  Assert(AFileName <> EmptyStr);

  NeedUnitialize := Succeeded(CoInitializeEx(nil, COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE));
  try
    FillChar(ExecInfo, SizeOf(ExecInfo), 0);
    ExecInfo.cbSize := SizeOf(ExecInfo);

    ExecInfo.Wnd := AWnd;
    ExecInfo.lpVerb := Pointer(AOperation);
    ExecInfo.lpFile := PChar(AFileName);
    ExecInfo.lpParameters := Pointer(AParameters);
    ExecInfo.lpDirectory := Pointer(ADirectory);
    ExecInfo.nShow := AShowCmd;
    ExecInfo.fMask := SEE_MASK_NOASYNC { = SEE_MASK_FLAG_DDEWAIT для старых версий Delphi }
                   or SEE_MASK_FLAG_NO_UI;
    {$IFDEF UNICODE}
    // Необязательно, см. http://www.transl-gunsmoker.ru/2015/01/what-does-SEEMASKUNICODE-flag-in-ShellExecuteEx-actually-do.html
    ExecInfo.fMask := ExecInfo.fMask or SEE_MASK_UNICODE;
    {$ENDIF}

    {$WARN SYMBOL_PLATFORM OFF}
    Win32Check(ShellExecuteEx(@ExecInfo));
    {$WARN SYMBOL_PLATFORM ON}
  finally
    if NeedUnitialize then
      CoUninitialize;
  end;
end;


  function ExecuteFile(const FileName, Parameters: string; const Waited: Boolean; const wShowWindow: Word): Integer;
  var
    StartupInfo: TStartupInfo;
    ProcessInformation: TProcessInformation;
    ErrorCode: Cardinal;
  begin
    ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
    StartupInfo.cb := SizeOf(StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW {$IFDEF CONSOLE} or STARTF_USESTDHANDLES{$ENDIF};
    StartupInfo.wShowWindow := wShowWindow;
    if Length(Trim(Parameters)) = 0 then
      if CreateProcess(PChar(FileName), nil, nil, nil, False, NORMAL_PRIORITY_CLASS, nil, PChar(ExtractFilePath(FileName)), StartupInfo, ProcessInformation) then
        Result := 0
      else
        Result := -1
    else
      if CreateProcess(nil, PChar('"' + FileName + '" ' + Parameters), nil, nil, False, NORMAL_PRIORITY_CLASS, nil, PChar(ExtractFilePath(FileName)), StartupInfo, ProcessInformation) then
        Result := 0
      else
        Result := -1;
    if Result = 0 then
      try
        if Waited then
          begin
            repeat
              ErrorCode := WaitForSingleObject(ProcessInformation.hProcess, 200);
              if ErrorCode = WAIT_FAILED then
                begin
                  Result := -2;
                  Break;
                end
              else
                begin
                  Application.ProcessMessages;
                  Sleep(300);
                end;
            until ErrorCode <> WAIT_TIMEOUT;
            if GetExitCodeProcess(ProcessInformation.hProcess, ErrorCode) then
              Result := ErrorCode
            else
              Result := -3;
          end;
      finally
        CloseHandle(ProcessInformation.hProcess);
        CloseHandle(ProcessInformation.hThread);
      end;
  end;

  function ShellFindExecutable(const FileName, DefaultDir: string): string;
   var
     Res: HINST;
     Buffer: array[0..MAX_PATH] of Char;
     P: PChar;
   begin
     FillChar(Buffer, SizeOf(Buffer), #0);
     if DefaultDir = EmptyStr then P := nil
      else
        P := PChar(DefaultDir);
     Res := FindExecutable(PChar(FileName), P, Buffer);
     if Res > 32 then
     begin
       P := Buffer;
       while PWord(P)^ <> 0 do
       begin
         if P^ = #0 then // FindExecutable replaces #32 with #0
          P^ := ' ';
         Inc(P);
       end;
       Result := Buffer;
     end
      else
       Result := EmptyStr;
   end;

  procedure Exec(Variables: TDeVariableList);
  const
    cmd_edit    = 'edit';
    cmd_explore = 'explore';
    cmd_find    = 'find';
    cmd_open    = 'open';
    cmd_print   = 'print';

    cmd_mailto  = 'mailto:';
    cmd_execute = 'execute';
    cmd_browse  = 'browse';
    cmd_console = 'console';
  var
    Parser: TDeParser;
    ExpressionItem: TExpressionItem;
    Calculator: TDeCalculator;
    ErrorCode: Integer;
    A,B,C : string;
    function ReadShowWindow(const wDefaultShowWindow: Word): Word;
    var
      Variable: TVariableItem;
      Value: string;
    begin
      Result := wDefaultShowWindow;
      if Assigned(Variables) then
        try
          Variable := Variables.GetByName(varActionShowWindow, False);
          if Assigned(Variable) then
            if VarIsType(Variable.Value, [varByte, varWord, varLongWord, varShortInt, varSmallint, varInteger, varInt64]) then
              case VarAsType(Variable.Value, varByte) of
                0: Result := SW_HIDE;
                1: Result := SW_SHOWNORMAL;
                2: Result := SW_SHOWMINIMIZED;
                3: Result := SW_SHOWMAXIMIZED
              end
            else
              if VarIsStr(Variable.Value) then
                begin
                  Value := Variable.Value;
                  if SameText(Value, 'hide') then
                    Result := SW_HIDE
                  else if SameText(Value, 'show') then
                    Result := SW_SHOWNORMAL
                  else if SameText(Value, 'minimized') then
                    Result := SW_SHOWMINIMIZED
                  else if SameText(Value, 'maximized') then
                    Result := SW_SHOWMAXIMIZED;
                end;
        except
          on E: Exception do
            begin
              {$IFDEF DEBUG}
              DebugLog('TDeActionData.ExecuteShellExecute.ReadShowWindow skip error: ' + E.Message);
              {$ENDIF}
              Result := wDefaultShowWindow;
            end;
        end;
    end;
  begin
    A:= ActionFile;
    B:= ActionProcedure;
    C:= ActionParams;

    // вычисляем 2й и 3й параметры
    Parser := TDeParser.Create;
    if not (DataSet = unassigned) then
      Parser.Table:=Metadata.TablesList.FindByID(DataSet);
    Parser.onGetIdentType:= GetIdentType; // А может в парсере всё же нужны переменные?!?! Добавил ...

    Calculator := TDeCalculator.Create;
    Calculator.OnGetIdentValue := GetIdentValue;

    ExpressionItem := TExpressionItem.Create;
    try
      Parser.Parse(B, ExpressionItem);
      if 0 < ExpressionItem.Count then
        B:= Calculator.Calculate(ExpressionItem);
    except
      {$IFDEF DEBUG}
      on E: Exception do
        DebugLog('TDeActionData.ExecuteShellExecute skip parse and calculate "B" error: ' + E.Message);
      {$ENDIF}
    end;
    ExpressionItem.Free;

    ExpressionItem := TExpressionItem.Create;
    try
      Parser.Parse(C, ExpressionItem);
      if 0 < ExpressionItem.Count then
        C:= Calculator.Calculate(ExpressionItem);
    except
      {$IFDEF DEBUG}
      on E: Exception do
        DebugLog('TDeActionData.ExecuteShellExecute skip parse and calculate "C" error: ' + E.Message);
      {$ENDIF}
    end;
    ExpressionItem.Free;

    Calculator.Free;
    Parser.Free;

    // определяем тип команды по умолчанию и исполняемый файл
    if (length(A)=0) and (length(B)=0) and (length(C)>0) then
       begin
         if length(B) = 0 then
           B:= ShellFindExecutable(Trim(C),EmptyStr);

         if 0 < length(B) then
           begin
             ErrorCode := ShellExecute(Application.Handle, nil, PChar(Trim(B)), PChar(Trim(C)), nil, ReadShowWindow(SW_SHOWNORMAL));
             {$IFDEF DeDEBUG}
             Funcs.WriteLog('%s: %s / %s', [A, B, C]);
             {$ENDIF}
           end
         else
           begin
             ErrorCode := ShellExecute(Application.Handle, nil, PChar(Trim(C)), nil, nil, ReadShowWindow(SW_SHOWNORMAL));
             {$IFDEF DeDEBUG}
             Funcs.WriteLog('%s: %s / %s', [A, C, EmptyStr]);
             {$ENDIF}
           end;
       end else

    if (SameText(A, cmd_explore)) or ( (length(A)=0) and (SysUtils.DirectoryExists(B)) ) then
       begin
         ErrorCode := ShellExecute(Application.Handle, cmd_explore, PChar(B), PChar(C), nil, ReadShowWindow(SW_SHOWNORMAL));
         {$IFDEF DeDEBUG}
         Funcs.WriteLog('%s: %s / %s', [cmd_explore, B, C]);
         {$ENDIF}
       end else

    if (SameText(A, cmd_open)) or ( (length(A)=0) and (SysUtils.FileExists(B)) ) then
       begin
         ErrorCode := ShellExecute(Application.Handle, nil, PChar(Trim(B+' '+C)), nil, nil, ReadShowWindow(SW_SHOWNORMAL));
         {$IFDEF DeDEBUG}
         Funcs.WriteLog('%s: %s / %s', [EmptyStr, B, C]);
         {$ENDIF}
       end else

    if (SameText(A, cmd_mailto)) or ( (length(A)=0) and ( pos('@',B)>0 ) ) then
       begin
         ErrorCode := ShellExecute(Application.Handle, nil, PChar(cmd_mailto+B), PChar(C), nil, ReadShowWindow(SW_SHOWNORMAL));
         {$IFDEF DeDEBUG}
         Funcs.WriteLog('%s: %s / %s', [EmptyStr, cmd_mailto+B, C]);
         {$ENDIF}
       end else

    if (SameText(A, cmd_browse)) or ( SameText(copy(A,1,5), 'http:') or SameText(copy(A,1,6), 'https:') or SameText(copy(A,1,4), 'ftp:')) then
       ErrorCode := ShellExecute(Application.Handle, nil, PChar(cmd_mailto+B), PChar(C), nil, ReadShowWindow(SW_SHOWNORMAL)) else // ????? При чём тут mailto: ?????

    if (SameText(A, cmd_print)) then
       begin
         ErrorCode := ShellExecute(Application.Handle, cmd_print, PChar(B), PChar(C), nil, ReadShowWindow(SW_MINIMIZE));
         {$IFDEF DeDEBUG}
         Funcs.WriteLog('%s: %s / %s', [cmd_print, B, C]);
         {$ENDIF}
       end else

    if (SameText(A, cmd_find)) then
       begin
         ErrorCode := ShellExecute(Application.Handle, cmd_find, PChar(B), PChar(C), nil, ReadShowWindow(SW_SHOWNORMAL));
         {$IFDEF DeDEBUG}
         Funcs.WriteLog('%s: %s / %s', [cmd_find, B, C]);
         {$ENDIF}
       end else

    if (SameText(A, cmd_edit)) then
       begin
         ErrorCode := ShellExecute(Application.Handle, cmd_edit, PChar(Trim(B+' '+C)), nil, nil, ReadShowWindow(SW_SHOWNORMAL));
         {$IFDEF DeDEBUG}
         Funcs.WriteLog('%s: %s / %s', [cmd_edit, Trim(B+' '+C), EmptyStr]);
         {$ENDIF}
       end else

    if (SameText(A, cmd_browse)) then
       begin
         ErrorCode := ShellExecute(Application.Handle, nil, PChar(Trim(B+' '+C)), nil, nil, ReadShowWindow(SW_SHOWNORMAL));
         {$IFDEF DeDEBUG}
         Funcs.WriteLog('%s: %s / %s', [EmptyStr, Trim(B+' '+C), EmptyStr]);
         {$ENDIF}
       end else

    if (SameText(A, cmd_execute)) then
       begin
         if length(B) = 0 then
           B:= ShellFindExecutable(Trim(C),EmptyStr);


         if 0 < length(B) then
           begin
             ErrorCode := ShellExecute(Application.Handle, nil, PChar(Trim(B)), PChar(Trim(C)), nil, ReadShowWindow(SW_SHOWNORMAL));
             {$IFDEF DeDEBUG}
             Funcs.WriteLog('%s: %s / %s', [A, B, C]);
             {$ENDIF}
           end
         else
           begin
             ErrorCode := ShellExecute(Application.Handle, nil, PChar(Trim(C)), nil, nil, ReadShowWindow(SW_SHOWNORMAL));
             {$IFDEF DeDEBUG}
             Funcs.WriteLog('%s: %s / %s', [A, C, '']);
             {$ENDIF}
           end;
       end else

    if (SameText(A, cmd_console)) then
       begin
         ErrorCode := ExecuteFile(B, C, True, ReadShowWindow({$IFDEF DEBUG}SW_SHOWNORMAL{$ELSE}SW_HIDE{$ENDIF}));
         {$IFDEF DeDEBUG}
         Funcs.WriteLog('%s: %s / %s', [cmd_console, B, C]);
         {$ENDIF}
         if ErrorCode < 0 then
           ErrorCode := 0 // Ошибка системы!!!
         else
           with Variables.GetByName(varActionResult) do
             begin
               Value:= ErrorCode;
               IsOutput:= True;
               ErrorCode:= 255; // Всё ОК: получили результат выполнения консольного приложения
             end;
       end else
       begin
         {$IFDEF DeDEBUG}
         Funcs.WriteLog('%s: %s / %s', [A, B, C]);
         {$ENDIF}
         ErrorCode := ShellExecute(Application.Handle, PChar(A), PChar(Trim(B+' '+C)), nil, nil, ReadShowWindow(SW_SHOWNORMAL));
       end;

    if ErrorCode in [0..32] then
      if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
        ProfitActions.FActionResults.Add('error:' + SysErrorMessage(GetLastError) + #10 + A + #10 + B + #10 + C)
      else
        Application.MessageBox(PChar(SysErrorMessage(GetLastError) + #10 + A + #10 + B + #10 + C),
                               PChar(GetTitle('_dE.error')), MB_OK or MB_ICONERROR or MB_APPLMODAL);
  end;
  function Calculate(aPostfix: TExpressionItem; const aDefaultResult: Variant): Variant;
  begin
    if Assigned(aPostfix) and (aPostfix.Count > 0) then
      Result := Calculator.Calculate(aPostfix)
    else
      Result := aDefaultResult;
  end;
begin
  // Разворачиваем массивы при необходимости в список для цикла ...
  ListManager := Variables.ExtractArrayList;
  try
    {$IFDEF DEBUG}
    ListManager.DebugVariablesListLog('Extract array list completed ...');
    {$ENDIF}
    Calculator := TDeCalculator.Create;
    try
      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, ListManager.Count, 1);
      try
        for ItemIndex := 0 to Pred(ListManager.Count) do
          begin
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, ItemIndex, 0);
            FExecuteVaribales := TDeVariableList.Create;
            try
              FExecuteVaribales.CopyFrom(ListManager[ItemIndex]);
              Calculator.OnGetIdentValue := ListManager[ItemIndex].GetIdentValue;
              if VarAsType(Calculate(EnabledPostfix, True), varBoolean) then
                begin
                  Exec(ListManager[ItemIndex]);
                  ListManager.CloneOutputParameters(ItemIndex, Succ(ItemIndex), Pred(ListManager.Count));
                  Variables.ReadOutputFrom(ListManager[ItemIndex], ItemIndex, ListManager.Count);
                end;
            finally
              FreeAndNil(FExecuteVaribales);
            end;
          end;
      finally
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
      end;
    finally
      Calculator.Free;
    end;
  finally
    ListManager.Free;
  end;
end;

procedure TDeActionData.ExecuteStoredProcedure(Variables: TDeVariableList);
var
  DataCache: TDataCache;
  Database: TDeCustomDatabase;
  TableMeta: TTableMeta;
  i,{CacheIndex, }ItemIndex: Integer;
  ListManager: TDeVariableListManager;
  Calculator: TDeCalculator;
  procedure Exec(Variables: TDeVariableList);
  var
    ErrorCode: Integer;
    Index: Integer;
    {$IFDEF DEBUG}
    DebugCount: Integer;
    {$ENDIF}
    ErrorText, LineText: string;
  begin
    ParseParams(ActionParams);
    try
      if FActionSource = asCommands then
        begin
          FExecuteVaribales := TDeVariableList.Create;
          try
            // Копируем старый вариант передачи параметров ...
            for Index := 0 to Pred(Fields.Count) do
              FExecuteVaribales.SetByName(TFieldMeta(Fields[Index]).Original, TFieldMeta(Fields[Index]).DefaultValue);
            // Дополняем параметрами от пользователя при необходимости ...
            if Assigned(Variables) then
              for Index := 0 to Pred(Variables.Count) do
                FExecuteVaribales.SetByName(Variables[Index].Name, Variables[Index].Value);
            {$IFDEF DEBUG}
            FExecuteVaribales.DebugVariablesLog(Format('Before stored procedure %s executing ...', [QuotedStr(ActionProcedure)]));
            {$ENDIF}
//TODO:  GetVariableValue зачем здесь обработчик только для Парсера ???
//          ErrorCode := Database.ExecuteStoredProcedure(ActionProcedure, FExecuteVaribales, {CacheItem} nil , GetVariableValue);
            ErrorCode := Database.ExecuteStoredProcedure(ActionProcedure, FExecuteVaribales);
          finally
            // Если параметры были переданы, то ...
            if Assigned(Variables) then
              begin
                // Все OUT параметры вернём обратно!!!
                {$IFDEF DEBUG}
                DebugCount := 0;
                {$ENDIF}
                for Index := 0 to Pred(FExecuteVaribales.Count) do
                  if FExecuteVaribales[Index].IsOutput then
                    begin
                      Variables.SetByName(FExecuteVaribales[Index].Name, FExecuteVaribales[Index].Value);
                      Variables.GetByName(FExecuteVaribales[Index].Name).IsOutput:= True;
                      {$IFDEF DEBUG}
                      Inc(DebugCount);
                      {$ENDIF}
                    end;
                {$IFDEF DEBUG}
                if DebugCount <> 0 then
                  Variables.DebugVariablesLog(Format('After stored procedure %s outputing ...', [QuotedStr(ActionProcedure)]));
                {$ENDIF}
              end;
            FreeAndNil(FExecuteVaribales);
          end;
        end;

      if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
        begin
          if ErrorCode > 0 then
            ProfitActions.FActionResults.Add(Caption + #10#13'_dL.procedureerror');
        end
      else
        if ErrorCode = 0 then
          SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo,
                      NativeInt(PChar(Caption + #10#13'_dL.procedureok')))
        else
          if ErrorCode > 0 then
            SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
                        NativeInt(PChar(Caption + #10#13'_dL.procedureerror')));
    except
      on E: Exception do
        begin
          ErrorText := Trim(E.Message);
          if Length(ErrorText) = 0 then
            if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
              ProfitActions.FActionResults.Add('error:_dL.procedureerror')
            else
              SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(Caption + #10#13'_dL.procedureerror')))
          else
            while Length(ErrorText) <> 0 do
              begin
                Index := Pos('||', ErrorText);
                if Index = 0 then
                  begin
                    LineText := ErrorText;
                    ErrorText := EmptyStr;
                  end
                else
                  begin
                    LineText := Trim(Copy(ErrorText, 1, Pred(Index)));
                    ErrorText := Trim(Copy(ErrorText, Index + 2, Length(ErrorText)));
                  end;
                if Length(LineText) <> 0 then
                  if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                    begin
                      if SameText(Copy(LineText, 1, 5), 'info:') or SameText(Copy(LineText, 1, 7), 'result:') or SameText(Copy(LineText, 1, 6), 'error:') then
                        ProfitActions.FActionResults.Add(LineText)
                      else
                        ProfitActions.FActionResults.Add('error:' + LineText);
                    end
                  else
                    if SameText(Copy(LineText, 1, 5), 'info:') then
                      SendMessage(Application.MainForm.Handle, DM_INFONOTIFY,  icoInfo,  NativeInt(PChar(Caption + #10#13 + Copy(LineText, 6, MaxInt))))
                    else if SameText(Copy(LineText, 1, 7), 'result:') then
                      SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoResult, NativeInt(PChar(Caption + #10#13 + Copy(LineText, 8, MaxInt))))
                    else if SameText(Copy(LineText, 1, 6), 'error:') then
                      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(Caption + #10#13 + Copy(LineText, 7, MaxInt))))
                    else
                      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(Caption + #10#13 + LineText)));
              end;
        end;
    end;
  end;
  function Calculate(aPostfix: TExpressionItem; const aDefaultResult: Variant): Variant;
  begin
    if Assigned(aPostfix) and (aPostfix.Count > 0) then
      Result := Calculator.Calculate(aPostfix)
    else
      Result := aDefaultResult;
  end;
begin
  if DataSet = 0 then
    DataCache := nil
  else
    DataCache := MetaData.FindActiveDataCache(DataSet, True);

  Database := MetaData.DatabaseByAlias(ActionFile);
  if not Assigned(Database) then
    begin
      if Assigned(DataCache) then
        if Assigned(DataCache.TableMeta) then
          TableMeta := DataCache.TableMeta
        else
          TableMeta := MetaData.GetTableMeta(DataSet)
      else
        TableMeta := nil;
      if Assigned(TableMeta) then
        Database := TableMeta.Database;
    end;

  for i:= Pred(MetaData.DatabasesCount) downto 0 do
    if not Assigned(Database) and  MetaData.Databases[i].CanStoredProcedure then
      Database := MetaData.Databases[i];

  if Assigned(Database) then
    if Database.CanStoredProcedure then
      begin
        MetaData.BeginUpdate;
        if Assigned(DataCache) then DataCache.BeginUpdate;
        try
          // Разворачиваем массивы при необходимости в список для цикла ...
          ListManager := Variables.ExtractArrayList;
          try
            {$IFDEF DEBUG}
            ListManager.DebugVariablesListLog('Extract array list completed ...');
            {$ENDIF}
            Calculator := TDeCalculator.Create;
            try
              SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, ListManager.Count, 1);
              try
                for ItemIndex := 0 to Pred(ListManager.Count) do
                  begin
                    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, ItemIndex, 0);
                    Calculator.OnGetIdentValue := ListManager[ItemIndex].GetIdentValue;
                    if VarAsType(Calculate(EnabledPostfix, True), varBoolean) then
                      begin
                        Exec(ListManager[ItemIndex]);
                        ListManager.CloneOutputParameters(ItemIndex, Succ(ItemIndex), Pred(ListManager.Count));
                        Variables.ReadOutputFrom(ListManager[ItemIndex], ItemIndex, ListManager.Count);
                      end;
                  end;
              finally
                SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
              end;
            finally
              Calculator.Free;
            end;
          finally
            ListManager.Free;
          end;
        finally
          if Assigned(DataCache) then DataCache.EndUpdate;
          MetaData.EndUpdate;
        end;
      end
    else
      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(Format(GetTitle('_eRror.DatabaseNotSupportExecStoredProc'), [ActionFile]))))
  else
    SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(Format(GetTitle('_eRror.DatabaseNotFoundForExecStoredProc'), [ActionFile]))));
end;

procedure TDeActionData.ExecuteReport(Variables: TDeVariableList; const QuestionEnabled: Boolean; const Mode: TActionMode);
var
  ItemIndex: Integer;
  VariableList: TDeVariableList;
  ListManager: TDeVariableListManager;
  Calculator: TDeCalculator;
  procedure DoPrint(Variables: TDeVariableList);
  var
    Report: TDeReport;
    Copies, Index: Integer;
    PrinterName, FileNameOnly, FileExtension, FileName, InfoText: string;
    Parameters: TDeVariableList;
  begin
    Copies := 1;
    Report := PrepareReport(Variables);
    while Assigned(Report) do
      begin
        Parameters := TDeVariableList.Create;
        try
          Parameters.LoadCommaText2(ActionParams);
          // Сначала положим в переменные отчёта введённые пользователем значения ...
          Report.Params.CopyFrom(Variables);
          // Дополним и заменим переменными из параметров действия ....
          Report.Params.CopyFrom(Parameters, False);
          // Параметризуем имена файлов:
          // 1. Исходный файл шаблона
          Report.Params.SetByName(varSourceFileName, Report.FileName);
          // 2. Результирующий файл
          if Report.Params.IndexByName(varTargetFileName) = -1 then
            Report.Params.SetByName(varTargetFileName, Report.TargetFileName);
        finally
          Parameters.Free;
        end;
        PrinterName := Variables.GetValueByName(varPrinterName);
        if Length(PrinterName) = 0 then
          Report.Preview
        else if Pos(':\', PrinterName) = 2 then
          begin
            // Указан каталог: Режим экспорта в файл ...
            FileNameOnly := NormalizeFileName(Caption + PrepareReportVariablesFileSuffix(Variables), True);
            FileExtension := ExtractFileExt(Report.FileName);
            Index := 1;
            FileName := PrinterName + FileNameOnly + FileExtension;
            SysUtils.ForceDirectories(PrinterName);
            while FileExists(FileName) do
              begin
                Inc(Index);
                FileName := PrinterName + FileNameOnly + ' ('+ Trim(Format('%3.3d', [Index])) + ')' + FileExtension;
              end;
            try
              Report.Print(FileName, Copies);
              if FileExists(FileName) then
                InfoText := Format(GetTitle('_iNfo.savefile.ok', ttFull), [QuotedStr(ExtractFileName(FileName))])
              else
                InfoText := EmptyStr;
              if Length(InfoText) <> 0 then
                if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                  ProfitActions.FActionResults.Add('info:' + InfoText)
                else
                  SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoResult, NativeInt(PChar(Caption + #10#13 + InfoText)));
            except
              on E: Exception do
                begin
                  if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                    ProfitActions.FActionResults.Add('error:' + E.Message)
                  else
                    SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(Caption + #10#13 + E.Message)));
                end;
            end;
          end
        else
          begin
            try
              Copies := Report.Params.GetValueByName(varPrintCopies);
              if Copies < 1 then Copies := 1;
            except
              on E: Exception do
                begin
                  {$IFDEF DEBUG}
                  DebugLog('TDeActionData.ExecuteReport().DoPrint skip error for variables %s: %s', [varPrintCopies, E.Message]);
                  {$ENDIF}
                  Copies := 1;
                end;
            end;
            Report.Print(PrinterName, Copies);
          end;
        FreeAndNil(Report);
      end;
  end;
  function Calculate(aPostfix: TExpressionItem; const aDefaultResult: Variant): Variant;
  begin
    if Assigned(aPostfix) and (aPostfix.Count > 0) then
      Result := Calculator.Calculate(aPostfix)
    else
      Result := aDefaultResult;
  end;
begin
  {$IFDEF DeDEBUG}
  Funcs.WriteLog('Execute report %d start ...', [ID], False, 'Reports');
  {$ENDIF}
  if FActionSource = asCommands then
    begin
      VariableList := TDeVariableList.Create;
      try
        if Assigned(Variables) then
          VariableList.CopyFrom(Variables)
        else
          if TDeCommandDialog.Execute(Self, VariableList, QuestionEnabled, Mode) <> mrOK then
            Exit;
        // Разворачиваем массивы при необходимости в список для цикла ...
        ListManager := VariableList.ExtractArrayList;
        try
          {$IFDEF DEBUG}
          ListManager.DebugVariablesListLog('Extract array list completed ...');
          {$ENDIF}
          Calculator := TDeCalculator.Create;
          try
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, ListManager.Count, 1);
            try
              for ItemIndex := 0 to Pred(ListManager.Count) do
                begin
                  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, ItemIndex, 0);
                  Calculator.OnGetIdentValue := ListManager[ItemIndex].GetIdentValue;
                  if VarAsType(Calculate(EnabledPostfix, True), varBoolean) then
                    begin
                      DoPrint(ListManager[ItemIndex]);
                      ListManager.CloneOutputParameters(ItemIndex, Succ(ItemIndex), Pred(ListManager.Count));
                      Variables.ReadOutputFrom(ListManager[ItemIndex], ItemIndex, ListManager.Count);
                    end;
                end;
            finally
              SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
            end;
          finally
            Calculator.Free;
          end;
        finally
          ListManager.Free;
        end;
      finally
        VariableList.Free;
      end;
    end;
  {$IFDEF DeDEBUG}
  Funcs.WriteLog('Execute report %d finish ...', [ID], False, 'Reports');
  {$ENDIF}
end;

procedure TDeActionData.ExecuteGroup(Variables: TDeVariableList; const QuestionEnabled: Boolean; const Mode: TActionMode);
var
  VariableList: TDeVariableList;
  ListManager: TDeVariableListManager;
  Calculator: TDeCalculator;
  ItemIndex, Index: Integer;
  function Calculate(aPostfix: TExpressionItem; const aDefaultResult: Variant): Variant;
  begin
    if Assigned(aPostfix) and (aPostfix.Count > 0) then
      Result := Calculator.Calculate(aPostfix)
    else
      Result := aDefaultResult;
  end;
begin
  if FActionSource = asCommands then
    begin
      VariableList := TDeVariableList.Create;
      try
        if Assigned(Variables) then
          VariableList.CopyFrom(Variables)
        else
          if TDeCommandDialog.Execute(Self, VariableList, QuestionEnabled, Mode) <> mrOK then
            Exit;
        // Разворачиваем массивы при необходимости в список для цикла ...
        ListManager := VariableList.ExtractArrayList;
        try
          {$IFDEF DEBUG}
          ListManager.DebugVariablesListLog('Extract array list completed ...');
          {$ENDIF}
          Calculator := TDeCalculator.Create;
          try
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, ListManager.Count, 1);
            try
              for ItemIndex := 0 to Pred(ListManager.Count) do
                begin
                  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, ItemIndex, 0);
                  Calculator.OnGetIdentValue := ListManager[ItemIndex].GetIdentValue;
                  if VarAsType(Calculate(EnabledPostfix, True), varBoolean) then
                    begin
                      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, ActionCount, 1);
                      try
                        for Index := 0 to Pred(ActionCount) do
                          begin
                            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Index, 0);
                            if Assigned(Actions[Index]) then
                              Actions[Index].Execute(ListManager[ItemIndex], QuestionEnabled, Mode)
                            else
                              if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                                ProfitActions.FActionResults.Add('error:' + Format(GetTitle('_eRror.ActionNotFound'), [ActionNames[Index]]))
                              else
                                SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
                                  NativeInt(PChar(Format(GetTitle('_eRror.ActionNotFound'), [ActionNames[Index]]))));
                          end;
                      finally
                        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
                      end;
                    end;
                end;
            finally
              SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
            end;
          finally
            Calculator.Free;
          end;
        finally
          ListManager.Free;
        end;
      finally
        VariableList.Free;
      end;
    end
  else
    Application.MessageBox(PChar(GetTitle('_eRror.notimplementedforoldmetadata')),
                           PChar(GetTitle('_dE.error')), MB_OK or MB_ICONERROR or MB_APPLMODAL);
end;

procedure TDeActionData.ExecuteUpdateValue(Variables: TDeVariableList; const QuestionEnabled: Boolean; const Mode: TActionMode);
var
  VariableList: TDeVariableList;
  DataCache: TDataCache;
  Index, FieldIndex, CacheIndex: Integer;
  Value: Variant;
  ValueType: Word;
begin
  if FActionSource = asCommands then
    if DataSet <> 0 then
      begin
        DataCache := MetaData.FindActiveDataCache(DataSet, True);
        if Assigned(DataCache) then
          begin
            if DataCache.SelectedCount = 0 then
              begin
                if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                  ProfitActions.FActionResults.Add('info:' + GetTitle('_eRror.noselectedobject'))
                else
                  SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(GetTitle('_eRror.noselectedobject'))));
                Exit;
              end;
            VariableList := TDeVariableList.Create;
            try
              if Assigned(Variables) then
                VariableList.CopyFrom(Variables)
              else
                if TDeCommandDialog.Execute(Self, VariableList, QuestionEnabled, Mode) <> mrOK then
                  Exit;

              FieldIndex := -1;
              for Index := Pred(VariableList.Count) downto 0 do //в обратном порядке быстрее найдем несистемный параметр
                begin
                  FieldIndex := DataCache.TableMeta.Fields.IndexByName(VariableList[Index].Name);
                  if -1 < FieldIndex then
                    Break;
                end;

              if FieldIndex = -1 then
                begin
                  if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                    ProfitActions.FActionResults.Add('error:_dL.procedureerror')
                  else
                    SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(Caption + #10#13'_dL.procedureerror')));
                  Exit;
                end;

              MetaData.BeginUpdate;
              DataCache.BeginUpdate;
              try
                SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, DataCache.SelectedCount, 1);
                try
                  for CacheIndex := 0 to Pred(DataCache.SelectedCount) do
                    try
                      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, CacheIndex, 0);
                      if VarAsType(DataCache.SelectedItems[CacheIndex].Calculate(EnabledPostfix, True), varBoolean) then
                        begin
                          for Index := 0 to Pred(VariableList.Count) do
                            begin
                              FieldIndex := DataCache.TableMeta.Fields.IndexByName(VariableList[Index].Name);
                              if FieldIndex <> -1 then
                                try
                                  Value := VariableList[Index].Value;
                                  ValueType := FieldTypeVarMap[DataCache.TableMeta.Fields[FieldIndex].DataType];
                                  DataCache.SelectedItems[CacheIndex].FieldValue[FieldIndex] := VarAsType(Value, ValueType);
                                except
                                  on E: Exception do
                                    begin
                                      if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                                        ProfitActions.FActionResults.Add('error:_dL.procedureerror')
                                      else
                                        SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(Caption + #10#13'_dL.procedureerror')));
                                      {$IFDEF DEBUG}
                                      DebugLog('TDeActionData.ExecuteUpdateValue error: ' + E.Message);
                                      {$ENDIF}
                                    end;
                                end;
                            end;
                          try
                            DataCache.UpdateRecord(DataCache.SelectedItems[CacheIndex]);
                          except
                            on E: Exception do
                              begin
                                if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                                  ProfitActions.FActionResults.Add('error:_dL.procedureerror')
                                else
                                  SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(Caption + #10#13'_dL.procedureerror')));
                                {$IFDEF DEBUG}
                                DebugLog('TDeActionData.ExecuteUpdateValue error: ' + E.Message);
                                {$ENDIF}
                              end;
                          end;
                        end;
                    except
                      {$IFDEF DEBUG}
                      on E: Exception do
                        DebugLog('TDeActionData.ExecuteUpdateValue error: ' + E.Message);
                      {$ENDIF}
                    end;
                finally
                  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
                end;
              finally
                DataCache.EndUpdate;
                MetaData.EndUpdate;
              end;
            finally
              VariableList.Free;
            end;
          end
        else
          Application.MessageBox(PChar(GetTitle('_eRror.notcacheforaction')),
                                 PChar(GetTitle('_dE.error')), MB_OK or MB_ICONERROR or MB_APPLMODAL);
      end
    else
      Application.MessageBox(PChar(GetTitle('_eRror.notdatasetforaction')),
                             PChar(GetTitle('_dE.error')), MB_OK or MB_ICONERROR or MB_APPLMODAL)
  else
    Application.MessageBox(PChar(GetTitle('_eRror.notimplementedforoldmetadata')),
                           PChar(GetTitle('_dE.error')), MB_OK or MB_ICONERROR or MB_APPLMODAL);
end;

procedure TDeActionData.ExecuteInsertValue(Variables: TDeVariableList; const QuestionEnabled: Boolean; const Mode: TActionMode);
var
  VariableList: TDeVariableList;
  DataCache: TDataCache;
  DataManager: TDataManager;
  RecordEditor: TRecordEditor;
  FieldName: String;
  FieldIndex, Index: Integer;
  Value: Variant;
  TableMeta: TTableMeta;
begin
  if FActionSource = asCommands then
    if DataSet <> 0 then
      begin
        TableMeta := MetaData.GetTableMeta(DataSet);
        if Assigned(TableMeta) then
          begin
            VariableList := TDeVariableList.Create;
            try
              if Assigned(Variables) then
                VariableList.CopyFrom(Variables)
              else
                if TDeCommandDialog.Execute(Self, VariableList, QuestionEnabled, Mode) <> mrOK then
                  Exit;

              DataManager := TDataManager.Create;
              try
                DataManager.Table := TableMeta;
                RecordEditor := TRecordEditor.Create(TableMeta, Null);
                try
                  RecordEditor.CacheItem.BeginInit;
                  try
                    DataManager.PrepareRecord(RecordEditor.CacheItem);
                  finally
                    RecordEditor.CacheItem.EndInit;
                  end;

                  if DataManager.CanInsertRecord(RecordEditor.CacheItem) then
                    try
                      DataManager.SetPrimaryKey(RecordEditor.CacheItem);

                      for Index := 0 to Pred(VariableList.Count) do
                        begin
                          FieldName := VariableList[Index].Name;
                          // Если это не системные переменные, то ...
                          if not SameText(Copy(FieldName, 1, 2), '__') then
                            begin
                              FieldIndex := TableMeta.Fields.IndexByName(FieldName);
                              if FieldIndex <> -1 then
                                begin
                                  Value := VariableList[Index].Value;
                                  try
                                    RecordEditor.CacheItem.FieldValue[FieldIndex]:=
                                                 VarToType(Value, TableMeta.Fields[FieldIndex].DataType);
                                  except
                                    on E: Exception do
                                      begin
                                        if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                                          ProfitActions.FActionResults.Add('error:_dL.procedureerror')
                                        else
                                          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(Caption + #10#13'_dL.procedureerror')));
                                        {$IFDEF DEBUG}
                                        DebugLog('TDeActionData.ExecuteInsertValue error: ' + E.Message);
                                        {$ENDIF}
                                      end;
                                  end;
                                end;
                            end;
                        end;

                      DataManager.InsertRecord(RecordEditor.CacheItem);

                      DataCache := MetaData.FindActiveDataCache(DataSet, True);
                      if Assigned(DataCache) then
                        DataCache.Update(mcInsert, RecordEditor.CacheItem.ID);
                      MetaData.UpdateLibrary(TableMeta.ID);
                    except
                      on E: Exception do
                        begin
                          if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                            ProfitActions.FActionResults.Add('error:_dL.procedureerror')
                          else
                            SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(Caption + #10#13'_dL.procedureerror')));
                          {$IFDEF DEBUG}
                          DebugLog('TDeActionData.ExecuteInsertValue error: ' + E.Message);
                          {$ENDIF}
                        end;
                    end
                  else
                    if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                      begin
                        if DataManager.Errors.Count>0 then
                          ProfitActions.FActionResults.Add('error:'+DataManager.Errors.GetMessage)
                        else
                          ProfitActions.FActionResults.Add('error:_dL.procedureerror')
                      end
                    else
                      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(Caption + #10#13'_dL.procedureerror')));
                finally
                  RecordEditor.Free;
                end;
              finally
                DataManager.Free;
              end;
            finally
              VariableList.Free;
            end;
          end
        else
          Application.MessageBox(PChar(GetTitle('_eRror.notcacheforaction')),
                                 PChar(GetTitle('_dE.error')), MB_OK or MB_ICONERROR or MB_APPLMODAL);
      end
    else
      Application.MessageBox(PChar(GetTitle('_eRror.notdatasetforaction')),
                             PChar(GetTitle('_dE.error')), MB_OK or MB_ICONERROR or MB_APPLMODAL)
  else
    Application.MessageBox(PChar(GetTitle('_eRror.notimplementedforoldmetadata')),
                           PChar(GetTitle('_dE.error')), MB_OK or MB_ICONERROR or MB_APPLMODAL);
end;

function TDeActionData.GetIdentType(const aIdent: string; var aType: TFieldType; aDataObject: pointer): Boolean;
var
  Variable: TVariableItem;
  FM: TFieldMeta;
begin
  Result := False;

  // ищем в списке переменных
  if assigned(FExecuteVaribales) then
    begin
      Variable := FExecuteVaribales.GetByName(aIdent, False);
      if Assigned(Variable) then
        begin
          aType:= ftUnknown;
          Exit(True);
        end;
    end;

  if assigned(DefaultMeta) then
    if DefaultMeta.Table.ID = DataSet then
      begin
        FM:=DefaultMeta.Table.GetFieldByName(aIdent, True);
        if assigned(FM) then
          begin
            aType:= FM.DataType;
            Exit(True);
          end;
      end;
end;

function TDeActionData.GetIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
var
  Variable: TVariableItem;
  ActiveMeta : TDataSetMeta;
begin
  Result := False;

  // ищем в списке переменных
  if assigned(FExecuteVaribales) then
    begin
      Variable := FExecuteVaribales.GetByName(aIdent, False);
      if Assigned(Variable) then
        begin
          aValue:= Variable.Value;
          Exit(True);
        end;
    end;

  // ищем в активном окне
  ActiveMeta:= MForm.ActiveMeta;
  if assigned(ActiveMeta) then
    if ActiveMeta.Table.ID = DataSet then
      if assigned(ActiveMeta.Cache.Fields.FindByName(aIdent, True)) then
        if assigned(ActiveMeta.Cache.FocusedItem) then
          begin
            aValue:= ActiveMeta.Cache.FocusedItem.ValueByName[aIdent];
            Exit(True);
          end;

  // ищем в главном окне - не факт, что тут надо искать...
  if assigned(DefaultMeta) then
    if DefaultMeta.Table.ID = DataSet then
      if assigned(DefaultMeta.Cache.Fields.FindByName(aIdent, True)) then
        if assigned(DefaultMeta.Cache.FocusedItem) then
          begin
            aValue:= DefaultMeta.Cache.FocusedItem.ValueByName[aIdent];
            Exit(True);
          end;

  {$IFDEF DEBUG}
  DebugLog('TDeActionData.GetIdentValue(%s, $%p) not found ...', [QuotedStr(aIdent), aDataObject]);
  {$ENDIF}
end;

function TDeActionData.UpdateSort(const BreakLine: Boolean; const Order: Integer): Boolean;
const
  BooleanValues: array[Boolean] of Byte = (0, 1);
var
  DataSet: TDeDataSet;
begin
  Result := False;
  if Assigned(MetaData.MetaTables[idxCommands]) then
    try
      Result := (BreakLine <> Self.BreakLine) or (Order <> Self.Order);
      if Result then
        begin
          DataSet := MetaData.MetadataDB.CreateQuery(qtUpdate);
          try
            DataSet.Descr.BeginUpdate;
            try
              DataSet.Descr.Table := tblCommand;
              DataSet.Descr.AddParamCondition(fldCommandID, ftInteger, opEQ, 'ID', ID);
              DataSet.Descr.AddParamField(fldCommandBreak, ftInteger);
              DataSet.Descr.ParamValueByName[fldCommandBreak] := BooleanValues[BreakLine];
              DataSet.Descr.AddParamField(fldCommandOrder, ftInteger);
              DataSet.Descr.ParamValueByName[fldCommandOrder] := Order;
            finally
              DataSet.Descr.EndUpdate;
            end;
            Result := DataSet.ExecuteQuery;
          finally
            DataSet.Free;
          end;
        end;
    except
      on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog('TDeActionData.UpdateSort(%s, %d) for %d skip error: %s', [BooleanNames[BreakLine], Order, ID, E.Message]);
          {$ENDIF}
          Result := False;
        end;
    end;
  if Result then
    begin
      FBreakLine := BreakLine;
      FOrder := Order;
    end;
end;

function TDeActionData.GetExecuteScriptIdentType(const aIdent: string; var aType: TFieldType; aDataObject: pointer): Boolean;
var
  Variable: TVariableItem;
begin
  Result := False;
  if Assigned(FScriptVaribales) then
    begin
      Variable := FScriptVaribales.GetByName(aIdent, False);
      if Assigned(Variable) then
        begin
          aType := ftUnknown;
          Exit(True);
        end;
    end
end;

function TDeActionData.GetExecuteScriptIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
var
  Variable: TVariableItem;
begin
  Result := False;
  if Assigned(FScriptVaribales) then
    begin
      Variable := FScriptVaribales.GetByName(aIdent, False);
      if Assigned(Variable) then
        begin
          aValue := Variable.Value;
          Exit(True);
        end;
    end
end;

function TDeActionData.ExecuteScript(const Script: string; Variables: TDeVariableList): Boolean;
var
  Strings: TStrings;
  ActionIndex, InnerIndex: Integer;
  Value: string;
  Parser: TDeParser;
  Calculator: TDeCalculator;
  ExpressionItem: TExpressionItem;
  Variable: TVariableItem;
begin
  try
    FScriptVaribales := Variables;
    Strings := TStringList.Create;
    try
      Strings.Text := Trim(Script);
      Result := Strings.Count <> 0;
      if Result then
        begin
          Result := False;
          Parser := TDeParser.Create;
          try
            Parser.onGetIdentType := GetExecuteScriptIdentType;
            // Строку ниже можно раскомментить, но особо смысла в идентификаторах не вижу, т.к. идёт выполнение процедур!
            // if Assigned(MetaData) then Parser.Table := MetaData.GetTableMeta(DataSet);
            Calculator := TDeCalculator.Create;
            try
              Calculator.OnGetIdentValue := GetExecuteScriptIdentValue;
                  Value := Strings.Text;
                  if (Length(Value) > 0) and Assigned(ProfitActions) then
                    begin
                      ActionIndex := ProfitActions.IndexByOriginal(Value);
                      if ActionIndex <> -1 then
                        begin
                          ProfitActions[ActionIndex].Execute;
                          Result := True;
                        end
                      else
                        begin
                          ExpressionItem := TExpressionItem.Create;
                          try
                            Parser.Parse(Value, ExpressionItem);
                            if ExpressionItem.Count <> 0 then
                              try
                                Calculator.Calculate(ExpressionItem);
                                for InnerIndex := 0 to Pred(Calculator.LocalVariables.Count) do
                                  begin
                                    Value := Calculator.LocalVariables[InnerIndex].Name;
                                    Variable := Variables.GetByName(Value, False);
                                    if Assigned(Variable) then
                                      Variable.Value := Calculator.LocalVariables[InnerIndex].Value;
                                  end;
                                Result := True;
                              except
                                {$IFDEF DEBUG}
                                on E: Exception do
                                  if Length(Original) = 0 then
                                    DebugLog('%s.ExecuteScript(%s) for %d skip calculate error: %s', [ClassName, QuotedStr(ReplaceText(Script, #13#10, ' ')), ID, E.Message])
                                  else
                                    DebugLog('%s.ExecuteScript(%s) for %s skip calculate error: %s', [ClassName, QuotedStr(ReplaceText(Script, #13#10, ' ')), QuotedStr(Original), E.Message]);
                                {$ENDIF}
                              end;
                          finally
                            ExpressionItem.Free;
                          end;
                        end;
                    end;
            finally
              Calculator.Free;
            end;
          finally
            Parser.Free;
          end;
        end;
    finally
      Strings.Free;
      FScriptVaribales := nil;
    end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        if Length(Original) = 0 then
          DebugLog('%s.ExecuteScript(%s) for %d skip error: %s', [ClassName, QuotedStr(ReplaceText(Script, #13#10, ' ')), ID, E.Message])
        else
          DebugLog('%s.ExecuteScript(%s) for %s skip error: %s', [ClassName, QuotedStr(ReplaceText(Script, #13#10, ' ')), QuotedStr(Original), E.Message]);
        {$ENDIF}
        Result := False;
      end;
  end;
end;

procedure TDeActionData.ExecuteDataSet;
var
  aContext: TContextMeta;
begin
  if DataSet <> -1 then
    begin
      aContext:= TContextMeta.Create(nil);
      aContext.Dataset:= MetaData.GetTableMeta(DataSet);
      aContext.ViewParams:= ActionParams;
      if Assigned(aContext.Dataset) then
        MForm.DoData(nil, aContext)
      else
        Application.MessageBox(PChar(GetTitle('_eRror.notcacheforaction')),
                               PChar(GetTitle('_dE.error')), MB_OK or MB_ICONERROR or MB_APPLMODAL);
      aContext.Free;
    end
  else
    Application.MessageBox(PChar(GetTitle('_eRror.notdatasetforaction')),
                           PChar(GetTitle('_dE.error')), MB_OK or MB_ICONERROR or MB_APPLMODAL);
end;

class function TDeActionData.PrepareCommandLineParameters(const CommandID: Integer): string;
var
  R: Integer;
  Database: TDeCustomDatabase;
  Dataset: TDeDataset;
  ParamName, ParamValue: string;
  Value: Variant;
begin
  Result := EmptyStr;
  try
    if Assigned(MetaData) then
      begin
        Database := MetaData.MetadataDB;
        if Assigned(Database) then
          begin
            Dataset := Database.CreateQuery(qtSelect);
            try
              Dataset.Descr.BeginUpdate;
              try
                Dataset.Descr.Table := tblCommandParams;
                Dataset.Descr.AddFields([fldCommandParamName, fldCommandParamValue]);
                Dataset.Descr.AddParamCondition(fldCommandParamCommand, ftInteger, opEQ, 'ID', CommandID);
                if mpCommandParamOrders in Metadata.MetadataPresents then
                  Dataset.Descr.AddSortFields([fldCommandParamOrder, fldCommandParamName])
                else
                  Dataset.Descr.AddSortField(fldCommandParamName);
              finally
                Dataset.Descr.EndUpdate;
              end;
              Dataset.Open;
              for R:=0 to Pred(Dataset.RecordCount) do
                begin
                  Dataset.RecNo:= R;
                  ParamName := Trim(Dataset.ValueByName[fldCommandParamName]);
                  if Length(ParamName) <> 0 then
                    begin
                      Value := Dataset.ValueByName[fldCommandParamValue];
                      if VarIsEmpty(Value) or VarIsNull(Value) then
                        ParamValue := EmptyStr
                      else
                        begin
                          ParamValue := Trim(Value);
                          if Length(ParamValue) <> 0 then
                            ParamValue := ':=' + QuotedStr(ParamValue);
                        end;
                      if Length(Result) <> 0 then Result := Result + ' ';
                      Result := Result + ParamName + ParamValue;
                    end;
                end;
            finally
              Dataset.Free;
            end;
          end;
      end;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('TDeActionData.PrepareCommandLineParameters(%d) error: %s', [CommandID, E.Message]);
    {$ENDIF}
  end;
end;

function TDeActionData.PrepareCommandLineParameters: string;
begin
  Result := PrepareCommandLineParameters(ID);
end;

function TDeActionData.GetVariantGUID: Variant;
begin
  if alsGUID in FStates then
    Result := GUIDToString(FGUID)
  else
    Result := Null;
end;

procedure TDeActionData.CheckDefaultMode;
var
  Parameters: TDeVariableList;
  Value: string;
begin
  Parameters := TDeVariableList.Create;
  try
    Parameters.LoadCommaText2(ActionParams);
    Value := Trim(VarToStr(Parameters.GetValueByName(varActionMode)));
    if SameText(Value, 'FOCUSED') then
      FDefaultMode := amFocused
    else if SameText(Value, 'MULTIIN') then
      FDefaultMode := amMultiIn
    else if SameText(Value, 'MULTIOUT') then
      FDefaultMode := amMultiOut
    else
      FDefaultMode := amMultiIn; // до v.16.10 это стандартное поведение при выполнении действия
    // ВНИМАНИЕ!!!
    // В данной процедуре недопустимо ставить amDefault для свойства FDefaultMode
    // и в этой процедуре надо ВСЕГДА заменять amDefault на другой режим!
  finally
    Parameters.Free;
  end;
end;
{$ENDREGION}

{$REGION 'Class: TDeActionResult ...'}
constructor TDeActionResult.Create(const AText: string);
begin
  FDateTime := Now;
  if SameText(Copy(AText, 1, 5), 'info:') then
    begin
      FStatus := asInformation;
      FText := Copy(AText, 6, MaxInt);
    end
  else if SameText(Copy(AText, 1, 7), 'result:') then
    begin
      FStatus := asResult;
      FText := Copy(AText, 8, MaxInt);
    end
  else if SameText(Copy(AText, 1, 6), 'error:') then
    begin
      FStatus := asError;
      FText := Copy(AText, 7, MaxInt);
    end
  else
    FText := AText;
end;
{$ENDREGION}

{$REGION 'Class: TDeActionResults ...'}
constructor TDeActionResults.Create(AOwner: TDeActionData);
begin
  FOwner := AOwner;
end;

destructor TDeActionResults.Destroy;
begin
  FreeAndNil(FInfoList);
  FreeAndNil(FResultList);
  FreeAndNil(FErrorList);
  FreeAndNil(FList);
  inherited Destroy;
end;

function TDeActionResults.GetCount: Integer;
begin
  if Assigned(FList) then
    Result := FList.Count
  else
    Result := 0;
end;

function TDeActionResults.GetItem(const Index: Integer): TDeActionResult;
begin
  if (Index >= 0) and (Index < Count) then
    Result := FList[Index] as TDeActionResult
  else
    Result := nil;
end;

function TDeActionResults.GetInfoCount: Integer;
begin
  if Assigned(FInfoList) then
    Result := FInfoList.Count
  else
    Result := 0;
end;

function TDeActionResults.GetInfos(const Index: Integer): TDeActionResult;
begin
  if (Index >= 0) and (Index < InfoCount) then
    Result := FInfoList[Index]
  else
    Result := nil;
end;

function TDeActionResults.GetResultCount: Integer;
begin
  if Assigned(FResultList) then
    Result := FResultList.Count
  else
    Result := 0;
end;

function TDeActionResults.GetResults(const Index: Integer): TDeActionResult;
begin
  if (Index >= 0) and (Index < ResultCount) then
    Result := FResultList[Index]
  else
    Result := nil;
end;

function TDeActionResults.GetErrorCount: Integer;
begin
  if Assigned(FErrorList) then
    Result := FErrorList.Count
  else
    Result := 0;
end;

function TDeActionResults.GetErrors(const Index: Integer): TDeActionResult;
begin
  if (Index >= 0) and (Index < ErrorCount) then
    Result := FErrorList[Index]
  else
    Result := nil;
end;

procedure TDeActionResults.Clear;
begin
  if Assigned(FInfoList) then FInfoList.Clear;
  if Assigned(FErrorList) then FErrorList.Clear;
  if Assigned(FResultList) then FResultList.Clear;
  if Assigned(FList) then FList.Clear;
end;

function TDeActionResults.Add(const Text: string): Integer;
var
  ActionResult: TDeActionResult;
begin
  if not Assigned(FList) then FList := TObjectList.Create;
  ActionResult := TDeActionResult.Create(Text);
  Result := FList.Add(ActionResult);
  if Result <> -1 then
    case ActionResult.Status of
      asInformation: { Информационное сообщение }
        begin
          if not Assigned(FInfoList) then FInfoList := TList.Create;
          FInfoList.Add(ActionResult);
        end;
      asResult: { Результат операции }
        begin
          if not Assigned(FResultList) then FResultList := TList.Create;
          FResultList.Add(ActionResult);
        end;
      asError: { Ошибка выполнения }
        begin
          if not Assigned(FErrorList) then FErrorList := TList.Create;
          FErrorList.Add(ActionResult);
        end;
    end
  else
    ActionResult.Free;
end;

procedure TDeActionResults.Delete(const Index: Integer);
var
  ActionResult: TDeActionResult;
begin
  ActionResult := Items[Index];
  if Assigned(FInfoList) then FInfoList.Remove(ActionResult);
  if Assigned(FResultList) then FResultList.Remove(ActionResult);
  if Assigned(FErrorList) then FErrorList.Remove(ActionResult);
  if Assigned(FList) then FList.Delete(Index);
end;

function TDeActionResults.PrepareText(const Status: TActionStatus; const MaxLength: Integer; const Separator: string): string;
var
  Index: Integer;
begin
  Result := EmptyStr;
  for Index := 0 to Pred(Count) do
    if Items[Index].Status = Status then
      begin
        if Length(Result) <> 0 then Result := Result + Separator;
        Result := Result + Items[Index].Text;
      end;
  if MaxLength <> 0 then
    Result := Copy(Result, 1, MaxLength) + ' ...';
end;

{$IFDEF DEBUG}
procedure TDeActionResults.DebugResultsLog(const Text: string);
  function PrepareResultLog(ActionResult: TDeActionResult): string;
  const
    StatusNames: array[TActionStatus] of PChar = ('asUnknown', 'asInformation', 'asResult', 'asError');
  begin
    Result := EmptyStr;
    if Assigned(ActionResult) then
      begin
        Result := Format(' %s | %-16s | %-104s |', [
          FormatDateTime('DD"."MM"."YYYY" "HH":"NN":"SS"."ZZZ', ActionResult.FDateTime),
          StrPas(StatusNames[ActionResult.Status]), Copy(ActionResult.Text, 1, 104)
          ]);
      end
    else
      Result := Format('%s|%s|%s|', [
        DupeString(' ', 25), DupeString(' ', 18), DupeString(' ', 106)
        ]);
  end;
  function PrepareResultsLog: string;
  var
    IndexSize, Index: Integer;
    Value: string;
  begin
    Result := EmptyStr;
    IndexSize := Length(IntToStr(Count));
    for Index := 0 to Pred(Count) do
      Result := Result + Format(#13#10'                        | %*d. |%s', [
        IndexSize, Index, PrepareResultLog(Items[Index])]);
    if Length(Result) <> 0 then
      begin
        Value := Format(#13#10'                        +%s+%s+%s+%s+', [
          DupeString('-', IndexSize + 3),
          DupeString('-', 25), DupeString('-', 18), DupeString('-', 106)
          ]);
        Result := Value +
          Format(#13#10'                        | %*s | %-23s | %-16s | %-104s |', [
            -Succ(IndexSize), 'No', 'Date & Time', 'Status', 'Text'
            ]) +
          Value + Result + Value;
      end;
  end;
begin
  DebugLog(Text + PrepareResultsLog);
end;
{$ENDIF}
{$ENDREGION}

{$REGION 'Class: TDeActionList ...'}
function TDeActionList.GetAction(const Index: Integer): TDeActionData;
begin
  Result := TDeActionData(inherited Items[Index]);
end;

function TDeActionList.GetExecutingAction: TDeActionData;
begin
  if Assigned(FActionResults) then
    Result := FActionResults.Owner
  else
    Result := nil;
end;

function TDeActionList.IndexByOriginal(const Original: string): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Pred(Count) do
    if SameText(Original, Items[Index].Original) then
      begin
        Result := Index;
        Break;
      end;
end;

function TDeActionList.IndexByID(const ActionID: Integer): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Pred(Count) do
    if Items[Index].ID = ActionID then
      begin
        Result := Index;
        Break;
      end;
end;

function TDeActionList.CreateCommandDataset(const ActionID: Integer): TDeDataset;
  procedure PrepareQueryDescr;
    procedure AppendCheckField(const FieldName: string);
    begin
      if Assigned(MetaData.MetaTables[idxCommands].Fields) then
        if MetaData.MetaTables[idxCommands].Fields.IndexByName(FieldName) <> -1 then
          Result.Descr.AddField(FieldName);
    end;
  begin
    try
      Result.Descr.BeginUpdate;
      try
        Result.Descr.Table := tblCommand;
        Result.Descr.AddFields([fldCommandID, fldCommandDataSet, fldCommandICO,
          fldCommandType, fldCommandOrder, fldCommandName, fldCommandCategory,
          fldCommandQuestion, fldCommandBreak, fldCommandActive, fldCommandFile,
          fldCommandProcedure, fldCommandParam, fldCommandShortCut,
          fldCommandEnabled, fldCommandVisible, fldCommandOriginal]);
        AppendCheckField(fldCommandBefore);
        AppendCheckField(fldCommandAfter);
        // 28.06.2016 -
        if mpDatabaseCommandGUID in Metadata.MetadataPresents then
          Result.Descr.AddField(fldCommandGUID);
        if ActionID = 0 then
          begin
            Result.Descr.AddCondition(fldCommandDeleted, ftSmallint, opNE, 1);
            Result.Descr.AddSortField(fldCommandOrder);
          end
        else
          begin
            Result.Descr.AddParamCondition(fldCommandID, ftSmallint, opEQ, 'CID', ActionID);
            Result.Descr.AddField(fldCommandDeleted);
          end;
      finally
        Result.Descr.EndUpdate;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog('%s.CreateCommandDataset(%d).PrepareQueryDescr exception: %s', [ClassName, ActionID, E.Message]);
          {$ENDIF}
          raise;
        end;
    end;
  end;
begin
  if Assigned(MetaData.MetaTables[idxCommands]) then
    if ActionID = 0 then
      begin
        Result := MetaData.MetadataDB.CreateQuery(qtSelect);
        try
          PrepareQueryDescr;
        except
          Result.Free;
          raise;
        end;
      end
    else
      begin
        Result := MetaData.MetadataDB.CreateQuery(qtRow);
        try
          PrepareQueryDescr;
        except
          Result.Free;
          raise;
        end;
      end
  else
    Result := nil;
end;

function TDeActionList.CreateCommandLinkParametersDataset(const ActionID: Integer): TDeDataset;
begin
  if Assigned(MetaData.MetaTables[idxCommandParams]) then
    begin
      Result := MetaData.MetadataDB.CreateQuery(qtSelect);
      try
        Result.Descr.BeginUpdate;
        try
          Result.Descr.Table := tblCommandParams;
          Result.Descr.AddCondition(fldCommandParamDataSet, ftSmallInt, opIsNot, Null);                       // Только линки!!!
          Result.Descr.AddParamCondition(fldCommandParamType, ftSmallInt, opEQ, 'TID', Integer(etDefault)); // = 6
          Result.Descr.AddCondition(fldCommandParamHidden, ftSmallInt, opEQ, 0);                // Только не скрытые!!!
          Result.Descr.AddOperation(opAnd);
          Result.Descr.AddOperation(opAnd);
          if ActionID = 0 then
            begin
              Result.Descr.AddFields([fldCommandParamCommand, fldCommandParamDataSet]);
              if mpCommandParamOrders in Metadata.MetadataPresents then
                Result.Descr.AddSortFields([fldCommandParamCommand, fldCommandParamOrder, fldCommandParamName])
              else
                Result.Descr.AddSortFields([fldCommandParamCommand, fldCommandParamName]);
            end
          else
            begin
              Result.Descr.AddParamCondition(fldCommandParamCommand, ftInteger, opEQ, 'CID', ActionID);
              Result.Descr.AddOperation(opAnd);
              Result.Descr.AddField(fldCommandParamDataSet);
              if mpCommandParamOrders in Metadata.MetadataPresents then
                Result.Descr.AddSortFields([fldCommandParamOrder, fldCommandParamName])
              else
                Result.Descr.AddSortField(fldCommandParamName);
            end
        finally
          Result.Descr.EndUpdate;
        end;
      except
        on E: Exception do
          begin
            {$IFDEF DEBUG}
            DebugLog('%s.CreateCommandLinkParametersDataset(%d) exception: %s', [ClassName, ActionID, E.Message]);
            {$ENDIF}
            Result.Free;
            raise;
          end;
      end;
    end
  else
    Result := nil;
end;

procedure TDeActionList.LoadCommands;
var
  DataSet: TDeDataset;
  Action: TDeActionData;
  R, Index: Integer;
begin
  if Assigned(MetaData.MetaTables[idxCommands]) then
    begin
      DataSet := CreateCommandDataset(0); // Полный список ...
      try
        DataSet.Open;
        for R:=0 to Pred(DataSet.RecordCount) do
          begin
            DataSet.RecNo:= R;
            Index := -1;
            try
              Action := TDeActionData.Create;
              Action.Assign(DataSet);

              if (Assigned(UserSession) and UserSession.IsAdmin) or VarIsEmpty(Action.Solution) or (Action.Solution<>MetaSolution) then
                Index:= Add(Action)
              else
                Index:= -1;
            finally
              if Index = -1 then Action.Free;
            end;
          end;
      finally
        DataSet.Free;
      end;
      {$IFDEF DEBUG}
      DebugActionsLog(Format('Actions for %s loaded ...', [tblCommand]));
      {$ENDIF}
    end;
end;

procedure TDeActionList.LoadCommandLinkParameters;
var
  DataSet: TDeDataset;
  Action: TDeActionData;
  R, CommandID, LinkID, Index: Integer;
begin
  if Assigned(MetaData.MetaTables[idxCommandParams]) then
    begin
      DataSet := CreateCommandLinkParametersDataset(0); // Полный список ...
      try
        DataSet.Open;
        Action := nil;
        for R:=0 to Pred(DataSet.RecordCount) do
          begin
            DataSet.RecNo:= R;
            CommandID := DataSet.Value[0]; // fldCommandParamCommand
            if Assigned(Action) and (Action.ID <> CommandID) then
              Action := nil;
            if not Assigned(Action) then
              begin
                Index := IndexByID(CommandID);
                if Index <> -1 then Action := Items[Index];
              end;
            if Assigned(Action) then
              begin
                LinkID := DataSet.Value[1]; // fldCommandParamDataSet
                if not Action.IsSupportLinkDataSet(LinkID) then
                  begin
                    SetLength(Action.FDataSetIDs, Succ(Length(Action.FDataSetIDs)));
                    Action.FDataSetIDs[High(Action.FDataSetIDs)] := LinkID;
                  end;
              end;
          end;
      finally
        DataSet.Free;
      end;
      {$IFDEF DEBUG}
      DebugActionsLog(Format('Actions for %s link parameters loaded ...', [tblCommand]));
      {$ENDIF}
    end;
end;

procedure TDeActionList.LoadData;
begin
  {$IFDEF DEBUG}
  DebugLog('%s.LoadData start ...', [ClassName]);
  {$ENDIF}
  LoadCommands;
  LoadCommandLinkParameters;
  {$IFDEF DEBUG}
  DebugLog('%s.LoadData finish ...', [ClassName]);
  {$ENDIF}
  {$IFDEF DEBUG}
  if Count <> 0 then WriteLog('Command list loaded ...', False, 'CommandLine');
  {$ENDIF}
end;

procedure TDeActionList.UpdateData(const ActionID: Integer; const Changed: TTypeChanged);
var
  DataSet, LinkDataset: TDeDataset;
  Action: TDeActionData;
  R, T, CommandID, LinkID, Index: Integer;
begin
  if Assigned(MetaData.MetaTables[idxCommands]) then
    begin
      DataSet := CreateCommandDataset(ActionID);
      try
        DataSet.Open;
        for R:=0 to Pred(DataSet.RecordCount) do
          begin
            DataSet.RecNo:= R;
            CommandID := VarToInt(DataSet.ValueByName[fldCommandID]);
            Index := IndexByID(CommandID);
            if Index <> -1 then
              begin
                Items[Index].FStates := [];
                Items[Index].Assign(DataSet);
                if DataSet.IndexByName(fldCommandDeleted) <> -1 then
                  if VarToInt(DataSet.ValueByName[fldCommandDeleted]) <> 0 then
                    begin
                      Items[Index].FEnabledStr := 'False';
                      {$IFDEF DEBUG}
                      DebugLog('Deleted action %d disabled ...', [CommandID]);
                      {$ENDIF}
                      CommandID := 0;
                    end;
                if CommandID <> 0 then
                  SetLength(Items[Index].FDataSetIDs, 0);
              end
            else
              begin
                CommandID := 0;
                if Changed = mcInsert then
                  begin
                    Action := TDeActionData.Create;
                    try
                      Action.Assign(DataSet);
                      Index := Add(Action);
                      CommandID := Action.ID;
                    finally
                      if Index = -1 then Action.Free;
                    end;
                    {$IFDEF DEBUG}
                    if Index <> -1 then
                      DebugLog('Inserted action %d loaded ...', [CommandID]);
                    {$ENDIF}
                  end;
              end;
            // Если нужно обновить линки, то ...
            if (Index <> -1) and (CommandID <> 0) then
              begin
                LinkDataset := CreateCommandLinkParametersDataset(CommandID);
                try
                  Action := Items[Index];
                  LinkDataset.Open;
                  for T:=0 to Pred(LinkDataset.RecordCount) do
                    begin
                      LinkDataset.RecNo:= T;
                      LinkID := LinkDataset.Value[0]; // fldCommandParamDataSet
                      if not Action.IsSupportLinkDataSet(LinkID) then
                        begin
                          SetLength(Action.FDataSetIDs, Succ(Length(Action.FDataSetIDs)));
                          Action.FDataSetIDs[High(Action.FDataSetIDs)] := LinkID;
                        end;
                    end;
                  {$IFDEF DEBUG}
                  if Length(Action.FDataSetIDs) <> 0 then
                    DebugLog('Link dataset action %d loaded ...', [CommandID]);
                  {$ENDIF}
                finally
                  LinkDataset.Free;
                end;
              end;
          end;
      finally
        DataSet.Free;
      end;
      {$IFDEF DEBUG}
      DebugActionsLog(Format('Actions for %s updated ...', [tblCommand]));
      {$ENDIF}
    end;
end;

procedure TDeActionList.BeforeExecute(Action: TDeActionData);
begin
  Assert(Assigned(Action), 'BeforeExecute action is nil!');
  if not Assigned(FActionResults) then
    FActionResults := TDeActionResults.Create(Action);
end;

procedure TDeActionList.AfterExecute(Action: TDeActionData; Variables: TDeVariableList);
begin
  Assert(Assigned(Action), 'AfterExecute action is nil!');
  if Assigned(FActionResults) and (FActionResults.Owner = Action) then
    begin
      // Здесь логика показа ВСЕХ сообщений от выполнения Action ...
      {$IFDEF DEBUG}
      FActionResults.DebugResultsLog(Format('TDeActionList.AfterExecute({%d}) ...', [Action.ID]));
      {$ENDIF}
      // ...
      if FActionResults.ErrorCount <> 0 then
        SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(GetTitle(Action.Caption, ttSecondName) +
          #10#13 + FActionResults.PrepareText(asError))))
      else if FActionResults.InfoCount <> 0 then
        SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(GetTitle(Action.Caption, ttSecondName) +
          #10#13 + FActionResults.PrepareText(asInformation))))
      else if FActionResults.ResultCount <> 0 then
        SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoResult, NativeInt(PChar(GetTitle(Action.Caption, ttSecondName) +
          #10#13 + FActionResults.PrepareText(asResult))))
      else
        if Action.ActionType in [atPreview, atReport] then
          begin
            // Если была печать на принтер, то ...
            if Assigned(Variables) and (Length(VarToStr(Variables.GetValueByName(varPrinterName))) <> 0) then
              SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(GetTitle(Action.Caption, ttSecondName) +
                #10#13' _dL.procedureok')));
          end
        else
          if Assigned(Variables) then
            begin
              if DeStrToBoolean(VarToStr(Variables.GetValueByName(varShowResult)), False) then
                SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(GetTitle(Action.Caption, ttSecondName) +
                  #10#13' _dL.procedureok')));
            end
          else
            SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(GetTitle(Action.Caption, ttSecondName) +
              #10#13' _dL.procedureok')));
      // ...
      // Разрушаем список сообщений выполненного Action ...
      FreeAndNil(FActionResults);
    end;
end;

{$IFDEF DEBUG}
procedure TDeActionList.DebugActionsLog(const Text: string);
  function PrepareActionLog(ActionData: TDeActionData): string;
  const
    BooleanNames: array[Boolean] of Char = ('-', '+');
    ModeNames: array[TActionMode] of PChar = ('amDefault', 'amFocused', 'amMultiIn', 'amMultiOut');
  var
    Value: string;
  begin
    Result := EmptyStr;
    if Assigned(ActionData) then
      begin
        if alsGUID in ActionData.States then
          Value := GUIDToString(ActionData.GUID)
        else
          Value := EmptyStr;
        if Length(ActionData.FDataSetIDs) <> 0 then
          begin
            Result := VariantToString(ActionData.FDataSetIDs);
            if (Length(Result) <> 0) and (Result[1] = '[') and (Result[Length(Result)] = ']') then
              Result := Copy(Result, 2, Length(Result) - 2);
          end
        else
          Result := EmptyStr;
        Result := Format(' %9d | %-10s | %-20s | %9d | %-76s | %22s | %-12s | %-46s | %-46s | %-46s | %9d | %9d | %s | %-38s | %-12s |', [
          ActionData.ID, StrPas(ActionSources[ActionData.FActionSource]), ActionTypeToString(ActionData.ActionType),
          ActionData.DataSet, ActionData.Caption, ShortCutToText(ActionData.ShortCut),
          StrPas(ActionCategories[ActionData.Category]), ActionData.SubCategories, ActionData.Original,
          Result, ActionData.ICO, ActionData.Order, BooleanNames[ActionData.BreakLine], Value,
          StrPas(ModeNames[ActionData.DefaultMode])]);
      end
    else
      Result := Format('%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|   |%s|%s|', [
        DupeString(' ', 11), DupeString(' ', 12), DupeString(' ', 22),
        DupeString(' ', 11), DupeString(' ', 78), DupeString(' ', 24),
        DupeString(' ', 14), DupeString(' ', 48), DupeString(' ', 48),
        DupeString(' ', 48), DupeString(' ', 11), DupeString(' ', 11),
        DupeString(' ', 40), DupeString(' ', 14)]);
  end;
  function PrepareActionsLog: string;
  var
    IndexSize, Index: Integer;
    Value: string;
  begin
    Result := EmptyStr;
    IndexSize := Length(IntToStr(Count));
    for Index := 0 to Pred(Count) do
      Result := Result + Format(#13#10'                        | %*d. |%s', [
        IndexSize, Index, PrepareActionLog(Items[Index])]);
    if Length(Result) <> 0 then
      begin
        Value := Format(#13#10'                        +-%s--+%s+%s+%s+%s+%s+%s+%s+%s+%s+%s+%s+%s+---+%s+%s+', [
          DupeString('-', IndexSize),
          DupeString('-', 11), DupeString('-', 12), DupeString('-', 22), DupeString('-', 11), DupeString('-', 78),
          DupeString('-', 24), DupeString('-', 14), DupeString('-', 48), DupeString('-', 48), DupeString('-', 48),
          DupeString('-', 11), DupeString('-', 11), DupeString('-', 40), DupeString('-', 14)]);
        Result := Value +
          Format(#13#10'                        | %*s | %-9s | %-10s | %-20s | %-9s | %-76s | %-22s | %-12s | %-46s | %-46s | %-46s | %-9s | %-9s | B | %-38s | %-12s |', [
            -Succ(IndexSize), 'No', 'ID', 'Source',
            'Type', 'DataSet', 'Caption',
            'ShortCut', 'Category', 'SubCategories',
            'Original', 'DataSets', 'ICO', 'Order', 'GUID', 'Default Mode']) +
          Value + Result + Value;
      end;
  end;
begin
  DebugLog(Text + PrepareActionsLog);
end;
{$ENDIF}
{$ENDREGION}

{$REGION 'Class: TActionsContainer ...'}
constructor TActionsContainer.Create;
begin
  inherited Create;
  FActions := TDeActionList.Create;
  ContainerStorage.Add(Self);
end;

destructor TActionsContainer.Destroy;
begin
  UnactivateActions;
  FActions.Free;
  ContainerStorage.Extract(Self);
  inherited Destroy;
end;

procedure TActionsContainer.UnactivateActions;
var I : integer;
begin
  for I := 0 to FActions.Count-1 do
    ProfitActions.Remove(FActions[I]);
  FActions.Clear;
end;

function TActionsContainer.ActivateAction(const Name: string; OnExecute: TActionExecuteMethod; const TableID: Integer;
                                          const BreakLine: Boolean; const Icon: Integer;
                                          const Category: TActionCategory; const SubCategories: string): Integer;
var
  ActionData: TDeActionData;
begin
  ActionData := TDeActionData.Create(Name, OnExecute, TableID);
  Result := FActions.Add(ActionData);
  if Result <> -1 then
    begin
      ActionData.FBreakLine := BreakLine;
      ActionData.FCategory := Category;
      ActionData.FSubCategories := ReplaceText(SubCategories, '/', '\');
      if -1 < Icon then ActionData.ICO:= Icon;
      ProfitActions.Add(ActionData);
    end
  else
    ActionData.Free;
end;
{$ENDREGION}

{$REGION 'Class: TActionContainerStorage ...'}
function TActionContainerStorage.GetContainer(const Index: Integer): TActionsContainer;
begin
  Result := TActionsContainer(inherited Items[Index]);
end;

procedure TActionContainerStorage.Notify(Ptr: Pointer; Action: TListNotification);
begin
{  if FActionsActivated and (Action = lnAdded) then
    TActionsContainer(Ptr).ActivateActions;  {}
{  inherited Notify(Ptr, Action);{}
end;

procedure TActionContainerStorage.ActivateAll;
var
  Index: Integer;
begin
  for Index := 0 to Pred(Count) do
    Items[Index].ActivateActions;
end;

procedure TActionContainerStorage.DeactivateAll;
var
  Index: Integer;
begin
  for Index := 0 to Pred(Count) do
    Items[Index].UnactivateActions;
end;
{$ENDREGION}

{$REGION 'Class: TMetaDataOperations ...'}
procedure TMetaDataOperations.TestConnection(Sender: TObject);
var
  DataCache: TDataCache;
  CacheItem: TCacheItem;
  ItemDB, FoundDB: TDeCustomDatabase;
  Index: Integer;
begin
  if Assigned(Sender) and (Sender is TDeActionData) then
    DataCache := MetaData.FindActiveDataCache((Sender as TDeActionData).DataSet, True)
  else
    if Assigned(MetaData.MetaTables[idxBase]) then
      DataCache := MetaData.FindActiveDataCache(MetaData.MetaTables[idxBase].ID, True)
    else
      DataCache := nil;
  if Assigned(DataCache) then
    begin
      UpdateWindow(Application.MainForm.Handle);
      CacheItem := DataCache.FocusedItem;
      if Assigned(CacheItem) then
        begin
          if CacheItem.ID = 0 then ItemDB := MetaData.CreateDatabase(CurrentConfig, False)
                              else ItemDB := MetaData.CreateDatabase(CacheItem, False);
          try
            FoundDB := nil;
            for Index := 0 to Pred(MetaData.DatabasesCount) do
              if (CompareText(MetaData.Databases[Index].ConnectString, ItemDB.ConnectString) = 0) and
                 (MetaData.Databases[Index].DatabaseType = ItemDB.DatabaseType) then
              begin
                FoundDB := MetaData.Databases[Index];
                Break;
              end;
            if not Assigned(FoundDB) then
              FoundDB := ItemDB;
            if FoundDB.CheckConnection then
              if Assigned(ProfitActions) and Assigned(ProfitActions.FActionResults) then
                ProfitActions.FActionResults.Add('info:' + GetTitle('_De.ConnectOk'))
              else
                SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(GetTitle('_De.ConnectOk'))))
            else
              SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
                NativeInt(PChar('_De.ConnectEr'#10+FoundDB.ConnectErrorMessage)));
          finally
            ItemDB.Free;
          end;
        end;
    end;
end;

procedure TMetaDataOperations.RegTables(Sender: TObject);
var
  DataCache, NewDataCache: TDataCache;
  CacheItem, NewCacheItem: TCacheItem;
  Index: Integer;
  ItemDB, FoundDB: TDeCustomDatabase;
  DMan: TDataSetDataManager;
  Tables: TStringList;
  NewTable: TTableMeta;
  BKCursor: TCursor;
begin
  if Assigned(Sender) and (Sender is TDeActionData) then
    DataCache := MetaData.FindActiveDataCache((Sender as TDeActionData).DataSet, True)
  else
    if Assigned(MetaData.MetaTables[idxBase]) then
      DataCache := MetaData.FindActiveDataCache(MetaData.MetaTables[idxBase].ID, True)
    else
      DataCache := nil;
  if Assigned(DataCache) then
    begin
      UpdateWindow(Application.MainForm.Handle);
      CacheItem := DataCache.FocusedItem;
      if Assigned(CacheItem) then
        begin
          if CacheItem.ID = 0 then Exit; //Не грузим метаструктуру
          ItemDB := MetaData.CreateDatabase(CacheItem, False);
          SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, 100, 1);
          try
            FoundDB := nil;
            for Index := 0 to Pred(MetaData.DatabasesCount) do
              if (CompareText(MetaData.Databases[Index].ConnectString, ItemDB.ConnectString) = 0) and
                 (MetaData.Databases[Index].DatabaseType = ItemDB.DatabaseType) then
                begin
                  FoundDB := MetaData.Databases[Index];
                  Break;
                end;
            if not Assigned(FoundDB) then
              FoundDB := ItemDB;
            if FoundDB.CheckConnection then
              begin
                DMan := TDataSetDataManager.Create;
                NewDataCache := TDataCache.Create(MetaData.MetaTables[idxDataset]);
                MetaData.BeginUpdate;

                Tables := TStringList.Create;
                BKCursor := Screen.Cursor;
                Screen.Cursor := crHourGlass;
                try
                  FoundDB.RetrieveTableNames(Tables);
                  for Index := 0 to Pred(Tables.Count) do
                    begin
                      NewCacheItem := NewDataCache.AddNewItem;
                      NewCacheItem.ValueNativeByName[fldDataSetName] := Tables[Index];
                      NewCacheItem.ValueNativeByName[fldDataSetTable] := Tables[Index];
                      NewCacheItem.ValueByName[fldDataSetDatabase] := FoundDB.ID;
                      NewCacheItem.ValueByName[fldDataSetReadOnly] := 0;
                      NewCacheItem.ValueByName[fldDataSetSolution] := 0;
                      NewCacheItem.ValueByName[fldDataSetParent] := Null;
                      NewCacheItem.ValueByName[fldDataSetGViewType] := Ord(vtList);
                      NewCacheItem.ValueByName[fldDataSetGViewPars] := Null;
                      NewCacheItem.ValueByName[fldDataSetDeleted] := 0;
                      NewCacheItem.ValueByName[fldDataSetIsList] := 0;
                      NewCacheItem.ValueByName[fldDataSetOnAddMenue] := 0;
                      if DMan.InsertRecord(NewCacheItem) then
                        begin
                          NewTable := MetaData.GetTableMeta(VarToInt(NewCacheItem.ID));
                          if Assigned(NewTable) then
                            NewTable.Fields;
                        end;
                      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, (100 * Succ(Index)) div Tables.Count, 0);
                    end;

                finally
                  MetaData.EndUpdate;
                  Screen.Cursor := BKCursor;
                  Tables.Free;
                  NewDataCache.Free;
                  DMan.Free;
                  if Assigned(CacheItem) then
                    DataCache.Update(mcNone, Null);
                end;
              end
            else
              SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
                NativeInt(PChar(Format(GetTitle('_ConnectionFailed'),
                [FoundDB.ConnectErrorMessage]))));
          finally
            ItemDB.Free;
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
          end;
        end;
    end;
end;

procedure TMetaDataOperations.Consistency(Sender: TObject);
var
  DataCache: TDataCache;
  CacheItem: TCacheItem;
  Form: TForm_Da_Consistency;
begin
  if Assigned(Sender) and (Sender is TDeActionData) then
    begin
      DataCache := MetaData.FindActiveDataCache((Sender as TDeActionData).DataSet, True);
      if Assigned(DataCache) then
        begin
          CacheItem := DataCache.FocusedItem;
          if Assigned(CacheItem) then
            begin
              Form := TForm_Da_Consistency.Create(Application);
              try
                if Form.Init(CacheItem) then
                  Form.ShowModal;
              finally
                Form.Free;
              end;
            end;
        end;
    end;
end;

procedure TMetaDataOperations.OpenThisTable(Sender: TObject);
var
  DataCache: TDataCache;
  CacheItem: TCacheItem;
  aContext: TContextMeta;
begin
  if Assigned(Sender) and (Sender is TDeActionData) then
    DataCache:= MetaData.FindActiveDataCache((Sender as TDeActionData).DataSet, True)
  else
    if Assigned(MetaData.MetaTables[idxDataset]) then
      DataCache:= MetaData.FindActiveDataCache(MetaData.MetaTables[idxDataset].ID, True)
    else
      DataCache:= nil;
  if Assigned(DataCache) then
    begin
      UpdateWindow(Application.MainForm.Handle);
      CacheItem := DataCache.FocusedItem;
      if Assigned(CacheItem) then
        begin
          aContext:= TContextMeta.Create(nil);
          aContext.Dataset:= MetaData.GetTableMeta(CacheItem.ValueByName[fldDatasetID]);
          MForm.DoData(nil, aContext);
          aContext.Free;
        end;
    end;
end;

procedure TMetaDataOperations.TableExportData(Sender: TObject);
var
  DataCache, WorkDataCache: TDataCache;
  Directory: string;
  Index: Integer;
  TableMeta: TTableMeta;
  Value: WideString;
  FS: TFileStream;
  Buffer: TBytes;
begin
  if Assigned(Sender) and (Sender is TDeActionData) then
    DataCache := MetaData.FindActiveDataCache((Sender as TDeActionData).DataSet, True)
  else
    if Assigned(MetaData.MetaTables[idxDataset]) then
      DataCache := MetaData.FindActiveDataCache(MetaData.MetaTables[idxDataset].ID, True)
    else
      DataCache := nil;
  if Assigned(DataCache) then
    begin
      Directory := Variables.AsString[RegDirPath];
      if SelectDirectory(GetTitle('_Dl.OutputFiles'), EmptyStr, Directory) then
        begin
          Directory := IncludeTrailingBackslash(Directory);
          UpdateWindow(Application.MainForm.Handle);
          SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, DataCache.SelectedCount, 1);
          try
            for Index := 0 to Pred(DataCache.SelectedCount) do
              begin
                SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Index, 0);
                TableMeta := Metadata.GetTableMeta(DataCache.SelectedItems[Index].ValueByName[fldDataSetId]);
                WorkDataCache := TDataCache.Create(TableMeta);
                try
                  WorkDataCache.PrepareData(True, fsMax);
                  Value := WorkDataCache.GetAsXML([dfStoredFields, dfAllRecords]);
                finally
                  WorkDataCache.Free;
                end;

                Buffer:= TEncoding.UTF8.GetBytes(Value);
                if 0 < Length(Buffer) then
                  try
                    FS:= TFileStream.Create(Directory + TableMeta.Table + sExtensionXML, fmCreate or fmShareDenyWrite);
                    FS.WriteBuffer(Buffer, Length(Buffer));
                  finally
                    FS.Free;
                  end;
              end;
          finally
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
          end;
        end;
    end;
end;

procedure TMetaDataOperations.TableImportData(Sender: TObject);
var
  DataCache, NewDataCache: TDataCache;
  CacheItem: TCacheItem;
  TableMeta: TTableMeta;
  FileName: string;
  OpenDialog: TOpenDialog;
  Index: Integer;
  DataManager: TDataManager;
begin
  if Assigned(Sender) and (Sender is TDeActionData) then
    DataCache := MetaData.FindActiveDataCache((Sender as TDeActionData).DataSet, True)
  else
    if Assigned(MetaData.MetaTables[idxDataset]) then
      DataCache := MetaData.FindActiveDataCache(MetaData.MetaTables[idxDataset].ID, True)
    else
      DataCache := nil;
  if Assigned(DataCache) then
    begin
      CacheItem := DataCache.FocusedItem;
      if Assigned(CacheItem) then
        begin
          TableMeta := Metadata.GetTableMeta(CacheItem.ValueByName[fldDataSetId]);

          FileName := TableMeta.Table + sExtensionXML;
          OpenDialog := TOpenDialog.Create(Application);
          try
            OpenDialog.Title := GetTitle('_Dl.Location');
            OpenDialog.InitialDir := Variables.AsString[RegDirPath];
            OpenDialog.Filter := sXMLFilter;
            OpenDialog.DefaultExt := sExtensionXML;
            if FileExists(IncludeTrailingBackslash(OpenDialog.InitialDir) + FileName) then
              OpenDialog.FileName := FileName;
            if OpenDialog.Execute then
              begin
                FileName := OpenDialog.FileName;
                if FileExists(FileName) then
                  begin
                    UpdateWindow(Application.MainForm.Handle);
                    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, 100, 1);
                    try
                      NewDataCache := TDataCache.Create(TableMeta);
                      try
                        NewDataCache.PrepareData;
                        NewDataCache.FillFromXML({t}ReadStringFromFile(FileName));
                        DataManager := CreateDataManager(TableMeta);
                        try
                          for Index := 0 to Pred(NewDataCache.Count) do
                            begin
                              SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, (Index * 100) div NewDataCache.Count, 0);
                              DataManager.InsertRecord(NewDataCache.Items[Index]);
                            end;
                        finally
                          DataManager.Free;
                        end;
                      finally
                        NewDataCache.Free;
                      end;
                    finally
                      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
                    end;
                  end;
              end;
          finally
            OpenDialog.Free;
          end;
        end;
    end;
end;

procedure TMetaDataOperations.TableClearData(Sender: TObject);
var
  DataCache: TDataCache;
  Index: Integer;
  Value: string;
  FilterItem: TFilterItem;
  TableMeta: TTableMeta;
  DataManager: TDataManager;
begin
  if Assigned(Sender) and (Sender is TDeActionData) then
    DataCache := MetaData.FindActiveDataCache((Sender as TDeActionData).DataSet, True)
  else
    if Assigned(MetaData.MetaTables[idxDataset]) then
      DataCache := MetaData.FindActiveDataCache(MetaData.MetaTables[idxDataset].ID, True)
    else
      DataCache := nil;
  if Assigned(DataCache) then
    begin
      if DataCache.SelectedCount = 1 then
        Value := GetTitle(DataCache.SelectedItems[0].ValueByName[fldDataSetName])
      else
        Value := '#' + IntToStr(DataCache.SelectedCount);
      if Application.MessageBox(
           PChar(Format(GetTitle('_qUerypermdeleteall'),[Value])),
           PChar(GetTitle('_dL.Confirmation')),
           MB_YESNO or MB_ICONQUESTION) = idYes then
        begin
          UpdateWindow(Application.MainForm.Handle);
          SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, DataCache.SelectedCount, 1);
          try
            FilterItem := TFilterItem.Create;
            try
              FilterItem.Clear;
              for Index := 0 to Pred(DataCache.SelectedCount) do
                begin
                  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Index, 0);
                  TableMeta := Metadata.GetTableMeta(DataCache.SelectedItems[Index].ValueByName[fldDataSetId]);
                  DataManager := CreateDataManager(TableMeta);
                  try
                    DataManager.DeleteRecords(FilterItem);
                  finally
                    DataManager.Free;
                  end;
                end;
            finally
              FilterItem.Free;
            end;
          finally
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
          end;
        end;
    end;
end;

function MergeCache(const Source, Target: TDataCache; GetKeys: Boolean = True): Boolean;
var k, i: Integer;
    CI: TCacheItem;
begin
  Result:= True;
  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Source.Count, 1);
  for k:=0 to Source.Count-1 do
    try
      CI:= Target.AddNewItem;
      for i:=0 to Pred(CI.Owner.FieldCount) do
        if CI.Owner.Fields[i].IsStored then
          CI.InitFieldValue(i, Source.Items[k].FieldValue[i]);

      if CI.Owner.DataManager.InsertRecord(CI) then
        begin

        end
      else
        begin
          Result:= False;
          Break;
        end;

      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, k, 0);
    except
      on E: Exception do
        SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
                      NativeInt('MergeCache: '+ Target.TableMeta.Table + ' Record:' + IntToStr(k) + ' ' + E.Message));
    end;
  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
end;

function MigrationTable(const SourceTM: TTableMeta; const TargetDB: TDeCustomDataBase): Boolean;
var
  k, i: Integer;
  TargetTM: TTableMeta;
  SourceCache, TargetCache : TDataCache;
  CI: TCacheItem;
begin
  if Not assigned(SourceTM) then Exit(False);
  if Not assigned(SourceTM.Database) then Exit(False);
  if Not assigned(TargetDB) then Exit(False);

  if SourceTM.Database = TargetDB then Exit(False);

  if Not SourceTM.Database.Connected then Exit(False);
  if Not TargetDB.Connected then Exit(False);

  Result:= True;

  // Начитываем данные из источника
  SourceCache := TDataCache.Create(SourceTM);
  SourceCache.LoadData(nil, fsMax);
  SourceCache.FillAll;

  // формируем метаструктуру
  TargetTM:= TTableMeta.Create(SourceTM.Table, TargetDB);
  TargetTM.IsDynamic := True;
  TargetTM.AssignFields(SourceTM.Fields);
  TargetTM.ID:= null;
  TargetTM.IsReadOnly:= False;

  // Создаем физическую модель
  TargetDB.CreateTable(SourceTM);
  TargetCache := TDataCache.Create(TargetTM);
  TargetCache.LoadData(nil, fsMax);
  TargetCache.FillAll;

  Metadata.BeginUpdate;

  TargetCache.BeginUpdate;

  // if (ddlSequence in SourceTM.Database.SupportDDLs) then
  for i:=0 to Pred(TargetTM.Fields.Count) do
    if (TargetTM.Fields[i].Key) and (TargetTM.Fields[i].DataType in IntegerTypes) then
      begin
        TargetDB.Sequence(stInsert, TargetTM.Table, TargetTM.Fields[i].Original);
        TargetTM.Fields[i].Unique:= (TargetCache.Count > 0);    // сильно ускоряет вставку, т.к. не перезапрашивает
        TargetTM.Fields[i].ReadOnly:= (TargetCache.Count > 0);  // если таблица пустая - льем со старыми PKey
      end;

  MergeCache(SourceCache, TargetCache);

  // Инициализируем значения счетчиков
  // if (ddlSequence in SourceTM.Database.SupportDDLs) then
  for i:=0 to Pred(TargetTM.Fields.Count) do
    if (TargetTM.Fields[i].Key) and (TargetTM.Fields[i].DataType in IntegerTypes) then
      begin
        TargetDB.Sequence(stUpdate, TargetTM.Table, TargetTM.Fields[i].Original);
        TargetTM.Fields[i].Unique:= True;
        TargetTM.Fields[i].ReadOnly:= True;
      end;

  TargetCache.EndUpdate;

  TargetCache.Free;
  SourceCache.Free;

  Metadata.EndUpdate;
end;

procedure TMetaDataOperations.TablePurgeData(Sender: TObject);
var
  DataCache, PurgeCache: TDataCache;
  Indicator: TfrmIndicator;
  Index, PurgeCount, PurgeIndex: Integer;
  TableMeta: TTableMeta;
  Value: string;
begin
  if Assigned(Sender) and (Sender is TDeActionData) then
    DataCache := MetaData.FindActiveDataCache((Sender as TDeActionData).DataSet, True)
  else
    if Assigned(MetaData.MetaTables[idxDataset]) then
      DataCache := MetaData.FindActiveDataCache(MetaData.MetaTables[idxDataset].ID, True)
    else
      DataCache := nil;
  if Assigned(DataCache) then
    begin
      Indicator := TfrmIndicator.Create(Application);
      Indicator.Caption := GetTitle('_dA.DelDeleted');
      Indicator.Show;
      Value := EmptyStr;
      MetaData.BeginUpdate;
      try
        for Index := 0 to Pred(DataCache.SelectedCount) do
          begin
            TableMeta := Metadata.GetTableMeta(DataCache.SelectedItems[Index].ValueByName[fldDataSetID]);
            if Assigned(TableMeta) then
              begin
                // пропускаем порожденные наборы данных
                if TableMeta.OwnerTable <> TableMeta then
                  Continue;
                Indicator.ProcessTitle := GetTitle(TableMeta.Name);
                Indicator.Position := (100 * Index) div DataCache.SelectedCount;
                if Assigned(TableMeta.DField) and not TableMeta.IsReadOnly then
                  begin
                    PurgeCache := TDataCache.Create(TableMeta);
                    try
                      PurgeCache.Filters.ParentManager := nil;
                      PurgeCache.Filters.NewFilter(TableMeta.DField, opEQ, TableMeta.DField.Value2);
                      PurgeCache.PrepareData;
                      //PurgeCache.BeginUpdate;
                      if PurgeCache.Count <> 0 then
                        begin
                          PurgeCount := 0;
                          for PurgeIndex := Pred(PurgeCache.Count) downto 0 do
                            begin
                              Indicator.ProcessTitle := GetTitle(TableMeta.Name) + ': ' + IntToStr(Succ(PurgeIndex));
                              Indicator.Repaint;
                              if PurgeCache.DeleteRecord(PurgeCache[PurgeIndex]) then
                                Inc(PurgeCount);
                            end;
                          Value := Value + #10#13 + GetTitle(PurgeCache.TableMeta.Name) + ': ' + IntToStr(PurgeCount);
                        end;
                    finally
                      PurgeCache.Free;
                    end;
                  end;
              end;
          end;
        Indicator.ProcessTitle := '_Dl.ProcedureOk';
        if Length(Value) = 0 then
          Indicator.ResultInfo := GetTitle('_pRocess.IsNotRequired')
        else
          Indicator.ResultInfo := GetTitle('_pRocess.Tables') + Value;
      finally
        MetaData.EndUpdate;
      end;
    end;
end;
{
procedure TMetaDataOperations.LoadSolution(Sender: TObject);
var dlgSelectPath : TOpenDialog;
    ConfigManager : TConfigManager;
    BackupCursor  : TCursor;
    LoadRes       : boolean;
    Indicator     : TfrmIndicator;
    InitDir       : string;
    DataCache     : TDataCache;
begin
  if Assigned(Sender) and (Sender is TDeActionData) then
    DataCache := MetaData.FindActiveDataCache((Sender as TDeActionData).DataSet, True)
  else
    if Assigned(MetaData.MetaTables[idxSolutions]) then
      DataCache := MetaData.FindActiveDataCache(MetaData.MetaTables[idxSolutions].ID, True)
    else
      DataCache := nil;
  if not Assigned(DataCache) then Exit;

  Indicator := nil;
  dlgSelectPath := nil;
  try
  if Assigned(DataCache) then
    dlgSelectPath := TOpenDialog.Create(Application);
    with dlgSelectPath do
      begin
        Title := GetTitle('_Dm.SolChooseFile');
        DefaultExt := sConfigExt;
        Options := Options + [ofFileMustExist];
        Filter      := GetTitle(dlgFilters, ttFull);
        FilterIndex := dlgFilterConfig;
        InitDir := ExtractFilePath(Application.ExeName)+SolutionsFolder;
        if SysUtils.DirectoryExists(InitDir) then
          InitialDir := InitDir;
        if Execute then
        begin
//          if (Sender is TControl) and (TControl(Sender).Owner is TWinControl) then
//            UpdateWindow(TWinControl(TControl(Sender).Owner).Handle);
          ConfigManager := nil;
          BackupCursor := Screen.Cursor;
          LoadRes := False;
          try
            UpdateWindow(Application.MainForm.Handle);
            Screen.Cursor := crHourGlass;
            ConfigManager := TConfigManager.Create;
            ConfigManager.DataPath := dlgSelectPath.FileName;
            Indicator := TfrmIndicator.Create(Application);
            Indicator.Caption := GetTitle('_Dm.SolLoading');
            Indicator.ProcessTitle := '_Dt.Solution "'+ConfigManager.ConfigName+'"';
            ConfigManager.Indicator := Indicator;
            LoadRes := ConfigManager.LoadConfig;
            if LoadRes then
              DataCache.Update(mcNone, Null);
          finally
            Screen.Cursor := BackupCursor;
            if LoadRes then
              begin
                if ConfigManager.Errors.Count > 0 then
                  Indicator.ResultInfo:=ConfigManager.Errors.GetMessage
                else
                  if Not UserSession.IsAdmin then
                    Indicator.ResultInfo:='_Dl.procedureok _dM.sollogin'
                  else
                    Indicator.ResultInfo:='_Dl.procedureok';
              end
            else
              Indicator.ResultInfo:='_Dl.procedureerror';
            ConfigManager.Free;
            if LoadRes and (Not UserSession.IsAdmin) then
              NeedLogout := true;
          end;
        end;
      end;
  finally
    dlgSelectPath.Free;
  end;
end;
{}
procedure TMetaDataOperations.ClearReportParameters(const ReportID: Integer);
var
  DataManager: TDataManager;
  FilterItem: TFilterItem;
begin
  DataManager := CreateDataManager(MetaData.MetaTables[idxCommandParams]);
  try
    FilterItem := TFilterItem.Create;
    FilterItem.AddCondition(MetaData.MetaTables[idxCommandParams].Fields.FindByName(fldCommandParamCommand), opEQ, ReportID);
    try
      if not DataManager.DeleteRecords(FilterItem) then
        SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(DataManager.Errors.GetMessage)));
    finally
      FilterItem.Free;
    end;
  finally
    DataManager.Free;
  end;
end;

function TMetaDataOperations.PrepareTempReport(const ReportID: Integer): TDeReport;
var
  TableMeta: TTableMeta;
  Image: TDeImage;
  DataSet: TDeDataset;
  FilePath, FileNameOnly, FileExtension, FileName: string;
  Index: Integer;
  Text: AnsiString;
begin
  TableMeta := MetaData.MetaTables[idxCommands];
  if Assigned(TableMeta) and Assigned(TableMeta.Database) then
    begin
      Image := TDeImage.Create(nil);
      try
        DataSet := TableMeta.Database.CreateQuery(qtSelect);
        try
          DataSet.Descr.BeginUpdate;
          try
            DataSet.Descr.Table := TableMeta.Table;
            DataSet.Descr.AddField(fldCommandData);
            DataSet.Descr.AddParamCondition(fldCommandID, ftInteger, opEQ, 'ID', ReportID);
          finally
            DataSet.Descr.EndUpdate;
          end;
          DataSet.Open;
          Image.Value := DataSet.Value[0]; // fldCommandData
        finally
          DataSet.Free;
        end;
        FilePath := IncludeTrailingPathDelimiter(DeTempDir);
        FileNameOnly := NormalizeFileName(ChangeFileExt(Image.OriginalFileName, EmptyStr), True);
        FileExtension := ExtractFileExt(Image.OriginalFileName);
        Index := 1;
        FileName := FilePath + FileNameOnly + FileExtension;
        while FileExists(FileName) and (Index < 1000) do
          begin
            Inc(Index);
            FileName := FilePath + FileNameOnly + ' ('+ Trim(Format('%3.3d', [Index])) + ')' + FileExtension;
          end;
        {$IFDEF MICROSOFTCOM}
        if TMicrosoftExcelReport.IsSupportExtension(FileExtension) then
          begin
            Image.SaveToFile(FileName);
            Result := TMicrosoftExcelReport.Create(FileName);
          end
        else if TMicrosoftWordReport.IsSupportExtension(FileExtension) then
          begin
            Image.SaveToFile(FileName);
            Result := TMicrosoftWordReport.Create(FileName);
          end
        {$ELSE}
        if TDeExcelReport.IsSupportExtension(FileExtension) then
          begin
            Image.SaveToFile(FileName);
            Result := TDeExcelReport.Create(FileName);
          end
        else if TDeWordReport.IsSupportExtension(FileExtension) then
          begin
            Image.SaveToFile(FileName);
            Result := TDeWordReport.Create(FileName);
          end
        {$ENDIF}
        else if TDeTextReport.IsSupportExtension(FileExtension) then
          begin
            Result := TDeTextReport.Create(FileName);
            Image.SaveToString(Text);
            (Result as TDeTextBasedReport).DeTemplate.Text := Text;
          end
        else
          begin
            Result := TDeSimpleReport.Create(FileName);
            Image.SaveToString(Text);
            (Result as TDeSimpleReport).DeTemplate.Text := Text;
          end;
        DM.RegisterTemporaryFile(FileName);
      finally
        Image.Free;
      end;
    end
  else
    Result := nil;
end;

procedure TMetaDataOperations.TempCacheInsertReportParam(CacheItem: TCacheItem; var CanInsert: Boolean);
var
  TableMeta: TTableMeta;
begin
  with CacheItem do
    begin
      ValueByName[fldCommandParamCommand] := FTempReportID;
      ValueByName[fldCommandParamName] := FTempReportParamDef.Name;
      ValueByName[fldCommandParamType] := FTempReportParamDef.ParamType;
      if FTempReportParamDef.ParamType = etDateTime then
        ValueByName[fldCommandParamColumn] := 2;
      if Length(FTempReportParamDef.Source) <> 0 then
        begin
          TableMeta := MetaData.GetTableByPath(FTempReportParamDef.Source);
          if Assigned(TableMeta) then
            begin
              ValueByName[fldCommandParamDataSet] := TableMeta.ID;
              {
              v.19.01 удалено +
              // Строка ниже оставлена для совместимости и нужно убрать после удаления поля MCP_SOURCE!
              ValueByName[fldCommandParamSource] := FTempReportParamDef.Source; //VarToStr(FTempReportParamDef.Value);
              }
            end;
        end;
      ValueByName[fldCommandParamValue] := VarToStr(FTempReportParamDef.Value);
      if Length(FTempReportCaption) <> 0 then
        begin
          ValueNativeByName[fldCommandParamCaption] := FTempReportCaption;
          FTempReportLabel := EmptyStr;
        end;
      if Length(FTempReportLabel) <> 0 then
        begin
          ValueNativeByName[fldCommandParamCaption] := FTempReportLabel;
          FTempReportLabel := EmptyStr;
        end;
      if Length(FTempReportParamDef.Filter) <> 0 then
        ValueByName[fldCommandParamFilter] := FTempReportParamDef.Filter;
    end;
end;

procedure TMetaDataOperations.SaveReportParameters(const ReportID, CodePage: Integer; ParamDefs: TDeReportParamDefs);
var
  DataCache: TDataCache;
  Index: Integer;
  RecordID: Variant;
  A, D: AnsiString;
begin
  if Assigned(ParamDefs) and (ParamDefs.ParamCount <> 0) then
    begin
      DataCache := TDataCache.Create(MetaData.MetaTables[idxCommandParams]);
      DataCache.OnInsertRecord := TempCacheInsertReportParam;
      DataCache.BeginUpdate;
      try
        FTempReportID := ReportID;
        for Index := 0 to Pred(ParamDefs.ParamCount) do
          begin
            FTempReportParamDef := ParamDefs.ParamDef[Index];
            FTempReportCaption := EmptyStr;
            if Length(FTempReportParamDef.Caption) <> 0 then
              case CodePage of
                1251: { Win1251 }
                  begin
                    A := Trim(FTempReportParamDef.Caption);
                    FTempReportCaption := A;
                  end;
                cp866: { DOS }
                  begin
                    D := Trim(FTempReportParamDef.Caption);
                    SetLength(A, Length(D));
                    OemToAnsiBuff(PAnsiChar(D), PAnsiChar(A), Length(A));
                    FTempReportCaption := A;
                  end;
                0: { UTF-8 }
                  FTempReportCaption := Trim(UTF8ToString(RawByteString(FTempReportParamDef.Caption)));
              else
                FTempReportCaption := Trim(FTempReportParamDef.Caption);
              end;
            if FTempReportParamDef.ParamType = etLabel then
              case CodePage of
                1251: { Win1251 }
                  begin
                    A := Trim(FTempReportParamDef.Name);
                    FTempReportLabel := A;
                  end;
                866: { DOS }
                  begin
                    D := Trim(FTempReportParamDef.Name);
                    SetLength(A, Length(D));
                    OemToAnsiBuff(PAnsiChar(D), PAnsiChar(A), Length(A));
                    FTempReportLabel := A;
                  end;
                0: { UTF-8 }
                  FTempReportLabel := Trim(UTF8ToString(RawByteString(FTempReportParamDef.Name)));
              else
                FTempReportLabel := Trim(FTempReportParamDef.Name);
              end
            else
              DataCache.AppendRecord(RecordID);
          end;
      finally
        DataCache.EndUpdate;
        MetaData.UpdateDataSets(DataCache.TableMeta.ID, mcUpdate, null);
        DataCache.Free;
      end;
    end;
end;

procedure TMetaDataOperations.UpdateCommandParameters(Sender: TObject);
var
  DataCache: TDataCache;
  CacheItem: TCacheItem;
  CommandID: Integer;
  CommandType: TActionType;
begin
  if Assigned(Sender) and (Sender is TDeActionData) then
    DataCache := MetaData.FindActiveDataCache((Sender as TDeActionData).DataSet, True)
  else
    if Assigned(MetaData.MetaTables[idxCommands]) then
      DataCache := MetaData.FindActiveDataCache(MetaData.MetaTables[idxCommands].ID, True)
    else
      DataCache := nil;
  if Assigned(DataCache) then
    begin
      CacheItem := DataCache.FocusedItem;
      if Assigned(CacheItem) then
        begin
          CommandID := CacheItem.ValueByName[fldCommandID];
          CommandType := TActionType(CacheItem.ValueByName[fldCommandType]);
          case CommandType of
            //atGroup: ;
            atStoredProcedure: { Хранимая процедура }
              UpdateCommandStoredProcedureParameters(CommandID, CacheItem.ValueByName[fldCommandProcedure], CacheItem.ValueByName[fldCommandFile]);
            atUpdateValue, { Обновление данных }
            atInsertValue: { Добавление данных }
              UpdateCommandValueParameters(CommandID, CacheItem.ValueByName[fldCommandDataSet]);
            atPreview, atReport: { Отчёт }
              UpdateCommandReportParameters(CommandID, CommandType);
            atShellExecute: { Запуск внешних приложений }
              UpdateCommandShellExecuteParameters(CommandID);
          end;
          DataCache.Update(mcNone, CommandID);
        end;
    end;
end;

procedure TMetaDataOperations.ExecuteSelectCommand(Sender: TObject);
var
  DataCache: TDataCache;
  CacheItem: TCacheItem;
  CommandID: Integer;
  CommandIndex: Integer;
begin
  if Assigned(Sender) and (Sender is TDeActionData) then
    DataCache := MetaData.FindActiveDataCache((Sender as TDeActionData).DataSet, True)
  else
    if Assigned(MetaData.MetaTables[idxCommands]) then
      DataCache := MetaData.FindActiveDataCache(MetaData.MetaTables[idxCommands].ID, True)
    else
      DataCache := nil;
  if Assigned(DataCache) then
    begin
      CacheItem := DataCache.FocusedItem;
      if Assigned(CacheItem) then
        begin
          CommandID := CacheItem.ValueByName[fldCommandID];
          CommandIndex := ProfitActions.IndexByID(CommandID);
          if CommandIndex <> -1 then
            if ProfitActions[CommandIndex].CheckPolicy(spExecute) then
              ProfitActions[CommandIndex].Execute
            else
              SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar('_eRror.right')))
          else
            SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
              NativeInt(PChar(Format(GetTitle('_eRror.ActionNotFound'), [IntToStr(CommandID)]))));
        end;
    end;
end;

procedure TMetaDataOperations.ActivateActions;
var IDDB, IDT, IDS, IDC, Index: Integer;
//    DB: TDeCustomDatabase;
begin
  IDDB:=MetaData.MetaTables[idxBase].ID;
  IDT :=MetaData.MetaTables[idxDataset].ID;
  IDS :=MetaData.MetaTables[idxSolutions].ID;

  ActivateAction('_Da.OpenThisTable', OpenThisTable, IDT);

  ActivateAction('_dA.Consistency', Consistency, IDT, False, 56);

//  ActivateAction('-', nil);
  //Экспорт из выделенных наборов данных в XML файлы
  ActivateAction('_dA.ExportADOXML', TableExportData, IDT, True, 8097979);
  //Импорт в выделенный набор данных из XML файла
  ActivateAction('_dA.ImportADOXML', TableImportData, IDT, False, 8032443);

  ActivateAction('_dA.TablePurgeData', TablePurgeData, IDT, True, 9212091);
  //Удалить все записи в выделенных наборах данных
  ActivateAction('_dA.TableClearData', TableClearData, IDT, True, 259260603);

  ActivateAction('_Da.TestConnect', TestConnection, IDDB, False, 117);
  ActivateAction('_dA.Consistency', Consistency, IDDB, False, 56);
  ActivateAction('_Da.RegTables', RegTables, IDDB, False, 191664315);
  //Экспорт из базы данных в ADOXML файлы
  ActivateAction('_dA.ExportADOXML', DatabaseExportData, IDDB, True, 8097816);
  //Импорт из базы данных в ADOXML файлы
  ActivateAction('_dA.ImportADOXML', DatabaseImportData, IDDB, False, 8032280);
  //Импорт из встроенной метабазы данных в ADOXML файлы
  ActivateAction( '_dA.ImportMetadata', DatabaseImportMetaData, IDDB, True, 8335378);

//  ActivateAction('_Dm.SolExporting', ExportSolution, IDS);
  ActivateAction('_Dm.SolLoading', LoadSolution, IDS, False, 168726650);
  ActivateAction('_Dm.SolUploading', ExtractSolution, IDS, False, 168726651);
  ActivateAction('_Dm.SolDeleting', DeleteSolution, IDS, True, 168726622);

  if Assigned(MetaData.MetaTables[idxCommands]) then
    begin
      IDC := MetaData.MetaTables[idxCommands].ID;
      // Временно отключил, т.к. неизвестно откуда брать текущий CacheItem для выполняемой команды!!!
      Index := ActivateAction('_dM.ExecuteSelectCommand', ExecuteSelectCommand, IDC, False, 97);
      if Index <> -1 then
        FActions[Index].FShortCut := TextToShortCut('F9');

      // Обновление списка параметров для команд отчётов ...
      Index := ActivateAction('_dM.UpdateReportParameters', UpdateCommandParameters, IDC);
      if Index <> -1 then
        begin
          FActions[Index].FEnabledStr := Format('%s = %d', [fldCommandType, Ord(atReport)]) + ' OR ' +
            Format('%s = %d', [fldCommandType, Ord(atPreview)]);
//          DB := MetaData.Databases[MetadataIndex];
//          if Assigned(DB) and DB.CanStoredProcedure and DB.StoredProcedureExists(prcUpdateCommandParams) then
            FActions[Index].FEnabledStr := FActions[Index].FEnabledStr + ' OR ' +
              Format('%s = %d', [fldCommandType, Ord(atShellExecute)]) + ' OR ' +
              Format('%s = %d', [fldCommandType, Ord(atStoredProcedure)]) + ' OR ' +
              Format('%s = %d', [fldCommandType, Ord(atUpdateValue)]) + ' OR ' +
              Format('%s = %d', [fldCommandType, Ord(atInsertValue)]);
        end;
      // Создание ярлыка для команды ...
      ActivateAction('_dA.CreateShortcut', CreateShortLink, IDC, False, 37, acCreate);
    end;
end;

procedure TMetaDataOperations.UpdateCommandStoredProcedureParameters(const CommandID: Integer; const ProcedureName, DatabaseAlias: string);
var
  Database, WorkDatabase: TDeCustomDatabase;
  Strings: TStrings;
  SelectDataset, InsertDataset, MaxDataset: TDeDataset;
  Index, Count: Integer;
  ParameterName: string;
begin
  Database := MetaData.MetadataDB;
  if Assigned(Database) then
    try
      Count := 0;
      Strings := TStringList.Create;
      try
        WorkDatabase := MetaData.DatabaseByAlias(Trim(DatabaseAlias));
        if Assigned(WorkDatabase) then
          begin
            if not WorkDatabase.RetrieveProcedureParameters(ProcedureName, Strings) then
              Strings.Clear;
          end
        else
          if not Database.RetrieveProcedureParameters(ProcedureName, Strings) then
            Strings.Clear;
        if Strings.Count <> 0 then
          begin
            SelectDataset := Database.CreateQuery(qtSelect);
            try
              SelectDataset.Descr.BeginUpdate;
              try
                SelectDataset.Descr.Table := tblCommandParams;
                SelectDataset.Descr.AddField(fldCommandParamID);
                SelectDataset.Descr.AddParamCondition(fldCommandParamCommand, ftInteger, opEQ, 'ID', CommandID);
                SelectDataset.Descr.AddParamCondition(fldCommandParamName, ftString, opLike, fldCommandParamName);
                SelectDataset.Descr.AddOperation(opAnd);
              finally
                SelectDataset.Descr.EndUpdate;
              end;
              InsertDataset := Database.CreateQuery(qtInsert);
              try
                InsertDataset.Descr.BeginUpdate;
                try
                  InsertDataset.Descr.Table := tblCommandParams;
                  InsertDataset.Descr.AddParamField(fldCommandParamID, ftInteger);
                  InsertDataset.Descr.AddParamField(fldCommandParamCommand, ftInteger);
                  InsertDataset.Descr.AddParamField(fldCommandParamType, ftInteger);
                  InsertDataset.Descr.AddParamField(fldCommandParamName, ftString);
                  InsertDataset.Descr.AddParamField(fldCommandParamCaption, ftString);
                  InsertDataset.Descr.AddParamField(fldCommandParamValue, ftString);
                  InsertDataset.Descr.AddParamField(fldCommandParamHidden, ftInteger);
                  InsertDataset.Descr.AddParamField(fldCommandParamColumn, ftInteger);
                  if mpCommandParamOrders in Metadata.MetadataPresents then
                    InsertDataset.Descr.AddParamField(fldCommandParamOrder, ftInteger);
                finally
                  InsertDataset.Descr.EndUpdate;
                end;
                MaxDataset := Database.CreateQuery(qtHole);
                try
                  if not Assigned(MaxDataset) then MaxDataset := Database.CreateQuery(qtSelect);
                  MaxDataset.Descr.BeginUpdate;
                  try
                    MaxDataset.Descr.Table := tblCommandParams;
                    MaxDataset.Descr.AddField(opMax, fldCommandParamID);
                  finally
                    MaxDataset.Descr.EndUpdate;
                  end;
                  for Index := 0 to Pred(Strings.Count) do
                    begin
                      ParameterName := Strings[Index];
                      if Pos('@', ParameterName) = 1 then Delete(ParameterName, 1, 1);
                      SelectDataset.Descr.ParamValueByName[fldCommandParamName] := ParameterName;
                      SelectDataset.Open;
                      try
                        if VarIsNull(SelectDataset.Value[0]) then
                          begin
                            InsertDataset.Descr.ParamValueByName[fldCommandParamCommand] := CommandID;
                            InsertDataset.Descr.ParamValueByName[fldCommandParamHidden] := 1;
                            InsertDataset.Descr.ParamValueByName[fldCommandParamType] := Integer(Strings.Objects[Index]);
                            InsertDataset.Descr.ParamValueByName[fldCommandParamName] := ParameterName;
                            InsertDataset.Descr.ParamValueByName[fldCommandParamCaption] := Null;

                            if mpCommandParamOrders in Metadata.MetadataPresents then
                              InsertDataset.Descr.ParamValueByName[fldCommandParamOrder] := Index;

                            case Integer(Strings.Objects[Index]) of
                              7: { Date }
                                begin
                                  InsertDataset.Descr.ParamValueByName[fldCommandParamValue] := 'TODAY';
                                  InsertDataset.Descr.ParamValueByName[fldCommandParamColumn] := 2;
                                  if SameText(Strings[Index], 'dDate') then
                                    InsertDataset.Descr.ParamValueByName[fldCommandParamCaption] := '_df.date';
                                end;
                              10: { Time }
                                begin
                                  InsertDataset.Descr.ParamValueByName[fldCommandParamValue] := 'TIME';
                                  InsertDataset.Descr.ParamValueByName[fldCommandParamColumn] := 2;
                                end;
                            else
                              InsertDataset.Descr.ParamValueByName[fldCommandParamValue] := Null;
                              InsertDataset.Descr.ParamValueByName[fldCommandParamColumn] := 1;
                            end;
                            MaxDataset.Open;
                            try
                              InsertDataset.Descr.ParamValueByName[fldCommandParamID] := MaxDataset.Value[0] + 1;
                            finally
                              MaxDataset.Close;
                            end;
                            if InsertDataset.ExecuteQuery then
                              Inc(Count);
                          end;
                      finally
                        SelectDataset.Close;
                      end;
                    end;
                finally
                  MaxDataset.Free;
                end;
              finally
                InsertDataset.Free;
              end;
            finally
              SelectDataset.Free;
            end;
          end;
      finally
        Strings.Free;
      end;
      if AppendCommandParameter(CommandID, Ord(etInteger), 1, varActionTimeout, '30') then
        Inc(Count);
      // v. 18.4 +
      if AppendCommandParameter(CommandID, Ord(etInteger), 1, varActionResult, '0') then
        Inc(Count);
      // v. 18.4 -
      if Count <> 0 then
        SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar('_dL.procedureok')));
    except
      on E: Exception do
        begin
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(
            PChar('_dL.procedureerror'#13#10 + E.Message)));
          {$IFDEF DEBUG}
          DebugLog('TMetaDataOperations.UpdateCommandStoredProcedureParameters(%d, %s, %s) error: %s', [CommandID,
            QuotedStr(ProcedureName), QuotedStr(DatabaseAlias), E.Message]);
          {$ENDIF}
        end;
    end;
end;

procedure TMetaDataOperations.UpdateCommandShellExecuteParameters(const CommandID: Integer);
begin
  AppendCommandParameter(CommandID, Ord(etInteger), 1, varActionResult, '0');
  AppendCommandParameter(CommandID, Ord(etInteger), 1, varActionShowWindow, 'Null');
end;

procedure TMetaDataOperations.UpdateCommandReportParameters(const CommandID: Integer; const ActionType: TActionType);
var
  Report: TDeReport;
  CodePage: Integer;
begin
  Report := PrepareTempReport(CommandID);
  try
    if Assigned(Report) then
      begin
        ClearReportParameters(CommandID);
        if Report is TDeXMLReport then
          CodePage := (Report as TDeXMLReport).CodePage
        else
          CodePage := 0;
        SaveReportParameters(CommandID, CodePage, Report.ParamDefs);
        // v. 18.4 +
        AppendCommandParameter(CommandID, Ord(etCheckBox), 1, varButtonPrint, '1');
        AppendCommandParameter(CommandID, Ord(etCheckBox), 1, varButtonPreview, '1');
        AppendCommandParameter(CommandID, Ord(etCheckBox), 1, varButtonExport, '0');
        // v. 18.4 -
        if ActionType = atPreview then
          AppendCommandParameter(CommandID, Ord(etDefault), 1, varPrinterName, 'NULL')
        else
          AppendCommandParameter(CommandID, Ord(etDefault), 1, varPrinterName, '__DEFAULT_PRINTER_NAME');
      end;
  finally
    Report.Free;
  end;
end;

procedure TMetaDataOperations.UpdateCommandValueParameters(const CommandID, TableID: Integer);
var
  Database: TDeCustomDatabase;
  TableMeta: TTableMeta;
  SelectDataset, InsertDataset, MaxDataset: TDeDataset;
  Index, Count: Integer;
  Caption: string;
begin
  Database := MetaData.MetadataDB;
  if Assigned(Database) then
    try
      TableMeta := MetaData.GetTableMeta(TableID);
      Count := 0;
      if Assigned(TableMeta) then
        begin
          SelectDataset := Database.CreateQuery(qtSelect);
          try
            SelectDataset.Descr.BeginUpdate;
            try
              SelectDataset.Descr.Table := tblCommandParams;
              SelectDataset.Descr.AddField(fldCommandParamID);
              SelectDataset.Descr.AddParamCondition(fldCommandParamCommand, ftInteger, opEQ, 'ID', CommandID);
              SelectDataset.Descr.AddParamCondition(fldCommandParamName, ftString, opLike, fldCommandParamName);
              SelectDataset.Descr.AddOperation(opAnd);
            finally
              SelectDataset.Descr.EndUpdate;
            end;
            InsertDataset := Database.CreateQuery(qtInsert);
            try
              InsertDataset.Descr.BeginUpdate;
              try
                InsertDataset.Descr.Table := tblCommandParams;
                InsertDataset.Descr.AddParamField(fldCommandParamID, ftInteger);
                InsertDataset.Descr.AddParamField(fldCommandParamCommand, ftInteger);
                InsertDataset.Descr.AddParamField(fldCommandParamType, ftInteger);
                InsertDataset.Descr.AddParamField(fldCommandParamDataSet, ftInteger);
                InsertDataset.Descr.AddParamField(fldCommandParamName, ftString);
                InsertDataset.Descr.AddParamField(fldCommandParamCaption, ftString);
                InsertDataset.Descr.AddParamField(fldCommandParamValue, ftString);
                InsertDataset.Descr.AddParamField(fldCommandParamHidden, ftInteger);
                InsertDataset.Descr.AddParamField(fldCommandParamColumn, ftInteger);
                if mpCommandParamOrders in Metadata.MetadataPresents then
                  InsertDataset.Descr.AddParamField(fldCommandParamOrder, ftInteger);
              finally
                InsertDataset.Descr.EndUpdate;
              end;
              MaxDataset := Database.CreateQuery(qtSelect);
              try
                MaxDataset.Descr.BeginUpdate;
                try
                  MaxDataset.Descr.Table := tblCommandParams;
                  MaxDataset.Descr.AddField(opMax, fldCommandParamID);
                finally
                  MaxDataset.Descr.EndUpdate;
                end;
                for Index := 0 to Pred(TableMeta.Fields.Count) do
                  if TableMeta.Fields[Index].IsStored and not TableMeta.Fields[Index].Key then
                    begin
                      SelectDataset.Descr.ParamValueByName[fldCommandParamName] := TableMeta.Fields[Index].Original;
                      SelectDataset.Open;
                      try
                        if VarIsNull(SelectDataset.Value[0]) then
                          begin
                            InsertDataset.Descr.ParamValueByName[fldCommandParamCommand] := CommandID;
                            case TableMeta.Fields[Index].VisibleLevel of
                              fvLevel2, fvLevel3: { Отображаемые }
                                if TableMeta.Fields[Index].IsReadOnly then
                                  InsertDataset.Descr.ParamValueByName[fldCommandParamHidden] := 2
                                else
                                  InsertDataset.Descr.ParamValueByName[fldCommandParamHidden] := 0;
                            else
                              InsertDataset.Descr.ParamValueByName[fldCommandParamHidden] := 1;
                            end;
                            case TableMeta.Fields[Index].DataType of
                              ftDate, ftDateTime, ftTimeStamp, ftTimeStampOffset, ftOraTimeStamp : { Date }
                                begin
                                  InsertDataset.Descr.ParamValueByName[fldCommandParamType] := Ord(etDateTime);
                                  InsertDataset.Descr.ParamValueByName[fldCommandParamColumn] := 2;
                                  InsertDataset.Descr.ParamValueByName[fldCommandParamValue] := 'TODAY';
                                end;
                              ftTime: { Time }
                                begin
                                  InsertDataset.Descr.ParamValueByName[fldCommandParamType] := Ord(etTime);
                                  InsertDataset.Descr.ParamValueByName[fldCommandParamColumn] := 2;
                                  InsertDataset.Descr.ParamValueByName[fldCommandParamValue] := 'TIME';
                                end;
                              else
                                begin
                                  InsertDataset.Descr.ParamValueByName[fldCommandParamType] := Ord(etDefault);
                                  InsertDataset.Descr.ParamValueByName[fldCommandParamColumn] := 1;
                                  InsertDataset.Descr.ParamValueByName[fldCommandParamValue] := Null;
                                end;
                            end;
                            if Length(Trim(TableMeta.Fields[Index].DefaultValue)) <> 0 then
                              InsertDataset.Descr.ParamValueByName[fldCommandParamValue] := TableMeta.Fields[Index].DefaultValue;
                            InsertDataset.Descr.ParamValueByName[fldCommandParamName] := TableMeta.Fields[Index].Original;
                            Caption := TableMeta.Fields[Index].Name;
                            InsertDataset.Descr.ParamValueByName[fldCommandParamCaption] := Caption;
                            if TableMeta.Fields[Index].Link = 0 then
                              InsertDataset.Descr.ParamValueByName[fldCommandParamDataSet] := Null
                            else
                              InsertDataset.Descr.ParamValueByName[fldCommandParamDataSet] := TableMeta.Fields[Index].Link;
                            if mpCommandParamOrders in Metadata.MetadataPresents then
                              InsertDataset.Descr.ParamValueByName[fldCommandParamOrder] := Index;
                            MaxDataset.Open;
                            try
                              InsertDataset.Descr.ParamValueByName[fldCommandParamID] := MaxDataset.Value[0] + 1;
                            finally
                              MaxDataset.Close;
                            end;
                            if InsertDataset.ExecuteQuery then
                              Inc(Count);
                          end;
                      finally
                        SelectDataset.Close;
                      end;
                    end;
              finally
                MaxDataset.Free;
              end;
            finally
              InsertDataset.Free;
            end;
          finally
            SelectDataset.Free;
          end;
        end
      else
        raise Exception.CreateFmt('Metadata for table %u not found', [TableID]);
      if Count <> 0 then
        begin
          MetaData.UpdateLibrary(TableMeta.ID);
          SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar('_dL.procedureok')));
        end;
    except
      on E: Exception do
        begin
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(
            PChar('_dL.procedureerror'#13#10 + E.Message)));
          {$IFDEF DEBUG}
          DebugLog('TMetaDataOperations.UpdateCommandValueParameters(%d, %d) error: %s', [CommandID, TableID, E.Message]);
          {$ENDIF}
        end;
    end;
end;

function TMetaDataOperations.AppendCommandParameter(const CommandID, ParamTypeID, ParamHiddenID: Integer; const ParamName, ParamValue: string; const ParamColumn: Integer): Boolean;
var
  Database: TDeCustomDatabase;
  SelectDataset, InsertDataset, OrderDataset: TDeDataset;
begin
  Result := False;
  Database := MetaData.MetadataDB;
  if Assigned(Database) then
    try
      SelectDataset := Database.CreateQuery(qtSelect);
      try
        SelectDataset.Descr.BeginUpdate;
        try
          SelectDataset.Descr.Table := tblCommandParams;
          SelectDataset.Descr.AddField(fldCommandParamID);
          SelectDataset.Descr.AddParamCondition(fldCommandParamCommand, ftInteger, opEQ, 'ID', CommandID);
          SelectDataset.Descr.AddParamCondition(fldCommandParamName, ftString, opLike, fldCommandParamName, ParamName);
          SelectDataset.Descr.AddOperation(opAnd);
        finally
          SelectDataset.Descr.EndUpdate;
        end;
        SelectDataset.Open;
        try
          Result := VarIsNull(SelectDataset.Value[0]);
        finally
          SelectDataset.Close;
        end;
      finally
        SelectDataset.Free;
      end;
      if Result then
        begin
          SelectDataset := Database.CreateQuery(qtHole);
          try
            if not Assigned(SelectDataset) then SelectDataset := Database.CreateQuery(qtSelect);
            SelectDataset.Descr.BeginUpdate;
            try
              SelectDataset.Descr.Table := tblCommandParams;
              SelectDataset.Descr.AddField(opMax, fldCommandParamID);
            finally
              SelectDataset.Descr.EndUpdate;
            end;
            InsertDataset := Database.CreateQuery(qtInsert);
            try
              InsertDataset.Descr.BeginUpdate;
              try
                InsertDataset.Descr.Table := tblCommandParams;
                InsertDataset.Descr.AddParamField(fldCommandParamID, ftInteger);
                InsertDataset.Descr.AddParamField(fldCommandParamCommand, ftInteger);
                InsertDataset.Descr.AddParamField(fldCommandParamType, ftInteger);
                InsertDataset.Descr.AddParamField(fldCommandParamName, ftString);
                InsertDataset.Descr.AddParamField(fldCommandParamValue, ftString);
                InsertDataset.Descr.AddParamField(fldCommandParamHidden, ftInteger);
                InsertDataset.Descr.AddParamField(fldCommandParamColumn, ftInteger);
                if mpCommandParamOrders in Metadata.MetadataPresents then
                  InsertDataset.Descr.AddParamField(fldCommandParamOrder, ftInteger);
              finally
                InsertDataset.Descr.EndUpdate;
              end;
              InsertDataset.Descr.ParamValueByName[fldCommandParamCommand] := CommandID;
              InsertDataset.Descr.ParamValueByName[fldCommandParamType] := ParamTypeID;
              InsertDataset.Descr.ParamValueByName[fldCommandParamName] := ParamName;
              InsertDataset.Descr.ParamValueByName[fldCommandParamValue] := ParamValue;
              InsertDataset.Descr.ParamValueByName[fldCommandParamHidden] := ParamHiddenID;
              InsertDataset.Descr.ParamValueByName[fldCommandParamColumn] := ParamColumn;
              if mpCommandParamOrders in Metadata.MetadataPresents then
                begin
                  OrderDataset := Database.CreateQuery(qtSelect);
                  try
                    OrderDataset.Descr.BeginUpdate;
                    try
                      OrderDataset.Descr.Table := tblCommandParams;
                      OrderDataset.Descr.AddField(opMax, fldCommandParamOrder);
                      OrderDataset.Descr.AddParamCondition(fldCommandParamCommand, ftInteger, opEQ, 'ID', CommandID);
                    finally
                      OrderDataset.Descr.EndUpdate;
                    end;
                    OrderDataset.Open;
                    InsertDataset.Descr.ParamValueByName[fldCommandParamOrder] := OrderDataset.IntValueDef(0) + 1;
                  finally
                    OrderDataset.Free;
                  end;
                end;
              SelectDataset.Open;
              try
                InsertDataset.Descr.ParamValueByName[fldCommandParamID] := SelectDataset.Value[0] + 1;
              finally
                SelectDataset.Close;
              end;
              Result := InsertDataset.ExecuteQuery;
            finally
              InsertDataset.Free;
            end;
          finally
            SelectDataset.Free;
          end;
        end;
    except
      on E: Exception do
        begin
          Result := False;
          {$IFDEF DEBUG}
          DebugLog('TMetaDataOperations.AppendCommandParameter error: %s', [E.Message]);
          {$ENDIF}
        end;
    end;
end;

procedure TMetaDataOperations.ExtractSolution(Sender: TObject);
var ADataCache: TDataCache;
    TM: TTableMeta;
    i, N, M, SolutionID: Integer;
    Value: WideString;
    Dialog: TSaveDialog;
    ZipFile: TZipFile;
    Buffer: TBytes;
    TF: TFieldMeta;
    Parser: TDeParser;

  procedure ExtractCache(aProgress: Integer; aTableMeta: TTableMeta; aFilter: String = ''; // aSort: String = '';
                         aFlags: TDataFlags = [dfStoredFields, dfAllRecords]);
  var eDataCache: TDataCache;
      eParser: TDeParser;
      eFilterItem: TFilterItem;
  begin
    eDataCache:= TDataCache.Create(aTableMeta);
    WriteLog(aTableMeta.Table+' b', 'ExtractCache');
    if 0 < Length(aFilter) then
      begin
        eFilterItem:= TFilterItem.Create;
        eParser:= TDeParser.Create;
        eParser.Table:= aTableMeta;
        try
          eParser.Parse(aFilter, eFilterItem);
        except
          on E: EDeParserError do
            Raise Exception.Create('Filter Error in ExtractSolution: ' + E.Message);
        end;
        eDataCache.LoadData(eFilterItem, fsMax);
        eParser.Free;
      end
    else
      begin
        eDataCache.LoadData(nil, fsMax);
      end;

    Buffer:= TEncoding.UTF8.GetBytes( eDataCache.GetAsXML(aFlags) );
    if 0 < Length(Buffer) then
      ZipFile.Add(Buffer, aTableMeta.Table + sExtensionXML);

    eDataCache.Free;
    WriteLog(aTableMeta.Table+' e', 'ExtractCache');
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, aProgress, 0);
  end;

begin
  if Not Assigned(MetaData.MetaTables[idxSolutions]) then Exit;
  ADataCache:= MetaData.FindActiveDataCache(MetaData.MetaTables[idxSolutions].ID, True);

  if not Assigned(ADataCache) then Exit;
  if Not Assigned(ADataCache.FocusedItem) then Exit;

  UpdateWindow(Application.MainForm.Handle);

  ZipFile := TZipFile.Create;
  try
    Dialog := TSaveDialog.Create(nil);
    try
      Dialog.DefaultExt := sExtensionZIP;//sExtensionXMLX;
      Dialog.Filter := 'DBCO XML Files|*.zip'; //'DBCO XML Files|*.xmlx';
      Dialog.InitialDir := Variables.AsString[RegDirPath];
      Dialog.FileName := ADataCache.FocusedItem.ValueByName[fldConfName];
      Dialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableIncludeNotify, ofEnableSizing, ofDontAddToRecent];
      if Dialog.Execute then
        ZipFile.Open(Dialog.FileName, zmWrite);
    finally
      Dialog.Free;
    end;

    if ZipFile.Mode = zmClosed then Exit;

    // добавляем значение в список доступных переменных в списке глобальных параметров
    SolutionID:= ADataCache.FocusedItem.ID;
    MetaData.Parameters.SetParam('ExtractID', SolutionID);

    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, 18, 1);

    ExtractCache( 0, Metadata.MetaTables[idxSolutions], fldConfID + ' = ExtractID');
    ExtractCache( 0, Metadata.MetaTables[idxLang]);
    ExtractCache( 0, Metadata.MetaTables[idxBase], fldBaseId + ' in select('''+Metadata.MetaTables[idxDataset].Table+''','''+fldDataSetDatabase+''','''+fldDataSetSolution+'= '+IntToStr(SolutionID)+''')' );

    ExtractCache( 1, Metadata.MetaTables[idxUsers]);
    ExtractCache( 2, Metadata.MetaTables[idxDataset], '('+fldDataSetSolution + ' = ExtractID) or ('+fldDataSetParent+'.'+fldDataSetSolution + ' = ExtractID)');
    ExtractCache( 3, Metadata.MetaTables[idxFields], fldFieldsTable+'.'+fldDataSetSolution + ' = ExtractID');

    // Меню выгружаем рекурсивно "с родителями" помечая нужные записи свойством Selected ...............................
    ADataCache:=  TDataCache.Create(Metadata.MetaTables[idxMenu]);
    ADataCache.LoadData(nil, fsMax);
    ADataCache.ClearSelection;
    for i:=0 to Pred(ADataCache.Count) do
      begin
        M:= i;
        N:= ADataCache[i].ValueByName[fldMenuDataSet];
        TM:= MetaData.GetTableMeta(N);
        if assigned(TM) and (TM.OwnerTable.SolutionID = SolutionID) then
          while (-1 < M) do
            begin
              ADataCache[M].Selected:= True;
              M:= ADataCache.IndexByID(ADataCache[M].ValueByName[fldMenuOwner]);
            end;
      end;
    Buffer:= TEncoding.UTF8.GetBytes( ADataCache.GetAsXML([dfStoredFields, dfSelectedRecords]) );
    if 0 < Length(Buffer) then
      ZipFile.Add(Buffer, Metadata.MetaTables[idxMenu].Table + sExtensionXML);

    ExtractCache( 5, Metadata.MetaTables[idxElement], fldElemDataSet+'.'+fldDataSetSolution + ' = ExtractID');
    ExtractCache( 6, Metadata.MetaTables[idxUserDataset], '('+fldUDataSetTable+'.'+fldDataSetSolution + ' = ExtractID) or ('+fldUDataSetTable+' is null)');
    ExtractCache( 7, Metadata.MetaTables[idxParameters]);

    // Исключение для выгрузки базового решения - решения о самой метаструктуре
    if 1 < SolutionID then
      Metadata.MetaTables[idxRights].AddCalcField(fldRightsObjectType+'.'+fldDataSetTable, fldRightsObjectType+'.'+fldDataSetTable );
    ExtractCache( 8, Metadata.MetaTables[idxRights], EmptyStr, [dfStoredFields, dfAllRecords, dfCalculatedFields]);

    ExtractCache( 9, Metadata.MetaTables[idxMembership]);
    ExtractCache(10, Metadata.MetaTables[idxTasks], '('+ fldTasksTableID+'.'+fldDataSetSolution + ' = ExtractID) or ('+fldTasksTableID+' is null)');
    ExtractCache(11, Metadata.MetaTables[idxUserTasks], '('+ fldUTTaskID+'.'+fldTasksTableID+'.'+fldDataSetSolution + ' = ExtractID) or ('+fldUTTaskID+'.'+fldTasksTableID+' is null)');
    ExtractCache(12, Metadata.MetaTables[idxInterRelations], '('+ fldIRParentTable+'.'+fldDataSetSolution + ' = ExtractID) or ('+ fldIRChildTable+'.'+fldDataSetSolution + ' = ExtractID)');
    ExtractCache(13, Metadata.MetaTables[idxNotes], fldNotesTable+'.'+fldDataSetSolution + ' = ExtractID');
    ExtractCache(14, Metadata.MetaTables[idxActionConditions], '('+ fldACTableMetaID+'.'+fldDataSetSolution + ' = ExtractID) or ('+fldACTableMetaID+' is null)');

    if mpDatabaseNewConstraints in MetaData.MetadataPresents then
      begin
        ExtractCache(15, Metadata.MetaTables[idxConstraints], fldConstraintsDataset+'.'+fldDataSetSolution + ' = ExtractID');
      end
    else
      begin
        TF:= Metadata.MetaTables[idxConstraints].AddCalcField(fldConstraintsID, fldID);
        TF:= Metadata.MetaTables[idxConstraints].AddCalcField(fldConstraintsDataset, fldCSDataset);
        TF:= Metadata.MetaTables[idxConstraints].AddCalcField(fldConstraintsField, fldCSErrorField);
        TF:= Metadata.MetaTables[idxConstraints].AddCalcField(fldConstraintsAction, fldCSAction);
        TF:= Metadata.MetaTables[idxConstraints].AddCalcField(fldConstraintsState, fldCSOffState);
        TF:= Metadata.MetaTables[idxConstraints].AddCalcField(fldConstraintsCases, fldCSCases);
        TF:= Metadata.MetaTables[idxConstraints].AddCalcField(fldConstraintsMessage, fldCSMessage);
        TF:= Metadata.MetaTables[idxConstraints].AddCalcField(fldConstraintsExpression, fldCSExpression);
        TF:= Metadata.MetaTables[idxConstraints].AddCalcField(fldConstraintsDeleted, fldCSDeleted);
        ExtractCache(15, Metadata.MetaTables[idxConstraints], fldCSDataset+'.'+fldDataSetSolution + ' = ExtractID',
                                                                    [dfStoredFields, dfAllRecords, dfCalculatedFields]);
      end;

    ExtractCache(16, Metadata.MetaTables[idxCommands], fldCommandSolution + ' = ExtractID');
    ExtractCache(17, Metadata.MetaTables[idxCommandParams], fldCommandParamCommand+'.'+fldCommandSolution + ' = ExtractID');
    ExtractCache(18, Metadata.MetaTables[idxDictionary]);

    ZipFile.Close;
    ZipFile.Free;

    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  finally
  end;
end;

procedure TMetaDataOperations.DeleteSolution(Sender: TObject);
var i: Integer;
    TMeta: TTableMeta;
    aParser: TDeParser;
    FilterItem: TFilterItem;
    Q: TDeDataset;
    ACacheItem: TCacheItem;
    ADataCache, DataCache: TDataCache;
begin
  if Assigned(MetaData.MetaTables[idxSolutions]) then
    ADataCache:= MetaData.FindActiveDataCache(MetaData.MetaTables[idxSolutions].ID, True)
  else
    ADataCache:= nil;
  if not Assigned(ADataCache) then Exit;

  ACacheItem := ADataCache.FocusedItem;
  if Not Assigned(ACacheItem) then Exit;

  UpdateWindow(Application.MainForm.Handle);

  if Application.MessageBox( pChar(GetTitle('_Dm.querydelete') + #10 + GetTitle('_dt.solution')+ ' "' +
       ACacheItem.Caption+'"'), pChar(GetTitle('_Dl.Confirmation')), MB_ICONEXCLAMATION or MB_YESNO) <> idYes then Exit;

  //--- удаляем метаданные данные решения
  MetaData.Parameters.SetParam('ExportID', ACacheItem.ID);
  FilterItem:= TFilterItem.Create;
  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Length(Metadata.MetaTables), 1);

  for i := High(Metadata.MetaTables) downto Low(Metadata.MetaTables) do
    if Assigned(Metadata.MetaTables[i]) then
      begin
        TMeta:= Metadata.MetaTables[i];

        DataCache := TDataCache.Create(TMeta);
        aParser:= TDeParser.Create;
        try
          aParser.Table:= TMeta;
          FilterItem:= TFilterItem.Create;
          try
            case i of
              idxSolutions:        aParser.Parse(     fldConfID + ' = ExportID', FilterItem);
              idxUsers:            aParser.Parse(     '0 = 1', FilterItem);
              idxBase:             aParser.Parse(     '0 = 1', FilterItem); // fldBaseId + ' in select('''+Metadata.MetaTables[idxDataset].Table+''','''+fldDataSetDatabase+''','''+fldDataSetDatabase+'= '+IntToStr(ACacheItem.ID)+''')', FilterItem);
              idxDataset:          aParser.Parse(     fldDataSetSolution + ' = ExportID', FilterItem);
              idxFields:           aParser.Parse(     fldFieldsTable+'.'+fldDataSetSolution + ' = ExportID', FilterItem);
              idxMenu:             aParser.Parse( '('+fldMenuDataSet+'.'+fldDataSetSolution + ' = ExportID)', FilterItem);
              idxElement:          aParser.Parse(     fldElemDataSet+'.'+fldDataSetSolution + ' = ExportID', FilterItem);
              idxUserDataset:      aParser.Parse( '('+fldUDataSetTable+'.'+fldDataSetSolution + ' = ExportID)', FilterItem);
              idxParameters:       aParser.Parse(     '0 = 1', FilterItem);
              idxRights:           aParser.Parse(     '0 = 1', FilterItem);
              idxMembership:       aParser.Parse(     '0 = 1', FilterItem);
              idxTasks:            aParser.Parse( '('+ fldTasksTableID+'.'+fldDataSetSolution + ' = ExportID)', FilterItem);
              idxUserTasks:        aParser.Parse( '('+ fldUTTaskID+'.'+fldTasksTableID+'.'+fldDataSetSolution + ' = ExportID)', FilterItem);
              idxInterRelations:   aParser.Parse( '('+ fldIRParentTable+'.'+fldDataSetSolution + ' = ExportID) or ('+ fldIRChildTable+'.'+fldDataSetSolution + ' = ExportID)', FilterItem);
              idxNotes:            aParser.Parse(     fldNotesTable+'.'+fldDataSetSolution + ' = ExportID', FilterItem);
              idxActionConditions: aParser.Parse( '('+ fldACTableMetaID+'.'+fldDataSetSolution + ' = ExportID)', FilterItem);
              idxConstraints:
                            if mpDatabaseNewConstraints in MetaData.MetadataPresents
                              then aParser.Parse(     fldConstraintsDataset+'.'+fldDataSetSolution + ' = ExportID', FilterItem)
                              else aParser.Parse(     fldCSDataset+'.'+fldDataSetSolution + ' = ExportID', FilterItem);
              idxCommands:         aParser.Parse(     fldCommandSolution + ' = ExportID', FilterItem);
              idxCommandParams:    aParser.Parse(     fldCommandParamCommand+'.'+fldCommandSolution + ' = ExportID', FilterItem);
              idxLang:             aParser.Parse(     '0 = 1', FilterItem);
              idxDictionary:       aParser.Parse(     '0 = 1', FilterItem);
            end;
          except
            on E: EDeParserError do
              Raise Exception.Create('Filter Error in DeleteSolution: '+E.Message);
          end;

          if 0 < FilterItem.Count then
            begin
              Q:= TMeta.Database.CreateQuery(qtDelete);
              Q.Descr.BeginUpdate;
              Q.Descr.TableMeta:= TMeta;
              Q.Descr.AssignFilter(FilterItem);
              Q.Descr.EndUpdate;
              Q.ExecuteQuery;
            end;
        finally
          aParser.Free;
        end;

        DataCache.Free;
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, High(Metadata.MetaTables)-i, 0);
      end;

  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
end;

procedure TMetaDataOperations.LoadSolution(Sender: TObject);
var i,j,N: Integer;
    CacheList: TObjectList<TDataCache>;
    DataCache: TDataCache;
    SourceDB, TargetDB: TDeCustomDatabase;
    OpenDialog: TOpenDialog;
    aTableList: TTablesList;
    TM: TTableMeta;
begin
  UpdateWindow(Application.MainForm.Handle);

  TargetDB:= Metadata.MetadataDB;

  if assigned(TargetDB) then
    if TargetDB.CheckConnection then
      begin
        OpenDialog := TOpenDialog.Create(Application);
        try
          OpenDialog.Title := GetTitle('_Dl.Location');
          OpenDialog.InitialDir := Variables.AsString[RegDirPath];
          OpenDialog.Filter := 'All Files|*.*';//sADXFilter;
          OpenDialog.DefaultExt := sExtensionXMLX;
          if OpenDialog.Execute then
            try
              Metadata.BeginUpdate;
              SourceDB:= TDBCOXMLDatabase.Create(nil);
              SourceDB.Server:= EmptyStr;
              SourceDB.Database:= OpenDialog.FileName;

              SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, aTableList.Count, 1);
              CacheList:= TObjectList<TDataCache>.Create(True);
              try
                SourceDB.Connected:= True;
                aTableList:= TTablesList.Create;
                SourceDB.RetrieveMetaTables(aTableList);
                for j:=0 to Pred(aTableList.Count) do
                  SourceDB.RetrieveMetaTableInfo(aTableList[j]);
                { нужно сделать проверку наличия основных полей, ну или хотя бы одного поля}
                //----------------------------------------------------------------------------------------------------
                for i:=Low(MetaTableNames) to High(MetaTableNames) do
                  for j:=0 to Pred(aTableList.Count) do
                    if SameText(MetaTableNames[i], aTableList[j].Table) then
                      begin
                        DataCache:= TDataCache.Create(MetaData.MetaTables[i]);
                        DataCache.FillFromXML( TDBCOXMLDatabase(SourceDB).Get_XMLTable(MetaData.MetaTables[i].Table) );
                        CacheList.Add(DataCache);
                        Break;
                      end;
              finally
                SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
              end;

              DoImportData(CacheList);
            finally;
              SourceDB.Free;
              CacheList.Free;
              Metadata.EndUpdate;
            end;
        finally
          OpenDialog.Free;
        end;
      end;
end;

procedure TMetaDataOperations.DatabaseExportData(Sender: TObject);
var
  aTableList: TTablesList;
  DataCacheDB, DataCache: TDataCache;
  CacheItemDB: TCacheItem;
  SourceDB: TDeCustomDatabase;

  Dialog: TSaveDialog;
  ZipFile: TZipFile;

  i: Integer;
  Value: WideString;
  Buffer: TBytes;
begin
  // получем Базу (TDeCustomDatabase) и её описание (TCacheItem)
  DataCacheDB:= nil;
  if Assigned(Sender) and (Sender is TDeActionData) then
    DataCacheDB := MetaData.FindActiveDataCache((Sender as TDeActionData).DataSet, True) else
  if Assigned(MetaData.MetaTables[idxBase]) then
    DataCacheDB := MetaData.FindActiveDataCache(MetaData.MetaTables[idxBase].ID, True);
  if Not Assigned(DataCacheDB) then Exit;

  CacheItemDB := DataCacheDB.FocusedItem;
  if Not Assigned(CacheItemDB) then Exit;

  UpdateWindow(Application.MainForm.Handle);

  if CacheItemDB.ID = 0
    then SourceDB:= MetaData.Config__DB
    else SourceDB:= MetaData.DatabaseByGUID(CacheItemDB.ValueByName[fldBaseGUID]);

  if not Assigned(SourceDB) then
   SourceDB:= MetaData.DatabaseByID(CacheItemDB.ID);

  if not assigned(SourceDB) then
    SourceDB:= MetaData.CreateDatabase(CacheItemDB, True);

  try
    SourceDB.Connected:= True;
  except
    Exit;
  end;

  ZipFile := TZipFile.Create;
  try
    Dialog := TSaveDialog.Create(nil);
    try
      Dialog.DefaultExt := sExtensionXMLX;
      Dialog.Filter := 'DBCO XML Files|*.xmlx';
      Dialog.InitialDir := Variables.AsString[RegDirPath];
      Dialog.FileName := SourceDB.Database;
      Dialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableIncludeNotify, ofEnableSizing, ofDontAddToRecent];
      if Dialog.Execute then
        ZipFile.Open(Dialog.FileName, zmWrite);
    finally
      Dialog.Free;
    end;

    if ZipFile.Mode = zmClosed then Exit;

    aTableList:= TTablesList.Create;
    SourceDB.RetrieveMetaTables(aTableList);

    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, aTableList.Count, 1);
    for i:=0 to Pred(aTableList.Count) do
      if Not aTableList[i].IsReadOnly then
        try
          WriteLog(IntToStr(i)+' '+aTableList[i].Table, 'export');
          SourceDB.GetMetaTableInfo(aTableList[i]);
          DataCache := TDataCache.Create(aTableList[i]);
          DataCache.PrepareData(True, fsFull);
          DataCache.OpenData;

          Value:= DataCache.GetAsXML([dfStoredFields, dfAllRecords]);

          Buffer:= TEncoding.UTF8.GetBytes(Value);
          if 0 < Length(Buffer) then
            ZipFile.Add(Buffer, aTableList[i].Table + sExtensionXML);

          DataCache.Free;
        finally
          SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, i, 0);
        end
      else
        WriteLog(IntToStr(i)+' skip '+aTableList[i].Table, 'export');

    ZipFile.Close;
    ZipFile.Free;

    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  finally
  end;
end;

procedure TMetaDataOperations.DatabaseImportData(Sender: TObject);
var i: Integer;
    DataCache: TDataCache;
    SourceDB, TargetDB: TDeCustomDatabase;
    OpenDialog: TOpenDialog;
    aTableList: TTablesList;
begin
  if Assigned(Sender) and (Sender is TDeActionData) then
    DataCache := MetaData.FindActiveDataCache((Sender as TDeActionData).DataSet, True)
  else
    if Assigned(MetaData.MetaTables[idxBase]) then
      DataCache := MetaData.FindActiveDataCache(MetaData.MetaTables[idxBase].ID, True)
    else
      DataCache := nil;

  if not Assigned(DataCache) then Exit;
  if Not Assigned(DataCache.FocusedItem) then Exit;

  UpdateWindow(Application.MainForm.Handle);

  TargetDB:= nil;
  for i:= 0 to Pred(MetaData.DatabasesCount) do
    if (MetaData.Databases[i].ID = DataCache.FocusedItem.ID) then TargetDB := MetaData.Databases[i];

  if assigned(TargetDB) then
    if TargetDB.CheckConnection then
      begin
        OpenDialog := TOpenDialog.Create(Application);
        try

          OpenDialog.Title := GetTitle('_Dl.Location');
          OpenDialog.InitialDir := Variables.AsString[RegDirPath];
          OpenDialog.Filter := 'All Files|*.*';//sADXFilter;
          OpenDialog.DefaultExt := sExtensionXMLX;
          if OpenDialog.Execute then
            try
              Metadata.BeginUpdate;
              SourceDB:= TDBCOXMLDatabase.Create(nil);
              SourceDB.Server:= EmptyStr;
              SourceDB.Database:= OpenDialog.FileName;

              try
                SourceDB.Connected:= True;
                aTableList:= TTablesList.Create;
                SourceDB.RetrieveMetaTables(aTableList);

                SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, aTableList.Count, 1);
                for i:=0 to Pred(aTableList.Count) do
                  try
                    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, i, 0);
                    aTableList[i].isDynamic:= True;
                    SourceDB.GetMetaTableInfo(aTableList[i]);
                    MigrationTable(aTableList[i], TargetDB);
                  except
                    on E: Exception do
                      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
                                             NativeInt('DatabaseImportData('+ aTableList[i].Table + ') ' + E.Message));
                  end;
              finally
                SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
              end;
            finally;
              SourceDB.Free;
              Metadata.EndUpdate;
            end;
        finally
          OpenDialog.Free;
        end;
      end;
end;

procedure TMetaDataOperations.DatabaseImportMetaData(Sender: TObject);
var i: Integer;
    DataCache: TDataCache;
    SourceDB, TargetDB: TDeCustomDatabase;
    aSourceList: TTablesList;
    ATargetList: TStringList;
begin
  if not (Application.MessageBox( PChar(GetTitle('_dA.importmetadata')),
                                  PChar(GetTitle('_dL.Confirmation')), MB_YESNO or MB_ICONQUESTION) = idYes) then Exit;

  if Assigned(Sender) and (Sender is TDeActionData)
    then DataCache:= MetaData.FindActiveDataCache((Sender as TDeActionData).DataSet, True)
    else if Assigned(MetaData.MetaTables[idxBase])
           then DataCache:= MetaData.FindActiveDataCache(MetaData.MetaTables[idxBase].ID, True)
           else DataCache:= nil;

  if not Assigned(DataCache) then Exit;
  if not Assigned(DataCache.FocusedItem) then Exit;

  TargetDB:= nil;
  if MetaData.DatabasesCount = 0 then Exit else
  if MetaData.DatabasesCount = 1 then TargetDB:= MetaData.Databases[0] else
    for i:= 0 to Pred(MetaData.DatabasesCount) do
      if (MetaData.Databases[i].ID = DataCache.FocusedItem.ID) then
        TargetDB := MetaData.Databases[i];

  if assigned(TargetDB) then
    if TargetDB.CheckConnection then
      begin

        ATargetList:= TStringList.Create;
        TargetDB.RetrieveTableNames(ATargetList);
        if ATargetList.Count = 0 then
          begin
            Metadata.BeginUpdate;
            aSourceList:= TTablesList.Create;
            SourceDB:= MetaData.SampleMTDB;
            SourceDB.Connected:= True;
            SourceDB.RetrieveMetaTables(aSourceList);

            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, aSourceList.Count, 1);
            for i:=0 to Pred(aSourceList.Count) do
              try
                SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, i, 0);
                SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY,
                                       NativeInt(PChar(aSourceList[i].Table+' - '+IntToStr(aSourceList.Count) )), -2);
                aSourceList[i].isDynamic:= True;
                SourceDB.GetMetaTableInfo(aSourceList[i]);
                MigrationTable(aSourceList[i], TargetDB);
              except
                on E: Exception do
                  SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
                                         NativeInt('DatabaseImportMetaData('+ aSourceList[i].Table + ') ' + E.Message));
              end;
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
            aSourceList.Free;
            Metadata.EndUpdate;
          end;
        aTargetList.Free;
      end;
end;

procedure TMetaDataOperations.CreateShortLink(Sender: TObject);
var
  DataCache: TDataCache;
  CacheItem: TCacheItem;
  CommandID: Integer;
  {$IFDEF ACTIONICONTYPED}
  ActionType: TActionType;
  {$ENDIF}
  FileName, IconFileName, Parameters, Value: string;
  IconIndex: Integer;
  Unknown: IUnknown;
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
begin
  if Assigned(CurrentConfig) then
    begin
      DataCache := MetaData.FindActiveDataCache(MetaData.MetaTables[idxCommands].ID, True);
      CacheItem := DataCache.FocusedItem;
      if Assigned(CacheItem) then
        begin
          FileName := GetDesktopDirectory + NormalizeFileName(CurrentConfig.Name + ' - ' + Trim(CacheItem.Caption)) + sExtensionLNK;
          Parameters := Variables.PrepareCommandLineParameters;
          if Length(Parameters) <> 0 then Parameters := ' ' + Parameters;
          Parameters := CurrentConfig.PrepareCommandLineParameters + Parameters;
          CommandID := CacheItem.ValueByName[fldCommandID];
          {$IFDEF ACTIONICONTYPED}
          IconFileName := Application.ExeName;
          ActionType := TActionType(CacheItem.ValueByName[fldCommandType]);
          case ActionType of
            atGroup, atStoredProcedure, atShellExecute, atPreview, atReport,
            atUpdateValue, atInsertValue: { Типизируем иконки }
              if not TryResourceToIconIndex(101 + Ord(ActionType), IconIndex) then
                IconIndex := 0;
          else
            IconIndex := 0;
          end;
          {$ELSE}
          {$IFDEF ACTIONICONREAL}
          IconFileName := EmptyStr;
          IconIndex := VarToInt(CacheItem.ValueByName[fldCommandICO]);
          // Логика взятия базовых иконок из EXE`шника:
          // BEGIN
          if (IconIndex > 0) and (IconIndex < 256) then
            begin
              if TryResourceToIconIndex('P' + UpperCase(IntToHex(IconIndex, 2)), IconIndex) then
                IconFileName := Application.ExeName;
            end;
          // END
          if (Length(IconFileName) = 0) and (IconIndex > 0) then
            begin
              IconFileName := IncludeTrailingBackslash(LogDirectory + GUIDToString(DM.IconManager.SchemaGUID));
              if SysUtils.ForceDirectories(IconFileName) then
                SetFileAttributes(PChar(IconFileName), GetFileAttributes(PChar(IconFileName)) or faHidden);
              IconFileName := IconFileName + IntToHex(IconIndex, 8) + sExtensionICO;
              if not FileExists(IconFileName) then
                if not DM.IconManager.ExportToFile(IconFileName, IconIndex) then
                  IconIndex := -1;
            end;
          if IconIndex < 1 then
            begin
              IconFileName := Application.ExeName;
              IconIndex := 0;
            end;
          {$ELSE}
          IconFileName := Application.ExeName;
          IconIndex := 0;
          {$ENDIF}
          {$ENDIF}
          Value := TDeActionData.PrepareCommandLineParameters(CommandID);
          if Length(Value) <> 0 then Value := ' ' + Value;
          Parameters := Parameters + Format(' "-command:%d %s" -quit', [CommandID, Value]);
          Unknown := CreateComObject(CLSID_ShellLink);
          try
            ShellLink := Unknown as IShellLink;
            try
              ShellLink.SetPath(PChar(Application.ExeName));
              ShellLink.SetArguments(PChar(Parameters));
              ShellLink.SetWorkingDirectory(PChar(ExtractFilePath(Application.ExeName)));
              ShellLink.SetDescription(PChar(CurrentConfig.Name));
              ShellLink.SetIconLocation(PChar(IconFileName), IconIndex);
              PersistFile := Unknown as IPersistFile;
              try
                PersistFile.Save(PChar(FileName), True);
              finally
                PersistFile := nil;
              end;
            finally
              ShellLink := nil;
            end;
          finally
            Unknown := nil;
          end;
          SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo,
            NativeInt(PChar(GetTitle('_Da.CreateDesktopShortcutCompleted'))));
        end;
    end;
end;

procedure TMetaDataOperations.ExportSolution(Sender: TObject);
resourcestring
  sExtensionPMX = '.pmx';
var
  DataCache: TDataCache;
  Dialog: TSaveDialog;
  Value: string;
  ZipFile: TZipFile;
  Buffer: TBytes;
begin
  if Assigned(Sender) and (Sender is TDeActionData) then
    DataCache := MetaData.FindActiveDataCache((Sender as TDeActionData).DataSet, True)
  else
    if Assigned(MetaData.MetaTables[idxSolutions]) then
      DataCache := MetaData.FindActiveDataCache(MetaData.MetaTables[idxSolutions].ID, True)
    else
      DataCache := nil;
  if Assigned(DataCache) and Assigned(DataCache.FocusedItem) then
    begin
      Dialog := TSaveDialog.Create(Application);
      try
        Dialog.Title := GetTitle('_Dm.SolChooseFile');
        Dialog.FileName := NormalizeFileName(DataCache.FocusedItem.Caption);
        Dialog.Filter := 'Solution Export Files|*' + sExtensionPMX;
        Dialog.DefaultExt := sExtensionPMX;
        Dialog.Options := Dialog.Options + [ofOverwritePrompt];
        if Dialog.Execute then
          begin
            UpdateWindow(Application.MainForm.Handle);
            ZipFile := TZipFile.Create;
            try
              ZipFile.Open(Dialog.FileName, zmWrite);
              try
                Value := MetaData.PrepareMetadataXML(DataCache.FocusedItem.ID, 0, ZipFile);
                Buffer := TEncoding.UTF8.GetBytes(Value);
                ZipFile.Add(Buffer, 'metadata' + sExtensionXML);
              finally
                ZipFile.Close;
              end;
            finally
              ZipFile.Free;
            end;
          end;
      finally
        Dialog.Free;
      end;
    end;
end;
{$ENDREGION}

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('DeActions unit initialization ...');
  {$ENDIF}
  ContainerStorage := TActionContainerStorage.Create(false);
  ProfitActions := TDeActionList.Create(false);
  MetaDataOperations := TMetaDataOperations.Create;
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DeActions unit finalization ...');
  {$ENDIF}
  FreeAndNil(MetaDataOperations);
  FreeAndNil(ProfitActions);
  FreeAndNil(ContainerStorage);
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

