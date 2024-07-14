unit DataCacheUnit;

interface

uses
  Windows, SysUtils, Classes, Contnrs, Graphics, Xml.XMLDoc, Xml.XMLIntf,
  DeTypes, DeCalculator, DeMeta, DeParser, DeDB, QueryDescriptor, DataManager, DeTemplate;

resourcestring
  sErrorOutOfBoundsFmt = 'Field index %d out of bounds [%d, %d].';

const
  CharSetHEX: array[0..15] of AnsiChar = '0123456789abcdef';

  // LocateTypes - типы позиционирования при синхронизации
  // Значения 0 и больше используются для позиционирования на запись по ее порядковому номеру
  ltItem  = -4;
  ltNone  = -3;
  ltRoot  = -2;
  ltLast  = -1;
  ltFirst =  0;

type
  // Флаги проверки изменения записи другими пользователями   .. проверяем в CheckChanges перед любыми изменениями
  TChangedFlag = (
    flExist,          // Запись не удалена
    flBasketBase,     // Признак удаленности = BaseValue    .. не изменилась
    flDataBase,       // Остальные поля = BaseValue         .. не изменились
    flBasketCurrent,  // Признак удаленности = CurrentValue .. кто-то уже сделал тоже самое удалил/восстановил
    flDataCurrent     // Остальные поля = CurrentValue      .. кто-то уже сделал те же изменения данных
  );
  TChangedFlags = set of TChangedFlag;

  TCacheItem  = class;
  TDataCache  = class;

  EBaseValueConflict = class(Exception); // Исключение при разных значениях в FieldBaseValue и БД.

  TItemEvent       = procedure (Item : TCacheItem) of object;
  TQueryItemEvent  = procedure (Item : TCacheItem;  var aCanProcess : boolean) of object;
  TItemAllowEvent  = procedure (var NewItem : TCacheItem;var Allow : Boolean) of object;
  TDisplayMessageEvent = procedure (const aMessage : string) of object;

  TDeFieldValue = array of Variant;

  TItemStateType = (
    isReadOnly,        // запись только для чтения
    isVirtual,         // запись виртуальная, в базе ее нет
    isNotInitialized,  // запись не инициализирована
    isInserted,        // запись добавлена
    isDeleted,         // запись удалена
    isModified,        // запись изменена
    isInitializing,    // в настоящий момент происходит инициализация записи
    isDeleting,        // в настоящий момент происходит удаление записи
    isModifying,       // в настоящий момент происходит изменение записи
    isSelect,          // запись выделена
    isBaseConflict,    // Разные значения в FieldBaseValue и БД
    isBaseNotFound,    // Запись не найдена в БД
    isWaitValue,       // Запись ожидает изменения ссылочных значений
    isFlagNull,
    isFlagSkip,
    isFlagEdit
  );

  TItemState = set of TItemStateType;
  TDecodeString = (dsNone, dsCodePage, dsCaption);

  TSyncronizeEvent = procedure (var AFocusedItem : TCacheItem; AIndex: Integer = ltNone) of object;
  TQueryOpenDataEvent = function(aDataSet : TDeDataSet) : boolean of object;
  TOnUpdateQueryDescr = procedure(aDescr : TCustomQueryDescr) of object;

  // Событие подготовки следующей партии для Fetch`а ...
  TOnFetchQueryDescr = function(Descr: TCustomQueryDescr): Boolean of object;

  TCacheItem = class(TObject)
  private
    FOwner       : TDataCache;
    FFields      : array of Variant;
    FState       : TItemState;
    FStage       : TFieldStage;
    FImageIndex  : Integer;
    FMapImageIndex : Integer;
    FFontColor   : Integer;
    FSourceIndex : Integer;

    function GetFocused : Boolean;
    function GetSelected : Boolean;
    procedure SetSelected(const Value: Boolean);
    function GetSelectedItemIndex: integer;

    function GetParentID: Variant;
    procedure SetParentID(const Value: Variant);
    function GetDeleted : boolean;
    procedure SetDeleted(const aDeleted : boolean);
    function GetDefault : boolean;
    procedure SetDefault(const aDefault : boolean);
    function GetIsFolder : boolean;
    procedure SetIsFolder(const aFolder : boolean);
    function GetValueByName(const aName: string): Variant;
    function GetBaseValueByName(const aName: string): Variant;
    procedure SetValueByName(const aName: string; const Value: Variant);
    function  GetClipBoardValue(const FieldIndex: Integer): string;
    procedure SetClipBoardValue(const FieldIndex: Integer; const Value: string);
    function GetXML: string;
    function GetHint: string;
    function GetFlagEdit: Boolean;
    function GetFlagNull: Boolean;
    function GetFlagSkip: Boolean;
    procedure SetFlagEdit(const Value: Boolean);
    procedure SetFlagNull(const Value: Boolean);
    procedure SetFlagSkip(const Value: Boolean);
    function GetFieldTextByName(const aName: string): string;
  protected
    function GetFieldValue(const Index: Integer): Variant; virtual;
    procedure SetFieldValue(const Index: Integer; const Value: Variant); virtual;
    function GetFieldNativeValue(const Index: Integer): Variant; virtual;
    procedure SetFieldNativeValue(const Index: Integer; const Value: Variant); virtual;

    function GetNativeValueByName(const aName : string): Variant;
    procedure SetNativeValueByName(const aName : string; const Value: Variant);

    function GetID: Variant;  virtual;
    procedure SetID(aID : Variant);  virtual;
    function GetCaption: String;  virtual;
    function GetCaptionValue: Variant; virtual;
    function GetFieldBaseValue(const Index: Integer): Variant; virtual;
    function GetImageIndex(const aIndex: Integer): Integer; virtual;
    function GetFontColor : TColor;  virtual;
    function GetMapImageIndex: Integer; virtual;
    property SourceIndex : Integer read FSourceIndex;
  public
    Data : Pointer;
    constructor Create(AOwner : TDataCache; const aSourceIndex: Integer);  virtual;
    destructor Destroy;  override;
    procedure SetFocus;
    procedure BeginInit;
    procedure EndInit;
    procedure InitFieldValue(const Index: integer; const Value: Variant);
    function InitFieldValueByNameExternal(const aName: string; const Value: Variant): Integer;
    procedure FillItem(const aStage: TFieldStage = fsBase); inline;

    property FieldValue[const Index: Integer]: Variant read GetFieldValue write SetFieldValue;
    property FieldBaseValue[const Index: Integer]: Variant read GetFieldBaseValue;
    property FieldNativeValue[const Index: Integer]: Variant read GetFieldNativeValue write SetFieldNativeValue;

    /// <summary>Функция возвращает значение поля в виде текста</summary>
    /// <param name="Index">Номер поля</param>
    /// <param name="MaxLength">Максимальная длина: =0 как есть; >0 не длиннее; <0 не длиннее с добавлением многоточия в конце</param>
    /// <returns>Функция возвращает значение поля в виде текста с ограничение по длине</returns>
    function FieldText(const Index: Integer): string; virtual;
    function FieldIsNewValue(const Index: Integer): Boolean;
    function KeyAsQuotedStr: string;
    function ItemTitle(aSeparator: String = SplitText): String;  virtual;

    property ValueByName[const aName : string]: Variant read GetValueByName write SetValueByName;
    property ValueNativeByName[const aName : string]: Variant read GetNativeValueByName write SetNativeValueByName;
    property FieldTextByName[const aName : string]: string read GetFieldTextByName;
    property BaseValueByName[const aName : string]: Variant read GetBaseValueByName;

    function FieldValueRecurce(const aName: string; var aValue: Variant; aDecodeString: TDecodeString = dsCaption): Boolean;
    property ClipboardValue[const Index: Integer] : string read GetClipboardValue write SetClipboardValue;
    property Selected : Boolean read GetSelected write SetSelected;
    property Focused : Boolean read GetFocused;
    property SelectedItemIndex : integer read GetSelectedItemIndex;
    property Owner : TDataCache read FOwner;
    property State : TItemState read FState write FState;
    property FlagEdit: Boolean read GetFlagEdit write SetFlagEdit;
    property FlagSkip: Boolean read GetFlagSkip write SetFlagSkip;
    property FlagNull: Boolean read GetFlagNull write SetFlagNull;
    property Stage : TFieldStage read FStage write FStage;
    property ID : Variant read GetID write SetID;
    property ParentID : Variant read GetParentID write SetParentID;
    property Caption : String read GetCaption;
    property CaptionValue : Variant read GetCaptionValue;
    property Hint: string read GetHint;
    property Deleted : boolean read GetDeleted write SetDeleted;
    property Default : boolean read GetDefault write SetDefault;
    function ValueChanged(const Index: Integer): Boolean; overload;
    function ValueChanged(const aName: String; var aNewValue: Variant): Boolean; overload;
    property IsFolder : boolean read GetIsFolder write SetIsFolder;
    property ImageIndex : integer index 1 read GetImageIndex;
    property SelectedImageIndex : integer index 0 read GetImageIndex;
    property MapImageIndex : integer read GetMapImageIndex;
    property FontColor : TColor read GetFontColor;
    procedure Restore;
    procedure ApplyChanges;
    procedure RollbackChanges;
    function GetFirstChild: TCacheItem;
    function GetNextChild(CurrentChild : TCacheItem): TCacheItem;
    function IsParent(ParentIDValue : Variant):Boolean;  virtual;
    procedure Assign(aItem: TObject);
    function Calculate(aPostfix: TExpressionItem; const aDefaultResult: Variant): Variant;
    function PrepareClipboardXML(const Level: Integer = -1): string;
  end;

  TConfigCacheItem = class(TCacheItem)
  protected
    function GetFieldValue(const Index: Integer): Variant; override;
    function GetImageIndex(const aIndex: Integer): Integer; override;
    function GetFieldBaseValue(const Index: Integer): Variant; override;
    procedure SetFieldValue(const Index: Integer; const Value: Variant); override;
  public
    constructor Create(AOwner : TDataCache; const aSourceIndex: Integer);  override;
  end;

  TRootItem = class(TCacheItem)
  protected
    procedure SetFieldValue(const Index: Integer; const Value: Variant); override;
    function GetID: Variant;  override;
    function GetCaption: string;  override;
    function GetFieldValue(const Index: Integer): Variant; override;
    function GetFieldBaseValue(const Index: Integer): Variant; override;
    function GetImageIndex(const aIndex: Integer): Integer; override;
    function GetFontColor : TColor;  override;
  public
    constructor Create(AOwner : TDataCache; const aSourceIndex: Integer);  override;
    function FieldText(const Index: Integer): string; override;
    function IsParent(ParentIDValue : Variant):Boolean;  override;
  end;

  /// <summary>Класс с информацией о сортировке по полю</summary>
  TSortInfo = class
  private
    FID        : Integer;
    FDirection : TDSSortDirection;
  public
    constructor Create(aField: TFieldMeta; const aDirection: TDSSortDirection = sdAscending); overload;
    constructor Create(aID: Integer; const aDirection: TDSSortDirection = sdAscending); overload;
    property FieldID: Integer read FID;
    property Direction: TDSSortDirection read FDirection write FDirection;
    function DirectionName: string;
    function NextDirection: TDSSortDirection;
  end;

  /// <summary>Класс со списком информации о сортировке по полям</summary>
  TSortList = class(TObjectList)
  private
    function GetItem(const Index: Integer): TSortInfo;
  public
    /// <summary>Функция добавления информации о сортировке по полю</summary>
    /// <param name="Field">Поле для сортировки</param>
    /// <param name="Direction">Направление для сортировки</param>
    /// <returns>Функция возвращает индекс в массиве или -1 в случае ошибки добавления</returns>
    function Add(aID: Integer; const Direction: TDSSortDirection = sdAscending): Integer; reintroduce;
    /// <summary>Функция добавления информации о сортировке по полям</summary>
    /// <param name="Fields">Поля набора данных</param>
    /// <param name="Text">Список полей</param>
    /// <param name="DefaultDirection">Направление сортировки по умолчанию</param>
    /// <returns>Функция возвращает True в случае добавления хотя бы одного поля</returns>
    /// <remarks>В списке полей необходимо указывать оригинальное имя поля и при необходимости тип сортировки (ASC - знак плюса или DESC - знак минуса).
    /// Если полей сортировки несколько, то их необходимо указать через символ вертикальной черты. Например: "ID|MODIFY_DATE-"</remarks>
    function AddFields(Fields: TFieldsMeta; const Text: string; const DefaultDirection: TDSSortDirection = sdNone): Boolean;
    /// <summary>Функция поиска объекта с информацией о сортировке по полю</summary>
    /// <param name="Field">Поле сортировки</param>
    /// <returns>Функция возвращает объект класса TSortInfo или nil в случае его отсутствия в массиве</returns>
    function ItemByID(FieldID: Integer): TSortInfo;
    /// <summary>Функция поиска объекта с информацией о сортировке по полю</summary>
    /// <param name="ID">Идентификатор поля</param>
    /// <returns>Функция возвращает индекс в массиве или -1 в случае отсутствия информации о поле в массиве</returns>
    function IndexByID(const ID: Integer): Integer;
    procedure Assign(Source: TObject);
    /// <summary>Массив с информацией о сортировке по полям</summary>
    property Items[const Index: Integer]: TSortInfo read GetItem; default;
    {$IFDEF DEBUG}
    procedure DebugFieldsLog(const Text: string);
    {$ENDIF}
    /// <summary>Функция сравнения двух списков сортировки</summary>
    /// <param name="Source">Список сортировки для сравнения</param>
    /// <returns>Функция возвращает True в случае идентичности двух списков, иначе - False</returns>
    function CompareTo(SortList: TSortList): Boolean;
    function FirstFieldDirestion(aField: TFieldMeta): TDSSortDirection;
  end;

  TCacheItemList = class(TObjectList)
  private
    function GetItem(const Index: Integer): TCacheItem;
    procedure PutItem(const Index: Integer; const Value: TCacheItem);
    function PrepareClipboardXML(const Level: Integer = -1; const InnerDataset: Boolean = False; const LinkID: Integer = 0): string;
  public
    {$IFDEF DEBUG}
    procedure DebugDumpLog(const FileName: string);
    {$ENDIF}
    property Items[const Index: Integer]: TCacheItem read GetItem write PutItem; default;
  end;

  TFieldsCache = class;

  /// <summary>Флаги состояния поля:
  /// <para><c>fsCalculating</c> - значение поле рассчитывается</para>
  /// <para><c>fsLinkedDataSet</c> - значение берётся из связанного DataSet`а</para>
  /// </summary>
  TFieldCacheState = (fsCalculating, fsLinkedDataSet);
  TFieldCacheStates = set of TFieldCacheState;

  TFieldCache = class(TFieldMeta)
  private
    FOwner: TFieldsCache;
    FPairIndex: Integer;
    FDatasetIndex: Integer;
    FFieldIndex: Integer;
    FIDIndex: Integer;
    FIDs: array of Variant;
    FFirstDateTime: TDateTime;  // Дата и время первого чтения
    FLastDateTime: TDateTime;   // Дата и время последнего чтения
    FAccessDateTime: TDateTime; // Дата и время последнего получения значения из поля
    FState: TFieldCacheStates;  // Состояние поля
    function GetDataCache: TDataCache;
    function GetDataSet: TDeDataset;
    function GetRowCount: Integer;
    function LinkSubDataset(const DatasetIndex, KeyFieldIndex: Integer): Boolean;
    procedure UnlinkSubDataset;
    function GetCalculating: Boolean;
    procedure SetCalculating(const Value: Boolean);
  public
    constructor Create(AOwner: TFieldsCache);
    destructor Destroy; override;
    function UpdateStats: Boolean;
    function ResetStats: Boolean;
    function IndexByRowID(const RowID: Variant): Integer;
    procedure Assign(Source: TObject; const FullAssign: Boolean = True); override;
    property Owner: TFieldsCache read FOwner;
    property DataCache: TDataCache read GetDataCache;                  // Ссылка на DataCache, в котором используется этот FieldCache
    property PairIndex: Integer read FPairIndex;                       //
    property DatasetIndex: Integer read FDatasetIndex;                 // Индекс вложенного DataSet`а в списке DataCache.SubDatasets
    property FieldIndex: Integer read FFieldIndex;                     // Индекс поля во вложенном DataSet`е
    property DataSet: TDeDataset read GetDataSet;                      // По DatasetIndex возращает сам DataSet или nil
    property RowCount: Integer read GetRowCount;                       // Количество строк в буфере
    function GetRowID(const aIndex: Integer): Variant;                     // Значение ключа во вложенном DataSet`е
    function GetRowValue(const aIndex, aField: Integer): Variant;                // Значение во вложенном DataSet`е
    property State: TFieldCacheStates read FState;                     // Состояние поля
    property Calculating: Boolean read GetCalculating write SetCalculating;
  end;

  TFieldsCache = class(TFieldsMeta)
  private
    FOwner: TDataCache;
    function GetItem(const Index: Integer): TFieldCache;
  public
    constructor Create(AOwner: TDataCache);
    procedure AssignFields(Source: TFieldsMeta);
    procedure ResetStats(const Stage: TFieldStage = fsBase);
    procedure SortByOrder; override;
    procedure ChangeStages(const aFieldNames: array of string; const Stage: TFieldStage = fsBase);
    //procedure SynchronizeUserFields(Target: TFieldsMeta);
    property Owner: TDataCache read FOwner;
    property Items[const Index: Integer]: TFieldCache read GetItem; default;
  end;

  TSubDatasets = class(TObjectList)
  private
    FOwner: TDataCache;
    function GetItem(const Index: Integer): TDeDataset;
  public
    constructor Create(AOwner: TDataCache);
    function Add(const OwnerFieldIndexList: array of Integer; const NewStage: TFieldStage): Integer; overload;
    function Add(const OwnerFieldIndex: Integer; const NewStage: TFieldStage): Integer; overload;
    procedure Delete(const Index: Integer);
    procedure Clear; override;
    property Owner: TDataCache read FOwner;
    property Items[const Index: Integer]: TDeDataset read GetItem; default;
  end;

  TCacheIndexes = class;

  TDataCache = class(TComponent)
  private
    FFirstFocused          : Boolean;
    FCacheItems            : TCacheItemList;
    FCacheSelectedItems    : TCacheItemList;
    FCanCreateItems        : Boolean;
    FFocusedItem           : TCacheItem;
    FFocusedChange         : TItemEvent;
    FSelectedChange        : TItemEvent;
    FRootItem              : TRootItem;
    FIDIndex               : integer;
    FParentIDIndex         : integer;
    FCaptionIndex          : Integer;
    FFolderSignIndex       : integer;
    FDeletedSignIndex      : integer;
    FDefaultSignIndex      : integer;
    FTableMeta             : TTableMeta;
    FSelectedChanging      : TItemEvent;
    FFocusedChanging       : TItemEvent;
    FAllowFocusedChanging  : TItemAllowEvent;
    FOffSelectedChange     : boolean;
    FDataset               : TDeDataset;
    FOnDataAfterOpen       : TNotifyEvent;
    FOnDataSetupFilter     : TOnUpdateQueryDescr;
    FOnBeforeSyncronize    : TSyncronizeEvent;
    FOnQueryOpenData       : TQueryOpenDataEvent;
    FSortList              : TSortList;
    FFilters               : TFiltersManager;
    FDataManager           : TDataManager;
    FIsLibrary             : boolean;
    FOnInsertRecord        : TQueryItemEvent;
    FOnUpdateRecord        : TQueryItemEvent;
    FOnDeleteRecord        : TQueryItemEvent;
    FOnDisplayMessage      : TDisplayMessageEvent;
    FIsFillAllItems        : Boolean;
    FUpdateLocks           : integer;       // количество вызовов BeginUpdate без парных EndUpdate
    FUpdateType            : TTypeChanged;  // тип последнего Update'а
    FUpdateKey             : Variant;       // ключ для последенго Update'а
    FFields                : TFieldsCache;  // Список полей для первой выборки (базовая выборка; копия из TTableMeta, но можно изменять)
    FSubDatasets           : TSubDatasets;  // Список вложенных подзапросов
    FBaseStage             : TFieldStage;   // Базовый уровень выборки (fsBase по умолчанию. Нужен для того, чтобы можно было делать выборки fsFill/fsBlob без дополнительных подзапросов.)
    FControlFilter         : TFilterItem;   // Фильтр BaseGridForm контрола, v. 16.12
    FCacheIndexes          : TCacheIndexes; // Индексы для кэша
    function GetCacheItems: TCacheItemList;
    type
      TClipboardExcelCellBorder = (cbBottom, cbLeft, cbRight, cbTop);
      TClipboardExcelCellBorders = set of TClipboardExcelCellBorder;
      TClipboardExcelStyle = class
      private
        FStyleID: Integer;
        FFormat: string;
        FColor: TColor;
        FBackground: TColor;
        FStyles: TFontStyles;
        FBorders: TClipboardExcelCellBorders;
        function GetName: string;
        function GetText: string;
      public
        constructor Create(const AStyleID: Integer; const AFormat: string = ''; const AColor: TColor = clBlack; const ABackground: TColor = clNone; const ABorders: TClipboardExcelCellBorders = []; const AStyles: TFontStyles = []);
        property StyleID: Integer read FStyleID;
        property Format: string read FFormat;
        property Color: TColor read FColor;
        property Background: TColor read FBackground;
        property Styles: TFontStyles read FStyles;
        property Borders: TClipboardExcelCellBorders read FBorders;
        property Name: string read GetName;
        property Text: string read GetText;
      end;
      TClipboardExcelStyles = class
      private
        FList: TObjectList;
        FNextStyleID: Integer;
        function GetCount: Integer;
        function GetItem(const Index: Integer): TClipboardExcelStyle;
        function GetText: string;
      public
        constructor Create;
        destructor Destroy; override;
        function IndexOf(const Format: string; const Color, Background: TColor; const Borders: TClipboardExcelCellBorders; const Styles: TFontStyles): Integer;
        function Add(const Format: string; const Color: TColor = clBlack; const Background: TColor = clNone; const Borders: TClipboardExcelCellBorders = []; const Styles: TFontStyles = []): Integer;
        function PrepareStyle(const Index: Integer): string;
        property Count: Integer read GetCount;
        property Item[const Index: Integer]: TClipboardExcelStyle read GetItem; default;
        property Text: string read GetText;
      end;
    function GetItem(const Index: Integer): TCacheItem;
    function GetHint(const Index: Integer): string;
    procedure InsertItem(index: integer; const Value: TCacheItem);
    procedure SetFocusedItem(const Value: TCacheItem);

    function GetSelectedKeys: Variant;
    procedure SetSelectedKeys(const Value: Variant);
    function GetSelectedItem(const Index: Integer): TCacheItem;
    function GetSelectedItemsCount: Integer;

    function  GetItemsCount: Integer;
    procedure SetItemsCount(const Value: Integer);
    procedure GetMainFieldsIndexes;
    function GetFieldCount : integer;
    procedure SetOffSelectedChange(const Value: boolean);
    procedure DestroyData;
    function GetActive : boolean;
    procedure DoDataAfterOpen;
    procedure DoDataSetupFilter(aDescr : TCustomQueryDescr);
    function QueryOpenData : boolean;
    procedure PrepareQueryDescr(aDescr: TCustomQueryDescr; const aFieldStage: TFieldStage = fsBase);
    procedure AfterDataOpen(Sender: TObject);
    function PrepareClipboardXML(const Level: Integer = -1): string;
    function PasteFromNode(Node: TDeNode; const DatasetID: Integer; var OldRecordID: Variant; const InsertIndex: Integer = 0): TCacheItem;
    function PasteChildFromNode(RootNode: TDeNode; RootCacheItem: TCacheItem): Integer;
    function CreateAll: Integer;
    function ReFillItem(aCacheItem: TCacheItem; const aStage: TFieldStage; const aInit: Boolean = True): Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);  override;
    function GetFieldValue(const ItemIndex, FieldIndex: Integer): Variant;  virtual;
  public
    constructor Create(aTableMeta: TTableMeta; const aIsLibrary: Boolean = false);  reintroduce;
    destructor Destroy;  override;
    {$IFDEF DEBUG}
    procedure DebugFieldsLog(const Text: string; Fields: TFieldsMeta);
    {$ENDIF}
    function CacheItemIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
    property DataManager : TDataManager read FDataManager;
    property RootItem : TRootItem read FRootItem;
    function FocusedIndex : Integer;
    property FocusedItem : TCacheItem read FFocusedItem write SetFocusedItem;
    function ItemSelected(const Index: Integer): Boolean;
    procedure ItemSelect(const Index: Integer);
    procedure ItemUnSelect(const Index: Integer);
    property SelectedItems[const Index: Integer]: TCacheItem read GetSelectedItem;
    property SelectedCount: Integer read GetSelectedItemsCount;
    property CacheSelectedItems : TCacheItemList read FCacheSelectedItems;
    property Items[const Index: Integer]: TCacheItem read GetItem; default;
    property Hints[const Index: Integer]: string read GetHint;
    property Values[const ItemIndex, FieldIndex: Integer]: Variant read GetFieldValue;
    property Count: Integer read GetItemsCount; // write SetItemsCount;
    function Agregate(aOperation: TOperationType; sField: String; var aValue : Variant): Boolean;
    property OnFocusedChanging: TItemEvent read FFocusedChanging write FFocusedChanging;
    property OnSelectedChanging: TItemEvent read FSelectedChanging write FSelectedChanging;
    property OnAllowFocusedChanging : TItemAllowEvent read FAllowFocusedChanging write FAllowFocusedChanging;
    property OnFocusedChange: TItemEvent read FFocusedChange write FFocusedChange;
    property OnSelectedChange: TItemEvent read FSelectedChange write FSelectedChange;
    property Fields : TFieldsCache  read FFields;
    property FieldCount : integer read GetFieldCount;
    property TableMeta : TTableMeta read FTableMeta;
    property IDIndex : integer read FIDIndex;
    property ParentIDIndex : integer read FParentIDIndex;
    property CaptionIndex : integer read FCaptionIndex;
    property FolderSignIndex : integer read FFolderSignIndex;
    property DeletedSignIndex : integer read FDeletedSignIndex;
    property DefaultSignIndex : integer read FDefaultSignIndex;
    property CacheItems: TCacheItemList read GetCacheItems;
    property CacheOwnedItems: TCacheItemList read FCacheItems; // Основной список итемов, которыми владеет кэш!
    property OffSelectedChange : boolean read FOffSelectedChange write SetOffSelectedChange default FALSE;
    property OnDataSetupFilter : TOnUpdateQueryDescr read FOnDataSetupFilter write FOnDataSetupFilter;
    property OnDataAfterOpen : TNotifyEvent read FOnDataAfterOpen write FOnDataAfterOpen;
    property OnBeforeSyncronize : TSyncronizeEvent read FOnBeforeSyncronize write FOnBeforeSyncronize;
    property OnQueryOpenData : TQueryOpenDataEvent read FOnQueryOpenData write FOnQueryOpenData;
    property SortList : TSortList read FSortList;
    property Filters : TFiltersManager read FFilters write FFilters;
    property Active : boolean read GetActive;
    procedure Clear;
    function AddNewItem : TCacheItem;
    function InsertNewItem(index: integer):TCacheItem;
    function InsertExistItem(InitialItem : TCacheItem):TCacheItem;
    function CanPasteFromClipboard(const ByOwnerValue: Boolean; var ClipID: Integer): Boolean;
    procedure CopyToClipboard;
    function InsertFromClipboard: TCacheItem;
    procedure FillItem(const aCacheItem: TCacheItem; const aStage: TFieldStage = fsBase); inline;
    procedure FillItems(const StartIndex, EndIndex: Integer; const aStage: TFieldStage = fsBase);
    //procedure FillSelectedItems(const Stage: TFieldStage = fsBase);
    procedure FillAll;
    procedure ClearLinks;
    procedure IncludeDependedFields;
    function IndexByID(const LocateToRecord: Variant): Integer;
    function IndexByItem(aItem: TCacheItem; const CheckFilters: Boolean = True): Integer;
    function FindByID(IDValue: Variant): TCacheItem;
    function IndexByValues(aFieldIndexes : array of integer; aValues : array of Variant;  aSearchFrom : integer = 0;
      aSearchTo : integer = MaxInt) : Integer;
    procedure CopyFrom(aCopyFrom : TDataCache);
    procedure LoadData(aFilter: TFilterItem = nil; const aStage: TFieldStage = fsBase);
    function AlowFocusedChanging(var NewItem: TCacheItem):boolean;
    function InsertSortField(aIndex:Integer; aField : TFieldMeta) : TSortInfo;
    function Update(const Changed: TTypeChanged; const InsertKey: Variant; const LocateIndex: Integer = ltNone): Boolean;
    procedure SelectAll;
    procedure ClearSelection;
    function  GetAsXML(const aFlags: TDataFlags): String;
    property  SelectedKeys: Variant read GetSelectedKeys write SetSelectedKeys;
    procedure FillFromXML(const aXML: string); overload;
    procedure FillFromXML(const aXML: TComponent); overload;
    { методы работы с набором данных, фильтрация и сортировка }
    procedure PrepareData(const AutoOpen: Boolean = True; const Stage: TFieldStage = fsBase);
    procedure OpenData;
    procedure CloseData;
    procedure SyncronizeOnData(LocateToRecord : Variant; DefaultIndex: Integer = ltNone; LocateToItem: TCacheItem = nil);
    procedure ClearSortList;
    { методы добавления/обновления/удаления данных }
    property OnInsertRecord : TQueryItemEvent read FOnInsertRecord write FOnInsertRecord;
    property OnUpdateRecord : TQueryItemEvent read FOnUpdateRecord write FOnUpdateRecord;
    property OnDeleteRecord : TQueryItemEvent read FOnDeleteRecord write FOnDeleteRecord;
    property OnDisplayMessage : TDisplayMessageEvent read FOnDisplayMessage write FOnDisplayMessage;
    function AppendRecord(var NewRecordID : Variant) : boolean;
    function InsertRecord(aIndex : integer;  var NewRecordID : Variant) : boolean;
    function UpdateRecord(aItem : TCacheItem) : boolean;
    function CanDeleteRecord(aItem : TCacheItem) : boolean;
    function DeleteRecord(aItem : TCacheItem) : boolean;
    function PasteRecords: Boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    function SelectedItemsCaption: String;
    function GetLookupPairIndex(const aIndex: Integer): Integer;
    function CheckChanges(const aCacheItem: TCacheItem; var aFlag: TChangedFlags): Boolean;
    /// <summary>Базовый уровень выборки</summary>
    property BaseStage: TFieldStage read FBaseStage;
    /// <summary>Метод дочитывания порции запроса при связях master-detail</summary>
    procedure FetchAll(OnFetch: TOnFetchQueryDescr);
    function ReInit(const Changed: TTypeChanged; const InsertKey: Variant; const LocateIndex: Integer = ltNone): Boolean;
    procedure SaveToFileCSV(const FileName: string; const VisibleLevel: TFieldVisible = fvLevel2; const DataOnly: Boolean = False);
    property ControlFilter: TFilterItem read FControlFilter;
    property CacheIndexes: TCacheIndexes read FCacheIndexes;
    property SourceSet: TDeDataset read FDataset;
  end;

  TDataCacheList = class(TObjectList)
  private
    function Get(const Index: Integer): TDataCache;
  public
    property Items[const Index: Integer]: TDataCache read Get; default;
    function ItemsByID(const ID: Integer; autoCreate: Boolean = False): TDataCache;
  end;

  TCacheIndex = class
  private
    FOwner: TCacheIndexes;
    FCacheItems: TCacheItemList;
    FSortList: TSortList;
    FFilterItem: TFilterItem;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TCacheItem;
    procedure Initialize;
    function Compare(const LeftIndex, RightIndex: Integer): Integer;
    type
      TCompareEvent = function(const LeftIndex, RightIndex: Integer): Integer of object;
    procedure QuickSort(LowIndex, HighIndex: Integer; OnCompare: TCompareEvent);
    procedure CheckSortItems;
    procedure CheckFilterItems;
    procedure Sort;
    procedure Filter;
  public
    constructor Create(AOwner: TCacheIndexes);
    destructor Destroy; override;
    function IndexOf(CacheItem: TCacheItem): Integer;
    {$IFDEF DEBUG}
    procedure DebugIndexLog(const Text: string);
    {$ENDIF}
    property Owner: TCacheIndexes read FOwner;
    property CacheItems: TCacheItemList read FCacheItems;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TCacheItem read GetItem; default;
    property SortList: TSortList read FSortList;
    property FilterItem: TFilterItem read FFilterItem;
  end;

  TCacheIndexes = class
  private
    FList: TObjectList;
    FDataCache: TDataCache;
    FItemIndex: Integer;
    FOnChange: TNotifyEvent;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TCacheIndex;
    function GetSelected: TCacheIndex;
    procedure SetItemIndex(const Value: Integer);
  protected
    procedure DoChange; dynamic;
  public
    constructor Create(ADataCache: TDataCache);
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(const Index: Integer);
    function IndexOf(ExpressionItem: TExpressionItem; SortList: TSortList): Integer;
    function Add(SortList: TSortList; const NewOnly: Boolean = False): Integer; overload;
    function Add(ExpressionItem: TExpressionItem; const NewOnly: Boolean = False): Integer; overload;
    function Add(FilterItem: TFilterItem; const NewOnly: Boolean = False): Integer; overload;
    function Add(ExpressionItem: TExpressionItem; SortList: TSortList; const NewOnly: Boolean = False): Integer; overload;
    function Add(FilterItem: TFilterItem; SortList: TSortList; const NewOnly: Boolean = False): Integer; overload;
    function Add(FiltersManager: TFiltersManager; SortList: TSortList): Integer; overload;
    function Select(const Index: Integer): Boolean; overload;
    function Select(CacheIndex: TCacheIndex): Boolean; overload;
    procedure UnSelect;
    property DataCache: TDataCache read FDataCache;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TCacheIndex read GetItem; default;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Selected: TCacheIndex read GetSelected;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {
  TCacheIndex = class(TObject)
  private
    FPrepared : Boolean;
    FCache: TDataCache;
    FSortList : TSortList;
    Z : Array of Integer;
    procedure SetPrepared(aPrepared: Boolean);
  public
    constructor Create(aCache : TDataCache); overload;
    destructor Destroy;  override;
    property SortList : TSortList read FSortList;
    property Prepared: Boolean read FPrepared write SetPrepared;
  end;
  }
const
  // используются при копировании/вставке
  UnassignedStr   = '<!&*#$unassigned%4>';
  NullStr         = '<!&*#$null%4>';

var
  DeletedRecordColor: TColor;

implementation

uses Variants, DB, Forms, DateUtils, Math, System.NetEncoding, ClipBrd, StrUtils,
     Types, DeLog, Dictionary, DataUnit, Funcs, XMLTable, AbstractTable,
     DeMetaData, Security, DeSettings;


{ TCacheIndex }
{
constructor TCacheIndex.Create(aCache : TDataCache);
begin
  FPrepared := False;
  FCache := aCache;
  FSortList := TSortList.Create;
  SetLength(Z, 0);
end;

destructor TCacheIndex.Destroy;
begin
  FSortList.Free;
  inherited;
  SetLength(Z, 0);
end;

procedure TCacheIndex.SetPrepared(aPrepared: Boolean);
begin
  if FPrepared=aPrepared then Exit;
end;
}
{ TCacheItem }

constructor TCacheItem.Create(AOwner : TDataCache; const aSourceIndex: Integer);
var
  FieldCount: Integer;
begin
  Assert(Assigned(aOwner), 'Cache item owner can`t be nil');
  inherited Create;
  FOwner := AOwner;
  FImageIndex  := -2;
  FMapImageIndex := -1;
  FFontColor   := unassigned;
  FSourceIndex := aSourceIndex;
  FStage       := AOwner.BaseStage;
  FieldCount := Owner.FieldCount;
  SetLength(FFields, FieldCount);
end;

destructor TCacheItem.Destroy;
begin
  FFields := nil;
  inherited Destroy;
end;

procedure TCacheItem.SetFocus;
begin
  Owner.FocusedItem := Self;
end;

function TCacheItem.GetFocused : Boolean;
begin
  result:= (Owner.FocusedItem = Self);
end;

function TCacheItem.GetSelected : Boolean;
begin
  result:= isSelect in FState;
end;

procedure TCacheItem.SetSelected(const Value: Boolean);
begin
  if (isSelect in FState) <> Value then
  begin
//    if Assigned(Owner.OnSelectedChange) then  // 7777777777777777777
//      Owner.OnSelectedChange(self);

    if Value then
      begin
        Include(FState,isSelect);
        Owner.FCacheSelectedItems.Add(self);
      end
    else
      begin
        Exclude(FState,isSelect);
        Owner.FCacheSelectedItems.Remove(self);
      end;

//    if Assigned(Owner.OnSelectedChanging) then
//      Owner.OnSelectedChanging(self);
  end;
end;

function TCacheItem.GetSelectedItemIndex: integer;
begin
  if Selected then
    result:=Owner.FCacheSelectedItems.IndexOf(Self)
  else
    result:=-1;
end;

function TCacheItem.GetParentID: Variant;
begin
  if (Owner.ParentIDIndex = -1) then
    result := Null
  else
    result := GetFieldValue(Owner.ParentIDIndex);
end;

procedure TCacheItem.SetParentID(const Value: Variant);
begin
  if Owner.ParentIDIndex >=0 then
    SetFieldValue(Owner.ParentIDIndex, Value);
end;

function TCacheItem.GetDeleted : boolean;
var
  DateTime: TDateTime;
begin
  Result := Assigned(Owner.TableMeta.DField);
  if Result then
    if Owner.TableMeta.DField.DataType in DateTypes then { Удаление по времени }
      if VarIsNullOrEmpty(FieldBaseValue[Owner.DeletedSignIndex]) then
        Result := False
      else
        Result := Assigned(Owner.TableMeta.Database) and
                  Owner.TableMeta.Database.ReadServerDateTime(DateTime) and
                  (FieldBaseValue[Owner.DeletedSignIndex] <= DateTime)
    else
      Result := CompareStr(VarToStr(FieldBaseValue[Owner.DeletedSignIndex]), Owner.TableMeta.DField.Value2) = 0;
end;

procedure TCacheItem.SetDeleted(const aDeleted : boolean);
var
  DateTime: TDateTime;
begin
  if Assigned(Owner.TableMeta.DField) then
    if Owner.TableMeta.DField.DataType in DateTypes then
      if aDeleted then
        begin
          if Assigned(Owner.TableMeta.Database) and Owner.TableMeta.Database.ReadServerDateTime(DateTime) then
            FieldValue[Owner.DeletedSignIndex] := DateTime;
        end
      else
        FieldValue[Owner.DeletedSignIndex] := Null
    else
      if aDeleted then
        FieldValue[Owner.DeletedSignIndex] := Owner.TableMeta.DField.Value2
      else
        FieldValue[Owner.DeletedSignIndex] := Owner.TableMeta.DField.Value1;
end;

function TCacheItem.GetDefault : boolean;
begin
  result := Assigned(Owner.TableMeta.OField) and
            (CompareStr(VarToStr(FieldValue[Owner.DefaultSignIndex]), Owner.TableMeta.OField.Value2) = 0);
end;

procedure TCacheItem.SetDefault(const aDefault : boolean);
begin
  if Assigned(Owner.TableMeta.OField) then
    if aDefault then FieldValue[Owner.DefaultSignIndex] := Owner.TableMeta.OField.Value2
                else FieldValue[Owner.DefaultSignIndex] := Owner.TableMeta.OField.Value1;
end;

function TCacheItem.ValueChanged(const Index: Integer): Boolean;
begin
  Result := not VarSameValue(FieldValue[Index], FieldBaseValue[Index]);
end;

function TCacheItem.ValueChanged(const aName: String; var aNewValue: Variant): Boolean;
var N: Integer;
begin
  Result:= False;
  aNewValue:= unassigned;

  N:= Owner.Fields.IndexByName(aName);
  if -1 < N then
    begin
      Result:= ValueChanged(N);
      aNewValue:= FieldValue[N];
    end;
end;

function TCacheItem.GetXML: string;
var
  Index, Size: Integer;
  Value: Variant;
  FieldName, BlobString: string;
  MemoryStream: TMemoryStream;
  StringStream: TStringStream;
  Base64Encoding: TBase64Encoding;
  ArrayPtr: Pointer;
begin
  Result := EmptyStr;
  BlobString := EmptyStr;
  if Assigned(Owner) then
    for Index := 0 to Pred(FOwner.FieldCount) do
      begin
        Value := FieldValue[Index];
        if not (VarIsEmpty(Value) or VarIsNull(Value)) then
          begin
            FieldName := Owner.Fields[Index].Original;
            case Owner.Fields[Index].DataType of
              ftBlob: { Blob поля }
                if VarIsArray(Value) then
                  begin
                    // Кодируем в BASE64 строки ...
                    Size := VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1);
                    MemoryStream := TMemoryStream.Create;
                    try
                      ArrayPtr := VarArrayLock(Value);
                      try
                        MemoryStream.WriteBuffer(ArrayPtr^, Size);
                      finally
                        VarArrayUnlock(Value);
                      end;
                      MemoryStream.Position := 0;
                      StringStream := TStringStream.Create;
                      try
                        Base64Encoding := TBase64Encoding.Create(76, #13);
                        try
                          Base64Encoding.Encode(MemoryStream, StringStream);
                        finally
                          Base64Encoding.Free;
                        end;
                        BlobString := BlobString + '<' + FieldName + '>'#13 +
                          StringStream.DataString + #13'</' + FieldName + '>'#13;
                      finally
                        StringStream.Free;
                      end;
                    finally
                      MemoryStream.Free;
                    end;
                  end;
            else
              Result := Result + ' ' + FieldName + XMLEncode(VarToStr(Value));
            end;
          end;
      end;
  if (Length(Result) <> 0) or (Length(BlobString) <> 0) then
    begin
      Result := '<row' + Result;
      if Length(BlobString) <> 0 then
        Result := Result + '>'#13 + BlobString + '</row>'#13
      else
        Result := Result + ' />'#13;
    end;
end;

function TCacheItem.GetIsFolder : boolean;
begin
  result := (Owner.FolderSignIndex >= 0) and
              VarSameValue(FieldValue[Owner.FolderSignIndex], Owner.TableMeta.GField.Value1);
end;

procedure TCacheItem.SetIsFolder(const aFolder : boolean);
begin
  if Owner.FolderSignIndex >= 0 then
    FieldValue[Owner.FolderSignIndex] := Owner.TableMeta.GField.Value1
  else
    FieldValue[Owner.FolderSignIndex] := Owner.TableMeta.GField.Value2;
end;

function TCacheItem.Calculate(aPostfix: TExpressionItem; const aDefaultResult: Variant): Variant;
var aCalculator : TDeCalculator;
begin
  Result := aDefaultResult;

  if Assigned(aPostfix) and (aPostfix.Count > 0) then
    begin
      aCalculator := TDeCalculator.Create;
      aCalculator.OnGetIdentValue := Owner.CacheItemIdentValue;
      aCalculator.CI:= Owner.FTableMeta.Database.CI;

      Result := aCalculator.Calculate(aPostfix, Self);

      aCalculator.Free;
    end;
end;

function TCacheItem.GetValueByName(const aName: string): Variant;
var P,N       : Integer;
    a,b       : string;
    LookupKey : Variant;
    CI        : TCacheItem;
begin
  N:= Owner.Fields.IndexByName(aName);
  if 0<=N then
    result := FieldValue[N]
  else
    try
      P:= pos('.',aName);
      if P>0 then
        begin
          a:=Trim(Copy(aName,1,P-1));
          b:=Trim(Copy(aName,P+1,MaxInt));

          N := FOwner.Fields.IndexByName(a);
          if N>=0 then
          if (FOwner.Fields[N].LookupPair<>nil) then
            begin
              LookupKey := FieldValue[N];
              CI := TDataCache(FOwner.Fields[N].LookupPair.Lookup).FindByID(LookupKey);
              if Not (CI=nil) then
                Result:=CI.GetValueByName(b);
            end;
        end;
    except
      {$IFDEF DEBUG}
      on E: Exception do
        DebugLog('TCacheItem.GetValueByName(%s) error: %s', [QuotedStr(aName), E.Message]);
      {$ENDIF}
    end;
end;

function TCacheItem.GetBaseValueByName(const aName: string): Variant;
var N       : Integer;
begin
  N:= Owner.Fields.IndexByName(aName);
  if 0<=N then
    result := FieldBaseValue[N];
end;

procedure TCacheItem.SetValueByName(const aName: string; const Value: Variant);
begin
  FieldValue[Owner.Fields.IndexByName(aName)]:= Value;
end;

function TCacheItem.FieldValueRecurce(const aName: string; var aValue: Variant; aDecodeString: TDecodeString = dsCaption): Boolean;
var P,N : Integer;
    CI  : TCacheItem;
    LookupKey : Variant;
begin
  Result:=False;
  aValue:=Null;

  P:= pos('.', aName);
  if P=0 then
    begin
      N:=Owner.Fields.IndexByName( aName );
      if N <> -1 then
        begin
          Result:=True;
          if Owner.Fields[N].DataType in StringTypes then
            case aDecodeString of
              dsNone: aValue:= FieldValue[N];
              dsCodePage: aValue:= GetFieldNativeValue(N);
              dsCaption: aValue:= FieldText(N);
            end
          else
              aValue:= FieldValue[N];
        end
      else
        begin
          {$IFDEF DEBUG}
          if Assigned(Owner.TableMeta) and (Length(Owner.TableMeta.Table) <> 0) then
            DebugLog('TCacheItem.FieldValueRecurce(%s, %s) not found in %s ...', [QuotedStr(aName), VariantToString(aValue), Owner.TableMeta.Table])
          else
            DebugLog('TCacheItem.FieldValueRecurce(%s, %s) not found ...', [QuotedStr(aName), VariantToString(aValue)]);
          {$ENDIF}
        end;
    end
  else
    begin
      N:=Owner.Fields.IndexByName( Trim(Copy(aName,1,P-1)) );
      if (-1 < N) then
        if (0 < Owner.Fields[N].Link )then
          begin
            LookupKey := FieldValue[N];
            if not VarIsNull(LookupKey) and assigned(Owner.Fields[N].Lookup) then
              begin
                CI := TDataCache(Owner.Fields[N].Lookup).FindByID(LookupKey);
                if not (CI = nil) then Result := CI.FieldValueRecurce( Trim(Copy(aName,P+1,MaxInt)) , aValue)
                                  else Result := True;
              end
            else
                Result:=True; // ссылка пуста, возвращаем null
          end;
    end;
end;

function TCacheItem.GetClipBoardValue(const FieldIndex: Integer): string;
var
  V: Variant;
  I: Integer;
  P: Pointer;
  S: TMemoryStream;
  T: TStringStream;
  E: TBase64Encoding;
begin
  if Owner.Fields[FieldIndex].DataType in BinaryTypes then
    begin
      V := FieldValue[FieldIndex];
      if VarIsArray(V) then
        begin
          // Кодируем в BASE64 строки ...
          I := VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1);
          S := TMemoryStream.Create;
          try
            P := VarArrayLock(V);
            try
              S.WriteBuffer(P^, I);
            finally
              VarArrayUnlock(V);
            end;
            S.Position := 0;
            T := TStringStream.Create;
            try
              E := TBase64Encoding.Create(0, #13);
              try
                E.Encode(S, T);
              finally
                E.Free;
              end;
              Result := T.DataString;
            finally
              T.Free;
            end;
          finally
            S.Free;
          end;
        end
      else
        Result := EmptyStr
    end
  else
    if VarIsEmpty(FieldBaseValue[FieldIndex]) then
      Result := UnassignedStr
    else if VarIsNull(FieldBaseValue[FieldIndex]) then
      Result :=  NullStr
    else
      Result := VarToStr(FieldValue[FieldIndex]);
end;

procedure TCacheItem.SetClipBoardValue(const FieldIndex: Integer; const Value: string);
var
  V : Variant;
  A : Variant;
  P : Pointer;
  I : Integer;
  S: TStringStream;
  T: TMemoryStream;
  E: TBase64Encoding;
begin
  if not Owner.Fields[FieldIndex].IsLookup then
  begin
    if CompareStr(Value, UnassignedStr) = 0 then
      FieldValue[FieldIndex] := Null //Unassigned
    else if CompareStr(Value, NullStr) = 0 then
      FieldValue[FieldIndex] := Null
    else
    begin
      V := Value;

      case Owner.Fields[FieldIndex].DataType of
        ftString:   FieldValue[FieldIndex] := V;
        ftBlob:
          begin
            // Декодируем из BASE64 строки ...
            I := Length(Value);
            if I <> 0 then
              begin
                T := TMemoryStream.Create;
                try
                  S := TStringStream.Create;
                  try
                    S.WriteString(Value);
                    S.Position := 0;
                    E := TBase64Encoding.Create(0, #13);
                    try
                      E.Decode(S, T);
                    finally
                      E.Free;
                    end;
                  finally
                    S.Free;
                  end;
                  A := VarArrayCreate([0, T.Size], varByte);
                  P := VarArrayLock(A);
                  try
                    Move(T.Memory^, P^, T.Size);
                  finally
                    VarArrayUnlock(A);
                  end;
                finally
                  T.Free;
                end;
              end
            else
              A := Null;
            FieldValue[FieldIndex] := A;
          end;
        ftMemo:     FieldValue[FieldIndex] := V;
        ftWideMemo: FieldValue[FieldIndex] := V;
        ftSmallInt: FieldValue[FieldIndex] := VarAsType(V, varSmallInt);
        ftInteger:  FieldValue[FieldIndex] :=
                  VarAsType(V, varInteger);//VarToInt(V);
        ftFloat:    FieldValue[FieldIndex] := VarAsType(V, varDouble);
        ftTime:     FieldValue[FieldIndex] := VarAsType(V, varDate);
        ftDate, ftDateTime, ftTimeStamp, ftTimeStampOffset, ftOraTimeStamp:
                    FieldValue[FieldIndex] := VarAsType(V, varDate);
        ftBoolean:  FieldValue[FieldIndex] := VarAsType(V, varBoolean);
        ftBCD:      begin
                      if Owner.Fields[FieldIndex].DataSize=0 then FieldValue[FieldIndex] := VarAsType(V, varInteger)
                                                             else FieldValue[FieldIndex] := VarAsType(V, varDouble);
                    end;
        ftAutoInc:  FieldValue[FieldIndex] := VarAsType(V, varInteger);
      end;
    end;
  end;
end;

function TCacheItem.GetID: Variant;
begin
  if Owner.IDIndex = -1 then
    begin
      result := Null;
      Exit;
    end;
  if (isNotInitialized in State) or (Not (isInserted in State) and (Stage < Owner.BaseStage)) then
    begin
      if (Stage < Owner.Fields[Owner.IDIndex].Stage) or (isNotInitialized in FState) then
        FillItem(Owner.BaseStage);
      Result := FFields[Owner.IDIndex];
    end
  else
    begin
      Result := FFields[Owner.IDIndex];
      if VarType(Result) = (varArray or varVariant) then Result := TDeFieldValue(Result)[1];
    end;
end;

function TCacheItem.GetFieldValue(const Index: Integer): Variant;
var pFM         : Pointer; // TFieldMeta
    V,LookupKey : Variant;
    pLookupItem : Pointer; // TCacheItem;
    i,N         : Integer;
    isLoaded    : Boolean;
    FieldCache  : TFieldCache;
begin
  if (Index < 0) or (Length(FFields) <= index) then Exit(Unassigned);

  pFM := Owner.Fields.Items[index];

  if Not TFieldMeta(pFM).PolicySelect then
    begin
      result:=null;
      exit;
    end;

  if (Stage < TFieldMeta(pFM).Stage) then
    begin
      if (State * [isNotInitialized, isInserted] = []) then
        if Owner.Fields[Index].UpdateStats then
          begin
            {$IFDEF DeDEBUG}
            Funcs.WriteLog(Format('Update statistic for ''%s'' ...', [TFieldMeta(pFM).Original]));
            {$ENDIF}
            if Owner.FSubDatasets.Add(Index, Stage) <> -1 then
              begin
                {$IFDEF DEBUG}
                Owner.DebugFieldsLog(Format('Update statistic for %d ...', [Index]), Owner.Fields);
                {$ENDIF}
              end;
          end;
    end;

  if VarIsEmpty(FFields[Index]) and Assigned(Owner) and not (isInserted in State) then
    begin
      FieldCache := Owner.Fields[Index];
      // Пытаемся получить данные из подзапроса, загруженного ранее в TFieldCache ...
      if Assigned(FieldCache) and (FieldCache.RowCount <> 0) then
        begin
          N := FieldCache.IndexByRowID(ID);
          if N <> -1 then
            begin
              FFields[Index] := FieldCache.GetRowValue(N, FieldCache.FieldIndex) ;
            end;
        end;
    end;

  if (isNotInitialized in State) or
     (TFieldMeta(pFM).IsStored and (Stage < TFieldMeta(pFM).Stage) and not ((isInserted in State) or TFieldMeta(pFM).IsLookup)) then
        begin
          {$IFDEF DeDEBUG}
          if not (isNotInitialized in State) then
            if Assigned(Owner) and Assigned(Owner.TableMeta) then
              Funcs.WriteLog(Format('Change table ''%s'' field ''%s'' stage %s for record %d ask %s ...',
                [ Owner.TableMeta.Table, TFieldMeta(pFM).Original, StrPas(FieldStages[TFieldMeta(pFM).Stage]),
                  FSourceIndex, StrPas(FieldStages[Stage]) ]))
            else
              Funcs.WriteLog(Format('Change table field ''%s'' stage %s for record %d ask %s ...',
                [ TFieldMeta(pFM).Original, StrPas(FieldStages[TFieldMeta(pFM).Stage]),
                  FSourceIndex, StrPas(FieldStages[Stage]) ]));
          {$ENDIF}
          {$IFDEF DEBUG}
          if Assigned(Owner) then
            begin
              if TFieldMeta(pFM).Stage > Owner.BaseStage then
                DebugLog('%s.GetFieldValue(%d) ...', [ClassName, Index]);
            end
          else
            if TFieldMeta(pFM).Stage > fsBase then
              DebugLog('%s.GetFieldValue(%d) ...', [ClassName, Index]);
          {$ENDIF}

          FillItem(TFieldMeta(pFM).Stage);
        end;

  if VarIsEmpty(FFields[Index]) and (not(isInserted in State)) then
    begin

      if TFieldMeta(pFM).Calculated and (not TFieldMeta(pFM).IsStored) then
        try
          // Если циклическая ссылка, то не рассчитываем значение!!!
          if Owner.Fields[Index].Calculating then
            FFields[Index] := Unassigned
          else
            begin
              Owner.Fields[Index].Calculating := True;
              try
                V:= Calculate(TFieldMeta(pFM).DefaultPostfix, Null);
                with Owner.TableMeta.Fields[index] do
                  if (DataType in StringTypes) and (CodePage = cpUTF8) and (Not (VarIsEmpty(V) or VarIsNull(V)) )
                    then FFields[Index]:= WideStringToUnicode(V)
                    else FFields[Index]:= V;
              finally
                Owner.Fields[Index].Calculating := False;
              end;
            end;
        except
          FFields[Index] := Null;
        end
      else
        if TFieldMeta(pFM).IsLookup then
           begin

             LookupKey := FieldValue[Owner.Fields[Index].PairIndex{Owner.FLookupPairIndex[index]}];

             if VarIsNull(LookupKey) or VarIsEmpty(LookupKey) then
               begin
                 FFields[Index]:= Null;
               end
             else
               if Assigned(TFieldMeta(pFM).Owner.NField) and (TFieldMeta(pFM).Owner.NField.ID = TFieldMeta(pFM).Owner.KField[0].ID) then
                 begin
                   FFields[Index]:= LookupKey
                 end
               else
                 begin
                   pLookupItem := TDataCache(TFieldMeta(pFM).Lookup).FindByID(LookupKey);
                   if Assigned(TCacheItem(pLookupItem)) then
                     FFields[Index] := TCacheItem(pLookupItem).CaptionValue
                   else
                     FFields[Index] := Null;
                 end;
           end
        else
           FFields[Index] := Null;

      Result := FFields[Index];
    end
  else
    if VarType(FFields[Index]) = (varArray or varVariant) then
      Result := TDeFieldValue(FFields[Index])[1]
    else
      Result := FFields[Index];
  // Запомним время последнего обращения к значению поля ...
  Owner.Fields[Index].FAccessDateTime := Now;
end;

function TCacheItem.FieldIsNewValue(const Index: Integer): Boolean;
begin
  if (isNotInitialized in State) then
    Result := False
  else
    begin
      if (Index < 0) or (Length(FFields) <= Index) then
        raise ERangeError.CreateResFmt(@sErrorOutOfBoundsFmt, [Index, 0, Length(FFields)]);
      {
      if VarIsEmpty(FFields[Index]) then
        Result:=False
      else {}
        Result := (VarType(FFields[Index]) = (varArray or varVariant));
    end;
end;

function TCacheItem.KeyAsQuotedStr: string;
var Index  : Integer;
    KField : TFieldMeta;
    KValue : string;
begin
  Result := EmptyStr;
  for Index := 0 to Pred(Owner.TableMeta.KeyCount) do
    begin
      if Length(Result) <> 0 then Result := Result + ',';

      KField := Owner.TableMeta.KField[Index];
      KValue := VarAsType(ValueByName[KField.Original], varString);
      Result := Result + AnsiQuotedStr(KValue, '"');
    end;
end;

type
  PVarsPair = ^TVarsPair;
  TVarsPair = packed record
    LowVar  : Variant;
    HighVar : Variant;
  end;

procedure TCacheItem.BeginInit;
begin
  Include(FState, isInitializing);
end;

procedure TCacheItem.EndInit;
begin
  Exclude(FState, isInitializing);
end;

procedure TCacheItem.FillItem(const aStage: TFieldStage);
begin
  Owner.FillItem(self, aStage);
end;

procedure TCacheItem.InitFieldValue(const Index: integer; const Value: Variant);
//var
//  S: AnsiString;
//  P: Pointer;
begin
  {
  if (index < 0) or (Length(FFields) <= index) then
    raise Exception.Create(ErrorOutOfBounds);

  if not (isInitializing in State) then Exit;
  {}
  { *** Не работает как надо :(
  if VarIsArray(Value) then
    begin
      SetLength(S, VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1));
      P := VarArrayLock(Value);
      try
        Move(P^, S[1], Length(S));
      finally
        VarArrayUnlock(Value);
      end;
      FFields[index] := S;
    end else
  }
  if (isInitializing in State) or (isInserted in State) then
    if VarType(Value) = varDate then
      FFields[Index] := RecodeMilliSecond(Value, 0)
    else
      FFields[Index] := Value;
end;

function TCacheItem.InitFieldValueByNameExternal(const aName: string; const Value: Variant): Integer;
var
  FieldIndex: Integer;
  procedure CheckField(FieldMeta: TFieldMeta);
  var
    Index, Position: Integer;
    PostfixName: string;
    Value: Variant;
  begin
    if Assigned(FieldMeta) and FieldMeta.IsStored and not FieldMeta.Key then
      if Assigned(FieldMeta.DefaultPostfix) and (FieldMeta.DefaultPostfix.Count <> 0) then
        for Index := 0 to Pred(FieldMeta.DefaultPostfix.Count) do
          if FieldMeta.DefaultPostfix[Index].ItemType = piIdent then
            begin
              PostfixName := FieldMeta.DefaultPostfix[Index].Ident;
              Position := Pos('.', PostfixName);
              if Position = 0 then
                begin
                  if SameText(aName, PostfixName) then
                    begin
                      Value := Calculate(FieldMeta.DefaultPostfix, FFields[FieldIndex]);
                      if VarType(Value) = varDate then
                        FFields[FieldIndex] := RecodeMilliSecond(Value, 0)
                      else
                        FFields[FieldIndex] := Value;
                    end;
                end
              else
                if SameText(aName, Copy(PostfixName, 1, Pred(Position))) then
                  begin
                    Value := Calculate(FieldMeta.DefaultPostfix, FFields[FieldIndex]);
                    if VarType(Value) = varDate then
                      FFields[FieldIndex] := RecodeMilliSecond(Value, 0)
                    else
                      FFields[FieldIndex] := Value;
                  end;
            end;
  end;
begin
  if isInitializing in State then
    begin
      Result := Owner.TableMeta.Fields.IndexByName(aName);
      if Result <> -1 then
        begin
          if (Owner.TableMeta.Fields[Result].DataType in StringTypes) and (Owner.TableMeta.Fields[Result].CodePage = cpUTF8)
            then FFields[Result]:= WideStringToUnicode(Value)
            else FFields[Result]:= Value;

          // Если вставка записи, то ...
          if isInserted in State then
            // Проверим связки для Default значений ...
            for FieldIndex := 0 to Pred(Owner.TableMeta.Fields.Count) do
              if FieldIndex <> Result then
                CheckField(Owner.TableMeta.Fields[FieldIndex]);
        end;
    end
  else
    Result:=-1;
end;

procedure TCacheItem.SetFieldValue(const Index: Integer; const Value: Variant);
var
  BaseValue : Variant;
  i : integer;
  LookupPairIndex : integer;
//  oldValue,newValue : string;
begin
  if (index < 0) or (Length(FFields) <= index) then
    raise ERangeError.CreateResFmt(@sErrorOutOfBoundsFmt, [Index, 0, Length(FFields)]);

  if (isNotInitialized in State) and (not (isInitializing in State)) then
    Owner.FillItems(FSourceIndex, FSourceIndex, Owner.Fields[index].Stage );

  if ((Owner.Fields[index].IsReadOnly)
    and(not((isNotInitialized in State)or(isInserted in State)))) then exit;

  if VarIsEmpty(FFields[Index]) then
  begin
    FFields[Index] := Value;
    //вставка ID в поле, соответствующее Lookup полю
    if Owner.Fields[index].IsLookup then
      if Not (VarIsEmpty(Value) or VarIsNull(Value)) then
        begin
          LookupPairIndex := -1;
          // ищем и сравниваем учитывая регистр
          for i:=0 to TDataCache(Owner.Fields[index].Lookup).Count-1 do
            if AnsiCompareStr(TDataCache(Owner.Fields[index].Lookup).Items[i].Caption, Value) = 0 then
              begin
                LookupPairIndex := Owner.Fields.IndexByName(Owner.Fields[index].LookupPair.Original);
                FieldValue[LookupPairIndex] := TDataCache(Owner.Fields[index].Lookup).Items[i].ID;
                Break;
              end;

          if LookupPairIndex = -1 then
          // ищем и сравниваем НЕ учитывая регистр
          for i:=0 to TDataCache(Owner.Fields[index].Lookup).Count-1 do
            if AnsiCompareText(TDataCache(Owner.Fields[index].Lookup).Items[i].Caption, Value) = 0 then
              begin
                LookupPairIndex := Owner.Fields.IndexByName(Owner.Fields[index].LookupPair.Original);
                FieldValue[LookupPairIndex] := TDataCache(Owner.Fields[index].Lookup).Items[i].ID;
                Break;
              end;
        end;
  end
  else
  begin
    if (VarType(FFields[Index]) <> (varArray or varVariant)) then
      begin
        if (VarType(FFields[Index]) = VarType(Value))or(VarNull = VarType(Value))then
          begin
            if Owner.Fields[Index].DataType in (BinaryTypes + [ftMemo, ftWideMemo]) then
              begin
                if DeVarSameValue(FFields[Index], Value) then
                  Exit;
              end
            else
              if VarSameValue(FFields[Index], Value) then
                Exit;
          end;
        BaseValue := FFields[Index];
        FFields[Index] := VarArrayCreate([0, 1], varVariant);
        VarArrayPut(FFields[Index], BaseValue, [0]);
      end
    else
      if (VarType(VarArrayGet(FFields[Index], [1])) = VarType(Value)) and VarSameValue(VarArrayGet(FFields[Index], [1]), Value) then
        Exit;
    if VarType(Value) = varDate then
      VarArrayPut(FFields[Index], RecodeMilliSecond(Value, 0), [1])
    else
      VarArrayPut(FFields[Index], Value, [1]);
  end;

  if not (isNotInitialized in State) then
    Include(FState,isModified);
end;

function TCacheItem.GetFlagEdit: Boolean;
begin
  Result:= isFlagEdit in FState;
end;

function TCacheItem.GetFlagNull: Boolean;
begin
  Result:= isFlagNull in FState;
end;

function TCacheItem.GetFlagSkip: Boolean;
begin
  Result:= isFlagSkip in FState;
end;

procedure TCacheItem.SetFlagEdit(const Value: Boolean);
begin
  if Value then Include(FState, isFlagEdit)
           else Exclude(FState, isFlagEdit);
end;

procedure TCacheItem.SetFlagNull(const Value: Boolean);
begin
  if Value then Include(FState, isFlagNull)
           else Exclude(FState, isFlagNull);
end;

procedure TCacheItem.SetFlagSkip(const Value: Boolean);
begin
  if Value then Include(FState, isFlagSkip)
           else Exclude(FState, isFlagSkip);
end;

function TCacheItem.GetFieldNativeValue(const Index: Integer): Variant;
begin
  result:= GetFieldValue(index);
  with Owner.TableMeta.Fields[index] do
    if (DataType in StringTypes) and (CodePage = cpUTF8) and (Not (VarIsEmpty(result) or VarIsNull(result)) )
      then Result:= UTF8ToString(RawByteString(Result));
end;

procedure TCacheItem.SetFieldNativeValue(const Index: Integer; const Value: Variant);
begin
  with Owner.TableMeta.Fields[index] do
    if (DataType in StringTypes) and (CodePage = cpUTF8) and (Not (VarIsEmpty(Value) or VarIsNull(Value)) )
      then FieldValue[Index]:= WideStringToUnicode(Value)
      else FieldValue[Index]:= Value;
end;

function TCacheItem.GetNativeValueByName(const aName : string): Variant;
var n: Integer;
begin
  n:= FOwner.Fields.IndexByName(aName);
  if n>-1 then
    Result:= GetFieldNativeValue(n)
  else
    Result:= EmptyStr;
end;

procedure TCacheItem.SetNativeValueByName(const aName : string; const Value: Variant);
var n: Integer;
begin
  n:= FOwner.Fields.IndexByName(aName);
  if n>-1 then
    SetFieldNativeValue(n, Value)
end;

function TCacheItem.GetFieldTextByName(const aName : string): String;
var n: Integer;
begin
  n:= FOwner.Fields.IndexByName(aName);
  if n>-1 then Result:= FieldText(n)
          else Result:= EmptyStr;
end;

function TCacheItem.FieldText(const Index: Integer): string;
var Field       : TFieldMeta;
    IntValue    : int64;
    DoubleValue : double;
    FldValue    : Variant;
    i,L         : Integer;
begin
  result := EmptyStr;
  FldValue := FieldValue[Index];   //<< ВСЕ ПРОВЕРКИ ЗДЕСЬ
  Field := Owner.Fields[Index];

  if (Field.ShowType = stYesNo) then
    if Length(Field.Value1) = 0 then
      if Length(Field.Value2) = 0 then
        // ДА/НЕТ не указан: если пусто, то ...
        if VarType(FldValue) in [varNull, varEmpty] then
          Result := EmptyStr
        else
          if Field.DataType = ftBoolean
            then if FldValue = True then Result:= CheckText
                                    else Result:= EmptyStr
            else Result := CheckText
      else
        // НЕТ не указан: если это ДА, то ...
        if DeVarSameValue(FldValue, Field.VariantValue2) then
          Result := CheckText
        else
          Result := BadCheckText
    else
      if Length(Field.Value2) = 0 then
        // ДА не указан: если это НЕТ, то ...
        if DeVarSameValue(FldValue, Field.VariantValue1) then
          Result := EmptyStr
        else
          Result := BadCheckText
      else
        // ДА/НЕТ указан: если это ДА, то ...
        if DeVarSameValue(FldValue, Field.VariantValue2) then
          Result := CheckText
        else
          // Если это НЕТ, то ...
          if DeVarSameValue(FldValue, Field.VariantValue1) then
            Result := EmptyStr
          else
            Result := BadCheckText
  else
    begin
      if (VarIsEmpty(FldValue) or VarIsNull(FldValue)) then
        begin
          Result := EmptyStr;
          Exit;
        end;

      if Field.DataType in StringTypes then
        begin
          if Field.CodePage = cpUTF8 then FldValue:= UTF8ToString(RawByteString(FldValue))
                                     else FldValue:= VarToStr(FldValue);
          case Field.ShowType of
            stTranslate:
              result:= GetTitle(FldValue);
            stPassword, stEncryptedPassword:
              result := '******';
            else
              begin
                Result := FldValue;
                L:=Length(Result);
                I:=L;
                while (0<i) and (Result[i]=' ') do Dec(i);
                if I<L then
                  Result:=Copy(Result,1,I);
              end;
          end;
          //DONE: Убран шаблон для вывода строковых поле
          {
          if Length(Field.Template) > 0 then
            try
              result := Format(Field.Template, [result]);
            except
            end;
          {}
        end else

      if Field.DataType in FloatTypes then
        begin
          DoubleValue := VarAsType(FldValue, varDouble);
          try
            if Length(Field.Template) > 0 then
              begin
                if Pos('#', Field.Template) <> 0 then
                  Result := FormatFloat(Field.Template, DoubleValue)
                else
                   Result := Format(Field.Template, [DoubleValue])
              end
            else
              Result := FloatToStr(Double(FldValue));
          except
            on E: Exception do
              begin
                {$IFDEF DEBUG}
                DebugLog('TCacheItem.GetFieldText(%d) float error: %s', [Index, E.Message]);
                {$ENDIF}
                Result := FloatToStr(Double(FldValue));
              end;
          end;
        end else

      if Field.DataType in IntegerTypes then
        begin
          IntValue := VarToInt(FldValue);
          try
            if Length(Field.Template) = 0 then   Result:= IntToStr(IntValue) else
            if Pos('#', Field.Template) = 0 then Result := Format(Field.Template, [IntValue]) else
                                                 Result := FormatFloat(Field.Template, IntValue);
          except
            on E: Exception do
              begin
                {$IFDEF DEBUG}
                DebugLog('TCacheItem.GetFieldText(%d) integer error: %s', [Index, E.Message]);
                {$ENDIF}
                Result:= IntToStr(IntValue);
              end;
          end;
        end else

      if Field.DataType in DateTimeTypes then
        begin
          DoubleValue := VarAsType(FldValue, varDate);
          if Length(Field.Template) > 0 then
            begin
              result := FormatDateTime(Field.Template, DoubleValue);
            end
          else
            if Field.DataType = ftDate then
              result := DateToStr(DoubleValue)
            else
            if Field.DataType = ftTime then
              result := TimeToStr(DoubleValue)
            else
            if Field.DataType = ftDateTime then
              result := DateTimeToStr(DoubleValue)
            else
            if Field.DataType = ftTimeStamp then
              result := DateTimeToStr(DoubleValue)
            else
            if Field.DataType = ftTimeStampOffset then
              result := DateTimeToStr(DoubleValue)
            else
            if Field.DataType = ftOraTimeStamp then
              result := DateTimeToStr(DoubleValue);
        end else

      if Field.DataType = ftGuid then
        Result := VarToStr(FldValue)
      else

      if Field.DataType = ftBytes then
        begin
          Result:= EmptyStr;
          if VarIsArray(FldValue) then
            for i := VarArrayLowBound(FldValue, 1) to VarArrayHighBound(FldValue, 1)  do
              Result:= Result + IntToHex(FldValue[i],2);
        end else

      if Field.DataType in BinaryTypes then
        begin
          if VarToStr(FldValue) = EmptyStr then
            result := EmptyStr
          else
            result := CheckText;
        end else

      if Field.DataType = ftBoolean then
        begin
          if VarIsArray(FldValue) then
            result := EmptyStr
          else
            result:= VarToStr(FldValue);
        end

      else
        begin
          result:= VarToStr(FldValue);
          {$IFDEF DeDEBUG}
          if Assigned(Field.Owner) and (Length(Field.Owner.Table) <> 0) and (Length(Field.Original) <> 0) then
            Funcs.WriteLog('Field ''%s.%s'' unknown type code %d for get as text!', [Field.Owner.Table, Field.Original, Ord(Field.DataType)], False, 'databases')
          else
            Funcs.WriteLog('Field ''%s'' unknown type code %d for convert as text!', [Field.Original, Ord(Field.DataType)], False, 'databases');
          {$ENDIF}
        end;
    end;
end;

procedure TCacheItem.SetID(aID : Variant);
var PreID: Variant;
begin
  if ID = aID then Exit;

  if (isInserted in State)
    then
      begin
        PreID:= FFields[Owner.IDIndex];
        if VarType(PreID) = (varArray or varVariant) then PreID:= TDeFieldValue(PreID)[0];

        FFields[Owner.IDIndex]:= VarArrayCreate([0, 1], varVariant);
        VarArrayPut(FFields[Owner.IDIndex], PreID, [0]);
        VarArrayPut(FFields[Owner.IDIndex], aID, [1]);
      end
    else raise Exception.Create('Can''not set TCacheItem.ID');
end;

function TCacheItem.GetCaption: String;
begin
  if Owner.CaptionIndex = -1 then result:= EmptyStr
                             else result:= FieldText(Owner.CaptionIndex);
end;

function TCacheItem.ItemTitle(aSeparator: String = SplitText): String;
var i: Integer;
    s: String;
begin
  result:= EmptyStr;
  for i:=0 to Pred(Owner.Fields.Count) do
    if (Not Owner.Fields[i].IsLookup) and ((i = Owner.CaptionIndex) or (Owner.Fields[i].VisibleLevel >= fvLevel3)) then
      begin
        s:= Trim(FieldText(i));
        if 0 < Length(s) then
          Result:= Result + aSeparator + FieldText(i);
      end;
end;

function TCacheItem.GetCaptionValue: Variant;
begin
  if Owner.CaptionIndex = -1 then result:= null
                             else result:= FieldValue[Owner.CaptionIndex];
end;

function TCacheItem.GetFieldBaseValue(const Index: Integer): Variant;
begin
  if (index < 0) or (Length(FFields) <= index) then
    raise ERangeError.CreateResFmt(@sErrorOutOfBoundsFmt, [Index, 0, Length(FFields)]);

  // 13.01.2016 * Заменил полностью логику для корректного получения базовых значений.
  if VarType(FFields[Index]) = (varArray or varVariant) then
    Result := TDeFieldValue(FFields[Index])[0]
  else
    Result := FieldValue[Index]; // ЗДЕСЬ ВСЯ ЛОГИКА НАЧИТЫВАНИЯ ЗНАЧЕНИЯ ДЛЯ НЕИНИЦИАЛИЗИРОВАННОГО УРОВНЯ ПОЛЕЙ!
  // 13.01.2016 -
end;

function TCacheItem.GetImageIndex(const aIndex: Integer): Integer;
var IVAlue        : Variant;
    LookupKey     : Variant;
    LookupItem    : TCacheItem;
    LinkedTable   : TTableMeta;
    LinkedTableID : integer;
begin
  if FImageIndex >= 0 then
  begin
      if (aIndex=0)and(FImageIndex=1) then
        result := 0
      else
    result := FImageIndex;
    Exit;
  end;

  if Owner.TableMeta.IsInterrelations then
  begin
    FImageIndex := Owner.TableMeta.ICO;
    LinkedTableID := VarToInt(ValueByName[fldIRChildTable]);
    if LinkedTableID = Owner.TableMeta.ID then
      LinkedTableID := VarToInt(ValueByName[fldIRParentTable]);
    LinkedTable := MetaData.GetTableMeta(LinkedTableID);
    if Assigned(LinkedTable) then
      FImageIndex := LinkedTable.ICO;
  end
  else if Assigned(Owner.TableMeta) then
  begin
    if not Assigned(Owner.TableMeta.IconField)then
      FImageIndex := Owner.TableMeta.ICO
    else
    if Owner.TableMeta.IconField.Link > 0 then
    begin
      LookupKey := FieldValue[Owner.Fields.IndexByID(Owner.TableMeta.IconField.ID)];
      if Assigned(Owner.TableMeta.IconField.LookupPair.Lookup)then
         LookupItem := TDataCache(Owner.TableMeta.IconField.LookupPair.Lookup).FindByID(LookupKey)
      else
         LookupItem := nil;

      if Assigned(LookupItem) then
         FImageIndex := LookupItem.ImageIndex
      else
         FImageIndex:=Owner.TableMeta.ICO;
    end
    else
    begin
      IVAlue := ValueByName[Owner.TableMeta.IconField.Original];
      if not (VarIsEmpty(IVAlue) or VarIsNull(IVAlue)) then
      try
        FImageIndex := VarToInt(IVAlue);
      except
        on E: Exception do
          begin
            {$IFDEF DEBUG}
            DebugLog('TCacheItem.GetImageIndex(%d) error: %s', [aIndex, E.Message]);
            {$ENDIF}
            FImageIndex := Owner.TableMeta.ICO
          end;
      end
      else
        FImageIndex:=Owner.TableMeta.ICO;
    end;
    if Owner.FolderSignIndex >= 0 then
      if CompareStr(VarToStr(FieldValue[Owner.FolderSignIndex]),
        Owner.TableMeta.GField.Value1) = 0 then
          FImageIndex := aIndex;
    if Owner.DeletedSignIndex >= 0 then
      if CompareStr(VarToStr(FieldValue[Owner.DeletedSignIndex]),
        Owner.TableMeta.DField.Value2) = 0 then
          begin
            if 0 < (FImageIndex and $000000FF) then FImageIndex := (FImageIndex and $FFFFF0FF) or $00000300;
            if 0 < (FImageIndex and $00FF0000) then FImageIndex := (FImageIndex and $F0FFFFFF) or $03000000;
          end;
  end
  else
    FImageIndex := -2;
  result := FImageIndex;
end;

function TCacheItem.GetMapImageIndex: Integer;
begin
  if FMapImageIndex < 0 then FMapImageIndex:= DM.MapIconIndex(ImageIndex);
  if FMapImageIndex < 0 then FMapImageIndex:= DM.MapIconIndex(Owner.TableMeta.Ico);
  if FMapImageIndex < 0 then FMapImageIndex:= DM.MapIconIndex(225);

  Result:= FMapImageIndex;
end;

function TCacheItem.GetFontColor : TColor;
var VVAlue      : Variant;
    LookupKey   : Variant;
    LookupItem  : TCacheItem;
begin
  if not (FFontColor = unassigned) then
  begin
    result := FFontColor;
    Exit;
  end;

  FFontColor := clWindowText;
  Result := FFontColor;

  if not Assigned(Owner.TableMeta) then Exit;
  if not Assigned(Owner.TableMeta.ColorField) then
    begin
      if Owner.DeletedSignIndex >= 0 then
        if (Owner.TableMeta.DField.DataType in DateTypes) then
          begin
            if not VarIsNull(FieldValue[Owner.DeletedSignIndex]) then FFontColor:= DeletedRecordColor;
          end else
        if CompareStr(VarToStr(FieldValue[Owner.DeletedSignIndex]), Owner.TableMeta.DField.Value2) = 0 then
            FFontColor := DeletedRecordColor;
            {
              // Расчёт цвета удалённой записи вынесен в инициализацию модуля и расчёт происходит один раз при запуске приложения!
                    RGB(
              (GetRValue(ColorToRGB(clWindowText))+GetRValue(ColorToRGB(clWindow))) div 2,
              (GetGValue(ColorToRGB(clWindowText))+GetGValue(ColorToRGB(clWindow))) div 2,
              (GetBValue(ColorToRGB(clWindowText))+GetBValue(ColorToRGB(clWindow))) div 2
                       );
            }
    end
  else
    begin
      if (Owner.TableMeta.ColorField.Link > 0) then
        begin
          LookupKey := FieldValue[Owner.Fields.IndexByID(Owner.TableMeta.ColorField.ID)];
          if Assigned(Owner.TableMeta.ColorField.LookupPair) then
            begin
              LookupItem := TDataCache(Owner.TableMeta.ColorField.LookupPair.Lookup).FindByID(LookupKey)
            end
          else
            LookupItem := nil;
          if Assigned(LookupItem) then
            FFontColor := LookupItem.FontColor;
        end
      else
        begin
          VVAlue := ValueByName[Owner.TableMeta.ColorField.Original];
          if not (VarIsEmpty(VVAlue) or VarIsNull(VVAlue)) then
          try
            FFontColor := StrToIntDef(VarToStr(VVAlue),clWindowText);
          except
            on E: Exception do
              begin
                {$IFDEF DEBUG}
                DebugLog('TCacheItem.GetFontColor error: ' + E.Message);
                {$ENDIF}
                FFontColor := clWindowText;
              end;
          end;
        end;
  end;
  Result:=FFontColor;
end;

function TCacheItem.GetHint: string;
var
  Index, FieldIndex: Integer;
  Value: string;
begin
  Result := EmptyStr;
  if Assigned(FOwner) and Assigned(FOwner.Fields) then
    for Index := 0 to Pred(FOwner.Fields.Count) do
     if (FOwner.Fields[Index].VisibleLevel >= fvLevel2) and not FOwner.Fields[Index].IsLookup then
        begin
          if Assigned(FOwner.Fields[Index].LookupPair) then
            FieldIndex := FOwner.Fields.IndexByID(FOwner.Fields[Index].LookupPair.ID)
          else
            FieldIndex := Index;

          Value := ReduceCaption(FieldText(FieldIndex), -255);
          if Length(Value) <> 0 then
            StrAdd(Result, #10, sp + FOwner.Fields[FieldIndex].Native + ': ' + Value + sp);
        end;
end;

procedure TCacheItem.Restore;
var
  I: Integer;
  BaseValue: Variant;
begin
  for I := 0 to Pred(Owner.FieldCount) do
  begin
    BaseValue := Self.FieldBaseValue[I];
//    if VarType(FFields[I]) = (varArray or varVariant) then
//      VarArrayRedim(FFields[i],0);
    FFields[I] := BaseValue;
  end;
  Exclude(FState, isDeleted);
  Exclude(FState, isModified);
end;

procedure TCacheItem.ApplyChanges;
var
  I: Integer;
begin
  for I := 0 to Pred(Owner.FieldCount) do
    begin
      if VarType(FFields[I]) = (varArray or varVariant) then
        FFields[I] := TDeFieldValue(FFields[I])[1];
    end;
end;

procedure TCacheItem.RollbackChanges;
var
  I: Integer;
begin
  for I := 0 to Pred(Owner.FieldCount) do
    begin
      if VarType(FFields[I]) = (varArray or varVariant) then
        FFields[I] := TDeFieldValue(FFields[I])[0];
    end;
  Exclude(FState, isModified);
end;

function TCacheItem.GetFirstChild: TCacheItem;
var i  : integer;
    CI : TCacheItem;
    V  : Variant;
begin
  Result := nil;
  V      := Self.ID;
  for i:=0 to Owner.Count -1 do
    begin
      CI:=Owner.Items[i];
      if not(isNotInitialized in CI.State)
         and ( ((Self = Owner.RootItem)and(VarIsZero(CI.ParentID))) or VarSameValue(CI.ParentID,V) )
      then
        begin
          Result := CI;
          Break;
        end;
    end;
end;

function TCacheItem.GetNextChild(CurrentChild : TCacheItem) : TCacheItem;
var i : integer;
    CI : TCacheItem;
begin
  if not Assigned(CurrentChild) then
    Exit(GetFirstChild);

  result := nil;
  for i:= Owner.IndexByItem(CurrentChild)+1 to Owner.Count -1 do
    begin
      CI:= Owner.Items[i];
      if not(isNotInitialized in CI.State)
         and( ((Self = Owner.RootItem)and(VarIsZero(CI.ParentID)))or VarSameValue(CI.ParentID,Self.ID) )
      then Exit(CI);
    end;
end;

function TCacheItem.IsParent(ParentIDValue : Variant): Boolean;
var ParentItem : TCacheItem;
begin
  result := False;
  if (Self = Owner.RootItem)
  or (VarSameValue(Self.ID,ParentIDValue))
  or (VarSameValue(Self.ID,Self.ParentID))
  then Exit;
  if (VarSameValue(Self.ParentID,ParentIDValue))
  or (VarSameValue(Self.ParentID,0) and VarIsNull(ParentIDValue))then
    result := true
  else
  begin
    ParentItem := Owner.FindById(Self.ParentID);
    if Assigned(ParentItem) then
      result := ParentItem.IsParent(ParentIDValue);
  end;
end;

procedure TCacheItem.Assign(aItem: TObject);
var
  Index: Integer;
begin
  if Assigned(aItem) then
    if aItem is TCacheItem then
      if Owner.TableMeta.ID = TCacheItem(aItem).Owner.TableMeta.ID then
        for Index := 0 to Pred(Owner.TableMeta.Fields.Count) do
          FFields[Index] := TCacheItem(aItem).FFields[Index];
end;

function TCacheItem.PrepareClipboardXML(const Level: Integer): string;
var
  Index, NextLevel, BufferSize, EncodePosition: Integer;
  Buffer: Pointer;
  ValueString, SecurityXML: string;
  InnerDataCache: TDataCache;
  QuantumList: TQuantumList;
  Value: Variant;
  FilterItem: TFilterItem;
  SourceStream, TargetStream: TStream;
begin
  Result := EmptyStr;
  if Assigned(Owner) and Assigned(Owner.Fields) then
    begin
      NextLevel := Level;
      if Level >= 0 then
        Inc(NextLevel);
      if Assigned(Owner.TableMeta) and Owner.TableMeta.IsSecurityProtected then
        begin
          QuantumList := TQuantumList.Create;
          try
            QuantumList.LoadObjectData(Owner.TableMeta.ID, Self.ID);
            SecurityXML := QuantumList.PrepareClipboardXML(NextLevel, Self.ID);
          finally
            QuantumList.Free;
          end;
        end
      else
        SecurityXML := EmptyStr;
      for Index := 0 to Pred(Owner.Fields.Count) do
        if not (Owner.Fields[Index].IsLookup or (frValueNotPaste in Owner.Fields[Index].Role)) then
          begin
            Value := FieldValue[Index];
            if not (VarIsNull(Value) or VarIsEmpty(Value)) then
              begin
                  Result:= Result + Format('<field id="%d" name="%s">', [Integer(Owner.Fields[Index].ID), XMLEncode(Owner.Fields[Index].Original)]);
                EncodePosition := Length(Result);
                if VarIsType(Value, [varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord, varInt64]) then
                  Result := Result + VarToStr(Value)
                else if VarIsType(Value, [varBoolean]) then
                  Result := Result + StrPas(BooleanNames[Value = True])
                else if VarIsType(Value, [varDate]) then
                  Result := Result + FormatDateTime('YYYYMMDD" "HH":"NN":"SS"."ZZZ', VarToDateTime(Value))
                else if VarIsType(Value, [varSingle, varDouble]) then
                  Result := Result + ReplaceText(FloatToStr(Value), FormatSettings.DecimalSeparator, '.')
                else if VarIsType(Value, varCurrency) then
                  Result := Result + ReplaceText(FormatFloat('#0.00', Value), FormatSettings.DecimalSeparator, '.')
                else if VarIsArray(Value) then
                  begin
                    SourceStream := TMemoryStream.Create;
                    try
                      BufferSize := VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1);
                      Buffer := VarArrayLock(Value);
                      try
                        SourceStream.WriteBuffer(Buffer^, BufferSize);
                      finally
                        VarArrayUnlock(Value);
                      end;
                      SourceStream.Seek(0, soFromBeginning);
                      TargetStream := TStringStream.Create;
                      try
                        TBase64Encoding.Base64.Encode(SourceStream, TargetStream);
                        Insert(' encode="base64"', Result, EncodePosition);
                        Result := Result + #13#10 + TStringStream(TargetStream).DataString + #13#10;
                      finally
                        TargetStream.Free;
                      end;
                    finally
                      SourceStream.Free;
                    end;
                  end
                else
                  begin
                    Insert(' encode="uri"', Result, EncodePosition);
                    Result := Result + XMLEncode(VarToStr(Value));
                  end;
                Result := Result + '</field>';
              end;
          end;
      if Assigned(Owner.TableMeta) and Assigned(Owner.TableMeta.Links) then
        for Index := 0 to Pred(Owner.TableMeta.Links.Count) do
          if Assigned(Owner.TableMeta.Links[Index].Owner) and (Owner.TableMeta.Links[Index].LinkType = daFullCascade) then
            begin
              InnerDataCache := TDataCache.Create(MetaData.GetTableMeta(Owner.TableMeta.Links[Index].Owner.ID));
              try
                FilterItem := TFilterItem.Create;
                try
                  FilterItem.AddCondition(Owner.TableMeta.Links[Index], opEQ, Self.ID);
                  if Assigned(InnerDataCache.TableMeta.DField) then
                    begin
                      FilterItem.AddCondition(InnerDataCache.TableMeta.DField, opNE, InnerDataCache.TableMeta.DField.Value2);
                      FilterItem.AddOperation(opAnd);
                    end;
                except
                  FilterItem.Free;
                  raise;
                end;
                InnerDataCache.LoadData(FilterItem, fsMax);
                if Assigned(InnerDataCache.FCacheItems) then
                  begin
                    InnerDataCache.FillAll;
                    Result := Result + InnerDataCache.FCacheItems.PrepareClipboardXML(NextLevel, True, Owner.TableMeta.Links[Index].ID);
                  end;
              finally
                InnerDataCache.Free;
              end;
            end;
      if Length(Result) <> 0 then
        begin
          if VarType(Self.ID) in [varByte, varShortInt, varWord, varSmallInt, varLongWord, varInteger] then
            ValueString := ' id="' + XMLEncode(VarToStr(Self.ID)) + '"'
          else
            ValueString := EmptyStr;
          Result := '<record' + ValueString + '>' + SecurityXML + Result + '</record>';
        end;
    end;
end;

{ TRootItem }

constructor TRootItem.Create(AOwner: TDataCache; const aSourceIndex: Integer);
begin
  inherited;
  exclude(FState, isNotInitialized);
  include(FState, isReadOnly);
  include(FState, isVirtual);
end;

function TRootItem.FieldText(const Index: Integer): string;
begin
  result := EmptyStr;
end;

procedure TRootItem.SetFieldValue(const Index: Integer; const Value: Variant);
begin
end;

function TRootItem.GetID: Variant;
begin
  result := Unassigned;
end;

function TRootItem.GetCaption: string;
begin
  result := GetTitle('_da.all');
end;

function TRootItem.GetFieldValue(const Index: Integer): Variant;
begin
  result := Null;
end;

function TRootItem.GetFieldBaseValue(const Index: Integer): Variant;
begin
  result := Null;
end;

function TRootItem.GetImageIndex(const aIndex: Integer): Integer;
begin
  Result := aIndex;
end;

function TRootItem.GetFontColor : TColor;
begin
  Result := clWindowText;
end;

function TRootItem.IsParent(ParentIDValue: Variant): Boolean;
begin
  result := false;
end;

{ TSortInfo }

constructor TSortInfo.Create(aField: TFieldMeta; const aDirection: TDSSortDirection);
begin
  inherited Create;
  FID:= aField.ID;
  FDirection:= aDirection;
end;

constructor TSortInfo.Create(aID: Integer; const aDirection: TDSSortDirection = sdAscending);
begin
  inherited Create;
  FID:= aID;
  FDirection:= aDirection;
end;

function TSortInfo.DirectionName: string;
const
  DirectionNames: array[TDSSortDirection] of PChar = ('', 'ASC', 'DESC');
begin
  Result := StrPas(DirectionNames[FDirection]);
end;

function TSortInfo.NextDirection: TDSSortDirection;
begin
  case FDirection of
    sdNone :      FDirection := sdAscending;
    sdAscending : FDirection := sdDescending;
    else          FDirection := sdNone;
  end;
  Result := FDirection;
end;

{ TSortList }

function TSortList.GetItem(const Index: Integer): TSortInfo;
begin
  Result := TSortInfo(inherited Items[Index]);
end;

function TSortList.Add(aID: Integer; const Direction: TDSSortDirection): Integer;
var
  SortInfo: TSortInfo;
begin
  SortInfo := TSortInfo.Create(aID, Direction);
  Result := inherited Add(SortInfo);
  if Result = -1 then SortInfo.Free;
end;

function TSortList.AddFields(Fields: TFieldsMeta; const Text: string; const DefaultDirection: TDSSortDirection): Boolean;
var
  Value, TagValue: string;
  Direction: TDSSortDirection;
  Field: TFieldMeta;
  Index: Integer;
begin
  Result := Assigned(Fields);
  if Result then
    begin
      Value := Trim(Text);
      Result := False;
      while Length(Value) <> 0 do
        begin
          TagValue := Trim(CutTextValue(Value, '|,', True));
          Value := Trim(Value);
          Index := Length(TagValue);
          if Index <> 0 then
            begin
              case TagValue[Index] of
                '+': { ASC }
                  begin
                    Direction := sdAscending;
                    System.Delete(TagValue, Index, 1);
                  end;
                '-': { DESC }
                  begin
                    Direction := sdDescending;
                    System.Delete(TagValue, Index, 1);
                  end;
              else
                Direction := sdAscending; //DefaultDirection;
              end;
              Field := Fields.FindByName(TagValue);
              if Assigned(Field) then
                begin
                  if Assigned(Field.LookupPair) then Field := Field.LookupPair;
                  if Add(Field.ID, Direction) <> -1 then
                    Result := True;
                end;
            end;
        end;
    end;
end;

procedure TSortList.Assign(Source: TObject);
var
  Index: Integer;
  List: TSortList;
begin
  Clear;
  if Assigned(Source) then
    if Source is TSortList then
      begin
        List := Source as TSortList;
        for Index := 0 to Pred(List.Count) do
          Add(List[Index].FID, List[Index].Direction);
      end
    else if Source is TSortInfo then
      Add(( Source as TSortInfo).FID, (Source as TSortInfo).Direction);
end;

function TSortList.ItemByID(FieldID: Integer): TSortInfo;
var
  Index: Integer;
begin
  Index := IndexByID(FieldID);
  if Index <> -1 then Result:= Items[Index]
                 else Result:= nil;
end;

function TSortList.IndexByID(const ID: Integer): Integer;
var
  Index: Integer;
begin
  Result:= -1;
  for Index:= 0 to Pred(Count) do
    if Items[Index].FID = ID then
      Exit(Index);
end;

{$IFDEF DEBUG}
procedure TSortList.DebugFieldsLog(const Text: string);
  function PrepareFieldsLog: string;
  var
    Index: Integer;
    SortInfo: TSortInfo;
    Value: string;
  begin
    Result := EmptyStr;
    for Index := 0 to Pred(Count) do
      begin
        Result := Result + Format(#13#10'                        | %2d. | ', [Index]);
        SortInfo := Items[Index];
        if Assigned(SortInfo) then
          Result := Result + Format('%-4s | %d |', [SortInfo.DirectionName, SortInfo.FieldID])
        else
          Result := Result + Format('%5s|%98s|', [' ', ' ']);
      end;
    if Length(Result) <> 0 then
      begin
        Value := Format(#13#10'                        +-----+------+%s+', [DupeString('-', 98)]);
        Result := Value + Format(#13#10'                        | No  | Sort | %-96s |', ['Field Name']) +
          Value + Result + Value;
      end;
  end;
begin
  DebugLog(Text + PrepareFieldsLog);
end;
{$ENDIF}

function TSortList.CompareTo(SortList: TSortList): Boolean;
var
  Index: Integer;
begin
  Result := Assigned(SortList) and (SortList.Count = Count);
  for Index := 0 to Pred(Count) do
    if Assigned(SortList[Index]) and Assigned(Items[Index]) and
      (SortList[Index].FID = Items[Index].FID) and
      (SortList[Index].Direction = Items[Index].Direction) then
      begin
        // Полное соответствие: ничего не делаем!
      end
    else
      begin
        Result := False;
        Break;
      end;
end;

function TSortList.FirstFieldDirestion(aField: TFieldMeta): TDSSortDirection;
begin
  if Not Assigned(aField) then Exit(sdNone);
  if 0 = Count then Exit(sdNone);
  if Items[0].FieldID <> aField.ID then Exit(sdNone);

  Result:= Items[0].Direction;
end;

{ TCacheItemList }

function TCacheItemList.GetItem(const Index: Integer): TCacheItem;
begin
  Result := TCacheItem(inherited Items[Index]);
end;

procedure TCacheItemList.PutItem(const Index: Integer; const Value: TCacheItem);
begin
  inherited Items[Index] := Value;
end;

function TCacheItemList.PrepareClipboardXML(const Level: Integer; const InnerDataset: Boolean; const LinkID: Integer): string;
var
  Index, NextLevel: Integer;
  ValueString: string;
  CacheItem: TCacheItem;
  DataCache: TDataCache;
begin
  Result := EmptyStr;
  NextLevel := Level;
  if Level >= 0 then
    Inc(NextLevel);

  DataCache := nil;
  for Index := 0 to Pred(Count) do
    begin
      CacheItem := Items[Index];
      if Assigned(CacheItem) then
        begin
          Result := Result + CacheItem.PrepareClipboardXML(NextLevel);
          if not Assigned(DataCache) then
            if Assigned(CacheItem.Owner) then
              DataCache := CacheItem.Owner;
        end;
    end;
  if Length(Result) <> 0 then
    begin
      if Assigned(DataCache) then
        begin
          if Assigned(DataCache.TableMeta) then
            begin
              ValueString := Format(' id="%d"', [Integer(DataCache.TableMeta.ID)]);
              if Length(DataCache.TableMeta.Table) <> 0 then
                ValueString := ValueString + ' name="' + XMLEncode(DataCache.TableMeta.Table) + '"';
            end
          else
            ValueString := EmptyStr;
        end
      else
        ValueString := EmptyStr;
      if InnerDataset then
        begin
          if LinkID <> 0 then
            ValueString := ValueString + ' link="' + IntToStr(LinkID) + '"';
        end
      else
        begin
          if Assigned(MetaData.MetadataDB) then
            begin
              if Length(MetaData.MetadataDB.Database) <> 0 then
                ValueString := ValueString + ' database="' + XMLEncode(MetaData.MetadataDB.Database) + '"';
              if Length(MetaData.MetadataDB.Server) <> 0 then
                ValueString := ValueString + ' server="' + XMLEncode(MetaData.MetadataDB.Server) + '"';
              if Length(MetaData.MetadataDB.Login) <> 0 then
                ValueString := ValueString + ' login="' + XMLEncode(MetaData.MetadataDB.Login) + '"';
            end;
        end;
      Result := '<dataset' + ValueString + '>' + Result + '</dataset>';
    end;
end;

{$IFDEF DEBUG}

procedure TCacheItemList.DebugDumpLog(const FileName: string);
type
  TLineBorder = (lbTop, lbMiddle, lbBottom);
var
  DataCache: TDataCache;
  ColumnFields: array of Integer;
  ColumnWidths: array of Integer;
  Index, FieldIndex, IndexSize, SourceIndexSize, FieldCount: Integer;
  CacheItem: TCacheItem;
  Stream: TStream;
  Value: string;
  Buffer: TBytes;

  function FindDataCache: TDataCache;
  var
    Index: Integer;
  begin
    Result := nil;
    for Index := 0 to Pred(Count) do
      if Assigned(Items[Index]) and Assigned(Items[Index].Owner) then
        begin
          Result := Items[Index].Owner;
          Break;
        end;
  end;

  function PrepareLineBorder(Border: TLineBorder): string;
  var
    Index: Integer;
    NextSymbol: Char;
    LastSymbol: string;
  begin
    case Border of
      lbTop:
        begin
          Result := '-';
          NextSymbol := 'T';
          LastSymbol := '¬'#13#10;
        end;
      lbBottom:
        begin
          Result := 'L';
          NextSymbol := '+';
          LastSymbol := '-';
        end;
    else
      Result := '+';
      NextSymbol := '+';
      LastSymbol := '+'#13#10;
    end;
    Result := Result + DupeString('-', IndexSize + 2) + NextSymbol +
                       DupeString('-', SourceIndexSize + 2);
    for Index := 0 to Pred(FieldCount) do
      Result := Result + NextSymbol + DupeString('-', ColumnWidths[Index] + 2);
    Result := Result + LastSymbol;
  end;
  function PrepareColumnsHeader: string;
  var
    Index: Integer;
  begin
    Result := PrepareLineBorder(lbTop) + Format('¦ %*s ¦ %*s ¦', [-IndexSize, 'Index', -SourceIndexSize, 'SourceIndex']);
    for Index := 0 to Pred(FieldCount) do
      Result := Result + Format(' %*s ¦', [-ColumnWidths[Index], DataCache.Fields[ColumnFields[Index]].Original]);
    Result := Result + #13#10 + PrepareLineBorder(lbMiddle);
  end;
begin
  DataCache := FindDataCache;
  if Assigned(DataCache) then
    begin
      SetLength(ColumnFields, DataCache.Fields.Count);
      FieldCount := 0;
      for Index := 0 to Pred(DataCache.Fields.Count) do
        if not DataCache.Fields[Index].IsLookup then
          begin
            ColumnFields[FieldCount] := Index;
            Inc(FieldCount);
          end;
      SetLength(ColumnFields, FieldCount);
      SetLength(ColumnWidths, FieldCount);
      FillChar(ColumnWidths[0], Length(ColumnWidths), #0);
      for Index := 0 to Pred(FieldCount) do
        ColumnWidths[Index] := Max(ColumnWidths[Index], Length(DataCache.Fields[ColumnFields[Index]].Original));
      SourceIndexSize := 0;
      for Index := 0 to Pred(Count) do
        begin
          CacheItem := Items[Index];
          if Assigned(CacheItem) and not (isNotInitialized in CacheItem.State) then
            begin
              SourceIndexSize := Max(SourceIndexSize, Length(IntToStr(CacheItem.SourceIndex)));
              for FieldIndex := 0 to Pred(FieldCount) do
                ColumnWidths[FieldIndex] := Max(ColumnWidths[FieldIndex], Length(CacheItem.FieldText(ColumnFields[FieldIndex])));
            end;
        end;
      IndexSize := Length(IntToStr(Succ(Count)));
      if IndexSize < 5 then IndexSize := 5;
      if SourceIndexSize < 11 then SourceIndexSize := 11;
      Value := EmptyStr;
      for Index := 0 to Pred(Count) do
        begin
          if Length(Value) <> 0 then Value := Value + #13#10;
          Value := Value + Format('¦ %*d ¦', [IndexSize, Succ(Index)]);
          CacheItem := Items[Index];
          if Assigned(CacheItem) and not (isNotInitialized in CacheItem.State) then
            begin
             Value := Value + Format(' %*d ¦', [SourceIndexSize, CacheItem.SourceIndex]);
              for FieldIndex := 0 to Pred(FieldCount) do
                Value := Value + Format(' %*s ¦', [ColumnWidths[FieldIndex], CacheItem.FieldText(ColumnFields[FieldIndex])]);
            end
          else
            begin
              Value := Value + Format(' %*s ¦', [SourceIndexSize, ' ']);
              for FieldIndex := 0 to Pred(FieldCount) do
                Value := Value + Format(' %*s ¦', [ColumnWidths[FieldIndex], ' ']);
            end;
        end;
      if Length(Value) <> 0 then
        Value := Value + #13#10 + PrepareLineBorder(lbBottom);
      Value := PrepareColumnsHeader + Value;
      Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
      try
        Buffer := TEncoding.UTF8.GetBytes(Value);
        Stream.WriteBuffer(Buffer, Length(Buffer));
      finally
        Stream.Free;
      end;
    end;
end;
{$ENDIF}

{ TDataCache }

constructor TDataCache.Create(aTableMeta: TTableMeta; const aIsLibrary: Boolean);
begin
  inherited Create(nil);
  FFocusedItem := nil;
  FCacheItems := TCacheItemList.Create;
  FCacheSelectedItems := TCacheItemList.Create(False);
  FRootItem := TRootItem.Create(Self, -1);
  FSortList := TSortList.Create;
  FFilters := TFiltersManager.Create;
  FIsLibrary := aIsLibrary;
  FTableMeta := aTableMeta;
  FIsFillAllItems := False;
  FSubDatasets := TSubDatasets.Create(Self);
  FFields := TFieldsCache.Create(Self); // Копируем поля из метаданных ...
  FControlFilter := TFilterItem.Create;
  if Assigned(aTableMeta) and not FIsLibrary then
    begin
      FFilters.ParentManager := FTableMeta.Filters;
      FDataManager := CreateDataManager(FTableMeta);
    end;
  FCacheIndexes := TCacheIndexes.Create(Self);
  FUpdateType:= mcNone;
  GetMainFieldsIndexes;
end;

destructor TDataCache.Destroy;
begin
  FreeAndNil(FCacheIndexes);
  FreeAndNil(FControlFilter);
  FreeAndNil(FFields);
  FreeAndNil(FSubDatasets);
  FFilters.Free;
  FSortList.Free;
  DestroyData;
  FDataManager.Free;
  FRootItem.Free;
  FCacheItems.Free;
  FCacheSelectedItems.Free;
  inherited Destroy;
end;

function TDataCache.GetItem(const Index: Integer): TCacheItem;
begin
  Result := CacheItems[Index];
  if (not Assigned(Result)) and FCanCreateItems then
    begin
      Result := TCacheItem.Create(Self, Index);
      FCacheItems[Index] := Result;
      Include(Result.FState, isNotInitialized);
    end;
end;

function TDataCache.FocusedIndex : Integer;
begin
  if Assigned(FocusedItem) then
 //   result:=IndexByItem(FocusedItem)           //2015
    result := CacheItems.IndexOf(FocusedItem)
  else
    Result:=-1;
end;

function TDataCache.ItemSelected(const Index: Integer): Boolean;
var CI : TCacheItem;
begin
  CI := CacheItems.Items[Index];
  if Assigned(CI) then Result := CI.GetSelected
                  else Result := False;
end;

procedure TDataCache.ItemSelect(const Index: Integer);
begin
  GetItem(Index).SetSelected(True);
end;

procedure TDataCache.ItemUnSelect(const Index: Integer);
var CI : TCacheItem;
begin
  CI := CacheItems.Items[Index];
  if Assigned(CI) then CI.SetSelected(False);
end;

function TDataCache.GetSelectedItem(const Index: Integer): TCacheItem;
begin
  if (0<=index) and (index < FCacheSelectedItems.Count) then
    result := FCacheSelectedItems[index]
  else
    result:=nil;
end;

function TDataCache.GetSelectedItemsCount : Integer;
begin
  result:=FCacheSelectedItems.Count;
end;

function TDataCache.GetSelectedKeys: Variant;
var i: Integer;
begin
  case FCacheSelectedItems.Count of
   0: Result:= unassigned;
   1: Result:= FCacheSelectedItems[0].ID;
  else
    begin
      Result:= VarArrayCreate([0, FCacheSelectedItems.Count-1], varVariant);
      for i:=0 to Pred(FCacheSelectedItems.Count) do
        VarArrayPut(Result, FCacheSelectedItems[i].ID, [i]);
    end;
  end;
end;

procedure TDataCache.SetSelectedKeys(const Value: Variant);
var i, p: Integer;
begin
  ClearSelection;

  if VarType(Value) = (varArray or varVariant) then
    for i:= VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
      begin
        P:= IndexByID(Value[i]);
        if P <> -1 then Items[P].Selected := True;
      end else

  if not VarIsEmpty(Value) then
    begin
      P:= IndexByID(Value);
      if P <> -1 then Items[P].Selected := True;
    end;
end;

function TDataCache.GetHint(const Index: Integer): string;
begin
  if (isNotInitialized in Items[Index].State) then Items[Index].FillItem;
  Result := Items[Index].Hint;
end;

procedure TDataCache.InsertItem(index: integer; const Value: TCacheItem);
begin
  FCacheItems.Insert(index, Value);
end;

procedure TDataCache.SetFocusedItem(const Value: TCacheItem);
var
  Allow   : boolean;
  NewItem : TCacheItem;
begin
  if (FFocusedItem = Value) and (FFirstFocused) then Exit;
  NewItem := Value;
  Allow := Self.AlowFocusedChanging(NewItem);
  if Allow and FFirstFocused and Assigned(FFocusedChanging) then
    FFocusedChanging(FFocusedItem);
  FFirstFocused :=True;

  if Allow then
    FFocusedItem:= NewItem;
  if Assigned(FFocusedChange) then FFocusedChange(FFocusedItem);
end;

function TDataCache.GetItemsCount: Integer;
begin
  Result := CacheItems.Count;
end;

procedure TDataCache.SetItemsCount(const Value: Integer);
begin
  Clear;
  FIsFillAllItems := False;
  FCacheItems.Count := Value;
  FCanCreateItems := True;
end;

procedure TDataCache.GetMainFieldsIndexes;
var i: Integer;
begin
  FIDIndex := -1;
  FParentIDIndex := -1;
  FCaptionIndex := -1;
  FFolderSignIndex := -1;
  FDeletedSignIndex := -1;
  FDefaultSignIndex := -1;
  if Assigned(TableMeta) and Assigned(Fields) then
    if TableMeta.IsDynamic then
      begin
        if Assigned(TableMeta.KField) and (TableMeta.KField.Count <> 0) then
          FIDIndex := Fields.IndexByName(TableMeta.KField[0].Original);
        if FIDIndex = -1 then
          for i:=0 to Pred(Fields.Count) do
            if Fields[i].Key then
              begin
                FIDIndex:= i;
                Break;
              end;
      end
    else
      begin
        if Assigned(TableMeta.KField) and (TableMeta.KField.Count <> 0) then
          FIDIndex := Fields.IndexByID(TableMeta.KField[0].ID);
        if Assigned(TableMeta.PField) then
          FParentIDIndex := Fields.IndexByID(TableMeta.PField.ID);
        if Assigned(TableMeta.GField) then
          FFolderSignIndex := Fields.IndexByID(TableMeta.GField.ID);
        if Assigned(TableMeta.DField) then
          FDeletedSignIndex := Fields.IndexByID(TableMeta.DField.ID);
        if Assigned(TableMeta.OField) then
          FDefaultSignIndex := Fields.IndexByID(TableMeta.OField.ID);
        if Assigned(TableMeta.NField) then
          begin
            if (TableMeta.NField.IsLookup=False) and (Assigned(TableMeta.NField.LookupPair)) then
              FCaptionIndex := Fields.IndexByID(TableMeta.NField.LookupPair.ID)
            else
              FCaptionIndex := Fields.IndexByID(TableMeta.NField.ID);
          end;
      end;
end;

function TDataCache.GetFieldCount : integer;
begin
  if Assigned(FFields) then
    result := FFields.Count
  else
    result := 0;
end;

procedure TDataCache.SetOffSelectedChange(const Value: boolean);
begin
  FOffSelectedChange := Value;
end;

function TDataCache.CacheItemIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
begin
  Result:= TCacheItem(aDataObject).FieldValueRecurce(aIdent, aValue);
end;

procedure TDataCache.DestroyData;
begin
  if Assigned(FDataSet) then
    FDataSet.RemoveFreeNotification(Self);
  FreeAndNil(FDataSet);
end;

function TDataCache.GetActive : boolean;
begin
  result := Assigned(FDataSet) and (FDataSet.Active);
end;

procedure TDataCache.DoDataAfterOpen;
begin
  if Assigned(FDataSet) and Assigned(FOnDataAfterOpen) then
    FOnDataAfterOpen(FDataSet);
end;

procedure TDataCache.DoDataSetupFilter(aDescr : TCustomQueryDescr);
begin
  if Assigned(aDescr) and Assigned(FOnDataSetupFilter) then
    FOnDataSetupFilter(aDescr);
end;

function TDataCache.QueryOpenData : boolean;
begin
  if Assigned(FDataSet) then
  begin
    TableMeta.Database.CheckConnection;
    result := TableMeta.Database.Connected;
  end
  else
    result := false;
  if Assigned(FOnQueryOpenData) then
   result := result and FOnQueryOpenData(FDataSet);
end;

function TDataCache.ReInit(const Changed: TTypeChanged; const InsertKey: Variant; const LocateIndex: Integer = ltNone): Boolean;
begin
  SetItemsCount(Count);
  Result := Update(Changed, InsertKey, LocateIndex);
end;

procedure TDataCache.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (aComponent = FDataSet) then
    FDataSet := nil;
  inherited Notification(aComponent, Operation);
end;

procedure TDataCache.AfterDataOpen(Sender: TObject);
var FM : TFieldMeta;
    N: Integer;
    {$IFDEF DEBUG}
    DebugString: string;
    {$ENDIF}
begin
  if FDataSet.Active then
    SetItemsCount(FDataSet.RecordCount)
  else
    Clear;

  if (TableMeta.IsMetabaseTable) and SameText(TableMeta.Table, tblBase) then
    begin
      FillAll;
      N:= IndexByID(0);
      if -1 < N then
        FCacheItems[N]:= TConfigCacheItem.Create(Self, N);
    end;

  //Загружаем и сортируем, если надо сортировать
  if (Count>1) and (SortList.Count>0) then
  begin
    FM:= TableMeta.Fields.FindByID(SortList.Items[0].FieldID);
                                             // Поле с переводом
    if (FM.Calculated and Not FM.IsStored) or  // Вычисляемое, нехранимое поле
       (FM.DataType in NotSortTypes) or
       ((FM.DataType in StringTypes) and ((FM.ShowType = stTranslate) or (FM.CodePage = cpUTF8) )) or (FM.IsLookup) then
      begin
        CreateAll;
        FillAll;
        CacheIndexes.ItemIndex := CacheIndexes.Add(SortList);
      end;
  end;

  DoDataAfterOpen;
end;

function TDataCache.Agregate(aOperation: TOperationType; sField: String; var aValue: Variant): Boolean;
var
  vValue, SumValue : Variant;
  i,F,iCount : integer;
begin
  F := Fields.IndexByName(sField);

  if aOperation = opCOUNT then
    try
      aValue:= Count;
    except
      Exit(False);
    end;

  if (0 <= F) and (aOperation in [opMAX]) then
    try
      for i := Count-1 downto 0 do
        begin
          vValue := Items[i].FieldValue[F];
          if (varCompareValue(aValue, vValue) = vrLessThan) or VarIsNull(aValue) or VarIsEmpty(aValue) then
            aValue := vValue;
        end;
    except
      Exit(False);
    end;

  if (0 <= F) and (aOperation in [opMIN]) then
    try
      for i := Count-1 downto 0 do
        begin
          vValue := Items[i].FieldValue[F];
          if (varCompareValue(aValue, vValue) = vrGreaterThan) or VarIsNull(aValue) or VarIsEmpty(aValue) then
            aValue := vValue;
        end;
    except
      Exit(False);
    end;

  if (0 <= F) and (aOperation in [opSUM]) then
    try
      vValue := 0;
      for i := Count-1 downto 0 do
        begin
          vValue := Items[i].FieldValue[F];
          if not ( VarIsNull(aValue) or VarIsEmpty(aValue) ) then
            aValue := aValue + vValue;
        end;
    except
      Exit(False);
    end;

  if (0 <= F) and (aOperation in [opAVG]) then
    try
      SumValue := 0;
      iCount   := 0;
      for i := Count-1 downto 0 do
        begin
          vValue := Items[i].FieldValue[F];
          if not ( VarIsNull(aValue) or VarIsEmpty(aValue) ) then
            begin
              SumValue := SumValue + vValue;
              aValue   := iCount + 1;
            end;
        end;

      if iCount>0 then aValue :=  SumValue / aValue;
    except
      Exit(False);
    end;

  Result := true;
end;

procedure TDataCache.Clear;
begin
  FFocusedItem := nil;
  FRootItem.Owner.FocusedItem:=nil;
  Exclude(FRootItem.FState, isSelect);
  FCacheItems.Clear;
  FCacheSelectedItems.Clear;
  FCacheIndexes.Clear;
  FFirstFocused :=False;
end;

function TDataCache.AddNewItem : TCacheItem;
begin
  result := InsertNewItem(Count);
end;

function TDataCache.InsertNewItem(index: integer): TCacheItem;
begin
  result := TCacheItem.Create(Self, index);
  InsertItem(index, result);
  Include(result.FState, isInserted);
end;

function TDataCache.InsertExistItem(InitialItem : TCacheItem): TCacheItem;
begin
  result := nil;
  if InitialItem.Owner.TableMeta.ID <> TableMeta.ID then Exit;
  if FindById(InitialItem.ID) = InitialItem then
     begin
       if RootItem=InitialItem then
         Result:=InitialItem;
       Exit;
     end;
  result := TCacheItem.Create(Self, -1);
  InsertItem(Count, result);
  result.Assign(InitialItem);
end;

function TDataCache.CanPasteFromClipboard(const ByOwnerValue: Boolean; var ClipID: Integer): Boolean;
var i : Integer;
begin
  ClipID:=-1;
  //Result := DeClipboard.ContainsDeData;
  Result := False;

  if DeClipboard.HasFormat(DeXML) then
    begin
      ClipID := DeClipboard.DataID;
      // данные скопированые в этой таблице
      Result := ClipID = FTableMeta.ID;

      // вставляем в таблицу взаимосвязей
      if (not Result) and (ClipID > 0) and FTableMeta.IsInterrelations then
        Result := True;

      // данные скопированые в родительской таблице
      if (not Result) and ByOwnerValue then
        for i := 0 to Pred(FTableMeta.Fields.Count) do
          if (not FTableMeta.Fields[i].isLookUp) and (ClipID = FTableMeta.Fields[i].Link) then
            begin
              Result := True;
              Break;
            end;
    end;

  if DeClipboard.HasFormat(dbcoXML) then
    begin
      Result := True;
    end;
end;

procedure TDataCache.CopyToClipboard;
type
  TShowDateTime = (sdDate, sdTime, sdDateTime);
var
  DataText, DataTextRTF, DataTextXLS, LineTextXLS, RecordStart, RecordStop, StrValue, DataHeaderXLS: AnsiString;
  StringData    : TStringList;
  hMem          : THandle;
  DataPtr       : pAnsiChar;
  LocaleData    : Longint;
  i,j,N,M    : integer;
  WStr          : WideString;
  SLen, SLenRTF, SLenXLS: Integer;
  isFirst       : Boolean;
{ Buffer   : array[0..MAX_PATH] of Char;
  s,aFile  : string;
  F        : Text;
  DropFiles: PDropFiles;
  hGlobal  : THandle;
  iLen,iID : Integer;}
  CopyLevel : Integer;
  ColumnCount, ColumnIndex: Integer;
  ExcelStyles: TClipboardExcelStyles;
  ExcelBackgroundRow: TColor;
  ExcelBorders: TClipboardExcelCellBorders;
  QuantumList: TQuantumList;
  ColumnList: TColumnList;
  BRegGridBrush: Boolean;

  procedure qsort(l,r: integer);
  var
    i,j,m,Fi,Fj,Fm : integer;
  begin
    i:=l; j:=r; m:=(r+l) div 2;
    Fm:= FCacheItems.IndexOf(SelectedItems[m]);
    repeat

      Fi:=FCacheItems.IndexOf(SelectedItems[i]);
      while (Fi < Fm) do
        begin i:=i+1; Fi:=FCacheItems.IndexOf(SelectedItems[i]); end;

      Fj:=FCacheItems.IndexOf(SelectedItems[j]);
      while (Fm < Fj) do
        begin j:=j-1; Fj:=FCacheItems.IndexOf(SelectedItems[j]); end;

      if (i<=j) then
      begin
        if (Fi > Fj) then
          FCacheSelectedItems.Exchange(i,j);
        i:=i+1; j:=j-1;
      end;
    until i>j;
    if l<j then qsort(l,j);
    if i<r then qsort(i,r);
  end;

  function TemplateShowDateTime(const Template: string): TShowDateTime;
  begin
    if (Pos('D', Template) = 0) and (Pos('M', Template) = 0)
      then if (Pos('H', Template) = 0) and (Pos('N', Template) = 0)
             then Result := sdDateTime
             else Result := sdTime
      else if (Pos('H', Template) = 0) and (Pos('N', Template) = 0)
             then Result := sdDate
             else Result := sdDateTime;
  end;

  function PrepareBackground(const aItem: TCacheItem): TColor;
  begin
  if BRegGridBrush then
    begin
      Result:= aItem.FontColor;
      if Result = clBlack then Result:= clNone
    end
  else
    Result := clNone;
  end;

  function PrepareCell(Field: TFieldMeta; const Value: Variant; const Color, Background: TColor; const Borders: TClipboardExcelCellBorders): string;
  var
    StyleIndex: Integer;
    WorkValue: Variant;
  begin
    Result := EmptyStr;
    StyleIndex := -1;
    if Assigned(Field) and not (VarIsEmpty(Value) or VarIsNull(Value)) then
      if Field.ShowType = stYesNo then
        begin
          if CompareStr(VarToStr(Value), Field.Value2) = 0 then
            begin
              Result := '<Data ss:Type="String">' + THTMLEncoding.HTML.Encode(CheckText) + '</Data>';
              StyleIndex := ExcelStyles.Add(EmptyStr, Color, Background, Borders);
            end;
        end
      else if Field.DataType in StringTypes then
        begin
          Result := '<Data ss:Type="String">';
          if Field.CodePage = cpUTF8 then WorkValue := VarToStr(Value)
                                     else WorkValue := Value;
          case Field.ShowType of
            //stUnicode:
            //  Result := Result + THTMLEncoding.HTML.Encode(UTF8ToString(Value));
            //stDOSString:
            //  Result := Result + THTMLEncoding.HTML.Encode(VarToStr(Value));
            stTranslate:
              Result := Result + THTMLEncoding.HTML.Encode(GetTitle(VarToStr(Value)));
            stPassword, stEncryptedPassword:
              Result := Result + '******';
          else
            Result := Result + THTMLEncoding.HTML.Encode(TrimRight(VarToStr(WorkValue)));
          end;
          Result := Result + '</Data>';
          StyleIndex := ExcelStyles.Add(EmptyStr, Color, Background, Borders);
        end
      else if Field.DataType in FloatTypes then
        begin
          Result := '<Data ss:Type="Number">' + ReplaceText(FloatToStr(Value), FormatSettings.DecimalSeparator, '.') + '</Data>';
          StyleIndex := ExcelStyles.Add(EmptyStr, Color, Background, Borders);
        end
      else if Field.DataType in IntegerTypes then
        begin
          Result := '<Data ss:Type="Number">' + IntToStr(Value) + '</Data>';
          StyleIndex := ExcelStyles.Add(EmptyStr, Color, Background, Borders);
        end
      else if Field.DataType in DateTimeTypes then
        begin
          Result := '<Data ss:Type="DateTime">';
          if Field.DataType = ftDate then
            Result := Result + FormatDateTime('YYYY"-"MM"-"DD', Value)
          else if Field.DataType = ftTime then
            Result := Result + FormatDateTime('HH":"NN":"SS', Value)
          else
            Result := Result + FormatDateTime('YYYY"-"MM"-"DD"T"HH":"NN":"SS"."ZZZ', Value);
          case TemplateShowDateTime(UpperCase(Field.Template)) of
            sdDate: StyleIndex := ExcelStyles.Add('Short Date', Color, Background, Borders);
            sdTime: StyleIndex := ExcelStyles.Add('Short Time', Color, Background, Borders);
          else
            if Value = Trunc(Value) then
              StyleIndex := ExcelStyles.Add('Short Date', Color, Background, Borders)
            else
              StyleIndex := ExcelStyles.Add('General Date', Color, Background, Borders);
          end;
          Result := Result + '</Data>'
        end
      else if Field.DataType = ftBlob then
        begin
          Result := '<Data ss:Type="String">' + THTMLEncoding.HTML.Encode(CheckText) + '</Data>';
          StyleIndex := ExcelStyles.Add(EmptyStr, Color, Background, Borders);
        end
      else
        begin
          Result := '<Data ss:Type="String">' + THTMLEncoding.HTML.Encode(VarToStr(Value)) + '</Data>';
          StyleIndex := ExcelStyles.Add(EmptyStr, Color, Background, Borders);
        end;
    if Length(Result) = 0 then
      StyleIndex := ExcelStyles.Add(EmptyStr, Color, Background, Borders);
    Result := #13#10'        <Cell' + ExcelStyles.PrepareStyle(StyleIndex) + '>' + Result + '</Cell>';
    Inc(ColumnIndex);
  end;
  function PrepareFieldText(Field: TFieldMeta; const Value: string): string;
  begin
    Result := Value;
    if Assigned(Field) and (Field.DataType in FloatTypes) then
      Result := ReplaceText(Value, FormatSettings.ThousandSeparator, '');
  end;
  function PrepareCellHeaders: string;
  var
    Index, StyleIndex: Integer;
    function PrepareCell(const Value: string): string;
    begin
      if StyleIndex = -2 then
        StyleIndex := ExcelStyles.Add(EmptyStr, clBlack, clSilver, ExcelBorders, [fsBold]);
      if Length(Value) <> 0 then
        Result := '<Data ss:Type="String">' + THTMLEncoding.HTML.Encode(Value) + '</Data>'
      else
        Result := EmptyStr;
      Result := #13#10'        <Cell' + ExcelStyles.PrepareStyle(StyleIndex) + '>' + Result + '</Cell>';
    end;
  begin
    Result := EmptyStr;
    StyleIndex := -2;
    {$IFDEF DEBUG}
    //ColumnList.DebugColumnsLog('PrepareCellPeaders ...');
    {$ENDIF}
    for Index := 0 to Pred(ColumnList.Count) do
      if (not ColumnList[Index].Field.IsLookup) and
         ((Ord(ColumnList[Index].Field.VisibleLevel) >= CopyLevel) or (frNameField in ColumnList[Index].Field.Role)) then
        begin
          if (CopyLevel = -1) or (not Assigned(ColumnList[Index].Field.LookupPair)) then
            Result := Result + PrepareCell(ColumnList[Index].Field.Native);
          if Assigned(ColumnList[Index].Field.LookupPair) then
            Result := Result + PrepareCell(ColumnList[Fields.IndexByID(ColumnList[Index].Field.LookupPair.ID)].Field.Native);
        end;
    if Length(Result) <> 0 then
        Result := #13#10'      <Row>' + Result + #13#10'      </Row>';
  end;
begin
  if (SelectedCount=0) or (Fields.Count=0) then Exit;
  try
    BRegGridBrush:= Variables.AsBoolean[RegGridBrush];
    {$IFDEF DEBUG}
    DebugFieldsLog('Copying to clipboard ...', Fields);
    {$ENDIF}
  // упорядочиваем список выделенных согласно их присутствию в основном списке

  //быстрая сортировка
  qsort(0,SelectedCount-1);

  // сохранение данных в буфере обмена
  Clipboard.Open;
  try
  Clipboard.Clear;

  if Variables.AsBoolean[RegCopyAllField] then CopyLevel:=-1
                                          else CopyLevel:= 2;
  // сохранение для использования DBCO
  if not TableMeta.IsInterrelations then
  begin
    DataText := PrepareClipboardXML(0);
    if Length(DataText) <> 0 then
      begin
        hMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, Succ(Length(DataText)));
        DataPtr := GlobalLock(hMem);
        StrCopy(DataPtr, PAnsiChar(DataText));
        GlobalUnlock(hMem);
        Clipboard.SetAsHandle(DeXML, hMem);
      end;
       {
         доделать бы, да руки не доходят
    DataText := GetAsXML([dfStoredFields, dfSelectedRecords]);
    if Length(DataText) <> 0 then
      begin
        hMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, Succ(Length(DataText)));
        DataPtr := GlobalLock(hMem);
        StrCopy(DataPtr, PAnsiChar(DataText));
        GlobalUnlock(hMem);
        Clipboard.SetAsHandle(dbcoXML, hMem);
      end;
      {}

    {
    StringData := TStringList.Create;
    try
      StringData.Append(IntToStr(TableMeta.ID));
      for I := 0 to Pred(TableMeta.Fields.Count) do
        begin
          StrValue := TableMeta.Fields[I].Original;
          StringData.Append(StrValue);
        end;
      DataText := StringData.CommaText;
      hMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, Succ(Length(DataText)));
      DataPtr := GlobalLock(hMem);
      StrCopy(DataPtr, PAnsiChar(DataText));
      GlobalUnlock(hMem);
      Clipboard.SetAsHandle(DeHeader, hMem);
      //SetClipboardData(DeHeader, hMem);

      StringData.Text := IntToStr(TableMeta.ID);
      for J := 0 to Pred(SelectedCount) do
        begin
          FillItems(J, J, fsMax);
          for I := 0 to Pred(TableMeta.Fields.Count) do
            begin
              StrValue := SelectedItems[J].ClipboardValue[I];
              StringData.Append(StrValue);
            end;
        end;

      DataText := StringData.CommaText;
      hMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, Succ(Length(DataText)));
      DataPtr := GlobalLock(hMem);
      StrCopy(DataPtr, PAnsiChar(DataText));
      GlobalUnlock(hMem);
      Clipboard.SetAsHandle(DeFormat, hMem);
      //SetClipboardData(DeFormat, hMem);

      // Если набор данных защищён, то ...
      if TableMeta.IsSecurityProtected then
        begin
          Metadata.CopySignatureToClipboard(False); // Добавим идентификатор базы данных в Clipboard ...
          QuantumList := TQuantumList.Create;
          try
            QuantumList.LoadObjectData(TableMeta.ID, 0);
            QuantumList.CopyToClipboard(False);
          finally
            QuantumList.Free;
          end;
        end;
    finally
      StringData.Free;
    end;
    }
  end;

  // подготовка заголовка для RTF
  ColumnList := TColumnList.Create;
  ColumnList.InitColumnList(Fields);
  try
  N:= 0;
  M:= 0;
  ColumnCount := 0;
  RecordStart:= EmptyStr;
  for I := 0 to Pred(ColumnList.Count) do
    if (not ColumnList[I].Field.IsLookup) and
       ((Ord(ColumnList[I].Field.VisibleLevel)>=CopyLevel) or (frNameField in ColumnList[I].Field.Role)) then
      begin
        if (CopyLevel=-1) and Assigned(ColumnList[I].Field.LookupPair) then
          begin
            Inc(N, ColumnList[I].Field.LookupPair.Width);
            Inc(ColumnCount);
          end;
        Inc(N, ColumnList[I].Field.Width);
        Inc(ColumnCount);
      end;

  for I := 0 to Pred(ColumnList.Count) do
    if (not ColumnList[I].Field.IsLookup) and
       ((Ord(ColumnList[I].Field.VisibleLevel)>=CopyLevel) or (frNameField in ColumnList[I].Field.Role)) then
      begin
        if (CopyLevel=-1) and Assigned(ColumnList[I].Field.LookupPair) then
          begin
            Inc(M, ColumnList[I].Field.LookupPair.Width);
            RecordStart:=RecordStart+'\cellx'+IntToStr((9900*M) div N);
          end;

        Inc(M, ColumnList[I].Field.Width);
        RecordStart:=RecordStart+'\cellx'+IntToStr((9900*M) div N);
      end;
  RecordStart:='\trowd'+RecordStart+'\pard\intbl ';
  RecordStop :='\row'#10;

  ExcelStyles := TClipboardExcelStyles.Create;
  try

  // подготовка данных для использования прочими приложениями
  DataText    := EmptyAnsiStr;
  DataTextXLS := EmptyAnsiStr;
  DataTextRTF := EmptyAnsiStr;

  // 11.01.2016 * Раскомментировать код ниже в случае необходимости добавлять рамки для ячеек
  {
  if Variables.AsBoolean[RegGridLines] then
    ExcelBorders := [cbLeft, cbRight, cbTop, cbBottom]
  else
    ExcelBorders := [];
  }
  // А строчку кода ниже убрать...
  ExcelBorders := [];
  // 11.01.2016 *

  if Variables.AsBoolean[RegCopyHeaderField] then
    DataHeaderXLS := PrepareCellHeaders
  else
    DataHeaderXLS := EmptyAnsiStr;

  for j:=0 to Pred(SelectedCount) do
    begin
      if j > 0 then
        DataText := DataText + #13#10;
      DataTextRTF := DataTextRTF + RecordStart;
      LineTextXLS := EmptyAnsiStr;
      ColumnIndex := 1;
      isFirst:=True;
       // 11.01.2016 * Раскомментировать код ниже в случае необходимости добавлять цвет фона для ячеек
      {
      ExcelBackgroundRow := PrepareBackground(SelectedItems[j]);
      }
      // А строчку кода ниже убрать...
      ExcelBackgroundRow := clNone;
      // 11.01.2016 *
      for i := 0 to Pred(ColumnList.Count) do
        if (not ColumnList[I].Field.IsLookup) and
           ((Ord(ColumnList[I].Field.VisibleLevel) >= CopyLevel) or (frNameField in ColumnList[I].Field.Role)) then
          begin
            // добавляем значение поля и ссылку
            if (CopyLevel=-1) or (not Assigned(ColumnList[I].Field.LookupPair)) then
              begin
                if isFirst then isFirst:=False
                           else DataText := DataText + #9;

                StrValue := SelectedItems[j].FieldText(ColumnList[I].ValueIndex);
                DataTextRTF := DataTextRTF + RTFEncode(StrValue) + '\cell ';

                // удаляем концы строк в значениях, помогает для Excel
                StrValue:=StringReplace(PrepareFieldText(ColumnList[I].Field, StrValue), #13#10, #10, [rfReplaceAll]);

                // берем в кавычки если есть мешающие символы, помогает для Excel
                if (AnsiPos(#9,  StrValue) > 0) or
                   (AnsiPos(#10, StrValue) > 0)then
                  StrValue := AnsiQuotedStr(StrValue, '"');

                DataText := DataText + StrValue;
                LineTextXLS := LineTextXLS + PrepareCell(ColumnList[I].Field, SelectedItems[j].FieldValue[ColumnList[I].ValueIndex],
                  SelectedItems[j].FontColor, ExcelBackgroundRow, ExcelBorders);
              end;

            // добавляем значение справочной таблицы
            if Assigned(ColumnList[I].Field.LookupPair) then
              begin
                if isFirst then isFirst:=False
                           else DataText := DataText + #9;

                StrValue := SelectedItems[j].FieldText(ColumnList[I].TextIndex);
                DataTextRTF := DataTextRTF + RTFEncode(StrValue) + '\cell ';

                // удаляем концы строк в значениях, помогает для Excel
                StrValue:=StringReplace(PrepareFieldText(ColumnList[I].Field, StrValue), #13#10, #10, [rfReplaceAll]);

                // берем в кавычки если есть мешающие символы, помогает для Excel
                if (AnsiPos(#9,  StrValue) > 0) or
                   (AnsiPos(#10, StrValue) > 0)then
                  StrValue := AnsiQuotedStr(StrValue, '"');

                DataText := DataText + StrValue;

                LineTextXLS := LineTextXLS + PrepareCell(TableMeta.Fields[ColumnList[I].TextIndex], SelectedItems[j].FieldValue[ColumnList[I].TextIndex],
                  SelectedItems[j].FontColor, ExcelBackgroundRow, ExcelBorders);
              end;
          end;

      DataTextRTF := DataTextRTF + RecordStop;
      if Length(LineTextXLS) <> 0 then
        DataTextXLS := DataTextXLS + #13#10'      <Row>' + LineTextXLS + #13#10'      </Row>';
    end;

  DataTextRTF := '{\rtf1\ansi ' + DataTextRTF + '}';
  DataTextXLS := '<?xml version="1.0"?>'#13#10'<?mso-application progid="Excel.Sheet"?>'#13#10 +
                 '<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet" xmlns:o="urn:schemas-microsoft-com:office:office" ' +
                 'xmlns:x="urn:schemas-microsoft-com:office:excel" xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet" ' +
                 'xmlns:html="http://www.w3.org/TR/REC-html40">'#13#10'  <Styles>' + ExcelStyles.Text +
                 #13#10'  </Styles>'#13#10'  <Worksheet ss:Name="' + TableMeta.Name + '">'#13#10'    <Table ' +
                 Format('ss:ExpandedColumnCount="%d" ss:ExpandedRowCount="%d"', [ColumnCount,
                 iif(Length(DataHeaderXLS) = 0, SelectedCount, Succ(SelectedCount))]) + ' ss:DefaultRowHeight="15">' +
                 DataHeaderXLS + DataTextXLS + #13#10'    </Table>'#13#10'  </Worksheet>'#13#10'</Workbook>';
  DataTextXLS := UTF8Encode(DataTextXLS);
  finally
    ExcelStyles.Free;
  end;
  SLen    := Length(DataText) + 1;
  SLenXLS := Length(DataTextXLS) + 1;
  SLenRTF := Length(DataTextRTF) + 1;
  {
  SLen    := StrLen(PAnsiChar(DataText))+1;
  SLenRTF := StrLen(PAnsiChar(DataTextRTF))+1;
  }
  // установка кодировки по умолчанию для CF_TEXT
  LocaleData := $0419;
  hMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SizeOf(LocaleData));
  DataPtr := GlobalLock(hMem);
  system.Move(LocaleData, DataPtr^, SizeOf(LocaleData));
  GlobalUnlock(hMem);
  Clipboard.SetAsHandle(CF_LOCALE, hMem);
  //SetClipboardData(CF_LOCALE, hMem);

  // DeExcelXML для приложений Excel
  hMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SLenXLS);
  DataPtr := GlobalLock(hMem);
  StrCopy(DataPtr, PAnsiChar(DataTextXLS));
  GlobalUnlock(hMem);
  Clipboard.SetAsHandle(DeExcelXML, hMem);
  //SetClipboardData(DeExcelXML, hMem);

  // CF_TEXT для приложений DOS
  hMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SLen);
  DataPtr := GlobalLock(hMem);
  StrCopy(DataPtr, PAnsiChar(DataText));
  GlobalUnlock(hMem);
  Clipboard.SetAsHandle(CF_TEXT, hMem);
  //SetClipboardData(CF_TEXT, hMem);

  // CF_OEMTEXT для приложений Win9x: Delphi, TEdit'ы
  hMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SLen);
  DataPtr := GlobalLock(hMem);
  StrCopy(DataPtr, PAnsiChar(DataText));
  GlobalUnlock(hMem);
  Clipboard.SetAsHandle(CF_OEMTEXT, hMem);
  //SetClipboardData(CF_OEMTEXT, hMem);

  // CF_UNICODETEXT для приложений WinXP: MS Office, NotePad, WordPad, ICQ, ....
  WStr:= WideString(DataText);
  hMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, 2*SLen);
  DataPtr := GlobalLock(hMem);
  Move(PWideChar(WStr)^,DataPtr^,2*SLen);
  GlobalUnlock(hMem);
  Clipboard.SetAsHandle(CF_UNICODETEXT, hMem);
  //SetClipboardData(CF_UNICODETEXT, hMem);

  // DeRTF для приложений текстовых редакторов в том числе MS Word
  hMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SLenRTF);
  DataPtr := GlobalLock(hMem);
  StrCopy(DataPtr, PAnsiChar(DataTextRTF));
  GlobalUnlock(hMem);
  Clipboard.SetAsHandle(DeRTF, hMem);
  //SetClipboardData(DeRTF, hMem);

  //TODO: сохранение для сохранения как файл
 {
  GetTempPath(Sizeof(Buffer)-1,Buffer);
  aFile:=Buffer+TableMeta.Name+' '+DateToStr(Date)+'.xml';

  AssignFile(F,aFile);
  Rewrite(F);
  s:= self.GetSelectedAsXML(True);
 {for i:=0 to Metadata.TableCount-1 do
    for j:=0 to Metadata.Tables[i].Fields.Count-1 do
      if  (Metadata.Tables[i].Fields[j].Link=self.TableMeta.ID)
      and (Metadata.Tables[i].Fields[j].LinkType=daCascade) then
        begin
          MetaData.FindActiveDataSet(Metadata.Tables[i].Fields[j].Link, iID);
          if iID>=0 then
            s:=s+#10+MetaData.DataSets[iID].Cache.GetAsXML(True);
        end;
  Write(F,s);
  Close(F);

  iLen := Length(aFile) + 2;
  aFile := aFile + #0#0;
  hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT,
    SizeOf(TDropFiles) + iLen);
  if (hGlobal = 0) then raise Exception.Create('Could not allocate memory.');
  begin
    DropFiles := GlobalLock(hGlobal);
    DropFiles^.pFiles := SizeOf(TDropFiles);
    Move(aFile[1], (PChar(DropFiles) + SizeOf(TDropFiles))^, iLen);
    GlobalUnlock(hGlobal);
    SetClipboardData(CF_HDROP, hGlobal);
  end; }
  finally
    Clipboard.Close;
  end;
  except
    on E: Exception do
      begin
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Copy clipboard error: ' + E.Message);
        {$ENDIF}
        raise;
      end;
  end;
  finally
    ColumnList.Free;
  end;
end;

function TDataCache.InsertFromClipboard: TCacheItem;
var
//  I, N: Integer;
//  StringData: TStringList;
  Index: Integer;
  Template: TDeTemplate;
  Handle: THandle;
  DataPtr: PAnsiChar;
  Node: TDeNode;
  DatasetID: Integer;
  OldRecordID: Variant;
begin
  Result := nil;
  if DeClipboard.HasFormat(DeXML) then
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
        if Assigned(Template.Root) then
          begin
            Node := Template.Root.FindNode('dataset');
            if Assigned(Node) then
              begin
                DatasetID := StrToInt(Node.Attributes['id']);
                Node := Node.FindNode('record');
                if Assigned(Node) then
                  begin
                    if Assigned(FocusedItem) then
                      Index := FocusedIndex
                    else
                      Index := 0;
                    Result := PasteFromNode(Node, DatasetID, OldRecordID, Index);
                  end;
              end;
          end;
      finally
        Template.Free;
      end;
    end;

end;

function TDataCache.CheckChanges(const aCacheItem: TCacheItem; var aFlag: TChangedFlags): Boolean;
var i,N: Integer;
    V: Variant;
    TempDataSet: TDeDataset;
begin
  Assert( Assigned(aCacheItem), 'CheckChanges: CacheItem not assigned');
  Assert( 0 < TableMeta.KField.Count, 'CheckChanges: Key Fields not found');

  try
    TempDataSet:= FDataSet.Database.CreateQuery(qtRow);

    PrepareQueryDescr(TempDataSet.Descr, aCacheItem.Stage);
    TempDataSet.CreateFields(Fields, aCacheItem.Stage);
    TempDataSet.Descr.AddParamCondition
                   (TableMeta.KField.Items[0].Original, TableMeta.KField.Items[0].DataType, opEQ, 'ID', aCacheItem.ID);
    TempDataSet.Open;

    Exclude(aCacheItem.FState, isBaseConflict);
    if TempDataSet.RecordCount = 1 then
      begin
        Exclude(aCacheItem.FState, isBaseNotFound);
        aFlag:= [flExist, flBasketBase, flDataBase, flBasketCurrent, flDataCurrent];

        for i:= Low(TempDataSet.Descr.Fields) to High(TempDataSet.Descr.Fields) do
          begin
            N:= TempDataSet.Descr.Fields[i].TargetIndex;
            V:= TempDataset.Value[i];

            if not DeVarSameValue(aCacheItem.FieldBaseValue[N], V) then
              begin
                Include(aCacheItem.FState, isBaseConflict);
                if N = DeletedSignIndex then Exclude(aFlag, flBasketBase)
                                        else Exclude(aFlag, flDataBase);
              end;

            if not DeVarSameValue(aCacheItem.FieldValue[N], V) then
              begin
                if N = DeletedSignIndex then Exclude(aFlag, flBasketCurrent)
                                        else Exclude(aFlag, flDataCurrent);
              end;
          end;
      end else
      begin
        aFlag:= [];
        Include(aCacheItem.FState, isBaseNotFound);
      end;

      TempDataSet.Close;
  finally
    TempDataSet.Free;
  end;
  Result:= (flExist in aFlag) and (flBasketBase in aFlag) and (flDataBase in aFlag);
end;

function TDataCache.ReFillItem(aCacheItem: TCacheItem; const aStage: TFieldStage; const aInit: Boolean): Boolean;
var i,S,T: Integer;
    V,baseV,newV: Variant;
    TempDataSet: TDeDataset;
    DB: TDeCustomDataBase;
begin
  Assert( Assigned(aCacheItem), 'ReFillItem: CacheItem not assigned');
  Assert( 0 < TableMeta.KField.Count, 'ReFillItem: Key Fields not found');

  Include(aCacheItem.FState, isInitializing);
  try
    if assigned(FDataset) then DB:= FDataSet.Database
                          else DB:= MetaData.DatabaseByID(TableMeta.OwnerTable.Database.ID);

    TempDataSet := DB.CreateQuery(qtRow);
    PrepareQueryDescr(TempDataSet.Descr, aStage);
    TempDataSet.CreateFields(Fields, aStage);
    // Установим связи полей кэша с полями набора данных ....
    try
      TempDataSet.Descr.AddParamCondition(TableMeta.KField.Items[0].Original, TableMeta.KField.Items[0].DataType,
                                                                                             opEQ, 'ID', aCacheItem.ID);
      TempDataSet.Open;
      Exclude(aCacheItem.FState, isBaseConflict);

      TempDataSet.RecNo:= 0;

      for i:=0 to Pred(TempDataSet.Descr.FieldCount) do
        begin
          S:= TempDataSet.Descr.Fields[i].SourceIndex;
          T:= TempDataSet.Descr.Fields[i].TargetIndex;

          if 0 <= S then NewV:= TempDataSet.Value[i]
                    else NewV:= unassigned;

          if aInit or (aCacheItem.Owner.Fields[T].Stage > aCacheItem.Stage)
            then aCacheItem.InitFieldValue(T, NewV)
            else  begin
                   // Ничего не далаем, используем старое значение. Хотя можно сравнить старое и новое значение
                 end;
        end;

      aCacheItem.Stage:= aStage;

      if isBaseConflict in aCacheItem.FState then
        raise EBaseValueConflict.Create(GetTitle('_eRror.Update.ChangedByUserInfo'));
      TempDataSet.Close;
    finally
      TempDataSet.Free;
    end;
    aCacheItem.Stage := aStage;
    Exclude(aCacheItem.FState, isNotInitialized);
  finally
    Exclude(aCacheItem.FState, isInitializing);
  end;
end;

procedure TDataCache.FillItem(const aCacheItem: TCacheItem; const aStage: TFieldStage);
var i,N: Integer;
begin
  Include(aCacheItem.FState, isInitializing);
  if aStage <= BaseStage then
    begin
      FDataSet.RecNo:= aCacheItem.FSourceIndex;

      for i:=0 to Pred(FDataSet.Descr.FieldCount) do
        begin
          N:= FDataSet.Descr.Fields[i].SourceIndex;
          if 0 <= N then aCacheItem.InitFieldValue(FDataSet.Descr.Fields[i].TargetIndex, FDataSet.Value[i])
                    else aCacheItem.InitFieldValue(FDataSet.Descr.Fields[i].TargetIndex, unassigned);
        end;

      aCacheItem.Stage:= BaseStage;//aStage;
    end
  else
    ReFillItem(aCacheItem, aStage, False);

  Exclude(aCacheItem.FState, isInitializing);
  Exclude(aCacheItem.FState, isNotInitialized);
end;

procedure TDataCache.FillItems(const StartIndex, EndIndex: Integer; const aStage: TFieldStage);
var I: integer;
    TmpItem      : TCacheItem;
begin
  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, (1 + EndIndex - StartIndex), 1);
  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, NativeInt(PChar('_dL.reading')), -2);
  try

  if Assigned(FDataSet) then
    for I := StartIndex to EndIndex do
      begin
        TmpItem := Items[I];
        if (isNotInitialized in TmpItem.State) or (TmpItem.Stage < aStage) then
          begin
            SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, i-StartIndex, 0);
            FillItem(TmpItem, aStage);
          end;
      end;

  finally
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  end;
end;

(*
procedure TDataCache.FillSelectedItems(const Stage: TFieldStage);
var
  Index, MinIndex, MaxIndex, SourceIndex: Integer;
  CacheItem: TCacheItem;
begin
  MinIndex := -1;
  MaxIndex := -1;
  for Index := 0 to Pred(SelectedCount) do
    begin
      CacheItem := SelectedItems[Index];
      if Assigned(CacheItem) and ((isNotInitialized in CacheItem.State) or (CacheItem.Stage < Stage)) then
        begin
          SourceIndex := CacheItem.SourceIndex;
          if MinIndex >= 0 then
            if SourceIndex = Succ(MaxIndex) then
              MaxIndex := SourceIndex
            else
              if (MinIndex > 0) and (SourceIndex = Pred(MinIndex)) then
                MinIndex := SourceIndex
              else
                begin
                  {$IFDEF DEBUG}
                  DebugLog('TDataCache.FillSelectedItems(%s): execute FillItems(%d, %d, %s) ...', [
                    StrPas(FieldStages[Stage]), MinIndex, MaxIndex, StrPas(FieldStages[Stage])]);
                  {$ENDIF}
                  FillItems(MinIndex, MaxIndex, Stage);
                  MinIndex := SourceIndex;
                  MaxIndex := SourceIndex;
                end
          else
            begin
              MinIndex := SourceIndex;
              MaxIndex := SourceIndex;
            end;
        end;
    end;
  if MinIndex >= 0 then
    begin
      {$IFDEF DEBUG}
      DebugLog('TDataCache.FillSelectedItems(%s): execute FillItems(%d, %d, %s) last ...', [
        StrPas(FieldStages[Stage]), MinIndex, MaxIndex, StrPas(FieldStages[Stage])]);
      {$ENDIF}
      FillItems(MinIndex, MaxIndex, Stage);
    end;
end;
*)

function TDataCache.CreateAll: Integer;
var
  Index: Integer;
  CacheItem: TCacheItem;
begin
  {$IFDEF DEBUG}
  if Assigned(TableMeta) then
    DebugLog('%s.CreateAll for %s start ...', [ClassName, QuotedStr(TableMeta.Table)])
  else
    DebugLog('%s.CreateAll start ...', [ClassName]);
  {$ENDIF}
  Result := 0;
  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, FCacheItems.Count, 1);
  try
    for Index := 0 to Pred(FCacheItems.Count) do
      begin
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Index, 0);
        CacheItem := FCacheItems[Index];
        if not Assigned(CacheItem) then
          begin
            CacheItem := TCacheItem.Create(Self, Index);
            FCacheItems[Index] := CacheItem;
            Include(CacheItem.FState, isNotInitialized);
            Inc(Result);
          end;
      end;
  finally
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  end;
  {$IFDEF DEBUG}
  if Assigned(TableMeta) then
    DebugLog('%s.CreateAll for %s finish and return %d ...', [ClassName, QuotedStr(TableMeta.Table), Result])
  else
    DebugLog('%s.CreateAll finish and return %d ...', [ClassName, Result]);
  {$ENDIF}
end;

procedure TDataCache.FillAll;
{$IFDEF DEBUG}
var
  DebugString: string;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  if Assigned(TableMeta) then
    DebugLog('%s.FillAll for %s start ...', [ClassName, QuotedStr(TableMeta.Table)])
  else
    DebugLog('%s.FillAll start ...', [ClassName]);
  {$ENDIF}
  FillItems(0, Pred(FCacheItems.Count), BaseStage);
  FIsFillAllItems := True;
  {$IFDEF DEBUG}
  if Assigned(TableMeta) then
    DebugLog('%s.FillAll for %s finish ...', [ClassName, QuotedStr(TableMeta.Table)])
  else
    DebugLog('%s.FillAll finish ...', [ClassName]);
  {$ENDIF}
end;

procedure TDataCache.ClearLinks;
var
   i : integer;
begin
  FCanCreateItems := False;
  for i:=0 to Count-1 do
    if Assigned(Items[i]) then
       Items[i].Data := nil;
  FCanCreateItems := True;
end;

procedure TDataCache.IncludeDependedFields;
var i,j : Integer;
begin
  if Active then Exit;

  for i := 0 to Pred(Fields.Count) do
     if (Fields[i].Calculated) and (Fields[i].Stage <= BaseStage) then
       for j:=0 to Pred(Fields.Count) do
         if BaseStage < Fields[j].Stage then
           if Fields[i].DefaultPostfix.DependOnIdent(Fields[j].Original) then
             if Fields[j].Key then
               Fields[j].Stage := fsKey
             else
               Fields[j].Stage := BaseStage;
end;

function TDataCache.GetFieldValue(const ItemIndex, FieldIndex: Integer): Variant;
begin
  if isNotInitialized in Items[ItemIndex].State then
    begin
      {$IFDEF DEBUG}
      if Fields[FieldIndex].Stage > BaseStage then
        DebugLog('TDataCache.GetFieldValue(%d, %d) ...', [ItemIndex, FieldIndex]);
      {$ENDIF}
      FillItems(ItemIndex, ItemIndex, Fields[FieldIndex].Stage);
    end;
  Result := Items[ItemIndex].FieldValue[FieldIndex];
end;

function TDataCache.IndexByID(const LocateToRecord: Variant): Integer;
var MinIndex, MaxIndex, MidIndex : integer;
    asc, AI, CI: Boolean;
    Value: Variant;
    FM: TFieldMeta;
begin
  Result := -1;
  if Assigned(TableMeta.Database) then
    begin
      AI := TableMeta.Database.AI;
      CI := TableMeta.Database.CI;
    end
  else
    begin
      AI := True;
      CI := True;
    end;
  if (SortList.Count > 0) then FM:= TableMeta.Fields.FindByID(SortList[0].FieldID)
                          else FM:= nil;

  if Assigned(FM) and (FM.DataType <> ftGUID) and
     (FM = TableMeta.KField[0]) and (SortList[0].FDirection <> sdNone) then
    begin  // быстрый поиск
      asc := (SortList[0].FDirection = sdAscending);

      MinIndex:=0;
      MaxIndex:=Count-1;
      Value := ValueForCompare(LocateToRecord, CI, AI);

      while (MaxIndex - MinIndex)>1 do
        begin
          MidIndex := (MaxIndex + MinIndex) Div 2;
          case VarCompareValue(Value, ValueForCompare(Items[MidIndex].ID, CI, AI)) of
           vrEqual      : begin
                            Result:= MidIndex;
                            break;
                          end;
           vrNotEqual   : exit;
           vrLessThan   : if asc then MaxIndex := MidIndex
                                 else MinIndex := MidIndex;
           vrGreaterThan: if asc then MinIndex := MidIndex
                                 else MaxIndex := MidIndex;
          end;
        end;

      if MaxIndex >= MinIndex then
        begin
          if VarCompareValue(Value, ValueForCompare(Items[MinIndex].ID, CI, AI)) = vrEqual then Result := MinIndex else
          if VarCompareValue(Value, ValueForCompare(Items[MaxIndex].ID, CI, AI)) = vrEqual then Result := MaxIndex;
        end;
    end
  else
    begin // перебор
      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Count, 1);
      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, NativeInt(PChar('_dL.locating')), -2);
      try
        Value := ValueForCompare(LocateToRecord, CI, AI);
        for MidIndex := 0 to Pred(Count) do
          begin
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, MidIndex, 0 );
            if VarCompareValue(Value, ValueForCompare(Items[MidIndex].ID, CI, AI)) = vrEqual then
              begin
                Result := MidIndex;
                Break;
              end;
          end;
      finally
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
      end;
    end;
end;

function TDataCache.IndexByItem(aItem : TCacheItem; const CheckFilters: Boolean): Integer;
var i, N, ResultIndex, MinIndex, MaxIndex, MidIndex, R : integer;
    CI, AI, Asc, IsHeavy : Boolean;
    Res: Variant;
    LocateToRecord,LocateToID : Variant;
    Relation : TVariantRelationship;
    aCalculator : TDeCalculator;
    FM: TFieldMeta;
begin

  if assigned(aItem) and CheckFilters then
    begin
      //не оптимизируем, если для проверки фильтра нужна тяжелая операция
      IsHeavy:= False;
      for i:=0 to Filters.Count-1 do
        if (Filters[i].Active) and (Filters[i].Count>0) then
          IsHeavy:= IsHeavy or Filters[i].IsHeavy;

      if Not IsHeavy then
      for i:=0 to Filters.Count-1 do
        if (Filters[i].Active) and (Filters[i].Count>0) then
          begin
            Res:= aItem.ID;
            aCalculator:= TDeCalculator.Create;
            aCalculator.OnGetIdentValue:= CacheItemIdentValue;
            aCalculator.CI:= FTableMeta.Database.CI;

            Res:=aCalculator.Calculate(Filters[i], aItem);

            aCalculator.Free;

            if Res=False then
               begin
                 Result := -1;
            {$IFDEF DEBUG}
                 WriteLog('IndexByItem stopped by Fileter reason.' , False, 'Optimizm');
            {$ENDIF}
                 Exit;
               end;
          end;
    end;

  CI := TableMeta.Database.CI;
  AI := TableMeta.Database.AI;

  Result := -1;

  // нет ключевого поля ищем в списке указателей
  if (TableMeta.KField.Count = 0) then
    begin
      if Assigned(aItem) then
        Result := FCacheItems.IndexOf(aItem);
      Exit;
    end;

  // нет сортировки нет ускорения
  if (SortList.Count=0) then
    begin
      Result:=IndexByID( aItem.ID);
      Exit;
    end;

  N := TableMeta.Fields.IndexByID(SortList.Items[0].FID);
  if -1 < N then FM:= TableMeta.Fields[N]
            else FM:= nil;

  if assigned(FM) and (FM.DataType in NotSortTypes) then
    begin
      Result:=IndexByID( aItem.ID);
      Exit;
    end;

  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, Count, 1);
  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, NativeInt(PChar('_dL.locating')), -2);

  Asc := (SortList[0].FDirection = sdAscending);
  MinIndex := 0;
  MaxIndex := Count-1;
  ResultIndex := -1;

  LocateToID     := aItem.ID;

  if assigned(FM) and (FM.DataType in StringTypes) then
    try
      // ищем запись ResultIndex с совпадающим значением поля сортировки
      LocateToRecord := ValueForCompare(aItem.FieldValue[N], CI, AI);

      while (MaxIndex - MinIndex)>1 do
        begin
          MidIndex := (MaxIndex + MinIndex) Div 2;
          R:=AnsiCompareStr(LocateToRecord, ValueForCompare(Values[MidIndex, N], CI, AI));

          if R=0 then     begin
                            ResultIndex:= MidIndex;
                            break;
                          end else
          if R<0 then     begin
                            if asc then MaxIndex := MidIndex
                                   else MinIndex := MidIndex;
                          end else
          if R>0 then     begin
                            if asc then MinIndex := MidIndex
                                   else MaxIndex := MidIndex;
                          end;

          SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Count + MinIndex - MaxIndex , 0);
        end;

      if (ResultIndex=-1) and (MaxIndex >= MinIndex) then
        begin
          if AnsiCompareStr(LocateToRecord, ValueForCompare(Values[MinIndex, N], CI, AI)) = 0 then ResultIndex := MinIndex  else
          if AnsiCompareStr(LocateToRecord, ValueForCompare(Values[MaxIndex, N], CI, AI)) = 0 then ResultIndex := MaxIndex;
        end;

      if ResultIndex>-1 then
        begin
          MinIndex := ResultIndex;
          while ( MinIndex-1 > -1)    and (AnsiCompareStr(LocateToRecord, ValueForCompare(Values[ MinIndex-1, N], CI, AI)) = 0) do
            Dec(MinIndex);

          MaxIndex := ResultIndex;
          while ( MaxIndex+1 < Count) and (AnsiCompareStr(LocateToRecord, ValueForCompare(Values[ MaxIndex+1, N], CI, AI)) = 0) do
            Inc(MaxIndex);
        end;
    except
      on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog('TDataCache.IndexByItem skip error: ' + E.Message);
          {$ENDIF}
          ResultIndex := -1;
        end;
    end
  else
    try
      // ищем запись ResultIndex с совпадающим значением поля сортировки
      LocateToRecord := aItem.FieldValue[N];

      while (MaxIndex - MinIndex)>1 do
        begin
          MidIndex := (MaxIndex + MinIndex) Div 2;
          if VarIsNull(LocateToRecord)
            then if varIsNull(Values[MidIndex, N]) then Relation:= vrEqual
                                                   else Relation:= vrGreaterThan
            else if varIsNull(Values[MidIndex, N]) then Relation:= vrLessThan
                                                   else Relation:= VarCompareValue(LocateToRecord, Values[MidIndex, N]);
          case Relation of
           vrEqual      : begin
                            ResultIndex:= MidIndex;
                            break;
                          end;
           vrNotEqual   : exit;
           vrLessThan   : if asc then MaxIndex := MidIndex
                                 else MinIndex := MidIndex;
           vrGreaterThan: if asc then MinIndex := MidIndex
                                 else MaxIndex := MidIndex;
          end;

          SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Count + MinIndex - MaxIndex, 0);
        end;

      if MaxIndex >= MinIndex then
        begin
          if VarCompareValue(LocateToRecord, Values[MinIndex, N]) = vrEqual then ResultIndex := MinIndex  else
          if VarCompareValue(LocateToRecord, Values[MaxIndex, N]) = vrEqual then ResultIndex := MaxIndex;
        end;

      if ResultIndex>-1 then
        begin
          MinIndex := ResultIndex;
          while ( MinIndex-1 > -1)    and (VarCompareValue(LocateToRecord, Values[ MinIndex-1, N]) = vrEqual) do
            Dec(MinIndex);

          MaxIndex := ResultIndex;
          while ( MaxIndex+1 < Count) and (VarCompareValue(LocateToRecord, Values[ MaxIndex+1, N]) = vrEqual) do
            Inc(MaxIndex);
        end;

    except
      on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog('TDataCache.IndexByItem skip error: ' + E.Message);
          {$ENDIF}
          ResultIndex := -1;
        end;
    end;

  if ResultIndex>-1 then
    for i:=MinIndex to MaxIndex do
      if VarCompareValue(LocateToID, Items[i].ID) = vrEqual then
        begin
          Result:= i;
          Break;
        end;

  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, -1, -1);

  if Result = -1 then
    begin
      Result:=IndexByID( aItem.ID );
      if Result>-1 then
        WriteLog('IndexByItem:' + TableMeta.Fields[n].Original + '=' + aItem.FieldText(n), False, 'Optimizm');
//DONE: -2014
    end;
end;

function TDataCache.FindByID(IDValue: Variant): TCacheItem;
var N: integer;
begin
  result := nil;
  try
    //проверяем и возможность преобразования и наличие поля IDIndex
    VarAsType(IDValue, FieldTypeVarMap[Fields[IDIndex].DataType]);

    N:=IndexByID(IDValue);
    if N>-1 then result := Items[N];
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('TDataCache.FindByID(%s) skip error: %s', [VariantToString(IDValue), E.Message]);
    {$ENDIF}
  end;
end;

function TDataCache.IndexByValues(aFieldIndexes : array of integer; aValues : array of Variant;
  aSearchFrom : integer = 0; aSearchTo : integer = MaxInt) : Integer;
var I, J, Start, Finish : integer;
    Found               : boolean;
begin
  result := -1;
  Start := Max(aSearchFrom, 0);
  Finish := Min(aSearchTo, Count-1);
  for I := Start to Finish do
    if (not (isDeleted in Items[I].State)) then
    begin
      Found := true;
      for J := 0 to Length(aFieldIndexes)-1 do
        if VarCompareValue(Values[I, aFieldIndexes[J]], aValues[J]) <> vrEqual then
        begin
          Found := false;
          break;
        end;
      if Found then
      begin
        result := I;
        Break;
      end;
    end;
end;

procedure TDataCache.CopyFrom(aCopyFrom : TDataCache);
var I : integer;
begin
  if aCopyFrom.TableMeta = TableMeta then
    for I := 0 to aCopyFrom.Count-1 do
      if not (isDeleted in aCopyFrom[I].State) then
      AddNewItem.Assign(aCopyFrom[I]);
end;

procedure TDataCache.LoadData(aFilter: TFilterItem; const aStage: TFieldStage);
begin
  if Assigned(aFilter) then
    FFilters.AddFilter(aFilter);
  PrepareData(True, aStage);
end;

function TDataCache.AlowFocusedChanging(var NewItem: TCacheItem): boolean;
begin
  result := True;
  if Assigned(FAllowFocusedChanging) then FAllowFocusedChanging(NewItem, result);
  if not Result then NewItem := FocusedItem;
end;

procedure TDataCache.SaveToFileCSV(const FileName: string; const VisibleLevel: TFieldVisible; const DataOnly: Boolean);
var
  ColumnList: TColumnList;
  Stream: TStream;
  S: AnsiString;
  IndexF, IndexR: Integer;
  Field: TFieldMeta;

  function PrepareQuotedString(const Text: string): string;
  begin
    if Pos('"', Text) = 0 then
      Result := Text
    else
      Result := AnsiQuotedStr(Text, '"');
  end;

begin
  ColumnList := TColumnList.Create;
  ColumnList.InitColumnList(Fields);
  try
    Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    try
      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Count, 1);

      if not DataOnly then
        begin
          S := EmptyStr;
          for IndexF := 0 to Pred(ColumnList.Count) do
            begin
              Field := ColumnList[IndexF].Field;
              if Assigned(Field) then
                begin
                  if Field.IsLookup then Continue;
                  if Field.VisibleLevel < VisibleLevel then Continue;
                  if Assigned(Field.LookupPair) then Field := Field.LookupPair;
                  if Length(S) <> 0 then S := S + ';';
                  S := S + PrepareQuotedString(Field.Name);
                end;
            end;
          S:= S + #13#10;

          Stream.WriteBuffer(S[1], Length(S));
        end;

      try
        for IndexR := 0 to Pred(Count) do
          begin
            S := EmptyStr;
            if Assigned(Items[IndexR]) then
              for IndexF := 0 to Pred(ColumnList.Count) do
                begin
                  Field := ColumnList[IndexF].Field;
                  if Assigned(Field) then
                    begin
                      if Field.IsLookup then Continue;
                      if Field.VisibleLevel < VisibleLevel then Continue;
                      if Assigned(Field.LookupPair) then Field := Field.LookupPair;
                      if Length(S) <> 0 then S := S + ';';
                      S := S + PrepareQuotedString(Items[IndexR].FieldTextByName[Field.Original]);
                    end;
                end;
            S := S + #13#10;

            Stream.WriteBuffer(S[1], Length(S));
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, IndexR, 0);
          end;
      finally
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
      end;
    finally
      Stream.Free;
    end;
  finally
    ColumnList.Free;
  end;
end;

procedure TDataCache.SelectAll;
var i : integer;
begin
  for i:=0 to Count-1 do ItemSelect(i);
end;

procedure TDataCache.ClearSelection;
var i : integer;
begin
  for i:=SelectedCount-1 downto 0 do
    FCacheSelectedItems[i].SetSelected(False);
//  for i:=0 to Count-1 do
//    Items[i].Selected := False;
end;

function  TDataCache.GetAsXML(const aFlags: TDataFlags): String;
var FDoc: TXMLDocumentISO;
    FTable: TXMLTable;
    n,i: Integer;
    MAP: Array of Integer;

  function AddRecord(aCacheItem: TCacheItem): IXMLNode;
  var vv: Variant;
      ss: String;
      ii: Integer;
  begin
    Result:= FTable.Item_Add;

    for ii:=0 to Pred(Length(MAP)) do
      if Fields[MAP[ii]].DataType in StringTypes then
        begin
          ss:= aCacheItem.FieldNativeValue[MAP[ii]];
          if (0 < Length(ss)) or Fields[MAP[ii]].NotNull then
            FTable.Item_SetFieldValue(Result, Fields[MAP[ii]].Original, ss);
        end else
        begin
          vv:= aCacheItem.FieldValue[MAP[ii]];
          if Not(VarIsEmpty(vv) or VarIsNull(vv)) then
            FTable.Item_SetFieldValue(Result, Fields[MAP[ii]].Original, vv);
        end;
  end;

begin
  SetLength(Map, 0);

  FDoc:= TXMLDocumentISO.Create(Application.MainForm);
  FDoc.PrepareHeader;
  FTable:= TXMLTable.Create(FDoc, TableMeta.OwnerTable);

  //-------------------------------------------------------------------------------------------------------------------
  for i:=0 to Pred(TableMeta.Fields.Count) do
    if ((dfStoredFields in aFlags) and (TableMeta.Fields[i].IsStored))  or
       ((dfLookupFields in aFlags) and (TableMeta.Fields[i].IsLookup)) or
       ((dfCalculatedFields in aFlags) and (TableMeta.Fields[i].Calculated)) then
      begin
        SetLength(MAP, Succ(Length(MAP)));
        MAP[Pred(Length(MAP))]:= i;
        FTable.Field_Add(TableMeta.Fields[i]);
      end;

  //-------------------------------------------------------------------------------------------------------------------
  if (dfAllRecords in aFlags) then
    begin
      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Count, 1);
      for N:=0 to Pred(Count) do
        begin
          SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, N, 0);
          AddRecord(Items[N]);
        end;
      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
    end else

  if (dfSelectedRecords in aFlags) then
    begin
      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, SelectedCount, 1);
      for N:=0 to Pred(SelectedCount) do
        begin
          SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, N, 0);
          AddRecord(SelectedItems[N]);
        end;
      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
    end else

  if (dfFocusedRecords in aFlags) and assigned(FocusedItem) then
    begin
      AddRecord(FocusedItem);
    end;

  //-------------------------------------------------------------------------------------------------------------------
  FDoc.SaveToXML(Result);
  FDoc.Free;
end;

function TDataCache.GetCacheItems: TCacheItemList;
begin
  if FCacheIndexes.ItemIndex <> -1 then
    Result := FCacheIndexes.Selected.CacheItems
  else
    Result := FCacheItems;
end;

procedure TDataCache.FillFromXML(const aXML: string);
var XMLTable : TXMLTableOld;
    i,j : integer;
begin
  XMLTable := TXMLTableOld.Create(nil);
  try
    XMLTable.XML := aXML;
    XMLTable.Active := True;
    SetItemsCount(XMLTable.RecordCount);
    for i:=0 to Pred(XMLTable.RecordCount) do
    begin
      Include(Items[i].FState, isInitializing);

      for j:=0 to Pred(XMLTable.Fields.Count) do
        begin
//DONE: -2012 исправлено для импорта данных, возможно помешает обмену с DLL
//        Items[i].FieldValue[Fields.IndexByName(XMLTable.Fields[j].Name)]:= XMLTable.FieldValue[j];
          Items[i].InitFieldValue(Fields.IndexByName(XMLTable.Fields[j].Name), XMLTable.FieldValue[j]);
        end;

      Exclude(Items[i].FState, isInitializing);
      Exclude(Items[i].FState, isNotInitialized);

      case XMLTable.RecState of
        rsInserted  :Include(Items[i].FState, isInserted);
        rsModified  :Include(Items[i].FState, isModified);
        rsDeleted   :Include(Items[i].FState, isDeleted );
      end;
      XMLTable.Next;
    end;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('TDataCache.FillFromXML error: ' + E.Message);
    {$ENDIF}
  end;
  XMLTable.Free;
end;

procedure TDataCache.FillFromXML(const aXML: TComponent);
var i,j,N: Integer;
    v: Variant;
    aXMLTable: TXMLTable;
begin
  if not(aXML is TXMLTable) then
    raise Exception.Create('FillFromXML: aXML is not TXMLTable"');
  aXMLTable:= TXMLTable(aXML);

  N:= Count;
  SetItemsCount(N + aXMLTable.Count);

  for i:=0 to Pred(aXMLTable.Count) do
    begin
      Include(Items[N+i].FState, isInitializing);
      for j:=0 to Pred(FieldCount) do
        if Fields[j].IsStored then
          begin
            v:= aXMLTable.Items[i].Attributes[Fields[j].Original];
            Items[N+i].InitFieldValue(j, v);
          end;

      Exclude(Items[N+i].FState, isInitializing);
      Exclude(Items[N+i].FState, isNotInitialized);

      Include(Items[i].FState, isInserted);
    end;
end;

function TDataCache.PrepareClipboardXML(const Level: Integer): string;
begin
  Result := EmptyStr;
  if Assigned(FCacheSelectedItems) then
    Result := FCacheSelectedItems.PrepareClipboardXML(Level);
end;

procedure TDataCache.PrepareData(const AutoOpen: Boolean; const Stage: TFieldStage);
var
  WorkStage: TFieldStage;
begin
  if Assigned(TableMeta) then
    try
      TableMeta.Database.CheckConnection;
      if not Assigned(FDataSet) then
        begin
          FDataSet := TableMeta.Database.CreateQuery(qtSelect);
          FDataSet.FreeNotification(Self);
          FDataSet.OnAfterOpen := AfterDataOpen;
        end;
      WorkStage := Stage;
      if Assigned(TableMeta.Database) and not TableMeta.Database.CanPortionSelect then
        WorkStage := fsMax;
      if TableMeta.ObjectType = otStoredProc then
        WorkStage := fsMax;
      if Length(Trim(TableMeta.SelectSQL)) <> 0 then
        WorkStage := fsMax;

      PrepareQueryDescr(FDataSet.Descr, WorkStage);

      FSubDatasets.Clear;
      Fields.ResetStats(WorkStage);
      FDataSet.CreateFields(Fields, WorkStage);
      // Запомним какого уровня выборка (fsBase, fsFill или fsBlob) лежит в запросе набора данных (FDataSet).
      // Это нужно для того, чтобы при PrepareData(True, fsFill) к примеру не делал дополнительные подзапросы
      // на поля с Stage равным fsFill, т.к. эти поля уже есть в базовой выборке...
      FBaseStage := WorkStage;
    except
      {$IFDEF DEBUG}
      on E: Exception do
        begin
          DebugLog('TDataCache.PrepareData error: ' + E.Message);
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 28, NativeInt(PChar(E.Message)));
        end;
      {$ENDIF}
    end;

  if AutoOpen and QueryOpenData then
    OpenData;
end;

procedure TDataCache.OpenData;
var
  AfterOpenMethod: TNotifyEvent;
  sError: String;
begin
  sError:= EmptyStr;
  if QueryOpenData then
  begin
    AfterOpenMethod := FDataSet.OnAfterOpen;
    FDataSet.OnAfterOpen := nil;
    TableMeta.Database.CheckConnection;
    if TableMeta.Database.Connected then
      try
        FDataSet.Open(true);
        AfterDataOpen(FDataSet);
      except
        on E: Exception do
          begin
            sError:= 'TDataCache.OpenData error: ' + E.Message;
            {$IFDEF DEBUG}
            DebugLog(sError);
            {$ENDIF}
          end;
      end;

    FDataSet.OnAfterOpen := AfterOpenMethod;
    if 0 < Length(sError) then
      raise Exception.Create(sError);
  end;
end;

procedure TDataCache.CloseData;
begin
  if Assigned(FDataSet) then
    FDataSet.Close;
end;

function TDataCache.Update(const Changed: TTypeChanged; const InsertKey: Variant; const LocateIndex: Integer = ltNone): Boolean;
{$IFDEF DEBUG}
const
  ChangeNames: array[TTypeChanged] of PChar = ('mcNone', 'mcUpdate', 'mcInsert', 'mcDelete', 'mcUpdateLibrary');
{$ENDIF}
var key             : Variant;
    keyItem         : TCacheItem;
    RecNumber       : Integer;
    DataActive      : Boolean;
    AfterOpenMethod : TNotifyEvent;
    SelectedKeys: array of Variant;
    Index, RowCount: Integer;
    SelectedItem: TCacheItem;
{$IFDEF DEBUG}
function LocateIndexToString: string;
begin
  case LocateIndex of
    ltItem: Result := 'ltItem';
    ltNone: Result := 'ltNone';
    ltRoot: Result := 'ltRoot';
    ltLast: Result := 'ltLast';
    ltFirst: Result := 'ltFirst';
  else
    Result := IntToStr(LocateIndex);
  end;
end;
{$ENDIF}
begin
  Result := True;
  // + ReLoad cache item ...
  if (Changed = mcUpdate) and not VarIsNull(InsertKey) then
    if VarIsArray(InsertKey) then
      begin
        for Index := VarArrayLowBound(InsertKey, 1) to VarArrayHighBound(InsertKey, 1) do
          begin
            KeyItem := FindByID(VarArrayGet(InsertKey, [Index]));
            // Если нашли в кэше обновляемую запись, то ...
            if Assigned(KeyItem) then
              begin
                // Перечитаем её из БД ...
                Result := ReFillItem(KeyItem, KeyItem.Stage);
                if Index = VarArrayHighBound(InsertKey, 1) then Exit;
              end
            else
              Break;
          end;
      end
    else
      begin
        KeyItem := FindByID(InsertKey);
        // Если нашли в кэше обновляемую запись, то ...
        if Assigned(KeyItem) then
          begin
            // Перечитаем её из БД ...
            Result := ReFillItem(KeyItem, KeyItem.Stage);
            Exit;
          end;
      end;
  // - ReLoad cache item ...

  keyItem :=TCacheItem.Create(self, -1);
  Include(keyItem.FState, isInserted);

  if FUpdateLocks > 0 then
    begin
      FUpdateType := Changed;
      FUpdateKey := InsertKey;
    end
  else
    if Assigned(FDataSet) then
      begin
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Update %s [%d]', [TableMeta.Table, Integer(TableMeta.ID)]);
        {$ENDIF}

{
        RecNumber:= Integer(FTableMeta)
        FTableMeta:= MetaData.GetTableMeta(TableMeta.ID);
        if RecNumber <> Integer(FTableMeta) then
          begin
//TODO:     Надо обновлять все настройки если сменилась версия TTableMeta
          end;
{}

        result := True;
        RecNumber := -1;
        TableMeta.Database.CheckConnection;
        if TableMeta.Database.Connected then
        begin
          AfterOpenMethod := FDataSet.OnAfterOpen;
          FDataSet.OnAfterOpen := nil;
          case Changed of
            mcUpdateLibrary:
              begin
                Key := Null;
                KeyItem:=nil;
              end;
            mcDelete:
              begin
                if Assigned(FocusedItem) then
                  RecNumber:= FocusedIndex;
                if RecNumber = (Count-1) then
                  dec(RecNumber)
                else
                  inc(RecNumber);
                if RecNumber >= 0 then
                  begin
                    Key:= Items[RecNumber].ID;
                    KeyItem.Assign(Items[RecNumber]);
                  end
                else
                  begin
                    Key := Null;
                    KeyItem:=nil;
                  end;
              end;
            mcInsert:
              begin
                Key := InsertKey;
                if VarIsEmpty(Key) then
                  begin
                    if 0<Count then Key:= items[0].ID;
                    for Index:= 1 to Pred(Count) do
                      if VarCompareValue(Key, items[Index].ID) = vrLessThan then
                        Key:= items[Index].ID;
                  end;

                KeyItem:=nil;
              end;
            else
              begin
                if Active and Assigned(FocusedItem) and not (FocusedItem is TRootItem) then
                  begin
                    Key := FocusedItem.ID;
                    KeyItem.Assign(FocusedItem);
                    // 13.04.2016 + Запоминаем выделенные записи ...
                    SetLength(SelectedKeys, SelectedCount);
                    RowCount := Low(SelectedKeys);
                    for Index := 0 to Pred(SelectedCount) do
                      begin
                        SelectedItem := SelectedItems[Index];
                        if Assigned(SelectedItem) then
                          begin
                            SelectedKeys[RowCount] := SelectedItem.ID;
                            Inc(RowCount);
                          end;
                      end;
                    SetLength(SelectedKeys, RowCount);
                    // 13.04.2016 -
                  end
                else
                  begin
                    Key := Null;
                    KeyItem:=nil;
                  end;
              end;
          end;
          DataActive:= FDataSet.Active;
          if DataActive then
            FDataSet.Close;
          {$IFDEF DEBUG}
          if Assigned(TableMeta) then
            DebugLog('TDataCache.Update(%s, %s, %s) %s [%d] ...', [StrPas(ChangeNames[Changed]),
              VariantToString(InsertKey), LocateIndexToString, TableMeta.Table, Integer(TableMeta.ID)])
          else
            DebugLog('TDataCache.Update(%s, %s, %s) ...', [StrPas(ChangeNames[Changed]),
              VariantToString(InsertKey), LocateIndexToString]);
          {$ENDIF}
          PrepareQueryDescr(FDataSet.Descr, BaseStage);
          try
            FSubDatasets.Clear;
            Fields.ResetStats(BaseStage);
            FDataSet.CreateFields(Fields, BaseStage);
            if SecuritySystem.CheckPolicyDataSet(TableMeta.ID, spSelect) then
              FDataSet.Open(true);
            FFocusedItem:= nil;
            FDataSet.OnAfterOpen := AfterOpenMethod;
            AfterDataOpen(Self);
            SyncronizeOnData(key, LocateIndex, KeyItem);
          except
            {$IFDEF DEBUG}
            on E: Exception do
              DebugLog('TDataCache.Update error: ' + E.Message);
            {$ENDIF}
          end;
        end;
        // 13.04.2016 + Восстанавливаем выделенные записи ...
        if Length(SelectedKeys) <> 0 then
          for Index := Low(SelectedKeys) to High(SelectedKeys) do
            begin
              RowCount := IndexByID(SelectedKeys[Index]);
              if RowCount <> -1 then
                begin
                  ItemSelect(RowCount);
                  if Assigned(OnSelectedChange) and Assigned(Items[RowCount]) then
                    OnSelectedChange(Items[RowCount]);
                end;
            end
        else // 13.04.2016 -
          if Assigned(OnSelectedChange) and Assigned(SelectedItems[0]) then
            OnSelectedChange(SelectedItems[0]);
        FUpdateType := mcNone;
        VarClear(FUpdateKey);
      end
    else
      result := False;

  keyItem.Free;
end;

procedure TDataCache.SyncronizeOnData(LocateToRecord : Variant; DefaultIndex: Integer = ltNone; LocateToItem: TCacheItem = nil);
var
  A            : integer;
  AFocusedItem : TCacheItem;
  AIndex : Integer;
begin
  if FDataSet.Active then
    begin
      if VarIsEmpty(LocateToRecord) then
        begin
          AFocusedItem := RootItem;
          AIndex  := ltRoot;
        end else

      if VarIsNull(LocateToRecord)then
        begin
          if Count > 0 then
            AFocusedItem := Items[0]
          else
            AFocusedItem := nil;
          AIndex  := ltFirst;
        end else

      if assigned(FocusedItem) and (FocusedItem.ID = LocateToRecord) then
        begin
          AFocusedItem := FocusedItem;  // ничего не делаем
          AIndex := ltItem;
        end else

        begin
          AFocusedItem := nil;
          AIndex := ltFirst;

          if assigned(LocateToItem) and (SortList.Count>0)
            then A:= IndexByItem(LocateToItem)
            else A:= IndexByID(LocateToRecord);

          if (A>-1) then
            begin
              AFocusedItem := Items[A];
              AIndex := ltItem;
            end else

          if (Count > 0) then
            begin
              if DefaultIndex = ltFirst then
                AFocusedItem := Items[0] else
              if DefaultIndex = ltLast then
                AFocusedItem := Items[Pred(Count)] else
              if DefaultIndex > 0 then
                AFocusedItem := Items[Min(DefaultIndex, Pred(Count))];
            end;
        end;

      if Assigned(OnBeforeSyncronize) then
        OnBeforeSyncronize(AFocusedItem, AIndex);

      if AFocusedItem<>nil then
        begin
          // Очищаем список выделенных записей, надо снять выделение с первой записи
          // которая выделяется автоматически

          if Not AFocusedItem.Selected then
            FCacheSelectedItems.Clear;

          AFocusedItem.Selected:=True;
        end;
      FocusedItem := AFocusedItem;
    end;
end;

procedure TDataCache.ClearSortList;
begin
  FSortList.Clear;
end;

function TDataCache.InsertSortField(aIndex:Integer; aField : TFieldMeta) : TSortInfo;
begin
  if not aField.IsStored then
    aField := TableMeta.KField[0];
  FSortList.Insert(aIndex,TSortInfo.Create(aField));
  result := FSortList[aIndex];
end;

procedure TDataCache.PrepareQueryDescr(aDescr: TCustomQueryDescr; const aFieldStage: TFieldStage);
var I, P       : Integer;
    SortField  : TFieldMeta;
    ParameterItem, VariableItem: TVariableItem;
    RealDataType: TFieldType;
    RealDataSize: Integer;
    s: String;
begin
  if Assigned(aDescr) then
    begin
      aDescr.BeginUpdate;
      try
        aDescr.Clear;
        aDescr.TableMeta := FTableMeta;
        if Assigned(FTableMeta.Database) and (FTableMeta.Database.CanSchema) then
          aDescr.Schema := FTableMeta.Schema;
        { список полей }
        case aDescr.QueryType of
          qtSelect, qtRow: { Выборка записей/записи }
            begin
              s:= TableMeta.UserSelectSQL;

              // Если выборка записи не переопределена, то ...
              if 0 < Length(s) then
                aDescr.SQL:= s
              else
                for I := 0 to Pred(Fields.Count) do
                  if (not Fields[I].IsLookup) and Fields[I].IsStored then
                    if Fields[I].Stage <= aFieldStage then
                      if Assigned(TableMeta.Database) and TableMeta.Database.IsFieldTypeConvert(Fields[I].DataType, Fields[I].DataSize, RealDataType, RealDataSize)
                        then aDescr.AddField(I, Fields[I].Original, RealDataType, RealDataSize)
                        else aDescr.AddField(I, Fields[I].Original);
            end;
          qtInsert: { Вставка новой записи }
            aDescr.SQL := TableMeta.UserInsertSQL;
          qtUpdate: { Обновление существующей записи }
            aDescr.SQL := TableMeta.UserUpdateSQL;
          qtDelete: { Удаление записи }
            aDescr.SQL := TableMeta.UserDeleteSQL;
        end;

        { Параметры хранимых процедур }
        if TableMeta.ObjectType = otStoredProc then
          begin
            for I := 0 to Pred(TableMeta.Parameters.Count) do
              begin
                ParameterItem := TableMeta.Parameters[I];
                if Assigned(ParameterItem) then
                  begin
                    VariableItem := aDescr.Params.FindVariable(ParameterItem.Name);
                    if (LeftStr(ParameterItem.Name, 1) = '@') and not Assigned(VariableItem) then
                      VariableItem := aDescr.Params.FindVariable(Copy(ParameterItem.Name, 2, Length(ParameterItem.Name)));
                    if Assigned(VariableItem) then
                      VariableItem.Value := ParameterItem.Value
                    else
                      begin
                        VariableItem := TVariableItem.Create(ParameterItem.DataType, ParameterItem.Name, ParameterItem.AccessMode);
                        P := -1;
                        try
                          VariableItem.Value := ParameterItem.Value;
                          P := aDescr.Params.Add(VariableItem);
                        finally
                          if P = -1 then VariableItem.Free;
                        end;
                      end;
                  end;
              end;
            {$IFDEF DEBUG}
            TableMeta.Parameters.DebugVariablesLog('StoredProc parameter ...');
            TableMeta.FilterPostfix.DebugPostfixLog('StoredProc filter ...');
            {$ENDIF}
          end;

        { фильтры каскада таблиц }
        if (aDescr.QueryType <> qtRow) and not (TableMeta.ObjectType = otStoredProc) then
          begin
            DoDataSetupFilter(aDescr);

            // Устанавливаем фильтры из Фильр-менеджера!!!
            for I := 0 to Pred(FFilters.Count) do
              if FFilters[I].Active then
                aDescr.AssignFilter(FFilters[I]);
            // Устанавливаем локальные/глобальные фильтры !!!
            for I := 0 to Pred(TableMeta.LinkFilters.Count) do
              if TableMeta.LinkFilters[I].Active then
                 aDescr.AssignFilter(TableMeta.LinkFilters[I]);
            // Дополнительный фильтр контрола (BaseGridForm), v. 16.12
            if Assigned(FControlFilter) and FControlFilter.Active then
              aDescr.AssignFilter(FControlFilter);
          end;

        { указание полей, по которым производится сортировка }
        if (aDescr.QueryType <> qtRow) and not (TableMeta.ObjectType = otStoredProc) then
          for I := 0 to Pred(SortList.Count) do
            if SortList[I].FDirection <> sdNone then
              begin
                SortField:= TableMeta.Fields.FindByID(SortList[I].FieldID);
                if SortField.IsStored then
                  if not (SortField.DataType in NotSortTypes) then
                    aDescr.AddSortField(SortField.Original, SortList[I].Direction);
              end;
      finally
        aDescr.EndUpdate;
      end;
    end;
end;

function TDataCache.AppendRecord(var NewRecordID : Variant) : boolean;
begin
  if Count > 0 then
    result := InsertRecord(Count, NewRecordID)
  else
    result := InsertRecord(0, NewRecordID);
end;

function TDataCache.InsertRecord(aIndex : integer;  var NewRecordID : Variant) : boolean;
var N: Integer;
    NewCacheItem      : TCacheItem;
    ExternalCanInsert : boolean;
begin
  result := false;
  VarClear(NewRecordID);
  Assert(not FIsLibrary, 'Cache was created in read-only mode');
  NewCacheItem := InsertNewItem(aIndex);
  NewCacheItem.Stage := fsMax;
  FDataManager.PrepareRecord(NewCacheItem);
  ExternalCanInsert := true;
  if Assigned(FOnInsertRecord) then
    FOnInsertRecord(NewCacheItem, ExternalCanInsert);
  if ExternalCanInsert
    and FDataManager.CanInsertRecord(NewCacheItem)
    and FDataManager.InsertRecord(NewCacheItem) then
      begin
        NewRecordID := NewCacheItem.ID;
        Update(mcInsert, NewRecordID);
        result := true;
      end
  else
    begin
      N:= self.IndexByItem(NewCacheItem);
      FCacheItems.Delete(N);
      if Assigned(FOnDisplayMessage) then
        FOnDisplayMessage(FDataManager.Errors.GetMessage)
      else
        SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
          NativeInt(PChar(FDataManager.Errors.GetMessage)));
    end;
end;

function TDataCache.UpdateRecord(aItem : TCacheItem) : boolean;
var ExternalCanUpdate : boolean;
begin
  result := false;
  Assert(not FIsLibrary, 'Cache was created in read-only mode');
  ExternalCanUpdate := true;
  if Assigned(FOnUpdateRecord) then
    FOnUpdateRecord(aItem, ExternalCanUpdate);
  if ExternalCanUpdate then
    if FDataManager.CanUpdateRecord(aItem) and FDataManager.UpdateRecord(aItem) then
      begin
        Update(mcUpdate, aItem.ID);

        result := true;
      end
    else
      begin
        Update(mcUpdate, Null);

        if Assigned(FOnDisplayMessage) then
          FOnDisplayMessage(FDataManager.Errors.GetMessage)
        else
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(FDataManager.Errors.GetMessage)));
      end;
end;

function TDataCache.CanDeleteRecord(aItem : TCacheItem) : boolean;
begin
  result := FDataManager.CanDelete(aItem);
  if (not result) and Assigned(FOnDisplayMessage) then
    FOnDisplayMessage(FDataManager.Errors.GetMessage)
end;

function TDataCache.DeleteRecord(aItem : TCacheItem) : boolean;
var ExternalCanDelete : boolean;
    mes               : string;
begin
  result := false;
  Assert(not FIsLibrary, 'Cache was created in read-only mode');
  ExternalCanDelete := true;
  if Assigned(FOnDeleteRecord) then
    FOnDeleteRecord(aItem, ExternalCanDelete);
  if ExternalCanDelete then
  begin
    result := true;
    if FDataManager.CanDelete(aItem) then
    begin
      if not FDataManager.DeleteRecord(aItem) then
      begin
        result := false;
        if Assigned(FOnDisplayMessage) then
          FOnDisplayMessage(FDataManager.Errors.GetMessage)
        else
          SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoError,
            NativeInt(FDataManager.Errors.GetMessage));
      end
    end
    else if FDataManager.Errors.Count > 0 then
    begin
      result := false;
      mes:=aItem.GetCaption;
      if Length(mes)=0 then
        mes:='_Dv.noname';
      FDataManager.Errors.SetEnvironment([mes]);
      if Assigned(FOnDisplayMessage) then
        FOnDisplayMessage(FDataManager.Errors.GetMessage)
      else
        SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoError,
          NativeInt(FDataManager.Errors.GetMessage));
    end;

    if (Not Result) or ((Not aItem.Deleted) and (Assigned(TableMeta.DField)) and (MetaData.ShowRemoved)) then
      Update(mcUpdate, Null {aItem.ID})
    else
      Update(mcDelete, null)
  end;
end;

function TDataCache.PasteFromNode(Node: TDeNode; const DatasetID: Integer; var OldRecordID: Variant; const InsertIndex: Integer): TCacheItem;
var
  Value: Variant;
  i, Index, FieldIndex: Integer;
  SourceStream, TargetStream: TStream;
  Buffer: Pointer;
  function StringToDate(const Text: string): TDateTime;
  begin
    Result := EncodeDateTime(StrToInt(Copy(Text, 1, 4)), StrToInt(Copy(Text, 5, 2)), StrToInt(Copy(Text, 7, 2)),
      StrToInt(Copy(Text, 10, 2)), StrToInt(Copy(Text, 13, 2)), StrToInt(Copy(Text, 16, 2)), StrToInt(Copy(Text, 19, 3)));
  end;
begin
  Result := nil;
  OldRecordID := Unassigned;
  if Assigned(Node) and Assigned(Node.ChildNodes) and Assigned(Fields) then
    begin
      Result := InsertNewItem(InsertIndex);
      if Assigned(Result) then
        begin
          for Index := 0 to Pred(Node.ChildNodes.Count) do
            if SameText(Node.ChildNodes[Index].NodeName, 'field') then
              begin
                FieldIndex:= -1;
                if tablemeta.id = DatasetID
                  then FieldIndex:= Fields.IndexByID(StrToInt(Node.ChildNodes[Index].Attributes['id']))
                  else begin
                         FieldIndex:= Fields.IndexByLink(DatasetID);
                         if FieldIndex <> -1 then
                           begin
                             if not(Fields[FieldIndex].LinkTable.KField[0].ID = StrToInt(Node.ChildNodes[Index].Attributes['id']))
                              then FieldIndex:= -1;
                           end;
                       end;

                if FieldIndex <> -1 then
                  begin
                    case Fields[FieldIndex].DataType of
                      ftByte, ftShortInt, ftWord, ftSmallInt, ftInteger:
                        Value := VarToInt(Node.ChildNodes[Index].Body);
                      ftLargeInt, ftLongWord:
                        Value := StrToInt64(Node.ChildNodes[Index].Body);
                      ftBCD, ftFloat, ftCurrency, ftSingle, ftExtended:
                        Value := StrToFloat(AnsiReplaceText(Node.ChildNodes[Index].Body, '.', FormatSettings.DecimalSeparator));
                      ftTime, ftDate, ftDateTime, ftTimeStamp, ftTimeStampOffset, ftOraTimeStamp:
                        Value := StringToDate(Node.ChildNodes[Index].Body);
                      ftBoolean:
                        Value := VarToType(Node.ChildNodes[Index].Body, ftBoolean);
                      ftBlob:
                        begin
                          SourceStream := TStringStream.Create(Node.ChildNodes[Index].Body);
                          try
                            SourceStream.Seek(0, soFromBeginning);
                            TargetStream := TMemoryStream.Create;
                            try
                              TBase64Encoding.Base64.Decode(SourceStream, TargetStream);
                              TargetStream.Seek(0, soFromBeginning);
                              Value := VarArrayCreate([0, TargetStream.Size], varByte);
                              Buffer := VarArrayLock(Value);
                              try
                                TargetStream.ReadBuffer(Buffer^, TargetStream.Size);
                              finally
                                VarArrayUnlock(Value);
                              end;
                            finally
                              TargetStream.Free;
                            end;
                          finally
                            SourceStream.Free;
                          end;
                        end
                    else
                      Value := XMLDecode(Node.ChildNodes[Index].Body);
                    end;
                    if Assigned(TableMeta) and (TableMeta.ID = DatasetID) then
                      if Fields[FieldIndex].Key then
                        OldRecordID := Value
                      else
                        Result.FieldValue[FieldIndex] := Value
                    else
                      if (Fields[FieldIndex].Link = DatasetID) and SameText(Node.NodeName, 'record') and Node.HasAttribute('id') and not Fields[FieldIndex].IsLookup then
                        begin
                          case Fields[FieldIndex].DataType of
                            ftByte, ftShortInt, ftWord, ftSmallInt, ftInteger, ftLongWord:
                              Value := VarToInt(Node.Attributes['id']);
                            ftLargeInt:
                              Value := StrToInt64(Node.Attributes['id']);
                          else
                            Value := XMLDecode(Node.Attributes['id']);
                          end;
                          Result.FieldValue[FieldIndex] := Value;
                          Break;
                        end;
                  end;
              end;
        end;
    end;
end;

function TDataCache.PasteChildFromNode(RootNode: TDeNode; RootCacheItem: TCacheItem): Integer;
var
  Index, DatasetID, LinkFieldID, OwnerIndex, ChildIndex, N: Integer;
  Node: TDeNode;
  ChildDataCache: TDataCache;
  ChildCacheItem: TCacheItem;
  OldRecordID: Variant;
  ExternalCanInsert: Boolean;
begin
  Result := 0; // Количество вставленных записей
  if Assigned(RootNode) and Assigned(RootNode.ChildNodes) and Assigned(RootCacheItem) and Assigned(RootCacheItem.Owner) then
      for Index := 0 to Pred(RootNode.ChildNodes.Count) do
        begin
          Node := RootNode.ChildNodes[Index];
          if Assigned(Node) and SameText(Node.NodeName, 'dataset') and TryStrToInt(Node.Attributes['id'], DatasetID) and TryStrToInt(Node.Attributes['link'], LinkFieldID) then
            begin
              ChildDataCache := TDataCache.Create(MetaData.GetTableMeta(DatasetID));
              try
                if Assigned(ChildDataCache.Fields) then
                  OwnerIndex := ChildDataCache.Fields.IndexByID(LinkFieldID)
                else if Assigned(ChildDataCache.TableMeta) and Assigned(ChildDataCache.TableMeta.Fields) then
                  OwnerIndex := ChildDataCache.TableMeta.Fields.IndexByID(LinkFieldID)
                else
                  OwnerIndex := -1;
                if (OwnerIndex <> -1) and Assigned(Node.ChildNodes) then
                  begin
                    for ChildIndex := 0 to Pred(Node.ChildNodes.Count) do
                      if SameText(Node.ChildNodes[ChildIndex].NodeName, 'record') then
                        begin
                          ChildCacheItem := ChildDataCache.PasteFromNode(Node.ChildNodes[ChildIndex], DatasetID, OldRecordID);
                          ChildCacheItem.FieldValue[OwnerIndex] := RootCacheItem.ID;
                          ChildDataCache.FDataManager.PrepareRecord(ChildCacheItem);
                          ExternalCanInsert := True;
                          if Assigned(ChildDataCache.FOnInsertRecord) then ChildDataCache.FOnInsertRecord(ChildCacheItem, ExternalCanInsert);
                          if ExternalCanInsert and ChildDataCache.FDataManager.CanInsertRecord(ChildCacheItem) and ChildDataCache.FDataManager.InsertRecord(ChildCacheItem) then
                            begin
                              Inc(Result);
                              PasteChildFromNode(Node.ChildNodes[ChildIndex], ChildCacheItem); // Рекурсия по наборам данных ...
                            end
                          else
                            begin
                              N := ChildDataCache.IndexByItem(ChildCacheItem);
                              ChildDataCache.FCacheItems.Delete(N);
                              if Assigned(ChildDataCache.FOnDisplayMessage) then
                                ChildDataCache.FOnDisplayMessage(ChildDataCache.FDataManager.Errors.GetMessage)
                              else
                                SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(ChildDataCache.FDataManager.Errors.GetMessage)));
                            end;
                        end;
                  end;
              finally
                ChildDataCache.Free;
              end;
            end;
        end;
end;

function TDataCache.PasteRecords: Boolean;
var NewCacheItem      : TCacheItem;
    OldRecordID, NewRecordID: Variant;
    ExternalCanInsert: Boolean;
    ClipID, N: Integer;
    QuantumList: TQuantumList;
    Handle: THandle;
    DataPtr: PAnsiChar;
    Template: TDeTemplate;
    Node: TDeNode;
    Index: Integer;
begin
  Result:= False;
  if CanPasteFromClipboard(True, ClipID) then
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
        if Assigned(Template.Root) then
          begin
            Node := Template.Root.FindNode('dataset');
            if Assigned(Node) and Assigned(Node.ChildNodes) and (Node.ChildNodes.Count <> 0) then
              begin
                SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Node.ChildNodes.Count, 1);
                try
                  MetaData.BeginUpdate;
                  BeginUpdate;
                  try
                    NewRecordID := Unassigned;
                    for Index := 0 to Pred(Node.ChildNodes.Count) do
                      begin
                        if SameText(Node.ChildNodes[Index].NodeName, 'record') then
                          begin
                            NewCacheItem := PasteFromNode(Node.ChildNodes[Index], ClipID, OldRecordID);
                            FDataManager.PrepareRecord(NewCacheItem);
                            ExternalCanInsert := True;
                            if Assigned(FOnInsertRecord) then FOnInsertRecord(NewCacheItem, ExternalCanInsert);
                            VarClear(NewRecordID);
                            if ExternalCanInsert and FDataManager.CanInsertRecord(NewCacheItem) and FDataManager.InsertRecord(NewCacheItem) then
                              begin
                                NewRecordID := NewCacheItem.ID;
                                // Если база скопированных данных соответствует текущей, то ...
                                if Metadata.CheckSignatureFromClipboard then
                                  begin
                                    QuantumList := TQuantumList.Create;
                                    try
                                      QuantumList.PasteFromClipboard;
                                      QuantumList.ModifyObjectData(TableMeta.ID, VarToInt(OldRecordID), VarToInt(NewRecordID));
                                      QuantumList.SaveBatchObjectData;
                                    finally
                                      QuantumList.Free;
                                    end;
                                  end;
                                PasteChildFromNode(Node.ChildNodes[Index], NewCacheItem); // Вставим подчинённые наборы данных!
                              end
                            else
                              begin
                                N := IndexByItem(NewCacheItem);
                                if 0<=N then FCacheItems.Delete(N);
                                if Assigned(FOnDisplayMessage) then
                                  FOnDisplayMessage(FDataManager.Errors.GetMessage)
                                else
                                  SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(FDataManager.Errors.GetMessage)));
                              end;
                          end;
                        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Index, 0);
                      end;
                    if not VarIsClear(NewRecordID) then
                      Update(mcInsert, NewRecordID);
                  finally
                    EndUpdate;
                    MetaData.EndUpdate;
                    Result:= True;
                  end;
                finally
                  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
                end;
              end else
              // Вставка из буфера XML по новому (общему) формату
              begin

              end;
          end;
      finally
        Template.Free;
      end;
    end;
end;

procedure TDataCache.BeginUpdate;
begin
  inc(FUpdateLocks);
end;

procedure TDataCache.EndUpdate;
begin
  Assert(FUpdateLocks > 0, 'Unexpected EndUpdate call');
  dec(FUpdateLocks);
  if (FUpdateLocks = 0) and
     ((not (FUpdateType = mcNone)) or ((not VarIsClear(FUpdateKey)) and (not VarIsNull(FUpdateKey)))) then
    Update(FUpdateType, FUpdateKey);
end;

function TDataCache.SelectedItemsCaption: String;
var i: Integer;
    s: String;
begin
  if SelectedCount=0 then
     Result := EmptyStr
  else
  if SelectedCount=1 then
    begin
      Result := EmptyStr;
      for i:=0 to Fields.Count-1 do
        if (Assigned(TableMeta.NField) and (Fields[I].ID = TableMeta.NField.ID)) or (Fields[I].VisibleLevel = fvLevel3) then
          if Not assigned(Fields[i].LookupPair) or (Fields[i].IsLookup) then
            begin
              s:=SelectedItems[0].FieldText(i);
              if Length(s)>60 then
                Result:=Copy(s,1,50)+'...'+Copy(s, Length(s)-7, 7);

              if (s = EmptyStr) and (Fields[i]=TableMeta.NField) then
                s:= GetTitle('_Dv.Noname');

              s:=Fields[i].Native+' :   '+s;
  //          if Length(Result)>0 then
                Result:=Result+Chr(10);
              Result:=Result+s;
            end;
    end
  else
    begin
      Result := GetTitle(TableMeta.Name)+' [ '+IntToStr(SelectedCount)+' ]';
    end;
end;

function TDataCache.GetLookupPairIndex(const aIndex: Integer): Integer;
begin
  Result:= FFields[aIndex].PairIndex; // FLookupPairIndex[aIndex];
end;

{$IFDEF DEBUG}
procedure TDataCache.DebugFieldsLog(const Text: string; Fields: TFieldsMeta);
  function PrepareDateTime(const DateTime: TDateTime): string;
  begin
    if DateTime = 0 then
      Result := EmptyStr
    else
      Result := FormatDateTime('DD"."MM"."YYYY" "HH":"NN":"SS"."ZZZ', DateTime);
  end;
  function PrepareFieldLog(Field: TFieldMeta): string;
  const
    BooleanNames: array[Boolean] of PChar = ('-', '+');
  var
    FieldName: string;
  begin
    if Assigned(Field) then
      begin
        if Assigned(Field.Owner) then
          begin
            FieldName := Field.Owner.Table;
            if Length(FieldName) <> 0 then FieldName := FieldName + '.';
          end
        else
          FieldName := EmptyStr;
        FieldName := FieldName + Field.Original;
        Result := Format(' %-56s | %9d | %s | %s | %s | %s | %-16s | %2d |', [FieldName, Field.ID,
          StrPas(BooleanNames[Field.Key]), StrPas(BooleanNames[Field.IsStored]),
          StrPas(BooleanNames[Field.IsLookup]), StrPas(BooleanNames[Field.Calculated]),
          StrPas(FieldStages[Field.Stage]), Field.Order]);
        if Field is TFieldCache then
          Result := Result + Format(' %2d | %23s |', [(Field as TFieldCache).PairIndex,
            PrepareDateTime((Field as TFieldCache).FAccessDateTime)])
        else
          Result := Result + Format('    |%25s|', [' ']);
      end
    else
      Result := Format('%s|%s|   |   |   |   |%s|    |    |%s|',
        [DupeString(' ', 58), DupeString(' ', 11), DupeString(' ', 18), DupeString(' ', 25)]);
  end;
  function PrepareFieldsLog: string;
  var
    Index, Count: Integer;
    Value, ColumnValue, TableValue1, TableValue2: string;
  begin
    Result := EmptyStr;
    if Assigned(Fields) then
      begin
        Count := Fields.Count;
        if Assigned(TableMeta) then
          Count := Max(TableMeta.Fields.Count, Count);
        for Index := 0 to Pred(Count) do
          begin
            Result := Result + Format(#13#10'                        | %2d. |', [Index]);
            Result := Result + PrepareFieldLog(Fields[Index]);
            if Assigned(TableMeta) then
              if Index < TableMeta.Fields.Count then
                Result := Result + PrepareFieldLog(TableMeta.Fields[Index])
              else
                Result := Result + PrepareFieldLog(nil);
          end;
        if Length(Result) <> 0 then
          begin
            Value := Format(#13#10'                        +-----+%s+%s+---+---+---+---+%s+----+----+%s+', [DupeString('-', 58),
              DupeString('-', 11), DupeString('-', 18), DupeString('-', 25)]);
            if Assigned(TableMeta) then
              begin
                Value := Value + Format('%s+%s+---+---+---+---+%s+----+----+%s+', [DupeString('-', 58),
                  DupeString('-', 11) , DupeString('-', 18), DupeString('-', 25)]);
                ColumnValue := Format(' %-56s | %-9s | K | S | L | C | %-16s | FO | PI | %-23s |', ['Field', 'ID', 'Stage', 'Last Access']);
                TableValue1 := '+' + DupeString('-', 141);
                TableValue2 := Format(' %-139s |', [TableMeta.ClassName]);
              end
            else
              begin
                ColumnValue := EmptyStr;
                TableValue1 := EmptyStr;
                TableValue2 := EmptyStr;
              end;
            Result :=
              Format(#13#10'                        +%s+'#13#10 +
                           '                        |       %-139s |%s', [
              DupeString('-', 147) + TableValue1, ClassName, TableValue2]) +
              Value +
              Format(#13#10'                        | No  | %-56s | %-9s | K | S | L | C | %-16s | FO | PI | %-23s |',
                ['Field', 'ID', 'Stage', 'Last Access']) +
              ColumnValue + Value + Result + Value;
          end;
      end;
  end;
begin
  DebugLog(Text + PrepareFieldsLog);
end;
{$ENDIF}

procedure TDataCache.FetchAll(OnFetch: TOnFetchQueryDescr);
var
  TempDataSet: TDeDataset;
  OnDataSetupFilter: TOnUpdateQueryDescr;
  Ready: Boolean;
  CacheItem: TCacheItem;
  R, i, Index: Integer;
begin
  if Assigned(OnFetch) and Assigned(FDataSet) then
    begin
      TempDataSet := FDataSet.Database.CreateQuery(qtSelect);
      try
        Ready := True;
        while Ready do
          begin
            TempDataSet.Descr.BeginUpdate;
            try
              OnDataSetupFilter := Self.OnDataSetupFilter;
              try
                Self.OnDataSetupFilter := nil;
                PrepareQueryDescr(TempDataSet.Descr, BaseStage);
              finally
                Self.OnDataSetupFilter := OnDataSetupFilter;
              end;
              TempDataSet.CreateFields(Fields, BaseStage);
              // Установим связи полей кэша с полями набора данных ....
              Ready := OnFetch(TempDataSet.Descr);
            finally
              TempDataSet.Descr.EndUpdate;
            end;
            // Если есть данные для начитывания в кэш, то ...
            if Ready then
              begin
                TempDataSet.Open;
                try
                  for R:=0 to Pred(TempDataSet.RecordCount) do
                    begin
                      TempDataSet.RecNo:= R;
                      CacheItem := TCacheItem.Create(Self, -1);
                      try
                        FCacheItems.Insert(Count, CacheItem);
                      except
                        CacheItem.Free;
                        raise;
                      end;
                      Include(CacheItem.FState, isInitializing);
                      for i:= Low(TempDataSet.Descr.Fields) to High(TempDataSet.Descr.Fields) do
                        CacheItem.InitFieldValue(TempDataSet.Descr.Fields[i].TargetIndex, TempDataSet.Value[i]);
                      // Запоминаем состояние загруженной записи ...
                      CacheItem.Stage:= BaseStage;
                      Exclude(CacheItem.FState, isInitializing);
                    end;
                finally
                  TempDataSet.Close;
                end;
              end;
          end;
      finally
        TempDataSet.Free;
      end;
    end;
end;

{ TDataCache.TClipboardExcelStyle }

constructor TDataCache.TClipboardExcelStyle.Create(const AStyleID: Integer; const AFormat: string; const AColor: TColor; const ABackground: TColor; const ABorders: TClipboardExcelCellBorders; const AStyles: TFontStyles);
begin
  FStyleID := AStyleID;
  FFormat := AFormat;
  FColor := AColor;
  FBackground := ABackground;
  FStyles := AStyles;
  FBorders := ABorders;
end;

function TDataCache.TClipboardExcelStyle.GetName: string;
begin
  if FStyleID = 0 then
    Result := 'Default'
  else
    Result := SysUtils.Format('s%u', [FStyleID]);
end;

function TDataCache.TClipboardExcelStyle.GetText: string;
  function ColorToWeb(const Color: TColor): string;
  var
    Value: Integer;
  begin
    Value := ColorToRGB(Color);
    Result := '#' + IntToHex(GetRValue(Value), 2) + IntToHex(GetGValue(Value), 2) + IntToHex(GetBValue(Value), 2);
  end;
  {
  function PrepareBorders: string;
  const
    Positions: array[TClipboardExcelCellBorder] of PChar = ('Left', 'Top', 'Right', 'Bottom');
  var
    Index: TClipboardExcelCellBorder;
  begin
    Result := EmptyStr;
    for Index := Low(Index) to High(Index) do
      if Index in Borders then
        Result := Result + #13#10'        <Border ss:Position="' + StrPas(Positions[Index]) +
          '" ss:LineStyle="Continuous" ss:Weight="1" ss:Color="#F0F0F0"/>';
    if Length(Result) <> 0 then
      Result := '      <Borders>' + Result + #13#10'      </Borders>'#13#10;
  end;
  {}
begin
  Result := #13#10'    <Style ss:ID="' + Name + '"';
  if FStyleID = 0 then Result := Result + ' ss:Name="Normal"';
  Result := Result + '>'#13#10 +
    '      <Alignment ss:Vertical="Bottom"/>'#13#10 + { PrepareBorders + {}
    '      <Font ss:FontName="Calibri" x:CharSet="204" x:Family="Swiss" ss:Size="11"';
  if Color <> clNone then
    Result := Result + ' ss:Color="' + ColorToWeb(Color) + '"';
  if fsBold in Styles then
    Result := Result + ' ss:Bold="1"';
  if fsItalic in Styles then
    Result := Result + ' ss:Italic="1"';
  if fsUnderline in Styles then
    Result := Result + ' ss:Underline="Single"';
  if fsStrikeOut in Styles then
    Result := Result + ' ss:StrikeThrough="1"';
  Result := Result + '/>'#13#10;
  if Background <> clNone then
    Result := Result + '      <Interior ss:Color="' + ColorToWeb(Background) + '" ss:Pattern="Solid"/>'#13#10;
  if Length(Format) <> 0 then
    Result := Result + '      <NumberFormat ss:Format="' + Format + '"/>'#13#10;
  Result := Result + '    </Style>';
end;

{ TDataCache.TClipboardExcelStyles }

constructor TDataCache.TClipboardExcelStyles.Create;
var
  Style: TClipboardExcelStyle;
begin
  FList := TObjectList.Create;
  Style := TClipboardExcelStyle.Create(FNextStyleID);
  if FList.Add(Style) = -1 then Style.Free;
  Inc(FNextStyleID);
//  FNextStyleID := 70;
end;

destructor TDataCache.TClipboardExcelStyles.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TDataCache.TClipboardExcelStyles.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDataCache.TClipboardExcelStyles.GetItem(const Index: Integer): TClipboardExcelStyle;
begin
  Result := FList[Index] as TClipboardExcelStyle;
end;

function TDataCache.TClipboardExcelStyles.GetText: string;
var
  Index: Integer;
begin
  Result := EmptyStr;
  for Index := 0 to Pred(Count) do
    Result := Result + Item[Index].Text;
end;

function TDataCache.TClipboardExcelStyles.IndexOf(const Format: string; const Color, Background: TColor; const Borders: TClipboardExcelCellBorders; const Styles: TFontStyles): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Pred(Count) do
    if SameText(Item[Index].Format, Format) and (Item[Index].Color = Color) and (Item[Index].Background = Background) and (Item[Index].Borders = Borders) and (Item[Index].Styles = Styles) then
      begin
        Result := Index;
        Break;
      end;
end;

function TDataCache.TClipboardExcelStyles.Add(const Format: string; const Color: TColor; const Background: TColor; const Borders: TClipboardExcelCellBorders; const Styles: TFontStyles): Integer;
var
  Style: TClipboardExcelStyle;
begin
  Result := IndexOf(Format, Color, Background, Borders, Styles);
  if Result = -1 then
    begin
      Style := TClipboardExcelStyle.Create(FNextStyleID, Format, Color, Background, Borders, Styles);
      Result := FList.Add(Style);
      if Result = -1 then
        Style.Free
      else
        Inc(FNextStyleID);
    end;
end;

function TDataCache.TClipboardExcelStyles.PrepareStyle(const Index: Integer): string;
var
  Style: TClipboardExcelStyle;
begin
  Result := EmptyStr;
  if (Index >= 0) and (Index < Count) then
    begin
      Style := Item[Index];
      if Assigned(Style) then
        if Style.StyleID <> 0 then
          Result := ' ss:StyleID="' + Style.Name + '"';
    end;
end;

{ TDataCacheList }

function TDataCacheList.Get(const Index: Integer): TDataCache;
begin
  Result := TDataCache(inherited Items[Index]);
end;

function TDataCacheList.ItemsByID(const ID: Integer; autoCreate: Boolean = False): TDataCache;
var
  Index: Integer;
  Table: TTableMeta;
begin
  Result := nil;
  for Index := 0 to Pred(Count) do
    if Items[Index].TableMeta.ID = ID then
      Exit(Items[Index]);

  if not Assigned(Result) and autoCreate then
    begin
      Table := MetaData.GetTableMeta(ID);
      Assert(Assigned(Table), 'Invalid TTableMeta ID');
      Result := TDataCache.Create(Table);
      Add(Result);
    end;
end;

{ TFieldCache }

procedure TFieldCache.Assign(Source: TObject; const FullAssign: Boolean);
begin
  inherited Assign(Source, FullAssign);
  if Assigned(Source) and FullAssign then
    begin
      if Source is TFieldMeta then
        DefaultPostfix.Assign((Source as TFieldMeta).DefaultPostfix);
    end;
end;

constructor TFieldCache.Create(AOwner: TFieldsCache);
begin
  inherited Create;
  FOwner := AOwner;
  Assert(Assigned(FOwner), 'Owner is nil');
  FPairIndex := -1;
  FDatasetIndex := -1;
  FFieldIndex := -1;
  FIDIndex := -1;
  FState := [];
end;

destructor TFieldCache.Destroy;
begin
  FIDs := nil;
  inherited Destroy;
end;

function TFieldCache.GetDataCache: TDataCache;
begin
  Result := Owner.Owner;
end;

function TFieldCache.GetDataSet: TDeDataset;
begin
  if DatasetIndex <> -1 then
    Result := DataCache.FSubDatasets[DatasetIndex]
  else
    Result := nil;
end;

function TFieldCache.GetRowCount: Integer;
begin
  Result := Length(FIDs);
end;

function TFieldCache.GetRowID(const aIndex: Integer): Variant;
begin
  if (aIndex < Low(FIDs)) or ( High(FIDs) < aIndex) then Exit(Unassigned);

  Result := FIDs[aIndex];
  if VarIsEmpty(Result) then
    if Assigned(DataSet) and DataSet.Active then
      begin
        DataSet.RecNo := aIndex;
        Result := DataSet.Value[0];
        FIDs[aIndex] := Result;
      end;
end;

function TFieldCache.GetRowValue(const aIndex, aField: Integer): Variant;
begin
  Result := Unassigned;
  if (aIndex < 0) or (RowCount <= aIndex) then Exit(Unassigned);

  if Assigned(DataSet) and DataSet.Active then
    begin
      DataSet.RecNo := aIndex;
      Result:= DataSet.Value[aField];
      // Если ID ещё не читали для этой записи, то читаем сразу при получении значения (для уменьшения SEEK`ов в наборе данных)
      if VarIsEmpty(FIDs[aIndex]) then
        FIDs[aIndex] := DataSet.Value[0];
    end;
end;

function TFieldCache.UpdateStats: Boolean;
var
  MinDateTime: TDateTime;
begin
  Result := (RowCount = 0) and IsStored and not IsLookup;
  if Result then
    begin
      MinDateTime := FFirstDateTime;
      FFirstDateTime := FLastDateTime;
      FLastDateTime := Now;

      if FFirstDateTime = 0 then
        Result := False else

      if MilliSecondSpan(FLastDateTime, FFirstDateTime) < 50 then
          begin
            Result := True;
            {$IFDEF DeDEBUG}
            Funcs.WriteLog(Format('Calculate statistic %s ms [%s > %s] ...', [
              FloatToStr( MilliSecondSpan(FLastDateTime, FFirstDateTime) ),
              FormatDateTime('HH":"NN":"SS"."ZZZ', FLastDateTime),
              FormatDateTime('HH":"NN":"SS"."ZZZ', FFirstDateTime)
              ]));
            {$ENDIF}
          end else

        if MinDateTime = 0 then
          Result := False else

        begin
          Result := ( MilliSecondSpan(FLastDateTime, MinDateTime) <= 1000 );
          {$IFDEF DeDEBUG}
          Funcs.WriteLog(Format('Calculate statistic %s ms [%s > %s >  %s] ...', [
            FloatToStr(MilliSecondSpan(FLastDateTime, MinDateTime) ),
            FormatDateTime('HH":"NN":"SS"."ZZZ', FLastDateTime),
            FormatDateTime('HH":"NN":"SS"."ZZZ', FFirstDateTime),
            FormatDateTime('HH":"NN":"SS"."ZZZ', MinDateTime)
            ]));
          {$ENDIF}
        end;
    end;
end;

function TFieldCache.ResetStats: Boolean;
begin
  Result := (FFirstDateTime <> 0) or (FLastDateTime <> 0);
  FFirstDateTime := 0;
  FLastDateTime := 0;
end;

function TFieldCache.IndexByRowID(const RowID: Variant): Integer;
var
  LowIndex, HighIndex, ScanIndex: Integer;
  {$IFDEF DEBUG}
  JumpCount, ReadCount: Integer;
  {$ENDIF}
  WorkID: Variant;
begin
  Result := -1;
  if RowCount <> 0 then
    begin
      LowIndex := Low(FIDs);
      HighIndex := High(FIDs);
      {$IFDEF DEBUG}
      JumpCount := 1;
      ReadCount := 0;
      {$ENDIF}
      while (HighIndex - LowIndex) > 1 do
        begin
          ScanIndex := (HighIndex + LowIndex) div 2;
          {$IFDEF DEBUG}
          if VarIsEmpty(FIDs[ScanIndex]) then Inc(ReadCount);
          {$ENDIF}
          WorkID := GetRowID(ScanIndex);
          if VarIsEmpty(WorkID) then
            Break
          else
            case VarCompareValue(RowID, WorkID) of
              vrEqual: { Равны }
                begin
                  Result := ScanIndex;
                  {$IFDEF DEBUG}
                  DebugLog('TFieldCache.IndexByRowID for %s: found %s record in %d jumps (%d reads) and return %d ...', [
                    QuotedStr(Original), QuotedStr(VarToStr(RowID)), JumpCount, ReadCount, Result]);
                  {$ENDIF}
                  Break;
                end;
              vrLessThan: { Меньше }
                HighIndex := ScanIndex;
              vrGreaterThan: { Больше }
                LowIndex := ScanIndex;
            else
              Break;
            end;
          {$IFDEF DEBUG}
          Inc(JumpCount);
          {$ENDIF}
        end;
      // Если всё ещё не нашли запись и можно посмотреть "крайние", то ...
      if (HighIndex >= LowIndex) and (Result = -1) then
        begin
          {$IFDEF DEBUG}
          if VarIsEmpty(FIDs[LowIndex]) then Inc(ReadCount);
          {$ENDIF}
          WorkID := GetRowID(LowIndex);
          if VarCompareValue(RowID, WorkID) = vrEqual then
            begin
              Result := LowIndex;
              {$IFDEF DEBUG}
              DebugLog('TFieldCache.IndexByRowID for %s: found %s record in %d jumps (%d reads) and return %d low ...', [
                QuotedStr(Original), QuotedStr(VarToStr(RowID)), JumpCount, ReadCount, Result]);
              {$ENDIF}
            end
          else
            begin
              {$IFDEF DEBUG}
              if VarIsEmpty(FIDs[HighIndex]) then Inc(ReadCount);
              {$ENDIF}
              WorkID := GetRowID(HighIndex);
              if VarCompareValue(RowID, WorkID) = vrEqual then
                begin
                  Result := HighIndex;
                  {$IFDEF DEBUG}
                  DebugLog('TFieldCache.IndexByRowID for %s: found %s record in %d jumps (%d reads) and return %d high ...', [
                    QuotedStr(Original), QuotedStr(VarToStr(RowID)), JumpCount, ReadCount, Result]);
                  {$ENDIF}
                end;
            end;
        end;
    end;
end;

function TFieldCache.LinkSubDataset(const DatasetIndex, KeyFieldIndex: Integer): Boolean;
var
  DataCache: TDataCache;
  SubDatasets: TSubDatasets;
  DataSet: TDeDataset;
begin
  UnlinkSubDataset; // Ситуаций повторного линка по определению не должно быть, но всё же лучше подстаховаться ...
  DataCache := Self.DataCache;
  Result := Assigned(DataCache);
  if Result then
    begin
      SubDatasets := DataCache.FSubDatasets;
      Result := Assigned(SubDatasets);
      if Result then
        begin
          Result := (DatasetIndex >= 0) and (DatasetIndex < SubDatasets.Count);
          if Result then
            begin
              DataSet := SubDatasets[DatasetIndex];
              Result := Assigned(DataSet);
              if Result then
                begin                            // единственное место в коде, где нужно количество полей в Source DataSet
                  Result := (KeyFieldIndex >= 0);// and (KeyFieldIndex < DataSet.FieldsCount);
                  if Result then
                    begin
                      FFieldIndex := DataSet.IndexByName(Original);
                      Result := FFieldIndex <> -1;
                      if Result then
                        begin
                          FDatasetIndex := DatasetIndex;
                          FIDIndex := KeyFieldIndex;
                          SetLength(FIDs, DataSet.RecordCount);
                          Include(FState, fsLinkedDataSet);
                        end;
                    end;
                end;
            end;
        end;
    end;
end;

procedure TFieldCache.UnlinkSubDataset;
var
  Index, FieldIndex, RowIndex: Integer;
  CacheItem: TCacheItem;
begin
  if Assigned(Owner) and Assigned(Owner.Owner) then
     begin
       FieldIndex := Owner.IndexOf(Self);
       if FieldIndex <> -1 then
         for Index := 0 to Pred(Owner.Owner.Count) do
           begin
             CacheItem := Owner.Owner.Items[Index];
             if Assigned(CacheItem) then
               if CacheItem.State * [isNotInitialized, isInserted] = [] then
                 if VarIsEmpty(CacheItem.FFields[FieldIndex]) then
                   begin
                     RowIndex := IndexByRowID(CacheItem.ID);
                     if RowIndex <> -1 then
                       begin
                         {$IFDEF DEBUG}
                         //DebugLog('Update vertical field value %d for cache item %d ...', [FieldIndex, Index]);
                         {$ENDIF}
                         CacheItem.FFields[FieldIndex] := GetRowValue(RowIndex, FFieldIndex);
                       end;
                   end;
           end;
     end;
  FDatasetIndex := -1;
  FFieldIndex := -1;
  FIDIndex := -1;
  FIDs := nil;
  Exclude(FState, fsLinkedDataSet);
end;

function TFieldCache.GetCalculating: Boolean;
begin
  Result := fsCalculating in FState;
end;

procedure TFieldCache.SetCalculating(const Value: Boolean);
begin
  if Value then
    Include(FState, fsCalculating)
  else
    Exclude(FState, fsCalculating);
end;

{ TFieldsCache }

constructor TFieldsCache.Create(AOwner: TDataCache);
var
  Index: Integer;
  Field: TFieldCache;
begin
  inherited Create(True);
  FOwner := AOwner;
  Assert(Assigned(FOwner), 'Owner is nil');
  if Assigned(FOwner.TableMeta) and Assigned(FOwner.TableMeta.Fields) then
    begin
      AssignFields(FOwner.TableMeta.Fields);
      for Index := 0 to Pred(Count) do
        begin
          Field := Items[Index];
          if Assigned(Field) then
            begin
              if Assigned(Field.LookupPair) then
                Field.FPairIndex := IndexByID(Field.LookupPair.ID);
              if Assigned(FOwner.TableMeta.KField) and (FOwner.TableMeta.KField.Count = 0) then
                Field.Stage := fsKey
              else if FOwner.FIsLibrary then
                if Field.Key then
                  Field.Stage := fsKey
                else if Assigned(FOwner.TableMeta.DField) and (FOwner.TableMeta.DField.ID = Field.ID) then
                  Field.Stage := fsBase
                else if Assigned(FOwner.TableMeta.NField) and (Field.ID = FOwner.TableMeta.NField.ID) then
                  Field.Stage := fsBase
                else if Assigned(FOwner.TableMeta.ColorField) and (Field.ID = FOwner.TableMeta.ColorField.ID) then
                  Field.Stage := fsBase
                else if Assigned(FOwner.TableMeta.IconField) and (Field.ID = FOwner.TableMeta.IconField.ID) then
                  Field.Stage := fsBase
                else if Field.DataType in BinaryTypes then
                  Field.Stage := fsBlob
                else if Field.DataType in [ftMemo, ftWideMemo] then
                  Field.Stage := fsBlob
                else
                  Field.Stage := fsFull
              else
                if Field.Key then
                  Field.Stage := fsKey
                else if Assigned(FOwner.TableMeta.OField) and (FOwner.TableMeta.OField.ID = Field.ID) then
                  Field.Stage := fsBase
                else if Assigned(FOwner.TableMeta.GField) and (FOwner.TableMeta.GField.ID = Field.ID) then
                  Field.Stage := fsBase
                else if Assigned(FOwner.TableMeta.NField) and (FOwner.TableMeta.NField.ID = Field.ID) then
                  Field.Stage := fsBase
                else if Assigned(FOwner.TableMeta.PField) and (FOwner.TableMeta.PField.ID = Field.ID) then
                  Field.Stage := fsBase
                else if Assigned(FOwner.TableMeta.DField) and (FOwner.TableMeta.DField.ID = Field.ID) then
                  Field.Stage := fsBase
                else if Assigned(FOwner.TableMeta.IconField) and (FOwner.TableMeta.IconField.ID = Field.ID) then
                  Field.Stage := fsBase
                else if Assigned(FOwner.TableMeta.ColorField) and (FOwner.TableMeta.ColorField.ID = Field.ID) then
                  Field.Stage := fsBase
                else if ((Field.OriginalVisibleLevel in [fvLevel2, fvLevel3]) or (Field.VisibleLevel = fvLevel2)) and not Field.IsLookup then
                  Field.Stage := fsBase
                else if Field.DataType in BinaryTypes then
                  Field.Stage := fsBlob
                else if Field.DataType in [ftMemo, ftWideMemo] then
                  Field.Stage := fsBlob
                else if Assigned(FOwner.SortList) and (FOwner.SortList.IndexByID(Field.ID) <> -1) then
                  Field.Stage := fsBase
                else
                  Field.Stage := fsFull;
            end;
        end;
    end;
end;

function TFieldsCache.GetItem(const Index: Integer): TFieldCache;
begin
  Result := inherited Items[Index] as TFieldCache;
end;

procedure TFieldsCache.AssignFields(Source: TFieldsMeta);
var
  Index, LookupIndex: Integer;
  Field: TFieldCache;
begin
  Clear;
  if Assigned(Source) then
    begin
      // копирование списка полей
      for Index := 0 to Pred(Source.Count) do
        begin
          Field := TFieldCache.Create(Self);
          Field.Assign(Source[Index]);
          Add(Field);
        end;
      // переназначение парных ссылок
      for Index := 0 to Pred(Count) do
        if Assigned(Items[Index].LookupPair) then
          begin
            LookupIndex := IndexByID(Items[Index].LookupPair.ID);
            if LookupIndex <> -1 then
              Items[Index].LookupPair := Items[LookupIndex]
            else
              Items[Index].LookupPair := nil;
          end;
    end;
end;

procedure TFieldsCache.ResetStats(const Stage: TFieldStage);
var
  Index: Integer;
  {$IFDEF DeDEBUG}
  FieldsString: string;
  {$ENDIF}
begin
  {$IFDEF DeDEBUG}
  FieldsString := EmptyStr;
  {$ENDIF}
  for Index := 0 to Pred(Count) do
    if Items[Index].IsStored and not Items[Index].IsLookup then
      if Items[Index].Stage <= Stage then
        if Items[Index].ResetStats then
          begin
            {$IFDEF DeDEBUG}
            if Length(FieldsString) <> 0 then
              FieldsString := FieldsString + ', ';
            FieldsString := FieldsString + QuotedStr(Items[Index].Original);
            {$ENDIF}
          end;
  {$IFDEF DeDEBUG}
  if Length(FieldsString) <> 0 then
    Funcs.WriteLog('Reset statistic %s for %s ...', [StrPas(FieldStages[Stage]), FieldsString]);
  {$ENDIF}
end;

procedure TFieldsCache.SortByOrder;
var
  Index: Integer;
  Field: TFieldCache;
begin
  inherited SortByOrder;
  for Index := 0 to Pred(Count) do
    begin
      Field := Items[Index];
      if Assigned(Field) and Assigned(Field.LookupPair) then
        Field.FPairIndex := IndexByID(Field.LookupPair.ID);
    end;
end;

{
procedure TFieldsCache.SynchronizeUserFields(Target: TFieldsMeta);
var
  Index: Integer;
  SourceField, TargetField: TFieldMeta;
begin
  if Assigned(Target) then
    for Index := 0 to Pred(Target.Count) do
      begin
        TargetField := Target[Index];
        if Assigned(TargetField) then
          begin
            SourceField := FindByID(TargetField.ID);
            if Assigned(SourceField) then
              begin
                TargetField.Order := SourceField.Order;
                TargetField.Width := SourceField.Width;
                TargetField.VisibleLevel := SourceField.VisibleLevel;
              end;
          end;
      end;
end;
}

procedure TFieldsCache.ChangeStages(const aFieldNames: array of string; const Stage: TFieldStage);
var
  Index, FieldIndex: Integer;
begin
  for Index := Low(aFieldNames) to High(aFieldNames) do
    begin
      FieldIndex := IndexByName(aFieldNames[Index]);
      if FieldIndex <> -1 then
        Items[FieldIndex].Stage := Stage;
    end;
end;

{ TSubDatasets }

constructor TSubDatasets.Create(AOwner: TDataCache);
begin
  inherited Create(True);
  FOwner := AOwner;
  Assert(Assigned(FOwner), 'Owner is nil');
end;

function TSubDatasets.GetItem(const Index: Integer): TDeDataset;
begin
  Result := inherited Items[Index] as TDeDataset;
end;

function TSubDatasets.Add(const OwnerFieldIndexList: array of Integer; const NewStage: TFieldStage): Integer;
var
  DataSet: TDeDataset;
  Index, KeyIndex, AppendCount: Integer;
  FieldCache: TFieldCache;
  {$IFDEF DeDEBUG}
  FieldsString: string;
  {$ENDIF}
begin
  Result := -1;
  if Assigned(Owner.FDataSet) and Assigned(Owner.FDataSet.Database) and Assigned(Owner.TableMeta) and Assigned(Owner.TableMeta.KField) and (Owner.TableMeta.KField.Count = 1) then
    begin
      DataSet := Owner.FDataSet.Database.CreateQuery(qtSelect);
      try
        DataSet.Descr.BeginUpdate;
        try
          DataSet.Descr.Table := Owner.TableMeta.Table;
          with Owner.TableMeta.KField[0] do
            DataSet.Descr.AddField(0, Original, DataType, DataSize);
          {$IFDEF DeDEBUG}
          FieldsString := EmptyStr;
          {$ENDIF}
          for Index := Low(OwnerFieldIndexList) to High(OwnerFieldIndexList) do
            if Owner.Fields[OwnerFieldIndexList[Index]].IsStored and (Owner.Fields[OwnerFieldIndexList[Index]].ID <> Owner.TableMeta.KField[0].ID) then
              begin
                DataSet.Descr.AddField(Owner.Fields[OwnerFieldIndexList[Index]].Original,
                                       Owner.Fields[OwnerFieldIndexList[Index]].DataType);
                {$IFDEF DeDEBUG}
                if Length(FieldsString) <> 0 then
                  FieldsString := FieldsString + ', ';
                FieldsString := FieldsString + Format('%s [%d]', [Owner.Fields[OwnerFieldIndexList[Index]].Original, OwnerFieldIndexList[Index]]);
                {$ENDIF}
              end;
          { фильтры каскада таблиц }
          Owner.DoDataSetupFilter(DataSet.Descr);
          if Assigned(Owner.Filters) then
            for Index := 0 to Pred(Owner.Filters.Count) do
              if Owner.Filters[Index].Active then
                DataSet.Descr.AssignFilter(Owner.Filters[Index]);
          // Добавим сортировку по ключевому полю ...
          DataSet.Descr.AddSortField(Owner.TableMeta.KField[0].Original);
        finally
          DataSet.Descr.EndUpdate;
        end;
        // Откроем набор данных ...
        DataSet.Open(True);
      except
        FreeAndNil(DataSet);
        raise;
      end;
      Result := inherited Add(DataSet);
      if Result <> -1 then
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Append sub dataset ''%s'' [%d] for %s fetched %d rows ...', [Owner.TableMeta.Table, Result, FieldsString, DataSet.RecordCount]);
          {$ENDIF}
          KeyIndex := DataSet.IndexByName(Owner.TableMeta.KField[0].Original);
          AppendCount := 0;
          for Index := Low(OwnerFieldIndexList) to High(OwnerFieldIndexList) do
            begin
              FieldCache := Owner.Fields[OwnerFieldIndexList[Index]];
              if Assigned(FieldCache) and FieldCache.IsStored then
                begin
                  // Если слинковали поле, то ...
                  if FieldCache.LinkSubDataset(Result, KeyIndex) then
                    begin
                      // Изменим Stage поля на новый ...
                      FieldCache.Stage := NewStage;
                      // Увеличим счётчик прилинкованных полей DataSet`а ...
                      Inc(AppendCount);
                    end;
                end;
            end;
          // Если не добавили не одного поля, то удалим лишний DataSet ...
          if AppendCount = 0 then DataSet.Free;
        end
      else
        DataSet.Free;
    end;
end;

function TSubDatasets.Add(const OwnerFieldIndex: Integer; const NewStage: TFieldStage): Integer;
var
  List: array of Integer;
  Index, Count: Integer;
  _Now : TDateTime;
begin
  _Now := Now;
  SetLength(List, Owner.Fields.Count);
  List[0] := OwnerFieldIndex;
  Count := 1;
  for Index := 0 to Pred(Owner.Fields.Count) do
    if (OwnerFieldIndex <> Index) and (Owner.Fields[Index].Stage > Owner.BaseStage) and
       Owner.Fields[Index].IsStored and (MilliSecondSpan(_Now, Owner.Fields[Index].FAccessDateTime) < 50) then
      begin
        List[Count] := Index;
        Inc(Count);
      end;
  SetLength(List, Count);
  Result := Add(List, NewStage);
end;

procedure TSubDatasets.Delete(const Index: Integer);
var
  FieldIndex: Integer;
  FieldCache: TFieldCache;
begin
  if Assigned(Owner.Fields) then
    for FieldIndex := 0 to Pred(Owner.Fields.Count) do
      begin
        FieldCache := Owner.Fields[FieldIndex];
        if FieldCache.DatasetIndex = Index then
          begin
            {$IFDEF DEBUG}
            DebugLog('TSubDatasets.Delete(%d) for %s ... ', [Index, FieldCache.Original]);
            {$ENDIF}
            FieldCache.UnlinkSubDataset;
          end
        else
          if FieldCache.DatasetIndex > Index then
            begin
              {$IFDEF DEBUG}
              DebugLog('TSubDatasets.Delete(%d) for %s change dataset index to %d ... ', [Index, FieldCache.Original, Pred(Index)]);
              {$ENDIF}
              Dec(FieldCache.FDatasetIndex);
            end;
      end;
  {$IFDEF DeDEBUG}
  if Assigned(Owner.TableMeta) then
    Funcs.WriteLog('Delete sub dataset ''%s'' [%d] ...', [Owner.TableMeta.Table, Index])
  else
    Funcs.WriteLog('Delete sub dataset %d ...', [Index]);
  {$ENDIF}
  inherited Delete(Index);
end;

procedure TSubDatasets.Clear;
begin
  while Count <> 0 do Delete(Pred(Count));
end;

{ TCacheIndex }

constructor TCacheIndex.Create(AOwner: TCacheIndexes);
begin
  Assert(Assigned(AOwner), 'Data cache indexes not assigned!');
  FCacheItems := TCacheItemList.Create(False);
  FSortList := TSortList.Create;
  FFilterItem := TFilterItem.Create;
  FOwner := AOwner;
end;

destructor TCacheIndex.Destroy;
begin
  FFilterItem.Free;
  FSortList.Free;
  FCacheItems.Free;
  inherited Destroy;
end;

function TCacheIndex.GetCount: Integer;
begin
  Result := FCacheItems.Count;
end;

function TCacheIndex.GetItem(const Index: Integer): TCacheItem;
begin
  Result := FCacheItems[Index];
end;

procedure TCacheIndex.Initialize;
var
  Index: Integer;
begin
  FCacheItems.Clear;
  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Owner.DataCache.CacheOwnedItems.Count, 1);
  try
    Owner.DataCache.FillAll;
    FCacheItems.Capacity := Owner.DataCache.CacheOwnedItems.Capacity;
    for Index := 0 to Owner.DataCache.CacheOwnedItems.Count - 1 do
      begin
        FCacheItems.Add(Owner.DataCache.CacheOwnedItems[Index]);
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Index, 0);
      end;
  finally
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  end;
end;

function TCacheIndex.Compare(const LeftIndex, RightIndex: Integer): Integer;
{
  Результат функции: 0 - оба кэша одинаковые
                    -1 - первый кэш меньше второго
                     1 - первый кэш больше второго
}
const
  CSTR_LESS_THAN = 1;
  CSTR_EQUAL = 2;
  CSTR_GREATER_THAN = 3;
const
  Directions: array[Boolean] of ShortInt = (-1, 1);
var
  Index, FieldIndex: Integer;
  Field: TFieldMeta;
  LeftText, RightText: string;
  LeftValue, RightValue: Variant;
  {$IFDEF DEBUG}
  DebugLeft, DebugRight: string;
  {$ENDIF}
begin
  Result := 0;
  {$IFDEF DEBUG}
  DebugLeft := EmptyStr;
  DebugRight := EmptyStr;
  {$ENDIF}
  for Index := 0 to Pred(FSortList.Count) do
    begin
      Field := FOwner.FDataCache.TableMeta.Fields.FindByID(FSortList[Index].FieldID);
      FieldIndex := FOwner.DataCache.Fields.IndexByID(Field.ID);
      if Field.DataType in StringTypes then
        begin
          LeftText := FCacheItems[LeftIndex].FieldText(FieldIndex);
          RightText := FCacheItems[RightIndex].FieldText(FieldIndex);
          {$IFDEF DEBUG}
          if Length(DebugLeft) <> 0 then DebugLeft := DebugLeft + ', ';
          DebugLeft := DebugLeft + QuotedStr(LeftText);
          if Length(DebugRight) <> 0 then DebugRight := DebugRight + ', ';
          DebugRight := DebugRight + QuotedStr(RightText);
          {$ENDIF}
          case CompareString(LOCALE_USER_DEFAULT, 0, PChar(LeftText), Length(LeftText), PChar(RightText), Length(RightText)) of
            1: { Первая строка меньше второй }
              begin
                Result := Directions[FSortList[Index].Direction = sdDescending];
                Break;
              end;
            3: { Первая строка больше второй }
              begin
                Result := Directions[FSortList[Index].Direction <> sdDescending];
                Break;
              end;
          end;
        end
      else
        begin
          LeftValue := {FOwner.DataCache.}CacheItems[LeftIndex].FieldValue[FieldIndex];
          RightValue := {FOwner.DataCache.}CacheItems[RightIndex].FieldValue[FieldIndex];
          {$IFDEF DEBUG}
          if Length(DebugLeft) <> 0 then DebugLeft := DebugLeft + ', ';
          DebugLeft := DebugLeft + VariantToString(LeftValue);
          if Length(DebugRight) <> 0 then DebugRight := DebugRight + ', ';
          DebugRight := DebugRight + VariantToString(RightValue);
          {$ENDIF}
          if VarIsNull(LeftValue) and Not VarIsNull(RightValue) then
            begin
              Result := Directions[FSortList[Index].Direction = sdDescending];
              Break;
            end else
          if VarIsNull(RightValue) and Not VarIsNull(LeftValue) then
            begin
              Result := Directions[FSortList[Index].Direction <> sdDescending];
              Break;
            end
          else
            case VarCompareValue(LeftValue, RightValue) of
              vrLessThan: { Первое значение меньше второго }
                begin
                  Result := Directions[FSortList[Index].Direction = sdDescending];
                  Break;
                end;
              vrGreaterThan: { Втрое значение больше второго }
                begin
                  Result := Directions[FSortList[Index].Direction <> sdDescending];
                  Break;
                end;
            end;
        end;
    end;
  {$IFDEF DEBUG}
  WriteLog('%s.Compare(%d, %d): [%s] ? [%s] = %d', [ClassName, LeftIndex, RightIndex, DebugLeft, DebugRight, Result], 'Compare');
  {$ENDIF}
end;

procedure TCacheIndex.QuickSort(LowIndex, HighIndex: Integer; OnCompare: TCompareEvent);
var
  I, J, P: Integer;
begin
  repeat
    I := LowIndex;
    J := HighIndex;
    P := (LowIndex + HighIndex) shr 1;                 
    repeat
      while OnCompare(I, P) < 0 do Inc(I);            
      while OnCompare(J, P) > 0 do Dec(J);           
      if I <= J then                               
      begin                                       
        if I <> J then                           
          FCacheItems.Exchange(I, J);           
        if P = I then
          P := J                              
        else if P = J then                   
          P := I;                          
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if LowIndex < J then QuickSort(LowIndex, J, OnCompare);
    LowIndex := I;
  until I >= HighIndex;
end;

procedure TCacheIndex.CheckSortItems;
var
  Stage: TFieldStage;
  Index: Integer;
  Field: TFieldCache;
begin
  Stage := Owner.DataCache.BaseStage;
  for Index := 0 to Pred(SortList.Count) do
    if -1 < SortList[Index].FieldID then
      begin
        Field := Owner.DataCache.Fields.FindByID(SortList[Index].FieldID) as TFieldCache;
        if Assigned(Field) and (Field.Stage > Stage) then
          Stage := Field.Stage;
      end;
  if Stage > Owner.DataCache.BaseStage then
    Owner.DataCache.FillItems(0, Pred(Owner.DataCache.CacheOwnedItems.Count), Stage);
end;

procedure TCacheIndex.CheckFilterItems;
var
  Stage: TFieldStage;
  Index: Integer;
  Field: TFieldCache;
begin
  Stage := Owner.DataCache.BaseStage;
  for Index := 0 to Pred(FilterItem.Count) do
    if FilterItem[Index].ItemType = piIdent then
      begin
        Field := Owner.DataCache.Fields.FindByName(FilterItem[Index].Ident) as TFieldCache;
        if Assigned(Field) and (Field.Stage > Stage) then
          Stage := Field.Stage;
      end;
  if Stage > Owner.DataCache.BaseStage then
    Owner.DataCache.FillItems(0, Pred(Owner.DataCache.CacheOwnedItems.Count), Stage);
end;

procedure TCacheIndex.Sort;
begin
  if Count > 1 then
    QuickSort(0, Pred(Count), Compare);
end;

procedure TCacheIndex.Filter;
var
  Index: Integer;
  CacheItem: TCacheItem;
begin
  for Index := Pred(Count) downto 0 do
    begin
      CacheItem := FCacheItems[Index];
      if Assigned(CacheItem) and CacheItem.Calculate(FilterItem, True) then
        // Все нормально. Оставляем кэш итем в списке ...
      else
        FCacheItems.Delete(Index);
    end;
end;

function TCacheIndex.IndexOf(CacheItem: TCacheItem): Integer;
begin
  Result := FCacheItems.IndexOf(CacheItem);
end;

{$IFDEF DEBUG}
procedure TCacheIndex.DebugIndexLog(const Text: string);
var
  Value: string;
  function PrepareSortFieldsLog: string;
  const
    SortNames: array[TDSSortDirection] of PChar = ('sdNone', 'sdAscending', 'sdDescending');
  var
    Index, IndexSize: Integer;
  begin
    Result := EmptyStr;
    if Assigned(FSortList) then
      begin
        IndexSize := Length(IntToStr(FSortList.Count));
        if IndexSize < 2 then IndexSize := 2;
        for Index := 0 to Pred(FSortList.Count) do
          Result := Format(#13#10'                        ¦ %*d ¦ %d ¦ %-16s ¦',
            [
            IndexSize, Index,
            FSortList[Index].FieldID,
            StrPas(SortNames[FSortList[Index].Direction])
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
  end;
begin
  if Assigned(FFilterItem) then
    begin
      Value := FFilterItem.PrepareDebugPostfixLog;
      if Length(Value) <> 0 then
        Value := #13#10'                        Process filters:' + Value;
    end
  else
    Value := EmptyStr;
  DebugLog(Text + PrepareSortFieldsLog + Value);
end;
{$ENDIF}

{ TCacheIndexes }

constructor TCacheIndexes.Create(ADataCache: TDataCache);
begin
  Assert(Assigned(ADataCache), 'Data cache not assigned!');
  FList := TObjectList.Create;
  FDataCache := ADataCache;
  FItemIndex := -1;
end;

destructor TCacheIndexes.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TCacheIndexes.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCacheIndexes.GetItem(const Index: Integer): TCacheIndex;
begin
  Result := FList[Index] as TCacheIndex;
end;

function TCacheIndexes.GetSelected: TCacheIndex;
begin
  if (FItemIndex >= 0) and (FItemIndex < Count) then
    Result := Items[FItemIndex]
  else
    Result := nil;
end;

procedure TCacheIndexes.Clear;
var
  Updated: Boolean;
begin
  Updated := FItemIndex <> -1;
  FList.Clear;
  FItemIndex := -1;
  if Updated then DoChange;
end;

procedure TCacheIndexes.Delete(const Index: Integer);
var
  CacheIndex: TCacheIndex;
begin
  CacheIndex := Selected;
  FList.Delete(Index);
  FItemIndex := FList.IndexOf(CacheIndex);
  if Assigned(CacheIndex) and (FItemIndex = -1) then DoChange;
end;

function TCacheIndexes.IndexOf(ExpressionItem: TExpressionItem; SortList: TSortList): Integer;
var
  Index: Integer;
begin
  Result := -1;
  if Assigned(ExpressionItem) then
    if Assigned(SortList) then
      begin
        // Указан фильтр и сортировка ...
        for Index := 0 to Pred(Count) do
          if Items[Index].SortList.CompareTo(SortList) and Items[Index].FilterItem.IdenticalTo(ExpressionItem) then
            begin
              Result := Index;
              Break;
            end;
      end
    else
      begin
        // Указан фильтр и БЕЗ сортировки ...
        for Index := 0 to Pred(Count) do
          if (Items[Index].SortList.Count = 0) and Items[Index].FilterItem.IdenticalTo(ExpressionItem) then
            begin
              Result := Index;
              Break;
            end;
      end
  else
    if Assigned(SortList) then
      begin
        // Указана сортировка и БЕЗ фильтра ...
        for Index := 0 to Pred(Count) do
          if Items[Index].SortList.CompareTo(SortList) and (Items[Index].FilterItem.Count = 0) then
            begin
              Result := Index;
              Break;
            end;
      end;
end;

function TCacheIndexes.Add(SortList: TSortList; const NewOnly: Boolean): Integer;
var
  CacheIndex: TCacheIndex;
begin
  Result := -1;
  if Assigned(SortList) and (SortList.Count <> 0) then
    begin
      if not NewOnly then Result := IndexOf(nil, SortList);
      if Result = -1 then
        begin
          CacheIndex := TCacheIndex.Create(Self);
          try
            CacheIndex.SortList.Assign(SortList);
            CacheIndex.CheckSortItems;
            CacheIndex.Initialize;
            CacheIndex.Sort;
            Result := FList.Add(CacheIndex);
          finally
            if Result = -1 then CacheIndex.Free;
          end;
        end;
    end;
end;

function TCacheIndexes.Add(ExpressionItem: TExpressionItem; const NewOnly: Boolean): Integer;
var
  CacheIndex: TCacheIndex;
begin
  Result := -1;
  if Assigned(ExpressionItem) and (ExpressionItem.Count <> 0) then
    begin
      if not NewOnly then Result := IndexOf(ExpressionItem, nil);
      if Result = -1 then
        begin
          CacheIndex := TCacheIndex.Create(Self);
          try
            CacheIndex.FilterItem.Assign(ExpressionItem);
            CacheIndex.CheckFilterItems;
            CacheIndex.Initialize;
            CacheIndex.Filter;
            Result := FList.Add(CacheIndex);
          finally
            if Result = -1 then CacheIndex.Free;
          end;
        end;
    end;
end;

function TCacheIndexes.Add(FilterItem: TFilterItem; const NewOnly: Boolean): Integer;
var
  CacheIndex: TCacheIndex;
begin
  Result := -1;
  if Assigned(FilterItem) and FilterItem.Active and (FilterItem.Count <> 0) then
    begin
      if not NewOnly then Result := IndexOf(FilterItem, nil);
      if Result = -1 then
        begin
          CacheIndex := TCacheIndex.Create(Self);
          try
            CacheIndex.Initialize;
            CacheIndex.FilterItem.Assign(FilterItem);
            CacheIndex.CheckFilterItems;
            CacheIndex.Filter;
            Result := FList.Add(CacheIndex);
          finally
            if Result = -1 then CacheIndex.Free;
          end;
        end;
    end;
end;

function TCacheIndexes.Add(ExpressionItem: TExpressionItem; SortList: TSortList; const NewOnly: Boolean): Integer;
begin
  if Assigned(ExpressionItem) and (ExpressionItem.Count <> 0) then
    begin
      if NewOnly then
        Result := -1
      else
        Result := IndexOf(ExpressionItem, SortList);
      if Result = -1 then
        begin
          Result := Add(ExpressionItem, True);
          if Assigned(SortList) and (Result <> -1) then
            begin
              Items[Result].SortList.Assign(SortList);
              Items[Result].CheckSortItems;
              Items[Result].Sort;
            end;
        end;
    end
  else
    Result := Add(SortList, NewOnly);
end;

function TCacheIndexes.Add(FilterItem: TFilterItem; SortList: TSortList; const NewOnly: Boolean): Integer;
begin
  if Assigned(FilterItem) then
    begin
      if NewOnly then
        Result := -1
      else
        Result := IndexOf(FilterItem, SortList);
      if Result = -1 then
        begin
          Result := Add(FilterItem, True);
          if Assigned(SortList) and (Result <> -1) then
            begin
              Items[Result].SortList.Assign(SortList);
              Items[Result].CheckSortItems;
              Items[Result].Sort;
            end;
        end;
    end
  else
    Result := Add(SortList);
end;

function TCacheIndexes.Add(FiltersManager: TFiltersManager; SortList: TSortList): Integer;
var
  FilterItem, WorkFilterItem: TFilterItem;
  PostfixItem: TPostfixItem;
  FilterIndex, PostfixIndex, AppendCount: Integer;
begin
  if Assigned(FiltersManager) then
    begin
      FilterItem := TFilterItem.Create;
      try
        AppendCount := 0;
        for FilterIndex := 0 to Pred(FiltersManager.Count) do
          begin
            WorkFilterItem := FiltersManager[FilterIndex];
            if Assigned(WorkFilterItem) and (WorkFilterItem.Count <> 0) then
              begin
                for PostfixIndex := 0 to Pred(WorkFilterItem.Count) do
                  begin
                    PostfixItem := FilterItem.AddPostfixItem;
                    PostfixItem.Assign(WorkFilterItem[PostfixIndex]);
                  end;
                Inc(AppendCount);
              end;
          end;
        for FilterIndex := 1 to Pred(AppendCount) do
          FilterItem.AddOperation(opAnd);
        Result := Add(FilterItem, True);
        if Assigned(SortList) and (Result <> -1) then
          begin
            Items[Result].SortList.Assign(SortList);
            Items[Result].CheckSortItems;
            Items[Result].Sort;
          end;
      finally
        FilterItem.Free;
      end;
    end
  else
    Result := Add(SortList);
end;

procedure TCacheIndexes.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TCacheIndexes.Select(const Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Count) and Assigned(Items[Index]);
  if Result then
    begin
      FItemIndex := Index;
      DoChange;
    end;
end;

function TCacheIndexes.Select(CacheIndex: TCacheIndex): Boolean;
var
  Index: Integer;
begin
  if Assigned(CacheIndex) then
    Index := FList.IndexOf(CacheIndex)
  else
    Index := -1;
  Result := Index <> -1;
  if Result then
    begin
      FItemIndex := Index;
      DoChange;
    end;
end;

procedure TCacheIndexes.SetItemIndex(const Value: Integer);
begin
  if FItemIndex <> Value then
    begin
      FItemIndex := Value;
      DoChange;
    end;
end;

procedure TCacheIndexes.UnSelect;
begin
  if FItemIndex <> -1 then
    begin
      FItemIndex := -1;
      DoChange;
    end;
end;

{$REGION 'Initialization & Finalization unit procedures ...'}
procedure Startup;
var
  WindowColor, WindowTextColor: TColor;
begin
  {$IFDEF DEBUG}
  DebugLog('DataCacheUnit unit initialization ...');
  {$ENDIF}
  WindowColor := ColorToRGB(clWindow);
  WindowTextColor := ColorToRGB(clWindowText);
  DeletedRecordColor := RGB(
                            (GetRValue(WindowTextColor) + GetRValue(WindowColor)) div 2,
                            (GetGValue(WindowTextColor) + GetGValue(WindowColor)) div 2,
                            (GetBValue(WindowTextColor) + GetBValue(WindowColor)) div 2
                           );
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DataCacheUnit unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}


{ TConfigCacheItem }

function TConfigCacheItem.GetFieldValue(const Index: Integer): Variant;
begin
  if Owner.Fields[Index].IsLookup then                     Result:= Inherited GetFieldValue(Index) else
  if Owner.Fields[Index].Original = fldBaseId then         Result:= 0 else
  if Owner.Fields[Index].Original = fldBaseGUID then       Result:= SampleMTDatabaseGUID else
  if Owner.Fields[Index].Original = fldBaseAlias then      Result:= GetTitle('_dL.metadata') else
  if Owner.Fields[Index].Original = fldBaseType then       Result:= CurrentConfig.DBType else
  if Owner.Fields[Index].Original = fldBaseServer then     Result:= CurrentConfig.Server else
  if Owner.Fields[Index].Original = fldBaseDatabase then   Result:= CurrentConfig.Database else
  if Owner.Fields[Index].Original = fldBaseCharset then    Result:= CurrentConfig.CodePage else
  if Owner.Fields[Index].Original = fldBaseConnectStr then Result:= CurrentConfig.ConnectString else
  if Owner.Fields[Index].Original = fldBaseLogin then      Result:= CurrentConfig.Login else
  if Owner.Fields[Index].Original = fldBasePassword then   Result:= null else
  if Owner.Fields[Index].Original = fldBaseReadOnly then   Result:= False else
  if Owner.Fields[Index].Original = fldBaseDeleted then    Result:= 0 else
  if Owner.Fields[Index].Original = fldBaseDefault then    Result:= 1 else
  if Owner.Fields[Index].Original = fldBaseSolution then   Result:= 0 else
                                                           Result:= null;
end;

function TConfigCacheItem.GetImageIndex(const aIndex: Integer): Integer;
begin
  Result:= 15;
end;

constructor TConfigCacheItem.Create(AOwner: TDataCache; const aSourceIndex: Integer);
begin
  inherited;
  exclude(FState, isNotInitialized);
  include(FState, isReadOnly);
end;

function TConfigCacheItem.GetFieldBaseValue(const Index: Integer): Variant;
begin
  Result:= GetFieldValue(Index);
end;

procedure TConfigCacheItem.SetFieldValue(const Index: Integer; const Value: Variant);
begin
  SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo,
                              NativeInt(PChar('_dT.field ' + Owner.Fields[Index].Original + #10 + '_dE.readonly')));
end;

initialization
  Startup;

finalization
  Shutdown;

end.

