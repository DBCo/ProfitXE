unit DeMeta;

interface

uses
  System.Types,  System.UITypes, Windows, SysUtils, Classes, Controls, Contnrs, Graphics, DB, ClipBrd,
  System.Generics.Collections, JPEG, Variants, Menus, Actions, ActnList,
  DeTypes, DeDB, DeParser;

const
  CategoryPresent = 'acPresentation';
  CategoryFilter  = 'acFilter';
  CategoryView    = 'acView';
  CategorySort    = 'acSort';
  CategoryGroup   = 'acGroup';
  CategorySplit   = 'acSplit';
  CategoryDisplay = 'acDisplay';
  CategoryMetrics = 'acMetrics';
  CategoryResults = 'acResults';
  LabelWidth = 12;

  cViewType = 'ViewType';
  cViewTypes = 'ViewTypes';
  cViewTypeList = 'List';
  cViewTypeTree = 'Tree';
  cViewTypeCalendar = 'Calendar';
  cViewTypeChart = 'Chart';
  cViewTypeMap = 'Map';
  cViewTypePivot = 'Pivot';

  // UDataSet Types
  uvImport = 1;
  uvFilterLast = 2;
  uvFields = 3;
  uvFilterCommon = 4;
  uvFilterGlobal = 5;
  uvFilterPersonal = 6;
  uvFindLast = 7;
  uvMap = 8;

type

  TWhenCalc = (wcAuto, wcNone, wcOne, wcEvery);

  // информация о параметре
  TParamItem = class
  private
    FName: string;
    FValue: Variant;
    FAsString: String;
    FDataType: TFieldType;
    FWhenCalc: TWhenCalc;
  public
    property Name: string read FName write FName;
    property Value: Variant read FValue write FValue;
    property AsString: String read FAsString write FAsString;
    property DataType: TFieldType read FDataType write FDataType;
    property WhenCalc: TWhenCalc read FWhenCalc write FWhenCalc;
    procedure Assign(Source: TObject);
  end;

  // список параметров
  TParamList = class(TObjectList)
  private
    function InnerGetParam(const Index: Integer): TParamItem;
    procedure InnerSetParam(const Index: Integer; const Param: TParamItem);
  public
    property Items[const Index: Integer]: TParamItem read InnerGetParam write InnerSetParam; default;
    function FindParam(const ParamName: string): TParamItem;
    function ParamIndex(const ParamName: string): Integer;
{$IFDEF DEBUG}
    procedure DebugParamsLog(const Text: string);
{$ENDIF}
    procedure MergeFromSystem;
    procedure AddParam(aName: String; aValue: Variant);
    procedure SetParam(aName: String; aValue: Variant);
  end;

  { TTreeMeta }
  TTreeMeta = class(TObjectList)
  private
    FId: Variant;
    FOwnerID: Variant;
    FName: string;
    FOwner: TTreeMeta;
    function GetItem(const Index: Integer): TTreeMeta;
    procedure PutItem(const Index: Integer; aChild: TTreeMeta);
    function GetLevel: Integer;
    // проверяет, не является ли данный узел родительским узлом переданного узла
    function IsOwner(aNode: TTreeMeta): boolean;
  protected
    procedure Notify(Ptr: pointer; Action: TListNotification); override;
  public
    constructor Create(const Owner: TTreeMeta = nil);
    destructor Destroy; override;
    property Items[const Index: Integer]: TTreeMeta read GetItem write PutItem; default;
    property ID: Variant read FId write FId;
    property OwnerID: Variant read FOwnerID write FOwnerID;
    property Name: string read FName write FName;
    property Owner: TTreeMeta read FOwner write FOwner;
    property Level: Integer read GetLevel;
    /// <summary>Функция ищет узел в дереве и его поддеревьях</summary>
    function FindNode(const aNodeID: Variant): TTreeMeta;
    function NodeFound(aNode: TTreeMeta): boolean;
    /// <summary>перестраивает список в дерево</summary>
    /// <returns>Возвращает True если перестроить удалось, иначе false - при перестроении возникли ошибки</returns>
    function MakeTree(aErrorsList: TObjectList = nil): boolean; overload;
    function MakeTree(RootID: Variant; aErrorsList: TObjectList = nil): boolean; overload;
    /// <summary>копирует значения из передаваемого класса</summary>
    procedure Assign(Source: TObject); virtual;
    // <summary>развертывание структуры в плоский список</summary>
    procedure ConvertToList(aList: TList);
{$IFDEF DeDEBUG}
    function Display(const aPrefix: string = ''): string;
{$ENDIF}
  end;

  /// <summary>список элементов с возможностью включения и исключения</summary>
  TControlsList = class(TObjectList)
  private
    function GetControl(const Index: Integer): TControl;
    procedure SetControl(const Index: Integer; aControl: TControl);
  public
    constructor Create;
    property Items[const Index: Integer]: TControl read GetControl
      write SetControl;
    procedure AddControl(aControl: TControl);
    procedure RemoveControl(aControl: TControl);
  end;

  TTableMeta = class;

  TFieldFlagStateType = (csKey, // Ключевое поле
    csUnique, // Уникальное поле
    csNotNull, // Обязательное поле
    csReadOnly, // Поле только для чтения
    csCalculated, // Вычисляемое поле
    csGUID // Есть GUID для поля ( IsNull(FGUID) = False )
    );
  TFieldFlagState = set of TFieldFlagStateType;

  /// <summary>описание полей данных в метаструктуре</summary>

  TFieldMeta = class
  private
    FId: Variant;
    FOriginal: string; // имя поля в базе данных
    FOwner: TTableMeta; // таблица, в которую входит поле
    FName: string; // имя поля до локализации
    FDescription: string; // описание
    // FReadOnly         : Boolean;        // =true, если редактирование поля запрещено
    // FKey              : Boolean;        // =true, если поле ключевое
    // FUnique           : Boolean;        // =true, если значение поля уникально
    // FNotNull          : Boolean;        // =true, если поле не может принимать пустое значение
    // FCalculated       : boolean;        // =true, если поле вычисляемое
    FDataType: TFieldType; // тип поля
    FDataSize: Integer; // размер поля в базе данных
    FPrecision: Integer; // точность для плавающей точки
    FLink: Variant;
    // ID Dataset'а, значение поля имени которого подтянется в набор данных
    FLinkType: TDeleteAction;
    // тип связи между дочерней и родительской таблицами
    FRole: TFieldRole; // роль служебного поля
    FVisibleLevel: TFieldVisible; // уровень видимости
    FShowType: TShowType; // тип показа
    FWidth: Integer; // ширина колонки Grid'а
    // порядковый номер (по нему сортируются поля при SQL-выборке)
    FOrder: Integer;
    FTemplate: string; // шаблон для вывода числовых полей
    FDefaultValue: string;
    FDefaultPostfix: TExpressionItem;
    FDefaultDefDB: string;
    // значение по умолчанию в базе данных;  формула, если поле вычисляемое
    FValue1: string; // дополнительное значение 1
    FValue2: string; // дополнительное значение 2
    VValue1: Variant;
    VValue2: Variant;

    FIsLookup: boolean; // =true, если поле представляет собой Lookup-поле
    FLookUp: TObject; // ссылка на источник значений поля
    FLookupPair: TFieldMeta;
    FControls: TControlsList;
    FErrorInStructure: boolean;
    FErrorMessage: string;
    FDeleted: boolean;
    FLinkRole: string;
    FCategoryID: Integer;
    FStored: boolean;
    FLinkTable: TTableMeta;
    FPolicySelect: boolean;

    FStage: TFieldStage;
    FUVisible: Variant; // видимое
    FUOrder: Variant; // порядковый номер
    FUWidth: Variant; // ширина колонки Grid'а
    FCodePage: Integer; // Кодировка поля
    FPolictyShow: TObjectRestrict;
    FGUID: TGUID; // GUID поля
    FFlagState: TFieldFlagState;
    procedure SetOriginal(const aOriginal: string);
    procedure SetName(const aName: string);
    function GetNative: string;
    function GetReadOnly: boolean;
    procedure SetRole(const aRole: TFieldRole);
    function GetWidth: Integer;
    procedure SetWidth(aWidth: Integer);
    function GetOrder: Integer;
    procedure SetOrder(aOrder: Integer);
    function GetAlignment: TAlignment;
    function GetLookup: TObject;
    function GetRoleAsInteger: Integer;
    procedure SetRoleAsInteger(const aIntRole: Integer);
    procedure SetStage(aStage: TFieldStage);
    procedure SetLink(const Value: Variant);
    function GetLinkTable: TTableMeta;
    function GetVisibleLevel: TFieldVisible;
    function GetOriginalVisibleLevel: TFieldVisible;
    procedure SetVisibleLevel(aVisibleLevel: TFieldVisible);
    function GetUVisible: Variant;
    function GetUOrder: Variant;
    function GetUWidth: Variant;
    function GetCodePage: Integer;
    function GetPolicyShow: boolean;
    function GetKey: boolean;
    procedure SetKey(const Value: boolean);
    function GetUnique: boolean;
    procedure SetUnique(const Value: boolean);
    function GetNotNull: boolean;
    procedure SetNotNull(const Value: boolean);
    procedure SetReadOnly(const Value: boolean);
    function GetInternalReadOnly: boolean;
    function GetCalculated: boolean;
    procedure SetCalculated(const Value: boolean);
    function GetVariantGUID: Variant;
    procedure SetGUID(const Value: TGUID);
    procedure SetValue1(const Value: string);
    procedure SetValue2(const Value: string);
    function GetDefaultDefDB: string;
    procedure SetDefaultDefDB(const Value: string);
  protected
    property UVisible: Variant read GetUVisible;
    property UOrder: Variant read GetUOrder;
    property UWidth: Variant read GetUWidth;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: Variant read FId write FId;
    property Original: string read FOriginal write SetOriginal;
    property Owner: TTableMeta read FOwner write FOwner;
    property Name: string read FName write SetName;
    property Description: string read FDescription write FDescription;
    property Native: string read GetNative;
    function IsReadOnlyExt: boolean;
    property ReadOnly: boolean read GetInternalReadOnly write SetReadOnly;
    property IsReadOnly: boolean read GetReadOnly write SetReadOnly;
    property Key: boolean read GetKey write SetKey;
    property Unique: boolean read GetUnique write SetUnique;
    property NotNull: boolean read GetNotNull write SetNotNull;
    property Calculated: boolean read GetCalculated write SetCalculated;
    property DataType: TFieldType read FDataType write FDataType;
    property DataSize: Integer read FDataSize write FDataSize;
    property Precision: Integer read FPrecision write FPrecision;
    property Link: Variant read FLink write SetLink;
    procedure SetLinkByName(aLinkTable: String);
    property LinkType: TDeleteAction read FLinkType write FLinkType;
    property Role: TFieldRole read FRole write SetRole;
    property VisibleLevel: TFieldVisible read GetVisibleLevel write SetVisibleLevel;
    property OriginalVisibleLevel: TFieldVisible read GetOriginalVisibleLevel;
    property InnerVisibleLevel: TFieldVisible read FVisibleLevel write FVisibleLevel;
    property ShowType: TShowType read FShowType write FShowType;
    property Width: Integer read GetWidth write SetWidth;
    property Order: Integer read GetOrder write SetOrder;
    property Alignment: TAlignment read GetAlignment;
    property Template: string read FTemplate write FTemplate;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
    property DefaultDefDB: string read GetDefaultDefDB write SetDefaultDefDB;
    function HaveDefaultDefDB: boolean;
    property Value1: string read FValue1 write SetValue1;
    property Value2: string read FValue2 write SetValue2;
    property VariantValue1: Variant read VValue1;
    property VariantValue2: Variant read VValue2;
    property IsLookup: boolean read FIsLookup;
    property Lookup: TObject read GetLookup;
    property LookupPair: TFieldMeta read FLookupPair write FLookupPair;
    property Deleted: boolean read FDeleted write FDeleted;
    property LinkRole: string read FLinkRole write FLinkRole;
    property Controls: TControlsList read FControls;
    property DefaultPostfix: TExpressionItem read FDefaultPostfix;
    property CategoryID: Integer read FCategoryID write FCategoryID;
    property IsStored: boolean read FStored write FStored;
    property LinkTable: TTableMeta read GetLinkTable;
    property PolicySelect: boolean read FPolicySelect;
    property Stage: TFieldStage read FStage write SetStage;
    property ErrorInStructure: boolean read FErrorInStructure write FErrorInStructure;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    /// <summary>роль как целое число</summary>
    /// <remarks>необходимо для взаимодействия с базой данных и запись в кэше</remarks>
    property RoleAsInteger: Integer read GetRoleAsInteger write SetRoleAsInteger;
    /// <summary>сохраняет значения полей класса в DataSet</summary>
    function Save: boolean;
    function UserApply: boolean;
    // function UserSave: Boolean;
    /// <summary>читает значения полей класса из DataSet'а, либо TCacheItem</summary>
    procedure Assign(Source: TObject; const FullAssign: boolean = True); virtual;
    procedure AssignToCacheItem(aItem: TObject);
    /// <summary>обновляет Control'ы, связанные с полем</summary>
    procedure UpdateControls;
    /// <summary>создает LookupPair для данного поля</summary>
    procedure CreateLookupPair;
    /// <summary>удаляет LookupPair для данного поля</summary>
    procedure DestroyLookupPair;
    property CodePage: Integer read GetCodePage write FCodePage;
    property PolicyShow: boolean read GetPolicyShow;
    property FlagState: TFieldFlagState read FFlagState;
    property GUID: TGUID read FGUID write SetGUID;
    property VariantGUID: Variant read GetVariantGUID;
    function TextLength: Integer;
  end;

  /// <summary>список полей данных в таблице метаструктуры</summary>
  TFieldsMeta = class(TObjectList)
  private
    function Get(const Index: Integer): TFieldMeta;
    procedure Put(const Index: Integer; Field: TFieldMeta);
    function GetUserXML: string;
  public
    function New(aOriginal: String; aName: String = ''): TFieldMeta;
    property Items[const Index: Integer]: TFieldMeta read Get write Put; default;
    function IndexByName(const aName: string): Integer;
    function FindByName(const aName: string; Recurse: boolean = False): TFieldMeta;
    function IndexByLink(const aLink: Integer): Integer;
    function IndexKeyField: Integer;
    function ExtLinkCount: Integer;
    function FindByID(const aID: Integer): TFieldMeta;
    function IndexByID(const aID: Integer): Integer;
    procedure SortByOrder; dynamic;
    procedure AssignFields(Source: TFieldsMeta);
    /// <summary>Сохранение пользовательских настроек полей</summary>
    /// <returns>Возвращает True в случае успешного сохранения пользовательских настроек полей</returns>
    function UserSave(const TableID: Integer): boolean;
{$IFDEF DEBUG}
    procedure DebugViewFieldsLog(const Text: string);
{$ENDIF}
    function PrepareClipboardXML(const Level: Integer = -1): string;
    /// <summary>Функция загружает в описание полей GUID`ы по умолчанию.</summary>
    /// <returns>Функция возвращает количество полей, для которых GUID`ы были загружены.</returns>
    function LoadDefaultGUIDs: Integer;
    property UserXML: string read GetUserXML;
    function FieldTypeByName(const FieldName: string): TFieldType;
    // v.19.07
    function FindByGUID(const GUID: TGUID): TFieldMeta;
  end;

  /// <summary>Сохраненяемая пользовательская настройка</summary>
  TUserValue = class
  private
    FID: Integer;        // Уникальный код экземпляра настройки
    FDataSetID: Integer; // Код набора данных (пусто для глобальных настроек)
    FSubject: Integer;   // Код пользователя (пусто для общих настроек)
    FType: Integer;      // Тип экземпляра настройки из жесткого списка
    FName: String;       // Отображаемое название экземпляра
    FXML: String;        // Значение экземпляра настройки
    FOrder: Integer;     // Номер по порядку для упорядоченных списков однотипных настроек
    FPublic: Integer;    // Задумывалось для публикации личных настроек для других пользователей
    FStatus: TDataChanges;
    procedure SetName(const aValue: string);
    procedure SetXML(const aValue: string);
    procedure SetSubject(const aValue: Integer);
    procedure SetType(const aValue: Integer);
    function DB_insert: Boolean;
    function DB_update: Boolean;
    function DB_delete: Boolean;
  public
    procedure Assign(Source: TObject);
    function Commit: Boolean;
    property ID: Integer read FID;
    property DataSetID: Integer read FDataSetID;
    property Subject: Integer read FSubject write SetSubject;
    property TypeValue: Integer read FType write SetType;
    property Name: String read FName write SetName;
    property XML: String read FXML write SetXML;
    property Order: Integer read FOrder write FOrder;
    property PublicValue: Integer read FPublic write FPublic;
    property Status: TDataChanges read FStatus write FStatus;
  end;

  /// <summary>список сохраненяемых пользовательских настроек</summary>
  TUserValues = class(TObjectList)
  private
    function Get(const Index: Integer): TUserValue;
    procedure Put(const Index: Integer; Field: TUserValue);
  public
    function GetIndexByID(const aID: Integer; var aIndex: Integer): Boolean;
    function GetFirstIndex(const aType: Integer; var aIndex: Integer; const IgnoreDeleted: Boolean = True): Boolean;
    function GetNextIndex(const aType: Integer; var aIndex: Integer; const IgnoreDeleted: Boolean = True): Boolean;
    function New(const aType, aDataSet, aSubject: Integer; aName, aXML: string; const aOrder: Integer = -1; const aPublic: Integer = 0): Integer;
    function Commit: Boolean;
    property Items[const Index: Integer]: TUserValue read Get write Put; default;
  end;

  TMetaAction = class(TAction)
  private
    FTableMeta: TTableMeta;
    FFieldMeta: TFieldMeta;
    FMenuItems: TObjectList<TMenuItem>;
  protected
    procedure SetChecked(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const aCategory: string; aObject: TObject; aEvent: TNotifyEvent; const Items: array of TMenuitem); overload;
    destructor Destroy; override;
    property TableMeta: TTableMeta read FTableMeta write FTableMeta;
    property FieldMeta: TFieldMeta read FFieldMeta write FFieldMeta;
  end;

  // класс, обеспечивающий возможность работы с условиями иерархии таблиц
  TDeConstraint = class;
  TFilterItem = class;
  TFilterList = class;

  TParentFieldValue = record
    Field: TFieldMeta;
    Value: Variant;
  end;

  /// <summary>главное меню приложения (mt_ViewMetaRead)</summary>
  TContextMeta = class(TTreeMeta)
  private
    FDataSet: TTableMeta; // основной набор данных
    FSolutionID: Integer; // явное указание на решение, при указании набора данных важнее решение набора данных
    FDSView: TDSViewType; // тип визуализации набора данных
    FDSParams: string; // параметры визуализации родительского набора данных
    FFilter: string; // дополнительный фильтр
    FDeleted: boolean; // признак удаленности данных
    FOrder: Integer; // порядковый номер
    FItemType: Integer; // тип элемента (папка/не папка)
    FSubjectID: Variant; // код пользователя-владельца
    FChanged: TTypeChanged;

    FRecordKey: Variant;
    FSelectedKeys: Variant;
    FFilterXML: string;
    FFilterActive: boolean;
    FFilterVisible: boolean;
    FGroups: TList<TParentFieldValue>;
    FAlign: TAlign;
    FSize: Integer;
    function GetMeta(const Index: Integer): TContextMeta;
    procedure SetMeta(const Index: Integer; const Meta: TContextMeta);
    function GetDatasetID: Integer;
    function GetIco: Integer;
    function GetSolutionID: Integer;
    function GetViewType: TDSViewType;
  public
    FIco: Integer; // номер иконки
    constructor Create(aRoot: TTreeMeta; const aLoadNow: Boolean = False);
    destructor Destroy; override;
    property Items[const Index: Integer]: TContextMeta read GetMeta write SetMeta; default;
    property Dataset: TTableMeta read FDataSet write FDataSet;
    property DatasetId: Integer read GetDatasetID;
    property Ico: Integer read GetIco write FIco;
    property SubjectID: Variant read FSubjectID write FSubjectID;
    property ViewType: TDSViewType read GetViewType write FDSView;
    property ViewParams: string read FDSParams write FDSParams;
    property Filter: string read FFilter write FFilter;
    property SolutionID: Integer read GetSolutionID write FSolutionID;
    property Deleted: boolean read FDeleted write FDeleted;
    property Order: Integer read FOrder write FOrder;
    property ItemType: Integer read FItemType write FItemType;
    property Changed: TTypeChanged read FChanged write FChanged;
    /// <summary>читает структуру главного меню</summary>
    procedure LoadMenu;
    /// <summary>читает значения полей класса из DeDataset'а, либо CacheItem'а</summary>
    procedure Assign(Source: TObject); override;
    /// <summary>возвращает описание как CacheItem'а</summary>
    procedure SaveTo(aCasheItem: TObject);
    /// <summary>создает новый дочерний элемент меню</summary>
    function AddMenuMeta: TContextMeta;
    /// <summary>удаляет элемент меню</summary>
    function DeleteMenu: boolean;
    /// <summary>сортировка элементов</summary>
    procedure SortByOrder;
    /// <summary>установка признака измененности</summary>
    procedure SetChanged(aChanged: TTypeChanged);
    /// <summary>сохранение изменений</summary>
    procedure SaveChanges;
    /// <summary>строит имя элемента</summary>
    function Caption: string;
    /// <summary>строит хинт элемента</summary>
    function Hint: string;
    function IsSeparator: boolean;
    function Optimize(aHideGroup: Boolean = False): Boolean;
    property Align: TAlign read FAlign write FAlign;
    property Size: Integer read FSize write FSize;
    property Groups: TList<TParentFieldValue> read FGroups;
    procedure AddGroupField(aField: TFieldMeta; aParentKey: Variant);

    property RecordKey: Variant read FRecordKey write FRecordKey;
    property SelectedKeys: Variant read FSelectedKeys write FSelectedKeys;
    property FilterXML: string read FFilterXML write FFilterXML;
    property FilterActive: boolean read FFilterActive write FFilterActive;
    property FilterVisible: boolean read FFilterVisible write FFilterVisible;
  end;

  THistoryList = class(TObjectList)
  private
    FCurrentIndex: Integer;
    function GetItem(const Index: Integer): TContextMeta;
    procedure SetCurrentIndex(const aValue: Integer);
    function GetCurrentItem: TContextMeta;
  public
    constructor Create;
{$IFDEF DEBUG}
    procedure DebugHistoryLog(const Text: string);
{$ENDIF}
    procedure Clear; override;
    property Items[const Index: Integer]: TContextMeta read GetItem; default;
    property CurrentIndex: Integer read FCurrentIndex write SetCurrentIndex;
    property CurrentItem: TContextMeta read GetCurrentItem;
    function NewItem: TContextMeta;
  end;

  TElementMeta = class;

  /// <summary>описание древовидной структуры элементов</summary>
  TElementMeta = class(TTreeMeta)
  private
    FTable: TTableMeta; // таблица элемента
    FFieldID: Variant; // поле, связ. с элементом
    FFont: TFont;
    FExtFontSize: boolean; // если =true, то размер шрифта увеличен в 1.5 раза
    FCaption: string;
    FEAL: Integer; // левый якорь
    FEAR: Integer; // правый якорь
    FEL: Integer; // смещение относительно левого якоря
    FER: Integer; // смещение относительно правого якоря
    FEH: Integer; // высота
    FET: Integer; // верхняя граница
    FLink: Integer; // ID Dataset'a
    FElementType: TElementType;
    FLinkTable: TTableMeta;
    FLinkFieldID: Variant;
    FNameInExpr: string;
    FExprFilter: string;
    FExprVisible: string;
    FExprReadOnly: string;
    FExprValue: string;
//    FParamsValue: string;
    FFilterPostfix: TExpressionItem;
    FVisiblePostfix: TExpressionItem;
    FReadOnlyPostfix: TExpressionItem;
    FValuePostfix: TExpressionItem;
    FVisible: boolean;
    FReadOnly: boolean;
    FDeleted: boolean;
    FChanges: TDataChanges;
    FParameters: TParamList;
    function GetItem(const Index: Integer): TElementMeta;
    procedure PutItem(const Index: Integer; Value: TElementMeta);
    function GetFontCode: Integer;
    procedure SetFontCode(aFontCode: Integer);
    function GetCaption: string;
    procedure SetCaption(const aValue: string);
    function GetElementColor: TColor;
    procedure SetElementColor(const aColor: TColor);
    procedure SetEAL(const aEAL: Integer);
    procedure SetEAR(const aEAR: Integer);
    procedure SetEL(const aEL: Integer);
    procedure SetER(const aER: Integer);
    procedure SetET(const aET: Integer);
    function GetEH: Integer;
    procedure SetEH(const aEH: Integer);
    procedure SetLink(const aLink: Integer);
    procedure SetElementType(const aElementType: TElementType);
    function GetWasChanged: boolean;
    function GetField: TFieldMeta;
    function GetLinkField: TFieldMeta;
  public
    /// <summary>создает экземпляр класса и заполняет поля (mt_ElementPrepare)</summary>
    constructor Create(aTable: TTableMeta = nil; aOwner: TElementMeta = nil); overload;
    constructor Create(aTable: TTableMeta; aName: String; aField: TFieldMeta;
      aElementType: TElementType; aLink, aEAL, aEAR, aEL, aER, aET, aEB: Integer); overload;
    destructor Destroy; override;

    /// <summary>предоставляет доступ к дочерним узлам</summary>
    property Items[const Index: Integer]: TElementMeta read GetItem write PutItem; default;

    /// <summary>создает TElementMeta с учетом типа, роли и других свойств поля</summary>
    function DesignElement(const aField: TFieldMeta; const aL, aT : Integer): Integer;

    // код шрифта: чтение и запись - оставлено для обратной совместимости со старыми данными
    property Font: TFont read FFont; // шрифт
    property Table: TTableMeta read FTable;
    property FieldID: Variant read FFieldID;
    property Field: TFieldMeta read GetField;
    property FontCode: Integer read GetFontCode write SetFontCode;
    property Caption: string read GetCaption write SetCaption;
    property Color: TColor read GetElementColor write SetElementColor;
    property EAL: Integer read FEAL write SetEAL;
    property EAR: Integer read FEAR write SetEAR;
    property EL: Integer read FEL write SetEL;
    property ER: Integer read FER write SetER;
    property ET: Integer read FET write SetET;
    property EH: Integer read GetEH write SetEH; // FEH
    function EB: Integer;
    property Link: Integer read FLink write SetLink;
    property ElementType: TElementType read FElementType write SetElementType;
    property WasChanged: boolean read GetWasChanged;
    property LinkTable: TTableMeta read FLinkTable;
    property LinkFieldID: Variant read FLinkFieldID;
    property LinkField: TFieldMeta read GetLinkField;
    property Deleted: boolean read FDeleted write FDeleted;
    property NameInExpr: string read FNameInExpr write FNameInExpr;
    property ExprFilter: string read FExprFilter write FExprFilter;
    property ExprVisible: string read FExprVisible write FExprVisible;
    property ExprReadOnly: string read FExprReadOnly write FExprReadOnly;
    property ExprValue: string read FExprValue write FExprValue;
    property FilterPostfix: TExpressionItem read FFilterPostfix;
    property VisiblePostfix: TExpressionItem read FVisiblePostfix;

    property ReadOnlyPostfix: TExpressionItem read FReadOnlyPostfix;
    property ValuePostfix: TExpressionItem read FValuePostfix;
    property IsVisible: boolean read FVisible write FVisible;
    property IsReadOnly: boolean read FReadOnly write FReadOnly;
    property Changes: TDataChanges read FChanges write FChanges;
    /// <summary>рассчитывает минимально допустимые размеры контейнера элемента</summary>
    procedure GetParentBounds(var aHeight, aWidth: Integer);
    /// <summary>рассчитывает положение и границы элемента</summary>
    procedure GetBounds(aClientWidth, aClientHeight: Integer; var aL, aT, aW, aH: Integer; aSimple: Boolean = True);
    /// <summary>рассчитывает и устанавливает положение и границы элемента</summary>
    function SetBounds(TC: TControl; Repaint: Boolean = False): boolean;
    /// <summary>Указывает на возможность вертикального увеличения элемента</summary>
    function SizableH: Boolean;
    /// <summary>удалаяет элемент и все дочерние элементы</summary>
    procedure DeleteElement(aMeta: TElementMeta);
    /// <summary>сохраняет описание элементов в базе данных (mt_ElementsMetaSave)</summary>
    procedure SaveElements;
    /// <summary>читает поля элемента из DataSet'а</summary>
    procedure Assign(Source: TObject); override;
    procedure RecursiveSetChanged(aChanged: TTypeChanged);
    /// <summary>поиск по имени в выражении</summary>
    function FindByExprName(const aExprName: string; StrongCompare: boolean = True): TElementMeta;
    function FindByFieldAndType(const aField: TFieldMeta; TypeList: TElementTypes = []): TElementMeta;
    property Parameters: TParamList read FParameters;

    // методы применимые только для узлов уровня форма
    function FindDatasetPage(const aID: Integer; aLinkField: TFieldMeta): TElementMeta;
    function ElementIsPageControl(EM: TElementMeta): boolean;
    function CreateBlankPage: TElementMeta;
    function CreateDatasetPage(const aID: Integer; aLinkField: TFieldMeta): TElementMeta;
    function GetDatasetPage(const aID: Integer; aLinkField: TFieldMeta; var aCreated: boolean): TElementMeta;
    procedure RemovePage(const aPage: TElementMeta);
    procedure CorrectInnerElements(aOwner: TElementMeta);
    procedure CorrectElements;
  end;

  TElementsManager = class;

  TTableFlagStateType = (tsLoading, tsPreloadFields, tsFields, tsUserValues, tsElements,
    // tsSelectSQL, tsUpdateSQL, tsDeleteSQL, tsInsertSQL, tsPermanentDeleteSQL,
    tsFilter, tsConstraints, tsActions, tsDynamic, tsGUID, tsGEO, tsPermissions);

  TTableFlagState = set of TTableFlagStateType;

  { TActionCondition }

  TEventMetaList = class;

  TEventMeta = class
    FId: Integer;
    FEventID: Integer;
    FProcedureID: Integer;
    FProcedureParameter: string;
    FConditionStr: string;
    FConditionPostfix: TExpressionItem;
  private
    function GetEventName: string;
    function GetProcedureName: string;
  public
    constructor Create(aOwner: TEventMetaList);
    destructor Destroy; override;
    procedure Assign(Source: TObject);

    property ID: Integer read FId;
    property EventID: Integer read FEventID;
    property EventName: string read GetEventName;
    property ProcedureID: Integer read FProcedureID;
    property ProcedureName: string read GetProcedureName;
    property ProcedureParameter: string read FProcedureParameter;
    property Condition: string read FConditionStr;
    property ConditionPostfix: TExpressionItem read FConditionPostfix;
  end;

  { TActionConditionList }

  TEventMetaList = class(TObjectList)
  private
    FTableMetaID: Integer;
    procedure SetTableMetaID(const Value: Integer);
    function Get(const aIndex: Integer): TEventMeta;
    procedure Put(const aIndex: Integer; Event: TEventMeta);
  public
    constructor Create(AOwnsObjects: boolean);
    destructor Destroy; override;
    procedure Refresh;
    property TableMetaID: Integer read FTableMetaID write SetTableMetaID;
    property Items[const Index: Integer]: TEventMeta read Get write Put; default;
  end;

  TTablesList = class;

  /// <summary>описание ограничений в метаструктуре</summary>
  TDeConstraint = class
  private
    FId: Integer;
    FTable: TTableMeta;
    FContext: TConditionContexts;
    FEvent: TConditionEvent;
    FDeleted: boolean;
    FActive: boolean;
    FExpression: string;
    FExprPostfix: TExpressionItem;
    FStrMessage: string;
    FErrorField: TFieldMeta;
    function GetContextAsInteger: Integer;
    procedure SetContextAsInteger(const aIntContext: Integer);
    procedure SetExpression(const aExpression: string);
  public
    constructor Create(aTable: TTableMeta);
    destructor Destroy; override;
    property ID: Integer read FId write FId;
    property Table: TTableMeta read FTable;
    property Context: TConditionContexts read FContext write FContext;
    property ContextAsInteger: Integer read GetContextAsInteger write SetContextAsInteger;
    property Event: TConditionEvent read FEvent write FEvent;
    property Deleted: boolean read FDeleted write FDeleted;
    property Active: boolean read FActive write FActive;
    property Expression: string read FExpression write SetExpression;
    property ExprPostfix: TExpressionItem read FExprPostfix;
    property StrMessage: string read FStrMessage write FStrMessage;
    property ErrorField: TFieldMeta read FErrorField write FErrorField;
    procedure Assign(Source: TObject);
    // v. 18.03
    function CheckPolicy(const Operation: TSecurityOperation): boolean;
  end;

  TFiltersManager = class;

  /// <summary>описание таблиц в метаструктуре</summary>
  TTableMeta = class
  private
    FId: Variant;
    FIco: Integer;
    FName: string;
    FTable: string;
    FSchema: string; // v.19.08
    FObjectType: TObjectType;
    FDescription: string;
    FKField: TFieldsMeta; // ключевое поле (или поля, если их несколько)
    FNField: TFieldMeta; // поле имени
    FDField: TFieldMeta; // поле, являющееся признаком удаленности
    FPField: TFieldMeta; // поле родителя в случае древовидности
    FOField: TFieldMeta;
    FDropBlobField: TFieldMeta;
    FDropNameField: TFieldMeta;
    FDropDateField: TFieldMeta;
    FDropSizeField: TFieldMeta;
    // поле, которое является признаком использования записи по умолчанию
    // FAField             : TFieldMeta;       // поле, являющееся признаком архивной записи
    FGField: TFieldMeta;
    // поле группировки;  в рамках группы задается значение по умолчанию
    // группировочных полей может быть несколько
    FIconField: TFieldMeta; // поле - содержащее номер иконки
    FPreviewField: TFieldMeta; // поле - содержащее путь или картинку иконки
    FColorField: TFieldMeta; // поле - содержащее цвет записи
    FLatitudeField: TFieldMeta;  // поле - широта
    FLongitudeField: TFieldMeta; // поле - долгота
    FAddressField: TFieldMeta;   // поле - адрес для GEO кодера
    FPhoneField: TFieldMeta;     // поле - номер телефона
    FMailField: TFieldMeta;      // поле - электронный адрес
    FFields: TFieldsMeta; // поля таблицы
    FLinks: TFieldsMeta; // !!! связи других таблиц с данной
    FElements: TElementsManager; // дерево элементов, отображающих поля таблицы
    FUserValues: TUserValues; // персональные и общие настройки из таболицы UDATASET
    FSelectSQL: string; // запрос, используемый для чтения данных
    FUpdateSQL: string; // запрос, используемый для обновления данных
    FInsertSQL: string; // запрос, используемый для добавления данных
    FDeleteSQL: string; // запрос, используемый для удаления данных
    FPermanentDeleteSQL: string;
    // запрос, используемый для окончательного удаления данных
    FReadOnly: boolean;
    FGroupViewType: TDSViewType;
    FGroupViewParams: string;
    FDatabase: TDeCustomDatabase;
    FDeletedFilter: TFilterItem; // условие-фильтр удаленных записей
    // FArchivedFilter     : TFilterItem;      // условие-фильтр архивных записей
    FLoadingState: TTableFlagState;
    FIsInterrelations: boolean;
    // для оптимизации - является ли таблица таблицей взаимосвязей
    FIsNotes: boolean; // для оптимизации - является ли таблица таблицей заметок
    FIsShowRight: boolean; // для оптимизации - надо ли показывать закладку прав
    FOwnerTable: TTableMeta; // предыдущая таблица в цепочке
    FChildrenTables: TTablesList;
    FDeleted: boolean;
    FSolutionID: Integer;
    FOnAddMenu: boolean;
    FIsDirectory: TIsDirectory;
    FFilter: string;
    FFilterPostfix: TFilterItem;
    FFiltersManager: TFiltersManager;
    FActionConditionList: TEventMetaList; // список условных действий
    FConstraintList: TObjectList<TDeConstraint>;
    FParameters: TVariableList;
    FLinkFilters: TFilterList;
    FGUID: TGUID; // Глобальный идентификатор таблицы
    FPermissions: TDeDatasetPermissions; // Права на уровне базы данных
    FMetaTableIndex: Integer;
    FVariantSolutionGUID: Variant; // GUID решения, в котором находится таблица
    procedure LoadActions;
    procedure LoadFilters;
    procedure LoadConstraints;
    procedure LoadUserValues;
    function GetFields: TFieldsMeta;
    function GetElements: TElementsManager;
    function GetFilters: TFiltersManager;
    function GetUserValues: TUserValues;
    function GetConstraints: TObjectList<TDeConstraint>;
    function GetKField: TFieldsMeta;
    function GetField(aIndex: Integer): TFieldMeta;
    function GetFirstForm: TElementMeta;
    function GetKeyCount: Integer;
    function GetIsReadOnly: boolean;
    function GetIsShowRight: boolean;
    function GetErrorsDetected: boolean;
    function GetErrorMessage: string;
    procedure SetFilter(const aFilter: string);
    function GetFilterPostfix: TFilterItem;
    function GetTable: string;
    function GetActionConditionList: TEventMetaList;
    function GetSelectSQL: string;
    function GetLinkFilters: TFilterList;
    procedure SetGUID(const Value: TGUID);
    function GetVariantGUID: Variant;
    function GetObjectType: TObjectType;
    function GetIco: Integer;
    procedure SetObjectType(aType: TObjectType);
    function GetPermissions: TDeDatasetPermissions;
    function GetVariantSolutionGUID: Variant;
    function GetOwnerTable: TTableMeta;
    function GetIsDynamic: Boolean;
    procedure SetIsDynamic(const Value: Boolean);
    procedure SetIco(const Value: Integer);
    procedure SetName(const Value: string);
  public
    FParentTableID: Variant;
    constructor Create; overload;
    constructor Create(const aTable: string; ADatabase: TDeCustomDatabase; const ALoadingState: TTableFlagState = []); overload;
    destructor Destroy; override;
    procedure CheckState(const State: TTableFlagStateType);
    function IsSecurityProtected: boolean;
    property ID: Variant read FId write FId;
    property Ico: Integer read GetIco write SetIco;
    property Name: string read FName write SetName;
    property Table: string read GetTable;
    property SetTable: string write FTable;
    property Description: string read FDescription write FDescription;
    property FullTable: string read FTable;
    property Fields: TFieldsMeta read GetFields;
    property Links: TFieldsMeta read FLinks;
    property UserValues: TUserValues read GetUserValues;
    function GetFieldByName(aName: string; Recurce: boolean = False): TFieldMeta;
    property Filters: TFiltersManager read GetFilters;
    property Constraints: TObjectList<TDeConstraint> read GetConstraints;
    procedure LoadElements;
    property Elements: TElementsManager read GetElements; // элементы формы
    property DeletedFilter: TFilterItem read FDeletedFilter;
    property FirstForm: TElementMeta read GetFirstForm;
    property KeyCount: Integer read GetKeyCount;
    property IsReadOnly: boolean read GetIsReadOnly write FReadOnly;
    property GroupViewType: TDSViewType read FGroupViewType write FGroupViewType;
    property GroupViewParams: string read FGroupViewParams write FGroupViewParams;
    property KField: TFieldsMeta read GetKField;
    property NField: TFieldMeta index 1 read GetField;
    property PField: TFieldMeta index 2 read GetField;
    property OField: TFieldMeta index 3 read GetField;
    property DField: TFieldMeta index 4 read GetField;
    property GField: TFieldMeta index 5 read GetField;
    property PreviewField: TFieldMeta index 6 read GetField;
    property IconField: TFieldMeta index 7 read GetField;
    property ColorField: TFieldMeta index 8 read GetField;
    // v. 18.6 +
    // Поля широты и долготы на карте
    property LatitudeField: TFieldMeta index 9 read GetField;
    property LongitudeField: TFieldMeta index 10 read GetField;
    property AddressField: TFieldMeta index 11 read GetField;
    property PhoneField: TFieldMeta index 12 read GetField;
    property MailField: TFieldMeta index 13 read GetField;
    // v. 18.6 -
    property DropBlobField: TFieldMeta index 14 read GetField;
    property DropNameField: TFieldMeta index 15 read GetField;
    property DropSizeField: TFieldMeta index 16 read GetField;
    property DropDateField: TFieldMeta index 17 read GetField;
    function CanDropFiles: Boolean;
    // property AField        : TFieldMeta  index  9 read GetField;
    property UserSelectSQL: string read GetSelectSQL;
    property UserUpdateSQL: string read FUpdateSQL;
    property UserInsertSQL: string read FInsertSQL;
    property UserDeleteSQL: string read FDeleteSQL;
    property UserPermanentDeleteSQL: string read FPermanentDeleteSQL;
    property Database: TDeCustomDatabase read FDatabase;
    // база данных, хранящая таблицу
    property ErrorsDetected: boolean read GetErrorsDetected;
    property ErrorMessage: string read GetErrorMessage;
    property SolutionID: Integer read FSolutionID;
    function IsSystem: boolean;
    property IsInterrelations: boolean read FIsInterrelations;
    property IsNotes: boolean read FIsNotes;
    property IsDirectory: TIsDirectory read FIsDirectory;
    property IsDynamic: Boolean read GetIsDynamic write SetIsDynamic;
    property Deleted: boolean read FDeleted;
    property isShowRight: boolean read GetIsShowRight;
    property Filter: string read FFilter write SetFilter;
    property FilterPostfix: TFilterItem read GetFilterPostfix;
    /// <summary>предыдущая таблица в цепочке</summary>
    property OwnerTable: TTableMeta read GetOwnerTable;
    /// <summary>первая таблица в цепочке</summary>
    function GrandOwnerTable: TTableMeta;
    /// <summary>включена ли таблица в меню "Add" главной формы</summary>
    property OnAddMenu: boolean read FOnAddMenu;
    function Save: boolean;
    /// <summary>загрузка полей таблицы из записи</summary>
    procedure Assign(Source: TObject);
    procedure AssignToCacheItem(aItem: TObject);
    procedure AssignFields(aList: TFieldsMeta);
    /// <summary>читает список полей из открытого DataSet'а и сохраняет метаописания в базе данных </summary>
    procedure LoadFields;
    // v. 18.9 +
    procedure PreLoadFields(Dataset: TDeDataset);
    // v. 18.9 -
    property ActionConditionList: TEventMetaList read GetActionConditionList;
    procedure DefineTableLinks;
    procedure UndefTableLinks;
    procedure Clear;
    procedure ClearElements(aClearData: Boolean = True);
    procedure ClearFields;
    procedure ClearUserValues;
    procedure DeleteUserValues(const aType: Integer; aUserID: Integer = 0);
    function FoundInParentTables(aTable: TTableMeta): boolean;
    // function  Links_Count : integer;
    // property  Links_Items[Index : integer] : TFieldMeta read Links_GetItem;
    property LoadingState: TTableFlagState read FLoadingState;
    property Parameters: TVariableList read FParameters;
    // Список внешних фильтров
    property LinkFilters: TFilterList read GetLinkFilters;
    property GUID: TGUID read FGUID write SetGUID;
    property VariantGUID: Variant read GetVariantGUID;
    property ObjectType: TObjectType read GetObjectType write SetObjectType;
    property SelectSQL: string read FSelectSQL write FSelectSQL;
    property UpdateSQL: string read FUpdateSQL write FUpdateSQL;
    property InsertSQL: string read FInsertSQL write FInsertSQL;
    property DeleteSQL: string read FDeleteSQL write FDeleteSQL;
    // v.19.06
    property Permissions: TDeDatasetPermissions read GetPermissions;
    // v.19.08
    property Schema: string read FSchema write FSchema;
    // v.19.09
    /// <summary>Индекс таблицы в списке метаструктурых таблиц</summary>
    /// <remarks>Если данное свойство имеет значение -1, то это не метаструктурная таблица!</remarks>
    property MetaTableIndex: Integer read FMetaTableIndex write FMetaTableIndex;
    /// <summary>Функция проверки принадлежности таблицы к метаструктой</summary>
    /// <returns>True - таблицы метаструктуры, False - НЕ метаструктурная таблица.</returns>
    function IsMetabaseTable: Boolean;
    // v.21.01
    property VariantSolutionGUID: Variant read GetVariantSolutionGUID;
    function AddCalcField(const aName: String; aDefaultValue: string;
                                   aType: TFieldType = ftString; aSize: Integer = 255; aScale: Integer = 0): TFieldmeta;
  end;

  /// <summary>список метаописаний таблиц</summary>
  TTablesList = class(TObjectList)
  private
    function Get(const Index: Integer): TTableMeta;
    procedure Put(const Index: Integer; TableMeta: TTableMeta);
  public
    function FindByID(const DatasetId: Integer): TTableMeta;
    function FindByTable(const Name: string): TTableMeta;
{$IFDEF DEBUG}
    procedure DebugTablesLog(const Text: string);
{$ENDIF}
    /// <summary>Функция проверяет наличие описания хотя бы одной таблицы с GUID в списке.</summary>
    function IsGUID: boolean;
    /// <summary>Функция загружает в описание таблиц GUID`ы по умолчанию.</summary>
    /// <param name="TableOnly">True - загрузить только GUID`ы таблиц. False - загрузить GUID`ы таблиц и GUID`ы их полей.</param>
    /// <returns>Функция возвращает количество таблиц, для которых GUID`ы были загружены.</returns>
    function LoadDefaultGUIDs(const TableOnly: boolean = True): Integer;
    /// <summary>Функция поиска описания таблицы по GUID`у таблицы.</summary>
    /// <param name="DatasetGUID">GUID искомой таблицы</param>
    /// <param name="DatasetOnly">True - искать только в GUID`ах, которые загружены из базы данных. False - искать и в эталонных GUID`ах по имени таблицы.</param>
    /// <returns>Функция возвращает объект с описанием таблицы в случае успеха. Иначе функция возвращает nil.</returns>
    function FindByGUID(const DatasetGUID: TGUID; const DatasetOnly: boolean = True): TTableMeta;
    property Items[const Index: Integer]: TTableMeta read Get write Put; default;
    // v. 19.07
    /// <summary>Функция поиска описания поля по GUID`у поля.</summary>
    /// <param name="FieldGUID">GUID искомого поля</param>
    /// <returns>Функция возвращает объект с описанием поля в случае успеха. Иначе функция возвращает nil.</returns>
    function FindFieldByGUID(const FieldGUID: TGUID): TFieldMeta;
    // v. 19.08
    /// <summary>Функция поиска индекса таблицы в списке.</summary>
    /// <param name="Name">имя таблицы</param>
    /// <param name="FromIndex">начальный индекс поиска</param>
    /// <returns>Функция возвращает индекс таблицы в случае успеха. Иначе функция возвращает -1.</returns>
    function IndexByName(const Name: string; const FromIndex: Integer = 0): Integer;
  end;

  /// <summary>Clipboard для нужд Profite</summary>
  TDeClipboard = class(TClipboard)
  private
    FTempList: TStrings;
    FStackList: TObjectList;
    function GetDataID: Integer;
    function GetSize: Integer;
  public
    destructor Destroy; override;
    /// <summary>возвращает ID записи в Clipboard и количество записей</summary>
    function GetInfo(var DataID: Integer; var aCount: Integer; var RecordID: Variant): boolean;
    /// <summary>возвращает ID записи в Clipboard; если нет записей Profit, то возвращает 0</summary>
    property DataID: Integer read GetDataID;
    /// <summary>возвращает размер массива в Clipboard = количество записей * количество полей</summary>
    property Size: Integer read GetSize;
    /// <summary>проверяет, содержит ли Clipboard данные Profit</summary>
    function ContainsDeData: boolean;
    /// <summary>запоминает в Clipboard имя файла для последующей работы с ним в проводнике</summary>
    function WriteFileName(const FileName: string; const Deleted: boolean = True): boolean;
    /// <summary>получает из Clipboard имя файла для последующей работы с ним</summary>
    function ReadFileName(const FileIndex: Cardinal = 0; const ForgetAll: boolean = True): string;
    /// <summary>запоминает в Clipboard указанную Bitmap картинку</summary>
    function WriteBitmap(Bitmap: TBitmap): boolean;
    /// <summary>получает из Clipboard Bitmap картинку</summary>
    function ReadBitmap(Bitmap: TBitmap; const ForgetAll: boolean = True): boolean;
    /// <summary>запоминает в Clipboard указанную картинку в Bitmap формате</summary>
    function WriteGraphic(Graphic: TGraphic): boolean;
    /// <summary>получает из Clipboard JPEG картинку</summary>
    function ReadJPEG(JPEGImage: TJPEGImage; const ForgetAll: boolean = True): boolean;
    /// <summary>сохраняет буфер обмена со всеми форматами в поток</summary>
    procedure SaveToStream(Stream: TStream);
    /// <summary>загружает буфер обмена со всеми форматами из потока</summary>
    procedure LoadFromStream(Stream: TStream);
    /// <summary>запоминает буфер обмена со всеми форматами</summary>
    function Push: boolean;
    /// <summary>восстанавливает буфер обмена со всеми форматами</summary>
    function Pop: boolean;
    function ReadValue(var Value: Variant): boolean; overload;
    function ReadValue(var Value: Variant; const ValueType: TFieldType; const TextParsed: boolean = True): boolean; overload;
    function ReadValueType(var ValueType: TFieldType): boolean;
    function WriteValue(const Value: Variant; const TextWrited: boolean = True) : boolean; overload;
    function WriteValue(const Value: Variant; const Text: string = '') : boolean; overload;
    function HasValueType(const ValueType: TFieldType; const TextParsed: boolean = True): boolean; overload;
    function HasValueType(const ValueTypes: array of TFieldType; const TextParsed: boolean = True): boolean; overload;
  end;

  TCounter = class(TList)
  private
    function FindRec(const ID: Integer): pointer;
    function GetCount(const ID: Integer): Integer;
    procedure SetCount(const ID: Integer; const Value: Integer);
  protected
    procedure Notify(Ptr: pointer; Action: TListNotification); override;
  public
    property Items[const ID: Integer]: Integer read GetCount write SetCount; default;
    procedure IncCount(const ID: Integer; const IncValue: Integer = 1);
    procedure DecCount(const ID: Integer; const DecValue: Integer = 1);
  end;

  /// <summary>фильтр</summary>
  TFilterItem = class(TExpressionItem)
  private
  public
  end;

  /// <summary>список фильтров</summary>
  TFilterList = class(TObjectList)
  private
    function GetFilter(const Index: Integer): TFilterItem;
  public
    property Items[const Index: Integer]: TFilterItem read GetFilter; default;
  end;

  PFilterList = ^TFilterList;

  /// <summary>абстрактный класс, обеспечивающий возможность работы с иерархией таблиц</summary>
  THierarchyManager = class
  private
    FOwner: TTableMeta;
    FState: TTableFlagStateType;
    FParentManager  : THierarchyManager;
    function GetParentManager: THierarchyManager;
    function GetParentCount: Integer;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TObject;
  protected
    FDataList: TObjectList;
    procedure CheckOwnerState;
    function CreateDataList: TObjectList; virtual; abstract;
  public
    constructor Create(const State: TTableFlagStateType; const Owner: TTableMeta = nil);
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TObject read GetItem; default;
    property ParentManager: THierarchyManager read FParentManager write FParentManager;
    procedure Clear;
    function Add(aDataObject: TObject): Integer;
    function Extract(aDataObject: TObject): TObject;
    function IndexOf(aDataObject: TObject): Integer;
  end;

  /// <summary>менеджер для списка фильтров в иерархии</summary>
  TFiltersManager = class(THierarchyManager)
  private
    function GetFilterItem(const Index: Integer): TFilterItem;
  protected
    function CreateDataList: TObjectList; override;
  public
    constructor Create(aOwner: TTableMeta = nil);
    property Items[const Index: Integer]: TFilterItem read GetFilterItem; default;
    procedure AddFilter(aFilter: TFilterItem);
    procedure RemoveFilter(aFilter: TFilterItem);
    procedure NewFilter(aFieldMeta: TFieldMeta; const aOperation: TOperationType; const aValue: Variant);
{$IFDEF DEBUG}
    procedure DebugFilterLog(const Text: string);
{$ENDIF}
  end;

  /// <summary>менеджер для форм визуализации</summary>
  TElementsManager = class
  private
    FTable: TTableMeta;
    FRoot: TElementMeta;
    FList: TObjectList<TElementMeta>;
    function GetForm(const Index: Integer): TElementMeta;
    function GetCount: Integer;
    function ElementGetVariable(const aIdent: string; var aVariable: TVariableItem; aDataObject: pointer): Boolean;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TElementMeta read GetForm; default;
    procedure LoadForms(const TableID: Integer);
    function AddBlankForm: TElementMeta;
    function AddDefaultForm: TElementMeta;
    procedure DeleteForm(aForm: TElementMeta);
  end;

  TColumnItem = class
  private
    FField: TFieldMeta;
    FValueIndex: Integer;
    FTextIndex: Integer;
  public
    property Field: TFieldMeta read FField;
    property ValueIndex: Integer read FValueIndex;
    property TextIndex: Integer read FTextIndex;
  end;

  TColumnList = class(TObjectList)
  private
    function GetItem(const Index: Integer): TColumnItem;
    procedure SetItem(const Index: Integer; const Value: TColumnItem);
  public
    constructor Create;
    procedure InitColumnList(Fields: TFieldsMeta);
    function FullWidth: Integer;
    function Add(const aList: TFieldsMeta; const aFieldMeta: TFieldMeta): Integer; overload;
    function Add(const aList: TFieldsMeta; const aIndex: Integer): Integer; overload;
    function IndexByFieldId(aID: Integer; aDefaultValue: Integer = -1): Integer;
    procedure SortByOrder;
{$IFDEF DEBUG}
    procedure DebugColumnsLog(const Text: string);
{$ENDIF}
    function LoadFromUserDataset(const TableID, UserID: Integer): boolean;
    procedure SaveToUserDataset(const TableID, UserID: Integer);
    property Items[const Index: Integer]: TColumnItem read GetItem write SetItem; default;
  end;

  TGroupFilter = class;

  TGroupFilterItem = class
  private
    FOwner: TGroupFilter;
    FId: Integer;
    FTypeID: Integer;
    FDataSet: Integer;
    FFilter: string;
    FVisibleLoaded: boolean;
    FVisibleRestrict: TObjectRestrict;
    FActiveLoaded: boolean;
    FActiveRestrict: TObjectRestrict;
    FFilterItem: TFilterItem;
    FVariables: TVariableList;
    function GetActive: boolean;
    procedure SetActive(const Value: boolean);
    function GetActiveRestrict: TObjectRestrict;
    function GetVisible: boolean;
    function GetVisibleRestrict: TObjectRestrict;
    function GetFilterItem: TFilterItem;
    function GetTableMeta: TTableMeta;
  public
    constructor Create(aOwner: TGroupFilter);
    destructor Destroy; override;
    procedure Assign(Source: TObject);
    function IsGlobal: boolean;
    property Owner: TGroupFilter read FOwner;
    property Dataset: Integer read FDataSet;
    property Filter: string read FFilter;
    property Active: boolean read GetActive write SetActive;
    property ActiveRestrict: TObjectRestrict read GetActiveRestrict;
    property Visible: boolean read GetVisible;
    property VisibleRestrict: TObjectRestrict read GetVisibleRestrict;
    property TableMeta: TTableMeta read GetTableMeta;
    property FilterItem: TFilterItem read GetFilterItem;
    property Variables: TVariableList read FVariables;
  end;

  TGroupFilters = class;

  TGroupFilter = class(TObjectList)
  private
    FOwner: TGroupFilters;
    function GetItem(const Index: Integer): TGroupFilterItem;
    procedure SetItem(const Index: Integer; const Value: TGroupFilterItem);
    function GetActive: boolean;
    procedure SetActive(const Value: boolean);
    function GetActiveRestrict: TObjectRestrict;
    function GetVisible: boolean;
    function GetVisibleRestrict: TObjectRestrict;
  public
    constructor Create(aOwner: TGroupFilters);
    procedure LoadFilters(TableMeta: TTableMeta);
    procedure UnloadFilters(TableMeta: TTableMeta);
    function IsGlobal: boolean;
    function IsLocal(const DatasetId: Integer): boolean;
    procedure UpdateFilters;
    function IdenticalTo(FilterItem: TFilterItem): boolean;
    property Owner: TGroupFilters read FOwner;
    property Items[const Index: Integer]: TGroupFilterItem read GetItem write SetItem; default;
    property Active: boolean read GetActive write SetActive;
    property ActiveRestrict: TObjectRestrict read GetActiveRestrict;
    property Visible: boolean read GetVisible;
    property VisibleRestrict: TObjectRestrict read GetVisibleRestrict;
  end;

  TGroupFilters = class
  private
    FList: TStrings;
    FDatasetIndexes: TStrings;
    function GetCount: Integer;
    function GetNames(const Index: Integer): string;
    function GetItem(const Index: Integer): TGroupFilter;
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOf(const Name: string): Integer;
    procedure Clear;
    procedure LoadFromDatabase;
    procedure LoadFilters(TableMeta: TTableMeta);
    procedure UnloadFilters(TableMeta: TTableMeta);
    function IdenticalTo(FilterItem: TFilterItem): boolean;
    function PrepareVariableIndex(const DatasetId: Integer; const Global: boolean): Integer;
    property Count: Integer read GetCount;
    property Names[const Index: Integer]: string read GetNames;
    property Items[const Index: Integer]: TGroupFilter read GetItem; default;
  end;

  /// <summary>читает список конфигураций</summary>
procedure LoadConfigurations(aList: TStrings);

var
  { DeFormat, DeHeader, DeIdentity, }
  DeRTF, DeExcelXML, DeValue, DeXML, dbcoXML: UINT;
  DeClipboard: TDeClipboard;
  MetaVersion: string; // версия метаструктуры
  MainMenuMeta: TContextMeta;
  GroupFilters: TGroupFilters;

implementation

uses Printers, Math, StdCtrls, ExtCtrls, Forms, ShlObj, ShellAPI, StrUtils,
  DeLog, Dictionary, DSMeta, DataManager, Funcs, DataUnit, DeVariable,
  DataCacheUnit, ActionUnit, DeMetadata, Security, DeScript, DeMatrix, DeFilters,
  DeControls, DeCalculator, QueryDescriptor, UnitA, DeTemplate
  {$IFDEF DEBUG}, uTextTable{$ENDIF};

// ------------ SERVICE FUNCTIONS ------------------------------------------------------

procedure LoadConfigurations(aList: TStrings);
var
  Q: TDeDataSet;
  R, ItemID: Integer;
begin
  Q := nil;
  aList.Clear;
  if MetaData.MetadataDB.Connected then
    try
      Q := MetaData.MetadataDB.CreateQuery(qtSelect);
      Q.Descr.BeginUpdate;
      try
        Q.Descr.Table := tblSolutions;
        Q.Descr.AddSortField(fldConfName);
        // 27.08.2015 + Куфенко: Читаем только необходимые поля!
        Q.Descr.AddFields([fldConfId, fldConfName]);
        // 27.08.2015 -
      finally
        Q.Descr.EndUpdate;
      end;
      Q.Open;
      for R:=0 to Pred(Q.RecordCount) do
        begin
          Q.RecNo:= R;
          ItemID := VarToInt(Q.ValueByName[fldConfId]) + 1;
          aList.AddObject(UTF8ToString(RawByteString(Q.ValueByName[fldConfName])), pointer(ItemID));
        end;
    finally
      Q.Free;
    end;
end;

// -------------------------------------------------------------------------------------
{ TParamItem }

procedure TParamItem.Assign(Source: TObject);
begin
  if Source is TDeDataSet then
  begin
    FValue:= unassigned;
    FName:= Trim(TDeDataSet(Source).StringValueByName(fldParamName));
    FAsString:= Trim(TDeDataSet(Source).StringValueByName(fldParamValue));
    FDataType:= TFieldType(VarToInt(TDeDataSet(Source).ValueByNameDef(fldParamDataType, Integer(ftString))));
    FWhenCalc:= TWhenCalc(VarToInt(TDeDataSet(Source).ValueByNameDef(fldParamWhenCalc, 0)))
  end;
end;

{ TParamsList }

function TParamList.InnerGetParam(const Index: Integer): TParamItem;
begin
  Result := TParamItem(inherited Items[Index]);
end;

procedure TParamList.InnerSetParam(const Index: Integer; const Param: TParamItem);
begin
  if Index > Count then
    Count := Succ(Index);
  inherited Items[Index] := Param;
end;

procedure TParamList.AddParam(aName: String; aValue: Variant);
var N: Integer;
begin
  N:= Add(TParamItem.Create);
  Items[N].Name:= aName;
  Items[N].Value:= aValue;
end;

procedure TParamList.SetParam(aName: String; aValue: Variant);
var N: Integer;
begin
  N:= ParamIndex(aName);
  if N = -1 then
    begin
      N:= Add(TParamItem.Create);
      Items[N].Name:= aName;
    end;

  Items[N].Value:= aValue;
end;

function TParamList.FindParam(const ParamName: string): TParamItem;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to Pred(Count) do
    if SameText(Items[Index].Name, ParamName) then Exit(Items[Index]);
end;

function TParamList.ParamIndex(const ParamName: string): Integer;
var
  Index: Integer;
begin
  for Index := 0 to Pred(Count) do
    if SameText(Items[Index].Name, ParamName) then
      Exit(Index);

  Exit(-1);
end;

{$IFDEF DEBUG}
procedure TParamList.DebugParamsLog(const Text: string);
  function PrepareParamsLog: string;
  const
    FieldTypeNames: array [TFieldType] of PChar = ('ftUnknown', 'ftString',
      'ftSmallint', 'ftInteger', 'ftWord', 'ftBoolean', 'ftFloat', 'ftCurrency',
      'ftBCD', 'ftDate', 'ftTime', 'ftDateTime', 'ftBytes', 'ftVarBytes',
      'ftAutoInc', 'ftBlob', 'ftMemo', 'ftGraphic', 'ftFmtMemo', 'ftParadoxOle',
      'ftDBaseOle', 'ftTypedBinary', 'ftCursor', 'ftFixedChar', 'ftWideString',
      'ftLargeint', 'ftADT', 'ftArray', 'ftReference', 'ftDataSet', 'ftOraBlob',
      'ftOraClob', 'ftVariant', 'ftInterface', 'ftIDispatch', 'ftGuid',
      'ftTimeStamp', 'ftFMTBcd', 'ftFixedWideChar', 'ftWideMemo',
      'ftOraTimeStamp', 'ftOraInterval', 'ftLongWord', 'ftShortint', 'ftByte',
      'ftExtended', 'ftConnection', 'ftParams', 'ftStream', 'ftTimeStampOffset',
      'ftObject', 'ftSingle'); // 49..51
    WhenCalcNames: array [TWhenCalc] of PChar = ('wcAuto', 'wcNone', 'wcOne',
      'wcEvery');
  var
    TextTable: TTextTable;
    Index: Integer;
    ParamItem: TParamItem;
  begin
    if Count = 0 then
      Result := EmptyStr
    else
      begin
        TextTable := TTextTable.Create;
        try
          TextTable.Columns.Add('No', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Name', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Type', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Calc', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Value', 0, taLeftJustify, taLeftJustify);
          for Index := 0 to Pred(Count) do
            begin
              TextTable.Lines[Index][0] := IntToStr(Succ(Index));
              ParamItem := Items[Index];
              if Assigned(ParamItem) then
                begin
                  TextTable.Lines[Index][1] := ParamItem.Name;
                  TextTable.Lines[Index][2] := StrPas(FieldTypeNames[ParamItem.DataType]);
                  TextTable.Lines[Index][3] := StrPas(WhenCalcNames[ParamItem.WhenCalc]);
                  TextTable.Lines[Index][4] := VariantToString(ParamItem.Value);
                end;
            end;
          Result := #13#10 + TextTable.AsText(24);
        finally
          TextTable.Free;
        end;
      end;
  end;
begin
  DebugLog(Text + PrepareParamsLog);
end;
{$ENDIF}

procedure TParamList.MergeFromSystem;
var
  StrValue: string;
  Value: Variant;
  Printer: TPrinter;
  procedure ModifyParam(const ParamName: string; const ParamType: TFieldType;
    const Value: Variant);
  var
    Index: Integer;
  begin
    Index := ParamIndex(ParamName);
    if Index = -1 then
    begin
      Index := Add(TParamItem.Create);
      Items[Index].Name := ParamName;
      Items[Index].DataType := ParamType;
      Items[Index].WhenCalc := wcNone;
    end;
    Items[Index].AsString := VarToStr(Value);
    Items[Index].Value := Value;
  end;

begin
  try
    Printer := Printers.Printer;
    Value := Printer.Printers[Printer.PrinterIndex];
  except
    on E: Exception do
    begin
      {$IFDEF DEBUG}
      DebugLog('%s.MergeFromSystem() prepare default printer name error: %s', [ClassName, E.Message]);
      {$ENDIF}
      Value := Null;
    end;
  end;
  ModifyParam(varDefaultPrinterName, ftString, Value);
  ModifyParam(varApplicationInstance, ftLargeInt, hInstance);
  if Assigned(Application.MainForm) then
    Value := Application.MainForm.Handle
  else
    Value := Null;
  ModifyParam(varMainFormHandle, ftLargeInt, Value);
  ModifyParam(varWindowsUserName, ftString, GetWindowsUserName);
  StrValue := GetComputerNetName;
  if Length(StrValue) = 0 then
    Value := Null
  else
    Value := StrValue;
  ModifyParam(varHostName, ftString, Value);
  ModifyParam(varUserHostName, ftString, Value);
  StrValue := GetHostIP(StrValue);
  if Length(StrValue) = 0 then
    Value := Null
  else
    Value := StrValue;
  ModifyParam(varIP, ftString, Value);
  ModifyParam(varUserIP, ftString, Value);
  StrValue := ExtractFilePath(GetModuleName(hInstance));
  if Length(StrValue) = 0 then
    Value := Null
  else
    Value := IncludeTrailingPathDelimiter(StrValue);
  ModifyParam(varApplicationDirectory, ftString, Value);
  StrValue := GetDocumentsDirectory(False);
  if Length(StrValue) = 0 then
    Value := Null
  else
    Value := StrValue;
  ModifyParam(varUserDocumentsDirectoryName, ftString, Value);
  StrValue := GetDocumentsDirectory(True);
  if Length(StrValue) = 0 then
    Value := Null
  else
    Value := StrValue;
  ModifyParam(varAllUsersDocumentsDirectoryName, ftString, Value);
  if GetSystemMetrics(SM_REMOTESESSION) <> 0 then
  begin
    StrValue := GetWTSClientIP;
    if Length(StrValue) = 0 then
      Value := Null
    else
      Value := StrValue;
    ModifyParam(varUserIP, ftString, Value);
    StrValue := GetWTSClientName;
    if Length(StrValue) = 0 then
      Value := Null
    else
      Value := StrValue;
    ModifyParam(varUserHostName, ftString, Value);
  end;
end;

// ---------------------------------------------------------------------------------------------------

{ TTreeMeta }

constructor TTreeMeta.Create(const Owner: TTreeMeta);
begin
  inherited Create;
  if Assigned(Owner) then
  begin
    FOwnerID := Owner.ID;
    FOwner := Owner;
    Owner.Add(Self);
  end;
end;

destructor TTreeMeta.Destroy;
begin
  if Assigned(Owner) then
    Owner.Extract(Self);
  inherited Destroy;
end;

function TTreeMeta.GetItem(const Index: Integer): TTreeMeta;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TTreeMeta(inherited Items[Index])
  else
    Result := nil;
end;

procedure TTreeMeta.PutItem(const Index: Integer; aChild: TTreeMeta);
begin
  if Index >= Count then
    Count := Succ(Index);
  inherited Items[Index] := aChild;
end;

function TTreeMeta.GetLevel: Integer;
begin
  if not Assigned(Owner) then
    Result := 0
  else
    Result := Owner.Level + 1;
end;

function TTreeMeta.IsOwner(aNode: TTreeMeta): boolean;
begin
  if Assigned(aNode) and (aNode <> Self) then
    while Assigned(aNode) and (aNode <> Self) do
      aNode := aNode.Owner;
  Result := (aNode <> nil);
end;

procedure TTreeMeta.Notify(Ptr: pointer; Action: TListNotification);
begin
  case Action of
    lnAdded:
      TTreeMeta(Ptr).Owner := Self;
    lnExtracted:
      TTreeMeta(Ptr).Owner := nil;
  end;
  inherited Notify(Ptr, Action);
end;

function TTreeMeta.FindNode(const aNodeID: Variant): TTreeMeta;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to Pred(Count) do
  begin
    if VarSameValue(Items[Index].ID, aNodeID) then
      Result := Items[Index]
    else
      Result := Items[Index].FindNode(aNodeID);
    if Assigned(Result) then
      Break;
  end;
end;

function TTreeMeta.NodeFound(aNode: TTreeMeta): boolean;
var
  I: Integer;
begin
  Result := (Self = aNode) or (IndexOf(aNode) >= 0);
  if not Result then
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].NodeFound(aNode);
      if Result then
        Break;
    end;
end;

function TTreeMeta.MakeTree(RootID: Variant; aErrorsList: TObjectList = nil): boolean;
var
  Node, NodeOwner: TTreeMeta;
  I: Integer;
begin
  Result := True;
  for I := Count - 1 downto 0 do
  begin
    Node := Items[I];
    if not(VarIsEmpty(Node.ID) or VarIsNull(Node.ID) or (Node.ID = RootID)) then
      if VarSameValue(Node.ID, Node.OwnerID) then
      begin
        // элемент замкнут сам на себя
        Result := False;
        if Assigned(aErrorsList) then
          aErrorsList.Add(TSelfRingError.Create(VarToInt(Node.ID)));
      end
      else if not VarSameValue(ID, Node.OwnerID) then
      begin
        NodeOwner := FindNode(Node.OwnerID);
        if Assigned(NodeOwner) then
          if Node.IsOwner(NodeOwner) then
          begin
            // обнаружена кольцевая ссылка
            Result := False;
            if Assigned(aErrorsList) then
              aErrorsList.Add(TRingError.Create(VarToInt(Node.ID)));
          end
          else
          begin
            Extract(Node);
            // Node.Owner := NodeOwner;
            NodeOwner.Insert(0, Node);
            // Extract(Node);
          end
        else
        begin
          // обнаружен элемент с неизвестным Owner'ом
          Result := False;
          if Assigned(aErrorsList) and (not(Node.OwnerID = RootID)) then
            aErrorsList.Add(TUnknownOwnerError.Create(VarToInt(Node.ID),
              Format(GetTitle('_eRror.tree'), [Node.Name, VarToStr(Node.ID)])));
        end
      end;
  end;
end;

function TTreeMeta.MakeTree(aErrorsList: TObjectList = nil): boolean;
begin
  Result := MakeTree(Null, aErrorsList);
end;

procedure TTreeMeta.Assign(Source: TObject);
begin
  if Source is TTreeMeta then
  begin
    ID := TTreeMeta(Source).ID;
    OwnerID := TTreeMeta(Source).OwnerID;
    Name := TTreeMeta(Source).Name;
  end;
end;

{$IFDEF DeDEBUG}

function TTreeMeta.Display(const aPrefix: string = ''): string;
var
  I: Integer;
begin
  Result := Format('%s%6d (%d)'#13#10, [aPrefix, VarToInt(ID),
    VarToInt(OwnerID)]);
  for I := 0 to Count - 1 do
    Result := Result + Items[I].Display(aPrefix + '    ');
end;
{$ENDIF}

procedure TTreeMeta.ConvertToList(aList: TList);
var
  I: Integer;
begin
  // разворачивает все дочерние элементы в плоский список
  for I := 0 to Count - 1 do
  begin
    if aList.IndexOf(Items[I]) < 0 then
      aList.Add(Items[I]);
    if Self <> Items[I] then
      Items[I].ConvertToList(aList);
  end;
end;

{ TControlsList }

constructor TControlsList.Create;
begin
  inherited Create(False);
end;

function TControlsList.GetControl(const Index: Integer): TControl;
begin
  Result := TControl(inherited Items[Index]);
end;

procedure TControlsList.SetControl(const Index: Integer; aControl: TControl);
begin
  if Index >= Count then
    Count := Succ(Index);
  inherited Items[Index] := aControl;
end;

procedure TControlsList.AddControl(aControl: TControl);
begin
  if IndexOf(aControl) < 0 then
    Add(aControl);
end;

procedure TControlsList.RemoveControl(aControl: TControl);
var
  ControlIdx: Integer;
begin
  ControlIdx := IndexOf(aControl);
  if ControlIdx >= 0 then
    Delete(ControlIdx);
end;

{ TFieldMeta }

constructor TFieldMeta.Create;
begin
  inherited Create;
  FVisibleLevel := fvLevel2;
  FControls := TControlsList.Create;
  FDefaultPostfix := TExpressionItem.Create;

  FPolicySelect := True; // Unassigned;
  // Закомментировал код ниже, т.к. Variant переменные по умолчанию Unassigned!

  {
    FUID           := Unassigned;
    FUVisible      := Unassigned;
    FUOrder        := Unassigned;
    FUWidth        := Unassigned;
  }
  FStage := fsBase;
  // ReadOnly      := False;
  FCodePage := -1; // Автоматически брать из кодовой страницы базы данных ...
end;

destructor TFieldMeta.Destroy;
begin
  FDefaultPostfix.Free;
  FControls.Free;
  if Assigned(FLookUp) then
    MetaData.ReleaseCache(TDataCache(FLookUp));
  inherited Destroy;
end;

procedure TFieldMeta.SetOriginal(const aOriginal: string);
begin
  FOriginal := aOriginal;
  if (not IsLookup) and Assigned(LookupPair) then
    FLookupPair.FOriginal := Original;
end;

procedure TFieldMeta.SetName(const aName: string);
begin
  FName := aName;
  if (not IsLookup) and Assigned(LookupPair) then
    FLookupPair.FName := Name;
end;

function TFieldMeta.GetNative: string;
begin
  Result := GetTitle(Name);
end;

function TFieldMeta.GetReadOnly: boolean;
begin
  Result := (csReadOnly in FFlagState) or FErrorInStructure;

  if (IsLookup) and Assigned(LookupPair) then
    Result := Result or FLookupPair.IsReadOnly;
end;

function TFieldMeta.IsReadOnlyExt: boolean;
begin
  Result := ReadOnly or Key or (Not FStored) or
    (Not SecuritySystem.CheckPolicyField(FId, sdUpdate));
end;

procedure TFieldMeta.SetRole(const aRole: TFieldRole);
begin
  FRole := aRole;
  if (not IsLookup) and Assigned(LookupPair) then
    FLookupPair.FRole := Role;
end;

function TFieldMeta.GetWidth: Integer;
var
  var_UWidth: Integer;
begin
  var_UWidth := UWidth;
  // если колонка имеет нулевую ширину, то ее не видно в списке и показать нельзя
  if VarIsEmpty(var_UWidth) or (var_UWidth = 0) or (Self = Owner.NField) then
    if FWidth = 0 then
      Result := 10
    else
      Result := FWidth
  else
    Result := var_UWidth;
end;

procedure TFieldMeta.SetWidth(aWidth: Integer);
begin
  if Assigned(Owner) then
    Owner.CheckState(tsUserValues);
  FUWidth := aWidth;
end;

function TFieldMeta.TextLength: Integer;
begin
  case DataType of
     ftByte: Result:= 3;
     ftShortInt: Result:= 4;
     ftWord: result:= 5;
     ftSmallint: Result:= 6;
     ftLongWord: Result:= 10;
     ftInteger, ftAutoInc: Result:= 11;
     ftLargeint: Result:= 20;
     else Result:= 10;
  end;
end;

function TFieldMeta.GetOrder: Integer;
begin
  // if (self=Owner.NField) then result:=0 else
  if VarIsEmpty(UOrder) then
    Result := FOrder
  else
    Result := UOrder;
end;

procedure TFieldMeta.SetOrder(aOrder: Integer);
begin
  FUOrder := aOrder;
end;

function TFieldMeta.GetAlignment: TAlignment;
begin
  if (not IsLookup) and Assigned(LookupPair) then Result := LookupPair.Alignment else
  if (ShowType = stYesNo)          then Result:= taCenter else
  if (frDeleteSignField in Role)   then Result:= taCenter else
    // if (frArchiveSignField in Role) then result:= taCenter else
  if (frDefaultRecordSign in Role) then Result:= taCenter else
  if DataType in NumericTypes      then Result:= taRightJustify else
  if DataType = ftBoolean          then Result:= taCenter else
  if DataType in BinaryTypes       then Result:= taCenter else
                                        Result:= taLeftJustify
end;

function TFieldMeta.GetCodePage: Integer;
begin
  if (FCodePage = -1) and Assigned(Owner) and Assigned(Owner.Database) then
    FCodePage := Owner.Database.CodePage;
  Result := FCodePage;
end;

function TFieldMeta.GetLookup: TObject;
begin
  if { IsLookup and } (not Assigned(FLookUp)) then
    FLookUp := MetaData.GetLibrary(Link);
  Result := FLookUp;
end;

function TFieldMeta.GetRoleAsInteger: Integer;
var
  I: TFieldRoleType;
begin
  Result := 0;
  for I := Low(TFieldRoleType) to High(TFieldRoleType) do
    if I in FRole then
      Result := Result or RoleFlag[I];
end;

procedure TFieldMeta.SetRoleAsInteger(const aIntRole: Integer);
var
  I: TFieldRoleType;
begin
  FRole := [];
  for I := Low(TFieldRoleType) to High(TFieldRoleType) do
    if (aIntRole and RoleFlag[I]) <> 0 then
      Include(FRole, I);
end;

procedure TFieldMeta.SetStage(aStage: TFieldStage);
begin
  if Not IsLookup then
    begin
      FStage:= aStage;
      if Assigned(LookupPair) then LookupPair.Stage:= aStage;
    end;
end;

procedure TFieldMeta.SetLink(const Value: Variant);
begin
  FLink := Value;
  FLinkTable := nil;
end;

procedure TFieldMeta.SetLinkByName(aLinkTable: String);
begin
  if Length(aLinkTable) = 0 then
    begin
      FLink := null;
      FLinkTable := nil;
    end
  else
    begin
      FLinkTable := MetaData.GetTableByName( aLinkTable, Owner.Database);
      if assigned(FLinkTable) then FLink := FLinkTable.ID
                              else FLink := null;
    end;
end;

function TFieldMeta.GetLinkTable: TTableMeta;
begin
  if FLink = null then Exit(nil);

  if not Assigned(FLinkTable) then FLinkTable:= MetaData.GetTableMeta(FLink);
  Result:= FLinkTable;
end;

function TFieldMeta.GetVariantGUID: Variant;
begin
  if csGUID in FFlagState then Result:= GUIDToString(FGUID)
                          else Result:= Null;
end;

function TFieldMeta.GetVisibleLevel: TFieldVisible;
begin
  if PolicySelect then
    if Assigned(Owner.NField) and (ID = Owner.NField.ID) then
      Result := fvCaption
    else if VarIsEmpty(UVisible) then
      Result := FVisibleLevel
    else if UVisible = 3 then
      Result := fvLevel3
    else if UVisible = 2 then
      Result := fvLevel2
    else
      Result := fvLevel1
  else
    Result := fvService;
end;

function TFieldMeta.GetOriginalVisibleLevel: TFieldVisible;
begin
  if PolicySelect then
    if Assigned(Owner.NField) and (ID = Owner.NField.ID) then
      Result := fvCaption
    else
      Result := FVisibleLevel
  else
    Result := fvService;
end;

function TFieldMeta.GetPolicyShow: boolean;
begin
  if FPolictyShow = orNone then
    if Assigned(SecuritySystem) then
      if SecuritySystem.CheckPolicyField(FId, sdShow) then
        FPolictyShow := orEnabled
      else
        FPolictyShow := orDisabled;
  Result := FPolictyShow in [orNone, orEnabled];
end;

procedure TFieldMeta.SetValue1(const Value: string);
var Parser   : TDeParser;
    Postfix  : TExpressionItem;
    NeedDefault: Boolean;
begin
  FValue1 := Value;
  NeedDefault:= True;

  if 0<Length(Value) then
    begin
      Parser:= TDeParser.Create;
      Postfix:= TExpressionItem.Create;

      try
        Parser.Parse(Value, Postfix);
        VValue1 := DM.Calculator.Calculate(Postfix, nil);
        NeedDefault:= False;
      except
        on E: Exception do
          begin
            {$IFDEF DEBUG}
            DebugLog(ClassName + '.SetValue1 calculate value error: ' + E.Message);
            {$ENDIF}
          end;
      end;
      Postfix.Free;
      Parser.Free;
    end;

  if NeedDefault then
    begin
     if DataType in IntegerTypes then VValue1:= 0 else
     if DataType in LogicalTypes then VValue1:= False else
     if DataType in StringTypes then
       if DataSize<5 then VValue2:= 'F'
                     else VValue2:= 'False'
    end;
end;

procedure TFieldMeta.SetValue2(const Value: string);
var Parser   : TDeParser;
    Postfix  : TExpressionItem;
    NeedDefault: Boolean;
begin
  FValue2 := Value;
  NeedDefault:= True;

  if 0<Length(Value) then
    begin
      Parser:= TDeParser.Create;
      Postfix:= TExpressionItem.Create;

      try
        Parser.Parse(Value, Postfix);
        VValue2 := DM.Calculator.Calculate(Postfix, nil);
        NeedDefault:= False;
      except
        on E: Exception do
          begin
            {$IFDEF DEBUG}
            DebugLog(ClassName + '.SetValue2 calculate value error: ' + E.Message);
            {$ENDIF}
          end;
      end;
      Postfix.Free;
      Parser.Free;
    end;

  if NeedDefault then
    begin
     if DataType in IntegerTypes then VValue2:= 1 else
     if DataType in LogicalTypes then VValue2:= True else
     if DataType in StringTypes then
       if DataSize<5 then VValue2:= 'T'
                     else VValue2:= 'True';
    end;
end;

procedure TFieldMeta.SetVisibleLevel(aVisibleLevel: TFieldVisible);
begin
  case aVisibleLevel of
    fvService, fvLevel1:
      begin
        if Assigned(Owner) then
          Owner.CheckState(tsUserValues);
        FUVisible := 1;
      end;
    fvLevel2, fvLevel3:
      begin
        if Assigned(Owner) then
          Owner.CheckState(tsUserValues);
        FUVisible := 2;
      end;
  end;
end;

function TFieldMeta.Save: boolean;
var
  DMan: TFieldsDataManager;
begin
  DMan := nil;
  try
    DMan := TFieldsDataManager.Create;
    Result := DMan.UpdateFieldMeta(Self);
    if not Result then
      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25,
        NativeInt(PChar(DMan.Errors.GetMessage)))
  finally
    DMan.Free;
  end;
end;

function TFieldMeta.UserApply: boolean;
begin
  FWidth := Width;
  FOrder := Order;
  FVisibleLevel := VisibleLevel;
  Result := True;
end;

procedure TFieldMeta.Assign(Source: TObject; const FullAssign: boolean);
var
  FieldIdx: Integer;
  DeSource: TDeDataSet;
  TM: TTableMeta;
begin
  if Source is TDeDataSet then
  begin
    DeSource := TDeDataSet(Source);
    FName:= DeSource.StringValue(indFieldsName);
    FWidth:= DeSource.IntValueDef(indFieldsWidth);
    FVisibleLevel:= TFieldVisible(DeSource.IntValueDef(indFieldsVisible));
    FOrder := DeSource.IntValueDef(indFieldsOrder);
    if FullAssign then
    begin
      FId := DeSource.IntValueDef(indFieldsID);
      FOriginal := DeSource.StringValue(indFieldsOriginal);
      ReadOnly := DeSource.IntValueDef(indFieldsReadOnly) <> 0;
      Key := DeSource.IntValueDef(indFieldsKey) <> 0;
      Unique := DeSource.IntValueDef(indFieldsUnique) <> 0;
      NotNull := DeSource.IntValueDef(indFieldsNotNull) <> 0;
      Calculated := DeSource.IntValueDef(indFieldsCalculated) <> 0;
      FDataType := TFieldType(DeSource.IntValueDef(indFieldsDataType));
      FDataSize := DeSource.IntValueDef(indFieldsDataSize);
      FLink := DeSource.IntValueDef(indFieldsLink);
      FLinkType := TDeleteAction(DeSource.IntValueDef(indFieldsOnDelete));

      RoleAsInteger := DeSource.IntValueDef(indFieldsRole);
      FShowType := TShowType(DeSource.IntValueDef(indFieldsShowType));
      FTemplate := DeSource.StringValue(indFieldsTemplate);
      FDefaultValue := DeSource.StringValue(indFieldsDefault);

      if mpFieldDefaultDB in MetaData.MetadataPresents then
        FDefaultDefDB := DeSource.StringValue(indFieldsDefaultDB);

      Value1 := DeSource.StringValue(indFieldsValue1);
      Value2 := DeSource.StringValue(indFieldsValue2);
      if 0<=indFieldsDeleted then FDeleted := DeSource.IntValueDef(indFieldsDeleted) <> 0
                             else FDeleted := False;
      FLinkRole := DeSource.StringValue(indFieldsLinkRole);
      if 0<= indFieldsDescription then FDescription := DeSource.StringValue(indFieldsDescription)
                                  else FDescription := EmptyStr;
      FCategoryID := DeSource.IntValueDef(indFieldsCategory);
      FStored := DeSource.IntValueDef(indFieldsStored) <> 0;
      ReadOnly := ReadOnly or (not IsStored);

      if Assigned(Owner) and Assigned(Owner.Database) then FCodePage := Owner.Database.CodePage
                                                      else FCodePage := cpNone; // ANSI по умолчанию ...
      if 0<= indFieldsCharset then FCodePage := DeSource.IntValueDef(indFieldsCharset, FCodePage);

      if (0<= indFieldsGUID) and not VarIsNull(DeSource.Value[indFieldsGUID]) then
        try
          FGUID := StringToGUID(VarToStr(DeSource.Value[indFieldsGUID]));
          Include(FFlagState, csGUID);
        except
{$IFDEF DEBUG}
          on E: Exception do
            if Length(FOriginal) = 0 then
              DebugLog(ClassName + '.Assign skip read GUID error: ' + E.Message)
            else
              DebugLog(ClassName + '.Assign skip read GUID error for ' + QuotedStr(FOriginal) + ': ' + E.Message);
{$ENDIF}
        end;
    end;
  end
  else if Source is TCacheItem then
    with TCacheItem(Source), TCacheItem(Source).Owner do
    begin
      Self.FName := ValueNativeByName[fldFieldsName];

      Self.FWidth := VarToInt(FieldValue[Fields.IndexByName(fldFieldsWidth)]);
      Self.FVisibleLevel := TFieldVisible(VarToInt(FieldValue[Fields.IndexByName(fldFieldsVisible)]));
      Self.FOrder := VarToInt(FieldValue[Fields.IndexByName(fldFieldsOrder)]);
      if FullAssign then
      begin
        Self.FId := VarToInt(FieldValue[Fields.IndexByName(fldFieldsID)]);
        Self.FOriginal := ValueNativeByName[fldFieldsOriginal];
        Self.Owner := MetaData.GetTableMeta(VarToInt(FieldValue[Fields.IndexByName(fldFieldsTable)]));
        Self.ReadOnly := (VarToInt(FieldValue[Fields.IndexByName(fldFieldsReadOnly)]) <> 0);
        Self.Key := (VarToInt(FieldValue[Fields.IndexByName(fldFieldsKey)]) <> 0);
        Self.Unique := (VarToInt(FieldValue[Fields.IndexByName(fldFieldsUnique)]) <> 0);
        Self.NotNull := (VarToInt(FieldValue[Fields.IndexByName(fldFieldsNotNull)]) <> 0);
        Self.Calculated := (VarToInt(FieldValue[Fields.IndexByName(fldFieldsCalculated)]) <> 0);
        Self.FDataType := TFieldType(VarToInt(FieldValue[Fields.IndexByName(fldFieldsDataType)]));
        Self.FDataSize := VarToInt(FieldValue[Fields.IndexByName(fldFieldsDataSize)]);
        Self.FLink := VarToInt(FieldValue[Fields.IndexByName(fldFieldsLink)]);
        Self.FLinkType := TDeleteAction(VarToInt(FieldValue[Fields.IndexByName(fldFieldsOnDelete)]));
        Self.RoleAsInteger := VarToInt(FieldValue[Fields.IndexByName(fldFieldsRole)]);
        Self.FShowType := TShowType(VarToInt(FieldValue[Fields.IndexByName(fldFieldsShowType)]));
        Self.FTemplate := ValueNativeByName[fldFieldsTemplate];
        Self.FDefaultValue := ValueNativeByName[fldFieldsDefault];
        if mpFieldDefaultDB in MetaData.MetadataPresents then
          Self.FDefaultDefDB := ValueNativeByName[fldFieldsDefaultDB];
        Self.Value1 := ValueNativeByName[fldFieldsValue1];
        Self.Value2 := ValueNativeByName[fldFieldsValue2];
        Self.FDeleted := (VarToInt(ValueByName[fldFieldsDeleted]) <> 0);
        Self.FLinkRole := ValueNativeByName[fldFieldsLinkRole];
        Self.FCategoryID := VarToInt(ValueByName[fldFieldsCategory]);
        Self.FStored := (VarToInt(ValueByName[fldFieldsStored]) <> 0);
        Self.ReadOnly := Self.ReadOnly or (not Self.IsStored);

        FieldIdx := Fields.IndexByName(fldFieldsDescription);
        if FieldIdx >= 0 then
          Self.FDescription := FieldNativeValue[FieldIdx];

        FieldIdx := Fields.IndexByName(fldFieldsPrecision);
        if FieldIdx >= 0 then
          Self.FPrecision := (VarToInt(FieldValue[FieldIdx]));

        if Assigned(Self.Owner) and Assigned(Self.Owner.Database) then
          FCodePage := Self.Owner.Database.CodePage
        else
          FCodePage := 0; // ANSI по умолчанию ...
        FieldIdx := Fields.IndexByName(fldFieldsCharset);
        if FieldIdx <> -1 then
        begin
          if VarIsNumeric(FieldValue[FieldIdx]) then
            FCodePage := FieldValue[FieldIdx];
        end
        else
          case VarToInt(ValueByName[fldFieldsShowType]) of
            2: { 2 stUnicode }
              FCodePage := cpUTF8;
            4: { 4 stDOSString }
              FCodePage := cp866;
          end;
        FieldIdx := Fields.IndexByName(fldFieldsGUID);
        if (FieldIdx <> -1) and not VarIsNull(FieldValue[FieldIdx]) then
          try
            FGUID := StringToGUID(VarToStr(FieldValue[FieldIdx]));
            Include(FFlagState, csGUID);
          except
{$IFDEF DEBUG}
            on E: Exception do
              if Length(FOriginal) = 0 then
                DebugLog(ClassName + '.Assign skip read GUID error: ' + E.Message)
              else
                DebugLog(ClassName + '.Assign skip read GUID error for ' + QuotedStr(FOriginal) + ': ' + E.Message);
{$ENDIF}
          end;
      end;
    end
  else if Source is TFieldMeta then
  begin
    FName := TFieldMeta(Source).Name;
    FVisibleLevel := TFieldMeta(Source).FVisibleLevel;
    FWidth := TFieldMeta(Source).FWidth;   // Строго без GET SET обертки
    FOrder := TFieldMeta(Source).FOrder;   // Строго без GET SET обертки
    if FullAssign then
    begin
      FId := TFieldMeta(Source).ID;
      FOriginal := TFieldMeta(Source).Original;
      FDescription := TFieldMeta(Source).Description;
      FOwner := TFieldMeta(Source).Owner;
      ReadOnly := TFieldMeta(Source).IsReadOnly;
      Key := TFieldMeta(Source).Key;
      Unique := TFieldMeta(Source).Unique;
      NotNull := TFieldMeta(Source).NotNull;
      Calculated := TFieldMeta(Source).Calculated;
      FDataType := TFieldMeta(Source).DataType;
      FDataSize := TFieldMeta(Source).DataSize;
      FPrecision := TFieldMeta(Source).Precision;
      FLink := TFieldMeta(Source).Link;
      FLinkType := TFieldMeta(Source).LinkType;
      FRole := TFieldMeta(Source).Role;
      FShowType := TFieldMeta(Source).ShowType;
      FTemplate := TFieldMeta(Source).Template;
      FDefaultValue := TFieldMeta(Source).DefaultValue;
      DefaultDefDB := TFieldMeta(Source).DefaultDefDB; // Строго через GET SET обертку
      FValue1 := TFieldMeta(Source).Value1;
      FValue2 := TFieldMeta(Source).Value2;
      VValue1 := TFieldMeta(Source).VariantValue1;
      VValue2 := TFieldMeta(Source).VariantValue2;
      FIsLookup := TFieldMeta(Source).IsLookup;
      FLookupPair := TFieldMeta(Source).LookupPair;
      FDeleted := TFieldMeta(Source).FDeleted;
      FLinkRole := TFieldMeta(Source).FLinkRole;
      FCategoryID := TFieldMeta(Source).FCategoryID;
      FStored := TFieldMeta(Source).FStored;
      FDescription := TFieldMeta(Source).Description;
      // 23.11.2016 +
      FUOrder := TFieldMeta(Source).FUOrder;
      FUWidth := TFieldMeta(Source).FUWidth;
      FUVisible := TFieldMeta(Source).FUVisible;
      // 23.11.2016 -
      FCodePage := TFieldMeta(Source).CodePage;
      if csGUID in TFieldMeta(Source).FlagState then
      begin
        FGUID := TFieldMeta(Source).GUID;
        Include(FFlagState, csGUID);
      end;
    end;
  end;

  if VarIsNull(FId) then
  begin
    FPolicySelect := False;
    FPolictyShow := orNone;
  end
  else
  begin
    if Assigned(Owner) and Owner.IsDynamic then
      begin
        FPolicySelect:= True;
        FPolictyShow:= orEnabled;
      end
    else
      begin
        FPolicySelect := SecuritySystem.CheckPolicyField(FId, sdSelect);
        if SecuritySystem.CheckPolicyField(FId, sdShow) then FPolictyShow := orEnabled
                                                        else FPolictyShow := orDisabled;
      end;
  end;

  if Key then
    FStage := fsKey
  else if FDataType in [ftBlob, ftMemo, ftWideMemo] then
    FStage := fsBlob
  else
    FStage := fsBase;

  { установка связи через роль набора данных }
  if Length(LinkRole) > 0 then
  begin
    try
      TM := MetaData.GetTableByName(MetaData.ParamValue[LinkRole]);

      if Assigned(TM) then
        FLink := TM.FId
      else
        FLink := StrToIntDef(MetaData.ParamValue[LinkRole], 0);

      ReadOnly := False;
    except
      on E: Exception do
      begin
{$IFDEF DEBUG}
        DebugLog(ClassName + '.Assign link role skip error: ' + E.Message);
{$ENDIF}
        FLink := 0;
      end;
    end;

    if (FLink = 0) or (not Assigned(MetaData.GetTableMeta(FLink))) then
    begin
      FLink := 0;
      ReadOnly := True;
    end;
  end;
end;

procedure TFieldMeta.AssignToCacheItem(aItem: TObject);
var
  CI: TCacheItem;
begin
  CI := TCacheItem(aItem);

  // заполнение полей записи полями TFieldMeta
  CI.ValueByName[fldFieldsID] := FId;
  CI.ValueNativeByName[fldFieldsOriginal] := FOriginal;

  if Assigned(FOwner.OwnerTable) then
    CI.ValueByName[fldFieldsTable] := FOwner.OwnerTable.ID
  else
    CI.ValueByName[fldFieldsTable] := FOwner.ID;

  CI.ValueNativeByName[fldFieldsName] := FName;
  CI.ValueByName[fldFieldsReadOnly] := BooleanToInt[ReadOnly];
  CI.ValueByName[fldFieldsKey] := BooleanToInt[Key];
  CI.ValueByName[fldFieldsUnique] := BooleanToInt[Unique];
  CI.ValueByName[fldFieldsNotNull] := BooleanToInt[NotNull];
  CI.ValueByName[fldFieldsCalculated] := BooleanToInt[Calculated];
  CI.ValueByName[fldFieldsDataType] := ord(FDataType);
  CI.ValueByName[fldFieldsDataSize] := FDataSize;
  CI.ValueByName[fldFieldsLink] := FLink;
  CI.ValueByName[fldFieldsOnDelete] := ord(FLinkType);

  CI.ValueByName[fldFieldsRole] := RoleAsInteger;
  CI.ValueByName[fldFieldsVisible] := ord(FVisibleLevel);
  CI.ValueByName[fldFieldsShowType] := ord(FShowType);
  CI.ValueByName[fldFieldsWidth] := FWidth;
  CI.ValueByName[fldFieldsOrder] := FOrder;
  CI.ValueNativeByName[fldFieldsTemplate] := FTemplate;
  CI.ValueNativeByName[fldFieldsDefault] := FDefaultValue;
  if mpFieldDefaultDB in MetaData.MetadataPresents then
    CI.ValueNativeByName[fldFieldsDefaultDB] := FDefaultDefDB;
  CI.ValueNativeByName[fldFieldsValue1] := FValue1;
  CI.ValueNativeByName[fldFieldsValue2] := FValue2;
  CI.ValueByName[fldFieldsDeleted] := BooleanToInt[FDeleted];
  CI.ValueNativeByName[fldFieldsLinkRole] := FLinkRole;
  CI.ValueByName[fldFieldsCategory] := FCategoryID;
  CI.ValueByName[fldFieldsStored] := BooleanToInt[FStored];

  if (FCodePage = cpUTF8) and (CI.Owner.Fields.IndexByName(fldFieldsCharset) <> -1) then
    CI.ValueByName[fldFieldsCharset] := FCodePage;

  if (Length(FDescription) <> 0) and
    (CI.Owner.Fields.IndexByName(fldFieldsDescription) <> -1) then
    CI.ValueByName[fldFieldsDescription] := FDescription;

  if (csGUID in FFlagState) and (CI.Owner.Fields.IndexByName(fldFieldsGUID)
    <> -1) then
    CI.ValueByName[fldFieldsGUID] := GUIDToString(FGUID);
end;

procedure TFieldMeta.UpdateControls;
var
  I: Integer;
  aValue: Variant;
begin
  for I := 0 to FControls.Count - 1 do
    if FControls[I] is TDeDSComboBox then
    begin
      aValue := TDeDSComboBox(FControls[I]).Value;
      TDeDSComboBox(FControls[I]).Prepared := False;
      TDeDSComboBox(FControls[I]).Value := aValue;
    end;
end;

procedure TFieldMeta.CreateLookupPair;
var
  Lookup_Table: TTableMeta;
  Lookup_NField: TFieldMeta;
begin
  if not IsLookup then
  begin
    if Assigned(LookupPair) then
      DestroyLookupPair;
    if Link > 0 then
    begin
      Lookup_Table := MetaData.GetTableMeta(Link);
      if Assigned(Lookup_Table) and Assigned(Lookup_Table.Database) and Assigned(Lookup_Table.NField) then
      begin
        FLookupPair := TFieldMeta.Create;
        FLookupPair.FId := -FId;
        FLookupPair.FOwner := Lookup_Table;
        Lookup_NField:= FLookupPair.Owner.NField;
        FLookupPair.FOriginal := Original + '.' + Lookup_NField.Original;

        Lookup_Table:= MetaData.GetTableMeta(Lookup_NField.Link);
        while Assigned(Lookup_Table) and Assigned(Lookup_Table.Database) and
              Assigned(Lookup_Table.NField) and (Lookup_NField<>Lookup_Table.NField) do
          begin
            Lookup_NField:= Lookup_Table.NField;
            FLookupPair.FOriginal := FLookupPair.FOriginal + '.' + Lookup_NField.Original;
            Lookup_Table:= MetaData.GetTableMeta(Lookup_NField.Link);
          end;

        FLookupPair.FName := Name;
        FLookupPair.FLink := Link;
        FLookupPair.FRole := Role;
        FLookupPair.FVisibleLevel := InnerVisibleLevel;
        FLookupPair.FOrder := Order;
        FLookupPair.FIsLookup := True;
        FLookupPair.FStored := False;
        FLookupPair.FLookupPair := Self;
        FLookupPair.FWidth := Self.Width; // FLookupPair.Owner.NField.Width;
        FLookupPair.FDataType := Lookup_NField.DataType;
        FLookupPair.FDataSize := Lookup_NField.DataSize;
        FLookupPair.FShowType := Lookup_NField.ShowType;
        FLookupPair.FTemplate := Lookup_NField.Template;
      end;
    end;
  end;
end;

procedure TFieldMeta.DestroyLookupPair;
begin
  if not IsLookup then
  begin
    FLookupPair.Free;
    FLookupPair := nil;
  end;
end;

function TFieldMeta.GetUVisible: Variant;
begin
  if Assigned(Owner) then
    Owner.CheckState(tsUserValues);
  Result := FUVisible;
end;

function TFieldMeta.GetUOrder: Variant;
begin
  if Assigned(Owner) then
    Owner.CheckState(tsUserValues);
  Result := FUOrder;
end;

function TFieldMeta.GetUWidth: Variant;
begin
  if Assigned(Owner) then
    Owner.CheckState(tsUserValues);
  Result := FUWidth;
end;

function TFieldMeta.GetKey: boolean;
begin
  Result := csKey in FFlagState;
end;

procedure TFieldMeta.SetKey(const Value: boolean);
begin
  if Value then
    Include(FFlagState, csKey)
  else
    Exclude(FFlagState, csKey);
end;

function TFieldMeta.GetUnique: boolean;
begin
  Result := csUnique in FFlagState;
end;

procedure TFieldMeta.SetUnique(const Value: boolean);
begin
  if Value then
    Include(FFlagState, csUnique)
  else
    Exclude(FFlagState, csUnique);
end;

function TFieldMeta.GetNotNull: boolean;
begin
  Result := csNotNull in FFlagState;
end;

procedure TFieldMeta.SetNotNull(const Value: boolean);
begin
  if Value then
    Include(FFlagState, csNotNull)
  else
    Exclude(FFlagState, csNotNull);
end;

function TFieldMeta.GetInternalReadOnly: boolean;
begin
  Result := csReadOnly in FFlagState;
end;

procedure TFieldMeta.SetReadOnly(const Value: boolean);
begin
  if Value then
    Include(FFlagState, csReadOnly)
  else
    Exclude(FFlagState, csReadOnly);
end;

function TFieldMeta.GetCalculated: boolean;
begin
  Result := csCalculated in FFlagState;
end;

procedure TFieldMeta.SetCalculated(const Value: boolean);
begin
  if Value then
    Include(FFlagState, csCalculated)
  else
    Exclude(FFlagState, csCalculated);
end;

procedure TFieldMeta.SetDefaultDefDB(const Value: string);
begin
  if mpFieldDefaultDB in MetaData.MetadataPresents then FDefaultDefDB:= Value
                                                   else FDefaultValue:= Value;
end;

function TFieldMeta.GetDefaultDefDB: string;
begin
  if mpFieldDefaultDB in MetaData.MetadataPresents then Result:= FDefaultDefDB
                                                   else Result:= FDefaultValue;
end;

function TFieldMeta.HaveDefaultDefDB: boolean;
begin
  if mpFieldDefaultDB in MetaData.MetadataPresents
    then Result:= (0 < Length(FDefaultDefDB))
    else Result:= (0 < Length(FDefaultValue)) and (DefaultPostfix.Count = 0); // выражение в сиснтаксисе базы
end;

procedure TFieldMeta.SetGUID(const Value: TGUID);
begin
  FGUID := Value;
  Include(FFlagState, csGUID);
end;

{ TFieldsMeta }

function TFieldsMeta.Get(const Index: Integer): TFieldMeta;
begin
  // контролировать нет смысла, поскольку ошибка будет при NIL.property
  // if (Index >= 0) and (Index < Count) then
  Result := TFieldMeta(inherited Items[Index])
  // else
  // Result := nil;
end;

procedure TFieldsMeta.Put(const Index: Integer; Field: TFieldMeta);
begin
  if Index >= Count then
    Count := Succ(Index);
  inherited Items[Index] := Field;
end;

function TFieldsMeta.IndexByName(const aName: string): Integer;
var
  I: Integer;
  bName: String;
begin
  Result := -1;
  for I := 0 to Pred(Count) do
    if SameText(TFieldMeta(inherited Items[i]).Original, aName) then
      Exit(I);

  // костыль для поддержки старых имен полей
  if UpperCase(Copy(aName, 1, 5)) = 'LINK_' then
  begin
    bName := Copy(aName, 6, 255);
    for I := 0 to Count - 1 do
      if CompareText(Items[I].Original, bName) = 0 then
        if Assigned(Items[I].LookupPair) then
          Exit(IndexOf(Items[I].LookupPair));
  end;
end;

function TFieldsMeta.FindByName(const aName: string; Recurse: boolean = False): TFieldMeta;
var FieldIndex, P: Integer;
    a, b: string;
begin
  FieldIndex := IndexByName(aName);
  if FieldIndex >= 0 then
    begin
      Result := Items[FieldIndex]
    end
  else
    begin
      Result := nil;
      if Recurse then
        begin
          P := Pos('.', aName);
          if P > 0 then
            begin
              a := Trim(Copy(aName, 1, P - 1));
              b := Trim(Copy(aName, P + 1, MaxInt));
              FieldIndex := IndexByName(a);
              if FieldIndex >= 0 then
                if (Items[FieldIndex].LookupPair <> nil) then
                  Result := Items[FieldIndex].LookupPair.Owner.Fields.FindByName(b, Recurse);
            end;
        end;
    end;
end;

function TFieldsMeta.IndexByLink(const aLink: Integer): Integer;
var I: Integer;
begin
  Result := -1;
  if aLink > 0 then
    for I := 0 to Count - 1 do
      if (not Items[I].IsLookup) and (Items[I].Link = aLink) then Exit(I);
end;

function TFieldsMeta.IndexKeyField: Integer;
var i: Integer;
begin
  Result:= -1;
  for i := 0 to Count - 1 do
    if Items[I].Key then Exit(i);
end;

function TFieldsMeta.ExtLinkCount: Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if (0<Items[I].Link) and (Items[I].Link <> Items[I].Owner.ID) then Inc(Result);
end;

function TFieldsMeta.FieldTypeByName(const FieldName: string): TFieldType;
var Index: Integer;
begin
  Index := IndexByName(FieldName);
  if Index <> -1 then
    Result := Items[Index].DataType
  else
    Result := ftUnknown;
end;

function TFieldsMeta.FindByID(const aID: Integer): TFieldMeta;
var Index: Integer;
begin
  Result := nil;
  for Index := 0 to Pred(Count) do
    if TFieldMeta(inherited Items[Index]).ID = aID then Exit(Items[Index]);
end;

function TFieldsMeta.FindByGUID(const GUID: TGUID): TFieldMeta;
var Index: Integer;
    Field: TFieldMeta;
begin
  Result := nil;
  for Index := 0 to Pred(Count) do
    begin
      Field := TFieldMeta(inherited Items[Index]);
      if Assigned(Field) and (csGUID in Field.FlagState) and IsEqualGUID(Field.GUID, GUID) then Exit(Field);
    end;
end;

function TFieldsMeta.IndexByID(const aID: Integer): Integer;
var I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if TFieldMeta(inherited Items[i]).ID = aID then Exit(I);
end;

procedure TFieldsMeta.SortByOrder;
var N: Integer;

  procedure qsort(l, r: Integer);
  var
    I, j, m: Integer;
    Fi, Fj, Fm: Variant;
    Gi, Gj, Gm: Variant;
  begin
    I := l;
    j := r;
    m := (r + l) div 2;
    Fm := Items[m].Order;
    Gm := Items[m].ID;
    repeat

      Fi := Items[I].Order;
      Gi := Items[I].ID;
      while (Fi < Fm) or ((Fi = Fm) and ((Gi < Gm))) do
      begin
        I := I + 1;
        Fi := Items[I].Order;
        Gi := Items[I].ID;
      end;

      Fj := Items[j].Order;
      Gj := Items[j].ID;
      while (Fm < Fj) or ((Fj = Fm) and ((Gm < Gj))) do
      begin
        j := j - 1;
        Fj := Items[j].Order;
        Gj := Items[j].ID;
      end;

      if (I <= j) then
      begin
        if (Fi > Fj) or ((Fi = Fm) and ((Gi > Gj))) then
          Exchange(I, j);
        I := I + 1;
        j := j - 1;
      end;
    until I > j;
    if l < j then
      qsort(l, j);
    if I < r then
      qsort(I, r);
  end;

begin
  N := Count - 1;
  while (0 <= N) and Items[N].IsLookup do
    Dec(N);
  if N > 0 then
    qsort(0, N);

  { Старая сортировка
    I := 2;
    while (I < Count) do
    begin
    Field := Items[I];
    if not Field.IsLookup then
    begin
    J := 1;
    while (J < I) and
    ( (Items[J].Order < Field.Order) OR ((Items[J].Order = Field.Order) and (Items[J].ID < Field.ID))  ) do
    inc(J);

    if I <> J then
    begin
    Move(I, J);
    if Assigned(Field.LookupPair) then
    Move(IndexOf(Field.LookupPair), J);
    end;
    end;
    inc(I);
    end;
    { }
end;

procedure TFieldsMeta.AssignFields(Source: TFieldsMeta);
var
  I: Integer;
  FMeta: TFieldMeta;
begin
  Clear;
  // копирование списка полей
  for I := 0 to Source.Count - 1 do
  begin
    FMeta := TFieldMeta.Create;
    FMeta.Assign(Source.Items[I]);
    Add(FMeta);
  end;
  // переназначение парных ссылок
  for I := 0 to Count - 1 do
    if Assigned(Items[I].LookupPair) then
      Items[I].LookupPair := Items[Source.IndexByID(Items[I].LookupPair.ID)];
end;

function TFieldsMeta.GetUserXML: string;
var
  Index: Integer;
  Field: TFieldMeta;
begin
  Result := EmptyStr;
  for Index := 0 to Pred(Count) do
  begin
    Field := Items[Index];
    if Assigned(Field) then
      Result := Result + Format(#13#10'  <field name=%s width="%d" visible="%d" order="%d" />',
                                [QuotedStr(Field.Original), Field.Width, ord(Field.VisibleLevel), Field.Order]);
  end;
  if Length(Result) <> 0 then
    Result := '<fields>' + Result + #13#10'</fields>';
end;

function TFieldsMeta.UserSave(const TableID: Integer): boolean;
var
  XML: string;
  Dataset: TDeDataSet;
  DataID: Integer;
begin
  try
    XML := UserXML;
    Result := Length(XML) <> 0;
    if Result then
    begin
      Dataset := MetaData.MetadataDB.CreateQuery(qtDelete);
      try
        Dataset.Descr.BeginUpdate;
        try
          Dataset.Descr.Table := tblUserDataSet;
          Dataset.Descr.AddCondition(fldUDataSetTable, ftInteger, opEQ, TableID);
          Dataset.Descr.AddCondition(fldUDataSetSubject, ftInteger, opEQ, UserSession.ID);
          Dataset.Descr.AddCondition(fldUDataSetType, ftSmallint, opEQ, 3);
          Dataset.Descr.AddOperation(opAnd);
          Dataset.Descr.AddOperation(opAnd);
        finally
          Dataset.Descr.EndUpdate;
        end;
        Dataset.ExecuteQuery;
      finally
        Dataset.Free;
      end;
      Dataset := MetaData.MetadataDB.CreateQuery(qtHole);
      try
        if not Assigned(Dataset) then
          Dataset := MetaData.MetadataDB.CreateQuery(qtSelect);
        Dataset.Descr.BeginUpdate;
        try
          Dataset.Descr.Table := tblUserDataSet;
          Dataset.Descr.AddField(opMax, fldUDataSetID);
        finally
          Dataset.Descr.EndUpdate;
        end;
        Dataset.Open;
        DataID := VarToInt(Dataset.Value[0]) + 1;
      finally
        Dataset.Free;
      end;
      Dataset := MetaData.MetadataDB.CreateQuery(qtInsert);
      try
        Dataset.Descr.BeginUpdate;
        try
          Dataset.Descr.Table := tblUserDataSet;
          Dataset.Descr.AddParamField(fldUDataSetID, ftInteger);
          Dataset.Descr.AddParamField(fldUDataSetTable, ftInteger);
          Dataset.Descr.AddParamField(fldUDataSetSubject, ftInteger);
          Dataset.Descr.AddParamField(fldUDataSetType, ftInteger);
          Dataset.Descr.AddParamField(fldUDataSetXML, ftString);
        finally
          Dataset.Descr.EndUpdate;
        end;
        Dataset.Descr.ParamValueByName[fldUDataSetID] := DataID;
        Dataset.Descr.ParamValueByName[fldUDataSetTable] := TableID;
        Dataset.Descr.ParamValueByName[fldUDataSetSubject] := UserSession.ID;
        Dataset.Descr.ParamValueByName[fldUDataSetType] := 3;
        Dataset.Descr.ParamValueByName[fldUDataSetXML] := XML;
        Result := Dataset.ExecuteQuery;
      finally
        Dataset.Free;
      end;
    end;
  except
    on E: Exception do
    begin
{$IFDEF DEBUG}
      DebugLog(ClassName + '.UserSave skip error: ' + E.Message);
{$ENDIF}
      Result := False;
    end;
  end;
end;

{$IFDEF DEBUG}
procedure TFieldsMeta.DebugViewFieldsLog(const Text: string);
  function PrepareFieldsLog: string;
  const
    DeleteNames: array [TDeleteAction] of PChar = ('daNone', 'daRestrict', 'daCascade', 'daSetNull', 'daFullCascade');
  var
    TextTable: TTextTable;
    Index: Integer;
    Field: TFieldMeta;
    function PrepareVisibleLevel(const Level: Variant): string;
    var
      IntLevel: NativeInt;
    begin
      if VarIsNumeric(Level) then
        begin
          IntLevel := VarToInt(Level);
          if IntLevel < 0 then
            Result := 'Skipped'
          else
            case TFieldVisible(IntLevel) of
              fvService: Result := 'fvService';
              fvLevel1: Result := 'fvLevel1';
              fvLevel2: Result := 'fvLevel2';
              fvLevel3: Result := 'fvLevel3';
              fvCaption: Result := 'fvCaption';
            else
              Result := IntToStr(IntLevel);
            end;
        end
      else
        Result := 'Unknown';
    end;
    function PrepareShowType(const ShowType: Variant): string;
    const
      TypeNames: array [TShowType] of PChar = ('stNone', 'stNative', 'stUnicode',
        'stTranslate', 'stDOSString', 'stYesNo', 'stPassword',
        'stEncryptedPassword', 'stBarCode');
    var
      IntShowType: NativeInt;
    begin
      if VarIsNumeric(ShowType) then
        begin
          IntShowType := VarToInt(ShowType);
          if (IntShowType >= NativeInt(Low(TypeNames))) and (IntShowType <= NativeInt(High(TypeNames))) then
            Result := StrPas(TypeNames[TShowType(IntShowType)])
          else
            Result := IntToStr(IntShowType);
        end
      else
        Result := 'Unknown';
    end;
  begin
    if Count = 0 then
      Result := EmptyStr
    else
      begin
        TextTable := TTextTable.Create;
        try
          TextTable.Columns.Add('No', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Order', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('UOrder', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('ID', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Field Name', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Visible', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('UVisible', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Width', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('UWidth', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Show Type', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('CodePage', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Link Type', 0, taLeftJustify, taLeftJustify);
          for Index := 0 to Pred(Count) do
            begin
              TextTable.Lines[Index][0] := IntToStr(Succ(Index));
              Field := Items[Index];
              if Assigned(Field) then
                begin
                  TextTable.Lines[Index][1] := IntToStr(Field.FOrder);
                  TextTable.Lines[Index][2] := VariantToString(Field.FUOrder);
                  TextTable.Lines[Index][3] := VariantToString(Field.FId);
                  TextTable.Lines[Index][4] := Field.FOriginal;
                  TextTable.Lines[Index][5] := PrepareVisibleLevel(Field.FVisibleLevel);
                  TextTable.Lines[Index][6] := PrepareVisibleLevel(Field.FUVisible);
                  TextTable.Lines[Index][7] := IntToStr(Field.FWidth);
                  TextTable.Lines[Index][8] := VariantToString(Field.FUWidth);
                  TextTable.Lines[Index][9] := PrepareShowType(Field.FShowType);
                  if Field.DataType in StringTypes then
                    case Field.CodePage of
                      - 1:
                        TextTable.Lines[Index][10] := 'Unknown';
                      cpNone:
                        TextTable.Lines[Index][10] := 'System';
                      cp866:
                        TextTable.Lines[Index][10] := 'DOS 866';
                      cpUTF8:
                        TextTable.Lines[Index][10] := 'UTF-8';
                    else
                      TextTable.Lines[Index][10] := IntToStr(Field.CodePage);
                    end;
                  TextTable.Lines[Index][11] := StrPas(DeleteNames[Field.FLinkType]);
                end;
            end;
          Result := #13#10 + TextTable.AsText(24);
        finally
          TextTable.Free;
        end;
      end;
  end;
begin
  DebugLog(Text + PrepareFieldsLog);
end;
{$ENDIF}

function TFieldsMeta.PrepareClipboardXML(const Level: Integer): string;
var
  Index: Integer;
  SpaceString, LineString: string;
  FieldMeta: TFieldMeta;
begin
  Result := EmptyStr;
  if Level >= 0 then
  begin
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
  for Index := 0 to Pred(Count) do
  begin
    FieldMeta := Items[Index];
    if Assigned(FieldMeta) then
    begin
      Result := Result + LineString + SpaceString + Format('<field id="%d"', [Integer(FieldMeta.ID)]);
      if Length(FieldMeta.Original) <> 0 then
        Result := Result + ' name="' + XMLEncode(FieldMeta.Original) + '"';
      Result := Result + Format(' type="%d"/>', [ord(FieldMeta.DataType)]);
    end;
  end;
  if Length(Result) <> 0 then
  begin
    if Level > 0 then
      SpaceString := DupeString('  ', Level);
    Result := LineString + SpaceString + '<fields>' + Result + LineString +
      SpaceString + '</fields>';
  end;
end;

function TFieldsMeta.LoadDefaultGUIDs: Integer;
var
  Index: Integer;
  FieldMeta: TFieldMeta;
  TableGUID, FieldGUID: TGUID;
begin
  Result := 0;
  for Index := 0 to Pred(Count) do
  begin
    FieldMeta := Items[Index];
    if Assigned(FieldMeta) and (Length(FieldMeta.Original) <> 0) and
      Assigned(FieldMeta.Owner) and (Length(FieldMeta.Owner.Table) <> 0) then
      if TDeMetadata.TryDatasetFieldToGUID(FieldMeta.Owner.Table,
        FieldMeta.Original, TableGUID, FieldGUID) then
        if tsGUID in FieldMeta.Owner.LoadingState then
        begin
          if IsEqualGUID(FieldMeta.Owner.GUID, TableGUID) then
          begin
            FieldMeta.FGUID := FieldGUID;
            Include(FieldMeta.FFlagState, csGUID);
            Inc(Result);
          end;
        end
        else
        begin
          FieldMeta.FGUID := FieldGUID;
          Include(FieldMeta.FFlagState, csGUID);
          Inc(Result);
        end;
  end;
end;

function TFieldsMeta.New(aOriginal: String; aName: String = ''): TFieldMeta;
begin
  Result:= TFieldMeta.Create;
  Result.FOriginal:= aOriginal;
  if Length(aName)=0 then Result.FName:= aOriginal
                     else Result.FName:= aName;
  Put(Count, Result);
end;

{ TContextMeta }

constructor TContextMeta.Create(aRoot: TTreeMeta; const aLoadNow: Boolean = False);
begin
  FAlign:= alNone;
  FSize:= 100;
  FDSView:= vtNone;
  inherited Create(aRoot);
  FGroups:= TList<TParentFieldValue>.Create;
  if aLoadNow then
    LoadMenu;
end;

destructor TContextMeta.Destroy;
begin
  FGroups.Free;
  inherited Destroy;
end;

function TContextMeta.GetMeta(const Index: Integer): TContextMeta;
begin
  Result := TContextMeta(inherited Items[Index]);
end;

function TContextMeta.GetSolutionID: Integer;
begin
  if not assigned(Dataset) then Exit(FSolutionID);

  if not assigned(DataSet.Database) then Exit(Dataset.SolutionID);

  if (Dataset.Database = Metadata.MetadataDB) or
     (Dataset.Database = Metadata.CatalogsDB) then Exit(MetaSolution)
                                              else Exit(Dataset.SolutionID);
end;

function TContextMeta.GetViewType: TDSViewType;
var
  VariableList: TDeVariableList;
begin
  if FDSView <> vtNone then
    Result:= FDSView
  else
    begin
      VariableList := TDeVariableList.Create;
      try
        VariableList.LoadCommaText(FDSParams);
        if VariableList.TestByName(cViewType, cViewTypeMap) then      Result:= vtTileMap else
        if VariableList.TestByName(cViewType, cViewTypeTree) then     Result:= vtTree else
        if VariableList.TestByName(cViewType, cViewTypeCalendar) then Result:= vtCalendar else
        if VariableList.TestByName(cViewType, cViewTypeChart) then    Result:= vtChart else
        if VariableList.TestByName(cViewType, cViewTypePivot) then    Result:= vtPivot else

        if assigned(Dataset) and (Dataset.GroupViewType <> vtNone) then Result:= Dataset.GroupViewType else
        if assigned(Dataset) and Assigned(Dataset.PField) then             Result:= vtTree else
        if assigned(Dataset) and Assigned(Dataset.FLatitudeField) and Assigned(Dataset.FLongitudeField) then
                                                                           Result:= vtTileMap else
                                                                           Result:= vtList;
      finally
        VariableList.Free;
      end;
    end;
end;

procedure TContextMeta.SetMeta(const Index: Integer; const Meta: TContextMeta);
begin
  inherited Items[Index] := Meta;
end;

function TContextMeta.GetDatasetID: Integer;
begin
  if Assigned(FDataSet) then
    Result := FDataSet.ID
  else
    Result := 0;
end;

function TContextMeta.GetIco: Integer;
begin
  Result := FIco;
  if Result = 0 then
  begin
    if Assigned(FDataSet) then
      Result := FDataSet.Ico
    else
      Result := 68;
  end;
end;

procedure TContextMeta.LoadMenu;
var
  Q: TDeDataSet;
  MM: TContextMeta;
  R, I: Integer;
begin
  Clear;
  Q := MetaData.MetadataDB.CreateQuery(qtSelect);
  try
    Q.Descr.Table:= tblMenu;
    Q.Descr.AddSortField(fldMenuOrder);
    Q.Descr.AddCondition(fldMenuDeleted, ftSmallint, opNE, 1);

    if MetaData.MetadataDB.FieldExists(tblMenu, fldMenuSubjectID) then
      begin
        Q.Descr.AddCondition(fldMenuSubjectID, ftSmallint, opEQ, UserSession.ID);
        Q.Descr.AddCondition(fldMenuSubjectID, ftSmallint, opIs, Null);
        Q.Descr.AddOperation(opOR);
        Q.Descr.AddOperation(opAnd);
      end;
    Q.Open;
    for R:=0 to Pred(Q.RecordCount) do
      begin
        Q.RecNo:= R;
        MM := TContextMeta.Create(nil);
        MM.Assign(Q);
        if not Assigned(MM.Dataset) then Add(MM) else
        if( not Assigned(SecuritySystem) or SecuritySystem.CheckPolicyDataSet(MM.Dataset.ID, spSelect))
             and ((MM.Dataset.SolutionID <> MetaSolution) or (UserSession.IsAdmin))
          then Add(MM)
          else MM.Free;
      end;
  finally
    Q.Free;
  end;

  MakeTree;
  for i:= Pred(Count) downto 0 do
    begin
      Items[i].Optimize(True);
      if (Items[I].Count=0) then Delete(i);
    end;

  SortByOrder;
end;

function TContextMeta.Optimize(aHideGroup: Boolean = False): Boolean;
var i: Integer;
begin
  Result:= false;

  while (0 < Count) and (Items[0].IsSeparator) do
    begin
      Delete(0);
      Result:= True;
    end;

  while (0 < Count) and (Items[Pred(Count)].IsSeparator) do
    begin
      Delete(Pred(Count));
      Result:= True;
    end;

  for i:= Pred(Count) downto 1 do
    if Items[i-1].IsSeparator and Items[i].IsSeparator then
      begin
        Delete(i);
        Result:= True;
      end;

  for i:= Pred(Count) downto 0 do
    begin
      Items[i].Optimize(aHideGroup);
      if aHideGroup and (Items[I].Count=0) and (Not Assigned(Items[i].FDataSet)) then
        Delete(i);
    end;
end;

procedure TContextMeta.Assign(Source: TObject);
var
  DatasetId, Index: Integer;
  TM: TTableMeta;
  ChildContext: TContextMeta;
  V: Variant;
begin
  inherited Assign(Source);
  if Source is TDeDataSet then
    begin
      FId:= VarToInt(TDeDataSet(Source).ValueByName[fldMenuId]);
      FOwnerID:= TDeDataSet(Source).ValueByName[fldMenuOwner];
      DatasetId:= TDeDataSet(Source).ValueByName[fldMenuDataSet];
      FDataSet:= MetaData.GetTableMeta(DatasetId);
      FName:= TDeDataSet(Source).StringValueByName(fldMenuName);
      FDSView:= TDSViewType(VarToInt(TDeDataSet(Source).ValueByName[fldMenuDataSetView]));
      FDSParams:= VarToStr(TDeDataSet(Source).ValueByName[fldMenuDataSetPars]);

      // TODO: Убрать после устаревания метаструктуры
      FFilter:= TDeDataSet(Source).StringValueByNameDef(fldMenuDataFilter, EmptyStr);

      // Поддержка старой логики, если есть поле fldMenuGroupField, добавляем его в строку
      Index := TDeDataSet(Source).IndexByName(fldMenuGroupField);
      if Index <> -1 then
        begin
          V:= TDeDataSet(Source).ValueByName[fldMenuGroupField];
          if Not VarIsNull(V) and (V<>0) then
            begin
              TM:= MetaData.TablesList.FindByID(DatasetId);
              if Assigned(TM) then
                begin
                  Index:= TM.Fields.IndexByID(V);
                  if Index <> -1 then
                    FDSParams:= 'GroupBy=' + TM.Fields[Index].Original + '; ' + FDSParams;
                end;
            end;
        end;

      Index := TDeDataSet(Source).IndexByName(fldMenuSolution);
      if Index <> -1 then
        begin
          FSolutionID:= TDeDataSet(Source).Value[Index];
        end
      else
        begin
          Index := TDeDataSet(Source).IndexByName(fldMenuSystem);
          if Index <> -1 then
            begin
              if (VarToInt(TDeDataSet(Source).Value[Index]) = 1) then FSolutionID:= MetaSolution
                                                                 else FSolutionID:= DefaultSolution
            end;
        end;

      Index := TDeDataSet(Source).IndexByName(fldMenuDeleted);
      if Index <> -1 then FDeleted:= (VarToInt(TDeDataSet(Source).Value[Index]) = 1)
                     else FDeleted:= False;
      FOrder:= (VarToInt(TDeDataSet(Source).ValueByName[fldMenuOrder]));
      FItemType:= TDeDataSet(Source).ValueByName[fldMenuType];
      FIco:= TDeDataSet(Source).ValueByName[fldMenuIco];

      Index := TDeDataSet(Source).IndexByName(fldMenuSubjectID);
      if Index <> -1 then FSubjectID:= VarToInt(TDeDataSet(Source).Value[Index])
                     else FSubjectID:= unassigned;
    end  else
  if Source is TContextMeta then
    begin
      FId:= TContextMeta(Source).FId;
      FName:= TContextMeta(Source).FName;
      FSolutionID:= TContextMeta(Source).FSolutionID;
      FDataSet:= TContextMeta(Source).FDataSet;
      FDSView:= TContextMeta(Source).FDSView;
      FDSParams:= TContextMeta(Source).FDSParams;
      FFilter:= TContextMeta(Source).FFilter;
      FDeleted:= TContextMeta(Source).FDeleted;
      FOrder:= TContextMeta(Source).FOrder;
      FItemType:= TContextMeta(Source).ItemType;
      FIco:= TContextMeta(Source).FIco;
      FSubjectID:= TContextMeta(Source).FSubjectID;

      FRecordKey:= TContextMeta(Source).FRecordKey;
      FSelectedKeys:= TContextMeta(Source).FSelectedKeys;

      FFilterXML:= TContextMeta(Source).FFilterXML;
      FFilterActive:= TContextMeta(Source).FFilterActive;
      FFilterVisible:= TContextMeta(Source).FFilterVisible;

      FGroups.Clear;
      for Index:=0 to Pred(TContextMeta(Source).Groups.Count) do
        FGroups.Add( TContextMeta(Source).Groups[Index] );

      Clear;
      for Index:=0 to Pred(TContextMeta(Source).Count) do
        begin
          ChildContext:= TContextMeta.Create(self);
          ChildContext.Assign(TContextMeta(Source).Items[Index]);
        end;

      FChanged:= mcInsert;
    end { else
  if Source is TCacheItem then
    begin
      FId:= TCacheItem(Source).ValueByName[fldMenuId];
      FOwnerID:= TCacheItem(Source).ValueByName[fldMenuOwner];
      DatasetId:= TCacheItem(Source).ValueByName[fldMenuDataSet];
      FName:= TCacheItem(Source).NativeValueByName[fldMenuName];
      FDataSet:= MetaData.GetTableMeta(DatasetId);
      FDSView:= TDSViewType(VarToInt(TCacheItem(Source).ValueByName[fldMenuDataSetView]));
      FDSParams:= VarToStr(TCacheItem(Source).ValueByName[fldMenuDataSetPars]);
      if -1 < TCacheItem(Source).Owner.Fields.IndexByName(fldMenuGroupField) then
        begin
          FGroupField:= TCacheItem(Source).ValueByName[fldMenuGroupField];
        end;

      Index:= TCacheItem(Source).Owner.TableMeta.Fields.IndexByName(fldMenuDataFilter);
      if Index <> -1
        then FFilter:= VarToStr(TCacheItem(Source).FieldValue[Index])
        else FFilter:= EmptyStr;

      Index:= TCacheItem(Source).Owner.Fields.IndexByName(fldMenuSolution);
      if Index <> -1 then
        begin
          FSolutionID:= VarToInt(TCacheItem(Source).FieldValue[Index]);
        end
      else
        begin
          Index:= TCacheItem(Source).Owner.Fields.IndexByName(fldMenuSystem);
          if Index <> -1 then
            if VarToInt(TCacheItem(Source).FieldValue[Index]) = 1 then FSolutionID:= MetaSolution
                                                                  else FSolutionID:= DefaultSolution;
        end;

      FDeleted:= (VarToInt(TCacheItem(Source).ValueByName[fldMenuDeleted]) = 1);
      FOrder:= (VarToInt(TCacheItem(Source).ValueByName[fldMenuOrder]));
      FItemType:= TCacheItem(Source).ValueByName[fldMenuType];
      FSubjectID:= TCacheItem(Source).ValueByName[fldMenuSubjectID];
      Index := TCacheItem(Source).Owner.TableMeta.Fields.IndexByName(fldMenuIco);
      if Index >= 0
        then FIco:= TCacheItem(Source).FieldValue[Index]
        else FIco:= Null;
    end;
  {}
end;

Procedure TContextMeta.SaveTo(aCasheItem: TObject);
var
  Fm: TFieldMeta;
  CI, pCI: TCacheItem;
  I: Integer;
  LookupKey: Variant;
begin
  CI := TCacheItem(aCasheItem);
  CI.BeginInit;

  CI.InitFieldValueByNameExternal(fldMenuName, WideStringToUnicode(FName));
  CI.InitFieldValueByNameExternal(fldMenuId, FId);
  CI.InitFieldValueByNameExternal(fldMenuOwner, OwnerID);
  CI.InitFieldValueByNameExternal(fldMenuType, FItemType);
  if Assigned(FDataSet) then CI.InitFieldValueByNameExternal(fldMenuDataSet, FDataSet.FId)
                        else CI.InitFieldValueByNameExternal(fldMenuDataSet, Null);
  CI.InitFieldValueByNameExternal(fldMenuDataSetPars, FDSParams);
  CI.InitFieldValueByNameExternal(fldMenuOrder, FOrder);
  CI.InitFieldValueByNameExternal(fldMenuDataSetView, ord(FDSView));
  CI.InitFieldValueByNameExternal(fldMenuDataFilter, FFilter);

  if CI.Owner.Fields.IndexByName(fldMenuSolution)<> -1 then
    CI.InitFieldValueByNameExternal(fldMenuSolution, SolutionID);

  if CI.Owner.Fields.IndexByName(fldMenuSubjectID)<> -1 then
    CI.InitFieldValueByNameExternal(fldMenuSubjectID, FSubjectID);

  if CI.Owner.Fields.IndexByName(fldMenuSystem)<> -1 then
    if SolutionID = MetaSolution then CI.InitFieldValueByNameExternal(fldMenuSystem, 1)
                                 else CI.InitFieldValueByNameExternal(fldMenuSystem, 0);

  CI.InitFieldValueByNameExternal(fldMenuDeleted, FDeleted);
  CI.InitFieldValueByNameExternal(fldMenuIco, FIco);

  for I := 0 to CI.Owner.Fields.Count - 1 do
  begin
    Fm := CI.Owner.Fields[I];
    if Fm.IsLookup then
    begin
      LookupKey := CI.FieldValue[CI.Owner.GetLookupPairIndex(I)];

      if VarIsNull(LookupKey) or VarIsEmpty(LookupKey) then
        CI.InitFieldValue(I, Null)
      else if Assigned(Fm.Owner.NField) and
        (Fm.Owner.NField = Fm.Owner.KField[0]) then
      begin
        CI.InitFieldValue(I, LookupKey);
      end
      else
      begin
        pCI := TDataCache(Fm.Lookup).FindByID(LookupKey);
        if Assigned(pCI) then
          CI.InitFieldValue(I, pCI.CaptionValue)
        else
          CI.InitFieldValue(I, Null);
      end;
    end;
  end;

  CI.EndInit;
end;

procedure TContextMeta.AddGroupField(aField: TFieldMeta; aParentKey: Variant);
var aItem: TParentFieldValue;
begin
  if Not Assigned(aField) then Exit; // по-хорошему надо защищать при вызове метода, а не в его теле.

  aItem.Field:= aField;
  aItem.Value:= aParentKey;
  FGroups.Add(aItem);
end;

function TContextMeta.AddMenuMeta: TContextMeta;
var
  NodeLevel: Integer;
  DMan: TMenuDataManager;
begin
  NodeLevel := Level;
  if NodeLevel >= 3 then
    Result := TContextMeta(Owner).AddMenuMeta
  else
  begin
    Result := TContextMeta.Create(Self);
    Result.ItemType := StrToInt(MetaData.MetaTables[idxMenu].Fields.FindByName
      (fldMenuType).Value2);
    DMan := TMenuDataManager.Create;
    DMan.SetupNewMeta(Result);
    DMan.Free;
    if NodeLevel = 0 then
      Result.ItemType := StrToInt(MetaData.MetaTables[idxMenu].Fields.FindByName
        (fldMenuType).Value1);
    Result.Changed := mcInsert;
  end;
end;

function TContextMeta.DeleteMenu: boolean;
var
  I: Integer;
  DMan: TMenuDataManager;
begin
  Result := True;
  for I := 0 to Count - 1 do
    Result := Result and Items[I].DeleteMenu;
  if Result then
  begin
    DMan := TMenuDataManager.Create;
    Result := Result and DMan.DeleteItem(Self);
    DMan.Free;
  end;
end;

function CompareOrders(aItem1, aItem2: pointer): Integer;
begin
  if TContextMeta(aItem1).Order < TContextMeta(aItem2).Order then     Result:= -1 else
  if TContextMeta(aItem1).Order > TContextMeta(aItem2).Order then     Result:=  1 else
  if TContextMeta(aItem1).Caption < TContextMeta(aItem2).Caption then Result:= -1 else
  if TContextMeta(aItem1).Caption > TContextMeta(aItem2).Caption then Result:=  1 else
                                                                      Result:= 0;
end;

procedure TContextMeta.SortByOrder;
begin
  Sort(CompareOrders);
end;

procedure TContextMeta.SetChanged(aChanged: TTypeChanged);
var
  I: Integer;
begin
  Changed := aChanged;
  if Changed = mcDelete then
    for I := 0 to Count - 1 do
      Items[I].SetChanged(aChanged);
end;

procedure TContextMeta.SaveChanges;
var
  DMan: TMenuDataManager;
  I: Integer;
begin
  DMan := nil;
  try
    DMan := TMenuDataManager(CreateDataManager(MetaData.MetaTables[idxMenu]));
    case Changed of
      mcDelete:
        if not VarIsClear(ID) and not VarIsNull(ID) then
          DMan.DeleteItem(Self);
      mcInsert:
        DMan.InsertItem(Self);
      mcUpdate:
        if not VarIsClear(ID) and not VarIsNull(ID) then
          DMan.UpdateItem(Self);
    end;
  finally
    DMan.Free;
  end;
  for I := Count - 1 downto 0 do
  begin
    Items[I].SaveChanges;
    if Items[I].Changed = mcDelete then
      Delete(I);
  end;
end;

function TContextMeta.Caption: string;
begin
  if (Length(Name) = 0) and (Assigned(Dataset)) then
    Result := GetTitle(Dataset.Name, ttFirstName)
  else
    Result := GetTitle(Name, ttFirstName);
end;

function TContextMeta.Hint: String;
var i: Integer;
begin
  if (FName = cLineCaption) then Exit(EmptyStr);

  if Assigned(FDataSet) then result:= FDataSet.FName
                        else result:= '_Dv.Noname';

  for i:= Pred(FGroups.Count) downto 0 do
    if i = Pred(FGroups.Count) then Result:= FGroups[i].Field.FName + ' - ' + Result
                               else Result:= FGroups[i].Field.FName + ', ' + Result;
end;

function TContextMeta.IsSeparator: boolean;
begin
  Result := (Name = cLineCaption);
end;

{ TElementMeta }

constructor TElementMeta.Create(aTable: TTableMeta = nil; aOwner: TElementMeta = nil);
begin
  inherited Create(aOwner);
  FTable := aTable;
  FElementType := etNone;
  FFont := TFont.Create;
  FFont.Assign(Application.MainForm.Font);
  FFilterPostfix := TExpressionItem.Create;
  FVisiblePostfix := TExpressionItem.Create;
  FReadOnlyPostfix := TExpressionItem.Create;
  FValuePostfix := TExpressionItem.Create;
  FVisible := True;
  FParameters := TParamList.Create;
end;

constructor TElementMeta.Create(aTable: TTableMeta; aName: String; aField: TFieldMeta; aElementType: TElementType;
  aLink, aEAL, aEAR, aEL, aER, aET, aEB: Integer);
begin
  inherited Create(nil);
  Name := aName;
  FTable := aTable;

  if Assigned(aField) then
    begin
      FFieldID := aField.ID;
      FTable := Field.Owner;
    end
  else
    begin
      FFieldID := null;
    end;
  FElementType := aElementType;
  Link := aLink;
  FFont := TFont.Create;
  FFont.Assign(Application.MainForm.Font);
  EAL := aEAL;
  EAR := aEAR;
  EL := aEL;
  ER := aER;
  ET := aET;
  EH := aEB;
  FFilterPostfix := TExpressionItem.Create;
  FVisiblePostfix := TExpressionItem.Create;
  FReadOnlyPostfix := TExpressionItem.Create;
  FValuePostfix := TExpressionItem.Create;
  FVisible := True;
  Changes := [mcInsert];
  FParameters := TParamList.Create;
end;

destructor TElementMeta.Destroy;
begin
  FFont.Free;
  FFilterPostfix.Free;
  FVisiblePostfix.Free;
  FReadOnlyPostfix.Free;
  FValuePostfix.Free;
  FParameters.Free;
  inherited Destroy;
end;

function TElementMeta.GetItem(const Index: Integer): TElementMeta;
begin
  Result := TElementMeta(inherited Items[Index]);
end;

procedure TElementMeta.PutItem(const Index: Integer; Value: TElementMeta);
begin
  inherited Items[Index] := Value;
end;

function TElementMeta.GetField: TFieldMeta;
begin
  if not Assigned(FTable) then Exit(nil);
  if VarIsEmpty(FFieldID) or VarIsNull(FFieldID) then Exit(nil);
  Result:= FTable.Fields.FindByID(VarToInt(FFieldID));
end;

function TElementMeta.GetLinkField: TFieldMeta;
begin
  if not Assigned(LinkTable) then Exit(nil);
  if VarIsEmpty(FLinkFieldID) or VarIsNull(FLinkFieldID) then Exit(nil);
  Result:= LinkTable.Fields.FindByID(VarToInt(FLinkFieldID));
end;

function TElementMeta.GetFontCode: Integer;
begin
  Result := 0;
  with Font do
  begin
    if fsBold in Style then
      Result := Result or sgBold;
    if fsUnderline in Style then
      Result := Result or sgUnderline;
    if FExtFontSize then
      Result := Result or sgExtSize;
    if SameText(Name, cMonoTypeFontName) then
      Result := Result or sgMonoType;
  end;
end;

procedure TElementMeta.SetFontCode(aFontCode: Integer);
var
  tmpFont: TFont;
  OldExtFontSize: boolean;
begin
  if FontCode <> aFontCode then
  begin
    with Font do
    begin
      Style := [];
      if (aFontCode and sgBold) > 0 then
        Style := Style + [fsBold];
      if (aFontCode and sgUnderline) > 0 then
        Style := Style + [fsUnderline];
      OldExtFontSize := FExtFontSize;
      FExtFontSize := (aFontCode and sgExtSize) > 0;
      if OldExtFontSize <> FExtFontSize then
        if FExtFontSize then
          Size := ((3 * Size) div 2)
        else
        begin
          tmpFont := TFont.Create;
          Size := tmpFont.Size;
          tmpFont.Free;
        end;
      if (aFontCode and sgMonoType) <> 0 then
        Font.Name := cMonoTypeFontName;
    end;
    Changes := Changes + [mcUpdate];
  end;
end;

function TElementMeta.GetCaption: string;
begin
  Result := FCaption;
  if (Length(Result) = 0) and Assigned(Field) then
    Result := Field.Native;
end;

procedure TElementMeta.SetCaption(const aValue: string);
begin
  if FCaption <> aValue then
  begin
    FCaption := aValue;
    Include(FChanges, mcUpdate);
  end;
end;

function TElementMeta.GetElementColor: TColor;
begin
  // TODO: Надо проверить на кривых случаях

  { if Font.Color= clNone
    Result := clBlack
    else { }
  Result := Font.Color;
end;

procedure TElementMeta.SetElementColor(const aColor: TColor);
begin
  if Color <> aColor then
  begin
    if aColor < 0 then
      Font.Color := RGB(0, 0, 0)
    else { clDefault }
      if aColor = 1 then
        Font.Color := RGB(0, 0, 0)
      else { clBlack }
        if aColor = 2 then
          Font.Color := RGB(153, 153, 153)
        else { clGray }
          if aColor = 3 then
            Font.Color := RGB(255, 0, 0)
          else { clRed }
            Font.Color := aColor;
    Include(FChanges, mcUpdate);
  end;
end;

procedure TElementMeta.SetEAL(const aEAL: Integer);
begin
  if FEAL <> aEAL then
  begin
    FEAL := aEAL;
    Include(FChanges, mcUpdate);
  end;
end;

procedure TElementMeta.SetEAR(const aEAR: Integer);
begin
  if FEAR <> aEAR then
  begin
    FEAR := aEAR;
    Include(FChanges, mcUpdate);
  end;
end;

procedure TElementMeta.SetEL(const aEL: Integer);
begin
  if FEL <> aEL then
  begin
    FEL := aEL;
    Include(FChanges, mcUpdate);
  end;
end;

procedure TElementMeta.SetER(const aER: Integer);
begin
  if FER <> aER then
  begin
    FER := aER;
    Include(FChanges, mcUpdate);
  end;
end;

procedure TElementMeta.SetET(const aET: Integer);
begin
  if FET <> aET then
  begin
    FET := aET;
    Include(FChanges, mcUpdate);
  end;
end;

procedure TElementMeta.SetEH(const aEH: Integer);
begin
  if FEH <> aEH then
  begin
    FEH := aEH;
    Include(FChanges, mcUpdate);
  end;
end;

procedure TElementMeta.SetLink(const aLink: Integer);
begin
  if FLink <> aLink then
  begin
    FLink := aLink;
    FLinkTable := MetaData.GetTableMeta(FLink);
    Include(FChanges, mcUpdate);
  end;
end;

procedure TElementMeta.SetElementType(const aElementType: TElementType);
begin
  if FElementType <> aElementType then
  begin
    FElementType := aElementType;
    Include(FChanges, mcUpdate);
  end;
end;

function TElementMeta.GetEH: Integer;
var i: Integer;
begin
  if ElementType in [etTabSheet, etListTabSheet] then Result:= 5
                                                 else Result:= FEH;
  if ElementType in ContainerElements  then
    for i:= 0 to Pred(Count) do
      Result:= Max(Result, Items[i].FET + Items[i].EH + 2);
end;

function TElementMeta.GetWasChanged: boolean;
var
  I: Integer;
begin
  Result := (Changes <> []) and (Changes <> [mcNone]);
  if not Result then
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].WasChanged;
      if Result then
        Break;
    end;
end;

Procedure TElementMeta.GetParentBounds(var aHeight, aWidth: Integer);
var
  I, m: Integer;
begin
  if (ET + EH + 1) * YStep > aHeight then
    aHeight := (ET + EH + 2) * YStep;

  if EAL = EAR then
    m := XStep * (ER - EL)
  else
    m := MarginLeft + MarginRight + (((AnchorsCount - 1) * XStep * (EL + MarginLeft - ER)) div (EAR - EAL));

  if aWidth < m then
    aWidth := m;

  for I := 0 to Count - 1 do
    Items[I].GetParentBounds(aHeight, aWidth);
end;

function TElementMeta.SizableH: Boolean;
begin
  case FElementType of
    etPanel, etBrowser, etExplorer, etCheckListBox, etGrid, etTreeBox, etBarCode, etTree: Result:= True;
    etString: Result:= (FEH > iif(FExtFontSize, 4, 3));
    etDefault:
      begin
        if assigned(Field) and (Field.DataType in StringTypes + [ftBlob])
          then Result:= (FEH > iif(FExtFontSize, 4, 3))
          else Result:= False;
      end;
    else Result:= False;
  end;
end;

procedure TElementMeta.GetBounds(aClientWidth, aClientHeight: Integer; var aL, aT, aW, aH: Integer; aSimple: Boolean = True);
var
  fClientWidth: Integer;
  MaxH, HH, i, j, l, r, N, AStep, ACount : Integer;
  GP: TElementMeta;
  Lines: Array of Integer;
  MAP: TList<Integer>;
begin
  if EAR < EAL then
    EAR := EAL; // тут выполняется Include(FChanges, mcUpdate); и при сохранении будет учтена правка

  fClientWidth:= aClientWidth - (MarginLeft + MarginRight);

  // расчет положения левой и правой границ элемента
  l:= ((fClientWidth * EAL) div (AnchorsCount - 1)) + XStep * EL;
  r:= ((fClientWidth * EAR) div (AnchorsCount - 1)) + XStep * ER;
  // коррекция неправильных данных
  if l < 0 then l:= 0 else
  if l > (fClientWidth - 1) then l := fClientWidth - 1;

  if r < 0 then r:= 0 else
  if r > (fClientWidth) then r := fClientWidth;
  if (r - l + 1) < XStep then r:= l + XStep - 1;

  aL:= l + MarginLeft;
  aW:= r - l - 1;

  if FElementType = etBevel then HH:= 2
                            else HH:= 3;
  if FExtFontSize then Inc(HH);
  if HH < EH      then HH:= EH;

  aT:= YStep * ET;
  aH:= YStep * HH - YSpace;

  if not aSimple then
    begin
      // ищем "папу" внутри которого расстановка
      GP:= self;
      while Assigned(GP.Owner) and not (GP.ElementType in [etNone, etForm, etTabSheet]) do
        GP:= TElementMeta(GP.Owner);

      MaxH:= GP.EH;
      ACount:= 0;
      SetLength(Lines, MaxH + 1);
      for i:=0 to MaxH do Lines[i]:= 0;

      // если растягиваемый элемент Grid единственный у родителя, то расягиваем по правилам etListTabSheet
      if (GP.Count = 1) and (GP[0].FElementType = etGrid) then
        begin
          aL:= MarginGrid;
          aT:= MarginGrid;
          aW:= aClientWidth-2*MarginGrid-1;
          aH:= aClientHeight-2*MarginGrid-1;
          Exit;
        end;

      // отбираем растягиваемые по высоте элементы
      MAP:= TList<Integer>.Create;
      for i:=0 to Pred(GP.Count) do
        if GP[i].SizableH then MAP.Add(i);

      // обработка списка растягиваемых элементов
      for i:=Pred(MAP.Count) downto 0 do
        begin
          N:= -1;
          for j:=Pred(i) downto 0 do
            begin
              if (GP[MAP[i]].ET <= GP[MAP[j]].ET) and (GP[MAP[j]].EB <= GP[MAP[i]].EB) then N:= i else
              if (GP[MAP[j]].ET <= GP[MAP[i]].ET) and (GP[MAP[i]].EB <= GP[MAP[j]].EB) then N:= j;
              if -1 < N then Break;
            end;
          if -1 < N then MAP.Delete(N)  // Элемент больше или равен другому - выбрасываем
                    else begin          // Элемент порождает растягивание сетки для него
                           Inc(ACount);
                           for j:= GP[MAP[i]].EB to MaxH do Lines[j]:= Lines[j] + 1;
                         end;
        end;


      if ACount = 0 then AStep:= 0
                    else AStep:= Max(0, (aClientHeight - 2 * MarginTop - (Pred(MaxH) * YStep)) div ACount);

      aT:= aT + AStep * Lines[ET];
      if SizableH then
        aH:= aH + AStep * (Lines[ET+HH] - Lines[ET]);
    end;
end;

function TElementMeta.SetBounds(TC: TControl; Repaint: Boolean = False): boolean;
var
  l, t, w, h: Integer;
  Simple: Boolean;
begin
  if Not Assigned(TC) then Exit(False);
  Result := True;

  if TC.Owner is TAForm then Simple:= TAForm(TC.Owner).InDesign
                        else Simple:= True;

  if not Simple and assigned(TC.Parent) and assigned(TC.Parent.Parent)
    then GetBounds(TC.Parent.ClientWidth, Min(TC.Parent.ClientHeight, TC.Parent.Parent.Height), l, t, w, h, Simple)
    else GetBounds(TC.Parent.ClientWidth, 0, l, t, w, h, Simple);

  if TC.Parent is TDeTabSheet then
    t := t - TDeTabSheet(TC.Parent).Y;

  if (TC is TBevel) then TC.SetBounds( l, t - 1, w + 1, YStep )
                    else TC.SetBounds( l, t,     w + 1, h);

  if Repaint then TC.Repaint;
end;

procedure TElementMeta.DeleteElement(aMeta: TElementMeta);
var
  MetaIdx: Integer;
begin
  MetaIdx := IndexOf(aMeta);
  if MetaIdx >= 0 then
  begin
    Items[MetaIdx].RecursiveSetChanged(mcDelete);
    Items[MetaIdx].SaveElements;
    Items[MetaIdx].Clear;
    Delete(MetaIdx);
  end;
end;

procedure TElementMeta.SaveElements;
var
  DMan: TElementsDataManager;
  I: Integer;
  mcDelete_Changes: Boolean;
begin
  if not Table.IsDynamic then
  begin
    DMan := nil;
    try
      DMan := TElementsDataManager
        (CreateDataManager(MetaData.MetaTables[idxElement]));
      if (mcInsert in Changes) and (mcDelete in Changes) then
      begin
        // ничего не делаем
      end
      else
      if mcDelete in Changes then
      begin
        if not(VarIsNull(ID) or VarIsClear(ID) or (ID = 0)) then
          DMan.DeleteElement(Self);
      end
      else if mcInsert in Changes then
        DMan.InsertElement(Self)
      else if mcUpdate in Changes then
        if not(VarIsNull(ID) or VarIsClear(ID) or (ID = 0)) then
          DMan.UpdateElement(Self);
    finally
      DMan.Free;
    end;

    for I := Pred(Count) downto 0 do
    begin
      mcDelete_Changes:= mcDelete in Items[I].Changes; //сохраняем в переменную, ибо после SaveElements будет недоступно
      Items[I].SaveElements;
      if mcDelete_Changes then
        Delete(I);
    end;
  end;
end;

function TElementMeta.DesignElement(const aField: TFieldMeta; const aL, aT: Integer): Integer;
begin
  if 0 < aField.Link then
    Result:= Add(TElementMeta.Create(FTable, EmptyStr, aField, etDefault, aField.Link, 0, 12, aL, 0, aT, 3)) else
  if frColor in aField.Role then
    Result:= Add(TElementMeta.Create(FTable, EmptyStr, aField, etColorComboBox, 0, 0, 0, aL, aL + 18, aT, 3)) else
  if frIcon in aField.Role then
    Result:= Add(TElementMeta.Create(FTable, EmptyStr, aField, etIconBox, 0, 0, 0, aL, aL + 18, aT, 5)) else

  if (aField.DataType in LogicalTypes) or (aField.ShowType = stYesNo) then
    Result:= Add(TElementMeta.Create(FTable, EmptyStr, aField, etCheckBox, 0, 0, 0, aL, aL + 2, aT, 3)) else
  if aField.DataType in NumericTypes then
    Result:= Add(TElementMeta.Create(FTable, EmptyStr, aField, etDefault, 0, 0, 0, aL, aL + 18, aT, 3)) else
  if aField.DataType in DateTimeTypes then
    Result:= Add(TElementMeta.Create(FTable, EmptyStr, aField, etDefault, 0, 0, 0, aL, aL + 18, aT, 3)) else
  if (aField.DataType in [ftMemo, ftWideMemo])  then
    Result:= Add(TElementMeta.Create(FTable, EmptyStr, aField, etDefault, 0, 0, 12, aL, 0, aT, 5)) else
  if (aField.DataType in StringTypes) and (aField.FDataSize <= 16) then
    Result:= Add(TElementMeta.Create(FTable, EmptyStr, aField, etDefault, 0, 0, 0, aL, aL + 18, aT, 3)) else
  if (aField.DataType in StringTypes) and (1024 < aField.FDataSize) then
    Result:= Add(TElementMeta.Create(FTable, EmptyStr, aField, etDefault, 0, 0, 12, aL, 0, aT, 5)) else
  if (aField.DataType in BinaryTypes) then
    Result:= Add(TElementMeta.Create(FTable, EmptyStr, aField, etDefault, 0, 0, 0, aL, aL + 18, aT, 5)) else

    Result:= Add(TElementMeta.Create(FTable, EmptyStr, aField, etDefault, 0, 0, 12, aL, 0, aT, 3));
end;

procedure TElementMeta.Assign(Source: TObject);
var
  I: Integer;
  V: Variant;
  OwnerTable: TTableMeta;
begin
  inherited Assign(Source);
  if Source is TDeDataSet then
    begin
      FId := VarToInt(TDeDataSet(Source).ValueByName[fldElemId]);
      FElementType := TElementType(VarToInt(TDeDataSet(Source).ValueByName[fldElemType]));
      FLink := VarToInt(TDeDataSet(Source).ValueByName[fldElemLink]);
      FLinkTable := MetaData.GetTableMeta(FLink);
      FName := TDeDataSet(Source).StringValueByName(fldElemName);
      if Assigned(Table) then FFieldID := VarToInt(TDeDataSet(Source).ValueByName[fldElemField])
                         else FFieldID := null;
      FontCode := VarToInt(TDeDataSet(Source).ValueByName[fldElemFont]);
      FEAL := VarToInt(TDeDataSet(Source).ValueByName[fldElemAL]);
      FEAR := VarToInt(TDeDataSet(Source).ValueByName[fldElemAR]);
      FEL := VarToInt(TDeDataSet(Source).ValueByName[fldElemL]);
      FER := VarToInt(TDeDataSet(Source).ValueByName[fldElemR]);
      FET := VarToInt(TDeDataSet(Source).ValueByName[fldElemT]);
      FEH := VarToInt(TDeDataSet(Source).ValueByName[fldElemH]);
      Color := VarToInt(TDeDataSet(Source).ValueByName[fldElemColor]);
      OwnerID := TDeDataSet(Source).ValueByName[fldElemOwner];
      FDeleted := (VarToInt(TDeDataSet(Source).ValueByNameDef(fldElemDeleted, 0)) = 1);
      if Assigned(FLinkTable) then FLinkFieldID := VarToInt(TDeDataSet(Source).ValueByName[fldElemLinkField])
                              else FLinkFieldID := null;
      FNameInExpr := VarToStr(TDeDataSet(Source).ValueByName[fldElemExprName]);
      FExprFilter := VarToStr(TDeDataSet(Source).ValueByName[fldElemExprFilter]);
      FExprVisible := VarToStr(TDeDataSet(Source).ValueByName[fldElemExprVisible]);
      FExprReadOnly := VarToStr(TDeDataSet(Source).ValueByName[fldElemExprReadOnly]);
      FExprValue := VarToStr(TDeDataSet(Source).ValueByName[fldElemExprValue]);
      FVisible := (VarToInt(TDeDataSet(Source).ValueByName[fldElemVisible]) = 1);
      FReadOnly :=(VarToInt(TDeDataSet(Source).ValueByName[fldElemReadOnly]) = 1);
      if Changes = [mcUpdate] then
        Changes := [mcNone];
    end else

  if Source is TCacheItem then
    with TCacheItem(Source), TCacheItem(Source).Owner do
    begin
      V:= FieldValue[Fields.IndexByName(fldElemId)];
      if not VarIsNull(V) and not VarIsEmpty(V) then Self.FId := VarToInt(V);

      Self.FElementType := TElementType(VarToInt(FieldValue[Fields.IndexByName(fldElemType)]));
      Self.FLink := VarToInt(FieldValue[Fields.IndexByName(fldElemLink)]);
      Self.FLinkTable := MetaData.GetTableMeta(Self.FLink);
      Self.FName := TCacheItem(Source).ValueNativeByName[fldElemName];
      if Assigned(Table) then Self.FFieldID := VarToInt(FieldValue[Fields.IndexByName(fldElemField)])
                         else Self.FFieldID := null;
      Self.FontCode := VarToInt(FieldValue[Fields.IndexByName(fldElemFont)]);
      Self.FEAL := VarToInt(FieldValue[Fields.IndexByName(fldElemAL)]);
      Self.FEAR := VarToInt(FieldValue[Fields.IndexByName(fldElemAR)]);
      Self.FEL := VarToInt(FieldValue[Fields.IndexByName(fldElemL)]);
      Self.FER := VarToInt(FieldValue[Fields.IndexByName(fldElemR)]);
      Self.FET := VarToInt(FieldValue[Fields.IndexByName(fldElemT)]);
      Self.FEH := VarToInt(FieldValue[Fields.IndexByName(fldElemH)]);
      Self.OwnerID := VarToInt(ValueByName[fldElemOwner]);
      Self.Color := VarToInt(FieldValue[Fields.IndexByName(fldElemColor)]);
      Self.FDeleted := (VarToInt(ValueByName[fldElemDeleted]) = 1);
      if Assigned(FLinkTable) then Self.FLinkFieldID := VarToInt(ValueByName[fldElemLinkField])
                              else Self.FLinkFieldID := null;
      Self.FNameInExpr := VarToStr(ValueByName[fldElemExprName]);
      Self.FExprFilter := VarToStr(ValueByName[fldElemExprFilter]);
      Self.FExprVisible := VarToStr(ValueByName[fldElemExprVisible]);
      Self.FExprReadOnly := VarToStr(ValueByName[fldElemExprReadOnly]);
      Self.FExprValue := VarToStr(ValueByName[fldElemExprValue]);
      Self.FVisible := (VarToInt(ValueByName[fldElemVisible]) = 1);
      Self.FReadOnly := (VarToInt(ValueByName[fldElemReadOnly]) = 1);
      if Changes = [mcUpdate] then
        Changes := [mcNone];
    end else
  if Source is TElementMeta then
    begin
      ID := TElementMeta(Source).ID;
      ElementType := TElementMeta(Source).ElementType;
      Link := TElementMeta(Source).Link;
      FLinkTable := TElementMeta(Source).LinkTable;
      FName := TElementMeta(Source).FName;
      FTable := TElementMeta(Source).FTable;
      FontCode := TElementMeta(Source).FontCode;
      EAL := TElementMeta(Source).EAL;
      EAR := TElementMeta(Source).EAR;
      EL := TElementMeta(Source).EL;
      ER := TElementMeta(Source).ER;
      ET := TElementMeta(Source).ET;
      FEH := TElementMeta(Source).FEH;
      OwnerID := TElementMeta(Source).OwnerID;
      Color := TElementMeta(Source).Color;
      FLinkFieldID := TElementMeta(Source).FLinkFieldID;
      FDeleted := TElementMeta(Source).FDeleted;
      Changes := TElementMeta(Source).Changes;
      FFieldID := TElementMeta(Source).FFieldID;
      FNameInExpr := TElementMeta(Source).FNameInExpr;
      FExprFilter := TElementMeta(Source).FExprFilter;
      FExprVisible := TElementMeta(Source).FExprVisible;
      FExprReadOnly := TElementMeta(Source).FExprReadOnly;
      FExprValue := TElementMeta(Source).FExprValue;
      FVisible := TElementMeta(Source).FVisible;
      FReadOnly := TElementMeta(Source).FReadOnly;
    end;

  { если поле связи отсутствует, то ищем его в соответствующей таблице }
  if Assigned(LinkTable) and (not Assigned(LinkField)) and (ElementType = etGrid) then
  begin
    for I := 0 to LinkTable.Fields.Count - 1 do
      if LinkTable.Fields[I].Link > 0 then
      begin
        OwnerTable := MetaData.GetTableMeta(LinkTable.Fields[I].Link);
        while Assigned(OwnerTable) and (OwnerTable <> OwnerTable.OwnerTable) and
          (OwnerTable.ID <> Table.ID) and (OwnerTable.ID <> Table.OwnerTable.ID) do
          OwnerTable := OwnerTable.GrandOwnerTable;
        if Assigned(OwnerTable) and
          ((OwnerTable.ID = Table.ID) or (LinkTable.Fields[I].Link = Table.OwnerTable.ID)) then
        begin
          FLinkFieldID := LinkTable.Fields[I].ID;
          Break;
        end;
      end;
    if Assigned(LinkField) and LinkField.IsLookup then
      FLinkFieldID := LinkField.LookupPair.ID;
  end;

  // в старым метаструктурах у корневой формы родитель был = 0, теперь = null. Сделано для совместимости
  // корректируем и помечаем элемент "отредактировано", чтобы сохранилось если форму будут редактировать
  if (FOwnerID = 0) and (FElementType=etForm) then
    begin
      OwnerID:= null;
      Changes:= Changes + [mcUpdate];
    end;
end;

procedure TElementMeta.RecursiveSetChanged(aChanged: TTypeChanged);
var
  I: Integer;
begin
  Changes := Changes + [aChanged];
  for I := 0 to Count - 1 do
    Items[I].RecursiveSetChanged(aChanged);
end;

function TElementMeta.FindByExprName(const aExprName: string;
  StrongCompare: boolean = True): TElementMeta;
var
  I, P: Integer;
begin
  P := Pos('.', aExprName);

  if CompareText(NameInExpr, aExprName) = 0 then
    Result := Self
  else if (not StrongCompare) and (1 < P) and
    (CompareText(NameInExpr, Copy(aExprName, 1, P - 1)) = 0) then
    Result := Self
  else
  begin
    Result := nil;
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].FindByExprName(aExprName, StrongCompare);
      if Assigned(Result) then
        Break;
    end;
  end;
end;

function TElementMeta.FindByFieldAndType(const aField: TFieldMeta; TypeList: TElementTypes = []): TElementMeta;
var i: Integer;
begin
  result:= nil;
  for i:=0 to pred(Count) do
    begin
      if (Items[i].Field = aField) and ((TypeList=[]) or (Items[i].ElementType in TypeList)) then Exit(Items[i]);
      if Items[i].ElementType in ContainerElements then
        begin
          Result:= Items[i].FindByFieldAndType(aField, TypeList);
          if assigned(Result) then Exit;
        end;
    end;
end;

function TElementMeta.FindDatasetPage(const aID: Integer; aLinkField: TFieldMeta): TElementMeta;
var
  Queue: TQueue;
  Elem: TElementMeta;
  I: Integer;
begin
  Result := nil;
  { ищем etGrid, либо etListTabSheet с нужным набором данных }
  Queue := TQueue.Create;
  Queue.Push(Self);
  while Queue.Count > 0 do
    begin
      Elem := TElementMeta(Queue.Pop);
      if (Elem.ElementType in [etGrid, etListTabSheet]) and (Elem.Link = aID) and
         ((Elem.LinkField = aLinkField) or (Assigned(Elem.LinkField) and
           Assigned(aLinkField) and (Elem.LinkField.ID = aLinkField.ID))) then
        begin
          Result := Elem;
          Break;
        end
      else
        for I := 0 to Elem.Count - 1 do
          Queue.Push(Elem[I]);
    end;
  Queue.Free;
  { если нашли элемент, то определяем его страницу }
  while Assigned(Result) and (TElementMeta(Result.Owner).ElementType = etTabSheet) do
    Result := TElementMeta(Result.Owner);
end;

function TElementMeta.EB: Integer;
begin
  Result:= FET + FEH;
end;

function TElementMeta.ElementIsPageControl(EM: TElementMeta): boolean;
begin
  Result := (EM.ElementType = etTabSheet) and (EM.Count > 0) and (EM[0].ElementType = etTabSheet);
end;

function TElementMeta.CreateBlankPage: TElementMeta;
begin
  Result := TElementMeta.Create(Table, '_Dl.Properties', nil, etTabSheet, 0, 0, 0, 0, 10, Count + 1, 3);
  Add(Result);
end;

function TElementMeta.CreateDatasetPage(const aID: Integer; aLinkField: TFieldMeta): TElementMeta;
var
  ListTable: TTableMeta;
begin
  Result := nil;
  ListTable := MetaData.GetTableMeta(aID);
  if Assigned(ListTable) then
  begin
    Result := TElementMeta.Create(Table, ListTable.Name, nil, etListTabSheet, aID, 0, 0, 0, 10, Count + 1, 3);
    Result.FLinkFieldID := aLinkField.ID;
    Add(Result);
  end;
end;

function TElementMeta.GetDatasetPage(const aID: Integer; aLinkField: TFieldMeta; var aCreated: boolean): TElementMeta;
begin
  aCreated := False;
  Result := FindDatasetPage(aID, aLinkField);
  if not Assigned(Result) then
  begin
    Result := CreateDatasetPage(aID, aLinkField);
    aCreated := True;
  end;
end;

procedure TElementMeta.RemovePage(const aPage: TElementMeta);
begin
  if Assigned(aPage) and (aPage.ElementType = etListTabSheet) then
    aPage.Free;
end;

procedure TElementMeta.CorrectInnerElements(aOwner: TElementMeta);
var
  I: Integer;
  IncCounter: boolean;
  ChildEM: TElementMeta;
begin
  I := 0;
  while I < aOwner.Count do
  begin
    IncCounter := True;
    ChildEM := aOwner.Items[I];
    if mcDelete in ChildEM.Changes then
      Continue;
    if ChildEM.ElementType = etTabSheet then
    begin
      IncCounter := False;
      if ElementIsPageControl(ChildEM) then
        ChildEM.Changes := ChildEM.Changes + [mcDelete];
      aOwner.Extract(ChildEM);
      Add(ChildEM);
      ChildEM.Changes := ChildEM.Changes + [mcUpdate];
    end;
    if (ChildEM.Count > 0) then
      CorrectInnerElements(ChildEM);
    if IncCounter then
      Inc(I);
  end;
end;

procedure TElementMeta.CorrectElements;
var
  I: Integer;
  NewEM, ChildEM: TElementMeta;
  TabSheetCreated, IncCounter: boolean;
begin
  TabSheetCreated := False;
  I := 0;
  NewEM := nil;
  while (TabSheetCreated and (I < (Count - 1))) or
    ((not TabSheetCreated) and (I < Count)) do
  begin
    IncCounter := True;
    ChildEM := Items[I];
    if mcDelete in ChildEM.Changes then
      Continue;
    if not(ChildEM.ElementType in [etTabSheet, etListTabSheet]) then
    begin
      IncCounter := False;
      if not TabSheetCreated then
      begin
        NewEM := TElementMeta.Create(Table, '_Dl.Properties', nil, etTabSheet, 0, 0, 0, 0, 10, 1, 3);
        NewEM.Changes := NewEM.Changes + [mcInsert];
        Add(NewEM);
        TabSheetCreated := True;
      end;
      Extract(ChildEM);
      NewEM.Add(ChildEM);
      ChildEM.Changes := ChildEM.Changes + [mcUpdate];
    end
    else if ElementIsPageControl(ChildEM) then
      ChildEM.Changes := ChildEM.Changes + [mcDelete];
    if (ChildEM.Count > 0) then
      CorrectInnerElements(ChildEM);
    if IncCounter then
      Inc(I);
  end;
end;

{ TDeConstraint }

constructor TDeConstraint.Create(aTable: TTableMeta);
begin
  inherited Create;
  FTable := aTable;
  FExprPostfix := TExpressionItem.Create;
end;

function TDeConstraint.GetContextAsInteger: Integer;
begin
  Result := 0;
  if ccInsert in Context then
    Result := Result or ctcInsert;
  if ccUpdate in Context then
    Result := Result or ctcUpdate;
  if ccDelete in Context then
    Result := Result or ctcDelete;
  if ccBasket in Context then
    Result := Result or ctcBasket;
end;

procedure TDeConstraint.SetContextAsInteger(const aIntContext: Integer);
begin
  FContext := [];
  if (aIntContext and ctcInsert) <> 0 then
    Include(FContext, ccInsert);
  if (aIntContext and ctcUpdate) <> 0 then
    Include(FContext, ccUpdate);
  if (aIntContext and ctcDelete) <> 0 then
    Include(FContext, ccDelete);
  if (aIntContext and ctcBasket) <> 0 then
    Include(FContext, ccBasket);
end;

procedure TDeConstraint.Assign(Source: TObject);
begin
  if Source is TDeDataSet then
    with TDeDataSet(Source) do
    begin
      FExprPostfix.Clear;
      if mpDatabaseNewConstraints in MetaData.MetadataPresents then
        begin
          FId := VarToInt(ValueByName[fldConstraintsID]);
          ContextAsInteger := VarToInt(ValueByName[fldConstraintsCases]);
          FEvent := TConditionEvent(VarToInt(TDeDataSet(Source).ValueByName[fldConstraintsAction]));
          FDeleted := (ValueByNameDef(fldConstraintsDeleted, 0) = 1);
          FActive := (VarToInt(ValueByName[fldConstraintsState]) <> 1);
          Expression := VarToStr(ValueByName[fldConstraintsExpression]);
          FStrMessage := StringValueByName(fldConstraintsMessage);
          FErrorField := FTable.Fields.FindByID(VarToInt(ValueByName[fldConstraintsField]));
        end
      else
        begin
          FId := VarToInt(ValueByName[fldID]);
          ContextAsInteger := VarToInt(ValueByName[fldCSCases]);
          FEvent := TConditionEvent(VarToInt(TDeDataSet(Source).ValueByName[fldCSAction]));
          FDeleted := (ValueByNameDef(fldDeleted, 0) = 1);
          FActive := (VarToInt(ValueByName[fldCSOffState]) <> 1);
          Expression := VarToStr(ValueByName[fldCSExpression]);
          FStrMessage := StringValueByName(fldCSMessage);
          FErrorField := FTable.Fields.FindByID(VarToInt(ValueByName[fldCSErrorField]));
        end;
    end;
end;

destructor TDeConstraint.Destroy;
begin
  FreeAndNil(FExprPostfix);
  inherited;
end;

procedure TDeConstraint.SetExpression(const aExpression: string);
var
  Parser: TDeParser;
begin
  if Length(aExpression) > 0 then
  begin
    Parser := TDeParser.Create;
    Parser.Table := Table;
    try
      Parser.Parse(aExpression, FExprPostfix);
    except
      on E: EDeParserError do
      begin
{$IFDEF DEBUG}
        if Assigned(Table) and (Length(Table.Table) <> 0) then
          DebugLog(ClassName + '.SetExpression(%s) skip error for %s: %s', [QuotedStr(aExpression), Table.Table, E.Message])
        else
          DebugLog(ClassName + '.SetExpression(%s) skip error: %s', [QuotedStr(aExpression), E.Message]);
{$ENDIF}
        FExprPostfix.Clear;
      end;
    end;
    Parser.Free;
  end
  else
    FExprPostfix.Clear;
end;

function TDeConstraint.CheckPolicy(const Operation: TSecurityOperation)
  : boolean;
begin
  Result := SecuritySystem.CheckPolicyConstraint(ID, Operation);
end;

{ TTableMeta }

constructor TTableMeta.Create;
begin
  inherited Create;
  FLoadingState:= FLoadingState;
  FObjectType:= otUnknown;
  FChildrenTables:= TTablesList.Create(False);
  FElements:= TElementsManager.Create;
  FLinks:= TFieldsMeta.Create;
  FFields:= TFieldsMeta.Create;
  FKField := TFieldsMeta.Create(False);

  FUserValues:= TUserValues.Create;
  FFilterPostfix:= TFilterItem.Create;
  FFiltersManager:= TFiltersManager.Create(Self);
  FConstraintList:= TObjectList<TDeConstraint>.Create;
  // FArchivedFilter:= TFilterItem.Create;
  FParameters:= TVariableList.Create;
  FMetaTableIndex:= -1;
  FVariantSolutionGUID:= Unassigned; // Пока не определено!!!
end;

constructor TTableMeta.Create(const aTable: string; ADatabase: TDeCustomDatabase; const ALoadingState: TTableFlagState = []);
begin
  Create;
  // Установка свойств только для динамических таблиц ...
  FDatabase := ADatabase;
  FIco := 0;
  FName := '_dF.table ' + aTable;
  FTable := aTable;
  FReadOnly := True;
  FGroupViewType := vtList;
end;

destructor TTableMeta.Destroy;
begin
  UndefTableLinks;
  FKField.Free;
  FElements.Free;
  FFiltersManager.RemoveFilter(FFilterPostfix);
  if Assigned(GroupFilters) then
    GroupFilters.UnloadFilters(Self);
  // FFiltersManager.RemoveFilter(FArchivedFilter);
  if (not Assigned(OwnerTable)) or (OwnerTable = Self) then
    FFields.Free;
  FLinks.Free;
  FUserValues.Free;
  FChildrenTables.Free;
  if Assigned(FActionConditionList) then
    FActionConditionList.Free;
  FFilterPostfix.Free;
  FFiltersManager.Free;
  FConstraintList.Free;
  // FArchivedFilter.Free;
  FParameters.Free;
  FreeAndNil(FLinkFilters);
  inherited Destroy;
end;

function TTableMeta.Save: boolean;
var
  DMan: TDatasetDataManager;
begin
  DMan := nil;
  try
    DMan := TDatasetDataManager.Create;
    Result := DMan.UpdateTableMeta(Self);
    if not Result then
      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(DMan.Errors.GetMessage)))
  finally
    if Assigned(DMan) then
      DMan.Free;
  end;
end;

function TTableMeta.AddCalcField(const aName: String; aDefaultValue: string; aType: TFieldType; aSize, aScale: Integer): TFieldMeta;
var Parser: TDeParser;
    N: Integer;
begin
  N:= Fields.IndexByName(aName);
  if -1 < N then Exit(Fields[N]);

  Result:= TFieldMeta.Create;
  Result.Original:= aName;
  Result.IsStored:= False;
  Result.Calculated:= True;
  Result.DefaultValue:= aDefaultValue;
  Result.CodePage:= 0;
  Result.Owner:= self;

  N:= Fields.IndexByName(aDefaultValue);
  if N = -1 then
    begin
      Result.DataType:= aType;
      Result.DataSize:= aSize;
      Result.Precision:= aScale;
    end
  else
    begin // копия поля с новым именем, для перехода на новую метаструктуру
      Result.DataType:= Fields[N].DataType;
      Result.DataSize:= Fields[N].DataSize;
      Result.Precision:= Fields[N].Precision;
    end;

  Parser := TDeParser.Create;
  Parser.Table := Self;
  try
    Parser.Parse(Result.DefaultValue, Result.DefaultPostfix);
    Fields.Add(Result);
  except
    Result.Free;
    Result:= nil;
  end;
  Parser.Free;
end;

procedure TTableMeta.Assign(Source: TObject);
begin
  if Source is TDeCustomDataBase then
  begin
    FDataBase:= TDeCustomDataBase(Source);
  end else

  if Source is TTableMeta then
  begin
    FLoadingState := FLoadingState + [tsLoading];
    FId:= TTableMeta(Source).FId;
    FIco:= TTableMeta(Source).FIco;
    FName:= TTableMeta(Source).FName;
    FTable:= TTableMeta(Source).FTable;
    FIsInterrelations:= TTableMeta(Source).FIsInterrelations;
    FIsNotes:= TTableMeta(Source).FIsNotes;
    FIsShowRight:= TTableMeta(Source).FIsShowRight;
    FSelectSQL:= TTableMeta(Source).FSelectSQL;
    FUpdateSQL:= TTableMeta(Source).FUpdateSQL;
    FInsertSQL:= TTableMeta(Source).FInsertSQL;
    FDeleteSQL:= TTableMeta(Source).FDeleteSQL;
    FDatabase:= TTableMeta(Source).FDatabase;
    FReadOnly:= TTableMeta(Source).FReadOnly;
    FIsDirectory:= TTableMeta(Source).FIsDirectory;
    FDeleted:= TTableMeta(Source).FDeleted;
    FParentTableID:= TTableMeta(Source).FParentTableID;
    FGroupViewType:= TTableMeta(Source).FGroupViewType;
    FGroupViewParams:= TTableMeta(Source).FGroupViewParams;
    FSolutionID:= TTableMeta(Source).FSolutionID;
    FOnAddMenu:= TTableMeta(Source).FOnAddMenu;
    FFilter:= TTableMeta(Source).FFilter;
    FDescription:= TTableMeta(Source).FDescription;
    FSchema := TTableMeta(Source).FSchema;
    if tsGUID in TTableMeta(Source).LoadingState then
    begin
      FGUID := TTableMeta(Source).FGUID;
      Include(FLoadingState, tsGUID);
    end;
    FLoadingState := FLoadingState - [tsLoading];
  end
  else if Source is TDeDataSet then
  begin
    FLoadingState:= FLoadingState + [tsLoading];
    FId:= VarToInt(TDeDataSet(Source).ValueByName[fldDataSetId]);
    FIco:= VarToInt(TDeDataSet(Source).ValueByName[fldDataSetICO]);
    FName:= TDeDataSet(Source).StringValueByName(fldDataSetName);
    FTable:= TDeDataSet(Source).StringValueByName(fldDataSetTable);
    FIsInterrelations:= CompareText(FTable, tblInterrelations) = 0;
    FIsNotes:= CompareText(FTable, tblNotes) = 0;
    FSelectSQL:= Trim(VarToStr(TDeDataSet(Source).ValueByName[fldDataSetSelect]));
    FUpdateSQL:= Trim(VarToStr(TDeDataSet(Source).ValueByName[fldDataSetUpdate]));
    FInsertSQL:= Trim(VarToStr(TDeDataSet(Source).ValueByName[fldDataSetInsert]));
    FDeleteSQL:= Trim(VarToStr(TDeDataSet(Source).ValueByName[fldDataSetDelete]));
    FDatabase:= MetaData.DatabaseByID(TDeDataSet(Source).ValueByName[fldDataSetDatabase]);
    if not assigned(FDatabase) then
      if Copy(FTable,1,3)='MD_'  then
        FDatabase:= metadata.CatalogsDB;

    FReadOnly:= StrToIntDef(VarToStr(TDeDataSet(Source).ValueByName[fldDataSetReadOnly]), 0) = 1;
    FIsDirectory:= TIsDirectory(VarToInt(TDeDataSet(Source).ValueByName[fldDataSetIsList]));
    FDeleted:= (TDeDataSet(Source).ValueByNameDef(fldDataSetDeleted, 0) = 1);
    FParentTableID:= VarToInt(TDeDataSet(Source).ValueByName[fldDatasetParent]);
    FGroupViewType:= TDSViewType(VarToInt(TDeDataSet(Source).ValueByName[fldDataSetGViewType]));
    FGroupViewParams:= VarToStr(TDeDataSet(Source).ValueByName[fldDataSetGViewPars]);
    FSolutionID := VarToInt(TDeDataSet(Source).ValueByName[fldDataSetSolution]);
    FDescription:= TDeDataSet(Source).StringValueByNameDef(fldDataSetDescription, EmptyStr);
    if (TDeDataSet(Source).IndexByName(fldDataSetGUID) <> -1) and
      not VarIsNull(TDeDataSet(Source).ValueByName[fldDataSetGUID]) then
      try
        FGUID:= StringToGUID(VarToStr(TDeDataSet(Source).ValueByName[fldDataSetGUID]));
        Include(FLoadingState, tsGUID);
      except
{$IFDEF DEBUG}
        on E: Exception do
          if Length(FTable) = 0 then
            DebugLog(ClassName + '.Assign skip read GUID error: ' + E.Message)
          else
            DebugLog(ClassName + '.Assign skip read GUID error for ' + QuotedStr(FTable) + ': ' + E.Message);
{$ENDIF}
      end;
    FIsShowRight:= IsSecurityProtected;
    try
      FOnAddMenu:= boolean(VarToInt(TDeDataSet(Source).ValueByName[fldDataSetOnAddMenue]));
    except
      on E: Exception do
      begin
{$IFDEF DEBUG}
        DebugLog(ClassName + '.Assign skip error: ' + E.Message);
{$ENDIF}
        FOnAddMenu := False;
      end;
    end;
    FFilter := TDeDataSet(Source).StringValueByName(fldDataSetFilter);
    FSchema := TDeDataSet(Source).StringValueByNameDef(fldDataSetSchema, EmptyStr);
    FLoadingState := FLoadingState - [tsLoading];
  end
  else if Source is TCacheItem then
  begin
    FLoadingState := FLoadingState + [tsLoading];
    FId := VarToInt(TCacheItem(Source).ValueByName[fldDataSetId]);
    FIco := VarToInt(TCacheItem(Source).ValueByName[fldDataSetICO]);
    FName:= TCacheItem(Source).ValueNativeByName[fldDataSetName];
    FTable:= TCacheItem(Source).ValueNativeByName[fldDataSetTable];
    FIsInterrelations:= CompareText(FTable, tblInterrelations) = 0;
    FIsNotes:= CompareText(FTable, tblNotes) = 0;
    FIsShowRight:= IsSecurityProtected;
    FSelectSQL:= Trim(VarToStr(TCacheItem(Source).ValueByName[fldDataSetSelect]));
    FUpdateSQL:= Trim(VarToStr(TCacheItem(Source).ValueByName[fldDataSetUpdate]));
    FInsertSQL:= Trim(VarToStr(TCacheItem(Source).ValueByName[fldDataSetInsert]));
    FDeleteSQL:= Trim(VarToStr(TCacheItem(Source).ValueByName[fldDataSetDelete]));
    FDatabase:= MetaData.DatabaseByID(StrToIntDef(VarToStr(TCacheItem(Source).ValueByName[fldDataSetDatabase]), 0));
    FReadOnly:= StrToIntDef(VarToStr(TCacheItem(Source).ValueByName[fldDataSetReadOnly]), 0) <> 0;
    FIsDirectory:= TIsDirectory(VarToInt(TCacheItem(Source).ValueByName[fldDataSetIsList]));
    FDeleted:= VarToInt(TCacheItem(Source).ValueByName[fldDataSetDeleted]) <> 0;
    FParentTableID:= VarToInt(TCacheItem(Source).ValueByName[fldDatasetParent]);
    FGroupViewType:= TDSViewType(VarToInt(TCacheItem(Source).ValueByName[fldDataSetGViewType]));
    FGroupViewParams:= VarToStr(TCacheItem(Source).ValueByName[fldDataSetGViewPars]);
    FSolutionID:= VarToInt(TCacheItem(Source).ValueByName[fldDataSetSolution]);
    if TCacheItem(Source).Owner.Fields.IndexByName(fldDataSetDescription) <> -1
      then FDescription:= VarToStr(TCacheItem(Source).ValueNativeByName[fldDataSetDescription])
      else FDescription:= EmptyStr;
    try
      FOnAddMenu:= boolean(VarToInt(TCacheItem(Source).ValueByName[fldDataSetOnAddMenue]));
    except
      on E: Exception do
      begin
{$IFDEF DEBUG}
        DebugLog(ClassName + '.Assign skip error: ' + E.Message);
{$ENDIF}
        FOnAddMenu := False;
      end;
    end;
    FFilter:= VarToStr(TCacheItem(Source).ValueByName[fldDataSetFilter]);

    if TCacheItem(Source).Owner.Fields.IndexByName(fldDataSetSchema) <> -1
      then FSchema := VarToStr(TCacheItem(Source).ValueByName[fldDataSetSchema])
      else FSchema := EmptyStr;

    FLoadingState:= FLoadingState - [tsLoading];
  end;
end;

procedure TTableMeta.AssignToCacheItem(aItem: TObject);
var
  FieldIndex: Integer;
  CI: TCacheItem;
begin
  CI := TCacheItem(aItem);

  // заполнение полей записи полями TTableMeta
  CI.ValueByName[fldDataSetId] := FId;
  CI.ValueByName[fldDataSetICO] := FIco;
  CI.ValueNativeByName[fldDataSetName] := FName;
  CI.ValueNativeByName[fldDataSetTable] := FTable;
  CI.ValueNativeByName[fldDataSetSelect] := FSelectSQL;
  CI.ValueNativeByName[fldDataSetInsert] := FInsertSQL;
  CI.ValueNativeByName[fldDataSetUpdate] := FUpdateSQL;
  CI.ValueNativeByName[fldDataSetDelete] := FDeleteSQL;
  CI.ValueByName[fldDataSetReadOnly] := BooleanToInt[FReadOnly];
  CI.ValueByName[fldDataSetIsList] := Integer(FIsDirectory);
  CI.ValueByName[fldDataSetDeleted] := BooleanToInt[FDeleted];

  if Assigned(FDatabase) then
    begin
      CI.ValueByName[fldDataSetDatabase] := FDatabase.ID;
      if Assigned(Metadata) and (mpDatabaseSchema in Metadata.MetadataPresents) then
        begin
          FieldIndex := CI.Owner.Fields.IndexByName(fldDataSetSchema);
          if FieldIndex <> -1 then
            CI.FieldValue[FieldIndex] := FSchema;
        end;
    end
  else
    begin
      CI.ValueByName[fldDataSetDatabase] := Unassigned;
      FieldIndex := CI.Owner.Fields.IndexByName(fldDataSetSchema);
      if FieldIndex <> -1 then
        CI.FieldValue[FieldIndex] := Unassigned;
    end;

  CI.ValueByName[fldDatasetParent] := FParentTableID;
  CI.ValueByName[fldDataSetSolution] := FSolutionID;
  CI.ValueNativeByName[fldDataSetFilter] := FFilter;

  CI.ValueByName[fldDataSetGViewType] := Ord(FGroupViewType);
  CI.ValueNativeByName[fldDataSetGViewPars] := FGroupViewParams;

  FieldIndex := CI.Owner.Fields.IndexByName(fldDataSetOnAddMenue);
  if FieldIndex >= 0 then
    CI.FieldValue[FieldIndex] := BooleanToInt[FOnAddMenu];

  FieldIndex := CI.Owner.Fields.IndexByName(fldDataSetDescription);
  if FieldIndex <> -1 then
    CI.FieldValue[FieldIndex] := FDescription;

  if tsGUID in FLoadingState then
  begin
    FieldIndex := CI.Owner.Fields.IndexByName(fldDataSetGUID);
    if FieldIndex <> -1 then
      CI.FieldValue[FieldIndex] := GUIDToString(FGUID);
  end;
end;

procedure TTableMeta.AssignFields(aList: TFieldsMeta);
var
  I: Integer;
  FMeta: TFieldMeta;
begin
  FKField.Clear;
  FFields.Clear;

  for I := 0 to aList.Count - 1 do
  begin
    FMeta := TFieldMeta.Create;
    FMeta.Owner := Self;
    FMeta.Assign(aList[I]);
    FFields.Add(FMeta);
    if FMeta.Key then
      FKField.Add(FMeta)
  end;

  FLoadingState := FLoadingState + [tsLoading];
end;

function TTableMeta.CanDropFiles: Boolean;
begin
  with SecuritySystem do
    Result:=  Not IsReadOnly and
    ( (Assigned(FDropBlobField) and not FDropBlobField.IsReadOnly and CheckPolicyField(FDropBlobField.ID, sdUpdate)) or
      (Assigned(FDropNameField) and not FDropNameField.IsReadOnly and CheckPolicyField(FDropNameField.ID, sdUpdate)) or
      (Assigned(FDropSizeField) and not FDropSizeField.IsReadOnly and CheckPolicyField(FDropSizeField.ID, sdUpdate)) or
      (Assigned(FDropDateField) and not FDropDateField.IsReadOnly and CheckPolicyField(FDropDateField.ID, sdUpdate)) );
end;

procedure TTableMeta.CheckState(const State: TTableFlagStateType);
begin
  if not(tsLoading in FLoadingState) then
    case State of
      tsFields: { Список полей }
          if not(tsFields in FLoadingState) or (FFields.Count = 0) then
            if IsDynamic then
              Include(FLoadingState, tsFields)
            else
              LoadFields;
      tsUserValues: { Пользовательские настройки }
        if not(tsUserValues in FLoadingState) then
          if IsDynamic then
            Include(FLoadingState, tsUserValues)
          else
            LoadUserValues;
      tsActions: { Список действий }
        if not(tsActions in FLoadingState) then
          LoadActions;
      tsElements: { Список элементов }
        if not(tsElements in FLoadingState) then
          LoadElements;
      tsFilter: { Список фильтров }
        if not(tsFilter in FLoadingState) then
          LoadFilters;
      tsConstraints: { Список ограничений }
        if not(tsConstraints in FLoadingState) then
          if IsDynamic then
            Include(FLoadingState, tsConstraints)
          else
            LoadConstraints;
      tsPermissions: { Права пользователя на уровне БД }
        if not (tsPermissions in FLoadingState) then
          begin
            Include(FLoadingState, tsLoading);
            try
              FPermissions := Database.DatasetPermissions(Table);
            finally
              Exclude(FLoadingState, tsLoading);
            end;
          end;
    end;
end;

function TTableMeta.GetIsDynamic: Boolean;
begin
  Result:= tsDynamic in FLoadingState;
end;

procedure TTableMeta.SetName(const Value: string);
begin
  FName := Value;
end;

function TTableMeta.GetIco: Integer;
begin
  Result:= FIco;
  if (Result = 0) and assigned(OwnerTable) then
    if self<>OwnerTable then Result:= OwnerTable.Ico;
end;

procedure TTableMeta.SetIco(const Value: Integer);
begin
  FIco:= Value;
end;

procedure TTableMeta.SetIsDynamic(const Value: Boolean);
begin
  if Value then Include(FLoadingState, tsDynamic)
           else Exclude(FLoadingState, tsDynamic);
end;

procedure TTableMeta.LoadActions;
begin
  if Not Assigned(FActionConditionList) then
    FActionConditionList := TEventMetaList.Create(True);

  FActionConditionList.TableMetaID := FId;
  Include(FLoadingState, tsActions);
end;

procedure TTableMeta.LoadFields;
var
  FMeta, LMeta: TFieldMeta;
  R, I, j, N, PairFieldIndex, FieldIndex: Integer;
  Parser: TDeParser;
  FMatrix: TContiguityMatrix;
  FailedFields: TList;
  fn, FailedNames: string;
  procedure CheckBaseFields;
  var
    I: Integer;
  begin
    if Fields.Count = 0 then Exit;
      //Eraise Exception.Create('TableMeta FieldList is Empty');

    // если не определено поле имени, то устанавливаем первое текстовое
    if not Assigned(FNField) then
      begin
        FNField:= Fields[0];
        for I:= 1 to Pred(FFields.Count) do
          if not (FFields[I].DataType in LogicalTypes) and not (FFields[I].DataType in NotSortTypes) then
          if (FFields[I].VisibleLevel > FNField.VisibleLevel) or
            ((FFields[I].VisibleLevel = FNField.VisibleLevel) and (FFields[I].DataType in StringTypes) and not(FNField.DataType in StringTypes))
            then FNField := Fields[i];
        FNField.Role:= FNField.Role + [frNameField];
      end;
    // если не определено ключевое поле, то устанавливаем его
    if FKField.Count = 0 then
      for I := 0 to Pred(FFields.Count) do
        if FFields[I].DataType = ftAutoInc then
        begin
          FFields[I].Key := True;
          FKField.Add(FFields[I]);
          Break;
        end;
    // если не определено ключевое поле, то устанавливаем его
    if FKField.Count = 0 then
      for I := 0 to Pred(FFields.Count) do
        if FFields[I].DataType in [ftGUID, ftWord, ftInteger, ftLargeint] then
        begin
          FFields[I].Key := True;
          FKField.Add(FFields[I]);
          Break;
        end;
    // если не определено ключевое поле, то устанавливаем его
    if FKField.Count = 0 then
      for I := 0 to Pred(FFields.Count) do
        if FFields[I].DataType in IntegerTypes then
        begin
          FFields[I].Key := True;
          FKField.Add(FFields[I]);
          Break;
        end;
    // нет целочисленных полей - ключевым становится первое в списке
    if (FKField.Count = 0) and (FFields.Count > 0) then
    begin
      FFields[0].Key := True;
      FKField.Add(FFields[0]);
    end;
    // если не определено поле имени, то устанавливаем его равным ключевому
    if (not Assigned(FNField)) and (FKField.Count > 0) then
    begin
      FNField := FKField[0];
      FNField.Role := FNField.Role + [frNameField];
    end;
  end;
  procedure CheckStoredProcParameter;
  var
    VariableItem: TVariableItem;
    VariableAccessMode: TVariableAccessMode;
  begin
    if (Pos('@', FMeta.Original) = 1) then
      if (ObjectType = otStoredProc) then
        begin
          VariableItem := FParameters.FindVariable(FMeta.Original);
          if not Assigned(VariableItem) then
          begin
            VariableAccessMode := [amRead];
            if not FMeta.IsReadOnly then
              Include(VariableAccessMode, amWrite);
            VariableItem := TVariableItem.Create(FMeta.DataType, FMeta.Original, VariableAccessMode);
            if FParameters.Add(VariableItem) = -1 then
              VariableItem.Free;
          end;
          FMeta.IsStored := False;
        end;
  end;

begin
  if IsDynamic then
  begin
    Include(FLoadingState, tsLoading);
    try
{$IFDEF DEBUG}
      DebugLog('%s.LoadFields dynamic start for %s ...', [ClassName, Table]);
{$ENDIF}
      if Assigned(Database) then
        if not (ObjectType = otStoredProc) then
          Database.GetMetaTableInfo(Self);
      for I := 0 to Pred(Fields.Count) do
        begin
          Fields[I].FId := -MetaData.GenerateDynamicFieldID;
        end;
      CheckBaseFields;
      Include(FLoadingState, tsFields);
    finally
      Exclude(FLoadingState, tsLoading);
{$IFDEF DEBUG}
      Fields.DebugViewFieldsLog(ClassName + '.LoadFields dynamic finish for ' + Table + ' ...');
{$ENDIF}
    end;
    Exit;
  end;

  if tsPreloadFields in FLoadingState then
    begin
      Include(FLoadingState, tsLoading);
      {$IFDEF DEBUG}
      DebugLog(ClassName + '.LoadFields start for %s preloaded ...', [Table]);
      {$ENDIF}
    end
  else
    begin
      if not Assigned(MetaData.FieldMetaDataSet)then
        Exit;

      Include(FLoadingState, tsLoading);
      if Assigned(OwnerTable) and (OwnerTable <> Self) then
        for I := 0 to OwnerTable.Fields.Count - 1 do
          begin
            // набор данных не нижнего уровня - наследуются поля
            FMeta := TFieldMeta.Create;
            FMeta.Assign(GrandOwnerTable.Fields[I]);
            if not FMeta.IsLookup then
              FMeta.Owner := Self;
            FFields.Add(FMeta);
          end;

      {$IFDEF DEBUG}
      DebugLog(ClassName + '.LoadFields start for %s ...', [Table]);
      {$ENDIF}
      MetaData.FieldMetaDataSet.Descr.BeginUpdate;
      MetaData.FieldMetaDataSet.Descr.ParamValueByName['ID'] := ID;
      MetaData.FieldMetaDataSet.Descr.ParamValueByName['ID2'] := ID;
      MetaData.FieldMetaDataSet.Descr.EndUpdate;
      MetaData.FieldMetaDataSet.Open;
      for R:=0 to Pred(MetaData.FieldMetaDataSet.RecordCount) do
      begin
        MetaData.FieldMetaDataSet.RecNo:= R;
        fn:= MetaData.FieldMetaDataSet.Value[indFieldsOriginal];
        if (0 < Length(Trim(fn))) then
          if (VarToInt(MetaData.FieldMetaDataSet.Value[indFieldsTable]) = ID) then
            begin
              // обнаружено поле
              FMeta:= FFields.FindByName(fn);
              if not Assigned(FMeta) then
                begin
                  FMeta := TFieldMeta.Create;
                  FMeta.Owner := Self;
                  FFields.Add(FMeta);
                end;
              FMeta.Assign(MetaData.FieldMetaDataSet);
              CheckStoredProcParameter;
            end
          else
            begin
              // обнаружена связь
              FMeta:= TFieldMeta.Create;
              FMeta.Owner := MetaData.GetTableMeta(VarToInt(MetaData.FieldMetaDataSet.Value[indFieldsTable]));
              FMeta.Assign(MetaData.FieldMetaDataSet);
              Links.Add(FMeta);
              CheckStoredProcParameter;
            end;
      end;

      MetaData.FieldMetaDataSet.Close;
    end;

  // установка указателей на служебные поля
  for I := 0 to FFields.Count - 1 do
  begin
    FMeta := FFields[I];
    if FMeta.Key then
      FKField.Add(FMeta);
    if frDeleteSignField in FMeta.Role then
    begin
      FDField := FMeta;
      if FMeta.DataType = ftDateTime then
        begin
          FDField.DefaultValue := EmptyStr;
          FDField.Value1 := EmptyStr;
          FDField.Value2 := EmptyStr;

          if Self = Self.OwnerTable then
            begin
              {
                Запись считаем удаленной если:

                1. Значение поля указано (IS NOT NULL)
                2. Значение меньше текущей даты на сервере! (Удалена в прошлом)

                Добавляем условие SQL для фильтрации удаленных записей:

                ([Поле] is null) or ([Поле] > GETDATE())
              }
              FDeletedFilter := TFilterItem.Create;
              FDeletedFilter.AddCondition(FMeta, opIs, Null);

              FDeletedFilter.AddIdent(FMeta.Original, FMeta.DataType, FMeta.CodePage);
              { TODO -oKruglov -cBugfix : Скорее всего из-за этого неправильный Postfix?! }
              FDeletedFilter.AddConst(True, ftBoolean);
              FDeletedFilter.AddFunc('NOW', 1);
              FDeletedFilter.AddOperation(opGT);

              FDeletedFilter.AddOperation(opOr);

              Filters.AddFilter(FDeletedFilter);
              FDeletedFilter.Active := not MetaData.ShowRemoved;
            end
          else
            FDeletedFilter := OwnerTable.DeletedFilter;
        end
      else
        begin
          if FMeta.ShowType = stNone then
            FMeta.ShowType := stYesNo;

          if FDField.Value1 = FDField.Value2 then
          begin
            FDField.DefaultValue := '0';
            FDField.Value1 := '0';
            FDField.Value2 := '1';
          end;
          if Self = Self.OwnerTable then
          begin
            FMeta.InnerVisibleLevel := fvService;
            FDeletedFilter := TFilterItem.Create;
            FDeletedFilter.AddCondition(FMeta, opEQ, FMeta.Value2);
            FDeletedFilter.AddOperation(opNot);
            Filters.AddFilter(FDeletedFilter);
            FDeletedFilter.Active := not MetaData.ShowRemoved;
          end
          else
            FDeletedFilter := OwnerTable.DeletedFilter;
        end;
    end;
    if frNameField in FMeta.Role then
      FNField := FMeta;
    if frDefaultRecordSign in FMeta.Role then
    begin
      FOField := FMeta;
      if FMeta.ShowType = stNone then
        FMeta.ShowType := stYesNo;

      if (FOField.DefaultValue = FOField.Value2) or (FOField.Value1 = FOField.Value2) then
      begin
        FOField.DefaultValue := '0';
        FOField.Value1 := '0';
        FOField.Value2 := '1';
      end;
    end;
    if frFolderSign in FMeta.Role then
      FGField := FMeta;
    if (frIcon in FMeta.Role) and (FMeta.DataType in StringTypes) or (FMeta.DataType in BinaryTypes) then
      FPreviewField := FMeta;
    if (frIcon in FMeta.Role) and ((FMeta.DataType in IntegerTypes) or assigned(FMeta.LinkTable)) then
      FIconField := FMeta;
    if (frColor in FMeta.Role) and ((FMeta.DataType in IntegerTypes) or assigned(FMeta.LinkTable)) then
      FColorField := FMeta;
    // v. 18.6 +
    if (frLatitude in FMeta.Role) and (FMeta.DataType in FloatTypes) then
      FLatitudeField := FMeta;
    if (frLongitude in FMeta.Role) and (FMeta.DataType in FloatTypes) then
      FLongitudeField := FMeta;
    if (frAddress in FMeta.Role) and (FMeta.DataType in StringTypes) then
      FAddressField := FMeta;
    if (frPhone in FMeta.Role) and (FMeta.DataType in StringTypes) then
      FPhoneField := FMeta;
    if (frEMail in FMeta.Role) and (FMeta.DataType in StringTypes) then
      FMailField := FMeta;
    if (frDropFile in FMeta.Role) then
      begin
        if (FMeta.DataType in BinaryTypes) then FDropBlobField:= FMeta else
        if (FMeta.DataType in StringTypes) then FDropNameField:= FMeta else
        if (FMeta.DataType in DateTimeTypes) then FDropDateField:= FMeta else
        if (FMeta.DataType in IntegerTypes) then FDropSizeField:= FMeta;
      end;
     // FMailField := FMeta; << ЭТО ЧЁ ЗА БЯКА?! ТАК ВСЕГДА ПОЛЕ MailField БУДЕТ!!!
    // v. 18.6 -
    if FMeta.Link = Self.ID then
    begin
      FPField := FMeta;
      LMeta := TFieldMeta.Create;
      LMeta.Assign(FMeta);
      FLinks.Add(LMeta);
    end;
  end;

  // v. 18.6 +
  if Assigned(FLatitudeField) then
    if Assigned(FLongitudeField) then
      begin
        if FLatitudeField = FLongitudeField then
          begin
            // НОНСЕНС!!! Широта и долгота - ОДНО поле!!!
            FLatitudeField := nil;
            FLongitudeField := nil;
          end
        else
          Include(FLoadingState, tsGEO);
      end
    else
      FLatitudeField := nil // Нельзя указывать ТОЛЬКО поле географической широты!!!
  else
    FLongitudeField := nil; // При отсутствии поля географической широты НЕЛЬЗЯ использовать поле географической долготы!!!
  // v. 18.6 -

  CheckBaseFields;

  // переставляем поле имени на первое место
  if Assigned(FNField) then
  begin
    I := FFields.IndexByID(FNField.ID);
    FFields.Move(I, 0);
  end;

  // создание Lookup-полей
  if Self = Self.OwnerTable then
  begin
    N := FFields.Count; // Запоминаем, чтобы не перебирать вновь созданные
    for I := 0 to N - 1 do
    begin
      FMeta := FFields[I];
      FMeta.CreateLookupPair;
      if Assigned(FMeta.LookupPair) then
        FFields.Add(FMeta.LookupPair);
    end
  end
  else
    if Assigned(OwnerTable) and Assigned(OwnerTable.FFields) then
      for I := 0 to Pred(OwnerTable.FFields.Count) do
        if OwnerTable.FFields[I].IsLookup then
          begin
            PairFieldIndex := Fields.IndexByID(OwnerTable.Fields[I].LookupPair.ID);
            FFields[I].LookupPair := FFields[PairFieldIndex];
            FFields[PairFieldIndex].LookupPair := FFields[I];
          end;

  Exclude(FLoadingState, tsLoading);
  Include(FLoadingState, tsFields);

  // разбор выражений
  Parser := TDeParser.Create;
  Parser.Table := Self;
  for I := 0 to FFields.Count - 1 do
    if (not FFields[I].Key) and (not FFields[I].IsLookup) and
      (Length(Trim(FFields[I].DefaultValue)) > 0) then
      try
        Parser.Parse(FFields[I].DefaultValue, FFields[I].DefaultPostfix);
      except
        on E: EDeParserError do
        begin
          {$IFDEF DEBUG}
          if Assigned(FFields[I]) and (Length(FFields[I].DefaultValue) <> 0) then
            DebugLog(ClassName + '.LoadFields skip parse default value ' + QuotedStr(FFields[I].DefaultValue) + ' error: ' + E.Message)
          else
            DebugLog(ClassName + '.LoadFields skip parse default value error: ' + E.Message);
          {$ENDIF}
          FFields[I].DefaultPostfix.Clear;
        end;
      end;
  Parser.Free;

  // анализируем взаимную зависимость выражений, строим матрицу смежности
  FailedNames := EmptyStr;
  FMatrix := TContiguityMatrix.Create;
  FMatrix.Size := FFields.Count;
  for I := 0 to FFields.Count - 1 do
    for j := 0 to FFields[I].DefaultPostfix.Count - 1 do
      if FFields[I].DefaultPostfix[j].ItemType = piIdent then
      begin
        FieldIndex := FFields.IndexByName(FFields[I].DefaultPostfix[j].Ident);
        if FieldIndex >= 0 then
          FMatrix.Link[I, FieldIndex] := True;
      end;

  // анализируем матрицу в поисках ошибочных полей
  FailedFields := TList.Create;
  FMatrix.CheckRings(FailedFields);
  for I := 0 to FailedFields.Count - 1 do
  begin
    StrAdd(FailedNames, #13#10, FFields[I].Original);
    FFields[NativeInt(FailedFields[I])].DefaultPostfix.Clear;
  end;
  FailedFields.Free;
  FMatrix.Free;

  if FailedNames <> EmptyStr then
  begin
    DeMetadata.ErrorMessage := GetTitle('_dL.correspondfields') + #13#10 +
      FailedNames;
    PostMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 0, 0);
  end;
  {$IFDEF DEBUG}
  if Length(FailedNames) <> 0 then
    FailedNames := ' (' + FailedNames + ')';
  if Assigned(FFields) then
    Fields.DebugViewFieldsLog(ClassName + '.LoadFields finish for ' + Table + FailedNames + ' ...')
  else
    DebugLog(ClassName + '.LoadFields finish for %s ...', [Table + FailedNames]);
  {$ENDIF}
end;

procedure TTableMeta.PreLoadFields(Dataset: TDeDataset);
var
  R, FieldTableIndex, FieldLinkIndex, Count: Integer;
  FieldMeta: TFieldMeta;
begin
  if Assigned(Dataset) and not (tsFields in FLoadingState) then
    begin
      FieldTableIndex := Dataset.IndexByName(fldFieldsTable);
      FieldLinkIndex := Dataset.IndexByName(fldFieldsLink);
      Count := 0;
      if (FieldTableIndex <> -1) and (FieldLinkIndex <> -1) then
        try
          {$IFDEF DEBUG}
          DebugLog(ClassName + '.PreLoadFields start for %s ...', [Table]);
          {$ENDIF}
          //Include(FLoadingState, tsLoading);
          for R:=0 to Pred(Dataset.RecordCount) do
            begin
              Dataset.RecNo:= R;
              if Dataset.IntValueDef(FieldTableIndex) = ID then
                begin
                  if OwnerTable = Self then
                    begin
                      // обнаружено поле
                      FieldMeta := TFieldMeta.Create;
                      FieldMeta.Owner := Self;
                      FieldMeta.Assign(Dataset);
                      if Length(Trim(FieldMeta.Original)) = 0 then
                        FieldMeta.Free
                      else
                        begin
                          FFields.Add(FieldMeta);
                          //CheckStoredProcParameter;
                          Inc(Count);
                        end;
                    end;
                  end
              else if Dataset.IntValueDef(FieldLinkIndex) = ID then
                begin
                  // обнаружена связь
                  FieldMeta := TFieldMeta.Create;
                  FieldMeta.Owner := MetaData.GetTableMeta(Dataset.IntValueDef(FieldTableIndex));
                  FieldMeta.Assign(Dataset);
                  if Length(Trim(FieldMeta.Original)) = 0 then
                    FieldMeta.Free
                  else
                    begin
                      Links.Add(FieldMeta);
                      //CheckStoredProcParameter;
                      Inc(Count);
                    end;
                end;
            end;
        finally
          {$IFDEF DEBUG}
          if Assigned(Fields) then
            Fields.DebugViewFieldsLog(ClassName + Format('.PreLoadFields finish %d field(s) for %s ...', [Count, Table]))
          else
            DebugLog(ClassName + '.PreLoadFields finish %d field(s) for %s ...', [Count, Table]);
          {$ENDIF}
          if Count <> 0 then Include(FLoadingState, tsPreloadFields);
        end;
    end;
end;

procedure TTableMeta.LoadUserValues;
var
  Dataset: TDeDataSet;
  Template: TDeTemplate;
  FieldNode: TDeNode;
  R, i,Index: Integer;
  FieldName, sXML: string;
  RecursiveField, Field: TFieldMeta;
  UserValue: TUserValue;
begin
  Include(FLoadingState, tsLoading);
  try
    Dataset:= MetaData.MetadataDB.CreateQuery(qtSelect);
    try
      try
        Dataset.Descr.Table:= tblUserDataSet;
        Dataset.Descr.AddCondition(fldUDataSetTable, ftInteger, opEQ, ID);
        Dataset.Descr.AddCondition(fldUDataSetSubject, ftInteger, opIs, Null);
        if Assigned(UserSession) then
          begin
            Dataset.Descr.AddCondition(fldUDataSetSubject, ftInteger, opEQ, UserSession.ID);
            Dataset.Descr.AddOperation(opOr);
          end;
        Dataset.Descr.AddCondition(fldUDataSetType, ftInteger, opEQ, uvFilterCommon);
        Dataset.Descr.AddOperation(opOr);
        Dataset.Descr.AddOperation(opAnd);
        Dataset.Descr.AddSortField(fldUDataSetOrder, sdAscending);
      finally
      end;

{$IFDEF DEBUG}
      if Assigned(UserSession)
        then DebugLog('%s.LoadUserValues start for %s [policy: %u] ...', [ClassName, Table, UserSession.ID])
        else DebugLog('%s.LoadUserValues start for %s ...', [ClassName, Table]);
{$ENDIF}

      try
        Dataset.Open;
        if Dataset.Active then
          begin
            for R:=0 to Pred(Dataset.RecordCount) do
              begin
                Dataset.RecNo:= R;
                UserValue:= TUserValue.Create;
                UserValue.Assign(DataSet);
                FUserValues.Add(UserValue);
              end;
          end;
      finally
        Dataset.Close;
      end;

      // Ищем персональные или общие настройки отображения полей
      sXML:= EmptyStr;
      for i:= 0 to Pred(FUserValues.count) do
        if (FUserValues[i].TypeValue = uvFields) then
          begin
            sXML:= Trim(FUserValues[i].XML);
            if 0 < Length(sXML) then Break;
          end;

      // Нашли персональные или общие настройки отображения полей - парсим
      if 0 < Length(sXML) then
      begin
        Template:= TDeTemplate.Create;
        try
          Template.Text:= sXML;
          if Assigned(Template.Root) then
          begin
            FieldNode:= Template.Root.FindNode('fields');
            if Assigned(FieldNode) then
              for Index:= 0 to Pred(FieldNode.Count) do
                if SameText(FieldNode.ChildNodes[Index].NodeName, 'field') then
                begin
                  FieldName:= FieldNode.ChildNodes[Index].Attributes['name'];
                  Field:= Fields.FindByName(FieldName);

                  {
                  if not Assigned(Field) then
                    begin
                      RecursiveField:= GetFieldByName(FieldName, True);
                      if assigned(RecursiveField) then
                        begin
                          Field:= Fields.New(FieldName, RecursiveField.FName);
                          Field.Assign(RecursiveField, True);
                          Field.IsStored:= false;
                          Field.Calculated:= True;
                          Field.DefaultValue:= FieldName;
                          Field.ID:= -RecursiveField.ID;
                    //      Field.IsLookup:= True;
                        end;
                    end;
                  {}
                  if Assigned(Field) then
                    begin
                      Field.FUWidth:= StrToIntDef(FieldNode.ChildNodes[Index].Attributes['width'], Field.FWidth);
                      Field.FUVisible:= TFieldVisible(StrToIntDef(FieldNode.ChildNodes[Index].Attributes['visible'],
                                                                                               ord(Field.FVisibleLevel)));
                      Field.FUOrder:= StrToIntDef(FieldNode.ChildNodes[Index].Attributes['order'], Field.FOrder);
                    end;
                end;
          end;
        finally
          Template.Free;
        end;
      end;
    finally
      Dataset.Free;
{$IFDEF DEBUG}
      if Assigned(UserSession) then
        if Assigned(FFields) then
          FFields.DebugViewFieldsLog(Format('%s.LoadUserFields finish for %s [policy: %u] ...', [ClassName, Table, UserSession.ID]))
        else
          DebugLog('%s.LoadUserFields finish for %s [policy: %u] ...', [ClassName, Table, UserSession.ID])
      else
        if Assigned(FFields) then
          FFields.DebugViewFieldsLog(Format('%s.LoadUserFields finish for %s ...', [ClassName, Table]))
        else
          DebugLog('%s.LoadUserFields finish for %s ...', [ClassName, Table]);
{$ENDIF}
    end;
    Include(FLoadingState, tsUserValues);
  finally
    Exclude(FLoadingState, tsLoading);
  end;
end;

procedure TTableMeta.DeleteUserValues(const aType: Integer; aUserID: Integer = 0);
var
  Dataset: TDeDataset;
begin
  Dataset := MetaData.MetadataDB.CreateQuery(qtDelete);
  try
    Dataset.Descr.BeginUpdate;
    try
      Dataset.Descr.Table := tblUserDataSet;
      Dataset.Descr.AddParamCondition(fldUDataSetTable, ftInteger, opEQ, 'DID', FId);
      Dataset.Descr.AddParamCondition(fldUDataSetType, ftSmallInt, opEQ, 'TID', aType);
      Dataset.Descr.AddOperation(opAnd);
      if AUserID <> 0 then
        begin
          Dataset.Descr.AddParamCondition(fldUDataSetSubject, ftInteger, opEQ, 'SID', AUserID);
          Dataset.Descr.AddOperation(opAnd);
        end;
    finally
      Dataset.Descr.EndUpdate;
    end;
    try
      Dataset.ExecuteQuery;
    except
      on E: Exception do
      if Length(E.Message) <> 0 then
        SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(pChar(E.Message)))
      else
        SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(pChar('_dL.procedureerror')));
    end;
  finally
    Dataset.Free;
  end;

  ClearUserValues;
end;

procedure TTableMeta.LoadElements;
begin
  FLoadingState := FLoadingState + [tsLoading];
  FElements.LoadForms(ID);
  FLoadingState := FLoadingState - [tsLoading];
  FLoadingState := FLoadingState + [tsElements];
end;

procedure TTableMeta.LoadFilters;
begin
  FLoadingState := FLoadingState + [tsFilter];
  FFiltersManager.AddFilter(FilterPostfix);
  // FFiltersManager.AddFilter(ArchivedFilter);
  if Assigned(GroupFilters) and not IsSystem then
    GroupFilters.LoadFilters(Self);
end;

procedure TTableMeta.LoadConstraints;
var
  Q: TDeDataSet;
  Constraint: TDeConstraint;
  R, dIndex: Integer;
begin
  { начитываем список ограничений }
  Q:= MetaData.MetadataDB.CreateQuery(qtSelect);
  try
    Q.Descr.Table := tblConstraints;
    if mpDatabaseNewConstraints in MetaData.MetadataPresents then
      begin
        Q.Descr.AddCondition(fldConstraintsDataset, ftInteger, opEQ, ID);
        Q.Open;
        dIndex:= Q.IndexByName(fldConstraintsDeleted);
      end
    else
      begin
        Q.Descr.AddCondition(fldCSDataset, ftInteger, opEQ, ID);
        Q.Open;
        dIndex:= Q.IndexByName(fldCSDeleted);
      end;

    for R:=0 to Pred(Q.RecordCount) do
      begin
        Q.RecNo:= R;
        if (dIndex=-1) or VarIsNull(Q.Value[dIndex]) or (Q.Value[dIndex]<>1) then
          begin
            Constraint:= TDeConstraint.Create(Self);
            Constraint.Assign(Q);
            FConstraintList.Add(Constraint);
          end;
      end;
    Q.Close;
  finally
    Q.Free;
  end;
  FLoadingState := FLoadingState + [tsConstraints];
end;

function TTableMeta.GetFields: TFieldsMeta;
begin
  CheckState(tsFields);
  Result := FFields;
end;

function TTableMeta.GetElements: TElementsManager;
begin
  CheckState(tsElements);
  Result := FElements;
end;

function TTableMeta.GetFilters: TFiltersManager;
begin
  CheckState(tsFilter);
  Result := FFiltersManager;
end;

function TTableMeta.GetConstraints: TObjectList<TDeConstraint>;
begin
  CheckState(tsConstraints);
  Result := FConstraintList;
end;

function TTableMeta.GetUserValues: TUserValues;
begin
  CheckState(tsUserValues);
  Result:= FUserValues;
end;

function TTableMeta.GetKField: TFieldsMeta;
begin
  CheckState(tsFields);
  Result := FKField;
end;

function TTableMeta.GetField(aIndex: Integer): TFieldMeta;
begin
  CheckState(tsFields);
  case aIndex of
    1:  Result := FNField;
    2:  Result := FPField;
    3:  Result := FOField;
    4:  Result := FDField;
    5:  Result := FGField;
    6:  Result := FPreviewField;
    7:  Result := FIconField;
    8:  Result := FColorField;
    9:  Result := FLatitudeField;
    10: Result := FLongitudeField;
    11: Result := FAddressField;
    12: Result := FPhoneField;
    13: Result := FMailField;
    14: Result := FDropBlobField;
    15: Result := FDropNameField;
    16: Result := FDropSizeField;
    17: Result := FDropDateField;
  else
    Result := nil;
  end;
end;

function TTableMeta.GetFieldByName(aName: string; Recurce: boolean = False)
  : TFieldMeta;
var
  I, P: Integer;
  FName, fAddName: String;
begin
  Result := nil;
  CheckState(tsFields);

  for I := 0 to Fields.Count - 1 do
    if AnsiCompareText(aName, FFields[I].Original) = 0 then
    begin
      Result := FFields[I];
      Break;
    end;

  // ищем рекурсивно
  if (Result = nil) and (Recurce) then
  begin
    P := Pos('.', aName);
    if (P > 0) then
    begin
      FName := Copy(aName, 1, P - 1);
      fAddName := Copy(aName, P + 1, MaxInt);

      for I := 0 to Fields.Count - 1 do
        if AnsiCompareText(FName, FFields[I].Original) = 0 then
        begin
          if Assigned(FFields[I].LinkTable) then
            Result := FFields[I].LinkTable.GetFieldByName(fAddName, True);
          Break;
        end;
    end;
  end;
end;

function TTableMeta.GetActionConditionList: TEventMetaList;
begin
  CheckState(tsActions);
  Result := FActionConditionList;
end;

function TTableMeta.GetFirstForm: TElementMeta;
var i: Integer;
begin
  Result := nil;

  CheckState(tsElements);
  for i:=0 to Pred(Elements.Count) do
    if 0 < Elements[i].Count then
      if Elements[i].Items[0].ElementType = etForm then
        Exit(Elements[i].Items[0]);
end;

function TTableMeta.GetKeyCount: Integer;
begin
  Result := KField.Count
end;

function TTableMeta.GetIsReadOnly: boolean;
begin
  Result := FReadOnly;
  if Assigned(FDatabase) then
    Result := Result or FDatabase.IsReadOnly;
end;

function TTableMeta.GetIsShowRight: boolean;
begin
  Result := FIsShowRight and UserSession.IsAdmin;
end;

function TTableMeta.GetErrorsDetected: boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to Pred(Fields.Count) do
    if Fields[Index].ErrorInStructure then
    begin
      Result := True;
      Break;
    end;
end;

function TTableMeta.GetErrorMessage: string;
var
  Index: Integer;
begin
  Result := EmptyStr;
  for Index := 0 to Pred(Fields.Count) do
    if (Fields[Index].ErrorInStructure) and
      (Length(Fields[Index].ErrorMessage) <> 0) then
      StrAdd(Result, #13#10, Fields[Index].ErrorMessage);
end;

function TTableMeta.GrandOwnerTable: TTableMeta;
begin
  if Assigned(FOwnerTable) and (Self <> FOwnerTable) then
    Result := FOwnerTable.GrandOwnerTable
  else
    Result := Self;
end;

procedure TTableMeta.DefineTableLinks;
begin
  if Assigned(FOwnerTable) then
    Exit;

  if (FParentTableID = ID) or (FParentTableID = 0) or (FParentTableID = Null)
  then
  begin
    FOwnerTable := Self;
  end
  else
  begin
    FOwnerTable := MetaData.GetTableMeta(FParentTableID);
    if Assigned(FOwnerTable) then
    begin
      FOwnerTable.DefineTableLinks;
      if FOwnerTable <> Self then
        FOwnerTable.FChildrenTables.Add(Self);
    end
    else
    begin
      FOwnerTable := Self;
    end;
  end;
end;

procedure TTableMeta.UndefTableLinks;
var
  I: Integer;
begin
  for I := 0 to FChildrenTables.Count - 1 do
  begin
    if FChildrenTables[I].FOwnerTable = Self then
      FChildrenTables[I].FOwnerTable := nil;
  end;

  if Assigned(FOwnerTable) and (FOwnerTable <> Self) then
    FOwnerTable.FChildrenTables.Extract(Self);
  FOwnerTable := nil;
end;

procedure TTableMeta.Clear;
begin
  ClearFields;
  FId := Null;
  FIco := Null;
  FName := EmptyStr;
  FTable := EmptyStr;
  FIsDirectory := idUnknown;
  FReadOnly := False;
  FGroupViewType := vtNone;
  FGroupViewParams := EmptyStr;
  FDatabase := nil;
  FOwnerTable:= nil;
  FLoadingState := [];
  FIsInterrelations := False;
  FIsNotes := False;
  FIsShowRight := False;
  UndefTableLinks;
  FParentTableID := FId;
end;

procedure TTableMeta.ClearElements(aClearData: Boolean = True);
begin
  if aClearData and (tsElements in FLoadingState) then
    FElements.Clear;
  Exclude(FLoadingState, tsElements);
end;

procedure TTableMeta.ClearUserValues;
begin
  if (tsUserValues in FLoadingState) then
    FUserValues.Clear;
  Exclude(FLoadingState, tsElements);
end;

procedure TTableMeta.ClearFields;
begin
  ClearUserValues;
  ClearElements;
  if tsFields in FLoadingState then
  begin
    FFields.Clear;// FreeAndNil(FFields);
    FKField.Clear;// FreeAndNil(FKField);
    FNField:= nil;
    FDField:= nil;
    FPField:= nil;
    FOField:= nil;
    FGField:= nil;

    FIconField:= nil;
    FPreviewField:= nil;
    FColorField:= nil;
    FLatitudeField:= nil;
    FLongitudeField:= nil;
    FAddressField:= nil;
    FPhoneField:= nil;
    FMailField:= nil;

    FDropBlobField:= nil;
    FDropNameField:= nil;
    FDropDateField:= nil;
    FDropSizeField:= nil;

    FLinks.Clear;
  end;

  FSelectSQL := EmptyStr;
  FUpdateSQL := EmptyStr;
  FInsertSQL := EmptyStr;
  FDeleteSQL := EmptyStr;
  FPermanentDeleteSQL := EmptyStr;

  Exclude(FLoadingState, tsFields);
  if tsConstraints in FLoadingState then
    FConstraintList.Clear;
  // Вот просто так без удаления объекта? Ладно, сделаю так же ...
  FDeletedFilter := nil;
  Exclude(FLoadingState, tsConstraints);
end;

function TTableMeta.FoundInParentTables(aTable: TTableMeta): boolean;
var
  I: Integer;
begin
  Result := False;
  if Assigned(aTable) then
    for I := 0 to Fields.Count - 1 do
      if Fields[I].Link = aTable.ID then
      begin
        Result := True;
        Break;
      end;
end;

procedure TTableMeta.SetFilter(const aFilter: string);
begin
  if FFilter <> aFilter then
  begin
    FFilter := aFilter;
    FFilterPostfix.Clear;
  end;
end;

function TTableMeta.GetFilterPostfix: TFilterItem;
var
  Parser: TDeParser;
begin
  Result := FFilterPostfix;
  if (Length(FFilter) > 0) and (FFilterPostfix.Count = 0) then
  begin
    Parser := TDeParser.Create;
    Parser.Table := Self;
    try
      Parser.Parse(FFilter, FFilterPostfix);
    except
      on E: EDeParserError do
      begin
{$IFDEF DEBUG}
        DebugLog('%s.GetFilterPostfix skip error for %s [%d]: %s', [ClassName, Table, VarToInt(ID), E.Message]);
{$ENDIF}
        FFilterPostfix.Clear;
      end;
    end;
    Parser.Free;
  end;
end;

function TTableMeta.GetTable: string;
{ создан для того, чтобы отрезать ключевое значение суффикса.
  суффикс используется для однозначного определения метаданных таблицы
  и отделяется от имени знаком '/' }
var
  P: Integer;
begin
  P := Pos('/', FTable);
  if P = 0 then
    Result := FTable
  else
    Result := Copy(FTable, 1, P - 1);
end;

function TTableMeta.IsSecurityProtected: boolean;
begin
  Result := SameText(FTable, tblDataset) or SameText(FTable, tblUserDataSet) or
    SameText(FTable, tblFields) or
  // SameText(FTable, tblAction) or
  // SameText(FTable, tblReports) or
    SameText(FTable, tblCommand) or // v.15.10
    SameText(FTable, tblConstraints) or // v.18.03
    SameText(FTable, tblSolutions) or // v.19.02
    ((tsGUID in LoadingState) and IsEqualGUID(GUID, guidAccount)) or // v.20.2 (расширенная метаструктура)!!!
    SameText(FTable, tblMenu);
end;

function TTableMeta.IsSystem: boolean;
begin
  if VarIsEmpty(SolutionID) then Result:= False
                            else Result:= (SolutionID=MetaSolution);
end;

function TTableMeta.GetSelectSQL: string;
var
  Index: Integer;
begin
  Result := FSelectSQL;
  if (Length(Result) = 0) and (ObjectType = otStoredProc) then
  begin
    for Index := 0 to Pred(FParameters.Count) do
    begin
      if Length(Result) <> 0 then
        Result := Result + ',';
      Result := Result + ' :' + FParameters[Index].Name;
    end;
    Result := 'exec ' + Table + Result;
    FSelectSQL := Result;
  end;
end;

function TTableMeta.GetLinkFilters: TFilterList;
begin
  if not Assigned(FLinkFilters) then
  begin
    FLinkFilters := TFilterList.Create(False);
    CheckState(tsFilter);
  end;
  Result := FLinkFilters;
end;

function TTableMeta.GetObjectType: TObjectType;
begin
  if FObjectType = otUnknown then
    try
      FObjectType:= Database.RetrieveMetaObjectType(Table);
    except
      FObjectType:= otNone
    end;
  Result:= FObjectType;
end;

function TTableMeta.GetOwnerTable: TTableMeta;
begin
  if assigned(FOwnerTable) then Result:= FOwnerTable
                           else Result:= self;
end;

procedure TTableMeta.SetObjectType(aType: TObjectType);
begin
  FObjectType:= aType;
end;

procedure TTableMeta.SetGUID(const Value: TGUID);
begin
  FGUID := Value;
  Include(FLoadingState, tsGUID);
end;

function TTableMeta.GetVariantGUID: Variant;
begin
  if tsGUID in FLoadingState then
    Result := GUIDToString(FGUID)
  else
    Result := Null;
end;

function TTableMeta.GetPermissions: TDeDatasetPermissions;
begin
  CheckState(tsPermissions);
  Result := FPermissions;
end;

function TTableMeta.IsMetabaseTable: Boolean;
begin
  if Assigned(Metadata) then
    Result := (FMetaTableIndex >= Low(Metadata.MetaTables)) and (FMetaTableIndex <= High(Metadata.MetaTables))
  else
    Result := False;
end;

function TTableMeta.GetVariantSolutionGUID: Variant;
var
  Dataset: TDeDataset;
begin
  { TODO -oКуфенко П. Ю. -cOptimization : По хорошему надо делать TDeSolutionMeta и хранить указатель на него! }
  if VarIsEmpty(FVariantSolutionGUID) then
    try
      FVariantSolutionGUID := Null; // Чтобы повторно не запрашивать!!!
      if Assigned(MetaData) and Assigned(MetaData.MetadataDB) then
        if mpMetadataSolutionGUID in MetaData.MetadataPresents then
          begin
            Dataset := MetaData.MetadataDB.CreateQuery(qtRow);
            try
              Dataset.Descr.BeginUpdate;
              try
                Dataset.Descr.Table := tblSolutions;
                Dataset.Descr.AddField(fldConfGUID);
                Dataset.Descr.AddParamCondition(fldConfID, ftInteger, opEQ, fldConfID, SolutionID);
              finally
                Dataset.Descr.EndUpdate;
              end;
              Dataset.Open;
              FVariantSolutionGUID := Dataset.Value[0];
            finally
              Dataset.Free;
            end;
          end;
    except
      { Ничего не делаем, т.к. результат NULL }
    end;
  Result := FVariantSolutionGUID;
end;

{ TTablesList }

function TTablesList.Get(const Index: Integer): TTableMeta;
begin
  if Index < Count then
    Result := TTableMeta(inherited Items[Index])
  else
    Result := nil;
end;

procedure TTablesList.Put(const Index: Integer; TableMeta: TTableMeta);
begin
  if Index >= Count then
    Count := Succ(Index);
  inherited Items[Index] := TableMeta;
end;

{$IFDEF DEBUG}
procedure TTablesList.DebugTablesLog(const Text: string);
  function PrepareTablesLog: string;
  var
    TextTable: TTextTable;
    ReadyGUID: boolean;
    Index: Integer;
    TableMeta: TTableMeta;
  begin
    if Count = 0 then
      Result := EmptyStr
    else
      begin
        ReadyGUID := IsGUID;
        TextTable := TTextTable.Create;
        try
          TextTable.Columns.Add('No', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('ID', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Schema', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Table', 0, taLeftJustify, taLeftJustify);
          if ReadyGUID then
            TextTable.Columns.Add('GUID', 38, taCenter, taCenter);
          TextTable.Columns.Add('MI', 0, taRightJustify, taCenter);
          for Index := 0 to Pred(Count) do
            begin
              TextTable.Lines[Index][0] := IntToStr(Succ(Index));
              TableMeta := Items[Index];
              if Assigned(TableMeta) then
                begin
                  TextTable.Lines[Index][1] := VarToStr(TableMeta.ID);
                  TextTable.Lines[Index][2] := TableMeta.Schema;
                  TextTable.Lines[Index][3] := TableMeta.Table;
                  if ReadyGUID then
                    begin
                      TextTable.Lines[Index][4] := GUIDToString(TableMeta.GUID);
                      TextTable.Lines[Index][5] := IntToStr(TableMeta.MetaTableIndex);
                    end
                  else
                    TextTable.Lines[Index][5] := IntToStr(TableMeta.MetaTableIndex);
                end;
            end;
          Result := #13#10 + TextTable.AsText(24);
        finally
          TextTable.Free;
        end;
      end;
  end;
begin
  DebugLog(Text + PrepareTablesLog);
end;
{$ENDIF}

function TTablesList.FindByID(const DatasetId: Integer): TTableMeta;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to Pred(Count) do
    if TTableMeta(inherited Items[Index]).FId = DatasetId then
    begin
      Result := Items[Index];
      Break;
    end;
end;

function TTablesList.FindByTable(const Name: string): TTableMeta;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to Pred(Count) do
    if SameText(TTableMeta(inherited Items[Index]).FTable, Name) then
    begin
      Result := Items[Index];
      Break;
    end;
end;

function TTablesList.IsGUID: boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to Pred(Count) do
    if Assigned(Items[Index]) and (tsGUID in Items[Index].LoadingState) then
    begin
      Result := True;
      Break;
    end;
end;

function TTablesList.LoadDefaultGUIDs(const TableOnly: boolean): Integer;
var
  Index: Integer;
  TableMeta: TTableMeta;
  GUID: TGUID;
begin
  Result := 0;
  for Index := 0 to Pred(Count) do
  begin
    TableMeta := Items[Index];
    if Assigned(TableMeta) and (Length(TableMeta.Table) <> 0) then
      if TDeMetadata.TryDatasetToGUID(TableMeta.Table, GUID) then
      begin
        TableMeta.FGUID := GUID;
        Include(TableMeta.FLoadingState, tsGUID);
        Inc(Result);
        if not TableOnly then
          TableMeta.Fields.LoadDefaultGUIDs;
      end;
  end;
end;

function TTablesList.FindByGUID(const DatasetGUID: TGUID;
  const DatasetOnly: boolean): TTableMeta;
var
  Index: Integer;
  TableMeta: TTableMeta;
  GUID: TGUID;
begin
  Result := nil;
  for Index := 0 to Pred(Count) do
  begin
    TableMeta := TTableMeta(inherited Items[Index]);
    if Assigned(TableMeta) then
      if tsGUID in TableMeta.LoadingState then
        begin
          if IsEqualGUID(TableMeta.GUID, DatasetGUID) then  Exit(TableMeta);
        end else
      if not DatasetOnly then
        if Length(TableMeta.Table) <> 0 then
          if TDeMetadata.TryDatasetToGUID(TableMeta.Table, GUID) then
            if IsEqualGUID(GUID, DatasetGUID) then Exit(TableMeta);
  end;
end;

function TTablesList.FindFieldByGUID(const FieldGUID: TGUID): TFieldMeta;
var
  Index: Integer;
  TableMeta: TTableMeta;
begin
  Result := nil;
  for Index := 0 to Pred(Count) do
    begin
      TableMeta := TTableMeta(inherited Items[Index]);
      if Assigned(TableMeta) then
        begin
          Result := TableMeta.Fields.FindByGUID(FieldGUID);
          if Assigned(Result) then Break;
        end;
    end;
end;

function TTablesList.IndexByName(const Name: string; const FromIndex: Integer): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := FromIndex to Pred(Count) do
    if SameText(TTableMeta(inherited Items[Index]).FTable, Name) then
      begin
        Result := Index;
        Break;
      end;
end;

{ TDeClipboard }

destructor TDeClipboard.Destroy;
var
  Index: Integer;
begin
  if Assigned(FTempList) then
  begin
    for Index := Pred(FTempList.Count) downto 0 do
      DeleteFile(FTempList[Index]);
    FreeAndNil(FTempList);
  end;
  FreeAndNil(FStackList);
  inherited Destroy;
end;

function TDeClipboard.GetInfo(var DataID: Integer; var aCount: Integer;
  var RecordID: Variant): boolean;
var
//  Strings: TStringList;
  Template: TDeTemplate;
  WorkHandle: THandle;
  DataPtr: PAnsiChar;
  Node: TDeNode;
  Index, TempID: Integer;
begin
  DataID := 0; // Unassigned;
  aCount := 0; // Unassigned;
  RecordID := unassigned;
  Result := HasFormat(DeXML); // ContainsDeData;
  if Result then
  begin
    Template := TDeTemplate.Create;
    try
      WorkHandle := GetAsHandle(DeXML);
      DataPtr := GlobalLock(WorkHandle);
      try
        Template.Text := StrPas(DataPtr);
      finally
        GlobalUnlock(WorkHandle);
      end;
      Result := Assigned(Template.Root);
      if Result then
      begin
        Node := Template.Root.FindNode('dataset');
        Result := Assigned(Node);
        if Result then
        begin
          DataID := StrToInt(Node.Attributes['id']);
          if Assigned(Node.ChildNodes) then
            for Index := 0 to Pred(Node.ChildNodes.Count) do
              if SameText(Node.ChildNodes[Index].NodeName, 'record') then
              begin
                Inc(aCount);
                if VarIsEmpty(RecordID) and
                  TryStrToInt(Node.ChildNodes[Index].Attributes['id'], TempID)
                then
                  RecordID := TempID;
              end;
        end;
      end;
    finally
      Template.Free;
    end;
  end;
end;

function TDeClipboard.GetDataID: Integer;
var
  DummyInt: Integer;
  DummyVar: Variant;
begin
  GetInfo(Result, DummyInt, DummyVar);
end;

function TDeClipboard.GetSize: Integer;
var
  DummyInt: Integer;
  DummyVar: Variant;
begin
  GetInfo(DummyInt, Result, DummyVar);
end;

function TDeClipboard.ContainsDeData: boolean;
begin
  Result := HasFormat(DeXML);
end;

function TDeClipboard.WriteFileName(const FileName: string;
  const Deleted: boolean): boolean;
var
  DropFiles: PDropFiles;
  Handle: THandle;
  Size: Integer;
begin
  Size := (Length(FileName) + 2) * SizeOf(Char);
  Handle := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT,
    SizeOf(TDropFiles) + Size);
  Result := Handle <> 0;
  if Result then
  begin
    Open;
    try
      DropFiles := GlobalLock(Handle);
      try
        DropFiles^.pFiles := SizeOf(TDropFiles);
        DropFiles^.fWide := True; // XE7
        Move(FileName[1], (PAnsiChar(DropFiles) + SizeOf(TDropFiles))^, Size);
      finally
        GlobalUnlock(Handle);
      end;
      SetAsHandle(CF_HDROP, Handle);
    finally
      Close;
    end;
  end;
  if Deleted then
  begin
    if not Assigned(FTempList) then
      FTempList := TStringList.Create;
    FTempList.Append(FileName);
  end;
end;

function TDeClipboard.ReadFileName(const FileIndex: Cardinal;
  const ForgetAll: boolean): string;
var
  Handle: THandle;
  Buffer: array [0 .. 1023] of Char;
begin
  Result := EmptyStr;
  if HasFormat(CF_HDROP) then
  begin
    Open;
    try
      Handle := GetAsHandle(CF_HDROP);
      if Handle <> 0 then
      begin
        ZeroMemory(@Buffer, SizeOf(Buffer));
        if DragQueryFile(Handle, FileIndex, Buffer, SizeOf(Buffer)) <> 0 then
          Result := StrPas(Buffer);
      end;
    finally
      Close;
    end;
  end;
  if ForgetAll then
    Clear;
end;

function TDeClipboard.WriteBitmap(Bitmap: TBitmap): boolean;
var
  Handle: THandle;
  Format: Word;
  Pallete: HPALETTE;
begin
  Result := Assigned(Bitmap);
  if Result then
  begin
    Open;
    try
      Format := CF_BITMAP;
      Bitmap.SaveToClipboardFormat(Format, Handle, Pallete);
      SetAsHandle(Format, Handle);
    finally
      Close;
    end;
  end;
end;

function TDeClipboard.ReadBitmap(Bitmap: TBitmap;
  const ForgetAll: boolean): boolean;
begin
  Result := Assigned(Bitmap) and HasFormat(CF_BITMAP);
  if Result then
  begin
    Open;
    try
      Bitmap.LoadFromClipboardFormat(CF_BITMAP, GetAsHandle(CF_BITMAP), 0);
    finally
      Close;
    end;
  end;
  if ForgetAll then
    Clear;
end;

function TDeClipboard.WriteGraphic(Graphic: TGraphic): boolean;
var
  Bitmap: TBitmap;
begin
  Result := Assigned(Graphic);
  if Result then
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.Assign(Graphic);
      Result := WriteBitmap(Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;

function TDeClipboard.ReadJPEG(JPEGImage: TJPEGImage;
  const ForgetAll: boolean): boolean;
var
  Bitmap: TBitmap;
begin
  Result := Assigned(JPEGImage);
  if Result then
  begin
    Bitmap := TBitmap.Create;
    try
      Result := ReadBitmap(Bitmap, ForgetAll);
      if Result then
        JPEGImage.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;

function TDeClipboard.ReadValue(var Value: Variant): boolean;
var
  Handle: THandle;
  ValuePtr: pointer;
  Stream: TMemoryStream;
  ValueSize: Cardinal;
  ValueType: TFieldType;
  ValueInt: Int64;
  ValueBool: boolean;
  ValueExt: Extended;
  ValueDate: TDateTime;
  ValueStr: string;
begin
  Result := HasFormat(DeValue);
  if Result then
    try
      Handle := GetAsHandle(DeValue);
      Stream := TMemoryStream.Create;
      try
        ValuePtr := GlobalLock(Handle);
        try
          Stream.WriteBuffer(ValuePtr^, GlobalSize(Handle));
        finally
          GlobalUnlock(Handle);
        end;
        Result := Stream.Size >= SizeOf(ValueType);
        if Result then
        begin
          Stream.Seek(0, soFromBeginning);
          Stream.ReadBuffer(ValueType, SizeOf(ValueType));
          case ValueType of
            ftUnknown:
              Value := Null;
            ftInteger:
              begin
                Stream.ReadBuffer(ValueInt, SizeOf(ValueInt));
                Value := ValueInt;
              end;
            ftBoolean:
              begin
                Stream.ReadBuffer(ValueBool, SizeOf(ValueBool));
                Value := ValueBool;
              end;
            ftFloat:
              begin
                Stream.ReadBuffer(ValueExt, SizeOf(ValueExt));
                Value := ValueExt;
              end;
            ftDateTime:
              begin
                Stream.ReadBuffer(ValueDate, SizeOf(ValueDate));
                Value := ValueDate;
              end;
            ftString:
              begin
                Stream.ReadBuffer(ValueSize, SizeOf(ValueSize));
                SetLength(ValueStr, ValueSize);
                if ValueSize <> 0 then
                  Stream.ReadBuffer(ValueStr[1], ValueSize * SizeOf(Char));
                Value := ValueStr;
              end;
          else
            Value := unassigned;
          end;
        end;
      finally
        Stream.Free;
      end;
    except
      on E: Exception do
      begin
{$IFDEF DEBUG}
        DebugLog(ClassName + '.ReadValue skip error: ' + E.Message);
{$ENDIF}
        Result := False;
      end;
    end;
end;

function TDeClipboard.ReadValue(var Value: Variant; const ValueType: TFieldType;
  const TextParsed: boolean): boolean;
var
  TempValue: Variant;
  TempText: string;
  ValueBool: boolean;
  ValueInt: Int64;
  ValueExt: Extended;
  ValueCur: Currency;
  ValueDate: TDateTime;
begin
  Result := ReadValue(TempValue);
  if Result then
    try
      case ValueType of
        ftString, ftWideString, ftMemo, ftWideMemo, ftFixedChar, ftFixedWideChar:
          Value := VarToStr(TempValue);
        ftSmallint:
          Value := VarAsType(TempValue, varSmallint);
        ftInteger:
          Value := VarAsType(TempValue, varInteger);
        ftWord:
          Value := VarAsType(TempValue, varWord);
        ftBoolean:
          Value := VarAsType(TempValue, varBoolean);
        ftFloat, ftExtended:
          Value := VarAsType(TempValue, varDouble);
        ftCurrency:
          Value := VarAsType(TempValue, varCurrency);
        ftDate, ftTime, ftDateTime, ftTimeStamp, ftTimeStampOffset, ftOraTimeStamp:
          Value := VarAsType(TempValue, varDate);
        ftLargeInt, ftLongWord:
          Value := VarAsType(TempValue, varInt64);
        ftShortint:
          Value := VarAsType(TempValue, varShortInt);
        ftByte:
          Value := VarAsType(TempValue, varByte);
        ftSingle:
          Value := VarAsType(TempValue, varSingle);
      else
        Result := False;
      end;
    except
      on E: Exception do
      begin
{$IFDEF DEBUG}
        DebugLog(ClassName + '.ReadValue skip error: ' + E.Message);
{$ENDIF}
        Result := False;
      end;
    end;
  if TextParsed and not Result then
    try
      TempText := AsText;
      case ValueType of
        ftString, ftWideString, ftMemo, ftWideMemo, ftFixedChar, ftFixedWideChar:
          begin
            Value := TempText;
            Result := True;
          end;
        ftByte, ftShortint, ftWord, ftSmallint, ftInteger, ftLargeInt, ftLongWord:
          if TryStrToInt64(TempText, ValueInt) then
          begin
            case ValueType of
              ftByte:     Value := VarAsType(ValueInt, varByte);
              ftShortint: Value := VarAsType(ValueInt, varShortInt);
              ftSmallint: Value := VarAsType(ValueInt, varSmallint);
              ftWord:     Value := VarAsType(ValueInt, varWord);
              ftInteger:  Value := VarAsType(ValueInt, varInteger);
            else
              Value := VarAsType(TempValue, varInt64);
            end;
            Result := True;
          end;
        ftBoolean:
          if DeTryStrToBoolean(TempText, ValueBool) then
          begin
            Result := True;
            Value := ValueBool;
          end;
        ftSingle, ftFloat, ftExtended:
          if TryStrToFloat(TempText, ValueExt) then
          begin
            case ValueType of
              ftSingle: Value := VarAsType(ValueExt, varSingle);
              ftFloat:  Value := VarAsType(ValueExt, varDouble);
            else
              Value := ValueExt;
            end;
            Result := True;
          end;
        ftCurrency:
          if TryStrToCurr(TempText, ValueCur) then
          begin
            Value := ValueCur;
            Result := True;
          end;
        ftDate:
          if TryStrToDate(TempText, ValueDate) then
          begin
            Value := ValueDate;
            Result := True;
          end;
        ftTime:
          if TryStrToTime(TempText, ValueDate) then
          begin
            Value := ValueDate;
            Result := True;
          end;
        ftDateTime:
          if TryStrToDateTime(TempText, ValueDate) then
          begin
            Value := ValueDate;
            Result := True;
          end;
      end;
    except
      on E: Exception do
      begin
{$IFDEF DEBUG}
        DebugLog(ClassName + '.ReadValue skip error: ' + E.Message);
{$ENDIF}
        Result := False;
      end;
    end;
end;

function TDeClipboard.ReadValueType(var ValueType: TFieldType): boolean;
var
  Handle: THandle;
  ValuePtr: ^TFieldType;
begin
  Result := HasFormat(DeValue);
  if Result then
    try
      Handle := GetAsHandle(DeValue);
      Result := GlobalSize(Handle) >= SizeOf(ValueType);
      if Result then
      begin
        ValuePtr := GlobalLock(Handle);
        try
          ValueType := ValuePtr^;
        finally
          GlobalUnlock(Handle);
        end;
      end;
    except
      on E: Exception do
      begin
{$IFDEF DEBUG}
        DebugLog(ClassName + '.ReadValue skip error: ' + E.Message);
{$ENDIF}
        Result := False;
      end;
    end;
end;

function TDeClipboard.WriteValue(const Value: Variant;
  const TextWrited: boolean): boolean;
var
  Stream: TMemoryStream;
  Handle: THandle;
  Buffer: pointer;
  BufferSize: Integer;
  AnsiBuffer: AnsiString;
  WideBuffer: WideString;
  LocaleData: LongInt;
  procedure WriteValueType(const ValueType: TFieldType);
  begin
    Stream.WriteBuffer(ValueType, SizeOf(ValueType));
  end;
  procedure WriteValueBool(const ValueBool: boolean);
  const
    Values: array [boolean] of Char = ('N', 'Y');
  begin
    WriteValueType(ftBoolean);
    Stream.WriteBuffer(ValueBool, SizeOf(ValueBool));
    AnsiBuffer := Values[ValueBool];
    WideBuffer := AnsiBuffer;
  end;
  procedure WriteValueDate(const ValueDate: TDateTime);
  begin
    WriteValueType(ftDateTime);
    Stream.WriteBuffer(ValueDate, SizeOf(ValueDate));
    AnsiBuffer := DateTimeToStr(ValueDate);
    WideBuffer := AnsiBuffer;
  end;
  procedure WriteValueExt(const ValueExt: Extended);
  begin
    WriteValueType(ftFloat);
    Stream.WriteBuffer(ValueExt, SizeOf(ValueExt));
    AnsiBuffer := FloatToStr(ValueExt);
    WideBuffer := AnsiBuffer;
  end;
  procedure WriteValueInt(const ValueInt: Int64);
  begin
    WriteValueType(ftInteger);
    Stream.WriteBuffer(ValueInt, SizeOf(ValueInt));
    AnsiBuffer := IntToStr(ValueInt);
    WideBuffer := AnsiBuffer;
  end;
  procedure WriteValueStr(const ValueStr: string);
  var
    ValueSize: Cardinal;
  begin
    WriteValueType(ftString);
    ValueSize := Length(ValueStr);
    Stream.WriteBuffer(ValueSize, SizeOf(ValueSize));
    if ValueSize <> 0 then
      Stream.WriteBuffer(ValueStr[1], ValueSize * SizeOf(Char));
    AnsiBuffer := ValueStr;
    WideBuffer := ValueStr;
  end;

begin
  try
    Open;
    try
      AnsiBuffer := EmptyStr;
      WideBuffer := EmptyStr;
      Stream := TMemoryStream.Create;
      try
        if VarIsEmpty(Value) then
          WriteValueType(ftObject)
        else if VarIsNull(Value) then
          WriteValueType(ftUnknown)
        else if VarIsType(Value, varBoolean) then
          WriteValueBool(Value)
        else if VarIsType(Value, varDate) then
          WriteValueDate(Value)
        else if VarIsFloat(Value) then
          WriteValueExt(Value)
        else if VarIsOrdinal(Value) then
          WriteValueInt(Value)
        else if VarIsStr(Value) then
          WriteValueStr(Value);
        Result := Stream.Size <> 0;
        if Result then
        begin
          Handle := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT,
            Stream.Size);
          Result := Handle <> 0;
          if Result then
          begin
            Buffer := GlobalLock(Handle);
            try
              Move(Stream.Memory^, Buffer^, Stream.Size);
            finally
              GlobalUnlock(Handle);
            end;
            SetAsHandle(DeValue, Handle);
          end;
        end;
      finally
        Stream.Free;
      end;
      if Result and TextWrited then
      begin
        // установка кодировки по умолчанию для CF_TEXT
        LocaleData := $0419;
        BufferSize := SizeOf(LocaleData);
        Handle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE or GMEM_ZEROINIT,
          BufferSize);
        Buffer := GlobalLock(Handle);
        try
          Move(LocaleData, Buffer^, BufferSize);
        finally
          GlobalUnlock(Handle);
        end;
        SetAsHandle(CF_LOCALE, Handle);
        // CF_TEXT для приложений DOS
        BufferSize := Succ(Length(AnsiBuffer));
        Handle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE or GMEM_ZEROINIT,
          BufferSize);
        Buffer := GlobalLock(Handle);
        try
          StrCopy(Buffer, PAnsiChar(AnsiBuffer));
        finally
          GlobalUnlock(Handle);
        end;
        SetAsHandle(CF_TEXT, Handle);
        // CF_OEMTEXT для приложений Win9x: Delphi, TEdit'ы
        Handle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE or GMEM_ZEROINIT,
          BufferSize);
        Buffer := GlobalLock(Handle);
        try
          StrCopy(Buffer, PAnsiChar(AnsiBuffer));
        finally
          GlobalUnlock(Handle);
        end;
        SetAsHandle(CF_OEMTEXT, Handle);
        // CF_UNICODETEXT для приложений WinXP: MS Office, NotePad, WordPad, ICQ, ....
        BufferSize := Succ(Length(WideBuffer)) * SizeOf(WideChar);
        Handle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE or GMEM_ZEROINIT,
          BufferSize);
        Buffer := GlobalLock(Handle);
        try
          Move(PWideChar(WideBuffer)^, Buffer^, BufferSize);
        finally
          GlobalUnlock(Handle);
        end;
        SetAsHandle(CF_UNICODETEXT, Handle);
      end;
    finally
      Close;
    end;
  except
    on E: Exception do
    begin
{$IFDEF DEBUG}
      DebugLog(ClassName + '.WriteValue skip error: ' + E.Message);
{$ENDIF}
      Result := False;
    end;
  end;
end;

function TDeClipboard.WriteValue(const Value: Variant;
  const Text: string): boolean;
var
  Handle: THandle;
  Buffer: pointer;
  BufferSize: Integer;
  AnsiBuffer: AnsiString;
  WideBuffer: WideString;
  LocaleData: LongInt;
begin
  Open;
  try
    Result := WriteValue(Value, False);
    if Result and (Length(Text) <> 0) then
      try
        AnsiBuffer := Text;
        WideBuffer := Text;
        // установка кодировки по умолчанию для CF_TEXT
        LocaleData := $0419;
        BufferSize := SizeOf(LocaleData);
        Handle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE or GMEM_ZEROINIT,
          BufferSize);
        Buffer := GlobalLock(Handle);
        try
          Move(LocaleData, Buffer^, BufferSize);
        finally
          GlobalUnlock(Handle);
        end;
        SetAsHandle(CF_LOCALE, Handle);
        // CF_TEXT для приложений DOS
        BufferSize := Succ(Length(AnsiBuffer));
        Handle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE or GMEM_ZEROINIT,
          BufferSize);
        Buffer := GlobalLock(Handle);
        try
          StrCopy(Buffer, PAnsiChar(AnsiBuffer));
        finally
          GlobalUnlock(Handle);
        end;
        SetAsHandle(CF_TEXT, Handle);
        // CF_OEMTEXT для приложений Win9x: Delphi, TEdit'ы
        Handle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE or GMEM_ZEROINIT,
          BufferSize);
        Buffer := GlobalLock(Handle);
        try
          StrCopy(Buffer, PAnsiChar(AnsiBuffer));
        finally
          GlobalUnlock(Handle);
        end;
        SetAsHandle(CF_OEMTEXT, Handle);
        // CF_UNICODETEXT для приложений WinXP: MS Office, NotePad, WordPad, ICQ, ....
        BufferSize := Succ(Length(WideBuffer)) * SizeOf(WideChar);
        Handle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE or GMEM_ZEROINIT,
          BufferSize);
        Buffer := GlobalLock(Handle);
        try
          Move(PWideChar(WideBuffer)^, Buffer^, BufferSize);
        finally
          GlobalUnlock(Handle);
        end;
        SetAsHandle(CF_UNICODETEXT, Handle);
      except
        on E: Exception do
        begin
{$IFDEF DEBUG}
          DebugLog(ClassName + '.WriteValue skip error: ' + E.Message);
{$ENDIF}
          Result := False;
        end;
      end;
  finally
    Close;
  end;
end;

function TDeClipboard.HasValueType(const ValueType: TFieldType;
  const TextParsed: boolean): boolean;
var
  DataType: TFieldType;
  TempText: string;
  ValueInt: Int64;
  ValueBool: boolean;
  ValueExt: Extended;
  ValueCur: Currency;
  ValueDate: TDateTime;
begin
  Result := ReadValueType(DataType) and (DataType = ValueType);
  if TextParsed and not Result then
    if HasFormat(CF_TEXT) then
    begin
      TempText := AsText;
      case ValueType of
        ftString, ftWideString:
          Result := True;
        ftByte, ftShortint, ftWord, ftSmallint, ftInteger, ftLargeInt, ftLongWord:
          Result := TryStrToInt64(TempText, ValueInt);
        ftBoolean:
          Result := DeTryStrToBoolean(TempText, ValueBool);
        ftSingle, ftFloat, ftExtended:
          Result := TryStrToFloat(TempText, ValueExt);
        ftCurrency:
          Result := TryStrToCurr(TempText, ValueCur);
        ftDate:
          Result := TryStrToDate(TempText, ValueDate);
        ftTime:
          Result := TryStrToTime(TempText, ValueDate);
        ftDateTime:
          Result := TryStrToDateTime(TempText, ValueDate);
      end;
    end;
end;

function TDeClipboard.HasValueType(const ValueTypes: array of TFieldType;
  const TextParsed: boolean): boolean;
var
  DataType: TFieldType;
  Index: Integer;
  TempText: string;
  ValueInt: Int64;
  ValueBool: boolean;
  ValueExt: Extended;
  ValueCur: Currency;
  ValueDate: TDateTime;
begin
  Result := False;
  if ReadValueType(DataType) then
    for Index := Low(ValueTypes) to High(ValueTypes) do
      if ValueTypes[Index] = DataType then
      begin
        Result := True;
        Break;
      end;
  if TextParsed and not Result then
    if HasFormat(CF_TEXT) then
    begin
      TempText := AsText;
      for Index := Low(ValueTypes) to High(ValueTypes) do
      begin
        case ValueTypes[Index] of
          ftString, ftWideString:
            Result := True;
          ftByte, ftShortint, ftWord, ftSmallint, ftInteger, ftLargeInt,
            ftLongWord:
            Result := TryStrToInt64(TempText, ValueInt);
          ftBoolean:
            Result := DeTryStrToBoolean(TempText, ValueBool);
          ftSingle, ftFloat, ftExtended:
            Result := TryStrToFloat(TempText, ValueExt);
          ftCurrency:
            Result := TryStrToCurr(TempText, ValueCur);
          ftDate:
            Result := TryStrToDate(TempText, ValueDate);
          ftTime:
            Result := TryStrToTime(TempText, ValueDate);
          ftDateTime:
            Result := TryStrToDateTime(TempText, ValueDate);
        end;
        if Result then
          Break;
      end;
    end;
end;

procedure TDeClipboard.SaveToStream(Stream: TStream);
var
  Writer: TWriter;
  Index: Integer;
  Handle: THandle;
  Memory: pointer;
  MemoryStream: TMemoryStream;
  Buffer: array [0 .. 255] of Char;
  FormatID: Word;
  FormatName: string;
begin
  Writer := TWriter.Create(Stream, 4096);
  try
    Open;
    try
      Writer.WriteListBegin;
      MemoryStream := TMemoryStream.Create;
      try
        for Index := 0 to Pred(FormatCount) do
        begin
          // эти типы данных в буфере не получается копировать :(
          // if Not (Clipboard.Formats[i] in [CF_BITMAP, CF_ENHMETAFILE]) then
          FormatID := Formats[Index];
          Handle := GetAsHandle(FormatID);
          if Handle <> 0 then
          begin
            MemoryStream.Size := 0;
            Memory := GlobalLock(Handle);
            try
              if Assigned(Memory) then
                MemoryStream.WriteBuffer(Memory^, GlobalSize(Handle));
            finally
              GlobalUnlock(Handle);
            end;
            if MemoryStream.Size <> 0 then
            begin
              if GetClipboardFormatName(FormatID, Buffer, SizeOf(Buffer)) = 0
              then
                FormatName := EmptyStr
              else
                FormatName := StrPas(Buffer);
              Writer.WriteInteger(FormatID);
              Writer.WriteString(FormatName);
              Writer.WriteInteger(MemoryStream.Size);
              Writer.Write(MemoryStream.Memory^, MemoryStream.Size);
            end;
          end;
        end;
      finally
        MemoryStream.Free;
      end;
      Writer.WriteListEnd;
    finally
      Close;
    end;
  finally
    Writer.Free;
  end;
end;

procedure TDeClipboard.LoadFromStream(Stream: TStream);
var
  Reader: TReader;
  MemoryStream: TMemoryStream;
  FormatID: Word;
  FormatName: string;
  Handle: THandle;
  Memory: pointer;
begin
  Reader := TReader.Create(Stream, 4096);
  try
    Open;
    try
      Clear;
      Reader.ReadListBegin;
      MemoryStream := TMemoryStream.Create;
      try
        while not Reader.EndOfList do
        begin
          FormatID := Reader.ReadInteger;
          FormatName := Reader.ReadString;
          MemoryStream.Size := Reader.ReadInteger;
          Reader.Read(MemoryStream.Memory^, MemoryStream.Size);
          if Length(FormatName) <> 0 then
            FormatID := RegisterClipboardFormat(PChar(FormatName));
          if FormatID <> 0 then
          begin
            Handle := GlobalAlloc(GHND or GMEM_DDESHARE, MemoryStream.Size);
            if Handle = 0 then
              OutOfMemoryError
            else
              try
                Memory := GlobalLock(Handle);
                try
                  if Assigned(Memory) then
                    MemoryStream.ReadBuffer(Memory^, MemoryStream.Size)
                  else
                    OutOfMemoryError;
                finally
                  GlobalUnlock(Handle);
                end;
                Open;
                try
                  SetAsHandle(FormatID, Handle);
                finally
                  Close;
                end;
              except
                GlobalFree(Handle);
                raise;
              end;
          end;
        end;
      finally
        MemoryStream.Free;
      end;
      Reader.ReadListEnd;
    finally
      Close;
    end;
  finally
    Reader.Free;
  end;
end;

function TDeClipboard.Push: boolean;
var
  Stream: TStream;
begin
  if not Assigned(FStackList) then
    FStackList := TObjectList.Create;
  Stream := TMemoryStream.Create;
  try
    SaveToStream(Stream);
    Result := FStackList.Add(Stream) <> -1;
  except
    Stream.Free;
    raise;
  end;
  if not Result then
    Stream.Free;
end;

function TDeClipboard.Pop: boolean;
begin
  Result := Assigned(FStackList) and (FStackList.Count <> 0);
  if Result then
  begin
    LoadFromStream(FStackList[Pred(FStackList.Count)] as TMemoryStream);
    FStackList.Delete(Pred(FStackList.Count));
  end;
end;

{ THistoryList }

constructor THistoryList.Create;
begin
  inherited Create;
  FCurrentIndex := -1;
end;

function THistoryList.GetItem(const Index: Integer): TContextMeta;
begin
  Result := TContextMeta(inherited Items[Index]);
end;

function THistoryList.NewItem: TContextMeta;
begin
  Result:= TContextMeta.Create(nil);
  Add(Result);
end;

procedure THistoryList.SetCurrentIndex(const aValue: Integer);
begin
  if aValue < Count then
    FCurrentIndex := aValue
  else
    raise Exception.Create('Invalid history index');
end;

function THistoryList.GetCurrentItem: TContextMeta;
begin
  if (FCurrentIndex >= 0) and (FCurrentIndex < Count) then
    Result := Items[FCurrentIndex]
  else
    Result := nil;
end;

{$IFDEF DEBUG}
procedure THistoryList.DebugHistoryLog(const Text: string);
  function PrepareHistoryLog: string;
  var
    TextTable: TTextTable;
    Index: Integer;
    HistoryData: TContextMeta;
  begin
    if Count = 0 then
      Result := EmptyStr
    else
      begin
        TextTable := TTextTable.Create;
        try
          TextTable.Columns.Add('No', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('ID', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Table', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Name', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('D', 1, taCenter, taCenter);
          for Index := 0 to Pred(Count) do
            begin
              TextTable.Lines[Index][0] := IntToStr(Succ(Index));
              HistoryData := Items[Index];
              if Assigned(HistoryData) then
                begin
                  if Assigned(HistoryData.DataSet) then
                    begin
                      TextTable.Lines[Index][1] := VarToStr(HistoryData.DataSet.ID);
                      TextTable.Lines[Index][2] := VarToStr(HistoryData.DataSet.Table);
                    end;
                  TextTable.Lines[Index][2] := HistoryData.Name;
                end;
              TextTable.Lines[Index][0] := iif(Index = CurrentIndex, '+', ' ');
            end;
          Result := #13#10 + TextTable.AsText(24);
        finally
          TextTable.Free;
        end;
      end;
  end;
begin
  DebugLog(Text + PrepareHistoryLog);
end;
{$ENDIF}

procedure THistoryList.Clear;
begin
  inherited Clear;
  FCurrentIndex := -1;
end;

{ TCounter }

type
  PCountRec = ^TCountRec;

  TCountRec = record
    DataID: Integer; // идентификатор данных
    Count: Integer; // количество ссылок
  end;

function TCounter.FindRec(const ID: Integer): pointer;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to Pred(Count) do
    if PCountRec(inherited Items[Index])^.DataID = ID then
    begin
      Result := inherited Items[Index];
      Break;
    end;
end;

function TCounter.GetCount(const ID: Integer): Integer;
var
  CountRec: PCountRec;
begin
  CountRec := PCountRec(FindRec(ID));
  if Assigned(CountRec) then
    Result := CountRec^.Count
  else
    Result := 0;
end;

procedure TCounter.SetCount(const ID: Integer; const Value: Integer);
var
  CountRec: PCountRec;
begin
  CountRec := PCountRec(FindRec(ID));
  if Assigned(CountRec) then
    CountRec^.Count := Value
  else
  begin
    CountRec := New(PCountRec);
    CountRec^.DataID := ID;
    CountRec^.Count := Value;
    if Add(CountRec) = -1 then
      Dispose(CountRec);
  end;
end;

procedure TCounter.Notify(Ptr: pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    Dispose(PCountRec(Ptr));
  inherited;
end;

procedure TCounter.IncCount(const ID: Integer; const IncValue: Integer);
begin
  Items[ID] := Items[ID] + IncValue;
end;

procedure TCounter.DecCount(const ID: Integer; const DecValue: Integer);
begin
  Items[ID] := Items[ID] - DecValue;
end;

{ === TEventMetaList ========================================================= }

constructor TEventMeta.Create(aOwner: TEventMetaList);
begin
  inherited Create;
  if aOwner <> nil then
    aOwner.Put(aOwner.Count, Self);
  FConditionPostfix := TExpressionItem.Create;
end;

destructor TEventMeta.Destroy;
begin
  inherited Destroy;
  FConditionPostfix.Free;
end;

procedure TEventMeta.Assign(Source: TObject);
begin
  if Source is TDeDataSet then
    begin
      FId := VarToInt(TDeDataSet(Source).ValueByName[fldACID]);
      FEventID := VarToInt(TDeDataSet(Source).ValueByName[fldACActionName]);
      FProcedureID := VarToInt(TDeDataSet(Source).ValueByName[fldACProcedureName]);
      FProcedureParameter := TDeDataSet(Source).StringValueByName(fldACProcParam);
      FConditionStr := TDeDataSet(Source).StringValueByName(fldACCondition);
      FConditionPostfix.Clear;
    end
  else if Source is TCacheItem then
    begin
      FId := VarToInt(TCacheItem(Source).ValueByName[fldACID]);
      FEventID := VarToInt(TCacheItem(Source).ValueByName[fldACActionName]);
      FProcedureID := VarToInt(TCacheItem(Source).ValueByName[fldACProcedureName]);
      FProcedureParameter := TCacheItem(Source).ValueNativeByName[fldACProcParam];
      FConditionStr := TCacheItem(Source).ValueNativeByName[fldACCondition];
      FConditionPostfix.Clear;
    end
  else if Source is TEventMeta then
  begin
    FId := TEventMeta(Source).ID;
    FEventID := TEventMeta(Source).EventID;
    FProcedureID := TEventMeta(Source).ProcedureID;
    FProcedureParameter := TEventMeta(Source).ProcedureParameter;
    FConditionStr := TEventMeta(Source).Condition;
    FConditionPostfix.Clear;
  end;
end;

function TEventMeta.GetEventName: string;
begin
  case FEventID of
    1:
      Result := 'OnLogin';
    2:
      Result := 'OnLogout';
    3:
      Result := 'OnInsert';
    4:
      Result := 'OnDelete';
    5:
      Result := 'OnChange';
    6:
      Result := 'OnUpdate';
  else
    Result := EmptyStr;
  end
end;

function TEventMeta.GetProcedureName: string;
begin
  case FProcedureID of
    1:
      Result := 'CreateMessage';
    2:
      Result := 'DeleteMessage';
    3:
      Result := 'UpdateMessage';
    4:
      Result := 'ShowMessage';
  else
    Result := EmptyStr;
  end
end;
{ --- TEventMetaList --------------------------------------------------------- }

constructor TEventMetaList.Create(AOwnsObjects: boolean);
begin
  inherited Create(AOwnsObjects);
end;

destructor TEventMetaList.Destroy;
begin
  inherited Destroy;
end;

function TEventMetaList.Get(const aIndex: Integer): TEventMeta;
begin
  if (0 <= aIndex) and (aIndex < Count) then
    Result := TEventMeta(inherited Items[aIndex])
  else
    Result := nil;
end;

procedure TEventMetaList.Put(const aIndex: Integer; Event: TEventMeta);
begin
  if aIndex >= Count then
    Count := Succ(aIndex);
  inherited Items[aIndex] := Event;
end;

procedure TEventMetaList.Refresh;
var
  R: Integer;
  Q: TDeDataSet;
  m: TEventMeta;
  Parser: TDeParser;
begin
  if Not Assigned(MetaData.MetaTables[idxActionConditions]) then
    Exit;
  Clear;

  Parser := TDeParser.Create;
  if FTableMetaID <> EmptyTableMetaID then
    Parser.Table := MetaData.GetTableMeta(FTableMetaID)
  else
    Parser.Table := nil;

  if FTableMetaID >= 0 then
  begin
    Q := MetaData.ActionMetaDataSet;
    Q.Descr.ParamValueByName['ID'] := FTableMetaID;
    Q.Open;
    for R:=0 to Pred(Q.RecordCount) do
      begin
        Q.RecNo:= R;
        m := TEventMeta.Create(Self);
        m.Assign(Q);
        try
          if Length(m.FConditionStr) > 0 then
            Parser.Parse(m.FConditionStr, m.FConditionPostfix)
          else
            Parser.Parse('TRUE', m.FConditionPostfix);
        except
          on E: EDeParserError do
            m.ConditionPostfix.Clear;
        end;
      end;
    Q.Close;
  end;

  Parser.Free;
end;

procedure TEventMetaList.SetTableMetaID(const Value: Integer);
begin
  if Assigned(MetaData.MetaTables[idxActionConditions]) then
  begin
    if FTableMetaID = Value then
      Exit;
    FTableMetaID := Value;
    Refresh;
  end;
end;

{ TFilterList }

function TFilterList.GetFilter(const Index: Integer): TFilterItem;
begin
  Result := TFilterItem(inherited Items[Index]);
end;

{ THierarchyManager }

constructor THierarchyManager.Create(const State: TTableFlagStateType; const Owner: TTableMeta);
begin
  inherited Create;
  FState := State;
  FOwner := Owner;
  FDataList := CreateDataList;
end;

destructor THierarchyManager.Destroy;
begin
  FDataList.Free;
  inherited Destroy;
end;

function THierarchyManager.GetParentManager: THierarchyManager;
begin
  if Assigned(FOwner) and (FOwner.GrandOwnerTable <> FOwner) then
    Result := FOwner.Filters// .GetHierarchyManager(FState)
  else
    Result := FParentManager;
end;

function THierarchyManager.GetParentCount: Integer;
var
  Man: THierarchyManager;
begin
  Man := GetParentManager;
  // DONE: Добавлено для предотвращения бесконечной рекурсии
  if (Not(Man = Self)) and Assigned(Man) then
    Result := Man.Count
  else
    Result := 0;
end;

function THierarchyManager.GetCount: Integer;
begin
  CheckOwnerState;
  Result := GetParentCount;
  if Assigned(FDataList) then
    Inc(Result, FDataList.Count);
end;

function THierarchyManager.GetItem(const Index: Integer): TObject;
var
  ParentCount: Integer;
begin
  CheckOwnerState;
  ParentCount := GetParentCount;
  Result := nil;
  if Index < ParentCount then
    Result := GetParentManager.Items[Index]
  else if Assigned(FDataList) then
    Result := FDataList[Index - ParentCount];
end;

procedure THierarchyManager.CheckOwnerState;
begin
  if Assigned(FOwner) then
    FOwner.CheckState(FState);
end;

resourcestring
  sFDataListNotCreated = 'FDataList not created';

procedure THierarchyManager.Clear;
begin
  Assert(Assigned(FDataList), sFDataListNotCreated);
  FDataList.Clear;
end;

function THierarchyManager.Add(aDataObject: TObject): Integer;
begin
  Assert(Assigned(FDataList), sFDataListNotCreated);
  // CheckOwnerState;
  Result := GetParentCount + FDataList.Add(aDataObject);
end;

function THierarchyManager.Extract(aDataObject: TObject): TObject;
begin
  Assert(Assigned(FDataList), sFDataListNotCreated);
  Result := FDataList.Extract(aDataObject);
end;

function THierarchyManager.IndexOf(aDataObject: TObject): Integer;
begin
  Assert(Assigned(FDataList), sFDataListNotCreated);
  CheckOwnerState;
  if Assigned(FOwner) and (FOwner.FOwnerTable <> FOwner) then
    Result := ParentManager.IndexOf(aDataObject)
  else
    Result := -1;
  if Result < 0 then
    Result := FDataList.IndexOf(aDataObject);
  if Result >= 0 then
    Inc(Result, GetParentCount);
end;

{ TFiltersManager }

constructor TFiltersManager.Create(aOwner: TTableMeta);
begin
  inherited Create(tsFilter, aOwner);
end;

function TFiltersManager.GetFilterItem(const Index: Integer): TFilterItem;
begin
  Result := TFilterItem(inherited Items[Index]);
end;

procedure TFiltersManager.NewFilter(aFieldMeta: TFieldMeta;
  const aOperation: TOperationType; const aValue: Variant);
var
  Fi: TFilterItem;
begin
  Fi := TFilterItem.Create;
  Fi.AddCondition(aFieldMeta, aOperation, aValue);
  Add(Fi);
end;

function TFiltersManager.CreateDataList: TObjectList;
begin
  Result := TFilterList.Create;
end;

procedure TFiltersManager.RemoveFilter(aFilter: TFilterItem);
begin
  Extract(aFilter);
end;

procedure TFiltersManager.AddFilter(aFilter: TFilterItem);
begin
  Add(aFilter);
end;

{$IFDEF DEBUG}
procedure TFiltersManager.DebugFilterLog(const Text: string);
  function PrepareFiltersLog: string;
  const
    FieldTypeNames: array [TFieldType] of PChar = ('ftUnknown', 'ftString',
      'ftSmallint', 'ftInteger', 'ftWord', 'ftBoolean', 'ftFloat', 'ftCurrency',
      'ftBCD', 'ftDate', 'ftTime', 'ftDateTime', 'ftBytes', 'ftVarBytes',
      'ftAutoInc', 'ftBlob', 'ftMemo', 'ftGraphic', 'ftFmtMemo', 'ftParadoxOle',
      'ftDBaseOle', 'ftTypedBinary', 'ftCursor', 'ftFixedChar', 'ftWideString',
      'ftLargeint', 'ftADT', 'ftArray', 'ftReference', 'ftDataSet', 'ftOraBlob',
      'ftOraClob', 'ftVariant', 'ftInterface', 'ftIDispatch', 'ftGuid',
      'ftTimeStamp', 'ftFMTBcd', 'ftFixedWideChar', 'ftWideMemo',
      'ftOraTimeStamp', 'ftOraInterval', 'ftLongWord', 'ftShortint', 'ftByte',
      'ftExtended', 'ftConnection', 'ftParams', 'ftStream', 'ftTimeStampOffset',
      'ftObject', 'ftSingle'); // 49..51
  var
    TextTable: TTextTable;
    Index, RowIndex, FilterIndex: Integer;
    FilterItem: TFilterItem;
    PostfixItem: TPostfixItem;
  begin
    if Count = 0 then
      Result := EmptyStr
    else
      begin
        TextTable := TTextTable.Create;
        try
          TextTable.Columns.Add('No', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('A', 1, taCenter, taCenter);
          TextTable.Columns.Add('Item Type', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Result Type', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Operation', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Identifier', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Value', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('CodePage', 0, taRightJustify, taCenter);
          RowIndex := 0;
          for Index := 0 to Pred(Count) do
            begin
              FilterItem := Items[Index];
              if Assigned(FilterItem) then
                begin
                  for FilterIndex := 0 to Pred(FilterItem.Count) do
                    begin
                      PostfixItem := FilterItem[FilterIndex];
                      if Assigned(PostfixItem) then
                        begin
                          TextTable.Lines[RowIndex][0] := Format('%d.%d.', [Succ(Index), Succ(FilterIndex)]);
                          TextTable.Lines[RowIndex][1] := iif(FilterItem.Active, '+', ' ');
                          TextTable.Lines[RowIndex][2] := StrPas(PostfixItemTypeNames[PostfixItem.ItemType]);
                          TextTable.Lines[RowIndex][3] := StrPas(FieldTypeNames[PostfixItem.ResultType]);
                          case PostfixItem.CodePage of
                            - 1:
                              TextTable.Lines[RowIndex][7] := 'Unknown';
                            cpNone:
                              TextTable.Lines[RowIndex][7] := 'System';
                            cp866:
                              TextTable.Lines[RowIndex][7] := 'DOS 866';
                            cpUTF8:
                              TextTable.Lines[RowIndex][7] := 'UTF-8';
                          else
                            TextTable.Lines[RowIndex][7] := IntToStr(PostfixItem.CodePage);
                          end;
                          case PostfixItem.ItemType of
                            piBinary:
                              begin
                                TextTable.Lines[RowIndex][4] := OperationLexeme[PostfixItem.BinaryOp];
                                TextTable.Lines[RowIndex][6] := VariantToString(PostfixItem.Data);
                              end;
                            piUnary:
                              begin
                                TextTable.Lines[RowIndex][4] := OperationLexeme[PostfixItem.UnaryOp];
                                TextTable.Lines[RowIndex][6] := VariantToString(PostfixItem.Data);
                              end;
                            piIdent:
                              begin
                                TextTable.Lines[RowIndex][5] := PostfixItem.Ident;
                                TextTable.Lines[RowIndex][6] := VariantToString(PostfixItem.Data);
                              end;
                            piVariable:
                              begin
                                TextTable.Lines[RowIndex][5] := PostfixItem.Variable.Name;
                                TextTable.Lines[RowIndex][6] := VariantToString(PostfixItem.Variable.Value);
                              end;
                            piParameter:
                              begin
                                TextTable.Lines[RowIndex][5] := PostfixItem.Parameter.Name;
                                TextTable.Lines[RowIndex][6] := VariantToString(PostfixItem.Parameter.Value);
                              end;
                            piFunction:
                              begin
                                TextTable.Lines[RowIndex][5] := PostfixItem.Func.Name;
                                TextTable.Lines[RowIndex][6] := StrPas(FieldTypeNames[PostfixItem.Func.DataType]);
                              end;
                            piLocal, piCommand:
                              TextTable.Lines[RowIndex][5] := PostfixItem.Name;
                          else
                            TextTable.Lines[RowIndex][6] := VariantToString(PostfixItem.Data);
                          end;
                          Inc(RowIndex);
                        end;
                    end;
                end;
            end;
          if TextTable.Lines.Count = 0 then
            Result := EmptyStr
          else
            Result := #13#10 + TextTable.AsText(24);
        finally
          TextTable.Free;
        end;
      end;
  end;
begin
  DebugLog(Text + PrepareFiltersLog);
end;
{$ENDIF}

{ TElementsManager }

constructor TElementsManager.Create;
begin
  inherited Create;
  FList:= TObjectList<TElementMeta>.Create;
end;

destructor TElementsManager.Destroy;
begin
  inherited Destroy;
  FList.Free;
end;

procedure TElementsManager.Clear;
begin
  FList.Clear;
end;

function TElementsManager.GetCount: Integer;
begin
  Result:= FList.Count;
end;

function TElementsManager.GetForm(const Index: Integer): TElementMeta;
begin
  Result := FList[Index];
end;

function TElementsManager.ElementGetVariable(const aIdent: string; var aVariable: TVariableItem; aDataObject: pointer): Boolean;
var i,P: Integer;
    aBaseName: string;
    aType: TFieldType;
    FM: TFieldMeta;
begin
  Result:= False;
  if aDataObject = nil then Exit;

  P:=Pos('.', aIdent);
  if 0<P then aBaseName:= Copy(aIdent, 1, P-1)
         else aBaseName:= aIdent;

  try
    for i:=0 to Pred(TElementMeta(aDataObject).Count) do
      begin
        if SameText(aBaseName, TElementMeta(aDataObject)[i].FNameInExpr) then
          begin
            if P = 0
              then if assigned(TElementMeta(aDataObject)[i].Field)
                     then aType:= TElementMeta(aDataObject)[i].Field.DataType
                     else aType:= ftUnknown
              else if Assigned(TElementMeta(aDataObject)[i].Field.LinkTable)
                     then
                       begin
                         FM:= TElementMeta(aDataObject)[i].Field.LinkTable.Fields.FindByName( Copy(aIdent, P+1, Maxint) );
                         if assigned(FM) then aType:= FM.FDataType
                                         else Continue;
                       end
                     else Continue;

            aVariable:= TVariableItem.Create(aType, aIdent, [amRead, amWrite]);
//          aVariable.OnGetValue:= GetFormVarValue;//GetVariableItemValue_Elements;
            Exit(True);
          end;

        Result:= ElementGetVariable(aIdent, aVariable, TElementMeta(aDataObject)[i]);
        if Result then Break;
      end
  except
    raise Exception.Create('aDataObject is not TElementMeta');
  end;
end;

procedure TElementsManager.LoadForms(const TableID: Integer);
var
  R, i: Integer;
  Q: TDeDataSet;
  CondParser: TDeParser;

  procedure PrepareCondition(aElementMeta: TElementMeta);
  var j: Integer;
  begin
    for j := 0 to Pred(aElementMeta.Count) do
      begin
        if Length(aElementMeta[j].FExprFilter) > 0 then
          try
            if Assigned(aElementMeta[j].LinkTable) then
              CondParser.Table:= aElementMeta[j].LinkTable else
            if Assigned(aElementMeta[j].Field) and Assigned(aElementMeta[j].Field.LinkTable) then
              CondParser.Table:= aElementMeta[j].Field.LinkTable else
              CondParser.Table:= FTable;

            CondParser.Parse(aElementMeta[j].FExprFilter, aElementMeta[j].FFilterPostfix);
          except
            on E: Exception do aElementMeta[j].FFilterPostfix.Clear;
          end;

        CondParser.Table := FTable;
        if Length(aElementMeta[j].FExprVisible) > 0 then
          try
            CondParser.Parse(aElementMeta[j].FExprVisible, aElementMeta[j].VisiblePostfix);
          except
            on E: Exception do aElementMeta[j].FVisiblePostfix.Clear;
          end;

        if Length(aElementMeta[j].FExprReadOnly) > 0 then
          try
            CondParser.Parse(aElementMeta[j].FExprReadOnly, aElementMeta[j].FReadOnlyPostfix);
          except
            on E: Exception do aElementMeta[j].FReadOnlyPostfix.Clear;
          end;

        if Length(aElementMeta[j].FExprValue) > 0 then
          try
            CondParser.Parse(aElementMeta[j].FExprValue, aElementMeta[j].FValuePostfix);
          except
            on E: Exception do aElementMeta[j].FValuePostfix.Clear;
          end;

        PrepareCondition(aElementMeta[j]);
      end;
  end;

begin
  // начитывание "плоского" списка элементов
  Clear;
  FTable:= MetaData.GetTableMeta(TableID);
  FRoot := TElementMeta.Create(FTable);
  FRoot.ID := null;
  FList.Add(FRoot);
    Q := MetaData.MetadataDB.CreateQuery(qtSelect);
    try
      Q.Descr.BeginUpdate;
      Q.Descr.Table:= tblElement;
      Q.Descr.AddCondition(fldElemDeleted, ftSmallint, opNE, 1);
      Q.Descr.AddCondition(fldElemDataset, ftInteger, opEQ, TableID);
      Q.Descr.AddOperation(opAnd);
      Q.Descr.AddSortField(fldElemT);
      Q.Descr.AddFields([fldElemId, fldElemType, fldElemLink, fldElemLinkField, fldElemOwner, // fldElemDeleted,
        fldElemName, fldElemField, fldElemFont, fldElemColor, fldElemVisible, fldElemReadOnly,
        fldElemAL, fldElemAR, fldElemL, fldElemR, fldElemT, fldElemH, fldElemExprName, fldElemDataSet,
        fldElemExprFilter, fldElemExprVisible, fldElemExprReadOnly, fldElemExprValue]);
      Q.Descr.EndUpdate;
      Q.Open;
      for R:=0 to Pred(Q.RecordCount) do
        begin
          Q.RecNo:= R;
          TElementMeta.Create(FTable, FRoot).Assign(Q);
        end;
      Q.Close;
    finally
      Q.Free;
    end;
  FRoot.MakeTree(FRoot.ID);

  CondParser := TDeParser.Create;
  CondParser.onGetVariable:= ElementGetVariable;
  CondParser.ObjectPointer:= FRoot;
  for i := 0 to FRoot.Count - 1 do
    begin
      FRoot[i].CorrectElements;
      PrepareCondition(FRoot[i]);
    end;
  CondParser.Free;
end;

function TElementsManager.AddBlankForm: TElementMeta;
begin
  Assert( Assigned(FTable), 'FTableMeta is unassigned');
  FTable.CheckState(tsElements);

  Result := TElementMeta.Create(FTable, FTable.Name, nil, etForm, 0, 0, 0, 0, 0, 0, 0);
  FList.Add(Result);
  Result.CreateBlankPage;
end;

function TElementsManager.AddDefaultForm: TElementMeta;
var
  I, NL, NC, NextTop: Integer;
  CurrentPage: TElementMeta;
begin
  Assert( Assigned(FTable), 'FTableMeta is unassigned');
  FTable.CheckState(tsElements);

  if FTable <> FTable.OwnerTable then
    if 0 < FTable.OwnerTable.Elements.Count then
      begin
        FTable.Elements.LoadForms(FTable.OwnerTable.ID);
        Result:= FTable.Elements[0];
        if Assigned(Result) and (Result.FElementType = etNone) and (0 < Result.Count) then Result:= Result.Items[0];

        if Result.ElementType = etForm then Exit;
      end;

  Result := AddBlankForm;
  CurrentPage := Result[0];
  NextTop := 1;
  for I := 0 to FTable.Fields.Count - 1 do
    if (fvService < FTable.Fields[I].VisibleLevel) and (not FTable.Fields[I].IsLookup) then
    if Not FTable.Fields[I].Key or (FTable.IsDirectory in [idGlobal]) then // ключевые поля показываем только в справочниках
    if Not (FTable.Fields[I] = FTable.FDField) then // поле признак удаление не показываем
      begin
        NL:= CurrentPage.Add(TElementMeta.Create(FTable, EmptyStr, FTable.Fields[I], etLabel, 0, 0, 0, 0, LabelWidth, NextTop, 3));
        NC:= CurrentPage.DesignElement(FTable.Fields[I], LabelWidth, NextTop);

        if FTable.Fields[I] = FTable.NField then
          begin
            CurrentPage.Items[NL].FontCode:= 1;
            CurrentPage.Items[NC].FontCode:= 1;
          end;
        Inc( NextTop, CurrentPage.Items[NC].EH );
      end;
  CurrentPage.EH := NextTop + 5;
  Result.EH := Min(36, Max(Result.EH, CurrentPage.EH));
//  Result.SaveElements;
end;

procedure TElementsManager.DeleteForm(aForm: TElementMeta);
var
  FormIndex: Integer;
begin
  Assert( Assigned(FTable), 'FTableMeta is unassigned');
  FTable.CheckState(tsElements);

  FormIndex := FList.IndexOf(aForm);
  if 0<=FormIndex then
    begin
      Items[FormIndex].RecursiveSetChanged(mcDelete);
      Items[FormIndex].SaveElements;
      FList.Delete(FormIndex);
    end;
end;

{ TColumnList }

constructor TColumnList.Create;
begin
  inherited Create(True);
end;

function TColumnList.FullWidth: Integer;
var i: Integer;
begin
  Result:= 0;
  for i:= 0 to Pred(Count) do
    Inc(Result, (Items[i] as TColumnItem).FField.Width );
end;

function TColumnList.GetItem(const Index: Integer): TColumnItem;
begin
  Result := inherited Items[Index] as TColumnItem;
end;

function TColumnList.LoadFromUserDataset(const TableID, UserID: Integer): Boolean;
begin
  // Пока не реализовано! Нужно для загрузки JOIN полей ...
  Result := False;
end;

procedure TColumnList.SaveToUserDataset(const TableID, UserID: Integer);
begin
  // Пока не реализовано! Нужно для сохранения JOIN полей ...
end;

procedure TColumnList.SetItem(const Index: Integer; const Value: TColumnItem);
begin
  if Index >= Count then
    Count := Succ(Index);
  inherited Items[Index] := Value;
end;

function TColumnList.IndexByFieldId(aID, aDefaultValue: Integer): Integer;
var i: Integer;
begin
  Result:= aDefaultValue;
  for i:=Pred(Count) downto 0 do
    if Items[i].Field.ID = aID then Exit(i);
end;

function TColumnList.Add(const aList: TFieldsMeta; const aFieldMeta: TFieldMeta): Integer;
begin
  Result:= Add(aList, aList.IndexOf(aFieldMeta));
end;

function TColumnList.Add(const aList: TFieldsMeta; const aIndex: Integer): Integer;
var
  Column: TColumnItem;
begin
  Result := inherited Add(TColumnItem.Create);
  Column:= Items[Result];

  Column.FField := aList[aIndex];
  if not Assigned(Column.FField.LookupPair) then
    begin
      Column.FValueIndex := aIndex;
      Column.FTextIndex := aIndex;
    end else
  if Column.FField.IsLookup then
    begin
      Column.FValueIndex := aList.IndexOf(Column.FField.LookupPair);
      Column.FTextIndex := aIndex;
    end
  else
    begin
      Column.FValueIndex := aIndex;
      Column.FTextIndex := aList.IndexOf(Column.FField.LookupPair);
    end;
end;

procedure TColumnList.InitColumnList(Fields: TFieldsMeta);
var
  i: Integer;
begin
  Clear;
  for i:= 0 to Pred(Fields.Count) do Add(Fields, i);
  SortByOrder;
end;

procedure TColumnList.SortByOrder;
var
  N: Integer;
  procedure QuickSort(const l, r: Integer);
  var
    I, j, m: Integer;
    Fi, Fj, Fm: Variant;
    Gi, Gj, Gm: Variant;
  begin
    I := l;
    j := r;
    m := (r + l) div 2;
    Fm := Items[m].Field.Order;
    Gm := Items[m].Field.ID;
    repeat
      Fi := Items[I].Field.Order;
      Gi := Items[I].Field.ID;
      while (Fi < Fm) or ((Fi = Fm) and ((Gi < Gm))) do
      begin
        Inc(I);
        Fi := Items[I].Field.Order;
        Gi := Items[I].Field.ID;
      end;
      Fj := Items[j].Field.Order;
      Gj := Items[j].Field.ID;
      while (Fm < Fj) or ((Fj = Fm) and ((Gm < Gj))) do
      begin
        Dec(j);
        Fj := Items[j].Field.Order;
        Gj := Items[j].Field.ID;
      end;
      if (I <= j) then
      begin
        if (Fi > Fj) or ((Fi = Fm) and ((Gi > Gj))) then
          Exchange(I, j);
        Inc(I);
        Dec(j);
      end;
    until I > j;
    if l < j then
      QuickSort(l, j);
    if I < r then
      QuickSort(I, r);
  end;

begin
  N := Count - 1;
  while (0 <= N) and Items[N].Field.IsLookup do
    Dec(N);
  if N > 0 then
    QuickSort(0, N);
end;

{$IFDEF DEBUG}

procedure TColumnList.DebugColumnsLog(const Text: string);
  function PrepareColumnsLog: string;
  const
    VisibleNames: array [TFieldVisible] of PChar = ('fvService', 'fvLevel1',
      'fvLevel2', 'fvLevel3', 'fvLevel4');
  var
    Index: Integer;
    Column: TColumnItem;
    Value: string;
  begin
    Result := EmptyStr;
    for Index := 0 to Pred(Count) do
    begin
      Result := Result +
        Format(#13#10'                        | %2d. |', [Index]);
      Column := Items[Index];
      if Assigned(Column) then
      begin
        Result := Result + Format(' %9d |', [Column.ValueIndex]);
        if Assigned(Column.Field) then
          Result := Result + Format(' %9d | %-96s | %-12s |',
            [Column.Field.Order, Column.Field.Original,
            StrPas(VisibleNames[Column.Field.VisibleLevel])])
        else
          Result := Result + Format('%11s|%98s|%14s|', [' ', ' ', ' ']);
      end
      else
        Result := Result + Format('%11s|%11s|%98s|%14s|', [' ', ' ', ' ', ' ']);
    end;
    if Length(Result) <> 0 then
    begin
      Value := Format(#13#10'                        +-----+%s+%s+%s+%s+',
        [DupeString('-', 11), DupeString('-', 11), DupeString('-', 98),
        DupeString('-', 14)]);
      Result := Value +
        Format(#13#10'                        | %-3s | %-9s | %-9s | %-96s | %-12s |',
        ['No', 'Original', 'Order', 'Field Name', 'Visible']) + Value +
        Result + Value;
    end;
  end;

begin
  DebugLog(Text + PrepareColumnsLog);
end;
{$ENDIF}

{ TGroupFilterItem }

constructor TGroupFilterItem.Create(aOwner: TGroupFilter);
begin
  FOwner := aOwner;
  FVariables := TVariableList.Create;
end;

destructor TGroupFilterItem.Destroy;
begin
  FreeAndNil(FFilterItem);
  FVariables.Free;
  inherited Destroy;
end;

procedure TGroupFilterItem.Assign(Source: TObject);
var
  Dataset: TDeDataSet;
begin
  if Assigned(Source) and (Source is TDeDataSet) then
  begin
    Dataset := Source as TDeDataSet;
    FId := Dataset.ValueByName[fldUDataSetID];
    FTypeID := Dataset.ValueByName[fldUDataSetType];
    FDataSet := Dataset.ValueByName[fldUDataSetTable];
    FFilter := Dataset.ValueByName[fldUDataSetXML];
  end;
end;

function TGroupFilterItem.GetActive: boolean;
begin
  Result := ActiveRestrict in [orNone, orEnabled];
end;

procedure TGroupFilterItem.SetActive(const Value: boolean);
begin
  if Active <> Value then
  begin
    FActiveLoaded := True;
    // Если не начитывалось, то и не надо начитывать, т.к. пользователь сам выбрал активность!
    if Value then
      FActiveRestrict := orEnabled
    else
      FActiveRestrict := orDisabled;
    FilterItem.Active := Value; // FActiveRestrict = orEnabled;
  end;
end;

function TGroupFilterItem.GetActiveRestrict: TObjectRestrict;
begin
  if not FActiveLoaded then
  begin
    FActiveRestrict := SecuritySystem.GetObjectPolicy
      (MetaData.MetaTables[idxUserDataset].ID, FId, spExecute);
    FActiveLoaded := True;
  end;
  Result := FActiveRestrict;
end;

function TGroupFilterItem.GetVisible: boolean;
begin
  Result := VisibleRestrict in [orNone, orEnabled];
end;

function TGroupFilterItem.GetVisibleRestrict: TObjectRestrict;
begin
  if not FVisibleLoaded then
  begin
    FVisibleRestrict := SecuritySystem.GetObjectPolicy
      (MetaData.MetaTables[idxUserDataset].ID, FId, spSelect);
    FVisibleLoaded := True;
  end;
  Result := FVisibleRestrict;
end;

function TGroupFilterItem.GetTableMeta: TTableMeta;
begin
  Result := MetaData.GetTableMeta(FDataSet);
end;

function TGroupFilterItem.GetFilterItem: TFilterItem;
var
  Parser: TDeParser;
  Index, FilterIndex: Integer;
  VariableBaseName, VariableName: string;
  VariableItem: TVariableItem;
begin
  if not Assigned(FFilterItem) then
  begin
    FFilterItem := TFilterItem.Create;
    Parser := TDeParser.Create;
    try
      Parser.Table := TableMeta;
      try
        Parser.Parse(FFilter, FFilterItem);
      except
        on E: EDeParserError do
        begin
          FFilterItem.Clear;
{$IFDEF DEBUG}
          if Assigned(TableMeta) then
            DebugLog('%s.GetFilterItem for %s skip error: %s', [ClassName, QuotedStr(TableMeta.Table), E.Message])
          else
            DebugLog(ClassName + '.GetFilterItem skip error: ' + E.Message);
{$ENDIF}
        end;
      end;
    finally
      Parser.Free;
    end;
    for Index := 0 to Pred(FilterItem.Count) do
      if (FilterItem[Index].ItemType = piConst) and
        not(VarIsArray(FilterItem[Index].Data) or
        VarIsNull(FilterItem[Index].Data)) then
      begin
        if IsGlobal then
          VariableBaseName := 'GlobalFilter'
        else
          VariableBaseName := 'LocalFilter';
        FilterIndex := Owner.Owner.PrepareVariableIndex(Dataset, IsGlobal);
        VariableName := Format('%s_%u', [VariableBaseName, FilterIndex]);
        while Assigned(Variables.FindVariable(VariableName)) do
        begin
          FilterIndex := Owner.Owner.PrepareVariableIndex(Dataset, IsGlobal);
          VariableName := Format('%s_%u', [VariableBaseName, FilterIndex]);
        end;
        VariableItem := TVariableItem.Create(FilterItem[Index].ResultType,
          VariableName, [amRead, amWrite]);
        Variables.Add(VariableItem);
        VariableItem.Value := FilterItem[Index].Data;
        FilterItem[Index].Parameter := VariableItem;
      end;
    FFilterItem.Active := Active;
  end;
  Result := FFilterItem;
end;

function TGroupFilterItem.IsGlobal: boolean;
begin
  Result := FTypeID = 5;
end;

{ TGroupFilter }

constructor TGroupFilter.Create(aOwner: TGroupFilters);
begin
  FOwner := aOwner;
end;

function TGroupFilter.GetItem(const Index: Integer): TGroupFilterItem;
begin
  Result := inherited Items[Index] as TGroupFilterItem;
end;

procedure TGroupFilter.SetItem(const Index: Integer;
  const Value: TGroupFilterItem);
begin
  if Index >= Count then
    Count := Succ(Index);
  inherited Items[Index] := Value;
end;

function TGroupFilter.GetActive: boolean;
begin
  Result := ActiveRestrict in [orNone, orEnabled];
end;

procedure TGroupFilter.SetActive(const Value: boolean);
var
  Index: Integer;
begin
  for Index := 0 to Pred(Count) do
    Items[Index].Active := Value;
end;

function TGroupFilter.GetActiveRestrict: TObjectRestrict;
var
  Index, EnabledCount: Integer;
begin
  Result := orNone;
  EnabledCount := 0;
  for Index := 0 to Pred(Count) do
    case Items[Index].ActiveRestrict of
      orDisabled: { Запрещён }
        begin
          // Считать не активным если хотя бы один фильтр запрещён!
          Result := orDisabled;
          Break;
        end;
      orEnabled: { Разрешён }
        Inc(EnabledCount);
    end;
  // Если нет запрещённых и все разрешены, то ...
  if (Result = orNone) and (EnabledCount = Count) then
    Result := orEnabled;
end;

function TGroupFilter.GetVisible: boolean;
begin
  Result := VisibleRestrict in [orNone, orEnabled];
  // Считать видимым если фильтр видим или не определён!
end;

function TGroupFilter.GetVisibleRestrict: TObjectRestrict;
var
  Index: Integer;
begin
  Result := orDisabled;
  for Index := 0 to Pred(Count) do
    if Items[Index].VisibleRestrict in [orNone, orEnabled] then
    begin
      // Считать видимым если хотя бы один фильтр видим или не определён!
      Result := orEnabled;
      Break;
    end;
end;

procedure TGroupFilter.LoadFilters(TableMeta: TTableMeta);
var
  Index: Integer;
  FilterItem: TFilterItem;
begin
  if Assigned(TableMeta) then
    for Index := 0 to Pred(Count) do
      if TableMeta.ID = Items[Index].Dataset then
      begin
        FilterItem := Items[Index].FilterItem;
        if Assigned(FilterItem) then
          TableMeta.LinkFilters.Add(FilterItem);
      end;
end;

procedure TGroupFilter.UnloadFilters(TableMeta: TTableMeta);
var
  Index: Integer;
begin
  if Assigned(TableMeta) then
    for Index := 0 to Pred(Count) do
      if TableMeta.ID = Items[Index].Dataset then
        if Assigned(Items[Index].FFilterItem) then
          TableMeta.LinkFilters.Remove(Items[Index].FilterItem);
end;

function TGroupFilter.IsGlobal: boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to Pred(Count) do
    if Items[Index].IsGlobal then
    begin
      Result := True;
      Break;
    end;
end;

function TGroupFilter.IsLocal(const DatasetId: Integer): boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to Pred(Count) do
    if (Items[Index].Dataset = DatasetId) and Items[Index].Visible and
      not Items[Index].IsGlobal then
    begin
      Result := True;
      Break;
    end;
end;

procedure TGroupFilter.UpdateFilters;
var
  Strings: TStrings;
  Index, TableID: Integer;
begin
  if Assigned(MetaData) then
  begin
    Strings := TStringList.Create;
    try
      for Index := 0 to Pred(Count) do
      begin
        TableID := Items[Index].Dataset;
        if Strings.IndexOf(IntToStr(TableID)) = -1 then
        begin
          MetaData.UpdateDataSets(TableID, mcUpdate, Null);
          MetaData.UpdateLibrary(TableID);
          Strings.Append(IntToStr(TableID));
        end;
      end;
    finally
      Strings.Free;
    end;
  end;

end;

function TGroupFilter.IdenticalTo(FilterItem: TFilterItem): boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to Pred(Count) do
    if FilterItem.IdenticalTo(Items[Index].FilterItem) then
    begin
      Result := True;
      Break;
    end;
end;

{ TGroupFilters }

constructor TGroupFilters.Create;
begin
  FList := TStringList.Create;
end;

destructor TGroupFilters.Destroy;
begin
  Clear;
  FList.Free;
  FDatasetIndexes.Free;
  inherited Destroy;
end;

function TGroupFilters.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TGroupFilters.GetNames(const Index: Integer): string;
begin
  Result := FList[Index];
end;

function TGroupFilters.GetItem(const Index: Integer): TGroupFilter;
begin
  Result := FList.Objects[Index] as TGroupFilter;
end;

function TGroupFilters.IndexOf(const Name: string): Integer;
begin
  Result := FList.IndexOf(Name);
end;

procedure TGroupFilters.Clear;
begin
  while FList.Count <> 0 do
  begin
    FList.Objects[0].Free;
    FList.Delete(0);
  end;
end;

procedure TGroupFilters.LoadFromDatabase;
var
  Query: TDeDataSet;
  Name: string;
  R, Index, NewIndex: Integer;
  GroupFilter: TGroupFilter;
  FilterItem: TGroupFilterItem;
begin
{$IFDEF DEBUG}
  DebugLog(ClassName + '.LoadFromDatabase start ...');
{$ENDIF}
  Clear;
  if Assigned(MetaData.MetaTables[idxUserDataset]) then
  begin
    Query := MetaData.MetadataDB.CreateQuery(qtSelect);
    try
      Query.Descr.BeginUpdate;
      try
        Query.Descr.Table := tblUserDataSet;
        Query.Descr.AddCondition(fldUDataSetSubject, ftSmallint, opIs, Null);
        Query.Descr.AddCondition(fldUDataSetType, ftSmallint, opEQ, 4);
        Query.Descr.AddCondition(fldUDataSetType, ftSmallint, opEQ, 5);
        Query.Descr.AddOperation(opOR);
        Query.Descr.AddOperation(opAnd);
        Query.Descr.AddSortFields([fldUDataSetTable, fldUDataSetOrder]);
        Query.Descr.AddFields([fldUDataSetID, fldUDataSetType, fldUDataSetTable, fldUDataSetName, fldUDataSetXML]);
      finally
        Query.Descr.EndUpdate;
      end;

      try
        Query.Open;
      except
        on E: Exception do
          begin
            {$IFDEF DeDEBUG}
            Funcs.WriteLog('Load group filters from database skip error: ' + E.Message);
            {$ENDIF}
            {$IFDEF DEBUG}
            DebugLog(ClassName + '.LoadFromDatabase skip error: ' + E.Message);
            {$ENDIF}
          end;
      end;

      for R:=0 to Pred(Query.RecordCount) do
        begin
          Query.RecNo:= R;
          Name := Query.StringValueByName(fldUDataSetName);
          Index := IndexOf(Name);
          if Index = -1 then
          begin
            GroupFilter := TGroupFilter.Create(Self);
            Index := FList.AddObject(Name, GroupFilter);
            if Index = -1 then
              GroupFilter.Free;
          end;
          if Index <> -1 then
          begin
            NewIndex := -1;
            FilterItem := TGroupFilterItem.Create(Items[Index]);
            try
              FilterItem.Assign(Query);
              NewIndex := Items[Index].Add(FilterItem);
            finally
              if NewIndex = -1 then
                FilterItem.Free;
            end;
          end;
        end;
    finally
      Query.Free;
    end;
  end;
{$IFDEF DEBUG}
  DebugLog(ClassName + '.LoadFromDatabase finish ...');
{$ENDIF}
end;

procedure TGroupFilters.LoadFilters(TableMeta: TTableMeta);
var
  Index: Integer;
begin
  if Assigned(TableMeta) then
    for Index := 0 to Pred(Count) do
      Items[Index].LoadFilters(TableMeta);
end;

procedure TGroupFilters.UnloadFilters(TableMeta: TTableMeta);
var
  Index: Integer;
begin
  if Assigned(TableMeta) then
    for Index := 0 to Pred(Count) do
      Items[Index].UnloadFilters(TableMeta);
end;

function TGroupFilters.IdenticalTo(FilterItem: TFilterItem): boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to Pred(Count) do
    if Items[Index].IdenticalTo(FilterItem) then
    begin
      Result := True;
      Break;
    end;
end;

function TGroupFilters.PrepareVariableIndex(const DatasetId: Integer;
  const Global: boolean): Integer;
const
  Names: array [boolean] of Char = ('L', 'G');
var
  Name, Value: string;
begin
  if not Assigned(FDatasetIndexes) then
    FDatasetIndexes := TStringList.Create;
  Name := Format('%s%d', [Names[Global], DatasetId]);
  Value := FDatasetIndexes.Values[Name];
  if TryStrToInt(Value, Result) then
    Inc(Result)
  else
    Result := 1;
  FDatasetIndexes.Values[Name] := IntToStr(Result);
end;

{ TMetaAction }

constructor TMetaAction.Create(AOwner: TComponent);
begin
  inherited;
end;

constructor TMetaAction.Create(AOwner: TComponent; const aCategory: string; aObject: TObject; aEvent: TNotifyEvent; const Items: array of TMenuitem);
var
  i: Integer;
  M: TMenuItem;
begin
  inherited Create(AOwner);
  Category:= aCategory;
  OnExecute:= aEvent;

  if aObject is TTableMeta then
    begin
      FFieldMeta:= nil;
      FTableMeta:= TTableMeta(aObject);
      Caption:= GetTitle(FTableMeta.Name);
      Hint:= Caption+' ['+FTableMeta.Table+']';
      ImageIndex:= DM.MapIconIndex(FTableMeta.Ico);
    end else

  if aObject is TFieldMeta then
    begin
      FFieldMeta:= TFieldMeta(aObject);
      FTableMeta:= FFieldMeta.FOwner;
      if assigned(FFieldMeta) then
        if assigned(FFieldMeta.LinkTable) then
          ImageIndex:= DM.MapIconIndex(FFieldMeta.LinkTable.Ico);

      Caption:= GetTitle(FFieldMeta.Name);
      Hint:= Caption+' ['+FFieldMeta.Original+']';
      ImageIndex:= -1;
    end else

    begin
      FFieldMeta:= nil;
      FTableMeta:= nil;
      Caption:= GetTitle('_dL.nO');
      Hint:= EmptyStr;
      ImageIndex:= -1;
    end;

  FMenuItems := TObjectList<TMenuItem>.Create(True);
  for i := Low(Items) to High(Items) do
    begin
      M:= TMenuItem.Create(nil);
      M.Action:= self;
      Items[i].Add(M);
      FMenuItems.Add(M);
      M.RadioItem:= SameText(Category, CategorySort) or
                    SameText(Category, CategorySplit) or
                    SameText(Category, CategoryMetrics);

      Items[i].Enabled:= True;
      Items[i].Visible:= True;
    end;
end;

destructor TMetaAction.Destroy;
begin
  FMenuItems.Free;
  inherited;
end;

procedure TMetaAction.SetChecked(Value: Boolean);
var i: Integer;
begin
  inherited;
  if Value and SameText(Category, CategorySplit) then
    for i:= 0 to Pred(ActionList.ActionCount) do
      if (ActionList[i] <> self) and SameText(Category, ActionList[i].Category) then
        ActionList[i].Checked:= False;
end;

{ TUserValue }

procedure TUserValue.Assign(Source: TObject);
var
  DeSource: TDeDataSet;
begin
  if Source is TDeDataSet then
  begin
    DeSource := TDeDataSet(Source);

    FID:= DeSource.IntValueByNameDef(fldUDataSetID);
    FDataSetID:= DeSource.IntValueByNameDef(fldUDataSetTable);
    FSubject:= DeSource.IntValueByNameDef(fldUDataSetSubject);
    FType:= DeSource.IntValueByNameDef(fldUDataSetType);
    FName:= DeSource.StringValueByName(fldUDataSetName);
    FXML:= DeSource.StringValueByName(fldUDataSetXML);
    FOrder:= DeSource.IntValueByNameDef(fldUDataSetOrder);
    try
      // поле отсутствует в старых метаструктурах
      FPublic:= DeSource.IntValueByNameDef(fldUDataSetPublic);
    except
      on E: Exception do
        begin
          FPublic:= 0;
          {$IFDEF DEBUG}
          DebugLog(ClassName + '.Assign field ' + QuotedStr(fldUDataSetPublic) + ' not found skip error: ' + E.Message);
          {$ENDIF}
        end;
    end;
  end

  else if Source is TCacheItem then
    begin
      FID:= VarToInt(TCacheItem(Source).ValueByName[fldUDataSetID]);
      FDataSetID:= VarToInt(TCacheItem(Source).ValueByName[fldUDataSetTable]);
      FSubject:= VarToInt(TCacheItem(Source).ValueByName[fldUDataSetSubject]);
      FType:= VarToInt(TCacheItem(Source).ValueByName[fldUDataSetType]);
      FOrder:= VarToInt(TCacheItem(Source).ValueByName[fldUDataSetOrder]);
      FName:= TCacheItem(Source).ValueNativeByName[fldUDataSetName];
      FXML:= TCacheItem(Source).ValueNativeByName[fldUDataSetXML];
      FPublic:= DeSource.ValueByNameDef(fldUDataSetPublic, 0);
    end

  else if Source is TUserValue then
  begin
    FID:= TUserValue(Source).ID;
    FDataSetID:= TUserValue(Source).DataSetID;
    FSubject:= TUserValue(Source).Subject;
    FType:= TUserValue(Source).TypeValue;
    FName:= TUserValue(Source).Name;
    FXML:= TUserValue(Source).XML;
    FOrder:= TUserValue(Source).Order;
    FPublic:= TUserValue(Source).PublicValue;
  end;
end;

function TUserValue.DB_insert: Boolean;
var FQuery: TDeDataSet;
    FM: TFieldMeta;
begin
  Result:= False;
  FQuery:= MetaData.MetadataDB.CreateQuery(qtSelect);
  try
    FQuery.Descr.BeginUpdate;
    FQuery.Descr.Table:= tblUserDataSet;
    FQuery.Descr.AddField(opMax, fldUDataSetID);
    FQuery.Descr.EndUpdate;
    FQuery.Open;
    if 0 < Integer(FQuery.Value[0]) then FID:= Succ(Integer(FQuery.Value[0]))
                                    else FID:= 1;
    FQuery.Close;
  finally
    FreeAndNil(FQuery);
  end;

  FQuery:= MetaData.MetadataDB.CreateQuery(qtInsert);
  try
    FQuery.Descr.BeginUpdate;
    FQuery.Descr.Table:= tblUserDataSet;
    FQuery.Descr.AddParamField(fldUDataSetID, ftInteger);
    FQuery.Descr.AddParamField(fldUDataSetTable, ftInteger);
    FQuery.Descr.AddParamField(fldUDataSetSubject, ftInteger);
    FQuery.Descr.AddParamField(fldUDataSetType, ftInteger);
    FQuery.Descr.AddParamField(fldUDataSetName, ftString);
    FQuery.Descr.AddParamField(fldUDataSetXML, ftString);
    FQuery.Descr.AddParamField(fldUDataSetOrder, ftInteger);
    FQuery.Descr.AddParamField(fldUDataSetPublic, ftInteger);
    FQuery.Descr.EndUpdate;

    FQuery.Descr.ParamValueByName[fldUDataSetID]:= FID;
    FQuery.Descr.ParamValueByName[fldUDataSetTable]:= FDataSetID;
    FQuery.Descr.ParamValueByName[fldUDataSetSubject]:= FSubject;
    FQuery.Descr.ParamValueByName[fldUDataSetType]:= FType;

    FM:= MetaData.GetSystemTableByName(tblUserDataSet).Fields.FindByName(fldUDataSetName);
    if assigned(FM) and (FM.CodePage = cpUTF8)
      then FQuery.Descr.ParamValueByName[fldUDataSetName]:= WideStringToUnicode(FName)
      else FQuery.Descr.ParamValueByName[fldUDataSetName]:= FName;

    FM:= MetaData.GetSystemTableByName(tblUserDataSet).Fields.FindByName(fldUDataSetXML);
    if assigned(FM) and (FM.CodePage = cpUTF8)
      then FQuery.Descr.ParamValueByName[fldUDataSetXML]:= WideStringToUnicode(FXML)
      else FQuery.Descr.ParamValueByName[fldUDataSetXML]:= FXML;

    FQuery.Descr.ParamValueByName[fldUDataSetOrder]:= FOrder;
    FQuery.Descr.ParamValueByName[fldUDataSetPublic]:= FPublic;

    Result:= FQuery.ExecuteQuery;
    FStatus:= [];
  finally
    FreeAndNil(FQuery);
  end;
end;

function TUserValue.DB_update: Boolean;
var FQuery: TDeDataSet;
    FM: TFieldMeta;
begin
  Result:= False;
  FQuery:= MetaData.MetadataDB.CreateQuery(qtUpdate);
  try
    FQuery.Descr.BeginUpdate;
    FQuery.Descr.Table:= tblUserDataSet;
    FQuery.Descr.AddCondition(fldUDataSetID, ftInteger, opEQ, FID);
    FQuery.Descr.AddParamField(fldUDataSetTable, ftInteger);
    FQuery.Descr.AddParamField(fldUDataSetSubject, ftInteger);
    FQuery.Descr.AddParamField(fldUDataSetType, ftInteger);
    FQuery.Descr.AddParamField(fldUDataSetName, ftString);
    FQuery.Descr.AddParamField(fldUDataSetXML, ftString);
    FQuery.Descr.AddParamField(fldUDataSetOrder, ftInteger);
    FQuery.Descr.AddParamField(fldUDataSetPublic, ftInteger);
    FQuery.Descr.EndUpdate;

    FQuery.Descr.ParamValueByName[fldUDataSetTable]:= FDataSetID;
    FQuery.Descr.ParamValueByName[fldUDataSetSubject]:= FSubject;
    FQuery.Descr.ParamValueByName[fldUDataSetType]:= FType;

    FM:= MetaData.GetSystemTableByName(tblUserDataSet).Fields.FindByName(fldUDataSetName);
    if assigned(FM) and (FM.CodePage = cpUTF8)
      then FQuery.Descr.ParamValueByName[fldUDataSetName]:= WideStringToUnicode(FName)
      else FQuery.Descr.ParamValueByName[fldUDataSetName]:= FName;

    FM:= MetaData.GetSystemTableByName(tblUserDataSet).Fields.FindByName(fldUDataSetXML);
    if assigned(FM) and (FM.CodePage = cpUTF8)
      then FQuery.Descr.ParamValueByName[fldUDataSetXML]:= WideStringToUnicode(FXML)
      else FQuery.Descr.ParamValueByName[fldUDataSetXML]:= FXML;

    FQuery.Descr.ParamValueByName[fldUDataSetOrder]:= FOrder;
    FQuery.Descr.ParamValueByName[fldUDataSetPublic]:= FPublic;

    Result:= FQuery.ExecuteQuery;
    FStatus:= [];
  finally
    FreeAndNil(FQuery);
  end;
end;

function TUserValue.DB_delete: Boolean;
var FQuery: TDeDataSet;
begin
  Result:= False;
  FQuery:= MetaData.MetadataDB.CreateQuery(qtDelete);
  try
    FQuery.Descr.Table:= tblUserDataSet;
    FQuery.Descr.AddCondition(fldUDataSetID, ftInteger, opEQ, FID);
    Result:= FQuery.ExecuteQuery;
    FStatus:= [];
  finally
    FreeAndNil(FQuery);
  end;
end;

function TUserValue.Commit: Boolean;
begin
  if (mcInsert in Status) and Not (mcDelete in Status) then Result:= DB_insert else
  if (mcUpdate in Status) and Not (mcDelete in Status) then Result:= DB_update else
  if (mcDelete in Status) and Not (mcInsert in Status) then Result:= DB_delete else
  Result:= True;
end;

procedure TUserValue.SetName(const aValue: string);
begin
  if Not SameText(FName, aValue) then
    begin
      FName:= aValue;
      include(FStatus, mcUpdate);
    end;
end;

procedure TUserValue.SetXML(const aValue: string);
begin
  if Not SameText(FXML, aValue) then
    begin
      FXML:= aValue;
      include(FStatus, mcUpdate);
    end;
end;

procedure TUserValue.SetSubject(const aValue: Integer);
begin
  if not (FSubject = aValue) then
    begin
      FSubject := aValue;
      include(FStatus, mcUpdate);
    end;
end;

procedure TUserValue.SetType(const aValue: Integer);
begin
  if not (FType = aValue) then
    begin
      FType:= aValue;
      include(FStatus, mcUpdate);
    end;
end;

{ TUserValues }

function TUserValues.Get(const Index: Integer): TUserValue;
begin
  Result := TUserValue(inherited Items[Index])
end;

procedure TUserValues.Put(const Index: Integer; Field: TUserValue);
begin
  if Index >= Count then
    Count := Succ(Index);
  inherited Items[Index] := Field;
end;

function TUserValues.GetFirstIndex(const aType: Integer; var aIndex: Integer; const IgnoreDeleted: Boolean): Boolean;
begin
  aIndex:= -1;
  Result:= GetNextIndex(aType, aIndex, IgnoreDeleted);
end;

function TUserValues.GetNextIndex(const aType: Integer; var aIndex: Integer; const IgnoreDeleted: Boolean): Boolean;
var i: Integer;
begin
  Result:= False;
  for i:= Succ(aIndex) to Pred(Count) do
    if TUserValue(inherited Items[i]).TypeValue = aType then
      if Not IgnoreDeleted or Not(mcDelete in TUserValue(inherited Items[i]).Status) then
        begin
          aIndex:= i;
          Exit(True);
        end;
end;

function TUserValues.GetIndexByID(const aID: Integer; var aIndex: Integer): Boolean;
var i: Integer;
begin
  for i:= 0 to Pred(Count) do
    if TUserValue(inherited Items[i]).ID = aID then
      begin
        aIndex:= i;
        Exit(True);
      end;

  aIndex:= -1;
  Result:= False;
end;

function TUserValues.New(const aType, aDataSet, aSubject: Integer; aName, aXML: string; const aOrder, aPublic: Integer): Integer;
var TUV: TUserValue;
    i,N : Integer;
begin
  TUV:= TUserValue.Create;
  TUV.FStatus:= [mcInsert];
  TUV.FType:= aType;
  TUV.FDataSetID:= aDataSet;
  TUV.FSubject:= aSubject;
  TUV.FName:= aName;
  TUV.FXML:= aXML;
  TUV.FPublic:= aPublic;

  if aOrder = -1 then
    begin
      N:= 1;
      for i:=0 to Pred(Count) do
        if (items[i].TypeValue = aType) and (items[i].DataSetID = aDataSet) and (items[i].Subject = aSubject) then
          N:= max(N, Succ(items[i].Order));
      TUV.FOrder:= N;
    end
  else
    TUV.FOrder:= aOrder;

  Add(TUV);
  Result:= IndexOf(TUV);
end;

function TUserValues.Commit: Boolean;
var i: Integer;
    PreStatus: DeTypes.TDataChanges;
begin
  Result:= True;
  for i:=Pred(Count) downto 0 do
    begin
      PreStatus:= items[i].Status;
      Result:= Result and items[i].Commit;
      if (mcDelete in PreStatus) and not (mcDelete in items[i].Status) then Delete(i);
    end;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}

procedure Startup;
begin
{$IFDEF DEBUG}
  DebugLog('DeMeta unit initialization ...');
{$ENDIF}
  // DeFormat := RegisterClipboardFormat('DEDATA');
  // DeHeader := RegisterClipboardFormat('DEHEAD');
  // DeIdentity := RegisterClipboardFormat('DEIDENT');
  DeXML := RegisterClipboardFormat('DEXML');
  dbcoXML := RegisterClipboardFormat('XML');
  DeRTF := RegisterClipboardFormat('rich text format');
  DeExcelXML := RegisterClipboardFormat('XML Spreadsheet');
  DeClipboard := TDeClipboard.Create;
  SetClipboard(DeClipboard).Free;
  GroupFilters := TGroupFilters.Create;
end;

procedure Shutdown;
begin
{$IFDEF DEBUG}
  DebugLog('DeMeta unit finalization ...');
{$ENDIF}
  FreeAndNil(GroupFilters);
  FreeAndNil(MainMenuMeta);
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

