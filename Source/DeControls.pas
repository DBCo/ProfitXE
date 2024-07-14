{$WARN UNIT_PLATFORM OFF}

unit DeControls;

interface

uses Graphics, SysUtils, ExtCtrls, ComCtrls, Variants, Forms, Classes, Controls,
  StdCtrls, CheckLst, Menus, Windows, Types, Dialogs, FileCtrl, DateUtils,
  Messages, DB, Mask, sHDocVw, DelphiTwain, CommCtrl, Themes, Math,
  BaseGridFormUnit, DeMeta, DSMeta, Codescan, DeDB, {ListFormUnit,} DataCacheUnit,
  DeTypes, DeFileDropper, DeCalculator, DeParser, DeScript;

const
  TimeMaskHHMMSS = '!99:99:99;1;0';
  TimeMaskHHMM   = '!99:99;1;0';

type
  TDeTabSheet = class(TTabSheet)
  private
    FY: Integer;
    FH: Integer;
    ListY       : TIntegerDynArray;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure SetY(aY:Integer);
{    procedure InitScroll; }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure DeOnResize(Sender: TObject);
    property Y : Integer read FY Write SetY;
  end;

  TDeEdit = class(TEdit)
  private
    procedure WMUndo(var Message: TWMCut);   message WM_UNDO;
  protected
    FValue: Variant;
    procedure SetValue(aValue: Variant);
    function GetValue: Variant;
  public
    constructor Create(Owner: TComponent); override;
    property Value: Variant read GetValue write SetValue;
  end;

  TDeMemo = class(Tmemo)
  protected
    procedure DeOnKeyPress(Sender: TObject; var Key: Char);
  public
    constructor Create(Owner: TComponent); override;
  end;

  TDeEditGUID = class(TButtonedEdit)
  private
    FValue: Variant;
    procedure WMUndo(var Message: TWMCut); message WM_UNDO;
  protected
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    procedure KeyPress(var Key: Char); override;
    procedure RightButtonClick(Sender: TObject);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property Value: Variant read GetValue write SetValue;
  end;

  TDeEditFile = class(TButtonedEdit)
  private
    FValue: Variant;
    procedure WMUndo(var Message: TWMCut); message WM_UNDO;
  protected
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    procedure RightButtonClick(Sender: TObject);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property Value: Variant read GetValue write SetValue;
  end;

  /// <summary>
  ///   Резиновая панель, рекурсивно организует внутренние контролы.
  ///   Контролирует свой размер согласно количеству ВИДИМЫХ внутренних элементов
  /// </summary>
  TDePanel = class(TPanel)
  private
    function GetInDesign: Boolean;
    procedure SetInDesign(aInDesign: Boolean);
  protected
  public
    constructor Create(Owner:TComponent); override;
    destructor Destroy; override;
  property InDesign: Boolean read GetInDesign write SetInDesign;
  end;

  /// <summary>
  ///   Элемент отображения Internet Explorer
  /// </summary>
  TDeBrowser = class(TCustomPanel{CustomEdit})
  private
    FValue  : String;
    FNavigateMode: Boolean;
    FBrowser : TWebBrowser;
    flag: OleVariant;
  const
    FInitString = 'about:blank';
  public
    constructor Create(Owner:TComponent); override;
    destructor Destroy; override;
    procedure SetValue(const Value: string);
    procedure SetNavigateMode(const aValue: Boolean);
  published
    /// <param name="Value">
    ///   HTML текст, отображаемый в браузере.
    /// </param>
    property Value: string read FValue write SetValue;
    property NavigateMode: Boolean read FNavigateMode write SetNavigateMode;
  end;

  /// <summary>
  ///   Элемент отображения штрих-кода
  /// </summary>
  TDeBarcode = class(TCustomPanel)
  private
    FValue  : String;
    FType   : Integer;
    FBarCode: TBarcode;
    procedure SetValue(Value:String);
  public
    constructor Create(Owner:TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property CodeType : Integer read FType   write FType;
    property Value    : String  read FValue  write SetValue;
  end;

type
  /// <summary>
  ///   Элемент отображения и управления файлами
  /// </summary>
  TDeExplorer = class(TCustomPanel)
  private
    FValue : String;
    Box : TFileListBox;
    Procedure FileListBoxDblClick(Sender: TObject);
  public
    constructor Create(Owner:TComponent); override;
    procedure Paint; override;
    procedure SetValue(Value:String);
  published
    property Value : String read FValue write SetValue;
  end;

type
  /// <summary>
  ///   Тип данных в элементе отображения и управления файлами
  /// </summary>
  TImageType = (itNone, itUnknown, itBMP, itJPEG, itGIF, itTIF, itPNG, itWMF);

  ETwainError = class(Exception);

  /// <summary>
  ///   Элемент отображения и управления графическими данными
  /// </summary>
  //DONE -oКуфенко П.Ю. -cTDeImage: Нужно реализовать чистку временных файлов!
  TDeImage = class(TCustomControl)
  private
    FFileData: array of Byte;
    FFileName: string;
    FOnChange: TNotifyEvent;
    FGraphic: TGraphic;
    FOverIcon: TIcon;
//    FBitmap: Graphics.TBitmap;
    FImageType: TImageType;
    MI_Open, MI_LoadFrom, MI_LoadTo, MI_ScanFrom, MI_Export,
    MI_Empty, MI_Paste, MI_Copy, MI_LinkTo, MI_Cut: TMenuItem;
    FInitialDir: string;
    FDropper: TDropper;
    FTwain: TDelphiTwain;
    FMultipleFiles: Boolean;
    FThreads: TThreadList;
    FVersion: Cardinal;
    FAuthor: string;
    FModified: TDateTime;
    FReadOnly: Boolean;
    procedure SetAuthor(const Value: string);
    procedure SetModified(const Value: TDateTime);
    function GetDataValue: Variant;
    procedure SetReadOnly(const Value: Boolean);
    type
      /// <summary>
      ///   Нить слежения за изменением открытого файла.
      /// </summary>
      TDeThread = class(TThread)
      private
        FFileName: string;
        FFileDate: TDateTime;
        FErrorMessage: string;
        FOnChange: TNotifyEvent;
        procedure ChangeNotification;
      protected
        procedure Execute; override;
      public
        constructor Create(const AFileName: string; AOnChange, AOnDestroy: TNotifyEvent);
        property FileName: string read FFileName;
        property FileDate: TDateTime read FFileDate;
        property ErrorMessage: string read FErrorMessage;
      end;
    function FileIsEditable: Boolean;
    procedure SetValue(const aValue: Variant);
    function GetValue: Variant;
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
    procedure OnDropUp(var Files: TStringList);
    /// <summary>
    ///   Обработчик события изменения файла от нити слежения за изменением открытого файла.
    /// </summary>
    /// <remarks>
    ///   Файл был изменён и его надо повторно загрузить в этом обработчике!
    /// </remarks>
    procedure OnThreadChange(Sender: TObject);
    /// <summary>
    ///   Обработчик события завершения нити слежения за изменением открытого файла.
    /// </summary>
    /// <remarks>
    ///   После обработки этого события нить завершается и освобождаются все ресурсы,
    ///   которые она использовала. Если указатель на нить был где-то в локальной
    ///   переменной, то её нужно обнулить в этом обработчике!
    /// </remarks>
    procedure OnThreadDone(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure UpdateImage(NewSource: Boolean);
    function IsLoadedImage(const AFileData:TByteDynArray):boolean;
    procedure onPopup(Sender: TObject);
    procedure onOpen(Sender: TObject);
    procedure onLoadFrom(Sender: TObject);
    procedure onLoadTo(Sender: TObject);
    procedure onEmpty(Sender: TObject);
    procedure onPaste(Sender: TObject);
    procedure onCopy(Sender: TObject);
    procedure onLinkTo(Sender: TObject);
    procedure onCut(Sender: TObject);
    procedure ExportToFile(const FileName: string);
    procedure OnExportTo(Sender: TObject);
//    procedure UpdateFileData;
    procedure TwainAcquire(Sender: TObject; const Index: Integer; Bitmap: Graphics.TBitmap; var Cancel: Boolean);
    /// <summary>
    ///   Метод генерации исключительной ситуации при ошибке TWAIN.
    /// </summary>
    /// <param name="Sender">
    ///    Объект TWAIN, вызывающий исключительную ситуацию.
    /// </param>
    /// <param name="Index">
    ///    Индекс источника TWAIN, вызывающий исключительную ситуацию.
    /// </param>
    /// <param name="ErrorCode">
    ///    Код ошибки TWAIN.
    /// </param>
    /// <param name="Additional">
    ///    Дополнительный код ошибки TWAIN.
    /// </param>
    /// <exception cref="ETwainError">Исключение по ошибке TWAIN.</exception>
    procedure TwainError(Sender: TObject; const Index: Integer; ErrorCode, Additional: Integer);
    procedure onScanFrom(Sender: TObject);
    /// <summary>
    ///   Метод перестроения списка доступных сканеров.
    /// </summary>
    procedure RebuildTwainSources;
    procedure PrepareDialogFilters(OpenDialog: TOpenDialog);
    procedure HintUpdate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    ///   Метод загрузки данных из указанного файла.
    /// </summary>
    /// <param name="FileName">
    ///    Имя файла, из которого необходимо загрузить данные.
    /// </param>
    /// <param name="OriginalFileName">
    ///    Имя оригинального файла (по умолчанию пусто и имя оригинального файла надо брать из параметра FileName).
    /// </param>
    /// <param name="Version">
    ///    Версия файла (по умолчанию 1.00).
    /// </param>
    /// <returns>Значение True указывает на успешную загрузку файла.</returns>
    /// <remarks>Если обновление данных, то надо указывать имя оригинального файла и его версию.</remarks>
    function LoadFromFile(const FileName: string; const OriginalFileName: string = ''; const Version: Cardinal = 100): Boolean;
    /// <summary>
    ///   Метод выгрузки данных в указанный файл.
    /// </summary>
    /// <param name="FileName">
    ///    Имя файла, в который необходимо выгрузить данные.
    /// </param>
    /// <returns>Значение True указывает на успешную выгрузку файла.</returns>
    /// <see cref="SaveToString">Выгрузка данных в виде Ansi строки.</see>
    /// <see cref="SaveToBitmap">Выгрузка данных в указанный файл в виде Bitmap картинки.</see>
    function   SaveToFile(const FileName: string): Boolean;
    /// <summary>
    ///   Метод выгрузки данных в указанную Ansi строку.
    /// </summary>
    /// <param name="aString">
    ///    Строка, в которую необходимо выгрузить данные.
    /// </param>
    /// <returns>Значение True указывает на успешную выгрузку данных.</returns>
    /// <see cref="SaveToFile">Выгрузка данных в файл.</see>
    /// <see cref="SaveToBitmap">Выгрузка данных в указанный файл в виде Bitmap картинки.</see>
    function   SaveToString(var aString: AnsiString): Boolean;
    /// <summary>
    ///   Метод выгрузки данных в указанный Bitmap файл.
    /// </summary>
    /// <param name="FileName">
    ///    Имя файла для сохранения в виде Bitmap картинки.
    /// </param>
    /// <returns>Значение True указывает на успешную выгрузку данных.</returns>
    /// <see cref="SaveToFile">Выгрузка данных в файл.</see>
    /// <see cref="SaveToString">Выгрузка данных в указанную Ansi строку.</see>
    function   SaveToBitmap(const FileName: string): Boolean;
    property   ImageType: TImageType read FImageType;
    property   OriginalFileName:string read FFileName;
//    property   Bitmap:Graphics.TBitmap read FBitmap;
    property   Graphic: TGraphic read FGraphic;
    property   InitialDir:string read FInitialDir write FInitialDir;
    /// <summary>
    ///   Свойство включения поддержки работы с несколькими файлами в компоненте.
    /// </summary>
    property   MultipleFiles: Boolean read FMultipleFiles write FMultipleFiles;
    property Version: Cardinal read FVersion;
    property Author: string read FAuthor write SetAuthor;
    property Modified: TDateTime read FModified write SetModified;
  published
    property Value: Variant read GetValue write SetValue;
    property DataValue: Variant read GetDataValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  end;

  TDEDateTimePicker=class(TDateTimePicker)
  private
    procedure WMSetFocus(var Msg: TMessage); message  WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMChar(var Msg: TMessage); message WM_CHAR;
    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;

  protected
    FAllowNull  : Boolean;
    FIsNull     : Boolean;

    FNullText   : String;
    FUndoValue  : Variant;
    FBackFormat : String;
    FPopupMenu  : TPopupMenu;
    MI_Cut      : TMenuItem;
    MI_Copy     : TMenuItem;
    MI_Paste    : TMenuItem;
    MI_Sep1     : TMenuItem;
    MI_Delete   : TMenuItem;
    MI_Default  : TMenuItem;
    MI_Sep2     : TMenuItem;
    MI_Undo     : TMenuItem;

    procedure OnDSCPopupMenu(Sender:TObject);
    procedure OnDSCCutClick(Sender:TObject);
    procedure OnDSCCopyClick(Sender:TObject);
    procedure OnDSCPasteClick(Sender:TObject);
    procedure OnDSCDeleteClick(Sender:TObject);
    procedure OnDSCDefaultClick(Sender:TObject);
    procedure OnDSCUndoClick(Sender:TObject);
    procedure OnDSCEnter(Sender:TObject);
    procedure OnDSCExit(Sender:TObject);

    function GetValue: Variant; virtual;
    procedure InnerSetValue(aValue: Variant);
    procedure SetValue(aValue:Variant); virtual;
    procedure SetFormat(const aFormat: string);
    procedure SetAllowNull(const aValue: Boolean);
    procedure CheckEmptyDate; override;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    property IsNull: Boolean read FIsNull;
    property Value: Variant read GetValue write SetValue;
    property Format: String read FBackFormat write SetFormat;
    property AllowNull: Boolean read FAllowNull write SetAllowNull;
    property EmptyText: String read FNullText write FNullText;
  end;

  TDETimePicker=class(TDEDateTimePicker)
  protected
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TTimeEdit=class(TMaskEdit)
  private
    FValue: Variant;
    FFormat: String;
    FMaskUpdating: Boolean;
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    procedure WMKeyPress(var Msg: TMessage); message WM_CHAR;
    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;

    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure SetFormat(const Value: String);
  protected
    procedure Change; override;
    procedure UpdateMask;
  public
    constructor Create(AOwner: TComponent); override;
    property Value : Variant read GetValue write SetValue;
    property Format: String read FFormat write SetFormat;
  end;


  /// <summary>
  ///   Выпадающий список, значения элементов которого берутся из набора данных
  /// </summary>
  TDeDSComboBox = class(TComboBox)
  private
    RepaintMes   : TWmPaint;
    FTextPress   : string;   // текст фильтра
    FLastPress   : string;
    FDataID      : integer;
    FError       : string;
    FValue       : Variant;
    FDefault     : Variant;
    FUndoValue   : Variant;  // значение назначенное снаружи
    FReadOnly    : boolean;
    FCache       : TDataCache;
    FPrepared    : Boolean;
    FTableMeta   : TTableMeta;
    FViewWidth   : Integer;
    FViewList    : TFieldsMeta;
    FAddNull     : Integer;
    FFilterList  : PFilterList;
    FNullText    : string;
    FFilterStr   : string;
    FArrayText   : string;

    FPopupMenu   : TPopupMenu;
    MI_Undo      : TMenuItem;
    MI_Sep1      : TMenuItem;
    MI_Cut       : TMenuItem;
    MI_Copy      : TMenuItem;
    MI_Paste     : TMenuItem;
    MI_Delete    : TMenuItem;
    MI_Default   : TMenuItem;
    MI_Sep2      : TMenuItem;
    MI_Sep3      : TMenuItem;
    MI_Open      : TMenuItem;
    MI_Create    : TMenuItem;
    procedure SetDataID(const aDataID : integer);
    function  GetValue : Variant;
    procedure InnerSetValue(const aValue : Variant);
    procedure SetValue(const aValue : Variant);
    procedure SetReadOnly(aReadOnly : boolean);
    procedure ComboBoxDrawItem(Control: TWinControl; Index: Integer; aRect: TRect; State: TOwnerDrawState);

    procedure OnDSCPopupMenu(Sender:TObject);
    procedure OnDSCUndoClick(Sender:TObject);
    procedure OnDSCCutClick(Sender:TObject);
    procedure OnDSCCopyClick(Sender:TObject);
    procedure OnDSCPasteClick(Sender:TObject);
    procedure OnDSCDeleteClick(Sender:TObject);
    procedure OnDSCOpenClick(Sender:TObject);
    procedure OnDSCCreateClick(Sender:TObject);
    procedure OnDSCDefaultClick(Sender:TObject);
    procedure SetFilterStr(const Value: string);
    procedure SetArrayText(const Value: string);
    procedure SetTextPress(const aValue: string);
  protected
    procedure WMPaint(var Message: TWmPaint); message WM_PAINT;
    procedure ExternalOnCloseUp(Sender: TObject);

    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;
    procedure KeyPress(var Key: Char); override;
    procedure SetEnabled(Value : boolean);  override;
    procedure SetPrepared(const aValue: Boolean);
    Function GetAllowNull: Boolean;
    procedure SetAllowNull(const aAllowNull: Boolean);
    procedure SetNullText(const Value: string);
    procedure DropDown; override;
    procedure CloseUp; override;
    property TextPress: string read FTextPress write SetTextPress;
  public
    constructor Create(AOwner: TComponent);  override;
    destructor Destroy;  override;
    function FocusedItem: TCacheItem;
    property DataID : integer read FDataID write SetDataID;
    property Value : Variant read GetValue write SetValue;
    property Default : Variant read FDefault write FDefault;
    property ReadOnly : boolean read FReadOnly write SetReadOnly;
    procedure Change;  override;

    property Prepared: Boolean read FPrepared write SetPrepared;
    property Cache: TDataCache read FCache;
    function GetItemID(I: Integer): Variant;
    property AllowNull: Boolean read GetAllowNull write SetAllowNull;
    property FilterList: PFilterList write FFilterList;
    property NullText: string read FNullText write SetNullText;
    property FilterStr: string read FFilterStr write SetFilterStr;
    // v. 15.10
    property ArrayText: string read FArrayText write SetArrayText;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  /// <summary>
  ///   Выпадающий список полей таблицы базы данных
  /// </summary>
  TDeFieldsComboBox = class(TComboBox)
  private
    FTableMeta : TTableMeta;
    procedure SetTableMeta(const aTable : TTableMeta);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property TableMeta : TTableMeta read FTableMeta write SetTableMeta;
  end;

  /// <summary>
  ///   Выпадающий список таблиц базы данных
  /// </summary>
  TDeTablesComboBox = class(TComboBox)
  private
    FDatabase : TDeCustomDatabase;
    procedure SetDatabase(const aDatabase : TDeCustomDatabase);
    function GetDatabaseID : integer;
    procedure SetDatabaseID(const Value : integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property Database : TDeCustomDatabase read FDatabase write SetDatabase;
    property DatabaseID : integer read GetDatabaseID write SetDatabaseID;
  end;

  TDeStateType = (dsChanging, dsTextAssigning);
  TDeState = set of TDeStateType;

  /// <summary>
  ///   Редактор зашифрованного пароля
  /// </summary>
  TDePasswordEdit = class(TEdit)
  private
    FState         : TDeState;
    FChanged       : boolean;
    FEncrypted     : boolean;
    function GetText : TCaption;
    procedure SetText(const aText : TCaption);
  protected
    procedure ClearText;
    procedure KeyPress(var Key : Char);  override;
    procedure Change;  override;
  public
    constructor Create(aOwner : TComponent);  override;
    property Encrypted : boolean read FEncrypted write FEncrypted;
    property Text : TCaption read GetText write SetText;
  end;

  /// <summary>
  ///   Редактор множества значений
  /// </summary>
  TDeCheckListBox = class(TCheckListBox)
  private
    FDataID : integer;
    procedure SetDataID(const aDataID : integer);
  public
    property DataID : integer read FDataID write SetDataID;
  end;

  /// <summary>
  ///   Закладка со списком записей
  /// </summary>
  TDeListTabSheet = class(TTabSheet)
  private
    FDataID    : integer;
    FLinkField : TFieldMeta;
    FParentDSM : TDataSetMeta;
    FGridForm  : TBaseGridForm;

    FDeEnabled : Boolean;
    FShowNow   : Boolean;
    FWaiting   : Boolean;
    FViewType: TDSViewType;
    procedure CreateData;
    procedure DestroyData;
    function GetCaption : string;
    procedure SetCaption(const aCaption : string);
    procedure SetDeEnabled(aValue: Boolean);
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);  override;
  published
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;  override;
    procedure PrepareToDestroy;
    property GridControl : TBaseGridForm read FGridForm;
    property DataID : integer read FDataID;
    property LinkField : TFieldMeta read FLinkField;
    property Caption : string read GetCaption write SetCaption;
    property DeEnabled : Boolean read FDeEnabled write SetDeEnabled;
    property ViewType : TDSViewType read FViewType write FViewType;
    procedure SetDataID(aDataID : integer;  aLinkField : TFieldMeta; aParentDSM : TDataSetMeta);
  end;

{ Include SpinEdit.pas + }
const
  ScrollFrequency = 80;
  ScrollDelayTimes = 5;

  d = 2; // надо, по хорошему, учитывать толщины рамок

type

  TScrollBtnPressed = (sbpNone, sbpUpHot, sbpDnHot, sbpUp, sbpDn, sbpInfo);
  TSpinValueState = (stcNormal, stcBounds, stcError);

  TDeCheckBox = class(TCustomCheckBox)
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;

    property OnClick;
    property OnKeyDown;
    property Caption;
    property State;
  end;

  /// <summary>
  ///   Редактор численных значений
  /// </summary>
  TDeSpinEdit = class(TCustomMaskEdit)
  private
    FValue: Variant;
    FSpinTextColor: TSpinValueState;

    FColor: TColor;
    FNumberType: TVarType;
    FMinValue: Int64;
    FMaxValue: Int64;
    FInfoText : string;
    FInfoWidth : Integer;
    FSpinWidth : Integer;

    FScrollBtnPressed: TScrollBtnPressed;
    FTimer: TTimer;
    FScrollTimes: Integer;
    FOnChange: TNotifyEvent;
    FCalculator: TDeCalculator;
    FParser    : TDeParser;
    FRectInfo  : TRect;
    FRectBtnUp : TRect;
    FRectBtnDn : TRect;
    FEditing: Boolean;
    FDisplayFormat: string;
    procedure SetValue(aValue: Variant);
    procedure SetInfo(aValue: String);
    procedure SetNumberType(aValue: TVarType);
    procedure TimerTimer(Sender: TObject);
    procedure SetColor(aValue: TColor);
    procedure CalculateInfoWidth;

    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMPaint(var Message: TWmPaint); message WM_PAINT;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    function TextWidth(const aValue: string): Integer;
    function ValueToStr(aValue: Variant): string;
    procedure SetDisplayFormat(const Value: string);
    procedure setSpinTextColor(aValue: TSpinValueState);
    property SpinTextColor: TSpinValueState read FSpinTextColor write setSpinTextColor ;
  protected
    property Info : string read FInfoText write SetInfo;
    procedure KeyPress(var Key: Char); override;
    procedure Change; override;
    function GetClientRect: TRect; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Font;
    property Value: Variant read FValue write SetValue;
    property ReadOnly;
    property Color read FColor write SetColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnKeyDown;
    property PopupMenu;
    property NumberType: TVarType read FNumberType write SetNumberType;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
  end;

  TDeSpinEditFloat = class(TDeSpinEdit)
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ Include SpinEdit.pas - }

function GetTempFile: string; overload;
function GetTempFile(const Prefix, Extension: string): string; overload;

implementation

{$R DeEditGUID.res}
{$R DeEditFile.res}

uses {Registry, }jpeg, {Clipbrd, }ShlObj, ActiveX, ComObj, {StrUtils,} ZLib, MSHTML, System.StrUtils,
     Vcl.Imaging.GIFImg, Vcl.Imaging.pngimage, System.IOUtils, System.Zip,
     Vcl.Samples.Gauges, ShellAPI, System.Contnrs, System.UITypes, DeLog,
     DataManager, UnitA, Funcs, Dictionary, ClipBrd, DataUnit, Security,
     {DataUnit, }DeSettings,  DeMetadata, {QueryDescriptor, }{DeParser, }
  //   tiffbmp,
     {FileMonitorThreadUnit, }DCPmd5, HintForm;

resourcestring
  sMicrosoftWordFilter = 'Microsoft Word|*.doc;*.docx;*.rtf';
  sMicrosoftExcelFilter = 'Microsoft Excel|*.xls;*.xlsx';
  sTextFilter = 'Text Files|*.txt';
  sShellLinkFilter = 'Shell Link Files|*.lnk';
  sZipFilter = 'Compressed Files|*.zip';
  sInternalFileFilter = 'Internal Files|*.dat';

var
  TempDirectory: string;
{
procedure Register;
begin
  RegisterComponents('Profit', [TDeBarcode]);
  RegisterComponents('Profit', [TDeImage]);
  RegisterComponents('Profit', [TDeDateTimePicker]);
end;
}
{
function GetTempFile: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  repeat
    GetTempPath(SizeOf(Buffer) - 1, Buffer);
    GetTempFileName(Buffer, '~', 0, Buffer);
  until FileExists(Buffer);
  Result:=Buffer;
end;
}

/// <summary>
///   Функция получения имени временного файла
/// </summary>
/// <returns>Возвращает полный путь к временному файлу.</returns>
function GetTempFile: string; overload;
var
  Directory: string;
begin
  Directory := TempDirectory;
  repeat
    Result := Directory + IntToHex(GetTickCount, 8) + sExtensionTMP;
  until not FileExists(Result);
end;

/// <summary>
///   Функция получения имени временного файла с префиксом и расширением.
/// </summary>
/// <param name="Prefix">префикс в имени файла</param>
/// <param name="Extension">расширение файла</param>
/// <returns>Возвращает полный путь к временному файлу.</returns>
function GetTempFile(const Prefix, Extension: string): string; overload;
var
  Directory: string;
  Index: Integer;
begin
  Directory := TempDirectory;
  Result := Directory + PathDelim + Prefix + Extension;
  if FileExists(Result) then
    begin
      Index := 1;
      Directory := Directory + Prefix;
      repeat
        Inc(Index);
        Result := Directory + IntToStr(Index) + Extension;
      until not FileExists(Result);
    end;
end;

function GetFileNamefromLink(LinkFileName: string): string;
var
  MyObject: IUnknown;
  MySLink: IShellLink;
  MyPFile: IPersistFile;
  FileInfo: TWin32FINDDATA;
  Buff: array[0..MAX_PATH] of Char;
begin
  Result := EmptyStr;
  if (fileexists(Linkfilename) = false) then
    exit;
  MyObject := CreateComObject(CLSID_ShellLink);
  MyPFile := MyObject as IPersistFile;
  MySLink := MyObject as IShellLink;
  if MyPFile.Load(Pointer(WideString(LinkFileName)), STGM_READ)<>S_OK then
    exit;
  MySLink.GetPath(Buff, Max_PATH, FileInfo, SLGP_UNCPRIORITY);
  Result := buff;
end;

procedure CreateShortcut(const FilePath, ShortcutPath,
  WorkDir, Description, Params: string);
var
  obj: IUnknown;
  isl: IShellLink;
  ipf: IPersistFile;
begin
  obj := CreateComObject(CLSID_ShellLink);
  isl := obj as IShellLink;
  ipf := obj as IPersistFile;
  with isl do
  begin
    SetPath(PChar(FilePath));
    SetArguments(PChar(Params));
    SetDescription(PChar(Description));
    SetWorkingDirectory(PChar(WorkDir));
  end;
  ipf.Save(PWChar(WideString(ShortcutPath)), False);
end;

{ TDeTabSheet }

procedure TDeTabSheet.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CLIPCHILDREN or WS_VSCROLL;
  FY:=0;
  ListY :=nil;
end;

procedure TDeTabSheet.DeOnResize(Sender: TObject);
var SI : TScrollInfo;
    i: Integer;
begin

  FH:=0;
  SetLength(ListY, ControlCount);
  for i:=0 to ControlCount-1 do
    if Controls[i].Align<>alClient then
      begin
        ListY[i] := Controls[i].Top;
        if FH<Controls[i].BoundsRect.Bottom then
          FH:=Controls[i].BoundsRect.Bottom;
      end;

  if FH > ClientHeight then
    begin
      SI.cbSize := sizeOf(SI);
      SI.fMask  := SIF_PAGE or SIF_POS or SIF_RANGE;
      SI.nMin   := 0;
      SI.nMax   := FH;
      SI.nPage  := ClientHeight;
      SI.nPos   := Y;

      if SI.nPos > SI.nMax+1-Integer(SI.nPage) then
        SI.nPos := SI.nMax+1-Integer(SI.nPage);

      SetScrollInfo(Handle, SB_VERT, SI, True);
      ShowScrollBar(Handle, SB_VERT, True);
      Y:=SI.nPos;
    end
  else
    begin
      ShowScrollBar(Handle, SB_VERT, False);
      Y:=0;
    end;

  inherited;
end;
{
procedure TDeTabSheet.InitScroll;
var i: Integer;
begin
  SetLength(ListY, ControlCount);
  FH:=0;
  for i:=0 to ControlCount-1 do
    if Controls[i].Align<>alClient then
      begin
        ListY[i] := Controls[i].Top;
        if FH<Controls[i].BoundsRect.Bottom then FH:=Controls[i].BoundsRect.Bottom;
      end;
  OnResize(nil);
end;
}
procedure TDeTabSheet.SetY(aY:Integer);
var i: Integer;
begin
  if FY<>aY then
    begin
      SetScrollPos(Handle, SB_VERT, aY, True);
      SendMessage(Handle, WM_SETREDRAW, 0, 0);

      for i:=0 to Length(ListY)-1 do
        Controls[i].Top:=ListY[i]-aY;

      SendMessage(Handle, WM_SETREDRAW, 1, 0);
      RedrawWindow(Handle, Nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);

      FY:=aY;
    end;
end;

procedure    TDeTabSheet.WMVScroll(var Message: TWMVScroll);
var SI : TScrollInfo;
begin
  SI.cbSize := sizeOf(SI);
  SI.fMask  := SIF_ALL;
  getScrollINfo(Handle,SB_VERT,SI);

  Case Message.ScrollCode of
    SB_LINEUP       : Y:=Max(SI.nPos-YStep,                               0);
    SB_PAGEUP       : Y:=Max(SI.nPos-YStep*(integer(SI.nPage) div YStep), 0);
    SB_LINEDOWN     : Y:=Min(SI.nPos+YStep,                               SI.nMax+1-integer(SI.nPage));
    SB_PAGEDOWN     : Y:=Min(SI.nPos+YStep*(integer(SI.nPage) div YStep), SI.nMax+1-integer(SI.nPage));
    SB_THUMBTRACK   : Y:=SI.nTrackPos;
//    SB_THUMBPOSITION: NewY:=SI.nTrackPos;
//    SB_TOP          : ;
//    SB_BOTTOM       : ;
//    SB_ENDSCROLL    : ;
  end;
end;

{ TDeEdit }

constructor TDeEdit.Create(Owner: TComponent);
begin
  inherited;
  AutoSize := False; // Иначе контрол подгоняется под текущий шрифт!!!
end;

procedure TDeEdit.SetValue(aValue: Variant);
begin
  FValue:= aValue;
  Modified:= False;
  Hint:= EmptyStr;

  if VarIsNull(aValue) or VarIsEmpty(aValue) then
    begin
      Clear;
    end else

  if VarIsArray(aValue) then
    begin
      if 0 < SizeOf(aValue) then Text:= VarArrayGet(Value, [VarArrayLowBound(aValue, 1)])
                            else Clear;
      Hint:= IntToStr(SizeOf(aValue))+' значений';
    end else

    begin
      Text:= VarToStr(aValue);
    end;
end;

procedure TDeEdit.WMUndo(var Message: TWMCut);
begin
  SetValue(FValue);
end;

function TDeEdit.GetValue: Variant;
begin
  if Modified then Result:= Text
              else Result:= FValue;
end;


{ TDePanel }

constructor TDePanel.Create(Owner: TComponent);
begin
  inherited;

  Color:= clWindow;
  BevelInner:= bvNone;
  BevelOuter:= bvNone;
  // ShowCaption:= False;

  BevelKind:= bkTile;
end;

destructor TDePanel.Destroy;
begin
  //
  inherited;
end;

function TDePanel.GetInDesign: Boolean;
begin
  Result:= ( BevelKind = bkTile );
end;

procedure TDePanel.SetInDesign(aInDesign: Boolean);
begin
  if aInDesign then BevelKind:= bkTile
               else BevelKind:= bkNone;
end;

{ TDeBrowser }

constructor TDeBrowser.Create(Owner:TComponent);
begin
  inherited Create(Owner);
  flag := navNoHistory	+ navNoReadFromCache + navNoWriteToCache;
  BorderStyle:= bsSingle;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  ParentCtl3D:= False;
  Ctl3D := false;
  FullRepaint:= False;
  DoubleBuffered:= True;

  FBrowser:=TWebBrowser.Create(self);
  TWinControl(FBrowser).Parent:= self;
  FBrowser.Align:=alClient;

  FValue:=unassigned;
  FNavigateMode:=False;
  FBrowser.Navigate(FInitString, flag);
end;

destructor TDeBrowser.Destroy;
begin
  FBrowser.Free;
  inherited;
end;

procedure TDeBrowser.SetValue(const Value: string);
var
  V: Variant;
  Document: IHTMLDocument2;
  E: IHTMLElement;
begin
  if FValue <> Value then
    if Navigatemode then
      try
        FBrowser.Navigate(FValue, flag);
      Except
        FBrowser.Navigate(FInitString, flag);
      end
    else
      try
        Document := IHTMLDocument2(FBrowser.Document);
        v:= VarArrayCreate([0, 0], varVariant);
        v[0]:= Value;
        Document.write(PSafeArray(TVarData(v).VArray));

        if assigned(Document.body) then
          try
            Document.body.Style.BorderStyle:= 'none';
          except
          end
        else
          try
            // не работает
            E:= Document.createElement('body');
            E.Style.BorderStyle:= 'none';
          except
          end;

        Document.close;
      Except
        FBrowser.Navigate(FInitString, flag);
      end;
  FValue:= Value;
end;

procedure TDeBrowser.SetNavigateMode(const aValue: Boolean);
begin
  FNavigatemode:=aValue;
  SetValue(FValue);
end;

{ TDeBarcode }

constructor TDeBarcode.Create(Owner:TComponent);
begin
  inherited Create(Owner);
  BorderStyle:= bsSingle;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  ParentCtl3D:= False;
  Ctl3D := false;
  FullRepaint:= False;
  DoubleBuffered:= True;
  FType      := -1;
  FBarCode:= TBarCode.Create;
end;

destructor TDeBarcode.Destroy;
begin
  FBarCode.Free;
  inherited;
end;

procedure TDeBarcode.Paint;
begin
  inherited Paint;

  Canvas.Pen.Color:= clWindow;
  Canvas.Brush.Color:= clWindow;
  Canvas.Rectangle(0,0,Width-1,Height-1);

  Canvas.Pen.Color:= clBlack;
  FBarCode.PaintBarCode(Canvas, -1);
end;

procedure TDeBarcode.SetValue(Value:String);
begin
  FValue:= Value;
  FBarCode.SetCode(bc_Auto, FValue);
end;

{ TDeExplorer }

constructor TDeExplorer.Create(Owner:TComponent);
begin
  inherited Create(Owner);
  BorderStyle:= bsSingle;
  BevelOuter := bvNone;
  Color := clWindow;
  FValue:= EmptyStr;
  FullRepaint:= False;
  Box:=TFileListBox.Create(self);
  Box.Parent:=self;
  Box.Align:=alClient;
  Box.BevelInner:=bvNone;
  Box.BevelOuter:=bvNone;
  Box.Ctl3D:=False;
  Box.ShowGlyphs:=True;
  Box.OnDblClick:= FileListBoxDblClick;
  Box.FileType  := [ftDirectory,ftNormal];
end;

procedure TDeExplorer.FileListBoxDblClick(Sender: TObject);
var s,LocateTo : String;
    i,n   : Integer;
    CanUp : Boolean;
    BackupCursor : TCursor;
begin
  inherited;

  if Not FileExists(Box.FileName) then
    begin
      s:= Box.Items[Box.ItemIndex];

      if s='..' then
        begin
          n:=0;
          for i:=1 to Length(Box.Directory) do
            if Box.Directory[i]='\' then n:=i;

          if (5<=n) then  //есть куда идти
            begin
              LocateTo:=Copy(Box.Directory,n+1,255);
              Box.Directory:=Copy(Box.Directory,1,n-1);
            end;

          CanUp:= Length(Box.Directory)>Length(FValue);
        end
      else
        begin
          s:=Copy(s,2,Length(s)-2);
          Box.Directory:= Box.Directory+'\'+s;
          CanUp:=True;
        end;

      // удаляем родные ссылки в структуре диска
      for i:=Box.Items.Count-1 downto 0 do
        if (Box.Items[i]='[.]') or (Box.Items[i]='[..]')
          then Box.Items.Delete(i);

      // добавляем ссылку вверх, если это необходимо
      if CanUp then
        Box.Items.Add('..');

      if Not (LocateTo=EmptyStr) then
        begin
          for i:=0 to Box.Items.Count-1 do
            if Box.Items[i]='['+LocateTo+']' then
              begin
                Box.ItemIndex:=i;
                Break;
              end;
        end;
    end
  else
    begin
      BackupCursor := Screen.Cursor;
      Screen.Cursor := crHourglass;
      try
        s:=Box.FileName;
        ShellExecute(Application.Handle, 'open', PChar(s), nil, nil, SW_SHOWNORMAL);
      finally
        Screen.Cursor := BackupCursor;
      end;
    end;
end;

procedure TDeExplorer.Paint;
begin
  inherited Paint;
  Box.Repaint;
  Caption:='нет файлов для отображения'
end;

procedure TDeExplorer.SetValue(Value:String);
var i:Integer;
begin
  FValue:=Value;
  if SysUtils.DirectoryExists(Value) then
    begin
      Box.Directory:=Value;
      Box.Visible:=True;
      Box.Update;
      Box.AutoComplete:=True;

      // удаляем родные ссылки вверх в структуре диска
      Box.Items.BeginUpdate;
      for i:=Box.Items.Count-1 downto 0 do
        if (Box.Items[i]='[.]') or (Box.Items[i]='[..]') then
          Box.Items.Delete(i);
      Box.Items.EndUpdate;
      Box.Refresh;
    end
  else
    begin
      Box.Visible:=false;
      Box.Directory:=EmptyStr;
      Box.Items.BeginUpdate;
      Box.Items.Clear;
      Box.Items.EndUpdate;
      Box.Update;
    end;
end;

{ TDeImage }

{
procedure TDeImage.UpdateFileData;
var
  Stream: TFileStream;
  TempFileName: string;
begin
  TempFileName := GS.TempPath + '~' + FFileName;
  try
    Sleep(100);
    Stream := TFileStream.Create(TempFileName, fmOpenRead or fmShareDenyNone);
    Stream.Position := 0;
    SetLength(FFileData, Stream.Size);
    Stream.ReadBuffer(FFileData[0], Stream.Size);
  finally
    Stream.Free;
  end;
end;
}

var
  IntDeImageLastDir : string = '';

procedure TDeImage.Paint;
var
  R: TRect;
  ImageRectWidth: Integer;
  ImageRectHeight: Integer;
begin
  inherited;
  // Если есть картинка и тип указан, то ...
  if Assigned(FGraphic) and (FImageType <> itNone) then
    begin
      case FImageType of
        itBMP, itJPEG, itGIF, itTIF, itPNG, itWMF: { Картинки }
          begin
           ImageRectWidth := FGraphic.Width;
           ImageRectHeight := FGraphic.Height;
          end;
      else
        // Иначе будут иконки ...
        ImageRectWidth := GetSystemMetrics(SM_CXICON);
        ImageRectHeight := GetSystemMetrics(SM_CYICON);
        {
        ImageRectWidth := 32;
        ImageRectHeight := 32;
        }
      end;
      R := ClientRect;
      InflateRect(R, -3, -3);
      if (ImageRectHeight <= (R.Bottom - R.Top)) and
         (ImageRectWidth <= (R.Right - R.Left)) then
        begin
          R.Top := (R.Top + R.Bottom - ImageRectHeight) div 2;
          R.Bottom := R.Top + ImageRectHeight;
          R.Left := (R.Left + R.Right - ImageRectWidth) div 2;
          R.Right := R.Left + ImageRectWidth;
        end
      else if ImageRectHeight * (R.Right - R.Left) <=
              ImageRectWidth * (R.Bottom - R.Top) then
        begin
          ImageRectHeight := ImageRectHeight * (R.Right - R.Left) div ImageRectWidth;
          R.Top := (R.Top + R.Bottom - ImageRectHeight) div 2;
          R.Bottom := R.Top + ImageRectHeight;
        end
      else
        begin
          ImageRectWidth := ImageRectWidth * (R.Bottom - R.Top) div ImageRectHeight;
          R.Left := (R.Left + R.Right - ImageRectWidth) div 2;
          R.Right := R.Left + ImageRectWidth;
        end;
      Canvas.StretchDraw(R, FGraphic);
      if Assigned(FOverIcon) then
        Canvas.StretchDraw(R, FOverIcon);
      if FImageType in [itBMP, itJPEG, itGIF, itTIF, itPNG, itWMF] then
        begin
          InflateRect(R, 1, 1);
          Canvas.Brush.Color := clWindowFrame;
          Canvas.FrameRect(R);
        end;
    end;
end;

constructor TDeImage.Create(AOwner: TComponent);
begin
  inherited;

  AutoSize := False; // Иначе контрол подгоняется под текущий шрифт!!!

  FImageType := itNone;
  Color := clWindow;
  ParentColor := False;
  BevelKind := bkTile;
  BevelInner := bvNone;
  DoubleBuffered := True;
  FReadOnly := False;
  FMultipleFiles := True; // Если не хотим делать ZIP`ы, то поставить False!
  FThreads := TThreadList.Create;

  FInitialDir := Variables.AsString[RegDirPath];

  PopupMenu := TPopupMenu.Create(Owner);
  PopupMenu.OnPopup := onPopUp;

  onDblClick := onOpen;

  MI_Open := TMenuItem.Create(Owner);
  MI_Open.Default := True;
  MI_Open.Caption := GetTitle('_Da.Open');
  MI_Open.OnClick := onOpen;
  PopupMenu.Items.Add(MI_Open);

  MI_Empty := TMenuItem.Create(self);
  MI_Empty.Caption := cLineCaption;
  PopupMenu.Items.Add(MI_Empty);

  MI_LoadFrom := TMenuItem.Create(Owner);
  MI_LoadFrom.Caption := GetTitle('_Da.LoadFrom', ttSecondName);
  MI_LoadFrom.OnClick := onLoadFrom;
  PopupMenu.Items.Add(MI_LoadFrom);

  FTwain := TDelphiTwain.Create(Owner);
  FTwain.OnTwainAcquire := TwainAcquire;
  FTwain.OnAcquireError := TwainError;
  //FTwain.TransferMode := ttmMemory;

  MI_ScanFrom := TMenuItem.Create(Owner);
  PopupMenu.Items.Add(MI_ScanFrom);
  // Построим меню со списком сканеров при необходимости ...
  RebuildTwainSources;

  MI_LoadTo:=TMenuItem.Create(Owner);
  MI_LoadTo.Caption := GetTitle('_Da.LoadTo', ttSecondName);
  MI_LoadTo.OnClick := onLoadTo;
  PopupMenu.Items.Add(MI_LoadTo);

  MI_Empty := TMenuItem.Create(self);
  MI_Empty.Caption := cLineCaption;
  PopupMenu.Items.Add(MI_Empty);

  MI_LinkTo:=TMenuItem.Create(Owner);
  MI_LinkTo.Caption := GetTitle('_Da.LinkTo', ttSecondName);
  MI_LinkTo.OnClick := onLinkTo;
  PopupMenu.Items.Add(MI_LinkTo);

  if Assigned(CurrentConfig) and (UserSession.IsAdmin) then
    begin
      MI_Export := TMenuItem.Create(Owner);
      MI_Export.Caption := GetTitle('_Da.ExportTo', ttSecondName);
      MI_Export.OnClick := OnExportTo;
      PopupMenu.Items.Add(MI_Export);
    end;

  MI_Empty := TMenuItem.Create(self);
  MI_Empty.Caption := cLineCaption;
  PopupMenu.Items.Add(MI_Empty);

  MI_Cut:=TMenuItem.Create(Owner);
  MI_Cut.Caption:=GetTitle('_Da.cut');
  MI_Cut.OnClick := onCut;
  PopupMenu.Items.Add(MI_Cut);

  MI_Copy:=TMenuItem.Create(Owner);
  MI_Copy.Caption := GetTitle('_Da.copy');
  MI_Copy.OnClick := onCopy;
  PopupMenu.Items.Add(MI_Copy);

  MI_Paste:=TMenuItem.Create(Owner);
  MI_Paste.Caption := GetTitle('_Da.paste');
  MI_Paste.OnClick := onPaste;
  PopupMenu.Items.Add(MI_Paste);

  MI_Empty := TMenuItem.Create(Owner);
  MI_Empty.Caption := GetTitle('_Da.delete');
  MI_Empty.OnClick := onEmpty;
  PopupMenu.Items.Add(MI_Empty);

  FDropper:=TDropper.Create(Owner);
  FDropper.OnDropUp:=OnDropUp;
end;

destructor TDeImage.Destroy;
var
  List: TList;
  Index: Integer;
begin
  List := FThreads.LockList;
  try
    while List.Count <> 0 do
      begin
        Index := Pred(List.Count);
        TDeThread(List[Index]).Terminate;
        List.Delete(Index);
      end;
  finally
    FThreads.UnlockList;
  end;
  FreeAndNil(FThreads);

  FreeAndNil(FTwain);

  FreeAndNil(FGraphic);
  FreeAndNil(FOverIcon);
  //if Assigned(FBitmap) then FBitmap.Free;
  //if Assigned(FileManager) then FileManager.FreeSource(Self);

  inherited;
end;

procedure TDeImage.PrepareDialogFilters(OpenDialog: TOpenDialog);
resourcestring
  sGraphicFilter = 'Graphic Files|*.jpg;*.jpeg;*.bmp;*.png;*.gif;*.wmf;*.emf;*.tif;*.tiff';
  sAllFilter = 'All Files|*.*';
var
  Filter: string;
  FilterIndex: Integer;
  procedure AppendFilter(const Text: string);
  begin
    if Length(Text) <> 0 then
      begin
        if Length(Filter) <> 0 then
          Filter := Filter + '|';
        Filter := Filter + Text;
        Inc(FilterIndex);
      end;
  end;
begin
  if Assigned(OpenDialog) then
    begin
      Filter := EmptyStr;
      FilterIndex := 0;
      if GetMicrosoftWordVersion <> 0 then
        AppendFilter(sMicrosoftWordFilter);
      if GetMicrosoftExcelVersion <> 0 then
        AppendFilter(sMicrosoftExcelFilter);
      AppendFilter(sGraphicFilter);
      AppendFilter(sAllFilter);
      OpenDialog.Filter := Filter;
      OpenDialog.FilterIndex := FilterIndex;
    end;
end;

procedure TDeImage.onPopup(Sender:TObject);
begin
  MI_Open.Enabled := ((0 < Length(FFileName)) or (FImageType <> itUnknown))  and (0 < Length(FFileData));
  MI_LoadTo.Enabled := (0 < Length(FFileData));
  MI_Empty.Enabled := Not ReadOnly and (0 < Length(FFileData));
  MI_Paste.Enabled := Not ReadOnly and DeClipboard.HasFormat(CF_HDROP) or DeClipboard.HasFormat(CF_BITMAP);
  MI_Cut.Enabled := Not ReadOnly and (ImageType <> itNone);
  MI_Copy.Enabled := (ImageType <> itNone);
  if Assigned(MI_Export) then
    MI_Export.Enabled := MI_LoadTo.Enabled;
  // MI_LoadFrom.Enabled := True; // Эти по умолчанию всегда включены ...
  // MI_LinkTo.Enabled := True;
end;

procedure TDeImage.onOpen(Sender: TObject);
var //proc : TFileUpdateEvent;
    s : string;
    i : Integer;
    Thread: TDeThread;
    List: TList;
begin
  if FileIsEditable then
    begin
      {
      i:=0;
      s:=DeTempDir + '\~' + FFileName;
      while FileExists(s) do
        begin
          Inc(i);
          s:=DeTempDir + '\~'+IntToStr(i)+ FFileName;
        end;
      }
      if length(FFileName)=0
        then s:= GetTempFile('~','.'+GraphicExtension(TGraphicClass(FGraphic.ClassType)))
        else S:= GetTempFile('~', FFileName);
      SaveToFile(s);

      if SameText(ExtractFileExt(FFileName),sExtensionLNK) then
        begin
          ShellExecute(Self.Handle, 'open', PChar(s), nil, nil, SW_SHOWNORMAL);
          DeleteFile(PChar(s));
        end
      else
        begin
          I := -1;
          Thread := TDeThread.Create(s, OnThreadChange, OnThreadDone);
          try
            List := FThreads.LockList;
            try
              I := List.Add(Thread);
            finally
              FThreads.UnlockList;
            end;
          finally
            if I <> -1 then
              begin
                {$IFDEF DEBUG}
                DebugLog('Monitoring file START ' + Thread.FileName);
                {$ENDIF}
                Thread.Start;
              end
            else
              Thread.Free;
          end;
          {
          proc := LoadFromFile;

          if not Assigned(FileManager) then
            FileManager := TFileDataManager.Create;

          FileManager.Add(s, Self, Self, proc);
          }
        end;
    end;
end;

procedure TDeImage.onLoadFrom(Sender: TObject);
var
  ODlg: TOpenDialog;
  ZipFile: TZipFile;
  FileName: string;
  Index: Integer;
  Gauge: TGauge;
begin
  ODlg := TOpenDialog.Create(self);
  try
    ODlg.Title := TMenuItem(Sender).Caption;
    if IntDeImageLastDir = EmptyStr then
      ODlg.InitialDir := InitialDir
    else
      ODlg.InitialDir := IntDeImageLastDir;
    PrepareDialogFilters(ODlg);
    {
    ODlg.Filter := GetTitle(dlgFilters);
    ODlg.FilterIndex := dlgFilterAll;
    }
    if FMultipleFiles then
      ODlg.Options := ODlg.Options + [ofAllowMultiSelect];
    if ODlg.Execute then begin
      if ODlg.Files.Count <> 1 then
        begin
          //FileName := TPath.GetTempPath + FormatDateTime('"Compress"DDMMYYYYHHNNSS', Now) + sExtensionZIP;
          FileName := GetTempFile(FormatDateTime('"Compress"DDMMYYYYHHNNSS', Now), sExtensionZIP);
          try
            ZipFile := TZipFile.Create;
            try
              ZipFile.Open(FileName, zmWrite);
              try
                Gauge := TGauge.Create(Self);
                try
                  Gauge.Parent := Self;
                  Gauge.Align := alClient;
                  Gauge.Kind := gkVerticalBar;
                  Gauge.BorderStyle := bsNone;
                  Gauge.MaxValue := ODlg.Files.Count;
                  Gauge.BackColor := Color;
                  Gauge.ForeColor := clActiveCaption;
                  for Index := 0 to Pred(ODlg.Files.Count) do
                    begin
                      ZipFile.Add(ODlg.Files[Index]);
                      Gauge.AddProgress(1);
                    end;
                finally
                  Gauge.Free;
                end;
              finally
                ZipFile.Close;
              end;
            finally
              ZipFile.Free;
            end;
            LoadFromFile(FileName);
          finally
            if FileExists(FileName) then SysUtils.DeleteFile(FileName);
          end;
        end
      else
        LoadFromFile(ODlg.FileName);
      IntDeImageLastDir := ExtractFilePath(ODlg.FileName);
    end;
  finally
    ODlg.Free;
  end;
end;

procedure TDeImage.onLoadTo(Sender: TObject);
var SDlg : TSaveDialog;
    Extension: string;
function IsFileExtension(const Extensions: array of string): Boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := Low(Extensions) to High(Extensions) do
    if SameText(Extension, Extensions[Index]) then
      begin
        Result := True;
        Break;
      end;
end;
begin
  SDlg := TSaveDialog.Create(Self);
  try
    SDlg.Title := TMenuItem(Sender).Caption;
    if Length(IntDeImageLastDir) = 0 then
      SDlg.InitialDir := InitialDir
    else
      SDlg.InitialDir := IntDeImageLastDir;
    SDlg.FileName := FFileName;
    if FImageType = itUnknown then
      begin
        Extension := ExtractFileExt(FFileName);
        if IsFileExtension([sExtensionDOC, sExtensionDOCX, sExtensionRTF]) then
          begin
            SDlg.Filter := sMicrosoftWordFilter;
            SDlg.DefaultExt := sExtensionDOC;
          end
        else if IsFileExtension([sExtensionXLS, sExtensionXLSX]) then
          begin
            SDlg.Filter := sMicrosoftExcelFilter;
            SDlg.DefaultExt := sExtensionXLS;
          end
        else if IsFileExtension([sExtensionTXT]) then
          begin
            SDlg.Filter := sTextFilter;
            SDlg.DefaultExt := sExtensionTXT;
          end
        else if IsFileExtension([sExtensionZIP]) then
          begin
            SDlg.Filter := sZipFilter;
            SDlg.DefaultExt := sExtensionZIP;
          end
        else if IsFileExtension([sExtensionLNK]) then
          begin
            SDlg.Filter := sShellLinkFilter;
            SDlg.DefaultExt := sExtensionLNK;
          end;
      end
    else
      if Assigned(FGraphic) then
        begin
          SDlg.Filter := GraphicFilter(TGraphicClass(FGraphic.ClassType));
          SDlg.DefaultExt := GraphicExtension(TGraphicClass(FGraphic.ClassType));
        end;
    SDlg.FileName := ExtractFileName(FFileName);
    if SDlg.Execute then
    begin
      SaveToFile(SDlg.FileName);
      IntDeImageLastDir := ExtractFilePath(SDlg.FileName);
    end;
  finally
    SDlg.Free;
  end;
end;

procedure TDeImage.onEmpty(Sender: TObject);
begin
  FFileName := EmptyStr;
  SetValue({EmptyStr} Null);
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TDeImage.ExportToFile(const FileName: string);
var
  Stream: TStream;
  Value: Variant;
  Buffer: Pointer;
  BufferSize: Integer;
begin
  Value := Self.Value;
  BufferSize := VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1);
  Buffer := VarArrayLock(Value);
  try
    Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    try
      Stream.WriteBuffer(Buffer^, BufferSize);
    finally
      Stream.Free;
    end;
  finally
    VarArrayUnlock(Value);
  end;
end;

procedure TDeImage.OnExportTo(Sender: TObject);
var
  Dialog: TSaveDialog;
begin
  Dialog := TSaveDialog.Create(Self);
  try
    Dialog.Title := TMenuItem(Sender).Caption;
    if Length(IntDeImageLastDir) = 0 then
      Dialog.InitialDir := InitialDir
    else
      Dialog.InitialDir := IntDeImageLastDir;
    Dialog.FileName := ChangeFileExt(ExtractFileName(FFileName), sExtensionDAT);
    Dialog.Filter := sInternalFileFilter;
    Dialog.DefaultExt := sExtensionDAT;
    if Dialog.Execute then
      begin
        ExportToFile(Dialog.FileName);
        IntDeImageLastDir := ExtractFilePath(Dialog.FileName);
      end;
  finally
    Dialog.Free;
  end;
end;

procedure TDeImage.SetValue(const aValue: Variant);
var
  Buf: AnsiString;
  FileNameLength: Integer;
  FileDataLength: Integer;
  FileVersion: Word;
  FileOffset: NativeInt;
  FileName: AnsiString;
  P: Pointer;
  BlockSize: Integer;
  SourceStream, PackStream: TStream;
begin
  if not VarIsArray(aValue) then
  begin
    FFileName := EmptyStr;
    FVersion := 0;
    FModified := 0;
    FAuthor := EmptyStr;
    SetLength(FFileData, 0);
  end
  else
  begin
    BlockSize := VarArrayHighBound(aValue, 1) - VarArrayLowBound(aValue, 1);
    SetLength(Buf, BlockSize);
    P := VarArrayLock(aValue);
    try
      Move(P^, Buf[1], BlockSize);
    finally
      VarArrayUnlock(aValue);
    end;
    if Copy(Buf, 1, DeSLength) = DeSignature2 then
    begin
      FileOffset := Succ(DeSLength);
      FileVersion := PWord(@Buf[FileOffset])^;
      FileOffset := FileOffset + SizeOf(FileVersion);
      Move(Buf[FileOffset], FVersion, SizeOf(FVersion));
      FileOffset := FileOffset + SizeOf(FVersion);
      Move(Buf[FileOffset], FModified, SizeOf(FModified));
      FileOffset := FileOffset + SizeOf(FModified);
      FileNameLength := PSmallint(@Buf[FileOffset])^;
      FileOffset := FileOffset + SizeOf(Smallint);
      SetLength(FileName, FileNameLength);
      Move(Buf[FileOffset], FileName[1], FileNameLength);
      FileOffset := FileOffset + FileNameLength;
      FFileName := FileName;
      FileNameLength := PSmallint(@Buf[FileOffset])^;
      FileOffset := FileOffset + SizeOf(Smallint);
      SetLength(FileName, FileNameLength);
      Move(Buf[FileOffset], FileName[1], FileNameLength);
      FileOffset := FileOffset + FileNameLength;
      FAuthor := FileName;

      FileDataLength := PInteger(@Buf[FileOffset])^;
      FileOffset := FileOffset + SizeOf(Integer);
      SetLength(FFileData, FileDataLength);
      if (FileVersion and 1) = 0 then
        Move(Buf[FileOffset], FFileData[0], FileDataLength)
      else
        begin
          FileNameLength := PInteger(@Buf[FileOffset])^;
          FileOffset := FileOffset + SizeOf(Integer);
          SourceStream := TMemoryStream.Create;
          try
            SourceStream.WriteBuffer(Buf[FileOffset], FileNameLength);
            SourceStream.Position := 0;
            PackStream := TDecompressionStream.Create(SourceStream);
            try
              PackStream.ReadBuffer(FFileData[0], FileDataLength);
            finally
              PackStream.Free;
            end;
          finally
            SourceStream.Free;
          end;
        end;
    end
    else if Copy(Buf, 1, DeSLength) = DeSignature then
    begin
      FVersion := 0;
      FModified := 0;
      FAuthor := EmptyStr;
      FileNameLength := PSmallint(@Buf[DeSLength + 1])^;
      SetLength(FileName, FileNameLength);
      Move(Buf[DeSLength + SizeOf(Smallint) + 1], FileName[1], FileNameLength);
      FileDataLength := Length(Buf) - (DeSLength + SizeOf(Smallint) + FileNameLength);
      FFileName := FileName;
      SetLength(FFileData, FileDataLength);
      Move(Buf[DeSLength + SizeOf(Smallint) + FileNameLength + 1],
        FFileData[0], FileDataLength);
    end
    else
    begin
      FFileName := EmptyStr;
      FVersion := 0;
      FModified := 0;
      FAuthor := EmptyStr;
      FileDataLength := Length(Buf);
      SetLength(FFileData, FileDataLength);
      Move(Buf[1], FFileData[0], FileDataLength);
    end;

  end;
  HintUpdate;
  UpdateImage(True);
end;

function TDeImage.GetValue: Variant;
var
//  Buf: array of Byte;
  Buf, FileName: AnsiString;
  FileNameLength: Integer;
  FileDataLength: Integer;
  P: Pointer;
  ImageStoreMode: Integer;
  TargetStream, PackStream, Stream: TStream;
  FileVersion: Word;
  FilePacked: Boolean;
  Extension, Extensions: string;
begin
  FileDataLength := Length(FFileData);
  if Length(FFileName) = 0 then
    begin
      SetLength(Buf, FileDataLength);
      Move(FFileData[0], Buf[1], FileDataLength);
    end
  else
    begin
      ImageStoreMode := Variables.AsInteger[RegImageStoreMode];
      if ImageStoreMode <> 0 then
        begin
          TargetStream := TMemoryStream.Create;
          try
            TargetStream.WriteBuffer(DeSignature2[1], DeSLength);
            FileVersion := $0100;
            if (ImageStoreMode and 1) <> 0 then
              FileVersion := FileVersion or $0001
            else
              begin
                Extensions := Trim(Variables.AsString[RegImageUnpackExt]);
                Extension := ExtractFileExt(FFileName);
                FilePacked := True;
                while Length(Extensions) <> 0 do
                  if SameText(Trim(CutTextValue(Extensions, ';')), Extension) then
                    begin
                      FilePacked := False;
                      Break;
                    end
                  else
                    Extensions := Trim(Extensions);
                if FilePacked then
                  FileVersion := FileVersion or $0001;
              end;
            TargetStream.WriteBuffer(FileVersion, SizeOf(FileVersion));
            TargetStream.WriteBuffer(FVersion, SizeOf(FVersion));
            TargetStream.WriteBuffer(FModified, SizeOf(FModified));
            FileName := FFileName;
            FileNameLength := Length(FileName) and $FFFF;
            TargetStream.WriteBuffer(FileNameLength, SizeOf(SmallInt));
            TargetStream.WriteBuffer(FileName[1], FileNameLength);
            FileName := FAuthor;
            FileNameLength := Length(FileName) and $FFFF;
            TargetStream.WriteBuffer(FileNameLength, SizeOf(SmallInt));
            TargetStream.WriteBuffer(FileName[1], FileNameLength);
            FileDataLength := Length(FFileData);
            TargetStream.WriteBuffer(FileDataLength, SizeOf(FileDataLength));
            if (FileVersion and 1) = 0 then
              TargetStream.WriteBuffer(FFileData[0], FileDataLength)
            else
              begin
                Stream := TMemoryStream.Create;
                try
                  PackStream := TCompressionStream.Create(clMax, Stream);
                  try
                    PackStream.WriteBuffer(FFileData[0], FileDataLength);
                  finally
                    PackStream.Free;
                  end;
                  Stream.Position := 0;
                  FileDataLength := Stream.Size;
                  TargetStream.WriteBuffer(FileDataLength, SizeOf(FileDataLength));
                  TargetStream.CopyFrom(Stream, FileDataLength);
                finally
                  Stream.Free;
                end;
              end;
            SetLength(Buf, TargetStream.Size);
            TargetStream.Position := 0;
            TargetStream.ReadBuffer(Buf[1], Length(Buf));
          finally
            TargetStream.Free;
          end;
        end
      else
        begin
          FileName := FFileName;
          FileNameLength := Length(FileName);
          SetLength(Buf, DeSLength + SizeOf(Smallint) + FileNameLength + FileDataLength);
          Move(DeSignature[1], Buf[1], DeSLength);
          PSmallint(@Buf[DeSLength + 1])^ := FileNameLength;
          Move(FileName[1], Buf[DeSLength + SizeOf(Smallint) + 1], FileNameLength);
          Move(FFileData[0], Buf[DeSLength + SizeOf(Smallint) + FileNameLength + 1], FileDataLength);
        end;
    end;
  Result := VarArrayCreate([0, Length(Buf)], varByte);
  P := VarArrayLock(Result);
  try
    Move(Buf[1], P^, Length(Buf));
  finally
    VarArrayUnlock(Result);
  end;
end;

function TDeImage.GetDataValue: Variant;
var
  Size: Integer;
  DataPtr: Pointer;
begin
  Size := Length(FFileData);
  if Size = 0 then
    Result := Null
  else
    begin
      Result := VarArrayCreate([0, Size], varByte);
      DataPtr := VarArrayLock(Result);
      try
        Move(FFileData[0], DataPtr^, Size);
      finally
        VarArrayUnlock(Result);
      end;
    end;
end;

function TDeImage.FileIsEditable: Boolean;
begin
  Result := FImageType <> itNone;
end;

function TDeImage.LoadFromFile(const FileName: string; const OriginalFileName: string; const Version: Cardinal): Boolean;
var
  Stream: TFileStream;
  FSize : Integer;
begin
  Result := True;
  if Length(OriginalFileName) <> 0 then
    FFileName := ExtractFileName(OriginalFileName)
  else
    begin
      FFileName := ExtractFileName(FileName);
      while FFileName[1] = '~' do Delete(FFileName, 1, 1);
    end;

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    FSize:=Stream.Size;
    SetLength(FFileData, FSize);
    Stream.Position := 0;
    Stream.ReadBuffer(FFileData[0], FSize);
    FVersion := Version;
    FModified := FileDateToDateTime(FileAge(FileName));
    FAuthor := GetWindowsUserName;
    HintUpdate;
    UpdateImage(True);
  except
    Result := False;
  end;

  Stream.Free;
  if Assigned(FOnChange) then FOnChange(Self);

  if not Application.MainForm.Focused then
    FlashWindow(Application.Handle, True);
end;

function TDeImage.SaveToFile(const FileName: string): Boolean;
var
  Stream: TFileStream;
begin
  Result := False;
  if (FileName = EmptyStr) or (Length(FFileData) = 0) then Exit;
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Stream.Position := 0;
    Stream.Write(FFileData[0], Length(FFileData));
    Result := True;
  finally
    Stream.Free;
  end;
  if FModified <> 0 then FileSetDate(FileName, DateTimeToFileDate(FModified));
end;

function   TDeImage.SaveToString(var aString: AnsiString): Boolean;
begin
  SetLength(aString,Length(FFileData));
  Move(FFileData[0],aString[1], Length(FFileData));
  Result := True;
end;

function TDeImage.SaveToBitmap(const FileName: string): Boolean;
var
  Bitmap: Graphics.TBitmap;
begin
  Result := Assigned(Graphic);
  if Result then
    begin
      Bitmap := Graphics.TBitmap.Create;
      try
        Bitmap.Assign(FGraphic);
        Bitmap.SaveToFile(FileName);
        if FModified <> 0 then FileSetDate(FileName, DateTimeToFileDate(FModified));
      finally
        Bitmap.Free;
      end;
    end;
end;

procedure TDeImage.UpdateImage(NewSource: Boolean);
var
  aFile, aTempFile, Extension: string;
  LinkFileData:TByteDynArray;
  LinkFileSize:integer;
  Stream:TStream;
  ZipFile: TZipFile;
  procedure LoadRegistryIcon(const aFileName: string);
    function CheckOverIcon(const NeedExtension: string; const ResourceName: string): Boolean;
    begin
      Result := SameText(Extension, NeedExtension) and
        (FindResource(hInstance, PChar(ResourceName), RT_GROUP_ICON) <> 0);
      if Result then
        begin
          FOverIcon := TIcon.Create;
          try
            FOverIcon.LoadFromResourceName(hInstance, ResourceName);
          except
            FreeAndNil(FOverIcon);
            raise;
          end;
        end;
    end;
  begin
    FGraphic := TIcon.Create;
    (FGraphic as TIcon).Handle := GetRegistryIconHandle(aFileName);
    // При необходимости получим иконку наложения ...
    if not CheckOverIcon(sExtensionLNK, 'P__LNK') then
      CheckOverIcon(sExtensionZIP, 'P__ZIP');
  end;

begin
//  if not NewSource then UpdateFileData;
  FreeAndNil(FGraphic);
  FreeAndNil(FOverIcon);
//  if Assigned(FBitmap) then FreeAndNil(FBitmap);
  if Length(FFileData) = 0 then
    FImageType := itNone
  else if not IsLoadedImage(TByteDynArray(FFileData))
  then
  begin
    Extension := ExtractFileExt(FFileName);
    if SameText(Extension, sExtensionLNK) then
      begin
        aFile := GetTempFile;
        try
          SaveToFile(aFile);
          Stream := TFileStream.Create(GetFileNameFromLink(aFile), fmOpenRead or fmShareDenyNone);
          try
            LinkFileSize:=Stream.Size;
            SetLength(LinkFileData, LinkFileSize);
            Stream.Position := 0;
            Stream.ReadBuffer(LinkFileData[0], LinkFileSize);
          finally
            Stream.Free;
          end;
          if not IsLoadedImage(LinkFileData) then
            begin
              LoadRegistryIcon(GetFileNamefromLink(aFile));
              FImageType := itUnknown;
            end;
          SetLength(LinkFileData,0);
        finally
          SysUtils.DeleteFile(aFile);
        end;
      end
    else if SameText(Extension, sExtensionZIP) then
      begin
        //aFile := TPath.GetTempPath + FFileName + sExtensionZIP;
        aFile := GetTempFile(FFileName, sExtensionZIP);
        try
          SaveToFile(aFile);
          if ZipFile.IsValid(aFile) then
            begin
              ZipFile := TZipFile.Create;
              try
                ZipFile.Open(aFile, zmRead);
                try
                  if ZipFile.FileCount = 1 then
                    begin
                      //aTempFile := TPath.GetTempPath + ExtractFileName(ZipFile.FileName[0]);
                      aTempFile := ExtractFileName(ZipFile.FileName[0]);
                      aTempFile := GetTempFile(ChangeFileExt(aTempFile, EmptyStr), ExtractFileExt(aTempFile));
                      try
                        ZipFile.Extract(ZipFile.FileName[0], ExtractFilePath(aTempFile), False);
                        Stream := TFileStream.Create(aTempFile, fmOpenRead or fmShareDenyNone);
                        try
                          LinkFileSize:=Stream.Size;
                          SetLength(LinkFileData, LinkFileSize);
                          Stream.ReadBuffer(LinkFileData[0], LinkFileSize);
                        finally
                          Stream.Free;
                        end;
                        if not IsLoadedImage(LinkFileData) then
                          begin
                            LoadRegistryIcon(aTempFile);
                            FImageType := itUnknown;
                          end;
                        SetLength(LinkFileData,0);
                      finally
                        if FileExists(aTempFile) then SysUtils.DeleteFile(aTempFile);
                      end;
                    end
                  else
                    begin
                      LoadRegistryIcon(aFile);
                      FImageType := itUnknown;
                    end;
                finally
                  ZipFile.Close;
                end;
              finally
                ZipFile.Free;
              end;
            end
          else
            begin
              LoadRegistryIcon(aFile);
              FImageType := itUnknown;
            end;
        finally
          if FileExists(aFile) then SysUtils.DeleteFile(aFile);
        end;
      end
    else
      begin
        LoadRegistryIcon(FFileName);
        FImageType := itUnknown
      end;
  end;
{  if NewSource then
  begin
    if Assigned(FThread) and not FThread.Terminated then
    begin
      if FThread.TempFileDeleted then
      FThread.Terminate;
      {while not}  //do;
    //  FThread.Free;
//    end;

{    if FileIsEditable then
    begin
      if Assigned(FThread) then
      begin
        while not FThread.TempFileDeleted do;
        FThread.Free;
      end;
      FThread := TDeFileNotificationThread.Create(False);
      FThread.FDeImage := Self;
    end; }
//  end;
  Invalidate;
end;

procedure TDeImage.HintUpdate;
var
  Value: string;
  Size: Integer;
begin
  Size := Length(FFileData);
  if Size = 0 then
    Value := EmptyStr
  else
    begin
      Value := IntToStr(Size);
      if Length(Value) > 3 then Insert(' ', Value, Length(Value) - 2);
      if Length(Value)> 7 then Insert(' ', Value, Length(Value) - 6);
      if Length(Value) > 11 then Insert(' ', Value, Length(Value) - 10);
      Value := GetTitle('_dF.Size: ') + Value + ' b';
    end;
  if (FVersion <> 0) and (FVersion <> 100) then
    begin
      if Length(Value) <> 0 then Value := Value + #13#10;
      Value := Value + GetTitle('_dF.Version: ') + Format('%.2f', [FVersion / 100]);
    end;
  if FModified <> 0 then
    begin
      if Length(Value) <> 0 then Value := Value + #13#10;
      Value := Value + GetTitle('_dF.Modified: ') + FormatDateTime('DD"."MM"."YYYY" "HH":"NN', FModified);
    end;
  if Length(FAuthor) <> 0 then
    begin
      if Length(Value) <> 0 then Value := Value + #13#10;
      Value := Value + GetTitle('_dF.Author: ') + FAuthor;
    end;
  if Length(FFileName) <> 0 then
    Value := GetTitle('_dF.Name: ') + FFileName + #13#10 + Value;
  Hint := Value;
end;

procedure TDeImage.SetAuthor(const Value: string);
begin
  if not SameText(FAuthor, Value) then
    begin
      FAuthor := Value;
      HintUpdate;
    end;
end;

procedure TDeImage.SetModified(const Value: TDateTime);
begin
  if FModified <> Value then
    begin
      FModified := Value;
      HintUpdate;
    end;
end;

procedure TDeImage.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  MI_LoadFrom.Enabled:= not FReadOnly;
  MI_ScanFrom.Enabled:= not FReadOnly;
  MI_Cut.Enabled:= not FReadOnly;
  MI_Paste.Enabled:= not FReadOnly;
  MI_Empty.Enabled:= not FReadOnly;
end;

{ TDeImage.TDeThread }

procedure TDeImage.TDeThread.ChangeNotification;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TDeImage.TDeThread.Create(const AFileName: string; AOnChange, AOnDestroy: TNotifyEvent);
begin
  inherited Create(True);
  Priority := tpLower;
  OnTerminate := AOnDestroy;
  FreeOnTerminate := True;
  FFileName := AFileName;
  if not FileAge(FFileName, FFileDate) then
    FFileDate := 0;
  FOnChange := AOnChange;
end;

procedure TDeImage.TDeThread.Execute;
var
  ProcessInformation: TProcessInformation;
  StartupInfo: TStartupInfo;
  ErrorCode: Cardinal;
  DateTime: TDateTime;
begin
  FErrorMessage := EmptyStr;
  try
    ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
    StartupInfo.cb := SizeOf(StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := {$IFDEF DEBUG}SW_MINIMIZE{$ELSE}SW_HIDE{$ENDIF};
    ZeroMemory(@ProcessInformation, SizeOf(ProcessInformation));
    if CreateProcess(nil, PChar('cmd.exe /c ' + AnsiQuotedStr(FFileName,'"')),
       nil, nil, False, NORMAL_PRIORITY_CLASS, nil,
       PChar(GetCurrentDir), StartupInfo, ProcessInformation) then
      begin
        while not Terminated do
          case WaitForSingleObject(ProcessInformation.hProcess, 200) of
            WAIT_FAILED: { Ошибка }
              RaiseLastOSError;
            WAIT_OBJECT_0: { Готов }
              begin
                if GetExitCodeProcess(ProcessInformation.hProcess, ErrorCode) then
                  if ErrorCode = STILL_ACTIVE then
                    Sleep(200)
                  else
                    begin
                      if FileExists(FileName) and FileAge(FileName, DateTime) then
                        if DateTime <> FileDate then
                          Synchronize(ChangeNotification);
                      Break;
                    end;
              end;
          else
            Sleep(300);
          end;
        CloseHandle(ProcessInformation.hThread);
        CloseHandle(ProcessInformation.hProcess);
      end
    else
      RaiseLastOSError;
  except
    on E: Exception do FErrorMessage := E.Message;
  end;
end;

{ --- TDeDateTimePicker ------------------------------------------------------ }

constructor TDEDateTimePicker.Create(AOwner: TComponent);
  procedure AddMenuItem( aPopupMenu: TPopupMenu; var aMenuItem: TMenuItem;
                        const aCaption: string; aEvent: TNotifyEvent; const aDefault: Boolean = False);
  begin
    aMenuItem:=TMenuItem.Create(self);
    aMenuItem.Caption := GetTitle(aCaption);
    aMenuItem.OnClick := aEvent;
    aMenuItem.Default := aDefault;
    aPopupMenu.Items.Add(aMenuItem);
  end;
begin
  inherited Create(AOwner);
  FBackFormat:= Format;
  FIsNull :=True;

  FAllowNull:= True;
  FNullText := EmptyStr;

  FPopupMenu:= TPopupMenu.Create(self);
  FPopupMenu.OnPopup := OnDSCPopupMenu;
  PopupMenu:=FPopupMenu;

  AddMenuItem(FPopupMenu, MI_Cut,    '_Da.Cut',    OnDSCCutClick);
  AddMenuItem(FPopupMenu, MI_Copy,   '_Da.Copy',   OnDSCCopyClick);
  AddMenuItem(FPopupMenu, MI_Paste,  '_Da.Paste',  OnDSCPasteClick);
  AddMenuItem(FPopupMenu, MI_Sep1,   cLineCaption, nil);
  AddMenuItem(FPopupMenu, MI_Delete, '_Da.Delete', OnDSCDeleteClick);
//AddMenuItem(FPopupMenu, MI_Default,'_Da.Default',OnDSCDefaultClick);
  AddMenuItem(FPopupMenu, MI_Sep2,   cLineCaption, nil);
  AddMenuItem(FPopupMenu, MI_Undo,   '_Da.Undo',   OnDSCUndoClick);

  OnEnter :=  OnDSCEnter;
  OnExit  :=  OnDSCExit;
end;

procedure TDEDateTimePicker.OnDSCCopyClick(Sender: TObject);
begin
  DeClipboard.WriteValue(Value, Text);
end;

procedure TDEDateTimePicker.OnDSCPasteClick(Sender: TObject);
var TempValue : Variant;
begin
  if DeClipboard.ReadValue(TempValue, ftDateTime) then
    InnerSetValue(TempValue);

  if assigned(OnChange) then OnChange(Sender);
end;

procedure TDEDateTimePicker.OnDSCCutClick(Sender: TObject);
begin
  OnDSCCopyClick(Sender);
  InnerSetValue(Null);

  if assigned(OnChange) then OnChange(Sender);
end;

procedure TDEDateTimePicker.OnDSCDeleteClick(Sender: TObject);
begin
  InnerSetValue(Null);

  if assigned(OnChange) then OnChange(Sender);
end;

procedure TDEDateTimePicker.OnDSCDefaultClick(Sender: TObject);
begin
  //
end;

procedure TDEDateTimePicker.OnDSCUndoClick(Sender: TObject);
begin
  InnerSetValue(FUndoValue);

  if assigned(OnChange) then OnChange(Sender);
end;

procedure TDEDateTimePicker.OnDSCEnter(Sender: TObject);
begin
  MI_Cut.ShortCut   := ShortCut(Word('X'), [ssCtrl]);;
  MI_Copy.ShortCut  := ShortCut(Word('C'), [ssCtrl]);;
  MI_Paste.ShortCut := ShortCut(Word('V'), [ssCtrl]);;
end;

procedure TDEDateTimePicker.OnDSCExit(Sender: TObject);
begin
  MI_Cut.ShortCut   := null;
  MI_Copy.ShortCut  := null;
  MI_Paste.ShortCut := null;
end;

procedure TDEDateTimePicker.OnDSCPopupMenu(Sender:TObject);
var HaveValue      : Boolean;
    HaveValue2     : Boolean;
begin
  HaveValue  := Not( VarIsNull(Value) or VarIsEmpty(Value) );
  HaveValue2 := Not( VarIsNull(FUndoValue) or VarIsEmpty(FUndoValue) );

  MI_Delete.Enabled := Enabled and HaveValue and AllowNull;
  MI_Cut.Enabled    := Enabled and HaveValue and AllowNull;
  MI_Copy.Enabled   := HaveValue;
  MI_Paste.Enabled  := DeClipBoard.HasValueType(ftDateTime);
  MI_Undo.Enabled   := (Enabled) and (not DeVarSameValue(Value, FUndoValue)) and (HaveValue or HaveValue2);
  try
    if Enabled then SetFocus;
  finally
  end;
end;

function TDEDateTimePicker.GetValue: Variant;
begin
  if IsNull and FAllowNull then Result := Null
                           else Result := DateTime;
end;

procedure TDEDateTimePicker.SetAllowNull(const aValue: Boolean);
begin
  FAllowNull:= aValue;
  InnerSetValue(DateTime);
end;

procedure TDEDateTimePicker.CheckEmptyDate;
begin
  if not FAllowNull then raise EDateTimeError.Create('Date must be set');
  Invalidate;
end;

procedure TDEDateTimePicker.SetFormat(const aFormat: string);
begin
  FBackFormat := aFormat;
  if IsNull then TDateTimePicker(self).Format:= ''''+FNullText+''''
            else TDateTimePicker(self).Format:= aFormat;
end;

procedure TDEDateTimePicker.InnerSetValue(aValue: Variant);
begin
  FIsNull:= (aValue = unassigned) or VarIsNull(aValue);

  if FIsNull then
    begin
      if Kind = dtkTime then DateTime:= Now
                        else DateTime:= SysUtils.Date;
      TDateTimePicker(self).Format:= ''''+FNullText+''''
    end
  else
    begin
      if VarIsStr(aValue) then
        DateTime := VarAsType(aValue, varDate)
      else
        DateTime:= aValue;
      TDateTimePicker(self).Format:= FBackFormat;
    end;
end;

procedure TDEDateTimePicker.Change;
begin
  InnerSetValue(DateTime);
  inherited;
end;

procedure TDEDateTimePicker.SetValue(aValue: Variant);
begin
  FUndoValue:= aValue;
  InnerSetValue(aValue);
end;

procedure TDEDateTimePicker.WMSetFocus(var Msg: TMessage);
begin
  TDateTimePicker(self).Format:= FBackFormat;
  inherited;
end;

// Нужно установить фокус редактирования ИМЕННО на первую часть даты по нажатию первой цифры
// Не понимаю как, но это работает. Подсмотрел тут
// https://social.msdn.microsoft.com/Forums/en-US/fc125ad0-c08d-4cbe-81cd-d75a5e6c840a/datetimepicker-focus-on-the-day-part?forum=winforms
procedure TDEDateTimePicker.WMChar(var Msg: TMessage);
begin
  if (Value = Null) and (Msg.WParamLo in [32]) then  // 32 Пробел - просто ставим текущую дату
    begin
      InnerSetValue(Date);
      if assigned(OnChange) then OnChange(TObject(self));
    end;

  if (Value = Null) and (Msg.WParamLo in [48..57]) then // 48-57 Цифры с обеих клавиатур
    begin                                               // начинаем первой же цифрой вводить новую дату
      InnerSetValue(Date);
      RecreateWnd;
    end;

  inherited;
end;

procedure TDEDateTimePicker.WMKeyDown(var Msg: TMessage);
begin
  if not (Value = Null) and (Msg.WParamLo in [8,46]) then // 8	Клавиша Backspace; 46 Клавиша DEL
    begin
      InnerSetValue(Null);
      if assigned(OnChange) then OnChange(TObject(self));
    end;

  inherited;
end;

procedure TDEDateTimePicker.WMKillFocus(var Msg: TMessage);
begin
  if FIsNull then
    TDateTimePicker(self).Format:= ''''+FNullText+''''
  else
    TDateTimePicker(self).Format:= FBackFormat;
  inherited;
end;

procedure TDEDateTimePicker.WMMouseWheel(var Msg: TWMMouseWheel);
begin
  if FIsNull then  if Kind = dtkTime then InnerSetValue(Now)
                                     else InnerSetValue(SysUtils.Date)
             else  if Msg.WheelDelta > 0 then keybd_event( VK_UP,   0, KEYEVENTF_EXTENDEDKEY, 0)
                                         else keybd_event( VK_DOWN, 0, KEYEVENTF_EXTENDEDKEY, 0);
end;

constructor TDETimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Kind     := dtkTime ;
  DateMode := dmUpDown;
//  OldFormat := 'H:mm:ss';
end;

{ --- TDeDSComboBox ------------------------------------------------------------ }

constructor TDeDSComboBox.Create (AOwner: TComponent);

procedure AddMenuItem( aPopupMenu: TPopupMenu; var aMenuItem: TMenuItem;
                      const aCaption: string; aEvent: TNotifyEvent; const aDefault: Boolean = False);
begin
  aMenuItem:=TMenuItem.Create(self);
  aMenuItem.Caption := GetTitle(aCaption);
  aMenuItem.OnClick := aEvent;
  aMenuItem.Default := aDefault;
  aPopupMenu.Items.Add(aMenuItem);
end;

begin
  inherited;// Create(AOwner);
  RepaintMes.Msg:= 0;
  AutoSize := False; // Иначе контрол подгоняется под текущий шрифт!!!
//  Сделать выпадающий список шире контрола
//  SendMessage(Handle, CB_SETDROPPEDWIDTH, 500, 0);

  OnCloseUp:= ExternalOnCloseUp;
  FTextPress := EmptyStr;
  FLastPress := EmptyStr;
  Style      := csOwnerDrawFixed;
  OnDrawItem := self.ComboBoxDrawItem;
  FDataID    := -1;
  FCache     := nil;
  FPrepared  := False;

  FViewList  := TFieldsMeta.Create(False);
  FAddNull   := 1;
  FNullText  := ' ***';
  FArrayText := ' *** %d ***';
  FDefault   := null;
  FTableMeta := nil;
  DropDownCount := 12;
  FFilterList:= nil;

  FPopupMenu:= TPopupMenu.Create(self);
  FPopupMenu.OnPopup := OnDSCPopupMenu;
  PopupMenu:=FPopupMenu;

  AddMenuItem(FPopupMenu, MI_Open,   '_Da.Open',   OnDSCOpenClick, True);
  AddMenuItem(FPopupMenu, MI_Create, '_Da.Create', OnDSCCreateClick);
  AddMenuItem(FPopupMenu, MI_Sep1,   cLineCaption, nil);
  AddMenuItem(FPopupMenu, MI_Cut,    '_Da.Cut',    OnDSCCutClick);
  AddMenuItem(FPopupMenu, MI_Copy,   '_Da.Copy',   OnDSCCopyClick);
  AddMenuItem(FPopupMenu, MI_Paste,  '_Da.Paste',  OnDSCPasteClick);
  AddMenuItem(FPopupMenu, MI_Sep2,   cLineCaption, nil);
  AddMenuItem(FPopupMenu, MI_Delete, '_Da.Delete', OnDSCDeleteClick);
  AddMenuItem(FPopupMenu, MI_Default,'_Da.Default',OnDSCDefaultClick);
  AddMenuItem(FPopupMenu, MI_Sep3,   cLineCaption, nil);
  AddMenuItem(FPopupMenu, MI_Undo,   '_Da.Undo',   OnDSCUndoClick);
end;

destructor TDeDSComboBox.Destroy;
begin
  OnDrawItem:=nil;

  // разрыв старой связи
  if FDataID > 0 then
    begin
      FTableMeta := MetaData.GetTableMeta(FDataID);
      if Assigned(FTableMeta.NField) then
        FTableMeta.NField.Controls.RemoveControl(Self);
    end;

  FViewList.Free;
  FPopupMenu.Free;
  if assigned(FCache) then FCache.Free;
  inherited Destroy;
end;

procedure TDeDSComboBox.DropDown;
begin
  try
    Prepared := True;
  except
    {$IFDEF DEBUG}
    on E: Exception do DebugLog('TDeDSComboBox.DropDown skip error: ' + E.Message);
    {$ENDIF}
  end;
  inherited DropDown;
end;

procedure TDeDSComboBox.CloseUp;
begin
  if not varIsArray(FValue) then
    if ItemIndex = FAddNull -1 then FValue:= null
                               else FValue:= FCache[ItemIndex - FAddNull].ID;
  TextPress:= EmptyStr;
  inherited Change;  // сохраняет изменение в ElementMeta
  inherited;
end;

procedure TDeDSComboBox.OnDSCPopupMenu(Sender:TObject);
var HaveValue      : Boolean;
    HaveValue2     : Boolean;
    CanPaste       : Boolean;
    aDataID, aSize : Integer;
    RecordID       : Variant;
begin
  HaveValue  := Not( VarIsNull(FValue) or VarIsEmpty(FValue) );
  HaveValue2 := Not( VarIsNull(FUndoValue) or VarIsEmpty(FUndoValue) );

// Проверяем наличие / совпадение класса / наличие строго одной записи
      CanPaste := DeClipboard.GetInfo(aDataID, aSize, RecordID);
  if CanPaste then
      CanPaste := ( FDataID = aDataID);
// Убрал ниже 2 строки, т.к. aSize теперь - количество записей!!!
//  if CanPaste then
//      CanPaste := (aSize = MetaData.GetTableMeta(FDataID).Fields.Count);
                                              // учтем что Unassigned <> Null
  MI_Undo.Enabled   := (Enabled) and (not DeVarSameValue(FValue, FUndoValue)) and (HaveValue or HaveValue2);
  MI_Cut.Enabled    := (Enabled) and HaveValue;
  MI_Copy.Enabled   := HaveValue;
  MI_Paste.Enabled  := (Enabled) and CanPaste;
  MI_Delete.Enabled := (Enabled) and AllowNull and HaveValue;
  MI_Default.Visible:= (Enabled) and not VarIsNull(FDefault);

  MI_Open.Enabled   := HaveValue;
//TODO: -2012 плохо работает синхрононизация после редактирования записи
  MI_Create.Enabled := (Enabled) and (Not ReadOnly);

  MI_Create.Visible := not (MetaData.GetTableMeta(FDataID).IsDirectory in [idLocal, idGlobal]);
end;

procedure TDeDSComboBox.OnDSCUndoClick(Sender:TObject);
begin
  InnerSetValue(FUndoValue);
  Change;
end;

procedure TDeDSComboBox.OnDSCCutClick(Sender:TObject);
begin
  OnDSCCopyClick(Sender);
  OnDSCDeleteClick(Sender);
end;

procedure TDeDSComboBox.ExternalOnCloseUp(Sender: TObject);
begin
  TextPress:= EmptyStr;
  inherited;
end;

procedure TDeDSComboBox.OnDSCCopyClick(Sender:TObject);
var
  CacheItem: TCacheItem;
  Index: Integer;
  procedure Select(const Value: Variant);
  begin
    CacheItem := FCache.FindById(Value);
    if Assigned(CacheItem) then CacheItem.Selected := True;
  end;
begin
  FCache.ClearSelection;
  if VarIsArray(FValue) then
    for Index := VarArrayLowBound(FValue, 1) to VarArrayHighBound(FValue, 1) do
      Select(VarArrayGet(FValue, [Index]))
  else
    Select(FValue);
  FCache.CopyToClipboard;
end;

procedure TDeDSComboBox.OnDSCPasteClick(Sender:TObject);
var
  ClipID, TempCount: Integer;
  RecordID: Variant;
begin
  if DeClipboard.GetInfo(ClipID, TempCount, RecordID) then
    begin
      if assigned(FCache) then FCache.CacheIndexes.Clear;
      InnerSetValue(RecordID);
      Change;
    end;
  SetFocus;
end;

procedure TDeDSComboBox.OnDSCDeleteClick(Sender:TObject);
begin
  InnerSetValue(Null);
  Change;
end;

procedure TDeDSComboBox.OnDSCDefaultClick(Sender:TObject);
begin
  InnerSetValue(FDefault);
  Change;
end;

procedure TDeDSComboBox.OnDSCOpenClick(Sender:TObject);
begin
  EditRecord(FTableMeta, FValue);
end;

procedure TDeDSComboBox.OnDSCCreateClick(Sender:TObject);
var RecordEditor  : TRecordEditor;
    DMan          : TDataManager;
begin
  // редактирование пользователем и сохранение новой записи в базе данных
  DMan := TDataManager.Create;
  DMan.Table:=FTableMeta;

  RecordEditor := TRecordEditor.Create(FTableMeta, null);
  DMan.PrepareRecord(RecordEditor.CacheItem);

  if RecordEditor.Execute then
  begin
    Items.BeginUpdate;
    DMan.SetPrimaryKey(RecordEditor.CacheItem);
    DMan.InsertRecord(RecordEditor.CacheItem);
    Value:= RecordEditor.CacheItem.ID;
    Change;
    Items.EndUpdate;
  end;

  RecordEditor.Free;
  DMan.Free;
end;

procedure TDeDSComboBox.SetDataID(const aDataID : integer);
var i : Integer;
begin
  if aDataID <> FDataID then
    begin
      Prepared:=False;
      if Assigned(FCache) then
        begin
          FCache.Free;
          FCache:=nil;
        end;

      // разрыв старой связи
      if FDataID > 0 then
        begin
          FTableMeta := MetaData.GetTableMeta(FDataID);
          if Assigned(FTableMeta.NField) then
            FTableMeta.NField.Controls.RemoveControl(Self);
          FViewWidth :=0;
          FViewList.Clear;
        end;
      FDataID := aDataID;
      // установка новой связи
      if FDataID > 0 then
        begin
          FTableMeta := MetaData.GetTableMeta(FDataID);
          if assigned(FTableMeta) then
            begin
              if Assigned(FTableMeta.NField) then
                FTableMeta.NField.Controls.AddControl(Self);
              FViewWidth :=0;
              FViewList.Clear;
              for i:=0 to FTableMeta.Fields.Count-1 do
                if ((FTableMeta.NField = FTableMeta.Fields[i]) or
                    (FTableMeta.Fields[i].OriginalVisibleLevel=fvLevel3)) and
                    (Not FTableMeta.Fields[i].IsLookup)  then
                  begin
                    Inc(FViewWidth,FTableMeta.Fields[i].Width);
                    FViewList.Add(FTableMeta.Fields[i])
                  end;
            end;
        end;
    end;
end;

procedure TDeDSComboBox.SetPrepared(const aValue: Boolean);
var FilterItem : TFilterItem;
    I          : integer;
    Field      : TFieldMeta;
    Parser     : TDeParser;
    Params     : TStringList;
    procedure CheckDependens;
    var
      Index: Integer;
    begin
      if Assigned(Field) and Field.Calculated and (Field.Stage <= fsBase) then
       for Index := 0 to Pred(FCache.Fields.Count) do
         if Field.DefaultPostfix.DependOnIdent(FCache.Fields[Index].Original) then
            FCache.Fields[Index].Stage := Field.Stage;
    end;
begin
  if aValue then
    begin
      if Not FPrepared then
        begin
          FError := EmptyStr;

          if Assigned(FCache) then
            begin
              FCache.Filters.Clear;
              FCache.ClearSortList;
              FCache.CacheIndexes.Clear;
            end
          else
            begin
              FCache := TDataCache.Create(FTableMeta, True);
              if Assigned(FCache.Fields) and (FCache.Fields.Count <> 0) and Assigned(FCache.TableMeta) then
                begin
                  for I := 0 to Pred(FCache.Fields.Count) do
                    begin
                      Field := FCache.Fields[I];
                      if Assigned(Field) then
                        if Field.Key then
                          Field.Stage := fsKey
                        else if Assigned(FCache.TableMeta.OField) and (FCache.TableMeta.OField.ID = Field.ID) then
                          Field.Stage := fsBase
//                      else if Assigned(FCache.TableMeta.GField) and (FCache.TableMeta.GField.ID = Field.ID) then
//                        Field.Stage := fsBase
                        else if Assigned(FCache.TableMeta.NField) and (FCache.TableMeta.NField.ID = Field.ID) then
                          Field.Stage := fsBase
                        else if Assigned(FCache.TableMeta.PField) and (FCache.TableMeta.PField.ID = Field.ID) then
                          Field.Stage := fsBase
                        else if Assigned(FCache.TableMeta.DField) and (FCache.TableMeta.DField.ID = Field.ID) then
                          Field.Stage := fsBase
//                      else if Assigned(FCache.TableMeta.IconField) and (FCache.TableMeta.IconField.ID = Field.ID) then
//                        Field.Stage := fsBase
                        else if Assigned(FCache.TableMeta.ColorField) and (FCache.TableMeta.ColorField.ID = Field.ID) then
                          Field.Stage := fsBase
                        else if (Field.OriginalVisibleLevel = fvLevel3) and not Field.IsLookup then
                          Field.Stage := fsBase
                        else if Field.DataType in BinaryTypes then
                          Field.Stage := fsBlob
                        else if Field.DataType in [ftMemo, ftWideMemo] then
                          Field.Stage := fsBlob
//                      else if Assigned(FCache.SortList) and (FCache.SortList.IndexByID(Field.ID) <> -1) then
//                        Field.Stage := fsBase
                        else if Assigned(FViewList) and (FViewList.IndexByID(Field.ID) <> -1) then
                          Field.Stage := fsBase
                        else
                          Field.Stage := fsFull;
                    end;
                  // Дополнительная проверка связей для калькулируемых полей ...
                  for I := 0 to Pred(FCache.Fields.Count) do
                    begin
                      Field := FCache.Fields[I];
                      CheckDependens;
                    end;
                end;
            end;

          Params := TStringList.Create;
          Params.Delimiter:= ';';
          Params.QuoteChar:= '"';
          Params.DelimitedText:= FTableMeta.GroupViewParams;
          for i:= 0 to Pred(Params.Count) do
            if SameText(Params.Names[i], 'SortBy') then
              begin
                FCache.SortList.AddFields(FTableMeta.OwnerTable.Fields, ClearName(Params.ValueFromIndex[i]));
                Break;
              end;
          Params.Free;

          if FCache.SortList.Count=0 then
            if not (FTableMeta.NField.DataType in NotSortTypes) then
              FCache.SortList.Add(FTableMeta.NField.ID);

          { расчет фильтров }
          if Assigned(FFilterList) then
            for I := 0 to (FFilterList^).Count-1 do
              begin
                FilterItem := TFilterItem.Create;
                FilterItem.Assign(FFilterList^[I]);
                FCache.Filters.AddFilter(FilterItem);
              end;
          for I := 0 to FTableMeta.Filters.Count-1 do
            begin
              FilterItem := TFilterItem.Create;
              FilterItem.Assign(FTableMeta.Filters[I]);
              FCache.Filters.AddFilter(FilterItem);
            end;

          if Length(FFilterStr) <> 0 then
            begin
              Parser := TDeParser.Create;
              try
                Parser.Table := FCache.TableMeta;
                FilterItem := TFilterItem.Create;
                try
                  Parser.Parse(FFilterStr, FilterItem);
                except
                  FilterItem.Free;
                  raise;
                end;
                FCache.Filters.AddFilter(FilterItem);
              finally
                Parser.Free;
              end;
            end;
          {$IFDEF DEBUG}
          if Assigned(FCache.TableMeta) then
            FCache.Filters.DebugFilterLog(Format('Preparing TDeDSComboBox for %s [%d] ...', [FCache.TableMeta.Table, VarToInt(FCache.TableMeta.ID)]))
          else
            FCache.Filters.DebugFilterLog('Preparing TDeDSComboBox ...');
          {$ENDIF}
          FCache.PrepareData;
          {$IFDEF DEBUG}
          if Assigned(FCache.TableMeta) then
            DebugLog(Format('Prepared TDeDSComboBox %d(%d) rows for %s [%d] ...', [FCache.Count, Items.Count, FCache.TableMeta.Table, VarToInt(FCache.TableMeta.ID)]))
          else
            DebugLog(Format('Prepared TDeDSComboBox %d(%d) rows ...', [FCache.Count, Items.Count]));
          {$ENDIF}

          FPrepared := True;
          Items.BeginUpdate;
          Items.Clear;
          if varIsArray(FValue) then Items.Text:= StringOfChar(Chr(10), FCache.Count + FAddNull + 1)
                                else Items.Text:= StringOfChar(Chr(10), FCache.Count + FAddNull);
          InnerSetValue(FValue);
          Items.EndUpdate;
        end;
    end
  else
    begin
      if not HandleAllocated then
        begin
          FPrepared := False;
          Items.Text:=Chr(10);
        end else
      if (FPrepared) or (ItemCount<>1) then
        begin
          if assigned(FCache) then FCache.CacheIndexes.Clear;
          FPrepared := False;
          Items.Text:=Chr(10);
        end
    end;
end;

function TDeDSComboBox.GetValue : Variant;
begin
///  if VarIsNull(FValue) then Change; { AK ???? }
  result := FValue;
end;

function TDeDSComboBox.GetItemID(I: Integer) : Variant;
begin
  if 0=FCache.Count then Result:= null
                    else Result:=FCache.Items[I].ID;
end;

procedure TDeDSComboBox.InnerSetValue(const aValue : Variant);
var cItem: TCacheItem;
    BackupCursor: TCursor;
    N:  Integer;
begin
   if FPrepared then
     begin
       if VarIsEmpty(aValue) or VarIsNull(aValue) then
         begin
           ItemIndex := FAddNull - 1;
           Hint:= EmptyStr;
         end
       else if VarIsArray(aValue) then
         begin
           ItemIndex := FAddNull;
           FValue := aValue;
           Hint:= EmptyStr;
           Repaint;
         end
       else
         begin
           if DeVarSameValue(FValue, aValue) and (ItemIndex>-1) then  Exit;

           BackupCursor  := Screen.Cursor;
           Screen.Cursor := crHourGlass;

           cItem:= FCache.FindById(aValue);

           Screen.Cursor := BackupCursor;

           if (cItem=nil) or (FCache.Fields.Count<=0) then
             begin
               ItemIndex:= FAddNull-1;
               Hint     := EmptyStr;
             end
           else
             begin
               N:=FCache.IndexByItem(cItem, False);
               ItemIndex:= N+FAddNull;
               Hint     := FCache.Hints[N]+'|';
             end;
         end;
     end

   else // not Prepared
     begin
       if VarIsEmpty(aValue) or VarIsNull(aValue) then
         begin
           if Assigned(FCache) then
             begin
               FCache.CloseData;
               FreeAndNil(FCache);
             end;

           ItemIndex:=FAddNull - 1;
           FValue := aValue;
           Hint:= EmptyStr;
           Repaint;
         end
       else if VarIsArray(aValue) then
         begin
           if Assigned(FCache) then
             begin
               FCache.CloseData;
               FreeAndNil(FCache);
             end;

           ItemIndex := FAddNull;
           FValue := aValue;
           Hint := EmptyStr;
           Repaint;
         end
       else
         begin
           if (VarType(FValue) = VarType(aValue)) and DeVarSameValue(FValue, aValue) and (ItemIndex >- 1) then Exit;

           if Assigned(FCache) then
             FCache.CloseData
           else
             FCache := TDataCache.Create(FTableMeta);

           FCache.Filters.Clear;
           FCache.Filters.NewFilter(FTableMeta.KField[0], opEQ, aValue);
           FCache.PrepareData;

           if FCache.Count=1 then Hint := FCache.Hints[0]+'|' else
           if FCache.Count=0 then Hint := EmptyStr
                             else Hint := '['+IntToStr(FCache.Count)+']: '+FCache.Hints[0]+'|';
           ItemIndex:= 0;
           Repaint;
         end;
     end;

  FValue := aValue;
end;

procedure TDeDSComboBox.SetValue(const aValue : Variant);
begin
  FUndoValue := aValue;
  InnerSetValue( aValue );
  Change;
end;

procedure TDeDSComboBox.Change;
var FAddArray: Integer;
begin
  if FPrepared then
    begin
      if VarIsArray(FValue) then FAddArray:= 1
                            else FAddArray:= 0;

      if (ItemIndex<FAddNull) then
        begin
          FValue:= null;
          Hint:= EmptyStr;
        end
      else
        if (FAddArray <= ItemIndex) then
          begin
            FValue:= FCache.Items[ItemIndex - FAddNull - FAddArray].ID;
            Hint:= FCache.Hints[ItemIndex - FAddNull - FAddArray]+'|';
          end;

      if FAddArray > Integer(VarIsArray(FValue)) then Items.Delete(0);
    end
  else
    begin
      if Assigned(FCache) and (FCache.Count = 1) then
        begin
          FValue:=FCache.Items[0].ID;
          Hint  :=FCache.Hints[0]+'|';
        end
      else
        begin
          FValue:= null;
          Hint:= EmptyStr;
        end;
    end;
  inherited Change;
end;

procedure TDeDSComboBox.SetReadOnly(aReadOnly : boolean);
begin
  FReadOnly := aReadOnly;
  Enabled := not FReadOnly;
end;

procedure TDeDSComboBox.SetEnabled(Value : boolean);
begin
  inherited SetEnabled(Value and (not ReadOnly));
end;

procedure TDeDSComboBox.SetFilterStr(const Value: string);
begin
  if not SameText(FFilterStr, Value) then
    begin
      FFilterStr := Value;
      if Prepared then Prepared := False;
    end;
end;

procedure TDeDSComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  ItemHeight:= AHeight-(Height - ItemHeight);
  inherited;
end;

procedure TDeDSComboBox.SetTextPress(const aValue: string);
var i, N: Integer;
  FI: TFilterItem;
  FM: TFieldMeta;
begin
  if aValue = FTextPress then Exit;

  if Prepared and assigned(FCache) then
    begin
      Items.BeginUpdate;
      FCache.CacheIndexes.Clear;

      if (0 < Length(aValue)) and (0 < FViewList.Count) then
        begin
          FI:= TFilterItem.Create;

          for i:= 0 to Pred(FViewList.Count)  do
            begin
              FM:= FViewList[i];
              if assigned(FM.LookupPair) and not (FM.IsLookup) then FM:= FM.LookupPair;

              FI.AddIdent(FM.Original, FM.DataType, FM.CodePage);
              if not (FM.DataType in StringTypes) then
                FI.AddFunc('String',1);

              FI.AddConst('%'+StringReplace(aValue,' ','%',[rfReplaceAll])+'%', ftWideString);
              FI.AddOperation(opLike);

              if 0 < i then
                FI.AddOperation(opOr);
            end;

          FCache.CacheIndexes.ItemIndex:= FCache.CacheIndexes.Add(FI);
        end;

      Items.Clear;
      if varIsArray(FValue) then Items.Text:= StringOfChar(Chr(10), FCache.Count + FAddNull + 1)
                            else Items.Text:= StringOfChar(Chr(10), FCache.Count + FAddNull);

      N:= FCache.IndexByID(Value);
      if N=-1 then
        begin
          if 0<FCache.Count then InnerSetValue(FCache.Items[0].ID)
                            else InnerSetValue(unassigned);
        end
      else
        begin
          InnerSetValue(FCache.Items[N].ID);
        end;

      Items.EndUpdate;
    end;

  FTextPress := aValue;
end;

procedure TDeDSComboBox.WMKeyDown(var Msg: TMessage);
begin
  // ловим на уровне сообщений от windows чтобы отличить кнопки вверх/вниз от колеса мыши см. Msg.LParamLo
  if (Msg.WParamLo in [VK_UP, VK_DOWN]) and not DroppedDown and not (Msg.LParamLo = 0) then
    begin
      DroppedDown:= True;
    end else

  if (Msg.WParamLo in [VK_ESCAPE]) then
    begin
      InnerSetValue(FUndoValue);
      if DroppedDown then DroppedDown:= False;
    end else

  if (Msg.WParamLo in [VK_DELETE]) and (FAddNull=1) then
    begin
      if DroppedDown then DroppedDown:= False;
      InnerSetValue(unassigned);
    end else

  if (Msg.WParamLo in [VK_RETURN]) and DroppedDown then
    begin
      if DroppedDown then DroppedDown:= False;
    end else

  if (Msg.WParamLo in [VK_TAB]) and DroppedDown then
    begin
      if DroppedDown then DroppedDown:= False;
    end else

    inherited;
end;

procedure TDeDSComboBox.KeyPress(var Key: Char);
begin
  if (Ord(Key) = VK_BACK) and (0 < Length(TextPress)) then
    begin
      TextPress:= Copy(TextPress,1,Length(TextPress)-1);
    end else

  if (Ord(' ') <= Ord(Key)) then
    begin
      Prepared:= True;

      TextPress:= TrimLeft(TextPress + Key);
      DroppedDown:= True;
      repaint;
      inherited KeyPress(Key);
    end;
  Key:= #0;
end;

function TDeDSComboBox.FocusedItem: TCacheItem;
begin
  if VarIsArray(FValue) then Result := nil else
  if FPrepared then
    if (ItemIndex=0) and (FAddNull=1) then Result:= nil
                                      else Result:= FCache.Items[ItemIndex - FAddNull]
  else
    if Assigned(FCache) and (FCache.Count=1) then Result:= FCache.Items[0]
                                             else Result:= nil;
end;

procedure TDeDSComboBox.WMPaint(var Message: TWmPaint);
var  FRectInfo: TRect;
     DC: HDC;
     brush: HBrush;
     H: Integer;
     FIcon: TIcon;
begin
  if DroppedDown and (0 < Length(FTextPress)) then
    begin
      if (FLastPress<>FTextPress) or (Message.Msg <> WM_PAINT) then
        begin
          FLastPress:= TextPress;
          FRectInfo:= ClientRect;
          H:= (FRectInfo.Bottom - Canvas.TextHeight('Yу')) div 2;

          FRectInfo.Left:= FRectInfo.Left + 2;
          FRectInfo.Top:= FRectInfo.Top + 2;
          FRectInfo.Right:= FRectInfo.Right - 1 - GetSystemMetrics(SM_CXVSCROLL);
          FRectInfo.Bottom:= FRectInfo.Bottom - 2;
          DC := GetWindowDC(Handle);

          Brush := CreateSolidBrush(RGB(224,224,224));
          FillRect(DC, FRectInfo, Brush);
          Selectobject(DC, Font.Handle);
          SetBkColor(DC, RGB(224,224,224));

          TextOut(DC, FRectInfo.Left + H + 24, H, PChar(TextPress), Length(TextPress));
          DeleteObject(Brush);

          FIcon:= TIcon.Create;
          DM.ilIcon16.GetIcon(DM.MapIconIndex(104), FIcon);
          FIcon.Assign(FIcon);
          Canvas.Draw(H, 2+ (FRectInfo.Bottom - 16) div 2 , FIcon);
          FIcon.Free;

          ReleaseDC(Handle, DC);
        end;
    end
  else
    inherited;
end;

procedure TDeDSComboBox.ComboBoxDrawItem(Control: TWinControl; Index: Integer;
  aRect: TRect; State: TOwnerDrawState);
var i,x,xL,xR,wL,wR,n : Integer;
    aCacheItem: TCacheItem;
    tmpS  : string;
    Rect2 : TRect;
    zColor, bColor : TColor;
begin
  // определяем CacheItem
  {if VarIsArray(FValue) then
    aCacheItem := nil
  else }
  if FPrepared then
    if (Index=0) and (FAddNull=1) then aCacheItem:= nil
                                  else if VarIsArray(FValue) then
                                         if Index = 0 then aCacheItem := nil
                                                      else aCacheItem:= FCache.Items[Pred(Index - FAddNull)]
                                       else
                                         if FCache.Count=0 then aCacheItem := nil
                                                           else aCacheItem:= FCache.Items[Index - FAddNull]
  else
    if Assigned(FCache) and (FCache.Count=1) then if VarIsArray(FValue) and (Index = 0) then aCacheItem := nil
                                                                                        else aCacheItem:= FCache.Items[0]
                                             else aCacheItem:= nil;

  // теперь рисуем
  if odComboBoxEdit in State then x:=aRect.Left+2 else x:=aRect.Left+1;

  if not Assigned(aCacheItem) then
    begin
      if odSelected in State then Canvas.Brush.Color:=clMenuHighlight
                             else Canvas.Brush.Color:=clSilver;
      Canvas.Font.Style := Canvas.Font.Style + [fsBold];
      if VarIsArray(FValue) then
        begin
          tmpS := Format(FArrayText, [Succ(VarArrayHighBound(FValue, 1) - VarArrayLowBound(FValue, 1))]);
          Canvas.TextRect(aRect, x, aRect.Top, tmpS);
        end
      else
        Canvas.TextRect(aRect, x, aRect.Top, FNullText);
      Exit;
    end;

  if odSelected in State then bColor:=clMenuHighlight
                         else bColor:=clWindow;

  zColor:=aCacheItem.FontColor;
  if zColor>0 then Canvas.Font.Color:= GetContrastColor(zColor, bColor);

  Canvas.Brush.Color:=bColor;

  if FViewList.Count < 2 then
    begin
      tmpS:=GetTitle(aCacheItem.Caption);
      Canvas.TextRect(aRect, x, aRect.Top, tmpS);
    end
  else
    begin
      wR:=0;
      wL:=0;
      for i:=0 to FViewList.Count-1 do
        begin
          if assigned(FViewList[i].LookupPair) then
            n:=FCache.Fields.IndexByID(FViewList[i].LookupPair.ID)
          else
            n:=FCache.Fields.IndexByID(FViewList[i].ID);
          tmpS:=GetTitle(aCacheItem.FieldText(n));

          Inc(wR,FViewList[i].Width);
          xL:=Round(wL*(2+aRect.Right-aRect.Left)/FViewWidth);
          xR:=Round(wR*(2+aRect.Right-aRect.Left)/FViewWidth);
          Rect2:=Rect(xL,aRect.Top,xR,aRect.Bottom);
          Canvas.TextRect( Rect2, xL+x, Rect2.Top, tmpS);

          if i>0 then
            begin
              Canvas.Pen.Color:=clSilver;
              Canvas.MoveTo(xL,Rect2.Top);
              Canvas.LineTo(xL,Rect2.Bottom);
            end;
          Inc(wL,FViewList[i].Width);
        end;
    end;
end;

Function TDeDSComboBox.GetAllowNull: Boolean;
begin
  Result := FAddNull=1;
end;

procedure TDeDSComboBox.SetAllowNull(const aAllowNull: Boolean);
begin
  if aAllowNull then FAddNull:=1
                else FAddNull:=0;
end;

procedure TDeDSComboBox.SetNullText(const Value: string);
begin
  if {Ansi}CompareStr(FNullText, Value) <> 0 then
    begin
      FNullText := Value;
      Invalidate;
    end;
end;

procedure TDeDSComboBox.SetArrayText(const Value: string);
begin
  if CompareStr(FArrayText, Value) <> 0 then
    begin
      FArrayText := Value;
      Invalidate;
    end;
end;

{ TDeFieldsComboBox }

constructor TDeFieldsComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSize := False; // Иначе контрол подгоняется под текущий шрифт!!!
  FTableMeta:= nil;
end;

procedure TDeFieldsComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  ItemHeight:= AHeight-(Height - ItemHeight);
  inherited;
end;

procedure TDeFieldsComboBox.SetTableMeta(const aTable : TTableMeta);
var i : integer;
    FTempTableMeta : TTableMeta;
begin
  if not assigned(aTable) then
      begin
        Items.Clear;
        FTableMeta:= nil;
      end else

  if not assigned(FTableMeta) or (aTable.Table <> FTableMeta.Table) then
    begin
      Items.Clear;
      FTableMeta:= aTable;

      FTempTableMeta := TTableMeta.Create;
      FTempTableMeta.SetTable:= aTable.Table;
      FTempTableMeta.ID:= aTable.ID;
      aTable.Database.GetMetaTableInfo(FTempTableMeta);

      for i := 0 to FTempTableMeta.Fields.Count-1 do
        Items.Add(FTempTableMeta.Fields[I].Original);

      FTempTableMeta.Free;
    end;
end;

{ TDeTablesComboBox }

procedure TDeTablesComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  ItemHeight:= AHeight-(Height - ItemHeight);
  inherited;
end;

procedure TDeTablesComboBox.SetDatabase(const aDatabase : TDeCustomDatabase);
var ATableList: TStringList;
begin
  if aDatabase <> FDatabase then
    try
      FDatabase := aDatabase;
      ATableList:= TStringList.Create;

      Items.Clear;
      if Assigned(FDatabase) then
        begin
          FDatabase.RetrieveTableNames(ATableList);
          self.Items.AddStrings(ATableList);
        end;
      ATableList.Free;
    except
    end;
end;

constructor TDeTablesComboBox.Create(AOwner: TComponent);
begin
  inherited;
  AutoSize := False; // Иначе контрол подгоняется под текущий шрифт!!!
end;

function TDeTablesComboBox.GetDatabaseID : integer;
begin
  if Assigned(Database) then
    result := Database.ID
  else
    result := 0;
end;

procedure TDeTablesComboBox.SetDatabaseID(const Value : integer);
begin
  Database := MetaData.DatabaseByID(Value);
end;

{ TDePasswordEdit }

constructor TDePasswordEdit.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  PasswordChar := '*';
  Encrypted := true;
end;

function TDePasswordEdit.GetText : TCaption;
begin
  result := inherited Text;
  if FChanged and Encrypted then
    result := MD5password(WideStringToUnicode(result));
end;

procedure TDePasswordEdit.SetText(const aText : TCaption);
begin
  FState := FState + [dsTextAssigning];
  inherited Text := aText;
  FChanged := false;
  FState := FState - [dsTextAssigning];
end;

procedure TDePasswordEdit.ClearText;
begin
  Clear;
  FChanged := true;
end;

procedure TDePasswordEdit.KeyPress(var Key : Char);
begin
  if not FChanged then
    ClearText;
  inherited;
end;

procedure TDePasswordEdit.Change;
begin
  if not (dsChanging in FState) then
    begin
      FState := FState + [dsChanging];
      if (not (dsTextAssigning in FState)) and (not FChanged) then
        ClearText;
      FState := FState - [dsChanging];
      inherited;
    end;
end;

{ TDeCheckListBox }

procedure TDeCheckListBox.SetDataID(const aDataID : integer);
var I     : integer;
    Cache : TDataCache;
begin
  if aDataID <> FDataID then
    begin
      FDataID := aDataID;
      Items.Clear;
      Cache := MetaData.GetLibrary(FDataID);
      for I := 0 to Cache.Count-1 do
        Items.AddObject( GetTitle(Cache.Items[I].Caption), pointer(VarToInt(Cache.Items[I].ID)) );
    end;
end;

{ TDeListTabSheet }

constructor TDeListTabSheet.Create(AOwner: TComponent);
begin
  FDataID     := 0;
  FLinkField  := nil;
  FGridForm   := nil;
  FParentDSM  := nil;

  FDeEnabled  := True;
  FShowNow    := False;
  FWaiting    := False;
  FViewType   := vtNone;
  inherited;
  BorderWidth := MarginGrid;
end;

destructor TDeListTabSheet.Destroy;
begin
  DestroyData;
  inherited;
end;

procedure TDeListTabSheet.PrepareToDestroy;
begin
  FWaiting    := False;
end;

function TDeListTabSheet.GetCaption : string;
begin
  result := inherited Caption;
end;

procedure TDeListTabSheet.SetCaption(const aCaption : string);
begin
  inherited Caption := aCaption;
  if Assigned(FGridForm) then
    FGridForm.FormCaption := Caption;
end;

procedure TDeListTabSheet.SetDeEnabled(aValue : Boolean);
begin
  FDeEnabled:=aValue;

  if not FDeEnabled then
      Cursor:= crNo;

  if Assigned(FGridForm) then
    begin
      FGridForm.Enabled:= FDeEnabled;
      FGridForm.pnlGrid.Color := DisableColor(FDeEnabled);
    end;
end;

procedure TDeListTabSheet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (aComponent = FGridForm) then
    FGridForm := nil;
  inherited Notification(aComponent, Operation);
end;

procedure TDeListTabSheet.CreateData;
var DSM  : TDataSetMeta;
begin
  if (FDataID>0)and(Assigned(FParentDSM)) then
  begin
    if (FViewType<>vtNone) then
      FGridForm:= TBaseGridForm(ViewTypeToClass(FViewType).Create(Application)) else
    if assigned(FLinkField) then
      FGridForm:= TBaseGridForm(ViewTypeToClass(FLinkField.Owner.GroupViewType).Create(Application)) else
      FGridForm:= TBaseGridForm(ViewTypeToClass(vtList).Create(Application));
    FGridForm.FreeNotification(Self);

    FGridForm.Enabled:= FDeEnabled;
    FGridForm.pnlGrid.Color := DisableColor(FDeEnabled);
    FGridForm.FormCaption := Caption;

    DSM := FParentDSM.CreateChild(FDataID, FLinkField);
    DSM.Cache.OpenData;

    if FDataID = MetaData.MetaTables[idxInterrelations].ID then
    begin
      DSM.Context.ViewType := vtList;
      DSM.Context.ViewParams := cListStyleParam + '=' +  ListViewStyleNames[lvsIcon];
    end;

    self.DoubleBuffered:=True;
    FGridForm.Visible := False;

    FGridForm.ManualDock(Self);
    FGridForm.Align   := alClient;
    FGridForm.ReinitForm(DSM);

    FGridForm.Visible := True;
    self.DoubleBuffered:=False;
  end;

  FWaiting:=False;
end;

procedure TDeListTabSheet.DestroyData;
var DSM : TDataSetMeta;
begin
  if Assigned(FGridForm) then
  begin
    DSM := FGridForm.DataSetMeta;
    FGridForm.DeInitForm;
    FGridForm.Free;
    DSM.Free;
  end;
  FDataID := 0;
  FLinkField := nil;
  FParentDSM := nil;
end;

procedure TDeListTabSheet.CMVisibleChanged(var Message: TMessage);
begin
  FShowNow := (Message.WParam<>0);

  if FShowNow then
    if FWaiting then
      CreateData
    else
      begin
        if Assigned(GridControl) and Assigned(GridControl.DataSetMeta) and Assigned(GridControl.DataSetMeta.Cache) then
          GridControl.DataSetMeta.Cache.EndUpdate;
      end
  else
    begin
      if Assigned(GridControl) and Assigned(GridControl.DataSetMeta) and Assigned(GridControl.DataSetMeta.Cache) then
        GridControl.DataSetMeta.Cache.BeginUpdate;
    end;
  inherited;
end;

procedure TDeListTabSheet.SetDataID(aDataID : integer;  aLinkField : TFieldMeta;
  aParentDSM : TDataSetMeta);
begin
  if FDataID <> aDataID then
  begin
    DestroyData;
    FDataID    := aDataID;
    FLinkField := aLinkField;
    FParentDSM := aParentDSM;

    if FShowNow or Showing then
      CreateData
    else
      FWaiting:=True;
  end;
end;


procedure TDeImage.onPaste(Sender: TObject);
var
//  f: THandle;
//  buffer: array [0..MAX_PATH] of Char;
//  i, numFiles: Integer;
  JPEGImage: TJPEGImage;
  Stream: TStream;
  FileName: string;
begin
  if DeClipboard.HasFormat(CF_HDROP) then
    begin
      DeClipboard.Open;
      try
        FileName := DeClipboard.ReadFileName;
        if Length(FileName) <> 0 then
          LoadFromFile(FileName);
        {
        f := DeClipboard.GetAsHandle(CF_HDROP);
        if f <> 0 then
          begin
//          numFiles := DragQueryFile(f, $FFFFFFFF, nil, 0);
//          for i := 0 to numfiles - 1 do
//          begin
            ZeroMemory(@Buffer, SizeOf(Buffer));
            //buffer[0] := #0;
            DragQueryFile(f, 0, buffer, SizeOf(buffer));
            LoadFromFile(buffer);
            DeClipboard.Clear;
//          end;
          end;
        }
      finally
        DeClipboard.Close;
      end;
    end
  else if DeClipboard.HasFormat(CF_BITMAP) then
    begin
      Stream := TMemoryStream.Create;
      try
        JPEGImage := TJPEGImage.Create;
        try
          (*
          Bitmap := Graphics.TBitmap.Create;
          try
            DeClipboard.ReadBitmap(Bitmap);
            {
            DeClipboard.Open;
            try
              Bitmap.LoadFromClipboardFormat(CF_BITMAP, DeClipboard.GetAsHandle(CF_BITMAP), 0);
              DeClipboard.Clear;
            finally
              DeClipboard.Close;
            end;
            }
            JPEGImage.Assign(Bitmap);
          finally
            Bitmap.Free;
          end;
          *)
          if DeClipboard.ReadJPEG(JPEGImage) then
            JPEGImage.SaveToStream(Stream);
        finally
          JPEGImage.Free;
        end;
        SetLength(FFileData, Stream.Size);
        if Stream.Size <> 0 then
          begin
            Stream.Seek(0, soFromBeginning);
            Stream.ReadBuffer(FFileData[0], Stream.Size);
            FFileName := FormatDateTime('"Clipboard"DDMMYYYYHHNNSS', Now) + sExtensionJPG;
          end
        else
          FFileName := EmptyStr;
        //Hint := FFileName;
        HintUpdate;
        UpdateImage(True);
      finally
        Stream.Free;
      end;
    end;
end;

procedure TDeImage.onCopy(Sender: TObject);
var
  aFile: string;
//  DropFiles: PDropFiles;
//  hGlobal: THandle;
//  iLen: Integer;
//  Format: Word;
//  Pallete: HPALETTE;
//  Bitmap: Graphics.TBitmap;
begin
  //aFile := TPath.GetTempPath + FFileName; // XE7
  aFile := GetTempFile(ChangeFileExt(FFileName, EmptyStr), ExtractFileExt(FFileName));
  SaveToFile(aFile);
//  iLen := (Length(aFile) + 2) * SizeOf(Char);
//  hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT,
//    SizeOf(TDropFiles) + iLen);
//  if (hGlobal = 0) then raise Exception.Create('Could not allocate memory.');
  DeClipboard.Open;
  try
    DeClipboard.WriteFileName(aFile);
    {
    DropFiles := GlobalLock(hGlobal);
    try
      DropFiles^.pFiles := SizeOf(TDropFiles);
      DropFiles^.fWide := True; // XE7
      Move(aFile[1], (PAnsiChar(DropFiles) + SizeOf(TDropFiles))^, iLen);
    finally
      GlobalUnlock(hGlobal);
    end;
    DeClipboard.SetAsHandle(CF_HDROP, hGlobal);
    }
    if (ImageType in [itBMP, itJPEG]) and Assigned(Graphic) then
      begin
        DeClipboard.WriteGraphic(Graphic);
        {
        //Format := CF_BITMAP;
        Bitmap := Graphics.TBitmap.Create;
        try
          Bitmap.Assign(Graphic);
          DeClipboard.WriteBitmap(Bitmap);
          //Bitmap.SaveToClipboardFormat(Format, hGlobal, Pallete);
        finally
          Bitmap.Free;
        end;
        //DeClipboard.SetAsHandle(Format, hGlobal);
        }
      end;
  finally
    DeClipboard.Close;
  end;
end;

procedure TDeImage.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  {сделаем окно способным принимать файлы}
  Params.ExStyle := Params.ExStyle or WS_EX_ACCEPTFILES;
end;

procedure TDeImage.WMDropFiles(var Message: TWMDropFiles);
var
  aFile, aTempFile: string;
  FileCount, FileIndex: Integer;
  Buffer: array[0..1023] of Char;
  //KeyState: byte;
  //Buffer: array[0..MAX_PATH] of Char;
  ZipFile: TZipFile;
  Gauge: TGauge;
begin
  inherited;
  if FMultipleFiles then
    begin
      {так можно узнать сколько файлов перетягивается}
      FileCount := DragQueryFile(Message.drop, $FFFFFFFF, nil, 0);
      if FileCount = 1 then
        begin
          if DragQueryFile(Message.Drop, 0, Buffer, SizeOf(Buffer)) <> 0 then
            begin
              aFile := StrPas(Buffer);
              if ByteBool((GetKeyState(VK_CONTROL) and 128)) and not SameText(ExtractFileExt(aFile), sExtensionLNK) then
                begin
                  //aTempFile := TPath.GetTempPath + ExtractFileName(aFile) + sExtensionZIP;
                  aTempFile := GetTempFile(ExtractFileName(aFile), sExtensionZIP);
                  ZipFile := TZipFile.Create;
                  try
                    ZipFile.Open(aTempFile, zmWrite);
                    try
                      ZipFile.Add(aFile);
                    finally
                      ZipFile.Close;
                    end;
                  finally
                    ZipFile.Free;
                  end;
                  aFile := aTempFile;
                end;
              LoadFromFile(aFile);
            end
        end
      else
        begin
          //aFile := TPath.GetTempPath + FormatDateTime('"Compress"DDMMYYYYHHNNSS', Now) + sExtensionZIP;
          aFile := GetTempFile(FormatDateTime('"Compress"DDMMYYYYHHNNSS', Now), sExtensionZIP);
          try
            ZipFile := TZipFile.Create;
            try
              ZipFile.Open(aFile, zmWrite);
              try
                Gauge := TGauge.Create(Self);
                try
                  Gauge.Parent := Self;
                  Gauge.Align := alClient;
                  Gauge.Kind := gkVerticalBar;
                  Gauge.BorderStyle := bsNone;
                  Gauge.MaxValue := FileCount;
                  Gauge.BackColor := Color;
                  Gauge.ForeColor := clActiveCaption;
                  for FileIndex := 0 to Pred(FileCount) do
                    begin
                      if DragQueryFile(Message.Drop, FileIndex, Buffer, SizeOf(Buffer)) <> 0 then
                        ZipFile.Add(StrPas(Buffer));
                      Gauge.AddProgress(1);
                    end;
                finally
                  Gauge.Free;
                end;
              finally
                ZipFile.Close;
              end;
            finally
              ZipFile.Free;
            end;
            LoadFromFile(aFile);
          finally
            if FileExists(aFile) then SysUtils.DeleteFile(aFile);
          end;
        end
    end
  else
  begin
    SetLength(aFile,MAX_PATH);
    SetLength(aFile,DragQueryFile(Message.drop, 0, PChar(aFile), MAX_PATH));
    if ByteBool((GetKeyState(VK_MENU) and 128)) and not SameText(ExtractFileExt(aFile), sExtensionLNK) then
      begin
        //aTempFile := TPath.GetTempPath + ExtractFileName(aFile) + sExtensionLNK;
        aTempFile := GetTempFile(ExtractFileName(aFile), sExtensionLNK);
        CreateShortCut(aFile, aTempFile, ExtractFilePath(aFile), EmptyStr, EmptyStr);
        aFile := aTempFile;
      end;
    if ByteBool((GetKeyState(VK_CONTROL) and 128)) and not SameText(ExtractFileExt(aFile), sExtensionLNK) then
      begin
        //aTempFile := TPath.GetTempPath + ExtractFileName(aFile) + sExtensionZIP;
        aTempFile := GetTempFile(ExtractFileName(aFile), sExtensionZIP);
        ZipFile := TZipFile.Create;
        try
          ZipFile.Open(aTempFile, zmWrite);
          try
            ZipFile.Add(aFile);
          finally
            ZipFile.Close;
          end;
        finally
          ZipFile.Free;
        end;
        aFile := aTempFile;
      end;
    LoadFromFile(aFile);
  end;
  DragFinish(Message.Drop);
end;

procedure TDeImage.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited;
  //if (State=dsDragEnter) then // Из-за этого контрол сам в себя мог кидаться и вываливалась ошибка kernel!
  if (State=dsDragEnter) and (Source <> Self) then
    begin
      EndDrag(False);
      FDropper.Enabled:=true;
      FDropper.StartDrag;
    end;
end;

procedure TDeImage.OnDropUp(var Files: TStringList);
var
  aFile: string;
begin
  //aFile := TPath.GetTempPath + FFileName;
  aFile := GetTempFile(ChangeFileExt(FFileName, EmptyStr), ExtractFileExt(FFileName));
  SaveToFile(aFile);
  Files.Text:=aFile;
end;

function TDeImage.IsLoadedImage(const AFileData: TByteDynArray): boolean;
//var
//  Stream: TMemoryStream;
//  TIFFBmp: TTIFFBitMap;
  function CheckSignature(const Signature: TByteDynArray): Boolean;
  begin
    Result := Length(AFileData) > Length(Signature);
    if Result then
      Result := CompareMem(@AFileData[0], @Signature[0], Length(Signature));
  end;
  function LoadGraphic(const ImageType: TImageType; GraphicClass: TGraphicClass): Boolean;
  var
    Stream: TStream;
  begin
    Result := Assigned(GraphicClass);
    if Result then
      begin
        FGraphic := GraphicClass.Create;
        try
          Stream := TMemoryStream.Create;
          try
            Stream.Write(AFileData[0], Length(AFileData));
            Stream.Position := 0;
            FGraphic.LoadFromStream(Stream);
          finally
            Stream.Free;
          end;
          FImageType := ImageType;
        except
          FreeAndNil(FGraphic);
          raise;
        end;
      end;
  end;
begin
  //Result := False;
  if CheckSignature([$47, $49, $46]) then // GIF
    Result := LoadGraphic(itGIF, TGIFImage)
  {
  else if string(Copy(AFileData, 0, 3)) = 'II*' then
  begin
    FBitmap := Graphics.TBitmap.Create;
    TIFFBmp := TTIFFBitMap.Create;
    Stream:= TMemoryStream.Create;
    Stream.Write(AFileData[0], Length(AFileData));
    Stream.Position := 0;
    TIFFBmp.LoadTiffFromStream(Stream);
    Stream.Free;
    FBitmap.Assign(TIFFBmp);
    FreeAndNil(TIFFBmp);
    FImageType := itTIF;
    Result:=true;
  end {}
  else if CheckSignature([$FF, $D8, $FF]) then // JPEG
    Result := LoadGraphic(itJPEG, TJPEGImage)
  else if CheckSignature([$89, $50, $4E, $47]) then // PNG
    Result := LoadGraphic(itPNG, TPngImage)
  else if CheckSignature([$42, $4D]) then // BMP
    Result := LoadGraphic(itBMP, Graphics.TBitmap)
  else if CheckSignature([$49, $49, $2A]) then // TIF
    Result := LoadGraphic(itTIF, TWICImage)
  else if SameText(ExtractFileExt(FFileName), sExtensionEMF) or SameText(ExtractFileExt(FFileName), sExtensionWMF) then
    Result := LoadGraphic(itWMF, TMetafile)
  else
    Result := False;
end;

procedure TDeImage.onLinkTo(Sender: TObject);
var
  ODlg: TOpenDialog;
  aFile: string;
begin
  ODlg := TOpenDialog.Create(self);
  try
    ODlg.Title := TMenuItem(Sender).Caption;
    if IntDeImageLastDir = EmptyStr then
      ODlg.InitialDir := InitialDir
    else
      ODlg.InitialDir := IntDeImageLastDir;
    ODlg.Filter := GetTitle(dlgFilters);
    ODlg.FilterIndex := dlgFilterAll;

    if ODlg.Execute then begin
      if not SameText(ExtractFileExt(ODlg.FileName), sExtensionLNK) then
        begin
          //aFile := TPath.GetTempPath+ExtractFileName(ODlg.FileName) + sExtensionLNK;
          aFile := GetTempFile(ExtractFileName(ODlg.FileName), sExtensionLNK);
          try
            CreateShortCut(ODlg.FileName, aFile, ExtractFilePath(ODlg.FileName),EmptyStr,EmptyStr);
            LoadFromFile(aFile);
          finally
            if FileExists(aFile) then SysUtils.DeleteFile(aFile);
          end;
        end
      else
        LoadFromFile(ODlg.FileName);
      IntDeImageLastDir := ExtractFilePath(ODlg.FileName);
    end;

  finally
    ODlg.Free;
  end;
end;

procedure TDeImage.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if (Button=mbLeft) and not (ssDouble in Shift)
    then BeginDrag(False,4);
end;

procedure TDeImage.onCut(Sender: TObject);
begin
  onCopy(Sender);
  onEmpty(Sender);
end;

procedure TDeImage.TwainAcquire(Sender: TObject; const Index: Integer; Bitmap: Graphics.TBitmap; var Cancel: Boolean);
var
  JPEGImage: TJPEGImage;
  Stream: TStream;
begin
  JPEGImage := TJPEGImage.Create;
  try
    JPEGImage.Assign(Bitmap);
    Stream := TMemoryStream.Create;
    try
      JPEGImage.SaveToStream(Stream);
      SetLength(FFileData, Stream.Size);
      Stream.Seek(0, soFromBeginning);
      Stream.ReadBuffer(FFileData[0], Stream.Size);
      FFileName := FormatDateTime('"Scan"DDMMYYYYHHNNSS', Now) + sExtensionJPG;
      //Hint := FFileName;
      HintUpdate;
      UpdateImage(True);
    finally
      Stream.Free;
    end;
  finally
    JPEGImage.Free;
  end;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TDeImage.onScanFrom(Sender: TObject);
begin
  if Assigned(Sender) and (Sender is TMenuItem) then
    begin
      FTwain.SelectedSourceIndex := (Sender as TMenuItem).Tag;
      if Assigned(FTwain.SelectedSource) then
        begin
          //Load source, select transference method and enable (display interface)}
          FTwain.SelectedSource.Loaded := True;
          FTwain.SelectedSource.ShowUI := False;
          FTwain.SelectedSource.Enabled := True;
        end;
    end;
end;

procedure TDeImage.RebuildTwainSources;
var
  Index: Integer;
  MenuItem: TMenuItem;
begin
  if Assigned(MI_ScanFrom) then
    begin
      MI_ScanFrom.Clear;

     {$IFDEF WIN32}
      if Assigned(FTwain) and FTwain.LoadLibrary and FTwain.LoadSourceManager and (FTwain.SourceCount > 1) then
        for Index := 0 to Pred(FTwain.SourceCount) do
          begin
            MenuItem := TMenuItem.Create(MI_ScanFrom);
            try
              MenuItem.Caption := FTwain.Source[Index].ProductName;
              MenuItem.Tag := Index;
              MenuItem.OnClick := onScanFrom;
              MI_ScanFrom.Add(MenuItem);
            except
              MenuItem.Free;
              raise;
            end;
          end;
      if MI_ScanFrom.Count = 0 then
        begin
          MI_ScanFrom.Enabled := FTwain.SourceCount = 1;
          MI_ScanFrom.Caption := GetTitle('_Da.Scan', ttFirstName);
          MI_ScanFrom.Hint := GetTitle('_Da.Scan', ttSecondName);
          //MI_ScanFrom.Caption := 'Сканировать ...';
          MI_ScanFrom.OnClick := onScanFrom;
        end
      else
        begin
          MI_ScanFrom.Enabled := True;
          MI_ScanFrom.Caption := GetTitle('_Da.ScanFrom', ttFirstName);
          MI_ScanFrom.Hint := GetTitle('_Da.ScanFrom', ttSecondName);
          //MI_ScanFrom.Caption := 'Сканировать из ...';
          MI_ScanFrom.OnClick := nil;
        end;
     {$ENDIF}
    end;
end;

procedure TDeImage.TwainError(Sender: TObject; const Index: Integer; ErrorCode, Additional: Integer);
resourcestring
  sErrorFmt = 'Scan error code %.8x, additional %.8x.';
begin
  raise ETwainError.CreateResFmt(@sErrorFmt, [ErrorCode, Additional]);
end;

procedure TDeImage.OnThreadChange(Sender: TObject);
var
//  List: TList;
  Thread: TDeThread;
begin
  if Assigned(Sender) and (Sender is TDeThread) then
    begin
      Thread := Sender as TDeThread;
      {$IFDEF DEBUG}
      if Assigned(Thread) then
        DebugLog('Monitoring file CHANGE ' + Thread.FileName);
      {$ENDIF}
      if Assigned(Thread) and FileExists(Thread.FileName) then
        begin
          try
            LoadFromFile(Thread.FileName, FFileName, FVersion + 1);
          finally
            HintUpdate;
          end;
          ShowHintWindow(Self, 'Файл ' + FFileName + ' был изменен');
          {$IFDEF DEBUG}
          DebugLog('TDeImage file ' + FFileName + ' changed');
          {$ENDIF}
        end;
    end;
end;

procedure TDeImage.OnThreadDone(Sender: TObject);
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
      {$IFDEF DEBUG}
      if Assigned(Sender) and (Sender is TDeThread) then
        DebugLog('Monitoring file STOP ' + (Sender as TDeThread).FileName)
      else
        DebugLog('Monitoring file STOP');
      {$ENDIF}
    end;
end;

{ TDeSpinEdit }

function PointIntoRect( X,Y : Integer; Rect: TRect): boolean;
begin
  Result := (Rect.Left <= x ) and (x <= Rect.Right) and
            (Rect.Top  <= y ) and (y <= Rect.Bottom);
end;

procedure TDeSpinEdit.Change;
var
  NewInt: Int64;
  NewExtended: Extended;
  NewVal: Variant;
  i,E: Integer;
  s: string;
  Ok: Boolean;
  FPostfix  : TExpressionItem;
begin
  s:= Trim(Text);

  if FNumberType in IntVarTypes then
    begin
      Val(s, NewInt, E);
      if E=1 then NewVal:= Null
             else NewVal:= NewInt;
    end
  else
    begin
      if ','<> FormatSettings.DecimalSeparator then s:=StringReplace(s,',',FormatSettings.DecimalSeparator,[rfReplaceAll]);
      if '.'<> FormatSettings.DecimalSeparator then s:=StringReplace(s,'.',FormatSettings.DecimalSeparator,[rfReplaceAll]);
      if TextToFloat(PChar(S), NewExtended, fvExtended) then E:=0
                                                        else E:=1;
      if E=1 then NewVal:= Null
             else NewVal:= NewExtended;
    end;

  if (E = 0) or ( Length(s)=0 ) then
    begin // Целое число или пусто - не отображаем подсказку
      FValue:=NewVal;

      if (E<>1) and (FValue < FMinValue) then
        begin
          Font.Color:=clRed;
          Info := '<'+IntToStr(FMinValue);
        end else
      if (E<>1) and (FMaxValue < FValue) then
        begin
          Font.Color:=clRed;
          Info := '>'+IntToStr(FMaxValue);
        end else
        begin
          Font.Color:=clBlack;
          Info := EmptyStr;
        end;
    end
  else
    begin
      for i:= Length(s) downto 1 do
        if s[i]=' ' then delete(s,i,1);

      FPostfix   := TExpressionItem.Create;
      try
        FParser.Parse(StringReplace(s,',','.',[rfReplaceAll]), FPostfix);
        NewVal := VarAsType(FCalculator.Calculate(FPostfix), FNumberType);
        Ok:=True;
      except
        Ok:=False;
      end;
      FPostfix.Free;

      if Ok then
        begin // удалось вычислить
          FValue := NewVal;
          Info := '='+ValueToStr(NewVal);

          if (FMinValue < FValue) and (FValue < FMaxValue) then
            SpinTextColor := stcNormal
          else
            SpinTextColor := stcBounds;
        end
      else
        begin // ошибка
          Info := '!';
          SpinTextColor := stcError;
        end;
    end;

  inherited;

  if assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDeSpinEdit.SetValue(aValue: Variant);
begin
  FValue := aValue;
  Text:= ValueToStr(FValue);
end;

procedure TDeSpinEdit.TimerTimer(Sender: TObject);
begin
  if ReadOnly then Exit;

  Inc(FScrollTimes);
  if FScrollTimes <= ScrollDelayTimes then Exit;
  case FScrollBtnPressed of
    sbpUp: Value := Value + 1;
    sbpDn: Value := Value - 1;
  end;
end;

procedure TDeSpinEdit.WMMouseWheel(var Msg: TWMMouseWheel);
var d: integer;
begin
  if ReadOnly then Exit;
  inherited;
  //крутим на 1000 при нажатом Ctrl
  if (msg.Keys and MK_CONTROL <> 0) then d:=1000 else d:=1;

  if Value = Null then Value := 0;

  if msg.WheelDelta > 0 then
    Value := Value + d;
  if msg.WheelDelta < 0 then
     Value := Value - d;
end;

procedure TDeSpinEdit.SetColor(aValue: TColor);
begin
  FColor := aValue;
  inherited Color := FColor;
end;

procedure TDeSpinEdit.SetDisplayFormat(const Value: string);
begin
  if not SameStr(FDisplayFormat, Value) then
    begin
      FDisplayFormat := Value;
      Invalidate;
    end;
end;

function TDeSpinEdit.GetClientRect: TRect;
begin
  result := inherited GetClientRect;


  if FSpinWidth>0 then
    begin
      FRectBtnUp := Rect( result.Right + d,              result.Top + d,
                          result.Right + FSpinWidth + d,(result.Bottom + result.Top) div 2 + d );
      FRectBtnDn := Rect( result.Right + d,             (result.Bottom + result.Top) div 2 + d ,
                          result.Right + FSpinWidth + d, result.Bottom + d                );
    end;

  Result.Right := Result.Right - FInfoWidth;

  if FInfoWidth>0 then FRectInfo := Rect( result.Right + d, result.Top + d, FInfoWidth + result.Right + d,  result.Bottom + d )
                  else FRectInfo := Rect( -10, -10, -10, -10 );

//  Result.Right := Result.Right - FSpinWidth;
end;

procedure TDeSpinEdit.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  if Message.CalcValidRects then
    begin
      Dec(Message.CalcSize_Params^.rgrc[0].Right, FSpinWidth);
//      Dec(Message.CalcSize_Params^.rgrc[0].Right, FInfoWidth);
      message.Result:=1;
    end
  else
    begin
      Dec(Message.CalcSize_Params^.rgrc[0].Right, FSpinWidth);
      message.Result:=1;
    end;
end;

procedure TDeSpinEdit.WMNCHitTest(var Message: TWMNCHitTest);
var P: TPoint;
begin
  P:=ScreenToClient( Point(message.XPos, message.YPos) );
  if (P.X < ClientRect.Left) and (ClientRect.Right < P.X) then Message.Result:= HTMENU
                                                          else message.Result:= HTCLIENT;
end;

function TDeSpinEdit.TextWidth(const aValue: string): Integer;
var
  DC : HDC;
  aSize: TSize;
begin
  DC := GetWindowDC(Handle);
  Selectobject(DC, Font.Handle);
  aSize.cx:=0;
  aSize.cy:=0;
  GetTextExtentPoint32(DC, PChar(aValue), Length(aValue), aSize);
    Result:=aSize.cx;
  ReleaseDC(Handle,DC);
end;

procedure TDeSpinEdit.CalculateInfoWidth;
var R : TRect;
    w1,w2: integer;
begin
  R:= inherited GetClientRect;

  w1:=TextWidth(Text) + 4;
  w2:=TextWidth(FInfoText) + 4;

  if (Length(FInfoText) = 0) or (w1 + w2 > R.Right - R.Left )then FInfoWidth := 0
                                                             else FInfoWidth := w2;
end;

procedure TDeSpinEdit.SetInfo(aValue: String);
begin
  FInfoText:= aValue;
  Hint := aValue;
  CalculateInfoWidth;
  SetWindowPos(Handle, 0, Left, Top, Width, Height, SWP_NOZORDER + SWP_NOACTIVATE);
  Invalidate;
end;

procedure TDeSpinEdit.WMSize(var Message: TWMSize);
begin
  CalculateInfoWidth;
  inherited;
end;

procedure TDeSpinEdit.WMPaint(var Message: TWmPaint);
var
  DC : HDC;
  InactiveFlag: DWORD;
  brush : HBrush;
  R: TRect;
  Value: string;
  I64: Int64;
  D64: Double;
  Canvas: TControlCanvas;
  Details: TThemedElementDetails;
begin
  inherited;
  try
    DC := GetWindowDC(Handle);

    if FInfoWidth > 0 then
      begin
        Brush := CreateSolidBrush(ColorToRGB(FColor));
        FillRect(DC, FRectInfo, Brush);
        Selectobject(DC, Font.Handle);

        if FSpinTextColor = stcError   then SetTextColor(DC, $0000FF) else
        if FSpinTextColor = stcBounds  then SetTextColor(DC, $FF0000) else
        if FScrollBtnPressed = sbpInfo then SetTextColor(DC, Font.Color) else
                                            SetTextColor(DC, $606060);

        TextOut(DC, FRectInfo.Left + 2, FRectInfo.Top + 1, PChar(FInfoText), Length(FInfoText));
        DeleteObject(Brush);
      end;

    // есть проблемы своей отрисовки, надо допиливать
    if not FEditing and not (VarIsEmpty(FValue) or VarIsNull(FValue)) then
      begin
        if Length(DisplayFormat) = 0 then
          Value:= ValueToStr(FValue)
        else
          try
            if Pos('%', DisplayFormat) <> 0 then
              if FNumberType in IntVarTypes then
                begin
                  I64 := VarAsType(FValue, varInt64);
                  Value := Format(DisplayFormat, [I64]);
                end
              else if FNumberType in FloatVarTypes then
                begin
                  D64 := VarAsType(FValue, varDouble);
                  Value := Format(DisplayFormat, [D64]);
                end
              else
                Value := Format(DisplayFormat, [VarToStr(FValue)])
            else if Pos('#', DisplayFormat) <> 0 then
              if FNumberType in IntVarTypes then
                begin
                  I64 := VarAsType(FValue, varInt64);
                  Value := FormatFloat(DisplayFormat, I64);
                end
              else if FNumberType in FloatVarTypes then
                begin
                  D64 := VarAsType(FValue, varDouble);
                  Value := FormatFloat(DisplayFormat, D64);
                end
              else
                Value := Format(DisplayFormat, [VarToStr(FValue)])
            else if Pos('{', DisplayFormat) <> 0 then
              Value := FormatString(DisplayFormat, VarToStr(FValue))
            else
              Value := Text;
          except
            Value := Text;
          end;
        //
        R := ClientRect;
        Canvas := TControlCanvas.Create;
        Canvas.Control := self;

        Canvas.Brush.Color := Color;
        Canvas.FillRect(R);
        Canvas.Font.Assign(Font);
        Canvas.TextRect(R, R.Left + 1, R.Top + 1, Value);
        Canvas.Free;
        //
        // Windows.ExtTextOut(DC, R.Left + 2, R.Top + 1, ETO_CLIPPED or ETO_OPAQUE, R, Value, Length(Value), nil);
      end;

    if FSpinWidth > 0 then
    begin
      if StyleServices.Enabled then
        with StyleServices do
          begin
            if ReadOnly then                     DrawElement(DC, GetElementDetails(tsArrowBtnUpDisabled), FRectBtnUp) else
            if FScrollBtnPressed = sbpUp then    DrawElement(DC, GetElementDetails(tsArrowBtnUpPressed), FRectBtnUp) else
            if FScrollBtnPressed = sbpUpHot then DrawElement(DC, GetElementDetails(tsArrowBtnUpHot), FRectBtnUp) else
                                                 DrawElement(DC, GetElementDetails(tsArrowBtnUpNormal), FRectBtnUp);

            if ReadOnly then                     DrawElement(DC, GetElementDetails(tsArrowBtnDownDisabled), FRectBtnDn) else
            if FScrollBtnPressed = sbpDn then    DrawElement(DC, GetElementDetails(tsArrowBtnDownPressed), FRectBtnDn) else
            if FScrollBtnPressed = sbpDnHot then DrawElement(DC, GetElementDetails(tsArrowBtnDownHot), FRectBtnDn) else
                                                 DrawElement(DC, GetElementDetails(tsArrowBtnDownNormal), FRectBtnDn);
          end
      else
        begin
          if ReadOnly then InactiveFlag := DFCS_INACTIVE
                      else InactiveFlag := 0;

          if (FScrollBtnPressed = sbpUp) and (not ReadOnly) then
            DrawFrameControl(DC, FRectBtnUp, DFC_SCROLL, DFCS_SCROLLUP or DFCS_PUSHED)
          else
            DrawFrameControl(DC, FRectBtnUp, DFC_SCROLL, DFCS_SCROLLUP or InactiveFlag);

          if (FScrollBtnPressed = sbpDn) and (not ReadOnly) then
            DrawFrameControl(DC, FRectBtnDn, DFC_SCROLL, DFCS_SCROLLDOWN or DFCS_PUSHED)
          else
            DrawFrameControl(DC, FRectBtnDn, DFC_SCROLL, DFCS_SCROLLDOWN or DFCS_HOT);
        end;
    end;

  finally
    ReleaseDC(Handle, DC);
  end;
end;

constructor TDeSpinEdit.Create(AOwner: TComponent);
begin
  inherited;
  AutoSize := False; // Иначе контрол подгоняется под текущий шрифт!!!
  ParentFont:= True;

  BorderStyle := bsSingle;//bsNone; //обязательно, иначе не неклиентские сообщения не обрабатываются
  FSpinTextColor:= stcNormal;
  FColor:= clWindow;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := ScrollFrequency;
  FTimer.OnTimer := TimerTimer;

  FCalculator := TDeCalculator.Create;
  FParser     := TDeParser.Create;
  DoubleBuffered:=True;

  NumberType  := VarInteger;
  FInfoWidth  := 0;
end;

destructor TDeSpinEdit.Destroy;
begin
  FreeAndNil(FCalculator);
  FreeAndNil(FParser);
  inherited;
end;

procedure TDeSpinEdit.DoEnter;
begin
  inherited;
  FEditing := True;
end;

procedure TDeSpinEdit.DoExit;
begin
  inherited;
  FEditing := False;
  Invalidate;
end;

function TDeSpinEdit.ValueToStr(aValue: Variant): string;
var i64: Int64;
    d64: Double;
begin
  if VarIsEmpty(aValue) or VarIsNull(aValue) then
    Result:= EmptyStr else
  if FNumberType in IntVarTypes then
    begin
      i64:= aValue;
      Result:=IntToStr(i64);
    end
  else
    begin
      d64:= aValue;
      Result:=FloatToStr(d64);
    end;
end;

procedure  TDeSpinEdit.SetNumberType(aValue: TVarType);
begin
  FNumberType:=aValue;

  case aValue of
    varByte     : begin FMinValue:=                    0;  FMaxValue :=                 255; end;
    varShortInt : begin FMinValue:=                 -127;  FMaxValue :=                 127; end;
    varWord     : begin FMinValue:=                    0;  FMaxValue :=               65535; end;
    varSmallint : begin FMinValue:=               -32767;  FMaxValue :=               32767; end;
    varLongWord : begin FMinValue:=                    0;  FMaxValue :=          4294967295; end;
    varInteger  : begin FMinValue:=          -2147483647;  FMaxValue :=          2147483647; end;
    varUInt64   : begin FMinValue:=                    0;  FMaxValue := 9223372036854775807; end;
    varInt64    : begin FMinValue:= -9223372036854775807;  FMaxValue := 9223372036854775807; end;

    varSingle   : begin FMinValue:= -9223372036854775807;  FMaxValue := 9223372036854775807; end;
    varDouble   : begin FMinValue:= -9223372036854775807;  FMaxValue := 9223372036854775807; end;
    varCurrency : begin FMinValue:= -922337203685477{.0};  FMaxValue := 922337203685477{.0}; end;

    else          begin FMinValue:= -9223372036854775807;  FMaxValue := 9223372036854775807; end;
  end;

  if FNumberType in IntVarTypes then FSpinWidth:=GetSystemMetrics(SM_CXHTHUMB)
                                else FSpinWidth:=0;
end;

procedure TDeSpinEdit.setSpinTextColor(aValue: TSpinValueState);
begin
if FSpinTextColor<>aValue then
  begin
    FSpinTextColor:= aValue;
    Repaint;
  end;
end;

procedure TDeSpinEdit.KeyPress(var Key: Char);
begin

  // по Enter подставляем рассчитанное значение
  if CharInSet(Key, [#10,#13]) then
  begin
    Text := ValueToStr(FValue);
    Key := #0;
    Exit;
  end;

  if CharInSet(Key, ['.',',',FormatSettings.DecimalSeparator]) and (FNumberType in FloatVarTypes) then
  begin
//    Key := DecimalSeparator;
  end else

  // пропускаем управляющие сигналы, но блокируем и Beep при вводе все символы кроме:
  if ( Ord(Key) > 31) and not (CharInSet(Key,[' ','(',')','-','+','/','*','0'..'9',Chr(VK_BACK)])) then
  begin
    Key := #0;
    MessageBeep($0FFFFFFFF);
    Exit;
  end;

  inherited;
end;

procedure TDeSpinEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  sbp: TScrollBtnPressed;
begin
  inherited;
  sbp := FScrollBtnPressed;

  if (not ReadOnly) and (FScrollBtnPressed in [sbpNone, sbpUpHot, sbpDnHot, sbpInfo]) then
    begin
      if PointIntoRect(X, Y, FRectBtnUp) then FScrollBtnPressed := sbpUpHot else
      if PointIntoRect(X, Y, FRectBtnDn) then FScrollBtnPressed := sbpDnHot else
      if PointIntoRect(X, Y, FRectInfo) and (FSpinTextColor in [stcNormal])
                                         then FScrollBtnPressed := sbpInfo else
                                              FScrollBtnPressed := sbpNone;

      case FScrollBtnPressed of
        sbpUpHot, sbpDnHot : Cursor := crArrow;
        sbpInfo            : Cursor := crDrag;
        else                 Cursor := crDefault;
      end;

    end;

  if Not(FScrollBtnPressed = sbp) then
    Invalidate;
end;

procedure TDeSpinEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FScrollTimes := 0;

  if PointIntoRect( X, Y, FRectBtnUp) and (not ReadOnly) then
    begin
      if Value = Null then Value := 0;
      FScrollBtnPressed := sbpUp;
      Value := Value + 1;
      Invalidate;
    end;

  if PointIntoRect( X, Y, FRectBtnDn) and (not ReadOnly) then
    begin
      if Value = Null then Value := 0;
      FScrollBtnPressed := sbpDn;
      Value := Value - 1;
      Invalidate;
    end;
end;

procedure TDeSpinEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if PointIntoRect(X, Y, FRectBtnUp) and (not ReadOnly) then
    FScrollBtnPressed := sbpUpHot
  else if PointIntoRect(X, Y, FRectBtnDn) and (not ReadOnly) then
    FScrollBtnPressed := sbpDnHot
  else
    FScrollBtnPressed := sbpNone;
  if PointIntoRect(X, Y, FRectInfo) and (not ReadOnly) and (FSpinTextColor in [stcNormal]) then
  begin
    Text := ValueToStr(FValue);
  end;
  Invalidate;
end;

{ TDeSpinEditFloat }

constructor TDeSpinEditFloat.Create(AOwner: TComponent);
begin
  inherited;
  NumberType := varDouble
end;

{$REGION 'Initialization & Finalization unit procedures ...'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('DeControls unit initialization ...');
  {$ENDIF}
  TempDirectory := TPath.GetTempPath;
  if 0 < Length(TempDirectory) then
    begin
      TempDirectory := IncludeTrailingPathDelimiter(TempDirectory) + GUIDToString(NewGUID);
      if SysUtils.ForceDirectories(TempDirectory) then
        SetFileAttributes(PChar(TempDirectory), GetFileAttributes(PChar(TempDirectory)) or faHidden);
    end;
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DeControls unit finalization ...');
  {$ENDIF}
  ClearDirectory(TempDirectory, True);
  RemoveDirectory(PChar(TempDirectory));
end;
{$ENDREGION}

{ TTimeEdit }

procedure TTimeEdit.Change;
var t: string;
    IsError: Boolean;
    H,M,S,P: Integer;
begin
  if Not FMaskUpdating then
    begin
      t:= text;
      if (t= '  :  :  ') or (t= '  :  ') or (t = EmptyStr) then FValue:=null
        else
          begin
            P:= SelStart;
            IsError:= False;
            t:= StringReplace( t, ' ', '0', [rfReplaceAll]);
            H:= StrToInt('0' + Copy(t, 1, 2));
            M:= StrToInt('0' + Copy(t, 4, 2));
            S:= StrToInt('0' + Copy(t, 7, 2));

            if 23 < H then  begin IsError:= True; H:= 23; end;
            if 59 < M then  begin IsError:= True; M:= 59; end;
            if 59 < S then  begin IsError:= True; S:= 59; end;

            if IsError then
              begin
                if SameText(FFormat, 'HH:MM')
                  then Text:= System.SysUtils.Format('%2d:%2d', [H,M])
                  else Text:= System.SysUtils.Format('%2d:%2d:%2d', [H,M,S]);
                SelStart:= P;
              end;
            FValue:= EncodeTime(H, M, S, 0);
          end;
    end;
  inherited;
end;

constructor TTimeEdit.Create(AOwner: TComponent);
begin
  inherited;
  EditMask:= EmptyStr;
end;

procedure TTimeEdit.UpdateMask;
begin
  if VarIsNull(FValue) and Not Focused
    then
      begin
        FMaskUpdating:= true;
        EditMask:= EmptyStr;
        FMaskUpdating:= false;
        Text:= EmptyStr;
      end
    else
      begin
        FMaskUpdating:= true;
        if SameText(FFormat, 'HH:MM')
          then EditMask:= TimeMaskHHMM
          else EditMask:= TimeMaskHHMMSS;
        FMaskUpdating:= false;
      end;
end;

procedure TTimeEdit.WMSetFocus(var Msg: TMessage);
begin
  inherited;
  UpdateMask;
end;

procedure TTimeEdit.WMKeyDown(var Msg: TMessage);
var Dir, P: Integer;
begin
  if Msg.WParamLo = VK_UP   then Dir:=1 else
  if Msg.WParamLo = VK_DOWN then Dir:=-1 else
                                 Dir:=0;
  if Dir <> 0 then
    begin
      P:= SelStart;
      if VarIsNull(FValue) then FValue:=0;
      case P of
        0,1: begin
               Value:= IncHour (FValue, Dir);
               SelStart:= 0;
               SelLength:= 2;
             end;
        3,4: begin
               Value:= IncMinute (FValue, Dir);
               SelStart:= 3;
               SelLength:= 2;
             end;
        6,7: begin
               Value:= IncSecond (FValue, Dir);
               SelStart:= 6;
               SelLength:= 2;
             end;
      end;
    end;

  inherited;
end;

procedure TTimeEdit.WMKeyPress(var Msg: TMessage);
begin
  if Msg.WParamLo=32 then Msg.WParamLo:= 48;

  inherited;

  //
end;

procedure TTimeEdit.WMKillFocus(var Msg: TMessage);
begin
  inherited;
  UpdateMask;
end;

procedure TTimeEdit.WMMouseWheel(var Msg: TWMMouseWheel);
begin
  inherited;
  if Msg.WheelDelta > 0 then keybd_event( VK_UP,   0, KEYEVENTF_EXTENDEDKEY, 0)
                        else keybd_event( VK_DOWN, 0, KEYEVENTF_EXTENDEDKEY, 0);
end;

function TTimeEdit.GetValue: Variant;
begin
  Result:= FValue;
end;

procedure TTimeEdit.SetFormat(const Value: String);
begin
  FFormat := Value;
  UpdateMask;
end;

procedure TTimeEdit.SetValue(const Value: Variant);
var H, M, S, SS : Word;
begin
  FValue:= Value;
  UpdateMask;
  if VarIsNull(FValue) then
    begin
      Text:= EmptyStr
    end
  else
    begin
      DecodeTime(FValue, H, M, S, SS);
      FValue:= EncodeTime(H, M, S, SS);
      if EditMask = TimeMaskHHMM
        then Text:= System.SysUtils.Format('%2d:%2d', [H,M])
        else Text:= System.SysUtils.Format('%2d:%2d:%2d', [H,M,S]);
    end;
end;

{ TDeMemo }

constructor TDeMemo.Create(Owner: TComponent);
begin
  inherited;
  OnKeyPress:= DeOnKeyPress;
end;

procedure TDeMemo.DeOnKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
   ^A: begin
        (Sender as TMemo).SelectAll;
         Key := #0;
       end;
    else inherited;
  end;
end;

{ TDeEditGUID }

constructor TDeEditGUID.Create(Owner: TComponent);
var
  Bitmap: Graphics.TBitmap;
begin
  inherited Create(Owner);
  AutoSize := False; // Иначе контрол подгоняется под текущий шрифт!!!
  CharCase := ecUpperCase;
  MaxLength := 38;
  Images := TImageList.Create(Self);
  Bitmap := Graphics.TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(hInstance, 'GEAR3');
    Images.AddMasked(Bitmap, Bitmap.Canvas.Pixels[0, 0]);
  finally
    Bitmap.Free;
  end;
  RightButton.ImageIndex := 1;
  RightButton.HotImageIndex := 0;
  RightButton.PressedImageIndex := 0;
  RightButton.DisabledImageIndex := 2;
  RightButton.Visible := True;
  RightButton.Enabled := False;
  OnRightButtonClick := RightButtonClick;
end;

destructor TDeEditGUID.Destroy;
begin
  Images.Free;
  inherited Destroy;
end;

function TDeEditGUID.GetValue: Variant;
begin
  if Modified then
    Result := Text
  else
    Result:= FValue;
end;

procedure TDeEditGUID.SetValue(const Value: Variant);
begin
  FValue := Value;
  Modified := False;
  Hint := EmptyStr;

  if VarIsNull(Value) or VarIsEmpty(Value) then
    begin
      Clear;
      RightButton.Enabled := True;
    end
  else
    begin
      RightButton.Enabled := False;
      if VarIsArray(Value) then
        begin
          if 0 < SizeOf(Value) then
            Text := VarArrayGet(Value, [VarArrayLowBound(Value, 1)])
          else
            Clear;
          Hint:= IntToStr(SizeOf(Value)) + ' значений';
        end
      else
        Text:= VarToStr(Value);
    end;
end;

//         11111111112222222222333333333
//12345678901234567890123456789012345678
//--------------------------------------
//01234567890123456789012345678901234567
//{282C33EA-D6B7-4B98-8E6A-6057A134F490}

procedure TDeEditGUID.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #8: { Ничего не делаем };
    '{': if SelStart <> 0 then Key := #0;
    '}': if SelStart <> 37 then Key := #0;
    '-': if (Pos('{', Text) = 1) and (SelStart in [9, 14, 19, 24]) then
           { Ничего не делаем }
         else
           Key := #0;
    '0'..'9', 'a'..'f', 'A'..'F':
       case SelStart of
         0: if Pos('{', Text) = 0 then
              begin
                Text := '{' + Text;
                SelStart := 2;
              end;
         9: begin
              Text := Text + '-';
              SelStart := 10;
            end;
         14: begin
               Text := Text + '-';
               SelStart := 15;
             end;
         19: begin
               Text := Text + '-';
               SelStart := 20;
             end;
         24: begin
               Text := Text + '-';
               SelStart := 25;
             end;
         36: if Pos('}', Text) = 0 then
               begin
                 Text := Text + '}';
                 SelStart := 36;
               end;
       end;
  else
    Key := #0;
  end;
end;

procedure TDeEditGUID.RightButtonClick(Sender: TObject);
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  SetValue(GUIDToString(GUID));
end;

procedure TDeEditGUID.WMUndo(var Message: TWMCut);
begin
  SetValue(FValue);
end;

{ TDeEditFile }

constructor TDeEditFile.Create(Owner: TComponent);
var
  Bitmap: Graphics.TBitmap;
begin
  inherited Create(Owner);
  AutoSize := False; // Иначе контрол подгоняется под текущий шрифт!!!
  Images := TImageList.Create(Self);
  Bitmap := Graphics.TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(hInstance, 'FOLDER3');
    Images.AddMasked(Bitmap, Bitmap.Canvas.Pixels[0, 0]);
  finally
    Bitmap.Free;
  end;
  RightButton.ImageIndex := 1;
  RightButton.HotImageIndex := 0;
  RightButton.PressedImageIndex := 0;
  RightButton.DisabledImageIndex := 2;
  RightButton.Visible := True;
  OnRightButtonClick := RightButtonClick;
end;

destructor TDeEditFile.Destroy;
begin
  Images.Free;
  inherited Destroy;
end;

function TDeEditFile.GetValue: Variant;
begin
  if Modified then
    Result := Text
  else
    Result:= FValue;
end;

procedure TDeEditFile.SetValue(const Value: Variant);
begin
  FValue := Value;
  Modified := False;
  Hint := EmptyStr;

  if VarIsNull(Value) or VarIsEmpty(Value) then
    Clear
  else
    if VarIsArray(Value) then
      begin
        if 0 < SizeOf(Value) then
          Text := VarArrayGet(Value, [VarArrayLowBound(Value, 1)])
        else
          Clear;
        Hint := IntToStr(SizeOf(Value)) + ' файлов';
      end
    else
      Text := VarToStr(Value);
end;

procedure TDeEditFile.WMUndo(var Message: TWMCut);
begin
  SetValue(FValue);
end;

procedure TDeEditFile.RightButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(Self);
  try
    OpenDialog.Filter := 'All files|*.*';
    OpenDialog.Options := OpenDialog.Options + [ofPathMustExist, ofFileMustExist];
    OpenDialog.InitialDir := ExtractFilePath(VarToStr(Value));
    OpenDialog.FileName := ExtractFileName(VarToStr(Value));
    if OpenDialog.Execute then
      SetValue(OpenDialog.FileName);
  finally
    OpenDialog.Free;
  end;
end;

{ TDeCheckBox }

constructor TDeCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption:= EmptyStr;
  AutoSize := False; // Иначе контрол подгоняется под текущий шрифт!!!
end;

procedure TDeCheckBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

initialization
  Startup;

finalization
  Shutdown;

end.

