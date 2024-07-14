unit DeTypes;

interface

uses Messages, CommCtrl, SysUtils;

Const
  varFmtBCD = $0111; // ���������������� ��� ������. ��� ������������ �������� �� ����� FmtBCD

  CatalogsIndex = -1;
  SampleMTIndex = -2;
  LanguageIndex = -3;

  InternalServerGUID =   '{A1310EE3-DBC0-4E14-9EA8-8589226E18E9}';

  LanguageDatabaseGUID = '{43FA4423-DBC0-4CFA-B508-D77BB092F94E}';
  CatalogsDatabaseGUID = '{9AE0FA7C-DBC0-43FF-858D-61DD8FF3AF7F}';
  SampleMTDatabaseGUID = '{FE8ED7E0-DBC0-4C3B-81B2-13BF1C6C9422}';
  MetadataDatabaseGUID = '{D9188055-DBC0-4A10-8750-80F9914B4E48}';

  EmptyPasswordHash = 'd41d8cd98f00b204e9800998ecf8427e';

  sp          = ' ';
  SplitText   = ' � ';
  CheckText   = '�';
  BadCheckText = '?';

  ExcelName = 'Excel.Application';
  WordName  = 'Word.Application';

  BDEDirPattern  = '\~de%0.5d.tmp';
  AnyFileMask    = '~*.*';
  LastValuesMask = 'LastValues.%d.%s';

  cApplicationName = 'dbco';
  cAppTitle = 'DBco|DataBase Companion';
  RegKey = '\Software\' + cApplicationName + '\';  // ��� ������ �������
  urlDownload = 'https://www.dbco.ru/download/';
  urlUpdate = 'https://www.dbco.ru/download/update.xml';
  urlUpdatePro = 'https://www.dbco.ru/download/dbco.exe';
  urlHome   = 'https://www.dbco.ru/';
  urlMail   = 'mailto:it@dbco.ru?subject=Support&body=';

  imCheckNo   = -1;
  imCheckPK   = 5;
  imCheckFK   = 6;

const
  StatusPrintID = 48;
  xmlHeaderStr  = '<?xml version="1.0" encoding="windows-1251"?>';

  SectionName : array[False..True] of string = ('OVERRIDE', 'DEFINE');

const
  { ������ ������������� ������� }
  ctcSelect            = $01;     // ��� ������
  ctcInsert            = $02;     // ��� �������
  ctcUpdate            = $04;     // ��� ����������
  ctcDelete            = $08;     // ��� ��������
  ctcBasket            = $10;     // ��� ������� � ��������

  DeSignature: AnsiString = 'DE'#222;
  DeSignature2: AnsiString = 'DE'#237; // ��������� ������ ������� ��� TDeImage (v.16.1)
  DeSLength   = 3;

type
  /// <summary>���� ������� � �������������:
  /// <para><c>mcNone</c> - ������ �� ���������</para>
  /// <para><c>mcUpdate</c> - ��������� ����������</para>
  /// <para><c>mcInsert</c> - ��������� �������</para>
  /// <para><c>mcDelete</c> - ��������� ��������</para>
  /// <para><c>mcUpdateLibrary</c> - ��������� ���������� ���������</para>
  /// </summary>
  TTypeChanged = (mcNone, mcUpdate, mcInsert, mcDelete, mcUpdateLibrary);

  TDataChanges = set of TTypeChanged;

  /// <summary>���� ���������� ����:
  /// <para><c>fsKey</c> - ������ �������� ����</para>
  /// <para><c>fsBase</c> - �������� ����, ���, �������, ���������</para>
  /// <para><c>fsFull</c> - ��������� �������� �� BLOB ����</para>
  /// <para><c>fsBlob</c> - BLOB ����, �������� ��������� ��������</para>
  /// </summary>
  TFieldStage =  (
    fsKey    =  0,     // ������ �������� ����
    fsBase   =  1,     // �������� ����, ���, �������, ���������
    fsFull   =  2,     // ��������� �������� �� ���� ����
    fsBlob   =  3,     // ���� ���� - ������ ��������� ��������
    fsMax    =  4      // ������������ �������, ��� ������� ������ �� ������������ (������ ��� TCacheItem)
    );
  TFieldStages = set of TFieldStage;

const
  itElement = 1;
  itGroup = 2;
  itAttribute = 3;

  UnSignedIntVarTypes   = [ varByte, varWord, varLongWord{, varWord64} ];
  SignedIntVarTypes = [ varShortInt, varSmallint, varInteger, varInt64 ];
  IntVarTypes =         SignedIntVarTypes + UnsignedIntVarTypes;

  FloatVarTypes =     [ varSingle, varDouble, varCurrency ];  // + varFmtBCD

{$IFDEF DeDEBUG}
const
  FieldStages: array[TFieldStage] of PChar = ('fsKey', 'fsBase', 'fsFull', 'fsBlob'{, 'fsLookUp', 'fsCalc', 'fsLocate'}, 'fsMax');
{$ENDIF}

{$IFDEF DEBUG}
function FieldStagesToString(const Stages: TFieldStages): string;
{$ENDIF}

type
  /// <summary>������� ������:
  /// <para><c>saDataset</c> - � ������ ������</para>
  /// <para><c>saCurrentCache</c> - � ������� ����</para>
  /// <para><c>saEverywhere</c> - �����</para>
  /// </summary>
  TSearchArea = (saDataset, saCurrentCache, saEverywhere);

  /// <summary>��� ������:
  /// <para><c>stInField</c> - ����� �� ����</para>
  /// <para><c>stByRole</c> - ����� �� ����</para>
  /// </summary>
  TSearchType = (stInField, stByRole);

  /// <summary>���� ���������� �����������</summary>
  TAgregateType = (
    atNo,
    atCount,
    atValue,
    atSum,
    atMin,
    atMax,
    atAverage
    );

  /// <summary>����� ���������� ������ �� TDataCache</summary>
  TDataFlag = (
    dfStoredFields,     // ������� �������� ����
    dfLookupFields,     // ������� ��������� ��������
    dfCalculatedFields, // ������� ����������� ����
    dfAllRecords,       // ��� ������
    dfSelectedRecords,  // ��� ���������� ������
    dfFocusedRecords    // ������ ������ � ������
    );

  TDataFlags = set of TDataFlag;

const
  TAgregateTypeNames : array[Low(TAgregateType).. High(TAgregateType)] of string = (
    '_Da.AgrNo',
    '_Da.AgrCount',
    '_Da.AgrValue',
    '_Da.AgrSum',
    '_Da.AgrMin',
    '_Da.AgrMax',
    '_Da.AgrAve'
    );

const
  TAgregateTypeID : array[Low(TAgregateType).. High(TAgregateType)] of string = (
    'No',
    'Count',
    'Value',
    'Sum',
    'Min',
    'Max',
    'Ave'
    );

type
  /// <summary>���� ���������� �����������</summary>
  TPolicyShowType = (
    psUnassigned        =  0,
    psShow              =  1,
    psNone              =  2
    );

  /// <summary>���� �����������</summary>
  TShowType = (
    stNone              =  0,
    stNative            =  1,
  //stUnicode           =  2,
    stTranslate         =  3,
  //stDOSString         =  4,
    stYesNo             =  5,
    stPassword          =  6,
    stEncryptedPassword =  7,
    stBarCode           =  8
    );

  /// <summary>������� ��������� ����</summary>
  TFieldVisible = (
    fvService  = 0,  // ��������� ����
    fvLevel1   = 1,  // ������� ��������� 1
    fvLevel2   = 2,  // ������� ��������� 2
    fvLevel3   = 3,  // ������� ��������� 3
    fvCaption  = 4   // ������� ��������� ���� ��������� (���������� ������!)
  );

  TFieldRoleType = (
    frNormal,                       // ������� ����
    frNameField,                    // ���� �����                   (NField)
    frDeleteSignField,              // ������� ����������� ������   (DField)
    frDefaultRecordSign,            // ������� ������ �� ���������  (OField)
    frGroupDefault,                 // �� ��������� (������)
    frFolderSign,                   // ���� ����������� (�����)     (GField)
    frValueNotPaste,                // ������� ������������� � ����� ������ �������� ���� (��� GUID`��; v. 18.12)
    frIcon,                         // ������� ���� ������
    frColor,                        // ������� ���� ����� ������
    // v.18.6
    frLatitude,                     // ���� �������������� ������   (LatitudeField)
    frLongitude,                    // ���� �������������� �������  (LongitudeField)
    frAddress,                      // ���� ������ ��� GEO ������   (AddressField)
    frPhone,                        // ���� ������ ��������         (PhoneField)
    frEMail,                        // ���� ������������ ������     (MailField)
    frDropFile                      // ���� ����������� ��� Drag&Drop: �������� - ftBtring
                                    //                                 ���������� - Bftlob
                                    //                                 ������ - ftInteger
                                    //                                 ���� �������� - ftDate
    );

  TFieldRole = set of TFieldRoleType;

const
  RoleFlag : array [Low(TFieldRoleType) .. High(TFieldRoleType)] of integer = (
    $000000, $000001, $000002, $000004, $000008, $000010, $000200, $000400, $000800,
    $004000, $008000, $010000, $020000, $040000, $080000
  );

type
  /// <summary>������������� �������</summary>
  TConditionEvent = (
    ckNone    = 0,    //  �������� �� ������
    ckError   = 1,    //  ������� ������� (������ ��� ����������, �������, ��������)
    ckWarning = 2     //  ������ ������� (��������� ��� ����������, ���������� ����������)
  );

  TOperationType = (
    opNone,         // �������� �� ������
    opAnd,          // and
    opOr,           // or
    opXor,          // xor
    opNot,          // not
    opPlus,         // +
    opPlusUno,      // +
    opMinus,        // -
    opMinusUno,     // -
    opMul,          // *
    opDiv,          // /
    opIntDiv,       // ������� ������
    opMod,          // ������� �� �������
    opAssign,       // :=
    opLT,           // <
    opLE,           // <=
    opGT,           // >
    opGE,           // >=
    opEQ,           // =
    opNE,           // <>
    opLike,         // Like
    opNotLike,      // not like
    opIs,           // ��������� � (NULL)
    opIsNot,        // ��������� � �� (NULL)
    opIsTrue,       // ��������� � TRUE
    opIsFalse,      // ��������� � FALSE
    opIN,           // ���������� �� ���������
    opNotIN,        // ���������� �� ���������
    opArray,        //  | [ ,  , ]  ������ ��������
    opNext,         //  ; ������ ��������
    opCOUNT,        // ���������� ��������� ���������
    opSUM,          // ����� ��������� ���������
    opMIN,          // ����������� ������� ���������
    opMAX,          // ������������ ������� ���������
    opAVG           // ������� ������� ���������
    );
const
  { ����� ���������������� ���������� �������� }
  TrueLexeme = 'true';
  FalseLexeme = 'false';
  NullLexeme = 'null';
  UnassignedLexeme = 'unassigned';

  OperationTypeNames: array[TOperationType] of PChar =
    (
      'NULL', 'AND', 'OR', 'XOR', 'NOT', 'PLUS', 'PLUSUNO', 'MINUS', 'MINUSUNO', 'MUL', 'DIV', 'INTDIV', 'MOD', 'ASSIGN',
      'LT', 'LE', 'GT', 'GE', 'EQ', 'NE', 'LIKE', 'NOTLIKE',
      'IS', 'ISNOT', 'ISTRUE', 'ISFALSE', 'IN', 'NOTIN', 'ARRAY', 'NEXT',
      'COUNT','SUM','MIN','MAX','AVG'
    );

  { ��������� ����������� �������� }
  OperationLexeme : array [Low(TOperationType)..High(TOperationType)] of string = (
    '', 'and', 'or', 'xor', 'not', '+', '+', '-', '-', '*', '/', 'div', 'mod', '',
    '<', '<=', '>', '>=', '=', '<>', 'like', 'not like',
    'is', 'is not', TrueLexeme, FalseLexeme, 'in', 'not in', ',', ';',
    'count', 'sum', 'min', 'max', 'avg');

  { ��������� ����������� �������� � ��������}
  OperationCaption : array [Low(TOperationType)..High(TOperationType)] of string = (
    '', 'and', 'or', 'xor', WideChar($00AC), '+', '+', '-', '-', '*', '/', 'div', 'mod', ':=',
    '<', '<=', '>', '>=', '=', '<>', WideChar($2248),  WideChar($00AC) + ' ' + WideChar($2248),
    WideChar($00F8), WideChar($00AC) + ' ' + WideChar($00F8), '= _OpIsTrue', '= _OpIsFalse', 'in', WideChar($00AC)+' in', ',', ';',
    'count', 'sum', 'min', 'max', 'avg');

  /// <summary>��������� ��������� ������� �������</summary>

type
  TConditionRec = record
                    Field    : array [0..30] of Char;    // Field Name
                    Value    : OleVariant;               // Field Value
                    Operation : TOperationType;           // Operation Types
                  end;
  PConditionRec = ^TConditionRec;

type
  /// <summary>��������� �������� ���������� ��������</summary>
  TConditionContext = (
    ccSelect,           // �������� ��� �������
    ccInsert,           // �������� ��� �������
    ccUpdate,           // �������� ��� ����������
    ccDelete,           // �������� ��� ��������
    ccBasket            // �������� ��� ������� � ��������
        );

  TConditionContexts = set of TConditionContext;

  /// <summary>���� ���������� ���������</summary>
  TElementType = (
    { ����������� }
    etNone         = 0,                 // ������� �� �����
    etForm         = 1,                 // �����
    etPanel        = 2,                 // ������
    etLabel        = 3,                 // �������
    etBevel        = 4,                 // �����
    etTabSheet     = 5,                 // ��������/��������
    etDefault      = 6,                 // ����� � ����������� �� ���� ����
    etDateTime     = 7,                 // ����/�����
    etCheckBox     = 8,                 // CheckBox
    etTime         = 10,                // �����
    etLinkedCombo  = 11,                // ��������� ComboBox
    etExplorer     = 12,                // ��� ���������
    etButton       = 13,                // ������

    etInteger      = 15,                // ����� �����
    etFloat        = 16,                // ������������ �����
    etString       = 17,                // ������

    etMapObject    = 20,                // �������������� ������� �������
    etMapPointObject    = 21,           // �������������� ����� �������

    { �������������� }
    etBrowser      = 50,                // Explorer
    etTreeBox      = 51,                // ����������� ComboBox
    etGrid         = 53,                // Grid
    etBarCode      = 54,                // BarCode
    etIconBox      = 55,                // ������ ������
    etCheckListBox = 56,                // ������ CheckBox'��
    etListTabSheet = 57,                // �������� �� ������� �������
    etColorComboBox= 58,                // ���������� ������ ��� ������ �����
    etTree         = 59,                // ������ ������� � ���� ������
    etFileName     = 60,                // ��� ����� �� ����� ��� �� ����

    { ������ ��� ������������� }
    etFieldsBox    = 101,               // ������ �����
    etTablesBox    = 102                // ������ ������
    );

    /// <summary>��� �������� ����������� ���������</summary>
    TElementControlType = (cnVisible, cnEnabled, cnValue, cnFilter);
    TElementControl = set of TElementControlType;
type
  TElementTypes = set of TElementType;

const
  /// <summary>���� ���������, ����������� ������������� ��������</summary>
  EditorElements : set of TElementType =
    [etDefault, etDateTime, etCheckBox, etTime, etTreeBox, etIconBox,
     etCheckListBox, etFieldsBox, etTablesBox, etLinkedCombo, etMapObject,
     etMapPointObject, etColorComboBox,
     etInteger, etFloat, etString, etFileName];
  /// <summary>���� ���������, ��������� ���������� ��������</summary>
  ViewerElements : set of TElementType =
    [etBarCode, etExplorer, etBrowser];
  /// <summary>���� ���������, ������� ����� ��������� ������ ��������</summary>
  ContainerElements : set of TElementType =
    [etForm, etPanel, etTabSheet];
  /// <summary>���� ���������, ������� ����� �������� � ���� "�������"</summary>
  CreateElements : set of TElementType =
    [etPanel, etLabel, etBevel, etGrid, etButton];
  /// <summary>���� ���������, ������� �� ��������� ��������</summary>
  SimpleElements : set of TElementType =
    [etNone, etForm, etPanel, etLabel, etBevel, etButton, etTabSheet];
type

  /// <summary>��������, ������������ ��������� �������� ������ ��� �������� ������������</summary>
  TDeleteAction = (
    daNone        = 0,  // �������� �� �����������
    daRestrict    = 1,  // ������ ������� ������������ ������ ��� ������������� ��������
    daCascade     = 2,  // ��� �������� ������ ��������� ��� �������� ������������
    daSetNull     = 3,  // ��� �������� ������������ ������ �������� �������� ����� ��������������� � Null
    daFullCascade = 4   // ��� �������� ������ ��������� ��� �������� ������������ � ���������� ��� ����������� � ����� ������
  );

  /// <summary>��������, ������������ ��������� ������ ��� Execute (������� ����) ������</summary>
  TExecuteAction = (
    eaNone,             // �������� �� �����������
    eaDetails,          // �������� ����� �������������� ��������� ������
    eaChildren          // ������� � ����������� ����� ������
    );

  /// <summary>��� ������������ �������������� ������������� ������ ������</summary>
  TDSViewType = (
    vtNone      = 0,    // ��� ������ �� �����
    vtList      = 1,    // �������
    vtTree      = 2,    // ������
    {$IFDEF VECTOR_MAP}
    vtVectorMap = 3,    // ��������� ��������
    {$ENDIF}
    vtCalendar  = 4,    // ���������
    vtChart     = 5,    // ���������
    vtGroupList = 6,    // �������������� ������
    vtPivot     = 7,    // Pivot Table
    vtTileMap   = 8     // �������� �����
    );

  /// <summary>���������� ������� � ������ ������</summary>
  /// �������� �� ������, � ���� ������������
  TDSSortDirection = (
    sdNone       =  0,   // ��� ����������
    sdAscending  =  1,   // ������ ����������
    sdDescending =  2    // �������� ����������
    );

type
  /// <summary>��������� ���� ��� ������</summary>
  TDatabaseType = (
    dtNone      = 0,
    dtInterbase = 1,    //  ������ ����� ���������� IBX
    dtBDE       = 2,    //  ������ ����� BDE
    dtParadox   = 3,    //  ������ � Paradox (����� BDE)
    dtOracle    = 4,    //  ������ ����� Oracle
    dt_5        = 5,    //  ������ ����� XML
    dtFileDB    = 6,    //  ������ ����� �������� �������
    dtPostgreSQL= 7,    //  ������ ����� dbExpress drivers PostgreSQL
    dtMSSql     = 8,    //  ������ � MS SQL  (����� ADO)
    dtODBC      = 9,    //  ������ ����� ODBC (����� ADO)
    dtdBase     = 10,   //  ������ � dBase (����� BDE)
    dtMSAccess  = 11,   //  ������ � MS Access (����� ADO)
    dtMySQL     = 12,
    dtSyBase    = 13,
    dtInformix  = 14,
    dtDB2       = 15,
    dtDBCOXML   = 16,   //  ������ ������ � XML / XMLX / Directory
    dtADOXML    = 17,   //  ������ ����� ADO XML / Directory
    dtFireBird  = 18,   //  ������ ����� ���������� FireDAC
    dtSQLite    = 19    //  ������ ����� ���������� FireDAC
    );

  TIsDirectory = (
    idUnknown = 0,
    idLocal = 1,
    idGlobal = 2,
    idPrimary = 3,
    idSecondory = 4,
    idExtention = 5
    );
const
  CaseSensitiveDatabaseType = [dtPostgreSQL];

  categoryNo        = '';
  categoryMenuItems = 'MenuItems';
  categoryDatasets  = 'DataSet';
  categoryReports   = 'Reports';
  categoryCommands  = 'Commands';
  categoryActions   = 'Actions';

type
  /// <summary>������� ����� � ���������:
  /// <para><c>lmUser</c> - ����� "������������"</para>
  /// <para><c>lmDeveloper</c> - ����� "�����������"</para>
  /// </summary>

  /// <summary>�������� � ��������� ������������</summary>
  TSecurityOperation = (
    spNone      = 0,    // �������� �� ������
    spAll       = 1,    // ��� ��������
    spSelect    = 2,    // ������� ������ (soDataset)
    spInsert    = 3,    // ������� ������ (soDataset)
    spUpdate    = 4,    // ��������� ������ (soDataset)
    spDelete    = 5,    // �������� ������ (soDataset)
    spExecute   = 6     // ���������� �������� (soAction)
  );

  /// <summary>����������� �� �������� ��� �������� ������������</summary>
  TObjectRestrict = (
    orNone       = 0,   // ����������� �� ������
    orEnabled    = 1,   // ������ ������������ ��������
    orDisabled   = 2    // ������ ������������ ����������
  );

  /// <summary>�������� � ��������� ������������ DataSet`�:
  /// <para><c>sdSelect</c> - ������� ������</para>
  /// <para><c>sdInsert</c> - ������� ������</para>
  /// <para><c>sdUpdate</c> - ��������� ������</para>
  /// <para><c>sdDelete</c> - �������� ������</para>
  /// <para><c>sdShow</c> - ����������� ������</para>
  /// </summary>
  TSecurityDataset = (sdSelect, sdInsert, sdUpdate, sdDelete, sdShow);

resourcestring
  // ����� ��������������� ������
  tblLanguage         = 'MD_LANGUAGE';
  tblDictonary        = 'M_DICTIONARY';
  tblUsers            = 'M_USERS';
  tblBase             = 'M_DATABASES';
  tblSolutions        = 'M_SOLUTIONS';
  tblDataset          = 'M_DATASETS';
  tblMenu             = 'M_MENU';
  tblFields           = 'M_FIELDS';
  tblElement          = 'M_ELEMENTS';
  //tblReports          = 'M_REPORTS';
  //tblReportParams     = 'M_REPORT_PARAMS';
  tblParameters       = 'M_PARAMETERS';
  tblMembership       = 'M_MEMBERSHIP';
  tblRights           = 'M_RIGHTS';
  tblTasks            = 'M_TASKS';
  tblUserTasks        = 'M_USERTASKS';
  tblInterRelations   = 'M_INTERRELATIONS';
  tblNotes            = 'M_NOTES';
  tblActionConditions = 'M_ACTIONCONDITIONS';
  tblConstraints      = 'M_CONSTRAINTS';
  tblUserDataSet      = 'M_UDATASET';
  //tblUserFields       = 'M_UFIELDS';  // � v.16.10 ��� �� ������������!
  //tblAction           = 'M_ACTIONS';  // ����� ���������� ��������, �� ������ � MD_ACTIONS
  tblCommand          = 'M_COMMANDS'; // ������� ������ M_ACTIONS! ����������� � v.15.10
  tblCommandParams    = 'M_COMMAND_PARAMS';

  // ����� ��������������� ��������
  // ������� � v.16.03 ��� ��������� ���������� � ����������.
  // prcUpdateCommandParams = 'SP_UPDATE_COMMAND_PARAMS'; // ���������� ���������� ��� �������� ��������. ����������� � v.15.10

  // ����� ������������
  tblDDatabaseType    = 'MD_DATABASE_TYPE';
  tblDCharSets        = 'MD_CHARSETS';
  tblDActions         = 'MD_ACTIONS';
  tblDProcNameActions = 'MD_PROCEDURENAME_ACTION';
  tblDElementTypes    = 'MD_ELEMENT_TYPE';

  { ����� ����� ������� M_LANGUAGE ������������� }
  fldLanguageID       = 'ID';
  fldLanguageName     = 'NAME';
  fldLanguageCode     = 'CODE';
  fldLanguageSign     = 'SIGN';

  { ����� ����� ������� M_FIELDS ������������� }
  fldFieldsID         = 'MFL_ID';
  fldFieldsOriginal   = 'MFL_ORIGINAL';
  fldFieldsTable      = 'MFL_TABLE';
  fldFieldsName       = 'MFL_NAME';
  fldFieldsDescription= 'MFL_DESCRIPTION';
  fldFieldsReadOnly   = 'MFL_READONLY';
  fldFieldsKey        = 'MFL_KEY';
  fldFieldsUnique     = 'MFL_UNIQUE';
  fldFieldsNotNull    = 'MFL_NOTNULL';
  fldFieldsCalculated = 'MFL_CALCULATED';
  fldFieldsDataType   = 'MFL_DATATYPE';
  fldFieldsDataSize   = 'MFL_DATASIZE';
  fldFieldsPrecision  = 'MFL_PRECISION';
  fldFieldsLink       = 'MFL_LINK';
  fldFieldsOnDelete   = 'MFL_ON_DELETE_ACTION';
  fldFieldsRole       = 'MFL_ROLE';
  fldFieldsVisible    = 'MFL_VISIBLE';
  fldFieldsWidth      = 'MFL_WIDTH';
  fldFieldsOrder      = 'MFL_ORDER';
  fldFieldsTemplate   = 'MFL_TEMPLATE';
  fldFieldsDefault    = 'MFL_DEFAULT';
  fldFieldsValue1     = 'MFL_VALUE1';
  fldFieldsValue2     = 'MFL_VALUE2';
  fldFieldsShowType   = 'MFL_SHOWTYPE';
  fldFieldsDeleted    = 'MFL_DELETED';
  fldFieldsLinkRole   = 'MFL_LINKROLE';
  fldFieldsCategory   = 'MFL_CATEGORY';
  fldFieldsStored     = 'MFL_STORED';
  fldFieldsMaxWidth   = 'MFL_MAXWIDTH'; // v.16.10
  fldFieldsCharset    = 'MFL_CHARSET';  // v.17.01
  fldFieldsGUID       = 'MFL_GUID';     // v.17.06
  fldFieldsDefaultDB  = 'MFL_DEFAULTDB';//
  fldFieldsScale      = 'MFL_SCALE';

  { ����� ����� ������� M_MENU ������������� }
  fldMenuId           = 'MMN_ID';
  fldMenuOwner        = 'MMN_OWNER';
  fldMenuOrder        = 'MMN_ORDER';
  fldMenuName         = 'MMN_NAME';
  fldMenuDataSet      = 'MMN_DATASET';
  fldMenuDataSetView  = 'MMN_DATASETVIEW';
  fldMenuDataSetPars  = 'MMN_DATASETPARS';
  fldMenuType         = 'MMN_TYPE';
  fldMenuGroupField   = 'MMN_GROUPFIELD';
  fldMenuDeleted      = 'MMN_DELETED';
  fldMenuIco          = 'MMN_ICO';
  fldMenuDataFilter   = 'MMN_FILTER';
  fldMenuSubjectID    = 'MMN_SUBJECTID';
  fldMenuGUID         = 'MMN_GUID';     // v.17.10
  fldMenuSolution     = 'MMN_SOLUTION';
  fldMenuBreak        = 'MMN_BREAK';
  fldMenuSystem       = 'DE_SYSTEM';  // ��� ����������� ������ ������������

  { ����� ����� ������� M_ELEMENTS ������������� }
  fldElemId           = 'MEL_ID';
  fldElemDataSet      = 'MEL_DATASET';
  fldElemOwner        = 'MEL_OWNER';
  fldElemName         = 'MEL_NAME';
  fldElemField        = 'MEL_FIELD';
  fldElemType         = 'MEL_TYPE';
  fldElemLink         = 'MEL_LINK';
  fldElemFont         = 'MEL_FONT';
  fldElemAL           = 'MEL_AL';
  fldElemAR           = 'MEL_AR';
  fldElemL            = 'MEL_L';
  fldElemR            = 'MEL_R';
  fldElemT            = 'MEL_T';
  fldElemH            = 'MEL_H';
  fldElemColor        = 'MEL_COLOR';
  fldElemLinkField    = 'MEL_LINKFIELD';
  fldElemDeleted      = 'MEL_DELETED';
  fldElemVisible      = 'MEL_VISIBLE';
  fldElemReadOnly     = 'MEL_READONLY';
  fldElemExprName     = 'MEL_EXPRNAME';
  fldElemExprFilter   = 'MEL_EXPR_FILTER';
  fldElemExprVisible  = 'MEL_EXPR_VISIBLE';
  fldElemExprReadOnly = 'MEL_EXPR_READONLY';
  fldElemExprValue    = 'MEL_EXPR_VALUE';
  fldElemXML          = 'MEL_XML';  // v.16.10

  { ����� ����� ������� M_SOLUTIONS ������������� }
  fldConfID           = 'MSL_ID';
  fldConfVersion      = 'MSL_VERSION';
  fldConfName         = 'MSL_NAME';
  fldConfDescription  = 'MSL_DESCRIPTION';
  fldConfDeveloper    = 'MSL_DEVELOPER';
  fldConfDeleted      = 'MSL_DELETED';
  fldConfGUID         = 'MSL_GUID';     // v.17.06

  { ����� ����� ������� M_DICTIONARY ������������� }
  fldDictionaryID           = 'MDC_ID';
  fldDictionaryName         = 'MDC_NAME';
  fldDictionaryLangID       = 'MDC_LANG';
  fldDictionaryTranslate    = 'MDC_TRANSLATE';

  //fldLangRus          = 'ML_RUS';  // ��������
  //fldLangEng          = 'ML_ENG';  // ��������

  { ����� ����� ������� M_PARAMETERS ������������� }
  fldParamID          = 'MPR_ID';
  fldParamConfig      = 'MPR_CONFIG';
  fldParamName        = 'MPR_NAME';
  fldParamValue       = 'MPR_VALUE';
  fldParamDeleted     = 'MPR_DELETED';
  fldParamDataType    = 'MPR_DATATYPE'; // v. 17.5
  fldParamWhenCalc    = 'MPR_WHENCALC';

  { ����� ����� ������� M_DATABASES ������������� }
  fldBaseId           = 'MDB_ID';
  fldBaseType         = 'MDB_TYPE';
  fldBaseConnectStr   = 'MDB_PATH';
  fldBaseLogin        = 'MDB_LOGIN';
  fldBasePassword     = 'MDB_PASSWORD';
  fldBaseDeleted      = 'MDB_DELETED';
  fldBaseDefault      = 'MDB_DEFAULT';
  fldBaseReadOnly     = 'MDB_READONLY';
  fldBaseAlias        = 'MDB_ALIAS';
  fldBaseServer       = 'MDB_SERVER';
  fldBaseDatabase     = 'MDB_DATABASE';
  fldBaseCharset      = 'MDB_CHARSET'; // v. 17.01
  fldBaseGUID         = 'MDB_GUID';
  fldBaseSolution     = 'MDB_SOLUTION';

  { ����� ����� ������� M_DATASETS ������������� }
  fldDataSetId        = 'MDS_ID';
  fldDataSetName      = 'MDS_NAME';
  fldDataSetICO       = 'MDS_ICO';
  fldDataSetTable     = 'MDS_TABLE';
  fldDataSetGener     = 'MDS_GENERATOR';
  fldDataSetSelect    = 'MDS_SELECT';
  //fldDataSetShort     = 'MDS_SHORT'; v.17.07 - ������ ��-�� ������������!
  fldDataSetUpdate    = 'MDS_UPDATE';
  fldDataSetInsert    = 'MDS_INSERT';
  fldDataSetDelete    = 'MDS_DELETE';
  fldDataSetDatabase  = 'MDS_DATABASE';
  fldDataSetReadOnly  = 'MDS_READONLY';
  fldDataSetSolution  = 'MDS_SOLUTION';
  fldDataSetIsList    = 'MDS_ISLIST';
  fldDataSetParent    = 'MDS_PARENT';
  fldDataSetGViewType = 'MDS_GVIEWTYPE';
  fldDataSetGViewPars = 'MDS_GVIEWPARS';
  fldDataSetDeleted   = 'MDS_DELETED';
  fldDataSetOnAddMenue= 'MDS_ONADDMENU';
  fldDataSetFilter    = 'MDS_FILTER';
  fldDataSetConfig    = 'MDS_SOLUTION';
  fldDataSetDescription = 'MDS_DESCRIPTION';
  fldDataSetCaption   = 'MDS_CAPTION';  // v.16.10
  fldDataSetGUID      = 'MDS_GUID';     // v.17.06
  fldDataSetSchema    = 'MDS_SCHEMA';   // v.19.08

  { ����� ����� ������� M_UDATASETS ������������� }
  fldUDataSetID      = 'MUD_ID';
  fldUDataSetTable   = 'MUD_DATASET';
  fldUDataSetSubject = 'MUD_SUBJECTID';
  fldUDataSetType    = 'MUD_TYPE';
  fldUDataSetName    = 'MUD_NAME';
  fldUDataSetXML     = 'MUD_XML';
  fldUDataSetOrder   = 'MUD_ORDER';
  fldUDataSetPublic  = 'MUD_PUBLIC';

  { ����� ����� ������� M_ACTIONS ������������� }
  {
	fldActionName       = 'MAC_NAME';
	fldActionID         = 'MAC_ID';
	fldActionSolution   = 'MAC_SOLUTION';
	fldActionDataSet    = 'MAC_DATASET';
	fldActionIco        = 'MAC_ICO';
	fldActionType       = 'MAC_TYPE';
	fldActionFile       = 'MAC_FILE';
	fldActionProcedure  = 'MAC_PROCEDURE';
	fldActionParam      = 'MAC_PARAM';
	//fldActionParamField = 'MAC_PARAMFIELD'; // ���������������, �.�. ����� �� ������������!
	fldActionQuestion   = 'MAC_QUESTION';
	//fldActionMenu       = 'MAC_MENU';       // ���������������, �.�. ����� �� ������������!
	fldActionEnabled    = 'MAC_ENABLED';
	fldActionVisible    = 'MAC_VISIBLE';
	fldActionComment    = 'MAC_COMMENT';
	fldActionDeleted    = 'MAC_DELETED';
	fldActionShortCut   = 'MAC_SHORTCUT';
	fldActionOrder      = 'MAC_ORDER';
  fldActionCategory   = 'MAC_CATEGORY';   // ��������� ��� Action ("�������", "������" � �.�.); v. 15.9 �����������
  }

  { ����� ����� ������� M_USERS ������������� }
  fldUsersID          = 'MUS_ID';
  fldUsersLogin       = 'MUS_LOGIN';
  fldUsersPassword    = 'MUS_PASSWORD';
  fldUsersDisabled    = 'MUS_DISABLED';
  fldUsersDeleted     = 'MUS_DELETED';
  fldUsersName        = 'MUS_NAME';
  fldUsersType        = 'MUS_TYPE';
  fldUsersAdmin       = 'MUS_ISADMIN';
  fldUsersWorkerID    = 'MUS_WORKERID';

  { ����� ����� ������� M_MEMBERSHIP ������������� }
  fldMSId             = 'MMB_ID';
  fldMSGroupId        = 'MMB_GROUPID';
  fldMSMemberId       = 'MMB_MEMBERID';

  { ����� ����� ������� M_RIGHTS ������������� }
  fldRightsId         = 'MRH_ID';
  fldRightsSubjectId  = 'MRH_SUBJECTID';
  fldRightsObjectType = 'MRH_OBJECTTYPE';
  fldRightsObjectId   = 'MRH_OBJECTID';
  fldRightsOperation  = 'MRH_OPERATION';
  fldRightsPolicy     = 'MRH_POLICY';
  fldRightsRights     = 'MRH_RIGHTS';
  //fldRightsDeleted    = 'MRH_DELETED'; // � v. 17.5 ������!

  { ����� ����� ������� M_TASKS ������������� }
  fldTasksID            = 'MTS_ID';
  fldTasksAuthor        = 'MTS_AUTHOR';
  fldTasksCreateTime    = 'MTS_CREATETIME';
  fldTasksModifyTime    = 'MTS_MODIFYTIME';
  fldTasksPriority      = 'MTS_PRIORITY';
  fldTasksAlertMsg      = 'MTS_ALERTMSG';
  fldTasksDeleted       = 'MTS_DELETED';
  fldTasksTableID       = 'MTS_TABLEID';
  fldTasksStrPrimaryKey = 'MTS_STRPRIMARYKEY';
  fldTasksDeadTime      = 'MTS_DEADLINE';
  fldTasksActualTime    = 'MTS_ACTUALTIME';
  fldTasksConditionID   = 'MTS_CONDITIONID';

  { ����� ����� ������� M_USERTASKS ������������� }
  fldUTID               = 'MUT_ID';
  fldUTUserID           = 'MUT_USERID';
  fldUTAlertTime        = 'MUT_ALERTTIME';
  fldUTState            = 'MUT_STATE';
  fldUTTaskID           = 'MUT_TASKID';
  fldUTModifyTime       = 'MUT_MODIFYTIME';
  fldUTDeleted          = 'MUT_DELETED';
  fldUTComments         = 'MUT_COMMENTS';
  fldUTPeriod           = 'MUT_PERIOD';

  { ����� ����� ������� M_INTERRELATIONS ������������� }
  fldIRParentTable      = 'IDPTABLE';
  fldIRParentKey        = 'PSTRKEY';
  fldIRParentRole       = 'IDPROLE';
  fldIRChildTable       = 'IDCTABLE';
  fldIRChildKey         = 'CSTRKEY';
  fldIRChildRole        = 'IDCROLE';
  fldIRNote             = 'NOTE';

  { ����� ����� ������� M_NOTES ������������� }
  fldNotesTable         = 'IDTABLE';
  fldNotesKey           = 'RECORDKEY';
  fldNotesAuthor        = 'IDAUTHOR';
  fldNotesCreated       = 'CREATEDATE';
  fldNotesNote          = 'NOTE';

  { ����� ����� ������� M_ACTIONCONDITIONS ������������� }
  fldACID               = 'MAC_ID';
  fldACTableMetaID      = 'MAC_TABLEMETAID';
  fldACCondition        = 'MAC_CONDITION';
  fldACProcedureName    = 'MAC_PROCEDURENAME';
  fldACActionName       = 'MAC_ACTIONNAME';
  fldACProcParam        = 'MAC_PROCPARAM';
  fldACDeleted          = 'MAC_DELETED';

  { ����� ����� ������� M_CONSTRAINTS ������������� }
  // ������ ������������ ����� (� v.18.12 �������� ������ ��� ��������� ������ �������������)
  fldCSDataset          = 'ID_DATASET';
  fldCSCases            = 'CASES';
  fldCSAction           = 'ID_ACTION';
  //fldCSStored           = 'STORED';
  fldCSOffState         = 'OFF_STATE';
  fldCSExpression       = 'EXPRESSION';
  fldCSMessage          = 'STR_MESSAGE';
  fldCSErrorField       = 'ID_ERROR_FIELD';
  fldCSDeleted          = 'DELETED';   // �� �������, ���� ��� ���� ������ �� � ������ ������!!!

   // ����� ������������ �����
  fldConstraintsID          = 'MCN_ID';
  fldConstraintsDataset     = 'MCN_DATASET';
  fldConstraintsField       = 'MCN_FIELD';
  fldConstraintsAction      = 'MCN_ACTION';
  fldConstraintsState       = 'MCN_STATE';
  fldConstraintsCases       = 'MCN_CASES';
  fldConstraintsMessage     = 'MCN_MESSAGE';
  fldConstraintsExpression  = 'MCN_EXPRESSION';
  fldConstraintsDeleted     = 'MCN_DELETED';

  { ����� ����� ������� M_ADDITIONAL ������������� }
  {
  fldAddID            =  'MDA_ID';
  fldAddName          =  'MDA_NAME';
  fldAddType          =  'MDA_TYPE';
  fldAddDataset       =  'MFL_DATASET';
  fldAddInfo          =  'MFL_INFO';
  fldAddUser          =  'MDA_USER';
  fldAddDeleted       =  'MDA_DELETED';
  }

  { ����� ����� ������� M_COMMANDS ������������� }
	fldCommandName      = 'MCO_NAME';
	fldCommandID        = 'MCO_ID';
	fldCommandSolution  = 'MCO_SOLUTION';
	fldCommandDataSet   = 'MCO_DATASET';
	fldCommandICO       = 'MCO_ICO';
	fldCommandType      = 'MCO_TYPE';
	fldCommandFile      = 'MCO_FILE';
	fldCommandProcedure = 'MCO_PROCEDURE';
	fldCommandParam     = 'MCO_PARAM';
	fldCommandQuestion  = 'MCO_QUESTION';
	fldCommandBreak     = 'MCO_BREAK';
	fldCommandEnabled   = 'MCO_ENABLED';
	fldCommandVisible   = 'MCO_VISIBLE';
	fldCommandComment   = 'MCO_COMMENT';
	fldCommandDeleted   = 'MCO_DELETED';
 	fldCommandActive    = 'MCO_ACTIVE';
	fldCommandShortCut  = 'MCO_SHORTCUT';
	fldCommandOrder     = 'MCO_ORDER';
  fldCommandCategory  = 'MCO_CATEGORY';
  fldCommandOriginal  = 'MCO_ORIGINAL';
  fldCommandData      = 'MCO_DATA';
  fldCommandBefore    = 'MCO_BEFORE'; // v.16.6
  fldCommandAfter     = 'MCO_AFTER';  // v.16.6
  fldCommandGUID      = 'MCO_GUID';   // v.17.9

  { ����� ����� ������� M_COMMAND_PARAMS ������������� }
  fldCommandParamID      = 'MCP_ID';
  fldCommandParamCommand = 'MCP_COMMAND';
  fldCommandParamDataSet = 'MCP_DATASET'; // ������ ���� ������ �������� MCP_SOURCE
	fldCommandParamName    = 'MCP_NAME';
  fldCommandParamCaption = 'MCP_CAPTION';
  fldCommandParamType    = 'MCP_TYPE';
  fldCommandParamColumn  = 'MCP_COLUMN';
  fldCommandParamValue   = 'MCP_VALUE';
  {
  v.19.01 ������� +
  fldCommandParamSource  = 'MCP_SOURCE'; // ���� �������� � �� ������ ������ ��������������!!! ����� �������!
  v.19.01 ������� -
  }
  fldCommandParamFilter  = 'MCP_FILTER';
  fldCommandParamHidden  = 'MCP_HIDDEN';
  fldCommandParamOrder   = 'MCP_ORDER'; // v.19.04

  // v.20.02 +

  { ����� ����� ������� M_ACCOUNT �������������� ������������� ��� ������ � POP3/SMTP }
  fldAccountID         = 'MMA_ID';
  fldAccountName       = 'MMA_NAME';
  fldAccountSender     = 'MMA_SENDER';
  fldAccountReceiver   = 'MMA_RECEIVER';
  fldAccountContact    = 'MMA_CONTACT';
  fldAccountCompany    = 'MMA_COMPANY';
  fldAccountCreated    = 'MMA_CREATED';
  fldAccountModified   = 'MMA_MODIFIED';
  fldAccountDeleted    = 'MMA_DELETED';

  { ����� ����� ������� M_MESSAGE �������������� ������������� ��� ������ � POP3/SMTP }
  fldMessageID         = 'MMM_ID';
  fldMessageParent     = 'MMM_PARENT';
  fldMessageFolder     = 'MMM_FOLDER';
  fldMessageAccount    = 'MMM_ACCOUNT';
  fldMessageFrom       = 'MMM_FROM';
  fldMessageSubject    = 'MMM_SUBJECT';
  fldMessagePriority   = 'MMM_PRIORITY';
  fldMessageGUID       = 'MMM_GUID';
  fldMessageDate       = 'MMM_DATE';
  fldMessageText       = 'MMM_TEXT';
  fldMessageEML        = 'MMM_EML';
  fldMessageCreated    = 'MMM_CREATED';
  fldMessageModified   = 'MMM_MODIFIED';
  fldMessageDeleted    = 'MMM_DELETED';

  { ����� ����� ������� M_SENDER �������������� ������������� ��� ������ � POP3/SMTP }
  fldSenderID          = 'MMS_ID';
  fldSenderName        = 'MMS_NAME';
  fldSenderHost        = 'MMS_HOST';
  fldSenderPort        = 'MMS_PORT';
  fldSenderLogin       = 'MMS_LOGIN';
  fldSenderPassword    = 'MMS_PASSWORD';
  fldSenderSSL         = 'MMS_SSL';
  fldSenderDelivery    = 'MMS_DELIVERY';
  fldSenderReading     = 'MMS_READING';
  fldSenderSender      = 'MMS_SENDER';
  fldSenderReply       = 'MMS_REPLY';
  fldSenderCreated     = 'MMS_CREATED';
  fldSenderModified    = 'MMS_MODIFIED';
  fldSenderDeleted     = 'MMS_DELETED';

  { ����� ����� ������� M_RECEIVER �������������� ������������� ��� ������ � POP3/SMTP }
  fldReceiverID        = 'MMR_ID';
  fldReceiverName      = 'MMR_NAME';
  fldReceiverHost      = 'MMR_HOST';
  fldReceiverPort      = 'MMR_PORT';
  fldReceiverLogin     = 'MMR_LOGIN';
  fldReceiverPassword  = 'MMR_PASSWORD';
  fldReceiverSSL       = 'MMR_SSL';
  fldReceiverKeep      = 'MMR_KEEP';
  fldReceiverCreated   = 'MMR_CREATED';
  fldReceiverModified  = 'MMR_MODIFIED';
  fldReceiverDeleted   = 'MMR_DELETED';

  { ����� ����� ������� M_RECIPIENT �������������� ������������� ��� ������ � POP3/SMTP }
  fldRecipientID       = 'MMP_ID';
  fldRecipientMessage  = 'MMP_MESSAGE';
  fldRecipientType     = 'MMP_TYPE';
  fldRecipientContact  = 'MMP_CONTACT';
  fldRecipientCreated  = 'MMP_CREATED';
  fldRecipientModified = 'MMP_MODIFIED';

  { ����� ����� ������� M_CONTACT �������������� ������������� ��� ������ � POP3/SMTP }
  fldContactID         = 'MMC_ID';
  fldContactName       = 'MMC_NAME';
  fldContactMail       = 'MMC_MAIL';
  fldContactUser       = 'MMC_USER';
  fldContactCreated    = 'MMC_CREATED';
  fldContactModified   = 'MMC_MODIFIED';
  fldContactDeleted    = 'MMC_DELETED';

  { ����� ����� ������� M_ATTACHMENT �������������� ������������� ��� ������ � POP3/SMTP }
  fldAttachmentID       = 'MMT_ID';
  fldAttachmentMessage  = 'MMT_MESSAGE';
  fldAttachmentFile     = 'MMT_FILE';
  fldAttachmentData     = 'MMT_DATA';
  fldAttachmentCreated  = 'MMT_CREATED';
  fldAttachmentModified = 'MMT_MODIFIED';

  // v.20.02 -

  // v.23.06 +

  { ����� ����� ������� MT_CONTACT �������������� ������������� ��� ������ � Telegram }
  fldTelegramContactID       = 'MTC_ID';
  fldTelegramContactCreated  = 'MTC_CREATED';
  fldTelegramContactModified = 'MTC_MODIFIED';
  fldTelegramContactDeleted  = 'MTC_DELETED';
  fldTelegramContactType     = 'MTC_TYPE';
  fldTelegramContactCode     = 'MTC_CODE';
  fldTelegramContactName     = 'MTC_NAME';
  fldTelegramContactFirst    = 'MTC_FIRST';
  fldTelegramContactLast     = 'MTC_LAST';
  fldTelegramContactToken    = 'MTC_TOKEN';
  fldTelegramContactNote     = 'MTC_NOTE';


  { ����� ����� ������� MT_CHAT �������������� ������������� ��� ������ � Telegram }
  fldTelegramChatID       = 'MTH_ID';
  fldTelegramChatCreated  = 'MTH_CREATED';
  fldTelegramChatModified = 'MTH_MODIFIED';
  fldTelegramChatDeleted  = 'MTH_DELETED';
  fldTelegramChatOwner    = 'MTH_OWNER';
  fldTelegramChatContact  = 'MTH_CONTACT';
  fldTelegramChatNote     = 'MTH_NOTE';

  { ����� ����� ������� MT_UPDATE �������������� ������������� ��� ������ � Telegram }
  fldTelegramUpdateID       = 'MTU_ID';
  fldTelegramUpdateCreated  = 'MTU_CREATED';
  fldTelegramUpdateModified = 'MTU_MODIFIED';
  fldTelegramUpdateDeleted  = 'MTU_DELETED';
  fldTelegramUpdateOwner    = 'MTU_OWNER';
  fldTelegramUpdateCode     = 'MTU_CODE';
  fldTelegramUpdateJSON     = 'MTU_JSON';

  { ����� ����� ������� MT_MESSAGE �������������� ������������� ��� ������ � Telegram }
  fldTelegramMessageID       = 'MTM_ID';
  fldTelegramMessageParent   = 'MTM_PARENT';
  fldTelegramMessageCreated  = 'MTM_CREATED';
  fldTelegramMessageModified = 'MTM_MODIFIED';
  fldTelegramMessageDeleted  = 'MTM_DELETED';
  fldTelegramMessageKind     = 'MTM_KIND';
  fldTelegramMessageOwner    = 'MTM_OWNER';
  fldTelegramMessageChat     = 'MTM_CHAT';
  fldTelegramMessageFrom     = 'MTM_FROM';
  fldTelegramMessageUpdate   = 'MTM_UPDATE';
  fldTelegramMessageDate     = 'MTM_DATE';
  fldTelegramMessageCode     = 'MTM_CODE';
  fldTelegramMessageToDo     = 'MTM_TODO';
  fldTelegramMessageDone     = 'MTM_DONE';
  fldTelegramMessageMessage  = 'MTM_MESSAGE';

  { ����� ����� ������� MT_FILE �������������� ������������� ��� ������ � Telegram }
  fldTelegramFileID         = 'MTF_ID';
  fldTelegramFileCreated    = 'MTF_CREATED';
  fldTelegramFileModified   = 'MTF_MODIFIED';
  fldTelegramFileDeleted    = 'MTF_DELETED';
  fldTelegramFileUpdate     = 'MTF_UPDATE';
  fldTelegramFileMessage    = 'MTF_MESSAGE';
  fldTelegramFileType       = 'MTF_TYPE';
  fldTelegramFileMime       = 'MTF_MIME';
  fldTelegramFileName       = 'MTF_NAME';
  fldTelegramFileSize       = 'MTF_SIZE';
  fldTelegramFileWidth      = 'MTF_WIDTH';
  fldTelegramFileHeight     = 'MTF_HEIGHT';
  fldTelegramFileDuration   = 'MTF_DURATION';
  fldTelegramFileCode       = 'MTF_CODE';
  fldTelegramFileUniqueCode = 'MTF_UNIQUE_CODE';
  fldTelegramFileData       = 'MTF_DATA';

  // v.23.06 -

  // ���������� �����
  fldID               = 'ID';
  fldName             = 'NAME';
  fldDeleted          = 'DELETED';

  // ������ �������������
  MetaDataVersion     = 'MetadataVersion';
  CurrentVersion      = '1.0.003';

  // ���������� ������
  sExtensionLNK = '.lnk';
  sExtensionZIP = '.zip';
  sExtensionTMP = '.tmp';
  sExtensionEMF = '.emf';
  sExtensionWMF = '.wmf';
  sExtensionICO = '.ico';
  sExtensionBMP = '.bmp';
  sExtensionJPG = '.jpg';
  sExtensionJPEG = '.jpeg';
  sExtensionGIF = '.gif';
  sExtensionPNG = '.png';

  sExtensionXML = '.xml';
  sExtensionCHM = '.chm';
  sExtensionPDF = '.pdf';
  sExtensionLNG = '.lng';
  sExtensionXMLX = '.xmlx';

  sExtensionXLS = '.xls';
  sExtensionXLSX = '.xlsx';

  sExtensionRTF = '.rtf';
  sExtensionDOC = '.doc';
  sExtensionDOCX = '.docx';

  sExtensionTXT = '.txt';
  sExtensionCSV = '.csv';
  sExtensionHTM = '.htm';
  sExtensionHTML = '.html';
  sExtensionJSON = '.json';

  {$IFDEF OPENOFFICE}
  sExtensionODT = '.odt'; // Writer
  sExtensionODS = '.ods'; // Calc
  {$ENDIF}

  sExtensionLOG = '.log';
  sExtensionWAV = '.wav';
  sExtensionDAT = '.dat';

  sExtensionEXE = '.exe';
  sExtensionDLL = '.dll';
  sExtensionPAS = '.pas';

  sExtensionMLX = '.mlx'; // v.19.06: Map Layer XML

  sXMLFilter = 'XML Files|*.xml';
  sADXFilter = 'ADO XML Files|*.adx';

const
  // ������� ��������������� ������
  idxSolutions        =  1;
  idxUsers            =  2;
  idxBase             =  3;
  idxDataset          =  4;
  idxFields           =  5;
  idxMenu             =  6;
  idxElement          =  7;
  idxUserDataset      =  8;
  idxParameters       =  9;
  idxRights           = 10;
  idxMembership       = 11;
  idxTasks            = 12;
  idxUserTasks        = 13;
  idxInterRelations   = 14;
  idxNotes            = 15;
  idxActionConditions = 16;
  idxConstraints      = 17;
  idxCommands         = 18; // ������� ������ M_ACTIONS! ����������� � v.15.10
  idxCommandParams    = 19;
  idxLang             = 20;
  idxDictionary       = 21;

  CMetaTablesCount    = 21;

  // ������ ���� ��������������� ������
  MetaTableCheck : array [1 .. 6] of string = (
                      tblUsers,
                      tblBase,
                      tblDataset,
                      tblFields,
                      tblMenu,
                      tblElement
                     );
  MetaTableNames : array [1 .. CMetaTablesCount] of string = (
                      tblSolutions,
                      tblUsers,
                      tblBase,
                      tblDataset,
                      tblFields,
                      tblMenu,
                      tblElement,
                      tblUserDataset,
                      //tblReports,
                      //tblReportParams,
                      tblParameters,
                      tblRights,
                      tblMembership,
                      tblTasks,
                      tblUserTasks,
                      tblInterrelations,
                      tblNotes,
                      tblActionConditions,
                      tblConstraints,
                      //tblUserFields,
                      //tblAction,
                      tblCommand,
                      tblCommandParams,
                      tblLanguage,
                      tblDictonary
                     );

  guidAccount   : TGUID = '{D4B30139-C841-4E77-81B7-CC9AD038F7D9}'; // M_ACCOUNT
  guidMessage   : TGUID = '{8B5C229D-070A-4932-A081-26AD4341BAA1}'; // M_MESSAGE
  guidSender    : TGUID = '{8E575324-FCDF-4A23-8053-D3F5D20FFDD3}'; // M_SENDER
  guidReceiver  : TGUID = '{B01F1857-C246-415B-8B26-484148DFF443}'; // M_RECEIVER
  guidRecipient : TGUID = '{78FE883C-211D-469A-828E-EA5FE197188D}'; // M_RECIPIENT
  guidContact   : TGUID = '{1DBA4FC5-7EF2-4CF5-8B1D-C9C7C2FF2786}'; // M_CONTACT
  guidAttachment: TGUID = '{FEDAB0D5-C1FE-4186-B5FE-3FAAA8F21630}'; // M_ATTACHMENT

  guidTelegramContact: TGUID = '{3F95E25C-4A8A-494D-A54C-D2AFDAEF9E66}'; // MT_CONTACT
  guidTelegramChat   : TGUID = '{DB591153-86B0-4109-A013-728C5750C6FB}'; // MT_CHAT
  guidTelegramUpdate : TGUID = '{88A18F68-E223-4CA6-A875-3CEABC1A3608}'; // MT_UPDATE
  guidTelegramMessage: TGUID = '{D91D47EF-31F7-46D3-9159-8C9E8AC14B0A}'; // MT_MESSAGE
  guidTelegramFile   : TGUID = '{EAE44E47-6265-4EF8-AE32-600DEC7F1911}'; // MT_FILE

  // ��������� ������� ������� Control'��
  MarginLeft   = 8;
  MarginRight  = 8;
  MarginTop    = 7;
  MarginGrid   = 2;
  AnchorsCount = 13;
  CubeSize     = 5;

  // ���������� ����, ������������ ����� ��������
  sgBold      = $0001;
  sgUnderline = $0002;
  sgExtSize   = $0004;
  sgGrayColor = $0008;
  sgRedColor  = $0010;
  sgMonoType  = $0020; // v.19.6: ������������ ��� ������ ��������� ���� �������� � �.�.

  cMonoTypeFontName = 'Courier New'; // v.19.6: ����� ��� ������ ������������� ������

  // ��������� ��� ��������/�������� ������������
  DefaultDataFile  = 'DeData.dat';
  sConfigDescr     = 'Info';
  sConfigName      = 'Name';
  sConfigVersion   = 'Version';
  sConfigData      = 'Data';
  sConfigDeveloper = 'Developer';
  sConfigDatabase  = 'Database';
  sConfigExt       = '.cfg';
  DeThousandSeperator = #0;
  DeDecimalSeparator  = '.';
  DeDateSeparator     = '/';
  DeTimeSeparator     = ':';
  DeShortDateFormat   = 'dd/mm/yyyy';
  DeLongTimeFormat    = 'hh:nn:ss';
  DeQuoteChar         = '"';
  DeDelimiter         = ',';

  cpNone = 0;
  cp866  = 866;
  cpUTF8 = 65001;

  // ��� XML-�����, ����������� ������ ������������ Profite
  ConfigXML = 'Config.xml';

  //LinkPrefix     = 'LINK_';

  // ����������� ��� ���������� ComboBox'��
  DeDelimeter     = '"';

  // ������ ������
  // DataUnit TImages
  icoResult       = 112;         // ��������� �� �������� ���������� ������� !!!! ����� ���������� - ����� ������ ������������ ���������� ���� DM_INFONOTIFY � DM_ERRORNOTIFY!!!
  icoQuery        = 113;         // ������
  icoError        = 114;         // ������
  icoLock         = 038;         // �����
  icoInfo         = 111;         // ��������� (����������)
  icoNext         = 121;         // >
  icoSeparator    = 208;         // |

  // ������ ������� �� ��������� � ������
  MetaSolution    = 1;
  DefaultSolution = 0;

  // ���� ���������
  DM_CLOSESESSION    = WM_USER + 200;
  //DM_ICONNOTIFY      = WM_USER + 199;
  DM_INFONOTIFY2     = WM_USER + 199; // v. 18.3
  DM_INFONOTIFY      = WM_USER + 198;
  DM_ERRORNOTIFY     = WM_USER + 197;
  DM_PROGRESSNOTIFY  = WM_USER + 196;
  DM_DOWNLOADNOTIFY  = WM_USER + 195;
  DM_ACTIONNOTIFY    = WM_USER + 194;
  DM_STATUS2NOTIFY   = WM_USER + 193;
  DM_STATUSNOTIFY    = WM_USER + 192;
  DM_FILTERCHANGE    = WM_USER + 191;
  DM_FAVORITES       = WM_USER + 190;
  DM_EXTSENDMESSAGE  = WM_USER + 189;
  DM_ATTENTION       = WM_USER + 188;
  dm_pnCONDITION     = 1;
  dm_pnCREATE        = 254;
  dm_pnDESTROY       = 255;
  dm_SHOWREMOVED     = 1;
  DM_SHOWARCHIVED    = 2;

  // ���������, ������������ ��� ���������� ��������
  dlgFilters         = '_fIlefilters';
  dlgFilterExcel     = 1;
  dlgFilterIB        = 2;
  dlgFilterConfig    = 3;
  dlgFilterAll       = 4;

  // ������� ����� ��������
  ElementPrefix   = 'element';

  // ��� ���������� � ������ ��������
  varPrinterName = '__PRINTER_NAME';
  // ��� ���������� � ������ ���������� ��������
  varActionOriginal = '__ACTION_ORIGINAL';
  // ��� ���������� � ����� ���������� ��������
  varActionType = '__ACTION_TYPE';
  // ��� ���������� � ��������������� ���������� ��������
  varActionID = '__ACTION_ID';
  // ��� ���������� � ����-����� ���������� ���������� ��������
  varActionTimeout = '__ACTION_TIMEOUT';
  // ��� ���������� � ������ ���������� ������ "������"
  varButtonPrint = '__ACTION_BUTTON_PRINT';
  // ��� ���������� � ������ ���������� ������ "��������"
  varButtonPreview = '__ACTION_BUTTON_PREVIEW';
  // ��� ���������� � ������ ���������� ������ "���������"
  varButtonExport = '__ACTION_BUTTON_EXPORT';
  // ��� ���������� � ����������� �����
  varPrintCopies = 'COPIES';

  // ��� ���������� ����� ����������
  varMailTo = '__MAIL_TO';
  // ��� ���������� ����� ���������� �����
  varMailCopyTo = '__MAIL_COPYTO';
  // ��� ���������� ����� �����������
  varMailFrom = '__MAIL_FROM';
  // ��� ���������� ���� ������
  varMailTitle = '__MAIL_TITLE';
  // ��� ���������� ���������� (�����) ������
  varMailText = '__MAIL_TEXT';
  // ��� ���������� �������� � ������
  varMailFILE = '__MAIL_FILE';

  // ��� ���������� � ������ ����������� � ���������� �������
  varShowResult = 'SHOWRESULT';
  // ��� ���������� � ������ ��������� ����� �������
  varSourceFileName = '__SOURCE_FILENAME';
  // ��� ���������� � ������ �������� �����
  varTargetFileName = '__TARGET_FILENAME';
  // ��� ���������� � ������ ����� � BLOB �����
  varBlobFileName = '__BLOB_FILENAME';
  // ��� ���������� � ������� �� ��������� ��� ���������� ��������
  varActionMode = '__ACTION_MODE';
  // ��� ���������� � ����������� ���������� �������� ��������� ��� ����������� ����������
  varActionResult = '__ACTION_RESULT'; // v. 18.4
  // ��� ���������� � ������� ���� ������������ ��������
  varActionShowWindow = '__ACTION_SHOWWINDOW'; // v. 18.4

  // ��� ���������� � ������ ���������� IP ������ ���������� ���������� � ������������ ������ (������ �������)
  varIP = '__IP';
  // ��� ���������� � ������ ���������� IP ������ ������������� ���������� � ������������ ������ (������������, ������� ����������� �������)
  varUserIP = '__USER_IP';
  // ��� ���������� � ������ ���������� ������������� ������������
  varWindowsUserName = '__WINDOWS_USERNAME';
  // ��� ���������� � ������ ���������� ������������� ����������
  varHostName = '__HOST';
  // ��� ���������� � ������ ���������� ������������� ���������� � ������������ ������
  varUserHostName = '__USER_HOST';

  // ��� ���������� � ��������������� ���������� (HINSTANCE)
  varApplicationInstance = '__INSTANCE';
  // ��� ���������� � ������� ��������� ���� ���������� (WND)
  varMainFormHandle = '__HANDLE';
  // ��� ���������� � ������ �������� �� ��������� � �������
  varDefaultPrinterName = '__DEFAULT_PRINTER_NAME';
  // ��� ���������� � ��������� ����������
  varApplicationDirectory = '__APPLICATION_DIRECTORY'; // v. 19.02
  // ��� ���������� � ������ ����� � ������� ������������ (C:\Documents and Settings\username\Documents)
  varUserDocumentsDirectoryName = '__USER_DOCUMENTS_DIRECTORY'; // v. 19.02
  // ��� ���������� � ������ ����� � ������� ��� ���� ������������� (C:\Documents and Settings\All Users\Documents)
  varAllUsersDocumentsDirectoryName = '__ALL_USERS_DOCUMENTS_DIRECTORY'; // v. 19.02

  // ���� � �������� � ��������� �� ���������
  SolutionsFolder = 'Solutions';

  // ����� ���������� �������
  //RegSerialHDD       = 'serialhdd';
  //RegSerialMB        = 'serialmb';
  RegRemStartup      = 'Remstartup';
  RegRemSoundPath    = 'Remsoundpath';
  RegSoundOn         = 'Soundon';
  RegPort            = 'Port';
  RegEndBits         = 'Port.End';
  RegOdd             = 'Port.Odd';
  RegSpeed           = 'Port.Sped';
  RegStop            = 'Port.Stop';
  RegBit             = 'Port.Bits';
  RegTray            = 'Tray';
  {$IFDEF DBCO_HTTP}
  RegHTTP            = 'HTTP';
  RegPortHTTP        = 'HTTP.Port';
  {$ENDIF}
  RegTime            = 'Time';
  RegAnim            = 'Anim';
  RegDForm           = 'Form';
  RegMenuA           = 'MainMenu.Align';
  RegMenuL           = 'MainMenu.Left';
  RegMenuT           = 'MainMenu.Top';
  RegMenuH           = 'MainMenu.Height';
  RegMenuW           = 'MainMenu.Width';
  RegMenuS           = 'Menus'; // v 16.8. - ��������������� � ����������� ����!
  RegLockSession     = 'Locksession';
  RegLockTime        = 'Locktime';

  RegLogFile         = 'Logfile';
  RegLogQuery        = 'Logquery';

  RegDriverMSSQL     = 'mssqldrv';
  RegImageStoreMode  = 'Imagestoremode';
  RegImageUnpackExt  = 'Imageunpackext';
  RegDragDrop        = 'DragDrop';
  RegDockFormA       = 'DockFormA';

  RegStart           = 'Auto';
  RegCheckUpdates    = 'CheckUpdates';
  RegCopyAllField    = 'CopyAllField';
  RegCopyHeaderField = 'CopyHeaderField';
  RegLastSolution    = 'LastSolution';
  RegLastUpdateMess  = 'LastUpdateMess';
  RegLastUpdateDate  = 'LastUpdateDate';
  RegLastImportPath  = 'LastImportFold';
  RegDirPath         = 'Fold';
  RegDefaultDataPath = 'DataPath';
  RegLang            = 'Lang';
  RegGridBrush       = 'GridBrush';
  RegGridLines       = 'GridLines';
  RegShowShortcuts   = 'ShowShortcuts';
  RegShowCardArea    = 'ShowCardArea';
  RegShowHint        = 'ShowHint';
  RegShowStatus      = 'ShowStatus';
  RegAutoSaveChanges = 'AutoSaveChanges';
  RegToolCard_SmallIcons = 'ToolBars.CardSmallIcons';
  RegToolBars_IcoSize = 'ToolBars.IcoSize';
  RegToolBars_ShowCaptions = 'ToolBars.ShowCaptions';
  RegToolBars_Visible = 'ToolBars.Visible';
  RegToolBars_List = 'ToolBars.List';
  RegToolBars_ShortCuts = 'ToolBars.ShortCuts';
  RegPrintHeight   = 'Print.Height';
  RegPrintWidth    = 'Print.Width';
  RegCommandHeight = 'Command.Height';
  RegCommandWidth  = 'Command.Width';
  RegCommandLastID = 'Command.LastID';
  RegCommandViewStyle = 'Command.ViewStyle';

  RegApplicationTitle = 'Title';
  RegApplicationHomeURL = 'HomeURL';
  RegApplicationMailURL = 'MailURL';
  RegFetchRecord = 'FetchRecord';
  RegHintTime = 'HintTime';
  RegCountInQuery = 'CountInQuery';
  RegConnectionTimeOut = 'ConnectionTimeOut';

  //RegMapTemplateURL = 'Map.TemplateURL';
  //RegMapCenterLatitude  = 'Map.Center.Latitude';
  //RegMapCenterLongitude  = 'Map.Center.Longitude';
  RegMapBrightness = 'Map.Brightness';
  RegMapCacheEnabled = 'Map.Cache.Enabled';
  RegMapBaseDirectory = 'Map.BaseDirectory';

  // ��������� ��� ���������� ConnectionString (��� ADO)
  {$IFDEF SQLNCLI}
  cProviderSQLNCLI = 'SQLNCLI';
  cProviderSQLNCLI9 = cProviderSQLNCLI;
  cProviderSQLNCLI10 = 'SQLNCLI10';
  cProviderSQLNCLI11 = 'SQLNCLI11';
  {$ENDIF}
  cProviderSQLOLEDB = 'SQLOLEDB';
  cProviderMSOLEDBSQL = 'MSOLEDBSQL';

  // ��������� ��� ���������� ���� ToolBar'��
  ToolbarPrefix      = 'MenuBar';
  ToolbarVarHost     = 'Host';
  ToolbarVarPosition = 'Position';
  ToolbarVarVisible  = 'Visible';
  ToolbarVarName     = 'Name';
  ToolbarVarButtons  = 'Buttons';

  // ����� ����������� Toolbar'��
  SystemToolbars : array [0..2] of string = ('_StandartToolBar', '_ViewToolBar', '_AdditionalToolBar');

  // ����������� DEMO-������ �� ���������� �������
  //TrialRecordsBound  = 15;

  EmptyTableMetaID = -1;

  //���������� ������ �������� �������� ��� ActionConditionList
  ACTION_OK             =  0;
  ACTION_FAILED         = -1;
  ACTION_NAME_NOT_FOUND = -2;
  PROCEDURE_NOT_FOUND   = -3;

type
  TListViewStyle =
  (
    lvsReport = 0,
    lvsReportWidth = 1,
    lvsReportHeight = 2,
    lvsColumn = 3,
    lvsColumnHeight = 4,
    lvsList = 5,
    lvsListBig = 6,
    lvsIconSmall = 7,
    lvsIcon = 8,
    lvsIconBig = 9,
    lvsIconLarge = 10,
    lvsPreview64 = 11,
    lvsPreview128 = 12,
    lvsPreview256 = 13,
    lvsPreview384 = 14,
    lvsPreview512 = 15,
    lvsPreviewmax = 16,
    lvsTile = 17,
    lvsTileBig = 18
  );

const
  cListStyleParam = 'ListStyle';
  cListSplitField = 'ListSplitField';
  cListSplitEmpty = 'ListSplitEmpty';
  cListAgregateField = 'ListAgregateField';
  cListAgregateFunc = 'ListAgregateFunc';

  // ���������� ��������� ��������
  lvsHeaderStyles = [lvsReport, lvsReportWidth, lvsReportHeight];
  // ����� ����������� ������
  lvsTilesStyles = [lvsTile, lvsTileBig];
  // ��������� subitems
  lvsSubItemsStyles = lvsHeaderStyles + lvsTilesStyles;
  // ������� ������� (�� ����������� ��������� ��)
  lvsReportStyles = lvsHeaderStyles + lvsTilesStyles + [lvsColumn, lvsColumnHeight];
  // ������ ������� items ��� ��������� ������� �����
  lvsResizeStyles = [lvsReportWidth, lvsReportHeight, lvsColumn, lvsColumnHeight];

  ListViewStyleNames: array[TListViewStyle] of string = ('Report', 'ReportWidth', 'ReportHeight',
                                                         'Column', 'ColumnHeight',
                                                         'List', 'ListBig',
                                                         'Small', 'Icon', 'IconBig', 'IconLarge',
                                                         'Preview64', 'Preview128', 'Preview256', 'Preview384', 'Preview512', 'PreviewMax',
                                                         'Tiles', 'TilesBig');

  ListViewStyleInt: array[TListViewStyle] of Integer =  (LV_VIEW_DETAILS, LV_VIEW_DETAILS, LV_VIEW_DETAILS,
                                                         LV_VIEW_DETAILS, LV_VIEW_DETAILS,
                                                         LV_VIEW_LIST, LV_VIEW_LIST,
                                                         LV_VIEW_TILE, LV_VIEW_TILE, LV_VIEW_ICON, LV_VIEW_ICON,
                                                         LV_VIEW_ICON, LV_VIEW_ICON, LV_VIEW_ICON, LV_VIEW_ICON, LV_VIEW_ICON, LV_VIEW_ICON,
                                                         LV_VIEW_TILE, LV_VIEW_TILE);

  ListViewStyleSize: array[TListViewStyle] of Integer = (16, 16, 32,
                                                         16, 32,
                                                         16, 32,
                                                         16, 32, 48, 64,
                                                         64, 128, 256, 384, 512, -1,
                                                         32, 48);

  ListViewStyleW: array[TListViewStyle] of Integer =    (0, 0, 0,
                                                         0, 0,
                                                         0, 0,
                                                         192, 192, 192, 192,
                                                         0, 0, 0, 0, 0, 0,
                                                         256, 256);

{$IFDEF DEBUG}
function ElementTypeToString(const ElementType: TElementType): string;
{$ENDIF}

function StringToOperationType(const Text: string; const Default: TOperationType = opNone): TOperationType;
function OperationTypeToString(const Operation: TOperationType): string;

implementation

uses DeLog, Funcs;

{$IFDEF DEBUG}
function ElementTypeToString(const ElementType: TElementType): string;
begin
  case ElementType of
    etNone: Result := 'etNone';
    etForm: Result := 'etForm';
    etPanel: Result := 'etPanel';
    etLabel: Result := 'etLabel';
    etBevel: Result := 'etBevel';
    etTabSheet: Result := 'etTabSheet';
    etDefault: Result := 'etDefault';
    etDateTime: Result := 'etDateTime';
    etCheckBox: Result := 'etCheckBox';
    etTime: Result := 'etTime';
    etLinkedCombo: Result := 'etLinkedCombo';
    etExplorer: Result := 'etExplorer';
    etButton: Result := 'etButton';
    etMapObject: Result := 'etMapObject';
    etMapPointObject: Result := 'etMapPointObject';
    etBrowser: Result := 'etBrowser';
    etTreeBox: Result := 'etTreeBox';
    etGrid: Result := 'etGrid';
    etBarCode: Result := 'etBarCode';
    etIconBox: Result := 'etIconBox';
    etCheckListBox: Result := 'etCheckListBox';
    etListTabSheet: Result := 'etListTabSheet';
    etColorComboBox: Result := 'etColorComboBox';
    etTree: Result := 'etTree';
    etFieldsBox: Result := 'etFieldsBox';
    etTablesBox: Result := 'etTablesBox';
  else
    Result := IntToStr(Ord(ElementType));
  end;
end;

function FieldStagesToString(const Stages: TFieldStages): string;
var
  Index: TFieldStage;
begin
  Result := EmptyStr;
  for Index := Low(Index) to High(Index) do
    if Index in Stages then
      begin
        if Length(Result) <> 0 then
          Result := Result + ', ';
        Result := Result + StrPas(FieldStages[Index]);
      end;
  Result := '[' + Result + ']';
end;
{$ENDIF}

function StringToOperationType(const Text: string; const Default: TOperationType): TOperationType;
var
  Index: TOperationType;
  Value, LeftValue, RightValue: string;
  Ready: Boolean;
begin
  Result := Default;
  LeftValue := Trim(Text);
  if Length(LeftValue) <> 0 then
    for Index := Low(OperationTypeNames) to High(OperationTypeNames) do
      begin
        RightValue := Trim(StrPas(OperationTypeNames[Index]));
        if Length(RightValue) <> 0 then
          if Pos(' ', RightValue) = 0 then
            begin
              if SameText(LeftValue, RightValue) then
                begin
                  Result := Index;
                  Break;
                end;
            end
          else
            begin
              Ready := True;
              repeat
                Value := UpperCase(Trim(CutTextValue(RightValue, ' ')));
                if Pos(Value, UpperCase(Trim(CutTextValue(LeftValue, ' ')))) = 0 then
                  begin
                    Ready := False;
                    Break;
                  end;
                LeftValue := Trim(LeftValue);
                RightValue := Trim(RightValue);
              until Length(RightValue) = 0;
              if Ready then
                 begin
                   Result := Index;
                   Break;
                 end
              else
                LeftValue := Trim(Text);
            end;
      end;
end;

function OperationTypeToString(const Operation: TOperationType): string;
begin
  Result := StrPas(OperationTypeNames[Operation]);
end;


{$IFDEF DEBUG}
initialization
  DebugLog('DeTypes unit initialization ...');

finalization
  DebugLog('DeTypes unit finalization ...');
{$ENDIF}

end.

