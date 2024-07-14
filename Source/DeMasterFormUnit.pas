unit DeMasterFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, LogoForm, ExtCtrls, ActnList,
  ComCtrls, ToolWin, StdCtrls, DSMeta, DeMetadata, DeMeta, Buttons, Grids, ExtDlgs, DB, Funcs, ValEdit, Dictionary,
  Menus, DeStdReport, DataCacheUnit, Security, XMLDoc, XMLIntf, CheckLst, ExtCheckLst, DeFilters, DeTypes, Math,
  DePrintStyles, DeReport, DeParser, System.Actions;

const
  otStandart = 1;
  otLast     = 2;
  otSaved    = 3;
  ImportTable = 'ImportTable';
  ImportSheet = 'ImportSheet';
  ImportFile  = 'ImportFile';

type
  TFilterMode = (
    fmAll,          // все записи
    fmCurrent,      // как в текущем наборе данных
    fmTemplate,     // шаблон
    fmStringFilter, // фильтр, заданный строкой
    fmControl       // фильтр-конструктор
    );

  TOperationInfo = class
    Name      : string;
    Name2     : string;
    Ext       : string;
    DatasetID : Integer;
    ProcType  : Integer;
    FileType  : Integer;
    Info      : String;
    Graphic   : TGraphic;
  end;

const

  stdSeparatorName : array[0..4] of string = ('_dL.no',';',',','|','Tab');
  stdSeparator     : array[0..4] of string = (    '><',';',',','|',  #9);
  stdEncode        : array[0..2] of string = ('Windows 1251','UniCode','DOS');

  //список типов файлов для экспорта
  EI_FILETYPE_TEMPLATE = 0;
  EI_FILETYPE_CLIPBOARD = 1;
  EI_FILETYPE_MSEXCEL = 2;
  EI_FILETYPE_MSWORD  = 3;
  EI_FILETYPE_TEXT    = 4;
  EI_FILETYPE_XML     = 5;
  EI_FILETYPES_MAX  = EI_FILETYPE_XML;

  EI_FILENAMES : array [0..EI_FILETYPES_MAX] of string =
                       ('EI_FILETYPE_TEMPLATE', 'EI_FILETYPE_CLIPBOARD', 'EI_FILETYPE_MSEXCEL', 'EI_FILETYPE_MSWORD', 'EI_FILETYPE_TEXT', 'EI_FILETYPE_XML');

  EI_FILETYPES : array [0..EI_FILETYPES_MAX] of string =
                       ( '_dV.template', '_dV.clipboard', dicMSExcel, dicMSWord, '_Dl.datatext', 'XML '+'_df.file');

  EI_FILEEXT : array [0..EI_FILETYPES_MAX] of string =
                       ( '.xls', '', '.xls', '.doc', '.txt', '.xml' );

  // Yes No
  tiYesNo : array[0..1] of string = ('_dL.no','_dL.yes');

  // First Record
//tiFRName    = '_iMp.first';
  tiFRName    = '_iMp.Header';
  tiFR : Array[0..15] of String =
   ('_dL.no','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15');

  // PrimaryKey Exists
  tiPEUpdate  = 0;
  tiPESkip    = 1;
  tiPECreate  = 2;
  tiPRBreak   = 3;
  tiPEName    = '_iMp.pkexist';
  tiPE : Array[0..3] of String =
       ('_iMp.UpdateRecord','_iMp.SkipRecord','_iMp.createrecord','_iMp.Break');

  // ForegnKey Error
  tiFKIgnore  = 0;
  tiFKSkipVal = 1;
  tiFKSkipRec = 2;
  tiFKBreak   = 3;
  tiFKName    = '_iMp.fkerror';
  tiFK : Array[0..3] of String =
                ('_iMp.ignore','_iMp.SkipValue','_iMp.SkipRecord','_iMp.Break');

  // ForegnKeyValidation
  tiFKVNo     = 0;
  tiFKVYes    = 1;
  tiFKVName   = '_iMp.fkcheck';

  // DataType Error
  tiDTSkipVal = 0;
  tiDTSkipRec = 1;
  tiDTBreak   = 2;
  tiDTName    = '_iMp.dterror';
  tiDT : Array[0..2] of String =
                              ('_iMp.SkipValue','_iMp.SkipRecord','_iMp.Break');

  // Empty Field Value
  tiEVIgnore  = 0;
  tiEVSkipVal = 1;
  tiEVName    = '_iMp.empty';
  tiEV : Array[0..1] of String =
       ('_iMp.UpdateRecord','_iMp.SkipValue');

  // Record Condition
  tiRCIgnore  = 0;
  tiRCSkipRec = 1;
  tiRCBreak   = 2;
  tiRCName    = '_iMp.Condition';
  tiRC : Array[0..2] of String =
       ('_iMp.ignore','_iMp.SkipRecord','_iMp.Break');

type

  TMasterOperation = ( moImport, moExport);

  TMasterPanel = class(TObject)
    Panel   : TPanel;
    Prepare : TNotifyEvent;
    Exit    : TNotifyEvent;
  end;

  TDeMaster = class(TFormLogo)
    ActionList1: TActionList;
    DIE_Da_Close: TAction;
    DIE_Da_Back: TAction;
    DIE_Da_Continue: TAction;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    SaveDlg: TSaveDialog;
    PopupMenu1: TPopupMenu;
    MI_dF_OriginalName: TMenuItem;
    MI_dF_Name: TMenuItem;
    N1: TMenuItem;
    MI_Da_View: TMenuItem;
    MI_Da_ClearAll: TMenuItem;
    MI_iMp_header: TMenuItem;
    MI_dL_no: TMenuItem;
    MainPanel: TPanel;
    PageControl: TPageControl;
    TabSheet7: TTabSheet;
    TabSheet2: TTabSheet;
    pnDataset: TPanel;
    DSBox: TListView;
    Panel9: TPanel;
    CB_Dl_HideSystemData: TCheckBox;
    TabSheet1: TTabSheet;
    pnSetFields: TPanel;
    Splitter1: TSplitter;
    PanelFrom: TPanel;
    ListViewFrom: TListView;
    Panel10: TPanel;
    SourceSheet: TLabel;
    cbSheetNum: TComboBox;
    PanelTo: TPanel;
    PCaption: TPanel;
    Panel11: TPanel;
    BDActions: TSpeedButton;
    Panel12: TPanel;
    LCaption: TLabel;
    LTable: TLabel;
    Panel13: TPanel;
    TabSheet3: TTabSheet;
    pnExport: TPanel;
    ExportFileTypeBox: TListBox;
    Panel7: TPanel;
    Panel8: TPanel;
    Confirm: TTabSheet;
    pnConfirm: TPanel;
    TypesPanel: TPanel;
    FinishTab: TTabSheet;
    pnFinish: TPanel;
    ValueList: TValueListEditor;
    ProgressPanel: TPanel;
    ProgressBar: TProgressBar;
    Panel6: TPanel;
    L_Dl_Results: TLabel;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    TabSheet4: TTabSheet;
    pnFieldsSelect: TPanel;
    Panel21: TPanel;
    lb_dA_choosecols: TLabel;
    FieldsBox: TExtCheckListBox;
    pnl2: TPanel;
    btn1: TSpeedButton;
    PopupMenu2: TPopupMenu;
    MI2_Da_ClearAll: TMenuItem;
    N3: TMenuItem;
    MI2_Da_InvertSelection: TMenuItem;
    MI2_dA_selectall: TMenuItem;
    N4: TMenuItem;
    MI2_dA_default: TMenuItem;
    TabSheet5: TTabSheet;
    pnFilter: TPanel;
    Panel19: TPanel;
    Lb_Df_File: TLabel;
    FileNameEdit: TEdit;
    BT_Da_Browse: TButton;
    Lbl_Da_Splitby: TLabel;
    SplitByBox: TComboBox;
    Bevel3: TBevel;
    XMLMemo2: TMemo;
    Splitter2: TSplitter;
    FilterPage: TPageControl;
    Bevel1: TBevel;
    StaticText1: TStaticText;
    ts_dA_all: TTabSheet;
    ts_dA_representation: TTabSheet;
    ts_dF_template: TTabSheet;
    ts_SQL: TTabSheet;
    ts_dF_filter: TTabSheet;
    PatternCheckListBox: TCheckListBox;
    pn_Filter2: TPanel;
    FilterError: TLabel;
    FilterMemo: TMemo;
    pn7: TPanel;
    pn_Menu: TScrollBox;
    Panel18: TPanel;
    Bevel2: TBevel;
    Panel20: TPanel;
    lb_dL_directories: TLabel;
    Panel22: TPanel;
    lB_dL_pNfinish: TLabel;
    ParamList: TValueListEditor;
    DIE_Da_Save: TAction;
    DIE_Da_Preview: TAction;
    DIE_Da_Print: TAction;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    TBPrint: TToolButton;
    Panel23: TPanel;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ToolButton7: TToolButton;
    Panel24: TPanel;
    ToolButton1: TToolButton;
    DIE_Da_Copy: TAction;
    N5: TMenuItem;
    ValueListEditorTo: TValueListEditor;
    pnImport: TPanel;
    Panel15: TPanel;
    FilePanel4: TPanel;
    BT2_Da_Browse: TButton;
    Panel3: TPanel;
    XMLMemo: TMemo;
    panFile: TPanel;
    Lb1_Df_File: TLabel;
    Panel14: TPanel;
    ImportFileTypeBox: TListBox;
    FilesPanel: TPanel;
    L_Df_table: TLabel;
    L_Dt_dataset: TLabel;
    Ldataset: TLabel;
    LDBTable: TLabel;
    Panel25: TPanel;
    L_Df_File: TLabel;
    MFile: TMemo;
    Panel16: TPanel;
    Lb1_Df_Template: TLabel;
    panTemplate: TPanel;
    Lb2_Df_Template: TLabel;
    panText: TPanel;
    Lb1_Dv_Text: TLabel;
    FilePanel2: TPanel;
    Lb2_Df_File: TLabel;
    FileNameEdit2: TEdit;

    procedure SetPage(aPage: TWinControl);
    procedure DIE_Da_CloseExecute(Sender: TObject);
    procedure DIE_Da_BackExecute(Sender: TObject);
    procedure DIE_Da_ContinueExecute(Sender: TObject);
    procedure pnDatasetEnter(Sender: TObject);
    procedure FileTypeUpdate(Sender: TObject);
    procedure ExportFileTypeBoxClick(Sender: TObject);
    procedure ReadComment(aComment: string; ReadDataSet: Boolean = True);
    procedure SetCanNext(Value: Boolean);
    procedure pnConfirmEnter(Sender: TObject);

    function GetMasterType(const aIdent: string; var aType: TFieldType; aDataObject: pointer): Boolean;
    function GetMasterValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;

    procedure onReportProgress(Sender:TObject;aPosition,aMax:integer);
      procedure DoExport(Sender: TObject);
      procedure DoImport(Sender: TObject);
    procedure pnFinishEnter(Sender: TObject);
    procedure BT_Da_BrowseClick(Sender: TObject);
    procedure FileTypeBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure DSBoxChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure CB_Dl_HideSystemDataClick(Sender: TObject);
    procedure ListViewFromUpdate(Sender: TObject);
    function IsFieldConnectionFull: Boolean;
    procedure pnSetFieldsEnter(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure pnSetFieldsResize(Sender: TObject);
    procedure MI_NameClick(Sender: TObject);
    procedure BDActionsClick(Sender: TObject);
    procedure MI_Da_ClearAllClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure pnImportEnter(Sender: TObject);
    procedure BT2_Da_BrowseClick(Sender: TObject);
    procedure FileNameEdit2Change(Sender: TObject);
    procedure cbSheetNumChangeExcel(Sender: TObject);
    procedure cbSheetNumChangeClipBoard(Sender: TObject);
    procedure ParamList_PickValues(index: Integer; DA: Array of String);
    procedure Da_UpdateFrom(Sender: TObject);
    procedure FileNameEditChange(Sender: TObject);
    procedure MI_dL_noClick(Sender: TObject);
    procedure ParamListAddConstraint;
    procedure ParamListSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
    procedure SetTableMeta(aTableMeta: TTableMeta);
    procedure ImportFileTypeBoxClick(Sender: TObject);
    procedure FilterMemoChange(Sender: TObject);
    procedure pnFieldsSelectEnter(Sender: TObject);
    procedure SplitByBoxChange(Sender: TObject);
    procedure MI2_Da_InvertSelectionClick(Sender: TObject);
    procedure MI2_Da_ClearAllClick(Sender: TObject);
    procedure MI2_dA_selectallClick(Sender: TObject);
    procedure MI2_dA_defaultClick(Sender: TObject);
    procedure pnExportEnter(Sender: TObject);
    procedure pnFilterEnter(Sender: TObject);
    procedure WMFilterChange(var Message: TMessage); message DM_FILTERCHANGE;
    procedure FilterPageChange(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure pnExportResize(Sender: TObject);
    procedure pnConfirmExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DIE_Da_SaveExecute(Sender: TObject);
    procedure DIE_Da_PreviewExecute(Sender: TObject);
    procedure DIE_Da_PrintExecute(Sender: TObject);

    function ClipBoardSplit(const aString: String; const aSep: String; var aSL: TStringList): Integer;
    function ClipBoardGetContent(var R: OLEVariant; ColCount, RowCount: integer): Boolean;
    procedure DIE_Da_CopyExecute(Sender: TObject);
    procedure ValueListEditorToDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ValueListEditorToDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ValueListEditorToSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure ImportFileTypeBoxDblClick(Sender: TObject);
    procedure ValueListEditorToDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure XMLMemoChange(Sender: TObject);
  private
    FBackList    : TList;
    FPanelList   : TList;
    FCanContinue : Boolean;
    FProcType    : Integer;
    FPage        : TWinControl;
    FOperation   : TMasterOperation;
    FTableName   : String;
    FTableMeta   : TTableMeta;
    FFileTypeNum : Integer;
    FFileEncode  : Integer;
    FFileSeparator : Integer;
    FSplitBy     : String;
    FFileName    : String;
    FFilePath    : String;
    FFilterMode  : TFilterMode;
    FFilter      : Variant;
    FieldConnection : array of string;
    FieldFileValue : array of TExpressionItem;
    FieldMetaValue : array of TExpressionItem;
    //соответствие столбцов:
    // unassigned  - нет соответствия
    // integer     - номер столбца,
    // string      - расчитывается калькулятором
    // добавляются переменные:
    // __A  указание на столбец
    // __A1 указание на значение из заголовка
    // __R1C1 указание на значение из заголовка
    // __page имя страницы Excel
    // __file имя файла
    aInfo        : array of TOperationInfo;
    FLR,FLR2     : Double;
    FShowNames   : Integer;
    FExcelReport : TDeStdExcelReport;
    FClipboard, FClipboardCaption : TStringList;
    FReportData  : OLEVariant;
    FRowIndex    : integer;

    FiFR         : Integer;
    FiPE         : Integer;
    FiFK         : Integer;
    FiFKV        : Integer;
    FiDT         : Integer;
    FiEV         : Integer;
    FIRC         : Integer;

    vAutoFilter         : string;
    vGuard              : Integer;
    vPasswordEdit       : String;
    vScenarios          : String;
    vFormattingCells    : String;
    vFormattingColumns  : String;
    vFormattingRows     : String;
    vInsertingColumns   : String;
    vInsertingRows      : String;
    vInsertingHyperlinks: String;
    vDeletingColumns    : String;
    vDeletingRows       : String;
    vSorting            : String;
    vFiltering          : String;
    vUsingPivotTables   : String;

    iAutoFilter         : Integer;
    iGuard              : Integer;
    iPasswordEdit       : Integer;
    iScenarios          : Integer;
    iFormattingCells    : Integer;
    iFormattingColumns  : Integer;
    iFormattingRows     : Integer;
    iInsertingColumns   : Integer;
    iInsertingRows      : Integer;
    iInsertingHyperlinks: Integer;
    iDeletingColumns    : Integer;
    iDeletingRows       : Integer;
    iSorting            : Integer;
    iFiltering          : Integer;
    iUsingPivotTables   : Integer;
    iFKValidate         : Integer;

    FProcedure   : String;
    FFilterControl: TDeFilterControl;
    { Private declarations }
    function InnerFieldConnectionDefined(aIndex: Integer): Boolean;
  public
    constructor Create(Sender: TComponent); Override;
    destructor Destroy; Override;
    procedure ReInit(aOperation: TMasterOperation);
    property CanNext:Boolean read FCanContinue write SetCanNext;
    property TableMeta : TTableMeta read FTableMeta write SetTableMeta;
    function ConfigString: string;
    Property FieldConnectionDefined[aIndex: Integer]: Boolean read InnerFieldConnectionDefined;
    { Public declarations }
  end;

var
  DeMaster: TDeMaster;
  _, _Yes, _No : string;

implementation

uses DeSettings, DataUnit, DataManager, DeDB, DeCalculator, DeToolbars, ClipBrd, DeDataset,
     DeScript, QueryDescriptor, DeActions, HintForm;
{$R *.dfm}

function ExcelColumnName(i: Integer) : String;
begin
  Result:=Chr(65+(i mod 26));
  if (i div 26)>0 then Result:= Chr(64+(i div 26))+ Result;
end;

function ExcelColumnIndex(S: String) : Integer;
begin
  case length(s) of
    1: if s[1] in ['A'..'Z','a'..'z']
         then Result:= ORD(UpperCase(s)[1])-Pred(Ord('A'))
         else Result:= -1;
    else Result:= -1;
  end;
end;

function YesNoToString(const aString: String):String;
begin
  if AnsiSameText(Trim(aString),_Yes) then Result:= '1'
                                      else Result:= '0';
end;

function WinToDos(const WinStr: PWideChar): AnsiString;
var tmp2 : PAnsiChar;
begin
  tmp2 := AllocMem(length(WinStr) + 1);
  if CharToOem(WinStr,tmp2) then
    Result := tmp2
  else
    Result := EmptyStr;
  FreeMem(tmp2);
end;

function IsCellName(const aIdent: string; var Col: integer; var Row: Integer): Boolean;
var s: string;
begin
  s:= uppercase(aIdent);
  Col:= 0;
  Row:= 0;

  while (0<length(s)) and (s[1] in ['A'..'Z']) do
    begin
      Col:= Col*26 + ( Ord(s[1])-Pred(Ord('A')) );
      delete(s,1,1);
    end;

  while (0<length(s)) and (s[1] in ['0'..'9']) do
    begin
      Row:= Row*10 + (Ord(s[1]) - Ord('0'));
      delete(s,1,1);
    end;

  Result:= (0=length(s)) and (0<Col) and (Col<26*26);
end;

constructor TDeMaster.Create(Sender: TComponent);
var i:Integer;
    aMasterPanel:TMasterPanel;
begin
  FBackList:=TList.Create;
  FPanelList:=TList.Create;
  FFilterMode := fmAll;
  FFilterControl := nil;

  _   :='    ';
  _Yes:=GetTitle('_dL.yes');
  _No :=GetTitle('_dL.no');

  vAutoFilter          := _Yes;        iAutoFilter  := MaxInt;
  vGuard               := 0;           iGuard       := MaxInt;
  vPasswordEdit        := EmptyStr;    iPasswordEdit:= MaxInt;
  vScenarios           := _Yes;
  vFormattingCells     := _Yes;
  vFormattingColumns   := _Yes;
  vFormattingRows      := _Yes;
  vInsertingColumns    := _No;
  vInsertingRows       := _No;
  vInsertingHyperlinks := _No;
  vDeletingColumns     := _No;
  vDeletingRows        := _No;
  vSorting             := _Yes;
  vFiltering           := _Yes;
  vUsingPivotTables    := _Yes;

  inherited;

  ExportFileTypeBox.OnDrawItem := FileTypeBoxDrawItem;

  for i:=0 to PageControl.PageCount-1 do
    if PageControl.Pages[i].ControlCount=1 then
      if PageControl.Pages[i].Controls[0] is TPanel then
        begin
          aMasterPanel:=TMasterPanel.Create;
          aMasterPanel.Panel   := TPanel(PageControl.Pages[i].Controls[0]);
          aMasterPanel.Prepare := aMasterPanel.Panel.OnEnter;
          aMasterPanel.Exit    := aMasterPanel.Panel.OnExit;
          aMasterPanel.Panel.OnEnter:=nil;
          aMasterPanel.Panel.OnExit :=nil;
          FPanelList.Add(aMasterPanel);
        end;
end;

procedure TDeMaster.ReInit(aOperation: TMasterOperation);
begin
  FBackList.Clear;

  FOperation:=aOperation;
  FiFR:= 1;
  FiPE:= tiPEUpdate;
  FiFK:= tiFKIgnore;
  FiFKV:= 1;
  FiDT:= tiDTSkipVal;
  FiEV:= tiEVIgnore;
  FIRC:= tiRCSkipRec;
  FTableName:= EmptyStr;
  FProcedure:= EmptyStr;

  FCanContinue:=True;
  FLR:= 0.5;
  FLR2:= 0.4;
  FShowNames:= 0;
  FFileTypeNum:= 0;
  FFileName:= EmptyStr;
  FFilter:= EmptyStr;

  FFileSeparator:= 0;
  FFileEncode:= 0;

  case aOperation of
    moImport: begin
                SetPage(pnImport);
                LangFormUpdate(self,'_Da.DataImport');
                FFilePath:= Variables.AsString[RegLastImportPath];
              end;
   else
  {moExport:} begin
                SetPage(pnExport);
                LangFormUpdate(self,'_Da.DataExport');
              end;
  end;

  if Length(FFilePath) = 0 then
    FFilePath:= Variables.AsString[RegDirPath];
end;

destructor TDeMaster.Destroy;
begin
  ExportFileTypeBox.OnDrawItem := nil;
  ImportFileTypeBox.OnDrawItem := nil;

  if Assigned(FExcelReport) then
    begin
      FExcelReport.CloseApp;
      FExcelReport.Free;
      FExcelReport:=nil;
    end;

  inherited;

  FBackList.Free;

  while FPanelList.Count>0 do
    begin
      TMasterPanel(FPanelList.Items[0]).Free;
      FPanelList.Delete(0);
    end;
  FPanelList.Free;
end;

procedure TDeMaster.DIE_Da_BackExecute(Sender: TObject);
begin
//возвращаем крестик
//  DIE_Da_Close.ImageIndex:= 26;

  if FBackList.Count>0 then
  begin
    CanNext:=True;
    SetPage(TPanel(FBackList.Items[FBackList.Count-1]));
  end;
end;

procedure TDeMaster.DIE_Da_CloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TDeMaster.SetCanNext(Value: Boolean);
begin
  if Value=FCanContinue then Exit;

  FCanContinue:=Value;
  DIE_Da_Continue.Enabled:=Value;
end;

//==============================================================================

procedure TDeMaster.SetPage(aPage:TWinControl);
var i:Integer;
    P:TMasterPanel;
begin
  DSBox.OnChange:=nil;

  if (FBackList.Count>0) and (TPanel(FBackList[FBackList.Count-1])=aPage) then
    FBackList.Delete(FBackList.Count-1)
  else
    if Assigned(FPage) then
      FBackList.Add(FPage);

  DIE_Da_Continue.Visible:= FOperation in [moImport, moExport];
  DIE_Da_Continue.Enabled:= CanNext and (aPage<>pnFinish);

  DIE_Da_Back.Visible:= FOperation in [moImport, moExport];
  DIE_Da_Back.Enabled:= (FBackList.Count>0) and (aPage<>pnFinish);

  DIE_Da_Save.Visible:= FOperation in [];
  DIE_Da_Preview.Visible:= FOperation in [];
  DIE_Da_Print.Visible:= FOperation in [];
  DIE_Da_Copy.Visible:= FOperation in [];

  NormalCaption.Caption:=GetTitle('_dL.'+aPage.Name);
  LiteCaption.Caption:=GetTitle('_Dl.Step ')+IntToStr(FBackList.Count+1);
  repaint;

  for i:=0 to FPanelList.Count-1 do
    if aPage=TMasterPanel(FPanelList.Items[i]).Panel then
      begin
        P:=TMasterPanel(FPanelList.Items[i]);
        P.Panel.Align:=alTop;
        P.Panel.Parent := MainPanel;
        P.Panel.Align:=alClient;
        P.Prepare(nil);
        P.Panel.Align:=alClient;
        MainPanel.Realign;
      end;

  for i:=0 to FPanelList.Count-1 do
    if aPage<>TMasterPanel(FPanelList.Items[i]).Panel then
      TMasterPanel(FPanelList.Items[i]).Panel.Parent := PageControl.Pages[0];

  FPage:=aPage;
  repaint;
end;

//==============================================================================

procedure TDeMaster.pnDatasetEnter(Sender: TObject);
var i  : Integer;
    LI : TListItem;
begin
  DSBox.Columns[0].Caption:=GetTitle('_dF.name');
  DSBox.Columns[1].Caption:=GetTitle('_dF.originalname');

  if assigned(DefaultMeta) then
    if Not assigned(TableMeta) then
      TableMeta:=DefaultMeta.Table;

  if Not assigned(TableMeta) and (MetaData.TableCount>0) then
    TableMeta:=MetaData.Tables[0];

  DSBox.OnChange:=nil;
  DSBox.Items.BeginUpdate;
  DSBox.Clear;

  for i:=0 to MetaData.TableCount-1 do
    if Not CB_Dl_HideSystemData.Checked or (MetaData.Tables[i].SolutionID <> MetaSolution) then
    begin
      LI:=DSBox.Items.Add;
      LI.Caption:=GetTitle(MetaData.Tables[i].Name,ttSecondName);
      LI.SubItems.Add(MetaData.Tables[i].Table);
      LI.ImageIndex:=DM.MapIconIndex(MetaData.Tables[i].ICO);
      LI.Data:=MetaData.Tables[i];
    end;

  DSBox.SortType:=stText;
  DSBox.SortType:=ComCtrls.stNone;
  DSBox.Items.EndUpdate;
  DSBox.Columns[0].AutoSize:=True;
  DSBox.Columns[1].AutoSize:=True;
  DSBox.Refresh;

  for i:=0 to DSBox.Items.Count-1 do
    if DSBox.Items[i].Data=TableMeta then
      begin
         DSBox.ItemIndex:=i;
         DSBox.ItemFocused:=DSBox.Items[i];
         DSBox.Selected:=DSBox.ItemFocused;
         Break;
      end;

  DSBox.OnChange:=DSBoxChange;
  if DSBox.ItemIndex>=0 then
    DSBox.Items[DSBox.ItemIndex].MakeVisible(True);

  CanNext:=assigned(TableMeta);
  DSBox.SetFocus;
end;

procedure TDeMaster.CB_Dl_HideSystemDataClick(Sender: TObject);
begin
  inherited;
  pnDatasetEnter(Sender);
end;

procedure TDeMaster.DSBoxChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Assigned(DSBox.ItemFocused) then
    TableMeta:=TTableMeta(DSBox.ItemFocused.Data)
  else
    TableMeta:=nil;

  CanNext:=assigned(TableMeta);
end;

//..............................................................................

procedure TDeMaster.pnExportEnter(Sender: TObject);
 var i,N :Integer;
     TM_UDS, TM: TTableMeta;
     DataCache: TDataCache;
     Filter: TFilterItem;
begin
  N:=0;
  if UserSession.IsAdmin then
    for i:=0 to EI_FILETYPES_MAX do
      begin
      if not (i in [EI_FILETYPE_CLIPBOARD, EI_FILETYPE_TEMPLATE]) then
        begin
          SetLength(aInfo, N+1);
          aInfo[N] :=  TOperationInfo.Create;
          aInfo[N].Name := GetTitle(EI_FILETYPES[i]);
          aInfo[N].ProcType := otStandart;
          aInfo[N].FileType := i;
          aInfo[N].Info := EmptyStr;
          aInfo[N].Graphic := TIcon.Create;
          case i of
            EI_FILETYPE_TEMPLATE:
              DM.ilIcon32.GetIcon( DM.MapIconIndex(2), TIcon(aInfo[N].Graphic) );
            EI_FILETYPE_CLIPBOARD:
              DM.ilIcon32.GetIcon( DM.MapIconIndex(102), TIcon(aInfo[N].Graphic) );
          else
            begin
              TIcon(aInfo[N].Graphic).Handle := GetRegistryIconHandle('abcd' + EI_FILEEXT[aInfo[N].FileType]);
              aInfo[N].Name2:= '*'+EI_FILEEXT[aInfo[N].FileType]
            end;

          end;
          Inc(N);
        end
      end
  else
    begin
      SetLength(aInfo,0);
    end;

  TM_UDS:= MetaData.GetSystemTableByName(tblUserDataSet);
  DataCache := TDataCache.Create(TM_UDS);

  try
    Filter := TFilterItem.Create;
    Filter.AddCondition(TM_UDS.Fields.FindByName(fldUDataSetSubject), opEQ, UserSession.ID);
    Filter.AddCondition(TM_UDS.Fields.FindByName(fldUDataSetSubject), opEQ, 0);
    Filter.AddCondition(TM_UDS.Fields.FindByName(fldUDataSetSubject), opEQ, null);
    Filter.AddOperation(opOr);
    Filter.AddOperation(opOr);
    Filter.AddCondition(TM_UDS.Fields.FindByName(fldUDataSetType), opEQ, uvImport);
    Filter.AddOperation(opAnd);

    DataCache.Filters.AddFilter(Filter);
    DataCache.PrepareData(True, fsFull);

    N:= Length(aInfo);

    for i:=0 to Pred(DataCache.Count) do
      begin
        if (DataCache.Items[i].ValueByName[fldUDataSetSubject]= UserSession.ID) or
           (SecuritySystem.CheckPolicy(TM_UDS.ID, DataCache.Items[i].ValueByName[fldUDataSetID], spExecute)) then
          begin
            SetLength(aInfo,N+1);
            aInfo[N]:=  TOperationInfo.Create;

            aInfo[N].DatasetID:= DataCache.Items[i].ValueByName[fldUDataSetTable];
            aInfo[N].Info     := DataCache.Items[i].ValueByName[fldUDataSetXML];
            aInfo[N].FileType := EI_FILETYPE_MSEXCEL;
            aInfo[N].Graphic  := TIcon.Create;

            if (DataCache.Items[i].ValueByName[fldUDataSetSubject]= UserSession.ID) then
              begin
                aInfo[N].ProcType := otLast;
                aInfo[N].Name := GetTitle('_Dl.lastracurse');
              end
            else
              begin
                aInfo[N].ProcType := otSaved;
                aInfo[N].Name:= DataCache.Items[i].ValueNativeByName[fldUDataSetName]
              end;

            TM:=Metadata.GetTableMeta(aInfo[N].DatasetID);
            if Assigned(TM) then
              begin
                aInfo[N].Name2 := GetTitle(TM.Name);
                DM.ilIcon32.GetIcon( DM.MapIconIndex(TM.Ico), TIcon(aInfo[N].Graphic) )
              end
            else
              begin
                aInfo[N].Name2 := GetTitle('_dV.noname');
                DM.ilIcon32.GetIcon( DM.MapIconIndex(99), TIcon(aInfo[N].Graphic) )
              end;
           Inc(N);
          end;
      end;

    finally
      DataCache.Free;
    end;

  ExportFileTypeBox.Items.Clear;
  for i:=low(aInfo) to high(aInfo) do
    ExportFileTypeBox.Items.Add(aInfo[i].Name);

  if ExportFileTypeBox.items.Count>0 then
     ExportFileTypeBox.ItemIndex:= FFileTypeNum;
  CanNext:=(Length(aInfo)>0);
end;

procedure TDeMaster.pnFilterEnter(Sender: TObject);
var i: Integer;
    s: string;
    LookUpCache: TDataCache;
begin
  if assigned(FFilterControl) then FFilterControl.Destroy;

  FFilterControl:=TDeFilterControl.Create(self);
  FFilterControl.Parent:=pn_Menu;
  FFilterControl.Align:=alTop;
  FFilterControl.Init(TableMeta.Fields);

  SaveDlg.Filter:= EmptyStr;
  for i:=0 to EI_FILETYPES_MAX do
      SaveDlg.Filter:=SaveDlg.Filter+GetTitle(EI_FILETYPES[i])+'|*'+EI_FILEEXT[i]+'|';
  SaveDlg.Filter:=SaveDlg.Filter+'All Files|*.*';

  ts_dA_representation.TabVisible := (TableMeta = DefaultMeta.Table);
  if ts_dA_representation.TabVisible then
      StaticText1.Caption:= DefaultMeta.Filter;

  FilterMemo.Text:=FFilter;

  if (FProcType = otSaved) and (not UserSession.IsAdmin) then
    begin
      ts_dA_representation.TabVisible := False;
      ts_dA_all.TabVisible            := not (FFilterMode = fmTemplate);
      ts_dF_filter.TabVisible         := not (FFilterMode = fmTemplate);
      ts_SQL.TabVisible               := not (FFilterMode = fmTemplate);
      ts_dF_template.TabVisible       :=     (FFilterMode = fmTemplate);
    end;

  case FFilterMode of
    fmAll:            FilterPage.ActivePage := ts_dA_all;
    fmCurrent:        FilterPage.ActivePage := ts_dA_representation;
    fmTemplate:       FilterPage.ActivePage := ts_dF_template;
    fmStringFilter:   FilterPage.ActivePage := ts_SQL
    else {fmControl:} FilterPage.ActivePage := ts_SQL
  end;

  SplitByBox.Items.Clear;
  PatternCheckListBox.Items.Clear;

  if assigned(TableMeta) then
    begin
      SplitByBox.Items.AddObject(' ***', nil);
      for i:=0 to TableMeta.Fields.Count-1 do
        if Assigned(TableMeta.Fields[i].LookupPair) then
          if not (TableMeta.Fields[i].isLookup) then
            begin
              LookUpCache:=MetaData.GetLibrary(TableMeta.Fields[i].LinkTable.ID);
              if Assigned (LookUpCache) then s:= ' - [ ' + IntToStr(LookUpCache.Count) + ' ]'
                                        else s:= EmptyStr;

              SplitByBox.         Items.AddObject(TableMeta.Fields[i].Native,     TObject(i));
              PatternCheckListBox.Items.AddObject(TableMeta.Fields[i].Native + s, TObject(i));
            end;
      SplitByBox.ItemIndex:=0;
    end;
  SplitByBox.Enabled:=(SplitByBox.Items.Count>1);

  FileNameEdit.Text:= IncludeTrailingPathDelimiter(FFilePath)+FFileName;
end;

procedure TDeMaster.FileTypeUpdate(Sender: TObject);
begin
  FFileTypeNum         := ExportFileTypeBox.ItemIndex;
end;

procedure TDeMaster.pnFieldsSelectEnter(Sender: TObject);
var i:Integer;
begin
  FieldsBox.Clear;
  for i:=0 to TableMeta.Fields.Count - 1 do
    begin
      FieldsBox.Items.AddObject(TableMeta.Fields[i].Native+'  [' + TableMeta.Fields[i].Original + ']  ' +
                                FieldTypeNames[TableMeta.Fields[i].DataType], TObject(i));

      FieldsBox.Checked[i]:= FieldConnectionDefined[i];
    end;
end;

procedure TDeMaster.ExportFileTypeBoxClick(Sender: TObject);
var ext: String;
    l  : Integer;
begin
  FileTypeUpdate(Sender);

  ext:=ExtractFileExt(FFileName);
  l:=Length(ext);
  if L > 0 then Delete(FFileName, Length(FFileName) - L + 1, L);

  //не указываем расширение для фалов MS Office - сам добавит .Doc или .Docx
  if FFileTypeNum in [ {EI_FILETYPE_MSEXCEL, EI_FILETYPE_MSWORD,} EI_FILETYPE_TEXT, EI_FILETYPE_XML] then
      FFileName := FFileName+EI_FILEEXT[FFileTypeNum];

  if (aInfo[ExportFileTypeBox.ItemIndex].ProcType=otStandart) or (not UserSession.IsAdmin) then
    XMLMemo2.Text := EmptyStr
  else
    XMLMemo2.Text := aInfo[ExportFileTypeBox.ItemIndex].Info;
end;

procedure TDeMaster.ReadComment(aComment: string; ReadDataSet: Boolean = True);
var XMLDoc   : TXMLDocument;
    Node     : IXMLNode;
    NodeName : String;
    ii,i,j,N,C,R : Integer;
begin
  // Читаем  параметры ракурса
  // XMLMemo.Text := aComment;
  FProcedure   := EmptyStr;
  if ReadDataSet then
    begin
      FTableName   := EmptyStr;
      TableMeta    := nil;
    end;

  if XMLMemo.Lines.Count>1 then XMLMemo.ScrollBars:= ssVertical
                           else XMLMemo.ScrollBars:= ssNone;

  XMLDoc:= TXMLDocument.Create(self);

  try
    XMLDoc.XML.Text:=aComment;
    XMLDoc.Active:=True;
	except

  end;

  if XMLDoc.Active then
    begin
      for ii := 0 to XMLDoc.ChildNodes.Count-1 do
        if AnsiCompareStr(XMLDoc.ChildNodes[ii].NodeName,'IMPORT')=0 then
          for i := 0 to XMLDoc.ChildNodes[ii].ChildNodes.Count-1 do
            begin
              Node:=XMLDoc.ChildNodes[ii].ChildNodes[i];
              NodeName:=Node.NodeName;
              if AnsiCompareStr(NodeName,'FR')=0  then FiFR:=StrToIntDef(Node.Text, 0)  else
              if AnsiCompareStr(NodeName,'PE')=0  then FiPE:=StrToIntDef(Node.Text, 0)  else
              if AnsiCompareStr(NodeName,'FK')=0  then FiFK:=StrToIntDef(Node.Text, 0)  else
              if AnsiCompareStr(NodeName,'FKV')=0 then FiFKV:=StrToIntDef(Node.Text, 0) else
              if AnsiCompareStr(NodeName,'DT')=0  then FiDT:=StrToIntDef(Node.Text, 0)  else
              if AnsiCompareStr(NodeName,'EV')=0  then FiEV:=StrToIntDef(Node.Text, 0)  else
              if AnsiCompareStr(NodeName,'RC')=0  then FiRC:=StrToIntDef(Node.Text, 0)  else
              if AnsiCompareStr(NodeName,'PROCEDURE')=0 then FProcedure:=XMLDecode(Node.Text) else

              if AnsiCompareStr(NodeName,'AUTOFILTER')=0 then vAutoFilter     := GetTitle(tiYesNo[StrToIntDef(Node.Text,0)]) else
              if AnsiCompareStr(NodeName,'GUARD')= 0 then vGuard              := StrToIntDef(Node.Text,0) else
              if AnsiCompareStr(NodeName,'GPE' ) = 0 then vPasswordEdit       := XMLDecode(Node.Text) else
              if AnsiCompareStr(NodeName,'GS'  ) = 0 then vScenarios          := GetTitle(tiYesNo[StrToIntDef(Node.Text,0)]) else
              if AnsiCompareStr(NodeName,'GAFS') = 0 then vFormattingCells    := GetTitle(tiYesNo[StrToIntDef(Node.Text,0)]) else
              if AnsiCompareStr(NodeName,'GAFC') = 0 then vFormattingColumns  := GetTitle(tiYesNo[StrToIntDef(Node.Text,0)]) else
              if AnsiCompareStr(NodeName,'GAFR') = 0 then vFormattingRows     := GetTitle(tiYesNo[StrToIntDef(Node.Text,0)]) else
              if AnsiCompareStr(NodeName,'GAIC') = 0 then vInsertingColumns   := GetTitle(tiYesNo[StrToIntDef(Node.Text,0)]) else
              if AnsiCompareStr(NodeName,'GAIR') = 0 then vInsertingRows      := GetTitle(tiYesNo[StrToIntDef(Node.Text,0)]) else
              if AnsiCompareStr(NodeName,'GAIH') = 0 then vInsertingHyperlinks:= GetTitle(tiYesNo[StrToIntDef(Node.Text,0)]) else
              if AnsiCompareStr(NodeName,'GADC') = 0 then vDeletingColumns    := GetTitle(tiYesNo[StrToIntDef(Node.Text,0)]) else
              if AnsiCompareStr(NodeName,'GADR') = 0 then vDeletingRows       := GetTitle(tiYesNo[StrToIntDef(Node.Text,0)]) else
              if AnsiCompareStr(NodeName,'GAS' ) = 0 then vSorting            := GetTitle(tiYesNo[StrToIntDef(Node.Text,0)]) else
              if AnsiCompareStr(NodeName,'GAF' ) = 0 then vFiltering          := GetTitle(tiYesNo[StrToIntDef(Node.Text,0)]) else
              if AnsiCompareStr(NodeName,'GAUP') = 0 then vUsingPivotTables   := GetTitle(tiYesNo[StrToIntDef(Node.Text,0)]);

              if AnsiCompareStr(NodeName,'FILTERMODE') = 0 then
                begin
                  if AnsiCompareStr(Node.Text,'Template') = 0 then
                    FFilterMode:=fmTemplate;
                end else

              if AnsiCompareStr(NodeName,'FILTER') = 0 then
                begin
                  FFilter:= XMLDecode(Node.Text);
                end else

              if (AnsiCompareStr(NodeName,'DATASET') = 0) then
                begin
                  FTableName:=Node.Text;
                  // если экспортируем - то смотрим на связь ракурса, игнорим название в XML
                  // запоминаем целевой набор данных для дальнейшего импорта
                  if ((TableMeta=nil) or (ReadDataSet)) then
                    begin
                      if Node.HasAttribute('ID') then
                        N:= StrToIntDef(Node.Attributes['ID'],-1)
                      else
                        N:=-1;

                      TableMeta:= Metadata.GetTableByNameAndID(Node.Text, nil, N)
                    end;
                end else

              if (AnsiCompareStr(NodeName,'FIELDS') = 0) and (assigned(TableMeta)) then
                begin
                  for j:=Low(FieldConnection) to High(FieldConnection) do
                    FieldConnection[j]:= EmptyStr;

                  for j := 0 to XMLDoc.ChildNodes[ii].ChildNodes[i].ChildNodes.Count - 1 do
                  begin
                    Node:=XMLDoc.ChildNodes[ii].ChildNodes[i].ChildNodes[j];
                    NodeName:=Node.NodeName;

                    N:= TableMeta.Fields.IndexByName(NodeName);

                    if ( Low(FieldConnection) <= N) and (N <= High(FieldConnection) ) then
                      if Node.HasAttribute('value') then
                        begin
                          FieldConnection[N]:= Node.Attributes['value'];
                          if IsCellName(FieldConnection[N],C,R) then
                            if R = 0 then
                              FieldConnection[N]:= ExcelColumnName(C-1);
                        end
                      else
                      // поддержка старой логики для ЦНТ
                        FieldConnection[N]:= ExcelColumnName(j); //StrToIntDef(Node.Text, j))
                  end;
                end;
            end;
    end
  else
    SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar('XML Error')));

  XMLDoc.Free;
end;

procedure TDeMaster.ImportFileTypeBoxClick(Sender: TObject);
begin
  FFileTypeNum:= ImportFileTypeBox.ItemIndex;
  CanNext:= FileExists(FFileName) or (aInfo[FFileTypeNum].FileType = EI_FILETYPE_CLIPBOARD);

  // Назначаем набор данных
  if aInfo[FFileTypeNum].DatasetID>0 then
    TableMeta:= Metadata.GetTableMeta(aInfo[FFileTypeNum].DatasetID);

  XMLMemo.Text:= aInfo[FFileTypeNum].Info;

  panText.Visible:= (aInfo[FFileTypeNum].FileType in [EI_FILETYPE_CLIPBOARD]);
  panFile.Visible:= (aInfo[FFileTypeNum].FileType in [EI_FILETYPE_TEMPLATE]);
  panTemplate.Visible:= aInfo[FFileTypeNum].ProcType in [otLast, otSaved];

  FilePanel4.Visible:= not (aInfo[FFileTypeNum].FileType in [EI_FILETYPE_CLIPBOARD]);
  FilePanel2.Visible:= not (aInfo[FFileTypeNum].FileType in [EI_FILETYPE_CLIPBOARD, EI_FILETYPE_TEMPLATE]);

  XMLMemo.Visible:= (aInfo[FFileTypeNum].FileType in [EI_FILETYPE_TEMPLATE, EI_FILETYPE_CLIPBOARD]) or
                                         (Length(XMLMemo.Text)>0);
end;


procedure TDeMaster.ImportFileTypeBoxDblClick(Sender: TObject);
begin
  inherited;
  if not (aInfo[FFileTypeNum].FileType = EI_FILETYPE_CLIPBOARD) then
    BT2_Da_BrowseClick(Sender);
end;

procedure TDeMaster.SplitByBoxChange(Sender: TObject);
begin
  if FOperation=moExport then
    begin
      if SplitByBox.ItemIndex>0 then
        FSplitBy := TableMeta.Fields[Integer(SplitByBox.Items.Objects[SplitByBox.ItemIndex])].Original
      else
        FSplitBy := ' ***';
    end;
end;

procedure TDeMaster.FileNameEditChange(Sender: TObject);
begin
  FFilePath:= ExtractFileDir(FileNameEdit.Text);
  FFileName:= ExtractFileName(FileNameEdit.Text)
end;

procedure TDeMaster.BT_Da_BrowseClick(Sender: TObject);
begin
  SaveDlg.InitialDir  := ExtractFileDir(FileNameEdit.Text);
  SaveDlg.FileName    := ExtractFileName(FileNameEdit.Text);
  SaveDlg.FilterIndex := FFileTypeNum+1;
  if SaveDlg.Execute then
    FileNameEdit.Text:=SaveDlg.FileName;
end;

//..............................................................................

function CursorRow(ValueListEditor: TValueListEditor; X,Y: Integer): Integer;
var i: Integer;
    R: TRect;
begin
  Result:=-1;
  for i:=0 to Pred(ValueListEditor.RowCount) do
   begin
     R:= ValueListEditor.CellRect(1, i);
     if (R.Top <= Y) and (Y<= R.Bottom) then Exit(i);
   end;
end;

procedure TDeMaster.ValueListEditorToDragDrop(Sender, Source: TObject; X, Y: Integer);
var FieldNum, ColumnNum: integer;
begin
  inherited;

  if ((Sender as TValueListEditor) = ValueListEditorTo) and assigned(ListViewFrom.Selected) then
    begin
      FieldNum := CursorRow(ValueListEditorTo, X, Y)-1;

      ColumnNum:= ListViewFrom.Selected.Index;
      FieldConnection[FieldNum]:= ExcelColumnName(ColumnNum);

      ValueListEditorTo.Values[TableMeta.Fields[FieldNum].Original]:= FieldConnection[FieldNum];
      ValueListEditorTo.Row:= FieldNum+1;

      ValueListEditorTo.Repaint;
      ValueListEditorTo.SetFocus;
      ValueListEditorTo.EditorMode:= True;
    end;

  CanNext:= IsFieldConnectionFull;
end;

procedure TDeMaster.ValueListEditorToDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  inherited;
  Accept:= (0 <= CursorRow(ValueListEditorTo, X, Y));
  //
end;

procedure TDeMaster.ValueListEditorToDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var Icon: TIcon;
    n,h: Integer;
    s: String;
begin
  if (0 < ARow) then
    begin
      h:= ValueListEditorTo.Canvas.TextHeight('Yy');

      if (0 = ACol) then
        begin
          ValueListEditorTo.Canvas.Pen.Color:= ValueListEditorTo.Color;
          ValueListEditorTo.Canvas.Brush.Color:= ValueListEditorTo.Color;
          ValueListEditorTo.Canvas.FillRect(Rect);

          if FShowNames=0 then S:= TableMeta.Fields[ARow-1].Original
                          else S:= TableMeta.Fields[ARow-1].Native;
          while (ValueListEditorTo.Canvas.TextWidth(S) > Rect.Width - Rect.Height ) and (Length(S)>3) do
            S:= Copy(s, 1, Length(s)-3)+'..';
          ValueListEditorTo.Canvas.TextOut(Rect.Left + ValueListEditorTo.DefaultRowHeight, Rect.Top + (Rect.Height-h) div 2, S);

          if TableMeta.Fields[ARow-1].Key then
            n:= DM.MapIconIndex(38) else
          if assigned(TableMeta.Fields[ARow-1].LinkTable) then
            n:= DM.MapIconIndex(TableMeta.Fields[ARow-1].LinkTable.Ico ) else
            n:= -1;

          if n>-1 then
            begin
              Icon := TIcon.Create;
              DM.ilIcon16.GetIcon(n, Icon);
              ValueListEditorTo.Canvas.Draw((ValueListEditorTo.DefaultRowHeight - 16) div 2,
                                             Rect.Top + (Rect.Bottom - Rect.Top - 16) div 2, Icon);
              FreeAndNil(Icon);
            end;
        end;

      if (1 = ACol)  then
        begin
          ValueListEditorTo.Canvas.Pen.Color:= ValueListEditorTo.FixedColor;
          ValueListEditorTo.Canvas.Brush.Color:= ValueListEditorTo.FixedColor;
          ValueListEditorTo.Canvas.FillRect(Rect);
//        s:= FieldConnection[ARow-1];
          s:= ValueListEditorTo.Values[TableMeta.Fields[ARow-1].Original];
          if SameText(s, TableMeta.Fields[ARow-1].DefaultValue)
            then ValueListEditorTo.Canvas.Font.Color:= clGray
            else ValueListEditorTo.Canvas.Font.Color:= clWindowText;

          while (ValueListEditorTo.Canvas.TextWidth(S) > Rect.Width - 6 ) and (Length(S)>3) do
            S:= Copy(s, 1, Length(s)-3)+'..';
          ValueListEditorTo.Canvas.TextOut(Rect.Left + 3, Rect.Top + (Rect.Height-h) div 2, S);
        end;
    end;

  inherited;
end;

procedure TDeMaster.ValueListEditorToSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  CanSelect:= not TableMeta.Fields[ARow-1].ReadOnly;
  ValueListEditorTo.Invalidate;
end;

procedure TDeMaster.ListViewFromUpdate(Sender: TObject);
var i: Integer;
    slH, slB: TStringList;
begin
  ListViewFrom.Items.Clear;

  slH:= TStringList.Create;
  slB:= TStringList.Create;

  case aInfo[FFileTypeNum].FileType of

    EI_FILETYPE_CLIPBOARD:
      begin
        if FClipBoard.Count=0 then Exit;
        ClipBoardSplit( FClipBoard[0], #9, slH);

        if 0 < FiFR then if FiFR < FClipBoard.Count then ClipBoardSplit( FClipBoard[1], #9, slB)
                                                    else slB.Clear
                    else slB:= slH;

        for i := 0 to Pred(slH.Count) do
          with ListViewFrom.Items.Add do
            begin
              Caption:= ExcelColumnName(i);
              if i<(slB.Count) then SubItems.Add(slH[i]);
              if i<(slB.Count) then SubItems.Add(slB[i]);
            end;
      end;

    EI_FILETYPE_MSEXCEL:
      begin
        FExcelReport.GetRecordFromFile(FiFR, cbSheetNum.ItemIndex+1, slB);

        if 0 < FiFR then FExcelReport.GetRecordFromFile(Pred(FiFR),cbSheetNum.ItemIndex+1, slH)
                    else slH:= slB;

        for i := 0 to Pred(slH.Count) do
          with ListViewFrom.Items.Add do
            begin
              Caption:= ExcelColumnName(i);
              if i<(slB.Count) then SubItems.Add(slH[i]);
              if i<(slB.Count) then SubItems.Add(slB[i]);
            end;
      end;
    end;
end;

procedure TDeMaster.cbSheetNumChangeExcel(Sender: TObject);
var i: Integer;
begin
  ListViewFromUpdate(Sender);

  for i:=Low(FieldConnection) to High(FieldConnection) do
    FieldConnection[i]:= unassigned;

  ValueListEditorTo.Repaint;
end;

procedure TDeMaster.cbSheetNumChangeClipBoard(Sender: TObject);
begin
  inherited;
  FiFR:= cbSheetNum.ItemIndex;
  ListViewFromUpdate(Sender);
end;

function TDeMaster.IsFieldConnectionFull: Boolean;
var i: Integer;
begin
  Result:= False;
  for i:= Low(FieldConnection) to High(FieldConnection) do
    if FieldConnectionDefined[i] then Exit(True);
end;

procedure TDeMaster.pnSetFieldsEnter(Sender: TObject);
var i: Integer;
    MI: TMenuItem;
begin
  CanNext:= IsFieldConnectionFull;

  LCaption.Caption:= GetTitle(TableMeta.Name) + ' ';
  LTable.Caption:= TableMeta.Table;

  cbSheetNum.Items.Clear;

  case aInfo[FFileTypeNum].FileType of

    EI_FILETYPE_CLIPBOARD:
      begin
        SourceSheet.Caption:= GetTitle('_iMp.first');
        for i:=0 to Length(tiFR)-1 do
          begin
            cbSheetNum.Items.Add( GetTitle(tiFR[i]));
            if i=FiFR then
              cbSheetNum.ItemIndex:=i;
          end;
        cbSheetNum.OnChange:= cbSheetNumChangeClipBoard;
      end;

    EI_FILETYPE_MSEXCEL:
      begin
        SourceSheet.Caption:=  GetTitle('_dL.sheet');

        if FExcelReport.SheetCount>0 then
          begin
            for i:=1 to FExcelReport.SheetCount do
              if FExcelReport.SheetVisible(i) then
                cbSheetNum.Items.Add( FExcelReport.SheetName(i));

            if cbSheetNum.Items.Count>0 then cbSheetNum.ItemIndex:=0;
            cbSheetNum.OnChange:= cbSheetNumChangeExcel;
          end;
      end;

  end;

  // ListViewFrom
  ListViewFrom.Columns[1].Caption := GetTitle('_Di.Column');
  ListViewFrom.Columns[2].Caption := GetTitle('_Df.Value');

  ListViewFromUpdate(Sender);

  // ListViewTo
  ValueListEditorTo.TitleCaptions[0]:= GetTitle('_Dt.Field');
  ValueListEditorTo.TitleCaptions[1]:= GetTitle('_Dl.Correspond');
  ValueListEditorTo.Strings.Clear;

  for i:=Low(FieldConnection) to High(FieldConnection) do
    ValueListEditorTo.Values[TableMeta.Fields[i].Original]:= FieldConnection[i];

  Da_UpdateFrom(Sender);
  ValueListEditorTo.Repaint;

  for i:= Pred(MI_imp_header.Count) downto 0 do
    if MI_imp_header[i].Tag > 0 then
      MI_imp_header.Delete(i);

  for I := Succ(Low(tiFR)) to High(tiFR) do
    begin
      MI:= TMenuItem.Create(PopupMenu1);
      MI.RadioItem:= True;
      MI.Caption:= tiFR[i];
      MI.OnClick:= MI_dl_noClick;
      MI.Tag:= i;
      MI_imp_header.Add(MI);
    end;

  inherited;
end;

//..............................................................................

procedure TDeMaster.pnConfirmEnter(Sender: TObject);
var i,N: Integer;
begin
  if aInfo[FFileTypeNum].FileType in [EI_FILETYPE_TEMPLATE] then
    begin
      TypesPanel.Visible:= False;
    end
  else
    begin
      TypesPanel.Visible:= True;
      Ldataset.Caption:= GetTitle(TableMeta.Name,ttSecondName);
      LDBTable.Caption:= TableMeta.Database.Alias+'.'+TableMeta.FullTable;
    end;

  MFile.Text:= FFileName;
  FilesPanel.Height:= 6 + L_Df_File.Height * Min(8, Max(1, MFile.Lines.Count));

  // создаем каждый раз заново,
  // компонент криво ведет себя при очистке списка параметров.
  ParamList.TitleCaptions[0]:=GetTitle('_dT.param');
  ParamList.TitleCaptions[1]:=GetTitle('_dF.value');
  ParamList.Strings.Clear;
  ParamList.Repaint;

 //............................................................................

  if FOperation = moExport then
    begin

      if VarIsNull(FFilter) then
        ParamList.Strings.Add(GetTitle('_Df.Filter=_dA.representation')) else
      if Length(FFilter)=0  then
        ParamList.Strings.Add(GetTitle('_Df.Filter=_eRror.emptyexpr')) else
        ParamList.Strings.Add(GetTitle('_Df.Filter=')+FFilter);

      if SplitByBox.Enabled then
        begin
          N:=ParamList.Strings.Add(GetTitle('_Da.SplitBy')+'='+SplitByBox.Items[SplitByBox.itemIndex]);
          ParamList.ItemProps[N].EditStyle:=esPickList;
          ParamList.ItemProps[N].KeyDesc:=GetTitle('_Da.SplitBy');
          for i:=0 to SplitByBox.Items.Count-1 do
             ParamList.ItemProps[N].PickList.Add(SplitByBox.Items[i]);
        end;

      if FFileTypeNum in [EI_FILETYPE_TEXT, EI_FILETYPE_XML] then
        begin
          N:=ParamList.Strings.Add(GetTitle('_Df.Code')+'='+stdencode[FFileEncode]);
          ParamList.ItemProps[N].EditStyle:=esPickList;
          ParamList.ItemProps[N].KeyDesc:=GetTitle('_Df.Code');
          for i:=0 to Length(stdencode)-1 do
            ParamList.ItemProps[N].PickList.Add(stdencode[i]);
        end;

      if FFileTypeNum = EI_FILETYPE_TEXT then
        begin
          N:=ParamList.Strings.Add(GetTitle('_Dl.Separator')+'='+GetTitle(stdSeparatorName[FFileSeparator]));
          ParamList.ItemProps[N].EditStyle:=esPickList;
          ParamList.ItemProps[N].KeyDesc:=GetTitle('_Dl.Separator');
          for i:=0 to Length(stdSeparator)-1 do
            ParamList.ItemProps[N].PickList.Add(GetTitle(stdSeparatorName[i]));
        end;
    end;

  //............................................................................

//  if FOperation = moImport then
    begin
      N:=ParamList.Strings.Add(GetTitle(tiFRName)+'='+GetTitle(tiFR[FiFR]));
      ParamList.ItemProps[N].EditStyle:=esPickList;
      ParamList.ItemProps[N].KeyDesc:=GetTitle(tiFRName);
      for i:=Low(tiFR) to High(tiFR) do
        ParamList.ItemProps[N].PickList.Add(GetTitle(tiFR[i]));

      N:=ParamList.Strings.Add(GetTitle(tiPEName)+'='+GetTitle(tiPE[FiPE]));
      ParamList.ItemProps[N].EditStyle:=esPickList;
      ParamList.ItemProps[N].KeyDesc:=GetTitle(tiPEName);
      for i:=Low(tiPE) to High(tiPE) do
        ParamList.ItemProps[N].PickList.Add(GetTitle(tiPE[i]));

      N:=ParamList.Strings.Add(GetTitle(tiFKName)+'='+GetTitle(tiFK[FiFK]));
      ParamList.ItemProps[N].EditStyle:=esPickList;
      ParamList.ItemProps[N].KeyDesc:=GetTitle(tiFKName);
      for i:=Low(tiFK) to High(tiFK) do
        ParamList.ItemProps[N].PickList.Add(GetTitle(tiFK[i]));

      N:=ParamList.Strings.Add(GetTitle(tiDTName)+'='+GetTitle(tiDT[FiDT]));
      ParamList.ItemProps[N].EditStyle:=esPickList;
      ParamList.ItemProps[N].KeyDesc:=GetTitle(tiDTName);
      for i:=Low(tiDT) to High(tiDT) do
        ParamList.ItemProps[N].PickList.Add(GetTitle(tiDT[i]));

      N:=ParamList.Strings.Add(GetTitle(tiEVName)+'='+GetTitle(tiEV[FiEV]));
      ParamList.ItemProps[N].EditStyle:=esPickList;
      ParamList.ItemProps[N].KeyDesc:=GetTitle(tiEVName);
      for i:=Low(tiEV) to High(tiEV) do
        ParamList.ItemProps[N].PickList.Add(GetTitle(tiEV[i]));

      N:=ParamList.Strings.Add(GetTitle(tiRCName)+'='+GetTitle(tiRC[FiRC]));
      ParamList.ItemProps[N].EditStyle:=esPickList;
      ParamList.ItemProps[N].KeyDesc:=GetTitle(tiRCName);
      for i:=Low(tiRC) to High(tiRC) do
        ParamList.ItemProps[N].PickList.Add(GetTitle(tiRC[i]));

      N:=ParamList.Strings.Add(GetTitle('_dT.procedure') + '=' + FProcedure);
      ParamList.ItemProps[N].EditStyle:=esSimple;
      ParamList.ItemProps[N].KeyDesc:=GetTitle('_dT.procedure');

      if FiFKV = 1 then iFKValidate:=ParamList.Strings.Add(GetTitle(tiFKVName)+'='+_Yes)
                   else iFKValidate:=ParamList.Strings.Add(GetTitle(tiFKVName)+'='+_No);
      ParamList_PickValues(iFKValidate, tiYesNo);
    end;

  if (FOperation = moExport) and (FFileTypeNum = EI_FILETYPE_MSEXCEL) then
    begin
      iAutoFilter:=ParamList.Strings.Add(GetTitle('_dL.AutoFilter')+'='+vAutoFilter);
      ParamList_PickValues(iAutoFilter, tiYesNo);

      if vGuard = 1 then iGuard:=ParamList.Strings.Add(GetTitle('_dL.Guard')+'='+_Yes)
                    else iGuard:=ParamList.Strings.Add(GetTitle('_dL.Guard')+'='+_No);
      ParamList_PickValues(iGuard, tiYesNo);
    end;

  ParamListAddConstraint;

  ParamList.Options:=ParamList.Options-[goRowSelect]+[goEditing];
  ParamList.OnSetEditText := ParamListSetEditText;

  if FOperation = moExport then
    if FProcType = otSaved then
      if not UserSession.IsAdmin then
        begin
          ParamList.Options:=ParamList.Options+[goRowSelect]-[goEditing];
          ParamList.OnSetEditText:=nil;
        end;
end;

procedure TDeMaster.pnConfirmExit(Sender: TObject);
begin
  inherited;
  ParamList.OnSetEditText:=nil;
end;

procedure TDeMaster.ParamListAddConstraint;
begin
  if (vGuard= 1) then
    begin
      if (Length(vPasswordEdit) > 0) and (not UserSession.IsAdmin) and (FProcType = otSaved) then
        begin
          iPasswordEdit:=ParamList.Strings.Add(_ + GetTitle('_df.password') + '=******');
          ParamList.ItemProps[iPasswordEdit].EditStyle:=esSimple;
        end
      else
        begin
          iPasswordEdit:=ParamList.Strings.Add(_ + GetTitle('_df.password') + '=' + vPasswordEdit);
          ParamList.ItemProps[iPasswordEdit].EditStyle:=esSimple;
        end;

      iScenarios:=ParamList.Strings.Add(_+GetTitle('_dl.msoScenarios')+'='+vScenarios);
      ParamList_PickValues(iScenarios, tiYesNo);

      iFormattingCells:=ParamList.Strings.Add(_+GetTitle('_dl.msoAllowFormattingCells')+'='+vFormattingCells);
      ParamList_PickValues(iFormattingCells, tiYesNo);

      iFormattingColumns:=ParamList.Strings.Add(_+GetTitle('_dl.msoAllowFormattingColumns')+'='+vFormattingColumns);
      ParamList_PickValues(iFormattingColumns, tiYesNo);

      iFormattingRows:=ParamList.Strings.Add(_+GetTitle('_dl.msoAllowFormattingRows')+'='+vFormattingRows);
      ParamList_PickValues(iFormattingRows, tiYesNo);

      iInsertingColumns:=ParamList.Strings.Add(_+GetTitle('_dl.msoAllowInsertingColumns')+'='+vInsertingColumns);
      ParamList_PickValues(iInsertingColumns, tiYesNo);

      iInsertingRows:=ParamList.Strings.Add(_+GetTitle('_dl.msoAllowInsertingRows')+'='+vInsertingRows);
      ParamList_PickValues(iInsertingRows, tiYesNo);

      iInsertingHyperlinks:=ParamList.Strings.Add(_+GetTitle('_dl.msoAllowInsertingHyperlinks')+'='+vInsertingHyperlinks);
      ParamList_PickValues(iInsertingHyperlinks, tiYesNo);

      iDeletingColumns:=ParamList.Strings.Add(_+GetTitle('_dl.msoAllowDeletingColumns')+'='+vDeletingColumns);
      ParamList_PickValues(iDeletingColumns, tiYesNo);

      iDeletingRows:=ParamList.Strings.Add(_+GetTitle('_dl.msoAllowDeletingRows')+'='+vDeletingRows);
      ParamList_PickValues(iDeletingRows, tiYesNo);

      iSorting:=ParamList.Strings.Add(_+GetTitle('_dl.msoAllowSorting')+'='+vSorting);
      ParamList_PickValues(iSorting, tiYesNo);

      iFiltering:=ParamList.Strings.Add(_+GetTitle('_dl.msoAllowFiltering')+'='+vFiltering);
      ParamList_PickValues(iFiltering, tiYesNo);

      iUsingPivotTables:=ParamList.Strings.Add(_+GetTitle('_dl.msoAllowUsingPivotTables')+'='+vUsingPivotTables);
      ParamList_PickValues(iUsingPivotTables, tiYesNo);
    end
  else
    begin
      iPasswordEdit:=MaxInt;
      iScenarios:=MaxInt;
      iFormattingCells:=MaxInt;
      iFormattingColumns:=MaxInt;
      iFormattingRows:=MaxInt;
      iInsertingColumns:=MaxInt;
      iInsertingRows:=MaxInt;
      iInsertingHyperlinks:=MaxInt;
      iDeletingColumns:=MaxInt;
      iDeletingRows:=MaxInt;
      iSorting:=MaxInt;
      iFiltering:=MaxInt;
      iUsingPivotTables:=MaxInt;
    end;
end;

procedure TDeMaster.ParamListSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
var i,N: Integer;
begin
  inherited;
  if not assigned(ParamList.ItemProps[ARow-1]) then Exit;

  if  ParamList.ItemProps[ARow-1].KeyDesc=GetTitle(tiFRName) then
    begin
      for i:=0 to Length(tiFR)-1 do
        if Value = GetTitle(tiFR[i]) then
          begin FiFR:=i; break; end;
    end;

  if  ParamList.ItemProps[ARow-1].KeyDesc=GetTitle(tiPEName) then
    begin
      for i:=0 to Length(tiPE)-1 do
        if Value = GetTitle(tiPE[i]) then
          begin FiPE:=i; break; end;
    end;

  if  ParamList.ItemProps[ARow-1].KeyDesc=GetTitle(tiFKName) then
    begin
      for i:=0 to Length(tiFK)-1 do
        if Value = GetTitle(tiFK[i]) then
          begin FiFK:=i; break; end;
    end;

  if  ParamList.ItemProps[ARow-1].KeyDesc=GetTitle(tiDTName) then
    begin
      for i:=0 to Length(tiDT)-1 do
        if Value = GetTitle(tiDT[i]) then
          begin FiDT:=i; break; end;
    end;

  if  ParamList.ItemProps[ARow-1].KeyDesc=GetTitle(tiEVName) then
    begin
      for i:=0 to Length(tiEV)-1 do
        if Value = GetTitle(tiEV[i]) then
          begin FiEV:=i; break; end;
    end;

  if  ParamList.ItemProps[ARow-1].KeyDesc=GetTitle(tiRCName) then
    begin
      for i:=0 to Length(tiRC)-1 do
        if Value = GetTitle(tiRC[i]) then
          begin FiRC:=i; break; end;
    end;

  if  ParamList.ItemProps[ARow-1].KeyDesc=GetTitle('_Dl.Separator') then
    begin
      for i:=0 to Length(stdSeparatorName)-1 do
        if Value = GetTitle(stdSeparatorName[i]) then
          begin FFileSeparator:=i; break; end;
    end;

  if  ParamList.ItemProps[ARow-1].KeyDesc=GetTitle('_Da.SplitBy') then
    begin
      FSplitBy := EmptyStr;

      for i:=0 to TableMeta.Fields.Count-1 do
        if Assigned(TableMeta.Fields[i].LookupPair) then
          if not (TableMeta.Fields[i].isLookup) then
            if AnsiCompareText(Value, TableMeta.Fields[i].Native) = 0 then
              begin
                FSplitBy := TableMeta.Fields[i].Original;
                Break;
              end;
    end;

  if  ParamList.ItemProps[ARow-1].KeyDesc=GetTitle('_Df.Code') then
    begin
      for i:=0 to Length(stdencode)-1 do
        if Value = stdencode[i] then
          begin FFileEncode:=i; break; end;
    end;

  if  ParamList.ItemProps[ARow-1].KeyDesc=GetTitle('_dT.procedure') then FProcedure:= Value;

  if (ARow-1 = iAutoFilter)         then vAutoFilter:= Value;
  if (ARow-1 = iPasswordEdit)       then vPasswordEdit:= Value;
  if (ARow-1 = iScenarios)          then vScenarios:= Value;
  if (ARow-1 = iFormattingCells)    then vFormattingCells:= Value;
  if (ARow-1 = iFormattingColumns)  then vFormattingColumns:= Value;
  if (ARow-1 = iFormattingRows)     then vFormattingRows:= Value;
  if (ARow-1 = iInsertingColumns)   then vInsertingColumns:= Value;
  if (ARow-1 = iInsertingRows)      then vInsertingRows:= Value;
  if (ARow-1 = iInsertingHyperlinks)then vInsertingHyperlinks:= Value;
  if (ARow-1 = iDeletingColumns)    then vDeletingColumns:= Value;
  if (ARow-1 = iDeletingRows)       then vDeletingRows:= Value;
  if (ARow-1 = iSorting)            then vSorting:= Value;
  if (ARow-1 = iFiltering)          then vFiltering:= Value;
  if (ARow-1 = iUsingPivotTables)   then vUsingPivotTables:= Value;
  if (ARow-1 = iFKValidate)         then begin if AnsiCompareText(Value,_Yes)=0 then FiFKV:=1 else FiFKV:=0; end;
  if (ARow-1 = iGuard) then
    begin
      if AnsiCompareText(Value,_Yes)=0 then N:=1 else N:=0;
      if not (vGuard = N) then
        begin
          vGuard := N;

          ParamList.OnSetEditText := nil;
          for i:=ParamList.RowCount-1 downto ARow+1 do
            ParamList.DeleteRow(i);

          ParamListAddConstraint;

          ParamList.Update;
          ParamList.OnSetEditText := ParamListSetEditText;
        end;  
    end;
end;

//..............................................................................

procedure TDeMaster.onReportProgress(Sender:TObject;aPosition,aMax:integer);
begin
  if aMax<0 then
    lB_dL_pNfinish.Caption := GetTitle(PChar(aPosition));

  if aMax>0 then
    ProgressBar.Position:=((100*aPosition) div aMax);
end;

procedure TDeMaster.DoExport(Sender: TObject);
var i,j,k,C,N,L : integer;
    DSM,GDSM : TDatasetMeta;
    s,fn : string;
    BackupCursor : TCursor;
    FM : TFieldMeta;
    CI : TCacheItem;
    LibCache: TDataCache;

function ExportLength(FM: TFieldMeta): Integer;
var _L,_N: Integer;
begin
  if FM.DataType in StringTypes then
    begin _L:=FM.DataSize+1; if FM.CodePage = cpUTF8 then _L:=(_L div 2); end else
  if FM.DataType in [ftSmallint, ftInteger, ftWord, ftAutoInc]     then _L:=5+1+1 else
  if FM.DataType in [ftLargeint] then _L:=10+1+1 else
  if FM.DataType in [ftFloat, ftCurrency]   then _L:=18+1 else
  if FM.DataType in [ftBCD]                 then _L:=18+1+FM.DataSize+1 else
  if FM.DataType in [ftTime]                then _L:=12+1 else
  if FM.DataType in [ftDate]                then _L:=10+1 else
  if FM.DataType in [ftDateTime]            then _L:=22+1 else
  if FM.DataType in [ftBoolean]             then _L:= 5+1 else
  if FM.DataType in [ftGuid]                then _L:=36+1 else
    begin
      _L:=36;
      WriteLog('Export error: unknown length of field ' + QuotedStr(FM.Original), True, 'Errors');
    end;

  _N:=Length(FM.Original)+2;

  if _L>_N then Result:=_L
           else Result:=_N;
end;

procedure InnerDoExport(aDSM: TDatasetMeta; aFileName: string);
var i, j, FixLength: Integer;
    f: TextFile;
    ExcelRep : TDeStdExcelReport;
    WordRep : TDeStdWordReport;
    tmps, tmps2, separator : string;
begin
   //...........................................................................

   case aInfo[FFileTypeNum].FileType of
    EI_FILETYPE_MSEXCEL:
      try
          ExcelRep := TDeStdExcelReport.Create(aDSM);
          ExcelRep.Dinamic:=True;

          for i:=0 to aDSM.Cache.Fields.Count-1 do
            if FieldConnectionDefined[i] then
              ExcelRep.FieldsList.Add(aDSM.Cache.Fields[i]);

          ExcelRep.Comment2 := ConfigString;
          {$IFDEF DeDEBUG}
          WriteLog('start export');
          {$ENDIF}
          ExcelRep.SaveToFile(IncludeTrailingPathDelimiter(FFilePath) + aFileName);
          {$IFDEF DeDEBUG}
          WriteLog('finish export');
          {$ENDIF}
          ExcelRep.Free;
      finally
      end;

    EI_FILETYPE_MSWORD:
      try
          WordRep := TDeStdWordReport.Create(aDSM);
          WordRep.SaveToFile(IncludeTrailingPathDelimiter(FFilePath) + aFileName);
          WordRep.Free;
      finally
      end;

    EI_FILETYPE_TEXT:
      begin
        AssignFile(f, IncludeTrailingPathDelimiter(FFilePath) + aFileName);
      {$I-}
        Rewrite(f);
      {$I+}
        if IOResult <> 0 then
          begin
            MessageBox(Handle,PChar(GetTitle('_dE.openfile')),
                     PChar(GetTitle('_dE.error')),MB_OK or MB_USERICON or MB_APPLMODAL);
            Exit;
          end;

        s := EmptyStr;
        separator := stdSeparator[FFileSeparator];
        //набиваем заголовки полей
        for j:=0 to aDSM.Table.Fields.Count-1 do
          begin
            tmps2 := aDSM.Table.Fields[j].Original;
            case FFileEncode of
              1: tmps:=WideStringToUnicode(tmps2);
              2: tmps:=WinToDos(PChar(tmps2));
              else tmps := tmps2;
            end;

            if (separator = stdSeparator[0]) then
              begin
                FixLength:=ExportLength(aDSM.Table.Fields[j]);
                while (Length(tmps)+2 < FixLength) do tmps:=tmps+' ';
                s := s + '<'+tmps+'>';
              end
            else
              begin
                tmps:=AnsiQuotedStr(tmps,'"');
                if (j<>aDSM.Table.Fields.Count-1) then s := s + tmps + separator
                                                  else s := s + tmps;
              end;
          end;
        writeln(f, s);

        //набиваем данные
        for i := 0 to aDSM.Cache.Count-1 do
          begin
            s := EmptyStr;
            for j:=0 to aDSM.Table.Fields.Count-1 do
              begin
                tmps2 := VarToStr(aDSM.Cache[I].FieldText(J));
                case FFileEncode of
                  1: tmps:=WideStringToUnicode(tmps2);
                  2: tmps:=WinToDos(PChar(tmps2));
                  else tmps := tmps2;
                end;

                if (separator = stdSeparator[0]) then
                  begin
                    FixLength:=ExportLength(aDSM.Table.Fields[j]);
                    while (Length(tmps) < FixLength) do tmps:=tmps+' ';
                    s := s + tmps;
                  end
                else
                  begin
                    tmps:=AnsiQuotedStr(tmps,'"');
                    if (j<>aDSM.Table.Fields.Count-1) then s := s + tmps + separator
                                                      else s := s + tmps;
                  end;
              end;
            writeln(f, s);
            ProgressBar.Position := i*100 div aDSM.Cache.Count;
          end;
        CloseFile(f);
      end;

    EI_FILETYPE_XML:
      begin
        AssignFile(f, IncludeTrailingPathDelimiter(FFilePath) + aFileName);
      {$I-}
        Rewrite(f);
      {$I+}
        if IOResult <> 0 then
          begin
            MessageBox(Handle,PChar(GetTitle('_dE.openfile')),
                     PChar(GetTitle('_dE.error')),MB_OK or MB_USERICON or MB_APPLMODAL);
            Exit;
          end;

        writeln(f, aDSM.Cache.GetAsXML([dfStoredFields, dfLookupFields, dfCalculatedFields, dfAllRecords]));
        CloseFile(f);
      end;
    end;
end;

begin
  BackupCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  DSM := TDataSetMeta.Create(utTemporary, TableMeta);
  ProgressBar.Position:=10;

  ValueList.Strings.Clear;
  ValueList.Strings.Add(getTitle('_Dl.OutputFiles')+'='+FFilePath);

  FM := TableMeta.Fields.FindByName(FSplitBy);
  if not Assigned(FM) then
    begin
      try
        ValueList.Strings.Add(getTitle('_Df.File')+'='+NormalizeFileName(FFileName));

        if FFilterMode = fmTemplate then
          begin
            DSM.Cache.Clear;
            for i:=PatternCheckListBox.Count-1 downto 0 do
              if PatternCheckListBox.Checked[i] then
                begin
                  N:=Integer(PatternCheckListBox.Items.Objects[i]);
                  LibCache:=MetaData.GetLibrary(TableMeta.Fields[N].LinkTable.ID);

                  if assigned(TableMeta.Fields[N].LookupPair) then
                    L := TableMeta.Fields.IndexByID(TableMeta.Fields[N].LookupPair.ID)
                  else
                    L := -1;

                  if DSM.Cache.Count=0 then
                    // первый множитель
                    begin
                      for j:=0 to LibCache.Count - 1 do
                        begin
                          CI:=DSM.Cache.AddNewItem;
                          CI.FieldValue[N]:= LibCache.Items[j].ID;
                          if 0 <= L then
                            CI.FieldValue[L]:= LibCache.Items[j].Caption;
                        end
                    end
                  else
                    // не первый множитель
                    begin
                      C:= DSM.Cache.Count;

                      for k:= 0 to LibCache.Count - 1 do
                        for j:= 0 to C - 1 do
                          begin
                            if k = 0 then
                              begin
                                CI := DSM.Cache[j];
                              end
                            else
                              begin
                                CI := DSM.Cache.AddNewItem;
                                CI.Assign(DSM.Cache[j]);
                              end;

                            CI.FieldValue[N]:= LibCache.Items[k].ID;

                            if 0 <= L then
                              CI.FieldValue[L]:= LibCache.Items[k].Caption;
                          end;
                    end;
                end;
            InnerDoExport(DSM, NormalizeFileName(FFileName));
          end
        else
          begin
            if (TableMeta = DefaultMeta.Table) and VarIsNull(FFilter) then
              DSM.FilterPostfix.Assign(DefaultMeta.FilterPostfix)
            else
              DSM.Filter:= FFilter;

            if (TableMeta = DefaultMeta.Table) then
              begin
                if (FFilterMode = fmCurrent) and (0 < DefaultMeta.UserFilterPostfix.Count) then
                  begin
                    DSM.FilterPostfix.CopyFrom(DefaultMeta.UserFilterPostfix);
                    if (DefaultMeta.UserFilterPostfix.Count < DSM.FilterPostfix.Count) then
                      DSM.FilterPostfix.AddOperation(opAnd);
                  end;

                DSM.Cache.SortList.Assign(DefaultMeta.Cache.SortList);
              end;

            ProgressBar.Position:=0;
            DSM.Cache.PrepareData(True, fsFull);
            InnerDoExport(DSM, NormalizeFileName(FFileName));
          end;
      except
      end
    end
  else
    begin
      GDSM := TDataSetMeta.Create(utTemporary, FM.LinkTable );
      GDSM.Cache.PrepareData;
      try
        for i:=0 to GDSM.Cache.Count-1 do
        begin
          DSM.Cache.CloseData;
          DSM.Filter:= EmptyStr;
          DSM.FilterPostfix.Clear;

          if (TableMeta = DefaultMeta.Table) and VarIsNull(FFilter) then
            DSM.FilterPostfix.Assign(DefaultMeta.FilterPostfix)
          else
            DSM.Filter:= FFilter;

          if (TableMeta = DefaultMeta.Table) then
            begin
              if (FFilterMode = fmCurrent) and (0 < DefaultMeta.UserFilterPostfix.Count) then
                begin
                  DSM.FilterPostfix.CopyFrom(DefaultMeta.UserFilterPostfix);
                  if (DefaultMeta.UserFilterPostfix.Count < DSM.FilterPostfix.Count) then
                    DSM.FilterPostfix.AddOperation(opAnd);
                end;

              DSM.Cache.SortList.Assign(DefaultMeta.Cache.SortList);
            end;

          DSM.FilterPostfix.AddCondition(FM, opEQ, GDSM.Cache.Items[i].ID);
          if 3 < DSM.FilterPostfix.Count then
            DSM.FilterPostfix.AddOperation(opAnd);

          DSM.Cache.PrepareData(True, fsFull);

          ProgressBar.Position:=10 + (80 * i) div (GDSM.Cache.Count - 1);
          fn:= FFileName+' ('+VarToStr(GDSM.Cache.Items[i].ID)+' - '+GDSM.Cache.Items[i].Caption+')';
          if DSM.Cache.Count>0 then
            InnerDoExport(DSM, NormalizeFileName(fn, True));
        end;
      except
      end;
      GDSM.Free;
    end;

  DSM.Free;
  Screen.Cursor := BackupCursor;
end;

function TDeMaster.ClipBoardSplit(const aString: String; const aSep: String; var aSL: TStringList): Integer;
var s: String;
    P: Integer;
begin
  try
    aSL.Clear;

    s:= aString;
    P:= Pos(aSep, s);
    while (0 < P) and (0 < Length(s)) do
      begin
        aSL.Add(Copy(s, 1, P-1));
        s:= Copy(s, P+Length(aSep), MaxInt);
        P:= Pos(aSep, s);
      end;

    if 0<Length(s) then
      begin
        aSL.Add(s);
      end;
  finally
    Result:= aSL.Count;
  end;
end;

function TDeMaster.ConfigString: string;
  var
    i, n: Integer;
    sFilter, sFields, sGuard: string;

  function NodeText(const aName, aValue: string) : String;
  begin
    if Length(Trim(aValue))>0 then Result:='<' +  aName + '>' + XMLEncode(Trim(aValue)) + '</' +  aName + '>' + #13#10
                              else Result:= EmptyStr;
  end;
begin
  Result:= EmptyStr;

  if FFilterMode= fmTemplate then
   sFilter:= NodeText('FILTERMODE','Template') else
  if Length(FFilter)> 0 then
   sFilter:= NodeText('FILTER',FFilter) else
  if VarIsNull(FFilter) then
   sFilter:= NodeText('FILTER', TableMeta.Filter)
  else
   sFilter:= EmptyStr;

  if vGuard = 1 then
    sGuard:= NodeText('GUARD','1') +
             NodeText('GPE',  vPasswordEdit) +
             NodeText('GS',   YesNoToString(vScenarios)) +
             NodeText('GAFS', YesNoToString(vFormattingCells)) +
             NodeText('GAFC', YesNoToString(vFormattingColumns)) +
             NodeText('GAFR', YesNoToString(vFormattingRows)) +
             NodeText('GAIC', YesNoToString(vInsertingColumns)) +
             NodeText('GAIR', YesNoToString(vInsertingRows)) +
             NodeText('GAIH', YesNoToString(vInsertingHyperlinks)) +
             NodeText('GADC', YesNoToString(vDeletingColumns)) +
             NodeText('GADR', YesNoToString(vDeletingRows)) +
             NodeText('GAS',  YesNoToString(vSorting)) +
             NodeText('GAF',  YesNoToString(vFiltering)) +
             NodeText('GAUP', YesNoToString(vUsingPivotTables))
   else
    sGuard:= EmptyStr;

  sFields:= EmptyStr;
  n:= 0;
  for i:= Low(FieldConnection) to high(FieldConnection) do
    if FieldConnectionDefined[i] then
      begin
        if VarIsType(FieldConnection[i], varInteger) then
          sFields:= sFields+ Format('  <%s column="%d"/>'+#13#10, [TableMeta.Fields[i].Original, n]) else
        if VarIsType(FieldConnection[i], [varString, varOleStr, varUString]) then
          sFields:= sFields+ Format('  <%s value="%s"/>'+#13#10, [TableMeta.Fields[i].Original, FieldConnection[i]]);
        inc(n);
      end;

  Result := xmlHeaderStr+#13#10+
        '<IMPORT>'+#13#10+
          NodeText('TYPE', 'EI_FILETYPE_MSEXCEL') +
          NodeText('DATABASE', TableMeta.Database.Alias) +
          Format('<DATASET ID="%d">%s</DATASET>',[Integer(TableMeta.ID), FTableName]) + #13#10 +
          sFilter +
          '<FIELDS>' + #13#10 +
             sFields +
          '</FIELDS>' + #13#10 +
          NodeText('AUTOFILTER', YesNoToString(vAutoFilter)) +
          sGuard+

          NodeText('FR',IntToStr(FiFR)) +
          NodeText('PE',IntToStr(FiPE)) +
          NodeText('FK',IntToStr(FiFK)) +
          NodeText('FKV',IntToStr(FiFKV))+
          NodeText('DT',IntToStr(FiDT)) +
          NodeText('EV',IntToStr(FiEV)) +
          NodeText('RC',IntToStr(FiRC)) +
          NodeText('PROCEDURE',FProcedure) +
        '</IMPORT>';
end;

function TDeMaster.ClipBoardGetContent(var R: OLEVariant; ColCount, RowCount: integer): Boolean;
var i,j : Integer;
    sl: TStringList;
begin
  SL:= TStringList.Create;
  Result:= False;
  R := VarArrayCreate([ 0, RowCount-1, 0, ColCount-1], varOleStr);
  for i:=0 to RowCount-1 do
    begin
      ClipBoardSplit( FClipBoard[i], #9, sl);
      for j:=0 to sl.Count-1 do
         R[i,j]:= sl[j];
    end;
end;

function TDeMaster.GetMasterType(const aIdent: string; var aType: TFieldType; aDataObject: pointer): Boolean;
var c, r: Integer;
begin
  aType:= ftString;
  Result:= SameText(aIdent, ImportTable) or
           SameText(aIdent, ImportSheet) or
           SameText(aIdent, ImportFile) or
           IsCellName(aIdent, c, r);
end;

function TDeMaster.GetMasterValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
var c, r: Integer;
begin
  if SameText(aIdent, ImportTable) then
    begin
      aValue:= FTableName;
      Exit(True);
    end;

  if SameText(aIdent, ImportSheet) then
    begin
     // aValue:= FSheetName;
      Exit(true);
    end;

  if SameText(aIdent, ImportFile) then
    begin
      aValue:= FFileName;
      Exit(true);
    end;

  if IsCellName(aIdent, c, r) then
    begin
      try
        if (0 < r) then aValue:= FReportData[r, c]
                   else aValue:= FReportData[FRowIndex, c];
        Exit(True);
      except
        Exit(False);
      end;
    end else

    Result:= False;
end;

procedure TDeMaster.DoImport(Sender: TObject);
var ColCount, RowCount, i, j, k, l, N, NextStart, RowFirst, KeyFieldIndex : integer;
    iAddCount, iUpdateCount, iSkipCount, iEmptyCount: integer;
    vAddCount, vUpdateCount, vSkipCount, vEmptyCount, vAllCount : integer;
    InsertMax: Integer;
    s: String;
    V: Variant;
    FlagBreak, FlagUpdate: Boolean;

    DSM : TDatasetMeta;
    TF: TFieldMeta;
    FT : TFieldType;
    ImportCache, LookUpCache: TDataCache;
    LookUpCaches : Array of TDataCache;
    IndexInFile : Array of Integer;
    InsertQuery: TDeDataset;

    BackupCursor : TCursor;
    CI, ResultItem : TCacheItem;

    Parser: TDeParser;
    Calculator: TDeCalculator;
    Postfix: TExpressionItem;

procedure IncCount(var aCounter: Integer; aValue: Integer = 1);
begin
  if aCounter = iAddCount then
    begin Inc(vAddCount, aValue); ValueList.Strings[aCounter]:= GetTitle('_iMp.resultadd='+IntToStr(vAddCount)); end else

  if aCounter = iUpdateCount then
    begin Inc(vUpdateCount, aValue); ValueList.Strings[aCounter]:= GetTitle('_iMp.resultupdate='+IntToStr(vUpdateCount)); end else

  if aCounter = iSkipCount then
    begin Inc(vSkipCount, aValue); ValueList.Strings[aCounter]:= GetTitle('_iMp.resultskip='+IntToStr(vSkipCount)); end else

  if aCounter = iEmptyCount then
    begin Inc(vEmptyCount, aValue); ValueList.Strings[aCounter]:= GetTitle('_iMp.resultempty='+IntToStr(vEmptyCount)); end;

  ValueList.Repaint;
end;

begin
  if not Assigned(TableMeta) then Exit;

  if Not (SecuritySystem.CheckPolicyDataSet(TableMeta.ID, spSelect) and
          SecuritySystem.CheckPolicyDataSet(TableMeta.ID, spUpdate)) then
  begin
    ShowHintWindow(nil, '_eRror.right', '_dE.error', icoError, mtError);
    Exit;
  end;

  BackupCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  Parser:= TDeParser.Create;
  Parser.Table:= TableMeta;
  Parser.onGetIdentType:= GetMasterType;

  // инициализируем массив флагов ошибок, массивы правил расчета значений
  FlagBreak:= False;
  FlagUpdate:= False;
  KeyFieldIndex:= -1;

  SetLength(LookUpCaches, Length(FieldConnection));
  SetLength(IndexInFile, Length(FieldConnection));

  for i:=low(FieldConnection) to high(FieldConnection) do
    begin
      // массив правил и значений из файла импорта
      IndexInFile[i]:= -1;
      FieldFileValue[i]:= TExpressionItem.Create;
    end;

  ImportCache:= TDataCache.Create(TableMeta);

  // парсим выражения
  // проверяем права на поля
  // определяем загрузку ключевого поля
  for i:=low(FieldConnection) to high(FieldConnection) do
    try
      // заполняем правила расчета импорта
      if (TableMeta.Fields[i].IsLookup) then TF:= TableMeta.Fields[i].LookupPair
                                        else TF:= TableMeta.Fields[i];

      if (TF.IsStored) and (Length(FieldConnection[i])>0) then
        begin
          Parser.Parse(FieldConnection[i], FieldFileValue[i]);

          if (FieldFileValue[i].IsEmpty) then
            begin
              SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(FieldConnection[i])))
            end
          else
            begin
              if TF.Key then
                begin
                  KeyFieldIndex:= i; // запоминаем ключевое поле
                  IndexInFile[i]:= TableMeta.Fields.IndexByID(TF.ID);;
                end else

              if {not (TF.IsReadOnly) and} SecuritySystem.CheckPolicyField(TF.ID, sdUpdate) then
                begin
                  IndexInFile[i]:= TableMeta.Fields.IndexByID(TF.ID);
                end else

                begin
                  SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25,
                                  NativeInt(PChar('_dT.field ' + TableMeta.Fields[i].Original + #10 + '_dE.readonly')));
                end;
            end;
        end;
    except
      on E:EDeParserError do
      begin
        SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(GetTitle(E.Message)+#10+FieldConnection[i])));
      end;
    end;

  // расчитываем массив правил и значений из метаструктуры по умолчанию, пропуская вычисляемые нехранимые..
  for i:=low(FieldConnection) to high(FieldConnection) do
    begin
      FieldMetaValue[i]:= TExpressionItem.Create;

      if (TableMeta.Fields[i].IsLookup) then TF:= TableMeta.Fields[i].LookupPair
                                        else TF:= TableMeta.Fields[i];

      if (0 < Length(TableMeta.Fields[i].DefaultValue)) and (TF.IsStored) then
        if SecuritySystem.CheckPolicyField(TF.ID, sdUpdate) then
          try
            Parser.Parse(TableMeta.Fields[i].DefaultValue, FieldMetaValue[i]);
            if Not (FieldMetaValue[i].IsEmpty) then
              IndexInFile[i]:= TableMeta.Fields.IndexByID(TF.ID);;
          except
            FieldMetaValue[i].Clear;
          end;
    end;

  // готовим LookUp для ссылочных полей со ЗНАЧЕНИЕМ
  for i:= low(FieldConnection) to high(FieldConnection) do
    if TableMeta.Fields[i].IsLookup and (not FieldFileValue[i].IsEmpty or not FieldMetaValue[i].IsEmpty)
      then LookUpCaches[i]:= MetaData.GetLibrary(TableMeta.Fields[i].LinkTable.ID);

  // копируем содержимое файла импорта в память
  case aInfo[FFileTypeNum].FileType of
    EI_FILETYPE_TEMPLATE: FExcelReport.GetContent(FReportData, ColCount, RowCount);
    EI_FILETYPE_MSEXCEL: FExcelReport.GetContent(FReportData, ColCount, RowCount);
    EI_FILETYPE_CLIPBOARD: begin
                             ColCount:= FClipboardCaption.Count;
                             RowCount:= FClipBoard.Count;
                             ClipBoardGetContent(FReportData, ColCount, RowCount);
                           end;
  end;

  RowFirst:= 1 + FiFR;
  vAllCount:= RowCount+1-RowFirst;
  ValueList.Strings.Add(GetTitle('_Quantity='+IntToStr(vAllCount)));
  iAddCount:= ValueList.Strings.Add(GetTitle('_iMp.resultadd=0'));
  iUpdateCount:= ValueList.Strings.Add(GetTitle('_iMp.resultupdate=0'));
  iSkipCount:= ValueList.Strings.Add(GetTitle('_iMp.resultskip=0'));
  iEmptyCount:= ValueList.Strings.Add(GetTitle('_iMp.resultempty=0'));
  ValueList.Repaint;

  vAddCount     := 0;
  vUpdateCount  := 0;
  vSkipCount    := 0;
  vEmptyCount   := 0;

  Calculator:= TDeCalculator.Create;
  Calculator.OnGetIdentValue:= GetMasterValue;

  //далее работаем с R, наполняя кэш ===========================================

  for i:=RowFirst to RowCount do
    begin
      FRowIndex:= i;
      ResultItem:= ImportCache.AddNewItem;
      ResultItem.FlagNull:= True;

      for k:= High(FieldConnection) downto Low(FieldConnection) do
        { Здесь учтены и уставливаются значения по умолчанию }

        if ((not FieldFileValue[k].IsEmpty) and (-1 < IndexInFile[k])) then
        begin
          v:= Calculator.Calculate(FieldFileValue[k]);

          if VarIsType(v, [varString, varOleStr, varUString]) then
            if Length(Trim(v))=0 then v:= varEmpty;

          if VarIsEmpty(V) and (FiEV = tiEVSkipVal) then
            Continue;

          if not VarIsEmpty(V) and not VarIsNull(V) then
            begin
              if TableMeta.Fields[k].IsLookup then
                begin
                  N:= -1;
                  V:= VarAsType(V, FieldTypeVarMap[TableMeta.Fields[k].DataType]);
                  // Для строковых полей сравниваем без учета регистра
                  if LookUpCaches[k].TableMeta.Fields[LookUpCaches[k].CaptionIndex].DataType in StringTypes then
                    begin
                      for l:= 0 to LookUpCaches[k].Count-1 do
                    //if (not (isDeleted in LookUpCache.Items[k].State)) then   // допускается совпадение с удаленными записями
                        if AnsiSameText(LookUpCaches[k].Items[l].Caption, V) then
                          begin
                            N:= l;
                            break;
                          end
                    end
                  else
                    begin
                      N:= LookUpCaches[k].IndexByValues([LookUpCaches[k].CaptionIndex],[V]);
                    end;

                  if -1 < N then
                    begin
                      ResultItem.FieldNativeValue[IndexInFile[k]]:= LookUpCaches[k].Items[N].ID;
                      ResultItem.FlagNull:= False;
                    end
                  else
                    case FiFK of
                      tiFKIgnore, tiFKSkipVal:
                        begin
                        //ValueList.Strings.Add(IntToStr(i+FiFR+1)+':'+GetTitle(tiFKName)+'='+TableMeta.Fields[k].Native+': '+VarToStr(V));
                        end;
                      tiFKSkipRec:
                        begin
                          ResultItem.FlagSkip:= True;
                          ValueList.Strings.Add(IntToStr(i+FiFR+1)+':'+GetTitle(tiFKName)+'='+TableMeta.Fields[k].Native+': '+VarToStr(V));
                          ValueList.Repaint;
                        end;
                      tiFKBreak:
                        begin
                          FlagBreak:= True;
                          ValueList.Strings.Add(IntToStr(i+FiFR+1)+':'+GetTitle(tiFKName)+'='+TableMeta.Fields[k].Native+': '+VarToStr(V));
                          ValueList.Repaint;
                        end;
                    end;
                end
              else
                begin
                  try
                    V := VarAsType(V, FieldTypeVarMap[TableMeta.Fields[k].DataType]);
                  except
                    case FiDT of
                      tiDTSkipVal: begin
                                     v:= UnAssigned;
                                   //ValueList.Strings.Add(IntToStr(i+FiFR+1)+':'+GetTitle(tiDTName)+'='+TableMeta.Fields[k].Native+': '+VarToStr(V));
                                   end;
                      tiDTSkipRec: begin
                                     ValueList.Strings.Add(IntToStr(i+FiFR+1)+':'+GetTitle(tiDTName)+'='+TableMeta.Fields[k].Native+': '+VarToStr(V));
                                     ValueList.Repaint;
                                     ResultItem.FlagSkip:= True;
                                   end;
                      tiDTBreak:   begin
                                     ValueList.Strings.Add(IntToStr(i+FiFR+1)+':'+GetTitle(tiDTName)+'='+TableMeta.Fields[k].Native+': '+VarToStr(V));
                                     ValueList.Repaint;
                                     FlagBreak:= True;
                                   end;
                    end;
                  end;

                  ResultItem.FieldNativeValue[IndexInFile[k]]:= v;
                  ResultItem.FlagNull:= False;
                end;
            end;
        end;

      // проверяем пустоту записи и уставливаются значения по умолчанию
      if not (ResultItem.FlagNull and (FiEV = tiEVSkipVal)) then
        for k:= High(FieldConnection) downto Low(FieldConnection) do
          if (not FieldMetaValue[k].IsEmpty) then
            if VarIsEmpty(ResultItem.FieldNativeValue[k]) then
              ResultItem.FieldNativeValue[k]:= ResultItem.Calculate(FieldMetaValue[k], null);
         //     ResultItem.FieldNativeValue[k]:= Calculator.Calculate(FieldMetaValue[k]);
    end;

  FReportData := unAssigned;

  // проверяем условия
  for i:= 0 to Pred(ImportCache.Count) do
    begin
      ImportCache.DataManager.Errors.Clear;
      if Not ImportCache.DataManager.CheckRecord(ImportCache.CacheItems[i], [ccInsert, ccUpdate]) then
        case FiRC of
          tiRCSkipRec:  begin
                          ValueList.Strings.Add(IntToStr(i+FiFR+1)+':'+GetTitle(tiRCName)+'='+ImportCache.DataManager.Errors.GetMessage);
                          ValueList.Repaint;
                          ImportCache.CacheItems[i].FlagSkip:= True;
                         end;
          tiRCBreak:    begin
                          ValueList.Strings.Add(IntToStr(i+FiFR+1)+':'+GetTitle(tiRCName)+'='+ImportCache.DataManager.Errors.GetMessage);
                          ValueList.Repaint;
                          FlagBreak:=True;
                          Break;
                        end;
        end;
    end;

  //........................................................................

  ProgressBar.Position:= 10;

  // При обнаружении первичного ключа меняем флаг
  if (-1 < KeyFieldIndex) then
    begin
      LookUpCache:= MetaData.GetLibrary(TableMeta.ID);
      if 0 < LookUpCache.Count then
        for i:=0 to Pred(ImportCache.Count) do
          if Not ImportCache.CacheItems[i].FlagSkip then
            begin
              V:= ImportCache[i].ID;
              if not VarIsNull(V) and not VarIsEmpty(V) then
                if assigned(LookUpCache.FindById(V)) then
                  case FiPE of
                    tiPEUpdate: begin
                                  ImportCache.CacheItems[i].FlagEdit:= True;
                                  FlagUpdate:= True;
                                end;
                    tiPESkip: ImportCache.CacheItems[i].FlagSkip:= True;
                    tiPRBreak: FlagBreak:= True;
                  end;
              ProgressBar.Position:= 10 + Round(10 * i / vAllCount);
            end;
    end;

  ProgressBar.Position:= 20;

  // удаляем пустые записи
  if Not FlagBreak then
    for i:= Pred(ImportCache.Count) downto 0 do
      if (ImportCache.CacheItems[i].FlagNull and (FiEV = tiEVSkipVal)) then
        begin
          ImportCache.CacheItems.Delete(i);
          IncCount(iEmptyCount);
          ProgressBar.Position:= 20 + Round(70 * (vAddCount + vUpdateCount + vSkipCount + vEmptyCount) / vAllCount);
        end;

  // удаляем лишние записи
  if Not FlagBreak then
    for i:= Pred(ImportCache.Count) downto 0 do
      if (ImportCache.CacheItems[i].FlagSkip) then
        begin
          ImportCache.CacheItems.Delete(i);
          IncCount(iSkipCount);
          ProgressBar.Position:= 20 + Round(70 * (vAddCount + vUpdateCount + vSkipCount + vEmptyCount) / vAllCount);
        end;

  //........................................................................

  NextStart:= 0;
  if FlagBreak then
    begin
      IncCount(iSkipCount, ImportCache.Count);
    end
  else
    begin
      for i:= 0 to Metadata.LibraryCount-1 do
        if Metadata.LibraryData[i].TableMeta = TableMeta then
           Metadata.LibraryData[i].BeginUpdate;

      // Штучный Update
      if FlagUpdate then
        begin
          DSM:= TDataSetMeta.Create(utTemporary, TableMeta);
          for j:= 0 to Length(FieldConnection)-1 do
            if FieldConnectionDefined[j] then
              DSM.Cache.Fields[j].Stage:= fsKey;

          DSM.Cache.PrepareData;
          DSM.Cache.BeginUpdate;

          for i:=Pred(ImportCache.Count) downto 0 do
            if ImportCache.Items[i].FlagEdit then
              begin
                CI:= DSM.Cache.FindById(ImportCache.Items[i].ID);

                if assigned(CI) then
                  if FiPE=tiPEUpdate then
                    try
                      for j:= low(FieldConnection) to High(FieldConnection) do
                        if (-1 < IndexInFile[j]) and (not FieldMetaValue[j].IsEmpty) then
                          begin
                            V:= ImportCache.Items[i].FieldNativeValue[IndexInFile[j]];
                            if not VarIsEmpty(V) or (FiEV = tiEVSkipVal) then
                              CI.FieldValue[IndexInFile[j]]:= V;
                          end;

                      if DSM.Cache.UpdateRecord(CI) then IncCount(iUpdateCount)
                                                    else IncCount(iSkipCount);
                    except
                      IncCount(iSkipCount);
                      ValueList.Strings.Add(IntToStr(i+FiFR+1)+':'+GetTitle('_dE.error')+' '+DSM.Cache.DataManager.Errors.GetMessage);
                      ValueList.Repaint;
                    end;
                ImportCache.CacheItems.Delete(i);
              end;

          DSM.Cache.EndUpdate;
          DSM.Destroy;
          ProgressBar.Position:= 20 + Round(70 * (vAddCount + vUpdateCount + vSkipCount + vEmptyCount) / vAllCount);
        end;

      InsertMax:= 0;
      for i:=0 to Pred(TableMeta.Fields.Count) do
        if (-1 < IndexInFile[i]) or (not FieldMetaValue[i].IsEmpty)then
          if not (TableMeta.Fields[i].DataType in IntegerTypes) then Inc(InsertMax);

      InsertMax:= Min(255 div Max(1,InsertMax), Variables.AsInteger[RegCountInQuery]);
      InsertMax:= Max(InsertMax, 1);
      {$IFDEF DEBUG}
      Writelog('InsertMaxRes '+IntToStr(InsertMax));
      {$ENDIF}

      // массовый insert
      for i:=0 to Pred(ImportCache.Count) do
        if (i = Pred(ImportCache.Count)) or (i - NextStart = InsertMax) then
          begin
            InsertQuery := TableMeta.Database.CreateQuery(qtInsert);
            try
              ImportCache.ClearSelection;
              for k:=NextStart to i do
                ImportCache.Items[k].Selected:= True;

              InsertQuery.Descr.Clear;
              InsertQuery.Descr.Table := TableMeta.Table;
              InsertQuery.Descr.Records:= ImportCache.CacheSelectedItems;
              InsertQuery.Descr.BeginUpdate;

              for j := 0 to TableMeta.Fields.Count-1 do
                if -1 < IndexInFile[j] then
                  if InsertQuery.Descr.IndexByName(TableMeta.Fields[IndexInFile[j]].Original)=-1 then
                  begin
                    InsertQuery.Descr.AddField( TableMeta.Fields[IndexInFile[j]].Original {, TableMeta.Fields[IndexInFile[j]].DataType} );
                    for k:=NextStart to i do
                      begin
                        InsertQuery.Descr.AddParamValue
                          ( TableMeta.Fields[IndexInFile[j]].Original + IntToStr(k-NextStart),
                            TableMeta.Fields[IndexInFile[j]].DataType,
                            ImportCache[k].FieldValue[IndexInFile[j]]);

                        TDeSQLDataset(InsertQuery).SetParamValue
                          ( TableMeta.Fields[IndexInFile[j]].Original + IntToStr(k-NextStart),
                            ImportCache[k].FieldValue[IndexInFile[j]],
                            TableMeta.Fields[IndexInFile[j]].DataType);
                      end;
                  end;

//            InsertQuery.Initialize;  // Initialize делаем руками ..
              InsertQuery.Descr.Calc:= TDeSQLCalculator.Create;
              InsertQuery.Descr.Calc.ParamList:= InsertQuery.Descr.Params;
              InsertQuery.Descr.Calc.onTableToStr:= TableMeta.Database.TableToStr;
              InsertQuery.Descr.Calc.onFieldToStr:= TableMeta.Database.FieldToStr;
              InsertQuery.Descr.Calc.onConstToStr:= TableMeta.Database.ConstToStr;
              InsertQuery.Descr.Calc.onFuncToStr:= TableMeta.Database.FuncToStr;
          //  InsertQuery.Descr.Calc.OnGetIdentValue:= GetMasterValue;

              InsertQuery.Descr.EndUpdate;
              TDeSQLDataset(InsertQuery).SetSql(InsertQuery.Descr.SQL);

              // собственно выполнение запроса
              if InsertQuery.ExecuteQuery
                then IncCount(iAddCount, i-NextStart+1)
                else IncCount(iSkipCount, i-NextStart+1);

            except
              on e: Exception do
                begin
                   WriteLog('Import Error: ' + E.Message);
                   IncCount(iSkipCount, i-NextStart+1);
                end;
            end;
            InsertQuery.Free;

            Application.ProcessMessages;
            NextStart:= i+1;
            ProgressBar.Position:= 20 + Round(70 * (vAddCount + vUpdateCount + vSkipCount + vEmptyCount) / vAllCount);
          end;

      if Length(FProcedure)>0 then
        try
          Postfix:= TExpressionItem.Create;
          try
            Parser.Parse(FProcedure, Postfix);
            if Postfix.Count <> 0 then
              if not Calculator.Calculate(Postfix) then
                SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(FProcedure)));
          except
            on E:EDeParserError do
            begin
              SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(FProcedure)));
            end;
          end;
        finally
          Postfix.Free;
        end;

        ProgressBar.Position:= 100;

      for i:=0 to Metadata.LibraryCount-1 do
        if Metadata.LibraryData[i].TableMeta = TableMeta then
           Metadata.LibraryData[i].EndUpdate;
    end;
  // ...........................................................................

  ImportCache.Free;
  MetaData.UpdateDataSets(TableMeta.ID, mcUpdate, null);

  Parser.Free;
  Calculator.Free;
  Screen.Cursor := BackupCursor;

  DIE_Da_Copy.Visible:= True;
  repaint;
end;

procedure TDeMaster.pnFinishEnter(Sender: TObject);
var i: Integer;
    DM: TDataManager;
    FilterItem : TFilterItem;
    CI: TCacheItem;
    Cache: TDataCache;
    s: string;
    FileNames: TStrings;
begin
  ValueList.TitleCaptions.Clear;
  ValueList.TitleCaptions.Add(GetTitle('_Dt.Param'));
  ValueList.TitleCaptions.Add(GetTitle('_Df.Value'));
  ValueList.Strings.Clear;
  ValueList.Repaint;

  ProgressBar.Position:=0;
  ProgressPanel.Visible:=True;

  DIE_Da_Close.Enabled:=False;
  DIE_Da_Back.Enabled:=False;
  repaint;
  //............................................................................
  try
    if (FOperation = moExport) then
      begin
        DoExport(Sender);
      end else

    if (FOperation = moImport) and (aInfo[FFileTypeNum].FileType in [EI_FILETYPE_CLIPBOARD, EI_FILETYPE_MSEXCEL]) then
      begin
        DoImport(Sender);
      end else

    if (FOperation = moImport) and (aInfo[FFileTypeNum].FileType in [EI_FILETYPE_TEMPLATE]) then
      begin
        FileNames:= TStringList.Create;
        FileNames.Text:= FFileName;

        for i:=0 to pred(FileNames.Count) do
          begin
            TableMeta:= nil;
            if Not Assigned(FExcelReport)
              then FExcelReport := TDeStdExcelReport.Create(nil)
              else FExcelReport.CloseAllFiles;

            CanNext:= FExcelReport.OpenFile(FileNames[i]);
            s:= FExcelReport.Comment;

            ValueList.Strings.Add(GetTitle('_dA.File=')+FileNames[i]);
            if Length(s)>0 then
              begin
                ReadComment(s, True);
                DoImport(Sender);
              end
            else
              begin
                ValueList.Strings.Add(GetTitle('=')+GetTitle('_dE.error'));
              end;
            FExcelReport.CloseAllFiles;
          end;

        FileNames.Free;
      end;

  except
    for i:=ProgressBar.Position downto 0 do
       begin
         ProgressBar.Position:=i; sleep(10);
       end;
  end;
  //............................................................................
  ProgressPanel.Visible:=False;

  DIE_Da_Close.Enabled:=True;
  repaint;

//  ставим галочку
//  DIE_Da_Close.ImageIndex:= 35;

  DM:=TDataManager.Create;

  FilterItem:= nil;
  Cache     := nil;
  
  if assigned(TableMeta) then
  try
    //Удаляем последние настройки импорта/экспорта
    DM.Table:=MetaData.GetSystemTableByName(tblUserDataSet);

    FilterItem := TFilterItem.Create;
    FilterItem.AddCondition( MetaData.MetaTables[idxUserDataset].Fields.FindByName(fldUDataSetSubject), opEQ, UserSession.ID );
    FilterItem.AddCondition( MetaData.MetaTables[idxUserDataset].Fields.FindByName(fldUDataSetType), opEQ, 1 );
    FilterItem.AddOperation(opAND);
    if not DM.DeleteRecords(FilterItem) then
      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(DM.Errors.GetMessage)));

    //Сохраняем последние настройки импорта/экспорта
    Cache := TDataCache.Create(MetaData.GetSystemTableByName(tblUserDataSet));

    CI:=Cache.AddNewItem;
    CI.ValueByName[fldUDataSetTable]   := TableMeta.ID;
    CI.ValueByName[fldUDataSetSubject] := UserSession.ID;
    CI.ValueByName[fldUDataSetXML]     := ConfigString;
    CI.ValueByName[fldUDataSetType]    := 1;
    CI.ValueByName[fldUDataSetOrder]   := 0;

    if not DM.InsertRecord(CI) then
      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(DM.Errors.GetMessage)));

  finally
    if assigned(FilterItem) then FilterItem.Free;
    if assigned(Cache) then Cache.Free;
  end;

  DM.Free;
end;

//==============================================================================

procedure TDeMaster.DIE_Da_ContinueExecute(Sender: TObject);
var BackupCursor : TCursor;
    s: string;
    i, N: Integer;
begin
  if FPage=pnImport then
  begin
    TableMeta:=nil;
    if aInfo[FFileTypeNum].FileType = EI_FILETYPE_CLIPBOARD then
      begin
        CanNext:= (0<FClipboard.Count);
        s:= EmptyStr;
      end else

    if aInfo[FFileTypeNum].FileType = EI_FILETYPE_TEMPLATE then
      begin
        CanNext:= (0<XMLMemo.Lines.Count);
        for i:=0 to Pred(XMLMemo.Lines.Count) do
          try
            CanNext:= CanNext and FileExists(XMLMemo.Lines[i]);
          except
            ShowHintWindow(FileNameEdit2, Format(GetTitle('_dE.file'),[XMLMemo.Lines[i]]),
                                          GetTitle('_dE.error'), icoError, mtError);
            Exit;
          end;
        FFileName:= XMLMemo.Text;
        SetPage(pnConfirm);
        Exit;
      end else

      begin
        BackupCursor := Screen.Cursor;
        Screen.Cursor := crHourGlass;
        try
          if assigned(FExcelReport) then
            try
              FExcelReport.CloseAllFiles;
            except
              // если Excel отвалился - обнуляем и открываем новый
              FExcelReport:= nil;
            end;

          if Not Assigned(FExcelReport) then
            FExcelReport := TDeStdExcelReport.Create(nil);

          CanNext:= FExcelReport.OpenFile(FFileName);
          s:= FExcelReport.Comment;

          if Length(s)>0 then
            ReadComment(s, True);
        except
          ShowHintWindow(FileNameEdit2, Format(GetTitle('_dE.file'),[FFileName]),
                                        GetTitle('_dE.error'), icoError, mtError);
          Screen.Cursor := BackupCursor;
          Exit;
        end;
        Screen.Cursor := BackupCursor;
      end;


    if UserSession.IsAdmin then
      begin
        if aInfo[FFileTypeNum].ProcType in [otLast, otSaved] then
          begin
            TableMeta:=  MetaData.GetTableMeta(aInfo[FFileTypeNum].DatasetID);

            if assigned(TableMeta) then
              begin
                ReadComment(aInfo[FFileTypeNum].Info, True);
                if assigned(TableMeta) then SetPage(pnSetFields)
                                       else SetPage(pnDataset);
              end
            else
                SetPage(pnDataset);
          end
        else
          begin
            SetPage(pnDataset);
          end;
      end
    else
      begin
        if assigned(TableMeta) then
          SetPage(pnConfirm)
        else;
          SetPage(pnFinish);
      end;
    Exit;
  end;

  if (FPage = pnExport) then
    begin
      FProcType:=aInfo[FFileTypeNum].ProcType;

      if aInfo[FFileTypeNum].ProcType = otStandart then
        SetPage(pnDataSet)
      else
        begin
          if (aInfo[FFileTypeNum].DatasetID>-1) then
            begin
              TableMeta:=MetaData.GetTableMeta(aInfo[FFileTypeNum].DatasetID);
              ReadComment(aInfo[FFileTypeNum].Info, False);
            end
          else
            begin
              ReadComment(aInfo[FFileTypeNum].Info, True);
            end;

          n:= 0;
          for i:=Low(FieldConnection) to High(FieldConnection) do
            if Length(FieldConnection[i])>0 then
              begin
                FieldConnection[i]:= ExcelColumnName(N);
                Inc(N);
              end;

          SetPage(pnFilter);
        end;
      exit;
    end;

  if (FPage=pnDataSet) and (FOperation=moImport) then
    begin
      if not SecuritySystem.CheckPolicyDataSet(FTableMeta.ID, spUpdate) then
        begin
          ShowHintWindow(ToolButton6, GetTitle('_eRror.right'),
                                      GetTitle('_dE.error'), icoError, mtError);
          exit;
        end;
      SetPage(pnSetFields);
      exit;
    end;

  if FPage=pnSetFields then
    begin
      for i:=low(FieldConnection) to High(FieldConnection) do
        FieldConnection[i]:= ValueListEditorTo.Values[TableMeta.Fields[i].Original];

      SetPage(pnConfirm);
      exit;
    end;
  //............................................................................

  if (FPage=pnDataSet) and (FOperation=moExport) then
    begin
      if not SecuritySystem.CheckPolicyDataSet(FTableMeta.ID, spSelect) then
      begin
        ShowHintWindow(ToolButton6, GetTitle('_eRror.right'),
                                    GetTitle('_dE.error'), icoError, mtError);
        exit;
      end;
      SetPage(pnFieldsSelect);
      exit;
    end;

  if FPage=pnFieldsSelect then
    begin
      N:= 0;

      for i:=0 to FTableMeta.Fields.Count-1 do
        if FieldsBox.Checked[i] then
          begin
            FieldConnection[i]:= ExcelColumnName(N);
            inc(N);
          end
        else
          begin
            FieldConnection[i]:= EmptyStr;
          end;

      SetPage(pnFilter);
      exit;
    end;

  if FPage=pnFilter then
    begin
      if (FilterPage.ActivePage = ts_dF_filter) then
          FFilter:= FFilterControl.FilterAsString;

      SetPage(pnConfirm);
      exit;
    end;
  //............................................................................

  if FPage=pnConfirm  then
  begin
    SetPage(pnFinish);
    exit;
  end;
end;

procedure TDeMaster.DIE_Da_CopyExecute(Sender: TObject);
begin
  inherited;

  ValueList.Strings;

  Clipboard.Open;
  try
    Clipboard.Clear;
    Clipboard.AsText:= ValueList.Strings.Text;
  finally
    Clipboard.Close;
  end;

end;

procedure TDeMaster.DIE_Da_PreviewExecute(Sender: TObject);
begin
  inherited;
  //
end;

procedure TDeMaster.DIE_Da_PrintExecute(Sender: TObject);
begin
  inherited;
  //
end;

procedure TDeMaster.DIE_Da_SaveExecute(Sender: TObject);
begin
  inherited;
 //
end;

procedure TDeMaster.Splitter1Moved(Sender: TObject);
begin
  inherited;
  FLR:=PanelFrom.width/(PanelFrom.width+PanelTo.width);
end;

procedure TDeMaster.pnSetFieldsResize(Sender: TObject);
begin
  inherited;
  PanelFrom.width:=Round(FLR*(pnSetFields.Width-Splitter1.Width));
end;

procedure TDeMaster.MI_NameClick(Sender: TObject);
begin
  inherited;
  FShowNames:=TMenuItem(Sender).Tag;
  TMenuItem(Sender).Checked:=true;
  ValueListEditorTo.Repaint;
end;

procedure TDeMaster.BDActionsClick(Sender: TObject);
Var P      : TPoint;
    paSave : TPopupAlignment;
begin
  P := TSpeedButton(Sender).ClientOrigin;
  paSave:= PopupMenu1.Alignment; 
  TSpeedButton(Sender).PopupMenu.Alignment:=paRight;
  TSpeedButton(Sender).PopupMenu.Popup(P.X+BDActions.Width,P.Y+BDActions.Height);
  TSpeedButton(Sender).PopupMenu.Alignment:=paSave;
end;

procedure TDeMaster.PopupMenu1Popup(Sender: TObject);
var i: Integer;
begin
  for i:= 0 to Pred(MI_imp_header.Count) do
    MI_imp_header[i].Checked:= (MI_imp_header[i].Tag = FIFR);
end;

procedure TDeMaster.MI_dL_noClick(Sender: TObject);
begin
  FIFR:= TMenuItem(Sender).Tag;
  ListViewFromUpdate(Sender);
end;

procedure TDeMaster.Da_UpdateFrom(Sender: TObject);
var i,j,N: Integer;
begin
  ListViewFrom.Items.BeginUpdate;

  for i:= 0 to ListViewFrom.Items.Count-1 do
    ListViewFrom.Items[i].ImageIndex:= 0;

  for i:=Low(FieldConnection) to High(FieldConnection) do
    if assigned(FieldFileValue[i]) then
      for j:=0 to Pred(FieldFileValue[i].Count) do
        if FieldFileValue[i].Items[j].ItemType = piIdent then
          begin
            N:= ExcelColumnIndex(FieldFileValue[i].Items[j].Name);
            if (0<=N) and (N<ListViewFrom.Items.count) then ListViewFrom.Items[N].ImageIndex:= 1;
          end;

  ListViewFrom.Items.EndUpdate;
end;

procedure TDeMaster.MI_Da_ClearAllClick(Sender: TObject);
var i: integer;
begin
  inherited;

  // ListFrom
  for i:=0 to ListViewFrom.Items.Count-1 do
    ListViewFrom.Items[i].ImageIndex := 0;
  ListViewFrom.Refresh;

  // ListTo
  for i:= Low(FieldConnection) to High(FieldConnection) do
    begin
      FieldConnection[i]:= unassigned;
      ValueListEditorTo.Values[TableMeta.Fields[i].Original]:= EmptyStr;
    end;
  ValueListEditorTo.Repaint;

  CanNext:= IsFieldConnectionFull;
end;

procedure TDeMaster.FileTypeBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  R: TRect;
begin
  with TListBox(Control),TListBox(Control).Canvas do
  begin
    if odSelected in State then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color  := clHighlightText;
      end
    else
      begin
        Canvas.Brush.Color := clWindow;
        Canvas.Font.Color  := clBtnText;
      end;

    Brush.Style := bsSolid;
    FillRect(Rect);

    R:=Rect;
    R.Bottom:=R.Top + (R.Bottom - R.Top) div 2 - 2;
    TextRect(R, R.Left + Rect.Height + 4, R.Bottom - TextHeight('W'), aInfo[Index].Name);

    Canvas.Font.Color  := clGray;

    R:=Rect;
    R.Top:=R.Top + (R.Bottom - R.Top) div 2;
    TextRect(R, R.Left + Rect.Height + 4, R.Top, aInfo[Index].Name2);

    if assigned(aInfo[Index].Graphic) then
      Draw( (Rect.Height - aInfo[Index].Graphic.Width) div 2,
             Rect.Top + (Rect.Bottom - Rect.Top - aInfo[Index].Graphic.Height) div 2,
             aInfo[Index].Graphic);

    if odFocused in State then
      begin
        InflateRect(Rect,-1,-1);
        DrawFocusRect(Rect);
      end;
  end;
end;

procedure TDeMaster.pnImportEnter(Sender: TObject);
var N,i,j : Integer;
    TM_UDS, TM: TTableMeta;
    DataCache: TDataCache;
    Filter: TFilterItem;
    Clipboard: TClipboard;
begin
  i:=0;

  Clipboard:= TClipboard.Create;
  try
    Clipboard.Open;
    ClipBoardSplit(Clipboard.AsText, #13#10, FClipboard);
    if 0 < FClipboard.Count then ClipBoardSplit(FClipboard[0], #9, FClipboardCaption)
                            else FClipboardCaption.Clear;
  finally
    Clipboard.Close;
  end;

    begin
      SetLength(aInfo, i+1);
      aInfo[i]:= TOperationInfo.Create;
      aInfo[i].ProcType := otStandart;
      aInfo[i].FileType := EI_FILETYPE_TEMPLATE;
      aInfo[i].Name     := GetTitle(EI_FILETYPES[aInfo[i].FileType]);
      aInfo[i].Name2    := '*' + EI_FILEEXT[aInfo[i].FileType];
      aInfo[i].Ext      := EmptyStr;
      aInfo[i].Info     := EmptyStr;
      aInfo[i].Graphic := TIcon.Create;
      DM.ilIcon32.GetIcon( DM.MapIconIndex(2), TIcon(aInfo[i].Graphic) );
      inc(i);
    end;

  if 0 < FClipBoard.Count then
    begin
      SetLength(aInfo, i+1);
      aInfo[i]:= TOperationInfo.Create;
      aInfo[i].ProcType := otStandart;
      aInfo[i].FileType := EI_FILETYPE_CLIPBOARD;
      aInfo[i].Name     := GetTitle(EI_FILETYPES[aInfo[i].FileType]);
      if (FClipBoard.Count = 1) and (FClipboardCaption.Count = 1)
        then aInfo[i].Name2:= FClipboardCaption.Text
        else aInfo[i].Name2:= IntToStr(FClipBoard.Count) + ' * ' + IntToStr(FClipboardCaption.Count);
      aInfo[i].Ext      := EmptyStr;
      aInfo[i].Info     := EmptyStr;
      for j:=0 to Pred(FClipboardCaption.Count) do
        aInfo[i].Info:= aInfo[i].Info + FClipboardCaption[j]+#13#10;
      aInfo[i].Graphic := TIcon.Create;
      DM.ilIcon32.GetIcon( DM.MapIconIndex(102), TIcon(aInfo[i].Graphic) );
      inc(i);
    end;

      SetLength(aInfo,i+1);
      aInfo[i]:= TOperationInfo.Create;
      aInfo[i].ProcType := otStandart;
      aInfo[i].FileType := EI_FILETYPE_MSEXCEL;
      aInfo[i].Name     := GetTitle(EI_FILETYPES[aInfo[i].FileType]);
      aInfo[i].Name2    := '*' + EI_FILEEXT[aInfo[i].FileType];
      aInfo[i].Ext      := EmptyStr;
      aInfo[i].Info     := EmptyStr;
      aInfo[i].Graphic := TIcon.Create;
      TIcon(aInfo[i].Graphic).Handle := GetRegistryIconHandle('abcd' + EI_FILEEXT[aInfo[i].FileType]);

  if UserSession.IsAdmin then
    try
      TM_UDS:= MetaData.GetSystemTableByName(tblUserDataSet);
      DataCache := TDataCache.Create(TM_UDS);

      Filter := TFilterItem.Create;
      Filter.AddCondition(TM_UDS.Fields.FindByName(fldUDataSetSubject), opEQ, UserSession.ID);
      Filter.AddCondition(TM_UDS.Fields.FindByName(fldUDataSetSubject), opEQ, 0);
      Filter.AddCondition(TM_UDS.Fields.FindByName(fldUDataSetSubject), opEQ, null);
      Filter.AddOperation(opOr);
      Filter.AddOperation(opOr);
      Filter.AddCondition(TM_UDS.Fields.FindByName(fldUDataSetType), opEQ, 1);
      Filter.AddOperation(opAnd);

      DataCache.Filters.AddFilter(Filter);
      DataCache.PrepareData(True, fsFull);

      N:= Length(aInfo);
      SetLength(aInfo, N + DataCache.Count);

      for i:=0 to Pred(DataCache.Count) do
        begin
          if (DataCache.Items[i].ValueByName[fldUDataSetSubject]= UserSession.ID) or
             (SecuritySystem.CheckPolicy(TM_UDS.ID, DataCache.Items[i].ValueByName[fldUDataSetID], spExecute)) then
            begin
              aInfo[N+i]:=  TOperationInfo.Create;

              aInfo[N+i].DatasetID:= DataCache.Items[i].ValueByName[fldUDataSetTable];
              aInfo[N+i].Info     := DataCache.Items[i].ValueByName[fldUDataSetXML];
              aInfo[N+i].FileType := EI_FILETYPE_MSEXCEL;
              aInfo[N+i].Graphic  := TIcon.Create;

              if (DataCache.Items[i].ValueByName[fldUDataSetSubject]= UserSession.ID) then
                begin
                  aInfo[N+i].ProcType := otLast;
                  aInfo[N+i].Name := GetTitle('_Dl.lastracurse');
                end
              else
                begin
                  aInfo[N+i].ProcType := otSaved;
                  aInfo[N+i].Name:= DataCache.Items[i].ValueNativeByName[fldUDataSetName]
                end;

              TM:=Metadata.GetTableMeta(aInfo[N+i].DatasetID);
              if Assigned(TM) then
                begin
                  aInfo[N+i].Name2 := GetTitle(TM.Name);
                  DM.ilIcon32.GetIcon( DM.MapIconIndex(TM.Ico), TIcon(aInfo[N+i].Graphic) )
                end
              else
                begin
                  aInfo[N+i].Name2 := GetTitle('_dV.noname');
                  DM.ilIcon32.GetIcon( DM.MapIconIndex(99), TIcon(aInfo[N+i].Graphic) )
                end;
            end;
        end;

    finally
      DataCache.Free;
    end;

  ImportFileTypeBox.Items.Clear;
  for i:=0 to Length(aInfo)-1 do
    ImportFileTypeBox.Items.Add(GetTitle(EI_FILETYPES[i]));

  ImportFileTypeBox.ItemIndex:=FFileTypeNum;
  ImportFileTypeBoxClick(Sender);

  CanNext:=FileExists(FFileName) or (aInfo[FFileTypeNum].FileType = EI_FILETYPE_CLIPBOARD);
  FFilePath:=Variables.AsString[RegDirPath];
  ImportFileTypeBox.OnDrawItem:= FileTypeBoxDrawItem;
end;

procedure TDeMaster.BT2_Da_BrowseClick(Sender: TObject);
var OpenDlg : TOpenDialog;
    i: Integer;
begin
  OpenDlg:=TOpenDialog.Create(nil);
  OpenDlg.InitialDir  := Variables.AsString[RegLastImportPath];
  if Length(OpenDlg.InitialDir)=0 then OpenDlg.InitialDir:= FFilePath;

  if (aInfo[FFileTypeNum].FileType = EI_FILETYPE_TEMPLATE)
    then OpenDlg.Options:=OpenDlg.Options + [ofAllowMultiSelect]
    else OpenDlg.Options:=OpenDlg.Options - [ofAllowMultiSelect];

  OpenDlg.Filter      := GetTitle(dlgFilters);
  OpenDlg.FilterIndex := dlgFilterExcel;
  if OpenDlg.Execute then
    begin
      Variables.AsString[RegLastImportPath]:= ExtractFilePath(OpenDlg.FileName);
      if aInfo[FFileTypeNum].FileType in [EI_FILETYPE_TEMPLATE] then
        begin
          XMLMemo.Lines.Clear;
          for i:=0 to OpenDlg.Files.Count-1 do
            XMLMemo.Lines.Add(OpenDlg.Files[i]);
        end
      else
        begin
          FileNameEdit2.Text:= OpenDlg.FileName;
        end;

    end;
  OpenDlg.Free;
end;

function TDeMaster.InnerFieldConnectionDefined(aIndex: Integer): Boolean;
begin
  Result:= (Low(FieldConnection)<=aIndex) and (aIndex<= High(FieldConnection)) and
           Not VarIsEmpty(FieldConnection[aIndex]) and (Length(FieldConnection[aIndex])>0);
end;

procedure TDeMaster.FileNameEdit2Change(Sender: TObject);
begin
  inherited;
  FFileName:= FileNameEdit2.Text;
  CanNext:=FileExists(FFileName);
end;

procedure TDeMaster.ParamList_PickValues(index: Integer; DA: Array of String);
var i: Integer;
begin
  ParamList.ItemProps[index].EditStyle:=esPickList;
  for i:=Low(DA) to High(DA) do
    ParamList.ItemProps[index].PickList.Add(GetTitle(DA[i]));
end;

procedure TDeMaster.SetTableMeta(aTableMeta: TTableMeta);
var i,n : Integer;
    A,B,C,D : Word;
    sFileName: string;
begin
  if FTableMeta=aTableMeta then Exit;

  FTableMeta:=aTableMeta;

  if Assigned(FTableMeta) then
    begin
      FTableName:=aTableMeta.Table;
      sFileName:=GetTitle(aTableMeta.Name, ttSecondName);

      if aInfo[FFileTypeNum].ProcType = otSaved then
        begin
          DecodeDate(Date,A,B,C);
          sFileName := sFileName+Format(' (%2.2d-%2.2d-%4.4d',[C,B,A]);
          DecodeTime(Time,A,B,C,D);
          sFileName := sFileName+Format(' %2.2d-%2.2d-%2.2d)',[A,B,C]);
        end;

      n:=FTableMeta.Fields.Count;
    end
  else
    begin
      FTableName:= EmptyStr;
      sFileName:= EmptyStr;
      n:= 0;
    end;
  if FOperation in [moExport] then FFileName:= sFileName;

  SetLength(FieldConnection, n);
  SetLength(FieldFileValue, n);
  SetLength(FieldMetaValue, n);

  if (aInfo[FFileTypeNum].ProcType = otStandart) and (FOperation = moExport)
    then
      for i:=0 to Length(FieldConnection)-1 do FieldConnection[i]:= ExcelColumnName(i)
    else
      for i:=0 to Length(FieldConnection)-1 do FieldConnection[i]:= EmptyStr;
end;

procedure TDeMaster.WMFilterChange(var Message: TMessage);
begin
  FFilterControl.Height:=FFilterControl.NeedHeight;
end;

procedure TDeMaster.XMLMemoChange(Sender: TObject);
var i: Integer;
begin
  inherited;
  for i:=pred(XMLMemo.Lines.Count) downto 0 do
    if Length(Trim(XMLMemo.Lines[i]))=0 then XMLMemo.Lines.Delete(i);

  if aInfo[FFileTypeNum].FileType = EI_FILETYPE_TEMPLATE then
    begin
      CanNext:= (XMLMemo.Lines.Count>0);
      for i:=0 to pred(XMLMemo.Lines.Count) do
        CanNext:= CanNext and FileExists(XMLMemo.Lines[i]);
    end;
end;

procedure TDeMaster.FilterMemoChange(Sender: TObject);
var Parser : TDeParser;
    FFilterPostfix : TFilterItem;
    s: string;
begin
  inherited;
  FFilter:= FilterMemo.text;
  s:= EmptyStr;

  if (Length(FFilter) > 0) then
    try
      Parser := TDeParser.Create;
      Parser.Table := FTableMeta;
      FFilterPostfix:= TFilterItem.Create;
      try
        Parser.Parse(FFilter, FFilterPostfix);
      except
        on E: EDeParserError do
          s:=E.Message;
      end;
      Parser.Free;
      FFilterPostfix.Free;
      FilterPage.ActivePage := ts_SQL;
    except
    end
  else
     FilterPage.ActivePage := ts_dA_all;

  FilterError.Caption :=GetTitle(s);
  CanNext:=(Length(s)=0);
end;

procedure TDeMaster.MI2_Da_InvertSelectionClick(Sender: TObject);
var i: Integer;
begin
  for i:=0 to FieldsBox.Items.Count-1 do
    FieldsBox.Checked[i]:=not FieldsBox.Checked[i];
end;

procedure TDeMaster.MI2_Da_ClearAllClick(Sender: TObject);
var i: Integer;
begin
  for i:=0 to FieldsBox.Items.Count-1 do
    FieldsBox.Checked[i]:= False;
  for i:=0 to Pred(Length(FieldConnection)) do
    FieldConnection[i]:= unassigned;
end;

procedure TDeMaster.MI2_dA_selectallClick(Sender: TObject);
var i: Integer;
begin
  for i:=0 to FieldsBox.Items.Count-1 do
    FieldsBox.Checked[i]:=True;
end;

procedure TDeMaster.MI2_dA_defaultClick(Sender: TObject);
var i: Integer;
begin
  for i:=0 to FieldsBox.Items.Count-1 do
    FieldsBox.Checked[i]:=
    (
     (FTableMeta.Fields[i].VisibleLevel in [fvLevel2,fvLevel3])
     and
     ((FTableMeta.Fields[i].IsLookup) or Not Assigned(FTableMeta.Fields[i].LookupPair))
    )
    or
    (FTableMeta.Fields[i].Key);
end;

procedure TDeMaster.FilterPageChange(Sender: TObject);
var i: Integer;
begin
  for i:=0 to FilterPage.PageCount - 1 do
      if FilterPage.ActivePage=FilterPage.Pages[i] then FilterPage.Pages[i].ImageIndex:=1
                                                   else FilterPage.Pages[i].ImageIndex:=0;

  if FilterPage.ActivePage = ts_dA_all then
    begin
      FFilterMode := fmAll;
      FFilter:= EmptyStr;
    end;
  if FilterPage.ActivePage = ts_dA_representation then
    begin
      FFilterMode := fmCurrent;
      FFilter:= Null;
    end;
  if FilterPage.ActivePage = ts_dF_template then
    begin
      FFilterMode := fmTemplate;
      FFilter:= EmptyStr;
    end;
  if FilterPage.ActivePage = ts_SQL then
    begin
      FFilterMode := fmStringFilter;
      FFilter:= FilterMemo.Text;
    end;
  if FilterPage.ActivePage = ts_dF_filter then
    begin
      FFilterMode := fmControl;
      FFilter:= FFilterControl.FilterAsString;
    end;

  SplitByBox.Enabled:= not (FFilterMode = fmTemplate);
end;

procedure TDeMaster.FormCreate(Sender: TObject);
begin
  inherited;
  DM.MapActionListIcons(ActionList1);
  FClipBoard := TStringList.Create;
  FClipboardCaption := TStringList.Create;
end;

procedure TDeMaster.Splitter2Moved(Sender: TObject);
begin
  inherited;
  FLR2:=ExportFileTypeBox.width/(pnExport.Width);
end;

procedure TDeMaster.pnExportResize(Sender: TObject);
begin
  inherited;
  ExportFileTypeBox.width:=Round(FLR2*(pnExport.Width-Splitter2.Width));
end;

end.



