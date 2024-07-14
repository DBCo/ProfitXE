unit DeCommandDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,  Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin, System.Actions, Vcl.ActnList, Vcl.Menus,
  DB, DeActions, DeTypes, ElementsMeta, DeMeta, DeVariable, DeParser;

type
  TDeCommandDialog = class(TForm)
    paClients: TPanel;
    paButtons: TPanel;
    paCommands: TPanel;
    spHorizontal: TSplitter;
    paCommandParams: TPanel;
    sbCommandParams: TScrollBox;
    lvCommands: TListView;
    alActions: TActionList;
    CD_Da_OK: TAction;
    CD_Da_Cancel: TAction;
    CD_Da_Preview: TAction;
    CD_Da_Print: TAction;
    tbButtons: TToolBar;
    tbPrint: TToolButton;
    tbPreview: TToolButton;
    tbOK: TToolButton;
    tbCancel: TToolButton;
    paHeader: TPanel;
    beBottom: TBevel;
    imLogo: TImage;
    laQuestion: TLabel;
    laTitle: TLabel;
    pmCommands: TPopupMenu;
    AC_Da_IconsBig: TAction;
    AC_Da_IconsSmall: TAction;
    AC_Da_IconsList: TAction;
    AC_Da_IconsReport: TAction;
    MI_Da_View: TMenuItem;
    MI_Da_IconsBig: TMenuItem;
    MI_Da_IconsSmall: TMenuItem;
    MI_Da_IconsList: TMenuItem;
    MI_Da_IconsReport: TMenuItem;
    CD_Da_Modify: TAction;
    N1: TMenuItem;
    MI_Da_Modify: TMenuItem;
    CD_Da_Export: TAction;
    tbAdditionals: TToolBar;
    tbSave: TToolButton;
    tbBarAlignNotVisible: TToolButton;
    imSpace: TImage;
    ToolButton1: TToolButton;
    CD_Mail_Send: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CD_Da_CancelExecute(Sender: TObject);
    procedure CD_Da_PrintExecute(Sender: TObject);
    procedure CD_Da_PreviewExecute(Sender: TObject);
    procedure lvCommandsData(Sender: TObject; Item: TListItem);
    procedure lvCommandsResize(Sender: TObject);
    procedure SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure AC_DA_IconsUpdate(Sender: TObject);
    procedure AC_DA_IconsExecute(Sender: TObject);
    procedure CD_Da_ModifyExecute(Sender: TObject);
    procedure CD_Da_ExportExecute(Sender: TObject);
    procedure CD_Da_OKExecute(Sender: TObject);
    procedure CD_Mail_SendExecute(Sender: TObject);
  private
    { Private declarations }
    FActionData: TDeActionData;
    FFormDispatcher: TDeFormDispatcher;
    FQuestionEnabled: Boolean;
    FProfitActions: array of Integer;
    FParameters: TVariableList;
    FVariables: TDeVariableList;
    FIdentCounter: Integer;
    FExportDirectory: string;
    FActionMode: TActionMode;
    procedure ClearParameterControls;
    function InternalBuildParameterControls(CommandAction: TDeActionData; var ElementTop: Integer): Integer;
    function BuildParameterControls(CommandAction: TDeActionData): Integer;
    procedure ReadParameterValues(Variables: TDeVariableList);
    function BuildActionList(const DatasetID: Integer): Integer; overload;
    function BuildActionList(CommandAction: TDeActionData): Integer; overload;
    function GetQuestion: string;
    procedure SetQuestion(const Value: string);
    function GetVariableItemValue_Control(Sender: TVariableItem): Variant;
    procedure SetActionData(const aValue: TDeActionData);
    type
      TArrayCompareEvent = function(const LeftIndex, RightIndex: Integer): Integer of object;
    procedure QuickSort(LowIndex, HighIndex: Integer; OnCompare: TArrayCompareEvent);
    function CompareByOrder(const LeftIndex, RightIndex: Integer): Integer;
    procedure SortByOrder;
    function CompareByCaption(const LeftIndex, RightIndex: Integer): Integer;
    procedure SortByCaption;
    function GetIdentType(const aIdent: string; var aType: TFieldType; aDataObject: pointer): Boolean;
    function GetIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
    function CommandGetVariable(const aIdent: string; var aVariable: TVariableItem; aDataObject: Pointer): Boolean;
    procedure ReadFormSize;
    procedure WriteFormSize;
    procedure QuestionUpdate(Action: TDeActionData);
    function RecalculateHeightCommandParams: Integer;
    procedure UpdateSizeCommandParams;
    procedure ReadLastCommandID;
    procedure WriteLastCommandID;
    function CheckParameterValues(var Control: TControl): Boolean;
    /// <summary>Функция поиска только одного GRID`а</summary>
    /// <returns>Функция возвращает ссылку на грид только тогда, когда он есть и он только один. Иначе возвращает nil.</returns>
    function FindJustOneGrid: TControl;
    function FindElementByName(const Name: string): TDeElement;
    function CheckSingleValueInArray(const Value: Variant): Variant;
    procedure CheckVariableButtons(CommandAction: TDeActionData; Variables: TDeVariableList);
  public
    { Public declarations }
    class function Execute(CommandAction: TDeActionData; Variables: TDeVariableList; const QuestionEnabled: Boolean; const Mode: TActionMode = amMultiIn): TModalResult; overload;
    class function Execute(const DatasetID: Integer; Variables: TDeVariableList; const Mode: TActionMode = amMultiIn): TModalResult; overload;
    property Question: string read GetQuestion write SetQuestion;
    property ActionData: TDeActionData read FActionData write SetActionData;
  end;

function DatasetRecordCaption(const DataSetID: Integer; const Key: Variant): string;

implementation

{$R *.dfm}

uses Printers, Math, StrUtils, Generics.Collections,
     DeLog, Dictionary, Funcs, DeMetadata, DeControls, DSMeta, DataUnit, DataCacheUnit, DeToolbars,
     DeCalculator, DeScript, LogoForm, DeSettings, UnitA, HintForm, Main, BaseGridFormUnit, Element;

function DatasetRecordCaption(const DataSetID: Integer; const Key: Variant): string;
var
  Index, Position: Integer;
  DataCache: TDataCache;
begin
  Result := EmptyStr;
  for Index := 0 to Pred(Metadata.LibraryCount) do
    if Metadata.LibraryData[Index].TableMeta.ID = DataSetID then
      begin
        Position := Metadata.LibraryData[Index].IndexByID(Key);
        if Position <> -1 then
          begin
            Result := Metadata.LibraryData[Index].Items[Position].Caption;
            Exit;
          end;
      end;
  DataCache := TDataCache.Create(Metadata.GetTableMeta(DataSetID));
  try
    DataCache.Filters.NewFilter(DataCache.TableMeta.KField[0], opEQ, Key);
    DataCache.PrepareData;
    if DataCache.Count = 1 then
      Result := DataCache[0].Caption;
  finally
    DataCache.Free;
  end;
end;

{ TDeCommandDialog }

resourcestring
  sQuestionPrint = 'Для печати нажмите кнопку "Печать" или для закрытия окна нажмите кнопку "Отмена".';
  sQuestionOK = 'Для выполнения нажмите кнопку "ОК" или для закрытия окна нажмите кнопку "Отмена".';

procedure TDeCommandDialog.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  DebugLog('Create ' + ClassName + ' ...');
  {$ENDIF}
  FParameters := TVariableList.Create;
  FVariables := TDeVariableList.Create;
  FFormDispatcher := TDeFormDispatcher.Create;
//  FFormDispatcher.OnGetVariableItem := GetVariableItemValue_Control;
  sbCommandParams.OnResize := FFormDispatcher.ElementPanelResize;
  LangFormUpdate(Self, '_Da.Commands');
  LoadLeftLogoPucture(imLogo.Picture);
  LoadCenterLogoPucture(imSpace.Picture);
  FExportDirectory := Variables.AsString[RegDirPath];
  if Length(FExportDirectory) <> 0 then
    FExportDirectory := IncludeTrailingPathDelimiter(FExportDirectory);
  DM.MapActionListIcons(alActions);
  //if Screen.Width > Constraints.MinWidth then
  //  Width := (Screen.Width div 100) * 80;
end;

procedure TDeCommandDialog.FormDestroy(Sender: TObject);
begin
  {$IFDEF DEBUG}
  if Assigned(FVariables) then
    FVariables.DebugVariablesLog('Destroy ' + ClassName + ' and free variables ...')
  else
    DebugLog('Destroy ' + ClassName + ' ...');
  {$ENDIF}
  ClearParameterControls;
  FreeAndNil(FParameters);
  FreeAndNil(FVariables);
  FFormDispatcher.Free;
end;

procedure TDeCommandDialog.FormShow(Sender: TObject);
begin
  if lvCommands.Visible and Assigned(lvCommands.OnSelectItem) then
    lvCommands.OnSelectItem(lvCommands, lvCommands.Selected, Assigned(lvCommands.Selected));
end;

procedure TDeCommandDialog.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
   VK_ESCAPE: { Выход с отменой }
     if CD_Da_Cancel.Enabled then
       CD_Da_CancelExecute(CD_Da_Cancel);
   VK_RETURN: { Выполнить действие }
     if CD_Da_OK.Visible and CD_Da_OK.Enabled then
       CD_Da_PreviewExecute(CD_Da_OK);
  end;
end;

procedure TDeCommandDialog.CD_Da_CancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TDeCommandDialog.CD_Da_PreviewExecute(Sender: TObject);
var
  Control: TControl;
begin
  FVariables.SetByName(varPrinterName, Null);
  if CheckParameterValues(Control) then
    ModalResult := mrOK
  else
    if Assigned(Control) then
      begin
        if (Control is TWinControl) and (Control as TWinControl).Enabled then
          FocusControl(Control as TWinControl);
        ShowHintWindow(Control, GetTitle('_eRror.SetField'), EmptyStr, icoInfo);
      end
    else
      SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(GetTitle('_eRror.Edit'))));
end;

procedure TDeCommandDialog.CD_Da_PrintExecute(Sender: TObject);
var
  Control: TControl;
begin
 if Assigned(Sender) then
   begin
     if Sender is TMenuItem then
       FVariables.SetByName(varPrinterName, Printer.Printers[(Sender as TMenuItem).Tag])
     else
       FVariables.SetByName(varPrinterName, Printer.Printers[Printer.PrinterIndex]);
     if CheckParameterValues(Control) then
       ModalResult := mrOK
     else
       begin
         if Assigned(Control) then
           begin
             ShowHintWindow(Control, GetTitle('_eRror.SetField'), EmptyStr, icoInfo);
             if (Control is TWinControl) and (Control as TWinControl).Enabled then
               FocusControl(Control as TWinControl);
           end
         else
           SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(GetTitle('_eRror.Edit'))));
         Exit;
       end;
   end;
end;

procedure TDeCommandDialog.CD_Mail_SendExecute(Sender: TObject);
begin
  ///
end;

procedure TDeCommandDialog.lvCommandsData(Sender: TObject; Item: TListItem);
var
  ActionData: TDeActionData;
begin
  ActionData := ProfitActions[FProfitActions[Item.Index]];
  Item.Caption := GetTitle(ActionData.Caption);
  Item.ImageIndex := DM.MapIconIndex(ActionData.ICO);
end;

procedure TDeCommandDialog.lvCommandsResize(Sender: TObject);
begin
  (Sender as TListView).Columns[0].Width := (Sender as TListView).Width - GetSystemMetrics(SM_CXVSCROLL);
end;

procedure TDeCommandDialog.SetActionData(const aValue: TDeActionData);
begin
  if assigned(aValue) and (FActionData=aValue) then Exit;
  FActionData := aValue;
  FVariables.Clear;
  ClearParameterControls;
  CD_Da_Print.Enabled := Assigned(aValue) and (Printer.Printers.Count > 0);
  CD_Da_Preview.Enabled := Assigned(aValue);
  CD_Da_Export.Enabled := Assigned(aValue) and (Length(FExportDirectory) <> 0);
  if Assigned(aValue) then
    begin
      laTitle.Caption := GetTitle(aValue.Caption, ttSecondName);
      CD_Da_Modify.Visible := lvCommands.Visible;
      CD_Da_Modify.Enabled := CD_Da_Modify.Visible and aValue.CheckPolicy(spUpdate);
      QuestionUpdate(aValue);
      FVariables.SetByName(varActionID, aValue.ID);
      FVariables.SetByName(varActionType, Ord(aValue.ActionType));
      FVariables.SetByName(varActionOriginal, aValue.Original);
      BuildParameterControls(aValue);
      CD_Mail_Send.Enabled := assigned(FVariables.FindVariable(varMailTo)) and
                              assigned(FVariables.FindVariable(varMailFrom)) and
                              assigned(FVariables.FindVariable(varMailTitle)) and
                              assigned(FVariables.FindVariable(varMailText));
    end
  else
    begin
      laTitle.Caption := EmptyStr;
      QuestionUpdate(aValue);
      FVariables.SetByName(varActionID, -2);
      FVariables.SetByName(varActionType, -1);
      FVariables.SetByName(varActionOriginal, Null);
      CD_Da_Modify.Visible := False;
      CD_Da_Modify.Enabled := False;
      CD_Mail_Send.Enabled := False;
    end;

end;

procedure TDeCommandDialog.SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected then ActionData:= ProfitActions[FProfitActions[Item.Index]]
              else ActionData:= nil;
end;

class function TDeCommandDialog.Execute(CommandAction: TDeActionData; Variables: TDeVariableList; const QuestionEnabled: Boolean; const Mode: TActionMode): TModalResult;
begin
  Assert(Assigned(Variables), 'Variables is nil!');
  // Если нужно получить параметры действия, то ...
  if Assigned(CommandAction) then
    begin
      if CommandAction.CheckPolicy(spExecute) then
        with Self.Create(Application) do
          try
            FActionMode := Mode;
            FActionData:= CommandAction;
            lvCommands.OnSelectItem := nil;
            lvCommands.Visible := CommandAction.ActionType = atGroup;
            laTitle.Caption := GetTitle(CommandAction.Caption, ttSecondName);
            spHorizontal.Visible := lvCommands.Visible;
            lvCommands.ViewStyle := vsReport;
            MI_Da_View.Visible := False;
            if not lvCommands.Visible then paCommandParams.Align := alClient;
            CD_Da_Preview.Visible := CommandAction.CheckActionType(atReport);
            CD_Da_Print.Visible := CD_Da_Preview.Visible;
            CD_Da_Export.Visible := CD_Da_Preview.Visible;

            CD_Da_OK.Visible := not CD_Da_Preview.Visible;
            //if CommandAction.ICO <> -1 then
            //  DM.sActiveCaptionImages{SImages}.GetIcon(CommandAction.ICO, Icon);
            if CD_Da_Print.Visible then BuildPrinterList(tbPrint);
            QuestionUpdate(CommandAction);
            FVariables.SetByName(varActionID, CommandAction.ID);
            FVariables.SetByName(varActionType, Ord(CommandAction.ActionType));
            FVariables.SetByName(varActionOriginal, CommandAction.Original);

            //if (CommandAction.ActionType = atPreview) or CD_Da_Preview.Visible then
              if Variables.IndexByName(varPrinterName) <> -1 then
                FVariables.SetByName(varPrinterName, Variables.GetValueByName(varPrinterName))
              else
                FVariables.SetByName(varPrinterName, Null);

            if BuildParameterControls(CommandAction) <> 0 then
              begin
                if lvCommands.Visible then
                  if BuildActionList(CommandAction) <> 0 then
                    lvCommands.Selected := lvCommands.Items[0];
                if CommandAction.ActionType in [atPreview, atReport] then Caption := GetTitle('_Dt.Report');
                UpdateSizeCommandParams;
                if CD_Da_Print.Visible then CheckVariableButtons(CommandAction, FVariables);
                if CD_Da_OK.Enabled then
                  CD_Da_OK.ShortCut := TextToShortCut('Enter');
                Result := ShowModal;
              end
            else
              begin
                Variables.CopyFrom(FVariables);
                if QuestionEnabled and FQuestionEnabled then
                  begin
                    Caption := GetTitle('_Dl.Confirmation');
                    paCommandParams.Visible := False;
                    paButtons.Height := Max(tbButtons.Height, tbAdditionals.Height) + 2;
                    Constraints.MinHeight := paHeader.Height + paButtons.Height; //Constraints.MinHeight div 2;
                    ClientWidth := Constraints.MinWidth;
                    ClientHeight := Constraints.MinHeight;
                    BorderStyle := bsSingle;
                    BorderIcons := [];
                    paHeader.Align := alClient;
                    if CD_Da_Print.Visible then CheckVariableButtons(CommandAction, FVariables);
                    if CD_Da_OK.Enabled then
                      CD_Da_OK.ShortCut := TextToShortCut('Enter');

                    ReadParameterValues(FVariables);
                    Variables.CopyFrom(FVariables);
                    Result := ShowModal;
                  end
                else
                  Result := mrOK;
              end;

            if Result = mrOK then
              begin
                if CD_Da_Print.Visible then
                  DeSettings.Variables.AsInteger[RegCommandLastID] := CommandAction.ID;
                ReadParameterValues(FVariables);
                Variables.CopyFrom(FVariables);
                {$IFDEF DEBUG}
                Variables.DebugVariablesLog(ClassName + ' executed ...');
                {$ENDIF}
              end;
          finally
            Free
          end
      else
        begin
          Result := mrAbort;
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar('_eRror.right')));
        end;
    end
  else
    begin
      // Иначе режим выполнения действия печати из списка
      Result := Execute(0, Variables, Mode);
    end;
end;

class function TDeCommandDialog.Execute(const DatasetID: Integer; Variables: TDeVariableList; const Mode: TActionMode): TModalResult;
begin
  Assert(Assigned(Variables), 'Variables is nil!');
  with Self.Create(Application) do
    try
      FActionMode := Mode;
      CD_Da_OK.Visible := False;
      BuildPrinterList(tbPrint);
      lvCommands.Checkboxes := False;
      if DatasetID = 0 then
        Caption := GetTitle('_Dt.Report', ttSecondName)
      else
        Caption := GetTitle('_Da.Print');
      if BuildActionList(DatasetID) <> 0 then
        begin
          SortByCaption;
          //if DatasetID = 0 then SortByCaption else SortByOrder;
          FVariables.CopyFrom(Variables);
          ReadFormSize;
          ReadLastCommandID;
          Result := ShowModal;
          WriteFormSize;
          if Result = mrOK then
            begin
              WriteLastCommandID;
              ReadParameterValues(FVariables);
              Variables.CopyFrom(FVariables);
              {$IFDEF DEBUG}
              Variables.DebugVariablesLog(ClassName + ' executed ...');
              {$ENDIF}
            end;
        end
      else
        Result := mrIgnore;
    finally
      Free;
    end;
end;

procedure TDeCommandDialog.ClearParameterControls;
begin
  FFormDispatcher.DestroyControls;
  FFormDispatcher.Clear;
end;

function TDeCommandDialog.InternalBuildParameterControls(CommandAction: TDeActionData; var ElementTop: Integer): Integer;
var
  Cache: TDataCache;
  N, I, J, ItemCount, OffsetLeft, DefaultColumn: Integer;
  FilterStr, ElementName, ElementValue, ElementCaption: string;
  ElementType: TElementType;
  ElementLink, ElementHidden, ElementColumn, ElementLeft, ElementRight, ElementBottom, AnchorLeft, AnchorRight: Integer;
  DefaultValue: Variant;
  ActionTable, ElementTable: TTableMeta;
  Parser: TDeParser;
  ValuePostfix: TExpressionItem;
  DataCache: TDataCache;
  ControlMeta, LabelMeta: TElementMeta;
  ControlElement, LabelElement: TDeElement;
  aList: TObjectList<TDeElement>;

begin
  Result := 0;
  ActionTable:= Metadata.GetTableMeta(FActionData.DataSet);
  // Сначала инициализируем и дополним параметрами самого действия ...
  Cache := TDataCache.Create(MetaData.MetaTables[idxCommandParams]);
  if Assigned(Cache.TableMeta) then
  try
    Cache.ClearSortList;
    Cache.SortList.Add( Cache.TableMeta.Fields.FindByName(fldCommandParamName).ID );
    // Строка ниже чтобы не разворачивался вертикально столбец ...
    Cache.Fields.FindByName(fldCommandParamHidden).Stage := fsBase;
    Cache.Filters.NewFilter( Cache.TableMeta.Fields.FindByName(fldCommandParamCommand), opEQ, CommandAction.ID);
    Cache.PrepareData;

    aList:= TObjectList<TDeElement>.Create;
    try
      Parser := TDeParser.Create;
      Parser.Table:= ActionTable;
      Parser.onGetVariable :=  CommandGetVariable; /// !!!!!!!!!  Есть сомнения в правильности установки этого Get-тера
      // создаем элементы и их видимые контролы с надписями
      OffsetLeft := 0;
      DefaultColumn := 0;

      // создаем все контролы, У НЕВИДИМЫХ СТАВИМ READONLY = TRUE - как признак что значения не установлено
      for i := 0 to Pred(Cache.Count) do
        begin
          ElementName:= Cache[i].ValueByName[fldCommandParamName];
          FVariables.SetByName(ElementName, unassigned);

          if FFormDispatcher.AllElements.IndexOf(ElementName)=-1 then
            begin
              ElementTable:= nil;
              ElementCaption:= Cache[i].ValueNativeByName[fldCommandParamCaption];
              ElementType:= TElementType(VarToInt(Cache[i].ValueByName[fldCommandParamType]));
              ElementLink:= Cache[i].ValueByName[fldCommandParamDataSet];
              ElementHidden:= Cache[i].ValueByName[fldCommandParamHidden];
              ElementColumn:= Cache[i].ValueByName[fldCommandParamColumn];
              ElementValue:= Cache[i].ValueByName[fldCommandParamValue];

              if (Length(ElementValue)=0) then
                begin
                 if (0<ElementLink) and (ElementLink=FActionData.DataSet) then
                   ElementTable:= ActionTable;
                end
              else
                try
                  ValuePostfix := TExpressionItem.Create;
                  try
                    Parser.Parse(ElementValue, ValuePostfix);
                    if ValuePostfix.Count=1 then
                      if (ValuePostfix.Items[0].ItemType=piIdent) then
                        if (SameText(ValuePostfix.Items[0].Ident, Parser.Table.KField[0].Original)) then
                          begin
                            ElementTable:= ActionTable;
                            ElementValue:= EmptyStr;
                          end;
                  finally
                    ValuePostfix.Free;
                  end;
                except
                  {$IFDEF DEBUG}
                  on E: Exception do
                    DebugLog('Parse value ' + QuotedStr(ElementValue) + ' error: ' + E.Message);
                  {$ENDIF}
                end;
//TODO: доделать колонки
              if ElementType=etGrid then ElementBottom:=10
                                    else ElementBottom:=3;

              ControlMeta:= TElementMeta.Create(ElementTable, ElementCaption, nil, ElementType, ElementLink,
                                                0, 12, 18, 0, ElementTop, ElementBottom);
              ControlMeta.Caption:= ElementCaption;
              ControlMeta.NameInExpr:= ElementName;
              ControlMeta.ExprValue:= ElementValue;
              ControlMeta.ExprFilter:= Cache[i].ValueByName[fldCommandParamFilter];
              // Hidden
              ControlMeta.IsVisible:= ((ElementHidden and 1) = 0);
              // disabled
              // !!! для невидимых строго ставим False, далее используем как флаг расчета скрытых полей
              // !!! т.е. считаем только visible или enabled контролы
              if ControlMeta.IsVisible then ControlMeta.IsReadOnly:= not ((ElementHidden and 2) = 0)
                                       else ControlMeta.IsReadOnly:= True;

              ControlElement:= FFormDispatcher.AddElement(ControlMeta);
              aList.Add(ControlElement);

              ControlElement.Visible:= ControlMeta.IsVisible;
              ControlElement.Enabled:= Not ControlMeta.IsReadOnly;
            end;
        end;

      Parser.onGetIdentType:= GetIdentType;
      Parser.onGetVariable:= CommandGetVariable;
      FFormDispatcher.Calculator.OnGetIdentValue:= GetIdentValue;
      Parser.Table:= nil;

      // ПАРСИМ поле ExprValue с НЕ пустым полем ExprValue
      for i:= 0 to Pred(aList.Count) do
        if (Length(Trim(aList[i].ElementMeta.ExprValue)) > 0) and (aList[i].ElementMeta.ValuePostfix.Count = 0) then
          try
            Parser.Parse(aList[i].ElementMeta.ExprValue, aList[i].ElementMeta.ValuePostfix);
          except
            on E: Exception do
              begin
                {$IFDEF DEBUG}
                DebugLog('InternalBuildParameterControls default value set unassigned and skip error: ' + E.Message);
                {$ENDIF}
                SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo,
                  NativeInt(PChar('Параметр: ' + ElementName + #10 + GetTitle(E.Message, ttFull))));
              end;
          end;

      for i:= 0 to Pred(aList.Count) do
        for j:= Succ(I) to Pred(aList.Count) do
          if //aList[I].ElementMeta.ValuePostfix.DependOnIdent( aList[j].ElementMeta.NameInExpr ) or
             0 < POS( aList[j].ElementMeta.NameInExpr, aList[i].ElementMeta.ExprFilter)
         //        aList[I].ElementMeta.FilterPostfix.DependOnIdent( aList[j].ElementMeta.NameInExpr )
           then
             begin
               aList.Exchange(i, j);
               FFormDispatcher.AllElements.Exchange(i, j);
             end;

      for i:= 0 to Pred(aList.Count) do
        begin
          if aList[i].Visible then
            begin
              aList[i].ElementMeta.ET:= ElementTop;

              LabelMeta:= TElementMeta.Create(nil, aList[i].ElementMeta.Caption, nil, etLabel, -1,
                                                                               0, 0, 0, 18, aList[i].ElementMeta.ET, 3);
              LabelElement:= FFormDispatcher.AddElement(LabelMeta);
              LabelElement.CreateControl(sbCommandParams, nil);

              aList[i].ElementMeta.ET:= ElementTop;

              Inc(ElementTop, aList[i].ElementMeta.EH );
              if ElementType in (EditorElements + [etGrid]) then Inc(Result);
            end;

          aList[i].CreateControl(sbCommandParams, nil);
        end;

      // УСТАНАВЛИВАЕМ КОНТЕКСТНЫЕ значения для ссылочных полей с пустым полем ExprValue
      for i:= 0 to Pred(aList.Count) do
        if (0<aList[i].ElementMeta.Link) then
          begin
            DefaultValue:= null;

            if (Length(Trim(aList[i].ElementMeta.ExprValue)) = 0) and (aList[i].CanMultiselect) then
              begin
                DataCache := MetaData.FindActiveDataCache( aList[i].ElementMeta.Link, True);

                if Assigned(DataCache) then
                  begin
                    if FActionMode = amFocused then
                      begin
                        // Режим выбора только одной записи в фокусе!!!
                        if Assigned(DataCache.FocusedItem) then
                          DefaultValue:= DataCache.FocusedItem.ID
                        else
                          DefaultValue:= null;
                      end
                    else
                      begin
                        ItemCount := DataCache.SelectedCount;
                        if ItemCount = 0 then DefaultValue:= null else
                        if ItemCount = 1 then DefaultValue:= DataCache.SelectedItems[0].ID
                                         else begin
                                                DefaultValue := VarArrayCreate([0, Pred(ItemCount)], varVariant);
                                                for j:= 0 to Pred(ItemCount) do
                                                  VarArrayPut(DefaultValue, DataCache.SelectedItems[j].ID, [j]);
                                              end;
                      end;
                  end
                else
                  begin
                    if MetaData.FindDataSetContext( aList[i].ElementMeta.Link, DefaultValue) then
                      if (FActionMode = amFocused) then
                        if VarIsArray(DefaultValue) then
                          DefaultValue:= DefaultValue[0];
                  end;
              end;

              //проверяем удовлетворение найденного значения текущему фильтру
              if 0 < Length(aList[i].ElementMeta.ExprFilter) then
                begin
                  try
                    if 0 < aList[i].ElementMeta.Link then Parser.Table:= MetaData.GetTableMeta(aList[i].ElementMeta.Link)
                                                     else Parser.Table:= nil;
                    Parser.Parse(aList[i].ElementMeta.ExprFilter, aList[i].ElementMeta.FilterPostfix);
                  except
                    on E: Exception do
                      begin
                        {$IFDEF DEBUG}
                        DebugLog('InternalBuildParameterControls default value set unassigned and skip error: ' + E.Message);
                        {$ENDIF}
                        SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo,
                          NativeInt(PChar('Параметр: ' + ElementName + #10 + GetTitle(E.Message, ttFull))));
                      end;
                  end;
                  if 0 < aList[i].ElementMeta.FilterPostfix.Count then
                    begin
                      if FFormDispatcher.Calculator.Calculate(aList[i].ElementMeta.FilterPostfix) = False then
                        DefaultValue:= unassigned;
                    end;
                end;

              if Not VarIsArray(DefaultValue) then
                if VarIsNull(DefaultValue) or (DefaultValue=unassigned) then
                  MetaData.FindDataSetDefault( aList[i].ElementMeta.Link, DefaultValue);

              if aList[i].Control is TDeDSComboBox then
                begin
                  (aList[i].Control as TDeDSComboBox).ArrayText:= GetTitle('_dV.SelectedCount');
                  (aList[i].Control as TDeDSComboBox).AllowNull:= False;
                  (aList[i].Control as TDeDSComboBox).FilterStr:= aList[i].ElementMeta.ExprFilter;
                  aList[i].InitValue(DefaultValue);
                end else

              if aList[i].Control is TBaseGridForm then
                begin
                  (aList[i].Control as TBaseGridForm).CaptionDirection := cdNone;
                  if assigned((aList[i].Control as TBaseGridForm).DataSetMeta) then
                    if assigned((aList[i].Control as TBaseGridForm).DataSetMeta.Cache) then
                      begin
                        if Length(aList[i].ElementMeta.ExprFilter) <> 0 then
                          try
                            Parser.Table:= aList[i].ElementMeta.Table;
                            Parser.Parse(aList[i].ElementMeta.ExprFilter,
                                         (aList[i].Control as TBaseGridForm).DataSetMeta.FilterPostfix);
                          except
                            on E: Exception do
                              SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo,
                                NativeInt(PChar('Параметр: ' + ElementName + #10 + GetTitle(E.Message, ttFull))));
                          end;

                        (aList[i].Control as TBaseGridForm).DataSetMeta.Cache.PrepareData;
                      end;
                  aList[i].InitValue(DefaultValue);
                end else

                begin
                  aList[i].Value:= DefaultValue;
                end;
          end;

      // РЕКУРСИВНО расчитываем ЗАВИСИМОСТИ от скрытых полей, делаем их enabled
      //
      // ?????????????????????????????????????????????????????????????????????
      //
      // а может и не надо - они сами расчитаются при рекусивном Calculate

      // РАСЧИТЫВАЕМ значения для ВИДИМЫХ и ЗАВИСИМЫХ вычисляемых полей с формулой
      for i:= 0 to Pred(aList.Count) do
        if (0 < aList[i].ElementMeta.ValuePostfix.Count) and (aList[i].Visible or aList[i].Enabled) then
           aList[i].Value:= FFormDispatcher.Calculator.Calculate(aList[i].ElementMeta.ValuePostfix);

      // для неназначенных параметров ставим:
      // а) значения выбранные при предыдущем выполнении команды
      // б) текущую дату для параметров типа etDateTime
      for i:= 0 to Pred(aList.Count) do
        if VarIsEmpty(aList[i].Value) or VarIsNull(aList[i].Value) then
          begin
            N:= MetaData.Parameters.ParamIndex(Format(LastValuesMask,[CommandAction.ID, aList[i].ElementMeta.NameInExpr]));
            if -1 < N then
              aList[i].Value:= MetaData.Parameters[N].Value else

            if aList[i].ElementMeta.ElementType = etDateTime then
              aList[i].Value:= Date;
          end;
(*
          if (ElementHidden and 1) = 0 then
            if Assigned(FFormDispatcher.AllElements.FindByName(ElementName)) then
              begin
                if DefaultColumn = 0 then
                  begin
                    OffsetLeft := 0;
                    DefaultColumn := ElementColumn;
                  end;
                ElementLeft := OffsetLeft + 1;
                ElementRight := OffsetLeft + 0;
                ElementBottom := 3;
                AnchorLeft := OffsetLeft + 0;
                AnchorRight := OffsetLeft + 2;
                if (OffsetLeft = 0) or (Length(ElementCaption) <> 0) then
                  begin
                    ElementType := etLabel;

                    BuildParameterControl(ElementCaption, Null);

                    AnchorLeft := OffsetLeft + 2;
                  end;
                ElementLeft := OffsetLeft + 0;
                ElementRight := OffsetLeft + 0;
                AnchorRight := OffsetLeft + 12 div ElementColumn;
                ElementType := TElementType(VarToInt(Cache[Index].ValueByName[fldCommandParamType]));

                BuildParameterControl(ElementName, DefaultValue);

                NameList.AddObject(ElementName, Pointer(DefaultType));
                if (DefaultColumn <> 0) and (Succ(Index) < Cache.Count) and (ElementColumn = Cache[Succ(Index)].ValueByName[fldCommandParamColumn]) then
                  begin
                    OffsetLeft := AnchorRight;
                    Dec(DefaultColumn);
                    if DefaultColumn = 0 then
                      ElementTop := ElementTop + ElementBottom;
                  end
                else
                  begin
                    ElementTop := ElementTop + ElementBottom;
                    OffsetLeft := 0;
                  end;
                // Если это элемент редактирования, то увеличим счётчик таких контролов ...
                if ElementType in (EditorElements + [etGrid]) then Inc(Result);
              end;
          {$IFDEF DEBUG}
          FParameters.DebugVariablesLog(Format('TDeCommandDialog.InternalBuildParameterControls: Processed %d parameter ...', [Index]));
          {$ENDIF}
        end;
            *)
    finally
      Parser.Free;
    end;

  finally
    Cache.Free;
  end;
  // А потом строим все вложенные параметры ...
  for i:= 0 to Pred(CommandAction.ActionCount) do
    if Assigned(CommandAction.Actions[i]) then
      Result:= Result + InternalBuildParameterControls(CommandAction.Actions[i], ElementTop);
end;

function TDeCommandDialog.BuildParameterControls(CommandAction: TDeActionData): Integer;
var
  ElementTop,i,H : Integer;
  GridControl: TControl;
begin
  {$IFDEF DEBUG}
  FVariables.DebugVariablesLog(Format('TDeCommandDialog.BuildParameterList({%d}, $%p) start ...', [CommandAction.ID, Pointer(FVariables)]));
  {$ENDIF}
  FParameters.Clear;
  ClearParameterControls;
  ElementTop := 0;

  Result := InternalBuildParameterControls(CommandAction, ElementTop);
  GridControl := FindJustOneGrid;
  if Assigned(GridControl) and (GridControl is TBaseGridForm) then
    TDeElement(GridControl.Tag).ElementMeta.EH := (sbCommandParams.Height - (GridControl as TBaseGridForm).Top) div 7 - (TDeElement(GridControl.Tag).ElementMeta.ET + 7);
  ElementTop := RecalculateHeightCommandParams;
  H := paCommandParams.Height;
  if ElementTop > paCommandParams.Height then
    begin
      sbCommandParams.VertScrollBar.Visible:=False;
      paCommandParams.Update;
      lvCommands.Align:=alTop;
      for i := 1 to DeSettings.Variables.AsInteger[RegAnim] do
      begin
        paCommandParams.Height := LiteResize(H, ElementTop, DeSettings.Variables.AsInteger[RegAnim],i,10);
        paCommandParams.Update;
      end;
      lvCommands.Align:=alClient;
      sbCommandParams.VertScrollBar.Visible:=True;
    end;

  {$IFDEF DEBUG}
  FVariables.DebugVariablesLog(Format('TDeCommandDialog.BuildParameterList({%d}, $%p) finish and return %d ...', [CommandAction.ID, Pointer(FVariables), Result]));
  {$ENDIF}
end;

procedure TDeCommandDialog.ReadParameterValues(Variables: TDeVariableList);
var
  Index: Integer;
  DeV: TVariableItem;
begin
  if Assigned(FVariables) and (Variables <> FVariables) then
    for Index:= 0 to Pred(FVariables.Count) do
      Variables.GetByName(FVariables[Index].Name).Assign(FVariables[Index]);

  for Index:= 0 to Pred(FFormDispatcher.AllElements.Count) do
    if FFormDispatcher.AllElements[Index].ElementMeta.ElementType<>etLabel then
      with FFormDispatcher.AllElements[Index] do
        begin
          DeV:= Variables.GetByName(ElementMeta.NameInExpr);
          DeV.DataSetID:= ElementMeta.Link;
          if Not Visible and Not Enabled and (0<ElementMeta.ValuePostfix.Count) then
            begin
              DeV.IsCalculated:= True;
              DeV.Value:= ElementMeta.ExprValue;
//            DeV.OnGetValue:= Variables.GetCalulatedValue;
            end
          else
            begin
              DeV.Value:= Value;
            end;
        end;
end;

procedure TDeCommandDialog.AC_DA_IconsUpdate(Sender: TObject);
begin
  with Sender as TAction do
    begin
      Enabled := MI_Da_View.Visible;
      Checked := Enabled and (Tag = NativeInt(lvCommands.ViewStyle));
    end;
end;

procedure TDeCommandDialog.AC_DA_IconsExecute(Sender: TObject);
begin
  with Sender as TAction do
    lvCommands.ViewStyle := TViewStyle(Tag);
end;

function TDeCommandDialog.BuildActionList(const DatasetID: Integer): Integer;
var
  Index: Integer;
  ActionData: TDeActionData;
begin
  Result := 0;
  try
    if Assigned(ProfitActions) then
      begin
        SetLength(FProfitActions, ProfitActions.Count);
        for Index := 0 to Pred(ProfitActions.Count) do
          begin
            ActionData := ProfitActions[Index];
            if Assigned(ActionData) and ActionData.CheckActionType(atReport) and ActionData.CheckPolicy(spSelect) and ActionData.Active then
              if (DatasetID = 0) or ((DatasetID <> 0) and ActionData.IsSupportDataSet(DatasetID)) then
                begin
                  FProfitActions[Result] := Index;
                  Inc(Result);
                end;
          end
      end;
  finally
    SetLength(FProfitActions, Result);
    lvCommands.Items.Count := Result;
  end;
end;

function TDeCommandDialog.BuildActionList(CommandAction: TDeActionData): Integer;
var
  Index, ItemIndex: Integer;
  ActionData: TDeActionData;
begin
  Result := 0;
  try
    if Assigned(CommandAction) and Assigned(ProfitActions) then
      begin
        SetLength(FProfitActions, CommandAction.ActionCount);
        for Index := 0 to Pred(CommandAction.ActionCount) do
          begin
            ActionData := CommandAction.Actions[Index];
            if Assigned(ActionData) and ActionData.CheckActionType(atReport) and ActionData.CheckPolicy(spSelect) and ActionData.Active then
              begin
                ItemIndex := ProfitActions.IndexOf(ActionData);
                if ItemIndex <> -1 then
                  begin
                    FProfitActions[Result] := ItemIndex;
                    Inc(Result);
                  end;
              end;
          end
      end;
  finally
    SetLength(FProfitActions, Result);
    lvCommands.Items.Count := Result;
  end;
end;

function TDeCommandDialog.GetQuestion: string;
begin
  Result := laQuestion.Caption;
end;

procedure TDeCommandDialog.SetQuestion(const Value: string);
var
  QuestionHeight: Integer;
begin
  if Length(Trim(Value)) = 0 then
    begin
      laQuestion.Caption := EmptyStr;
      laQuestion.Visible := False;
      if paHeader.Align = alTop then
        paHeader.Height := 72 + beBottom.Height;
    end
  else
    begin
      laQuestion.Anchors := [akLeft, akTop];
      laQuestion.Caption := Value;
      laQuestion.AutoSize := True;
      QuestionHeight := laQuestion.Height;
      laQuestion.AutoSize := False;
      laQuestion.SetBounds(laQuestion.Left, laQuestion.Top, laTitle.Width, QuestionHeight);
      laQuestion.Anchors := [akLeft, akTop, akRight, akBottom];
      if paHeader.Align = alTop then
        paHeader.Height := Max(72, laQuestion.Top + QuestionHeight + laTitle.Top) +
          beBottom.Height;
      laQuestion.Visible := True;
    end;
end;

procedure TDeCommandDialog.QuickSort(LowIndex, HighIndex: Integer; OnCompare: TArrayCompareEvent);
var
  I, J, P, V: Integer;
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
          begin
            V := FProfitActions[I];
            FProfitActions[I] := FProfitActions[J];
            FProfitActions[J] := V;
          end;
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

function TDeCommandDialog.CompareByOrder(const LeftIndex, RightIndex: Integer): Integer;
var
  LeftValue, RightValue: Integer;
begin
  LeftValue := ProfitActions[FProfitActions[LeftIndex]].Order;
  RightValue := ProfitActions[FProfitActions[RightIndex]].Order;
  if LeftValue = RightValue then
    Result := 0
  else if LeftValue < RightValue then
    Result := -1
  else
    Result := 1;
end;

procedure TDeCommandDialog.SortByOrder;
begin
  QuickSort(Low(FProfitActions), High(FProfitActions), CompareByOrder);
end;

function TDeCommandDialog.CompareByCaption(const LeftIndex, RightIndex: Integer): Integer;
begin
  Result := AnsiCompareStr(ProfitActions[FProfitActions[LeftIndex]].Caption,
    ProfitActions[FProfitActions[RightIndex]].Caption);
end;

procedure TDeCommandDialog.SortByCaption;
begin
  QuickSort(Low(FProfitActions), High(FProfitActions), CompareByCaption);
end;

function TDeCommandDialog.GetIdentType(const aIdent: string; var aType: TFieldType; aDataObject: pointer): Boolean;
var
  VariableItem: TVariableItem;
  LDataCache: TDataCache;
  LFieldMeta: TFieldMeta;
begin
  Result:= False;

  // ищем в параметрах формы
  VariableItem := FParameters.FindVariable(aIdent);
  if Assigned(VariableItem) then
    begin
      aType:= VariableItem.DataType;
      Exit(True);
    end;

  LDataCache := MetaData.FindActiveDataCache( FActionData.DataSet, True);
  if Assigned(LDataCache) then
    begin
      LFieldMeta:= LDataCache.TableMeta.Fields.FindByName(aIdent, true);
      if Assigned(LFieldMeta) then
        begin
          aType:= LFieldMeta.DataType;
          Inc(FIdentCounter);
          Exit(True);
        end;
    end;
end;

function TDeCommandDialog.GetIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
var
  tempValue: Variant;
  VariableItem: TVariableItem;
  LDataCache: TDataCache;
  LFieldMeta: TFieldMeta;
  i: Integer;
begin
  Result:= False;

  // ищем в параметрах формы
  VariableItem := FParameters.FindVariable(aIdent);
  if Assigned(VariableItem) then
    begin
      aValue:= VariableItem.Value;
      Exit(True);
    end;

  LDataCache := MetaData.FindActiveDataCache( FActionData.DataSet, True);
  if Assigned(LDataCache) then
    begin
      LFieldMeta:= LDataCache.TableMeta.Fields.FindByName(aIdent, true);
      if Assigned(LFieldMeta) then
        begin
          if LDataCache.SelectedCount = 0
            then aValue:= null
            else begin
                   if LDataCache.SelectedItems[0].FieldValueRecurce(aIdent, aValue, dsCodePage) then
                     for i:= 1 to pred(LDataCache.SelectedCount) do
                       if LDataCache.SelectedItems[i].FieldValueRecurce(aIdent, tempValue, dsCodePage) then
                         if aValue <> tempValue then
                           begin
                             aValue:= null;
                             Break;
                           end
                    else
                      aValue:= null;
                 end;
          Exit(True);
        end;
    end;
end;

function TDeCommandDialog.GetVariableItemValue_Control(Sender : TVariableItem): Variant;
var
  P: integer;
  DotName: string;
  Element: TDeElement;
  FCache : TDataCache;
  CI     : TCacheItem;
begin
  Result:= Unassigned;

  P:=pos('.', Sender.Name);
  if 0<P then
    begin
      Element := FindElementByName(Trim(copy(Sender.Name, 1, P-1)));
      DotName := Trim(copy(Sender.Name, P+1, MaxInt))
    end
  else
    begin
      Element := FindElementByName(Sender.Name);
      DotName := EmptyStr;
    end;

  if Not Assigned(Element) then Exit;

  case Element.ElementMeta.ElementType of
    etCheckBox: Result := (Element.Control as TCheckBox).Checked;
    etDateTime: if Element.Control is TDeDateTimePicker then Result:= (Element.Control as TDeDateTimePicker).Value
                                                        else Result:= Element.Value;

    etGrid: if Assigned(Element.Control) and (Element.Control is TBaseGridForm) then
              with TBaseGridForm(Element.Control) do
                if Assigned(DataSetMeta) and Assigned(DataSetMeta.Cache) and Assigned(DataSetMeta.Cache.FocusedItem) then
                  Result := DataSetMeta.Cache.FocusedItem.ID;

    etDefault:
      if Assigned(Element.Control) and (Element.Control is TDeDSComboBox) then
        begin
          if (0 < Length(DotName)) and (0 < Element.ElementMeta.Link) then
            begin
              FCache := Metadata.GetLibrary(Element.ElementMeta.Link);
              if assigned(FCache) then
                begin
                  CI:=FCache.FindById( (Element.Control as TDeDSComboBox).Value );
                  if assigned(CI) then
                    CI.FieldValueRecurce(DotName, Result);
                end
              else
                Result:=null;
            end
          else
            Result:= (Element.Control as TDeDSComboBox).Value;
        end
      else
        Result:= Element.Value

//    etString, etInteger, etFloat: Result := Element.Value;
    else Result:= Element.Value
  end;
end;

function TDeCommandDialog.CommandGetVariable(const aIdent: string; var aVariable: TVariableItem; aDataObject: Pointer): Boolean;
var
  P,i: integer;
  DotName   : string;
  Element   : TDeElement;
begin
  if FFormDispatcher.OnGetVariableItem(aIdent, aVariable, aDataObject) then
    Exit(True);

  Result:=False;
  // ищем контрол

  P:=pos('.', aIdent);
  if 0<P then
    begin
      Element := FindElementByName(Trim(copy(aIdent, 1, P-1)));
      DotName := Trim(copy(aIdent, P+1, MaxInt))
    end
  else
    begin
      Element := FindElementByName(aIdent);
      DotName := EmptyStr;
    end;

  for i:= 0 to Pred(FVariables.Count) do
    if SameText(FVariables[i].Name, aIdent) then
      begin
        // sleep(9);
      end;

  for i:= 0 to Pred(FParameters.Count) do
    if SameText(FParameters[i].Name, aIdent) then
      begin
        // sleep(9);
      end;

  for i:= 0 to Pred(FFormDispatcher.AllElements.Count) do
    if SameText(FFormDispatcher.AllElements[i].ElementMeta.NameInExpr, aIdent) then
      begin
        // sleep(9);
      end;

  if Assigned(Element) then
    begin
      aVariable:= TVariableItem.Create(Element.GetDataType, aIdent, [amRead, amWrite]);
      aVariable.Value:= Element.Value;
      aVariable.OnGetValue:= GetVariableItemValue_Control;
    end;

end;

         {
function TDeCommandDialog.CommandGetVariable(const aIdent: string; var aVariable: TVariableItem; aDataObject: Pointer): Boolean;
var
  P,N       : integer;
  Element   : string;
  DotName   : string;
  DSName, FLDName: string;
  ElementMeta: TElementMeta;
begin
  Result:=False;

  P := pos('.',aIdent);
  if 0 < P then
    begin
      DSName := Trim(copy(aIdent,1,P-1));
      FLDName:= Trim(copy(aIdent,P+1,MaxInt));
      N:= FCaches.IndexOf(DSName);
      if -1 < N then
        begin
          flMeta:= (FCaches.Objects[N] as TDataCache).TableMeta.GetFieldByName(FLDName, True);
          if assigned(flMeta) then
            begin
              aVariable:= TVariableItem.Create(flMeta.DataType, aIdent, [amRead]);
              aVariable.OnGetValue:= GetVariableItemValue_Caches;
              Exit(True);
            end;
        end;

      // ищем в списке параметров
      N:=Params.IndexByName(DSName);
      if (-1<N) then
        if 0 < Params[N].DataSetID then
          begin
            tbMeta:=Metadata.GetTableMeta(Params[N].DataSetID);
            if assigned(tbMeta) then
              begin
                flMeta:= tbMeta.GetFieldByName(FLDName, True);
                if assigned(flMeta) then
                  begin
                    aVariable:= TVariableItem.Create(flMeta.DataType, aIdent, [amRead]);
                    aVariable.OnGetValue:= GetVariableItemValue_ParamCaches;
                    Exit(True);
                  end;
              end;
          end;
    end;

  // ищем целевой файл
  if SameText(aIdent, varTargetFileName) then
    begin
      aVariable:= TVariableItem.Create(ftString, aIdent, [amRead, amWrite]);
      aVariable.OnGetValue:= GetVariableItemValue_TargetFile;
      Exit(True);
    end;

  // ищем в списке переменных
  N:= FVariables.IndexByName(aIdent);
  if -1 < N then
    begin
      aVariable:= TVariableItem.Create(ftUnknown, aIdent, [amRead, amWrite]);
      aVariable.OnGetValue:= GetVariableItemValue_Variables;
      Exit(True);
    end;

  // ищем в списке параметров
  N:= FParams.IndexByName(aIdent);
  if -1 < N then
    begin
      aVariable:= TVariableItem.Create(ftUnknown, aIdent, [amRead, amWrite]);
      aVariable.OnGetValue:= GetVariableItemValue_Params;
      Exit(True);
    end;
end;
{}

procedure TDeCommandDialog.ReadFormSize;
var
  H, W: Integer;
begin
  H := DeSettings.Variables.AsInteger[RegCommandHeight];
  if H < Constraints.MinHeight then
    H := Constraints.MinHeight
  else
    H := Min(H, Application.MainForm.Height - 40);
  W := DeSettings.Variables.AsInteger[RegCommandWidth];
  if W < Constraints.MaxWidth then
    W := Constraints.MaxWidth
  else
    W := Min(W, Application.MainForm.Width - 40);
  Height := H;
  Width := W;
  lvCommands.ViewStyle := TViewStyle(DeSettings.Variables.AsInteger[RegCommandViewStyle]);
end;

procedure TDeCommandDialog.WriteFormSize;
begin
  DeSettings.Variables.AsInteger[RegCommandWidth] := Width;
  DeSettings.Variables.AsInteger[RegCommandHeight] := Height;
  DeSettings.Variables.AsInteger[RegCommandViewStyle] := Integer(lvCommands.ViewStyle);
end;

procedure TDeCommandDialog.CD_Da_ModifyExecute(Sender: TObject);
begin
  if Assigned(lvCommands.Selected) then
    if EditRecord(MetaData.MetaTables[idxCommands], ProfitActions[FProfitActions[lvCommands.Selected.Index]].ID) then
        begin
          ProfitActions[FProfitActions[lvCommands.Selected.Index]].Refresh;
          ActionData:= nil;
          ActionData:= ProfitActions[FProfitActions[lvCommands.Selected.Index]];
        end;
end;

procedure TDeCommandDialog.QuestionUpdate(Action: TDeActionData);
var
  FQuestion: string;
begin
  if Assigned(Action) then FQuestion:= Trim(Action.Question)
                      else FQuestion:= EmptyStr;
  FQuestionEnabled:= Length(FQuestion) <> 0;
  if FQuestionEnabled then Question:= FQuestion
                      else if CD_Da_Preview.Visible then Question:= sQuestionPrint
                                                    else Question:= sQuestionOK;
end;

function TDeCommandDialog.RecalculateHeightCommandParams: Integer;
var
  MinTop, Top, Height, Index: Integer;
  Control: TControl;
begin
  MinTop := -1;
  Top := 0;
  Height := 0;
  for Index := 0 to Pred(sbCommandParams.ControlCount) do
    begin
      Control := sbCommandParams.Controls[Index];
      if Assigned(Control) and Control.Visible then
        begin
          if Control.Top = Top then
            Height := Max(Height, Control.Height)
          else
            if Control.Top > Top then
              begin
                Top := Control.Top;
                Height := Control.Height;
              end;
          if MinTop <> -1 then
            MinTop := Min(Control.Top, MinTop)
          else
            MinTop := Control.Top;
        end;
    end;
  Result := Top + (Height * 2) + Max(MinTop, 0);
end;

procedure TDeCommandDialog.UpdateSizeCommandParams;
var
  Offset: Integer;
begin
  if sbCommandParams.AlignWithMargins then
    Offset := sbCommandParams.Margins.Bottom + sbCommandParams.Margins.Top
  else
    Offset := 0;
 if paCommandParams.Align = alClient then
   begin
     Constraints.MinHeight := RecalculateHeightCommandParams + Offset + paHeader.Height + paButtons.Height;
     ClientHeight := Constraints.MinHeight;
   end
 else
   paCommandParams.Height := RecalculateHeightCommandParams + Offset;
end;

procedure TDeCommandDialog.ReadLastCommandID;
var
  LastID, Index: Integer;
begin
  LastID := DeSettings.Variables.AsInteger[RegCommandLastID];
  for Index := Low(FProfitActions) to High(FProfitActions) do
    if ProfitActions[FProfitActions[Index]].ID = LastID then
      begin
        lvCommands.Selected := lvCommands.Items[Index];
        lvCommands.Selected.MakeVisible(True);
        Exit;
      end;

  if lvCommands.Items.Count <> 0 then
    lvCommands.Selected := lvCommands.Items[0];
end;

procedure TDeCommandDialog.WriteLastCommandID;
var
  LastID: Integer;
begin
  if Assigned(lvCommands.Selected) then
    LastID := ProfitActions[FProfitActions[lvCommands.Selected.Index]].ID
  else
    LastID := -1;
  DeSettings.Variables.AsInteger[RegCommandLastID] := LastID;
end;

function TDeCommandDialog.CheckParameterValues(var Control: TControl): Boolean;
var
  Index: Integer;
  ElementMeta: TElementMeta;
  Element: TDeElement;
  function CheckParameter(const aName: string; const Value: Variant): Boolean;
  begin
    Result := not (VarIsNull(Value) or VarIsEmpty(Value));

    if Length(aName)>0 then
      MetaData.Parameters.SetParam(Format(LastValuesMask,[FActionData.ID, aName]), Value);
  end;
begin
  Result := True;
  Control := nil;
  for Index := 0 to Pred(sbCommandParams.ControlCount) do
    if sbCommandParams.Controls[Index].visible then
      begin
        Element := TDeElement(sbCommandParams.Controls[Index].Tag);
        ElementMeta := Element.ElementMeta;
        case ElementMeta.ElementType of
          etDateTime: { DateTime }
            if Element.Control is TDeDateTimePicker then
              Result := CheckParameter(ElementMeta.NameInExpr, (Element.Control as TDeDateTimePicker).Value)
            else
              Result := CheckParameter(ElementMeta.NameInExpr, Element.Value);
          etDefault: { Other controls }
            if Element.Control is TDeDSComboBox then
              Result := CheckParameter(ElementMeta.NameInExpr, Element.Value);
        end;
        if not Result then
          begin
            Control := sbCommandParams.Controls[Index];
            Break;
          end;
      end;
end;

procedure TDeCommandDialog.CD_Da_ExportExecute(Sender: TObject);
var
  Control: TControl;
begin
  if Length(FExportDirectory) <> 0 then
    begin
      FVariables.SetByName(varPrinterName, FExportDirectory);
      if CheckParameterValues(Control) then
        ModalResult := mrOK
      else
        if Assigned(Control) then
          begin
            if (Control is TWinControl) and (Control as TWinControl).Enabled then
              FocusControl(Control as TWinControl);
            ShowHintWindow(Control, GetTitle('_eRror.SetField'), EmptyStr, icoInfo);
          end
        else
          SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(GetTitle('_eRror.Edit'))));
    end;
end;

function TDeCommandDialog.FindJustOneGrid: TControl;
var
  Index: Integer;
  Control: TControl;
begin
  Result := nil;
  for Index := 0 to Pred(sbCommandParams.ControlCount) do
    begin
      Control := sbCommandParams.Controls[Index];
      if Assigned(Control) and Control.Visible then
        if Control is TBaseGridForm then
          if Assigned(Result) then
            begin
              Result := nil;
              Break;
            end
          else
            begin
              if Index = Pred(sbCommandParams.ControlCount) then
                Result := Control
              else
                Break;
            end;
    end;
end;

function TDeCommandDialog.FindElementByName(const Name: string): TDeElement;
var
  Index: Integer;
  Element: TDeElement;
  ElementMeta: TElementMeta;
begin
  Result := nil;
  for Index := 0 to Pred(sbCommandParams.ControlCount) do
    begin
      Element := TDeElement(sbCommandParams.Controls[Index].Tag);
      ElementMeta := Element.ElementMeta;
      if SameText(ElementMeta.Name, Name) and (ElementMeta.ElementType <> etLabel) then
        begin
          Result := Element;
          Break;
        end;
    end;
end;

function TDeCommandDialog.CheckSingleValueInArray(const Value: Variant): Variant;
var
  FirstValue: Variant;
  Index: Integer;
begin
  Result := Value;
  if VarIsArray(Result) then
    begin
      FirstValue := VarArrayGet(Result, [VarArrayLowBound(Result, 1)]);
      for Index := Succ(VarArrayLowBound(Result, 1)) to VarArrayHighBound(Result, 1) do
        if not DeVarSameValue(FirstValue, VarArrayGet(Result, [Index])) then
          Exit;
      Result := FirstValue;
    end;
end;

procedure TDeCommandDialog.CheckVariableButtons(CommandAction: TDeActionData; Variables: TDeVariableList);
begin
  if Assigned(Variables) then
    begin
      CD_Da_Print.Visible := DeStrToBoolean(VarToStr(Variables.GetValueByName(varButtonPrint)), CD_Da_Print.Visible);
      CD_Da_Preview.Visible := DeStrToBoolean(VarToStr(Variables.GetValueByName(varButtonPreview)), CD_Da_Preview.Visible);
      CD_Da_Export.Visible := DeStrToBoolean(VarToStr(Variables.GetValueByName(varButtonPrint)), False);
      if CD_Da_Print.Visible or CD_Da_Preview.Visible then
        CD_Da_OK.Visible := False
      else
        CD_Da_OK.Visible := True;
      if Assigned(CommandAction) then QuestionUpdate(CommandAction);
    end;
end;

procedure TDeCommandDialog.CD_Da_OKExecute(Sender: TObject);
var
  Control: TControl;
begin
  if CheckParameterValues(Control) then
    ModalResult := mrOK
  else
    if Assigned(Control) then
      begin
        if (Control is TWinControl) and (Control as TWinControl).Enabled then
          FocusControl(Control as TWinControl);
        ShowHintWindow(Control, GetTitle('_eRror.SetField'), EmptyStr, icoInfo);
      end
    else
      SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(GetTitle('_eRror.Edit'))));
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DeCommandDlg unit initialization ...');

finalization
  DebugLog('DeCommandDlg unit finalization ...');
{$ENDIF}

end.

