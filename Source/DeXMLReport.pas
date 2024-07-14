unit DeXMLReport;

interface

uses Classes, Contnrs, DB,
     DeTypes, DeMeta, DeReport, DeTemplate, DataCacheUnit, DeVariable, DeParser, DeScript, DeCalculator;

const
  OriginalValue     = '!';
  slrREPORT_TAG     = 'REPORT';
  slrREPORT_OPEN_STR   = '<' +slrREPORT_TAG+'>';
  slrREPORT_CLOSE_STR  = '</'+slrREPORT_TAG+'>';
  slrPARAMS_TAG     = 'PARAMS';
  slrPARAM_TAG      = 'PARAM';
    slrTYPE_ATR     = 'TYPE';
    slrCAPTION_ATR  = 'CAPTION';
    slrFILTER_ATR   = 'FILTER';

  //slrFILEPATH       = 'FILEPATH';
  //slrFILENAME       = 'FILENAME';
  //slrFILEEXT        = 'FILEEXT';
  slrHEADER_TAG     = 'HEADER';
  slrBODY_TAG       = 'BODY';
  slrCODEPAGE_TAG   = 'CODEPAGE';
  slrDECIMALSEP     = 'DECIMALSEPARATOR';
  slrDATASET_TAG    = 'DATASET';
  slrMAKE_TAG       = 'MAKE';
  slrFOR_TAG        = 'FOR';
  slrWHILE_TAG      = 'WHILE';
  slrCASE_TAG       = 'CASE';
  slrMESSAGE_TAG    = 'MESSAGE';
  slrFILTER_TAG     = 'FILTER';
    slrERROR        = 'ERROR';
    slrINFO         = 'INFO';
    slrRESULT       = 'RESULT';
  slrCONDITION_TAG  = 'CONDITION';
  slrOPERATION_TAG  = 'OPERATION';
  slrSEQ_TAG        = 'SEQ';
  slrSORTBY_TAG     = 'SORTBY';
    slrDIRECTION_ATR = 'DIRECTION';
  slrFIELD_TAG      = 'FIELD';
  slrVAR_TAG        = 'VAR';
    slrVALUE_ATR    = 'VALUE';
    slrINC_ATR      = 'INC';
    slrCALC_ATR     = 'CALCULATE';
  slrCOUNT_TAG      = 'COUNT';
  slrDSMAX_TAG      = 'MAX';
  slrDSMIN_TAG      = 'MIN';
  slrDSSUM_TAG      = 'SUM';
  slrDSAVG_TAG      = 'AVG';
    slrVAR_ATR      = 'VAR';
  slrIMAGE_TAG      = 'IMAGE';
  //slrNEXTRECORD_TAG = 'NEXT';
  slrGROUPS_TAG     = 'GROUPS';
  slrGROUP_TAG      = 'GROUP';
  slrNOTE_TAG       = '--';
  slrBLOCK_TAG      = 'BLOCK';
  slrFOOTER_TAG     = 'FOOTER';
  slrDATASET_ATR    = 'DATASET';
  slrRANGE_ATR      = 'RANGE';
  slrNAME_ATR       = 'NAME';
  slrSOURCE_ATR     = 'SOURCE';
  slrRELATION_ATR   = 'RELATION';
  slrFIELD_ATR      = 'FIELD';
  slrFILE_ATR       = 'FILE';
  slrCOMPARETO_ATR  = 'COMPARETO';
  slrOPERATION_ATR  = 'OP';
  slrFORMAT_ATR     = 'FMT';
  slrWIDTH_ATR      = 'WIDTH';
  slrBARCODE_ATR    = 'BARCODE';
  slrLENGTH_ATR     = 'LENGTH';
  slrISNULL_ATR     = 'ISNULL';
  slrBREAK_TAG      = 'BREAK';    // v. 18.3
  slrCONTINUE_TAG   = 'CONTINUE'; // v. 18.3
  slrFILE_TAG       = 'FILE';     // v. 18.3

type
  TLoopStatus =
    (
      lsNone,      // Цикл не определён!
      lsExecuting, // В режиме обработки
      lsBreaking,  // В режиме прерывания
      lsContinuing // В режиме продолжения
    );

  TLoopStack = class
  private
    FList: TObjectList;
    FStatus: TLoopStatus;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Push(const Node: TDeNode): Boolean;
    function Pop(const Node: TDeNode): Boolean;
    function Peek: TDeNode;
    function BreakLoop: Boolean;
    function ContinueLoop: Boolean;
    function ResetLoop: Boolean;
    property Status: TLoopStatus read FStatus;
  end;

  TDeXMLReport = class(TDeReport)
  private
    function ParseParamDef(ParamNode: TDeNode): TDeReportParamDef;
    procedure BuildReplaceList(RootNode: TDeNode);
    function GetCodePage: Integer;
    function GetParametersValue(VariableItem: TVariableItem): Variant;
    function GetVariableItemValue_Caches(Sender : TVariableItem): Variant;
    function GetVariableItemValue_ParamCaches(Sender: TVariableItem): Variant;
    function GetVariableItemValue_TargetFile(Sender: TVariableItem): Variant;
    function GetVariableItemValue_Variables(Sender: TVariableItem): Variant;
    function GetVariableItemValue_Params(Sender: TVariableItem): Variant;
    function GetVariableItemValue_GlobalVariables(Sender: TVariableItem): Variant;
    function ProcessForDatasetTag(NodeValue: TDeNode; const aBody: Boolean): Boolean;
    function ProcessForVarTag(NodeValue: TDeNode; const aBody: Boolean): Boolean;
  protected
    FDeTemplate : TDeTemplate;
    FCodePage   : Integer;
    FDecimalSeparator : Char;

    FParser    : TDeParser;
    FCalc      : TDeCalculator;
    FLoopStack : TLoopStack;
    FVariables  : TDeVariableList;
    FCaches     : TStringList;

    procedure LoadParamDefs; override;

    function GetDataCache(const sDSName: string): TDataCache;
    function GetSequence(const sSequence: string; var vValue: Variant): Boolean;

    function GetVariable(const sParam, sFormat: string; var vValue: Variant): Boolean;
    function GetField(const sField, sFormat: string; const Plain: Boolean; var vValue: Variant): Boolean;
    function ReportGetVariable(const aIdent: string; var aVariable: TVariableItem; aDataObject: Pointer): Boolean;
    function getGlobalFunc(const sParam, sFormat: string; var vValue: Variant): Boolean;

    procedure ProcessTemplate; virtual;
    function ConditionResult(NodeValue: TDeNode): Boolean;
    function ProcessBlockTag(NodeValue: TDeNode; const aBody, aNotify: Boolean): Boolean; virtual;
    function ProcessForTag(NodeValue: TDeNode; const aBody: Boolean): Boolean;
    function ProcessMakeTag(NodeValue: TDeNode): Boolean; virtual;
    function ProcessVarTag(NodeValue: TDeNode): Boolean;
    function ProcessUnknownTag(NodeValue: TDeNode; const aBody: Boolean): Boolean; virtual;
    function ProcessMessageTag(NodeValue: TDeNode): Boolean; virtual;

    function ProcessSeqTag(NodeValue: TDeNode; var vValue: Variant): Boolean;
    function ProcessImageTag(aNodeValue: TDeNode; var aValue: Variant): Boolean;
    function ProcessTextTag(aNodeValue: TDeNode; var aValue: Variant; asText: Boolean = True): Boolean;

    function ProcessDatasetTag(NodeValue: TDeNode): Boolean;
    function ParseCondition(aFilter: TFilterItem; aTable: TTableMeta; NodeValue: TDeNode): Boolean;

    function ProcessWhileTag(NodeValue: TDeNode; const aBody: Boolean): Boolean;
    function ProcessCaseTag(NodeValue: TDeNode; const aBody, aNotify: Boolean): Boolean;

    // v 17.4
    /// <summary>Метод уведомления о изменении целевого имени файла</summary>
    /// <remarks>Должен перекрываться в наследниках для правильной смены имени целевого файла!</remarks>
    procedure ChangeTargetFileName(const OldFileName, NewFileName: string); dynamic;
    procedure SetTargetFileName(const AFileName: string); override;
    // v. 18.3
    function ProcessFileTag(NodeValue: TDeNode; const ABody: Boolean): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    class function IsSupportExtension(const Extension: string): Boolean; override;

    property DeTemplate: TDeTemplate read FDeTemplate;
    procedure SetCodePage(const aValue: Variant);
    procedure SetDecimalSeparator(const aValue: Variant);
    property CodePage: Integer read GetCodePage;
  end;


implementation

uses Windows, Types, SysUtils, Variants, Graphics, Forms,
     DeLog, Funcs, DeMetadata, DeDB,
     {$IFDEF VECTOR_MAP}DeEMFStorage, VectorFormUnit, DeSchema2D, {$ENDIF}
     DataUnit, DeControls, codescan;

constructor TDeXMLReport.Create;
begin
  inherited Create;

  FLoopStack := TLoopStack.Create;
  FDeTemplate:= TDeTemplate.Create;
  FVariables := TDeVariableList.Create;
  FCaches    := TStringList.Create;
  FCodePage  := -1; //1251; // Чтобы при первом обращении определить кодировку!
  FDecimalSeparator := #0;

  FCalc := TDeCalculator.Create;
  FParser := TDeParser.Create;
  FParser.onGetVariable := ReportGetVariable;
end;

destructor TDeXMLReport.Destroy;
begin
  FCalc.Free;
  FParser.Free;

  FDeTemplate.Free;
  FVariables.Free;

  while FCaches.Count > 0 do
    try
      TDataCache(FCaches.Objects[Pred(FCaches.Count)]).Free;
    finally
      FCaches.Delete(Pred(FCaches.Count));
    end;
  FCaches.Free;
  FLoopStack.Free;
  inherited Destroy;
end;

//..............................................................................

procedure TDeXMLReport.LoadParamDefs;
var
  ParamsNode : TDeNode;
  i          : integer;
  ParamDef   : TDeReportParamDef;
begin
  FParamDefs.Clear;
  ParamsNode := FDeTemplate.Root.FindNodeByCode(Code_Header);

  if Assigned(ParamsNode) then
    for i := 0 to ParamsNode.Count - 1 do
      if ParamsNode.ChildNodes[i].Code = Code_Param then
        begin
          ParamDef := ParseParamDef(ParamsNode.ChildNodes[i]);
          if Assigned(ParamDef) then
            FParamDefs.Add(ParamDef);
        end;

  if 0<DeTemplate.ErrorCount then
    SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(DeTemplate.ErrorStrings.Text)));
end;

function TDeXMLReport.ParseParamDef(ParamNode: TDeNode): TDeReportParamDef;
var
  parType : string;
  parName : string;
  parValue: string;
  Logic: Boolean;
begin
  Result := nil;
  parName := ParamNode.Attributes['NAME'];
  if parName = EmptyStr then exit;

  Result := TDeReportParamDef.Create(nil, parName);

//  Result.Top         := StrToIntDef(ParamNode.Attributes['T'],0);
//  Result.Left        := StrToIntDef(ParamNode.Attributes['L'],0);
//  Result.Height      := StrToIntDef(ParamNode.Attributes['H'],3);
//  Result.Width       := StrToIntDef(ParamNode.Attributes['W'],0);
//  Result.AnchorLeft  := StrToIntDef(ParamNode.Attributes['AL'],0);
//  Result.AnchorRight := StrToIntDef(ParamNode.Attributes['AR'],0);
  Result.Caption     := ParamNode.Attributes[slrCAPTION_ATR];
  Result.Filter      := ParamNode.Attributes[slrFILTER_ATR];

  parType := ParamNode.Attributes[slrTYPE_ATR];
  if SameText(parType, 'LABEL') then
    begin
      Result.ParamType := etLabel;
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Parameter ''%s'' type ''%s'' is no longer used for report.', [parName, parType], False, 'Reports')
      {$ENDIF}
    end
  else if SameText(parType, 'DATE') or SameText(parType, 'DATETIME') then
    begin
      Result.ParamType := etDateTime;
      if ParamNode.HasAttribute(slrVALUE_ATR) then
        Result.Value := ParamNode.Attributes[slrVALUE_ATR];
    end
  else if SameText(parType, 'INT') or SameText(parType, 'INTEGER') then
    begin
      Result.ParamType := etInteger;
      Result.Value := ParamNode.Attributes[slrVALUE_ATR];
    end
  else if SameText(parType, 'FLOAT') or SameText(parType, 'MONEY') or SameText(parType, 'BCD')then
    begin
      Result.ParamType := etFloat;
      Result.Value := ParamNode.Attributes[slrVALUE_ATR];
    end
  else if SameText(parType, 'STRING') or SameText(parType, 'VARCHAR')then
    begin
      Result.ParamType := etString;
      Result.Value := ParamNode.Attributes[slrVALUE_ATR];
    end
  else if SameText(parType, 'DBLOOKUPLIST') then
    begin
      Result.ParamType := etDefault;
      Result.Source := ParamNode.Attributes[slrSOURCE_ATR];
      if ParamNode.HasAttribute(slrVALUE_ATR) then
        Result.Value := ParamNode.Attributes[slrVALUE_ATR];
    end
  else if SameText(parType, 'CHECKBOX') then
    begin
      Result.ParamType := etCheckBox;
      // 13.01.2016 + Добавлено в v.15.10 (2016)
      if ParamNode.HasAttribute(slrVALUE_ATR) then
        begin
          parValue := ParamNode.Attributes[slrVALUE_ATR];
          if DeTryStrToBoolean(parValue, Logic) then
            Result.Value := Logic;
        end;
      // 13.01.2016 -
    end
  else
    begin
      {$IFDEF DeDEBUG}
      if Length(Trim(parType)) = 0 then
        Funcs.WriteLog('Parameter ''%s'' contains an unknown type and control will be used by default.', [parName], False, 'Reports')
      else
        Funcs.WriteLog('Parameter ''%s'' contains an unknown type ''%s''. Use type DATE, INT, FLOAT, STRING, DBLOOKUPLIST or CHECKBOX.', [parName, parType], False, 'Reports');
      {$ENDIF}
      // 13.01.2016 * Изменена логика в v.15.10 (2016)
      Result.ParamType := etDefault;
      if ParamNode.HasAttribute(slrVALUE_ATR) then
        begin
          parValue := ParamNode.Attributes[slrVALUE_ATR];
//          if Pos('USER.', UpperCase(parValue)) = 1 then
            Result.Value := parValue
//          else
//            Result.Value := QuotedStr(parValue);
        end;
      {
      Result.Free;
      Result := nil;
      }
      // 13.01.2016 -
      if ParamNode.HasAttribute(slrSOURCE_ATR) then
        Result.Source := ParamNode.Attributes[slrSOURCE_ATR];
    end;
end;

function TDeXMLReport.GetCodePage: Integer;
var
  BodyNode: TDeNode;
begin
  if FCodePage = -1 then
    begin
      BodyNode := DeTemplate.Root.FindNode(slrBODY_TAG);
      if Assigned(BodyNode) then
        begin
          if BodyNode.HasAttribute(slrCODEPAGE_TAG) then
            SetCodePage(BodyNode.Attributes[slrCODEPAGE_TAG]);
        end;
      if FCodePage = -1 then
        FCodePage := 1251;
    end;
  Result := FCodePage;
end;

function TDeXMLReport.GetDataCache(const sDSName: string): TDataCache;
var
  Index: Integer;
begin
  { поиск набора данных }
  Index := FCaches.IndexOf(sDSName);
  if Index <> -1 then
    begin
      Result := FCaches.Objects[Index] as TDataCache;
      if Assigned(Result) and not Result.Active then
        begin
          Result := nil;
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Active cache ''%s'' not found.', [sDSName], False, 'Reports');
          {$ENDIF}
        end;
    end
  else
    begin
      Result := nil;
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Cache ''%s'' not found.', [sDSName], False, 'Reports');
      {$ENDIF}
    end;
end;

function TDeXMLReport.GetSequence(const sSequence: string; var vValue: Variant): Boolean;
var
  Variable: TVariableItem;
begin
  Variable := FVariables.GetByName(sSequence);
  Result := Assigned(Variable);
  if Result then
    begin
      Variable.Value := StrToIntDef(Variable.Value, 0) + 1;
      vValue := Variable.Value;
    end;
end;

//..............................................................................

function TDeXMLReport.GetVariable(const sParam, sFormat: string; var vValue: Variant): Boolean;
var
  Variable: TVariableItem;
begin
  Variable := FVariables.GetByName(sParam, False);
  Result := Assigned(Variable);
  if Result then
    try
      vValue := FormatValue(Variable.Value, sFormat);
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          if Length(Trim(sFormat)) = 0 then
            Funcs.WriteLog('Variable ''%s'' error: %s', [sParam, E.Message], False, 'Reports')
          else
            Funcs.WriteLog('Variable ''%s'' format ''%s'' error: %s', [sParam, sFormat, E.Message], False, 'Reports');
          {$ENDIF}
          {$IFDEF DEBUG}
          DebugLog(ClassName + '.getVariable skip error: ' + E.Message);
          {$ENDIF}
          vValue := EmptyStr;
        end;
    end;
end;

function TDeXMLReport.ProcessSeqTag(NodeValue: TDeNode; var vValue: Variant): Boolean;
begin
  vValue := Unassigned;
  Result := WideSameText(NodeValue.NodeName, slrSEQ_TAG) and
            GetSequence(Utf8ToAnsi(RawByteString(NodeValue.Attributes[slrNAME_ATR])), vValue);
end;

function TDeXMLReport.GetField(const sField, sFormat: string; const Plain: Boolean; var vValue: Variant): Boolean;
var
  P         : integer;
  DSName    : string;
  FLDName   : string;
  Cache     : TDataCache;
  FM        : TFieldMeta;
  v         : Variant;
  F         : Double;
  BackSeparator : Char;
  VariableItem  : TVariableItem;
begin
  Result := false;
  P := pos('.',sField);
  if P <= 0 then Exit;

  DSName  := Trim(copy(sField, 1, P-1));
  FLDName := Trim(copy(sField, P+1, MaxInt));
  Cache := GetDataCache(DSName);

  if not Assigned(Cache) then Exit;

  if SameText(FLDName, '*') then
    begin
      vValue := Cache.Count;
      Result := True;
      Exit;
    end;

  if Cache.FocusedItem <> nil then
    begin
      v := null;
      FM:=Cache.TableMeta.GetFieldByName(FLDName, True);

      if Assigned(FM) then
        begin
          if (sFormat = OriginalValue) then
            Result := Cache.FocusedItem.FieldValueRecurce( FLDName, v) else

          if (FM.DataType in FloatTypes )  then
            begin
              BackSeparator:= FormatSettings.DecimalSeparator;
              if (FDecimalSeparator<>#0) and (FDecimalSeparator<>BackSeparator) then
                 FormatSettings.DecimalSeparator := FDecimalSeparator;

              try
                Cache.FocusedItem.FieldValueRecurce(FLDName, v);
                if not VarIsEmpty(v) and not VarIsNull(v) then
                  begin
                    F:=V;
                    if 0 < Pos('#', sFormat)     then v := FormatFloat(sFormat, F) else
                    if 0 < Length(sFormat)       then v := FormatValue(F, sFormat) else
                    if 0 < Pos('#', FM.Template) then v := FormatFloat(FM.Template, F) else
                    if 0 < Length(FM.Template)   then v := FormatValue(F, FM.Template)
                                                 else v := FloatToStr(F);
                  end;

                Result:=True;
              except
                on E: Exception do
                  begin
                    {$IFDEF DeDEBUG}
                    Funcs.WriteLog('Field ''%s'' error: %s', [sField, E.Message], False, 'Reports');
                    {$ENDIF}
                    {$IFDEF DEBUG}
                    DebugLog(ClassName + '.getField ' + QuotedStr(sField) + ' error: ' + E.Message);
                    {$ENDIF}
                    Result:=False;
                  end;
              end;
              if (FDecimalSeparator<>#0)and (FDecimalSeparator<>BackSeparator) then
                FormatSettings.DecimalSeparator := BackSeparator;
            end else

            begin
              Result := Cache.FocusedItem.FieldValueRecurce(FLDName, v);
              if 0< Length(sFormat) then
                V:= FormatValue(V, sFormat)
              else
                V:= FormatValue(V, FM.Template)
            end;
        end;

      vValue:=v
    end
  else
    begin
      Result := True;
    end;

  // 15.07.2016 + Для хранимых процедур можем искать их параметры!!!
  if not Result then
    begin
      VariableItem := Cache.TableMeta.Parameters.FindVariable(FLDName);
      if (Copy(FLDName, 1, 1) <> '@') and not Assigned(VariableItem) then
        VariableItem := Cache.TableMeta.Parameters.FindVariable('@' + FLDName);
      if Assigned(VariableItem) then
        begin
          vValue := VariableItem.Value;
          Result := True;
        end;
    end;
  // 15.07.2016 -
end;
{
function TDeXMLReport.GetDSCount(const sDataset, sField, sValue, sFormat: string; var vValue: Variant): Boolean;
var
  Cache     : TDataCache;
  aList     : tStringList;
  i,n       : integer;
  iValue    : integer;
  varValue  : Variant;
  FM        : TFieldMeta;
begin
  Result := false;
  vValue := unAssigned;
  Cache  := GetDataCache(sDataset);
  if Cache = nil then exit;

  iValue := 0;

  FM:=nil;
  if Not((sField = EmptyStr) or (sField=Unassigned)) then
    FM := Cache.Fields.FindByName(sField, True);

  //............................................................................
  if FM=nil then
    begin                             // для простого подсчета (поле не указано)
      iValue:=Cache.Count;
    end
  else
    begin                                // для сложного подсчета (поле указано)
      if (sValue=unAssigned) then
        begin                                               // считаем дубликаты
          aList := tStringList.Create;
          n:=Cache.Fields.IndexByName(sField);
          if n>-1 then
            try
              aList.Sorted := true;
              aList.Duplicates := dupIgnore;
                for i := 0 to Cache.Count - 1 do
                  aList.Add(Cache[i].FieldValue[n]);
              iValue := aList.Count;
            finally
              aList.Free;
            end;
        end
      else
        begin                                              // считаем по условию
          for i := 0 to Cache.Count - 1 do
            if Cache[i].FieldValueRecurce(SField, varValue) then
              if VarValue=sValue then
                begin
                  Inc(iValue);
                end;
        end;
    end;

  // обработка результата в текст
  if (Copy(sFormat,1,1)='%') or (Length(sFormat)=0) then
    vValue := FormatValue(iValue,sFormat) // стандартный шаблон для чисел
  else
    begin                                 // метка (флаг) если количестов > 0
      if (iValue>0) then vValue := sFormat
                    else vValue := EmptyStr;
    end;
  Result := true;
end;
{}

function TDeXMLReport.ProcessBlockTag(NodeValue: TDeNode; const aBody, aNotify : Boolean): Boolean;
var i,W      : integer;
    Cache    : TDataCache;
    sField   : string;
    iNode    : TDeNode;
    vValue   : Variant;
    sFormat  : string;
    sValue   : string;
begin
  Result := Assigned(NodeValue);

  if Result then
    begin
      // Если идёт прерывание цикла, то выходим без обработки!!!
      if FLoopStack.Status in [lsBreaking, lsContinuing] then Exit;
      W:=0;
      for I := 0 to NodeValue.Count-1 do
        inc(W, NodeValue.ChildNodes[i].Weight);

      if aNotify then SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, W, 1)
                 else SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, W, NodeValue.Weight);
      try
      W:=0;

      for i := 0 to NodeValue.ChildNodes.Count - 1 do
        begin
          SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, W, 0);
          iNode:= NodeValue.ChildNodes[i];
          Inc(W, iNode.Weight);

          case iNode.Code of
            Code_Block  : if ConditionResult(iNode) then
                            Result := Result and ProcessBlockTag(iNode, aBody, False);
            Code_Case   :   Result := Result and ProcessCaseTag(iNode, aBody, False);
            Code_For:       Result := Result and ProcessForTag(iNode, aBody);
            Code_While:     Result := Result and ProcessWhileTag(iNode, aBody);
            Code_DataSet:   Result := Result and ProcessDatasetTag(iNode);
            Code_Make:    if aBody then
                            Result := Result and ProcessMakeTag(iNode);
            Code_Var:       Result := Result and ProcessVarTag(iNode);
            Code_Message:   Result := Result and ProcessMessageTag(iNode);

            Code_Break: if ConditionResult(iNode) then
                          begin
                            FLoopStack.BreakLoop;
                            Break;
                          end;
            Code_Continue: if ConditionResult(iNode) then
                             begin
                               FLoopStack.ContinueLoop;
                               Break;
                             end;
            Code_File: if not ProcessFileTag(iNode, aBody) then
                         begin
                           Result := False;
                           Break;
                         end;

            Code_Count, Code_Min, Code_Max, Code_Sum, Code_Avg :
                          begin
                            Cache   := GetDataCache(Trim(iNode.Attributes[slrDATASET_ATR]));
                            sField  := Trim(iNode.Attributes[slrFIELD_ATR]);
                            sFormat := iNode.Attributes[slrFORMAT_ATR];
                            if iNode.HasAttribute(slrISNULL_ATR) then vValue := iNode.Attributes[slrISNULL_ATR]
                                                                 else vValue := unAssigned;

                            case iNode.Code of
                              Code_Count:  Result := Result and Cache.Agregate(opCOUNT, sField, vValue);
                              Code_Sum:    Result := Result and Cache.Agregate(opSUM,   sField, vValue);
                              Code_Min:    Result := Result and Cache.Agregate(opMIN,   sField, vValue);
                              Code_Max:    Result := Result and Cache.Agregate(opMAX,   sField, vValue);
                              Code_Avg:    Result := Result and Cache.Agregate(opAVG,   sField, vValue);
                            end;

                            if NodeValue.HasAttribute(slrVAR_ATR) then
                              begin
                                sValue := Trim(NodeValue.Attributes[slrVAR_ATR]);
                                if Length(sValue) = 0 then
                                  begin
                                    {$IFDEF DeDEBUG}
                                    Funcs.WriteLog('Unknown variable in attribute %s for aggregate tag %s ...',
                                      [QuotedStr(slrVAR_ATR), QuotedStr(NodeValue.NodeName)], False, 'Reports');
                                    {$ENDIF}
                                  end
                                else
                                  FVariables.SetByName(sValue, vValue);
                              end;

                            if aBody then
                              begin
                                Result := Result and ProcessMakeTag(iNode);
                              end;
                          end;
            Code_Unknown:   Result := Result and ProcessUnknownTag(iNode, aBody);
          end;
        end;
      finally
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
      end;
    end;
end;

function TDeXMLReport.ProcessCaseTag(NodeValue: TDeNode; const aBody, aNotify: Boolean): Boolean;
var
  Index, Weight: Integer;
  Node: TDeNode;
  {$IFDEF DeDEBUG}
  NonConditionBlockCount, ConditionBlockCount: Integer;
  FirstNonConditionBlock: Boolean;
  {$ENDIF}
begin
  Result := Assigned(NodeValue);

  if Result then
    begin
      Weight := 0;
      {$IFDEF DeDEBUG}
      NonConditionBlockCount := 0;
      ConditionBlockCount := 0;
      FirstNonConditionBlock := False;
      {$ENDIF}
      for Index := 0 to Pred(NodeValue.Count) do
        begin
          Inc(Weight, NodeValue.ChildNodes[Index].Weight);
          {$IFDEF DeDEBUG}
          if NodeValue.ChildNodes[Index].Code = Code_Block then
            if NodeValue.ChildNodes[Index].HasAttribute(slrCONDITION_TAG) then
              Inc(ConditionBlockCount)
            else
              begin
                // Если это первый блок и без условия, то ...
                if (NonConditionBlockCount + ConditionBlockCount) = 0 then
                  FirstNonConditionBlock := True;
                Inc(NonConditionBlockCount);
              end;
          {$ENDIF}
        end;
      {$IFDEF DeDEBUG}
      if FirstNonConditionBlock then
        Funcs.WriteLog('First tag %s in tag %s without condition. It needs to be last in tag %s!!!',
          [QuotedStr(slrBLOCK_TAG), QuotedStr(NodeValue.NodeName), QuotedStr(NodeValue.NodeName)], False, 'Reports')
      else
        if NonConditionBlockCount > 1 then
          Funcs.WriteLog('More than one unconditional block in tag %s. Will always execute only the first!!!',
            [QuotedStr(NodeValue.NodeName)], False, 'Reports')
        else
          if ConditionBlockCount = 0 then
            if NonConditionBlockCount <> 0 then
              Funcs.WriteLog('Use the tag %s without put in tag %s!!!',
                [QuotedStr(slrBLOCK_TAG), QuotedStr(NodeValue.NodeName)], False, 'Reports');
      {$ENDIF}

      if aNotify then SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Weight, 1)
                 else SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Weight, NodeValue.Weight);
      try
        Weight := 0;
        for Index := 0 to Pred(NodeValue.ChildNodes.Count) do
          begin
            SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, Weight, 0);
            Node := NodeValue.ChildNodes[Index];
            Inc(Weight, Node.Weight);
            // Если в CASE теге нашли тэг BLOCK, то ...
            case Node.Code of
              Code_Block: { Блок с условием или без условия }
                begin
                  if ConditionResult(Node) then
                    begin
                      Result := ProcessBlockTag(Node, aBody, False);
                      Break;
                    end;
                end;
              Code_Note: { Блок комметария тупо пропускаем };
            {$IFDEF DeDEBUG}
            else
              if Node.TagType = ttTEXT then
                begin
                  if Length(Trim(Node.Text)) <> 0 then
                    Funcs.WriteLog('Skip unknown text %s in tag %s ...',
                      [QuotedStr(Node.Text), QuotedStr(NodeValue.NodeName)], False, 'Reports')
                end
              else
                Funcs.WriteLog('Skip unknown tag %s in tag %s ...',
                  [QuotedStr(Node.NodeName), QuotedStr(NodeValue.NodeName)], False, 'Reports')

            {$ENDIF}
            end;
          end;
      finally
        SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
      end;
    end;
end;

procedure TDeXMLReport.ProcessTemplate;
var
  HeaderNode, BodyNode  : TDeNode;
  WH,WB: Integer;
begin
  HeaderNode := DeTemplate.Root.FindNodeByCode(Code_Header);
  BodyNode := DeTemplate.Root.FindNodeByCode(Code_Body);

  // Очистым стек циклов ...
  FLoopStack.Clear;

  if assigned(HeaderNode) then WH:= HeaderNode.Weight else WH:=1;
  if assigned(BodyNode) then   WB:= BodyNode.Weight   else WB:=1;

  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, WH+WB, 1);
  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, WH, WH);

  if Assigned(HeaderNode) then
    ProcessBlockTag(HeaderNode, False, False);

  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, WH,  0);
  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, WB, WB);

  if Assigned(BodyNode) then
    begin
      if FCodePage = -1 then
        begin
          if BodyNode.HasAttribute(slrCODEPAGE_TAG) then
            SetCodePage(BodyNode.Attributes[slrCODEPAGE_TAG]);
          if FCodePage = -1 then
            FCodePage := 1251;
        end;
      if BodyNode.HasAttribute(slrDECIMALSEP) then
        SetDecimalSeparator(BodyNode.Attributes[slrDECIMALSEP]);
      BuildReplaceList(BodyNode); // Построим список подмен на основании атрибутов узла BODY ...

      ProcessBlockTag(BodyNode, True, False);
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Tag ''%s'' not found.', [slrBODY_TAG], False, 'Reports');
      {$ENDIF}
    end;

  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
end;

function TDeXMLReport.ProcessVarTag(NodeValue: TDeNode): Boolean;
var aName  : string;
    DeV,DeV2 : TVariableItem;
    aField : String;
    V2     : Variant;
    IntValue: Integer;
    ExtValue: Extended;
    DateTime: TDateTime;
begin
  {$IFDEF DeDEBUG}
  if SameText(NodeValue.NodeName, slrVAR_TAG) and (NodeValue.TagType <> ttSingle) then
    begin
      aName := NodeValue.Attributes[slrNAME_ATR];
      if Length(aName) <> 0 then
        aName := Format(' for variable ''%s''', [aName]);
      Funcs.WriteLog('Tag ''%s''%s contains inner tags!!!', [slrVAR_TAG, aName], False, 'Reports');
    end;
  {$ENDIF}

  aName:= NodeValue.NodeName;
  if WideSameText(aName, slrVAR_TAG) and NodeValue.HasAttribute(slrNAME_ATR) then
    aName := NodeValue.Attributes[slrNAME_ATR];

  if Length((aName))=0 then Exit(True);

  if NodeValue.Calculation=nil then
    try
      NodeValue.Calculation := TExpressionItem.Create;
      if NodeValue.HasAttribute(slrCALC_ATR) then
        FParser.Parse(NodeValue.Attributes[slrCALC_ATR], NodeValue.Calculation );
    except
      on E: EDeParserError do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Variable ''%s'' calculation error: %s', [aName, E.Message], False, 'Reports');
          {$ENDIF}
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(E.Message)));
          Exit(False);
        end;
    end;

  Result:=True;
  DeV:=FVariables.GetByName(aName);

  // Calc Value
  if 0 < NodeValue.Calculation.Count then
    DeV.Value:=FCalc.Calculate(NodeValue.Calculation) else

  // Set Value
  if NodeValue.HasAttribute(slrVALUE_ATR) then
    begin
      aField := Trim(NodeValue.Attributes[slrVALUE_ATR]);
      if TryStrToInt(aField, IntValue) then
        DeV.Value := IntValue
      else if TryStrToFloat(aField, ExtValue) then
        DeV.Value := ExtValue
      else if TryStrToDateTime(aField, DateTime) then
        DeV.Value := DateTime
      else
        DeV.Value := NodeValue.Attributes[slrVALUE_ATR];
    end else

  // Inc Value
  if NodeValue.HasAttribute(slrINC_ATR) then
    begin
      aField :=NodeValue.Attributes[slrINC_ATR];

      if Not getField(aField, OriginalValue, False, V2) then
        begin
          DeV2:=FVariables.GetByName(aField,False);
          if Assigned(DeV2) then
            V2:=DeV2.Value
          else
            V2:=aField;
        end;

      DeV.Value:=ConvertValue(DeV.Value, ftFloat) + ConvertValue(V2, ftFloat);
    end else
  DeV.Value := null;

  if SameText(aName, varTargetFileName) then
    ChangeTargetFileName(TargetFileName, VarToStr(DeV.Value));
end;


function TDeXMLReport.ProcessUnknownTag(NodeValue: TDeNode; const aBody: Boolean): Boolean;
begin
  Result := ProcessVarTag(NodeValue);
end;

function TDeXMLReport.ProcessMakeTag(NodeValue: TDeNode): Boolean;
begin
  Result := False; // По умолчанию не поддерживается!!!
end;

function TDeXMLReport.ProcessMessageTag(NodeValue: TDeNode): Boolean;
var
  TypeName, ValueText: string;
begin
  Result := Assigned(NodeValue);
  if Result then
    if NodeValue.HasAttribute(slrERROR) then
      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(NodeValue.Attributes[slrERROR])))
    else if NodeValue.HasAttribute(slrINFO) then
      SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(NodeValue.Attributes[slrINFO])))
    else if NodeValue.HasAttribute(slrRESULT) then
      SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(NodeValue.Attributes[slrRESULT])))
    else
      begin
        if NodeValue.HasAttribute(slrTYPE_ATR) then
          TypeName := Trim(NodeValue.Attributes[slrTYPE_ATR])
        else
          TypeName := EmptyStr;

        if not Assigned(NodeValue.Calculation) then
          try
            NodeValue.Calculation := TExpressionItem.Create;
            if NodeValue.HasAttribute(slrCALC_ATR) then
              FParser.Parse(NodeValue.Attributes[slrCALC_ATR], NodeValue.Calculation);
          except
            on E: EDeParserError do
              begin
                {$IFDEF DeDEBUG}
                if Length(TypeName) = 0 then
                  Funcs.WriteLog('Message calculation value error: %s', [E.Message], False, 'Reports')
                else
                  Funcs.WriteLog('Message type ''%s'' calculation value error: %s', [TypeName, E.Message], False, 'Reports');
                {$ENDIF}
                SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(E.Message)));
                Exit(False);
              end;
          end;

        if NodeValue.Calculation.Count <> 0 then
          ValueText := FCalc.Calculate(NodeValue.Calculation)
        else
          ValueText := NodeValue.Attributes[slrVALUE_ATR];

        if SameText(TypeName, slrERROR) then
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar(ValueText))) else
        if SameText(TypeName, slrRESULT) then
          SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(ValueText))) else
        if SameText(TypeName, slrINFO) or (Length(TypeName) = 0) then
          SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(PChar(ValueText)))
        else
          begin
            {$IFDEF DeDEBUG}
            Funcs.WriteLog('Unknown message type %s. Skip show message %s.', [QuotedStr(TypeName), QuotedStr(ValueText)], False, 'Reports');
            {$ENDIF}
          end;
      end;
end;

function TDeXMLReport.ConditionResult(NodeValue: TDeNode): Boolean;
begin
  Result:=True;

  if NodeValue.Condition=nil then
    try
      NodeValue.Condition := TExpressionItem.Create;
      if NodeValue.HasAttribute(slrCONDITION_TAG) then
        try
          FParser.Parse(NodeValue.Attributes[slrCONDITION_TAG], NodeValue.Condition );
        except
          on E: Exception do
            begin
              {$IFDEF DeDEBUG}
              Funcs.WriteLog('Parse attribute ''%s'' for tag ''%s'' error: %s', [slrCONDITION_TAG, NodeValue.NodeName, E.Message], False, 'Reports');
              {$ENDIF}
              raise;
            end;
        end;
    except
      on E: EDeParserError do
        begin
          Result:=False;
          {$IFDEF DEBUG}
          NodeValue.Condition.DebugPostfixLog(Format('Dump postfix exception for tag %s parse attribute %s: %s', [QuotedStr(NodeValue.NodeName), QuotedStr(slrCONDITION_TAG), E.Message]));
          {$ENDIF}
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(E.Message )));
          Exit;
        end;
    end;

  if 0 < NodeValue.Condition.Count then
    Result:=FCalc.Calculate(NodeValue.Condition);
end;

function TDeXMLReport.GetVariableItemValue_Caches(Sender : TVariableItem): Variant;
var
  P       : integer;
  DSName  : string;
  FLDName : string;
  Cache   : TDataCache;
begin
  result:= null;

  // ищем в наборах данных
  P := pos('.', Sender.Name);
  DSName:= Trim(copy(Sender.Name, 1, P-1));
  FLDName:= Trim(copy(Sender.Name, P+1, MaxInt));
  Cache := GetDataCache(DSName);

  if Assigned(Cache) then
    if Assigned(Cache.FocusedItem) then
      if Not Cache.FocusedItem.FieldValueRecurce(FLDName, Result) then
        SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar('_eRror.unknownident : '+Sender.Name)))
end;

function TDeXMLReport.GetVariableItemValue_ParamCaches(Sender : TVariableItem): Variant;
var
  P, N    : integer;
  DSName  : string;
  FLDName : string;
  tbMeta  : TTableMeta;
  Cache   : TDataCache;
begin
  result:= null;

  // ищем в наборах данных
  P := pos('.', Sender.Name);
  DSName:= Trim(copy(Sender.Name, 1, P-1));
  FLDName:= Trim(copy(Sender.Name, P+1, MaxInt));
  N:=Params.IndexByName(DSName);

  if -1 < N  then
    if 0 < Params[N].DataSetID then
      begin
        tbMeta:= Metadata.GetTableMeta(Params[N].DataSetID);
        if assigned(tbMeta) then
          begin
            Cache := TDataCache.Create(tbMeta);
            FCaches.AddObject(DSName, Cache);
            Cache.Filters.NewFilter(tbMeta.KField[0], opEQ, Params[N].Value);
            Cache.PrepareData;
            if 0 < Cache.Count then
              Cache.FocusedItem := Cache[0];

            if Assigned(Cache.FocusedItem) then
              if Not Cache.FocusedItem.FieldValueRecurce(FLDName, Result) then
                SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar('_eRror.unknownident : '+Sender.Name)))
          end;
      end;
end;

function TDeXMLReport.GetVariableItemValue_TargetFile(Sender : TVariableItem): Variant;
var
  N: integer;
begin
  N:= FVariables.IndexByName(Sender.Name);
  if (-1 < N) and not VarIsEmpty(FVariables[N].Value)
    then Result:= FVariables[N].Value
    else begin
           N:= FParams.IndexByName(Sender.Name);
           if (-1 < N) then Result:= FParams[N].Value
                       else Result:= TargetFileName;
         end;
end;

function TDeXMLReport.GetVariableItemValue_Variables(Sender : TVariableItem): Variant;
var
  N: integer;
begin
  N:= FVariables.IndexByName(Sender.Name);
  if -1 < N then Result:= FVariables[N].Value
            else Result:= null;
end;

function TDeXMLReport.GetVariableItemValue_GlobalVariables(Sender: TVariableItem): Variant;
var
  V: TVariableItem;
begin
  V := GlobalVariables.FindVariable(Sender.Name);
  if Assigned(V) then
    Result := V.Value
  else
    Result := null;
end;

function TDeXMLReport.GetVariableItemValue_Params(Sender : TVariableItem): Variant;
var
  N: integer;
begin
  N:= Params.IndexByName(Sender.Name);
  if -1 < N then Result:= Params[N].Value
            else Result:= null;
end;

function TDeXMLReport.ReportGetVariable(const aIdent: string; var aVariable: TVariableItem; aDataObject: Pointer): Boolean;
var
  P,N       : integer;
  DSName    : string;
  FLDName   : string;
  tbMeta    : TTableMeta;
  flMeta    : TFieldMeta;
  VI        : TVariableItem;
begin
  Result:=False;

  // ищем в наборах данных
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
      VI := GlobalVariables.FindVariable(aIdent);
      if Assigned(VI) then
        begin
          aVariable := TVariableItem.Create(VI.DataType, aIdent, [amRead]);
          aVariable.OnGetValue := GetVariableItemValue_GlobalVariables;
          Exit(True);
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

class function TDeXMLReport.IsSupportExtension(const Extension: string): Boolean;
begin
  Result := SameText(Extension, sExtensionXML);
end;

function TDeXMLReport.getGlobalFunc(const sParam, sFormat: string; var vValue: Variant): Boolean;
var Postfix : TExpressionItem;
begin
  result:=False;
  Postfix := TExpressionItem.Create;

  try
    FParser.Parse(sParam, Postfix);
  except
    on E: EDeParserError do
      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(E.Message )));
  end;

  if 0<Postfix.Count then
    begin
      try
        vValue := FormatValue(FCalc.Calculate(Postfix), sFormat);
      except
        on E: Exception do
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(E.Message )));
      end;

      result:= True;
    end;

  Postfix.Free;  {}
end;

function TDeXMLReport.ProcessForDatasetTag(NodeValue: TDeNode; const aBody: Boolean): Boolean;
var
  i            : integer;
  Cache        : TDataCache;
  TableName    : string;
begin
  Result := False;
  TableName := Trim(NodeValue.Attributes[slrDATASET_ATR]);
  Cache := GetDataCache(TableName);
  if not Assigned(Cache) then
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Cache for table %s not found ...', [QuotedStr(TableName)], False, 'Reports');
      {$ENDIF}
      Exit;
    end;

  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, Cache.Count, NodeValue.Weight);
  try
    FLoopStack.Push(NodeValue);
    try
      Result := True;
      for i := 0 to Pred(Cache.Count) do
        begin
          SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, i, 0);
          Cache.Items[i].SetFocus;
          if ProcessBlockTag(NodeValue, aBody, True) then
            begin
              // Проверим статус цикла:
              case FLoopStack.Status of
                lsBreaking:
                  // При нахождении вложенного тега прерывания цикла выходим из обработки всех тегов!!!
                  Break;
                lsContinuing:
                  // Если корневой тег и есть тег для продолжения цикла, то ...
                  if FLoopStack.Peek = NodeValue then
                    // Продолжим выполнение ...
                    FLoopStack.ResetLoop;
              end;
            end
          else
            begin
              Result := False;
              Break;
            end;
        end;
    finally
      FLoopStack.Pop(NodeValue);
    end;
  finally
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  end;
end;

function TDeXMLReport.ProcessForVarTag(NodeValue: TDeNode; const aBody: Boolean): Boolean;
var
  VariableName: string;
  VariableValue: Variant;
  Count, Index, Position: Integer;
begin
  Result := False;
  VariableName := Trim(NodeValue.Attributes[slrVAR_ATR]);
  if Length(VariableName) = 0 then
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Empty attribute %s for tag %s ...', [QuotedStr(slrVAR_ATR), QuotedStr(NodeValue.NodeName)], False, 'Reports');
      {$ENDIF}
    end
  else
    begin
      VariableValue := FVariables.GetValueByName(VariableName);
      if VarIsArray(VariableValue) then
        begin
          Count := Succ(VarArrayHighBound(VariableValue, 1) - VarArrayLowBound(VariableValue, 1));
          case Count of
            0: Result := True;
            1: { Обработка одного элемента значения без цикла }
              begin
                FVariables.SetByName(VariableName, VarArrayGet(VariableValue, [VarArrayLowBound(VariableValue, 1)]));
                Result := ProcessBlockTag(NodeValue, aBody, True);
              end;
          else
            SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, Count, NodeValue.Weight);
            try
              FLoopStack.Push(NodeValue);
              try
                Result := True;
                Position := 0;
                for Index := VarArrayLowBound(VariableValue, 1) to VarArrayHighBound(VariableValue, 1) do
                  begin
                    FVariables.SetByName(VariableName, VarArrayGet(VariableValue, [Index]));
                    SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, Position, 0);
                    if ProcessBlockTag(NodeValue, aBody, True) then
                      begin
                        // Проверим статус цикла:
                        case FLoopStack.Status of
                          lsBreaking:
                            // При нахождении вложенного тега прерывания цикла выходим из обработки всех тегов!!!
                            Break;
                          lsContinuing:
                            // Если корневой тег и есть тег для продолжения цикла, то ...
                            if FLoopStack.Peek = NodeValue then
                              // Продолжим выполнение ...
                              FLoopStack.ResetLoop;
                        end;
                      end
                    else
                      begin
                        Result := False;
                        Break;
                      end;
                    Inc(Position);
                  end;
              finally
                FLoopStack.Pop(NodeValue);
              end;
            finally
              SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
            end;
          end;
        end
      else
        Result := ProcessBlockTag(NodeValue, aBody, True);
    end;
end;

function TDeXMLReport.ProcessForTag(NodeValue: TDeNode; const aBody: Boolean): Boolean;
begin
  if NodeValue.HasAttribute(slrDATASET_ATR) then
    Result := ProcessForDatasetTag(NodeValue, aBody)
  else
    if NodeValue.HasAttribute(slrVAR_ATR) then
      Result := ProcessForVarTag(NodeValue, aBody)
    else
      begin
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Attributes %s or %s not found for tag %s ...', [QuotedStr(slrDATASET_ATR), QuotedStr(slrVAR_ATR), QuotedStr(NodeValue.NodeName)], False, 'Reports');
        {$ENDIF}
        Result := False;
      end;
end;

function TDeXMLReport.ProcessWhileTag(NodeValue: TDeNode; const aBody: Boolean): Boolean;
begin
  ConditionResult(NodeValue);
  Result := Assigned(NodeValue.Condition) and (NodeValue.Condition.Count <> 0);

  if Not Result then
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Error attribute %s for tag %s loop ...', [QuotedStr(slrCONDITION_TAG), QuotedStr(NodeValue.NodeName)], False, 'Reports');
      {$ENDIF}
      Exit(False);
    end;
  FLoopStack.Push(NodeValue);
  try
    while ConditionResult(NodeValue) do
      if ProcessBlockTag(NodeValue, aBody, True) then
        begin
          // Проверим статус цикла:
          case FLoopStack.Status of
            lsBreaking:
              // При нахождении вложенного тега прерывания цикла выходим из обработки всех тегов!!!
              Break;
            lsContinuing:
              // Если корневой тег и есть тег для продолжения цикла, то ...
              if FLoopStack.Peek = NodeValue then
                // Продолжим выполнение ...
                FLoopStack.ResetLoop;
          end;
        end
      else
        begin
          Result := False;
          Break;
        end;
  finally
    FLoopStack.Pop(NodeValue);
  end;
end;

function TDeXMLReport.ProcessImageTag(aNodeValue: TDeNode; var aValue: Variant): Boolean;
var
  sField     : String;
  Cache      : TDataCache;
  {$IFDEF VECTOR_MAP}
  FGraphic   : TGraphic;
  aSchema    : TDeSchema;
  deEMF      : TdeEMFStorage;
  deDSMeta   : TdeDSMetaStorage;
  {$ENDIF}

  DeImage   : TDeImage;
  P         : integer;
  FMeta     : TFieldMeta;
  DSName    : string;
  FLDName   : string;
  BitMap    : TBitMap;
  TagBarCode: string;
  sFormat: String;
  sValue: AnsiString;
  BarCodeType : Integer;
  BarCode: TBarCode;

function StringIsDigits(Value: String): Boolean;
var i: Integer;
begin
  Result:= True;
  for i:= 1 to Length(Value) do
    if Not CharInSet(Value[i], ['0'..'9']) then
      Exit(False);
end;

begin
  Result:= False;

  aValue:= unassigned;
  sField:= aNodeValue.NodeName;

  if Length(sField)>0 then
    begin
      P := pos('.',sField);
      if P <= 0 then
        begin
          // значение - берем значение для штрих кода
          DSName  := EmptyStr;
          FLDName := Trim(sField);
          sFormat:= '';{Utf8ToAnsi}(aNodeValue.Attributes[slrFORMAT_ATR]);

          Result:=
              { ProcessAggregateTag(aNodeValue, aValue)
            or {}
               ProcessSeqTag(aNodeValue, aValue)
            or getField(aNodeValue.NodeName, sFormat, false, aValue)
            or getParam(aNodeValue.NodeName, sFormat, aValue)
            or getVariable(aNodeValue.NodeName, sFormat, aValue)
            or getGlobalParam(aNodeValue.NodeName, sFormat, aValue)
            or getGlobalVariable(aNodeValue.NodeName, sFormat, aValue)
            or getGlobalFunc(aNodeValue.NodeName, sFormat, aValue);

          if not result then Exit;
        end
      else
        begin
          DSName  := Trim(copy(sField,1,P-1));
          FLDName := Trim(copy(sField,P+1,MaxInt));

          Cache := GetDataCache(DSName);
          if Not (Cache = nil) then
            if (Cache.FocusedItem<>nil) then
              begin
                FMeta:=Cache.Fields.FindByName(FLDName,True);

                // строка - берем значение для штрих кода
                if (FMeta.DataType in StringTypes)  then
                  begin
                    Cache.FocusedItem.FieldValueRecurce(FLDName, aValue);
                  end else

                // картинка - вставляем как есть
                if (FMeta.DataType = ftBLOB)  then
                  begin
                    DeImage:= TDeImage.Create(nil);
                    try
                      DeImage.Value:=Cache.FocusedItem.ValueByName[FLDName];
                      if Assigned(DeImage.Graphic) and not DeImage.Graphic.Empty then
                        begin
                          aValue := DeGetTempFileName('','DEI', sExtensionBMP);
                          DeImage.SaveToBitmap(aValue);
                          DM.RegisterTemporaryFile(aValue);
                         end;
                     Finally
                     end;
                   DeImage.Free;
                   Exit(True);
                 end;

              end;
        end;

      TagBarCode:= aNodeValue.Attributes[slrBARCODE_ATR];
      sValue:= aValue;
      if Length(sValue)>0 then
        begin
          if Length(TagBarCode)=0
            then if StringIsDigits(sValue)
                    then if (Length(sValue) = 13) then TagBarCode:='EAN13' else
                         if (Length(sValue) = 8) then  TagBarCode:='EAN8' else
                                                       TagBarCode:='25'
                    else                               TagBarCode:='39';

          if SameText(TagBarCode, 'QR')    then BarCodeType:= bc_QR else
          if SameText(TagBarCode, 'EAN8')  then BarCodeType:= bc_EAN8 else
          if SameText(TagBarCode, 'EAN13') then BarCodeType:= bc_EAN13 else
          if SameText(TagBarCode, 'UPCA')  then BarCodeType:= bc_UPCA else
          if SameText(TagBarCode, 'UPCE')  then BarCodeType:= bc_UPCE else
          if SameText(TagBarCode, '25')    then BarCodeType:= bc_25 else
          if SameText(TagBarCode, '39')    then BarCodeType:= bc_39 else
          if SameText(TagBarCode, '93')    then BarCodeType:= bc_93 else
          if SameText(TagBarCode, '93A')   then BarCodeType:= bc_93A else
          if SameText(TagBarCode, '128A')  then BarCodeType:= bc_128A else
          if SameText(TagBarCode, '128B')  then BarCodeType:= bc_128B else
          if SameText(TagBarCode, '128C')  then BarCodeType:= bc_128C else
          if SameText(TagBarCode, 'BAR')   then BarCodeType:= bc_BAR else
                                                BarCodeType:= bc_None;

          if BarCodeType <> bc_None then
            begin
              BarCode:= TBarCode.Create;
              BarCode.SetCode(BarCodeType, sValue);

              aValue := DeGetTempFileName('','DEI', sExtensionBMP);
              DM.RegisterTemporaryFile(aValue);

              BarCode.SaveToFile(aValue);
              BarCode.Free;
            end;
        end;
      Exit;
    end;

  //------------------------------- формируем как схему из набора данных ---
  {$IFDEF VECTOR_MAP}
  sDataset := Trim(NodeValue.Attributes[slrDATASET_ATR]);
  Cache  := GetDataCache(sDataset);
  if assigned(Cache) and (Cache.Count>0) then
    begin
      FGraphic := TMetaFile.Create;
      TMetaFile(FGraphic).Enhanced := true;

      aSchema:=TDeSchema.Create;

      deDSMeta:=TdeDSMetaStorage.Create(Cache);
      deDSMeta.Load( aSchema );

      deEMF:=TdeEMFStorage.Create(TMetaFile(FGraphic));
      if NodeValue.HasAttribute('info') then
        aSchema.PrepareInfo;
      deEMF.Save(aSchema);

      aValue := DeGetTempFileName('','DEI', sExtensionEMF);
      FGraphic.SaveToFile(aValue);
      DM.RegisterTemporaryFile(aValue);

      deDSMeta.Free;
      deEMF.Free;
      aSchema.Free;
      FGraphic.Free;

      Result:=True;
    end;
  {$ENDIF}
end;

function TDeXMLReport.ProcessTextTag(aNodeValue: TDeNode; var aValue: Variant; asText: Boolean = True): Boolean;
var  Cache   : TDataCache;
     sField  : string;
     sFormat : string;
     sValue  : string;
     nLength : Integer;
begin
  Result:= False;
  aValue:= EmptyStr;
  if not Assigned(aNodeValue) then Exit;

  if 0 = Length(aNodeValue.NodeName) then
    begin
       aValue := aNodeValue.Text;
    end
  else
    try
      if not ConditionResult(aNodeValue) then
        begin
          Result:= True;
          aValue:= EmptyStr // не выполняется условие - ничего не выводим
        end
      else
        begin
          sFormat:= {Utf8ToAnsi}(aNodeValue.Attributes[slrFORMAT_ATR]);

          Result:=
              { ProcessAggregateTag(aNodeValue, aValue)
            or {}
               ProcessSeqTag(aNodeValue, aValue)
            or getField(aNodeValue.NodeName, sFormat, false, aValue)
            or getParam(aNodeValue.NodeName, sFormat, aValue)
            or getVariable(aNodeValue.NodeName, sFormat, aValue)
            or getGlobalParam(aNodeValue.NodeName, sFormat, aValue)
            or getGlobalVariable(aNodeValue.NodeName, sFormat, aValue)
            or getGlobalFunc(aNodeValue.NodeName, sFormat, aValue);

          if Not Result then
            begin
              Cache   := GetDataCache(Trim(aNodeValue.Attributes[slrDATASET_ATR]));
              sField  := Trim(aNodeValue.Attributes[slrFIELD_ATR]);
              if aNodeValue.HasAttribute(slrISNULL_ATR) then
                aValue := aNodeValue.Attributes[slrISNULL_ATR];

              case aNodeValue.Code of
                Code_Count:  Result := Cache.Agregate(opCOUNT, sField, aValue);
                Code_Sum:    Result := Cache.Agregate(opSUM,   sField, aValue);
                Code_Min:    Result := Cache.Agregate(opMIN,   sField, aValue);
                Code_Max:    Result := Cache.Agregate(opMAX,   sField, aValue);
                Code_Avg:    Result := Cache.Agregate(opAVG,   sField, aValue);
              end;
            end;

          {$IFDEF DeDEBUG}
          if not Result then
            Funcs.WriteLog('Field, parameter or variable %s not found ...', [QuotedStr(aNodeValue.NodeName)], False, 'Reports');
          {$ENDIF}
          if asText or VarIsType(aValue, [varString, varOleStr, varUString]) or (0<Length(sFormat)) then
          begin
            if VarIsNull(aValue) then sValue := EmptyStr
                                 else sValue := aValue;

            sValue := ReplaceList.Replace(sValue); // Замена символов по шаблону

            if Length(sValue)=0 then
              if aNodeValue.HasAttribute(slrISNULL_ATR) then
                sValue := aNodeValue.Attributes[slrISNULL_ATR];

            if aNodeValue.HasAttribute(slrLENGTH_ATR) then
              begin
                nLength := StrToIntDef(aNodeValue.Attributes[slrLENGTH_ATR],0);
                if nLength > Length(sValue) then
                   sValue := sValue +  StringOfChar(' ', nLength - Length(sValue)) else
                if (nLength<0) and (abs(nLength) > Length(sValue)) then
                   sValue := StringOfChar(' ', abs(nLength) - Length(sValue)) + sValue else
                if 0 < abs(nLength) then
                   sValue := Copy(sValue, 1, abs(nLength));
              end;
            aValue := sValue;
          end;
        end;
    except
      on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog(ClassName + '.ProcessTextTag error: ' + E.Message);
          {$ENDIF}
          Result := False;
        end;
    end;
end;

//..............................................................................

function TDeXMLReport.ProcessDatasetTag(NodeValue: TDeNode): Boolean;
var
  i          : integer;
  Name       : string;
  NodeFilter : TDeNode;
  NodeCond   : TDeNode;
  aFilter    : TFilterItem;
  NodeOrder  : TDeNode;
  anOrder    : TFieldMeta;
  Cache      : TDataCache;
  tbMeta     : TTableMeta;
  CacheNum   : Integer;
  OldCacheMeta : Boolean;

  FExpr, Source  : String;
  FFilterPostfix : TExpressionItem;
  FFilterItem    : TFilterItem;
  Direction: TDSSortDirection;
  DirectionText: string;
  Database: TDeCustomDatabase;
begin
  Result := false;

  if not(NodeValue.HasAttribute(slrNAME_ATR)) then
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Attribute ''%s'' not found for tag ''%s'' ...', [slrNAME_ATR, slrDATASET_TAG], False, 'Reports');
      {$ENDIF}
      Exit;
    end;
  Name   := NodeValue.Attributes[slrNAME_ATR];

  if not(NodeValue.HasAttribute(slrSOURCE_ATR)) then
    begin
      Result := True;
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Attribute ''%s'' not found for tag ''%s'' ...', [slrSOURCE_ATR, slrDATASET_TAG], False, 'Reports');
      {$ENDIF}
      Exit;
    end;

  Source := NodeValue.Attributes[slrSOURCE_ATR];
  tbMeta := MetaData.GetTableByPath(Source);
  if not Assigned(tbMeta) then
    tbMeta := MetaData.GetTableByFullName(Source);
  if not Assigned(tbMeta) then
    begin
      FExpr := Source;
      I := Pos('.', FExpr);
      if I = 0 then
        begin
          Source := MetaData.MetadataDB.Alias;// EmptyStr;
        end
      else
        begin
          Source := Copy(FExpr, 1, Pred(I));
          FExpr := Copy(FExpr, Succ(I), Length(FExpr));
        end;
      Database := MetaData.DatabaseByAlias(Source);
      {$IFDEF DeDEBUG}
      if not Assigned(Database) then
        Funcs.WriteLog('Database %s not found ...', [QuotedStr(Source)], False, 'Reports');
      {$ENDIF}

      tbMeta := MetaData.GetDynamicTableByName(FExpr, Database);
      if Assigned(tbMeta) and (tbMeta.ObjectType = otStoredProc) then
        tbMeta.Parameters.SetGetValueMethod(GetParametersValue);
    end;

  if not Assigned(tbMeta) then
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Table %s not found ...', [QuotedStr(NodeValue.Attributes[slrSOURCE_ATR])], False, 'Reports');
      {$ENDIF}
      Exit;
    end;

  Result := true;
  CacheNum := FCaches.IndexOf(Name);

  if CacheNum >= 0 then
    begin
      Cache := TDataCache(FCaches.Objects[CacheNum]);
      OldCacheMeta:= Cache.TableMeta=tbMeta;

      if Not(OldCacheMeta) then
        try
          Cache.Free;
          FCaches.Delete(CacheNum);

          Cache := TDataCache.Create(tbMeta);
          FCaches.AddObject(Name,Cache);
        except
          {$IFDEF DEBUG}
          on E: Exception do
            DebugLog(ClassName + '.ProcessDatasetTag skip error: ' + E.Message);
          {$ENDIF}
        end

    end
  else
    begin
          OldCacheMeta:= False;
          Cache := TDataCache.Create(tbMeta);
          FCaches.AddObject(Name,Cache);
    end;

  //обработать условия
  Cache.Filters.Clear;

  try
    if NodeValue.HasAttribute(slrFILTER_TAG) then
      begin
        FParser.Table := tbMeta;

        FExpr := NodeValue.Attributes[slrFILTER_TAG];
        FFilterPostfix:= TExpressionItem.Create;
        try
          FParser.Parse( FExpr, FFilterPostfix);
          {$IFDEF DEBUG}
          FFilterPostfix.DebugPostfixLog(Format('Report dataset filter for %s (%s [%d]): %s ...',
            [QuotedStr(Name), QuotedStr(FParser.Table.Table), FParser.Table.ID, QuotedStr(FExpr)]));
          {$ENDIF}
        except
          on E: Exception do
            begin
              {$IFDEF DEBUG}
              DebugLog(ClassName + '.ProcessDatasetTag skip error: ' + E.Message);
              {$ENDIF}
              {$IFDEF DeDEBUG}
              Funcs.WriteLog('Filter %s for %s error: %s', [QuotedStr(FExpr), QuotedStr(Source), E.Message], False, 'Reports');
              {$ENDIF}
            end;
        end;

        if FFilterPostfix.Count>0 then
          begin
            FFilterItem := TFilterItem.Create;
            FFilterItem.Assign(FFilterPostfix);
            Cache.Filters.Add(FFilterItem);
          end;

        FFilterPostfix.Free;
        FParser.Table:=nil;
      end
    else
      begin
        NodeFilter := NodeValue.FindNode(slrFILTER_TAG);

        if Assigned(NodeFilter) then
          begin
            NodeCond:=NodeValue.FindNode(slrOPERATION_TAG);
            if Not Assigned(NodeCond) then
              NodeCond:=NodeValue.FindNode(slrCONDITION_TAG);

            if Assigned(NodeCond) then
              begin
                aFilter := TFilterItem.Create;
                ParseCondition(aFilter, Cache.TableMeta, NodeCond);
                if aFilter.Count > 0 then Cache.Filters.AddFilter(aFilter)
                                     else aFilter.Free;
              end;
          end;
      end;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog(ClassName + '.ProcessDatasetTag skip error: ' + E.Message);
    {$ENDIF}
  end;

  //обработать сортировки
  Cache.ClearSortList;
  try
    if NodeValue.HasAttribute(slrSORTBY_TAG) then
      if not Cache.SortList.AddFields(Cache.TableMeta.Fields, NodeValue.Attributes[slrSORTBY_TAG], sdAscending) then
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Sort field %s not found ...', [QuotedStr(NodeValue.Attributes[slrSORTBY_TAG])], False, 'Reports');
          {$ENDIF}
        end;

    NodeOrder := NodeValue.FindNode(slrSORTBY_TAG);
    if Assigned(NodeOrder) then
      for i := 0 to NodeOrder.ChildNodes.Count - 1 do
        if WideSameText(NodeOrder.ChildNodes[i].NodeName, slrFIELD_TAG) then
          if (NodeOrder.ChildNodes[i].HasAttribute(slrNAME_ATR)) then
            begin
              anOrder := Cache.TableMeta.Fields.FindByName
                              (NodeOrder.ChildNodes[i].Attributes[slrNAME_ATR]);
              if Assigned(anOrder) then
                begin
                  Direction := sdNone;
                  if NodeOrder.ChildNodes[i].HasAttribute(slrDIRECTION_ATR) then
                    begin
                      DirectionText := NodeOrder.ChildNodes[i].Attributes[slrDIRECTION_ATR];
                      if SameText(DirectionText, 'ASC') then
                        Direction := sdAscending
                      else if SameText(DirectionText, 'DESC') then
                        Direction := sdDescending
                      else
                        begin
                          {$IFDEF DeDEBUG}
                          Funcs.WriteLog('Unknown direction ''%s'' for ''%s''.', [DirectionText, anOrder.Original]);
                          {$ENDIF}
                        end;
                    end;
                  Cache.SortList.Add(anOrder.ID, Direction);
                end;
            end;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog(ClassName + '.ProcessDatasetTag skip error: ' + E.Message);
    {$ENDIF}
  end;

  try
    if OldCacheMeta then
      Cache.Update(mcNone, Null, ltFirst)
    else
      Cache.PrepareData(True, fsFull);

    if 0 < Cache.Count then
      Cache.FocusedItem := Cache[0];
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog(ClassName + '.ProcessDatasetTag skip error: ' + E.Message);
    {$ENDIF}
  end;
end;

function TDeXMLReport.ParseCondition(aFilter: TFilterItem; aTable: TTableMeta; NodeValue: TDeNode): Boolean;
var
  sName, sOperation, sConst : string;
  vValue     : Variant;
  i          : integer;
  Next       : Boolean;
  Operation  : TOperationType;
  FMeta      : TFieldMeta;
begin
  Result:=False;
  if NodeValue.TagType = ttTEXT then Exit;

  if WideSameText(NodeValue.NodeName, slrCONDITION_TAG) then
    begin
       Result:=True;
      if NodeValue.HasAttribute(slrFIELD_ATR   )  then FMeta := aTable.Fields.FindByName(NodeValue.Attributes[slrFIELD_ATR])
                                                  else FMeta := aTable.KField[0];

      if NodeValue.HasAttribute(slrRELATION_ATR)  then
        begin
          sOperation := NodeValue.Attributes[slrRELATION_ATR];
          if CompareText(sOperation, 'IS')=0       then Operation := opIs     else
          if CompareText(sOperation, 'EQ')=0       then Operation := opEQ     else
          if CompareText(sOperation, 'NE')=0       then Operation := opNE     else
          if CompareText(sOperation, 'LT')=0       then Operation := opLT     else
          if CompareText(sOperation, 'LE')=0       then Operation := opLE     else
          if CompareText(sOperation, 'GT')=0       then Operation := opGT     else
          if CompareText(sOperation, 'GE')=0       then Operation := opGE     else
          if CompareText(sOperation, 'IN')=0       then Operation := opIn     else
          if CompareText(sOperation, 'LIKE')=0     then Operation := opLike
          else
            begin
              Operation := opEQ;
              {$IFDEF DeDEBUG}
              Funcs.WriteLog('Unknown value ''%s'' for attribute ''%s''. Use default value ''EQ''.', [sOperation, slrRELATION_ATR], False, 'Reports');
              {$ENDIF}
            end;
        end
      else
        begin
          Operation := opEQ;
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Attribute ''%s'' not found. Use default value ''EQ''.', [slrRELATION_ATR], False, 'Reports');
          {$ENDIF}
        end;

      if NodeValue.HasAttribute(slrCOMPARETO_ATR) then sConst := NodeValue.Attributes[slrCOMPARETO_ATR]
                                                  else sConst := EmptyStr;

      if Not Assigned(FMeta)   then SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25,
                                      NativeInt(PChar('_Error.unknownident+ "'+NodeValue.Attributes[slrFIELD_ATR]+'"'))) else
      if Operation = opNone    then {ничего не делаем} else
      if Operation = opIs  then aFilter.AddCondition(FMeta, opIs, null) else
      if Operation = opIsNot then aFilter.AddCondition(FMeta, opIsNot, null) else
      if Operation = opIsTrue  then aFilter.AddCondition(FMeta, opEQ, True) else
      if Operation = opIsFalse then aFilter.AddCondition(FMeta, opEQ, False) else
        begin
          if (pos(':',sConst) = 1) then
            begin
              sName := copy(sConst,2,Length(sConst));
              if getField(sName, OriginalValue, true, vValue) then else
              if getParam(sName, EmptyStr, vValue)          then else
              if getVariable(sName, EmptyStr, vValue)       then else
              if getGlobalParam(sName, EmptyStr, vValue)    then else
              if getGlobalVariable(sName, EmptyStr, vValue) then else
              if getGlobalFunc(sName, EmptyStr, vValue)     then else
                 vValue := sConst;
            end
          else
            vValue := sConst;

          if (varIsEmpty(vValue) or varIsNULL(vValue)) then
            begin
              if (Operation = opEQ) then
                aFilter.AddCondition(FMeta, opIs, null)
              else
                SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25,
                                      NativeInt(PChar('_Error.argtype+ : '+FMeta.Original+' '+ OperationLexeme[Operation]+' NULL')))
            end
          else
            aFilter.AddCondition(FMeta, Operation, vValue);
        end;
    end else

  if WideSameText(NodeValue.NodeName, slrOPERATION_TAG) then
    if (NodeValue.HasAttribute(slrOPERATION_ATR)) then
      begin
        Result:=True;
        sOperation := UpperCase(NodeValue.Attributes[slrOPERATION_ATR]);
        if (sOperation = 'AND') then
          Operation := opAnd
        else if (sOperation = 'OR') then
          Operation := opOr
        else
          begin
            Operation := opNone;
            {$IFDEF DeDEBUG}
            Funcs.WriteLog('Unknown value ''%s'' for attribute ''%s''.', [sOperation, slrOPERATION_ATR], False, 'Reports');
            {$ENDIF}
          end;

        Next:=False;
        if Operation <> opNone then
          for i := 0 to NodeValue.ChildNodes.Count-1 do
            if not (NodeValue.ChildNodes[i].TagType = ttTEXT) then
              if ParseCondition(aFilter, aTable, NodeValue.ChildNodes[i]) then
                begin
                 if Next then aFilter.AddOperation(Operation);
                 Next := True;
                end;
      end;
end;

procedure TDeXMLReport.SetCodePage(const aValue: Variant);
var
  S: string;
  I: Integer;
begin
  if VarIsType(aValue, [varString, varUString]) then
    begin
      S := aValue; // VarAsType(aValue, varString);
      if ( CompareText(S, 'DOS') = 0 ) or
         ( CompareText(S, 'CP866') = 0 ) or
         ( CompareText(S, '866') = 0 ) then
         FCodePage := 866
      else if SameText(S, 'UTF-8') or SameText(S, 'UTF8') then
        FCodePage := 0
      else if SameText(S, 'WINDOWS-1251') or SameText(S, 'WINDOWS1251') then
        FCodePage := 1251
      else
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Unknown codepage ''%s''.', [S], False, 'Reports');
          {$ENDIF}
        end;
    end
  else if VarIsType(aValue, [varInteger, varWord, varSmallInt, varLongWord]) then
    begin
      I := aValue;
      case I of
        866, 1251: { OEM, ANSI }
          FCodePage := I;
        cpUTF8: { UTF-8 }
          FCodePage := 0;
      else
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Unknown codepage %d.', [I], False, 'Reports');
        {$ENDIF}
      end;
    end;
end;

procedure TDeXMLReport.SetDecimalSeparator(const aValue: Variant);
var
  S: string;
begin
  if VarIsType(aValue, varString) or VarIsType(aValue, varUString) then
    begin
      S := aValue; //VarAsType(aValue, varString);

      if ( CompareText(S, 'dot') = 0 )   then FDecimalSeparator:='.'  else
      if ( CompareText(S, 'comma') = 0 ) then FDecimalSeparator:=','  else
      if Length(S)=1                         then FDecimalSeparator:=S[1] else
                                                  FDecimalSeparator:=#0;
    end;
end;

procedure TDeXMLReport.SetTargetFileName(const AFileName: string);
begin
  inherited SetTargetFileName(AFileName);
  FVariables.SetByName(varTargetFileName, TargetFileName);
end;

procedure TDeXMLReport.BuildReplaceList(RootNode: TDeNode);
var
  Index: Integer;
  Name, Value: string;
  function TranslateText(const Text: string): string;
  var
    Value: string;
    Index: Integer;
  begin
    Result := EmptyStr;
    Value := Text;
    if (Length(Value) <> 0) and (Value[1] = '$') then
      Delete(Value, 1, 1);
    while Length(Value) <> 0 do
      begin
        Index := Pos('$', Value);
        if Index = 0 then
          begin
            Result := Result + Chr(StrToInt('$' + Value));
            Value := EmptyStr;
          end
        else
          begin
            Result := Result + Chr(StrToInt('$' + Copy(Value, 1, Pred(Index))));
            Value := Copy(Value, Succ(Index), Length(Value));
          end;
      end;
  end;
begin
  // Очистим список подмен ...
  ReplaceList.Clear;
  // Если указан узел и у него есть атрибуты, то ...
  if Assigned(RootNode) and Assigned(RootNode.NodeAttributes) then
    for Index := 0 to Pred(RootNode.NodeAttributes.Count) do
      begin
        // Получим имя атрибута ...
        Name := Trim(RootNode.NodeAttributes[Index].Name);
        // Если это специальный атрибут подмены, то ...
        if Pos('$', Name) = 1 then
          begin
            // Получим "что" менять ...
            Name := TranslateText(Name);
            // Получим "на что" менять ...
            Value := RootNode.NodeAttributes[Index].Value;
            // Если требуется конвертация в символы, то ...
            if Pos('$', Value) = 1 then
              try
                Value := TranslateText(Value);
              except
                // При ошибке ничего не делаем
              end;
            // Добавим в список подмен ...
            ReplaceList.Add(Name, Value);
          end;
      end;
end;

function TDeXMLReport.GetParametersValue(VariableItem: TVariableItem): Variant;
var
  VariableName: string;
begin
  Result := Unassigned;
  if Assigned(VariableItem) then
    begin
      VariableName := VariableItem.Name;
      if Params.IndexByName(VariableName) <> -1 then
        Result := Params.GetValueByName(VariableName)
      else
        if Copy(VariableName, 1, 1) = '@' then
          begin
            Delete(VariableName, 1, 1);
            if Params.IndexByName(VariableName) <> -1 then
              Result := Params.GetValueByName(VariableName)
            else
              if Assigned(FVariables) and (FVariables.IndexByName(VariableName) <> -1) then
                Result := FVariables.GetValueByName(VariableName);
          end
        else
          begin
            VariableName := '@' + VariableName;
            if Params.IndexByName(VariableName) <> -1 then
              Result := Params.GetValueByName(VariableName);
          end;
    end;
end;

procedure TDeXMLReport.ChangeTargetFileName(const OldFileName, NewFileName: string);
begin
  if OldFileName <> NewFileName then
    begin
      TargetFileName := NewFileName;
      {$IFDEF DEBUG}
      DebugLog('%s.ChangeTargetFileName(%s, %s) completed ...', [ClassName, QuotedStr(OldFileName), QuotedStr(NewFileName)]);
      {$ENDIF}
    end;
end;

function TDeXMLReport.ProcessFileTag(NodeValue: TDeNode; const ABody: Boolean): Boolean;
var
  FieldName, DatasetName: string;
  DataCache: TDataCache;
  FieldMeta: TFieldMeta;
  Image: TDeImage;
  FileName, Directory: string;
  Index: Integer;
begin
  Result := Assigned(NodeValue);
  if Result then
    begin
      FieldName := Trim(NodeValue.Attributes[slrFIELD_ATR]);
      if Length(FieldName) = 0 then
        begin
          Result := False;
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Tag %s is empty attribute %s.', [QuotedStr(NodeValue.NodeName), QuotedStr(slrFIELD_ATR)], False, 'Reports');
          {$ENDIF}
        end
      else
        begin
          {$IFDEF DeDEBUG}
          if SameText(NodeValue.NodeName, slrFILE_TAG) and (NodeValue.TagType <> ttSingle) then
            Funcs.WriteLog('Tag ''%s'' contains inner tags!!!', [NodeValue.NodeName], False, 'Reports');
          {$ENDIF}
          Index := Pos('.', FieldName);
          if Index = 0 then
            DatasetName := EmptyStr
          else
            begin
              DatasetName := Trim(Copy(FieldName, 1, Pred(Index)));
              FieldName := Trim(Copy(FieldName, Succ(Index), Length(FieldName)));
            end;
          DataCache := GetDataCache(DatasetName);
          if Assigned(DataCache) then
            begin
              FieldMeta := DataCache.Fields.FindByName(FieldName, True);
              if Assigned(FieldMeta) then
                if (FieldMeta.DataType = ftBLOB) or (FieldMeta.DataType in StringTypes) then
                  begin
                    if Assigned(DataCache.FocusedItem) then
                      begin
                        Image := TDeImage.Create(nil);
                        try
                          Image.Value := DataCache.FocusedItem.ValueByName[FieldName];
                          Params.SetByName(varBlobFileName, Image.OriginalFileName);
                          if not Assigned(NodeValue.Condition) then
                            begin
                              NodeValue.Condition := TExpressionItem.Create;
                              if NodeValue.HasAttribute(slrFILE_ATR) then
                                try
                                  FParser.Parse(NodeValue.Attributes[slrFILE_ATR], NodeValue.Condition);
                                except
                                  on E: Exception do
                                    begin
                                      {$IFDEF DeDEBUG}
                                      Funcs.WriteLog('Parse attribute ''%s'' for tag ''%s'' error: %s', [slrFILE_ATR, NodeValue.NodeName, E.Message], False, 'Reports');
                                      {$ENDIF}
                                      {$IFDEF DEBUG}
                                      NodeValue.Condition.DebugPostfixLog(Format('Dump postfix exception for tag %s parse attribute %s: %s', [QuotedStr(NodeValue.NodeName), QuotedStr(slrFILE_ATR), E.Message]));
                                      {$ENDIF}
                                      NodeValue.Condition.Clear;
                                    end;
                                end;
                            end;
                          if NodeValue.Condition.Count = 0 then
                            FileName := Image.OriginalFileName
                          else
                            FileName := FCalc.Calculate(NodeValue.Condition);
                          Params.SetByName(varBlobFileName, Null);
                          Directory := ExtractFilePath(FileName);
                          if Length(Directory) = 0 then Directory := ExtractFilePath(TargetFileName);
                          if Length(Directory) <> 0 then Directory := IncludeTrailingBackslash(Directory);
                          FileName := ExtractFileName(FileName);
                          if Length(FileName) = 0 then
                            FileName := NormalizeFileName(VariantToString(DataCache.FocusedItem.ID)) + sExtensionDAT;
                          FileName := Directory + FileName;
                          if ABody then Image.SaveToFile(FileName);
                        finally
                          Image.Free;
                        end;
                      end;
                  end
                else
                  begin
                    Result := False;
                    {$IFDEF DeDEBUG}
                    Funcs.WriteLog('Field %s in dataset %s unknown type.', [QuotedStr(FieldName), QuotedStr(DatasetName)], False, 'Reports');
                    {$ENDIF}
                  end
              else
                begin
                  Result := False;
                  {$IFDEF DeDEBUG}
                  Funcs.WriteLog('Field %s in dataset %s not found.', [QuotedStr(FieldName), QuotedStr(DatasetName)], False, 'Reports');
                  {$ENDIF}
                end;
            end
          else
            begin
              Result := False;
              {$IFDEF DeDEBUG}
              Funcs.WriteLog('Dataset %s not found.', [QuotedStr(DatasetName)], False, 'Reports');
              {$ENDIF}
            end;
        end;
    end;
end;

{ TLoopStack }

constructor TLoopStack.Create;
begin
  FList := TObjectList.Create(False);
end;

destructor TLoopStack.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TLoopStack.Clear;
begin
  if Assigned(FList) then FList.Clear;
end;

function TLoopStack.Push(const Node: TDeNode): Boolean;
begin
  Result := Assigned(Node);
  if Result then
    begin
      FList.Insert(FList.Count, Node);
      FStatus := lsExecuting;
    end;
end;

function TLoopStack.Pop(const Node: TDeNode): Boolean;
begin
  Result := Peek = Node;
  if Result then
    Result := FList.RemoveItem(Node, FromEnd) <> -1;
  if Result then ResetLoop;
end;

function TLoopStack.Peek: TDeNode;
begin
  if FList.Count <> 0 then
    Result := FList[Pred(FList.Count)] as TDeNode
  else
    Result := nil;
end;

function TLoopStack.BreakLoop: Boolean;
begin
  Result := Assigned(Peek());
  if Result then FStatus := lsBreaking;
end;

function TLoopStack.ContinueLoop: Boolean;
begin
  Result := Assigned(Peek());
  if Result then FStatus := lsContinuing;
end;

function TLoopStack.ResetLoop: Boolean;
begin
  Result := FStatus in [lsBreaking, lsContinuing];
  if Result then
    if FList.Count = 0 then
      FStatus := lsNone
    else
      FStatus := lsExecuting;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DeXMLReport unit initialization ...');

finalization
  DebugLog('DeXMLReport unit finalization ...');
{$ENDIF}

end.

