unit DeVariable;

interface

uses Classes, Contnrs, System.Types, System.SysUtils, Variants,
     DeParser;

type

  /// <summary>класс, описывающий одну переменную</summary>
  {  TDeVariable = class(TObject)
  private
    FName: string;
    FValue: Variant;
    FDataSetID: Integer;
    FDirection: TDeVariableDirection;
    FCalculated: Boolean;
  public
    procedure Assign(aSource: TDeVariable);

    property Name: string read FName;
    property Value: Variant read FValue write FValue;
    property DataSetID: Integer read FDataSetID  write FDataSetID;
    property Calculated: Boolean read FCalculated write FCalculated;
    property Direction: TDeVariableDirection read FDirection write FDirection; // v.17.11
  end;
{}

  TDeVariableListManager = class;

  /// <summary>класс, описывающий список переменных <see cref="TDeVariable" /></summary>
  TDeVariableList = class(TVariableList)
  private
    function InternalCalculateItem(aIndex: Integer): Variant;
    function GetOrCalculateValue(const aIndex: Integer): Variant;
  public
    constructor Create;
    function IndexByName(const Name: string): Integer;
    function TestByName(const aName: string; const aValue: string): Boolean;
    function Calculate(const aName: string; const aDefault: Variant; const aVarType: TVarType = varUnknown): Variant;
    function GetByName(const aName: string; const AutoCreate: Boolean = True): TVariableItem;
    procedure SetByName(const aName: string; const aValue: Variant);
    function GetValueByName(const aName: string): Variant;
    procedure LoadCommaText(const aText: string);
    procedure LoadCommaText2(const aText: string);
    {$IFDEF DEBUG}
    procedure DebugVariablesLog(const Text: string);
    {$ENDIF}
    /// <summary>Метод копирования переменных</summary>
    /// <param name="Source">Источник данных для копирования переменных</param>
    /// <param name="DoClear">True - очищать список перед копированием в него переменных из другого объекта</param>
    procedure CopyFrom(Source: TList; const DoClear: Boolean = True);
    /// <summary>Метод загрузки OUTPUT переменных</summary>
    /// <param name="Source">Источник данных для загрузки переменных</param>
    procedure ReadOutputFrom(aSource: TDeVariableList; const aIndex, aCount: Integer);
    function ExtractArrayList(ConvertArrayMultiOut: Boolean = True): TDeVariableListManager;

    function GetCalulatedValue(aVariable: TVariableItem): Variant;
    function GetVariableItem(const aIdent: string; var aVariable: TVariableItem; aDataObject: Pointer): Boolean;
    function GetIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
    /// <summary>Метод загрузки переменных из XML файла</summary>
    /// <param name="FileName">Имя XML файла</param>
    /// <param name="Defined">Какой узел использовать для загруки: True - Define, False - Override</param>
    procedure LoadFromFile(const FileName: string; const Defined: Boolean);
    /// <summary>Метод сохранения переменных в XML файл</summary>
    /// <param name="FileName">Имя XML файла</param>
    /// <param name="Defined">Какой узел использовать для сохранения: True - Define, False - Override</param>
    procedure SaveToFile(const FileName: string; const Defined: Boolean);
    /// <summary>Метод загрузки переменных из параметров командной строки приложения</summary>
    procedure LoadFromParameters;
  end;

  /// <summary>класс, описывающий список списков переменных <see cref="TDeVariableList" /></summary>
  TDeVariableListManager = class(TObjectList)
  private
    function GetItem(const Index: Integer): TDeVariableList;
    procedure SetItem(const Index: Integer; const Value: TDeVariableList);
  public
    // <summary>Метод перемещает списки из указанного менеджера списков к себе в список</summary>
    procedure ExtractFrom(Source: TDeVariableListManager);
    {$IFDEF DEBUG}
    procedure DebugVariablesListLog(const Text: string);
    {$ENDIF}
    procedure CloneOutputParameters(const SourceIndex, LowIndex, HighIndex: Integer);
    property Items[const Index: Integer]: TDeVariableList read GetItem write SetItem; default;
  end;

implementation

uses XMLDoc, XMLIntf, StrUtils,  Math, Data.DB,
     DeLog, DeTypes, DeMeta, DeMetadata, DeScript, DeCalculator, DataCacheUnit, DeDB, Funcs, QueryDescriptor;

{ TDeVariableList }

constructor TDeVariableList.Create;
begin
  inherited;
end;

function TDeVariableList.IndexByName(const Name: string): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Pred(Count) do
    if SameText(Items[Index].Name, Name) then
      Exit(Index);
end;

function TDeVariableList.GetByName(const aName: string; const AutoCreate: Boolean): TVariableItem;
var i: Integer;
begin
  for i:=0 to Pred(Count) do
    if AnsiCompareText(Items[i].Name, aName)=0 then
      Exit(Items[i]);

  if AutoCreate then
    begin
      Result:= TVariableItem.Create( ftUnknown, aName, [amRead, amWrite] );
      Add(Result);
    end
  else
    Result:=nil;
end;

function  TDeVariableList.GetValueByName(const aName: string): Variant;
var V: TVariableItem;
    pStr: PChar;
begin
  V:=GetByName(aName, False);
  if Assigned(V) then
    begin
      Result := V.Value;
      if (TVarData(Result).VType = varUString) or (TVarData(Result).VType = varString) then
        begin
          pStr:= PChar(String(Result));
          if 1 < Length(pStr) then
            if pStr[0] = pStr[Length(pStr)-1] then
              begin
                if pStr[0] = '''' then Result:= AnsiExtractQuotedStr(pStr,'''') else
                if pStr[0] = '"'  then Result:= AnsiExtractQuotedStr(pStr,'"');
              end;
        end;
    end
  else
    Result := Unassigned; // Result:=varEmpty;  // Ошибка исправлена в v.15.10
end;

procedure TDeVariableList.SetByName(const aName: string; const aValue: Variant);
begin
  if Length(aName)>0 then
    with GetByName(aName) do
      Value := aValue;
end;

function TDeVariableList.TestByName(const aName: string; const aValue: string): Boolean;
var V: Variant;
begin
  V:= ClearName(GetByName(aName).Value);
  Result:= CompareText(String(V),aValue)=0;
end;

function TDeVariableList.InternalCalculateItem(aIndex: Integer): Variant;
var
  Parser: TDeParser;
  Postfix: TExpressionItem;
  Calculator: TDeCalculator;
begin
  Result:= unassigned;
  Parser:= TDeParser.Create;
  Parser.onGetVariable:= GetVariableItem;

  Calculator:= TDeCalculator.Create;
  Calculator.OnGetIdentValue:= GetIdentValue;
  Postfix:= TExpressionItem.Create;

  try
    Parser.Parse(VarToStr(Items[aIndex].Value), Postfix);
    Result:= Calculator.Calculate(Postfix, nil);
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('CalculateValue error: ' + E.Message);
        {$ENDIF}
      end;
  end;

  Postfix.Free;
  Parser.Free;
  Calculator.Free;
end;

function TDeVariableList.GetOrCalculateValue(const aIndex: Integer): Variant;
begin
  if Items[aIndex].IsCalculated then
    begin
      Items[aIndex].Value:= InternalCalculateItem(aIndex);
      Items[aIndex].IsCalculated:= False;
    end;
  Result:= Items[aIndex].Value;
end;

function TDeVariableList.GetCalulatedValue(aVariable: TVariableItem): Variant;
var
  Parser: TDeParser;
  Postfix: TExpressionItem;
  Calculator: TDeCalculator;
begin
  Result:= unassigned;
  Parser:= TDeParser.Create;
  Parser.onGetVariable:= GetVariableItem;

  Calculator:= TDeCalculator.Create;
  Calculator.OnGetIdentValue:= GetIdentValue;
  Postfix:= TExpressionItem.Create;

  try
    Parser.Parse(VarToStr(aVariable.Formula), Postfix);
    Result:= Calculator.Calculate(Postfix, nil);
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('CalculateValue error: ' + E.Message);
        {$ENDIF}
      end;
  end;

  Postfix.Free;
  Parser.Free;
  Calculator.Free;
end;

function TDeVariableList.Calculate(const aName: string; const aDefault: Variant; const aVarType: TVarType): Variant;
var N: Integer;
begin
  N:= IndexByName(aName);
  if N = -1
    then Result:= aDefault
    else if (aVarType=varUnknown) then Result:= InternalCalculateItem(N)
                                  else Result:= VarAsType(InternalCalculateItem(N), aVarType);
end;

procedure TDeVariableList.LoadCommaText(const aText: string);
var Params       : TStringList;
    i            : Integer;
    aName,aValue : String;
begin
  Params := TStringList.Create;
  Params.Delimiter := ';';
  Params.QuoteChar := '"';
  Params.DelimitedText := aText;
//  Params.CommaText := aText;
  for i := 0 to Pred(Params.Count) do
    begin
      aName := Params.Names[i];
      aValue := Params.Values[aName];
      if Length(aName) <> 0 then
        SetByName(aName, Trim(aValue));
//      if Length(aValue)>0 then
//        if aValue[Length(aValue)]=';' then
//          aValue:=Copy(aValue,1,Length(aValue)-1);
//      SetByName(aName,Trim(aValue));
    end;
  Params.Free;
end;

procedure TDeVariableList.LoadCommaText2(const aText: string);
var p  : Integer;
    t  : String;

    procedure DoPart(aText: string);
    var pCh: pChar;
        p: Integer;
    begin
      p:= Pos('=',aText);
      if p<0 then Exit;
      pCh:=PChar(Copy(aText,p+1,MaxInt));

      if Copy(aText,p+1,1)='"' then
        SetByName( Trim(Copy(aText,1,p-1)), AnsiExtractQuotedStr(pCh,'"') ) else
      if Copy(aText,p+1,1)='''' then
        SetByName( Trim(Copy(aText,1,p-1)), AnsiExtractQuotedStr(pCh,'''') ) else
        SetByName( Trim(Copy(aText,1,p-1)), Copy(aText,p+1,MaxInt) );
    end;

begin
  t:=aText;
  p:=pos(';',t);
  while p>0 do
    begin
      DoPart(Copy(t,1,p-1));
      system.Delete(t,1,p);
      p:=pos(';',t);
    end;
  DoPart(t);
end;

function TDeVariableList.GetVariableItem(const aIdent: string; var aVariable: TVariableItem; aDataObject: Pointer): Boolean;
var i: Integer;
begin
  Result:= False;
  for i:= 0 to Pred(Count) do
    if SameText(Items[i].Name, aIdent) then
      begin
        aVariable:= TVariableItem.Create(ftUnknown, aIdent, [amRead, amWrite]);
        aVariable.Assign(Items[i]);
        Result:= True;
        Break;
      end;
end;

function TDeVariableList.GetIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
var
  Index: Integer;
  DataCache: TDataCache;
  FieldMeta: TFieldMeta;
  CacheItem: TCacheItem;
  Dataset: TDeDataset;
begin
  Result := False;

  for Index := 0 to Pred(Count) do
    begin
      DataCache := MetaData.FindActiveDataCache(Items[Index].DataSetID, True);
      if Assigned(DataCache) and Assigned(DataCache.Fields) then
        begin
          FieldMeta := DataCache.Fields.FindByName(aIdent, True);
          if Assigned(FieldMeta) then
            begin
              CacheItem := DataCache.FindByID(Items[Index].Value);
              if Assigned(CacheItem) then
                begin
                  aValue := CacheItem.ValueByName[aIdent];
                  Exit(True);
                end
              else
                if Assigned(FieldMeta.Owner) and Assigned(FieldMeta.Owner.Database) and Assigned(FieldMeta.Owner.KField) and (FieldMeta.Owner.KField.Count = 1) then
                  try
                    Dataset := FieldMeta.Owner.Database.CreateQuery(qtRow);
                    try
                      Dataset.Descr.BeginUpdate;
                      try
                        Dataset.Descr.Table := FieldMeta.Owner.Table;
                        Dataset.Descr.AddField(FieldMeta.Original);
                        Dataset.Descr.AddCondition(FieldMeta.Owner.KField[0].Original, FieldMeta.Owner.KField[0].DataType, opEQ, Items[Index].Value);
                      finally
                        Dataset.Descr.EndUpdate;
                      end;
                      Dataset.Open;
                      aValue:= Dataset.Value[0];
                      Result:= True;
                      Break;
                    finally
                      Dataset.Free;
                    end;
                  except
                    {$IFDEF DEBUG}
                    on E: Exception do
                      DebugLog('%s.GetIdentValue(%s) skip load from dataset error: %s', [ClassName, QuotedStr(aIdent), E.Message]);
                    {$ENDIF}
                  end;
            end;
        end;
    end;
end;

{$IFDEF DEBUG}
procedure TDeVariableList.DebugVariablesLog(const Text: string);
  function PrepareVariableLog(Variable: TVariableItem): string;
  begin
    if Assigned(Variable) then
      begin
        Result := Format(' %*s  | %-40s |', [-(SizeOf(NativeInt) * 2), 'nil', ' ']);
        Result := Format(' %9d | %-48s | %-9s | %-96s |%s', [Variable.DataSetID, Variable.Name, '', VariantToString(Variable.Value), Result])
      end
    else
      Result := Format('%11s|%50s|%11s|%98s|%*s|%42s|', [' ', ' ', ' ', ' ', (SizeOf(NativeInt) * 2) + 3, ' ', ' ']);
  end;
  function PrepareVariablesLog: string;
  var
    IndexSize, Index: Integer;
    Value: string;
  begin
    Result := EmptyStr;
    IndexSize := Length(IntToStr(Count));
    for Index := 0 to Pred(Count) do
      Result := Result + Format(#13#10'                        | %*d. |%s', [
        IndexSize, Index, PrepareVariableLog(Items[Index])]);
    if Length(Result) <> 0 then
      begin
        Value := Format(#13#10'                        +-%s--+%s+%s+%s+%s+%s+%s+', [DupeString('-', IndexSize),
          DupeString('-', 11), DupeString('-', 50), DupeString('-', 11), DupeString('-', 98),
          DupeString('-', (SizeOf(NativeInt) * 2) + 3), DupeString('-', 42)]);
        Result := Value +
          Format(#13#10'                        | %*s | %-9s | %-48s | %-9s | %-96s | %*s | %-40s |',
            [-IndexSize, 'No', 'DataSetID', 'Name', 'Direction', 'Value', -Succ(SizeOf(NativeInt) * 2), 'Ptr', 'Class']) +
          Value + Result + Value;
      end;
  end;
begin
  DebugLog(Text + PrepareVariablesLog);
end;
{$ENDIF}

procedure TDeVariableList.CopyFrom(Source: TList; const DoClear: Boolean);
var
  Index: Integer;
begin
  if DoClear then Clear;
  if Assigned(Source) then
    if Source is TDeVariableList then
      for Index := 0 to Pred(Source.Count) do
        GetByName(TDeVariableList(Source)[Index].Name).assign(TDeVariableList(Source)[Index]);
end;

procedure TDeVariableList.ReadOutputFrom(aSource: TDeVariableList; const aIndex, aCount: Integer);
var
  i: Integer;
  V: Variant;
  aTargetItem: TVariableItem;
begin
  if Assigned(aSource) then
    for i:= 0 to Pred(aSource.Count) do
      if aSource[i].IsOutput then
          begin
            aTargetItem:= GetByName(aSource[i].Name);
            aTargetItem.IsOutput:= aSource[i].IsOutput;
            if aCount < 2 then
              begin
                aTargetItem.Value:= aSource[i].Value;
              end
            else
              begin                  // через переменнную, чтобы не "раздражать" Get-функцию
                V:= aTargetItem.Value;
                if not VarIsArray(V) or  not (aCount = VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1)) then
                  V:= VarArrayCreate([0, Pred(aCount)], varVariant);

                VarArrayPut(V, aSource[i].Value, [aIndex]);
                aTargetItem.Value:= V;
              end;
          end;
end;

function TDeVariableList.ExtractArrayList(ConvertArrayMultiOut: Boolean = True): TDeVariableListManager;

  function VariantToString(const Value: Variant; Brackets: Boolean ): string;
  const
    BooleanNames: array[Boolean] of Char = ('0', '1');
  var
    Index: Integer;
  begin
    if VarIsEmpty(Value) or VarIsNull(Value) then
      Result := EmptyStr
    else if VarIsType(Value, [varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord, varInt64]) then
      Result := VarToStr(Value)
    else if VarIsType(Value, [varBoolean]) then
      Result := BooleanNames[Value = True]
    else if VarIsType(Value, [varDate]) then
      Result := QuotedStr(FormatDateTime('YYYYMMDD" "HH":"NN":"SS"."ZZZ', VarToDateTime(Value)))
    else if VarIsType(Value, [varSingle, varDouble]) then
      Result := ReplaceText(FloatToStr(Value), FormatSettings.DecimalSeparator, '.')
    else if VarIsType(Value, varCurrency) then
      Result := ReplaceText(FormatFloat('#0.00', Value), FormatSettings.DecimalSeparator, '.')
    else if VarIsArray(Value) then
      begin
        Result := EmptyStr;
        for Index := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
          begin
            if Length(Result) <> 0 then Result := Result + ', ';
            Result := Result + VariantToString(VarArrayGet(Value, [Index]), True);
          end;
        if Brackets then Result := '[' + Result + ']';  // верхний уровень без скобок
      end
    else
      Result := QuotedStr(VarToStr(Value));
  end;

  function InternalExtractArrayList(Variables: TDeVariableList): TDeVariableListManager;
  var
    LocalVariables: TDeVariableList;
    LocalManager: TDeVariableListManager;
    ArrayIndex, ItemIndex: Integer;
    Value: Variant;
  begin
    Result:= TDeVariableListManager.Create;
    try
      LocalVariables:= TDeVariableList.Create;
      try
        LocalVariables.CopyFrom(Variables);

        ArrayIndex:= -1;
        for ItemIndex:= 0 to Pred(LocalVariables.Count) do
          if VarIsArray(LocalVariables.GetOrCalculateValue(ItemIndex)) then
            begin
              ArrayIndex:= ItemIndex;
              Break;
            end;

        if ArrayIndex <> -1 then
          begin
            Value:= LocalVariables.GetOrCalculateValue(ArrayIndex);

            for ItemIndex:= VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
              begin
                LocalVariables[ArrayIndex].Value:= VarArrayGet(Value, [ItemIndex]);
                LocalManager:= InternalExtractArrayList(LocalVariables);
                try
                  while LocalManager.Count <> 0 do
                    Result.Add(LocalManager.Extract(LocalManager[0]));

             //     Result.ExtractFrom(LocalManager);
                finally
                  LocalManager.Free;
                end;
              end;
          end
        else
          if Result.Add(LocalVariables) <> -1 then
            LocalVariables := nil;
      finally
        LocalVariables.Free;
      end;
    except
      Result.Free;
      raise;
    end;
  end;

var
  Index: Integer;
  Value: Variant;
begin
  // v.17.10 - Преобразуем в строку параметры с массивом, которые не нужно учитывать при развороте!!!
  if ConvertArrayMultiOut then
    for Index := 0 to Pred(Count) do
      if EndsText('_MULTIOUT', Items[Index].Name) then
        begin
          Value:= GetOrCalculateValue(Index);
          if VarIsArray(Value) then
            if 0 < SizeOf(Value) then Items[Index].Value := VariantToString(Value, False)
                                 else Items[Index].Value := Null;
        end;
  //рекурсивно разворачиваем массив в набор массивов
  Result := InternalExtractArrayList(Self);
end;

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
  function FindAttribute(const AttributeName: string; const Node: IXMLNode): Variant;
  var
    Index: Integer;
  begin
    Result := Unassigned;
    if Assigned(Node) then
      for Index := 0 to Pred(Node.AttributeNodes.Count) do
        if SameText(Node.AttributeNodes[Index].NodeName, AttributeName) then
          begin
            Result := Node.Attributes[Node.AttributeNodes[Index].NodeName];
            Break;
          end;
  end;

procedure TDeVariableList.LoadFromFile(const FileName: string; const Defined: Boolean);
var
  Component: TComponent;
  Document: TXMLDocument;
  ChildNode: IXMLNode;
  Index: Integer;
  Value: Variant;
  TypeName: string;
begin
  try
    Clear;
    Component := TComponent.Create(nil);
    try
      Document := TXMLDocument.Create(Component);
      Document.LoadFromFile(FileName);
      ChildNode := FindNodeByName(SectionName[Defined], Document.ChildNodes.Last.ChildNodes);
      if Assigned(ChildNode) then
        for Index := 0 to Pred(ChildNode.ChildNodes.Count) do
          if SameText(ChildNode.ChildNodes[Index].NodeName, 'VAR') then
            try
              Value := FindAttribute('VALUE', ChildNode.ChildNodes[Index]); // ChildNode.ChildNodes[Index].Attributes['VALUE'];
              TypeName := FindAttribute('TYPE', ChildNode.ChildNodes[Index]); // ChildNode.ChildNodes[Index].Attributes['TYPE'];
              if SameText(TypeName, 'integer') then
                Value := VarAsType(Value, varInteger)
              else if SameText(TypeName, 'word') then
                Value := VarAsType(Value, varWord)
              else if SameText(TypeName, 'byte') then
                Value := VarAsType(Value, varByte)
              else if SameText(TypeName, 'float') then
                Value := VarAsType(Value, varDouble);
              SetByName(FindAttribute('NAME', ChildNode.ChildNodes[Index]){ChildNode.ChildNodes[Index].Attributes['NAME']}, Value);
            except
              {$IFDEF DEBUG}
              on E: Exception do
                DebugLog('TDeVariableList.LoadFromFile skip set variable ' + IntToStr(Index) + ' error: ' + E.Message);
              {$ENDIF}
            end;
    finally
      Component.Free;
    end;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('TDeVariableList.LoadFromFile skip error: ' + E.Message);
    {$ENDIF}
  end;
end;

procedure TDeVariableList.SaveToFile(const FileName: string; const Defined: Boolean);
var
  Component: TComponent;
  Document: TXMLDocument;
  RootNode, ChildNode, Node: IXMLNode;
  Index: Integer;
  TypeName: string;
begin
  try
    Component := TComponent.Create(nil);
    try
      Document := TXMLDocument.Create(Component);
      if FileExists(FileName) then
        Document.LoadFromFile(FileName)
      else
        begin
          Document.XML.Text := '<?xml version="1.0" standalone="yes"?>'#13#10'<DATAPACKET Version="2.0">'#13#10'</DATAPACKET>';
          Document.Active := True;
        end;

      ChildNode := FindNodeByName(SectionName[Defined], Document.ChildNodes.Last.ChildNodes);
      if not Assigned(ChildNode) then
        begin
          ChildNode := Document.CreateNode( SectionName[Defined] );
          Document.ChildNodes.Last.ChildNodes.Insert(0,ChildNode);
        end;

      if Assigned(ChildNode) then
        begin
          ChildNode.ChildNodes.Clear;
          for Index := 0 to Pred(Count) do
            begin
              Node := ChildNode.AddChild('VAR');
              if Assigned(Node) then
                begin
                  Node.Attributes['NAME'] := Items[Index].Name;
                  case VarType(Items[Index].Value) of
                    varShortInt, varSmallint, varInteger: TypeName := 'integer';
                    varSingle, varDouble: TypeName := 'float';
                    varByte: TypeName := 'byte';
                    varWord: typeName := 'word';
                    varEmpty, varNull: TypeName := EmptyStr;
                  else
                    TypeName := 'string';
                  end;
                  if Length(TypeName) <> 0 then Node.Attributes['TYPE'] := TypeName;
                  if not (VarIsNull(Items[Index].Value) or VarIsEmpty(Items[Index].Value)) then
                    Node.Attributes['VALUE'] := Items[Index].Value;
                end;
            end;
          ChildNode := Document.ChildNodes.Last.ChildNodes.Nodes['ROWDATA'] ;

          if ChildNode<>Document.ChildNodes.Last.ChildNodes.Last then
            RootNode.ChildNodes.ReplaceNode(ChildNode, Document.ChildNodes.Last.ChildNodes.Last);


          Document.SaveToFile(FileName);
        end;
    finally
      Component.Free;
    end;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('TDeVariableList.SaveToFile skip error: ' + E.Message);
    {$ENDIF}
  end;
end;

procedure TDeVariableList.LoadFromParameters;
var
  Index, Position: Integer;
  Value, ParamName: string;
  IntValue: Integer;
  ExtValue: Extended;
  DateValue: TDateTime;
  ParamValue: Variant;
begin
  Clear;
  for Index := 1 to ParamCount do
    begin
      Value := ParamStr(Index);
      if (Pos('-', Value) = 1) or (Pos('/', Value) = 1) then
        begin
          System.Delete(Value, 1, 1);
          if Pos('var:', LowerCase(Value)) = 1 then
            begin
              System.Delete(Value, 1, 4);
              Position := Pos('=', Value);
              if Position = 0 then
                begin
                  ParamName := Value;
                  SetByName(ParamName, Null);
                end
              else
                begin
                  ParamName := Copy(Value, 1, Pred(Position));
                  Value := Copy(Value, Succ(Position), Length(Value));
                  if TryStrToInt(Value, IntValue) then
                    ParamValue := IntValue
                  else if TryStrToFloat(Value, ExtValue) then
                    ParamValue := ExtValue
                  else if TryStrToDateTime(Value, DateValue) then
                    ParamValue := DateValue
                  else
                    ParamValue := Value;
                  SetByName(ParamName, ParamValue);
                end;
            end;
        end;
    end;
end;

{ TDeVariableListManager }

function TDeVariableListManager.GetItem(const Index: Integer): TDeVariableList;
begin
  Result := inherited Items[Index] as TDeVariableList;
end;

procedure TDeVariableListManager.SetItem(const Index: Integer; const Value: TDeVariableList);
begin
  inherited Items[Index] := Value;
end;

procedure TDeVariableListManager.ExtractFrom(Source: TDeVariableListManager);
var
  Variables: TObject;
begin
  if Assigned(Source) then
    while Source.Count <> 0 do
      begin
        Variables := Source.Extract(Source[0]);
        try
          if Assigned(Variables) and (Add(Variables) <> -1) then
            Variables := nil;
        finally
          Variables.Free;
        end;
      end;
end;

{$IFDEF DEBUG}
procedure TDeVariableListManager.DebugVariablesListLog(const Text: string);
  function PrepareVariableLog(Variable: TVariableItem): string;
  begin
    if Assigned(Variable) then
      begin
        Result := Format(' %*s  | %-40s |', [-(SizeOf(NativeInt) * 2), 'nil', ' ']);
        Result := Format(' %9d | %-48s | %-9s | %-96s |%s', [Variable.DataSetID, Variable.Name, '', VariantToString(Variable.Value), Result])
      end
    else
      Result := Format('%11s|%50s|%11s|%98s|%*s|%42s|', [' ', ' ', ' ', ' ', (SizeOf(NativeInt) * 2) + 3, ' ', ' ']);

  end;
  function PrepareVariablesLog(const ListIndex, MaxSize: Integer): string;
  var
    Index: Integer;
    List: TDeVariableList;
  begin
    List := Items[ListIndex];
    if Assigned(List) then
      begin
        Result := EmptyStr;
        for Index := 0 to Pred(List.Count) do
          Result := Result + Format(#13#10'                        | %*s |%s', [
            MaxSize, Format('%d.%d.', [ListIndex, Index]), PrepareVariableLog(List[Index])]);
      end
    else
      Result := Format(#13#10'                        | %*s |%s', [
        MaxSize, Format('%d.?.', [ListIndex]), PrepareVariableLog(nil)]);
  end;
  function PrepareVariablesListLog: string;
  var
    Index: Integer;
    MaxSize: Integer;
    Value: string;
  begin
    MaxSize := 0;
    for Index := 0 to Pred(Count) do
      if Items[Index].Count > MaxSize then
        MaxSize := Items[Index].Count;
    MaxSize := Length(IntToStr(MaxSize) + IntToStr(Count)) + 2; // yyyy.xxx.
    Value := Format(#13#10'                        +-%s-+%s+%s+%s+%s+%s+%s+', [DupeString('-', MaxSize),
      DupeString('-', 11), DupeString('-', 50), DupeString('-', 11), DupeString('-', 98),
      DupeString('-', (SizeOf(NativeInt) * 2) + 3), DupeString('-', 42)]);
    Result := EmptyStr;
    for Index := 0 to Pred(Count) do
       begin
         if Length(Result) <> 0 then
           Result := Result + Value;
         Result := Result + PrepareVariablesLog(Index, MaxSize);
       end;
    if Length(Result) <> 0 then
      begin
        Result := Value +
          Format(#13#10'                        | %*s | %-9s | %-48s | %-9s | %-96s | %*s | %-40s |',
          [-MaxSize, 'No', 'DataSetID', 'Name', 'Direction', 'Value', -Succ(SizeOf(NativeInt) * 2), 'Ptr', 'Class']) +
          Value + Result + Value;
      end;
  end;
begin
  DebugLog(Text + PrepareVariablesListLog);
end;
{$ENDIF}

procedure TDeVariableListManager.CloneOutputParameters(const SourceIndex, LowIndex, HighIndex: Integer);
var
  MinIndex, MaxIndex, OutputIndex, Index: Integer;
  List: TDeVariableList;
  Name: string;
  Value: Variant;
begin
  Assert((SourceIndex >= 0) and (SourceIndex < Count), 'Source index ' + IntToStr(SourceIndex) + ' not in range[0..' + IntToStr(Pred(Count)) + ']!');
  List := Items[SourceIndex];
  if Assigned(List) then
    begin
      MinIndex := Max(0, LowIndex);
      MaxIndex := Min(Pred(Count), HighIndex);
      {$IFDEF DEBUG}
      DebugLog('%s.CloneOutputParameters(%d, %d, %d) cloning output for %d to %d ...', [ClassName, SourceIndex, LowIndex, HighIndex, MinIndex, MaxIndex]);
      {$ENDIF}
      for OutputIndex := 0 to Pred(List.Count) do
        if List[OutputIndex].IsOutput then
          begin
            Name := List[OutputIndex].Name;
            Value := List[OutputIndex].Value;
            for Index := MinIndex to MaxIndex do
              if Index <> SourceIndex then
                Items[Index].SetByName(Name, Value);
          end;
      {$IFDEF DEBUG}
      DebugLog('%s.CloneOutputParameters(%d, %d, %d) cloned output for %d to %d ...', [ClassName, SourceIndex, LowIndex, HighIndex, MinIndex, MaxIndex]);
      {$ENDIF}
    end;
end;

end.
