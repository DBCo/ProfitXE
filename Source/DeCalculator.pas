unit DeCalculator;

interface

uses Windows, SysUtils, DB, Contnrs, Classes, DateUtils, {Messages, }
     DeTypes, DeParser, DeFunctions;

type

  TStackItem = record
    Value    : Variant;
    CodePage : Integer;
    DataType : TFieldType;
    Postfix  : TPostfixItem; // указатель на TPostfixItem, который создал элемент стека
    IsValue  : Boolean;
    function IsOperation(aOperation: TOperationType): Boolean;
    procedure Assign(aSource: TStackItem);
  end;

  /// <summary>стек, используемый для вычисления выражений</summary>
  TPostfixStack = class
  private
    FStack : array of TStackItem;
    FIndex : integer;
    function GetSize: integer; inline;
    procedure SetSize(const Value: integer); inline;
    function GetStackItem(aIndex: Integer): TStackItem; inline;
  public
    constructor Create;
    destructor Destroy;  override;
    property Index : Integer read FIndex;
    property Items[aIndex : integer]: TStackItem read GetStackItem;  default;
    property Size : integer read GetSize write SetSize;
    function Push(aItem: TPostfixItem; aValue: Variant; aDataType: TFieldType; aCodePage: Integer = -1): TStackItem;
    function PushSQL(aItem: TPostfixItem; aValue : String; aDataType: TFieldType = ftUnknown; aCodePage: Integer = -1): TStackItem;
    function Pop : TStackItem;
    function Peek : TStackItem;
    procedure Clear;
  end;

  TTableToStrEvent = function(const tableName: String; const tableAlias: String = ''; const tableDatabase: String = '';
                              const tableSchema: String = ''): string of object;
  TFieldToStrEvent = function(const fieldName: String; const fieldAlias: String = ''; const tableAlias: String = '';
                              const fieldOperation: TOperationType = opNone; const DataType: TFieldType = ftUnknown; const DataSize: Integer = 0): string of object;
  TConstToStrEvent = function(const aValue: Variant; const aType: TFieldType): string of object;
  TFuncToStrEvent = function(aFunction: string; aParams: array of variant): string of object;

  { вычислитель выражений }
  /// <summary>вычислитель выражений</summary>
  TDeCalculator = class
  private
    FStack           : TPostfixStack;
    FDataObject      : pointer;
    FOnGetIdentValue : TGetIdentValueEvent;
    FCaseInsensitive : Boolean;
    FLocalVar        : TVariableList;
    procedure ExecConst(aItem: TPostfixItem); inline;
    procedure ExecVariable(aItem: TPostfixItem); inline;
    procedure ExecLocal(aItem: TPostfixItem);
    procedure ExecCommand(aItem: TPostfixItem);
  protected
    function ExecutePostfixItem(aItem: TPostfixItem): boolean;
    { перекрываемые методы }
    procedure ExecParameter(aItem: TPostfixItem); virtual;
    procedure ExecIdent(aItem: TPostfixItem); virtual;
    procedure ExecBinary(aItem: TPostfixItem); virtual;
    procedure ExecUnary(aItem: TPostfixItem); virtual;
    procedure ExecFunction(aItem: TPostfixItem); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property OnGetIdentValue : TGetIdentValueEvent read FOnGetIdentValue write FOnGetIdentValue;
    function Calculate(const aExpression: TExpressionItem; aDataObject: Pointer = nil): Variant;
    property CI: boolean read FCaseInsensitive write FCaseInsensitive;
    // v 18.3
    property LocalVariables: TVariableList read FLocalVar;
  end;

  /// <summary>вычислитель для преобразования Postfix'а в условия SQL - запроса</summary>
  TDeSQLCalculator = class(TDeCalculator)
  private
    FParamList: TVariableList;
    FTableToStr: TTableToStrEvent;
    FFieldToStr: TFieldToStrEvent;
    FConstToStr: TConstToStrEvent;
    FFuncToStr: TFuncToStrEvent;
    function InnerConstToStr(const aValue: Variant; const aType: TFieldType): string;
    function SQLString(aItem: TStackItem; const aCodePage: Integer = -1): string;
  protected
    procedure ExecParameter(aItem: TPostfixItem);  override;
    procedure ExecIdent(aItem: TPostfixItem); override;
    procedure ExecBinary(aItem: TPostfixItem);  override;
    procedure ExecUnary(aItem: TPostfixItem);  override;
    procedure ExecFunction(aItem: TPostfixItem);  override;
  public
    constructor Create; overload;
    /// <summary>если назначен внешний ParamList, то параметры формируются в этот список</summary>
    property ParamList: TVariableList read FParamList write FParamList;
    property onTableToStr: TTableToStrEvent read FTableToStr write FTableToStr;
    property onFieldToStr: TFieldToStrEvent read FFieldToStr write FFieldToStr;
    property onConstToStr: TConstToStrEvent read FConstToStr write FConstToStr;
    property onFuncToStr: TFuncToStrEvent read FFuncToStr write FFuncToStr;
  end;

implementation

uses Variants, Forms,
     DeLog, Funcs, DeActions, DeVariable, DeScript, DataCacheUnit, Meta, DSMeta, DeMetadata;

{ TPostfixStack }

constructor TPostfixStack.Create;
begin
  inherited Create;
  FIndex := -1;
  Size   := 32;
end;

destructor TPostfixStack.Destroy;
begin
  Size := 0;
  inherited;
end;

function TPostfixStack.GetSize: integer;
begin
  result := Length(FStack);
end;

function TPostfixStack.GetStackItem(aIndex: Integer): TStackItem;
begin
  Assert( (0 <= aIndex) and (aIndex <= FIndex), 'Stack item out of bounds');
  result := FStack[aIndex];
end;

function TPostfixStack.Peek: TStackItem;
begin
  if FIndex < 0 then
    raise EAbort.Create('Stack is empty')
  else
    result := FStack[FIndex];
end;

function TPostfixStack.Pop: TStackItem;
begin
  if FIndex < 0 then
    raise EAbort.Create('Stack is empty')
  else
  begin
    result := FStack[FIndex];

    {$IF DEFINED(DEBUG) or DEFINED(DeDEBUG)}
    FStack[FIndex].Value:= unassigned;
    FStack[FIndex].DataType:= ftUnknown;
    FStack[FIndex].CodePage:= 0;
    FStack[FIndex].Postfix:=nil;
    FStack[FIndex].IsValue:= False;
    {$ENDIF}

    dec(FIndex);
  end;
end;

function TPostfixStack.Push(aItem: TPostfixItem; aValue: Variant; aDataType: TFieldType; aCodePage: Integer = -1): TStackItem;
begin
  inc(FIndex);
  if FIndex > Size then
    Size := Size + 32;
  FStack[FIndex].Postfix:= aItem;
  FStack[FIndex].Value:= aValue;
  FStack[FIndex].DataType:= aDataType;
  FStack[FIndex].CodePage:= aCodePage;
  FStack[FIndex].IsValue:= True;
  result := FStack[FIndex];
end;

function TPostfixStack.PushSQL(aItem: TPostfixItem; aValue: String; aDataType: TFieldType = ftUnknown; aCodePage: Integer = -1): TStackItem;
begin
  inc(FIndex);
  if FIndex > Size then
    Size := Size + 32;
  FStack[FIndex].Postfix:= aItem;
  FStack[FIndex].Value:= aValue;
  FStack[FIndex].DataType:= aDataType;
  FStack[FIndex].CodePage:= aCodePage;
  FStack[FIndex].IsValue:= False;
  result := FStack[FIndex];
end;

procedure TPostfixStack.SetSize(const Value: integer);
begin
  SetLength(FStack, Value);
end;

procedure TPostfixStack.Clear;
begin
  FIndex:= -1;
  if Size>32 then Size:= 32;
end;

{ TDeCustomCalculator }

constructor TDeCalculator.Create;
begin
  inherited Create;
  FStack:= TPostfixStack.Create;
  FLocalVar:= TVariableList.Create;
  FCaseInsensitive:= False;
end;

destructor TDeCalculator.Destroy;
begin
  FLocalVar.Free;
  FStack.Free;
  inherited;
end;

function TDeCalculator.ExecutePostfixItem(aItem : TPostfixItem) : boolean;
begin
  try
    case aItem.ItemType of
      piConst:     ExecConst(aItem);
      piIdent:     ExecIdent(aItem);
      piBinary:    ExecBinary(aItem);
      piUnary:     ExecUnary(aItem);
      piFunction:  ExecFunction(aItem);
      piCommand:   ExecCommand(aItem);
      piLocal:     ExecLocal(aItem);
      piVariable:  ExecVariable(aItem);
      piParameter: ExecParameter(aItem);
    end;
    Result:= True;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('Calculator error: ' + E.Message);
        {$ENDIF}
        raise Exception.Create(E.Message);
        Result:= False;
      end;
  end;
end;

procedure TDeCalculator.ExecConst(aItem: TPostfixItem);
begin
  try
    FStack.Push(aItem, aItem.Data, aItem.ResultType, aItem.CodePage);
  except
    raise Exception.Create('Error in TDeCalculator.ExecConst');
  end;
end;

procedure TDeCalculator.ExecVariable(aItem : TPostfixItem);
begin
  try
    FStack.Push(aItem, aItem.Variable.Value, aItem.ResultType, aItem.CodePage);
  except
    raise Exception.Create('Error in TDeCalculator.ExecVariable');
  end;
end;

procedure TDeCalculator.ExecParameter(aItem : TPostfixItem);
begin
  try
    FStack.Push(aItem, aItem.Parameter.Value, aItem.ResultType, aItem.CodePage);
  except
    raise Exception.Create('Error in TDeCalculator.ExecParameter');
  end;
end;

procedure TDeCalculator.ExecLocal(aItem : TPostfixItem);
var VI: TVariableItem;
begin
  try
    VI:= FLocalVar.FindVariable(aItem.Name);
    if assigned(VI) then FStack.Push(aItem, VI.Value, VI.DataType)
                    else FStack.Push(aItem, Unassigned, ftUnknown);
  except
    raise Exception.Create('Error in TDeCalculator.ExecLocal');
  end;
end;

procedure TDeCalculator.ExecIdent(aItem : TPostfixItem);
var V: Variant;
begin
  try
    if Assigned(FOnGetIdentValue) then
      begin
        if FOnGetIdentValue(aItem.Ident, V, FDataObject) then
          FStack.Push(aItem, V, aItem.ResultType, aItem.CodePage)
        else
          FStack.Push( aItem, Unassigned, aItem.ResultType, aItem.CodePage)
      end
    else
      FStack.Push( aItem, Unassigned, aItem.ResultType, aItem.CodePage);
  except
    raise Exception.Create('Error in TDeCalculator.ExecIdent');
  end;
end;

procedure TDeCalculator.ExecBinary(aItem: TPostfixItem);
var i: Integer;
    a,b: String;
    Res: Boolean;
    vi: TVariableItem;
    aLeft, aRight: TStackItem;
begin
  try
    aRight:= FStack.Pop;
    aLeft := FStack.Pop;

    case aItem.BinaryOp of
      opIs    : FStack.Push(aItem, VarIsNull(aLeft.Value), aItem.ResultType);

      opIsNot : FStack.Push(aItem, Not VarIsNull(aLeft.Value), aItem.ResultType);

      opAnd   : if VarIsType(aLeft.Value, [varEmpty, varNull]) or VarIsType(aRight.Value, [varEmpty, varNull])
                  then FStack.Push(aItem, null, aItem.ResultType)
                  else FStack.Push(aItem, aLeft.Value and aRight.Value, aItem.ResultType);

      opOr    : if VarIsType(aLeft.Value, [varEmpty, varNull])
                  then if VarIsType(aRight.Value, [varEmpty, varNull])
                         then FStack.Push(aItem, null, aItem.ResultType)
                         else FStack.Push(aItem, aRight.Value, aItem.ResultType)
                  else if VarIsType(aRight.Value, [varEmpty, varNull])
                         then FStack.Push(aItem, aLeft.Value, aItem.ResultType)
                         else FStack.Push(aItem, aLeft.Value or aRight.Value, aItem.ResultType);

      opXor   : if VarIsType(aLeft.Value, [varEmpty, varNull]) or VarIsType(aRight.Value, [varEmpty, varNull])
                  then FStack.Push(aItem, null, aItem.ResultType)
                  else FStack.Push(aItem, aLeft.Value xor aRight.Value, aItem.ResultType);

      opPlus  : if VarIsType(aLeft.Value, [varEmpty, varNull])
                  then if VarIsType(aRight.Value, [varEmpty, varNull])
                         then FStack.Push(aItem, null, aItem.ResultType)
                         else FStack.Push(aItem, aRight.Value, aItem.ResultType)
                  else if VarIsType(aRight.Value, [varEmpty, varNull])
                         then FStack.Push(aItem, aLeft.Value, aItem.ResultType)
                         else FStack.Push(aItem, aLeft.Value + aRight.Value, aItem.ResultType);

      opMinus : if VarIsType(aLeft.Value, [varEmpty, varNull])
                  then if VarIsType(aRight.Value, [varEmpty, varNull])
                         then FStack.Push(aItem, null, aItem.ResultType)
                         else FStack.Push(aItem, -aRight.Value, aItem.ResultType)
                  else if VarIsType(aRight.Value, [varEmpty, varNull])
                         then FStack.Push(aItem, aLeft.Value, aItem.ResultType)
                         else FStack.Push(aItem, aLeft.Value - aRight.Value, aItem.ResultType);

      opMul   : if VarIsType(aLeft.Value, [varEmpty, varNull]) or VarIsType(aRight.Value, [varEmpty, varNull])
                  then FStack.Push(aItem, null, aItem.ResultType) else
                if VarIsType(aLeft.Value, [varDate]) and VarIsType(aRight.Value, [varInteger, varSingle, varDouble, varShortInt, varByte, varWord, varLongWord, varInt64, varUInt64])
                  then FStack.Push(aItem, FloatToDateTime(double(TimeOf(aLeft.Value)) * double(aRight.Value)), aItem.ResultType) else
                if VarIsType(aRight.Value, [varDate]) and VarIsType(aLeft.Value, [varInteger, varSingle, varDouble, varShortInt, varByte, varWord, varLongWord, varInt64, varUInt64])
                  then FStack.Push(aItem, FloatToDateTime(double(aLeft.Value) * double(TimeOf(aRight.Value))), aItem.ResultType) else
                       FStack.Push(aItem, aLeft.Value * aRight.Value, aItem.ResultType);

      opDiv   : if VarIsType(aLeft.Value, [varEmpty, varNull]) or VarIsType(aRight.Value, [varEmpty, varNull]) or (aRight.Value = 0)
                  then FStack.Push(aItem, null, aItem.ResultType)
                  else FStack.Push(aItem, aLeft.Value / aRight.Value, aItem.ResultType);

      opIntDiv: if VarIsType(aLeft.Value, [varEmpty, varNull]) or VarIsType(aRight.Value, [varEmpty, varNull]) or (aRight.Value = 0)
                  then FStack.Push(aItem, null, aItem.ResultType)
                  else FStack.Push(aItem, aLeft.Value div aRight.Value, aItem.ResultType);

      opMod   : if VarIsType(aLeft.Value, [varEmpty, varNull]) or VarIsType(aRight.Value, [varEmpty, varNull]) or (aRight.Value = 0)
                  then FStack.Push(aItem, null, aItem.ResultType)
                  else FStack.Push(aItem, aLeft.Value mod aRight.Value, aItem.ResultType);

      opEQ    : if VarIsType(aLeft.Value, [varEmpty, varNull]) and VarIsType(aRight.Value, [varEmpty, varNull])
                  then FStack.Push(aItem, True, aItem.ResultType)
                  else if CI and VarIsType(aLeft.Value, [varString, varUString]) and VarIsType(aRight.Value, [varString, varUString])
                         then FStack.Push(aItem, AnsiCompareText(aLeft.Value, aRight.Value) = 0, aItem.ResultType)
                         else FStack.Push(aItem, VarCompareValue(aLeft.Value, aRight.Value) = vrEqual, aItem.ResultType);

      opNE    : if VarIsType(aLeft.Value, [varEmpty, varNull]) and VarIsType(aRight.Value, [varEmpty, varNull])
                  then FStack.Push(aItem, False, aItem.ResultType)
                  else if CI and VarIsType(aLeft.Value, [varString, varUString]) and VarIsType(aRight.Value, [varString, varUString])
                         then FStack.Push(aItem, AnsiCompareText(aLeft.Value, aRight.Value) <> 0, aItem.ResultType)
                         else FStack.Push(aItem, VarCompareValue(aLeft.Value, aRight.Value) in [vrNotEqual, vrLessThan, vrGreaterThan], aItem.ResultType);

      opLE    : if VarIsType(aLeft.Value, [varEmpty, varNull]) and VarIsType(aRight.Value, [varEmpty, varNull])
                  then FStack.Push(aItem, True, aItem.ResultType)
                  else FStack.Push(aItem, VarCompareValue(aLeft.Value, aRight.Value) in [vrLessThan, vrEqual], aItem.ResultType);

      opGE    : if VarIsType(aLeft.Value, [varEmpty, varNull]) and VarIsType(aRight.Value, [varEmpty, varNull])
                  then FStack.Push(aItem, True, aItem.ResultType)
                  else FStack.Push(aItem, VarCompareValue(aLeft.Value, aRight.Value) in [vrGreaterThan, vrEqual], aItem.ResultType);

      opLT    : if VarIsType(aLeft.Value, [varEmpty, varNull]) and VarIsType(aRight.Value, [varEmpty, varNull])
                  then FStack.Push(aItem, False, aItem.ResultType)
                  else FStack.Push(aItem, VarCompareValue(aLeft.Value, aRight.Value) = vrLessThan, aItem.ResultType);

      opGT    : if VarIsType(aLeft.Value, [varEmpty, varNull]) and VarIsType(aRight.Value, [varEmpty, varNull])
                  then FStack.Push(aItem, False, aItem.ResultType)
                  else FStack.Push(aItem, VarCompareValue(aLeft.Value, aRight.Value) = vrGreaterThan, aItem.ResultType);

      opLike :  begin
                  if VarIsType(aLeft.Value, [varEmpty, varNull])  then a:= ''
                                                                  else a:= VarAsType(aLeft.Value, varString);
                  if VarIsType(aRight.Value, [varEmpty, varNull]) then b:= ''
                                                                  else b:= VarAsType(aRight.Value, varString);
                  FStack.Push(aItem, Like(a, b, CI), aItem.ResultType)
                end;

      opNotLike:begin
                  if VarIsType(aLeft.Value, [varEmpty, varNull])  then a:= ''
                                                                  else a:= VarAsType(aLeft.Value, varString);
                  if VarIsType(aRight.Value, [varEmpty, varNull]) then b:= ''
                                                                  else b:= VarAsType(aRight.Value, varString);
                  FStack.Push(aItem, Not Like(a, b, CI), aItem.ResultType)
                end;
                
      opArray : FStack.Push(aItem, VarArrayPlusVarArray(aLeft.Value, aRight.Value), ftArray);

      opNext  : begin
                  FStack.Clear;
                  FStack.Push(aItem, aRight.Value, aRight.DataType, aRight.CodePage);
                end;

      opIN, opNotIN
              : begin
                  Res:= False;
                  // Пустое множество значений
                  if VarIsNullOrEmpty(aRight.Value) then
                    begin
                      // ничего не делаем
                    end else

                  // Массив значений
                  if VarIsArray(aRight.Value) then
                    begin
                      for i:=VarArrayLowBound(aRight.Value, 1) to VarArrayHighBound(aRight.Value, 1) do
                         begin
                           Res:= (VarCompareValue(aLeft.Value, aRight.Value[i]) = vrEqual);
                           if Res then Break;
                         end;
                    end else

                  // Одно значение
                    begin
                      Res:= VarCompareValue(aLeft.Value, aRight.Value) = vrEqual;
                    end;

                  //  Для NOT IN меняем результат на обратный
                  if aItem.BinaryOp = opIN then FStack.Push(aItem,     Res, aItem.ResultType)
                                           else FStack.Push(aItem, Not Res, aItem.ResultType);
                end;
      opAssign: case aLeft.Postfix.ItemType of
                  piLocal:    begin
                                VI:= FLocalVar.FindVariable(aLeft.Postfix.Name);
                                if not assigned(VI) then
                                   begin
                                     VI:=TVariableItem.Create(aRight.DataType, aLeft.Postfix.Name, [amRead, amWrite]);
                                     FLocalVar.Add(VI);
                                   end;
                                VI.Value:= aRight.Value;
                                FStack.Push(aItem, aRight.Value, aRight.DataType, aRight.CodePage);
                              end;
                  piVariable: begin
                                aItem.Variable.Value:= aRight.Value;
                                FStack.Push(aItem, aRight.Value, aRight.DataType, aRight.CodePage);
                              end;
                  else        raise Exception.Create('Assign operation only for Local or Variable Item');
               end;
    else
      raise Exception.Create('Unknown Operation in TDeCalculator.ExecBinary');
    end;
  except
    raise Exception.Create('Error in TDeCalculator.ExecBinary');
  end;
end;

procedure TDeCalculator.ExecUnary(aItem: TPostfixItem);
var aRight : TStackItem;
begin
  try
    aRight:= FStack.Pop;
    case aItem.UnaryOp of
      opPlusUno: FStack.Push(aItem, aRight.Value, aRight.DataType);

      opMinusUno:if VarIsType(aRight.Value, [varNull, varEmpty])
                   then FStack.Push(aItem,  aRight.Value, aRight.DataType)
                   else FStack.Push(aItem, -aRight.Value, aRight.DataType);

      opNot    : if VarIsType(aRight.Value, [varNull, varEmpty])
                   then FStack.Push(aItem, aRight.Value, aRight.DataType)
                   else FStack.Push(aItem, not aRight.Value, aRight.DataType);
    else
      raise Exception.Create('Unknown Operation in TDeCalculator.ExecUnary');
    end;
  except
    raise Exception.Create('Error in TDeCalculator.ExecUnary');
  end;
end;

procedure TDeCalculator.ExecFunction(aItem: TPostfixItem);
var Args : array of Variant;
    I    : integer;
begin
  SetLength(Args, aItem.Func.ArgCount);
  // складываем в обратном порядке
  for I := 0 to aItem.Func.ArgCount-1 do
    Args[(aItem.Func.ArgCount-1)-I]:= FStack.Pop.Value;
  FStack.Push(aItem, aItem.Func.Execute(Args), aItem.ResultType);
  SetLength(Args, 0);
end;

procedure TDeCalculator.ExecCommand(aItem: TPostfixItem);
var I : Integer;
    DeVL: TDeVariableList;
    T: TVariableItem;
    Res: Boolean;
begin
  DeVL:= TDeVariableList.Create;
  for I := 0 to FLocalVar.Count-1 do
    DeVL.GetByName(FLocalVar.Items[i].Name).Value:=FLocalVar.Items[i].Value;

  I:= ProfitActions.IndexByOriginal(aItem.Name);

  res := True;
  try
    ProfitActions.BeforeExecute(ProfitActions[I]);        // Накапливаем сообщения в список
    ProfitActions[I].Execute(DeVL, True, amFocused);
    ProfitActions.AfterExecute(ProfitActions[I], DeVL);  // выводим в конце, если надо
  except
    res := False;
  end;

  for I := 0 to DeVL.Count-1 do
    if  DeVL.Items[i].IsOutput then
      begin
        T:=FLocalVar.FindVariable(DeVL.Items[i].Name);
        if assigned(T) then
          T.Value := DeVL.Items[i].Value;
      end;
  DeVL.Free;

  FStack.Push(aItem, Res, ftBoolean);
end;

function TDeCalculator.Calculate(const aExpression: TExpressionItem; aDataObject: pointer = nil): Variant;
var I : integer;
begin
  Result := Unassigned;

  if 0 < aExpression.Count then
    begin
      FStack.Clear;
      FDataObject := aDataObject;

      for I := 0 to aExpression.Count-1 do
        if not ExecutePostfixItem(aExpression[I]) then Exit;

      Result := FStack.Peek.Value;
    end;
end;

{ TDeSQLCalculator }

constructor TDeSQLCalculator.Create;
begin
  inherited;
  FParamList:= nil;
end;

procedure TDeSQLCalculator.ExecParameter(aItem : TPostfixItem);
begin
  try
    FStack.PushSQL(aItem, Format(':%s', [aItem.Parameter.Name]), aItem.ResultType, aItem.CodePage);
  except
    raise Exception.Create('Error in TDeSQLCalculator.ExecParameter');
  end;
end;

procedure TDeSQLCalculator.ExecIdent(aItem : TPostfixItem);
begin
  try
    FStack.PushSQL(aItem, aItem.IdentSQL, aItem.ResultType, aItem.CodePage);
  except
    raise Exception.Create('Error in TDeSQLCalculator.ExecIdent');
  end;
end;

function TDeSQLCalculator.InnerConstToStr(const aValue: Variant; const aType: TFieldType): string;
var H,M,S,X : Word;
begin
  if assigned(FConstToStr) then Result:= FConstToStr(aValue, aType) else

  if aType in NumericTypes then Result:= VarToStr(aValue) else
  if aType in FloatTypes then Result:= StringReplace(VarToStr(aValue), ',', '.', []) else
  if aType in StringTypes then Result:= '''' + VarToStr(aValue) + '''' else
  if aType in LogicalTypes then  Result:= {'''' +} VarToStr(aValue) {+ ''''} else
  if aType in [ftTime] then begin
                              DecodeTime(aValue, H, M, S, X);
                              if (X=0)
                                then Result := FormatDateTime('hh:nn:ss', aValue)
                                else Result := FormatDateTime('hh:nn:ss.zzz', aValue);
                            end else
  if aType in DateTimeTypes then begin
                                   DecodeTime(aValue, H, M, S, X);
                                   if (H=0) and (M=0) and (S=0) and (X=0)
                                     then Result:= FormatDateTime('DD.MM.YYYY', aValue)
                                     else Result:= FormatDateTime('YYYYMMDD hh:nn:ss', aValue);
                                 end else
                                 Result:= '''' + VarToStr(aValue) + '''';
end;

function TDeSQLCalculator.SQLString(aItem: TStackItem; const aCodePage: Integer = -1): string;
var VI: TVariableItem;
    i: Integer;
begin
  // фрагмент SQL выражения
  if Not aItem.isValue then Result:= VarToStr(aItem.Value) else

  // null никогда не параметризуем
  if VarIsNullOrEmpty(aItem.Value) then Result:= 'null' else

  // time (не datetime) передается как параметр криво, поэтому буквами
  if (aItem.DataType in [ftTime]) and (VarType(aItem.Value) in [varDate])  then
    Result:= ''''+FormatDateTime('hh:mm:ss', aItem.Value)+'''' else

  // в debug версии целые не парметризуем
  {$IF DEFINED(DEBUG) or DEFINED(DeDEBUG)}
  if (aItem.DataType in IntegerTypes) and (VarType(aItem.Value) in IntVarTypes) then
    Result:= IntToStr(aItem.Value) else
  {$ENDIF}

  // массив, приводим к непараметризованному тексту через запятую
  if VarIsArray(aItem.Value) then
    begin
      Result:= EmptyStr;
      for i:= VarArrayLowBound(aItem.Value, 1) to VarArrayHighBound(aItem.Value, 1) do
        begin
          if  VarArrayLowBound(aItem.Value, 1) < i  then Result:= Result + ',';
          if (aItem.DataType in AnsiStringTypes) and (aCodePage = cpUTF8)
            then Result:= Result + InnerConstToStr(WideStringToUnicode(aItem.Value[i]), aItem.DataType)
            else Result:= Result + InnerConstToStr(aItem.Value[i], aItem.DataType);
        end;
    end else

  // параметризуем при наличии массива параметров
  if assigned(FParamList) then
    begin
      VI:= TVariableItem.Create(aItem.DataType, 'Param_'+IntToStr(FParamList.Count+1)+'_D', [amRead, amWrite]);
      if (aItem.DataType in AnsiStringTypes) and (aCodePage = cpUTF8)
        then VI.Value:= WideStringToUnicode(aItem.Value)
        else VI.Value:= aItem.Value;
      FParamList.Add(VI);
      Result:= ':' + VI.Name;
    end else

  // приводим константу или значение к тексту
  if (aItem.DataType in AnsiStringTypes) and (aCodePage = cpUTF8)
    then Result:= InnerConstToStr(WideStringToUnicode(aItem.Value), aItem.DataType)
    else Result:= InnerConstToStr(aItem.Value, aItem.DataType);
end;

procedure TDeSQLCalculator.ExecBinary(aItem: TPostfixItem);
var aLeft, aRight: TStackItem;
begin
  aRight:= FStack[FStack.FIndex];
  aLeft:=  FStack[Pred(FStack.FIndex)];

  if aItem.BinaryOp in [opAssign]
    then inherited ExecBinary(aItem) else

  if aLeft.IsValue and aRight.IsValue
    then inherited ExecBinary(aItem) else

    try
      aRight:= FStack.Pop;
      aLeft:= FStack.Pop;

      case aItem.BinaryOp of
        opIs, opIsNot:
          FStack.PushSQL(aItem, Format('%s %s %s', [SQLString(aLeft), OperationLexeme[aItem.BinaryOp], SQLString(aRight)]));

        opAnd:
          if ((aLeft.IsValue) and (aLeft.Value = False)) or ((aRight.IsValue) and (aRight.Value = False))
            then FStack.Push(aItem, False, ftBoolean)
            else if ((aLeft.IsValue) and (aLeft.Value = True))
                   then FStack.PushSQL( aRight.Postfix, SQLString(aRight))
                   else if ((aRight.IsValue) and (aRight.Value = True))
                          then FStack.PushSQL(aLeft.Postfix, SQLString(aLeft))
                          else if aLeft.IsOperation(aItem.BinaryOp) or (aLeft.Postfix.ItemType = piUnary)
                                 then if aRight.IsOperation(aItem.BinaryOp) or (aRight.Postfix.ItemType = piUnary)
                                        then FStack.PushSQL(aItem, Format('%s %s %s', [SQLString(aLeft), OperationLexeme[aItem.BinaryOp], SQLString(aRight)]))
                                        else FStack.PushSQL(aItem, Format('%s %s (%s)', [SQLString(aLeft), OperationLexeme[aItem.BinaryOp], SQLString(aRight)]))
                                 else if aRight.IsOperation(aItem.BinaryOp) or (aRight.Postfix.ItemType = piUnary)
                                        then FStack.PushSQL(aItem, Format('(%s) %s %s', [SQLString(aLeft), OperationLexeme[aItem.BinaryOp], SQLString(aRight)]))
                                        else FStack.PushSQL(aItem, Format('(%s) %s (%s)', [SQLString(aLeft), OperationLexeme[aItem.BinaryOp], SQLString(aRight)]));

        opOr:
          if ((aLeft.IsValue) and (aLeft.Value = True)) or ((aRight.IsValue) and (aRight.Value = True))
            then FStack.Push(aItem, True, ftBoolean)
            else if (aLeft.IsValue) and (aLeft.Value = False)
                   then FStack.PushSQL(aRight.Postfix, SQLString(aRight))
                   else if (aRight.IsValue) and (aRight.Value = False)
                          then FStack.PushSQL(aLeft.Postfix, SQLString(aLeft))
                          else if aLeft.IsOperation(aItem.BinaryOp) or (aLeft.Postfix.ItemType = piUnary)
                                 then if aRight.IsOperation(aItem.BinaryOp) or (aRight.Postfix.ItemType = piUnary)
                                        then FStack.PushSQL(aItem, Format('%s %s %s', [SQLString(aLeft), OperationLexeme[aItem.BinaryOp], SQLString(aRight)]))
                                        else FStack.PushSQL(aItem, Format('%s %s (%s)', [SQLString(aLeft), OperationLexeme[aItem.BinaryOp], SQLString(aRight)]))
                                 else if aRight.IsOperation(aItem.BinaryOp) or (aRight.Postfix.ItemType = piUnary)
                                        then FStack.PushSQL(aItem, Format('(%s) %s %s', [SQLString(aLeft), OperationLexeme[aItem.BinaryOp], SQLString(aRight)]))
                                        else FStack.PushSQL(aItem, Format('(%s) %s (%s)', [SQLString(aLeft), OperationLexeme[aItem.BinaryOp], SQLString(aRight)]));

        opXor:
          if ((aLeft.IsValue) and (aLeft.Value = False))
            then FStack.PushSQL(aRight.Postfix, SQLString(aRight))
            else if ((aRight.IsValue) and (aRight.Value = False))
                   then FStack.PushSQL(aLeft.Postfix, SQLString(aLeft))
                   else if (aLeft.Postfix.ItemType = piUnary)
                           then if (aRight.Postfix.ItemType = piUnary)
                                  then FStack.PushSQL(aItem, Format('%s %s %s', [SQLString(aLeft), OperationLexeme[aItem.BinaryOp], SQLString(aRight)]))
                                  else FStack.PushSQL(aItem, Format('%s %s (%s)', [SQLString(aLeft), OperationLexeme[aItem.BinaryOp], SQLString(aRight)]))
                           else if (aRight.Postfix.ItemType = piUnary)
                                  then FStack.PushSQL(aItem, Format('(%s) %s %s', [SQLString(aLeft), OperationLexeme[aItem.BinaryOp], SQLString(aRight)]))
                                  else FStack.PushSQL(aItem, Format('(%s) %s (%s)', [SQLString(aLeft), OperationLexeme[aItem.BinaryOp], SQLString(aRight)]));

        opIN, opNOTIN:
          if aLeft.Postfix.ItemType = piIdent then
            FStack.PushSQL(aItem,  Format('%s %s (%s)', [SQLString(aLeft, aRight.CodePage), OperationLexeme[aItem.BinaryOp], SQLString(aRight, aLeft.CodePage)]))
          else
            FStack.PushSQL(aItem,  Format('(%s) %s (%s)', [SQLString(aLeft, aRight.CodePage), OperationLexeme[aItem.BinaryOp], SQLString(aRight, aLeft.CodePage)]));

        opEQ: { Равно }
          if VarIsNullOrEmpty(aLeft.Value)
            then FStack.PushSQL(aItem, Format('%s %s %s', [SQLString(aRight), OperationLexeme[opIs], NullLexeme ]))
            else if VarIsNullOrEmpty(aRight.Value)
                   then FStack.PushSQL(aItem, Format('%s %s %s', [SQLString(aLeft), OperationLexeme[opIs], NullLexeme ]))
                   else FStack.PushSQL(aItem, Format('%s %s %s', [SQLString(aLeft, aRight.CodePage), OperationLexeme[aItem.BinaryOp], SQLString(aRight, aLeft.CodePage)]));

        opNE: { Не равно }
          if VarIsNullOrEmpty(aLeft.Value)
            then FStack.PushSQL(aItem, Format('%s %s %s', [SQLString(aRight), OperationLexeme[opIsNot], NullLexeme ]))
            else if VarIsNullOrEmpty(aRight.Value)
                   then FStack.PushSQL(aItem, Format('%s %s %s', [SQLString(aLeft), OperationLexeme[opIsNot], NullLexeme ]))
                   else FStack.PushSQL(aItem, Format('%s %s %s', [SQLString(aLeft, aRight.CodePage), OperationLexeme[aItem.BinaryOp], SQLString(aRight, aLeft.CodePage)]));

        opNext: FStack.PushSQL(aItem, aRight.Value, aRight.DataType, aRight.CodePage);

      else
        if aItem.BinaryOp in BinaryOperations then
          FStack.PushSQL(aItem, Format('%s %s %s', [SQLString(aLeft, aRight.CodePage), OperationLexeme[aItem.BinaryOp], SQLString(aRight, aLeft.CodePage)]))
        else
          raise Exception.Create('Unknown Operation in TDeSQLCalculator.ExecBinary');
    end;
  except
    raise Exception.Create('Error in TDeSQLCalculator.ExecBinary');
  end;
end;

procedure TDeSQLCalculator.ExecUnary(aItem: TPostfixItem);
var aRight: TStackItem;
begin
  aRight:=FStack[FStack.FIndex];

  if aRight.IsValue then inherited ExecUnary(aItem) else

  try
    aRight := FStack.Pop;
    if (aItem.UnaryOp in [opPlusUno, opMinusUno])
      then FStack.PushSQL(aItem, Format('%s%s', [OperationLexeme[aItem.UnaryOp], SQLString(aRight)]))
      else if aItem.UnaryOp in UnaryOperations
             then FStack.PushSQL(aItem, Format('%s (%s)', [OperationLexeme[aItem.UnaryOp], SQLString(aRight)]))
             else raise Exception.Create('Unknown Operation in TDeSQLCalculator.ExecUnary');
  except
    raise Exception.Create('Error in TDeSQLCalculator.ExecUnary');
  end;
end;

procedure TDeSQLCalculator.ExecFunction(aItem: TPostfixItem);
var Args: array of Variant;
    i: integer;
    fIsValues: Boolean;
begin
  // складываем в обратном порядке
  fIsValues:= True;

  // Транслируем функцию "Now(True)" на SQL сервер по возможности ...
  if SameText(aItem.Func.Name, 'NOW') then
    if aItem.Func.ArgCount = 1 then
      if FStack[FStack.FIndex].isValue and (FStack[FStack.FIndex].DataType = ftBoolean) then
        if FStack[FStack.FIndex].Value = True then
          fIsValues := False;

  for I := 0 to aItem.Func.ArgCount-1 do
    fIsValues:= fIsValues and FStack[FStack.FIndex-i].isValue;

  if fIsValues then
    inherited ExecFunction(aItem)
  else
    begin
      if not assigned(FFuncToStr) then
        raise Exception.Create('function '+aItem.Name+' not define in Database');

      SetLength(Args, aItem.Func.ArgCount);
      for I := 0 to aItem.Func.ArgCount-1 do
        Args[(aItem.Func.ArgCount-1)-I]:= FStack.Pop.Value;

      FStack.PushSQL(aItem, FFuncToStr(aItem.Func.Name, Args), aItem.ResultType);
      SetLength(Args, 0);
    end;
end;

{ TStackItem }

function TStackItem.IsOperation(aOperation: TOperationType): Boolean;
begin
  case Postfix.ItemType of
    piBinary: Result:= (Postfix.BinaryOp = aOperation);
    piUnary: Result:= (Postfix.UnaryOp = aOperation);
    else Result:= False;
  end;
end;

procedure TStackItem.Assign(aSource: TStackItem);
begin
  Postfix:= aSource.Postfix;
  IsValue:= aSource.IsValue;
  Value:= aSource.Value;
  DataType:= aSource.DataType;
  CodePage:= aSource.CodePage;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DeCalculator unit initialization ...');

finalization
  DebugLog('DeCalculator unit finalization ...');
{$ENDIF}

end.
