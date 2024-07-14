unit DeParser;

{
  <script> ::= <expression> | <assignment>[; <assignment>[; ...]]
  <assignment> ::= <variable> := <expression>
  <expression> ::= <simple expression> [ < | <= | > | >= | = | <> <simple expression>]
  <simple expression> ::= <term> [+ | - | or | xor <term>]
  <term> ::= <factor> [* | / | and <factor>]
  <factor> ::= <variable> | <constant> | <expression> | <unary> <factor> | <function call>
  <function call> ::= <function identifier>[([<expression>, [<expression>, ...]])]
  <unary> ::= + | - | not
}

interface

uses SysUtils, Classes, Contnrs, DB, DeTypes, DeFunctions, Variants;

const
  FieldTypeNames: array[TFieldType] of PChar = ('ftUnknown', 'ftString', 'ftSmallint', 'ftInteger', 'ftWord',
    'ftBoolean', 'ftFloat', 'ftCurrency', 'ftBCD', 'ftDate', 'ftTime', 'ftDateTime',
    'ftBytes', 'ftVarBytes', 'ftAutoInc', 'ftBlob', 'ftMemo', 'ftGraphic', 'ftFmtMemo',
    'ftParadoxOle', 'ftDBaseOle', 'ftTypedBinary', 'ftCursor', 'ftFixedChar', 'ftWideString',
    'ftLargeint', 'ftADT', 'ftArray', 'ftReference', 'ftDataSet', 'ftOraBlob', 'ftOraClob',
    'ftVariant', 'ftInterface', 'ftIDispatch', 'ftGuid', 'ftTimeStamp', 'ftFMTBcd',
    'ftFixedWideChar', 'ftWideMemo', 'ftOraTimeStamp', 'ftOraInterval',
    'ftLongWord', 'ftShortint', 'ftByte', 'ftExtended', 'ftConnection', 'ftParams', 'ftStream',
    'ftTimeStampOffset', 'ftObject', 'ftSingle');

  LogicalOperations    = [opAnd, opOr, opXor, opNot];
  ComparisonOperations = [opLT, opLE, opGT, opGE, opEQ, opNE, opLike, opNotLike, opIs, opIsNot, opIN, opNotIn];
  UnaryOperations      = [opPlusUno, opMinusUno, opNot];
  BinaryOperations     = [opAnd, opOr, opXor, opLT, opLE, opGT, opGE, opEQ, opNE, opIs, opIsNot, opLike, opNotLike,
                          opPlus, opMinus, opMul, opDiv, opIntDiv, opMod, opIN, opNotIn, opArray, opNext, opAssign];
  AgregateOperations   = [opCOUNT, opSUM, opMIN, opMAX, opAVG];

  // Binary Operation Priority
  LastOperationPriority  = 7;
  OperationPriority : array[0..LastOperationPriority] of set of TOperationType =
    ([opMul, opDiv, opIntDiv, opMod], // в первую очередь
     [opPlus, opMinus],
     [opLT, opLE, opGT, opGE, opEQ, opNE, opIs, opIsNot, opLike, opNotLike, opIn, opNotIN],
     [opAnd],
     [opOr, opXor],
     [opArray],
     [opAssign],
     [opNext]);      // в последнюю очередь

type
  TBracketType = (
    btNone,          //  пустое значение
    btSimple,        //  ()
    btSquare,        //  []
    btFigure         //  {}
  );

  TTokenType = (
    tkNone,         // тип не задан
    tkNull,         // null
    tkString,       // константа
    tkInteger,      // целочисленная константа
    tkFloat,        // константа с плавающей точкой
    tkDateTime,     // константа дата
    tkBoolean,      // логическая константа
    tkIdent,        // идентификатор
    tkOperation,    // операция
    tkLeftBracket,  // левая скобка
    tkRightBracket, // правая скобка
    tkComma,        // запятая
    tkSemicolon     // точка с запятой
    );

  TSetOfChar = set of char;

  /// <summary>тип элемента дерева разбора выражения</summary>
  TPostfixItemType = (
    piNone,         // тип не задан
    piConst,        // константа
    piIdent,        // идентификатор
    piVariable,     // переменная
    piLocal,        // локальная переменная
    piParameter,    // параметр SQL-запроса
    piBinary,       // бинарная операция
    piUnary,        // унарная операция
    piFunction,     // функция
    piCommand       // команда
    );
  TPostfixItemTypes = set of TPostfixItemType;

  TVariableAccessModeType = (
    amRead,   // значение параметра можно прочитать
    amWrite,  // значение параметра можно записать
    amCalculated, // признак что храним функцию... временно потом надо просто хранить указатель на Get функцию
    amOutput  // при хранении паремтров команд флаг - указывает на необходимость вернуть значение в переменную
    );

  TVariableAccessMode = set of TVariableAccessModeType;

const
  { разрешенные символы идентификатора }
  RusChars = ['А'..'Я','а'..'п','р'..'я','Ё','ё'];
  IdentFirstChars = ['_', 'a'..'z', 'A'..'Z'] + RusChars;
  IdentChars = IdentFirstChars + ['0'..'9', '.'];
  FieldChars = IdentChars + ['.'];

  { разрешенные символы в числовых константах и константах - датах }
  Digits = ['0'..'9'];
  HexDigits = Digits + ['a'..'f', 'A'..'F'];
  FloatChars = Digits + ['+', '-', 'e', 'E', DeDecimalSeparator];

  { разрешенные кавычки }
  CommasChars = ['''', '"'];

  { коды ошибок выражения }
  Expr_OK                        =  0;  // выражение не содержит ошибок
  Error_IllegalSymbol            =  1;  // некорректный символ
  Error_BracketExpected          =  2;  // ожидается ')'
  Error_UnterminatedString       =  3;  // незакрытая строка
  Error_UnexpectedEndOfScript    =  4;  // неожиданный конец скрипта
  Error_UnknownIdent             =  5;  // неизвестный идентификатор
  Error_IncompatibleTypes        =  6;  // несовместимые типы
  Error_UnknownFunction          =  7;  // неизвестная функция
  Error_EmptyScript              =  8;  // пустое выражение
  Error_InvalidArgNumber         =  9;  // неверное количество аргументов функции
  Error_InvalidArgType           = 10;  // неверный тип аргумента функции
  Error_EndOfScript              = 11;  // выражение разобрано полностью
  Error_UnexpectedEndOfStatement = 12;  // неожиданный конец инструкции
  Error_InvalidOperandType       = 13;  // неверный тип операнда
  Error_InvalidConst             = 14;  // неверная константа
//  Error_ManyAssignments          = 15;  // более чем одно присваивание в выражении

  Error_Last = 14;  // последний номер ошибки
  ParserErrorMessage : array [1..Error_Last] of string = (
    '_Error.illegalsymbol "%s" _atpos %d _inexpr "%s"',
    '_Error.bracketexpected _atpos %d _inexpr "%s"',
    '_Error.unclosedstring "%s"',
    '_Error.unexpectedend "%s"',
    '_Error.unknownident "%s"',
    '_Error.incompatibletypes _atpos %d _inexpr "%s"',
    '_Error.unknownfunction "%s"',
    '_Error.emptyexpr',
    '_Error.argcount "%s"',
    '_Error.argtype _atpos %d _inexpr "%s"',
    '_Error.exprend',
    '_Error.statementend',
    '_Error.operandtype _atpos %d _inexpr "%s"',
    '_Error.illegalconst "%s"'
    );
{$IFDEF DEBUG}
const
  PostfixItemTypeNames: array[TPostfixItemType] of PChar = (
   'piNone', 'piConst', 'piIdent', 'piVariable', 'piLocal', 'piParameter',
   'piBinary', 'piUnary', 'piFunction', 'piCommand');
{$ENDIF}

type
  /// <summary>описание элемента выражения в фазе разбора</summary>
  TTokenInfo = class
  private
    FToken     : TTokenType;
    FTokenPos  : integer;
    FTokenData : Variant;
  public
    // тип Token'а
    property Token : TTokenType read FToken;
    // положение начала лексемы Token'а в выражении
    property TokenPos : integer read FTokenPos write FTokenPos;

    // назначение Token'а
    procedure SetData(const aType: TTokenType; Value: Variant);
    // константа
    property Data: Variant read FTokenData;
    // операция
    function Operation: TOperationType;
    // идентификатор
    function Ident : string;
    // Скобка
    function Bracket: TBracketType;
  end;

  { описание переменной }
  TVariableItem = class;
  TVariableList = class;
  TGetVariableItemValueEvent = function(Sender : TVariableItem): Variant of object;
  TSetVariableItemValueEvent = procedure(Sender : TVariableItem;  const Value : Variant) of object;

  TGetIdentTypeEvent = function(const aIdent: string; var aType: TFieldType; aDataObject: pointer): Boolean of object;
  TGetIdentValueEvent = function(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean of object;

  TGetVariableEvent = function(const aIdent: string; var aVariable: TVariableItem; aDataObject: pointer): Boolean of object;
//  TGetVariableValueEvent = function(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean of object;

  TVariableItem = class
  private
    FOwner      : TVariableList;
    FName       : string;
    FValue      : Variant;
    FType       : TFieldType;
    FDataSetID  : Integer;
    FAccessMode : TVariableAccessMode;
    FOnGetValue : TGetVariableItemValueEvent;
    FOnSetValue : TSetVariableItemValueEvent;
    function GetOutput: Boolean;
    procedure SetOutput(const Value: Boolean);
    function GetCalculated: Boolean;
    procedure SetCalculated(const Value: Boolean);
  protected
    function GetValue : Variant;  virtual;
    procedure SetValue(const aValue : Variant);  virtual;
  public
    constructor Create(const aType: TFieldType; const aName: string; const aAccessMode: TVariableAccessMode);  overload;
    destructor Destroy; override;
//  property Owner : TVariableList read FOwner;
    property DataType : TFieldType read FType;
    property DataSetID: Integer read FDataSetID  write FDataSetID;
    property Value : Variant read GetValue write SetValue;
    property Formula: Variant read FValue;
    property Name : string read FName;
    property AccessMode : TVariableAccessMode read FAccessMode;
    property IsOutput: Boolean read GetOutput write SetOutput;
    property IsCalculated: Boolean read GetCalculated write SetCalculated;
    property OnGetValue : TGetVariableItemValueEvent read FOnGetValue write FOnGetValue;
    property OnSetValue : TSetVariableItemValueEvent read FOnSetValue write FOnSetValue;
    procedure Assign(Source : TVariableItem);
    function IdenticalTo(Source : TVariableItem): Boolean;
  end;

  /// <summary>список переменных</summary>
  TVariableList = class(TObjectList)
  private
  protected
    function GetVariable(const Index: Integer): TVariableItem; virtual;
    procedure SetVariable(const Index: Integer; Value: TVariableItem);
  protected
    procedure Notify(Ptr: pointer; Action: TListNotification);  override;
  public
    property Items[const Index: Integer]: TVariableItem read GetVariable write SetVariable; default;
    function FindVariable(const aName : string) : TVariableItem; virtual;
    procedure Assign(Source : TVariableList);
    procedure CopyFrom(Source : TVariableList);
    function IdenticalTo(Source : TVariableList): Boolean;
    procedure SetGetValueMethod(aMethod : TGetVariableItemValueEvent);
    procedure SetSetValueMethod(aMethod : TSetVariableItemValueEvent);  // ПОКА НЕ ИСПОЛЬЗУЕТСЯ
    {$IFDEF DEBUG}
    procedure DebugVariablesLog(const Text: string);
    {$ENDIF}
  end;

  /// <summary>элемент постфиксной записи выражения</summary>
  TPostfixItem = class
  private
    FType       : TPostfixItemType;
    FResultType : TFieldType;
    FData       : Variant;
    FName       : String; // имена параметров функций и процедур
    FCodePage   : Integer;
    function GetIdent: string;
    procedure SetIdent(const Value: string);
    function GetIdentSQL: string;
    procedure SetIdentSQL(const Value: string);
    function GetBinaryOperation: TOperationType;
    procedure SetBinaryOperation(const Value: TOperationType);
    function GetUnaryOperation: TOperationType;
    procedure SetUnaryOperation(const Value: TOperationType);
    function GetFunc: TFunctionDescr;
    procedure SetFunc(const Value: TFunctionDescr);
    function GetCommand: String;
    procedure SetCommand(const Value: String);
    function GetVar: TVariableItem;
    procedure SetVar(const Value: TVariableItem);
    function GetLocal: TVariableItem;
    procedure SetLocal(const Value: TVariableItem);
    function GetParameter: TVariableItem;
    procedure SetParameter(const Value: TVariableItem);
  public
    property Name : String read FName write FName;
    property Data : Variant read FData;
    property CodePage: Integer read FCodePage write FCodePage;
    property ItemType : TPostfixItemType read FType;
    property ResultType : TFieldType read FResultType write FResultType;
    property Ident : string read GetIdent write SetIdent;
    property IdentSQL : string read GetIdentSQL write SetIdentSQL;
    property BinaryOp : TOperationType read GetBinaryOperation write SetBinaryOperation;
    property UnaryOp : TOperationType read GetUnaryOperation write SetUnaryOperation;
    property Func : TFunctionDescr read GetFunc write SetFunc;
    property Command : String read GetCommand write SetCommand;
    property Variable : TVariableItem read GetVar write SetVar;
    property Local : TVariableItem read GetLocal write SetLocal;
    property Parameter : TVariableItem read GetParameter write SetParameter;
    procedure SetData(const aData : Variant;  aDataType : TFieldType);
    procedure Assign(Source : TPostfixItem);
    function IdenticalTo(Source : TPostfixItem): Boolean;
  end;

  /// <summary>элемент разбора - выражение = постфикс</summary>
  TExpressionItem = class(TObjectList)
  private
    FVariables : TVariableList;
    FActive: Boolean;
    function GetItem(const Index: Integer): TPostfixItem;
    procedure SetItem(const Index: Integer; const Value: TPostfixItem);
  public
    constructor Create;
    destructor Destroy;  override;
    property Items[const Index: Integer]: TPostfixItem read GetItem write SetItem; default;
    property Variables : TVariableList read FVariables;
    function DependOnIdent(const aIdent : string) : boolean;
    function AddPostfixItem : TPostfixItem; overload;
    function AddPostfixItem(const aDataType: TFieldType): TPostfixItem; overload;
    function AddOperation(const aOperation: TOperationType; const aDataType: TFieldType = ftBoolean): TPostfixItem;
    function AddIdent(const aIdentName: String; const aDataType: TFieldType; aCodePage: Integer): TPostfixItem;
    function AddConst(const aValue: Variant; const aDataType: TFieldType): TPostfixItem;
    function AddFunc(const aIdentName: String; const ArgCount: Integer = -1): TPostfixItem;
    procedure AddCondition(const aObject: TObject; const aOperation: TOperationType; const aValue: Variant); overload;
    procedure Assign(Source : TExpressionItem);
    procedure CopyFrom(Source : TExpressionItem);  virtual;
    function  IdenticalTo(Source : TExpressionItem): Boolean;
    function GetActive: Boolean;
    property Active: Boolean read GetActive write FActive;
    {$IFDEF DEBUG}
    function PrepareDebugPostfixLog: string;
    procedure DebugPostfixLog(const Text: string);
    {$ENDIF}
    function ItemExists(const ItemTypes: TPostfixItemTypes): Boolean;
    function IndexOfIdent(const Ident: string; const FromIndex: Integer = 0): Integer;
    function IsEmpty: Boolean;
    function IsHeavy: Boolean;
  end;

  { фильтр = выражение логического типа }

   /// <summary>ошибка в фазе разбора выражения</summary>
  EDeParserError = class(Exception)
  private
    function GetErrorMessage : string;
  public
    Error     : integer;
    ErrorPos  : integer;
    Script    : string;
    ErrorData : string;
    constructor Create(aError : integer;  const aScript : string;
      aErrorPos : integer;  const aErrorData : string = '');  overload;
    constructor Create(aError : integer;
      const aErrorData : string = '');  overload;
  end;

implementation

uses Types, StrUtils, DeLog, Funcs, DeMeta, DeActions
  {$IFDEF DEBUG}, uTextTable{$ENDIF};

{$IFOPT C+}
resourcestring
  sInvalidTokenType = 'Invalid token type';
  sOperationIsUnassigned = 'Operation is unassigned';
  sInvalidPostfixItemType = 'Invalid postfix item type';
{$ENDIF}

{ TTokenInfo }

procedure TTokenInfo.SetData(const aType: TTokenType; Value: Variant);
begin
  FToken := aType;
  FTokenData := Value;
end;

function TTokenInfo.Bracket: TBracketType;
begin
  Assert(FToken in [tkLeftBracket, tkRightBracket], sInvalidTokenType);
  Result:=  TBracketType(FTokenData);
end;

function TTokenInfo.Ident : string;
begin
  Assert(FToken = tkIdent, sInvalidTokenType);
  Assert(VarToStr(FTokenData) <> EmptyStr, 'Ident token data is empty');
  result := VarToStr(FTokenData);
end;

function TTokenInfo.Operation: TOperationType;
begin
  Assert(FToken = tkOperation, sInvalidTokenType);
  result := TOperationType(FTokenData);
end;

{ TVariableItem }

constructor TVariableItem.Create(const aType: TFieldType; const aName: string; const aAccessMode: TVariableAccessMode);
begin
  inherited Create;
  FType := aType;
  FName := aName;
  FAccessMode := aAccessMode;
end;

destructor TVariableItem.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.Extract(Self);
  inherited;
end;

function TVariableItem.GetValue: Variant;
{$IFOPT C+}
resourcestring
  sGetAccessError = 'Can''t get variable value';
{$ENDIF}
begin
  Assert(amRead in FAccessMode, sGetAccessError);
  if Assigned(FOnGetValue) then
    result := FOnGetValue(Self)
  else
    result := FValue;
end;

procedure TVariableItem.SetValue(const aValue: Variant);
{$IFOPT C+}
resourcestring
  sSetAccessError = 'Can''t set variable value';
{$ENDIF}
begin
  Assert(amWrite in FAccessMode, sSetAccessError);

  if Assigned(FOnSetValue) then
    FOnSetValue(Self, aValue)
  else
    FValue := aValue;

  if (FType=ftUnknown) then
    FType:=VarTypeToDataType(VarType(aValue));
end;

procedure TVariableItem.Assign(Source : TVariableItem);
begin
  FType := Source.FType;
  FName := Source.FName;
  FAccessMode := Source.FAccessMode;
  FOnGetValue := Source.FOnGetValue;
  FOnSetValue := Source.FOnSetValue;
  FValue := Source.FValue;
  FDataSetID := Source.FDataSetID;
end;

function TVariableItem.GetCalculated: Boolean;
begin
  Result:= amCalculated in FAccessMode;
end;

procedure TVariableItem.SetCalculated(const Value: Boolean);
begin
  if Value then Include(FAccessMode, amCalculated)
           else Exclude(FAccessMode, amCalculated);
end;

function TVariableItem.GetOutput: Boolean;
begin
  Result:= amOutput in FAccessMode;
end;

procedure TVariableItem.SetOutput(const Value: Boolean);
begin
  if Value then Include(FAccessMode, amOutput)
           else Exclude(FAccessMode, amOutput);
end;

function TVariableItem.IdenticalTo(Source : TVariableItem): Boolean;
begin
  Result:=(FType = Source.FType) and
          (FName = Source.Name) and
//          (FAccessMode = Source.FAccessMode) and
//          (FOnGetValue = Source.FOnGetValue) and
//          (FOnSetValue = Source.FOnSetValue) and
          (Value = Source.Value);
//DONE: TVariableItem.IdenticalTo
end;

{ TVariableList }

function TVariableList.FindVariable(const aName: string): TVariableItem;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to Pred(Count) do
    if SameText(Items[Index].Name, aName) then Exit(Items[Index]);
end;

function TVariableList.GetVariable(const Index: Integer): TVariableItem;
begin
  Result := TVariableItem(inherited Items[Index]);
end;

procedure TVariableList.Notify(Ptr: pointer; Action: TListNotification);
begin
  case Action of
    lnAdded     : TVariableItem(Ptr).FOwner := Self;
    lnExtracted : TVariableItem(Ptr).FOwner := nil;
  end;
  inherited Notify(Ptr, Action);
end;

procedure TVariableList.SetVariable(const Index: Integer; Value: TVariableItem);
begin
  if Index >= Count then
    Count := Succ(Index);
  inherited Items[Index] := Value;
end;

procedure TVariableList.Assign(Source : TVariableList);
begin
  Clear;
  CopyFrom(Source);
end;

procedure TVariableList.CopyFrom(Source : TVariableList);
var I : integer;
begin
  for I := 0 to Source.Count-1 do
    Items[Add(TVariableItem.Create)].Assign(Source[I]);
end;

function TVariableList.IdenticalTo(Source : TVariableList): Boolean;
var I : integer;
begin
  Result:= ( Count=Source.Count);
  if Result then
    for I := 0 to Source.Count-1 do
      Result:=Result and Items[I].IdenticalTo(Source.Items[I]);
//DONE: TVariableList.IdenticalTo
end;

procedure TVariableList.SetGetValueMethod(aMethod : TGetVariableItemValueEvent);
var I : integer;
begin
  for I := 0 to Count-1 do
    Items[I].OnGetValue := aMethod;
end;

procedure TVariableList.SetSetValueMethod(aMethod : TSetVariableItemValueEvent);
var I : integer;
begin
  for I := 0 to Count-1 do
    Items[I].OnSetValue := aMethod;
end;

{$IFDEF DEBUG}
procedure TVariableList.DebugVariablesLog(const Text: string);
  function PrepareVariablesLog: string;
  var
    TextTable: TTextTable;
    VariableItem: TVariableItem;
    Index: Integer;
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
          TextTable.Columns.Add('Value', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('G', 1, taCenter, taCenter);
          TextTable.Columns.Add('S', 1, taCenter, taCenter);
          for Index := 0 to Pred(Count) do
            begin
              TextTable.Lines[Index][0] := IntToStr(Succ(Index));
              VariableItem := Items[Index];
              if Assigned(VariableItem) then
                begin
                  TextTable.Lines[Index][1] := VariableItem.FName;
                  TextTable.Lines[Index][2] := StrPas(FieldTypeNames[VariableItem.FType]);
                  TextTable.Lines[Index][3] := VariantToString(VariableItem.FValue);
                  TextTable.Lines[Index][4] := iif(Assigned(VariableItem.FOnGetValue), '+', '-');
                  TextTable.Lines[Index][5] := iif(Assigned(VariableItem.FOnSetValue), '+', '-');
                end;
            end;
          Result := #13#10 + TextTable.AsText(24);
        finally
          TextTable.Free;
        end;
      end;
  end;
begin
  DebugLog(Text + PrepareVariablesLog);
end;
{$ENDIF}

{ TPostfixItem }

function TPostfixItem.GetIdent: string;
begin
  Assert(FType = piIdent, sInvalidPostfixItemType);
  Assert(VarToStr(FData)<> EmptyStr, 'Ident postfix item data is empty');
  result := VarToStr(FData);
end;

procedure TPostfixItem.SetIdent(const Value: string);
begin
  FType := piIdent;
  FData := Value;
  FName := EmptyStr;
end;

function TPostfixItem.GetIdentSQL: string;
begin
  Assert(FType = piIdent, sInvalidPostfixItemType);
  if (Length(FName)=0) then Result := GetIdent
                       else result := FName;
end;

procedure TPostfixItem.SetIdentSQL(const Value: string);
begin
  Assert(FType = piIdent, sInvalidPostfixItemType);
  Assert(Value <> EmptyStr, 'Invalid comand name');
  FName := Value;
end;

function TPostfixItem.GetBinaryOperation: TOperationType;
begin
  Assert(FType = piBinary, sInvalidPostfixItemType);
  Assert((not VarIsEmpty(FData)) and (not VarIsNull(FData)), 'Operation postfix item data is empty');
  result := TOperationType(VarAsType(FData, varInteger));
end;

procedure TPostfixItem.SetBinaryOperation(const Value: TOperationType);
begin
  Assert(Value in BinaryOperations, 'Invalid binary operation');
  FType := piBinary;
  FData := ord(Value);
  FName := OperationLexeme[Value];
end;

function TPostfixItem.GetUnaryOperation: TOperationType;
begin
  Assert(FType = piUnary, sInvalidPostfixItemType);
  Assert((not VarIsEmpty(FData)) and (not VarIsNull(FData)), 'Operation postfix item data is empty');
  result := TOperationType(VarAsType(FData, varInteger));
end;

procedure TPostfixItem.SetUnaryOperation(const Value: TOperationType);
begin
  Assert(Value in UnaryOperations, 'Invalid unary operation');
  FType := piUnary;
  FData := ord(Value);
  FName := OperationLexeme[Value];
end;

function TPostfixItem.GetFunc: TFunctionDescr;
begin
  Assert(FType = piFunction, sInvalidPostfixItemType);
  Assert((not VarIsEmpty(FData)) and (not VarIsNull(FData)),
    'Function postfix item data is empty');
  result := TFunctionDescr(NativeInt(VarAsType(FData, {$IFDEF WIN64}varInt64{$ELSE}varInteger{$ENDIF})));
end;

procedure TPostfixItem.SetFunc(const Value: TFunctionDescr);
begin
  Assert(Value <> nil, 'Invalid function descr');
  FType := piFunction;
  FData := NativeInt(Pointer(Value));
  FResultType := Value.DataType;
end;

function TPostfixItem.GetCommand: String;
begin
  Assert(FType = piCommand, sInvalidPostfixItemType);
  Assert(Length(FName)=0,  'Command postfix item name is empty');
  result := FName;
end;

procedure TPostfixItem.SetCommand(const Value: String);
begin
  Assert(Value <> EmptyStr, 'Invalid comand name');
  FType := piCommand;
  FName := Value;
end;

function TPostfixItem.GetVar: TVariableItem;
begin
  Assert(FType = piVariable, sInvalidPostfixItemType);
  Assert((not VarIsEmpty(FData)) and (not VarIsNull(FData)), 'Variable postfix item data is empty');
  result := TVariableItem(NativeInt(VarAsType(FData, {$IFDEF WIN64}varInt64{$ELSE}varInteger{$ENDIF})));
end;

procedure TPostfixItem.SetVar(const Value: TVariableItem);
begin
  Assert(Value <> nil, 'Invalid variable item');
  FType := piVariable;
  FData := NativeInt(Pointer(Value));
  FResultType := Value.DataType;
end;

function TPostfixItem.GetLocal: TVariableItem;
begin
  Assert(FType = piLocal, sInvalidPostfixItemType);
  Assert((not VarIsEmpty(FData)) and (not VarIsNull(FData)), 'Local postfix item data is empty');
  result := TVariableItem(NativeInt(VarAsType(FData, {$IFDEF WIN64}varInt64{$ELSE}varInteger{$ENDIF})));
end;

procedure TPostfixItem.SetLocal(const Value: TVariableItem);
begin
  Assert(Value <> nil, 'Invalid local item');
  FType := piLocal;
  FName := Value.FName;
  FData := NativeInt(Pointer(Value));
  FResultType := Value.DataType;
end;

function TPostfixItem.GetParameter: TVariableItem;
begin
  Assert(FType = piParameter, sInvalidPostfixItemType);
  Assert((not VarIsEmpty(FData)) and (not VarIsNull(FData)), 'Parameter postfix item data is empty');
  result := TVariableItem(NativeInt(VarAsType(FData, {$IFDEF WIN64}varInt64{$ELSE}varInteger{$ENDIF})));
end;

procedure TPostfixItem.SetParameter(const Value: TVariableItem);
begin
  Assert(Value <> nil, 'Invalid parameter item');
  FType := piParameter;
  FData := NativeInt(Pointer(Value));
  FResultType := Value.DataType;
end;

procedure TPostfixItem.SetData(const aData : Variant; aDataType : TFieldType);
begin
  FType := piConst;
  FData := aData;
  FResultType := aDataType;
end;

procedure TPostfixItem.Assign(Source : TPostfixItem);
begin
  FName := Source.FName;
  FType := Source.FType;
  FResultType := Source.FResultType;
  FData := Source.FData;
  FCodePage := Source.CodePage;
end;

function TPostfixItem.IdenticalTo(Source : TPostfixItem): Boolean;
begin
  Result:=(FType = Source.FType) and
//        (FData = Source.FData) and
          (FResultType = Source.FResultType);
  if (Result) then
    case FType of
    piVariable: Result:= Variable.IdenticalTo(Source.Variable);
    else        Result:= (FData = Source.FData);
    end;
end;

{ TExpressionItem }

constructor TExpressionItem.Create;
begin
  inherited Create;
  FVariables := TVariableList.Create;
  FActive:= True;
end;

destructor TExpressionItem.Destroy;
begin
  FVariables.Free;
  inherited Destroy;
end;

function TExpressionItem.GetItem(const Index: Integer): TPostfixItem;
begin
  Result := TPostfixItem(inherited Items[Index]);
end;

procedure TExpressionItem.SetItem(const Index: Integer; const Value: TPostfixItem);
begin
  if Index >= Count then Count := Succ(Index);
  inherited Items[Index] := Value;
end;

function TExpressionItem.GetActive: Boolean;
begin
  Result := FActive and (Count > 0);
end;

{$IFDEF DEBUG}
function TExpressionItem.PrepareDebugPostfixLog: string;
const
  FieldTypeNames: array[TFieldType] of PChar = ('ftUnknown', 'ftString', 'ftSmallint', 'ftInteger', 'ftWord',
  'ftBoolean', 'ftFloat', 'ftCurrency', 'ftBCD', 'ftDate', 'ftTime', 'ftDateTime',
  'ftBytes', 'ftVarBytes', 'ftAutoInc', 'ftBlob', 'ftMemo', 'ftGraphic', 'ftFmtMemo',
  'ftParadoxOle', 'ftDBaseOle', 'ftTypedBinary', 'ftCursor', 'ftFixedChar', 'ftWideString',
  'ftLargeint', 'ftADT', 'ftArray', 'ftReference', 'ftDataSet', 'ftOraBlob', 'ftOraClob',
  'ftVariant', 'ftInterface', 'ftIDispatch', 'ftGuid', 'ftTimeStamp', 'ftFMTBcd',
  'ftFixedWideChar', 'ftWideMemo', 'ftOraTimeStamp', 'ftOraInterval',
  'ftLongWord', 'ftShortint', 'ftByte', 'ftExtended', 'ftConnection', 'ftParams', 'ftStream',
  'ftTimeStampOffset', 'ftObject', 'ftSingle'); //49..51
var
  TextTable: TTextTable;
  Index: Integer;
  PostfixItem: TPostfixItem;
begin
  if Count = 0 then
    Result := EmptyStr
  else
    begin
      TextTable := TTextTable.Create;
      try
        TextTable.Columns.Add('No', 0, taRightJustify, taCenter);
        TextTable.Columns.Add('Item Type', 0, taLeftJustify, taLeftJustify);
        TextTable.Columns.Add('Result Type', 0, taLeftJustify, taLeftJustify);
        TextTable.Columns.Add('Operation', 0, taLeftJustify, taLeftJustify);
        TextTable.Columns.Add('Identifier', 0, taLeftJustify, taLeftJustify);
        TextTable.Columns.Add('Value', 0, taLeftJustify, taLeftJustify);
        TextTable.Columns.Add('CodePage', 0, taRightJustify, taCenter);
        for Index := 0 to Pred(Count) do
          begin
            TextTable.Lines[Index][0] := IntToStr(Succ(Index));
            PostfixItem := Items[Index];
            if Assigned(PostfixItem) then
              begin
                TextTable.Lines[Index][1] := StrPas(PostfixItemTypeNames[PostfixItem.FType]);
                TextTable.Lines[Index][2] := StrPas(FieldTypeNames[PostfixItem.FResultType]);
                case PostfixItem.CodePage of
                  - 1:
                    TextTable.Lines[Index][6] := 'Unknown';
                  cpNone:
                    TextTable.Lines[Index][6] := 'System';
                  cp866:
                    TextTable.Lines[Index][6] := 'DOS 866';
                  cpUTF8:
                    TextTable.Lines[Index][6] := 'UTF-8';
                else
                  TextTable.Lines[Index][6] := IntToStr(PostfixItem.CodePage);
                end;
                case PostfixItem.FType of
                  piBinary:
                    begin
                      TextTable.Lines[Index][3] := OperationLexeme[PostfixItem.BinaryOp];
                      TextTable.Lines[Index][5] := VariantToString(PostfixItem.FData);
                    end;
                  piUnary:
                    begin
                      TextTable.Lines[Index][3] := OperationLexeme[PostfixItem.UnaryOp];
                      TextTable.Lines[Index][5] := VariantToString(PostfixItem.FData);
                    end;
                  piIdent:
                    begin
                      TextTable.Lines[Index][4] := PostfixItem.Ident;
                      TextTable.Lines[Index][5] := VariantToString(PostfixItem.FData);
                    end;
                  piVariable:
                    begin
                      TextTable.Lines[Index][4] := PostfixItem.Variable.Name;
                      TextTable.Lines[Index][5] := VariantToString(PostfixItem.Variable.Value);
                    end;
                  piParameter:
                    begin
                      TextTable.Lines[Index][4] := PostfixItem.Parameter.Name;
                      TextTable.Lines[Index][5] := VariantToString(PostfixItem.Parameter.Value);
                    end;
                  piFunction:
                    begin
                      TextTable.Lines[Index][4] := PostfixItem.Func.Name;
                      TextTable.Lines[Index][5] := StrPas(FieldTypeNames[PostfixItem.Func.DataType]);
                    end;
                  piLocal, piCommand:
                    TextTable.Lines[Index][4] := PostfixItem.Name;
                else
                  TextTable.Lines[Index][5] := VariantToString(PostfixItem.FData);
                end;
              end;
          end;
        Result := #13#10 + TextTable.AsText(24);
      finally
        TextTable.Free;
      end;
    end;
end;

procedure TExpressionItem.DebugPostfixLog(const Text: string);
begin
  DebugLog(Text + PrepareDebugPostfixLog);
end;
{$ENDIF}

function TExpressionItem.DependOnIdent(const aIdent: string): boolean;
var I : integer;
begin
  result := false;

  { ищем идентификатор среди переменных }
  for I := 0 to FVariables.Count-1 do
    if Assigned(FVariables[I]) and (CompareText(FVariables[I].Name, aIdent) = 0) then
        begin
          result := true;
          break;
        end;

  { ищем идентификатор в выражении }
  if not result then
    for I := 0 to Count-1 do
      if Assigned(Items[I]) and (Items[I].ItemType = piIdent) and (CompareText(Items[I].Ident, aIdent) = 0)then
          begin
            result := true;
            break;
          end;

  { ищем идентификатор среди переменных по ссылке}
  if not result then
    for I := 0 to FVariables.Count-1 do
      if Assigned(FVariables[I]) and (Pos(aIdent+'.',FVariables[I].Name ) = 1 ) then
          begin
            result := true;
            break;
          end;
end;

function TExpressionItem.AddPostfixItem: TPostfixItem;
begin
  result := Items[Add(TPostfixItem.Create)];
end;

function TExpressionItem.AddPostfixItem(const aDataType: TFieldType): TPostfixItem;
begin
  result := Items[Add(TPostfixItem.Create)];
  result.FResultType:= aDataType;
end;

function TExpressionItem.AddOperation(const aOperation: TOperationType; const aDataType: TFieldType): TPostfixItem;
begin
  result := AddPostfixItem;
  if aOperation in BinaryOperations then result.BinaryOp := aOperation
                                    else result.UnaryOp := aOperation;
  result.FResultType := aDataType;
end;

function TExpressionItem.AddIdent(const aIdentName: String; const aDataType: TFieldType; aCodePage: Integer): TPostfixItem;
begin
  result := AddPostfixItem;
  result.Ident:= aIdentName;
  result.ResultType := aDataType;
  result.CodePage:= aCodePage;
end;

function TExpressionItem.AddConst(const aValue: Variant; const aDataType: TFieldType): TPostfixItem;
  Function ArrayAsType(const V: Variant; AVarType: TVarType): variant;
  var i: Integer;
  begin
    if VarType(V) = (varArray or varVariant) then
      begin
        Result:= VarArrayCreate([VarArrayLowBound(V, 1), VarArrayHighBound(V, 1)], varVariant);
        for i:= VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
          Result[i]:= VarAsType(V[i], AVarType);
        Result:= V;
      end
    else
      Result:= VarAsType(V, AVarType);
  end;
begin
  result := AddPostfixItem;
  if VarIsNull(aValue) then            result.SetData( aValue, aDataType) else
  if aDataType in IntegerTypes then    result.SetData( ArrayAsType(aValue, varInt64), ftInteger) else
  if aDataType in FloatTypes then      result.SetData( ArrayAsType(aValue, varDouble), ftFloat) else
  if aDataType in WideStringTypes then result.SetData( ArrayAsType(aValue, varString), ftWideString) else
  if aDataType in StringTypes then     result.SetData( ArrayAsType(aValue, varString), ftString) else
  if aDataType in [ftTime] then        result.SetData( ArrayAsType(aValue, varDate), ftTime) else
  if aDataType in [ftDate] then        result.SetData( ArrayAsType(aValue, varDate), ftDate) else
  if aDataType in DateTimeTypes then   result.SetData( ArrayAsType(aValue, varDate), ftDateTime) else
  if aDataType = ftBoolean then        result.SetData( ArrayAsType(aValue, varBoolean), ftBoolean) else
  if aDataType = ftGUID then           result.SetData( ArrayAsType(aValue, varString), ftGUID);
end;

function TExpressionItem.AddFunc(const aIdentName: String; const ArgCount: Integer = -1): TPostfixItem;
var i: Integer;
begin
  Result:= nil;
  for i:= 0 to Pred(FunctionList.Count) do
    if SameText(FunctionList.Items[i].Name, aIdentName) then
     if (ArgCount = -1) or (ArgCount = FunctionList.Items[i].ArgCount) then
      begin
        result:= AddPostfixItem;
        result.Func:= FunctionList.Items[i];
        Break;
      end;

  if (Result = nil) then
    raise Exception.Create('Function '''+aIdentName+''' not found');
end;

procedure TExpressionItem.AddCondition(const aObject: TObject; const aOperation: TOperationType; const aValue: Variant);
var FM: TFieldMeta;
begin
  Assert( aObject is TFieldMeta, 'AddCondition: TObject type is not TFieldMeta');
  FM:= TFieldMeta(aObject);

  { поле }
  AddIdent(FM.Original, FM.DataType, FM.CodePage);

  { значение - если операция IsNull то параметр не нужен}
  if aOperation in BinaryOperations then
    AddConst(aValue, FM.DataType);

  { операция }
  AddOperation(aOperation);
end;

procedure TExpressionItem.CopyFrom(Source : TExpressionItem);
var I, N, VarIndex : integer;
begin
  FVariables.CopyFrom(TExpressionItem(Source).Variables);
  for I := 0 to Source.Count-1 do
  begin
    N:= Add(TPostfixItem.Create);
    Items[N].Assign(TExpressionItem(Source)[I]);

    if Items[N].ItemType = piVariable then
    begin
      VarIndex := TExpressionItem(Source).Variables.IndexOf(
        TExpressionItem(Source)[I].Variable);
      if VarIndex >= 0 then
        Items[N].Variable := FVariables[VarIndex];
    end;
  end;

  FActive := Source.FActive;
end;

procedure TExpressionItem.Assign(Source: TExpressionItem);
begin
  Clear;
  CopyFrom(Source);
end;

function  TExpressionItem.IdenticalTo(Source : TExpressionItem): Boolean;
var I : integer;
begin
  Result:= (FVariables.IdenticalTo(TExpressionItem(Source).Variables));
  Result:=Result and (Count=Source.Count);
  if Result then
    for I := 0 to Source.Count-1 do
      Result:= Result and Items[I].IdenticalTo(TExpressionItem(Source).Items[I]);
end;

function TExpressionItem.ItemExists(const ItemTypes: TPostfixItemTypes): Boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to Pred(Count) do
    if Assigned(Items[Index]) and (Items[Index].FType in ItemTypes) then
      begin
        Result := True;
        Break;
      end;
end;

function TExpressionItem.IndexOfIdent(const Ident: string; const FromIndex: Integer): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := FromIndex to Pred(Count) do
    if Assigned(Items[Index]) and (Items[Index].FType = piIdent) and SameText(Items[Index].Ident, Ident) then
      begin
        Result := Index;
        Break;
      end;
end;

function TExpressionItem.IsEmpty: Boolean;
begin
  Result:= (Count=0)
end;

function TExpressionItem.IsHeavy: Boolean;
var i: Integer;
begin
  Result:= False;
  for i:= 0 to Pred(Count) do
    if Items[i].FType = piFunction then
      if Items[i].Func.IsHeavy then Exit(True);
end;

{ TParserError }

constructor EDeParserError.Create(aError: integer; const aScript: string;
  aErrorPos: integer; const aErrorData: string = '');
begin
  Error := aError;
  ErrorPos := aErrorPos;
  Script := aScript;
  ErrorData := aErrorData;
  inherited Create(GetErrorMessage);
end;

constructor EDeParserError.Create(aError: integer;  const aErrorData: string = '');
begin
  Error := aError;
  ErrorData := aErrorData;
  inherited Create(GetErrorMessage);
end;

function EDeParserError.GetErrorMessage: string;
begin
  result := ParserErrorMessage[Error];
  case Error of
    Error_IllegalSymbol :
      result := Format(result, [ErrorData, ErrorPos, Script]);
    Error_UnexpectedEndOfScript :
      result := Format(result, [Script]);
    Error_BracketExpected,
    Error_IncompatibleTypes,
    Error_InvalidArgType,
    Error_InvalidOperandType :
      result := Format(result, [ErrorPos, Script]);
    Error_UnterminatedString,
    Error_UnknownIdent,
    Error_UnknownFunction,
    Error_InvalidArgNumber,
    Error_InvalidConst :
      result := Format(result, [ErrorData]);
  end;
end;


initialization
  {$IFDEF DEBUG}
  DebugLog('DeParser unit initialization ...');
  {$ENDIF}
finalization
  {$IFDEF DEBUG}
  DebugLog('DeParser unit finalization ...');
  {$ENDIF}

end.
