unit DeScript;

interface

uses Classes, SysUtils, DB, Contnrs, {DateUtils,}
     DeFunctions, DeTypes, DeParser, DeMeta, Variants;

const
  UserID        = 'User.ID';
  UserLogin     = 'User.Login';
  UserName      = 'User.Name';
  UserAdmin     = 'User.Admin';
  UserWorker    = 'User.Worker';
  UserWorkerRec =  UserWorker+'.';

  // v. 19.02
  cMetadataID       = 'Metadata.ID';
  cMetadataType     = 'Metadata.Type';
  cMetadataAlias    = 'Metadata.Alias';
  cMetadataServer   = 'Metadata.Server';
  cMetadataDatabase = 'Metadata.Database';
  cMetadataLogin    = 'Metadata.Login';
  cMetadataPassword = 'Metadata.Password';

  cFolderTemp       = 'Folder.Temp';
  cFolderOut        = 'Folder.Out';
  cFolderMap        = 'Folder.Map';
  cFolderLog        = 'Folder.Log';

type
  { лексический анализатор }
  TLexicalAnalyzer = class
  private
    FScript: string;
    FTokens: TObjectList;
    FScriptLength: integer;
    FCurrentChar: char;
    FCurrentPos: integer;
    FEndOfScript: boolean;
    function NextChar : char;
    // пропускает пробелы, табуляции и т.п.
    procedure SkipSpaces;
    // читает строковую константу
    function ReadString : string;
    // читает строку цифровых символов
    function ReadChars(const Chars : TSetOfChar) : string;
    // читает числовую константу (целую, либо вещественную)
    procedure GetNumericToken(aToken : TTokenInfo);
    // проверяет, совпадает ли строка с образцом
    function CheckString(const Template : string) : boolean;
    // читает идентификатор
    function ReadIdent : string;
    function GetToken(const Index: Integer): TTokenInfo;
    function GetTokenCount: Integer;
    procedure SetScript(const aScript : string);
  protected
    /// <summary>добавляет описатель нового Token'а</summary>
    function AddTokenInfo: TTokenInfo;
    /// <summary>сброс результатов разбора</summary>
    procedure Clear;
    /// <summary>инициализация лексического анализатора</summary>
    procedure Initialize;  virtual;
    /// <summary>разбор кода на составляющие</summary>
    procedure ParsScript;  virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Script: string read FScript write SetScript;
    property Tokens[const Index: Integer]: TTokenInfo read GetToken;
    property TokenCount: Integer read GetTokenCount;
  end;

  { построение дерева разбора без использования идентификаторов }
  TDeSimpleParser = class
  private
    FExpression : TExpressionItem;
    FLexicalAnalyzer : TLexicalAnalyzer;
    FTokenIndex      : integer;
    FEndOfScript     : boolean;
    FEndOfStatement  : boolean;
    FCurrentToken    : TTokenInfo;
    FNextToken       : TTokenInfo;
    function NextToken: TTokenInfo;
    procedure SetTokenIndex(aIndex: Integer);
    procedure CheckEndOfStatement;
    Function ReadFunctionArgs: TArrayArguments;
    function Factor : TPostfixItem;
    function Expr(Level: Integer) : TPostfixItem;
    function GetScript : string;
    procedure SetScript(const Value : string);
    procedure ParsScript;
  protected
    FInnerVar : TVariableList;
    /// <summary>создает лексический анализатор</summary>
    function CreateLexicalAnalyzer : TLexicalAnalyzer;  virtual;
    /// <summary>вызывается при обнаружении переменной</summary>
    function ExecVariable(const aVariable : string) : TPostfixItem;  virtual;
    /// <summary>вызывается при обнаружении идентификатора</summary>
    function ExecIdent(const aIdent: string): TPostfixItem; virtual; abstract;
    /// <summary>вызывается при оращении к локальной переменной</summary>
    function ExecLocal(const aVariable : string; autoCreate: Boolean = False): TPostfixItem;  virtual;

    /// <summary>вызывается при обнаружении константы</summary>
    function ExecConst(const aConst: Variant; const aType: TFieldType): TPostfixItem; virtual;
    /// <summary>вызывается при обнаружении унарной операции</summary>
    function ExecUnary(aUnaryOp : TOperationType; aRight : TPostfixItem) : TPostfixItem;  virtual;
    /// <summary>вызывается при обнаружении бинарной операции</summary>
    function ExecBinary(aBinaryOp : TOperationType; aLeft, aRight : TPostfixItem) : TPostfixItem;  virtual;
    /// <summary>вызывается при обнаружении команды</summary>
    function ExecCommand(const aCommand: string): TPostfixItem; virtual;
    /// <summary>вызывается при обнаружении функции</summary>
    function ExecFunction(const aFunction: TFunctionDescr; aArguments: TArrayArguments): TPostfixItem; virtual;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
    property Script : string read GetScript;
    procedure Parse(const aScript : string;  aData : TExpressionItem);  virtual;
    property TokenIndex: Integer read FTokenIndex write SetTokenIndex;
  end;

  { построение дерева разбора с идентификаторами - полями набора данных и внешним списком переменных}
  TDeParser = class(TDeSimpleParser)
  private
    FTable : TTableMeta;
    /// указатель на объект содержащий данные, необходим при рекурсивном поиске
    FObjectPointer: pointer;
    FonGetVariable : TGetVariableEvent;
    FonGetIdentType : TGetIdentTypeEvent;
  protected
    function ExecVariable(const aVariable : string) : TPostfixItem;  override;
    function ExecIdent(const aIdent : string) : TPostfixItem;  override;
  public
    property Table : TTableMeta read FTable write FTable;
    property ObjectPointer : pointer read FObjectPointer write FObjectPointer;
    property onGetVariable : TGetVariableEvent read FonGetVariable write FonGetVariable;
    property onGetIdentType : TGetIdentTypeEvent read FonGetIdentType write FonGetIdentType;
  end;

  { список глобальных переменных }
  TGlobalVariables = class(TVariableList)
  private
    function GetUserID(aVariable : TVariableItem) : Variant;
    function GetUserLogin(aVariable : TVariableItem) : Variant;
    function GetUserName(aVariable : TVariableItem) : Variant;
    function GetUserIsAdmin(aVariable: TVariableItem) : Variant;
    function GetUserWorker(aVariable: TVariableItem) : Variant;
    function GetWorkerInfo(aVariable: TVariableItem): Variant;
    function GetMetadataID(aVariable: TVariableItem): Variant;
    function GetMetadataType(aVariable: TVariableItem): Variant;
    function GetMetadataAlias(aVariable: TVariableItem): Variant;
    function GetMetadataServer(aVariable: TVariableItem): Variant;
    function GetMetadataDatabase(aVariable: TVariableItem): Variant;
    function GetMetadataLogin(aVariable: TVariableItem): Variant;
    function GetMetadataPassword(aVariable: TVariableItem): Variant;
    function GetFolderOut(aVariable : TVariableItem): Variant;
    function GetFolderTemp(aVariable : TVariableItem): Variant;
    function GetFolderMap(aVariable : TVariableItem): Variant;
    function GetFolderLog(aVariable : TVariableItem): Variant;
  protected
  public
    constructor Create;
    function FindVariable(const aName : string) : TVariableItem; override;
  end;

var
  GlobalVariables: TGlobalVariables;

implementation

uses DeLog, Funcs, Security,
     DataUnit, DeDB, DeMetaData, QueryDescriptor, DataCacheUnit, DeActions, DeSettings;

{ TLexicalAnalyzer }

function TLexicalAnalyzer.NextChar : char;
begin
  if FEndOfScript then
    raise EDeParserError.Create(Error_EndOfScript);
  FCurrentChar := Script[FCurrentPos];
  inc(FCurrentPos);
  FEndOfScript := (FCurrentPos > FScriptLength);
  result := FCurrentChar;
end;

procedure TLexicalAnalyzer.SetScript(const aScript: string);
var TrimScript : string;
begin
  TrimScript := Trim(aScript);
  if Length(TrimScript) = 0 then
    raise EDeParserError.Create(Error_EmptyScript);
  Clear;
  FScript := TrimScript+#0;
  Initialize;
  ParsScript;
end;

procedure TLexicalAnalyzer.SkipSpaces;
begin
  if FCurrentChar <= #32 then
    while (not FEndOfScript) and (NextChar <= #32) do;
end;

function TLexicalAnalyzer.ReadString : string;
var Commas      : char;
    EndOfString : boolean;
begin
  { читаем строку, пока не найдем завершающую одинарную кавычку }
  Commas := FCurrentChar;
  result := EmptyStr;
  EndOfString := false;
  while (not FEndOfScript) and (not EndOfString) do
  begin
    while (not FEndOfScript) and (NextChar <> Commas) do
      if not FEndOfScript then
        result := result + FCurrentChar;
    if not FEndOfScript then
      if FCurrentChar = Commas then
        if NextChar = Commas then
          result := result + Commas
        else
          EndOfString := true;
  end;
  if FEndOfScript and (not EndOfString) then
    raise EDeParserError.Create(Error_UnterminatedString, result);
end;

function TLexicalAnalyzer.ReadChars(const Chars : TSetOfChar) : string;
begin
  Assert(Chars <> [], 'Empty set of char');
  { читаем символы из указанного множества }
  if CharInSet(FCurrentChar, Chars) then
  begin
    result := FCurrentChar;
    while (not FEndOfScript) and CharInSet(NextChar, Chars) do
      result := result + FCurrentChar;
  end
  else
    result := EmptyStr;
end;

function HexToInt(const Hex : string) : integer;
var HexValue : string;
    HexLen, I : integer;
begin
  HexValue := LowerCase(Hex);
  result := 0;
  HexLen := Length(HexValue);
  for I := HexLen downto 1 do
    case HexValue[I] of
    '0'..'9': inc(result, (ord(HexValue[I])-ord('0')) * (1 shl ((HexLen - I)*4)));
    'a'..'f': inc(result, (10 + ord(HexValue[I])-ord('a')) * (1 shl ((HexLen - I)*4)));
    end;
end;

procedure TLexicalAnalyzer.GetNumericToken(aToken : TTokenInfo);
var NumericString       : string;
    lDataType           : TVarType;
    BackupDS, StartChar : char;
begin
  Assert(aToken <> nil, 'Empty argument');
  { читаем числовую константу }
  StartChar := FCurrentChar;
  if (FCurrentChar = '0') and CharInSet(NextChar, ['x', 'X']) then
    begin
      { шестнадцатеричная константа }
      NextChar;
      NumericString := ReadChars(HexDigits);
      if (Length(NumericString) div 2 + Length(NumericString) mod 2) > SizeOf(integer) then
        raise EDeParserError.Create(Error_InvalidConst, NumericString);
      aToken.SetData(tkInteger, HexToInt(NumericString) );
    end
  else
    begin
      { целочисленная, дата, либо вещественная константа }
      lDataType := varInteger;

      if StartChar <> '0' then
        NumericString := ReadChars(Digits)
      else
        NumericString := '0' + ReadChars(Digits);
      if (not FEndOfScript) and (FCurrentChar = DeDecimalSeparator) then
        begin
          { читаем разделитель целой и дробной частей и дробную часть }
          lDataType := varDouble;
          NumericString := NumericString + FCurrentChar;
          if CharInSet(NextChar, Digits) then
            NumericString := NumericString + ReadChars(Digits);
        end;
      { числовая константа содержит экспоненциальную часть }
      if (not FEndOfScript) and CharInSet(FCurrentChar, ['e', 'E']) then
      begin
        lDataType := varDouble;
        NumericString := NumericString + FCurrentChar;
        if CharInSet(NextChar, ['+', '-']) then
          begin
            NumericString := NumericString + FCurrentChar;
            if CharInSet(NextChar, Digits) then
              NumericString := NumericString + ReadChars(Digits)
            else
              raise EDeParserError.Create(Error_IllegalSymbol, Script, FCurrentPos-1, FCurrentChar);
          end
        else
          raise EDeParserError.Create(Error_IllegalSymbol, Script, FCurrentPos-1, FCurrentChar);
      end;

      if lDataType = varDouble then
        if (not FEndOfScript) and (FCurrentChar = DeDecimalSeparator) then
          begin
            lDataType := varDate;
            NumericString := NumericString + FCurrentChar;
            if CharInSet(NextChar, Digits) then
              NumericString := NumericString + ReadChars(Digits)
            else
              raise EDeParserError.Create(Error_IllegalSymbol, Script, FCurrentPos-1, FCurrentChar);
          end;

      if lDataType = varDouble then
        begin
          BackupDS := FormatSettings.DecimalSeparator;
          FormatSettings.DecimalSeparator := DeDecimalSeparator;
          try
            try
              aToken.SetData(tkFloat, StrToFloat(Trim(NumericString)) );
            except
              raise EDeParserError.Create(Error_InvalidConst, NumericString);
            end;
          finally
            FormatSettings.DecimalSeparator := BackupDS;
          end;
        end;

      if lDataType = varInteger then
        try
          aToken.SetData(tkInteger, StrToInt64(Trim(NumericString)) );
        except
          raise EDeParserError.Create(Error_InvalidConst, NumericString);
        end;

      if lDataType = varDate then
        begin
          try
            aToken.SetData(tkDateTime, StrToDate(Trim(NumericString)) );
          except
            raise EDeParserError.Create(Error_InvalidConst, NumericString);
          end;
        end;
    end;
end;

function TLexicalAnalyzer.GetToken(const Index: Integer): TTokenInfo;
begin
  Result := TTokenInfo(FTokens[Index]);
end;

function TLexicalAnalyzer.GetTokenCount: Integer;
begin
  Result := FTokens.Count;
end;

function TLexicalAnalyzer.AddTokenInfo: TTokenInfo;
begin
  Result := TTokenInfo.Create;
  if FTokens.Add(result) = -1 then FreeAndNil(Result);
end;

function TLexicalAnalyzer.CheckString(const Template : string) : boolean;
var I : integer;
begin
  Assert(Template <> EmptyStr, 'Empty argument');

  // шаблон <= выражения
  result := FCurrentPos-1+Length(Template) <= Length(Script);

  // шаблон <  выражения (следующий символ в выраждении должен быть разделителем)
  if FCurrentPos-1+Length(Template) < Length(Script) then
    Result:=Result and not CharInSet(Script[FCurrentPos-1+Length(Template)], IdentChars);
  I := 2;

  while (Result) and (I <= Length(Template)) do
  begin
    if UpCase(Script[FCurrentPos-2+I]) = UpCase(Template[I]) then
      inc(I)
    else
      result := false;
  end;

  if Result then
    begin
      Inc(FCurrentPos,Length(Template));
      FCurrentChar:=Script[FCurrentPos-1];
      FEndOfScript := (FCurrentPos > FScriptLength);
    end;
end;

function TLexicalAnalyzer.ReadIdent : string;
begin
  if CharInSet(FCurrentChar, IdentChars) then
  begin
    result := FCurrentChar;
    while (not FEndOfScript) and CharInSet(NextChar, IdentChars) do
      result := result + FCurrentChar;
  end;
end;

procedure TLexicalAnalyzer.Clear;
begin
  FTokens.Clear;
  SetLength(FScript, 0);

  FScriptLength := 0;
  FCurrentChar := #0;
  FCurrentPos := 0;
  FEndOfScript := false;
end;

constructor TLexicalAnalyzer.Create;
begin
  FTokens := TObjectList.Create;
end;

destructor TLexicalAnalyzer.Destroy;
begin
  FTokens.Free;
end;

procedure TLexicalAnalyzer.Initialize;
begin
  FScriptLength := Length(Script);
  FCurrentPos := 1;
end;

procedure TLexicalAnalyzer.ParsScript;
var CurrentToken  : TTokenInfo;
    Brackets : string;
begin
  Brackets:='*';

  while not FEndOfScript do
  begin
    CurrentToken := AddTokenInfo;
    SkipSpaces;
    CurrentToken.TokenPos := FCurrentPos-1;
    if not FEndOfScript then

      if CharInSet(FCurrentChar, Digits) then GetNumericToken(CurrentToken) else

      if CharInSet(FCurrentChar, CommasChars) then CurrentToken.SetData(tkString, ReadString ) else

      begin
        case UpCase(FCurrentChar) of
          #9,#10,#13 :
                begin NextChar;
                end;
          '(' : begin CurrentToken.SetData(tkLeftBracket,  btSimple );
                      Brackets:= FCurrentChar+Brackets;
                      NextChar;
                end;
          '{' : begin CurrentToken.SetData(tkLeftBracket,  btFigure );
                      Brackets:= FCurrentChar+Brackets;
                      NextChar;
                end;
          '[' : begin CurrentToken.SetData(tkLeftBracket,  btSquare );
                      Brackets:= FCurrentChar+Brackets;
                      NextChar;
                end;

          ')' : begin CurrentToken.SetData(tkRightBracket, btSimple );
                      if Brackets[1]='(' then Delete(Brackets,1,1)
                                         else raise EDeParserError.Create(Error_BracketExpected, Script, FCurrentPos);
                      NextChar;
                end;
          '}' : begin CurrentToken.SetData(tkRightBracket, btFigure );
                      if Brackets[1]='{' then Delete(Brackets,1,1)
                                         else raise EDeParserError.Create(Error_BracketExpected, Script, FCurrentPos);
                      NextChar;
                end;
          ']' : begin CurrentToken.SetData(tkRightBracket, btSquare );
                      if Brackets[1]='[' then Delete(Brackets,1,1)
                                         else raise EDeParserError.Create(Error_BracketExpected, Script, FCurrentPos);
                      NextChar;
                end;

          ',' : begin CurrentToken.SetData(tkComma, unassigned);        NextChar; end;

          '+' : begin CurrentToken.SetData(tkOperation, opPlus );       NextChar; end;
          '-' : begin CurrentToken.SetData(tkOperation, opMinus);       NextChar; end;
          '*' : begin CurrentToken.SetData(tkOperation, opMul);         NextChar; end;
          '/' : begin CurrentToken.SetData(tkOperation, opDiv);         NextChar; end;
          '|' : begin CurrentToken.SetData(tkOperation, opArray);       NextChar; end;
          ';' : begin CurrentToken.SetData(tkOperation, opNext);        NextChar; end;

          ':' : begin CurrentToken.SetData(tkOperation, opAssign);      NextChar;
                      if FCurrentChar = '=' then                        NextChar; // ":"  =  ":="
                end;
          '=' : begin CurrentToken.SetData(tkOperation, opEQ);          NextChar;
                      if FCurrentChar = '<' then begin CurrentToken.SetData(tkOperation, opLE); NextChar; end else
                      if FCurrentChar = '>' then begin CurrentToken.SetData(tkOperation, opGE); NextChar; end
                end;
          '<' : begin CurrentToken.SetData(tkOperation, opLT);          NextChar;
                      if FCurrentChar = '=' then begin CurrentToken.SetData(tkOperation, opLE); NextChar; end else
                      if FCurrentChar = '>' then begin CurrentToken.SetData(tkOperation, opNE); NextChar; end else
                      if FCurrentChar = '<' then                        NextChar; // "<"  =  "<<"
                end;
          '>' : begin CurrentToken.SetData(tkOperation, opGT);          NextChar;
                      if FCurrentChar = '=' then begin CurrentToken.SetData(tkOperation, opGE); NextChar; end else
                      if FCurrentChar = '<' then begin CurrentToken.SetData(tkOperation, opNE); NextChar; end else
                      if FCurrentChar = '>' then                        NextChar; // ">"  =  ">>"
                end;

          'A': if CheckString(OperationLexeme[opAnd])    then CurrentToken.SetData(tkOperation, opAnd);
          'D': if CheckString(OperationLexeme[opIntDiv]) then CurrentToken.SetData(tkOperation, opIntDiv);
          'F': if CheckString(FalseLexeme)               then CurrentToken.SetData(tkBoolean, False);
          'I': if CheckString(OperationLexeme[opIN])     then CurrentToken.SetData(tkOperation, opIN) else
               if CheckString(OperationLexeme[opIS])     then
                                                           begin
                                                             CurrentToken.SetData(tkOperation, opIS);
                                                             SkipSpaces;
                                                             if CheckString(OperationLexeme[opNot]) then
                                                               CurrentToken.SetData(tkOperation, opISNOT);
                                                           end;
          'L': if CheckString(OperationLexeme[opLike])   then CurrentToken.SetData(tkOperation, opLike);
          'M': if CheckString(OperationLexeme[opMod])    then CurrentToken.SetData(tkOperation, opMod);
          'N': if CheckString(NullLexeme)                then CurrentToken.SetData(tkNull, Null) else
               if CheckString(OperationLexeme[opNot])    then
                 begin
                   CurrentToken.SetData(tkOperation, opNot);
                   SkipSpaces;
                   if CheckString(OperationLexeme[opIn])     then CurrentToken.SetData(tkOperation, opNotIn) else
                   if CheckString(OperationLexeme[opLike])   then CurrentToken.SetData(tkOperation, opNotLike);
                 end;
          'O': if CheckString(OperationLexeme[opOr])     then CurrentToken.SetData(tkOperation, opOr);
          'T': if CheckString(TrueLexeme)                then CurrentToken.SetData(tkBoolean, True);
          'U': if CheckString(UnassignedLexeme)          then CurrentToken.SetData(tkNull, unassigned);
          'X': if CheckString(OperationLexeme[opXor])    then CurrentToken.SetData(tkOperation, opXor);
        end;

        // ничего не обнаружили ищем Ident
        if (CurrentToken.Token = tkNone) then
          if CharInSet(FCurrentChar, IdentFirstChars) then
            CurrentToken.SetData( tkIdent, ReadIdent);
      end;

    if CurrentToken.Token = tkNone then
      raise EDeParserError.Create(Error_IllegalSymbol, Script, FCurrentPos, FCurrentChar);
  end;

  if Not(Brackets='*') then
   raise EDeParserError.Create(Error_BracketExpected, Script, FCurrentPos);
end;

{ TDeSimpleParser }

constructor TDeSimpleParser.Create;
begin
  inherited Create;
  FInnerVar := TVariableList.Create;
  FLexicalAnalyzer := CreateLexicalAnalyzer;
  Assert(FLexicalAnalyzer<>nil, 'Lexical analyzer not created');
end;

destructor TDeSimpleParser.Destroy;
begin
  FInnerVar.Free;
  FLexicalAnalyzer.Free;
  inherited Destroy;
end;

procedure TDeSimpleParser.CheckEndOfStatement;
begin
  if FEndOfStatement then
    raise EDeParserError.Create(Error_UnexpectedEndOfStatement)
  else if FEndOfScript then
    raise EDeParserError.Create(Error_UnexpectedEndOfScript, Script, Length(Script));
end;

procedure TDeSimpleParser.Clear;
begin
  FLexicalAnalyzer.Clear;
  FTokenIndex := 0;
  FEndOfScript := false;
  FEndOfStatement := false;
  FCurrentToken  := nil;
  FNextToken := nil;
end;

function TDeSimpleParser.CreateLexicalAnalyzer : TLexicalAnalyzer;
begin
  result := TLexicalAnalyzer.Create;
end;

function TDeSimpleParser.ExecVariable(const aVariable : string) : TPostfixItem;
var VarItem : TVariableItem;
    N: Integer;
begin
  Assert(aVariable <> EmptyStr, 'Variable name is empty');

  result:=nil;

  // ищем в списке глобальных переменных
  VarItem := GlobalVariables.FindVariable(aVariable);
  if Assigned(VarItem) then
    begin
      result := TPostfixItem.Create;
      result.Variable := VarItem;
      FExpression.Add(result);
      Exit;
    end;

  // ищем в списке глобальных параметров
  N:= MetaData.Parameters.ParamIndex(aVariable);
  if -1 < N then
    begin
      result:= TPostfixItem.Create;
      VarItem:= TVariableItem.Create(MetaData.Parameters[N].DataType, aVariable, [amRead, amWrite]);
      VarItem.Value:= MetaData.Parameters[N].Value;
      result.Variable := VarItem;
      FExpression.Add(result);
    end;
end;

function TDeSimpleParser.ExecLocal(const aVariable : string; autoCreate: Boolean = False) : TPostfixItem;
Var VarItem: TVariableItem;
begin
  Assert(aVariable <> EmptyStr, 'Variable name is empty');

  VarItem:= FInnerVar.FindVariable(aVariable);
  if autoCreate and (Not Assigned(VarItem)) then
    begin
      VarItem:= TVariableItem.Create(ftUnknown, aVariable, [amRead, amWrite]);
      FInnerVar.Add(VarItem);
    end;

  if assigned(VarItem) then
    begin
      result := TPostfixItem.Create;
      result.Local := VarItem;
      FExpression.Add(result);
    end
  else
    raise Exception.Create('Local Variable '+aVariable+' not exist');
end;

function TDeSimpleParser.ExecFunction(const aFunction: TFunctionDescr; aArguments: TArrayArguments): TPostfixItem;
var CheckRes : integer;
    ResType     : TFieldType;
begin
  Assert(aFunction <> nil, 'Function is null');

  result := TPostfixItem.Create;
  result.Func := aFunction;
  try
    CheckRes := result.Func.CheckSyntax(aArguments, ResType);
    if CheckRes = -1 then
      raise EDeParserError.Create(Error_InvalidArgNumber, aFunction.Name)
    else if CheckRes <> 0 then
      raise EDeParserError.Create(Error_InvalidArgType);
    result.ResultType := ResType;
    FExpression.Add(result);
  except
    FreeAndNil(result);
    raise;
  end;
end;

function TDeSimpleParser.Expr(Level: Integer): TPostfixItem;
var Left, Right : TPostfixItem;
    Operation   : TOperationType;
    ErrorPos    : integer;
begin
  try
    if Level=0 then Left := Factor
               else Left := Expr(Level-1);
    while (not FEndOfStatement) and
       (FCurrentToken.Token = tkOperation) and
       (FCurrentToken.Operation in OperationPriority[Level])
    do begin
         Operation := FCurrentToken.Operation;
         NextToken;
         CheckEndOfStatement;
         if Level=0 then Right := Factor
                    else Right := Expr(Level-1);
         Left := ExecBinary(Operation, Left, Right);
       end;
    result := Left;
  except
    on E: EDeParserError do
    begin
      if Assigned(FCurrentToken) then ErrorPos := FCurrentToken.TokenPos
                                 else ErrorPos := Length(Script);
      raise EDeParserError.Create(E.Error, Script, ErrorPos, E.ErrorData);
    end;
  end;
end;

function TDeSimpleParser.Factor: TPostfixItem;
var IdentName: string;
    FunctionArgs: TArrayArguments;
    HaveBracket: Boolean;
    Operation: TOperationType;
    LastError, ErrorPos, nTokenIndex, nComm, i : integer;
begin
  result:= nil;
  try
    case FCurrentToken.Token of
      tkString :
        begin
          result := ExecConst(FCurrentToken.Data, ftString);
          NextToken;
        end;
      tkInteger :
        begin
          result := ExecConst(FCurrentToken.Data, ftInteger);
          NextToken;
        end;
      tkFloat :
        begin
          result := ExecConst(FCurrentToken.Data, ftFloat);
          NextToken;
        end;
      tkDateTime :
        begin
          result := ExecConst(FCurrentToken.Data, ftDateTime);
          NextToken;
        end;
      tkBoolean :
        begin
          result := ExecConst(FCurrentToken.Data, ftBoolean);
          NextToken;
        end;
      tkNull :
        begin
          result := ExecConst(Null, ftUnknown);
          NextToken;
        end;

      tkOperation :
        begin
          Operation := FCurrentToken.Operation;
          NextToken;

          case Operation of
            opPlus:   begin CheckEndOfStatement; result := ExecUnary(opPlusUno, Factor); end;
            opMinus:  begin CheckEndOfStatement; result := ExecUnary(opMinusUno, Factor); end;
            opNot:    begin CheckEndOfStatement; result := ExecUnary(opNot, Factor); end;
          end;
       end;

      tkLeftBracket :
        case FCurrentToken.Bracket of

          btSimple, btFigure :
            begin
              NextToken;
              CheckEndOfStatement;
              result := Expr(Pred(LastOperationPriority));
              CheckEndOfStatement;
              if FCurrentToken.Token = tkRightBracket then
                NextToken
              else
                raise EDeParserError.Create(Error_BracketExpected, Script, FCurrentToken.TokenPos);
            end;

          btSquare :
            begin
              NextToken;
              CheckEndOfStatement;

              if FCurrentToken.Token = tkRightBracket then
                  begin
                    NextToken;
                    result := ExecConst(Unassigned, ftUnknown);
                  end
                else
                  begin
                    Result := Expr(Pred(LastOperationPriority));
                    while (FCurrentToken.Token = tkComma) do
                      begin
                        NextToken;
                        CheckEndOfStatement;
                        Result := ExecBinary(opArray, Result, Expr(Pred(LastOperationPriority)));
                      end;

                    if (FCurrentToken.Token = tkRightBracket) then
                      NextToken
                    else
                      raise EDeParserError.Create(Error_BracketExpected, Script, FCurrentToken.TokenPos);
                  end;
            end;
        end;

      tkIdent :
        begin
          IdentName:= FCurrentToken.Ident;
          nTokenIndex:= FTokenIndex;
          LastError:=0;

          // смотрим наличие скобки после Идента, если есть - это функция или команда, читаем массив параметров
          HaveBracket:= Assigned(FNextToken) and (FNextToken.Token = tkLeftBracket);

          // объявление переменной
          if Assigned(FNextToken) and (FNextToken.Token = tkOperation) and (FNextToken.Operation = opAssign) then
              begin
                result := ExecLocal(IdentName, True);
                NextToken;
              end;

          // чтение переменной
          if not Assigned(result) and not HaveBracket and assigned(FInnerVar.FindVariable(IdentName)) then
             begin
               result:=ExecLocal(IdentName, False);
               NextToken;
             end;

          //ищем идентификатор (имя поля, контрола)
          if not Assigned(result) and not HaveBracket then
            begin
              result := ExecIdent(IdentName);
              if Assigned(result) then NextToken;
            end;

          // ищем переменную
          if not Assigned(result) and not HaveBracket then
            begin
              result := ExecVariable(IdentName);
              if Assigned(result) then NextToken;
            end;

          if not Assigned(result) then
            begin
              //формируем массив параметров для функции или команды
              if HaveBracket then
                begin
                  NextToken;
                  CheckEndOfStatement;
                  FunctionArgs:= ReadFunctionArgs;
                end
              else
                begin
                  NextToken;
                  SetLength(FunctionArgs, 0);
                end;

              nComm:= ProfitActions.IndexByOriginal(IdentName);
              if -1 < nComm then
                // команда
                begin
                  result:= ExecCommand(IdentName);
                end
              else
                // функция
                begin
                  // для поддержки старого синтаксиса isnull(x) >> x is null
                  if SameText('isnull', IdentName) and (length(FunctionArgs) = 1) then
                    begin
                      result:= TPostfixItem.Create;
                      result.SetData(null, ftUnknown);
                      FExpression.Add(result);

                      result:= TPostfixItem.Create;
                      result.BinaryOp:= opIs;
                      result.ResultType:= ftBoolean;
                      FExpression.Add(result);
                    end
                  else
                    for i := 0 to Pred(FunctionList.Count) do
                      if SameText(FunctionList.Items[i].Name, IdentName) then
                      try
                        Result := ExecFunction(FunctionList.Items[i], FunctionArgs);
                        LastError:= 0;
                        Break;
                      except
                        on E: EDeParserError do
                           if E.Error in [Error_InvalidArgNumber, Error_InvalidArgType]
                             then LastError:= E.Error;
                             else raise EDeParserError.Create(LastError, IdentName)
                      end;
                end;

              // ничего не нашли восстанавливаем указатель на текущий Ident
              if not Assigned(result) then
                TokenIndex:= nTokenIndex;
            end;

          // ничего не нашли
          if not Assigned(result) then
            begin
              if 0<LastError then
                raise EDeParserError.Create(LastError, IdentName) else

              if HaveBracket then raise EDeParserError.Create(Error_BracketExpected, IdentName)
                             else raise EDeParserError.Create(Error_UnknownIdent, IdentName);
              NextToken;
            end;
        end;
    end;
  except
    on E: EDeParserError do
    begin
      if Assigned(FCurrentToken) then
        ErrorPos := FCurrentToken.TokenPos
      else
        ErrorPos := Length(Script);
      raise EDeParserError.Create(E.Error, Script, ErrorPos, E.ErrorData);
    end;
  end;
end;

function TDeSimpleParser.GetScript: string;
begin
  result := FLexicalAnalyzer.Script;
end;

function TDeSimpleParser.NextToken: TTokenInfo;
begin
  if FEndOfScript then
    raise EDeParserError.Create(Error_EndOfScript);
  if FTokenIndex < FLexicalAnalyzer.TokenCount then
    FCurrentToken := FLexicalAnalyzer.Tokens[FTokenIndex]
  else
    FCurrentToken := nil;
  inc(FTokenIndex);
  if FTokenIndex < FLexicalAnalyzer.TokenCount then
    FNextToken := FLexicalAnalyzer.Tokens[FTokenIndex]
  else
    FNextToken := nil;
  FEndOfScript := FTokenIndex > FLexicalAnalyzer.TokenCount;
  FEndOfStatement := FEndOfScript or (not Assigned(FCurrentToken)) or (FCurrentToken.Token = tkSemicolon);
  result := FCurrentToken;
end;

function TDeSimpleParser.ExecUnary(aUnaryOp: TOperationType; aRight: TPostfixItem): TPostfixItem;
begin
  Assert(aUnaryOp <> opNone, 'Unknown operation');
  Assert(aRight <> nil, 'Operand is not assigned');
  Result := nil;
  case aUnaryOp of
    opPlusUno, opMinusUno:
      if aRight.ResultType in NumericTypes + [ftUnknown] then
        result:= FExpression.AddOperation(aUnaryOp, aRight.ResultType);
    opNot:
      if aRight.ResultType in IntegerTypes + [ftBoolean, ftUnknown] then
        result:= FExpression.AddOperation(aUnaryOp, aRight.ResultType);
  end;

  if not Assigned(result) then
    raise EDeParserError.Create(Error_InvalidOperandType);
end;

function TDeSimpleParser.ExecBinary(aBinaryOp: TOperationType; aLeft, aRight: TPostfixItem): TPostfixItem;
var ErrorString: String;
begin
{$HINTS OFF}

  Result := nil;
  ErrorString:= EmptyStr;

  if aBinaryOp = opNone then ErrorString:= 'Unknown operation' else
  if aLeft = nil  then       ErrorString:= 'Left operand is not assigned' else
  if aRight = nil then       ErrorString:= 'Right operand is not assigned' else

    case aBinaryOp of
      opEQ, opNE, opLT, opLE, opGT, opGE: // допускается сравнение с пусто, он всегда False
        if TypesCompatible(aLeft.ResultType, aRight.ResultType, tcLight) or (aLeft.ResultType = ftUnknown) or (aRight.ResultType = ftUnknown)
          then result:= FExpression.AddOperation(aBinaryOp, ftBoolean)
          else ErrorString:= Format('несовместимые типы %s и %s', [FieldTypeNames[aLeft.ResultType], FieldTypeNames[aRight.ResultType]]);
      opIs, opIsNot:
        if (aRight.ResultType = ftUnknown) and (aRight.ItemType=piConst) and VarIsNull(aRight.Data) then
          result:= FExpression.AddOperation(aBinaryOp, ftBoolean);
      opIn, opNotIN:
        if TypesCompatible(aLeft.ResultType, aRight.ResultType, tcLight) or (aLeft.ResultType = ftUnknown) or (aRight.ResultType in [ftArray, ftUnknown]) then
          result:= FExpression.AddOperation(aBinaryOp, ftBoolean);
      opLike, opNotLike:
        if (aLeft.ResultType in StringTypes+[ftUnknown]) and (aRight.ResultType in StringTypes+[ftUnknown]) then
          result:= FExpression.AddOperation(aBinaryOp, ftBoolean);
      opArray:
          result:= FExpression.AddOperation(aBinaryOp, ftArray);
      opPlus:
        if (aLeft.ResultType = ftUnknown) or (aRight.ResultType = ftUnknown) then result:= FExpression.AddOperation(aBinaryOp, ftUnknown) else
        if (aLeft.ResultType in IntegerTypes) and (aRight.ResultType in IntegerTypes) then result:= FExpression.AddOperation(aBinaryOp, ftInteger) else
        if (aLeft.ResultType in NumericTypes) and (aRight.ResultType in NumericTypes) then result:= FExpression.AddOperation(aBinaryOp, ftFloat) else
        if (aLeft.ResultType in StringTypes) and (aRight.ResultType in StringTypes) then result:= FExpression.AddOperation(aBinaryOp, ftString) else
        if ((aLeft.ResultType in DateTimeTypes) and (aRight.ResultType in NumericTypes)) or
           ((aLeft.ResultType in NumericTypes) and (aRight.ResultType in DateTimeTypes)) then result:= FExpression.AddOperation(aBinaryOp, ftDateTime);
      opMinus:
        if (aLeft.ResultType = ftUnknown) or (aRight.ResultType = ftUnknown) then result:= FExpression.AddOperation(aBinaryOp, ftUnknown) else
        if (aLeft.ResultType in IntegerTypes) and (aRight.ResultType in IntegerTypes) then result:= FExpression.AddOperation(aBinaryOp, ftInteger) else
        if (aLeft.ResultType in NumericTypes) and (aRight.ResultType in NumericTypes) then result:= FExpression.AddOperation(aBinaryOp, ftFloat) else
        if (aLeft.ResultType in DateTimeTypes) and (aRight.ResultType in NumericTypes) then result:= FExpression.AddOperation(aBinaryOp, ftDateTime) else
        if (aLeft.ResultType in DateTimeTypes) and (aRight.ResultType in DateTimeTypes) then result:= FExpression.AddOperation(aBinaryOp, ftFloat);
      opMul:
        if (aLeft.ResultType = ftUnknown) or (aRight.ResultType = ftUnknown) then result:= FExpression.AddOperation(aBinaryOp, ftUnknown) else
        if (aLeft.ResultType in IntegerTypes) and (aRight.ResultType in IntegerTypes) then result:= FExpression.AddOperation(aBinaryOp, ftInteger) else
        if (aLeft.ResultType in NumericTypes) and (aRight.ResultType in NumericTypes) then result:= FExpression.AddOperation(aBinaryOp, ftFloat) else
        if ((aLeft.ResultType = ftTime) and (aRight.ResultType in NumericTypes)) or
           ((aLeft.ResultType in NumericTypes) and (aRight.ResultType = ftTime)) then result:= FExpression.AddOperation(aBinaryOp, ftTime);
      opDiv:
        if (aLeft.ResultType in NumericTypes+[ftUnknown]) and (aRight.ResultType in NumericTypes+[ftUnknown]) then result:= FExpression.AddOperation(aBinaryOp, ftFloat);
      opIntDiv, opMod:
        if (aLeft.ResultType in IntegerTypes+[ftUnknown]) and (aRight.ResultType in IntegerTypes+[ftUnknown]) then result:= FExpression.AddOperation(aBinaryOp, ftInteger);
      opAnd, opOr, opXor:
        if (aLeft.ResultType = ftUnknown) and (aRight.ResultType = ftUnknown) then result:= FExpression.AddOperation(aBinaryOp, ftUnknown) else
        if (aLeft.ResultType in IntegerTypes+[ftUnknown]) and (aRight.ResultType in IntegerTypes+[ftUnknown]) then result:= FExpression.AddOperation(aBinaryOp, ftInteger) else
        if (aLeft.ResultType in [ftBoolean,ftUnknown]) and (aRight.ResultType in [ftBoolean,ftUnknown]) then result:= FExpression.AddOperation(aBinaryOp, ftBoolean);
      opAssign:
        result:= FExpression.AddOperation(aBinaryOp, aRight.ResultType);
      opNext:
        result:= FExpression.AddOperation(aBinaryOp, aRight.ResultType);
    end;

  if not Assigned(result) then
    raise EDeParserError.Create(Error_InvalidOperandType, ErrorString);
{$HINTS ON}
end;

function TDeSimpleParser.ExecCommand(const aCommand: string): TPostfixItem;
begin
  Result := TPostfixItem.Create;
  Result.Command := aCommand;
  FExpression.Add(Result);
end;

function TDeSimpleParser.ExecConst(const aConst: Variant; const aType: TFieldType) : TPostfixItem;
begin
  result := TPostfixItem.Create;
  result.SetData(aConst, aType);
  FExpression.Add(result);
end;

procedure TDeSimpleParser.Parse(const aScript : string; aData : TExpressionItem);
begin
  FExpression := aData;
  SetScript(aScript);
end;

procedure TDeSimpleParser.ParsScript;
var ErrorPos: Integer;
begin
  FLexicalAnalyzer.ParsScript;
  NextToken;
  while not FEndOfScript do
  begin

    try
      Expr(LastOperationPriority);
    except
      on E: EDeParserError do
      begin
        if Assigned(FCurrentToken) then
          ErrorPos := FCurrentToken.TokenPos
        else
          ErrorPos := Length(Script);
        raise EDeParserError.Create(E.Error, Script, ErrorPos, E.ErrorData);
      end;
    end;

    break;  // временно - ограничиваемся вычислением выражений
  end;
end;

function TDeSimpleParser.ReadFunctionArgs: TArrayArguments;
var ArgIndex, ErrorPos: integer;
    WaitBracket : TBracketType;
begin
  WaitBracket := btNone;
  if (not FEndOfScript) then
    if (FCurrentToken.Token = tkLeftBracket) then
      begin WaitBracket := FCurrentToken.Bracket; NextToken; end;

  Result := nil;
  ArgIndex := 0;
  while (not FEndOfScript) and (not FEndOfStatement) and
        (not((FCurrentToken.Token = tkRightBracket) and (FCurrentToken.Bracket = WaitBracket))) do
  begin
    SetLength(Result, Length(Result)+1);

    Result[ArgIndex] := Expr(Pred(LastOperationPriority));

    CheckEndOfStatement;
    if FCurrentToken.Token = tkComma then
      begin
        inc(ArgIndex);
        NextToken;
        Continue;
      end;

    if (FCurrentToken.Token = tkRightBracket) and (WaitBracket = FCurrentToken.Bracket) then
      begin
        NextToken;
        Exit;
      end;

    if Assigned(FCurrentToken) then
      ErrorPos := FCurrentToken.TokenPos
    else
      ErrorPos := Length(Script);
    raise EDeParserError.Create(Error_BracketExpected, Script, ErrorPos);
  end;
end;

procedure TDeSimpleParser.SetScript(const Value: string);
begin
  Clear;
  FLexicalAnalyzer.Script := Value;
  ParsScript;
end;

procedure TDeSimpleParser.SetTokenIndex(aIndex: Integer);
begin
  if FTokenIndex <> aIndex then
    begin
      FTokenIndex:= aIndex;

      // curent
      if (-1<FTokenIndex) and (Pred(FTokenIndex) < FLexicalAnalyzer.TokenCount) then
        FCurrentToken := FLexicalAnalyzer.Tokens[Pred(FTokenIndex)]
      else
        FCurrentToken := nil;

      //next
      if (FTokenIndex < FLexicalAnalyzer.TokenCount) then
        FNextToken := FLexicalAnalyzer.Tokens[FTokenIndex]
      else
        FNextToken := nil;

      FEndOfScript := FTokenIndex > FLexicalAnalyzer.TokenCount;
      FEndOfStatement := FEndOfScript or (not Assigned(FCurrentToken)) or (FCurrentToken.Token = tkSemicolon);
    end;
end;

{ TDeParser }

function TDeParser.ExecIdent(const aIdent : string) : TPostfixItem;
var FM: TFieldMeta;
    FT: TFieldType;
begin
  Assert(aIdent <> EmptyStr, 'Ident name is empty');

  result:= nil;

  if assigned(FonGetIdentType) then
    if FonGetIdentType(aIdent, FT, FObjectPointer) then
      begin
        result := TPostfixItem.Create;
        result.Ident := aIdent;
        result.ResultType := FT;
        FExpression.Add(result);
      end;

  if Assigned(FTable) and not assigned(result) then
    begin
      FM := FTable.GetFieldByName(aIdent,True);
      if Assigned(FM) then
        begin
          result := TPostfixItem.Create;
          result.Ident := aIdent;
          result.ResultType := FM.DataType;
          if FM.DataType = ftWideString
            then result.CodePage := 0
            else result.CodePage := FM.CodePage;
          FExpression.Add(result);
        end;
    end;
end;

function TDeParser.ExecVariable(const aVariable : string) : TPostfixItem;
var VarItem : TVariableItem;
    P: Integer;
begin
  Assert(aVariable <> EmptyStr, 'Ident name is empty');
  result:= nil;

  P:= pos('.',aVariable);
  if 0 < P then
    begin
{      VarItem := FExpression.Variables.FindVariable(Copy(aVariable, 1, P-1));
      if Assigned(VarItem) then
        if 0 < VarItem.DataSetID then
          begin
            Result := TPostfixItem.Create;
            Result.Variable := VarItem;
            Result.ResultType := VarItem.DataType;
            FExpression.Add(Result);
          end; {}
    end
  else
    begin
      VarItem := FExpression.Variables.FindVariable(aVariable);
      if Assigned(VarItem) then
        begin
          Result := TPostfixItem.Create;
          Result.Variable := VarItem;
          Result.ResultType := VarItem.DataType;
          FExpression.Add(Result);
        end;
    end;

  if (result = nil) and Assigned(FonGetVariable) then
    if FOnGetVariable(aVariable, VarItem, FObjectPointer) then
      begin
        FExpression.Variables.Add(VarItem);
        Result := TPostfixItem.Create;
        Result.Variable := VarItem;
        Result.Name:= VarItem.Name;
        Result.ResultType := VarItem.DataType;
        FExpression.Add(Result);
      end;

  if (result = nil) then
    result := inherited ExecVariable(aVariable);
end;

{ TGlobalVariables }

constructor TGlobalVariables.Create;
begin
  inherited Create;

  Items[Add(TVariableItem.Create(ftInteger, UserID, [amRead]))].OnGetValue := GetUserID;
  Items[Add(TVariableItem.Create(ftString, UserLogin, [amRead]))].OnGetValue := GetUserLogin;
  Items[Add(TVariableItem.Create(ftString, UserName, [amRead]))].OnGetValue := GetUserName;
  Items[Add(TVariableItem.Create(ftBoolean, UserAdmin, [amRead]))].OnGetValue := GetUserIsAdmin;
  Items[Add(TVariableItem.Create(ftInteger, UserWorker, [amRead]))].OnGetValue := GetUserWorker;

  // v. 19.02
  Items[Add(TVariableItem.Create(ftInteger, cMetadataID, [amRead]))].OnGetValue := GetMetadataID;
  Items[Add(TVariableItem.Create(ftInteger, cMetadataType, [amRead]))].OnGetValue := GetMetadataType;
  Items[Add(TVariableItem.Create(ftString, cMetadataAlias, [amRead]))].OnGetValue := GetMetadataServer;
  Items[Add(TVariableItem.Create(ftString, cMetadataServer, [amRead]))].OnGetValue := GetMetadataServer;
  Items[Add(TVariableItem.Create(ftString, cMetadataDatabase, [amRead]))].OnGetValue := GetMetadataDatabase;
  Items[Add(TVariableItem.Create(ftString, cMetadataLogin, [amRead]))].OnGetValue := GetMetadataLogin;
  Items[Add(TVariableItem.Create(ftString, cMetadataPassword, [amRead]))].OnGetValue := GetMetadataPassword;
end;

function TGlobalVariables.GetUserID(aVariable : TVariableItem) : Variant;
begin
  result := UserSession.ID;
end;

function TGlobalVariables.GetUserLogin(aVariable : TVariableItem) : Variant;
begin
  result := UserSession.Login;
end;

function TGlobalVariables.GetUserName(aVariable : TVariableItem) : Variant;
begin
  result := UserSession.Name;
end;

function TGlobalVariables.GetUserIsAdmin(aVariable : TVariableItem) : Variant;
begin
  result := UserSession.IsAdmin;
end;

function TGlobalVariables.GetUserWorker(aVariable : TVariableItem) : Variant;
begin
  result := UserSession.WorkerID;
end;

function TGlobalVariables.GetWorkerInfo(aVariable : TVariableItem): Variant;
var CacheItem : TCacheItem;
begin
  CacheItem := UserSession.WorkerItem;
  if assigned(CacheItem) then
    CacheItem.FieldValueRecurce( Copy(aVariable.Name, Length(UserWorkerRec)+1, MaxInt), Result)
  else
    Result:= null;
end;

function TGlobalVariables.GetMetadataID(aVariable: TVariableItem): Variant;
begin
  Result := MetaData.MetadataDB.ID;
end;

function TGlobalVariables.GetMetadataType(aVariable: TVariableItem): Variant;
begin
  Result := Ord(MetaData.MetadataDB.DatabaseType);
end;

function TGlobalVariables.GetMetadataAlias(aVariable: TVariableItem): Variant;
begin
  Result := MetaData.MetadataDB.Alias;
end;

function TGlobalVariables.GetMetadataServer(aVariable: TVariableItem): Variant;
begin
  Result := MetaData.MetadataDB.Server;
end;

function TGlobalVariables.GetMetadataDatabase(aVariable: TVariableItem): Variant;
begin
  Result := MetaData.MetadataDB.Database;
end;

function TGlobalVariables.GetMetadataLogin(aVariable: TVariableItem): Variant;
begin
  Result := MetaData.MetadataDB.Login;
end;

function TGlobalVariables.GetMetadataPassword(aVariable: TVariableItem): Variant;
begin
  Result := MetaData.MetadataDB.Password;
end;

function TGlobalVariables.GetFolderOut(aVariable : TVariableItem): Variant;
begin
  Result:= NormalizeFilePath(Variables.AsString[RegDirPath]);
end;

function TGlobalVariables.GetFolderTemp(aVariable : TVariableItem): Variant;
begin
  Result:= NormalizeFilePath(DeTempDir);
end;

function TGlobalVariables.GetFolderMap(aVariable : TVariableItem): Variant;
begin
  Result:= NormalizeFilePath(Variables.AsString[RegMapBaseDirectory]);
end;

function TGlobalVariables.GetFolderLog(aVariable : TVariableItem): Variant;
begin
  Result:= NormalizeFilePath(LogDirectory);
end;

function TGlobalVariables.FindVariable(const aName : string) : TVariableItem;
var TM: TTableMeta;
    FM: TFieldMeta;
begin
  Result:= Inherited;
  
  if Result = nil then
    begin
      TM := MetaData.GetTableMeta(MetaData.ParamValue['WorkerTable']);
      if Assigned(TM) and SameText(UserWorkerRec, Copy(aName, 1, Length(UserWorkerRec))) then
        begin
          FM:= TM.GetFieldByName(Copy(aName, Length(UserWorkerRec)+1, MaxInt), True);
          if assigned(FM) then
            begin
              result:=Items[Add(TVariableItem.Create(FM.DataType, aName, [amRead]))];
              TVariableItem(result).OnGetValue := GetWorkerInfo;
            end;
        end;
    end;

  if (Result = nil) and SameText(cFolderOut, aName) then
    begin
      result:=Items[Add(TVariableItem.Create(ftString, aName, [amRead]))];
      TVariableItem(result).OnGetValue := GetFolderOut;
    end else

  if (Result = nil) and SameText(cFolderTemp, aName) then
    begin
      result:=Items[Add(TVariableItem.Create(ftString, aName, [amRead]))];
      TVariableItem(result).OnGetValue := GetFolderTemp;
    end else

  if (Result = nil) and SameText(cFolderMap, aName) then
    begin
      result:=Items[Add(TVariableItem.Create(ftString, aName, [amRead]))];
      TVariableItem(result).OnGetValue := GetFolderMap;
    end else

  if (Result = nil) and SameText(cFolderLog, aName) then
    begin
      result:=Items[Add(TVariableItem.Create(ftString, aName, [amRead]))];
      TVariableItem(result).OnGetValue := GetFolderLog;
    end;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('DeScript unit initialization ...');
  {$ENDIF}
  GlobalVariables := TGlobalVariables.Create;
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DeScript unit finalization ...');
  {$ENDIF}
  FreeAndNil(GlobalVariables);
end;
{$ENDREGION}

initialization
  Startup;
finalization
  Shutdown;

end.
