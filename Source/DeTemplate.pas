unit DeTemplate;

interface

uses Contnrs, Variants, SysUtils, Classes, DeTypes, DeVariable, DeParser;

const
    ProfitSetPrefix = 'PROFITSETPREFIX';
    Code_Null     = -1;

    Code_Unknown  =  0;
    Code_Note     =  1;
    Code_Header   =  2;
    Code_Param    =  3;
    Code_Body     =  4;
    Code_Block    =  5;

    Code_DataSet  =  6;
    Code_Make     =  7;

    Code_For      =  8;
    Code_Var      =  9;
    Code_Message  = 10;
    Code_Count    = 11;

    Code_Min      = 12;
    Code_Max      = 13;
    Code_Sum      = 14;
    Code_Avg      = 15;

    Code_While    = 16; // v. 17.1
    Code_Case     = 17; // v. 17.1

    Code_Break    = 18; // v. 18.3
    Code_Continue = 19; // v. 18.3

    Code_File    = 20; // v. 18.3

    CodeWeight : array[-1..20] of Integer = (0, 1,1,1,1,1,1, 5,3, 1,1,1,1, 2,2,2,2, 3, 1, 1, 1, 2);

    Agregates = [ Code_Count, Code_Min,  Code_Max, Code_Sum, Code_Avg ];
type
    // типы тегов
    TDeTagType    = (ttTEXT,     // просто копируемый текст,
                     ttFILE,     // корневой тег - весь файл
                     ttSINGLE,   // одинарный тег  <TMPL_VAR abc/>
                     ttDOUBLE,   // парный тег     <TMPL_IF abc=0>..</TMPL_IF>
                     ttPROFIT);  // специальный тег смены префикса
    // состояния парсинга параметров тега
    TLogicPosition = (lp_ERROR, lp_GO_NAME, lp_IN_NAME, lp_GO_EQ, lp_GO_VALUE, lp_IN_VALUE );

    TDeNode = class;
    TDeNodes = class;
    TDeTemplate = class;

    // узел дерева
    TDeNode = class(TObject)
    private
      FDeTemplate : TDeTemplate;     // шаблон
      FParent     : TDeNode;         // родительский узел
      FItems      : TDeNodes;        // дочерние узлы
      FAttributes : TDeVariableList; // аттрибуты узла
      FConditionItem   : TExpressionItem;
      FCalculationItem : TExpressionItem;

      FName    : string;         // имя тега
      FTagType : TDeTagType;     // тип тега,           Internal property only
      FCode    : Integer;        // код прекомпиляции,  Extermal property only
      FWeight  : Integer;        // вес

      FTextA: Integer;           // первый символ блока
      FTextB: Integer;           // последний символ блока
      FParsA: Integer;           // первый символ параметра открывающего тега
      FParsB: Integer;           // последний символ параметра открывающего тега
      FBodyA: Integer;           // первый символ текста между открывающми и закрывающим тегами
      FBodyB: Integer;           // последний символ текста между открывающми и закрывающим тегами
      FLevel: Integer;           // уровень данного тега в дереве

      function GetText: string;  // получение всего блока
      function GetPars: string;  // получение параметра открывающего тега
      function GetBody: string;  // получение текста между открывающми и закрывающим тегами
      function GetItemsCount: Integer;
      function GetAttribute(const Index: string): string;
      function GetInnerXML: string;
      function GetOuterXML: string;
      function GetLevel: Integer;
    public
      // перекрываемые конструкторы с разными параметрами создания экземпляра
      constructor Create; overload;
      constructor Create(aTemplate: TDeTemplate); overload;
      constructor Create(aOwner: TDeNode); overload;
      constructor Create(aOwner: TDeNode; const aFirst, aLast: Integer); overload; // создание тега Plain текста
      // общий деструктор
      destructor Destroy; override;

      property DeTemplate: TDeTemplate read FDeTemplate;
      property ChildNodes: TDeNodes read FItems;
      property Count: Integer read GetItemsCount;

      property NodeName: string read FName;
      property TagType: TDeTagType read FTagType;
      function Code: Integer;
      function HasAttribute(const Index: string): Boolean;
      function FindNode(const Index: string): TDeNode;
      function FindNodeByCode(const Index: Integer): TDeNode;
      function Weight: Integer;

      property Text: String read GetText;
      property TextA: Integer read FTextA;
      property TextB: Integer read FTextB;

      property Pars: String read GetPars;
      property ParsA: Integer read FParsA;
      property ParsB: Integer read FParsB;

      property Body: string read GetBody;
      property BodyA: Integer read FBodyA;
      property BodyB: Integer read FBodyB;

      property Attributes[const Index: string]: string read GetAttribute;
      property Condition : TExpressionItem read FConditionItem write FConditionItem;
      property Calculation : TExpressionItem read FCalculationItem write FCalculationItem;

      property NodeAttributes: TDeVariableList read FAttributes;

      // v. 18.3
      function FindObject(const Path: string): TObject;

      // v. 18.4
      /// <summary>Все вложенные теги в формате XML</summary>
      property InnerXML: string read GetInnerXML;
      /// <summary>Сам тег и все вложенные теги в формате XML</summary>
      property OuterXML: string read GetOuterXML;
      /// <summary>Уровень данного тега в дереве</summary>
      property Level: Integer read GetLevel;
      {$IFDEF DEBUG}
      procedure DebugNodesLog(const Text: string);
      {$ENDIF}
    end;

    // список узлов
    TDeNodes = class(TObjectList)
    private
      function GetItem(const Index: Integer): TDeNode;
      procedure PutItem(const Index: Integer; const Value: TDeNode);
    public
      property Items[const Index: Integer]: TDeNode read GetItem write PutItem; default;
    end;

    // шаблон
    TDeTemplate = class(TObject)
    private
      FPrefix: String;             // обязательный префикс каждого "нашего" тега
      FText: String;               // текст шаблона
      FRoot: TDeNode;              // корневой узел после разбора
      FEStrings : TStringList;     // список ошибок

      // получение имени тега начиная с указанного символа
      function GetNameAtPos(const aA, aB: Integer): string;
      // добавление ошибки в список ошибок
      procedure AddErrorLine(const aChar: Integer; const aString: string);

      // разбор параметров тега
      procedure ParseParams(aNode: TDeNode);
      // основная процерура разбора текста
      function Parse: Boolean;
      // внешнее назначение текста
      procedure SetText(const aString: string);
    public
      constructor Create(const aText: String = ''; const aPrefix: String = '');
      destructor Destroy; override;

      property Text: String read FText write SetText;
      property Root: TDeNode read FRoot;

      // получение номера строки по номеру символа
      function GetLineByChar(const aChar: Integer): Integer;

      function ErrorCount : Integer;
      property ErrorStrings : TStringList read FEStrings;
      property Prefix: String read FPrefix write FPrefix;
    end;

implementation

uses StrUtils, Funcs, DeXMLReport;

{ TDeNode ==================================================================== }

constructor TDeNode.Create;
begin
  inherited create;

  FParsA  :=  0;
  FParsB  := -1;
  FBodyA  :=  0;
  FBodyB  := -1;
  FTextA  :=  0;
  FTextB  := -1;
  FLevel  := -1;
  FName   := EmptyStr;

  FCode   := Code_Null;
  FWeight := -1;
  FTagType:= ttTEXT;

  FParent := nil;
  FDeTemplate := nil;

  FItems := TDeNodes.Create;
  FAttributes := TDeVariableList.Create;
  FConditionItem := nil;
  FCalculationItem := nil;
end;

constructor TDeNode.Create(aTemplate: TDeTemplate);
begin
  Create;
  FDeTemplate := aTemplate;
end;

constructor TDeNode.Create(aOwner: TDeNode);
begin
  Create;
  FParent := aOwner;
  FDeTemplate := FParent.FDeTemplate;
  FParent.FItems.Add(Self);
end;

constructor TDeNode.Create(aOwner: TDeNode; const aFirst, aLast: Integer);
begin
  Create(aOwner);
  FName   := EmptyStr;
  FTagType:= ttTEXT;
  FTextA  := aFirst;   FTextB  := aLast;
  FParsA  := aFirst;   FParsB  := aLast;
  FBodyA  := aFirst;   FBodyB  := aLast;
end;

destructor TDeNode.Destroy;
begin
  FAttributes.Free;
  FItems.Free;
  if Assigned(FConditionItem) then FConditionItem.Free;
  if Assigned(FCalculationItem) then FCalculationItem.Free;
  inherited Destroy;
end;

function TDeNode.Code: Integer;
var s: string;
begin
  if FCode = Code_Null then
    begin
      s := Copy(FName, Succ(Length(FDeTemplate.Prefix)), MaxInt);

      if SameText(s, slrHEADER_TAG) then  FCode := Code_Header else
      if SameText(s, slrPARAMS_TAG) then  FCode := Code_Header else
      if SameText(s, slrPARAM_TAG) then   FCode := Code_Param else
      if SameText(s, slrBODY_TAG) then    FCode := Code_Body else
      if SameText(s, slrNOTE_TAG) then    FCode := Code_Note else
      if SameText(s, slrBLOCK_TAG) then   FCode := Code_Block  else
      if SameText(s, slrDATASET_TAG) then FCode := Code_DataSet else
      if SameText(s, slrMAKE_TAG) then    FCode := Code_Make else
      if SameText(s, slrFOR_TAG) then     FCode := Code_For else
      if SameText(s, slrWHILE_TAG) then   FCode := Code_While else
      if SameText(s, slrCASE_TAG) then    FCode := Code_Case else
      if SameText(s, slrMESSAGE_TAG) then FCode := Code_Message else
      if SameText(s, slrCOUNT_TAG) then   FCode := Code_Count else
      if SameText(s, slrDSMIN_TAG) then   FCode := Code_Min else
      if SameText(s, slrDSMAX_TAG) then   FCode := Code_Max else
      if SameText(s, slrDSSUM_TAG) then   FCode := Code_Sum else
      if SameText(s, slrDSAVG_TAG) then   FCode := Code_Avg else
      if SameText(s, slrVAR_TAG) then     FCode := Code_Var else
      if SameText(s, slrBREAK_TAG) then   FCode := Code_Break else
      if SameText(s, slrCONTINUE_TAG) then FCode := Code_Continue else
      if SameText(s, slrFILE_TAG) then    FCode := Code_File else
                                          FCode := Code_Unknown;
    end;

  Result := FCode;
end;

function TDeNode.Weight: Integer;
var
  Index: Integer;
begin
  if FWeight < 0 then
    begin
      FWeight := 0;
      for Index := 0 to Pred(Count) do
        Inc(FWeight, ChildNodes[Index].Weight);

      if FCode = Code_For then
        FWeight := 3 * FWeight + CodeWeight[Code]
      else
        FWeight := FWeight + CodeWeight[Code];
    end;
  Result := FWeight;
end;

function TDeNode.GetText: string;
begin
  if FTextB = -1 then
    Result := Copy(FDeTemplate.FText, FTextA, FBodyA - FTextA )   // незавершенный тег или <text>
  else
    Result := Copy(FDeTemplate.FText, FTextA, Succ(FTextB - FTextA));
end;

function TDeNode.GetPars: string;
begin
  if FTagType in [ttFILE, ttTEXT] then
    Result := EmptyStr
  else
    Result := Copy(FDeTemplate.FText, FParsA, Succ(FParsB - FParsA));
end;

function TDeNode.GetBody: string;
begin
  if FBodyB = -1 then
    Result := EmptyStr
  else
    Result := Copy(FDeTemplate.FText, FBodyA, Succ(FBodyB - FBodyA));
end;

Function TDeNode.GetItemsCount: Integer;
begin
  Result := FItems.Count;
end;

function TDeNode.HasAttribute(const Index: string): Boolean;
begin
  Result := Assigned(FAttributes.GetByName(Index, False));
end;

function TDeNode.GetAttribute(const Index: string): string;
var DeVar: TVariableItem;
begin
  DeVar := FAttributes.GetByName(Index, False);
  if Assigned(DeVar) then Result := DeVar.Value
                     else Result := EmptyStr;
end;

function TDeNode.FindNode(const Index: string): TDeNode;
var i: Integer;
begin
  if SameText(FName, Index) then
    Result := Self
  else
    begin
      Result := nil;
      for i := 0 to Pred(FItems.Count) do
        begin
          Result := FItems[i].FindNode(Index);
          if Assigned(Result) then Break;
        end;
    end;
end;

function TDeNode.FindNodeByCode(const Index: Integer): TDeNode;
var i: Integer;
begin
  if Code = Index then
    Result := Self
  else
    begin
      Result := nil;
      for i := 0 to Pred(FItems.Count) do
        begin
          Result := FItems[i].FindNodeByCode(Index);
          if Assigned(Result) then Break;
        end;
    end;
end;

function TDeNode.FindObject(const Path: string): TObject;
var
  Value, Name, SubPath: string;
  Index, NodeIndex: Integer;
begin
  SubPath := Path;
  if Length(SubPath) = 0 then
    Result := Self
  else
    begin
      if SubPath[1] = '/' then Delete(SubPath, 1, 1);
      Index := Pos('/', SubPath);
      if Index = 0 then
        begin
          Name := SubPath;
          SubPath := EmptyStr;
        end
      else
        begin
          Name := Copy(SubPath, 1, Pred(Index));
          SubPath := Copy(SubPath, Index, Length(SubPath));
        end;
      if Length(Name) = 0 then
        Result := Self
      else
        if Name[1] = '@' then
          begin
            Delete(Name, 1, 1);
            if Length(Name) = 0 then
              begin
                Result := nil;
                {$IFDEF DeDEBUG}
                WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': empty attribute name not supported');
                {$ENDIF}
                Exit;
              end
            else
              if Length(SubPath) = 0 then
                begin
                  Index := FAttributes.IndexByName(Name);
                  if Index <> -1 then
                    Result := FAttributes[Index]
                  else
                    Result := nil;
                end
              else
                begin
                  Result := nil;
                  {$IFDEF DeDEBUG}
                  WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': sub path in attribute ' + QuotedStr(Name) + ' not supported');
                  {$ENDIF}
                  Exit;
                end;
          end
        else
          begin
            Index := Pos('[', Name);
            if Index = 0 then
              Value := EmptyStr
            else
              begin
                Value := Copy(Name, Succ(Index), Length(Name));
                Name := Copy(Name, 1, Pred(Index));
                Index := Pos(']', Value);
                if Index = 0 then
                  begin
                    Result := nil;
                    {$IFDEF DeDEBUG}
                    WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': range symbol '']'' not found');
                    {$ENDIF}
                    Exit;
                  end
                else
                  if Length(Copy(Value, Succ(Index), Length(Value))) = 0 then
                    begin
                      Value := Copy(Value, 1, Pred(Index));
                      if Length(Trim(Value)) = 0 then
                        begin
                          Result := nil;
                          {$IFDEF DeDEBUG}
                          WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': empty range not supported');
                          {$ENDIF}
                          Exit;
                        end;
                    end
                  else
                    begin
                      Result := nil;
                      {$IFDEF DeDEBUG}
                      WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': unknown after range string');
                      {$ENDIF}
                      Exit;
                    end;
              end;
            // Если ищем узел без индекса, то ...
            if Length(Value) = 0 then
              begin
                if SameText(Name, '.') then
                  if Length(SubPath) = 0 then
                    Result := Self
                  else
                    Result := FindObject(SubPath)
                else
                  if SameText(Name, '..') then
                    if Length(SubPath) = 0 then
                      Result := Self.FParent
                    else
                      if Assigned(FParent) then
                        Result := FParent.FindObject(SubPath)
                      else
                        begin
                          Result := nil;
                          {$IFDEF DeDEBUG}
                          WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': parent node is nil');
                          {$ENDIF}
                        end
                  else
                    begin
                      Result := nil;
                      for Index := 0 to Pred(FItems.Count) do
                        if SameText(FItems[Index].NodeName, Name) then
                          begin
                            if Length(SubPath) = 0 then
                              Result := FItems[Index]
                            else
                              Result := FItems[Index].FindObject(SubPath);
                            Break;
                          end;
                    end;
              end
            else
              if TryStrToInt(Value, NodeIndex) then
                if NodeIndex >= 0 then
                  begin
                    Result := nil;
                    for Index := 0 to Pred(FItems.Count) do
                      if SameText(FItems[Index].NodeName, Name) then
                        if NodeIndex = 0 then
                          begin
                            if Length(SubPath) = 0 then
                              Result := FItems[Index]
                            else
                              Result := FItems[Index].FindObject(SubPath);
                            Break;
                          end
                        else
                          Dec(NodeIndex);
                  end
                else
                  begin
                    Result := nil;
                    {$IFDEF DeDEBUG}
                    WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': range index ' + IntToStr(NodeIndex) + ' not between 0 and ' + IntToStr(MaxInt));
                    {$ENDIF}
                  end
              else
                begin
                  Result := nil;
                  {$IFDEF DeDEBUG}
                  WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': range ' + QuotedStr(Value) + ' not integer value');
                  {$ENDIF}
                end;
          end;
    end;
end;

function TDeNode.GetInnerXML: string;
begin
  if Assigned(FDeTemplate) and (FBodyB <> -1) then
    Result := Copy(FDeTemplate.FText, FBodyA, Succ(FBodyB - FBodyA))
  else
    Result := EmptyStr;
end;

function TDeNode.GetOuterXML: string;
begin
  if Assigned(FDeTemplate) then
    if FTextB <> -1 then
      Result := Copy(FDeTemplate.FText, FTextA, Succ(FTextB - FTextA))
    else
      Result := Copy(FDeTemplate.FText, FTextA, FBodyA - FTextA)   // незавершенный тег или <text>
  else
    Result := EmptyStr;
end;

function TDeNode.GetLevel: Integer;
var
  Node: TDeNode;
begin
  if FLevel = -1 then
    begin
      FLevel := 0;
      Node := FParent;
      while Assigned(Node) do
        begin
          Inc(FLevel);
          Node := Node.FParent;
        end;
    end;
  Result := FLevel;
end;

{$IFDEF DEBUG}
procedure TDeNode.DebugNodesLog(const Text: string);
  function PrepareNodesLog(Node: TDeNode): string;
  const
    TypeNames: array[TDeTagType] of PChar = ('TEXT', 'FILE', 'SINGLE', 'DOUBLE', 'PROFIT');
  var
    Index: Integer;
  begin
    if Assigned(Node) then
      begin
        Result := Format(#13#10'                        │ %9d │ %-8s │ %-32s │ %9d │ %9d │ %9d │ %9d │ %9d │ %9d │',
          [
            Node.Level, StrPas(TypeNames[Node.TagType]),
            NormalizeCaption(Node.NodeName, 32), Node.FTextA, Node.FTextB,
            Node.FParsA, Node.FParsB, Node.FBodyA, Node.FBodyB
          ]);
        for Index := 0 to Pred(Node.Count) do
          Result := Result + PrepareNodesLog(Node.ChildNodes[Index]);
      end
    else
      Result := Format(#13#10'                        │ %9s │ %8s │ %32s │ %9s │ %9s │ %9s │ %9s │ %9s │ %9s │',
        [
          ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '
        ]);
  end;
  function PrepareTableLog(const InnerText: string): string;
  begin
    if Length(InnerText) = 0 then
      Result := EmptyStr
    else
      begin
        Result :=
          Format(#13#10'                        ┌%s┬%s┬%s┬%s┬%s┬%s┬%s┬%s┬%s┐', [
            DupeString('─', 11),
            DupeString('─', 10),
            DupeString('─', 34),
            DupeString('─', 11),
            DupeString('─', 11),
            DupeString('─', 11),
            DupeString('─', 11),
            DupeString('─', 11),
            DupeString('─', 11)
            ]) +
          Format(#13#10'                        │ %-9s │ %-8s │ %-32s │ %-9s │ %-9s │ %-9s │ %-9s │ %-9s │ %-9s │', [
            'Level',
            'Type',
            'Name',
            'TextA',
            'TextB',
            'ParsA',
            'ParsB',
            'BodyA',
            'BodyB'
            ]) +
          Format(#13#10'                        ├%s┼%s┼%s┼%s┼%s┼%s┼%s┼%s┼%s┤', [
            DupeString('─', 11),
            DupeString('─', 10),
            DupeString('─', 34),
            DupeString('─', 11),
            DupeString('─', 11),
            DupeString('─', 11),
            DupeString('─', 11),
            DupeString('─', 11),
            DupeString('─', 11)
            ]) +
          InnerText +
          Format(#13#10'                        └%s┴%s┴%s┴%s┴%s┴%s┴%s┴%s┴%s┘', [
            DupeString('─', 11),
            DupeString('─', 10),
            DupeString('─', 34),
            DupeString('─', 11),
            DupeString('─', 11),
            DupeString('─', 11),
            DupeString('─', 11),
            DupeString('─', 11),
            DupeString('─', 11)
            ]);
      end;
  end;
begin
  WriteLog(Text + PrepareTableLog(PrepareNodesLog(Self)), True, 'Debug');
end;
{$ENDIF}

{ TDeNodes =================================================================== }

function TDeNodes.GetItem(const Index: Integer): TDeNode;
begin
  Result := TDeNode(inherited Items[Index]);
end;

procedure TDeNodes.PutItem(const Index: Integer; const Value: TDeNode);
begin
  inherited Items[Index] := Value;
end;

{ TDeTemplate ================================================================ }

constructor TDeTemplate.Create(const aText: String = ''; const aPrefix: String = '');
begin
  inherited Create;

  FPrefix:= aPrefix;
  FRoot:= TDeNode.Create(Self);
  FEStrings:= nil;
  SetText(aText);
end;

destructor TDeTemplate.Destroy;
begin
  if Assigned(FEStrings) then FEStrings.Free;
  inherited Destroy;
end;

function TDeTemplate.GetNameAtPos(const aA, aB: Integer): string;
var N : Integer;
begin
  N:=0;
  while (aA+N<=aB) and CharInSet(FText[aA+N], ['a'..'z','A'..'Z','0'..'9','_','.','-', '$']) do
    Inc(N);

  Result:=AnsiUpperCase(Copy(FText,aA,N));
end;

function TDeTemplate.GetLineByChar(const aChar: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  if aChar <> 0 then
    begin
      Inc(Result);
      for i := 1 to aChar do
        if FText[i] = Chr(13) then Inc(Result);
    end;
end;

Procedure TDeTemplate.AddErrorLine(const aChar: Integer; const aString: string);
begin
  if FEStrings = nil then
    FEStrings := TStringList.Create;

  FEStrings.Add('Line: '+IntToStr(GetLineByChar(aChar))+'; '+'Description: '+aString);
end;

function TDeTemplate.ErrorCount: Integer;
begin
  if Assigned(FEStrings) then Result := FEStrings.Count
                         else Result := 0;
end;

procedure TDeTemplate.ParseParams(aNode: TDeNode);
var i,aPos : Integer;
    L      : TLogicPosition;
    sName, sValue : WideString;
    q : Char;
begin
  q:='''';
  aPos :=0;
  L:=lp_GO_NAME;

  i:= aNode.FParsA;
  while i <= aNode.FParsB do
    begin
                 case L of
      // ждем начала имени
      lp_GO_NAME  : begin
                      if CharInSet(FText[i], ['"','''']) then begin L:=lp_IN_VALUE; aPos:=i+1; q:=FText[i]; sName:='Name' end else
                      if CharInSet(FText[i], ['A'..'Z','a'..'z', '$']) then begin  L:=lp_IN_NAME; aPos:=i; end else
                      if not CharInSet(FText[i], [#0..#32,',',';']) then begin L:=lp_ERROR; end;
                    end;
      // ждем окончания имени
      lp_IN_NAME  : begin
                      if CharInSet(FText[i], [#0..#32]) then begin L:=lp_GO_EQ; sName:=Copy(FText, aPos, i-aPos) end else
                      if CharInSet(FText[i], ['=']) then begin L:=lp_GO_VALUE; sName:=Copy(FText, aPos, i-aPos) end else
                      if not CharInSet(FText[i], ['A'..'Z','a'..'z','_','-','$','0'..'9']) then begin L:=lp_ERROR end;
                    end;
      // ждем знака "="
      lp_GO_EQ    : begin
                      if FText[i]='=' then begin
                      L:=lp_GO_VALUE
                       end else
                      if not CharInSet(FText[i], [#0..#32]) then begin L:=lp_ERROR end;
                    end;
      // ждем кавычек, т.е. начала значения
      lp_GO_VALUE : begin
                      if CharInSet(FText[i], ['"','''']) then begin L:=lp_IN_VALUE; aPos:=i+1; q:=FText[i]; end else
                      if not CharInSet(FText[i], [#0..#32]) then begin  L:=lp_ERROR end;
                    end;
      // ждем кавычек, т.е. окончания значения
      lp_IN_VALUE : begin
                      if (FText[i] = q) then
                        begin
                          if (i<aNode.FParsB) and ( FText[i]=FText[i+1]) then
                            Inc(i)
                          else
                            begin
                              L:=lp_GO_NAME;
                              sVALUE := Copy(FText, aPos, i-aPos);
                              sVALUE := StringReplace(sVALUE, '>>', '>', [rfReplaceAll]);
                              sVALUE := StringReplace(sVALUE, '<<', '<', [rfReplaceAll]);
                              sVALUE := StringReplace(sVALUE,  q+q, q, [rfReplaceAll]);
                              aNode.FAttributes.GetByName(sName).Value:=sValue;
                            end;
                        end;
                    end;
      lp_ERROR:   begin
                    Break;
                  end;
      end;
      Inc(i);
    end;

  case L of
    lp_IN_NAME  : AddErrorLine(aNode.FTextA, 'Waiting end of attribute name in Tag "'+aNode.FName+'": '+Copy(aNode.Text,1,255));
    lp_GO_EQ    : AddErrorLine(aNode.FTextA, 'Waiting simbol = in Tag "'+aNode.FName+'": '+Copy(aNode.Text,1,255));
    lp_GO_VALUE : AddErrorLine(aNode.FTextA, 'Waiting open quote in Tag "'+aNode.FName+'": '+Copy(aNode.Text,1,255));
    lp_IN_VALUE : AddErrorLine(aNode.FTextA, 'Waiting close quote in Tag "'+aNode.FName+'": '+Copy(aNode.Text,1,255));
    lp_ERROR    : AddErrorLine(aNode.FTextA, 'Error attribute description in Tag "'+aNode.FName+'+": '+Copy(aNode.Text,1,255));
  end;
end;

Function TDeTemplate.Parse: Boolean;
var LL,aP,aE : Integer;
    cNode    : TDeNode;
    aNew     : String;
begin
  // инициализация
  LL:=Length(FText);
  Result:=False;

  FRoot.ChildNodes.Clear;

  FRoot.FDeTemplate:=self;
  FRoot.FTextA  := 1;
  FRoot.FTextB  :=LL;
  FRoot.FParsA  := 1;
  FRoot.FParsB  :=LL;
  FRoot.FBodyA  := 1;
  FRoot.FBodyB  :=LL;
  FRoot.FTagType:=ttFile;

  aP:=1;
  aE:=1; // последний символ предыдущего тега, используется для создания тегов Plain текста
  cNode:=FRoot;

  // крутим пока не достигли конца текста
  while aP<=LL do
  begin

    if FText[aP]='<' then
      begin
        // пропускаем сдвоенный знак меньше "<<"
        if (aP+1<=LL) and (FText[aP+1]='<') then
          begin
            Inc(aP,2);
          end
        else

        // найден признак парного закрывающего тега "</"
        if (aP+1<=LL) and (FText[aP+1]='/') then
          begin
            if aE <= ap-1 then TDeNode.Create( cNode, aE, aP-1 );

            aNew:=GetNameAtPos(aP+2,LL);

            if Not((aP+Length(aNew)+2<=LL) and (FText[aP+Length(aNew)+2]='>')) then
              begin
                AddErrorLine(aP,'Common error for closed tag "'+aNew+'"');
                Exit;
              end;

            // это текущий тег
            if CompareText(aNew, FPrefix+cNode.FName)=0 then
              begin
                // попытка закрыть уже закрытый тег
                if Not (cNode.FTagType=ttDOUBLE) then
                  begin
                    AddErrorLine(aP,'Tag "'+aNew+'" already closed');
                    Exit;
                  end;

                // закрываем, переходим к родительскому тегу
                cNode.FBodyB:=aP-1;
                cNode.FTextB:=aP+Length(aNew)+2;
                cNode:=cNode.FParent;
                Inc(aP,Length(aNew)+3);
                aE:=aP;
              end
            else
              begin
                // попытка закрытия "нашего" тега, который не открывался
                if CompareText(FPrefix,Copy(aNew,1,Length(FPrefix)))=0 then
                  begin
                    AddErrorLine(aP,'Cannot find open tag for closed tag "'+aNew+'"');
                    Exit;
                  end;

                // пропускаем чужой закрывающий тег
                Inc(aP,Length(aNew)+3);
              end;
          end
        else
          begin
            aNew:=GetNameAtPos(aP+1,LL);
            // пропускаем открывающую скобку или знак меньше без тега "<_"
            if Length(aNew)=0 then
                begin
                  inc(aP);
                end
            else

            // найден "наш" открывающий тег, создаем узел
            if (CompareText(FPrefix,Copy(FText,aP+1,Length(FPrefix)))=0) then
              begin
                if aE <= aP-1 then TDeNode.Create( cNode, aE, aP-1 );

                cNode:=TDeNode.Create(cNode);
                cNode.FName   := aNew;
                cNode.FTagType:= ttSINGLE;
                cNode.FTextA  := aP;
                cNode.FParsA  := aP+Length(aNew)+1;
                while (cNode.FParsA<Length(FText))and(FText[cNode.FParsA]<=' ') do
                  cNode.FParsA:=cNode.FParsA+1;

                Inc(aP,Length(aNew)+1);
                aE := aP;
              end

            else
              // пропускаем чужой открывающий тег
              begin
                inc(aP);
              end;
          end
      end else

    if FText[aP]='>' then
      begin
        // пропускаем сдвоенный знак больше ">>"
        if (aP+1<=LL) and (FText[aP+1]='>') then
          begin
            Inc(aP,2);
          end
        else

        // закрытие "нашего" одинарного тега
        if (cNode.FTagType=ttSINGLE) and (aP-1>=1) and (FText[aP-1]='/') then
          begin
            cNode.FTextB := aP;
            cNode.FParsB := aP-2;
            while (FText[cNode.FParsB]<=' ') do
              cNode.FParsB:=cNode.FParsB-1;

            // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            // специальный тег смены префикса тега, для парсинга HTML
            if (cNode.FParent=FRoot) and
               (CompareText(FPrefix+ProfitSetPrefix,cNode.FName)=0) then
              begin
                cNode.FTagType:= ttPROFIT;
                FPrefix:= cNode.GetPars;
              end;
            // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            ParseParams(cNode);
            cNode:=cNode.FParent;
            inc(aP,1);
            aE := aP;
          end
        else

        // "наш" одинарный тег становится парным - ждем пару
        if (cNode.FTagType=ttSINGLE) then
          begin
            cNode.FBodyA :=aP+1;
            cNode.FParsB :=aP-1;
            while (FText[cNode.FParsB]<=' ') do
              cNode.FParsB:=cNode.FParsB-1;

            cNode.FTagType:= ttDOUBLE;
            ParseParams(cNode);
            inc(aP,1);
            aE := aP;
          end
        else
          // пропускаем чужой тег
          begin
            Inc(aP);
          end;
      end else

    // пропускаем символ, если он не структуризирующий, т.е. не "<",">"
    Inc(aP);
  end;

  if aE <= LL then TDeNode.Create( cNode, aE, LL );

  // остались "наши" не закрытые теги
  if cNode<>FRoot then
    begin
      AddErrorLine(cNode.FTextA,'Cann''t find closed tag "'+CNode.FName+'"');
      Exit;
    end;

  Result:= not Assigned(FEStrings);
end;

procedure TDeTemplate.SetText(const aString: string);
var aStringXML: String;
begin
  aStringXML:= StringReplace(   aString, #145, '''', [rfReplaceAll]);
  aStringXML:= StringReplace(aStringXML, #146, '''', [rfReplaceAll]);
  aStringXML:= StringReplace(aStringXML, #147, '"',  [rfReplaceAll]);
  aStringXML:= StringReplace(aStringXML, #148, '"',  [rfReplaceAll]);

  if FText = aStringXML then Exit;
  if Assigned(FEStrings) then FreeAndNil(FEStrings);
  FText:= aStringXML;
  Parse;
end;

end.

