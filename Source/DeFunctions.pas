{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}

unit DeFunctions;

interface
uses Contnrs, DB, Classes, msxmldom, XMLDoc, XMLIntf, System.JSON
     {$IFDEF INDYUSED},  IdCompressorZLib, IdHTTP, uTelegramManager{$ENDIF};

type
  /// <summary>массив аргументов функции</summary>
  TArrayArguments = array of Pointer; {array of TPostfixItem }

  { описатель функции }
  TDataTypesArray = array of TFieldType;
  TCheckSyntaxMethod = function(Sender: TObject; const aArgs: TArrayArguments; var aResultType: TFieldType): Integer;
  TExecuteMethod = function(Sender: TObject; const aArguments: array of Variant): Variant;

  TFunctionDescr = class
  private
    FName              : string;
    FDataType          : TFieldType;
    FArgTypes          : TDataTypesArray;
    FCheckSyntaxMethod : TCheckSyntaxMethod;
    FExecuteMethod     : TExecuteMethod;
    FIsHeavy           : Boolean;
    function GetArgCount: Integer;
  public
    constructor Create(const aName: string; const aDataType: TFieldType; const aArgTypes: TDataTypesArray;
                       aCheckSyntaxMethod: TCheckSyntaxMethod; aExecuteMethod: TExecuteMethod; aIsHeavy: Boolean = False);
    destructor Destroy; override;
    property Name: string read FName;
    property DataType: TFieldType read FDataType;
    property ArgCount: Integer read GetArgCount;
    property IsHeavy: Boolean read fIsHeavy;
    // проверяет правильность типов аргументов
    // возвращает 0, если ошибок не обнаружено, либо номер аргумента, вызвавшего ошибку (1..)
    // возвращает -1, если обнаружено несоответствие количества аргументов
    function CheckSyntax(const aArgs: TArrayArguments; var aResultType: TFieldType): Integer;
    // вычисляет значение функции
    function Execute(const aArgs: array of Variant): Variant;
    /// <summary>Метод сброса в начальное состояние контента функции</summary>
    procedure ResetContents; dynamic;
  end;

  TFunctionDescrClass = class of TFunctionDescr;

  { список функций }
  TFunctionList = class(TObjectList)
  private
    function GetItem(const Index: Integer): TFunctionDescr;
    procedure SetItem(const Index: Integer; const Value: TFunctionDescr);
  public
    property Items[const Index: integer]: TFunctionDescr read GetItem write SetItem; default;
    /// <summary>Метод регистрации функции в списке</summary>
    /// <param name="aFunctionName">Имя функции</param>
    /// <param name="aResultType">Тип возвращаемого результата</param>
    /// <param name="aArgTypes">Массив с типами аргументов</param>
    /// <param name="aCheckSyntaxMethod">Адрес метода проверки синтаксиса</param>
    /// <param name="aExecuteMethod">Адрес метода проверки значения</param>
    /// <param name="aFunctionDescrClass">Класс создаваемого дескриптора с информацией о функции</param>
    /// <returns>Функция вернёт индекс добавленной функции в списоке. При ошибке вернёт значение -1.</returns>
    function RegisterFunction(const aFunctionName: string; const aResultType: TFieldType;
      const aArgTypes: TDataTypesArray; aCheckSyntaxMethod: TCheckSyntaxMethod;
      aExecuteMethod: TExecuteMethod; aFunctionDescrClass: TFunctionDescrClass; aHeavy: Boolean = False): Integer; overload;
    /// <summary>Метод регистрации функции в списке</summary>
    /// <param name="aFunctionName">Имя функции</param>
    /// <param name="aResultType">Тип возвращаемого результата</param>
    /// <param name="aArgTypes">Массив с типами аргументов</param>
    /// <param name="aCheckSyntaxMethod">Адрес метода проверки синтаксиса</param>
    /// <param name="aExecuteMethod">Адрес метода проверки значения</param>
    /// <returns>Функция вернёт индекс добавленной функции в списоке. При ошибке вернёт значение -1.</returns>
    function RegisterFunction(const aFunctionName: string; const aResultType: TFieldType;
      const aArgTypes: TDataTypesArray; aCheckSyntaxMethod: TCheckSyntaxMethod;
      aExecuteMethod: TExecuteMethod; aHeavy: Boolean = False): Integer; overload;
    /// <summary>Метод сброса в начальное состояние контента функций</summary>
    procedure ResetContents;
    {$IFDEF DEBUG}
    procedure DebugFunctionsLog(const Text: string);
    {$ENDIF}
  end;

  {$IFDEF INDYUSED}
  TFunctionIndyDescr = class(TFunctionDescr)
  private
    FHTTP: TIdHTTP;
    procedure IdHTTPRedirect(Sender: TObject; var dest: string; var
      NumRedirect: Integer; var Handled: Boolean; var VMethod: string);
  public
    destructor Destroy; override;
    procedure ResetContents; override;
    function Get(const URL: string; Headers: TStrings): string;
  end;

  TFunctionTelegramDescr = class(TFunctionDescr)
  private
    FTelegramManager: TTelegramManager;
    function GetTelegramManager: TTelegramManager;
  public
    destructor Destroy; override;
    procedure ResetContents; override;
    property TelegramManager: TTelegramManager read GetTelegramManager;
  end;
  {$ENDIF}

  TFunctionPathXmlDescr = class(TFunctionDescr)
  private
    FContainer: TComponent;
    FDocument: TXMLDocument;
    function GetRootNode: IXMLNode;
  public
    destructor Destroy; override;
    procedure ResetContents; override;
    procedure LoadXML(const XML: string);
    property RootNode: IXMLNode read GetRootNode;
  end;

  function IncludeExecute(Sender: TObject; const aArguments: array of Variant): Variant;

var
  FunctionList: TFunctionList;

implementation

uses Windows, SysUtils, DateUtils, Variants, Math, StrUtils, NetEncoding, Vcl.Forms,
     {$IFDEF INDYUSED}IdCookieManager, IdSSLOpenSSL, uMailManager,{$ENDIF}
     DeLog, uPathXML, Funcs, DeParser, DeScript, Security, DeMetadata, DeMeta,
     DeTypes, DeDB, QueryDescriptor, Dictionary, DeControls, DataUnit, DSMeta,
     DataCacheUnit, UnitA {$IFDEF DEBUG}, uTextTable{$ENDIF};

{ TFunctionDescr }

constructor TFunctionDescr.Create(const aName: string; const aDataType: TFieldType; const aArgTypes: TDataTypesArray;
              aCheckSyntaxMethod: TCheckSyntaxMethod; aExecuteMethod: TExecuteMethod; aIsHeavy: Boolean = False);
var I : integer;
begin
  inherited Create;
  FName := aName;
  FDataType := aDataType;
  SetLength(FArgTypes, Length(aArgTypes));
  for I := 0 to Length(aArgTypes)-1 do
    FArgTypes[I] := aArgTypes[I];
  FCheckSyntaxMethod := aCheckSyntaxMethod;
  if not Assigned(aExecuteMethod) then
    raise EDeParserError.Create('Execute method expected in the function ' + aName);
  FExecuteMethod := aExecuteMethod;
  FIsHeavy:= aIsHeavy
end;

destructor TFunctionDescr.Destroy;
begin
  FArgTypes := nil;
  inherited;
end;

function TFunctionDescr.GetArgCount: Integer;
begin
  Result := Length(FArgTypes);
end;

function TFunctionDescr.CheckSyntax(const aArgs: TArrayArguments; var aResultType: TFieldType): Integer;
var I: Integer;
{$IFDEF DEBUG}
  function PrepareArgumentTypesLog: string;
  var
    TextTable: TTextTable;
    Count, Index: Integer;
  begin
    Result := EmptyStr;
    Count := Max(ArgCount, Length(aArgs));
    TextTable := TTextTable.Create;
    try
      TextTable.Columns.Add('No', 0, taRightJustify, taCenter);
      TextTable.Columns.Add('Type', 0, taLeftJustify, taLeftJustify);
      TextTable.Columns.Add('Default', 0, taLeftJustify, taLeftJustify);
      for Index := 0 to Pred(Count) do
        begin
          TextTable.Lines[Index][0] := IntToStr(Succ(Index));
          if Index < Length(aArgs) then
            TextTable.Lines[Index][1] := StrPas(FieldTypeNames[TPostfixItem(aArgs[Index]).ResultType]);
          if Index < ArgCount then
            TextTable.Lines[Index][2] := StrPas(FieldTypeNames[FArgTypes[Index]]);
        end;
      if TextTable.Lines.Count = 0 then
        TextTable.Lines[Index][1] := 'Empty array!!!';
      Result := TextTable.AsText(24);
    finally
      TextTable.Free;
    end;
    if Length(Result) <> 0 then
      Result := Result + #13#10;
    Result := Result + DupeString(' ', 24) + 'RESULT TYPE:  ' + StrPas(FieldTypeNames[aResultType]);
  end;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  DebugLog(Format('%s.CheckSyntax: before check for %s ...'#13#10'%s', [ClassName, QuotedStr(Name), PrepareArgumentTypesLog]));
  {$ENDIF}
  aResultType := DataType;
  if ArgCount <> Length(aArgs) then
    Result := -1
  else
  begin
    result := 0;
    for I := 0 to ArgCount-1 do
      if (FArgTypes[i] <> ftUnknown) and
         (TPostfixItem(aArgs[i]).ResultType <> ftUnknown) and
         not( TypesCompatible(TPostfixItem(aArgs[i]).ResultType, FArgTypes[i], tcInclude) ) then
//      (ArgType[I] <> aArgTypes[I]) then
      begin
        result := I+1;
        break;
      end;
  end;
  if (Result = 0) and Assigned(FCheckSyntaxMethod) then
    Result := FCheckSyntaxMethod(Self, aArgs, aResultType);
  {$IFDEF DEBUG}
  DebugLog(Format('%s.CheckSyntax: after check for %s ...'#13#10'%s'#13#10'%sRESULT VALUE: %d', [ClassName, QuotedStr(Name), PrepareArgumentTypesLog, DupeString(' ', 24), Result]));
  {$ENDIF}
end;

function TFunctionDescr.Execute(const aArgs: array of Variant): Variant;
begin
  if Assigned(FExecuteMethod) then
    Result := FExecuteMethod(Self, aArgs)
  else
    Result := Unassigned;
end;

procedure TFunctionDescr.ResetContents;
begin
  // В базовом классе нет контента и делать ничего не надо!!!
end;

{ TFunctionList }

function TFunctionList.GetItem(const Index: Integer): TFunctionDescr;
begin
  Result := TFunctionDescr(inherited Items[Index]);
end;

procedure TFunctionList.SetItem(const Index: Integer; const Value: TFunctionDescr);
begin
  if Index >= Count then Count := Succ(Index);
  inherited Items[Index] := Value;
end;

function TFunctionList.RegisterFunction(const aFunctionName: string; const aResultType: TFieldType;
  const aArgTypes: TDataTypesArray; aCheckSyntaxMethod: TCheckSyntaxMethod; aExecuteMethod: TExecuteMethod;
  aFunctionDescrClass: TFunctionDescrClass; aHeavy: Boolean = False): Integer;
begin
  Result := Add(aFunctionDescrClass.Create(aFunctionName, aResultType, aArgTypes, aCheckSyntaxMethod, aExecuteMethod, aHeavy));
end;

function TFunctionList.RegisterFunction(const aFunctionName: string; const aResultType: TFieldType;
  const aArgTypes: TDataTypesArray; aCheckSyntaxMethod: TCheckSyntaxMethod; aExecuteMethod: TExecuteMethod;
  aHeavy: Boolean = False): Integer;
begin
  Result := RegisterFunction(aFunctionName, aResultType, aArgTypes, aCheckSyntaxMethod, aExecuteMethod, TFunctionDescr, aHeavy);
end;

procedure TFunctionList.ResetContents;
var
  Index: Integer;
begin
  for Index := 0 to Pred(Count) do
    Items[Index].ResetContents;
end;

{$IFDEF DEBUG}
procedure TFunctionList.DebugFunctionsLog(const Text: string);
  function DataTypesArrayToString(const DataTypesArray: TDataTypesArray): string;
  var
    Index: Integer;
  begin
    Result := EmptyStr;
    for Index := Low(DataTypesArray) to High(DataTypesArray) do
      begin
        if Length(Result) <> 0 then
          Result := Result + ', ';
        Result := Result + StrPas(FieldTypeNames[DataTypesArray[Index]]);
      end;
    Result := '[' + Result + ']';
  end;
  function PrepareFunctionsLog: string;
  var
    TextTable: TTextTable;
    Index: Integer;
    FunctionDescr: TFunctionDescr;
  begin
    if Count = 0 then
      Result := EmptyStr
    else
      begin
        TextTable := TTextTable.Create;
        try
          TextTable.Columns.Add('No', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Class', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('C', 1, taCenter, taCenter);
          TextTable.Columns.Add('E', 1, taCenter, taCenter);
          TextTable.Columns.Add('H', 1, taCenter, taCenter);
          TextTable.Columns.Add('Type', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Name', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Parameters', 0, taLeftJustify, taLeftJustify);
          for Index := 0 to Pred(Count) do
            begin
              TextTable.Lines[Index][0] := IntToStr(Succ(Index));
              FunctionDescr := Items[Index];
              if Assigned(FunctionDescr) then
                begin
                  TextTable.Lines[Index][1] := FunctionDescr.ClassName;
                  TextTable.Lines[Index][2] := iif(Assigned(FunctionDescr.FCheckSyntaxMethod), '+', '-');
                  TextTable.Lines[Index][3] := iif(Assigned(FunctionDescr.FExecuteMethod), '+', '-');
                  TextTable.Lines[Index][4] := iif(FunctionDescr.FIsHeavy, '+', '-');
                  TextTable.Lines[Index][5] := StrPas(FieldTypeNames[FunctionDescr.DataType]);
                  TextTable.Lines[Index][6] := FunctionDescr.Name;
                  TextTable.Lines[Index][7] := DataTypesArrayToString(FunctionDescr.FArgTypes);
                end;
            end;
          Result := #13#10 + TextTable.AsText(24);
        finally
          TextTable.Free;
        end;
      end;
  end;
begin
  DebugLog(Text + PrepareFunctionsLog);
end;
{$ENDIF}

{$IFDEF INDYUSED}

{ TFunctionIndyDescr }

destructor TFunctionIndyDescr.Destroy;
begin
  FreeAndNil(FHTTP);
  inherited Destroy;
end;

procedure TFunctionIndyDescr.ResetContents;
begin
  inherited ResetContents;
  FreeAndNil(FHTTP);
end;

procedure TFunctionIndyDescr.IdHTTPRedirect(Sender: TObject; var dest: string; var
    NumRedirect: Integer; var Handled: Boolean; var VMethod: string);
begin
   Handled := True;
end;

function TFunctionIndyDescr.Get(const URL: string; Headers: TStrings): string;
var
  Index: Integer;
  Name, Value: string;
begin
  if Assigned(FHTTP) then
    begin
      if assigned(FHTTP.IOHandler) then FHTTP.IOHandler.Free;
      if assigned(FHTTP.CookieManager) then FHTTP.CookieManager.Free;
      if assigned(FHTTP.Compressor) then FHTTP.Compressor.Free;

      FHTTP.Free;
      FHTTP:= nil;
    end;

  if not Assigned(FHTTP) then
    begin
      FHTTP := TIdHTTP.Create(nil);

      FHTTP.HandleRedirects:= True;
      FHTTP.RedirectMaximum:= 15;
      FHTTP.OnRedirect:= IdHTTPRedirect;

      FHTTP.AllowCookies:= True;
      FHTTP.CookieManager:= TIdCookieManager.Create(FHTTP);

      FHTTP.ProtocolVersion:= pv1_1;
      FHTTP.HTTPOptions:= FHTTP.HTTPOptions + [hoInProcessAuth,hoKeepOrigProtocol,hoTreat302Like303,hoNoProtocolErrorException];

      FHTTP.Response.KeepAlive:= True;
      FHTTP.Request.Connection:= 'keep-alive';
      FHTTP.Request.CacheControl:='max-age=0';
      FHTTP.Request.UserAgent:='Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/66.0.3359.181 Safari/537.36';
//    FHTTP.Request.UserAgent:= 'Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 1.1.4322)';
//    FHTTP.Request.Accept:='text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8';
//    FHTTP.Request.AcceptLanguage:='ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7';

      if Pos('HTTPS:', UpperCase(URL)) = 1 then
        if DM.OpenSSL then
          begin
            FHTTP.IOHandler:= TIdSSLIOHandlerSocketOpenSSL.Create(FHTTP);
            TIdSSLIOHandlerSocketOpenSSL(FHTTP.IOHandler).SSLOptions.SSLVersions:= [ sslvTLSv1_1,sslvTLSv1_2]
          end
        else
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar('Can''t load OpenSSL library')));

      FHTTP.Request.CustomHeaders.Clear;
      FHTTP.Request.CustomHeaders.Add('Upgrade-Insecure-Requests: 1');
      FHTTP.Request.CustomHeaders.Add('X-Compress: null');
      FHTTP.Request.AcceptEncoding:='gzip, deflate';
      FHTTP.Compressor:= TIdCompressorZLib.Create(FHTTP);

      for Index := 0 to Pred(Headers.Count) do
        begin
          Name := Headers.Names[Index];
          Value := Headers.ValueFromIndex[Index];

          if SameText(Name, 'Allow-Cookies') and  SameText(Value, 'True') then
            begin
              FHTTP.AllowCookies:= True;
              FHTTP.CookieManager:= TIdCookieManager.Create(FHTTP);
            end else

          if SameText(Name, 'Handle-Redirects') and  SameText(Value, 'True') then
            begin
              FHTTP.HandleRedirects:= True;
              FHTTP.RedirectMaximum:= 15;
              FHTTP.OnRedirect:= IdHTTPRedirect;
            end else

          if SameText(Name, 'Keep-Alive') and  SameText(Value, 'True') then FHTTP.Response.KeepAlive:= True else
          if SameText(Name, 'User-Agent') then FHTTP.Request.UserAgent:= Value else
          if SameText(Name, 'Host') then FHTTP.Request.Host:= Value else
          if SameText(Name, 'Referer') then FHTTP.Request.Referer:= Value else
          if SameText(Name, 'Accept') then FHTTP.Request.Accept:= Value else
          if SameText(Name, 'Accept-Charset') then FHTTP.Request.AcceptCharSet:= Value else
          if SameText(Name, 'Accept-Encoding') then FHTTP.Request.AcceptEncoding:= Value else
          if SameText(Name, 'Accept-Language') then FHTTP.Request.AcceptLanguage:= Value else

          FHTTP.Request.CustomHeaders.Values[Name]:= Value;
        end;
    end;

  try
    Result:= FHTTP.Get(URL);
    FHTTP.Disconnect;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('%s.Get request headers:'#13#10'%s', [ClassName, FHTTP.Request.RawHeaders.Text]);
        DebugLog('%s.Get response headers:'#13#10'%s', [ClassName, FHTTP.Response.RawHeaders.Text]);
        {$ENDIF}
        raise;
      end;
  end;
end;

{ TFunctionTelegramDescr }

destructor TFunctionTelegramDescr.Destroy;
begin
  FreeAndNil(FTelegramManager);
  inherited Destroy;
end;

procedure TFunctionTelegramDescr.ResetContents;
begin
  inherited ResetContents;
  FreeAndNil(FTelegramManager);
end;

function TFunctionTelegramDescr.GetTelegramManager: TTelegramManager;
begin
  if not Assigned(FTelegramManager) then
    begin
      if Pos('https://', LowerCase(cTelegramAPI)) = 1 then
        if not DM.OpenSSL then
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(PChar('Can''t load OpenSSL library')));
      FTelegramManager := TTelegramManager.Create;
    end;
  Result := FTelegramManager;
end;
{$ENDIF}

{ TFunctionPathXmlDescr }

destructor TFunctionPathXmlDescr.Destroy;
begin
  FDocument := nil;
  FreeAndNil(FContainer);
  inherited Destroy;
end;

procedure TFunctionPathXmlDescr.ResetContents;
begin
  inherited ResetContents;
  FDocument := nil;
  FreeAndNil(FContainer);
end;

procedure TFunctionPathXmlDescr.LoadXML(const XML: string);
begin
  // разрешает ProhibitDTD
  msxmldom.MSXMLDOMDocumentFactory.AddDOMProperty('ProhibitDTD', False);

  if Assigned(FDocument) then
    begin
      if not SameText(FDocument.XML.Text, XML) then
        begin
          FDocument.Active := False;
          FDocument.XML.Text := XML;
          FDocument.Active := True;
        end;
    end
  else
    begin
      if not Assigned(FContainer) then FContainer := TComponent.Create(nil);
      FDocument := TXMLDocument.Create(FContainer);
      FDocument.XML.Text := XML;
      FDocument.Active := True;
    end;
end;

function TFunctionPathXmlDescr.GetRootNode: IXMLNode;
begin
  if Assigned(FDocument) then
    Result := FDocument.Node
  else
    Result := nil;
end;

{ список функций }

function IIfCheckSyntax(Sender: TObject; const aArgs: TArrayArguments; var aResultType: TFieldType): Integer;
var aType1, aType2 : TFieldType;
begin
  Result:= 0;
  aType1:= TPostfixItem(aArgs[1]).ResultType;
  aType2:= TPostfixItem(aArgs[2]).ResultType;
  if ( aType1 = aType2 )                                       then aResultType:= aType1  else
  if ((aType1 in StringTypes  ) and (aType2 in StringTypes)  ) then aResultType:= ftString else
  // v.16.8 +
  if ((aType1 in IntegerTypes ) and (aType2 in IntegerTypes) ) then aResultType:= ftLargeInt else
  // v.16.8 -
  if ((aType1 in NumericTypes ) and (aType2 in NumericTypes) ) then aResultType:= ftFloat else
  if ((aType1 in DateTimeTypes) and (aType2 in DateTimeTypes)) then aResultType:= ftDateTime else
  if (aType1 = ftUnknown)                                      then aResultType:= aType2 else
  if (aType2 = ftUnknown)                                      then aResultType:= aType1 else
                                                                    Result:= 3;
end;

function IIfExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  if aArguments[0] then
    Result := aArguments[1]
  else
    Result := aArguments[2];
end;

function SelectedExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var TM: TTableMeta;
    DSM: TDataSetMeta;
begin
  Result:= unassigned;
  TM:= Metadata.TablesList.FindByTable(aArguments[0]);
  if assigned(TM) then
    begin
      DSM:= Metadata.FindActiveDataSet(TM.ID, True);
      if assigned(DSM) then
        Result:= DSM.GetKeyValue(gkmSelected);
    end;
end;

function FocusedExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var TM: TTableMeta;
    DSM: TDataSetMeta;
begin
  Result:= unassigned;
  TM:= Metadata.TablesList.FindByTable(aArguments[0]);
  if assigned(TM) then
    begin
      DSM:= Metadata.FindActiveDataSet(TM.ID, True);
      if assigned(DSM) then
        Result:= DSM.GetKeyValue(gkmFocused);
    end;
end;

function ValueSelectExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var TM: TTableMeta;
    FM: TFieldMeta;
    Dataset: TDeDataset;
    ReadyCache: TDataCache;
    N,M: Integer;
begin
   Result:= unassigned;

  // А вдруг есть подходящий открытый lookup датасет с нужными полями!? тогда обходимся без запроса
  if (VarType(aArguments[0]) in IntVarTypes) then
    begin
      ReadyCache:= Metadata.GetLibrary(aArguments[0], False); // здесь не осоздаем справочник, это надо делать заранее
      if assigned(ReadyCache) then // нашли подходящий готовый датасет
        begin
{          if VarIsNull(aArguments[1])
            then N:=ReadyCache.CaptionIndex // если не указано - берем главное поле
            else{} N:= ReadyCache.Fields.IndexByName(aArguments[1]);
          if -1 < N then // нашли нужное поле
            begin
              M:= ReadyCache.IndexByID(aArguments[2]);
              if -1 < M then
                Result:= ReadyCache.Items[M].FieldNativeValue[N];
              Exit;
            end;
        end;
    end;

  if (VarType(aArguments[0]) in IntVarTypes)
    then TM:= MetaData.TablesList.FindByID(aArguments[0]) else
  if (VarType(aArguments[0]) = varString) or  (VarType(aArguments[0]) = varUString)
    then TM:= MetaData.TablesList.FindByTable(aArguments[0]) else
         Exit;

  if Not assigned(TM) then Exit;

  FM:= TM.Fields.FindByName(aArguments[1]);
  if Not assigned(FM) then Exit;

  Dataset := TM.Database.CreateQuery(qtRow);
  try
    Dataset.Descr.Table := TM.Table;
    Dataset.Descr.AddField(FM.Original);
    Dataset.Descr.AddCondition( TM.KField[0].Original, TM.KField[0].DataType, opEQ, aArguments[2]);
    Dataset.Open(True);
    if Dataset.RecordCount = 1 then
      Result:= Dataset.Value[0];
  finally
    Dataset.Free;
  end;
end;

function InnerSelectExecute(aTable: TTableMeta; sField, SFilter: String): Variant;
var i: Integer;
    aCache: TDataCache;
    aParser: TDeParser;
    aFilter: TFilterItem;
    StageValue: TFieldStage;
    v: Variant;
begin
  try
    aCache := TDataCache.Create(aTable);
    with aCache.Fields.FindByName(sField) do
      if Calculated then
        begin
          StageValue:= fsFull;
        end
      else
        begin
          StageValue:= fsKey;
          Stage:= fsKey;
        end;

    if (Length(sFilter) > 0) then
      begin
        aParser:= TDeParser.Create;
        try
          aParser.Table:= aTable;
          aFilter:= TFilterItem.Create;
          try
            aParser.Parse(sFilter, aFilter);
          except
            on E: EDeParserError do
              begin
                aFilter.Free;
                Raise Exception.Create('Filter Error in SelectExecute: '+E.Message);
              end;
          end;
          aCache.LoadData(aFilter, StageValue);
        finally
          aParser.Free;
        end;
      end
    else
      begin
        aCache.LoadData(nil, StageValue);
      end;

    if 1 = aCache.Count then
      begin
        Result:= aCache[0].ValueByName[sField];
      end else
    if 1 < aCache.Count then
      begin
        aCache.FillAll;
        Result:= VarArrayCreate([0, aCache.Count-1], varVariant);
        VarArrayLock(Result);
        for i:=0 to aCache.Count-1 do
          begin
            v:=aCache[i].ValueByName[sField];
//            Result[i]:= v;
            VarArrayPut(Result, V, [i]);
          end;
        VarArrayUnlock(Result);
      end;

    aCache.CloseData;
    aCache.Free;
  except
    {$IFDEF DEBUG}
    on E: Exception do DebugLog('SelectArray error: ' + E.Message);
    {$ENDIF}
  end;
end;

function SelectABCCheckSyntax(Sender: TObject; const aArgs: TArrayArguments; var aResultType: TFieldType): Integer;
var
  TableName: string;
  TableGUID: TGUID;
  TableMeta: TTableMeta;
  TableField: TFieldMeta;
begin
  Result:= 0;
  if (TPostfixItem(aArgs[0]).ItemType <> piConst) then Exit;

  TableMeta := nil;

  if Assigned(MetaData) and Assigned(MetaData.TablesList) then
    if VarIsStr(TPostfixItem(aArgs[0]).Data) then
      begin
        TableName := Trim(TPostfixItem(aArgs[0]).Data);
        if TryStringToGUID(TableName, TableGUID) then
          TableMeta := MetaData.TablesList.FindByGUID(TableGUID)
        else
          TableMeta := MetaData.TablesList.FindByTable(TableName);
      end
    else
      if VarType(TPostfixItem(aArgs[0]).Data) in IntVarTypes then
        TableMeta := MetaData.GetTableMeta(TPostfixItem(aArgs[0]).Data);

  if not Assigned(TableMeta) then Exit(1);

  if (TPostfixItem(aArgs[1]).ItemType <> piConst) then Exit;

  TableField := TableMeta.Fields.FindByName(Trim(TPostfixItem(aArgs[1]).Data));
  if not Assigned(TableField) then Exit(2);

  aResultType := TableField.DataType;
end;

function SelectABCExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  TableName: string;
  TableGUID: TGUID;
  TableMeta: TTableMeta;
  TableField: TFieldMeta;
begin
  Result := Unassigned;
  TableMeta := nil;

  if Assigned(MetaData) and Assigned(MetaData.TablesList) then
    if VarIsStr(aArguments[0]) then
      begin
        TableName := aArguments[0];
        if TryStringToGUID(TableName, TableGUID) then
          TableMeta := MetaData.TablesList.FindByGUID(TableGUID)
        else
          TableMeta := MetaData.TablesList.FindByTable(TableName);
      end
    else
      if VarType(aArguments[0]) in IntVarTypes then
        TableMeta := MetaData.GetTableMeta(aArguments[0]);

  if not Assigned(TableMeta) then
    raise Exception.Create(Format('Table "%s" not exist',[aArguments[0]]));

  if not SecuritySystem.CheckPolicyDataSet(TableMeta.ID, spSelect) then Exit;

  TableField := TableMeta.Fields.FindByName(Trim(aArguments[1]));
  if not Assigned(TableField) then
    raise Exception.Create(Format('Field "%s" not exist in table "%s"',[aArguments[1], aArguments[0]]));

  if not SecuritySystem.CheckPolicyField(TableField.ID, sdSelect) then Exit;

  if Length(aArguments) = 3 then Result:= InnerSelectExecute(TableMeta, TableField.Original, aArguments[2])
                            else Result:= InnerSelectExecute(TableMeta, TableField.Original, EmptyStr);
end;

function SelectSExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var aTable: TTableMeta;
    aField: TFieldMeta;
    s, sTable, sField: string;
    p: Integer;
begin
  s:=Trim(aArguments[0]);

  p:= pos(',', s);
  if 0 < p then begin sTable:= Trim(Copy(s, 1, p-1)); s:= Copy(s, p+1, MaxInt); end
           else begin sTable:= EmptyStr;                                        end;

  p:= pos(',', s);
  if 0 < p then begin sField:= Trim(Copy(s, 1, p-1)); s:= Copy(s, p+1, MaxInt); end
           else begin sField:= s;                     s:= EmptyStr;             end;

  //разобрали строку на части, теперь ищем таблицу и поля
  aTable:=MetaData.GetTableByName(Trim(sTable));
  if not Assigned(aTable) then
    Raise Exception.Create(Format('Table "%s" not exist',[sTable]));

  if Not SecuritySystem.CheckPolicyDataSet(aTable.ID, spSelect) then Exit;

  aField:= aTable.Fields.FindByName(Trim(sField));
  if not Assigned(aField) then
    Raise Exception.Create(Format('Field "%s" not exist in table "%s"',[sField, sTable]));

  if Not SecuritySystem.CheckPolicyField(aField.ID, sdSelect) then Exit;

  Result:= InnerSelectExecute(aTable, sField, s);
end;

var
  Version: Double;

function VersionExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := Version;
end;

function TodayExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var DT: TDateTime;
begin
  DT:= Today;
  Result:= DT;
end;

function TomorrowExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var DT: TDateTime;
begin
  DT:= Tomorrow;
  Result := DT;
end;

function YesterdayExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var DT: TDateTime;
begin
  DT:= Yesterday;
  Result := DT;
end;

function DateExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var DT: TDateTime;
begin
  DT:= Date();
  result := DT;
end;

function TimeExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var DT: TTime;
begin
  DT := Time();
  result := DT;
end;

function NowExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var DT: TDateTime;
begin
  DT := Now;
  Result:= DT;
end;

function DateOfExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := DateOf(aArguments[0]);
end;

function TimeOfExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := TimeOf(aArguments[0]);
end;

function DayOfExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := DayOf(aArguments[0]);
end;

function MonthOfExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := MonthOf(aArguments[0]);
end;

function YearOfExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := YearOf(aArguments[0]);
end;

function DaysInMonthExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := DaysInMonth(aArguments[0]); // Последний день месяца (28, 29, 30, 31)
end;

function DaysInYearExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := DaysInYear(aArguments[0]); // Количество дней в году (365, 366)
end;

function WeekOfExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := WeekOf(aArguments[0]);
end;

function DayOfWeekExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := DayOfTheWeek(aArguments[0]); // ISO 8601 (1 - пн, 2 - вт, 3 - ср, 4 - чт, 5 - пт, 6 - сб, 7 - вс)
end;

function DayOfYearExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := DayOfTheYear(aArguments[0]); // Номер дня в году
end;

function EncodeDateExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := EncodeDate(aArguments[0], aArguments[1], aArguments[2]);
end;

function IncDayExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := IncDay(aArguments[0], aArguments[1]);
end;

function IncWeekExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := IncWeek(aArguments[0], aArguments[1]);
end;

function IncMonthExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := IncMonth(aArguments[0], aArguments[1]);
end;

function IncYearExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := IncYear(aArguments[0], aArguments[1]);
end;

function IncHourExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := IncHour(aArguments[0], aArguments[1]);
end;

function IncMinuteExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := IncMinute(aArguments[0], aArguments[1]);
end;

function IncSecondExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := IncSecond(aArguments[0], aArguments[1]);
end;

function IncMilliSecondExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := IncMilliSecond(aArguments[0], aArguments[1]);
end;

function DaysBetweenExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := DaysBetween(aArguments[0], aArguments[1]);
end;

function WeeksBetweenExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := WeeksBetween(aArguments[0], aArguments[1]);
end;

function MonthsBetweenExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := MonthsBetween(aArguments[0], aArguments[1]);
end;

function YearsBetweenExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var D0, M0, Y0, D1, M1, Y1 : Word;
begin
  DecodeDate(aArguments[0], Y0, M0, D0);
  DecodeDate(aArguments[1], Y1, M1, D1);

  // исключение с 29.02.2000 по 28.02.2001 - считаем, что прошел год !!!
  if (M0=2) and (D0=29) and (M1=2) and (D1=28) and (DaysInYear(Y1)=365) then Exit( Max(0, Y1 - Y0) );

  if (M1>M0) or ( (M1=M0) and (D1>=D0) ) then Result:= Max(0, Y1 - Y0     )
                                         else Result:= Max(0, Y1 - Y0 - 1 );
end;

function ToDateTimeExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  DateTime: TDateTime;
begin
  case VarType(aArguments[0]) of
    varByte, varShortInt, varWord, varSmallInt, varInteger, varLongWord,
    varInt64, varUInt64, varSingle, varDouble, varCurrency, varDate:
      try
        DateTime := aArguments[0];
        Result := DateTime;
      except
        Result := Null;
      end;
    varString, varOleStr, varUString:
      if TryStrToDateTime(VarToStr(aArguments[0]), DateTime) then Result := DateTime else
      if TryStrToDateTime(StringReplace(VarToStr(aArguments[0]), 'T', ' ', []), DateTime) then Result := DateTime else
      if TryStrToDateTime(StringReplace(VarToStr(aArguments[0]), 'T', ' ', []), DateTime, Funcs.fs) then Result := DateTime else
          Result:= Null;
  else
    Result := Null;
  end;
end;

function FormatDateTimeExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  DateTime: TDateTime;
begin
  DateTime := aArguments[1];
  Result := FormatDateTime(VarToStr(aArguments[0]), DateTime);
end;

function DateTimeToStringODBCExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  DateTime: TDateTime;
begin
  DateTime := aArguments[0];
  Result := FormatDateTime('YYYY"-"MM"-"DD" "HH":"NN":"SS', DateTime);
end;

function DateTimeToStringMSSQLExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  DateTime: TDateTime;
begin
  DateTime := aArguments[0];
  Result := FormatDateTime('YYYYMMDD" "HH":"NN":"SS"."ZZZ', DateTime);
end;

function DateTimeToStringISO8601Execute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  DateTime: TDateTime;
begin
  DateTime := aArguments[0];
  Result := FormatDateTime('YYYY"-"MM"-"DD"T"HH":"NN":"SS"."ZZZ', DateTime);
end;

function ToFloatExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  Value: Extended;
  Text: string;
begin
  case VarType(aArguments[0]) of
    varByte, varShortInt, varWord, varSmallInt, varInteger, varLongWord,
    varInt64, varUInt64, varSingle, varDouble, varCurrency, varDate:
      try
        Value := aArguments[0];
        Result := Value;
      except
        Result := Null;
      end;
    varString, varOleStr, varUString:
      begin
        Text := ReplaceText(VarToStr(aArguments[0]), DeDecimalSeparator, FormatSettings.DecimalSeparator);
        if TryStrToFloat(Text, Value, FormatSettings) then
          Result := Value
        else
          Result := Null;
      end;
  else
    Result := Null;
  end;
end;

function ToMoneyExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  Value: Currency;
  Text: string;
begin
  case VarType(aArguments[0]) of
    varByte, varShortInt, varWord, varSmallInt, varInteger, varLongWord,
    varInt64, varUInt64, varSingle, varDouble, varCurrency, varDate:
      try
        Value := aArguments[0];
        Result := Value;
      except
        Result := Null;
      end;
    varString, varOleStr, varUString:
      begin
        Text := ReplaceText(VarToStr(aArguments[0]), DeDecimalSeparator, FormatSettings.DecimalSeparator);
        if TryStrToCurr(Text, Value, FormatSettings) then
          Result := Value
        else
          Result := Null;
      end;
  else
    Result := Null;
  end;
end;

function VarIsDate(const V: Variant): Boolean;
begin
  Result := (FindVarData(V)^.VType = varDate);
end;

function ToIntegerExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result:=null;
  if VarIsClear(aArguments[0]) or VarIsNull(aArguments[0]) then Result:=null else

  if VarIsOrdinal(aArguments[0])    then try Result := Integer(aArguments[0])  except Result:=null; end else
  if VarIsStr(aArguments[0])        then try Result := StrToInt(aArguments[0]) except Result:=null; end else
  if VarIsFloat(aArguments[0])      then try Result := Round(aArguments[0])    except Result:=null; end else
  if VarIsDate(aArguments[0])       then try Result := Trunc(aArguments[0])    except Result:=null; end;
end;

function ToStringExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := VarToStr(aArguments[0]);
end;

function ToCharExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := Char(VarToInt(aArguments[0]));
end;

function FormatExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result:= FormatValue(aArguments[0], aArguments[1]);
end;

function LengthExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := Length(VarToStr(aArguments[0]));
end;

function TruncExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := Trunc(aArguments[0]);
end;

function InnerRound(f: Extended): Extended; inline;
begin
  if (0.5 <= Frac(f)) and (f > 0) then Result:= Int(f)+1
                                  else Result:= Int(f);
end;

function RoundExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result:= InnerRound(aArguments[0]);
end;

function RoundExtExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var i, M: Integer;
begin
  M:=1;
  for i := 1 to VarToInt(abs(aArguments[1])) do M:=M*10;

  if aArguments[1] > 0 then Result:= InnerRound(aArguments[0]*M)/M else
  if aArguments[1] < 0 then Result:= InnerRound(aArguments[0]/M)*M else
                            Result:= InnerRound(aArguments[0]);
end;

function FracExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := Frac(aArguments[0]);
end;

function AbsExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := Abs(aArguments[0]);
end;

function AbsExtCheckSyntax(Sender: TObject; const aArgs: TArrayArguments; var aResultType: TFieldType): Integer;
begin
  Result := 0;

  if TPostfixItem(aArgs[0]).ResultType in IntegerTypes then
    aResultType := ftInteger
  else
    if TPostfixItem(aArgs[0]).ResultType in FloatTypes then
      aResultType :=  ftFloat
    else
      Result := 1;
end;

function AbsExtExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  IntValue: Int64;
  ExtValue: Extended;
begin
  case VarType(aArguments[0]) of
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord, varInt64, varUInt64: { Целое }
      begin
        IntValue := VarAsType(aArguments[0], varInt64);
        Result := Abs(IntValue);
      end;
    varSingle, varDouble, varCurrency: { Вещественное }
      begin
        ExtValue := VarAsType(aArguments[0], varDouble);
        Result := Abs(ExtValue);
      end;
  else
    Result := Null;
  end;
end;

function PowerExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := Power ( (aArguments[0]) , (aArguments[1]) );
end;

function RandomExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := Random(Trunc(aArguments[0]));
end;

function SubstringExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  try
    result := Copy( VarToStr(aArguments[0]), VarToInt(aArguments[1]), VarToInt(aArguments[2]) )
  except
    result := EmptyStr;
  end;
end;

function ExtractStringExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var pFrom,pTo: Integer;
begin
  try
    if Length(VarToStr(aArguments[1])) = 0
      then pFrom:= 1
      else pFrom:= Pos(VarToStr(aArguments[1]), VarToStr(aArguments[0]))+Length(aArguments[1]);

    if Length(VarToStr(aArguments[2])) = 0
      then pTo:= MaxInt
      else pTo:= Pos(VarToStr(aArguments[2]), VarToStr(aArguments[0]));

    if (0 < pFrom) and (0 < pTo)
      then result:= Copy( VarToStr(aArguments[0]), pFrom, pTo-pFrom )
      else result:= EmptyStr;
  except
    result := EmptyStr;
  end;
end;

function SplitStringExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var S,Q: String;
    P,N: Integer;
begin
  S:= VarToStr(aArguments[0]);
  N:= VarToInt(aArguments[1]);
  if Length(aArguments) = 3 then Q:= VarToStr(aArguments[2])
                            else Q:= Char(10);
  Result:= EmptyStr;
  while 0 < Length(S) do
    begin
      P:= Pos(Q,S);
      if P=0 then
        if N=0 then Exit(S)
               else S:= EmptyStr
      else
        if N=0 then Exit(Copy(s,1,P-1))
               else s:= Copy(s,P+Length(Q), MaxInt);
      Dec(N);
    end;
end;

function TrimExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := Trim( VarToStr(aArguments[0]) );
end;

function UpperCaseExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := AnsiUpperCase( VarToStr(aArguments[0]) );
end;

function LowerCaseExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := AnsiLowerCase( VarToStr(aArguments[0]) );
end;

function ReplaceExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  if VarIsEmpty(aArguments[0])
    then result:= aArguments[0]
    else result:= StringReplace( VarToStr(aArguments[0]), VarToStr(aArguments[1]), VarToStr(aArguments[2]), [rfReplaceAll] );
end;

function PosExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  result := Pos( VarToStr(aArguments[0]), VarToStr(aArguments[1]) );
end;

function CounterExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var s,s1,s2,s3 : String;
    N          : Integer;
    VI         : TVariableItem;
begin
  s:=(aArguments[0]);
  N:=StrToIntDef(s,MaxInt);
  if N=MaxInt then
    begin
      VI:=GlobalVariables.FindVariable(s);
      if Not (VI=nil) then  s:=VI.Value;

      s1:= EmptyStr;
      s2:= EmptyStr;
      s3:= EmptyStr;
      while (Length(s)>0) and Not CharInSet(s[1], ['0'..'9'] ) do
        begin  s1:=s1+Copy(s,1,1); Delete(s,1,1);  end;
      while (Length(s)>0) and  CharInSet(s[1], ['0'..'9'] ) do
        begin  s2:=s2+Copy(s,1,1); Delete(s,1,1);  end;
      s3:=s;

      s:=IntToStr(StrToIntDef(s2,0)+1);
      While Length(s)<Length(s2) do s:='0'+s;

      if Not (VI=nil) then VI.Value:=s1+s+s3;

      Result:=s1+s+s3;
    end
  else
    begin
      result := N+1;
    end;
end;

function IncludeExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  Index: Integer;
  procedure Append(const Value: Variant);
  var
    Index: Integer;
    procedure Add(const Value: Variant);
    var
      Index: Integer;
    begin
      if not (VarIsClear(Value) or VarIsNull(Value)) then
        begin
          if VarIsEmpty(Result) then
            begin
              Index := 0;
              Result := VarArrayCreate([0, Index], varVariant);
            end
          else
            begin
              // Проверяем на дублирование значения ...
              for Index := VarArrayLowBound(Result, 1) to VarArrayHighBound(Result, 1) do
                if DeVarSameValue(Value, VarArrayGet(Result, [Index])) then
                  Exit;
              // Если не дубль, то добавляем значение в массив ...
              Index := Succ(VarArrayHighBound(Result, 1));
              VarArrayRedim(Result, Index);
            end;
          VarArrayPut(Result, Value, [Index]);
        end;
    end;
  begin
    if VarIsArray(Value) then
      for Index := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
        Add(VarArrayGet(Value, [Index]))
    else
      Add(Value);
  end;
begin
  Result := Unassigned;
  for Index := Low(aArguments) to High(aArguments) do
    Append(aArguments[Index]);
end;

function ExcludeExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  Index: Integer;
  //Value: Variant;
  procedure Append(const Value: Variant);
  var
    Index: Integer;
    function IsExcluded(const ExcludeValue: Variant): Boolean;
    var
      Index: Integer;
    begin
      if VarIsArray(ExcludeValue) then
        begin
          Result := False;
          for Index := VarArrayLowBound(ExcludeValue, 1) to VarArrayHighBound(ExcludeValue, 1) do
            if DeVarSameValue(Value, VarArrayGet(ExcludeValue, [Index])) then
              begin
                Result := True;
                Break;
              end;
        end
      else
        Result := DeVarSameValue(Value, ExcludeValue);
    end;
    procedure Add(const Value: Variant);
    var
      Index: Integer;
    begin
      if not (VarIsClear(Value) or VarIsNull(Value)) then
        begin
          if VarIsEmpty(Result) then
            begin
              Index := 0;
              Result := VarArrayCreate([0, Index], varVariant);
            end
          else
            begin
              // Проверяем на дублирование значения ...
              for Index := VarArrayLowBound(Result, 1) to VarArrayHighBound(Result, 1) do
                if DeVarSameValue(Value, VarArrayGet(Result, [Index])) then
                  Exit;
              // Если не дубль, то добавляем значение в массив ...
              Index := Succ(VarArrayHighBound(Result, 1));
              VarArrayRedim(Result, Index);
            end;
          VarArrayPut(Result, Value, [Index]);
        end;
    end;
  begin
    if not (VarIsClear(Value) or VarIsNull(Value)) then
      begin
        // Проверяем на исключаемое значение ...
        for Index := Succ(Low(aArguments)) to High(aArguments) do
          if IsExcluded(aArguments[Index]) then Exit;
        // Если не исключаемое значение, то добавляем значение в массив ...
        Add(Value);
      end;
  end;
begin
  Result := Unassigned;
  if (Length(aArguments) <> 0) and VarIsArray(aArguments[0]) then
    for Index := VarArrayLowBound(aArguments[0], 1) to VarArrayHighBound(aArguments[0], 1) do
      Append(VarArrayGet(aArguments[0], [Index]));
end;

function PermissionCheckSyntax(Sender: TObject; const aArgs: TArrayArguments; var aResultType: TFieldType): Integer;
begin
  Result := 0;
  if TPostfixItem(aArgs[0]).ResultType in (StringTypes + IntegerTypes) then
    aResultType := ftBoolean
  else
    Result := 1;
end;

function PermissionExecute(Sender: TObject; const Arguments: array of Variant): Variant;
var
  TableMeta: TTableMeta;
  TableName: string;
  TableGUID: TGUID;
begin
  if Assigned(SecuritySystem) then
    if VarIsStr(Arguments[0]) then
      begin
        TableName := Arguments[0];
        if TryStringToGUID(TableName, TableGUID) then
          TableMeta := MetaData.GetTableMeta(TableGUID)
        else
          TableMeta := MetaData.GetTableByName(TableName);
        if Assigned(TableMeta) then
          Result := SecuritySystem.CheckPolicy(TableMeta.ID, Arguments[1], Arguments[2])
        else
          Result := True; // По умолчанию всё разрешено!
      end
    else
      Result := SecuritySystem.CheckPolicy(Arguments[0], Arguments[1], Arguments[2])
  else
    Result := True; // По умолчанию всё разрешено!
end;

function UpdateCheckSyntax(Sender: TObject; const aArgs: TArrayArguments; var aResultType: TFieldType): Integer;
begin
  Result := 0;
  if TPostfixItem(aArgs[0]).ResultType in (StringTypes + IntegerTypes) then
    aResultType := ftBoolean
  else
    Result := 1;
end;

function UpdateExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  TableMeta: TTableMeta;
  TableName: string;
  TableGUID: TGUID;
begin
  Result := False;
  if Assigned(MetaData) then
    begin
      if VarIsStr(aArguments[0]) then
        begin
          TableName := aArguments[0];
          if TryStringToGUID(TableName, TableGUID) then
            begin
              Result:= MetaData.UpdateDataSets(TableGUID, mcUpdate, null) > 0;
              MetaData.UpdateLibrary(TableGUID);
            end
          else
            begin
              Result:= MetaData.UpdateDataSets(TableName, mcUpdate, null) > 0;
              MetaData.UpdateLibrary(TableName);
            end;
        end
      else
        if VarType(aArguments[0]) in IntVarTypes then
          begin
            TableMeta := MetaData.GetTableMeta(aArguments[0]);
            if Assigned(TableMeta) then
              begin
                Result:= MetaData.UpdateDataSets(TableMeta.ID, mcUpdate, null) > 0;
                MetaData.UpdateLibrary(TableMeta.ID);
              end;
          end;
    end;
end;

function UpdateCheckExSyntax(Sender: TObject; const aArgs: TArrayArguments; var aResultType: TFieldType): Integer;
begin
  Result := 0;
  if TPostfixItem(aArgs[0]).ResultType in StringTypes then
    aResultType := ftBoolean
  else
    if TPostfixItem(aArgs[0]).ResultType in IntegerTypes then
      aResultType :=  ftBoolean
    else
      Result := 1;
  if (Result = 0) and not (TPostfixItem(aArgs[1]).ResultType in IntegerTypes) then
    Result :=  2;
end;

function UpdateExExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  TableMeta: TTableMeta;
  TableName: string;
  TableGUID: TGUID;
begin
  Result := False;
  if Assigned(MetaData) then
    begin
      if VarIsStr(aArguments[0]) then
        begin
          TableName := aArguments[0];
          if TryStringToGUID(TableName, TableGUID) then
            begin
              Result:= MetaData.UpdateDataSets(TableGUID, mcUpdate, aArguments[1]) > 0;
              MetaData.UpdateLibrary(TableGUID);
            end
          else
            begin
              Result:= MetaData.UpdateDataSets(TableName, mcUpdate, aArguments[1]) > 0;
              MetaData.UpdateLibrary(TableName);
            end;
        end
      else
        if VarType(aArguments[0]) in IntVarTypes then
          begin
            TableMeta := MetaData.GetTableMeta(aArguments[0]);
            if Assigned(TableMeta) then
              begin
                Result:= MetaData.UpdateDataSets(TableMeta.ID, mcUpdate, aArguments[1]);
                MetaData.UpdateLibrary(TableMeta.ID);
              end;
          end;
    end;
end;

function OpenCheckSyntax(Sender: TObject; const aArgs: TArrayArguments; var aResultType: TFieldType): Integer;
begin
  Result := 0;
  if TPostfixItem(aArgs[0]).ResultType in (StringTypes + IntegerTypes) then
    aResultType := ftBoolean
  else
    Result := 1;

  if (Result = 0) and not (TPostfixItem(aArgs[1]).ResultType in IntegerTypes) then
    Result := 2;
end;

function OpenExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  TableName: string;
  TableGUID: TGUID;
begin
  Result := False;
  if Assigned(MetaData) and Assigned(MetaData.TablesList) then
    begin
      if VarIsStr(aArguments[0]) then
        begin
          TableName := aArguments[0];
          if TryStringToGUID(TableName, TableGUID) then
            Result := EditRecord(MetaData.TablesList.FindByGUID(TableGUID), aArguments[1])
          else
            Result := EditRecord(MetaData.TablesList.FindByTable(TableName), aArguments[1]);
        end
      else
        begin
          if VarType(aArguments[0]) in IntVarTypes then
          Result := EditRecord(MetaData.GetTableMeta(aArguments[0]), aArguments[1]);
        end;
    end;
end;

function FocusCheckSyntax(Sender: TObject; const aArgs: TArrayArguments; var aResultType: TFieldType): Integer;
begin
  Result := 0;
  if TPostfixItem(aArgs[0]).ResultType in (StringTypes + IntegerTypes) then
    aResultType := ftBoolean
  else
    Result := 1;

  if (Result = 0) and not (TPostfixItem(aArgs[1]).ResultType in IntegerTypes) then
    Result := 2;
end;

function FocusExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  TableName: string;
  TableGUID: TGUID;
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  CacheItem: TCacheItem;
begin
  Result := False;
  if Assigned(MetaData) and Assigned(MetaData.TablesList) then
    begin
      if VarIsStr(aArguments[0]) then
        begin
          TableName := aArguments[0];
          if TryStringToGUID(TableName, TableGUID) then
            TableMeta := MetaData.TablesList.FindByGUID(TableGUID)
          else
            TableMeta := MetaData.TablesList.FindByTable(TableName);
        end
      else
        if VarType(aArguments[0]) in IntVarTypes then
          TableMeta:= MetaData.TablesList.FindByID(aArguments[0])
        else
          TableMeta := nil;
      if Assigned(TableMeta) then
        begin
          DataCache := MetaData.FindActiveDataCache(TableMeta.ID, True);
          if Assigned(DataCache) and (DataCache.Count <> 0) then
            begin
              CacheItem := DataCache.FindByID(aArguments[1]);
              if Assigned(CacheItem) then
                begin
                  DataCache.ClearSelection;
                  DataCache.FocusedItem := CacheItem;
                  Result := True;
                end;
            end;
        end;
    end;
end;

function InGroupCheckSyntax(Sender: TObject; const aArgs: TArrayArguments; var aResultType: TFieldType): Integer;
begin
  case Length(aArgs) of
    0: { Не указано параметров }
      Result := 1;
    1, 2: { Указано допустимое количетсво параметров }
      begin
        Result := 0;
        if TPostfixItem(aArgs[0]).ResultType in (StringTypes + IntegerTypes) then
          aResultType := ftBoolean
        else
          Result := 1;

        if (Result = 0) and (Length(aArgs) = 2) then
          if TPostfixItem(aArgs[1]).ResultType in (StringTypes + IntegerTypes + [ftUnknown]) then
            aResultType := ftBoolean
          else
            Result := 2;
      end;
  else
    Result := 3; // Другое количество параметров!
  end;
end;

function InGroupExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  CheckSession, GroupSession: TUserSession;
  UserList: TUserList;
  Index: Integer;
begin
  Result := False;
  CheckSession := TUserSession.Create;
  try
    if Length(aArguments) = 2 then
      if VarIsNull(aArguments[1]) or VarIsEmpty(aArguments[1]) then
        if Assigned(UserSession) then
          CheckSession.ID := UserSession.ID
        else
          FreeAndNil(CheckSession)
      else if VarIsNumeric(aArguments[1]) then
        CheckSession.ID := aArguments[1]
      else if VarIsStr(aArguments[1]) then
        begin
          UserList := TUserList.Create;
          try
            UserList.LoadFromDatabase;
            Index := UserList.IndexByLogin(aArguments[1]);
            if Index <> -1 then
              CheckSession.ID := UserList.Items[Index].ID
            else
              FreeAndNil(CheckSession);
          finally
            UserList.Free;
          end;
        end
      else
        FreeAndNil(CheckSession)
    else
      if Assigned(UserSession) then
        CheckSession.ID := UserSession.ID
      else
        FreeAndNil(CheckSession);
    // Если пользователь определён, то ...
    if Assigned(CheckSession) then
      begin
        CheckSession.LoadGroup;
        if VarIsNumeric(aArguments[0]) then
          GroupSession := CheckSession.Groups.FindByID(aArguments[0])
        else
          GroupSession := CheckSession.Groups.FindUser(aArguments[0]);
        Result := Assigned(GroupSession);
      end;
  finally
    CheckSession.Free;
  end;
end;

function TranslateExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  case VarToInt(aArguments[1]) of
    0: Result := GetTitle(VarToStr(aArguments[0]), ttFull);
    1: Result := GetTitle(VarToStr(aArguments[0]), ttFirstName);
    2: Result := GetTitle(VarToStr(aArguments[0]), ttSecondName);
  else
    Result := Unassigned;
  end;
end;

function WrapStringExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := WrapString(VarToStr(aArguments[0]), [VarToInt(aArguments[2]), VarToInt(aArguments[3])], VarToStr(aArguments[1]));
end;

function UnitsExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := Units(VarToInt(aArguments[0]), VarToStr(aArguments[1]), VarToStr(aArguments[2]), VarToStr(aArguments[3]) );
end;

function QuotedStringExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  Value, Symbols: string;
begin
  Value := VarToStr(aArguments[0]);
  Symbols := VarToStr(aArguments[1]);
  if Length(Symbols) = 0 then Symbols := '''';
  Result := Value.QuotedString(Symbols[1]);
end;

function DeQuotedStringExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  Value, Symbols: string;
begin
  Value := VarToStr(aArguments[0]);
  Symbols := VarToStr(aArguments[1]);
  if Length(Symbols) = 0 then Symbols := '''';
  Result := Value.DeQuotedString(Symbols[1]);
end;

function IncludeTrailingBackslashExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  Value: string;
begin
  Value := VarToStr(aArguments[0]);
  if Length(Value) <> 0 then Value := IncludeTrailingBackslash(Value);
  Result := Value;
end;

function ExcludeTrailingBackslashExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := ExcludeTrailingBackslash(VarToStr(aArguments[0]));
end;

function FileExistsExecute(Sender: TObject; const aArguments: array of Variant): Variant;
const
  faExculde = faDirectory or faVolumeID;
var
  FileName: string;
  ErrorCode: Integer;
  SearchRec: TSearchRec;
begin
  FileName := VarToStr(aArguments[0]);
  if (Pos('*', FileName) <> 0) or (Pos('?', FileName) <> 0) then
    begin
      Result := False;
      ErrorCode := FindFirst(FileName, faAnyFile, SearchRec);
      if ErrorCode = 0 then
        try
          while ErrorCode = 0 do
            begin
              if (SearchRec.Attr and faExculde) = 0 then
                begin
                  Result := True;
                  Break;
                end;
              ErrorCode := FindNext(SearchRec);
            end;
        finally
          FindClose(SearchRec);
        end;
    end
  else
    Result := FileExists(FileName);
end;

function DirectoryExistsExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  FileName: string;
  ErrorCode: Integer;
  SearchRec: TSearchRec;
begin
  FileName := VarToStr(aArguments[0]);
  if (Pos('*', FileName) <> 0) or (Pos('?', FileName) <> 0) then
    begin
      Result := False;
      ErrorCode := FindFirst(FileName, faAnyFile, SearchRec);
      if ErrorCode = 0 then
        try
          while ErrorCode = 0 do
            begin
              if ((SearchRec.Attr and faDirectory) <> 0) and (Length(SearchRec.Name) <> 0) and (SearchRec.Name[1] <> '.') then
                begin
                  Result := True;
                  Break;
                end;
              ErrorCode := FindNext(SearchRec);
            end;
        finally
          FindClose(SearchRec);
        end;
    end
  else
    Result := SysUtils.DirectoryExists(FileName);
end;

function ExtractFileDriveExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := ExcludeTrailingBackslash(ExtractFileDrive(VarToStr(aArguments[0])));
end;

function ExtractFilePathExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  Value: string;
begin
  Value := ExtractFilePath(VarToStr(aArguments[0]));
  if Length(Value) <> 0 then Value := IncludeTrailingBackslash(Value);
  Result := Value;
end;

function ExtractFileDirExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  Directory, Value: string;
  Index: Integer;
begin
  Directory := ExtractFilePath(VarToStr(aArguments[0]));
  Value := ExcludeTrailingBackslash(ExtractFileDrive(Directory));
  if Length(Value) <> 0 then
    begin
      Index := Pos(UpperCase(Value), UpperCase(Directory));
      if Index = 1 then Delete(Directory, 1, Length(Value));
    end;
  if Length(Directory) = 0 then
    Result := '\'
  else
    Result := IncludeTrailingBackslash(Directory);
end;

function ExtractFileNameExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := ChangeFileExt(ExtractFileName(VarToStr(aArguments[0])), EmptyStr);
end;

function ExtractFileExtExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := ExtractFileExt(VarToStr(aArguments[0]));
end;

{
function ExpandFileNameExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  Value, Directory, FileName: string;
  DateTime: TDateTime;
  Index: Integer;
begin
  DateTime := Now;
  Value := VarToStr(aArguments[0]);                                        // 08.04.2017 09:05:00.010
  Value := ReplaceText(Value, '%YYYY%', FormatDateTime('YYYY', DateTime)); //   2017
  Value := ReplaceText(Value, '%YY%', FormatDateTime('YY', DateTime));     //     17
  Value := ReplaceText(Value, '%MMMM%', FormatDateTime('MMMM', DateTime)); //  April
  Value := ReplaceText(Value, '%MMM%', FormatDateTime('MMM', DateTime));   //    Apr
  Value := ReplaceText(Value, '%MM%', FormatDateTime('MM', DateTime));     //     04
  Value := ReplaceText(Value, '%M%', FormatDateTime('M', DateTime));       //      4
  Value := ReplaceText(Value, '%DDDD%', FormatDateTime('DDDD', DateTime)); // Friday
  Value := ReplaceText(Value, '%DDD%', FormatDateTime('DDD', DateTime));   //    Fri
  Value := ReplaceText(Value, '%DD%', FormatDateTime('DD', DateTime));     //     08
  Value := ReplaceText(Value, '%D%', FormatDateTime('D', DateTime));       //      8
  Value := ReplaceText(Value, '%HH%', FormatDateTime('HH', DateTime));     //     09
  Value := ReplaceText(Value, '%H%', FormatDateTime('H', DateTime));       //      9
  Value := ReplaceText(Value, '%NN%', FormatDateTime('NN', DateTime));     //     05
  Value := ReplaceText(Value, '%N%', FormatDateTime('N', DateTime));       //      5
  Value := ReplaceText(Value, '%SS%', FormatDateTime('SS', DateTime));     //     00
  Value := ReplaceText(Value, '%S%', FormatDateTime('S', DateTime));       //      0
  Value := ReplaceText(Value, '%ZZZ%', FormatDateTime('ZZZ', DateTime));   //    010
  Value := ReplaceText(Value, '%Z%', FormatDateTime('Z', DateTime));       //     10
  if Pos('%SEQ%', UpperCase(ExtractFilePath(Value))) <> 0 then
    begin
      Index := 1;
      repeat
        Directory := ExtractFilePath(Value);
        if Length(Directory) <> 0 then Directory := IncludeTrailingBackslash(Directory);
        Directory := ReplaceText(Directory, '%SEQ%', Format('%u', [Index]));
        Inc(Index);
      until not DirectoryExists(Directory);
      Value := Directory + ExtractFileName(Value);
    end;
  if Pos('%SEQ%', UpperCase(ExtractFileName(Value))) <> 0 then
    begin
      Directory := ExtractFilePath(Value);
      if Length(Directory) <> 0 then Directory := IncludeTrailingBackslash(Directory);
      Index := 1;
      repeat
        FileName := ReplaceText(ExtractFileName(Value), '%SEQ%', Format('%u', [Index]));
        Inc(Index);
      until not FileExists(Directory + FileName);
      Value := Directory + FileName;
    end;
  Result := Value;
end;
}

function ChangeFilePathExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := ChangeFilePath(VarToStr(aArguments[0]), VarToStr(aArguments[1]));
end;

function ChangeFileNameExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  FileName: string;
begin
  FileName := ExtractFilePath(VarToStr(aArguments[0]));
  if Length(FileName) <> 0 then FileName := IncludeTrailingBackslash(FileName);
  FileName := FileName + VarToStr(aArguments[1]) + ExtractFileExt(VarToStr(aArguments[0]));
  Result := FileName;
end;

function ChangeFileExtExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := ChangeFileExt(VarToStr(aArguments[0]), VarToStr(aArguments[1]));
end;

function CreateDirExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := ForceDirectories(VarToStr(aArguments[0]));
end;

{$IFDEF DANGEROUSFUNCTIONS}

function RemoveDirExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := RemoveDir(VarToStr(aArguments[0]));
end;

function DeleteFileExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := DeleteFile(VarToStr(aArguments[0]));
end;

function RenameFileExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := RenameFile(VarToStr(aArguments[0]), VarToStr(aArguments[1]));
end;

{$ENDIF}

function NormalizeFilePathExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  Directory, Value: string;
  Index: Integer;
begin
  Value := ExtractFilePath(VarToStr(aArguments[0]));
  Directory := ExtractFileDrive(Value);
  if Pos(Directory, Value) = 1 then
    begin
      Delete(Value, 1, Length(Directory));
      if (Length(Value) <> 0) and (Value[1] = '\') then Delete(Value, 1, 1);
    end;
  Value := ExcludeTrailingBackslash(Value);
  if Length(Directory) <> 0 then Directory := IncludeTrailingBackslash(Directory);
  while Length(Value) <> 0 do
    begin
      Index := Pos('\', Value);
      if Index = 0 then
        begin
          Directory := IncludeTrailingBackslash(Directory + NormalizeFileName(Value));
          Value := EmptyStr;
        end
      else
        begin
          Directory := IncludeTrailingBackslash(Directory + NormalizeFileName(Copy(Value, 1, Pred(Index))));
          Value := Copy(Value, Succ(Index), Length(Value));
        end;
    end;
  Result := Directory + NormalizeFileName(ExtractFileName(VarToStr(aArguments[0])));
end;

function NormalizeFileNameExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := NormalizeFileName(VarToStr(aArguments[0]));
end;

Function FileToAttributes(const aValue: Variant; var aFileName: AnsiString; var aFileSize: Integer;
   var aFileDate: TDateTime; var aFileVersion: Cardinal; var aFileAuthor: AnsiString; var aFileBody: Variant): Boolean;
var
  P: Pointer;
  Buf: AnsiString;
  BlockSize, FileOffset: Integer;
  sLength: Integer;
  FileVersion: Word;
  FFileData: array of Byte;
begin
  // copy from unit Funcs;
  // procedure TDeImage.SetValue(const aValue: Variant);

  aFileName:= EmptyStr;
  aFileSize:= unassigned;
  aFileDate:= unassigned;
  aFileVersion:= unassigned;
  aFileAuthor:= EmptyStr;
  aFileBody:= unassigned;
  SetLength(FFileData, 0);
  Result:= VarIsArray(aValue);

  if not Result then Exit;

  BlockSize := VarArrayHighBound(aValue, 1) - VarArrayLowBound(aValue, 1);
  SetLength(Buf, BlockSize);
  P := VarArrayLock(aValue);
  try
    Move(P^, Buf[1], BlockSize);
  finally
    VarArrayUnlock(aValue);
  end;

  if Copy(Buf, 1, DeSLength) = DeSignature then
    begin
      sLength:= PSmallint(@Buf[DeSLength + 1])^;
      SetLength(aFileName, sLength);
      Move(Buf[DeSLength + SizeOf(Smallint) + 1], aFileName[1], sLength);             // aFileName
      aFileSize:= Length(Buf) - (DeSLength + SizeOf(Smallint) + sLength);             // aFileSize
      SetLength(FFileData, aFileSize);
      Move(Buf[DeSLength + SizeOf(Smallint) + sLength + 1], FFileData[0], aFileSize);
      aFileBody:= FFileData;                                                          // aFileBody
    end else

  if Copy(Buf, 1, DeSLength) = DeSignature2 then
    begin
      FileOffset := Succ(DeSLength);
      FileVersion := PWord(@Buf[FileOffset])^;
      FileOffset := FileOffset + SizeOf(FileVersion);
      Move(Buf[FileOffset], aFileVersion, SizeOf(Cardinal));
      FileOffset := FileOffset + SizeOf(Cardinal);
      Move(Buf[FileOffset], aFileDate, SizeOf(TDateTime));    // aFileDate
      FileOffset := FileOffset + SizeOf(TDateTime);
      sLength := PSmallint(@Buf[FileOffset])^;
      FileOffset := FileOffset + SizeOf(Smallint);
      SetLength(aFileName, sLength);
      Move(Buf[FileOffset], aFileName[1], sLength);           // aFileName
      FileOffset := FileOffset + sLength;
      sLength := PSmallint(@Buf[FileOffset])^;
      FileOffset := FileOffset + SizeOf(Smallint);
      SetLength(aFileAuthor, sLength);
      Move(Buf[FileOffset], aFileAuthor[1], sLength);         //aFileAuthor
      FileOffset := FileOffset + sLength;
      aFileSize := PInteger(@Buf[FileOffset])^;               // aFileSize
    end else

    begin
      aFileName:= EmptyStr;                                   // aFileName
      aFileSize:= BlockSize;                                  // aFileSize
      aFileBody:= aValue;                                     // aFileBody
    end;
end;

function FileToNameExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  aName, aAuthor: AnsiString;
  aSize: Integer;
  aDate: TDateTime;
  aVersion: Cardinal;
  aBody: Variant;
begin
  if FileToAttributes(aArguments[0], aName, aSize, aDate, aVersion, aAuthor, aBody)
    then Result:= aName
    else Result:= EmptyStr;
end;

function FileToSizeExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  aName, aAuthor: AnsiString;
  aSize: Integer;
  aDate: TDateTime;
  aVersion: Cardinal;
  aBody: Variant;
begin
  if FileToAttributes(aArguments[0], aName, aSize, aDate, aVersion, aAuthor, aBody)
    then Result:= aSize
    else Result:= Unassigned;
end;

function FileToDateExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  aName, aAuthor: AnsiString;
  aSize: Integer;
  aDate: TDateTime;
  aVersion: Cardinal;
  aBody: Variant;
begin
  if FileToAttributes(aArguments[0], aName, aSize, aDate, aVersion, aAuthor, aBody)
    then Result:= aDate
    else Result:= Unassigned;
end;

function FileToBodyExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  aName, aAuthor: AnsiString;
  aSize: Integer;
  aDate: TDateTime;
  aVersion: Cardinal;
  aBody: Variant;
begin
  if FileToAttributes(aArguments[0], aName, aSize, aDate, aVersion, aAuthor, aBody)
    then Result:= aBody
    else Result:= Unassigned;
end;

function LatitudeExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var aLatitide, aLongitude: Double;
begin
  if DM.MyCoordinate(aLatitide, aLongitude) then Result:= aLatitide
                                            else Result:= unassigned;
end;

function LongitudeExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var aLatitide, aLongitude: Double;
begin
  if DM.MyCoordinate(aLatitide, aLongitude) then Result:= aLongitude
                                            else Result:= unassigned;
end;

function NewGUIDExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := GUIDToString(NewGUID);
end;

function IsGUIDExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := IsGUID(aArguments[0]);
end;

function RGBExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := RGB(aArguments[0], aArguments[1], aArguments[2]);
end;

function HLSExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  R, G, B: Integer;
begin
  HLStoRGB(aArguments[0], aArguments[1], aArguments[2], R, G, B);
  Result := RGB(R, G, B);
end;

function CMYKExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := CMYK(aArguments[0], aArguments[1], aArguments[2], aArguments[3]);
end;

function SleepExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  Value: Integer;
begin
  try
    Value := VarToInt(aArguments[0]);
    Result := Value > 0;
    if Result then Sleep(Value);
  except
    Result := False;
  end;
end;

function InfoExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoInfo, NativeInt(VarToStr(aArguments[0])) );
  Result:=True;
end;

function ErrorExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError, NativeInt(VarToStr(aArguments[0])) );
  Result:=True;
end;

function ValueFromPathXMLExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  FunctionDescr: TFunctionPathXmlDescr;
  Component: TComponent;
  Document: TXMLDocument;
  XML, Path: string;
begin
  try
    XML := VarToStr(aArguments[0]);
    if Length(XML) = 0 then
      Result := Null
    else
      begin
        Path := VarToStr(aArguments[1]);
        // Если используется специальный дескриптор для кэширования парсинга XML, то ...
        if Assigned(Sender) and (Sender is TFunctionPathXmlDescr) then
          begin
            FunctionDescr := Sender as TFunctionPathXmlDescr;
            FunctionDescr.LoadXML(XML);
            Result := ValueFromPathXML(FunctionDescr.RootNode, Path);
          end
        else
          begin
            Component := TComponent.Create(nil);
            try
              Document := TXMLDocument.Create(Component);
              Document.XML.Text := XML;
              Document.Active := True;
              Result := ValueFromPathXML(Document.Node, Path);
            finally
              Component.Free;
            end;
          end;
      end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('DeFunctions.ValueFromPathXML skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function BodyFromPathXMLExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  FunctionDescr: TFunctionPathXmlDescr;
  Component: TComponent;
  Document: TXMLDocument;
  XML, Path: string;
begin
  try
    XML := VarToStr(aArguments[0]);
    if Length(XML) = 0 then
      Result := Null
    else
      begin
        Path := VarToStr(aArguments[1]);
        // Если используется специальный дескриптор для кэширования парсинга XML, то ...
        if Assigned(Sender) and (Sender is TFunctionPathXmlDescr) then
          begin
            FunctionDescr := Sender as TFunctionPathXmlDescr;
            FunctionDescr.LoadXML(XML);
            Result := BodyFromPathXML(FunctionDescr.RootNode, Path);
          end
        else
          begin
            Component := TComponent.Create(nil);
            try
              Document := TXMLDocument.Create(Component);
              Document.XML.Text := XML;
              Document.Active := True;
              Result := ValueFromPathXML(Document.Node, Path);
            finally
              Component.Free;
            end;
          end;
      end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('DeFunctions.ValueFromPathXML skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function XMLFromPathXMLExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  FunctionDescr: TFunctionPathXmlDescr;
  Component: TComponent;
  Document: TXMLDocument;
  XML, Path: string;
  P: Integer;
begin
  try
    XML := VarToStr(aArguments[0]);
    While Copy(XML,1,2)='<?' do
      begin
        P:=Pos('?>', XML);
        if 0<P then Delete(XML, 1, P + 1)
               else Break;
        while (0 < Length(XML)) and ((XML[1] = '') or (XML[1] =#9) or (XML[1] = #10) or (XML[1] =#13)) do
          Delete(XML, 1, 1);
      end;

    if Length(XML) = 0 then
      Result := Null
    else
      begin
        Path := VarToStr(aArguments[1]);
        // Если используется специальный дескриптор для кэширования парсинга XML, то ...
        if Assigned(Sender) and (Sender is TFunctionPathXmlDescr) then
          begin
            FunctionDescr := Sender as TFunctionPathXmlDescr;
            FunctionDescr.LoadXML(XML);
            Result := XMLFromPathXML(FunctionDescr.RootNode, Path);
          end
        else
          begin
            Component := TComponent.Create(nil);
            try
              Document := TXMLDocument.Create(Component);
              Document.XML.Text := XML;
              Document.Active := True;
              Result := XMLFromPathXML(Document.Node, Path);
            finally
              Component.Free;
            end;
          end;
      end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('DeFunctions.XMLFromPathXML skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function ValueToPathXMLExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  FunctionDescr: TFunctionPathXmlDescr;
  Document: TXMLDocument;
  XML, Path: string;
begin
  try
    XML := VarToStr(aArguments[0]);
    if Length(XML) = 0 then
      Result := Null
    else
      begin
        Path := VarToStr(aArguments[1]);
        // Если используется специальный дескриптор для кэширования парсинга XML, то ...
        if Assigned(Sender) and (Sender is TFunctionPathXmlDescr) then
          begin
            FunctionDescr := Sender as TFunctionPathXmlDescr;
            if Assigned(FunctionDescr.FDocument) then
              begin
                if not SameText(FunctionDescr.FDocument.XML.Text, XML) then
                  begin
                    FunctionDescr.FDocument.Active := False;
                    FunctionDescr.FDocument.XML.Text := XML;
                    FunctionDescr.FDocument.Active := True;
                  end;
              end
            else
              begin
                FunctionDescr.FDocument := TXMLDocument.Create(nil);
                FunctionDescr.FDocument.XML.Text := XML;
                FunctionDescr.FDocument.Active := True;
              end;
            if ValueToPathXML(FunctionDescr.FDocument.DocumentElement, Path, aArguments[2]) then
              Result := FunctionDescr.FDocument.XML.Text
            else
              Result := XML;
          end
        else
          begin
            Document := TXMLDocument.Create(nil);
            try
              Document.XML.Text := XML;
              Document.Active := True;
              if ValueToPathXML(Document.DocumentElement, Path, aArguments[2]) then
                Result := Document.XML.Text
              else
                Result := XML;
            finally
              Document.Free;
            end;
          end;
      end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('DeFunctions.ValueToPathXML skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function ValueFromPathJSONExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var JSON, Path: string;
    i: Integer;
    JData: TJSONValue;
    JItems: TJSONArray;
begin
  JSON:= Trim(aArguments[0]);
  Path:= Trim(aArguments[1]);

  // WriteLog(Json,'json.txt');
  try
    JData:= TJSONObject.ParseJSONValue(JSON);
  except
    JData:= nil;
  end;

  if not Assigned(JData) then Exit;

  if Length(Path)=0 then
    begin
      if JDATA is TJSONArray then
      begin
        JItems:= TJSONArray(JData);
        case JItems.count of
          0: Result:= EmptyStr;
          1: Result:= JItems.items[0].ToString;
          else begin
                 Result:= VarArrayCreate([0, JItems.count-1], varVariant);
                 for i:=0 to Pred(JItems.count) do
                   Result[i]:= JItems.items[i].ToString;
               end;
        end;
      end
    end
  else
    try
      if JDATA is TJSONObject then
        if TJSONObject(JDATA).Values[Path].Null
          then Result:= null
          else Result:= TJSONObject(JDATA).Values[Path].Value;
    finally

    end;
end;

function EncodeURLExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := TURLEncoding.URL.Encode(VarToStr(aArguments[0]));
end;

function DecodeURLExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := TURLEncoding.URL.Decode(VarToStr(aArguments[0]));
end;

function EncodeHTMLExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := THTMLEncoding.HTML.Encode(VarToStr(aArguments[0]));
end;

function DecodeHTMLExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := THTMLEncoding.HTML.Decode(VarToStr(aArguments[0]));
end;

function EncodeBase64Execute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := TBase64Encoding.Base64.Encode(VarToStr(aArguments[0]));
end;

function EncodeBase64BlobExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  Value: Variant;
  ValuePtr: Pointer;
  Buffer: array of Byte;
  BufferSize: Integer;
begin
  Value := aArguments[0];
  if VarIsArray(Value) then
    begin
      BufferSize := VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1);
      SetLength(Buffer, BufferSize);
      ValuePtr := VarArrayLock(Value);
      try
        Move(ValuePtr^, Buffer[0], BufferSize);
      finally
        VarArrayUnlock(Value);
      end;
      Result := TBase64Encoding.Base64.Encode(Buffer);
    end
  else
    Result := Null;
end;

function DecodeBase64Execute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := TBase64Encoding.Base64.Decode(VarToStr(aArguments[0]));
end;

function DecodeUnicodeExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var s: AnsiString;
begin
  s:= aArguments[0];
  Result := UTF8ToString(s);
//  Result := TEncoding.UTF8.GetString(TBytes(VarToStr(aArguments[0])));
end;

{$IFDEF INDYUSED}
function GetHTTPExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  Strings: TStrings;
begin
  try
    Strings := TStringList.Create;
    try
      Strings.Text := VarToStr(aArguments[1]);
      Result := (Sender as TFunctionIndyDescr).Get(VarToStr(aArguments[0]), Strings);
    finally
      Strings.Free;
    end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('GetHTTPExecute skip error: ' + E.Message);
        {$ENDIF}
        SendMessage(Application.MainForm.Handle, DM_INFONOTIFY, icoError,
                    NativeInt(PChar('GetHTTPExecute error: ' + E.Message)));
        Result := Null;
      end;
  end;
end;

function ContactExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  try
    Result := TMailManager.Contact(aArguments[0]);
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('ContactExecute skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function SendMailAllExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  try
    Result := TMailManager.SendMail(Null);
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('SendMailAllExecute skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function SendMailExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  try
    Result := TMailManager.SendMail(aArguments[0]);
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('SendMailExecute skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function ReceiveMailAllExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  try
    Result := TMailManager.ReceiveMail(Null, False);
    // Закомментировал, т.к. ПАРСЕР НЕ РАБОТАЕТ!!!
    //Result := TMailManager.ReceiveMail(Null, aArguments[0]);
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('ReceiveMailAllExecute skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function ReceiveMailExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  try
    Result := TMailManager.ReceiveMail(aArguments[0], aArguments[1]);
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('ReceiveMailExecute skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function TGBotExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  try
    Result := (Sender as TFunctionTelegramDescr).TelegramManager.Bot(aArguments[0]);
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TGBotExecute skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function TGContactExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  try
    Result := (Sender as TFunctionTelegramDescr).TelegramManager.Contact(aArguments[0]);
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TGContactExecute skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function TGChatExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  try
    // Нельзя создавать чаты, можно только возвращать созданные ранее!!!
    Result := (Sender as TFunctionTelegramDescr).TelegramManager.Chat(aArguments[0], aArguments[1], True);
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TGChatExecute skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function TGReadMessagesExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  try
    Result := (Sender as TFunctionTelegramDescr).TelegramManager.ReadMessage(aArguments[0]);
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TGReadMessagesExecute skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function TGSendMessageExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := Unassigned;
  try
    case Length(aArguments) of
      2: { ChatID, Text }
        Result := (Sender as TFunctionTelegramDescr).TelegramManager.SendMessage(aArguments[0], aArguments[1]);
      3: { ChatID, Text, TextFormat / ChatID, Text, ReplyToMessageID }
        if VarIsStr(aArguments[2]) then
          Result := (Sender as TFunctionTelegramDescr).TelegramManager.SendMessage(aArguments[0], aArguments[1], aArguments[2])
        else
          Result := (Sender as TFunctionTelegramDescr).TelegramManager.SendMessage(aArguments[0], aArguments[1], EmptyStr, aArguments[2]);
      4: { ChatID, Text, TextFormat, ReplyToMessageID }
        Result := (Sender as TFunctionTelegramDescr).TelegramManager.SendMessage(aArguments[0], aArguments[1], aArguments[2], aArguments[3]);
    else
      {$IFDEF DEBUG}
      DebugLog('TGSendMessageExecute: Error %d arguments count!', [Length(aArguments)]);
      {$ENDIF}
    end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TGSendMessageExecute skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function TGSendPhotoExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := Unassigned;
  try
    case Length(aArguments) of
      2: { ChatID, FileName }
        Result := (Sender as TFunctionTelegramDescr).TelegramManager.SendPhoto(aArguments[0], aArguments[1]);
      3: { ChatID, FileName, Caption / ChatID, FileName, ReplyToMessageID }
        if VarIsStr(aArguments[2]) then
          Result := (Sender as TFunctionTelegramDescr).TelegramManager.SendPhoto(aArguments[0], aArguments[1], aArguments[2])
        else
          Result := (Sender as TFunctionTelegramDescr).TelegramManager.SendPhoto(aArguments[0], aArguments[1], EmptyStr, EmptyStr, aArguments[2]);
      4: { ChatID, FileName, Caption, CaptionFormat}
        Result := (Sender as TFunctionTelegramDescr).TelegramManager.SendPhoto(aArguments[0], aArguments[1], aArguments[2], aArguments[3]);
      5: { ChatID, FileName, Caption, CaptionFormat, ReplyToMessageID }
        Result := (Sender as TFunctionTelegramDescr).TelegramManager.SendPhoto(aArguments[0], aArguments[1], aArguments[2], aArguments[3], aArguments[4]);
    else
      {$IFDEF DEBUG}
      DebugLog('TGSendPhotoExecute: Error %d arguments count!', [Length(aArguments)]);
      {$ENDIF}
    end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TGSendPhotoExecute skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function TGSendDocumentExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  Result := Unassigned;
  try
    case Length(aArguments) of
      2: { ChatID, FileName }
        Result := (Sender as TFunctionTelegramDescr).TelegramManager.SendDocument(aArguments[0], aArguments[1]);
      3: { ChatID, FileName, Caption / ChatID, FileName, ReplyToMessageID }
        if VarIsStr(aArguments[2]) then
          Result := (Sender as TFunctionTelegramDescr).TelegramManager.SendDocument(aArguments[0], aArguments[1], aArguments[2])
        else
          Result := (Sender as TFunctionTelegramDescr).TelegramManager.SendDocument(aArguments[0], aArguments[1], EmptyStr, EmptyStr, aArguments[2]);
      4: { ChatID, FileName, Caption, CaptionFormat}
        Result := (Sender as TFunctionTelegramDescr).TelegramManager.SendDocument(aArguments[0], aArguments[1], aArguments[2], aArguments[3]);
      5: { ChatID, FileName, Caption, CaptionFormat, ReplyToMessageID }
        Result := (Sender as TFunctionTelegramDescr).TelegramManager.SendDocument(aArguments[0], aArguments[1], aArguments[2], aArguments[3], aArguments[4]);
    else
      {$IFDEF DEBUG}
      DebugLog('TGSendDocumentExecute: Error %d arguments count!', [Length(aArguments)]);
      {$ENDIF}
    end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TGSendDocumentExecute skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function TGGetFileExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  try
    Result := (Sender as TFunctionTelegramDescr).TelegramManager.LoadFile(aArguments[0], aArguments[1]);
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TGGetFileExecute skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function TGReadAllExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  try
    Result := (Sender as TFunctionTelegramDescr).TelegramManager.ReadAll;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TGReadAllExecute skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function TGSendAllExecute(Sender: TObject; const aArguments: array of Variant): Variant;
begin
  try
    Result := (Sender as TFunctionTelegramDescr).TelegramManager.SendAll;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TGSendAllExecute skip error: ' + E.Message);
        {$ENDIF}
        Result := Null;
      end;
  end;
end;
{$ENDIF}

function BlobExecute(Sender: TObject; const aArguments: array of Variant): Variant;
var
  Image: TDeImage;
begin
  Image := TDeImage.Create(nil);
  try
    Image.Value := aArguments[0];
    Result := Image.DataValue;
  finally
    Image.Free;
  end;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
var
  VersionMS, VersionLS: Cardinal;
begin
  {$IFDEF DEBUG}
  DebugLog('DeFunctions unit initialization ...');
  {$ENDIF}
  GetFileVersion(GetModuleName(hInstance), VersionMS, VersionLS, True);
  Version := HiWord(VersionMS) + ((LoWord(VersionMS) mod 100) / 100);
  FunctionList := TFunctionList.Create;
  FunctionList.RegisterFunction('iif', ftUnknown, [ftBoolean, ftUnknown, ftUnknown], IifCheckSyntax, IifExecute);
  FunctionList.RegisterFunction('ValueSelect', ftUnknown, [ftInteger, ftString, ftUnknown], nil, ValueSelectExecute, TRUE);
  FunctionList.RegisterFunction('ValueSelect', ftUnknown, [ftString, ftString, ftUnknown], nil, ValueSelectExecute, TRUE);
  FunctionList.RegisterFunction('Selected', ftUnknown, [ftString], nil, SelectedExecute);
  FunctionList.RegisterFunction('Focused', ftUnknown, [ftString], nil, FocusedExecute);
  FunctionList.RegisterFunction('Select', ftUnknown, [ftString], nil, SelectSExecute, TRUE);
  FunctionList.RegisterFunction('Select', ftUnknown, [ftString, ftString], SelectABCCheckSyntax, SelectABCExecute, TRUE);
  FunctionList.RegisterFunction('Select', ftUnknown, [ftString, ftString, ftString], SelectABCCheckSyntax, SelectABCExecute, TRUE);
  FunctionList.RegisterFunction('Version', ftFloat, [], nil, VersionExecute);
  FunctionList.RegisterFunction('Sleep', ftBoolean, [ftInteger], nil, SleepExecute, TRUE); // v. 18.3
  FunctionList.RegisterFunction('Info', ftBoolean, [ftString], nil, InfoExecute);
  FunctionList.RegisterFunction('Error', ftBoolean, [ftString], nil, ErrorExecute);

  FunctionList.RegisterFunction('Date', ftDateTime, [], nil, DateExecute);
  FunctionList.RegisterFunction('Time', ftDateTime, [], nil, TimeExecute);
  FunctionList.RegisterFunction('Today', ftDateTime, [], nil, TodayExecute);
  FunctionList.RegisterFunction('Tomorrow', ftDateTime, [], nil, TomorrowExecute); //  v.16.3
  FunctionList.RegisterFunction('Yesterday', ftDateTime, [], nil, YesterdayExecute); //  v.16.3
  FunctionList.RegisterFunction('Now', ftDateTime, [], nil, NowExecute);
  FunctionList.RegisterFunction('Now', ftDateTime, [ftBoolean], nil, NowExecute);  // v.21.12
  FunctionList.RegisterFunction('DateOf', ftDateTime, [ftDateTime], nil, DateOfExecute);
  FunctionList.RegisterFunction('TimeOf', ftDateTime, [ftDateTime], nil, TimeOfExecute);
  FunctionList.RegisterFunction('DaysInMonth', ftInteger, [ftDateTime], nil, DaysInMonthExecute); //  v.16.3
  FunctionList.RegisterFunction('DaysInYear', ftInteger, [ftDateTime], nil, DaysInYearExecute); //  v.16.3
  FunctionList.RegisterFunction('WeekOf', ftInteger, [ftDateTime], nil, WeekOfExecute); //  v.16.3
  FunctionList.RegisterFunction('DayOfWeek', ftInteger, [ftDateTime], nil, DayOfWeekExecute); //  v.16.3
  FunctionList.RegisterFunction('DayOfYear', ftInteger, [ftDateTime], nil, DayOfYearExecute); //  v.16.3
  FunctionList.RegisterFunction('DayOf', ftInteger, [ftDateTime], nil, DayOfExecute);
  FunctionList.RegisterFunction('MonthOf', ftInteger, [ftDateTime], nil, MonthOfExecute);
  FunctionList.RegisterFunction('YearOf', ftInteger, [ftDateTime], nil, YearOfExecute);
  FunctionList.RegisterFunction('EncodeDate', ftDateTime, [ftInteger, ftInteger, ftInteger], nil, EncodeDateExecute);
  FunctionList.RegisterFunction('FormatDateTime', ftString, [ftString, ftDateTime], nil, FormatDateTimeExecute);         // v.19.02
  FunctionList.RegisterFunction('DateTimeToStringODBC', ftString, [ftDateTime], nil, DateTimeToStringODBCExecute);       // v.19.02
  FunctionList.RegisterFunction('DateTimeToStringMSSQL', ftString, [ftDateTime], nil, DateTimeToStringMSSQLExecute);     // v.19.02
  FunctionList.RegisterFunction('DateTimeToStringISO8601', ftString, [ftDateTime], nil, DateTimeToStringISO8601Execute); // v.19.02

  FunctionList.RegisterFunction('IncDay', ftDateTime, [ftDateTime, ftInteger], nil, IncDayExecute);     // v.16.10
  FunctionList.RegisterFunction('IncWeek', ftDateTime, [ftDateTime, ftInteger], nil, IncWeekExecute);   // v.16.10
  FunctionList.RegisterFunction('IncMonth', ftDateTime, [ftDateTime, ftInteger], nil, IncMonthExecute); // v.16.10
  FunctionList.RegisterFunction('IncYear', ftDateTime, [ftDateTime, ftInteger], nil, IncYearExecute);   // v.16.10

  FunctionList.RegisterFunction('IncHour', ftDateTime, [ftDateTime, ftInteger], nil, IncHourExecute);               // v.16.10
  FunctionList.RegisterFunction('IncMinute', ftDateTime, [ftDateTime, ftInteger], nil, IncMinuteExecute);           // v.16.10
  FunctionList.RegisterFunction('IncSecond', ftDateTime, [ftDateTime, ftInteger], nil, IncSecondExecute);           // v.16.10
  FunctionList.RegisterFunction('IncMillisecond', ftDateTime, [ftDateTime, ftInteger], nil, IncMilliSecondExecute); // v.16.10

  FunctionList.RegisterFunction('DaysBetween', ftInteger, [ftDateTime, ftDateTime], nil, DaysBetweenExecute);       // v.18.3
  FunctionList.RegisterFunction('WeeksBetween', ftInteger, [ftDateTime, ftDateTime], nil, WeeksBetweenExecute);     // v.18.3
  FunctionList.RegisterFunction('MonthsBetween', ftInteger, [ftDateTime, ftDateTime], nil, MonthsBetweenExecute);   // v.18.3
  FunctionList.RegisterFunction('YearsBetween', ftInteger, [ftDateTime, ftDateTime], nil, YearsBetweenExecute);     // v.18.3

  FunctionList.RegisterFunction('Integer', ftInteger, [ftUnknown], nil, ToIntegerExecute);
  FunctionList.RegisterFunction('String', ftString, [ftUnknown], nil, ToStringExecute);
  FunctionList.RegisterFunction('DateTime', ftDateTime, [ftUnknown], nil, ToDateTimeExecute); // v.18.3
  FunctionList.RegisterFunction('Float', ftFloat, [ftUnknown], nil, ToFloatExecute);          // v.18.3
  FunctionList.RegisterFunction('Money', ftCurrency, [ftUnknown], nil, ToMoneyExecute);       // v.18.3

  FunctionList.RegisterFunction('Trunc', ftInteger, [ftFloat], nil, TruncExecute);
  FunctionList.RegisterFunction('Round', ftInteger, [ftFloat], nil, RoundExecute);
  FunctionList.RegisterFunction('Round', ftFloat, [ftFloat, ftInteger], nil, RoundExtExecute); // v.18.4
  FunctionList.RegisterFunction('Frac', ftFloat, [ftFloat], nil, FracExecute);
  FunctionList.RegisterFunction('iabs', ftInteger, [ftInteger], nil, AbsExecute);
  FunctionList.RegisterFunction('fabs', ftFloat, [ftFloat], nil, AbsExecute);
  FunctionList.RegisterFunction('abs', ftUnknown, [ftUnknown], AbsExtCheckSyntax, AbsExtExecute); // v.18.4
  FunctionList.RegisterFunction('Power', ftFloat, [ftFloat, ftFloat], nil, PowerExecute);
  FunctionList.RegisterFunction('Random', ftInteger, [ftInteger], nil, RandomExecute);
  FunctionList.RegisterFunction('Counter', ftString, [ftString], nil, CounterExecute);
  FunctionList.RegisterFunction('Units', ftString, [ftInteger, ftString, ftString, ftString], nil, UnitsExecute); // v.16.12

  FunctionList.RegisterFunction('Char', ftString, [ftInteger], nil, ToCharExecute);
  FunctionList.RegisterFunction('Format', ftString, [ftUnknown, ftString], nil, FormatExecute);
  FunctionList.RegisterFunction('Length', ftInteger, [ftString], nil, LengthExecute);
  FunctionList.RegisterFunction('SubString', ftString, [ftString, ftInteger, ftInteger], nil, SubstringExecute);
  FunctionList.RegisterFunction('ExtractString', ftString, [ftString, ftString, ftString], nil, ExtractStringExecute);
  FunctionList.RegisterFunction('Split', ftString, [ftString, ftInteger, ftString], nil, SplitStringExecute);
  FunctionList.RegisterFunction('Split', ftString, [ftString, ftInteger], nil, SplitStringExecute);
  FunctionList.RegisterFunction('Trim', ftString, [ftString], nil, TrimExecute);
  FunctionList.RegisterFunction('UpperCase', ftString, [ftString], nil, UpperCaseExecute);
  FunctionList.RegisterFunction('LowerCase', ftString, [ftString], nil, LowerCaseExecute);
  FunctionList.RegisterFunction('Replace', ftString, [ftString, ftString, ftString], nil, ReplaceExecute);
  FunctionList.RegisterFunction('Pos', ftInteger, [ftString, ftString], nil, PosExecute);
  FunctionList.RegisterFunction('Translate', ftString, [ftString, ftInteger], nil, TranslateExecute); // v.16.8
  FunctionList.RegisterFunction('WrapString', ftString, [ftString, ftString, ftWord, ftWord], nil, WrapStringExecute); // v.18.3

  FunctionList.RegisterFunction('QuotedString', ftString, [ftString, ftString], nil, QuotedStringExecute); // v.17.2
  FunctionList.RegisterFunction('DequotedString', ftString, [ftString, ftString], nil, DeQuotedStringExecute); // v.17.2

  FunctionList.RegisterFunction('ValueFromPathXML', ftString, [ftString, ftString], nil, ValueFromPathXMLExecute, TFunctionPathXmlDescr, TRUE); // v.18.3
  FunctionList.RegisterFunction('BodyFromPathXML', ftString, [ftString, ftString], nil, BodyFromPathXMLExecute, TFunctionPathXmlDescr, TRUE);
  FunctionList.RegisterFunction('XMLFromPathXML', ftString, [ftString, ftString], nil, XMLFromPathXMLExecute, TFunctionPathXmlDescr, TRUE); // v.19.2
  FunctionList.RegisterFunction('ValueToPathXML', ftString, [ftString, ftString, ftString{ ftUnknown}], nil, ValueToPathXMLExecute, TFunctionPathXmlDescr, TRUE); // v.18.5

  FunctionList.RegisterFunction('ValueFromPathJSON', ftString, [ftString, ftString], nil, ValueFromPathJSONExecute, TRUE); // v.20.3

  FunctionList.RegisterFunction('Include', ftUnknown, [ftUnknown, ftUnknown], nil, IncludeExecute);
  FunctionList.RegisterFunction('Exclude', ftUnknown, [ftUnknown, ftUnknown], nil, ExcludeExecute);

  FunctionList.RegisterFunction('Permission', ftBoolean, [ftUnknown, ftUnknown, ftInteger], PermissionCheckSyntax, PermissionExecute);
  FunctionList.RegisterFunction('InGroup', ftBoolean, [ftUnknown{, ftUnknown}], InGroupCheckSyntax, InGroupExecute);
  FunctionList.RegisterFunction('InGroup', ftBoolean, [ftUnknown, ftUnknown], InGroupCheckSyntax, InGroupExecute);

  // Функции ТОЛЬКО для действий!!!
  FunctionList.RegisterFunction('Update', ftBoolean, [ftUnknown], UpdateCheckSyntax, UpdateExecute); // v.16.6
  FunctionList.RegisterFunction('Update', ftBoolean, [ftUnknown, ftUnknown], UpdateCheckExSyntax, UpdateExExecute); // v.18.5
  FunctionList.RegisterFunction('Open', ftBoolean, [ftUnknown, ftUnknown], OpenCheckSyntax, OpenExecute); // v.17.11
  FunctionList.RegisterFunction('Focus', ftBoolean, [ftUnknown, ftUnknown], FocusCheckSyntax, FocusExecute); // v.18.3

  // Функции работы с файлами и каталогами ...
  FunctionList.RegisterFunction('IncludeTrailingBackslash', ftString, [ftString], nil, IncludeTrailingBackslashExecute); // v.17.4
  FunctionList.RegisterFunction('ExcludeTrailingBackslash', ftString, [ftString], nil, ExcludeTrailingBackslashExecute); // v.17.4

  FunctionList.RegisterFunction('FileExists', ftBoolean, [ftString], nil, FileExistsExecute); // v.17.4
  FunctionList.RegisterFunction('DirectoryExists', ftBoolean, [ftString], nil, DirectoryExistsExecute); // v.17.4

  FunctionList.RegisterFunction('ExtractFileDrive', ftString, [ftString], nil, ExtractFileDriveExecute); // v.17.4
  FunctionList.RegisterFunction('ExtractFilePath', ftString, [ftString], nil, ExtractFilePathExecute);   // v.17.4
  FunctionList.RegisterFunction('ExtractFileDir', ftString, [ftString], nil, ExtractFileDirExecute);     // v.17.4
  FunctionList.RegisterFunction('ExtractFileName', ftString, [ftString], nil, ExtractFileNameExecute);   // v.17.4
  FunctionList.RegisterFunction('ExtractFileExt', ftString, [ftString], nil, ExtractFileExtExecute);     // v.17.4

//  FunctionList.RegisterFunction('ExpandFileName', ftString, [ftString], nil, ExpandFileNameExecute); // v.17.4

  FunctionList.RegisterFunction('ChangeFilePath', ftString, [ftString, ftString], nil, ChangeFilePathExecute); // v.17.4
  FunctionList.RegisterFunction('ChangeFileName', ftString, [ftString, ftString], nil, ChangeFileNameExecute); // v.17.4
  FunctionList.RegisterFunction('ChangeFileExt', ftString, [ftString, ftString], nil, ChangeFileExtExecute);   // v.17.4

  // Только при включенной директиве условной компиляции!!! По умолчанию собирается без ЭТИХ ОПАСНЫХ функций!!!
  FunctionList.RegisterFunction('CreateDir', ftBoolean, [ftString], nil, CreateDirExecute); // v.17.4
  {$IFDEF DANGEROUSFUNCTIONS}
  FunctionList.RegisterFunction('RemoveDir', ftBoolean, [ftString], nil, RemoveDirExecute); // v.17.4
  FunctionList.RegisterFunction('DeleteFile', ftBoolean, [ftString], nil, DeleteFileExecute); // v.17.4
  FunctionList.RegisterFunction('RenameFile', ftBoolean, [ftString, ftString], nil, RenameFileExecute); // v.17.4
  {$ENDIF}

  FunctionList.RegisterFunction('NormalizeFilePath', ftString, [ftString], nil, NormalizeFilePathExecute); // v.17.4
  FunctionList.RegisterFunction('NormalizeFileName', ftString, [ftString], nil, NormalizeFileNameExecute); // v.17.4

  FunctionList.RegisterFunction('FileToName', ftString, [ftUnknown], nil, FileToNameExecute); // v.22.1
  FunctionList.RegisterFunction('FileToSize', ftInteger, [ftUnknown], nil, FileToSizeExecute); // v.22.1
  FunctionList.RegisterFunction('FileToDate', ftDateTime, [ftUnknown], nil, FileToDateExecute); // v.22.1
  FunctionList.RegisterFunction('FileToBody', ftBlob, [ftUnknown], nil, FileToBodyExecute); // v.22.1

  FunctionList.RegisterFunction('Latitude', ftFloat, [], nil, LatitudeExecute); // v.20.5
  FunctionList.RegisterFunction('Longitude', ftFloat, [], nil, LongitudeExecute); // v.20.5

  FunctionList.RegisterFunction('NewGUID', ftGUID, [], nil, NewGUIDExecute); // v.17.6
  FunctionList.RegisterFunction('IsGUID', ftBoolean, [ftUnknown], nil, IsGUIDExecute); // v.17.6

  FunctionList.RegisterFunction('RGB', ftInteger, [ftByte, ftByte, ftByte], nil, RGBExecute);           // v.17.11
  FunctionList.RegisterFunction('HLS', ftInteger, [ftByte, ftByte, ftByte], nil, HLSExecute);           // v.17.11
  FunctionList.RegisterFunction('CMYK', ftInteger, [ftByte, ftByte, ftByte, ftByte], nil, CMYKExecute); // v.17.11

  FunctionList.RegisterFunction('EncodeURL', ftString, [ftString], nil, EncodeURLExecute);       // v.18.3
  FunctionList.RegisterFunction('DecodeURL', ftString, [ftString], nil, DecodeURLExecute);       // v.18.3
  FunctionList.RegisterFunction('EncodeHTML', ftString, [ftString], nil, EncodeHTMLExecute);     // v.18.3
  FunctionList.RegisterFunction('DecodeHTML', ftString, [ftString], nil, DecodeHTMLExecute);     // v.18.3
  FunctionList.RegisterFunction('EncodeBase64', ftString, [ftString], nil, EncodeBase64Execute); // v.18.3
  FunctionList.RegisterFunction('DecodeBase64', ftString, [ftString], nil, DecodeBase64Execute); // v.18.3
  FunctionList.RegisterFunction('DecodeUnicode', ftString, [ftString], nil, DecodeUnicodeExecute); // v.18.3

  FunctionList.RegisterFunction('EncodeBase64', ftString, [ftBlob], nil, EncodeBase64BlobExecute); // v.18.9
  FunctionList.RegisterFunction('BLOB', ftBlob, [ftBlob], nil, BlobExecute);                       // v.18.9

  {$IFDEF INDYUSED}
  FunctionList.RegisterFunction('GetHTTP', ftString, [ftString, ftString], nil, GetHTTPExecute, TFunctionIndyDescr, TRUE); // v.18.3
  // Эти функции ДОЛЖНЫ РАБОТАТЬ С МАССИВАМИ, но калькулятор и парсер не поддерживает работу с массивами!!!
  FunctionList.RegisterFunction('Contact', ftUnknown, [ftString], nil, ContactExecute, TRUE);                     // v.20.2
  FunctionList.RegisterFunction('SendMail', ftUnknown, [], nil, SendMailAllExecute, TRUE);                        // v.20.2
  FunctionList.RegisterFunction('SendMail', ftUnknown, [ftUnknown], nil, SendMailExecute, TRUE);                  // v.20.2
  FunctionList.RegisterFunction('ReceiveMail', ftUnknown, [], nil, ReceiveMailAllExecute, TRUE);                  // v.20.2
  // Пока закомментировал ЭТИ ДВЕ функции, т.к. парсер НЕ РАБОТАЕТ!!!
  //FunctionList.RegisterFunction('ReceiveMail', ftUnknown, [ftBoolean], nil, ReceiveMailAllExecute, TRUE);         // v.20.2
  //FunctionList.RegisterFunction('ReceiveMail', ftUnknown, [ftUnknown, ftBoolean], nil, ReceiveMailExecute, TRUE); // v.20.2

  FunctionList.RegisterFunction('TGBot', ftInteger, [ftString], nil, TGBotExecute, TFunctionTelegramDescr, True);                                                       // v.23.6
  FunctionList.RegisterFunction('TGContact', ftInteger, [ftString], nil, TGContactExecute, TFunctionTelegramDescr, True);                                               // v.23.6
  FunctionList.RegisterFunction('TGChat', ftInteger, [ftInteger, ftInteger], nil, TGChatExecute, TFunctionTelegramDescr, True);                                         // v.23.6
  FunctionList.RegisterFunction('TGGetFile', ftInteger, [ftInteger, ftInteger], nil, TGGetFileExecute, TFunctionTelegramDescr, True);                                   // v.23.6
  FunctionList.RegisterFunction('TGReadMessages', ftInteger, [ftInteger], nil, TGReadMessagesExecute, TFunctionTelegramDescr, True);                                    // v.23.6

  FunctionList.RegisterFunction('TGSendMessage', ftInteger, [ftInteger, ftString], nil, TGSendMessageExecute, TFunctionTelegramDescr, True);                            // v.23.6
  FunctionList.RegisterFunction('TGSendMessage', ftInteger, [ftInteger, ftString, ftString], nil, TGSendMessageExecute, TFunctionTelegramDescr, True);                  // v.23.6
  FunctionList.RegisterFunction('TGSendMessage', ftInteger, [ftInteger, ftString, ftInteger], nil, TGSendMessageExecute, TFunctionTelegramDescr, True);                 // v.23.6
  FunctionList.RegisterFunction('TGSendMessage', ftInteger, [ftInteger, ftString, ftString, ftInteger], nil, TGSendMessageExecute, TFunctionTelegramDescr, True);       // v.23.6

  FunctionList.RegisterFunction('TGSendPhoto', ftInteger, [ftInteger, ftString], nil, TGSendPhotoExecute, TFunctionTelegramDescr, True);                                // v.23.6
  FunctionList.RegisterFunction('TGSendPhoto', ftInteger, [ftInteger, ftString, ftString], nil, TGSendPhotoExecute, TFunctionTelegramDescr, True);                      // v.23.6
  FunctionList.RegisterFunction('TGSendPhoto', ftInteger, [ftInteger, ftString, ftInteger], nil, TGSendPhotoExecute, TFunctionTelegramDescr, True);                     // v.23.6
  FunctionList.RegisterFunction('TGSendPhoto', ftInteger, [ftInteger, ftString, ftString, ftString], nil, TGSendPhotoExecute, TFunctionTelegramDescr, True);            // v.23.6
  FunctionList.RegisterFunction('TGSendPhoto', ftInteger, [ftInteger, ftString, ftString, ftString, ftInteger], nil, TGSendPhotoExecute, TFunctionTelegramDescr, True); // v.23.6

  FunctionList.RegisterFunction('TGSendDocument', ftInteger, [ftInteger, ftString], nil, TGSendDocumentExecute, TFunctionTelegramDescr, True);                                // v.23.6
  FunctionList.RegisterFunction('TGSendDocument', ftInteger, [ftInteger, ftString, ftString], nil, TGSendDocumentExecute, TFunctionTelegramDescr, True);                      // v.23.6
  FunctionList.RegisterFunction('TGSendDocument', ftInteger, [ftInteger, ftString, ftInteger], nil, TGSendDocumentExecute, TFunctionTelegramDescr, True);                     // v.23.6
  FunctionList.RegisterFunction('TGSendDocument', ftInteger, [ftInteger, ftString, ftString, ftString], nil, TGSendDocumentExecute, TFunctionTelegramDescr, True);            // v.23.6
  FunctionList.RegisterFunction('TGSendDocument', ftInteger, [ftInteger, ftString, ftString, ftString, ftInteger], nil, TGSendDocumentExecute, TFunctionTelegramDescr, True); // v.23.6

  FunctionList.RegisterFunction('TGReadAll', ftInteger, [], nil, TGReadAllExecute, TFunctionTelegramDescr, True);                                                             // v.23.6
  FunctionList.RegisterFunction('TGSendAll', ftInteger, [], nil, TGSendAllExecute, TFunctionTelegramDescr, True);                                                             // v.23.6
  {$ENDIF}

  {$IFDEF DEBUG}
  FunctionList.DebugFunctionsLog('Function list loaded ...');
  {$ENDIF}
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DeFunctions unit finalization ...');
  {$ENDIF}
  FreeAndNil(FunctionList);
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

