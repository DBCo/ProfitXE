{$WARN SYMBOL_PLATFORM OFF}

unit uTelegramManager;

interface

uses Windows, SysUtils, JSON, IdHTTP;

type
  ETelegramException = class(Exception);

  TTelegramManager = class
  private
    FHTTP: TIdHTTP;
    FTelegramURL: string;
    FUserAgent: string;
    FTempDirectory: string;
    procedure Connect;
    procedure Disconnect;
    function GetMe(const Token: string): string;
    function GetUpdates(const Token: string; const Offset: string = ''; const Limit: Integer = 100; const Timeout: Integer = 0): string;
    function BotEditMessageText(const Token, RecipientCode, MessageCode, TextMessage: string; const TextParseMode: string = ''): string;
    function BotEditMessageCaption(const Token, RecipientCode, MessageCode, TextCaption: string; const CaptionParseMode: string = ''): string;
    function BotSendMessage(const Token, RecipientCode, TextMessage: string; const TextParseMode: string = ''; const ReplyMessageCode: string = ''): string;
    function BotSendPhoto(const Token, RecipientCode, FileName: string; const TextCaption: string = ''; const CaptionParseMode: string = ''; const ReplyMessageCode: string = ''): string;
    function BotSendDocument(const Token, RecipientCode, FileName: string; const TextCaption: string = ''; const CaptionParseMode: string = ''; const ReplyMessageCode: string = ''): string;
    function BotDeleteMessage(const Token, RecipientCode, MessageCode: string): string;
    function GetFile(const Token, FileCode: string): string;
    function RegisterBot(const Token: string): Variant;
    function GetLastUpdateID(const OwnerID: Integer; var UpdateID: Integer): Boolean;
    function GetMessageFileCount(const MessageID: Integer): Integer;
    function GetBotToken(const BotID: Integer): string;
    function AppendUpdate(const OwnerID: Integer; const JSON: string; const Code: string = ''): Integer;
    function AppendMessage(const OwnerID, ChatID, FromID: Integer; const TextMessage: string; const MessageKindID: Integer = 0; const ParentID: Integer = 0; const UpdateID: Integer = 0; const Code: string = ''; const DoneStageID: Integer = 0): Integer;
    function AppendFile(const UpdateID, MessageID, TypeID: Integer; const FileName, FileCode: string; const FileSize: Integer = 0; const FileMime: string = ''; const FileUniqueCode: string = ''; const Width: Integer = 0; const Height: Integer = 0; const Duration: Integer = 0): Integer;
    function FindContactByJSON(ObjectJSON: TJSONValue; const OwnerID: Integer): Integer;
    function ChatKicked(const ChatID: Integer): Boolean;
    function FindUpdateByCode(const UpdateCode: string; const OwnerID: Integer): Integer;
    function FindMessageByCode(const MessageCode: string; const OwnerID: Integer): Integer;
    function FindFileByCode(const FileCode: string; const FileTypeID, UpdateID: Integer; const MessageID: Integer = 0): Integer;
    function ProcessDocumentJSON(DocumentJSON: TJSONObject; const FileTypeID, MessageID, UpdateID, OwnerID: Integer): Integer;
    function ProcessPhotoJSON(PhotoJSON: TJSONArray; const MessageID, UpdateID, OwnerID: Integer): Integer;
    function ProcessMessageJSON(MessageJSON: TJSONObject; const OwnerID, UpdateID, DoneStageID: Integer): Integer;
    function ProcessMyChatMemberJSON(MyChatMemberJSON: TJSONObject; const OwnerID, UpdateID: Integer): Integer;
    function ProcessUpdateJSON(UpdateJSON: TJSONObject; const OwnerID: Integer): Integer;
    function SendQueueMessage(const MessageID: Integer): Boolean;
    function SendMessageCreateCacheItem(CacheItem: TObject): Boolean;
    function SendMessageUpdateCacheItem(CacheItem: TObject): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Bot(const Text: string): Variant;
    function Contact(const Name: string): Variant;
    function Chat(const OwnerID, ContactID: Integer; const ReadOnly: Boolean): Variant;
    function ReadMessage(const OwnerID: Integer; const FromUpdateID: Integer = 0; const Limit: Integer = 100; const Timeout: Integer = 0): Variant;
    function SendMessage(const ChatID: Integer; const Text: string; const TextFormat: string = ''; ReplyToMessageID: Integer = 0): Variant;
    function SendPhoto(const ChatID: Integer; const FileName: string; const Caption: string = ''; const CaptionFormat: string = ''; ReplyToMessageID: Integer = 0): Variant;
    function SendDocument(const ChatID: Integer; const FileName: string; const Caption: string = ''; const CaptionFormat: string = ''; ReplyToMessageID: Integer = 0): Variant;
    function LoadFile(const BotID, FileID: Integer): Variant;
    function ReadAll(const Limit: Integer = 100; const Timeout: Integer = 0): Variant;
    function SendAll: Variant;
  end;

const
  cTelegramAPI = 'https://api.telegram.org/'; // Адрес сервера Telegram API.
  cMaxAutoLoagingFileSize = 4 * 1024;         // Максимальный размер файла,
                                              // который будет загружен автоматически
                                              // при получении пакета обновления!
                                              // 0 - не загружать автоматически!

  cContentTypeJSON = 'Application/json';
  cContentCharsetUTF8 = 'utf-8';

implementation

uses Variants, StrUtils, DateUtils, IOUtils, Classes, IdMultipartFormData,
  IdCookieManager, IdSSLOpenSSL, DB, {$IFDEF DEBUG}DeLog,{$ENDIF} DeTypes,
  Funcs, QueryDescriptor, DeDB, DeMeta, DeMetadata, DeControls, DataCacheUnit;

{ TTelegramManager }

constructor TTelegramManager.Create;
var
  VersionMS, VersionLS: Cardinal;
begin
  FUserAgent := UpperCase(cApplicationName) + ', version ';
  if GetFileVersion(GetModuleName(hInstance), VersionMS, VersionLS) then
    FUserAgent := FUserAgent + Format('%u.%u.%u.%u', [HiWord(VersionMS), LoWord(VersionMS), HiWord(VersionLS), LoWord(VersionLS)])
  else
    FUserAgent := FUserAgent + '1.0a';
  {$IFDEF WIN32}
  FUserAgent := FUserAgent + ', x86';
  {$ENDIF}
  {$IFDEF WIN64}
  FUserAgent := FUserAgent + ', x64';
  {$ENDIF}
  FTempDirectory := TPath.GetTempPath;
  if Length(FTempDirectory) <> 0 then
    FTempDirectory := IncludeTrailingBackslash(FTempDirectory);
end;

destructor TTelegramManager.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TTelegramManager.Connect;
begin
  if not Assigned(FHTTP) then
    begin
      FHTTP := TIdHTTP.Create(nil);
      FHTTP.CookieManager := TIdCookieManager.Create(FHTTP);
      FTelegramURL := cTelegramAPI;
      if (Length(FTelegramURL) <> 0) and (FTelegramURL[Length(FTelegramURL)] <> '/') then
        FTelegramURL := FTelegramURL + '/';
      if Pos('https://', LowerCase(FTelegramURL)) = 1 then
        begin
          FHTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FHTTP);
          TIdSSLIOHandlerSocketOpenSSL(FHTTP.IOHandler).SSLOptions.Method := sslvSSLv23;
        end;
    end;
  FHTTP.Request.UserAgent := FUserAgent;
  FHTTP.Request.ContentType := cContentTypeJSON;
end;

procedure TTelegramManager.Disconnect;
begin
  FreeAndNil(FHTTP);
  FTelegramURL := EmptyStr;
end;

function TTelegramManager.GetMe(const Token: string): string;
var
  URL: string;
  Strings: TStrings;
  Stream: TStream;
begin
  Connect;
  URL := FTelegramURL + 'bot' + Token + '/getMe';
  Strings := TStringList.Create;
  try
    Stream := TMemoryStream.Create;
    try
      FHTTP.Get(URL, Stream);
      Stream.Seek(0, soFromBeginning);
      Strings.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

function TTelegramManager.GetUpdates(const Token, Offset: string; const Limit, Timeout: Integer): string;
var
  URL: string;
  RealLimit: Integer;
  Stream: TStream;
  Strings: TStrings;
begin
  Connect;
  URL := FTelegramURL + 'bot' + Token + '/getUpdates?limit=';
  if Limit < 1 then
    begin
      RealLimit := 1;
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Limit value %d not between 1 and 100! The default minimum value of 1 will be used.', [Limit], False, 'telegram');
      {$ENDIF}
    end
  else
    if Limit > 100 then
      begin
        RealLimit := 100;
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Limit value %d not between 1 and 100! The default maximum value of 100 will be used.', [Limit], False, 'telegram');
        {$ENDIF}
      end
    else
      RealLimit := Limit;
  URL := URL + IntToStr(RealLimit);
  if Length(Offset) <> 0 then
    URL := URL + '&offset=' + Offset;
  if Timeout <> 0 then
    URL := URL + '&timeout=' + IntToStr(Timeout);
  Stream := TMemoryStream.Create;
  try
    Strings := TStringList.Create;
    try
      FHTTP.Get(URL, Stream);
      Stream.Seek(0, soFromBeginning);
      Strings.LoadFromStream(Stream);
      Result := Strings.Text;
    finally
      Strings.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function TTelegramManager.BotEditMessageText(const Token, RecipientCode, MessageCode, TextMessage, TextParseMode: string): string;
var
  URL: string;
  RequestStream, ResponseStream: TStream;
  Strings: TStrings;
  PreambleSize: Integer;
begin
  Connect;
  URL := FTelegramURL + 'bot' + Token + '/editMessageText';
  RequestStream := TMemoryStream.Create;
  try
    Strings := TStringList.Create;
    try
      Strings.Text := '{'#13#10'"chat_id": "' + RecipientCode + '",'#13#10'"message_id": "' + MessageCode + '",';
      if Length(TextParseMode) <> 0 then
        Strings.Append('"parse_mode": "' + TextParseMode + '",');
      Strings.Append('"text": "' +  ReplaceText(TextMessage, '"', '\"') + '"'#13#10'}');
      ResponseStream := TMemoryStream.Create;
      try
        Strings.SaveToStream(ResponseStream, TUTF8Encoding.UTF8);
        PreambleSize := Length(TUTF8Encoding.UTF8.GetPreamble);
        ResponseStream.Seek(PreambleSize, soFromBeginning);
        RequestStream.CopyFrom(ResponseStream, ResponseStream.Size - PreambleSize);
      finally
        ResponseStream.Free;
      end;
      RequestStream.Seek(0, soFromBeginning);
      ResponseStream := TMemoryStream.Create;
      try
        FHTTP.Post(URL, RequestStream, ResponseStream);
        ResponseStream.Seek(0, soFromBeginning);
        Strings.LoadFromStream(ResponseStream);
      finally
        ResponseStream.Free;
      end;
      Result := Strings.Text;
    finally
      Strings.Free;
    end;
  finally
    RequestStream.Free;
  end;
end;

function TTelegramManager.BotEditMessageCaption(const Token, RecipientCode, MessageCode, TextCaption, CaptionParseMode: string): string;
var
  URL: string;
  RequestStream, ResponseStream: TStream;
  Strings: TStrings;
  PreambleSize: Integer;
begin
  Connect;
  URL := FTelegramURL + 'bot' + Token + '/editMessageCaption';
  RequestStream := TMemoryStream.Create;
  try
    Strings := TStringList.Create;
    try
      Strings.Text := '{'#13#10'"chat_id": "' + RecipientCode + '",'#13#10'"message_id": "' + MessageCode + '",';
      if Length(CaptionParseMode) <> 0 then
        Strings.Append('"parse_mode": "' + CaptionParseMode + '",');
      Strings.Append('"caption": "' +  ReplaceText(TextCaption, '"', '\"') + '"'#13#10'}');
      ResponseStream := TMemoryStream.Create;
      try
        Strings.SaveToStream(ResponseStream, TUTF8Encoding.UTF8);
        PreambleSize := Length(TUTF8Encoding.UTF8.GetPreamble);
        ResponseStream.Seek(PreambleSize, soFromBeginning);
        RequestStream.CopyFrom(ResponseStream, ResponseStream.Size - PreambleSize);
      finally
        ResponseStream.Free;
      end;
      RequestStream.Seek(0, soFromBeginning);
      ResponseStream := TMemoryStream.Create;
      try
        FHTTP.Post(URL, RequestStream, ResponseStream);
        ResponseStream.Seek(0, soFromBeginning);
        Strings.LoadFromStream(ResponseStream);
      finally
        ResponseStream.Free;
      end;
      Result := Strings.Text;
    finally
      Strings.Free;
    end;
  finally
    RequestStream.Free;
  end;
end;

function TTelegramManager.BotSendMessage(const Token, RecipientCode, TextMessage, TextParseMode, ReplyMessageCode: string): string;
var
  URL: string;
  RequestStream, ResponseStream: TStream;
  Strings: TStrings;
  PreambleSize: Integer;
begin
  Connect;
  URL := FTelegramURL + 'bot' + Token + '/sendMessage';
  RequestStream := TMemoryStream.Create;
  try
    Strings := TStringList.Create;
    try
      Strings.Text := '{'#13#10'"chat_id": "' + RecipientCode + '",';
      if Length(ReplyMessageCode) <> 0 then
        Strings.Append('"reply_to_message_id": "' + ReplyMessageCode + '",');
      if Length(TextParseMode) <> 0 then
        Strings.Append('"parse_mode": "' + TextParseMode + '",');
      Strings.Append('"text": "' +  ReplaceText(TextMessage, '"', '\"') + '"'#13#10'}');
      ResponseStream := TMemoryStream.Create;
      try
        Strings.SaveToStream(ResponseStream, TUTF8Encoding.UTF8);
        PreambleSize := Length(TUTF8Encoding.UTF8.GetPreamble);
        ResponseStream.Seek(PreambleSize, soFromBeginning);
        RequestStream.CopyFrom(ResponseStream, ResponseStream.Size - PreambleSize);
      finally
        ResponseStream.Free;
      end;
      RequestStream.Seek(0, soFromBeginning);
      ResponseStream := TMemoryStream.Create;
      try
        FHTTP.Post(URL, RequestStream, ResponseStream);
        ResponseStream.Seek(0, soFromBeginning);
        Strings.LoadFromStream(ResponseStream);
      finally
        ResponseStream.Free;
      end;
      Result := Strings.Text;
    finally
      Strings.Free;
    end;
  finally
    RequestStream.Free;
  end;
end;

function TTelegramManager.BotSendPhoto(const Token, RecipientCode, FileName, TextCaption, CaptionParseMode, ReplyMessageCode: string): string;
var
  URL: string;
  RequestStream: TIdMultiPartFormDataStream;
  ResponseStream: TStream;
  Strings: TStrings;
  Index: Integer;
  WorkFileName: string;
begin
  Connect;
  URL := FTelegramURL + 'bot' + Token + '/sendPhoto';
  RequestStream := TIdMultiPartFormDataStream.Create;
  try
    RequestStream.AddFormField('chat_id', RecipientCode);
    if Length(ReplyMessageCode) <> 0 then
      RequestStream.AddFormField('reply_to_message_id', ReplyMessageCode);
    if Length(TextCaption) <> 0 then
      begin
        if Length(CaptionParseMode) <> 0 then
          RequestStream.AddFormField('parse_mode', CaptionParseMode);
        RequestStream.AddFormField('caption', TextCaption, 'utf-8');
      end
    else
      if Length(CaptionParseMode) <> 0 then
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('SendPhoto: Parameter "parse_mode" is not used without parameter "caption"!', False, 'telegram');
          {$ENDIF}
        end;
    Strings := TStringList.Create;
    try
      Strings.Text := Trim(FileName);
      for Index := 0 to Pred(Strings.Count) do
        begin
          WorkFileName := Strings[Index];
          {$IFDEF DeDEBUG}
          if not FileExists(WorkFileName) then
            Funcs.WriteLog('SendPhoto: Photo %s not found!', [QuotedStr(WorkFileName)], False, 'telegram');
          {$ENDIF}
          if (Index = 0) and (Strings.Count = 1) then
            RequestStream.AddFile('photo', WorkFileName)
          else
            RequestStream.AddFile('photo' + IntToStr(Succ(Index)), WorkFileName);
        end;
    finally
      Strings.Free;
    end;
    ResponseStream := TMemoryStream.Create;
    try
      Strings := TStringList.Create;
      try
        FHTTP.Post(URL, RequestStream, ResponseStream);
        ResponseStream.Seek(0, soFromBeginning);
        Strings.LoadFromStream(ResponseStream);
        Result := Strings.Text;
      finally
        Strings.Free;
      end;
    finally
      ResponseStream.Free;
    end;
  finally
    RequestStream.Free;
  end;
end;

function TTelegramManager.BotSendDocument(const Token, RecipientCode, FileName, TextCaption, CaptionParseMode, ReplyMessageCode: string): string;
var
  URL, WorkFileName: string;
  RequestStream: TIdMultiPartFormDataStream;
  ResponseStream: TStream;
  Strings: TStrings;
  Index: Integer;
begin
  Connect;
  URL := FTelegramURL + 'bot' + Token + '/sendDocument';
  RequestStream := TIdMultiPartFormDataStream.Create;
  try
    RequestStream.AddFormField('chat_id', RecipientCode);
    if Length(ReplyMessageCode) <> 0 then
      RequestStream.AddFormField('reply_to_message_id', ReplyMessageCode);
    if Length(TextCaption) <> 0 then
      begin
        if Length(CaptionParseMode) <> 0 then
          RequestStream.AddFormField('parse_mode', CaptionParseMode);
        RequestStream.AddFormField('caption', TextCaption);
      end
    else
      if Length(CaptionParseMode) <> 0 then
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('SendDocument: Parameter "parse_mode" is not used without parameter "caption"!', False, 'telegram');
          {$ENDIF}
        end;
    Strings := TStringList.Create;
    try
      Strings.Text := Trim(FileName);
      for Index := 0 to Pred(Strings.Count) do
        begin
          WorkFileName := Strings[Index];
          {$IFDEF DeDEBUG}
          if not FileExists(WorkFileName) then
            Funcs.WriteLog('SendDocument: Document %s not found!', [QuotedStr(WorkFileName)], False, 'telegram');
          {$ENDIF}
          if (Index = 0) and (Strings.Count = 1) then
            RequestStream.AddFile('document', WorkFileName)
          else
            RequestStream.AddFile('document' + IntToStr(Succ(Index)), WorkFileName);
        end;
    finally
      Strings.Free;
    end;
    ResponseStream := TMemoryStream.Create;
    try
      Strings := TStringList.Create;
      try
        FHTTP.Post(URL, RequestStream, ResponseStream);
        ResponseStream.Seek(0, soFromBeginning);
        Strings.LoadFromStream(ResponseStream);
        Result := Strings.Text;
      finally
        Strings.Free;
      end;
    finally
      ResponseStream.Free;
    end;
  finally
    RequestStream.Free;
  end;
end;

function TTelegramManager.BotDeleteMessage(const Token, RecipientCode, MessageCode: string): string;
var
  URL: string;
  Strings: TStrings;
  Stream: TStream;
begin
  Connect;
  URL := FTelegramURL + 'bot' + Token + '/deleteMessage?chat_id=' + RecipientCode + '&message_id=' + MessageCode;
  Strings := TStringList.Create;
  try
    Stream := TMemoryStream.Create;
    try
      FHTTP.Get(URL, Stream);
      Stream.Seek(0, soFromBeginning);
      Strings.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

function TTelegramManager.GetFile(const Token, FileCode: string): string;
var
  URL: string;
  Stream: TStream;
  Strings: TStrings;
  PreambleSize: Integer;
begin
  Connect;
  URL := FTelegramURL + 'bot' + Token + '/getFile?file_id=' + FileCode;
  Strings := TStringList.Create;
  try
    Stream := TMemoryStream.Create;
    try
      FHTTP.Get(URL, Stream);
      Stream.Seek(0, soFromBeginning);
      Strings.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

function TTelegramManager.RegisterBot(const Token: string): Variant;
var
  ResponseText: string;
  ResponseJSON, ResultJSON: TJSONObject;
  ValueJSON: TJSONValue;
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  CacheItem: TCacheItem;
  Value: string;
  function PrepareResultValue(ValueJSON: TJSONValue): string;
  begin
    if Assigned(ValueJSON) then
      Result := ValueJSON.Value
    else
      Result := EmptyStr;
  end;
begin
  try
    ResponseText := GetMe(Token);
    ResponseJSON := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;
    ValueJSON := ResponseJSON.Values['ok'];
    if Assigned(ValueJSON) and (ValueJSON is TJSONTrue) then
      begin
        ResultJSON := ResponseJSON.Values['result'] as TJSONObject;
        TableMeta := MetaData.TablesList.FindByGUID(guidTelegramContact);
        DataCache := TDataCache.Create(TableMeta);
        try
          CacheItem := DataCache.AddNewItem;
          DataCache.DataManager.PrepareRecord(CacheItem);
          CacheItem.ValueByName[fldTelegramContactType] := 1; // Бот
          CacheItem.ValueByName[fldTelegramContactToken] := Token;
          Value := Trim(PrepareResultValue(ResultJSON.Values['id']));
          if Length(Value) <> 0 then
            CacheItem.ValueByName[fldTelegramContactCode] := Value;
          Value := Trim(PrepareResultValue(ResultJSON.Values['first_name']));
          if Length(Value) <> 0 then
            CacheItem.ValueByName[fldTelegramContactFirst] := Value;
          Value := Trim(PrepareResultValue(ResultJSON.Values['last_name']));
          if Length(Value) <> 0 then
            CacheItem.ValueByName[fldTelegramContactLast] := Value;
          Value := Trim(PrepareResultValue(ResultJSON.Values['username']));
          if Length(Value) <> 0 then
            CacheItem.ValueByName[fldTelegramContactName] := Value;
          if DataCache.DataManager.CanInsertRecord(CacheItem) then
            if DataCache.DataManager.InsertRecord(CacheItem) then
              Result := DataCache[0].ValueByName[fldTelegramContactID]
            else
              raise ETelegramException.Create('Append bot error!')
          else
            raise ETelegramException.Create('Register bot failed!');
        finally
          DataCache.Free;
        end;
      end
    else
      raise ETelegramException.Create('Error receiving bot data response:'#13#10 + ResponseText);
  except
    on E: Exception do
      begin
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Register bot %s error: %s', [QuotedStr(Token), E.Message], False, 'telegram');
        {$ENDIF}
        Result := Null;
      end;
  end;
end;

function TTelegramManager.Bot(const Text: string): Variant;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
begin
  Result := Unassigned;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramContact);
  if Assigned(TableMeta) then
    try
      // Ищем бота ...
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramContactType), opEQ, 1); // Бот
          // Если это токен Telegram, то ...
          if Pos(':', Text) <> 0 then
            FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramContactToken), opEQ, Text)
          else
            FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramContactName), opEQ, Text);
          FilterItem.AddOperation(opAnd);
          DataCache.Fields.ChangeStages([fldTelegramContactCreated, fldTelegramContactModified], fsBlob);
          DataCache.LoadData(FilterItem);
          FilterItem := nil;
          if DataCache.Count = 0 then
            Result := Null
          else
            Result := DataCache[0].ValueByName[fldTelegramContactID];
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
      // Если бот не найден и указан токен, то ...
      if VarIsNullOrEmpty(Result) and (Pos(':', Text) <> 0) then
        Result := RegisterBot(Text);
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Contact error: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := Null;
        end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Contact table %s not found!', [GUIDToString(guidTelegramContact)], False, 'telegram');
      {$ENDIF}
    end;
end;

function TTelegramManager.Contact(const Name: string): Variant;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
begin
  Result := Unassigned;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramContact);
  if Assigned(TableMeta) then
    try
      // Ищем пользователя или группу ...
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramContactType), opNE, 1); // Не Бот
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramContactName), opEQ, Name);
          FilterItem.AddOperation(opAnd);
          DataCache.Fields.ChangeStages([fldTelegramContactCreated, fldTelegramContactModified], fsBlob);
          DataCache.LoadData(FilterItem);
          FilterItem := nil;
          if DataCache.Count = 0 then
            Result := Null
          else
            Result := DataCache[0].ValueByName[fldTelegramContactID];
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Contact error: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := Null;
        end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Contact table %s not found!', [GUIDToString(guidTelegramContact)], False, 'telegram');
      {$ENDIF}
    end;
end;

function TTelegramManager.Chat(const OwnerID, ContactID: Integer; const ReadOnly: Boolean): Variant;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  CacheItem: TCacheItem;
begin
  Result := Unassigned;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramChat);
  if Assigned(TableMeta) then
    try
      // Ищем пользователя или группу ...
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramChatOwner), opEQ, OwnerID); // Бот
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramChatContact), opEQ, ContactID);
          FilterItem.AddOperation(opAnd);
          DataCache.Fields.ChangeStages([fldTelegramChatCreated, fldTelegramChatModified], fsBlob);
          DataCache.LoadData(FilterItem);
          FilterItem := nil;
          if DataCache.Count = 0 then
            Result := Null
          else
            Result := DataCache[0].ValueByName[fldTelegramChatID];
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
      // Если между контактами нет чата, то создадим и вернём его если только не искали уже существующий ...
      if VarIsNull(Result) and not ReadOnly then
        begin
          DataCache := TDataCache.Create(TableMeta);
          try
            CacheItem := DataCache.AddNewItem;
            DataCache.DataManager.PrepareRecord(CacheItem);
            CacheItem.ValueByName[fldTelegramChatOwner] := OwnerID;
            CacheItem.ValueByName[fldTelegramChatContact] := ContactID;
            if DataCache.DataManager.CanInsertRecord(CacheItem) then
              if DataCache.DataManager.InsertRecord(CacheItem) then
                Result := DataCache[0].ValueByName[fldTelegramChatID];
          finally
            DataCache.Free;
          end;
        end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Chat error: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := Null;
        end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Chat table %s not found!', [GUIDToString(guidTelegramChat)], False, 'telegram');
      {$ENDIF}
    end;
end;

function TTelegramManager.GetLastUpdateID(const OwnerID: Integer; var UpdateID: Integer): Boolean;
var
  TableMeta: TTableMeta;
  Dataset: TDeDataset;
  Value: Variant;
begin
  Result := False;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramUpdate);
  if Assigned(TableMeta) and Assigned(TableMeta.Database) then
    begin
      Dataset := TableMeta.Database.CreateQuery(qtRow);
      try
        Dataset.Descr.BeginUpdate;
        try
          Dataset.Descr.Table := TableMeta.Table;
          Dataset.Descr.AddField(opMAX, fldTelegramUpdateID);
          Dataset.Descr.AddCondition(fldTelegramUpdateOwner, ftInteger, opEQ, OwnerID);
        finally
          Dataset.Descr.EndUpdate;
        end;
        Dataset.Open;
        Value := Dataset.Value[0];
        if VarIsNullOrEmpty(Value) then
          UpdateID := 0
        else
          UpdateID := VarToInt(Value);
        Result:= True;
      finally
        Dataset.Free;
      end;
    end;
end;

function TTelegramManager.GetMessageFileCount(const MessageID: Integer): Integer;
var
  TableMeta: TTableMeta;
  Dataset: TDeDataset;
begin
  Result := 0;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramFile);
  if Assigned(TableMeta) and Assigned(TableMeta.Database) then
    begin
      Dataset := TableMeta.Database.CreateQuery(qtRow);
      try
        Dataset.Descr.BeginUpdate;
        try
          Dataset.Descr.Table := TableMeta.Table;
          Dataset.Descr.AddField(opCOUNT, fldTelegramFileID);
          Dataset.Descr.AddCondition(fldTelegramFileMessage, ftInteger, opEQ, MessageID);
        finally
          Dataset.Descr.EndUpdate;
        end;
        Dataset.Open;
        Result := VarToInt(Dataset.Value[0]);
      finally
        Dataset.Free;
      end;
    end;
end;

function TTelegramManager.GetBotToken(const BotID: Integer): string;
var
  TableMeta: TTableMeta;
  Dataset: TDeDataset;
begin
  Result := EmptyStr;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramContact);
  if Assigned(TableMeta) and Assigned(TableMeta.Database) then
    begin
      Dataset := TableMeta.Database.CreateQuery(qtRow);
      try
        Dataset.Descr.BeginUpdate;
        try
          Dataset.Descr.Table := TableMeta.Table;
          Dataset.Descr.AddField(fldTelegramContactToken);
          Dataset.Descr.AddCondition(fldTelegramContactID, ftInteger, opEQ, BotID);
          Dataset.Descr.AddCondition(fldTelegramContactType, ftInteger, opEQ, 1); // Бот
          Dataset.Descr.AddOperation(opAnd);
        finally
          Dataset.Descr.EndUpdate;
        end;
        Dataset.Open;
        Result := VarToStr(Dataset.Value[0]);
      finally
        Dataset.Free;
      end;
    end;
end;

function TTelegramManager.AppendUpdate(const OwnerID: Integer; const JSON, Code: string): Integer;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  CacheItem: TCacheItem;
begin
  Result := 0;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramUpdate);
  if Assigned(TableMeta) then
    try
      DataCache := TDataCache.Create(TableMeta);
      try
        CacheItem := DataCache.AddNewItem;
        DataCache.DataManager.PrepareRecord(CacheItem);
        CacheItem.ValueByName[fldTelegramUpdateOwner] := OwnerID;
        if Length(Code) <> 0 then
          CacheItem.ValueByName[fldTelegramUpdateCode] := Code;
        if Length(JSON) <> 0 then
          CacheItem.ValueByName[fldTelegramUpdateJSON] := JSON;
        if DataCache.DataManager.CanInsertRecord(CacheItem) then
          if DataCache.DataManager.InsertRecord(CacheItem) then
            Result := VarToInt(DataCache[0].ValueByName[fldTelegramUpdateID])
          else
            raise ETelegramException.Create('Append update package error!')
        else
          raise ETelegramException.Create('Append update package failed!');
      finally
        DataCache.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Error appending the update package: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := 0;
        end;
    end;
end;

function TTelegramManager.AppendFile(const UpdateID, MessageID, TypeID: Integer;
  const FileName, FileCode: string; const FileSize: Integer; const FileMime,
  FileUniqueCode: string; const Width, Height, Duration: Integer): Integer;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  CacheItem: TCacheItem;
  Value: string;
  function DurationToString(Value: Integer): string;
  var
    Second, Minute: Integer;
  begin
    Second := Value mod 60;
    Value := Value div 60;
    Minute := Value mod 60;
    Value := Value div 60;
    Result := Format('%.2u-%.2u-%.2u', [Value, Minute, Second]);
  end;
begin
  Result := 0;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramFile);
  if Assigned(TableMeta) then
    try
      DataCache := TDataCache.Create(TableMeta);
      try
        CacheItem := DataCache.AddNewItem;
        DataCache.DataManager.PrepareRecord(CacheItem);
        if UpdateID <> 0 then
          CacheItem.ValueByName[fldTelegramFileUpdate] := UpdateID;
        if MessageID <> 0 then
          CacheItem.ValueByName[fldTelegramFileMessage] := MessageID;
        CacheItem.ValueByName[fldTelegramFileType] := TypeID;
        if Length(FileName) = 0 then
          begin
            case TypeID of
              2: { Photo }
                begin
                  Value := 'Photo';
                  if (Width <> 0) and (Height <> 0) then
                    Value := Value + '_' + Format('%ux%u', [Width, Height]);
                  CacheItem.ValueByName[fldTelegramFileName] := Value;
                end;
              3: { Audio }
                begin
                  Value := 'Audio';
                  if Duration <> 0 then
                    Value := Value + '_' + DurationToString(Duration);
                  CacheItem.ValueByName[fldTelegramFileName] := Value;
                end;
              4: { Video }
                begin
                  Value := 'Video';
                  if Duration <> 0 then
                    Value := Value + '_' + DurationToString(Duration);
                  CacheItem.ValueByName[fldTelegramFileName] := Value;
                end;
            end;
          end
        else
          CacheItem.ValueByName[fldTelegramFileName] := FileName;
        if Length(FileCode) <> 0 then
          CacheItem.ValueByName[fldTelegramFileCode] := FileCode;
        if FileSize <> 0 then
          CacheItem.ValueByName[fldTelegramFileSize] := FileSize;
        if Length(FileMime) <> 0 then
          CacheItem.ValueByName[fldTelegramFileMime] := FileMime;
        if Length(FileUniqueCode) <> 0 then
          CacheItem.ValueByName[fldTelegramFileUniqueCode] := FileUniqueCode;
        if (Width <> 0) or (Height <> 0) then
          begin
            CacheItem.ValueByName[fldTelegramFileWidth] := Width;
            CacheItem.ValueByName[fldTelegramFileHeight] := Height;
          end;
        if Duration <> 0 then
          CacheItem.ValueByName[fldTelegramFileDuration] := Duration;
        if DataCache.DataManager.CanInsertRecord(CacheItem) then
          if DataCache.DataManager.InsertRecord(CacheItem) then
            Result := VarToInt(DataCache[0].ValueByName[fldTelegramFileID])
          else
            raise ETelegramException.Create('Append file error!')
        else
          raise ETelegramException.Create('Append file failed!');
      finally
        DataCache.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Error appending the file: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := 0;
        end;
    end;
end;

function TTelegramManager.AppendMessage(const OwnerID, ChatID, FromID: Integer; const TextMessage: string; const MessageKindID, ParentID, UpdateID: Integer; const Code: string; const DoneStageID: Integer): Integer;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  CacheItem: TCacheItem;
begin
  Result := 0;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramMessage);
  if Assigned(TableMeta) then
    try
      DataCache := TDataCache.Create(TableMeta);
      try
        CacheItem := DataCache.AddNewItem;
        DataCache.DataManager.PrepareRecord(CacheItem);
        if MessageKindID <> 0 then
          CacheItem.ValueByName[fldTelegramMessageKind] := MessageKindID;
        CacheItem.ValueByName[fldTelegramMessageOwner] := OwnerID;
        if ParentID <> 0 then
          CacheItem.ValueByName[fldTelegramMessageParent] := ParentID;
        if ChatID <> 0 then
          CacheItem.ValueByName[fldTelegramMessageChat] := ChatID;
        if FromID <> 0 then
          CacheItem.ValueByName[fldTelegramMessageFrom] := FromID;
        if UpdateID <> 0 then
          CacheItem.ValueByName[fldTelegramMessageUpdate] := UpdateID;
        if Length(Code) <> 0 then
          CacheItem.ValueByName[fldTelegramMessageCode] := Code;
        if DoneStageID <> 0 then
          CacheItem.ValueByName[fldTelegramMessageDone] := DoneStageID;
        if Length(TextMessage) <> 0 then
          CacheItem.ValueByName[fldTelegramMessageMessage] := TextMessage;
        if DataCache.DataManager.CanInsertRecord(CacheItem) then
          if DataCache.DataManager.InsertRecord(CacheItem) then
            Result := VarToInt(DataCache[0].ValueByName[fldTelegramMessageID])
          else
            raise ETelegramException.Create('Append message error!')
        else
          raise ETelegramException.Create('Append message failed!');
      finally
        DataCache.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Error appending the message: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := 0;
        end;
    end;
end;

function TTelegramManager.FindContactByJSON(ObjectJSON: TJSONValue; const OwnerID: Integer): Integer;
var
  ValueJSON: TJSONValue;
  TableMeta: TTableMeta;
  FilterItem: TFilterItem;
  DataCache: TDataCache;
  CacheItem: TCacheItem;
  IsChat: Boolean;
  Value: string;
  function PrepareResultValue(ValueJSON: TJSONValue): string;
  begin
    if Assigned(ValueJSON) then
      Result := ValueJSON.Value
    else
      Result := EmptyStr;
  end;
begin
  Result := 0;
  if Assigned(ObjectJSON) and (ObjectJSON is TJSONObject) then
    begin
      ValueJSON := (ObjectJSON as TJSONObject).Values['id'];
      if Assigned(ValueJSON) and (Length(Trim(ValueJSON.Value)) <> 0) then
        begin
          TableMeta := MetaData.TablesList.FindByGUID(guidTelegramContact);
          // Ищем контакт ...
          FilterItem := TFilterItem.Create;
          try
            DataCache := TDataCache.Create(TableMeta);
            try
              FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramContactCode), opEQ, ValueJSON.Value);
              DataCache.Fields.ChangeStages([fldTelegramContactCreated, fldTelegramContactModified], fsBlob);
              DataCache.LoadData(FilterItem);
              FilterItem := nil;
              if DataCache.Count <> 0 then
                Result := VarToInt(DataCache[0].ValueByName[fldTelegramContactID]);
            finally
              DataCache.Free;
            end;
          finally
            FilterItem.Free;
          end;
          IsChat := False;
          // Если контакт не найден, то ...
          if Result = 0 then
            begin
              DataCache := TDataCache.Create(TableMeta);
              try
                CacheItem := DataCache.AddNewItem;
                DataCache.DataManager.PrepareRecord(CacheItem);
                CacheItem.ValueByName[fldTelegramContactCode] := ValueJSON.Value;
                ValueJSON := (ObjectJSON as TJSONObject).Values['is_bot'];
                if Assigned(ValueJSON) and (ValueJSON is TJSONTrue) then
                  CacheItem.ValueByName[fldTelegramContactType] := 1 // Бот
                else
                  begin
                    ValueJSON := (ObjectJSON as TJSONObject).Values['type'];
                    if Assigned(ValueJSON) then
                      begin
                        IsChat := True;
                        if SameText(ValueJSON.Value, 'private') then
                          CacheItem.ValueByName[fldTelegramContactType] := 2 // Пользователь
                        else
                          CacheItem.ValueByName[fldTelegramContactType] := 3; // Группа
                      end
                    else
                      CacheItem.ValueByName[fldTelegramContactType] := 2; // Пользователь
                  end;
                Value := Trim(PrepareResultValue((ObjectJSON as TJSONObject).Values['first_name']));
                if Length(Value) <> 0 then
                  CacheItem.ValueByName[fldTelegramContactFirst] := Value;
                Value := Trim(PrepareResultValue((ObjectJSON as TJSONObject).Values['last_name']));
                if Length(Value) <> 0 then
                  CacheItem.ValueByName[fldTelegramContactLast] := Value;
                Value := Trim(PrepareResultValue((ObjectJSON as TJSONObject).Values['username']));
                if Length(Value) = 0 then
                  begin
                    Value := Trim(PrepareResultValue((ObjectJSON as TJSONObject).Values['title']));
                    if Length(Value) <> 0 then
                      CacheItem.ValueByName[fldTelegramContactName] := Value;
                  end
                else
                  CacheItem.ValueByName[fldTelegramContactName] := Value;

                if DataCache.DataManager.CanInsertRecord(CacheItem) then
                  if DataCache.DataManager.InsertRecord(CacheItem) then
                    Result := VarToInt(DataCache[0].ValueByName[fldTelegramContactID]);
              finally
                DataCache.Free;
              end;
            end;
          // Если это чат, контакт создан и указан владелец чата, то ...
          if IsChat and (Result <> 0) and (OwnerID <> 0) then
            begin
              TableMeta := MetaData.TablesList.FindByGUID(guidTelegramChat);
              DataCache := TDataCache.Create(TableMeta);
              try
                CacheItem := DataCache.AddNewItem;
                DataCache.DataManager.PrepareRecord(CacheItem);
                CacheItem.ValueByName[fldTelegramChatOwner] := OwnerID;
                CacheItem.ValueByName[fldTelegramChatContact] := Result;
                if DataCache.DataManager.CanInsertRecord(CacheItem) then
                  DataCache.DataManager.InsertRecord(CacheItem);
              finally
                DataCache.Free;
              end;
            end;
        end;
    end;
end;

function TTelegramManager.ChatKicked(const ChatID: Integer): Boolean;
var
  TableMeta: TTableMeta;
  FilterItem: TFilterItem;
  DataCache: TDataCache;
begin
  Result := False;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramChat);
  if Assigned(TableMeta) then
    try
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramChatID), opEQ, ChatID);
          DataCache.LoadData(FilterItem);
          FilterItem := nil;
          if DataCache.Count <> 0 then
            if DataCache.CanDeleteRecord(DataCache[0]) then
              Result := DataCache.DeleteRecord(DataCache[0]);
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Error kicked from chat: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := False;
        end;
    end;
end;

function TTelegramManager.FindUpdateByCode(const UpdateCode: string; const OwnerID: Integer): Integer;
var
  TableMeta: TTableMeta;
  FilterItem: TFilterItem;
  DataCache: TDataCache;
begin
  Result := 0;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramUpdate);
  if Assigned(TableMeta) and (Length(UpdateCode) <> 0) then
    try
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramUpdateOwner), opEQ, OwnerID);
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramUpdateCode), opEQ, UpdateCode);
          FilterItem.AddOperation(opAnd);
          DataCache.LoadData(FilterItem, fsKey);
          FilterItem := nil;
          if DataCache.Count <> 0 then
            Result := VarToInt(DataCache[0].ValueByName[fldTelegramUpdateID]);
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Error find update: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := 0;
        end;
    end;
end;

function TTelegramManager.FindMessageByCode(const MessageCode: string; const OwnerID: Integer): Integer;
var
  TableMeta: TTableMeta;
  FilterItem: TFilterItem;
  DataCache: TDataCache;
begin
  Result := 0;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramMessage);
  if Assigned(TableMeta) and (Length(MessageCode) <> 0) then
    try
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramMessageOwner), opEQ, OwnerID);
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramMessageCode), opEQ, MessageCode);
          FilterItem.AddOperation(opAnd);
          DataCache.LoadData(FilterItem, fsKey);
          FilterItem := nil;
          if DataCache.Count <> 0 then
            Result := VarToInt(DataCache[0].ValueByName[fldTelegramMessageID]);
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Error find message: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := 0;
        end;
    end;
end;

function TTelegramManager.FindFileByCode(const FileCode: string; const FileTypeID, UpdateID, MessageID: Integer): Integer;
var
  TableMeta: TTableMeta;
  FilterItem: TFilterItem;
  DataCache: TDataCache;
begin
  Result := 0;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramFile);
  if Assigned(TableMeta) and (Length(FileCode) <> 0) then
    try
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramFileType), opEQ, FileTypeID);
          if UpdateID <> 0 then
            begin
              FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramFileUpdate), opEQ, UpdateID);
              FilterItem.AddOperation(opAnd);
            end;
          if MessageID <> 0 then
            begin
              FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramFileMessage), opEQ, MessageID);
              FilterItem.AddOperation(opAnd);
            end;
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramFileCode), opEQ, FileCode);
          FilterItem.AddOperation(opAnd);
          DataCache.LoadData(FilterItem, fsKey);
          FilterItem := nil;
          if DataCache.Count <> 0 then
            Result := VarToInt(DataCache[0].ValueByName[fldTelegramFileID]);
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Error find message: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := 0;
        end;
    end;
end;

function TTelegramManager.ProcessDocumentJSON(DocumentJSON: TJSONObject; const FileTypeID, MessageID, UpdateID, OwnerID: Integer): Integer;
var
  FileCode: string;
  FileSize: Integer;
  function PrepareStringValue(const ValueName: string): string;
  var
    ValueJSON: TJSONValue;
  begin
    ValueJSON := DocumentJSON.Values[ValueName];
    if Assigned(ValueJSON) then
      Result := ValueJSON.Value
    else
      Result := EmptyStr;
  end;
  function PrepareIntegerValue(const ValueName: string): Integer;
  begin
    Result := StrToIntDef(PrepareStringValue(ValueName), 0);
  end;
begin
  Result := 0;
  if Assigned(DocumentJSON) then
    begin
      FileCode := PrepareStringValue('file_id');
      Result := FindFileByCode(FileCode, FileTypeID, UpdateID, MessageID);
      if Result = 0 then
        begin
          FileSize := PrepareIntegerValue('file_size');
          Result := AppendFile(UpdateID, MessageID, FileTypeID, PrepareStringValue('file_name'),
            FileCode, FileSize,
            PrepareStringValue('mime_type'), PrepareStringValue('file_unique_id'),
            PrepareIntegerValue('width'), PrepareIntegerValue('height'));
          // Если файл добавлен успешно и указан Бот, то можно попробовать загрузить файл автоматически при маленьком размере и включенной автозагрузке!
          if (Result <> 0) and (OwnerID <> 0) then
            if (FileSize <> 0) and (cMaxAutoLoagingFileSize <> 0) and (FileSize <= cMaxAutoLoagingFileSize) then
              LoadFile(OwnerID, Result);
        end;
    end;
end;

function TTelegramManager.ProcessPhotoJSON(PhotoJSON: TJSONArray; const MessageID, UpdateID, OwnerID: Integer): Integer;
var
  Index: Integer;
begin
  Result := 0;
  if Assigned(PhotoJSON) then
    for Index := 0 to Pred(PhotoJSON.Count) do
      if PhotoJSON.Items[Index] is TJSONObject then
        if ProcessDocumentJSON(PhotoJSON.Items[Index] as TJSONObject, 2, MessageID, UpdateID, OwnerID) <> 0 then
          Inc(Result);
end;

function TTelegramManager.ProcessMessageJSON(MessageJSON: TJSONObject; const OwnerID, UpdateID, DoneStageID: Integer): Integer;
var
  ValueJSON: TJSONValue;
  UnixDate: Int64;
  DateTime: TDateTime;
  MessageCode, MessageText: string;
  MessageKindID, ChatID, FromID, ParentID: Integer;
begin
  Result := 0;
  if Assigned(MessageJSON) then
    begin
      ValueJSON := MessageJSON.Values['message_id'];
      if Assigned(ValueJSON) then
        MessageCode := ValueJSON.Value
      else
        MessageCode := EmptyStr;
      Result := FindMessageByCode(MessageCode, OwnerID);
      if Result = 0 then
        begin
          DateTime := 0;
          MessageKindID := 0; // По умолчанию
          ValueJSON := MessageJSON.Values['date'];
          if Assigned(ValueJSON) then
            if TryStrToInt64(ValueJSON.Value, UnixDate) then
              DateTime := UnixToDateTime(UnixDate);
          ValueJSON := MessageJSON.Values['text'];
          if Assigned(ValueJSON) then
            begin
              MessageText := ValueJSON.Value;
              if Pos('/', MessageText) = 1 then
                MessageKindID := 2; // Команда!
            end
          else
            begin
              ValueJSON := MessageJSON.Values['caption'];
              if Assigned(ValueJSON) then
                MessageText := ValueJSON.Value;
            end;
          ChatID := Chat(OwnerID, FindContactByJSON(MessageJSON.Values['chat'], OwnerID), False);
          FromID := FindContactByJSON(MessageJSON.Values['from'], OwnerID);
          ValueJSON := MessageJSON.Values['reply_to_message'];
          // Если это ответ на сообщение, то найдём сначала оригинальное сообщение ...
          if Assigned(ValueJSON) and (ValueJSON is TJSONObject) then
            ParentID := ProcessMessageJSON(ValueJSON as TJSONObject, OwnerID, UpdateID, 1)
          else
            ParentID := 0;
          Result := AppendMessage(OwnerID, ChatID, FromID, MessageText, MessageKindID, ParentID, UpdateID, MessageCode, DoneStageID);
          ValueJSON := MessageJSON.Values['document'];
          // Если есть вложение документа, то ...
          if Assigned(ValueJSON) and (ValueJSON is TJSONObject) then
            ProcessDocumentJSON(ValueJSON as TJSONObject, 1, Result, UpdateID, OwnerID);
          ValueJSON := MessageJSON.Values['photo'];
          // Если есть вложение фотографий, то ...
          if Assigned(ValueJSON) and (ValueJSON is TJSONArray) then
            ProcessPhotoJSON(ValueJSON as TJSONArray, Result, UpdateID, OwnerID);
        end;
    end;
end;

function TTelegramManager.ProcessMyChatMemberJSON(MyChatMemberJSON: TJSONObject; const OwnerID, UpdateID: Integer): Integer;
var
  ValueJSON: TJSONValue;
  UnixDate: Int64;
  DateTime: TDateTime;
  ObjectJSON: TJSONObject;
  FromID, ChatID, UserID: Integer;
  UserName: string;
begin
  Result := 0;
  if Assigned(MyChatMemberJSON) then
    begin
      ValueJSON := MyChatMemberJSON.Values['date'];
      if Assigned(ValueJSON) then
        if TryStrToInt64(ValueJSON.Value, UnixDate) then
          DateTime := UnixToDateTime(UnixDate);
      ChatID := Chat(OwnerID, FindContactByJSON(MyChatMemberJSON.Values['chat'], 0), True);
      FromID := FindContactByJSON(MyChatMemberJSON.Values['from'], OwnerID);
      ValueJSON := MyChatMemberJSON.Values['new_chat_member'];
      if Assigned(ValueJSON) and (ValueJSON is TJSONObject) then
        begin
          ObjectJSON := ValueJSON as TJSONObject;
          ValueJSON := ObjectJSON.Values['status'];
          // Если есть новый статус и "пнули ногой под зад", то ...
          if Assigned(ValueJSON) and SameText(ValueJSON.Value, 'kicked') then
            begin
              UserID := FindContactByJSON(ObjectJSON.Values['user'], 0);
              if (ChatID <> 0) and (UserID = OwnerID) then
                ChatKicked(ChatID);
              ValueJSON := ObjectJSON.Values['user'];
              if Assigned(ValueJSON) and (ValueJSON is TJSONObject) then
                begin
                  ObjectJSON := ValueJSON as TJSONObject;
                  ValueJSON := ObjectJSON.Values['first_name'];
                  if Assigned(ValueJSON) then
                    UserName := Trim(ValueJSON.Value)
                  else
                    begin
                      ValueJSON := ObjectJSON.Values['username'];
                      if Assigned(ValueJSON) then
                        UserName := Trim(ValueJSON.Value)
                      else
                        UserName := 'User';
                    end;
                  // Добавим системное сообщение и вернём его идентификатор!!!
                  Result := AppendMessage(OwnerID, ChatID, FromID, UserName + ' kicked from chat', 1, 0, UpdateID);
                end;
            end;
        end;
    end;
end;

function TTelegramManager.ProcessUpdateJSON(UpdateJSON: TJSONObject; const OwnerID: Integer): Integer;
var
  ValueJSON: TJSONValue;
  UpdateCode: string;
begin
  Result := 0;
  if Assigned(UpdateJSON) then
    begin
      ValueJSON := UpdateJSON.Values['update_id'];
      if Assigned(ValueJSON) then
        begin
          UpdateCode := ValueJSON.Value;
          Result := FindUpdateByCode(UpdateCode, OwnerID);
        end
      else
        UpdateCode := EmptyStr;
      if Result = 0 then
        Result := AppendUpdate(OwnerID, UpdateJSON.ToString, UpdateCode);
      if Result <> 0 then
        begin
          ValueJSON := UpdateJSON.Values['message'];
          // Если есть сообщение, то ...
          if Assigned(ValueJSON) and (ValueJSON is TJSONObject) then
            ProcessMessageJSON(ValueJSON as TJSONObject, OwnerID, Result, 1)
          else
            begin
              ValueJSON := UpdateJSON.Values['my_chat_member'];
              // Если есть изменение статуса чата, то ...
              if Assigned(ValueJSON) and (ValueJSON is TJSONObject) then
                ProcessMyChatMemberJSON(ValueJSON as TJSONObject, OwnerID, Result);
            end;
        end;
    end;
end;

function TTelegramManager.ReadMessage(const OwnerID, FromUpdateID, Limit, Timeout: Integer): Variant;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  UpdateID: Integer;
  UpdateCode, BotToken: string;
  ResponseText: string;
  ResponseJSON: TJSONObject;
  ResultJSON: TJSONArray;
  ValueJSON: TJSONValue;
  Index, Count: Integer;
begin
  Result := Unassigned;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramUpdate);
  if Assigned(TableMeta) then
    try
      UpdateID := FromUpdateID;
      if UpdateID = 0 then
        GetLastUpdateID(OwnerID, UpdateID);
      if UpdateID = 0 then
        UpdateCode := EmptyStr
      else
        begin
          FilterItem := TFilterItem.Create;
          try
            DataCache := TDataCache.Create(TableMeta);
            try
              FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramUpdateID), opEQ, UpdateID);
              DataCache.Fields.ChangeStages([fldTelegramUpdateCode], fsKey);
              DataCache.LoadData(FilterItem, fsKey);
              FilterItem := nil;
              if DataCache.Count <> 0 then
                UpdateCode := Trim(VarToStr(DataCache[0].ValueByName[fldTelegramUpdateCode]));
            finally
              DataCache.Free;
            end;
          finally
            FilterItem.Free;
          end;
        end;
      BotToken := GetBotToken(OwnerID);
      ResponseText := GetUpdates(BotToken, UpdateCode);
      ResponseJSON := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;
      Result := Null;
      ValueJSON := ResponseJSON.Values['ok'];
      if Assigned(ValueJSON) and (ValueJSON is TJSONTrue) then
        begin
          ResultJSON := ResponseJSON.Values['result'] as TJSONArray;
          Count := 0;
          for Index := 0 to Pred(ResultJSON.Count) do
            if ResultJSON.Items[Index] is TJSONObject then
              if ProcessUpdateJSON(ResultJSON.Items[Index] as TJSONObject, OwnerID) <> 0 then
                Inc(Count);
          Result := Count;
        end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Error reading the update package: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := Null;
        end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('The %s update table was not found!', [GUIDToString(guidTelegramUpdate)], False, 'telegram');
      {$ENDIF}
    end;
end;

function TTelegramManager.SendMessage(const ChatID: Integer; const Text, TextFormat: string; ReplyToMessageID: Integer): Variant;
var
  ChatTableMeta, MessageTableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  OwnerID, MessageID: Integer;
  BotToken, ContactCode, MessageCode, ResponseText: string;
  ResponseJSON: TJSONObject;
  ValueJSON: TJSONValue;
begin
  Result := Unassigned;
  ChatTableMeta := MetaData.TablesList.FindByGUID(guidTelegramChat);
  MessageTableMeta := MetaData.TablesList.FindByGUID(guidTelegramMessage);
  if Assigned(ChatTableMeta) then
    if Assigned(MessageTableMeta) then
      try
        OwnerID := 0;
        BotToken := EmptyStr;
        ContactCode := EmptyStr;
        MessageCode := EmptyStr;
        // Ищем пользователя или группу ...
        FilterItem := TFilterItem.Create;
        try
          DataCache := TDataCache.Create(ChatTableMeta);
          try
            FilterItem.AddCondition(ChatTableMeta.Fields.FindByName(fldTelegramChatID), opEQ, ChatID);
            DataCache.Fields.ChangeStages([fldTelegramChatOwner, fldTelegramChatContact], fsKey);
            DataCache.LoadData(FilterItem, fsKey);
            FilterItem := nil;
            if DataCache.Count = 0 then
              raise ETelegramException.CreateFmt('Chat %d not found.', [ChatID])
            else
              if VarToInt(DataCache[0].ValueByName[fldTelegramChatOwner + '.' + fldTelegramContactType]) = 1 then
                begin
                  OwnerID := VarToInt(DataCache[0].ValueByName[fldTelegramChatOwner]);
                  BotToken := VarToStr(DataCache[0].ValueByName[fldTelegramChatOwner + '.' + fldTelegramContactToken]);
                  ContactCode := VarToStr(DataCache[0].ValueByName[fldTelegramChatContact + '.' + fldTelegramContactCode]);
                end
              else
                raise ETelegramException.Create('Only a bot can send messages.');
          finally
            DataCache.Free;
          end;
        finally
          FilterItem.Free;
        end;
        if ReplyToMessageID <> 0 then
          begin
            FilterItem := TFilterItem.Create;
            try
              DataCache := TDataCache.Create(MessageTableMeta);
              try
                FilterItem.AddCondition(MessageTableMeta.Fields.FindByName(fldTelegramMessageID), opEQ, ReplyToMessageID);
                DataCache.Fields.ChangeStages([fldTelegramMessageCode, fldTelegramMessageOwner, fldTelegramMessageChat], fsKey);
                DataCache.LoadData(FilterItem, fsKey);
                FilterItem := nil;
                if DataCache.Count = 0 then
                  raise ETelegramException.CreateFmt('Message %d not found.', [ReplyToMessageID])
                else
                  if VarToInt(DataCache[0].ValueByName[fldTelegramMessageOwner]) = OwnerID then
                    if VarToInt(DataCache[0].ValueByName[fldTelegramMessageChat]) = ChatID then
                      MessageCode := VarToStr(DataCache[0].ValueByName[fldTelegramMessageCode])
                    else
                      raise ETelegramException.CreateFmt('Message %d is not a message for chat %d.', [ReplyToMessageID, ChatID])
                  else
                    raise ETelegramException.CreateFmt('Message %d is not a message for owner %d.', [ReplyToMessageID, OwnerID]);
              finally
                DataCache.Free;
              end;
            finally
              FilterItem.Free;
            end;
          end;
        ResponseText := BotSendMessage(BotToken, ContactCode, Text, TextFormat, MessageCode);
        ResponseJSON := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;
        Result := Null;
        ValueJSON := ResponseJSON.Values['ok'];
        if Assigned(ValueJSON) and (ValueJSON is TJSONTrue) then
          begin
            MessageID := ProcessMessageJSON(ResponseJSON.Values['result'] as TJSONObject, OwnerID, 0, 2);
            if MessageID <> 0 then
              Result := MessageID;
          end;
      except
        on E: Exception do
          begin
            {$IFDEF DeDEBUG}
            Funcs.WriteLog('Error sending message: %s', [E.Message], False, 'telegram');
            {$ENDIF}
            Result := Null;
          end;
      end
    else
      begin
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Message table %s not found!', [GUIDToString(guidTelegramMessage)], False, 'telegram');
        {$ENDIF}
      end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Chat table %s not found!', [GUIDToString(guidTelegramChat)], False, 'telegram');
      {$ENDIF}
    end;
end;

function TTelegramManager.SendPhoto(const ChatID: Integer; const FileName, Caption, CaptionFormat: string; ReplyToMessageID: Integer): Variant;
var
  ChatTableMeta, MessageTableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  OwnerID, MessageID: Integer;
  BotToken, ContactCode, MessageCode, ResponseText: string;
  ResponseJSON: TJSONObject;
  ValueJSON: TJSONValue;
begin
  Result := Unassigned;
  ChatTableMeta := MetaData.TablesList.FindByGUID(guidTelegramChat);
  MessageTableMeta := MetaData.TablesList.FindByGUID(guidTelegramMessage);
  if Assigned(ChatTableMeta) then
    if Assigned(MessageTableMeta) then
      try
        OwnerID := 0;
        BotToken := EmptyStr;
        ContactCode := EmptyStr;
        MessageCode := EmptyStr;
        // Ищем пользователя или группу ...
        FilterItem := TFilterItem.Create;
        try
          DataCache := TDataCache.Create(ChatTableMeta);
          try
            FilterItem.AddCondition(ChatTableMeta.Fields.FindByName(fldTelegramChatID), opEQ, ChatID);
            DataCache.Fields.ChangeStages([fldTelegramChatOwner, fldTelegramChatContact], fsKey);
            DataCache.LoadData(FilterItem, fsKey);
            FilterItem := nil;
            if DataCache.Count = 0 then
              raise ETelegramException.CreateFmt('Chat %d not found.', [ChatID])
            else
              if VarToInt(DataCache[0].ValueByName[fldTelegramChatOwner + '.' + fldTelegramContactType]) = 1 then
                begin
                  OwnerID := VarToInt(DataCache[0].ValueByName[fldTelegramChatOwner]);
                  BotToken := VarToStr(DataCache[0].ValueByName[fldTelegramChatOwner + '.' + fldTelegramContactToken]);
                  ContactCode := VarToStr(DataCache[0].ValueByName[fldTelegramChatContact + '.' + fldTelegramContactCode]);
                end
              else
                raise ETelegramException.Create('Only a bot can send photos.');
          finally
            DataCache.Free;
          end;
        finally
          FilterItem.Free;
        end;
        if ReplyToMessageID <> 0 then
          begin
            FilterItem := TFilterItem.Create;
            try
              DataCache := TDataCache.Create(MessageTableMeta);
              try
                FilterItem.AddCondition(MessageTableMeta.Fields.FindByName(fldTelegramMessageID), opEQ, ReplyToMessageID);
                DataCache.Fields.ChangeStages([fldTelegramMessageCode, fldTelegramMessageOwner, fldTelegramMessageChat], fsKey);
                DataCache.LoadData(FilterItem, fsKey);
                FilterItem := nil;
                if DataCache.Count = 0 then
                  raise ETelegramException.CreateFmt('Message %d not found.', [ReplyToMessageID])
                else
                  if VarToInt(DataCache[0].ValueByName[fldTelegramMessageOwner]) = OwnerID then
                    if VarToInt(DataCache[0].ValueByName[fldTelegramMessageChat]) = ChatID then
                      MessageCode := VarToStr(DataCache[0].ValueByName[fldTelegramMessageCode])
                    else
                      raise ETelegramException.CreateFmt('Message %d is not a message for chat %d.', [ReplyToMessageID, ChatID])
                  else
                    raise ETelegramException.CreateFmt('Message %d is not a message for owner %d.', [ReplyToMessageID, OwnerID]);
              finally
                DataCache.Free;
              end;
            finally
              FilterItem.Free;
            end;
          end;
        ResponseText := BotSendPhoto(BotToken, ContactCode, FileName, Caption, CaptionFormat, MessageCode);
        ResponseJSON := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;
        Result := Null;
        ValueJSON := ResponseJSON.Values['ok'];
        if Assigned(ValueJSON) and (ValueJSON is TJSONTrue) then
          begin
            MessageID := ProcessMessageJSON(ResponseJSON.Values['result'] as TJSONObject, OwnerID, 0, 2);
            if MessageID <> 0 then
              Result := MessageID;
          end;
      except
        on E: Exception do
          begin
            {$IFDEF DeDEBUG}
            Funcs.WriteLog('Error sending photo: %s', [E.Message], False, 'telegram');
            {$ENDIF}
            Result := Null;
          end;
      end
    else
      begin
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Message table %s not found!', [GUIDToString(guidTelegramMessage)], False, 'telegram');
        {$ENDIF}
      end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Chat table %s not found!', [GUIDToString(guidTelegramChat)], False, 'telegram');
      {$ENDIF}
    end;
end;

function TTelegramManager.SendDocument(const ChatID: Integer; const FileName, Caption, CaptionFormat: string; ReplyToMessageID: Integer): Variant;
var
  ChatTableMeta, MessageTableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  OwnerID, MessageID: Integer;
  BotToken, ContactCode, MessageCode, ResponseText: string;
  ResponseJSON: TJSONObject;
  ValueJSON: TJSONValue;
begin
  Result := Unassigned;
  ChatTableMeta := MetaData.TablesList.FindByGUID(guidTelegramChat);
  MessageTableMeta := MetaData.TablesList.FindByGUID(guidTelegramMessage);
  if Assigned(ChatTableMeta) then
    if Assigned(MessageTableMeta) then
      try
        OwnerID := 0;
        BotToken := EmptyStr;
        ContactCode := EmptyStr;
        MessageCode := EmptyStr;
        // Ищем пользователя или группу ...
        FilterItem := TFilterItem.Create;
        try
          DataCache := TDataCache.Create(ChatTableMeta);
          try
            FilterItem.AddCondition(ChatTableMeta.Fields.FindByName(fldTelegramChatID), opEQ, ChatID);
            DataCache.Fields.ChangeStages([fldTelegramChatOwner, fldTelegramChatContact], fsKey);
            DataCache.LoadData(FilterItem, fsKey);
            FilterItem := nil;
            if DataCache.Count = 0 then
              raise ETelegramException.CreateFmt('Chat %d not found.', [ChatID])
            else
              if VarToInt(DataCache[0].ValueByName[fldTelegramChatOwner + '.' + fldTelegramContactType]) = 1 then
                begin
                  OwnerID := VarToInt(DataCache[0].ValueByName[fldTelegramChatOwner]);
                  BotToken := VarToStr(DataCache[0].ValueByName[fldTelegramChatOwner + '.' + fldTelegramContactToken]);
                  ContactCode := VarToStr(DataCache[0].ValueByName[fldTelegramChatContact + '.' + fldTelegramContactCode]);
                end
              else
                raise ETelegramException.Create('Only a bot can send documents.');
          finally
            DataCache.Free;
          end;
        finally
          FilterItem.Free;
        end;
        if ReplyToMessageID <> 0 then
          begin
            FilterItem := TFilterItem.Create;
            try
              DataCache := TDataCache.Create(MessageTableMeta);
              try
                FilterItem.AddCondition(MessageTableMeta.Fields.FindByName(fldTelegramMessageID), opEQ, ReplyToMessageID);
                DataCache.Fields.ChangeStages([fldTelegramMessageCode, fldTelegramMessageOwner, fldTelegramMessageChat], fsKey);
                DataCache.LoadData(FilterItem, fsKey);
                FilterItem := nil;
                if DataCache.Count = 0 then
                  raise ETelegramException.CreateFmt('Message %d not found.', [ReplyToMessageID])
                else
                  if VarToInt(DataCache[0].ValueByName[fldTelegramMessageOwner]) = OwnerID then
                    if VarToInt(DataCache[0].ValueByName[fldTelegramMessageChat]) = ChatID then
                      MessageCode := VarToStr(DataCache[0].ValueByName[fldTelegramMessageCode])
                    else
                      raise ETelegramException.CreateFmt('Message %d is not a message for chat %d.', [ReplyToMessageID, ChatID])
                  else
                    raise ETelegramException.CreateFmt('Message %d is not a message for owner %d.', [ReplyToMessageID, OwnerID]);
              finally
                DataCache.Free;
              end;
            finally
              FilterItem.Free;
            end;
          end;
        ResponseText := BotSendDocument(BotToken, ContactCode, FileName, Caption, CaptionFormat, MessageCode);
        ResponseJSON := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;
        Result := Null;
        ValueJSON := ResponseJSON.Values['ok'];
        if Assigned(ValueJSON) and (ValueJSON is TJSONTrue) then
          begin
            MessageID := ProcessMessageJSON(ResponseJSON.Values['result'] as TJSONObject, OwnerID, 0, 2);
            if MessageID <> 0 then
              Result := MessageID;
          end;
      except
        on E: Exception do
          begin
            {$IFDEF DeDEBUG}
            Funcs.WriteLog('Error sending photo: %s', [E.Message], False, 'telegram');
            {$ENDIF}
            Result := Null;
          end;
      end
    else
      begin
        {$IFDEF DeDEBUG}
        Funcs.WriteLog('Message table %s not found!', [GUIDToString(guidTelegramMessage)], False, 'telegram');
        {$ENDIF}
      end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Chat table %s not found!', [GUIDToString(guidTelegramChat)], False, 'telegram');
      {$ENDIF}
    end;
end;

function TTelegramManager.LoadFile(const BotID, FileID: Integer): Variant;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  BotToken, ResponseText, FileName, FilePath: string;
  ResponseJSON, ResultJSON: TJSONObject;
  ValueJSON: TJSONValue;
  FileSize: Integer;
  Stream: TStream;
  Image: TDeImage;
begin
  Result := Unassigned;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramFile);
  if Assigned(TableMeta) then
    try
      BotToken := GetBotToken(BotID);
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramFileID), opEQ, FileID);
         { TODO -oKruglov -cBug : Здесь открываем кэш с максимальным стейджем, но BLOB поля не сохраняются! }
          DataCache.LoadData(FilterItem, fsBlob);
          FilterItem := nil;
          if DataCache.Count = 0 then
            raise ETelegramException.CreateFmt('The file with the identifier %d was not found or deleted!', [FileID])
          else
            begin
              ResponseText := GetFile(BotToken, Trim(VarToStr(DataCache[0].ValueByName[fldTelegramFileCode])));
              ResponseJSON := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;
              Result := Null;
              ValueJSON := ResponseJSON.Values['ok'];
              if Assigned(ValueJSON) and (ValueJSON is TJSONTrue) then
                begin
                  ValueJSON := ResponseJSON.Values['result'];
                  if Assigned(ValueJSON) and (ValueJSON is TJSONObject) then
                    begin
                      ResultJSON := ValueJSON as TJSONObject;
                      ValueJSON := ResultJSON.Values['file_path'];
                      if Assigned(ValueJSON) then
                        begin
                          FilePath := ValueJSON.Value;
                          FileName := ExtractFileName(ReplaceText(FilePath, '/', '\'));
                          if Length(FileName) = 0 then
                            begin
                              ValueJSON := ResultJSON.Values['file_unique_id'];
                              if Assigned(ValueJSON) then
                                FileName := ValueJSON.Value;
                            end;
                          if Length(FileName) = 0 then
                            FileName := Format('%u', [FileID]);
                          FileName := FTempDirectory + FileName;
                          Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
                          try
                            FHTTP.Get(FTelegramURL + '/file/bot' + BotToken + '/' + FilePath, Stream);
                          finally
                            Stream.Free;
                          end;
                          Image := TDeImage.Create(nil);
                          try
                            Image.LoadFromFile(FileName, ExtractFileName(ReplaceText(FilePath, '/', '\')));
                            DataCache[0].ValueByName[fldTelegramFileData] := Image.Value;
                          finally
                            Image.Free;
                          end;
                          ValueJSON := ResultJSON.Values['file_unique_id'];
                          if Assigned(ValueJSON) then
                            begin
                              ResponseText := ValueJSON.Value;
                              if not SameStr(ResponseText, VarToStr(DataCache[0].ValueByName[fldTelegramFileUniqueCode])) then
                                DataCache[0].ValueByName[fldTelegramFileUniqueCode] := ResponseText;
                            end;
                          ValueJSON := ResultJSON.Values['file_size'];
                          if Assigned(ValueJSON) then
                            begin
                              ResponseText := ValueJSON.Value;
                              if TryStrToInt(ResponseText, FileSize) then
                                if FileSize <> VarToInt(DataCache[0].ValueByName[fldTelegramFileSize]) then
                                  DataCache[0].ValueByName[fldTelegramFileSize] := FileSize;
                            end;
                          if not DataCache.UpdateRecord(DataCache[0]) then
                            raise ETelegramException.Create('The file data could not be saved in the database.');

                          Result := FileID;
                        end
                      else
                        raise ETelegramException.Create('Telegram API did not return the path with the file name to download!');
                    end;
                end;
            end;
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Error loading file: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := Null;
        end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('File table %s not found!', [GUIDToString(guidTelegramFile)], False, 'telegram');
      {$ENDIF}
    end;
end;

function TTelegramManager.ReadAll(const Limit, Timeout: Integer): Variant;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  Counter: Variant;
  Index: Integer;
begin
  Result := Unassigned;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramContact);
  if Assigned(TableMeta) then
    try
      // Ищем ботов для чтения обновлений ...
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramContactType), opEQ, 1); // Бот
          DataCache.LoadData(FilterItem, fsKey);
          FilterItem := nil;
          Result := 0;
          for Index := 0 to Pred(DataCache.Count) do
            begin
              Counter := ReadMessage(DataCache[0].ValueByName[fldTelegramContactID], 0, Limit, Timeout);
              if VarIsNumeric(Counter) then
                Result := Result + Counter;
            end;
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Contact error: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := Null;
        end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Contact table %s not found!', [GUIDToString(guidTelegramContact)], False, 'telegram');
      {$ENDIF}
    end;
end;

function TTelegramManager.SendAll: Variant;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  Index: Integer;
begin
  Result := Unassigned;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramMessage);
  if Assigned(TableMeta) then
    try
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramMessageToDo), opIsNot, Null);
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramMessageDone), opIs, Null);
          FilterItem.AddOperation(opAnd);
          DataCache.SortList.Add(TableMeta.Fields.FindByName(fldTelegramMessageDate).ID);
          DataCache.LoadData(FilterItem, fsKey);
          FilterItem := nil;
          Result := 0;
          for Index := 0 to Pred(DataCache.Count) do
            if SendQueueMessage(DataCache[Index].ValueByName[fldTelegramMessageID]) then
              Result := Result + 1;
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Error sending messages: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := Null;
        end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Message table %s not found!', [GUIDToString(guidTelegramMessage)], False, 'telegram');
      {$ENDIF}
    end;
end;

function TTelegramManager.SendQueueMessage(const MessageID: Integer): Boolean;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  ToDoAction: Integer;
  ResponseText: string;
  ResponseJSON: TJSONObject;
  ValueJSON: TJSONValue;
begin
  Result := False;
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramMessage);
  if Assigned(TableMeta) then
    try
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramMessageID), opEQ, MessageID);
          DataCache.LoadData(FilterItem, fsMax);
          FilterItem := nil;
          if DataCache.Count = 0 then
            raise ETelegramException.CreateFmt('Message with ID %d not found!', [MessageID]);
          try
            if not VarIsNullOrEmpty(DataCache[0].ValueByName[fldTelegramMessageDone]) then
              raise ETelegramException.CreateFmt('Message with ID %d already has process status!', [MessageID]);
            if DataCache[0].ValueByName[fldTelegramMessageKind] <> 3 then
              raise ETelegramException.CreateFmt('Message with ID %d must be a simple message!', [MessageID]);
            if DataCache[0].ValueByName[fldTelegramMessageOwner + '.' + fldTelegramContactType] <> 1 then
              raise ETelegramException.CreateFmt('The owner of the message with ID %d must be a bot to send it!', [MessageID]);

            ToDoAction := DataCache[0].ValueByName[fldTelegramMessageToDo];
            case ToDoAction of
              2: { Создание сообщения в Telegram }
                begin
                  if not VarIsNullOrEmpty(DataCache[0].ValueByName[fldTelegramMessageCode]) then
                    raise ETelegramException.CreateFmt('Message with ID %d cannot be created if field %s is filled!', [MessageID, QuotedStr(fldTelegramMessageCode)]);
                  Result := SendMessageCreateCacheItem(DataCache[0]);
                end;
              3: { Обновление сообщения в Telegram }
                begin
                  if VarIsNullOrEmpty(DataCache[0].ValueByName[fldTelegramMessageCode]) then
                    raise ETelegramException.CreateFmt('Message with ID %d cannot be updated if field %s is empty!', [MessageID, QuotedStr(fldTelegramMessageCode)]);
                  Result := SendMessageUpdateCacheItem(DataCache[0]);
                end;
              4: { Удаление сообщения в Telegram }
                begin
                  if VarIsNullOrEmpty(DataCache[0].ValueByName[fldTelegramMessageCode]) then
                    raise ETelegramException.CreateFmt('Message with ID %d cannot be deleted if field %s is empty!', [MessageID, QuotedStr(fldTelegramMessageCode)]);
                  ResponseText := BotDeleteMessage(
                    DataCache[0].ValueByName[fldTelegramMessageOwner + '.' + fldTelegramContactToken],
                    DataCache[0].ValueByName[fldTelegramMessageChat + '.' + fldTelegramChatContact + '.' + fldTelegramContactCode],
                    DataCache[0].ValueByName[fldTelegramMessageCode]);
                  ResponseJSON := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;
                  ValueJSON := ResponseJSON.Values['ok'];
                  if not (Assigned(ValueJSON) and (ValueJSON is TJSONTrue)) then
                    raise ETelegramException.CreateFmt('Message with ID %d could not be deleted on the server that replied %s!', [MessageID, QuotedStr(ResponseText)]);
                end;
            else
              raise ETelegramException.CreateFmt('Message with ID %s has an invalid value in the processing status of the send queue!', [MessageID]);
            end;
            DataCache[0].ValueByName[fldTelegramMessageDone] := DataCache[0].ValueByName[fldTelegramMessageToDo];
            DataCache[0].ValueByName[fldTelegramMessageToDo] := Null;
            Result := DataCache.UpdateRecord(DataCache[0]);
          except
            if Assigned(DataCache.DataManager) then
              begin
                DataCache[0].ValueByName[fldTelegramMessageDone] := 5; // Ошибка
                if DataCache.DataManager.CanUpdateRecord(DataCache[0]) then
                  DataCache.DataManager.UpdateRecord(DataCache[0]);
              end;
            raise;
          end;
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Error sending message: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := False;
        end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Message table %s not found!', [GUIDToString(guidTelegramMessage)], False, 'telegram');
      {$ENDIF}
    end;
end;

function TTelegramManager.SendMessageCreateCacheItem(CacheItem: TObject): Boolean;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  ResponseText, MessageText, FileName, MessageCode: string;
  ResponseJSON, MessageJSON: TJSONObject;
  ValueJSON: TJSONValue;
  Strings: TStrings;
  PhotoOnly: Boolean;
  Image: TDeImage;
  Index, MessageID: Integer;
  Stream: TStream;
  UnixDate: Int64;
begin
  Result := False;
  Assert(Assigned(CacheItem) and (CacheItem is TCacheItem));
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramFile);
  if Assigned(TableMeta) then
    try
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          MessageID := TCacheItem(CacheItem).ValueByName[fldTelegramMessageID];
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramFileMessage), opEQ, MessageID);
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramFileCode), opIs, Null);
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramFileData), opIsNot, Null);
          FilterItem.AddOperation(opAnd);
          DataCache.LoadData(FilterItem);
          FilterItem := nil;
          MessageText := VarToStr(TCacheItem(CacheItem).ValueByName[fldTelegramMessageMessage]);
          if DataCache.Count = 0 then
            ResponseText := BotSendMessage(
              TCacheItem(CacheItem).ValueByName[fldTelegramMessageOwner + '.' + fldTelegramContactToken],
              TCacheItem(CacheItem).ValueByName[fldTelegramMessageChat + '.' + fldTelegramChatContact + '.' + fldTelegramContactCode],
              MessageText, iif((Pos('<', MessageText) <> 0) and (Pos('</', MessageText) <> 0), 'html', EmptyStr),
              TCacheItem(CacheItem).ValueByName[fldTelegramMessageParent + '.' + fldTelegramMessageCode])
          else
            begin
              Strings := TStringList.Create;
              try
                Image := TDeImage.Create(nil);
                try
                  PhotoOnly := DataCache.Count = 0;
                  for Index := 0 to Pred(DataCache.Count) do
                    begin
                      Image.Value := DataCache[Index].ValueByName[fldTelegramFileData];
                      if not (Image.ImageType in [itJPEG, itGIF, itPNG]) then
                        PhotoOnly := False;
                      FileName := Trim(VarToStr(DataCache[Index].ValueByName[fldTelegramFileName]));
                      if Length(FileName) = 0 then
                        begin
                          if Image.ImageType in [itBMP, itJPEG, itGIF, itTIF, itPNG, itWMF] then
                            FileName := 'image'
                          else
                            FileName := 'file';
                          FileName := FileName + IntToStr(Succ(Index));
                        end;
                      if Length(ExtractFileExt(FileName)) = 0 then
                        case Image.ImageType of
                          itBMP: FileName := FileName + sExtensionBMP;
                          itJPEG: FileName := FileName + sExtensionJPG;
                          itGIF: FileName := FileName + sExtensionGIF;
                          itTIF: FileName := FileName + '.tif';
                          itPNG: FileName := FileName + sExtensionPNG;
                          itWMF: FileName := FileName + sExtensionWMF;
                        end;
                      if (Image.ImageType in [itBMP, itJPEG, itGIF, itTIF, itPNG, itWMF]) and Assigned(Image.Graphic) then
                        begin
                          if VarIsNullOrEmpty(DataCache[Index].ValueByName[fldTelegramFileWidth]) then
                            DataCache[Index].ValueByName[fldTelegramFileWidth] := Image.Graphic.Width;
                          if VarIsNullOrEmpty(DataCache[Index].ValueByName[fldTelegramFileHeight]) then
                            DataCache[Index].ValueByName[fldTelegramFileHeight] := Image.Graphic.Height;
                        end;
                      FileName := FTempDirectory + FileName;
                      if Image.SaveToFile(FileName) then
                        Strings.Append(FileName)
                      else
                        begin
                          {$IFDEF DeDEBUG}
                          Funcs.WriteLog('File with ID %d not saved!', [VarToInt(DataCache[Index].ValueByName[fldTelegramFileID])], False, 'telegram');
                          {$ENDIF}
                        end;
                      if VarIsNullOrEmpty(DataCache[Index].ValueByName[fldTelegramFileSize]) and FileExists(FileName) then
                        begin
                          Stream := TFileStream.Create(FileName, fmOpenRead);
                          try
                            DataCache[Index].ValueByName[fldTelegramFileSize] := Stream.Size;
                          finally
                            Stream.Free;
                          end;
                        end;
                      DataCache[Index].ValueByName[fldTelegramFileCode] := EmptyStr;
                    end;
                finally
                  Image.Free;
                end;
                if PhotoOnly then
                  ResponseText := BotSendPhoto(
                    TCacheItem(CacheItem).ValueByName[fldTelegramMessageOwner + '.' + fldTelegramContactToken],
                    TCacheItem(CacheItem).ValueByName[fldTelegramMessageChat + '.' + fldTelegramChatContact + '.' + fldTelegramContactCode],
                    Trim(Strings.Text),
                    MessageText, iif((Pos('<', MessageText) <> 0) and (Pos('</', MessageText) <> 0), 'html', EmptyStr),
                    TCacheItem(CacheItem).ValueByName[fldTelegramMessageParent + '.' + fldTelegramMessageCode])
                else
                  ResponseText := BotSendDocument(
                    TCacheItem(CacheItem).ValueByName[fldTelegramMessageOwner + '.' + fldTelegramContactToken],
                    TCacheItem(CacheItem).ValueByName[fldTelegramMessageChat + '.' + fldTelegramChatContact + '.' + fldTelegramContactCode],
                    Trim(Strings.Text),
                    MessageText, iif((Pos('<', MessageText) <> 0) and (Pos('</', MessageText) <> 0), 'html', EmptyStr),
                    TCacheItem(CacheItem).ValueByName[fldTelegramMessageParent + '.' + fldTelegramMessageCode]);
              finally
                Strings.Free;
              end;
            end;
          ResponseJSON := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;
          ValueJSON := ResponseJSON.Values['ok'];
          if Assigned(ValueJSON) and (ValueJSON is TJSONTrue) then
            begin
              MessageJSON := ResponseJSON.Values['result'] as TJSONObject;
              ValueJSON := MessageJSON.Values['message_id'];
              if Assigned(ValueJSON) then
                begin
                  MessageCode := Trim(ValueJSON.Value);
                  if Length(MessageCode) <> 0 then
                    TCacheItem(CacheItem).ValueByName[fldTelegramMessageCode] := MessageCode;
                end;
              ValueJSON := MessageJSON.Values['date'];
              if Assigned(ValueJSON) then
                if TryStrToInt64(ValueJSON.Value, UnixDate) then
                  TCacheItem(CacheItem).ValueByName[fldTelegramMessageDate] := UnixToDateTime(UnixDate);

              if VarIsNullOrEmpty(TCacheItem(CacheItem).ValueByName[fldTelegramMessageFrom]) then
                TCacheItem(CacheItem).ValueByName[fldTelegramMessageFrom] := TCacheItem(CacheItem).ValueByName[fldTelegramMessageOwner];

              ValueJSON := MessageJSON.Values['document'];
              // Если есть вложение документа, то ...
              if Assigned(ValueJSON) and (ValueJSON is TJSONObject) then
                ProcessDocumentJSON(ValueJSON as TJSONObject, 1, MessageID, 0,
                  VarToInt(TCacheItem(CacheItem).ValueByName[fldTelegramMessageOwner]));
              ValueJSON := MessageJSON.Values['photo'];
              // Если есть вложение фотографий, то ...
              if Assigned(ValueJSON) and (ValueJSON is TJSONArray) then
                ProcessPhotoJSON(ValueJSON as TJSONArray, MessageID, 0,
                  VarToInt(TCacheItem(CacheItem).ValueByName[fldTelegramMessageOwner]));

              for Index := 0 to Pred(DataCache.Count) do
                if not DataCache.UpdateRecord(DataCache[Index]) then
                  raise ETelegramException.CreateFmt('File with ID %d not updated in database!', [VarToInt(DataCache[Index].ValueByName[fldTelegramFileID])]);

              Result := True;
            end
          else
            raise ETelegramException.CreateFmt('Message with ID %d was not sent because the server replied %s!', [MessageID, QuotedStr(ResponseText)]);
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Error sending message: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := False;
        end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('File table %s not found!', [GUIDToString(guidTelegramFile)], False, 'telegram');
      {$ENDIF}
    end;
end;

function TTelegramManager.SendMessageUpdateCacheItem(CacheItem: TObject): Boolean;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  ResponseText, MessageText, FileName, MessageCode: string;
  ResponseJSON, MessageJSON: TJSONObject;
  ValueJSON: TJSONValue;
  Strings: TStrings;
  PhotoOnly: Boolean;
  Image: TDeImage;
  Index, MessageID: Integer;
  Stream: TStream;
  UnixDate: Int64;
begin
  Result := False;
  Assert(Assigned(CacheItem) and (CacheItem is TCacheItem));
  TableMeta := MetaData.TablesList.FindByGUID(guidTelegramFile);
  if Assigned(TableMeta) then
    try
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          MessageID := TCacheItem(CacheItem).ValueByName[fldTelegramMessageID];
          MessageCode := TCacheItem(CacheItem).ValueByName[fldTelegramMessageCode];
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramFileMessage), opEQ, MessageID);
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramFileCode), opIs, Null);
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldTelegramFileData), opIsNot, Null);
          FilterItem.AddOperation(opAnd);
          DataCache.LoadData(FilterItem);
          FilterItem := nil;
          MessageText := VarToStr(TCacheItem(CacheItem).ValueByName[fldTelegramMessageMessage]);
          if DataCache.Count <> 0 then
            raise ETelegramException.CreateFmt('You can''t change files for message with ID %d!', [MessageID]);
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
      if GetMessageFileCount(MessageID) = 0 then
        ResponseText := BotEditMessageText(
            TCacheItem(CacheItem).ValueByName[fldTelegramMessageOwner + '.' + fldTelegramContactToken],
            TCacheItem(CacheItem).ValueByName[fldTelegramMessageChat + '.' + fldTelegramChatContact + '.' + fldTelegramContactCode],
            MessageCode, MessageText, iif((Pos('<', MessageText) <> 0) and (Pos('</', MessageText) <> 0), 'html', EmptyStr))
      else
        ResponseText := BotEditMessageCaption(
            TCacheItem(CacheItem).ValueByName[fldTelegramMessageOwner + '.' + fldTelegramContactToken],
            TCacheItem(CacheItem).ValueByName[fldTelegramMessageChat + '.' + fldTelegramChatContact + '.' + fldTelegramContactCode],
            MessageCode, MessageText, iif((Pos('<', MessageText) <> 0) and (Pos('</', MessageText) <> 0), 'html', EmptyStr));

      ResponseJSON := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;
      ValueJSON := ResponseJSON.Values['ok'];
      if Assigned(ValueJSON) and (ValueJSON is TJSONTrue) then
        begin
          MessageJSON := ResponseJSON.Values['result'] as TJSONObject;

          ValueJSON := MessageJSON.Values['date'];
          if Assigned(ValueJSON) then
            if TryStrToInt64(ValueJSON.Value, UnixDate) then
              TCacheItem(CacheItem).ValueByName[fldTelegramMessageDate] := UnixToDateTime(UnixDate);

          if VarIsNullOrEmpty(TCacheItem(CacheItem).ValueByName[fldTelegramMessageFrom]) then
            TCacheItem(CacheItem).ValueByName[fldTelegramMessageFrom] := TCacheItem(CacheItem).ValueByName[fldTelegramMessageOwner];

          ValueJSON := MessageJSON.Values['document'];
          // Если есть вложение документа, то ...
          if Assigned(ValueJSON) and (ValueJSON is TJSONObject) then
            ProcessDocumentJSON(ValueJSON as TJSONObject, 1, MessageID, 0,
              VarToInt(TCacheItem(CacheItem).ValueByName[fldTelegramMessageOwner]));
          ValueJSON := MessageJSON.Values['photo'];
          // Если есть вложение фотографий, то ...
          if Assigned(ValueJSON) and (ValueJSON is TJSONArray) then
            ProcessPhotoJSON(ValueJSON as TJSONArray, MessageID, 0,
              VarToInt(TCacheItem(CacheItem).ValueByName[fldTelegramMessageOwner]));

          Result := True;
        end
      else
        raise ETelegramException.CreateFmt('Message with ID %d was not sent because the server replied %s!', [MessageID, QuotedStr(ResponseText)]);
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Error updating message: %s', [E.Message], False, 'telegram');
          {$ENDIF}
          Result := False;
        end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('File table %s not found!', [GUIDToString(guidTelegramFile)], False, 'telegram');
      {$ENDIF}
    end;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('uTelegramManager unit initialization ...');

finalization
  DebugLog('uTelegramManager unit finalization ...');
{$ENDIF}

end.

