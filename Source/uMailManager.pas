{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}

unit uMailManager;

interface

uses Windows, SysUtils, IdSMTP, IdPOP3, IdEMailAddress, IdHeaderList,
     IdMessageParts;

type
  TMailManager = class
  private
    FMailAgent: string;
    FMailReceived: string;
    FTempDirectory: string;
    FDatabaseRecevied: string;
    FDatabaseText: string;
    FLocalText: string;
    FRemoteReceived: string;
    FRemoteText: string;
    FReceivedHeader1: string;
    FReceivedHeader2: string;
    function AppendMessageContactID(const MessageID, ContactID, ContactTypeID: Integer): Boolean;
    function AppendMessageContactByAddresses(const MessageID, ContactTypeID: Integer; Addresses: string): Integer;
    function CreateSMTP(const SenderID: Integer; var RecipientSenderID, RecipientReplyID: Integer; var ReadingNotification, DeliveryNotification: Boolean): TIdSMTP;
    function PrepareMailAddress(const ContactID: Integer): string;
    function ReadMessageContact(const MessageID, ContactTypeID: Integer): string;
    procedure ReadMessageContacts(const MessageID, ContactTypeID: Integer; List: TIdEMailAddressList);
    function ReadMessageAttachmentCount(const MessageID: Integer): Integer;
    function ReadMessageAttachments(const MessageID: Integer; List: TIdMessageParts): Integer;
    function TextToAcceptString(const Text: string): string;
    function CreateMessageGUID: string;
    function PrepareMessageGUID(const Text: string): string;
    function ReadMessageGUID(const MessageID: Integer): string;
    function FindMessageByGUID(const AccountID: Integer; const MessageGUID: string): Variant;
    procedure MailMessageInitializeISO(var VHeaderEncoding: Char; var VCharSet: string);
    procedure BuildReceivedMessageHeaders(const MessageID: Integer; List: TIdHeaderList);
    function InternalSendMail(const AccountIDs: Variant): Variant;
    function CreatePOP3(const ReceiverID: Integer; var KeepMessage: Boolean): TIdPOP3;
    function WriteMessageAttachments(const MessageID: Integer; const DateTime: TDateTime; List: TIdMessageParts): Integer;
    function ConvertTextFromHTML(const HTML: string): string;
    function SearchMessageText(List: TIdMessageParts; var Text: string): Boolean;
    function InternalReceiveMail(const AccountIDs: Variant): Variant;
    function InternalReceiveHeader(const AccountIDs: Variant): Variant;
  public
    constructor Create;
    class function Contact(const Text: string): Variant;
    class function SendMail(const AccountIDs: Variant): Variant;
    class function ReceiveMail(const AccountIDs: Variant; const HeaderOnly: Boolean): Variant;
  end;

implementation

uses Variants, StrUtils, DateUtils, IOUtils, Classes, IdGlobal, IdMessage, Vcl.Forms,
     IdSSLOpenSSL, IdExplicitTLSClientServerBase, IdText, IdAttachmentFile,
     {$IFDEF DEBUG}DeLog,{$ENDIF}
     DeTypes, Funcs, Security, DeMeta, DeMetadata, DataCacheUnit, DeControls,
     DataUnit, ConnectOperationsUnit;

{ TMailManager }

constructor TMailManager.Create;
var
  VersionMS, VersionLS: Cardinal;
  TableMeta: TTableMeta;
  VersionInfo: TOSVersionInfo;
  MSSQLInformation: TMSSQLInformation;
  InterbaseInformation: TInterbaseInformation;
  Value: string;
  Index: Integer;
begin
  FMailAgent := UpperCase(cApplicationName) + ', version ';
  if GetFileVersion(GetModuleName(hInstance), VersionMS, VersionLS) then
    begin
      FMailReceived := Format('%u.%u.%u.%u', [HiWord(VersionMS), LoWord(VersionMS), HiWord(VersionLS), LoWord(VersionLS)]);
      FMailAgent := FMailAgent + FMailReceived;
      FMailReceived := UpperCase(cApplicationName) + ' (' + FMailReceived + ')';
    end
  else
    begin
      FMailAgent := FMailAgent + '1.0a';
      FMailReceived := UpperCase(cApplicationName) + ' (1.0.0.0)'
    end;
  {$IFDEF WIN32}
  FMailAgent := FMailAgent + ', x86';
  {$ENDIF}
  {$IFDEF WIN64}
  FMailAgent := FMailAgent + ', x64';
  {$ENDIF}
  FTempDirectory := TPath.GetTempPath;
  if Length(FTempDirectory) <> 0 then
    FTempDirectory := IncludeTrailingBackslash(FTempDirectory);
  TableMeta := MetaData.TablesList.FindByGUID(guidMessage);
  if Assigned(TableMeta) and Assigned(TableMeta.Database) then
    begin
      case TableMeta.Database.DatabaseType of
        dtMSSQL: { Microsoft SQL Server }
          begin
            FDatabaseRecevied := 'MSSQL';
            if GetMSSQLInformation(MSSQLInformation) then
              if Length(MSSQLInformation.ProductName) = 0 then
                FDatabaseRecevied := FDatabaseRecevied + Format(' (%u.%u)', [MSSQLInformation.MajorVersion, MSSQLInformation.MinorVersion])
              else
                FDatabaseRecevied := MSSQLInformation.ProductName;
            FDatabaseRecevied := ReplaceText(FDatabaseRecevied, 'MSSQL', 'Microsoft SQL Server');
          end;
        dtInterbase: { Interbase Server }
          begin
            FDatabaseRecevied := 'Interbase Compatible';
            if GetInterbaseInformation(InterbaseInformation) then
               case InterbaseInformation.InterbaseType of
                 itInterbase:
                   case InterbaseInformation.MajorVersion of
                     13: { 2017 }
                       FDatabaseRecevied := 'Interbase 2017';
                     12: { XE7 }
                       FDatabaseRecevied := 'Interbase XE7';
                     11: { XE3 }
                       FDatabaseRecevied := 'Interbase XE3';
                     10: { XE }
                       FDatabaseRecevied := 'Interbase XE';
                     9: { 2009 }
                       FDatabaseRecevied := 'Interbase 2009';
                     8: { 2007 }
                       begin
                         FDatabaseRecevied := 'Interbase 2007';
                         if InterbaseInformation.MinorVersion = 1 then
                           case InterbaseInformation.ReleaseVersion of
                            0: { SP2 }
                              FDatabaseRecevied := FDatabaseRecevied + 'SP2';
                            1: { SP3 }
                              FDatabaseRecevied := FDatabaseRecevied + 'SP3';
                           end;
                       end;
                   else
                     FDatabaseRecevied := FDatabaseRecevied + Format(' (%u.%u)', [InterbaseInformation.MajorVersion, InterbaseInformation.MinorVersion]);
                   end;
                 itFirebird:
                   FDatabaseRecevied := Format('Firebird (%u.%u)', [InterbaseInformation.MajorVersion, InterbaseInformation.MinorVersion]);
               else
                 FDatabaseRecevied := FDatabaseRecevied + Format(' (%u.%u)', [InterbaseInformation.MajorVersion, InterbaseInformation.MinorVersion]);
               end;
          end;
      else
        FDatabaseRecevied := FMailReceived;
      end;
      Value := Trim(TableMeta.Database.Server);
      Index := Pos(':', Value);
      if Index <> 0 then Value := Trim(Copy(Value, 1, Pred(Index)));
      Index := Pos(',', Value);
      if Index <> 0 then Value := Trim(Copy(Value, 1, Pred(Index)));
      if (Length(Value) = 0) or SameText(Value, '.') then
        Value := 'localhost';
      FDatabaseText := Value;
      if SameText(FDatabaseText, 'localhost') then
        FDatabaseText := FDatabaseText + ' ([127.0.0.1])'
      else
        begin
          Value := GetHostIP(FDatabaseText);
          if Length(Value) <> 0 then
            if SameText(Value, FDatabaseText) then
              FDatabaseText := '([' + Value + '])'
            else
              FDatabaseText := FDatabaseText + ' ([' + Value + '])';
        end;
      FDatabaseText := Trim(FDatabaseText);
    end;
  FLocalText := LowerCase(GetComputerNetName);
  Value := GetHostIP(FLocalText);
  if Length(Value) <> 0 then
    FLocalText := FLocalText + ' ([' + Value + '])';
  FLocalText := Trim(FLocalText);
  if GetSystemMetrics(SM_REMOTESESSION) <> 0 then
    begin
      FRemoteText := GetWTSClientName;
      Value := GetWTSClientIP;
      if Length(Value) <> 0 then
        if (Length(FRemoteText) = 0) or SameText(FRemoteText, Value) then
          FRemoteText := '([' + Value + '])'
        else
          FRemoteText := FRemoteText + ' ([' + Value + '])';
      FRemoteText := Trim(FRemoteText);
      FRemoteReceived := 'Remote Desktop';
      VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
      if GetVersionEx(VersionInfo) then
        FRemoteReceived := FRemoteReceived + Format(' (%u.%u.%u)', [VersionInfo.dwMajorVersion, VersionInfo.dwMinorVersion, VersionInfo.dwBuildNumber]);
    end;
  if Length(FRemoteText) = 0 then
    if Length(FDatabaseText) = 0 then
      begin
        FReceivedHeader1 := 'from localhost ([127.0.0.2])';
        if Length(FLocalText) <> 0 then
          FReceivedHeader1 := FReceivedHeader1 + ' by ' + FLocalText;
        if Length(FMailReceived) <> 0 then
          FReceivedHeader1 := FReceivedHeader1 + ' with ' + FMailReceived;
      end
    else
      begin
        FReceivedHeader1 := 'from ' + FDatabaseText;
        if Length(FLocalText) <> 0 then
          FReceivedHeader1 := FReceivedHeader1 + ' by ' + FLocalText;
        if Length(FDatabaseRecevied) <> 0 then
          FReceivedHeader1 := FReceivedHeader1 + ' with ' + FDatabaseRecevied;
      end
  else
    if Length(FDatabaseText) = 0 then
      begin
        FReceivedHeader1 := 'from ' + FRemoteText;
        if Length(FLocalText) <> 0 then
          FReceivedHeader1 := FReceivedHeader1 + ' by ' + FLocalText;
        if Length(FRemoteReceived) <> 0 then
          FReceivedHeader1 := FReceivedHeader1 + ' with ' + FRemoteReceived;
      end
    else
      begin
        FReceivedHeader1 := 'from ' + FDatabaseText + ' by ' + FRemoteText;
        if Length(FDatabaseRecevied) <> 0 then
          FReceivedHeader1 := FReceivedHeader1 + ' with ' + FDatabaseRecevied;
        FReceivedHeader2 := 'from ' + FRemoteText + ' by ';
        if Length(FLocalText) = 0 then
          FReceivedHeader2 := FReceivedHeader2 + 'localhost ([127.0.0.3])'
        else
          FReceivedHeader2 := FReceivedHeader2 + FLocalText;
        if Length(FRemoteReceived) <> 0 then
          FReceivedHeader1 := FReceivedHeader1 + ' with ' + FRemoteReceived;
      end;
end;

function TMailManager.AppendMessageContactID(const MessageID, ContactID, ContactTypeID: Integer): Boolean;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  CacheItem: TCacheItem;
  FilterItem: TFilterItem;
begin
  TableMeta := MetaData.TablesList.FindByGUID(guidRecipient);
  Result := Assigned(TableMeta) and Assigned(TableMeta);
  if Result then
    begin
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldRecipientMessage), opEQ, MessageID);
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldRecipientContact), opEQ, ContactID);
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldRecipientType), opEQ, ContactTypeID);
          FilterItem.AddOperation(opAnd);
          FilterItem.AddOperation(opAnd);
          DataCache.LoadData(FilterItem, fsFull);
          FilterItem := nil;
          if DataCache.Count = 0 then
            begin
              CacheItem := DataCache.AddNewItem;
              DataCache.DataManager.PrepareRecord(CacheItem);
              CacheItem.ValueByName[fldRecipientMessage] := MessageID;
              CacheItem.ValueByName[fldRecipientContact] := ContactID;
              CacheItem.ValueByName[fldRecipientType] := ContactTypeID;
              Result := False;
              if DataCache.DataManager.CanInsertRecord(CacheItem) then
                if DataCache.DataManager.InsertRecord(CacheItem) then
                  Result := True
                else
                  begin
                    {$IFDEF DeDEBUG}
                    Funcs.WriteLog('Append mail contact error: %s', [DataCache.DataManager.Errors.GetMessage], False, 'mail');
                    {$ENDIF}
                  end;
            end;
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    end;
end;

function TMailManager.AppendMessageContactByAddresses(const MessageID, ContactTypeID: Integer; Addresses: string): Integer;
var
  MailContact, WorkContact: Variant;
  Index: Integer;
begin
  Result := 0;
  MailContact := Contact(Addresses);
  if not VarIsNullOrEmpty(MailContact) then
    if VarIsArray(MailContact) then
      for Index := VarArrayLowBound(MailContact, 1) to VarArrayHighBound(MailContact, 1) do
        begin
          WorkContact := VarArrayGet(MailContact, [Index]);
          if not VarIsNullOrEmpty(WorkContact) then
            if AppendMessageContactID(MessageID, WorkContact, ContactTypeID) then
              Inc(Result);
        end
    else
      if AppendMessageContactID(MessageID, MailContact, ContactTypeID) then
        Inc(Result);
end;

function TMailManager.CreateSMTP(const SenderID: Integer; var RecipientSenderID, RecipientReplyID: Integer; var ReadingNotification, DeliveryNotification: Boolean): TIdSMTP;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  Value: string;
begin
  Result := nil;
  TableMeta := MetaData.TablesList.FindByGUID(guidSender);
  if Assigned(TableMeta) then
    begin
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldSenderID), opEQ, SenderID);
          DataCache.LoadData(FilterItem, fsFull);
          FilterItem := nil;
          if DataCache.Count = 1 then
            begin
              Result := TIdSMTP.Create(nil);
              try
                Result.Host := DataCache[0].ValueByName[fldSenderHost];
                Result.Port := DataCache[0].ValueByName[fldSenderPort];
                Value := DataCache[0].ValueByName[fldSenderLogin];
                if Length(Value) <> 0 then
                  begin
                    Result.UserName := Value;
                    Result.Password := DataCache[0].ValueByName[fldSenderPassword];
                  end;
                if VarToInt(DataCache[0].ValueByName[fldSenderSSL]) <> 0 then
                  begin
                    Result.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Result);
                    Result.UseTLS := utUseRequireTLS;
                  end;
                if VarIsNull(DataCache[0].ValueByName[fldSenderSender]) then
                  RecipientSenderID := 0
                else
                  RecipientSenderID := DataCache[0].ValueByName[fldSenderSender];
                if VarIsNull(DataCache[0].ValueByName[fldSenderReply]) then
                  RecipientReplyID := 0
                else
                  RecipientReplyID := DataCache[0].ValueByName[fldSenderReply];
                DeliveryNotification := VarToInt(DataCache[0].ValueByName[fldSenderDelivery]) <> 0;
                ReadingNotification := VarToInt(DataCache[0].ValueByName[fldSenderReading]) <> 0;
                Result.MailAgent := FMailAgent;
              except
                FreeAndNil(Result);
                raise;
              end;
            end;
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    end;
end;

function TMailManager.PrepareMailAddress(const ContactID: Integer): string;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  Value: string;
begin
  Result := EmptyStr;
  TableMeta := MetaData.TablesList.FindByGUID(guidContact);
  if Assigned(TableMeta) and Assigned(TableMeta) then
    begin
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldContactID), opEQ, ContactID);
          DataCache.Fields.ChangeStages([fldContactName, fldContactMail], fsBase);
          DataCache.LoadData(FilterItem);
          FilterItem := nil;
          if DataCache.Count = 0 then
            begin
              {$IFDEF DeDEBUG}
              Funcs.WriteLog('Mail contact identifier %d not found', [ContactID], False, 'mail');
              {$ENDIF}
            end
          else
            begin
              Result := Trim(DataCache[0].ValueByName[fldContactMail]);
              if Length(Result) <> 0 then
                begin
                  Value := Trim(DataCache[0].ValueByName[fldContactName]);
                  if Length(Value) <> 0 then
                    Result := '"' + Value + '" <' + Result + '>';
                end;
            end;
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    end;
end;

function TMailManager.ReadMessageContact(const MessageID, ContactTypeID: Integer): string;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  RecipientIDs: Variant;
  Index: Integer;
  AddressItem: TIdEMailAddressItem;
begin
  Result := EmptyStr;
  TableMeta := MetaData.TablesList.FindByGUID(guidRecipient);
  if Assigned(TableMeta) then
    begin
      RecipientIDs := Null;
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldRecipientMessage), opEQ, MessageID);
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldRecipientType), opEQ, ContactTypeID);
          FilterItem.AddOperation(opAnd);
          DataCache.Fields.ChangeStages([fldRecipientCreated, fldRecipientModified], fsBlob);
          DataCache.LoadData(FilterItem);
          FilterItem := nil;
          case DataCache.Count of
            0: { Нет адресатов };
            1: { Только один адресат }
              RecipientIDs := DataCache[0].ValueByName[fldRecipientContact];
          else
            RecipientIDs := VarArrayCreate([0, Pred(DataCache.Count)], varInteger);
            for Index := 0 to Pred(DataCache.Count) do
              VarArrayPut(RecipientIDs, DataCache[Index].ValueByName[fldRecipientContact], [Index]);
          end;
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
      if not VarIsNull(RecipientIDs) then
        begin
          TableMeta := MetaData.TablesList.FindByGUID(guidContact);
          if Assigned(TableMeta) then
            begin
              FilterItem := TFilterItem.Create;
              try
                DataCache := TDataCache.Create(TableMeta);
                try
                  if VarIsArray(RecipientIDs) then
                    FilterItem.AddCondition(TableMeta.Fields.FindByName(fldContactID), opIn, RecipientIDs)
                  else
                    FilterItem.AddCondition(TableMeta.Fields.FindByName(fldContactID), opEQ, RecipientIDs);
                  DataCache.Fields.ChangeStages([fldContactCreated, fldContactModified], fsBlob);
                  DataCache.LoadData(FilterItem);
                  FilterItem := nil;
                  if DataCache.Count <> 0 then
                    begin
                      AddressItem := TIdEMailAddressItem.Create;
                      try
                        AddressItem.Address := DataCache[0].ValueByName[fldContactMail];
                        AddressItem.Name := DataCache[0].ValueByName[fldContactName];
                        Result := AddressItem.Text;
                      finally
                        AddressItem.Free;
                      end;
                      {$IFDEF DeDEBUG}
                      if DataCache.Count <> 1 then
                        Funcs.WriteLog('Is need one contact with type identifier %d. Retrieved %d contacts of this type for a message with identifier %d. Only the first one from the list will be used.', [ContactTypeID, DataCache.Count, MessageID], False, 'mail');
                      {$ENDIF}
                    end;
                finally
                  DataCache.Free;
                end;
              finally
                FilterItem.Free;
              end;
            end
          else
            begin
              {$IFDEF DeDEBUG}
              Funcs.WriteLog('Contact table %s not found for message identifier %d and type identifier %d!', [GUIDToString(guidContact), MessageID, ContactTypeID], False, 'mail');
              {$ENDIF}
            end;
        end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Message contact table %s not found for message identifier %d and type identifier %d!', [GUIDToString(guidRecipient), MessageID, ContactTypeID], False, 'mail');
      {$ENDIF}
    end;
end;

procedure TMailManager.ReadMessageContacts(const MessageID, ContactTypeID: Integer; List: TIdEMailAddressList);
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  RecipientIDs: Variant;
  Index: Integer;
  AddressItem: TIdEMailAddressItem;
begin
  TableMeta := MetaData.TablesList.FindByGUID(guidRecipient);
  if Assigned(TableMeta) then
    begin
      RecipientIDs := Null;
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldRecipientMessage), opEQ, MessageID);
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldRecipientType), opEQ, ContactTypeID);
          FilterItem.AddOperation(opAnd);
          DataCache.Fields.ChangeStages([fldRecipientCreated, fldRecipientModified], fsBlob);
          DataCache.LoadData(FilterItem);
          FilterItem := nil;
          case DataCache.Count of
            0: { Нет адресатов };
            1: { Только один адресат }
              RecipientIDs := DataCache[0].ValueByName[fldRecipientContact];
          else
            RecipientIDs := VarArrayCreate([0, Pred(DataCache.Count)], varInteger);
            for Index := 0 to Pred(DataCache.Count) do
              VarArrayPut(RecipientIDs, DataCache[Index].ValueByName[fldRecipientContact], [Index]);
          end;
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
      List.Clear;
      if not VarIsNull(RecipientIDs) then
        begin
          TableMeta := MetaData.TablesList.FindByGUID(guidContact);
          if Assigned(TableMeta) then
            begin
              FilterItem := TFilterItem.Create;
              try
                DataCache := TDataCache.Create(TableMeta);
                try
                  if VarIsArray(RecipientIDs) then
                    FilterItem.AddCondition(TableMeta.Fields.FindByName(fldContactID), opIn, RecipientIDs)
                  else
                    FilterItem.AddCondition(TableMeta.Fields.FindByName(fldContactID), opEQ, RecipientIDs);
                  DataCache.Fields.ChangeStages([fldContactCreated, fldContactModified], fsBlob);
                  DataCache.LoadData(FilterItem);
                  FilterItem := nil;
                  for Index := 0 to Pred(DataCache.Count) do
                    begin
                      AddressItem := List.Add;
                      AddressItem.Address := DataCache[Index].ValueByName[fldContactMail];
                      AddressItem.Name := DataCache[Index].ValueByName[fldContactName];
                    end;
                finally
                  DataCache.Free;
                end;
              finally
                FilterItem.Free;
              end;
            end
          else
            begin
              {$IFDEF DeDEBUG}
              Funcs.WriteLog('Contact table %s not found for message identifier %d and type identifier %d!', [GUIDToString(guidContact), MessageID, ContactTypeID], False, 'mail');
              {$ENDIF}
            end;
        end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Message contact table %s not found for message identifier %d and type identifier %d!', [GUIDToString(guidRecipient), MessageID, ContactTypeID], False, 'mail');
      {$ENDIF}
    end;
end;

function TMailManager.ReadMessageAttachmentCount(const MessageID: Integer): Integer;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
begin
  Result := 0;
  TableMeta := MetaData.TablesList.FindByGUID(guidAttachment);
  if Assigned(TableMeta) then
    begin
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldAttachmentMessage), opEQ, MessageID);
          DataCache.Fields.ChangeStages([fldAttachmentCreated, fldAttachmentModified, fldAttachmentFile], fsFull);
          DataCache.LoadData(FilterItem);
          FilterItem := nil;
          Result := DataCache.Count;
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Message attachment table %s not found for message identifier %d!', [GUIDToString(guidAttachment), MessageID], False, 'mail');
      {$ENDIF}
    end;
end;

function TMailManager.ReadMessageAttachments(const MessageID: Integer; List: TIdMessageParts): Integer;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  BlobData: TDeImage;
  Directory, FileName, SaveFileName: string;
  AttachmentFile: TIdAttachmentFile;
  Index: Integer;
  FileBody: Variant;
begin
  Result := 0;
  TableMeta := MetaData.TablesList.FindByGUID(guidAttachment);
  if Assigned(TableMeta) then
    begin
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldAttachmentMessage), opEQ, MessageID);
          DataCache.LoadData(FilterItem);
          FilterItem := nil;
          if DataCache.Count <> 0 then
            begin
              Directory := FTempDirectory + Format('~MA%.8u', [MessageID]);
              if ForceDirectories(Directory) then
                SetFileAttributes(PChar(Directory), GetFileAttributes(PChar(Directory)) or faHidden);
              if Assigned(DM) then DM.RegisterTemporaryFile(Directory);
              Directory := IncludeTrailingBackslash(Directory);
              for Index := 0 to Pred(DataCache.Count) do
                begin
                  FileName := NormalizeFileName(DataCache[Index].ValueByName[fldAttachmentFile]);
                  FileBody := DataCache[Index].ValueByName[fldAttachmentData];
                  if VarIsNull(FileBody) then
                    begin
                      Result:= -1;
                      Break;
                    end;

                  BlobData := TDeImage.Create(nil);
                  try
                    BlobData.Value := FileBody;
                    if Length(FileName) = 0 then
                      FileName := BlobData.OriginalFileName;
                    SaveFileName := Directory + Format('~AF%.8u', [VarToInt(DataCache[Index].ValueByName[fldAttachmentID])]) + ExtractFileExt(FileName);
                    BlobData.SaveToFile(SaveFileName);
                  finally
                    BlobData.Free;
                  end;
                  if Assigned(DM) then DM.RegisterTemporaryFile(SaveFileName);
                  AttachmentFile := TIdAttachmentFile.Create(List, SaveFileName);
                  AttachmentFile.FileName := FileName;
                  Inc(Result);
                end;
            end;
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Message attachment table %s not found for message identifier %d!', [GUIDToString(guidAttachment), MessageID], False, 'mail');
      {$ENDIF}
    end;
end;

function TMailManager.TextToAcceptString(const Text: string): string;
const
  AccentSymbols: PChar = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
var
  Index: Integer;
begin
  Result := EmptyStr;
  for Index := 1 to Length(Text) do
    if Pos(Text[Index], AccentSymbols) <> 0 then
      Result := Result + Text[Index];
end;

function TMailManager.CreateMessageGUID: string;
var
  GUID: TGUID;
begin
  Result := TextToAcceptString(GetComputerNetName);
  if Length(Result) <> 0 then Result := '@' + Result;
  CreateGUID(GUID);
  Result := ReplaceText(ReplaceText(ReplaceText(GUIDToString(GUID), '{', ''), '}', ''), '-', '') + Result;
end;

function TMailManager.PrepareMessageGUID(const Text: string): string;
begin
  Result := Trim(Text);
  while (Length(Result) <> 0) and (Result[1] = '<') do
    Delete(Result, 1, 1);
  while (Length(Result) <> 0) and (Result[Length(Result)] = '>') do
    Delete(Result, Length(Result), 1);
  Result := Trim(Result);
end;

function TMailManager.ReadMessageGUID(const MessageID: Integer): string;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
begin
  Result := EmptyStr;
  TableMeta := MetaData.TablesList.FindByGUID(guidMessage);
  if Assigned(TableMeta) then
    begin
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldMessageID), opEQ, MessageID);
          DataCache.Fields.ChangeStages([fldMessageGUID], fsKey);
          DataCache.LoadData(FilterItem);
          FilterItem := nil;
          if DataCache.Count <> 0 then
            Result := Trim(DataCache[0].ValueByName[fldMessageGUID]);
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    end;
end;

function TMailManager.FindMessageByGUID(const AccountID: Integer; const MessageGUID: string): Variant;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
begin
  Result := Unassigned;
  TableMeta := MetaData.TablesList.FindByGUID(guidMessage);
  if Assigned(TableMeta) then
    begin
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldMessageAccount), opEQ, AccountID);
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldMessageGUID), opEQ, MessageGUID);
          FilterItem.AddOperation(opAnd);
          DataCache.LoadData(FilterItem, fsKey);
          FilterItem := nil;
          if DataCache.Count = 0 then
            Result := Null
          else
            Result := DataCache[0].ValueByName[fldMessageID];
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    end;
end;

procedure TMailManager.BuildReceivedMessageHeaders(const MessageID: Integer; List: TIdHeaderList);
var
  Value: string;
begin
  if Length(FReceivedHeader1) <> 0 then
    begin
      Value := FReceivedHeader1 + ' id ' + Format('1%s', [IntToStr36(MessageID, 8)]) + '; ' + LocalDateTimeToGMT(IncSecond(Now, -1));
      List.AddValue('Received', Value);
    end;
  if Length(FReceivedHeader2) <> 0 then
    begin
      Value := FReceivedHeader2 + ' id ' + Format('2%s', [IntToHex(GetTickCount, 8)]) + '; ' + LocalDateTimeToGMT(Now);
      List.AddValue('Received', Value);
    end;
end;

procedure TMailManager.MailMessageInitializeISO(var VHeaderEncoding: Char; var VCharSet: string);
begin
  VHeaderEncoding := 'B';
  VCharSet := 'utf-8';
end;

class function TMailManager.Contact(const Text: string): Variant;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  ContactIDs: TStrings;
  Index: Integer;
  List: TIdEMailAddressList;
  ContactID, ContactIndex: Integer;
  CacheItem: TCacheItem;
begin
  Result := Unassigned;
  TableMeta := MetaData.TablesList.FindByGUID(guidContact);
  if Assigned(TableMeta) then
    try
      List := TIdEMailAddressList.Create(nil);
      try
        List.EMailAddresses := Text;
        if List.Count = 0 then
          begin
            Result := Null;
            {$IFDEF DeDEBUG}
            if Length(Text) <> 0 then
              Funcs.WriteLog('Contact %s not found.', [QuotedStr(Text)], False, 'mail');
            {$ENDIF}
          end
        else
          begin
            ContactIDs := TStringList.Create;
            try
              for Index := 0 to Pred(List.Count) do
                begin
                  ContactIndex := -1;
                  if Assigned(UserSession) then
                    begin
                      FilterItem := TFilterItem.Create;
                      try
                        DataCache := TDataCache.Create(TableMeta);
                        try
                          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldContactMail), opEQ, List[Index].Address);
                          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldContactUser), opEQ, UserSession.ID);
                          FilterItem.AddOperation(opAnd);
                          DataCache.Fields.ChangeStages([fldContactCreated, fldContactModified], fsBlob);
                          DataCache.LoadData(FilterItem);
                          FilterItem := nil;
                          if DataCache.Count <> 0 then
                            begin
                              ContactID := DataCache[0].ValueByName[fldContactID];
                              ContactIndex := ContactIDs.Add(IntToStr(ContactID));
                            end;
                        finally
                          DataCache.Free;
                        end;
                      finally
                        FilterItem.Free;
                      end;
                    end;
                  if ContactIndex = -1 then
                    begin
                      FilterItem := TFilterItem.Create;
                      try
                        DataCache := TDataCache.Create(TableMeta);
                        try
                          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldContactMail), opEQ, List[Index].Address);
                          DataCache.SortList.Add(TableMeta.Fields.FindByName(fldContactUser).ID);
                          DataCache.LoadData(FilterItem);
                          FilterItem := nil;
                          if DataCache.Count = 0 then
                            begin
                              CacheItem := DataCache.AddNewItem;
                              DataCache.DataManager.PrepareRecord(CacheItem);
                              if Length(List[Index].Name) <> 0 then
                                CacheItem.ValueByName[fldContactName] := List[Index].Name;
                              CacheItem.ValueByName[fldContactMail] := List[Index].Address;
                              if Assigned(UserSession) then
                                CacheItem.ValueByName[fldContactUser] := UserSession.ID;
                              if DataCache.DataManager.CanInsertRecord(CacheItem) then
                                if DataCache.DataManager.InsertRecord(CacheItem) then
                                  begin
                                    ContactID := DataCache[0].ValueByName[fldContactID];
                                    ContactIDs.Append(IntToStr(ContactID));
                                  end
                                else
                                  begin
                                    {$IFDEF DeDEBUG}
                                    Funcs.WriteLog('Append contact %s error: %s', [QuotedStr(List[Index].Text), DataCache.DataManager.Errors.GetMessage], False, 'mail');
                                    {$ENDIF}
                                  end;
                            end
                          else
                            begin
                              ContactID := DataCache[0].ValueByName[fldContactID];
                              ContactIndex := ContactIDs.Add(IntToStr(ContactID));
                            end;
                        finally
                          DataCache.Free;
                        end;
                      finally
                        FilterItem.Free;
                      end;
                    end;
                end;
              case ContactIDs.Count of
                0: { Контакты не найдены }
                  Result := Null;
                1: { Найден один контакт }
                  if TryStrToInt(ContactIDs[0], ContactID) then
                    Result := ContactID
                  else
                    Result := Null;
              else
                Result := VarArrayCreate([0, Pred(ContactIDs.Count)], varVariant);
                for Index := 0 to Pred(ContactIDs.Count) do
                  if TryStrToInt(ContactIDs[Index], ContactID) then
                    VarArrayPut(Result, ContactID, [Index])
                  else
                    VarArrayPut(Result, Null, [Index]);
              end;
            finally
              ContactIDs.Free;
            end;
          end;
      finally
        List.Free;
      end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Contact error: %s', [E.Message], False, 'mail');
          {$ENDIF}
          Result := Null;
        end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      Funcs.WriteLog('Contact table %s not found!', [GUIDToString(guidContact)], False, 'mail');
      {$ENDIF}
    end;
end;

class function TMailManager.SendMail(const AccountIDs: Variant): Variant;
begin
  with Self.Create do
    try
      Result := InternalSendMail(AccountIDs);
    finally
      Free;
    end;
end;

function TMailManager.InternalSendMail(const AccountIDs: Variant): Variant;
var
  AccountTableMeta, MessageTableMeta: TTableMeta;
  AccountDataCache, MessageDataCache: TDataCache;
  AccountFilterItem, MessageFilterItem: TFilterItem;
  FieldMeta: TFieldMeta;
  Index, Count, AttachmentCount, MessageIndex, MessageID, MailPriority: Integer;
  RecipientSenderID, RecipientFromID, RecipientReplyID: Integer;
  RecipientSenderText, RecipientFromText, RecipientReplyText: string;
  MessageText, CompanyText, FileName: string;
  RecipientReadingText, RecipientDeliveryText: string;
  ReadingNotification, DeliveryNotification: Boolean;
  SMTP: TIdSMTP;
  MailMessage: TIdMessage;
  BlobData: TDeImage;
  BodyText: TIdText;
  MessageIDs: TStrings;
begin
  Result := Unassigned;
  if Assigned(MetaData) and Assigned(MetaData.TablesList) then
    try
      AccountTableMeta := MetaData.TablesList.FindByGUID(guidAccount);
      MessageTableMeta := MetaData.TablesList.FindByGUID(guidMessage);
      if Assigned(AccountTableMeta) and Assigned(MessageTableMeta) then
        begin
          AccountDataCache := TDataCache.Create(AccountTableMeta);
          try
            AccountFilterItem := TFilterItem.Create;
            try
              Count := 0;
              // Если аккаунты указаны, то ...
              if not VarIsNullOrEmpty(AccountIDs) then
                begin
                  Inc(Count);
                  FieldMeta := AccountTableMeta.Fields.FindByName(fldAccountID);
                  // Если указан один аккаунт, то ...
                  if not VarIsArray(AccountIDs) then
                    AccountFilterItem.AddCondition(FieldMeta, opEQ, AccountIDs)
                  else
                    AccountFilterItem.AddCondition(FieldMeta, opIN, AccountIDs);
                end;
              FieldMeta := AccountTableMeta.Fields.FindByName(fldAccountSender);
              AccountFilterItem.AddCondition(FieldMeta, opEQ, Null);
              AccountFilterItem.AddOperation(opNot);
              if Count <> 0 then AccountFilterItem.AddOperation(opAnd);
              AccountDataCache.Fields.ChangeStages([fldAccountCreated, fldAccountModified, fldAccountReceiver], fsBlob);
              AccountDataCache.LoadData(AccountFilterItem, fsFull);
              AccountFilterItem := nil;
              MessageIDs := TStringList.Create;
              try
                // Пробегаемся по всем аккаунтам ...
                for Index := 0 to Pred(AccountDataCache.Count) do
                  begin
                    CompanyText := Trim(AccountDataCache[Index].ValueByName[fldAccountCompany]);
                    if VarIsNull(AccountDataCache[Index].ValueByName[fldAccountContact]) then
                      begin
                        RecipientFromID := 0;
                        RecipientFromText := EmptyStr;
                      end
                    else
                      begin
                        RecipientFromID := AccountDataCache[Index].ValueByName[fldAccountContact];
                        RecipientFromText := PrepareMailAddress(RecipientFromID);
                      end;
                    MessageDataCache := TDataCache.Create(MessageTableMeta);
                    try
                      MessageFilterItem := TFilterItem.Create;
                      try
                        FieldMeta := MessageTableMeta.Fields.FindByName(fldMessageAccount);
                        MessageFilterItem.AddCondition(FieldMeta, opEQ, AccountDataCache[Index].ValueByName[fldAccountID]);
                        FieldMeta := MessageTableMeta.Fields.FindByName(fldMessageFolder);
                        MessageFilterItem.AddCondition(FieldMeta, opEQ, 4); // Из папки "Исходящие" ...
                        MessageFilterItem.AddOperation(opAnd);
                        MessageDataCache.LoadData(MessageFilterItem, fsBlob);
                        MessageFilterItem := nil;
                        // Если есть сообщения на отправку, то ...
                        if MessageDataCache.Count <> 0 then
                          begin
                            SMTP := CreateSMTP(AccountDataCache[Index].ValueByName[fldAccountSender], RecipientSenderID, RecipientReplyID, ReadingNotification, DeliveryNotification);
                            try
                              if Assigned(SMTP) then
                                begin
                                  if RecipientSenderID = 0 then
                                    RecipientSenderText := EmptyStr
                                  else
                                    RecipientSenderText := PrepareMailAddress(RecipientSenderID);
                                  if RecipientReplyID = 0 then
                                    RecipientReplyText := EmptyStr
                                  else
                                    RecipientReplyText := PrepareMailAddress(RecipientReplyID);
                                  SMTP.Connect;
                                  try
                                    // Пробегаемся по всем отправляемым письмам в аккаунте ...
                                    for MessageIndex := 0 to Pred(MessageDataCache.Count) do
                                      begin
                                        MessageID := MessageDataCache[MessageIndex].ValueByName[fldMessageID];
                                        // Если дата отправки указана и она в будущем, то пропускаем отправку этого письма до наструпления будущего!!!
                                        if not VarIsNull(MessageDataCache[MessageIndex].ValueByName[fldMessageDate]) then
                                          if MessageDataCache[MessageIndex].ValueByName[fldMessageDate] > Now then
                                            Continue;
                                        if RecipientSenderID <> 0 then
                                          AppendMessageContactID(MessageID, RecipientSenderID, 1);
                                        if RecipientFromID <> 0 then
                                          AppendMessageContactID(MessageID, RecipientFromID, 2);
                                        if RecipientReplyID <> 0 then
                                          AppendMessageContactID(MessageID, RecipientReplyID, 3);
                                        MailMessage := TIdMessage.Create(nil);
                                        try
                                          MailMessage.OnInitializeISO := MailMessageInitializeISO;
                                          BuildReceivedMessageHeaders(MessageID, MailMessage.Headers);
                                          MailMessage.Sender.Text := RecipientSenderText;
                                          MailMessage.From.Text := RecipientFromText;
                                          ReadMessageContacts(MessageID, 3, MailMessage.ReplyTo);
                                          MailMessage.Organization := CompanyText;
                                          // Если нужно уведомление о прочтении, то ...
                                          if ReadingNotification then
                                            begin
                                              RecipientReadingText := ReadMessageContact(MessageID, 7);
                                              if Length(RecipientReadingText) = 0 then
                                                if RecipientReplyID = 0 then
                                                  if RecipientSenderID = 0 then
                                                    if RecipientFromID = 0 then
                                                      begin
                                                        {$IFDEF DeDEBUG}
                                                        Funcs.WriteLog('Could not detecting the address for receiving confirmation of reading the message with identifier %d.', [MessageID], False, 'mail');
                                                        {$ENDIF}
                                                      end
                                                    else
                                                      begin
                                                        AppendMessageContactID(MessageID, RecipientFromID, 7);
                                                        RecipientReadingText := RecipientFromText;
                                                      end
                                                  else
                                                    begin
                                                      AppendMessageContactID(MessageID, RecipientSenderID, 7);
                                                      RecipientReadingText := RecipientSenderText;
                                                    end
                                                else
                                                  begin
                                                    AppendMessageContactID(MessageID, RecipientReplyID, 7);
                                                    RecipientReadingText := RecipientReplyText;
                                                  end;
                                              MailMessage.ReceiptRecipient.Text := RecipientReadingText;
                                            end;
                                          // Если нужно уведомление о доставке, то ...
                                          if DeliveryNotification then
                                            begin
                                              RecipientDeliveryText := ReadMessageContact(MessageID, 8);
                                              if Length(RecipientDeliveryText) = 0 then
                                                if RecipientReplyID = 0 then
                                                  if RecipientSenderID = 0 then
                                                    if RecipientFromID = 0 then
                                                      begin
                                                        {$IFDEF DeDEBUG}
                                                        Funcs.WriteLog('Could not detecting the address for receiving confirmation of delivery the message with identifier %d.', [MessageID], False, 'mail');
                                                        {$ENDIF}
                                                      end
                                                    else
                                                      begin
                                                        AppendMessageContactID(MessageID, RecipientFromID, 8);
                                                        RecipientDeliveryText := RecipientFromText;
                                                      end
                                                  else
                                                    begin
                                                      AppendMessageContactID(MessageID, RecipientSenderID, 8);
                                                      RecipientDeliveryText := RecipientSenderText;
                                                    end
                                                else
                                                  begin
                                                    AppendMessageContactID(MessageID, RecipientReplyID, 8);
                                                    RecipientDeliveryText := RecipientReplyText;
                                                  end;
                                              MailMessage.ExtraHeaders.Values['Return-Receipt-To'] := RecipientDeliveryText;
                                            end;
                                          ReadMessageContacts(MessageID, 4, MailMessage.Recipients);
                                          ReadMessageContacts(MessageID, 5, MailMessage.CCList);
                                          ReadMessageContacts(MessageID, 6, MailMessage.BCCList);
                                          if VarIsNull(MessageDataCache[MessageIndex].ValueByName[fldMessageDate]) then
                                            begin
                                              MessageDataCache[MessageIndex].ValueByName[fldMessageDate] := Now;
                                              if MessageDataCache.DataManager.CanUpdateRecord(MessageDataCache[MessageIndex]) then
                                                if MessageDataCache.DataManager.UpdateRecord(MessageDataCache[MessageIndex]) then
                                                  begin
                                                    MailMessage.Date := MessageDataCache[MessageIndex].ValueByName[fldMessageDate];
                                                    MessageDataCache[MessageIndex].ApplyChanges;
                                                  end
                                                else
                                                  begin
                                                    {$IFDEF DeDEBUG}
                                                    Funcs.WriteLog('Message %d not present and not updating date', [MessageID], False, 'mail');
                                                    {$ENDIF}
                                                  end;
                                            end
                                          else
                                            MailMessage.Date := MessageDataCache[MessageIndex].ValueByName[fldMessageDate];
                                          MailMessage.Subject := MessageDataCache[MessageIndex].ValueByName[fldMessageSubject];
                                          if not VarIsNull(MessageDataCache[MessageIndex].ValueByName[fldMessagePriority]) then
                                            begin
                                              MailPriority := MessageDataCache[MessageIndex].ValueByName[fldMessagePriority];
                                              if MailPriority > 0 then
                                                MailMessage.Priority := mpLow
                                              else
                                                if MailPriority < 0 then
                                                  MailMessage.Priority := mpHigh
                                                else
                                                  MailMessage.Priority := mpNormal;
                                            end;
                                          MessageText := MessageDataCache[MessageIndex].ValueByName[fldMessageGUID];
                                          if Length(MessageText) = 0 then
                                            begin
                                              MessageText := LowerCase(CreateMessageGUID);
                                              MailMessage.ExtraHeaders.Values['Message-ID'] := '<' + MessageText + '>';
                                              MessageDataCache[MessageIndex].ValueByName[fldMessageGUID] := MessageText;
                                              if MessageDataCache.DataManager.CanUpdateRecord(MessageDataCache[MessageIndex]) then
                                                if MessageDataCache.DataManager.UpdateRecord(MessageDataCache[MessageIndex]) then
                                                  MessageDataCache[MessageIndex].ApplyChanges
                                                else
                                                  begin
                                                    {$IFDEF DeDEBUG}
                                                    Funcs.WriteLog('Message %d not present identifier and not updating identifier', [MessageID], False, 'mail');
                                                    {$ENDIF}
                                                  end;
                                            end;
                                          if not VarIsNull(MessageDataCache[MessageIndex].ValueByName[fldMessageParent]) then
                                            begin
                                              MessageText := ReadMessageGUID(MessageDataCache[MessageIndex].ValueByName[fldMessageParent]);
                                              MailMessage.InReplyTo := MessageText;
                                              MailMessage.References := MessageText;
                                            end;
                                          MessageText := Trim(MessageDataCache[MessageIndex].ValueByName[fldMessageText]);

                                          // Одной процедурой "считаем", "проверяем" и "грузим" вложенные файлы
                                          AttachmentCount:= ReadMessageAttachments(MessageID, MailMessage.MessageParts);

                                          if AttachmentCount < 0 then
                                            begin
                                              // "-1" означает сбой загрузки вложенных файлов
                                              MessageDataCache[MessageIndex].ValueByName[fldMessageFolder]:= 6; // Перемещаем в папку "Удаленные" ...
                                            end else
                                            begin
                                              if AttachmentCount = 0 then
                                                begin
                                                  if (Pos('<body', MessageText) = 1) or (Pos('<head', MessageText) = 1) or (Pos('<html', MessageText) = 1) then
                                                    MailMessage.ContentType := 'text/html'
                                                  else
                                                    MailMessage.ContentType := 'text/plain';
                                                  MailMessage.CharSet := 'utf-8';
                                                  MailMessage.Body.Text := MessageText;
                                                end else

                                                begin
                                                  BodyText := TIdText.Create(MailMessage.MessageParts);
                                                  if (Pos('<body', MessageText) = 1) or (Pos('<head', MessageText) = 1) or (Pos('<html', MessageText) = 1) then
                                                    BodyText.ContentType := 'text/html'
                                                  else
                                                    BodyText.ContentType := 'text/plain';
                                                  BodyText.CharSet := 'utf-8';
                                                  BodyText.ContentTransfer := 'base64';
                                                  BodyText.Body.Text := MessageText;
                                                end;

                                                try
                                                  SMTP.Send(MailMessage);

                                                  FileName := FTempDirectory + Format('~MA%.8u.eml', [MessageID]);
                                                  if Assigned(DM) then DM.RegisterTemporaryFile(FileName);
                                                  MailMessage.SaveToFile(FileName);
                                                  BlobData := TDeImage.Create(nil);
                                                  try
                                                    BlobData.LoadFromFile(FileName);
                                                    MessageDataCache[MessageIndex].ValueByName[fldMessageEML] := BlobData.Value;
                                                  finally
                                                    BlobData.Free;
                                                  end;
                                                  MessageDataCache[MessageIndex].ValueByName[fldMessageFolder]:= 5 // Перемещаем в папку "Отправленные" ...
                                                except
                                                  on E: Exception do
                                                    begin
                                                      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar('Ошибка отправки сообщения'+':'+E.Message)));
                                                      MessageDataCache[MessageIndex].ValueByName[fldMessageFolder]:= 6; // Перемещаем в папку "Удаленные" ...
                                                    end;
                                                end;
                                            end;

                                          try
                                            if MessageDataCache.DataManager.CanUpdateRecord(MessageDataCache[MessageIndex]) then
                                              if MessageDataCache.DataManager.UpdateRecord(MessageDataCache[MessageIndex]) then
                                                begin
                                                  MessageDataCache[MessageIndex].ApplyChanges;
                                                  MessageIDs.Append(IntToStr(MessageID));
                                                end
                                              else
                                                begin
                                                  {$IFDEF DeDEBUG}
                                                  Funcs.WriteLog('Message %d not change folder and not updating EML!', [MessageID], False, 'mail');
                                                  {$ENDIF}
                                                end;
                                          except
                                            on E: Exception do
                                              begin
                                                SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar('Ошибка отправки сообщения'+':'+E.Message)));
                                              end;
                                          end;
                                        finally
                                          MailMessage.Free;
                                        end;
                                      end;
                                  finally
                                    SMTP.Disconnect;
                                  end;
                                end;
                            finally
                              FreeAndNil(SMTP);
                            end;
                          end;
                      finally
                        MessageFilterItem.Free;
                      end;
                    finally
                      MessageDataCache.Free;
                    end;
                  end;
                case MessageIDs.Count of
                  0: { Сообщения не отправлены }
                    Result := Null;
                  1: { Отправлено одно сообщение }
                    if TryStrToInt(MessageIDs[0], MessageID) then
                      Result := MessageID
                    else
                      Result := Null;
                else
                  Result := VarArrayCreate([0, Pred(MessageIDs.Count)], varVariant);
                  for Index := 0 to Pred(MessageIDs.Count) do
                    if TryStrToInt(MessageIDs[Index], MessageID) then
                      VarArrayPut(Result, MessageID, [Index])
                    else
                      VarArrayPut(Result, Null, [Index]);
                end;
              finally
                MessageIDs.Free;
              end;
            finally
              AccountFilterItem.Free;
            end;
          finally
            AccountDataCache.Free;
          end;
        end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Send mail error: %s', [E.Message], False, 'mail');
          {$ENDIF}
          Result := Null;
        end;
    end;
end;

function TMailManager.CreatePOP3(const ReceiverID: Integer; var KeepMessage: Boolean): TIdPOP3;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  Value: string;
begin
  Result := nil;
  TableMeta := MetaData.TablesList.FindByGUID(guidReceiver);
  if Assigned(TableMeta) then
    begin
      FilterItem := TFilterItem.Create;
      try
        DataCache := TDataCache.Create(TableMeta);
        try
          FilterItem.AddCondition(TableMeta.Fields.FindByName(fldReceiverID), opEQ, ReceiverID);
          DataCache.LoadData(FilterItem, fsFull);
          FilterItem := nil;
          if DataCache.Count = 1 then
            begin
              Result := TIdPOP3.Create(nil);
              try
                Result.Host := DataCache[0].ValueByName[fldReceiverHost];
                Result.Port := DataCache[0].ValueByName[fldReceiverPort];
                Value := DataCache[0].ValueByName[fldReceiverLogin];
                if Length(Value) <> 0 then
                  begin
                    Result.UserName := Value;
                    Result.Password := DataCache[0].ValueByName[fldReceiverPassword];
                  end;
                if VarToInt(DataCache[0].ValueByName[fldReceiverSSL]) <> 0 then
                  begin
                    Result.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Result);
                    Result.UseTLS := utUseRequireTLS;
                  end;
                KeepMessage := VarToInt(DataCache[0].ValueByName[fldReceiverKeep]) <> 0;
              except
                FreeAndNil(Result);
                raise;
              end;
            end;
        finally
          DataCache.Free;
        end;
      finally
        FilterItem.Free;
      end;
    end;
end;

class function TMailManager.ReceiveMail(const AccountIDs: Variant; const HeaderOnly: Boolean): Variant;
begin
  with Self.Create do
    try
      if HeaderOnly then
        Result := InternalReceiveHeader(AccountIDs)
      else
        Result := InternalReceiveMail(AccountIDs);
    finally
      Free;
    end;
end;

function TMailManager.WriteMessageAttachments(const MessageID: Integer; const DateTime: TDateTime; List: TIdMessageParts): Integer;
var
  TableMeta: TTableMeta;
  DataCache: TDataCache;
  FilterItem: TFilterItem;
  CacheItem: TCacheItem;
  Directory, FileName: string;
  AttachmentDate: TDateTime;
  Index: Integer;
  Forced: Boolean;
  AttachmentFile: TIdAttachmentFile;
  BlobData: TDeImage;
begin
  Result := 0;
  if Assigned(MetaData) and Assigned(MetaData.TablesList) and Assigned(List) then
    begin
      TableMeta := MetaData.TablesList.FindByGUID(guidAttachment);
      if Assigned(TableMeta) then
        try
          Directory := FTempDirectory + Format('~MA%.8u', [MessageID]);;
          if Assigned(DM) then DM.RegisterTemporaryFile(Directory);
          Directory := IncludeTrailingBackslash(Directory);
          AttachmentDate := DateTime;
          if AttachmentDate = 0 then
            if Assigned(List.OwnerMessage) and (List.OwnerMessage is TIdMessage) then
              AttachmentDate := (List.OwnerMessage as TIdMessage).Date;
          FilterItem := TFilterItem.Create;
          try
            DataCache := TDataCache.Create(TableMeta);
            try
              FilterItem.AddCondition(TableMeta.Fields.FindByName(fldAttachmentMessage), opEQ, MessageID);
              DataCache.LoadData(FilterItem);
              FilterItem := nil;
              Forced := True;
              for Index := 0 to Pred(List.Count) do
                if List[Index] is TIdAttachmentFile then
                  begin
                    AttachmentFile := List[Index] as TIdAttachmentFile;
                    if Length(Trim(AttachmentFile.ContentID)) = 0 then
                      begin
                        if Forced then
                          begin
                            if ForceDirectories(Directory) then
                              SetFileAttributes(PChar(Directory), GetFileAttributes(PChar(Directory)) or faHidden);
                            Forced := False;
                          end;
                        FileName := AttachmentFile.FileName;
                        if Length(FileName) = 0 then FileName := Format('Attachment%u', [Index]);
                        FileName := Directory + FileName;
                        if Assigned(DM) then DM.RegisterTemporaryFile(FileName);
                        AttachmentFile.SaveToFile(FileName);
                        if AttachmentDate <> 0 then
                          FileSetDate(FileName, DateTimeToFileDate(AttachmentDate));
                        BlobData := TDeImage.Create(nil);
                        try
                          BlobData.LoadFromFile(FileName);
                          CacheItem := DataCache.AddNewItem;
                          DataCache.DataManager.PrepareRecord(CacheItem);
                          CacheItem.ValueByName[fldAttachmentMessage] := MessageID;
                          CacheItem.ValueByName[fldAttachmentFile] := ExtractFileName(FileName);
                          CacheItem.ValueByName[fldAttachmentData] := BlobData.Value;
                          if DataCache.DataManager.CanInsertRecord(CacheItem) then
                            if DataCache.DataManager.InsertRecord(CacheItem) then
                              Inc(Result)
                            else
                              begin
                                {$IFDEF DeDEBUG}
                                Funcs.WriteLog('Append attachment file %s for message identifier %d error: %s', [QuotedStr(ExtractFileName(FileName)), MessageID, DataCache.DataManager.Errors.GetMessage], False, 'mail');
                                {$ENDIF}
                              end;
                        finally
                          BlobData.Free;
                        end;
                        //DeleteFile(FileName);
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
              Funcs.WriteLog('Write attachment error: %s', [E.Message], False, 'mail');
              {$ENDIF}
            end;
        end;
    end;
end;

function TMailManager.ConvertTextFromHTML(const HTML: string): string;
var
  Value: string;
  Index: Integer;
begin
  Result := EmptyStr;
  Value := HTML;
  while Length(Value) <> 0 do
    begin
      Index := Pos('<', Value);
      if Index = 0 then
        begin
          Result := Result + Value;
          Value := EmptyStr;
        end
      else
        begin
          Result := Result + Copy(Value, 1, Pred(Index));
          Value := Copy(Value, Succ(Index), Length(Value));
          Index := Pos('>', Value);
          if Index = 0 then
            Value := EmptyStr
          else
            Value := Copy(Value, Succ(Index), Length(Value));
        end;
    end;
end;

function TMailManager.SearchMessageText(List: TIdMessageParts; var Text: string): Boolean;
var
  PlainPart, FormatPart, ScanPart: TIdText;
  Index: Integer;
begin
  Text := EmptyStr;
  Result := Assigned(List);
  if Result then
    begin
      PlainPart := nil;
      FormatPart := nil;
      Result := False;
      for Index := 0 to Pred(List.Count) do
        if List[Index] is TIdText then
          begin
            ScanPart := List[Index] as TIdText;
            if Length(Trim(ScanPart.ContentID)) = 0 then
              if SameText(ScanPart.ContentType, 'text/plain') then
                begin
                  PlainPart := ScanPart;
                  Break;
                end
              else
                if SameText(ScanPart.ContentType, 'text/html') then
                  begin
                    if not Assigned(FormatPart) then
                      FormatPart := ScanPart;
                  end;
          end;
      if Assigned(PlainPart) then
        begin
          Text := Trim(PlainPart.Body.Text);
          Result := True;
        end
      else
        if Assigned(FormatPart) then
          begin
            Text := ConvertTextFromHTML(Trim(FormatPart.Body.Text));
            Result := True;
          end;
    end;
end;

function TMailManager.InternalReceiveMail(const AccountIDs: Variant): Variant;
var
  AccountTableMeta, MessageTableMeta: TTableMeta;
  AccountDataCache, MessageDataCache, UpdateDataCache: TDataCache;
  AccountFilterItem, MessageFilterItem, UpdateFilterItem: TFilterItem;
  FieldMeta: TFieldMeta;
  Count, Index, AccountID, MessageID, MessageIndex: Integer;
  CacheItem: TCacheItem;
  MessageIDs: TStrings;
  POP3: TIdPOP3;
  MailMessage: TIdMessage;
  KeepMessage: Boolean;
  MessageGUID, ReplyMessageGUID, FileName: string;
  MailContact: Variant;
  AttachmentDate: TDateTime;
  BlobData: TDeImage;
  function FindMessageCacheItem(const MessageGUID: string): TCacheItem;
  var
    Index: Integer;
  begin
    Result := nil;
    for Index := 0 to Pred(MessageDataCache.Count) do
      if SameStr(MessageGUID, Trim(MessageDataCache[Index].ValueByName[fldMessageGUID])) then
        begin
          Result := MessageDataCache[Index];
          Break;
        end;
  end;
begin
  Result := Unassigned;
  if Assigned(MetaData) and Assigned(MetaData.TablesList) then
    try
      AccountTableMeta := MetaData.TablesList.FindByGUID(guidAccount);
      MessageTableMeta := MetaData.TablesList.FindByGUID(guidMessage);
      if Assigned(AccountTableMeta) and Assigned(MessageTableMeta) then
        begin
          AccountDataCache := TDataCache.Create(AccountTableMeta);
          try
            AccountFilterItem := TFilterItem.Create;
            try
              Count := 0;
              // Если аккаунты указаны, то ...
              if not VarIsNullOrEmpty(AccountIDs) then
                begin
                  Inc(Count);
                  FieldMeta := AccountTableMeta.Fields.FindByName(fldAccountID);
                  // Если указан один аккаунт, то ...
                  if not VarIsArray(AccountIDs) then
                    AccountFilterItem.AddCondition(FieldMeta, opEQ, AccountIDs)
                  else
                    AccountFilterItem.AddCondition(FieldMeta, opIN, AccountIDs);
                end;
              FieldMeta := AccountTableMeta.Fields.FindByName(fldAccountReceiver);
              AccountFilterItem.AddCondition(FieldMeta, opEQ, Null);
              AccountFilterItem.AddOperation(opNot);
              if Count <> 0 then AccountFilterItem.AddOperation(opAnd);
              AccountDataCache.Fields.ChangeStages([fldAccountCreated, fldAccountModified, fldAccountSender], fsBlob);
              AccountDataCache.LoadData(AccountFilterItem, fsFull);
              AccountFilterItem := nil;
              MessageIDs := TStringList.Create;
              try
                // Пробегаемся по всем аккаунтам ...
                for Index := 0 to Pred(AccountDataCache.Count) do
                  begin
                    AccountID := AccountDataCache[Index].ValueByName[fldAccountID];
                    MessageDataCache := TDataCache.Create(MessageTableMeta);
                    try
                      MessageFilterItem := TFilterItem.Create;
                      try
                        FieldMeta := MessageTableMeta.Fields.FindByName(fldMessageAccount);
                        MessageFilterItem.AddCondition(FieldMeta, opEQ, AccountID);
                        FieldMeta := MessageTableMeta.Fields.FindByName(fldMessageFolder);
                        MessageFilterItem.AddCondition(FieldMeta, opIn, VarArrayOf([1, 2])); // Из папки "Входящие / Входящие (заголовки)" ...
                        MessageFilterItem.AddOperation(opAnd);
                        MessageDataCache.Fields.ChangeStages([fldMessageFolder, fldMessageGUID], fsKey);
                        MessageDataCache.LoadData(MessageFilterItem, fsKey);
                        MessageFilterItem := nil;
                        // Если есть сообщения для получения, то ...
                        if MessageDataCache.Count <> 0 then
                          begin
                            { TODO -oКуфенко П.Ю. -cОптимизация : Обращаю внимание, что в переделанной логике ДВА раза создаётся подключение и ДВА раза коннектимся к серверу! Надо сделать один коннект и два цикла (получение заголовков, получение тел писем). }
                            // Этап 1. Получаем все заголовки, чтобы не вываливаться на длительной загрузке тел писем
                            POP3:= CreatePOP3(AccountDataCache[Index].ValueByName[fldAccountReceiver], KeepMessage);
                            try
                              if Assigned(POP3) then
                                begin
                                  POP3.Connect;
                                  try
                                    Count := POP3.CheckMessages;
                                    for MessageIndex:= Count downto 1 do
                                      begin
                                        MailMessage := TIdMessage.Create(nil);
                                        try
                                          if POP3.RetrieveHeader(MessageIndex, MailMessage) then
                                            begin
                                              MessageGUID := PrepareMessageGUID(MailMessage.MsgID);
                                              CacheItem := FindMessageCacheItem(MessageGUID);
                                              if not Assigned(CacheItem) then
                                                begin
                                                  {
                                                  Конечно же здесь можно проигнорировать новые письма
                                                  и ничего не делать! Но считаю, что раз уж получили
                                                  Header письма, то и надо такие письма помечать в
                                                  папке "Входящие (заголовки)", но обрабатывать при
                                                  следующем вызове данного метода! (Раз уж разделяем
                                                  на чтение заголовков и тел писем).
                                                  }
                                                  if Length(MessageGUID) <> 0 then
                                                    begin
                                                      CacheItem := MessageDataCache.AddNewItem;
                                                      MessageDataCache.DataManager.PrepareRecord(CacheItem);
                                                      CacheItem.ValueByName[fldMessageFolder] := 1; // "Входящие (заголовки)"
                                                      CacheItem.ValueByName[fldMessageAccount] := AccountID;
                                                      CacheItem.ValueByName[fldMessageGUID] := MessageGUID;
                                                      CacheItem.ValueByName[fldMessageSubject] := MailMessage.Subject;
                                                      case MailMessage.Priority of
                                                        mpHigh: { Высокий }
                                                          CacheItem.ValueByName[fldMessagePriority] := -1;
                                                        mpLow: { Низкий }
                                                          CacheItem.ValueByName[fldMessagePriority] := 1;
                                                        mpNormal: { Обычный }
                                                          CacheItem.ValueByName[fldMessagePriority] := 0;
                                                      end;
                                                      if MailMessage.Date <> 0 then
                                                        CacheItem.ValueByName[fldMessageDate] := MailMessage.Date;
                                                      MailContact := Contact(MailMessage.From.Text);
                                                      if not VarIsNullOrEmpty(MailContact) then
                                                        CacheItem.ValueByName[fldMessageFrom] := MailContact;
                                                      MessageGUID := PrepareMessageGUID(MailMessage.InReplyTo);
                                                      if Length(MessageGUID) <> 0 then
                                                        CacheItem.ValueByName[fldMessageParent] := FindMessageByGUID(AccountID, MessageGUID);
                                                      if MessageDataCache.DataManager.CanInsertRecord(CacheItem) then
                                                        if MessageDataCache.DataManager.InsertRecord(CacheItem) then
                                                          begin
                                                            MessageID := CacheItem.ValueByName[fldMessageID];
                                                            AppendMessageContactByAddresses(MessageID, 4, MailMessage.Recipients.EMailAddresses);
                                                            AppendMessageContactByAddresses(MessageID, 5, MailMessage.CCList.EMailAddresses);
                                                            AppendMessageContactByAddresses(MessageID, 6, MailMessage.BCCList.EMailAddresses);
                                                            AppendMessageContactByAddresses(MessageID, 1, MailMessage.Sender.Text);
                                                            AppendMessageContactByAddresses(MessageID, 2, MailMessage.From.Text);
                                                            AppendMessageContactByAddresses(MessageID, 3, MailMessage.ReplyTo.EMailAddresses);
                                                          end
                                                        else
                                                          begin
                                                            {$IFDEF DeDEBUG}
                                                            Funcs.WriteLog('Append message header error: %s', [MessageDataCache.DataManager.Errors.GetMessage], False, 'mail');
                                                            {$ENDIF}
                                                          end;
                                                    end;
                                                end;
                                            end;
                                        finally
                                          MailMessage.Free;
                                        end;
                                      end;
                                  finally
                                    POP3.Disconnect;
                                  end;
                                end;
                            finally
                              FreeAndNil(POP3);
                            end;

                            // Этап 2. Получаем тела, если отвалится, продолжим с того же места
                            // Закончили
                            POP3:= CreatePOP3(AccountDataCache[Index].ValueByName[fldAccountReceiver], KeepMessage);
                            try
                              if Assigned(POP3) then
                                begin
                                  POP3.Connect;
                                  try
                                    Count := POP3.CheckMessages;
                                    for MessageIndex := Count downto 1 do
                                      begin
                                        MailMessage := TIdMessage.Create(nil);
                                        try
                                          if POP3.RetrieveHeader(MessageIndex, MailMessage) then
                                            begin
                                              MessageGUID := PrepareMessageGUID(MailMessage.MsgID);
                                              CacheItem := FindMessageCacheItem(MessageGUID);
                                              if Assigned(CacheItem) then
                                                begin
                                                  // Если это папка "Входящие (заголовки)", то ...
                                                  if CacheItem.ValueByName[fldMessageFolder] = 1 then
                                                    begin
                                                      MessageID := CacheItem.ValueByName[fldMessageID];
                                                      if POP3.Retrieve(MessageIndex, MailMessage) then
                                                        begin
                                                          UpdateDataCache := TDataCache.Create(MessageTableMeta);
                                                          try
                                                            UpdateFilterItem := TFilterItem.Create;
                                                            try
                                                              FieldMeta := MessageTableMeta.Fields.FindByName(fldMessageID);
                                                              UpdateFilterItem.AddCondition(FieldMeta, opEQ, MessageID);
                                                              UpdateDataCache.LoadData(UpdateFilterItem, fsMax);
                                                              UpdateFilterItem := nil;
                                                              if UpdateDataCache.Count = 1 then
                                                                CacheItem := UpdateDataCache[0]
                                                              else
                                                                CacheItem := nil;
                                                              if Assigned(CacheItem) then
                                                                begin
                                                                  // Переносим в папку "Входящие" ...
                                                                  CacheItem.ValueByName[fldMessageFolder] := 2;
                                                                  CacheItem.ValueByName[fldMessageSubject] := MailMessage.Subject;
                                                                  case MailMessage.Priority of
                                                                    mpHigh: { Высокий }
                                                                      CacheItem.ValueByName[fldMessagePriority] := -1;
                                                                    mpLow: { Низкий }
                                                                      CacheItem.ValueByName[fldMessagePriority] := 1;
                                                                    mpNormal: { Обычный }
                                                                      CacheItem.ValueByName[fldMessagePriority] := 0;
                                                                  end;
                                                                  AttachmentDate := MailMessage.Date;
                                                                  if AttachmentDate <> 0 then
                                                                    CacheItem.ValueByName[fldMessageDate] := AttachmentDate;
                                                                  MailContact := Contact(MailMessage.From.Text);
                                                                  if not VarIsNullOrEmpty(MailContact) then
                                                                    CacheItem.ValueByName[fldMessageFrom] := MailContact;
                                                                  ReplyMessageGUID := PrepareMessageGUID(MailMessage.InReplyTo);
                                                                  if Length(ReplyMessageGUID) <> 0 then
                                                                    CacheItem.ValueByName[fldMessageParent] := FindMessageByGUID(AccountID, ReplyMessageGUID);
                                                                  if SearchMessageText(MailMessage.MessageParts, FileName) then
                                                                    if Length(Trim(FileName)) = 0 then
                                                                      CacheItem.ValueByName[fldMessageText] := Null
                                                                    else
                                                                      CacheItem.ValueByName[fldMessageText] := FileName
                                                                  else
                                                                    if Length(Trim(MailMessage.Body.Text)) <> 0 then
                                                                      if SameText(MailMessage.ContentType, 'text/html') then
                                                                        CacheItem.ValueByName[fldMessageText] := ConvertTextFromHTML(MailMessage.Body.Text)
                                                                      else
                                                                        CacheItem.ValueByName[fldMessageText] := MailMessage.Body.Text
                                                                    else
                                                                      CacheItem.ValueByName[fldMessageText] := Null;
                                                                  FileName := FTempDirectory + Format('~MA%.8u.eml', [MessageID]);
                                                                  if Assigned(DM) then DM.RegisterTemporaryFile(FileName);
                                                                  MailMessage.SaveToFile(FileName);
                                                                  BlobData := TDeImage.Create(nil);
                                                                  try
                                                                    BlobData.LoadFromFile(FileName);
                                                                    CacheItem.ValueByName[fldMessageEML] := BlobData.Value;
                                                                  finally
                                                                    BlobData.Free;
                                                                  end;
                                                                  if UpdateDataCache.DataManager.CanUpdateRecord(CacheItem) then
                                                                    if UpdateDataCache.DataManager.UpdateRecord(CacheItem) then
                                                                      begin
                                                                        AppendMessageContactByAddresses(MessageID, 4, MailMessage.Recipients.EMailAddresses);
                                                                        AppendMessageContactByAddresses(MessageID, 5, MailMessage.CCList.EMailAddresses);
                                                                        AppendMessageContactByAddresses(MessageID, 6, MailMessage.BCCList.EMailAddresses);
                                                                        AppendMessageContactByAddresses(MessageID, 1, MailMessage.Sender.Text);
                                                                        AppendMessageContactByAddresses(MessageID, 2, MailMessage.From.Text);
                                                                        AppendMessageContactByAddresses(MessageID, 3, MailMessage.ReplyTo.EMailAddresses);
                                                                        WriteMessageAttachments(MessageID, AttachmentDate, MailMessage.MessageParts);
                                                                        MessageIDs.Append(IntToStr(MessageID));
                                                                        // Если не надо оставлять письма в ящике, то ...
                                                                        if not KeepMessage then
                                                                          if not POP3.Delete(MessageIndex) then
                                                                            begin
                                                                              {$IFDEF DeDEBUG}
                                                                              Funcs.WriteLog('Delete message %s in mailbox failed!', [QuotedStr(MessageGUID)], False, 'mail');
                                                                              {$ENDIF}
                                                                            end;
                                                                      end
                                                                    else
                                                                      begin
                                                                        {$IFDEF DeDEBUG}
                                                                        Funcs.WriteLog('Update message identifier %d error: %s', [MessageID, MessageDataCache.DataManager.Errors.GetMessage], False, 'mail');
                                                                        {$ENDIF}
                                                                      end;
                                                                end
                                                              else
                                                                begin
                                                                  {$IFDEF DeDEBUG}
                                                                  Funcs.WriteLog('Update message identifier %d not found!!!', [MessageID], False, 'mail');
                                                                  {$ENDIF}
                                                                end;
                                                            finally
                                                              UpdateFilterItem.Free;
                                                            end;
                                                          finally
                                                            UpdateDataCache.Free;
                                                          end;
                                                        end
                                                      else
                                                        begin
                                                          {$IFDEF DeDEBUG}
                                                          Funcs.WriteLog('Receive message identifier %d by index %d error!', [MessageID, MessageIndex], False, 'mail');
                                                          {$ENDIF}
                                                        end;
                                                    end
                                                  else
                                                    if not KeepMessage then
                                                      if not POP3.Delete(MessageIndex) then
                                                        begin
                                                          {$IFDEF DeDEBUG}
                                                          Funcs.WriteLog('Delete message %s in mailbox failed!', [QuotedStr(MessageGUID)], False, 'mail');
                                                          {$ENDIF}
                                                        end;
                                                end;
                                            end;
                                        finally
                                          MailMessage.Free;
                                        end;
                                      end;
                                  finally
                                    POP3.Disconnect;
                                  end;
                                end;
                            finally
                              FreeAndNil(POP3);
                            end;
                            // Закончили
                          end;
                      finally
                        MessageFilterItem.Free;
                      end;
                    finally
                      MessageDataCache.Free;
                    end;
                  end;
                case MessageIDs.Count of
                  0: { Сообщения не получены }
                    Result := Null;
                  1: { Получено одно сообщение }
                    if TryStrToInt(MessageIDs[0], MessageID) then
                      Result := MessageID
                    else
                      Result := Null;
                else
                  Result := VarArrayCreate([0, Pred(MessageIDs.Count)], varVariant);
                  for Index := 0 to Pred(MessageIDs.Count) do
                    if TryStrToInt(MessageIDs[Index], MessageID) then
                      VarArrayPut(Result, MessageID, [Index])
                    else
                      VarArrayPut(Result, Null, [Index]);
                end;
              finally
                MessageIDs.Free;
              end;
            finally
              AccountFilterItem.Free;
            end;
          finally
            AccountDataCache.Free;
          end;
        end;
    except
      on E: Exception do
        begin
          Funcs.WriteLog('Receive mail error: %s', [E.Message], False, 'mail');
          SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(E.Message)));
          Result := Null;
        end;
    end;
end;

function TMailManager.InternalReceiveHeader(const AccountIDs: Variant): Variant;
var
  AccountTableMeta, MessageTableMeta: TTableMeta;
  AccountDataCache, MessageDataCache: TDataCache;
  AccountFilterItem, MessageFilterItem: TFilterItem;
  FieldMeta: TFieldMeta;
  Count, Index, AccountID, MessageID, MessageIndex: Integer;
  CacheItem: TCacheItem;
  MessageIDs: TStrings;
  POP3: TIdPOP3;
  MailMessage: TIdMessage;
  KeepMessage: Boolean;
  MessageGUID, ReplyMessageGUID, FileName: string;
  MailContact: Variant;
  AttachmentDate: TDateTime;
  BlobData: TDeImage;
  function FindMessageCacheItem(const MessageGUID: string): TCacheItem;
  var
    Index: Integer;
  begin
    Result := nil;
    for Index := 0 to Pred(MessageDataCache.Count) do
      if SameStr(MessageGUID, Trim(MessageDataCache[Index].ValueByName[fldMessageGUID])) then
        begin
          Result := MessageDataCache[Index];
          Break;
        end;
  end;
begin
  Result := Unassigned;
  if Assigned(MetaData) and Assigned(MetaData.TablesList) then
    try
      AccountTableMeta := MetaData.TablesList.FindByGUID(guidAccount);
      MessageTableMeta := MetaData.TablesList.FindByGUID(guidMessage);
      if Assigned(AccountTableMeta) and Assigned(MessageTableMeta) then
        begin
          AccountDataCache := TDataCache.Create(AccountTableMeta);
          try
            AccountFilterItem := TFilterItem.Create;
            try
              Count := 0;
              // Если аккаунты указаны, то ...
              if not VarIsNullOrEmpty(AccountIDs) then
                begin
                  Inc(Count);
                  FieldMeta := AccountTableMeta.Fields.FindByName(fldAccountID);
                  // Если указан один аккаунт, то ...
                  if not VarIsArray(AccountIDs) then
                    AccountFilterItem.AddCondition(FieldMeta, opEQ, AccountIDs)
                  else
                    AccountFilterItem.AddCondition(FieldMeta, opIN, AccountIDs);
                end;
              FieldMeta := AccountTableMeta.Fields.FindByName(fldAccountReceiver);
              AccountFilterItem.AddCondition(FieldMeta, opEQ, Null);
              AccountFilterItem.AddOperation(opNot);
              if Count <> 0 then AccountFilterItem.AddOperation(opAnd);
              AccountDataCache.Fields.ChangeStages([fldAccountCreated, fldAccountModified, fldAccountSender], fsBlob);
              AccountDataCache.LoadData(AccountFilterItem, fsFull);
              AccountFilterItem := nil;
              MessageIDs := TStringList.Create;
              try
                // Пробегаемся по всем аккаунтам ...
                for Index := 0 to Pred(AccountDataCache.Count) do
                  begin
                    AccountID := AccountDataCache[Index].ValueByName[fldAccountID];
                    MessageDataCache := TDataCache.Create(MessageTableMeta);
                    try
                      MessageFilterItem := TFilterItem.Create;
                      try
                        FieldMeta := MessageTableMeta.Fields.FindByName(fldMessageAccount);
                        MessageFilterItem.AddCondition(FieldMeta, opEQ, AccountID);
                        FieldMeta := MessageTableMeta.Fields.FindByName(fldMessageFolder);
                        MessageFilterItem.AddCondition(FieldMeta, opIn, VarArrayOf([1, 2])); // Из папки "Входящие / Входящие (заголовки)" ...
                        MessageFilterItem.AddOperation(opAnd);
                        MessageDataCache.Fields.ChangeStages([fldMessageGUID], fsKey);
                        MessageDataCache.LoadData(MessageFilterItem, fsKey);
                        MessageFilterItem := nil;
                        POP3 := CreatePOP3(AccountDataCache[Index].ValueByName[fldAccountReceiver], KeepMessage);
                        try
                          if Assigned(POP3) then
                            begin
                              POP3.Connect;
                              try
                                Count := POP3.CheckMessages;
                                for MessageIndex := 1 to Count do
                                  begin
                                    MailMessage := TIdMessage.Create(nil);
                                    try
                                      if POP3.RetrieveHeader(MessageIndex, MailMessage) then
                                        begin
                                          MessageGUID := PrepareMessageGUID(MailMessage.MsgID);
                                          CacheItem := FindMessageCacheItem(MessageGUID);
                                          if not Assigned(CacheItem) then
                                            begin
                                              CacheItem := MessageDataCache.AddNewItem;
                                              MessageDataCache.DataManager.PrepareRecord(CacheItem);
                                              CacheItem.ValueByName[fldMessageFolder] := 1; // "Входящие (заголовки)"
                                              CacheItem.ValueByName[fldMessageAccount] := AccountID;
                                              if Length(MessageGUID) <> 0 then
                                                CacheItem.ValueByName[fldMessageGUID] := MessageGUID;
                                              CacheItem.ValueByName[fldMessageSubject] := MailMessage.Subject;
                                              case MailMessage.Priority of
                                                mpHigh: { Высокий }
                                                  CacheItem.ValueByName[fldMessagePriority] := -1;
                                                mpLow: { Низкий }
                                                  CacheItem.ValueByName[fldMessagePriority] := 1;
                                                mpNormal: { Обычный }
                                                  CacheItem.ValueByName[fldMessagePriority] := 0;
                                              end;
                                              if MailMessage.Date <> 0 then
                                                CacheItem.ValueByName[fldMessageDate] := MailMessage.Date;
                                              MailContact := Contact(MailMessage.From.Text);
                                              if not VarIsNullOrEmpty(MailContact) then
                                                CacheItem.ValueByName[fldMessageFrom] := MailContact;
                                              MessageGUID := PrepareMessageGUID(MailMessage.InReplyTo);
                                              if Length(MessageGUID) <> 0 then
                                                CacheItem.ValueByName[fldMessageParent] := FindMessageByGUID(AccountID, MessageGUID);
                                              if MessageDataCache.DataManager.CanInsertRecord(CacheItem) then
                                                if MessageDataCache.DataManager.InsertRecord(CacheItem) then
                                                  begin
                                                    MessageID := CacheItem.ValueByName[fldMessageID];
                                                    AppendMessageContactByAddresses(MessageID, 4, MailMessage.Recipients.EMailAddresses);
                                                    AppendMessageContactByAddresses(MessageID, 5, MailMessage.CCList.EMailAddresses);
                                                    AppendMessageContactByAddresses(MessageID, 6, MailMessage.BCCList.EMailAddresses);
                                                    AppendMessageContactByAddresses(MessageID, 1, MailMessage.Sender.Text);
                                                    AppendMessageContactByAddresses(MessageID, 2, MailMessage.From.Text);
                                                    AppendMessageContactByAddresses(MessageID, 3, MailMessage.ReplyTo.EMailAddresses);
                                                    MessageIDs.Append(IntToStr(MessageID));
                                                  end
                                                else
                                                  begin
                                                    {$IFDEF DeDEBUG}
                                                    Funcs.WriteLog('Append message header error: %s', [MessageDataCache.DataManager.Errors.GetMessage], False, 'mail');
                                                    {$ENDIF}
                                                  end;
                                            end;
                                        end;
                                    finally
                                      MailMessage.Free;
                                    end;
                                  end;
                              finally
                                POP3.Disconnect;
                              end;
                            end;
                        finally
                          FreeAndNil(POP3);
                        end;
                      finally
                        MessageFilterItem.Free;
                      end;
                    finally
                      MessageDataCache.Free;
                    end;
                  end;
                case MessageIDs.Count of
                  0: { Сообщения не получены }
                    Result := Null;
                  1: { Получено одно сообщение }
                    if TryStrToInt(MessageIDs[0], MessageID) then
                      Result := MessageID
                    else
                      Result := Null;
                else
                  Result := VarArrayCreate([0, Pred(MessageIDs.Count)], varVariant);
                  for Index := 0 to Pred(MessageIDs.Count) do
                    if TryStrToInt(MessageIDs[Index], MessageID) then
                      VarArrayPut(Result, MessageID, [Index])
                    else
                      VarArrayPut(Result, Null, [Index]);
                end;
              finally
                MessageIDs.Free;
              end;
            finally
              AccountFilterItem.Free;
            end;
          finally
            AccountDataCache.Free;
          end;
        end;
    except
      on E: Exception do
        begin
          {$IFDEF DeDEBUG}
          Funcs.WriteLog('Receive header error: %s', [E.Message], False, 'mail');
          {$ENDIF}
          Result := Null;
        end;
    end;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('uMailManager unit initialization ...');

finalization
  DebugLog('uMailManager unit finalization ...');
{$ENDIF}

end.

