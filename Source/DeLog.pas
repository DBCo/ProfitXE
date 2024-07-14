{ *************************************************************************************** }
{ *                                                                                     * }
{ * Название: Модуль логирования                                                        * }
{ *    Автор: Куфенко П. Ю.                                                             * }
{ *                                                                                     * }
{ * Описание: Данный модуль используется для логирования работы программы и всегда      * }
{ *           выполняет логирование при вызове своих процедур WriteLog. А вот вызов     * }
{ *           процедур DebugLog возможен только при условии, что программа              * }
{ *           скомпилирована в режиме отладки (с отладочной информацией).               * }
{ *                                                                                     * }
{ *************************************************************************************** }

{$WARN SYMBOL_DEPRECATED OFF}

unit DeLog;

interface

uses DB;

const
  cApplicationName = 'dbco';
  cExtensionLOG = '.log';

  cRegistrySectionLog = '\Software\' + cApplicationName + '\Log\';

const
  BooleanNames: array[Boolean] of PChar = ('False', 'True');

var
  LogDirectory: string;

procedure WriteLog(const Text: string; const FileName: string = ''); overload;
procedure WriteLog(const Format: string; const Arguments: array of const; const FileName: string = ''); overload;

procedure WriteQueryLog(DataSet: TDataSet; const FileName: string = '');

function PrepareProjectCompiler: string;
function PrepareProjectOptions: string;
function PrepareProjectDefinitions: string;

{$IFDEF DEBUG}
procedure DebugLog(const Text: string); overload;
procedure DebugLog(const Format: string; const Arguments: array of const); overload;
{$ENDIF}

function VariantToString(const Value: Variant): string;

type
  TApplicationFolderType =
    (
      aftDefault,        // Сначала CSIDL_APPDATA и при пустом CSIDL_LOCAL_APPDATA
      aftUser,           // CSIDL_APPDATA:        C:\Documents and Settings\username\Application Data (roaming)
      aftCommon,         // CSIDL_COMMON_APPDATA: C:\Documents and Settings\All Users\Application Data
      aftLocal           // CSIDL_LOCAL_APPDATA:  C:\Documents and Settings\username\Local Settings\Application Data (nonroaming)
    );

function GetApplicationDataDirectory(const FolderType: TApplicationFolderType = aftDefault): string;

procedure ReadParameterLog(var FullDateTimeEnabled: Boolean; var MaxSize: Int64);
procedure WriteParameterLog(const FullDateTimeEnabled: Boolean; const MaxSize: Int64);

implementation

uses Windows, SysUtils, StrUtils, DateUtils, Variants, Classes, Registry,
     ShFolder, ADODB, IBQuery, FireDAC.Comp.Client, FireDAC.Stan.Param;

var
  LogFullDateTime: Boolean;
  LogMaxSize: Int64;

function VariantToString(const Value: Variant): string;
var
  Index: Integer;
begin
  if VarIsEmpty(Value) then
    Result := 'unassigned'
  else if VarIsNull(Value) then
    Result := 'null'
  else if VarIsType(Value, [varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord, varInt64]) then
    Result := VarToStr(Value)
  else if VarIsType(Value, [varBoolean]) then
    Result := StrPas(BooleanNames[Value = True])
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
          Result := Result + VariantToString(VarArrayGet(Value, [Index]));
        end;
      Result := '[' + Result + ']';
    end
  else
    Result := QuotedStr(VarToStr(Value));
end;

function GetApplicationDataDirectory(const FolderType: TApplicationFolderType): string;
var
  Path: PChar;
begin
  Path := AllocMem(Windows.MAX_PATH);
  try
    Result := EmptyStr;
    case FolderType of
      aftUser:
        if SHGetFolderPath(0, CSIDL_APPDATA, 0, 0, Path) = S_OK then
          Result := StrPas(Path);
      aftCommon:
        if SHGetFolderPath(0, CSIDL_COMMON_APPDATA, 0, 0, Path) = S_OK then
          Result := StrPas(Path);
      aftLocal:
        if SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, Path) = S_OK then
          Result := StrPas(Path);
    else
      if SHGetFolderPath(0, CSIDL_APPDATA, 0, 0, Path) = S_OK then
        Result := StrPas(Path);
      if Length(Result) = 0 then
        if SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, Path) = S_OK then
          Result := StrPas(Path);
    end;
  finally
    FreeMem(Path);
  end;
  if Length(Result) <> 0 then
    Result := IncludeTrailingPathDelimiter(Result);
end;

const
  cFullDateTimeIdent = 'FullDateTime';
  cMaxSizeIdent = 'MaxSize';

procedure ReadParameterLog(var FullDateTimeEnabled: Boolean; var MaxSize: Int64);
begin
  FullDateTimeEnabled := False;
  MaxSize := 0;
  try
    with TRegistry.Create do
      try
        RootKey := HKEY_CURRENT_USER;
        if OpenKeyReadOnly(cRegistrySectionLog) then
          begin
            if ValueExists(cFullDateTimeIdent) then
              FullDateTimeEnabled := ReadBool(cFullDateTimeIdent);
            if ValueExists(cMaxSizeIdent) then
              MaxSize := ReadInteger(cMaxSizeIdent);
          end;
      finally
        Free;
      end;
  except
    { Ничего не делаем }
  end;
end;

procedure WriteParameterLog(const FullDateTimeEnabled: Boolean; const MaxSize: Int64);
begin
  try
    with TRegistry.Create do
      try
        RootKey := HKEY_CURRENT_USER;
        if OpenKey(cRegistrySectionLog, True) then
          begin
            WriteBool(cFullDateTimeIdent, FullDateTimeEnabled);
            LogFullDateTime := FullDateTimeEnabled;
            if MaxSize > 0 then
              WriteInteger(cMaxSizeIdent, MaxSize)
            else
              WriteInteger(cMaxSizeIdent, 0);
            LogMaxSize := MaxSize;
            if LogMaxSize > 0 then
              LogMaxSize := LogMaxSize * 1024 * 1024
            else
              LogMaxSize := 0;
          end;
      finally
        Free;
      end;
  except
    { Ничего не делаем }
  end;
end;

procedure WriteLog(const Text: string; const FileName: string);
var
  LogDateTime: TDateTime;
  LogFileName: string;
  LogFileMode: Word;
  LogDateMode: Boolean;
  LogString: string;
  LogBuffer: TBytes;
  LogStream, MemStream: TStream;
  LogSymbol: Byte;
begin
  try
    LogDateTime := Now;
    if Length(FileName) = 0 then
      LogFileName := LogDirectory + cApplicationName + cExtensionLOG
    else
      begin
        LogFileName := ExtractFilePath(FileName);
        if Length(LogFileName) = 0 then LogFileName := LogDirectory;
        if Length(LogFileName) <> 0 then LogFileName := IncludeTrailingPathDelimiter(LogFileName);
        LogFileName := LogFileName + ExtractFileName(FileName);
        if Length(ExtractFileExt(FileName)) = 0 then
          LogFileName := LogFileName + cExtensionLOG;
      end;
    LogString := ExtractFilePath(LogFileName);
    if Length(LogString) <> 0 then ForceDirectories(LogString);
    if FileExists(LogFileName) then
      begin
        LogFileMode := fmOpenReadWrite;
        LogDateMode := not SameDate(FileDateToDateTime(FileAge(LogFileName)), LogDateTime);
      end
    else
      begin
        LogFileMode := fmCreate;
        LogDateMode := True;
      end;
    if LogDateMode or LogFullDateTime then
      LogString := FormatDateTime('DD"."MM"."YYYY" "', LogDateTime)
    else
      LogString := '           ';
    LogString := LogString + FormatDateTime('HH":"NN":"SS"."ZZZ', LogDateTime) + TrimRight(' ' + Text) + #13#10;
    LogBuffer := TEncoding.UTF8.GetBytes(LogString);
    LogStream := TFileStream.Create(LogFileName, LogFileMode or fmShareDenyWrite);
    try
      if (LogMaxSize > 0) and (LogStream.Size > (LogMaxSize - Length(LogBuffer))) then
        begin
          while LogStream.Position < LogStream.Size do
            begin
              LogStream.ReadBuffer(LogSymbol, SizeOf(LogSymbol));
              if LogSymbol = 13 then
                begin
                  LogStream.ReadBuffer(LogSymbol, SizeOf(LogSymbol));
                  if LogSymbol <> 10 then
                    LogStream.Seek(-SizeOf(LogSymbol), soFromCurrent);
                  if (LogMaxSize - Length(LogBuffer)) > (LogStream.Size - LogStream.Position) then
                    Break;
                end;
            end;
          if LogStream.Position < LogStream.Size then
            begin
              MemStream := TMemoryStream.Create;
              try
                MemStream.CopyFrom(LogStream, LogStream.Size - LogStream.Position);
                MemStream.Seek(0, soFromBeginning);
                LogStream.Seek(0, soFromBeginning);
                LogStream.CopyFrom(MemStream, MemStream.Size);
                LogStream.Size := MemStream.Size;
              finally
                MemStream.Free;
              end;
            end
          else
            LogStream.Size := 0;
        end;
      LogStream.Seek(0, soFromEnd);
      LogStream.WriteBuffer(LogBuffer, Length(LogBuffer));
    finally
      LogStream.Free;
    end;
  except
    { Ничего не делаем }
  end;
end;

procedure WriteLog(const Format: string; const Arguments: array of const; const FileName: string);
begin
  try
    WriteLog(SysUtils.Format(Format, Arguments), FileName);
  except
    { Ничего не делаем }
  end;
end;

procedure WriteQueryLog(DataSet: TDataSet; const FileName: string);
  procedure WriteADOQuery(Query: TADOQuery);
  var
    Value: string;
    Index: Integer;
    Param: TParameter;
  begin
    Value := EmptyStr;
    if Assigned(Query) then
      begin
        for Index := 0 to Pred(Query.Parameters.Count) do
          begin
            Param := Query.Parameters[Index];
            if Assigned(Param) then
              begin
                if Length(Value) <> 0 then
                  Value := Value + '; ';
                Value := Value + Param.Name + '=';
                if VarIsEmpty(Param.Value) then
                  Value := Value + '/* unassigned */'
                else if VarIsNull(Param.Value) then
                  Value := Value + 'null'
                else
                  case Param.DataType of
                    ftString, ftWideString, ftGUID: Value := Value + QuotedStr(VarToStr(Param.Value));
                    ftBoolean:
                      if Param.Value then
                        Value := Value + 'True'
                      else
                        Value := Value + 'False';
                    ftDate: Value := Value + QuotedStr(FormatDateTime('YYYYMMDD', Param.Value));
                    ftDateTime: Value := Value + QuotedStr(FormatDateTime('YYYYMMDD HH":"NN":"SS"."ZZZ', Param.Value));
                    ftBlob: Value := Value + '/* ... BLOB ... */';
                  else
                    try
                      Value := Value + VarToStr(Param.Value);
                    except
                      on E: Exception do
                        Value := Value + '/* Error: ' + E.Message + ' */';
                    end;
                  end;
              end;
          end;
        if Length(Value) <> 0 then
          Value := ' -- ' + Value;
        WriteLog(Trim(Query.SQL.Text) + Value, FileName);
      end;
  end;
  procedure WriteADOStoredProc(StoredProc: TADOStoredProc);
  var
    Value: string;
    Index: Integer;
    Param: TParameter;
  begin
    Value := EmptyStr;
    if Assigned(StoredProc) then
      begin
        for Index := 0 to Pred(StoredProc.Parameters.Count) do
          begin
            Param := StoredProc.Parameters[Index];
            if Assigned(Param) and (Param.Direction <> pdReturnValue) then
              begin
                if Length(Value) <> 0 then
                  Value := Value + ', ';
                Value := Value + Param.Name + '=';
                if VarIsEmpty(Param.Value) then
                  Value := Value + '/* unassigned */'
                else if VarIsNull(Param.Value) then
                  Value := Value + 'null'
                else
                  case Param.DataType of
                    ftString, ftWideString, ftGUID: Value := Value + QuotedStr(VarToStr(Param.Value));
                    ftBoolean:
                      if Param.Value then
                        Value := Value + '1'
                      else
                        Value := Value + '0';
                    ftDate: Value := Value + QuotedStr(FormatDateTime('YYYYMMDD', Param.Value));
                    ftDateTime: Value := Value + QuotedStr(FormatDateTime('YYYYMMDD HH":"NN":"SS"."ZZZ', Param.Value));
                    ftBlob: Value := Value + '/* ... BLOB ... */';
                  else
                    try
                      Value := Value + VarToStr(Param.Value);
                    except
                      on E: Exception do
                        Value := Value + '/* Error: ' + E.Message + ' */';
                    end;
                  end;
                if Param.Direction = pdOutput then Value := Value + ' out';
              end;
          end;
        WriteLog('exec ' + StoredProc.ProcedureName + TrimRight(' ' + Value), FileName);
      end;
  end;
  procedure WriteFDQuery(Query: TFDQuery);
  var
    Value: string;
    Index: Integer;
    Param: TFDParam;
  begin
    Value := EmptyStr;
    if Assigned(Query) then
      begin
        for Index := 0 to Pred(Query.Params.Count) do
          begin
            Param := Query.Params[Index];
            if Assigned(Param) then
              begin
                if Length(Value) <> 0 then
                  Value := Value + '; ';
                Value := Value + Param.Name + '=';
                if VarIsEmpty(Param.Value) then
                  Value := Value + '/* unassigned */'
                else if VarIsNull(Param.Value) then
                  Value := Value + 'null'
                else
                  case Param.DataType of
                    ftString, ftWideString, ftGUID: Value := Value + QuotedStr(VarToStr(Param.Value));
                    ftBoolean:
                      if Param.Value then
                        Value := Value + '1'
                      else
                        Value := Value + '0';
                    ftDate: Value := Value + QuotedStr(FormatDateTime('YYYYMMDD', Param.Value));
                    ftDateTime: Value := Value + QuotedStr(FormatDateTime('YYYYMMDD HH":"NN":"SS"."ZZZ', Param.Value));
                    ftBlob: Value := Value + '/* ... BLOB ... */';
                  else
                    try
                      Value := Value + VarToStr(Param.Value);
                    except
                      on E: Exception do
                        Value := Value + '/* Error: ' + E.Message + ' */';
                    end;
                  end;
              end;
          end;
        if Length(Value) <> 0 then
          Value := ' -- ' + Value;
        WriteLog(Trim(Query.SQL.Text) + Value, FileName);
      end;
  end;
begin
  try
    if Assigned(DataSet) then
      if DataSet is TADOQuery then
        WriteADOQuery(DataSet as TADOQuery)
      else if DataSet is TADOStoredProc then
        WriteADOStoredProc(DataSet as TADOStoredProc)
      else if DataSet is TIBQuery then
        WriteLog((DataSet as TIBQuery).SQL.Text, FileName)
      else if DataSet is TFDQuery then
        WriteFDQuery(DataSet as TFDQuery);
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('WriteQueryLog error: ' + E.Message);
    {$ENDIF}
  end;
end;

{$REGION 'Debug Runtime'}
{$IFDEF DEBUG}
procedure DebugLog(const Text: string);
begin
  try
    WriteLog(Text, LogDirectory + 'Debug' + cExtensionLOG);
  except
    { Ничего не делаем }
  end;
end;

procedure DebugLog(const Format: string; const Arguments: array of const); overload;
begin
  try
    DebugLog(SysUtils.Format(Format, Arguments));
  except
    { Ничего не делаем }
  end;
end;
{$ENDIF}
{$ENDREGION}

function PrepareProjectCompiler: string;
begin
  {$IF DEFINED(VER350)}
  Result := 'Delphi 11.0 Alexandria';
  {$ELSEIF DEFINED(VER340)}
  Result := 'Delphi 10.4 Sydney';
  {$ELSEIF DEFINED(VER330)}
  Result := 'Delphi 10.3 Rio';
  {$ELSEIF DEFINED(VER320)}
  Result := 'Delphi 10.2 Tokyo';
  {$ELSEIF DEFINED(VER310)}
  Result := 'Delphi 10.1 Berlin';
  {$ELSEIF DEFINED(VER300)}
  Result := 'Delphi 10 Seattle';
  {$ELSEIF DEFINED(VER290)}
  Result := 'RAD Studio XE 8';
  {$ELSEIF DEFINED(VER280)}
  Result := 'RAD Studio XE 7';
  {$ELSEIF DEFINED(VER270)}
  Result := 'RAD Studio XE 6';
  {$ELSEIF DEFINED(VER260)}
  Result := 'RAD Studio XE 5';
  {$ELSEIF DEFINED(VER250)}
  Result := 'RAD Studio XE 4';
  {$ELSEIF DEFINED(VER240)}
  Result := 'RAD Studio XE 3';
  {$ELSEIF DEFINED(VER230)}
  Result := 'RAD Studio XE 2';
  {$ELSEIF DEFINED(VER220)}
  Result := 'RAD Studio XE';
  {$ELSEIF DEFINED(VER210)}
  Result := 'Delphi 2010';
  {$ELSEIF DEFINED(VER200)}
  Result := 'Delphi 2009';
  {$ELSEIF DEFINED(VER190)}
  Result := 'Delphi 2007 for .NET';
  {$ELSEIF DEFINED(VER185)}
  Result := 'Delphi 2007';
  {$ELSEIF DEFINED(VER180)}
  Result := 'Delphi 2006';
  {$ELSEIF DEFINED(VER170)}
  Result := 'Delphi 2005';
  {$ELSEIF DEFINED(VER160)}
  Result := 'Delphi 8 for .NET';
  {$ELSEIF DEFINED(VER150)}
  Result := 'Delphi 7';
  {$ELSE}
  Result := EmptyStr;
  {$IFEND}
  if Length(Result) <> 0 then
    Result := #13#10'                        Project compiler: ' + Result;
end;

function PrepareProjectOptions: string;
type
  TTestEnumRec = (Test1, Test2);
  {$IFOPT A+}
  TTestAlignRec = record
    A: Byte;
    B: Int64;
  end;
  {$ENDIF}
  procedure Append(const Text: string); overload;
  begin
    if Length(Result) <> 0 then Result := Result + ', ';
    Result := Result + Text;
  end;
  procedure Append(const Name: string; const Enabled: Boolean); overload;
  const
    EnabledNames: array[Boolean] of Char = ('-', '+');
  begin
    Append(Name + EnabledNames[Enabled]);
  end;
begin
  Result := EmptyStr;
  Append('A', {$IFOPT A+}True{$ELSE}False{$ENDIF});  // Align fields
  {$IFOPT A+}
  {$IF SizeOf(TTestAlignRec) = 9}Append('A1');{$ENDIF}
  {$IF SizeOf(TTestAlignRec) = 10}Append('A2');{$ENDIF}
  {$IF SizeOf(TTestAlignRec) = 12}Append('A4');{$ENDIF}
  {$IF SizeOf(TTestAlignRec) = 16}Append('A8');{$ENDIF}
  {$IF SizeOf(TTestAlignRec) > 16}Append('A16');{$ENDIF}
  {$ENDIF}
  Append('B', {$IFOPT B+}True{$ELSE}False{$ENDIF});  // Boolean short-circuit evaluation
  Append('C', {$IFOPT C+}True{$ELSE}False{$ENDIF});  // Assert directives
  Append('D', {$IFOPT D+}True{$ELSE}False{$ENDIF});  // Debug information
  Append('G', {$IFOPT G+}True{$ELSE}False{$ENDIF});  // Imported data
  Append('H', {$IFOPT H+}True{$ELSE}False{$ENDIF});  // Long strings
  Append('I', {$IFOPT I+}True{$ELSE}False{$ENDIF});  // Input output checking
  Append('J', {$IFOPT J+}True{$ELSE}False{$ENDIF});  // Writeable typed constants
  Append('L', {$IFOPT L+}True{$ELSE}False{$ENDIF});  // Local symbol information
  Append('M', {$IFOPT M+}True{$ELSE}False{$ENDIF});  // Run-Time Type Information
  Append('O', {$IFOPT O+}True{$ELSE}False{$ENDIF});  // Optimization
  Append('P', {$IFOPT P+}True{$ELSE}False{$ENDIF});  // Open String Parameters
  Append('Q', {$IFOPT Q+}True{$ELSE}False{$ENDIF});  // Overflow checking
  Append('R', {$IFOPT R+}True{$ELSE}False{$ENDIF});  // Range checking
  Append('T', {$IFOPT T+}True{$ELSE}False{$ENDIF});  // Type-checked pointers
  Append('U', {$IFOPT U+}True{$ELSE}False{$ENDIF});  // Pentium-safe FDIV operations
  Append('V', {$IFOPT V+}True{$ELSE}False{$ENDIF});  // Var-string checking
  Append('W', {$IFOPT W+}True{$ELSE}False{$ENDIF});  // Stack frames
  Append('X', {$IFOPT X+}True{$ELSE}False{$ENDIF});  // Extended syntax
  Append('Y', {$IFOPT Y+}True{$ELSE}False{$ENDIF});  // Symbol declaration and cross-reference information
  Append('Z', {$IFOPT Z+}True{$ELSE}False{$ENDIF});  // Minimum enumeration size
  {$IF SizeOf(TTestEnumRec) = 1}Append('Z1');{$ENDIF}
  {$IF SizeOf(TTestEnumRec) = 2}Append('Z2');{$ENDIF}
  {$IF SizeOf(TTestEnumRec) > 2}Append('Z4');{$ENDIF}
  if Length(Result) <> 0 then
    Result := #13#10'                        Project options: ' + Result;
end;

function PrepareProjectDefinitions: string;
  procedure Append(const Name: string);
  begin
    if Length(Result) <> 0 then
      Result := Result + ', ';
    Result := Result + Name;
  end;
begin
  Result := EmptyStr;
  {$IFDEF MSWINDOWS}Append('MSWINDOWS');{$ENDIF}   // Indicates that the operating environment is Windows.
  {$IFDEF WIN32}Append('WIN32');{$ENDIF}           // Target platform is the native 32-bit Windows platform.
  {$IFDEF WIN64}Append('WIN64');{$ENDIF}           // Target platform is 64-bit Windows.
  {$IFDEF LINUX}Append('LINUX');{$ENDIF}           // Since Kylix
  {$IFDEF LINUX32}Append('LINUX32');{$ENDIF}       // Since Kylix
  {$IFDEF POSIX}Append('POSIX');{$ENDIF}           // Since Kylix
  {$IFDEF POSIX32}Append('POSIX32');{$ENDIF}       // Since Kylix
  {$IFDEF IOS}Append('IOS');{$ENDIF}               // Defined if the target platform is iOS.
  {$IFDEF MACOS}Append('MACOS');{$ENDIF}           // Target platform is Mac OS X.
  {$IFDEF MACOS}Append('MACOS32');{$ENDIF}         // Target platform is 32-bit Mac OS X.
  {$IFDEF ANDROID}Append('ANDROID');{$ENDIF}       // Defined if the target platform is Android.
  {$IFDEF GUI}Append('GUI');{$ENDIF}               // Defined if an application is being compiled as a GUI application.
  {$IFDEF CONSOLE}Append('CONSOLE');{$ENDIF}       // Defined if an application is being compiled as a console application.
  {$IFDEF UNICODE}Append('UNICODE');{$ENDIF}       // UNICODE is defined as the default string type.
  {$IFDEF NATIVECODE}Append('NATIVECODE');{$ENDIF} // Since Delphi.Net
  {$IFDEF CPUX86}Append('CPUX86');{$ENDIF}         // CPU 8086 support
  {$IFDEF CPU386}Append('CPU386');{$ENDIF}         // CPU 80386 support
  {$IFDEF CPUX64}Append('CPUX64');{$ENDIF}         // CPU x64 support
  {$IFDEF CPUARM}Append('CPUARM');{$ENDIF}         // CPU ARM support
  {$IFDEF RAM3GB}Append('RAM3GB');{$ENDIF}         // Use 3Gb RAM
  {$IFDEF FASTRUN}Append('FASTRUN');{$ENDIF}       // Fast run from removable and network
  {$IFDEF NEXTGEN}Append('NEXTGEN');{$ENDIF}       // Defined for compilers (such as the Delphi mobile compilers) that use "next-generation" language features, such as 0-based strings.
  {$IFDEF DEBUG}Append('DEBUG');{$ENDIF}           // Debug configiration links
  {$IFDEF PRERELEASE}Append('PRERELEASE');{$ENDIF} // Pre-release configiration links
  {$IFDEF RELEASE}Append('RELEASE');{$ENDIF}       // Release configiration links
  // Additionals
  {$IFDEF DeDEBUG}Append('DeDEBUG');{$ENDIF}       // Additional logging configiration links
  {$IFDEF DBCO_HTTP}Append('DBCO_HTTP');{$ENDIF}
  {$IFDEF MSG_ON}Append('MSG_ON');{$ENDIF}
  {$IFDEF NewBase}Append('NewBase');{$ENDIF}
  {$IFDEF SQLNCLI}Append('SQLNCLI');{$ENDIF}
  {$IFDEF USER_HAVE_ONCE_PASSWORD}Append('USER_HAVE_ONCE_PASSWORD');{$ENDIF}
  {$IFDEF MICROSOFTCOM}Append('MICROSOFTCOM');{$ENDIF}
  {$IFDEF OPENOFFICE}Append('OPENOFFICE');{$ENDIF}
  {$IFDEF ACTIONICONTYPED}Append('ACTIONICONTYPED');{$ENDIF}
  {$IFDEF ACTIONICONREAL}Append('ACTIONICONREAL');{$ENDIF}
  {$IFDEF DANGEROUSFUNCTIONS}Append('DANGEROUSFUNCTIONS');{$ENDIF}
  {$IFDEF OPENSSL}Append('OPENSSL');{$ENDIF}
  {$IFDEF INDYUSED}Append('INDYUSED');{$ENDIF}
  {$IFDEF OLDPLUGIONS}Append('OLDPLUGIONS');{$ENDIF}
  {$IFDEF VECTOR_MAP}Append('VECTOR_MAP');{$ENDIF}
  if Length(Result) <> 0 then
    Result := #13#10'                        Project definitions: ' + Result;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  LogDirectory := IncludeTrailingPathDelimiter(GetApplicationDataDirectory + cApplicationName);
  ReadParameterLog(LogFullDateTime, LogMaxSize);
  if LogMaxSize > 0 then
    LogMaxSize := LogMaxSize * 1024 * 1024
  else
    LogMaxSize := 0;
  {$IFDEF DEBUG}
  DebugLog('DeLog unit initialization ...');
  {$ENDIF}
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DeLog unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

