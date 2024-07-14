unit DeSettings;

interface

uses Windows, SysUtils, Classes, Registry, DeTypes{, Dictionary};


resourcestring
   ERegDataTypeError    = 'Error data or datatype';
   ERegCantDeleteError  = 'Cant delete variable ''%s''';
   ERegCantSaveError    = 'Cant create or save variable ''%s''';
   ERegCantLoadError    = 'Cant load variables';
   ERegVarExistsError   = 'Variable ''%s'' is not exists';
   ERegConvertError     = 'Datatype conversion error';

type

  TVarRec = record
               Name    : AnsiString;
               Buf     : Pointer;
               BufSize : integer;
               DataType: DWORD;
             end;

  PVarRec = ^TVarRec;

  TRegVars = class
  private
    FReg  : TRegistry;
    FVars : TList;
    FAutomatic : Boolean;
    function GetAsDateTime(const VarName: string): TDateTime;
    function GetAsString(const VarName: string): AnsiString;
    function GetAsBoolean(const VarName: string): Boolean;
    function GetAsDouble(const VarName: string): Double;
    function GetAsInteger(const VarName: string): Integer;
    function GetAsByte(const VarName: string): Byte;
    function GetAsWord(const VarName: string): Word;
    procedure SetAsBoolean(const VarName: string; const Value: Boolean);
    procedure SetAsDateTime(const VarName: string; const Value: TDateTime);
    procedure SetAsString(const VarName: string; const Value: AnsiString);
    procedure SetAsDouble(const VarName: string; const Value: Double);
    procedure SetAsInteger(const VarName: string; const Value: Integer);
    procedure SetAsByte(const VarName: string; const Value: Byte);
    procedure SetAsWord(const VarName: string; const Value: Word);
    function FindVariable(const VarName: string): Integer;
    function AddVar(const VarName: string; Value: Variant): Integer;
    procedure SetInteger(AVar: PVarRec; const Value: Integer);
    procedure SetPChar(AVar: PVarRec; Value: PAnsiChar);
    procedure SetBinary(AVar: PVarRec; Value: Pointer; const ValSize: Integer);
    function GetInteger(const VarName: string): integer;
    //function GetPChar(const VarName: string): PAnsiChar;
    procedure GetBinary(const VarName: string; Buffer: Pointer; const BufSize: Integer);
    procedure SaveVar(const Index: Integer; const OpenKey: Boolean = True);
    procedure DeleteVar(const Index: Integer);
    function GetVar(const VarName: string): PVarRec;
    function GetAnsiString(const VarName: string): AnsiString;
  public
    constructor Create(const Automatic: Boolean = True);
    destructor Destroy; override;
    function VarExists(const VarName: string): Boolean;
    procedure AddVariable(const VarName: string; const Value: Variant);
    procedure DeleteVariable(const VarName: string);
    procedure LoadVariables(const Merged: Boolean = False);
    procedure SaveVariables;
    {$IFDEF DEBUG}
    procedure DebugVarsLog(const Text: string);
    {$ENDIF}
    /// <summary>Метод загрузки переменных из параметров командной строки приложения</summary>
    procedure LoadFromParameters;
    // v. 17.07
    function PrepareCommandLineParameters: string;
    property AsString[const VarName: string]: AnsiString read GetAsString write SetAsString;
    property AsBoolean[const VarName: string]: Boolean read GetAsBoolean write SetAsBoolean;
    property AsInteger[const VarName: string]: Integer read GetAsInteger write SetAsInteger;
    property AsWord[const VarName: string]: Word read GetAsWord write SetAsWord;
    property AsByte[const VarName: string]: Byte read GetAsByte write SetAsByte;
    property AsFloat[const VarName: string]: Double read GetAsDouble write SetAsDouble;
    property AsDateTime[const VarName: string]: TDateTime read GetAsDateTime write SetAsDateTime;
   // property Variables [VarName : string]: Variant   read GetVariable   write SetVariable;
    property PVars[const VarName: string]: PVarRec read GetVar;
  end;

//  function RegTypeByVarType(Value : Variant):DWORD;


var
  Variables : TRegVars;


implementation

uses Variants, Types, StrUtils, DeLog, {$IFDEF DEBUG} uTextTable, {$ENDIF}
     Funcs, DeMetaData, DeVariable, uMapUtils, DeParser, DataUnit;

{ TRegVars }

{
function RegTypeByVarType(Value : Variant):DWORD;
begin
  case TVarData(Value).VType of
  varBoolean,
  varSmallint,
  varInteger,
  varWord,
  varLongWord : Result := REG_DWORD;
  varInt64,
  varSingle,
  varDouble,
  varCurrency,
  varDate,
  varArray     : Result := REG_BINARY;
  varOleStr,
  varUString,
  varString   : Result := REG_SZ;
  else
    Result := REG_NONE;
  end;
end;
}

constructor TRegVars.Create(const Automatic: Boolean);
begin
  FReg  := TRegistry.Create;
  FReg.RootKey := HKEY_CURRENT_USER;
  FVars := TList.Create;
  FAutomatic := Automatic;
  if FAutomatic then LoadVariables;
end;

destructor TRegVars.Destroy;
begin
  FReg.Free;
  FVars.Free;
  inherited;
end;

function TRegVars.FindVariable(const VarName: string): Integer;
var
  i : integer;
begin
  Result := -1;
  for i := 0 to Pred(FVars.Count) do
    if SameText(TVarRec(FVars[i]^).Name,VarName) then
    begin
      Result := i;
      Break;
    end;
end; 

function TRegVars.AddVar(const VarName: string; Value: Variant): Integer;
var
  NewVar : PVarRec;
  NewStr : AnsiString;
begin
  NewVar := New(PVarRec);
  New(NewVar^.Buf);
  NewVar^.Name := VarName;

  case TVarData(Value).VType of
  varBoolean,
  varSmallint,
  varInteger,
  varShortInt,
  varByte,
  varWord,
  varLongWord : SetInteger(NewVar,Value);

  varInt64    :SetBinary(NewVar,@TVarData(Value).VInt64,SizeOf(TVarData(Value).VInt64));
  varSingle   :SetBinary(NewVar,@TVarData(Value).VSingle,SizeOf(TVarData(Value).VSingle));
  varDouble   :SetBinary(NewVar,@TVarData(Value).VDouble,SizeOf(TVarData(Value).VDouble));
  varCurrency :SetBinary(NewVar,@TVarData(Value).VCurrency,SizeOf(TVarData(Value).VCurrency));
  varDate     :SetBinary(NewVar,@TVarData(Value).VDate,SizeOf(TVarData(Value).VDate));

  varOleStr,
  varString,
  varUString   : begin
                   NewStr := VarToStr(Value);
                   SetPChar(NewVar,PAnsiChar(NewStr));
                 end;

  else
    begin
      Dispose(NewVar);
      raise Exception.Create(ERegDataTypeError);
    end
  end;
  result := FVars.Add(NewVar);
end;


procedure TRegVars.DeleteVar(const Index: Integer);
var
  NewRec   : PVarRec;
begin
  NewRec := FVars[Index];
  FReg.OpenKey(RegKey,True);
  if not FReg.DeleteValue(NewRec^.Name) then
    raise Exception.CreateFmt(ERegCantDeleteError,[NewRec^.Name]);
  FReg.CloseKey;
end;

procedure TRegVars.SaveVar(const Index: Integer; const OpenKey: Boolean);
var
  NewRec   : TVarRec;
begin
   if OpenKey then  FReg.OpenKey(regKey,True);
   NewRec := PVarRec(FVars[Index])^;
   if RegSetValueExA(FReg.CurrentKey, PAnsiChar(NewRec.Name), 0, NewRec.DataType, NewRec.Buf,  NewRec.BufSize) <> ERROR_SUCCESS then
     raise Exception.CreateFmt(ERegCantSaveError,[NewRec.Name]);
   if OpenKey then FReg.CloseKey;
end;

function TRegVars.VarExists(const VarName: string): Boolean;
begin
  result := (FindVariable(VarName) >= 0);
end;

procedure TRegVars.AddVariable(const VarName: string; const Value: Variant);
var
  VarIndex : integer;
begin
  if not VarExists(VarName) then
  begin
    VarIndex := AddVar(VarName,Value);
    if FAutomatic and (VarIndex >=0) then
      SaveVar(VarIndex);
  end;
end;

procedure TRegVars.DeleteVariable(const VarName: string);
var
  i : integer;
begin
  i := FindVariable(VarName);
  if i >= 0  then
  begin
    DeleteVar(i);
    Dispose(FVars.Extract(FVars[I]));
  end;
end;

procedure TRegVars.LoadVariables(const Merged: Boolean);
var
  Names   : TStringList;
  Index   : Integer;
  NewVar  : PVarRec;
  VarIndex: Integer;
  Buffer: PAnsiChar;
  BufferSize: Integer;
  BufferString: AnsiString;
begin
  Names := TStringList.Create;
  try
    if not Merged then FVars.Clear;
    try
      FReg.OpenKey(RegKey, False);
      try
        FReg.GetValueNames(Names);
        for Index := 0 to Pred(Names.Count) do
          begin
            VarIndex := FindVariable(Names[Index]);
            if VarIndex <> -1 then
              begin
                case FReg.GetDataType(Names[Index]) of
                  rdInteger: SetInteger(PVarRec(FVars[VarIndex]), FReg.ReadInteger(Names[Index]));
                  rdBinary:
                    begin
                      BufferSize := FReg.GetDataSize(Names[Index]);
                      GetMem(Buffer, BufferSize);
                      try
                        FReg.ReadBinaryData(Names[Index], Buffer, BufferSize);
                        SetBinary(PVarRec(FVars[VarIndex]), Buffer, BufferSize);
                      finally
                        FreeMem(Buffer);
                      end;
                    end;
                  rdString, rdExpandString:
                    begin
                      BufferString := FReg.ReadString(Names[Index]);
                      SetPChar(PVarRec(FVars[VarIndex]), PAnsiChar(BufferString));
                    end;
                end;
              end
            else
              begin
                NewVar := New(PVarRec);
                New(NewVar^.Buf);
                NewVar^.Name := Names[Index];
                NewVar^.BufSize := FReg.GetDataSize(Names[Index]);
                GetMem(NewVar^.Buf, NewVar^.BufSize);
                NewVar^.DataType := REG_NONE;
                if RegQueryValueExA(FReg.CurrentKey, PAnsiChar(NewVar^.Name), nil, @NewVar^.DataType, NewVar^.Buf, @NewVar^.BufSize) = ERROR_SUCCESS then
                  FVars.Add(NewVar)
                else
                  begin
                    FreeMem(NewVar^.Buf);
                    Dispose(NewVar^.Buf);
                    Dispose(NewVar);
                  end;
              end;
          end;
      finally
        FReg.CloseKey;
      end;
    except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('%s.LoadVariables error: %s', [ClassName, E.Message]);
        {$ENDIF}
        raise Exception.Create(ERegCantLoadError);
      end;
    end;
  finally
    Names.Free;
  end;
end;

procedure TRegVars.SaveVariables;
var
  Index: Integer;
begin
  if not FAutomatic then
    begin
      FReg.OpenKey(RegKey, True);
      try
        for Index := 0 to Pred(FVars.Count) do
          SaveVar(Index, False);
      finally
        FReg.CloseKey;
      end;
    end;
end;

procedure TRegVars.GetBinary(const VarName: string; Buffer: Pointer; const BufSize: Integer);
var
  i : integer;
begin
  i := FindVariable(VarName);
  if i >= 0 then
  begin
    if BufSize >= PVarRec(FVars[i])^.BufSize then
    begin
      System.Move(PVarRec(FVars[i])^.Buf^,Buffer^,PVarRec(FVars[i])^.BufSize);
    end
    else
      raise Exception.Create(ERegConvertError);
  end
  else
    raise Exception.CreateFmt(ERegVarExistsError,[VarName]);
end;

function TRegVars.GetAnsiString(const VarName: string): AnsiString;
var
  Index: Integer;
begin
  Index := FindVariable(VarName);
  if Index <> -1 then
    begin
      SetLength(Result, PVarRec(FVars[Index])^.BufSize);
      Move(PVarRec(FVars[Index])^.Buf^, Result[1], PVarRec(FVars[Index])^.BufSize);
      Delete(Result, Length(Result), 1);
    end
  else
    raise Exception.CreateFmt(ERegVarExistsError, [VarName]);
end;

{
function TRegVars.GetPChar(const VarName: string): PAnsiChar;
var
  i : integer;
begin
  i := FindVariable(VarName);
  if i >= 0 then
  begin
    GetMem(Result,PVarRec(FVars[i])^.BufSize);
  //  System.Move(PVarRec(FVars[i])^.Buf,Result,PVarRec(FVars[i])^.BufSize);
    Result := StrLCopy(Result, PVarRec(FVars[i])^.Buf, PVarRec(FVars[i])^.BufSize);
  end
  else
    raise Exception.CreateFmt(ERegVarExistsError,[VarName]);
end;
}

function TRegVars.GetInteger(const VarName: string): Integer;
var
  i : integer;
begin
  i := FindVariable(VarName);
  if i >= 0 then
    if (REG_DWORD = PVarRec(FVars[i])^.DataType) then
        System.Move(PVarRec(FVars[i])^.Buf^,Result,PVarRec(FVars[i])^.BufSize)
    else
      raise Exception.Create(ERegConvertError)
  else
    raise Exception.CreateFmt(ERegVarExistsError,[VarName]);
end;


procedure TRegVars.SetBinary(AVar: PVarRec; Value: Pointer; const ValSize: Integer);
begin
 { if Assigned(AVar^.Buf) then
    FreeMem(AVar^.Buf);}
  AVar^.BufSize := ValSize;
  GetMem(AVar^.Buf, AVar^.BufSize);
  System.Move(Value^, AVar^.Buf^, AVar^.BufSize);
  AVar^.DataType := REG_BINARY;
end;

procedure TRegVars.SetInteger(AVar: PVarRec; const Value: Integer);
begin
  {if Assigned(AVar^.Buf) then
    FreeMem(AVar^.Buf);}
  AVar^.BufSize := SizeOf(Value{Integer});
  GetMem(AVar^.Buf, AVar^.BufSize);
  System.Move(Value, AVar^.Buf^, AVar^.BufSize);
  AVar^.DataType := REG_DWORD;
end;

procedure TRegVars.SetPChar(AVar: PVarRec; Value: PAnsiChar);
begin
 { if Assigned(AVar^.Buf) then
    FreeMem(AVar^.Buf);}
  AVar^.BufSize := Succ(StrLen(Value));
  GetMem(AVar^.Buf, AVar^.BufSize);
  AVar^.Buf := StrLCopy(AVar^.Buf,Value,AVar^.BufSize);
  AVar^.DataType := REG_SZ;
end;

function TRegVars.GetAsDateTime(const VarName: string): TDateTime;
begin
  if Assigned(ConfigList) and (ConfigList.OverrideVariables.IndexByName(VarName) <> -1) then
    Result := ConfigList.OverrideVariables.GetValueByName(VarName)
  else
    GetBinary(VarName, @Result, SizeOf(Result));
end;

function TRegVars.GetAsDouble(const VarName: string): Double;
begin
  if Assigned(ConfigList) and (ConfigList.OverrideVariables.IndexByName(VarName) <> -1) then
    Result := ConfigList.OverrideVariables.GetValueByName(VarName)
  else
    GetBinary(VarName, @Result, SizeOf(Result));
end;

function TRegVars.GetAsString(const VarName: string): AnsiString;
begin
  if Assigned(ConfigList) and (ConfigList.OverrideVariables.IndexByName(VarName) <> -1) then
    Result := ConfigList.OverrideVariables.GetValueByName(VarName)
  else
    Result := GetAnsiString(VarName); //StrPas(GetPChar(VarName));
end;

function TRegVars.GetAsBoolean(const VarName: string): Boolean;
begin
  if Assigned(ConfigList) and (ConfigList.OverrideVariables.IndexByName(VarName) <> -1) then
    Result := ConfigList.OverrideVariables.GetValueByName(VarName) > 0
  else
    Result := (GetInteger(VarName) > 0);
end;

function TRegVars.GetAsInteger(const VarName: string): Integer;
begin
  if Assigned(ConfigList) and (ConfigList.OverrideVariables.IndexByName(VarName) <> -1) then
    Result := ConfigList.OverrideVariables.GetValueByName(VarName)
  else
    Result := GetInteger(VarName);
end;

function TRegVars.GetAsByte(const VarName: string): Byte;
begin
  try
    if Assigned(ConfigList) and (ConfigList.OverrideVariables.IndexByName(VarName) <> -1) then
      Result := ConfigList.OverrideVariables.GetValueByName(VarName)
    else
      Result := GetInteger(VarName);
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('%s.GetAsByte(%s) skip error: %s', [ClassName, QuotedStr(VarName), E.Message]);
        {$ENDIF}
        Result := 0;
      end;
  end;
end;

function TRegVars.GetAsWord(const VarName: string): Word;
begin
  try
    if Assigned(ConfigList) and (ConfigList.OverrideVariables.IndexByName(VarName) <> -1) then
      Result := ConfigList.OverrideVariables.GetValueByName(VarName)
    else
      Result := GetInteger(VarName);
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('%s.GetAsWord(%s) skip error: %s', [ClassName, QuotedStr(VarName), E.Message]);
        {$ENDIF}
        Result := 0;
      end;
  end;
end;

procedure TRegVars.SetAsBoolean(const VarName: string; const Value: Boolean);
var
  i : integer;
begin
  i := FindVariable(VarName);
  if i >= 0 then
    SetInteger(PVarRec(FVars[i]),Integer(Value))
  else
    i := AddVar(VarName,Value);
  if FAutomatic then SaveVar(i);
end;

procedure TRegVars.SetAsDateTime(const VarName: string; const Value: TDateTime);
var
  i : integer;
begin
  i := FindVariable(VarName);
  if i >= 0 then
    SetBinary(PVarRec(FVars[i]),@Value, SizeOf(Value))
  else
    i := AddVar(VarName,Value);
  if FAutomatic then SaveVar(i);
end;

procedure TRegVars.SetAsString(const VarName: string; const Value: AnsiString);
var
  i : integer;
begin
  i := FindVariable(VarName);
  if i >= 0 then
    SetPChar(PVarRec(FVars[i]),PAnsiChar(Value))
  else
    i := AddVar(VarName,Value);
  if FAutomatic then SaveVar(i);
end;


procedure TRegVars.SetAsDouble(const VarName: string; const Value: Double);
var
  i : integer;
begin
  i := FindVariable(VarName);
  if i >= 0 then
    SetBinary(PVarRec(FVars[i]),@Value, SizeOf(Value))
  else
    i := AddVar(VarName,Value);
  if FAutomatic then SaveVar(i);
end;


procedure TRegVars.SetAsInteger(const VarName: string; const Value: Integer);
var
  i : integer;
begin
  i := FindVariable(VarName);
  if i >= 0 then
     SetInteger(PVarRec(FVars[i]),Value)
  else
    i := AddVar(VarName,Value);
  if FAutomatic then SaveVar(i);
end;


procedure TRegVars.SetAsByte(const VarName: string; const Value: Byte);
var
  i : integer;
begin
  i := FindVariable(VarName);
  if i >= 0 then
    SetInteger(PVarRec(FVars[i]),Integer(Value))
  else
    i := AddVar(VarName,Value);
  if FAutomatic then SaveVar(i);
end;


procedure TRegVars.SetAsWord(const VarName: string; const Value: Word);
var
  i : integer;
begin
  i := FindVariable(VarName);
  if i >= 0 then
    SetInteger(PVarRec(FVars[i]),Integer(Value))
  else
    i := AddVar(VarName,Value);
  if FAutomatic then SaveVar(i);
end;

function TRegVars.GetVar(const VarName: string): PVarRec;
var
  i : integer;
begin
  i := FindVariable(VarName);
  if i >= 0 then
    result := PVarRec(FVars[i])
  else
    result := nil;
end;

{$IFDEF DEBUG}
procedure TRegVars.DebugVarsLog(const Text: string);
  function PrepareVarsListLog: string;
  var
    TextTable: TTextTable;
    Index, IntValue: Integer;
    VarRec: PVarRec;
    StrValue: AnsiString;
  begin
    if FVars.Count = 0 then
      Result := EmptyStr
    else
      begin
        TextTable := TTextTable.Create;
        try
          TextTable.Columns.Add('No', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Name', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Type', 0, taLeftJustify, taLeftJustify);
          TextTable.Columns.Add('Value', 0, taLeftJustify, taLeftJustify);
          for Index := 0 to Pred(FVars.Count) do
            begin
              TextTable.Lines[Index][0] := IntToStr(Succ(Index));
              VarRec := PVarRec(FVars[Index]);
              if Assigned(VarRec) then
                begin
                  TextTable.Lines[Index][1] := VarRec^.Name;
                  case VarRec^.DataType of
                    REG_DWORD:
                      begin
                        TextTable.Lines[Index][2] := 'REG_DWORD';
                        IntValue := 0;
                        System.Move(VarRec^.Buf^, IntValue, VarRec^.BufSize);
                        TextTable.Lines[Index][3] := IntToStr(IntValue);
                      end;
                    REG_SZ:
                      begin
                        TextTable.Lines[Index][2] := 'REG_SZ';
                        if VarRec^.BufSize > 1 then
                          begin
                            SetLength(StrValue, Pred(VarRec^.BufSize));
                            System.Move(VarRec^.Buf^, StrValue[1], VarRec^.BufSize);
                            TextTable.Lines[Index][3] := StrValue;
                          end;
                      end;
                  else
                    if VarRec^.DataType = REG_BINARY then
                      TextTable.Lines[Index][2] := 'REG_BINARY'
                    else
                      TextTable.Lines[Index][2] := UpperCase(IntToHex(VarRec^.DataType, 8)) + 'H';
                    IntValue := 1;
                    StrValue := EmptyStr;
                    while IntValue <= VarRec^.BufSize do
                      begin
                        if Length(StrValue) <> 0 then
                          StrValue := StrValue + ' ';
                        StrValue := StrValue + UpperCase(IntToHex(PByteArray(VarRec^.Buf)^[IntValue], 2));
                        Inc(IntValue);
                      end;
                    TextTable.Lines[Index][3] := StrValue;
                  end;
                end;
            end;
          Result := #13#10 + TextTable.AsText(24);
        finally
          TextTable.Free;
        end;
      end;
  end;
begin
  DebugLog(Text + PrepareVarsListLog);
end;
{$ENDIF}

procedure TRegVars.LoadFromParameters;
var
  Index, Position: Integer;
  Value, ParamName: string;
  IntValue: Integer;
  ExtValue: Extended;
  DateValue: TDateTime;
begin
  for Index := 1 to ParamCount do
    begin
      Value := ParamStr(Index);
      if (Pos('-', Value) = 1) or (Pos('/', Value) = 1) then
        begin
          System.Delete(Value, 1, 1);
          Position := Pos('=', Value);
          if Position <> 0 then
            begin
              ParamName := Copy(Value, 1, Pred(Position));
              if FindVariable(ParamName) <> -1 then
                begin
                  Value := Copy(Value, Succ(Position), Length(Value));
                  if TryStrToInt(Value, IntValue) then
                    AsInteger[ParamName] := IntValue
                  else if TryStrToFloat(Value, ExtValue) then
                    AsFloat[ParamName] := ExtValue
                  else if TryStrToDateTime(Value, DateValue) then
                    AsDateTime[ParamName] := DateValue
                  else
                    AsString[ParamName] := Value;
                end
              else
                begin
                  {$IFDEF DeDEBUG}
                  Funcs.WriteLog('Registry variable ' + QuotedStr(ParamName) + ' not found and skip for command line parameters!');
                  {$ENDIF}
                end;
            end;
        end;
    end;
end;

function TRegVars.PrepareCommandLineParameters: string;
  procedure Append(const Name: string; const Value: Integer);
  begin
    if Length(Result) <> 0 then Result := Result + ' ';
    Result := Result + Format('%s=%d', [Name, Value]);
  end;
begin
  Result := EmptyStr;
  {$IFDEF SQLNCLI}
  if Variables.AsInteger[RegDriverMSSQL] <> 0 then
    Append('-' + RegDriverMSSQL, Variables.AsInteger[RegDriverMSSQL]);
  {$ENDIF}
  Append('-' + RegMenuS, Variables.AsInteger[RegMenuS]);
  if Variables.AsInteger[RegShowCardArea] <> 1 then
    Append('/' + RegShowCardArea, Variables.AsInteger[RegShowCardArea]);
  if Variables.AsInteger[RegShowStatus] <> 1 then
    Append('/' + RegShowStatus, Variables.AsInteger[RegShowStatus]);
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
var
  i: Integer;
  s, FileName: string;
  procedure UpdateFromVariables(SourceVariables: TDeVariableList);
  var
    Index: Integer;
  begin
    if Assigned(SourceVariables) then
      for Index := 0 to Pred(SourceVariables.Count) do
        case VarType(SourceVariables[Index].Value) of
          varShortInt, varSmallint, varInteger:
            Variables.AsInteger[SourceVariables[Index].Name] := SourceVariables[Index].Value;
          varSingle, varDouble:
            Variables.AsFloat[SourceVariables[Index].Name] := SourceVariables[Index].Value;
          varByte:
            Variables.AsByte[SourceVariables[Index].Name] := SourceVariables[Index].Value;
          varWord:
            Variables.AsWord[SourceVariables[Index].Name] := SourceVariables[Index].Value;
          varEmpty, varNull: { Пропускаем!!! };
        else
          Variables.AsString[SourceVariables[Index].Name] := SourceVariables[Index].Value;
        end;
  end;
  procedure UpdateVariablesFromXML(const Defined: Boolean);
  var
    OverrideVariables: TDeVariableList;
  begin
    OverrideVariables := TDeVariableList.Create;
    try
      OverrideVariables.LoadFromFile(FileName, Defined);
      UpdateFromVariables(OverrideVariables);
    finally
      OverrideVariables.Free;
    end;
  end;
  {
  procedure UpdateVariablesFromParameters;
  var
    Variables: TDeVariableList;
  begin
    Variables := TDeVariableList.Create;
    try
      Variables.LoadFromParameters;
      UpdateFromVariables(Variables);
    finally
      Variables.Free;
    end;
  end;
  }
begin
  {$IFDEF DEBUG}
  DebugLog('DeSettings unit initialization ...');
  {$ENDIF}
  Variables := TRegVars.Create(False);

  // 1. EXE значения по умолчанию ...
  Variables.AddVariable(RegEndBits,0);
  Variables.AddVariable(RegOdd,0);
  Variables.AddVariable(RegSpeed,6);
  Variables.AddVariable(RegTray,0);
  Variables.AddVariable(RegPort,1);
  {$IFDEF DBCO_HTTP}
  Variables.AddVariable(RegHTTP,0);
  Variables.AddVariable(RegPortHTTP,80);
  {$ENDIF}
  Variables.AddVariable(RegStop,13);
  Variables.AddVariable(RegTime,25);
  Variables.AddVariable(RegAnim,7);
  Variables.AddVariable(RegDForm,0);
  Variables.AddVariable(RegMenuA,2);
  Variables.AddVariable(RegMenuS,0); // v 16.8. По умолчанию разворачивать меню в стандартное!
  Variables.AddVariable(RegMenuL,0);
  Variables.AddVariable(RegMenuT,5);
  Variables.AddVariable(RegMenuH,400);
  Variables.AddVariable(RegMenuW,200);
  Variables.AddVariable(RegLockSession,0);
  Variables.AddVariable(RegLockTime,15);

  Variables.AddVariable(RegDragDrop, 0);
  {$IFDEF SQLNCLI}
  Variables.AddVariable(RegDriverMSSQL, 0);
  {$ENDIF}
  Variables.AddVariable(RegImageStoreMode, 1);
  Variables.AddVariable(RegImageUnpackExt, sExtensionZIP  + ';' + sExtensionJPG + ';' + sExtensionJPEG + ';' +
                                           sExtensionDOCX + ';' + sExtensionXLSX);
  Variables.AddVariable(RegLogFile, 0);
  Variables.AddVariable(RegLogQuery, 0);

  Variables.AddVariable(RegBit, 8);
  Variables.AddVariable(RegStart, 0);
  Variables.AddVariable(RegCheckUpdates, 0); // не проверяем по умолчанию
  Variables.AddVariable(RegCopyAllField, 0);
  Variables.AddVariable(RegCopyHeaderField, 0);
  Variables.AddVariable(RegLastSolution, EmptyStr);
  Variables.AddVariable(RegLastUpdateMess, EmptyStr);
  Variables.AddVariable(RegLastUpdateDate, 0);
  Variables.AddVariable(RegLastImportPath, EmptyStr);
  Variables.AddVariable(RegDirPath, EmptyStr);
  Variables.AddVariable(RegDefaultDataPath, EmptyStr);
  Variables.AddVariable(RegLang, LANG_NEUTRAL);
  Variables.AddVariable(RegGridBrush, 0);
  Variables.AddVariable(RegGridLines, 1);
  Variables.AddVariable(RegShowShortcuts, 1);
  Variables.AddVariable(RegShowCardArea, 1);
  Variables.AddVariable(RegShowHint, 1);
  Variables.AddVariable(RegShowStatus, 1);
  Variables.AddVariable(RegAutoSaveChanges, 1);

  Variables.AddVariable(RegToolCard_SmallIcons, 0);
  if (GetSystemMetrics(SM_CXSCREEN) < 1920) or (GetSystemMetrics(SM_CYSCREEN) < 1080)
    then Variables.AddVariable(RegToolBars_IcoSize, 24)
    else Variables.AddVariable(RegToolBars_IcoSize, 32);
  Variables.AddVariable(RegToolBars_ShowCaptions,1);
  s:= EmptyStr;
  for i := Low(SystemToolbars) to High(SystemToolbars) do
    if i = Low(SystemToolbars) then s:= SystemToolbars[i]
                               else s:= s + ',' + SystemToolbars[i];
  Variables.AddVariable(RegToolBars_List, s);
  Variables.AddVariable(RegToolBars_ShortCuts, EmptyStr);
  Variables.AddVariable(RegToolBars_Visible, 1); // v.16.8 - отображать панель инструментов по умолчанию

  Variables.AddVariable(RegRemStartup, false);
  Variables.AddVariable(RegSoundOn, false);
  Variables.AddVariable(RegRemSoundPath, 'alert.wav');

  Variables.AddVariable(RegPrintHeight, -1);
  Variables.AddVariable(RegPrintWidth, -1);

  Variables.AddVariable(RegCommandHeight, -1);
  Variables.AddVariable(RegCommandWidth, -1);
  Variables.AddVariable(RegCommandLastID, -1);
  Variables.AddVariable(RegCommandViewStyle, 3); // vsReport

  Variables.AddVariable(RegApplicationTitle, cAppTitle);
  Variables.AddVariable(RegApplicationHomeURL, urlHome);
  Variables.AddVariable(RegApplicationMailURL, urlMail);
  Variables.AddVariable(RegFetchRecord, 1000);
  Variables.AddVariable(RegMapBrightness, 128);
  Variables.AddVariable(RegMapCacheEnabled, False);
  Variables.AddVariable(RegMapBaseDirectory, EmptyStr);
  Variables.AddVariable(RegHintTime, -1);
  Variables.AddVariable(RegCountInQuery, 100);

  {$IFDEF DEBUG}
  Variables.DebugVarsLog('1. EXE значения по умолчанию ...');
  {$ENDIF}

  FileName := ExtractFilePath(GetModuleName(hInstance)) + ConfigXML;
  if FileExists(FileName) then
    begin
      // 2. Значения по умолчанию из XML файла ...
      UpdateVariablesFromXML(True);
      {$IFDEF DEBUG}
      Variables.DebugVarsLog('2. Значения по умолчанию из XML файла ...');
      {$ENDIF}
    end;
  // 3. Пользовательские настройки из реестра ...
  Variables.LoadVariables(True);
  {$IFDEF DEBUG}
  Variables.DebugVarsLog('3. Пользовательские настройки из реестра ...');
  {$ENDIF}
  if FileExists(FileName) then
    begin
      // 4. Перекрываемые значения из XML файла ...
      UpdateVariablesFromXML(False);
      {$IFDEF DEBUG}
      Variables.DebugVarsLog('4. Перекрываемые значения из XML файла ...');
      {$ENDIF}
    end;
  // 5. Параметры из командной строки ...
  // UpdateVariablesFromParameters;
  Variables.LoadFromParameters;
  {$IFDEF DEBUG}
  Variables.DebugVarsLog('5. Параметры из командной строки ...');
  {$ENDIF}
  Variables.FAutomatic := True;
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DeSettings unit finalization ...');
  {$ENDIF}
  FreeAndNil(Variables);
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

