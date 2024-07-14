unit DeReport;

interface

uses Windows, SysUtils, Classes, Contnrs, Forms,
     DeTypes, DeVariable;

type
  TDeReportParamDefs = class;

  TDeReportParamDef = class
  private
    FOwner: TDeReportParamDefs;
    FName: string;
    FType: TElementType;
    FValue: Variant;
//    FALeft: Integer;
//    FARight: Integer;
//    FTopLeft: TPoint;
//    FSize: TSize;
    FTag: Integer;
    FCaption: string;
    FFilter: string;
    FSource: string;
  public
    constructor Create(AOwner: TDeReportParamDefs; const AName: string);
    destructor Destroy; override;
    property Name: string read FName;
    property Caption: string read FCaption write FCaption;
    property ParamType: TElementType read FType write FType;
    property Value: Variant read FValue write FValue;
//    property AnchorLeft: Integer read FALeft write FALeft;
//    property AnchorRight: Integer read FARight write FARight;
//    property TopLeft: TPoint read FTopLeft write FTopLeft;
//    property Top: Integer read FTopLeft.Y write FTopLeft.Y;
//    property Left: Integer read FTopLeft.X write FTopLeft.X;
//    property Size: TSize read FSize write FSize;
//    property Height: Integer read FSize.cY write FSize.cY;
//    property Width: Integer read FSize.cX write FSize.cX;
    property Tag: Integer read FTag write FTag;
    property Filter: string read FFilter write FFilter;
    property Source: string read FSource write FSource;
  end;

  TDeReportParamDefs = class
  private
    FItems: TStringList;
    function GetParamCount: Integer;
    function GetParamDef(const Index: Integer): TDeReportParamDef;
    function GetParamDefByName(const aName: string): TDeReportParamDef;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(AParamDef: TDeReportParamDef);
    procedure Delete(const Index: Integer);
    procedure Remove(AParamDef: TDeReportParamDef);
    function IndexOf(AParamDef: TDeReportParamDef): Integer; overload;
    function IndexOf(const Name: string): Integer; overload;
    property ParamCount: Integer read GetParamCount;
    property ParamDef[const Index: Integer]: TDeReportParamDef read GetParamDef;
    property ParamDefByName[const aName: string]: TDeReportParamDef read GetParamDefByName;
  end;

type
  TDeReportProgressEvent = procedure(Sender: TObject; aPosition, aMax: Integer) of object;

type
  EDeReplaceStringsError = class(Exception);

  TDeReplaceString = class
  private
    FFromString: string;
    FToString: string;
  public
    constructor Create(const AFromString, AToString: string);
    function Replace(const Value: string; const ReplaceFlags: TReplaceFlags = [rfReplaceAll]): string;
    property FromString: string read FFromString;
    property ToString: string read FToString;
  end;

  TDeReplaceStrings = class
  private
    FList: TObjectList;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TDeReplaceString;
    function InternalAdd(const FromString, ToString: string): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOf(const FromString: string): Integer;
    function Add(const FromString, ToString: string): Integer;
    procedure Delete(const Index: Integer);
    procedure Clear;
    procedure MergeFromStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    function Replace(const Value: string; const ReplaceFlags: TReplaceFlags = [rfReplaceAll]): string;
    property Count: Integer read GetCount;
    property Item[const Index: Integer]: TDeReplaceString read GetItem; default;
  end;

type
  TDeReport = class
  private
    FReportName: string;
    FFileName: string;
    FTargetFileName: string;
    FReplaceList: TDeReplaceStrings;
    function GetReplaceList: TDeReplaceStrings;
    procedure SetFileName(const aFileName: string);
    function GetTargetFileName: string;
  protected
    FParamDefs: TDeReportParamDefs;
    FParams: TDeVariableList;
    procedure LoadParamDefs; virtual;
    function GetParamDefs: TDeReportParamDefs; virtual;
    procedure SetTargetFileName(const AFileName: string); virtual;

    function GetParam(const sParam, sFormat: string; var vValue: Variant): Boolean;
    function GetGlobalVariable(const sParam, sFormat: string; var vValue: Variant): Boolean;
    function GetGlobalParam(const sParam, sFormat: string; var vValue: Variant): Boolean;

    procedure doPreview; virtual;
    procedure doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); virtual;
    procedure doPrintToFile(const aFileName: string); virtual;
    procedure doSaveToFile(const aFileName: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    class function IsSupportExtension(const Extension: string): Boolean; dynamic;
    procedure Preview;
    procedure Print(const PrinterName: string; const CopyCount: Integer = 1; const SplitCopies: Boolean = True);
    procedure PrintToFile(const aFileName: string);
    procedure SaveToFile(const aFileName: string);

    property ParamDefs: TDeReportParamDefs read GetParamDefs;
    property Params: TDeVariableList read FParams;
    property ReportName: string read FReportName write FReportName;
    property FileName: string read FFileName write SetFileName;
    property ReplaceList: TDeReplaceStrings read GetReplaceList;
    // v. 17.4
    property TargetFileName: string read GetTargetFileName write SetTargetFileName;
  end;

implementation

uses Variants, RTLConsts, Types,
     DeLog, Funcs, DeMeta, DeMetadata, DeScript, DeParser;

{ TDeReportParamDef }

constructor TDeReportParamDef.Create(AOwner: TDeReportParamDefs; const AName: string);
begin
  inherited Create;
  FOwner := AOwner;
  FName := AName;
  FType := etNone;
  FValue := Unassigned;
//  FALeft := 0;
//  FARight := 0;
//  FTopLeft := Point(0,0);
//  FSize.cx := 0;
//  FSize.cy := 0;
  FTag := 0;
  if Assigned(FOwner) then FOwner.Add(Self);
end;

destructor TDeReportParamDef.Destroy;
begin
  if Assigned(FOwner) then FOwner.Remove(Self);
  inherited Destroy;
end;

{ TDeReportParamDefs }

constructor TDeReportParamDefs.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
  //FItems.Sorted := true;
  //FItems.Duplicates := dupError;
end;

destructor TDeReportParamDefs.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

function TDeReportParamDefs.GetParamCount: Integer;
begin
  Result := FItems.Count;
end;

function TDeReportParamDefs.GetParamDef(const Index: Integer): TDeReportParamDef;
begin
  Result := TDeReportParamDef(FItems.Objects[Index]);
end;

function TDeReportParamDefs.GetParamDefByName(const aName: string): TDeReportParamDef;
var
  Index: Integer;
begin
  Index := FItems.IndexOf({Ansi}UpperCase(aName));
  if Index <> -1 then
    Result := ParamDef[Index]
  else
    Result := nil;
end;

procedure TDeReportParamDefs.Clear;
begin
  while ParamCount > 0 do ParamDef[Pred(ParamCount)].Free;
end;

procedure TDeReportParamDefs.Add(AParamDef: TDeReportParamDef);
begin
  if IndexOf(AParamDef.Name) = -1 then
    begin
      FItems.AddObject(AParamDef.Name, AParamDef);
      AParamDef.FOwner := Self;
    end
  else
    raise EStringListError.CreateRes(@sDuplicateString);
end;

procedure TDeReportParamDefs.Delete(const Index: Integer);
begin
  FItems.Delete(Index);
end;

procedure TDeReportParamDefs.Remove(AParamDef: TDeReportParamDef);
var
  Index: Integer;
begin
  Index := IndexOf(AParamDef);
  if Index <> -1 then Delete(Index);
end;

function TDeReportParamDefs.IndexOf(AParamDef: TDeReportParamDef): Integer;
begin
  Result := FItems.IndexOfObject(AParamDef);
end;

function TDeReportParamDefs.IndexOf(const Name: string): Integer;
begin
  Result := FItems.IndexOf(UpperCase(Name));
end;

{ TDeReport }

constructor TDeReport.Create;
begin
  inherited Create;
  {
  // Закомментировал, т.к. по умолчанию при создании объекта переменные уже имеют такие значения.
  FReportName := EmptyStr;
  FFileName := EmptyStr;
  FParamDefs := nil;
  }
  FParams := TDeVariableList.Create;
end;

destructor  TDeReport.Destroy;
begin
  FreeAndNil(FReplaceList);
  FreeAndNil(FParamDefs);
  FParams.Free;
  inherited Destroy;
end;

function TDeReport.GetParamDefs: TDeReportParamDefs;
begin
  if not Assigned(FParamDefs) then
    begin
      FParamDefs := TDeReportParamDefs.Create;
      LoadParamDefs;
    end;
  Result := FParamDefs;
end;

function TDeReport.GetParam(const sParam, sFormat: string; var vValue: Variant): Boolean;
var
  aParam: TVariableItem;
begin
  aParam := FParams.GetByName(sParam, False);
  Result := Assigned(aParam);
  if Result then
    try
      vValue := FormatValue(aParam.Value, sFormat);
      Result := True;
    except
      on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog('TDeReport.GetParam(%s, %s, %s) error: %s', [QuotedStr(sParam), QuotedStr(sFormat),
            VariantToString(vValue), E.Message]);
          {$ENDIF}
          Result := False;
        end;
    end;
end;

function TDeReport.GetGlobalParam(const sParam, sFormat: string; var vValue: Variant): Boolean;
var
  ParamItem: TParamItem;
begin
  ParamItem := MetaData.Parameters.FindParam(sParam);
  Result := Assigned(ParamItem);
  if Result then
    try
      vValue := FormatValue(ParamItem.Value, sFormat);
    except
      on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog('TDeReport.GetGlobalParam(%s, %s, %s) error: %s', [QuotedStr(sParam), QuotedStr(sFormat),
            VariantToString(vValue), E.Message]);
          {$ENDIF}
          Result := False;
        end;
    end;
end;

function TDeReport.GetGlobalVariable(const sParam, sFormat: string; var vValue: Variant): Boolean;
var
  VariableItem: TVariableItem;
begin
  VariableItem := GlobalVariables.FindVariable(sParam);
  Result := Assigned(VariableItem);
  if Result then
    try
      vValue := FormatValue(VariableItem.Value, sFormat);
    except
      on E: Exception do
        begin
          {$IFDEF DEBUG}
          DebugLog('TDeReport.GetGlobalVariable(%s, %s, %s) error: %s', [QuotedStr(sParam), QuotedStr(sFormat),
            VariantToString(vValue), E.Message]);
          {$ENDIF}
          Result := False;
        end;
    end;
end;

procedure TDeReport.doPreview;
begin
  {$IFDEF DEBUG}
  DebugLog('TDeReport.doPreview ...');
  {$ENDIF}
end;

procedure TDeReport.doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean);
begin
  {$IFDEF DEBUG}
  DebugLog('TDeReport.doPrint(%s, %d, %s) ...', [QuotedStr(PrinterName), CopyCount, StrPas(BooleanNames[SplitCopies])]);
  {$ENDIF}
end;

procedure TDeReport.doPrintToFile(const aFileName: string);
begin
  {$IFDEF DEBUG}
  DebugLog('TDeReport.doPrintToFile(%s) ...', [QuotedStr(aFileName)]);
  {$ENDIF}
end;

procedure TDeReport.doSaveToFile(const aFileName: string);
begin
  {$IFDEF DEBUG}
  DebugLog('TDeReport.doSaveToFile(%s) ...', [QuotedStr(aFileName)]);
  {$ENDIF}
end;

procedure TDeReport.SetFileName(const aFileName: string);
begin
  FFileName := aFileName;
  FParams.SetByName(varSourceFileName, FFileName);
end;

function TDeReport.GetTargetFileName: string;
begin
  if Length(FTargetFileName) = 0 then
    begin
      FTargetFileName := FFileName;
      FParams.SetByName(varTargetFileName, FTargetFileName);
    end;
  Result := FTargetFileName;
end;

procedure TDeReport.SetTargetFileName(const AFileName: string);
var
  Value: string;
begin
  Value := ExtractFilePath(AFileName);
  if Length(Value) = 0 then
    begin
      Value := ExtractFilePath(FileName);
      if Length(Value) <> 0 then Value := IncludeTrailingBackslash(Value);
      Value := Value + ExtractFileName(AFileName);
    end
  else
    Value := AFileName;
  FTargetFileName := Value;
  FParams.SetByName(varTargetFileName, FTargetFileName);
end;

procedure TDeReport.Preview;
begin
  {$IFDEF DEBUG}
  if Assigned(FParams) then
    FParams.DebugVariablesLog('TDeReport.Preview: MetaData.UpdateLibrary ...')
  else
    DebugLog('TDeReport.Preview: MetaData.UpdateLibrary ...');
  {$ENDIF}
  MetaData.UpdateLibrary;
  doPreview;
end;

procedure TDeReport.Print(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean);
begin
  {$IFDEF DEBUG}
  if Assigned(FParams) then
    FParams.DebugVariablesLog(Format('TDeReport.Print(%s, %d, %s): MetaData.UpdateLibrary ...',
      [QuotedStr(PrinterName), CopyCount, StrPas(BooleanNames[SplitCopies])]))
  else
    DebugLog('TDeReport.Print(%s, %d, %s): MetaData.UpdateLibrary ...', [QuotedStr(PrinterName),
      CopyCount, StrPas(BooleanNames[SplitCopies])]);
  {$ENDIF}
  MetaData.UpdateLibrary;
  doPrint(PrinterName, CopyCount, SplitCopies);
end;

procedure TDeReport.PrintToFile(const aFileName: string);
begin
  {$IFDEF DEBUG}
  if Assigned(FParams) then
    FParams.DebugVariablesLog(Format('TDeReport.PrintToFile(%s): MetaData.UpdateLibrary ...', [QuotedStr(aFileName)]))
  else
    DebugLog('TDeReport.PrintToFile(%s): MetaData.UpdateLibrary ...', [QuotedStr(aFileName)]);
  {$ENDIF}
  MetaData.UpdateLibrary;
  doPrintToFile(aFileName);
end;

procedure TDeReport.SaveToFile(const aFileName: string);
begin
  {$IFDEF DEBUG}
  DebugLog('TDeReport.SaveToFile(%s): MetaData.UpdateLibrary ...', [QuotedStr(aFileName)]);
  {$ENDIF}
  MetaData.UpdateLibrary;
  doSaveToFile(aFileName);
end;

class function TDeReport.IsSupportExtension(const Extension: string): Boolean;
begin
  // По умолчанию класс не поддерживает никаких расширений файлов!
  Result := False;
end;

procedure TDeReport.LoadParamDefs;
begin
  { Ничего не делаем! }
end;

function TDeReport.GetReplaceList: TDeReplaceStrings;
begin
  if not Assigned(FReplaceList) then
    FReplaceList := TDeReplaceStrings.Create;
  Result := FReplaceList;
end;

{ TDeReplaceString }

constructor TDeReplaceString.Create(const AFromString, AToString: string);
begin
  FFromString := AFromString;
  FToString := AToString;
end;

function TDeReplaceString.Replace(const Value: string; const ReplaceFlags: TReplaceFlags): string;
begin
  Result := StringReplace(Value, FromString, ToString, ReplaceFlags);
end;

{ TDeReplaceStrings }

constructor TDeReplaceStrings.Create;
begin
  FList := TObjectList.Create(True);
end;

destructor TDeReplaceStrings.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TDeReplaceStrings.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDeReplaceStrings.GetItem(const Index: Integer): TDeReplaceString;
begin
  Result := FList[Index] as TDeReplaceString;
end;

function TDeReplaceStrings.IndexOf(const FromString: string): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Pred(Count) do
    if CompareStr(FromString, Item[Index].FromString) = 0 then
      begin
        Result := Index;
        Break;
      end;
end;

function TDeReplaceStrings.InternalAdd(const FromString, ToString: string): Integer;
var
  NewItem: TDeReplaceString;
begin
  Result := -1;
  NewItem := TDeReplaceString.Create(FromString, ToString);
  try
    Result := FList.Add(NewItem);
  finally
    if Result = -1 then NewItem.Free;
  end;
end;

function TDeReplaceStrings.Add(const FromString, ToString: string): Integer;
resourcestring
  sDuplicateErrorFmt = 'Duplicate from string value %d.';
begin
  Result := IndexOf(FromString);
  if Result <> -1 then
    raise EDeReplaceStringsError.CreateResFmt(@sDuplicateErrorFmt, [Result]);
  Result := InternalAdd(FromString, ToString);
end;

procedure TDeReplaceStrings.Delete(const Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TDeReplaceStrings.Clear;
begin
  FList.Clear;
end;

procedure TDeReplaceStrings.MergeFromStream(Stream: TStream);
var
  Count, Index: Integer;
  FromString, ToString: string;
  function ReadString: string;
  var
    Size: Integer;
  begin
    Stream.ReadBuffer(Size, SizeOf(Size));
    SetLength(Result, Size);
    Stream.ReadBuffer(Result[1], Size * SizeOf(Char));
  end;
begin
  Stream.ReadBuffer(Count, SizeOf(Count));
  for Index := 0 to Pred(Count) do
    begin
      FromString := ReadString;
      ToString := ReadString;
      Add(FromString, ToString);
    end;
end;

procedure TDeReplaceStrings.LoadFromStream(Stream: TStream);
begin
  Clear;
  MergeFromStream(Stream);
end;

procedure TDeReplaceStrings.SaveToStream(Stream: TStream);
var
  Count, Index: Integer;
  procedure WriteString(const Value: string);
  var
    Size: Integer;
  begin
    Size := Length(Value);
    Stream.WriteBuffer(Size, SizeOf(Size));
    if Size <> 0 then
      Stream.WriteBuffer(Value[1], Size * SizeOf(Char));
  end;
begin
  Count := Self.Count;
  Stream.WriteBuffer(Count, SizeOf(Count));
  for Index := 0 to Pred(Count) do
    with Item[Index] do
      begin
        WriteString(FromString);
        WriteString(ToString);
      end;
end;

function TDeReplaceStrings.Replace(const Value: string; const ReplaceFlags: TReplaceFlags): string;
var
  Index: Integer;
begin
  Result := Value;
  for Index := 0 to Pred(Count) do
    Result := Item[Index].Replace(Result, ReplaceFlags);
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DeReport unit initialization ...');

finalization
  DebugLog('DeReport unit finalization ...');
{$ENDIF}

end.

