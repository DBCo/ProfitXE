unit SolutionOperations;

interface

uses Windows, SysUtils, Classes,
     DataManager, DataCacheUnit, DeMeta, {DSMeta, }DeMatrix, DeParser{, DeActions}, IndicatorForm;

type
  TMatrixArray = array of array of TFieldsMeta;

  TTableContiguityMatrix = class
  private
    FMatrix : TMatrixArray;
    FList   : TTablesList;
    FOnCompare : TCompareEvent;
    function GetSize : integer; inline;
    function GetChildLinksCount(const Index: Integer): Integer;
    function GetParentLinksCount(const Index: Integer): Integer;
    function GetLink(const Index1, Index2: Integer): Boolean; inline;
    function GetLinkField(const Index1, Index2: Integer): TFieldMeta;
    function CompareDirect(const Index1, Index2: Integer): Integer;
    function CompareReverse(const Index1, Index2: Integer): Integer;
    procedure Sort;
    procedure ClearDouble;
  public
    constructor Create;
    destructor Destroy;  override;
    property Objects : TTablesList read FList;
    property Size : integer read GetSize;
    property ChildLinksCount[const Index: Integer]: Integer read GetChildLinksCount;
    property ParentLinksCount[const Index: Integer]: Integer read GetParentLinksCount;
    property Link[const Index1, Index2: Integer]: Boolean read GetLink;
    property LinkField[const Index1, Index2: Integer]: TFieldMeta read GetLinkField;
    procedure AddTable(aTable : TTableMeta);
    procedure AddLink(aLinkField : TFieldMeta);
    procedure RemoveLink(aLinkField : TFieldMeta);
    procedure Exchange(const Index1, Index2: Integer);
    procedure SortDirect;
    procedure SortReverse;
    procedure Move(const FromIndex, ToIndex: Integer);
{$ifdef DeDEBUG}
    procedure Display;
{$endif}
  end;

  TSeparatorsState = (ssOriginal, ssSpecial);

  /// <summary>DataManager для выгрузки и загрузки конфигурации</summary>
  TConfigManager = class(TDataManager)
  private
    FMatrix     : TTableContiguityMatrix;  // матрица смежности
    FID: Integer;                // ID решения
    FPath: string;               // путь к файлу конфигурации
    FFile: string;               // имя файла конфигурации
    FCacheList  : TDataCacheList;       // список кэшей, хранящих выгружаемые данные
    FDataStream : TFileStream;          // поток, в который записываются данные конфигурации
    FConfigName: String;
    FConfigDeveloper: String;
    FConfigVersion: String;
    FIndicator  : TfrmIndicator;
    FSeparatorsState     : TSeparatorsState;
    FBKThousandSeparator : char;
    FBKDecimalSeparator  : char;
    FBKDateSeparatot     : char;
    FBKTimeSeparator     : char;
    FBKShortDateFormat   : string;
    FBKLongTimeFormat    : string;
    FTempVarValue        : Variant;
    function GetTempVarValue(aVariable : TVariableItem) : Variant;
    procedure SetPath(const aPath : string);
    function GetTotalRecordsCount : integer;
    procedure BuildMatrix(const aExclude : array of string);
    procedure SwitchSeparators(const NewState: TSeparatorsState);
    procedure ItemAsStrings(aItem : TCacheItem;  aStrings : TStringList);
    procedure ProcessChildLinks(aIndex : integer;  aCache : TDataCache; aLoadTo : TDataCache);
    procedure ProcessParentLinks(aIndex : integer;  aCache : TDataCache; aLoadTo : TDataCache);
    procedure ProcessSelfLinks(aIndex : integer;  aCache : TDataCache; aLoadTo : TDataCache);
    procedure PrepareCache(aCache : TDataCache;  aReverse : boolean = false);
    procedure MergeDictionary(aIndex : integer);
    procedure StoreDatabases;
    procedure StoreTableData(aIndex : integer);
    procedure DeleteConfigData;
  public
    constructor Create;
    destructor Destroy;  override;
    function LoadConfig: boolean;                                                  // загрузка конфигурации
    function ExtractConfig(const aPath : string;  const aID : integer) : boolean;  // выгрузка конфигурации
    function DeleteConfig(const aID : integer) : boolean;                          // удаление конфигурации

    property DataPath : string read FPath write SetPath;
    property DataFile : string read FFile;
    property ConfigName : string read FConfigName;
    property ConfigDeveloper : string read FConfigDeveloper;
    property ConfigVersion : string read FConfigVersion;
    property Indicator : TfrmIndicator read FIndicator write FIndicator;
    property TotalRecordsCount : integer read GetTotalRecordsCount;
  end;

implementation

uses Types, Variants, IniFiles, Contnrs, DB, Controls, Forms,
     DeLog, Funcs, DeTypes, Dictionary, DeDB, DeMetadata, QueryDescriptor, DataUnit;

{ TTableContiguityMatrix }

constructor TTableContiguityMatrix.Create;
begin
  inherited Create;
  FList := TTablesList.Create(false);
end;

destructor TTableContiguityMatrix.Destroy;
var I, J : integer;
begin
  for I := 0 to Size-1 do
    for J := 0 to Size-1 do
      FMatrix[I, J].Free;
  FMatrix := nil;
  FList.Free;
  inherited Destroy;
end;

function TTableContiguityMatrix.GetSize: integer;
begin
  result := FList.Count;
end;

function TTableContiguityMatrix.GetChildLinksCount(const Index: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Pred(Index) do
    if Assigned(FMatrix[I, Index]) then Inc(Result);
end;

function TTableContiguityMatrix.GetParentLinksCount(const Index: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Pred(Index) do
    if Assigned(FMatrix[Index, I]) then Inc(Result);
end;

function TTableContiguityMatrix.GetLink(const Index1, Index2: Integer): Boolean;
begin
  Result := Assigned(FMatrix[Index1, Index2]);
end;

function TTableContiguityMatrix.GetLinkField(const Index1, Index2: Integer): TFieldMeta;
begin
  if Assigned(FMatrix[Index1, Index2]) and (FMatrix[Index1, Index2].Count > 0) then
    Result := FMatrix[Index1, Index2][0]
  else
    Result := nil;
end;

function TTableContiguityMatrix.CompareDirect(const Index1, Index2: Integer): Integer;
begin
  if Assigned(FMatrix[Index1, Index2]) then Result := -1 else
  if Assigned(FMatrix[Index2, Index1]) then Result := 1 else
                                            Result := 0;
end;

function TTableContiguityMatrix.CompareReverse(const Index1, Index2: Integer): Integer;
begin
  if Assigned(FMatrix[Index1, Index2]) then Result := 1 else
  if Assigned(FMatrix[Index2, Index1]) then Result := -1 else
                                            Result := 0;
end;

procedure TTableContiguityMatrix.Sort;
var i, j : integer;
begin
  if Assigned(FOnCompare) then
    for i:=0 to Size-1 do
      for j:=i+1 to Size-1 do
        begin
          if FOnCompare(i, j) > 0 then
            Exchange(i, j);
        end;
end;

procedure TTableContiguityMatrix.AddTable(aTable: TTableMeta);
var I, TableIndex : integer;
begin
  if FList.IndexOf(aTable) < 0 then
    begin
      FList.Add(aTable);
      SetLength(FMatrix, Size, Size);
      { добавляем связи к родительским таблицам }
      for I := 0 to aTable.Fields.Count-1 do
        if aTable.Fields[I].IsLookup then
          begin
            TableIndex := FList.IndexOf(aTable.Fields[I].Owner);
            if TableIndex >= 0 then
              AddLink(aTable.Fields[I].LookupPair);
          end;
      { добавляем связи дочерних таблиц }
      for I := 0 to aTable.Links.Count-1 do
        begin
          TableIndex := FList.IndexOf(aTable.Links[I].Owner);
          if TableIndex >= 0 then
            AddLink(aTable.Links[I].Owner.Fields.FindByID(aTable.Links[I].ID));
        end;
    end;
end;

procedure TTableContiguityMatrix.AddLink(aLinkField: TFieldMeta);
var ParentIndex, ChildIndex : integer;
begin
  ParentIndex := FList.IndexOf(aLinkField.LookupPair.Owner);
  ChildIndex := FList.IndexOf(aLinkField.Owner);
  Assert(ParentIndex >=0, 'Parent table not found');
  Assert(ChildIndex >=0, 'Child table not found');
  if (not Assigned(FMatrix[ParentIndex, ChildIndex])) or
    (FMatrix[ParentIndex, ChildIndex].IndexOf(aLinkField) < 0) then
    begin
      if not Assigned(FMatrix[ParentIndex, ChildIndex]) then
        FMatrix[ParentIndex, ChildIndex] := TFieldsMeta.Create(false);
      FMatrix[ParentIndex, ChildIndex].Add(aLinkField);
    end;
end;

procedure TTableContiguityMatrix.RemoveLink(aLinkField: TFieldMeta);
var ParentIndex, ChildIndex : integer;
begin
  // Здесь падала AV, т.к. aLinkField может быть nil!
  // Для примера выбрать решение в справочнике решений и в действиях выбрать "Удалить решение".
  // Заменил закомментированный код ...
  // +
  //if aLinkField.LookupPair=nil then Exit;
  if not (Assigned(aLinkField) and Assigned(aLinkField.LookupPair)) then Exit;
  // -
  ParentIndex := FList.IndexOf(aLinkField.LookupPair.Owner);
  ChildIndex := FList.IndexOf(aLinkField.Owner);
  Assert(ParentIndex >=0, 'Parent table not found');
  Assert(ChildIndex >=0, 'Child table not found');
  FMatrix[ParentIndex, ChildIndex].Extract(aLinkField);
  if FMatrix[ParentIndex, ChildIndex].Count = 0 then
    FreeAndNil(FMatrix[ParentIndex, ChildIndex]);
end;

procedure TTableContiguityMatrix.Exchange(const Index1, Index2: Integer);
var tmpValue  : TFieldsMeta;
    I         : integer;
begin
  // переставляем колонки
  for I := 0 to Size-1 do
    begin
      tmpValue := FMatrix[I, Index1];
      FMatrix[I, Index1] := FMatrix[I, Index2];
      FMatrix[I, Index2] := tmpValue;
    end;
  // переставляем строки
  for I := 0 to Size-1 do
    begin
      tmpValue := FMatrix[Index1, I];
      FMatrix[Index1, I] := FMatrix[Index2, I];
      FMatrix[Index2, I] := tmpValue;
    end;
  // переставляем объекты
  FList.Exchange(Index1, Index2);
end;

procedure TTableContiguityMatrix.SortDirect;
begin
  FOnCompare := CompareDirect;
  Sort;
end;

procedure TTableContiguityMatrix.SortReverse;
begin
  FOnCompare := CompareReverse;
  Sort;
end;

procedure TTableContiguityMatrix.Move(const FromIndex, ToIndex: Integer);
var
  Index: Integer;
begin
  for Index := FromIndex to Pred(ToIndex) do
    Exchange(Index, Succ(Index));
end;

{$ifdef DeDEBUG}
procedure TTableContiguityMatrix.Display;
const BoolStr : array[boolean] of byte = (0, 1);
var I, J, MaxSize, MaxNumSize : integer;
    TitleFmt, NumFmt, DisplayStr : string;
begin
  // готовим форматы для вывода данных
  MaxSize := 0;
  for I := 0 to Size-1 do
    if Length(Objects[I].Table) > MaxSize then
      MaxSize := Length(Objects[I].Table);
  MaxNumSize := Length(IntToStr(Size));
  TitleFmt := '%'+IntToStr(MaxNumSize)+'d. %-'+IntToStr(MaxSize)+'s  ';
  if not odd(MaxNumSize) then
    inc(MaxNumSize);
  NumFmt := ' %'+IntToStr(MaxNumSize)+'d ';

  // выводим заголовочную часть таблицы
  DisplayStr := #13#10 + StringOfChar('-', Length(IntToStr(Size))+2+MaxSize+2+Size*(MaxNumSize+2)) + #13#10;
  DisplayStr := DisplayStr + StringOfChar(' ', Length(IntToStr(Size))+2+MaxSize+2);
  for I := 0 to Size-1 do
    DisplayStr := DisplayStr + Format(NumFmt, [I]);
  DisplayStr := DisplayStr + #13#10 + StringOfChar('-', Length(IntToStr(Size))+2+MaxSize+2+Size*(MaxNumSize+2)) + #13#10;

  // выводим данные
  for I := 0 to Size-1 do
  begin
    DisplayStr := DisplayStr +
      Format(TitleFmt, [I, Objects[I].Table]);
    for J := 0 to Size-1 do
      if not Assigned(FMatrix[J, I]) then
        DisplayStr := DisplayStr + '   - '//Format(NumFmt, [0])
      else
        DisplayStr := DisplayStr + Format(NumFmt, [FMatrix[J, I].Count]);
    DisplayStr := DisplayStr + #13#10;
  end;

  Funcs.WriteLog(DisplayStr);
end;
{$endif}

{ TConfigManager }

constructor TConfigManager.Create;
begin
  inherited Create;
  FCacheList := TDataCacheList.Create;
end;

destructor TConfigManager.Destroy;
begin
  FCacheList.Free;
  FMatrix.Free;
  inherited Destroy;
end;

procedure TConfigManager.SetPath(const aPath : string);
begin
  FPath := aPath;
  FFile:= ExtractFilePath(FPath);
end;

function TConfigManager.GetTotalRecordsCount : integer;
var I : integer;
begin
  result := 0;
  for I := 0 to FCacheList.Count-1 do
    result := result + TDataCache(FCacheList[I]).Count;
end;

procedure TConfigManager.BuildMatrix(const aExclude : array of string);
var I, J     : integer;
    Excluded : boolean;
begin
  // наполняем матрицу смежности наборами данных
  FMatrix := TTableContiguityMatrix.Create;
  with FMatrix do
    for I := Low(MetaTableNames) to High(MetaTableNames) do
      if Assigned(MetaData.MetaTables[I]) then    // для выгрузки старых решений
        begin
          Excluded := false;
          for J := 0 to Length(aExclude)-1 do
            if CompareText(MetaTableNames[I], aExclude[J]) = 0 then
              begin
                Excluded := true;
                break;
              end;
          if not Excluded then
            AddTable(MetaData.MetaTables[I]);
        end;
end;

procedure TConfigManager.SwitchSeparators(const NewState: TSeparatorsState);
begin
  if FSeparatorsState <> NewState then
    begin
      if NewState = ssSpecial then
        begin
          FBKThousandSeparator := FormatSettings.ThousandSeparator;
          FBKDecimalSeparator := FormatSettings.DecimalSeparator;
          FBKDateSeparatot := FormatSettings.DateSeparator;
          FBKTimeSeparator := FormatSettings.TimeSeparator;
          FBKShortDateFormat := FormatSettings.ShortDateFormat;
          FBKLongTimeFormat := FormatSettings.LongTimeFormat;

          FormatSettings.ThousandSeparator := DeThousandSeperator;
          FormatSettings.DecimalSeparator := DeDecimalSeparator;
          FormatSettings.DateSeparator := DeDateSeparator;
          FormatSettings.TimeSeparator := DeTimeSeparator;
          FormatSettings.ShortDateFormat := DeShortDateFormat;
          FormatSettings.LongTimeFormat := DeLongTimeFormat;
        end
      else
        begin
          FormatSettings.ThousandSeparator := FBKThousandSeparator;
          FormatSettings.DecimalSeparator := FBKDecimalSeparator;
          FormatSettings.DateSeparator := FBKDateSeparatot;
          FormatSettings.TimeSeparator := FBKTimeSeparator;
          FormatSettings.ShortDateFormat := FBKShortDateFormat;
          FormatSettings.LongTimeFormat := FBKLongTimeFormat;
        end;
      FSeparatorsState := NewState;
    end;
end;

procedure TConfigManager.ItemAsStrings(aItem : TCacheItem; aStrings : TStringList);
var I: integer;
    aValue: Variant;
    BlobStream: TFileStream;
    P: Pointer;
begin
  aStrings.Clear;
  BlobStream := nil;
  for I := 0 to aItem.Owner.FieldCount-1 do
    if not aItem.Owner.Fields[I].IsLookup then
      begin
        aValue := aItem.FieldValue[I];
        if VarIsEmpty(aValue) or VarIsNull(aValue) then aStrings.Add(EmptyStr) else
        if aItem.Owner.Fields[I].DataType in IntegerTypes then aStrings.Add(IntToStr(VarToInt(aValue))) else
        if aItem.Owner.Fields[I].DataType in DateTimeTypes then aStrings.Add(DateTimeToStr(VarAsType(aValue, varDate))) else
        if aItem.Owner.Fields[I].DataType in FloatTypes then aStrings.Add(FloatToStr(VarAsType(aValue, varDouble))) else
        if aItem.Owner.Fields[I].DataType in StringTypes then if aItem.Owner.Fields[I].CodePage = cpUTF8
                                                                then aStrings.Add(UTF8ToString(VarAsType(aValue, varString)))
                                                                else aStrings.Add(VarAsType(aValue, varString)) else
        if aItem.Owner.Fields[I].DataType in LogicalTypes then if aValue then aStrings.Add(TrueLexeme)
                                                                         else aStrings.Add(FalseLexeme) else
        if aItem.Owner.Fields[I].DataType in [ftBlob] then
          try
            aStrings.Add(aItem.Owner.TableMeta.Table+' '+VarToStr(aItem.ID)+'.dat');
            P := VarArrayLock(aValue);
            BlobStream := TFileStream.Create(FPath + aStrings[aStrings.Count-1], fmCreate or fmShareDenyWrite);
            BlobStream.Write(P, VarArrayHighBound(aValue, 1) - VarArrayLowBound(aValue, 1) + 1);
          finally
            VarArrayUnlock(aValue);
            BlobStream.Free;
          end;
      end;
end;

function TConfigManager.GetTempVarValue(aVariable : TVariableItem) : Variant;
begin
  result := FTempVarValue;
end;

procedure TConfigManager.ProcessChildLinks(aIndex : integer; aCache : TDataCache;  aLoadTo : TDataCache);
var FCaches     : TList;
    FFilters    : TObjectList;
    IndexList   : TIntegerDynArray;
    I           : integer;
    LinkField   : TFieldMeta;
    FilterItem  : TFilterItem;

  function ProcessFinished : boolean;
  var I : integer;
  begin
    // =true, если перебор записей завершен
    result := true;
    for I := 0 to Length(IndexList)-1 do
      if IndexList[I] < TDataCache(FCaches[I]).Count then Exit(false);
  end;

  procedure NextCombination;
  var ToInc : integer;
  begin
    // выбор следующей комбинации элементов
    ToInc := Length(IndexList)-1;
    while (ToInc > 0) and (IndexList[ToInc] >= (TDataCache(FCaches[ToInc]).Count-1)) do
      begin
        IndexList[ToInc] := 0;
        dec(ToInc);
      end;
    inc(IndexList[ToInc]);
  end;

begin
  // обрабатываем прямые связи (родитель-ребенок)
  FCaches := TList.Create;
  FFilters := TObjectList.Create;
  try
    IndexList := nil;
    // строим список кэшей-предков;  готовим условия связи с родительскими наборами данных
    for I := 0 to aIndex-1 do
      if FMatrix.Link[I, aIndex] then
        begin
          FCaches.Add(FCacheList.ItemsByID(FMatrix.Objects[I].ID, True));
          LinkField := FMatrix.LinkField[I, aIndex];
          FilterItem := TFilterItem.Create;
          with FilterItem[FilterItem.Add(TPostfixItem.Create)] do
            begin
              Ident := LinkField.Original;
              ResultType := LinkField.DataType;
            end;
          FilterItem[FilterItem.Add(TPostfixItem.Create)].Variable :=
            TVariableItem.Create(LinkField.DataType, 'V_'+LinkField.Original, [amRead, amWrite]);
          FilterItem.Variables.Add(FilterItem[FilterItem.Count-1].Variable);
          with FilterItem[FilterItem.Add(TPostfixItem.Create)] do
            begin
              BinaryOp := opEQ;
              ResultType := ftBoolean;
            end;
          aCache.Filters.AddFilter(FilterItem);
          FFilters.Add(FilterItem);
        end;
    // выгружаем данные
    if FCaches.Count > 0 then
      begin
        SetLength(IndexList, FCaches.Count);
        while not ProcessFinished do
          begin
    //TODO: -2014
            for I := 0 to FFilters.Count-1 do
              try
                if IndexList[I] >= TDataCache(FCaches[I]).Count then
                  TFilterItem(FFilters[I]).Variables[0].Value := 0
                else
                  begin
                    TFilterItem(FFilters[I]).Variables[0].Value := TDataCache(FCaches[I])[IndexList[I]].ID;
                    aCache.PrepareData;
                    aCache.FillAll;
                    aLoadTo.CopyFrom(aCache);
                  end;
              except
              end;
            NextCombination;
          end;
      end;
  finally
    // удаляем условия связи с родительскими наборами данных
    for I := 0 to FCaches.Count-1 do
      aCache.Filters.RemoveFilter(TFilterItem(FFilters[I]));
    // удаляем списки и динамические массивы
    FCaches.Free;
    FFilters.Free;
    IndexList := nil;
  end;
end;

procedure TConfigManager.ProcessParentLinks(aIndex : integer; aCache : TDataCache;  aLoadTo : TDataCache);
var I, J       : integer;
    KeyValue   : Variant;
    LinkIndex  : TIntegerDynArray;
    FCaches    : TList;
    FilterItem : TFilterItem;
begin
  // обрабатываем обратные связи (ребенок-родитель)
  FCaches := TList.Create;
  LinkIndex := nil;  FilterItem := nil;
  try
    // создаем условия для выборки из родительского набора данных
    FilterItem := TFilterItem.Create;
    with FilterItem[FilterItem.Add(TPostfixItem.Create)] do
      begin
        Ident := aCache.TableMeta.KField[0].Original;
        ResultType := aCache.TableMeta.KField[0].DataType;
      end;
    FilterItem[FilterItem.Add(TPostfixItem.Create)].Variable :=
      TVariableItem.Create(aCache.TableMeta.KField[0].DataType, 'V_' + aCache.TableMeta.KField[0].Original, [amRead, amWrite]);
    FilterItem.Variables.Add(FilterItem[FilterItem.Count-1].Variable);
    FilterItem.Variables[0].OnGetValue := GetTempVarValue;
    with FilterItem[FilterItem.Add(TPostfixItem.Create)] do
      begin
        BinaryOp := opEQ;
        ResultType := ftBoolean;
      end;
    aCache.Filters.AddFilter(FilterItem);
    // создаем список дочерних наборов данных
    SetLength(LinkIndex, FMatrix.ParentLinksCount[aIndex]);
    for I := 0 to aIndex-1 do
      if FMatrix.Link[aIndex, I] then
        begin
          FCaches.Add(FCacheList.ItemsByID(FMatrix.Objects[I].ID, True));
          for J := 0 to aCache.TableMeta.Links.Count-1 do
            if aCache.TableMeta.Links[J].Owner = FMatrix.Objects[I] then
              LinkIndex[FCaches.Count-1] :=
                FMatrix.Objects[I].Fields.IndexOf(FMatrix.Objects[I].Fields.FindByID(aCache.TableMeta.Links[J].ID));
        end;
    // выгружаем данные
    for I := 0 to FCaches.Count-1 do
      for J := 0 to TDataCache(FCaches[I]).Count-1 do
        begin
          KeyValue := TDataCache(FCaches[I])[J].FieldValue[LinkIndex[I]];
          if (not VarIsEmpty(KeyValue)) and (not VarIsNull(KeyValue)) and (not Assigned(aLoadTo.FindById(KeyValue))) then
            begin
              FTempVarValue := KeyValue;
              aCache.PrepareData;
              aCache.FillAll;
              aLoadTo.CopyFrom(aCache);
            end;
        end;
  finally
    // удаляем условия
    aCache.Filters.RemoveFilter(FilterItem);
    FilterItem.Free;
    // удаляем списки и динамические массивы
    FCaches.Free;
    LinkIndex := nil;
  end;
end;

procedure TConfigManager.ProcessSelfLinks(aIndex : integer; aCache : TDataCache;  aLoadTo : TDataCache);
var I          : integer;
    FilterItem : TFilterItem;
    KeyValue   : Variant;
begin
  // обрабатываем "уши" (связи с записями своей таблицы)
  FilterItem := nil;
  try
    // создаем условия для выборки из набора данных
    FilterItem := TFilterItem.Create;
    with FilterItem[FilterItem.Add(TPostfixItem.Create)] do
      begin
        Ident := aCache.TableMeta.KField[0].Original;
        ResultType := aCache.TableMeta.KField[0].DataType;
      end;
    FilterItem[FilterItem.Add(TPostfixItem.Create)].Variable :=
      TVariableItem.Create(aCache.TableMeta.KField[0].DataType, 'V_' + aCache.TableMeta.KField[0].Original, [amRead, amWrite]);
    FilterItem.Variables.Add(FilterItem[FilterItem.Count-1].Variable);
    FilterItem.Variables[0].OnGetValue := GetTempVarValue;
    with FilterItem[FilterItem.Add(TPostfixItem.Create)] do
      begin
        BinaryOp := opEQ;
        ResultType := ftBoolean;
      end;
    aCache.Filters.AddFilter(FilterItem);
    // выгружаем данные
    I := 0;
    while I < aLoadTo.Count do
      begin
        KeyValue := aLoadTo[I].FieldValue[aLoadTo.ParentIdIndex];
        if (not VarIsEmpty(KeyValue)) and (not VarIsNull(KeyValue)) and (not Assigned(aLoadTo.FindById(KeyValue))) then
          begin
            FTempVarValue :=KeyValue;
            aCache.PrepareData;
            aCache.FillAll;
            aLoadTo.CopyFrom(aCache);
          end;
        inc(I);
      end;
  finally
    // удаляем условия
    aCache.Filters.RemoveFilter(FilterItem);
    FilterItem.Free;
  end;
end;

procedure TConfigManager.PrepareCache(aCache : TDataCache; aReverse : boolean = false);
var I, N   : integer;
begin
   if aCache.ParentIDIndex >= 0 then
    if aReverse then
      begin
        // дочерние записи должны предшествовать родительским
        I := 0;
        while I < aCache.Count do
          begin
            N := aCache.IndexByValues([aCache.ParentIDIndex], [aCache[I].ID], I+1, aCache.Count-1);
            if -1 < N then aCache.CacheItems.Exchange(I, N)
                      else inc(I);
          end;
      end
    else
      begin
        // родительские записи должны предшествовать дочерним
        I := aCache.Count-1;
        while 0 <= I do
          begin
            N := aCache.IndexByValues([aCache.ParentIDIndex], [aCache[I].ID], 0, I-1);
            if -1 < N then aCache.CacheItems.Exchange(I, N)
                      else dec(I);
          end;
      end;
end;

procedure TConfigManager.MergeDictionary(aIndex : integer);
var CurrentCache    : TDataCache;
    TranslatedValue : Variant;
    DMan            : TDataManager;
    I, N, NameIndex, LangIndex, TranslatedIndex : integer;
    aCache          : TDataCache;
begin
  // объединение словарей
  aCache := TDataCache.Create(FMatrix.Objects[aIndex]);
  DMan := CreateDataManager(FMatrix.Objects[aIndex]);
  aCache.PrepareData;
  CurrentCache := FCacheList.ItemsByID(FMatrix.Objects[aIndex].ID, True);
  NameIndex := CurrentCache.Fields.IndexByName(fldDictionaryName);
  LangIndex := CurrentCache.Fields.IndexByName(fldDictionaryLangId);
  TranslatedIndex := CurrentCache.Fields.IndexByName(fldDictionaryTranslate);
  for I := 0 to CurrentCache.Count-1 do
    begin
      N := aCache.IndexByValues( [NameIndex, LangIndex], [CurrentCache[I].FieldValue[NameIndex],
                                                          CurrentCache[I].FieldValue[LangIndex]]);
      if -1 < N then
        begin
          TranslatedValue := CurrentCache[I].FieldValue[TranslatedIndex];
          if not VarSameValue(aCache[N].FieldValue[TranslatedIndex], TranslatedValue) then
            begin
              aCache[N].FieldValue[TranslatedIndex] := CurrentCache[I].FieldValue[TranslatedIndex];
              aCache.UpdateRecord(aCache[N]);
            end;
        end
      else
        begin
          DMan.PrepareRecord(CurrentCache[I]);
          CurrentCache[I].BeginInit;
          CurrentCache[I].InitFieldValueByNameExternal(fldDictionaryID, unassigned);
          CurrentCache[I].EndInit;
          DMan.InsertRecord(CurrentCache[I]);
        end;
      if Assigned(FIndicator) then
        FIndicator.Position := FIndicator.Position + 1;
    end;
  DMan.Free;
  aCache.Free;
end;

procedure TConfigManager.StoreDatabases;
var DBCache                    : TDataCache;
    I                          : integer;
    NotStored                  : TStringList;
    DBPath, tmpStr, DestFolder : string;
begin
  DBCache := FCacheList.ItemsByID(MetaData.MetaTables[idxBase].ID, True);
  DestFolder := ExtractFilePath(MetaData.MetadataDB.ConnectString);
  if Length(DestFolder) > 0 then
    system.Delete(DestFolder, Length(DestFolder), 1);
  NotStored := TStringList.Create;
  try
    for I := 0 to DBCache.Count-1 do
      begin
       // удаляем пути к базам данных
        DBPath := DBCache[I].ValueByName[fldBaseConnectStr];
        if Length(DBPath) = 0 then
          DBPath := DBCache[I].ValueByName[fldBaseDatabase];
//      DBPath := ExtractFileName(DBPath);
        if CopyFile(pChar(FPath), pChar(DestFolder + '\' + DBPath), false)
          then SetFileAttributes( pChar(DestFolder + '\' + DBPath), FILE_ATTRIBUTE_ARCHIVE)
          else NotStored.Add(DBPath);
      end;
  finally
    if NotStored.Count > 0 then
      begin
        tmpStr := EmptyStr;
        for I := 0 to NotStored.Count-1 do
          StrAdd(tmpStr, ', ', NotStored[I]);
        Errors.Clear;
        if NotStored.Count = 1
          then Errors.Add(TAnyError.Create( Format(GetTitle('_dM.solcopydb'), [tmpStr, DestFolder])))
          else Errors.Add(TAnyError.Create( Format(GetTitle('_dM.solcopydb'), [tmpStr, DestFolder])));
      end;
    NotStored.Free;
  end;
end;

procedure TConfigManager.StoreTableData(aIndex : integer);
var I, J, K, L : integer;
    LinkIndex : TIntegerDynArray;
    ACache, LCache : TDataCache;
    Key, V : Variant;
    DMan : TDataManager;
begin
  DMan := CreateDataManager(FMatrix.Objects[aIndex]);
  try
    ACache := FCacheList.ItemsByID(FMatrix.Objects[aIndex].ID, True);

    // ставим родителей в начало списка
    PrepareCache(ACache);

    // сохраняем данные
    for I := 0 to ACache.Count-1 do
      begin
        Key := VarArrayCreate([0, 1], varVariant);
        VarArrayPut(Key, ACache[I].ID, [0]);  // получаем старый ключ

        DMan.PrepareRecord(ACache[I]);

     // ACache[I].BeginInit;
        ACache[I].InitFieldValue(ACache.IDIndex, unassigned); // очищаем ключ, чтобы сгенерить новый
     // ACache[I].EndInit;

        // меняем старую родительскую ссылку на новую, таблица должна быть обработана ранее
        for j:= 0 to Pred(ACache.Fields.Count) do
          if 0 < ACache.Fields[j].Link then
            begin
              V:= ACache[i].FieldValue[j];
              if VarIsType(V, [varNull, varEmpty]) then Continue;

              if ACache.Fields[j].Link = ACache.TableMeta.ID
                then  // дерево, ищем в текущем кэше
                      begin
                        for l := 0 to Pred(i) do
                          if VarCompareValue(V, ACache[l].FieldBaseValue[ACache.IDIndex]) = vrEqual then
                            begin
                              ACache[i].FieldValue[j]:= ACache[l].ID;
                              Break;
                            end
                      end
                else  // ищем в ранее загруженных кэшах
                      begin
                        for k := 0 to Pred(aIndex) do
                          if ACache.Fields[j].Link = FMatrix.Objects[k].ID then
                            begin
                              LCache:= FCacheList.ItemsByID(FMatrix.Objects[k].ID, True);
                              for l := 0 to Pred(LCache.Count) do
                                if VarCompareValue(V, LCache[l].FieldBaseValue[LCache.IDIndex]) = vrEqual then
                                   begin
                                     ACache[i].FieldValue[j]:= LCache[l].ID;
                                     Break;
                                   end;{}
                              Break;
                            end;
                      end;
            end;

        DMan.InsertRecord(ACache[I]);
        VarArrayPut(Key, ACache[I].ID, [1]); // получаем новый ключ

        ACache[I].BeginInit;
        ACache[I].InitFieldValue(ACache.IDIndex, Key); // сохраняем пару ключей в кеше для последующих замен
        ACache[I].EndInit;

        if Assigned(FIndicator) then
           FIndicator.Position := FIndicator.Position + 1;
      end;

    if CompareText(FMatrix.Objects[aIndex].Table, tblDataset) = 0 then
      MetaData.DefineTableLinks;
  finally
    LinkIndex := nil;
  end;
  DMan.Free;
end;

procedure TConfigManager.DeleteConfigData;
var I, J, TypeIndex : integer;
    CurrentCache    : TDataCache;
    CanDelete       : boolean;
    Q               : TDeDataset;
    DMan            : TDataManager;
begin
  for I := 0 to FMatrix.Size-1 do
    begin
      DMan := CreateDataManager(FMatrix.Objects[I]);
      CurrentCache := FCacheList.ItemsByID(FMatrix.Objects[I].ID, True);
      PrepareCache(CurrentCache, true);
      if CompareText(FMatrix.Objects[I].Table, tblMenu) <> 0 then
        for J := 0 to CurrentCache.Count-1 do
          DMan.DeleteRecord(CurrentCache[J], false)
      else
        begin
          TypeIndex := CurrentCache.Fields.IndexByName(fldMenuType);
          for J := 0 to CurrentCache.Count-1 do
            begin
              CanDelete:= true;
              if SameText( VarToStr(CurrentCache[J].FieldValue[TypeIndex]), CurrentCache.Fields[TypeIndex].Value1) then
                begin
                  Q := FMatrix.Objects[I].Database.CreateQuery(qtSelect);
                  try
                    Q.Descr.BeginUpdate;
                    try
                      Q.Descr.Table := FMatrix.Objects[I].Table;
                      Q.Descr.AddField(opCount);
                      Q.Descr.AddCondition( FMatrix.Objects[I].Fields.FindByName(fldMenuOwner).Original,
                                            FMatrix.Objects[I].Fields.FindByName(fldMenuOwner).DataType,
                                            opEQ, CurrentCache[J].ID);
                      Q.Descr.AddCondition( FMatrix.Objects[I].DField.Original, FMatrix.Objects[I].DField.DataType,
                                            opEQ, FMatrix.Objects[I].DField.Value2);
                      Q.Descr.AddOperation(opNot);
                      Q.Descr.AddOperation(opAnd);
                    finally
                      Q.Descr.EndUpdate;
                    end;
                    Q.Open;
                    CanDelete := Q.Value[0] = 0;
                  finally
                    Q.Free;
                  end;
                end;
              if CanDelete then
                DMan.DeleteRecord(CurrentCache[J], false)
            end;
        end;
    DMan.Free;
    if Assigned(FIndicator) then
      FIndicator.Position := FIndicator.Position + 1;
  end;
end;


procedure  TTableContiguityMatrix.ClearDouble;
var i,j,k,N,C : Integer;
begin
  for I := 0 to Size-1 do
    begin
      if assigned(FMatrix[i,i]) then
        begin
          FMatrix[i,i].Clear;
          FMatrix[i,i]:=nil;
        end;

      N:=-1;
      for j := 0 to i-1 do
        begin
          if n>-1 then
            begin
              if assigned(FMatrix[i,j]) then
                begin
                  FMatrix[i,j].Clear;
                  FreeAndNil(FMatrix[i,j]);
                end;

              if assigned(FMatrix[j,i]) then
                begin
                  FMatrix[j,i].Clear;
                  FreeAndNil(FMatrix[j,i]);
                end;
            end
          else
            begin
              if assigned(FMatrix[j,i]) and (0<FMatrix[j,i].Count) then
                begin
                  N:=j;

                  C:=0;
                  for k:= FMatrix[j,i].Count-1 downto 0 do
                    if FMatrix[j,i].Items[k].LinkType in [daCascade, daFullCascade] then Inc(C);

                  if 0<C then
                  while not (FMatrix[j,i].Items[0].LinkType in [daCascade, daFullCascade]) do
                    FMatrix[j,i].Delete(0);

                  while 1<FMatrix[j,i].Count do
                    FMatrix[j,i].Delete(1);
                end;

              if assigned(FMatrix[i,j]) and (0<FMatrix[i,j].Count) then
                begin
                  N:=j;

                  C:=0;
                  for k:= FMatrix[i,j].Count-1 downto 0 do
                    if FMatrix[i,j].Items[k].LinkType in [daCascade, daFullCascade] then Inc(C);

                  if 0<C then
                  while not (FMatrix[i,j].Items[0].LinkType in [daCascade, daFullCascade]) do
                    FMatrix[i,j].Delete(0);

                  while 1<FMatrix[i,j].Count do
                    FMatrix[i,j].Delete(1);
                end;
            end;
        end
    end;
end;

function TConfigManager.LoadConfig : boolean;
var II, I, J     : integer;
    BackupCursor : TCursor;
    ConfigStrings, ItemData: TStringList;
    TableMeta   : TTableMeta;
    FieldsIndex : TIntegerDynArray;
    DCache      : TDataCache;
    DItem       : TCacheItem;
    FType       : TFieldType;
    V           : Variant;
    Stream      : TFileStream;
    BlobData    : string;
begin
  result := false;
  if Length(Trim(DataPath)) > 0 then
    begin
      // готовим матрицу смежности
      BuildMatrix([tblUsers, tblNotes, tblInterrelations, tblRights, tblMembership, tblTasks, tblUserTasks, {tblUserFields, }tblUserDataSet]);
      FMatrix.SortDirect;
  {$ifdef DeDEBUG}
      FMatrix.Display;
  {$endif}
      // загрузка данных
      FDataStream := nil;
      try
        BackupCursor := Screen.Cursor;
        try
          Screen.Cursor := crHourglass;
          try
            FDataStream := TFileStream.Create(FPath+DataFile, fmOpenRead);
            //..........................................................................................................
            ConfigStrings := TStringList.Create;
            ItemData := TStringList.Create;
            ItemData.QuoteChar := DeQuoteChar;
            ItemData.Delimiter := DeDelimiter;
            try
              SwitchSeparators(ssSpecial);
              ConfigStrings.LoadFromStream(FDataStream);
              I := 0;
              FieldsIndex := nil;
              while I < ConfigStrings.Count do
                begin
                  TableMeta := MetaData.GetTableByName(Trim(ConfigStrings[I]), MetaData.MetadataDB);
                  inc(I);
                  if (I < ConfigStrings.Count) and Assigned(TableMeta) then
                    begin
                      ItemData.DelimitedText := ConfigStrings[I];
                      SetLength(FieldsIndex, ItemData.Count);
                      for J := 0 to ItemData.Count-1 do
                        FieldsIndex[J] := TableMeta.Fields.IndexByName(Trim(ItemData[J]));
                      inc(I);
                      DCache := FCacheList.ItemsByID(TableMeta.ID, True);
                      while (I < ConfigStrings.Count) and (Trim(ConfigStrings[I]) <> EmptyStr) do
                        begin
                          ItemData.DelimitedText := ConfigStrings[I];
                          DItem := DCache.AddNewItem;
                          for J := 0 to ItemData.Count-1 do
                            if FieldsIndex[J] >= 0 then
                              try
                                FType:= DCache.Fields[FieldsIndex[J]].DataType;
                                V:= Null;

                                if (Trim(ItemData[J]) = EmptyStr) and not(FType in StringTypes) then V:= Null else
                                if FType in IntegerTypes then V:= StrToInt64(ItemData[J]) else
                                if FType in DateTimeTypes then V:= StrToDateTime(ItemData[J]) else
                                if FType in FloatTypes then V:= StrToFloat(ItemData[J]) else
                                if FType in StringTypes then V:= ItemData[J] else
                                if FType in LogicalTypes then begin
                                                                if (Trim(ItemData[J]) = TrueLexeme) or (Trim(ItemData[J]) = '1') then V:= True else
                                                                if (Trim(ItemData[J]) = FalseLexeme) or (Trim(ItemData[J]) = '0') then V:= False;
                                                              end else
                                if FType in [ftBlob] then begin
                                                            Stream := TFileStream.Create( FPath+ItemData[J], fmOpenRead or fmShareDenyWrite);
                                                            try
                                                              Stream.Position := 0;
                                                              SetLength(BlobData, Stream.Size);
                                                              Stream.ReadBuffer(BlobData[1], Stream.Size);
                                                              V:= BlobData;
                                                            finally
                                                              Stream.Free;
                                                            end;
                                                          end;

                                // восстанавливаем связи по новым идентификаторам
                                if 0 < DCache.Fields[FieldsIndex[J]].Link then
                                  begin

                                  end;

                                if FType in StringTypes then DItem.FieldNativeValue[FieldsIndex[J]]:= V
                                                        else DItem.FieldValue[FieldsIndex[J]]:= V;
                              except
                               // sleep(9);
                              end;
                          inc(I);
                        end;
                      inc(I);
                    end;
                end;
            finally
              SwitchSeparators(ssOriginal);
              ConfigStrings.Free;
              ItemData.Free;
            end;
            //..........................................................................................................

          finally
            FDataStream.Free;
          end;
        finally
          Screen.Cursor := BackupCursor;
        end;

        if Assigned(FIndicator) then
          begin
            FIndicator.Min := 0;
            FIndicator.Max := TotalRecordsCount;
            FIndicator.Position := 0;
            FIndicator.Show;
            UpdateWindow(FIndicator.Handle);
          end;
        StoreDatabases;
        for II := 0 to FMatrix.Size-1 do
          if SameText(FMatrix.Objects[II].Table, tblDictonary)
            then MergeDictionary(II)
            else StoreTableData(II);
        result := true;
      finally
      end;
    end;
end;

function TConfigManager.ExtractConfig(const aPath : string; const aID : integer) : boolean;
var I, J          : integer;
    CurrentCache, TempCache  : TDataCache;
    ConfigStrings, ItemStrings : TStringList;
    FFilter: TFilterItem;
begin
  result := false;
  FID:= aID;
  DataPath := aPath;


(*
  if Length(DataPath) > 0 then
  begin
    // готовим матрицу смежности
    BuildMatrix([tblUsers, tblNotes, tblInterrelations, tblRights, tblMembership, tblTasks, tblUserTasks, {tblUserFields, }tblUserDataSet ]);

    with FMatrix do
    begin
{$ifdef DeDEBUG}
      Display;
{$endif}
      SortDirect;
//    ClearDouble;
      {
      // подавляем ненужные связи
      RemoveLink(MetaData.MetaTables[idxElement].Fields.FindByName(fldElemField));
      RemoveLink(MetaData.MetaTables[idxElement].Fields.FindByName(fldElemLinkField));
      RemoveLink(MetaData.MetaTables[idxElement].Fields.FindByName(fldElemLink));
      RemoveLink(MetaData.MetaTables[idxConstraints].Fields.FindByName(fldCSErrorField));
      RemoveLink(MetaData.MetaTables[idxElement].Fields.FindByName(fldElemOwner));
      RemoveLink(MetaData.MetaTables[idxDataset].Fields.FindByName(fldDataSetParent));
      RemoveLink(MetaData.MetaTables[idxMenu].Fields.FindByName(fldMenuGroupField));
      //DONE: добавлено всвязи с изменение связей метаструктуры
      RemoveLink(MetaData.MetaTables[idxFields].Fields.FindByName(fldFieldsLink));
      RemoveLink(MetaData.MetaTables[idxActions].Fields.FindByName(fldActionDataSet));
      // расставляем наборы данные в требуемом порядке
      SortDirect;
      {}

{$ifdef DeDEBUG}
      Display;
{$endif}
    end;
    // выгрузка данных
    FDataStream := nil;
    try
      if Assigned(FIndicator) then
      begin
        FIndicator.Min := 0;
        FIndicator.Max := 2*FMatrix.Size{-1};
        FIndicator.Position := 0;
      end;
      //................................................................................................................

      for I := 0 to FMatrix.Size-1 do
      begin
        TempCache := TDataCache.Create(FMatrix.Objects[I]);

        // SOLUTIONS
        if TempCache.TableMeta = MetaData.MetaTables[idxSolutions] then
          begin
            FFilter := TFilterItem.Create;
            FFilter.AddCondition(MetaData.MetaTables[idxSolutions].Fields.FindByName(fldConfID), opEQ, FID);
            TempCache.Filters.AddFilter(FFilter);
          end;

        // DATABASES
        if TempCache.TableMeta = MetaData.MetaTables[idxBase] then
          begin
            FFilter := TFilterItem.Create;
            FFilter.AddCondition(MetaData.MetaTables[idxBase].Fields.FindByName(fldBaseId), opNE, 0);
            TempCache.Filters.AddFilter(FFilter);
          end;

        // Остальные c прямой ссылкой на решение
        for J := 0 to Pred(TempCache.TableMeta.Fields.Count) do
          if TempCache.TableMeta.Fields[j].IsStored then
            if TempCache.TableMeta.Fields[j].Link = MetaData.MetaTables[idxSolutions].ID then
              begin
                FFilter := TFilterItem.Create;
                FFilter.AddCondition(TempCache.TableMeta.Fields[j], opEQ, FID);
                TempCache.Filters.AddFilter(FFilter);
              end;

        TempCache.PrepareData(True, fsMax);
        TempCache.FillAll;

        // готовим данные для сохранения
        CurrentCache := FCacheList.ItemsByID(FMatrix.Objects[I].ID, True);
        if (FMatrix.ChildLinksCount[I] = 0) and (FMatrix.ParentLinksCount[I] = 0) then
          begin
            CurrentCache.CopyFrom(TempCache);
          end
        else
          begin
            if FMatrix.ChildLinksCount[I] > 0 then ProcessChildLinks(I, TempCache, CurrentCache);
            if FMatrix.Link[I, I] then ProcessSelfLinks(I, TempCache, CurrentCache);
            if FMatrix.ParentLinksCount[I] > 0 then ProcessParentLinks(I, TempCache, CurrentCache);
          end;
        TempCache.Free;

        if Assigned(FIndicator) then
          with FIndicator do
            Position := Position + 1;
      end;
      //................................................................................................................
      try
        FDataStream := TFileStream.Create(FPath+DefaultDataFile, fmCreate);

        ConfigStrings := TStringList.Create;
        ItemStrings := TStringList.Create;
        ItemStrings.QuoteChar := DeQuoteChar;
        ItemStrings.Delimiter := DeDelimiter;
        try
          SwitchSeparators(ssSpecial);
          for I := 0 to FMatrix.Size-1 do
            begin
              CurrentCache := FCacheList.ItemsByID(FMatrix.Objects[I].ID, True);
             // CurrentCache.PrepareData(True, fsMax);
              if CurrentCache.Count > 0 then
                begin
                  // выгружаем заголовочную часть
                  ConfigStrings.Add(FMatrix.Objects[I].Table);
                  ItemStrings.Clear;
                  for J := 0 to FMatrix.Objects[I].Fields.Count-1 do
                    if not FMatrix.Objects[I].Fields[J].IsLookup then
                      ItemStrings.Add(FMatrix.Objects[I].Fields[J].Original);
                  ConfigStrings.Add(ItemStrings.DelimitedText);

                  // сохраняем данные в списке строк;  сохраняем BLOB-поля в отдельных файлах
                  for J := 0 to CurrentCache.Count-1 do
                    begin
                      ItemAsStrings(CurrentCache[J], ItemStrings);
                      ConfigStrings.Add(ItemStrings.DelimitedText);
                    end;
                  ConfigStrings.Add(EmptyStr);
                  if Assigned(FIndicator) then
                      FIndicator.Position := FIndicator.Position + 1;
                end;
            end;
          SwitchSeparators(ssOriginal);
          // сохраняем данные таблицы в файле
          ConfigStrings.SaveToStream(FDataStream);

          if Assigned(FIndicator) then
            FIndicator.Position := FIndicator.Position + 1;
        finally
          ConfigStrings.Free;
          ItemStrings.Free;
        end;

      finally
        FDataStream.Free;
      end;
      result := true;
    finally

    end;
  end;
  *)
end;

function TConfigManager.DeleteConfig(const aID : integer) : boolean;
begin
  FID:= aID;

  // готовим матрицу смежности
  BuildMatrix([tblUsers, tblDictonary, tblNotes, tblInterrelations, tblRights, tblMembership, tblTasks, tblUserTasks, {tblUserFields, }tblUserDataSet]);
  with FMatrix do
  begin
    // подавляем ненужные связи
    RemoveLink(MetaData.MetaTables[idxElement].Fields.FindByName(fldElemField));
    RemoveLink(MetaData.MetaTables[idxElement].Fields.FindByName(fldElemLinkField));
    RemoveLink(MetaData.MetaTables[idxElement].Fields.FindByName(fldElemLink));
    if mpDatabaseNewConstraints in MetaData.MetadataPresents then
      RemoveLink(MetaData.MetaTables[idxConstraints].Fields.FindByName(fldConstraintsField))
    else
      RemoveLink(MetaData.MetaTables[idxConstraints].Fields.FindByName(fldCSErrorField));
    RemoveLink(MetaData.MetaTables[idxElement].Fields.FindByName(fldElemOwner));
    RemoveLink(MetaData.MetaTables[idxDataset].Fields.FindByName(fldDataSetParent));
    RemoveLink(MetaData.MetaTables[idxMenu].Fields.FindByName(fldMenuGroupField));
    //DONE: добавлено всвязи с изменение связей метаструктуры
    RemoveLink(MetaData.MetaTables[idxFields].Fields.FindByName(fldFieldsLink));

    // расставляем наборы данные в требуемом порядке
    SortDirect;
    // переставляем набор tblBase после tblDataSet
    Move(Objects.IndexOf(MetaData.MetaTables[idxBase]),
      Objects.IndexOf(MetaData.MetaTables[idxDataset])+1);
{$ifdef DeDEBUG}
    Display;
{$endif}
  end;

  try
    if Assigned(FIndicator) then
      begin
        FIndicator.Min := 0;
        FIndicator.Max := 2*FMatrix.Size;
        FIndicator.Position := 0;
        FIndicator.Show;
        UpdateWindow(FIndicator.Handle);
      end;

    // перестроение матрицы для удаления данных
    with FMatrix do
      begin
        // подавляем ненужные связи - связи уже подавлены ??? - ПРОВЕРИТЬ
        AddLink(MetaData.MetaTables[idxElement].Fields.FindByName(fldElemField));
        if mpDatabaseNewConstraints in MetaData.MetadataPresents then
          AddLink(MetaData.MetaTables[idxConstraints].Fields.FindByName(fldConstraintsField))
        else
          AddLink(MetaData.MetaTables[idxConstraints].Fields.FindByName(fldCSErrorField));
        AddLink(MetaData.MetaTables[idxElement].Fields.FindByName(fldElemOwner));
        // расставляем наборы данные в требуемом порядке
        SortReverse;
  {$ifdef DeDEBUG}
        Display;
  {$endif}
      end;
    // удаление данных
    DeleteConfigData;
    result := true;
  except
    result := False;
  end;
end;

{ ----------------------------------------------------------------------------

  Конфигурация имеет следующую структуру:

  1. Файл с описанием конфигурации

      [Description]
      Name=<наименование конфигурации>
      Developer=<наименование разработчика>
      Data=<имя файла с данными>
      Version=<версия конфигурации>


  2. Файл с данными

     ...
     <пустая строка>
     ИМЯ_ТАБЛИЦЫ_В_БАЗЕ_ДАННЫХ(КОД_НАБОРА_ДАННЫХ)
     ПОЛЕ_1,ПОЛЕ_2,...,ПОЛЕ_№
     ЗАПИСЬ_1
     ЗАПИСЬ_2
     ...

     ЗАПИСЬ - это перечень значений полей, отделенных запятыми и взятых в кавычки


  3. Файлы с содержимым BLOB-полей

  ---------------------------------------------------------------------------- }

{$IFDEF DEBUG}
initialization
  DebugLog('SolutionOperations unit initialization ...');

finalization
  DebugLog('SolutionOperations unit finalization ...');
{$ENDIF}

end.

