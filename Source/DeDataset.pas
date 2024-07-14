unit DeDataset;

interface

uses SysUtils, Classes, Contnrs, DB, DeTypes, XMLTable, UFileDB, FDBConsts, DeDB, ADOInt, Xml.XMLIntf,

     FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
     FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
     FireDAC.Comp.Client, FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
     FireDAC.DApt, FireDAC.Comp.DataSet,

     QueryDescriptor, DeParser;

type

  /// <summary>Абстрактный класс-предок для SQL-Dataset'ов Profite'а</summary>
  TDeSQLDataset = class(TDeDataset)
  private
    FIsNeedQueryCount : Boolean;
    FRecordCount      : Integer;
    procedure CheckDatasetObject; inline;
    procedure SetSQLParamValue(aParam : TVariableItem);
    procedure SetSelectParams(autoPrepare: Boolean = True);
    procedure SetInsertParams(aRecord : TObject);
    procedure SetUpdateParams(aRecord : TObject);
    procedure SetDeleteParams(aRecord : TObject);
    procedure LogBeforeOpen(DataSet: TDataSet);
  protected
    procedure CreateDescriptor(const aQueryType: TDeQueryType); override;
    function GetActive : boolean;  override;
    function Initialize: Boolean; override;
    function GetRecordCount : integer;  override;
    function GetValue(const Index: Integer): Variant; override;
    procedure SetValue(const Index: Integer; const Value: Variant); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);  override;

    function GetRecNo: Integer; override;
    procedure SetRecNo(const Value: Integer); override;
    procedure DoOpen;  override;
    procedure DoClose;  override;
    /// <summary>получение количества параметров в запросе</summary>
    /// <returns>Возврашает количество параметров</returns>
    function GetParamCount: Integer;  virtual;  abstract;
  public
    constructor Create(aOwner : TComponent);  override;
    destructor Destroy;  override;
    function Internal_Count: Integer; override;
    function Internal_Goto(const aIndex: integer): Boolean; override;
    function Internal_GetIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean; override;
    property ParamCount : integer read GetParamCount;
    // по имени поля возвращает его номер
    function IndexByName(const aName : string) : Integer; override;
    //****************************************************************************************
    /// <summary>проверяет существование параметра в запросе</summary>
    /// <param name="PName">имя параметра в запросе</param>
    /// <returns>Возвращает True в случае существования параметра в запросе</returns>
    function ParamExists(const PName: string): Boolean; overload; virtual;
    function ParamNames(const PNum: Integer): string; overload; virtual;
    function ParamValues(const PNum: Integer): Variant; overload; virtual;
    /// <summary>устанавливает SQL для запроса</summary>
    /// <param name="SQLText">текст SQL запроса</param>
    procedure SetSQL(const SQLText: string); virtual; abstract;
    /// <summary>процедура удаления всех параметров</summary>
    procedure ClearParams; virtual;  abstract;
    /// <summary>процедура создания параметра</summary>
    /// <param name="PName">имя параметра в запросе</param>
    procedure CreateParam(const PName: string); virtual; abstract;
    procedure SetParamValue(const PName: string; const PValue: Variant; const ParamType: TFieldType = ftUnknown); overload; virtual; abstract;
    //****************************************************************************************
    //ExecuteQuery - открывает, закрывает DataSet, если передан Query
    function ExecuteQuery: Boolean;  override;
    function InsertRecord(aRecord : TObject) : boolean;  override;
    function UpdateRecord(aRecord : TObject) : boolean;  override;
    function DeleteRecord(aRecord : TObject) : boolean;  override;
    function GetMaxKeyValue(const MetaID: Integer = -1): Variant; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; const Options: TLocateOptions): Boolean; override;
    procedure CreateFields(aFields : TObjectList; const Stage: TFieldStage); override;
    function IndexTFieldByName(const FieldName: string; var aField: TField): Integer; override;
  end;

  // набор данных для BDE
  {
  TDeBDEDataset = class(TDeSQLDataset)
  protected
    function GetParamCount: Integer;  override;
    function FetchAll : boolean;  override;
  public
    constructor Create(aOwner : TComponent);  override;
    //****************************************************************************************
    //SetSql - устанавливает SQL для Query;
    procedure SetSql(SQLText : String); override;
    //ClearParams - удаление параметровж
    procedure ClearParams;override;
    //CreateParam - создание параметра
    procedure CreateParam(PName: String);override;
    //ParamExists - проверяет существование параметраж
    function ParamExists(const PName: String):boolean;overload;  override;
    function ParamNames(PNum: Integer):String;overload;  override;
    function ParamValues(PNum: Integer):Variant; overload;  override;
    procedure SetParamValue(PName: string; PValue : Variant;
      ParamType : TFieldType = ftUnknown); overload;  override;
    //****************************************************************************************
    procedure Prepare;  override;
    procedure BlockReadMode; override;
    procedure UnBlockReadMode; override;
    function ExecuteQuery: Boolean;  override;
  end;
  {}
  /// <summary>набор данных для Interbase</summary>
  TDeIBDataset = class(TDeSQLDataset)
  protected
    /// <summary>получение количества параметров в запросе</summary>
    /// <returns>Возврашает количество параметров</returns>
    function GetParamCount: Integer;  override;
    function FetchAll : boolean;  override;
  public
    constructor Create(aOwner : TComponent);  override;
    destructor Destroy;  override;
   //****************************************************************************************
    /// <summary>устанавливает SQL для запроса</summary>
    /// <param name="SQLText">текст SQL запроса</param>
    procedure SetSQL(const SQLText: string); override;
    /// <summary>процедура удаления всех параметров</summary>
    procedure ClearParams;override;
    /// <summary>процедура создания параметра</summary>
    /// <param name="PName">имя параметра в запросе</param>
    procedure CreateParam(const PName: string); override;
    /// <summary>проверяет существование параметра в запросе</summary>
    /// <param name="PName">имя параметра в запросе</param>
    /// <returns>Возвращает True в случае существования параметра в запросе</returns>
    function ParamExists(const PName: string): Boolean; overload; override;
    function ParamNames(const PNum: Integer): string; overload; override;
    function ParamValues(const PNum: Integer): Variant; overload; override;
    procedure SetParamValue(const PName: string; const PValue: Variant; const ParamType: TFieldType = ftUnknown); overload; override;
    //****************************************************************************************
    procedure Prepare;  override;
    function ExecuteQuery: Boolean;  override;
  end;

  TDeFireDACDataset = class(TDeSQLDataset)
  protected
    /// <summary>получение количества параметров в запросе</summary>
    /// <returns>Возврашает количество параметров</returns>
    function GetParamCount: Integer;  override;
    function FetchAll : boolean;  override;
    function GetRecordCount : integer;  override;
  protected
  public
    constructor Create(aOwner : TComponent);  override;
    destructor Destroy;  override;

    function Internal_Count: Integer; override;
   //****************************************************************************************
    /// <summary>устанавливает SQL для запроса</summary>
    /// <param name="SQLText">текст SQL запроса</param>
    procedure SetSQL(const SQLText: string); override;
    /// <summary>процедура удаления всех параметров</summary>
    procedure ClearParams;override;
    /// <summary>процедура создания параметра</summary>
    /// <param name="PName">имя параметра в запросе</param>
    procedure CreateParam(const PName: string); override;
    /// <summary>проверяет существование параметра в запросе</summary>
    /// <param name="PName">имя параметра в запросе</param>
    /// <returns>Возвращает True в случае существования параметра в запросе</returns>
    function ParamExists(const PName: string): Boolean; overload; override;
    function ParamNames(const PNum: Integer): string; overload; override;
    function ParamValues(const PNum: Integer): Variant; overload; override;
    procedure SetParamValue(const PName: string; const PValue: Variant; const ParamType: TFieldType = ftUnknown); overload; override;
    //****************************************************************************************
    procedure Prepare;  override;
    function ExecuteQuery: Boolean;  override;
  end;

  TDeFBDataset = class(TDeFireDACDataset)
  protected
  public
  end;

  TDePGDataset = class(TDeFireDACDataset)
  protected
  public
  end;

  /// <summary>набор данных для ADO-таблиц</summary>
  TDeADODataset = class(TDeSQLDataset)
  protected
    /// <summary>получение количества параметров в запросе</summary>
    /// <returns>Возврашает количество параметров</returns>
    function GetParamCount: Integer; override;
    function FetchAll: Boolean; override;
    function NewDescriptor(const aQueryType: TDeQueryType): TCustomQueryDescr; override;
  public
    constructor Create(aOwner : TComponent);  override;
    destructor Destroy;  override;

    function Internal_Goto(const aIndex: integer): Boolean; override;

    //****************************************************************************************
    /// <summary>устанавливает SQL для запроса</summary>
    /// <param name="SQLText">текст SQL запроса</param>
    procedure SetSQL(const SQLText: string); override;

    /// <summary>процедура удаления всех параметров</summary>
    procedure ClearParams;override;
    /// <summary>процедура создания параметра</summary>
    /// <param name="PName">имя параметра в запросе</param>
    procedure CreateParam(const PName: string); override;
    /// <summary>проверяет существование параметра в запросе</summary>
    /// <param name="PName">имя параметра в запросе</param>
    /// <returns>Возвращает True в случае существования параметра в запросе</returns>
    function ParamExists(const PName: string): Boolean; overload; override;
    /// <summary>получение имени параметра в запросе по индексу</summary>
    /// <param name="PNum">индекс параметра в запросе</param>
    /// <returns>Возвращает имя параметра в запросе</returns>
    function ParamNames(const PNum: Integer): string; overload; override;
    /// <summary>получение значение параметра в запросе по индексу</summary>
    /// <param name="PNum">индекс параметра в запросе</param>
    /// <returns>Возвращает значение параметра в запросе</returns>
    function ParamValues(const PNum: Integer): Variant; overload; override;
    procedure SetParamValue(const PName: string; const PValue: Variant; const ParamType: TFieldType = ftUnknown); overload; override;
    //****************************************************************************************
    /// <summary>подготавливает запрос к выполнению</summary>
    procedure Prepare; override;
    function ExecuteQuery: Boolean; override;
    procedure SaveToADOXML(const FileName: string);
  end;
  {
  TDeOracleDataset = class(TDeSQLDataset)
  protected
    function GetParamCount: Integer;  override;
    function FetchAll : boolean;  override;
  public
    constructor Create(aOwner : TComponent);  override;
    destructor Destroy;  override;
    //****************************************************************************************
    //SetSql - устанавливает SQL для Query;
    procedure SetSql(SQLText : String); override;
    //ClearParams - удаление параметровж
    procedure ClearParams;override;
    //CreateParam - создание параметра
    procedure CreateParam(PName: String);override;
    //ParamExists - проверяет существование параметраж
    function ParamExists(const PName: String):boolean;overload;  override;
    function ParamNames(PNum: Integer):String;overload;  override;
    function ParamValues(PNum: Integer):Variant; overload;  override;
    procedure SetParamValue(PName: string; PValue : Variant;
      ParamType : TFieldType = ftUnknown); overload;  override;
    //****************************************************************************************
    procedure Prepare;  override;
    function ExecuteQuery: Boolean;  override;
  end;
  {}

//++++++++++++++FileDB DE DataSet+++++++++++++++++++++++++++++++++++++++++++++++++++++++

  TFileDBField = record
                fName : string;
                fType : TFieldType;
                fSize : integer;
              end;
  TFileDBFields = array of TFileDBField;

  TFileDB_DEDS=class(TDeDataSet)
  private
    FActive: boolean;
    FFileDB: TFileDB;
    FFields: TFileDBFields;
    procedure ReadFields;
    procedure ReadData;
    function CheckConditions():Boolean;
  protected
    function Initialize : boolean;  override;
    function GetRecordCount : integer; override;
    function GetActive : boolean; override;
    // возвращает значение поля с указанным номером
    function GetValue(const Index: Integer): Variant; override;
    // устанавливает значение поля с указанным номером
    procedure SetValue(const Index: Integer; const Value: Variant); override;
    function GetRecNo : integer; override;
    procedure DoOpen; override;
    procedure DoClose; override;
    // закачивает записи на сторону клиента
    function FetchAll : boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FileDB:TFileDB read FFileDB write FFileDB;
    function SetValueToField(const FieldName:string; const Value: Variant; var fl: TFile): Boolean;

    //+++++++++++++DE DATA SET++++++++++++++++++++++++++++++++++++++
    // возвращает максимальное значение первичного ключа
    function GetMaxKeyValue(const MetaID: Integer = -1): Variant; override;
    // по имени поля возвращает его номер
    function IndexByName(const Name: string): Integer; override;
    property RecordCount : integer read GetRecordCount;     // количество записей набора
    // подготавливает запрос для выполнения
    procedure Prepare; override;
    // вставляет запись в набор данных
    function InsertRecord(aRecord : TObject) : boolean; override;
    // обновляет запись в наборе данных
    function UpdateRecord(aRecord : TObject) : boolean; override;
    // удаляет запись из набора данных
    function DeleteRecord(aRecord : TObject) : boolean; override;
    // навигационные методы
    function Locate(const KeyFields: string; const KeyValues: Variant; const Options: TLocateOptions): Boolean; override;
    // получение метаописания полей набора данных
    procedure RetrieveFields(aFields: TObjectList; aTableMeta: TObject = nil);  override;
    // наполнение набора данных полями на основании метаописания полей
    procedure CreateFields(aFields : TObjectList; const Stage: TFieldStage);  override;
  end;
  {
  // набор данных для PostgreSQL
  TDePGDataset = class(TDeSQLDataset)
  private
    FQueryType:TDeQueryType;
    FUseQuery:boolean;
  protected
    function GetParamCount: Integer;  override;
    function FetchAll : boolean;  override;
  public
    constructor Create(aOwner : TComponent);override;
    procedure CreateData(aQueryType: TDeQueryType);
    destructor Destroy;  override;
   //****************************************************************************************
    //SetSql - устанавливает SQL для Query;
    procedure SetSql(SQLText : String); override;
    //ClearParams - удаление параметровж
    procedure ClearParams;override;
    //CreateParam - создание параметра
    procedure CreateParam(PName: String);override;
    //ParamExists - проверяет существование параметраж
    function ParamExists(const PName: String):boolean;overload;  override;
    function ParamNames(PNum: Integer):String;overload;  override;
    function ParamValues(PNum: Integer):Variant; overload;  override;
    // SetParamValue  -  устанавливает параметр запроса
    procedure SetParamValue(PName: string; PValue : Variant;
      ParamType : TFieldType = ftUnknown); overload;  override;
    //****************************************************************************************
    procedure Prepare;  override;
    function ExecuteQuery: Boolean;  override;
  end;
   {}

 TDeADOXMLDataset = class(TDeIndexedDataset)
  private
    FFileName: string;
    FModified: Boolean;
    //procedure CheckConditions(aNode: Pointer; var aResult : Boolean);
    procedure CheckXMLObject;
    //procedure CommitTable;
    //procedure FilterRecord(DataSet: TDataSet; var Accept: Boolean);
  protected
    function GetActive: Boolean; override;
    procedure SetRecNo(const Value: Integer); override;
    function GetValue(const Index: Integer): Variant; override;
    procedure SetValue(const Index: Integer; const Value: Variant); override;
    function Initialize: Boolean; override;
    procedure AfterOpen; override;
    procedure DoOpen; override;
    procedure DoClose; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Internal_Count: Integer; override;
    function Internal_Goto(const aIndex: integer): Boolean; override;
    function Internal_GetIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean; override;
    function InsertRecord(ARecord: TObject): Boolean; override;
    function UpdateRecord(ARecord: TObject): Boolean; override;
    function DeleteRecord(ARecord: TObject): Boolean; override;
    function GetMaxKeyValue(const MetaID: Integer = -1): Variant; override;
    procedure Prepare; override;
    function FetchAll: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; const Options: TLocateOptions): Boolean; override;
    procedure RetrieveFields(Fields: TObjectList; TableMeta: TObject = nil); override;
    procedure CreateFields(Fields: TObjectList; const Stage: TFieldStage); override;
    // по имени поля возвращает его номер
    function IndexByName(const Name: string): Integer; override;
    {$IFDEF DEBUG}
//    procedure DebugDumpLog(const FileName: string); override;
    {$ENDIF}
  end;

  TDBCOXMLDataset = class(TDeIndexedDataset)
  private
    FInternalNode: IXMLNode;
    FCurrentNode: IXMLNode;
    FModified: Boolean;
    //procedure CommitTable;
    //procedure FilterRecord(DataSet: TDataSet; var Accept: Boolean);
  protected
    procedure SetRecNo(const Value: Integer); override;
      //procedure Notification(AComponent: TComponent;
      //  Operation: TOperation);  override;
    function GetActive: Boolean; override;
    function GetValue(const Index: Integer): Variant; override;
    procedure SetValue(const Index: Integer; const Value: Variant); override;
    function Initialize: Boolean; override;
    procedure DoOpen; override;
    procedure DoClose; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Internal_Count: Integer; override;
    function Internal_Goto(const aIndex: integer): Boolean; override;
    function Internal_GetIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean; override;
    function InsertRecord(ARecord: TObject): Boolean; override;
    function UpdateRecord(ARecord: TObject): Boolean; override;
    function DeleteRecord(ARecord: TObject): Boolean; override;
    function GetMaxKeyValue(const MetaID: Integer = -1): Variant; override;
    procedure Prepare; override;
    function FetchAll: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; const Options: TLocateOptions): Boolean; override;
    procedure CreateFields(aFields: TObjectList; const Stage: TFieldStage); override;
    // по имени поля возвращает его номер
    function IndexByName(const aName: string): Integer; override;
  end;

implementation

uses Windows, Variants, StrUtils, Math, Registry, DateUtils, DeLog,
     // Ora, OraClasses,
     IBQuery, IBDatabase, IdMultipartFormData, System.Win.ComObj,
     // DBTables,
     ADODB, DataCacheUnit,
     Dictionary, Funcs, ConnectOperationsUnit, DeMeta,
     AbstractTable, DeMetaData, {DSMeta, }DeCalculator, {DataUnit, }Security;

var
  FLastFileID:integer=-1;
{ ---------------------------------------------------------------------------- }

{ TDeSQLDataset }

constructor TDeSQLDataset.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FIsNeedQueryCount := True;
  FRecordCount      := -1;
//  FRecNo := 0;
end;

destructor TDeSQLDataset.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSourceSet);
end;

procedure TDeSQLDataset.LogBeforeOpen(DataSet: TDataSet);
begin
  WriteQueryLog(DataSet);
end;

procedure TDeSQLDataset.CheckDatasetObject;
begin
  if not Assigned(FSourceSet) then
    raise EDeDatasetError.Create('SourceSet object not found');
end;

function TDeSQLDataset.Internal_Count: Integer;
var  TmpData : TDeDataSet;
begin
  if FRecordCount < 0 then
    begin
      CheckDatasetObject;
      if FIsNeedQueryCount then
        begin
          TmpData := Database.CreateQuery(qtRow, Self);
          try
            TmpData.Descr.Assign(Descr);
            TmpData.Descr.SortFields.Clear;
            TmpData.Descr.ClearFields;
            TmpData.Descr.AddField(opCount);
            try
              TmpData.Open;
              FRecordCount := VarToInt(TmpData.Value[0]);
              TmpData.Close;
            except
              on E: Exception do
                begin
                  Funcs.WriteLog('TDeSQLDataset.GetRecordCount skip error: ' + E.Message);
                  FRecordCount := 0;
                end;
            end;
          finally
            TmpData.Free;
          end;
        end
      else
        begin
          FRecordCount := TDataSet(FSourceSet).RecordCount;
        end;
    end;
  Result := FRecordCount;
end;

function TDeSQLDataset.Internal_Goto(const aIndex: integer): Boolean;
begin
  TDataSet(FSourceSet).RecNo := Succ(aIndex);
end;

function TDeSQLDataset.Internal_GetIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
begin

end;

procedure TDeSQLDataset.SetSQLParamValue(aParam : TVariableItem);
begin
//  if ParamExists(aParam.Name)then
    SetParamValue(aParam.Name, aParam.Value, aParam.DataType);
end;

procedure TDeSQLDataset.SetSelectParams(autoPrepare: Boolean = True);
var i : integer;
    V : TVariableItem;
begin
  for i := 0 to Pred(Descr.Filter.Count) do
    if Descr.Filter.Items[i].ItemType = piParameter then
      begin
        if autoPrepare and not (qsInitialized in State) then Prepare;
        V:= TVariableItem(NativeInt(Descr.Filter.Items[i].Data));
        SetParamValue(V.Name, V.Value, V.DataType);
      end;

  for I := 0 to Pred(Descr.Params.Count) do
    begin
      if autoPrepare and not (qsInitialized in State) then Prepare;
      V := Descr.Params[I];
      SetParamValue( V.Name, V.Value, V.DataType);
    end;
end;

procedure TDeSQLDataset.SetInsertParams(aRecord : TObject);
var n,i  : Integer;
begin
  if aRecord is TCacheItem then
    with TCacheItem(aRecord) do
    begin
      // устанавливаем значения параметров
      for i:= Pred(Owner.Fields.Count) downto 0 do
        if not (Owner.Fields[i].IsLookup or (not Owner.Fields[i].IsStored)) then
          if ParamExists(Owner.Fields[i].Original) then
              SetParamValue(Owner.Fields[i].Original, FieldValue[i], Owner.Fields[i].DataType);
    end else

  if aRecord is TCacheItemList then
    for n:=0 to Pred(TCacheItemList(aRecord).Count) do
    with TCacheItemList(aRecord).Items[n] do
    begin
      // устанавливаем значения параметров
      for i:= Pred(Owner.Fields.Count) downto 0 do
        if not (Owner.Fields[i].IsLookup or (not Owner.Fields[i].IsStored)) then
          if ParamExists(Owner.Fields[i].Original + IntToStr(n)) then
              SetParamValue(Owner.Fields[i].Original + IntToStr(n), FieldValue[i], Owner.Fields[i].DataType);
    end;
end;

procedure TDeSQLDataset.SetUpdateParams(aRecord : TObject);
var I, FieldIndex : integer;
    PName, FName  : string;
begin
  // устанавливаем значения параметров
  if aRecord is TCacheItem then
    with TCacheItem(aRecord) do
      for I := 0 to ParamCount-1 do
      begin
        PName := ParamNames(i);
        if Copy(PName,1,10) = 'OLD_VALUE_' then
          FName := Copy(PName, 11, MaxInt)
        else
          FName := PName;
        FieldIndex := Owner.{TableMeta.}Fields.IndexByName(FName);
        if FieldIndex >= 0 then
        begin
          SetParamValue(
            PName,FieldValue[FieldIndex],
            Owner.Fields[FieldIndex].DataType);
        end;
      end;
end;

procedure TDeSQLDataset.SetDeleteParams(aRecord : TObject);
var I, FieldIndex : integer;
    PName, FName  : string;
begin
  if aRecord is TCacheItem then
    with TCacheItem(aRecord) do
      // устанавливаем значения параметров
      for I := 0 to ParamCount-1 do
      begin
        PName := ParamNames(i);
        if Copy(PName,1,10) = 'OLD_VALUE_' then FName := Copy(PName, 11, MaxInt)
        else FName := PName;
        FieldIndex := Owner.{TableMeta.}Fields.IndexByName(FName);
        if FieldIndex >= 0 then
        begin
          SetParamValue(
            PName,
            FieldValue[FieldIndex],
            Owner.Fields[FieldIndex].DataType);
        end;
      end;
end;

procedure TDeSQLDataset.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (aComponent = FSourceSet) then
    FSourceSet := nil;
  inherited Notification(aComponent, Operation);
end;

function TDeSQLDataset.GetRecNo : integer;
begin
  CheckDatasetObject;
//result := Pred(TDataSet(FSourceSet).RecNo);
  Result:= FCurrentIndex;
end;

procedure TDeSQLDataset.SetRecNo(const Value: Integer);
begin
  FCurrentIndex:= Value;
  CheckDatasetObject;
  Internal_Goto(FCurrentIndex);
end;

function TDeSQLDataset.GetActive : boolean;
begin
  result := Assigned(FSourceSet) and TDataSet(FSourceSet).Active;
end;

function TDeSQLDataset.GetRecordCount: Integer;
begin
  Result:= Internal_Count;
end;

function TDeSQLDataset.GetValue(const Index: Integer): Variant;
var N: Integer;
begin
  CheckDatasetObject;
  N:= Descr.Fields[Index].SourceIndex;
  if N=-1 then Exit(unassigned);
  
  if TDataSet(FSourceSet).Fields[N].DataType in DateTimeTypes
    then  if TDataSet(FSourceSet).Fields[N].IsNull
            then Result:= Null
            else Result:= RecodeMilliSecond(TDataSet(FSourceSet).Fields[N].Value, 0)
    else         Result:= TDataSet(FSourceSet).Fields[N].Value;
end;

procedure TDeSQLDataset.SetValue(const Index: Integer; const Value: Variant);
begin
  CheckDatasetObject;
  TDataSet(FSourceSet).Fields[Index].Value := Value;
end;

function TDeSQLDataset.IndexByName(const aName : string) : integer;
var Field : TField;
begin
  result := -1;
  CheckDatasetObject;
  Field := TDataSet(FSourceSet).FindField(aName);
  if Assigned(Field) then result := Field.Index;
end;

procedure TDeSQLDataset.DoOpen;
var i,j: Integer;
begin
  CheckDatasetObject;
  if Descr.QueryType in [qtSelect, qtRow] then
    SetSelectParams(True);
  TDataSet(FSourceSet).DisableControls;
  TDataSet(FSourceSet).Open;

  if Descr.FieldCount = 0 then
    begin
      // Наполняем массив полей, чтобы  1) возвращать значения по SourceIndex 2) получить поля в запросах "select * from..."
      for i:=0 to Pred(TDataSet(FSourceSet).Fields.Count) do
        with TDataSet(FSourceSet).Fields[i] do
          Descr.AddSourceField(i, FieldName, DataType, DataSize);
    end else
    begin
      // Устанавливаем SourceIndex - по нему возвращать значения
      for i:= Low(Descr.Fields) to High(Descr.Fields) do
        for j:= 0 to Pred(TDataSet(FSourceSet).Fields.Count) do
          if SameText(Descr.Fields[i].TargetName, TDataSet(FSourceSet).Fields[j].FieldName) then
            begin
              Descr.Fields[i].SourceIndex:= j;
              Break;
            end;
    end;

  FRecordCount:= -1;
  FCurrentIndex:= 0;
end;

procedure TDeSQLDataset.DoClose;
begin
  CheckDatasetObject;
  TDataSet(FSourceSet).Close;
  FRecordCount:= 0;
end;

function TDeSQLDataset.ExecuteQuery: Boolean;
begin
  Result:= (inherited ExecuteQuery) and Assigned(FSourceSet);
end;

function TDeSQLDataset.InsertRecord(aRecord : TObject) : boolean;
var i: Integer;
    TM: TTableMeta;
    V: Variant;
begin
  if (aRecord is TCacheItem)
    then TM:= TCacheItem(aRecord).Owner.TableMeta else
  if (aRecord is TCacheItemList) and (0 < TCacheItemList(aRecord).Count)
    then TM:= TCacheItemList(aRecord)[0].Owner.TableMeta
    else TM:= nil;

  if Not Assigned(TM) then Exit(False);

  Descr.Clear;
  Descr.Table:= TM.Table;
  Descr.Records:= aRecord;

  if 0<Length(TM.UserInsertSQL) then
    Descr.SQL:=TM.UserInsertSQL
  else
    for i := 0 to TM.Fields.Count-1 do
      // пропускаем нехранимые поля, поля только чтения
      if TM.Fields[i].IsStored and not(TM.Fields[i].ReadOnly) and not(TM.Fields[i].IsLookup) then
        // проверяем запрет на Insert
        if SecuritySystem.CheckPolicyField(TM.Fields[i].ID, sdInsert) then
          // пропускаем поля NotNull в которых пустое значение
          // база должна сама проставить дефолтное значение установленное в БД
          if TM.Fields[i].NotNull then
            begin
              V:= TCacheItem(aRecord).ValueByName[TM.Fields[i].Original];
              if VarIsNullOrEmpty(V) and TM.Fields[i].HaveDefaultDefDB
                then {не включаем поле в запрос, база сама должна определить}
                else Descr.AddParamField(TM.Fields[i].Original, TM.Fields[i].DataType);
            end
          else
            Descr.AddParamField(TM.Fields[i].Original, TM.Fields[i].DataType);

  Initialize; // SetSql(Descr.SQL); //делает тоже самое, но с установкой статуса
  SetInsertParams(aRecord);
  // собственно выполнение запроса
  result := ExecuteQuery;
end;

function TDeSQLDataset.UpdateRecord(aRecord : TObject) : boolean;
var TM      : TTableMeta;
    i       : Integer;
begin
  result := false;
  if Not (aRecord is TCacheItem) then Exit;

  TM:=TCacheItem(aRecord).Owner.TableMeta;
  if Not Assigned(TM) then Exit;

  Descr.Clear;
  Descr.Table := TM.Table;;

  if 0<Length(TM.UserUpdateSQL) then
    Descr.SQL:=TM.UserUpdateSQL
  else
    begin
      for i := 0 to TM.Fields.Count-1 do
        if (not TM.Fields[i].IsLookup) and TM.Fields[i].IsStored then
          if not(TM.Fields[i].Key) then
            if not(TM.Fields[i].IsReadOnly) then
              if SecuritySystem.CheckPolicyField(TM.Fields[i].ID, sdUpdate) then
                 Descr.AddParamField(TM.Fields[i].Original, TM.Fields[i].DataType);

      for i:=0 to TM.KeyCount-1 do
        begin
          if i>0 then
            Descr.AddOperation(opAnd);
          Descr.AddParamCondition(TM.KField[i].Original, TM.KField[i].DataType, opEQ, TM.KField[i].Original);
        end;
    end;

  // устанавливаем SQL-запрос
  Initialize; //  SetSql(Descr.SQL);
  SetUpdateParams(aRecord);

  // собственно выполнение запроса
  // try except end на верхнем уровне
  Result := ExecuteQuery;
end;

function TDeSQLDataset.DeleteRecord(aRecord : TObject) : boolean;
var TM   : TTableMeta;
    i    : Integer;
begin
  result := false;
  if Not (aRecord is TCacheItem) then Exit;

  TM:=TCacheItem(aRecord).Owner.TableMeta;
  if Not Assigned(TM) then Exit;

  Descr.Clear;
  Descr.Table := TM.Table;;

  if 0<Length(TM.UserDeleteSQL) then
    Descr.SQL:=TM.UserDeleteSQL
  else
    for i:=0 to TM.KeyCount-1 do
      begin
        if i>0 then Descr.AddOperation(opAnd);
        Descr.AddParamCondition(TM.KField[i].Original,TM.KField[i].DataType,
                                                    opEQ,TM.KField[i].Original);
      end;

  // устанавливаем SQL-запрос
  Initialize; // SetSql(vSQL);
  SetDeleteParams(aRecord);
  // собственно выполнение запроса
  result := ExecuteQuery;

//  CheckSynchronize(0);
end;

function TDeSQLDataset.GetMaxKeyValue(const MetaID: Integer): Variant;
var TableMeta : TTableMeta;
begin
  if MetaID>-1 then
    begin
      TableMeta := MetaData.GetTableMeta(MetaID);
      Descr.Table := TableMeta.Table;
    end
  else
    begin
      TableMeta := MetaData.GetTableByName(Descr.Table, Database);
    end;

  Descr.AddField(opMAX, TableMeta.KField[0].Original);
  Open;
  result:= Value[0];
  Close;
end;

function TDeSQLDataset.Locate(const KeyFields: string; const KeyValues: Variant; const Options: TLocateOptions): Boolean;
var
  KFields  : TStringList;
  R,i,vCount : integer;
begin
  result:= False;
  CheckDatasetObject;

  KFields := TStringList.Create;
  //KFields.Delimiter := ';';
  KFields.CommaText :=  StringReplace(KeyFields,' ','',[rfReplaceAll]);
  vCount:= VarArrayDimCount(KeyValues);

  if (KFields.Count = 0) or (KFields.Count <> VarArrayDimCount(KeyValues)) then Exit;

  if KFields.Count = 1
    then
      for R:= 0 to Pred(RecordCount) do
        begin
          RecNo:= R;
          Result:= VarSameValue(ValueByName[KFields[i]], KeyValues);
          if Result then Break;
        end
    else
      for R:= 0 to Pred(RecordCount) do
        begin
          RecNo:= R;
          Result:= true;
          for i:= 0 to Pred(KFields.Count) do
            if not VarSameValue(ValueByName[KFields[i]], KeyValues[i]) then
              begin
                Result:= False;
                Break;
              end;

          if Result then Break;
        end;
  KFields.Free;
end;

procedure TDeSQLDataset.CreateFields(aFields : TObjectList; const Stage: TFieldStage);
var I        : Integer;
    TF       : TField;
    aType, aNewType: TFieldType;
    aSize, aNewSize: Integer;
    aClass   : TFieldClass;
begin
  if Active then Close;
  Assert(Assigned(FSourceSet), 'Dataset not assigned!');
  TDataSet(FSourceSet).Fields.Clear;

  for I := 0 to aFields.Count-1 do
    if TFieldMeta(aFields[I]).Stage <= Stage then
      if (TFieldMeta(aFields[I]).IsStored) then
        if Length(TFieldMeta(aFields[I]).Original) > 0 then
          begin
            aType := TFieldMeta(aFields[I]).DataType;
            aSize := TFieldMeta(aFields[I]).DataSize;
            if Assigned(Database) and Database.IsFieldTypeConvert(aType, aSize, aNewType, aNewSize) then
              begin
                aType := aNewType;
                aSize := aNewSize;
              end;
            aClass := DefaultFieldClasses[aType];

            if Assigned(aClass) then TF := aClass.Create(FSourceSet)
                                else TF := TField.Create(FSourceSet);

            TF.FieldName         := TFieldMeta(aFields[I]).Original;
            if (aType in [ftBCD]) then TF.Size:= aSize;
            if (aType in [ftString, ftWideString, ftBytes, ftVarBytes]) then
              if (aSize=0) then TF.Size:= MaxInt
                           else TF.Size:= aSize;
            TF.DataSet           := TDataSet(FSourceSet);
//          TF.DisplayLabel      := TFieldMeta(aFields[I]).Native; // нигде не используется, местами сильно тормозит
//          TF.Tag               := NativeInt(TFieldMeta(aFields[I]));
          end;
end;

procedure TDeSQLDataset.CreateDescriptor(const aQueryType: TDeQueryType);
begin
  inherited CreateDescriptor(aQueryType);
  if Assigned(Descr) then
    Descr.OnSetParamValue := SetSQLParamValue;
end;

function TDeSQLDataset.Initialize: Boolean;
begin
  result := inherited Initialize;

  if Not Assigned(Descr.Calc) then
    begin
      Descr.Calc:= TDeSQLCalculator.Create;
      Descr.Calc.ParamList:= Descr.Params;
      Descr.Calc.onTableToStr:= Database.TableToStr;
      Descr.Calc.onFieldToStr:= Database.FieldToStr;
      Descr.Calc.onConstToStr:= Database.ConstToStr;
      Descr.Calc.onFuncToStr:= Database.FuncToStr;
    end;

  SetSql(Descr.SQL);
end;

function TDeSQLDataset.ParamExists(const PName: String): boolean;
var
  I: Integer;
begin
  Result:=False;
  for i := 0 to ParamCount - 1 do
    if AnsiCompareText(PName, ParamNames(i)) = 0 then
      begin
        Result:=True;
        Break;
      end
end;

function TDeSQLDataset.ParamNames(const PNum: Integer): string;
begin
  Result := EmptyStr;
end;

function TDeSQLDataset.ParamValues(const PNum: Integer): Variant;
begin
  Result := Unassigned;
end;

function TDeSQLDataset.IndexTFieldByName(const FieldName: string; var aField: TField): Integer;
begin
  Result:= inherited IndexTFieldByName(FieldName, aField);
  if TDataSet(FSourceSet).Active then
    try
      Result:= TDataSet(FSourceSet).FieldList.IndexOf(FieldName);
      if -1 < Result then
        aField:= TDataSet(FSourceSet).FieldList[Result];
    except
      Result:= -1;
    end;
end;

{ TDeBDEDataset }
{
constructor TDeBDEDataset.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FIsNeedQueryCount := False;

  FData := TQuery.Create(nil);
  if Assigned(Data) then
    Data.FreeNotification(Self);
end;

{function TDeBDEDataset.GetParamByNumber(PNum: Integer): TParam;
begin
  result := nil;
  try
    result := TQuery(Data).Params[PNum];
  except
  end
end;

function TDeBDEDataset.GetParamByName(const PName: String): TParam;
begin
  result := nil;
  try
    result := TQuery(Data).Params.FindParam(PName);
  except
  end
end;}
 {
function TDeBDEDataset.GetParamCount: Integer;
begin
  Result:=TQuery(Data).ParamCount;
end;

function TDeBDEDataset.FetchAll : boolean;
begin
  Result := Active;
 // if Result then TQuery(Data).FetchAll;
end;

procedure TDeBDEDataset.Prepare;
begin
  TQuery(Data).Prepare;
end;

procedure TDeBDEDataset.BlockReadMode;
begin
  TQuery(Data).BlockReadSize:= TQuery(Data).RecordCount;
end;

procedure TDeBDEDataset.UnBlockReadMode;
begin
  TQuery(Data).BlockReadSize:=0;
end;

function TDeBDEDataset.ExecuteQuery: Boolean;
begin
  // Обработка ошибок "try except end" выполняется на верхнем уровне
  Result := False;
  if inherited ExecuteQuery then
    begin
      TQuery(Data).ExecSQL;
      Result := True;
    end;
end;

function TDeBDEDataset.ParamExists(const PName: String): boolean;
begin
  result := Assigned(TQuery(Data).Params.FindParam(PName));
end;

function TDeBDEDataset.ParamNames(PNum: Integer): String;
begin
  result := TQuery(Data).Params[PNum].Name;
end;

function TDeBDEDataset.ParamValues(PNum: Integer): Variant;
begin
  result := TQuery(Data).Params[PNum].Value;
end;

procedure TDeBDEDataset.ClearParams;
begin
  TQuery(Data).Params.Clear;
end;

procedure TDeBDEDataset.CreateParam(PName: String);
var
  aParam : TParam;
begin
  if ParamExists(PName) then Exit ;
  aParam := TParam.Create(TQuery(Data).Params);
  aParam.Name := PName;
  TQuery(Data).Params.AddParam(aParam);
end;

procedure TDeBDEDataset.SetParamValue(PName: string; PValue: Variant;
  ParamType : TFieldType = ftUnknown);
var
  aParam : TParam;
begin
  if not ParamExists(PName) then
    CreateParam(PName);
  try
    aParam :=  TQuery(Data).ParamByName(PName);
    aParam.DataType := ParamType;
    aParam.Value:=PValue;
  except
  end
end;

procedure TDeBDEDataset.SetSql(SQLText: String);
var
  CurrentSQL : string;
  p                  : integer;
begin
  try
    CurrentSQL := TQuery(Data).SQL.Text;
    p := Length(CurrentSQL) - 1;
    if system.Copy(CurrentSQL, p, 2) = #$0D#$0A then
      Delete(CurrentSQL, p, 2);
    if CompareText(CurrentSQL, SQLText) <> 0 then
      begin
        TQuery(Data).SQL.Text := SQLText;
        TQuery(Data).Prepare;
      end;
  except
  end;
end;

{ TDeIBDataset }

constructor TDeIBDataset.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FIsNeedQueryCount := True;

  FSourceSet := TIBQuery.Create(nil);
  if Assigned(FSourceSet) then
    begin
      FSourceSet.FreeNotification(Self);
      TIBQuery(FSourceSet).BeforeOpen := LogBeforeOpen;
    end;
end;

destructor TDeIBDataset.Destroy;
var TR : TIBTransaction;
    N  : Integer;
begin
  TR := TIBQuery(FSourceSet).Transaction;
  TIBQuery(FSourceSet).Close;
  TIBQuery(FSourceSet).Transaction:=nil;

  if Assigned(TR) then
    N  := TR.SQLObjectCount
  else
    N:=0;

  if N=0 then TR.Free;

  if Assigned(FSourceSet) then TIBQuery(FSourceSet).BeforeOpen := nil;

  inherited Destroy;
end;

{function TDeIBDataset.GetParamByNumber(PNum: Integer): TParam;
begin
  result := nil;
  try
    result := TIBQuery(Data).Params[PNum];
  except
  end
end;

function TDeIBDataset.GetParamByName(const PName: String): TParam;
begin
  result := nil;
  try
    result := TIBQuery(Data).Params.FindParam(PName);
  except
  end
end;}

function TDeIBDataset.GetParamCount: Integer;
begin
  Result:=TIBQuery(FSourceSet).ParamCount;
end;

function TDeIBDataset.FetchAll : boolean;
begin
  Result := Active;
 { if Result then
    TIBQuery(Data).FetchAll;}
end;

procedure TDeIBDataset.Prepare;
begin
  TIBQuery(FSourceSet).Prepare;
end;

function TDeIBDataset.ExecuteQuery: Boolean;
begin
  // Обработка ошибок "try except end" выполняется на верхнем уровне
  Result := False;
  if inherited ExecuteQuery then
    begin
      TIBQuery(FSourceSet).ExecSQL;
      Result := True;
    end;
end;

procedure TDeIBDataset.ClearParams;
begin
  TIBQuery(FSourceSet).Params.Clear;
end;

procedure TDeIBDataset.CreateParam(const PName: string);
{var
  aParam : TParam;}
begin
  {RK20050119.1 Пусть сам параметры создает
  if ParamExists(PName) then Exit ;
  aParam := TParam.Create(TIBQuery(Data).Params);
  aParam.Name := PName;
  TIBQuery(Data).Params.AddParam(aParam);
  }
end;

function TDeIBDataset.ParamExists(const PName: String): boolean;
begin
  result := Assigned(TIBQuery(FSourceSet).Params.FindParam(PName));
end;

function TDeIBDataset.ParamNames(const PNum: Integer): string;
begin
  Result := TIBQuery(FSourceSet).Params[PNum].Name;
end;

function TDeIBDataset.ParamValues(const PNum: Integer): Variant;
begin
  Result := TIBQuery(FSourceSet).Params[PNum].Value;
end;

procedure TDeIBDataset.SetParamValue(const PName: string; const PValue: Variant; const ParamType: TFieldType);
var
  aParam : TParam;
begin
  {RK20050119.1 Ну нет параметра и нечего его трогать
  if not ParamExists(PName) then
    CreateParam(PName);
  }
  if not ParamExists(PName) then
    exit;
  {RK20050119.1}
  try
    aParam := TIBQuery(FSourceSet).ParamByName(PName);
    aParam.DataType := ParamType;
    aParam.Value:=PValue;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('TDeIBDataset.SetParamValue error: ' + E.Message);
    {$ENDIF}
  end
end;

procedure TDeIBDataset.SetSQL(const SQLText: string);
var
  CurrentSQL   : string;
  p            : integer;
begin
  try
    CurrentSQL := TIBQuery(FSourceSet).SQL.Text;
    p := Length(CurrentSQL) - 1;
    if system.Copy(CurrentSQL, p, 2) = #$0D#$0A then
      Delete(CurrentSQL, p, 2);
    if CompareText(CurrentSQL, SQLText) <> 0 then
    begin
      TIBQuery(FSourceSet).SQL.Text := SQLText;
      TIBQuery(FSourceSet).Prepare;
    end;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('TDeIBDataset.SetSQL error: ' + E.Message);
    {$ENDIF}
  end;
end;

{ TDeFireDACDataset }

constructor TDeFireDACDataset.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FIsNeedQueryCount := False; //True;

  FSourceSet := TFDQuery.Create(nil);
  if Assigned(FSourceSet) then
    begin
      // Без этого параметра возвращаются значения с пробелами справа и ошибками длины поля если кодировка UTF
      TFDQuery(FSourceSet).FormatOptions.StrsTrim2Len := True;
      TFDQuery(FSourceSet).FetchOptions.RecordCountMode:= cmTotal;
      FSourceSet.FreeNotification(Self);
      TIBQuery(FSourceSet).BeforeOpen := LogBeforeOpen;
    end;
end;

destructor TDeFireDACDataset.Destroy;
var TR : TIBTransaction;
    N  : Integer;
begin
 {
  TR := TIBQuery(Data).Transaction;
  TFDQuery(Data).Close;
  TFDQuery(Data).Transaction:=nil;

  if Assigned(TR) then
    N  := TR.SQLObjectCount
  else
    N:=0;

  if N=0 then TR.Free;
  {}
  if Assigned(FSourceSet) then TIBQuery(FSourceSet).BeforeOpen := nil;
  inherited Destroy;
end;

{function TDeFBDataset.GetParamByNumber(PNum: Integer): TParam;
begin
  result := nil;
  try
    result := TIBQuery(Data).Params[PNum];
  except
  end
end;

function TDeFBDataset.GetParamByName(const PName: String): TParam;
begin
  result := nil;
  try
    result := TIBQuery(Data).Params.FindParam(PName);
  except
  end
end;}

function TDeFireDACDataset.GetParamCount: Integer;
begin
  Result:=TFDQuery(FSourceSet).ParamCount;
end;

function TDeFireDACDataset.GetRecordCount: integer;
begin
  Result:= Internal_Count;
end;

function TDeFireDACDataset.Internal_Count: Integer;
begin
  { TFDTable и TFDQuery имеют общего родителя TFDCustomQuery }
  Result:= TFDCustomQuery(FSourceSet).RecordCount;
  { если количество записей равно размеру кеша - запрашиваем отдельным запросом }
  {
  if Result = TFDCustomQuery(FSourceSet).FetchOptions.RowsetSize then
    Result:= inherited GetRecordCount;
  {}
end;

function TDeFireDACDataset.FetchAll : boolean;
begin
  Result := Active;
 { if Result then
    TIBQuery(Data).FetchAll;}
end;

procedure TDeFireDACDataset.Prepare;
begin
  try
    TFDCustomQuery(FSourceSet).Prepare;
  except
    on E: Exception do
      Funcs.WriteLog('TDeIBDataset.SetParamValue error: ' + E.Message);
  end;
end;

function TDeFireDACDataset.ExecuteQuery: Boolean;
begin
  // Обработка ошибок "try except end" выполняется на верхнем уровне
  Result := False;
  if inherited ExecuteQuery then
    begin
      TFDQuery(FSourceSet).ExecSQL;
      Result := True;
    end;
end;

procedure TDeFireDACDataset.ClearParams;
begin
  TFDQuery(FSourceSet).Params.Clear;
end;

procedure TDeFireDACDataset.CreateParam(const PName: string);
var
  aParam : TFDParam;
begin
  if not ParamExists(PName) then
    begin
      Assert(Assigned(FSourceSet), 'TDeFireDACDataset(Data) is nil!!!');
      aParam := TFDQuery(FSourceSet).Params.Add;
      aParam.Name := PName;
    end;
end;

function TDeFireDACDataset.ParamExists(const PName: String): boolean;
begin
  result := Assigned(TFDQuery(FSourceSet).Params.FindParam(PName));
end;

function TDeFireDACDataset.ParamNames(const PNum: Integer): string;
begin
  Result := TFDQuery(FSourceSet).Params[PNum].Name;
end;

function TDeFireDACDataset.ParamValues(const PNum: Integer): Variant;
begin
  Result := TFDQuery(FSourceSet).Params[PNum].Value;
end;

procedure TDeFireDACDataset.SetParamValue(const PName: string; const PValue: Variant; const ParamType: TFieldType);
var
  aParam : TFDParam;
begin
  aParam:= TFDQuery(FSourceSet).Params.FindParam(PName);
  if not Assigned(aParam) then
    begin
      aParam := TFDQuery(FSourceSet).Params.Add;
      aParam.Name := PName;
    end;

  try
    // Параметры типа ftMemo надо приводить к ftString, иначе получаем ошибку

    case ParamType of
      ftDateTime: aParam.DataType:= ftTimeStamp;
      ftMemo:     aParam.DataType:= ftString;
      ftWideMemo: aParam.DataType:= ftString;
      else        aParam.DataType:= ParamType;
    end;
 //   aParam.Size := 0;

    aParam.Value := PValue;
  except
    on E: Exception do
      Funcs.WriteLog('TDeFireDACDataset.SetParamValue error: ' + E.Message);
  end
end;

procedure TDeFireDACDataset.SetSQL(const SQLText: string);
var
  CurrentSQL   : string;
  p            : integer;
begin
  try
    CurrentSQL := TFDQuery(FSourceSet).SQL.Text;
    p := Length(CurrentSQL) - 1;
    if system.Copy(CurrentSQL, p, 2) = #$0D#$0A then
      Delete(CurrentSQL, p, 2);
    if CompareText(CurrentSQL, SQLText) <> 0 then
    begin
      TFDQuery(FSourceSet).SQL.Text := SQLText;
      TFDQuery(FSourceSet).Prepare;
    end;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('TDeIBDataset.SetSQL error: ' + E.Message);
    {$ENDIF}
  end;
end;

{ TDeADODataset }

constructor TDeADODataset.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FSourceSet := TADOQuery.Create(nil);
  if Assigned(FSourceSet) then
    begin
      FSourceSet.FreeNotification(Self);
      TADOQuery(FSourceSet).ProfitOptimized := True;
      TIBQuery(FSourceSet).BeforeOpen := LogBeforeOpen;
    end;
  FIsNeedQueryCount := False; // XE7
end;

destructor TDeADODataset.Destroy;
begin
  if Assigned(FSourceSet) then TIBQuery(FSourceSet).BeforeOpen := nil;
  inherited Destroy;
end;

function TDeADODataset.GetParamCount: Integer;
begin
  // Поменял закомментированный код, т.к. по FreeNotification свойство Data может быть nil!!!
  //Result:= TADOQuery(Data).Parameters.Count;
  if Assigned(FSourceSet) and (FSourceSet is TADOQuery) then
    Result:= (FSourceSet as TADOQuery).Parameters.Count
  else
    Result := 0;
end;

function TDeADODataset.Internal_Goto(const aIndex: integer): Boolean;
var Delta: Integer;
begin
  // в случае MS SQL дает значительный прирост
  // время SET RECNO = номер записи / 100 мкс
  // время MOVE TO   = дистанция * 2 мкс
  Delta := Succ(aIndex) - TDataSet(FSourceSet).RecNo;
  if Abs(Delta) < aIndex div 200 then
    TDataSet(FSourceSet).MoveBy(Delta)
  else
    TDataSet(FSourceSet).RecNo := Succ(aIndex);
end;

function TDeADODataset.NewDescriptor(const aQueryType: TDeQueryType): TCustomQueryDescr;
begin
  case aQueryType of
    qtHole: { Запрос поиска "дырок" }
      Result := TMSSQLHoleFieldQueryDescr.Create;
  else
    Result := inherited NewDescriptor(aQueryType);
  end;
end;

function TDeADODataset.FetchAll: Boolean;
begin
  Result := Active;
end;

procedure TDeADODataset.Prepare;
begin
  // Поменял закомментированный код, т.к. по FreeNotification свойство Data может быть nil!!!
  //TADOQuery(Data).Prepared:=True;
  if Assigned(FSourceSet) and (FSourceSet is TADOQuery) then
    (FSourceSet as TADOQuery).Prepared := True;
end;

function TDeADODataset.ExecuteQuery: Boolean;
begin
  // Обработка ошибок "try except end" выполняется на верхнем уровне
  Result := False;
  if inherited ExecuteQuery then
    begin
      Assert(Assigned(FSourceSet), 'TADOQuery(Data) is nil!!!');
      LogBeforeOpen(TADOQuery(FSourceSet));
      Result := (0 < TADOQuery(FSourceSet).ExecSQL); // возвращает количество записей которым примениля ExecSQL
    end;
end;

procedure TDeADODataset.ClearParams;
begin
  // Поменял закомментированный код, т.к. по FreeNotification свойство Data может быть nil!!!
  //TADOQuery(Data).Parameters.Clear;
  if Assigned(FSourceSet) and (FSourceSet is TADOQuery) then
    (FSourceSet as TADOQuery).Parameters.Clear;
end;

procedure TDeADODataset.CreateParam(const PName: string);
var
  aParam : TParameter;
begin
  if not ParamExists(PName) then
    begin
      Assert(Assigned(FSourceSet), 'TADOQuery(Data) is nil!!!');
      aParam := TADOQuery(FSourceSet).Parameters.AddParameter;
      aParam.Name := PName;
    end;
end;

function TDeADODataset.ParamExists(const PName: string): Boolean;
begin
  // Поменял закомментированный код, т.к. по FreeNotification свойство Data может быть nil!!!
  //result := Assigned(TADOQuery(Data).Parameters.FindParam(PName));
  if Assigned(FSourceSet) and (FSourceSet is TADOQuery) then
    Result := Assigned((FSourceSet as TADOQuery).Parameters.FindParam(PName))
  else
    Result := False;
end;

function TDeADODataset.ParamNames(const PNum: Integer): string;
begin
  // Поменял закомментированный код, т.к. по FreeNotification свойство Data может быть nil!!!
  //result:=TADOQuery(Data).Parameters[PNum].Name;
  if Assigned(FSourceSet) and (FSourceSet is TADOQuery) then
    Result := (FSourceSet as TADOQuery).Parameters[PNum].Name
  else
    Result := EmptyStr;
end;

function TDeADODataset.ParamValues(const PNum: Integer): Variant;
begin
  // Поменял закомментированный код, т.к. по FreeNotification свойство Data может быть nil!!!
  //result:=TADOQuery(Data).Parameters[PNum].Value;
  if Assigned(FSourceSet) and (FSourceSet is TADOQuery) then
    Result := (FSourceSet as TADOQuery).Parameters[PNum].Value
  else
    Result := Unassigned;
end;

procedure TDeADODataset.SetParamValue(const PName: string; const PValue: Variant; const ParamType: TFieldType);
begin
  if not ParamExists(PName) then
    CreateParam(PName);
  try

    // Параметры типа ftMemo надо приводить к ftString, иначе получаем ошибку
    case ParamType of
      ftMemo:     TADOQuery(FSourceSet).Parameters.ParamByName(PName).DataType:= ftString;
      ftWideMemo: TADOQuery(FSourceSet).Parameters.ParamByName(PName).DataType:= ftWideString;
      else        TADOQuery(FSourceSet).Parameters.ParamByName(PName).DataType:= ParamType;
    end;

    if ParamType in StringTypes then
      begin
        // Из-за параметризации пустой строки натыкались на грабли (без NativeClient):
        // Если длина строкового параметра 0, то это TEXT тип SQL Server`а,
        // а его нельзя использовать в простых запросах!!!
        TADOQuery(FSourceSet).Parameters.ParamByName(PName).Size := Max(Length(VarToStr(PValue)), 1);
        // Это что за фигня? Т.е. если я в строкое поле вписываю "null",
        // то поле не будет содержать этого слова? На мой взгляд это бред...
        // Но оставил чтоб ничего не поломать...
        if VarIsNull(PValue) or (CompareStr(VarToStr(PValue),'null')=0) then
          TADOQuery(FSourceSet).Parameters.ParamByName(PName).Value:=null
        else
          TADOQuery(FSourceSet).Parameters.ParamByName(PName).Value:=PValue;
      end else

    if ParamType in BinaryTypes then
      begin
        if Length(VarToStr(PValue))=0 then
          TADOQuery(FSourceSet).Parameters.ParamByName(PName).Value:=null
        else
          TADOQuery(FSourceSet).Parameters.ParamByName(PName).Value:=PValue;
      end else

     if ParamType in NumericTypes then
       begin
        if Length(VarToStr(PValue))=0 then
          TADOQuery(FSourceSet).Parameters.ParamByName(PName).Value:=null
        else
          TADOQuery(FSourceSet).Parameters.ParamByName(PName).Value:=PValue;
       end else

     if ParamType in DateTimeTypes then
       begin
        if Length(VarToStr(PValue))=0 then
          TADOQuery(FSourceSet).Parameters.ParamByName(PName).Value:=null
        else
          TADOQuery(FSourceSet).Parameters.ParamByName(PName).Value:=PValue;
       end else

      begin
        TADOQuery(FSourceSet).Parameters.ParamByName(PName).Size := 0;
        if VarIsStr(PValue) and (Length(VarToStr(PValue)) = 0) then
          TADOQuery(FSourceSet).Parameters.ParamByName(PName).Value := null
        else
          TADOQuery(FSourceSet).Parameters.ParamByName(PName).Value := PValue;
      end;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('TDeADODataset.SetParamValue error: ' + E.Message);
    {$ENDIF}
  end
end;

(*
procedure TDeADODataset.SetParamValue(const PName: string; const PValue: Variant; const ParamType: TFieldType);
var P: TParameter;
    PSize: Integer;
    V: Variant;
begin
  try
    P:= TADOQuery(Data).Parameters.ParamByName(PName);
  except
    P:= nil;
  end;

  try
    if ParamType in StringTypes then
      begin
        // Из-за параметризации пустой строки натыкались на грабли (без NativeClient):
        // Если длина строкового параметра 0, то это TEXT тип SQL Server`а,
        // а его нельзя использовать в простых запросах!!!
        PSize := Max(Length(VarToStr(PValue)), 1);
        // Это что за фигня? Т.е. если я в строкое поле вписываю "null",
        // то поле не будет содержать этого слова? На мой взгляд это бред...
        // Но оставил чтоб ничего не поломать...
        if VarIsNull(PValue) or (CompareStr(VarToStr(PValue),'null')=0) then V:= null
                                                                        else V:= PValue;
      end else

    if ParamType in (NumericTypes + DateTimeTypes + [ftBLOB]) then
      begin
        if Length(VarToStr(PValue))=0 then V:= null
                                      else V:= PValue;
      end else

      begin
        if VarIsStr(PValue) and (Length(VarToStr(PValue)) = 0) then V:= null
                                                               else V:= PValue;
      end;

    if P <> nil  then
      begin
        if P.DataType <> ParamType then P.DataType:= ParamType;
        if P.Size <> PSize then P.Size:= PSize;
        if P.Value <> V then P.Value:= V;
      end
    else
      begin
        Assert(Assigned(Data), 'TADOQuery(Data) is nil!!!');
        P:= TADOQuery(Data).Parameters.CreateParameter( PName, ParamType, pdInput, PSize, V);
      end;

  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('TDeADODataset.SetParamValue error: ' + E.Message);
    {$ENDIF}
  end
end;
*)
procedure TDeADODataset.SetSQL(const SQLText: String);
begin
  try
    TADOQuery(FSourceSet).SQL.Text := SQLText;
  except
    on E: Exception do
      WriteLog('TDeADODataset.SetSQL error: ' + E.Message, True, 'Errors');
  end;
end;

procedure TDeADODataset.SaveToADOXML(const FileName: string);
begin
  TADOQuery(FSourceSet).SaveToFile(FileName, pfXML);
end;

{ TDeOracleDataset }
 {
procedure TDeOracleDataset.ClearParams;
begin
  TOraQuery(Data).Params.Clear;
end;

constructor TDeOracleDataset.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FData := TOraQuery.Create(Self);
  if Assigned(Data) then
    Data.FreeNotification(Self);
  //TOracleDataSet(Data)
end;

procedure TDeOracleDataset.CreateParam(PName: String);
begin
  if TOraQuery(Data).FindParam(PName)<>nil then Exit ;
//TODO: ORACLE   TOraQuery(Data).Params.a .DeclareVariable(UpperCase(PName),otString);
end;

destructor TDeOracleDataset.Destroy;
begin
  Data.Free;
  inherited;
end;

function TDeOracleDataset.ExecuteQuery: Boolean;
begin
  // Обработка ошибок "try except end" выполняется на верхнем уровне
  Result := False;
  if inherited ExecuteQuery then
    begin
      TOraQuery(Data).ExecSQL;
      Result := True;
    end;
end;

function TDeOracleDataset.FetchAll: boolean;
begin
  result := True;
end;

function TDeOracleDataset.GetParamCount: Integer;
begin
  result := TOraQuery(Data).ParamCount;
end;

function TDeOracleDataset.ParamExists(const PName: String): boolean;
begin
  result := (TOraQuery(Data).ParamByName(UpperCase(PName))<>nil);
end;

function TDeOracleDataset.ParamNames(PNum: Integer): string;
begin
  result := TOraQuery(Data).Params[PNum].Name;
end;

function TDeOracleDataset.ParamValues(PNum: Integer): Variant;
begin
  result := TOraQuery(Data).Params[PNum].Value;
end;

procedure TDeOracleDataset.Prepare;
begin
  TOraQuery(Data).Prepare;
end;

procedure TDeOracleDataset.SetParamValue(PName: string; PValue: Variant;
  ParamType : TFieldType = ftUnknown);
var TP: TParam;
begin
  try
    TP:=TOraQuery(Data).FindParam(PName);
    if TP=nil then
      TP:=TOraQuery(Data).Params.CreateParam(ParamType, PName, ptInputOutput);

    TP.Value:=PValue;
  except
  end
end;

procedure TDeOracleDataset.SetSql(SQLText: String);
begin
  try
    TOraQuery(Data).SQL.Text := SQLText;
    TOraQuery(Data).Prepare;
  except
  end;
end;

//++++++++++++++FileDB DE DataSet+++++++++++++++++++++++++++++++++++++++++++++++++++++++

{ TFileDB_DEDS }

function TFileDB_DEDS.CheckConditions: Boolean;
begin
  result := true;
end;

constructor TFileDB_DEDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive:=False;
  FileDB:=TFileDB.Create(Self);
end;

procedure TFileDB_DEDS.CreateFields(aFields: TObjectList; const Stage: TFieldStage);
var
  i : integer;
begin
  setlength(FFields,0);
  for i:= 0 to aFields.Count - 1 do
    if not (TFieldMeta(aFields[i]).IsLookup or (not TFieldMeta(aFields[i]).IsStored)) then
    begin
      SetLength(FFields,length(FFields)+1);
      FFields[High(FFields)].fName := TFieldMeta(aFields[i]).Original;
      FFields[High(FFields)].fType := TFieldMeta(aFields[i]).DataType;
      FFields[High(FFields)].fSize := TFieldMeta(aFields[i]).DataSize;
    end;
end;

function TFileDB_DEDS.DeleteRecord(aRecord: TObject): boolean;
var
  i:integer;
  fl:TFile;
begin
  Result:=False;
  fl.ID:=-1;
  for i:=0 to TCacheItem(aRecord).Owner.FieldCount-1 do begin
    if AnsiUpperCase(TCacheItem(aRecord).Owner.Fields.Items[i].Original)='ID' then Result:=True;
    SetValueToField(TCacheItem(aRecord).Owner.Fields.Items[i].Original,
                    TCacheItem(aRecord).FieldValue[i],
                    fl);
  end;
  if fl.ID=-1 then Result:=False;
  if Result then FileDB.DeleteFile(fl.ID);
end;

destructor TFileDB_DEDS.Destroy;
begin
  inherited Destroy;
end;

procedure TFileDB_DEDS.DoClose;
begin
  FActive:=False;
end;

procedure TFileDB_DEDS.DoOpen;
begin
  if length(FFields)<=0 then
    ReadFields;
  Prepare;
  FActive:=True;
end;

function TFileDB_DEDS.FetchAll: boolean;
begin
//  FActive:=True;
  Result:=True;
end;

function TFileDB_DEDS.GetActive: boolean;
begin
  Result:=FActive;
end;

function TFileDB_DEDS.GetMaxKeyValue(const MetaID: Integer = -1): Variant;
begin
  Result := 0;
end;

function TFileDB_DEDS.GetRecNo: integer;
begin
  Result:=FileDB.FileDataSet.RecNo;
end;

function TFileDB_DEDS.GetRecordCount: integer;
begin
  Result:=FileDB.FileDataSet.Count;
end;

function TFileDB_DEDS.GetValue(const Index: Integer): Variant;
begin
  if (Index>=0) and (Index <Length(FFields))then
    Result := FileDB.FileDataSet.FieldByName(FFields[Index].fName).AsVariant
  else
    Result := Unassigned;
end;

function TFileDB_DEDS.IndexByName(const Name: string): Integer;
begin
  { TODO -oКуфенко П. Ю. -cOptimization : Вообще-то лучше бы сделать это через массив... }
  if SameText(Name, 'ID') then
    Result := 0
  else if SameText(Name, 'NAME') then
    Result := 1
  else if SameText(Name, 'BODY') then
    Result := 2
  else if SameText(Name, 'FILETYPE') then
    Result := 3
  else if SameText(Name, 'SIZE') then
    Result := 4
  else if SameText(Name, 'DATETIME') then
    Result := 5
  else if SameText(Name, 'FILEATTR') then
    Result := 6
  else if SameText(Name, 'FULLPATH') then
    Result := 7
  else if SameText(Name, 'REALFILENAME') then
    Result := 8
  else if SameText(Name, 'PARENTFOLDER') then
    Result := 9
  else if SameText(Name, 'PARENTSUBFOLDER') then
    Result := 10
  else if SameText(Name, 'REALFULLPATH') then
    Result := 11
  else
    Result := -1;
end;

function TFileDB_DEDS.Initialize: boolean;
begin
  Result:=FileDB.SetCurrentTable(Descr.Table);
end;

function TFileDB_DEDS.InsertRecord(aRecord: TObject): boolean;
var
  fl:TFile;
begin
  fl.Name:= 'New file.txt';
  fl.Body:= EmptyStr;
  FLastFileID:=FileDB.InsertFile(fl);
  Result:=True;
end;
{
procedure TFileDB_DEDS.LoadRecord(aRecord: TObject; const aStage: TFieldStage; aSourceIndex: Integer);
var
  j              : integer;
  ErrorFieldName,s  : string;
  CacheItem      : TCacheItem;
begin
  if aRecord is TCacheItem then begin
    CacheItem := TCacheItem(aRecord);
    FileDB.FileDataSet.RecNo:= aSourceIndex;
    ErrorFieldName := EmptyStr;
    for j := 0 to CacheItem.Owner.FieldCount-1 do
      if not (CacheItem.Owner.Fields[j].IsLookup or CacheItem.Owner.Fields[j].Calculated) then
        try
          s:=FFields[j].fName;
          CacheItem.FieldValue[J] := FileDB.FileDataSet.FieldByName(s).AsVariant;
        except
          on E:Exception do begin
            CacheItem.Owner.Fields[j].ErrorInStructure := true;
            CacheItem.Owner.Fields[j].ErrorMessage :=
              Format(GetTitle('_Error.Readfield'),
                [CacheItem.Owner.Fields[j].Native, E.Message]);
          end;
        end;
  end;
end;
{}
function TFileDB_DEDS.Locate(const KeyFields: string; const KeyValues: Variant; const Options: TLocateOptions): Boolean;
var
  aKey, aValue : Variant;
  R: integer;
begin
  Result:= False;

  if  VarIsArray(KeyValues) then aKey := KeyValues[0]
                            else aKey := KeyValues;

  if VarIsType(aKey,varInteger) then
    if aKey=1 then aKey:=FLastFileID;

  for R:=0 to Pred(FFileDB.FileDataSet.Count) do
    begin
      FFileDB.FileDataSet.RecNo:= R;
      aValue :=FFileDB.FileDataSet.FieldByName(KeyFields).AsVariant;
      if VarSameValue(aKey,aValue) then
        begin
          Result := True;
          Break;
        end;
    end;
end;

procedure TFileDB_DEDS.Prepare;
var
  CFDB:TCommandToFDB;
//  Num:integer;
begin
//  Num:=FileDB.FileDataSet.RecNo;
  FileDB.FileDataSet.Clear;
  CFDB.FDB_ID:=FileDB.CurrentConnection.ClientFDB_ID;
  CFDB.ConnectionID:=FileDB.CurrentConnection.ConnectionID;
  CFDB.iCommand:=fdbc_SELECT;
  CFDB.Params:='0'+cr+'0'+cr+'0';
  FileDB.SendCommandToFDB(CFDB,FileDB.CurrentConnection.LocalConnect);

  if FileDB.CurrentConnection.LocalConnect then begin
  end else begin
  end;
  ReadData;
//  FileDB.FileDataSet.RecNo:=Num;
end;

procedure TFileDB_DEDS.ReadData;
var
  fls: array of TFile;
  R,i: integer;
begin
  For R:=0 to Pred(FileDB.FileDataSet.Count) do
    begin
      FileDB.FileDataSet.RecNo:= R;
      if CheckConditions then begin
        SetLength(fls,High(fls)+2);
        for i:=0 to High(fls)-1 do
          fls[i+1]:=fls[i];
        fls[0]:=FileDB.FileDataSet.GetCurrentRec;
      end else begin
        SetLength(fls,High(fls)+2);
        fls[High(fls)]:=FileDB.FileDataSet.GetCurrentRec;
      end;
    end;
  FileDB.FileDataSet.Clear;
  for i:=0 to High(fls) do
    FileDB.FileDataSet.AddRec(fls[i]);
end;

procedure TFileDB_DEDS.ReadFields;
var
  i:integer;
begin
  SetLength(FFields,12);
  for i:=0 to High(FFields) do
    case i of
      0:begin
          FFields[i].fName:='ID';
          FFields[i].fType:=ftInteger;
          FFields[i].fSize:=0;
        end;
      1:begin
          FFields[i].fName:='NAME';
          FFields[i].fType:=ftString;
          FFields[i].fSize:=255;
        end;
      2:begin
          FFields[i].fName:='BODY';
          FFields[i].fType:=ftBLOB;
          FFields[i].fSize:=0;
        end;
      3:begin
          FFields[i].fName:='FILETYPE';
          FFields[i].fType:=ftInteger;
          FFields[i].fSize:=0;
        end;
      4:begin
          FFields[i].fName:='SIZE';
          FFields[i].fType:=ftInteger;
          FFields[i].fSize:=0;
        end;
      5:begin
          FFields[i].fName:='DATETIME';
          FFields[i].fType:=ftDateTime;
          FFields[i].fSize:=0;
        end;
      6:begin
          FFields[i].fName:='FILEATTR';
          FFields[i].fType:=ftInteger;
          FFields[i].fSize:=0;
        end;
      7:begin
          FFields[i].fName:='FULLPATH';
          FFields[i].fType:=ftString;
          FFields[i].fSize:=255;
        end;
      8:begin
          FFields[i].fName:='REALFILENAME';
          FFields[i].fType:=ftString;
          FFields[i].fSize:=255;
        end;
      9:begin
          FFields[i].fName:='PARENTFOLDER';
          FFields[i].fType:=ftInteger;
          FFields[i].fSize:=0;
        end;
      10:begin
          FFields[i].fName:='PARENTSUBFOLDER';
          FFields[i].fType:=ftInteger;
          FFields[i].fSize:=0;
        end;
      11:begin
          FFields[i].fName:='REALFULLPATH';
          FFields[i].fType:=ftString;
          FFields[i].fSize:=255;
        end;
    end;//Case
end;

procedure TFileDB_DEDS.RetrieveFields(aFields: TObjectList; aTableMeta: TObject = nil);
var
  i, P : integer;
  FMeta : TFieldMeta;
begin
  //ReadFields;
  for i:=0 to High(FFields) do begin
    FMeta := TFieldMeta.Create;
    FMeta.Owner := TTableMeta(aTableMeta);
    FMeta.Original := FFields[i].FName;
    FMeta.DataType := FFields[i].FType;
    FMeta.DataSize := FFields[i].FSize;
    FMeta.Order := I;
    P := Pos('_',FMeta.Original);
    if P = 0 then
      FMeta.Name := FMeta.Original
    else
      FMeta.Name := Copy(FMeta.Original, P, MaxInt);
    FMeta.ShowType := stNative;
    FMeta.Width := 1000 div Length(FFields) +1;
    aFields.Add(FMeta);
  end;
end;

procedure TFileDB_DEDS.SetValue(const Index: Integer; const Value: Variant);
begin
//  if aIndex=1 then;
// { TODO -oКуфенко П. Ю. -cNo implementation : Странная реализация метода, который ВООБЩЕ НИЧЕГО НЕ ДЕЛАЕТ! }
end;

function TFileDB_DEDS.SetValueToField(const FieldName: string; const Value: Variant; var fl: TFile): Boolean;
begin
  //FieldName:={Ansi}UpperCase(FieldName);
  Result:=True;
  if SameText(FieldName, 'ID') then
    fl.ID := Value
  else if SameText(FieldName, 'NAME') then
    fl.Name := Value
  else if SameText(FieldName, 'BODY') then
    fl.Body := Value
  else if SameText(FieldName, 'FILETYPE') then
    fl.FileType := Value
  else if SameText(FieldName, 'SIZE') then
    fl.Size := Value
  else if SameText(FieldName, 'DATETIME') then
    fl.DateTime := Value
  else if SameText(FieldName, 'FILEATTR') then
    fl.FileAttr := Value
  else if SameText(FieldName, 'FULLPATH') then
    fl.FullPath := Value
  else if SameText(FieldName, 'REALFILENAME') then
    fl.RealFileName := Value
  else if SameText(FieldName, 'PARENTFOLDER') then
    fl.iParentFolder := Value
  else if SameText(FieldName, 'PARENTSUBFOLDER') then
    fl.iParentSubFolder := Value
  else if SameText(FieldName, 'REALFULLPATH') then
    fl.RealFullPath := Value
  else
    Result:=False;
end;

function TFileDB_DEDS.UpdateRecord(aRecord: TObject): boolean;
var
  ID:integer;
  R, i:integer;
  fl:TFile;
  Fnd:boolean;
begin
  Result:=False;
  id:=-1;
  for i:=0 to TCacheItem(aRecord).Owner.FieldCount-1 do
    if AnsiUpperCase(TCacheItem(aRecord).Owner.Fields.Items[i].Original)='ID' then begin
      id:=TCacheItem(aRecord).FieldValue[i];
      Break;
    end;
  if Id<0 then Exit;

  Fnd:=False;
  For R:= 0 to Pred(FileDB.FileDataSet.Count) do
    begin
      FileDB.FileDataSet.RecNo:= R;
      if FileDB.FileDataSet.FieldByName('ID').AsInteger=id then
        begin
          Fnd:=True;
          Break;
        end;
    end;
  if not Fnd then Exit;

  fl:=FileDB.FileDataSet.GetCurrentRec;
  for i:=0 to TCacheItem(aRecord).Owner.FieldCount-1 do
    begin
      if AnsiUpperCase(TCacheItem(aRecord).Owner.Fields.Items[i].Original)='ID'
        then Result:=True
        else SetValueToField(TCacheItem(aRecord).Owner.Fields.Items[i].Original, TCacheItem(aRecord).FieldValue[i], fl);
    end;

  if Result then
    FileDB.UpdateFile(fl);

//  Prepare;
end;

{ TDeADOXMLDataset }

constructor TDeADOXMLDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSourceSet := nil;
end;

destructor TDeADOXMLDataset.Destroy;
begin
  if assigned(FSourceSet) then
    begin
      TADODataSet(FSourceSet).Close;
      FreeAndNil(FSourceSet);
    end;

  inherited Destroy;
end;

procedure TDeADOXMLDataset.CheckXMLObject;
begin
  if Assigned(FSourceSet) then
    if FSourceSet is TADODataSet then
      { Ничего не делаем }
    else
      raise EDeDatasetError.Create('Unknown ADO dataset')
  else
    raise EDeDatasetError.Create('ADO dataset not found');
end;

function TDeADOXMLDataset.GetActive: Boolean;
begin
  if assigned(FSourceSet) then Result:= TADODataSet(FSourceSet).Active
                          else Result:= False;
end;

procedure TDeADOXMLDataset.SetRecNo(const Value: Integer);
begin
  FCurrentIndex:= Value;
  Internal_Goto(IND_External2Internal(Value));
end;

function TDeADOXMLDataset.Internal_GetIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
var Field: TField;
begin
  Field:= TADODataSet(FSourceSet).FindField(aIdent);
  Result:= Assigned(Field);
  if Result then aValue:= Field.Value
            else aValue:= null;
end;

function TDeADOXMLDataset.GetValue(const Index: Integer): Variant;
begin
  Result:= inherited GetValue(Index);
  if not VarIsEmpty(Result) then Exit;

  Result:= TADODataSet(FSourceSet).Fields[Descr.Fields[Index].SourceIndex].Value;
end;

procedure TDeADOXMLDataset.SetValue(const Index: Integer; const Value: Variant);
begin
  TADODataSet(FSourceSet).Fields[Index].Value := Value;
end;

function TDeADOXMLDataset.Initialize: Boolean;
begin
  Result := inherited Initialize;
end;

procedure TDeADOXMLDataset.DoOpen;
var i,j,N: Integer;
begin
  FSourceSet := TADODataSet.Create(Self);
  TADODataSet(FSourceSet).CacheSize := 1000;

  N:= TDeADOXMLDatabase(FDatabase).IndexByName(Descr.Table);
  if (-1 < N) then FFileName := TDeADOXMLDatabase(FDatabase).GetFileName(N)
              else FFileName := EmptyStr;

  FModified:= False;
  if FileExists(FFileName) then TADODataSet(FSourceSet).LoadFromFile(FFileName)
                           else raise Exception.Create('Error TDeADOXMLDataset.DoOpen');

  if not TADODataSet(FSourceSet).Active then
    raise Exception.Create('Cannot open file ' + FFileName);

  if Descr.FieldCount = 0 then
    begin
      // Наполняем массив полей, чтобы  1) возвращать значения по SourceIndex 2) получить поля в запросах "select * from..."
      for i:=0 to Pred(TADODataSet(FSourceSet).FieldCount) do
        with TADODataSet(FSourceSet).Fields[i] do
          Descr.AddSourceField(i, FieldName, DataType, Size);
    end else
    begin
      // Устанавливаем SourceIndex - по нему возвращать значения
      for i:= Low(Descr.Fields) to High(Descr.Fields) do
        for j:= 0 to Pred(TADODataSet(FSourceSet).FieldCount) do
          if SameText(Descr.Fields[i].TargetName, TADODataSet(FSourceSet).Fields[j].FieldName) then
            begin
              Descr.Fields[i].SourceIndex:= j;
              Break;
            end;
    end;

  IND_Build;
end;

procedure TDeADOXMLDataset.AfterOpen;
begin
  inherited AfterOpen;
end;

procedure TDeADOXMLDataset.DoClose;
begin
  CheckXMLObject;
  IND_Clear;
  if assigned(FSourceSet) then
    begin
      TADODataSet(FSourceSet).Close;
      FreeAndNil(FSourceSet);
    end;
end;

procedure TDeADOXMLDataset.Prepare;
begin
  { Ничего не делаем }
end;

function TDeADOXMLDataset.FetchAll: Boolean;
begin
  Result := True;
end;

function TDeADOXMLDataset.Locate(const KeyFields: string; const KeyValues: Variant; const Options: TLocateOptions): Boolean;
begin
  Result := TADODataSet(FSourceSet).Locate(KeyFields, KeyValues, Options);
end;

procedure TDeADOXMLDataset.RetrieveFields(Fields: TObjectList; TableMeta: TObject);
var
  Count, Index, FieldIndex, Position: Integer;
  FieldMeta: TFieldMeta;
begin
  Count := TADODataSet(FSourceSet).Fields.Count;
  for Index :=0 to Pred(Count) do
    begin
      FieldIndex := -1;
      FieldMeta := TFieldMeta.Create;
      try
        FieldMeta.Owner := TTableMeta(TableMeta);
        FieldMeta.Original := TADODataSet(FSourceSet).Fields[Index].FieldName;
        FieldMeta.DataType := TADODataSet(FSourceSet).Fields[Index].DataType;
        FieldMeta.DataSize := TADODataSet(FSourceSet).Fields[Index].Size;
        FieldMeta.NotNull := TADODataSet(FSourceSet).Fields[Index].Required;
        FieldMeta.Order := Succ(Index);
        Position := Pos('_', FieldMeta.Original);
        if Position = 0 then
          FieldMeta.Name := FieldMeta.Original
        else
          FieldMeta.Name := Copy(FieldMeta.Original, Position, MaxInt);
        FieldMeta.ShowType := stNative;
        FieldMeta.Width := 1000 div Succ(Count);
        FieldIndex := Fields.Add(FieldMeta);
      finally
        if FieldIndex = -1 then FieldMeta.Free;
      end;
  end;
end;

procedure TDeADOXMLDataset.CreateFields(Fields: TObjectList; const Stage: TFieldStage);
//var
//  Index: Integer;
begin
//  TADODataSet(FTable).Fields.Clear;
//  TADODataSet(FTable).FieldDefs.Clear;
  // Для XML берём все столбцы!!!
  {
  for Index := 0 to Pred(Fields.Count) do
    if not (TFieldMeta(Fields[Index]).IsLookup or (not TFieldMeta(Fields[Index]).IsStored)) then
      begin
        TADODataSet(FTable).FieldDefs.Add(TFieldMeta(Fields[Index]).Original,
          TFieldMeta(Fields[Index]).DataType, TFieldMeta(Fields[Index]).DataSize,
          TFieldMeta(Fields[Index]).NotNull);
      end;
  }
end;

function TDeADOXMLDataset.IndexByName(const Name: string): Integer;
var
  Field: TField;
begin
  Result := -1;
  if Descr.FieldCount = 0 then
    begin
      Field := TADODataSet(FSourceSet).Fields.FindField(Name);
      if Assigned(Field) then
        Result := TADODataSet(FSourceSet).Fields.IndexOf(Field);
    end
  else
    Result := Descr.IndexByName(Name); // Маппим поле по запросу!!!
end;

function TDeADOXMLDataset.InsertRecord(ARecord: TObject): Boolean;
var
  Index: Integer;
  Value: Variant;
  Field: TField;
begin
  Result := False;

  try
    Initialize;
    DoOpen;
    TADODataSet(FSourceSet).Append;
    try
      for Index := 0 to Pred(TCacheItem(ARecord).Owner.FieldCount) do
        begin
          Field := TADODataSet(FSourceSet).Fields.FindField(TCacheItem(ARecord).Owner.Fields[Index].Original);
          if Assigned(Field) then
            begin
              Value := TCacheItem(ARecord).FieldValue[Index];
              if VarIsEmpty(Value) then
                Field.Value := Null
              else
                Field.Value := Value;
            end;
        end;
      TADODataSet(FSourceSet).Post;
    except
      TADODataSet(FSourceSet).Cancel;
      raise;
    end;
    TADODataSet(FSourceSet).SaveToFile(FFileName, pfXML);
    Result := True;
  except
    on E: Exception do
      Funcs.WriteLog('TDeADOXMLDataset.InsertRecord error: ' + E.Message);
  end;
end;

function TDeADOXMLDataset.Internal_Count: Integer;
begin
  Result:= TADODataSet(FSourceSet).RecordCount;
end;

function TDeADOXMLDataset.Internal_Goto(const aIndex: integer): Boolean;
begin
  FInternalIndex:= aIndex;
  TADODataSet(FSourceSet).RecNo:= Succ(FInternalIndex);
end;

function TDeADOXMLDataset.UpdateRecord(ARecord: TObject): Boolean;
var
  Index: Integer;
  Field: TField;
begin
  Result := False;

  try
    Initialize;
    DoOpen;
    if TADODataSet(FSourceSet).Locate(TCacheItem(ARecord).Owner.TableMeta.KField[0].Original, TCacheItem(ARecord).ID, []) then
      begin
        TADODataSet(FSourceSet).Edit;
        try
          for Index := 0 to Pred(TCacheItem(ARecord).Owner.FieldCount) do
            begin
              Field := TADODataSet(FSourceSet).Fields.FindField(TCacheItem(ARecord).Owner.Fields[Index].Original);
              if Assigned(Field) then
                Field.Value := TCacheItem(ARecord).FieldValue[Index];
            end;
          TADODataSet(FSourceSet).Post;
        except
          TADODataSet(FSourceSet).Cancel;
          raise;
        end;
        TADODataSet(FSourceSet).SaveToFile(FFileName, pfXML);
        Result := True;
      end;
  except
    on E: Exception do
      Funcs.WriteLog('TDeADOXMLDataset.UpdateRecord error: ' + E.Message);
  end;
end;

function TDeADOXMLDataset.DeleteRecord(ARecord: TObject): Boolean;
begin
  Result := False;

  try
    Initialize;
    DoOpen;
    if TADODataSet(FSourceSet).Locate(TCacheItem(ARecord).Owner.TableMeta.KField[0].Original, TCacheItem(ARecord).ID, []) then
      begin
        TADODataSet(FSourceSet).Delete;
        TADODataSet(FSourceSet).SaveToFile(FFileName, pfXML);
        Result := True;
      end;
  except
    on E: Exception do
      Funcs.WriteLog('TDeADOXMLDataset.DeleteRecord error: ' + E.Message);
  end;
end;

function TDeADOXMLDataset.GetMaxKeyValue(const MetaID: Integer): Variant;
var
  TableMeta: TTableMeta;
  FieldName: string;
  Index: Integer;
  KeyValue, MaxKey: Variant;

begin
  if MetaID <> -1 then
    TableMeta := MetaData.GetTableMeta(MetaID)
  else
    TableMeta := MetaData.GetTableByName(Descr.Table, Database);
  FieldName := TableMeta.KField[0].Original;
  Open;
  if TableMeta.KField[0].DataType in NumericTypes then
    MaxKey := 0
  else
    MaxKey := EmptyStr;
  for Index := 0 to Pred(TADODataSet(FSourceSet).RecordCount) do
    begin
      KeyValue := TADODataSet(FSourceSet).FieldValues[FieldName];

      try
        if TableMeta.KField[0].DataType in NumericTypes then
          MaxKey := Max(VarAsType(KeyValue, varDouble), VarAsType(MaxKey, varDouble))
        else if CompareStr(VarToStr(KeyValue), VarToStr(MaxKey)) > 0 then
          MaxKey := VarToStr(KeyValue);
      except
        {$IFDEF DEBUG}
        on E: Exception do
          DebugLog('TDeADOXMLDataset.GetMaxKeyValue error: ' + E.Message);
        {$ENDIF}
      end;
      TADODataSet(FSourceSet).Next;
    end;
  Result := MaxKey;
end;

(*
procedure TDeADOXMLDataset.FilterRecord(DataSet: TDataSet; var Accept: Boolean);
var
  Calculator: TDeCalculator;
begin
  if Descr.Filter.Count = 0 then
    Accept := True
  else
    begin
      Calculator := TDeCalculator.Create;
      try
        Calculator.OnGetIdentValue := GetIdentValue;
        Accept := Calculator.Calculate(Descr.Filter, DataSet);
      finally
        Calculator.Free;
      end;
      {$IFDEF DEBUG}
      //DebugLog('TDeADOXMLDataset.FilterRecord({%s}, %s) ...', [ExtractFileName(FFileName), BoolToStr(Accept)]);
      {$ENDIF}
    end;
end;
*)

{ TDBCOXMLDataset }

constructor TDBCOXMLDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSourceSet := nil;
end;

destructor TDBCOXMLDataset.Destroy;
begin
  inherited Destroy;
end;

procedure TDBCOXMLDataset.CreateFields(aFields: TObjectList; const Stage: TFieldStage);
begin
  // Ничего не делаем поля и так все есть
end;

function TDBCOXMLDataset.GetActive: Boolean;
begin
  Result := Database.Connected and Assigned(FSourceSet);
end;

function TDBCOXMLDataset.IndexByName(const aName: string): Integer;
var i: Integer;
begin
  if Not Assigned(FSourceSet) then Exit(-1);
  if Not Assigned(TXMLTable(FSourceSet).TableMeta) then Exit(-1);

  Result:= -1;
  for i:= Low(Descr.Fields) to High(Descr.Fields) do
    if SameText(Descr.Fields[i].TargetName, aName) then Exit(i);
end;

function TDBCOXMLDataset.GetValue(const Index: Integer): Variant;
begin
  Result:= inherited GetValue(Index);
  if not VarIsEmpty(Result) then Exit;

  if not TXMLTable(FSourceSet).Item_GetFieldValue(FCurrentNode, Descr.Fields[Index].SourceIndex, Result) then
    Result:= null;
end;

procedure TDBCOXMLDataset.SetRecNo(const Value: Integer);
begin
  FCurrentIndex:= Value;
  if Internal_Goto(IND_External2Internal(Value)) then FCurrentNode:= FInternalNode
                                                 else FCurrentNode:= nil;
end;

function TDBCOXMLDataset.Internal_Count: Integer;
begin
  Result:= TXMLTable(FSourceSet).Count;
end;

function TDBCOXMLDataset.Internal_Goto(const aIndex: integer): Boolean;
begin
  Result:= True;
  FInternalIndex:= aIndex;
  if (0 <= aIndex) and (aIndex < TXMLTable(FSourceSet).Count) then FInternalNode:= TXMLTable(FSourceSet).Items[aIndex]
                                                              else FInternalNode:= nil;
end;

function TDBCOXMLDataset.Internal_GetIdentValue(const aIdent: string; var aValue: Variant; aDataObject: pointer): Boolean;
var N: Integer;
begin
  N:= TXMLTable(FSourceSet).TableMeta.Fields.IndexByName(aIdent);
  Result:= (-1 < N);
  if Result then Result:= TXMLTable(FSourceSet).Item_GetFieldValue(FInternalNode, N, aValue)
            else aValue:= Null;
end;

function TDBCOXMLDataset.Initialize: Boolean;
begin
  Result:= inherited Initialize;
  if Result then Result:= assigned(FDatabase);
  if Result then Result:= FDatabase.Connected;
end;

procedure TDBCOXMLDataset.DoOpen;
var i,j: Integer;
begin
  if not assigned(FDatabase) then raise Exception.Create('XMLDatabase not Exists');
  if not FDatabase.Connected then raise Exception.Create('XMLDatabase not Connected');

  FModified := False;
  FSourceSet:= TDBCOXMLDatabase(FDatabase).Get_XMLTable(Descr.Table);

  if Descr.FieldCount = 0 then
    begin
      // Наполняем массив полей, чтобы  1) возвращать значения по SourceIndex 2) получить поля в запросах "select * from..."
      for i:=0 to Pred(TXMLTable(FSourceSet).TableMeta.Fields.Count) do
        with TXMLTable(FSourceSet).TableMeta.Fields[i] do
          Descr.AddSourceField(i, Original, DataType, DataSize);
    end else
    begin
      // Устанавливаем SourceIndex - по нему возвращать значения
      for i:= Low(Descr.Fields) to High(Descr.Fields) do
        for j:= 0 to Pred(TXMLTable(FSourceSet).TableMeta.Fields.Count) do
          if SameText(Descr.Fields[i].TargetName, TXMLTable(FSourceSet).TableMeta.Fields[j].Original) then
            begin
              Descr.Fields[i].SourceIndex:= j;
              Break;
            end;
    end;

  IND_Build;

  if 0 < RecordCount then RecNo:=  0
                     else RecNo:= -1;
end;

procedure TDBCOXMLDataset.DoClose;
begin
  IND_Clear;
  inherited;
end;

procedure TDBCOXMLDataset.Prepare;
begin
  { Ничего не делаем }
end;

procedure TDBCOXMLDataset.SetValue(const Index: Integer; const Value: Variant);
begin
//  TDataSet(FSourceSet).Fields[Index].Value := Value;
end;

function TDBCOXMLDataset.FetchAll: Boolean;
begin
  Result:= True;
end;

function TDBCOXMLDataset.Locate(const KeyFields: string; const KeyValues: Variant; const Options: TLocateOptions): Boolean;
begin
  Result:= inherited Locate(KeyFields, KeyValues, Options);
end;

function TDBCOXMLDataset.InsertRecord(ARecord: TObject): Boolean;
var
  Index: Integer;
  Value: Variant;
  Field: TField;
begin
  Result := False;
  (*
  try
    Initialize;
    DoOpen;
    TADODataSet(FDataSet).Append;
    try
      for Index := 0 to Pred(TCacheItem(ARecord).Owner.FieldCount) do
        begin
          Field := FDataSet.Fields.FindField(TCacheItem(ARecord).Owner.Fields[Index].Original);
          if Assigned(Field) then
            begin
              Value := TCacheItem(ARecord).FieldValue[Index];
              if VarIsEmpty(Value) then
                Field.Value := Null
              else
                Field.Value := Value;
            end;
        end;
      TADODataSet(FDataSet).Post;
    except
      TADODataSet(FDataSet).Cancel;
      raise;
    end;
    TADODataSet(FDataSet).SaveToFile(FFileName, pfXML);
    TDeXMLDatabase(FXMLDatabase).ReloadFile(FFileName);
    Result := True;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('TDeXMLDataset.InsertRecord error: ' + E.Message);
    {$ENDIF}
  end;
  (* *)
end;

function TDBCOXMLDataset.UpdateRecord(ARecord: TObject): Boolean;
var
  Index: Integer;
  Field: TField;
begin
  Result := False;
  (*
  try
    Initialize;
    DoOpen;
    if TADODataSet(FDataSet).Locate(TCacheItem(ARecord).Owner.TableMeta.KField[0].Original, TCacheItem(ARecord).ID, []) then
      begin
        TADODataSet(FDataSet).Edit;
        try
          for Index := 0 to Pred(TCacheItem(ARecord).Owner.FieldCount) do
            begin
              Field := FDataSet.Fields.FindField(TCacheItem(ARecord).Owner.Fields[Index].Original);
              if Assigned(Field) then
                Field.Value := TCacheItem(ARecord).FieldValue[Index];
            end;
          TADODataSet(FDataSet).Post;
        except
          TADODataSet(FDataSet).Cancel;
          raise;
        end;
        TADODataSet(FDataSet).SaveToFile(FFileName, pfXML);
        TDeXMLDatabase(FXMLDatabase).ReloadFile(FFileName);
        Result := True;
      end;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('TDeXMLDataset.UpdateRecord error: ' + E.Message);
    {$ENDIF}
  end;
  (* *)
end;

function TDBCOXMLDataset.DeleteRecord(ARecord: TObject): Boolean;
begin
  Result := False;
  (*
  try
    Initialize;
    DoOpen;
    if TADODataSet(FDataSet).Locate(TCacheItem(ARecord).Owner.TableMeta.KField[0].Original, TCacheItem(ARecord).ID, []) then
      begin
        TADODataSet(FDataSet).Delete;
        TADODataSet(FDataSet).SaveToFile(FFileName, pfXML);
        TDeXMLDatabase(FXMLDatabase).ReloadFile(FFileName);
        Result := True;
      end;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('TDeXMLDataset.DeleteRecord error: ' + E.Message);
    {$ENDIF}
  end;
  (* *)
end;

function TDBCOXMLDataset.GetMaxKeyValue(const MetaID: Integer): Variant;
var
  TableMeta: TTableMeta;
  FieldName: string;
  Index: Integer;
  KeyValue, MaxKey: Variant;
  FNode: IXMLNode;
begin
  if MetaID <> -1 then
    TableMeta := MetaData.GetTableMeta(MetaID)
  else
    TableMeta := MetaData.GetTableByName(Descr.Table, Database);
  FieldName := TableMeta.KField[0].Original;
  Open;
  if TableMeta.KField[0].DataType in NumericTypes then
    MaxKey := 0
  else
    MaxKey := EmptyStr;
  // ищем во всей таблице НЕ УЧИТЫВАЯ текущий фильтр
  for Index := 0 to Pred(TXMLTable(FSourceSet).Count) do
    begin
      FNode:= TXMLTable(FSourceSet).Items[Index];
      if TXMLTable(FSourceSet).Item_GetFieldValue(FNode, TXMLTable(FSourceSet).TableMeta.KField[0].Original, KeyValue) then
      try
        if TableMeta.KField[0].DataType in IntegerTypes then
          MaxKey := Max(VarAsType(KeyValue, varInt64), VarAsType(MaxKey, varInt64)) else
        if TableMeta.KField[0].DataType in NumericTypes then
          MaxKey := Max(VarAsType(KeyValue, varDouble), VarAsType(MaxKey, varDouble)) else
        if CompareStr(VarToStr(KeyValue), VarToStr(MaxKey)) > 0 then
          MaxKey := VarToStr(KeyValue);
      except
        {$IFDEF DEBUG}
        on E: Exception do
          DebugLog('TDeXMLDataset.GetMaxKeyValue error: ' + E.Message);
        {$ENDIF}
      end;
    end;
  Result := MaxKey;
end;

initialization
{$IFDEF DEBUG}
  DebugLog('DeDataset unit initialization ...');
{$ENDIF}

finalization
{$IFDEF DEBUG}
  DebugLog('DeDataset unit finalization ...');
{$ENDIF}

end.

