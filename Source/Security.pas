unit Security;

interface

uses
  Windows, SysUtils, Classes, Contnrs, DataCacheUnit,
  DeTypes, DeParser, DeTemplate{, DataManager};

type
  /// <summary>Квант безопасности</summary>
  TQuantum = class
  private
    FUserID     : Integer;
    FObjType    : Integer;
    FObjID      : Integer;
    FOperation  : TSecurityOperation;
    FRestrict   : TObjectRestrict;
  public
    procedure Assign(Source : TObject);
    property UserID     : Integer Read FUserID;
    property ObjectType : Integer Read FObjType;
    property ObjectID   : Integer Read FObjID;
    property Operation  : TSecurityOperation Read FOperation;
    property Restrict   : TObjectRestrict Read FRestrict;
  end;

  /// <summary>Список квантов безопасности</summary>
  TQuantumList = class(TObjectList)
  private
    FObjectType: Integer;
    FObjectID: Integer;
    function GetQuantum(const Index: Integer): TQuantum;
    procedure SetQuantum(const Index: Integer; const aQuantum: TQuantum);
    procedure DeleteObjectData(const aObjectType, aObjectID, aSubjectID: Integer);
    procedure MergeRecordFromNode(Node: TDeNode; const DatasetID, RecordID: Integer);
    procedure MergeDatasetFromNode(Node: TDeNode; const DatasetID: Integer);
    procedure MergeFromNode(Node: TDeNode);
  public
    procedure New(const aUserID, aObjType, aObjID: Integer; const aOperation: TSecurityOperation; const aRestrict: TObjectRestrict);
    procedure LoadObjectData(const aObjectType, aObjectID: Integer);
    procedure SaveObjectData(const aObjectType, aObjectID, aSubjectID: Integer);
    function PrepareClipboardXML(const Level: Integer = -1; const ObjectID: Integer = 0): string;
    /// <summary>Метод вставки квантов безопасности из буфера обмена</summary>
    /// <remarks>Если в буфере обмена нет данных, то существующий список не изменится. А если есть - полностью заменится на список из буфера обмена.</remarks>
    procedure PasteFromClipboard;
    /// <summary>Метод замены идентификатора объекта в квантах безопасности со старого на новый</summary>
    /// <param name="ObjectTypeID">идентификатор типа объекта (M_RIGHTS.MRH_OBJECTTYPE в метаструктуре)</param>
    /// <param name="OldObjectID">старый идентификатор объекта (M_RIGHTS.MRH_OBJECTID в метаструктуре)</param>
    /// <param name="NewObjectID">новый идентификатор объекта (M_RIGHTS.MRH_OBJECTID в метаструктуре)</param>
    /// <param name="NotEqualDeleted">флаг удаления при несовпадении условий:
    /// <para>True - удалять другие типы объектов и объекты с другими идентификаторами (по умолчанию),</para>
    /// <para>False - ничего не удалять из списка квантов безопасности.</para>
    /// </param>
    /// <remarks>Используется при вставке из буфера обмена нового объекта.</remarks>
    procedure ModifyObjectData(const ObjectTypeID, OldObjectID, NewObjectID: Integer; const NotEqualDeleted: Boolean = True);
    /// <summary>Метод сохранения загруженных ранее квантов безопасности</summary>
    /// <remarks>ObjectType и ObjectID берётся из кванта безопасности, а не как в методе <see cref="SaveObjectData">SaveObjectData</see> из параметров.</remarks>
    procedure SaveBatchObjectData;

    property ObjectType: Integer read FObjectType;
    property ObjectID: Integer read FObjectID;
    property Items[const Index: Integer]: TQuantum read GetQuantum write SetQuantum; default;
  end;

  TUserList = class;

  /// <summary>данные о пользователе (в том числе вошедший в систему)</summary>
  TUserSession = class
  private
    FID         : integer;
    FLogin      : string;
    FName       : string;
    FWorkerID   : Variant;
    FIsAdmin    : boolean;
    FIsDisabled : boolean;
    FIsDeleted  : boolean;
    FIsGroup    : boolean;
    FQuantumList : TQuantumList;
    FGroupList  : TUserList;
    FVariant    : Variant;
    FLastReport : Variant;
    FWorkerCache: TDataCache;
    function GetWorkerItem: TCacheItem;
  public
    constructor Create(const aLogin, aPassword : string); overload;
    constructor Create; reintroduce; overload;
    destructor Destroy; override;
    procedure Assign(Source: TObject);
    procedure LoadGroup;
    function ChangePassword(const Password: string): Boolean;
    property ID: Integer read FID write FID;
    property Login : string read FLogin;
    property Name : string read FName;
    property WorkerID : Variant read FWorkerID;
    property IsAdmin : boolean read FIsAdmin;
    property Disabled : Boolean read FIsDisabled;
    property Deleted : Boolean read FIsDeleted;
    property isGroup: Boolean read FIsGroup;
    property List: TQuantumList read FQuantumList;
    property Groups: TUserList read FGroupList;
    property VTag: Variant read FVariant write FVariant;
    property LastReport: Variant read FLastReport write FLastReport;
    property WorkerItem: TCacheItem read getWorkerItem;
  end;

  /// <summary>Список пользователей</summary>
  TUserList = class(TObjectList)
  private
    function GetUserSession(const Index: Integer): TUserSession;
    procedure SetUserSession(const Index: Integer; const User: TUserSession);
  public
    procedure Init(const DefaultGroup: Boolean = False);
    procedure LoadFromDatabase;
    property Items[const Index: Integer]: TUserSession read GetUserSession write SetUserSession; default;
    function FindUser(const aUserSession : string) : TUserSession;
    function FindByID(const aID: integer): TUserSession;
    function IndexByLogin(const Login: string): Integer;
  end;

  /// <summary>данные объекта безопасности для отдельной операции над объектом</summary>
  TSecurityItem = class
  private
    FRestrict : TObjectRestrict;
    FFilter   : TExpressionItem;
    function UnionRestricts(const aRestrict1, aRestrict2: TObjectRestrict): TObjectRestrict;
    procedure Union(aUnionWith : TSecurityItem);
  public
    constructor Create;
    destructor Destroy;  override;
    property Restrict : TObjectRestrict read FRestrict write FRestrict;
    property Filter : TExpressionItem read FFilter;
    function CreateCopy : TSecurityItem;
  end;

  /// <summary>политика безопасности объекта</summary>
  TObjectPolicy = class
  private
    FObjectType : Integer;
    FObjectID   : string;
    FItems      : array [TSecurityOperation] of TSecurityItem;
    procedure DestroyItem(aOperation : TSecurityOperation);
    function GetItem(aOperation : TSecurityOperation) : TSecurityItem;
  public
    constructor Create(const AObjectType: Integer; const AObjectID: string);
    destructor Destroy; override;
    property ObjectType : Integer read FObjectType;
    property ObjectID : string read FObjectID;
    property Items[aOperation : TSecurityOperation] : TSecurityItem read GetItem;  default;
    procedure Clear;
    procedure PutItem(const aOperation: TSecurityOperation; const aRestrict: TObjectRestrict; const aFilter: string);
    function CreateCopy : TObjectPolicy;
    procedure Union(aUnionWith : TObjectPolicy);
  end;

  /// <summary>политика безопасности для отдельного пользователя</summary>
  TUserPolicy = class(TObjectList)
  private
    FSorted : boolean;
    function Get(const Index: Integer): TObjectPolicy;
    procedure Put(const Index: Integer; const aItem: TObjectPolicy);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    property Items[const Index: Integer]: TObjectPolicy read Get write Put; default;
    function FindPolicy(const aObjectType: Integer; const aObjectID: string): TObjectPolicy;
    procedure Merge(aMergeWith : TUserPolicy);
    procedure Union(aUnionWith : TUserPolicy);
    procedure SortPolicy;
    {$IFDEF DEBUG}
    procedure DebugPolicyLog(const Text: string);
    {$ENDIF}
  end;

  /// <summary>управление системой безопасности</summary>
  TSecuritySystem = class
  private
    FPolicy : TUserPolicy;
    procedure DestroyPolicy;
    procedure LoadPrincipalPolicy(const aPrincipalID: Integer; aPolicy: TUserPolicy);
    procedure GetPrincipalPolicy(const aPrincipalID: Integer; aPolicy: TUserPolicy);
    procedure BuildSubjectList(const aSubjectID: Integer; aLoadTo: TList);
  public
    destructor Destroy;  override;
    /// <summary>загрузка политики безопасности данного пользователя</summary>
    function LoadPolicy : boolean;
    procedure ClearPolicy;
    /// <summary>операции установки прав на объекты</summary>
    procedure SetObjectPolicy(
      const aObjectType : Integer;
      const aObjectID : Variant;
      const aOperation : TSecurityOperation;
      const aRestrict : TObjectRestrict);
    /// <summary>Функция получения прав на объекты</summary>
    function GetObjectPolicy(const ObjectType: Integer; const ObjectID: Variant; const Operation: TSecurityOperation): TObjectRestrict;
    /// <summary>операции проверки прав на объекты</summary>
    function CheckPolicy( const aObjectType : Integer;
                          const aObjectID : Variant; const aOperation : TSecurityOperation) : boolean;
    function CheckPolicyDataset( const aObjectID : integer; const aOperation : TSecurityOperation) : boolean;
    function CheckPolicyField(const AObjectID: Integer; const AOperation: TSecurityDataset): Boolean;
    function CheckPolicyMenu( const aObjectID : integer; const aOperation : TSecurityOperation) : boolean;
    //function CheckPolicyReport( const aObjectID : integer; const aOperation : TSecurityOperation) : boolean;
    //function CheckPolicyAction( const aObjectID : integer; const aOperation : TSecurityOperation) : boolean;
    function CheckPolicyCommand(const ObjectID: Integer; const Operation: TSecurityOperation): Boolean;

    function GetPolicyDetails(
      const aObjectType : Integer;
      const aObjectID : Variant;
      const aOperation : TSecurityOperation) : TSecurityItem;
    // v. 18.03
    function CheckPolicyConstraint(const ObjectID: Integer; const Operation: TSecurityOperation): Boolean;
  end;

  ESecurityError = class(Exception);
  ESecurityDisabledError = class(ESecurityError);

var
  UserSession    : TUserSession;
  SecuritySystem : TSecuritySystem;

function IsStringMD5(const Text: string): Boolean;

implementation

uses Variants, DB, ClipBrd, StrUtils, DCPmd5,
     DeLog, Funcs, DeMeta, Dictionary, DeDB, DeMetaData, QueryDescriptor, DeScript;

function IsStringMD5(const Text: string): Boolean;
const
  HexChars = '0123456789abcdef';
var
  Index: Integer;
begin
  Result := Length(Text) = 32;
  for Index := 1 to Length(Text) do
    if Pos(Text[Index], HexChars) = 0 then
      begin
        Result := False;
        Break;
      end;
end;

{ TQuantum }

procedure TQuantum.Assign(Source : TObject);
var V: Variant;
begin
  if Source is TDeDataSet then
    with TDeDataSet(Source) do
      begin
        FUserID := ValueByName[fldRightsSubjectId];
        FObjType:= ValueByName[fldRightsObjectType];
        FObjID  := ValueByName[fldRightsObjectId];

        V:=ValueByName[fldRightsOperation];
        if TSecurityOperation(V) in [spNone, spAll, spSelect, spInsert, spUpdate, spDelete, spExecute ] then
          FOperation:= TSecurityOperation(V);

        V:=ValueByName[fldRightsPolicy];
        if TObjectRestrict(V) in [orNone, orEnabled, orDisabled] then
          FRestrict:= TObjectRestrict(V);
      end;

  if Source is TQuantum then
      begin
        FUserID := TQuantum(Source).UserID;
        FObjType:= TQuantum(Source).ObjectType;
        FObjID  := TQuantum(Source).FObjID;
        FOperation := TQuantum(Source).Operation;
        FRestrict  := TQuantum(Source).Restrict;
      end;
end;

{TQuantumList}

function TQuantumList.GetQuantum(const Index: Integer): TQuantum;
begin
  Result := TQuantum(inherited Items[Index]);
end;

procedure TQuantumList.SetQuantum(const Index: integer; const aQuantum: TQuantum);
begin
  if Index > Count then Count := Succ(Index);
  inherited Items[Index] := aQuantum;
end;

procedure TQuantumList.New(const aUserID, aObjType, aObjID: Integer; const aOperation: TSecurityOperation; const aRestrict: TObjectRestrict);
var N : Integer;
begin
  N:=Add(TQuantum.Create);
  Items[N].FUserID   := aUserID;
  Items[N].FObjType  := aObjType;
  Items[N].FObjID    := aObjID;
  Items[N].FOperation:= aOperation;
  Items[N].FRestrict := aRestrict;
end;

procedure TQuantumList.LoadObjectData(const aObjectType, aObjectID: Integer);
var R: Integer;
    Q: TDeDataset;
begin
  FObjectType:= aObjectType;
  FObjectID := aObjectID;

  Q := MetaData.MetadataDB.CreateQuery(qtSelect);
  try
    Q.Descr.BeginUpdate;
    try
      Q.Descr.Table := tblRights;

      Q.Descr.AddParamCondition(fldRightsObjectType, ftInteger, opEQ, 'TID', Integer(FObjectType));
      if aObjectID <> 0 then
        begin
          Q.Descr.AddParamCondition(fldRightsObjectId,   ftInteger, opEQ, 'ID', FObjectID);
          Q.Descr.AddOperation(opAnd);
        end;
      // 27.08.2015 + Куфенко: Читаем только необходимые поля!
      Q.Descr.AddFields([fldRightsSubjectId, fldRightsObjectType, fldRightsObjectId, fldRightsOperation, fldRightsPolicy]);
      // 27.08.2015 -
    finally
      Q.Descr.EndUpdate;
    end;
    Q.Open;

    for R:=0 to Pred(Q.RecordCount) do
      begin
        Q.RecNo:= R;
        Add(TQuantum.Create);
        Items[Count-1].Assign(Q);
      end;
  finally
    Q.Free;
  end;
end;

procedure TQuantumList.DeleteObjectData(const aObjectType, aObjectID, aSubjectID: Integer);
var
  DataSet: TDeDataset;
begin
  DataSet := MetaData.MetadataDB.CreateQuery(qtDelete);
  try
    DataSet.Descr.BeginUpdate;
    try
      DataSet.Descr.Table := tblRights;
      DataSet.Descr.AddParamCondition(fldRightsObjectType, ftInteger, opEQ, 'TID', aObjectType);
      DataSet.Descr.AddParamCondition(fldRightsObjectID,   ftInteger, opEQ, 'ID', aObjectID);
      if aSubjectID = 0 then
        DataSet.Descr.AddCondition(fldRightsSubjectID, ftInteger, opIs, Null)
      else
        DataSet.Descr.AddParamCondition(fldRightsSubjectID,  ftInteger, opEQ, 'SID', aSubjectID);
      DataSet.Descr.AddOperation(opAnd);
      DataSet.Descr.AddOperation(opAnd);
    finally
      DataSet.Descr.EndUpdate;
    end;
    try
      DataSet.ExecuteQuery;
    except
      {$IFDEF DEBUG}
      on E: Exception do
        DebugLog('%s.DeleteObjectData(%d, %d, %d) error: %s', [ClassName, aObjectType, aObjectID, aSubjectID, E.Message]);
      {$ENDIF}
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TQuantumList.SaveObjectData(const aObjectType, aObjectID, aSubjectID: Integer);
var
  DataSet, MaxDataSet: TDeDataset;
  Index: Integer;
begin
  // Удаляем старые записи ...
  DeleteObjectData(aObjectType, aObjectID, aSubjectID);
  // Добавляем новые ...
  DataSet := MetaData.MetadataDB.CreateQuery(qtInsert);
  try
    DataSet.Descr.BeginUpdate;
    try
      DataSet.Descr.Table := tblRights;
      DataSet.Descr.AddParamField(fldRightsID, ftInteger);
      DataSet.Descr.AddParamField(fldRightsObjectType, ftInteger);
      DataSet.Descr.AddParamField(fldRightsObjectID, ftInteger);
      if aSubjectID <> 0 then
        DataSet.Descr.AddParamField(fldRightsSubjectID, ftInteger);
      DataSet.Descr.AddParamField(fldRightsOperation, ftInteger);
      DataSet.Descr.AddParamField(fldRightsPolicy, ftInteger);
    finally
      DataSet.Descr.EndUpdate;
    end;
    MaxDataset := MetaData.MetadataDB.CreateQuery(qtHole);
    try
      if not Assigned(MaxDataset) then MaxDataset := MetaData.MetadataDB.CreateQuery(qtSelect);
      MaxDataset.Descr.BeginUpdate;
      try
        MaxDataset.Descr.Table := tblRights;
        MaxDataset.Descr.AddField(opMax, fldRightsID);
      finally
        MaxDataset.Descr.EndUpdate;
      end;
      DataSet.Descr.ParamValueByName[fldRightsObjectType] := aObjectType;
      DataSet.Descr.ParamValueByName[fldRightsObjectID] := aObjectID;
      if aSubjectID <> 0 then
        DataSet.Descr.ParamValueByName[fldRightsSubjectID] := aSubjectID;
      for Index := 0 to Pred(Count) do
        begin
          DataSet.Descr.ParamValueByName[fldRightsOperation] := Ord(Items[Index].Operation);
          DataSet.Descr.ParamValueByName[fldRightsPolicy] := Ord(Items[Index].Restrict);
          MaxDataset.Open;
          try
            DataSet.Descr.ParamValueByName[fldRightsID] := VarToInt(MaxDataset.Value[0]) + 1;
          finally
            MaxDataset.Close;
          end;
          DataSet.ExecuteQuery;
        end;
    finally
      MaxDataSet.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TQuantumList.PasteFromClipboard;
var
  Handle: THandle;
  DataPtr: PAnsiChar;
  Template: TDeTemplate;
begin
  if Clipboard.HasFormat(DeXML) then
    begin
      Template := TDeTemplate.Create;
      try
        Handle := Clipboard.GetAsHandle(DeXML);
        DataPtr := GlobalLock(Handle);
        try
          Template.Text := StrPas(DataPtr);
          if Assigned(Template.Root) then
            begin
              Clear;
              MergeFromNode(Template.Root);
            end;
        finally
          GlobalUnlock(Handle);
        end;
      finally
        Template.Free;
      end;
    end;
end;

function TQuantumList.PrepareClipboardXML(const Level: Integer; const ObjectID: Integer): string;
var
  Index: Integer;
  SpaceString, LineString: string;
begin
  Result := EmptyStr;
  if Level >= 0 then
    begin
      if Level = 0 then
        SpaceString := EmptyStr
      else
        SpaceString := DupeString('  ', Level);
      LineString := #13#10;
    end
  else
    begin
      SpaceString := EmptyStr;
      LineString := EmptyStr;
    end;
  for Index := 0 to Pred(Count) do
    if (Items[Index].Restrict <> orNone) and ((ObjectID = 0) or (Items[Index].ObjectID = ObjectID)) then
      begin
        Result := Result + LineString + SpaceString + '<security';
        if ObjectID = 0 then
          Result := Result + Format(' id="%d"', [Items[Index].ObjectID]);
        if Items[Index].UserID <> 0 then
          Result := Result + Format(' user="%d"', [Items[Index].UserID]);
        Result := Result + Format(' operation="%d" restrict="%d" />', [ Ord(Items[Index].Operation), Ord(Items[Index].Restrict) ]);
      end;
end;

procedure TQuantumList.ModifyObjectData(const ObjectTypeID, OldObjectID, NewObjectID: Integer; const NotEqualDeleted: Boolean);
var
  Index: Integer;
  Deleted: Boolean;
begin
  for Index := Pred(Count) downto 0 do
    begin
      Deleted := True;
      if Items[Index].ObjectType = ObjectTypeID then
        if Items[Index].ObjectID = OldObjectID then
          begin
            Items[Index].FObjID := NewObjectID;
            Deleted := False;
          end;
      if Deleted and NotEqualDeleted then
        Delete(Index);
    end;
end;

procedure TQuantumList.SaveBatchObjectData;
var
  DataSet, MaxDataSet: TDeDataset;
  Strings: TStrings;
  Index, ObjectTypeID, ObjectID, UserID: Integer;
  Value: string;
begin
  DataSet := MetaData.MetadataDB.CreateQuery(qtInsert);
  try
    DataSet.Descr.BeginUpdate;
    try
      DataSet.Descr.Table := tblRights;
      DataSet.Descr.AddParamField(fldRightsID, ftInteger);
      DataSet.Descr.AddParamField(fldRightsObjectType, ftInteger);
      DataSet.Descr.AddParamField(fldRightsObjectID, ftInteger);
      DataSet.Descr.AddParamField(fldRightsSubjectID, ftInteger);
      DataSet.Descr.AddParamField(fldRightsOperation, ftInteger);
      DataSet.Descr.AddParamField(fldRightsPolicy, ftInteger);
    finally
      DataSet.Descr.EndUpdate;
    end;
    MaxDataset := MetaData.MetadataDB.CreateQuery(qtSelect);
    try
      if not Assigned(MaxDataset) then MaxDataset := MetaData.MetadataDB.CreateQuery(qtSelect);
      MaxDataset.Descr.BeginUpdate;
      try
        MaxDataset.Descr.Table := tblRights;
        MaxDataset.Descr.AddField(opMax, fldRightsID);
      finally
        MaxDataset.Descr.EndUpdate;
      end;
      Strings := TStringList.Create;
      try
        for Index := 0 to Pred(Count) do
          begin
            ObjectTypeID := Items[Index].ObjectType;
            ObjectID := Items[Index].ObjectID;
            UserID := Items[Index].UserID;
            Value := Format('%d,%d,%d', [ObjectTypeID, ObjectID, UserID]);
            if Strings.IndexOf(Value) = -1 then
              begin
                DeleteObjectData(ObjectTypeID, ObjectID, UserID);
                // Добавляем в список пропускаемых ...
                Strings.Append(Value);
              end;
            DataSet.Descr.ParamValueByName[fldRightsObjectType] := ObjectTypeID;
            DataSet.Descr.ParamValueByName[fldRightsObjectID] := ObjectID;
            if UserID = 0 then
              DataSet.Descr.ParamValueByName[fldRightsSubjectID] := Null
            else
              DataSet.Descr.ParamValueByName[fldRightsSubjectID] := UserID;
            DataSet.Descr.ParamValueByName[fldRightsOperation] := Ord(Items[Index].Operation);
            DataSet.Descr.ParamValueByName[fldRightsPolicy] := Ord(Items[Index].Restrict);
            MaxDataset.Open;
            try
              DataSet.Descr.ParamValueByName[fldRightsID] := VarToInt(MaxDataset.Value[0]) + 1;
            finally
              MaxDataset.Close;
            end;
            DataSet.ExecuteQuery;
          end;
      finally
        Strings.Free;
      end;
    finally
      MaxDataSet.Free;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TQuantumList.MergeRecordFromNode(Node: TDeNode; const DatasetID, RecordID: Integer);
var
  Index: Integer;
begin
  if Assigned(Node) then
    if SameText(Node.NodeName, 'security') and Node.HasAttribute('operation') and Node.HasAttribute('restrict') then
      New(StrToIntDef(Node.Attributes['user'], 0), DatasetID, RecordID,
        TSecurityOperation(StrToIntDef(Node.Attributes['operation'], 0)),
        TObjectRestrict(StrToIntDef(Node.Attributes['restrict'], 0)))
    else
      if Assigned(Node.ChildNodes) then
        for Index := 0 to Pred(Node.ChildNodes.Count) do
          MergeRecordFromNode(Node.ChildNodes[Index], DatasetID, RecordID);
end;

procedure TQuantumList.MergeDatasetFromNode(Node: TDeNode; const DatasetID: Integer);
var
  Index: Integer;
begin
  if Assigned(Node) then
    begin
      if SameText(Node.NodeName, 'record') and Node.HasAttribute('id') then
        MergeRecordFromNode(Node, DatasetID, StrToInt(Node.Attributes['id']));
      if Assigned(Node.ChildNodes) then
        for Index := 0 to Pred(Node.ChildNodes.Count) do
          MergeDatasetFromNode(Node.ChildNodes[Index], DatasetID);
    end;
end;

procedure TQuantumList.MergeFromNode(Node: TDeNode);
var
  Index: Integer;
begin
  if Assigned(Node) then
    begin
      if SameText(Node.NodeName, 'dataset') and Node.HasAttribute('id') then
        MergeDatasetFromNode(Node, StrToInt(Node.Attributes['id']));
      if Assigned(Node.ChildNodes) then
        for Index := 0 to Pred(Node.ChildNodes.Count) do
          MergeFromNode(Node.ChildNodes[Index]);
    end;
end;

{ TUserSession }

constructor TUserSession.Create(const aLogin, aPassword : string);
var Q: TDeDataset;
    PassOK, Disabled: Boolean;
    TypedPassword, StoredLogin, StoredPassword: string;
    R, LoginCodepage, PasswordCodepage: Integer;
    {$IFDEF DEBUG}
    DebugString: string;
    {$ENDIF}
begin
  FQuantumList:=TQuantumList.Create;
  FGroupList  :=TUserList.Create;
  FVariant := UnAssigned;
  FLastReport := UnAssigned;
  PassOk:=False;

  Q := MetaData.MetadataDB.CreateQuery(qtSelect);
  try
    Q.Descr.BeginUpdate;
    try
      Q.Descr.Table := tblUsers;
      Q.Descr.AddParamCondition(fldUsersDeleted,  ftSmallint, opEQ, 'DID', 0);
      Q.Descr.AddParamCondition(fldUsersType,  ftSmallint, opEQ, 'TID', 1);
      Q.Descr.AddOperation(opAnd);
      // 27.08.2015 + Куфенко: Читаем только необходимые поля!
      Q.Descr.AddFields([fldUsersID, fldUsersType, fldUsersWorkerID, fldUsersLogin, fldUsersPassword, fldUsersName,
        fldUsersAdmin, fldUsersDisabled{, fldUsersDeleted}]);
      // 27.08.2015 -
    finally
      Q.Descr.EndUpdate;
    end;
    Q.Open;
    {$IFDEF DEBUG}
    DebugString := EmptyStr;
    {$ENDIF}
    Disabled := False;
    if Length(aPassword) = 0 then
      TypedPassword := EmptyPasswordHash
    else
      TypedPassword := MD5password(WideStringToUnicode(aPassword));
    for R:= 0  to Pred(Q.RecordCount) do
      begin
        Q.RecNo:= R;
        StoredLogin:= Q.StringValueByName(fldUsersLogin);

        {$IFDEF DEBUG}
        DebugString := DebugString + Format(#13#10'                        | %9d. | %-80s |', [Q.RecNo, StoredLogin]);
        {$ENDIF}
        if AnsiSametext(StoredLogin, aLogin) then
          if Q.ValueByName[fldUsersDisabled] = 1 then
            begin
              Disabled := True;
              {$IFDEF DEBUG}
              DebugString := DebugString + ' + |';
              {$ENDIF}
            end
          else
            begin
              {$IFDEF DEBUG}
              DebugString := DebugString + ' - |';
              {$ENDIF}
              StoredPassword:= Q.StringValueByName(fldUsersPassword);

              if not IsStringMD5(StoredPassword) then
                begin
                  FID := Q.ValueByName[fldUsersID];
                  {$IFDEF DEBUG}
                  Funcs.WriteLog('%s.Create: password not hash! Change to hash for %d ...', [ClassName, FID], True, 'Policy');
                  {$ENDIF}
                  if ChangePassword(StoredPassword) then
                    begin
                      StoredPassword := MD5password(WideStringToUnicode(StoredPassword));
                      {$IFDEF DEBUG}
                      Funcs.WriteLog('%s.Create: password change to hash for %d completed ...', [ClassName, FID], True, 'Policy');
                      {$ENDIF}
                    end;
                end;
              {$IFDEF DEBUG}
              DebugString := DebugString + Format(' %-32s |', [StoredPassword]);
              {$ENDIF}
              PassOk := (TypedPassword = StoredPassword) or (GetWindowsUserName = aLogin);
              Break;
            end
        else
          begin
            {$IFDEF DEBUG}
            StoredPassword := Q.ValueByName[fldUsersPassword];
            DebugString := DebugString + Format('   | %-32s |', [StoredPassword]);
            {$ENDIF}
          end;
      end;
    if PassOK then
      Assign(Q)
    else
      begin
        {$IFDEF DEBUG}
        if Length(DebugString) <> 0 then
          begin
            StoredPassword :=  Format(#13#10'                        +%s+%s+---+%s+',
              [
                 DupeString('-', 12), DupeString('-', 82), DupeString('-', 34)
              ]);
            DebugString := StoredPassword +
              Format(#13#10'                        | %10s | %-80s | D | %-32s |', ['No', 'Login', 'Hash']) +
              StoredPassword + DebugString + StoredPassword;
          end;
         Funcs.WriteLog('%s.Create: login %s and password %s failed ...%s', [ClassName, QuotedStr(aLogin), QuotedStr(TypedPassword), DebugString], True, 'Policy');
        {$ENDIF}
        if Disabled then
          raise ESecurityDisabledError.Create('The user account is disabled')
        else
          raise ESecurityError.Create('Invalid user or password');
      end;
  finally
    Q.Free;
  end;
end;

constructor TUserSession.Create;
begin
  inherited;
  FQuantumList:= TQuantumList.Create;
  FGroupList:= TUserList.Create;
  FVariant := UnAssigned;
  FLastReport := UnAssigned;
end;

destructor TUserSession.Destroy;
begin
  FreeAndNil(FWorkerCache);
  FQuantumList.Free;
  FGroupList.Free;
  inherited Destroy;
end;

function TUserSession.GetWorkerItem: TCacheItem;
var
  TableMeta: TTableMeta;
begin
  if not Assigned(FWorkerCache) then
    begin
      //TableMeta := MetaData.GetTableMeta(StrToIntDef( MetaData.ParamValue['WorkerTable'], -1));
      TableMeta := MetaData.GetTableMeta(MetaData.ParamValue['WorkerTable']);
      if Assigned(TableMeta) then
        begin
          FWorkerCache := TDataCache.Create(TableMeta);
          FWorkerCache.Filters.NewFilter(TableMeta.KField[0], opEQ, WorkerID);
          FWorkerCache.PrepareData(True, fsFull);
        end
      else
        begin
          TableMeta := MetaData.GetTableMeta(idxUsers);
          FWorkerCache := TDataCache.Create(TableMeta);
          FWorkerCache.Filters.NewFilter(TableMeta.KField[0], opEQ, ID);
          FWorkerCache.PrepareData(True, fsFull);
        end;
    end;
  if Assigned(FWorkerCache) and (FWorkerCache.Count = 1) then
    Result := FWorkerCache[0]
  else
    Result := nil;
end;

procedure TUserSession.Assign(Source: TObject);
begin
  if Source is TDeDataSet then
    with TDeDataSet(Source) do
      begin
        FID := ValueByName[fldUsersID];
        FLogin := StringValueByName(fldUsersLogin);
        FName := StringValueByName(fldUsersName);
        FWorkerID := ValueByName[fldUsersWorkerID];
        FIsAdmin := VarToInt(ValueByName[fldUsersAdmin])    = 1;
        FIsDisabled := VarToInt(ValueByName[fldUsersDisabled]) = 1;
        FIsDeleted :=  TDeDataSet(Source).ValueByNameDef(fldUsersDeleted, False);
        FIsGroup := VarToInt(ValueByName[fldUsersType]) = 2;
      end;
end;

procedure TUserSession.LoadGroup;
var Q, G: TDeDataset;
  R, GroupID, GroupIndex: Integer;
begin
  FGroupList.Clear;
  Q := MetaData.MetadataDB.CreateQuery(qtSelect);
  try
    Q.Descr.BeginUpdate;
    try
      Q.Descr.Table := tblMembership;
      Q.Descr.AddParamCondition(fldMSMemberId, ftInteger, opEQ, 'ID', ID);
      // 27.08.2015 + Куфенко: Читаем только необходимые поля!
      Q.Descr.AddField(fldMSGroupId);
      // 27.08.2015 -
      Q.Descr.AddSortField(fldMSGroupID, sdAscending);
    finally
      Q.Descr.EndUpdate;
    end;
    G := MetaData.MetadataDB.CreateQuery(qtSelect);
    try
      G.Descr.BeginUpdate;
      try
        G.Descr.Table := tblUsers;
        G.Descr.AddParamCondition(fldUsersDeleted, ftSmallint, opEQ, 'DID', 0);
        //G.Descr.AddOperation(opNot);
        G.Descr.AddParamCondition(fldUsersDisabled, ftSmallint, opEQ, 'EID', 0);
        //G.Descr.AddOperation(opNot);
        G.Descr.AddOperation(opAnd);
        G.Descr.AddParamCondition(fldUsersType, ftSmallint, opEQ, 'TID', 2);
        G.Descr.AddOperation(opAnd);
        G.Descr.AddFields([fldUsersID, fldUsersName]);
        G.Descr.AddSortField(fldUsersID, sdAscending);
      finally
        G.Descr.EndUpdate;
      end;
      Q.Open;
      for R:=0 to Pred(Q.RecordCount) do
        begin
          Q.RecNo:= R;
          GroupID := Q.ValueByName[fldMSGroupID];
          GroupIndex := FGroupList.Add(TUserSession.Create);
          FGroupList.Items[GroupIndex].FID := GroupID;
          if not G.Active then G.Open;
          if G.Locate(fldUsersID, GroupID, []) then
            FGroupList.Items[GroupIndex].FName := G.StringValueByName(fldUsersName)
          else
            FGroupList.Delete(GroupIndex); // Если группа не активна или помечена удалённой, то удаляем её из списка!!!
        end;
    finally
      G.Free;
    end;
  finally
    Q.Free;
  end;
end;

function TUserSession.ChangePassword(const Password: string): Boolean;
var
  DataSet: TDeDataset;
begin
  try
    DataSet := MetaData.MetadataDB.CreateQuery(qtUpdate);
    try
      DataSet.Descr.BeginUpdate;
      try
        DataSet.Descr.Table := tblUsers;
        DataSet.Descr.AddParamCondition(fldUsersID, ftInteger, opEQ, 'ID', FID);
        DataSet.Descr.AddParamField(fldUsersPassword, ftString);
        if Length(Password) = 0 then
          DataSet.Descr.ParamValueByName[fldUsersPassword] := EmptyPasswordHash
        else
          DataSet.Descr.ParamValueByName[fldUsersPassword] := MD5password(WideStringToUnicode(Password));
      finally
        DataSet.Descr.EndUpdate;
      end;
      Result := DataSet.ExecuteQuery;
    finally
      DataSet.Free;
    end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TUserSession.ChangePassword skip error: ' + E.Message);
        {$ENDIF}
        Result := False;
      end;
  end;
end;

{ TUserList }

function TUserList.GetUserSession(const Index: Integer): TUserSession;
begin
  Result := TUserSession(inherited Items[Index]);
end;

procedure TUserList.SetUserSession(const Index: Integer; const User: TUserSession);
begin
  if Index > Count then Count := Succ(Index);
  inherited Items[Index] := User;
end;

function TUserList.FindUser(const aUserSession: string): TUserSession;
var I : integer;
begin
  result := nil;
  for I := 0 to Count-1 do
    if CompareText(Items[I].Name, aUserSession) = 0 then
    begin
      result := Items[I];
      break;
    end;
end;

function TUserList.FindByID(const aID: Integer): TUserSession;
var I : integer;
begin
  result := nil;
  for I := 0 to Count-1 do
    if (Items[I].ID=aID) then
    begin
      result := Items[I];
      break;
    end;
end;

function TUserList.IndexByLogin(const Login: string): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Pred(Count) do
    if SameText(Items[Index].Login, Login) then
      begin
        Result := Index;
        Break;
      end;
end;

procedure TUserList.Init(const DefaultGroup: Boolean);
var
  DataSet: TDeDataset;
  UserSession: TUserSession;
  R, Index: Integer;
begin
  DataSet := MetaData.MetadataDB.CreateQuery(qtSelect);
  try
    DataSet.Descr.BeginUpdate;
    try
      DataSet.Descr.Table := tblUsers;
      if not MetaData.ShowRemoved then
        begin
          DataSet.Descr.AddParamCondition(fldUsersDeleted, ftSmallInt, opEQ, 'DID', 0);
          //DataSet.Descr.AddOperation(opNot);
        end;
      DataSet.Descr.AddSortField(fldUsersType, sdDescending);
      DataSet.Descr.AddSortField(fldUsersLogin);
      // 27.08.2015 + Куфенко: Читаем только необходимые поля!
      DataSet.Descr.AddFields([fldUsersID, fldUsersType, fldUsersWorkerID,
        fldUsersLogin, fldUsersName, fldUsersAdmin, fldUsersDisabled]);
      // 27.08.2015 -
    finally
      DataSet.Descr.EndUpdate;
    end;
    DataSet.Open;
    for R:=0 to Pred(DataSet.RecordCount) do
      begin
        DataSet.RecNo:= R;
        Index := Add(TUserSession.Create);
        Items[Index].Assign(DataSet);
        if Items[Index].isGroup then
          Items[Index].FName:= getTitle(Items[Index].FName);
      end;
  finally
    DataSet.Free;
  end;
  if DefaultGroup then
    begin
      UserSession := TUserSession.Create;
      try
        UserSession.FID := 0;
        UserSession.FName := GetTitle('_Da.default');
        UserSession.FIsGroup := True;
        Insert(0, UserSession);
      except
        UserSession.Free;
        raise;
      end;
    end;
end;

procedure TUserList.LoadFromDatabase;
var
  DataSet: TDeDataset;
  R, UserIndex: Integer;
begin
  Clear;
  DataSet := MetaData.MetadataDB.CreateQuery(qtSelect);
  try
    DataSet.Descr.BeginUpdate;
    try
      DataSet.Descr.Table := tblUsers;
      DataSet.Descr.AddParamCondition(fldUsersDeleted, ftSmallInt, opEQ, 'DID', 0);
      //DataSet.Descr.AddOperation(opNot);
      DataSet.Descr.AddParamCondition(fldUsersDisabled, ftSmallint, opEQ, 'EID', 0);
      //DataSet.Descr.AddOperation(opNot);
      DataSet.Descr.AddOperation(opAnd);
      DataSet.Descr.AddParamCondition(fldUsersType, ftSmallint, opEQ, 'TID', 1);
      DataSet.Descr.AddOperation(opAnd);
      DataSet.Descr.AddSortField(fldUsersLogin);
      DataSet.Descr.AddFields([fldUsersID, fldUsersLogin]);
    finally
      DataSet.Descr.EndUpdate;
    end;
    DataSet.Open;
    for R:=0 to Pred(DataSet.RecordCount) do
      begin
        DataSet.RecNo:= R;
        UserIndex := Add(TUserSession.Create);
        Items[UserIndex].FID := DataSet.ValueByName[fldUsersID];
        Items[UserIndex].FLogin := DataSet.ValueByName[fldUsersLogin];
      end;
  finally
    DataSet.Free;
  end;
end;

{ TSecurityItem }

constructor TSecurityItem.Create;
begin
  inherited Create;
  FFilter := TExpressionItem.Create;
end;

destructor TSecurityItem.Destroy;
begin
  FFilter.Free;
  inherited;
end;

function TSecurityItem.UnionRestricts(const aRestrict1, aRestrict2: TObjectRestrict): TObjectRestrict;
begin
  if (aRestrict1 = orEnabled)  or (aRestrict2 = orEnabled)  then result := orEnabled else
  if (aRestrict1 = orDisabled) or (aRestrict2 = orDisabled) then result := orDisabled else
                                                                 result := orNone;
end;

function TSecurityItem.CreateCopy: TSecurityItem;
begin
  result := TSecurityItem.Create;
  result.FRestrict := FRestrict;
  result.FFilter.Assign(FFilter);
end;

procedure TSecurityItem.Union(aUnionWith: TSecurityItem);
begin
  FRestrict := UnionRestricts(FRestrict, aUnionWith.Restrict);
  if FRestrict in [orDisabled] then
    FFilter.Clear
  else
  begin
    FFilter.CopyFrom(aUnionWith.Filter);
    if (FFilter.Count > aUnionWith.Filter.Count) and (aUnionWith.Filter.Count > 0) then
      with FFilter.AddPostfixItem do
      begin
        BinaryOp := opOr;
        ResultType := ftBoolean;
      end;
  end;
end;

{ TObjectPolicy }

procedure TObjectPolicy.Clear;
var I : TSecurityOperation;
begin
  for I := Low(FItems) to High(FItems) do
    DestroyItem(I);
end;

constructor TObjectPolicy.Create(const aObjectType: Integer; const aObjectID: string);
begin
  inherited Create;
  FObjectType := aObjectType;
  FObjectId := aObjectId;
end;

function TObjectPolicy.CreateCopy: TObjectPolicy;
var I : TSecurityOperation;
begin
  result := TObjectPolicy.Create(ObjectType, ObjectID);
  for I := Low(FItems) to High(FItems) do
    if Assigned(FItems[I]) then
      result.FItems[I] := FItems[I].CreateCopy;
end;

destructor TObjectPolicy.Destroy;
begin
  Clear;
  inherited;
end;

procedure TObjectPolicy.DestroyItem(aOperation: TSecurityOperation);
begin
  FreeAndNil(FItems[aOperation]);
end;

function TObjectPolicy.GetItem(
  aOperation: TSecurityOperation): TSecurityItem;
begin
  result := FItems[aOperation];
end;

procedure TObjectPolicy.Union(aUnionWith: TObjectPolicy);
var I : TSecurityOperation;
begin
  for I := Low(FItems) to High(FItems) do
    if Assigned(aUnionWith[I]) then
      if Assigned(FItems[I]) then
        FItems[I].Union(aUnionWith[I])
      else
        FItems[I] := aUnionWith[I].CreateCopy;
end;

procedure TObjectPolicy.PutItem(const aOperation: TSecurityOperation; const aRestrict: TObjectRestrict; const aFilter: string);
var aItem   : TSecurityItem;
    I       : TSecurityOperation;
    Parser  : TDeParser;
    TableID : integer;
begin
  DestroyItem(aOperation);
  aItem := TSecurityItem.Create;
  aItem.Restrict := aRestrict;
  if ObjectType = MetaData.MetaTables[idxDataset].ID then
  begin
    TableID := StrToIntDef(ObjectID, 0);
    Parser := TDeParser.Create;
    Parser.Table := MetaData.GetTableMeta(TableID);
    if Trim(aFilter) <> EmptyStr then
      Parser.Parse(aFilter, aItem.Filter);
    Parser.Free;
  end;
  if aOperation = spAll then
  begin
    for I := Succ(spAll) to High(FItems) do
    begin
      DestroyItem(I);
      FItems[I] := aItem.CreateCopy;
    end;
    aItem.Free;
  end
  else
    FItems[aOperation] := aItem;
end;

{ TUserPolicy }

function _CompareObject(
  aObject : TObjectPolicy;
  const aObjectType : Integer;
  const aObjectID : string) : integer;
begin
  if aObject.ObjectType < aObjectType then
    result := -1
  else if aObject.ObjectType > aObjectType then
    result := 1
  else
    result := CompareText(aObject.ObjectId, aObjectId);
end;

function _CompareObjects(Item1, Item2 : pointer) : integer;
begin
  result := _CompareObject(
    TObjectPolicy(Item1),
    TObjectPolicy(Item2).ObjectType,
    TObjectPolicy(Item2).ObjectID
  );
end;

function TUserPolicy.FindPolicy(const aObjectType: Integer;
  const aObjectID: string): TObjectPolicy;
var Start, Finish, Middle, CompareRes : integer;
begin
  SortPolicy;
  Start := 0;  Finish := Count-1;
  result := nil;
  while Start <> Finish do
  begin
    Middle := (Start + Finish) div 2;
    CompareRes := _CompareObject(Items[Middle], aObjectType, aObjectId);
    if CompareRes = 0 then
    begin
      result := Items[Middle];
      break;
    end
    else if CompareRes > 0 then
      Finish := Middle
    else
      if Middle > Start then
        Start := Middle
      else
        Start := Middle + 1;
  end;
  if (not Assigned(result)) and
     (_CompareObject(Items[Start], aObjectType, aObjectID) = 0) then
    result := Items[Start];
end;

function TUserPolicy.Get(const Index: Integer): TObjectPolicy;
begin
  Result := TObjectPolicy(inherited Items[Index]);
end;

procedure TUserPolicy.Merge(aMergeWith: TUserPolicy);
var I, J, K, CompareRes : integer;
begin
  SortPolicy;
  aMergeWith.SortPolicy;
  {$IFDEF DEBUG}
  DebugPolicyLog(ClassName + '.Merge:');
  aMergeWith.DebugPolicyLog(' from:');
  {$ENDIF}
  I := 0;  J := 0;
  while (I < Count) and (J < aMergeWith.Count) do
  begin
    CompareRes := _CompareObjects(Items[I], aMergeWith[J]);
    if CompareRes < 0 then
      inc(I)
    else if CompareRes = 0 then
    begin
      inc(I);
      inc(J);
    end
    else
    begin
      Insert(I, aMergeWith[J].CreateCopy);
      inc(I);
      inc(J);
    end;
  end;
  for K := J to aMergeWith.Count-1 do
    Add(aMergeWith.Items[K].CreateCopy);
  FSorted := true;
end;

procedure TUserPolicy.Union(aUnionWith : TUserPolicy);
var I, J, K, CompareRes : integer;
begin
  // объединение двух политик безопасности
  SortPolicy;
  aUnionWith.SortPolicy;
  {$IFDEF DEBUG}
  DebugPolicyLog(ClassName + '.Union:');
  aUnionWith.DebugPolicyLog(' from:');
  {$ENDIF}
  I := 0;  J := 0;
  while (I < Count) and (J < aUnionWith.Count) do
  begin
    CompareRes := _CompareObjects(Items[I], aUnionWith[J]);
    if CompareRes < 0 then
      inc(I)
    else if CompareRes = 0 then
    begin
      Items[I].Union(aUnionWith[J]);
      inc(I);
      inc(J);
    end
    else
    begin
      Insert(I, aUnionWith[J].CreateCopy);
      inc(I);
      inc(J);
    end;
  end;
  for K := J to aUnionWith.Count-1 do
    Add(aUnionWith.Items[K].CreateCopy);
  FSorted := true;
end;

procedure TUserPolicy.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnAdded then
    FSorted := false;
end;

procedure TUserPolicy.Put(const Index: Integer; const aItem: TObjectPolicy);
begin
  if Index >= Count then Count := Succ(Index);
  inherited Items[Index] := aItem;
end;

procedure TUserPolicy.SortPolicy;
begin
  if not FSorted then
  begin
    Sort(_CompareObjects);
    FSorted := true;
  end;
end;

{$IFDEF DEBUG}
procedure TUserPolicy.DebugPolicyLog(const Text: string);
  function PreparePolicyLog: string;
  type
    TSecurityItems = array [TSecurityOperation] of TSecurityItem;
    PSecurityItems = ^TSecurityItems;
  var
    Index: Integer;
    ObjectPolicy: TObjectPolicy;
    Value: string;
    function PrepareSecurityItem(SecurityItem: TSecurityItem): string;
    const
      ObjectRestrictNames: array[TObjectRestrict] of PChar = ('orNone', 'orEnabled', 'orDisabled');
    begin
      if Assigned(SecurityItem) then
        Result := StrPas(ObjectRestrictNames[SecurityItem.Restrict])
      else
        Result := EmptyStr;
      Result := Format(' %-11s |', [Result]);
    end;
    function PrepareSecurityItems(SecurityItems: PSecurityItems): string;
    var
      Index: TSecurityOperation;
    begin
      Result := EmptyStr;
      for Index := Low(Index) to High(Index) do
        if Assigned(SecurityItems) then
          Result := Result + PrepareSecurityItem(SecurityItems^[Index])
        else
          Result := Result + PrepareSecurityItem(nil);
    end;
    function PrepareSecurityOperationHeader(const Texted: Boolean): string;
    const
      SecurityOperationNames: array[TSecurityOperation] of PChar = ('spNone', 'spAll', 'spSelect', 'spInsert', 'spUpdate', 'spDelete', 'spExecute');
    var
      Index: TSecurityOperation;
    begin
      Result := EmptyStr;
      for Index := Low(Index) to High(Index) do
        if Texted then
          Result := Result + Format(' %-11s |', [StrPas(SecurityOperationNames[Index])])
        else
          Result := Result + DupeString('-', 13) + '+';
    end;
  begin
    Result := EmptyStr;
    for Index := 0 to Pred(Count) do
      begin
        Result := Result + Format(#13#10'                        | %9d. | ', [Index]);
        ObjectPolicy := Items[Index];
        if Assigned(ObjectPolicy) then
          Result := Result + Format('%9d | %-9s |%s', [ObjectPolicy.ObjectType,
            ObjectPolicy.ObjectID, PrepareSecurityItems(@ObjectPolicy.FItems)])
        else
          Result := Result + Format('%-9s | %-9s |%s', [' ', ' ',
            PrepareSecurityItems(nil)]);
      end;
    if Length(Result) <> 0 then
      begin
        Value := Format(#13#10'                        +%s+%s+%s+%s', [
          DupeString('-', 12), DupeString('-', 11), DupeString('-', 11),
          PrepareSecurityOperationHeader(False)]);
        Result := Value +
          Format(#13#10'                        | %-10s | %-9s | %-9s |%s', [
          'No', 'Type', 'ID', PrepareSecurityOperationHeader(True)]) +
          Value + Result + Value;
      end;
  end;
begin
  Funcs.WriteLog(Text + PreparePolicyLog, True, 'Policy');
end;
{$ENDIF}

{ TSecuritySystem }

procedure TSecuritySystem.BuildSubjectList(const aSubjectID: integer; aLoadTo: TList);
var Q            : TDeDataset;
    R, NewSubjectID : integer;
begin
  Q := MetaData.MetadataDB.CreateQuery(qtSelect);
  try
    Q.Descr.BeginUpdate;
    try
      Q.Descr.Table := tblMembership;
      Q.Descr.AddParamCondition(fldMSMemberId, ftInteger, opEQ, 'SID', aSubjectID);
      // 27.08.2015 + Куфенко: Читаем только необходимые поля!
      Q.Descr.AddField(fldMSGroupId);
      // 27.08.2015 -
    finally
      Q.Descr.EndUpdate;
    end;
    Q.Open;
    for R:=0 to Pred(Q.RecordCount) do
    begin
      Q.RecNo:= R;
      NewSubjectID := VarToInt(Q.ValueByName[fldMSGroupId]);
      aLoadTo.Add(pointer(NewSubjectId));
      BuildSubjectList(NewSubjectID, aLoadTo);
    end;
  finally
    Q.Free;
  end;
end;

function TSecuritySystem.CheckPolicyDataset(const aObjectID: integer; const aOperation: TSecurityOperation): boolean;
var M: TTableMeta;
begin
  M:= Metadata.GetTableMeta(aObjectID);
  if Assigned(M) and (aOperation in [spInsert, spUpdate, spDelete]) and ((M.IsReadOnly) or (M.ObjectType = otView)) then
    Exit(False);

  result := CheckPolicy(MetaData.MetaTables[idxDataset].ID, aObjectID, aOperation);

  if result then
    if aOperation in [spInsert, spUpdate, spDelete] then
      if Not UserSession.IsAdmin then
        if (aObjectID = MetaData.MetaTables[idxBase].ID) or
           (aObjectID = MetaData.MetaTables[idxDataset].ID) or
           (aObjectID = MetaData.MetaTables[idxFields].ID) or
           (aObjectID = MetaData.MetaTables[idxElement].ID) or
           (aObjectID = MetaData.MetaTables[idxCommands].ID) then Result:=False;
end;

function TSecuritySystem.CheckPolicyField(const AObjectID: Integer; const AOperation: TSecurityDataset): Boolean;
const
  Operations: array[TSecurityDataset] of TSecurityOperation = (spSelect, spInsert, spUpdate, spDelete, spExecute);
begin
  Result:= CheckPolicy(MetaData.MetaTables[idxFields].ID, aObjectID, Operations[AOperation]);
end;

function TSecuritySystem.CheckPolicyMenu(const aObjectID: integer;
  const aOperation: TSecurityOperation): boolean;
begin
  result := CheckPolicy(MetaData.MetaTables[idxMenu].ID, aObjectID, aOperation);
end;

{
function TSecuritySystem.CheckPolicyReport(const aObjectID: integer;
  const aOperation: TSecurityOperation): boolean;
begin
  result := CheckPolicy(MetaData.MetaTables[idxReports].ID, aObjectID, aOperation);
end;

function TSecuritySystem.CheckPolicyAction(const aObjectID: integer;
  const aOperation: TSecurityOperation): boolean;
begin
  if (-1 < aObjectID) then
    result := CheckPolicy(MetaData.MetaTables[idxActions].ID, aObjectID, aOperation)
  else
    result := True;
end;
}

function TSecuritySystem.CheckPolicyCommand(const ObjectID: Integer; const Operation: TSecurityOperation): Boolean;
begin
  Result := CheckPolicy(MetaData.MetaTables[idxCommands].ID, ObjectID, Operation);
end;

function TSecuritySystem.CheckPolicyConstraint(const ObjectID: Integer; const Operation: TSecurityOperation): Boolean;
begin
  Result := CheckPolicy(MetaData.MetaTables[idxConstraints].ID, ObjectID, Operation);
end;

function TSecuritySystem.GetObjectPolicy(const ObjectType: Integer; const ObjectID: Variant; const Operation: TSecurityOperation): TObjectRestrict;
var
  SecurityItem: TSecurityItem;
begin
  SecurityItem := GetPolicyDetails(ObjectType, ObjectID, Operation);
  // Права на конкретный объект не заданы ищем права на все объекты данного типа
  if not Assigned(SecurityItem) then
    SecurityItem := GetPolicyDetails(ObjectType, EmptyStr, Operation);

  if Assigned(SecurityItem) then
    Result := SecurityItem.Restrict
  else
    result := orNone; // По-умолчанию разрешение не определено
end;

function TSecuritySystem.CheckPolicy(const aObjectType: Integer;
  const aObjectID: Variant; const aOperation: TSecurityOperation): boolean;
//var SecurityItem : TsecurityItem;
begin
  if Assigned(UserSession) and UserSession.IsAdmin then Exit(True);

  if VarIsNullorEmpty(aObjectID)
    then Result:= True  // у полей и таблиц нет ID
    else Result:= GetObjectPolicy(aObjectType, aObjectID, aOperation) in [orNone, orEnabled];
  {
  SecurityItem := GetPolicyDetails(aObjectType, aObjectId, aOperation);

  // Права на конкретный объект не заданы ищем права на все объекты данного типа
  if (not Assigned(SecurityItem)) then
    SecurityItem := GetPolicyDetails(aObjectType, EmptyStr, aOperation);

  if Assigned(SecurityItem) then
    Result := (SecurityItem.Restrict in [orNone, orEnabled])
  else
    result := True; // По-умолчанию разрешено, в том числе для администратора
  }
end;

procedure TSecuritySystem.ClearPolicy;
begin
  if Assigned(FPolicy) then
    FPolicy.Clear;
end;

procedure TSecuritySystem.SetObjectPolicy(const aObjectType : Integer;
  const aObjectID : Variant;  const aOperation : TSecurityOperation;
  const aRestrict : TObjectRestrict);
var Item    : TSecurityItem;
    ObjItem : TObjectPolicy;
begin
  Item := GetPolicyDetails(aObjectType, aObjectID, aOperation);
  if Assigned(Item) then
    Item.Restrict := aRestrict
  else
  begin
    ObjItem := TObjectPolicy.Create(aObjectType, aObjectID);
    ObjItem.PutItem(spAll, orEnabled, EmptyStr);
    ObjItem.PutItem(aOperation, aRestrict, EmptyStr);
    FPolicy.Add(ObjItem);
    FPolicy.SortPolicy;
  end;
end;

destructor TSecuritySystem.Destroy;
begin
  DestroyPolicy;
  inherited;
end;

procedure TSecuritySystem.DestroyPolicy;
begin
  FreeAndNil(FPolicy);
end;

function TSecuritySystem.GetPolicyDetails(
  const aObjectType: Integer; const aObjectID: Variant;
  const aOperation: TSecurityOperation): TSecurityItem;
{$IFDEF DEBUG}
const SecurityOperationNames: array[TSecurityOperation] of PChar = ('spNone', 'spAll', 'spSelect', 'spInsert', 'spUpdate', 'spDelete', 'spExecute');
{$ENDIF}
var ObjectID : string;
    I        : integer;
begin
  result := nil;
  if Assigned(FPolicy) then // and Assigned(UserSession) and (not UserSession.IsAdmin)
  begin
    {$IFDEF DEBUG}
    if aObjectType = 716 then
      FPolicy.DebugPolicyLog(Format('%s.GetPolicyDetails(%d, %s, %s) ...', [
        ClassName, aObjectType, VariantToString(aObjectID),
        StrPas(SecurityOperationNames[aOperation])]));
    {$ENDIF}
    I := 0;
    ObjectID := VarToStr(aObjectID);
    while (I < FPolicy.Count) and
          (_CompareObject(FPolicy[I], aObjectType, ObjectID) < 0) do
      inc(I);
    if (I < FPolicy.Count) and
       (_CompareObject(FPolicy[I], aObjectType, ObjectID) = 0) then
      result := FPolicy[I][aOperation];
  end;
end;

function TSecuritySystem.LoadPolicy: boolean;
begin
  result := Assigned(UserSession);

  if Assigned(UserSession) then
  begin
    DestroyPolicy;
    FPolicy := TUserPolicy.Create;
    GetPrincipalPolicy(UserSession.ID, FPolicy);
    if (not UserSession.IsAdmin) then
    begin
      // добавляем дополнительные ограничения, накладываемые на системные
      // таблицы при входе в режиме пользователя
      SetObjectPolicy( MetaData.MetaTables[idxDataset].ID,
                       MetaData.MetaTables[idxSolutions].ID, spInsert, orDisabled);
    end;
  end;
end;

procedure TSecuritySystem.LoadPrincipalPolicy(const aPrincipalID: Integer; aPolicy: TUserPolicy);
var Q : TDeDataset;
    ObjectPolicy : TObjectPolicy;
    R, _ObjectType, ObjectType   : Integer;
    _ObjectID, ObjectID     : string;
begin
  Q := MetaData.MetadataDB.CreateQuery(qtSelect);
  try
    Q.Descr.Table := tblRights;
    if aPrincipalID = 0 then
      Q.Descr.AddCondition(fldRightsSubjectId, ftInteger, opIs, Null)
    else
      Q.Descr.AddCondition(fldRightsSubjectId, ftInteger, opEQ, aPrincipalID);
    //Q.Descr.AddParamCondition(fldRightsDeleted, ftInteger, opEQ, 'DID', 1);
    // 27.08.2015 + Куфенко: Читаем только необходимые поля!
    Q.Descr.AddFields([fldRightsObjectID, fldRightsObjectType, fldRightsOperation, fldRightsPolicy, fldRightsRights]);
    // 27.08.2015 -
    //Q.Descr.AddOperation(opNot);
   // Q.Descr.AddOperation(opAnd);
    Q.Descr.AddSortFields([fldRightsObjectType, fldRightsObjectId]);
    Q.Open;

    ObjectPolicy:= nil;
    for R:= 0 to Pred(Q.RecordCount) do
      begin
        Q.RecNo:= R;
        ObjectType:= VarToInt(Q.ValueByName[fldRightsObjectType]);
        ObjectId:=   VarToStr(Q.ValueByName[fldRightsObjectID]);

        if not assigned(ObjectPolicy) or not (ObjectType = _ObjectType) or not (ObjectId = _ObjectId) then
          begin
            if assigned(ObjectPolicy) then aPolicy.Add(ObjectPolicy);

            ObjectPolicy:= TObjectPolicy.Create(ObjectType, ObjectID);
            _ObjectType:= ObjectType;
            _ObjectId:= ObjectId;
          end;

        ObjectPolicy.PutItem( TSecurityOperation(VarToInt(Q.ValueByName[fldRightsOperation])),
                              TObjectRestrict(VarToInt(Q.ValueByName[fldRightsPolicy])),
                              VarToStr(Q.ValueByName[fldRightsRights]));
      end;

      if assigned(ObjectPolicy) then aPolicy.Add(ObjectPolicy);
    aPolicy.Sort(_CompareObjects);
  finally
    Q.Free;
  end;
  {$IFDEF DEBUG}
  if Assigned(aPolicy) then
    aPolicy.DebugPolicyLog(Format('%s.LoadPrincipalPolicy(%d, {}) ...', [ClassName, aPrincipalID]));
  {$ENDIF}
end;

procedure TSecuritySystem.GetPrincipalPolicy(const aPrincipalID: Integer; aPolicy: TUserPolicy);
var Q : TDeDataset;
    R, I : integer;
    Policies : TObjectList;
    TempPolicy : TUserPolicy;
begin
  {$IFDEF DEBUG}
  Funcs.WriteLog('%s.GetPrincipalPolicy(%d, {}) start ...', [ClassName, aPrincipalID], True, 'Policy');
  {$ENDIF}
  // начитываем политику для указанного участника безопасности
  LoadPrincipalPolicy(aPrincipalID,  aPolicy);

  Policies := TObjectList.Create;
  try
    // начитываем политики для дочерних объектов безопасности
    Q := MetaData.MetadataDB.CreateQuery(qtSelect);
    try
      Q.Descr.BeginUpdate;
      try
        Q.Descr.Table := tblMembership;
        Q.Descr.AddParamCondition(fldMSMemberId, ftInteger, opEQ, 'PID', aPrincipalID);
        // 27.08.2015 + Куфенко: Читаем только необходимые поля!
        Q.Descr.AddField(fldMSGroupId);
        // 27.08.2015 -
      finally
        Q.Descr.EndUpdate;
      end;
      Q.Open;
      for R:=0 to Pred(Q.RecordCount) do
      begin
        Q.RecNo:= R;
        TempPolicy := TUserPolicy.Create;
        Policies.Add(TempPolicy);
        LoadPrincipalPolicy(VarToInt(Q.ValueByName[fldMSGroupId]), TempPolicy);
      end;
    finally
      Q.Free;
    end;

    // объединяем политики для дочерних участников безопасности
    if Policies.Count  > 0 then
    begin
      if Policies.Count  > 1 then
        for I := 1 to Pred(Policies.Count) do
          TUserPolicy(Policies[0]).Union(TUserPolicy(Policies[I]));
      aPolicy.Merge(TUserPolicy(Policies[0]));
    end;
  finally
    Policies.Free;
  end;

  TempPolicy := TUserPolicy.Create;
  try
    // начитываем политику по умолчанию для участника безопасности
    LoadPrincipalPolicy(0,  TempPolicy);
    aPolicy.Merge(TempPolicy);
  finally
    TempPolicy.Free;
  end;
  {$IFDEF DEBUG}
  Funcs.WriteLog('%s.GetPrincipalPolicy(%d, {}) finish ...', [ClassName, aPrincipalID], True, 'Policy');
  {$ENDIF}
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('Security unit initialization ...');
  {$ENDIF}
  SecuritySystem := TSecuritySystem.Create;
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('Security unit finalization ...');
  {$ENDIF}
  FreeAndNil(SecuritySystem);
  FreeAndNil(UserSession);
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

