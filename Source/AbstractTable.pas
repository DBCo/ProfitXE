unit AbstractTable;

interface

uses SysUtils, Classes, DB, Variants, System.Generics.Collections;

type

  TRecordState  = (rsNotchanged, rsInserted, rsModified, rsDeleted);

  TConstrType = (ctPrimaryKey, ctForeignKey, ctUnique, ctNotNull{, ctCustom});

  TConstraintType = set of TConstrType;

  (*
  TCustomFields   = class;
  //TCustomIndex = class;

  TCustomField = class
  private
    FSize: integer;
    FName: string;
    FFieldType: TFieldType;
    FOwner : TCustomFields;
    function GetFieldNum: integer;
  public
    constructor Create(AOwner:TCustomFields; FieldName : string);
    property Name : string read FName;
    property FieldType : TFieldType read FFieldType write FFieldType;
    property Size : integer read FSize write FSize;
    property FieldNum : integer read GetFieldNum;
  end;
   *)

(*  //  пока нигде не используется  
  TCustomConstraint = class
  private
//    FFields  : TList;
    FCType   : TConstraintType;
    function GetField(index: integer): TCustomField;
  public
    function  AddField(AField : TCustomField):integer;
    procedure DeleteField(AField : TCustomField);overload;
    procedure DeleteField(Index : integer);overload;
    function  FieldCount : integer;
    property  Fields[index:integer] : TCustomField read GetField;
    property  ConstraintType :  TConstraintType read  FCType;
  end;
*)
(*
  TCustomFields = class
  private
    FCustomFields : array of TCustomField;
    function GetField(index: integer): TCustomField;
    function GetFieldByName(FieldName: string): TCustomField;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;
    function  AddField(FieldName : string; FieldType : TFieldType = ftUnknown; FieldSize : integer = 0):TCustomField;
    procedure DeleteField(Field : TCustomField);overload;
    procedure DeleteField(Index : integer);overload;
    function  IndexOf(Field : TCustomField): integer;overload;
    function  IndexOf(const FieldName : string): integer;overload;
    property Count : integer read GetCount;
    property Field[index : integer]:TCustomField read GetField ;default;
    property FieldByName[FieldName : string]:TCustomField read GetFieldByName;
  end;
 *)


  TRecordEvent = procedure(ARecord : Pointer; var Allow : Boolean) of object;

  TCustomFields = TObjectList<TField>;

  TRecordsList = class
  private
    FRecords : TList;
    FRecordDelete: TRecordEvent;
    FRecordInsert: TRecordEvent;
    function  GetRecord(Index: Integer): Pointer;
    function  GetCount: integer;
    function DoRecordAdd(aRecord : Pointer):Boolean;
    function DoRecordDelete(aRecord : Pointer):Boolean;
  public
    constructor Create;
    destructor  Destroy;override;
    function  Add(aRecord: Pointer): Integer;
    procedure Insert(Index: Integer; aRecord: Pointer);
    procedure Clear;
    procedure Delete(Index: Integer);
    function  Find(aRecord : Pointer): integer;
    property  Count : integer read GetCount;
    property  Records[Index: Integer]: Pointer read GetRecord;default;
    property  OnRecordInsert : TRecordEvent read  FRecordInsert write FRecordInsert;
    property  OnRecordDelete : TRecordEvent read  FRecordDelete write FRecordDelete ;
  end;

  TFieldNameValue = record
                      Name  : string;
                      Value : Variant;
                    end;

  TDataNotificationType = (dnInsert, dnDelete, dnUpdate, dnScroll);
  TNotifyState = set of TDataNotificationType;

  TDataNotifyEvent = procedure (Sender : TObject; NotificationType :  TDataNotificationType) of object;

  TDataNotification = record
                        OnNotify : TDataNotifyEvent;
                        NotifyState : TNotifyState;
                      end;



  TAbstractTable = class(TComponent)
  private
    FNotifications : TList;
//    FFields   : TCustomFields;
    FFields   : TObjectList<TField>;
    FRecords  : TRecordsList;
    FCurrentIndex : integer;
    FActive: Boolean;
//    FOnReadRecord: TRecordEvent;
    FInsertRecord: TRecordEvent;
    FDeleteRecord: TRecordEvent;
    FEOF         : Boolean;
    FCanNotify   : Boolean;
    procedure CheckEmpty;
    function  GetFieldValue(index: integer): Variant;
    procedure SetFieldValue(index: integer; const Value: Variant);
    procedure SetActive(const Value: Boolean);
    procedure DoOnInsertRecord(ARecord : Pointer; var Allow : Boolean);
    procedure DoOnDeleteRecord(ARecord : Pointer; var Allow : Boolean);
    function  GetFieldValueByName(aName: string): Variant;
    procedure SetFieldValueByName(aName: string; const Value: Variant);
    procedure DoNotify(ANotificationType : TDataNotificationType);
  protected
    procedure InternalGetFieldValue(CurrentRecord : Pointer; FieldIndex : integer; var   Value: Variant); virtual;abstract;
    procedure InternalSetFieldValue(CurrentRecord : Pointer; FieldIndex : integer; const Value: Variant); virtual;abstract;
    procedure InternalReadFields(Fields : TObjectList<TField>); virtual; abstract;
    procedure InternalReadData(RecordsList : TRecordsList); virtual;abstract;
    function  InternalGetDataObject: TObject; virtual;abstract;
    function  InternalAppendRecord(Values : array of TFieldNameValue): Pointer; virtual;abstract;
    function  InternalDeleteRecord(CurrentRecord : Pointer): Boolean; virtual;abstract;
    function  InternalGetRecordState(CurrentRecord : Pointer): TRecordState; virtual;abstract;
   // function  InternalGetActive():Boolean;virtual;abstract;
  public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy;override;
    procedure ReadMeta;
    procedure ReadData;
    procedure AppendRecord(Values : array of TFieldNameValue);
    procedure DeleteRecord;
    procedure Next;
    procedure Prior;
    procedure First;
    procedure Last;
    function  EOF:Boolean;
    function  RecNo:integer;
    function  RecState: TRecordState;
    function  RecordCount:integer;
    function  Locate(const KeyFields: string;const KeyValues: Variant; Options: TLocateOptions): Boolean;
    function  AddNotification(OnNotify : TDataNotifyEvent; NotifyState : TNotifyState= [dnInsert, dnDelete, dnUpdate, dnScroll]): integer;
    function  FieldIndexByName(const aName: String): Integer;
    procedure DeleteNotification(index : integer);
    procedure DisableControls;
    procedure EnableControls;
    property  DataObject : TObject read InternalGetDataObject;
    property  Fields : TObjectList<TField> read FFields;
    property  FieldValue[index : integer] : Variant read GetFieldValue write SetFieldValue;
    property  FieldValueByName[aName : string] : Variant read GetFieldValueByName write SetFieldValueByName;
    property  Active : Boolean read FActive write SetActive;
    property  OnInsertRecord : TRecordEvent read  FInsertRecord write  FInsertRecord;
    property  OnDeleteRecord : TRecordEvent read  FDeleteRecord write  FDeleteRecord;
  end;

implementation

{ TCustomField }
(*
constructor TCustomField.Create(AOwner:TCustomFields; FieldName : string);
begin
  if not Assigned(AOwner) then
    raise Exception.Create('Owner dos not exists');
  FOwner := AOwner;
  if FieldName = EmptyStr then
    raise Exception.Create('Field name is empty');
  FName := FieldName;
  FieldType := ftUnknown;
  FSize := 0;
end;

function TCustomField.GetFieldNum: integer;
begin
  result := FOwner.IndexOf(Self);
end;

{ TCustomFields }

constructor TCustomFields.Create;
begin
  SetLength(FCustomFields,0);
end;

destructor TCustomFields.Destroy;
begin
   SetLength(FCustomFields,0);
   inherited;
end;

procedure TCustomFields.Clear;
begin
  while Count > 0 do
    DeleteField(High(FCustomFields));
end;

function TCustomFields.GetCount: integer;
begin
  result := Length(FCustomFields);
end;

function TCustomFields.AddField(FieldName: string; FieldType : TFieldType; FieldSize : integer): TCustomField;
begin
  result := TCustomField.Create(Self, FieldName);
  SetLength(FCustomFields,Count+1);
  FCustomFields[High(FCustomFields)]:= result;
end;

procedure TCustomFields.DeleteField(Field : TCustomField);
var
  i : integer;
begin
  i := IndexOf(Field);
  DeleteField(i);
end;

procedure TCustomFields.DeleteField(Index: integer);
begin
  System.Move(FCustomFields[Index+1], FCustomFields[Index],
      (Count - Index) * SizeOf(TCustomField));
  SetLength(FCustomFields, Count-1);
end;

function TCustomFields.GetField(index: integer): TCustomField;
begin
  if (index<0) or (index > High(FCustomFields))then
    raise Exception.Create('Field index out of bounds');
  result := FCustomFields[index];
end;

function TCustomFields.GetFieldByName(FieldName: string): TCustomField;
var
  i : integer;
begin
  result := nil;
  for i := Low(FCustomFields) to High(FCustomFields) do
    if SameText(FCustomFields[i].Name,FieldName) then
    begin
      result := FCustomFields[i];
      Break;
    end;
  if not Assigned(result) then
    raise Exception.CreateFmt('Field %1 dose not exists',[FieldName]);
end;

function TCustomFields.IndexOf(Field: TCustomField): integer;
var
  i : integer;
begin
  result := -1;
  if not Assigned(Field) then Exit;
  for i:= Low(FCustomFields) to High(FCustomFields) do
    if FCustomFields[i] = Field then
    begin
      result := i;
      Break;
    end;
end;
 
function TCustomFields.IndexOf(const FieldName: string): integer;
var
  i : integer;
begin
  result := -1;
  for i:= Low(FCustomFields) to High(FCustomFields) do
    if SameText(FCustomFields[i].Name,FieldName) then
    begin
      result := i;
      Break;
    end;
end;
  *)
{ TRecordsList }

constructor TRecordsList.Create;
begin
  FRecords := TList.Create;
end;

destructor TRecordsList.Destroy;
begin
  FRecords.Free;
  inherited;
end;

function TRecordsList.Add(aRecord: Pointer): Integer;
begin
  if DoRecordAdd(aRecord) then
    result := FRecords.Add(aRecord)
  else
    result := -1;
end;

procedure TRecordsList.Insert(Index: Integer; aRecord: Pointer);
begin
  if DoRecordAdd(aRecord) then
      FRecords.Insert(Index, aRecord);
end;

procedure TRecordsList.Clear;
begin
  FRecords.Clear;
end;

procedure TRecordsList.Delete(Index: Integer);
begin
  if  DoRecordDelete(FRecords[Index]) then
    FRecords.Delete(Index);
end;

function TRecordsList.Find(aRecord: Pointer): integer;
begin
  result := FRecords.IndexOf(aRecord);
end;

function TRecordsList.GetRecord(Index: Integer): Pointer;
begin
  result := FRecords[Index];
end;

function TRecordsList.GetCount: integer;
begin
  result := FRecords.Count;
end;

function TRecordsList.DoRecordAdd(aRecord: Pointer): Boolean;
begin
  result := True;
  if Assigned(OnRecordInsert) then OnRecordInsert(aRecord,result);
end;

function TRecordsList.DoRecordDelete(aRecord: Pointer): Boolean;
begin
  result := True;
  if Assigned(OnRecordDelete) then OnRecordDelete(aRecord,result);
end;

{ TAbstractTable }

constructor TAbstractTable.Create(AOwner: TComponent);
begin
  inherited;
  FFields := TObjectList<TField>.Create;
  FRecords   := TRecordsList.Create;
  FNotifications :=  TList.Create;
  FRecords.OnRecordInsert := DoOnInsertRecord;
  FRecords.OnRecordDelete := DoOnDeleteRecord;
  FCurrentIndex := -1;
  FCanNotify := True;
  FEOF := True;
end;

destructor TAbstractTable.Destroy;
begin
  FFields.Free;
  FRecords.Free;
  FNotifications.Free;
  inherited;
end;


procedure TAbstractTable.SetActive(const Value: Boolean);
begin
  if FActive = Value then Exit;
  FActive := Value;
  if FActive then
  begin
    if FFields.Count = 0 then
      ReadMeta;
    if FFields.Count > 0 then
      ReadData;
  end
  else
  begin
    FRecords.Clear;
  end;
end;

procedure TAbstractTable.ReadData;
begin
  FRecords.Clear;
  FCurrentIndex := -1;
  InternalReadData(FRecords);
  if FRecords.Count>0 then
    FCurrentIndex := 0;
  FEOF := (FRecords.Count=0);
end;

procedure TAbstractTable.ReadMeta;
begin
  FFields.Clear;
  InternalReadFields(FFields);
end;

procedure TAbstractTable.CheckEmpty;
begin
  if RecordCount = 0 then
    raise Exception.Create('Data is empty');
end;

function TAbstractTable.EOF: Boolean;
begin
//  FEOF := FEOF and (not (RecordCount > 0));
  result := FEOF;
end;

procedure TAbstractTable.First;
begin
  if RecordCount > 0 then
    FCurrentIndex := 0;
  FEOF := not (RecordCount > 0);
  DoNotify(dnScroll);
end;

procedure TAbstractTable.Last;
begin
  if RecordCount > 0 then
    FCurrentIndex := RecordCount -1;
  FEOF := not (RecordCount > 0);
  DoNotify(dnScroll);
end;

procedure TAbstractTable.Next;
begin
  if (RecordCount > 0) then
    if FCurrentIndex = RecordCount - 1 then
      FEOF := True
    else
      inc(FCurrentIndex);
    DoNotify(dnScroll);
end;

procedure TAbstractTable.Prior;
begin
  if (RecordCount > 0)and(FCurrentIndex >0) then
    dec(FCurrentIndex);
  FEOF := not (RecordCount > 0);
  DoNotify(dnScroll);
end;

function TAbstractTable.RecNo: integer;
begin
  result := FCurrentIndex +1;
end;

function TAbstractTable.RecState: TRecordState;
begin
  CheckEmpty;
  result := InternalGetRecordState(FRecords[FCurrentIndex]);
end;

function TAbstractTable.RecordCount: integer;
begin
  result := FRecords.Count;
end;

function TAbstractTable.GetFieldValue(index: integer): Variant;
begin
  CheckEmpty;
  InternalGetFieldValue(FRecords[FCurrentIndex],index,result);
end;

procedure TAbstractTable.SetFieldValue(index: integer; const Value: Variant);
begin
  CheckEmpty;
  InternalSetFieldValue(FRecords[FCurrentIndex],index,Value);
  DoNotify(dnUpdate);
end;

function TAbstractTable.FieldIndexByName(const aName: String): Integer;
var i: Integer;
begin
  Result:= -1;
  for i:=0 to Pred(Fields.Count) do
    if Sametext(Fields[i].FieldName, aName) then Exit(i);
end;

function TAbstractTable.GetFieldValueByName(aName: string): Variant;
var
  FieldIndex : integer;
begin
  FieldIndex := FieldIndexByName(aName);
  result := GetFieldValue(FieldIndex);
end;

procedure TAbstractTable.SetFieldValueByName(aName: string; const Value: Variant);
var
  FieldIndex : integer;
begin
  FieldIndex := FieldIndexByName(aName);
  SetFieldValue(FieldIndex, Value);
end;

procedure TAbstractTable.DeleteRecord;
var
  result : boolean;
begin
  CheckEmpty;
  result := InternalDeleteRecord(FRecords[FCurrentIndex]);
  if result then
  begin
    DoNotify(dnDelete);
    FRecords.Delete(FCurrentIndex);
    if FCurrentIndex >= RecordCount then
      FCurrentIndex := RecordCount -1;
  end;
end;

procedure TAbstractTable.AppendRecord(Values: array of TFieldNameValue);
var
  result : pointer;
begin
  result := InternalAppendRecord(Values);
  if Assigned(result) then
  begin
    FCurrentIndex :=FRecords.Add(result);
    DoNotify(dnInsert);
  end;
end;

function TAbstractTable.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  KFields  : TStringList;
  i,j,i_pos,f,vCount : integer;
  FOldCanNotify : Boolean;
  v             : Variant;
  vFields       : array of Integer;
begin
  result := false;
  i_pos  := -1;
  if RecordCount = 0 then Exit;

  KFields := TStringList.Create;
  //KFields.Delimiter := ';';
  KFields.DelimitedText :=  StringReplace(KeyFields,' ',EmptyStr,[rfReplaceAll]);
  if VarIsArray(KeyValues) then
    vCount := VarArrayHighBound(KeyValues,1)+1
  else
    vCount := 1;

  if (vCount<>KFields.Count)or(vCount<1) then Exit;

  SetLength(vFields,vCount);
  for i:=0 to vCount-1 do
    vFields[i]:= FieldIndexByName(KFields[i]);

  F:=FCurrentIndex;
  FOldCanNotify :=  FCanNotify;
  FCanNotify := False;

  for i:=1 to RecordCount do
    begin
      result := true;
      // перебираем начиная со следующей записи
      i_pos:=(F+i) mod RecordCount;

      if KFields.Count=1 then
          begin
            InternalGetFieldValue(FRecords[i_pos], vFields[0], v);
            result := VarSameValue( v, KeyValues);
          end
      else
        for j := 0 to KFields.Count -1 do
          begin
            InternalGetFieldValue(FRecords[i_pos], vFields[j], v);
            result := result and VarSameValue( v, KeyValues[j]);
          end;

      if result then Break;
    end;

  KFields.Free;
  SetLength(vFields,0);

  FCanNotify := FOldCanNotify;
  if Result then
    begin
      FCurrentIndex:=i_pos;
      DoNotify(dnScroll);
    end;

//  if result then DoNotify(dnInsert);
end;

procedure TAbstractTable.DoOnDeleteRecord(ARecord: Pointer;
  var Allow: Boolean);
begin
  Allow := True;
  if Assigned(OnDeleteRecord) then OnDeleteRecord(ARecord, Allow);
end;

procedure TAbstractTable.DoOnInsertRecord(ARecord: Pointer;
  var Allow: Boolean);
begin
  Allow := True;
  if Assigned(OnInsertRecord) then OnInsertRecord(ARecord, Allow);
end;

function TAbstractTable.AddNotification(OnNotify: TDataNotifyEvent;
  NotifyState: TNotifyState): integer;
var
  N : ^TDataNotification;
begin
  New(N);
  N^.OnNotify := OnNotify;
  N^.NotifyState := NotifyState;
  result := FNotifications.Add(N);
end;

procedure TAbstractTable.DeleteNotification(index: integer);
var
  N : ^TDataNotification;
begin
  N:= FNotifications.Extract(FNotifications.Items[index]);
  Dispose(N);
end;

procedure TAbstractTable.DoNotify(
  ANotificationType: TDataNotificationType);
var
  i : integer;
  N : ^TDataNotification;
begin
  if not FCanNotify then Exit;
  for i := 0  to FNotifications.Count -1 do
  begin
     N := FNotifications.Items[i];
     if ANotificationType in N^.NotifyState then
      N^.OnNotify(Self, ANotificationType);
  end;
end;

procedure TAbstractTable.DisableControls;
begin
  FCanNotify := False;
end;

procedure TAbstractTable.EnableControls;
begin
  FCanNotify := True;
end;

(* //  пока нигде не используется
{ TCustomConstraint }

function TCustomConstraint.AddField(AField: TCustomField): integer;
begin
   
end;

procedure TCustomConstraint.DeleteField(AField: TCustomField);
begin

end;

procedure TCustomConstraint.DeleteField(Index: integer);
begin

end;

function TCustomConstraint.FieldCount: integer;
begin

end;

function TCustomConstraint.GetField(index: integer): TCustomField;
begin

end;
*)

end.

