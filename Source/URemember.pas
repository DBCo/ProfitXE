unit URemember;

interface

uses Classes, Forms, Messages, SysUtils, ExtCtrls, Contnrs, Variants,
     DateUtils, Funcs;

const
  RM_ICONNOTIFY = WM_USER + 1; //сообщение от иконки
  htHoldDay  = -1;
  htHoldWeek = -7;

type
  TAllert = class(TObject)     // некоторое событие
      AllertID    : integer;   // индекс события
      AllertAuthor: String;    // автор
      AllertMesg  : String;    // текстовое сообщение
      AllertTime  : TDateTime; // время срабатывания напоминания
      AllertFTime : TDateTime; // первое время срабатывания напоминания
      AllertMTime : TDateTime; // время изменения напоминания
      AllertPrior : Integer;   // Приоритет задачи

      TableID     : integer;   // ID таблицы объекта
      TableName   : string;    // Имя таблицы объекта
      PrimaryKey  : string;    // первичный ключ
      Active      : Boolean;   // активно или уже сработало
      UpdateFlag  : Boolean;   // Флаг для синхноризации списков
  public
  end;

  TAllerts = class(TObjectList)
  private
    FTimer       : TTimer;
    FInterval    : Integer;
    FOnAllert    : TNotifyEvent;
    FCanActivate : Boolean;
    FFocusedAllert : TAllert;
    function GetAllert(aIndex: integer): TAllert;
    procedure OnUpdateData( Sender: TObject);
    function getInterval : Integer;
    procedure setInterval (aInterval: Integer);
    function getEnabled : Boolean;
    procedure setEnabled (aEnabled: Boolean);
  public
    constructor Create;
    destructor Destroy;  override;
    procedure Show;
    property Items[aIndex : integer]: TAllert read GetAllert;  default;
    function DeleteByID(aAllertID: Integer; InBase : Boolean = True): Boolean;
    function HoldOverByID(aAllertID, aMinutes: Integer): Boolean;
    function AllertByID(aID: Integer): TAllert;
    property onAllert: TNotifyEvent read FOnAllert write FOnAllert;
    property Interval: Integer read getInterval write setInterval;
    property Enabled: Boolean read getEnabled write setEnabled;
    property CanActivate: Boolean read FCanActivate write FCanActivate;
    property FocusedAllert: TAllert read FFocusedAllert write FFocusedAllert;
  end;

//==============================================================================

implementation
uses DB, DeMeta, DataUnit, QueryDescriptor, DeTypes, DeMetadata, DeDB, Security, Dictionary;

// === TAllerts ================================================================

constructor TAllerts.Create;
begin
  inherited;
  FTimer:=TTimer.Create(Application.MainForm);
  FTimer.OnTimer  := OnUpdateData;
  FTimer.Interval := 15000;
  FInterval       := 60000;
  FTimer.Enabled  := False;
  FFocusedAllert  := nil;
  FCanActivate    := True;
end;

destructor TAllerts.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TAllerts.Show;
begin
  if Assigned(FOnAllert) then FOnAllert(self);
end;

function TAllerts.getEnabled : Boolean;
begin
  Result:=FTimer.Enabled;
end;

procedure TAllerts.setEnabled (aEnabled: Boolean);
begin
  FTimer.Enabled:=aEnabled;
end;

function TAllerts.getInterval : Integer;
begin
  result:=FInterval;
end;

procedure TAllerts.setInterval(aInterval:Integer);
begin
  FInterval:=aInterval;
  FTimer.Interval:=aInterval;
end;

procedure TAllerts.OnUpdateData( Sender : TObject);
var Q,R,A      : TDeDataSet;
    Allert     : TAllert;
    T,AllertID,i : Integer;
    TM         : TTableMeta;
    NewAllert  : Boolean;
begin
  FTimer.Interval:=FInterval;
  
  for i:=0 to Count-1 do
    items[i].UpdateFlag:=False;

  NewAllert:=False;

  Q := MetaData.MetadataDB.CreateQuery(qtSelect);
  Q.Descr.BeginUpdate;
  try
    Q.Descr.Table := tblUserTasks;
    Q.Descr.AddParamCondition(fldUTDeleted, ftSmallint, opEQ, 'DID', 0);
    Q.Descr.AddParamCondition(fldUTUserID, ftInteger, opEQ, 'ID', UserSession.ID);
    Q.Descr.AddCondition(fldUTUserID, ftInteger, opIs, Null);
    Q.Descr.AddOperation(opOr);
    Q.Descr.AddOperation(opAnd);
    Q.Descr.AddParamCondition(fldUTAlertTime, ftDateTime, opLT, 'DT', Now);
    Q.Descr.AddOperation(opAnd);
    Q.Descr.AddSortField(fldUTAlertTime);
    Q.Descr.AddFields([fldUTTaskID, fldUTAlertTime, fldUTPeriod, fldUTModifyTime]);
  finally
    Q.Descr.EndUpdate;
  end;
  R := MetaData.MetadataDB.CreateQuery(qtSelect);
  R.Descr.BeginUpdate;
  try
    R.Descr.Table := tblTasks;
    R.Descr.AddParamCondition(fldTasksID, ftInteger, opEQ, 'ID');
    R.Descr.AddFields([fldTasksTableID, fldTasksStrPrimaryKey, fldTasksAlertMsg, fldTasksPriority, fldTasksAuthor]);
  finally
    R.Descr.EndUpdate;
  end;
  A := MetaData.MetadataDB.CreateQuery(qtSelect);
  A.Descr.BeginUpdate;
  try
    A.Descr.Table := tblUsers;
    A.Descr.AddParamCondition(fldUsersID, ftInteger, opEQ, 'ID');
    A.Descr.AddField(fldUsersName);
  finally
    A.Descr.EndUpdate;
  end;

  try
    Q.Open;
    for T:=0 to Pred(Q.RecordCount) do
    begin
      Q.RecNo:= T;
      AllertID := Q.ValueByName[fldUTTaskID];

      Allert:=AllertByID(AllertID);
      if Allert=nil then
        begin
          R.Descr.ParamValueByName['ID'] := AllertID;
          R.Open;

          Allert:=TAllert.Create;
          Allert.AllertID    := AllertID;
          Allert.TableID     := R.ValueByName[fldTasksTableID];
          Allert.PrimaryKey  := R.ValueByName[fldTasksStrPrimaryKey];
          Allert.AllertMesg  := R.StringValueByName(fldTasksAlertMsg);

          if VarIsNull(R.ValueByName[fldTasksPriority]) then Allert.AllertPrior := 2
                                                        else Allert.AllertPrior := R.ValueByName[fldTasksPriority];

          try
            A.Descr.ParamValueByName['ID'] := R.ValueByName[fldTasksAuthor];
            A.Open;
            Allert.AllertAuthor:=A.ValueByName[fldUsersName];
            A.Close;
          except
            Allert.AllertAuthor:=GetTitle('_Dv.noname');
          end;

          TM:=MetaData.GetTableMeta(Allert.TableID);
          if assigned(TM) then Allert.TableName:= TM.Name
                          else Allert.TableName:= EmptyStr;

          R.Close;
          Add(Allert)
        end;

      Allert.AllertFTime := Q.ValueByName[fldUTAlertTime];

      if VarIsNull(Q.ValueByName[fldUTPeriod])     then Allert.AllertTime  := Allert.AllertFTime
                                                   else Allert.AllertTime  := Q.ValueByName[fldUTPeriod];

      if VarIsNull(Q.ValueByName[fldUTModifyTime]) then Allert.AllertMTime := UnAssigned
                                                   else Allert.AllertMTime := Q.ValueByName[fldUTModifyTime];

      if Allert.AllertTime<Now then
        begin
          NewAllert:=True;
          FFocusedAllert:=Allert;
        end;

      Allert.UpdateFlag:=True;
    end;
  except
    {$IFDEF DeDEBUG}
    WriteLog('TAllerts.OnUpdateData error');
    {$ENDIF}
  end;

  A.Free;
  R.Free;
  Q.Free;

  for i:=Count-1 downto 0 do
    if Not items[i].UpdateFlag then Delete(i);

  if (NewAllert)and(FCanActivate) then Show;
end;

function TAllerts.AllertByID( aID: Integer ): TAllert;
var i:Integer;
begin
  Result:=nil;
  for i:=0 to Count-1 do
    if items[i].AllertID=aID then
      begin
        Result:=items[i];
        Exit;
      end;
end;

function TAllerts.GetAllert(aIndex: integer): TAllert;
begin
  result := TAllert(inherited Items[aIndex]);
end;

function TAllerts.HoldOverByID(aAllertID, aMinutes: Integer): Boolean;
var i  : Integer;
    Q  : TDeDataSet;
    DT : TDateTime;
begin
  result := False;
  for i:=0 to Count-1 do
    if items[i].AllertID=aAllertID then
      begin
        if (aMinutes = htHoldDay)or(aMinutes = htHoldWeek) then
          begin
            items[i].AllertTime := Date+1;
            While (aMinutes=htHoldWeek)and(1<DayOfWeek(items[i].AllertTime))do
              items[i].AllertTime:=items[i].AllertTime+1;
          end
        else
          begin
            DT:=Date;
            ReplaceTime(DT,EncodeTime(HourOf(Now),MinuteOf(Now),0,0));
            DT:=Dt+(aMinutes/(24*60));
            items[i].AllertTime := DT;
          end;

        try
          Q := MetaData.MetadataDB.CreateQuery(qtUpdate);
          Q.Descr.BeginUpdate;
          try
            Q.Descr.Table := tblUserTasks;
            Q.Descr.AddParamCondition(fldUTTaskID, ftSmallInt, opEQ, 'ID', aAllertID);
            Q.Descr.AddParamField(fldUTPeriod, ftDateTime);
            Q.Descr.AddParamField(fldUTModifyTime, ftDateTime);
            Q.Descr.ParamValueByName[fldUTPeriod]:= items[i].AllertTime;
            Q.Descr.ParamValueByName[fldUTModifyTime]:= Now;
          finally
            Q.Descr.EndUpdate;
          end;
          Q.ExecuteQuery;
          Q.Free;
          Result:=True;
        except
        end;
      end;
end;

function TAllerts.DeleteByID(aAllertID:Integer; InBase : Boolean = True): Boolean;
var i : Integer;
    Q : TDeDataSet;
begin
  for i:=Count-1 downto 0 do
    if items[i].AllertID=aAllertID then Delete(i);

  if InBase then
    try
      Q := MetaData.MetadataDB.CreateQuery(qtUpdate);
      Q.Descr.BeginUpdate;
      try
        Q.Descr.Table := tblUserTasks;
        Q.Descr.AddCondition(fldUTTaskID, ftSmallInt, opEQ, aAllertID);
        Q.Descr.AddParamField(fldUTModifyTime, ftDateTime);
        Q.Descr.ParamValueByName[fldUTModifyTime]:= Now;
        Q.Descr.AddParamField(fldUTDeleted, ftInteger);
        Q.Descr.ParamValueByName[fldUTDeleted]:= 1;
      finally
        Q.Descr.EndUpdate;
      end;
      Q.ExecuteQuery;
      Q.Free;
      Result:=True;
    except
      Result:=False;
    end
  else
    Result:=True;
end;

//==============================================================================

end.

