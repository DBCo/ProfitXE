unit ActionUnit;
{
 -= поддерживаемые действия =-
 OnLogin
 OnLogout
 OnInsert
 OnDelete
 OnUpdate
 OnChange
 ByHand

 -= поддерживаемые процедуры =-
 CreateMessage
 DeleteMessages
 UpdateMessage
 Say
 RefreshPeriodicalMessage
 OpenURL
 SendMail
 SendMessageToList
}
interface

uses DataCacheUnit;

//основная глобальная процедура вызова действия
//для глобального действия используется aCacheItem = nil
function DoAct(const ActionName: string; aCacheItem: TCacheItem): Integer;

implementation

uses Windows, Classes, SysUtils, Variants, Dialogs, ShellAPI, DateUtils, DB, Forms, Controls,
     DeLog, Funcs, DeMetadata, Security, DeScript, Dictionary, DeMeta, DeDB,
     DeParser, DeTypes, QueryDescriptor, SendListUnit, DataUnit;

var Form_SendList: TForm_SendList;

//*****************************************************************************/
function ExtractPart(var aString: String; aSuffix: String): String;
var p: Integer;
begin
  p:=pos(aSuffix,aString);
  if p>0 then
    begin
      Result := copy(aString, 1, P-1);
      Delete(aString, 1, P);
    end
  else
      Result:= EmptyStr;
end;

function GetAlertBeforeTime(s: string) : Double;
var days,hours,mins,secs : string;
    dt  : TDateTime;
    i   : integer;
    d   : double;
    Hour, Min, Sec, MSec :word;
begin
  s := UpperCase(s);
  for i:=0 to Length(s) do
    if s[i]<'0' then s[i] := FormatSettings.DecimalSeparator;

  d := StrToFloatDef(s,-1);
  if d<>-1 then
  begin
    DecodeTime(Frac(d), Hour, Min, Sec, MSec);
    s := Format('%dD%dH%dM%dS',[Trunc(d), Hour, Min, Sec]);
  end;

  //на входе cтрока типа xDxHxMxS: соответственно дни, часы, минуты, секунды
  days := ExtractPart( s, 'D' );
  hours:= ExtractPart( s, 'H' );
  mins := ExtractPart( s, 'M' );
  secs := ExtractPart( s, 'S' );

  try
    if not TryEncodeTime(StrToIntDef(hours,0),StrToIntDef(mins,0),
                         StrToIntDef(secs,0), 0, dt) then dt := 0;
    Result := IncDay(dt, StrToIntDef(days,0));
  except
    on EConvertError do Result := 0;
    {$IFDEF DEBUG}
    on E: Exception do DebugLog('GetAlertBeforeTime error: ' + E.Message);
    {$ENDIF}
  end;
end;

//*****************************************************************************/

function DeleteMessage(aCacheItem: TCacheItem; ConditionID : integer) : integer;
var TaskID,R : integer;
    Q,Q2,Q3  : TDeDataSet;
begin
  Result := ACTION_OK;
  try
    // подготавливаем список записей из "M_TASKS"
    Q := MetaData.MetadataDB.CreateQuery(qtSelect);
    try
      Q.Descr.BeginUpdate;
      try
        Q.Descr.Table := tblTasks;
        Q.Descr.AddCondition(fldTasksDeleted, ftSmallInt, opNE, 1);
        if ConditionID>0 then
          begin
            Q.Descr.AddCondition(fldTasksConditionID, ftSmallInt, opEQ, ConditionID);
            Q.Descr.AddOperation(opAnd);
          end;
        if Assigned(aCacheItem) then
          begin
            Q.Descr.AddCondition(fldTasksTableID, ftInteger, opEQ, aCacheItem.Owner.TableMeta.ID);
            Q.Descr.AddOperation(opAnd);
            Q.Descr.AddCondition(fldTasksStrPrimaryKey, ftString, opEQ, aCacheItem.KeyAsQuotedStr);
            Q.Descr.AddOperation(opAnd);
          end;
      finally
        Q.Descr.EndUpdate;
      end;
      Q.Open;

      //подготавливаем запрос списка пользовательских сообщений
      Q2 := MetaData.MetadataDB.CreateQuery(qtUpdate, Q);
      try
        Q2.Descr.BeginUpdate;
        try
          Q2.Descr.Table := tblUserTasks;
          Q2.Descr.AddParamCondition(fldUTTaskID, ftInteger, opEQ, 'aParam');
          Q2.Descr.AddField(fldUTModifyTime, ftDateTime, Now);
          Q2.Descr.AddField(fldUTDeleted, ftInteger, 1);
        finally
          Q2.Descr.EndUpdate;
        end;
        for R:=0 to Pred(Q.RecordCount) do
          try
            Q.RecNo:= R;
            TaskID := Q.ValueByName[fldTasksID];

            Q2.Descr.ParamValueByName['aParam'] := TaskID;
            Q2.ExecuteQuery;
            if Assigned(Rem) then
              Rem.DeleteByID(TaskID, False);

          except
            on E: Exception do
              begin
                {$IFDEF DEBUG}
                DebugLog('DeleteMessage error: ' + E.Message);
                {$ENDIF}
                Result := ACTION_FAILED;
              end;
          end;

      finally
        Q2.Free;
      end;
      Q.Close;

      // Удаляем задачи
      Q3 := MetaData.MetadataDB.CreateQuery(qtUpdate);
      try
        Q3.Descr.BeginUpdate;
        try
          Q3.Descr.Assign(Q.Descr);
          Q3.Descr.AddField(fldTasksDeleted, ftInteger, 1);
        finally
          Q3.Descr.EndUpdate;
        end;
        Q3.ExecuteQuery;
      finally
        Q3.Free;
      end;

    finally
      Q.Free;
    end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('DeleteMessage error: ' + E.Message);
        {$ENDIF}
        Result := ACTION_FAILED;
      end;
  end;
end;

//*****************************************************************************/
function CalculateValue(aCacheItem: TCacheItem; aValue: String; aVarType: TVarType; var aOk: Boolean): Variant;
var Parser   : TDeParser;
    Postfix  : TExpressionItem;
begin
  Parser    := TDeParser.Create;
  Postfix   := TExpressionItem.Create;
  aOk       := True;

  try
    if Assigned(aCacheItem) then
      begin
        Parser.Table := aCacheItem.Owner.TableMeta;
        Parser.Parse(aValue, Postfix);
        Result := VarAsType(   aCacheItem.Calculate(Postfix, null), aVarType)
      end
    else
      begin
        Parser.Parse(aValue, Postfix);
        Result := VarAsType(DM.Calculator.Calculate(Postfix      ), aVarType);
      end
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('CalculateValue error: ' + E.Message);
        {$ENDIF}
        aOk:=False;
      end;
  end;

  Postfix.Free;
  Parser.Free;
end;

function CreateMessage(aCacheItem: TCacheItem; aParam: String; aID: Integer) : integer;
var s,t : string;
    SList : TStringList;
    TasksCache, UserTasksCache : TDataCache;
    TaskItem, UserTaskItem : TCacheItem;
    d : double;
    key : variant;
    UserId : Integer;
    Q  : TDeDataSet;
    Ok : Boolean;
begin
  Result := ACTION_FAILED;
  if aCacheItem.Deleted                              then Exit;
  if not Assigned(MetaData.MetaTables[idxUserTasks]) then Exit;
  if not Assigned(MetaData.MetaTables[idxTasks])     then Exit;

  Result := ACTION_OK;
  TasksCache := TDataCache.Create(MetaData.MetaTables[idxTasks]);
  UserTasksCache := TDataCache.Create(MetaData.MetaTables[idxUserTasks]);

  SList := TStringList.Create;
  SList.CommaText := aParam;

  TasksCache.AppendRecord(key);
  TaskItem := TasksCache.FindById(key);

  // определяем текст сообщения ................................................
  s := SList.Values['AUTHOR'];
  if s<>EmptyStr then TaskItem.ValueByName[fldTasksAuthor] := s;

  s:=CalculateValue(aCacheItem, SList.Values['TEXT'], varString, Ok);
  if Not Ok then
    begin
      if Length(SList.Values['TEXT'])>0 then s:=SList.Values['TEXT']
                                        else s:=GetTitle('_Da.remember');
    end;

  t:=CalculateValue(aCacheItem, SList.Values['CAPTION'], varString, Ok);
  if Not Ok then
    t:=aCacheItem.Caption;

  TaskItem.ValueNativeByName[fldTasksAlertMsg] := s+' : '+t;

  // определяем время срабатывания напоминания .................................
  TaskItem.ValueByName[fldTasksCreateTime] := DateTimeToStr(Now);
  TaskItem.ValueByName[fldTasksTableID] := aCacheItem.Owner.TableMeta.ID;

    d :=CalculateValue(aCacheItem, SList.Values['TIME'], varDate, Ok);
  if (d=0) or (Not Ok) then
    d :=CalculateValue(aCacheItem, SList.Values['ALTERTIME'], varDate, Ok);
  if (d=0) or (Not Ok) then
    d:=Now;

  // обрабатываем, если это памятная дата ......................................
  s := UpperCase(SList.Values['ISDOB']);
  if s='TRUE' then
  begin
    while d<Now do
      d := IncYear(d,1);
    TaskItem.ValueByName[fldTasksDeadTime] := d + 1;//до конца дня
  end;

  TaskItem.ValueByName[fldTasksStrPrimaryKey] := aCacheItem.KeyAsQuotedStr;
  TaskItem.ValueByName[fldTasksConditionID]   := aID;

  //вставляем запись в кэш
  if not TasksCache.UpdateRecord(TaskItem) then
  begin
    Result := ACTION_FAILED;
    TasksCache.DeleteRecord(TaskItem);
    SList.Free;
    TasksCache.Free;
    UserTasksCache.Free;
    Exit;
  end;

  UserTasksCache.AppendRecord(key);
  UserTaskItem := UserTasksCache.FindById(key);
  UserTaskItem.ValueByName[fldUTTaskID] := TaskItem.ValueByName[fldTasksID];

  // определяем адресата сообщения, в случае ошибки отправляем "всем" ..........
  if Length(SList.Values['USERID'])=0 then
    begin
      UserId := CalculateValue(aCacheItem, SList.Values['WORKERID'], varInteger, Ok);

      Q := MetaData.MetadataDB.CreateQuery(qtSelect);
      try
        Q.Descr.BeginUpdate;
        try
          Q.Descr.Table := tblUsers;
          Q.Descr.AddCondition(fldUsersWorkerID, ftInteger, opEQ, Result);
          // 27.08.2015 + Куфенко: Читаем только необходимые поля!
          Q.Descr.AddField(fldUsersID);
          // 27.08.2015 -
        finally
          Q.Descr.EndUpdate;
        end;
        Q.Open;
        if Q.RecordCount>0 then
          UserId := Q.ValueByName[fldUsersID];
        Q.Close;
      finally
        Q.Free;
      end;
    end
  else
    begin
      UserId := CalculateValue(aCacheItem, SList.Values['USERID'], varInteger, Ok);
    end;

  if UserId<=0 then
    UserTaskItem.ValueByName[fldUTUserID] := null
  else
    UserTaskItem.ValueByName[fldUTUserID] := UserId;

  //............................................................................
  s := SList.Values['ALERTBEFORE'];
  if s<>EmptyStr then d := d - GetAlertBeforeTime(s);
  UserTaskItem.ValueByName[fldUTAlertTime] := d;

  s := UpperCase(SList.Values['PERIOD']);
  if s<>EmptyStr then
    begin
      if s='Y' then UserTaskItem.ValueByName[fldUTPeriod] := 365;
      if s='M' then UserTaskItem.ValueByName[fldUTPeriod] := 30;
      if s='W' then UserTaskItem.ValueByName[fldUTPeriod] := 7;
      if s='D' then UserTaskItem.ValueByName[fldUTPeriod] := 1;
      if StrToIntDef(s,0)>0 then
                    UserTaskItem.ValueByName[fldUTPeriod] := StrToIntDef(s,0);
    end;

  //вставляем запись в кэш
  if not UserTasksCache.UpdateRecord(UserTaskItem) then
    begin
      Result := ACTION_FAILED;
      UserTasksCache.DeleteRecord(UserTaskItem);
      TasksCache.DeleteRecord(TaskItem);
    end;

  //чистим за собой память
  UserTasksCache.Free;
  TasksCache.Free;
  SList.Free;
end;

//*****************************************************************************/

function Say(aParam: String; aCacheItem: TCacheItem) : integer;
begin
  ShowMessage(aParam);
  Result := ACTION_OK;
end;

//*****************************************************************************/

function UpdateMessage(aCacheItem: TCacheItem; aParam: String; aID: Integer): integer;
begin
  Result := ACTION_OK;
  DeleteMessage(aCacheItem, aID);
  CreateMessage(aCacheItem, aParam, aID );
end;

//*****************************************************************************/

function RefreshPeriodicalMessage(param: string; aCacheItem: TCacheItem; ConditionID : integer) : integer;
//Обновление времени срабатывания периодического сообщения
//aCacheItem - пользовательское сообщение
var
  TaskCache, ActionCache, ElemCache : TDataCache;
  TableMetaID : integer;
  KeyValues : TStringList;
begin
  TaskCache := TDataCache.Create(MetaData.MetaTables[idxTasks]);
  TaskCache.Filters.NewFilter(MetaData.MetaTables[idxTasks].Fields.FindByName(fldTasksID), opEQ, VarToInt(aCacheItem.ValueByName[fldUTTaskID]));
  TaskCache.LoadData;
  if TaskCache.Count = 0 then
  begin
    TaskCache.Free;
    Result := ACTION_FAILED;
    Exit;
  end;
  ConditionID := TaskCache.Items[0].ValueByName[fldTasksConditionID];

  ActionCache := TDataCache.Create(MetaData.MetaTables[idxActionConditions]);
  ActionCache.Filters.NewFilter(MetaData.MetaTables[idxActionConditions].Fields.FindByName(fldACID), opEQ, ConditionID);
  ActionCache.LoadData;
  if ActionCache.Count = 0 then
  begin
    ActionCache.Free;
    TaskCache.Free;
    Result := ACTION_FAILED;
    Exit;
  end;
  param := ActionCache.Items[0].ValueByName[fldACProcParam];
  ActionCache.Free;
  TableMetaID := TaskCache.Items[0].ValueByName[fldTasksTableID];
  KeyValues := TStringList.Create;
  KeyValues.CommaText := TaskCache.Items[0].ValueByName[fldTasksStrPrimaryKey];
  TaskCache.Free;

  if TableMetaID = EmptyTableMetaID then
  begin
    DeleteMessage(nil, ConditionID);
    Result := CreateMessage( nil, param, ConditionID);
  end
  else
  begin
    //находим тело элемента по идент. таблицы и набору ключевых значений
    ElemCache := TDataCache.Create(MetaData.GetTableMeta(TableMetaID));
    if ElemCache.TableMeta<>nil then
    begin
      ElemCache.Filters.NewFilter(TTableMeta(MetaData.GetTableMeta(TableMetaID)).KField[0], opEQ, KeyValues.Strings[0]);
      ElemCache.LoadData;
      if ElemCache.Count<>0 then
      begin
        DeleteMessage(ElemCache.Items[0], ConditionID);
        Result := CreateMessage( ElemCache.Items[0], param, ConditionID);
      end
      else Result := ACTION_FAILED;
    end
    else Result := ACTION_FAILED;
    ElemCache.Free;
  end;
  KeyValues.Free;
end;

//*****************************************************************************/

function SendMail(param: string; aCacheItem: TCacheItem; ConditionID : integer) : integer;
var
  mail_addr : string;
begin
  Result := ACTION_OK;
  if aCacheItem.Owner.Fields.IndexByName(param) <> -1 then
    mail_addr := aCacheItem.ValueByName[param]
  else
    mail_addr := param;
  ShellExecute(0, nil, PChar('mailto:' + mail_addr), nil, nil, SW_SHOWNORMAL);
end;

//*****************************************************************************/

function SendMessageToList(const param: string; aCacheItem: TCacheItem; ConditionID : integer) : integer;
var
  SList : TStringList;
  s, tmps, comments : string;
  len,i : integer;
  userid : integer;
  TasksCache : TDataCache;
  prKey : string;
  Filter : TFilterItem;
  Parser : TDeParser;
  Postfix : TExpressionItem;
begin
  Result := ACTION_OK;
  SList := TStringList.Create;
  prKey := aCacheItem.KeyAsQuotedStr;

  TasksCache := TDataCache.Create(MetaData.MetaTables[idxTasks]);

  Filter := TFilterItem.Create;
  //на идентификатор условия
  Filter.AddCondition( TasksCache.Fields.FindByName(fldTasksConditionID), opEQ, ConditionID);
  //на идентификатор таблицы
  Filter.AddCondition( TasksCache.Fields.FindByName(fldTasksTableID), opEQ, aCacheItem.Owner.TableMeta.ID);
  //на первичный ключ записи
  Filter.AddCondition( TasksCache.Fields.FindByName(fldTasksStrPrimaryKey), opEQ, prKey);
  Filter.AddOperation(opAnd);
  Filter.AddOperation(opAnd);

  TasksCache.LoadData(Filter);
  if TasksCache.Count>0 then
  begin
    TasksCache.Free;
    SList.Free;
    Exit;
  end
  else TasksCache.Free;

  SList.CommaText := param;
  if not Assigned(Form_SendList) then Application.CreateForm(TForm_SendList, Form_SendList);
  //определяем внешний вид формы
  comments := UTF8ToString(RawByteString(SList.Values['REASONMSG']));
  Form_SendList.lReason.Caption := comments;
  s := SList.Values['NECREC'];
  if s<>EmptyStr then
  begin
    Parser := TDeParser.Create;
    Parser.Table := aCacheItem.Owner.TableMeta;

    len := 1;
    for i:=1 to length(s) do
      if s[i]=',' then inc(len);
    inc(len);//так как разделителей на один меньше, чем операндов
    SetLength(Form_SendList.NecessaryRecipients, len);
    for i:=0 to len-1 do
    begin
      if pos(',',s)>0 then
      begin
        tmps := copy(s,1,pos(',',s)-1);
        delete(s,1,pos(',',s));
      end
      else
        tmps := s;

      tmps := trim(tmps);

      //////// нужно пробежаться парсером по tmps //////////

      Postfix := TExpressionItem.Create;
      try
        Parser.Parse(tmps, Postfix);
      except
        on E: EDeParserError do {$IFDEF DEBUG}DebugLog('SendMessageToList skip error:' + E.Message){$ENDIF};
        {$IFDEF DEBUG}
        on E: Exception do
          begin
            DebugLog('SendMessageToList error: ' + E.Message);
            raise;
          end;
        {$ENDIF}
      end;

      if assigned(aCacheItem) then tmps := VarAsType(   aCacheItem.Calculate(Postfix, tmps ), varString)
                              else tmps := VarAsType(DM.Calculator.Calculate(Postfix), varString);
      Postfix.Free;

      try
        Form_SendList.NecessaryRecipients[i] := StrToInt(tmps);
      except
        on EConvertError do Form_SendList.NecessaryRecipients[i] := -1;
        {$IFDEF DEBUG}
        on E: Exception do
          begin
            DebugLog('SendMessageToList error: ' + E.Message);
            raise;
          end;
        {$ENDIF}
      end;
    end;
    Parser.Free;
  end;
  if Form_SendList.ShowModal <> mrOk then
  begin
    SList.Free;
    Exit;
  end;

  for i:=0 to Form_SendList.mComments.Lines.Count-1 do
    comments := comments + ' ' + Form_SendList.mComments.Lines[i];
  //создаем сообщения этого экшена
  for i := 0 to Form_SendList.RecipientList.Items.Count-1 do
    //если был выбран адресат в списке
    if Form_SendList.RecipientList.Items[i].Checked then
    begin
      //получаем его пользовательский индекс
      userid := StrToInt(Form_SendList.RecipientList.Items[i].SubItems[0]);
      //создаем нужную строку параметров s
      s :=     '"ALERTMSG=' + comments + '"';
      s := s + ', "ALERTTIME=' + DateTimeToStr(Now) + '"';
      s := s + ', "AUTHOR=' + IntToStr(UserSession.ID) + '"';
      s := s + ', "USERID=' + IntToStr(userid) + '"';
      //создаем сообщение
      CreateMessage( aCacheItem, WideStringToUnicode(s), ConditionID);
    end;

  //заносим (конкотенируем) комментарии в нужное поле
  s := SList.Values['COMFIELD'];
  if aCacheItem.Owner.Fields.IndexByName(s)<>-1 then
  begin
    if not VarIsNull(aCacheItem.ValueByName[s]) then
      tmps := UTF8ToString(RawByteString(aCacheItem.ValueByName[s]))
    else tmps := EmptyStr;
    tmps := tmps + ' ' + comments;
    aCacheItem.ValueNativeByName[s] := tmps;
    aCacheItem.Owner.UpdateRecord(aCacheItem);
  end;
  SList.Free;
end;

//*****************************************************************************/

function ExecuteProcedure(aCacheItem: TCacheItem; ProcName: string; ProcParam: string; ConditionID : integer): integer;
begin
  Result := PROCEDURE_NOT_FOUND;
  ProcName := UpperCase(ProcName);

  if ProcName = 'CREATEMESSAGE'  then
    begin
      DeleteMessage(aCacheItem, ConditionID);
      Result := CreateMessage(aCacheItem, ProcParam, ConditionID);
    end;
  if ProcName = 'DELETEMESSAGE'  then Result := DeleteMessage(aCacheItem, -1);
  if ProcName = 'UPDATEMESSAGE'  then Result := UpdateMessage(aCacheItem, ProcParam, ConditionID);
  if ProcName = 'SHOWMESSAGE'    then Result := Say(ProcParam, aCacheItem);
  if ProcName = 'REFRESHPERIODICALMESSAGE'  then
    Result := RefreshPeriodicalMessage(ProcParam, aCacheItem, ConditionID);
  if ProcName = 'SENDMAIL'       then Result := SendMail(ProcParam, aCacheItem, ConditionID);
  if ProcName = 'SENDMESSAGETOLIST' then Result := SendMessageToList(ProcParam, aCacheItem, ConditionID);
end;

//*****************************************************************************/

procedure UpdateActionConditions(ActionName: string; aCacheItem: TCacheItem);
var i       : integer;
    tableID : integer;
begin
  if Not (ActionName = 'OnChange') then Exit;

  if not VarIsNull(aCacheItem.ValueByName[fldACTableMetaID]) then
      begin
        if VarAsType(aCacheItem.ValueByName[fldACTableMetaID], varInteger) = EmptyTableMetaID then
          GlobalActionConditionList.Refresh
      end
    else
      if not VarIsNull(aCacheItem.ValueByName[fldACTableMetaID]) then
        begin
          tableID := VarAsType(aCacheItem.ValueByName[fldACTableMetaID], varInteger);

          for i:=0 to Metadata.TableCount-1 do
            if Metadata.Tables[i].ID = tableID then
              begin
                Metadata.Tables[i].ActionConditionList.Refresh;
                Break;
              end;
        end;
end;

function DoAct(const ActionName: string; aCacheItem: TCacheItem): integer;
var i       : integer;
    M       : TEventMeta;
begin
  Result := ACTION_NAME_NOT_FOUND;
  // Exception in 'Gift' database!
  if not Assigned(GlobalActionConditionList) then Exit;

  // ищем в глобальном списке
  for i:=0 to GlobalActionConditionList.Count-1 do
    if SameText(GlobalActionConditionList.Items[i].EventName, ActionName) then
      begin
        Result := ACTION_OK;

        M := GlobalActionConditionList.Items[i];
        if VarAsType( DM.Calculator.Calculate(M.ConditionPostfix) , varBoolean) then
          Result := ExecuteProcedure( aCacheItem, M.ProcedureName, M.ProcedureParameter, M.ID );
      end;

  if Not Assigned(aCacheItem) then Exit;

  // обновляем список действий если это таблица действий
  if (aCacheItem.Owner.TableMeta.Table = tblActionConditions) then
    UpdateActionConditions(ActionName, aCacheItem);

  // ищем в локальном списке
  if assigned(aCacheItem.Owner.TableMeta.ActionConditionList) then
    for i:=0 to aCacheItem.Owner.TableMeta.ActionConditionList.Count-1 do
      if SameText(aCacheItem.Owner.TableMeta.ActionConditionList.Items[i].EventName, ActionName) then
        begin
          Result := ACTION_OK;

          M := aCacheItem.Owner.TableMeta.ActionConditionList.Items[i];
          if VarAsType( aCacheItem.Calculate(M.ConditionPostfix, True) , varBoolean) then
            Result := ExecuteProcedure( aCacheItem, M.ProcedureName, M.ProcedureParameter, M.ID );
        end;
end;

//*****************************************************************************/

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  Form_SendList := nil;
  {$IFDEF DEBUG}
  DebugLog('ActionUnit unit initialization ...');
  {$ENDIF}
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('ActionUnit unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

