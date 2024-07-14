unit StructureUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, {Dialogs, }StdCtrls, ExtCtrls, ComCtrls,
  ActnList, Actions, ToolWin, Contnrs, System.UITypes, System.Generics.Collections,
  LogoForm, DeMeta, DeDB, DataCacheUnit, Vcl.Buttons;

type
  // типы несоответствий
  TTypeCorrect = (tcDelDataset, tcDelField,
                  tcDataType, tcDataSize,
                  tcDatasetName, tcFieldName,
                  tcIsView, tcPrimaryKey, tcNotNull, tcReadOnly, tcUnique,
                  tcForegn, tcDefault,
                  tcDatasetGUID, tcFieldGUID, tcDatasetEmptyGUID, tcFieldEmptyGUID,
                  tcNewDataset, tcNewField,
                  tcInsertDenyDataset, tcUpdateDenyDataset, tcDeleteDenyDataset,
                  tcInsertDenyField, tcUpdateDenyField, tcDeleteDenyField,
                  tcDatasetEmptySchema, tcNone);
  TTypeCorrects = set of TTypeCorrect;

const
  CanMeta: TTypeCorrects = [tcNewDataset, tcIsView, tcNewField, tcDelField,
          tcDataType, tcDataSize, tcPrimaryKey, tcNotNull, tcUnique, tcReadOnly, tcDefault, tcForegn,
          tcDatasetGUID, tcFieldGUID, tcDatasetEmptyGUID, tcFieldEmptyGUID, tcDatasetName, tcFieldName];

  CanDB: TTypeCorrects = [tcDelDataset, tcNewField, tcDelField, tcDataSize, tcNotNull];

  CanProperty: TTypeCorrects = [tcDelDataset, tcIsView, tcDelField,
          tcDataType, tcDataSize, tcPrimaryKey, tcNotNull, tcUnique, tcReadOnly, tcDefault, tcForegn, tcDatasetName, tcFieldName];

type
  TProblem = class
  public
    Code : TTypeCorrect;

    Title: String;
    dValue  : string;
    mValue  : string;

    Ok   : Boolean;
    Color: TColor;

    dTable  : TTableMeta;
    dField  : TFieldMeta;

    mTable  : TTableMeta;
    mFieldOriginal : String;
//  mField  : TFieldMeta; // поскольку поля перечитываются при сохранении
                          // используется непрямая ссылка, а имя
    function CompareWith(aWith: TProblem): Integer;
  end;

type
  TForm_Da_Consistency = class(TFormLogo)
    Panel1: TPanel;
    Panel3: TPanel;
    ToolButton4: TToolButton;
    ActionList1: TActionList;
    CA_dA_Close: TAction;
    CA_Da_Update: TAction;
    CA_Da_Correct: TAction;
    CA_Da_Properties: TAction;
    CA_Da_SelectAll: TAction;
    CA2_Da_Correct: TAction;
    ToolButton8: TToolButton;
    CorrectButton: TToolButton;
    Panel2: TPanel;
    BA_dT_database: TAction;
    BA_dT_metadata: TAction;
    ToolButton1: TToolButton;
    Panel5: TPanel;
    LBox: TListBox;
    Panel4: TPanel;
    Label4: TLabel;
    RB_Dt_MetaData: TRadioButton;
    RB_Dt_DataBase: TRadioButton;
    aPanel: TPanel;
    LabelNote: TLabel;
    ProgressBar: TProgressBar;
    ToolButton2: TToolButton;
    procedure CA_dA_CloseExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CA_Da_UpdateExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CA_Da_PropertiesExecute(Sender: TObject);
    procedure LBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure FormResize(Sender: TObject);
    procedure LBoxClick(Sender: TObject);
    procedure CA_Da_CorrectExecute(Sender: TObject);
    procedure CA2_Da_CorrectExecute(Sender: TObject);
    procedure CA_Da_SelectAllExecute(Sender: TObject);
    procedure BA_dT_databaseExecute(Sender: TObject);
    procedure BA_dT_metadataExecute(Sender: TObject);
  private
    { Private declarations }
    IndexToMeta, IndexToDB: Integer;
    aDB         : TDeCustomDatabase;
    dTL, mTL    : TTablesList;
    FirstShow   : Boolean;
    TableID     : Variant;
    TableName   : String;
    TableSchema : String;
    Z           : TObjectList<TProblem>;

    procedure PrepareColumns(I: Integer);
    procedure AddError(const aCode: TTypeCorrect; aDBTable: TTableMeta = nil; aDBField: TFieldMeta = nil;
                                      aMTTable: TTableMeta = nil; aMTField: TFieldMeta = nil; aColor: TColor = clBlack);
  public
    { Public declarations }
    function Init(varCacheItem: TCacheItem) : Boolean;
  end;

//var
//  Form_Da_Consistency: TForm_Da_Consistency;

implementation

uses Variants, DB, Forms,
     DeLog, DeTypes, Funcs, Dictionary, DataUnit, DeMetadata, QueryDescriptor,
     Security, DataManager, UnitA;

{$R *.dfm}

procedure TForm_Da_Consistency.FormCreate(Sender: TObject);
begin
  inherited;
  FirstShow:=True;
  dTL:= TTablesList.Create;
end;

procedure TForm_Da_Consistency.FormActivate(Sender: TObject);
begin
  inherited;
  update;
  if FirstShow then
    begin
      CA_Da_UpdateExecute(Sender);
      FirstShow:=False;
    end;
end;

procedure TForm_Da_Consistency.FormResize(Sender: TObject);
begin
  lBox.Repaint;
end;

procedure TForm_Da_Consistency.FormDestroy(Sender: TObject);
begin
  Z.Clear;
  dTL.Free;
  inherited;
end;

procedure TForm_Da_Consistency.CA_dA_CloseExecute(Sender: TObject);
begin
  inherited;
  ModalResult:=mrOk;
end;

function TForm_Da_Consistency.Init(varCacheItem: TCacheItem): Boolean;
var aCacheItem  : TCacheItem;
    N,LookupKey : Integer;
begin
  Z:= TObjectList<TProblem>.Create(True);
  LangFormUpdate(self);
  DM.MapActionListIcons(ActionList1);

  IndexToMeta:= DM.MapIconIndex(EncodeIcon(120, 3));
  IndexToDB:= DM.MapIconIndex(EncodeIcon(121, 15));

  dTL.Clear;
  mTL:=MetaData.TablesList;
  aDB:= nil;

  Label4.Caption:='[ '+GetTitle('_Dt.Dataset')+ ' ] . '+GetTitle('_Dt.Field')+ ' : '+GetTitle('_Dl.properties');

  // Определяем базу
  if not assigned(varCacheItem) and (0 < MetaData.DatabasesCount) then
    begin
      aDB:= Metadata.Databases[0];
    end else

  if varCacheItem.Owner.TableMeta = MetaData.MetaTables[idxDataset] then
    try
      TableID:= varCacheItem.ValueByName[fldDataSetId];
      TableName:= varCacheItem.ValueByName[fldDataSetTable];
      if Assigned(varCacheItem.Owner) and Assigned(varCacheItem.Owner.Fields) and (varCacheItem.Owner.Fields.IndexByName(fldDataSetSchema) <> -1)
        then TableSchema:= Trim(varCacheItem.ValueByName[fldDataSetSchema])
        else TableSchema:= EmptyStr;

      BoldCaption.Caption:= GetTitle('_Df.Name: ')+GetTitle(varCacheItem.FieldTextByName[fldDataSetName]);
      NormalCaption.Caption:= GetTitle('_Dt.Dataset: ') + iif(Length(TableSchema)=0, EmptyStr, TableSchema+ '.') + varCacheItem.ValueByName[fldDataSetTable];
        N:=varCacheItem.Owner.Fields.IndexByName(fldDataSetDatabase);
        LookupKey:= varCacheItem.FieldValue[N];

      case LookupKey of
          0: aDB:= Metadata.MetadataDB;
         -1: aDB:= Metadata.CatalogsDB;
         -2: aDB:= Metadata.SampleMTDB;
        else aDB:= Metadata.DatabaseByID(LookupKey);
      end;

    except
      Result:= False;
    end else

  if varCacheItem.Owner.TableMeta = MetaData.MetaTables[idxBase] then
    try
      TableID:= unassigned;
      TableName:= EmptyStr;
      TableSchema:= EmptyStr;
      aCacheItem:= varCacheItem;
      // подстановку значений для базы данных метаструктуры делает TConfigCacheItem
      BoldCaption.Caption:= GetTitle('_Df.Alias: ')+aCacheItem.FieldTextByName[fldBaseAlias];
      NormalCaption.Caption:= GetTitle('_Dt.Database: ')+aCacheItem.FieldTextByName[fldBaseDatabase];

      aDB:= MetaData.CreateDatabase(aCacheItem, False);
      aDB.Connected:= True;
      Result:= aDB.Connected;
      RB_Dt_DataBase.Enabled:= aDB.SupportDDLs = [ddlCreateTable, ddlCreateField, ddlModifyField, ddlDeleteField];

      aCacheItem.Restore;
    except
      Result:= False;
    end else

    begin
      Result:= False;
    end;

  // Подключаем
  try
    aDB.Connected:=True;
    RB_Dt_DataBase.Enabled:= aDB.SupportDDLs = [ddlCreateTable, ddlCreateField, ddlModifyField, ddlDeleteField];
    Result:= aDB.Connected;
  except
    Result:= False;
  end;
end;

Function GetMeta(aObject: TObject; var TM: TTableMeta; var FM: TFieldMeta): Boolean;
begin
  if aObject is TFieldMeta then
      begin
        FM:=TFieldMeta(aObject);
        TM:=FM.Owner;
        Result:=True;
      end else
  if aObject is TTableMeta then
      begin
        FM:=nil;
        TM:=TTableMeta(aObject);
        Result:=True;
      end
  else
      begin
        FM:=nil;
        TM:=nil;
        Result:=False;
      end;
end;

procedure TForm_Da_Consistency.CA_Da_UpdateExecute(Sender: TObject);
var BackupCursor  : TCursor;
    i,j,k,r,Weight : Integer;
    TM  : TTableMeta;
    FM  : TFieldMeta;
    FieldMetaDataSet : TDeDataSet;
    LoginSchema, TableSchema: string;
    //..........................................................................
    procedure DeleteError( i: Integer);
    begin
      Z.Delete(i);
      LBox.Items.Delete(i);
    end;

begin
  BackupCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;

  ProgressBar.Position:= 0;
  LabelNote.Visible:= False;
  ProgressBar.Visible:= True;

  LabelNote.Caption := EmptyStr;
  Z.Clear;
  LBox.Items.Clear;
  LBox.Refresh;

  dTL.Clear;
  aDB.GetMetaTables(dTL);

  //  если сверяем одну таблицу удаляем лишние данные
  if Length(TableName) <> 0 then
    if Length(TableSchema) = 0 then
      begin
        for I := Pred(dTL.Count) downto 0 do
          if not SameText(dTL[I].Table, TableName) then dTL.Delete(I);
      end
    else
      for I := Pred(dTL.Count) downto 0 do
        if not (SameText(dTL[I].Table, TableName) and SameText(dTL[I].Schema, TableSchema)) then dTL.Delete(I);

  if mpMetadataDatasetGUID in Metadata.MetadataPresents then
    dTL.LoadDefaultGUIDs; // Загружаем GUID`ы только для таблиц!

  LoginSchema := aDB.LoginSchema;

  Weight:= 100;
  ProgressBar.Max:= dTL.Count * Weight + mTL.Count;
  //---------------------------------------------------- цикл по базе данных ---
  for i:=0 to Pred(dTL.Count) do
    begin
      ProgressBar.Position:= Weight * i;
      aDB.GetMetaTableInfo(dTL[i]);

      if mpMetadataFieldGUID in Metadata.MetadataPresents then
        dTL[i].Fields.LoadDefaultGUIDs; // Загружаем GUID`ы для полей таблицы!

      TM:=nil;
      for k := 0 to Pred(mTL.Count) do
        begin
          TableSchema := mTL[k].Schema;
          if Length(TableSchema) = 0 then
            TableSchema := LoginSchema;
          if SameText(mTL[k].Table, dTL[i].Table) and (0 = TableID) or (mTL[k].ID = TableID) then
          if assigned(mTL[k].Database) and (mTL[k].Database.ID = aDB.ID) then
            if SameText(TableSchema, LoginSchema) or (Length(TableSchema) = 0) or (Length(LoginSchema) = 0) then
              begin
                TM:=mTL[k];
                dTL[i].ID:=TM.ID;

                if mpMetadataDatasetGUID in Metadata.MetadataPresents then
                  if TM.Database = Metadata.MetadataDB then
                    begin
                      if tsGUID in dTL[i].LoadingState then
                        if tsGUID in TM.LoadingState then
                          begin
                            if not IsEqualGUID(TM.GUID, dTL[i].GUID) then
                              AddError(tcDatasetGUID, dTL[i], nil, TM, nil);
                          end
                        else
                          AddError(tcDatasetGUID, dTL[i], nil, TM, nil);
                    end
                  else
                    if not (tsGUID in TM.LoadingState) then
                      AddError(tcDatasetEmptyGUID, nil, nil, TM, nil);

                for j := 0 to Pred(dTL[i].Fields.Count) do
                  begin
                    FM := TM.Fields.FindByName(dTL[i].Fields[j].Original);
                    if Assigned(FM) then
                      begin
                        if mpMetadataFieldGUID in Metadata.MetadataPresents then
                          if TM.Database = Metadata.MetadataDB then
                            begin
                              if csGUID in dTL[i].Fields[j].FlagState then
                                if csGUID in FM.FlagState then
                                  begin
                                    if not IsEqualGUID(FM.GUID, dTL[i].Fields[j].GUID) then
                                      AddError(tcFieldGUID, dTL[i], dTL[i].Fields[j], TM, FM);
                                  end
                                else
                                  AddError(tcFieldGUID, dTL[i], dTL[i].Fields[j], TM, FM);
                            end
                          else
                            if not (csGUID in FM.FlagState) then
                              AddError(tcFieldEmptyGUID, nil, nil, TM, FM);
                      end
                    else
                      AddError(tcNewField, dTL[i], dTL[i].Fields[j], TM, nil);
                  end;
              end;
        end;

      if Not Assigned(TM) and (Length(Trim(dTL[i].SelectSQL))=0) then
        AddError(tcNewDataset, dTL[i], nil, nil, nil);
    end;

  //----------------------------------------- пропускаем неиспользуемые поля ---
  try
    FieldMetaDataSet := MetaData.MetadataDB.CreateQuery(qtSelect);
    FieldMetaDataSet.Descr.BeginUpdate;
    try
      FieldMetaDataSet.Descr.Table := tblFields;
      FieldMetaDataSet.Descr.AddCondition(fldFieldsDeleted, ftInteger, opEQ, 1);
      FieldMetaDataSet.Descr.AddOperation(opNot);
      FieldMetaDataSet.Descr.AddCondition(fldFieldsVisible, ftInteger, opEQ, -1);
      FieldMetaDataSet.Descr.AddOperation(opAnd);
    finally
      FieldMetaDataSet.Descr.EndUpdate;
    end;

    FieldMetaDataSet.Prepare;

    FieldMetaDataSet.Open;
    for R:=0 to Pred(FieldMetaDataSet.RecordCount) do
      begin
        FieldMetaDataSet.RecNo:= R;
        For i:= Z.Count-1 downto 0 do
          if (Z.Items[i].Code = tcNewField) then
          if (Z.Items[i].mTable.ID = FieldMetaDataSet.ValueByName[fldFieldsTable]) then
          if (Z.Items[i].dField.Original = FieldMetaDataSet.ValueByName[fldFieldsOriginal]) then
             DeleteError(i);
      end;

    FieldMetaDataSet.Close;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('Skip skipped fields error: ' + E.Message);
    {$ENDIF}
  end;

  //-------------------------------------------------- цикл по метаструктуре ---
  for i:=0 to Pred(mTL.Count) do
    if (VarIsEmpty(TableID) and not mTL[i].Deleted) or (mTL[i].ID = TableID) then
      begin
        ProgressBar.Position:=Weight * dTL.Count + i;

        TM:=dTL.FindByTable(mTL[i].Table);
        if Not Assigned(TM) then
          begin
            if assigned(mTL[i].Database) then
              if (mTL[i].Database.ID = aDB.ID) then
                // пропускаем VIEW, при наличии запроса в поле SelectSQL
                if not ((mTL[i].IsReadOnly) and (Length(Trim(mTL[i].SelectSQL))>0)) then
                  AddError(tcDelDataset, nil, nil, mTL[i], nil);
          end
        else
          begin
            if (mTL[i].Table <> TM.Table) and (aDB.DatabaseType in CaseSensitiveDatabaseType) then // Ошибка в регистре
              AddError(tcDatasetName, TM, nil, mTL[i], nil);

            if (mTL[i].IsReadOnly = False) and (TM.IsReadOnly = True) then
              AddError(tcIsView, TM, nil, mTL[i], nil);

            for j:=0 to Pred(mTL[i].Fields.Count) do
              if mTL[i].Fields[j].IsStored and not mTL[i].Fields[j].Deleted then // Пропускаем нехранимые поля
                begin
                  FM:=TM.Fields.FindByName(mTL[i].Fields[j].Original);
                  if Not Assigned(FM) then
                    begin
                      // пропускаем поля VIEW, при наличии запроса в поле SelectSQL
                      if not ((mTL[i].IsReadOnly) and (Length(Trim(mTL[i].SelectSQL))>0)) then
                        AddError(tcDelField, TM, nil, mTL[i], mTL[i].Fields[j]);
                    end
                  else
                    begin
                      if (mTL[i].Fields[j].Original <> FM.Original) and (aDB.DatabaseType in CaseSensitiveDatabaseType) then // Ошибка в регистре
                        AddError(tcFieldName, TM, FM, mTL[i], mTL[i].Fields[j]);

                      if FM.DataType <> mTL[i].Fields[j].DataType then
                        AddError(tcDataType, TM, FM, mTL[i], mTL[i].Fields[j]) else
                                                                              // DataType включает ошибки DataSize
                      if FM.DataSize <> mTL[i].Fields[j].DataSize then
                        AddError(tcDataSize, TM, FM, mTL[i], mTL[i].Fields[j]);

                      if (FM.Link > 0) and (mTL[i].Fields[j].Link = 0) then
                        AddError(tcForegn, TM, FM, mTL[i], mTL[i].Fields[j]);

                      { для View игнорируем ошибку }                     //  FM.*                - META
                      if (Not TM.IsReadOnly) then                        //  mTL[i].Fields[j].*  - БАЗА
                        begin

                          if (not FM.Key) and (mTL[i].Fields[j].Key) then
                            AddError(tcPrimaryKey, TM, FM, mTL[i], mTL[i].Fields[j]);

                          if (FM.NotNull) <> mTL[i].Fields[j].NotNull then
                            AddError(tcNotNull, TM, FM, mTL[i], mTL[i].Fields[j]);

                          if (FM.ReadOnly) <> (mTL[i].Fields[j].ReadOnly) then
                            AddError(tcReadOnly, TM, FM, mTL[i], mTL[i].Fields[j]);

                          if (FM.Unique) and (not mTL[i].Fields[j].Unique) then
                            AddError(tcUnique, TM, FM, mTL[i], mTL[i].Fields[j]);

                          // для ReadOnly полей, FM.Default должно быть пусто, а mTL[i].Fields[j].Default - пофиг
                          if (FM.ReadOnly) and (0 < Length(Trim(FM.DefaultDefDB))) then
                                   begin
                                     mTL[i].Fields[j].DefaultDefDB:= EmptyStr;
                                     AddError(tcDefault, TM, FM, mTL[i], mTL[i].Fields[j]);
                                   end;
                          // Для базы с полями mpFieldDefaultDB
                          if (not FM.ReadOnly) and (mpFieldDefaultDB in Metadata.MetadataPresents) and
                             (Not SameFormula(FM.DefaultDefDB, mTL[i].Fields[j].DefaultDefDB, FM)) then
                                      AddError(tcDefault, TM, FM, mTL[i], mTL[i].Fields[j]);

                          // Для базы без полей mpFieldDefaultDB
                          if (not FM.ReadOnly) and (not (mpFieldDefaultDB in Metadata.MetadataPresents)) and
                             (Not SameFormula(FM.DefaultDefDB, mTL[i].Fields[j].DefaultDefDB, FM)) and
                              (Length(Trim(FM.DefaultDefDB)) > 0) then
                                      AddError(tcDefault, TM, FM, mTL[i], mTL[i].Fields[j]);
                        end;
                    end;
                end    
          end;
      end;
  //----------------------------------------------------------------------------
  if (LBox.Count>0) then
    begin
      LBox.ItemIndex:=0;
      LBox.Selected[0]:=True;
    end;
  LBoxClick(Sender);

  ProgressBar.Visible:=False;
  LabelNote.Visible:=true;
  Screen.Cursor := BackupCursor;
end;

procedure TForm_Da_Consistency.LBoxClick(Sender: TObject);
var s: string;
    fm: TFieldMeta;
begin
  if (LBox.Items.Count=0) or (LBox.ItemIndex<0) then
    begin
      CA_Da_Properties.Enabled:= False;
      CA_Da_Correct.Enabled:= False;
      CA2_Da_Correct.Enabled:= False;
      LabelNote.Caption:= EmptyStr;
    end
  else
    begin
      CA_Da_Properties.Enabled:= Z[LBox.ItemIndex].Code in CanProperty;
      CA_Da_Correct.Enabled:= ((Z[LBox.ItemIndex].Code in CanMeta) and (Not Z[LBox.ItemIndex].Ok)) or (LBox.SelCount>1);
      CA2_Da_Correct.Enabled:= ((Z[LBox.ItemIndex].Code in CanDB) and (Not Z[LBox.ItemIndex].Ok)) or (LBox.SelCount>1);

      if assigned(Z[LBox.ItemIndex].mTable) then
        begin
          if assigned(Z[LBox.ItemIndex].mTable.Database)
            then s:= Z[LBox.ItemIndex].mTable.Database.Alias+' - '
            else s:= EmptyStr;

          s:= s + GetTitle(Z[LBox.ItemIndex].mTable.Name);
          if Length(Z[LBox.ItemIndex].mTable.Filter)>0 then
            s:=s+' ['+Z[LBox.ItemIndex].mTable.Filter+']';
          if length(Z[LBox.ItemIndex].mFieldOriginal)>0 then
            begin
              fm:=Z[LBox.ItemIndex].mTable.GetFieldByName(Z[LBox.ItemIndex].mFieldOriginal);
              if assigned(fm) then s:=s+' . '+fm.Native;
            end;
          LabelNote.Caption:= s;
        end
      else
        LabelNote.Caption:= EmptyStr;
    end;
end;

procedure TForm_Da_Consistency.PrepareColumns(I : Integer);
  function DatasetSecurityString(const DatasetID: Integer; const Operation: TSecurityOperation): string;
  begin
    if Assigned(SecuritySystem) then
      if SecuritySystem.CheckPolicyDataset(DatasetID, Operation) then
        Result := GetTitle('_sC.Grant')
      else
        Result := GetTitle('_sC.Deny')
    else
      Result := EmptyStr;
  end;
var mFM: TFieldMeta;
begin
  if Z[I].Code in [tcDataType, tcDataSize, tcPrimaryKey, tcNotNull, tcReadOnly, tcUnique, tcDefault, tcForegn, tcFieldGUID] then
    mFM:= Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal)
  else
    mFm:= nil;

  case Z[I].Code of
    tcNewDataset: begin
                    Z[I].Title   := '[ '+Z[I].dTable.Table+' ]';
                    Z[I].mValue  := GetTitle('_sC.Missing');
                    Z[I].dValue  := EmptyStr;
                  end;
    tcDelDataset: begin
                    Z[I].Title   := '[ '+Z[I].mTable.Table+' ]';
                    Z[I].mValue  := EmptyStr;
                    Z[I].dValue  := GetTitle('_sC.Missing');
                  end;
    tcIsView:     begin
                    Z[I].Title   := '[ '+Z[I].dTable.Table+' ] : IsView';
                    Z[I].mValue  := BoolToStr(Z[I].mTable.IsReadOnly);
                    Z[I].dValue  := BoolToStr(Z[I].dTable.IsReadOnly);
                  end;
    tcNewField:   begin
                    Z[I].Title   := '[ '+Z[I].dTable.Table+' ] . '+Z[I].dField.Original;
                    Z[I].mValue  := GetTitle('_sC.Missing');
                    Z[I].dValue  := EmptyStr;
                  end;
    tcDelField:   begin
                    Z[I].Title   := '[ '+Z[I].mTable.Table+' ] . '+Z[I].mFieldOriginal;
                    Z[I].mValue  := EmptyStr;
                    Z[I].dValue  := GetTitle('_sC.Missing');
                  end;
    tcDataType:   begin
                    Z[I].Title   := '[ '+Z[I].mTable.Table+' ] . '+Z[I].mFieldOriginal+' : DataType';
                    Z[I].mValue  := FieldTypeNames[mFM.DataType]
                                    + iif( mFM.DataSize = 0, EmptyStr, '('+IntToStr(mFM.DataSize)+')');
                    Z[I].dValue  := FieldTypeNames[Z[I].dField.DataType]
                                    + iif( Z[I].dField.DataSize = 0, EmptyStr, '('+IntToStr(Z[I].dField.DataSize)+')');
                  end;
    tcDataSize:   begin
                    Z[I].Title   := '[ '+Z[I].mTable.Table+' ] . '+Z[I].mFieldOriginal+' : DataSize';
                    Z[I].mValue  := IntToStr(mFM.DataSize);
                    Z[I].dValue  := IntToStr(Z[I].dField.DataSize);
                  end;
    tcPrimaryKey: begin
                    Z[I].Title   := '[ '+Z[I].mTable.Table+' ] . '+Z[I].mFieldOriginal+' : PrimaryKey';
                    Z[I].mValue  := BoolToStr(mFM.Key);
                    Z[I].dValue  := BoolToStr(Z[I].dField.Key);
                  end;
    tcNotNull:    begin
                    Z[I].Title   := '[ '+Z[I].mTable.Table+' ] . '+Z[I].mFieldOriginal+' : NotNull';
                    Z[I].mValue  := BoolToStr(mFM.NotNull);
                    Z[I].dValue  := BoolToStr(Z[I].dField.NotNull);
                  end;
    tcReadOnly:   begin
                    Z[I].Title   := '[ '+Z[I].mTable.Table+' ] . '+Z[I].mFieldOriginal+' : ReadOnly';
                    Z[I].mValue  := BoolToStr(mFM.ReadOnly);
                    Z[I].dValue  := BoolToStr(Z[I].dField.ReadOnly);
                  end;
    tcUnique:     begin
                    Z[I].Title   := '[ '+Z[I].mTable.Table+' ] . '+Z[I].mFieldOriginal+' : tcUnique';
                    Z[I].mValue  := BoolToStr(mFM.Unique);
                    Z[I].dValue  := BoolToStr(Z[I].dField.Unique);
                  end;
    tcDefault:    begin
                    Z[I].Title   := '[ '+Z[I].mTable.Table+' ] . '+Z[I].mFieldOriginal+' : Default';
                    Z[I].mValue  := ReduceCaption(mFM.DefaultDefDB, 24);
                    Z[I].dValue  := ReduceCaption(Z[I].dField.DefaultDefDB, 24);
                  end;
    tcForegn:     begin
                    Z[I].Title   := '[ '+Z[I].mTable.Table+' ] . '+Z[I].mFieldOriginal+' : ForegnKey';
                    Z[I].mValue  := '-';
                    if assigned(Z[I].dField.LinkTable) then
                      Z[I].dValue  := Z[I].dField.LinkTable.Table;
                  end;
    tcDataSetName:begin
                    Z[I].Title   := '[ '+Z[I].dTable.Table+' ] : CaseSensivite';
                    Z[I].mValue  := Z[I].mTable.Table;
                    Z[I].dValue  := Z[I].dTable.Table;
                  end;
    tcFieldName:  begin
                    Z[I].Title   := '[ '+Z[I].mTable.Table+' ] . '+Z[I].mFieldOriginal+' : CaseSensivite';
                    Z[I].mValue  := mFM.Original;
                    Z[I].dValue  := Z[I].dField.Original;
                  end;
    tcDatasetGUID:begin
                    if tsGUID in Z[I].mTable.LoadingState then
                      begin
                        Z[I].Title   := '[ ' + Z[I].mTable.Table + ' ] : GUID';
                        Z[I].mValue  := GetTitle('_sC.Incorrect');
                        Z[I].dValue  := GetTitle('_sC.Correct');
                      end
                    else
                      begin
                        Z[I].Title   := '[ ' + Z[I].mTable.Table + ' ] : GUID';
                        Z[I].mValue  := GetTitle('_sC.Missing');
                        Z[I].dValue  := GetTitle('_sC.Correct');
                      end
                  end;
    tcDatasetEmptyGUID:
                  begin
                    Z[I].Title   := '[ ' + Z[I].mTable.Table + ' ] : GUID';
                    Z[I].mValue  := GetTitle('_sC.Missing');
                    Z[I].dValue  := EmptyStr;
                  end;
    tcFieldGUID:  begin
                    if csGUID in mFM.FlagState then
                      begin
                        Z[I].Title   := '[ ' + Z[I].mTable.Table + ' ] . ' + Z[I].mFieldOriginal + ' : GUID';
                        Z[I].mValue  := GetTitle('_sC.Incorrect');
                        Z[I].dValue  := GetTitle('_sC.Correct');
                      end
                    else
                      begin
                        Z[I].Title   := '[ ' + Z[I].mTable.Table + ' ] . ' + Z[I].mFieldOriginal + ' : GUID';
                        Z[I].mValue  := GetTitle('_sC.Missing');
                        Z[I].dValue  := GetTitle('_sC.Correct');
                      end
                  end;
    tcFieldEmptyGUID:
                  begin
                    Z[I].Title   := '[ ' + Z[I].mTable.Table + ' ] . ' + Z[I].mFieldOriginal + ' : GUID';
                    Z[I].mValue  := GetTitle('_sC.Missing');
                    Z[I].dValue  := EmptyStr;
                  end;
    tcInsertDenyDataset:
                  begin
                    Z[I].Title   := '[ ' + Z[I].dTable.Table + ' ]: INSERT [ ' + MetaData.MetaTables[idxDataset].Table + ' ]';
                    Z[I].mValue := DatasetSecurityString(MetaData.MetaTables[idxDataset].ID, spInsert);
                    Z[I].dValue := GetTitle('_sC.Deny');
                  end;
    tcUpdateDenyDataset:
                  begin
                    Z[I].Title   := '[ ' + Z[I].dTable.Table + ' ]: UPDATE [ ' + MetaData.MetaTables[idxDataset].Table + ' ]';
                    Z[I].mValue := DatasetSecurityString(MetaData.MetaTables[idxDataset].ID, spUpdate);
                    Z[I].dValue := GetTitle('_sC.Deny');
                  end;
    tcDeleteDenyDataset:
                  begin
                    Z[I].Title  := '[ ' + Z[I].dTable.Table + ' ]: DELETE [ ' + MetaData.MetaTables[idxDataset].Table + ' ]';
                    Z[I].mValue := DatasetSecurityString(MetaData.MetaTables[idxDataset].ID, spDelete);
                    Z[I].dValue := GetTitle('_sC.Deny');
                  end;
    tcInsertDenyField:
                  begin
                    Z[I].Title   := '[ ' + Z[I].dTable.Table + ' ] . ' + Z[I].dField.Original + ' : INSERT [ ' + MetaData.MetaTables[idxFields].Table + ' ]';
                    Z[I].mValue := DatasetSecurityString(MetaData.MetaTables[idxFields].ID, spInsert);
                    Z[I].dValue := GetTitle('_sC.Deny');
                  end;
    tcUpdateDenyField:
                  begin
                    Z[I].Title   := '[ ' + Z[I].mTable.Table + ' ] . ' + Z[I].mFieldOriginal + ' : UPDATE [ ' + MetaData.MetaTables[idxFields].Table + ' ]';
                    Z[I].mValue := DatasetSecurityString(MetaData.MetaTables[idxFields].ID, spUpdate);
                    Z[I].dValue := GetTitle('_sC.Deny');
                  end;
    tcDeleteDenyField:
                  begin
                    Z[I].Title  := '[ ' + Z[I].mTable.Table + ' ] . ' + Z[I].mFieldOriginal + ' : DELETE [ ' + MetaData.MetaTables[idxFields].Table + ' ]';
                    Z[I].mValue := DatasetSecurityString(MetaData.MetaTables[idxFields].ID, spDelete);
                    Z[I].dValue := GetTitle('_sC.Deny');
                  end;
  end;
end;

procedure TForm_Da_Consistency.LBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var R: Integer;
    ValueColor: TColor;
begin

  if  (odFocused in State) or ( odSelected in State) then
    begin
      LBox.Canvas.Brush.Color:= clActiveCaption;
      LBox.Canvas.Font.Color:= clCaptionText;
      ValueColor:= clCaptionText;
    end
  else
    begin
      LBox.Canvas.Brush.Color:= clWindow;
      LBox.Canvas.Font.Color:=clBlack;
      ValueColor:= Z[Index].Color;
    end;

  LBox.Canvas.Pen.Color:=LBox.Canvas.Brush.Color;
  LBox.Canvas.Rectangle(Rect);

  if(Z[Index].Ok) then LBox.Canvas.Font.Style:=[fsStrikeOut];
  LBox.Canvas.TextOut(4,  Rect.Top+1, LBox.Items[Index]);
  R:=LBox.Canvas.TextWidth(Z[Index].mValue);

  LBox.Canvas.Font.Color:= ValueColor;
  LBox.Canvas.TextOut(Rect.Right-230-(r div 2), Rect.Top+1, Z[Index].mValue);
  R:=LBox.Canvas.TextWidth(Z[Index].dValue);
  LBox.Canvas.TextOut(Rect.Right- 70-(r div 2), Rect.Top+1, Z[Index].dValue);

  // разделитель
  if (odFocused in State) or ( odSelected in State) then

  if BA_dT_database.Checked then DM.ilIcon16.Draw(LBox.Canvas, Rect.Right-158, Rect.Top+1, IndexToDB)
                            else DM.ilIcon16.Draw(LBox.Canvas, Rect.Right-158, Rect.Top+1, IndexToMeta)
  else
  if BA_dT_database.Checked then DM.ilIcon08.Draw(LBox.Canvas, Rect.Right-154, Rect.Top+5, IndexToDB)
                            else DM.ilIcon08.Draw(LBox.Canvas, Rect.Right-154, Rect.Top+5, IndexToMeta);
end;

procedure TForm_Da_Consistency.CA_Da_PropertiesExecute(Sender: TObject);
var n: Integer;
    mFM: TfieldMeta;
begin
  N:=LBox.ItemIndex;
  if N<0 then Exit;

  mFM:= Z[N].mTable.GetFieldByName(Z[N].mFieldOriginal);

  if assigned(mFM) then
    EditRecord( MetaData.MetaTables[idxFields],  mFM.ID) else
  if assigned(Z[N].mTable) then
    EditRecord( MetaData.MetaTables[idxDataset], Z[N].mTable.ID);

  PrepareColumns(LBox.ItemIndex);
end;

procedure TForm_Da_Consistency.AddError(const aCode: TTypeCorrect; aDBTable: TTableMeta = nil; aDBField: TFieldMeta = nil;
                                      aMTTable: TTableMeta = nil; aMTField: TFieldMeta = nil; aColor: TColor = clBlack);
var i,N: Integer;
    P: TProblem;
begin
  P:= TProblem.Create;
  P.Code := aCode;
  P.Ok   := False;
  P.dTable := aDBTable;
  P.dField := aDBField;
  P.mTable := aMTTable;
  if assigned(aMTField) then P.mFieldOriginal:= aMTField.Original
                        else P.mFieldOriginal:= EmptyStr;

  case P.Code of
    tcDelDataset, tcDelField, tcDataType, tcDataSize: P.Color:= clRed;

    tcPrimaryKey: if aDBField.Key then P.Color:= clRed
                                  else if aDBField.DataType = ftAutoinc then P.Color:= clSilver
                                                                        else P.Color:= clBlack;
    tcNotNull:  if aDBField.NotNull then P.Color:= clRed
                                    else P.Color:= clSilver;
    tcUnique:   if aDBField.Unique then P.Color:= clRed
                                   else P.Color:= clSilver;
    tcReadOnly: if aMTField.Key and aMTField.ReadOnly
                  then if (0 = Length(Trim(aDBField.DefaultDefDB))) and (aDBField.DataType <> ftAutoinc)
                         then P.Color:= clRed
                         else P.Color:= clSilver
                  else if aMTField.ReadOnly
                         then P.Color:= clSilver
                         else P.Color:= clBlack;

    tcDataSetName, tcFieldName: P.Color:= clNavy;

    tcNewDataset, tcNewField: P.Color:= clSilver;
    tcDefault:  if assigned(aMTField) then
                  begin
                    if aMTField.NotNull then
                      begin
                        if (0 < Length(Trim(aDBField.DefaultDefDB))) and (0 = Length(Trim(aMTField.DefaultDefDB)))
                          then P.Color:= clRed
                          else P.Color:= clSilver;
                      end
                    else
                      begin
                        if (0 < Length(Trim(aDBField.DefaultDefDB))) and (0 < Length(Trim(aMTField.DefaultDefDB)))
                          then P.Color:= clNavy
                          else P.Color:= clSilver;
                      end;
                  end;
    tcForegn:   P.Color:= clSilver;

    else P.Color:= clBlack;
  end;

  N:= -1;
  for i:=0 to Pred(z.Count) do
    if Z.Items[i].CompareWith(P) < 0 then
      begin
        N:= i;
        Break;
      end;

  if N= -1 then
    begin
      N:= Z.Count;
      Z.Add(P);
      PrepareColumns(N);
      LBox.AddItem(Z[N].Title, Z[N]);
    end
  else
    begin
      Z.Insert(N, P);
      PrepareColumns(N);
      LBox.Items.InsertObject(N, Z[N].Title, Z[N]);
    end;

  LBox.ItemIndex:= 0;

  LiteCaption.Caption := GetTitle('_De.Error : ',ttSecondName) + IntToStr(LBox.Count);
  LiteCaption.Update;
  LBox.Update;
end;

procedure TForm_Da_Consistency.BA_dT_databaseExecute(Sender: TObject);
begin
  inherited;
  BA_dT_database.Checked:=True;
  BA_dT_Metadata.Checked:=False;
  CorrectButton.Action:= CA2_Da_Correct;
  LBox.Repaint;//
end;

procedure TForm_Da_Consistency.BA_dT_metadataExecute(Sender: TObject);
begin
  inherited;
  BA_dT_database.Checked:= False;
  BA_dT_Metadata.Checked:= True;
  CorrectButton.Action:= CA_dA_Correct;
  LBox.Invalidate; //
end;

procedure TForm_Da_Consistency.CA_Da_CorrectExecute(Sender: TObject);
var I,J,N,K : Integer;
    FDMan : TFieldsDataManager;
    DDMan : TDatasetDataManager;
    FCache: TDataCache;
    FCacheItem: TCacheItem;
    mFM : TFieldMeta;
begin
  for I:=0 to LBox.Items.Count-1 do
  if Not Z[I].Ok then
  if (LBox.Selected[I]) or ((LBox.SelCount=0) and (I=LBox.ItemIndex)) then
  begin
    case Z[I].Code of
      tcNewDataset: try
                      if dpInsert in MetaData.MetaTables[idxDataset].Permissions then
                        begin
                          DDMan:= TDatasetDataManager.Create;
                          FCache:= TDataCache.Create(MetaData.MetaTables[idxDataset]);
                          FCacheItem:= FCache.AddNewItem;
                          Z[I].dTable.AssignToCacheItem(FCacheItem);
                          FCacheItem.FieldValue[FCacheItem.Owner.Fields.IndexByName(fldDataSetDatabase)]:=aDB.ID;
                          FCacheItem.FieldValue[FCacheItem.Owner.Fields.IndexByName(fldDataSetGViewType)]:=1;
                          // Проставляем Solution по умолчанию у нового dataset`а ...
                          FCacheItem.FieldValue[FCacheItem.Owner.Fields.IndexByName(fldDataSetSolution)]:= MetaData.ReadDefaultSolution;
                          DDMan.PrepareRecord(FCacheItem);
                          if not DDMan.InsertRecord(FCacheItem) then
                            SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25,
                                                NativeInt(PChar(DDMan.Errors.GetMessage)));
                          Z[I].dTable.ID :=FCacheItem.ID;
                          FCache.Free;
                          DDMan.Free;
                          if Z[I].dTable.Fields.Count <> 0 then
                            if dpInsert in MetaData.MetaTables[idxFields].Permissions then
                              for J:=0 to Pred(Z[I].dTable.Fields.Count) do
                                begin
                                  FDMan:= TFieldsDataManager.Create;
                                  FCache:= TDataCache.Create(MetaData.MetaTables[idxFields]);
                                  FCacheItem:= FCache.AddNewItem;
                                  Z[I].dTable.Fields[J].Owner:= Z[I].dTable;
                                  Z[I].dTable.Fields[J].AssignToCacheItem(FCacheItem);
                                  FCacheItem.ValueByName[fldFieldsOrder] := J; // Переопределяем порядок по умолчанию ...
                                  FDMan.PrepareRecord(FCacheItem);
                                  if not FDMan.InsertRecord(FCacheItem) then
                                    SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25,
                                                        NativeInt(PChar(FDMan.Errors.GetMessage)));
                                  FCache.Free;
                                  FDMan.Free;
                                end
                            else
                              AddError(tcInsertDenyField, Z[I].dTable, Z[I].dTable.Fields[0]);
                        end
                      else
                        AddError(tcInsertDenyDataset, Z[I].dTable);
                      Z[I].Ok := True;
                    except
                      {$IFDEF DeDEBUG}
                      on E: Exception do
                        Funcs.WriteLog('New dataset skip error:' + E.Message, True, 'Metadata');
                      {$ENDIF}
                    end;
      tcDataSetName:begin
                      if dpUpdate in MetaData.MetaTables[idxDataset].Permissions then
                        begin
                          Z[I].mTable.SetTable:= Z[I].dTable.Table;
                          Z[I].mTable.Save;
                        end
                      else
                        AddError(tcUpdateDenyDataset, Z[I].mTable);
                      Z[I].Ok := True;
                    end;
      tcIsView:     begin
                      if dpUpdate in MetaData.MetaTables[idxDataset].Permissions then
                        begin
                          Z[I].mTable.IsReadOnly := Z[I].dTable.IsReadOnly;
                          Z[I].mTable.Save;
                        end
                      else
                        AddError(tcUpdateDenyDataset, Z[I].mTable);
                      Z[I].Ok := True;
                    end;
      tcNewField:   try
                      if dpInsert in MetaData.MetaTables[idxFields].Permissions then
                        begin
                          FDMan:= TFieldsDataManager.Create;
                          FCache:= TDataCache.Create(MetaData.MetaTables[idxFields]);
                          FCacheItem:= FCache.AddNewItem;
                          Z[I].dField.Owner:=Z[I].mTable;
                          Z[I].dField.AssignToCacheItem(FCacheItem);

                          FCacheItem.ID:= unassigned;
                          N:=  0;
                          K:= -1;
                          for J:=0 to Pred(Z[I].dField.Owner.Fields.Count) do
                            if N <= Z[I].dField.Owner.Fields[J].Order then
                               begin
                                 K:= J;
                                 N:= Z[I].dField.Owner.Fields[J].Order + 1;
                               end;
                          // в последнем поле увеличиваем порядок, чтобы было увеличение при вставке нескольких полей
                          if -1 < K then
                            Z[I].dField.Owner.Fields[K].Order:= N;

                          FCacheItem.ValueByName[fldFieldsOrder] := N;
                          FDMan.PrepareRecord(FCacheItem);
                          if not FDMan.InsertRecord(FCacheItem) then
                            SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 25, NativeInt(PChar(FDMan.Errors.GetMessage)));
                          FCache.Free;
                          FDMan.Free;
                        end
                      else
                        AddError(tcInsertDenyField, Z[I].dTable, Z[I].dField);
                      Z[I].Ok:= True;
                    except
                      {$IFDEF DeDEBUG}
                      on E: Exception do
                        Funcs.WriteLog('New field skip error:' + E.Message, True, 'Metadata');
                      {$ENDIF}
                    end;
      tcDelField:   begin
                      mFM := Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal);
                      if Assigned(Z[I].mTable.DField) then
                        if dpUpdate in MetaData.MetaTables[idxFields].Permissions then
                          begin
                            mFM.Deleted := True;
                            mFM.Save;
                          end
                        else
                          AddError(tcUpdateDenyField, Z[I].dTable, Z[I].dField, Z[I].mTable, mFM)
                      else
                        if dpDelete in MetaData.MetaTables[idxFields].Permissions then
                          begin
                            mFM.Deleted := True;
                            mFM.Save;
                          end
                        else
                          AddError(tcDeleteDenyField, Z[I].dTable, Z[I].dField, Z[I].mTable, mFM);
                      Z[I].Ok := True;
                    end;
      tcFieldName:  begin
                      mFM := Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal);
                      if dpUpdate in MetaData.MetaTables[idxFields].Permissions then
                        begin
                          mFM.Original := Z[I].dField.Original;
                          mFM.Save;
                        end
                      else
                        AddError(tcUpdateDenyField, Z[I].dTable, Z[I].dField, Z[I].mTable, mFM);
                      Z[I].Ok := True;
                    end;
      tcDataType:   begin
                      mFM := Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal);
                      if dpUpdate in MetaData.MetaTables[idxFields].Permissions then
                        begin
                          mFM.DataType := Z[I].dField.DataType;
                          mFM.DataSize:= Z[I].dField.DataSize;
                          mFM.Save;
                        end
                      else
                        AddError(tcUpdateDenyField, Z[I].dTable, Z[I].dField, Z[I].mTable, mFM);
                      Z[I].Ok := True;
                    end;
      tcDataSize:   begin
                      mFM := Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal);
                      if dpUpdate in MetaData.MetaTables[idxFields].Permissions then
                        begin
                          mFM.DataSize:= Z[I].dField.DataSize;
                          mFM.Save;
                        end
                      else
                        AddError(tcUpdateDenyField, Z[I].dTable, Z[I].dField, Z[I].mTable, mFM);
                      Z[I].Ok := True;
                    end;
      tcPrimaryKey: begin
                      mFM := Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal);
                      if dpUpdate in MetaData.MetaTables[idxFields].Permissions then
                        begin
                          mFM.Key := Z[I].dField.Key;
                          mFM.Save;
                        end
                      else
                        AddError(tcUpdateDenyField, Z[I].dTable, Z[I].dField, Z[I].mTable, mFM);
                      Z[I].Ok := True;
                    end;
      tcNotNull:    begin
                      mFM := Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal);
                      if dpUpdate in MetaData.MetaTables[idxFields].Permissions then
                        begin
                          mFM.NotNull := Z[I].dField.NotNull;
                          mFM.Save;
                        end
                      else
                        AddError(tcUpdateDenyField, Z[I].dTable, Z[I].dField, Z[I].mTable, mFM);
                      Z[I].Ok := True;
                    end;
      tcReadOnly:   begin
                      mFM := Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal);
                      if dpUpdate in MetaData.MetaTables[idxFields].Permissions then
                        begin
                          mFM.ReadOnly := Z[I].dField.ReadOnly;
                          mFM.Save;
                        end
                      else
                        AddError(tcUpdateDenyField, Z[I].dTable, Z[I].dField, Z[I].mTable, mFM);
                      Z[I].Ok := True;
                    end;
      tcUnique:     begin
                      mFM := Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal);
                      if dpUpdate in MetaData.MetaTables[idxFields].Permissions then
                        begin
                          mFM.Unique := Z[I].dField.Unique;
                          mFM.Save;
                        end
                      else
                        AddError(tcUpdateDenyField, Z[I].dTable, Z[I].dField, Z[I].mTable, mFM);
                      Z[I].Ok := True;
                    end;
      tcDefault:    begin
                      mFM := Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal);
                      if dpUpdate in MetaData.MetaTables[idxFields].Permissions then
                        begin
                          mFM.DefaultDefDB:= Z[I].dField.DefaultDefDB;
                          mFM.Save;
                        end
                      else
                        AddError(tcUpdateDenyField, Z[I].dTable, Z[I].dField, Z[I].mTable, mFM);
                      Z[I].Ok := True;
                    end;
      tcForegn:   begin
                      mFM := Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal);
                      if dpUpdate in MetaData.MetaTables[idxFields].Permissions then
                        begin
                          mFM.Link := Z[I].dField.Link;
                          mFM.Save;
                        end
                      else
                        AddError(tcUpdateDenyField, Z[I].dTable, Z[I].dField, Z[I].mTable, mFM);
                      Z[I].Ok := True;
                    end;
      tcDatasetGUID:begin
                      if dpUpdate in MetaData.MetaTables[idxDataset].Permissions then
                        begin
                          Z[I].mTable.GUID := Z[I].dTable.GUID;
                          Z[I].mTable.Save;
                        end
                      else
                        AddError(tcUpdateDenyDataset, Z[I].mTable);
                      Z[I].Ok := True;
                    end;
      tcDatasetEmptyGUID:
                    begin
                      if dpUpdate in MetaData.MetaTables[idxDataset].Permissions then
                        begin
                          Z[I].mTable.GUID := NewGUID;
                          Z[I].mTable.Save;
                        end
                      else
                        AddError(tcUpdateDenyDataset, Z[I].mTable);
                      Z[I].Ok := True;
                    end;
      tcFieldGUID:  begin
                      mFM := Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal);
                      if dpUpdate in MetaData.MetaTables[idxFields].Permissions then
                        begin
                          mFM.GUID := Z[I].dField.GUID;
                          mFM.Save;
                        end
                      else
                        AddError(tcUpdateDenyField, Z[I].dTable, Z[I].dField, Z[I].mTable, mFM);
                      Z[I].Ok := True;
                    end;
      tcFieldEmptyGUID:
                    begin
                      mFM:= Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal);
                      if dpUpdate in MetaData.MetaTables[idxFields].Permissions then
                        begin
                          mFM.GUID := NewGUID;
                          mFM.Save;
                        end
                      else
                        AddError(tcUpdateDenyField, Z[I].dTable, Z[I].dField, Z[I].mTable, mFM);
                      Z[I].Ok := True;
                    end;
    end;
    PrepareColumns(I);
    LBox.Invalidate;
  end;
end;

procedure TForm_Da_Consistency.CA2_Da_CorrectExecute(Sender: TObject);
var I : Integer;
    mFM : TFieldMeta;
begin
  for I:=0 to LBox.Items.Count-1 do
    if Not Z[I].Ok then
      if (LBox.Selected[I]) or ((LBox.SelCount=0) and (I=LBox.ItemIndex)) then
        begin
          case Z[I].Code of
            tcDelDataset: begin
                            // Create Table in DB
                            Z[I].Ok:= Z[I].mTable.Database.CreateTable(Z[I].mTable);
                          end;
            tcNewField:   begin
                            // Delete Field in DB
                            Z[I].Ok:= Z[I].mTable.Database.DeleteField(Z[I].dField);
                          end;
            tcDelField:   begin
                            // Create Field in DB
                            Z[I].Ok:= Z[I].mTable.Database.CreateField(Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal));
                          end;
            tcDataSize:   begin
                            mFM:=Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal);
                            Z[I].dField.DataSize:= mFM.DataSize;
                            Z[I].Ok:= Z[I].mTable.Database.ModifyField(Z[I].dField);
                          end;
            tcNotNull:    begin
                            mFM:=Z[I].mTable.GetFieldByName(Z[I].mFieldOriginal);
                            Z[I].dField.NotNull:= mFM.NotNull;
                            Z[I].Ok:= Z[I].mTable.Database.ModifyField(Z[I].dField);
                          end;
          end;
          PrepareColumns(I);
        end;
end;

procedure TForm_Da_Consistency.CA_Da_SelectAllExecute(Sender: TObject);
begin
  LBox.SelectAll;
end;

{ TProblem }

function TProblem.CompareWith(aWith: TProblem): Integer;
begin
  Result:=0;
  if (Result = 0) and assigned(dTable) and assigned(aWith.dTable) then
    Result:= CompareStr(aWith.dTable.Table, dTable.Table);

  if Color <> aWith.Color then
    begin
      if (Color = clRed)         then Result:= +1 else
      if (aWith.Color = clRed)   then Result:= -1 else
      if (Color = clNavy)        then Result:= +1 else
      if (aWith.Color = clNavy)  then Result:= -1 else
      if (Color = clBlack)       then Result:= +1 else
      if (aWith.Color = clBlack) then Result:= -1;
    end;

  if (Result = 0) and (Code <> aWith.Code) then
    if Ord(Code)<Ord(aWith.Code) then Result:= -1
                                 else Result:=  1;

  if (Result = 0) and assigned(dField) and assigned(aWith.dField) then
    Result:= CompareStr(aWith.dField.Original, dField.Original);
end;

{$IFDEF DEBUG}
initialization
  DebugLog('StructureUnit unit initialization ...');

finalization
  DebugLog('StructureUnit unit finalization ...');
{$ENDIF}

end.

