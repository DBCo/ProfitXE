{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}

unit uMapLayers;

interface

uses Windows, Classes, Contnrs, SysUtils, Graphics, uMapUtils, DeMeta;

type
  TMapLayerStage = (mlsNone, mlsInserted, mlsModified, mlsDeleted);

  TMapLayer = class
  private
    { Private declarations }
    FLayerID: Integer;
    FUserID: Integer;
    FDatasetID: Integer;
    FName: string;
    FStage: TMapLayerStage;
    procedure SetName(const Value: string);
    procedure SetUserID(const Value: Integer);
    procedure SaveToDatabase;
  public
    { Public declarations }
    MP: TMapProperties;
    procedure MarkDeleted;
    property LayerID: Integer read FLayerID;
    property UserID: Integer read FUserID write SetUserID;
    property DatasetID: Integer read FDatasetID;
    property Name: string read FName write SetName;
    property Stage: TMapLayerStage read FStage;
  end;

  TMapLayers = class
  private
    { Private declarations }
    FList: TObjectList;
    FSelected: TMapLayer;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TMapLayer;
    function GetModified: Boolean;
    function GetSelected: TMapLayer;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromDatabase(const UserID, DatasetID: Integer);
    procedure LoadSystemDefaults;
    procedure SaveToDatabase;
    procedure CreateDefaultLayer;
    function SelectLayer(const Index: Integer): Boolean;
    function LoadLayer(const KeyID: Variant; TableMeta: TTableMeta; FieldMeta: TFieldMeta; var MP: TMapProperties): Boolean;
    function IndexByID(const LayerID: Integer): Integer;
    function IndexByName(const LayerName: String): Integer;
    function NewLayer(SourceLayer: TMapLayer = nil): TMapLayer;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TMapLayer read GetItem; default;
    property Selected: TMapLayer read GetSelected;
    property Modified: Boolean read GetModified;
  end;

  TMapTilePoolJob = class
  private
    { Private declarations }
    FSource: string;
    FTileRow: Integer;
    FTileCol: Integer;
    FTilePriority: Integer;
    FNormalizeTileRow: Integer;
    FNormalizeTileCol: Integer;
    FMapScale: TMapScale;
    FBackgroundColor: TColor;
    FTileURL: string;
    FTileDirectory: string;
    FThread: TThread;
  public
    { Public declarations }
    constructor Create(const aSource: string; const ATileCol, ATileRow, ANormalizeTileCol, ANormalizeTileRow, ATilePriority: Integer;
                    const AMapScale: TMapScale; const ABackgroundColor: TColor; const ATileURL, ATileDirectory: string);
    property Source: string read FSource;
    property MapScale: TMapScale read FMapScale;
    property TileRow: Integer read FTileRow;
    property TileCol: Integer read FTileCol;
    property TilePriority: Integer read FTilePriority;
    property TileURL: string read FTileURL;
    property TileDirectory: string read FTileDirectory;
    property BackgroundColor: TColor read FBackgroundColor;
    property NormalizeTileRow: Integer read FNormalizeTileRow;
    property NormalizeTileCol: Integer read FNormalizeTileCol;
    property Thread: TThread read FThread write FThread;
  end;

  TMapTilePoolThread = class(TThread)
  private
    { Private declarations }
    FSource: string;
    FTileRow: Integer;
    FTileCol: Integer;
    FTilePriority: Integer;
    FNormalizeTileRow: Integer;
    FNormalizeTileCol: Integer;
    FMapScale: TMapScale;
    FBackgroundColor: TColor;
    FTileURL: string;
    FTileDirectory: string;
    FFileName: string;
    FErrorMessage: string;
    FTempFileName: string;
  protected
    { Protected declarations }
    //procedure ConvertImageSinchronized;
    procedure Execute; override;
  public
    { Public declarations }
    constructor Create(AJob: TMapTilePoolJob);
    property Source: String read FSource;
    property MapScale: TMapScale read FMapScale;
    property TileRow: Integer read FTileRow;
    property TileCol: Integer read FTileCol;
    property TilePriority: Integer read FTilePriority;
    property NormalizeTileRow: Integer read FNormalizeTileRow;
    property NormalizeTileCol: Integer read FNormalizeTileCol;
    property BackgroundColor: TColor read FBackgroundColor;
    property FileName: string read FFileName;
    property ErrorMessage: string read FErrorMessage;
  end;

implementation

uses Variants, DB, IOUtils, UrlMon, PngImage, Jpeg, GIFImg, Math, StrUtils,
     DeTypes, DeMetadata, DeDB, QueryDescriptor, Funcs, DeTemplate,
     DeLog, DeControls;

{ TMapLayer }

procedure TMapLayer.SetName(const Value: string);
begin
  FName := Value;
  if FStage = mlsNone then FStage := mlsModified;
end;

procedure TMapLayer.SetUserID(const Value: Integer);
begin
  FUserID := Value;
  if FStage = mlsNone then FStage := mlsModified;
end;

procedure TMapLayer.SaveToDatabase;
var
  Query: TDeDataset;
  NewLayerID: Integer;
begin
  case FStage of
    mlsInserted: { Новая запись }
      begin
        Query := MetaData.MetadataDB.CreateQuery(qtHole);
        try
          if Assigned(Query) then
            begin
              Query.Descr.BeginUpdate;
              try
                Query.Descr.Table := tblUserDataSet;
                Query.Descr.AddField(fldUDataSetID);
              finally
                Query.Descr.EndUpdate;
              end;
              Query.Open;
              NewLayerID := Query.IntValueDef(0);
            end
          else
           begin
             Query := MetaData.MetadataDB.CreateQuery(qtRow);
              Query.Descr.BeginUpdate;
              try
                Query.Descr.Table := tblUserDataSet;
                Query.Descr.AddField(opMax, fldUDataSetID);
              finally
                Query.Descr.EndUpdate;
              end;
              Query.Open;
              NewLayerID := Query.IntValueDef(0);
           end;
          Inc(NewLayerID);
        finally
          Query.Free;
        end;
        // Добавим новую запись ...
        Query := MetaData.MetadataDB.CreateQuery(qtInsert);
        try
          Query.Descr.BeginUpdate;
          try
            Query.Descr.Table := tblUserDataSet;
            Query.Descr.AddParamField(fldUDataSetID, ftInteger);
            Query.Descr.AddParamField(fldUDataSetType, ftInteger);
            if FDatasetID <> 0 then
              Query.Descr.AddParamField(fldUDataSetTable, ftInteger);
            if FUserID <> 0 then
              Query.Descr.AddParamField(fldUDataSetSubject, ftInteger);
            Query.Descr.AddParamField(fldUDataSetName, ftString);
            Query.Descr.AddParamField(fldUDataSetXML, ftString);
          finally
            Query.Descr.EndUpdate;
          end;
          Query.Descr.ParamValueByName[fldUDataSetID] := NewLayerID;
          Query.Descr.ParamValueByName[fldUDataSetType] := 8;
          if FDatasetID <> 0 then
            Query.Descr.ParamValueByName[fldUDataSetTable] := FDatasetID;
          if FUserID <> 0 then
            Query.Descr.ParamValueByName[fldUDataSetSubject] := FUserID;
          if Length(FName) = 0 then
            Query.Descr.ParamValueByName[fldUDataSetName] := Null
          else
            Query.Descr.ParamValueByName[fldUDataSetName] := FName;
          if (MP.TileDeadInHours >= 0) or (MP.Coord <> mctDefault) then
            Query.Descr.ParamValueByName[fldUDataSetXML] := '<map url="' + MP.Source + '" timeout="' + IntToStr(MP.TileDeadInHours) + '" type="' + MP.GetCoordName + '" />'
          else
            Query.Descr.ParamValueByName[fldUDataSetXML] := MP.Source;
          Query.ExecuteQuery;
          FLayerID := NewLayerID;
        finally
          Query.Free;
        end;
        FStage := mlsNone;
      end;
    mlsModified: { Изменённая запись }
      begin
        // Обновим запись ...
        Query := MetaData.MetadataDB.CreateQuery(qtUpdate);
        try
          Query.Descr.BeginUpdate;
          try
            Query.Descr.Table := tblUserDataSet;
            Query.Descr.AddParamField(fldUDataSetType, ftInteger);
            Query.Descr.AddParamField(fldUDataSetTable, ftInteger);
            Query.Descr.AddParamField(fldUDataSetSubject, ftInteger);
            Query.Descr.AddParamField(fldUDataSetName, ftString);
            Query.Descr.AddParamField(fldUDataSetXML, ftString);
            Query.Descr.AddParamCondition(fldUDataSetID, ftInteger, opEQ, fldUDataSetID, FLayerID);
          finally
            Query.Descr.EndUpdate;
          end;
          Query.Descr.ParamValueByName[fldUDataSetType] := 8;
          if FDatasetID = 0 then
            Query.Descr.ParamValueByName[fldUDataSetTable] := Null
          else
            Query.Descr.ParamValueByName[fldUDataSetTable] := FDatasetID;
          if FUserID = 0 then
            Query.Descr.ParamValueByName[fldUDataSetSubject] := Null
          else
            Query.Descr.ParamValueByName[fldUDataSetSubject] := FUserID;
          if Length(FName) = 0 then
            Query.Descr.ParamValueByName[fldUDataSetName] := Null
          else
            Query.Descr.ParamValueByName[fldUDataSetName] := FName;
          if (MP.TileDeadInHours >= 0) or (MP.Coord <> mctDefault) then
            Query.Descr.ParamValueByName[fldUDataSetXML] := '<map url="' + MP.Source + '" timeout="' + IntToStr(MP.TileDeadInHours) + '" type="' + MP.GetCoordName + '" />'
          else
            Query.Descr.ParamValueByName[fldUDataSetXML] := MP.Source;
          Query.ExecuteQuery;
        finally
          Query.Free;
        end;
        FStage := mlsNone;
      end;
    mlsDeleted: { Удалённая запись }
      begin
        // Удалим запись ...
        Query := MetaData.MetadataDB.CreateQuery(qtDelete);
        try
          Query.Descr.BeginUpdate;
          try
            Query.Descr.Table := tblUserDataSet;
            Query.Descr.AddParamCondition(fldUDataSetID, ftInteger, opEQ, 'ID', FLayerID);
          finally
            Query.Descr.EndUpdate;
          end;
          Query.ExecuteQuery;
        finally
          Query.Free;
        end;
      end;
  end;
end;

procedure TMapLayer.MarkDeleted;
begin
  FStage := mlsDeleted;
end;

{ TMapLayers }

constructor TMapLayers.Create;
begin
  FList := TObjectList.Create;
end;

destructor TMapLayers.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TMapLayers.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMapLayers.GetItem(const Index: Integer): TMapLayer;
begin
  Result := FList[Index] as TMapLayer;
end;

function TMapLayers.GetModified: Boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to Pred(Count) do
    if Items[Index].Stage <> mlsNone then
      begin
        Result := True;
        Break;
      end;
end;

function TMapLayers.GetSelected: TMapLayer;
begin
  if Not assigned(FSelected) then
    FSelected:= Items[0];
  Result:= FSelected;
end;

procedure TMapLayers.LoadFromDatabase(const UserID, DatasetID: Integer);
var
  R: Integer;
  Query: TDeDataset;
  Layer: TMapLayer;
  Value: string;
  Template: TDeTemplate;
  Node: TDeNode;
begin
  FList.Clear;
  Query := MetaData.MetadataDB.CreateQuery(qtSelect);
  try
    Query.Descr.BeginUpdate;
    try
      Query.Descr.Table := tblUserDataSet;
      Query.Descr.AddField(fldUDataSetID);
      Query.Descr.AddField(fldUDataSetSubject);
      Query.Descr.AddField(fldUDataSetTable);
      Query.Descr.AddField(fldUDataSetName);
      Query.Descr.AddField(fldUDataSetXML);
      Query.Descr.AddParamCondition(fldUDataSetType, ftSmallInt, opEQ, fldUDataSetType, 8);
      if UserID = 0 then
        if DatasetID = 0 then
          begin
            // Ничего не указано ...
            Query.Descr.AddCondition(fldUDataSetTable, ftInteger, opIs, Null);
            Query.Descr.AddCondition(fldUDataSetSubject, ftInteger, opIs, Null);
            Query.Descr.AddOperation(opAnd);
            Query.Descr.AddOperation(opAnd);
          end
        else
          begin
            // Указан только датасет ...
            Query.Descr.AddCondition(fldUDataSetTable, ftInteger, opIs, Null);
            Query.Descr.AddCondition(fldUDataSetSubject, ftInteger, opIs, Null);
            Query.Descr.AddOperation(opAnd);
            Query.Descr.AddParamCondition(fldUDataSetTable, ftInteger, opEQ, fldUDataSetTable, DatasetID);
            Query.Descr.AddCondition(fldUDataSetSubject, ftInteger, opIs, Null);
            Query.Descr.AddOperation(opAnd);
            Query.Descr.AddOperation(opOr);
            Query.Descr.AddOperation(opAnd);
            Query.Descr.AddSortField(fldUDataSetTable, sdDescending);
          end
      else
        if DatasetID = 0 then
          begin
            // Указан только пользователь ..
            Query.Descr.AddCondition(fldUDataSetTable, ftInteger, opIs, Null);
            Query.Descr.AddCondition(fldUDataSetSubject, ftInteger, opIs, Null);
            Query.Descr.AddOperation(opAnd);
            Query.Descr.AddCondition(fldUDataSetTable, ftInteger, opIs, Null);
            Query.Descr.AddParamCondition(fldUDataSetSubject, ftInteger, opEQ, fldUDataSetSubject, UserID);
            Query.Descr.AddOperation(opAnd);
            Query.Descr.AddOperation(opOr);
            Query.Descr.AddOperation(opAnd);
            Query.Descr.AddSortField(fldUDataSetSubject, sdDescending);
          end
        else
          begin
            // Указан и пользователь и датасет ...
            Query.Descr.AddCondition(fldUDataSetTable, ftInteger, opIs, Null);
            Query.Descr.AddCondition(fldUDataSetSubject, ftInteger, opIs, Null);
            Query.Descr.AddOperation(opAnd);
            Query.Descr.AddCondition(fldUDataSetTable, ftInteger, opIs, Null);
            Query.Descr.AddParamCondition(fldUDataSetSubject, ftInteger, opEQ, 'SUID', UserID);
            Query.Descr.AddOperation(opAnd);
            Query.Descr.AddOperation(opOr);
            Query.Descr.AddParamCondition(fldUDataSetTable, ftInteger, opEQ, 'SDID', DatasetID);
            Query.Descr.AddCondition(fldUDataSetSubject, ftInteger, opIs, Null);
            Query.Descr.AddOperation(opAnd);
            Query.Descr.AddParamCondition(fldUDataSetTable, ftInteger, opEQ, 'HDID', DatasetID);
            Query.Descr.AddParamCondition(fldUDataSetSubject, ftInteger, opEQ, 'HUID', UserID);
            Query.Descr.AddOperation(opAnd);
            Query.Descr.AddOperation(opOr);
            Query.Descr.AddOperation(opOr);
            Query.Descr.AddOperation(opAnd);
            Query.Descr.AddSortFields([fldUDataSetSubject, fldUDataSetTable], sdDescending);
          end;
      Query.Descr.AddSortField(fldUDataSetOrder, sdAscending);
    finally
      Query.Descr.EndUpdate;
    end;
    Query.Open;
    for R:=0 to Pred(Query.RecordCount) do
      begin
        Query.RecNo:= R;
        Layer := TMapLayer.Create;
        if FList.Add(Layer) <> -1 then
          begin
            Layer.FLayerID := Query.IntValueDef(0);   // fldUDataSetID
            Layer.FUserID := Query.IntValueDef(1);    // fldUDataSetSubject;
            Layer.FDatasetID := Query.IntValueDef(2); // fldUDataSetTable
            Layer.FName := Query.StringValue(3);      // fldUDataSetName
            Value := Query.StringValue(4);            // fldUDataSetXML
            if (Pos('HTTP:/', UpperCase(Value)) = 1) or (Pos('HTTPS:/', UpperCase(Value)) = 1) then
              Layer.MP.Source := Value
            else
              begin
                Template := TDeTemplate.Create;
                try
                  Template.Text := Value;
                  if Assigned(Template.Root) then
                    begin
                      Node := Template.Root.FindNode('map');
                      if Assigned(Node) then
                        begin
                          Layer.MP.Source := Trim(Node.Attributes['url']);
                          Layer.MP.TileDeadInHours := StrToIntDef(Trim(Node.Attributes['timeout']), Layer.MP.TileDeadInHours);
                          Layer.MP.SetCoordName(Trim(Node.Attributes['type']));
                        end;
                    end;
                finally
                  Template.Free;
                end;
              end;
          end
        else
          Layer.Free;
      end;
  finally
    Query.Free;
  end;
end;

procedure TMapLayers.LoadSystemDefaults;
const
  faExclude = faDirectory or faVolumeID;
var
  Directory, URL, Value: string;
  ErrorCode, TileDeadHours, Index, LayerCount: Integer;
  SearchRec: TSearchRec;
  Template: TDeTemplate;
  Node: TDeNode;
  CoordType: TMapCoordType;
  procedure AppendSystemLayer(const LayerName, TemplateURL: string; const TileDeadHours: Integer; const CoordType: TMapCoordType);
  var
    Layer: TMapLayer;
  begin
    Layer := TMapLayer.Create;
    if FList.Add(Layer) <> -1 then
      begin
        Layer.FStage := mlsInserted;
        Layer.FName := LayerName;
        Layer.MP.Source := TemplateURL;
        Layer.MP.TileDeadInHours := TileDeadHours;
        Layer.MP.Coord := CoordType;
      end
    else
      Layer.Free;
  end;
begin
  FList.Clear;
  Directory := ExtractFilePath(GetModuleName(hInstance));
  if Length(Directory) <> 0 then
    Directory := IncludeTrailingBackslash(Directory);
  ErrorCode := FindFirst(Directory + '*' + sExtensionMLX, faAnyFile, SearchRec);
  if ErrorCode = 0 then
    try
      Template := TDeTemplate.Create;
      try
        while ErrorCode = 0 do
          begin
            if (SearchRec.Attr and faExclude) = 0 then
              try
                Template.Text := ReadStringFromFile(Directory + SearchRec.Name);
                Node := Template.Root.FindNode('layers');
                // Если это файл библиотеки слоёв, то ...
                if Assigned(Node) then
                  begin
                    if Assigned(Node.ChildNodes) then
                      begin
                        LayerCount := 0;
                        for Index := 0 to Pred(Node.ChildNodes.Count) do
                          if Assigned(Node.ChildNodes[Index]) and SameText(Node.ChildNodes[Index].NodeName, 'map') then
                            begin
                              Inc(LayerCount);
                              URL := Trim(Node.ChildNodes[Index].Attributes['url']);
                              TileDeadHours := StrToIntDef(Trim(Node.ChildNodes[Index].Attributes['timeout']), -1);
                              Value := Trim(Node.ChildNodes[Index].Attributes['type']);
                              if SameText(Value, 'ELLIPSE') then
                                CoordType := mctEllipse
                              else if SameText(Value, 'SPHERE') then
                                CoordType := mctSphere
                              else if SameText(Value, 'DECARTE') then
                                CoordType := mctDecarte
                              else
                                CoordType := mctDefault;
                              Value := Trim(Node.ChildNodes[Index].Attributes['name']);
                              if Length(Value) = 0 then
                                Value := Trim(ChangeFileExt(SearchRec.Name, EmptyStr) + ' #' + IntToStr(LayerCount));
                              AppendSystemLayer(Value, URL, TileDeadHours, CoordType);
                            end;
                      end;
                  end
                else
                  begin
                    Node := Template.Root.FindNode('map');
                    if Assigned(Node) then
                      begin
                        URL := Trim(Node.Attributes['url']);
                        TileDeadHours := StrToIntDef(Trim(Node.Attributes['timeout']), -1);
                        Value := Trim(Node.Attributes['type']);
                        if SameText(Value, 'ELLIPSE') then
                          CoordType := mctEllipse
                        else if SameText(Value, 'SPHERE') then
                          CoordType := mctSphere
                        else if SameText(Value, 'DECARTE') then
                          CoordType := mctDecarte
                        else
                          CoordType := mctDefault;
                        AppendSystemLayer(ChangeFileExt(SearchRec.Name, EmptyStr), URL, TileDeadHours, CoordType);
                      end;
                  end;
              except
                on E: Exception do
                  begin
                    {$IFDEF DEBUG}
                    DebugLog('TMapLayers.LoadSystemDefaults loading file %s skip error: %s', [SearchRec.Name, E.Message]);
                    {$ENDIF}
                    {$IFDEF DeDEBUG}
                    WriteLog('Load layer file %s error', [SearchRec.Name], 'Map');
                    {$ENDIF}
                  end;
              end;
            ErrorCode := FindNext(SearchRec);
          end;
      finally
        Template.Free;
      end;
    finally
      FindClose(SearchRec);
    end;
  // Если не загружены файлы слоёв, то берём слои по умолчанию по старой логике ...
  if FList.Count = 0 then
    begin
      AppendSystemLayer('Yandex', cDefaultTemplateMapYandexURL, -1, mctEllipse);
      AppendSystemLayer('Google', cDefaultTemplateMapGoogleURL, -1, mctEllipse);
      AppendSystemLayer('Open Street Map', cDefaultTemplateOpenStreetMapURL, -1, mctSphere);
      AppendSystemLayer('2GIS', cDefaultTemplate2GISURL, -1, mctEllipse);
    end;
end;

function TMapLayers.IndexByID(const LayerID: Integer): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Pred(Count) do
    if Items[Index].LayerID = LayerID then
      begin
        Result := Index;
        Break;
      end;
end;

function TMapLayers.IndexByName(const LayerName: String): Integer;
var i: Integer;
begin
  Result:= -1;
  for i:= 1 to Pred(Count) do
    if AnsiSameText(Items[i].Name, LayerName) then Exit(i);
end;

function TMapLayers.SelectLayer(const Index: Integer): Boolean;
begin
  if Index < 0  then Exit(False);
  if Pred(Count) < Index then Exit(False);

  Result:= InitializeMapEnvirovment(Items[Index].MP, Items[Index].FLayerID);
  if Result then FSelected:= Items[Index]
            else FSelected:= nil;
end;

procedure TMapLayers.CreateDefaultLayer;
var
  Layer: TMapLayer;
begin
  Layer := TMapLayer.Create;
  if FList.Add(Layer) <> -1 then
    begin
      Layer.FStage := mlsNone;
      Layer.FName := '_Da.default';
      Layer.MP.Source := EmptyStr;
      Layer.MP.TileDeadInHours := MaxInt;
      Layer.MP.Coord := mctDecarte;
      SelectLayer(0);
    end
  else
    Layer.Free;
end;

procedure TMapLayers.SaveToDatabase;
var
  Index: Integer;
begin
  for Index := 0 to Pred(Count) do
    Items[Index].SaveToDatabase;
  for Index := Pred(Count) downto 0 do
    if Items[Index].Stage = mlsDeleted then
      FList.Delete(Index);
end;

function TMapLayers.NewLayer(SourceLayer: TMapLayer): TMapLayer;
begin
  Result := TMapLayer.Create;
  if FList.Add(Result) <> -1 then
      begin
        if Assigned(SourceLayer) then
          begin
            Result.FStage := mlsInserted;
            Result.FUserID := SourceLayer.UserID;
            Result.FDatasetID := SourceLayer.DatasetID;
            Result.FName := SourceLayer.Name;
            Result.MP.Source := SourceLayer.MP.Source;
            Result.MP.TileDeadInHours := SourceLayer.MP.TileDeadInHours;
          end;
      end
  else
    FreeAndNil(Result);
end;

{ TMapTilePoolJob }

constructor TMapTilePoolJob.Create(const aSource: string; const ATileCol, ATileRow, ANormalizeTileCol, ANormalizeTileRow, ATilePriority: Integer;
                    const AMapScale: TMapScale; const ABackgroundColor: TColor; const ATileURL, ATileDirectory: string);
begin
  FSource:= aSource;
  FTileCol:= ATileCol;
  FTileRow:= ATileRow;
  FNormalizeTileCol:= ANormalizeTileCol;
  FNormalizeTileRow:= ANormalizeTileRow;
  FTilePriority:= ATilePriority;
  FMapScale:= AMapScale;
  FBackgroundColor:= ABackgroundColor;
  FTileURL:= ATileURL;
  FTileDirectory:= ATileDirectory;
  if Length(FTileDirectory) <> 0 then FTileDirectory := IncludeTrailingPathDelimiter(FTileDirectory);
  FTileDirectory := IncludeTrailingPathDelimiter(FTileDirectory + IntToStr(FMapScale));
end;

function TMapLayers.LoadLayer(const KeyID: Variant; TableMeta: TTableMeta; FieldMeta: TFieldMeta; var MP: TMapProperties): Boolean;
var
  Table: TTableMeta;
  Field: TFieldMeta;
  Index: Integer;
  Value, Extension, FileName: string;
  Dataset: TDeDataset;
  Image: TDeImage;
begin
  // Проверим, что задано хотя бы таблица или поле ...
  Result := Assigned(TableMeta) or Assigned(FieldMeta);
  if Result then
    begin
      // Если не указана таблица, то берём её по полю ...
      if Assigned(TableMeta) then
        Table := TableMeta
      else
        Table := FieldMeta.Owner;
      // Если не указано поле, то ищем первое BLOB поле ...
      if Assigned(FieldMeta) then
        Field := FieldMeta
      else
        begin
          Field:= nil;
          for Index := 0 to Pred(Table.Fields.Count) do
            if Table.Fields[Index].DataType in BinaryTypes then
              begin
                Field := Table.Fields[Index];
                Break;
              end;
        end;

      // Проверим наличие таблицы и поля ...
      Result := Assigned(Table) and Assigned(Field);
      if Result then
        begin
          Result := Assigned(Table.Database);
          // Если у таблицы есть база данных, то ...
          if Result then
            begin
              Result := Table.KeyCount = 1;
              // Если ключевое поле в таблице одно, то ...
              if Result then
                begin
                  Dataset := Table.Database.CreateQuery(qtRow);
                  try
                    Dataset.Descr.BeginUpdate;
                    try
                      // Возьмём из таблицы BLOB поле со схемой ...
                      Dataset.Descr.Table := Table.Table;
                      Dataset.Descr.AddField(Field.Original);
                      Value := Table.KField[0].Original;
                      Dataset.Descr.AddParamCondition(Value, Table.KField[0].DataType, opEQ, Value, KeyID);
                    finally
                      Dataset.Descr.EndUpdate;
                    end;
                    Dataset.Open;
                    Image := TDeImage.Create(nil);
                    try
                      Image.Value := Dataset.Value[0];
                      Result := Assigned(Image.Graphic) and not Image.Graphic.Empty;
                      // Если данные получены, то ...
                      if Result then
                        begin
                          case Image.ImageType of
                            itBMP: Extension := sExtensionBMP;
                            itJPEG: Extension := sExtensionJPG;
                            itGIF: Extension := sExtensionGIF;
                            itPNG: Extension := sExtensionPNG;
                          else
                            Extension := EmptyStr;
                          end;
                          Result := Length(Extension) <> 0;
                          // Если в BLOB поле была картинка, то ...
                          if Result then
                            begin
                              FileName := DeGetTempFileName(EmptyStr, 'MAP', Extension);
                              // Сохраним файл ...
                              Result := Image.SaveToFile(FileName);
                              // Если файл сохранён, то попытаемся инициализировать карту по сохранённому файлу ...
                              if Result then
                                begin
                                  MP.SourceType:= mstDatabase;
                                  MP.Source:= FileName;
                                  MP.Coord:= mctDecarte;
                                  MP.OriginalX:= Image.Graphic.Width;
                                  MP.OriginalY:= Image.Graphic.Height;
                                  MP.InitializeMapScales( Low(MapDecarteScale), High(MapDecarteScale) );
                                  MP.SetScale((Low(MapDecarteScale) + High(MapDecarteScale)) div 2);
                                end;
                            end
                          else
                            begin
                              {$IFDEF DeDEBUG}
                              WriteLog('Unknown graphic type in table %s and field %s key %s for load the layer!', [QuotedStr(Table.Table), QuotedStr(Field.Original), VarToStr(KeyID)], 'Map');
                              {$ENDIF}
                            end;
                        end;
                    finally
                      Image.Free;
                    end;
                  finally
                    Dataset.Free;
                  end;
                end
              else
                begin
                  {$IFDEF DeDEBUG}
                  WriteLog('Multi primary key fields in table %s for load the layer!', [QuotedStr(Table.Table)], 'Map');
                  {$ENDIF}
                end;
            end
          else
            begin
              {$IFDEF DeDEBUG}
              WriteLog('Unknown database in table %s for load the layer!', [QuotedStr(Table.Table)], 'Map');
              {$ENDIF}
            end;
        end
      else
        begin
          {$IFDEF DeDEBUG}
          if Assigned(Table) then
            WriteLog('Unknown blob field in table %s for load the layer!', [QuotedStr(Table.Table)], 'Map')
          else
            WriteLog('Unknown table for load the layer!', 'Map');
          {$ENDIF}
        end;
    end
  else
    begin
      {$IFDEF DeDEBUG}
      WriteLog('Unknown from where load the layer!', 'Map');
      {$ENDIF}
    end;
end;

{ TMapTilePoolThread }

constructor TMapTilePoolThread.Create(AJob: TMapTilePoolJob);
begin
  inherited Create(True);
  FSource := AJob.Source;
  FTileRow := AJob.TileRow;
  FTileCol := AJob.TileCol;
  FTilePriority := AJob.TilePriority;
  FMapScale := AJob.MapScale;
  FBackgroundColor := AJob.BackgroundColor;
  FTileURL := AJob.TileURL;
  FTileDirectory := AJob.TileDirectory;
  if Length(FTileDirectory) <> 0 then FTileDirectory := IncludeTrailingPathDelimiter(FTileDirectory);
  FNormalizeTileRow := AJob.NormalizeTileRow;
  FNormalizeTileCol := AJob.NormalizeTileCol;
  FreeOnTerminate := True;
end;

procedure TMapTilePoolThread.Execute;
var
  GraphicFileType: TGraphicFileType;
  TileFileName: string;
begin
  FFileName := EmptyStr;
  FErrorMessage := EmptyStr;
  try
    FTempFileName := FTileDirectory + Format('%d_%d%s', [FNormalizeTileRow, FNormalizeTileCol, '.tmp']);
    if FileExists(FTempFileName) then DeleteFile(FTempFileName);
    try
      ForceDirectories(ExtractFilePath(FTempFileName));
      if Pos('file://', LowerCase(FTileURL)) = 0 then
        if URLDownloadToFile(nil, PChar(FTileURL), PChar(FTempFileName), 0, nil) = S_OK then
          begin
            GraphicFileType := DetectingGraphicFileType(FTempFileName);
            if GraphicFileType  = gftUnknown then
              raise Exception.CreateFmt('Download tile %s unknown format', [QuotedStr(FTileURL)])
            else
              begin
                FFileName := ChangeFileExt(FTempFileName, GraphicFileTypeToExtension(GraphicFileType));
                if FileExists(FFileName) then
                  DeleteFile(FFileName);
                if not RenameFile(FTempFileName, FFileName) then
                  FFileName := EmptyStr;
              end;
          end
        else
          raise Exception.CreateFmt('Download tile %s failed', [QuotedStr(FTileURL)])
      else
        begin
          TileFileName := Copy(FTileURL, 8, Length(FTileURL));
          TileFileName := ReplaceText(TileFileName, '/', '\');
          FFileName := ChangeFileExt(FTempFileName, ExtractFileExt(TileFileName));
          if not CopyFile(PChar(TileFileName), PChar(FFileName), False) then
            FFileName := EmptyStr;
        end;
    finally
      if FileExists(FTempFileName) then DeleteFile(FTempFileName);
    end;
  except
    on E: Exception do FErrorMessage := E.Message;
  end;
end;

{
procedure TMapTilePoolThread.ConvertImageSinchronized;
var
  Extension: string;
  GraphicClass: TGraphicClass;
  Graphic: TGraphic;
begin
  if DetectingGraphicFileClass(FTempFileName, GraphicClass, Extension) then
    begin
      Graphic := GraphicClass.Create;
      try
        Graphic.LoadFromFile(FTempFileName);
        FFileName := FTileDirectory + Format('%d_%d%s', [FNormalizeTileRow, FNormalizeTileCol, Extension]);
        Graphic.SaveToFile(ChangeFileExt(FFileName, EmptyStr) + '.original' + Extension);
        // Если размер тайла 256x256, то ...
        if (Graphic.Width = cTileSize) and (Graphic.Height = cTileSize) then
          begin
            ForceDirectories(ExtractFilePath(FFileName));
            Graphic.SaveToFile(FFileName);
          end
        else
          begin
            // Иначе: приводим тайл к размеру 256x256 с центрированием...
            ResizeCenterGraphic(Graphic, cTileSize, cTileSize, FBackgroundColor);
            ForceDirectories(ExtractFilePath(FFileName));
            Graphic.SaveToFile(FFileName);
          end;
      finally
        Graphic.Free;
      end;
    end
  else
    raise Exception.CreateFmt('Download tile %s unknown format', [QuotedStr(FTileURL)]);
end;
}

procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('uMapLayers unit initialization ...');
  {$ENDIF}
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('uMapLayers unit finalization ...');
  {$ENDIF}
end;

initialization
  Startup;

finalization
  Shutdown;

end.

