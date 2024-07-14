unit DeDBStorage;

interface

uses {Windows, }Classes, {Graphics, }
     {G2D, } {Funcs, } DeSchema2D;


type
  TdeDBStorage = class(TdeSchemaStorage)
  private
    FIndex1    : array of packed record
      objOwner  : integer;
      objLevel  : integer;
      objParent : integer;
      objZOrder : integer;
      objID     : integer;
      objClass  : integer;
      objData   : string;
    end;
    FCount1    : integer;
    FCapacity1 : integer;
    FIndex2    : array of packed record
      Item      : TObject;
      objID     : integer;
      objOwner  : integer;
    end;
    FCount2    : integer;
    FCapacity2 : integer;
    FIndex3    : array of packed record
      objID     : integer;
      Item      : TObject;
    end;
    FCount3    : integer;
    FCapacity3 : integer;
    FSaved     : tStringList;
    FStepsCount: integer;
    FStep      : integer;
    procedure   GrowIndex1;
    procedure   GrowIndex2;
    procedure   GrowIndex3;
  protected
    procedure   setStepsCount(aValue:integer);virtual;
    procedure   setStep(aValue:integer);virtual;
    function    IsEmpty:boolean;
    procedure   clearIndex1;
    procedure   clearIndex2;
    procedure   clearIndex3;
    function    findIndex1(objOwner  : integer;
                           objLevel  : integer;
                           objParent : integer;
                           objZOrder : integer;
                           objID     : integer;
                           var index : integer):boolean;
    function    findIndex2(anItem    : TObject;
                           var index : integer):boolean;
    function    findIndex3(objID     : integer;
                           var index : integer):boolean;
    procedure   addIndex1( objOwner  : integer;
                           objLevel  : integer;
                           objParent : integer;
                           objZOrder : integer;
                           objID     : integer;
                           objClass  : integer;
                           objData   : string);
    procedure   addIndex2( anItem    : TObject;
                           objID     : integer;
                           objOwner  : integer);
    procedure   addIndex3( objID     : integer;
                           anItem    : TObject);
    function    getIndex2(anItem     : TObject; var anOwner:integer):integer;
    function    getIndex3(objID      : integer):TObject;
    procedure   delIndexes(objID:integer);
    procedure   MarkSaved(objID:integer;aMark:boolean);
    function    IsSaved(objID:integer):boolean;
    function    SaveObject(anObject:TObject):integer;virtual;
    procedure   doSave(aSchema:TdeSchema);virtual;
    procedure   doLoad(aSchema:TdeSchema);virtual;
    function    OpenDatabase(forWrite:boolean):boolean;virtual;
    procedure   CloseDatabase;virtual;
    function    getLoadingObjectsCount:integer;virtual;
    procedure   InsertObject(var objID:integer;
                             objOwner,objClass,objParent,objZOrder:integer;
                             objData:string);virtual;abstract;
    procedure   UpdateObject(objID,objOwner,objClass,objParent,objZOrder:integer;
                             objData:string);virtual;abstract;
    procedure   DeleteObject(objID:integer);virtual;abstract;
    function    FirstObject(var objID:integer):boolean;virtual;abstract;
    function    NextObject(var objID:integer):boolean;virtual;abstract;
    function    getObjectData(objID:integer;
                              var objOwner,objClass,objParent,objZOrder:integer;
                              var objData:string):integer;virtual;abstract;
    function    getObjectLevel(objID:integer):integer;virtual;abstract;
    property    StepsCount:integer read FStepsCount write setStepsCount;
    property    Step:integer read FStep write setStep;
  public
    constructor Create;
    destructor  Destroy;override;
    procedure   Load(aSchema:TdeSchema);override;
    procedure   Save(aSchema:TdeSchema);override;
  end;


type
  TdeCachedDBStorage = class(TdeDBStorage)
  private
    FCount      : integer;
    FCapacity   : integer;
    FCurrent    : integer;
    FCache      : array of packed record
      objID      : integer;
      objOwner   : integer;
      objClass   : integer;
      objParent  : integer;
      objZOrder  : integer;
      objData    : string;
      objLevel   : integer;
    end;
    procedure   GrowCache;
  protected
    procedure   ClearCache;
    function    FindCache(objID:integer;var index:integer):boolean;
    procedure   AddCache(objID,objOwner,objClass,objParent,objZOrder:integer;
                         objData:string);
    function    getMaxObjID:integer;
    procedure   InsertObject(var objID:integer;
                             objOwner,objClass,objParent,objZOrder:integer;
                             objData:string);override;
    procedure   UpdateObject(objID,objOwner,objClass,objParent,objZOrder:integer;
                             objData:string);override;
    function    FirstObject(var objID:integer):boolean;override;
    function    NextObject(var objID:integer):boolean;override;
    function    getObjectData(objID:integer;
                              var objOwner,objClass,objParent,objZOrder:integer;
                              var objData:string):integer;override;
    function    getObjectLevel(objID:integer):integer;override;
    procedure   doInsertObject(var objID:integer;
                               objOwner,objClass,objParent,objZOrder:integer;
                               objData:string);virtual;abstract;
    procedure   doUpdateObject(objID,objOwner,objClass,objParent,objZOrder:integer;
                               objData:string);virtual;abstract;
  public
    constructor Create;
    destructor  Destroy;override;
  end;


implementation

uses SysUtils,
     DeLog;

{TdeDBStorage}

constructor TdeDBStorage.Create;
begin
  inherited Create;
  FIndex1    := nil;
  FCapacity1 := 0;
  FCount1    := 0;
  FIndex2    := nil;
  FCapacity2 := 0;
  FCount2    := 0;

  FSaved        := TStringList.Create;
  FSaved.Sorted := true;
  FSaved.Duplicates := dupIgnore;
end;

destructor  TdeDBStorage.Destroy;
begin
  FSaved.Free;
  FIndex1    := nil;
  FCapacity1 := 0;
  FCount1    := 0;
  FIndex2    := nil;
  FCapacity2 := 0;
  FCount2    := 0;
  inherited Destroy;
end;

procedure   TdeDBStorage.GrowIndex1;
var
  Delta: Integer;
begin
  if FCapacity1 > 64 then
    Delta := FCapacity1 div 4
  else
    if FCapacity1 > 8 then
      Delta := 16
    else
      Delta := 4;
  FCapacity1 := FCapacity1+Delta;
  setLength(FIndex1,FCapacity1);
end;

procedure   TdeDBStorage.GrowIndex2;
var
  Delta: Integer;
begin
  if FCapacity2 > 64 then
    Delta := FCapacity2 div 4
  else
    if FCapacity2 > 8 then
      Delta := 16
    else
      Delta := 4;
  FCapacity2 := FCapacity2+Delta;
  setLength(FIndex2,FCapacity2);
end;

procedure   TdeDBStorage.GrowIndex3;
var
  Delta: Integer;
begin
  if FCapacity3 > 64 then
    Delta := FCapacity3 div 4
  else
    if FCapacity3 > 8 then
      Delta := 16
    else
      Delta := 4;
  FCapacity3 := FCapacity3+Delta;
  setLength(FIndex3,FCapacity3);
end;

procedure   TdeDBStorage.setStepsCount(aValue:integer);
begin
  FStepsCount := aValue;
end;

procedure   TdeDBStorage.setStep(aValue:integer);
begin
  FStep := aValue;
end;

function    TdeDBStorage.IsEmpty:boolean;
begin
  Result := (FCount2 = 0);
end;

procedure   TdeDBStorage.clearIndex1;
begin
  FIndex1    := nil;
  FCount1    := 0;
  FCapacity1 := 0;
end;

procedure   TdeDBStorage.clearIndex2;
begin
  FIndex2    := nil;
  FCount2    := 0;
  FCapacity2 := 0;
end;

procedure   TdeDBStorage.clearIndex3;
begin
  FIndex3    := nil;
  FCount3    := 0;
  FCapacity3 := 0;
end;

function    TdeDBStorage.findIndex1(objOwner  : integer;
                                    objLevel  : integer;
                                    objParent : integer;
                                    objZOrder : integer;
                                    objID     : integer;
                                    var index : integer):boolean;
var
  iMin,iMax,iMid,C : integer;
begin
  Result := false;
  iMin := 0;
  iMax := FCount1-1;
  //TODO: В этой процедуре иногда зациливается...
  while (iMin <= iMax) do begin
    iMid := (iMin+iMax) shr 1;
    C := FIndex1[iMid].objOwner-objOwner;
    if (C < 0) then
      iMin := iMid+1
    else if (C > 0) then
      iMax := iMid-1
    else begin
      C := FIndex1[iMid].objLevel-objLevel;
      if (C < 0) then
        iMin := iMid+1
      else if (C > 0) then
        iMax := iMid-1
      else begin
        C := FIndex1[iMid].objParent-objParent;
        if (C < 0) then
          iMin := iMid+1
        else if (C > 0) then
          iMax := iMid-1
        else begin
          C := FIndex1[iMid].objZOrder-objZOrder;
          if (C < 0) then
            iMin := iMid+1
          else if (C > 0) then
            iMax := iMid-1
          else begin
            C := FIndex1[iMid].objID-objID;
            if (C < 0) then
              iMin := iMid+1
            else if (C > 0) then begin
              iMax := iMid-1;
              Result := (C=0);
            end;
          end;
        end;
      end;
    end;
  end;
  index := iMin;
end;

function    TdeDBStorage.findIndex2(anItem:TObject;var index:integer):boolean;
var
  iMin,iMax,iMid,C : integer;
begin
  Result := false;
  iMin := 0;
  iMax := FCount2-1;
  while (iMin <= iMax) do begin
    iMid := (iMin+iMax) shr 1;
    C := integer(FIndex2[iMid].Item)-integer(anItem);
    if (C < 0) then
      iMin := iMid+1
    else begin
      iMax := iMid-1;
      Result := (C=0);
    end;
  end;
  index := iMin;
end;

function    TdeDBStorage.findIndex3(objID     : integer;
                                    var index : integer):boolean;
var
  iMin,iMax,iMid,C : integer;
begin
  Result := false;
  iMin := 0;
  iMax := FCount3-1;
  while (iMin <= iMax) do begin
    iMid := (iMin+iMax) shr 1;
    C := FIndex3[iMid].objID-objID;
    if (C < 0) then
      iMin := iMid+1
    else begin
      iMax := iMid-1;
      Result := (C=0);
    end;
  end;
  index := iMin;
end;

procedure   TdeDBStorage.addIndex1( objOwner  : integer;
                                    objLevel  : integer;
                                    objParent : integer;
                                    objZOrder : integer;
                                    objID     : integer;
                                    objClass  : integer;
                                    objData   : string);
var
  i,index : integer;
begin
  if not(findIndex1(objOwner,objLevel,objParent,objZOrder,objID,index))
  then begin
    if FCount1 = FCapacity1 then GrowIndex1;
    for i := FCount1 downto index+1 do
      FIndex1[i] := FIndex1[i-1];
    FIndex1[index].objOwner  := objOwner;
    FIndex1[index].objLevel  := objLevel;
    FIndex1[index].objParent := objParent;
    FIndex1[index].objZOrder := objZOrder;
    FIndex1[index].objID     := objID;
    FCount1 := FCount1 + 1;
  end;
  FIndex1[index].objClass  := objClass;
  FIndex1[index].objData   := objData;
end;

procedure   TdeDBStorage.addIndex2( anItem    : TObject;
                                    objID     : integer;
                                    objOwner  : integer);
var
  i,index : integer;
begin
  if not(findIndex2(anItem,index))
  then begin
    if FCount2 = FCapacity2 then GrowIndex2;
    for i := FCount2 downto index+1 do
      FIndex2[i] := FIndex2[i-1];
    FIndex2[index].Item  := anItem;
    FCount2 := FCount2 + 1;
  end;
  FIndex2[index].objID    := objID;
  FIndex2[index].objOwner := objOwner;
end;

procedure   TdeDBStorage.addIndex3( objID     : integer;
                                    anItem    : TObject);
var
  i,index : integer;
begin
  if not(findIndex3(objID,index))
  then begin
    if FCount3 = FCapacity3 then GrowIndex3;
    for i := FCount3 downto index+1 do
      FIndex3[i] := FIndex3[i-1];
    FIndex3[index].objID  := objID;
    FCount3 := FCount3 + 1;
  end;
  FIndex3[index].Item := anItem;
end;

function    TdeDBStorage.getIndex2(anItem      : TObject;
                                   var anOwner : integer):integer;
var
  index : integer;
begin
  Result  :=-1;
  anOwner :=-1;
  if findIndex2(anItem,index) then begin
    Result  := FIndex2[index].objID;
    anOwner := FIndex2[index].objOwner;
  end;
end;

function    TdeDBStorage.getIndex3(objID:integer):TObject;
var
  index : integer;
begin
  Result := nil;
  if findIndex3(objID,index) then
    Result := FIndex3[index].Item;
end;

procedure   TdeDBStorage.delIndexes(objID:integer);
var
  i,index : integer;
  anItem  : TObject;
begin
  if findIndex3(objID,index) then begin
    anItem := FIndex3[index].Item;
    for i := index to FCount3-2 do
      FIndex3[i] := FIndex3[i+1];
    FCount3 := FCount3 - 1;
    if findIndex2(anItem,index) then begin
      for i := index to FCount2-2  do
        FIndex2[i] := FIndex2[i+1];
    end;
    FCount2 := FCount2 - 1;
  end;
end;

procedure   TdeDBStorage.MarkSaved(objID:integer;aMark:boolean);
var
  i : integer;
begin
  if aMark then
    FSaved.Add(IntToHex(integer(objID),8))
  else begin
    i := FSaved.IndexOf(IntToHex(objID,8));
    if i>=0 then
      FSaved.Delete(i);
  end;
end;

function    TdeDBStorage.IsSaved(objID:integer):boolean;
begin
  Result := (FSaved.IndexOf(IntToHex(objID,8)) >= 0);
end;

function    TdeDBStorage.SaveObject(anObject:TObject):integer;
var
  i         : integer;
  s         : string;
  prntID    : integer;
  objZOrder : integer;
  objClass  : integer;
  anItem    : TdeSchemaObject;
  aShortcut : TdeSchemaShortcut;
  objOwner  : integer;
  bModified : boolean;
begin
  Result :=-1;
  if (anObject = nil) then exit;
  Result  := getIndex2(anObject,objOwner);
  if (Result<0) or not(IsSaved(Result)) then
  begin
    if (anObject is TdeSchemaLayer)
    then begin
      if TdeSchemaLayer(anObject).ReadOnly then
        s := '-'+TdeSchemaLayer(anObject).Name
      else
        s := '+'+TdeSchemaLayer(anObject).Name;
      prntID    :=-1;
      objClass  :=-2;
      objZOrder :=-1;
      bModified := TdeSchemaLayer(anObject).Modified;
      TdeSchemaLayer(anObject).Modified := false;
    end
    else if (anObject is TdeSchemaShortcut)
    then begin
      aShortcut := TdeSchemaShortcut(anObject);
      s := ' ';
      i := 0;
      if not aShortcut.getData(@(s[1]),i)
      then begin
        setLength(s,i);
        aShortcut.getData(@(s[1]),i)
      end;
      prntID := SaveObject(aShortcut.LinkedObject);
      objClass  :=-3;
      objZOrder :=-1;
      bModified := aShortcut.Modified;
      aShortcut.Modified := false;
    end
    else begin
      anItem := TdeSchemaObject(anObject);
      if (anItem.Level=0) then
        prntID := SaveObject(anItem.Layer)
      else
        prntID := SaveObject(anItem.Parent);
      objClass := anItem.ClassID;
      //
      s := ' ';
      i := 0;
      if not anItem.getData(@(s[1]),i)
      then begin
        setLength(s,i);
        anItem.getData(@(s[1]),i)
      end;
      objZOrder := anItem.Index;
      bModified := anItem.Modified;
      anItem.Modified := false;
    end;
    if (Result<0)
    then begin
      objOwner :=-1;
      InsertObject(Result,objOwner,objClass,prntID,objZOrder,s);
      addIndex2(anObject,Result,objOwner);
      addIndex3(Result,anObject);
    end
    else if bModified then
      UpdateObject(Result,objOwner,objClass,prntID,objZOrder,s);
    MarkSaved(Result,true);
  end;
end;

procedure   TdeDBStorage.doSave(aSchema:TdeSchema);
var
  i         : integer;
  anItem    : TdeSchemaObject;
begin
  FSaved.Clear;
  StepsCount := aSchema.ItemsCount + FCount2;
  Step := 0;
  anItem := aSchema.First;
  while (anItem<>nil) do begin
    SaveObject(anItem);
    anItem := aSchema.Next(anItem);
    Step := Step + 1;
  end;
  {}
  //DONE: Сохранение слоев
  for i := 0 to aSchema.LayersCount - 1 do
    if aSchema.Layer[i].Modified then
      begin
        SaveObject(aSchema.Layer[i]);
        Step := Step + 1;
      end;
  {}
  for i := 0 to aSchema.ShortCutsCount - 1 do
  begin
    SaveObject(aSchema.Shortcut[i]);
    Step := Step + 1;
  end;
  {}
  for i := FCount2-1 downto 0 do begin
    if (FSaved.IndexOf(IntToHex(FIndex2[i].objID,8))<0)
    then begin
      DeleteObject(FIndex2[i].objID);
      delIndexes(FIndex2[i].objID);
    end;
    Step := Step + 1;
  end;
end;

function    TdeDBStorage.getLoadingObjectsCount:integer;
begin
  Result := 1;
end;

procedure   TdeDBStorage.doLoad(aSchema:TdeSchema);
var
  objID     : integer;
  objOwner  : integer;
  objClass  : integer;
  objParent : integer;
  objZOrder : integer;
  objLevel  : integer;
  sData     : string;
  i         : integer;
  LName     : string;
  LReadOnly : boolean;
  PrntObj   : TObject;
  anItem    : TdeSchemaObject;
  aLayer    : TdeSchemaLayer;
  aParent   : TdeSchemaGroup;
  aShortcut : TdeSchemaShortcut;
begin
  clearIndex1;
  clearIndex2;
  clearIndex3;
  //перебираем объекты начиная с первого
  StepsCount := 3 * getLoadingObjectsCount;
  Step := 0;
  if FirstObject(objID) then repeat;
    //получить данные объекта
    getObjectData(objID,objOwner,objClass,objParent,objZOrder,sData);
    //вычисляем уровень
    objLevel := getObjectLevel(objParent) + 1;
    //строим индекс
    addIndex1(objOwner,objLevel,objParent,objZOrder,objID,{->}objClass,sData);
    Step := Step + 1;
  until not NextObject(objID);

  for i := 0 to FCount1-1 do begin
    //создать объект согласно классу
    if (FIndex1[i].objClass=-2)
    then begin
      //создать слой
      sData := FIndex1[i].objData;
      if sData = EmptyStr then
        begin
          LReadOnly := False;
          LName     := 'Layer '+IntToStr(i);
        end
      else
        begin
          LReadOnly := Copy(sData,1,1)='-';
          LName     := Copy(sData,2,length(sData)-1);
        end;

      aLayer := aSchema.LayerByName[LName];
      if (aLayer<>nil) then
        begin
          addIndex2(aLayer,FIndex1[i].objID,FIndex1[i].objOwner);
          addIndex3(FIndex1[i].objID,aLayer);
          aLayer.ReadOnly := LReadOnly;
        end;
    end
    else if (FIndex1[i].objClass=-3)
    then begin
      //создать Shortcut
      PrntObj   := getIndex3(FIndex1[i].objParent);
      aShortcut := aSchema.NewShortcut;
      //загрузить данные в объект
      if (aShortcut<>nil)
      then begin
        addIndex2(aShortcut,FIndex1[i].objID,FIndex1[i].objOwner);
        addIndex3(FIndex1[i].objID,aShortcut);
        aShortcut.setData(@(FIndex1[i].objData[1]),length(FIndex1[i].objData));
        aShortcut.LinkedObject := TdeSchemaObject(PrntObj);
      end;
    end
    else begin
      PrntObj := getIndex3(FIndex1[i].objParent);
      anItem  := nil;
      aParent := nil;
      LName   := '';
      if (PrntObj <> nil) then begin
        if PrntObj is TdeSchemaLayer then begin
          aLayer := TdeSchemaLayer(PrntObj);
          LName := aLayer.Name;
        end
        else if PrntObj is TdeSchemaGroup then
          aParent := TdeSchemaGroup(PrntObj);
      end;
      if (FIndex1[i].objClass >= 0) then     //создать объект
        anItem := aSchema.New(FIndex1[i].objClass,aParent,LName)
      else if (FIndex1[i].objClass = -1) then           //создать группу
        anItem := aSchema.NewGroup(aParent,LName);
      //загрузить данные в объект
      if (anItem<>nil)
      then begin
        anItem.Lock;
        addIndex2(anItem,FIndex1[i].objID,FIndex1[i].objOwner);
        addIndex3(FIndex1[i].objID,anItem);
        anItem.setData(@(FIndex1[i].objData[1]),length(FIndex1[i].objData));
      end;
    end;
    Step := Step + 1;
  end;

  anItem := aSchema.Last;
  while (anItem <> nil) do begin
    if findIndex2(anItem,i) then begin
      if (anItem is TdeSchemaObject) then
        TdeSchemaObject(anItem).unLock;
      Step := Step + 1;
    end;
    anItem := aSchema.Prev(anItem);
  end;
  clearIndex1;

  for i := 0 to aSchema.LayersCount-1 do
    aSchema.Layer[i].Modified := false;
  for i := 0 to aSchema.ShortcutsCount-1 do
    aSchema.Shortcut[i].Modified := false;
  anItem := aSchema.First;
  while anItem <> nil do begin
    anItem.Modified := false;
    anItem := aSchema.Next(anItem);
  end;
end;

function    TdeDBStorage.OpenDatabase(forWrite:boolean):boolean;
begin
  //FSaved.Clear;
  Result := true;
end;

procedure   TdeDBStorage.CloseDatabase;
begin
  //FSaved.Clear;
end;

procedure   TdeDBStorage.Load(aSchema:TdeSchema);
begin
  aSchema.Lock;
  if OpenDatabase(false) then try
    doLoad(aSchema);
  finally
    CloseDatabase;
  end;
  aSchema.unLock;
end;

procedure   TdeDBStorage.Save(aSchema:TdeSchema);
begin
  if OpenDatabase(true) then try
    doSave(aSchema);
  finally
    CloseDatabase;
  end;
end;

(**)

{TdeCachedDBStorage}
constructor TdeCachedDBStorage.Create;
begin
  inherited Create;
  FCache     := nil;
  FCount     := 0;
  FCapacity  := 0;
  FCurrent   :=-1;
end;

destructor  TdeCachedDBStorage.Destroy;
begin
  FCache     := nil;
  FCount     := 0;
  FCapacity  := 0;
  FCurrent   :=-1;
  inherited Destroy;
end;

procedure   TdeCachedDBStorage.GrowCache;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  FCapacity := FCapacity+Delta;
  setLength(FCache,FCapacity);
end;

procedure   TdeCachedDBStorage.ClearCache;
begin
  FCache    := nil;
  FCount    := 0;
  FCapacity := 0;
  FCurrent  :=-1;
end;

function    TdeCachedDBStorage.FindCache(objID:integer;var index:integer):boolean;
var
  iMin,iMax,iMid,C : integer;
begin
  Result := false;
  iMin := 0;
  iMax := FCount-1;
  while (iMin <= iMax) do begin
    iMid := (iMin+iMax) shr 1;
    C := FCache[iMid].objID-objID;
    if (C < 0) then
      iMin := iMid+1
    else begin
      iMax := iMid-1;
      Result := (C=0);
    end;
  end;
  index := iMin;
end;

procedure   TdeCachedDBStorage.AddCache(objID,objOwner,objClass,objParent,objZOrder:integer;
                         objData:string);
var
  i,index : integer;
begin
  if not(findCache(objID,index))
  then begin
    if FCount = FCapacity then GrowCache;
    for i := FCount downto index+1 do
      FCache[i] := FCache[i-1];
    FCache[index].objID     := objID;
    FCount := FCount + 1;
  end;
  FCache[index].objOwner  := objOwner;
  FCache[index].objClass  := objClass;
  FCache[index].objParent := objParent;
  FCache[index].objZOrder := objZOrder;
  FCache[index].objData   := objData;
  FCache[index].objLevel  :=-2; //not assigned
end;

function    TdeCachedDBStorage.getMaxObjID:integer;
begin
  if (FCount = 0) then
    Result :=-1
  else
    Result := FCache[FCount-1].objID;
end;

procedure   TdeCachedDBStorage.InsertObject(var objID:integer;
                                       objOwner,objClass,objParent,objZOrder:integer;
                                       objData:string);
begin
  doInsertObject(objID,objOwner,objClass,objParent,objZOrder,objData);
  addCache(objID,objOwner,objClass,objParent,objZOrder,objData);
end;

procedure   TdeCachedDBStorage.UpdateObject(objID,objOwner,objClass,objParent,objZOrder:integer;
                                       objData:string);
begin
  doUpdateObject(objID,objOwner,objClass,objParent,objZOrder,objData);
  addCache(objID,objOwner,objClass,objParent,objZOrder,objData);
end;

function    TdeCachedDBStorage.FirstObject(var objID:integer):boolean;
begin
  FCurrent := 0;
  Result := (FCurrent < FCount);
  if Result then
    objID := FCache[FCurrent].objID
  else begin
    objID    := -1;
    FCurrent := -1;
  end;
end;

function    TdeCachedDBStorage.NextObject(var objID:integer):boolean;
begin
  inc(FCurrent);
  Result := (FCurrent < FCount);
  if Result then
    objID := FCache[FCurrent].objID
  else
    objID := -1;
end;

function    TdeCachedDBStorage.getObjectData(objID : integer;
                                             var objOwner,objClass,objParent,objZOrder:integer;
                                             var objData:string):integer;
var
  index  : integer;
begin
  if (FCurrent >= 0) and (FCache[FCurrent].objID = objID) then
    index := FCurrent
  else begin
    if not findCache(objID,index) then
      index :=-1;
  end;
  if (index >= 0) then begin
    objOwner  := FCache[index].objOwner;
    objClass  := FCache[index].objClass;
    objParent := FCache[index].objParent;
    objZOrder := FCache[index].objZOrder;
    objData   := FCache[index].objData;
  end;
  Result    := index;
end;

function    TdeCachedDBStorage.getObjectLevel(objID:integer):integer;
var
  index : integer;
begin
  if findCache(objID,index) then begin
    Result := FCache[index].objLevel;
    if Result = -2 then begin
      Result := getObjectLevel(FCache[index].objParent)+1;
      FCache[index].objLevel := Result;
    end;
  end
  else
    Result :=-3;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DeDBStorage unit initialization ...');

finalization
  DebugLog('DeDBStorage unit finalization ...');
{$ENDIF}

end.

