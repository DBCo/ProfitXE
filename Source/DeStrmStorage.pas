unit DeStrmStorage;

interface

uses
  Classes, {Graphics, }
  {G2D, }deSchema2D;


type
  TdeStreamStorage = class(TdeSchemaStorage)
  protected
    FStream   : TStream;
  public
    constructor Create(aStream:TStream);
    destructor  Destroy;override;
    procedure   Save(aSchema:TdeSchema);override;
    procedure   Load(aSchema:TdeSchema);override;
  end;

implementation

uses SysUtils, Windows,
     DeLog;

type
  TObjectRec = packed record
    iSize     : dword;
    iObjID    : integer;
    iClass    : integer;
    iParent   : integer;
    iDataSize : dword;
  end;

{TdeStreamStorage}
constructor TdeStreamStorage.Create(aStream:TStream);
begin
  inherited Create;
  FStream   := aStream;
end;

destructor  TdeStreamStorage.Destroy;
begin
  inherited Destroy;
end;

procedure   TdeStreamStorage.Save(aSchema:TdeSchema);
var
  anItem         : TdeSchemaObject;
  aLayer         : TdeSchemaLayer;
  aShortcut      : TdeSchemaShortcut;
  SchemaStartPos : int64;
  SchemaEndPos   : int64;
  SchemaSize     : dword;
  ObjRec         : TObjectRec;
  Buffer         : array of byte;
  BufferLen      : integer;
  Len            : integer;
  i,RO           : integer;
  Saved          : tStringList;
  ObjID          : integer;
  function       ItemKey(anObject:TObject):string;
  begin
    Result := IntToHex(integer(anObject),8);
  end;
  function       ObjectID(anObject:TObject):integer;
  begin
    Result := Saved.IndexOf(ItemKey(anObject));
    if (Result >= 0) then Result := integer(Saved.Objects[Result]);
  end;
begin
  if (FStream = nil) or (aSchema = nil) then exit;
  //��������� ������� � ������, ������ ������ ������
  SchemaStartPos := FStream.Position;
  //�������� ����� ��� ������ ������ �����
  SchemaSize  := sizeOf(SchemaSize);
  FStream.Write(SchemaSize,SchemaSize);

  ObjID := 0;
  Saved := tStringList.Create;
  try
    Saved.Sorted := true;

    //��������� ���� �����
    for i := 0 to aSchema.LayersCount-1 do begin
      // ������� ������ ����
      aLayer := aSchema.Layer[i];
      Len := sizeOf(integer)+length(aLayer.Name);
      // ����������� ������������ ������
      ObjRec.iSize     := sizeOf(TObjectRec)+Len;
      ObjRec.iDataSize := Len;
      ObjRec.iObjID    := ObjID;
      inc(ObjID);
      ObjRec.iParent   :=-1;
      ObjRec.iClass    :=-2;
      // �������� ������������ ������
      FStream.Write(ObjRec,sizeOf(ObjRec));
      RO := ord(aLayer.ReadOnly);
      // �������� ������ ����
      FStream.Write(RO,sizeOf(RO));
      FStream.Write(aLayer.Name[1],length(aLayer.Name));
      // ��������������� ����� ������ �����
      SchemaSize := SchemaSize + ObjRec.iSize;
      // ��������� ����������� ������ � ������� (��� ������ �����)
      Saved.AddObject(ItemKey(aLayer),tObject(ObjRec.iObjID));
    end;
    //��������� ������� �����
    BufferLen := 32768;
    setLength(Buffer,BufferLen);
    // ���������� ��� ������� ����� � Z-�������
    anItem := aSchema.First;
    while (anItem <> nil) do begin
      //������� ������ �������
      Len := BufferLen;
      if not anItem.getData(@(Buffer[0]),Len) then begin
        BufferLen := Len;
        setLength(Buffer,BufferLen);
        anItem.getData(@(Buffer[0]),Len);
      end;
      //����������� ������������ ������
      ObjRec.iSize     := sizeOf(TObjectRec)+Len;
      ObjRec.iDataSize := Len;
      ObjRec.iObjID    := ObjID;
      inc(ObjID);
      //��������� ������ �� ��������� �������
      if anItem.Parent <> nil then
        ObjRec.iParent := ObjectID(anItem.Parent)
      else if anItem.Layer <> nil then
        ObjRec.iParent := ObjectID(anItem.Layer)
      else
        ObjRec.iParent :=-1;
      ObjRec.iClass    := anItem.ClassID;
      //�������� ������������ ������
      FStream.Write(ObjRec,sizeOf(ObjRec));
      //�������� ������ �������
      FStream.Write(Buffer[0],Len);
      // ��������������� ����� ������ �����
      SchemaSize := SchemaSize + ObjRec.iSize;
      // ��������� ����������� ������ � ������� (��� ������ �����)
      Saved.AddObject(ItemKey(anItem),tObject(ObjRec.iObjID));
      // ������� � ���������� ������� � Z-�������
      anItem := aSchema.Next(anItem);
    end;
    //��������� Shortcuts
    // ���������� ��� Shortcuts
    for i := 0 to aSchema.ShortCutsCount - 1 do
    begin
      aShortcut := aSchema.Shortcut[i];
      //������� ������ �������
      Len := BufferLen;
      if not aShortcut.getData(@(Buffer[0]),Len) then begin
        BufferLen := Len;
        setLength(Buffer,BufferLen);
        aShortcut.getData(@(Buffer[0]),Len);
      end;
      //����������� ������������ ������
      ObjRec.iSize     := sizeOf(TObjectRec)+Len;
      ObjRec.iDataSize := Len;
      ObjRec.iObjID    := ObjID;
      inc(ObjID);
      //��������� ������ �� ��������� �������
      ObjRec.iParent   := ObjectID(aShortcut.LinkedObject);
      ObjRec.iClass    :=-3;
      //�������� ������������ ������
      FStream.Write(ObjRec,sizeOf(ObjRec));
      //�������� ������ �������
      FStream.Write(Buffer[0],Len);
      // ��������������� ����� ������ �����
      SchemaSize := SchemaSize + ObjRec.iSize;
    end;
    Buffer := nil;
  finally
    Saved.Free;
  end;
  //�������� ������ ������ ����� � ���������� ����� �����
  SchemaEndPos := FStream.Position;
  FStream.Seek(SchemaStartPos,soFromBeginning);
  FStream.Write(SchemaSize,sizeOf(SchemaSize));
  //������� ������� ������ � ����� ���������� ������
  FStream.Seek(SchemaEndPos,soFromBeginning);
end;

procedure   TdeStreamStorage.Load(aSchema:TdeSchema);
var
  anItem         : TdeSchemaObject;
  aLayer         : TdeSchemaLayer;
  aParent        : TdeSchemaGroup;
  aShortcut      : TdeSchemaShortcut;
  SchemaSize     : dword;
  SchemaPos      : dword;
  ObjRec         : TObjectRec;
  Buffer         : array of byte;
  BufferLen      : integer;
  Len            : integer;
  Loaded         : TStringList;
  RO             : integer;
  LName          : string;
  ParentObj      : TObject;
  i              : integer;
  function       ItemKey(anID:integer):string;
  begin
    Result := IntToHex(anID,8);
  end;
  function       ObjectByID(anID:integer):TObject;
  var index : integer;
  begin
    index := Loaded.IndexOf(ItemKey(anID));
    if (index >= 0) then Result := Loaded.Objects[index] else Result:=nil;
  end;
begin
  if (FStream = nil) or (aSchema = nil) then exit;
  if (FStream.Position > (FStream.Size-4)) then exit;
  //��������� ������ ������ �����
  FStream.Read(SchemaSize,sizeOf(SchemaSize));
  SchemaPos  := sizeOf(SchemaSize);

  aSchema.Lock;
  try

    Loaded  := TStringList.Create;
    try
      Loaded.Sorted := true;

      BufferLen := 32768;
      setLength(Buffer,BufferLen);
      try

        while (FStream.Position < FStream.Size)
              and(SchemaPos<SchemaSize)
        do begin
          //������ ������������ ������ �������
          if FStream.Read(ObjRec,sizeOf(ObjRec)) = sizeOf(ObjRec)
          then begin
            //������� ����
            if ObjRec.iClass = -2 then begin
              FStream.Read(RO,sizeOf(RO));
              Len := ObjRec.iDataSize-sizeOf(RO);
              if Len > 0 then begin
                setLength(LName,Len);
                Len := FStream.Read(LName[1],Len);
                setLength(LName,Len);
                aLayer := aSchema.LayerByName[LName];
                if aLayer <> nil then begin
                  aLayer.ReadOnly := (RO = 1);
                  Loaded.AddObject(ItemKey(ObjRec.iObjID),aLayer);
                end;
              end;
            end
            //������� Shortcut
            else if ObjRec.iClass = -3 then begin
              aShortcut := aSchema.NewShortcut;
              Len := BufferLen;
              if Len < integer(ObjRec.iDataSize) then begin
                BufferLen := ObjRec.iDataSize;
                setLength(Buffer,BufferLen);
              end;
              Len := FStream.Read(Buffer[0],ObjRec.iDataSize);
              if (aShortcut<>nil) then begin
                aShortcut.setData(@(Buffer[0]),Len);
                aShortcut.LinkedObject := TdeSchemaObject(ObjectByID(ObjRec.iParent));
              end;
            end
            //������� ������
            else begin
              ParentObj := ObjectByID(ObjRec.iParent);
              aParent := nil;
              LName   := EmptyStr;
              if ParentObj is TdeSchemaLayer then begin
                aLayer := TdeSchemaLayer(ParentObj);
                if aLayer <> nil then
                  LName := aLayer.Name;
              end
              else if ParentObj is TdeSchemaGroup then
                aParent := TdeSchemaGroup(ParentObj);
              if ObjRec.iClass = -1 then begin
                anItem := aSchema.NewGroup(aParent,LName);
                anItem.Lock;
              end
              else
                anItem := aSchema.New(ObjRec.iClass,aParent,LName);
              Loaded.AddObject(ItemKey(ObjRec.iObjID),anItem);
              Len := BufferLen;
              if Len < integer(ObjRec.iDataSize) then begin
                BufferLen := ObjRec.iDataSize;
                setLength(Buffer,BufferLen);
              end;
              Len := FStream.Read(Buffer[0],ObjRec.iDataSize);
              if (anItem<>nil) then
                anItem.setData(@(Buffer[0]),Len);
            end;
          end;
        end;

      finally
        Buffer := nil;
      end;
      // ����� ���������� � �����
      for i := Loaded.Count-1 downto 0 do begin
        anItem := TdeSchemaObject(Loaded.Objects[i]);
        anItem.unLock;
      end;

    finally
      Loaded.Free;
    end;

  finally
    aSchema.unLock;
  end;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DeStrmStorage unit initialization ...');

finalization
  DebugLog('DeStrmStorage unit finalization ...');
{$ENDIF}

end.

