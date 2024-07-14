unit XMLTable;

interface

uses SysUtils, Classes, System.Generics.Collections, DB, XMLDoc, XMLIntf,
   AbstractTable, DeDB, DeMeta, DataCacheUnit, DeTypes;

resourcestring
   ETableStructureError = 'Table structure is incorrect';
   ETableCantOpenError  = 'On table open error';
   ETableNotOpenError   = 'Table is not active';

const
  ROWSTATE = '_DE_ROWSTATE_' ;
  NodeRoot = 'xml';
      NodeRoot_s = 'xmlns:s';    NodeRoot_s_value = 'uuid:BDC6E3F0-6DA3-11d1-A2A3-00AA00C14882';
      NodeRoot_dt = 'xmlns:dt';  NodeRoot_dt_value = 'uuid:C2F41010-65B3-11d1-A29F-00AA00C14882';
      NodeRoot_rs = 'xmlns:rs';  NodeRoot_rs_value = 'urn:schemas-microsoft-com:rowset';
      NodeRoot_z = 'xmlns:z';    NodeRoot_z_value = '#RowsetSchema';

      NodeSchema = 's:Schema';
          NodeSchema_id = 'id';  NodeSchema_id_value = 'RowsetSchema';

          NodeElement = 's:ElementType';
              NodeElement_name = 'name';               NodeElement_name_value = 'row';
              NodeElement_content = 'content';         NodeElement_content_value = 'eltOnly';
              NodeElement_updatable = 'rs:updatable';  NodeElement_updatable_value = 'true';

          //  NodeElementDescription = 'Description';

              NodeAttribute = 's:AttributeType';
                  NodeAttribute_name = 'name';  // для маппинга с секцией данных
                  NodeAttribute_nameoriginal = 'rs:name'; // для человека
                  NodeAttribute_number = 'rs:number';
                  NodeAttribute_nullable = 'rs:nullable';
                  NodeAttribute_writeunknown = 'rs:writeunknown';
                  NodeAttribute_basecatalog = 'rs:basecatalog';
                  NodeAttribute_basetable = 'rs:basetable';
                  NodeAttribute_basecolumn = 'rs:basecolumn';
                  NodeAttribute_keycolumn = 'rs:keycolumn';
                  NodeAttribute_autoincrement = 'rs:autoincrement';
                  NodeAttribute_keyforegn = 'foregn';

          //      NodeAttributeDescription = 'rs:Description';

                  NodeDatatype = 's:datatype';
                      NodeDatatype_type = 'dt:type';
                      NodeDatatype_dbtype = 'rs:dbtype';
                      NodeDatatype_maxLength = 'dt:maxLength';
                      NodeDatatype_scale = 'rs:scale';
                      NodeDatatype_precision = 'rs:precision';
                      NodeDatatype_fixedlength = 'rs:fixedlength';
                      NodeDatatype_maybenull = 'rs:maybenull';
                      NodeDatatype_long = 'rs:long';               // для бинарных данных

                  NodeDefaultValue = 's:default';  // имя надо уточнить, возможно просто 'default'

              NodeExtends = 's:extends';
                  NodeExtends_type = 'type';   NodeExtends_type_value = 'rs:rowbase';

      NodeData = 'rs:data';
          Node_Row = 'z:row';

type
  TXMLDocumentISO = class;

  TXMLTable = class(TComponent)
  private
    FDoc: TXMLDocumentISO;
    FTName: String;
    FTMeta: TTableMeta;
    function GetItem(Index: Integer): IXMLNode;
  public
    constructor Create(AOwner: TXMLDocumentISO; aTableName: String); overload;
    constructor Create(AOwner: TXMLDocumentISO; aTableMeta: TTableMeta); overload;
    destructor Destroy;

    function Field_Add(const aFMeta: TFieldMeta): IXMLNode;
    function Field_Index(const aName: String): Integer;
    function Item_Add(aNode: IXMLNode = nil): IXMLNode;
    function Item_GetFieldValue(const aNode: IXMLNode; Const aIndex: Integer; Var aValue: Variant): Boolean; overload;
    function Item_GetFieldValue(const aNode: IXMLNode; Const aName: String; Var aValue: Variant): Boolean; overload;
    function Item_SetFieldValue(const aNode: IXMLNode; Const aIndex: Integer; Const aValue: Variant): Boolean; overload;
    function Item_SetFieldValue(const aNode: IXMLNode; Const aName: String; Const aValue: Variant): Boolean; overload;

    property TableMeta: TTableMeta read FTMeta;
    property TableName: String read FTName;

    function Count: Integer;
    property Items[Index: Integer]: IXMLNode read GetItem;
  end;

  TXMLFieldmeta = class(TFieldmeta)
  private
    FLinkForegnName: String;
  public
    property LinkForegnName: String read FLinkForegnName write FLinkForegnName;
  end;

  TXMLDocumentISO = class(TXMLDocument)
  private
    FShemaName: String;
    FXMLTables: TObjectList<TXMLTable>;
    FSingleMode: Boolean;
    function GetXMLString: String;
    function GetTableByName(const aName: String): Integer;
    function GetXMLTableByName(const aName: String): TXMLTable;
    function GetItemXMLTable(Index: Integer): TXMLTable;
    procedure PrepareToSave;
  protected
    procedure SetActive(const Value: Boolean); override;
  public
    FRootNode: IXMLNode;
    FSchemaNode: IXMLNode;
    FElementNode: IXMLNode;
    FExtendsNode: IXMLNode;
    FDataNode: IXMLNode;
    constructor Create(AOwner: TComponent); overload;
    destructor Destroy; overload;
    function PrepareHeader: Boolean;
    function ReadHeader: Boolean;
    function TableIndexByName(const aName: String): Integer;
    function CreateTable(aParent: TXMLTable = nil; aLinkField: string = ''): TXMLTable;
    procedure SaveToFile(const AFileName: String); overload;
    procedure SaveToFileUTF8(const AFileName: String);
    procedure SaveToStream(const Stream: TStream); overload;
    procedure SaveToStreamUTF8(const Stream: TStream);
    procedure SaveToXML(var XML: String); overload;
    procedure SaveToXMLUTF8(var XML: String);
    property Items[Index: Integer]: TXMLTable read GetItemXMLTable;
    property ShemaName: String read FShemaName write FShemaName;
  end;

  EXMLTableError = class(Exception);

  TXMLTableOld = class(TAbstractTable)
  private
    FTable   : TXMLDocument;
    RootNode : IXMLNode;
    SchemaNode : IXMLNode;
    ElementNode : IXMLNode;
    DataNode : IXMLNode;
    FCheckChanges: Boolean;
    procedure CheckTableLoaded;
    procedure CheckTableActive;
    function  GetXML: String;
    procedure SetXML(const Value: String);
    procedure CreateMainNodes;
    procedure ReadMainNodes;
    function  GetTableProp(PropName: String): Variant;
    procedure SetTableProp(PropName: String; const Value: Variant);
    function GetEncoding: String;

    procedure LoadFromFile(FileName : string);
    procedure SaveToFile(FileName : string);
  protected
    function  InternalGetDataObject: TObject; override;
    procedure InternalGetFieldValue(CurrentRecord : Pointer; FieldIndex : integer; var   Value: Variant); override;
    procedure InternalSetFieldValue(CurrentRecord : Pointer; FieldIndex : integer; const Value: Variant); override;
    procedure InternalReadFields(Fields : TCustomFields); override;
    procedure InternalReadData(RecordsList : TRecordsList); override;
    function  InternalAppendRecord(Values : array of TFieldNameValue): Pointer; override;
    function  InternalDeleteRecord(CurrentRecord : Pointer): Boolean; override;
    function  InternalGetRecordState(CurrentRecord : Pointer): TRecordState; override;
  public
    constructor Create(AOwner: TComponent);override;
    procedure RestructTable(DeleteOldFields : boolean = False);
    procedure PostChanges;
    procedure Assign(aTable : TXMLTableOld);  reintroduce;
    procedure AssignOnlyChanges(aTable : TXMLTableOld);
    property XML : String read GetXML write SetXML;
    property TableProperty[PropName : String] : Variant read GetTableProp write SetTableProp;
    property CheckChanges : Boolean read FCheckChanges write FCheckChanges;
    property Encoding: String read GetEncoding;
  end;

  function FindChildByName(ParentNode: IXMLNode; ChildName: WideString): IXMLNode;
  function FindChildByAttrValue(ParentNode: IXMLNode; AttrName : WideString; AttrValue : Variant): IXMLNode;
  function FindAttrByName (ParentNode: IXMLNode; AttrName : WideString): IXMLNode;
  function FindAttrValue  (ParentNode: IXMLNode; AttrName : WideString): Variant;

  function FieldTypeByTypeName(tName : string; stName : string ): TFieldType;

  procedure LockDC;
  procedure UnLockDC;


implementation

uses ActiveX, Variants, NetEncoding,
     DeDataset, DeLog, Funcs;

{ Service Functions }

function FindChildByName(ParentNode: IXMLNode; ChildName: WideString): IXMLNode;
var
  i : integer;
begin
  result := nil;
  if Assigned(ParentNode) and Assigned(ParentNode.ChildNodes) then
    for i:=0 to ParentNode.ChildNodes.Count -1 do
      if WideSameText(ParentNode.ChildNodes[i].NodeName, ChildName) then
      begin
        result := ParentNode.ChildNodes[i];
        Break;
      end;
end;

function FindAttrByName(ParentNode: IXMLNode;  AttrName: WideString): IXMLNode;
var
  i : integer;
  Node : IXMLNode;
begin
  result:= nil;
  if Assigned(ParentNode) and Assigned(ParentNode.AttributeNodes) then
   for i:=0 to ParentNode.AttributeNodes.Count -1 do
     begin
       Node:= ParentNode.AttributeNodes.Nodes[i];
       if WideSameText(Node.NodeName, AttrName) then Exit(Node);
     end;
end;

function FindAttrValue (ParentNode: IXMLNode; AttrName  : WideString): Variant;
var
 aNode :  IXMLNode;
begin
  aNode := FindAttrByName(ParentNode,AttrName);
  if Assigned(aNode) then
    result := aNode.NodeValue
  else
    result := UnAssigned;
end;

function ReadAsString (ParentNode: IXMLNode; AttrName  : WideString): Variant;
var
 aNode :  IXMLNode;
begin
  aNode := FindAttrByName(ParentNode,AttrName);
  if Assigned(aNode) then result:= aNode.NodeValue
                     else result:= UnAssigned;
end;

function ReadAsInteger (ParentNode: IXMLNode; AttrName : WideString): Variant;
var
  aNode : IXMLNode;
  iValue: Int64;
begin
  aNode := FindAttrByName(ParentNode,AttrName);
  if Assigned(aNode) and  TryStrToInt64( aNode.NodeValue, iValue)
    then Result:= iValue
    else Result:= UnAssigned;
end;

function ReadAsBoolean (ParentNode: IXMLNode; AttrName : WideString): Variant;
var
  aNode : IXMLNode;
  iValue: Boolean;
begin
  aNode := FindAttrByName(ParentNode,AttrName);
  if Assigned(aNode) and  TryStrToBool( aNode.NodeValue, iValue)
    then Result:= iValue
    else Result:= UnAssigned;
end;

function FindChildByAttrValue(ParentNode: IXMLNode; AttrName : WideString; AttrValue : Variant): IXMLNode;
var
  i : integer;
  aNode :  IXMLNode;
begin
  result := nil;
  for i:= 0 to ParentNode.ChildNodes.Count -1 do
  begin
    aNode := ParentNode.ChildNodes[i];
    if VarSameValue(FindAttrValue(aNode,AttrName), AttrValue) then
    begin
      result := aNode;
      Break;
    end;
  end;
end;

function FieldTypeByTypeName(tName : string; stName : string ): TFieldType;
begin
  if SameText(tName,'string') then
    result := ftString
  else
  if SameText(tName,'widestring') then
    result := ftWideString
  else
  if SameText(tName,'smallint')or SameText(tName,'i2') then
    result := ftSmallInt
  else
  if SameText(tName,'integer')or SameText(tName,'i4')  then
    result := ftInteger
  else
  if SameText(tName,'float')or SameText(tName,'r8') then
    result := ftFloat
  else
  if SameText(tName,'boolean')or SameText(tName,'bool') then
    result := ftBoolean
  else
  if SameText(tName,'date')then
    result :=  ftDate
  else
  if SameText(tName,'datetime'){ or SameText(tName,'time')}then
    result :=  ftDateTime
  else
  if SameText(tName,'blob') then
     result := ftBLOB
  else
  if SameText(tName,'memo') then
    result := ftMemo
  else
  if SameText(tName,'bin.hex') then
  begin
    if SameText(stName,'text') then
      result := ftMemo
    else
      result := ftBLOB;
  end
  else
    result := ftUnknown;
end;

var
  DC : CHar;

procedure LockDC;
begin
  DC := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
end;

procedure UnLockDC;
begin
  FormatSettings.DecimalSeparator := DC;
end;

{ TXMLDocumentISO }

constructor TXMLDocumentISO.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXMLTables:= TObjectList<TXMLTable>.Create;
  FRootNode:= nil;
  FSchemaNode:= nil;
  FElementNode:= nil;
  FExtendsNode:= nil;
  FDataNode:= nil;
end;

destructor TXMLDocumentISO.Destroy;
begin
  FXMLTables.Free;
  inherited Destroy;
end;

function TXMLDocumentISO.CreateTable(aParent: TXMLTable = nil; aLinkField: string = ''): TXMLTable;
begin
 //
end;

function TXMLDocumentISO.GetItemXMLTable(Index: Integer): TXMLTable;
begin
  Result:= FXMLTables[Index];
end;

function TXMLDocumentISO.GetTableByName(const aName: String): Integer;
var i: Integer;
begin
  Result:= -1;
  for i:=0 to Pred(FXMLTables.Count) do
    if Sametext(FXMLTables[i].FTName, aName) then Exit(i);
end;

function TXMLDocumentISO.GetXMLString: String;
begin
end;

function TXMLDocumentISO.GetXMLTableByName(const aName: String): TXMLTable;
var N: Integer;
begin
  N:= GetTableByName(aName);
  if -1 < N then Result:= FXMLTables[N]
            else Result:= TXMLTable.Create(self, aName);
end;

function TXMLDocumentISO.PrepareHeader: Boolean;
begin
  Active:= True;
  if Not Active then raise Exception.Create('Document not active');

  if Node.ChildNodes.Count = 0 then
    begin  // Создаем главные узлы
      FRootNode := Node.AddChild(NodeRoot);
        FRootNode.Attributes[NodeRoot_s] := NodeRoot_s_value;
        FRootNode.Attributes[NodeRoot_dt] := NodeRoot_dt_value;
        FRootNode.Attributes[NodeRoot_rs] := NodeRoot_rs_value;
        FRootNode.Attributes[NodeRoot_z] := NodeRoot_z_value;
      FSchemaNode := FRootNode.AddChild(NodeSchema);
        FSchemaNode.Attributes[NodeSchema_id] := NodeSchema_id_value;
      FElementNode := FSchemaNode.AddChild(NodeElement);
        FElementNode.Attributes[NodeElement_name] := NodeElement_name_value;
        FElementNode.Attributes[NodeElement_content] := NodeElement_content_value;
        FElementNode.Attributes[NodeElement_updatable] := NodeElement_updatable_value;
      FDataNode := FRootNode.AddChild(NodeData);
    end
end;

function TXMLDocumentISO.ReadHeader: Boolean;
var i: Integer;
    v, v_type, v_dbtype : String;
    v_autoinc, v_maxLength, v_fixedlength, v_scale, v_precision, v_long: Variant;
    FNode, FSubNode : IXMLNode;
    FMeta: TXMLFieldMeta;
    FXMLTable: TXMLTable;

  function StrToIntVar(aValue: String): Variant;
  var i64: Int64;
  begin
    if TryStrToInt64(aValue, i64) then result:= i64
                                  else result:= Unassigned;
  end;

begin
  if Not Active then raise Exception.Create('Document not active');

  if not(FRootNode = nil) then Exit(True);

  FRootNode:= nil;
  FSchemaNode:= nil;
  FElementNode:= nil;
  FExtendsNode:= nil;
  FDataNode:= nil;

  // Читаем главные узлы
  for i:=0 to Pred(Node.ChildNodes.Count) do // перебором, т.к. мешает тег <?xml version="1.0" codepage="UTF-8"?>
    begin
      FNode:= Node.ChildNodes.Get(i);
      if (FNode.NodeType = ntElement) and (FNode.NodeName = NodeRoot) then
        FRootNode := FNode;
    end;
  if not Assigned(FRootNode) then raise Exception.Create('Node "' + NodeRoot + '" not Exists');

  FSchemaNode := FRootNode.ChildNodes.FindNode(NodeSchema);
  if not Assigned(FSchemaNode) then raise Exception.Create('Node "' + NodeSchema + '" not Exists');

  FElementNode := FSchemaNode.ChildNodes.FindNode(NodeElement);
  if not Assigned(FElementNode) then raise Exception.Create('Node "' + NodeElement + '" not Exists');

  FDataNode := FRootNode.ChildNodes.FindNode(NodeData);
  if not Assigned(FDataNode) then raise Exception.Create('Node "' + NodeData + '" not Exists');


  // читаем поля
  for i:=0 to Pred(FElementNode.ChildNodes.Count) do
    begin
      FNode:= FElementNode.ChildNodes.Get(i);
      if FNode.NodeName = NodeAttribute then
        begin
          FSubNode:= FNode.ChildNodes.FindNode(NodeDatatype);
          if not Assigned(FSubNode) then Continue;

          v:=                               FNode.Attributes[NodeAttribute_basetable];

          FXMLTable:= GetXMLTableByName(v);
          FMeta:= TXMLFieldmeta.Create;
          FMeta.Owner:= FXMLTable.FTMeta;
          FXMLTable.FTMeta.Fields.Add(FMeta);

          FMeta.IsStored:= True;
          FMeta.Calculated:= False;
          FMeta.IsStored:= True;
          FMeta.Link:= 0;
          FMeta.LinkType:= daNone;
          FMeta.Role:= [];
          if FMeta.Key then FMeta.VisibleLevel:= fvLevel1
                       else FMeta.VisibleLevel:= fvLevel2;
          FMeta.ShowType:= stNative;
          FMeta.Width:= 20;
          FMeta.Template:= EmptyStr;
          FMeta.DefaultValue:= EmptyStr;
          FMeta.DefaultDefDB:= EmptyStr;
          FMeta.Value1:= EmptyStr;
          FMeta.Value2:= EmptyStr;

          FMeta.Name:=                      FNode.Attributes[NodeAttribute_name];
          FMeta.Original:=                  FNode.Attributes[NodeAttribute_basecolumn];
          FMeta.Key:=      SameText(        FNode.Attributes[NodeAttribute_keycolumn], TrueLexeme );
          FMeta.NotNull:=  SameText(        FNode.Attributes[NodeAttribute_nullable], FalseLexeme )
                       or  SameText(     FSubNode.Attributes[NodeDatatype_maybenull], FalseLexeme );
          FMeta.Order:=                     FNode.Attributes[NodeAttribute_number];
          FMeta.LinkForegnName:=            FNode.Attributes[NodeAttribute_keyforegn];

          v_autoinc:=      SameText(        FNode.Attributes[NodeAttribute_autoincrement], TrueLexeme );
          v_type:=                       FSubNode.Attributes[NodeDatatype_type];
          v_dbtype:=                     FSubNode.Attributes[NodeDatatype_dbtype];
          v_maxLength:=    StrToIntVar(  FSubNode.Attributes[NodeDatatype_maxLength] );
          v_fixedlength:=  SameText(     FSubNode.Attributes[NodeDatatype_fixedlength], TrueLexeme );
          v_scale:=        StrToIntVar(  FSubNode.Attributes[NodeDatatype_scale] );
          v_precision:=    StrToIntVar(  FSubNode.Attributes[NodeDatatype_precision] );
          v_long:=         SameText(     FSubNode.Attributes[NodeDatatype_long], TrueLexeme );

          // разбираемся с типом данных
          if SameText(v_type, 'boolean') then  begin FMeta.DataType:= ftBoolean; end else
          if SameText(v_type, 'string') then
            begin
              if SameText(v_dbtype, 'str') then
                begin
                  if v_long = True then        begin FMeta.DataType:= ftMemo; end else
                  if v_fixedlength = True then begin FMeta.DataType:= ftFixedChar; FMeta.DataSize:= v_maxLength;  end else
                                               begin FMeta.DataType:= ftString; FMeta.DataSize:= v_maxLength;  end;
                end else
                begin
                  if v_long = True then        begin FMeta.DataType:= ftWideMemo; end else
                  if v_fixedlength = True then begin FMeta.DataType:= ftFixedWideChar; FMeta.DataSize:= v_maxLength;  end else
                                               begin FMeta.DataType:= ftWideString; FMeta.DataSize:= v_maxLength;  end;
                end;
            end else
          if SameText(v_type, 'time') then     begin FMeta.DataType:= ftTime; end else
          if SameText(v_type, 'date') then     begin FMeta.DataType:= ftDate; end else
          if SameText(v_type, 'datetime') then
            if SameText(v_dbtype, 'timestamp')
              then if v_maxLength = 18 then    begin FMeta.DataType:= ftTimeStampOffset; end
                                       else    begin FMeta.DataType:= ftTimeStamp; end
              else                             begin FMeta.DataType:= ftDateTime; end else
          if SameText(v_type, 'ui1') then      begin FMeta.DataType:= ftByte; end else
          if SameText(v_type, 'ui2') then      begin FMeta.DataType:= ftWord; end else
          if SameText(v_type, 'ui4') then      begin FMeta.DataType:= ftLongWord; end else
          if SameText(v_type, 'i1') then
            if v_autoinc = True then           begin FMeta.DataType:= ftAutoInc; FMeta.DataSize:= 1; end
                                else           begin FMeta.DataType:= ftShortInt; end else
          if SameText(v_type, 'i2') then
            if v_autoinc = True then           begin FMeta.DataType:= ftAutoInc; FMeta.DataSize:= 2; end
                                else           begin FMeta.DataType:= ftSmallInt; end else
          if SameText(v_type, 'int') then
            if v_autoinc = True then           begin FMeta.DataType:= ftAutoInc; FMeta.DataSize:= 4; end
                                else           begin FMeta.DataType:= ftInteger; end else
          if SameText(v_type, 'i8') then
            if v_autoinc = True then           begin FMeta.DataType:= ftAutoInc; FMeta.DataSize:= 8; end
                                else           begin FMeta.DataType:= ftLargeint; end else
          if SameText(v_type, 'float') then    begin FMeta.DataType:= ftFloat; end else
          if SameText(v_type, 'number') then
            if SameText(v_dbtype, 'currency')
              then                             begin FMeta.DataType:= ftCurrency; FMeta.DataSize:= 8; end
              else                             begin FMeta.DataType:= ftBCD; FMeta.DataSize:= v_maxLength; FMeta.Precision:= v_precision; end else
          if SameText(v_type, 'uuid') then     begin FMeta.DataType:= ftGUID; end else
          if SameText(v_type, 'bin.hex') then
                begin
                  if v_long = True then        begin FMeta.DataType:= ftBlob; end else
                  if v_fixedlength = True then begin FMeta.DataType:= ftBytes; FMeta.DataSize:= v_maxLength;  end else
                                               begin FMeta.DataType:= ftVarBytes; FMeta.DataSize:= v_maxLength;  end;
                end else
                                               begin FMeta.DataType:= ftUnknown; end;

          FSubNode:= FNode.ChildNodes.FindNode(NodeDefaultValue);
          if assigned(FSubNode) then
            FMeta.DefaultDefDB:= FSubNode.Text;
        end;
    end;

  Result:= True;
end;

procedure TXMLDocumentISO.PrepareToSave;
begin
  if not Assigned(FExtendsNode) then
    begin
      // Добавляем запись в конце списка полей
      FExtendsNode:= FElementNode.AddChild(NodeExtends);
      FExtendsNode.Attributes[NodeExtends_type]:= NodeExtends_type_value;
    end;
end;

procedure TXMLDocumentISO.SaveToFile(const AFileName: String);
begin
  PrepareToSave;
  inherited SaveToFile(AFileName);
end;

procedure TXMLDocumentISO.SaveToFileUTF8(const AFileName: String);
begin
  PrepareToSave;
  SetEncoding('UTF-8');
  inherited SaveToFile(AFileName);
end;

procedure TXMLDocumentISO.SaveToStream(const Stream: TStream);
begin
  PrepareToSave;
  inherited SaveToStream(Stream);
end;

procedure TXMLDocumentISO.SaveToStreamUTF8(const Stream: TStream);
begin
  PrepareToSave;
  SetEncoding('UTF-8');
  inherited SaveToStream(Stream);
end;

procedure TXMLDocumentISO.SaveToXML(var XML: String);
begin
  PrepareToSave;
  inherited SaveToXML(XML);
end;

procedure TXMLDocumentISO.SaveToXMLUTF8(var XML: String);
begin
  PrepareToSave;
  SetEncoding('UTF-8');
  inherited SaveToXML(XML);
end;

procedure TXMLDocumentISO.SetActive(const Value: Boolean);
var DataAssigned: Boolean;
begin
  if (Value = Active) then Exit;
  inherited;

  FRootNode:= nil;
  FSchemaNode:= nil;
  FElementNode:= nil;
  FExtendsNode:= nil;
  FDataNode:= nil;
end;

function TXMLDocumentISO.TableIndexByName(const aName: String): Integer;
var i: Integer;
begin
  Result:= -1;
  for i:= 0 to Pred(FXMLTables.Count) do
    if SameText(aName, FXMLTables[i].FTName) then Exit(i);
end;

{ TXMLTableOld }

constructor TXMLTableOld.Create(AOwner: TComponent);
begin
  inherited;
  FTable := TXMLDocument.Create(self);
  FTable.Active := True;
  FTable.Version:= '1.0';
//  FTable.StandAlone := 'yes';
  FTable.Encoding :=  'windows-1251';
  CreateMainNodes;
  FCheckChanges := False;
end;

function TXMLTableOld.GetXML: String;
begin
  LockDC;
  FTable.NodeIndentStr:= #9;
  FTable.Options := FTable.options+[doNodeAutoIndent];
  RESULT := FTable.XML.Text;
  UnLockDC;
end;

procedure TXMLTableOld.SetXML(const Value: String);
begin
  LockDC;
  RootNode := nil;
  SchemaNode := nil;
  ElementNode := nil;
  DataNode := nil;

  Active := False;
  Fields.Clear;    //?????
  FTable.Active := False;
  try
    FTable.XML.Clear;
    FTable.XML.Text := Value;
    FTable.Active := True;
  except
    on E: Exception do
      Funcs.WriteLog('SetXML error: ' + E.Message);
  end;

  ReadMainNodes;
  UnLockDC;
end;

procedure TXMLTableOld.CheckTableLoaded;
begin
   if not FTable.Active then
   try
     FTable.Active := True;
   except
      EXMLTableError.Create(ETableCantOpenError);
   end;
end;

procedure TXMLTableOld.CheckTableActive;
begin
  CheckTableLoaded;
  if not Active then
    raise EXMLTableError.Create(ETableNotOpenError);
end;


procedure TXMLTableOld.InternalGetFieldValue(CurrentRecord: Pointer;
  FieldIndex: integer; var Value: Variant);
var
  s{,t }: String;
  //l   : Integer;
begin
  CheckTableActive;
  if Fields[FieldIndex].DataType=ftBlob then
    begin
//      s:=FindAttrValue(IXMLNode(CurrentRecord),Fields[FieldIndex].Name);
//      Value := TBase64Encoding.Base64.Decode(s);
    end
  else
    if Fields[FieldIndex].DataType in StringTypes then
    Value := XMLDecode (FindAttrValue(IXMLNode(CurrentRecord),Fields[FieldIndex].Name))
  else
    Value := FindAttrValue(IXMLNode(CurrentRecord),Fields[FieldIndex].Name);
end;


procedure TXMLTableOld.InternalSetFieldValue(CurrentRecord: Pointer; FieldIndex: integer; const Value: Variant);
const
  Convert: array[0..15] of AnsiChar = '0123456789abcdef';
var
  Node : IXMLNode;
  b: Byte;
  s    : OleVariant;
  i,vSize: Integer;
  sHEX: AnsiString;
begin
  CheckTableActive;
  Node := IXMLNode(CurrentRecord);
  if FCheckChanges and (RecState = rsNotchanged) then
    Node.Attributes[ROWSTATE]:='MOD';
  if Fields[FieldIndex].DataType=ftBlob then
    begin
//      Node.Attributes[Fields[FieldIndex].Name] := TBase64Encoding.Base64.Encode(VarToStr(Value));
      vSize := VarArrayHighBound(Value,1) - VarArrayLowBound(Value,1) + 1;
      setLength(sHEX, 2*vSize);
      for i := VarArrayLowBound(Value,1) to VarArrayHighBound(Value,1)do
        begin
          b:= VarArrayGet(Value, i);
          sHEX[i*2 + 0] := Convert[b shr 4];
          sHEX[i*2 + 1] := Convert[b and $F];
        end;
     setLength(sHEX, 2*vSize);
     s:= sHex;
     Node.Attributes[Fields[FieldIndex].Name] := s;
    end
  else
    if Fields[FieldIndex].DataType in StringTypes then
    Node.Attributes[Fields[FieldIndex].Name]:= XMLEncode(Value)
  else
    Node.Attributes[Fields[FieldIndex].Name]:= Value;
end;

procedure TXMLTableOld.CreateMainNodes;
begin
  CheckTableLoaded;
  try
    RootNode := FTable.Node.AddChild(NodeRoot);
      RootNode.Attributes[NodeRoot_s] := NodeRoot_s_value;
      RootNode.Attributes[NodeRoot_dt] := NodeRoot_dt_value;
      RootNode.Attributes[NodeRoot_rs] := NodeRoot_rs_value;
      RootNode.Attributes[NodeRoot_z] := NodeRoot_z_value;
    SchemaNode := RootNode.AddChild(NodeSchema);
      SchemaNode.Attributes[NodeSchema_id] := NodeSchema_id_value;
    ElementNode := SchemaNode.AddChild(NodeElement);
      ElementNode.Attributes[NodeElement_name] := NodeElement_name_value;
      ElementNode.Attributes[NodeElement_content] := NodeElement_content_value;
      ElementNode.Attributes[NodeElement_updatable] := NodeElement_updatable_value;
    DataNode := RootNode.AddChild(NodeData);
  except
  end;

  Assert( Assigned(RootNode), ETableStructureError);
  Assert( Assigned(SchemaNode), ETableStructureError);
  Assert( Assigned(ElementNode), ETableStructureError);
  Assert( Assigned(DataNode), ETableStructureError);
end;
 
procedure TXMLTableOld.ReadMainNodes;
begin
  CheckTableLoaded;
  if not Assigned(RootNode) then
    begin
      RootNode := FindChildByName(FTable.Node, NodeRoot);
      Assert( Assigned(RootNode), ETableStructureError);
    end;

  if not Assigned(SchemaNode) then
    begin
      SchemaNode := FindChildByName(RootNode, NodeSchema);
      Assert( Assigned(SchemaNode), ETableStructureError);
    end;

  if not Assigned(ElementNode) then
    begin
      ElementNode := FindChildByName(SchemaNode, NodeElement);
      Assert( Assigned(ElementNode), ETableStructureError);
    end;

  if not Assigned(DataNode) then
    begin
      DataNode := FindChildByName(RootNode, NodeData);
      Assert( Assigned(DataNode), ETableStructureError);
    end;
end;

procedure TXMLTableOld.InternalReadFields(Fields: TCustomFields);
var
  i : integer;
  Field : TField;
  iField: IXMLNode;
  iType: IXMLNode;
  iDefault: IXMLNode;

  FieldDef: TFieldDef;

  _name: Variant;
  _nameoriginal: Variant;
  _nullable: Variant;
  _keycolumn: Variant;
  _autoincrement: Variant;

  _type: Variant;
  _dbtype: Variant;
  _maxLength: Variant;
  _scale: Variant;
  _precision: Variant;
  _fixedlength: Variant;
  _maybenull: Variant;
  _long: Variant;

  _default: String;
begin
  ReadMainNodes;

  for i:=0 to ElementNode.ChildNodes.Count-1 do
    if WideSameText(ElementNode.ChildNodes.Nodes[i].NodeName, NodeAttribute) then
      try
        iField:= ElementNode.ChildNodes.Nodes[i];
        _name:=          ReadAsString (iField, NodeAttribute_name);
        _nameoriginal:=  ReadAsString (iField, NodeAttribute_nameoriginal);
        _nullable:=      ReadAsBoolean(iField, NodeAttribute_nullable);
        _keycolumn:=     ReadAsBoolean(iField, NodeAttribute_keycolumn);
        _autoincrement:= ReadAsBoolean(iField, NodeAttribute_autoincrement);

        iType:= FindChildByName(iField, NodeDatatype);
        if Not assigned(iType) then iType:= iField;
        _type:=        ReadAsString (iType, NodeDatatype_type);
        _dbtype:=      ReadAsString (iType, NodeDatatype_dbtype);
        _maxLength:=   ReadAsInteger(iType, NodeDatatype_maxLength);
        _scale:=       ReadAsInteger(iType, NodeDatatype_scale);
        _precision:=   ReadAsInteger(iType, NodeDatatype_precision);
        _fixedlength:= ReadAsInteger(iType, NodeDatatype_fixedlength);
        _maybenull:=   ReadAsBoolean(iType, NodeDatatype_maybenull);
        _long:=        FindAttrValue(iType, NodeDatatype_long);

        iDefault:= FindChildByName(iField, NodeDefaultValue);
        if Not assigned(iDefault) then _Default:= unassigned
                                  else _Default:= iDefault.Text;

        Field:= TField.Create(self);
        Field.FieldName:= _name;

        FieldDef.Name:= _name;
        FieldDef.DisplayName:= _nameoriginal;
        FieldDef.Size:= _maxLength;
        FieldDef.Precision:= _precision;
        if (_nullable = False) or (_maybenull = False) then FieldDef.Required:= True else
                                                            FieldDef.Required:= False;
//      if (_nullable = False) or (_maybenull = False) then FieldDef.Required:= True else
//      if (_nullable = True) or (_maybenull = True) then   FieldDef.Required:= False else
//                                                          FieldDef.Required:= unassigned;

        if SameText(_type, 'string') then
          begin
            FieldDef.DataType:= ftString;
          end else
        if SameText(_type, 'int') then
          begin
            case _maxLength of
              1: FieldDef.DataType:= ftShortInt;
              2: FieldDef.DataType:= ftSmallInt;
              8: FieldDef.DataType:= ftLargeint;
              else FieldDef.DataType:= ftInteger;
            end;
          end else
          begin
            FieldDef.DataType:= ftString; // по умолчанию
          end;

        Field.SetFieldProps(FieldDef);

      except
      end;
end;

procedure TXMLTableOld.InternalReadData(RecordsList: TRecordsList);
var
  i : integer;
begin
  ReadMainNodes;
  LockDC;
  for i:=0 to DataNode.ChildNodes.Count-1 do
  begin
    if (WideSameText(DataNode.ChildNodes.Nodes[i].NodeName, Node_Row))
    and(DataNode.ChildNodes.Nodes[i].AttributeNodes.Count >0) then
       RecordsList.Add(Pointer(DataNode.ChildNodes.Nodes[i]));
  end;
  UnLockDC;
end;

function TXMLTableOld.InternalAppendRecord(Values: array of TFieldNameValue): Pointer;
var
  Node : IXMLNode;
  i,N : integer;
begin
  result := nil;
  CheckTableActive;
  if Assigned(DataNode) then
  try
    Node :=  DataNode.AddChild(Node_Row);
    if FCheckChanges then
      Node.Attributes[ROWSTATE]:='INS';
    for i := Low(Values) to High(Values) do
      begin
        N:= FieldIndexByName(Values[i].Name);
        if -1 < N then
          Node.Attributes[Values[i].Name] := Values[i].Value;
      end;
    result := Pointer(Node);
  except
  end
end;

function TXMLTableOld.InternalDeleteRecord(CurrentRecord : Pointer): Boolean;
begin
  result := false;
  CheckTableActive;
  if Assigned(DataNode) then
  try
    if FCheckChanges then
      IXMLNode(CurrentRecord).Attributes[ROWSTATE] := 'DEL'
    else
    begin
      DataNode.ChildNodes.Delete(DataNode.ChildNodes.IndexOf(IXMLNode(CurrentRecord)));
      result := true;
    end;
  except
  end
end;

function TXMLTableOld.InternalGetDataObject: TObject;
begin
  result := FTable;
end;

function TXMLTableOld.InternalGetRecordState(CurrentRecord: Pointer): TRecordState;
var
  RState : Variant;
begin
  RState := FindAttrValue(IXMLNode(CurrentRecord),ROWSTATE);
  if VarIsEmpty(RState)then
    result := rsNotchanged
  else
  begin
    if (WideSameText(VarToStr(RState),'DEL'))then result := rsDeleted else
    if (WideSameText(VarToStr(RState),'MOD'))then result := rsModified else
    if (WideSameText(VarToStr(RState),'INS'))then result := rsInserted else
                                                  result := rsNotchanged;
  end;
end;

function TXMLTableOld.GetEncoding: String;
begin
  Result:= FTable.Encoding;
end;

function TXMLTableOld.GetTableProp(PropName: String): Variant;
begin
  try
    ReadMainNodes;
  except
    CreateMainNodes;
  end;  
  result := FindAttrValue(ElementNode, PropName);
end;

procedure TXMLTableOld.SetTableProp(PropName: String; const Value: Variant);
begin
  try
    ReadMainNodes;
  except
    CreateMainNodes;
  end;
  ElementNode.Attributes[PropName]:= Value;
end;

procedure TXMLTableOld.RestructTable(DeleteOldFields: boolean);
var
  FNode: IXMLNode;
  i : integer;
begin
  try
    ReadMainNodes;
  except
    CreateMainNodes;
  end;

  if DeleteOldFields then
    for  i:= ElementNode.ChildNodes.Count -1 downto 0 do
      ElementNode.ChildNodes.Delete(i);
  {}
  for  i := 0 to Fields.Count -1 do
  begin
    FNode := FindChildByAttrValue(ElementNode, NodeAttribute_name, Fields[i].Name);
    if not Assigned(FNode)then
      FNode := ElementNode.AddChild(NodeAttribute);
    FNode.Attributes[NodeAttribute_name]:= Fields[i].Name;

    FNode.Attributes['fieldtype']:= FieldTypeNames[Fields[i].DataType];
    if Fields[i].Size > 0 then
      FNode.Attributes['WIDTH']:= Fields[i].Size;
  end;
end;

procedure TXMLTableOld.PostChanges;
var
  i : integer;
  Node, Attr : IXMLNode;
begin
  CheckTableActive;
  for i:=DataNode.ChildNodes.Count-1 downto 0 do
    if (WideSameText(DataNode.ChildNodes.Nodes[i].NodeName, Node_Row)) then
    begin
      Node := DataNode.ChildNodes.Nodes[i];
      Attr := FindAttrByName(Node,ROWSTATE);
      if Assigned(Attr)then
        if (WideSameText(Attr.NodeValue,'INS')) or (WideSameText(Attr.NodeValue,'MOD')) then
          Node.AttributeNodes.Delete(ROWSTATE) else
        if (WideSameText(Attr.NodeValue,'DEL'))then
          try
            DataNode.ChildNodes.Delete(DataNode.ChildNodes.IndexOf(Node));
          except
          end;
    end;
  ReadData;
end;

procedure TXMLTableOld.LoadFromFile(FileName: string);
var
  TmpStrings : TStringList;
begin
  TmpStrings := TStringList.Create;
  try
    TmpStrings.LoadFromFile(FileName);
  except
  end;
  XML := TmpStrings.Text;
  TmpStrings.Free;
end;

procedure TXMLTableOld.SaveToFile(FileName: string);
var
  TmpStrings : TStringList;
begin
  TmpStrings := TStringList.Create;
  try
    PostChanges;
    TmpStrings.Text := XML;
    TmpStrings.SaveToFile(FileName);
  finally
    TmpStrings.Free;
  end;
end;

procedure TXMLTableOld.Assign(aTable :TXMLTableOld);
begin
  XML := aTable.XML;
end;

procedure TXMLTableOld.AssignOnlyChanges(aTable :TXMLTableOld);
var
  i : integer;
  Node, Attr : IXMLNode;
begin
  Assign(aTable);
  CheckTableLoaded;
  for i:=DataNode.ChildNodes.Count-1 downto 0 do
    if (WideSameText(DataNode.ChildNodes.Nodes[i].NodeName, Node_Row)) then
    begin
      Node := DataNode.ChildNodes.Nodes[i];
      Attr := FindAttrByName(Node,ROWSTATE);
      if not Assigned(Attr)then
        try
          DataNode.ChildNodes.Delete(DataNode.ChildNodes.IndexOf(Node));
        except
        end;
    end;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('XMLTable unit initialization ...');
  {$ENDIF}
  CoInitialize(nil);
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('XMLTable unit finalization ...');
  {$ENDIF}
  CoUnInitialize;
end;
{$ENDREGION}

{ TXMLTable }
constructor TXMLTable.Create(AOwner: TXMLDocumentISO; aTableMeta: TTableMeta);
begin
  FDoc:= aOwner;
  FTMeta:= aTableMeta;
  FTName:= aTableMeta.Table;
end;

constructor TXMLTable.Create(AOwner: TXMLDocumentISO; aTableName: String);
begin
  FDoc:= aOwner;
  FTName:= aTableName;
  FTMeta:= TTableMeta.Create;
  FTMeta.ObjectType:= otTable;
  FTMeta.SetTable:= aTableName;
  FTMeta.IsDynamic:= True;
  AOwner.FXMLTables.Add(self);
end;

function TXMLTable.Count: Integer;
begin
  if assigned(FDoc) and Assigned(FDoc.FDataNode) then Result:= FDoc.FDataNode.ChildNodes.Count
                                                 else Result:= 0;
end;

function TXMLTable.GetItem(Index: Integer): IXMLNode;
begin
  if assigned(FDoc) and Assigned(FDoc.FDataNode) then Result:= FDoc.FDataNode.ChildNodes[Index]
                                                 else Result:= nil;
end;

function TXMLTable.Item_Add(aNode: IXMLNode): IXMLNode;
begin
  if aNode = nil then Result:= FDoc.FDataNode.AddChild(Node_Row)
                 else Result:= aNode.AddChild(Node_Row);
end;

function TXMLTable.Item_GetFieldValue(const aNode: IXMLNode; const aIndex: Integer; var aValue: Variant): Boolean;
var FM: TFieldMeta;
begin
  Result:= (0 <= aIndex) and (aIndex < FTMeta.Fields.Count);

  if Result then
    begin
      FM:= FTMeta.Fields[aIndex];
      aValue:= StringISOToValue(aNode.Attributes[FM.Original], FM.DataType, Null);
    end;
end;

function TXMLTable.Item_GetFieldValue(const aNode: IXMLNode; const aName: String; var aValue: Variant): Boolean;
var N: Integer;
begin
  N:= FTMeta.Fields.IndexByName(aName);
  if -1 < N then Item_GetFieldValue(aNode, N, aValue)
            else raise Exception.Create('Field ''' + aName + ''' not Exists');
end;

function TXMLTable.Item_SetFieldValue(const aNode: IXMLNode; const aIndex: Integer; const aValue: Variant): Boolean;
begin
  if FTMeta.Fields[aIndex].DataType in StringTypes
    then aNode.Attributes[FTMeta.Fields[aIndex].Original]:= aValue
    else aNode.Attributes[FTMeta.Fields[aIndex].Original]:=
                                                     ValueToStringISO(aValue, FTMeta.Fields[aIndex].DataType, EmptyStr);
end;

function TXMLTable.Item_SetFieldValue(const aNode: IXMLNode; const aName: String; const aValue: Variant): Boolean;
var N: Integer;
begin
  N:= FTMeta.Fields.IndexByName(aName);
  if -1 < N then Item_SetFieldValue(aNode, N, aValue)
            else raise Exception.Create('Field ''' + aName + ''' not Exists');

end;

destructor TXMLTable.Destroy;
begin
  //
end;

function TXMLTable.Field_Index(const aName: String): Integer;
var i: Integer;
begin
  Result:= -1;
  for i:=0 to Pred(FTMeta.Fields.Count) do
    if SameText(aName, FTMeta.Fields[i].Original) then Exit(i);
end;

function TXMLTable.Field_Add(const aFMeta: TFieldMeta): IXMLNode;
var fNodeT: IXMLNode;
    fNodeD: IXMLNode;
    FMeta: TFieldMeta;
  procedure Properties(const aNode: IXMLNode; const aType: string; const aDBType: Variant; const aFixedlength: Variant;
                                           const aMaxLength: Variant; const aPrecision: Variant; const aScale: Variant);
  begin
                                       aNode.Attributes[NodeDatatype_type]:=        aType;
    if not VarIsEmpty(aDBType)    then aNode.Attributes[NodeDatatype_dbtype]:=      aDBType;
    if not VarIsEmpty(aMaxLength) then aNode.Attributes[NodeDatatype_maxLength]:=   aMaxLength;
    if not VarIsEmpty(aPrecision) then aNode.Attributes[NodeDatatype_precision]:=   aPrecision;
    if not VarIsEmpty(aScale)     then aNode.Attributes[NodeDatatype_scale]:=       aScale;
    if not VarIsEmpty(aFixedlength) then
      begin
        if (aFixedlength = True)  then aNode.Attributes[NodeDatatype_fixedlength]:= True;
        if (aFixedlength = False) then aNode.Attributes[NodeDatatype_long]:=        True;
      end;
  end;

begin
  FMeta:= FTMeta.Fields.FindByName(aFMeta.Original);
  if not assigned(FMeta) then
    begin
      FMeta:= TFieldMeta.Create;
      FMeta.Owner := FTMeta;
      FMeta.Assign(aFMeta);
      FTMeta.Fields.Add(aFMeta);
    end;

  Result:= FDoc.FElementNode.AddChild(NodeAttribute);
  Result.Attributes[NodeAttribute_name]:= aFMeta.Original;
  Result.Attributes[NodeAttribute_number]:= FDoc.FElementNode.ChildNodes.Count; // нумерация полей общая или у каждой таблицы своя

//  if not aFMeta.NotNull then
//    Result.Attributes[NodeAttribute_nullable]:= True;

//  Result.Attributes[NodeAttribute_writeunknown]:= True; /// не понятно зачем
//if assigned(Tablemeta) then
//  if assigned(Tablemeta.Database) then
//    FAttributeNode.Attributes[NodeAttribute_basecatalog]:= TableMeta.Database.Database;
  if assigned(Tablemeta) then
    Result.Attributes[NodeAttribute_basetable]:= TableMeta.Table;
  Result.Attributes[NodeAttribute_basecolumn]:= aFMeta.Original;
  if aFMeta.Key then
    Result.Attributes[NodeAttribute_keycolumn]:= True;

  if aFMeta.DataType = ftAutoinc then
    Result.Attributes[NodeAttribute_autoincrement]:= True;

  if assigned(aFMeta.LinkTable) then
    if aFMeta.LinkTable.Database.ID = aFMeta.Owner.Database.ID then
      Result.Attributes[NodeAttribute_keyforegn]:= aFMeta.LinkTable.Table;

  fNodeT:= Result.AddChild(NodeDatatype);
  with aFMeta do
    case DataType of   //Properties(fNodeT,      Type,       DBType, Fixedlength, MaxLength,  Precision,      Scale );
      ftBoolean:         Properties(fNodeT,  'boolean',  unassigned,       True,          2, unassigned, unassigned );
      ftString:          Properties(fNodeT,   'string',       'str', unassigned,   DataSize, unassigned, unassigned );
      ftFixedChar:       Properties(fNodeT,   'string',       'str',       True,   DataSize, unassigned, unassigned );
      ftMemo:            Properties(fNodeT,   'string',       'str',      False, 2147483647, unassigned, unassigned );
      ftWideString:      Properties(fNodeT,   'string',  unassigned, unassigned,   DataSize, unassigned, unassigned );  // MaxLength = DataSize div 2 - ???
      ftFixedWideChar:   Properties(fNodeT,   'string',  unassigned,       True,   DataSize, unassigned, unassigned );  // MaxLength = DataSize div 2 - ???
      ftWideMemo:        Properties(fNodeT,   'string',  unassigned,      False, 1073741823, unassigned, unassigned );
      ftTime:            Properties(fNodeT,     'time',  unassigned,       True, unassigned, unassigned, unassigned );  // aMaxLength - ???
      ftDate:            Properties(fNodeT,     'date',  unassigned,       True,          6,         10, unassigned );
      ftDateTime:        Properties(fNodeT, 'datetime',  unassigned,       True,         16,         23,          3 );
      ftTimeStamp:       Properties(fNodeT, 'datetime', 'timestamp',       True,         16,         23,          3 );
      ftTimeStampOffset: Properties(fNodeT, 'datetime', 'timestamp',       True,         18,         34,          7 );  // aMaxLength - ???
      ftByte:            Properties(fNodeT,      'ui1',  unassigned,       True,          1,          3, unassigned );
      ftWord:            Properties(fNodeT,      'ui2',  unassigned,       True,          2,          5, unassigned );
      ftLongWord:        Properties(fNodeT,      'ui4',  unassigned,       True,          4,         10, unassigned );
      ftShortInt:        Properties(fNodeT,       'i1',  unassigned,       True,          1,          3, unassigned );
      ftSmallInt:        Properties(fNodeT,       'i2',  unassigned,       True,          2,          5, unassigned );
      ftInteger:         Properties(fNodeT,      'int',  unassigned,       True,          4,         10, unassigned );
      ftLargeint:        Properties(fNodeT,       'i8',  unassigned,       True,          8,         19, unassigned );
      ftAutoInc: case aFMeta.DataSize of
                      1: Properties(fNodeT,       'i1',  unassigned,       True,          1,          3, unassigned );
                      2: Properties(fNodeT,       'i2',  unassigned,       True,          2,          5, unassigned );
                      8: Properties(fNodeT,       'i8',  unassigned,       True,          8,         19, unassigned );
                   else  Properties(fNodeT,      'int',  unassigned,       True,          4,         10, unassigned );
                 end;
      ftFloat:           Properties(fNodeT,    'float',  unassigned,       True,          8,         15, unassigned );
      ftCurrency:        Properties(fNodeT,   'number',  'currency',       True,          8,         19, unassigned );  // aScale - ???
      ftBCD:             Properties(fNodeT,   'number',  unassigned,       True,          8,         19, unassigned );  // aScale - ???
      ftGUID:            Properties(fNodeT,     'uuid',  unassigned,       True,         16, unassigned, unassigned );
      ftVarBytes:        Properties(fNodeT,  'bin.hex',  unassigned, unassigned,   DataSize, unassigned, unassigned );
      ftBytes:           Properties(fNodeT,  'bin.hex',  unassigned,       True,   DataSize, unassigned, unassigned );
      ftBlob:            Properties(fNodeT,  'bin.hex',  unassigned,      False, 2147483647, unassigned, unassigned );
      ftUnknown:         Properties(fNodeT,  'UNKNOWN',  unassigned, unassigned, unassigned, unassigned, unassigned );
       else              Properties(fNodeT,  'UNKNOWN',  unassigned, unassigned, unassigned, unassigned, unassigned );
    end;

  if aFMeta.NotNull then
    fNodeT.Attributes[NodeDatatype_maybenull]:= False;

  if 0 < Length(aFMeta.DefaultDefDB) then
    begin
      fNodeD:= Result.AddChild(NodeDefaultValue);
      fNodeD.Text:= aFMeta.DefaultDefDB;
    end;
end;

initialization
  Startup;



finalization
  Shutdown;

end.

