unit UFileDB;

{$DEFINE MSG_ON}
{$DEFINE USER_HAVE_ONCE_PASSWORD}
{$DEFINE Local}

interface
uses
  Classes, FDBConsts, ScktComp;

const
  WroneSym=[0..19,21..32,92,47,58,42,63,34,60,62,124];

type

  TFileClass=class(TComponent)
  protected
    FID:integer;
    FName:string;
    FBody:string;
    FFileType:TDEFileType;
    FSize:integer;
    FDateTime:TDateTime;
    FFileAttr:integer;
    FFullPath:string;
    FiParentFolder:integer;
    FiParentSubFolder:integer;
    FRealFullPath:string;
    FRealFileName:string;
  published
    property ID:integer read FID write FID;
    property Name:string read FName write FName;
    property Body:string read FBody write FBody;
    property FileType:TDEFileType read FFileType write FFileType;
    property Size:integer read FSize write FSize;
    property DateTime:TDateTime read FDateTime write FDateTime;
    property FileAttr:integer read FFileAttr write FFileAttr;
    property FullPath:string read FFullPath write FFullPath;
    property iParentFolder:integer read FiParentFolder write FiParentFolder;
    property iParentSubFolder:integer read FiParentSubFolder write FiParentSubFolder;
    property RealFullPath:string read FRealFullPath write FRealFullPath;
    property RealFileName:string read FRealFileName write FRealFileName;
  end;

  TFileUserList=class(TObject)
  protected
    FOwner:TObject;
    FAccounts:TList;
    FUsersActive:TList;
    FOnWork:TOnFDBAnswerLocal;
    FOnClientListChange:TOnClientListChange;
    function Get(Index: Integer): TUserRec;
    function GetCount:integer;
    function GetA(Index: Integer): TUserRec;
    function GetCountA:integer;
    function CheckNewUserA(Item:TUserRec):integer;
    procedure PutA(Index: Integer; Item: TUserRec);
    procedure Put(Index: Integer; Item: TUserRec);
    procedure TryLogin(LocalConnect:boolean; IP:string; Port, SocketHandle:integer; Login,Password,dbName,dbPath:string; ConnectionID:integer);
    procedure DisconnectClient(ClientID, ConnectionID:integer; LocalConnect:boolean);
  private
    function GetClientRules(ClientID:integer):TDERules;
    procedure SendReport(Sender:TObject; FDB_Answer:TFDB_Answer; ClientID:integer);
    procedure SendReportSHandle(Sender:TObject; FDB_Answer:TFDB_Answer; LocalConnect:boolean; ClientID:integer; SHandle:integer);
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index: Integer]: TUserRec read Get write Put; default;
    property ItemsA[Index: Integer]: TUserRec read GetA write PutA;
    property Count: Integer read GetCount;
    property CountA: Integer read GetCountA;
    function StateToStr(des:TDEState):string;
    function RulesToStr(der:TDERules):string;
    function IndexIsCurrect(Index:integer):boolean;
    function GetUniqueID:integer;
    function Add(Item: TUserRec): Integer;
    function AddA(Item: TUserRec): Integer;
    function GetUniqueIDA:integer;
    function IndexIsCurrectA(Index:integer):boolean;
    function GetClientNum(ClientID:integer):integer;
    procedure LoadUserListFromFile(DirPath:string);
    procedure SaveUserListToFile(DirPath:string; NewFile:boolean; DBAlias:string);
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; Item: TUserRec);
    procedure ClearUserRec(var UserRec:TUserRec);
    procedure Replace(Index:integer; Item: TUserRec);
    procedure ClearA;
    procedure DeleteA(Index: Integer);
    procedure InsertA(Index: Integer; Item: TUserRec);
    procedure ReplaceA(Index:integer; Item: TUserRec);
    property OnWork:TOnFDBAnswerLocal read FOnWork write FOnWork;
    property OnClientListChange:TOnClientListChange read FOnClientListChange write FOnClientListChange;
  end;

  TFileDataSet=class(TObject)
  private
    FCount:integer;
    FList:TList;
    FRecNo:integer;
    FEOF:boolean;
    FBOF:boolean;
    FTableList:string;
    function Get(const Index: Integer): Variant;
    function GetCountTables:integer;
    function GetCount:integer;
  public
    constructor Create;
    destructor Destroy; override;

    property Fields[const Index: Integer]: Variant read Get; default;
    property Count:integer read GetCount;
    property RecNo:integer read FRecNo write FRecNo;
    property CountTables:integer read GetCountTables;

    function FieldByName(const FieldName: string): TDEFileField;
    function GetTableName(aNumber:integer):string;
    function GetCurrentRec:TFile;

    procedure AddRec(FileRec:TFile);
    procedure Clear;
  end;

  TFileList=class(TObject)
  private
    FList:TList;
    procedure SetTruePathAndNameAndType(var fl:TFile);
    procedure SetRealPathAndNameAndType(var fl:TFile);
  protected
    function Get(const Index: Integer): TFile;
    procedure Put(const Index: Integer; Item: TFile);
    function GetCount:integer;
    function CreateFile(var fl:TFile; Var FDB_Answer:TFDB_Answer):integer;

    function DeleteFile(fl:TFile; Var FDB_Answer:TFDB_Answer):integer;
    function RenameFile(Old_fl,New_fl :TFile; Var FDB_Answer:TFDB_Answer):integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Items[const Index: Integer]: TFile read Get write Put; default;
    property Count: Integer read GetCount;
    function Add(var Item: TFile; Var FDB_Answer:TFDB_Answer; bMakeFile:boolean): Integer;
    function Rename(Index: integer; fl_new:TFile; Var FDB_Answer:TFDB_Answer):integer;
    procedure Clear;
    procedure Delete(const Index: Integer; var FDB_Answer: TFDB_Answer);
    procedure Insert(const Index: Integer; var Item: TFile; var FDB_Answer:TFDB_Answer; const bMakeFile: Boolean);
    function GetUniqueID:integer;
    function GetFileType(const FilePath: string): TDEFileType;
    procedure WriteToFileUpdate(fl:TFile);
  end;

  PSubFolder=^TSubFolder;
  TSubFolder=record
    Name:string;
    FullPath:string;
    iParentFolder:integer;
    Files:TFileList;
  end;

  TSubFolderList=class(TObject)
  protected
    FList:TList;
    function Get(const Index: Integer): TSubFolder;
    procedure Put(const Index: Integer; Item: TSubFolder);
    function GetCount:integer;
    function GetSubFolderNum(const SubFolder: string): Integer;
  public
    property Items[const Index: Integer]: TSubFolder read Get write Put; default;
    constructor Create;
    destructor Destroy; override;
    function Add(Item: TSubFolder): Integer;
    procedure Clear;
    procedure Delete(const Index: Integer);
    property Count: Integer read GetCount;
    procedure Insert(const Index: Integer; Item: TSubFolder);
  end;

  PFOlder=^TFolder;
  TFolder=record
    Name:string;
    FullPath:string;
    SubFolders:TSubFolderList;
  end;

  PConnection=^TConnection;
  TConnection=record
    Connected,
    Connecting,
    LocalConnect:boolean;
    ConnectionID:integer;
    Socket:TClientSocket;
    DBAlias:string;
    IP:string;
    Port:integer;
    DBPath:string;
    ClientFDB_ID:integer;
    UserName:string;
    Password:string;
  end;


  TFileDB=class(TComponent)
  private
    FOnDataReady:TOnFDBAnswerLocal;
    FOnConnect:TOnFDBAnswerLocal;
    FOnNotAccept:TDENotAccept;
    FOnError:TDEError;
    FOnDisconnect:TDEDisconnect;
    FOnUpdateDB:TNotifyEvent;
    FolderList:TList;
    FFileDataSet:TFileDataSet;
    ConnectList:TList;
    FUserList:TFileUserList;
    FTableSet:boolean;
    FCurrentConnection:TConnection;
    FLastFileID_:integer;
    function Get(Index: Integer): TFolder;
    procedure Put(Index: Integer; Item: TFolder);
    function GetCount:integer;
    procedure FindSubFolders(iParentFolder:integer);
    procedure FindFiles(iParentFolder,iParentSubFolder:integer);
    function GetFileBody(FilePath:string):string;
    procedure ClearConnectionRec_and_CreateSocket(var Connection:TConnection; CreateSocket:boolean);
    function GetUniqueConnectID:integer;

    {Function Like SQL}
    procedure SetCurTable(ClientID:integer; FolderName:string);
    procedure SetSelect(ClientID:integer; FileNum,FileCount:integer; NoBody:boolean);
    procedure SetTableList(ClientID:integer);
    procedure SetInsert(ClientID:integer; fl:TFile);
    procedure SetUpdate(ClientID:integer; fl_new:TFile);
    procedure SetDelete(ClientID:integer; ID:integer);
    procedure DoCommand(CommandToFDB:TCommandToFDB; LocalConnect:boolean; SocketHandle:integer; FromIP:string);

    {-----------------}

    procedure AnswerOnWork(FDB_Answer:TFDB_Answer; LocalConnect:boolean);
    procedure FDBMessage(Sender:TObject; FDB_Answer:TFDB_Answer);
    function ClientCanDoThisCommand(FDBCommand: TFDBCommand; ClientRules:TDERules):boolean;
    procedure LoadDBList;
    function WroneDirectoryForFDB(DirPath, DBAlias:string):boolean;
    function DirectoryIsEmpty(DirPath:string):boolean;
    function TableExists(DBAlias,TableName:string):boolean;
    procedure CreateDefaultLocalUser;
  protected
    function StringToFileRec(FileStr:string):TFile;
    function FileRecToString(FileRec:TFile):string;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy; override;

    property FileDataSet:TFileDataSet read FFileDataSet;
    property Items[Index: Integer]: TFolder read Get write Put; default;
    property Count: Integer read GetCount;
    property UserList:TFileUserList read FUserList write FUserList;
    property CurrentConnection:TConnection read FCurrentConnection write FCurrentConnection;

    function Exists_DBName_or_DBPath(dbSome: string; LocalConnect:boolean; var DBName:string; var DBPath:string): boolean;
    function DBNameExists(DBName:string; var DBPath:string):boolean;
    function DBFolderPathExists(FolderPath:string; var DBAlias:string):boolean;
    function Add(Item: TFolder; CheckDirForFDB:boolean): Integer;
    function AddEx(DBName,FolderPath:string; CheckDirForFDB:boolean):boolean;
    function GetDBList:string;
    function GetDBPath(DBAlias:string):string;
    function ConnectToDB(LocalConnect:boolean; IP:string; Port:integer; Login,Password, DBName:string):boolean;
    function ConnectExists( IP:string; Port:integer; DBName, DBPath:string):boolean;
    function GetConnectionNum(ConnectionID:integer):integer;
    function GetCurrentConnection:integer;

    procedure GetTableList(TableList:TStrings);
    procedure SendCommandToFDB(CommandToFDB: TCommandToFDB; LocalConnect:boolean);
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; Item: TFolder);
    procedure DisconnectDB();
    procedure SaveUserListToFileEx;
    procedure Set_OK_Connect(FDB_Answer:TFDB_Answer);
    procedure Set_OK_Disconnect(FDB_Answer:TFDB_Answer);
    function InsertFile(fl:TFile):integer;
    procedure UpdateFile(fl:TFile);
    procedure DeleteFile(ID:integer);
    procedure SelectFiles(aRecordNumber, aRecordCount: integer; aNoBody: boolean);
    function SetCurrentTable(TableName:string):boolean;


  published
    property OnDataReady: TOnFDBAnswerLocal read FOnDataReady write FOnDataReady;
    property OnConnect: TOnFDBAnswerLocal read FOnConnect write FOnConnect;
    property OnNotAccept:TDENotAccept read FOnNotAccept write FOnNotAccept;
    property OnError:TDEError read FOnError write FOnError;
    property OnDisconnect:TDEDisconnect read FOnDisconnect write FOnDisconnect;
    property OnUpdateDB:TNotifyEvent read FOnUpdateDB write FOnUpdateDB;
  end;

function GetDEFileTypeAsString(DEFileType:TDEFileType):string;
procedure CopyFileRecToFileClass(var FileClass:TFileClass; var FileRec:TFile; Back:boolean);
procedure CopyFileToFile_Update(Source:TFile; var Dest:TFile);

implementation

uses Windows, SysUtils, Math, Dialogs,
     DeLog, DeTypes;

function GetDEFileTypeAsString(DEFileType:TDEFileType):string;
begin
  case DEFileType of
    deftText:Result:='Text';
    deftExe:Result:='Exe';
    deftBitmap:Result:='Bitmap';
    deftDocument:Result:='Document';
    deftJpeg:Result:='Jpeg';
    deftPas:Result:='Pascal';
    deftDll:Result:='DLL';
    deftUnknown:Result:='Unknown';
  end;//CASE
end;

procedure CopyFileToFile_Update(Source:TFile; var Dest:TFile);
begin
  Dest.Name:=Source.Name;
  Dest.Body:=Source.Body;
  Dest.Size:=Length(Dest.Body);
  Dest.FullPath:=ExtractFilePath(Dest.FullPath)+Dest.Name;
end;

{ TFileDB } //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TFileDB.ConnectToDB(LocalConnect:boolean; IP: string; Port: integer; Login,
  Password, DBName: string):boolean;
var
  Cnct:PConnection;
  FDBC:TCommandToFDB;
begin
  if ConnectExists(IP,Port,DBName,DBName) then begin
    Result:=False;
    Exit;
  end;
  Result:=True;

  New(Cnct);
  ClearConnectionRec_and_CreateSocket(Cnct^,True);


  Cnct^.Connected:=False;
  Cnct^.Connecting:=False;
  Cnct^.LocalConnect:=True;
  Cnct^.ConnectionID:=GetUniqueConnectID;
  Cnct^.DBAlias:=DBName;
  Cnct^.IP:=IP;
  Cnct^.Port:=Port;
  Cnct^.DBPath:=DBName;
  Cnct^.ClientFDB_ID:=-1;
  Cnct^.UserName:=Login;
  Cnct^.Password:=Password;
  ConnectList.Add(Cnct);

  FCurrentConnection:=Cnct^;

  FDBC.FDB_ID:=-1;
  FDBC.ConnectionID:=Cnct^.ConnectionID;
  FDBC.iCommand:=fdbc_TRY_CONNECT;
  FDBC.Params:=Login+cr+Password+cr+DBName;

  if LocalConnect then begin
    SendCommandToFDB(FDBC,LocalConnect);
  end else begin
    Cnct^.Socket.Address:=IP;
    Cnct^.Socket.Port:=Port;
    Cnct^.Socket.Open;
  end;
end;

procedure TFileDB.ClearConnectionRec_and_CreateSocket(var Connection: TConnection; CreateSocket:boolean);
begin
  Connection.Connected:=False;
  Connection.Connecting:=False;
  Connection.LocalConnect:=True;
  Connection.ConnectionID:=-1;
  if CreateSocket then Connection.Socket:=TClientSocket.Create(Self);
  Connection.DBAlias:=EmptyStr;
  Connection.IP:=EmptyStr;
  Connection.Port:=0;
  Connection.DBPath:=EmptyStr;
  Connection.ClientFDB_ID:=-1;
  Connection.UserName:=EmptyStr;
  Connection.Password:=EmptyStr;
end;


function TFileDB.ConnectExists(IP: string; Port: integer;
  DBName, DBPath: string): boolean;
var
  i:integer;
  c:PConnection;
begin
  Result:=False;
  if not Assigned(ConnectList) then Exit;
  for i:=0 to ConnectList.Count-1 do begin
    c:=ConnectList.Items[i];
    if ((AnsiUpperCase(c^.DBAlias)=AnsiUpperCase(DBName)) or (AnsiUpperCase(c^.DBPath)=AnsiUpperCase(DBPath)))
    and (AnsiUpperCase(c^.IP)=AnsiUpperCase(IP))
    and (c^.Port=Port) then begin
      Result:=True;
      Break;
    end;
  end;
end;


procedure TFileDB.DisconnectDB();
var
  cNum:integer;
  CommandToFDB:TCommandToFDB;
begin
  cNum:=GetCurrentConnection;



  if cNum<0 then Exit;
  if not PConnection(ConnectList.Items[cNum])^.Connected then Exit;

  CommandToFDB.FDB_ID:=PConnection(ConnectList.Items[cNum])^.ClientFDB_ID;
  CommandToFDB.ConnectionID:=PConnection(ConnectList.Items[cNum])^.ConnectionID;
  CommandToFDB.iCommand:=fdbc_DISCONNECT;
  CommandToFDB.Params:=EmptyStr;
  CommandToFDB.FDB_Data:=EmptyStr;

  if PConnection(ConnectList.Items[cNum])^.LocalConnect then begin
    SendCommandToFDB(CommandToFDB,True);
  end else begin
  end;
end;

function TFileDB.GetCurrentConnection: integer;
var
  i:integer;
  c:PConnection;
begin
  Result:=-1;
  for i:=0 to ConnectList.Count-1 do begin
    c:=ConnectList.Items[i];
    if ((AnsiUpperCase(c^.DBAlias)=AnsiUpperCase(CurrentConnection.DBAlias)) or (AnsiUpperCase(c^.DBPath)=AnsiUpperCase(CurrentConnection.DBPath)))
    and (AnsiUpperCase(c^.IP)=AnsiUpperCase(CurrentConnection.IP))
    and (c^.Port=CurrentConnection.Port) then begin
      Result:=i;
      Break;
    end;
  end;
end;

function TFileDB.GetConnectionNum(ConnectionID:integer): integer;
var
  i:integer;
begin
  Result:=-1;
  for i:=0 to ConnectList.Count-1 do
    if PConnection(ConnectList.Items[i])^.ConnectionID=ConnectionID then begin
      Result:=i;
      Break;
    end;
end;


function TFileDB.DBFolderPathExists(FolderPath: string; var DBAlias:string): boolean;
var
  i:integer;
begin
  Result:=False;
  if Length(FolderPath)>0 then
    if FolderPath[Length(FolderPath)]<>'\' then FolderPath:=FolderPath+'\';
  for i:=0 to Count-1 do
    if AnsiUpperCase(Items[i].FullPath)=AnsiUpperCase(FolderPath) then begin
      DBAlias:=Items[i].Name;
      Result:=True;
      Break;
    end;
end;

function TFileDB.DBNameExists(DBName: string; var DBPath:string): boolean;
var
  i:integer;
begin
  Result:=False;
  for i:=0 to Count-1 do
    if AnsiUpperCase(Items[i].Name)=AnsiUpperCase(DBName) then begin
      DBPath:=Items[i].FullPath;
      Result:=True;
      Break;
    end;
end;

function TFileDB.Exists_DBName_or_DBPath(dbSome: string; LocalConnect:boolean; var DBName:string; var DBPath:string): boolean;
var
  bDBAlias,bDBPath:boolean;
  s:string;
begin

  bDBAlias:=DBNameExists(dbSome,s);
  if bDBAlias then begin
    DBName:=dbSome;
    DBPath:=s;
  end;

  bDBPath:=DBFolderPathExists(dbSome,s);
  if bDBPath then begin
    DBName:=s;
    DBPath:=dbSome;
  end;

  if (not bDBAlias) and (not bDBPath) then Result:=False else Result:=True;

  if LocalConnect then begin
    DBName:='*';
    DBPath:=dbSome;
    Result:=True;
    Exit;
  end;
end;


procedure TFileDB.SendCommandToFDB(CommandToFDB: TCommandToFDB;
  LocalConnect: boolean);
begin
  if LocalConnect then begin
    DoCommand(CommandToFDB,True,0,'localhost');
  end else begin
  end;
end;


procedure TFileDB.DoCommand(CommandToFDB: TCommandToFDB; LocalConnect:boolean;
          SocketHandle:integer; FromIP:string);
  procedure SendErr_TableNotSelect(CommandToFDB: TCommandToFDB);
  var
    FDB_Answer:TFDB_Answer;
  begin
    FDB_Answer.MsgCode:=FDB_ERROR_TABLE_NOT_SELECT;
    FDB_Answer.MsgStr:=sFDB_ERROR_TABLE_NOT_SELECT;
    FDB_Answer.FDB_ID:=CommandToFDB.FDB_ID;
    FDB_Answer.ConnectionID:=CommandToFDB.ConnectionID;
    FDB_Answer.Rules:=derNone;
    FDB_Answer.CanOverload:=False;
    UserList.SendReport(Self,FDB_Answer,FDB_Answer.FDB_ID);
  end;

var
  sl:TStringList;
  sParams:array[0..15] of string;
  i,Num:integer;
  ClientRules:TDERules;
  CanWork,bTmp:boolean;
  CurUsr:TUserRec;
  FDB_Answer:TFDB_Answer;
  fl:TFile;
  DBN_,DBP_:string;
begin
  sl:=TStringList.Create;
  sl.Text:=CommandToFDB.Params;
  for i:=0 to High(sParams) do sParams[i]:=EmptyStr;
  for i:=0 to Min(sl.Count-1,High(sParams)) do sParams[i]:=sl.Strings[i];

  Num:=UserList.GetClientNum(CommandToFDB.FDB_ID);
  CanWork:=False;
  if Num>=0 then begin// ГЫ, а такого клиента вообще нет, странно...хакер что ли?..
    CurUsr:=UserList.Items[Num];
    ClientRules:=UserList.GetClientRules(CommandToFDB.FDB_ID);
    if ClientRules=derNone then begin //Клиент не подключен к этой БД
      sl.Free;
      Exit;
    end;
    CanWork:=ClientCanDoThisCommand(CommandToFDB.iCommand,ClientRules);
  end;
  if (Num<0) and (CommandToFDB.iCommand=fdbc_TRY_CONNECT) then CanWork:=True;

  if CanWork then begin
    case CommandToFDB.iCommand of
      fdbc_SELECT:begin
                    if sParams[0]=EmptyStr then sParams[0]:='0';
                    if sParams[1]=EmptyStr then sParams[1]:='1';
                    if (sParams[2]='1') then bTmp:=True else bTmp:=False;

                    if CurUsr.TableName<>EmptyStr then
                      SetSelect(CurUsr.ID,StrToInt(sParams[0]),StrToInt(sParams[1]),bTmp)
                        else SendErr_TableNotSelect(CommandToFDB);
                  end;
      fdbc_INSERT:begin
                    fl:=StringToFileRec(CommandToFDB.FDB_Data);
                    if CurUsr.TableName<>EmptyStr then
                      SetInsert(CurUsr.ID,fl)
                        else SendErr_TableNotSelect(CommandToFDB);
                  end;
      fdbc_UPDATE:begin
                    fl:=StringToFileRec(CommandToFDB.FDB_Data);
                    if CurUsr.TableName<>EmptyStr then
                      SetUpdate(CurUsr.ID,fl)
                        else SendErr_TableNotSelect(CommandToFDB);
                  end;
      fdbc_DELETE:begin
                    if sParams[0]=EmptyStr then sParams[0]:='-1';
                    if CurUsr.TableName<>EmptyStr then
                      SetDelete(CurUsr.ID,StrToInt(sParams[0]))
                        else SendErr_TableNotSelect(CommandToFDB);
                  end;
      fdbc_GET_LIST_DB:begin
                     end;
      fdbc_CREATE_DB:begin
                     end;
      fdbc_DELETE_DB:begin
                     end;
      fdbc_UPDATE_DB:begin
                     end;
      fdbc_GET_LIST_TBL:begin
                        end;
      fdbc_CREATE_TBL:begin
                      end;
      fdbc_DELETE_TBL:begin
                      end;
      fdbc_UPDATE_TBL:begin
                      end;
      fdbc_GET_LIST_USR:begin
                        end;
      fdbc_CREATE_USR:begin
                      end;
      fdbc_DELETE_USR:begin
                      end;
      fdbc_UPDATE_USR:begin
                      end;
      fdbc_TRY_CONNECT:begin
                         if Exists_DBName_or_DBPath(sParams[2],LocalConnect, DBN_, DBP_) then
                           UserList.TryLogin(LocalConnect,FromIP,0,SocketHandle,sParams[0],sParams[1],DBN_,DBP_,CommandToFDB.ConnectionID)
                             else begin
                               FDB_Answer.MsgCode:=FDB_CONNECT_WRONE_LOGIN_PASS_DB;
                               FDB_Answer.MsgStr:=sFDB_CONNECT_WRONE_LOGIN_PASS_DB;
                               FDB_Answer.FDB_ID:=-1;
                               FDB_Answer.Rules:=derNone;
                               FDB_Answer.ConnectionID:=CommandToFDB.ConnectionID;
                               UserList.SendReportSHandle(Self,FDB_Answer,LocalConnect,-1,SocketHandle);
                             end;
                       end;
      fdbc_DISCONNECT:begin
                        UserList.DisconnectClient(CommandToFDB.FDB_ID,CommandToFDB.ConnectionID,LocalConnect);
                      end;
      fdbc_SELECT_CURRENT_TABLE:begin
                                  SetCurTable(CommandToFDB.FDB_ID,sParams[0]);
                                end;
      fdbc_GET_FILES_COUNT:begin
                             if sParams[0]=EmptyStr then sParams[0]:='0';
                             if sParams[1]=EmptyStr then sParams[1]:='0';
                             if CurUsr.TableName<>EmptyStr then
                               SetSelect(CurUsr.ID,StrToInt(sParams[0]),StrToInt(sParams[1]),True)
                                 else SendErr_TableNotSelect(CommandToFDB);
                           end;
      fdbc_GET_TABLES_LIST:begin
                         SetTableList(CurUsr.ID);
                       end;

    end;//CASE
  end else begin // Client Can't work
  end;
  sl.Free;
end;


procedure TFileDB.Set_OK_Connect(FDB_Answer: TFDB_Answer);
var
  i:integer;
begin
  for i:=0 to ConnectList.Count-1 do
     if PCOnnection(ConnectList[i])^.ConnectionID=FDB_Answer.ConnectionID then begin
       PCOnnection(ConnectList[i])^.Connected:=True;
       PCOnnection(ConnectList[i])^.Connecting:=False;
       PCOnnection(ConnectList[i])^.ClientFDB_ID:=FDB_Answer.FDB_ID;

       CurrentConnection:=PCOnnection(ConnectList[i])^;
       Break;
     end;
end;

procedure TFileDB.Set_OK_Disconnect(FDB_Answer: TFDB_Answer);
var        
  i:integer;
begin
  i:=0;
  While i<ConnectList.Count do begin
     if PCOnnection(ConnectList[i])^.ConnectionID=FDB_Answer.ConnectionID then begin
       PCOnnection(ConnectList[i])^.Socket.Close;
       FreeAndNil(PCOnnection(ConnectList[i])^.Socket);
       ClearConnectionRec_And_Createsocket(PCOnnection(ConnectList[i])^,False);
       Dispose(PCOnnection(ConnectList[i]));
       ConnectList.Delete(i);
       Break;
     end;
    Inc(i);
  end;
  if ConnectList.Count>0 then
    CurrentConnection:=PCOnnection(ConnectList[ConnectList.Count-1])^;

end;


function TFileDB.Add(Item: TFOlder; CheckDirForFDB:boolean): Integer;
var
  fo:PFolder;
  s:string;
begin
  Result:=-1;
  if Assigned(FolderList) then begin
    if DBFolderPathExists(Item.FullPath,s) then Result:=-2;
    if DBNameExists(Item.Name,s) then Result:=-3;
    if not SysUtils.DirectoryExists(Item.FullPath) then Result:=-4;
    if CheckDirForFDB then if WroneDirectoryForFDB(Item.FullPath, Item.Name) then Result:=-5;
    if Result>=-1 then begin
      if Copy(Item.FullPath,Length(Item.FullPath),1)<>'\' then
        Item.FullPath:=Item.FullPath+'\';
      New(fo);
      fo^:=Item;
      fo^.SubFolders:=TSubFolderList.Create;
      Result:=FolderList.Add(fo);
      FindSubFolders(Result);
      UserList.LoadUserListFromFile(Item.FullPath);
    end;
  end;
end;


function TFileDB.GetDBList: string;
var
  sl:TStringList;
  i:integer;
begin
  sl:=TStringList.Create;
  for i:=0 to Count-1 do begin
    sl.Sorted:=True;
    sl.CaseSensitive:=False;
    sl.Duplicates:=dupIgnore;
    sl.Add(Items[i].Name);
  end;
  Result:=sl.Text;
  sl.Free;
end;

function TFileDB.GetDBPath(DBAlias: string): string;
var
  i:integer;
begin
  for i:=0 to Count-1 do
    if AnsiUpperCase(Items[i].Name)=AnsiUpperCase(DBAlias) then begin
       Result:=Items[i].FullPath;
       Break;
    end;
end;

procedure TFileDB.SaveUserListToFileEx;
var
  sl:TStringList;
  i:integer;
begin
  sl:=TStringList.Create;
  sl.Text:=GetDBList;
  for i:=0 to sl.Count-1 do
    if sl.Strings[i]<>EmptyStr then
      UserList.SaveUserListToFile(GetDBPath(sl.Strings[i]),False,sl.Strings[i]);
  sl.Free;
end;

function TFileDB.AddEx(DBName, FolderPath: string; CheckDirForFDB:boolean):boolean;
var
  fo:TFolder;
begin
  fo.Name:=DBName;
  fo.FullPath:=FolderPath;
  if Add(fo,CheckDirForFDB)<0 then Result:=False else Result:=True;
end;


function TFileDB.WroneDirectoryForFDB(DirPath, DBAlias: string): boolean;
begin
  Result:=True;
  if not SysUtils.DirectoryExists(DirPath) then Exit;
  if DirectoryIsEmpty(DirPath) then begin
    UserList.SaveUserListToFile(DirPath,True,DBAlias);
    Result:=False;
  end else begin
    if FileExists(DirPath+cFileUserPrevilegies) then Result:=False;
  end;
end;


procedure TFileDB.FDBMessage(Sender:TObject; FDB_Answer: TFDB_Answer);
begin
  AnswerOnWork(FDB_Answer, True);
end;

procedure TFileDB.AnswerOnWork(FDB_Answer: TFDB_Answer; LocalConnect:boolean);
begin
  case FDB_Answer.MsgCode of
    FDB_CONNECT_SUCCESSFULL:begin//connect to db is succesfull
                              Set_OK_Connect(FDB_Answer);
                              if Assigned(FOnConnect) then FOnConnect(Self,FDB_Answer);
                            end;
    FDB_CONNECT_WRONE_LOGIN_PASS_DB..FDB_CONNECT_WRONE_IP:begin
                                                       Set_OK_Disconnect(FDB_Answer);
                                                       if Assigned(FOnNotAccept) then FOnNotAccept(Self,FDB_Answer.MsgCode,FDB_Answer.MsgStr);
                                                     end;
    FDB_ERROR_FIRST..FDB_ERROR_LAST:begin
                                      if Assigned(FOnError) then FOnError(Self,FDB_Answer.MsgCode,FDB_Answer.MsgStr);
                                    end;
    FDB_DATA_FIRST..FDB_DATA_LAST:begin

                                    if FDB_Answer.MsgCode=FDB_DATA_FILE_CREATE_DONE then begin
                                      FLastFileID_:=StrToInt(FDB_Answer.Data);
                                    end;

                                    if FDB_answer.MsgCode=FDB_DATA_TABLE_SET_OK then FTableSet:=True;
                                    if FDB_Answer.MsgCode=FDB_DATA_START then FileDataSet.Clear;
                                    if FDB_Answer.MsgCode=FDB_DATA_RECORD_READY then
                                      FileDataSet.AddRec(StringToFileRec(FDB_Answer.Data));
                                    if FDB_Answer.MsgCode=FDB_DATA_TABLE_LIST_READY then
                                      FileDataSet.FTableList:=FDB_Answer.Data;
                                    if FDB_Answer.MsgCode=FDB_DATA_READY then
                                      if Assigned(FOnDataReady) then FOnDataReady(Self,FDB_Answer);
                                  end;
    FDB_DISCONNECT_FIRST..FDB_DISCONNECT_LAST:begin
                                                Set_OK_Disconnect(FDB_Answer);
                                             //   FConnected:=False;
                                                if Assigned(FOnDisconnect) then FOnDisconnect(Self,False,FDB_Answer);
                                              end;
  end;// Case
end;


procedure TFileDB.Clear;
var
  i:integer;
  fo:PFolder;
begin
  if Assigned(FolderList) then begin
    for i:=0 to FolderList.Count-1 do begin
      fo:=FolderList.Items[i];
      fo^.SubFolders.Free;
      Dispose(fo);
    end;
    FolderList.Clear;
  end;
end;

constructor TFileDB.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FolderList:=TList.Create;
  ConnectList:=TList.Create;
  FFileDataSet:=TFileDataSet.Create;
  FUserList:=TFileUserList.Create;
  if Assigned(UserList) then UserList.OnWork:=FDBMessage;
  UserList.FOwner:=Self;
  LoadDBList;
  CreateDefaultLocalUser;
  Randomize;
end;

procedure TFileDB.CreateDefaultLocalUser;
var
  ur:TUserRec;
begin
  UserList.ClearUserRec(ur);
  ur.Name:=cDefaultUserName;
  ur.Password:=cDefaultUserPassword;
  ur.Rules:=derAll;
  ur.IP:='localhost';
  ur.LocalClient:=True;
  ur.DBAlias:='*';
  ur.DBPath:='*';
  UserList.AddA(ur);
end;

destructor TFileDB.Destroy;
var
  i:integer;
  fo:PFolder;
  Cnct:PConnection;
begin
  if Assigned(FolderList) then begin
    for i:=0 to FolderList.Count-1 do begin
      fo:=FolderList.Items[i];
      fo^.SubFolders.Free;
      Dispose(fo);
    end;
    FolderList.Clear;
    FolderList.Free;
    FolderList:=nil;
  end;

  for i:=0 to ConnectList.Count-1 do begin
    Cnct:=ConnectList.Items[i];
    Dispose(Cnct);
  end;
  ConnectList.Clear;
  ConnectList.Free;
  ConnectList:=nil;

  FFileDataSet.Free;
  FFileDataSet:=nil;
  FUserList.Free;
  FUserList:=nil;

  inherited;
end;


procedure TFileDB.LoadDBList;
  {$IFNDEF Local}
var
  sl:TStringList;
  s,Alias,Path:string;
  i:integer;
  {$ENDIF}
begin
  {$IFNDEF Local}
  sl:=TStringList.Create;
  s:=ExtractFilePath(ParamStr(0))+cFileDBList;
  if FileExists(s) then begin
    sl.LoadFromFile(s);
    for i:=0 to sl.Count-1 do begin
      s:=sl.Strings[i];
      if Pos(cPrfxAliasInFile,s)>0 then begin
        Alias:=Copy(s,1,Pos(cPrfxAliasInFile,s)-1);
        Path:=Copy(s,Pos(cPrfxAliasInFile,s)+Length(cPrfxAliasInFile),Length(s));
        if (Length(Alias)>0) and (Length(Path)>0) then begin
          if Path[Length(Path)]<>'\' then Path:=Path+'\';
          AddEx(Alias,Path,True);
        end;
      end;
    end;
  end;
  sl.Free;
  {$ENDIF}
  {$IFDEF Local}

  {$ENDIF}
end;

function TFileDB.DirectoryIsEmpty(DirPath: string): boolean;
var
  sr: TSearchRec;
  NoDirs,NoFiles:boolean;
begin
  NoDirs:=True;
  NoFiles:=True;
  if SysUtils.FindFirst(DirPath+'*', faAnyFile, sr) = 0 then begin
    repeat
      if ((sr.Attr and faDirectory) = faDirectory) and (sr.Name<>'.') and (sr.Name<>'..') then begin
        NoFiles:=False;
      end;
    until SysUtils.FindNext(sr) <> 0;
    SysUtils.FindClose(sr);
  end;
  if SysUtils.FindFirst(DirPath+'*', faAnyFile, sr) = 0 then begin
    repeat
      if ((sr.Attr and faDirectory) <> faDirectory) then begin
        NoDirs:=False;
      end;
    until SysUtils.FindNext(sr) <> 0;
    SysUtils.FindClose(sr);
  end;
  Result:=((NoFiles) and (NoDirs));
end;

function TFileDB.TableExists(DBAlias, TableName: string): boolean;
var
  i,j:integer;
begin
  Result:=False;
  for i:=0 to Count-1 do
    if AnsiUpperCase(Items[i].Name)=AnsiUpperCase(DBalias) then
      for j:=0 to Items[i].SubFolders.Count-1 do
        if AnsiUpperCase(Items[i].SubFolders[j].Name)=AnsiUpperCase(TableName) then begin
          Result:=True;
          Break;
        end;
end;


procedure TFileDB.Delete(Index: Integer);
var
  fo:PFolder;
begin
  if Assigned(FolderList) then begin
    fo:=FolderList.Items[Index];
    fo^.SubFolders.Free;
    Dispose(fo);
    FolderList.Delete(Index);
  end;
end;


function TFileDB.ClientCanDoThisCommand(FDBCommand: TFDBCommand; ClientRules: TDERules): boolean;
begin
//  TDERules=(derIgnore, derRW, derR, derW, derAll, derNone);
{
  TFDBCommand=(fdbc_SELECT, fdbc_INSERT, fdbc_UPDATE, fdbc_DELETE,
               fdbc_GET_LIST_DB, fdbc_CREATE_DB, fdbc_DELETE_DB, fdbc_UPDATE_DB,
               fdbc_GET_LIST_TBL, fdbc_CREATE_TBL, fdbc_DELETE_TBL, fdbc_UPDATE_TBL,
               fdbc_GET_LIST_USR, fdbc_CREATE_USR, fdbc_DELETE_USR, fdbc_UPDATE_USR,
               fdbc_TRY_CONNECT, fdbc_DISCONNECT, fdbc_SELECT_CURRENT_TABLE,
               fdbc_GET_FILES_COUNT, fdbc_TABLES_LIST);
               }
  Result:=False;
  if FDBCommand=fdbc_SELECT then begin
    if ClientRules in [derRW,derR,derAll] then Result:=True;

  end else if FDBCommand=fdbc_INSERT then begin
    if ClientRules in [derRW,derW,derAll] then Result:=True;

  end else if FDBCommand=fdbc_UPDATE then begin
    if ClientRules in [derRW,derW,derAll] then Result:=True;

  end else if FDBCommand=fdbc_DELETE then begin
    if ClientRules in [derRW,derW,derAll] then Result:=True;

  end else if FDBCommand=fdbc_GET_LIST_DB then begin
    if ClientRules in [derRW,derR,derAll] then Result:=True;

  end else if FDBCommand=fdbc_CREATE_DB then begin
    if ClientRules in [derRW,derW,derAll] then Result:=True;

  end else if FDBCommand=fdbc_DELETE_DB then begin
    if ClientRules in [derRW,derW,derAll] then Result:=True;

  end else if FDBCommand=fdbc_UPDATE_DB then begin
    if ClientRules in [derRW,derW,derAll] then Result:=True;

  end else if FDBCommand=fdbc_GET_LIST_TBL then begin
    if ClientRules in [derRW,derR,derAll] then Result:=True;

  end else if FDBCommand=fdbc_CREATE_TBL then begin
    if ClientRules in [derRW,derW,derAll] then Result:=True;

  end else if FDBCommand=fdbc_DELETE_TBL then begin
    if ClientRules in [derRW,derW,derAll] then Result:=True;

  end else if FDBCommand=fdbc_UPDATE_TBL then begin
    if ClientRules in [derRW,derW,derAll] then Result:=True;

  end else if FDBCommand=fdbc_GET_LIST_USR then begin
    if ClientRules in [derAll] then Result:=True;

  end else if FDBCommand=fdbc_CREATE_USR then begin
    if ClientRules in [derAll] then Result:=True;

  end else if FDBCommand=fdbc_DELETE_USR then begin
    if ClientRules in [derAll] then Result:=True;

  end else if FDBCommand=fdbc_UPDATE_USR then begin
    if ClientRules in [derAll] then Result:=True;

  end else if FDBCommand=fdbc_TRY_CONNECT then begin
    Result:=True;

  end else if FDBCommand=fdbc_DISCONNECT then begin
    Result:=True;

  end else if FDBCommand=fdbc_SELECT_CURRENT_TABLE then begin
    Result:=True;

  end else if FDBCommand=fdbc_GET_FILES_COUNT then begin
    if ClientRules in [derRW,derR,derAll] then Result:=True;

  end else if FDBCommand=fdbc_GET_TABLES_LIST then begin
    Result:=True;

  end;
end;


procedure TFileDB.SetCurTable(ClientID: integer; FolderName: string);
var
  Num:integer;
  ur:TUserRec;
  FDB_Answer:TFDB_Answer;
begin
  Num:=UserList.GetClientNum(ClientID);
  if Num>-1 then begin
    ur:=UserList.Items[Num];
    if TableExists(ur.DBAlias,FolderName) then begin
      ur.TableName:=FolderName;
      UserList.Items[Num]:=ur;

      FDB_Answer.MsgStr:=sFDB_DATA_TABLE_SET_OK;
      FDB_Answer.MsgCode:=FDB_DATA_TABLE_SET_OK;
      FDB_Answer.CanOverload:=True;
      FDB_Answer.FDB_ID:=ClientID;
      FDB_Answer.ConnectionID:=ur.ConnectionID;
      FDB_Answer.Rules:=derNone;
      UserList.SendReport(Self,FDB_Answer,ClientID);
    end else begin
      ur.TableName:=EmptyStr;
      UserList.Items[Num]:=ur;

      FDB_Answer.MsgStr:=sFDB_ERROR_TABLE_NOT_EXISTS;
      FDB_Answer.MsgCode:=FDB_ERROR_TABLE_NOT_EXISTS;
      FDB_Answer.CanOverload:=True;
      FDB_Answer.FDB_ID:=ClientID;
      FDB_Answer.ConnectionID:=ur.ConnectionID;
      FDB_Answer.Rules:=derNone;
      UserList.SendReport(Self,FDB_Answer,ClientID);
    end;
  end;
  if Assigned(UserList.FOnClientListChange) then UserList.FOnClientListChange(Self,UserList.Items[Num],False);
end;

procedure TFileDB.SetTableList(ClientID: integer);
var
  i,j:integer;
  FDB_Answer:TFDB_Answer;
  Num:integer;
  UserRec:TUserRec;
  s:string;
begin
  Num:=UserList.GetClientNum(ClientID);
  if Num<0 then Exit;
  UserRec:=UserList[Num];

  FDB_Answer.MsgStr:=sFDB_DATA_START;
  FDB_Answer.MsgCode:=FDB_DATA_START;
  FDB_Answer.CanOverload:=False;
  FDB_Answer.FDB_ID:=ClientID;
  FDB_Answer.Rules:=derNone;
  UserList.SendReport(Self,FDB_Answer,ClientID);

  s:=EmptyStr;
  for i:=0 to Count-1 do
    if AnsiUpperCase(Items[i].Name)=AnsiUpperCase(UserRec.DBAlias) then
      for j:=0 to Items[i].SubFolders.Count-1 do begin
        s:=s+Items[i].SubFolders.Items[j].Name+cr;
      end;

  if s=EmptyStr then begin
    FDB_Answer.MsgStr:=sFDB_DATA_TABLE_LIST_EMPTY;
    FDB_Answer.MsgCode:=FDB_DATA_TABLE_LIST_EMPTY;
  end else begin
    FDB_Answer.MsgStr:=sFDB_DATA_TABLE_LIST_READY;
    FDB_Answer.MsgCode:=FDB_DATA_TABLE_LIST_READY;
  end;
  FDB_Answer.CanOverload:=False;
  FDB_Answer.ConnectionID:=UserRec.ConnectionID;
  FDB_Answer.FDB_ID:=ClientID;
  FDB_Answer.Rules:=derNone;
  FDB_Answer.Data:=s;
  UserList.SendReport(Self,FDB_Answer,ClientID);

end;


procedure TFileDB.SetSelect(ClientID:integer; FileNum,FileCount:integer; NoBody:boolean);
var
  i,j,LeftSide,RightSide,k:integer;
  fl:TFile;
  FDB_Answer:TFDB_Answer;
  Num:integer;
  UserRec:TUserRec;
  Fnd:boolean;
  tmps : string;
begin
  Num:=UserList.GetClientNum(ClientID);
  if Num<0 then Exit;

  UserRec:=UserList[Num];

  LeftSide:=FileNum;
  RightSide:=FileNum+(FileCount-1);
  FDB_Answer.MsgStr:=sFDB_DATA_START;
  FDB_Answer.MsgCode:=FDB_DATA_START;
  FDB_Answer.CanOverload:=False;
  FDB_Answer.FDB_ID:=ClientID;
  FDB_Answer.Rules:=derNone;
  UserList.SendReport(Self,FDB_Answer,ClientID);

  Fnd:=False;
  for i:=0 to Count-1 do
    if AnsiUpperCase(Items[i].Name)=AnsiUpperCase(UserRec.DBAlias) then
      for j:=0 to Items[i].SubFolders.Count-1 do
        if AnsiUpperCase(Items[i].SubFolders[j].Name)=AnsiUpperCase(UserRec.TableName) then
          for k:=0 to Items[i].SubFolders[j].Files.Count-1 do
            if ((k>=LeftSide) and (k<=RightSide)) or (FileCount=0) or (FileCount=-1) then begin
              fl:=Items[i].SubFolders[j].Files[k];
              if not NoBody then
              begin
               fl.Body:=GetFileBody(fl.RealFullPath);
               //if (fl.FileType=deftBitmap)or(fl.FileType=deftJpeg) then
               begin
                 SetLength(tmps,2);
                 tmps[1] := char((SmallInt(Length(fl.RealFileName)) and $FF));
                 tmps[2] := char((SmallInt(Length(fl.RealFileName)) and $FF00) shl 8);
                 fl.Body := DeSignature + tmps + fl.RealFileName + fl.Body;
               end;
              end;
              Fnd:=True;
              FDB_Answer.MsgStr:=sFDB_DATA_RECORD_READY;
              FDB_Answer.MsgCode:=FDB_DATA_RECORD_READY;
              FDB_Answer.CanOverload:=False;
              FDB_Answer.ConnectionID:=UserRec.ConnectionID;
              FDB_Answer.FDB_ID:=ClientID;
              FDB_Answer.Rules:=derNone;
              FDB_Answer.Data:=FileRecToString(fl);
              UserList.SendReport(Self,FDB_Answer,ClientID);
            end;

  if not Fnd then begin
    FDB_Answer.MsgStr:=sFDB_DATA_EMPTY;
    FDB_Answer.MsgCode:=FDB_DATA_EMPTY;
  end else begin
    FDB_Answer.MsgStr:=sFDB_DATA_READY;
    FDB_Answer.MsgCode:=FDB_DATA_READY;
  end;
  FDB_Answer.CanOverload:=False;
  FDB_Answer.ConnectionID:=UserRec.ConnectionID;
  FDB_Answer.FDB_ID:=ClientID;
  FDB_Answer.Rules:=derNone;
  FDB_Answer.Data:=EmptyStr;
  UserList.SendReport(Self,FDB_Answer,ClientID);
end;

procedure TFileDB.SetInsert(ClientID: integer; fl:TFile);
var
  i,j, ClientNum,Num:integer;
  FDB_Answer:TFDB_Answer;
  Usr:TUserRec;
begin
  Num:=MaxInt;
  ClientNum:=UserList.GetClientNum(ClientID);
  if ClientNum<0 then Exit;
  Usr:=UserList.Items[ClientNum];
  for i:=0 to Count-1 do
    if AnsiUpperCase(Items[i].Name)=AnsiUpperCase(Usr.DBAlias) then begin
      j:=Items[i].SubFolders.GetSubFolderNum(Usr.TableName);
      if j>=0 then begin
        fl.iParentSubFolder:=j;
        fl.iParentFolder:=i;
        fl.FullPath:=Items[i].SubFolders[j].FullPath+fl.Name;
        Items[i].SubFolders[j].Files.SetRealPathAndNameAndType(fl);

        fl.Size:=Length(fl.Body);

        fl.FileAttr:=faAnyFile;
        fl.DateTime:=Now;
        if (Num<0) or (Num>Items[i].SubFolders[j].Files.Count-1) then Items[i].SubFolders[j].Files.Add(fl,FDB_Answer,True) else
          Items[i].SubFolders[j].Files.Insert(Num,fl,FDB_Answer,True);

        FDB_Answer.FDB_ID:=ClientID;
        FDB_Answer.Rules:=derNone;
        FDB_Answer.ConnectionID:=Usr.ConnectionID;
        UserList.SendReport(Self,FDB_Answer,FDB_Answer.FDB_ID);
      end else begin
        FDB_Answer.MsgStr:=sFDB_ERROR_FOLDER_NOT_EXISTS;
        FDB_Answer.MsgCode:=FDB_ERROR_FOLDER_NOT_EXISTS;
        FDB_Answer.CanOverload:=False; //TODO -oDF: Вот с этим перекрытием посидеть, паебаться
        FDB_Answer.FDB_ID:=ClientID;
        FDB_Answer.FDB_ID:=Usr.ConnectionID;
        FDB_Answer.Rules:=derNone;
        UserList.SendReport(Self,FDB_Answer,ClientID);
      end;
    end;
end;

procedure TFileDB.SetUpdate(ClientID: integer; fl_new:TFile);
var
  i,j,k,ClientNum:integer;
  FDB_Answer:TFDB_Answer;
  Usr:TUserRec;
  fl:TFile;
  OldFName:string;
begin
  ClientNum:=UserList.GetClientNum(ClientID);
  if ClientNum<0 then Exit;
  Usr:=UserList.Items[ClientNum];
  for i:=0 to Count-1 do
    if AnsiUpperCase(Items[i].Name)=AnsiUpperCase(Usr.DBAlias) then begin
      j:=Items[i].SubFolders.GetSubFolderNum(Usr.TableName);
      if j>=0 then begin
        for k:=0 to Items[i].SubFolders[j].Files.Count-1 do
          if Items[i].SubFolders[j].Files[k].ID=fl_new.ID then begin
            fl:=Items[i].SubFolders[j].Files[k];
            CopyFileToFile_Update(fl_new,fl);
            fl.FileType:=Items[i].SubFolders[j].Files.GetFileType(Items[i].SubFolders[j].Files[k].FullPath);
            Items[i].SubFolders[j].Files.WriteToFileUpdate(fl);
            OldFName:=fl.RealFullPath;
//            if fl.RealFileName <> fl_new.RealFileName then
//нужно проверять изменение полного пути файла (только какого?)
//перед тем, как его переименовать
              Items[i].SubFolders[j].Files.SetRealPathAndNameAndType(fl);
            SysUtils.RenameFile(OldFName,fl.RealFullPath);
            Items[i].SubFolders[j].Files[k]:=fl;
            FDB_Answer.MsgStr:=sFDB_DATA_FILE_UPDATE_DONE;
            FDB_Answer.MsgCode:=FDB_DATA_FILE_UPDATE_DONE;
            Break;
          end;
        FDB_Answer.FDB_ID:=ClientID;
        FDB_Answer.Rules:=derNone;
        FDB_Answer.ConnectionID:=Usr.ConnectionID;
        UserList.SendReport(Self,FDB_Answer,FDB_Answer.FDB_ID);
        Break;
      end else begin
        FDB_Answer.MsgStr:=sFDB_ERROR_FOLDER_NOT_EXISTS;
        FDB_Answer.MsgCode:=FDB_ERROR_FOLDER_NOT_EXISTS;
        FDB_Answer.CanOverload:=False; //TODO -oDF: Вот с этим перекрытием посидеть, паебаться
        FDB_Answer.FDB_ID:=ClientID;
        FDB_Answer.FDB_ID:=Usr.ConnectionID;
        FDB_Answer.Rules:=derNone;
        UserList.SendReport(Self,FDB_Answer,ClientID);
      end;
    end;
end;

procedure TFileDB.SetDelete(ClientID: integer; ID:integer);
var
  i,j,k,ClientNum:integer;
  FDB_Answer:TFDB_Answer;
  Usr:TUserRec;
begin
  ClientNum:=UserList.GetClientNum(ClientID);
  if ClientNum<0 then Exit;
  Usr:=UserList.Items[ClientNum];

  for i:=0 to Count-1 do
    if AnsiUpperCase(Items[i].Name)=AnsiUpperCase(Usr.DBAlias) then begin
      j:=Items[i].SubFolders.GetSubFolderNum(Usr.TableName);
      if j>=0 then begin
        for k:=0 to Items[i].SubFolders[j].Files.Count-1 do
          if Items[i].SubFolders[j].Files[k].ID=ID then begin
            Items[i].SubFolders[j].Files.Delete(k,FDB_Answer);
            FDB_Answer.FDB_ID:=ClientID;
            FDB_Answer.Rules:=derNone;
            UserList.SendReport(Self,FDB_Answer,ClientID);
            Break;
          end;
      end else begin
        FDB_Answer.MsgStr:=sFDB_ERROR_FOLDER_NOT_EXISTS;
        FDB_Answer.MsgCode:=FDB_ERROR_FOLDER_NOT_EXISTS;
        FDB_Answer.FDB_ID:=ClientID;
        FDB_Answer.Rules:=derNone;
        UserList.SendReport(Self,FDB_Answer,ClientID);
      end;
      Break;
    end;
end;



procedure TFileDB.FindFiles(iParentFolder,iParentSubFolder: integer);
var
  sr: TSearchRec;
  s:string;
  fl:TFile;
  FDB_Answer:TFDB_Answer;
begin
  s:=Items[iParentFolder].SubFolders[iParentSubFolder].FullPath;
  if SysUtils.FindFirst(s+'*', faAnyFile, sr) = 0 then begin
    repeat
      if ((sr.Attr and faDirectory) <> faDirectory) and (ExtractFileExt(s+sr.Name)<>cFileInfoExt) then begin
//        fl.Name:=ExtractFileName(sr.Name);
//        fl.FullPath:=s+sr.Name;
        fl.RealFileName:=ExtractFileName(sr.Name);
        fl.RealFullPath:=s+sr.Name;

        fl.DateTime:=FileDateToDateTime(sr.Time);
        fl.Size:=sr.Size;
        fl.FileAttr:=sr.Attr;
        fl.Body:=EmptyStr;
        fl.iParentFolder:=iParentFolder;
        fl.iParentSubFolder:=iParentSubFolder;
        Items[iParentFolder].SubFolders[iParentSubFolder].Files.Add(fl,FDB_Answer,False);
      end;
    until SysUtils.FindNext(sr) <> 0;
    SysUtils.FindClose(sr);
  end;
end;

procedure TFileDB.FindSubFolders(iParentFolder: integer);
var
  sr: TSearchRec;
  sf:TSubFolder;
begin
  if SysUtils.FindFirst(Items[iParentFolder].FullPath+'*', faAnyFile, sr) = 0 then begin
    repeat
      if ((sr.Attr and faDirectory) = faDirectory) and (sr.Name<>'.') and (sr.Name<>'..') then begin
        sf.Name:=sr.Name;
        sf.FullPath:=Items[iParentFolder].FullPath+sr.Name+'\';
        sf.iParentFolder:=iParentFolder;
        FindFiles(iParentFolder,Items[iParentFolder].SubFolders.Add(sf));
      end;
    until SysUtils.FindNext(sr) <> 0;
    SysUtils.FindClose(sr);
  end;
end;

function TFileDB.Get(Index: Integer): TFolder;
begin
  if (Index<0) or (Index>=FolderList.Count) then begin
    Raise Exception.Create('List index out of bounds');
    Exit;
  end;
  Result:=PFolder(FolderList.Items[Index])^;
end;

function TFileDB.GetCount: integer;
begin
  Result:=0;
  if Assigned(FolderList) then begin
    Result:=FolderList.Count;
  end;
end;

function TFileDB.GetFileBody(FilePath: string): string;
var
  fs:TFileStream;
  ss:TStringStream;
begin
  Result:=EmptyStr;
  fs:=TFileStream.Create(FilePath,fmOpenRead);
  ss:=TStringStream.Create(EmptyStr);
  fs.Seek(0,0);
  ss.CopyFrom(fs,fs.Size);
  Result:=ss.DataString;
  fs.Free;
  ss.Free;
end;


function TFileDB.GetUniqueConnectID: integer;
var
  i, TempID:integer;
begin
  TempID:=Random(MaxInt-1)+1;
  i:=0;
  While i<Connectlist.Count-1 do begin
    if PCOnnection(Connectlist[i])^.ConnectionID=TempID then begin
      i:=0;
      TempID:=Random(MaxInt-1)+1;
      Continue;
    end;
    Inc(i);
  end;
  Result:=TempID;
end;

procedure TFileDB.Insert(Index: Integer; Item: TFolder);
var
  fo:PFolder;
begin
  if Assigned(FolderList) then begin
    New(fo);
    fo^:=Item;
    fo^.SubFolders:=TSubFolderList.Create;
    FolderList.Insert(Index,fo);
  end;

end;

procedure TFileDB.Put(Index: Integer; Item: TFolder);
begin
  if (Index<0) or (Index>=FolderList.Count) then begin
    Raise Exception.Create('List index out of bounds');
    Exit;
  end;
  PFolder(FolderList.Items[Index])^:=Item;
end;

//++++++++++++++++++++++DEDataSet+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


procedure CopyFileRecToFileClass(var FileClass:TFileClass; var FileRec:TFile; Back:boolean);
begin
  if not Assigned(FileClass) then exit;
  if not Back then begin
    FileClass.ID:=FileRec.ID;
    FileClass.Name:=FileRec.Name;
    FileClass.Body:=FileRec.Body;
    FileClass.FileType:=FileRec.FileType;
    FileClass.Size:=FileRec.Size;
    FileClass.DateTime:=FileRec.DateTime;
    FileClass.FileAttr:=FileRec.FileAttr;
    FileClass.FullPath:=FileRec.FullPath;
    FileClass.iParentFolder:=FileRec.iParentFolder;
    FileClass.iParentSubFolder:=FileRec.iParentSubFolder;
    FileClass.RealFullPath:=FileRec.RealFullPath;
    FileClass.RealFileName:=FileRec.RealFileName;
  end else begin
    FileRec.ID:=FileClass.ID;
    FileRec.Name:=FileClass.Name;
    FileRec.Body:=FileClass.Body;
    FileRec.FileType:=FileClass.FileType;
    FileRec.Size:=FileClass.Size;
    FileRec.DateTime:=FileClass.DateTime;
    FileRec.FileAttr:=FileClass.FileAttr;
    FileRec.FullPath:=FileClass.FullPath;
    FileRec.iParentFolder:=FileClass.iParentFolder;
    FileRec.iParentSubFolder:=FileClass.iParentSubFolder;
    FileRec.RealFileName:=FileClass.RealFileName;
    FileRec.RealFullPath:=FileClass.RealFullPath
  end;
end;

function TFileDB.StringToFileRec(FileStr: string): TFile;
var
  FileClass:TFileClass;
  ss:TStringStream;
begin
  FileClass:=TFileClass.Create(Self);
  ss:=TStringStream.Create(FileStr);
  ss.Seek(0,0);

  ss.ReadComponent(FileClass);

  CopyFileRecToFileClass(FileClass,Result,True);

  FileClass.Free;
  ss.Free;
end;

function TFileDB.FileRecToString(FileRec: TFile): string;
var
  FileClass:TFileClass;
  ss:TStringStream;
begin
  FileClass:=TFileClass.Create(Self);
  ss:=TStringStream.Create(EmptyStr);

  CopyFileRecToFileClass(FileClass,FileRec,False);

  ss.WriteComponent(FileClass);
  ss.Seek(0,0);
  Result:=ss.DataString;

  FileClass.Free;
  ss.Free;
end;




function TFileDB.SetCurrentTable(TableName: string):boolean;
var
  CFDB:TCommandToFDB;
begin
  CFDB.FDB_ID:=CurrentConnection.ClientFDB_ID;
  CFDB.ConnectionID:=CurrentConnection.ConnectionID;
  CFDB.iCommand:=fdbc_SELECT_CURRENT_TABLE;
  CFDB.Params:=TableName;
  FTableSet:=False;
  SendCommandToFDB(CFDB,CurrentConnection.LocalConnect);
  Result:=FTableSet;
end;
//DONE: Прописать для клиентской стороны запрос селекта

procedure TFileDB.GetTableList(TableList:TStrings);
var
  CFDB:TCommandToFDB;
  i:integer;
begin
  CFDB.FDB_ID:=CurrentConnection.ClientFDB_ID;
  CFDB.ConnectionID:=CurrentConnection.ConnectionID;
  CFDB.iCommand:=fdbc_GET_TABLES_LIST;
  CFDB.Params:=EmptyStr;
  SendCommandToFDB(CFDB,CurrentConnection.LocalConnect);
  if CurrentConnection.LocalConnect then begin
  end else begin

  end;
  TableList.BeginUpdate;
  for i:=0 to FileDataSet.GetCountTables-1 do begin
    TableList.Add(FileDataSet.GetTableName(i));
  end;
  TableList.EndUpdate;
end;

procedure TFileDB.SelectFiles(aRecordNumber, aRecordCount: integer;
  aNoBody: boolean);
var
  CFDB:TCommandToFDB;
  s:string;
begin
  CFDB.FDB_ID:=CurrentConnection.ClientFDB_ID;
  CFDB.ConnectionID:=CurrentConnection.ConnectionID;
  CFDB.iCommand:=fdbc_SELECT;
  if aNoBody then s:='1' else s:='0';
  CFDB.Params:=IntToStr(aRecordNumber)+cr+IntToStr(aRecordCount)+cr+s;
  SendCommandToFDB(CFDB,CurrentConnection.LocalConnect);

  if CurrentConnection.LocalConnect then begin//Данные уже у нас
  end else begin //Данные только идут, фиг знает чего делать...
  end;
end;


function TFileDB.InsertFile(fl: TFile):integer;
var
  CFDB:TCommandToFDB;
begin
  CFDB.FDB_ID:=CurrentConnection.ClientFDB_ID;
  CFDB.ConnectionID:=CurrentConnection.ConnectionID;
  CFDB.iCommand:=fdbc_INSERT;
  CFDB.Params:=EmptyStr;
  CFDB.FDB_Data:=FileRecToString(fl);
  SendCommandToFDB(CFDB,CurrentConnection.LocalConnect);
//  Result:=0;
  Result:=FlastFileID_;
end;

procedure TFileDB.DeleteFile(ID: integer);
var
  CFDB:TCommandToFDB;
begin
  CFDB.FDB_ID:=CurrentConnection.ClientFDB_ID;
  CFDB.ConnectionID:=CurrentConnection.ConnectionID;
  CFDB.iCommand:=fdbc_DELETE;
  CFDB.Params:=IntToStr(ID);
  CFDB.FDB_Data:=EmptyStr;
  SendCommandToFDB(CFDB,CurrentConnection.LocalConnect);
end;

procedure TFileDB.UpdateFile(fl: TFile);
var
  CFDB:TCommandToFDB;
begin
  CFDB.FDB_ID:=CurrentConnection.ClientFDB_ID;
  CFDB.ConnectionID:=CurrentConnection.ConnectionID;
  CFDB.iCommand:=fdbc_UPDATE;
  CFDB.Params:=EmptyStr;
  CFDB.FDB_Data:=FileRecToString(fl);
  SendCommandToFDB(CFDB,CurrentConnection.LocalConnect);
end;


{ TFileList } //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

constructor TFileList.Create;
begin
  FList:=TList.Create;
end;

function TFileList.GetUniqueID: integer;
var
  i, TempID:integer;
begin
  TempID:=Random(MaxInt-2)+2;
  i:=0;
  While i<Count do begin
    if Items[i].ID=TempID then begin
      i:=0;
      TempID:=Random(MaxInt-2)+2;
      Continue;
    end;
    Inc(i);
  end;
  Result:=TempID;
end;


function TFileList.GetFileType(const FilePath: string): TDEFileType;
var
  Extension: string;
begin
  Extension := ExtractFileExt(FilePath);
  if SameText(Extension, sExtensionTXT) then
    Result := deftText
  else if SameText(Extension, sExtensionDOC) then
    Result := deftDocument
  else if SameText(Extension, sExtensionBMP) then
    Result := deftBitmap
  else if SameText(Extension, sExtensionJPG) then
    Result := deftJpeg
  else if SameText(Extension, sExtensionEXE) then
    Result := deftExe
  else if SameText(Extension, sExtensionXML) then
    Result := deftXML
  else if SameText(Extension, sExtensionDLL) then
    Result := deftDll
  else if SameText(Extension, sExtensionPAS) then
    Result := deftPas
  else
    Result := deftUnknown;
end;


procedure TFileList.SetTruePathAndNameAndType(var fl: TFile);
var
  s,s2,Tmp,c:string;
  i:integer;
begin
  s:=fl.RealFullPath;
  i:=1;
  s2:=EmptyStr;
  While i<=Length(s) do begin
    if (Ord(s[i])=35) and (i+2<Length(s)) then begin
      Tmp:=s[i+1]+s[i+2];
      try
        c:=Chr(StrToInt('$'+Tmp));
      except
        c:=EmptyStr;
      end;
      s2:=s2+c;
      Inc(i,2);
    end else s2:=s2+s[i];
    Inc(i);
  end;
  if Pos(Chr(cCountFilesSymbol),s2)>0 then System.Delete(s2,Pos(Chr(cCountFilesSymbol),s2),4);
  fl.FullPath:=s2;     //TODO -oDF: Умный парсер? О да, хочешь поебаться? Напиши умный парсер :-))
  fl.RealFileName:=ExtractFileName(fl.RealFullPath);
  fl.Name:=ExtractFileName(fl.FullPath);
  fl.FileType:=GetFileType(fl.RealFullPath);
end;

procedure TFileList.SetRealPathAndNameAndType(var fl: TFile);
  function GetFNum(RealFullPath:string; var rfp_woc:string):integer;
  var
    s:string;
    p:integer;
  begin
    p:=Pos(Chr(cCountFilesSymbol),RealFullPath);
    If p>0 then begin
      s:=Copy(RealFullPath,p+1,3);
      rfp_woc:=RealFullPath;
      System.Delete(rfp_woc,p,4);
      Result:=StrToInt('$'+s);
    end else begin
      Result:=0;
      rfp_woc:=RealFullPath;
    end;
  end;
var
  s,s2,rfp_woc:string;
  i,j, fnum, TrueNum:integer;
  FilesNum:array of integer;
begin
  SetLength(FilesNum,0);
  s:=ExtractFileName(fl.FullPath);
  fl.Name:=ExtractFileName(fl.FullPath);
  // Запрещенные символы
  s2:=EmptyStr;
  for i:=1 to Length(s) do
    if Ord(s[i]) in WroneSym then begin
       s2:=s2+'#'+IntToHex(Ord(s[i]),2);
    end else s2:=s2+s[i];
  fl.RealFileName:=s2;
  fl.RealFullPath:=ExtractFilePath(fl.FullPath)+s2;

  //Копии файлов с одинаковым названием
  for i:=0 to Count-1 do begin
    fnum:=GetFNum(Items[i].RealFullPath,rfp_woc);
    if AnsiUpperCase(rfp_woc)=AnsiUpperCase(fl.RealFullPath) then begin
      SetLength(FilesNum,High(FilesNum)+2);
      FilesNum[High(FilesNum)]:=fnum;
    end;
  end;
  if High(FilesNum)<0 then TrueNum:=0 else begin
    for i:=0 to High(FilesNum)-1 do
      for j:=i+1 to High(FilesNum) do
        if FilesNum[i]>FilesNum[j] then begin
          TrueNum:=FilesNum[i];
          FilesNum[i]:=FilesNum[j];
          FilesNum[j]:=TrueNum;
        end;
    TrueNum:=FilesNum[High(FilesNum)]+1;
    for i:=0 to High(FilesNum)-1 do
      if FilesNum[i+1]-FilesNum[i]>1 then begin
        TrueNum:=FilesNum[i]+1;
        Break;
      end;
  end;

  if TrueNum>0 then begin
    s2:=ExtractFileExt(fl.RealFileName);
    s:=Copy(fl.RealFileName,1,Length(fl.RealFileName)-Length(s2));
    s:=s+Chr(cCountFilesSymbol)+IntToHex(TrueNum,3);
    s:=s+s2;
    fl.RealFullPath:=ExtractFilePath(fl.RealFullPath)+s;
    fl.RealFileName:=s;
  end;
  fl.FileType:=GetFileType(fl.RealFullPath);
end;      

function TFileList.Add(var Item: TFile; Var FDB_Answer:TFDB_Answer; bMakeFile:boolean): Integer;
var
  fl:PFile;
begin
  New(fl);
  fl^:=Item;
  if Double(fl^.DateTime)<0 then fl^.DateTime:=now;
  fl^.ID:=GetUniqueID;

  SetTruePathAndNameAndType(fl^);

  FDB_Answer.MsgStr:=sFDB_DATA_FILE_ADD_DONE;
  FDB_Answer.MsgCode:=FDB_DATA_FILE_ADD_DONE;
  FDB_Answer.CanOverload:=True;

  if bMakeFile then CreateFile(fl^,FDB_Answer);

  Result:=Flist.Add(fl);
end;

procedure TFileList.Clear;
var
  fl:PFile;
  i:integer;
begin
  if Assigned(FList) then begin
    for i:=0 to FList.Count-1 do begin
      fl:=FList.Items[i];
      Dispose(fl);
    end;
    FList.Clear;
  end;
end;

procedure TFileList.Delete(const Index: Integer; var FDB_Answer: TFDB_Answer);
var
  fl:PFile;
begin
  if Assigned(FList) then begin
    if (Index<0) or (Index>=Count) then begin
      FDB_Answer.MsgStr:=sFDB_ERROR_FILE_NOT_IN_LIST;
      FDB_Answer.MsgCode:=FDB_ERROR_FILE_NOT_IN_LIST;
    end else begin
      fl:=FList.Items[Index];
      DeleteFile(fl^,FDB_Answer);
      Dispose(fl);
      Flist.Delete(Index);
    end;
  end;
end;

function TFileList.Rename(Index: integer; fl_new:TFile;
  var FDB_Answer: TFDB_Answer): integer;
begin
  Result:=-1;
  if Assigned(FList) then begin
    if (Index<0) or (Index>=Count) then begin
      Result:=2;
      FDB_Answer.MsgStr:=sFDB_ERROR_FILE_NOT_IN_LIST;
      FDB_Answer.MsgCode:=FDB_ERROR_FILE_NOT_IN_LIST;
    end else begin
      Result:=1;
      RenameFile(Items[Index],fl_new,FDB_Answer);
      Items[Index]:=fl_new;
      FDB_Answer.MsgStr:=sFDB_DATA_FILE_UPDATE_DONE;
      FDB_Answer.MsgCode:=FDB_DATA_FILE_UPDATE_DONE;
    end;
  end;
end;

function TFileList.GetCount: integer;
begin
  Result:=0;
  if Assigned(FList) then begin
    Result:=Flist.Count;
  end;
end;

function TFileList.CreateFile(var fl: TFile; Var FDB_Answer:TFDB_Answer):integer;
var
  ss:TStringStream;
  fs:TFileStream;
begin
  Result:=1;
//  fl.Name:=ExtractFileName(fl.FullPath);
//  fl.RealFileName:=ExtractFileName(fl.FullPath);
  ss:=TStringStream.Create(fl.Body);
  fs:=TFileStream.Create(fl.RealFullPath,fmCreate);
  ss.Seek(0,0);
  fs.CopyFrom(ss,ss.Size);
  fs.Free;
  ss.Free;
  FDB_Answer.Data:=IntToStr(fl.ID);
  FDB_Answer.MsgCode:=FDB_DATA_FILE_CREATE_DONE;
  FDB_Answer.MsgStr:=sFDB_DATA_FILE_CREATE_DONE;
end;

{function TFileList.CreateFile(var fl: TFile; Var FDB_Answer:TFDB_Answer):integer;

  function GetFileNameFromInfo(FilePath:string):string;
  var
    infFilePath:string;
    ss:TStringStream;
    fs:TFileStream;
  begin
    infFilePath:=Copy(FilePath,1,Length(FilePath)-Length(ExtractFileExt(FilePath)))+cFileInfoExt;
    Result:=EmptyStr;
    if FileExists(infFilePath) then begin
      ss:=TStringStream.Create(EmptyStr);
      fs:=TFileStream.Create(infFilePath,fmOpenRead);
      fs.Seek(0,0);
      ss.CopyFrom(fs,fs.Size);
      Result:=ss.DataString;
      ss.Free;
      fs.Free;
    end;
  end;

  procedure RenameFile_CreateInfoFile(var fl:TFile; CreateNewData:boolean);
  var
    fs:TFileStream;
    ss:TStringStream;
    s1,s2:string;
    i:integer;
  begin
    if not CreateNewData then begin
      fl.RealFileName:=fl.Name;
      fl.RealFullPath:=fl.FullPath;
      fl.Name:=GetFileNameFromInfo(fl.FullPath);
      fl.FullPath:=ExtractFilePath(fl.FullPath)+fl.Name;
      fl.FileType:=GetFileType(fl.FullPath);
      s1:=ExtractFileName(fl.RealFileName);
      s2:=ExtractFileExt(ExtractFileName(fl.RealFileName));
      s1:=Copy(s1,1,Length(s1)-Length(s2));
      i:=fl.ID;
      try
        fl.ID:=StrToInt(s1);
      except
        fl.ID:=i;
      end;        
    end else begin
      ss:=TStringStream.Create(fl.Name);
      fs:=TFileStream.Create(ExtractFilePath(fl.FullPath)+IntToStr(fl.ID)+cFileInfoExt,fmCreate);
      ss.Seek(0,0);
      fs.CopyFrom(ss,ss.Size);
      ss.Free;
      fs.Free;
      SysUtils.RenameFile(fl.FullPath,ExtractFilePath(fl.FullPath)+IntToStr(fl.ID)+cFileDataExt);
      fl.RealFullPath:=ExtractFilePath(fl.FullPath)+IntToStr(fl.ID)+cFileDataExt;
      fl.RealFileName:=ExtractFileName(fl.RealFullPath);
    end;
  end;

var
  fs:TFileStream;
  ss:TStringStream;
begin
  Result:=1;
  FDB_Answer.Data:=IntToStr(fl.ID);
  if ExtractFileExt(fl.FullPath)=cFileDataExt then begin
    if FileExists(fl.FullPath) then RenameFile_CreateInfoFile(fl,False) else begin
      FDB_Answer.MsgCode:=FDB_ERROR_FILE_NOT_EXISTS;
      FDB_Answer.MsgStr:=sFDB_ERROR_FILE_NOT_EXISTS;
    end;
  end else begin
    if not FileExists(fl.FullPath) then begin
      fs:=nil;
      ss:=TStringStream.Create(fl.Body);
      ss.Seek(0,0);
      try
        fs:=TFileStream.Create(fl.FullPath,fmCreate);
        fs.CopyFrom(ss,ss.Size);
        FDB_Answer.MsgCode:=FDB_DATA_FILE_CREATE_DONE;
        FDB_Answer.MsgStr:=sFDB_DATA_FILE_CREATE_DONE;
      except
        FDB_Answer.MsgCode:=FDB_ERROR_CREATE_FILE;
        FDB_Answer.MsgStr:=sFDB_ERROR_CREATE_FILE;
      end;
      ss.Free;
      fs.Free;
    end;
    fl.FileType:=GetFileType(fl.FullPath);
    RenameFile_CreateInfoFile(fl,True);
  end;
end;   }


function TFileList.DeleteFile(fl: TFile; Var FDB_Answer:TFDB_Answer):integer;
var
  fInfo:string;
begin
  Result:=1;
  if not FileExists(fl.RealFullPath) then begin
    FDB_Answer.MsgCode:=FDB_ERROR_FILE_NOT_EXISTS;
    FDB_Answer.MsgStr:=sFDB_ERROR_FILE_NOT_EXISTS;
    Result:=3;
    Exit;
  end;
  fInfo:=Copy(fl.RealFullPath,1,Length(fl.RealFullPath)-Length(ExtractFileExt(fl.RealFullPath)));
  fInfo:=fInfo+cFileInfoExt;
  SysUtils.DeleteFile(fl.RealFullPath);
  SysUtils.DeleteFile(fInfo);
  FDB_Answer.MsgCode:=FDB_DATA_FILE_DELETE_DONE;
  FDB_Answer.MsgStr:=sFDB_DATA_FILE_DELETE_DONE;
end;

function TFileList.RenameFile(Old_fl,New_fl :TFile; Var FDB_Answer:TFDB_Answer):integer;
begin
  Result:=1;
  if not FileExists(old_fl.FullPath) then begin
    Result:=3;
    Exit;
  end;
  if FileExists(new_fl.FullPath) then begin
    Result:=2;
    Exit;
  end;
  SysUtils.RenameFile(Old_fl.FullPath,New_fl.FullPath);
end;

procedure TFileList.Insert(const Index: Integer; var Item: TFile; var FDB_Answer: TFDB_Answer; const bMakeFile: Boolean);
var
  fl:PFile;
begin
  Item.ID:=GetUniqueID;
  New(fl);
  fl^:=Item;

  SetTruePathAndNameAndType(fl^);

  FDB_Answer.MsgStr:=sFDB_DATA_FILE_INSERT_DONE;
  FDB_Answer.MsgCode:=FDB_DATA_FILE_INSERT_DONE;
  FDB_Answer.CanOverload:=True;
  if bMakeFile then CreateFile(fl^,FDB_Answer);

  Flist.Insert(Index,fl);
end;



destructor TFileList.Destroy;
var
  i:integer;
  fl:PFile;
begin
  for i:=0 to FList.Count-1 do begin
    fl:=FList.Items[i];
    Dispose(fl);
  end;
  Flist.Clear;
  FList.Destroy;
  inherited;
end;

function TFileList.Get(const Index: Integer): TFile;
begin
  if (Index<0) or (Index>=Flist.Count) then begin
    Raise Exception.Create('List index out of bounds');
    Exit;
  end;
  Result:=PFile(Flist.Items[Index])^;
end;


procedure TFileList.Put(const Index: Integer; Item: TFile);
begin
  if (Index<0) or (Index>=Flist.Count) then begin
    Raise Exception.Create('List index out of bounds');
    Exit;
  end;
  PFile(FList.Items[Index])^:=Item;
end;

{ TSubFolderList } //+++++++++++++++++++++++++++++++++++++++++++++++++++++

function TSubFolderList.Add(Item: TSubFolder): Integer;
var
  sf:PSubFolder;
begin
  Result:=-1;
  if Assigned(FList) then begin
    New(sf);
    sf^:=Item;
    sf^.Files:=TFileList.Create;
    Result:=Flist.Add(sf);
  end;
end;

procedure TSubFolderList.Clear;
var
  i:integer;
  sf:PSubFolder;
begin
  if Assigned(FList) then begin
    for i:=0 to FList.Count-1 do begin
      sf:=FList.Items[i];
      sf^.Files.Free;
      Dispose(sf);
    end;
    FList.Clear;
  end;
end;

constructor TSubFolderList.Create;
begin
  FList:=TList.Create;
end;

procedure TSubFolderList.Delete(const Index: Integer);
var
  sf:PSubFolder;
begin
  if Assigned(FList) then begin
    sf:=Flist.Items[Index];
    sf^.Files.Free;
    Dispose(sf);
    Flist.Delete(Index);
  end;
end;

destructor TSubFolderList.Destroy;
var
  i:integer;
  sf:PSubFolder;
begin
  for i:=0 to FList.Count-1 do begin
    sf:=FList.Items[i];
    sf^.Files.Free;
    Dispose(sf);
  end;
  Flist.Clear;
  FList.Destroy;
  inherited;
end;

function TSubFolderList.Get(const Index: Integer): TSubFolder;
begin
  if (Index<0) or (Index>=Flist.Count) then begin
    Raise Exception.Create('List index out of bounds');
    Exit;
  end;
  Result:=PSubFolder(Flist.Items[Index])^;
end;


function TSubFolderList.GetCount: integer;
begin
  Result:=0;
  if Assigned(FList) then begin
    Result:=Flist.Count;
  end;
end;


function TSubFolderList.GetSubFolderNum(const SubFolder: string): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Pred(Count) do
    if SameText(Items[Index].Name, SubFolder) then
      begin
        Result := Index;
        Break;
      end;
end;

procedure TSubFolderList.Insert(const Index: Integer; Item: TSubFolder);
var
  sf:PSubFolder;
begin
  if Assigned(FList) then begin
    New(sf);
    sf^:=Item;
    sf^.Files:=TFileList.Create;
    Flist.Insert(Index,sf);
  end;
end;

procedure TSubFolderList.Put(const Index: Integer; Item: TSubFolder);
begin
  if (Index<0) or (Index>=Flist.Count) then begin
    Raise Exception.Create('List index out of bounds');
    Exit;
  end;
  PSubFolder(FList.Items[Index])^:=Item;
end;


{ TDEFileDataSet }

procedure TFileDataSet.AddRec(FileRec: TFile);
var
  fl:PFile;
begin
  New(fl);
  fl^:=FileRec;
  FList.Add(fl);
  FCount:=FList.Count;
  FRecNo:=FCount;
end;

procedure TFileDataSet.Clear;
var
  i:integer;
  fl:PFile;
begin
  for i:=0 to FList.Count-1 do begin
    fl:=FList.Items[i];
    fl^.Body:=EmptyStr;
    Dispose(fl);
  end;
  FList.Clear;
  FCount:=FList.Count;
  FRecNo:=0;
end;

constructor TFileDataSet.Create;
begin
  FList:=TList.Create;
end;

destructor TFileDataSet.Destroy;
var
  i:integer;
  fl:PFile;
begin
  for i:=0 to FList.Count-1 do begin
    fl:=Flist.Items[i];
    fl^.Body:=EmptyStr;
    Dispose(fl);
  end;
  FList.Clear;
  FList.Free;
  inherited;
end;

function TFileDataSet.FieldByName(const FieldName: string): TDEFileField;
var
  fl:PFile;
begin
  if (FRecNo<1) or (FRecNo>FList.Count) then begin
    Exit;
  end;
  fl:=Flist.Items[RecNo-1];
  if AnsiUpperCase(FieldName)='NAME' then begin
    Result.AsString:=fl^.Name;
    Result.AsInteger:=0;
    Result.AsDateTime:=0;
    Result.AsVariant:=fl^.Name;
  end else if AnsiUpperCase(FieldName)='REALFILENAME' then begin
    Result.AsString:=fl^.RealFileName;
    Result.AsInteger:=0;
    Result.AsDateTime:=0;
    Result.AsVariant:=fl^.RealFileName;
  end else if AnsiUpperCase(FieldName)='REALFULLPATH' then begin
    Result.AsString:=fl^.RealFullPath;
    Result.AsInteger:=0;
    Result.AsDateTime:=0;
    Result.AsVariant:=fl^.RealFullPath;
  end else if AnsiUpperCase(FieldName)='BODY' then begin
    Result.AsString:=fl^.Body;
    Result.AsInteger:=0;
    Result.AsDateTime:=0;
    Result.AsVariant:=fl^.Body;
  end else if AnsiUpperCase(FieldName)='FULLPATH' then begin
    Result.AsString:=fl^.FullPath;
    Result.AsInteger:=0;
    Result.AsDateTime:=0;
    Result.AsVariant:=fl^.FullPath;
  end else if AnsiUpperCase(FieldName)='SIZE' then begin
    Result.AsString:=IntToStr(fl^.Size);
    Result.AsInteger:=fl^.Size;
    Result.AsDateTime:=0;
    Result.AsVariant:=fl^.Size;
  end else if AnsiUpperCase(FieldName)='FILEATTR' then begin
    Result.AsString:=EmptyStr;
//    if (fl^.FileAttr and faReadOnly)=fl^.FileAttr then Result.AsString:=Result.AsString+'Только чтение, ';
//    if (fl^.FileAttr and faHidden)=fl^.FileAttr then Result.AsString:=Result.AsString+'Скрытый, ';
//    if (fl^.FileAttr and faSysFile)=fl^.FileAttr then Result.AsString:=Result.AsString+'Системный, ';
//    if (fl^.FileAttr and faVolumeID)=fl^.FileAttr then Result.AsString:=Result.AsString+'Метка тома, ';
    if (fl^.FileAttr and faDirectory)=fl^.FileAttr then Result.AsString:=Result.AsString+'Директория, ';
//    if (fl^.FileAttr and faArchive)=fl^.FileAttr then Result.AsString:=Result.AsString+'Архивный, ';
    if (fl^.FileAttr and faAnyFile)=fl^.FileAttr then Result.AsString:=Result.AsString+'Файл, ';
    if Copy(Result.AsString,Length(Result.AsString)-1,2)=', ' then Delete(Result.AsString,Length(Result.AsString)-1,2);
    Result.AsInteger:=fl^.FileAttr;
    Result.AsDateTime:=0;
    Result.AsVariant:=fl^.FileAttr;
  end else if AnsiUpperCase(FieldName)='DATETIME' then begin
    Result.AsString:=DateToStr(fl^.DateTime)+' '+TimeToStr(fl^.DateTime);
    Result.AsInteger:=Trunc(Double(fl.DateTime));
    Result.AsDateTime:=fl.DateTime;
    Result.AsVariant:=fl.DateTime;
  end else if AnsiUpperCase(FieldName)='ID' then begin
    Result.AsString:=IntToStr(fl.ID);
    Result.AsInteger:=fl.ID;
    Result.AsVariant:=fl.ID;
  end else if AnsiUpperCase(FieldName)='FILETYPE' then begin
    Result.AsString:=GetDEFileTypeAsString(fl^.FileType);
    Result.AsInteger:=Integer(fl^.FileType);
    Result.AsDateTime:=0;
    Result.AsVariant:=fl^.FileType;
  // MODIFY BY SAVELIEV 25.12.2004
  end else if AnsiUpperCase(FieldName)='PARENTFOLDER' then begin
    Result.AsString:=IntToStr(fl.iParentFolder);
    Result.AsInteger:=fl.iParentFolder;
    Result.AsVariant:=fl.iParentFolder;
  end else if AnsiUpperCase(FieldName)='PARENTSUBFOLDER' then begin
    Result.AsString:=IntToStr(fl.iParentSubFolder);
    Result.AsInteger:=fl.iParentSubFolder;
    Result.AsVariant:=fl.iParentSubFolder;
  end;
end;

function TFileDataSet.Get(const Index: Integer): Variant;
var
  fl:PFile;
begin
  if (FRecNo<=0) or (FRecNo>FList.Count) then begin
    Exit;
  end;
  fl:=Flist.Items[FRecNo];
  case Index of
    0:begin//ID
        Result:=fl^.ID;
      end;
    1:begin//Name
        Result:=fl^.Name;
      end;
    2:begin//RealName
        Result:=fl^.RealFileName;
      end;
    3:begin//Body
        Result:=fl^.Body;
      end;
    4:begin//FileType
        Result:=fl^.FileType;
      end;
    5:begin//Size
        Result:=fl^.Size;
      end;
    6:begin//DateTime
        Result:=fl^.DateTime;
      end;
    7:begin//FileAttr
        Result:=fl^.FileAttr;
      end;
    8:begin//FullPath
        Result:=fl^.FullPath;
      end;
    9:begin//iParentFolder
        Result:=fl^.iParentFolder;
      end;
    10:begin//iParentSubFolder
         Result:=fl^.iParentSubFolder;
       end;
    11:begin//RealFullPath
         Result:=fl^.RealFullPath;
       end;
  end;//CASE
end;

function TFileDataSet.GetCount: integer;
begin
  Result:=FList.Count;
end;

function TFileDataSet.GetCountTables: integer;
var
  sl:TStringList;
begin
  sl:=TStringList.Create;
  sl.Text:=FTableList;
  Result:=sl.Count;
  sl.Free;
end;

function TFileDataSet.GetCurrentRec: TFile;
begin
  if (FRecNo<=0) or (FRecNo>FList.Count) then begin
    Exit;
  end;
  Result:=PFile(Flist.Items[FRecNo-1])^;
end;

function TFileDataSet.GetTableName(aNumber: integer): string;
var
  sl:TStringList;
begin
  sl:=TStringList.Create;
  sl.Text:=FTableList;
  Result:=EmptyStr;
  if (aNumber>=0) and (aNumber<sl.Count) then Result:=sl.Strings[aNumber];
  sl.Free;
end;

{ TFileUserList }

function TFileUserList.Add(Item: TUserRec): Integer;
var
  ur:PUserRec;
begin
  Result:=-1;
  if Assigned(FUsersActive) then begin
    New(ur);
    ur^:=Item;
    Result:=FUsersActive.Add(ur);
  end;
end;

procedure TFileUserList.Clear;
var
  ur:PUserRec;
  i:integer;
begin
  if Assigned(FUsersActive) then begin
    for i:=0 to FUsersActive.Count-1 do begin
      ur:=FUsersActive.Items[i];
      Dispose(ur);
    end;
    FUsersActive.Clear;
  end;
end;

constructor TFileUserList.Create;
begin
  FUsersActive:=TList.Create;
  FAccounts:=TList.Create;
end;

procedure TFileUserList.Delete(Index: Integer);
var
  ur:PUserRec;
begin
  if Assigned(FUsersActive) then begin
    ur:=FUsersActive.Items[Index];
    Dispose(ur);
    FUsersActive.Delete(Index);
  end;
end;

destructor TFileUserList.Destroy;
var
  i:integer;
  ur:PUserRec;
begin
  for i:=0 to FAccounts.Count-1 do begin
    ur:=FAccounts.Items[i];
    Dispose(ur);
  end;
  FAccounts.Clear;
  FAccounts.Destroy;

  for i:=0 to FUsersActive.Count-1 do begin
    ur:=FUsersActive.Items[i];
    Dispose(ur);
  end;
  FUsersActive.Clear;
  FUsersActive.Destroy;

  inherited;
end;

function TFileUserList.Get(Index: Integer): TUserRec;
begin
  if (Index<0) or (Index>=FUsersActive.Count) then begin
    Raise Exception.Create('List index out of bounds');
    Exit;
  end;
  Result:=PUserRec(FUsersActive.Items[Index])^;
end;

function TFileUserList.GetCount: integer;
begin
  Result:=0;
  if Assigned(FUsersActive) then begin
    Result:=FUsersActive.Count;
  end;
end;

procedure TFileUserList.Insert(Index: Integer; Item: TUserRec);
var
  ur:PUserRec;
begin
  if Assigned(FUsersActive) then begin
    New(ur);
    ur^:=Item;
    FUsersActive.Insert(Index,ur);
  end;
end;

procedure TFileUserList.Put(Index: Integer; Item: TUserRec);
begin
  if (Index<0) or (Index>=FUsersActive.Count) then begin
    Raise Exception.Create('List index out of bounds');
    Exit;
  end;
  PUserRec(FUsersActive.Items[Index])^:=Item;
end;


function TFileUserList.GetUniqueID: integer;
var
  i, TempID:integer;
begin
  TempID:=Random(MaxInt-1)+1;
  i:=0;
  While i<Count do begin
    if Items[i].ID=TempID then begin
      i:=0;
      TempID:=Random(MaxInt-1)+1;
      Continue;
    end;
    Inc(i);
  end;
  Result:=TempID;
end;

procedure TFileUserList.ClearUserRec(var UserRec: TUserRec);
begin
  UserRec.Name:=EmptyStr;
  UserRec.Password:=EmptyStr;
  UserRec.Rules:=derNone;
  UserRec.State:=desNone;
  UserRec.IP:=EmptyStr;
  UserRec.Port:=-1;
  UserRec.ID:=-1;
  UserRec.SockHandle:=-1;
  UserRec.LocalClient:=False;
end;


function TFileUserList.StateToStr(des: TDEState): string;
begin
  case des of
    desOnLine:Result:='Подключен';
    desOffLine:Result:='Не подключен';
    desNone:Result:='Нет данных';
  end;
end;

function TFileUserList.IndexIsCurrect(Index: integer): boolean;
begin
  if (Index<0) or (Index>=FUsersActive.Count) then Result:=False else Result:=True;
end;

procedure TFileUserList.Replace(Index: integer; Item: TUserRec);
begin
  if not IndexIsCurrect(Index) then Exit;
  PUserRec(FUsersActive.Items[Index])^:=Item;
end;



function TFileUserList.AddA(Item: TUserRec): Integer;
var
  ur:PUserRec;
begin
  Result:=-1;
  if Assigned(FAccounts) then begin
    Result:=CheckNewUserA(Item);
    if Result>=-1 then begin
      New(ur);
      ur^:=Item;
      Result:=FAccounts.Add(ur);
    end;
  end;
end;

procedure TFileUserList.ClearA;
var
  ur:PUserRec;
  i:integer;
begin
  if Assigned(FAccounts) then begin
    for i:=0 to FAccounts.Count-1 do begin
      ur:=FAccounts.Items[i];
      Dispose(ur);
    end;
    FAccounts.Clear;
  end;
end;

procedure TFileUserList.DeleteA(Index: Integer);
var
  ur:PUserRec;
begin
  if Assigned(FAccounts) then begin
    ur:=FAccounts.Items[Index];
    Dispose(ur);
    FAccounts.Delete(Index);
  end;
end;

function TFileUserList.GetA(Index: Integer): TUserRec;
begin
  if (Index<0) or (Index>=FAccounts.Count) then begin
    Raise Exception.Create('List index out of bounds');
    Exit;
  end;
  Result:=PUserRec(FAccounts.Items[Index])^;
end;

function TFileUserList.GetCountA: integer;
begin
  Result:=0;
  if Assigned(FAccounts) then begin
    Result:=FAccounts.Count;
  end;
end;

function TFileUserList.GetUniqueIDA: integer;
var
  i, TempID:integer;
begin
  TempID:=Random(MaxInt-1)+1;
  i:=0;
  While i<CountA do begin
    if ItemsA[i].ID=TempID then begin
      i:=0;
      TempID:=Random(MaxInt-1)+1;
      Continue;
    end;
    Inc(i);
  end;
  Result:=TempID;
end;

function TFileUserList.IndexIsCurrectA(Index: integer): boolean;
begin
  if (Index<0) or (Index>=FAccounts.Count) then Result:=False else Result:=True;
end;

procedure TFileUserList.InsertA(Index: Integer; Item: TUserRec);
var
  ur:PUserRec;
begin
  if Assigned(FAccounts) then begin
    New(ur);
    ur^:=Item;
    FAccounts.Insert(Index,ur);
  end;
end;

procedure TFileUserList.PutA(Index: Integer; Item: TUserRec);
begin
  if (Index<0) or (Index>=FAccounts.Count) then begin
    Raise Exception.Create('List index out of bounds');
    Exit;
  end;
  PUserRec(FAccounts.Items[Index])^:=Item;
end;

procedure TFileUserList.ReplaceA(Index: integer; Item: TUserRec);
begin
  if not IndexIsCurrectA(Index) then Exit;
  PUserRec(FAccounts.Items[Index])^:=Item;
end;

function TFileUserList.RulesToStr(der: TDERules): string;
begin
  case der of
    derIgnore:Result:='Игнорируемый';
    derRW:Result:='Чтение/запись';
    derR:Result:='Чтение';
    derW:Result:='Запись';
    derAll:Result:='Полные';
    derNone:Result:='Нет данных';
  end;
end;

function TFileUserList.CheckNewUserA(Item: TUserRec): integer;
var
  i:integer;
begin
  Result:=-1;
  for i:=0 to CountA-1 do begin
    if (AnsiUpperCase(ItemsA[i].DBAlias)=AnsiUpperCase(Item.DBAlias)) then
      if (AnsiUpperCase(ItemsA[i].Name)=AnsiUpperCase(Item.Name))
      {$IFDEF USER_HAVE_ONCE_PASSWORD}or{$ENDIF}{$IFNDEF USER_HAVE_ONCE_PASSWORD}and{$ENDIF}
         (ItemsA[i].Password=Item.Password) then begin
        Result:=-2;
        Break;
      end;
  end;
end;

procedure TFileUserList.TryLogin(LocalConnect: boolean; IP: string; Port,
  SocketHandle: integer; Login, Password,dbName,dbPath: string; ConnectionID:integer);
var
  FDB_Answer:TFDB_Answer;
  i:integer;
  ur,ur_new:TUserRec;
  TrueClient:boolean;
begin

  FDB_Answer.MsgCode:=0;
  FDB_Answer.MsgStr:='Login not in list';
  FDB_Answer.FDB_ID:=-1;
  FDB_Answer.Rules:=derNone;
  FDB_Answer.ConnectionID:=ConnectionID;
  FDB_Answer.MsgCode:=FDB_CONNECT_WRONE_LOGIN_PASS_DB;
  FDB_Answer.MsgStr:=sFDB_CONNECT_WRONE_LOGIN_PASS_DB;

  TrueClient:=False;

  for i:=0 to CountA-1 do begin
    ur:=ItemsA[i];
    if LocalConnect then begin
      if  (AnsiLowerCase(Login)=AnsiLowerCase(cDefaultUserName))
      and (AnsiLowerCase(Password)=AnsiLowerCase(cDefaultUserPassword)) then begin
        TrueClient:=True;
        TFileDB(FOwner).AddEx(dbPath,dbPath,False);
      end;
    end else begin
      if (AnsiUpperCase(ur.Name)=AnsiUpperCase(Login))
      and ((AnsiUpperCase(ur.DBAlias)=AnsiUpperCase(dbName)) or (AnsiUpperCase(ur.DBPath)=AnsiUpperCase(dbName)))
      and (AnsiUpperCase(ur.Password)=AnsiUpperCase(Password)) then begin
        if (ur.IP='*') or (AnsiLowerCase(ur.IP)='all') then TrueClient:=True else
          if AnsiLowerCase(ur.IP)=AnsiLowerCase(IP) then TrueClient:=True else begin
            FDB_Answer.MsgCode:=FDB_CONNECT_WRONE_IP;
            FDB_Answer.MsgStr:=sFDB_CONNECT_WRONE_IP;
          end;
      end else begin
        FDB_Answer.MsgCode:=FDB_CONNECT_WRONE_LOGIN_PASS_DB;
        FDB_Answer.MsgStr:=sFDB_CONNECT_WRONE_LOGIN_PASS_DB;
      end;
    end;// LOCAL CONNECT
    if TrueClient then begin
      ClearUserRec(ur_new);
      ur_new.Name:=Login;
      ur_new.Password:=Password;
      ur_new.Rules:=ur.Rules;
      ur_new.State:=desOnLine;
      ur_new.IP:=IP;
      ur_new.Port:=Port;
      ur_new.ID:=GetUniqueID;
      ur_new.SockHandle:=SocketHandle;
      ur_new.LocalClient:=LocalConnect;
      if not LocalConnect then ur_new.DBAlias:=DBName else ur_new.DBAlias:=DBPath;
      ur_new.DBPath:=DBPath;
      ur_new.TableName:=EmptyStr;
      ur_new.ConnectionID:=ConnectionID;
      Add(ur_new);

      FDB_Answer.MsgCode:=FDB_CONNECT_SUCCESSFULL;
      FDB_Answer.MsgStr:=sFDB_CONNECT_SUCCESSFULL;
      FDB_Answer.FDB_ID:=ur_new.ID;
      FDB_Answer.Rules:=ur_new.Rules;
      FDB_Answer.ConnectionID:=ConnectionID;

      if Assigned(OnClientListChange) then FOnClientListChange(Self,ur_new, True);
      Break;
    end;
  end;
  SendReportSHandle(Self,FDB_Answer,LocalConnect,-1,SocketHandle);
end;


procedure TFileUserList.LoadUserListFromFile(DirPath:string);
var
  ss:TStringStream;
  fs:TFileStream;
  FilePath,s,sRec,sPole:string;
  p1,p2,p3,p4,NumPole:integer;
  usr:TUserRec;
  FileNotGood:boolean;
begin
  FilePath:=DirPath+cFileUserPrevilegies;
  if not FileExists(FilePath) then Exit;
  ss:=TStringStream.Create(EmptyStr);
  fs:=TFileStream.Create(FilePath,fmOpenRead);
  fs.Seek(0,0);
  ss.CopyFrom(fs,fs.Size);
  s := ss.DataString;
  ss.Free;
  fs.Free;
  p1:=Pos(cBeginFile,s);
  p2:=Pos(cEndFile,s);
  if (p1<=0) or (p2<=0) then begin
    {$IFDEF MSG_ON}
    ShowMessage('Неправильный файл');
    {$ENDIF}
    Exit;
  end;
//  Clear;
  FileNotGood:=False;
  System.Delete(s,p2,Length(s)-p2+1);// обрезали по стартам и стопам файл
  System.Delete(s,1,p1);
  While True do begin
    p1:=Pos(cBeginRecord,s);
    p2:=Pos(cEndRecord,s);
    if (p1<=0) or (p2<=0) then Break;
    sRec:=Copy(s,p1+1,p2-p1-1);// выбрали запись и обрезали файл
    System.Delete(s,1,p2);
    ClearUserRec(usr);
    p3:=Pos(cBeginOnceData,sRec);
    p4:=Pos(cEndOnceData,sRec);
    While (p3>0) and (p4>0) do begin
      sPole:=Copy(sRec,p3+1,p4-p3-1);
      System.Delete(sRec,1,p4);
      if Length(sPole)<=0 then Continue;
      NumPole:=Ord(Copy(sPole,1,1)[1])-64;
      sPole:=Copy(sPole,2,Length(sPole));
      p3:=Pos(cBeginOnceData,sRec);
      p4:=Pos(cEndOnceData,sRec);
      case NumPole of
        1:begin
            Usr.Name:=sPole;
          end;
        2:begin
            Usr.Password:=sPole;
          end;
        3:begin
            try
              Usr.Rules:=TDERules(StrToInt(sPole));
            except
              FileNotGood:=True;
            end;
          end;
        4:begin
            try
              Usr.State:=TDEState(StrToInt(sPole));
            except
              FileNotGood:=True;
            end;
          end;
        5:begin
            Usr.IP:=sPole;
          end;
        6:begin
            try
              Usr.Port:=StrToInt(sPole);
            except
              FileNotGood:=True;
            end;
          end;
        7:begin
            try
              Usr.ID:=StrToInt(sPole);
            except
              FileNotGood:=True;
            end;
          end;
        8:begin
            try
              Usr.SockHandle:=StrToInt(sPole);
            except
              FileNotGood:=True;
            end;
          end;
        9:begin
            try
              Usr.LocalClient:=Boolean(StrToInt(sPole));
            except
              FileNotGood:=True;
            end;
          end;
        10:begin
             Usr.DBAlias:=sPole;
           end;
        11:begin
             Usr.DBPath:=sPole;
           end;
        12:begin
             Usr.TableName:=sPole;
           end;
         13:begin
             try
               Usr.ConnectionID:=StrToInt(sPole);
             except
               FileNotGood:=True;
             end;
          end;
      end;//Case
    end;//While
    AddA(usr);
  end;//While
  if FileNotGood then     {$IFDEF MSG_ON}
    MessageBox(0, 'Файл поврежден', 'Предупреждение', MB_ICONEXCLAMATION or MB_OK);
  {$ENDIF}
end;

procedure TFileUserList.SaveUserListToFile(DirPath:string; NewFile:boolean; DBAlias:string);
var
  i:integer;
  s,FilePath:string;
  ss:TStringStream;
  fs:TFileStream;
begin
  s:=#1;
  if not NewFile then begin
    for i:=0 to CountA-1 do
      if AnsiUpperCase(ItemsA[i].DBAlias)=AnsiUpperCase(DBAlias) then begin
        s:=s+#2;
        s:=s+#3+Chr(64+1)+ItemsA[i].Name+#4;
        s:=s+#3+Chr(64+2)+ItemsA[i].Password+#4;
        s:=s+#3+Chr(64+3)+IntToStr(Integer(ItemsA[i].Rules))+#4;
        s:=s+#3+Chr(64+4)+IntToStr(Integer(ItemsA[i].State))+#4;
        s:=s+#3+Chr(64+5)+ItemsA[i].IP+#4;
        s:=s+#3+Chr(64+6)+IntToStr(ItemsA[i].Port)+#4;
        s:=s+#3+Chr(64+7)+IntToStr(ItemsA[i].ID)+#4;
        s:=s+#3+Chr(64+8)+IntToStr(ItemsA[i].SockHandle)+#4;
        s:=s+#3+Chr(64+9)+IntToStr(Integer(ItemsA[i].LocalClient))+#4;
        s:=s+#3+Chr(64+10)+ItemsA[i].DBAlias+#4;
        s:=s+#3+Chr(64+11)+ItemsA[i].DBPath+#4;
        s:=s+#3+Chr(64+12)+ItemsA[i].TableName+#4;
        s:=s+#3+Chr(64+13)+IntToStr(ItemsA[i].ConnectionID)+#4;
        s:=s+#5;
      end;
  end else begin
    s:=s+#2;
    s:=s+#3+Chr(64+1)+'Sysdba'+#4;
    s:=s+#3+Chr(64+2)+'systemroot'+#4;
    s:=s+#3+Chr(64+3)+IntToStr(Integer(derAll))+#4;
    s:=s+#3+Chr(64+4)+IntToStr(Integer(desOffLine))+#4;
    s:=s+#3+Chr(64+5)+'All'+#4;
    s:=s+#3+Chr(64+6)+IntToStr(0)+#4;
    s:=s+#3+Chr(64+7)+IntToStr(0)+#4;
    s:=s+#3+Chr(64+8)+IntToStr(0)+#4;
    s:=s+#3+Chr(64+9)+IntToStr(Integer(False))+#4;
    s:=s+#3+Chr(64+10)+DBAlias+#4;
    s:=s+#3+Chr(64+11)+DirPath+#4;
    s:=s+#3+Chr(64+12)+EmptyStr+#4;
    s:=s+#3+Chr(64+13)+'-1'+#4;
    s:=s+#5;
  end;
  s:=s+#6;
  FilePath:=DirPath+cFileUserPrevilegies;
  ss:=TStringStream.Create(s);
  fs:=TFileStream.Create(FilePath,fmCreate);
  ss.Seek(0,0);
  fs.CopyFrom(ss,ss.Size);
  ss.Free;
  fs.Free;
end;


function TFileUserList.GetClientRules(ClientID: integer): TDERules;
var
  i:integer;
begin
  Result:=derNone;
  for i:=0 to Count-1 do
    if Items[i].ID=ClientID then begin
      Result:=Items[i].Rules;
      Break;
    end;
end;


procedure TFileUserList.SendReport(Sender: TObject; FDB_Answer: TFDB_Answer; ClientID:integer);
var
  sh,Ind:integer;
begin
  Ind:=GetClientNum(ClientID);
  if Ind>=0 then begin
    sh:=Items[Ind].SockHandle;
    SendReportSHandle(Sender, FDB_Answer,Items[Ind].LocalClient,ClientID,sh);
  end;
end;

procedure TFileUserList.SendReportSHandle(Sender:TObject; FDB_Answer:TFDB_Answer; LocalConnect:boolean; ClientID:integer; SHandle:integer);
begin
  if LocalConnect then begin
      if Assigned(FOnWork) then FOnWork(Self,FDB_Answer);
  end else begin
    // Тот же ответ но по сети, через SocketHandle
  end;
end;


procedure TFileUserList.DisconnectClient(ClientID, ConnectionID: integer; LocalConnect:boolean);
var
  Ind:integer;
  FDB_Answer:TFDB_Answer;
  Ur:TUserRec;
  sh:integer;
begin
  Ind:=GetClientNum(ClientID);
  sh:=-1;
  ur:=Items[Ind];
  if ur.LocalClient then begin
    if Ind<0 then begin
      FDB_Answer.MsgCode:=FDB_DISCONNECT_CLIENT_NOT_IN_LIST;
      FDB_Answer.MsgStr:=sFDB_DISCONNECT_CLIENT_NOT_IN_LIST;
      FDB_Answer.FDB_ID:=ClientID;
      FDB_Answer.Rules:=derNone;
    end else begin
      FDB_Answer.MsgCode:=FDB_DISCONNECT_ON_REQUEST;
      FDB_Answer.MsgStr:=sFDB_DISCONNECT_ON_REQUEST;
      FDB_Answer.FDB_ID:=ClientID;
      FDB_Answer.ConnectionID:=ConnectionID;
      FDB_Answer.Rules:=ur.Rules;
      sh:=ur.SockHandle;
      Delete(Ind);
      if Assigned(OnClientListChange) then FOnClientListChange(Self,ur, True);
    end;
    SendReportSHandle(Self,FDB_Answer,LocalConnect,-1,sh);
  end else begin
//    Socket.Close;
  end;
end;

function TFileUserList.GetClientNum(ClientID: integer): integer;
var
  i:integer;
begin
  Result:=-1;
  for i:=0 to Count-1 do
    if Items[i].ID=ClientID then begin
      Result:=i;
      Break;
    end;
end;


procedure TFileList.WriteToFileUpdate(fl: TFile);
var
  fs:TFileStream;
  ss:TStringStream;
  FileNameLength : SmallInt;
  FileDataLength : integer;
begin
  if FileExists(fl.RealFullPath) then begin
    fs:=TFileStream.Create(fl.RealFullPath,fmCreate);
    FileNameLength := PSmallint(@(fl.Body[DeSLength + 1]))^;
    FileDataLength := Length(fl.Body) - (DeSLength + SizeOf(Smallint) + FileNameLength);
    ss:=TStringStream.Create(copy(fl.Body,DeSLength + SizeOf(Smallint) + FileNameLength +1, FileDataLength));
    ss.Seek(0,0);
    fs.CopyFrom(ss,ss.Size);
    fs.Free;
    ss.Free;
  end;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('UFileDB unit initialization ...');

finalization
  DebugLog('UFileDB unit finalization ...');
{$ENDIF}

end.

