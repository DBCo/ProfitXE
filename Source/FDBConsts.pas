unit FDBConsts;

interface
const

  cr=#13#10;
  cBeginFile=#1;//
  cBeginRecord=#2;//
  cBeginOnceData=#3;//
  cEndOnceData=#4;//
  cEndRecord=#5;//
  cEndFile=#6;//
  cFileDBList='dblist.dat';
  cFileUserPrevilegies='users.dat';
  cPrfxAliasInFile='->';
  cFileInfoExt='.fdb_file_info';
  cFileDataExt='.fdb_file_data';
  cDefaultUserName='sysdba';
  cDefaultUserPassword='systemroot';
  cCountFilesSymbol=133;


  {}
  FDB_CONNECT_FIRST=1;

  {}
  FDB_CONNECT_SUCCESSFULL=1;
  sFDB_CONNECT_SUCCESSFULL='Connect successfull';

  {}
  FDB_CONNECT_WRONE_LOGIN_PASS_DB=2;
  sFDB_CONNECT_WRONE_LOGIN_PASS_DB='Connect failure, DB, login or password is wrone';

  {}
  FDB_CONNECT_WRONE_IP=3;
  sFDB_CONNECT_WRONE_IP='Connect failure, IP for this login and password is wrone';

  {}
  FDB_CONNECT_LAST=100;

  {}
  FDB_ERROR_FIRST=101;

  {}
  FDB_ERROR_NOT_PREMISION=101;
  sFDB_ERROR_NOT_PREMISION='You are have not premision for this action';

  {}
  FDB_ERROR_FOLDER_NOT_EXISTS=102;
  sFDB_ERROR_FOLDER_NOT_EXISTS='Folder not exists';

  {}
  FDB_ERROR_FOLDER_EXISTS=103;
  sFDB_ERROR_FOLDER_EXISTS='Folder exists';

  {}
  FDB_ERROR_FILE_NOT_EXISTS=104;
  sFDB_ERROR_FILE_NOT_EXISTS='File not exists';

  {}
  FDB_ERROR_FILE_EXISTS=105;
  sFDB_ERROR_FILE_EXISTS='Filr exists';

  {}
  FDB_ERROR_CREATE_FILE=106;
  sFDB_ERROR_CREATE_FILE='Error create file';

  {}
  FDB_ERROR_FILE_NOT_IN_LIST=107;
  sFDB_ERROR_FILE_NOT_IN_LIST='File not in list';

  {}
  FDB_ERROR_FILE_IN_LIST=108;
  sFDB_ERROR_FILE_IN_LIST='File in list';

  {}
  FDB_ERROR_UNKNOWN_COMMAND=109;
  sFDB_ERROR_UNKNOWN_COMMAND='Unknown command';

  {}
  FDB_ERROR_TABLE_NOT_SELECT=110;
  sFDB_ERROR_TABLE_NOT_SELECT='Table not select';

  {}
  FDB_ERROR_TABLE_NOT_EXISTS=111;
  sFDB_ERROR_TABLE_NOT_EXISTS='Table not exists';

  {}
  FDB_ERROR_LAST=200;

  {}
  FDB_DATA_FIRST=201;

  {}
  FDB_DATA_READY=201;
  sFDB_DATA_READY='Data is ready';

  {}
  FDB_DATA_EMPTY=202;
  sFDB_DATA_EMPTY='Data is empty';

  {}
  FDB_DATA_START=203;
  sFDB_DATA_START='Data start';

  {}
  FDB_DATA_FILE_INSERT_DONE=204;
  sFDB_DATA_FILE_INSERT_DONE='File insert successfull';

  {}
  FDB_DATA_FILE_CREATE_DONE=205;
  sFDB_DATA_FILE_CREATE_DONE='File create successfull';

  {}
  FDB_DATA_FILE_DELETE_DONE=206;
  sFDB_DATA_FILE_DELETE_DONE='File delete successfull';

  {}
  FDB_DATA_FILE_ADD_DONE=207;
  sFDB_DATA_FILE_ADD_DONE='File add successfull';

  {}
  FDB_DATA_FILE_UPDATE_DONE=208;
  sFDB_DATA_FILE_UPDATE_DONE='File update successfull';

  {}
  FDB_DATA_RECORD_READY=209;
  sFDB_DATA_RECORD_READY='Once record ready';

  {}
  FDB_DATA_FILE_COUNT_READY=210;
  sFDB_DATA_FILE_COUNT_READY='File count ready';

  {}
  FDB_DATA_TABLE_LIST_READY=211;
  sFDB_DATA_TABLE_LIST_READY='Table list ready';

  {}
  FDB_DATA_TABLE_LIST_EMPTY=212;
  sFDB_DATA_TABLE_LIST_EMPTY='Table list empty';

  {}
  FDB_DATA_TABLE_SET_OK=213;
  sFDB_DATA_TABLE_SET_OK='Table set successfull';

  {}
  FDB_DATA_LAST=300;

  {}
  FDB_DISCONNECT_FIRST=301;

  {}
  FDB_DISCONNECT_ON_REQUEST=301;
  sFDB_DISCONNECT_ON_REQUEST='Disconnect on request';

  {}
  FDB_DISCONNECT_CLIENT_NOT_IN_LIST=302;
  sFDB_DISCONNECT_CLIENT_NOT_IN_LIST='Disconect failure? client not in list';

  {}
  FDB_DISCONNECT_LAST=400;

type
  TDENotAccept=procedure (Sender:TObject; ReasonCode:integer; MsgStr:string) of object;
  TDEError=procedure (Sender:TObject; ErrorCode:integer; MsgStr:string) of object;

  TDEFileField=record
    AsString:string;
    AsInteger:integer;
    AsDateTime:TDateTime;
    AsVariant:Variant;
  end;

  TDEFileType=(deftText, deftExe, deftBitmap, deftDocument, deftJpeg, deftPas, deftDll,
               deftXML, deftUnknown);

  PFile=^TFile;
  TFile=record   // !!!!!!!!!!!!!!! При изменении полей !Обязательно!
    ID:integer;
    Name:string; // !!!!!!!!!!!!!!!  Подкорректировать функцию
    Body:string; // !!!!! function TDEFileDataSet.FieldByName(const FieldName: string): TDEFileField;
    FileType:TDEFileType; //!!!!!!!!!!!! Fields[Index:integer]
    Size:integer;
    DateTime:TDateTime;
    FileAttr:integer;
    FullPath:string;
    RealFileName:string;
    iParentFolder:integer;
    iParentSubFolder:integer;
    RealFullPath:string;
  end;

  TDERules=(derIgnore, derRW, derR, derW, derAll, derNone);
  TDEState=(desOnLine, desOffLine, desNone);

  TFDBCommand=(fdbc_SELECT, fdbc_INSERT, fdbc_UPDATE, fdbc_DELETE,
               fdbc_GET_LIST_DB, fdbc_CREATE_DB, fdbc_DELETE_DB, fdbc_UPDATE_DB,
               fdbc_GET_LIST_TBL, fdbc_CREATE_TBL, fdbc_DELETE_TBL, fdbc_UPDATE_TBL,
               fdbc_GET_LIST_USR, fdbc_CREATE_USR, fdbc_DELETE_USR, fdbc_UPDATE_USR,
               fdbc_TRY_CONNECT, fdbc_DISCONNECT, fdbc_SELECT_CURRENT_TABLE,
               fdbc_GET_FILES_COUNT, fdbc_GET_TABLES_LIST);

  TFDB_Answer=record
    MsgCode:integer;
    MsgStr:string;
    FDB_ID:integer;
    ConnectionID:integer;
    Rules:TDERules;
    CanOverload:boolean;
    Data:string;
  end;

  TDEDisconnect=procedure (Sender:TObject; NotFound:boolean; FDB_Answer:TFDB_Answer) of object;


  TCommandToFDB=record
    FDB_ID:integer;
    ConnectionID:integer;
    iCommand:TFDBCommand;
    Params:string;
    FDB_Data:string;
  end;

  TOnFDBAnswer= procedure (Sender:TObject; FDB_Answer:TFDB_Answer; LocalConnect:boolean) of object;
  TOnFDBAnswerLocal= procedure (Sender:TObject; FDB_Answer:TFDB_Answer) of object;

  PUserRec=^TUserRec;
  TUserRec=record
    Name:string;            //NumPole - 1
    Password:string;        //NumPole - 2
    Rules:TDERules;         //NumPole - 3
    State:TDEState;         //NumPole - 4
    IP:string;              //NumPole - 5
    Port:integer;           //NumPole - 6
    ID:integer;             //NumPole - 7
    SockHandle:integer;     //NumPole - 8
    LocalClient:boolean;    //NumPole - 9
    DBAlias:string;         //NumPole - 10
    DBPath:string;          //NumPole - 11
    TableName:string;       //NumPole - 12
    ConnectionID:integer;   //NumPole - 13
  end;

  TOnClientListChange= procedure (Sender:TObject; NewUser:TUserRec; bNew:boolean) of object;
  

implementation

end.
