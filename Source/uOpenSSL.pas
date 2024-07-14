unit uOpenSSL;

interface

uses Windows, SysUtils, Contnrs;

type
  TLibOpenSSL = class
  private
    { Private declarations }
    FFileName: string;
    FLoaded: Boolean;
    FInstalled: Boolean;
    function GetProductVersion: string;
    procedure Refresh;
  public
    { Public declarations }
    constructor Create(const AFileName: string);
    property FileName: string read FFileName;
    property Installed: Boolean read FInstalled;
    property Loaded: Boolean read FLoaded;
    property ProductVersion: string read GetProductVersion;
  end;

  TOpenSSL = class
  private
    { Private declarations }
    FList: TObjectList;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TLibOpenSSL;
    function GetInstalled: Boolean;
    function GetLoaded: Boolean;
    function GetProductVersion: string;
    procedure Append(const FileName: string);
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    function Install: Boolean;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TLibOpenSSL read GetItem; default;
    property Installed: Boolean read GetInstalled;
    property Loaded: Boolean read GetLoaded;
    property ProductVersion: string read GetProductVersion;
  end;

const
  LibSSL1 = 'libeay32.dll'; // Должна быть первой!!!
  LibSSL2 = 'ssleay32.dll'; // Должна быть второй!!!

implementation

uses Classes, StrUtils, ZIP;

{$IFDEF OPENSSL}
  {$IFDEF WIN32}
  {$R SSL32.RES}
  {$ENDIF}

  {$IFDEF WIN64}
  {$R SSL64.RES}
  {$ENDIF}
{$ENDIF}

{ TLibOpenSSL }

constructor TLibOpenSSL.Create(const AFileName: string);
begin
  FFileName := AFileName;
  Refresh;
end;

function TLibOpenSSL.GetProductVersion: string;
var
  BufferSize, Size: Cardinal;
  Buffer: Pointer;
  FileInfo: PVSFixedFileInfo;
  Translation: PDWORD;
  Section: string;
  Value: PChar;
begin
  Result := EmptyStr;
  BufferSize := GetFileVersionInfoSize(PChar(FFileName), Size);
  if BufferSize <> 0 then
    begin
      GetMem(Buffer, BufferSize);
      try
        if GetFileVersionInfo(PChar(FFileName), Size, BufferSize, Buffer) then
          begin
            if VerQueryValue(Buffer, '\VarFileInfo\Translation', Pointer(Translation), Size) then
              begin
                Section := '\StringFileInfo\' + IntToHex(LoWord(Translation^), 4) + IntToHex(HiWord(Translation^), 4) + '\ProductVersion';
                if VerQueryValue(Buffer, PChar(Section), Pointer(Value), Size) then
                  Result := StrPas(Value);
              end;
            if Length(Result) = 0 then
              if VerQueryValue(Buffer, '\', Pointer(FileInfo), BufferSize) then
                Result := Format('%u.%u.%u.%u', [
                  HiWord(FileInfo^.dwProductVersionMS), 
                  LoWord(FileInfo^.dwProductVersionMS),
                  HiWord(FileInfo^.dwProductVersionLS),
                  LoWord(FileInfo^.dwProductVersionLS)
                  ]);
          end;
      finally
        FreeMem(Buffer);
      end;
    end;
end;

procedure TLibOpenSSL.Refresh;
var
  Handle: THandle;
  Directory: string;
begin
  FInstalled := False;
  Handle := LoadLibrary(PChar(FFileName));
  FLoaded := Handle <> 0;
  if FLoaded then
    try
      Directory := ExtractFilePath(GetModuleName(hInstance));
      if Length(Directory) <> 0 then
        Directory := IncludeTrailingBackslash(Directory);
      FInstalled := FileExists(Directory + ExtractFileName(FFileName));
    finally
      FreeLibrary(Handle);
    end;
end;

{ TOpenSSL }

constructor TOpenSSL.Create;
begin
  FList := TObjectList.Create;
  // Если здесь поменять местами - метод Install работать не будет,
  // т.к. вторая импортирует процедуры из первой и она тупо не загрузиться без первой!!! 
  Append(LibSSL1);
  Append(LibSSL2);
end;

destructor TOpenSSL.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TOpenSSL.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TOpenSSL.GetItem(const Index: Integer): TLibOpenSSL;
begin
  Result := FList[Index] as TLibOpenSSL;
end;

function TOpenSSL.GetInstalled: Boolean;
var
  Index: Integer;
begin
  Result := Count <> 0;
  if Result then
    for Index := 0 to Pred(Count) do
      if not Items[Index].Installed then
        begin
          Result := False;
          Break;
        end;
end;

function TOpenSSL.GetLoaded: Boolean;
var
  Index: Integer;
begin
  Result := Count <> 0;
  if Result then
    for Index := 0 to Pred(Count) do
      if not Items[Index].Loaded then
        begin
          Result := False;
          Break;
        end;
end;

function TOpenSSL.GetProductVersion: string;
var
  Strings: TStrings;
  Name, Value: string;
  Index: Integer;
begin
  Result := EmptyStr;
  if Count <> 0 then
    begin
      Strings := TStringList.Create;
      try
        for Index := 0 to Pred(Count) do
          begin
            Name := Items[Index].ProductVersion;
            Value := Strings.Values[Name];
            if Length(Value) <> 0 then
              Value := Value + ', ';
            Strings.Values[Name] := Value + ChangeFileExt(ExtractFileName(Items[Index].FileName), EmptyStr);
          end;
        if Strings.Count = 1 then
          Result := Strings.Names[0]
        else
          for Index := 0 to Pred(Strings.Count) do
            begin
              if Length(Result) <> 0 then Result := Result + '; ';
              Name := Strings.Names[Index];
              if Length(Name) = 0 then Name := '???';
              Result := Result + Name + ' [' + Strings.ValueFromIndex[Index] + ']';
            end;
      finally
        Strings.Free;
      end;
    end;
end;

procedure TOpenSSL.Append(const FileName: string);
var
  LibOpenSSL: TLibOpenSSL;
begin
  LibOpenSSL := TLibOpenSSL.Create(FileName);
  if FList.Add(LibOpenSSL) = -1 then LibOpenSSL.Free;
end;

function TOpenSSL.Install: Boolean;
const
  {$IFDEF WIN32}
  ResourceName = 'SSL32';
  {$ENDIF}
  {$IFDEF WIN64}
  ResourceName = 'SSL64';
  {$ENDIF}
var
  Directory, FileName, BackupFileName: string;
  Stream: TStream;
  ZipFile: TZipFile;
  Index, FileIndex: Integer;
begin
  Result := FindResource(hInstance, ResourceName, RT_RCDATA) <> 0;
  if Result then
    begin
      Directory := ExtractFilePath(GetModuleName(hInstance));
      if Length(Directory) <> 0 then
        Directory := IncludeTrailingBackslash(Directory);
      Stream := TResourceStream.Create(hInstance, ResourceName, RT_RCDATA);
      try
        ZipFile := TZipFile.Create;
        try
          ZipFile.Open(Stream, zmRead);
          for Index := 0 to Pred(Count) do
            begin
              FileName := ExtractFileName(Items[Index].FileName);
              FileIndex := ZipFile.IndexOf(FileName);
              if FileIndex = -1 then
                begin
                  Result := False;
                  Break;
                end
              else
                begin
                  FileName := Directory + FileName;
                  if FileExists(FileName) then
                    begin
                      BackupFileName := ChangeFileExt(FileName, '.~' + ReplaceText(ExtractFileExt(FileName), '.', EmptyStr));
                      if FileExists(BackupFileName) then
                        begin
                          if not DeleteFile(BackupFileName) then
                            begin
                              Result := False;
                              Break;
                            end;
                        end;
                      if not RenameFile(FileName, BackupFileName) then
                        begin
                          Result := False;
                          Break;
                        end;
                      ZipFile.Extract(FileIndex, Directory);
                      Items[Index].Refresh;
                      if not Items[Index].Loaded then
                        begin
                          if FileExists(BackupFileName) then
                            RenameFile(BackupFileName, FileName);
                          Result := False;
                          Break;
                        end;
                    end
                  else
                    begin
                      ZipFile.Extract(FileIndex, Directory);
                      Items[Index].Refresh;
                      if not Items[Index].Loaded then
                        begin
                          DeleteFile(FileName);
                          Result := False;
                          Break;
                        end;
                    end;
                end;
            end;
        finally
          ZipFile.Free;
        end;
      finally
        Stream.Free;
      end;
    end;
end;

end.

