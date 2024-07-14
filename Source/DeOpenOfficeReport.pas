unit DeOpenOfficeReport;

interface

uses Classes,
     DeXMLReport, DeTemplate;

type
  TOpenOfficeReport = class(TDeXMLReport)
  private
  protected
    FManager: OleVariant;
    FDesktop: OleVariant;
    procedure LoadParamDefs; override;
    procedure doOpenTemplate; virtual; abstract;
    procedure doLoadTemplate; virtual; abstract;
    procedure doOpenDoc; virtual; abstract;
    procedure doShowDoc; virtual; abstract;
    procedure doPrintDoc(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); virtual; abstract;
    procedure doClose; virtual; abstract;
    procedure doPreview; override;
    procedure Quit; virtual; abstract;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    class function IsSupportExtension(const Extension: string): Boolean; override;
  end;

  TOpenOfficeWriterReport = class(TOpenOfficeReport)
  private
  protected
//    procedure ClearRanges;
//    procedure doOpenTemplate; override;
//    procedure doLoadTemplate; override;
//    procedure doOpenDoc; override;
//    procedure doShowDoc; override;
//    procedure doPrintDoc(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); override;
//    procedure doClose; override;
//    procedure ProcessTemplate; override;
//    function ProcessMakeTag(NodeValue: TDeNode): Boolean; override;
//    procedure Quit; override;
//    procedure doSaveToFile(const aFileName: string); override;
//    procedure doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); override;
    // v 17.4
//    procedure ChangeTargetFileName(const OldFileName, NewFileName: string); override;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    class function IsSupportExtension(const Extension: string): Boolean; override;
  end;


function GetOpenOfficeWriterVersion: Integer;
function GetOpenOfficeCalcVersion: Integer;

implementation

uses Windows, SysUtils, Registry, Variants, ComObj, Funcs, DeTypes, DeLog, DataUnit;


function GetOpenOfficeProductVersion(const ProductName: string): Integer;
var
  Registry: TRegistry;
  FileName: string;
  VersionMS, VersionLS: Cardinal;
  Index: Integer;
begin
  Result := 0;
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CLASSES_ROOT;
    if Registry.OpenKeyReadOnly('opendocument.' + ProductName + 'Document\CurVer') then
      begin
        FileName := Registry.ReadString(EmptyStr);
        Registry.CloseKey;
        if Length(FileName) <> 0 then
          if Registry.OpenKeyReadOnly(FileName + '\CLSID') then
            begin
              FileName := Registry.ReadString(EmptyStr);
              Registry.CloseKey;
              if Registry.OpenKeyReadOnly('\CLSID\' + FileName + '\LocalServer32') then
                begin
                  FileName := Registry.ReadString(EmptyStr);
                  Registry.CloseKey;
                end
              else
                FileName := EmptyStr;
            end
          else
            FileName := EmptyStr;
      end
    else
      FileName := EmptyStr;
  finally
    Registry.Free;
  end;
  if Length(FileName) <> 0 then
    begin
      // Пропускаем параметры запуска файла ...
      while (Length(FileName) <> 0) and not FileExists(FileName) do
        begin
          Index := LastDelimiter(' ', FileName);
          if Index = 0 then
            Break
          else
            FileName := Trim(Copy(FileName, 1, Pred(Index)));
        end;
      // Пытаемся получить версию файла ...
      if GetFileVersion(FileName, VersionMS, VersionLS) then
        Result := VersionMS;
    end;
end;

function GetOpenOfficeWriterVersion: Integer;
begin
  Result := GetOpenOfficeProductVersion('Writer');
end;

function GetOpenOfficeCalcVersion: Integer;
begin
  Result := GetOpenOfficeProductVersion('Calc');
end;

{ TOpenOfficeReport }

constructor TOpenOfficeReport.Create(const AFileName: string);
begin
  inherited Create;
  FileName := AFileName;
  DM.RegisterTemporaryFile(AFileName);
  FManager := CreateOleObject('com.sun.star.ServiceManager');
  if VarType(FManager) <> varDispatch then
    raise Exception.Create('OpenOffice not installed!!!');
  FDesktop := FManager.CreateInstance('com.sun.star.frame.Desktop');
end;

destructor TOpenOfficeReport.Destroy;
begin
  FDesktop := Unassigned;
  FManager := Unassigned;
  inherited Destroy;
end;

class function TOpenOfficeReport.IsSupportExtension(const Extension: string): Boolean;
begin
  // Базовый класс офисных отчётов не поддерживает никаких расширений файлов!
  Result := False;
end;

procedure TOpenOfficeReport.LoadParamDefs;
begin
  if DeTemplate.Root.Count = 0 then
    begin
      doOpenTemplate;
      doLoadTemplate;
      doClose;
    end;
  if DeTemplate.Root.Count > 0 then
    inherited LoadParamDefs;
end;

procedure TOpenOfficeReport.doPreview;
begin
  doPrint(EmptyStr, 1, True);
end;

{ TOpenOfficeWriterReport }

constructor TOpenOfficeWriterReport.Create(const AFileName: string);
begin
  inherited Create(AFileName);

end;

destructor TOpenOfficeWriterReport.Destroy;
begin

  inherited Destroy;
end;

class function TOpenOfficeWriterReport.IsSupportExtension(const Extension: string): Boolean;
begin
  Result := IsFileExtension(Extension, [sExtensionODT{, sExtensionRTF, sExtensionDOC, sExtensionDOCX}]);
end;

end.

