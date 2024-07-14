unit DeTextReport;

interface

uses Windows, SysUtils, Classes, {Messages, }
     DeXMLReport, DeTemplate{, DeReport};

type
  TDeTextBasedReport = class(TDeXMLReport)
  private
  protected
    procedure doPreview; override;
    procedure doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); override;
  public
    constructor Create(const aFileName: string);
    class function IsSupportExtension(const Extension: string): Boolean; override;
  end;

  TDeSimpleReport = class(TDeTextBasedReport)
  private
  protected
    procedure ProcessTemplate; override;
    procedure doPreview; override;
    procedure doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); override;
  public
  end;

  TDeTextReport = class(TDeTextBasedReport)
  private
    FStream: TMemoryStream;
    function ToOutput(const s: string; const ReplaceDoubleTagSymbol: Boolean = False): Boolean;
  protected
    procedure doPreview; override;
    procedure doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); override;
    procedure ProcessTemplate; override;
    function ProcessMakeTag(NodeValue: TDeNode): Boolean; override;
    function ProcessUnknownTag(NodeValue: TDeNode; const aBody: Boolean): Boolean; override;
    // v 17.4
    procedure ChangeTargetFileName(const OldFileName, NewFileName: string); override;
  public
    constructor Create(const aFileName: string);
    destructor Destroy; override;
    class function IsSupportExtension(const Extension: string): Boolean; override;
  end;

implementation

uses Variants, ShellAPI,
     DeLog, DeTypes, Funcs, DataUnit;

{ TDeTextBasedReport }

constructor TDeTextBasedReport.Create(const aFileName: string);
begin
  inherited Create;
  FileName := aFileName;
  DM.RegisterTemporaryFile(aFileName);
end;

procedure TDeTextBasedReport.doPreview;
begin
  doPrint(EmptyStr, 1, True);
end;

procedure TDeTextBasedReport.doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean);
var i         : Integer;
    OldActive : String;
begin
  if Length(PrinterName) = 0 then
    begin
      ShellExecute(0, nil, PChar(TargetFileName), nil, nil, SW_SHOWNORMAL);
      Exit;
    end;
  if Pos(':\', PrinterName) = 2 then
    begin
      ForceDirectories(ExtractFilePath(PrinterName));
      CopyFile(PChar(TargetFileName), PChar(PrinterName), True);
      Exit;
    end;

  OldActive := GetDefaultPrinter;

  if Pos(PrinterName,OldActive)=0 then
    SetDefaultPrinterByName(PrinterName);

  sleep(100);

  for i:=1 to CopyCount do
    ShellExecute(0, 'print', PChar(TargetFileName), '', '', SW_HIDE);

  sleep(100);

  if Pos(PrinterName,OldActive)=0 then
    SetDefaultPrinter(OldActive);
end;

class function TDeTextBasedReport.IsSupportExtension(const Extension: string): Boolean;
begin
  // Базовый класс текстовых отчётов не поддерживает никаких расширений файлов!
  Result := False;
end;

{ TDeSimpleReport }

procedure TDeSimpleReport.ProcessTemplate;
var  F : TextFile;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  Write(F, DeTemplate.Text);
  CloseFile(F);
end;

procedure TDeSimpleReport.doPreview;
begin
  ProcessTemplate;
  inherited doPreview;
end;

procedure TDeSimpleReport.doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean);
begin
  ProcessTemplate;
  inherited doPrint(PrinterName, CopyCount, SplitCopies);
end;

{ TDeTextReport }

constructor TDeTextReport.Create(const aFileName: string);
begin
  inherited Create(aFileName);
  FStream := TMemoryStream.Create;
end;

destructor TDeTextReport.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TDeTextReport.ChangeTargetFileName(const OldFileName, NewFileName: string);
begin
  if OldFileName <> NewFileName then
    begin
      if FStream.Size <> 0 then
        begin
          ForceDirectories(ExtractFilePath(OldFileName));
          FStream.SaveToFile(OldFileName);
          FStream.Size := 0;
        end;
    end;
  inherited ChangeTargetFileName(OldFileName, NewFileName);
end;

procedure TDeTextReport.ProcessTemplate;
begin
  Inherited ProcessTemplate;
  FStream.Position := 0;
  //FStream.SaveToFile(FileName);
  ForceDirectories(ExtractFilePath(TargetFileName));
  FStream.SaveToFile(TargetFileName);
end;

procedure TDeTextReport.doPreview;
begin
  ProcessTemplate;
  inherited doPreview;
end;

procedure TDeTextReport.doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean);
begin
  ProcessTemplate;
  inherited doPrint(PrinterName, CopyCount, SplitCopies);
end;

class function TDeTextReport.IsSupportExtension(const Extension: string): Boolean;
begin
  Result := IsFileExtension(Extension, [sExtensionTXT, sExtensionXML, sExtensionHTM, sExtensionHTML, sExtensionJSON, sExtensionCSV]);
end;

function TDeTextReport.ToOutput(const s: string; const ReplaceDoubleTagSymbol: Boolean): Boolean;
var t: AnsiString;
//    u: Word;
begin
  Result:=True;

  if 0<Length(s) then
    begin
      if ReplaceDoubleTagSymbol then
        t:=StringReplace(
           StringReplace( s, '<<', '<', [rfReplaceAll]),
                             '>>', '>', [rfReplaceAll]    )
      else
        t:=s;

      case CodePage of
        866: { DOS }
          AnsiToOem( PAnsiChar(t), PAnsiChar(t));
        0: { UTF-8 }
          begin
            t := AnsiToUtf8(t);
            {
            if FStream.Size = 0 then
              begin
                u := $BBEF;
                FStream.WriteBuffer(u, SizeOf(u));
              end;
            }
          end;
      end;

      FStream.Write( t[1], Length(t));
    end;
end;

function TDeTextReport.ProcessMakeTag(NodeValue: TDeNode): Boolean;
begin
  Result := ToOutput(NodeValue.Body, True);
end;

function TDeTextReport.ProcessUnknownTag(NodeValue: TDeNode; const aBody: Boolean): Boolean;
var V : Variant;
begin
  if NodeValue.TagType = ttFile then
    Result := False
  else
    begin
      Result := True; // Без этого присвоения возвращался случайный результат (из стека) при обработке не BODY тега!!!!
      if aBody then
        try
          if ProcessTextTag(NodeValue, V) then
            ToOutput(VarToStr(V))
          else
            ToOutput(NodeValue.Text, True);
        except
          on E: Exception do
            begin
              {$IFDEF DeDEBUG}
              if Length(NodeValue.NodeName) = 0 then
                Funcs.WriteLog('Process unknown tag error: %s', [E.Message])
              else
                Funcs.WriteLog('Process unknown tag %s error: %s', [QuotedStr(NodeValue.NodeName), E.Message]);
              {$ENDIF}
              Result := False;
            end;
        end;
    end;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DeTextReport unit initialization ...');

finalization
  DebugLog('DeTextReport unit finalization ...');
{$ENDIF}

end.

