unit DeXLReport;

interface

uses
  Windows, SysUtils, Messages, Classes, Clipbrd, Generics.Collections,
  {ClipBrd, }{OleServer, }{Graphics, }{Contnrs, }{XMLDoc, }{XMLIntf, }
  {Meta, }{DSMeta, }{DeDataset, }{DeDB, }{DataCacheUnit, }{DeReport, }DeXMLReport, DeTemplate;

const
   debugShowWindow = False;
   wdReplaceOne = 1;
   wdReplaceAll = 2;
   wdFindContinue = 1;

   wdCollapseEnd = $00000000;

type
  TDeMSOfficeProc = procedure(S: string) of object;

type
  TDeMSOfficeReport = class(TDeXMLReport)
  private
    FApp: OleVariant;
  protected
    procedure LoadParamDefs; override;
    procedure doOpenTemplate; virtual; abstract;
    procedure doLoadTemplate; virtual; abstract;
    procedure doOpenDoc; virtual; abstract;
    procedure doShowDoc; virtual; abstract;
    procedure doPrintDoc(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); virtual; abstract;
    procedure doClose; virtual;
    procedure doPreview; override;
    procedure doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); override;
    procedure doPrintToFile(const aFileName: string); override;
    procedure Quit; virtual; abstract;
  public
    constructor Create(const aFileName: string);
    //destructor Destroy; override;
    class function IsSupportExtension(const Extension: string): Boolean; override;
  end;

type
  TDeExcelReport = class(TDeMSOfficeReport)
  private
    FBook: OleVariant;
    FSheets: TStringList;
    procedure ClearSheets;
    procedure AddSheetInfo(const rName: string; const aLastTemplateRow: Integer);
    function GetSheetInfo(const rName: string): TObject;
  protected
    procedure doOpenTemplate; override;
    procedure doLoadTemplate; override;
    procedure doOpenDoc; override;
    procedure doShowDoc; override;
    procedure doPrintDoc(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); override;
    procedure ProcessTemplate; override;
    function ProcessMakeTag(NodeValue: TDeNode): Boolean; override;
    procedure Quit; override;
    procedure doSaveToFile(const aFileName: string); override;
  public
    constructor Create(const aFileName: string);
    destructor Destroy; override;
    class function IsSupportExtension(const Extension: string): Boolean; override;
  end;

type
  TDeWordReport = class(TDeMSOfficeReport)
  private
    FResultDoc    : OLEVariant;
    FExampleDoc   : OLEVariant;
    FResultRange  : OLEVariant;
    FExampleRange : OLEVariant;
    FRanges     : array of OleVariant;
    FRangesText : array of string;
  protected
    procedure ClearRanges;
    procedure DoOpenTemplate; override;
    procedure doLoadTemplate; override;
    procedure doOpenDoc; override;
    procedure DoShowDoc; override;
    procedure DoPrintDoc(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); override;
    procedure DoClose; override;
    procedure ProcessTemplate; override;
    function ProcessMakeTag(NodeValue:TDeNode):boolean; override;
    procedure Quit; override;
    procedure doSaveToFile(const aFileName: string); override;
  public
    constructor Create(const aFileName: string);
    destructor Destroy; override;
    class function IsSupportExtension(const Extension: string): Boolean; override;
  end;

implementation

uses Variants, ComObj, DB, Forms,
     DeLog, DeTypes, Funcs, Dictionary{, DeMetaData{, DeParser}{, DeScript}, DataUnit;

//var
//  IntReportsCount : integer = 0;

const
  wdDialogFilePrint = $00000058;

{Процедуры сохранения и восстановления чужого буфера обмена в поток ===========}

procedure CopyStreamToClipboard(fmt: Cardinal; S: TStream);
 var
   hMem: THandle;
   pMem: Pointer;
 begin
   Assert(Assigned(S));
   S.Position := 0;
   hMem       := GlobalAlloc(GHND or GMEM_DDESHARE, S.Size);
   if hMem <> 0 then
   begin
     pMem := GlobalLock(hMem);
     if pMem <> nil then
     begin
       try
         S.Read(pMem^, S.Size);
         S.Position := 0;
       finally
         GlobalUnlock(hMem);
       end;
       Clipboard.Open;
       try
         Clipboard.SetAsHandle(fmt, hMem);
       finally
         Clipboard.Close;
       end;
     end
     else
     begin
       GlobalFree(hMem);
       OutOfMemoryError;
     end;
   end
   else
     OutOfMemoryError;
 end;

 procedure CopyStreamFromClipboard(fmt: Cardinal; S: TStream);
 var
   hMem: THandle;
   pMem: Pointer;
 begin
   Assert(Assigned(S));
   hMem := Clipboard.GetAsHandle(fmt);
   if hMem <> 0 then
   begin
     pMem := GlobalLock(hMem);
     if pMem <> nil then
     begin
       try
         S.Write(pMem^, GlobalSize(hMem));
         S.Position := 0;
       finally
         GlobalUnlock(hMem);
       end;
     end
     else
       raise Exception.Create('CopyStreamFromClipboard: could not lock global handle '+
         'obtained from clipboard!');
   end;
 end;

 procedure SaveClipboardFormat(fmt: Word; writer: TWriter);
 var
   fmtname: array[0..128] of Char;
   ms: TMemoryStream;
 begin
   Assert(Assigned(writer));
   if 0 = GetClipboardFormatName(fmt, fmtname, SizeOf(fmtname)) then
     fmtname[0] := #0;
   ms := TMemoryStream.Create;
   try
     CopyStreamFromClipboard(fmt, ms);
     if ms.Size > 0 then
     begin
       writer.WriteInteger(fmt);
       writer.WriteString(fmtname);
       writer.WriteInteger(ms.Size);
       writer.Write(ms.Memory^, ms.Size);
     end;
   finally
     ms.Free
   end;
 end;

 procedure LoadClipboardFormat(reader: TReader);
 var
   fmt: Integer;
   fmtname: string;
   Size: Integer;
   ms: TMemoryStream;
 begin
   Assert(Assigned(reader));
   fmt     := reader.ReadInteger;
   fmtname := reader.ReadString;
   Size    := reader.ReadInteger;
   ms      := TMemoryStream.Create;
   try
     ms.Size := Size;
     reader.Read(ms.memory^, Size);
     if Length(fmtname) > 0 then
       fmt := RegisterCLipboardFormat(PChar(fmtname));
     if fmt <> 0 then
       CopyStreamToClipboard(fmt, ms);
   finally
     ms.Free;
   end;
 end;

 procedure SaveClipboard(S: TStream);
 var
   writer: TWriter;
   i: Integer;
 begin
   Assert(Assigned(S));
   writer := TWriter.Create(S, 4096);
   try
     Clipboard.Open;
     try
       writer.WriteListBegin;
       for i := 0 to Clipboard.formatcount - 1 do
         // эти типы данных в буфере не получается копировать :(
         if Not (Clipboard.Formats[i] in [CF_BITMAP, CF_ENHMETAFILE]) then
           SaveClipboardFormat(Clipboard.Formats[i], writer);
       writer.WriteListEnd;
     finally
       Clipboard.Close;
     end;
   finally
     writer.Free
   end;
 end;

 procedure LoadClipboard(S: TStream);
 var
   reader: TReader;
 begin
   Assert(Assigned(S));
   S.Position:=0;
   reader := TReader.Create(S, 4096);
   try
     Clipboard.Open;
     try
       clipboard.Clear;
       reader.ReadListBegin;
       while not reader.EndOfList do
         LoadClipboardFormat(reader);
       reader.ReadListEnd;
     finally
       Clipboard.Close;
     end;
   finally
     reader.Free
   end;
 end;
{}
{TDeMSOfficeReport ============================================================}

constructor TDeMSOfficeReport.Create(const aFileName: string);
begin
  inherited Create;
  FileName := aFileName;
  DM.RegisterTemporaryFile(aFileName);
end;

//destructor  TDeMSOfficeReport.Destroy;
//begin
//  inherited Destroy;
//end;

procedure TDeMSOfficeReport.DoClose;
begin
  FApp := Unassigned;
end;

procedure TDeMSOfficeReport.LoadParamDefs;
var  ProgressItem : TStatusItem;
begin
  if DeTemplate.Root.Count=0 then
    begin
      doOpenTemplate;
      doLoadTemplate;

//    aApp.DisplayAlerts:=Alerts;
      FApp.Quit;
      ProgressItem:= DeStatus.StatusItemByID(StatusPrintID);
      if assigned(ProgressItem) then ProgressItem.Progress:=20;

      FApp := unAssigned;
    end;

  if DeTemplate.Root.Count>0 then
    Inherited LoadParamDefs;
end;

procedure TDeMSOfficeReport.doPreview;
begin
  doPrint(EmptyStr, 1, True);
end;

procedure TDeMSOfficeReport.doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean);
var Alerts  : boolean;
begin
  // открыть файл шаблона
  DoOpenTemplate;
  Alerts:=FApp.DisplayAlerts;
  if Alerts then FApp.DisplayAlerts:=False;

  if DeTemplate.Root.Count = 0 then
    begin
      doLoadTemplate;
      doOpenDoc;
    end;
  if DeTemplate.Root.Count = 0 then exit; //Error!

  try
    // "до": Сохраняем буфер обмена
    // 1. Это не мешает работе в программе
    // 2. Наличие остатков формирования отчета даёт запрос на закрытие Word'a
    //TODO 10: ЕСть проблемы надо поправить

    // обработать шаблон
    ProcessTemplate;
    FParams := nil;

    // распечатать на принтер или на экран или в файл
    if Length(PrinterName) = 0 then
      DoShowDoc
    else
      if Pos(':\', PrinterName) = 2 then
        DoSaveToFile(PrinterName)
      else
        DoPrintDoc(PrinterName,CopyCount,SplitCopies);
    FApp.DisplayAlerts:=Alerts;
    if PrinterName <> EmptyStr then Quit;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TDeMSOfficeReport.doPrint(%s, %d, %s) error: %s', [QuotedStr(PrinterName), CopyCount,
          StrPas(BooleanNames[SplitCopies]), E.Message]);
        {$ENDIF}
        Quit;
      end;
  end;
  DoClose;
end;

procedure TDeMSOfficeReport.doPrintToFile(const aFileName: string);
begin
  {$IFDEF DEBUG}
  DebugLog('TDeMSOfficeReport.doPrintToFile(%s) ...', [QuotedStr(aFileName)]);
  {$ENDIF}
end;

class function TDeMSOfficeReport.IsSupportExtension(const Extension: string): Boolean;
begin
  // Базовый класс офисных отчётов не поддерживает никаких расширений файлов!
  Result := False;
end;

{TSheetInfo ===================================================================}
type
  TSheetInfo = class
  private
    FTemplRow: Integer;
    FOutLines: TList<Integer>;
    procedure CheckOutline(Column: Integer);
    function getOutLine(Column: Integer): Integer;
    procedure setOutLine(Column: Integer; Value: Integer);
  public
    constructor Create(aLastTemplateRow: Integer);
    destructor Destroy; override;
    procedure AdjustOutlines(minCol, maxCol, maxRow: Integer);
    function OutLineForRange(minCol, maxCol: NativeInt): Integer;
    property LastTemplateRow:integer read FTemplRow;
    property OutLine[Column:integer]: Integer read getOutLine;
  end;

constructor TSheetInfo.Create(aLastTemplateRow: integer);
begin
  inherited Create;
  FTemplRow:= aLastTemplateRow;
  FOutLines:= TList<Integer>.Create;
end;

destructor TSheetInfo.Destroy;
begin
  FOutLines.Free;
  inherited Destroy;
end;

procedure TSheetInfo.CheckOutline(Column: Integer);
var
  i: Integer;
begin
  if (Column >= FOutLines.Count) then
  begin
    for i:= FOutLines.Count to Column do
      FOutLines.Add(FTemplRow+1);
  end;
end;

function TSheetInfo.getOutLine(Column: Integer): Integer;
begin
  CheckOutLine(Column);
  Result:= FOutLines[Column];
end;

procedure TSheetInfo.setOutLine(Column:Integer; Value:Integer);
begin
  CheckOutLine(Column);
  FOutLines[Column]:= Value;
end;

procedure TSheetInfo.AdjustOutlines(minCol, maxCol, maxRow: Integer);
var
  i: Integer;
begin
  for i:= minCol to maxCol do
    setOutLine(i, maxRow);
end;

function TSheetInfo.OutLineForRange(minCol,maxCol:NativeInt): Integer;
var
  i: Integer;
begin
  Result:= 1;
  for i:= minCol to maxCol do
    if Result < OutLine[i] then Result:= OutLine[i];
end;

function Cell(x,y: Integer):String;
begin
  if x>26 then result:= CHR(64+(x-1)div(26)) + CHR(65+(x-1)mod(26)) + IntToStr(y)
          else result:= CHR(64+x) + IntToStr(y);
end;

{TDeExcelReport ===============================================================}

constructor TDeExcelReport.Create(const aFileName: string);
begin
  inherited Create(aFileName);
  FSheets := TStringList.Create;
end;

destructor  TDeExcelReport.Destroy;
begin
  ClearSheets;
  FSheets.Free;
  inherited Destroy;
end;

procedure TDeExcelReport.ClearSheets;
var
  Index: Integer;
begin
  while FSheets.Count > 0 do
    begin
      Index := Pred(FSheets.Count);
      try
        FSheets.Objects[Index].Free;
      except
        {$IFDEF DEBUG}
        on E: Exception do
          DebugLog('TDeExcelReport.ClearSheets skip error: ' + E.Message);
        {$ENDIF}
      end;
      FSheets.Delete(Index);
    end;
end;

procedure TDeExcelReport.Quit;
begin
  FApp.Quit;
end;

procedure TDeExcelReport.AddSheetInfo(const rName: string; const aLastTemplateRow: Integer);
begin
  if FSheets.IndexOf(rName) = -1 then
    FSheets.AddObject(rName, TSheetInfo.Create(aLastTemplateRow));
end;

function TDeExcelReport.GetSheetInfo(const rName:string): TObject;
var
  Index: Integer;
begin
  Index := FSheets.IndexOf(rName);
  if Index <> -1 then
    Result := FSheets.Objects[index]
  else
    Result := nil;
end;

class function TDeExcelReport.IsSupportExtension(const Extension: string): Boolean;
begin
  Result := IsFileExtension(Extension, [sExtensionXLS, sExtensionXLSX]);
end;

procedure TDeExcelReport.doLoadTemplate;
var
  sTemplate, sTemplateUpper: string;
  Sheet: OleVariant;
begin
  DeTemplate.Text:= EmptyStr;
  if not(VarType(FBook) = varDispatch) then
    begin
      SendMessage( Application.MainForm.Handle, DM_ERRORNOTIFY, 29, NativeInt(Format(GetTitle('_De.File'),[FileName])));
      Exit;
    end;
  try
    Sheet :=FApp.WorkSheets[1];

    if varIsEmpty(Sheet.Range['Main']) then sTemplate := EmptyStr
                                       else sTemplate := Sheet.Range['Main'].Value;

    sTemplateUpper := {Ansi}UpperCase(sTemplate);
    if ( 0 = pos(slrREPORT_OPEN_STR, sTemplateUpper )) and ( 0 = pos(slrREPORT_CLOSE_STR, sTemplateUpper )) then
      sTemplate := slrREPORT_OPEN_STR + sTemplate + slrREPORT_CLOSE_STR;

    DeTemplate.Text := sTemplate;
  finally
    Sheet  := unAssigned;
  end;
end;

procedure TDeExcelReport.doOpenDoc;
begin
//
end;

procedure TDeExcelReport.DoOpenTemplate;
begin
  // подгрузить EXCEL
  try
    FApp := CreateOleObject(ExcelName);
    FApp.Visible := False;
    try
      FBook := FApp.Workbooks.Open(FileName);
    except
      {$IFDEF DEBUG}
      on E: Exception do
        DebugLog('TDeExcelReport.DoOpenTemplate skip error: ' + E.Message);
      {$ENDIF}
    end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TDeExcelReport.DoOpenTemplate error: ' + E.Message);
        {$ENDIF}
        FApp := Unassigned;
        //FBook := Unassigned;
      end;
  end;
end;

procedure TDeExcelReport.DoShowDoc;
begin
  FBook.WorkSheets[1].Activate;
  FBook.WorkSheets[1].Range['A1'].Select;

  FApp.Application.WindowState := 2;
  FApp.Visible := True;
  FApp.Application.WindowState := 1;
end;

procedure TDeExcelReport.DoPrintDoc(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean);
begin
  //F App.ActiveWorkBook
  FBook.PrintOut(From := 1, To := 9999, Copies := CopyCount, Preview := False,
    ActivePrinter := PrinterName, Collate := SplitCopies);
end;

procedure TDeExcelReport.ProcessTemplate;
var
  UsedRange : OLEVariant;
  UsedArea  : OLEVariant;
  Template  : OLEVariant;
  iSheet    : integer;
  aSheet    : OLEVariant;
  aSheetInfo: TSheetInfo;
  i         : integer;
begin
//  FBook := F App.ActiveWorkbook;
  for iSheet := 1 to FBook.WorkSheets.Count do
  begin
    aSheet    := FBook.WorkSheets[iSheet];
    UsedRange := aSheet.UsedRange;
    UsedArea  := UsedRange.Cells[UsedRange.Rows.Count,UsedRange.Columns.Count];
    Template  := aSheet.Range['A1',UsedArea.Cells[1,1].Address];
    AddSheetInfo(aSheet.Name,Template.Rows.Count+1);
  end;

  inherited ProcessTemplate;

  for iSheet := FBook.WorkSheets.Count downto 1 do
  begin
    aSheet    := FBook.WorkSheets[iSheet];
    aSheet.Activate;
    aSheetInfo := TSheetInfo(getSheetInfo(aSheet.Name));
    if aSheetInfo <> nil then
    begin
      for i := 1 to aSheetInfo.LastTemplateRow do
        aSheet.Rows[1].Delete;
    end;
    aSheet.Range['A1'].Select;
  end;
  FBook.Save;
//  FBook := unAssigned;
end;

function TDeExcelReport.ProcessMakeTag(NodeValue: TDeNode): Boolean;
var
  Doc: TDeTemplate;
  sRange     : string;
  aSheet     : OLEVariant;
  Range      : OLEVariant;
  OutRange   : OLEVariant;
  Z          : OleVariant;
  SI         : TSheetInfo;
  vValue     : Variant;
  line, col  : integer;
  minCol, maxCol, startRow, maxRow : integer;
begin
  Result := false;
  if not(NodeValue.HasAttribute(slrRANGE_ATR))then exit;
  sRange := ANSIUpperCase(Utf8ToAnsi(NodeValue.Attributes[slrRANGE_ATR]));

  FApp.Goto(sRange,true);

  aSheet := FApp.ActiveSheet;
  if varIsEmpty(aSheet) then exit;

  SI := TSheetInfo(getSheetInfo(aSheet.Name));
  if SI = nil then exit;

  Range  := aSheet.Range[sRange];
  if varIsEmpty(Range) then exit;

  minCol   := Range.Column;
  maxCol   := minCol + Range.Columns.Count - 1;

  startRow := SI.OutLineForRange(minCol, maxCol);
  maxRow   := Range.Rows.Count;

  OutRange := aSheet.Range[Cell(minCol,startRow),Cell(maxCol, startRow+maxRow-1)];
  Range.Select;
  Range.Copy(OutRange);

  Z := VarArrayCreate([ 1, maxRow, minCol, maxCol], varOleStr);
  Z := Range.Value;

  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, maxRow, NodeValue.Weight);
  try
  for line := 1 to maxRow do
    begin
      SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, Line-1, 0);
      for col := minCol to maxCol do
        try
          Doc:= TDeTemplate.Create(Z[line, col], FDeTemplate.Prefix);
          ProcessTextTag(Doc.Root.ChildNodes[0], vValue);
          Doc.Free;
          Z[line, col]:= ConvertValue(vValue, ftString);
          //TODO: -надо доделать вывод в Эксель раздельно текста и других значение
          //            if 0<length(vValue) then
          //              if pos('''',vValue)=0 then vValue:=''''+vValue;
        except
          on E: Exception do
            begin
              {$IFDEF DEBUG}
              DebugLog('TDeExcelReport.ProcessMakeTag skip error: ' + E.Message);
              {$ENDIF}
              Z[line, col] := EmptyStr;
            end;
        end;
    end;
  finally
    SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, -1, -1);
  end;

  OutRange.Value := Z;

  SI.AdjustOutlines(minCol,maxCol,OutRange.Row+OutRange.Rows.Count);
  Result := true;
end;

procedure TDeExcelReport.doSaveToFile(const aFileName: string);
begin
  FBook.SaveAs(aFileName);
end;

{ TDeWordReport }

constructor TDeWordReport.Create(const aFileName: string);
begin
  inherited Create(aFileName);
  FExampleDoc := unAssigned;
  FResultDoc  := unAssigned;
end;

destructor TDeWordReport.Destroy;
begin
  ClearRanges;
  inherited Destroy;
end;

procedure TDeWordReport.ClearRanges;
var i: Integer;
begin
  for i:=Length(FRanges)-1 downto 0 do
    FRanges[i]:=Unassigned;
  SetLength(FRanges, 0);
end;

procedure   TDeWordReport.doLoadTemplate;
var
  FPreFind      : OLEVariant;
  sTemplate, sTemplateUpper : string;
  N, HiddenBegin, HiddenEnd, HiddenLastEnd : Integer;
  ProgressItem : TStatusItem;
begin

  DeTemplate.Text:= EmptyStr;

  ClearRanges;

  // Для терминального сервера краковреммено показываем окно ворда, это в разы ускоряет формирование
  if Not(GetSystemMetrics(SM_REMOTESESSION) = 0) then
    begin
      FApp.Application.WindowState := 2;
      FApp.visible:=true;   // сделать экземпляр Word видимым и тут же
    end;
  FApp.Visible := debugShowWindow;

  FExampleDoc.Bookmarks.Item('\Startofdoc').Range.select; //FExampleDoc.Range(0,0);
  FExampleRange := FApp.Selection;

  try
    sTemplate := EmptyStr;
    FPreFind := FExampleRange.Find;

    FPreFind.ClearFormatting;
    FPreFind.Forward := True;
    FPreFind.Text := EmptyStr;
    FPreFind.Replacement.Text := EmptyStr;
    FPreFind.MatchWildcards := True;
    FPreFind.Font.Hidden := True;

    HiddenLastEnd := 0;
    if FPreFind.Execute then
      begin
        HiddenBegin   := FExampleRange.Start;
        HiddenEnd     := FExampleRange.End;
      end;

    ProgressItem:=DeStatus.StatusItemByID(StatusPrintID);
    if assigned(ProgressItem) then ProgressItem.Progress:=15;

    while (HiddenBegin < HiddenEnd) and (HiddenLastEnd < HiddenEnd) do
      begin
        if (0 < HiddenLastEnd) and (HiddenLastEnd < HiddenBegin) then
          begin
            N :=Length(FRanges);
            SetLength(FRanges, N+1);
            SetLength(FRangesText, N+1);

            FRanges[N]:=FExampleDoc.Range( Start := HiddenLastEnd, End := HiddenBegin ).FormattedText;
            FRangesText[N]:=FRanges[N].Text;

            sTemplate := sTemplate + Format('<%s %s="%s"/>', [slrMAKE_TAG, slrRANGE_ATR, IntToStr(N)]);
          end;

        sTemplate :=  sTemplate + FExampleDoc.Range( HiddenBegin, HiddenEnd ).Text;

        FPreFind.Execute;
        HiddenLastEnd := HiddenEnd;
        HiddenBegin   := FExampleRange.Start;
        HiddenEnd     := FExampleRange.End;
        FExampleRange.Collapse( Direction:=wdCollapseEnd );
      end;
    FPreFind      := Unassigned;
    FExampleRange := Unassigned;

    sTemplateUpper := ANSIUpperCase(sTemplate);
    if ( 0 = pos(slrREPORT_OPEN_STR, sTemplateUpper )) and ( 0 = pos(slrREPORT_CLOSE_STR, sTemplateUpper )) then
      sTemplate := slrREPORT_OPEN_STR + sTemplate + slrREPORT_CLOSE_STR;

    DeTemplate.text := sTemplate;
  finally
  end;
end;

procedure   TDeWordReport.DoOpenTemplate;
begin
  try
    FApp := CreateOLEObject(WordName);

    FExampleDoc:=FApp.Documents.Add (Template:=FileName, DocumentType:=0);
    FExampleDoc.ActiveWindow.View.ShowAll := true;  // Отображаем спецсимволы
//    FExampleRange := FApp.Selection;
  except
    FApp := unAssigned;
    FExampleDoc  := unAssigned;
  end;
end;

procedure TDeWordReport.doOpenDoc;
begin
  try
    FResultDoc :=FApp.Documents.Add (Template:=FileName, DocumentType:=0);
    FResultDoc.ActiveWindow.View.ShowAll := False;  // скрываем спецсимволы
    FResultDoc.Range.Delete;
  except
    FResultDoc := unAssigned;
  end;
end;

procedure TDeWordReport.DoShowDoc;
begin
  FExampleDoc.close(False);
  FExampleDoc := unassigned;
  FResultDoc.SaveAs(FileName);

  FApp.Application.WindowState := 2;
  FApp.Visible := true;
  FApp.Activate; // только для Word
  FApp.Application.WindowState := 1;
end;

class function TDeWordReport.IsSupportExtension(const Extension: string): Boolean;
begin
  Result := IsFileExtension(Extension, [sExtensionRTF, sExtensionDOC, sExtensionDOCX]);
end;

procedure TDeWordReport.DoPrintDoc(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean);
begin
  FApp.ActivePrinter := PrinterName;

  FExampleDoc.Close(False);
  FResultDoc.SaveAs(FileName);
  FResultDoc.PrintOut(Background:=False, Copies:=CopyCount, Collate:=SplitCopies);
end;

procedure TDeWordReport.DoClose;
begin
  ClearRanges;
  inherited doClose;
end;

procedure TDeWordReport.Quit;
begin
  FApp.Quit(0);
end;

procedure TDeWordReport.ProcessTemplate;
begin
  FResultRange := FResultDoc.Range.Find;
  FResultRange.ClearFormatting;
  FResultRange.Forward := True;
  FResultRange.Text := '[<]*[/][>]';
  FResultRange.Replacement.Text := EmptyStr;
  FResultRange.MatchWildcards := True;
  FResultRange.Wrap := wdFindContinue;
  FResultRange.Font.Hidden := False;

  inherited ProcessTemplate;

  FResultRange := UnAssigned;
end;

function TDeWordReport.ProcessMakeTag(NodeValue: TDeNode): Boolean;
var
  Doc: TDeTemplate;
  s,t        : string;
  L,p,iRange : Integer;
  vValue     : Variant;
  MS         : TMemoryStream;
  MError     : Boolean;
begin
  Result := false;
  iRange := StrToIntDef(NodeValue.Attributes[slrRANGE_ATR],-1);

  if (iRange<0) or (Length(FRanges)<=iRange) then exit;

  //try
    FResultDoc.Bookmarks.Item('\endofdoc').Range.FormattedText := FRanges[iRange];

    //пройтись по тексту блока, найти и выполнить подстановку данных
    s:=FRangesText[iRange];

    L:=Length(s);
    SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, L, NodeValue.weight);
    try

    p:=Pos('<',s);
    while 0<p do
    begin
      SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, L-Length(s), 0);
      Delete(s,1,p-1);
      p:=Pos('>',s);

      if p>0 then // проверяем наличие закрывающей скобки ">"
        begin

          if CompareText(Copy(s,2,5),'IMAGE')=0 then
            begin
              ProcessImageValue(NodeValue,Copy(s,1,p),vValue);
              t:=ConvertValue(vValue, ftString);

              FResultRange.Execute;

              if FileExists(t) then
                FApp.Selection.InlineShapes.AddPicture(t,False,true)
              else
                FApp.Selection.Delete;
            end
          else
            begin
              Doc:= TDeTemplate.Create( Copy(s,1,p), FDeTemplate.Prefix);
              ProcessTextTag(Doc.Root.ChildNodes[0], vValue);
              Doc.Free;

              t:= ConvertValue(vValue,ftString);

              if 254 < length(t) then
                begin
                  MError:= False;

                  ms := TMemoryStream.Create;
                  try
                    SaveClipboard(ms);
                  except
                    MError:=True;
                  end;

                  Clipboard.Open;
                  Clipboard.asText:=t;
                  Clipboard.Close;
                  FResultRange.Execute( ReplaceWith:= '^c', Replace := wdReplaceOne);

                  try
                    if Not MError then LoadClipboard(ms);
                  except
                    MError:=True;
                  end;
                  ms.Free;
                  if MError then ClipBoard.Clear;
                end
              else
                begin
                  t:= StringReplace(t,'^','^^',[rfReplaceAll]);
                  t:= StringReplace(t,'\','^0092',[rfReplaceAll]);
                  FResultRange.Execute( ReplaceWith:= t, Replace := wdReplaceOne);
                end;
            end;

          Delete(s,1,p);
          p:=Pos('<',s);
        end;
    end;
    finally
      SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
    end;

    Result := true;
  //finally
  //end;
end;

procedure TDeWordReport.doSaveToFile(const aFileName: string);
begin
  FResultDoc.SaveAs(aFileName);
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DeXLReport unit initialization ...');

finalization
  DebugLog('DeXLReport unit finalization ...');
{$ENDIF}

end.

