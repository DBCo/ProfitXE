{$WARN SYMBOL_PLATFORM OFF}

unit DeMicrosoftOfficeReport;

interface

uses Classes, Excel_TLB, Word_TLB, Generics.Collections,
  DeXMLReport, DeTemplate;

type
  TMicrosoftOfficeReport = class(TDeXMLReport)
  private
  protected
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
    constructor Create(const aFileName: string);
    //destructor Destroy; override;
    class function IsSupportExtension(const Extension: string): Boolean; override;
  end;

  TMicrosoftExcelReport = class(TMicrosoftOfficeReport)
  private
    FExcelApplication: Excel_TLB._Application;
    FExcelWorkbook: Excel_TLB._Workbook;
    FSheets: TStrings;
    type
      TSheetInfo = class
      private
        FLastTemplateRow: Integer;
        FOutLines: TList<Integer>;
        FOSizeR, FOSizeC, FRSizeR, FRSizeC: array of Integer;
        procedure CheckOutline(const Column: Integer);
        function GetOutLine(const Column: Integer): Integer;
        procedure SetOutLine(const Column: Integer; const Value: Integer);
      public
        constructor Create(const ALastTemplateRow: Integer);
        destructor Destroy; override;
        function GetWidth(const aWorksheet: Excel_TLB._Worksheet; const Column: Integer): Integer;
        function GetHeight(const aWorksheet: Excel_TLB._Worksheet; const Row: Integer): Integer;
        procedure SetWidth(const aWorksheet: Excel_TLB._Worksheet; const Column, Value: Integer);
        procedure SetHeight(const aWorksheet: Excel_TLB._Worksheet; const Row, Value: Integer);
        procedure AdjustOutlines(const minCol, maxCol, maxRow: Integer);
        function OutLineForRange(const minCol, maxCol: Integer): Integer;
        property LastTemplateRow: Integer read FLastTemplateRow;
        property OutLine[const Column: Integer]: Integer read GetOutLine;
      end;
    procedure ClearSheets;
    procedure AddSheetInfo(const Name: string; const LastTemplateRow: Integer);
    function GetSheetInfo(const Name: string): TSheetInfo;
  protected
    procedure doOpenTemplate; override;
    procedure doLoadTemplate; override;
    procedure doOpenDoc; override;
    procedure doShowDoc; override;
    procedure doPrintDoc(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); override;
    procedure ProcessTemplate; override;
    function ProcessMakeTag(NodeValue: TDeNode): Boolean; override;
    procedure doClose; override;
    procedure Quit; override;
    procedure doSaveToFile(const aFileName: string); override;
    procedure doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); override;
  public
    constructor Create(const aFileName: string);
    destructor Destroy; override;
    class function IsSupportExtension(const Extension: string): Boolean; override;
  end;

  TMicrosoftWordRangeRecord = packed record
    Range: Word_TLB.WordRange;
    Text: string;
  end;

  TMicrosoftWordReport = class(TMicrosoftOfficeReport)
  private
    FWordApplication: Word_TLB._Application;
    FTemplateDocument: Word_TLB.WordDocument;
    FResultDocument: Word_TLB.WordDocument;
    FResultFind: Word_TLB.Find;
    FRanges: array of TMicrosoftWordRangeRecord;
    procedure UpdateResultFind;
  protected
    procedure ClearRanges;
    procedure doOpenTemplate; override;
    procedure doLoadTemplate; override;
    procedure doOpenDoc; override;
    procedure doShowDoc; override;
    procedure doPrintDoc(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); override;
    procedure doClose; override;
    procedure ProcessTemplate; override;
    function ProcessMakeTag(NodeValue: TDeNode): Boolean; override;
    procedure Quit; override;
    procedure doSaveToFile(const aFileName: string); override;
    procedure doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean); override;
    // v 17.4
    procedure ChangeTargetFileName(const OldFileName, NewFileName: string); override;
  public
    destructor Destroy; override;
    class function IsSupportExtension(const Extension: string): Boolean; override;
  end;

implementation

uses Windows, SysUtils, Variants, ComObj, Forms, DB, Math,
  Funcs, DeTypes, DeLog, Dictionary, DeMeta, DataUnit;

function Cell(const X, Y: Integer): string;
begin
  if X > 26 then
    Result := Chr(64 + (X - 1)div 26) + Chr(65 + (X - 1) mod 26) + IntToStr(Y)
  else
    Result := Chr(64 + X) + IntToStr(Y);
end;

{ TMicrosoftOfficeReport }

constructor TMicrosoftOfficeReport.Create(const aFileName: string);
begin
  inherited Create;
  FileName := aFileName;
  DM.RegisterTemporaryFile(aFileName);
end;

class function TMicrosoftOfficeReport.IsSupportExtension(const Extension: string): Boolean;
begin
  // Базовый класс офисных отчётов не поддерживает никаких расширений файлов!
  Result := False;
end;

procedure TMicrosoftOfficeReport.LoadParamDefs;
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

procedure TMicrosoftOfficeReport.doPreview;
begin
  doPrint(EmptyStr, 1, True);
end;

{ TMicrosoftExcelReport }

constructor TMicrosoftExcelReport.Create(const aFileName: string);
begin
  inherited Create(aFileName);
  FSheets := TStringList.Create;
end;

destructor TMicrosoftExcelReport.Destroy;
begin
  ClearSheets;
  FSheets.Free;
  inherited Destroy;
end;

class function TMicrosoftExcelReport.IsSupportExtension(const Extension: string): Boolean;
begin
  Result := IsFileExtension(Extension, [sExtensionXLS, sExtensionXLSX]);
end;

procedure TMicrosoftExcelReport.ClearSheets;
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
          DebugLog('TMicrosoftExcelReport.ClearSheets skip error: ' + E.Message);
        {$ENDIF}
      end;
      FSheets.Delete(Index);
    end;
end;

procedure TMicrosoftExcelReport.AddSheetInfo(const Name: string; const LastTemplateRow: Integer);
begin
  if FSheets.IndexOf(Name) = -1 then
    FSheets.AddObject(Name, TSheetInfo.Create(LastTemplateRow));
end;

function TMicrosoftExcelReport.GetSheetInfo(const Name: string): TSheetInfo;
var
  Index: Integer;
begin
  Index := FSheets.IndexOf(Name);
  if Index <> -1 then
    Result := FSheets.Objects[Index] as TSheetInfo
  else
    Result := nil;
end;

procedure TMicrosoftExcelReport.doOpenTemplate;
begin
  try
    FExcelApplication := CreateComObject(CLASS_ExcelApplication) as Excel_TLB._Application;
    FExcelApplication.Visible[0] := False;
    try
      FExcelWorkbook := FExcelApplication.Workbooks.Open(FileName, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
        EmptyParam, EmptyParam,EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
        EmptyParam, 0);
    except
      {$IFDEF DEBUG}
      on E: Exception do
        DebugLog('TMicrosoftExcelReport.doOpenTemplate skip error: ' + E.Message);
      {$ENDIF}
    end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TMicrosoftExcelReport.doOpenTemplate error: ' + E.Message);
        {$ENDIF}
        FExcelApplication := nil;
      end;
  end;
end;

procedure TMicrosoftExcelReport.doLoadTemplate;
var
  Template, TemplateUpper: string;
  Worksheet: Excel_TLB._Worksheet;
  Range: Excel_TLB.ExcelRange;
  Value: OleVariant;
  i,j: Integer;
begin
    {
    FExcelApplication.Application.WindowState[0] := 2;
    FExcelApplication.Visible[0] := True;
    FExcelApplication.Application.WindowState[0] := 1;
    {}
  DeTemplate.Text := EmptyStr;
  if not Assigned(FExcelWorkbook) then
    begin
      SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 29, NativeInt(Format(GetTitle('_De.File'),[FileName])));
      Exit;
    end;
  Worksheet := FExcelApplication.Worksheets[1] as Excel_TLB._Worksheet;
  try
    try
      Range := FExcelApplication.Range['Main', EmptyParam];
    except
      Range := Worksheet.Range[Cell(1,1),Cell(1,1)];
    end;
    try
      if Assigned(Range) then
        begin
          Value := Range.Value[xlRangeValueDefault];

          if VarIsEmpty(Value) then  Template := EmptyStr else

          if Not VarIsArray(Value) then Template := VarToStr(Value) else

       // if VarIsArray(Value) then
            begin
              Template:= EmptyStr;
              for i:= VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
                begin
                  for j:= VarArrayLowBound(Value, 2) to VarArrayHighBound(Value, 2) do
                    Template:= Template + VarToStr(VarArrayGet( Value, [i,j])) + ' ';
                  Template:= Template + #13#10;
                end;
            end;
        end
      else
        Template := EmptyStr;
    finally
      Range := nil;
    end;
    TemplateUpper := UpperCase(Template);
    if (0 = pos(slrREPORT_OPEN_STR, TemplateUpper)) and (0 = pos(slrREPORT_CLOSE_STR, TemplateUpper)) then
      Template := slrREPORT_OPEN_STR + Template + slrREPORT_CLOSE_STR;
    DeTemplate.Text := Template;
  finally
    Worksheet := nil;
  end;
end;

procedure TMicrosoftExcelReport.doOpenDoc;
begin
  // Ничего не делаем ...
end;

procedure TMicrosoftExcelReport.doShowDoc;
var
  Worksheet: Excel_TLB._Worksheet;
begin
  Worksheet := FExcelWorkbook.WorkSheets[1] as Excel_TLB._Worksheet;
  try
    Worksheet.Activate(0);
    Worksheet.Range['A1', EmptyParam].Select;

    FExcelApplication.Application.WindowState[0] := 2;
    FExcelApplication.Visible[0] := True;
    FExcelApplication.Application.WindowState[0] := 1;
  finally
    Worksheet := nil;
  end;
end;

procedure TMicrosoftExcelReport.doPrintDoc(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean);
begin
  FExcelWorkbook.PrintOut(1, 9999, CopyCount, False, PrinterName, EmptyParam, SplitCopies, EmptyParam, EmptyParam, 0);
end;

procedure TMicrosoftExcelReport.ProcessTemplate;
var
  SheetIndex{, Index}: Integer;
  Worksheet: Excel_TLB._Worksheet;
  UsedRange, UsedArea{, Template}: Excel_TLB.ExcelRange;
  SheetInfo: TSheetInfo;
begin
  for SheetIndex := 1 to FExcelWorkbook.WorkSheets.Count do
    begin
      Worksheet := FExcelWorkbook.WorkSheets[SheetIndex] as Excel_TLB._Worksheet;
      try
        UsedRange := Worksheet.UsedRange[0];
        try
          AddSheetInfo(Worksheet.Name, UsedRange.Rows.Count + 1);
        finally
          UsedRange := nil;
        end;
      finally
        Worksheet := nil;
      end;
    end;

  inherited ProcessTemplate;

  for SheetIndex := FExcelWorkbook.WorkSheets.Count downto 1 do
  begin
    Worksheet := FExcelWorkbook.WorkSheets[SheetIndex] as Excel_TLB._Worksheet;
    try
      Worksheet.Activate(0);
      SheetInfo := GetSheetInfo(Worksheet.Name);
      if Assigned(SheetInfo) then
        begin
          UsedRange := Worksheet.UsedRange[0];
          UsedArea := Worksheet.Rows.Range[Cell(1, 1), Cell(UsedRange.Columns.Count + 1, SheetInfo.LastTemplateRow)].EntireRow;
          UsedArea.Delete(xlUp);
        end;
        {
        for Index := 1 to SheetInfo.LastTemplateRow do
          begin
            UsedRange := Worksheet.UsedRange[0];
            UsedArea := Worksheet.Rows.Range[Cell(1, 1), Cell(UsedRange.Columns.Count + 1, 1)];
            UsedArea.Delete(xlShiftUp);
          end;
        }
      Worksheet.Range['A1', EmptyParam].Select;
    finally
      Worksheet := nil;
    end;
  end;
  FExcelWorkbook.Save(0);
end;

function TMicrosoftExcelReport.ProcessMakeTag(NodeValue: TDeNode): Boolean;
var
  Doc: TDeTemplate;
  s, sRange: string;
  p1, p2, RangeRowsCount, RangeColsCount: Integer;
  Worksheet: Excel_TLB._Worksheet;
  SheetInfo: TSheetInfo;
  Range, OutRange: Excel_TLB.ExcelRange;
  Z: OleVariant;
  vValue, cellValue: Variant;
  line, col, minCol, maxCol, minRow, maxRow, originalRow, originalCol: Integer;
begin
  Result := False;
  if not NodeValue.HasAttribute(slrRANGE_ATR) then Exit;
  sRange := UpperCase(UTF8ToString(RawByteString(NodeValue.Attributes[slrRANGE_ATR])));

//  FExcelApplication.Goto_(sRange, True, 0);

  Worksheet := FExcelApplication.ActiveSheet as Excel_TLB._Worksheet;
  if not Assigned(Worksheet) then Exit;

  SheetInfo := GetSheetInfo(Worksheet.Name);
  if not Assigned(SheetInfo) then Exit;

  Range := Worksheet.Range[sRange, EmptyParam];
  if not Assigned(Range) then Exit;

  RangeRowsCount:= Range.Rows.Count;
  RangeColsCount:= Range.Columns.Count;
  originalRow:= Range.Row;
  originalCol:= Range.Column;

  if NodeValue.HasAttribute('RIGHT') then
    begin
      minRow:= 1;
      for line:= 0 to Pred(SheetInfo.FOutLines.Count) do
        if minRow < SheetInfo.OutLine[line] - RangeRowsCount then minRow:= SheetInfo.OutLine[line] - RangeRowsCount;
      MaxRow:= RangeRowsCount;

      MinCol:= 1;
      for line:= 0 to Pred(SheetInfo.FOutLines.Count) do
        if minRow = SheetInfo.OutLine[line] - RangeRowsCount then MinCol:= Max(MinCol, line+1);
      MaxCol:= MinCol + RangeColsCount - 1;
    end
  else
    begin
      MinCol:= originalCol;
      MaxCol:= MinCol + RangeColsCount - 1;

      MinRow:= SheetInfo.OutLineForRange(minCol, maxCol);
      MaxRow:= RangeRowsCount;
    end;


  OutRange := Worksheet.Range[Cell(MinCol, MinRow),Cell(maxCol, MinRow + MaxRow - 1)];
//  Range.Select;
  Range.Copy(OutRange);

  Z:= VarArrayCreate([ 1, maxRow, minCol, maxCol], varOleStr);
  Z:= Range.Value[xlRangeValueDefault];

  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, maxRow, NodeValue.Weight);
  try
  for line:= 1 to maxRow do
    begin
      SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, Line-1, 0);

      if not NodeValue.HasAttribute('RIGHT') then
        SheetInfo.SetHeight(Worksheet, minRow+line-1, SheetInfo.GetHeight(Worksheet, originalRow));

      for col := minCol to maxCol do
        try
          if NodeValue.HasAttribute('RIGHT') then
            SheetInfo.SetWidth(Worksheet, Col, SheetInfo.GetWidth(Worksheet, originalCol+col-minCol));

          if VarIsArray(Z) then s:= Z[line, col+1-minCol]
                           else s:= Z;

          p1:=Pos('<',s);
          p2:=Pos('/>',s);

          if p1=0 then Continue;             // если нет тега - оставляем шаблон как текст

          if (p1=1) and (p2+1 = Length(s)) then  // ровно один тег без текста - вставляем как значение
            begin
                  Doc:= TDeTemplate.Create(s, FDeTemplate.Prefix);
                  ProcessTextTag(Doc.Root.ChildNodes[0], cellValue, False);
                  Doc.Free;
            end

          else
            begin                            // составное значение, преобразуем и вставляем как текст
              cellValue:= EmptyStr;
              while (0<p1) and (p1<p2) do
                begin
                  Doc:= TDeTemplate.Create(Copy(s,p1,p2-p1+2), FDeTemplate.Prefix);
                  ProcessTextTag(Doc.Root.ChildNodes[0], vValue);
                  Doc.Free;

                  cellValue:= cellValue + Copy(s, 1, p1-1) + ConvertValue(vValue, ftString);

                  Delete(s, 1, p2+1);
                  p1:=Pos('<',s);
                  p2:=Pos('/>',s);
                end;
            end;

          if varIsType(cellValue, [varString, varOleStr, varUString]) then
            cellValue:= StringReplace(StringReplace(cellValue,'^','^^', [rfReplaceAll]), '\','^0092',[rfReplaceAll]);
          if VarIsArray(Z) then Z[line, col+1-minCol]:= cellValue
                           else Z:= cellValue
        except
          on E: Exception do
            begin
              {$IFDEF DEBUG}
              DebugLog('TDeExcelReport.ProcessMakeTag skip error: ' + E.Message);
              {$ENDIF}
              Z[line, col+1-minCol] := EmptyStr;
            end;
        end;
    end;
  finally
    SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, -1, -1);
  end;

  OutRange.Value[xlRangeValueDefault] := Z;

  SheetInfo.AdjustOutlines(minCol, maxCol, MinRow + RangeRowsCount);
  Result := True;
end;

procedure TMicrosoftExcelReport.Quit;
begin
  FExcelApplication.Quit;
end;

procedure TMicrosoftExcelReport.doClose;
begin
  FExcelWorkbook := nil;
  FExcelApplication := nil;
end;

procedure TMicrosoftExcelReport.doSaveToFile(const aFileName: string);
begin
  FExcelWorkbook.SaveAs(aFileName, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
    EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, 0);
end;

procedure TMicrosoftExcelReport.doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean);
var
  Alerts: Boolean;
begin
  // открыть файл шаблона
  DoOpenTemplate;
  Alerts := FExcelApplication.DisplayAlerts[0];
  if Alerts then FExcelApplication.DisplayAlerts[0] := False;

  if DeTemplate.Root.Count = 0 then
    begin
      doLoadTemplate;
      doOpenDoc;
    end;
  if DeTemplate.Root.Count = 0 then Exit; //Error!

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
    FExcelApplication.DisplayAlerts[0] := Alerts;
    if PrinterName <> EmptyStr then Quit;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TMicrosoftExcelReport.doPrint(%s, %d, %s) error: %s', [QuotedStr(PrinterName), CopyCount,
          StrPas(BooleanNames[SplitCopies]), E.Message]);
        {$ENDIF}
        Quit;
      end;
  end;
  DoClose;
end;

{ TMicrosoftExcelReport.TSheetInfo }

constructor TMicrosoftExcelReport.TSheetInfo.Create(const ALastTemplateRow: Integer);
begin
  inherited Create;
  FLastTemplateRow := ALastTemplateRow;
  FOutLines := TList<Integer>.Create;
  SetLength(FOSizeC, 0);
  SetLength(FOSizeR, 0);
  SetLength(FRSizeC, 0);
  SetLength(FRSizeR, 0);
end;

destructor TMicrosoftExcelReport.TSheetInfo.Destroy;
begin
  FOutLines.Free;
  inherited Destroy;
end;

procedure TMicrosoftExcelReport.TSheetInfo.CheckOutline(const Column: Integer);
var
  Index: Integer;
begin
  if (Column >= FOutLines.Count) then
    for Index := FOutLines.Count to Column do
      FOutLines.Add(Succ(FLastTemplateRow));
end;

function TMicrosoftExcelReport.TSheetInfo.GetOutLine(const Column: Integer): Integer;
begin
  CheckOutLine(Column);
  Result:= FOutLines[Column];
end;

function TMicrosoftExcelReport.TSheetInfo.GetHeight(const aWorksheet: Excel_TLB._Worksheet; const Row: Integer): Integer;
begin
  while Length(FOSizeR) <= Row do
    begin
      SetLength(FOSizeR, Length(FOSizeR) + 1);
      FOSizeR[Pred(Length(FOSizeR))]:= -1;
    end;

  if FOSizeR[Row] = -1 then
    FOSizeR[Row]:= aWorksheet.Range[Cell(1, Row), Cell(1, Row)].RowHeight;

  Result:= FOSizeR[Row];
end;

function TMicrosoftExcelReport.TSheetInfo.GetWidth(const aWorksheet: Excel_TLB._Worksheet; const Column: Integer): Integer;
begin
  while Length(FOSizeC) <= Column do
    begin
      SetLength(FOSizeC, Length(FOSizeC) + 1);
      FOSizeC[Pred(Length(FOSizeC))]:= -1;
    end;

  if FOSizeC[Column] = -1 then
    FOSizeC[Column]:= aWorksheet.Range[Cell(Column, 1), Cell(Column, 1)].ColumnWidth;

  Result:= FOSizeC[Column];
end;

procedure TMicrosoftExcelReport.TSheetInfo.SetHeight(const aWorksheet: Excel_TLB._Worksheet; const Row, Value: Integer);
begin
  while Length(FRSizeR) <= Row do
    begin
      SetLength(FRSizeR, Length(FRSizeR) + 1);
      FRSizeR[Pred(Length(FRSizeR))]:= -1;
    end;

  if FRSizeR[Row] = -1 then
    begin
      FRSizeR[Row]:= Value;
      aWorksheet.Range[Cell(1, Row), Cell(1, Row)].RowHeight:= Value;
    end;
end;

procedure TMicrosoftExcelReport.TSheetInfo.SetWidth(const aWorksheet: Excel_TLB._Worksheet; const Column, Value: Integer);
begin
  while Length(FRSizeC) <= Column do
    begin
      SetLength(FRSizeC, Length(FRSizeC) + 1);
      FRSizeC[Pred(Length(FRSizeC))]:= -1;
    end;

  if FRSizeC[Column] = -1 then
    begin
      FRSizeC[Column]:= Value;
      aWorksheet.Range[Cell(Column, 1), Cell(Column, 1)].ColumnWidth:= Value;
    end;
end;

procedure TMicrosoftExcelReport.TSheetInfo.SetOutLine(const Column, Value: Integer);
begin
  CheckOutLine(Column);
  FOutLines[Column]:= Value;
end;

procedure TMicrosoftExcelReport.TSheetInfo.AdjustOutlines(const minCol, maxCol, maxRow: Integer);
var
  Index: Integer;
begin
  for Index:= minCol to maxCol do
    SetOutLine(Index, maxRow);
end;

function TMicrosoftExcelReport.TSheetInfo.OutLineForRange(const minCol, maxCol: Integer): Integer;
var
  Index: Integer;
begin
  Result:= 1;
  for Index := minCol to maxCol do
    if Result < OutLine[Index] then
      Result:= OutLine[Index];
end;

{ TMicrosoftWordReport }

destructor TMicrosoftWordReport.Destroy;
begin
  ClearRanges;
  FTemplateDocument := nil;
  FWordApplication := nil;
  inherited Destroy;
end;

class function TMicrosoftWordReport.IsSupportExtension(const Extension: string): Boolean;
begin
  Result := IsFileExtension(Extension, [sExtensionRTF, sExtensionDOC, sExtensionDOCX]);
end;

procedure TMicrosoftWordReport.ChangeTargetFileName(const OldFileName, NewFileName: string);
var
  Document: Word_TLB.WordDocument;
begin
  ForceDirectories(ExtractFilePath(OldFileName));
  FResultDocument.SaveAs(OldFileName, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
    EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
  inherited ChangeTargetFileName(OldFileName, NewFileName);
  Document := FResultDocument;
  try
    Win32Check(CopyFile(PChar(OldFileName), PChar(NewFileName), False));
    doOpenDoc;
    UpdateResultFind;
  except
    on E: Exception do
      begin
        FResultDocument := Document;
        {$IFDEF DEBUG}
        Funcs.WriteLog('Create new document %s error: %s', [QuotedStr(ExtractFileName(NewFileName)), E.Message], False, 'Reports');
        {$ENDIF}
      end;
  end;
end;

procedure TMicrosoftWordReport.ClearRanges;
var
  Index: Integer;
begin
  for Index := High(FRanges) downto Low(FRanges) do
    FRanges[Index].Range := nil;
  SetLength(FRanges, 0);
end;

procedure TMicrosoftWordReport.doOpenTemplate;
begin
  try
    FWordApplication := CreateComObject(CLASS_WordApplication) as Word_TLB._Application;
    FTemplateDocument := FWordApplication.Documents.Add(FileName, EmptyParam, 0, EmptyParam);
    FTemplateDocument.ActiveWindow.View.ShowAll := True;  // Отображаем спецсимволы
  except
    on E: Exception do
      begin
        FTemplateDocument := nil;
        FWordApplication := nil;
        {$IFDEF DEBUG}
        DebugLog('TMicrosoftWordReport.doOpenTemplate error: ' + E.Message);
        {$ENDIF}
      end;
  end;
end;

procedure TMicrosoftWordReport.doLoadTemplate;
var
  Selection: Word_TLB.WordSelection;
  Find: Word_TLB.Find;
  sTemplate, sTemplateUpper: string;
  N, HiddenBegin, HiddenEnd, HiddenLastEnd: Integer;
begin
  DeTemplate.Text := EmptyStr;
  ClearRanges;

  // Для терминального сервера краковреммено показываем окно ворда, это в разы ускоряет формирование
  if not (GetSystemMetrics(SM_REMOTESESSION) = 0) then
    begin
      FWordApplication.Application.WindowState := 2;
      FWordApplication.Visible := True;   // сделать экземпляр Word видимым и тут же
    end;
  FWordApplication.Visible := False;

  FTemplateDocument.Bookmarks.Item('\Startofdoc').Range.Select;
  Selection := FWordApplication.Selection;
  try
    sTemplate := EmptyStr;
    Find := Selection.Find;
    try
      Find.ClearFormatting;
      Find.Forward := True;
      Find.Text := EmptyStr;
      Find.Replacement.Text := EmptyStr;
      Find.MatchWildcards := True;
      Find.Font.Hidden := 1;//True;

      HiddenBegin := 0;
      HiddenEnd := 0;
      HiddenLastEnd := 0;
      if Find.Execute(EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam) then
        begin
          HiddenBegin := Selection.Start;
          HiddenEnd := Selection.End_;
        end;

      while (HiddenBegin < HiddenEnd) and (HiddenLastEnd < HiddenEnd) do
        begin
          if (0 < HiddenLastEnd) and (HiddenLastEnd < HiddenBegin) then
            begin
              N := Length(FRanges);
              SetLength(FRanges, Succ(N));
              FRanges[N].Range := FTemplateDocument.Range(HiddenLastEnd, HiddenBegin).FormattedText;
              FRanges[N].Text := FRanges[N].Range.Text;
              sTemplate := sTemplate + Format('<%s %s="%s"/>', [slrMAKE_TAG, slrRANGE_ATR, IntToStr(N)]);
            end;
          sTemplate := sTemplate + FTemplateDocument.Range(HiddenBegin, HiddenEnd).Text;
          Find.Execute(EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
          HiddenLastEnd := HiddenEnd;
          HiddenBegin := Selection.Start;
          HiddenEnd := Selection.End_;
          Selection.Collapse(wdCollapseEnd);
        end;
      sTemplateUpper := UpperCase(sTemplate);
      if (0 = Pos(slrREPORT_OPEN_STR, sTemplateUpper)) and (0 = Pos(slrREPORT_CLOSE_STR, sTemplateUpper)) then
        sTemplate := slrREPORT_OPEN_STR + sTemplate + slrREPORT_CLOSE_STR;
      DeTemplate.text := sTemplate;
    finally
      Find := nil;
    end;
  finally
    Selection := nil;
  end;
end;

procedure TMicrosoftWordReport.doOpenDoc;
begin
  try
    FResultDocument := FWordApplication.Documents.Add(TargetFileName, EmptyParam, 0, EmptyParam);
    FResultDocument.ActiveWindow.View.ShowAll := False;  // скрываем спецсимволы
    FResultDocument.Range(EmptyParam, EmptyParam).Delete(EmptyParam, EmptyParam);
  except
    on E: Exception do
      begin
        FResultDocument := nil;
        {$IFDEF DEBUG}
        DebugLog('TMicrosoftWordReport.doOpenDoc error: ' + E.Message);
        {$ENDIF}
      end;
  end;
end;

procedure TMicrosoftWordReport.doShowDoc;
begin
  FTemplateDocument.Close(False, EmptyParam, EmptyParam);
  FTemplateDocument := nil;
  FResultDocument.SaveAs(TargetFileName, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
    EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
  FWordApplication.Application.WindowState := 2;
  FWordApplication.Visible := True;
  FWordApplication.Activate; // только для Word
  FWordApplication.Application.WindowState := 1;
end;

procedure TMicrosoftWordReport.doPrintDoc(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean);
begin
  FWordApplication.ActivePrinter := PrinterName;
  FTemplateDocument.Close(False, EmptyParam, EmptyParam);
  FResultDocument.SaveAs(TargetFileName, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
    EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
  FResultDocument.PrintOut(False, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, CopyCount,
    EmptyParam, EmptyParam, EmptyParam, SplitCopies, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TMicrosoftWordReport.doClose;
begin
  ClearRanges;
  FResultDocument := nil;
  FWordApplication := nil;
end;

procedure TMicrosoftWordReport.ProcessTemplate;
begin
  try
    UpdateResultFind;
    inherited ProcessTemplate;
  finally
    FResultFind := nil;
  end;
end;

function TMicrosoftWordReport.ProcessMakeTag(NodeValue: TDeNode): Boolean;
var
  Doc: TDeTemplate;
  RangeIndex, TextLength, Position, W : Integer;
  Text, TagText, TagGUID: string;
  Value: Variant;
  ClipboardError: Boolean;
  Find: Word_TLB.Find;
  InlineShape: OleVariant;
begin
  Result := False;
  RangeIndex := StrToIntDef(NodeValue.Attributes[slrRANGE_ATR], -1);

  if (RangeIndex < 0) or (Length(FRanges) <= RangeIndex) then Exit;

  FResultDocument.Bookmarks.Item('\endofdoc').Range.FormattedText := FRanges[RangeIndex].Range;

  //пройтись по тексту блока, найти и выполнить подстановку данных
  Text := FRanges[RangeIndex].Text;
  TextLength := Length(Text);
  SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, TextLength, NodeValue.Weight);
  try
    Position := Pos('<', Text);
    while 0 < Position do
      begin
        SendMessage(Application.MainForm.Handle,DM_PROGRESSNOTIFY, TextLength - Length(Text), 0);
        Delete(Text, 1, Pred(Position));
        Position := Pos('>', Text);
        if Position > 0 then // проверяем наличие закрывающей скобки ">"
          begin
            Doc:= TDeTemplate.Create( Copy(Text, 1, Position), FDeTemplate.Prefix);
            if Doc.Root.Count > 0 then
              begin
                if SameText( Doc.Root.ChildNodes[0].Attributes[slrFORMAT_ATR], 'IMAGE') then
                  begin
                    ProcessImageTag(Doc.Root.ChildNodes[0], Value);
                    TagText:= ConvertValue(Value, ftString);

                    if FileExists(TagText) then
                      begin
                        TagGUID:= GUIDToString(NewGUID);
                        FResultFind.Execute(EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
                          wdFindContinue, EmptyParam, TagGUID, wdReplaceOne, EmptyParam, EmptyParam, EmptyParam, EmptyParam);

                        Find:= FWordApplication.Selection.Find;
                        try
                          Find.ClearFormatting;
                          Find.Forward:= True;
                          Find.MatchWildcards:= False;

                          if Find.Execute( TagGUID, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
                                           EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
                                           EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam)
                            then
                              begin
                                InlineShape:= FWordApplication.Selection.InlineShapes.AddPicture( TagText, False, True, EmptyParam);
                                W:= StrToIntDef(Doc.Root.ChildNodes[0].Attributes[slrWIDTH_ATR], -1);
                                if W>0 then InlineShape.Width:= W;
                              end;
                        finally
                          Find := nil;
                        end;
                      end
                    else
                      begin
                        FResultFind.Execute(EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
                          wdFindContinue, EmptyParam, '', wdReplaceOne, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
                      end;
                  end
                else
                  begin
                    ProcessTextTag(Doc.Root.ChildNodes[0], Value);
                    TagText := ConvertValue(Value, ftString);
                    if 254 < length(TagText) then
                      begin
                        try
                          ClipboardError := not DeClipboard.Push;
                        except
                          on E: Exception do
                            begin
                              ClipboardError := True;
                              {$IFDEF DEBUG}
                              DebugLog('TMicrosoftWordReport.ProcessMakeTag clipboard push error: ' + E.Message);
                              {$ENDIF}
                            end;
                        end;
                        DeClipboard.Open;
                        DeClipboard.AsText := TagText;
                        DeClipboard.Close;
                        FResultFind.Execute(EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
                          EmptyParam, EmptyParam, '^c', wdReplaceOne, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
                        try
                          if not ClipboardError then ClipboardError := not DeClipboard.Pop;
                        except
                          on E: Exception do
                            begin
                              ClipboardError := True;
                              {$IFDEF DEBUG}
                              DebugLog('TMicrosoftWordReport.ProcessMakeTag clipboard pop error: ' + E.Message);
                              {$ENDIF}
                            end;
                        end;
                        if ClipboardError then DeClipboard.Clear;
                      end
                    else
                      begin
                        TagText := StringReplace(TagText, '^', '^^', [rfReplaceAll]);
                        TagText := StringReplace(TagText, '\', '^0092', [rfReplaceAll]);
                        FResultFind.Execute(EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
                          wdFindContinue, EmptyParam, TagText, wdReplaceOne, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
                      end;
                  end;
              end;
            Delete(Text, 1, Position);
            Position := Pos('<', Text);
            Doc.Free;
          end;
      end;
  finally
    SendMessage(Application.MainForm.Handle, DM_PROGRESSNOTIFY, -1, -1);
  end;
  Result := True;
end;

procedure TMicrosoftWordReport.Quit;
begin
  FWordApplication.Quit(False, EmptyParam, EmptyParam);
end;

procedure TMicrosoftWordReport.doSaveToFile(const aFileName: string);
begin
  FResultDocument.SaveAs(aFileName, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
    EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TMicrosoftWordReport.doPrint(const PrinterName: string; const CopyCount: Integer; const SplitCopies: Boolean);
var
  Alerts: Word_TLB.WdAlertLevel;
begin
  // открыть файл шаблона
  DoOpenTemplate;
  Alerts := FWordApplication.DisplayAlerts;
  if Alerts <> wdAlertsNone then FWordApplication.DisplayAlerts := wdAlertsNone;

  if DeTemplate.Root.Count = 0 then
    begin
      doLoadTemplate;
      doOpenDoc;
    end;
  if DeTemplate.Root.Count = 0 then Exit; //Error!

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
      DoShowDoc else
    if Pos(':\', PrinterName) = 2 then
      DoSaveToFile(PrinterName) else
    if Sametext(PrinterName, 'pdf') then
      FResultDocument.ExportAsFixedFormat(
        ExtractFilePath(TargetFileName)+ChangeFileExt(ExtractFileName(TargetFileName),'.pdf'), wdExportFormatPDF,
        False, wdExportOptimizeForPrint, wdExportAllDocument, 1, 32000, wdExportDocumentContent, True, True,
        wdExportCreateNoBookmarks, True, True, False, EmptyParam)
    else
      DoPrintDoc(PrinterName, CopyCount, SplitCopies);
    FWordApplication.DisplayAlerts := Alerts;
    if PrinterName <> EmptyStr then Quit;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TMicrosoftExcelReport.doPrint(%s, %d, %s) error: %s', [QuotedStr(PrinterName), CopyCount,
          StrPas(BooleanNames[SplitCopies]), E.Message]);
        {$ENDIF}
        Quit;
      end;
  end;
  DoClose;
end;

procedure TMicrosoftWordReport.UpdateResultFind;
begin
  FResultFind := FResultDocument.Range(EmptyParam, EmptyParam).Find;
  FResultFind.ClearFormatting;
  FResultFind.Forward := True;
  FResultFind.Text := '[<]*[/][>]';
  FResultFind.Replacement.Text := EmptyStr;
  FResultFind.MatchWildcards := True;
  FResultFind.Wrap := wdFindContinue;
  FResultFind.Font.Hidden := 0; //False;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DeMicrosoftOfficeReport unit initialization ...');

finalization
  DebugLog('DeMicrosoftOfficeReport unit finalization ...');
{$ENDIF}

end.


