unit uTextTable;

interface

uses Classes, Contnrs;

type
  TTextTableColumns = class;

  TTextTableColumn = class
  private
    { Private declarations }
    FOwner: TTextTableColumns;
    FCaption: string;
    FWidth: Cardinal;
    FCaptionAlignment: TAlignment;
    FDataAlignment: TAlignment;
    FAutoSize: Boolean;
    FEllipsed: Boolean;
  public
    { Public declarations }
    constructor Create(AOwner: TTextTableColumns; const ACaption: string; const AWidth: Cardinal = 0; const ADataAlignment: TAlignment = taLeftJustify; const ACaptionAlignment: TAlignment = taLeftJustify);
    function TextCaption: string;
    property Caption: string read FCaption;
    property Width: Cardinal read FWidth;
    property CaptionAlignment: TAlignment read FCaptionAlignment;
    property DataAlignment: TAlignment read FDataAlignment;
    property AutoSize: Boolean read FAutoSize;
    property Ellipsed: Boolean read FEllipsed;
  end;

  TTextTable = class;

  TTextBorder =
    (
      tbTop,
      tbCenter,
      tbBottom
    );

  TTextTableColumns = class
  private
    { Private declarations }
    FList: TObjectList;
    FOwner: TTextTable;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TTextTableColumn;
  public
    { Public declarations }
    constructor Create(AOwner: TTextTable);
    destructor Destroy; override;
    function Add(const ACaption: string; const AWidth: Cardinal; const ADataAlignment, ACaptionAlignment: TAlignment): Integer;
    procedure Delete(const Index: Integer);
    procedure Clear;
    function TextBorder(const Border: TTextBorder): string;
    function AsText(const LeftSpaces: Cardinal = 0): string;
    property Owner: TTextTable read FOwner;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TTextTableColumn read GetItem; default;
  end;

  TTextTableLines = class;

  TTextTableValues = array of string;

  TTextTableLine = class
  private
    { Private declarations }
    FOwner: TTextTableLines;
    FValues: TTextTableValues;
    function GetCount: Integer;
    function GetItem(const Index: Integer): string;
    procedure SetItem(const Index: Integer; const Value: string);
    procedure Delete(const Index: Integer);
  public
    { Public declarations }
    constructor Create(AOwner: TTextTableLines);
    destructor Destroy; override;
    function TextData(const Col: Integer): string;
    function AsText: string;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: string read GetItem write SetItem; default;
  end;

  TTextTableLines = class
  private
    { Private declarations }
    FList: TObjectList;
    FOwner: TTextTable;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TTextTableLine;
    function GetValues(const Row, Col: Integer): string;
    procedure SetValues(const Row, Col: Integer; const Value: string);
  public
    { Public declarations }
    constructor Create(AOwner: TTextTable);
    destructor Destroy; override;
    function Add: Integer; overload;
    function Add(const Columns: array of Variant): Integer; overload;
    procedure Delete(const Index: Integer);
    procedure Clear;
    function AsText(const LeftSpaces: Cardinal = 0): string;
    property Owner: TTextTable read FOwner;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TTextTableLine read GetItem; default;
    property Values[const Row, Col: Integer]: string read GetValues write SetValues;
  end;

  TTextTableBorderSymbols = array[1..13] of Char;

  TTextTable = class
  private
    { Private declarations }
    FColumns: TTextTableColumns;
    FLines: TTextTableLines;
    FBorderSymbols: TTextTableBorderSymbols;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    function AsText(const LeftSpaces: Cardinal = 0): string;
    property Columns: TTextTableColumns read FColumns;
    property Lines: TTextTableLines read FLines;
  end;

implementation

uses SysUtils, StrUtils, Variants {$IFDEF DEBUG}, DeLog{$ENDIF};

{ TTextTableColumn }

constructor TTextTableColumn.Create(AOwner: TTextTableColumns; const ACaption: string; const AWidth: Cardinal; const ADataAlignment, ACaptionAlignment: TAlignment);
begin
  FOwner := AOwner;
  FCaption := ACaption;
  FWidth := AWidth;
  FCaptionAlignment := ACaptionAlignment;
  FDataAlignment := ADataAlignment;
  FAutoSize := FWidth = 0; // Считаем ширину автоматически!
  if FAutoSize then
    FWidth := Length(FCaption);
  FEllipsed := Length(FCaption) > FWidth;
end;

function TTextTableColumn.TextCaption: string;
var
  LeftSpace, RightSpace, Size: Integer;
  CaptionEllipsed: Boolean;
begin
  Result := FCaption;
  Size := Length(Result);
  CaptionEllipsed := Size > FWidth;
  if CaptionEllipsed then
    begin
      Result := Copy(Result, 1, FWidth);
      Size := Length(Result);
      FEllipsed := True;
    end;
  LeftSpace := (FWidth - Size) div 2;
  RightSpace := FWidth - Size - LeftSpace;
  case FCaptionAlignment of
    taLeftJustify: { Заголовок слева }
      begin
        RightSpace := RightSpace + LeftSpace;
        LeftSpace := 0;
      end;
    taRightJustify: { Заголовок справа }
      begin
        LeftSpace := LeftSpace + RightSpace;
        RightSpace := 0;
      end;
  end;
  if LeftSpace > 0 then
    Result := DupeString(' ', LeftSpace) + Result;
  if RightSpace > 0 then
    Result := Result + DupeString(' ', RightSpace);
  if CaptionEllipsed then
    Result := Result + '…'
  else
    if FEllipsed then
      Result := Result + ' ';
end;

{ TTextTableColumns }

constructor TTextTableColumns.Create(AOwner: TTextTable);
begin
  FList := TObjectList.Create;
  FOwner := AOwner;
  Assert(Assigned(FOwner));
end;

destructor TTextTableColumns.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TTextTableColumns.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TTextTableColumns.GetItem(const Index: Integer): TTextTableColumn;
begin
  Result := FList[Index] as TTextTableColumn;
end;

function TTextTableColumns.Add(const ACaption: string; const AWidth: Cardinal; const ADataAlignment, ACaptionAlignment: TAlignment): Integer;
var
  Column: TTextTableColumn;
begin
  Column := TTextTableColumn.Create(Self, ACaption, AWidth, ADataAlignment, ACaptionAlignment);
  try
    Result := FList.Add(Column);
  finally
    if Result = -1 then Column.Free;
  end;
end;

procedure TTextTableColumns.Delete(const Index: Integer);
var
  Row: Integer;
begin
  FList.Delete(Index);
  for Row := 0 to Pred(FOwner.Lines.Count) do
    FOwner.Lines[Row].Delete(Index);
end;

procedure TTextTableColumns.Clear;
begin
  FList.Clear;
end;

function TTextTableColumns.TextBorder(const Border: TTextBorder): string;
var
  Index, OffsetBorder: Integer;
begin
  Result := EmptyStr;
  case Border of
    tbTop: { Верхняя рамка }
      OffsetBorder := 1;
    tbCenter: { Разделительная рамка }
      OffsetBorder := 5;
    tbBottom: { Нижняя рамка }
      OffsetBorder := 9;
  end;
  for Index := 0 to Pred(Count) do
    begin
      if Index = 0 then
        Result := Result + FOwner.FBorderSymbols[OffsetBorder]
      else
        Result := Result + FOwner.FBorderSymbols[OffsetBorder + 2];
      Result := Result + DupeString(FOwner.FBorderSymbols[OffsetBorder + 1], Length(Items[Index].TextCaption) + 2);
      if Index = Pred(Count) then
        Result := Result + FOwner.FBorderSymbols[OffsetBorder + 3];
    end;
end;

function TTextTableColumns.AsText(const LeftSpaces: Cardinal): string;
var
  SpaceText: string;
  Index: Integer;
begin
  Result := EmptyStr;
  if LeftSpaces = 0 then
    SpaceText := EmptyStr
  else
    SpaceText := DupeString(' ', LeftSpaces);
  Result := SpaceText + TextBorder(tbTop) + #13#10 + SpaceText;
  for Index := 0 to Pred(Count) do
    begin
      Result := Result + FOwner.FBorderSymbols[13] + ' ' + Items[Index].TextCaption + ' ';
      if Index = Pred(Count) then
        Result := Result + FOwner.FBorderSymbols[13];
    end;
  Result := Result + #13#10 + SpaceText;
  if FOwner.Lines.Count <> 0 then
    Result := Result + TextBorder(tbCenter)
  else
    Result := Result + TextBorder(tbBottom);
end;

{ TTextTableLine }

constructor TTextTableLine.Create(AOwner: TTextTableLines);
begin
  FOwner := AOwner;
  Assert(Assigned(FOwner));
end;

destructor TTextTableLine.Destroy;
begin
  SetLength(FValues, 0);
  inherited Destroy;
end;

function TTextTableLine.GetCount: Integer;
begin
  Result := Length(FValues);
end;

function TTextTableLine.GetItem(const Index: Integer): string;
begin
  if (Index >= Low(FValues)) and (Index <= High(FValues)) then
    Result := FValues[Index]
  else
    Result := EmptyStr;
end;

procedure TTextTableLine.SetItem(const Index: Integer; const Value: string);
var
  NewValue: string;
begin
  Assert((Index >= 0) and (Index < FOwner.Owner.Columns.Count));
  while Index >= Length(FValues) do
    begin
      SetLength(FValues, Succ(Length(FValues)));
      FValues[High(FValues)] := EmptyStr;
    end;
  NewValue := ReplaceText(ReplaceText(ReplaceText(Value, #13, '¶'), #10, EmptyStr), #9, ' ');
  if FOwner.Owner.Columns[Index].AutoSize then
    begin
      if Length(NewValue) > FOwner.Owner.Columns[Index].Width then
        FOwner.Owner.Columns[Index].FWidth := Length(NewValue);
    end
  else
    if Length(NewValue) > FOwner.Owner.Columns[Index].Width then
      begin
        NewValue := Copy(NewValue, 1, Succ(FOwner.Owner.Columns[Index].Width));
        FOwner.Owner.Columns[Index].FEllipsed := True;
      end;
  FValues[Index] := NewValue;
end;

procedure TTextTableLine.Delete(const Index: Integer);
var
  Col: Integer;
begin
  if (Index >= Low(FValues)) and (Index <= High(FValues)) then
    begin
      if Index <> High(FValues) then
        for Col := Succ(Index) to High(FValues) do
          FValues[Pred(Col)] := FValues[Col];
      SetLength(FValues, Pred(Length(FValues)));
    end;
end;

function TTextTableLine.TextData(const Col: Integer): string;
var
  LeftSpace, RightSpace, Size: Integer;
  DataEllipsed: Boolean;
begin
  Result := Items[Col];
  Size := Length(Result);
  DataEllipsed := Size > FOwner.Owner.Columns[Col].Width;
  if DataEllipsed then
    begin
      Result := Copy(Result, 1, FOwner.Owner.Columns[Col].Width);
      Size := Length(Result);
    end;
  LeftSpace := (FOwner.Owner.Columns[Col].Width - Size) div 2;
  RightSpace := FOwner.Owner.Columns[Col].Width - Size - LeftSpace;
  case FOwner.Owner.Columns[Col].DataAlignment of
    taLeftJustify: { Данные слева }
      begin
        RightSpace := RightSpace + LeftSpace;
        LeftSpace := 0;
      end;
    taRightJustify: { Данные справа }
      begin
        LeftSpace := LeftSpace + RightSpace;
        RightSpace := 0;
      end;
  end;
  if LeftSpace > 0 then
    Result := DupeString(' ', LeftSpace) + Result;
  if RightSpace > 0 then
    Result := Result + DupeString(' ', RightSpace);
  if DataEllipsed then
    Result := Result + '…'
  else
    if FOwner.Owner.Columns[Col].Ellipsed then
      Result := Result + ' ';
end;

function TTextTableLine.AsText: string;
var
  Index: Integer;
begin
  Result := EmptyStr;
  for Index := 0 to Pred(FOwner.Owner.Columns.Count) do
    begin
      Result := Result + FOwner.Owner.FBorderSymbols[13] + ' ' + TextData(Index) + ' ';
      if Index = Pred(FOwner.Owner.Columns.Count) then
        Result := Result + FOwner.Owner.FBorderSymbols[13];
    end;
end;

{ TTextTableLines }

constructor TTextTableLines.Create(AOwner: TTextTable);
begin
  FList := TObjectList.Create;
  FOwner := AOwner;
  Assert(Assigned(FOwner));
end;

destructor TTextTableLines.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TTextTableLines.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TTextTableLines.GetItem(const Index: Integer): TTextTableLine;
var
  NewIndex: Integer;
begin
  Assert(Index >= 0);
  while Index >= Count do
    begin
      NewIndex := Add;
      Assert(NewIndex <> -1);
    end;
  Result := FList[Index] as TTextTableLine;
end;

function TTextTableLines.GetValues(const Row, Col: Integer): string;
begin
  Result := Items[Row][Col];
end;

procedure TTextTableLines.SetValues(const Row, Col: Integer; const Value: string);
begin
  Items[Row][Col] := Value;
end;

function TTextTableLines.Add: Integer;
var
  Line: TTextTableLine;
begin
  Line := TTextTableLine.Create(Self);
  try
    Result := FList.Add(Line);
  finally
    if Result = -1 then Line.Free;
  end;
end;

function TTextTableLines.Add(const Columns: array of Variant): Integer;
var
  Index: Integer;
begin
  Result := Add;
  if Result <> -1 then
    begin
      Assert(Length(Columns) <= FOwner.Columns.Count);
      for Index := Low(Columns) to High(Columns) do
        Values[Result, Index] := VarToStr(Columns[Index]);
    end;
end;

procedure TTextTableLines.Delete(const Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TTextTableLines.Clear;
begin
  FList.Clear;
end;

function TTextTableLines.AsText(const LeftSpaces: Cardinal): string;
var
  SpaceText: string;
  Index: Integer;
begin
  Result := EmptyStr;
  if LeftSpaces = 0 then
    SpaceText := EmptyStr
  else
    SpaceText := DupeString(' ', LeftSpaces);
  for Index := 0 to Pred(Count) do
    Result := Result + SpaceText + Items[Index].AsText + #13#10;
end;

{ TTextTable }

constructor TTextTable.Create;
begin
  FColumns := TTextTableColumns.Create(Self);
  FLines := TTextTableLines.Create(Self);
  //                 1234567890123
  FBorderSymbols := '┌─┬┐├─┼┤└─┴┘│';
end;

destructor TTextTable.Destroy;
begin
  FLines.Free;
  FColumns.Free;
  inherited Destroy;
end;

function TTextTable.AsText(const LeftSpaces: Cardinal): string;
var
  SpaceText: string;
begin
  Result := EmptyStr;
  if LeftSpaces = 0 then
    SpaceText := EmptyStr
  else
    SpaceText := DupeString(' ', LeftSpaces);
  Result := Lines.AsText(LeftSpaces);
  if Length(Result) = 0 then
    Result := Columns.AsText(LeftSpaces)
  else
    Result := Columns.AsText(LeftSpaces) + #13#10 + Result + SpaceText + Columns.TextBorder(tbBottom);
end;

{$IFDEF DEBUG}
initialization
  DebugLog('uTextTable unit initialization ...');

finalization
  DebugLog('uTextTable unit finalization ...');
{$ENDIF}

end.

