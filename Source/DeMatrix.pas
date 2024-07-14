unit DeMatrix;

interface

uses Classes, Contnrs;

type
  { матрица смежности }
  TMatrixArray = array of array of boolean;
  TCompareEvent = function (const Index1, Index2: Integer): Integer of object;

  TContiguityMatrix = class
  private
    FSize      : integer;
    FMatrix    : TMatrixArray;
    FOnCompare : TCompareEvent;
    function GetLink(const FromIndex, ToIndex: Integer): Boolean;
    procedure SetLink(const FromIndex, ToIndex: Integer; const Value: Boolean);
    function GetIncLinkCount(const Index: Integer): Integer;
    function GetOutLinkCount(const Index: Integer): Integer;
    function CompareDirect(const Index1, Index2: Integer): Integer;
    function CompareReverse(const Index1, Index2: Integer): Integer;
    procedure Sort;
  protected
    function GetSize : integer;  virtual;
    procedure SetSize(const aSize : integer);  virtual;
  public
    destructor Destroy;  override;
    property Size : integer read GetSize write SetSize;
    property Link[const FromIndex, ToIndex: Integer]: Boolean read GetLink write SetLink;
    property OnCompare : TCompareEvent read FOnCompare write FOnCompare;
    property IncLinkCount[const Index: Integer]: Integer read GetIncLinkCount;
    property OutLinkCount[const Index: Integer]: Integer read GetOutLinkCount;
    procedure Exchange(const Index1, Index2: Integer);  virtual;
    procedure SortDirect;
    procedure SortReverse;
    procedure RemoveLink(const Index1, Index2: Integer);
    procedure AddLink(const Index1, Index2: Integer);
    procedure Move(const FromIndex, ToIndex: Integer);
    procedure CheckRings(aNodes : TList);
  end;

  TUniqueQueue = class(TQueue)
  protected
    procedure PushItem(AItem: Pointer);   override;
  public
    procedure Clear;
  end;


implementation

uses DeLog;

{ TContiguityMatrix }

destructor TContiguityMatrix.Destroy;
begin
  FMatrix := nil;
  inherited Destroy;
end;

function TContiguityMatrix.GetLink(const FromIndex, ToIndex: Integer): Boolean;
begin
  Result := FMatrix[FromIndex, ToIndex];
end;

procedure TContiguityMatrix.SetLink(const FromIndex, ToIndex: Integer; const Value: Boolean);
begin
  FMatrix[FromIndex, ToIndex] := Value;
end;

function TContiguityMatrix.GetIncLinkCount(const Index: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Pred(Size) do
    if Link[I, Index] then
      Inc(Result);
end;

function TContiguityMatrix.GetOutLinkCount(const Index: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Pred(Size) do
    if Link[Index, I] then
      Inc(Result);
end;

function TContiguityMatrix.CompareDirect(const Index1, Index2: Integer): Integer;
begin
  if Link[Index1, Index2] then
    Result := -1
  else if Link[Index2, Index1] then
    Result := 1
  else
    Result := 0;
end;

function TContiguityMatrix.CompareReverse(const Index1, Index2: Integer): Integer;
begin
  if Link[Index1, Index2] then
    Result := 1
  else if Link[Index2, Index1] then
    Result := -1
  else
    Result := 0;
end;

procedure TContiguityMatrix.Sort;
var I, J, Found : integer;
begin
  if Assigned(OnCompare) then
  begin
    I := 1;
    while I < Size do
    begin
      Found := -1;
      for J := I to Size-1 do
        if FOnCompare(I-1, J) > 0 then
          begin
            Found := J;
            break;
          end;
      if Found >= 0 then
        Exchange(I-1, Found)
      else
        inc(I);
    end;
  end;
end;

function TContiguityMatrix.GetSize : integer;
begin
  result := FSize;
end;

procedure TContiguityMatrix.SetSize(const aSize : integer);
begin
  if FSize <> aSize then
  begin
    FSize := aSize;
    SetLength(FMatrix, FSize, FSize);
  end;  
end;

procedure TContiguityMatrix.Exchange(const Index1, Index2: Integer);
var tmpValue  : boolean;
    I         : integer;
begin
  // переставляем колонки
  for I := 0 to Size-1 do
  begin
    tmpValue := FMatrix[I, Index1];
    FMatrix[I, Index1] := FMatrix[I, Index2];
    FMatrix[I, Index2] := tmpValue;
  end;
  // переставляем строки
  for I := 0 to Size-1 do
  begin
    tmpValue := FMatrix[Index1, I];
    FMatrix[Index1, I] := FMatrix[Index2, I];
    FMatrix[Index2, I] := tmpValue;
  end;
end;

procedure TContiguityMatrix.SortDirect;
begin
  FOnCompare := CompareDirect;
  Sort;
end;

procedure TContiguityMatrix.SortReverse;
begin
  FOnCompare := CompareReverse;
  Sort;
end;

procedure TContiguityMatrix.RemoveLink(const Index1, Index2: Integer);
begin
  FMatrix[Index1, Index2] := False;
end;

procedure TContiguityMatrix.AddLink(const Index1, Index2: Integer);
begin
  FMatrix[Index1, Index2] := True;
end;

procedure TContiguityMatrix.Move(const FromIndex, ToIndex: Integer);
var
  Index: Integer;
begin
  for Index := FromIndex to Pred(ToIndex) do
    Exchange(Index, Succ(Index));
end;

procedure TContiguityMatrix.CheckRings(aNodes : TList);
var I, K : integer;
    LocalVisits, GlobalVisits : TList;
    UQ : TUniqueQueue;

  function CheckGraph : boolean;
  var Node, J : integer;
  begin
    result := true;
    while UQ.Count > 0 do
    begin
      Node := integer(UQ.Pop);
      LocalVisits[Node] := pointer(integer(LocalVisits[Node]) + 1);
      if integer(LocalVisits[Node]) > 1 then
        result := false
      else
        for J := 0 to Size-1 do
          if Link[Node, J] then
            UQ.Push(pointer(J));
    end;
  end;

begin
  { возвращает список всех вершин, входящих в кольца, либо связанных с входящими в кольца }
  aNodes.Clear;
  LocalVisits := TList.Create;
  GlobalVisits := TList.Create;
  GlobalVisits.Count := Size;
  UQ := TUniqueQueue.Create;
  for I := 0 to Size-1 do
  begin
    LocalVisits.Clear;
    LocalVisits.Count := Size;
    UQ.Clear;
    if IncLinkCount[I] = 0 then
    begin
      UQ.Push(pointer(I));
      if not CheckGraph then
        for K := 0 to LocalVisits.Count-1 do
          if (NativeInt(LocalVisits[K]) > 0) and (aNodes.IndexOf(pointer(K)) < 0) then
            aNodes.Add(pointer(K));
      for K := 0 to LocalVisits.Count-1 do
        if (NativeInt(LocalVisits[K]) > 0) then
          GlobalVisits[K] := pointer(NativeInt(GlobalVisits[K]) + NativeInt(LocalVisits[K]));
    end;
  end;
  for I := 0 to Size-1 do
    if (IncLinkCount[I] > 0) and (NativeInt(GlobalVisits[I]) = 0) then
      aNodes.Add(pointer(I));
  LocalVisits.Free;
  GlobalVisits.Free;
  UQ.Free;
end;

{ TUniqueQueue }

procedure TUniqueQueue.PushItem(AItem: Pointer);
begin
  if List.IndexOf(aItem) < 0 then
    inherited PushItem(aItem);
end;

procedure TUniqueQueue.Clear;
begin
  List.Clear;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DeMatrix unit initialization ...');

finalization
  DebugLog('DeMatrix unit finalization ...');
{$ENDIF}

end.

