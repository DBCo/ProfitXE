{$WARN SYMBOL_PLATFORM OFF}

unit uIconManager;

interface

uses Contnrs, Graphics, ImgList, uIconUtils;

type
  TStackIcon = class
  private
    FIcon: TIcon;
    FWidth: Integer;
    FHeight: Integer;
    FInitialized: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Assign(Source: TStackIcon): Boolean;
    function CreateEmptyIcon(ImageList: TCustomImageList): Boolean;
    function LoadIcon(const IconIndex: Integer; ImageList: TCustomImageList): Boolean;
    property Icon: TIcon read FIcon;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Initialized: Boolean read FInitialized;
  end;

  TIconList = class;

  TStackIcons = class
  private
    FList: TObjectList;
    FIconIndex: TIconIndex;
    FSchemaColor: TSchemaColor;
    FInversed: Boolean;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TStackIcon;
  public
    constructor Create;
    destructor Destroy; override;
    function Assign(Source: TStackIcons): Boolean;
    function Find(const Width, Height: Integer; const FromIndex: Integer = 0; const Approx: boolean = False): TStackIcon;
    function CreateEmptyIcons(IconList: TIconList): Integer;
    function LoadIcons(const IconIndex: Integer; IconList: TIconList): Integer;
    function CloneColorStackIcons(const TargetColor: TColor): TStackIcons;
    function CloneLightStackIcons: TStackIcons;
    function CloneInvertStackIcons: TStackIcons;
    function CloneOverlayStackIcons(const Position: TOverlayIconPosition; BaseStackIcons, OverlayStackIcons: TStackIcons): TStackIcons;
    function ExportToFile(const FileName: string): Boolean;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TStackIcon read GetItem; default;
    property IconIndex: TIconIndex read FIconIndex;
    property SchemaColor: TSchemaColor read FSchemaColor;
    property Inversed: Boolean read FInversed;
    {$IFDEF DEBUG}
    procedure DebugStackLog(const Text: string);
    {$ENDIF}
  end;

  TIconList = class
  private
    FList: TComponentList;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TCustomImageList;
  public
    constructor Create(const AImageLists: array of TCustomImageList);
    destructor Destroy; override;
    function Find(const Width, Height: Integer; const FromIndex: Integer = 0): TCustomImageList;
    function LoadIconFromResource(const ResourceName: string): Integer;
    function RemoveIcon(const RealIndex: Integer): Integer;
    function LoadColorIcon(const RealIndex: Integer; SourceList: TIconList; const TargetColor: TColor): Integer;
    function LoadLightIcon(const RealIndex: Integer; SourceList: TIconList): Integer;
    function CopyInvertIcon(const RealIndex: Integer; SourceList: TIconList): Integer;
    function CreateStackIcons(const RealIndex: Integer): TStackIcons;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TCustomImageList read GetItem; default;
  end;

  TMapItem = class
  private
    FRealIndex: Integer;
    FLogicalIndex: Integer;
  public
    constructor Create(const ALogicalIndex, ARealIndex: Integer);
    property RealIndex: Integer read FRealIndex;
    property LogicalIndex: Integer read FLogicalIndex;
  end;

  TMapList = class
  private
    FList: TObjectList;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TMapItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Find(const LogicalIndex: Integer; var MapIndex: Integer): Boolean;
    function Map(const LogicalIndex, RealIndex: Integer): Integer;
    function UnMap(const RealIndex: Integer): Integer;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TMapItem read GetItem; default;
  end;

  TIconManager = class
  private
    FMapList: TMapList;
    FNormalLists: TIconList;
    FHotLists: TIconList;
    FDisabledLists: TIconList;
    FSchemaGUID: TGUID;
    function LoadIconFromResource(const ResourceName: string; const LogicalIndex: Integer): Integer;
    function CopyShemaIcon(const RealIndex: Integer; const SchemaColor: TSchemaColor): Integer;
    function CopyInvertIcon(const RealIndex: Integer): Integer;
    function BuildStackIcons(const IconIndex: Word; const Inversed: Boolean; IconList: TIconList): TStackIcons;
    function ReadStackIcons(StackIcons: TStackIcons; IconList: TIconList): Integer;
    function CalculareGUID: TGUID;
  public
    constructor Create(const ANormalLists, AHotLists, ADisabledLists: array of TCustomImageList; const ASchemaColors: TSchemaColorArray);
    destructor Destroy; override;
    function LoadIcons: Integer;
    function CreateStackIcons(const LogicalIndex: Integer): TStackIcons;
    function MapIcon(const LogicalIndex: Integer): Integer;
    function UnMapIcon(const RealIndex: Integer): Integer;
    function ExportToFile(const FileName: string; const LogicalIndex: Integer): Boolean;
    property MapList: TMapList read FMapList;
    property SchemaGUID: TGUID read FSchemaGUID;
  end;

implementation

uses Windows, SysUtils, StrUtils, Classes, MD5, DeLog
     {$IFDEF DEBUG}, uTextTable{$ENDIF};

{ TStackIcon }

constructor TStackIcon.Create;
begin
  FIcon := TIcon.Create;
end;

destructor TStackIcon.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

function TStackIcon.Assign(Source: TStackIcon): Boolean;
begin
  Result := Assigned(Source);
  if Result then
    begin
      FWidth := Source.Width;
      FHeight := Source.Height;
      FIcon.Assign(Source.Icon);
      FInitialized := Source.Initialized;
    end;
end;

function TStackIcon.CreateEmptyIcon(ImageList: TCustomImageList): Boolean;
const
  cEmptyIconResource = 'P__';
begin
  Result := Assigned(ImageList)and (FindResource(hInstance, cEmptyIconResource, RT_GROUP_ICON) <> 0);
  if Result then
    begin
      FWidth := ImageList.Width;
      FHeight := ImageList.Height;
      FIcon.Handle := LoadImage(hInstance, cEmptyIconResource, IMAGE_ICON, ImageList.Width, ImageList.Height, LR_SHARED);
      FInitialized := True;
    end;
end;

function TStackIcon.LoadIcon(const IconIndex: Integer; ImageList: TCustomImageList): Boolean;
begin
  if IconIndex = -1 then
    Result := CreateEmptyIcon(ImageList)
  else
    begin
      Result := Assigned(ImageList) and (IconIndex >= 0) and (IconIndex < ImageList.Count);
      if Result then
        begin
          FWidth := ImageList.Width;
          FHeight := ImageList.Height;
          ImageList.GetIcon(IconIndex, FIcon);
          FInitialized := True;
        end;
    end;
end;

{ TStackIcons }

constructor TStackIcons.Create;
begin
  FList := TObjectList.Create;
end;

destructor TStackIcons.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TStackIcons.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TStackIcons.GetItem(const Index: Integer): TStackIcon;
begin
  Result := FList[Index] as TStackIcon;
end;

function TStackIcons.Assign(Source: TStackIcons): Boolean;
resourcestring
  sAssignErrorFmt = 'Assign icon index %d to stack error.';
var
  Index: Integer;
  Stack: TStackIcon;
begin
  Result := Assigned(Source);
  if Result then
    begin
      FList.Clear;
      FIconIndex := Source.IconIndex;
      FSchemaColor := Source.SchemaColor;
      FInversed := Source.Inversed;
      for Index := 0 to Pred(Source.Count) do
        begin
          Stack := TStackIcon.Create;
          try
            Stack.Assign(Source[Index]);
            if FList.Add(Stack) = -1 then
              raise Exception.CreateResFmt(@sAssignErrorFmt, [Index]);
          except
            Stack.Free;
            raise;
          end;
        end;
    end;
end;

function TStackIcons.Find(const Width, Height: Integer; const FromIndex: Integer = 0; const Approx: boolean = False): TStackIcon;
var
  Index: Integer;
begin
  Result := nil;
  for Index := FromIndex to Pred(Count) do
    if (Items[Index].Width = Width) and (Items[Index].Height = Height) then
      begin
        Result := Items[Index];
        Break;
      end else
    if Approx and (Items[Index].Width < Width) and (Items[Index].Height < Height) and
      ( (Result = nil) or ((Result.Width < Items[Index].Width) and (Result.Height < Items[Index].Height)) ) then
      begin
        Result := Items[Index];
      end;
end;

function TStackIcons.CreateEmptyIcons(IconList: TIconList): Integer;
resourcestring
  sAppendError = 'Create empty icon to stack error.';
var
  Index: Integer;
  Stack: TStackIcon;
begin
  Result := 0;
  FList.Clear;
  FIconIndex := 0;
  FSchemaColor := 0;
  FInversed := False;
  if Assigned(IconList) and (IconList.Count <> 0) then
    for Index := 0 to Pred(IconList.Count) do
      begin
        Stack := TStackIcon.Create;
        try
         Stack.CreateEmptyIcon(IconList[Index]);
         if FList.Add(Stack) <> -1 then
           Inc(Result)
         else
           raise Exception.CreateRes(@sAppendError);
        except
          Stack.Free;
          raise;
        end;
      end;
end;

function TStackIcons.LoadIcons(const IconIndex: Integer; IconList: TIconList): Integer;
resourcestring
  sAppendErrorFmt = 'Load icon %d to stack error.';
var
  Index: Integer;
  Stack: TStackIcon;
begin
  Result := 0;
  FList.Clear;
  if Assigned(IconList) and (IconList.Count <> 0) then
    for Index := 0 to Pred(IconList.Count) do
      begin
        Stack := TStackIcon.Create;
        try
         Stack.LoadIcon(IconIndex, IconList[Index]);
         if FList.Add(Stack) <> -1 then
           Inc(Result)
         else
           raise Exception.CreateResFmt(@sAppendErrorFmt, [IconIndex]);
        except
          Stack.Free;
          raise;
        end;
      end;
end;

function TStackIcons.CloneColorStackIcons(const TargetColor: TColor): TStackIcons;
resourcestring
  sCloneColorErrorFmt = 'Clone colored icon index %d to stack error.';
var
  Index: Integer;
  Stack: TStackIcon;
begin
  Result := TStackIcons.Create;
  try
    Result.FIconIndex := IconIndex;
    Result.FSchemaColor := SchemaColor;
    Result.FInversed := Inversed;
    for Index := 0 to Pred(Count) do
      begin
        Stack := TStackIcon.Create;
        try
          Stack.FWidth := Items[Index].Width;
          Stack.FHeight := Items[Index].Height;
          Stack.FInitialized := UpdateColorIcon(Items[Index].Icon, Stack.FIcon, TargetColor);
          if Result.FList.Add(Stack) = -1 then
            raise Exception.CreateResFmt(@sCloneColorErrorFmt, [Index]);
        except
          Stack.Free;
          raise;
        end;
      end;
  except
    Result.Free;
    raise;
  end;
end;

function TStackIcons.CloneLightStackIcons: TStackIcons;
resourcestring
  sCloneLightErrorFmt = 'Clone lighted icon index %d to stack error.';
var
  Index: Integer;
  Stack: TStackIcon;
begin
  Result := TStackIcons.Create;
  try
    for Index := 0 to Pred(Count) do
      begin
        Stack := TStackIcon.Create;
        try
          Stack.FWidth := Items[Index].Width;
          Stack.FHeight := Items[Index].Height;
          Stack.FInitialized := UpdateColorIcon(Items[Index].Icon, Stack.FIcon, clFocused);
          if Result.FList.Add(Stack) = -1 then
            raise Exception.CreateResFmt(@sCloneLightErrorFmt, [Index]);
        except
          Stack.Free;
          raise;
        end;
      end;
  except
    Result.Free;
    raise;
  end;
end;

function TStackIcons.CloneInvertStackIcons: TStackIcons;
resourcestring
  sCloneInvertErrorFmt = 'Clone inverted icon index %d to stack error.';
var
  Index: Integer;
  Stack: TStackIcon;
begin
  Result := TStackIcons.Create;
  try
    Result.FIconIndex := IconIndex;
    Result.FSchemaColor := SchemaColor;
    Result.FInversed := True;
    for Index := 0 to Pred(Count) do
      begin
        Stack := TStackIcon.Create;
        try
          Stack.FWidth := Items[Index].Width;
          Stack.FHeight := Items[Index].Height;
          Stack.FInitialized := UpdateInvertIcon(Items[Index].Icon, Stack.FIcon);
          if Result.FList.Add(Stack) = -1 then
            raise Exception.CreateResFmt(@sCloneInvertErrorFmt, [Index]);
        except
          Stack.Free;
          raise;
        end;
      end;
  except
    Result.Free;
    raise;
  end;
end;

function TStackIcons.CloneOverlayStackIcons(const Position: TOverlayIconPosition; BaseStackIcons, OverlayStackIcons: TStackIcons): TStackIcons;
resourcestring
  sCloneOverlayErrorFmt = 'Clone overlay icon index %d to stack error.';
var
  Index, SSize, OSize: Integer;
  Stack, BaseStack, OverStack, HalfStack: TStackIcon;
  HalfDestroying: Boolean;
begin
  Result := TStackIcons.Create;
  try
    if Assigned(OverlayStackIcons) then
      for Index := 0 to Pred(Count) do
        begin
          Stack := TStackIcon.Create;
          try
            Stack.FWidth := Items[Index].Width;
            Stack.FHeight := Items[Index].Height;
            // Если наложить две иконки одинакового размера, то ...
            SSize:= SourceSize(Stack.Width, Stack.Height, Position);
            BaseStack:= BaseStackIcons.Find(SSize, SSize, 0, True);
            OSize:= OverlaySize(Stack.Width, Stack.Height, Position);
            OverStack := OverlayStackIcons.Find(OSize, OSize, 0, True);
                   (*
            if (Position <> 0) and not Assigned(OverStack) then
              begin
                {$IFDEF DEBUG}
                OverlayStackIcons.DebugStackLog('Before half size icon ...');
                {$ENDIF}
                OverStack := OverlayStackIcons.Find(Stack.Width, Stack.Height);
                if Assigned(OverStack) then
                  begin
                    {$IFDEF DEBUG}
                    DebugLog('Creating half size icon %dx%d ...', [Stack.Width, Stack.Height]);
                    {$ENDIF}
                    HalfDestroying := True;
                    HalfStack := TStackIcon.Create;
                    try
                      HalfStack.FWidth := Stack.Width div 2;
                      HalfStack.FHeight := Stack.Height div 2;
                      HalfStack.FInitialized := UpdateHalfSizeIcon(OverStack.FIcon, HalfStack.FIcon);
                      HalfDestroying := OverlayStackIcons.FList.Add(HalfStack) = -1;
                    finally
                      if HalfDestroying then FreeAndNil(HalfStack);
                    end;
                    OverStack := HalfStack;
                  end;
                {$IFDEF DEBUG}
                OverlayStackIcons.DebugStackLog('After half size icon ...');
                {$ENDIF}
              end;  *)
            if Assigned(OverStack) and (Stack.FWidth >=16 ) then
              begin
                Stack.FInitialized := UpdateOverlayIcon(Stack.Width, Stack.Height, BaseStack.Icon, OverStack.Icon, Stack.FIcon, Position)
              end
            else
              begin
                Stack.FIcon.Assign(Items[Index].Icon); // Здесь возможно надо брать OverlayIndex ...
                Stack.FInitialized := Items[Index].Initialized;
              end;
            if Result.FList.Add(Stack) = -1 then
              raise Exception.CreateResFmt(@sCloneOverlayErrorFmt, [Index]);
          except
            Stack.Free;
            raise;
          end;
        end;
  except
    Result.Free;
    raise;
  end;
end;

{$IFDEF DEBUG}
procedure TStackIcons.DebugStackLog(const Text: string);
  function PrepareStackLog: string;
  const
    SchemaColors: array[TSchemaColor] of PChar =
      (
        'None',
        'Black', // Disabled
        'White', // Highligth
        'Gray',  // GrayScale
        '30°',
        '60°',
        '90°',
        '120°',
        '150°',
        '180°',
        '210°',
        '240°',
        '270°',
        '300°',
        '330°',
        '360°'
      );
    InverseNames: array[Boolean] of PChar = ('No', 'Yes');
    BoolNames: array[Boolean] of Char = ('N', 'Y');
  var
    TextTable: TTextTable;
    Index: Integer;
    StackIcon: TStackIcon;
  begin
    if Count = 0 then
      Result := EmptyStr
    else
      begin
        TextTable := TTextTable.Create;
        try
          TextTable.Columns.Add('No', 0, taRightJustify, taCenter);
          TextTable.Columns.Add('Width', 0, taRightJustify, taLeftJustify);
          TextTable.Columns.Add('Height', 0, taRightJustify, taLeftJustify);
          TextTable.Columns.Add('I', 1, taCenter, taCenter);
          TextTable.Columns.Add('Icon', 0, taRightJustify, taLeftJustify);
          for Index := 0 to Pred(Count) do
            begin
              TextTable.Lines[Index][0] := IntToStr(Succ(Index));
              StackIcon := Items[Index];
              if Assigned(StackIcon) then
                begin
                  TextTable.Lines[Index][1] := IntToStr(StackIcon.Width);
                  TextTable.Lines[Index][2] := IntToStr(StackIcon.Height);
                  TextTable.Lines[Index][3] := BoolNames[StackIcon.Initialized];
                  if not StackIcon.Icon.Empty then
                    TextTable.Lines[Index][4] := Format('%dx%d', [StackIcon.Icon.Width, StackIcon.Icon.Height]);
                end;
            end;
          Result := #13#10 + TextTable.AsText(24);
        finally
          TextTable.Free;
        end;
      end;
    Result :=
      Format(#13#10'                        Logical number: %.3u (%.2xh)', [IconIndex, IconIndex]) +
      Format(#13#10'                        Schema color:   %.3u (%.2xh) - %s', [SchemaColor, SchemaColor, StrPas(SchemaColors[SchemaColor])]) +
      Format(#13#10'                        Inversed:       %s', [StrPas(InverseNames[Inversed])]) +
      Result;
  end;
begin
  DebugLog(Text + PrepareStackLog);
end;
{$ENDIF}

function CustomStackIconsSort(Item1, Item2: Pointer): Integer;
var
  StackIcon1, StackIcon2: TStackIcon;
begin
  // Сначала "БОЛЬШИЕ", а потом "МАЛЕНЬКИЕ" ...
  StackIcon1 := TStackIcon(Item1);
  StackIcon2 := TStackIcon(Item2);
  if StackIcon1.Width > StackIcon2.Width then
    Result := -1
  else if StackIcon1.Width < StackIcon2.Width then
    Result := 1
  else if StackIcon1.Height > StackIcon2.Height then
    Result := -1
  else if StackIcon1.Height < StackIcon2.Height then
    Result := 1
  else
    Result := 0;
end;

function TStackIcons.ExportToFile(const FileName: string): Boolean;
type
  TIconFileHeader = packed record
    Reserved: Word;
    ImageType: Word;
    Count: Word;
  end;
  TIconFileRecord = packed record
    Width: Byte;
    Height: Byte;
    Colors: Byte;
    Reserved: Byte;
    Planes: Word;
    BPP: Word;
    Size: Cardinal;
    Offset: Cardinal;
  end;
var
  List: TObjectList;
  TargetStream: TStream;
  FileHeader: TIconFileHeader;
  FileRecord: TIconFileRecord;
  FreePosition: Int64;
  MemoryStream: TMemoryStream;
  Index: Integer;
begin
  Result := Count <> 0;
  if Result then
    begin
      List := TObjectList.Create(False);
      try
        for Index := 0 to Pred(Count) do
          if Items[Index].Initialized and Assigned(Items[Index].Icon) and not Items[Index].Icon.Empty then
            List.Add(Items[Index]);
        List.Sort(CustomStackIconsSort);
        Result := List.Count <> 0;
        if Result then
          begin
            TargetStream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
            try
              FileHeader.Reserved := 0;
              FileHeader.ImageType := RC3_ICON;
              FileHeader.Count := List.Count;
              TargetStream.WriteBuffer(FileHeader, SizeOf(FileHeader));
              ZeroMemory(@FileRecord, SizeOf(FileRecord));
              for Index := 1 to List.Count do
                TargetStream.WriteBuffer(FileRecord, SizeOf(FileRecord));
              FreePosition := TargetStream.Position;
              MemoryStream := TMemoryStream.Create;
              try
                for Index := 0 to Pred(List.Count) do
                  begin
                    TargetStream.Seek(SizeOf(FileHeader) + (Index * SizeOf(FileRecord)), soFromBeginning);
                    (List[Index] as TStackIcon).Icon.SaveToStream(MemoryStream);
                    MemoryStream.Seek(SizeOf(FileHeader), soFromBeginning);
                    MemoryStream.ReadBuffer(FileRecord, SizeOf(FileRecord));
                    MemoryStream.Seek(FileRecord.Offset, soFromBeginning);
                    FileRecord.Offset := FreePosition;
                    TargetStream.WriteBuffer(FileRecord, SizeOf(FileRecord));
                    TargetStream.Seek(FileRecord.Offset, soFromBeginning);
                    TargetStream.CopyFrom(MemoryStream, FileRecord.Size);
                    FreePosition := TargetStream.Position;
                    MemoryStream.Size := 0;
                  end;
              finally
                MemoryStream.Free;
              end;
            finally
              TargetStream.Free;
            end;
          end;
      finally
        List.Free;
      end;
    end;
end;

{ TIconList }

constructor TIconList.Create(const AImageLists: array of TCustomImageList);
var
  Index: Integer;
  ImageList: TCustomImageList;
begin
  FList := TComponentList.Create(False);
  for Index := Low(AImageLists) to High(AImageLists) do
    begin
      ImageList := AImageLists[Index];
      if Assigned(ImageList) then FList.Add(ImageList);
    end;
end;

destructor TIconList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TIconList.Find(const Width, Height, FromIndex: Integer): TCustomImageList;
var
  Index: Integer;
  ImageList: TCustomImageList;
begin
  Result := nil;
  Assert(FromIndex >= 0, 'Define low index error!');
  for Index := FromIndex to Pred(Count) do
    begin
      ImageList := Items[Index];
      if Assigned(ImageList) then
        if (ImageList.Width = Width) and (ImageList.Height = Height) then
          begin
            Result := ImageList;
            Break;
          end;
    end;
end;

function TIconList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TIconList.GetItem(const Index: Integer): TCustomImageList;
begin
  Result := FList[Index] as TCustomImageList;
end;

function TIconList.LoadIconFromResource(const ResourceName: string): Integer;
var
  Index, NewIndex: Integer;
  ImageList: TCustomImageList;
  Icon: TIcon;
begin
  Result := -1;
  for Index := 0 to Pred(Count) do
    begin
      NewIndex := -1;
      ImageList := Items[Index];
      if Assigned(ImageList) then
        begin
          Icon := TIcon.Create;
          try
            Icon.Handle := LoadImage(hInstance, PChar(ResourceName), IMAGE_ICON, ImageList.Width, ImageList.Height, LR_SHARED);
            if Icon.Handle <> 0 then
              NewIndex := ImageList.AddIcon(Icon);
          finally
            Icon.Free;
          end;
        end;
      if Index = 0 then
        if NewIndex = -1 then
          Break
        else
          Result := NewIndex
      else
        if Result <> NewIndex then
          begin
            if Assigned(ImageList) and (NewIndex <> -1) then ImageList.Delete(NewIndex);
            for NewIndex := Pred(Index) downto 0 do
              begin
                ImageList := Items[NewIndex];
                if Assigned(ImageList) then ImageList.Delete(Result);
              end;
            Result := -1; // Рассинхронизация списка иконок!!!
            Break;
          end;
    end;
end;

function TIconList.RemoveIcon(const RealIndex: Integer): Integer;
var
  Index: Integer;
  ImageList: TCustomImageList;
begin
  Result := 0;
  if RealIndex >= 0 then
    for Index := 0 to Pred(Count) do
      begin
        ImageList := Items[Index];
        if Assigned(ImageList) and (RealIndex < ImageList.Count) then
          begin
            ImageList.Delete(RealIndex);
            Inc(Result);
          end;
      end;
end;

function TIconList.LoadColorIcon(const RealIndex: Integer; SourceList: TIconList; const TargetColor: TColor): Integer;
var
  Index, NewIndex: Integer;
  TargetImageList, SourceImageList: TCustomImageList;
  SourceIcon, TargetIcon: TIcon;
begin
  Result := -1;
  if Assigned(SourceList) and (RealIndex >= 0) then
    for Index := 0 to Pred(Count) do
      begin
        NewIndex := -1;
        TargetImageList := Items[Index];
        if Assigned(TargetImageList) then
          begin
            SourceImageList := SourceList.Find(TargetImageList.Width, TargetImageList.Height);
            if Assigned(SourceImageList) and (RealIndex < SourceImageList.Count) then
              begin
                SourceIcon := TIcon.Create;
                try
                  SourceImageList.GetIcon(RealIndex, SourceIcon);
                  TargetIcon := TIcon.Create;
                  try
                    if UpdateColorIcon(SourceIcon, TargetIcon, TargetColor) then
                      NewIndex := TargetImageList.AddIcon(TargetIcon);
                  finally
                    TargetIcon.Free;
                  end;
                finally
                  SourceIcon.Free;
                end;
              end;
          end;
        if Index = 0 then
          if NewIndex = -1 then
            Break
          else
            Result := NewIndex
        else
          if Result <> NewIndex then
            begin
              if Assigned(TargetImageList) and (NewIndex <> -1) then TargetImageList.Delete(NewIndex);
              for NewIndex := Pred(Index) downto 0 do
                begin
                  TargetImageList := Items[NewIndex];
                  if Assigned(TargetImageList) then TargetImageList.Delete(Result);
                end;
              Result := -1; // Рассинхронизация списка иконок!!!
              Break;
            end;
      end;
end;

function TIconList.LoadLightIcon(const RealIndex: Integer; SourceList: TIconList): Integer;
var
  Index, NewIndex: Integer;
  TargetImageList, SourceImageList: TCustomImageList;
  SourceIcon, TargetIcon: TIcon;
begin
  Result := -1;
  if Assigned(SourceList) and (RealIndex >= 0) then
    for Index := 0 to Pred(Count) do
      begin
        NewIndex := -1;
        TargetImageList := Items[Index];
        if Assigned(TargetImageList) then
          begin
            SourceImageList := SourceList.Find(TargetImageList.Width, TargetImageList.Height);
            if Assigned(SourceImageList) and (RealIndex < SourceImageList.Count) then
              begin
                SourceIcon := TIcon.Create;
                try
                  SourceImageList.GetIcon(RealIndex, SourceIcon);
                  TargetIcon := TIcon.Create;
                  try
                    if UpdateColorIcon(SourceIcon, TargetIcon, clFocused) then
                      NewIndex := TargetImageList.AddIcon(TargetIcon);
                  finally
                    TargetIcon.Free;
                  end;
                finally
                  SourceIcon.Free;
                end;
              end;
          end;
        if Index = 0 then
          if NewIndex = -1 then
            Break
          else
            Result := NewIndex
        else
          if Result <> NewIndex then
            begin
              if Assigned(TargetImageList) and (NewIndex <> -1) then TargetImageList.Delete(NewIndex);
              for NewIndex := Pred(Index) downto 0 do
                begin
                  TargetImageList := Items[NewIndex];
                  if Assigned(TargetImageList) then TargetImageList.Delete(Result);
                end;
              Result := -1; // Рассинхронизация списка иконок!!!
              Break;
            end;
      end;
end;

function TIconList.CopyInvertIcon(const RealIndex: Integer; SourceList: TIconList): Integer;
var
  Index, NewIndex: Integer;
  TargetImageList, SourceImageList: TCustomImageList;
  SourceIcon, TargetIcon: TIcon;
begin
  Result := -1;
  if Assigned(SourceList) and (RealIndex >= 0) then
    for Index := 0 to Pred(Count) do
      begin
        NewIndex := -1;
        TargetImageList := Items[Index];
        if Assigned(TargetImageList) then
          begin
            SourceImageList := SourceList.Find(TargetImageList.Width, TargetImageList.Height);
            if Assigned(SourceImageList) and (RealIndex < SourceImageList.Count) then
              begin
                SourceIcon := TIcon.Create;
                try
                  SourceImageList.GetIcon(RealIndex, SourceIcon);
                  TargetIcon := TIcon.Create;
                  try
                    if UpdateInvertIcon(SourceIcon, TargetIcon) then
                      NewIndex := TargetImageList.AddIcon(TargetIcon);
                  finally
                    TargetIcon.Free;
                  end;
                finally
                  SourceIcon.Free;
                end;
              end;
          end;
        if Index = 0 then
          if NewIndex = -1 then
            Break
          else
            Result := NewIndex
        else
          if Result <> NewIndex then
            begin
              if Assigned(TargetImageList) and (NewIndex <> -1) then TargetImageList.Delete(NewIndex);
              for NewIndex := Pred(Index) downto 0 do
                begin
                  TargetImageList := Items[NewIndex];
                  if Assigned(TargetImageList) then TargetImageList.Delete(Result);
                end;
              Result := -1; // Рассинхронизация списка иконок!!!
              Break;
            end;
      end;
end;

function TIconList.CreateStackIcons(const RealIndex: Integer): TStackIcons;
begin
  Result := TStackIcons.Create;
  try
    Result.LoadIcons(RealIndex, Self);
  except
    Result.Free;
    raise;
  end;
end;

{ TMapItem }

constructor TMapItem.Create(const ALogicalIndex, ARealIndex: Integer);
begin
  FLogicalIndex := ALogicalIndex;
  FRealIndex := ARealIndex;
end;

{ TMapList }

constructor TMapList.Create;
begin
  FList := TObjectList.Create;
  FList.Capacity := 256;
end;

destructor TMapList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TMapList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMapList.GetItem(const Index: Integer): TMapItem;
begin
  Result := FList[Index] as TMapItem;
end;

function TMapList.Find(const LogicalIndex: Integer; var MapIndex: Integer): Boolean;
var
  LowIndex, HighIndex, Index, CheckIndex: Integer;
begin
  Result := False;
  MapIndex := 0;
  if LogicalIndex >= 0 then
    begin
      LowIndex := 0;
      HighIndex := Pred(Count);
      while LowIndex <= HighIndex do
        begin
          Index := (LowIndex + HighIndex) shr 1;
          CheckIndex := Items[Index].LogicalIndex;
          if CheckIndex < LogicalIndex then
            begin
              LowIndex := Succ(Index);
              MapIndex := LowIndex;
            end
          else if CheckIndex = LogicalIndex then
            begin
              MapIndex := Index;
              Result := True;
              Break;
            end
          else
            begin
              HighIndex := Pred(Index);
              MapIndex := Index;
            end;
        end;
    end;
end;

function TMapList.Map(const LogicalIndex, RealIndex: Integer): Integer;
var
  MapItem: TMapItem;
begin
  if not Find(LogicalIndex, Result) then
    begin
      MapItem := TMapItem.Create(LogicalIndex, RealIndex);
      try
        FList.Insert(Result, MapItem);
      except
        MapItem.Free;
        raise;
      end;
    end;
end;

function TMapList.UnMap(const RealIndex: Integer): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Pred(Count) do
    if Items[Index].RealIndex = RealIndex then
      begin
        Result := Index;
        Break;
      end;
end;

{ TIconManager }

constructor TIconManager.Create(const ANormalLists, AHotLists, ADisabledLists: array of TCustomImageList; const ASchemaColors: TSchemaColorArray);
begin
  FMapList := TMapList.Create;
  FNormalLists := TIconList.Create(ANormalLists);
  FHotLists := TIconList.Create(AHotLists);
  FDisabledLists := TIconList.Create(ADisabledLists);
  FSchemaGUID := CalculareGUID;
end;

destructor TIconManager.Destroy;
begin
  FDisabledLists.Free;
  FHotLists.Free;
  FNormalLists.Free;
  FMapList.Free;
  inherited Destroy;
end;

function TIconManager.LoadIconFromResource(const ResourceName: string; const LogicalIndex: Integer): Integer;
var
  Index: Integer;
begin
  Result := FNormalLists.LoadIconFromResource(ResourceName);
  if Result <> -1 then
    begin
      Index := FHotLists.LoadColorIcon(Result, FNormalLists, clFocused);
      if Index = Result then
        begin
          if FDisabledLists.Count <> 0 then
            begin
              Index := FDisabledLists.LoadColorIcon(Result, FNormalLists, clDisabled);
              if Index <> Result then
                begin
                  if Index <> -1 then FDisabledLists.RemoveIcon(Index);
                  FNormalLists.RemoveIcon(Result);
                  FHotLists.RemoveIcon(Result);
                  Result := -1; // Рассинхронизация индексов иконок!!!
                end;
            end;
        end
      else
        begin
          if Index <> -1 then FHotLists.RemoveIcon(Index);
          FNormalLists.RemoveIcon(Result);
          Result := -1; // Рассинхронизация индексов иконок!!!
        end;
    end;
  // Маппим загруженную иконку ...
  FMapList.Map(LogicalIndex, Result);
end;

function TIconManager.CopyShemaIcon(const RealIndex: Integer; const SchemaColor: TSchemaColor): Integer;
var
  Index: Integer;
begin
  Result := FNormalLists.LoadColorIcon(RealIndex, FNormalLists, SchemaColors[SchemaColor]);
  if Result <> -1 then
    begin
      Index := FHotLists.LoadLightIcon(Result, FNormalLists);
      if Index = Result then
        begin
          if FDisabledLists.Count <> 0 then
            begin
              Index := FDisabledLists.LoadColorIcon(Result, FNormalLists, clDisabled);
              if Index <> Result then
                begin
                  if Index <> -1 then FDisabledLists.RemoveIcon(Index);
                  FNormalLists.RemoveIcon(Result);
                  FHotLists.RemoveIcon(Result);
                  Result := -1; // Рассинхронизация индексов иконок!!!
                end;
            end;
        end
      else
        begin
          if Index <> -1 then FHotLists.RemoveIcon(Index);
          FNormalLists.RemoveIcon(Result);
          Result := -1; // Рассинхронизация индексов иконок!!!
        end;
    end;
end;

function TIconManager.CopyInvertIcon(const RealIndex: Integer): Integer;
var
  Index: Integer;
begin
  Result := FNormalLists.CopyInvertIcon(RealIndex, FNormalLists);
  if Result <> -1 then
    begin
      Index := FHotLists.CopyInvertIcon(RealIndex, FHotLists);
      if Index = Result then
        begin
          if FDisabledLists.Count <> 0 then
            begin
              Index := FDisabledLists.CopyInvertIcon(RealIndex, FDisabledLists);
              if Index <> Result then
                begin
                  if Index <> -1 then FDisabledLists.RemoveIcon(Index);
                  FNormalLists.RemoveIcon(Result);
                  FHotLists.RemoveIcon(Result);
                  Result := -1; // Рассинхронизация индексов иконок!!!
                end;
            end;
        end
      else
        begin
          if Index <> -1 then FHotLists.RemoveIcon(Index);
          FNormalLists.RemoveIcon(Result);
          Result := -1; // Рассинхронизация индексов иконок!!!
        end;
    end;
end;

function EnumIconGroupResources(Module: HMODULE; ResType, ResName: PChar; Data: Pointer): BOOL; stdcall;
var
  Index: Integer;
  Name: string;
begin
  Result := True;
  if NativeInt(Pointer(ResName)) > 65535  then
    begin
      Name := StrPas(ResName);
      if Assigned(Data) and (Length(Name) = 3) and (Name[1] = 'P') then
        if TryStrToInt('$' + Copy(Name, 2, 2), Index) then
          TStrings(Data).AddObject(ResName, Pointer(NativeInt(Index)));
    end;
end;

function TIconManager.LoadIcons: Integer;
var
  Strings: TStrings;
  Index, ImageIndex: Integer;
  ResourceName: string;
begin
  Result := 0;
  Strings := TStringList.Create;
  try
    Strings.Capacity := 256;
    Win32Check(EnumResourceNames(hInstance, RT_GROUP_ICON, @EnumIconGroupResources, NativeInt(Strings)));
    for Index := 0 to Pred(Strings.Count) do
      begin
        ResourceName := Strings[Index];
        ImageIndex := Integer(Strings.Objects[Index]);
        if LoadIconFromResource(ResourceName, ImageIndex) <> -1 then
          Inc(Result);
      end;
  finally
    Strings.Free;
  end;
end;

function TIconManager.BuildStackIcons(const IconIndex: Word; const Inversed: Boolean; IconList: TIconList): TStackIcons;
var
  LogicalIndex, MapIndex: Integer;
  StackIcons, TempStackIcons: TStackIcons;
  SchemaColor: TSchemaColor;
begin
  Result := TStackIcons.Create;
  try
    Result.FIconIndex := LoByte(IconIndex);
    Result.FSchemaColor := HiByte(IconIndex) and $0F;
    Result.FInversed := Inversed;
    // Если это нулевая иконка, то ...
    if LoByte(IconIndex) = 0 then
      LogicalIndex := $0000
    else
      LogicalIndex := IconIndex and $0FFF;
    if Inversed then LogicalIndex := LogicalIndex or $40000000;
    if FMapList.Find(LogicalIndex, MapIndex) then
      Result.LoadIcons(FMapList[MapIndex].RealIndex, IconList)
    else
      begin
        StackIcons := TStackIcons.Create;
        try
          StackIcons.FIconIndex := Result.IconIndex;
          StackIcons.FSchemaColor := Result.SchemaColor;
          StackIcons.FInversed := Result.Inversed;
          // Если это нулевая иконка, то ...
          if LoByte(IconIndex) = 0 then
            if FMapList.Find($0000, MapIndex) then
              StackIcons.LoadIcons(FMapList[MapIndex].RealIndex, IconList)
            else
              StackIcons.CreateEmptyIcons(IconList)
          else if FMapList.Find(LogicalIndex and $0FFF, MapIndex) then
            StackIcons.LoadIcons(FMapList[MapIndex].RealIndex, IconList)
          else if FMapList.Find(LogicalIndex and $00FF, MapIndex) then
            begin
              StackIcons.FSchemaColor := 0;
              StackIcons.LoadIcons(FMapList[MapIndex].RealIndex, IconList);
              SchemaColor := HiByte(LogicalIndex) and $0F;
              if SchemaColor <> 0 then
                begin
                  TempStackIcons := StackIcons.CloneColorStackIcons(SchemaColors[SchemaColor]);
                  try
                    StackIcons.Assign(TempStackIcons);
                  finally
                    TempStackIcons.Free;
                  end;
                end;
            end;
          if Inversed then
            begin
              TempStackIcons := StackIcons.CloneInvertStackIcons;
              try
                Result.Assign(TempStackIcons);
              finally
                TempStackIcons.Free;
              end;
            end
          else
            Result.Assign(StackIcons);
        finally
          StackIcons.Free;
        end;
      end;
  except
    Result.Free;
    raise;
  end;
end;

function TIconManager.ReadStackIcons(StackIcons: TStackIcons; IconList: TIconList): Integer;
var
  Index, NewIndex: Integer;
  Stack: TStackIcon;
begin
  Result := -1;
  if Assigned(StackIcons) and Assigned(IconList) then
    begin
      for Index := 0 to Pred(IconList.Count) do
        begin
          Stack := StackIcons.Find(IconList[Index].Width, IconList[Index].Height);
          if Assigned(Stack) then
            NewIndex := IconList[Index].AddIcon(Stack.Icon)
          else
            NewIndex := -1;
          if Index = 0 then
            if NewIndex = -1 then
              Break
            else
              Result := NewIndex
          else
            if Result <> NewIndex then
              begin
                if NewIndex <> -1 then IconList[Index].Delete(NewIndex);
                for NewIndex := Pred(Index) downto 0 do
                  IconList[NewIndex].Delete(Result);
                Result := -1; // Рассинхронизация списка иконок!!!
                Break;
              end;
        end;
    end;
end;

function TIconManager.CreateStackIcons(const LogicalIndex: Integer): TStackIcons;
var
  BaseStackIcons, OverStackIcons: TStackIcons;
  Position: TOverlayIconPosition;
begin
  // Если базовой иконки нет, то ...
  if LoByte(LoWord(LogicalIndex)) = 0 then
    // Если накладываемой иконки нет, то ...
    if LoByte(HiWord(LogicalIndex)) = 0 then
      begin
        Result := TStackIcons.Create;
        try
          Result.CreateEmptyIcons(FNormalLists);
        except
          Result.Free;
          raise;
        end;
      end
    else
      begin
        // Есть накладываемая иконка, но нет базовой ...
        BaseStackIcons := BuildStackIcons(LoWord(LogicalIndex), (HiWord(LogicalIndex) and $4000) <> 0, FNormalLists);
        try
          Position := HiByte(LoWord(LogicalIndex)) shr 4;
          OverStackIcons := BuildStackIcons(HiWord(LogicalIndex), False, FNormalLists);
          try
            Result := BaseStackIcons.CloneOverlayStackIcons(Position, BaseStackIcons, OverStackIcons);
          finally
            OverStackIcons.Free;
          end;
        finally
          BaseStackIcons.Free;
        end;
      end
  else
    begin
      try
        if LoByte(HiWord(LogicalIndex)) = 0 then
          begin // Если накладываемой иконки нет, то ...
            BaseStackIcons := BuildStackIcons(LoWord(LogicalIndex), (HiWord(LogicalIndex) and $4000) <> 0, FNormalLists);
            Result := TStackIcons.Create;
            try
              Result.Assign(BaseStackIcons);
            except
              Result.Free;
              raise;
            end;
          end
        else
          begin // Есть базовая и накладываемая иконка ...
            try
              BaseStackIcons := BuildStackIcons(LoWord(LogicalIndex), (HiWord(LogicalIndex) and $4000) <> 0, FNormalLists);
              OverStackIcons := BuildStackIcons(HiWord(LogicalIndex), False, FNormalLists);
              Result := BaseStackIcons.CloneOverlayStackIcons(HiByte(LoWord(LogicalIndex)) shr 4, BaseStackIcons, OverStackIcons);
            finally
              OverStackIcons.Free;
            end;
          end;
      finally
        BaseStackIcons.Free;
      end;
    end;
end;

function TIconManager.MapIcon(const LogicalIndex: Integer): Integer;
var
  MapIndex, Index: Integer;
  ResultStackIcons: TStackIcons;
begin
  // Если иконка уже маппилась, то ...
  if FMapList.Find(LogicalIndex, MapIndex) then
    Result := FMapList[MapIndex].RealIndex // Возвращаем её реальный номер с TImageList`ах ...
  else
    begin
      ResultStackIcons := CreateStackIcons(LogicalIndex);
      try
        Result := ReadStackIcons(ResultStackIcons, FNormalLists);
      finally
        ResultStackIcons.Free;
      end;
      if Result <> -1 then
        begin
          Index := FHotLists.LoadColorIcon(Result, FNormalLists, clFocused);
          if Index = Result then
            begin
              if FDisabledLists.Count <> 0 then
                begin
                  Index := FDisabledLists.LoadColorIcon(Result, FNormalLists, clDisabled);
                  if Index <> Result then
                    begin
                      if Index <> -1 then FDisabledLists.RemoveIcon(Index);
                      FNormalLists.RemoveIcon(Result);
                      FHotLists.RemoveIcon(Result);
                      Result := -1; // Рассинхронизация индексов иконок!!!
                    end;
                end;
            end
          else
            begin
              if Index <> -1 then FHotLists.RemoveIcon(Index);
              FNormalLists.RemoveIcon(Result);
              Result := -1; // Рассинхронизация индексов иконок!!!
            end;
        end;
      // Маппим загруженную иконку ...
      FMapList.Map(LogicalIndex, Result);
    end;
end;

function TIconManager.UnMapIcon(const RealIndex: Integer): Integer;
var
  MapIndex: Integer;
begin
  MapIndex := FMapList.UnMap(RealIndex);
  if MapIndex <> -1 then
    Result := FMapList[MapIndex].LogicalIndex
  else
    Result := -1;
end;

function TIconManager.CalculareGUID: TGUID;
var
  Stream: TStream;
  Index: TSchemaColor;
  procedure WriteColor(const Color: TColor);
  var
    Value: Integer;
  begin
    Value := ColorToRGB(Color);
    Stream.WriteBuffer(Value, SizeOf(Value));
  end;
begin
  Stream := TMemoryStream.Create;
  try
    WriteColor(clFocused);
    WriteColor(clDisabled);
    for Index := Low(Index) to High(Index) do
      WriteColor(SchemaColors[Index]);
    Result := MD5Stream(Stream).G;
  finally
    Stream.Free;
  end;
end;

function TIconManager.ExportToFile(const FileName: string; const LogicalIndex: Integer): Boolean;
var
  StackIcons: TStackIcons;
begin
  StackIcons := CreateStackIcons(LogicalIndex);
  try
    Result := Assigned(StackIcons) and (StackIcons.Count <> 0);
    if Result then
      Result := StackIcons.ExportToFile(FileName);
  finally
    StackIcons.Free;
  end;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('uIconManager unit initialization ...');
  {$ENDIF}
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('uIconManager unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

