unit Dictionary;

interface

uses Windows, SysUtils, Classes, Contnrs, Forms;

const
  LANG_ENGLISH_FULL = 1033;
  LANG_RUSSIAN_FULL = 1049;
type
  // тип перевода
  TTranslationType =
    (ttFirstName,     // вернуть только первое имя
     ttSecondName,    // вернуть второе имя
     ttFull           // вернуть перевод полностью
     );

  TLanguage = record
    LngID: LANGID;
    FileName : String;
    Caption: String;
 //   HasXML: Boolean;
  end;

  TDictionarySource =
    (
      dsNone,     // Не определён
      dsResource, // Системные переводы из ресурсов
      dsFile,     // Переводы из XML файлов
      dsDatabase  // Переводы из M_DICTIONARY таблицы
    );
  TDictionarySources = set of TDictionarySource;

  // элемент словаря
  TDictionaryWord = class
  private
    FName: string;
    FTranslated: string;
    FSource: TDictionarySource;
  public
    constructor Create(const AName, ATranslated: string; const ASource: TDictionarySource = dsNone);
    property Name: string read FName;
    property Translated: string read FTranslated;
    property Source: TDictionarySource read FSource;
  end;

  // словарь
  TDictionary = class(TObjectList)
  private
    FDefaultLanguageID: LANGID;
    FActiveLang: LANGID;
    FLanguages: array of TLanguage;
    function Get(const Index: Integer): TDictionaryWord;
    procedure Put(const Index: Integer;  Value: TDictionaryWord);
    function GetCurrentLang: LANGID;
    function FindIdentStart(const aPhrase: string): Integer;
    function FindIdentEnd(const aPhrase: string): Integer;
    function GetLanguageCount: Integer;
    function GetLanguage(const Index: Integer): TLanguage;
    function UpdateFromResource(const LanguageID: LANGID): Boolean;
  public
    constructor Create;
    property Items[const Index: Integer]: TDictionaryWord read Get write Put; default;
    property ActiveLang: LANGID read FActiveLang;
    function SetActiveLang(ALangID: LANGID): Boolean;
    property CurrentLang: LANGID read GetCurrentLang;
    function Load: Boolean;
    function TranslateWord(const aWord: string; const aTranslateType: TTranslationType = ttFirstName): string;
    function TranslatePhrase(const aPhrase: string; const aTranslateType: TTranslationType = ttFirstName): string;
    function UpdateFromFile(var Lang: LANGID): Boolean;
    function LanguageIndexOf(const ALangID: LANGID): Integer;
    function Find(const Name: string; var WordIndex: Integer): Boolean;
    procedure ClearSource(const RemoveSources: TDictionarySources);
    {$IFDEF DEBUG}
    procedure DebugDictionaryLog(const Text: string);
    {$ENDIF}
    property DefaultLanguageID: LANGID read FDefaultLanguageID;
    property LanguageCount: Integer read GetLanguageCount;
    property Languages[const Index: Integer]: TLanguage read GetLanguage;
  end;

function Translate(const NameA: string; var NameB: string): Boolean;
function GetTitle(const _Name: string; const aTranslateType: TTranslationType = ttFirstName): string;
procedure LangFormUpdate(F: TForm; const CaptionID: string = '');
procedure UpdateDictionary(var Lang: LANGID);

const
 dicMSWord  = 'Microsoft Word';
 dicMSExcel = 'Microsoft Excel';

var
  Dict: TDictionary;

implementation

uses StrUtils, Variants, DB, Menus, StdCtrls, ActnList, Buttons, ComCtrls, ExtCtrls, XMLIntf,
     DeLog, Funcs, DeMeta, MainMenuList, DataManager, ElementsMeta, QueryDescriptor, DeTypes, XMLTable, DeDB, DataUnit,
     DeDataset, DeSettings, DeMetadata, DeParser;

const
  // символ, являющийся началом переводимого слова
  IdentStartChar = '_';
  // множество символов, являющихся концом переводимого слова
  IdentBreakChars : set of AnsiChar = [
    '!', '"', '#', '$', '%', '&', '''', '(', ')', '*', '+', ',', '-', {'.',} '/',
    ':', ';', '<', '=', '>', '?', '@', '[', '\', ']', '^', '`', '{', '|', '}',
    '~', #10, ' ', #13];
  // множество символов, разрешенных в идентификаторах Delphi
  DelphiIdentChars = ['a'..'z', 'A'..'Z', '0'..'9', '_'];

procedure UpdateDictionary(var Lang: LANGID);
begin
  Dict.UpdateFromFile(Lang);
end;

{ --- TDictionaryWord -------------------------------------------------------- }

constructor TDictionaryWord.Create(const aName, aTranslated : string; const ASource: TDictionarySource);
begin
  inherited Create;
  FName := aName;
  FTranslated := aTranslated;
  FSource := ASource;
end;


{ --- TDictionary ------------------------------------------------------------ }

constructor TDictionary.Create;
var
  Directory: string;
  ErrorCode, LanguageID, Index: Integer;
  SearchRec: TSearchRec;
begin
  inherited Create;
  FDefaultLanguageID := GetSystemDefaultLangID;
  FActiveLang := LANG_NEUTRAL;
  //
  Directory := ExtractFilePath(Application.ExeName);
  if Length(Directory) <> 0 then Directory := IncludeTrailingPathDelimiter(Directory);
  try
    SetLength(FLanguages, 2);
    FLanguages[0].LngID := LANG_ENGLISH_FULL;
    FLanguages[0].Caption := 'English';
    FLanguages[1].LngID := LANG_RUSSIAN_FULL;
    FLanguages[1].caption := 'Русский';

    ErrorCode := FindFirst(Directory + '*' + sExtensionLNG, faAnyFile, SearchRec);
    if ErrorCode = 0 then
      try
        while ErrorCode = 0 do
          begin
            Index:= Length(FLanguages);
            SetLength(FLanguages, Succ(Index));
//TODO: НАДО Добавить код языка из файла
            FLanguages[Index].LngID:= LanguageID;
            FLanguages[Index].FileName:= SearchRec.Name;
            FLanguages[Index].Caption:= ChangeFileExt(FLanguages[Index].FileName, EmptyStr);
            ErrorCode := FindNext(SearchRec);
          end;
      finally
        FindClose(SearchRec);
      end;

    ActivateKeyboardLayout(HKL_NEXT, 0);

  finally
  end;
  UpdateFromResource(CurrentLang);
  //
  Application.UpdateFormatSettings := False;
  SetActiveLang(Variables.AsWord[RegLang]);
end;

function TDictionary.Get(const Index: Integer): TDictionaryWord;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TDictionaryWord(inherited Items[Index])
  else
    Result := nil;
end;

procedure TDictionary.Put(const Index: Integer; Value: TDictionaryWord);
begin
  if Index >= Count then Count := Succ(Index);
  inherited Items[Index] := Value;
end;

function TDictionary.Load: Boolean;
var Q: TDeDataSet;
    R, NameIndex, TranslatedIndex, Index: Integer;
    s: String;
    LangID: Word;
begin
  {$IFDEF DEBUG}
  DebugDictionaryLog(ClassName + '.Load start%s ...');
  {$ENDIF}
  ClearSource([dsDatabase]);
  try
    Result := Assigned(MetaData) and Assigned(MetaData.MetadataDB);
    if Result then
      begin
        LangID:= CurrentLang;
        if MetaData.MetadataDB.FieldExists(tblLanguage, fldLanguageCode) then
          begin
            Q := MetaData.MetadataDB.CreateQuery(qtSelect);
            Q.Descr.Table := tblLanguage;
            Q.Descr.AddCondition(fldLanguageCode, ftInteger, opEQ, CurrentLang);
            Q.Open;
            if Q.RecordCount=1 then LangID:= Q.ValueByName[fldLanguageID];
            Q.Free;
          end;

        Q := MetaData.MetadataDB.CreateQuery(qtSelect);
        try
          Q.Descr.BeginUpdate;
          try
            Q.Descr.Table := tblDictonary;
            Q.Descr.AddCondition(fldDictionaryLangId, ftInteger, opEQ, LangID);
            Q.Descr.AddCondition(fldDictionaryName, ftString, opIs, Null);
            Q.Descr.AddOperation(opNot);
            Q.Descr.AddOperation(opAnd);
            Q.Descr.AddSortField(fldDictionaryName);
            // 27.08.2015 + Куфенко: Читаем только необходимые поля!
            Q.Descr.AddFields([fldDictionaryName, fldDictionaryTranslate]);
            // 27.08.2015 -
          finally
            Q.Descr.EndUpdate;
          end;
          Q.Open;
          NameIndex       := Q.IndexByName(fldDictionaryName);
          TranslatedIndex := Q.IndexByName(fldDictionaryTranslate);

          for R:=0 to Pred(Q.RecordCount) do
            begin
              Q.RecNo:= R;
              s:= Q.StringValue(TranslatedIndex);

              if Find(Q.Value[NameIndex], Index) then
                begin
                  Items[Index].FTranslated:= s;
                  Items[Index].FSource:= dsDatabase;
                end
              else
                Insert(Index, TDictionaryWord.Create(Q.Value[NameIndex], s, dsDatabase));
            end;
        finally
          Q.Free;
        end;
      end;
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('%s.Load error: %s', [ClassName, E.Message]);
        {$ENDIF}
        Result := False;
      end;
  end;
  {$IFDEF DEBUG}
  DebugDictionaryLog(ClassName + '.Load finish%s ...');
  {$ENDIF}
end;

function TDictionary.SetActiveLang(ALangID: LANGID): Boolean;
var
  NewLang: Word;
begin
  Result:= True;
  NewLang:= ALangID;
  if not UpdateFromResource(NewLang) then
  if not UpdateFromFile(NewLang) then
    begin
      Result:= False;
      NewLang:= GetCurrentLang;
      if not UpdateFromResource(NewLang) then
      if not UpdateFromFile(NewLang) then
        begin
         NewLang:= LANG_ENGLISH_FULL;
         UpdateFromResource(NewLang)
        end;
    end;

  FActiveLang := NewLang;

  if Assigned(MetaData) and(MetaData.DatabasesCount > 0) and MetaData.MetadataDB.Connected then Load;
  SetThreadLocale(((GetThreadLocale shr 16) shl 16) or  CurrentLang);
  Variables.AsWord[RegLang] := FActiveLang;
end;

function TDictionary.GetCurrentLang: LANGID;
var i, CurrentLang: Integer;
begin
  if FActiveLang = 0 then CurrentLang:= GetSystemDefaultLangID
                     else CurrentLang:= FActiveLang;

  for i:=Low(FLanguages) to High(FLanguages) do
    if (CurrentLang = FLanguages[i].LngID) then Exit(CurrentLang);

  Result:= LANG_ENGLISH_FULL;
end;

function TDictionary.TranslateWord(const aWord: string; const aTranslateType: TTranslationType): string;
type TCase = (caseNone, caseLower, caseUpper, caseFirstUpper);
var s{,u}     : String;
    i,p,a,z : Integer;
    ok      : Boolean;
    StrCase : TCase;
begin
  if aWord='_' then Exit(aWord);

  s:=LowerCase(aWord);

  if Length(s)<2 then                StrCase := caseNone else
    if CharInSet(aWord[1], ['A'..'Z']) then
      if CharInSet(aWord[2], ['A'..'Z']) then StrCase := caseUpper      {AA{}
                                         else StrCase := caseFirstUpper {Aa{} else
      if CharInSet(aWord[2], ['A'..'Z']) then StrCase := caseNone       {aA{}
                                         else StrCase := caseLower;     {aa{}

  Result:=aWord;
  Ok:=False;

  a:=0;            // быстрый поиск в упорядоченном списке DB
  z:=Count-1;
  while a<=z do
    begin
      i:=(z+a) div 2;
      p:=CompareStr(s, Items[i].name);
      if p<0 then z:=i-1 else
      if p>0 then a:=i+1 else
                  begin
                    Result := Items[i].Translated;
                    Ok:=True;
                    Break;
                  end;
    end;

  if aTranslateType <> ttFull then
    begin
      p:=Pos('|',Result);
      if p>0 then
        begin
          if aTranslateType = ttSecondName then
            System.Delete(Result,1,p)
          else
            System.Delete(Result,p,MaxInt);
        end;
    end;

  if not Ok then
    WriteLog(aWord + ' ' + IntToStr(GetCurrentLang), True, 'Dictionary');

  case StrCase of
    caseUpper:  result := AnsiUpperCase(result);
    caseLower:  result := AnsiLowerCase(result);
    caseFirstUpper:
      result := AnsiUpperCase(copy(result, 1, 1))+
                AnsiLowerCase(copy(result, 2, Length(result)));
  end;
end;

function TDictionary.FindIdentStart(const aPhrase: string): Integer;
var P : integer;
begin
  result := pos(IdentStartChar, aPhrase);
  if result > 0 then
    if (result = 1) or CharInSet(aPhrase[result-1], IdentBreakChars) then
      inc(result)
    else
      if result = Length(aPhrase) then
        result := 0
      else
      begin
        P := FindIdentStart(system.Copy(aPhrase, result+1, MaxInt));
        if P = 0 then
          result := 0
        else
          result := result + P;
      end;
end;

function TDictionary.FindIdentEnd(const aPhrase: string): Integer;
var I, PhraseLen : integer;
begin
  // первым символом строки является первый символ идентификатора
  I := 1;
  PhraseLen := Length(aPhrase);
  while (I <= PhraseLen) and (not CharInSet(aPhrase[I], IdentBreakChars)) do
    inc(I);
  result := I-1;
end;

function TDictionary.TranslatePhrase(const aPhrase: string; const aTranslateType: TTranslationType): string;
var Source, Ident : String;
    p             : Integer;
begin
  Source := aPhrase;
  Ident := EmptyStr;
  Result := EmptyStr;
  P := FindIdentStart(Source);
  if P > 0 then
    while P > 0 do
      begin
        result := result + system.Copy(Source, 1, P-2);
        system.Delete(Source, 1, P-1);
        P := FindIdentEnd(Source);
        if P > 0 then
        begin
          Ident := Copy(Source, 1, P);
          system.Delete(Source, 1, P);
          result := result + TranslateWord(Ident, aTranslateType);
        end;
        P := FindIdentStart(Source);
      end
    else
      begin
        P := Pos('|', Source);
        if P <> 0 then
          case aTranslateType of
            ttFirstName: Source := Copy(Source, 1, Pred(P));
            ttSecondName: Source := Copy(Source, Succ(P), Length(Source));
          end;
      end;
  result := result + Source;
end;

function TDictionary.UpdateFromFile(var Lang: LANGID): Boolean;
var
  XMLFile: TXMLDocumentISO;
  XMLT: TXMLTable;
  XMLNode: IXMLNode;
  XMLName: WideString;
  R,P,i: Integer;
  s,t: Variant;
begin
  P:= LanguageIndexOf(Lang);
  if -1 < P then XMLName:= ExtractFileDir(Application.ExeName)+'\'+FLanguages[P].FileName
            else Exit(False);

  if not FileExists(XMLName) then Exit(False);

  Result:= True;
  try
    XMLFile:= TXMLDocumentISO.Create(Application);
    XMLFile.LoadFromFile(XMLName);
    XMLFile.ReadHeader;

    XMLT:= XMLFile.Items[0];
    try
      if (-1<P) then
        for R:= 0 to Pred(XMLT.Count) do
          begin
            XMLNode:= XMLT.Items[R];
            XMLT.Item_GetFieldValue(XMLNode, fldDictionaryName, s);
            XMLT.Item_GetFieldValue(XMLNode, fldDictionaryTranslate, t);

            if Find(s, i) then
              begin
                Items[i].FTranslated:= t;
                Items[i].FSource:= dsFile;
              end
            else
              Insert(i, TDictionaryWord.Create(s, t, dsFile));
          end
    except
      Result:= False;
    end;
  finally
    XMLFile.Free;
  end;
end;

function TDictionary.LanguageIndexOf(const ALangID: LANGID): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := Low(FLanguages) to High(FLanguages) do
    if FLanguages[Index].LngID = ALangID then
      begin
        Result := Index;
        Break;
      end;
end;

function TDictionary.GetLanguageCount: Integer;
begin
  Result := Length(FLanguages);
end;

function TDictionary.GetLanguage(const Index: Integer): TLanguage;
begin
  Result := FLanguages[Index];
end;

function TDictionary.Find(const Name: string; var WordIndex: Integer): Boolean;
var
  LowIndex, HighIndex, Index, Code: Integer;
  CheckName: string;
begin
  Result := False;
  WordIndex := 0;
  LowIndex := 0;
  HighIndex := Pred(Count);
  while LowIndex <= HighIndex do
    begin
      Index := (LowIndex + HighIndex) shr 1;
      CheckName := Items[Index].Name;
      Code := CompareStr(CheckName, Name);
      if Code < 0 then
        begin
          LowIndex := Succ(Index);
          WordIndex := LowIndex;
        end
      else if Code > 0 then
        begin
          HighIndex := Pred(Index);
          WordIndex := Index;
        end
      else
        begin
          WordIndex := Index;
          Result := True;
          Break;
        end;
    end;
end;

procedure TDictionary.ClearSource(const RemoveSources: TDictionarySources);
var
  Index: Integer;
begin
  for Index := Pred(Count) downto 0 do
    if Items[Index].Source in RemoveSources then
      Delete(Index);
end;

function TDictionary.UpdateFromResource(const LanguageID: LANGID): Boolean;
const
  cDeLanguageEN = 'EN';
  cDeLanguageRU = 'RU';
  function Update(const ResourceName: string): Boolean;
  var
    Strings: TStrings;
    Stream: TStream;
    Index, ItemIndex: Integer;
    Name: string;
  begin
    Result := FindResource(hInstance, PChar(ResourceName), RT_RCDATA) <> 0;
    if Result then
      begin
        Strings := TStringList.Create;
        try
          Stream := TResourceStream.Create(hInstance, ResourceName, RT_RCDATA);
          try
            Strings.LoadFromStream(Stream);
          finally
            Stream.Free;
          end;
          for Index := 0 to Pred(Strings.Count) do
            begin
              Name := Strings.Names[Index];
              if Find(Name, ItemIndex) then
                begin
                  Items[ItemIndex].FTranslated := Strings.ValueFromIndex[Index];
                  Items[ItemIndex].FSource := dsResource;
                end
              else
                Insert(ItemIndex, TDictionaryWord.Create(Name, Strings.ValueFromIndex[Index], dsResource));
            end;
        finally
          Strings.Free;
        end;
      end;
  end;
begin
  if LanguageID = LANG_ENGLISH_FULL then
    Result := Update(cDeLanguageEN)
  else if LanguageID = LANG_RUSSIAN_FULL then
    Result := Update(cDeLanguageRU)
  else
    Result := False;
end;

{$IFDEF DEBUG}
procedure TDictionary.DebugDictionaryLog(const Text: string);
const
  cTranslatedFile = 'Translated';
  function PrepareDictionaryLog: string;
  const
    SourceNames: array[TDictionarySource] of PChar = ('dsNone', 'dsResource', 'dsFile', 'dsDatabase');
  var
    Index, IndexSize: Integer;
    DictionaryWord: TDictionaryWord;
    function PrepareLanguageName(const LanguageID: LANGID): string;
    begin
      case LanguageID of
        LANG_NEUTRAL: Result := 'LANG_NEUTRAL';
        LANG_ENGLISH_FULL: Result := 'LANG_ENGLISH';
        LANG_RUSSIAN_FULL: Result := 'LANG_RUSSIAN';
      else
        Result := '$' + IntToHex(FActiveLang, 4) + ' (' + IntToStr(FActiveLang) + ')';
      end;
    end;
  begin
    Result := EmptyStr;
    IndexSize := Length(IntToStr(Count));
    for Index := 0 to Pred(Count) do
      begin
        DictionaryWord := Items[Index];
        if Assigned(DictionaryWord) then
          Result := Result + Format(#13#10'                        ¦ %*d. ¦ %-10s ¦ %-64s ¦ %-160s ¦',
            [
              IndexSize, Index, StrPas(SourceNames[DictionaryWord.Source]),
              DictionaryWord.Name, DictionaryWord.Translated
            ])
        else
          Result := Result + Format(#13#10'                        ¦ %*d. ¦ %10s ¦ %64s ¦ %160s ¦',
            [
              IndexSize, Index, EmptyStr, EmptyStr, EmptyStr
            ]);
      end;
    if Length(Result) <> 0 then
      begin
        Result :=
          Format(#13#10'                        -%sT%sT%sT%s¬', [
            DupeString('-', IndexSize + 3),
            DupeString('-', 12),
            DupeString('-', 66),
            DupeString('-', 162)
            ]) +
          Format(#13#10'                        ¦ %*s ¦ %-10s ¦ %-64s ¦ %-160s ¦', [
            Succ(IndexSize), 'No',
            'Source',
            'Original',
            'Translated'
            ]) +
          Format(#13#10'                        +%s+%s+%s+%s+', [
            DupeString('-', IndexSize + 3),
            DupeString('-', 12),
            DupeString('-', 66),
            DupeString('-', 162)
            ]) +
          Result +
          Format(#13#10'                        L%s+%s+%s+%s-', [
            DupeString('-', IndexSize + 3),
            DupeString('-', 12),
            DupeString('-', 66),
            DupeString('-', 162)
            ]);
      end;
    Result :=
      #13#10'                        Active language: ' + PrepareLanguageName(FActiveLang) +
      #13#10'                        Default language: ' + PrepareLanguageName(FDefaultLanguageID) +
      Result;
  end;
begin
  WriteLog(Format(Text, [EmptyStr]) + PrepareDictionaryLog, True, cTranslatedFile);
  DebugLog(Text, [' saved to file ' + QuotedStr(cTranslatedFile + sExtensionLog)]);
end;
{$ENDIF}

{ --- Service ------------------------------------------------------------ }

function Translate(const NameA: string; var NameB: string): Boolean;
var p: Integer;
begin
  p:=Pos(IdentStartChar, NameA);
  Result := (p > 0);
  if p > 0 then NameB := '_'+StringReplace( Copy(NameA, p+1, MaxInt), '_', '.', [rfReplaceAll])
           else NameB := NameA;
end;
//------------------------------------------------------------------------------
function GetTitle(const _Name: string; const aTranslateType: TTranslationType): string;
begin
  Result := Dict.TranslatePhrase(_Name, aTranslateType);
end;
//------------------------------------------------------------------------------
{
procedure LangUpdateMenu(Lang:Integer; TMI : TMenuItem);
var i   : Integer;
    abc : String;
begin
  if Translate(TMI.Name, abc) then
    TMI.Caption:= GetTitle(abc);

  for i:=0 to TMI.Count-1 do
    LangUpdateMenu(Lang, TMI.Items[i]);
end;
}
//------------------------------------------------------------------------------
procedure LangFormUpdate(F: TForm; const CaptionID: string);
var i,j    : Integer;
    abc    : String;
    ok     : Boolean;
    FClass : TClass;
    aForm  : TCustomForm;
    TB     : TToolBar;
begin
  if Translate(F.Name, abc) and (Length(CaptionID) = 0) then
    F.Caption := GetTitle(abc)
  else
    F.Caption := GetTitle(CaptionID);

  for i := 0 to F.ComponentCount-1 do
    begin
      FClass := F.Components[i].ClassType;

      if FClass=TComboBox then
        TComboBox(F.Components[i]).Invalidate;

      if (Copy(F.Components[i].Name,1,7)=ElementPrefix) and (Not (FClass = TCheckBox))
        then ok:= Translate(TDeElement(TComponent(F.Components[i]).Tag).ElementMeta.Name, abc)
        else ok:= Translate(F.Components[i].Name, abc);

      if (FClass = TMenuCont) then
        begin
          for j:=0 to TMenuCont(F.Components[i]).ItemCount-1 do
          TMenuCont(F.Components[i]).Items[j].Caption:=
            GetTitle(TContextMeta(TMenuCont(F.Components[i]).Items[j].Data).Name, ttSecondName);
        end else

      if not ok then Continue else

      if (FClass = TAction) then
        begin
          TAction(F.Components[i]).Caption:= GetTitle(abc);
          TAction(F.Components[i]).Hint   := GetTitle(abc, ttSecondName);
        end else
      //......................................................................
      if (FClass = TMenuItem) then
        TMenuItem(F.Components[i]).Caption:= GetTitle(abc) else
      //......................................................................
      if (FClass = TLabel) then
        TLabel(F.Components[i]).Caption := GetTitle(abc) else
      //......................................................................
      if (FClass = TButton) then
        TButton(F.Components[i]).Caption := GetTitle(abc) else
      //......................................................................
      if (FClass = TSpeedButton) then
        begin
          TSpeedButton(F.Components[i]).Caption:= GetTitle(abc);
          TSpeedButton(F.Components[i]).Hint   := GetTitle(abc, ttSecondName);
        end else
      //......................................................................
      if (FClass = TCheckBox) then
        TCheckBox(F.Components[i]).Caption := GetTitle(abc) else
      //......................................................................
      if (FClass = TTabSheet) then
        begin
          if (Copy(F.Components[i].Name,1,7)=ElementPrefix) and (TDeElement(TComponent(F.Components[i]).Tag)<>nil)
            and (TDeElement(TComponent(F.Components[i]).Tag).ElementMeta.ElementType=etListTabSheet)
            then TTabSheet(F.Components[i]).Caption:= GetTitle(abc, ttSecondName)
            else TTabSheet(F.Components[i]).Caption:= GetTitle(abc);
        end else
      //......................................................................
      if (FClass = TToolBar) then
        begin
          TB := TToolBar(F.Components[i]);
          TB.Caption:= GetTitle(abc);
          aForm := getParentForm(TB);
          if TB.Floating and (aForm <> nil) then
            aForm.Caption := TB.Caption;
        end else
      //......................................................................
      if (FClass = TRadioButton) then
        TRadioButton(F.Components[i]).Caption := GetTitle(abc) else
      //......................................................................
      if (FClass = TGroupBox) then
        TGroupBox(F.Components[i]).Caption:= ' '+GetTitle(abc)+' ' else
      //......................................................................
      if (FClass = TRadioGroup) then
        TRadioGroup(F.Components[i]).Caption:= ' '+GetTitle(abc)+' ';
      //--------------------------------------------------------------------------
    end;
end;

{$REGION 'Startup & Shutdown Unit Runtime ...'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('Dictionary unit initialization ...');
  {$ENDIF}
  Dict := TDictionary.Create;
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('Dictionary unit finalization ...');
  {$ENDIF}
  FreeAndNil(Dict);
end;
{$ENDREGION}

{ TLanguage }

initialization
  Startup;

finalization
  Shutdown;

end.

