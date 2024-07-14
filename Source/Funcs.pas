{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}

unit Funcs;

interface

uses Windows, SysUtils, Controls, Graphics, Forms, DB,
     ShlObj, Winapi.ActiveX, System.Win.ComObj;

const
      { множества типов полей }
      LogicalTypes = [ftBoolean];
      FloatTypes = [ftFloat, ftCurrency, ftBCD, ftExtended, ftSingle, ftFMTBCD];
      IntegerTypes = [ftShortint, ftLongWord, ftSmallint, ftInteger, ftWord, ftAutoInc, ftLargeint, ftByte];
      NumericTypes = FloatTypes + IntegerTypes;
      StringTypes = [ftString, ftMemo, ftFixedChar, ftWideString, ftWideMemo, ftFixedWideChar];
      WideStringTypes = [ftWideString, ftFixedWideChar, ftWideMemo];
      AnsiStringTypes = StringTypes - WideStringTypes;
      DateTypes = [ftDate, ftDateTime, ftTimeStamp, ftTimeStampOffset, ftOraTimeStamp];
      DateTimeTypes = DateTypes + [ftTime];
      BinaryTypes = [ftBlob, ftBytes, ftVarBytes];
      NotSortTypes = [ftBlob, ftBytes, ftVarBytes, ftMemo, ftWideMemo, ftArray];

      //Base64   : AnsiString = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

const
      HexChars: Array[0..15] of WideChar = ('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');
      HLSMAX = 240;
      RGBMAX = 255;
      UNDEFINED = (HLSMAX*2) div 3;

var  fs: TFormatSettings;

type
  // <summary>типы событий в метаструктуре</summary>
  TNumericCompare = (tcStrong, tcInclude, tcLight);

function CtrlDown(State : TKeyboardState) : Boolean;
function ShiftDown(State : TKeyboardState) : Boolean;
function AltDown(State : TKeyboardState) : Boolean;

function TypesCompatible(const aType1, aType2: TFieldType; const aPrecision: TNumericCompare = tcStrong): Boolean;
function VarToType(const aValue: Variant; const aType: TFieldType): Variant;

function GetHValue(rgb: DWORD): Byte;
function GetLValue(rgb: DWORD): Byte;
function GetSValue(rgb: DWORD): Byte;
function GetAValue(rgb: DWORD): Byte;

procedure RGBtoHLS(const R, G, B: LongInt; var H, L, S: LongInt); { H-оттенок, L-яркость, S-насыщенность }
procedure HLStoRGB(const H, L, S: LongInt; var R, G, B: LongInt);
function ChangeBrightnessByValue(const AColor: TColor; const Value: Byte): TColor;
function ChangeBrightnessByDelta(const AColor: TColor; const Delta: Integer): TColor;

function ColorToString(const aColor: TColor): string;
function StringToColor(const aString: string; var aColor: TColor): Boolean;

function IconToString(const aIcon : Integer): string;
function StringToIcon(const aString: string; var aIcon : Integer): boolean;

function GetComputerNetName: string;
//function DiskSerialNo(disk:char):String;
Function GetCPUSpeed: Integer;
function GetCPUName: string;

Function DisableFontColor(const B: Boolean): TColor;

Function SuccStr(str:String):string;
Function ClearName(str:String):string;

/// <summary>Функция укорачивания слишком длинной строки при выводе на экран</summary>
/// <param name="aCaption">Исходная строка</param>
/// <param name="aMax">максимальная длина: "+" строка просто обрезается, "-" в строке средина меняется на мр=ноготочие</param>
/// <returns>Укороченная строка, например "стро...ка"</returns>
function ReduceCaption(aCaption: string; aMax: Integer): String;
function Like(const Text, Mask: String;
              fCaseInsensitive : Boolean = True;
              fSQLMask : Boolean = True;
              fFileMask : Boolean = False): boolean;

function WideStringToUnicode(wStr: WideString): AnsiString;

//function Base64Encode(pData:pointer;iSize:integer):ansistring;
//function Base64Decode(const Value: AnsiString; pData: Pointer; var iSize: Integer): Boolean;

function XMLEncode(const AStr: String): String;
function XMLDecode(const AStr: String): String;

function RTFEncode(const Value: AnsiString): AnsiString;

function IntToName(const X: Integer; const UpperCase: Boolean = True): string;
function SelectCase(aString, aFormat: String): string;
function ConvertDateToString(const Date: TDateTime): string;
Function Units(aValue: Integer; const s1,s2,s5: string) : string;
Function IntToWords(Value: int64; a1000: Int64 = 0): string;
Function GenerateName(a:string): String;
Procedure StrAdd(var Str: string; const Separator, SubStr: string); inline;
Function LiteResize(x1,x2,Step,i,SleepValue:Integer):Integer;
function LiteTimeResize(x1,x2,Step:Integer; Start:TTime):Integer;

Procedure big2small(Source,Dist : TBitMap);
Procedure rgb2varsmall(ILrgb: TImageList; var ILsmall: TImageList);
Procedure rgb2bw(ILrgb: TImageList; var ILbw: TImageList);
Procedure rgb2light(ILrgb: TImageList; var ILbw: TImageList; Border : Boolean);
Procedure rgb2transparent(oldColor,newColor : TColor; var aBitmap: TBitMap);
Procedure transp2color_bw (aColor: TColor; A, B: TBitMap; Transparent: Boolean = True);
function Distance(const A, B : TColor): Integer;
function GetContrastColor(const aFont, aGround: TColor): TColor;
Function DisableColor(const B: Boolean): TColor;

procedure DecodeIcon(aInt: Integer; var Icon1 : Byte; var Color1 : Byte; var IsSymbol1: Byte;
                                    var Icon2 : Byte; var Color2 : Byte; var IsSymbol2: Byte;
                                    var Combination : Byte; var Inverse : Byte);

function  EncodeIcon(Const Icon1 : Byte;     Const Color1 : Byte;     Const IsSymbol1: Byte = 0;
                     Const Icon2 : Byte = 0; Const Color2 : Byte = 0; Const IsSymbol2: Byte = 0;
                     Const Combination : Byte = 0; Const Inverse : Byte = 0 ): Integer;

function GetMicrosoftAccessVersion: Integer;
function GetMicrosoftExcelVersion: Integer;
function GetMicrosoftWordVersion: Integer;
function GetMicrosoftOutlookVersion: Integer;
function GetMicrosoftChartVersion: Integer;
function MicrosoftOfficeVersionToString(const ProductName: string; const Version: Integer): string;

function IsFileExtension(const FileName: string; const Extensions: array of string): Boolean;

type
  TFileVersionFlag =
    (
      fvfDebug,        // Отладочная сборка
      fvfPreRelease,   // Кандидат в релизы
      fvfPatched,      // Файл был пропатчен
      fvfPrivateBuild, // Личная сборка
      fvfSpecialBuild  // Специальная сборка
    );
  TFileVersionFlags = set of TFileVersionFlag;

  TFileVersion = packed record
    Major: Word;
    Minor: Word;
    Release: Word;
    Build: Word;
    Flags: TFileVersionFlags;
  end;

function GetFileVersion(const FileName: string; var FileVersion: TFileVersion; const Product: Boolean = False): Boolean; overload;
function GetFileVersion(const FileName: string; var VersionMS, VersionLS: Cardinal; const Product: Boolean = False): Boolean; overload;

function CompareFileVersion(const FileVersion1, FileVersion2: TFileVersion): Integer;
function FileVersionToString(const FileVersion: TFileVersion; const ShortOnly: Boolean = False): string;
function TryStringToFileVersion(const Text: string; var FileVersion: TFileVersion): Boolean;

function GetMSOLAPVersion: Integer;
//function GetMSOLAPVersion: string;
function GetRegistryIconHandle(const FileName: string): HICON;

Procedure WindowToCenter(Form:TForm);
procedure SetTabStop(Q: TWinControl);
Function GetFirstControl(Q: TWinControl):TWinControl;

//     type TTrayIconOperation = ( tioInsert, tioUpdate, tioDelete);
//procedure TrayIcon(Operation: TTrayIconOperation; Handle: THandle;
//                                           Event: Integer; Ico, Title : String);
function VarIsNullOrEmpty(V: Variant): Boolean;
function ValueForCompare(aValue: Variant; CaseInsensitive: Boolean = False; AccentInsensitive : Boolean = False): Variant;

function IsOLEObjectInstalled(const Name: string): Boolean;
function GetWindowsUserName: string;
function isDigit(const aString: string): Boolean;

function GetInetString(const fileURL: String; var aText:String): boolean;
function GetInetFile(const fileURL, FileName: String): boolean;

function VarArrayPlusVarArray(const L,R: Variant): Variant;

function ValueToStringISO(const aValue: Variant; const aDataType: TFieldType; const NullValue: Variant): Variant;
function StringISOToValue(const aValue: String; const aDataType: TFieldType; const NullValue: Variant): Variant;
function BoolToStr( const aValue : Variant) : string;
function VarToInt(const aValue : Variant) : int64;
function ConvertValue   (const Value: Variant; const aToType: TFieldType): Variant;
function VarIsZero      (const Value: Variant) : boolean;
function FormatValue(const aValue: Variant; const aFormat: string):Variant;
function DeTryStrToBoolean(const Text: string; var Value: Boolean): Boolean;
function DeStrToBoolean(const Text: string; const Default: Boolean = False): Boolean;
function TryStrToWord(const Text: string; out Value: Word): Boolean;

function GetCommonTempFolder: string;
function GetAppDataFolder: string;
function GetDocumentsDirectory(const AllUsers: Boolean = False): string;
function NormalizeFileName(const aName: string; const ReplaceDot: Boolean = False): string;
function NormalizeFilePath(const aName: string; const LastSeparator: Boolean = False): string;
function NextName(aName: string): String;
function DeGetTempFileName(sPath: string; const sPrefix, sExt: string): string;

function GetDefaultPrinter: string;
procedure SetDefaultPrinter(const NewDefPrinter: string);
procedure SetDefaultPrinterByName(const PrinterName: string);

//==============================================================================

/// <summary>
///   Функция сравнения друх вариантных переменных
/// </summary>
/// <param name="A">Первый параметр</param>
/// <param name="B">Второй параметр</param>
/// <returns>Функция вернёт True при условии, что параметры идентичны</returns>
function DeVarSameValue(const A, B: Variant): Boolean;
/// <summary>
///   Функция получения "куска массива"
/// </summary>
/// <param name="Value">Вариантный массив</param>
/// <param name="LowIndex">Нижняя граница массива</param>
/// <param name="HighIndex">Верхняя граница массива</param>
/// <returns>Функция вернёт:
/// <para>если <see cref="Value">Value</see> не массив, то само значение этого параметра;</para>
/// <para>если границы массива <see cref="Value">Value</see> вне диапазона, то значение Unassigned;</para>
/// <para>если границы массива <see cref="Value">Value</see> описывают один элемент, то его значение;</para>
/// <para>иначе "кусок массива" <see cref="Value">Value</see> с границами <see cref="LowIndex">LowIndex</see> и <see cref="HighIndex">HighIndex</see></para>
/// </returns>
function DeVarArrayRange(const Value: Variant; const LowIndex, HighIndex: Integer): Variant;

function CutTextValue(var Text: string; const Separator: String; const IsArrayOfChar: Boolean = False): string;
function FormatString(const Format, Text: string): string;
function WrapString(const Text: string; const LineSizes: array of Word; const Separators: string = ''): string;

/// <summary>
///   Процедура удаления всех файлов в указанном каталоге
/// </summary>
/// <param name="RootDirectory">Каталог для очистки</param>
/// <param name="Recursive">Рекурсивная обработка вложенных подкаталогов</param>
procedure ClearDirectory(const RootDirectory: string; const Recursive: Boolean = False);
function IsEmptyDirectory(const RootDirectory: string): Boolean;
function GetTempDirectory(const SubDirCreating: Boolean = False): string;

procedure WriteLog(const Text: string; const SkipSettings: Boolean = False; const FileName: string = ''); overload;
procedure WriteLog(const Format: string; const Arguments: array of const; const SkipSettings: Boolean = False; const FileName: string = ''); overload;

procedure WriteQueryLog(DataSet: TDataSet; const SkipSettings: Boolean = False; const FileName: string = '');

function ReadStringFromFile(const FileName: string): string;
procedure WriteStringToFile(const FileName, Text: string);

function iif(const Condition: Boolean; const ValueTRUE, ValueFALSE: string): string; overload; inline;
function iif(const Condition: Boolean; const ValueTRUE, ValueFALSE: Integer): Integer; overload; inline;
function iif(const Condition: Boolean; const ValueTRUE, ValueFALSE: TAlign): TAlign; overload; inline;

function IntToStr36(const Value: Int64; const Digits: SmallInt): string;

/// <summary>Функция выполнения XOR над символами в строке.</summary>
function XorAnsiString(const Value, Mask: AnsiString): AnsiString;

/// <summary>Функция кодирования строки в Base64.</summary>
function AnsiStringToBase64(const Value: AnsiString; const CharsPerLine: Integer = 76; const LineSeparator: string = #13): AnsiString;
/// <summary>Функция декодирования строки из Base64.</summary>
function AnsiStringFromBase64(const Value: AnsiString): AnsiString;

{$IFDEF DEBUG}
/// <summary>Функция дампинга строки.</summary>
function DumpAnsiString(const Text: AnsiString; const PrefixLine: AnsiString = ''; const ColCount: Integer = 16; const ShowASCII: Boolean = True; const ShowOffset: Boolean = True): AnsiString;
{$ENDIF}

/// <summary>Функция получения каталога рабочего стола.</summary>
/// <remarks>"C:\Documents and Settings\username\Desktop" или "C:\Users\Public\Desktop" - зависит от параметра.</remarks>
function GetDesktopDirectory(const PublicDesktop: Boolean = False): string;

function TryStringToGUID(const Text: string; var GUID: TGUID): Boolean;
function IsGUID(const Value: Variant): Boolean;
function NewGUID: TGUID;

// Функции работы с Windows Terminal Server ...
function GetWTSClientIP: string;
function GetWTSClientName: string;
function GetWTSDomainName: string;

// Функции работы с WinSock ...
function GetHostIP(const HostName: string): string;

/// <summary> Убираем переводы строк, табуляции, двойные пробелы и излишние внешние скобки </summary>
function TrimFormula(const aText: String): string;
function SameFormula(const aFormula, bFormula: String; const aObject: TObject = nil): Boolean;

function NormalizeCaption(const Text: string; const MaxLength: Integer): string;

procedure GetShellFolderItfPtr(const FolderName: string; Malloc: IMalloc; out TargetFolder: IShellFolder);
function GetExtractImageItfPtr(const FileName: string; out XtractImage: IExtractImage): Boolean;
function ExtractImageGetFileThumbnail(const XtractImage: IExtractImage; ImgWidth, ImgHeight, ImgColorDepth: Integer;
  var Flags: DWORD; out RunnableTask: IRunnableTask; out Bmp: TBitmap): Boolean;

implementation

uses Messages, DateUtils, StrUtils, Variants, Math, Registry, Classes, Printers, WinSpool, ShellAPI, SHFolder,
     ADODB, IBQuery, WinInet, WinSock, System.NetEncoding,
     DeLog, Dictionary, DeMeta, DeTypes;

var  clWindowDisable{, E, D} : TColor;
     RTFEncoder: TStrings;

function CtrlDown(State : TKeyboardState) : Boolean;
begin
  Result := ((State[vk_Control] And 128) <> 0);
end;

function ShiftDown(State : TKeyboardState) : Boolean;
begin
  Result := ((State[vk_Shift] and 128) <> 0);
end;

function AltDown(State : TKeyboardState) : Boolean;
begin
  Result := ((State[vk_Menu] and 128) <> 0);
end;

function TypesCompatible(const aType1, aType2: TFieldType; const aPrecision: TNumericCompare): Boolean;
begin
  result :=
    ((aType1 in StringTypes)   and (aType2 in StringTypes))  or
    ((aType1 in IntegerTypes)  and (aType2 in IntegerTypes)) or
    ((aType1 in FloatTypes)    and (aType2 in FloatTypes)) or
    ((aType1 in IntegerTypes)  and (aType2 in NumericTypes) and (aPrecision = tcInclude) ) or
    ((aType1 in NumericTypes)  and (aType2 in NumericTypes) and (aPrecision = tcLight)   ) or
    ((aType1 = ftBoolean)      and (aType2 =  ftBoolean))    or
    ((aType1 in DateTimeTypes) and (aType2 in DateTimeTypes));
end;

function VarToType(const aValue: Variant; const aType: TFieldType): Variant;
var D: Double;
    P: Integer;
    AFormatSettings: TFormatSettings;
begin
  Result:= aValue;

  case VarType(aValue) of

    varEmpty, varNull: Result:= unassigned;

    varByte, varShortInt, varWord, varSmallInt, varInteger, varLongWord, varInt64, varUInt64:
      try
        if aType in StringTypes then Result:= IntToStr(aValue) else
        if aType in FloatTypes then Result:= aValue;
      except
        Result:= unassigned;
      end;

    varSingle, varDouble, varCurrency:
      try
        if aType in IntegerTypes then Result:= Round(aValue) else
        if aType in StringTypes then Result:= FloatToStr(aValue);
      except
        Result:= unassigned;
      end;

    varString, varOleStr, varUString:
      try
        if aType in FloatTypes then
          begin
           if TryStrToFloat(ReplaceText(VarToStr(aValue), DeDecimalSeparator, FormatSettings.DecimalSeparator), D, FormatSettings)
              then Result:= D
              else Result:= unassigned;
          end else

        if aType in IntegerTypes then
           Result:= StrToIntDef(aValue, unassigned) else

        if aType in LogicalTypes then
          begin
            if SameText('True', aValue) or SameText('1',aValue) then Result:= True;
            if SameText('False',aValue) or SameText('0',aValue) then Result:= False;
          end else

        if aType in [ftTime] then
          begin
            P:= Pos('T', UpperCase(aValue));
            if 0<P then
              Result:= StrToTime(Copy(aValue,P+1,8))
            else
              Result:= StrToTime(aValue)
          end else

        if aType in [ftDate, ftDateTime, ftTimeStamp] then
           begin
             P:= Pos('T', UpperCase(aValue));
             if 0<P then
               begin
                 AFormatSettings.DateSeparator:='-';
                 AFormatSettings.ShortDateFormat:='YYYY-MM-DD';
                 Result:= StrToDate(Copy(aValue,1,P-1), AFormatSettings);
                 Result:= Result + StrToTime(Copy(aValue,P+1,8)) ;
               end
             else
               begin
                 Result:= StrToDate(aValue);
               end;
           end;
      except
        Result:= unassigned;
      end;
  end;

end;

function GetHValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 16);
end;

function GetLValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 8);
end;

function GetSValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb);
end;

function GetAValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 24);
end;

procedure RGBtoHLS(const R, G, B: LongInt; var H, L, S: LongInt);
Var
 cMax,cMin  : integer;
 Rdelta,Gdelta,Bdelta : single;
Begin
   cMax := max( max(R,G), B);
   cMin := min( min(R,G), B);
   L := round( ( ((cMax+cMin)*HLSMAX) + RGBMAX )/(2*RGBMAX) );

   if (cMax = cMin) then begin
      S := 0; H := UNDEFINED;
   end else begin
      if (L <= (HLSMAX/2)) then
         S := round( ( ((cMax-cMin)*HLSMAX) + ((cMax+cMin)/2) ) / (cMax+cMin) )
      else
         S := round( ( ((cMax-cMin)*HLSMAX) + ((2*RGBMAX-cMax-cMin)/2) )
            / (2*RGBMAX-cMax-cMin) );
      Rdelta := ( ((cMax-R)*(HLSMAX/6)) + ((cMax-cMin)/2) ) / (cMax-cMin);
      Gdelta := ( ((cMax-G)*(HLSMAX/6)) + ((cMax-cMin)/2) ) / (cMax-cMin);
      Bdelta := ( ((cMax-B)*(HLSMAX/6)) + ((cMax-cMin)/2) ) / (cMax-cMin);
      if (R = cMax) then H := round(Bdelta - Gdelta)
      else if (G = cMax) then H := round( (HLSMAX/3) + Rdelta - Bdelta)
      else H := round( ((2*HLSMAX)/3) + Gdelta - Rdelta );
      if (H < 0) then H:=H + HLSMAX;
      if (H > HLSMAX) then H:= H - HLSMAX;
   end;
   if S<0 then S:=0; if S>HLSMAX then S:=HLSMAX;
   if L<0 then L:=0; if L>HLSMAX then L:=HLSMAX;
end;

procedure HLStoRGB(const H, L, S: LongInt; var R, G, B: LongInt);
Var
 Magic1,Magic2 : single;

  function HueToRGB(n1, n2, hue: Single): Single;
  begin
     if (hue < 0) then hue := hue+HLSMAX;
     if (hue > HLSMAX) then hue:=hue -HLSMAX;
     if (hue < (HLSMAX/6)) then
        result:= ( n1 + (((n2-n1)*hue+(HLSMAX/12))/(HLSMAX/6)) )
     else
     if (hue < (HLSMAX/2)) then result:=n2 else
     if (hue < ((HLSMAX*2)/3)) then
        result:= ( n1 + (((n2-n1)*(((HLSMAX*2)/3)-hue)+(HLSMAX/12))/(HLSMAX/6)))
     else result:= ( n1 );
  end;

begin
   if (S = 0) then begin
      B:=round( (L*RGBMAX)/HLSMAX ); R:=B; G:=B;
   end else begin
      if (L <= (HLSMAX/2)) then Magic2 := (L*(HLSMAX + S) + (HLSMAX/2))/HLSMAX
      else Magic2 := L + S - ((L*S) + (HLSMAX/2))/HLSMAX;
      Magic1 := 2*L-Magic2;
      R := round( (HueToRGB(Magic1,Magic2,H+(HLSMAX/3))*RGBMAX + (HLSMAX/2))/HLSMAX );
      G := round( (HueToRGB(Magic1,Magic2,H)*RGBMAX + (HLSMAX/2)) / HLSMAX );
      B := round( (HueToRGB(Magic1,Magic2,H-(HLSMAX/3))*RGBMAX + (HLSMAX/2))/HLSMAX );
   end;
   if R<0 then R:=0; if R>RGBMAX then R:=RGBMAX;
   if G<0 then G:=0; if G>RGBMAX then G:=RGBMAX;
   if B<0 then B:=0; if B>RGBMAX then B:=RGBMAX;
end;


function ChangeBrightnessByValue(const AColor: TColor; const Value: Byte): TColor;
var
  V: Byte;
  I, r, g, b, h, l, s: LongInt;
begin
  V := Min(Value, HLSMAX);
  I := ColorToRGB(AColor);
  r := GetRValue(I);
  g := GetGValue(I);
  b := GetBValue(I);
  RGBtoHLS(r, g, b, h, l, s);
  l := V;
  HLStoRGB(h, l, s, r, g, b);
  Result := RGB(r, g, b);
end;

function ChangeBrightnessByDelta(const AColor: TColor; const Delta: Integer): TColor;
var
  I, r, g, b, h, l, s: LongInt;
begin
  I := ColorToRGB(AColor);
  r := GetRValue(I);
  g := GetGValue(I);
  b := GetBValue(I);
  RGBtoHLS(r, g, b, h, l, s);
  l := l + Delta;
  l:= Max(Min(l, HLSMAX), 0);
  HLStoRGB(h, l, s, r, g, b);
  Result := RGB(r, g, b);
end;

function ColorToString(const aColor: TColor): string;
var
  Value: Integer;
begin
  Value := ColorToRGB(aColor);
  Result:='#' + IntToHex(GetBValue(Value), 2)
              + IntToHex(GetGValue(Value), 2)
              + IntToHex(GetRValue(Value), 2);
end;

function StringToColor(const aString: string; var aColor: TColor): Boolean;
var s: String;
begin
  s := Trim(aString);
  if Length(s) <> 7 then Exit(False);

  if (s[1]='#') and CharInSet(s[2], ['0'..'9','a'..'f','A'..'F']) and CharInSet(s[3], ['0'..'9','a'..'f','A'..'F'])
                and CharInSet(s[4], ['0'..'9','a'..'f','A'..'F']) and CharInSet(s[5], ['0'..'9','a'..'f','A'..'F'])
                and CharInSet(s[6], ['0'..'9','a'..'f','A'..'F']) and CharInSet(s[7], ['0'..'9','a'..'f','A'..'F']) then
    begin
      aColor := RGB(StrToInt('$' + Copy(s, 6, 2)), StrToInt('$' + Copy(s, 4, 2)), StrToInt('$' + Copy(s, 2, 2)));
      Result := True;
    end
  else
    Result := False;
end;

function GetFileVersion(const FileName: string; var FileVersion: TFileVersion; const Product: Boolean): Boolean;
var
  BufferSize, Size: Cardinal;
  Buffer: Pointer;
  FileInfo: PVSFixedFileInfo;
  procedure UpdateFileVersionFlag(const dwMask: Cardinal; FileVersionFlag: TFileVersionFlag);
  begin
    if (FileInfo^.dwFileFlags and dwMask) <> 0 then
      Include(FileVersion.Flags, FileVersionFlag);
  end;
begin
  ZeroMemory(@FileVersion, SizeOf(FileVersion));
  BufferSize := GetFileVersionInfoSize(PChar(FileName), Size);
  Result := BufferSize <> 0;
  if Result then
    begin
      GetMem(Buffer, BufferSize);
      try
        Result := GetFileVersionInfo(PChar(FileName), Size, BufferSize, Buffer);
        if Result then
          begin
            Result := VerQueryValue(Buffer, '\', Pointer(FileInfo), BufferSize);
            if Result then
              begin
                if Product then
                  begin
                    FileVersion.Major := HiWord(FileInfo^.dwProductVersionMS);
                    FileVersion.Minor := LoWord(FileInfo^.dwProductVersionMS);
                    FileVersion.Release := HiWord(FileInfo^.dwProductVersionLS);
                    FileVersion.Build := LoWord(FileInfo^.dwProductVersionLS);
                  end
                else
                  begin
                    FileVersion.Major := HiWord(FileInfo^.dwFileVersionMS);
                    FileVersion.Minor := LoWord(FileInfo^.dwFileVersionMS);
                    FileVersion.Release := HiWord(FileInfo^.dwFileVersionLS);
                    FileVersion.Build := LoWord(FileInfo^.dwFileVersionLS);
                  end;
                UpdateFileVersionFlag(VS_FF_DEBUG, fvfDebug);
                UpdateFileVersionFlag(VS_FF_PRERELEASE, fvfPreRelease);
                UpdateFileVersionFlag(VS_FF_PATCHED, fvfPatched);
                UpdateFileVersionFlag(VS_FF_PRIVATEBUILD, fvfPrivateBuild);
                UpdateFileVersionFlag(VS_FF_SPECIALBUILD, fvfSpecialBuild);
              end;
          end;
      finally
        FreeMem(Buffer);
      end;
    end;
end;

function GetFileVersion(const FileName: string; var VersionMS, VersionLS: Cardinal; const Product: Boolean): Boolean;
var
  BufferSize, Size: Cardinal;
  Buffer: Pointer;
  FileInfo: PVSFixedFileInfo;
begin
  VersionMS := 0;
  VersionLS := 0;
  BufferSize := GetFileVersionInfoSize(PChar(FileName), Size);
  Result := BufferSize <> 0;
  if Result then
    begin
      GetMem(Buffer, BufferSize);
      try
        Result := GetFileVersionInfo(PChar(FileName), Size, BufferSize, Buffer);
        if Result then
          begin
            Result := VerQueryValue(Buffer, '\', Pointer(FileInfo), BufferSize);
            if Result then
              if Product then
                begin
                  VersionMS := FileInfo^.dwProductVersionMS;
                  VersionLS := FileInfo^.dwProductVersionLS;
                end
              else
                begin
                  VersionMS := FileInfo^.dwFileVersionMS;
                  VersionLS := FileInfo^.dwFileVersionLS;
                end;
          end;
      finally
        FreeMem(Buffer);
      end;
    end;
end;

function CompareFileVersion(const FileVersion1, FileVersion2: TFileVersion): Integer;
begin
  if FileVersion1.Major < FileVersion2.Major then
    Result := -1
  else if FileVersion1.Major > FileVersion2.Major then
    Result := 1
  else if FileVersion1.Minor < FileVersion2.Minor then
    Result := -1
  else if FileVersion1.Minor > FileVersion2.Minor then
    Result := 1
  else if FileVersion1.Release < FileVersion2.Release then
    Result := -1
  else if FileVersion1.Release > FileVersion2.Release then
    Result := 1
  else if FileVersion1.Build < FileVersion2.Build then
    Result := -1
  else if FileVersion1.Build > FileVersion2.Build then
    Result := 1
  else
    Result := 0;
end;

function FileVersionToString(const FileVersion: TFileVersion; const ShortOnly: Boolean): string;
begin
  if ShortOnly then
    begin
      Result := '%u.%u';
      if FileVersion.Release <> 0 then
        Result := Result + '.%u';
    end
  else
    Result := '%u.%u.%u.%u';
  Result := Format(Result, [FileVersion.Major, FileVersion.Minor, FileVersion.Release, FileVersion.Build]);
end;

function TryStringToFileVersion(const Text: string; var FileVersion: TFileVersion): Boolean;
var
  Value: string;
begin
  Result := False;
  ZeroMemory(@FileVersion, SizeOf(FileVersion));
  Value := Text;
  if TryStrToWord(CutTextValue(Value, '.'), FileVersion.Major) then
    if TryStrToWord(CutTextValue(Value, '.'), FileVersion.Minor) then
      if TryStrToWord(CutTextValue(Value, '.'), FileVersion.Release) then
        if TryStrToWord(CutTextValue(Value, '.'), FileVersion.Build) then
          Result := Length(Value) = 0;
end;

function GetMSOLAPVersion: Integer;
var
  Registry: TRegistry;
  FileName: string;
  VersionMS, VersionLS: Cardinal;
begin
  Result := 0;
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CLASSES_ROOT;
    if Registry.OpenKeyReadOnly('\MSOLAP\CLSID') then
      begin
        FileName := Registry.ReadString(EmptyStr);
        Registry.CloseKey;
        if Registry.OpenKeyReadOnly('\CLSID\' + FileName + '\InprocServer32') then
          begin
            FileName := Registry.ReadString(EmptyStr);
            Registry.CloseKey;
          end
        else
          FileName := EmptyStr;
      end
    else
      FileName := EmptyStr;
  finally
    Registry.Free;
  end;
  if GetFileVersion(FileName, VersionMS, VersionLS) then
    Result := VersionMS;
end;

Function DisableColor(const B: Boolean): TColor;
begin
  if B then DisableColor := clWindow
       else DisableColor := clWindowDisable;
end;

procedure DecodeIcon(aInt: Integer; var Icon1 : Byte; var Color1 : Byte; var IsSymbol1: Byte;
                                    var Icon2 : Byte; var Color2 : Byte; var IsSymbol2: Byte;
                                    var Combination : Byte; var Inverse : Byte);
begin
  Icon1       := (aInt shr $00) and $FF;   // XXXX-XXXX 0000-0000 0000-0000 0000-0000
  Color1      := (aInt shr $08) and $0F;   // 0000-0000 XXXX-0000 0000-0000 0000-0000
  Combination := (aInt shr $0C) and $0F;   // 0000-0000 0000-XXXX 0000-0000 0000-0000

  Icon2       := (aInt shr $10) and $FF;   // 0000-0000 0000-0000 XXXX-XXXX 0000-0000
  Color2      := (aInt shr $18) and $0F;   // 0000-0000 0000-0000 0000-0000 XXXX-0000
  IsSymbol1   := (aInt shr $1C) and $01;   // 0000-0000 0000-0000 0000-0000 0000-X000
  IsSymbol2   := (aInt shr $1D) and $01;   // 0000-0000 0000-0000 0000-0000 0000-0X00
  Inverse     := (aInt shr $1E) and $01;   // 0000-0000 0000-0000 0000-0000 0000-00X0
end;

function  EncodeIcon(Const Icon1 : Byte;     Const Color1 : Byte;     Const IsSymbol1: Byte = 0;
                     Const Icon2 : Byte = 0; Const Color2 : Byte = 0; Const IsSymbol2: Byte = 0;
                     Const Combination : Byte = 0; Const Inverse : Byte = 0 ): Integer;
begin
  Result:=         1*Icon1 +         256*(Color1 and $0F) + 256*16*(Combination and $0F)
           + 256*256*Icon2 + 256*256*256*(Color2 and $0F);
end;


function IconToString(const aIcon : Integer): string;
var Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse: Byte;
begin
  DecodeIcon(aIcon ,Icon1, Color1, IsSymbol1, Icon2, Color2, IsSymbol2, Combination, Inverse);
  Result:=        IntToHex(Icon1, 2) + '.' + IntToHex(Color1, 1) + '.' + IntToHex(IsSymbol1, 1)
          + '-' + IntToHex(Icon2, 2) + '.' + IntToHex(Color2, 1) + '.' + IntToHex(IsSymbol2, 1)
          + '-' + IntToHex(Combination, 1) + '.' + IntToHex(Inverse, 1);
end;

function StringToIcon(const aString: string; var aIcon : Integer): boolean;
var s: String;
begin
  s:=trim(aString);
  if Length(s)<>17 then Exit(False);

  if                    CharInSet(s[ 1], ['0'..'9','a'..'f','A'..'F']) and CharInSet(s[2], ['0'..'9','a'..'f','A'..'F'])
    and (s[ 3]='.') and CharInSet(s[ 4], ['0'..'9','a'..'f','A'..'F'])
    and (s[ 5]='.') and CharInSet(s[ 6], ['0'..'1'])
    and (s[ 7]='-') and CharInSet(s[ 8], ['0'..'9','a'..'f','A'..'F']) and CharInSet(s[9], ['0'..'9','a'..'f','A'..'F'])
    and (s[10]='.') and CharInSet(s[11], ['0'..'9','a'..'f','A'..'F'])
    and (s[12]='.') and CharInSet(s[13], ['0'..'1'])
    and (s[14]='-') and CharInSet(s[15], ['0'..'9','a'..'f','A'..'F'])
    and (s[16]='.') and CharInSet(s[17], ['0'..'1']) then
    begin
      aIcon := EncodeIcon( StrToInt('$'+Copy(s,  1, 2)), StrToInt('$'+Copy(s,  4, 1)), StrToInt('$'+Copy(s,  6, 1)),
                           StrToInt('$'+Copy(s,  8, 2)), StrToInt('$'+Copy(s, 11, 1)), StrToInt('$'+Copy(s, 13, 1)),
                           StrToInt('$'+Copy(s, 15, 1)), StrToInt('$'+Copy(s, 17, 1)) );
      Result:=true;
    end
  else
      Result:=False;
end;

function GetMicrosoftOfficeProductVersion(const ProductName: string): Integer;
var
  Registry: TRegistry;
  function IsSupport(const Version: Word): Boolean;
  begin
    Result := Registry.OpenKeyReadOnly(Format('\%s.Application.%u\', [ProductName, Version]));
  end;
begin
  Result := 0;  // Office Version Not Found
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CLASSES_ROOT;

    if IsSupport(16) then Result :=16 else // Office2016;
    if IsSupport(14) then Result :=14 else // Office2010;
    if IsSupport(12) then Result :=12 else // Office2007;
    if IsSupport(11) then Result :=11 else // Office2003;
    if IsSupport(10) then Result :=10 else // OfficeXP;
    if IsSupport(9) then Result := 9 else // Office2000;
    if IsSupport(8) then Result := 8;     // Office97;

  finally
    Registry.Free;
  end;
end;

function GetMicrosoftAccessVersion: Integer;
//var Reg: TRegistry;
begin
  Result := GetMicrosoftOfficeProductVersion('Access');
  {
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Office\14.0\Access') then Result :=14 else // Office2010;
    if Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Office\12.0\Access') then Result :=12 else // Office2007;
    if Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Office\11.0\Access') then Result :=11 else // Office2003;
    if Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Office\10.0\Access') then Result :=10 else // OfficeXP;
    if Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Office\9.0\Access') then Result := 9 else // Office2000;
    if Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Office\8.0\Access') then Result := 8;     // Office97;

  finally
    Reg.Free;
  end;
  }
end;

function GetMicrosoftExcelVersion: Integer;
begin
  Result := GetMicrosoftOfficeProductVersion('Excel');
end;

function GetMicrosoftWordVersion: Integer;
begin
  Result := GetMicrosoftOfficeProductVersion('Word');
end;

function GetMicrosoftOutlookVersion: Integer;
begin
  Result := GetMicrosoftOfficeProductVersion('Outlook');
end;

function GetMicrosoftChartVersion: Integer;
var
  Registry: TRegistry;
begin
  Result := 0;  // Office Version Not Found
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CLASSES_ROOT;
    if Registry.OpenKeyReadOnly('\OWC11.ChartSpace.11\') then
      Result :=11 // Office2003
    else if Registry.OpenKeyReadOnly('\OWC10.ChartSpace.10\') then
      Result :=10 // OfficeXP
    else if Registry.OpenKeyReadOnly('\TypeLib\{0002E540-0000-0000-C000-000000000046}') then
      Result := 9; // Office2000
  finally
    Registry.Free;
  end;
end;

function MicrosoftOfficeVersionToString(const ProductName: string; const Version: Integer): string;
begin
  case Version of
    16: Result := ProductName + ' 2016';
    14: Result := ProductName + ' 2010';
    12: Result := ProductName + ' 2007';
    11: Result := ProductName + ' 2003';
    10: Result := ProductName + ' XP';
    9: Result := ProductName + ' 2000';
    8: Result := ProductName + ' 97';
  else
    Result := EmptyStr;
  end;
end;

function IsFileExtension(const FileName: string; const Extensions: array of string): Boolean;
var
  Value: string;
  Index: Integer;
begin
  Result := False;
  Value := ExtractFileExt(FileName);
  for Index := Low(Extensions) to High(Extensions) do
    if SameText(Value, Extensions[Index]) then
      begin
        Result := True;
        Break;
      end;
end;

function GetRegistryIconHandle(const FileName: string): HICON;
var
  R: TRegistry;
  Alias, //псевдвним для расширения в реестре
  IconPath: string; //путь для файла с иконкой
  IconNum, //номер иконки в файле
  QPos: Integer; //позиция запятой в записи реестра
begin
  IconNum := 0;
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CLASSES_ROOT;
    //чтение псевданима
    if R.OpenKeyReadOnly('\' + ExtractFileExt(FileName)) then
      Alias := R.ReadString(EmptyStr);
    R.CloseKey;
     //чтение записи об иконке
    if R.OpenKeyReadOnly('\' + Alias + '\DefaultIcon') then
      IconPath := R.ReadString(EmptyStr);
    R.CloseKey;
    //поиск запятой
    QPos := Pos(',', IconPath);
    //чтение номера иконки в файле если она имеется
    if QPos <> 0 then
    begin
      IconNum := StrToInt(Copy(IconPath, QPos + 1, 4));
      IconPath := Copy(IconPath, 1, QPos - 1)
    end;
  finally
    R.Free;
  end;
  //передача хендлера иконки как результат выполнения
  Result := ExtractIcon(hInstance, PChar(IconPath), IconNum);

  //пытаемся достать иконку из самого файла
  if Result = 0 then
    Result := ExtractIcon(hInstance, PChar(FileName), 0);

  //ничего не нашли - иконка по умолчанию
  if Result = 0 then
    Result := ExtractIcon(hInstance, shell32, 0);
end;

function Like(const Text, Mask: String;
              fCaseInsensitive : Boolean = True;
              fSQLMask : Boolean = True;
              fFileMask : Boolean = False): boolean;

var LT, LM : Integer;

  function MskType(Index: Integer): Integer;
  begin
    Result := 0;
    case Mask[Index] of
      '*': if fFileMask then Result := 2;
      '?': if fFileMask then Result := 1;
      '%': if fSQLMask  then Result := 2;
      '_': if fSQLMask  then Result := 1;
    else
      if LM < Index then Result := 3;
    end;
  end;

  function InnerLike(T, M: Word): boolean;
  begin
    if LM < M then
      Result := (LT < T)
    else begin
      case MskType(M) of
        0: begin
            if (0 < T) and (T <= LT) then
              Result := (Text[T] = Mask[M]) or (fCaseInsensitive and (ANSICompareText(Text[T], Mask[M]) = 0))
            else
              Result:=False;

            if Result and ((T < LT) or (M < LM)) then
              Result := InnerLike(T+1, M+1);
          end;
        1: if (0 < T) and (T <= LT) then
            Result := InnerLike(T+1, M+1)
          else
            Result := False;
        2: Result := InnerLike(T, M+1) or ((1 <= T) and (T <= LT) and InnerLike(T+1, M));
        3: Result := (M+1<=LM) and (Text[T] = Mask[M+1]) and ((1 <= T) and (T <= LT) and InnerLike(T+1, M+2))
      else
        Result := False;
      end;
    end;
  end;

begin
  LT:= Length(Text);
  LM:= Length(Mask);
  Result := InnerLike(1, 1);
end;

type
  tTripleByte = array[0..2] of byte;
  pTripleByte =^tTripleByte;

function WideStringToUnicode(wStr: WideString): AnsiString;
var strLen: Integer;
begin
  strLen :=Length(wStr);
  SetLength(Result,strLen*2);

  strLen := WideCharToMultiByte( CP_UTF8, 0, PWideChar(wStr), strLen,
                                             PAnsiChar(Result), strLen*2, nil, nil);
  SetLength(Result,strLen);
end;

{
function Base64Encode(pData:pointer;iSize:integer):ansistring;
var
  nSize, i, n   : integer;
  triCnt,extCnt : integer;
  byte3         : pTripleByte;
  iShift        : integer;
begin
  triCnt := (iSize div 3);
  extCnt := (iSize mod 3);
  nSize  := triCnt*4;
  n      := nSize;
  if (extCnt>0) then
    nSize := nSize + 4;
  setLength(Result,nSize);
  iShift := 0;
  i      := 0;
  while (i<n) do
  begin
    byte3 := @(pByteArray(pData)[iShift]);
    Result[i+1] := Base64[(byte3^[0] and $3F)+1];
    Result[i+2] := Base64[(((byte3^[0] shr 6)+(byte3^[1] shl 2)) and $3F)+1];
    Result[i+3] := Base64[(((byte3^[1] shr 4)+(byte3^[2] shl 4)) and $3F)+1];
    Result[i+4] := Base64[((byte3^[2] shr 2) and $3F)+1];
    inc(iShift,3);
    inc(i,4);
  end;
  byte3 := @(pByteArray(pData)[iShift]);
  if (extCnt=1) then
  begin
    Result[i+1]   := Base64[(byte3^[0] and $3F)+1];
    Result[i+2] := Base64[((byte3^[0] shr 6) and $3F)+1];
    Result[i+3] := '=';
    Result[i+4] := '=';
  end
  else if (extCnt=2) then begin
    Result[i+1]   := Base64[(byte3^[0] and $3F)+1];
    Result[i+2] := Base64[(((byte3^[0] shr 6)+(byte3^[1] shl 2)) and $3F)+1];
    Result[i+3] := Base64[((byte3^[1] shr 4) and $3F)+1];
    Result[i+4] := '=';
  end;
end;

function Base64Decode(const Value: AnsiString; pData: Pointer; var iSize: Integer): Boolean;
var
  l,i,nSize  : integer;
  byte4      : array[0..3] of byte;
  iShift     : integer;
begin
  l := Length(Value);
  Result := ((l mod 4)=0);
  if not(Result) then
  begin
    iSize :=-1;
  end
  else begin
    if l=0 then
    begin
      iSize := 0;
    end
    else begin
      nSize := (l div 4)*3;
      if (Value[l]='=') then begin
        nSize := nSize-1;
        if (Value[l-1]='=') then
          nSize := nSize-1;
      end;
      Result := (iSize>=nSize);
      if Result then begin
        i      := 1;
        iShift := 0;
        while (i<l) do begin
          byte4[0] := deBase64[Ord(Value[i])];
          byte4[1] := deBase64[Ord(Value[i+1])];
          byte4[2] := deBase64[Ord(Value[i+2])];
          byte4[3] := deBase64[Ord(Value[i+3])];
          pByteArray(pData)^[iShift]:=byte4[0]+(byte4[1] shl 6);
          if (iShift+1)<nSize then begin
            pByteArray(pData)^[iShift+1]:=(byte4[1] shr 2)+(byte4[2] shl 4);
            if (iShift+2)<nSize then
              pByteArray(pData)^[iShift+2]:=(byte4[2] shr 4)+(byte4[3] shl 2);
          end;
          inc(i,4);
          inc(iShift,3);
        end;
      end;
      iSize := nSize;
    end;
  end;
end;
}

function RTFEncode(const Value: AnsiString): AnsiString;
var i:Integer;
begin
  Result := EmptyStr;
  for i:=1 to Length(Value) do
    if (i>1) and (Ord(Value[i-1]) in [10,13]) and
                 (Ord(Value[ i ]) in [10,13]) and
                 (Ord(Value[i-1]) <> Ord(Value[i])) then
    else
      //result:= result + RTFChar[Ord(Value[i])];
      Result := Result + AnsiString(RTFEncoder[Ord(Value[i])]);
end;
//==============================================================================
function XMLEncode(const AStr: String): String;
var I: Integer;
begin
  Result := EmptyStr;

  for I := Length(AStr) downto 1 do
    case AStr[i] of
      Chr(1)..Chr(31): Result := '&#' + Chr(48 + ord(AStr[i]) div 10) + Chr(48 + ord(AStr[i]) mod 10) + ';' + Result;
            Chr(127) : Result := '&#127;' + Result;
                '&'  : Result := '&amp;'  + Result;
                '<'  : Result := '&lt;'   + Result;
                '>'  : Result := '&gt;'   + Result;
                '"'  : Result := '&quot;' + Result;
               ''''  : Result := '&apos;' + Result;
                else   Result := AStr[i]  + Result;
    end;
end;

function XMLDecode(const AStr: String): String;
const sInvalidXMLEncodedChar = 'Invalid XML encoded character (%s) at position %d';
var
  I, L: Integer;
begin
  Result := EmptyStr;
  i:=1;
  L:=Length(AStr);
  while (i<=L) do
    if AStr[i]='&' then
      begin
        if (Copy(AStr,i+1,4) = 'amp;')  then begin Result := Result + '&'; Inc(i,5); end else
        if (Copy(AStr,i+1,3) = 'lt;')   then begin Result := Result + '<'; Inc(i,4); end else
        if (Copy(AStr,i+1,3) = 'gt;')   then begin Result := Result + '>'; Inc(i,4); end else
        if (Copy(AStr,i+1,5) = 'quot;') then begin Result := Result + '"'; Inc(i,6); end else
        if (Copy(AStr,i+1,5) = 'apos;') then begin Result := Result +''''; Inc(i,6); end else
        if (Copy(AStr,i+1,5) = '#127;') then begin Result := Result + Chr(127); Inc(i,6); end else
        if ( AStr[i+1] = '#') and CharInSet(AStr[i+2], ['0'..'9']) and CharInSet(AStr[i+3], ['0'..'9']) and (AStr[i+4] = ';')
                                        then begin Result := Result + Chr(StrToInt(Copy(AStr,i+2,2))); Inc(i,4); end else
        raise EConvertError.CreateFmt(sInvalidXMLEncodedChar, [ AStr[i], i ]);
      end
    else
      begin
        Result:=Result+AStr[i];
        Inc(i);
      end;
end;
//==============================================================================
function GetComputerNetName: string;
var
  buffer: array[0..MAX_COMPUTERNAME_LENGTH] of char;
  size: dword;
begin
  size := MAX_COMPUTERNAME_LENGTH;
  if GetComputerName(buffer, size) then
    Result := StrPas(buffer)
  else
    Result := EmptyStr;
end;
//==============================================================================
{
function DiskSerialNo(disk:char):String;
var VolumeName                           : array [0..MAX_PATH-1] of Char;
    VolumeSerialNo                       : LongInt;
    MaxComponentLength, FileSystemFlags  : Cardinal;
    DiskName                             : String;
begin
  DiskName :=Disk+':\';
  // А где SetErrorMode?!?!?
  if not GetVolumeInformation(PChar(DiskName), VolumeName,MAX_PATH,
         @VolumeSerialNo, MaxComponentLength, FileSystemFlags, nil, MAX_PATH)
  then VolumeSerialNo:=0;
  DiskSerialNo:=IntToStr(VolumeSerialNo);
end;
}
//==============================================================================
function GetInetString(const fileURL: String; var aText:String): boolean;
const
  BufferSize = 1024;
var
  hSession, hURL: HInternet;
  Buffer: array[1..BufferSize] of Byte;
  BufferLen: DWORD;
  sAppName : string;
  i : Integer;
begin
   Result:= False;
   aText := EmptyStr;
   sAppName := ExtractFileName(Application.ExeName);
   hSession := InternetOpen(PChar(sAppName), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
   try
      hURL := InternetOpenURL(hSession, PChar(fileURL), nil, 0,
      INTERNET_FLAG_RELOAD + INTERNET_FLAG_NO_UI + INTERNET_FLAG_NO_CACHE_WRITE + INTERNET_FLAG_EXISTING_CONNECT, 0);
      if hURL<>nil then
      try
         repeat
            InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen);
            for i:=1 to BufferLen do
              aText:=aText+Char(Buffer[i]);
         until BufferLen = 0;
         Result:=True;
      finally
      InternetCloseHandle(hURL)
      end
   finally
   InternetCloseHandle(hSession);
   end
end;
//==============================================================================

function GetInetFile(const fileURL, FileName: String): boolean;
const
  BufferSize = 1024;
var
  hSession, hURL: HInternet;
  Buffer: array[1..BufferSize] of Byte;
  BufferLen: DWORD;
  f : File;
  sAppName : string;
begin
   Result:=False;
   sAppName := ExtractFileName(Application.ExeName);
   hSession := InternetOpen(PChar(sAppName), INTERNET_OPEN_TYPE_PRECONFIG,
         nil, nil, 0);
   try
      hURL := InternetOpenURL(hSession, PChar(fileURL), nil, 0, 0, 0);
      if hURL<>nil then
      try
         AssignFile(f, FileName);
         Rewrite(f,1);
         repeat
            InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen);
            BlockWrite(f, Buffer, BufferLen);
         until BufferLen = 0;
         CloseFile(f);
         Result:=True;
      finally
      InternetCloseHandle(hURL)
      end
   finally
   InternetCloseHandle(hSession);
   end
end;

//==============================================================================
function isDigit(const aString: string): Boolean;
var
  i:Integer;
begin
  Result:=True;
  for i:=1 to Length(aString) do
    if Not CharInSet(aString[i], ['0'..'9']) then
      begin
        Result:=False;
        Break;
      end;
end;
//==============================================================================
function IsOLEObjectInstalled(const Name: string): Boolean;
var ClassID: TCLSID;
begin
  Result := Succeeded(CLSIDFromProgID(PWideChar(WideString(Name)), ClassID));
end;
//==============================================================================
function GetWindowsUserName: string;
var
  UserName : string;
  UserNameLen : Dword;
begin
  UserNameLen := 255;
  SetLength(userName, UserNameLen);
  if GetUserName(PChar(UserName), UserNameLen) then
    Result := Copy(UserName,1,UserNameLen - 1)
  else
    Result := EmptyStr;
end;

{$REGION 'CPU Information ...'}
{
// Убрал и заменил на реестровую версию, т.к. для новых процессоров расчёты неправильные!
function GetCPUSpeed: Integer;
const DelayTime = 100;
var   TimerHi, TimerLo: DWORD;
      PriorityClass, Priority: Integer;
begin
  PriorityClass := GetPriorityClass(GetCurrentProcess);
  Priority := GetThreadPriority(GetCurrentThread);
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL);
  Sleep(10);
  asm
    dw 310Fh // rdtsc
    mov TimerHi, edx
    mov TimerLo, eax
  end;
  Sleep(DelayTime);
  asm
    dw 310Fh // rdtsc
    sub eax, TimerLo
    sbb edx, TimerHi
    mov TimerLo, eax
    mov TimerHi, edx
  end;
  SetThreadPriority(GetCurrentThread, Priority);
  SetPriorityClass(GetCurrentProcess, PriorityClass);
  Result := 1000*Round(TimerLo / (1000*DelayTime));
end;
}
const
  cCentralProcessorSection = '\HARDWARE\DESCRIPTION\System\CentralProcessor\0';

function GetCPUSpeed: Integer;
const
  cTickCountIdent = '~MHz';
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(cCentralProcessorSection) and ValueExists(cTickCountIdent) then
        Result := ReadInteger(cTickCountIdent)
      else
        Result := 0;
    finally
      Free;
    end;
end;

function GetCPUName: string;
const
  cProcessorNameStringIdent = 'ProcessorNameString';
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(cCentralProcessorSection) and ValueExists(cProcessorNameStringIdent) then
        Result := ReadString(cProcessorNameStringIdent)
      else
        Result := EmptyStr;
    finally
      Free;
    end;
  while Pos('  ', Result) <> 0 do
    Result := ReplaceText(Result, '  ', ' ');
end;
{$ENDREGION}

procedure SetTabStop(Q: TWinControl);
var i,j : Integer;
begin
  for i:=0 to Q.ControlCount-1  do
    if (Q.Controls[i] is TWinControl)and(TWinControl(Q.Controls[i]).TabStop) then
      for j:=0 to Q.ControlCount-1  do
        if (Q.Controls[j] is TWinControl)and(TWinControl(Q.Controls[j]).TabStop) then
          if (TWinControl(Q.Controls[i]).TabOrder<TWinControl(Q.Controls[j]).TabOrder)
             xor ((Q.Controls[i].Top*10000+Q.Controls[i].Left) <
                  (Q.Controls[j].Top*10000+Q.Controls[j].Left)) then
            TWinControl(Q.Controls[i]).TabOrder := TWinControl(Q.Controls[j]).TabOrder;
end;
//==============================================================================
Function GetFirstControl(Q: TWinControl):TWinControl;
var i:Integer;
begin
  Result:=nil;
  for i:=0 to Q.ControlCount-1 do
    if (Q.Controls[i] is TWinControl) then
      if (TWinControl(Q.Controls[i]).TabStop) then
        if (Result=nil) or (TWinControl(Q.Controls[i]).TabOrder < Result.TabOrder) then
           Result:=TWinControl(Q.Controls[i]);
end;
//==============================================================================
Procedure WindowToCenter(Form:TForm);
begin
  Form.SetBounds
    ( Application.MainForm.Left+(Application.MainForm.Width -Form.Width ) div 2,
      Application.MainForm.Top +(Application.MainForm.Height-Form.Height) div 2,
      Form.Width,
      Form.Height);
end;
//==============================================================================
function DisableFontColor(const B: Boolean): TColor;
begin
  if B then DisableFontColor := clBtnText
       else DisableFontColor := clGrayText;
end;
//..............................................................................
procedure StrAdd(var Str: string; const Separator, SubStr: string);
begin
  if 0 = Length(Str) then Str:= SubStr
                     else if 0 < Length(SubStr) then Str:= Str + Separator + SubStr;
end;
//..............................................................................
Function SuccStr(str:String):string;
var i:integer;
    s:string;
begin
  s:=str;
  i:=Length(s);
  while (i>0)and(s[i]>'8') do
    begin
      s[i]:='0';
      Dec(i);
    end;
  if i=0 then s:='1'+s
         else s[i]:=Succ(s[i]);
  SuccStr:=s;
end;
//..............................................................................
Function ClearName(str:String):string;
var L: integer;
begin
  result:=Trim(str);
  if pos(';',result)=Length(result) then
    Delete(result,length(result),1);

  L:=Length(result);
  if L>2 then
    if ((result[1]='''') and (result[L]='''')) or
       ((result[1]= '"') and (result[L]='"'))  or
       ((result[1]= '[') and (result[L]=']')) then result:=Copy(result,2,L-2);
end;
//..............................................................................
function ReduceCaption(aCaption: string; aMax: Integer): String;
var N: Integer;
begin
  if Length(aCaption) <= abs(aMax) then Exit(aCaption);

  if 0 < aMax then Exit(Copy(aCaption, 1, aMax - 3) + ' ..');

  N:= abs(aMax) div 4;
  Result:= Copy(aCaption, 1, abs(aMax) - N - 4)+' .. '+Copy(aCaption, Length(aCaption) + 1 - N, N);
end;
//..............................................................................
Function GenerateName(a:string): String;
var i:Integer;
begin
  result:= EmptyStr;
  for i:=1 to Length(a) do
    result:= result+Chr(65+(Ord(a[i]) div 16)) + Chr(65+(Ord(a[i]) mod 16))
end;
//..............................................................................
function IntToName(const X: Integer; const UpperCase: Boolean): string;
begin
  if UpperCase then
    begin
      if x>=26 then result:=CHR(64+(x div 26))+CHR(65+(x mod 26))
               else result:=CHR(65+((x+26) mod 26))
    end                          //    64 @ 96 `
  else                           //    65 A 97 a
    begin
      if x>=26 then result:=CHR(96+(x div 26))+CHR(97+(x mod 26))
               else result:=CHR(97+((x+26) mod 26))
    end;
end;
//..............................................................................
function SelectCase(aString, aFormat: string): string;
begin
  if aFormat[1] = AnsiUpperCase(aFormat[1]) then
    if aFormat[2] = AnsiUpperCase(aFormat[2]) then Result := AnsiUpperCase(aString)
                                              else Result := AnsiUpperCase(Copy(aString,1,1)) + AnsiLowerCase(Copy(aString,2,MaxInt))
                                            else
    if aFormat[2] = AnsiUpperCase(aFormat[2]) then Result := aString
                                              else Result := AnsiLowerCase(aString);
end;
//..............................................................................
const
  MonthNames: array[1..12] of string = ('января', 'февраля', 'марта', 'апреля',
    'мая', 'июня', 'июля', 'августа', 'сентября', 'октября', 'ноября', 'декабря');
  NamesOfThousand0: array[1..2] of string = ('тысячного', 'двухтысячного');
  NamesOfThousand: array[1..2] of string = ('одна тысяча', 'две тысячи');
  NamesOfHandreds0: array[1..9] of string = ('сотого', 'двухсотого', 'трехсотого', 'четырехсотого',
    'пятисотого', 'шестисотого', 'семисотого', 'восьмисотого', 'девятисотого');
  NamesOfHandreds: array[1..9] of string =('сто', 'двести', 'триста', 'четыреста',
    'пятьсот', 'шестьсот', 'семьсот', 'восемьсот', 'девятьсот');
  NamesOfTens00: array[1..3] of string =('десятое', 'двадцатое', 'тридцатое');
  NamesOfTens0: array[1..9] of string =('десятого', 'двадцатого', 'тридцатого', 'сорокового',
    'пятидесятого', 'шестидесятого', 'семидесятого', 'восьмидесятого', 'девяностого');
  NamesOfTens: array[2..9] of string = ('двадцать', 'тридцать', 'сорок',
    'пятьдесят', 'шестьдесят', 'семьдесят', 'восемьдесят', 'девяносто');
  NamesOfFistTen0: array[1..9] of string = ('одиннадцатое', 'двенадцатое', 'тринадцатое', 'четырнадцатое',
    'пятнадцатое', 'шестнадцатое', 'семнадцатое', 'восемнадцатое', 'девятнадцатое');
  NamesOfFistTen: array[1..9] of string = ('одиннадцатого', 'двенадцатого', 'тринадцатого', 'четырнадцатого',
    'пятнадцатого', 'шестнадцатого', 'семнадцатого', 'восемнадцатого', 'девятнадцатого');
  NamesOfUnits0: array[1..9] of string = ('первое', 'второе', 'третье', 'четвертое',
    'пятое', 'шестое', 'седьмое', 'восьмое', 'девятое');
  NamesOfUnits: array[1..9] of string = ('первого', 'второго', 'третьего',
    'четвертого', 'пятого', 'шестого', 'седьмого', 'восьмого', 'девятого');
//..............................................................................
function ConvertDateToString(const Date: TDateTime): string;
resourcestring
  sYear = ' года';
var
  year, month, day: word;
  th, hn, tn, un: word;
begin
  DecodeDate(Date, year, month, day);
  // разбираем число
  tn := day div 10;
  un := day mod 10;
  if un = 0 then
    result := NamesOfTens00[tn]
  else if tn = 0 then
    result := NamesOfUnits0[un]
  else if tn = 1 then
    result := NamesOfFistTen0[un]
  else
    result := NamesOfTens[tn] + ' ' + NamesOfUnits0[un];
  // разбираем месяц
  result := result + ' ' + MonthNames[month];
  // разбираем год
  th := year div 1000;
  hn := year mod 1000 div 100;
  tn := year mod 1000 mod 100 div 10;
  un := year mod 1000 mod 100 mod 10;
  if ((hn = 0) and (tn = 0) and (un = 0)) then
  begin
    Result := Result + ' ' + NamesOfThousand0[th] + sYear;
    exit;
  end
  else
    result := result + ' ' + NamesOfThousand[th];
  if hn > 0 then
  begin
    if ((tn = 0) and (un = 0)) then
    begin
      Result := Result + ' ' + NamesOfHandreds0[hn] + sYear;
      exit;
    end
    else
      result := result + ' ' + NamesOfHandreds[hn];
  end;
  if un = 0 then
    result := result + ' ' + NamesOfTens0[tn]
  else if tn = 0 then
    result := result + ' ' + NamesOfUnits[un]
  else if tn = 1 then
    result := result + ' ' + NamesOfFistTen[un]
  else
    result := result + ' ' + NamesOfTens[tn] + ' ' + NamesOfUnits[un];
  Result := Result + sYear;
end;

Function Units(aValue: Integer; const s1,s2,s5: string) : string;
begin
  if (10 < (aValue mod 100)) and ((aValue mod 100) < 20) then Result := s5 else
  if ( 1 = (aValue mod  10))                             then Result := s1 else
  if ( 1 < (aValue mod  10)) and ((aValue mod  10) < 5 ) then Result := s2 else
  if ( 0 < (aValue))                                     then Result := s5;
end;

function IntToWords(Value: int64; a1000: Int64 = 0): string;   //TODO 10: Сделать английский язык
var
   Rend: boolean;
   ValueTemp: int64;
    //..........................................................................
    procedure Num(Value: byte);
    begin
      case Value of
        1: if Rend then Result := Result + 'один ' else Result := Result + 'одна ';
        2: if Rend then Result := Result + 'два ' else Result := Result + 'две ';
        3: Result := Result + 'три ';
        4: Result := Result + 'четыре ';
        5: Result := Result + 'пять ';
        6: Result := Result + 'шесть ';
        7: Result := Result + 'семь ';
        8: Result := Result + 'восемь ';
        9: Result := Result + 'девять ';
        10: Result := Result + 'десять ';
        11: Result := Result + 'одиннадцать ';
        12: Result := Result + 'двенадцать ';
        13: Result := Result + 'тринадцать ';
        14: Result := Result + 'четырнадцать ';
        15: Result := Result + 'пятнадцать ';
        16: Result := Result + 'шестнадцать ';
        17: Result := Result + 'семнадцать ';
        18: Result := Result + 'восемнадцать ';
        19: Result := Result + 'девятнадцать ';
      end
    end;
    //..........................................................................
    procedure Num10(Value: byte);
    begin
      case Value of
        2: Result := Result + 'двадцать ';
        3: Result := Result + 'тридцать ';
        4: Result := Result + 'сорок ';
        5: Result := Result + 'пятьдесят ';
        6: Result := Result + 'шестьдесят ';
        7: Result := Result + 'семьдесят ';
        8: Result := Result + 'восемьдесят ';
        9: Result := Result + 'девяносто ';
      end;
    end;
    //..........................................................................
    procedure Num100(Value: byte);
    begin
      case Value of
        1: Result := Result + 'сто ';
        2: Result := Result + 'двести ';
        3: Result := Result + 'триста ';
        4: Result := Result + 'четыреста ';
        5: Result := Result + 'пятьсот ';
        6: Result := Result + 'шестьсот ';
        7: Result := Result + 'семьсот ';
        8: Result := Result + 'восемьсот ';
        9: Result := Result + 'девятьсот ';
      end
    end;
    //..........................................................................
    //На входе число от 1 до 999
    procedure Num00;
    begin
      //Добавляем сотни если они есть
      Num100(ValueTemp div 100);
      //Отбрасываем сотни
      ValueTemp := ValueTemp mod 100;
      //Если меньше 20, то добавляем число прописью от 1 до 19
      if ValueTemp < 20
        then Num(ValueTemp)
        else begin
           //Добавляем десятки
           Num10(ValueTemp div 10);
           //Отбрасываем десятки
           ValueTemp := ValueTemp mod 10;
           //Добавляем число прописью от 1 до 9
           Num(ValueTemp);
         end;
    end;
    //..........................................................................
    //Mult-Предел обработки числа
    //s1- единственное число, именительный падеж (например ‘миллион’)
    //s2- единственное число, родительный падеж (например ‘миллиона’)
    //s3- множественное число, родительный падеж (например ‘миллионов’)
    procedure NumMult(Mult: int64; s1,s2,s3: string);
    var ValueRes: int64;
    begin
       //Если число больше предела обработки, то обрабатываем
       if Value >= Mult then
        begin
           //Выделяем число в диапазоне от 1 до 999
           ValueTemp := Value div Mult;
           ValueRes := ValueTemp;
           //Добавляем число прописью в диапазоне от 1 до 999
           Num00;
           //Добавляем обозначение числа в диапазоне от 1 до 999 (например, миллионов)
           if ValueTemp = 1 then Result := Result + s1
           else if (1 < ValueTemp) and (ValueTemp < 5) then Result := Result + s2
           else Result := Result + s3;
           //Вычитаем обработанное число
           Value := Value - Mult * ValueRes;
         end;
    end;

    //..........................................................................

Begin
   //Определяем если ноль
   if (Value = 0) then
       Result := 'ноль'
   else
     begin
       Result := EmptyStr;

       Rend := true; //устанавливаем окончания мужского рода (триллион, миллиард, миллион)
       NumMult(1000000000000000000,'квинтиллион','квинтиллиона','квинтиллионов');
       NumMult(1000000000000000,'квадриллион','квадриллиона','квадриллионов');
       NumMult(1000000000000,'триллион ','триллиона ','триллионов ');
       NumMult(1000000000,'миллиард ','миллиарда ','миллиардов ');
       NumMult(1000000,'миллион ','миллиона ','миллионов ');

       Rend := false; //устанавливаем окончания женского рода (тысячи)
       NumMult(1000,'тысяча ','тысячи ','тысяч ');

       Rend := true; //устанавливаем окончания мужского рода
       ValueTemp := Value;
       Num00;

       Rend := false; //устанавливаем окончания женского рода (десятая, сотая, тысячная)  1/10 1/100 1/1000
       if a1000 mod 10 > 0 then
         begin
           ValueTemp := a1000 div 1;
           Result := Result + 'и ';
           Num00;
           Result := Result + Units(ValueTemp, 'тысячная', 'тысячные', 'тысячных');
         end else
       if a1000 mod 100  > 0 then
         begin
           ValueTemp :=  a1000 div 10;
           Result := Result + 'и ';
           Num00;
           Result := Result + Units(ValueTemp, 'сотая', 'сотые', 'сотых');
         end else
       if a1000         > 0 then
         begin
           ValueTemp :=  a1000 div 100;
           Result := Result + 'и ';
           Num00;
           Result := Result + Units(ValueTemp, 'десятая', 'десятые', 'десятых');
         end;
       Result:= Trim(Result);
     end;
end;
//..............................................................................
function LiteResize(x1,x2,Step,i,SleepValue:Integer):Integer;
begin
  if (i=0) or (i=Step) then
    LiteResize:=x2
  else
    begin
      LiteResize:=x1+Round((x2-x1)*0.5*(1-Cos(i*pi/Step)));
      sleep(SleepValue);
    end;
end;
//..............................................................................
function LiteTimeResize(x1,x2,Step:Integer; Start:TTime):Integer;
var H,M,S,T : Word;
    i:Double;
begin
  DecodeTime(Time-Start,H,M,S,T);
  i:=(S*1000+T)/20;
  if i>=Step then
    Result:=x2
  else
    Result:=x1+Round((x2-x1)*0.5*(1-Cos(i*pi/Step)));
end;
//==============================================================================
Procedure big2small(Source,Dist : TBitMap);
var W,H,i,j,c    : Integer;
    ps1,ps2,ps3  : PByteArray;
begin
  H:=Source.Height div 2;
  W:=Source.Width  div 2;
  for j:=0 to H-1 do
  begin ps1 := Source.ScanLine[2*j  ];
        ps2 := Source.ScanLine[2*j+1];
        ps3 := Dist.  ScanLine[  j  ];
        for i:=0 to W-1 do
        for c:=0 to 2   do
          ps3[3*i+c]:=(ps1[6*i+c]+ps1[6*i+3+c]+ps2[6*i+c]+ps2[6*i+3+c]) div 4;
  end;
end;

Procedure rgb2varsmall(ILrgb: TImageList; var ILsmall: TImageList);
var BM,BMS        : TBitMap;
    psr           : PByteArray;
    ps            : Array of PByteArray;
    x, y, px, py, W, H, BMCount : Integer;
    r, g, b, xini, xfi, yini, yfi, saltx, salty, dpix, tpix: single;

 function MyTrunc(const X: single): Integer;
 begin
   if Frac(x)=0 then Result := Trunc(x)-1
                else Result := Trunc(x);
 end;
begin
  BMCount:=ILrgb.Count;
  H :=ILrgb.Height;
  W :=ILrgb.Width;
  BM :=TBitMap.Create;
  BM.PixelFormat := pf24Bit;
  BM.Height:=H;
  BM.Width :=W*BMCount;
  for x:=0 to ILrgb.Count-1 do
    ILrgb.Draw( BM.Canvas, x*W,0,x);

  SetLength(ps,BM.Height);
  for y:=0 to BM.Height-1 do
    ps[y]:=BM.ScanLine[ y ];

  H:=ILsmall.Height;
  W:=ILsmall.Width;
  BMS:=TBitMap.Create;
  BMS.PixelFormat := pf24Bit;
  BMS.Height:=H;
  BMS.Width :=W*BMCount;

  saltx := BM.Width/BMS.Width;
  salty := BM.Height/BMS.Height;

  yfi := 0;
  for y := 0 to BMS.Height - 1 do
    begin
      psr := BMS.ScanLine[ y ];

      yini := yfi;
      yfi := yini + salty;
      if yfi >= BM.Height then yfi := BM.Height - 1;

      xfi := 0;
      for x := 0 to BMS.Width - 1 do
        begin
          xini := xfi;
          xfi := xini + saltx;
          if xfi >= BM.Width then xfi := BM.Width - 1;

          r := 0;
          g := 0;
          b := 0;
          tpix := 0;

          for py := Trunc(yini) to MyTrunc(yfi) do
            for px := Trunc(xini) to MyTrunc(xfi) do
              begin
                dpix:=1;
                if py<yini  then dpix:=dpix*(yini-py);
                if py+1>yfi then dpix:=dpix*(py+1-yfi);

                if px<xini  then dpix:=dpix*(xini-px);
                if px+1>xfi then dpix:=dpix*(px+1-xfi);

                b := b + ps[py][px*3+0]*dpix;
                g := g + ps[py][px*3+1]*dpix;
                r := r + ps[py][px*3+2]*dpix;
                tpix:=tpix+dpix;
              end;

          psr[x*3+0] := Trunc(b / tpix);
          psr[x*3+1] := Trunc(g / tpix);
          psr[x*3+2] := Trunc(r / tpix);
        end;
    end;

  ILsmall.AddMasked(BMS, clFuchsia);
  SetLength(ps,0);
  BM.Free;
  BMS.Free;
end;
//==============================================================================
Procedure rgb2bw(ILrgb: TImageList; var ILbw: TImageList);
var H,W,i,j,BMCount,q : Integer;
    c      : TColor;
    bg     : DWord;
    BM     : TBitMap;
    ps     : PByteArray;
begin
  bg:=ILrgb.bkColor;
  BMCount:=ILrgb.Count;
  H:=ILrgb.Height;
  W:=ILrgb.Width;

  BM:=TBitMap.Create;
  BM.PixelFormat := pf24Bit;
  BM.Height:=H;
  BM.Width :=W*BMCount;

  for j:=0 to BMCount-1 do
    ILrgb.Draw( BM.Canvas, j*W,0,j);

  for j:=0 to H-1 do
  begin ps := BM.ScanLine[j];
        for i:=0 to (W*BMCount)-1 do
        begin q:=i*3;
              if Not (RGB(ps[q+2],ps[q+1],ps[q+0])= bg) then
                begin
                  c:=(36*ps[q+2]+53*ps[q+1]+11*ps[q+0]) div 100;
                  FillChar(ps[q],3,c);
                end;
        end;
  end;

  ILbw.AddMasked(BM,clFuchsia);
  BM.Free;
end;
//==============================================================================
Procedure rgb2light(ILrgb: TImageList; var ILbw: TImageList; Border : Boolean);
var H,W,i,j,BMCount,q : Integer;
    c,_c : TColor;
    BM   : TBitMap;
    ps   : PByteArray;
begin
  _c:=GetSysColor(COLOR_ACTIVECAPTION);

  BMCount:=ILrgb.Count;
  H:=ILrgb.Height;
  W:=ILrgb.Width;

  BM:=TBitMap.Create;
  BM.PixelFormat := pf24Bit;
  BM.Height:=H;
  BM.Width :=W*BMCount;

  ILrgb.Masked:=False;
  for j:=0 to BMCount-1 do
    ILrgb.Draw( BM.Canvas, j*W,0,j);

  for j:=0 to H-1 do
  begin ps := BM.ScanLine[j];
        for i:=0 to (BMCount*W)-1 do
        begin q:=3*i;
          if (Border) and ((i mod W =0) or (i mod W=W-1) or (j=0) or (j=H-1))
          then begin ps[q+2]:=GetRValue(_c);
                     ps[q+1]:=GetGValue(_c);
                     ps[q  ]:=GetBValue(_c);
               end
          else begin c:=(13*ps[q  ]) div 10; if c>255 then c:=255; ps[q  ]:=c;
                     c:=(13*ps[q+1]) div 10; if c>255 then c:=255; ps[q+1]:=c;
                     c:=(13*ps[q+2]) div 10; if c>255 then c:=255; ps[q+2]:=c;
               end;
        end;
  end;

  ILbw.AddMasked(BM,clFuchsia);
  BM.Free;
end;
//==============================================================================
Procedure transp2color_bw (aColor: TColor; A, B: TBitMap; Transparent: Boolean = True);
var H,W,i,j,q,rr,gg,bb : Integer;
    ps1,ps2,pr : PByteArray;
    trans : array[0..2] of byte;
    c:TColor;
begin

  rr:=GetRValue(ColorToRGB(aColor));
  gg:=GetGValue(ColorToRGB(aColor));
  bb:=GetBValue(ColorToRGB(aColor));

  trans[0]:=GetBValue(ColorToRGB(clFuchsia));
  trans[1]:=GetGValue(ColorToRGB(clFuchsia));
  trans[2]:=GetRValue(ColorToRGB(clFuchsia));

  H:=A.Height div 2;
  W:=A.Width;

  for j:=0 to H-1 do
  begin ps1 := A.ScanLine[ j ];
        ps2 := A.ScanLine[H+j];
        pr  := B.ScanLine[ j ];
        for i:=0 to  W-1 do
          begin q :=(i)*3;
                if (Transparent) and (ps1[q+0]+ps1[q+1]+ps1[q+2]=3*255) and
                                     (ps2[q+0]+ps2[q+1]+ps2[q+2]=0) then
                  begin
                    Move(trans,pr[    q],3);
                    Move(trans,pr[3*W+q],3);
                  end
                else
                  begin
                    pr[q+0]:=((ps1[q+0]-ps2[q+0])*bb) div 256 + ps2[q+0];
                    pr[q+1]:=((ps1[q+1]-ps2[q+1])*gg) div 256 + ps2[q+1];
                    pr[q+2]:=((ps1[q+2]-ps2[q+2])*rr) div 256 + ps2[q+2];

                    c:=(36*pr[q+2]+53*pr[q+1]+11*pr[q+0]) div 100;
                    FillChar(pr[3*W+q],3,c);
                  end;
          end;
  end;
end;

Procedure rgb2transparent(oldColor,newColor : TColor; var aBitmap: TBitMap);
var
  i,j : Integer;
begin
  aBitmap.PixelFormat := pf24Bit;
  for i:=0 to aBitmap.Height-1 do
    for j:=0 to aBitmap.Width-1 do
      if aBitmap.Canvas.Pixels[i,j] = oldColor
        then aBitmap.Canvas.Pixels[i,j]:=newColor;
end;
//==============================================================================
function HLS(H, L, S : Integer): TColor;
var R,G,B : Integer;
begin
  HLStoRGB( H, L, Max(Min(S , HLSMAX), 0),
            R, G, B);
  Result := RGB(r, g, b);
end;
//==============================================================================
function Distance(const A, B : TColor): Integer;
var AA, BB : TColor;
begin
  AA := ColorToRGB(A);
  BB := ColorToRGB(B);
  Result := Round( Sqrt ( sqr( GetRValue(AA) - GetRValue(BB) )
                        + sqr( GetGValue(AA) - GetGValue(BB) )
                        + sqr( GetBValue(AA) - GetBValue(BB) ) ) );
end;
//==============================================================================
function GetContrastColor(const aFont, aGround: TColor): TColor;
var H,L,Lg,S : Integer;
    FColor, GColor : TColor;
begin
  GColor := ColorToRGB(aGround);
  RGBtoHLS( GetRValue(GColor), GetGValue(GColor), GetBValue(GColor), H, Lg, S );

  FColor := ColorToRGB(aFont);
  RGBtoHLS( GetRValue(FColor), GetGValue(FColor), GetBValue(FColor), H, L, S );

  while ( Distance(FColor, GColor) < 96) or ( Abs(L-Lg) < 96 ) do
  begin
    if Lg<128 then Inc(L, 16)
              else Dec(L, 16);
    FColor := HLS(H, L, S);
  end;

  Result:= FColor;
end;
//==============================================================================
{
procedure TrayIcon(Operation: TTrayIconOperation; Handle: THandle;
                                           Event: Integer; Ico, Title : String);
var nidata : TNotifyIconData;
begin

  nidata.cbSize := SizeOf(TNotifyIconData);
  nidata.Wnd := Handle;    //HWND окна принимающего обратные сообщения
  nidata.uID := 0;         // номер значка
  if Operation in [tioInsert,tioUpdate] then
    begin
      nidata.uFlags := NIF_ICON or NIF_MESSAGE or NIF_TIP;
      nidata.uCallBackMessage := Event;
      nidata.hIcon := LoadIcon(hInstance, pChar('MainIcon'));;
      StrPCopy(nidata.szTip,Title);
    end;

  case Operation of
    tioInsert : Shell_NotifyIcon(NIM_ADD, @nidata);
    tioUpdate : Shell_NotifyIcon(NIM_MODIFY, @nidata);
    tioDelete : Shell_NotifyIcon(NIM_DELETE, @nidata);
  end;
end; }
//==============================================================================
function VarIsNullOrEmpty(V: Variant): Boolean;
var i : Integer;
begin
  if VarIsType(V, [varEmpty, varNull]) then Result:= True else
  if VarIsType(V, VarArray) then
    begin
      Result:= True;
      for i:= VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
        if Not VarIsType(V[i], [varEmpty, varNull]) then
          Exit(False);
    end
  else
      Result:= False;
end;

function ValueForCompare(aValue: Variant; CaseInsensitive: Boolean = False; AccentInsensitive : Boolean = False): Variant;
var i: Integer;
    s: string;
begin
  if VarIsType(aValue, varString) then
    begin
      if CaseInsensitive then s:=AnsiUpperCase(aValue)
                         else s:=aValue;

      if AccentInsensitive then
        for i:=Length(s) downto 1 do
          begin
            if s[i]='Ё'  then s[i]:='Е' else
            if s[i]='ё'  then s[i]:='е' else
            if s[i]='''' then Delete(s,i,1);
          end;

      Result := s;
    end
  else
      Result := aValue;
end;

function VarArrayPlusVarArray(const L,R: Variant): Variant;
var i,N: Integer;
begin
  N:=0;
  if VarIsArray(L) then
    Inc(N,VarArrayHighBound(L,1)-VarArrayLowBound(L,1)+1)
  else
    Inc(N);

  if VarIsArray(R) then
    Inc(N,VarArrayHighBound(R,1)-VarArrayLowBound(R,1)+1)
  else
    Inc(N);

  Result:=VarArrayCreate([0, N-1], varVariant);
  VarArrayLock(Result);
  N:=0;

  if VarIsArray(L) then
    for i:=VarArrayLowBound(L, 1) to VarArrayHighBound(L, 1) do
      begin  Result[N]:= L[i];   Inc(N);  end
  else
      begin  Result[N]:= L;      Inc(N);  end;

  if VarIsArray(R) then
    for i:=VarArrayLowBound(R, 1) to VarArrayHighBound(R, 1) do
      begin  Result[N]:= R[i];  {Inc(N);} end
  else
      begin  Result[N]:= R;     {Inc(N);} end;

  VarArrayUnlock(Result);
end;

function VarToInt(const aValue : Variant) : int64;
begin
  if VarIsEmpty(aValue) or VarIsNull(aValue) then
    result := 0
  else
    try
      result := VarAsType(aValue, varInt64);
    except
      result:=0;
    end;
end;

function ValueToStringISO(const aValue: Variant; const aDataType: TFieldType; const NullValue: Variant): Variant;
var
  W1, W2, W3, W4: Word;
  sHEX: WideString;
  i: Integer;
  b: Byte;
begin
  if VarIsEmpty(aValue) or VarIsNull(aValue) then Exit(NullValue);

  if aDataType in LogicalTypes then
    begin
      if aValue = True  then Result:= TrueLexeme else
      if aValue = False then Result:= FalseLexeme else
                             Result:= NullValue;
    end else

  if aDataType in IntegerTypes then
    begin
      Result:= IntToStr(aValue);
    end else

  if aDataType in FloatTypes then
    begin
      Result:= StringReplace(VarToStr(aValue), ',', '.', []);
    end else

  if aDataType = ftDate then
    begin
      Result:= FormatDateTime('YYYY-MM-DD', aValue);
    end else

  if aDataType = ftTime then
    begin
      DecodeTime(aValue, W1, W2, W3, W4);
      if W4 > 0 then Result:= FormatDateTime('hh:nn:ss.zzz', aValue)
                else Result:= FormatDateTime('hh:nn:ss', aValue);
    end else

  if aDataType in DateTimeTypes then
    begin
      Result:= FormatDateTime('YYYY-MM-DD', aValue);

      DecodeTime(aValue, W1, W2, W3, W4);
      if (W4 > 0) then
        Result:= Result + 'T' + FormatDateTime('hh:nn:ss.zzz', aValue) else
      if (W1 > 0) or (W2 > 0) or (W3 > 0) then
        Result:= Result + 'T' + FormatDateTime('hh:nn:ss', aValue);
    end else

  if aDataType in BinaryTypes then
    begin
      setLength(sHEX, 2 * (VarArrayHighBound(aValue, 1) - VarArrayLowBound(aValue, 1) + 1));
      for i:= VarArrayLowBound(aValue, 1) to VarArrayHighBound(aValue, 1) do
        begin
          b:= VarArrayGet(aValue, i);
          sHEX[i*2 + 1] := HexChars[b shr 4];
          sHEX[i*2 + 2] := HexChars[b and $F];
        end;
      Result:= sHex;
    end else

    begin
      Result:= VarToStr(aValue);
    end
end;

function StringISOToValue(const aValue: String; const aDataType: TFieldType; const NullValue: Variant): Variant;
var s: String;
    i: Integer;
    _i64: Int64;
    _Bytes: TBytes;
    _Float: Double;
    _GUID: TGUID;
    _DateTime: TDateTime;
begin
  Result:= null;
  if VarIsNull(aValue) then Exit;

  s:= aValue;

  if (Length(s) = 0) and not (aDataType in StringTypes) then Result:= null else

  if aDataType in StringTypes then Result:= s else

  if aDataType in IntegerTypes then
    if TryStrToInt64(s, _i64) then Result:= _i64
                              else raise Exception.Create('Value ''' + s + ''' is not Integer type') else

  if aDataType in FloatTypes then
    if TryStrToFloat(s, _Float, fs) then Result:= _Float
                                    else raise Exception.Create('Value ''' + s + ''' is not Float type') else

  if aDataType in LogicalTypes then
    if SameText(s,  TrueLexeme) or SameText(s, 'T') or SameText(s, '1')  then Result:= True else
    if SameText(s, FalseLexeme) or SameText(s, 'F') or SameText(s, '0')  then Result:= False else
                                   raise Exception.Create('Value ''' + s + ''' is not Logical type') else

  if aDataType = ftTime then
    if TryStrToTime(s, _DateTime, fs) then Result:= TDate(_DateTime)
                                      else raise Exception.Create('Value ''' + s + ''' is not Time type') else

  if aDataType = ftDate then
    if TryStrToDate(s, _DateTime, fs) then Result:= TDate(_DateTime)
                                      else raise Exception.Create('Value ''' + s + ''' is not Date type') else

  if aDataType in DateTimeTypes then
    if TryStrToDateTime(StringReplace(s, 'T', ' ', []), _DateTime, fs)
                                      then Result:= TDate(_DateTime)
                                      else raise Exception.Create('Value ''' + s + ''' is not DateTime type') else

  if aDataType = ftGuid then
      if TryStringToGUID(s, _GUID) then Result:= GuidToString(_GUID)
                                   else raise Exception.Create('Value ''' + s + ''' is not GUID type') else

  if aDataType in BinaryTypes then try
                                     for i:= Length(s) downto 1 do
                                       if not (s[i] in ['0'..'9','a'..'f','A'..'F']) then delete(s, i, 1);

                                     SetLength(_Bytes, Length(s) div 2);
                                     for i:= 0 to Pred(Length(s) div 2) do
                                       _Bytes[i]:= Byte(StrToInt('$' + s[2*i+1] + s[2*i+2]));

                                     Result:= _Bytes;
                                   except
                                     raise Exception.Create('Value is not HEX Binary type')
                                   end else

  raise Exception.Create('Value ''' + s + ''' is Unknown type');

end;

function BoolToStr( const aValue : Variant) : string;
begin
  if  VarIsNull(aValue)           then Result:='Null'  else
  if (aValue=1) or (aValue=True)  then Result:='True'  else
  if (aValue=0) or (aValue=False) then Result:='False' else
                                       Result:='Unknown';
end;

function ConvertValue(const Value: Variant; const aToType: TFieldType): Variant;
var s:String;
    i:Integer;
    VT : TVarType;
begin
  Result:=Value;
  VT := VarType(Value);

  if (aToType in IntegerTypes) then
    begin
      if (VT = varEmpty) or (VT = varNull) then
        Result:=0 else

      if (VT = varSingle) or (VT = varDouble) or (VT = varCurrency) then
        Result:=Round(Value) else

      if (VT = varString) or (VT = varOleStr) or (VT = varUString) then
        begin
          s:=Result;
          for i:=Length(s) downto 1 do
            if not CharInSet(s[i], ['-','0'..'9']) then Delete(s,i,1);
          Result:=StrToIntDef(s, 0);
        end;
    end;
  if (aToType in NumericTypes) then
    begin
      if VT in [varEmpty, varNull] then
        Result:=0 else

     if VT in [varSingle, varDouble, varCurrency, varSmallint, varInteger,
               varShortInt, varByte, varWord, varLongWord, varInt64] then
        Result:=Value else

      if (VT = varString) or (VT = varOleStr) or (VT = varUString) then
        begin
          s:=Result;
          for i:=Length(s) downto 1 do
            if not CharInSet(s[i], ['-','0'..'9','.',',']) then Delete(s,i,1);
          Result:=StrToFloatDef(s, 0);
        end;
    end;
  if (aToType in StringTypes) then
    begin
      if (VT = varEmpty) or (VT = varNull) then
        Result := EmptyStr else

      if (VT = varSingle) or (VT = varDouble) or (VT = varCurrency) then
        Result:= FloatToStr(Value) else

      if (VT = varInteger) or (VT = varSmallint) or (VT = varShortInt) or
         (VT = varByte) or (VT = varWord) or (VT = varLongWord) or (VT = varInt64) then
        Result:=IntToStr(Value)
      else
        Result := VarToStr(Value);
    end;
end;

function VarIsZero (const Value: Variant) : boolean;
var s:String;
begin
  case VarType(Value) of
    varNull, VarEmpty
      : Result:=True;
    varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64
      : Result := (Value = 0);
    varSingle, varDouble, varCurrency, varDate
      : Result := (Value = 0);
    varBoolean
      : Result:= Not Value;
    varString, varOleStr
      : begin
          s:=Trim(Value);
          result := (Length(s)=0) or (s='0');
        end;
    else Result:=False;
  end;
end;

const VarIntegerTypes  = [varSmallint, varInteger, varByte, varWord, varLongWord, varInt64, varSingle, varDouble, varCurrency];
      VarFloatTypes    = [varSingle, varDouble, varCurrency];
      VarNumericTypes  = VarFloatTypes + VarIntegerTypes;

function    FormatValue(const aValue: Variant; const aFormat: string): Variant;
var fValue : Double;
    iValue : Int64;
begin
//  FormatTextByName


  Result := aValue;
  if Length(aFormat) <> 0 then
    try

      if SameText(aFormat, 'trunc') and (VarType(aValue) in VarNumericTypes) then
        begin
          iValue := Round(aValue*10000);
          Result:=IntToStr(iValue div 10000);
        end else

      if SameText(aFormat, 'word') and (VarType(aValue) in VarNumericTypes) then
        begin
          iValue := Round(aValue*10000);
          Result := SelectCase( IntToWords(iValue div 10000), aFormat);
        end else

      if SameText(aFormat, 'word3') and (VarType(aValue) in VarNumericTypes) then
        begin
          iValue := Round(aValue*1000);
          Result := SelectCase( IntToWords(iValue div 1000, iValue mod 1000), aFormat);
        end else

      if SameText(Copy(aFormat, 1, 4), 'frac') and (VarType(aValue) in VarNumericTypes) then
        begin
          iValue:=StrToIntDef(Copy(aFormat,5,1),2);
          Result:=Copy(FormatValue(frac(aValue),'%.'+IntToStr(iValue)+'f'), 3, MaxInt);
        end else

      case VarType(aValue) of

        varEmpty, varNull:
                    begin
                      Result:= EmptyStr;
                    end;

        varString, varUString, varOleStr:
                    begin
                      Result:= aValue;
                      if (1<length(aFormat)) and WideSameText( Copy(aFormat, 1, 1),'C') then
                        begin
                          iValue:=StrToIntDef( Copy(aFormat, 2, MaxInt), 0);
                          if 0 < iValue then
                            Result := Copy(aValue, iValue, 1)
                        end;
                    end;

        varDate:    begin
                      if SameText(aFormat, 'mmmmm') then  // Копируем для управления регистром
                        Result := SelectCase( GetTitle('_'+aFormat+IntToStr(MonthOf(aValue)), ttSecondName), aFormat )
                      else if SameText(aFormat, 'word') then
                        Result := SelectCase( ConvertDateToString(aValue), aFormat )
                      else
                        Result := FormatDateTime(aFormat, varToDateTime(aValue))
                    end;

        varSmallint, varInteger, varByte, varWord, varLongWord, varInt64:
                    begin
                      iValue:=aValue;
                      if Pos('#', aFormat) = 0 then
                        Result := Format(aFormat, [iValue])
                      else
                        Result := FormatFloat(aFormat, iValue);
                    end;

        varSingle, varDouble, varCurrency:
                    begin
                      fValue:=aValue;

                      if (Length(aFormat)=5) and
                         CharInSet(aFormat[1], ['%']) and
                         CharInSet(aFormat[2], ['0'..'9']) and
                         CharInSet(aFormat[3], ['.', ',', '-', '=', ':']) and
                         CharInSet(aFormat[4], ['0'..'9']) and
                         CharInSet(aFormat[5], ['f', 'F']) then
                        begin
                          Result := StringReplace( Format('%'+aFormat[2]+'.'+aFormat[4]+'f',[fValue]), FormatSettings.DecimalSeparator, aFormat[3], []);
                        end
                      else
                      if (Length(aFormat)=6) and
                         CharInSet(aFormat[1], ['%']) and
                         CharInSet(aFormat[2], ['0'..'9']) and
                         CharInSet(aFormat[3], ['0'..'9']) and
                         CharInSet(aFormat[4], ['.', ',', '-', '=', ':']) and
                         CharInSet(aFormat[5], ['0'..'9']) and
                         CharInSet(aFormat[6], ['f', 'F']) then
                        begin
                          Result := StringReplace( Format('%'+aFormat[2]+aFormat[3]+'.'+aFormat[5]+'f',[fValue]), FormatSettings.DecimalSeparator, aFormat[4], []);
                        end
                      else
                      if Pos('#', aFormat) <> 0 then
                        begin
                          Result := FormatFloat(aFormat, fValue);
                        end
                      else
                        begin
                          Result := Format(aFormat,[fValue]);
                        end;
                    end;

        else        begin
                      Result := Format(aFormat,[aValue]);
                    end;
      end;
    except
      Result := aValue;
    end;
end;
//..............................................................................
function DeTryStrToBoolean(const Text: string; var Value: Boolean): Boolean;
begin
 if SameText(Text, 'Y') or SameText(Text, 'YES') or SameText(Text, 'T') or SameText(Text, 'TRUE') or SameText(Text, '1') then
   begin
     Value := True;
     Result := True;
   end else
 if SameText(Text, 'N') or SameText(Text, 'NO') or SameText(Text, 'F') or SameText(Text, 'FALSE') or SameText(Text, '0') then
   begin
     Value := False;
     Result := True;
   end else
   begin
     Value := Unassigned;
     Result := False;
   end;
end;

function DeStrToBoolean(const Text: string; const Default: Boolean): Boolean;
begin
  if not DeTryStrToBoolean(Text, Result) then
    Result := Default;
end;
//..............................................................................

function TryStrToWord(const Text: string; out Value: Word): Boolean;
var
  IntValue: Integer;
begin
  Result := TryStrToInt(Text, IntValue);
  if Result and (IntValue >= Low(Value)) and (IntValue <= High(Value)) then
    Value := IntValue
  else
    Result := False;
end;

function GetCommonTempFolder: string;
begin
  Result := GetEnvironmentVariable('TMPDIR');
  if Result.Length = 0 then
    Result:=GetEnvironmentVariable('TEMP');
end;

function GetAppDataFolder: string;
var
  P: PChar;
begin
  P := nil;
  Result := EmptyStr;

    try
      P := AllocMem(Windows.MAX_PATH);
      if SHGetFolderPath(0, CSIDL_APPDATA,       0, 0, P) = S_OK then Result := P;
    finally
      FreeMem(P);
    end;

  if Length(Result)=0 then
    try
      P := AllocMem(Windows.MAX_PATH);
      if SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, P) = S_OK then Result := P;
    finally
      FreeMem(P);
    end;
end;

function GetDocumentsDirectory(const AllUsers: Boolean): string;
var
  Path: PChar;
begin
  Path := AllocMem(Windows.MAX_PATH);
  try
    Result := EmptyStr;
    if AllUsers then
      begin
        if SHGetFolderPath(0, CSIDL_COMMON_DOCUMENTS, 0, 0, Path) = S_OK then
          Result := StrPas(Path);
      end
    else
      if SHGetFolderPath(0, CSIDL_PERSONAL, 0, 0, Path) = S_OK then
        Result := StrPas(Path);
  finally
    FreeMem(Path);
  end;
  if Length(Result) <> 0 then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function NormalizeFileName(const aName: string; const ReplaceDot: Boolean = False): string;
var
  Index: Integer;
begin
  Result:= aName;
  for Index := Length(Result) downto 1 do
    case Result[Index] of
      char(0)..char(31): Delete(Result, Index, 1);
      '"': Result[Index] := '''';
      '<': Result[Index] := '(';
      '>': Result[Index] := ')';
      ':': Result[Index] := '=';
      '?': Result[Index] := '!';
      '*': Result[Index] := '~';
      '|': Result[Index] := '&';
      '/': Result[Index] := '-';
      '\': Result[Index] := '-';
      '.': if ReplaceDot then Result[Index] := '_';
    end;

  // есть сомнения, может стрельнуть
  Result:= Trim(Result);
  if Length(Result)=0 then Result:='-';
end;

function NormalizeFilePath(const aName: string; const LastSeparator: Boolean = False): string;
begin
  Result:= aName;
  while Copy(Result, Length(Result), 1) = '\' do
    Result:= Copy(Result, 1, Length(Result)-1);

  Result:= ReplaceStr(Result, '\\', '\');

  if LastSeparator then Result:= Result + '\';
end;

function NextName(aName: string): String;
var i: Integer;
begin
  if Length(aName)=0 then Exit('1');

  Result:=aName;
  for i := Length(Result) downto 1 do
    case Result[i] of
     '9':      begin
                 Result[i]:='0';
                 if i=1 then Exit('1'+Result);
               end;
     '0'..'8': begin
                 Result[i]:=Succ(Result[i]);
                 Exit(Result);
               end
     else      begin
                 Exit( Copy(Result,1,i)+ '1' + Copy(Result,i+1,MaxInt));
               end;
    end;
end;

function DeGetTempFileName(sPath: string; const sPrefix, sExt: string): string;
var
  iLen,rLen : integer;
  sRes      : string;
begin
  Result := EmptyStr;
  sPath := trim(sPath);
  if Length(sPath) = 0 then
  begin
    iLen := Windows.MAX_PATH;
    setLength(sPath,iLen);
    rLen := getTempPath(iLen,pChar(sPath));
    setLength(sPath,rLen);
  end;
  setLength(sRes,Windows.MAX_PATH);
  if getTempFileName(pChar(sPath),pChar(sPrefix),0,pChar(sRes)) <> 0 then
  begin
    setLength(sRes,strLen(pChar(sRes)));
    Result := ChangeFileExt(sRes,sExt);
  end;
end;

{
You typically switch a printer to duplex mode by changing its
TDeviceMode(API: DEVMODE)record .But first you should test whether
the installed printer driver supports this:
}

 function PrinterSupportsDuplex: Boolean;
 var
   Device, Driver, Port: array[0..255] of Char;
   hDevMode: THandle;
 begin
   Printer.GetPrinter(Device, Driver, Port, hDevmode);
   Result :=
     WinSpool.DeviceCapabilities(Device, Port, DC_DUPLEX, nil, nil) <>
     0;
 end;

 {if it does you can try to switch the duplex mode on before you call
 richedit.print:}


function SetdUPLEXPrinter: string;
 var
   Device, Driver, Port: array[0..80] of Char;
   DevMode: THandle;
   pDevmode: PDeviceMode;
   begin
      // Get printer device mode handle.
    Printer.GetPrinter(Device, Driver, Port, DevMode);
     if Devmode <> 0 then begin
        // lock it to get pointer to DEVMODE record
      pDevMode := GlobalLock(Devmode);
       if pDevmode <> nil then
       try
         with pDevmode^ do begin
           dmDuplex := DMDUP_VERTICAL;
           dmFields := dmFields or DM_DUPLEX;
         end;
       finally
         // unlock devmode handle.
        GlobalUnlock(Devmode);
       end;
     end; { If }
   end;

function GetDefaultPrinter: string;
var
  ResStr: array[0..255] of Char;
begin
  GetProfileString('Windows', 'device', '', ResStr, 255);
  Result := StrPas(ResStr);
end;

procedure SetDefaultPrinter(const NewDefPrinter: string);
var
  ResStr: array[0..255] of Char;
begin
  StrPCopy(ResStr, NewdefPrinter);
  WriteProfileString('windows', 'device', ResStr);
  StrCopy(ResStr, 'windows');
  SendMessage(HWND_BROADCAST, WM_WININICHANGE, 0, Longint(@ResStr));
end;

procedure SetDefaultPrinterByName(const PrinterName: string);
var
  I: Integer;
  Device: PChar;
  Driver: PChar;
  Port: PChar;
  HdeviceMode: THandle;
  aPrinter: TPrinter;
begin
  Printer.PrinterIndex := -1;
  GetMem(Device, 255);
  GetMem(Driver, 255);
  GetMem(Port, 255);
  aPrinter := TPrinter.Create;
  try
    for I := 0 to Printer.Printers.Count - 1 do
    begin
      if Printer.Printers[i] = PrinterName then
      begin
        aprinter.PrinterIndex := i;
        aPrinter.getprinter(device, driver, port, HdeviceMode);
        StrCat(Device, ',');
        StrCat(Device, Driver);
        StrCat(Device, Port);
        WriteProfileString('windows', 'device', Device);
        StrCopy(Device, 'windows');
        SendMessage(HWND_BROADCAST, WM_WININICHANGE, 0, Longint(@Device));
      end;
    end;
  finally
    aPrinter.Free;
  end;
  FreeMem(Device, 255);
  FreeMem(Driver, 255);
  FreeMem(Port, 255);
end;

function DeVarSameValue(const A, B: Variant): Boolean;
var
  LA, LB: TVarData;
  SA, SB: Integer;
  PA, PB: Pointer;
  Y1,Y2, N1,N2, D1,D2,
  H1,H2, M1,M2, S1,S2, T1,T2 : Word;
begin
  LA := FindVarData(A)^;
  LB := FindVarData(B)^;

  if (LA.VType in [varEmpty, varNull]) and (LB.VType in [varEmpty, varNull]) then
    Result:=True else

  if (LA.VType = LB.VType) and (LA.VType = varDate ) then
    begin
      DecodeDate(A, Y1, N1, D1);
      DecodeDate(B, Y2, N2, D2);

      DecodeTime(A, H1, M1, S1, T1);
      DecodeTime(B, H2, M2, S2, T2);
      Result := (Y1 = Y2) and (N1 = N2) and (D1 = D2) and (H1 = H2) and (M1 = M2) and (S1 = S2); // игнорируем миллисукнды
//    Result := (CompareDateTime(A,B) = 0);
    end else

  if VarIsArray(A) or VarIsArray(B) then
    if VarType(A) <> VarType(B) then Result := False else
      if (VarArrayDimCount(A) <> VarArrayDimCount(B)) or (VarArrayDimCount(A) <> 1) then Result := False else
        begin
          SA := VarArrayHighBound(A, 1) - VarArrayLowBound(A, 1);
          SB := VarArrayHighBound(B, 1) - VarArrayLowBound(B, 1);
          if SA <> SB then
            Result := False
          else
            begin
              PA := VarArrayLock(A);
              try
                PB := VarArrayLock(B);
                try
                  Result := CompareMem(PA, PB, SA);
                finally
                  VarArrayUnlock(B);
                end;
              finally
                VarArrayUnlock(A);
              end;
            end;
        end
    else

  if (LA.VType in [varNull]) xor (LB.VType in [varNull])
    then Result:= False else

  if (LB.VType = LA.VType)
    then Result:= (A = B) else

    try
      Result:= (A = B);
    except
       Result:= false;
    end
end;

function DeVarArrayRange(const Value: Variant; const LowIndex, HighIndex: Integer): Variant;
var
  BeginIndex, EndIndex: Integer;
  Index: Integer;
begin
  if VarIsArray(Value) then
    begin
      BeginIndex := VarArrayLowBound(Value, 1);
      EndIndex := VarArrayHighBound(Value, 1);
      if LowIndex > EndIndex then
        Result := Unassigned
      else
        if HighIndex < BeginIndex then
          Result := Unassigned
        else
          begin
            if LowIndex > BeginIndex then
              if LowIndex <= EndIndex then
                BeginIndex := LowIndex;
            if HighIndex < EndIndex then
              EndIndex := HighIndex;
            Index := Succ(EndIndex - BeginIndex);
            if Index = 1 then
              Result := VarArrayGet(Value, [BeginIndex])
            else
              if Index > 1 then
                begin
                  Result := VarArrayCreate([0, Pred(Index)], varVariant);
                  for Index := BeginIndex to EndIndex do
                    VarArrayPut(Result, VarArrayGet(Value, [Index]), [Index - BeginIndex]);
                end
              else
                Result := Unassigned;
          end;
    end
  else
    Result := Value;
end;

function CutTextValue(var Text: string; const Separator: String; const IsArrayOfChar: Boolean = False): string;
var
  Index, i, P : Integer;
begin
  Index:= 0;
  if IsArrayOfChar
    then for i:= 1 to Length(Separator) do
           begin
             P:= Pos(Separator[i], Text);
             if (0 < P) and ((Index = 0) or (P < Index)) then Index:= P;
           end
    else Index:= Pos(Separator, Text);

  if Index = 0 then
    begin
      Result:= Text;
      Text:= EmptyStr;
    end
  else
    begin
      Result:= Copy(Text, 1, Pred(Index));
      if IsArrayOfChar
        then Text:= Copy(Text, Index + 1, Length(Text))
        else Text:= Copy(Text, Index + Length(Separator), Length(Text));
    end;
end;

function FormatString(const Format, Text: string): string;
var
  Value, TagValue, IntValue1, IntValue2, IntValue3: string;
  Separator: Char;
  BlockLength, BlockPosition, BlockCount: Integer;
  function CutIntegerValue(var Value: string): string;
  begin
    Result := EmptyStr;
    while Length(Value) <> 0 do
      case Value[1] of
        '-': { Минус для выравнивания }
          if Pos('-', Result) <> 0 then
            Break
          else
            begin
              Result := Result + '-';
              Delete(Value, 1, 1);
            end;
        '0'..'9': { Цифры }
          begin
            Result := Result + Value[1];
            Delete(Value, 1, 1);
          end;
      else
        Break;
      end;
  end;
begin
  Result := EmptyStr;
  Value := Format;
  while Length(Value) <> 0 do
    begin
      Result := Result + CutTextValue(Value, '{');
      if Length(Value) <> 0 then
        if Value[1] = '{' then
          begin
            Result := Result + '{';
            Delete(Value, 1, 1);
          end
        else
          begin
            if Pos('}', Value) = 0 then
              raise EConvertError.CreateFmt('Не найден закрывающий тег ''}'' в строке ''%s''.', [Value]);
            TagValue := CutTextValue(Value, '}');
            IntValue1 := CutIntegerValue(TagValue);
            TagValue := Trim(TagValue);
            if (Length(TagValue) <> 0) and (TagValue[1] = ',') then
              begin
                Delete(TagValue, 1, 1);
                TagValue := Trim(TagValue);
              end;
            IntValue2 := CutIntegerValue(TagValue);
            TagValue := Trim(TagValue);
            if (Length(TagValue) <> 0) and (TagValue[1] = ',') then
              begin
                Delete(TagValue, 1, 1);
                TagValue := Trim(TagValue);
              end;
            IntValue3 := CutIntegerValue(TagValue);
            TagValue := Trim(TagValue);
            if (Length(TagValue) <> 0) and (TagValue[1] = ',') then
              begin
                Delete(TagValue, 1, 1);
                TagValue := Trim(TagValue);
              end;
            Separator := Chr(StrToIntDef(TagValue, 32));
            BlockLength := StrToIntDef(IntValue1, 0);
            BlockPosition := StrToIntDef(IntValue2, 1);
            BlockCount := StrToIntDef(IntValue3, Length(Text));
            if BlockPosition <= 0 then
              raise EConvertError.Create('Позиция символа не может быть меньше или равна 0.');
            if BlockCount <= 0 then
              raise EConvertError.Create('Количество символов не может быть меньше 0.');
            TagValue := Copy(Text, BlockPosition, BlockCount);
            while Length(TagValue) < Abs(BlockLength) do
              if BlockLength > 0 then
                TagValue := TagValue + Separator
              else
                TagValue := Separator + TagValue;
            Result := Result + TagValue;
          end;
    end;
end;

function WrapString(const Text: string; const LineSizes: array of Word; const Separators: string = ''): string;
var
  Value: string;
  Index: Integer;
  Symbols: string;
  function CutString(var Text: string; const Size: Word): string;
  var
    Count, Index: Integer;
  begin
    if Size = 0 then
      begin
        Result := Text;
        Text := EmptyStr;
      end
    else
      begin
        Count := Length(Text);
        if Count > Size then
          begin
            Result := Copy(Text, 1, Size);
            Delete(Text, 1, Size);
            if Pos(Result[Size], Symbols) = 0 then
              if Pos(Text[1], Symbols) <> 0 then
                Text := TrimLeft(Text)
              else
                begin
                  Index := Size;
                  while Index <> 0 do
                    if Pos(Result[Index], Symbols) <> 0 then
                      Break
                    else
                      Dec(Index);
                  if Index <> 0 then
                    begin
                      Text := Copy(Result, Succ(Index), Length(Result)) + Text;
                      Result := Copy(Result, 1, Index);
                      Result := Result + DupeString(' ', Size - Length(Result));
                    end;
                end;
          end
        else
          if Count < Size then
            begin
              Result := Text + DupeString(' ', Size - Count);
              Text := EmptyStr;
            end
          else
            begin
              Result := Text;
              Text := EmptyStr;
            end;
      end;
  end;
begin
  Result := EmptyStr;
  Symbols := Separators;
  if Length(Text) = 0 then
    for Index := Low(LineSizes) to High(LineSizes) do
      Result := Result + DupeString(' ', LineSizes[Index])
  else
    begin
      if Length(Symbols) = 0 then
        for Index := 0 to 32 do
          Symbols := Symbols + Chr(Index);
      Value := Text;
      Index := Low(LineSizes);
      while Length(Value) <> 0 do
        begin
          Result := Result + CutString(Value, LineSizes[Index]);
          if Index <> High(LineSizes) then
            Inc(Index);
        end;
    end;
end;

procedure ClearDirectory(const RootDirectory: string; const Recursive: Boolean);
const
  faExclude = faVolumeID or faDirectory;
var
  Directory: string;
  SearchRec: TSearchRec;
  ErrorCode: Integer;
begin
  Directory := RootDirectory;
  if Length(Directory) <> 0 then
    Directory := IncludeTrailingPathDelimiter(Directory);
  ErrorCode := FindFirst(Directory + '*.*', faAnyFile, SearchRec);
  if ErrorCode = 0 then
    try
      while ErrorCode = 0 do
        begin
          if Length(SearchRec.Name) <> 0 then
            if (SearchRec.Attr and faExclude) = 0 then
              DeleteFile(PChar(Directory + SearchRec.Name))
            else
              if Recursive and ((SearchRec.Attr and faDirectory) <> 0) and (SearchRec.Name[1] <> '.') then
                begin
                  ClearDirectory(Directory + SearchRec.Name, Recursive);
                  RemoveDirectory(PChar(Directory + SearchRec.Name))
                end;
          ErrorCode := FindNext(SearchRec);
        end;
    finally
      SysUtils.FindClose(SearchRec);
    end;
end;

function IsEmptyDirectory(const RootDirectory: string): Boolean;
const
  faExclude = faVolumeID or faDirectory;
var
  Directory: string;
  SearchRec: TSearchRec;
  ErrorCode: Integer;
begin
  Result := True;
  Directory := RootDirectory;
  if Length(Directory) <> 0 then Directory := IncludeTrailingPathDelimiter(Directory);
  ErrorCode := FindFirst(Directory + '*.*', faAnyFile, SearchRec);
  if ErrorCode = 0 then
    try
      while ErrorCode = 0 do
        begin
          if Length(SearchRec.Name) <> 0 then
            if (SearchRec.Attr and faExclude) = 0 then
              begin
                Result := False;
                Break;
              end
            else
              if ((SearchRec.Attr and faDirectory) <> 0) and (SearchRec.Name[1] <> '.') then
                begin
                  Result := False;
                  Break;
                end;
          ErrorCode := FindNext(SearchRec);
        end;
    finally
      SysUtils.FindClose(SearchRec);
    end;
end;

function GetTempDirectory(const SubDirCreating: Boolean): string;
var
  Buffer: array[0..2047] of Char;
begin
  if GetTempPath(SizeOf(Buffer), Buffer) <> 0 then
    Result := StrPas(Buffer)
  else
    Result := EmptyStr;
  if Length(Result) <> 0 then Result := IncludeTrailingBackslash(Result);
  if SubDirCreating then
    begin
      Result := Result + GUIDToString(NewGUID);
      if ForceDirectories(Result) then
        SetFileAttributes(PChar(Result), GetFileAttributes(PChar(Result)) or faHidden);
      Result := IncludeTrailingBackslash(Result);
    end;
end;

function ReadStringFromFile(const FileName: string): string;
var
  Strings: TStrings;
begin
  Strings := TStringList.Create;
  try
    Strings.LoadFromFile(FileName);
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

procedure WriteStringToFile(const FileName, Text: string);
var
  Strings: TStrings;
begin
  Strings := TStringList.Create;
  try
    Strings.Text := Text;
    Strings.SaveToFile(FileName);
  finally
    Strings.Free;
  end;
end;

{$REGION 'Log Runtime Utilites ...'}
function InternalIsLogEnabled(const Name: string): Boolean;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      Result := OpenKeyReadOnly(RegKey);
      if Result then
        begin
          Result := ValueExists(Name);
          if Result then
            try
              Result := Boolean(ReadInteger(Name));
            except
              Result := False;
            end;
        end;
    finally
      Free;
    end;
end;

procedure WriteLog(const Text: string; const SkipSettings: Boolean; const FileName: string);
begin
  try
    if SkipSettings or InternalIsLogEnabled(RegLogFile) then
      DeLog.WriteLog(Text, FileName);
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('Funcs.WriteLog error: ' + E.Message);
    {$ENDIF}
  end;
end;

procedure WriteLog(const Format: string; const Arguments: array of const; const SkipSettings: Boolean; const FileName: string);
begin
  try
    WriteLog(SysUtils.Format(Format, Arguments), SkipSettings, FileName);
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('Funcs.WriteLog error: ' + E.Message);
    {$ENDIF}
  end;
end;

procedure WriteQueryLog(DataSet: TDataSet; const SkipSettings: Boolean; const FileName: string);
begin
  try
    if SkipSettings or InternalIsLogEnabled(RegLogQuery) then
      DeLog.WriteQueryLog(DataSet, FileName);
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog('Funcs.WriteQueryLog error: ' + E.Message);
    {$ENDIF}
  end;
end;

{$ENDREGION}

function iif(const Condition: Boolean; const ValueTRUE, ValueFALSE: string): string;
begin
  if Condition then Result := ValueTRUE
               else Result := ValueFALSE;
end;

function iif(const Condition: Boolean; const ValueTRUE, ValueFALSE: Integer): Integer;
begin
  if Condition then Result := ValueTRUE
               else Result := ValueFALSE;
end;

function iif(const Condition: Boolean; const ValueTRUE, ValueFALSE: TAlign): TAlign;
begin
  if Condition then Result := ValueTRUE
               else Result := ValueFALSE;
end;

function IntToStr36(const Value: Int64; const Digits: SmallInt): string;
const
  Symbols: string = '0123456789abcdefghijklmnopqrstuvwxyz';
var
  Index: Int64;
  Count: Integer;
begin
  Index := Value;
  if Index = 0 then
    Result := Symbols[1]
  else
    begin
      Result := EmptyStr;
      Count := Length(Symbols);
      while Index <> 0 do
        begin
          Result := Symbols[Succ(Index mod Count)] + Result;
          Index := Index div Count;
        end;
    end;
  while Length(Result) < Digits do
    Result := Symbols[1] + Result;
end;

function XorAnsiString(const Value, Mask: AnsiString): AnsiString;
var
  Index: Integer;
begin
  if Length(Mask) = 0 then
    Result := Value
  else
    begin
      Result := EmptyStr;
      for Index := 1 to Length(Value) do
        Result := Result + Chr(Ord(Value[Index]) xor Ord(Mask[Index mod Length(Mask)]));
    end;
end;

function AnsiStringToBase64(const Value: AnsiString; const CharsPerLine: Integer; const LineSeparator: string): AnsiString;
var
  MemoryStream: TMemoryStream;
  StringStream: TStringStream;
  Base64Encoding: TBase64Encoding;
begin
  MemoryStream := TMemoryStream.Create;
  try
    MemoryStream.WriteBuffer(Value[1], Length(Value));
    MemoryStream.Position := 0;
    StringStream := TStringStream.Create;
    try
      Base64Encoding := TBase64Encoding.Create(CharsPerLine, LineSeparator);
      try
        Base64Encoding.Encode(MemoryStream, StringStream);
      finally
        Base64Encoding.Free;
      end;
      Result := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  finally
    MemoryStream.Free;
  end;
end;

function AnsiStringFromBase64(const Value: AnsiString): AnsiString;
var
  MemoryStream: TMemoryStream;
  StringStream: TStringStream;
  Base64Encoding: TBase64Encoding;
begin
  MemoryStream := TMemoryStream.Create;
  try
    MemoryStream.WriteBuffer(Value[1], Length(Value));
    MemoryStream.Position := 0;
    StringStream := TStringStream.Create;
    try
      Base64Encoding := TBase64Encoding.Create;
      try
        Base64Encoding.Decode(MemoryStream, StringStream);
      finally
        Base64Encoding.Free;
      end;
      Result := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  finally
    MemoryStream.Free;
  end;
end;

{$IFDEF DEBUG}
function DumpAnsiString(const Text: AnsiString; const PrefixLine: AnsiString; const ColCount: Integer; const ShowASCII: Boolean; const ShowOffset: Boolean): AnsiString;
var
  OffsetLength, Index, ColIndex: Integer;
  DumpString, AsciiString: AnsiString;
begin
  Result := EmptyStr;
  if Length(Text) > 16777215 then
    OffsetLength := 8
  else if Length(Text) > 65535 then
    OffsetLength := 6
  else
    OffsetLength := 4;
  DumpString := EmptyStr;
  AsciiString := EmptyStr;
  ColIndex := 1;
  for Index := 1 to Length(Text) do
    begin
      if (ColIndex = 1) and ShowOffset then
        DumpString := DumpString + UpperCase(IntToHex(Pred(Index), OffsetLength)) + ': ';
      DumpString := DumpString + UpperCase(IntToHex(Ord(Text[Index]), 2)) + ' ';
      case Ord(Text[Index]) of
        32..126: { Печатаемые символы }
          AsciiString := AsciiString + Text[Index];
      else
        AsciiString := AsciiString + '.';
      end;
      Inc(ColIndex);
      if ColIndex > ColCount then
        begin
          if ShowASCII then
            DumpString := DumpString + AsciiString
          else
            DumpString := TrimRight(DumpString);
          DumpString := PrefixLine + DumpString;
          Result := Result + DumpString + #13#10;
          ColIndex := 1;
          DumpString := EmptyStr;
          AsciiString := EmptyStr;
        end;
    end;
  if (ColIndex <> 1) and ShowASCII then
    while ColIndex <= ColCount do
      begin
        DumpString := DumpString + '   ';
        Inc(ColIndex);
      end;
  if Length(DumpString) <> 0 then
    begin
      if ShowASCII then
        DumpString := DumpString + AsciiString
      else
        DumpString := TrimRight(DumpString);
      DumpString := PrefixLine + DumpString;
      Result := Result + DumpString + #13#10;
    end;
end;
{$ENDIF}

function GetDesktopDirectory(const PublicDesktop: Boolean): string;
const
  CSIDL_DESKTOPDIRECTORY = $0010;
  CSIDL_COMMON_DESKTOPDIRECTORY = $0019;
const
  Flags: array[Boolean] of Cardinal = (CSIDL_DESKTOPDIRECTORY, CSIDL_COMMON_DESKTOPDIRECTORY);
var
  Path: PChar;
begin
  Path := AllocMem(MAX_PATH);
  try
    if SHGetFolderPath(0, Flags[PublicDesktop], 0, SHGFP_TYPE_CURRENT, Path) = S_OK then
      Result := StrPas(Path)
    else
      Result := EmptyStr;
  finally
    FreeMem(Path);
  end;
  if Length(Result) <> 0 then Result := IncludeTrailingPathDelimiter(Result);
end;

function TryStringToGUID(const Text: string; var GUID: TGUID): Boolean;
begin
  if Length(Text) <> 38 then Exit(False);
  if Text[1] <> '{' then Exit(False);
  Result := True;
  try
    GUID := StringToGUID(Text);
  except
    Result := False;
  end;
end;

function IsGUID(const Value: Variant): Boolean;
var
  GUID: TGUID;
begin
  Result := TryStringToGUID(VarToStr(Value), GUID);
end;

function NewGUID: TGUID;
begin
  CreateGUID(Result);
end;

{$REGION 'Работа с Windows Terminal Server ...'}
type
  WTS_INFO_CLASS = (
    WTSInitialProgram,
    WTSApplicationName,
    WTSWorkingDirectory,
    WTSOEMId,
    WTSSessionId,
    WTSUserName,
    WTSWinStationName,
    WTSDomainName,
    WTSConnectState,
    WTSClientBuildNumber,
    WTSClientName,
    WTSClientDirectory,
    WTSClientProductId,
    WTSClientHardwareId,
    WTSClientAddress, //Returns pointer to a WTS_CLIENT_ADDRESS - structure
    WTSClientDisplay,
    WTSClientProtocolType);

  WTS_CONNECTSTATE_CLASS = (
    WTSActive,              // User logged on to WinStation
    WTSConnected,           // WinStation connected to client
    WTSConnectQuery,        // In the process of connecting to client
    WTSShadow,              // Shadowing another WinStation
    WTSDisconnected,        // WinStation logged on without client
    WTSIdle,                // Waiting for client to connect
    WTSListen,              // WinStation is listening for connection
    WTSReset,               // WinStation is being reset
    WTSDown,                // WinStation is down due to error
    WTSInit);

  WTSINFO = record
    State: WTS_CONNECTSTATE_CLASS;
    SessionID: DWORD;
    IncomingBytes: DWORD;
    OutgoingBytes: DWORD;
    IncomingCompressedBytes: DWORD;
    OutgoingCompressedBytes: DWORD;
    WinStationName: WCHAR;
    Domain: WCHAR;
    UserName: WCHAR;
    ConnectTime: LARGE_INTEGER;
    DisconnectTime: LARGE_INTEGER;
    LastInputTime: LARGE_INTEGER;
    LogonTime: LARGE_INTEGER;
    CurrentTime: LARGE_INTEGER;
  end;

const
  WTS_CURRENT_SERVER_HANDLE = THandle(0);

type
 PWTS_CLIENT_ADDRESS = ^WTS_CLIENT_ADDRESS;
  WTS_CLIENT_ADDRESS = record
    AddressFamily: DWORD;           // AF_INET, AF_IPX, AF_NETBIOS, AF_UNSPEC
    Address: array [0..19] of Byte; // client network address
  end;

const
  AF_INET = 2; // internetwork: UDP, TCP, etc.
  WTS_CURRENT_SESSION = $FFFFFFFF;

type
  TWTSFreeMemoryFunc = procedure(pMemory: pointer); stdcall;
  TWTSQuerySessionInformationFunc = function(hServer: THandle; SessionID: DWORD; WTSInfoClass: WTS_INFO_CLASS; var ppBuffer: Pointer; var pBytesReturned: DWORD): BOOL; stdcall;

function GetWTSClientIP: string;
var
  Handle: THandle;
  WTSFreeMemory: TWTSFreeMemoryFunc;
  WTSQuerySessionInformation: TWTSQuerySessionInformationFunc;
  Buffer: Pointer;
  BufferSize: DWORD;
begin
  Result := EmptyStr;
  Handle := LoadLibrary(wtsapi32);
  if Handle <> 0 then
    try
      WTSFreeMemory := GetProcAddress(Handle, 'WTSFreeMemory');
      if Assigned(WTSFreeMemory) then
        begin
          WTSQuerySessionInformation := GetProcAddress(Handle, 'WTSQuerySessionInformationA');
          if Assigned(WTSQuerySessionInformation) then
            if WTSQuerySessionInformation(WTS_CURRENT_SERVER_HANDLE, WTS_CURRENT_SESSION, WTSClientAddress, Buffer, BufferSize) then
              try
                if PWTS_CLIENT_ADDRESS(Buffer)^.AddressFamily = AF_INET then
                  Result := Format('%u.%u.%u.%u',
                    [
                      PWTS_CLIENT_ADDRESS(Buffer)^.Address[2],
                      PWTS_CLIENT_ADDRESS(Buffer)^.Address[3],
                      PWTS_CLIENT_ADDRESS(Buffer)^.Address[4],
                      PWTS_CLIENT_ADDRESS(Buffer)^.Address[5]
                    ]);
              finally
                WTSFreeMemory(Buffer);
              end;
        end;
    finally
      FreeLibrary(Handle);
    end;
end;

function GetWTSClientName: string;
var
  Handle: THandle;
  WTSFreeMemory: TWTSFreeMemoryFunc;
  WTSQuerySessionInformation: TWTSQuerySessionInformationFunc;
  Buffer: Pointer;
  BufferSize: DWORD;
  AnsiBuffer: PAnsiChar;
begin
  Result := EmptyStr;
  Handle := LoadLibrary(wtsapi32);
  if Handle <> 0 then
    try
      WTSFreeMemory := GetProcAddress(Handle, 'WTSFreeMemory');
      if Assigned(WTSFreeMemory) then
        begin
          WTSQuerySessionInformation := GetProcAddress(Handle, 'WTSQuerySessionInformationA');
          if Assigned(WTSQuerySessionInformation) then
            if WTSQuerySessionInformation(WTS_CURRENT_SERVER_HANDLE, WTS_CURRENT_SESSION, WTSClientName, Buffer, BufferSize) then
              try
                AnsiBuffer := Buffer;
                Result := StrPas(AnsiBuffer);
              finally
                WTSFreeMemory(Buffer);
              end;
        end;
    finally
      FreeLibrary(Handle);
    end;
end;

function GetWTSDomainName: string;
var
  Handle: THandle;
  WTSFreeMemory: TWTSFreeMemoryFunc;
  WTSQuerySessionInformation: TWTSQuerySessionInformationFunc;
  Buffer: Pointer;
  BufferSize: DWORD;
  AnsiBuffer: PAnsiChar;
begin
  Result := EmptyStr;
  Handle := LoadLibrary(wtsapi32);
  if Handle <> 0 then
    try
      WTSFreeMemory := GetProcAddress(Handle, 'WTSFreeMemory');
      if Assigned(WTSFreeMemory) then
        begin
          WTSQuerySessionInformation := GetProcAddress(Handle, 'WTSQuerySessionInformationA');
          if Assigned(WTSQuerySessionInformation) then
            if WTSQuerySessionInformation(WTS_CURRENT_SERVER_HANDLE, WTS_CURRENT_SESSION, WTSDomainName, Buffer, BufferSize) then
              try
                AnsiBuffer := Buffer;
                Result := StrPas(AnsiBuffer);
              finally
                WTSFreeMemory(Buffer);
              end;
        end;
    finally
      FreeLibrary(Handle);
    end;
end;
{$ENDREGION}

function GetHostIP(const HostName: string): string;
var
  WSAData: TWSAData;
  Name: AnsiString;
  HostEnt: PHostEnt;
begin
  Result := EmptyStr;
  if Length(HostName) <> 0 then
    if WSAStartup($0101, WSAData) = 0 then
      try
        Name := HostName;
        HostEnt := GetHostByName(PAnsiChar(Name));
        if Assigned(HostEnt) then
          Result := Trim(inet_ntoa(PInAddr(HostEnt.h_addr_list^)^));
      finally
        WSACleanup;
      end;
end;

function TrimFormula(const aText: String): string;
var i,N: Integer;
begin
  Result:= Trim(StringReplace( {A}
                    StringReplace( {B}
                        StringReplace(aText, #9, ' ',[rfReplaceAll]),
                                   {B}  #10,' ',[rfReplaceAll]),
                               {A} #13,' ',[rfReplaceAll]));

  while 0 < Pos('  ', Result) do
    Result:= StringReplace(Result,'  ',' ',[rfReplaceAll]);

  // проверяем минимальный уровень вложенности, чтобы не обрезать нужные скобки
  // "((A+B)*(C+D))" >> "(A+B)*(C+D)" >> "A+B)*(C+D"  последний шаг ОШИБКА
  while (2 <= Length(Result)) and (Result[1] = '(') and (Result[Length(Result)] = ')') do
    begin
      N:= 1;
      for i:= 2 to Pred(Length(Result)) do  //
        if Result[i] = '(' then begin Inc(N); end else
        if Result[i] = ')' then begin Dec(N); if N=0 then Exit(Result); end;

      Result:= Trim(Copy(Result, 2, Length(Result) - 2));
    end;
end;

function SameFormula(const aFormula, bFormula: String; const aObject: TObject = nil): Boolean;
var a,b : String;
    FM: TFieldMeta;
begin
  a:= TrimFormula(aFormula);
  b:= TrimFormula(bFormula);

  Result:= Sametext( a, b);
  if Result then Exit;

  if assigned(aObject) then
    if aObject is TFieldMeta then
      begin
        FM:= TFieldMeta(aObject);

//      if FM.Owner.Database.DatabaseType = dtMSSQL then

        if (FM.DataType = ftBoolean) then
          begin
            if SameText(a,'1') or SameText(a,'T') then a:=TrueLexeme else
            if SameText(a,'0') or SameText(a,'F') then a:=FalseLexeme;

            if SameText(b,'1') or SameText(b,'T') then b:=TrueLexeme else
            if SameText(b,'0') or SameText(b,'F') then b:=FalseLexeme;
          end else

        if (FM.DataType in DateTimeTypes) then
          begin
            if SameText(a,'Now()') or SameText(a,'GetDate()') or SameText(a,'current_timestamp')  then a:='Now';
            if SameText(b,'Now()') or SameText(b,'GetDate()') or SameText(b,'current_timestamp')  then b:='Now';
          end else

        if (FM.DataType = ftGUID) then
          begin
            if SameText(a,'NewGUID()') or SameText(a,'NewID()') then a:='NewGUID';
            if SameText(b,'NewGUID()') or SameText(b,'NewID()') then b:='NewGUID';
          end;

        Result:= SameText(a, b);
      end;

  Result:= Sametext( a, b);
end;

function NormalizeCaption(const Text: string; const MaxLength: Integer): string;
var
  Index: Integer;
  Symbol: Char;
begin
  Result := EmptyStr;
  for Index := 1 to Length(Text) do
    begin
      Symbol := Text[Index];
      if Ord(Symbol) > 31 then
        Result := Result + Symbol;
    end;
  if MaxLength > 0 then
    begin
      Index := Length(Result);
      if Index > MaxLength then
        case MaxLength of
          1..5: { От 1 до 5 символов: просто обрезаем }
            Result := Copy(Result, 1, MaxLength);
          6..9: { От 6 до 9 символов: оставляем в конце один символ }
            Result := Copy(Result, 1, MaxLength - 4) + '...' + Copy(Result, Index, 1);
          10..15: { От 10 до 15 символов: оставляем в конце 3 символа }
            Result := Copy(Result, 1, MaxLength - 6) + '...' + Copy(Result, Index - 3, 3);
          16..19: { От 16 до 19 символов: оставляем в конце 5 символов }
            Result := Copy(Result, 1, MaxLength - 8) + '...' + Copy(Result, Index - 5, 5);
        else
          Result := Copy(Result, 1, MaxLength - 10) + '...' + Copy(Result, Index - 7, 7);
        end;
    end;
end;

procedure ShellFolderParseDisplayName(const ShellFolder: IShellFolder; const DisplayName: string; out PIDL: PItemIDList);
var
  Attributes, Eaten: DWORD;
begin
  OleCheck(ShellFolder.ParseDisplayName(0, nil, PWideChar(WideString(DisplayName)), Eaten, PIDL, Attributes));
end;

procedure GetShellFolderItfPtr(const FolderName: string; Malloc: IMalloc; out TargetFolder: IShellFolder);
var
  DesktopFolder: IShellFolder;
  ItemIDList: PItemIDList;
begin
  OleCheck(SHGetDesktopFolder(DesktopFolder));
  ShellFolderParseDisplayName(DesktopFolder, FolderName, ItemIDList);
  try
    OleCheck(DesktopFolder.BindToObject(ItemIDList, nil, IShellFolder, Pointer(TargetFolder)));
  finally
    Malloc.Free(ItemIDList);
  end;
end;

function GetExtractImageItfPtr(const FileName: string; out XtractImage: IExtractImage): Boolean;
var
  TargetFolder: IShellFolder;
  FilePath: string;
  ItemIDList: PItemIDList;
  Malloc: IMalloc;

  function ShellFolderGetUIObjectOf(const ShellFolder: IShellFolder; cidl: DWORD; var PIDL: PItemIDList; riid: TGUID; out pv): Boolean;
  begin
    Result := NOERROR = ShellFolder.GetUIObjectOf(0, cidl, PIDL, riid, nil, Pointer(pv));
  end;
begin
  FilePath := ExcludeTrailingBackslash(ExtractFilePath(FileName));
  OleCheck(SHGetMalloc(Malloc));
  GetShellFolderItfPtr(FilePath, Malloc, TargetFolder);
  ShellFolderParseDisplayName(TargetFolder, ExtractFileName(FileName), ItemIDList);
  try
    Result := ShellFolderGetUIObjectOf(TargetFolder, 1, ItemIDList, IExtractImage, XtractImage);
  finally
    Malloc.Free(ItemIDList);
  end;
end;

function ExtractImageGetFileThumbnail(const XtractImage: IExtractImage; ImgWidth, ImgHeight, ImgColorDepth: Integer;
  var Flags: DWORD; out RunnableTask: IRunnableTask; out Bmp: TBitmap): Boolean;
var
  Size: TSize;
  Buf: array[0..MAX_PATH] of WideChar;
  BmpHandle: HBITMAP;
  Priority: DWORD;
  GetLocationRes: HRESULT;
begin
  Result := False;
  RunnableTask := nil;
  Size.cx := ImgWidth;
  Size.cy := ImgHeight;
  Priority := IEIT_PRIORITY_NORMAL;
  Flags := Flags or IEIFLAG_ASYNC;
  GetLocationRes := XtractImage.GetLocation(Buf, sizeof(Buf), Priority, Size, ImgColorDepth, Flags);

  if (GetLocationRes = NOERROR) or (GetLocationRes = E_PENDING) then
  begin
    if GetLocationRes = E_PENDING then
    begin
      { if QI for IRunnableTask succeed, we can use RunnableTask
        interface pointer later to kill running extraction process.
        We could spawn a new thread here to extract image. }
      if S_OK <> XtractImage.QueryInterface(IRunnableTask, RunnableTask) then
        RunnableTask := nil;
    end;
    Bmp := TBitmap.Create;
    try
      OleCheck(XtractImage.Extract(BmpHandle)); // This could consume a long time.
                                                // If RunnableTask is available
                                                // then calling Kill() method
                                                // will immediately abort the process.
      Bmp.Handle := BmpHandle;
      Result := True;
    except
      on E: EOleSysError do
      begin
        //-------------
        OutputDebugString(PChar(string(E.ClassName) + ': ' + E.Message));
        //-------------
        FreeAndNil(Bmp);
        Result := False;
      end;
      else
      begin
        FreeAndNil(Bmp);
        raise;
      end;
    end; { try/except }
  end;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
var
  E, D: TColor;
  Stream: TStream;
  ApplicationVersionMS, ApplicationVersionLS: Cardinal;
begin
  LogDirectory := GetAppDataFolder;
  if Length(LogDirectory) <> 0 then LogDirectory := IncludeTrailingBackslash(LogDirectory);
  LogDirectory := IncludeTrailingBackslash(LogDirectory + cApplicationName);
  {$IFDEF DEBUG}
  DebugLog('Funcs unit initialization ...');
  {$ENDIF}
  GetFileVersion(GetModuleName(hInstance), ApplicationVersionMS, ApplicationVersionLS);
  Funcs.WriteLog(Format('Initialization of application version %d.%d.%d.%d'{$IFDEF PRERELEASE} + ' beta'{$ELSE}{$IFDEF DEBUG} + ' debug'{$ENDIF}{$ENDIF} + ' ...', [
    HiWord(ApplicationVersionMS), LoWord(ApplicationVersionMS),
    HiWord(ApplicationVersionLS), LoWord(ApplicationVersionLS)]) +
    PrepareProjectCompiler + PrepareProjectOptions + PrepareProjectDefinitions);
  E:=ColorToRGB(clWindow);
  D:=ColorToRGB(clBtnFace);
  clWindowDisable :=RGB((GetRValue(E)+GetRValue(D)) div 2,
                        (GetGValue(E)+GetGValue(D)) div 2,
                        (GetBValue(E)+GetBValue(D)) div 2 );
  RTFEncoder := TStringList.Create;
  Stream := TResourceStream.Create(hInstance, 'RTF', RT_RCDATA);
  try
    RTFEncoder.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
  {
  with TResourceStream.Create(hInstance, 'BASE64', RT_RCDATA) do
    try
      ReadBuffer(DeBase64[0], Length(DeBase64));
    finally
      Free;
    end;
  }
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('Funcs unit finalization ...');
  {$ENDIF}
  Funcs.WriteLog('Finalization of application ...');
  FreeAndNil(RTFEncoder);
end;
{$ENDREGION}

initialization
  Startup;

  fs := TFormatSettings.Create;
  fs.DateSeparator := '-';
  fs.ShortDateFormat := 'yyyy-MM-dd';
  fs.TimeSeparator := ':';
  fs.ShortTimeFormat := 'hh:mm';
  fs.LongTimeFormat := 'hh:mm:ss';
  fs.DecimalSeparator := '.';

finalization
  Shutdown;

end.

