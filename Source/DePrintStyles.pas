unit DePrintStyles;

interface

uses
  SysUtils, Windows, Classes, Printers, Graphics;

// Размеры полей по умолчанию для листа формата A4 (в 0,1 мм)
const
  cMarginLeft   = 300;
  cMarginTop    = 100;
  cMarginRight  = 100;
  cMarginBottom = 200;

  PS_Count              = 3;
  PS_Table_TypeID       = 0;
  PS_Card_TypeID        = 1;
  PS_Chart_TypeID       = 2;
  PS_Caption : array[0 .. PS_Count-1] of string = ('_Da.ViewList', '_Da.ViewCard', '_Da.Viewchart');
  PS_Ico : array[0 .. PS_Count-1] of integer = (60, 64, 22);

  PFS_Title             = 'Title';
  PFS_Section           = 'Section';
  PFS_ColumnHeader      = 'ColumnHeader';
  PFS_Label             = 'Label';
  PFS_Data              = 'Data';

type
  EDeNamedObjectList = class(Exception);

type
  TDePrintStyleType = 0..15;
  TDePrintStyleTypes = set of TDePrintStyleType;

type
  TDePrintStyle = class
  private
    FTypeID    : TDePrintStyleType;
    FIco       : Integer;
    FFonts     : TStringList;
    FOrientation : TPrinterOrientation;
    FFields      : TRect;
  protected
    procedure AddFont(aType: String; aName: String; aColor: TColor; aSize: Integer; aStyle: TFontStyles);
  public
    constructor Create;
    destructor  Destroy;override;
    property    Ico: Integer read FIco write FIco;
    property    TypeID: TDePrintStyleType read FTypeID write FTypeID;
    property    Orientation:TPrinterOrientation read FOrientation write FOrientation;
    property    Fields:TRect read FFields write FFields; //in 0.1 mm units
    function    FontByName(aName: string): TFont;
  end;

type
  TDePrintStyles = class(TStringList)
  private
  protected
    function    GetStyle(aIndex: Integer): TDePrintStyle;
    function    GetStyleByName(aName: string): TDePrintStyle;
  public
    procedure   InitPrintStyles;
    function    Add(const S: string):integer; override;
    property    Style[aIndex: String]:TDePrintStyle read GetStyleByName; default;
  end;

var
  PrintStyles : TDePrintStyles;

implementation

uses DeLog;

{TDePrintStyle}

constructor TDePrintStyle.Create;
begin
  inherited Create;
  FTypeID:= 0;
  FIco:= 13;
  FFonts:= TStringList.Create;
  FOrientation := poPortrait;
  FFields:= Rect(cMarginLeft, cMarginTop, cMarginRight, cMarginBottom);
end;

destructor  TDePrintStyle.Destroy;
var i: Integer;
begin
  for i:=0 to FFonts.Count-1 do
    FFonts.Objects[i].Free;

  FFonts.Free;
  inherited Destroy;
end;

function TDePrintStyle.FontByName(aName: string): TFont;
var R: Integer;
begin
  R:=FFonts.IndexOf(aName);
  if 0<=R then Result:=TFont(FFonts.Objects[R])
          else Result:= nil;
end;

procedure TDePrintStyle.AddFont(aType: String; aName: String; aColor: TColor; aSize: Integer; aStyle: TFontStyles);
begin
  FFonts.AddObject(aType, TFont.Create);
  with TFont(FFonts.Objects[FFonts.Count-1]) do
  begin
    Name  := aName;
    Color := aColor;
    Size  := aSize;
    Style := aStyle;
  end
end;

{TDePrintStyles}

function TDePrintStyles.getStyle(aIndex: Integer): TDePrintStyle;
begin
  Result:= TDePrintStyle(Objects[aIndex]);
end;

function TDePrintStyles.GetStyleByName(aName:string):TDePrintStyle;
var N: Integer;
begin
  N:= IndexOfName(aName);
  if 0<=N then Result := TDePrintStyle(Objects[N])
          else Result := nil;
end;

function TDePrintStyles.Add(const S: string): integer;
begin
  Result:= AddObject(S+'=', TDePrintStyle.Create);
end;

procedure  TDePrintStyles.InitPrintStyles;
var N: Integer;
begin
  N:= PrintStyles.Add(PS_Caption[0]);
  with PrintStyles.getStyle(N) do
  begin
    TypeID := PS_Table_TypeID;
    Ico:= 60;
    AddFont(PFS_Title, 'Arial', $00993333, 14, []);
    AddFont(PFS_Section, 'Arial', $00993333, 10, [fsBold]);
    AddFont(PFS_ColumnHeader, 'Arial', clBlack, 8, [fsBold]);
    AddFont(PFS_Label, 'Arial', clBlack, 8, [fsBold]);
    AddFont(PFS_Data, 'Arial', clBlack, 8, []);
  end;

  N:= PrintStyles.Add(PS_Caption[1]);
  with PrintStyles.GetStyle(N) do
  begin
    TypeID := PS_Card_TypeID;
    Ico:= 64;
    Orientation := poLandscape;
    AddFont(PFS_Title, 'Arial', $00993333, 14, []);
    AddFont(PFS_Section, 'Arial', $00993333, 10, [fsBold]);
    AddFont(PFS_ColumnHeader, 'Arial', clBlack, 8, [fsBold]);
    AddFont(PFS_Label, 'Arial', clBlack, 8, [fsBold]);
    AddFont(PFS_Data, 'Arial', clBlack, 8, []);
  end;

  N:= PrintStyles.Add(PS_Caption[2]);
  with PrintStyles.GetStyle(N) do
  begin
    TypeID := PS_Chart_TypeID;
    Ico:= 22;
    AddFont(PFS_Title, 'Arial', $00993333, 14, []);
  end;

end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('DePrintStyles unit initialization ...');
  {$ENDIF}
  PrintStyles := TDePrintStyles.Create;
  PrintStyles.InitPrintStyles;
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DePrintStyles unit finalization ...');
  {$ENDIF}
  PrintStyles.Free;
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

