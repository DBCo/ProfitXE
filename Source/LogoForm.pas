{$WARN SYMBOL_PLATFORM OFF}

unit LogoForm;

interface

uses
  Windows, Messages, SysUtils, {Variants, }Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ToolWin, ComCtrls, Vcl.StdCtrls;

type
  TFormLogo = class(TForm)
    LogoPanel: TPanel;
    ImageR: TImage;
    ImageL: TImage;
    SeparatorPanelT: TPanel;
    PanelB: TPanel;
    SeparatorPanel2: TPanel;
    ToolBarR: TToolBar;
    ToolBarL: TToolBar;
    ImageC: TImage;
    BoldCaption: TLabel;
    NormalCaption: TLabel;
    LiteCaption: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function LoadLeftLogoPucture(Picture: TPicture): Boolean;
function LoadCenterLogoPucture(Picture: TPicture): Boolean;
function LoadRightLogoPucture(Picture: TPicture): Boolean;

function GetHolidayDirectory(const DateTime: TDateTime = 0): string;

implementation

uses SysConst, DateUtils, JPEG, GIFImg, DeLog, DeTypes, uIconUtils;

{$R *.dfm}

function LoadCustomPuctureFile(const Name: string; const {Width,} Height: Integer; Picture: TPicture; const DefaultAssigned: Boolean = True; const Path: string = ''): Boolean;
var
  FileName: string;
  JPEGImage: TJPEGImage;
  Bitmap: TBitmap;
  GIFImage: TGIFImage;
begin
  Result := Length(Name) <> 0;
  if Result then
    begin
      FileName := Path;
      if Length(FileName) = 0 then
        FileName := ExtractFilePath(Application.ExeName);
      if Length(FileName) <> 0 then
        FileName := IncludeTrailingPathDelimiter(FileName);
      FileName := FileName + Name + sExtensionGIF;
      Result := FileExists(FileName);
      if Result then
        begin
          GIFImage := TGIFImage.Create;
          try
            GIFImage.LoadFromFile(FileName);
            Result := {(GIFImage.Width = Width) and} (GIFImage.Height = Height);
            if Result then
              begin
                if Assigned(GIFImage.Images) and (GIFImage.Images.Count > 1) then
                  GIFImage.Animate := True;
                Picture.Assign(GIFImage);
              end;
          finally
            GIFImage.Free;
          end;
        end;
      if not Result then
        begin
          FileName := ChangeFileExt(FileName, sExtensionJPG);
          Result := FileExists(FileName);
          if Result then
            begin
              JPEGImage := TJPEGImage.Create;
              try
                JPEGImage.LoadFromFile(FileName);
                Result := {(JPEGImage.Width = Width) and} (JPEGImage.Height = Height);
                if Result then
                  Picture.Assign(JPEGImage);
              finally
                JPEGImage.Free;
              end;
            end;
        end;
      if not Result then
        begin
          FileName := ChangeFileExt(FileName, sExtensionBMP);
          Result := FileExists(FileName);
          if Result then
            begin
              Bitmap := TBitmap.Create;
              try
                Bitmap.LoadFromFile(FileName);
                Result := {(Bitmap.Width = Width) and} (Bitmap.Height = Height);
                if Result then
                  Picture.Assign(Bitmap);
              finally
                Bitmap.Free;
              end;
            end;
        end;
      if DefaultAssigned and not Result then
        begin
          Result := FindResource(hInstance, PChar(Name), RT_BITMAP) <> 0;
          if Result then
            begin
              Bitmap := TBitmap.Create;
              try
                Bitmap.LoadFromResourceName(hInstance, Name);
                Picture.Assign(Bitmap);
              finally
                Bitmap.Free;
              end;
            end;
        end;
    end;
end;

function GetHolidayDirectory(const DateTime: TDateTime): string;
begin
  Result := ExtractFilePath(Application.ExeName);
  if Length(Result) <> 0 then
    Result := IncludeTrailingBackslash(Result);
  Result := IncludeTrailingBackslash(Result + 'Holidays');
  if DateTime <> 0 then
    begin
      case MonthOf(DateTime) of
        MonthJanuary: Result := Result + sLongMonthNameJan;
        MonthFebruary: Result := Result + sLongMonthNameFeb;
        MonthMarch: Result := Result + sLongMonthNameMar;
        MonthApril: Result := Result + sLongMonthNameApr;
        MonthMay: Result := Result + sLongMonthNameMay;
        MonthJune: Result := Result + sLongMonthNameJun;
        MonthJuly: Result := Result + sLongMonthNameJul;
        MonthAugust: Result := Result + sLongMonthNameAug;
        MonthSeptember: Result := Result + sLongMonthNameSep;
        MonthOctober: Result := Result + sLongMonthNameOct;
        MonthNovember: Result := Result + sLongMonthNameNov;
        MonthDecember: Result := Result + sLongMonthNameDec;
      else
        Result := Result + FormatDateTime('MM', DateTime);
      end;
      Result := IncludeTrailingBackslash(Result);
      Result := IncludeTrailingBackslash(Result + FormatDateTime('DD', DateTime));
    end;
end;

function LoadLeftLogoPucture(Picture: TPicture): Boolean;
begin
  Result := LoadCustomPuctureFile('LOGO_L', 72, Picture, False, GetHolidayDirectory(Now));
  if not Result then
    Result := LoadCustomPuctureFile('LOGO_L', 72, Picture);
end;

function LoadCenterLogoPucture(Picture: TPicture): Boolean;
begin
  Result := LoadCustomPuctureFile('LOGO_C', 72, Picture, False, GetHolidayDirectory(Now));
  if not Result then
    Result := LoadCustomPuctureFile('LOGO_C', 72, Picture);
end;

function LoadRightLogoPucture(Picture: TPicture): Boolean;
begin
    Result := LoadCustomPuctureFile('LOGO_R', 72, Picture, False, GetHolidayDirectory(Now));
  if not Result then
    begin
      Result := LoadCustomPuctureFile('LOGO', 72, Picture);
      if not Result then
        Result := LoadCustomPuctureFile('LOGO_R', 72, Picture);
    end;
end;

{ TFormLogo }

procedure TFormLogo.FormCreate(Sender: TObject);
begin
  LoadLeftLogoPucture(ImageL.Picture); {ImageL.Picture.Bitmap.LoadFromResourceName(hInstance, 'LOGO_L');}
  LoadCenterLogoPucture(ImageC.Picture); // v.17.2
  LoadRightLogoPucture(ImageR.Picture); //  ImageR.Picture.Bitmap.LoadFromResourceName(hInstance, 'LOGO');

  BoldCaption.Caption:= EmptyStr;
  NormalCaption.Caption:= EmptyStr;
  LiteCaption.Caption:= EmptyStr;

  BoldCaption.Font.Size:= BoldCaption.Font.Size + 4;
  LiteCaption.Font.Color:= clGray;
end;
{
procedure TFormLogo.ImageRClick(Sender: TObject);
var
  R1,R2,C1,C2,P: Byte;
  I1,I2,I: TIcon;
begin
  R1 := Random(255);
  while FindResource(hInstance, PChar('P' + IntToHex(R1, 2)), RT_GROUP_ICON) = 0 do
    R1 := Random(255);

  R2 := Random(255);
  while FindResource(hInstance, PChar('P' + IntToHex(R2, 2)), RT_GROUP_ICON) = 0 do
    R2 := Random(255);

  C1:=Random(16);
  C2:=Random(16);
  P:=Random(12);

  I1 := TIcon.Create;
  I2 := TIcon.Create;
  I := TIcon.Create;
  try
    I1.Handle := LoadImage(hInstance, PChar('P' + IntToHex(R1, 2)), IMAGE_ICON, 64, 64, 0);
    I2.Handle := LoadImage(hInstance, PChar('P' + IntToHex(R2, 2)), IMAGE_ICON, 32, 32, 0);

    UpdateColorIcon(I1, I, TSchemaColor(C1));
    I1.Assign(I);
    UpdateColorIcon(I2, I, TSchemaColor(C2));
    I2.Assign(I);

    UpdateOverlayIcon(I1, I2, I, TOverlayIconPosition(P));

    ImageR.Picture.Assign(I);
  finally
    I.Free;
    I1.Free;
    I2.Free;
  end;
end;
{}
{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('LogoForm unit initialization ...');
  {$ENDIF}
  RegisterClass(TFormLogo);
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('LogoForm unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
    Startup;

finalization
  Shutdown;

end.

