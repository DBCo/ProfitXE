unit uIconUtils;

interface

uses Graphics, ImgList, System.SysUtils, DeTypes;

type
  /// <summary>��� ����������� �������� ����� ��� ����������</summary>
  TSchemaColor = 0..15; // ����� 16 �������� ����
  /// <summary>��� ������������ ������ ������ ��� ����������</summary>
  TIconIndex = 0..255;

/// <summary>������� ������ ����� � ������</summary>
/// <param name="SourceIcon">������� ������, ������� ��������� �����������</param>
/// <param name="TargetIcon">�������������� ������, ������� ����� �������� (���� ������ ������� ����� �������� ������ �������)</param>
/// <param name="TargetColor">����� ���� ��������</param>
/// <param name="SourceColor">������ ���� ��������, ������� ����� �������� (���� ������� ��������� clNone, �� �������� ����� ���� ��������, � ����� ������ ���������� �����)</param>
/// <returns>���������� True � ������ ��������� ��������, ����� - False</returns>
/// <remarks>������ ����� ���������� ������ � 32-� ������ ������� � ����� �������!</remarks>
function UpdateColorIcon(SourceIcon, TargetIcon: TIcon; const TargetColor: TColor): Boolean;

/// <summary>������� ��������� ������������ � ������</summary>
/// <param name="SourceIcon">������� ������, ������������ ������� ��������� ���������</param>
/// <param name="TargetIcon">�������������� ������, ������� ����� �������� (���� ������ ������� ����� �������� ������ �������)</param>
/// <returns>���������� True � ������ ��������� ��������, ����� - False</returns>
/// <remarks>��������� ������������ ���������� ������ � 32-� ������ ������� � ����� �������!</remarks>
function UpdateAlphaIcon(SourceIcon, TargetIcon: TIcon; aValue: double): Boolean;

/// <summary>������� �������� �����-������ � ����� � ������</summary>
/// <param name="SourceIcon">������� ������ ��� ��������</param>
/// <param name="TargetIcon">�������������� ������, ������� ����� ������������� (���� ������ ������� ����� �������� ������ �������)</param>
/// <returns>���������� True � ������ ��������� ��������, ����� - False</returns>
/// <remarks>�������� ���������� ������ � 32-� ������ ������� � ����� �������!</remarks>
function UpdateInvertIcon(SourceIcon, TargetIcon: TIcon): Boolean;

type
  /// <summary>������� ��������� ����� ������ �� ������:
  /// <para><c>1</c> - ����� ������;</para>
  /// <para><c>2</c> - �� ������ ������;</para>
  /// <para><c>3</c> - ������ ������;</para>
  /// <para><c>4</c> - ����� �� ������;</para>
  /// <para><c>5</c> - �� ������;</para>
  /// <para><c>6</c> - ������ �� ������;</para>
  /// <para><c>7</c> - ����� �����;</para>
  /// <para><c>8</c> - �� ������ �����;</para>
  /// <para><c>9</c> - ������ �����</para>
  /// </summary>
  TOverlayIconPosition = 0..31;
  //  +---+---+---+
  //  | 1 | 2 | 3 |
  //  +---+---+---+
  //  | 4 | 5 | 6 |
  //  +---+---+---+
  //  | 7 | 8 | 9 |
  //  +---+---+---+

/// <summary>������� ��������� ����� ������ �� ������ ������</summary>
/// <param name="SourceIcon">������, �� ������� ��������� �������� ������ ������</param>
/// <param name="OverlayIcon">������, ������� ����� �������� ������ �������� ������</param>
/// <param name="TargetIcon">�������������� ������, � ������� ����� ������ � ����������</param>
/// <param name="Position">������� ��������� ������:
/// <para><c>0</c> - �������� ���������� �� ������� ������ ���� �� ������</para>
/// <para><c>1</c> - ����� ������;</para>
/// <para><c>2</c> - �� ������ ������;</para>
/// <para><c>3</c> - ������ ������;</para>
/// <para><c>4</c> - ����� �� ������;</para>
/// <para><c>5</c> - �� ������;</para>
/// <para><c>6</c> - ������ �� ������;</para>
/// <para><c>7</c> - ����� �����;</para>
/// <para><c>8</c> - �� ������ �����;</para>
/// <para><c>9</c> - ������ �����;</para>
/// <para><c>10</c> - �������� ���������� �� ������� ������ ���� �� ������ � ������;</para>
/// <para><c>11</c> - �������� ������ ���� �� ������ � ������ �� ������ (������ 5)</para>
/// <para><c>12</c> - �� ������, �� ��� ���������� ������ ������ �������� ������������� ������ (������ 5 ��� ��������� ������)</para>
/// </param>
/// <returns>���������� True � ������ ��������� ��������, ����� - False</returns>
/// <remarks>��������� ������ �������� ������ ��� 32-� ������ ������ � ����� �������!</remarks>
function UpdateOverlayIcon(_Width,_Height: Integer; SourceIcon, OverlayIcon, TargetIcon: TIcon; const Position: TOverlayIconPosition): Boolean;

/// <summary>������� ���������� � 2 ���� ������</summary>
/// <param name="SourceIcon">������� ������, ������� ��������� ��������</param>
/// <param name="TargetIcon">�������������� ������</param>
/// <returns>���������� True � ������ ��������� ��������, ����� - False</returns>
/// <remarks>���������� ���������� ������ ��� 32-� ������ ������ � ����� �������!</remarks>
function UpdateHalfSizeIcon(SourceIcon, TargetIcon: TIcon): Boolean;

type
  TSchemaColorArray = array[TSchemaColor] of TColor;

const
  /// <summary>����������� �������� ����� ��� ����������</summary>
  /// ������ ������ �������, ����� ����� ���� �� ������ ��������� ����������� ��������������.
  /// $64 = 100 = 1.0 �������� ����� - �.�. ��� ��������������
  /// ������������ ��� ����������� �������������� ��������� ����� * K / 100, �������� ��������
  /// 0 0.01 0,02 .... 0.99 1.00 1.01 .. 2.54
  ///           A H L S
  clNormal   = $FFFFFFFF;  // ������ �� ������
  clFocused  = $FFFFA0A0;  // ������ Light A0 = 160/256 � Saturation A0 = 160/256
  clDisabled = $60FFFFFF;  // ������ Alpha E0 = 224/256 // � Light 40 = 64/256
  clGrayed   = $FFFFFF00;  // ������ Saturation 00 = 0/256

  SchemaColors: TSchemaColorArray =
    (
      clNormal,  // �� �������� (��� ��������� �� ��������)
      clFocused, //      Black
      clDisabled,//      White
      clGrayed,  //      Gray
      $003380CC, //  30
      $0033CCCC, //  60
      $0033CC80, //  90
      $0033CC33, // 120  Green
      $0080CC33, // 150
      $00CCCC33, // 180
      $00CC8033, // 210
      $00CC3333, // 240  Blue
      $00CC3380, // 270
      $00CC33CC, // 300
  //    $FFEB963C,  // ������� ������� ������� :(
      $008033CC, // 330
      $003333CC //  360  Red
    );

  SchemaColorsRGB: TSchemaColorArray =
    (
      clNormal,  // �� �������� (��� ��������� �� ��������)  // �� ������
      $00000000, //      Black                                // ������ �������      *1.3 - ���������
      $00FFFFFF, //      White                                // ������ ������������ *0.5 - ������������
      $00C0C0C0, //      Gray                                 // ������ ������������ *0.0 - � ������� ������
      $003380CC, //  30
      $0033CCCC, //  60
      $0033CC80, //  90
      $0033CC33, // 120  Green
      $0080CC33, // 150
      $00CCCC33, // 180
      $00CC8033, // 210
      $00CC3333, // 240  Blue
      $00CC3380, // 270
      $00CC33CC, // 300
  //    $FFEB963C,  // ����� ������� ������� :(
      $008033CC, // 330
      $003333CC //  360  Red
    );

  MaskSchemaColor = $0F; // ����� �������� �����

function SourceSize(aWidth, aHeight, aPosition: Integer): Integer;
function OverlaySize(aWidth, aHeight, aPosition: Integer): Integer;

/// <summary>������� ���������� ������ �� ������� ����������</summary>
/// <param name="ResourceName">��� ������� ������</param>
/// <param name="ImageLists">������ ������� ������ (���� ��������� ������)</param>
/// <returns>���������� ������ ����������� ������ � ������ ������ ��� -1 � ������ ������</returns>
function LoadIconGroupToImageLists(const ResourceName: string; const ImageLists: array of TCustomImageList): Integer; overload;

/// <summary>������� ���������� ������ �� ������� ���������� � ������ ������</summary>
/// <param name="ResourceName">��� ������� ������</param>
/// <param name="NormalLists">������ ������� ������� ������ (���� ��������� ������� ������)</param>
/// <param name="HotLists">������ ������� ���������� ������ (���� ��������� ���������� ������)</param>
/// <param name="DisabledLists">������ ������� ���������� ������ (���� ��������� ���������� ������)</param>
/// <param name="DisabledColor">���� ���������� ������</param>
/// <returns>���������� ������ ����������� ������ � ������ ������ ��� -1 � ������ ������</returns>
function LoadIconGroupToImageLists(const ResourceName: string; const NormalLists, HotLists, DisabledLists: array of TCustomImageList; const DisabledColor: TColor = clDisabled): Integer; overload;

/// <summary>������� �������� ����� � �����-������ � ������ ��� ������� ������</summary>
/// <param name="IconIndex">������ ������ � ������� ������� ������, �������� ������� ��������� ���������</param>
/// <param name="SourceLists">������� ������ ������, �������� ������ ������� ��������� ���������</param>
/// <param name="TargetLists">�������������� ������ ������, � ������� ����� ��������� ��������������� ������</param>
/// <returns>���������� ������ ����������� ������ � ������ ������ ��� -1 � ������ ������</returns>
/// <remarks>�������� ���������� ������ � 32-� ������ ������� � ����� �������!</remarks>
function LoadInvertIconToImageLists(const IconIndex: Integer; const SourceLists: array of TCustomImageList; const TargetLists: array of TCustomImageList): Integer;

/// <summary>������� ������ ����� � ������ ��� ������� ������</summary>
/// <param name="IconIndex">������ ������ � ������� ������� ������, ���� ������� ��������� ��������</param>
/// <param name="SourceLists">������� ������ ������, ���� ������ ������� ��������� ��������</param>
/// <param name="TargetLists">�������������� ������ ������, � ������� ����� ��������� ������ � ��������� ������</param>
/// <param name="TargetColor">����� ���� ��������</param>
/// <param name="SourceColor">������ ���� ��������, ������� ����� �������� (���� ������� ��������� clNone, �� �������� ����� ���� ��������, � ����� ������ ���������� �����)</param>
/// <returns>���������� ������ ����������� ������ � ������ ������ ��� -1 � ������ ������</returns>
/// <remarks>������ ����� ���������� ������ � 32-� ������ ������� � ����� �������!</remarks>
function LoadColorIconToImageLists(const IconIndex: Integer; const SourceLists: array of TCustomImageList; const TargetLists: array of TCustomImageList; const TargetColor: TColor): Integer;

/// <summary>������� ��������� ����� ������ �� ������ ������ ��� ������� ������</summary>
/// <param name="IconIndex">������ ������� ������ � ������� ������� ������</param>
/// <param name="OverlayIndex">������ ������������� ������ � ������� ������� ������</param>
/// <param name="SourceLists">������� ������ ������, ������ ������� ��������� ��������</param>
/// <param name="TargetLists">�������������� ������ ������, � ������� ����� ��������� ������ � ���������� ������ ������</param>
/// <param name="Position">������� ��������� ������</param>
/// <returns>���������� ������ ����������� ������ � ������ ������ ��� -1 � ������ ������</returns>
/// <remarks>��������� ������ �������� ������ ��� 32-� ������ ������ � ����� �������!</remarks>
function LoadOverlayIconToImageLists(const IconIndex, OverlayIndex: Integer; const SourceLists: array of TCustomImageList; const TargetLists: array of TCustomImageList; const Position: TOverlayIconPosition): Integer;

/// <summary>��������� �������� ������ �� ������� ������</summary>
/// <param name="IconIndex">������ ��������� ������ �� �������</param>
/// <param name="ImageLists">������ ������� ������ (������ ������� ������)</param>
procedure DeleteIconFromImageLists(const IconIndex: Integer; const ImageLists: array of TCustomImageList);

/// <summary>������� ������ �������� ����� ������ ��� ������� ������</summary>
/// <param name="IconIndex">������ ������ � ������� ������� ������, �������� ����� ������� ��������� ��������</param>
/// <param name="NormalLists">������ ������� ������� ������ (���� ��������� ������� ������)</param>
/// <param name="HotLists">������ ������� ���������� ������ (���� ��������� ���������� ������)</param>
/// <param name="DisabledLists">������ ������� ���������� ������ (���� ��������� ���������� ������)</param>
/// <param name="DisabledColor">���� ���������� ������</param>
/// <returns>���������� ������ ����������� ������ � ������ ������ ��� -1 � ������ ������</returns>
function CopyShemaIconToImageLists(const IconIndex: Integer; const SchemaColor: TSchemaColor; const NormalLists, HotLists, DisabledLists: array of TCustomImageList; const DisabledColor: TColor = clDisabled): Integer;

/// <summary>������� ������ �������� ����� ������ ��� ������� ������</summary>
/// <param name="IconIndex">������ ������� ������ � ������� ������</param>
/// <param name="OverlayIndex">������ ������������� ������ � ������� ������</param>
/// <param name="NormalLists">������ ������� ������� ������ (���� ��������� ������� ������)</param>
/// <param name="HotLists">������ ������� ���������� ������ (���� ��������� ���������� ������)</param>
/// <param name="DisabledLists">������ ������� ���������� ������ (���� ��������� ���������� ������)</param>
/// <param name="Position">������� ��������� ������</param>
/// <returns>���������� ������ ����������� ������ � ������ ������ ��� -1 � ������ ������</returns>
function CopyOverlayIconToImageLists(const IconIndex, OverlayIndex: Integer; const NormalLists, HotLists, DisabledLists: array of TCustomImageList; const Position: TOverlayIconPosition): Integer;

/// <summary>������� �������� ����� � �����-������ � ������ ��� ������� ������</summary>
/// <param name="IconIndex">������ ������ � ������� ������� ������, �������� ������� ��������� ���������</param>
/// <param name="NormalLists">������ ������� ������� ������ (���� ��������� ������� ������)</param>
/// <param name="HotLists">������ ������� ���������� ������ (���� ��������� ���������� ������)</param>
/// <param name="DisabledLists">������ ������� ���������� ������ (���� ��������� ���������� ������)</param>
/// <returns>���������� ������ ����������� ������ � ������ ������ ��� -1 � ������ ������</returns>
function CopyInvertIconToImageLists(const IconIndex: Integer; const NormalLists, HotLists, DisabledLists: array of TCustomImageList): Integer;

/// <summary>������� ��������� ������� ������ �� ������� ����������</summary>
/// <param name="ResourceID">������������� ������� ������</param>
/// <param name="IconIndex">������ ��������� ������</param>
/// <returns>���������� True ��� ������� ��������� ������ � ��������, ����� ������� ���������� False</returns>
/// <remarks>��� �������� � ����� IShellLink.SetIconLocation</remarks>
function TryResourceToIconIndex(const ResourceID: Word; var IconIndex: Integer): Boolean; overload;

/// <summary>������� ��������� ������� ������ �� ������� ����������</summary>
/// <param name="ResourceName">��� ������� ������</param>
/// <param name="IconIndex">������ ��������� ������</param>
/// <returns>���������� True ��� ������� ��������� ������ � ��������, ����� ������� ���������� False</returns>
/// <remarks>��� �������� � ����� IShellLink.SetIconLocation</remarks>
function TryResourceToIconIndex(const ResourceName: string; var IconIndex: Integer): Boolean; overload;

type
  TIconRecord = packed record
    IconIndex: TIconIndex;      // ����� ������ (0..255)
    SchemaColor: TSchemaColor;  // �������� ����� ������ (0..15)
  end;

  TIconDescrOption =
    (
      idoBaseIcon,     // ������� ������� ������
      idoCoverIcon,    // ������� ������-��������
      idoPosition,     // ������� ���������������� ������� ������
      idoInversed      // ������� �������� ������� ������
    );
  TIconDescrOptions = set of TIconDescrOption;

  TIconDescr = packed record
    BaseIcon: TIconRecord;          // ���������� � ������� ������
    CoverIcon: TIconRecord;         // ���������� � ������������� ������
    Position: TOverlayIconPosition; // ������� ���������� ������
    Options: TIconDescrOptions;     // ����� ������
  end;

/// <summary>������� ��������� ���������� � ������ �� � �������</summary>
/// <param name="ImageIndex">������ ������ (�� ���� ������)</param>
/// <param name="IconDescr">���������� � ������� (���������)</param>
/// <returns>���������� True � ������ ��������� ��������, ����� - False</returns>
function TryImageIndexToIconDescr(const ImageIndex: Integer; var IconDescr: TIconDescr): Boolean;

/// <summary>������� ��������� ������� ������ �� � ����������</summary>
/// <param name="IconDescr">���������� � �������</param>
/// <param name="ImageIndex">������ ������ (��� ���� ������)</param>
/// <returns>���������� True � ������ ��������� ��������, ����� - False</returns>
function TryIconDescrToImageIndex(const IconDescr: TIconDescr; var ImageIndex: Integer): Boolean;

implementation

uses Windows, Classes, GraphUtil, Math, Funcs
     {$IFDEF DEBUG}, DeLog{$ENDIF};

type
  PAlphaRGB = ^TAlphaRGB;
  TAlphaRGB = packed record
    bBlue: Byte;   // �����
    bGreen: Byte;  // ������
    bRed: Byte;    // �������
    bAlpha: Byte;  // �����-�����
  end;

function UpdateColorIcon(SourceIcon, TargetIcon: TIcon; const TargetColor: TColor): Boolean;
var X, Y: Integer;
    SourceInfo, TargetInfo: TIconInfo;
    HeaderSize, ImageSize: Cardinal;
    Bitmap: Graphics.TBitmap;
    Header, Bits, Buffer, ScanLine: PByte;
    R, G, B, H, L, S: Integer;
    AA, HH, GG, SS, LL: Double;
begin
  Result := False;
  if Assigned(SourceIcon) and Assigned(TargetIcon) and GetIconInfo(SourceIcon.Handle, SourceInfo) then
    if (TargetColor = clNone) or (TargetColor = clNormal)
      then
        begin // ���� ��� �������������� - ������ ��������
          TargetIcon.Assign(SourceIcon);
           Result := True;
        end
      else
        begin  // ���� TargetColor - �������, � �� ���� - ������� ������������ ���������
          // ���������� �������� 1-254, �������� 128 - ����������� � �� �������� � ��������������
          // ���� �������� ��������������:
          // 0 ������������� � 0
          // 128 ������������� � �������� �������� ������������ (���� = 128, �� ������� ������ �� ������)
          // 255 ������������� � 255, ������ 0>0, 255>255 ��� ���� ������� �������
          if 0 < GetAValue(TargetColor) then  {HLSMAX=240}
            begin
              if GetAValue(TargetColor) <  255 then AA:= GetAValue(TargetColor)/256
                                               else AA:= 1;
              if GetHValue(TargetColor) <= 240 then begin GG:= 0; HH:= GetHValue(TargetColor); end
                                               else begin GG:= 1; HH:= 0; end;
              if GetLValue(TargetColor) <  255 then LL:= math.LogN(0.5, GetLValue(TargetColor)/256)
                                               else LL:= 1;
              if GetSValue(TargetColor) <  255 then SS:= math.LogN(0.5, GetSValue(TargetColor)/256)
                                               else SS:= 1;
           // if GetAValue(TargetColor) <  255 then AA:= math.LogN(0.5, GetAValue(TargetColor)/256)
           //                                  else AA:= 1;
            end
          else
            begin // ���� TargetColor - ����, �� ��������� ��������� ������������
              RGBtoHLS(GetRValue(TargetColor), GetGValue(TargetColor), GetBValue(TargetColor), H, L, S);
              AA:= 1;
              GG:= 0; HH:=H;
              LL:= 1;
              SS:= 1;
            end;

          // ����������� �� ����������� �������������
          try
            Bitmap := Graphics.TBitmap.Create;
            try
              Bitmap.PixelFormat := pf32bit;
              Bitmap.AlphaFormat := afDefined;
              Bitmap.Width := SourceIcon.Width;
              Bitmap.Height := SourceIcon.Height;
              GetDIBSizes(SourceInfo.hbmColor, HeaderSize, ImageSize);
              GetMem(Header, HeaderSize + ImageSize);
              try
                Bits := Header + HeaderSize;
                if GetDIB(SourceInfo.hbmColor, 0, Header^, Bits^) then
                  if (PBitmapInfo(Header)^.bmiHeader.biBitCount = 32) and (PBitmapInfo(Header)^.bmiHeader.biHeight = Bitmap.Height) and (PBitmapInfo(Header)^.bmiHeader.biWidth = Bitmap.Width) then
                    begin
                      Buffer := Bits;
                      for Y := Pred(Bitmap.Height) downto 0 do
                        begin
                          ScanLine := Bitmap.ScanLine[Y];
                          for X := 0 to Pred(Bitmap.Width) do
                            begin
                              RGBtoHLS(PAlphaRGB(Buffer)^.bRed, PAlphaRGB(Buffer)^.bGreen, PAlphaRGB(Buffer)^.bBlue, H,L,S);
                              HLStoRGB(Min(255, Round(GG*H + HH)), Round( 255*Math.Power( L/255, LL)), Round( 255*Math.Power( S/255, SS)), R, G, B);

                              PAlphaRGB(ScanLine)^.bRed   := R;
                              PAlphaRGB(ScanLine)^.bGreen := G;
                              PAlphaRGB(ScanLine)^.bBlue  := B;
                              PAlphaRGB(ScanLine)^.bAlpha := MAx(0, Min(255, Round( AA * (PAlphaRGB(Buffer)^.bAlpha))));
                           // PAlphaRGB(ScanLine)^.bAlpha := Round( 255 * Math.Power( PAlphaRGB(Buffer)^.bAlpha/255, AA));
                              ScanLine := ScanLine + SizeOf(TAlphaRGB);
                              Buffer := Buffer + SizeOf(TAlphaRGB);
                            end;
                        end;
                      TargetInfo.fIcon := True;
                      TargetInfo.xHotspot := 0;
                      TargetInfo.yHotspot := 0;
                      TargetInfo.hbmMask := SourceInfo.hbmMask;
                      TargetInfo.hbmColor := Bitmap.Handle;
                      TargetIcon.Handle := CreateIconIndirect(TargetInfo);
                      Result := TargetIcon.Handle <> 0;
                    end;
              finally
                FreeMem(Header);
              end;
            finally
              Bitmap.Free;
            end;
          finally
            DeleteObject(SourceInfo.hbmMask);
            DeleteObject(SourceInfo.hbmColor);
          end;

        Result := True;
      end;
end;

function UpdateAlphaIcon(SourceIcon, TargetIcon: TIcon; aValue: double): Boolean;
var
  X, Y: Integer;
  SourceInfo, TargetInfo: TIconInfo;
  HeaderSize, ImageSize: Cardinal;
  Bitmap: Graphics.TBitmap;
  Header, Bits, Buffer, ScanLine: PByte;
begin
  Result := False;
  if Assigned(SourceIcon) and Assigned(TargetIcon) and GetIconInfo(SourceIcon.Handle, SourceInfo) then
    try
      Bitmap := Graphics.TBitmap.Create;
      try
        Bitmap.PixelFormat := pf32bit;
        Bitmap.AlphaFormat := afDefined;
        Bitmap.Width := SourceIcon.Width;
        Bitmap.Height := SourceIcon.Height;
        GetDIBSizes(SourceInfo.hbmColor, HeaderSize, ImageSize);
        GetMem(Header, HeaderSize + ImageSize);
        try
          Bits := Header + HeaderSize;
          if GetDIB(SourceInfo.hbmColor, 0, Header^, Bits^) then
            if (PBitmapInfo(Header)^.bmiHeader.biBitCount = 32) and (PBitmapInfo(Header)^.bmiHeader.biHeight = Bitmap.Height) and (PBitmapInfo(Header)^.bmiHeader.biWidth = Bitmap.Width) then
              begin
                Buffer := Bits;
                for Y := Pred(Bitmap.Height) downto 0 do
                  begin
                    ScanLine := Bitmap.ScanLine[Y];
                    for X := 0 to Pred(Bitmap.Width) do
                      begin
                        PAlphaRGB(ScanLine)^.bRed   := PAlphaRGB(Buffer)^.bRed;
                        PAlphaRGB(ScanLine)^.bGreen := PAlphaRGB(Buffer)^.bGreen;
                        PAlphaRGB(ScanLine)^.bBlue  := PAlphaRGB(Buffer)^.bBlue;
                        PAlphaRGB(ScanLine)^.bAlpha := Min(255, Round(PAlphaRGB(Buffer)^.bAlpha*aValue));
                        ScanLine := ScanLine + SizeOf(TAlphaRGB);
                        Buffer := Buffer + SizeOf(TAlphaRGB);
                      end;
                  end;
                TargetInfo.fIcon := True;
                TargetInfo.xHotspot := 0;
                TargetInfo.yHotspot := 0;
                TargetInfo.hbmMask := SourceInfo.hbmMask;
                TargetInfo.hbmColor := Bitmap.Handle;
                TargetIcon.Handle := CreateIconIndirect(TargetInfo);
                Result := TargetIcon.Handle <> 0;
              end;
        finally
          FreeMem(Header);
        end;
      finally
        Bitmap.Free;
      end;
    finally
      DeleteObject(SourceInfo.hbmMask);
      DeleteObject(SourceInfo.hbmColor);
    end;
end;

function UpdateInvertIcon(SourceIcon, TargetIcon: TIcon): Boolean;
var
  X, Y: Integer;
  SourceInfo, TargetInfo: TIconInfo;
  HeaderSize, ImageSize: Cardinal;
  Bitmap, MaskBitmap, OldMaskBitmap: Graphics.TBitmap;
  Header, Bits, Buffer, ScanLine: PByte;
begin
  Result := False;
  if Assigned(SourceIcon) and Assigned(TargetIcon) and GetIconInfo(SourceIcon.Handle, SourceInfo) then
    try
      Bitmap := Graphics.TBitmap.Create;
      try
        MaskBitmap := Graphics.TBitmap.Create;
        try
          Bitmap.PixelFormat := pf32bit;
          Bitmap.AlphaFormat := afDefined;
          Bitmap.Width := SourceIcon.Width;
          Bitmap.Height := SourceIcon.Height;
          MaskBitmap.Monochrome := True;
          MaskBitmap.Width := Bitmap.Width;
          MaskBitmap.Height := Bitmap.Height;
          OldMaskBitmap := Graphics.TBitmap.Create;
          try
            OldMaskBitmap.Handle := SourceInfo.hbmMask;
            MaskBitmap.Canvas.Draw(0, 0, OldMaskBitmap);
          finally
            OldMaskBitmap.Free;
          end;
          GetDIBSizes(SourceInfo.hbmColor, HeaderSize, ImageSize);
          GetMem(Header, HeaderSize + ImageSize);
          try
            Bits := Header + HeaderSize;
            if GetDIB(SourceInfo.hbmColor, 0, Header^, Bits^) then
              if (PBitmapInfo(Header)^.bmiHeader.biBitCount = 32) and (PBitmapInfo(Header)^.bmiHeader.biHeight = Bitmap.Height) and (PBitmapInfo(Header)^.bmiHeader.biWidth = Bitmap.Width) then
                begin
                  Buffer := Bits;
                  for Y := Pred(Bitmap.Height) downto 0 do
                    begin
                      ScanLine := Bitmap.ScanLine[Y];
                      for X := 0 to Pred(Bitmap.Width) do
                        begin
                          PAlphaRGB(ScanLine)^.bRed := PAlphaRGB(Buffer)^.bRed;
                          PAlphaRGB(ScanLine)^.bGreen := PAlphaRGB(Buffer)^.bGreen;
                          PAlphaRGB(ScanLine)^.bBlue := PAlphaRGB(Buffer)^.bBlue;
                          PAlphaRGB(ScanLine)^.bAlpha := 255 - PAlphaRGB(Buffer)^.bAlpha;
                          if MaskBitmap.Canvas.Pixels[X, Y] = 0 then
                            MaskBitmap.Canvas.Pixels[X, Y] := clWhite
                          else
                            MaskBitmap.Canvas.Pixels[X, Y] := clBlack;
                          ScanLine := ScanLine + SizeOf(TAlphaRGB);
                          Buffer := Buffer + SizeOf(TAlphaRGB);
                        end;
                    end;
                  TargetInfo.fIcon := True;
                  TargetInfo.xHotspot := 0;
                  TargetInfo.yHotspot := 0;
                  TargetInfo.hbmMask := MaskBitmap.Handle; // SourceInfo.hbmMask;
                  TargetInfo.hbmColor := Bitmap.Handle;
                  TargetIcon.Handle := CreateIconIndirect(TargetInfo);
                  Result := TargetIcon.Handle <> 0;
                end;
          finally
            FreeMem(Header);
          end;
        finally
          MaskBitmap.Free;
        end;
      finally
        Bitmap.Free;
      end;
    finally
      DeleteObject(SourceInfo.hbmMask);
      DeleteObject(SourceInfo.hbmColor);
    end;
end;

function PreSize(aSize: Integer): Integer;
begin
  case aSize of
     0..12: Result:=  8;
    13..16: Result:= 12;
    17..24: Result:= 16;
    25..32: Result:= 24;
    33..48: Result:= 32;
    49..64: Result:= 48;
       else Result:= 64;
  end;
end;

function SourceSize(aWidth, aHeight, aPosition: Integer): Integer;
begin
  if aWidth <> aHeight then Exit(aWidth);

  case aPosition of
    0..10:  Result:= aWidth;
    11..14: Result:= PreSize(aWidth);
  end;
end;

function OverlaySize(aWidth, aHeight, aPosition: Integer): Integer;
begin
  if aWidth <> aHeight then Exit(aWidth);

  case aPosition of
    0:      Result:= aWidth;
    1..9:   Result:= aWidth div 2;
    10..14: Result:= PreSize(aWidth);
  end;
end;

function UpdateOverlayIcon(_Width,_Height: Integer; SourceIcon, OverlayIcon, TargetIcon: TIcon; const Position: TOverlayIconPosition): Boolean;
const
  AX : array[0..15] of integer = (0,  0, 0, 0,  0, 0, 0,  0, 0, 0,  0,  2, 0, 2, 0,  0);
  AY : array[0..15] of integer = (0,  0, 0, 0,  0, 0, 0,  0, 0, 0,  0,  2, 2, 0, 0,  0);

  DX : array[0..15] of integer = (0,  0, 1, 2,  0, 1, 2,  0, 1, 2,  1,  0, 2, 0, 2,  0);
  DY : array[0..15] of integer = (0,  0, 0, 0,  1, 1, 1,  2, 2, 2,  1,  0, 0, 2, 2,  0);
var
  N, X, Y, XX, YY, NX, NY, MX, MY, PX, PY, A, AA, H, W, HH, LL, SS : Integer;
  MainA, OverA, ColorA: Double;
  SourceInfo, OverlayInfo, TargetInfo: TIconInfo;
  HeaderSize, ImageSize: Cardinal;
  Bitmap, MaskBitmap, TmpMaskBitmap: Graphics.TBitmap;
  Header, Bits, Buffer, ScanLine : PByte;
  Scans: Array of Byte;
begin
  if (_Width < 16) and (Position>0) then Exit(True);

  Result := False;
  if not Assigned(SourceIcon) then Exit;
  if not Assigned(OverlayIcon) then Exit;
  if not Assigned(TargetIcon) then Exit;

  if Not (Position in [0,1..9,10,11..14]) then Exit;

  if GetIconInfo(SourceIcon.Handle, SourceInfo) then
    try
      if GetIconInfo(OverlayIcon.Handle, OverlayInfo) then
        try
          NX:= AX[Position] * (_Width - SourceIcon.Width) div 2;
          NY:= AY[Position] * (_Height - SourceIcon.Height) div 2;
          MX:= DX[Position] * (_Width - OverlayIcon.Width) div 2;
          MY:= DY[Position] * (_Height - OverlayIcon.Height) div 2;

          Bitmap := Graphics.TBitmap.Create;
          try
            Bitmap.PixelFormat := pf32bit;
            Bitmap.AlphaFormat := afDefined;
            Bitmap.Width := _Width;
            Bitmap.Height := _Height;
            MaskBitmap := Graphics.TBitmap.Create;
            try
              MaskBitmap.Monochrome := True;
              MaskBitmap.Width := Bitmap.Width;
              MaskBitmap.Height := Bitmap.Height;
              TmpMaskBitmap := Graphics.TBitmap.Create;
              try
                TmpMaskBitmap.Handle := SourceInfo.hbmMask;
                if (MaskBitmap.Width <> TmpMaskBitmap.Width) or (MaskBitmap.Height <> TmpMaskBitmap.Height) then
                  begin
                    for Y:=0 to Pred(Bitmap.Height) do
                      begin
                        ScanLine := Bitmap.ScanLine[Y];
                        for X:=0 to Pred(Bitmap.Width) do
                          begin
                            PAlphaRGB(ScanLine)^.bAlpha := 0;
                            ScanLine := ScanLine + SizeOf(TAlphaRGB);
                          end;
                      end

                  end;
                MaskBitmap.Canvas.Draw (NX, NY, TmpMaskBitmap);
              finally
                TmpMaskBitmap.Free;
              end;

              GetDIBSizes(SourceInfo.hbmColor, HeaderSize, ImageSize);
              GetMem(Header, HeaderSize + ImageSize);
              try
                Bits := Header + HeaderSize;
                if GetDIB(SourceInfo.hbmColor, 0, Header^, Bits^) then
                  if (PBitmapInfo(Header)^.bmiHeader.biBitCount = 32) and
                     (PBitmapInfo(Header)^.bmiHeader.biHeight = SourceIcon.Height) and
                     (PBitmapInfo(Header)^.bmiHeader.biWidth = SourceIcon.Width) then
                    begin
                      Buffer := Bits;
                        for Y := Pred(SourceIcon.Height) downto 0 do
                          begin
                            ScanLine := Bitmap.ScanLine[NY + Y];
                            ScanLine := ScanLine + NX * SizeOf(TAlphaRGB);

                            for X := 0 to Pred(SourceIcon.Width) do
                              begin
                                PAlphaRGB(ScanLine)^.bRed := PAlphaRGB(Buffer)^.bRed;
                                PAlphaRGB(ScanLine)^.bGreen := PAlphaRGB(Buffer)^.bGreen;
                                PAlphaRGB(ScanLine)^.bBlue := PAlphaRGB(Buffer)^.bBlue;
                                PAlphaRGB(ScanLine)^.bAlpha := PAlphaRGB(Buffer)^.bAlpha;  // ����� ���� �������� ������������ �������
                                ScanLine := ScanLine + SizeOf(TAlphaRGB);
                                Buffer := Buffer + SizeOf(TAlphaRGB);
                              end;
                          end;
                      Result := True;
                    end;
              finally
                FreeMem(Header);
              end;

              if Result then
                begin
                  Result := False;
                  {$IFDEF DEBUG}
                  //if OverlayIcon.Width in [8, 24] then
                  //  OverlayIcon.SaveToFile(LogDirectory + FormatDateTime('YYYYMMDDHHNNSSZZZ', Now) + '.ico');
                  {$ENDIF}
                  GetDIBSizes(OverlayInfo.hbmColor, HeaderSize, ImageSize);
                  GetMem(Header, HeaderSize + ImageSize);
                  try
                    Bits := Header + HeaderSize;
                    if GetDIB(OverlayInfo.hbmColor, 0, Header^, Bits^) then
                      if (PBitmapInfo(Header)^.bmiHeader.biBitCount = 32) and
                         (PBitmapInfo(Header)^.bmiHeader.biHeight = OverlayIcon.Height) and
                         (PBitmapInfo(Header)^.bmiHeader.biWidth = OverlayIcon.Width) then
                        begin
                          Buffer := Bits;

                          for Y := Pred(OverlayIcon.Height) downto 0 do
                            begin
                              ScanLine := Bitmap.ScanLine[MY + Y];
                              ScanLine := ScanLine + MX * SizeOf(TAlphaRGB);
                              for X := 0 to Pred(OverlayIcon.Width) do
                                begin
                                  if (PAlphaRGB(Buffer)^.bAlpha = 255) or (PAlphaRGB(ScanLine)^.bAlpha = 0) then
                                    begin
                                      MaskBitmap.Canvas.Pixels[MX + X, MY + Y]:= clBlack;
                                      Move(PAlphaRGB(Buffer)^.bBlue, PAlphaRGB(ScanLine)^.bBlue, 4);
                                    end else
                                  if PAlphaRGB(Buffer)^.bAlpha > 0 then
                                    begin
                                      // 0.0       - ��������� ���������
                                      // 1.0 = 255 - �� ���������
                                      MainA := PAlphaRGB(ScanLine)^.bAlpha / 255;
                                      OverA := PAlphaRGB(Buffer)^.bAlpha / 255;
                                      ColorA:= Min(1, (OverA) / (OverA + (1-OverA) * MainA));

                                      MaskBitmap.Canvas.Pixels[MX + X, MY + Y]:= clBlack;
                                      PAlphaRGB(ScanLine)^.bAlpha := Trunc(255*(MainA + (1-MainA)*(OverA)));
                                      PAlphaRGB(ScanLine)^.bRed   := Trunc( (1-ColorA) * PAlphaRGB(ScanLine)^.bRed   + (ColorA) * PAlphaRGB(Buffer)^.bRed);
                                      PAlphaRGB(ScanLine)^.bGreen := Trunc( (1-ColorA) * PAlphaRGB(ScanLine)^.bGreen + (ColorA) * PAlphaRGB(Buffer)^.bGreen);
                                      PAlphaRGB(ScanLine)^.bBlue  := Trunc( (1-ColorA) * PAlphaRGB(ScanLine)^.bBlue  + (ColorA) * PAlphaRGB(Buffer)^.bBlue);
                                    end;
                                  ScanLine := ScanLine + SizeOf(TAlphaRGB);
                                  Buffer := Buffer + SizeOf(TAlphaRGB);
                                end;
                            end;

                          TargetInfo.fIcon := True;
                          TargetInfo.xHotspot := 0;
                          TargetInfo.yHotspot := 0;
                          TargetInfo.hbmMask := MaskBitmap.Handle;
                          TargetInfo.hbmColor := Bitmap.Handle;
                          TargetIcon.Handle := CreateIconIndirect(TargetInfo);
                          Result := TargetIcon.Handle <> 0;
                          {$IFDEF DEBUG}
                          //if OverlayIcon.Width in [8, 24] then
                          //  TargetIcon.SaveToFile(LogDirectory + FormatDateTime('YYYYMMDDHHNNSSZZZ', Now) + '.ico');
                          {$ENDIF}
                        end;
                  finally
                    FreeMem(Header);
                  end;
                end;
            finally
              MaskBitmap.Free;
            end;
          finally
            Bitmap.Free;
          end;
        finally
          DeleteObject(OverlayInfo.hbmMask);
          DeleteObject(OverlayInfo.hbmColor);
        end;
    finally
      DeleteObject(SourceInfo.hbmMask);
      DeleteObject(SourceInfo.hbmColor);
    end;
end;

function UpdateHalfSizeIcon(SourceIcon, TargetIcon: TIcon): Boolean;
var
  X, Y: Integer;
  SourceInfo, TargetInfo: TIconInfo;
  HeaderSize, ImageSize, R, G, B, A: Cardinal;
  Bitmap, MaskBitmap: Graphics.TBitmap;
  Header, Bits, Buffer, ScanLine: PByte;
begin
  Result := False;
  if Assigned(SourceIcon) and Assigned(TargetIcon) and (not SourceIcon.Empty) and GetIconInfo(SourceIcon.Handle, SourceInfo) then
    try
      Bitmap := Graphics.TBitmap.Create;
      try
        Bitmap.PixelFormat := pf32bit;
        Bitmap.AlphaFormat := afDefined;
        Bitmap.Width := SourceIcon.Width div 2;
        Bitmap.Height := SourceIcon.Height div 2;
        MaskBitmap := Graphics.TBitmap.Create;
        try
          MaskBitmap.Monochrome := True;
          MaskBitmap.Width := Bitmap.Width;
          MaskBitmap.Height := Bitmap.Height;
          GetDIBSizes(SourceInfo.hbmColor, HeaderSize, ImageSize);
          GetMem(Header, HeaderSize + ImageSize);
          try
            Bits := Header + HeaderSize;
            if GetDIB(SourceInfo.hbmColor, 0, Header^, Bits^) then
              if (PBitmapInfo(Header)^.bmiHeader.biBitCount = 32) and (PBitmapInfo(Header)^.bmiHeader.biHeight = SourceIcon.Height) and (PBitmapInfo(Header)^.bmiHeader.biWidth = SourceIcon.Width) then
                for Y := 0 to Pred(Bitmap.Height) do
                  begin
                    ScanLine := Bitmap.ScanLine[Y];

                    for X := 0 to Pred(Bitmap.Width) do
                      begin
                        Buffer := Bits + (SizeOf(TAlphaRGB) * ((Pred(Bitmap.Height) - Y) * 2 * Bitmap.Width * 2 + (X * 2)));
                        A :=     PAlphaRGB(Buffer)^.bAlpha;
                        R :=     PAlphaRGB(Buffer)^.bAlpha * PAlphaRGB(Buffer)^.bRed;
                        G :=     PAlphaRGB(Buffer)^.bAlpha * PAlphaRGB(Buffer)^.bGreen;
                        B :=     PAlphaRGB(Buffer)^.bAlpha * PAlphaRGB(Buffer)^.bBlue;

                        Buffer := Bits + (SizeOf(TAlphaRGB) * ((Pred(Bitmap.Height) - Y) * 2 * Bitmap.Width * 2 + Succ(X * 2)));
                        A := A + PAlphaRGB(Buffer)^.bAlpha;
                        R := R + PAlphaRGB(Buffer)^.bAlpha * PAlphaRGB(Buffer)^.bRed;
                        G := G + PAlphaRGB(Buffer)^.bAlpha * PAlphaRGB(Buffer)^.bGreen;
                        B := B + PAlphaRGB(Buffer)^.bAlpha * PAlphaRGB(Buffer)^.bBlue;

                        Buffer := Bits + (SizeOf(TAlphaRGB) * (Succ((Pred(Bitmap.Height) - Y) * 2) * Bitmap.Width * 2 + Succ(X * 2)));
                        A := A + PAlphaRGB(Buffer)^.bAlpha;
                        R := R + PAlphaRGB(Buffer)^.bAlpha * PAlphaRGB(Buffer)^.bRed;
                        G := G + PAlphaRGB(Buffer)^.bAlpha * PAlphaRGB(Buffer)^.bGreen;
                        B := B + PAlphaRGB(Buffer)^.bAlpha * PAlphaRGB(Buffer)^.bBlue;

                        Buffer := Bits + (SizeOf(TAlphaRGB) * (Succ((Pred(Bitmap.Height) - Y) * 2) * Bitmap.Width * 2 + Succ(X * 2)));
                        A := A + PAlphaRGB(Buffer)^.bAlpha;
                        R := R + PAlphaRGB(Buffer)^.bAlpha * PAlphaRGB(Buffer)^.bRed;
                        G := G + PAlphaRGB(Buffer)^.bAlpha * PAlphaRGB(Buffer)^.bGreen;
                        B := B + PAlphaRGB(Buffer)^.bAlpha * PAlphaRGB(Buffer)^.bBlue;

                        PAlphaRGB(ScanLine)^.bAlpha := A div 4;
                        if A > 3 then
                          begin
                            PAlphaRGB(ScanLine)^.bRed := Byte(R div A);
                            PAlphaRGB(ScanLine)^.bGreen := Byte(G div A);
                            PAlphaRGB(ScanLine)^.bBlue := Byte(B div A);
                          end
                        else
                          begin
                            PAlphaRGB(ScanLine)^.bRed := 0;
                            PAlphaRGB(ScanLine)^.bGreen := 0;
                            PAlphaRGB(ScanLine)^.bBlue := 0;
                          end;

                        if PAlphaRGB(ScanLine)^.bAlpha = 0 then
                          MaskBitmap.Canvas.Pixels[X, Y] := clWhite
                        else
                          MaskBitmap.Canvas.Pixels[X, Y] := clBlack;
                        ScanLine := ScanLine + SizeOf(TAlphaRGB);
                      end;

                  TargetInfo.fIcon := True;
                  TargetInfo.xHotspot := 0;
                  TargetInfo.yHotspot := 0;
                  TargetInfo.hbmMask := MaskBitmap.Handle;
                  TargetInfo.hbmColor := Bitmap.Handle;
                  TargetIcon.Handle := CreateIconIndirect(TargetInfo);
                  {$IFDEF DEBUG}
                  //if MaskBitmap.Width in [8, 24] then
                  //  MaskBitmap.SaveToFile(LogDirectory + FormatDateTime('YYYYMMDDHHNNSSZZZ', Now) + 'm.bmp');
                  {$ENDIF}
                  Result := TargetIcon.Handle <> 0;
                end;
          finally
            FreeMem(Header);
          end;
        finally
          MaskBitmap.Free;
        end;
      finally
        Bitmap.Free;
      end;
    finally
      DeleteObject(SourceInfo.hbmMask);
      DeleteObject(SourceInfo.hbmColor);
    end;
end;

function LoadIconGroupToImageLists(const ResourceName: string; const ImageLists: array of TCustomImageList): Integer;
var
  Index, NewIndex: Integer;
  ImageList: TCustomImageList;
  Icon: TIcon;
begin
  Result := -1;
  for Index := Low(ImageLists) to High(ImageLists) do
    begin
      NewIndex := -1;
      ImageList := ImageLists[Index];
      if Assigned(ImageList) then
        begin
          Icon := TIcon.Create;
          try
            Icon.Handle := LoadImage(hInstance, PChar(ResourceName),  IMAGE_ICON, ImageList.Width, ImageList.Height, LR_SHARED);
            if Icon.Handle <> 0 then
              NewIndex := ImageList.AddIcon(Icon);
          finally
            Icon.Free;
          end;
        end;
      if Index = Low(ImageLists) then
        if NewIndex = -1 then
          Break
        else
          Result := NewIndex
      else
        if Result <> NewIndex then
          begin
            if Assigned(ImageList) and (NewIndex <> -1) then ImageList.Delete(NewIndex);
            for NewIndex := Pred(Index) downto Low(ImageLists) do
              begin
                ImageList := ImageLists[NewIndex];
                if Assigned(ImageList) then ImageList.Delete(Result);
              end;
            Result := -1; // ���������������� ������ ������!!!
            Break;
          end;
    end;
end;

function LoadIconGroupToImageLists(const ResourceName: string; const NormalLists, HotLists, DisabledLists: array of TCustomImageList; const DisabledColor: TColor): Integer;
var
  Index: Integer;
begin
  Result := LoadIconGroupToImageLists(ResourceName, NormalLists);
  if Result <> -1 then
    begin
      //Index := LoadLightIconToImageLists(Result, NormalLists, HotLists);
      //Index := LoadInvertIconToImageLists(Result, NormalLists, HotLists);
      Index := LoadColorIconToImageLists(Result, NormalLists, HotLists, clWhite);
      if Index = Result then
        begin
          if Length(DisabledLists) <> 0 then
            begin
              Index := LoadColorIconToImageLists(Result, NormalLists, DisabledLists, DisabledColor);
              if Index <> Result then
                begin
                  if Index <> -1 then DeleteIconFromImageLists(Index, DisabledLists);
                  DeleteIconFromImageLists(Result, NormalLists);
                  DeleteIconFromImageLists(Result, HotLists);
                  Result := -1; // ���������������� �������� ������!!!
                end;
            end;
        end
      else
        begin
          if Index <> -1 then DeleteIconFromImageLists(Index, HotLists);
          DeleteIconFromImageLists(Result, NormalLists);
          Result := -1; // ���������������� �������� ������!!!
        end;
    end;
end;

function FindImageList(const Width, Height: Integer; const ImageLists: array of TCustomImageList): TCustomImageList;
var
  Index: Integer;
  ImageList: TCustomImageList;
begin
  Result := nil;
  for Index := Low(ImageLists) to High(ImageLists) do
    begin
      ImageList := ImageLists[Index];
      if Assigned(ImageList) and (ImageList.Width = Width) and (ImageList.Height = Height) then Exit(ImageList);
    end;
end;

function LoadInvertIconToImageLists(const IconIndex: Integer; const SourceLists: array of TCustomImageList; const TargetLists: array of TCustomImageList): Integer;
var
  Index, NewIndex: Integer;
  TargetImageList, SourceImageList: TCustomImageList;
  SourceIcon, TargetIcon: TIcon;
begin
  Result := -1;
  for Index := Low(TargetLists) to High(TargetLists) do
    begin
      NewIndex := -1;
      TargetImageList := TargetLists[Index];
      if Assigned(TargetImageList) then
        begin
          SourceImageList := FindImageList(TargetImageList.Width, TargetImageList.Height, SourceLists);
          if Assigned(SourceImageList) then
            begin
              SourceIcon := TIcon.Create;
              try
                SourceImageList.GetIcon(IconIndex, SourceIcon);
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
      if Index = Low(TargetLists) then
        if NewIndex = -1 then
          Break
        else
          Result := NewIndex
      else
        if Result <> NewIndex then
          begin
            if Assigned(TargetImageList) and (NewIndex <> -1) then TargetImageList.Delete(NewIndex);
            for NewIndex := Pred(Index) downto Low(TargetLists) do
              begin
                TargetImageList := TargetLists[NewIndex];
                if Assigned(TargetImageList) then TargetImageList.Delete(Result);
              end;
            Result := -1; // ���������������� ������ ������!!!
            Break;
          end;
    end;
end;

function LoadColorIconToImageLists(const IconIndex: Integer; const SourceLists: array of TCustomImageList;
                                     const TargetLists: array of TCustomImageList; const TargetColor: TColor): Integer;
var
  Index, NewIndex: Integer;
  TargetImageList, SourceImageList: TCustomImageList;
  SourceIcon, TargetIcon: TIcon;
begin
  Result := -1;
  for Index := Low(TargetLists) to High(TargetLists) do
    begin
      NewIndex := -1;
      TargetImageList := TargetLists[Index];
      if Assigned(TargetImageList) then
        begin
          SourceImageList := FindImageList(TargetImageList.Width, TargetImageList.Height, SourceLists);
          if Assigned(SourceImageList) then
            begin
              SourceIcon := TIcon.Create;
              try
                SourceImageList.GetIcon(IconIndex, SourceIcon);
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
      if Index = Low(TargetLists) then
        if NewIndex = -1 then
          Break
        else
          Result := NewIndex
      else
        if Result <> NewIndex then
          begin
            if Assigned(TargetImageList) and (NewIndex <> -1) then TargetImageList.Delete(NewIndex);
            for NewIndex := Pred(Index) downto Low(TargetLists) do
              begin
                TargetImageList := TargetLists[NewIndex];
                if Assigned(TargetImageList) then TargetImageList.Delete(Result);
              end;
            Result := -1; // ���������������� ������ ������!!!
            Break;
          end;
    end;
end;

function LoadOverlayIconToImageLists(const IconIndex, OverlayIndex: Integer; const SourceLists: array of TCustomImageList; const TargetLists: array of TCustomImageList; const Position: TOverlayIconPosition): Integer;
var
  Index, NewIndex, SourceWH, OverlayWH: Integer;
  TargetImageList, SourceImageList, OverlayImageList: TCustomImageList;
  SourceIcon, OverlayIcon, TargetIcon: TIcon;
begin
  Result := -1;
  for Index := Low(TargetLists) to High(TargetLists) do
    begin
      NewIndex := -1;
      TargetImageList := TargetLists[Index];
      SourceWH:=  SourceSize(TargetImageList.Width, TargetImageList.Height, Position);
      OverlayWH:= OverlaySize(TargetImageList.Width, TargetImageList.Height, Position);
      if Assigned(TargetImageList) then
        begin
          SourceImageList := FindImageList(SourceWH, SourceWH, SourceLists);
          if Assigned(SourceImageList) then
            begin
              OverlayImageList := FindImageList(OverlayWH, OverlayWH, SourceLists);
              if Assigned(OverlayImageList) then
                begin
                  SourceIcon := TIcon.Create;
                  try
                    SourceImageList.GetIcon(IconIndex, SourceIcon);
                    OverlayIcon := TIcon.Create;
                    try
                      OverlayImageList.GetIcon(OverlayIndex, OverlayIcon);
                      TargetIcon := TIcon.Create;
                      try
                        if UpdateOverlayIcon(TargetImageList.Width, TargetImageList.Height, SourceIcon, OverlayIcon, TargetIcon, Position) then
                          NewIndex := TargetImageList.AddIcon(TargetIcon);
                      finally
                        TargetIcon.Free;
                      end;
                    finally
                      OverlayIcon.Free;
                    end;
                  finally
                    SourceIcon.Free;
                  end;
                end
              else
                begin
                  // ��� ��������� 16x16 �������� ����, �.�. ��� ������ 8x8!
                  TargetIcon := TIcon.Create;
                  try
                    SourceImageList.GetIcon(IconIndex, TargetIcon); // ����� �������� ���� ����� OverlayIndex ...
                    NewIndex := TargetImageList.AddIcon(TargetIcon);
                  finally
                    TargetIcon.Free;
                  end;
                end;
            end;
        end;
      if Index = Low(TargetLists) then
        if NewIndex = -1 then
          Break
        else
          Result := NewIndex
      else
        if Result <> NewIndex then
          begin
            if Assigned(TargetImageList) and (NewIndex <> -1) then TargetImageList.Delete(NewIndex);
            for NewIndex := Pred(Index) downto Low(TargetLists) do
              begin
                TargetImageList := TargetLists[NewIndex];
                if Assigned(TargetImageList) then TargetImageList.Delete(Result);
              end;
            Result := -1; // ���������������� ������ ������!!!
            Break;
          end;
    end;
end;

procedure DeleteIconFromImageLists(const IconIndex: Integer; const ImageLists: array of TCustomImageList);
var
  Index: Integer;
  ImageList: TCustomImageList;
begin
  if IconIndex >= 0 then
    for Index := Low(ImageLists) to High(ImageLists) do
      begin
        ImageList := ImageLists[Index];
        if Assigned(ImageList) and (IconIndex < ImageList.Count) then
          ImageList.Delete(IconIndex);
      end;
end;

function CopyShemaIconToImageLists(const IconIndex: Integer; const SchemaColor: TSchemaColor; const NormalLists, HotLists, DisabledLists: array of TCustomImageList; const DisabledColor: TColor): Integer;
var
  Index: Integer;
begin
  Result := LoadColorIconToImageLists(IconIndex, NormalLists, NormalLists, SchemaColors[SchemaColor]);
  if Result <> -1 then
    begin
      Index := LoadColorIconToImageLists(Result, NormalLists, HotLists, DisabledColor);
      if Index = Result then
        begin
          if Length(DisabledLists) <> 0 then
            begin
              Index := LoadColorIconToImageLists(Result, NormalLists, DisabledLists, DisabledColor);
              if Index <> Result then
                begin
                  if Index <> -1 then DeleteIconFromImageLists(Index, DisabledLists);
                  DeleteIconFromImageLists(Result, NormalLists);
                  DeleteIconFromImageLists(Result, HotLists);
                  Result := -1; // ���������������� �������� ������!!!
                end;
            end;
        end
      else
        begin
          if Index <> -1 then DeleteIconFromImageLists(Index, HotLists);
          DeleteIconFromImageLists(Result, NormalLists);
          Result := -1; // ���������������� �������� ������!!!
        end;
    end;
end;

function CopyOverlayIconToImageLists(const IconIndex, OverlayIndex: Integer; const NormalLists, HotLists, DisabledLists: array of TCustomImageList; const Position: TOverlayIconPosition): Integer;
var
  Index: Integer;
begin
  Result := LoadOverlayIconToImageLists(IconIndex, OverlayIndex, NormalLists, NormalLists, Position);
  if Result <> -1 then
    begin
      Index := LoadOverlayIconToImageLists(IconIndex, OverlayIndex, HotLists, HotLists, Position);
      if Index = Result then
        begin
          if Length(DisabledLists) <> 0 then
            begin
              Index := LoadOverlayIconToImageLists(IconIndex, OverlayIndex, DisabledLists, DisabledLists, Position);
              if Index <> Result then
                begin
                  if Index <> -1 then DeleteIconFromImageLists(Index, DisabledLists);
                  DeleteIconFromImageLists(Result, NormalLists);
                  DeleteIconFromImageLists(Result, HotLists);
                  Result := -1; // ���������������� �������� ������!!!
                end;
            end;
        end
      else
        begin
          if Index <> -1 then DeleteIconFromImageLists(Index, HotLists);
          DeleteIconFromImageLists(Result, NormalLists);
          Result := -1; // ���������������� �������� ������!!!
        end;
    end;
end;

function CopyInvertIconToImageLists(const IconIndex: Integer; const NormalLists, HotLists, DisabledLists: array of TCustomImageList): Integer;
var
  Index: Integer;
begin
  Result := LoadInvertIconToImageLists(IconIndex, NormalLists, NormalLists);
  if Result <> -1 then
    begin
      Index := LoadInvertIconToImageLists(IconIndex, HotLists, HotLists);
      if Index = Result then
        begin
          if Length(DisabledLists) <> 0 then
            begin
              Index := LoadInvertIconToImageLists(IconIndex, DisabledLists, DisabledLists);
              if Index <> Result then
                begin
                  if Index <> -1 then DeleteIconFromImageLists(Index, DisabledLists);
                  DeleteIconFromImageLists(Result, NormalLists);
                  DeleteIconFromImageLists(Result, HotLists);
                  Result := -1; // ���������������� �������� ������!!!
                end;
            end;
        end
      else
        begin
          if Index <> -1 then DeleteIconFromImageLists(Index, HotLists);
          DeleteIconFromImageLists(Result, NormalLists);
          Result := -1; // ���������������� �������� ������!!!
        end;
    end;
end;

type
  PFindResourceIconIndexRecord = ^TFindResourceIconIndexRecord;
  TFindResourceIconIndexRecord = packed record
    ResourceName: PChar;
    IconIndex: Integer;
    IndexFound: Boolean;
  end;

function EnumIconGroupToIconIndexResources(Module: HMODULE; ResType, ResName: PChar; Data: Pointer): BOOL; stdcall;
var
  FindResourceID, ResourceID: NativeInt;
  FindResourceName, ResourceName: string;
begin
  Result := Assigned(Data);
  if Result then
    begin
      FindResourceID := NativeInt(Pointer(ResName));
      ResourceID := NativeInt(Pointer(PFindResourceIconIndexRecord(Data)^.ResourceName));
      if FindResourceID > 65535 then
        begin
          if ResourceID > 65535 then
            begin
              FindResourceName := StrPas(ResName);
              ResourceName := StrPas(PFindResourceIconIndexRecord(Data)^.ResourceName);
              if SameText(FindResourceName, ResourceName) then
                begin
                  PFindResourceIconIndexRecord(Data)^.IndexFound := True;
                  Result := False;
                end;
            end;
        end
      else
        if FindResourceID = ResourceID then
          begin
            PFindResourceIconIndexRecord(Data)^.IndexFound := True;
            Result := False;
          end;
      if Result then Inc(PFindResourceIconIndexRecord(Data)^.IconIndex);
    end;
end;

function TryResourceToIconIndex(const ResourceID: Word; var IconIndex: Integer): Boolean;
var
  FindResourceIconIndexRecord: TFindResourceIconIndexRecord;
begin
  FindResourceIconIndexRecord.ResourceName := PChar(ResourceID);
  FindResourceIconIndexRecord.IconIndex := 0;
  FindResourceIconIndexRecord.IndexFound := False;
  EnumResourceNames(hInstance, RT_GROUP_ICON, @EnumIconGroupToIconIndexResources, NativeInt(@FindResourceIconIndexRecord));
  Result := FindResourceIconIndexRecord.IndexFound;
  if Result then
    IconIndex := FindResourceIconIndexRecord.IconIndex
  else
    IconIndex := 0;
end;

function TryResourceToIconIndex(const ResourceName: string; var IconIndex: Integer): Boolean;
var
  FindResourceIconIndexRecord: TFindResourceIconIndexRecord;
begin
  FindResourceIconIndexRecord.ResourceName := PChar(ResourceName);
  FindResourceIconIndexRecord.IconIndex := 0;
  FindResourceIconIndexRecord.IndexFound := False;
  EnumResourceNames(hInstance, RT_GROUP_ICON, @EnumIconGroupToIconIndexResources, NativeInt(@FindResourceIconIndexRecord));
  Result := FindResourceIconIndexRecord.IndexFound;
  if Result then
    IconIndex := FindResourceIconIndexRecord.IconIndex
  else
    IconIndex := 0;
end;

function TryImageIndexToIconDescr(const ImageIndex: Integer; var IconDescr: TIconDescr): Boolean;
begin
  Result := ImageIndex > 0;
  if Result then
    begin
      IconDescr.Options := [];
      IconDescr.BaseIcon.IconIndex := LoByte(LoWord(ImageIndex));
      IconDescr.BaseIcon.SchemaColor := HiByte(LoWord(ImageIndex)) and MaskSchemaColor;
      if IconDescr.BaseIcon.IconIndex <> 0 then Include(IconDescr.Options, idoBaseIcon);
      IconDescr.CoverIcon.IconIndex := LoByte(HiWord(ImageIndex));
      IconDescr.CoverIcon.SchemaColor := HiByte(HiWord(ImageIndex)) and MaskSchemaColor;
      if IconDescr.CoverIcon.IconIndex <> 0 then Include(IconDescr.Options, idoCoverIcon);
      IconDescr.Position := HiByte(LoWord(ImageIndex)) shr 4;
      if (idoBaseIcon in IconDescr.Options) and (IconDescr.Position <> 0) then
        Include(IconDescr.Options, idoPosition);
      if (HiWord(ImageIndex) and $4000) <> 0 then
        Include(IconDescr.Options, idoInversed);
    end;
end;

function TryIconDescrToImageIndex(const IconDescr: TIconDescr; var ImageIndex: Integer): Boolean;
begin
  ImageIndex := 0;
  if idoBaseIcon in IconDescr.Options then
    begin
      ImageIndex := ImageIndex or IconDescr.BaseIcon.IconIndex or
                                  IconDescr.BaseIcon.SchemaColor shl 8;
      if idoPosition in IconDescr.Options then
        ImageIndex := ImageIndex or IconDescr.Position shl 12;
      if idoInversed in IconDescr.Options then
        ImageIndex := ImageIndex or $40000000;
    end;
  if idoCoverIcon in IconDescr.Options then
    ImageIndex := ImageIndex or IconDescr.CoverIcon.IconIndex shl 16 or
                                IconDescr.CoverIcon.SchemaColor shl 24;
  Result := ImageIndex <> 0;
  if not Result then ImageIndex := -1;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('uIconUtils unit initialization ...');

finalization
  DebugLog('uIconUtils unit finalization ...');
{$ENDIF}

end.

