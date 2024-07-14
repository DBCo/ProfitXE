unit About;

interface

uses
  Windows, Messages, SysUtils, Classes, {Graphics, }Controls, Forms, Vcl.Graphics, System.StrUtils,
  StdCtrls, ExtCtrls, ComCtrls, ToolWin, LogoForm, DeTypes;

type
  _OSVERSIONINFOEX = record
    dwOSVersionInfoSize : DWORD;
    dwMajorVersion      : DWORD;
    dwMinorVersion      : DWORD;
    dwBuildNumber       : DWORD;
    dwPlatformId        : DWORD;
    szCSDVersion        : array[0..127] of AnsiChar;
    wServicePackMajor   : WORD;
    wServicePackMinor   : WORD;
    wSuiteMask          : WORD;
    wProductType        : BYTE;
    wReserved           : BYTE;
  end;
  TOSVERSIONINFOEX = _OSVERSIONINFOEX;

const
  VER_NT_WORKSTATION    : Integer = 1;

type
  TForm_Da_About = class(TFormLogo)
    Label_dL_OSVersion: TLabel;
    Label_dL_EnabledDB: TLabel;
    Label_dL_CPUFrequency: TLabel;
    Label_Dl_MemoryAll: TLabel;
    L_Dl_SystemRes: TLabel;
    Bev1: TBevel;
    SystL: TLabel;
    DBL: TLabel;
    CPUL: TLabel;
    MemL: TLabel;
    FreeL: TLabel;
    CancelBtn: TToolButton;
    Label_dL_EnabledOLAP: TLabel;
    OLAPL: TLabel;
    Label_dL_EnabledOfficePack: TLabel;
    MSO: TLabel;
    Label_dL_CPUName: TLabel;
    CPUN: TLabel;
    Label_dL_DriversDB: TLabel;
    DBD: TLabel;
    L_Dl_ComputerName: TLabel;
    ComputerL: TLabel;
    L_Dl_UserName: TLabel;
    UserNameL: TLabel;
    L_Dl_ComputerIP: TLabel;
    IPL: TLabel;
    Bevel1: TBevel;
    Label_dL_EnabledMailClient: TLabel;
    MC: TLabel;
    UpdateBtn: TToolButton;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure ImageLClick(Sender: TObject);
    procedure UpdateBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    {$IFDEF INDYUSED}
    procedure MoveControls(const Top, Height: Integer);
    {$ENDIF}
  public
    { Public declarations }
    class procedure Execute;
    procedure WMDOWNLOAD(var Msg: TMessage); message DM_DOWNLOADNOTIFY;
  end;

function GetVersionEx(var lpVersionInformationEx: TOSVERSIONINFOEX): BOOL; stdcall; overload;
  external kernel32 name 'GetVersionExA';

implementation

uses DeLog, uWindowsVersion, {$IFDEF OPENOFFICE}DeOpenOfficeReport,{$ENDIF}
  Funcs, Dictionary, ConnectOperationsUnit, DeSettings, DataUnit
  {$IFDEF INDYUSED}, uOpenSSL{$ENDIF};

{$R *.dfm}

procedure TForm_Da_About.UpdateBtnClick(Sender: TObject);
begin
  inherited;
  DM.DoUpdateApplication(True);
end;

//==============================================================================

procedure TForm_Da_About.CancelBtnClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TForm_Da_About.WMDOWNLOAD(var Msg: TMessage);
var i,t: Integer;
begin
  t:= LiteCaption.Top;

  sleep(300);
  for i := 0 to Variables.AsInteger[RegAnim] do
      begin
        LiteCaption.Top := LiteResize(t, LiteCaption.Parent.Height, Variables.AsInteger[RegAnim], i, 10);
        LiteCaption.Repaint;
      end;

  case Msg.WParam of
    auNew: LiteCaption.Font.Color := clWindowText;
    //auOld: // Можно обработать Downgrade
  else
    LiteCaption.Font.Color := clGray;
  end;
  LiteCaption.Caption := PChar(Msg.LParam);

  sleep(300);
  for i := 0 to Variables.AsInteger[RegAnim] do
      begin
        LiteCaption.Top := LiteResize(LiteCaption.Parent.Height, t, Variables.AsInteger[RegAnim], i, 10);
        LiteCaption.Repaint;
      end;

  UpdateBtn.Visible := (Msg.WParam = auNew) and (GetSystemMetrics(SM_REMOTESESSION) = 0);
end;
//==============================================================================

procedure TForm_Da_About.FormActivate(Sender: TObject);
begin
  inherited;
  Repaint;
end;

procedure TForm_Da_About.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

//==============================================================================

procedure TForm_Da_About.FormKeyPress(Sender: TObject; var Key: Char);
begin
  ModalResult:=mrOk;
end;

procedure TForm_Da_About.ImageLClick(Sender: TObject);
begin
  {$IFDEF DEBUG}
  DM.DebugDumpIconIndexes;
  {$ENDIF}
end;

//==============================================================================

procedure TForm_Da_About.FormCreate(Sender: TObject);
resourcestring
  sUnknownMHz = '<not determined>';
  sMicrosoftRemoteDesktopFmt = 'Microsoft Remote Desktop %d.%d';
var VI       : TOSVersionInfoEx;
    fn, s, x : string;
    MS       : TMemoryStatus;
    MSEX     : TMemoryStatusEx;
    lpSInfo  : TSystemInfo;

    Y,M,D    : Word;

    FileVersion: TFileVersion;

    FH       : Integer;
    MHz      : Integer;
//    InfoSize : DWORD;
    InterbaseInformation: TInterbaseInformation;
    MSSQLInformation: TMSSQLInformation;
    {$IFDEF INDYUSED}
    IndyLabel, VersionLabel: TLabel;
    OpenSSL: TOpenSSL;
    {$ENDIF}
begin
  inherited;
  {$IFDEF INDYUSED}
  IndyLabel := TLabel.Create(Self);
  IndyLabel.Parent := Self;
  IndyLabel.Left := Label_dL_EnabledOfficePack.Left;
  IndyLabel.Top := Label_dL_EnabledOfficePack.Top;
  IndyLabel.Caption := GetTitle('_dL.EnabledOpenSSL');
  MoveControls(Label_dL_EnabledOfficePack.Top, Label_dL_EnabledMailClient.Top - Label_dL_EnabledOfficePack.Top);
  {$ENDIF}
  ToolBarL.Visible := False;

  CancelBtn.ImageIndex:= DM.MapIconIndex(116);
  CancelBtn.Caption:= GetTitle('_dA.Close');

  UpdateBtn.ImageIndex:= DM.MapIconIndex(8032262);
  UpdateBtn.Caption:= GetTitle('_dA.Update');
  UpdateBtn.Visible:= False;

  LangFormUpdate(self);
  //........................................................ CPU Information ...
  MHz := GetCPUSpeed;
  if MHz <> 0 then
    CPUL.Caption := Format('%u MHz', [MHz])
  else
    CPUL.Caption := sUnknownMHz;

  // слишком длинное название процессора, сократил и причесал немного по-честному:
  // '11th Gen Intel(R) Core(TM) i3-1115G4 @ 3.00GHz'
  // '11th Gen Intel® Core™ i3-1115G4 @ 3.00GHz'
  CPUN.Caption := ReplaceText(ReplaceText(GetCPUName, '(R)', '®'), '(TM)', '™');

  try
    {$IFDEF WIN32}
    GetNativeSystemInfo(lpSInfo);
    {$ELSE}
    GetSystemInfo(lpSInfo);
    {$ENDIF}
    if (lpSInfo.dwNumberOfProcessors > 1) and (MHz <> 0) then
      CPUL.Caption := IntToStr(lpSInfo.dwNumberOfProcessors) + ' * ' + CPUL.Caption;
  except
    CPUL.Caption := sUnknownMHz;
  end;
  if lpSInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64 then
    x := '; x64'
  else
    x := EmptyStr;
  //........................................................ program version ...
  fn:=Application.ExeName;

  DecodeDate(FileDateToDateTime(FileAge(fn)), Y, M, D);
  s:= Format('%s %d', [GetTitle('_Mmmmm'+IntToStr(M)), Y]);


  if GetFileVersion(fn, FileVersion) then begin
                           s:= FileVersionToString(FileVersion, True) +
                              IIF(
                                  fvfPreRelease in FileVersion.Flags,
                                  ' beta',
                                  IIF(
                                      fvfDebug in FileVersion.Flags,
                                      ' debug',
                                      EmptyStr
                                     )
                                 )
                              + ' - ' + s;
                     end;

  NormalCaption.Caption:= GetTitle('_dL.version')+': '+s;
  //................................................................. Memory ...
  MSEX.dwLength := SizeOf(MSEX);
  if GlobalMemoryStatusEx(MSEX) then
    begin
      MemL.Caption:=FormatFloat('#,##0" Mb"', MSEX.ullTotalPhys / (1024 * 1024));
      FreeL.Caption:=Format('%d %%', [100-MSEX.dwMemoryLoad]);
    end
  else
    begin
      GlobalMemoryStatus(MS);
      MemL.Caption:=FormatFloat('#,##0" Mb"', MS.dwTotalPhys / (1024 * 1024));
      FreeL.Caption:=Format('%d %%', [100-MS.dwMemoryLoad]);
    end;
  //........................................................ Windows Version ...
  ComputerL.Caption := GetComputerNetName;
  UserNameL.Caption := GetWindowsUserName;
  s := GetHostIP(s);
  if Length(s) = 0 then s := '127.0.0.1';
  IPL.Caption := s;
  if GetSystemMetrics(SM_REMOTESESSION) = 0 then
    SystL.Caption := GetWindowsVersion
  else
    begin
      VI.dwOSVersionInfoSize := SizeOf(TOSVersionInfoEx);
      GetVersionEx(VI);
      SystL.Caption := Format(sMicrosoftRemoteDesktopFmt, [VI.dwMajorVersion, VI.dwMinorVersion]);
      s := GetWTSDomainName;
      if Length(s) <> 0 then
        ComputerL.Caption := s + '\' + ComputerL.Caption;
      s := GetWTSClientName;
      if Length(s) <> 0 then
        ComputerL.Caption := ComputerL.Caption + ' [' + s + ']';
      s := GetWTSClientIP;
      if Length(s) <> 0 then
        begin
          IPL.Caption := IPL.Caption + ' [' + s + ']';
          L_Dl_ComputerIP.Caption := L_Dl_ComputerIP.Caption + ' [RDP]';
        end;
    end;
  //................................. Create Logo BitMap, User and Directory ...

  BoldCaption.Caption := dicAppFullName;
  //............................................................. Enabled DB ...
  s := EmptyStr;
 // FH:=GetBDEVersion; if FH>0 then StrAdd(s,'; ','BDE '+IntToStr(FH div 100)+'.'+IntToStr(FH mod 100));
  if GetMSSQLInformation(MSSQLInformation) then
    StrAdd(s, '; ', MSSQLInformation.ProductName);
  if GetInterbaseInformation(InterbaseInformation) then
    StrAdd(s, '; ', InterbaseInformation.ProductName);
  FH := GetOracleVersion;
  if FH <> 0 then
    StrAdd(s, '; ', Format('Oracle %d.%d', [HiWord(FH), LoWord(FH)]));
  FH := GetMicrosoftAccessVersion;
  if FH <> 0 then
    StrAdd(s, '; ', MicrosoftOfficeVersionToString('Access', FH));

  if Length(s) = 0 then DBL.Caption := '-'
                   else DBL.Caption := s;

  //.............................................................. Driver DB ...
  s := EmptyStr;
  FH := GetMDACVersion;
  if FH > 0 then
    StrAdd(s, '; ', Format('MDAC %d.%d', [HiWord(FH), LoWord(FH)]));
  FH := GetNativeClientVersion;
  if FH <> 0 then
    StrAdd(s, '; ', Format('SQLNCLI %d.%d', [HiWord(FH), LoWord(FH)]));

  if Length(s) = 0 then DBD.Caption := '-'
                   else DBD.Caption := s;

  //........................................................... OLAP Version ...
  s := EmptyStr;
  FH := GetMSOLAPVersion;
  if FH <> 0 then
    StrAdd(s, '; ', Format('MSOLAP %d.%d', [HiWord(FH), LoWord(FH)]));
  FH := GetMicrosoftChartVersion;
  if FH <> 0 then
    StrAdd(s, '; ', MicrosoftOfficeVersionToString('Chart', FH));
  if Length(s) = 0 then OLAPL.Caption := '-'
                   else OLAPL.Caption := s;

  //............................................................... Open SSL ...
  {$IFDEF INDYUSED}
  s := EmptyStr;
  VersionLabel := TLabel.Create(Self);
  VersionLabel.Parent := Self;
  VersionLabel.Left := MSO.Left;
  VersionLabel.Top := IndyLabel.Top;
  VersionLabel.AutoSize := False;
  VersionLabel.Width := MSO.Width;
  VersionLabel.Height := MSO.Height;
  VersionLabel.Alignment := taRightJustify;
  try
    OpenSSL := TOpenSSL.Create;
    try
      s := OpenSSL.ProductVersion;
    finally
      OpenSSL.Free;
    end;
  except
    { Ничего не делаем! }
  end;
  if Length(s) = 0 then
    VersionLabel.Caption := '-'
  else
    VersionLabel.Caption := s;
  {$ENDIF}

  //......................................................... Office Version ...
  s := EmptyStr;
  FH := GetMicrosoftWordVersion;
  if FH <> 0 then
    StrAdd(s, '; ', MicrosoftOfficeVersionToString('Word', FH));
  FH := GetMicrosoftExcelVersion;
  if FH <> 0 then
    StrAdd(s, '; ', MicrosoftOfficeVersionToString('Excel', FH));
  {$IFDEF OPENOFFICE}
  FH := GetOpenOfficeWriterVersion;
  if FH <> 0 then
    StrAdd(s, '; ', Format('Writer %d.%d', [HiWord(FH), LoWord(FH)]));
  FH := GetOpenOfficeCalcVersion;
  if FH <> 0 then
    StrAdd(s, '; ', Format('Calc %d.%d', [HiWord(FH), LoWord(FH)]));
  {$ENDIF}
  if Length(s) = 0 then MSO.Caption := '-'
                   else MSO.Caption := s;

  s := EmptyStr;
  FH := GetMicrosoftOutlookVersion;
  if FH <> 0 then
    StrAdd(s, '; ', MicrosoftOfficeVersionToString('Outlook', FH));
  if Length(s) = 0 then MC.Caption := '-'
                   else MC.Caption := s;

  LiteCaption.Caption:= EmptyStr;

  // 32-х битная версия больше не поддерживается и
  // соответственно отключаем для неё автоматическое обновление!
  {$IFDEF WIN64}
  DM.GetUpdateInfo(Handle);
  {$ENDIF}
end;

class procedure TForm_Da_About.Execute;
begin
  with Self.Create(Application) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

{$IFDEF INDYUSED}
procedure TForm_Da_About.MoveControls(const Top, Height: Integer);
var
  Index: Integer;
begin
  for Index := Pred(ControlCount) downto 0 do
    if Controls[Index].Align = alNone then
      if Controls[Index].Top < Top then
        Controls[Index].Top := Controls[Index].Top - Height;
  Self.ClientHeight := Self.ClientHeight + Height;
end;
{$ENDIF}

{$IFDEF DEBUG}
initialization
  DebugLog('About unit initialization ...');

finalization
  DebugLog('About unit finalization ...');
{$ENDIF}

end.

