unit uWindowsVersion;

interface

uses Windows, SysUtils, Registry, Classes;

type
  TOSVersionInfoEx = packed record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformID: DWORD;
    szCSDVersion: array[0..127] of AnsiChar; { Maintenance string for PSS usage }
    wServicePackMajor: Word;
    wServicePackMinor: Word;
    wSuiteMask: Word;
    wProductType: Byte;
    wReserved : Byte;
  end;

const
  VER_SUITE_ENTERPRISE = $00000002;
  VER_SUITE_EMBEDDEDNT = $00000040;
  VER_SUITE_DATACENTER = $00000080;
  VER_SUITE_PERSONAL = $00000200;
  VER_SUITE_BLADE = $00000400;
  VER_SUITE_STORAGE_SERVER = $00002000;
  VER_SUITE_COMPUTE_SERVER = $00004000;
  VER_SUITE_WH_SERVER = $00008000;

  VER_NT_WORKSTATION = $0000001;
  VER_NT_DOMAIN_CONTROLLER = $0000002;
  VER_NT_SERVER = $0000003;

  PROCESSOR_ARCHITECTURE_INTEL = 0;
  PROCESSOR_ARCHITECTURE_IA64 = 6;
  PROCESSOR_ARCHITECTURE_AMD64 = 9;

  SM_MEDIACENTER = 87;
  SM_STARTER = 88;
  SM_SERVERR2 = 89;

  cWinNT = 'WINNT';

  PRODUCT_UNDEFINED = $00000000;
  PRODUCT_UNLICENSED = $ABCDABCD;
  PRODUCT_ULTIMATE = $00000001; // Ultimate
  PRODUCT_HOME_BASIC = $00000002; // Home Basic
  PRODUCT_HOME_PREMIUM = $00000003; // Home Premium
  PRODUCT_ENTERPRISE = $00000004; // Enterprise
  PRODUCT_HOME_BASIC_N = $00000005; // Home Basic N
  PRODUCT_BUSINESS = $00000006; // Business
  PRODUCT_STANDARD_SERVER = $00000007; // Server Standard (full installation)
  PRODUCT_DATACENTER_SERVER = $00000008; // Server Datacenter (full installation)
  PRODUCT_SMALLBUSINESS_SERVER = $00000009; // Windows Small Business Server
  PRODUCT_ENTERPRISE_SERVER = $0000000A; // Server Enterprise (full installation)
  PRODUCT_STARTER = $0000000B; // Starter
  PRODUCT_DATACENTER_SERVER_CORE = $0000000C; // Server Datacenter (core installation)
  PRODUCT_STANDARD_SERVER_CORE = $0000000D; // Server Standard (core installation)
  PRODUCT_ENTERPRISE_SERVER_CORE = $0000000E; // Server Enterprise (core installation)
  PRODUCT_ENTERPRISE_SERVER_IA64 = $0000000F; // Server Enterprise for Itanium-based Systems
  PRODUCT_BUSINESS_N = $00000010; // Business N
  PRODUCT_WEB_SERVER = $00000011; // Web Server (full installation)
  PRODUCT_CLUSTER_SERVER = $00000012; // HPC Edition
  PRODUCT_STORAGE_EXPRESS_SERVER = $00000014; // Storage Server Express
  PRODUCT_STORAGE_STANDARD_SERVER = $00000015; // Storage Server Standard
  PRODUCT_STORAGE_WORKGROUP_SERVER = $00000016; // Storage Server Workgroup
  PRODUCT_STORAGE_ENTERPRISE_SERVER = $00000017; // Storage Server Enterprise
  PRODUCT_SERVER_FOR_SMALLBUSINESS = $00000018; // Windows Server 2008 for Windows Essential Server Solutions
  PRODUCT_SMALLBUSINESS_SERVER_PREMIUM = $00000019;
  PRODUCT_HOME_PREMIUM_N = $0000001A; // Home Premium N
  PRODUCT_ENTERPRISE_N = $0000001B; // Enterprise N
  PRODUCT_ULTIMATE_N = $0000001C; // Ultimate N
  PRODUCT_WEB_SERVER_CORE = $0000001D; // Web Server (core installation)
  PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT = $0000001E; // Windows Essential Business Server Management Server
  PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY = $0000001F; // Windows Essential Business Server Security Server
  PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING = $00000020; // Windows Essential Business Server Messaging Server
  PRODUCT_SERVER_FOUNDATION = $00000021; // Server Foundation
  PRODUCT_SERVER_FOR_SMALLBUSINESS_V = $00000023; // Windows Server 2008 without Hyper-V for Windows Essential Server Solutions
  PRODUCT_STANDARD_SERVER_V = $00000024; // Server Standard without Hyper-V (full installation)
  PRODUCT_DATACENTER_SERVER_V = $00000025; // Server Datacenter without Hyper-V (full installation)
  PRODUCT_ENTERPRISE_SERVER_V = $00000026; // Server Enterprise without Hyper-V (full installation)
  PRODUCT_DATACENTER_SERVER_CORE_V = $00000027; // Server Datacenter without Hyper-V (core installation)
  PRODUCT_STANDARD_SERVER_CORE_V = $00000028; // Server Standard without Hyper-V (core installation)
  PRODUCT_ENTERPRISE_SERVER_CORE_V = $00000029; // Server Enterprise without Hyper-V (core installation)
  PRODUCT_HYPERV = $0000002A; // Microsoft Hyper-V Server
  PRODUCT_STARTER_N = $0000002F; // Starter N
  PRODUCT_PROFESSIONAL = $00000030; // Professional
  PRODUCT_PROFESSIONAL_N = $00000031; // Professional N
  PRODUCT_STARTER_E = $00000042; // Starter E
  PRODUCT_HOME_BASIC_E = $00000043; // Home Basic E
  PRODUCT_HOME_PREMIUM_E = $00000044; // Home Premium E
  PRODUCT_PROFESSIONAL_E = $00000045; // Professional E
  PRODUCT_ENTERPRISE_E = $00000046; // Enterprise E
  PRODUCT_ULTIMATE_E = $00000047; // Ultimate E

resourcestring
  sMicrosoft = 'Microsoft ';
  sWindowsNT = 'Windows NT %d.%d %s';
  sWindows2000 = 'Windows 2000 %s';
  sWindowsXP = 'Windows XP %s';
  sWindows2003 = 'Windows Server 2003';
  sWindows2003R2 = 'Windows Server 2003 R2';
  sWindowsVista = 'Vista %s';
  sWindows2008 = 'Windows Server 2008 %s';
  sWindows2008R2 = 'Windows Server 2008 R2 %s';
  sWindows7 = 'Windows 7 %s';
  sWindowsHomeServer = 'Windows Home Server';
  sWindowsStorageServer = 'Windows Storage Server';
  sWindows8 = 'Windows 8 %s';
  sWindows2012 = 'Windows Server 2012 %s';
  sWindows81 = 'Windows 8.1 %s';
  sWindows2012R2 = 'Windows Server 2012 R2 %s';
  sWindows10 = 'Windows 10 %s';
  sWindows2016 = 'Windows Server 2016 %s';

  sComputeClusterEdition = ' Compute Cluster Edition';
  sDataCenterEdition = ' Datacenter Edition';
  sEnterpriseEdition = ' Enterprise Edition';
  sWebEdition = ' Web Edition';
  sStandardEdition = ' Standard Edition';

  sDataCenterEdition64 = ' Datacenter x64 Edition';
  sEnterpriseEdition64 = ' Enterprise x64 Edition';
  sStandardEdition64 = ' Standard x64 Edition';

  sStarterEdition = 'Starter Edition';
  sMediaCenterEdition = 'Media Center Edition';
  sEmbeddedEdition = 'Embedded Edition';

  sItanium = ' for Itanium-based Systems';
  sEdition64 = ' x64 Edition';
  sCoreInstallation = ' (core installation)';

  sHomeEdition = 'Home Edition';
  sProfessional = 'Professional';
  sDatacenterServer = 'Datacenter Server';
  sAdvancedServer = 'Advanced Server';
  sWorkstation = 'Workstation';
  sServer = 'Server';
  sMillenniumEdition = 'Millennium Edition';

  sWindows95 = 'Windows 95';
  sWindows95OSR1 = 'Windows 95 OSR';
  sWindows95OSR2 = 'Windows 95 OSR2';
  sWindows98 = 'Windows 98';
  sWindows98SE = 'Windows 98 Second Edition';
  sWindows9X = 'Windows 9X';

  sUltimateEdition = 'Ultimate Edition';
  sHomePremiumEdition = 'Home Premium Edition';
  sHomeBasicEdition = 'Home Basic Edition';
  sBusinessEdition = 'Business Edition';
  sClusterServerEdition = 'Cluster Server Edition';
  sSmallBusinessServer = 'Small Business Server';
  sSmallBusinessServerPremiumEdition = 'Small Business Server Premium Edition';
  sWebServerEdition = 'Web Server Edition';

  sProcessor32 = ', 32-bit';
  sProcessor64 = ', 64-bit';

function GetVersionEx(var VersionInformation: TOSVersionInfoEx): Boolean;
procedure GetSystemInfo(var SystemInfo: TSystemInfo);
function GetProductInfo(dwOSMajorVersion, dwOSMinorVersion, dwSPMajorVersion, dwSPMinorVersion: DWORD; var dwProductType: DWORD): Boolean;

function GetRegistryProductType: string;
function GetRegistryProductTypeName: string;

function GetProductInfoName(const dwProductType: DWORD): string;
function GetWindowsVersion: string;

function GetAgentVersion: string;

implementation

function InternalGetVersionEx(lpVersionInformation: Pointer): BOOL; stdcall; external kernel32 name 'GetVersionExA';

function GetVersionEx(var VersionInformation: TOSVersionInfoEx): Boolean;
begin
  ZeroMemory(@VersionInformation, SizeOf(VersionInformation));
  VersionInformation.dwOSVersionInfoSize := SizeOf(VersionInformation);
  Result := InternalGetVersionEx(@VersionInformation);
  if not Result then
    begin
      VersionInformation.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
      Result := InternalGetVersionEx(@VersionInformation);
    end;
end;

procedure GetSystemInfo(var SystemInfo: TSystemInfo);
var
  Proc: procedure(var SystemInfo: TSystemInfo); stdcall;
begin
  ZeroMemory(@SystemInfo, SizeOf(SystemInfo));
  Proc := GetProcAddress(GetModuleHandle(Kernel32), 'GetNativeSystemInfo');
  if Assigned(Proc) then
    Proc(SystemInfo)
  else
    Windows.GetSystemInfo(SystemInfo);
end;

function GetProductInfo(dwOSMajorVersion, dwOSMinorVersion, dwSPMajorVersion, dwSPMinorVersion: DWORD; var dwProductType: DWORD): Boolean;
var
  Proc: function(dwOSMajorVersion, dwOSMinorVersion, dwSPMajorVersion, dwSPMinorVersion: DWORD; var dwProductType: DWORD): BOOL; stdcall;
begin
  Proc := GetProcAddress(GetModuleHandle(Kernel32), 'GetProductInfo');
  if Assigned(Proc) then
    Result := Proc(dwOSMajorVersion, dwOSMinorVersion, dwSPMajorVersion, dwSPMinorVersion, dwProductType)
  else
    begin
      dwProductType := PRODUCT_UNDEFINED;
      Result := True;
    end;
end;

function GetRegistryProductType: string;
const
  cProductSection = '\SYSTEM\CurrentControlSet\Control\ProductOptions';
  cProductTypeIdent = 'ProductType';
begin
  with TRegIniFile.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(cProductSection) then
        Result := Trim(ReadString('', cProductTypeIdent, ''))
      else
        Result := '';
    finally
      Free;
    end;
end;

function GetRegistryProductTypeName: string;
begin
  Result := GetRegistryProductType;
  if AnsiCompareText(cWinNT, Result) = 0 then
    Result := sProfessional
  else if AnsiCompareText('LANMANNT', Result) = 0 then
    Result := sServer
  else if AnsiCompareText('SERVERNT', Result) = 0 then
    Result := sAdvancedServer
  else
    Result := '';
end;

function GetProductInfoName(const dwProductType: DWORD): string;
begin
  case dwProductType of
    PRODUCT_ULTIMATE:
      Result := sUltimateEdition;
    PRODUCT_PROFESSIONAL:
      Result := sProfessional;
    PRODUCT_HOME_PREMIUM:
      Result := sHomePremiumEdition;
    PRODUCT_HOME_BASIC:
      Result := sHomeBasicEdition;
    PRODUCT_ENTERPRISE:
      Result := Trim(sEnterpriseEdition);
    PRODUCT_BUSINESS:
      Result := sBusinessEdition;
    PRODUCT_STARTER:
      Result := sStarterEdition;
    PRODUCT_CLUSTER_SERVER:
      Result := sClusterServerEdition;
    PRODUCT_DATACENTER_SERVER:
      Result := Trim(sDatacenterEdition);
    PRODUCT_DATACENTER_SERVER_CORE:
      Result := Trim(sDatacenterEdition) + sCoreInstallation;
    PRODUCT_ENTERPRISE_SERVER:
      Result := Trim(sEnterpriseEdition);
    PRODUCT_ENTERPRISE_SERVER_CORE:
      Result := Trim(sEnterpriseEdition) + sCoreInstallation;
    PRODUCT_ENTERPRISE_SERVER_IA64:
      Result := Trim(sEnterpriseEdition) + sItanium;
    PRODUCT_SMALLBUSINESS_SERVER:
      Result := sSmallBusinessServer;
    PRODUCT_SMALLBUSINESS_SERVER_PREMIUM:
      Result := sSmallBusinessServerPremiumEdition;
    PRODUCT_STANDARD_SERVER:
      Result := Trim(sStandardEdition);
    PRODUCT_STANDARD_SERVER_CORE:
      Result := Trim(sStandardEdition) + sCoreInstallation;
    PRODUCT_WEB_SERVER:
      Result := sWebServerEdition;
  else
    Result := '';
  end;
end;

function GetWindowsVersion: string;
var
  VersionInfo: TOSVersionInfoEx;
  SystemInfo: TSystemInfo;
  ProductType: DWORD;
  function GetNameNT: string;
  begin
    if (AnsiCompareText(GetRegistryProductType, cWinNT) = 0) and (VersionInfo.wProductType = VER_NT_WORKSTATION) then
      Result := sWorkstation
    else
      Result := GetRegistryProductTypeName;
  end;
  function GetReleaseWindows95: string;
  begin
    Result := StrPas(VersionInfo.szCSDVersion);
    if Length(Result) <> 0 then
      if CharInSet(Result[1], ['C', 'B']) then
        Result := sWindows95OSR2
      else if CharInSet(Result[1], ['A', 'W']) then
        Result := sWindows95OSR1
      else
        Result := sWindows95
    else
      Result := '';
  end;
  function GetReleaseWindows98: string;
  begin
    Result := StrPas(VersionInfo.szCSDVersion);
    if (Length(Result) <> 0) and (Result[1] = 'A') then
      Result := sWindows98SE
    else
      Result := sWindows98;
  end;
begin
  Result := '';
  if GetVersionEx(VersionInfo) then
    case VersionInfo.dwPlatformID of
      VER_PLATFORM_WIN32_NT: { Семейство Windows NT }
        begin
          Result := sMicrosoft;
          if VersionInfo.dwMajorVersion <= 4 then
            Result := Result + Trim(Trim(Format(sWindowsNT,
             [VersionInfo.dwMajorVersion, VersionInfo.dwMinorVersion, GetNameNT])) + ' ' + String(VersionInfo.szCSDVersion))
          else if VersionInfo.dwMajorVersion = 5 then
            case VersionInfo.dwMinorVersion of
              0: { Windows 2000 }
                if VersionInfo.wProductType = VER_NT_WORKSTATION then
                  Result := Result + Trim(Trim(Format(sWindows2000, [sProfessional])) + ' ' + String(VersionInfo.szCSDVersion))
                else if (VersionInfo.wSuiteMask and VER_SUITE_DATACENTER) <> 0 then
                  Result := Result + Trim(Trim(Format(sWindows2000, [sDatacenterServer])) + ' ' + String(VersionInfo.szCSDVersion))
                else if (VersionInfo.wSuiteMask and VER_SUITE_ENTERPRISE) <> 0 then
                  Result := Result + Trim(Trim(Format(sWindows2000, [sAdvancedServer])) + ' ' + String(VersionInfo.szCSDVersion))
                else
                  Result := Result + Trim(Trim(Format(sWindows2000, [sServer])) + ' ' + String(VersionInfo.szCSDVersion));
              1: { Windows XP }
                if (VersionInfo.wSuiteMask and VER_SUITE_PERSONAL) <> 0 then
                  Result := Result + Trim(Format(sWindowsXP, [sHomeEdition]))
                else if GetSystemMetrics(SM_STARTER) <> 0 then
                  Result := Result + Trim(Format(sWindowsXP, [sStarterEdition]))
                else if GetSystemMetrics(SM_MEDIACENTER) <> 0 then
                  Result := Result + Trim(Format(sWindowsXP, [sMediaCenterEdition]))
                else if (VersionInfo.wSuiteMask and VER_SUITE_EMBEDDEDNT) <> 0 then
                  Result := Result + Trim(Format(sWindowsXP, [sEmbeddedEdition]))
                else
                  Result := Result + Trim(Format(sWindowsXP, [sProfessional]));
              2: { Windows XP x64 or Windows 2003 }
                if GetSystemMetrics(SM_SERVERR2) <> 0 then
                  begin
                    Result := Result + sWindows2003R2;
                    GetSystemInfo(SystemInfo);
                    case SystemInfo.wProcessorArchitecture of
                      PROCESSOR_ARCHITECTURE_INTEL: { x32 }
                        if (VersionInfo.wSuiteMask and VER_SUITE_COMPUTE_SERVER) <> 0 then
                          Result := Result + sComputeClusterEdition
                        else if (VersionInfo.wSuiteMask and VER_SUITE_DATACENTER) <> 0 then
                          Result := Result + sDataCenterEdition
                        else if (VersionInfo.wSuiteMask and VER_SUITE_ENTERPRISE) <> 0 then
                          Result := Result + sEnterpriseEdition
                        else if (VersionInfo.wSuiteMask and VER_SUITE_BLADE) <> 0 then
                          Result := Result + sWebEdition
                        else
                          Result := Result + sStandardEdition;
                      PROCESSOR_ARCHITECTURE_AMD64: { x64 }
                        if (VersionInfo.wSuiteMask and VER_SUITE_DATACENTER) <> 0 then
                          Result := Result + sDataCenterEdition64
                        else if (VersionInfo.wSuiteMask and VER_SUITE_ENTERPRISE) <> 0 then
                          Result := Result + sEnterpriseEdition64
                        else
                          Result := Result + sStandardEdition64;
                      PROCESSOR_ARCHITECTURE_IA64: { Itanium }
                        if (VersionInfo.wSuiteMask and VER_SUITE_DATACENTER) <> 0 then
                          Result := Result + sDataCenterEdition + sItanium
                        else if (VersionInfo.wSuiteMask and VER_SUITE_ENTERPRISE) <> 0 then
                           Result := Result + sEnterpriseEdition + sItanium;
                    end;
                  end
                else if (VersionInfo.wSuiteMask and VER_SUITE_STORAGE_SERVER) <> 0 then
                  Result := Result + sWindowsStorageServer
                else if (VersionInfo.wSuiteMask and VER_SUITE_WH_SERVER) <> 0 then
                  Result := Result + sWindowsHomeServer
                else
                  begin
                    GetSystemInfo(SystemInfo);
                    if (VersionInfo.wProductType = VER_NT_WORKSTATION) and (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
                      Result := Result + Format(sWindowsXP, [sProfessional + sEdition64])
                    else
                      if VersionInfo.wProductType <> VER_NT_WORKSTATION then
                        begin
                          Result := Result + sWindows2003;
                          case SystemInfo.wProcessorArchitecture of
                            PROCESSOR_ARCHITECTURE_INTEL: { x32 }
                              if (VersionInfo.wSuiteMask and VER_SUITE_COMPUTE_SERVER) <> 0 then
                                Result := Result + sComputeClusterEdition
                              else if (VersionInfo.wSuiteMask and VER_SUITE_DATACENTER) <> 0 then
                                Result := Result + sDataCenterEdition
                              else if (VersionInfo.wSuiteMask and VER_SUITE_ENTERPRISE) <> 0 then
                                Result := Result + sEnterpriseEdition
                              else if (VersionInfo.wSuiteMask and VER_SUITE_BLADE) <> 0 then
                                Result := Result + sWebEdition
                              else
                                Result := Result + sStandardEdition;
                            PROCESSOR_ARCHITECTURE_AMD64: { x64 }
                              if (VersionInfo.wSuiteMask and VER_SUITE_DATACENTER) <> 0 then
                                Result := Result + sDataCenterEdition64
                              else if (VersionInfo.wSuiteMask and VER_SUITE_ENTERPRISE) <> 0 then
                                Result := Result + sEnterpriseEdition64
                              else
                                Result := Result + sStandardEdition64;
                            PROCESSOR_ARCHITECTURE_IA64: { Itanium }
                              if (VersionInfo.wSuiteMask and VER_SUITE_DATACENTER) <> 0 then
                                Result := Result + sDataCenterEdition + sItanium
                              else if (VersionInfo.wSuiteMask and VER_SUITE_ENTERPRISE) <> 0 then
                                Result := Result + sEnterpriseEdition + sItanium;
                          end;
                        end;
                  end;
            end
          else if VersionInfo.dwMajorVersion = 6 then
            begin
              GetSystemInfo(SystemInfo);
              if not GetProductInfo(VersionInfo.dwMajorVersion, VersionInfo.dwMinorVersion, 0, 0, ProductType) then
                ProductType := PRODUCT_UNDEFINED;
              case VersionInfo.dwMinorVersion of
                0: { Vista or Server 2008 }
                  if VersionInfo.wProductType = VER_NT_WORKSTATION then
                    Result := Result + Format(sWindowsVista, [GetProductInfoName(ProductType)])
                  else
                    Result := Result + Format(sWindows2008, [GetProductInfoName(ProductType)]);
                1: { Windows 7 or Server 2008 R2 }
                  if VersionInfo.wProductType = VER_NT_WORKSTATION then
                    Result := Result + Format(sWindows7, [GetProductInfoName(ProductType)])
                  else
                    Result := Result + Format(sWindows2008R2, [GetProductInfoName(ProductType)]);
                2: { Windows 8 or Server 2012 }
                  if VersionInfo.wProductType = VER_NT_WORKSTATION then
                    Result := Result + Format(sWindows8, [GetProductInfoName(ProductType)])
                  else
                    Result := Result + Format(sWindows2012, [GetProductInfoName(ProductType)]);
                3: { Windows 8.1 or Server 2012 R2 }
                  if VersionInfo.wProductType = VER_NT_WORKSTATION then
                    Result := Result + Format(sWindows81, [GetProductInfoName(ProductType)])
                  else
                    Result := Result + Format(sWindows2012R2, [GetProductInfoName(ProductType)]);
              end;
              case SystemInfo.wProcessorArchitecture of
                PROCESSOR_ARCHITECTURE_INTEL: { x32 }
                  Result := Result + sProcessor32;
                PROCESSOR_ARCHITECTURE_AMD64: { x64 }
                  Result := Result + sProcessor64;
              end;
            end
          else if VersionInfo.dwMajorVersion = 10 then
            begin
              GetSystemInfo(SystemInfo);
              if not GetProductInfo(VersionInfo.dwMajorVersion, VersionInfo.dwMinorVersion, 0, 0, ProductType) then
                ProductType := PRODUCT_UNDEFINED;
              case VersionInfo.dwMinorVersion of
                0: { Windows 10 or Server 2016 }
                  if VersionInfo.wProductType = VER_NT_WORKSTATION then
                    Result := Result + Format(sWindows10, [GetProductInfoName(ProductType)])
                  else
                    Result := Result + Format(sWindows2016, [GetProductInfoName(ProductType)]);
              end;
              case SystemInfo.wProcessorArchitecture of
                PROCESSOR_ARCHITECTURE_INTEL: { x32 }
                  Result := Result + sProcessor32;
                PROCESSOR_ARCHITECTURE_AMD64: { x64 }
                  Result := Result + sProcessor64;
              end;
            end;
        end;
      VER_PLATFORM_WIN32_WINDOWS: { Семейство Windows 9X }
        if VersionInfo.dwMajorVersion = 4 then
          begin
            Result := sMicrosoft;
            case VersionInfo.dwMinorVersion of
              0: { Windows 95 }
                Result := Result + GetReleaseWindows95;
              10: { Windows 98 }
                Result := Result + GetReleaseWindows98;
              90: { Windows ME }
                Result := Result + sMillenniumEdition;
            else
              Result := Result + sWindows9X;
            end;
          end;
    end;
  Result := Trim(Result);
end;

function GetAgentVersion: string;
var
  VersionInfo: TOSVersionInfoEx;
  SystemInfo: TSystemInfo;
begin
  Result := '';
  if GetVersionEx(VersionInfo) then
    begin
      Result := 'Win';
      if VersionInfo.dwPlatformID = VER_PLATFORM_WIN32_NT then
        Result := Result + 'NT';
      Result := Format('%s %u.%u', [Result, VersionInfo.dwMajorVersion, VersionInfo.dwMinorVersion]);
      GetSystemInfo(SystemInfo);
      case SystemInfo.wProcessorArchitecture of
        PROCESSOR_ARCHITECTURE_INTEL: { x32 }
          Result := Result + '; x86';
        PROCESSOR_ARCHITECTURE_AMD64: { x64 }
          Result := Result + '; x64';
      end;
    end;
  if Length(Result) <> 0 then Result := ' (' + Result + ')';
end;

end.

