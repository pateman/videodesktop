unit ufunctions;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, IniFiles;

const
  GLOBAL_SECTION = 'Global';
  RUNONSTARTUP_VALUE = 'RunOnStartup';
  LOOPVIDEOS_VALUE = 'LoopVideos';

function GetScreenshotFileName(const VidFile: string): string;
procedure SplitString(const Delimiter: char; const Str: string;
  const ListOfStrings: TStrings);
function GetMonitorName(const Hnd: HMONITOR): string;
function GetCurrentUser(): string;
function InitializeConfig(): boolean;

var
  Config: TIniFile;

implementation

uses
  SysUtils, Forms, Multimon, ActiveX, ComObj, Variants, Dialogs, Character;

function TrimLeadingZeros(const S: string): string;
var
  I, L: integer;
begin
  L := Length(S);
  I := 1;
  while (I < L) and (S[I] = '0') do
    Inc(I);
  Result := Copy(S, I);
end;

function GetScreenshotFileName(const VidFile: string): string;
begin
  Result := IncludeTrailingBackslash(ExtractFilePath(Application.ExeName)) +
    ChangeFileExt(ExtractFileName(VidFile), '.png');
end;

function GetMonitorName(const Hnd: HMONITOR): string;
const
  WbemUser = '';
  WbemPassword = '';
  WbemComputer = 'localhost';
  wbemFlagForwardOnly = $00000020;
type
  TMonitorInfoEx = record
    cbSize: DWORD;
    rcMonitor: TRect;
    rcWork: TRect;
    dwFlags: DWORD;
    szDevice: array[0..CCHDEVICENAME - 1] of AnsiChar;
  end;
var
  DispDev: TDisplayDevice;
  monInfo: TMonitorInfoEx;
  SL: TStringList;
  FSWbemLocator: olevariant;
  FWMIService, FWMIService2: olevariant;
  FWbemObjectSet: olevariant;
  FWbemObject: olevariant;
  oEnum: IEnumvariant;
  Query: WideString;
  DeviceId: string;
  iValue: longword;
  I: integer;
begin
  Result := '';

  monInfo.cbSize := sizeof(monInfo);
  SL := TStringList.Create();
  try
    if GetMonitorInfo(Hnd, @monInfo) then
    begin
      DispDev.cb := SizeOf(DispDev);
      EnumDisplayDevices(@monInfo.szDevice, 0, @DispDev, 0);

      Result := StrPas(DispDev.DeviceString);
      SplitString('\', DispDev.DeviceID, SL);
      if ((SL.Count = 4) and (SL[0] = 'MONITOR')) then
      begin
        FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
        FWMIService := FSWbemLocator.ConnectServer(WbemComputer,
          'root\CIMV2', WbemUser, WbemPassword);
        FWMIService2 := FSWbemLocator.ConnectServer(WbemComputer,
          'root\WMI', WbemUser, WbemPassword);

        Query := 'SELECT * FROM Win32_PnPEntity WHERE ClassGUID = ''' +
          SL[2] + ''' AND DeviceID LIKE ''DISPLAY\\' + SL[1] + '\\' +
          TrimLeadingZeros(SL[3]) + '%''';

        FWbemObjectSet := FWMIService.ExecQuery(Query, 'WQL', wbemFlagForwardOnly);
        oEnum := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
        if (oEnum.Next(1, FWbemObject, iValue) = 0) then
        begin
          DeviceId := StringReplace(VarToUnicodeStr(FWbemObject.DeviceId),
            '\', '\\', [rfReplaceAll]);
          FWbemObject := Unassigned;

          Query :=
            'SELECT UserFriendlyName FROM WmiMonitorId WHERE Active = True AND InstanceName LIKE "'
            + DeviceId + '%"';

          FWbemObjectSet := FWMIService2.ExecQuery(Query, 'WQL', wbemFlagForwardOnly);
          oEnum := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
          if (oEnum.Next(1, FWbemObject, iValue) = 0) then
          begin
            Result := '';
            for I := VarArrayLowBound(FWbemObject.UserFriendlyName, 1)
              to VarArrayHighBound(FWbemObject.UserFriendlyName, 1) do
              Result := Result + TCharacter.ConvertFromUtf32(
                UCS4Char(integer(FWbemObject.UserFriendlyName[I])));

            Result := Trim(Result);
          end;
        end;
      end;
    end;
  finally
    SL.Free();
  end;
end;

procedure SplitString(const Delimiter: char; const Str: string;
  const ListOfStrings: TStrings);
begin
  ListOfStrings.Clear();

  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.StrictDelimiter := True;
  ListOfStrings.DelimitedText := Str;
end;

function GetCurrentUser(): string;
var
  nSize: DWord;
begin
  nSize := 1024;
  SetLength(Result, nSize);
  if GetUserName(PChar(Result), nSize) then
    SetLength(Result, nSize - 1)
  else
    Result := '';
end;

function InitializeConfig(): boolean;
begin
  Config := TIniFile.Create(IncludeTrailingBackslash(
    ExtractFilePath(Application.ExeName)) + 'config.ini', True);

  // Populate the configuration file with defualt values.
  Result := Config.SectionExists(GLOBAL_SECTION);
  if (not Result) then
  begin
    Config.WriteBool(GLOBAL_SECTION, RUNONSTARTUP_VALUE, True);
    Config.WriteBool(GLOBAL_SECTION, LOOPVIDEOS_VALUE, True);
  end;
end;

end.
