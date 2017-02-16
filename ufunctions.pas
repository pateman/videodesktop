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
procedure InitializeConfig();

var
  Config: TIniFile;

implementation

uses
  SysUtils, Forms, Multimon;

function GetScreenshotFileName(const VidFile: string): string;
begin
  Result := IncludeTrailingBackslash(ExtractFilePath(Application.ExeName)) +
    ChangeFileExt(ExtractFileName(VidFile), '.png');
end;

function GetMonitorName(const Hnd: HMONITOR): string;
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
begin
  Result := '';

  monInfo.cbSize := sizeof(monInfo);
  if GetMonitorInfo(Hnd, @monInfo) then
  begin
    DispDev.cb := SizeOf(DispDev);
    EnumDisplayDevices(@monInfo.szDevice, 0, @DispDev, 0);
    Result := StrPas(DispDev.DeviceString);
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

procedure InitializeConfig();
begin
  Config := TIniFile.Create(IncludeTrailingBackslash(
    ExtractFilePath(Application.ExeName)) + 'config.ini', True);

  // Populate the configuration file with defualt values.
  if (not Config.SectionExists(GLOBAL_SECTION)) then
  begin
    Config.WriteBool(GLOBAL_SECTION, RUNONSTARTUP_VALUE, True);
    Config.WriteBool(GLOBAL_SECTION, LOOPVIDEOS_VALUE, True);
  end;
end;

end.

