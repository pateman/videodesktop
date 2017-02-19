unit uwinapi;

{$mode objfpc}{$H+}

interface

uses
  Windows;

const
  PW_CLIENTONLY = $00000001;
  INTERNET_MAX_PATH_LENGTH = 2048;
  INTERNET_MAX_SCHEME_LENGTH = 32;
  INTERNET_MAX_URL_LENGTH =
    INTERNET_MAX_SCHEME_LENGTH + Length('://') + 1 + INTERNET_MAX_PATH_LENGTH;
  AD_APPLY_SAVE = $00000001;
  AD_APPLY_HTMLGEN = $00000002;
  AD_APPLY_REFRESH = $00000004;
  AD_APPLY_ALL = AD_APPLY_SAVE or AD_APPLY_HTMLGEN or AD_APPLY_REFRESH;
  AD_APPLY_FORCE = $00000008;
  AD_APPLY_BUFFERED_REFRESH = $00000010;
  AD_APPLY_DYNAMICREFRESH = $00000020;

type
  PCompPos = ^TCompPos;

  _tagCOMPPOS = record
    dwSize: DWORD;
    iLeft: integer;
    iTop: integer;
    dwWidth: DWORD;
    dwHeight: DWORD;
    izIndex: integer;
    fCanResize: BOOL;
    fCanResizeX: BOOL;
    fCanResizeY: BOOL;
    iPreferredLeftPercent: integer;
    iPreferredTopPercent: integer;
  end;
  COMPPOS = _tagCOMPPOS;
  TCompPos = _tagCOMPPOS;

  PCompStateInfo = ^TCompStateInfo;

  _tagCOMPSTATEINFO = record
    dwSize: DWORD;
    iLeft: integer;
    iTop: integer;
    dwWidth: DWORD;
    dwHeight: DWORD;
    dwItemState: DWORD;
  end;
  COMPSTATEINFO = _tagCOMPSTATEINFO;
  TCompStateInfo = _tagCOMPSTATEINFO;

  PWallPaperOpt = ^TWallPaperOpt;

  _tagWALLPAPEROPT = record
    dwSize: DWORD;
    dwStyle: DWORD;
  end;
  WALLPAPEROPT = _tagWALLPAPEROPT;
  TWallPaperOpt = _tagWALLPAPEROPT;

  PComponentsOpt = ^TComponentsOpt;

  _tagCOMPONENTSOPT = record
    dwSize: DWORD;
    fEnableComponents: BOOL;
    fActiveDesktop: BOOL;
  end;
  COMPONENTSOPT = _tagCOMPONENTSOPT;
  TComponentsOpt = _tagCOMPONENTSOPT;

  PComponent = ^COMPONENT;

  _tagCOMPONENT = record
    dwSize: DWORD;
    dwID: DWORD;
    iComponentType: integer;
    fChecked: BOOL;
    fDirty: BOOL;
    fNoScroll: BOOL;
    cpPos: COMPPOS;
    wszFriendlyName: array[0..MAX_PATH - 1] of widechar;
    wszSource: array[0..INTERNET_MAX_URL_LENGTH - 1] of widechar;
    wszSubscribedURL: array[0..INTERNET_MAX_URL_LENGTH - 1] of widechar;
    dwCurItemState: DWORD;
    csiOriginal: TCompStateInfo;
    csiRestored: TCompStateInfo;
  end;
  COMPONENT = _tagCOMPONENT;

  IActiveDesktop = interface(IUnknown)
    ['{F490EB00-1240-11D1-9888-006097DEACF9}']
    function ApplyChanges(dwFlags: DWORD): HResult; stdcall;
    function GetWallpaper(pwszWallpaper: PWideChar; cchWallpaper: UINT;
      dwReserved: DWORD): HResult; stdcall;
    function SetWallpaper(pwszWallpaper: PWideChar;
      dwReserved: DWORD): HResult; stdcall;
    function GetWallpaperOptions(out pwpo: TWallPaperOpt;
      dwReserved: DWORD): HResult; stdcall;
    function SetWallpaperOptions(const pwpo: TWallPaperOpt;
      dwReserved: DWORD): HResult; stdcall;
    function GetPattern(pwszPattern: PWideChar; cchPattern: UINT;
      dwReserved: DWORD): HResult; stdcall;
    function SetPattern(pwszPattern: PWideChar;
      dwReserved: DWORD): HResult; stdcall;
    function GetDesktopItemOptions(out pco: TComponentsOpt;
      dwReserved: DWORD): HResult; stdcall;
    function SetDesktopItemOptions(const pco: TComponentsOpt;
      dwReserved: DWORD): HResult; stdcall;
    function AddDesktopItem(var pcomp: COMPONENT;
      dwReserved: DWORD): HResult; stdcall;
    function AddDesktopItemWithUI(hwnd: HWND; var pcomp: COMPONENT;
      dwReserved: DWORD): HResult; stdcall;
    function ModifyDesktopItem(var pcomp: COMPONENT;
      dwFlags: DWORD): HResult; stdcall;
    function RemoveDesktopItem(var pcomp: COMPONENT;
      dwReserved: DWORD): HResult; stdcall;
    function GetDesktopItemCount(out lpiCount: integer;
      dwReserved: DWORD): HResult; stdcall;
    function GetDesktopItem(nComponent: integer; var pcomp: COMPONENT;
      dwReserved: DWORD): HResult; stdcall;
    function GetDesktopItemByID(dwID: ULONG; var pcomp: COMPONENT;
      dwReserved: DWORD): HResult; stdcall;
    function GenerateDesktopItemHtml(pwszFileName: PWideChar;
      var pcomp: COMPONENT; dwReserved: DWORD): HResult; stdcall;
    function AddUrl(hwnd: HWND; pszSource: PWideChar; var pcomp: COMPONENT;
      dwFlags: DWORD): HResult; stdcall;
    function GetDesktopItemBySource(pwszSource: PWideChar;
      var pcomp: COMPONENT; dwReserved: DWORD): HResult; stdcall;
  end;

implementation

end.

