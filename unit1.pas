unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Windows, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Dialogs, StdCtrls,
  PasLibVlcUnit, Classes, MultiMon, Graphics;

const
  PANEL_WIDTH = 220;
  THUMB_WIDTH = 113;
  THUMB_HEIGHT = 55;
  THUMB_COMPNAME = 'thumb';

  PW_CLIENTONLY = $00000001;
  INTERNET_MAX_PATH_LENGTH = 2048;
  INTERNET_MAX_SCHEME_LENGTH = 32;         // longest protocol name length
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
    dwSize: DWORD;                  // Size of this structure
    iLeft: integer;                 // Left of top-left corner in screen co-ordinates.
    iTop: integer;                  // Top of top-left corner in screen co-ordinates.
    dwWidth: DWORD;                 // Width in pixels.
    dwHeight: DWORD;                // Height in pixels.
    izIndex: integer;               // Indicates the Z-order of the component.
    fCanResize: BOOL;               // Is the component resizeable?
    fCanResizeX: BOOL;              // Resizeable in X-direction?
    fCanResizeY: BOOL;              // Resizeable in Y-direction?
    iPreferredLeftPercent: integer; // Left of top-left corner as percent of screen width
    iPreferredTopPercent: integer;  // Top of top-left corner as percent of screen height
  end;
  COMPPOS = _tagCOMPPOS;
  TCompPos = _tagCOMPPOS;

  PCompStateInfo = ^TCompStateInfo;

  _tagCOMPSTATEINFO = record
    dwSize: DWORD;             // Size of this structure.
    iLeft: integer;            // Left of the top-left corner in screen co-ordinates.
    iTop: integer;             // Top of top-left corner in screen co-ordinates.
    dwWidth: DWORD;            // Width in pixels.
    dwHeight: DWORD;           // Height in pixels.
    dwItemState: DWORD;
    // State of the component (full-screen mode or split-screen or normal state.
  end;
  COMPSTATEINFO = _tagCOMPSTATEINFO;
  TCompStateInfo = _tagCOMPSTATEINFO;

  PWallPaperOpt = ^TWallPaperOpt;

  _tagWALLPAPEROPT = record
    dwSize: DWORD;     // size of this Structure.
    dwStyle: DWORD;    // WPSTYLE_* mentioned above
  end;
  WALLPAPEROPT = _tagWALLPAPEROPT;
  TWallPaperOpt = _tagWALLPAPEROPT;

  PComponentsOpt = ^TComponentsOpt;

  _tagCOMPONENTSOPT = record
    dwSize: DWORD;            // Size of this structure
    fEnableComponents: BOOL;  // Enable components?
    fActiveDesktop: BOOL;     // Active desktop enabled ?
  end;
  COMPONENTSOPT = _tagCOMPONENTSOPT;
  TComponentsOpt = _tagCOMPONENTSOPT;

  PComponent = ^COMPONENT;

  _tagCOMPONENT = record
    dwSize: DWORD;               // Size of this structure
    dwID: DWORD;                 // Reserved: Set it always to zero.
    iComponentType: integer;     // One of COMP_TYPE_*
    fChecked: BOOL;              // Is this component enabled?
    fDirty: BOOL;
    // Had the component been modified and not yet saved to disk?
    fNoScroll: BOOL;             // Is the component scrollable?
    cpPos: COMPPOS;              // Width, height etc.,
    wszFriendlyName: array[0..MAX_PATH - 1] of widechar;
    // Friendly name of component.
    wszSource: array[0..INTERNET_MAX_URL_LENGTH - 1] of widechar;
    // URL of the component.
    wszSubscribedURL: array[0..INTERNET_MAX_URL_LENGTH - 1] of widechar;
    // Subscrined URL

    //New fields are added below. Everything above here must exactly match the IE4COMPONENT Structure.
    dwCurItemState: DWORD;       // Current state of the Component.
    csiOriginal: TCompStateInfo;
    // Original state of the component when it was first added.
    csiRestored: TCompStateInfo; // Restored state of the component.
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

  TVideoWallpaper = record
    WndHandle: HWND;
    WndClassName: string;
    WndClass: TWndClass;
    VideoPtr: libvlc_media_player_t_ptr;
    MediaPtr: libvlc_media_t_ptr;
    EvtMgr: libvlc_event_manager_t_ptr;
    CurrentVid: string;
    VideoPathComponent: TWinControl;
  end;
  PVideoWallpaper = ^TVideoWallpaper;

  { TForm1 }
  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Button1: TButton;
    Button2: TButton;
    ImageList1: TImageList;
    OpenDialog1: TOpenDialog;
    ScrollBox1: TScrollBox;
    TrayIcon1: TTrayIcon;
    procedure ApplicationProperties1Minimize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
  private
    { private declarations }
    p_li: libvlc_instance_t_ptr;

    procedure Initialize();
    procedure PanelButtonSetClick(Sender: TObject);
    procedure PanelButtonClearClick(Sender: TObject);
    function GetMonitorName(const Hnd: HMONITOR): string;
    function CreatePanelForMonitor(const MonitorIdx: integer): TPanel;

    procedure HandlePanelScroll(const ScrollForward: boolean);
    procedure HandlePanelScrollButtons();
  public
    { public declarations }
    WndHandles: array of TVideoWallpaper;
    CurrentPanel: integer;
  end;

  { TSnapshotThread }

  TSnapshotThread = class(TThread)
  protected
    VideoWall: TVideoWallpaper;
    procedure Execute; override;
  public
    constructor Create(const Wall: TVideoWallpaper);
  end;

var
  Form1: TForm1;
  WorkerW: HWND;

implementation

{$R *.lfm}

uses
  ShlObj, ComObj;

function EnumWindowsProc(_para1: HWND; _para2: LPARAM): WINBOOL; stdcall;
begin
  if (FindWindowEx(_para1, 0, 'SHELLDLL_DefView', nil) > 0) then
    WorkerW := FindWindowEx(0, _para1, 'WorkerW', nil);
  Result := True;
end;

function MyWndProc(_para1: HWND; _para2: UINT; _para3: WPARAM;
  _para4: LPARAM): LRESULT; stdcall;
begin
  Result := DefWindowProc(_para1, _para2, _para3, _para4);
end;

function GetScreenshotFileName(const VidFile: string): string;
begin
  Result := IncludeTrailingBackslash(ExtractFilePath(Application.ExeName)) +
    ChangeFileExt(ExtractFileName(VidFile), '.png');
end;

procedure SnapshotTaken(p_event: libvlc_event_t_ptr; Data: Pointer); cdecl;
var
  VideoWall: PVideoWallpaper;
begin
  VideoWall := Data;

  libvlc_event_detach(VideoWall^.EvtMgr, libvlc_MediaPlayerSnapshotTaken,
    @SnapshotTaken, nil);

  ShowWindow(VideoWall^.WndHandle, SW_SHOW);
  TImage(VideoWall^.VideoPathComponent.FindComponent(THUMB_COMPNAME)).Picture.
    LoadFromFile(GetScreenshotFileName(VideoWall^.CurrentVid));
end;

{ TSnapshotThread }

procedure TSnapshotThread.Execute;
var
  ScreenshotPath: string;
begin
  while (libvlc_media_player_has_vout(VideoWall.VideoPtr) = 0) do ;

  libvlc_event_attach(VideoWall.EvtMgr, libvlc_MediaPlayerSnapshotTaken,
    @SnapshotTaken, @VideoWall);

  ScreenshotPath := GetScreenshotFileName(VideoWall.CurrentVid);

  libvlc_video_take_snapshot(VideoWall.VideoPtr, 0,
    PAnsiChar(Utf8Encode(ScreenshotPath)), THUMB_WIDTH, THUMB_HEIGHT);
end;

constructor TSnapshotThread.Create(const Wall: TVideoWallpaper);
begin
  inherited Create(False);

  FreeOnTerminate := False;
  Self.VideoWall := Wall;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  Progman: HWND;
  Res: DWORD;
begin
  Progman := FindWindow('Progman', nil);
  SendMessageTimeout(Progman, $052C, 0, 0, SMTO_NORMAL, 1000, Res);
  EnumWindows(@EnumWindowsProc, 0);

  libvlc_dynamic_dll_init();

  if (libvlc_dynamic_dll_error <> '') then
  begin
    MessageDlg(libvlc_dynamic_dll_error, mtError, [mbOK], 0);
    exit;
  end;

  with TArgcArgs.Create([libvlc_dynamic_dll_path, '--intf=dummy',
      '--ignore-config', '--quiet', '--vout=direct3d', '--no-video-title-show',
      '--no-video-on-top', '--no-snapshot-preview', '--no-stats',
      '--no-sub-autodetect-file', '--quiet', '--freetype-opacity=0']) do
  begin
    p_li := libvlc_new(ARGC, ARGS);
    Free();
  end;

  SetLength(Self.WndHandles, Screen.MonitorCount);
  Self.Initialize();
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  HandlePanelScroll(False);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  HandlePanelScroll(True);
end;

procedure TForm1.ApplicationProperties1Minimize(Sender: TObject);
begin
  Application.Minimize();
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  I: integer;
  hObj: IUnknown;
  ADesktop: IActiveDesktop;
begin
  for I := Length(WndHandles) - 1 downto 0 do
  begin
    if (WndHandles[I].VideoPtr <> nil) then
    begin
      libvlc_media_player_stop(WndHandles[I].VideoPtr);
      libvlc_media_player_release(WndHandles[I].VideoPtr);
    end;
    DestroyWindow(WndHandles[I].WndHandle);
    Windows.UnRegisterClass(PChar(WndHandles[I].WndClassName), hInstance);
  end;
  SetLength(WndHandles, 0);
  libvlc_release(p_li);

  hObj := CreateComObject(CLSID_ActiveDesktop);
  ADesktop := hObj as IActiveDesktop;
  ADesktop.ApplyChanges(AD_APPLY_ALL or AD_APPLY_FORCE or AD_APPLY_BUFFERED_REFRESH);
end;

procedure TForm1.TrayIcon1DblClick(Sender: TObject);
begin
  Self.Show();
  Application.Restore();
end;

procedure TForm1.Initialize;
var
  I: integer;
begin
  for I := 0 to Length(WndHandles) - 1 do
  begin
    WndHandles[I].VideoPathComponent :=
      CreatePanelForMonitor(Screen.Monitors[I].MonitorNum);
    WndHandles[I].VideoPathComponent.Parent := ScrollBox1;
    WndHandles[I].VideoPathComponent.Left := I * WndHandles[I].VideoPathComponent.Width;
  end;

  HandlePanelScrollButtons();
end;

procedure TForm1.PanelButtonSetClick(Sender: TObject);
var
  Btn: TButton;
  I, TaskbarHeight, W, H: integer;
  Pth: string;
begin
  if (OpenDialog1.Execute()) then
  begin
    Btn := TButton(Sender);
    I := Btn.Tag;

    if (WndHandles[I].WndHandle = 0) then
    begin
      WndHandles[I].WndClassName := 'VideoWallWnd' + IntToStr(I);
      WndHandles[I].WndClass.hInstance := hInstance;
      with WndHandles[I].WndClass do
      begin
        lpszClassName := PChar(WndHandles[I].WndClassName);
        Style := CS_HREDRAW or CS_VREDRAW;
        hIcon := LoadIcon(hInstance, 'MAINICON');
        lpfnWndProc := @MyWndProc;
        hbrBackground := CreateSolidBrush(RGB(I * 100, 0, 0));
        hCursor := LoadCursor(0, IDC_ARROW);
      end;
      Windows.RegisterClass(WndHandles[I].WndClass);

      TaskbarHeight := Screen.Monitors[I].Height -
        Screen.Monitors[I].WorkareaRect.Bottom;
      WndHandles[I].WndHandle :=
        CreateWindow(PChar(WndHandles[I].WndClassName), 'VideoWallWnd',
        WS_POPUP or WS_VISIBLE, Abs(Screen.Monitors[I].Left),
        TaskbarHeight, Screen.Monitors[I].Width, Screen.Monitors[I].Height,
        0, 0, hInstance, nil);

      SetWindowLong(WndHandles[I].WndHandle, GWL_EXSTYLE,
        GetWindowLong(WndHandles[I].WndHandle, GWL_EXSTYLE) or WS_EX_LAYERED);
      SetLayeredWindowAttributes(WndHandles[I].WndHandle, RGB(255, 255, 255),
        0, LWA_COLORKEY);

      Windows.SetParent(WndHandles[I].WndHandle, WorkerW);
    end;

    Pth := Trim(OpenDialog1.FileName);
    if (Length(Pth) = 0) then
      exit;

    WndHandles[I].MediaPtr := libvlc_media_new_path(p_li, PAnsiChar(UTF8Encode(Pth)));
    if (WndHandles[I].MediaPtr <> nil) then
    begin
      if (WndHandles[I].VideoPtr <> nil) then
      begin
        libvlc_media_player_stop(WndHandles[I].VideoPtr);
        libvlc_media_player_release(WndHandles[I].VideoPtr);
      end;
      WndHandles[I].VideoPtr :=
        libvlc_media_player_new_from_media(WndHandles[I].MediaPtr);
      if (WndHandles[I].VideoPtr <> nil) then
      begin
        WndHandles[I].CurrentVid := Pth;
        WndHandles[I].EvtMgr :=
          libvlc_media_player_event_manager(WndHandles[I].VideoPtr);
        libvlc_video_set_key_input(WndHandles[I].VideoPtr, 1);
        libvlc_video_set_mouse_input(WndHandles[I].VideoPtr, 1);
        libvlc_media_add_option(WndHandles[I].MediaPtr, 'input-repeat=-1');
        libvlc_media_player_set_display_window(WndHandles[I].VideoPtr,
          WndHandles[I].WndHandle);

        ShowWindow(WndHandles[I].WndHandle, SW_HIDE);
        libvlc_media_player_play(WndHandles[I].VideoPtr);

        TSnapshotThread.Create(WndHandles[I]);
        libvlc_media_release(WndHandles[I].MediaPtr);
      end;
    end;
  end;
end;

procedure TForm1.PanelButtonClearClick(Sender: TObject);
var
  Btn: TButton;
  I: integer;
begin
  Btn := TButton(Sender);
  I := Btn.Tag;

  if (WndHandles[I].VideoPtr <> nil) then
  begin
    libvlc_media_player_stop(WndHandles[I].VideoPtr);
    libvlc_media_player_release(WndHandles[I].VideoPtr);
    WndHandles[I].VideoPtr := nil;

    TImage(WndHandles[I].VideoPathComponent.FindComponent(THUMB_COMPNAME)).
      Picture.Clear();
  end;
end;

function TForm1.GetMonitorName(const Hnd: HMONITOR): string;
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

function TForm1.CreatePanelForMonitor(const MonitorIdx: integer): TPanel;
var
  Container: TPanel;
  IconMonitor, IconThumb: TImage;
  MonitorName: TLabel;
  Bmp: TBitmap;
  ButtonSet, ButtonClear: TButton;
begin
  Container := TPanel.Create(Self);
  Container.Width := PANEL_WIDTH;
  Container.Height := 192;

  IconMonitor := TImage.Create(Container);
  IconMonitor.Parent := Container;
  IconMonitor.Width := 220;
  IconMonitor.Height := 96;
  IconMonitor.Left := 48;
  IconMonitor.Top := 24;

  IconThumb := TImage.Create(Container);
  IconThumb.Parent := Container;
  IconThumb.Width := THUMB_WIDTH;
  IconThumb.Height := THUMB_HEIGHT;
  IconThumb.Left := 56;
  IconThumb.Top := 32;
  IconThumb.Name := THUMB_COMPNAME;

  Bmp := TBitmap.Create();
  try
    ImageList1.GetBitmap(0, Bmp);
    IconMonitor.Picture.Assign(Bmp);
  finally
    Bmp.Free();
  end;

  MonitorName := TLabel.Create(Container);
  MonitorName.Parent := Container;
  MonitorName.Top := 120;
  MonitorName.AutoSize := False;
  MonitorName.Width := Container.Width;
  MonitorName.Alignment := taCenter;
  MonitorName.Font.Style := [fsBold];
  MonitorName.Caption := GetMonitorName(Screen.Monitors[Screen.MonitorCount -
    MonitorIdx - 1].Handle);

  ButtonSet := TButton.Create(Container);
  ButtonSet.Parent := Container;
  ButtonSet.Left := 8;
  ButtonSet.Top := 151;
  ButtonSet.Caption := 'Set';
  ButtonSet.Width := 75;
  ButtonSet.OnClick := @PanelButtonSetClick;
  ButtonSet.Tag := MonitorIdx;

  ButtonClear := TButton.Create(Container);
  ButtonClear.Parent := Container;
  ButtonClear.Left := 128;
  ButtonClear.Top := 151;
  ButtonClear.Caption := 'Clear';
  ButtonClear.Width := 75;
  ButtonClear.OnClick := @PanelButtonClearClick;
  ButtonClear.Tag := MonitorIdx;

  Result := Container;
end;

procedure TForm1.HandlePanelScroll(const ScrollForward: boolean);
var
  ScrollDelta: integer;
begin
  if (ScrollForward) then
  begin
    ScrollDelta := -PANEL_WIDTH;
    Inc(CurrentPanel);
  end
  else
  begin
    ScrollDelta := PANEL_WIDTH;
    Dec(CurrentPanel);
  end;
  ScrollBox1.ScrollBy(ScrollDelta, 0);
  HandlePanelScrollButtons();
end;

procedure TForm1.HandlePanelScrollButtons;
begin
  Button1.Enabled := (CurrentPanel > 0);
  Button2.Enabled := (CurrentPanel < (Length(WndHandles) - 1));
end;

end.
