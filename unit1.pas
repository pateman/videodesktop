unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Windows, SysUtils, Forms, Controls, ExtCtrls, Dialogs, StdCtrls,
  PasLibVlcUnit, Classes, Graphics, Menus, IniFiles, regexpr, uwinapi;

const
  PANEL_WIDTH = 220;
  THUMB_WIDTH = 113;
  THUMB_HEIGHT = 55;
  THUMB_COMPNAME = 'thumb';

type
  { TVideoWallpaper }
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
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog1: TOpenDialog;
    ScrollBox1: TScrollBox;
    TrayIcon1: TTrayIcon;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
  private
    { private declarations }
    p_li: libvlc_instance_t_ptr;
    WndHandles: array of TVideoWallpaper;
    CurrentUser: string;

    procedure Initialize();
    procedure PanelButtonSetClick(Sender: TObject);
    procedure PanelButtonClearClick(Sender: TObject);
    function CreatePanelForMonitor(const MonitorIdx: integer): TPanel;

    procedure SetVideoForMonitor(const MonitorIdx: integer; const FileName: string);
    procedure StoreSettingForMonitor(const MonitorIdx: integer; const FileName: string);

    procedure MyOnSize(var Msg: TMessage); message WM_SIZE;
  public
    { public declarations }
  end;

  { TSnapshotThread }
  TSnapshotThread = class sealed(TThread)
  protected
    { protected declarations }
    VideoWall: TVideoWallpaper;
    procedure Execute; override;
  public
    { public declarations }
    constructor Create(const Wall: TVideoWallpaper);
  end;

var
  Form1: TForm1;
  WorkerW: HWND;

implementation

{$R *.lfm}

uses
  ShlObj, ComObj, ufunctions, Unit2, Unit3;

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
  CurrentUser := GetCurrentUser();

  Self.Initialize();
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
  Config.Free();

  // Repaint the desktop.
  hObj := CreateComObject(CLSID_ActiveDesktop);
  ADesktop := hObj as IActiveDesktop;
  ADesktop.ApplyChanges(AD_APPLY_ALL or AD_APPLY_FORCE or AD_APPLY_BUFFERED_REFRESH);
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  Form3 := TForm3.Create(Application);
  try
    Form3.ShowModal();
  finally
    Form3.Free();
  end;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  Form2 := TForm2.Create(Application);
  try
    Form2.ShowModal();
  finally
    Form2.Free();
  end;
end;

procedure TForm1.TrayIcon1DblClick(Sender: TObject);
begin
  Application.Restore();
  Self.Show();
end;

procedure TForm1.Initialize;
var
  I: integer;
  SL, Split: TStringList;
  Val: string;
begin
  InitializeConfig();

  for I := 0 to Length(WndHandles) - 1 do
  begin
    WndHandles[I].VideoPathComponent :=
      CreatePanelForMonitor(Screen.Monitors[I].MonitorNum);
    WndHandles[I].VideoPathComponent.Parent := ScrollBox1;
    WndHandles[I].VideoPathComponent.Left := I * WndHandles[I].VideoPathComponent.Width;
  end;

  SL := TStringList.Create();
  Split := TStringList.Create();
  try
    Config.ReadSectionValues(Self.CurrentUser, SL);
    if (SL.Count > 0) then
      for I := 0 to SL.Count - 1 do
      begin
        if ExecRegExpr('^Monitor_\d+', SL.Names[I]) then
        begin
          SplitString('_', SL.Names[I], Split);
          Val := Trim(ReplaceRegExpr('\\\\(.{1,1})', SL.ValueFromIndex[I], '$1', True));

          if ((Val <> '') and (FileExists(Val))) then
            SetVideoForMonitor(StrToInt(Split[1]), Val);
        end;
      end;
  finally
    Split.Free();
    SL.Free();
  end;
end;

procedure TForm1.PanelButtonSetClick(Sender: TObject);
var
  MonitorIdx: integer;
  DstFile: string;
begin
  if (OpenDialog1.Execute()) then
  begin
    MonitorIdx := TButton(Sender).Tag;
    DstFile := Trim(OpenDialog1.FileName);

    SetVideoForMonitor(MonitorIdx, DstFile);
    StoreSettingForMonitor(MonitorIdx, DstFile);
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
    StoreSettingForMonitor(I, '');
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

procedure TForm1.SetVideoForMonitor(const MonitorIdx: integer; const FileName: string);
var
  Wnd: PVideoWallpaper;
  TaskbarHeight: integer;
begin
  if (FileName = '') then
    exit;

  Wnd := @WndHandles[MonitorIdx];

  if (Wnd^.WndHandle = 0) then
  begin
    Wnd^.WndClassName := 'VideoWallWnd' + IntToStr(MonitorIdx);
    Wnd^.WndClass.hInstance := hInstance;
    with Wnd^.WndClass do
    begin
      lpszClassName := PChar(Wnd^.WndClassName);
      Style := CS_HREDRAW or CS_VREDRAW;
      hIcon := LoadIcon(hInstance, 'MAINICON');
      lpfnWndProc := @MyWndProc;
      hbrBackground := CreateSolidBrush(RGB(MonitorIdx * 100, 0, 0));
      hCursor := LoadCursor(0, IDC_ARROW);
    end;
    Windows.RegisterClass(Wnd^.WndClass);

    TaskbarHeight := Screen.Monitors[MonitorIdx].Height -
      Screen.Monitors[MonitorIdx].WorkareaRect.Bottom;
    Wnd^.WndHandle :=
      CreateWindow(PChar(Wnd^.WndClassName), 'VideoWallWnd', WS_POPUP or
      WS_VISIBLE, Abs(Screen.Monitors[MonitorIdx].Left), TaskbarHeight,
      Screen.Monitors[MonitorIdx].Width, Screen.Monitors[MonitorIdx].Height,
      0, 0, hInstance, nil);

    SetWindowLong(Wnd^.WndHandle, GWL_EXSTYLE,
      GetWindowLong(Wnd^.WndHandle, GWL_EXSTYLE) or WS_EX_LAYERED);
    SetLayeredWindowAttributes(Wnd^.WndHandle, RGB(255, 255, 255),
      0, LWA_COLORKEY);

    Windows.SetParent(Wnd^.WndHandle, WorkerW);
  end;

  Wnd^.MediaPtr := libvlc_media_new_path(p_li, PAnsiChar(UTF8Encode(FileName)));
  if (Wnd^.MediaPtr <> nil) then
  begin
    if (Wnd^.VideoPtr <> nil) then
    begin
      libvlc_media_player_stop(Wnd^.VideoPtr);
      libvlc_media_player_release(Wnd^.VideoPtr);
    end;
    Wnd^.VideoPtr :=
      libvlc_media_player_new_from_media(Wnd^.MediaPtr);
    if (Wnd^.VideoPtr <> nil) then
    begin
      Wnd^.CurrentVid := FileName;
      Wnd^.EvtMgr :=
        libvlc_media_player_event_manager(Wnd^.VideoPtr);
      libvlc_video_set_key_input(Wnd^.VideoPtr, 1);
      libvlc_video_set_mouse_input(Wnd^.VideoPtr, 1);
      if (Config.ReadBool(GLOBAL_SECTION, LOOPVIDEOS_VALUE, True)) then
        libvlc_media_add_option(Wnd^.MediaPtr, 'input-repeat=-1');
      libvlc_media_player_set_display_window(Wnd^.VideoPtr,
        Wnd^.WndHandle);

      ShowWindow(Wnd^.WndHandle, SW_HIDE);
      libvlc_media_player_play(Wnd^.VideoPtr);

      TSnapshotThread.Create(Wnd^);
      libvlc_media_release(Wnd^.MediaPtr);
    end;
  end;
end;

procedure TForm1.StoreSettingForMonitor(const MonitorIdx: integer;
  const FileName: string);
begin
  Config.WriteString(Self.CurrentUser, 'Monitor_' + IntToStr(MonitorIdx),
    ReplaceRegExpr('(\W)', FileName, '\\\\$1', True));
end;

procedure TForm1.MyOnSize(var Msg: TMessage);
begin
  if (Msg.wParam = SIZE_MINIMIZED) then
     Self.Hide();
  Msg.Result := 1;
end;

end.
