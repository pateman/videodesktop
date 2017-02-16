unit Unit3;

{$mode objfpc}{$H+}

interface

uses
 Forms, StdCtrls;

const
  VIDEODESKTOP_REG_KEY = 'VideoDesktop';

type
  { TForm3 }
  TForm3 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbRunOnStartup: TCheckBox;
    cbLoop: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.lfm}

uses
  UFunctions, Registry;

{ TForm3 }

procedure TForm3.FormCreate(Sender: TObject);
begin
  cbRunOnStartup.Checked := Config.ReadBool(GLOBAL_SECTION, RUNONSTARTUP_VALUE, True);
  cbLoop.Checked := Config.ReadBool(GLOBAL_SECTION, LOOPVIDEOS_VALUE, True);
end;

procedure TForm3.Button1Click(Sender: TObject);
var
  Reg: TRegistry;
begin
  Config.WriteBool(GLOBAL_SECTION, RUNONSTARTUP_VALUE, cbRunOnStartup.Checked);
  Config.WriteBool(GLOBAL_SECTION, LOOPVIDEOS_VALUE, cbLoop.Checked);

  Reg := TRegistry.Create();
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if (Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Run', False)) then
    begin
      if ((not Reg.ValueExists(VIDEODESKTOP_REG_KEY)) and (cbRunOnStartup.Checked)) then
        Reg.WriteString(VIDEODESKTOP_REG_KEY, '"' + Application.ExeName + '"')
      else if ((Reg.ValueExists(VIDEODESKTOP_REG_KEY)) and
        (not cbRunOnStartup.Checked)) then
        Reg.DeleteValue(VIDEODESKTOP_REG_KEY);

      Reg.CloseKey();
    end;
  finally
    Reg.Free();
  end;

  Self.Close();
end;

end.

