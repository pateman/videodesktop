program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, uwinapi, ufunctions, Unit2
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='VideoDesktop';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.ShowMainForm := False;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

