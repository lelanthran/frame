program FrameBrowser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainWin, FrameWrapper
  { you can add units after this };

{$R *.res}

var current_frame: PChar;

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  frame_var := frm_init('/home/lelanthran/.framedb');
  current_frame := frm_current(frame_var);
  Writeln('current_frame: ', current_frame);
  Application.Run;
end.

