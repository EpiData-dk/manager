program epidatamanager;

{$codepage UTF8}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, design_frame, fieldedit, Design_Field_Frame,
  settings, design_label_form, design_autoalign_form;

{$IFDEF WINDOWS}
  {$IFDEF WIN32}
    {$R epidatamanager.rc}
  {$ENDIF}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

