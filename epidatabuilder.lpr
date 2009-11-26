program epidatabuilder;

{$codepage UTF8}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, LResources, design_frame, fieldedit, Design_Field_Frame,
designutils, settings, design_label_form;

{$IFDEF WINDOWS}
  {$IFDEF WIN32}
    {$R epidatabuilder.rc}
  {$ENDIF}
{$ENDIF}

begin
  {$I epidatabuilder.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

