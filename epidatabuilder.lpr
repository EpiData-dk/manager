program epidatabuilder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, LResources
  { you can add units after this };

{$IFDEF WINDOWS}{$R epidatabuilder.rc}{$ENDIF}

begin
  {$I epidatabuilder.lrs}
  Application.Title := 'EpiData Builder';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

