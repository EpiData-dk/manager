unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    AboutPage: TTabSheet;
    AboutPageControl: TPageControl;
    ButtonPanel: TPanel;
    CloseButton: TBitBtn;
    CoreVersionLabel: TLabel;
    FPCVersionLabel: TLabel;
    Image1: TImage;
    PlatformLabel: TLabel;
    RevisionLabel: TLabel;
    VersionLabel: TLabel;
    VersionPage: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

function GetProgramInfo: string;

implementation

uses
  settings2, epiversionutils;

function ManagerVersionCaption: string;
begin
  result := 'Program Version: ' + GetManagerVersion;
end;

function CoreVersionCaption: string;
begin
  result := 'Core version: ' + GetCoreVersionInfo;
end;

function RevisionCaption: string;
begin
  result := 'r' + RevisionStr;
end;

function CoreRevisionCaption: string;
begin
  result := 'r' + GetCoreRevision;
end;

function FPCCaption: string;
begin
  result := 'FPC Version: ' + {$I %FPCVERSION%};
end;

function PlatformCaption: string;
begin
  result := 'Platform: ' + {$I %FPCTARGETCPU%} + '-' + {$I %FPCTARGETOS%};
end;

function GetProgramInfo: string;
begin
  Result := 'EpiData Manager' + LineEnding +
            ManagerVersionCaption + ' ' + RevisionCaption + LineEnding +
            CoreVersionCaption + ' ' + CoreRevisionCaption + LineEnding +
            FPCCaption + LineEnding +
            PlatformCaption;
end;

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  AboutPageControl.PageIndex := 0;

  VersionLabel.Caption     := ManagerVersionCaption;
  CoreVersionLabel.Caption := CoreVersionCaption;
  RevisionLabel.Caption    := RevisionCaption;
  FPCVersionLabel.Caption  := FPCCaption;
  PlatformLabel.Caption    := PlatformCaption;
end;

end.

