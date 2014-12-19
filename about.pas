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
    CoreRevisionLabel: TLabel;
    CoreVersionLabel: TLabel;
    FPCVersionLabel: TLabel;
    Image1: TImage;
    ManagerRevisionLabel: TLabel;
    Memo1: TMemo;
    PlatformLabel: TLabel;
    Shape1: TShape;
    VersionLabel: TLabel;
    VersionPage: TTabSheet;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    class procedure RestoreDefaultPos;
  end; 

function GetProgramInfo: string;

implementation

{$R *.lfm}

uses
  settings2, epiversionutils, settings2_var;

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

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  AboutPageControl.PageIndex := 0;

  VersionLabel.Caption         := ManagerVersionCaption;
  ManagerRevisionLabel.Caption := RevisionCaption;
  CoreVersionLabel.Caption     := CoreVersionCaption;
  CoreRevisionLabel.Caption    := CoreRevisionCaption;
  FPCVersionLabel.Caption      := FPCCaption;
  PlatformLabel.Caption        := PlatformCaption;
end;

procedure TAboutForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, Self.ClassName);
end;

procedure TAboutForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, Self.ClassName);
end;

class procedure TAboutForm.RestoreDefaultPos;
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 500;
  Aform.Height := 500;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, TAboutForm.ClassName);
  AForm.free;
end;

end.

