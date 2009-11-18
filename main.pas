unit main; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, StdActns, ExtCtrls, Buttons, ComCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    ControlBtn: TBitBtn;
    MetaDataBtn: TBitBtn;
    DesignBtn: TBitBtn;
    MainFormActionList: TActionList;
    FileExit1: TFileExit;
    MainFormMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExitMenuItem: TMenuItem;
    ProgressPanel: TPanel;
    StatusBar1: TStatusBar;
    procedure DesignBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  design_frame;

var
  Frame: TFrame;

{ TMainForm }

procedure TMainForm.DesignBtnClick(Sender: TObject);
begin
  Frame := TDesignFrame.Create(self);
  Frame.Align := alClient;
  Frame.BringToFront;
  Frame.Parent := Self;
  DesignBtn.Enabled := false;
end;

initialization
  {$I main.lrs}

end.

