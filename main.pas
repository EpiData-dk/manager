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
    PageControl1: TPageControl;
    ProgressPanel: TPanel;
    StatusBar1: TStatusBar;
    StatusBar2: TStatusBar;
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


{ TMainForm }

procedure TMainForm.DesignBtnClick(Sender: TObject);
var
  Frame: TFrame;
  TabSheet: TTabSheet;
begin
  TabSheet := TTabSheet.Create(PageControl1);
  TabSheet.PageControl := PageControl1;
  TabSheet.Name := 'TabSheet' + IntToStr(PageControl1.PageCount);
  PageControl1.ActivePage := TabSheet;

  if PageControl1.PageCount > 1 then
    PageControl1.ShowTabs := true;

  Frame := TDesignFrame.Create(self);
  Frame.Name := 'Frame' + IntToStr(PageControl1.PageCount);
  Frame.Align := alClient;
  Frame.Parent := TabSheet;
end;

initialization
  {$I main.lrs}

end.

