unit main; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, StdActns, ExtCtrls, Buttons, ComCtrls, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    ControlBtn: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MetaDataBtn: TBitBtn;
    DesignBtn: TBitBtn;
    MainFormActionList: TActionList;
    FileExit1: TFileExit;
    MainFormMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExitMenuItem: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    ProgressPanel: TPanel;
    StatusBar1: TStatusBar;
    StatusBar2: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure DesignBtnClick(Sender: TObject);
  private
    { private declarations }
    TabNameCount: integer;
    procedure CloseTab(Sender: TObject);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
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
  TabSheet.Name := 'TabSheet' + IntToStr(TabNameCount);
  PageControl1.ActivePage := TabSheet;

  if PageControl1.PageCount > 1 then
  begin
    PageControl1.ShowTabs := true;
    PageControl1.Options := PageControl1.Options + [nboShowCloseButtons];
    PageControl1.OnCloseTabClicked := @CloseTab;
  end;
  Frame := TDesignFrame.Create(TabSheet);
  Frame.Name := 'Frame' + IntToStr(TabNameCount);
  Frame.Align := alClient;
  Frame.Parent := TabSheet;

  Inc(TabNameCount);
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Panel1.ControlCount - 1 do
    if Panel1.Controls[i] is TLabel then
      TLabel(Panel1.Controls[i]).Caption := Panel1.Controls[i].Name;
end;

procedure TMainForm.CloseTab(Sender: TObject);
begin
  if not (Sender is TTabSheet) then exit;

  PageControl1.ActivePage := PageControl1.FindNextPage(TTabSheet(Sender), True, True);
  (Sender as TTabSheet).Free;
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  TabNameCount := 1;
end;

initialization
  {$I main.lrs}

end.

