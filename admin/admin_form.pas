unit admin_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Grids, StdCtrls, ComCtrls;

type

  { TAdminForm }

  TAdminForm = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    StringGrid3: TStringGrid;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid2DblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

uses
  admin_user_form, admin_group_form;

{ TAdminForm }

procedure TAdminForm.StringGrid1DblClick(Sender: TObject);
var
  F: TUserAdminForm;
begin
  F := TUserAdminForm.Create(Self);
  F.ShowModal;
  F.Free;
end;

procedure TAdminForm.StringGrid2DblClick(Sender: TObject);
var
  F: TGroupAdminForm;
begin
  F := TGroupAdminForm.Create(self);
  F.ShowModal;
  F.Free;
end;

end.

