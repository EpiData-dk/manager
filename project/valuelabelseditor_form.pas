unit valuelabelseditor_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ExtDlgs, Grids, ActnList;

type

  { TValueLabelEditor }

  TValueLabelEditor = class(TForm)
    DeleteValueLabelSets: TAction;
    NewValueLabelSet: TAction;
    ActionList1: TActionList;
    Panel1: TPanel;
    Splitter1: TSplitter;
    StringGrid1: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    TreeView1: TTreeView;
    procedure DeleteValueLabelSetsExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure NewValueLabelSetExecute(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end; 


implementation

{$R *.lfm}

uses
  project_frame;

{ TValueLabelEditor }

procedure TValueLabelEditor.NewValueLabelSetExecute(Sender: TObject);
begin
  //
end;

constructor TValueLabelEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TValueLabelEditor.DeleteValueLabelSetsExecute(Sender: TObject);
begin
  //
end;

procedure TValueLabelEditor.FormShow(Sender: TObject);
begin
  ToolBar1.Images := TProjectFrame(Owner).ActionList1.Images;
  ActionList1.Images := ToolBar1.Images;
  NewValueLabelSet.ImageIndex := 3;
  DeleteValueLabelSets.ImageIndex := 4;
end;

procedure TValueLabelEditor.ListBox1SelectionChange(Sender: TObject;
  User: boolean);
begin
  ShowMessage(IntToStr(ListBox1.ItemIndex));
end;

end.

