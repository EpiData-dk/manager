unit study_frame;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, ComCtrls;

type

  { TStudyFrame }

  TStudyFrame = class(TFrame)
    Panel1: TPanel;
    Splitter1: TSplitter;
    RelateTreeView: TTreeView;
    procedure RelateTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure RelateTreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
  private
    { private declarations }
    FActiveFrame: TFrame;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
  end; 

implementation

uses
  design_frame, Controls;

{ TStudyFrame }

procedure TStudyFrame.RelateTreeViewChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  FActiveFrame.Parent := nil;
  FActiveFrame.Align := alNone;
end;

procedure TStudyFrame.RelateTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  FActiveFrame := TStudyFrame(Node.Data);
  FActiveFrame.Parent := self;
  FActiveFrame.Align := alClient;
end;

constructor TStudyFrame.Create(AOwner: TComponent);
var
  DFrame: TDesignFrame;
begin
  inherited Create(AOwner);
  DFrame := TDesignFrame.Create(Self);
  DFrame.Name := 'a';
  DFrame.Align := alClient;
  DFrame.Parent := Self;
  FActiveFrame := DFrame;

  RelateTreeView.Items.AddObject(nil, 'main', DFrame);

  DFrame := TDesignFrame.Create(Self);
  DFrame.Name := 'b';
  RelateTreeView.Items.AddObject(nil, 'second', DFrame);
end;

initialization
  {$I study_frame.lrs}

end.

