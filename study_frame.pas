unit study_frame;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, ComCtrls, StdCtrls;

type

  { TStudyFrame }

  TStudyFrame = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
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
    destructor Destroy; override;
  end; 

implementation

uses
  design_frame, Controls;

{ TStudyFrame }

procedure TStudyFrame.RelateTreeViewChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  if Assigned(node) then
    Label1.Caption := TDesignFrame(Node.Data).Name;
  FActiveFrame.Parent := nil;
  FActiveFrame.Align := alNone;
end;

procedure TStudyFrame.RelateTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  Label2.Caption := TDesignFrame(Node.Data).Name;
  FActiveFrame := TDesignFrame(Node.Data);
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

destructor TStudyFrame.Destroy;
begin
  inherited Destroy;
end;

initialization
  {$I study_frame.lrs}

end.

