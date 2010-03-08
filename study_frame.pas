unit study_frame;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, ComCtrls, StdCtrls,
  managertypes;

type

  { TStudyFrame }

  TStudyFrame = class(TFrame, IManagerFrame)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    RelateTreeView: TTreeView;
    Procedure Button1Click(Sender: TObject);
    procedure RelateTreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    Procedure RelateTreeViewSelectionChanged(Sender: TObject);
  private
    { private declarations }
    FActiveFrame: TFrame;
    FActivated: boolean;
    FrameCount: integer;
  protected
    property Activated: boolean read FActivated write FActivated;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure  ActivateFrame;
    procedure  DeActivateFrame;
  end; 

implementation

uses
  design_frame, Controls;

{ TStudyFrame }

procedure TStudyFrame.RelateTreeViewChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  if csDestroying in ComponentState then exit;

  if Assigned(node) then
    Label1.Caption := TDesignFrame(Node.Data).Name;

  if not Assigned(FActiveFrame) then exit;
  (FActiveFrame as IManagerFrame).DeActivateFrame;
  FActiveFrame.Parent := nil;
  FActiveFrame.Align := alNone;
end;

Procedure TStudyFrame.RelateTreeViewSelectionChanged(Sender: TObject);
Begin
  if csDestroying in ComponentState then exit;

  Label2.Caption := TDesignFrame(RelateTreeView.Selected.Data).Name;
  FActiveFrame := TDesignFrame(RelateTreeView.Selected.Data);
  FActiveFrame.Parent := self;
  FActiveFrame.Align := alClient;
  FActiveFrame.SetFocus;
  (FActiveFrame as IManagerFrame).ActivateFrame;
end;

Procedure TStudyFrame.Button1Click(Sender: TObject);
Var
  DFrame: TDesignFrame;
Begin
  inc(FrameCount);

  DFrame := TDesignFrame.Create(Self);
  DFrame.Name := 'Frame' + IntToStr(FrameCount);
  DFrame.Align := alClient;
  RelateTreeView.Selected := RelateTreeView.Items.AddObject(nil, DFrame.Name, DFrame);
end;

constructor TStudyFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FrameCount := 0;
  FActiveFrame := nil;
  Activated := true;

  Button1.Click;
end;

destructor TStudyFrame.Destroy;
begin
  inherited Destroy;
end;

Procedure TStudyFrame.ActivateFrame;
Begin
  if Activated then exit;
  (FActiveFrame as IManagerFrame).ActivateFrame;
  Activated := true;
End;

Procedure TStudyFrame.DeActivateFrame;
Begin
  if not Activated then exit;
  (FActiveFrame as IManagerFrame).DeActivateFrame;
  Activated := false;
End;

initialization
  {$I study_frame.lrs}

end.

