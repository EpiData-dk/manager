unit study_frame;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, ComCtrls, StdCtrls,
  managertypes, Controls, Menus, ActnList, StdActns, epidocument, epidatatypes;

type

  { TProjectFrame }

  TProjectFrame = class(TFrame, IManagerFrame)
    NewDataFormAction: TAction;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OpenStudyAction: TAction;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    RelateTreeView: TTreeView;
    SaveStudyAsAction: TAction;
    SaveStudyAction: TAction;
    StudyActionList: TActionList;
    StudyImageList: TImageList;
    MenuItem1: TMenuItem;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    OpenProjectBtn: TToolButton;
    StudyDivider1: TToolButton;
    SaveProjectBtn: TToolButton;
    SaveProjectAsBtn: TToolButton;
    StudyDivider3: TToolButton;
    NewDataformBtn: TToolButton;
    procedure NewDataFormActionExecute(Sender: TObject);
    Procedure OpenStudyActionExecute(Sender: TObject);
    procedure RelateTreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    Procedure RelateTreeViewSelectionChanged(Sender: TObject);
    procedure SaveStudyActionExecute(Sender: TObject);
    procedure SaveStudyAsActionExecute(Sender: TObject);
  private
    { private declarations }
    FActiveFrame: TFrame;
    FActivated: boolean;
    FrameCount: integer;
    FEpiDocument: TEpiDocument;
    procedure OnDatafileChange(Sender: TObject; EventType: TEpiDataFileChangeEventType; Data: Pointer);
  protected
    property Activated: boolean read FActivated write FActivated;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure  ActivateFrame;
    procedure  DeActivateFrame;
    property   EpiDocument: TEpiDocument read FEpiDocument;
    property   ActiveFrame: TFrame read FActiveFrame;
  end; 

implementation

uses
  design_frame, epidatafile;


type

  { TEpiDataFileEx }

  TEpiDataFileEx = class(TEpiDataFile)
  private
    FTreeNode: TTreeNode;
  public
    property TreeNode: TTreeNode read FTreeNode write FTreeNode;
  end;

{ TProjectFrame }

procedure TProjectFrame.RelateTreeViewChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  if csDestroying in ComponentState then exit;

  if Assigned(node) then
    Label1.Caption := TDesignFrame(Node.Data).Name;

  if not Assigned(FActiveFrame) then exit;
  (FActiveFrame as IManagerFrame).DeActivateFrame;
  FActiveFrame.Parent := panel3;
  FActiveFrame.Align := alNone;
end;

Procedure TProjectFrame.RelateTreeViewSelectionChanged(Sender: TObject);
Begin
  if csDestroying in ComponentState then exit;

  Label2.Caption := TDesignFrame(RelateTreeView.Selected.Data).Name;
  FActiveFrame := TDesignFrame(RelateTreeView.Selected.Data);
  FActiveFrame.Parent := self;
  FActiveFrame.Align := alClient;
  FActiveFrame.SetFocus;
  (FActiveFrame as IManagerFrame).ActivateFrame;
end;

procedure TProjectFrame.SaveStudyActionExecute(Sender: TObject);
begin
  //
end;

procedure TProjectFrame.SaveStudyAsActionExecute(Sender: TObject);
begin
  //
end;

procedure TProjectFrame.OnDatafileChange(Sender: TObject;
  EventType: TEpiDataFileChangeEventType; Data: Pointer);
begin
  case EventType of
    dceName:
      begin
        TEpiDataFileEx(Sender).TreeNode.Text := TEpiDataFileEx(Sender).FileName;
      end;
  end;
end;

procedure TProjectFrame.NewDataFormActionExecute(Sender: TObject);
Var
  Frame: TDesignFrame;
  Df: TEpiDataFileEx;
begin
  inc(FrameCount);

  Df := TEpiDataFileEx.Create;
  DF.RegisterOnChangeHook(@OnDatafileChange);
  EpiDocument.DataFiles.Add(Df);

  Frame := TDesignFrame.Create(Self, Df);
  Frame.Name := 'Frame' + IntToStr(FrameCount);
  Frame.Align := alClient;
  RelateTreeView.Selected := RelateTreeView.Items.AddObject(nil, Frame.Name, Frame);
  Df.TreeNode := RelateTreeView.Selected;
end;

Procedure TProjectFrame.OpenStudyActionExecute(Sender: TObject);
Begin
  //
end;

constructor TProjectFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FrameCount := 0;
  FActiveFrame := nil;
  Activated := true;

  FEpiDocument := TEpiDocument.Create;

  {$IFDEF EPI_DEBUG}
  Panel2.Visible := true;
  {$ELSE EPI_DEBUG}
  Panel2.Visible := false;
  {$ENDIF}
end;

destructor TProjectFrame.Destroy;
begin
  inherited Destroy;
end;

Procedure TProjectFrame.ActivateFrame;
Begin
  if Activated then exit;
  (FActiveFrame as IManagerFrame).ActivateFrame;
  Activated := true;
End;

Procedure TProjectFrame.DeActivateFrame;
Begin
  if not Activated then exit;
  (FActiveFrame as IManagerFrame).DeActivateFrame;
  Activated := false;
End;

initialization
  {$I study_frame.lrs}

end.

