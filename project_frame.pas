unit project_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, ComCtrls, ActnList,
  Controls, epidocument, epidatafiles, epicustombase;

type

  { TProjectFrame }

  TProjectFrame = class(TFrame)
    DeleteDataFormAction: TAction;
    SaveProjectAsAction: TAction;
    SaveProjectAction: TAction;
    OpenProjectAction: TAction;
    ProjectImageList: TImageList;
    NewDataFormAction: TAction;
    ActionList1: TActionList;
    ProjectPanel: TPanel;
    DataFilesTreeView: TTreeView;
    ToolBar1: TToolBar;
    OpenProjectToolBtn: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    procedure NewDataFormActionExecute(Sender: TObject);
    procedure OpenProjectActionExecute(Sender: TObject);
    procedure SaveProjectActionExecute(Sender: TObject);
  private
    { private declarations }
    FActiveFrame: TFrame;
    FrameCount: integer;
    FEpiDocument: TEpiDocument;
    procedure OnDataFileChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property   EpiDocument: TEpiDocument read FEpiDocument;
    property   ActiveFrame: TFrame read FActiveFrame;
  end;

implementation

{$R *.lfm}

uses
  design_frame, Clipbrd;

type

  { TEpiDataFileEx }

  TEpiDataFileEx = class(TEpiDataFile)
  private
    FTreeNode: TTreeNode;
  public
    property TreeNode: TTreeNode read FTreeNode write FTreeNode;
  end;

{ TProjectFrame }

procedure TProjectFrame.NewDataFormActionExecute(Sender: TObject);
var
  Df: TEpiDataFileEx;
  Frame: TDesignFrame;
begin
  inc(FrameCount);

  Df := TEpiDataFileEx(EpiDocument.DataFiles.NewItem(TEpiDataFileEx));
  Df.RegisterOnChangeHook(@OnDataFileChange);

  Frame := TDesignFrame.Create(Self, Df);
  Frame.Name := 'Dataform' + IntToStr(FrameCount);
  Frame.Align := alClient;
  Frame.Parent := Self;
  DataFilesTreeView.Selected := DataFilesTreeView.Items.AddObject(nil, Frame.Name, Frame);
  Df.TreeNode := DataFilesTreeView.Selected;
end;

procedure TProjectFrame.OpenProjectActionExecute(Sender: TObject);
begin
  //
end;

procedure TProjectFrame.SaveProjectActionExecute(Sender: TObject);
begin
  Clipboard.AsText := UTF8ToSys(FEpiDocument.SaveToXml());
end;

procedure TProjectFrame.OnDataFileChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  TEpiDataFileEx(Sender).TreeNode.Text := TEpiDataFileEx(Sender).Name.Text;
end;

constructor TProjectFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FrameCount := 0;

  FrameCount := 0;
  FActiveFrame := nil;

  FEpiDocument := TEpiDocument.Create('en');
  NewDataFormAction.Execute;
end;

end.

