unit project_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, ComCtrls, ActnList,
  Controls, Dialogs, epidocument, epidatafiles, epicustombase;

type

  { TProjectFrame }

  TProjectFrame = class(TFrame)
    DeleteDataFormAction: TAction;
    SaveProjectDialog: TSaveDialog;
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
    procedure SaveProjectAsActionExecute(Sender: TObject);
  private
    { private declarations }
    FFileName: string;
    FActiveFrame: TFrame;
    FrameCount: integer;
    FEpiDocument: TEpiDocument;
    procedure OnDataFileChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure DoSaveProject(AFileName: string);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property   EpiDocument: TEpiDocument read FEpiDocument;
    property   ActiveFrame: TFrame read FActiveFrame;
    property   ProjectFileName: string read FFileName;
  end;

implementation

{$R *.lfm}

uses
  design_frame, Clipbrd, settings;

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
  Df.Name.Text := 'Dataform ' + IntToStr(FrameCount);

  Frame := TDesignFrame.Create(Self, Df);
  Frame.Align := alClient;
  Frame.Parent := Self;
  DataFilesTreeView.Selected := DataFilesTreeView.Items.AddObject(nil, Df.Name.Text, Frame);
  Df.TreeNode := DataFilesTreeView.Selected;
  Df.Name.RegisterOnChangeHook(@OnDataFileChange);
end;

procedure TProjectFrame.OpenProjectActionExecute(Sender: TObject);
begin
  //
end;

procedure TProjectFrame.SaveProjectActionExecute(Sender: TObject);
begin
  if ProjectFileName = '' then
    SaveProjectAsAction.Execute
  else
    DoSaveProject(ProjectFileName);
end;

procedure TProjectFrame.SaveProjectAsActionExecute(Sender: TObject);
begin
  SaveProjectDialog.InitialDir := ManagerSettings.WorkingDirUTF8;
  if not SaveProjectDialog.Execute then exit;
  DoSaveProject(SaveProjectDialog.FileName);
end;

procedure TProjectFrame.OnDataFileChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup = eegCustomBase) and (EventType = Word(ecceText)) then
    TEpiDataFileEx(Sender).TreeNode.Text := TEpiDataFileEx(Sender).Name.Text;
end;

procedure TProjectFrame.DoSaveProject(AFileName: string);
var
  Fs: TFileStream;
  Ss: TStringStream;
begin
  if AFileName <> ProjectFileName then
    FFileName := AFileName;
  Fs := TFileStream.Create(AFileName, fmCreate);
  Ss := TStringStream.Create(FEpiDocument.SaveToXml());
  Fs.CopyFrom(Ss, Ss.Size);
  Ss.Free;
  Fs.Free;
end;

constructor TProjectFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FrameCount := 0;

  FrameCount := 0;
  FActiveFrame := nil;
  FFileName := '';

  FEpiDocument := TEpiDocument.Create('en');

  {$IFDEF EPI_DEBUG}

  {$ELSE}
  ProjectPanel.Enabled := false;
  ProjectPanel.Visible := false;
  {$ENDIF}
end;

end.

