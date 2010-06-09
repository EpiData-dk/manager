unit project_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, ComCtrls, ActnList,
  Controls, Dialogs, epidocument, epidatafiles, epicustombase, epiadmin;

type

  { TProjectFrame }

  TProjectFrame = class(TFrame)
    DeleteDataFormAction: TAction;
    OpenProjectDialog: TOpenDialog;
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
    SaveProjectToolBtn: TToolButton;
    SaveProjectAsToolBtn: TToolButton;
    ToolButton4: TToolButton;
    AddDataFormToolBtn: TToolButton;
    DeleteDataFormToolBtn: TToolButton;
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
    function  DoCreateNewDocument: TEpiDocument;
    function  NewDataFileItem(Sender: TEpiCustomList; DefaultItemClass: TEpiCustomItemClass): TEpiCustomItemClass;
    procedure DoSaveProject(AFileName: string);
    procedure DoOpenProject(AFileName: string);
    procedure DoNewDataForm(Df: TEpiDataFile);

    procedure DoCreateReleaseSections;
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
  design_frame, Clipbrd, settings, rttiutils, typinfo,
  main;

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
  Df: TEpiDataFile;
  Frame: TDesignFrame;
begin
  inc(FrameCount);

  Df := EpiDocument.DataFiles.NewDataFile;
  Df.Name.Text := 'Dataform ' + IntToStr(FrameCount);
  DoCreateReleaseSections;

  DoNewDataForm(Df);
end;

procedure TProjectFrame.OpenProjectActionExecute(Sender: TObject);
begin
  OpenProjectDialog.InitialDir := ManagerSettings.WorkingDirUTF8;
  if not OpenProjectDialog.Execute then exit;
  DoOpenProject(OpenProjectDialog.FileName);
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
var
  Df: TEpiDataFileEx;
begin
  if (EventGroup = eegCustomBase) and (EventType = Word(ecceText)) then
  begin
    Df := TEpiDataFileEx(TEpiCustomNamedItem(Sender).Owner);
    Df.TreeNode.Text := Df.Name.Text;
  end;
end;

function TProjectFrame.DoCreateNewDocument: TEpiDocument;
begin
  Result := TEpiDocument.Create('en');
  Result.DataFiles.OnNewItemClass := @NewDataFileItem;
end;

function TProjectFrame.NewDataFileItem(Sender: TEpiCustomList;
  DefaultItemClass: TEpiCustomItemClass): TEpiCustomItemClass;
begin
  result := TEpiDataFileEx;
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

procedure TProjectFrame.DoOpenProject(AFileName: string);
var
  Frame: TDesignFrame;
begin
  FEpiDocument.Free;

  // TODO : Delete ALL dataforms!
  FActiveFrame.Free;
  DataFilesTreeView.Items.Clear;

  FEpiDocument := DoCreateNewDocument;
  FEpiDocument.LoadFromFile(AFileName);
  DoNewDataForm(FEpiDocument.DataFiles[0]);
end;

procedure TProjectFrame.DoNewDataForm(Df: TEpiDataFile);
var
  Frame: TDesignFrame;
begin
  Frame := TDesignFrame.Create(Self);
  Frame.Align := alClient;
  Frame.Parent := Self;
  Frame.DataFile := Df;
  FActiveFrame := Frame;

  DataFilesTreeView.Selected := DataFilesTreeView.Items.AddObject(nil, Df.Name.Text, Frame);
  TEpiDataFileEx(Df).TreeNode := DataFilesTreeView.Selected;
  Df.Name.RegisterOnChangeHook(@OnDataFileChange);
end;

procedure TProjectFrame.DoCreateReleaseSections;
var
  TmpEpiSection: TEpiSection;
  i: Integer;
  H: TEpiHeading;
  LocalAdm: TEpiAdmin;
  Grp: TEpiGroup;
begin
  {$IFDEF EPI_RELEASE}
  with FEpiDocument.DataFiles[0] do
  begin
    TmpEpiSection := NewSection;
    TmpEpiSection.Name.Text := 'This is a test module for EpiData Manager';
    TmpEpiSection.Top := 5;
    TmpEpiSection.Left := 20;
    TmpEpiSection.Width := {$IFDEF WINDOWS}600{$ELSE}700{$ENDIF};
    TmpEpiSection.Height := 310;

    for i := 1 to 14 do
    begin
      H := TmpEpiSection.NewHeading;
      H.Left := 20;
      H.Top  := 20 * (i - 1) + 5;

      if (i >= 4) and (i <= 10) then
        H.Left := 30;
      if (i = 13) then
        H.Left := 70;

      case i of
         1: H.Caption.Text := 'Comment and discuss on the epidata-list, see Http://www.epidata.dk.';
         2: H.Caption.Text := 'Please test: add fields, headings and sections. Import old datafiles. Save Project';
         3: H.Caption.Text := '========================================================';
         4: H.Caption.Text := 'A: Add fields and sections - click on buttons above and click in the form';
         5: H.Caption.Text := 'B: Move fields/headings into and out of sections.';
         6: H.Caption.Text := 'C: Change or delete fields, sections & headings (red "X"/"DEL" key/pencil).';
         7: H.Caption.Text := 'D: Edit fields, sections or headings (using "pencil" or "ENTER" key)';
         8: H.Caption.Text := '   NEW in this version:';
         9: H.Caption.Text := 'E: Statusbar, Default working directory in settings.';
        10: H.Caption.Text := 'F: Import datafile structure, Save and Read Project files (see menu - file)';
        11: H.Caption.Text := '========================================================';
        12: H.Caption.Text := 'NOTE 1): A section is a subdevision of a data entry form.';
        13: H.Caption.Text := 'Later restricted access (via password) can be tied to section level';
        14: H.Caption.Text := 'NOTE 2): Export is NOT part of this test release.';
      end;
    end;

    TmpEpiSection := NewSection;
    TmpEpiSection.Name.Text := 'Known major bugs:';
    TmpEpiSection.Top := 315;
    TmpEpiSection.Left := 20;
    TmpEpiSection.Width := {$IFDEF WINDOWS}600{$ELSE}700{$ENDIF};
    TmpEpiSection.Height := 100;

    for i := 1 to 3 do
    begin
      H := TmpEpiSection.NewHeading;
      H.Left := 30;
      H.Top  := 20 * (i - 1) + 5;
      if (i = 2) then
        H.Left := 45;
      case i of
        1: H.Caption.Text := 'A: On creating a new section dragging the cursor outside the program and';
        2: H.Caption.Text := 'releasing the button, can cause the drawn area not to disapear. (Windows only)';
        3: H.Caption.Text := 'B: Dragging fields/headings/sections in Mac OS X may not always work.';
      end;
    end;
  end;
  {$ENDIF}

  {$IFDEF EPI_DEBUG}
  // DEBUGGING!!!!
  LocalAdm := FEpiDocument.Admin;
  Grp := LocalAdm.NewGroup;
  Grp.Name.Text := 'Group 1';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  Grp := LocalAdm.NewGroup;
  Grp.Name.Text := 'Group 2';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  Grp := LocalAdm.NewGroup;
  Grp.Name.Text := 'Group 3';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  {$ENDIF}
end;

constructor TProjectFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FrameCount := 0;

  FrameCount := 0;
  FActiveFrame := nil;
  FFileName := '';

  FEpiDocument := DoCreateNewDocument;

  {$IFDEF EPI_DEBUG}

  {$ELSE}
  ProjectPanel.Enabled := false;
  ProjectPanel.Visible := false;
  {$ENDIF}
end;

end.

