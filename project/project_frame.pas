unit project_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, ComCtrls, ActnList,
  Controls, Dialogs, epidocument, epidatafiles, epicustombase, epiadmin;

type

  { TProjectFrame }

  TProjectFrame = class(TFrame)
    ProjectSettingsAction: TAction;
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
    procedure ProjectSettingsActionExecute(Sender: TObject);
    procedure SaveProjectActionExecute(Sender: TObject);
    procedure SaveProjectAsActionExecute(Sender: TObject);
  private
    { private declarations }
    FFileName: string;
    FActiveFrame: TFrame;
    FModified: Boolean;
    FOnModified: TNotifyEvent;
    FrameCount: integer;
    FEpiDocument: TEpiDocument;
    procedure OnDataFileChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    function  DoCreateNewDocument: TEpiDocument;
    function  NewDataFileItem(Sender: TEpiCustomList; DefaultItemClass: TEpiCustomItemClass): TEpiCustomItemClass;
    procedure DoSaveProject(AFileName: string);
    procedure DoOpenProject(AFileName: string);
    procedure DoNewDataForm(Df: TEpiDataFile);
    procedure DoCreateReleaseSections;
    procedure EpiDocumentModified(Sender: TObject);
    procedure SetModified(const AValue: Boolean);
    procedure SetOnModified(const AValue: TNotifyEvent);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property   EpiDocument: TEpiDocument read FEpiDocument;
    property   ActiveFrame: TFrame read FActiveFrame;
    property   ProjectFileName: string read FFileName;
    property   Modified: Boolean read FModified write SetModified;
    property   OnModified: TNotifyEvent read FOnModified write SetOnModified;
  end;

implementation

{$R *.lfm}

uses
  design_frame, Clipbrd, settings, rttiutils, typinfo,
  main, project_settings;

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
  EpiDocument.Modified := false;

  DoNewDataForm(Df);
end;

procedure TProjectFrame.OpenProjectActionExecute(Sender: TObject);
begin
  OpenProjectDialog.InitialDir := ManagerSettings.WorkingDirUTF8;

  {$IFNDEF EPI_DEBUG}
  if MessageDlg('Warning', 'Opening project will clear all.' + LineEnding +
       'Continue?',
       mtWarning, mbYesNo, 0, mbNo) = mrNo then exit;
  {$ENDIF}

  if not OpenProjectDialog.Execute then exit;
  DoOpenProject(OpenProjectDialog.FileName);
end;

procedure TProjectFrame.ProjectSettingsActionExecute(Sender: TObject);
var
  ProjectSettings: TProjectSettingsForm;
begin
  ProjectSettings := TProjectSettingsForm.Create(self, EpiDocument.ProjectSettings);
  ProjectSettings.ShowModal;
  TDesignFrame(ActiveFrame).UpdateFrame;
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
    Df := TEpiDataFileEx(TEpiCustomItem(Sender).Owner);
    Df.TreeNode.Text := Df.Name.Text;
  end;
end;

function TProjectFrame.DoCreateNewDocument: TEpiDocument;
begin
  Result := TEpiDocument.Create('en');
  Result.DataFiles.OnNewItemClass := @NewDataFileItem;
  Result.OnModified := @EpiDocumentModified;
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
  Ss := TStringStream.Create(EpiDocument.SaveToXml());
  Fs.CopyFrom(Ss, Ss.Size);
  Ss.Free;
  Fs.Free;
  EpiDocument.Modified := false;
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
    TmpEpiSection.Height := 315;

    for i := 1 to 14 do
    begin
      H := TmpEpiSection.NewHeading;
      H.Left := 20;
      H.Top  := 20 * (i - 1) + 5;

      if (i >= 4) and (i <= 10) then
        H.Left := 30;
      if (i = 13) then
        H.Left := {$IFDEF WINDOWS}70{$ELSE}90{$ENDIF};

      case i of
         1: H.Caption.Text := 'Comment and discuss on the epidata-list, see http://www.epidata.dk.';
         2: H.Caption.Text := 'Please test: add fields, headings and sections. Import old datafiles.';
         3: H.Caption.Text := '========================================================';
         4: H.Caption.Text := 'A: Add fields and sections - click on buttons above and click in the form';
         5: H.Caption.Text := 'B: Move fields/headings into and out of sections.';
         6: H.Caption.Text := 'C: Edit or delete fields, sections & headings (red "X"/"DEL"/"ENTER" key/pencil).';
         7: H.Caption.Text := 'D: Open/Save projects in new EpiData XML File format. (See "File" menu)';
         8: H.Caption.Text := ' -- NEW in this version --';
         9: H.Caption.Text := 'E: Paste text as fields - (See "Edit" menu or right click with mouse)';
        10: H.Caption.Text := 'F: Only unique field names allowed.';
        11: H.Caption.Text := '========================================================';
        12: H.Caption.Text := 'NOTE 1): A section is a subdevision of a data entry form.';
        13: H.Caption.Text := 'Later restricted access (via password) can be tied to section level';
        14: H.Caption.Text := 'NOTE 2): Export is NOT part of this test release.';
      end;
    end;

    TmpEpiSection := NewSection;
    TmpEpiSection.Name.Text := 'Known major bugs in EpiData Manager:';
    TmpEpiSection.Top := 335;
    TmpEpiSection.Left := 20;
    TmpEpiSection.Width := {$IFDEF WINDOWS}600{$ELSE}700{$ENDIF};
    TmpEpiSection.Height := 110;

    for i := 1 to 4 do
    begin
      H := TmpEpiSection.NewHeading;
      H.Left := 30;
      H.Top  := 20 * (i - 1) + 5;
      if (i = 2) or (i = 5) then
        H.Left := {$IFDEF WINDOWS}45{$ELSE}50{$ENDIF};
      case i of
        1: H.Caption.Text := 'A: On creating a new section dragging the cursor outside the program and';
        2: H.Caption.Text := 'releasing the button, can cause the drawn area not to disapear. (Windows only)';
        3: H.Caption.Text := 'B: Dragging fields/headings/sections in Mac OS X may not always work.';
        4: H.Caption.Text := 'C: Dragging field/heading within same section does strange things.';
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

procedure TProjectFrame.EpiDocumentModified(Sender: TObject);
begin
  Modified := TEpiDocument(Sender).Modified;
end;

procedure TProjectFrame.SetModified(const AValue: Boolean);
begin
  if FModified = AValue then exit;
  FModified := AValue;
  if Assigned(FOnModified) then
    FOnModified(Self);
end;

procedure TProjectFrame.SetOnModified(const AValue: TNotifyEvent);
begin
  if FOnModified = AValue then exit;
  FOnModified := AValue;
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

