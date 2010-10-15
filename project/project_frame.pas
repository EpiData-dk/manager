unit project_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, ComCtrls, ActnList,
  Controls, Dialogs, epidocument, epidatafiles, epicustombase, epiadmin;

type

  { TProjectFrame }

  TProjectFrame = class(TFrame)
    ExportStataAction: TAction;
    ProjectSettingsAction: TAction;
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
    SaveProjectToolBtn: TToolButton;
    SaveProjectAsToolBtn: TToolButton;
    ToolButton4: TToolButton;
    AddDataFormToolBtn: TToolButton;
    DeleteDataFormToolBtn: TToolButton;
    ToolButton7: TToolButton;
    procedure ExportStataActionExecute(Sender: TObject);
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
    procedure DoCloseProject;
    procedure DoCreateReleaseSections;
    procedure EpiDocumentModified(Sender: TObject);
    procedure SetModified(const AValue: Boolean);
    procedure SetOnModified(const AValue: TNotifyEvent);
    procedure UpdateCaption;
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
  design_frame, Clipbrd, settings, project_settings, epimiscutils,
  epiexport, main;

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

procedure TProjectFrame.ExportStataActionExecute(Sender: TObject);
var
  SaveDlg: TSaveDialog;
  Exporter: TEpiExport;
begin
  SaveDlg := TSaveDialog.Create(Self);
  try
    SaveDlg.Title := 'Export current form to Stata file...';
    SaveDlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    SaveDlg.Filter := GetEpiDialogFilter(false, false, false, false, false, false, true, false, false, false, false);
    SaveDlg.Options := SaveDlg.Options + [ofOverwritePrompt];
    if not SaveDlg.Execute then exit;

    // TODO : Support different datafiles export.
    Exporter := TEpiExport.Create;
    Exporter.ExportStata(SaveDlg.FileName, EpiDocument.DataFiles[0]);
  finally
    SaveDlg.Free;
    Exporter.Free;
  end;
end;

procedure TProjectFrame.OpenProjectActionExecute(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(self);
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
  Dlg.Filter := GetEpiDialogFilter(true, true, false, false, false, false,
    false, false, false, true, false);

  {$IFNDEF EPI_DEBUG}
  if MessageDlg('Warning', 'Opening project will clear all.' + LineEnding +
       'Continue?',
       mtWarning, mbYesNo, 0, mbNo) = mrNo then exit;
  {$ENDIF}

  if not Dlg.Execute then exit;
  DoOpenProject(Dlg.FileName);
  Dlg.Free;
end;

procedure TProjectFrame.ProjectSettingsActionExecute(Sender: TObject);
var
  ProjectSettings: TProjectSettingsForm;
begin
  ProjectSettings := TProjectSettingsForm.Create(self, EpiDocument.ProjectSettings);
  ProjectSettings.ShowModal;
  TDesignFrame(ActiveFrame).UpdateFrame;
  ProjectSettings.Free;
end;

procedure TProjectFrame.SaveProjectActionExecute(Sender: TObject);
begin
  if ProjectFileName = '' then
    SaveProjectAsAction.Execute
  else
    DoSaveProject(ProjectFileName);
end;

procedure TProjectFrame.SaveProjectAsActionExecute(Sender: TObject);
var
  Dlg: TSaveDialog;
begin
  Dlg := TSaveDialog.Create(Self);
  Dlg.Filter := GetEpiDialogFilter(true, true, false, false, false, false,
    false, false, false, false, false);
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
  Dlg.FilterIndex := ManagerSettings.SaveType+1;
  Dlg.Options := Dlg.Options + [ofOverwritePrompt];
  if not Dlg.Execute then exit;
  DoSaveProject(Dlg.FileName);
  Dlg.Free;
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
  Ms: TMemoryStream;
begin
  if AFileName <> ProjectFileName then
    FFileName := AFileName;
  Ms := TMemoryStream.Create;
  EpiDocument.SaveToStream(Ms);
  Ms.Position := 0;

  if ExtractFileExt(UTF8ToSys(AFileName)) = '.epz' then
    StreamToZipFile(Ms, AFileName)
  else begin
    Fs := TFileStream.Create(AFileName, fmCreate);
    Fs.CopyFrom(Ms, Ms.Size);
    Fs.Free;
  end;
  Ms.Free;
  EpiDocument.Modified := false;
  UpdateCaption;
end;

procedure TProjectFrame.DoOpenProject(AFileName: string);
var
  St: TMemoryStream;
begin
  DoCloseProject;

  St := TMemoryStream.Create;
  if ExtractFileExt(UTF8ToSys(AFileName)) = '.epz' then
    ZipFileToStream(St, AFileName)
  else
    St.LoadFromFile(AFileName);

  St.Position := 0;
  FEpiDocument := DoCreateNewDocument;
  FEpiDocument.LoadFromStream(St);
  FFileName := AFileName;
  DoNewDataForm(FEpiDocument.DataFiles[0]);
  St.Free;

  UpdateCaption;
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

procedure TProjectFrame.DoCloseProject;
begin
  if not Assigned(FEpiDocument) then exit;

  FreeAndNil(FEpiDocument);

  // TODO : Delete ALL dataforms!
  FreeAndNil(FActiveFrame);
  DataFilesTreeView.Items.Clear;
end;

procedure TProjectFrame.DoCreateReleaseSections;
var
  {$IFDEF EPI_RELEASE}
  TmpEpiSection: TEpiSection;
  i: Integer;
  H: TEpiHeading;
  {$ENDIF EPI_RELEASE}
  {$IFDEF EPI_DEBUG}
  LocalAdm: TEpiAdmin;
  Grp: TEpiGroup;
  {$ENDIF}
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
  UpdateCaption;
  if Assigned(FOnModified) then
    FOnModified(Self);
end;

procedure TProjectFrame.SetOnModified(const AValue: TNotifyEvent);
begin
  FOnModified := AValue;
end;

procedure TProjectFrame.UpdateCaption;
var
  S: String;
begin
  S := 'EpiData Manager (v' + GetManagerVersion + ')';

  if Assigned(EpiDocument) then
  begin
    S := S + ' - ' + ExtractFileName(FFileName);
    if EpiDocument.Modified then
      S := S + '*';

    if Assigned(ActiveFrame) and (TDesignFrame(ActiveFrame).DataFile.Name.Text <> '') then
      S := S + ' [' + Copy(TDesignFrame(ActiveFrame).DataFile.Name.Text, 1, 15) + ']';
  end;
  MainForm.Caption := S;
end;

constructor TProjectFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FrameCount := 0;

  FrameCount := 0;
  FActiveFrame := nil;
  FFileName := '';

  FEpiDocument := DoCreateNewDocument;
  UpdateCaption;

  {$IFDEF EPI_DEBUG}

  {$ELSE}
  ProjectPanel.Enabled := false;
  ProjectPanel.Visible := false;
  {$ENDIF}
end;

end.

