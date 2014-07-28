unit append_form2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, projectfilelist_frame, epiv_dataform_treeview,
  epiv_projecttreeview_frame, epidocument, epidatafiles;

type

  { TAppendForm2 }

  TAppendForm2 = class(TForm)
    Button1: TButton;
    ButtonPanel: TPanel;
    MainPanel: TPanel;
    FileListPanel: TPanel;
    Panel2: TPanel;
    ProjectTreePanel: TPanel;
    DataFormTreePanel: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFileList: TProjectFileListFrame;
    FProjectTree: TEpiVProjectTreeViewFrame;
    FDataFormList: TDataFormTreeViewFrame;
    procedure FileListAddToGrid(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure ProjectTreeDataFileSelected(const DataFile: TEpiDataFile);
  public
    { public declarations }
  end;

var
  AppendForm2: TAppendForm2;

implementation

{$R *.lfm}

uses
  epiv_documentfile;

{ TAppendForm2 }

procedure TAppendForm2.Button1Click(Sender: TObject);
var
  Dlg: TOpenDialog;
  Docfile: TDocumentFile;
  i: Integer;
begin
  Dlg := TOpenDialog.Create(self);
  Dlg.Options := Dlg.Options + [ofAllowMultiSelect];
  if not Dlg.Execute then exit;

  for i := 0 to Dlg.Files.Count - 1 do
  begin
    Docfile := TDocumentFile.Create;
    if Docfile.OpenFile(Dlg.Files[i], true) then
      FFileList.AddDocument(DocFile);
  end;
end;

procedure TAppendForm2.FileListAddToGrid(Sender: TObject;
  Document: TEpiDocument; const Filename: string; const RowNo: Integer);
begin
  FProjectTree.AddDocument(Document);
end;

procedure TAppendForm2.FormCreate(Sender: TObject);
begin
  ButtonPanel.Caption := '';

  FFileList := TProjectFileListFrame.Create(self);
  FFileList.Align := alClient;
  FFileList.Parent := FileListPanel;
  FFileList.OnAfterAddToGrid := @FileListAddToGrid;

  FProjectTree := TEpiVProjectTreeViewFrame.Create(self);
  FProjectTree.DisplayMode := pdmCommon;
  FProjectTree.Align := alClient;
  FProjectTree.Parent := ProjectTreePanel;
  FProjectTree.AllowSelectProject := false;
  FProjectTree.CheckType := pctCascade;
  FProjectTree.EditStructure := true;
  FProjectTree.EditCaption := true;
  FProjectTree.ShowCheckBoxes := true;
  FProjectTree.ShowHint := true;

  FProjectTree.OnDataFileSelected := @ProjectTreeDataFileSelected;

  FDataFormList := TDataFormTreeViewFrame.Create(Self);
  FDataFormList.Align := alClient;
  FDataFormList.Parent := DataFormTreePanel;
end;

procedure TAppendForm2.ProjectTreeDataFileSelected(const DataFile: TEpiDataFile
  );
begin
  FDataFormList.DataFile := DataFile;
end;

end.

