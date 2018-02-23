unit export_securitylog_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, EditBtn, ButtonPanel,
  epiopenfile;

type

  { TExportSecurityLogForm }

  TExportSecurityLogForm = class(TForm)
  private
    FLabel1: TLabel;
    FDaysEdit: TEdit;
    FDeleteLogCheckbox: TCheckBox;
    FLabel2: TLabel;
    FFilenameEdit: TFileNameEdit;
    FButtonPanel: TButtonPanel;
    FDocFile: TEpiDocumentFile;
    procedure SetDocFile(AValue: TEpiDocumentFile);
    procedure UpdateCaption;
  public
    constructor Create(TheOwner: TComponent); override;
    property DocFile: TEpiDocumentFile read FDocFile write SetDocFile;
  end;

implementation

uses
  Controls, epimiscutils, ExtCtrls;

{ TExportSecurityLogForm }

procedure TExportSecurityLogForm.SetDocFile(AValue: TEpiDocumentFile);
begin
  if FDocFile = AValue then Exit;
  FDocFile := AValue;

  UpdateCaption;
end;

procedure TExportSecurityLogForm.UpdateCaption;
begin
  if (Assigned(FDocFile)) then
    Caption := 'Export security log: ' + FDocFile.Document.Study.Title.Text;
end;

constructor TExportSecurityLogForm.Create(TheOwner: TComponent);
var
  MainPanel: TPanel;
begin
  inherited CreateNew(TheOwner);

  MainPanel          := TPanel.Create(Self);
  FLabel1            := TLabel.Create(MainPanel);
  FDaysEdit          := TEdit.Create(MainPanel);
  FLabel2            := TLabel.Create(MainPanel);
  FFilenameEdit      := TFileNameEdit.Create(MainPanel);
  FDeleteLogCheckbox := TCheckBox.Create(MainPanel);

  FButtonPanel  := TButtonPanel.Create(Self);
  FButtonPanel.Parent      := Self;
  FButtonPanel.Align       := alBottom;
  FButtonPanel.ShowButtons := [pbOK, pbCancel];
  FButtonPanel.ShowGlyphs  := [pbOK, pbCancel];

  MainPanel.Align  := alClient;
  MainPanel.Parent := Self;
  MainPanel.BevelInner := bvNone;
  MainPanel.BevelOuter := bvNone;

  FDaysEdit.Parent := MainPanel;
  FDaysEdit.Anchors := [];
  FDaysEdit.AnchorParallel(akRight, 10, MainPanel);
  FDaysEdit.AnchorParallel(akTop, 10, MainPanel);
  FDaysEdit.AnchorToNeighbour(akLeft, 10, FLabel1);
  FDaysEdit.AutoSize := true;
  FDaysEdit.Text := '';
  FDaysEdit.NumbersOnly := true;

  FLabel1.Parent := MainPanel;
  FLabel1.Anchors := [];
  FLabel1.AnchorParallel(akLeft, 10, MainPanel);
  FLabel1.AnchorParallel(akBottom, 0, FDaysEdit);
  FLabel1.Caption := 'Export log entries older than X days:';

  FDeleteLogCheckbox.Parent  := MainPanel;
  FDeleteLogCheckbox.Anchors := [];
  FDeleteLogCheckbox.AnchorToNeighbour(akTop, 15, FLabel1);
  FDeleteLogCheckbox.AnchorParallel(akLeft, 10, MainPanel);
  FDeleteLogCheckbox.Caption := 'Delete exported log from current project!';

  FLabel2.Parent := MainPanel;
  FLabel2.Anchors := [];
  FLabel2.AnchorParallel(akLeft, 10, MainPanel);
  FLabel2.AnchorToNeighbour(akTop, 15, FDeleteLogCheckbox);
  FLabel2.Caption := 'Filename:';

  FFilenameEdit.Parent      := MainPanel;
  FFilenameEdit.Anchors     := [];
  FFilenameEdit.Filter      := GetEpiDialogFilter([dfEPX, dfEPZ]);
  FFilenameEdit.DefaultExt  := GetEpiDialogFilterExt([dfEPX]);
  FFilenameEdit.DialogKind  := dkSave;
  FFilenameEdit.DialogTitle := 'Export Log Filename';
  FFilenameEdit.AnchorParallel(akLeft, 10, MainPanel);
  FFilenameEdit.AnchorParallel(akRight, 10, MainPanel);
  FFilenameEdit.AnchorToNeighbour(akTop, 10, FLabel2);


  Position := poOwnerFormCenter;
  AutoSize := true;
end;

end.

