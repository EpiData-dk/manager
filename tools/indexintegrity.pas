unit indexintegrity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  epidocument, epidatafiles, datasetviewer_frame;

type

  { TIndexIntegrityForm }

  TIndexIntegrityForm = class(TForm)
  private
    { private declarations }
    FDataSetViewer: TDataSetViewFrame;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const EpiDoc: TEpiDocument);
    procedure   ShowRecords(Const Records: TBoundArray);
  end;

procedure CheckIndexIntegrity(Const EpiDoc: TEpiDocument);

implementation

uses
  epiintegritycheck, epidatafilestypes, settings2,
  settings2_var, main, LCLIntf, manager_messages, LCLType;

{$R *.lfm}

procedure CheckIndexIntegrity(const EpiDoc: TEpiDocument);
var
  Checker: TEpiIntegrityChecker;
  FailedRecords: TBoundArray;
  Res: Integer;
  F: TIndexIntegrityForm;
  V: TDataSetViewFrame;
  Fld: TEpiField;
  i: Integer;
  DoSend: Boolean;
begin
  if EpiDoc.DataFiles[0].KeyFields.Count = 0 then
  begin
    ShowMessage('No Index defined!');
    Exit;
  end;

  try
    Checker := TEpiIntegrityChecker.Create;
    if not Checker.IndexIntegrity(EpiDoc.DataFiles[0], FailedRecords) then
    begin
      Res :=
        MessageDlg('Unique Index Verification',
                   'Index is not unique:' + LineEnding +
                   IntToStr(Length(FailedRecords)) + ' non-unique records exists.' + LineEnding +
                   LineEnding +
                   'Add extra field with index information?',
                   mtWarning,
                   mbYesNoCancel, 0, mbYes);

      if res = mrCancel then exit;
      if res = mrYes then
      begin
        DoSend := false;
        Fld := EpiDoc.DataFiles[0].Fields.FieldByName['_KeyUnique'];
        if not Assigned(Fld) then
        begin
          Fld := EpiDoc.DataFiles[0].MainSection.NewField(ftInteger);
          DoSend := true;
        end;

        Fld.Name := '_KeyUnique';
        Fld.Question.Text := 'Unique Key for this record ?';
        for i := 0 to Length(FailedRecords) -1 do
          Fld.AsInteger[FailedRecords[i]] := 0;
        for i := 0 to Fld.Size -1 do
          if Fld.IsMissing[i] then
            Fld.AsInteger[i] := 1;

        if DoSend then
          SendMessage(MainForm.Handle, LM_DESIGNER_ADDFIELD, WParam(Fld), 0);
      end;

      F := nil;
      try
        F := TIndexIntegrityForm.Create(nil, EpiDoc);
        F.ShowRecords(FailedRecords);

        if ManagerSettings.SaveWindowPositions then
          LoadFormPosition(F, 'DataSetViewer');
        F.ShowModal;
        if ManagerSettings.SaveWindowPositions then
          SaveFormPosition(F, 'DataSetViewer');
      finally
        F.Free;
      end;
    end else
      ShowMessage('No records break the rule of a unique index');
  finally
    Checker.Free;
  end;
end;

{ TIndexIntegrityForm }

constructor TIndexIntegrityForm.Create(TheOwner: TComponent;
  const EpiDoc: TEpiDocument);
begin
  inherited Create(TheOwner);
  FDataSetViewer := TDataSetViewFrame.Create(Self, EpiDoc.DataFiles[0]);
  FDataSetViewer.Align := alClient;
  FDataSetViewer.Parent := Self;
end;

procedure TIndexIntegrityForm.ShowRecords(const Records: TBoundArray);
begin
  FDataSetViewer.ShowRecords(Records);
  FDataSetViewer.SortByIndexAction.Execute;
end;

end.

