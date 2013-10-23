unit dataset_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles;


procedure ShowDataSetViewerForm(TheOwner: TComponent;
  const FormCaption: string;
  Const DataFile: TEpiDataFile;
  const Records: TBoundArray = nil;
  const KeyFields: TEpiFields = nil;
  const DisplayFields: TEpiFields = nil;
  const SortFieldNo: Integer = 0; // -1 = Index | 0 = Rec no| 1+ = Field...
  const ShowIndexFields: boolean = false
  );

procedure DataSetViewerFormRestoreDefaultPos;


implementation

uses
  Forms, Controls, settings2_var, settings2, epiv_dataset_viewer_frame;

const
  FormName = 'DataSetViewerForm';

procedure ShowDataSetViewerForm(TheOwner: TComponent;
  const FormCaption: string; const DataFile: TEpiDataFile;
  const Records: TBoundArray; const KeyFields: TEpiFields;
  const DisplayFields: TEpiFields; const SortFieldNo: Integer;
  const ShowIndexFields: boolean);
var
  F: TForm;
  V: TDatasetViewerFrame;
begin
  F := TForm.CreateNew(TheOwner);
  F.Caption := FormCaption;

  V := TDatasetViewerFrame.Create(F, DataFile);
  V.Align := alClient;
  V.Parent := F;
  if Assigned(KeyFields) then
    V.KeyFields := KeyFields;
  if Assigned(Records) then
    V.ShowRecords(Records);
  if Assigned(DisplayFields) then
    V.DisplayFields := DisplayFields;

  if SortFieldNo = -1 then
    V.SortByIndexAction.Execute
  else
    V.ListGridHeaderClick(nil, true, SortFieldNo);

  if ShowIndexFields then
    V.ShowIndexOrAllFieldsAction.Execute;

  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(F, FormName);
  F.ShowModal;
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(F, FormName);
  F.Free;
end;

procedure DataSetViewerFormRestoreDefaultPos;
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 600;
  Aform.Height := 600;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, FormName);
  AForm.free;
end;

end.

