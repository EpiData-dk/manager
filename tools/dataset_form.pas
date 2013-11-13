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
  Forms, Controls, settings2_var, settings2, epiv_dataset_viewer_frame,
  LCLType,


  Dialogs;

const
  FormName = 'DataSetViewerForm';

type

  { TFormHandler }

  TFormHandler = class
  private
    Frame: TDatasetViewerFrame;
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ShowForm(Sender: TObject);
  end;

procedure ShowDataSetViewerForm(TheOwner: TComponent;
  const FormCaption: string; const DataFile: TEpiDataFile;
  const Records: TBoundArray; const KeyFields: TEpiFields;
  const DisplayFields: TEpiFields; const SortFieldNo: Integer;
  const ShowIndexFields: boolean);
var
  F: TForm;
  V: TDatasetViewerFrame;
  FH: TFormHandler;
  T1: TDateTime;
  T2: TDateTime;
begin
  FH := TFormHandler.Create;

  F := TForm.CreateNew(TheOwner);
  F.Caption := FormCaption;
  F.OnKeyDown := @FH.KeyDown;
  F.OnShow    := @FH.ShowForm;
  F.KeyPreview := True;

  T1 := Now;
  V := TDatasetViewerFrame.Create(F, DataFile);
  FH.Frame := V;
  T2 := Now;

//  ShowMessage('TDatasetViewerFrame.Create(F, DataFile): ' +
//    FormatDateTime('NN:SS:ZZZZ', T2-T1));

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
  FH.Free;
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

{ TFormHandler }

procedure TFormHandler.KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_ESCAPE) and (Shift = [])) or
     {$IFDEF MSWINDOWS}
     ((Key = VK_F4)  and (Shift = [ssAlt]))
     {$ENDIF}
     {$IFDEF LINUX}
     ((Key = VK_W)  and (Shift = [ssCtrl]))
     {$ENDIF}
     {$IFDEF DARWIN}
     ((Key = VK_W)  and (Shift = [ssMeta]))
     {$ENDIF}
  then
  begin
    Key := VK_UNKNOWN;
    TForm(Sender).Close;
  end;
end;

procedure TFormHandler.ShowForm(Sender: TObject);
begin
  Frame.InitVisual;
end;

end.

