unit validate_double_entry_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, CheckLst,
  ExtCtrls, ActnList, report_types, report_base, epidocument;

type

  { TValideDoubleEntryFrame }

  TValideDoubleEntryFrame = class(TFrame, IReportOptionFrame)
    Bevel1: TBevel;
    CmpFAllBtn: TButton;
    CmpFAutoDateTimeBtn: TButton;
    CmpFAutoIncBtn: TButton;
    CmpFCheckList: TCheckListBox;
    CmpFTextBtn: TButton;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    KFAutoIncBtn: TButton;
    KFCheckList: TCheckListBox;
    KFIndexBtn: TButton;
    KFNoneBtn: TButton;
    Label7: TLabel;
    Label8: TLabel;
    OptionsChkGrp: TCheckGroup;
  private
    { private declarations }
    FMainDoc: TEpiDocument;
    FDupDoc: TEpiDocument;
    procedure UpdateKeyFields;
    procedure UpdateCompareFields;
  public
    { public declarations }
    function  GetFrameCaption: string;
    procedure UpdateFrame(Selection: TStrings);
    procedure ApplyReportOptions(Report: TReportBase);
    function CanClose: boolean;
  end;

implementation

{$R *.lfm}

uses
  epidatafiles, report_double_entry_validation, epitools_val_dbl_entry;

{ TValideDoubleEntryFrame }

procedure TValideDoubleEntryFrame.UpdateKeyFields;
var
  MainDF: TEpiDataFile;
  DupDF: TEpiDataFile;
  i: Integer;
begin
  KFCheckList.Clear;

  MainDF := FMainDoc.DataFiles[0];
  DupDF  := FDupDoc.DataFiles[0];

  KFCheckList.Items.BeginUpdate;
  for i := 0 to MainDF.Fields.Count - 1 do
  begin
    if DupDF.Fields.ItemExistsByName(MainDF.Fields[i].Name) then
      KFCheckList.AddItem(MainDF.Fields[i].Name, MainDF.Fields[i]);
  end;
  KFCheckList.Items.EndUpdate;
end;

procedure TValideDoubleEntryFrame.UpdateCompareFields;
var
  MainDF: TEpiDataFile;
  DupDF: TEpiDataFile;
  i: Integer;
begin
  CmpFCheckList.Clear;

  MainDF := FMainDoc.DataFiles[0];
  DupDF  := FDupDoc.DataFiles[0];

  CmpFCheckList.Items.BeginUpdate;
  for i := 0 to MainDF.Fields.Count - 1 do
  begin
    if DupDF.Fields.ItemExistsByName(MainDF.Fields[i].Name) then
      CmpFCheckList.AddItem(MainDF.Fields[i].Name, MainDF.Fields[i]);
  end;
  CmpFCheckList.Items.EndUpdate;
end;

function TValideDoubleEntryFrame.GetFrameCaption: string;
begin
  result := 'Double Entry Validation Options';
end;

procedure TValideDoubleEntryFrame.UpdateFrame(Selection: TStrings);
begin
  if Selection.Count <> 2 then
    // Raise some error?
    Exit;

  FMainDoc := TEpiDocument(Selection.Objects[0]);
  FDupDoc  := TEpiDocument(Selection.Objects[1]);
  UpdateKeyFields;
  UpdateCompareFields;
end;

procedure TValideDoubleEntryFrame.ApplyReportOptions(Report: TReportBase);
var
  KF: TEpiFields;
  CF: TEpiFields;
  i: Integer;
  Options: TEpiToolsDblEntryValidateOptions;
begin
  KF := TEpiFields.Create(nil);
  for i := 0 to KFCheckList.Count - 1 do
    if KFCheckList.Checked[i] then
      KF.AddItem(TEpiField(KFCheckList.Items.Objects[i]));

  CF := TEpiFields.Create(nil);
  for i := 0 to CmpFCheckList.Count - 1 do
    if CmpFCheckList.Checked[i] then
      CF.AddItem(TEpiField(CmpFCheckList.Items.Objects[i]));

  Options := [];
  if OptionsChkGrp.Checked[0] then Include(Options, devIgnoreDeleted);
  if OptionsChkGrp.Checked[1] then Include(Options, devCaseSensitiveText);
  if OptionsChkGrp.Checked[2] then Include(Options, devIgnoreMissingRecords);
  if OptionsChkGrp.Checked[3] then Include(Options, devAddResultToField);

  with TReportDoubleEntryValidation(Report) do
  begin
    KeyFields := KF;
    CompareFields := CF;
    DblEntryValidateOptions := Options;
  end;
end;

function TValideDoubleEntryFrame.CanClose: boolean;
begin
  result := true;
end;

end.

