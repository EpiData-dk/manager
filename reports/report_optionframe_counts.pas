unit report_optionframe_counts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, ExtCtrls,
  CheckLst, ActnList, report_types, report_base, projectfilelist_frame;

type

  { TReportOptionsFrameCounts }

  TReportOptionsFrameCounts = class(TFrame, IReportOptionFrame)
    IndexAction: TAction;
    NoneAction: TAction;
    MoveItemDown: TAction;
    MoveItemUp: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    CheckListBox1: TCheckListBox;
    Label1: TLabel;
    Panel1: TPanel;
    MoveItemUpSpdBtn: TSpeedButton;
    MoveItemDownSpdBtn: TSpeedButton;
    procedure IndexActionExecute(Sender: TObject);
    procedure MoveItemExecute(Sender: TObject);
    procedure MoveItemUpdate(Sender: TObject);
    procedure NoneActionExecute(Sender: TObject);
  private
    { private declarations }
    FOldSelected: TStrings;
    procedure PopulateAvailable(Selection: TStrings);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    function GetFrameCaption: string;
    procedure ApplyReportOptions(Report: TReportBase);
    procedure UpdateFrame(Selection: TStrings);
    function OkToAdvance(ProjectList: TProjectFileListFrame): boolean;
    function OkToAdvanceText: string;
    function CanClose: boolean;
  end;

implementation

uses
  epidocument, epidatafiles, epidatafilestypes, report_counts,
  LCLType;

{$R *.lfm}

{ TReportOptionsFrameCounts }

procedure TReportOptionsFrameCounts.MoveItemExecute(Sender: TObject);
var
  Idx1: Integer;
  Idx2: Integer;
  Chk1: Boolean;
  Chk2: Boolean;
begin
  CheckListBox1.Items.BeginUpdate;

  Idx1 := CheckListBox1.ItemIndex;
  if Sender = MoveItemUp then
    Idx2 := Idx1 - 1
  else
    Idx2 := Idx1 + 1;

  Chk1 := CheckListBox1.Checked[Idx1];
  Chk2 := CheckListBox1.Checked[Idx2];

  CheckListBox1.Items.Exchange(Idx1, Idx2);
  CheckListBox1.ItemIndex := Idx2;

  CheckListBox1.Checked[Idx1] := Chk2;
  CheckListBox1.Checked[Idx2] := Chk1;
  CheckListBox1.Items.EndUpdate;
end;

procedure TReportOptionsFrameCounts.IndexActionExecute(Sender: TObject);
var
  KeyFields: TEpiFields;
  i: Integer;
begin
  KeyFields := TEpiField(CheckListBox1.Items.Objects[0]).DataFile.KeyFields;

  CheckListBox1.Items.BeginUpdate;
  NoneAction.Execute;
  for i := 0 to CheckListBox1.Count - 1 do
    if KeyFields.FieldExists(TEpiField(CheckListBox1.Items.Objects[i])) then
      CheckListBox1.Checked[i] := true;
  CheckListBox1.Items.EndUpdate;
end;

procedure TReportOptionsFrameCounts.MoveItemUpdate(Sender: TObject);
begin
  if Sender = MoveItemUp then
    TAction(Sender).Enabled := CheckListBox1.ItemIndex > 0
  else
    TAction(Sender).Enabled := CheckListBox1.ItemIndex < (CheckListBox1.Count - 1);
  TAction(Sender).Enabled := TAction(Sender).Enabled and (CheckListBox1.ItemIndex <> -1);
end;

procedure TReportOptionsFrameCounts.NoneActionExecute(Sender: TObject);
begin
  CheckListBox1.CheckAll(cbUnchecked, false, false);
end;

procedure TReportOptionsFrameCounts.PopulateAvailable(Selection: TStrings);
var
  i, j, k: Integer;
  Doc: TEpiDocument;
  DF: TEpiDataFile;
  F: TEpiField;
  lF: TEpiField;
  Idx: Integer;
begin
  CheckListBox1.Items.BeginUpdate;
  Doc := TEpiDocument(Selection.Objects[0]);
  for i := 0 to Doc.DataFiles.Count - 1 do
  begin
    DF := Doc.DataFiles[i];
    for j := 0 to DF.Fields.Count - 1 do
    begin
      F := Df.Fields[j];
      CheckListBox1.AddItem(F.Name, F);
    end;
  end;

  for i := 0 to Selection.Count - 1 do
  begin
    Doc := TEpiDocument(Selection.Objects[i]);
    for j := 0 to Doc.DataFiles.Count - 1 do
    begin
      DF := Doc.DataFiles[j];

      for k := CheckListBox1.Count - 1 downto 0 do
      begin
        F := Df.Fields.FieldByName[CheckListBox1.Items[k]];
        if not Assigned(F) then
        begin
          CheckListBox1.Items.Delete(k);
          Continue;
        end;

        if F.FieldType <> TEpiField(CheckListBox1.Items.Objects[k]).FieldType then
        begin
          CheckListBox1.Items.Delete(k);
          Continue;
        end;
      end;
    end;
  end;
  CheckListBox1.Items.EndUpdate;
end;

constructor TReportOptionsFrameCounts.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FOldSelected := TStringList.Create;
end;

function TReportOptionsFrameCounts.GetFrameCaption: string;
begin
  result := 'Field Selection';
end;

procedure TReportOptionsFrameCounts.ApplyReportOptions(Report: TReportBase);
var
  FL: TEpiFields;
  i: Integer;
begin
  FL := TEpiFields.Create(nil);

  for i := 0 to CheckListBox1.Count - 1 do
    if CheckListBox1.Checked[i] then
      FL.AddItem(TEpiField(CheckListBox1.Items.Objects[i]));


  with TReportCounts(Report) do
    FieldList := FL;
end;

procedure TReportOptionsFrameCounts.UpdateFrame(Selection: TStrings);
begin
  PopulateAvailable(Selection);
end;

function TReportOptionsFrameCounts.OkToAdvance(
  ProjectList: TProjectFileListFrame): boolean;
begin
  result := ProjectList.SelectedList.Count > 0;
end;

function TReportOptionsFrameCounts.OkToAdvanceText: string;
begin
  result :=
    'Select at least 1 file to advance!'
end;

function TReportOptionsFrameCounts.CanClose: boolean;
begin
  result := CheckListBox1.Count > 0;
end;

end.

