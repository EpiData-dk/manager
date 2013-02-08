unit report_optionframe_counts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, ExtCtrls,
  report_types, report_base;

type

  { TReportOptionsFrameCounts }

  TReportOptionsFrameCounts = class(TFrame, IReportOptionFrame)
    Label1: TLabel;
    Label2: TLabel;
    AvailableListBox: TListBox;
    SelectedListBox: TListBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure AvailableListBoxDblClick(Sender: TObject);
    procedure AvailableListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SelectedListBoxDblClick(Sender: TObject);
    procedure SelectedListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { private declarations }
    FOldSelected: TStrings;
    procedure ClearListBoxes;
    procedure StoreSelected;
    procedure PopulateAvailable(Selection: TStrings);
    procedure RestoreSelected;
    procedure FinishListBoxes;
    procedure MoveItemsBetweenListboxes(Const FromBox, ToBox: TListBox);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    function GetFrameCaption: string;
    procedure ApplyReportOptions(Report: TReportBase);
    procedure UpdateFrame(Selection: TStrings);
    function CanClose: boolean;
  end;

implementation

uses
  epidocument, epidatafiles, epidatafilestypes, report_counts,
  LCLType;

{$R *.lfm}

{ TReportOptionsFrameCounts }

procedure TReportOptionsFrameCounts.SpeedButton1Click(Sender: TObject);
begin
  MoveItemsBetweenListboxes(AvailableListBox, SelectedListBox);
end;

procedure TReportOptionsFrameCounts.AvailableListBoxDblClick(Sender: TObject);
begin
  MoveItemsBetweenListboxes(AvailableListBox, SelectedListBox);
end;

procedure TReportOptionsFrameCounts.AvailableListBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    MoveItemsBetweenListboxes(AvailableListBox, SelectedListBox);
    Key := VK_UNKNOWN;
  end;
end;

procedure TReportOptionsFrameCounts.SelectedListBoxDblClick(Sender: TObject);
begin
  MoveItemsBetweenListboxes(SelectedListBox, AvailableListBox);
end;

procedure TReportOptionsFrameCounts.SelectedListBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    MoveItemsBetweenListboxes(SelectedListBox, AvailableListBox);
    Key := VK_UNKNOWN;
  end;
end;

procedure TReportOptionsFrameCounts.SpeedButton2Click(Sender: TObject);
begin
  MoveItemsBetweenListboxes(SelectedListBox, AvailableListBox);
end;

procedure TReportOptionsFrameCounts.ClearListBoxes;
begin
  AvailableListBox.Items.BeginUpdate;
  AvailableListBox.Clear;

  SelectedListBox.Items.BeginUpdate;
  SelectedListBox.Clear;
end;

procedure TReportOptionsFrameCounts.StoreSelected;
begin
  FOldSelected.Clear;
  FOldSelected.AddStrings(SelectedListBox.Items);
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
  Doc := TEpiDocument(Selection.Objects[i]);
  for i := 0 to Doc.DataFiles.Count - 1 do
  begin
    DF := Doc.DataFiles[i];
    for j := 0 to DF.Fields.Count - 1 do
    begin
      F := Df.Fields[j];
      AvailableListBox.AddItem(F.Name, F);
    end;
  end;

  for i := 0 to Selection.Count - 1 do
  begin
    Doc := TEpiDocument(Selection.Objects[i]);
    for j := 0 to Doc.DataFiles.Count - 1 do
    begin
      DF := Doc.DataFiles[j];

      for k := AvailableListBox.Count - 1 downto 0 do
      begin
        if not Df.Fields.ItemExistsByName(AvailableListBox.Items[k]) then
          AvailableListBox.Items.Delete(k);
      end;
{


      for k := 0 to DF.Fields.Count - 1 do
      begin
        F := Df.Fields[k];
        Idx := AvailableListBox.Items.IndexOf(F.Name);
        if Idx <> -1 then
        begin
          lF := TEpiField(AvailableListBox.Items.Objects[Idx]);
          if lF.FieldType <> F.FieldType then
            AvailableListBox.AddItem(F.Name, F);
        end else
          AvailableListBox.AddItem(F.Name, F);
      end;      }
    end;
  end;
end;

procedure TReportOptionsFrameCounts.RestoreSelected;
var
  F: TEpiField;
  i: Integer;
  S: String;
  Idx: Integer;
begin
  for i := 0 to FOldSelected.Count - 1 do
  begin
    S := FOldSelected[i];
    Idx := AvailableListBox.Items.IndexOf(S);
    if Idx <> -1 then
    begin
      F := TEpiField(AvailableListBox.Items.Objects[Idx]);
      AvailableListBox.Items.Delete(Idx);
      SelectedListBox.AddItem(F.Name, F);
    end;
  end;
end;

procedure TReportOptionsFrameCounts.FinishListBoxes;
begin
  if SelectedListBox.Count = 0 then
  begin
    // No "old" selection exists, if a KeyField exists then try to
    // apply that.
  end;

  AvailableListBox.Items.EndUpdate;
  SelectedListBox.Items.EndUpdate;
end;

procedure TReportOptionsFrameCounts.MoveItemsBetweenListboxes(const FromBox,
  ToBox: TListBox);
var
  i: Integer;
  F: TEpiField;
begin
  FromBox.Items.BeginUpdate;
  ToBox.Items.BeginUpdate;

  if FromBox.SelCount > 0 then
    for i := FromBox.Count - 1 downto 0 do
    begin
      if not FromBox.Selected[i] then
        continue;

      F := TEpiField(FromBox.Items.Objects[i]);
      FromBox.Items.Delete(i);
      ToBox.AddItem(F.Name, F);
    end;

  ToBox.Items.EndUpdate;
  FromBox.Items.EndUpdate;
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

  for i := 0 to SelectedListBox.Count - 1 do
    FL.AddItem(TEpiField(SelectedListBox.Items.Objects[i]));

  TReportCounts(Report).FieldList := FL;
end;

procedure TReportOptionsFrameCounts.UpdateFrame(Selection: TStrings);
begin
  StoreSelected;

  ClearListBoxes;
  PopulateAvailable(Selection);
  RestoreSelected;
  FinishListBoxes;
end;

function TReportOptionsFrameCounts.CanClose: boolean;
begin
  result := SelectedListBox.Count > 0;
end;

end.

