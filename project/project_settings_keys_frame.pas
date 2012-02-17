unit project_settings_keys_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Buttons, StdCtrls,
  project_settings_interface, epicustombase, epidocument, epidatafiles;

type

  { TProjectSettings_KeysFrame }

  TProjectSettings_KeysFrame = class(TFrame, IProjectSettingsFrame)
    AddJumpBtn: TSpeedButton;
    Bevel4: TBevel;
    Label1: TLabel;
    RemoveJumpBtn: TSpeedButton;
    RightBevel: TBevel;
    TopBevel: TBevel;
    procedure AddJumpBtnClick(Sender: TObject);
    procedure RemoveJumpBtnClick(Sender: TObject);
  private
    { private declarations }
    FEpiDoc: TEpiDocument;
    FKeyFields: TEpiFields;
    FKeyList: TList;
    function  DoAddNewKey: TComboBox;
    procedure SetItemIndexOnField(Combo: TComboBox; Field: TEpiField);
    procedure AddFieldsToCombo(Combo: TComboBox);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure SetProjectSettings(AValue: TEpiCustomBase);
    function  ApplySettings: boolean;
  end;

implementation

{$R *.lfm}

{ TProjectSettings_KeysFrame }

procedure TProjectSettings_KeysFrame.AddJumpBtnClick(Sender: TObject);
begin
  DoAddNewKey;
end;

procedure TProjectSettings_KeysFrame.RemoveJumpBtnClick(Sender: TObject);
var
  Cmb: TComboBox;
begin
  Cmb := TComboBox(FKeyList.Last);
  FKeyList.Delete(FKeyList.Count - 1);

  if FKeyList.Count = 0  then
  begin
    AddJumpBtn.AnchorToNeighbour(akBottom, 3, TopBevel);
    RemoveJumpBtn.Enabled := false;
  end else
    AddJumpBtn.AnchorVerticalCenterTo(TControl(FKeyList.Last));
  Cmb.Free;
end;

function TProjectSettings_KeysFrame.DoAddNewKey: TComboBox;
begin
  result := TComboBox.Create(self);

  with result do
  begin
    if FKeyList.Count = 0 then
      AnchorToNeighbour(akTop, 3, TopBevel)
    else
      AnchorToNeighbour(akTop, 3, TControl(FKeyList.Last));
    AnchorParallel(akLeft, 0, TopBevel);
    AnchorToNeighbour(akRight, 5, RightBevel);
    AddFieldsToCombo(result);
    Style := csDropDownList;
    Parent := Self;
  end;
  FKeyList.Add(Result);

  AddJumpBtn.AnchorVerticalCenterTo(result);
  RemoveJumpBtn.Enabled := true;
end;

procedure TProjectSettings_KeysFrame.SetItemIndexOnField(Combo: TComboBox;
  Field: TEpiField);
var
  Idx: Integer;
begin
  Idx := Combo.Items.IndexOfObject(Field);
  if Idx <> -1 then
    Combo.ItemIndex := Idx;
end;

procedure TProjectSettings_KeysFrame.AddFieldsToCombo(Combo: TComboBox);
var
  Flds: TEpiFields;
  i: Integer;
begin
  Combo.Clear;

  Flds := FEpiDoc.DataFiles[0].Fields;
  for i := 0 to Flds.Count - 1 do
  begin
    Combo.AddItem(
      Flds[i].Name + BoolToStr(Flds[i].Question.Text <> '', ': ' + Flds[i].Question.Text, ''),
      Flds[i]
    );
  end;
end;

constructor TProjectSettings_KeysFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FKeyList := TList.Create;
end;

procedure TProjectSettings_KeysFrame.SetProjectSettings(AValue: TEpiCustomBase);
var
  i: Integer;
  Flds: TEpiFields;
begin
  FEpiDoc := TEpiDocument(AValue);
  FKeyFields := FEpiDoc.DataFiles[0].KeyFields;

  for i := 0 to FKeyFields.Count -1 do
    SetItemIndexOnField(DoAddNewKey, FKeyFields[i]);
end;

function TProjectSettings_KeysFrame.ApplySettings: boolean;
var
  i: Integer;
begin
  result := true;

  for i := 0 to FKeyList.Count - 1 do
  with TComboBox(FKeyList[i]) do
    if ItemIndex = -1 then result := false;

  if not result then exit;

  FKeyFields.Clear;
  for i := 0 to FKeyList.Count - 1 do
  with TComboBox(FKeyList[i]) do
    FKeyFields.AddItem(TEpiField(Items.Objects[ItemIndex]));
end;

end.

