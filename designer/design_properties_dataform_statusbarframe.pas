unit design_properties_dataform_statusbarframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, ExtCtrls,
  epidatafiles, epidatafilerelations, epitools_statusbarparser;

type

  { TStatusbarContentFrame }

  TStatusbarContentFrame = class(TFrame)
    RelateOrderLabel1: TLabel;
    GotoDataformLabel1: TLabel;
    AddNewLine: TSpeedButton;
    RemoveLine: TSpeedButton;
    LeftLowerBevel: TBevel;
    RightLowerBevel: TBevel;
    TopBevel: TBevel;
    LeftUpperBevel: TBevel;
    RightUpperBevel: TBevel;
    GroupBox1: TGroupBox;
    Edit2: TEdit;
    procedure AddNewLineClick(Sender: TObject);
    procedure Edit2EditingDone(Sender: TObject);
    procedure RemoveLineClick(Sender: TObject);
  private
    { Lines }
    FStatubarComponentsList: TList;
    function  DoAddNewContentLine: Pointer;
    procedure ClearGUI;
    procedure UpdateGUI;
  private
    { Combo/Edit }
    procedure AddTypesToCombo(Combo: TComboBox);
    procedure AddFieldsToCombo(Combo: TComboBox);
    procedure ContentDropDownSelect(Sender: TObject);
    procedure TypeDropDownSelect(Sender: TObject);
    procedure UpdateContentType(TypeCombo: TComboBox);
    procedure ContentEditDone(Sender: TObject);
  private
    { Content String }
    procedure UpdateContentString;
  private
    { Parser }
    FParser: TEpiStatusbarStringParser;
    procedure ParserIdentifier(Sender: TObject; IdentType: TEpiSBSIdentType;
      const IdentName: string);
    procedure ParserText(Sender: TObject; const S: string);
    procedure ParserErrorIdentifier(Sender: TObject; CaretPos: Integer;
      const Msg: String);
  private
    { Other }
    FValidated: boolean;
    FDataFile: TEpiDataFile;
    FRelation: TEpiMasterRelation;
    procedure SetDataFile(AValue: TEpiDataFile);
    procedure SetRelation(AValue: TEpiMasterRelation);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure UpdateContent;
    procedure ApplyContent;
    function  ValidateContent: Boolean;
//    property  DataFile: TEpiDataFile read FDataFile write SetDataFile;
    property  Relation: TEpiMasterRelation read FRelation write SetRelation;
  end;

implementation

{$R *.lfm}

uses
  Dialogs, strutils;

type
  TStatusbarComponents = record
    TypeDropDown: TComboBox;
    ContentDropDown: TComboBox;
    ContentEdit: TEdit;
  end;
  PStatusbarComponents = ^TStatusbarComponents;

{ TStatusbarContentFrame }

procedure TStatusbarContentFrame.AddNewLineClick(Sender: TObject);
begin
  DoAddNewContentLine;
  if (FDataFile.Fields.Count > 0) then
    UpdateContentString;
end;

procedure TStatusbarContentFrame.Edit2EditingDone(Sender: TObject);
begin
  if TEdit(Sender).Modified then
    UpdateGUI;
end;

procedure TStatusbarContentFrame.RemoveLineClick(Sender: TObject);
begin
//  DoDeleteContentLine;
end;

function TStatusbarContentFrame.DoAddNewContentLine: Pointer;
var
  TypeDropDown: TComboBox;
  ContentDropDown: TComboBox;
  ContentEdit: TEdit;
  RRec: PStatusbarComponents;
begin
  TypeDropDown := TComboBox.Create(Self);
  with TypeDropDown do
  begin
    if FStatubarComponentsList.Count = 0 then
      AnchorToNeighbour(akTop, 5, TopBevel)
    else
      AnchorToNeighbour(akTop, 5, PStatusbarComponents(FStatubarComponentsList[FStatubarComponentsList.Count-1])^.TypeDropDown);
    AnchorParallel(akLeft, 0, TopBevel);
    AnchorToNeighbour(akRight, 5, LeftLowerBevel);
    AddTypesToCombo(TypeDropDown);
    Style := csDropDownList;
    Parent := Self;
    OnSelect := @TypeDropDownSelect;
    ItemIndex := 0;
  end;

  ContentDropDown := TComboBox.Create(Self);
  with ContentDropDown do
  begin
    AnchorToNeighbour(akLeft, 10, LeftLowerBevel);
    AnchorToNeighbour(akRight, 5, RightLowerBevel);
    AnchorVerticalCenterTo(TypeDropDown);
    AddFieldsToCombo(ContentDropDown);
    Style := csDropDownList;
    Parent := Self;
    OnSelect := @ContentDropDownSelect;
    ItemIndex := 0;
  end;

  ContentEdit := TEdit.Create(Self);
  with ContentEdit do
  begin
    AnchorToNeighbour(akLeft, 10, LeftLowerBevel);
    AnchorToNeighbour(akRight, 5, RightLowerBevel);
    AnchorVerticalCenterTo(TypeDropDown);
    Text := '';
    Parent := Self;
    OnEditingDone := @ContentEditDone;
  end;

  AddNewLine.AnchorVerticalCenterTo(TypeDropDown);
  RemoveLine.Enabled := true;

  TypeDropDown.Tag      := FStatubarComponentsList.Count;
  TypeDropDown.TabOrder := (FStatubarComponentsList.Count * 3);
  ContentDropDown.TabOrder := (FStatubarComponentsList.Count * 3) + 1;
  ContentEdit.TabOrder := (FStatubarComponentsList.Count * 3) + 2;

  RRec := New(PStatusbarComponents);
  RRec^.TypeDropDown    := TypeDropDown;
  RRec^.ContentDropDown := ContentDropDown;
  RRec^.ContentEdit     := ContentEdit;

  FStatubarComponentsList.Add(RRec);
  UpdateContentType(TypeDropDown);
  Result := RRec;
end;

procedure TStatusbarContentFrame.ClearGUI;
begin
  // Clear all previous visual controls
  // - remove akTop, since AnchorVertical uses akTop and not akBottom.
  AddNewLine.Anchors := AddNewLine.Anchors - [akTop];
  AddNewLine.AnchorToNeighbour(akBottom, 3, TopBevel);
  RemoveLine.Enabled := False;
  while FStatubarComponentsList.Count > 0 do
    with PStatusbarComponents(FStatubarComponentsList.Last)^ do
    begin
      TypeDropDown.Free;
      ContentDropDown.Free;
      ContentEdit.Free;
      FStatubarComponentsList.Delete(FStatubarComponentsList.Count-1);
    end;
end;

procedure TStatusbarContentFrame.UpdateGUI;
begin
  ClearGUI;
  FValidated := FParser.ParseString(Edit2.Text);
  UpdateContentString;
end;

procedure TStatusbarContentFrame.AddTypesToCombo(Combo: TComboBox);
begin
  Combo.Items.BeginUpdate;

  with Combo.Items do
  begin
    AddObject('Data',    TObject(0));
    AddObject('Text',    TObject(1));
    AddObject('Variable',TObject(2));
    AddObject('Caption', TObject(3));
  end;
end;

procedure TStatusbarContentFrame.AddFieldsToCombo(Combo: TComboBox);
var
  F: TEpiField;
  R: TEpiMasterRelation;
  DF: TEpiDataFile;
  S: String;
begin
  Combo.Items.BeginUpdate;

  R := Relation;
  while Assigned(R) do
    begin
      DF := R.Datafile;

      if (DF <> FDataFile) then
        S := DF.Name + '.';

      for F in DF.Fields do
        Combo.AddItem(S + F.Name, F);

      if (R is TEpiDetailRelation) then
        R := TEpiDetailRelation(R).MasterRelation
      else
        R := nil;
    end;

  Combo.Items.EndUpdate;
end;

procedure TStatusbarContentFrame.ContentDropDownSelect(Sender: TObject);
begin
  UpdateContentString;
end;

procedure TStatusbarContentFrame.TypeDropDownSelect(Sender: TObject);
begin
  UpdateContentType(TComboBox(Sender));
  UpdateContentString;
end;

procedure TStatusbarContentFrame.UpdateContentType(TypeCombo: TComboBox);
var
  SBType: PtrInt;
  Rec: PStatusbarComponents;
begin
  SBType := PtrInt(TypeCombo.Items.Objects[TypeCombo.ItemIndex]);
  Rec    := PStatusbarComponents(FStatubarComponentsList[TypeCombo.Tag]);

  if SBType = 1 then
    begin
      Rec^.ContentEdit.Visible := true;
      Rec^.ContentDropDown.Visible := false;
    end
  else
    begin
      Rec^.ContentEdit.Visible := False;
      Rec^.ContentDropDown.Visible := True;
    end;
end;

procedure TStatusbarContentFrame.ContentEditDone(Sender: TObject);
begin
  UpdateContentString;
end;

procedure TStatusbarContentFrame.UpdateContentString;
var
  S: String;
  I: Integer;
  Rec: PStatusbarComponents;

  function AdjustedFieldName(F: TEpiField): String;
  begin
    Result := F.Name;
    if (F.DataFile <> FDataFile) then
      Result := F.DataFile.Name + '.' + Result;
  end;

begin
  S := '';
  for I := 0 to FStatubarComponentsList.Count - 1 do
    begin
      Rec := PStatusbarComponents(FStatubarComponentsList[i]);
      with Rec^ do
      begin
        case TypeDropDown.ItemIndex of
          0: S += '%d(' + AdjustedFieldName(TEpiField(ContentDropDown.Items.Objects[ContentDropDown.ItemIndex])) + ')';
          1: S += ContentEdit.Text;
          2: S += '%f(' + AdjustedFieldName(TEpiField(ContentDropDown.Items.Objects[ContentDropDown.ItemIndex])) + ')';
          3: S += '%c(' + AdjustedFieldName(TEpiField(ContentDropDown.Items.Objects[ContentDropDown.ItemIndex])) + ')';
        end;
      end;
    end;
  Edit2.Text := S;
end;

procedure TStatusbarContentFrame.ParserIdentifier(Sender: TObject;
  IdentType: TEpiSBSIdentType; const IdentName: string);
var
  Rec: PStatusbarComponents;
  F: TEpiField;
  Idx: Integer;
  DFName, FieldName: String;
  R: TEpiMasterRelation;
begin
  Rec := PStatusbarComponents(DoAddNewContentLine);

  case IdentType of
    esiData:    Rec^.TypeDropDown.ItemIndex := 0;
    esiField:   Rec^.TypeDropDown.ItemIndex := 2;
    esiCaption: Rec^.TypeDropDown.ItemIndex := 3;
  end;
  TypeDropDownSelect(Rec^.TypeDropDown);

  if Pos('.', IdentName) > 0 then
    begin
      DFName := ExtractWord(1, IdentName, ['.']);
      FieldName  := ExtractWord(2, IdentName, ['.']);
      R := Relation;

      while Assigned(R) do
        begin
          if (R.Datafile.Name = DFName) then Break;

          if (R is TEpiDetailRelation) then
            R := TEpiDetailRelation(R).MasterRelation
          else
            R := nil;
        end;
      F := R.Datafile.Fields.FieldByName[FieldName];
    end
  else
    F := FDataFile.Fields.FieldByName[IdentName];

  if Assigned(F) then
  begin
    Idx := Rec^.ContentDropDown.Items.IndexOfObject(F);
    Rec^.ContentDropDown.ItemIndex := Idx;
  end;
end;

procedure TStatusbarContentFrame.ParserText(Sender: TObject; const S: string);
var
  Rec: PStatusbarComponents;
begin
  Rec := PStatusbarComponents(DoAddNewContentLine);
  Rec^.TypeDropDown.ItemIndex := 1;
  TypeDropDownSelect(Rec^.TypeDropDown);

  Rec^.ContentEdit.Text := S;
end;

procedure TStatusbarContentFrame.ParserErrorIdentifier(Sender: TObject;
  CaretPos: Integer; const Msg: String);
begin
  ShowMessage(Msg);
end;

procedure TStatusbarContentFrame.SetDataFile(AValue: TEpiDataFile);
begin
  if FDataFile = AValue then Exit;
  FDataFile := AValue;

  UpdateContent;
end;

procedure TStatusbarContentFrame.SetRelation(AValue: TEpiMasterRelation);
begin
  if FRelation = AValue then Exit;
  FRelation := AValue;

  SetDataFile(AValue.Datafile);
end;

constructor TStatusbarContentFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FStatubarComponentsList := TList.Create;
  FValidated              := true;

  FParser                 := TEpiStatusbarStringParser.Create;
  FParser.OnIdentifierFound := @ParserIdentifier;
  FParser.OnTextFound       := @ParserText;
  FParser.OnParseError      := @ParserErrorIdentifier;
end;

procedure TStatusbarContentFrame.UpdateContent;
begin
  Edit2.Text := FDataFile.StatusbarContentString;
  UpdateGUI;
end;

procedure TStatusbarContentFrame.ApplyContent;
begin
  FDataFile.StatusbarContentString := Edit2.Text;
end;

function TStatusbarContentFrame.ValidateContent: Boolean;
begin
  UpdateGUI;
  result := FValidated;
end;

end.

