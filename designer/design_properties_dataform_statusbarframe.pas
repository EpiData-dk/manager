unit design_properties_dataform_statusbarframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, ExtCtrls,
  epidatafiles, epitools_statusbarparser;

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
    procedure RemoveLineClick(Sender: TObject);
  private
    { Lines }
    FStatubarComponentsList: TList;
    function DoAddNewContentLine: Pointer;
    procedure UpdateLines;
  private
    { Combo/Edit }
    procedure AddTypesToCombo(Combo: TComboBox);
    procedure AddFieldsToCombo(Combo: TComboBox);
    procedure ContentDropDownSelect(Sender: TObject);
    procedure TypeDropDownSelect(Sender: TObject);
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
    FDataFile: TEpiDataFile;
    procedure SetDataFile(AValue: TEpiDataFile);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure UpdateContent;
    procedure ApplyContent;
    function  ValidateContent: Boolean;
    property  DataFile: TEpiDataFile read FDataFile write SetDataFile;
  end;

implementation

{$R *.lfm}

uses
  Dialogs;

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
  end;

  ContentEdit := TEdit.Create(Self);
  with ContentEdit do
  begin
    AnchorToNeighbour(akLeft, 10, LeftLowerBevel);
    AnchorToNeighbour(akRight, 5, RightLowerBevel);
    AnchorVerticalCenterTo(TypeDropDown);
    Text := '';
    Parent := Self;
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
  TypeDropDownSelect(TypeDropDown);
  Result := RRec;
end;

procedure TStatusbarContentFrame.UpdateLines;
begin
  FParser.ParseString(FDataFile.StatusbarContentString);
end;

procedure TStatusbarContentFrame.AddTypesToCombo(Combo: TComboBox);
begin
  Combo.Items.BeginUpdate;

  with Combo.Items do
  begin
    AddObject('Data',    TObject(1));
    AddObject('Text',    TObject(2));
    AddObject('Field',   TObject(3));
    AddObject('Caption', TObject(4));
  end;
end;

procedure TStatusbarContentFrame.AddFieldsToCombo(Combo: TComboBox);
begin

end;

procedure TStatusbarContentFrame.ContentDropDownSelect(Sender: TObject);
begin

end;

procedure TStatusbarContentFrame.TypeDropDownSelect(Sender: TObject);
var
  Combo: TComboBox absolute Sender;
  SBType: PtrInt;
  Rec: PStatusbarComponents;
begin
  SBType := PtrInt(Combo.Items.Objects[Combo.ItemIndex]);
  Rec    := PStatusbarComponents(FStatubarComponentsList[Combo.Tag]);

  if SBType = 2 then
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

procedure TStatusbarContentFrame.UpdateContentString;
begin

end;

procedure TStatusbarContentFrame.ParserIdentifier(Sender: TObject;
  IdentType: TEpiSBSIdentType; const IdentName: string);
var
  Rec: PStatusbarComponents;
  F: TEpiField;
  Idx: Integer;
begin
  Rec := PStatusbarComponents(DoAddNewContentLine);

  case IdentType of
    esiData:    Rec^.TypeDropDown.ItemIndex := 1;
    esiField:   Rec^.TypeDropDown.ItemIndex := 2;
    esiCaption: Rec^.TypeDropDown.ItemIndex := 4;
  end;
  TypeDropDownSelect(Rec^.TypeDropDown);

  F := DataFile.Fields.FieldByName[IdentName];
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
  Rec^.TypeDropDown.ItemIndex := 3;
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

constructor TStatusbarContentFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FStatubarComponentsList := TList.Create;
  FParser                 := TEpiStatusbarStringParser.Create;

  FParser.OnIdentifierFound := @ParserIdentifier;
  FParser.OnTextFound       := @ParserText;
  FParser.OnParseError      := @ParserErrorIdentifier;
end;

procedure TStatusbarContentFrame.UpdateContent;
begin
  UpdateLines;
end;

procedure TStatusbarContentFrame.ApplyContent;
begin
  DataFile.StatusbarContentString := Edit2.Text;
end;

function TStatusbarContentFrame.ValidateContent: Boolean;
begin
  result := true;
end;

end.

