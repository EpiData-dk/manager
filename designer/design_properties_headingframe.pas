unit design_properties_headingframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  epicustombase, design_types, design_properties_baseframe;

type

  { THeadingPropertiesFrame }

  THeadingPropertiesFrame = class(TDesignPropertiesFrame, IDesignPropertiesFrame)
    NameEdit: TEdit;
    CaptionEdit: TEdit;
    Label10: TLabel;
    Label9: TLabel;
    HeadingTypeRadioGrp: TRadioGroup;
  private
    { private declarations }
    FHeadings: TEpiCustomControlItemArray;
    FLeaveAsIsObject: TObject;
    procedure UpdateVisibility;
    procedure UpdateContent;
    procedure DoUpdateCaption;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure FocusOnNewControl;
    procedure SetEpiControls(EpiControls: TEpiCustomControlItemArray);
    procedure ResetControls;
    function ApplyChanges: boolean;
  end;

implementation

{$R *.lfm}

uses
  epidatafiles, LazUTF8, epistringutils, epidatafilestypes;

{ THeadingPropertiesFrame }

procedure THeadingPropertiesFrame.UpdateVisibility;
begin
  if Length(FHeadings) > 1 then
    NameEdit.Enabled := false;
end;

procedure THeadingPropertiesFrame.UpdateContent;
var
  i: Integer;
begin
  NameEdit.Text := TEpiHeading(FHeadings[0]).Name;

  CaptionEdit.Text := TEpiHeading(FHeadings[0]).Caption.Text;
  for i := Low(FHeadings)+1 to High(FHeadings) do
    if CaptionEdit.Text <> TEpiHeading(FHeadings[i]).Caption.Text then
      begin
        CaptionEdit.Text := '';
        break;
      end;

  HeadingTypeRadioGrp.ItemIndex :=
    HeadingTypeRadioGrp.Items.IndexOfObject(TObject(PtrInt(TEpiHeading(FHeadings[0]).HeadingType)));
  for i := Low(FHeadings)+1 to High(FHeadings) do
    if HeadingTypeRadioGrp.Items.IndexOfObject(TObject(PtrInt(TEpiHeading(FHeadings[i]).HeadingType))) <> HeadingTypeRadioGrp.ItemIndex then
    begin
      HeadingTypeRadioGrp.ItemIndex := HeadingTypeRadioGrp.Items.IndexOfObject(FLeaveAsIsObject);
      Break;
    end;
end;

procedure THeadingPropertiesFrame.DoUpdateCaption;
var
  S: String;
  i: Integer;
begin
  S := FHeadings[0].Name;
  for i := 1 to High(FHeadings) do
    S := S + ', ' + FHeadings[i].Name;

  S := EpiCutString(S, 20);
  UpdateCaption('Heading Properties: ' + S);
end;

constructor THeadingPropertiesFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FLeaveAsIsObject := TObject.Create;
  HeadingTypeRadioGrp.Items.BeginUpdate;
  with HeadingTypeRadioGrp.Items do
  begin
    AddObject('Heading 1', TObject(htH1));
    AddObject('Heading 2', TObject(htH2));
    AddObject('Heading 3', TObject(htH3));
    AddObject('Heading 4', TObject(htH4));
    AddObject('Heading 5', TObject(htH5));
    AddObject('Leave As Is', FLeaveAsIsObject);
  end;
end;

destructor THeadingPropertiesFrame.Destroy;
begin
  FLeaveAsIsObject.Free;
  inherited Destroy;
end;

procedure THeadingPropertiesFrame.FocusOnNewControl;
begin
  CaptionEdit.SetFocus;
end;

procedure THeadingPropertiesFrame.SetEpiControls(
  EpiControls: TEpiCustomControlItemArray);
begin
  FHeadings := EpiControls;

  if not Assigned(FHeadings[0]) then exit;

  UpdateVisibility;
  UpdateContent;
  DoUpdateCaption;
end;

procedure THeadingPropertiesFrame.ResetControls;
begin
  UpdateContent;
end;

function THeadingPropertiesFrame.ApplyChanges: boolean;
var
  i: Integer;
begin
  result := false;

  if NameEdit.Modified then
    if not TEpiHeading(FHeadings[0]).ValidateRename(NameEdit.Text, false)
    then
      begin
        ShowHintMsg('Name already exists or invalid identifier', NameEdit);
        Exit;
      end;

  if (CaptionEdit.Modified) and
     (UTF8Length(CaptionEdit.Text) = 0)
  then
    begin
      ShowHintMsg('Empty heading not allowed...', CaptionEdit);
      Exit;
    end;

  if NameEdit.Modified
  then
    TEpiHeading(FHeadings[0]).Name := NameEdit.Text;

  if (CaptionEdit.Modified)
  then
    for i := Low(FHeadings) to High(FHeadings) do
      TEpiHeading(FHeadings[i]).Caption.Text := CaptionEdit.Text;


  if HeadingTypeRadioGrp.ItemIndex <> HeadingTypeRadioGrp.Items.IndexOfObject(FLeaveAsIsObject) then
    for i := Low(FHeadings) to High(FHeadings) do
      TEpiHeading(FHeadings[i]).HeadingType := TEpiHeadingType(PtrInt(HeadingTypeRadioGrp.Items.Objects[HeadingTypeRadioGrp.ItemIndex]));

  ShowHintMsg('', nil);
  result := true;

  DoUpdateCaption;
end;

end.

