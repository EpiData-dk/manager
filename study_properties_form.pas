unit study_properties_form; 

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, MaskEdit, Buttons, epistudy;

type

  { TStudyPropertiesForm }

  TStudyPropertiesForm = class(TForm)
    AuthorEdit: TEdit;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Panel1: TPanel;
    SubjectEdit: TEdit;
    TemporalEdit: TEdit;
    OwnerEdit: TEdit;
    CreatorEdit: TEdit;
    LanguageEdit: TEdit;
    RightsholderEdit: TEdit;
    SourceEdit: TEdit;
    TitleEdit: TEdit;
    ProvenanceEdit: TEdit;
    SpatialEdit: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    CreatedEdit: TMaskEdit;
    LastModifiedEdit: TMaskEdit;
    AbstractMemo: TMemo;
    DesciptionMemo: TMemo;
    ProtocolMemo: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
  private
    FStudy: TEpiStudy;
    procedure SetStudy(const AValue: TEpiStudy);
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure SaveToStudy;
    property  Study: TEpiStudy read FStudy write SetStudy;
  end;

implementation

uses
  settings;

{ TStudyPropertiesForm }

procedure TStudyPropertiesForm.SetStudy(const AValue: TEpiStudy);
begin
  if FStudy = AValue then exit;
  FStudy := AValue;

  if not Assigned(FStudy) then exit;

  With Study do
  begin
    TitleEdit.Text := Title;
    SubjectEdit.Text := Subject;
    ProvenanceEdit.Text := Provenance;
    SourceEdit.Text := Source;
    SpatialEdit.Text := Spatial;
    TemporalEdit.Text := Temporal;

    AuthorEdit.Text := Author;
    OwnerEdit.Text := Owner;
    CreatorEdit.Text := Creator;
    RightsHolderEdit.Text := RightsHolder;

    AbstractMemo.Lines.Text := AbstractText;
    ProtocolMemo.Lines.Text := Protocol;
    DesciptionMemo.Lines.Text := Description;

    LanguageEdit.Text := Language;
    CreatedEdit.Text := FormatDateTime('yyyy-mm-dd hh:mm:ss', Created);
    LastModifiedEdit.Text := FormatDateTime('yyyy-mm-dd hh:mm:ss', ModifiedDate);
  end;
end;

constructor TStudyPropertiesForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FStudy := nil;
end;

procedure TStudyPropertiesForm.SaveToStudy;
begin
  if not Assigned(Study) then exit;

  With Study do
  begin
    Title := TitleEdit.Text;
    Subject := SubjectEdit.Text;
    Provenance := ProvenanceEdit.Text;
    Source := SourceEdit.Text;
    Spatial := SpatialEdit.Text;
    Temporal := TemporalEdit.Text;

    Author := AuthorEdit.Text;
    Owner := OwnerEdit.Text;
    Creator := CreatorEdit.Text;
    RightsHolder := RightsHolderEdit.Text;

    AbstractText := AbstractMemo.Lines.Text;
    Protocol := ProtocolMemo.Lines.Text;
    Description := DesciptionMemo.Lines.Text;

    Language := LanguageEdit.Text;
  end;
end;

initialization
  {$I study_properties_form.lrs}

end.

