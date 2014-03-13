unit project_studyunit_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, EditBtn,
  project_types, epistudy;

type

  { TStudyUnitFrame }

  TStudyUnitFrame = class(TFrame, IProjectFrame)
    AbstractMemo: TMemo;
    AgencyEdit: TEdit;
    AuthorsMemo: TMemo;
    CitationsMemo: TMemo;
    DesignMemo: TMemo;
    FromDateEdit: TDateEdit;
    FundingMemo: TMemo;
    GeoCoverageEdit: TEdit;
    IdentfierEdit: TEdit;
    KeywordsEdit: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LanguageEdit: TEdit;
    PageControl1: TPageControl;
    PopulationMemo: TMemo;
    PublisherEdit: TEdit;
    PurposeMemo: TMemo;
    RightsMemo: TMemo;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TitleEdit: TEdit;
    ToDateEdit: TDateEdit;
    UnitOfObsMemo: TMemo;
    VersionEdit: TEdit;
  private
    { private declarations }
    FStudy: TEpiStudy;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; StudyInfo: TEpiStudy);
    procedure UpdateFrame;
    procedure Activate;
    function DeActivate(aHide: boolean): boolean;
  end;

implementation

{$R *.lfm}

uses
  RegExpr, Dialogs;

{ TStudyUnitFrame }

constructor TStudyUnitFrame.Create(TheOwner: TComponent; StudyInfo: TEpiStudy);
begin
  inherited Create(TheOwner);

  FStudy := StudyInfo;
  with FStudy do
  begin
    TitleEdit.Text        := Title.Text;
    IdentfierEdit.Text    := Identifier;
    // LanguageEdit.Text  := Lang?;
    VersionEdit.Text      := Version;
    AbstractMemo.Text     := AbstractText.Text;
    GeoCoverageEdit.Text  := GeographicalCoverage.Text;
//    TimeCoverageEdit.Text := TimeCoverage.Text;
    if (DataCollectionStart <> MaxDateTime) then
      FromDateEdit.Date   := DataCollectionStart;
    if (DataCollectionEnd <> MaxDateTime) then
      ToDateEdit.Date     := DataCollectionEnd;
    PopulationMemo.Text   := Population.Text;
    KeywordsEdit.Text     := Keywords;
    PurposeMemo.Text      := Purpose.Text;
    CitationsMemo.Text    := Citations.Text;
    PublisherEdit.Text    := Publisher.Text;
    DesignMemo.Text       := Design.Text;
    UnitOfObsMemo.Text    := UnitOfObservation.Text;
    AgencyEdit.Text       := Agency;
    AuthorsMemo.Text      := Author;
    RightsMemo.Text       := Rights.Text;
    FundingMemo.Text      := Funding.Text;
  end;

  PageControl1.ActivePageIndex := 0;
end;

procedure TStudyUnitFrame.UpdateFrame;
begin
  // Empty... (so far).
end;

procedure TStudyUnitFrame.Activate;
begin
  BringToFront;
end;

function TStudyUnitFrame.DeActivate(aHide: boolean): boolean;
var
  Reg: TRegExpr;

const
  Letter = '[A-Za-z]';
  Digits = '[0-9]';
begin
  Result := true;

  if AgencyEdit.Text <> '' then
    begin
      Reg := TRegExpr.Create;

      Reg.Expression :=
        '(' + Letter + '|_)' +
        '(' + Letter + '|' + Digits + '|\.|-|_)*';
      Reg.Exec(AgencyEdit.Text);

      // we do not have support for all the UTF-8 characters... but at least all ASCII
      // http://www.w3.org/TR/1999/REC-xml-names-19990114/#NT-NCName
      if (Reg.MatchLen[0] <> Length(Reg.InputString)) then
        begin
          ShowMessage(
            'Agency is not valid according to XML Standard:' + LineEnding +
            'Please visit this website for more information on NCName or use blank name:' + LineEnding +
            LineEnding +
            'http://www.w3.org/TR/1999/REC-xml-names-19990114/#NT-NCName'
          );
          result := false;
        end;
      Reg.Free;
      if not Result then exit;
    end;

  with FStudy do
  begin
    Title.Text                := TitleEdit.Text;
    Identifier                := IdentfierEdit.Text;
    // Lang?                  := LanguageEdit.Text;
    Version                   := VersionEdit.Text;
    AbstractText.Text         := AbstractMemo.Text;
    GeographicalCoverage.Text := GeoCoverageEdit.Text;
    if FromDateEdit.Date > 0 then
      DataCollectionStart := FromDateEdit.Date;
    if ToDateEdit.Date > 0 then
      DataCollectionEnd := ToDateEdit.Date;
    Population.Text           := PopulationMemo.Text;
    Keywords                  := KeywordsEdit.Text;
    Purpose.Text              := PurposeMemo.Text;
    Citations.Text            := CitationsMemo.Text;
    Publisher.Text            := PublisherEdit.Text;
    Design.Text               := DesignMemo.Text;
    UnitOfObservation.Text    := UnitOfObsMemo.Text;
    Agency                    := AgencyEdit.Text;
    Author                    := AuthorsMemo.Text;
    Rights.Text               := RightsMemo.Text;
    Funding.Text              := FundingMemo.Text;
  end;

  if aHide then
    SendToBack;
end;

end.

