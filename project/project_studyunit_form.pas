unit project_studyunit_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, StdCtrls, epistudy;

type

  { TStudyUnitForm }

  TStudyUnitForm = class(TForm)
    AbstractMemo: TMemo;
    AgencyEdit: TEdit;
    AuthorsMemo: TMemo;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CitationsMemo: TMemo;
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
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LanguageEdit: TEdit;
    PopulationMemo: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    PublisherEdit: TEdit;
    PurposeMemo: TMemo;
    RightsMemo: TMemo;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TimeCoverageEdit: TEdit;
    TitleEdit: TEdit;
    VersionEdit: TEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FStudy: TEpiStudy;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; StudyInfo: TEpiStudy);
    class procedure RestoreDefaultPos;
  end;

var
  StudyUnitForm: TStudyUnitForm;

implementation

{$R *.lfm}

uses
  settings2_var, settings2;

{ TStudyUnitForm }

procedure TStudyUnitForm.BitBtn1Click(Sender: TObject);
begin
  with FStudy do
  begin
    Title.Text                := TitleEdit.Text;
    Identifier                := IdentfierEdit.Text;
    // Lang?                  := LanguageEdit.Text;
    Version                   := VersionEdit.Text;
    AbstractText.Text         := AbstractMemo.Text;
    GeographicalCoverage.Text := GeoCoverageEdit.Text;
    TimeCoverage.Text         := TimeCoverageEdit.Text;
    Population.Text           := PopulationMemo.Text;
    Keywords                  := KeywordsEdit.Text;
    Purpose.Text              := PurposeMemo.Text;
    Citations.Text            := CitationsMemo.Text;
    Publisher.Text            := PublisherEdit.Text;
    Agency                    := AgencyEdit.Text;
    Author                    := AuthorsMemo.Text;
    Rights.Text               := RightsMemo.Text;
    Funding.Text              := FundingMemo.Text;
  end;
end;

procedure TStudyUnitForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'StudyUnitForm');
end;

procedure TStudyUnitForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'StudyUnitForm');
end;

constructor TStudyUnitForm.Create(TheOwner: TComponent; StudyInfo: TEpiStudy);
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
    TimeCoverageEdit.Text := TimeCoverage.Text;
    PopulationMemo.Text   := Population.Text;
    KeywordsEdit.Text     := Keywords;
    PurposeMemo.Text      := Purpose.Text;
    CitationsMemo.Text    := Citations.Text;
    PublisherEdit.Text    := Publisher.Text;
    AgencyEdit.Text       := Agency;
    AuthorsMemo.Text      := Author;
    RightsMemo.Text       := Rights.Text;
    FundingMemo.Text      := Funding.Text;
  end;

  PageControl1.ActivePageIndex := 0;
end;

class procedure TStudyUnitForm.RestoreDefaultPos;
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 500;
  Aform.Height := 500;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, 'StudyUnitForm');
  AForm.free;
end;


end.

