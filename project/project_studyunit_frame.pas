unit project_studyunit_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, EditBtn,
  Buttons, project_types, epistudy, epicustombase;

type

  { TStudyUnitFrame }

  TStudyUnitFrame = class(TFrame, IProjectFrame)
    AbstractMemo: TMemo;
    AgencyEdit: TEdit;
    AuthorsMemo: TMemo;
    BitBtn1: TBitBtn;
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
    Memo1: TMemo;
    PageControl1: TPageControl;
    PopulationMemo: TMemo;
    PublisherEdit: TEdit;
    PurposeMemo: TMemo;
    RightsMemo: TMemo;
    TitleSheet: TTabSheet;
    WelcomeSheet: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TitleEdit: TEdit;
    ToDateEdit: TDateEdit;
    UnitOfObsMemo: TMemo;
    VersionEdit: TEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure StudyEditingDone(Sender: TObject);
    procedure TitleEditEditingDone(Sender: TObject);
  private
    { private declarations }
    FStudy: TEpiStudy;
    procedure StudyHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure StudyTitleHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure RegisterHooks;
    procedure UnRegisterHooks;
    function  ValidateTitle: boolean;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; StudyInfo: TEpiStudy;
      Const IsNew: boolean = false);
    destructor Destroy; override;
    procedure UpdateFrame;
    procedure Activate;
    function DeActivate(aHide: boolean): boolean;
    procedure AssignActionLinks;
  end;

implementation

{$R *.lfm}

uses
  RegExpr, Dialogs, main;

{ TStudyUnitFrame }

procedure TStudyUnitFrame.BitBtn1Click(Sender: TObject);
begin
  PageControl1.ActivePage := TitleSheet;
  WelcomeSheet.TabVisible := False;
end;

procedure TStudyUnitFrame.StudyEditingDone(Sender: TObject);
begin
  FStudy.Modified := true;
end;

procedure TStudyUnitFrame.TitleEditEditingDone(Sender: TObject);
begin
  if not ValidateTitle then exit;

  StudyEditingDone(Sender);
  FStudy.Title.Text := TitleEdit.Text;
end;

procedure TStudyUnitFrame.StudyHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if Initiator <> FStudy.Title then exit;
  if EventGroup <> eegCustomBase then exit;

  if TEpiCustomChangeEventType(EventType) = ecceDestroy then
    begin
      UnRegisterHooks;
      FStudy := nil;
    end;
end;

procedure TStudyUnitFrame.StudyTitleHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if Initiator <> FStudy.Title then exit;
  if EventGroup <> eegCustomBase then exit;

  if TEpiCustomChangeEventType(EventType) = ecceText then
    begin
      TitleEdit.Text := FStudy.Title.Text;
    end;
end;

procedure TStudyUnitFrame.RegisterHooks;
begin
  FStudy.Title.RegisterOnChangeHook(@StudyTitleHook, true);
  FStudy.RegisterOnChangeHook(@StudyHook, true);
end;

procedure TStudyUnitFrame.UnRegisterHooks;
begin
  if Assigned(FStudy) then
    begin
      FStudy.Title.UnRegisterOnChangeHook(@StudyTitleHook);
      FStudy.UnRegisterOnChangeHook(@StudyHook);
    end;
end;

function TStudyUnitFrame.ValidateTitle: boolean;
begin
  Result := true;

  if trim(TitleEdit.Text) = '' then
    begin
      ShowMessage('Title cannot be empty!');
      Result := false;
      Exit;
    end;
end;

constructor TStudyUnitFrame.Create(TheOwner: TComponent; StudyInfo: TEpiStudy;
  const IsNew: boolean);
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

  if IsNew then
    PageControl1.ActivePage := WelcomeSheet
  else begin
    WelcomeSheet.TabVisible := false;
    PageControl1.ActivePage := TitleSheet;
  end;

  // Hooks
  RegisterHooks;

  {$IFDEF darwin}
  PageControl1.TabPosition := tpTop;
  {$ENDIF}
end;

destructor TStudyUnitFrame.Destroy;
begin
  UnRegisterHooks;
  inherited Destroy;
end;

procedure TStudyUnitFrame.UpdateFrame;
begin
  // Empty... (so far).
end;

procedure TStudyUnitFrame.Activate;
begin
//  BringToFront;
  Show;
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
          PageControl1.ActivePage := TabSheet5;
          AgencyEdit.SetFocus;

          ShowMessage(
            'The "Agency" specified in ownership as part of' + LineEnding +
            'Study Information does not conform to the w3 standard.' + LineEnding +
            'Please correct or leave it blank.' + LineEnding +
            LineEnding +
            'A simplified rule is restrict content to:' + LineEnding +
            ' a..z, A..Z, 0...9 and _ - /' + LineEnding +
            LineEnding +
            'See:' + LineEnding +
            'http://www.w3.org/TR/1999/REC-xml-names-19990114/#NT-NCName'
          );
          result := false;
        end;
      Reg.Free;
      if not Result then exit;
    end;

  if not ValidateTitle then exit(false);

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
    Hide;
end;

procedure TStudyUnitFrame.AssignActionLinks;
begin
  with MainForm do
  begin
    PrintDataFormMenuItem.Action     := nil;
    // -
    AddStructureMenuItem.Action      := nil;
    AddStructFromBLMenuItem.Action   := nil;

    // Edit
    UndoMenuItem.Action              := nil;
    RedoMenuItem.Action              := nil;
    // -
    CutMenuItem.Action               := nil;
    CopyMenuItem.Action              := nil;
    PasteMenuItem.Action             := nil;
    // -
    PasteAsHeadingMenuItem.Action    := nil;
    PasteAsIntMenuItem.Action        := nil;
    PasteAsFloatMenuItem.Action      := nil;
    PasteAsStringMenuItem.Action     := nil;
    PasteAsDateMenuItem.Action       := nil;
    RenameControlsMenuItem.Action    := nil;
    RenameControlsPopupMenuItem.Action := nil;

    // Align
    AlignLeftMenuItem.Action         := nil;
    AlignRightMenuItem.Action        := nil;
    AlignTopMenuItem.Action          := nil;
    AlignBottomMenuItem.Action       := nil;
    AlignMenuItem.Action             := nil;

    // Select
    SelectAllIntsMenuItem.Action     := nil;
    SelectAllFloatMenuItem.Action    := nil;
    SelectAllStringMenuItem.Action   := nil;
    SelectAllBoolMenuItem.Action     := nil;


    // DataSet
    BrowseDataMenuItem.Action        := nil;
    BrowseDatasetMenuItem.Action     := nil;
  end;
end;

end.

