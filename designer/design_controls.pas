unit design_controls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, ActnList, AVL_Tree, epicustombase, epidatafiles,
  epidocument, epivaluelabels, LCLType;

type

  { IDesignEpiControl }

  IDesignEpiControl = interface ['{D816F23A-0CC6-418A-8A6F-B1D28FC42E52}']
    function  GetEpiControl: TEpiCustomControlItem;
    function  GetXTreeNode: TAVLTreeNode;
    function  GetYTreeNode: TAVLTreeNode;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure SetXTreeNode(const AValue: TAVLTreeNode);
    procedure SetYTreeNode(const AValue: TAVLTreeNode);
    property  EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
    property  XTreeNode: TAVLTreeNode read GetXTreeNode write SetXTreeNode;
    property  YTreeNode: TAVLTreeNode read GetYTreeNode write SetYTreeNode;
  end;

  { IPositionHandler }

  IPositionHandler = interface ['{EE58F27F-C0EB-43E1-BCF2-8525F632F527}']
    function GetXTree: TAVLTree;
    function GetYTree: TAVLTree;
    property XTree: TAVLTree read GetXTree;
    property YTree: TAVLTree read GetYTree;
  end;

  { TDesignField }

  TDesignField = class(TEdit, IDesignEpiControl)
  private
    // IDesignEpiControl
    FField: TEpiField;
    FXTreeNode: TAVLTreeNode;
    FYTreeNode: TAVLTreeNode;
    function    GetEpiControl: TEpiCustomControlItem;
    function    GetXTreeNode: TAVLTreeNode;
    function    GetYTreeNode: TAVLTreeNode;
    procedure   SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure   SetXTreeNode(const AValue: TAVLTreeNode);
    procedure   SetYTreeNode(const AValue: TAVLTreeNode);
  private
    FNameLabel: TLabel;
    FQuestionLabel: TLabel;
    procedure   OnFieldChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure   OnQuestionChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure   OnProjectSettingsChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure   UpdateNameLabel;
    procedure   UpdateHint;
  protected
    procedure   SetParent(NewParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
    property    NameLabel: TLabel read FNameLabel;
    property    QuestionLabel: TLabel read FQuestionLabel;
    property    XTreeNode: TAVLTreeNode read GetXTreeNode write SetXTreeNode;
    property    YTreeNode: TAVLTreeNode read GetYTreeNode write SetYTreeNode;
  end;

  { TDesignSection }

  TDesignSection = Class(TGroupBox, IDesignEpiControl, IPositionHandler)
  private
    // IDesignEpiControl
    FSection: TEpiSection;
    FXTreeNode: TAVLTreeNode;
    FYTreeNode: TAVLTreeNode;
    function GetEpiControl: TEpiCustomControlItem;
    function GetXTreeNode: TAVLTreeNode;
    function GetYTreeNode: TAVLTreeNode;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure SetXTreeNode(const AValue: TAVLTreeNode);
    procedure SetYTreeNode(const AValue: TAVLTreeNode);
  private
    // IPositionHandler
    FXTree: TAVLTree;
    FYTree: TAVLTree;
    function GetXTree: TAVLTree;
    function GetYTree: TAVLTree;
  private
    procedure OnChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure UpdateHint;
  public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; override;
    // IDesignEpiControl
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
    property XTreeNode: TAVLTreeNode read GetXTreeNode write SetXTreeNode;
    property YTreeNode: TAVLTreeNode read GetYTreeNode write SetYTreeNode;
    // IPositionHandler
    property XTree: TAVLTree read GetXTree;
    property YTree: TAVLTree read GetYTree;
  end;

  { TDesignHeading }

  TDesignHeading = Class(TLabel, IDesignEpiControl)
  private
    FHeading: TEpiHeading;
    FXTreeNode: TAVLTreeNode;
    FYTreeNode: TAVLTreeNode;
    function    GetEpiControl: TEpiCustomControlItem;
    function    GetXTreeNode: TAVLTreeNode;
    function    GetYTreeNode: TAVLTreeNode;
    procedure   SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure   SetXTreeNode(const AValue: TAVLTreeNode);
    procedure   SetYTreeNode(const AValue: TAVLTreeNode);
  private
    procedure   OnHeadingChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure   OnCaptionChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure   UpdateHint;
  public
    constructor Create(AOwner: TComponent); Override;
    destructor  Destroy; override;
    property    EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
    property    XTreeNode: TAVLTreeNode read GetXTreeNode write SetXTreeNode;
    property    YTreeNode: TAVLTreeNode read GetYTreeNode write SetYTreeNode;
  published
    property    OnStartDock;
    property    OnEndDock;
  end;

  { TDesignControlsForm }

  TDesignControlsForm = class(TForm)
    FieldRangesBtn: TButton;
    FieldRangesEdit: TEdit;
    Label10: TLabel;
    FieldTypeLabel: TLabel;
    FieldRangesLabel: TLabel;
    ShiftTo2Action: TAction;
    ShiftTo1Action: TAction;
    ShiftTo2: TAction;
    ShiftTo1: TAction;
    CancelAction: TAction;
    CloseAction: TAction;
    ApplyAction: TAction;
    ActionList1: TActionList;
    FieldAdvancedSheet: TTabSheet;
    FieldBasicSheet: TTabSheet;
    CancelBtn: TBitBtn;
    CaptionEdit: TEdit;
    DecimalsEdit: TEdit;
    DecimalsLabel: TLabel;
    GroupAssignedListBox: TListBox;
    GroupAvailableListBox: TListBox;
    Label9: TLabel;
    SectionGroupAccessGroupBox: TGroupBox;
    GrpRightsMoveLeft: TSpeedButton;
    GrpRightsMoveRight: TSpeedButton;
    HeightEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LengthEdit: TEdit;
    LengthLabel: TLabel;
    ManageValueLabelsButton: TButton;
    NameEdit: TEdit;
    SectionNameEdit: TEdit;
    OkBtn: TBitBtn;
    EpiControlPageControl: TPageControl;
    FieldTabSheet: TTabSheet;
    HeadingTabSheet: TTabSheet;
    ApplyBtn: TBitBtn;
    FieldPageControl: TPageControl;
    SectionPageControl: TPageControl;
    Panel3: TPanel;
    QuestionEdit: TEdit;
    SectionTabSheet: TTabSheet;
    SectionBasicSheet: TTabSheet;
    SectionAdvancedSheet: TTabSheet;
    ValueLabelComboBox: TComboBox;
    WidthEdit: TEdit;
    procedure ApplyActionExecute(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure FieldRangesEditUTF8KeyPress(Sender: TObject;
      var UTF8Key: TUTF8Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LengthEditChange(Sender: TObject);
    procedure ManageValueLabelsButtonClick(Sender: TObject);
    procedure ShiftTo1Execute(Sender: TObject);
    procedure ShiftTo2Execute(Sender: TObject);
  private
    { private declarations }
    FEpiControl: TEpiCustomControlItem;
    FHintWindow: THintWindow;
    constructor Create(TheOwner: TComponent); override;
    procedure   SetEpiControl(const AValue: TEpiCustomControlItem);
    function    UpdateValueLabels: boolean;
    procedure   ShowHintMsg(Msg: string; Ctrl: TControl);
    function    ValidateControl: boolean;
  private
    {Section Sheet}
    procedure GrpRightsMoveLeftClick(Sender: TObject);
    procedure GrpRightsMoveRightClick(Sender: TObject);
  private
    {ValueLabel hooks}
    FValueLabelSets: TEpiValueLabelSets;
    procedure ValueLabelSetHook(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure ValueLabelSetsHook(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const EpiDocument: TEpiCustomBase);
    destructor  Destroy; override;
    procedure   RestoreDefaultPos;
    procedure   Show;
    property    EpiControl: TEpiCustomControlItem read FEpiControl write SetEpiControl;
  end;


function XTreeSort(Item1, Item2: Pointer): Integer;
function YTreeSort(Item1, Item2: Pointer): Integer;
function WriteTree(Tree: TAVLTree): string;

procedure AddToPositionHandler(PositionHandler: IPositionHandler;
  Ctrl: TControl);
procedure RemoveFromPositionHandler(PositionHandler: IPositionHandler;
  Ctrl: TControl);

function EpiTextToControlText(Const Str: string): string;
function ControlTextToEpiText(Const Str: string): string;

var
  DesignControlsForm: TDesignControlsForm;

implementation

{$R *.lfm}

uses
  epidatafilestypes, math, types, valuelabelseditor_form, epiadmin,
  LCLProc, settings2_var, settings2, epimiscutils, main, episettings,
  epiranges, strutils;

const
  rsVLWarning = 'Warning: Valuelabels have changed...';

function XTreeSort(Item1, Item2: Pointer): Integer;
var
  Ctrl1: TControl absolute Item1;
  Ctrl2: TControl absolute Item2;
  EpiCtrl1: TEpiCustomControlItem;
  EpiCtrl2: TEpiCustomControlItem;
begin
  if Item1 = Item2 then exit(0);

  EpiCtrl1 := (Ctrl1 as IDesignEpiControl).EpiControl;
  EpiCtrl2 := (Ctrl2 as IDesignEpiControl).EpiControl;

  if EpiCtrl1.Left > EpiCtrl2.Left then
    result := 1
  else if EpiCtrl1.Left < EpiCtrl2.Left then
    result := -1
  else
    if EpiCtrl1.Top > EpiCtrl2.Top then
      result := 1
    else if EpiCtrl1.Top < EpiCtrl2.Top then
      result := -1
    else
      result := 0;
end;

function YTreeSort(Item1, Item2: Pointer): Integer;
var
  Ctrl1: TControl absolute Item1;
  Ctrl2: TControl absolute Item2;
  EpiCtrl1: TEpiCustomControlItem;
  EpiCtrl2: TEpiCustomControlItem;
begin
  if Item1 = Item2 then exit(0);

  EpiCtrl1 := (Ctrl1 as IDesignEpiControl).EpiControl;
  EpiCtrl2 := (Ctrl2 as IDesignEpiControl).EpiControl;

  if EpiCtrl1.Top > EpiCtrl2.Top then
    result := 1
  else if EpiCtrl1.Top < EpiCtrl2.Top then
    result := -1
  else
    if EpiCtrl1.Left > EpiCtrl2.Left then
      result := 1
    else if EpiCtrl1.Left < EpiCtrl2.Left then
      result := -1
    else
      result := 0;
end;

function WriteTree(Tree: TAVLTree): string;

  function WriteTreeNode(ANode: TAVLTreeNode; const Prefix: string): string;
  begin
    if ANode=nil then exit('');
    Result := WriteTreeNode(ANode.Right, Prefix + '  ');
    Result += Prefix +
      Format(
      'Node: %s (%d, %d)',
      [TControl(aNode.Data).Name,
       TControl(aNode.Data).Left,
       TControl(aNode.Data).Top]
      ) + LineEnding;
    Result += WriteTreeNode(ANode.Left,  Prefix + '  ');
  end;

  begin
  result := WriteTreeNode(Tree.Root, '');
end;

procedure AddToPositionHandler(PositionHandler: IPositionHandler; Ctrl: TControl
  );
begin
  (Ctrl as IDesignEpiControl).XTreeNode := PositionHandler.XTree.Add(Ctrl);
  (Ctrl as IDesignEpiControl).YTreeNode := PositionHandler.YTree.Add(Ctrl);
end;

procedure RemoveFromPositionHandler(PositionHandler: IPositionHandler;
  Ctrl: TControl);
begin
  PositionHandler.XTree.Remove(Ctrl);
  PositionHandler.YTree.Remove(Ctrl);
  (Ctrl as IDesignEpiControl).XTreeNode := nil;
  (Ctrl as IDesignEpiControl).YTreeNode := nil;
end;

function EpiTextToControlText(const Str: string): string;
begin
  result := StringReplace(Str, '&', '&&', [rfReplaceAll]);
end;

function ControlTextToEpiText(const Str: string): string;
begin
  result := StringReplace(Str, '&&', '&', [rfReplaceAll]);
end;

{ TDesignField }

function TDesignField.GetEpiControl: TEpiCustomControlItem;
begin
  result := FField;
end;

function TDesignField.GetXTreeNode: TAVLTreeNode;
begin
  result := FXTreeNode;
end;

function TDesignField.GetYTreeNode: TAVLTreeNode;
begin
  result := FYTreeNode;
end;

procedure TDesignField.SetEpiControl(const AValue: TEpiCustomControlItem);
var
  ProjectSettings: TEpiProjectSettings;
begin
  FField := TEpiField(AValue);
  FField.RegisterOnChangeHook(@OnFieldChange);
  FField.Question.RegisterOnChangeHook(@OnQuestionChange);
  ProjectSettings := TEpiDocument(FField.RootOwner).ProjectSettings;
  ProjectSettings.RegisterOnChangeHook(@OnProjectSettingsChange);
  Name := FField.Id;
  Caption := '';

  if not ProjectSettings.ShowFieldBorders then
    BorderStyle := bsNone;
  if not ProjectSettings.ShowFieldNames then
    FNameLabel.Visible := false;
end;

procedure TDesignField.SetXTreeNode(const AValue: TAVLTreeNode);
begin
  FXTreeNode := AValue;
end;

procedure TDesignField.SetYTreeNode(const AValue: TAVLTreeNode);
begin
  FYTreeNode := AValue;
end;

procedure TDesignField.OnFieldChange(Sender: TObject; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer);
var
  Cv: TCanvas;
  S: Char;
  SideBuf: Integer;
begin
  case EventGroup of
    eegCustomBase:
      case TEpiCustomChangeEventType(EventType) of
        ecceDestroy:
          begin
            FField := nil;
            Exit;
          end;
        // General update - set everything.
        ecceUpdate:
          begin
            // Set label first - else width of Question is not calculated.
            FNameLabel.Caption        := FField.Name;

            Left                      := FField.Left;
            FField.Question.Left      := FField.Left - (FQuestionLabel.Width + 5);
            Top                       := FField.Top;
            FField.Question.Top       := FField.Top;

            if Self.Parent is TScrollBox then
              Cv := TScrollBox(Self.Parent).Canvas
            else
              Cv := TScrollBox(Self.Parent.Parent).Canvas;
            case FField.FieldType of
              ftString,
              ftUpperString: S := 'W';
            else
              S := '4';
            end;

            case BorderStyle of
              bsNone:   SideBuf := 0;
              bsSingle: SideBuf := 6;
            end;

            //         Side buffer (pixel from controls left side to first character.
            Width   := (SideBuf * 2) + Cv.GetTextWidth(S) * FField.Length;
          end;
        ecceSetLeft:
          begin
            Left                      := FField.Left;
            FField.Question.Left      := FField.Left - (FQuestionLabel.Width + 5);
          end;
        ecceSetTop:
          begin
            Top                       := FField.Top;
            FField.Question.Top       := FField.Top;
          end;
        ecceText: FNameLabel.Caption  := FField.Name;
      end;
  end;
  UpdateNameLabel;
  UpdateHint;
end;

procedure TDesignField.OnQuestionChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  Question: TEpiHeading absolute Sender;
begin
  with FQuestionLabel do
  case EventGroup of
    eegCustomBase:
      case TEpiCustomChangeEventType(EventType) of
        ecceDestroy: exit;
        ecceUpdate:
          begin
            Caption := Question.Caption.Text;
            Left    := Question.Left;
            Top     := Question.Top;
          end;
        ecceSetTop:
            Top     := Question.Top;
        ecceSetLeft:
            Left    := Question.Left;
        ecceText:
            Caption := Question.Caption.Text;
      end;
  end;
  UpdateHint;
end;

procedure TDesignField.OnProjectSettingsChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  ProjectSettings: TEpiProjectSettings absolute Sender;
begin
  if (EventGroup = eegCustomBase) and
     (TEpiCustomChangeEventType(EventType) = ecceDestroy) then exit;

  FNameLabel.Visible := ProjectSettings.ShowFieldNames;
  UpdateNameLabel;
  if ProjectSettings.ShowFieldBorders then
    BorderStyle := bsSingle
  else
    BorderStyle := bsNone;
end;

procedure TDesignField.UpdateNameLabel;
begin
  FNameLabel.Left := FQuestionLabel.Left - (FNameLabel.Width + 5);
  FNameLabel.Top := FQuestionLabel.Top;
end;

procedure TDesignField.UpdateHint;
{$IFDEF EPI_DEBUG}
var
  S: string;
{$ENDIF}
begin
  {$IFDEF EPI_DEBUG}
  WriteStr(S, FField.FieldType);
  {$ENDIF}
  With FField do
    Hint := WideFormat(
      'Name: %s' + LineEnding +
      {$IFDEF EPI_DEBUG}
      'Type: %s' + LineEnding +
      {$ENDIF}
      'Length: %d' + LineEnding +
      'Question: %s' + LineEnding +
      'X: %d, Y: %d',
      [UTF8Decode(Name), {$IFDEF EPI_DEBUG} S, {$ENDIF}
       Length, UTF8Decode(Question.Caption.Text),
       Left, Top]
    );
end;

procedure TDesignField.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if csDestroying in ComponentState then exit;

  FNameLabel.Parent := NewParent;
  FQuestionLabel.Parent := NewParent;
end;

constructor TDesignField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNameLabel := TLabel.Create(Self);
  FQuestionLabel := TLabel.Create(Self);

  // Standard properties being set for the component.
  DragKind := dkDock;
  DragMode := dmAutomatic;
  AutoSelect := false;
  AutoSize := false;
  ReadOnly := true;
  TabStop := false;
  Align := alNone;
  ShowHint := true;
  ParentColor := false;
end;

destructor TDesignField.Destroy;
begin
  FNameLabel.Free;
  FQuestionLabel.Free;
  if Assigned(FField) then
  begin
    FField.UnRegisterOnChangeHook(@OnFieldChange);
    FField.Question.UnRegisterOnChangeHook(@OnQuestionChange);
  end;
  inherited Destroy;
end;

{ TDesignSection }

function TDesignSection.GetEpiControl: TEpiCustomControlItem;
begin
  result := FSection;
end;

function TDesignSection.GetXTreeNode: TAVLTreeNode;
begin
  result := FXTreeNode;
end;

function TDesignSection.GetYTreeNode: TAVLTreeNode;
begin
  result := FYTreeNode;
end;

procedure TDesignSection.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FSection := TEpiSection(AValue);
  FSection.RegisterOnChangeHook(@OnChange);
  Name := FSection.Id;
  Caption := '';
end;

procedure TDesignSection.SetXTreeNode(const AValue: TAVLTreeNode);
begin
  FXTreeNode := AValue;
end;

procedure TDesignSection.SetYTreeNode(const AValue: TAVLTreeNode);
begin
  FYTreeNode := AValue;
end;

function TDesignSection.GetXTree: TAVLTree;
begin
  result := FXTree;
end;

function TDesignSection.GetYTree: TAVLTree;
begin
  result := FYTree;
end;

procedure TDesignSection.OnChange(Sender: TObject; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer);
begin
  case EventGroup of
    eegCustomBase:
      case TEpiCustomChangeEventType(EventType) of
        ecceDestroy: exit;
        ecceSetLeft: Left := FSection.Left;
        ecceSetTop:  Top  := FSection.Top;
        ecceText:    Caption := EpiTextToControlText(FSection.Name.Text);
        ecceUpdate:
          begin
            Left := FSection.Left;
            Top  := FSection.Top;
            Caption := EpiTextToControlText(FSection.Name.Text);
            Width := FSection.Width;
            Height := FSection.Height;
          end;
      end;
    eegSections:
      begin
        Case TEpiSectionsChangeEventType(EventType) of
          esceWidth:  Width := FSection.Width;
          esceHeight: Height := FSection.Height;
        end;
      end;
  end;
  UpdateHint;
end;

procedure TDesignSection.UpdateHint;
var
  S: String;
  i: Integer;
begin
{  S := '';
  for i := 0 to FSection.Groups.Count - 1 do
    S += FSection.Groups[i].Name.Text + ',';
  Delete(S, Length(S), 1);}

  With FSection do
    Hint := WideFormat(
      'Name: %s' + LineEnding +
//      'Groups: %s' + LineEnding +
      'X: %d, Y: %d' + LineEnding +
      'W: %d, H: %d',
      [UTF8Decode(Name.Text), {UTF8Decode(S),}
       Left, Top, Width, Height]
    );
end;

constructor TDesignSection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FXTree := TAVLTree.Create(@XTreeSort);
  FYTree := TAVLTree.Create(@YTreeSort);

  DragKind := dkDock;
  DragMode := dmAutomatic;
  DockSite := true;
  ShowHint := true;
  Color := clWhite;
  ParentColor := false;
end;

destructor TDesignSection.Destroy;
begin
  FXTree.Free;
  FYTree.Free;
  inherited Destroy;
end;

{ TDesignHeading }

function TDesignHeading.GetEpiControl: TEpiCustomControlItem;
begin
  result := FHeading;
end;

function TDesignHeading.GetXTreeNode: TAVLTreeNode;
begin
  result := FXTreeNode;
end;

function TDesignHeading.GetYTreeNode: TAVLTreeNode;
begin
  result := FYTreeNode;
end;

procedure TDesignHeading.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FHeading := TEpiHeading(AValue);
  FHeading.RegisterOnChangeHook(@OnHeadingChange);
  FHeading.Caption.RegisterOnChangeHook(@OnCaptionChange);
  Name := FHeading.Id;
  Caption := '';
end;

procedure TDesignHeading.SetXTreeNode(const AValue: TAVLTreeNode);
begin
  FXTreeNode := AValue;
end;

procedure TDesignHeading.SetYTreeNode(const AValue: TAVLTreeNode);
begin
  FYTreeNode := AValue;
end;

procedure TDesignHeading.OnHeadingChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  case EventGroup of
    eegCustomBase:
      case TEpiCustomChangeEventType(EventType) of
        ecceDestroy: exit;
        ecceSetLeft: Left := FHeading.Left;
        ecceSetTop:  Top  := FHeading.Top;
        ecceUpdate:
          begin
            Left := FHeading.Left;
            Top  := FHeading.Top;
          end;
      end;
  end;
  UpdateHint;
end;

procedure TDesignHeading.OnCaptionChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup = eegCustomBase) and
     (EventType = Word(ecceDestroy)) then exit;

  Caption := EpiTextToControlText(TEpiTranslatedText(Sender).Text);
  UpdateHint;
end;

procedure TDesignHeading.UpdateHint;
begin
  with FHeading do
    Hint := WideFormat(
      'Caption: %s' + LineEnding +
      'X: %d, Y: %d',
      [UTF8Decode(Caption.Text),
       Left,
       Top]
    );
end;

constructor TDesignHeading.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Standard properties being set for the component.
  DragKind := dkDock;
  DragMode := dmAutomatic;
  Font.Style := [fsBold];
  Align := alNone;
  ShowHint := true;
  ParentColor := true;
end;

destructor TDesignHeading.Destroy;
begin
  inherited Destroy;
end;

{ TDesignControlsForm }

procedure TDesignControlsForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'ControlsForm');
  EpiControlPageControl.ShowTabs := false;
end;

procedure TDesignControlsForm.LengthEditChange(Sender: TObject);
begin
  if ValueLabelComboBox.ItemIndex = -1 then exit;

  if UpdateValueLabels then
    ShowHintMsg(rsVLWarning, TControl(Sender))
  else
    FHintWindow.Hide;
end;

procedure TDesignControlsForm.ManageValueLabelsButtonClick(Sender: TObject);
begin
  GetValueLabelsEditor(TEpiDocument(FValueLabelSets.RootOwner)).Show;
end;

procedure TDesignControlsForm.ShiftTo1Execute(Sender: TObject);
begin
  if EpiControlPageControl.ActivePage = SectionTabSheet then
    SectionPageControl.ActivePage := SectionBasicSheet;

  if EpiControlPageControl.ActivePage = FieldTabSheet then
    FieldPageControl.ActivePage := FieldBasicSheet;
end;

procedure TDesignControlsForm.ShiftTo2Execute(Sender: TObject);
begin
  if EpiControlPageControl.ActivePage = SectionTabSheet then
    SectionPageControl.ActivePage := SectionAdvancedSheet;

  if EpiControlPageControl.ActivePage = FieldTabSheet then
    FieldPageControl.ActivePage := FieldAdvancedSheet;
end;

procedure TDesignControlsForm.ApplyActionExecute(Sender: TObject);
begin
  if ValidateControl then
    MainForm.SetFocus;
end;

procedure TDesignControlsForm.CancelActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TDesignControlsForm.CloseActionExecute(Sender: TObject);
begin
  if ValidateControl then Close;
end;

procedure TDesignControlsForm.FieldRangesEditUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
var
  I: integer;
  Ch: LongWord;
begin
  Ch := UTF8CharacterToUnicode(@UTF8Key[1], I);
  if not (Char(Ch) in [VK_0..VK_9,VK_BACK,'.',',','|','-']) then
    UTF8Key := '';
end;

procedure TDesignControlsForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'ControlsForm');
end;

procedure TDesignControlsForm.FormDestroy(Sender: TObject);
var
  B: Boolean;
begin
  B:=true;
  FormCloseQuery(nil, B);
end;

procedure TDesignControlsForm.SetEpiControl(const AValue: TEpiCustomControlItem);
var
  FField: TEpiField;
  FSection: TEpiSection;
  LocalGroups: TEpiGroups;
  i: Integer;
  S: String;

  function RangesToText(Const Ranges: TEpiRanges): string;
  var
    i: integer;
  begin
    result := '';
    if not Assigned(Ranges) then exit;

    for i := 0 to Ranges.Count - 1 do
    with TEpiRange(Ranges[i]) do
    begin
      Result := Result + AsString[true];
      if not Single then
        Result := Result + '-' + AsString[false];
      Result := Result + '|';
    end;
    Delete(Result, Length(Result), 1);
  end;

begin
  if FEpiControl = AValue then exit;
  FEpiControl := AValue;

  BeginFormUpdate;
  if FEpiControl is TEpiSection then
  begin
    Caption := 'Section Properties';
    EpiControlPageControl.ActivePage := SectionTabSheet;
    FSection := TEpiSection(EpiControl);

    // Basic Page
    SectionNameEdit.Text := FSection.Name.Text;

    {$IFNDEF EPI_DEBUG}
    SectionGroupAccessGroupBox.Visible := false;
    SectionGroupAccessGroupBox.Enabled := false;
    {$ELSE}
    GroupAssignedListBox.Items.BeginUpdate;
    GroupAvailableListBox.Items.BeginUpdate;
    GroupAssignedListBox.Clear;
    GroupAvailableListBox.Clear;
    LocalGroups := TEpiDocument(FSection.RootOwner).Admin.Groups;
    for i := 0 to LocalGroups.Count - 1 do
    begin
      if FSection.Groups.ItemExistsById(LocalGroups[i].Id) then
        GroupAssignedListBox.Items.AddObject(LocalGroups[i].Name.Text, LocalGroups[i])
      else
        GroupAvailableListBox.Items.AddObject(LocalGroups[i].Name.Text, LocalGroups[i]);
    end;
    GroupAvailableListBox.Items.EndUpdate;
    GroupAssignedListBox.Items.EndUpdate;
    {$ENDIF}

    // Advanced Page
    WidthEdit.Text := IntToStr(FSection.Width);
    HeightEdit.Text := IntToStr(FSection.Height);
  end;

  if FEpiControl is TEpiField then
  begin
    Caption := 'Field Properties';
    EpiControlPageControl.ActivePage := FieldTabSheet;
    FField := TEpiField(EpiControl);

    // Setup Basic page
    NameEdit.Text         := FField.Name;
    FieldTypeLabel.Caption := EpiTypeNames[FField.FieldType];
    QuestionEdit.Text     := FField.Question.Caption.Text;
    if FField.FieldType = ftFloat then
      LengthEdit.Text     := IntToStr(FField.Length - (FField.Decimals + 1))
    else
      LengthEdit.Text     := IntToStr(FField.Length);
    DecimalsEdit.Text     := IntToStr(FField.Decimals);

    // Visible edits
    LengthEdit.Visible    := FField.FieldType in [ftInteger, ftAutoInc, ftFloat, ftString, ftUpperString];
    LengthLabel.Visible   := LengthEdit.Visible;
    DecimalsEdit.Visible  := FField.FieldType = ftFloat;
    DecimalsLabel.Visible := DecimalsEdit.Visible;

    // Setup "advanced" page.
    ValueLabelComboBox.ItemIndex := ValueLabelComboBox.Items.IndexOfObject(nil);
    FieldRangesEdit.Visible := FField.FieldType in [ftInteger, ftFloat, ftDMYDate, ftMDYDate, ftYMDDate, ftTime];
    FieldRangesLabel.Visible := FieldRangesEdit.Visible;
    FieldRangesEdit.Enabled := FField.FieldType in [ftInteger, ftFloat];
    FieldRangesBtn.Visible := FieldRangesEdit.Visible and (not FieldRangesEdit.Enabled);
    if FieldRangesEdit.Visible then
      FieldRangesEdit.Text := RangesToText(FField.Ranges)
    else
      FieldRangesEdit.Text := '';

    UpdateValueLabels;
  end;

  if FEpiControl is TEpiHeading then
  begin
    Caption := 'Heading Properties';
    EpiControlPageControl.ActivePage := HeadingTabSheet;
    CaptionEdit.Text := TEpiHeading(EpiControl).Caption.Text;
  end;
  EndFormUpdate;
end;

function TDesignControlsForm.UpdateValueLabels: boolean;
var
  i: Integer;
  DoAdd: boolean;
  FList: TStringList;
  Idx: LongInt;
  l: Integer;
  PreSelectedVLSet: TEpiValueLabelSet;
  OIdx: LongInt;
  CurrentVLSet: TEpiValueLabelSet;
  IntL: Integer;
  DecL: Integer;
  j: Integer;
  S: String;
  FField: TEpiField;
begin
  FField := TEpiField(EpiControl);
  PreSelectedVLSet := nil;
  if ValueLabelComboBox.ItemIndex >= 0 then
    PreSelectedVLSet := TEpiValueLabelSet(ValueLabelComboBox.Items.Objects[ValueLabelComboBox.ItemIndex]);
  Idx := -1;

  ValueLabelComboBox.Items.BeginUpdate;
  ValueLabelComboBox.Clear;
  ValueLabelComboBox.Sorted := true;
  if (FValueLabelSets.Count = 0) or
     (not (FField.FieldType in [ftInteger, ftFloat, ftString, ftUpperString])) then
  begin
    OIdx := ValueLabelComboBox.Items.AddObject('(none)', nil);
    if not (FField.FieldType in [ftInteger, ftFloat, ftString, ftUpperString]) then
      ValueLabelComboBox.Hint := 'ValueLabels not support for this field type!'
    else
      ValueLabelComboBox.Hint := 'No Valuelabels Defined.' + LineEnding + 'Press "Manage" to create a ValueLabel set.';
  end else begin
   for i := 0 to FValueLabelSets.Count - 1 do
   begin
     CurrentVLSet := FValueLabelSets[i];
     case CurrentVLSet.LabelType of
       ftInteger:
         begin
           DoAdd := FField.FieldType in [ftInteger, ftFloat];
           if FField.FieldType = ftFloat then
             DoAdd := DoAdd and (CurrentVLSet.MaxValueLength <= StrToIntDef(LengthEdit.Text, (FField.Length - FField.Decimals - 1)))
           else
             DoAdd := DoAdd and (CurrentVLSet.MaxValueLength <= StrToIntDef(LengthEdit.Text, FField.Length));
         end;
       ftFloat:
         begin
           DoAdd := FField.FieldType = ftFloat;

           if DoAdd then
           begin
             IntL := 0;
             DecL := 0;
             for j := 0 to CurrentVLSet.Count - 1 do
             begin
               S := CurrentVLSet[j].ValueAsString;
               l := Pos(DecimalSeparator, S);
               if l = 0 then
                 IntL := Length(S)
               else
                 IntL := Max(IntL, l - 1);
               DecL := Max(DecL, Length(S) - (IntL+1));
             end;
           end;
           DoAdd := DoAdd and
             (IntL <= StrToIntDef(LengthEdit.Text, FField.Length - FField.Decimals - 1)) and
             (DecL <= StrToIntDef(DecimalsEdit.Text, FField.Decimals));
         end;
       ftString:  DoAdd := FField.FieldType in [ftString, ftUpperString];
     end;

     if DoAdd then
       ValueLabelComboBox.AddItem(CurrentVLSet.Name, CurrentVLSet);
   end;
   S := 'Support ValueLabel types:' + LineEnding;
   case FField.FieldType of
     ftInteger: S := S + EpiTypeNames[ftInteger];
     ftFloat:   S := S + EpiTypeNames[ftInteger] + LineEnding + EpiTypeNames[ftFloat];
     ftString,
     ftUpperString: S := S + EpiTypeNames[ftString] + LineEnding + EpiTypeNames[ftUpperString];
   end;
   ValueLabelComboBox.Hint := S;

   OIdx := ValueLabelComboBox.Items.AddObject('(none)', nil);
   if Assigned(PreSelectedVLSet) then
     Idx := ValueLabelComboBox.Items.IndexOfObject(PreSelectedVLSet)
   else if Assigned(FField.ValueLabelSet) then
     Idx := ValueLabelComboBox.Items.IndexOfObject(FField.ValueLabelSet);
  end;
  if Idx = -1 then
    Idx := OIdx;
  ValueLabelComboBox.Items.EndUpdate;
  ValueLabelComboBox.ItemIndex := Idx;

  result := (PreSelectedVLSet <> ValueLabelComboBox.Items.Objects[Idx]);
end;

procedure TDesignControlsForm.ShowHintMsg(Msg: string; Ctrl: TControl);
var
  R: TRect;
  P: TPoint;
begin
  R := FHintWindow.CalcHintRect(0, Msg, nil);
  P := Ctrl.ClientToScreen(Point(0,0));
  OffsetRect(R, P.X, P.Y + Ctrl.Height + 2);
  FHintWindow.ActivateHint(R, Msg);
end;

function TDesignControlsForm.ValidateControl: boolean;
var
  FSection: TEpiSection;
  FField: TEpiField;
  FHeading: TEpiHeading;
  NewLen: LongInt;
  NewDecLen: LongInt;
  i: Integer;
  Ranges: TEpiRanges;

  function  TextToRanges(Text: string; Const AOwner: TEpiField; Out Ranges: TEpiRanges): boolean;
  var
    SList: TStringList;
    i:     integer;
    IntRes: EpiInteger;
    FlRes: EpiFloat;
    S, T: string;
    P: LongInt;
    Range: TEpiRange;
  begin
    Result := true;

    Ranges := TEpiRanges.Create(AOwner);
    Ranges.ItemOwner := true;

    try
      S := Copy2SymbDel(Text, '|');

      while Length(S) > 0 do
      begin
        Range := Ranges.NewRange;
        T := Copy2SymbDel(S, '-');
        case Ranges.FieldType of
          ftInteger: begin
                       if not TryStrToInt64(T, IntRes) then
                       begin
                         ShowHintMsg(T + ' is not a valid integer', FieldRangesEdit);
                         Exit(false);
                       end;
                       Range.AsInteger[true] := IntRes;

                       if (Length(S) > 0) and (not TryStrToInt64(S, IntRes)) then
                       begin
                         ShowHintMsg(S + ' is not a valid integer', FieldRangesEdit);
                         Exit(false);
                       end;
                       Range.AsInteger[false] := IntRes;
                     end;
          ftFloat:   begin
                       if not TryStrToFloat(T, FlRes) then
                       begin
                         ShowHintMsg(T + ' is not a valid float', FieldRangesEdit);
                         Exit(false);
                       end;
                       Range.AsFloat[true] := FlRes;

                       if (Length(S) > 0) and (not TryStrToFloat(S, FlRes)) then
                       begin
                         ShowHintMsg(S + ' is not a valid float', FieldRangesEdit);
                         Exit(false);
                       end;
                       Range.AsFloat[false] := FlRes;
                     end;
        end;
        S := Copy2SymbDel(Text, '|');
      end;
    except
      FreeAndNil(Ranges);
      Result := false;
    end;
  end;

begin
  Result := false;

  if EpiControl is TEpiSection then
  begin
    FSection := TEpiSection(EpiControl);
    FSection.BeginUpdate;

    // Basic Page
    FSection.Name.Text := SectionNameEdit.Text;
    for i := 0 to GroupAssignedListBox.Count - 1 do
      if not FSection.Groups.ItemExistsById(TEpiGroup(GroupAssignedListBox.Items.Objects[i]).Id) then
        FSection.Groups.AddItem(TEpiGroup(GroupAssignedListBox.Items.Objects[i]));

    // Advanced Page
    FSection.Width := StrToInt(WidthEdit.Text);
    FSection.Height := StrToInt(HeightEdit.Text);

    FSection.EndUpdate;

    Result := true;
    Exit;
  end;

  if EpiControl is TEpiField then
  begin
    FField := TEpiField(EpiControl);

    // "Standard" page
    // Rules for field creation!
    NewLen := FField.Length;
    if LengthEdit.Visible then
    begin
      if (not TryStrToInt(LengthEdit.Text, NewLen)) or
         ((FField.FieldType = ftInteger) and (NewLen >= 19)) or
         (NewLen <= 0)
      then
      begin
        LengthEdit.SetFocus;
        ShowHintMsg('Invalid length', LengthEdit);
        exit;
      end;
    end;

    NewDecLen := FField.Decimals;
    if DecimalsEdit.Visible then
    begin
      if (not TryStrToInt(DecimalsEdit.Text, NewDecLen)) or
         (NewDecLen <= 0) then
      begin
        DecimalsEdit.SetFocus;
        ShowHintMsg('Invalid decimals', DecimalsEdit);
        exit;
      end;
    end;

    FField.BeginUpdate;
    if NameEdit.Text <> FField.Name then
    begin
      FField.Name := NameEdit.Text;
      if FField.Name <> NameEdit.Text then
      begin
        // Could not rename Fieldname, possibly due to same name already exists or invalid identifier.
        NameEdit.SetFocus;
        ShowHintMsg('Name already exists or invalid identifier', NameEdit);
        FField.EndUpdate;
        Exit;
      end;
    end;
    FField.Length := NewLen;
    FField.Decimals := NewDecLen;
    if NewDecLen > 0 then
      FField.Length := FField.Length + FField.Decimals + 1;
    FField.Question.Caption.Text := QuestionEdit.Text;

    // "Advanced" page
    if ValueLabelComboBox.ItemIndex >= 0 then
      FField.ValueLabelSet := TEpiValueLabelSet(ValueLabelComboBox.Items.Objects[ValueLabelComboBox.ItemIndex]);

    if (FieldRangesEdit.Text <> '') and (TextToRanges(FieldRangesEdit.Text, FField, Ranges)) then
    begin
      FField.Ranges.Free;
      FField.Ranges := Ranges;
    end;

    FField.EndUpdate;
    Result := true;
    exit;
  end;

  if EpiControl is TEpiHeading then
  begin
    FHeading := TEpiHeading(EpiControl);

    if UTF8Length(CaptionEdit.Text) = 0 then
    begin
      ShowHintMsg('Empty heading not allowed...', CaptionEdit);
      Exit;
    end;

    FHeading.BeginUpdate;
    FHeading.Caption.Text := CaptionEdit.Text;
    FHeading.EndUpdate;

    Result := true;
    Exit;
  end;
end;

procedure TDesignControlsForm.GrpRightsMoveLeftClick(Sender: TObject);
var
  Idx: LongInt;
  Grp: TEpiGroup;
begin
  if GroupAssignedListBox.ItemIndex < 0 then exit;

  Idx := GroupAssignedListBox.ItemIndex;
  Grp := TEpiGroup(GroupAssignedListBox.Items.Objects[Idx]);
  GroupAvailableListBox.Items.AddObject(Grp.Name.Text, Grp);
  GroupAssignedListBox.Items.Delete(Idx);
end;

procedure TDesignControlsForm.GrpRightsMoveRightClick(Sender: TObject);
var
  Grp: TEpiGroup;
  Idx: LongInt;
begin
  if GroupAvailableListBox.ItemIndex < 0 then exit;

  Idx := GroupAvailableListBox.ItemIndex;
  Grp := TEpiGroup(GroupAvailableListBox.Items.Objects[Idx]);
  GroupAssignedListBox.Items.AddObject(Grp.Name.Text, Grp);
  GroupAvailableListBox.Items.Delete(Idx);
end;

constructor TDesignControlsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FHintWindow := THintWindow.Create(Self);
  FHintWindow.AutoHide := true;
  FHintWindow.HideInterval := 5 * 1000;

  GrpRightsMoveLeft.OnClick := @GrpRightsMoveLeftClick;
  GrpRightsMoveRight.OnClick := @GrpRightsMoveRightClick;
end;

procedure TDesignControlsForm.ValueLabelSetHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup = eegValueLabels) and (EventType = Word(evceName)) then
    UpdateValueLabels;
end;

procedure TDesignControlsForm.ValueLabelSetsHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup = eegCustomBase) and (EventType = Word(ecceAddItem)) then
    TEpiValueLabelSet(Data).RegisterOnChangeHook(@ValueLabelSetHook, true);

  if (EventGroup = eegCustomBase) and (EventType = Word(ecceDelItem)) then
  begin
    TEpiValueLabelSet(Data).UnRegisterOnChangeHook(@ValueLabelSetHook);
    if UpdateValueLabels then
      ShowHintMsg(
        Format('Warning: Valuelabels changed for field "%s"', [TEpiField(EpiControl).Name]),
        GetValueLabelsEditor(TEpiDocument(FValueLabelSets.RootOwner)).ToolBar1);
  end;

  if (EventGroup = eegCustomBase) and (EventType = Word(ecceUpdate)) then
  begin
    if UpdateValueLabels then
      ShowHintMsg(rsVLWarning, Self);
  end;
end;

constructor TDesignControlsForm.Create(TheOwner: TComponent;
  const EpiDocument: TEpiCustomBase);
var
  i: Integer;
begin
  Create(TheOwner);
  FValueLabelSets := TEpiDocument(EpiDocument).ValueLabelSets;
  FValueLabelSets.RegisterOnChangeHook(@ValueLabelSetsHook, true);

  for i := 0 to FValueLabelSets.Count - 1 do
    FValueLabelSets[i].RegisterOnChangeHook(@ValueLabelSetHook, true);
end;

destructor TDesignControlsForm.Destroy;
var
  i: Integer;
begin
  for i := 0 to FValueLabelSets.Count - 1 do
    FValueLabelSets[i].UnRegisterOnChangeHook(@ValueLabelSetHook);
  FValueLabelSets.UnRegisterOnChangeHook(@ValueLabelSetsHook);
  inherited Destroy;
end;

procedure TDesignControlsForm.RestoreDefaultPos;
begin
  BeginFormUpdate;
  Width := 500;
  Height := 500;
  Top := 20;
  Left := 300;
  EndFormUpdate;
  SaveFormPosition(Self, 'ControlsForm');
end;

procedure TDesignControlsForm.Show;
begin
  if not Assigned(EpiControl) then exit;

  Visible := true;
  if EpiControl is TEpiSection then
  begin
    SectionPageControl.ActivePage := SectionBasicSheet;
    SectionNameEdit.SetFocus;
  end;
  if EpiControl is TEpiField then
  begin
    FieldPageControl.ActivePage := FieldBasicSheet;
    QuestionEdit.SetFocus;
  end;
  if EpiControl is TEpiHeading then
    CaptionEdit.SetFocus;
  BringToFront;
end;

end.

