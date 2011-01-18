unit design_controls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, ActnList, AVL_Tree, epicustombase, epidatafiles,
  epidocument, epivaluelabels, LCLType;

type

  { TDesignPropertiesFrame }

  TDesignPropertiesFrame = class;

  TDesingFrameShowHintEvent = procedure(Sender: TDesignPropertiesFrame; Ctrl: TControl; const Msg: string) of object;
  TDesignPropertiesFrame = class(TFrame)
  private
    FEpiControl: TEpiCustomControlItem;
    FOnShowHintMsg: TDesingFrameShowHintEvent;
  protected
    procedure SetEpiControl(const AValue: TEpiCustomControlItem); virtual;
    procedure ShiftToTabSheet(Const SheetNo: Byte); virtual; abstract;
    procedure ShowHintMsg(const Msg: string; Ctrl: TControl);
  public
    function  ValidateControl: boolean; virtual; abstract;
    procedure UpdateFormContent; virtual; abstract;
    procedure ForceShow; virtual;
    property  EpiControl: TEpiCustomControlItem read FEpiControl write SetEpiControl;
    property  OnShowHintMsg: TDesingFrameShowHintEvent read FOnShowHintMsg write FOnShowHintMsg;
  end;

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

  TDesignerControlType = (dctField, dctSection, dctHeading);

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
    procedure OnTextChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
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
    CancelAction: TAction;
    CloseAction: TAction;
    ApplyAction: TAction;
    ActionList1: TActionList;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    ApplyBtn: TBitBtn;
    Panel3: TPanel;
    procedure ApplyActionExecute(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FActiveFrame: TDesignPropertiesFrame;
    FEpiControl: TEpiCustomControlItem;
    FHintWindow: THintWindow;
    FEpiDocument: TEpiDocument;
    constructor Create(TheOwner: TComponent); override;
    procedure   SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure   ShowHintMsg(Sender: TDesignPropertiesFrame; Ctrl: TControl; const Msg: string);
    procedure   UpdateControlContent;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const EpiDocument: TEpiCustomBase);
    destructor  Destroy; override;
    function    ValidateControl: boolean;
    procedure   RestoreDefaultPos;
    procedure   Show;
    property    EpiControl: TEpiCustomControlItem read FEpiControl write SetEpiControl;
    property    ActiveFrame: TDesignPropertiesFrame read FActiveFrame;
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

function DesignControlTypeFromControl(Ctrl: TControl): TDesignerControlType;

var
  DesignControlsForm: TDesignControlsForm;

implementation

{$R *.lfm}

uses
  epidatafilestypes, math, types, valuelabelseditor_form, epiadmin,
  LCLProc, settings2_var, settings2, epimiscutils, main, episettings,
  epiranges, strutils, epiconvertutils,
  // EpiControls property frames.
  design_fieldproperties_frame, design_headingproperties_frame,
  design_sectionproperties_frame;

var
  DesignPropertyFrames: array[0..2] of TDesignPropertiesFrame;

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

function DesignControlTypeFromControl(Ctrl: TControl): TDesignerControlType;
begin
  if Ctrl is TDesignField then exit(dctField);
  if Ctrl is TDesignHeading then exit(dctHeading);
  if (Ctrl is TDesignSection) or
     (Ctrl is TScrollBox) then exit(dctSection);
end;

{ TDesignPropertiesFrame }

procedure TDesignPropertiesFrame.SetEpiControl(
  const AValue: TEpiCustomControlItem);
begin
  FEpiControl := AValue;
end;

procedure TDesignPropertiesFrame.ShowHintMsg(const Msg: string; Ctrl: TControl);
begin
  if Assigned(OnShowHintMsg) then
    OnShowHintMsg(Self, Ctrl, Msg);
end;

procedure TDesignPropertiesFrame.ForceShow;
begin
  ShiftToTabSheet(1);
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
              bsNone:   SideBuf := {$IFDEF DARWIN}    6 {$ELSE} 0 {$ENDIF};
              bsSingle: SideBuf := {$IFDEF MSWINDOWS} 7 {$ELSE} 6 {$ENDIF};
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
  FSection.Name.RegisterOnChangeHook(@OnTextChange);
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

procedure TDesignSection.OnTextChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  Caption := EpiTextToControlText(FSection.Name.Text);
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

  if Assigned(FActiveFrame) and
     Assigned(FActiveFrame.EpiControl) then
    FActiveFrame.UpdateFormContent;
end;

procedure TDesignControlsForm.ApplyActionExecute(Sender: TObject);
begin
  if FActiveFrame.ValidateControl then
    MainForm.SetFocus;
end;

procedure TDesignControlsForm.CancelActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TDesignControlsForm.CloseActionExecute(Sender: TObject);
begin
  if FActiveFrame.ValidateControl then Close;
end;

procedure TDesignControlsForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'ControlsForm');
end;

procedure TDesignControlsForm.FormDeactivate(Sender: TObject);
var
  TmpForm: TCustomForm;
begin
  if not Showing then exit;

  if not (FActiveFrame.ValidateControl) then
    Self.SetFocus;
end;

procedure TDesignControlsForm.FormDestroy(Sender: TObject);
var
  B: Boolean;
begin
  B:=true;
  FormCloseQuery(nil, B);
end;

procedure TDesignControlsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key in [VK_0..VK_9]) and ([ssAlt] = Shift) then
    FActiveFrame.ShiftToTabSheet(Key - VK_0);
end;

procedure TDesignControlsForm.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  try
    BeginFormUpdate;

    if Assigned(FActiveFrame) then
    begin
      if FActiveFrame.EpiControl = AValue then
      begin
        FActiveFrame.UpdateFormContent;
        exit;
      end;

      if Assigned(FActiveFrame.FEpiControl) and
         (not FActiveFrame.ValidateControl) then exit;
      FActiveFrame.Parent := nil;
    end;

    if AValue is TEpiField then
      FActiveFrame := DesignPropertyFrames[0];
    if AValue is TEpiSection then
      FActiveFrame := DesignPropertyFrames[1];
    if AValue is TEpiHeading then
      FActiveFrame := DesignPropertyFrames[2];
    FActiveFrame.Align := alClient;
    FActiveFrame.Parent := self;

    FActiveFrame.EpiControl := AValue;
  finally
    EndFormUpdate;
  end;
end;

procedure TDesignControlsForm.ShowHintMsg(Sender: TDesignPropertiesFrame;
  Ctrl: TControl; const Msg: string);
var
  R: TRect;
  P: TPoint;
begin
  if (Msg = '') and (Ctrl = nil) then
  begin
    FHintWindow.Hide;
    exit;
  end;

  R := FHintWindow.CalcHintRect(0, Msg, nil);
  P := Ctrl.ClientToScreen(Point(0,0));
  OffsetRect(R, P.X, P.Y + Ctrl.Height + 2);
  FHintWindow.ActivateHint(R, Msg);
end;

procedure TDesignControlsForm.UpdateControlContent;
begin
  BeginFormUpdate;
  FActiveFrame.UpdateFormContent;
  EndFormUpdate;
end;

function TDesignControlsForm.ValidateControl: boolean;
begin
  if (not Showing) then exit(true);
  result := FActiveFrame.ValidateControl;
end;

constructor TDesignControlsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FHintWindow := THintWindow.Create(Self);
  FHintWindow.AutoHide := true;
  FHintWindow.HideInterval := 5 * 1000;
end;

constructor TDesignControlsForm.Create(TheOwner: TComponent;
  const EpiDocument: TEpiCustomBase);
var
  i: Integer;
begin
  Create(TheOwner);
  FEpiDocument := TEpiDocument(EpiDocument);

  DesignPropertyFrames[0] := TFieldPropertiesFrame.Create(Self, FEpiDocument.ValueLabelSets);
  DesignPropertyFrames[0].OnShowHintMsg := @ShowHintMsg;
  DesignPropertyFrames[1] := TSectionPropertiesFrame.Create(Self, FEpiDocument.Admin);
  DesignPropertyFrames[1].OnShowHintMsg := @ShowHintMsg;
  DesignPropertyFrames[2] := THeadingPropertiesFrame.Create(Self);
  DesignPropertyFrames[2].OnShowHintMsg := @ShowHintMsg;
end;

destructor TDesignControlsForm.Destroy;
begin
  FActiveFrame.Free;
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
  if not Assigned(FActiveFrame) then exit;

  Visible := true;
  FActiveFrame.ForceShow;
  BringToFront;
end;

end.

