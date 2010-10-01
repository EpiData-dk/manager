unit design_field;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, MaskEdit, ComCtrls, epidatafiles, epicustombase,
  design_custombase, AVL_Tree, episettings;

type
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

  { TDesignFieldForm }

  TDesignFieldForm = class(TDesignCustomForm)
    CancelBtn: TBitBtn;
    NameEdit: TEdit;
    PageControl1: TPageControl;
    QuestionEdit: TEdit;
    LengthEdit: TEdit;
    DecimalsEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LengthLabel: TLabel;
    DecimalsLabel: TLabel;
    OkBtn: TBitBtn;
    Panel1: TPanel;
    BasicSheet: TTabSheet;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FField: TEpiField;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  protected
    procedure SetEpiControl(const AValue: TEpiCustomControlItem); override;
    function GetEpiControl: TEpiCustomControlItem; override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end; 

implementation

{$R *.lfm}

uses
  design_section, epidatafilestypes, settings,
  epidocument;

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

{ TDesignFieldForm }

procedure TDesignFieldForm.FormShow(Sender: TObject);
begin
  if not DecimalsEdit.Visible then
    Height :=
      (PageControl1.Height - Panel1.ClientHeight) +
      LengthEdit.Top +
      LengthEdit.Height +  // Bottom of length edit :)
      (OkBtn.Height - LengthEdit.Height) +
      8;                   // A little additional spacing below...
  if not LengthEdit.Visible then
    Height :=
      (PageControl1.Height - Panel1.ClientHeight) +
      QuestionEdit.Top +
      QuestionEdit.Height +  // Bottom of question edit :)
      8 +                    // A little additional spacing above...
      OkBtn.Height +         // get to bottom of button.
      8;                     // A little additional spacing below...

  QuestionEdit.SetFocus;
end;

procedure TDesignFieldForm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
var
  NewLen: LongInt;
  NewDecLen: LongInt;
begin
  if ModalResult <> mrOK then exit;

  // Rules for field creation!
  NewLen := FField.Length;
  if LengthEdit.Visible then
  begin
    NewLen := StrToInt(LengthEdit.Text);
    if ((FField.FieldType = ftInteger) and (NewLen >= 19)) or
       (NewLen <= 0)
    then
    begin
      LengthEdit.SetFocus;
      CanClose := false;
      exit;
    end;
  end;

  NewDecLen := FField.Decimals;
  if DecimalsEdit.Visible then
  begin
    NewDecLen := StrToInt(DecimalsEdit.Text);
    if (NewDecLen <= 0) then
    begin
      DecimalsEdit.SetFocus;
      CanClose := false;
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
      CanClose := false;
      FField.EndUpdate;
      Exit;
    end;
  end;
  FField.Length := NewLen;
  FField.Decimals := NewDecLen;
  if NewDecLen > 0 then
    FField.Length := FField.Length + FField.Decimals + 1;
  FField.Question.Caption.Text := QuestionEdit.Text;

  FField.EndUpdate;
end;

procedure TDesignFieldForm.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FField := TEpiField(AValue);
  if not Assigned(FField) then exit;

  NameEdit.Text := FField.Name;
  QuestionEdit.Text := FField.Question.Caption.Text;

  LengthEdit.Text := IntToStr(FField.Length);
  Case FField.FieldType of
    ftFloat:
      begin
        DecimalsLabel.Visible := true;
        DecimalsEdit.Visible := true;
        LengthEdit.Text := IntToStr(FField.Length - (FField.Decimals + 1));
        DecimalsEdit.Text := IntToStr(FField.Decimals);
      end;
    ftBoolean,
    ftDMYDate, ftDMYToday,
    ftMDYDate, ftMDYToday,
    ftYMDDate, ftYMDToday,
    ftTime, ftTimeNow:
      begin
        LengthLabel.Visible := false;
        LengthEdit.Visible := false;
      end;
  end;
end;

function TDesignFieldForm.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FField;
end;

constructor TDesignFieldForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnCloseQuery := @FormCloseQuery;
end;

destructor TDesignFieldForm.Destroy;
begin
  inherited Destroy;
end;

end.

