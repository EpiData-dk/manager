unit design_field;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, MaskEdit, ComCtrls, epidatafiles, epicustombase,
  design_custombase, AVL_Tree;

type
  { TDesignField }

  TDesignField = class(TEdit, IDesignEpiControl)
  private
    // IDesignEpiControl
    FField: TEpiField;
    FXTreeNode: TAVLTreeNode;
    FYTreeNode: TAVLTreeNode;
    function GetEpiControl: TEpiCustomControlItem;
    function GetXTreeNode: TAVLTreeNode;
    function GetYTreeNode: TAVLTreeNode;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure SetXTreeNode(const AValue: TAVLTreeNode);
    procedure SetYTreeNode(const AValue: TAVLTreeNode);
  private
    FNameLabel: TLabel;
    FQuestionLabel: TLabel;
    procedure   OnFieldChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure   OnQuestionChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure   UpdateNameLabel;
    procedure   UpdateHint;
  protected
    procedure   SetParent(NewParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
    property    XTreeNode: TAVLTreeNode read GetXTreeNode write SetXTreeNode;
    property    YTreeNode: TAVLTreeNode read GetYTreeNode write SetYTreeNode;
  end;

  { TDesignFieldForm }

  TDesignFieldForm = class(TDesignCustomForm)
    CancelBtn: TBitBtn;
    Label5: TLabel;
    NameEdit: TEdit;
    IdEdit: TEdit;
    PageControl1: TPageControl;
    QuestionEdit: TEdit;
    LengthEdit: TEdit;
    DecimalsEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
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
  design_section, epidatafilestypes, settings;

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
begin
  FField := TEpiField(AValue);
  FField.RegisterOnChangeHook(@OnFieldChange);
  FField.Question.RegisterOnChangeHook(@OnQuestionChange);
  Name := FField.Id;
  Caption := '';
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
            FNameLabel.Caption        := FField.Name.Text;

            Left                      := FField.Left;
            FField.Question.Left      := FField.Left - (FQuestionLabel.Width + 5);
            Top                       := FField.Top;
            FField.Question.Top       := FField.Top;
            if Self.Parent is TScrollBox then
              Width                   := TScrollBox(Self.Parent).Canvas.GetTextWidth('W') * FField.Length
            else
              Width                   := TScrollBox(Self.Parent.Parent).Canvas.GetTextWidth('W') * FField.Length;
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
        ecceText: FNameLabel.Caption  := FField.Name.Text;
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

procedure TDesignField.UpdateNameLabel;
begin
  FNameLabel.Left := FQuestionLabel.Left - (FNameLabel.Width + 5);
  FNameLabel.Top := FQuestionLabel.Top;
end;

procedure TDesignField.UpdateHint;
begin
  With FField do
    Hint := WideFormat(
      'Id: %s' + LineEnding +
      'Name: %s' + LineEnding +
      'Type: %s' + LineEnding +
      'Length: %d' + LineEnding +
      'Question: %s' + LineEnding +
      'X: %d, Y: %d',
      [UTF8Decode(Id), UTF8Decode(Name.Text), '',
       Length, UTF8Decode(Question.Caption.Text), Left, Top]
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

  if not ManagerSettings.ShowFieldBorder then
    BorderStyle := bsNone;
  if not ManagerSettings.ShowFieldNamesInLabel then
    FNameLabel.Visible := false;
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
  QuestionEdit.SetFocus;
end;

procedure TDesignFieldForm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  if ModalResult <> mrOK then exit;

  FField.BeginUpdate;

  FField.Id := IdEdit.Text;
  FField.Name.Text := NameEdit.Text;
  FField.Question.Caption.Text := QuestionEdit.Text;
  FField.Length := StrToInt(LengthEdit.Text);
  FField.Decimals := StrToInt(DecimalsEdit.Text);
  if FField.Decimals > 0 then
    FField.Length := FField.Length + FField.Decimals + 1;

  FField.EndUpdate;
end;

procedure TDesignFieldForm.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FField := TEpiField(AValue);
  if not Assigned(FField) then exit;

  IdEdit.Text := FField.Id;
  NameEdit.Text := FField.Name.Text;
  QuestionEdit.Text := FField.Question.Caption.Text;
  if FField.FieldType = ftFloat then
    LengthEdit.Text := IntToStr(FField.Length - (FField.Decimals + 1))
  else
    LengthEdit.Text := IntToStr(FField.Length);
  DecimalsEdit.Text := IntToStr(FField.Decimals);
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

