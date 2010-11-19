unit design_heading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, epidatafiles, epicustombase, design_custombase,
  AVL_Tree, design_controls;

type

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

  { TDesignHeadingForm }

  TDesignHeadingForm = class(TDesignCustomForm)
    CancelBtn: TBitBtn;
    CaptionEdit: TEdit;
    OkBtn: TBitBtn;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FHeading: TEpiHeading;
  protected
    function GetEpiControl: TEpiCustomControlItem; override;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem); override;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end; 

var
  DesignHeadingForm: TDesignHeadingForm;

implementation

{$R *.lfm}

uses
  LCLProc;

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

{ TDesignHeadingForm }

procedure TDesignHeadingForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ModalResult <> mrOK then exit;

  CanClose := false;
  if UTF8Length(CaptionEdit.Text) = 0 then exit;
  CanClose := true;

  FHeading.BeginUpdate;
  FHeading.Caption.Text := CaptionEdit.Text;
  FHeading.EndUpdate;
end;

procedure TDesignHeadingForm.FormShow(Sender: TObject);
begin
  CaptionEdit.SetFocus;
end;

function TDesignHeadingForm.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FHeading;
end;

procedure TDesignHeadingForm.SetEpiControl(const AValue: TEpiCustomControlItem
  );
begin
  FHeading := TEpiHeading(AValue);
  CaptionEdit.Text := FHeading.Caption.Text;
end;

constructor TDesignHeadingForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnCloseQuery := @FormCloseQuery;
end;

destructor TDesignHeadingForm.Destroy;
begin
  inherited Destroy;
end;

end.

