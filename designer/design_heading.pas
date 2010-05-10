unit design_heading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, epidatafiles, epicustombase, design_custombase;

type

  { TDesignHeading }

  TDesignHeading = Class(TLabel, IDesignEpiControl)
  private
    FHeading: TEpiHeading;
    function GetEpiControl: TEpiCustomControlItem;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure OnHeadingChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure OnCaptionChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure UpdateHint;
  public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; override;
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;

  { TDesignHeadingForm }

  TDesignHeadingForm = class(TDesignCustomForm)
    CancelBtn: TBitBtn;
    IdEdit: TEdit;
    Label1: TLabel;
    Label5: TLabel;
    CaptionEdit: TEdit;
    OkBtn: TBitBtn;
    Panel1: TPanel;
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

{ TDesignHeading }

function TDesignHeading.GetEpiControl: TEpiCustomControlItem;
begin
  result := FHeading;
end;

procedure TDesignHeading.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FHeading := TEpiHeading(AValue);
  FHeading.RegisterOnChangeHook(@OnHeadingChange);
  FHeading.Caption.RegisterOnChangeHook(@OnCaptionChange);
  Name := FHeading.Id;
  Caption := '';
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

  Caption := TEpiTranslatedText(Sender).Text;
  UpdateHint;
end;

procedure TDesignHeading.UpdateHint;
begin
  with FHeading do
    Hint := WideFormat(
      'Id: %s' + LineEnding +
//      'Name: %s' + LineEnding +
      'Caption: %s' + LineEnding +
      'X: %d, Y: %d',
      [UTF8Decode(Id),
//       UTF8Decode(Name.Text),
       UTF8Decode(Caption.Text),
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
  ParentColor := false;
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

  FHeading.BeginUpdate;

  FHeading.Id := IdEdit.Text;
  FHeading.Caption.Text := CaptionEdit.Text;

  FHeading.EndUpdate;
end;

function TDesignHeadingForm.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FHeading;
end;

procedure TDesignHeadingForm.SetEpiControl(const AValue: TEpiCustomControlItem
  );
begin
  FHeading := TEpiHeading(AValue);
  IdEdit.Text := FHeading.Id;
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

