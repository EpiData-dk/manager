unit design_section;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, epidatafiles, epicustombase, design_custombase;

type

  { TDesignSection }

  TDesignSection = Class(TGroupBox, IDesignEpiControl)
  private
    FSection: TEpiSection;
    function GetEpiControl: TEpiCustomControlItem;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure OnChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
  public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; override;
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;

  { TDesignSectionForm }

  TDesignSectionForm = class(TDesignCustomForm)
    BitBtn1: TBitBtn;
  private
    { private declarations }
    FSection: TEpiSection;
  protected
    function GetEpiControl: TEpiCustomControlItem; override;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end; 

var
  DesignSectionForm: TDesignSectionForm;

implementation

{$R *.lfm}

{ TDesignSection }

function TDesignSection.GetEpiControl: TEpiCustomControlItem;
begin
  result := FSection;
end;

procedure TDesignSection.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FSection := TEpiSection(AValue);
  FSection.RegisterOnChangeHook(@OnChange);
end;

procedure TDesignSection.OnChange(Sender: TObject; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer);
begin
  case EventGroup of
    eegCustomBase:
      case TEpiCustomChangeEventType(EventType) of
        ecceSetLeft: Left := FSection.Left;
        ecceSetTop:  Top  := FSection.Top;
        ecceUpdate:
          begin
            Left := FSection.Left;
            Top  := FSection.Top;
          end;
      end;
  end;
end;

constructor TDesignSection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDesignSection.Destroy;
begin
  inherited Destroy;
end;

{ TDesignSectionForm }

function TDesignSectionForm.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FSection;
end;

procedure TDesignSectionForm.SetEpiControl(const AValue: TEpiCustomControlItem
  );
begin
  FSection := TEpiSection(AValue);
end;

constructor TDesignSectionForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TDesignSectionForm.Destroy;
begin
  inherited Destroy;
end;

end.

