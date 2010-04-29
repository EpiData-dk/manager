unit design_heading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, epidatafiles, epicustombase, design_custombase;

type

  { TDesignHeading }

  TDesignHeading = Class(TLabel, IDesignEpiControl)
  private
    FHeading: TEpiHeading;
    function GetEpiControl: TEpiCustomControlItem;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure OnChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
  public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; override;
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;

  { TDesignHeadingForm }

  TDesignHeadingForm = class(TDesignCustomForm)
    BitBtn1: TBitBtn;
  private
    { private declarations }
    FHeading: TEpiHeading;
  protected
    function GetEpiControl: TEpiCustomControlItem; override;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem); override;
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
  FHeading.RegisterOnChangeHook(@OnChange);
end;

procedure TDesignHeading.OnChange(Sender: TObject; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer);
begin
  case EventGroup of
    eegCustomBase:
      case TEpiCustomChangeEventType(EventType) of
        ecceSetLeft: Left := FHeading.Left;
        ecceSetTop:  Top  := FHeading.Top;
        ecceUpdate:
          begin
            Left := FHeading.Left;
            Top  := FHeading.Top;
          end;
      end;
  end;
end;

constructor TDesignHeading.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDesignHeading.Destroy;
begin
  inherited Destroy;
end;

{ TDesignHeadingForm }

function TDesignHeadingForm.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FHeading;
end;

procedure TDesignHeadingForm.SetEpiControl(const AValue: TEpiCustomControlItem
  );
begin
  FHeading := TEpiHeading(AValue);
end;

constructor TDesignHeadingForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TDesignHeadingForm.Destroy;
begin
  inherited Destroy;
end;

end.

