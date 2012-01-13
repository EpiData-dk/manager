unit export_stata_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  StdCtrls, ExtCtrls, export_frame_types, epiexportsettings,
  epimiscutils;

type

  { TExportStataFrame }

  TExportStataFrame = class(TFrame, IExportSettingsPresenterFrame)
    VersionComboBox: TComboBox;
    Label4: TLabel;
    FieldNamingRGrp: TRadioGroup;
  private
    { private declarations }
    LocalFrame: TFrame;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    function UpdateExportSetting(Setting: TEpiExportSetting): boolean;
    function GetFrameCaption: string;
    function GetExportName: string;
    function GetFileDialogExtensions: TEpiDialogFilters;
  end;

implementation

{$R *.lfm}

uses
  export_customvaluelabel_frame, epieximtypes, export_form;

{ TExportStataFrame }

constructor TExportStataFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  LocalFrame := TCustomValueLabelFrame.Create(self);
  LocalFrame.Parent := self;
  LocalFrame.AnchorToNeighbour(akTop, 10, FieldNamingRGrp);
  LocalFrame.AnchorParallel(akLeft, 20, Self);
  LocalFrame.AnchorParallel(akRight, 20, Self);
  LocalFrame.AnchorParallel(akBottom, 10, Self);

  with VersionComboBox.Items do
  begin
    Clear;
    AddObject('Stata 4',      TObject(dta4));
    AddObject('Stata 5, 6',   TObject(dta6));
    AddObject('Stata 7',      TObject(dta7));
    AddObject('Stata 8, 9',   TObject(dta8));
    AddObject('Stata 10, 11', TObject(dta10));
    AddObject('Stata 12',     TObject(dta12));
  end;
  VersionComboBox.ItemIndex := 0;

  with FieldNamingRGrp.Items do
  begin
    Clear;
    AddObject('UPPERCASE', TObject(fncUpper));
    AddObject('lowercase', TObject(fncLower));
    AddObject('Leave As Is', TObject(fncAsIs));
  end;
  FieldNamingRGrp.ItemIndex := FieldNamingRGrp.Items.Count - 1;
end;

function TExportStataFrame.UpdateExportSetting(Setting: TEpiExportSetting): boolean;
begin
  with TEpiStataExportSetting(Setting) do
  begin
    Version := TEpiStataVersion(PtrUInt(VersionComboBox.Items.Objects[VersionComboBox.ItemIndex]));
    FieldNameCase := TEpiStataFieldNamingCase(PtrUInt(FieldNamingRGrp.Items.Objects[VersionComboBox.ItemIndex]));
  end;

  result := result and
    (LocalFrame as IExportSettingsFrame).UpdateExportSetting(Setting);
end;

function TExportStataFrame.GetFrameCaption: string;
begin
  result := 'Stata Options';
end;

function TExportStataFrame.GetExportName: string;
begin
  result := 'Stata';
end;

function TExportStataFrame.GetFileDialogExtensions: TEpiDialogFilters;
begin
  result := [dfDTA];
end;

initialization
  RegisterExportFrame(TExportStataFrame, TEpiStataExportSetting);

end.

