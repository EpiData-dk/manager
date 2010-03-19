unit datafile_documentation_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ActnList, epidatafile, epidatatypes;

type

  { TDatafileDocumentationForm }

  TDatafileDocumentationForm = class(TForm)
    CloseAction: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure CloseActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { private declarations }
    FDatafile: TEpiDatafile;
    LocalUpdating: Boolean;
    procedure DataFileChange(Sender: TObject;
      EventType: TEpiDataFileChangeEventType; OldValue: Pointer);
    procedure DataFileFieldChange(Sender: TObject;
      EventType: TEpiFieldChangeEventType; OldValue: Pointer);
    procedure SetDatafile(const AValue: TEpiDatafile);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; aDatafile: TEpiDataFile);
    destructor Destroy; override;
    procedure ForceUpdate;
    property Datafile: TEpiDatafile read FDatafile write SetDatafile;
  end; 

implementation

uses
  Main;

function SortFields(Item1, Item2: Pointer): integer;
var
  Field1: TEpiField absolute Item1;
  Field2: TEpiField absolute Item2;
begin
  result := 0;
  if Field1 = Field2 then
    exit;

  if Field1.FieldTop < Field2.FieldTop then
    result := -1
  else if Field1.FieldTop > Field2.FieldTop then
    result := 1
  else
    if Field1.FieldLeft < Field2.FieldLeft then
      result := -1
    else if Field1.FieldLeft > Field2.FieldLeft then
      result := 1
end;

{ TDatafileDocumentationForm }

procedure TDatafileDocumentationForm.DataFileChange(Sender: TObject;
  EventType: TEpiDataFileChangeEventType; OldValue: Pointer);
begin
  // If a new field is added, we need to register a hook to it.
  // - otherwise changes made to the field will not be updated on the form.
  if EventType = dceAddField then
  with TEpiDataFile(Sender) do
    Field[FieldCount-1].RegisterOnChangeHook(@DataFileFieldChange);

  if EventType = dceName then
    Caption := 'Documenting: ' + TEpiDataFile(Sender).FileName;

  ForceUpdate;
end;

procedure TDatafileDocumentationForm.DataFileFieldChange(Sender: TObject;
  EventType: TEpiFieldChangeEventType; OldValue: Pointer);
begin
  ForceUpdate;
end;

procedure TDatafileDocumentationForm.SetDatafile(const AValue: TEpiDatafile);
var
  i: Integer;
begin
  FDatafile := AValue;
  Datafile.RegisterOnChangeHook(@DataFileChange);

  for i := 0 to FDatafile.FieldCount - 1 do
    Datafile[i].RegisterOnChangeHook(@DataFileFieldChange);
end;

procedure TDatafileDocumentationForm.CloseActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TDatafileDocumentationForm.FormActivate(Sender: TObject);
begin
  ForceUpdate;
end;

constructor TDatafileDocumentationForm.Create(TheOwner: TComponent; aDatafile: TEpiDataFile);
begin
  inherited Create(TheOwner);
  Datafile := aDatafile;

  Caption := 'Documenting: ' + MainForm.PageControl1.ActivePage.Caption;
end;

destructor TDatafileDocumentationForm.Destroy;
begin
  FDatafile.UnRegisterOnChangeHook(@DataFileChange);
  inherited Destroy;
end;

procedure TDatafileDocumentationForm.ForceUpdate;
begin
  if LocalUpdating then exit;
  LocalUpdating := true;
  Datafile.SortFields(@SortFields);
  Memo1.Lines.Assign(Datafile.DocumentDatafile);
  LocalUpdating := false;
end;

initialization
  {$I datafile_documentation_form.lrs}

end.

