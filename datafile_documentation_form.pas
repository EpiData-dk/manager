unit datafile_documentation_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ActnList, UEpiDataFile, UDataFileTypes;

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
      EventType: TEpiDataFileChangeEventType; OldValue: EpiVariant);
    procedure DataFileFieldChange(Sender: TObject;
      EventType: TEpiFieldChangeEventType; OldValue: EpiVariant);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; aDatafile: TEpiDataFile);
    destructor Destroy; override;
    procedure ForceUpdate;
    property Datafile: TEpiDatafile read FDatafile;
  end; 

var
  DatafileDocumentationForm: TDatafileDocumentationForm;

implementation

function SortFields(Item1, Item2: Pointer): integer;
var
  Field1: TEpiField absolute Item1;
  Field2: TEpiField absolute Item2;
begin
  result := 0;
  if Field1 = Field2 then
    exit;

  if Field1.FieldY < Field2.FieldY then
    result := -1
  else if Field1.FieldY > Field2.FieldY then
    result := 1
  else
    if Field1.FieldX < Field2.FieldX then
      result := -1
    else if Field1.FieldX > Field2.FieldX then
      result := 1
end;

{ TDatafileDocumentationForm }

procedure TDatafileDocumentationForm.DataFileChange(Sender: TObject;
  EventType: TEpiDataFileChangeEventType; OldValue: EpiVariant);
begin
  ForceUpdate;
end;

procedure TDatafileDocumentationForm.DataFileFieldChange(Sender: TObject;
  EventType: TEpiFieldChangeEventType; OldValue: EpiVariant);
begin
  ForceUpdate;
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
var
  i: Integer;
begin
  inherited Create(TheOwner);
  FDatafile := aDatafile;
  FDatafile.RegisterOnChangeHook(@DataFileChange);

  for i := 0 to FDatafile.NumFields - 1 do
    FDatafile[i].RegisterOnChangeHook(@DataFileFieldChange);

  Caption := 'Documenting: ' +
    Datafile.FileName;
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

