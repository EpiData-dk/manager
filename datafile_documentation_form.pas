unit datafile_documentation_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ActnList, UEpiDataFile;

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
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FDatafile: TEpiDatafile;
    LocalUpdating: Boolean;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; aDatafile: TEpiDataFile);
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

procedure TDatafileDocumentationForm.FormShow(Sender: TObject);
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
begin
  inherited Create(TheOwner);
  FDatafile := aDatafile;
  Caption := 'Documenting: ' +
    Datafile.FileName;
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

