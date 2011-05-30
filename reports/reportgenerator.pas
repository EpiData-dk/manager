unit reportgenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ActnList;

type

  { TReportGeneratorForm }

  TReportGeneratorForm = class(TForm)
    MoveItemLeftAction: TAction;
    MoveItemRightAction: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    AvailableReportItemList: TListBox;
    UsedReportItemList: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end; 

var
  ReportGeneratorForm: TReportGeneratorForm;

implementation

{$R *.lfm}

uses
  epireport_valuelabels, epireport_databasegrid,
  epireport_fieldlist_simple;

{ TReportGeneratorForm }

constructor TReportGeneratorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  AvailableReportItemList.Items.AddObject('List of Valuelabels', TObject(TEpiReportValueLabelsHtml));
  AvailableReportItemList.Items.AddObject('List of Datasets', TObject(TEpiReportDataSetsGridHtml));
  AvailableReportItemList.Items.AddObject('List of Fields (simple)', TObject(TEpiReportSimpleFieldListHtml));
end;

end.

