unit workflow_frame;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls;

type

  { TWorkFlowFrame }

  TWorkFlowFrame = class(TFrame)
    BackgroundImage: TImage;
    PrepareBtnImage: TImage;
    DesignBtnImage: TImage;
    ModifyBtnImage: TImage;
    ExtendBtnImage: TImage;
    DocumentBtnImage: TImage;
    AdvBtnImage: TImage;
    BgPanel: TPanel;
    procedure FrameResize(Sender: TObject);
    procedure PrepareBtnImageClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end; 

implementation

uses
  main;

{ TWorkFlowFrame }

procedure TWorkFlowFrame.FrameResize(Sender: TObject);
begin
  BgPanel.Left := (Width div 2) - (BgPanel.Width div 2);
  BgPanel.Top := (Height div 2) - (BgPanel.Height div 2);
end;

procedure TWorkFlowFrame.PrepareBtnImageClick(Sender: TObject);
begin
  // shellexecute
end;

constructor TWorkFlowFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  DesignBtnImage.OnClick := MainForm.NewDataFormBtn.OnClick;
  AdvBtnImage.OnClick := MainForm.EditorBtn.OnClick;
end;

initialization
  {$I workflow_frame.lrs}

end.

