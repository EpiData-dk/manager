unit workflow_frame;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls;

type

  { TWorkFlowFrame }

  TWorkFlowFrame = class(TFrame)
    BackgroundImage: TImage;
    DesignBtnImage: TImage;
    ModifyBtnImage: TImage;
    ExtendBtnImage: TImage;
    DocumentBtnImage: TImage;
    AdvBtnImage: TImage;
    BgPanel: TPanel;
    procedure FrameResize(Sender: TObject);
  private
    { private declarations }
    CircleIdleStream: TMemoryStream;
    CircleOverStrean: TMemoryStream;
    CircleDownStream: TMemoryStream;
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

constructor TWorkFlowFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

{  CircleIdleStream := TMemoryStream.Create;
  CircleIdleStream.LoadFromFile('images/circleidle.png');
  CircleOverStrean := TMemoryStream.Create;
  CircleOverStrean.LoadFromFile('images/circleover.png');
  CircleDownStream := TMemoryStream.Create;
  CircleDownStream.LoadFromFile('images/circledown.png');     }
  DesignBtnImage.OnClick := MainForm.NewDataFormBtn.OnClick;
  AdvBtnImage.OnClick := MainForm.EditorBtn.OnClick;
end;

initialization
  {$I workflow_frame.lrs}

end.

