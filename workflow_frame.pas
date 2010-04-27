unit workflow_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, StdCtrls, Controls;

type

  { TWorkFlowFrame }

  TWorkFlowFrame = class(TFrame)
    EditorBtnImage: TImage;
    BackgroundImage: TImage;
    BgPanel: TPanel;
    DesignBtnImage: TImage;
    DocumentBtnImage: TImage;
    ExtendBtnImage: TImage;
    WorkFlowImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FrameResize(Sender: TObject);
    procedure ImgBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImgBtnMouseEnter(Sender: TObject);
    procedure ImgBtnMouseLeave(Sender: TObject);
    procedure ImgBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    procedure LoadImage(Img: TImage; Idx: Integer);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end; 

implementation

{$R *.lfm}

uses
  main, Graphics;

{ TWorkFlowFrame }

procedure TWorkFlowFrame.FrameResize(Sender: TObject);
begin
  BgPanel.Left := (Width div 2) - (BgPanel.Width div 2);
  BgPanel.Top := (Height div 2) - (BgPanel.Height div 2);
end;

procedure TWorkFlowFrame.ImgBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  LoadImage(TImage(Sender), 0);
end;

procedure TWorkFlowFrame.ImgBtnMouseEnter(Sender: TObject);
begin
  LoadImage(TImage(Sender), 2);
end;

procedure TWorkFlowFrame.ImgBtnMouseLeave(Sender: TObject);
begin
  LoadImage(TImage(Sender), 1);
end;

procedure TWorkFlowFrame.ImgBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  LoadImage(TImage(Sender), 2);
end;

procedure TWorkFlowFrame.LoadImage(Img: TImage; Idx: Integer);
var
  Bmp: TBitmap;
begin
  // Imagelist:
  //  0 = Down
  //  1 = Idle
  //  2 = Over
  Bmp := TBitmap.Create;
  WorkFlowImageList.GetBitmap(Idx, Bmp);
  Img.Picture.Bitmap := bmp;
  Bmp.Free;
end;

constructor TWorkFlowFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  LoadImage(DesignBtnImage, 1);
  LoadImage(ExtendBtnImage, 1);
  LoadImage(DocumentBtnImage, 1);
  LoadImage(EditorBtnImage, 1);

  DesignBtnImage.Action := MainForm.NewProjectAction;
end;

end.

