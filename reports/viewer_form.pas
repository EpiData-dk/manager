unit viewer_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Htmlview, Htmlsubs;

type

  { THtmlViewerForm }

  THtmlViewerForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    HTMLViewer1: THTMLViewer;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure SetHtml(Const HtmlString: string);
  end; 

implementation

uses
  LCLType;
{$R *.lfm}

{ THtmlViewerForm }

procedure THtmlViewerForm.Button1Click(Sender: TObject);
var
  Ss: TStringStream;
  Fs: TFileStream;
begin
  if not SaveDialog1.Execute then exit;

  Ss := TStringStream.Create(HTMLViewer1.DocumentSource);
  Ss.Position := 0;
  Fs := TFileStream.Create(SaveDialog1.FileName, fmCreate);
  Fs.CopyFrom(Ss, ss.Size);
  Fs.Free;
  Ss.Free;
end;

procedure THtmlViewerForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Shift = []) then Close;
end;

constructor THtmlViewerForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure THtmlViewerForm.SetHtml(const HtmlString: string);
begin
  HTMLViewer1.BeginUpdateBounds;
  HTMLViewer1.LoadFromString(HtmlString);
  HTMLViewer1.EndUpdateBounds;
end;

end.

