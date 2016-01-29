unit admin_logviewer_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, VirtualTrees, epilogger,
  epidatafiles, epidocument;

type

  { TLogViewerFrame }

  TLogViewerFrame = class(TFrame)
  private
    FDocument: TEpiDocument;
    FLog: TEpiLog;
    FVst: TVirtualStringTree;
    procedure LogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure LogInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure SetDocument(AValue: TEpiDocument);
  public
    constructor Create(TheOwner: TComponent); override;
    property Document: TEpiDocument read FDocument write SetDocument;
//    property  Log: TEpiLogger read FLog write SetLog;
  end;

implementation

{$R *.lfm}

{ TLogViewerFrame }

procedure TLogViewerFrame.LogGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Idx: Integer;
  Df: TEpiDataFile;
  S: String;
begin
  Idx := PtrInt(FVst.GetNodeData(Node)^);

  with FLog do
    case Column of
      0: // Time
        CellText := DateTimeToStr(FTime.AsDateTime[Idx]);

      1: // Usernames
        CellText := FUserNames.AsString[Idx];

      2: // Cycle
        CellText := FCycle.AsString[Idx];

      3: // Type
        begin
          case FType.AsEnum[Idx] of
            ltNone: ;
            ltSuccessLogin: CellText := 'Login';
            ltFailedLogin:  CellText := 'Failed Login';
            ltSearch:       CellText := 'Search';
            ltNewRecord:    CellText := 'New Record';
            ltEditRecord:   CellText := 'Edit Record';
            ltViewRecord:   CellText := 'View Record';
            ltPack:         CellText := 'Pack';
            ltAppend:       CellText := 'Append';
            ltExport:       CellText := 'Export';
            ltClose:        CellText := 'Closed';
          else
            CellText := 'Unimplemented Log Feaure';
          end;
        end;

      4: // DataFiles
        begin
          S := FDataFileNames.AsString[Idx];
          Df := FDocument.DataFiles.GetDataFileByName(S);
          if Assigned(Df) then
            CellText := Df.Caption.Text
          else
            CellText := S;
        end;

      5: // Keys
        begin
          if (FType.AsEnum[Idx] in [ltNewRecord, ltEditRecord, ltViewRecord]) then
          begin
            S := FDataFileNames.AsString[Idx];
            Df := FDocument.DataFiles.GetDataFileByName(S);
            if Assigned(Df) and
               (Df.KeyFields.Count > 0)
            then
              CellText := FKeyFieldValues.AsString[Idx]
            else
              CellText := 'RecNo: ' + FKeyFieldValues.AsString[Idx];
          end;
        end;
      6: // Content
        begin
          case FType.AsEnum[Idx] of
            ltSuccessLogin,
            ltFailedLogin:
              CellText := 'Hostname = ' + FLogContent.AsString[Idx];
            ltSearch:
              CellTExt := 'Search string: ' + FLogContent.AsString[Idx];
            ltClose:
              CellText := 'Last Edit = ' + FLogContent.AsString[Idx];
{            ltNone: ;
            ltNewRecord: ;
            ltEditRecord: ;
            ltViewRecord: ;
            ltPack: ;
            ltAppend: ;
            ltExport: ;}
          end;
        end;
    end;
end;

procedure TLogViewerFrame.LogInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  PtrInt(FVst.GetNodeData(Node)^) := Node^.Index;
end;

procedure TLogViewerFrame.SetDocument(AValue: TEpiDocument);
begin
  if FDocument = AValue then Exit;
  FDocument := AValue;
  FLog := FDocument.Logger.Log;

  FVst.RootNodeCount := FLog.Size;
  FVSt.Header.AutoFitColumns(false);
end;

constructor TLogViewerFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FVst := TVirtualStringTree.Create(Self);
  with FVst do
  begin
    BeginUpdate;

    NodeDataSize := SizeOf(Integer);

    with TreeOptions do
    begin
      AnimationOptions := [];
      AutoOptions      := [];
      MiscOptions      := [toFullRepaintOnResize, toGridExtensions, toWheelPanning];
      PaintOptions     := [toShowButtons, toShowVertGridLines, toFullVertGridLines,
                           toShowHorzGridLines,
                           toThemeAware, toUseBlendedImages];
      SelectionOptions := [toExtendedFocus, toFullRowSelect];
      StringOptions    := [];
    end;

    with Header do
    begin
      Options := [hoAutoResize, hoColumnResize, hoDblClickResize, hoVisible,
                  hoFullRepaintOnResize];

      with Columns.Add do
      begin
        Text := 'Time';
        CheckBox   := false;
        CheckType  := ctNone;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize, coAllowFocus];
        Width      := 150;
      end;

      with Columns.Add do
      begin
        Text := 'User';
        CheckBox   := false;
        CheckType  := ctNone;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize, coAllowFocus];
        Width      := 150;
      end;

      with Columns.Add do
      begin
        Text := 'Cycle';
        CheckBox   := false;
        CheckType  := ctNone;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize, coAllowFocus];
        Width      := 150;
      end;

      with Columns.Add do
      begin
        Text := 'Type';
        CheckBox   := false;
        CheckType  := ctNone;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize, coAllowFocus];
        Width      := 150;
      end;

      with Columns.Add do
      begin
        Text := 'Dataform';
        CheckBox   := false;
        CheckType  := ctNone;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize, coAllowFocus];
        Width      := 150;
      end;

      with Columns.Add do
      begin
        Text := 'Keys/RecNo';
        CheckBox   := false;
        CheckType  := ctNone;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize, coAllowFocus];
        Width      := 150;
      end;

      with Columns.Add do
      begin
        Text := 'Content';
        CheckBox   := false;
        CheckType  := ctNone;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize, coAllowFocus];
        Width      := 150;
      end;

      MainColumn := 0;
      AutoSizeIndex := 8;
    end;

    Align := alClient;
    Parent := Self;

//    OnBeforeItemErase := @UsersBeforeItemErase;
    OnGetText  := @LogGetText;
    OnInitNode := @LogInitNode;
//    OnNodeDblClick    := @UsersNodeDblClick;

    EndUpdate;
  end;
end;

end.

