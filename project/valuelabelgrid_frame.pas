unit valuelabelgrid_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, VirtualTrees, epivaluelabels,
  epidatafilestypes, LCLType, ComCtrls, design_types, epicustombase, LMessages,
  manager_messages;

type

  { TValueLabelGridFrame }

  TValueLabelGridFrame = class(TFrame)
    DelLineBtn: TToolButton;
    NewLineBtn: TToolButton;
    ToolBar1: TToolBar;
    procedure DelLineBtnClick(Sender: TObject);
    procedure NewLineBtnClick(Sender: TObject);
  private
    { StringTree privates }
    FVLG: TVirtualStringTree;
    procedure DoAddLine;
    procedure VLGChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VLGChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure VLGEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VLGEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure VLGEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VLGFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VLGFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure VLGGetNodeText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VLGInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VLGKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure VLGSendPostEdit(Data: PtrInt);
    procedure VLGSetNodeText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
    procedure VLGUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    { Other frame parts }
    FOnShowHintMsg: TDesignFrameShowHintEvent;
    FValueLabelSet: TEpiValueLabelSet;
    procedure InternalSetup;
    procedure UpdateGrid;
    procedure EventHook(Const Sender, Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure SetValueLabelSet(AValue: TEpiValueLabelSet);
    function  ValueLabelFromNode(Node: PVirtualNode): TEpiCustomValueLabel;
    procedure NodeError(Node: PVirtualNode; Column: TColumnIndex; Const Msg: string);
    procedure DoShowHintMsg(Ctrl: TControl; Const Msg: String); overload;
    procedure DoShowHintMsg(R: TRect; Const Msg: string); overload;
    procedure LM_NewValueLabel(Var Msg: TLMessage); message LM_VLG_NEWVALUELABEL;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    function    ValidateGridEntries: boolean;
    property    ValueLabelSet: TEpiValueLabelSet read FValueLabelSet write SetValueLabelSet;
    property    VLG: TVirtualStringTree read FVLG;
    property    OnShowHintMsg: TDesignFrameShowHintEvent read FOnShowHintMsg write FOnShowHintMsg;
  end; 

implementation

{$R *.lfm}

uses
  Graphics, Dialogs, LazUTF8, epimiscutils, Math, LCLIntf, Clipbrd, StdCtrls,
  strutils, admin_authenticator, epiadmin;

type
  PEpiValueLabel = ^TEpiCustomValueLabel;

  { TValidatedStringEditLink }

  TValidatedStringEditLink = class(TStringEditLink, IVTEditLink)
  private
    FEditor: TValueLabelGridFrame;
    FInitialText: string;
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create; override;
    function BeginEdit: Boolean; override; stdcall;
    function EndEdit: Boolean; override; stdcall;
    property Editor: TValueLabelGridFrame read FEditor write FEditor;
  end;

{ TValidatedStringEditLing }

procedure TValidatedStringEditLink.KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FEditor.DoShowHintMsg(nil, '');

  if (FColumn = 1) and
     (Key = VK_N) and
     (Shift = [ssCtrlOS]) and
     // Doing an EndEdit, will trigger the VLGEdited method, but it only acts
     // if Column = 0, so we may safely send a postmessage here!
     (FTree.EndEditNode)
  then
    begin
      PostMessage(FEditor.Handle, LM_VLG_NEWVALUELABEL, 0, 0);
      Key := VK_UNKNOWN;
    end;

  if (Key = VK_C) and
     (Shift = [ssCtrlOS])
  then
    Edit.CopyToClipboard;
end;

constructor TValidatedStringEditLink.Create;
begin
  inherited Create;
  Edit.OnKeyDown  := @KeyDown;
end;

function TValidatedStringEditLink.BeginEdit: Boolean; stdcall;
begin
  Result := inherited BeginEdit;
  FInitialText := Edit.Text;
end;

function TValidatedStringEditLink.EndEdit: Boolean; stdcall;
var
  I: integer;
  F: Extended;
  S: TCaption;
begin
  Result := not FStopping;

  if (result) and (FColumn = 0) then
  begin
    // Type check
    case Editor.FValueLabelSet.LabelType of
      ftInteger:
        begin
          S := Edit.Text;
          result := TryStrToInt(S, I);
        end;
      ftFloat:
        begin
          S := StringsReplace(
                 Edit.Text,
                 [',','.'],
                 [DefaultFormatSettings.DecimalSeparator, DefaultFormatSettings.DecimalSeparator],
                 [rfReplaceAll]
          );
          result := TryStrToFloat(S, F);
        end;
      ftString:
        S := Edit.Text;
    end;

    if not Result then
      FEditor.DoShowHintMsg(Edit, '"' + S + '" is not a valid ' + LowerCase(EpiTypeNames[Editor.FValueLabelSet.LabelType]));

    // Check for existing value
    if Result and
       (S <> FInitialText) and
       (Editor.FValueLabelSet.ValueLabelExists[S])
    then
    begin
      FEditor.DoShowHintMsg(Edit, 'Value "' + S + '" already exists!');
      Result := false;
    end;
  end;

  if Result then
    FInitialText := '';

  Result := Result and inherited;
end;

{ TValueLabelGridFrame }

procedure TValueLabelGridFrame.NewLineBtnClick(Sender: TObject);
begin
  DoAddLine;
//  VLG.SetFocus;
end;

procedure TValueLabelGridFrame.DelLineBtnClick(Sender: TObject);
var
  NewNode: PVirtualNode;
  VL: TEpiCustomValueLabel;
begin
  if not Assigned(VLG.FocusedNode) then exit;

  NewNode := VLG.GetNextSibling(VLG.FocusedNode);
  if not Assigned(NewNode) then
    NewNode := VLG.GetPreviousSibling(VLG.FocusedNode);


  VL := ValueLabelFromNode(VLG.FocusedNode);
  with VL do
    if MessageDlg('Warning',
         Format('Are you sure you want to delete "%s = %s"?',[ValueAsString, TheLabel.Text]),
         mtWarning, mbYesNo, 0, mbNo) = mrNo then exit;

  VLG.DeleteNode(VLG.FocusedNode);
  VL.Free;

  if Assigned(NewNode) then
  begin
    VLG.FocusedNode := NewNode;
    VLG.Selected[NewNode] := true;
  end;
  VLG.Refresh;
  DoShowHintMsg(nil, '');
end;

procedure TValueLabelGridFrame.DoAddLine;
var
  Node: PVirtualNode;
  Last: PVirtualNode;
  V1, V2: Extended;
begin
  if not Assigned(ValueLabelSet) then exit;

  DoShowHintMsg(nil, '');

  VLG.BeginUpdate;

  try
    Last := VLG.GetLast();

    if Assigned(Last) and
       (VLG.Text[Last, 1] = '')
    then
      begin
        DoShowHintMsg(VLG.GetDisplayRect(Last, 1, true),
          'Please enter a label or delete entry before making a new!'
        );
        Exit;
      end;

    FValueLabelSet.NewValueLabel;
    Node := VLG.GetLast();

    if FValueLabelSet.LabelType in [ftFloat,ftInteger] then
    begin
      if Assigned(Last) then
      begin
        if FValueLabelSet.LabelType = ftInteger then
          VLG.Text[Node, 0] := FloatToStr(StrToInt(ValueLabelFromNode(Last).ValueAsString) + 1)
        else begin
          if Assigned(VLG.GetPrevious(Last)) then
          begin
            V2 := StrToFloat(ValueLabelFromNode(Last).ValueAsString);
            V1 := StrToFloat(ValueLabelFromNode(VLG.GetPrevious(Last)).ValueAsString);
            VLG.Text[Node, 0] := FloatToStr(V2 + (V2 - V1));
          end else begin
            V1 := StrToFloat(ValueLabelFromNode(Last).ValueAsString) * 2;
            if SameValue(V1, 0) then
              VLG.Text[Node, 0] := '1'
            else
              VLG.Text[Node, 0] := FloatToStr(V1);
          end;
        end;
      end
      else
        VLG.Text[Node, 0] := '1';
    end;

    VLG.FocusedNode := Node;
    VLG.FocusedColumn := 1;
    VLG.Selected[Node] := true;

    Application.QueueAsyncCall(@VLGSendPostEdit, PtrInt(Node));

  finally
    VLG.EndUpdate;
  end;
end;

procedure TValueLabelGridFrame.VLGChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  // TODO: This should not be nessesary, somethings is f*cked with the VLG.
  VLG.Invalidate;
end;

procedure TValueLabelGridFrame.VLGChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  if (ValueLabelSet.LabelScope = vlsExternal) or
     (not Authenticator.IsAuthorized([earDefineProject]))
  then
    begin
      Allowed := false;
      Exit;
    end
  else
    ValueLabelFromNode(Node).IsMissingValue := NewState in [csCheckedNormal, csCheckedPressed];
end;

procedure TValueLabelGridFrame.VLGEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if Column = 0 then Application.QueueAsyncCall(@VLGSendPostEdit, PtrInt(Node));
end;

procedure TValueLabelGridFrame.VLGEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Authenticator.IsAuthorized([earDefineProject]);
end;

procedure TValueLabelGridFrame.VLGEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  EL: TValidatedStringEditLink;
begin
  EL := TValidatedStringEditLink.Create;
  EL.Editor := Self;
  EditLink := EL;
end;

procedure TValueLabelGridFrame.VLGFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  // TODO: This should not be nessesary, somethings is f*cked with the VLG.
  VLG.Invalidate;
end;

procedure TValueLabelGridFrame.VLGFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  if (NewColumn = 2) or (NewColumn = -1) then Allowed := false;
end;

procedure TValueLabelGridFrame.VLGGetNodeText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  VL: TEpiCustomValueLabel;
begin
  VL := ValueLabelFromNode(Node);
  with VL do
  begin
    case Column of
      0: CellText := ValueAsString;
      1: CellText := TheLabel.Text;
      2: CellText := '';
    end;
  end;
end;

procedure TValueLabelGridFrame.VLGInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node^.CheckType := ctCheckBox;
  if ValueLabelFromNode(Node).IsMissingValue then
    Node^.CheckState := csCheckedNormal;
end;

procedure TValueLabelGridFrame.VLGKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  DoShowHintMsg(nil, '');

  // External valuelabels do not need shortcut keys
  if Assigned(FValueLabelSet) and
     (FValueLabelSet.LabelScope = vlsExternal)
  then
    Exit;

  case Key of
    VK_RETURN:
      begin
        if (Shift = []) then
        begin
          if not Assigned(VLG.FocusedNode) then
            VLG.FocusedNode := VLG.GetFirstSelected(false);

          VLG.EditNode(VLG.FocusedNode, VLG.FocusedColumn);
          Key := VK_UNKNOWN;
        end;
      end;
    VK_DELETE:
      begin
        DelLineBtn.Click;
        Key := VK_UNKNOWN;
      end;
    VK_N:
      begin
        if not (Shift = [ssCtrlOS]) then exit;

        DoAddLine;
        Key := VK_UNKNOWN;
      end;
    VK_C:
      begin
        if not (Shift = [ssCtrlOS]) then exit;

        Clipboard.AsText := VLG.Text[VLG.FocusedNode, VLG.FocusedColumn];
        Key := VK_UNKNOWN;
      end;
  end;
end;

procedure TValueLabelGridFrame.VLGSendPostEdit(Data: PtrInt);
begin
  {$IFDEF MSWINDOWS}
  //   ===  relevant for FieldValueEditorForm   ===
  // Note: When pressing CTRL+N with VLG in editing mode, the following happens:
  // 1: When the Edit is hiding (in EndEdit) the form recieves focus, and apparently
  //    in windows, this forwards the focus to the form edit.
  // 2: When the "PostEdit" message get here, we call the EditNode, which in turn calls
  // 3: KillFocus on the form edit, which calls
  // 4: OnEditingDone. This method in turns sets focus to the VLG here (for reasons, this OnEditingDone MUST
  //    call VLG.SetFocus) which changes focus from the TValidatedStringEdit to the VLG.
  //
  // Therefore on Windows focus should be removed from the form edit and set to the VLG, such
  // that when the VLG enters edit mode, the focus remains on the TValidatedStringEdit

  VLG.SetFocus;
  {$ENDIF}
  VLG.FocusedColumn := 1;
  VLG.FocusedNode   := PVirtualNode(Data);
  VLG.EditNode(PVirtualNode(Data), 1);
end;

procedure TValueLabelGridFrame.VLGSetNodeText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
var
  VL: TEpiCustomValueLabel;
  S: String;
begin
  VL := ValueLabelFromNode(Node);

  with VL do
  begin
    case Column of
      0: case FValueLabelSet.LabelType of
           ftInteger:     TEpiIntValueLabel(VL).Value := StrToInt(NewText);
           ftFloat:       begin
                            S := StringsReplace(
                                   NewText,
                                   [',','.'],
                                   [DefaultFormatSettings.DecimalSeparator, DefaultFormatSettings.DecimalSeparator],
                                   [rfReplaceAll]
                            );
                            TEpiFloatValueLabel(Vl).Value := StrToFloat(S);
                          end;
           ftString:      TEpiStringValueLabel(VL).Value := NewText;
           ftUpperString: TEpiStringValueLabel(VL).Value := UTF8UpperCase(NewText);
         end;
      1: TheLabel.Text := NewText;
      2: ; // do nothing
    end;
  end;
end;

procedure TValueLabelGridFrame.VLGUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if UTF8Key = Char(VK_SPACE) then exit;
  if UTF8Key = Char(VK_RETURN) then exit;

  if not Assigned(VLG.FocusedNode) then
    VLG.FocusedNode := VLG.GetFirstSelected(false);

  VLG.EditNode(VLG.FocusedNode, VLG.FocusedColumn);
end;

procedure TValueLabelGridFrame.InternalSetup;
var
  Allowed: Boolean;
begin
  if not Assigned(FValueLabelSet) then exit;

  if FValueLabelSet.LabelScope = vlsExternal then
    VLG.TreeOptions.MiscOptions := VLG.TreeOptions.MiscOptions - [toEditable]
  else
    VLG.TreeOptions.MiscOptions := VLG.TreeOptions.MiscOptions + [toEditable];

  Allowed := (FValueLabelSet.LabelScope = vlsInternal) and (Authenticator.IsAuthorized([earDefineProject]));

  NewLineBtn.Enabled := Allowed;
  DelLineBtn.Enabled := Allowed;
end;

procedure TValueLabelGridFrame.UpdateGrid;
begin
  VLG.Clear;
  if Assigned(FValueLabelSet) then
    VLG.RootNodeCount := FValueLabelSet.Count
  else
    VLG.RootNodeCount := 0;
  VLG.Invalidate;
end;

procedure TValueLabelGridFrame.EventHook(const Sender,
  Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if Initiator <> FValueLabelSet then exit;

  case EventGroup of
    eegCustomBase:
      begin
        case TEpiCustomChangeEventType(EventType) of
          ecceDestroy:
            begin
              FValueLabelSet := nil;
              UpdateGrid;
            end;
          ecceUpdate,
          ecceAddItem,
          ecceDelItem: UpdateGrid;
          ecceSetItem,
          ecceSetTop,
          ecceSetLeft,
          ecceText,
          ecceName:    Exit;
        end;
      end;
  end;
end;

procedure TValueLabelGridFrame.SetValueLabelSet(AValue: TEpiValueLabelSet);
begin
  if FValueLabelSet = AValue then Exit;

  if Assigned(FValueLabelSet) then
    FValueLabelSet.UnRegisterOnChangeHook(@EventHook);

  FValueLabelSet := AValue;

  if Assigned(FValueLabelSet) then
    FValueLabelSet.RegisterOnChangeHook(@EventHook, true);

  InternalSetup;
  UpdateGrid;
end;

function TValueLabelGridFrame.ValueLabelFromNode(Node: PVirtualNode
  ): TEpiCustomValueLabel;
begin
  Result := FValueLabelSet[Node^.Index];
end;

procedure TValueLabelGridFrame.NodeError(Node: PVirtualNode;
  Column: TColumnIndex; const Msg: string);
var
  R: TRect;
begin
  R := VLG.GetDisplayRect(Node, Column, true);
  OffsetRect(R, 0, R.Bottom - R.Top);
  DoShowHintMsg(R, Msg);
  VLG.Selected[Node] := true;
  VLG.EditNode(Node, Column);
end;

procedure TValueLabelGridFrame.DoShowHintMsg(Ctrl: TControl; const Msg: String);
begin
  if Assigned(OnShowHintMsg) then
    OnShowHintMsg(Self, Ctrl, Msg);
end;

procedure TValueLabelGridFrame.DoShowHintMsg(R: TRect; const Msg: string);
var
  Ctrl: TWinControl;
begin
  Ctrl := TWinControl.Create(nil);
  Ctrl.Top := R.Top;
  Ctrl.Left := R.Left;
  Ctrl.Width := R.Right - R.Left;
  Ctrl.Height := R.Bottom - R.Top;
  Ctrl.Visible := false;
  Ctrl.Parent := VLG;
  DoShowHintMsg(Ctrl, Msg);
  Ctrl.Free;
end;

procedure TValueLabelGridFrame.LM_NewValueLabel(var Msg: TLMessage);
begin
  DoAddLine;
end;

constructor TValueLabelGridFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FVLG := TVirtualStringTree.Create(Self);
  FVLG.BeginUpdate;

  with VLG do
  begin
    Align := alClient;
    Color := clWindow;
    NodeDataSize := SizeOF(PEpiValueLabel);
    WantTabs := true;
    TabStop := true;
    EditDelay := 0;

    // Events:
    OnInitNode      := @VLGInitNode;
    OnGetText       := @VLGGetNodeText;
    OnNewText       := @VLGSetNodeText;
    OnFocusChanging := @VLGFocusChanging;
    OnFocusChanged  := @VLGFocusChanged;
    OnKeyDown       := @VLGKeyDown;
    OnUTF8KeyPress  := @VLGUTF8KeyPress;
    OnChecking      := @VLGChecking;
    OnChecked       := @VLGChecked;
    OnCreateEditor  := @VLGEditor;
    OnEdited        := @VLGEdited;
    OnEditing := @VLGEditing;

    with TreeOptions do
    begin
      AnimationOptions := [];
      AutoOptions      := [];
      MiscOptions      := [toCheckSupport, toEditable, toGridExtensions, toWheelPanning, toEditOnDblClick];
      PaintOptions     := [toShowHorzGridLines, toShowVertGridLines, toThemeAware];
      SelectionOptions := [toExtendedFocus, toRightClickSelect, toCenterScrollIntoView];
      StringOptions    := [toAutoAcceptEditChange];
    end;

    with Header do
    begin
      Options := [hoAutoResize, hoColumnResize, hoDblClickResize, hoVisible];
      with Columns.Add do
      begin
        Text := 'Value';
        Options := [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coAllowFocus, coEditable];
        Width := 20;
      end;

      with Columns.Add do
      begin
        Text := 'Label';
        Options := [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coAllowFocus, coEditable];
        Width := 40;
      end;

      with Columns.Add do
      begin
        Alignment := taCenter;
        Text := 'Missing';
        Options := [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coVisible, coEditable];
        CheckBox := true;
        Width := 70;
      end;
      AutoSizeIndex := 1;
      Height := 25;
      MainColumn := 2;
    end;

    Parent := Self;
  end;

  FVLG.EndUpdate;
end;

destructor TValueLabelGridFrame.Destroy;
begin
  if Assigned(FValueLabelSet) then
    FValueLabelSet.UnRegisterOnChangeHook(@EventHook);

  inherited Destroy;
end;

function TValueLabelGridFrame.ValidateGridEntries: boolean;
var
  VL: TEpiCustomValueLabel;
  Node: PVirtualNode;
begin
  if not Assigned(FValueLabelSet) then exit(true);

  Result := false;
  Node := VLG.GetFirstChild(nil);
  while Assigned(node) do
  begin
    VL := ValueLabelFromNode(Node);

    if Trim(VL.ValueAsString) = '' then
    begin
      NodeError(Node, 0, Format('Value must not be empty (Row: %d)', [Node^.Index + 1]));
      Exit;
    end;

    if Trim(VL.TheLabel.Text) = '' then
    begin
      NodeError(Node, 1, Format('Label must no be empty (Value = %s)', [VL.ValueAsString]));
      Exit;
    end;

    Node := VLG.GetNextSibling(Node);
  end;

  result := true;
end;

end.

