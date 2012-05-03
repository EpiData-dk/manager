{%MainUnit ../../shortcuts.pas}

procedure InitShortCuts;
begin
  // Main Form
  M_NewProject           := KeyToShortCut(VK_N, [ssCtrl]);
  M_Settings             := KeyToShortCut(VK_S, [ssAlt]);
  M_Exit                 := KeyToShortCut(VK_F4, [ssAlt]);
  M_OpenRecent           := KeyToShortCut(VK_UNKNOWN, [ssShift, ssCtrl]);
  M_ShowAbout            := KeyToShortCut(VK_UNKNOWN, []);
  M_CopyProjectInfo      := KeyToShortCut(VK_UNKNOWN, []);
  M_CheckVersion         := KeyToShortCut(VK_UNKNOWN, []);
  M_DefaultPos           := KeyToShortCut(VK_UNKNOWN, []);
  M_CloseProject         := KeyToShortCut(VK_F4, [ssCtrl]);
  M_OpenProject          := KeyToShortCut(VK_O, [ssCtrl]);
  M_StartEntryClient     := KeyToShortCut(VK_UNKNOWN, []);
  M_Pack                 := KeyToShortCut(VK_UNKNOWN, []);
  M_Export               := KeyToShortCut(VK_UNKNOWN, []);
  M_QuestionListReport   := KeyToShortCut(VK_UNKNOWN, []);
  M_ValueLabelListReport := KeyToShortCut(VK_UNKNOWN, []);
  M_CombinedListReport   := KeyToShortCut(VK_UNKNOWN, []);
  M_ExtendedListReport   := KeyToShortCut(VK_UNKNOWN, []);
  M_ProjectOverviewReport:= KeyToShortCut(VK_UNKNOWN, []);

  // Project Frame
  P_SaveProject                := KeyToShortCut(VK_S, [ssCtrl]);
  P_SaveProjectAs              := KeyToShortCut(VK_S, [ssCtrl, ssShift]);
  P_NewDataForm                := KeyToShortCut(VK_UNKNOWN, []);
  P_DelDataForm                := KeyToShortCut(VK_UNKNOWN, []);
  P_ProjectSettings            := KeyToShortCut(VK_P, [ssAlt]);
  P_StartValueLabelEditor      := KeyToShortCut(VK_V, [ssAlt]);
  P_OpenProject                := KeyToShortCut(VK_UNKNOWN, []);  // this overlaps with M_OpenProject!?!?!
  P_KeyFields                  := KeyToShortCut(VK_UNKNOWN, []);

  // Designer Frame
  D_NewIntField                := KeyToShortCut(VK_1, []);
  D_NewIntField_Fast           := KeyToShortCut(VK_1, [ssShift]);
  D_NewFloatField              := KeyToShortCut(VK_2, []);
  D_NewFloatField_Fast         := KeyToShortCut(VK_2, [ssShift]);
  D_NewStringField             := KeyToShortCut(VK_3, []);
  D_NewStringField_Fast        := KeyToShortCut(VK_3, [ssShift]);
  D_NewDateField               := KeyToShortCut(VK_4, []);
  D_NewDateField_Fast          := KeyToShortCut(VK_4, [ssShift]);
  D_NewHeading                 := KeyToShortCut(VK_5, []);
  D_NewHeading_Fast            := KeyToShortCut(VK_5, [ssShift]);
  D_NewSection                 := KeyToShortCut(VK_6, []);
  D_EditControl                := KeyToShortCut(VK_RETURN, []);
  D_DeleteControl              := KeyToShortCut(VK_DELETE, []);
  D_DeleteControl_Fast         := KeyToShortCut(VK_DELETE, [ssShift]);
  D_ImportData                 := KeyToShortCut(VK_UNKNOWN, []);
  D_AddStructure               := KeyToShortCut(VK_UNKNOWN, []);
  D_MoveTop                    := KeyToShortCut(VK_HOME, []);
  D_MoveSideUp                 := KeyToShortCut(VK_PRIOR, []);
  D_MoveControlUp              := KeyToShortCut(VK_UP, []);
  D_MoveControlDown            := KeyToShortCut(VK_DOWN, []);
  D_MoveSideDown               := KeyToShortCut(VK_NEXT, []);
  D_MoveBottom                 := KeyToShortCut(VK_END, []);
  D_DeleteAllControl           := KeyToShortCut(VK_UNKNOWN, []);
  D_PasteAs                    := KeyToShortCut(VK_G, [ssCtrl]);
  D_CopyControl                := KeyToShortCut(VK_C, [ssCtrl]);
  D_PasteControl               := KeyToShortCut(VK_V, [ssCtrl]);
  D_ViewDataSet                := KeyToShortCut(VK_UNKNOWN, []);

  // ValueLabel Editor
  // - tree actions
  V_TREE_DeleteValueLabelSet      := KeyToShortCut(VK_DELETE, []);
  V_TREE_DeleteValueLabelSet_Fast := KeyToShortCut(VK_DELETE, [ssShift]);
  V_TREE_NewIntValueLabelSet      := KeyToShortCut(VK_1, [ssCtrl]);
  V_TREE_NewFloatValueLabelSet    := KeyToShortCut(VK_2, [ssCtrl]);
  V_TREE_NewStringValueLabelSet   := KeyToShortCut(VK_3, [ssCtrl]);
  // - grid
  V_GRID_DeleteRow                := KeyToShortCut(VK_DELETE, []);
  V_GRID_DeleteRow_Fast           := KeyToShortCut(VK_DELETE, [ssShift]);
  V_GRID_InsertRow                := KeyToShortCut(VK_INSERT, []);
end;

