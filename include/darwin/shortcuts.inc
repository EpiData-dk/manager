{%MainUnit ../../shortcuts.pas}

procedure InitShortCuts;
begin
  // Main Form
  M_NewProject           := KeyToShortCut(VK_N, [ssMeta]);
  M_Settings             := KeyToShortCut(VK_OEM_COMMA, [ssMeta]);
  M_Exit                 := KeyToShortCut(VK_UNKNOWN, []);
  M_OpenRecent           := KeyToShortCut(VK_UNKNOWN, [ssShift, ssMeta]);
  M_ShowAbout            := KeyToShortCut(VK_UNKNOWN, []);
  M_CopyProjectInfo      := KeyToShortCut(VK_UNKNOWN, []);
  M_CheckVersion         := KeyToShortCut(VK_UNKNOWN, []);
  M_DefaultPos           := KeyToShortCut(VK_0, [ssMeta, ssShift]);
  M_CloseProject         := KeyToShortCut(VK_W, [ssMeta]);
  M_OpenProject          := KeyToShortCut(VK_O, [ssMeta]);
  M_StartEntryClient     := KeyToShortCut(VK_UNKNOWN, []);
  M_Pack                 := KeyToShortCut(VK_UNKNOWN, []);
  M_Export               := KeyToShortCut(VK_UNKNOWN, []);
  M_QuestionListReport   := KeyToShortCut(VK_UNKNOWN, []);
  M_ValueLabelListReport := KeyToShortCut(VK_UNKNOWN, []);
  M_ExtendedListReport   := KeyToShortCut(VK_UNKNOWN, []);
  M_ProjectOverviewReport:= KeyToShortCut(VK_UNKNOWN, []);

  // Project Frame
  P_SaveProject                := KeyToShortCut(VK_S, [ssMeta]);
  P_SaveProjectAs              := KeyToShortCut(VK_S, [ssMeta, ssShift]);
  P_NewDataForm                := KeyToShortCut(VK_UNKNOWN, []);
  P_DelDataForm                := KeyToShortCut(VK_UNKNOWN, []);
  P_ProjectSettings            := KeyToShortCut(VK_P, [ssMeta, ssShift]);
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
  D_NewTimeField               := KeyToShortCut(VK_5, []);
  D_NewTimeField_Fast          := KeyToShortCut(VK_5, [ssShift]);
  D_NewHeading                 := KeyToShortCut(VK_6, []);
  D_NewHeading_Fast            := KeyToShortCut(VK_6, [ssShift]);
  D_NewSection                 := KeyToShortCut(VK_UNKNOWN, []);
  D_SelectAll                  := KeyToShortCut(VK_A, [ssMeta]);
  D_ExpandDataframe            := KeyToShortCut(VK_RETURN, [ssMeta]);

  // - align
  D_AlignForm                  := KeyToShortCut(VK_A, [ssCtrl, ssMeta]);
  D_AlignLeft                  := KeyToShortCut(VK_LEFT, [ssCtrl, ssMeta]);
  D_AlignRight                 := KeyToShortCut(VK_RIGHT, [ssCtrl, ssMeta]);
  D_AlignTop                   := KeyToShortCut(VK_UP, [ssCtrl, ssMeta]);
  D_AlignBottom                := KeyToShortCut(VK_DOWN, [ssCtrl, ssMeta]);

  D_EditControl                := KeyToShortCut(VK_RETURN, []);
  D_DeleteControl              := KeyToShortCut(VK_BACK, []);
  D_DeleteControl_Fast         := KeyToShortCut(VK_BACK, [ssMeta]);
  D_ImportData                 := KeyToShortCut(VK_I, [ssMeta]);
  D_ImportDataCB               := KeyToShortCut(VK_I, [ssMeta, ssShift]);
  D_MoveTop                    := KeyToShortCut(VK_UP, [ssMeta]);
  D_MoveSideUp                 := KeyToShortCut(VK_UP, [ssAlt]);
  D_MoveControlUp              := KeyToShortCut(VK_UP, []);
  D_MoveControlDown            := KeyToShortCut(VK_DOWN, []);
  D_MoveSideDown               := KeyToShortCut(VK_DOWN, [ssAlt]);
  D_MoveBottom                 := KeyToShortCut(VK_DOWN, [ssMeta]);
  D_DeleteAllControl           := KeyToShortCut(VK_UNKNOWN, []);
  D_PasteAs                    := KeyToShortCut(VK_G, [ssMeta]);
  D_CutControl                 := KeyToShortCut(VK_X, [ssMeta]);
  D_CopyControl                := KeyToShortCut(VK_C, [ssMeta]);
  D_PasteControl               := KeyToShortCut(VK_V, [ssMeta]);
  D_ViewDataSet                := KeyToShortCut(VK_UNKNOWN, []);
  D_Undo                       := KeyToShortCut(VK_Z, [ssMeta]);
  D_Redo                       := KeyToShortCut(VK_Z, [ssMeta, ssShift]);
  D_PrintDataForm              := KeyToShortCut(VK_P, [ssMeta]);
  D_BrowseData                 := KeyToShortCut(VK_D, [ssMeta]);

  // ValueLabel Editor
  // - tree actions
  V_TREE_DeleteValueLabelSet      := KeyToShortCut(VK_BACK, []);
  V_TREE_DeleteValueLabelSet_Fast := KeyToShortCut(VK_BACK, [ssMeta]);
  V_TREE_NewIntValueLabelSet      := KeyToShortCut(VK_1, [ssMeta]);
  V_TREE_NewFloatValueLabelSet    := KeyToShortCut(VK_2, [ssMeta]);
  V_TREE_NewStringValueLabelSet   := KeyToShortCut(VK_3, [ssMeta]);
  // - grid
  V_GRID_DeleteRow                := KeyToShortCut(VK_D, [ssMeta]);
  V_GRID_DeleteRow_Fast           := KeyToShortCut(VK_D, [ssMeta, ssShift]);
  V_GRID_InsertRow                := KeyToShortCut(VK_INSERT, []);

  A_NewGroup                      := KeyToShortCut(VK_G, [ssMeta]);
  A_NewUser                       := KeyToShortCut(VK_U, [ssMeta]);
  A_DeleteGroup                   := KeyToShortCut(VK_UNKNOWN, []);
  A_DeleteUser                    := KeyToShortCut(VK_UNKNOWN, []);
  A_AddUserToGroup                := KeyToShortCut(VK_UNKNOWN, []);
  A_RemoveUserFromGroup           := KeyToShortCut(VK_UNKNOWN, []);
end;

