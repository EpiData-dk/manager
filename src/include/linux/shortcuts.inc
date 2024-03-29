{%MainUnit ../../shortcuts.pas}

procedure InitShortCuts;
begin
  // Main Form
  M_NewProject                 := KeyToShortCut(VK_N,       [ssCtrl]);
  M_Settings                   := KeyToShortCut(VK_S,       [ssAlt]);
  M_Exit                       := KeyToShortCut(VK_Q,       [ssCtrl]);
  M_OpenRecent                 := KeyToShortCut(VK_UNKNOWN, [ssShift, ssCtrl]);
  M_ShowAbout                  := KeyToShortCut(VK_UNKNOWN, []);
  M_CopyProjectInfo            := KeyToShortCut(VK_UNKNOWN, []);
  M_CheckVersion               := KeyToShortCut(VK_UNKNOWN, []);
  M_DefaultPos                 := KeyToShortCut(VK_0,       [ssCtrl, ssShift]);
  M_CloseProject               := KeyToShortCut(VK_W,       [ssCtrl]);
  M_OpenProject                := KeyToShortCut(VK_O,       [ssCtrl]);
  M_StartEntryClient           := KeyToShortCut(VK_UNKNOWN, []);
  M_Pack                       := KeyToShortCut(VK_UNKNOWN, []);
  M_Export                     := KeyToShortCut(VK_UNKNOWN, []);
  M_QuestionListReport         := KeyToShortCut(VK_UNKNOWN, []);
  M_ValueLabelListReport       := KeyToShortCut(VK_UNKNOWN, []);
  M_ExtendedListReport         := KeyToShortCut(VK_UNKNOWN, []);
  M_ProjectOverviewReport      := KeyToShortCut(VK_UNKNOWN, []);

  // Project Frame
  P_SaveProject                := KeyToShortCut(VK_S,       [ssCtrl]);
  P_SaveProjectAs              := KeyToShortCut(VK_S,       [ssCtrl, ssShift]);
  P_NewDataForm                := KeyToShortCut(VK_UNKNOWN, []);
  P_DelDataForm                := KeyToShortCut(VK_UNKNOWN, []);
  P_ProjectSettings            := KeyToShortCut(VK_P,       [ssAlt]);
  P_StartValueLabelEditor      := KeyToShortCut(VK_V,       [ssAlt]);
  P_OpenProject                := KeyToShortCut(VK_UNKNOWN, []);  // this overlaps with M_OpenProject!?!?!
  P_KeyFields                  := KeyToShortCut(VK_UNKNOWN, []);
  P_ViewLog                    := KeyToShortCut(VK_L,       [ssCtrl]);
  P_Users                      := KeyToShortCut(VK_U,       [ssCtrl]);
  P_Groups                     := KeyToShortCut(VK_G,       [ssCtrl]);
  P_EntryRight                 := KeyToShortCut(VK_E,       [ssCtrl]);

  // Designer Frame
  D_NewIntField                := KeyToShortCut(VK_1,       []);
  D_NewIntField_Fast           := KeyToShortCut(VK_1,       [ssShift]);
  D_NewFloatField              := KeyToShortCut(VK_2,       []);
  D_NewFloatField_Fast         := KeyToShortCut(VK_2,       [ssShift]);
  D_NewStringField             := KeyToShortCut(VK_3,       []);
  D_NewStringField_Fast        := KeyToShortCut(VK_3,       [ssShift]);
  D_NewDateField               := KeyToShortCut(VK_4,       []);
  D_NewDateField_Fast          := KeyToShortCut(VK_4,       [ssShift]);
  D_NewTimeField               := KeyToShortCut(VK_5,       []);
  D_NewTimeField_Fast          := KeyToShortCut(VK_5,       [ssShift]);
  D_NewHeading                 := KeyToShortCut(VK_6,       []);
  D_NewHeading_Fast            := KeyToShortCut(VK_6,       [ssShift]);
  D_NewSection                 := KeyToShortCut(VK_UNKNOWN, []);
  D_EditControl                := KeyToShortCut(VK_RETURN,  []);
  D_SelectAll                  := KeyToShortCut(VK_A,       [ssCtrl]);
  D_ExpandDataframe            := KeyToShortCut(VK_RETURN,  [ssCtrl]);

  // - align
  D_AlignForm                  := KeyToShortCut(VK_A, [ssCtrl, ssShift]);
  D_AlignLeft                  := KeyToShortCut(VK_LEFT, [ssCtrl, ssShift]);
  D_AlignRight                 := KeyToShortCut(VK_RIGHT, [ssCtrl, ssShift]);
  D_AlignTop                   := KeyToShortCut(VK_UP, [ssCtrl, ssShift]);
  D_AlignBottom                := KeyToShortCut(VK_DOWN, [ssCtrl, ssShift]);

  D_DeleteControl              := KeyToShortCut(VK_DELETE, []);
  D_DeleteControl_Fast         := KeyToShortCut(VK_DELETE, [ssShift]);
  D_ImportData                 := KeyToShortCut(VK_I, [ssCtrl]);
  D_ImportDataCB               := KeyToShortCut(VK_I, [ssCtrl, ssShift]);
  D_MoveTop                    := KeyToShortCut(VK_HOME, []);
  D_MoveSideUp                 := KeyToShortCut(VK_PRIOR, []);
  D_MoveControlUp              := KeyToShortCut(VK_UP, []);
  D_MoveControlDown            := KeyToShortCut(VK_DOWN, []);
  D_MoveSideDown               := KeyToShortCut(VK_NEXT, []);
  D_MoveBottom                 := KeyToShortCut(VK_END, []);
  D_DeleteAllControl           := KeyToShortCut(VK_UNKNOWN, []);
  D_PasteAs                    := KeyToShortCut(VK_G, [ssCtrl]);
  D_CutControl                 := KeyToShortCut(VK_X, [ssCtrl]);
  D_CopyControl                := KeyToShortCut(VK_C, [ssCtrl]);
  D_PasteControl               := KeyToShortCut(VK_V, [ssCtrl]);
  D_ViewDataSet                := KeyToShortCut(VK_UNKNOWN, []);
  D_Undo                       := KeyToShortCut(VK_Z, [ssCtrl]);
  D_Redo                       := KeyToShortCut(VK_Z, [ssCtrl, ssShift]);
  D_PrintDataForm              := KeyToShortCut(VK_P, [ssCtrl]);
  D_BrowseData                 := KeyToShortCut(VK_D, [ssCtrl]);

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

  // Admin
  A_NewGroup                      := KeyToShortCut(VK_G, [ssCtrl]);
  A_NewUser                       := KeyToShortCut(VK_U, [ssCtrl]);
  A_DeleteGroup                   := KeyToShortCut(VK_UNKNOWN, []);
  A_DeleteUser                    := KeyToShortCut(VK_UNKNOWN, []);
  A_AddUserToGroup                := KeyToShortCut(VK_UNKNOWN, []);
  A_RemoveUserFromGroup           := KeyToShortCut(VK_UNKNOWN, []);

  // Admin Groups:
  AG_NewGroup                      := KeyToShortCut(VK_N, [ssCtrl]);
  AG_DeleteGroup                   := KeyToShortCut(VK_DELETE, []);
  AG_EditGroup                     := KeyToShortCut(VK_RETURN, []);

  // Admin Users:
  AU_NewUser                       := KeyToShortCut(VK_N, [ssCtrl]);
  AU_DeleteUser                    := KeyToShortCut(VK_DELETE, []);
  AU_EditUser                      := KeyToShortCut(VK_RETURN, []);

end;

