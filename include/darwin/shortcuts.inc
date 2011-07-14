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
  M_DefaultPos           := KeyToShortCut(VK_UNKNOWN, []);
  M_CloseProject         := KeyToShortCut(VK_W, [ssMeta]);
  M_OpenProject          := KeyToShortCut(VK_O, [ssMeta]);
  M_StartEntryClient     := KeyToShortCut(VK_UNKNOWN, []);
  M_Pack                 := KeyToShortCut(VK_UNKNOWN, []);
  M_ExportStata          := KeyToShortCut(VK_UNKNOWN, []);
  M_QuestionListReport   := KeyToShortCut(VK_UNKNOWN, []);
  M_ValueLabelListReport := KeyToShortCut(VK_UNKNOWN, []);
  M_CombinedListReport   := KeyToShortCut(VK_UNKNOWN, []);
  M_ExtendedListReport   := KeyToShortCut(VK_UNKNOWN, []);
  M_ProjectOverviewReport:= KeyToShortCut(VK_UNKNOWN, []);

  // Project Frame
{  SaveProjectAction.ShortCut := ShortCut(VK_S, [ssMeta]);
  SaveProjectAsAction.ShortCut := ShortCut(VK_S, [ssShift, ssMeta]);}

  P_SaveProject                := KeyToShortCut(VK_S, [ssMeta]);
  P_SaveProjectAs              := KeyToShortCut(VK_S, [ssMeta, ssShift]);
  P_NewDataForm                := KeyToShortCut(VK_UNKNOWN, []);
  P_DelDataForm                := KeyToShortCut(VK_UNKNOWN, []);
  P_ProjectSettings            := KeyToShortCut(VK_P, [ssMeta, ssShift]);
  P_StartValueLabelEditor      := KeyToShortCut(VK_V, [ssMeta]);
  P_OpenProject                := KeyToShortCut(VK_UNKNOWN, []);  // this overlaps with M_OpenProject!?!?!

  // Designer Frame
  D_NewIntField                := KeyToShortCut(VK_1, []);
  D_NewIntField_Fast           := KeyToShortCut(VK_1, [ssMeta]);
  D_NewFloatField              := KeyToShortCut(VK_2, []);
  D_NewFloatField_Fast         := KeyToShortCut(VK_2, [ssMeta]);
  D_NewStringField             := KeyToShortCut(VK_3, []);
  D_NewStringField_Fast        := KeyToShortCut(VK_3, [ssMeta]);
  D_NewDateField               := KeyToShortCut(VK_4, []);
  D_NewDateField_Fast          := KeyToShortCut(VK_4, [ssMeta]);
  D_NewHeading                 := KeyToShortCut(VK_5, []);
  D_NewHeading_Fast            := KeyToShortCut(VK_5, [ssMeta]);
  D_NewSection                 := KeyToShortCut(VK_6, []);
  D_EditControl                := KeyToShortCut(VK_RETURN, []);
  D_DeleteControl              := KeyToShortCut(VK_BACK, []);
  D_DeleteControl_Fast         := KeyToShortCut(VK_DELETE, [ssMeta]);
  D_ImportData                 := KeyToShortCut(VK_UNKNOWN, []);
  D_AddStructure               := KeyToShortCut(VK_UNKNOWN, []);
  D_MoveTop                    := KeyToShortCut(VK_UP, [ssMeta]);
  D_MoveSideUp                 := KeyToShortCut(VK_UP, [ssAlt]);
  D_MoveControlUp              := KeyToShortCut(VK_UP, []);
  D_MoveControlDown            := KeyToShortCut(VK_DOWN, []);
  D_MoveSideDown               := KeyToShortCut(VK_DOWN, [ssAlt]);
  D_MoveBottom                 := KeyToShortCut(VK_DOWN, [ssMeta]);
  D_DeleteAllControl           := KeyToShortCut(VK_UNKNOWN, []);
  D_PasteAs                    := KeyToShortCut(VK_G, [ssMeta]);
  D_CopyControl                := KeyToShortCut(VK_C, [ssMeta]);
  D_PasteControl               := KeyToShortCut(VK_V, [ssMeta]);

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
end;

