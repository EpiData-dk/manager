unit shortcuts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

var
  // Main Form
  M_NewProject,
  M_Settings,
  M_Exit,
  M_OpenRecent,
  M_ShowAbout,
  M_CopyProjectInfo,
  M_CheckVersion,
  M_DefaultPos,
  M_CloseProject,
  M_OpenProject,
  M_StartEntryClient,
  M_Pack,
  M_Export,
  M_QuestionListReport,
  M_ValueLabelListReport,
  M_CombinedListReport,
  M_ExtendedListReport,
  M_ProjectOverviewReport:
    TShortCut;

  // Project Frame
  P_SaveProject,
  P_SaveProjectAs,
  P_NewDataForm,
  P_DelDataForm,
  P_ProjectSettings,
  P_StartValueLabelEditor,
  P_OpenProject,
  P_KeyFields:
    TShortCut;

  // Designer Frame
  D_NewIntField, D_NewIntField_Fast,
  D_NewFloatField, D_NewFloatField_Fast,
  D_NewStringField, D_NewStringField_Fast,
  D_NewDateField, D_NewDateField_Fast,
  D_NewTimeField, D_NewTimeField_Fast,
  D_NewHeading, D_NewHeading_Fast,
  D_NewSection,
  D_EditControl,
  D_DeleteControl, D_DeleteControl_Fast,
  D_ImportData,
  D_MoveTop,
  D_MoveSideUp,
  D_MoveControlUp,
  D_MoveControlDown,
  D_MoveSideDown,
  D_MoveBottom,
  D_SelectAll,
  D_DeleteAllControl,
  D_PasteAs,
  D_CutControl,
  D_CopyControl,
  D_PasteControl,
  D_ViewDataSet,
  D_Undo,
  D_Redo:
    TShortCut;


  // ValueLabel Editor
  // - tree actions
  V_TREE_DeleteValueLabelSet, V_TREE_DeleteValueLabelSet_Fast,
  V_TREE_NewIntValueLabelSet,
  V_TREE_NewFloatValueLabelSet,
  V_TREE_NewStringValueLabelSet,
  // - grid
  V_GRID_DeleteRow, V_GRID_DeleteRow_Fast,
  V_GRID_InsertRow:
    TShortCut;

implementation

uses
  LCLType;

{$I shortcuts.inc}

initialization
  InitShortCuts;

end.

