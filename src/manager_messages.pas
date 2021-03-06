unit manager_messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages;

const
  // Designer messages
  LM_DESIGNER_ADD      = LM_USER+1;                 // WParam = EpiCtrl;
  LM_DESIGNER_USER_AUTHED = LM_DESIGNER_ADD + 1;    // No input. Result = 0: user not authorized, Result > 0: user authorized
  // Last Designer message... always update when inserting new messages.!
  LM_DESIGNER_LAST     = LM_DESIGNER_USER_AUTHED;

  // Main messages
  LM_MAIN_OPENPROJECT  = LM_DESIGNER_LAST    + 1;   // WParam: TString object containing the filename. Has been checked for exisistence. (TString object MUST BE FREED at the message handler).
  LM_MAIN_OPENRECENT   = LM_MAIN_OPENPROJECT + 1;   // WParam: 0 = use LParam; >0 = Pointer to a TAction which initiated the call
                                                    // LParam: 0+ = Index into RecentFilesActionList
  LM_MAIN_NEWPROJECT   = LM_MAIN_OPENRECENT  + 1;
  LM_MAIN_CLOSEPROJECT = LM_MAIN_NEWPROJECT  + 1;   // WParam: 0 = default behaviour; 1 = force save and close (used when creating Admin project)
  LM_MAIN_IMPORTTONEW  = LM_MAIN_CLOSEPROJECT + 1;  // WParam: 0 = Open File Dialog; 1 = Read from clipboard
  // Last Main message... always update when inserting new messages.!
  LM_MAIN_LAST         = LM_MAIN_IMPORTTONEW;

  // Value Label Editor (2) messages
  LM_VLEDIT_STARTEDIT  = LM_MAIN_LAST + 1;          // WParam: PVirtualNode.
  LM_VLEDIT_FOCUSCHECK = LM_VLEDIT_STARTEDIT + 1;   // WParam: PVirtualNode.

  // Last ValueLabelEditor message... always update when inserting new messages.!
  LM_VLEDIT_LAST       = LM_VLEDIT_FOCUSCHECK;

  // Value Label Grid message
  LM_VLG_NEWVALUELABEL = LM_VLEDIT_LAST + 1;      // Send message to GridFrame to start adding new valuelabel.

implementation

end.

