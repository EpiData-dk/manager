unit manager_messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages;

const
  // Designer messages
  LM_DESIGNER_DEL = LM_USER + 1;
  // Last Designer message... always update when inserting new messages.!
  LM_DESIGNER_LAST = LM_DESIGNER_DEL;

  LM_MAIN_OPENPROJECT  = LM_DESIGNER_LAST    + 1;
  LM_MAIN_OPENRECENT   = LM_MAIN_OPENPROJECT + 1;
  LM_MAIN_NEWPROJECT   = LM_MAIN_OPENRECENT  + 1;
  LM_MAIN_CLOSEPROJECT = LM_MAIN_NEWPROJECT  + 1;
  // Last Main message... always update when inserting new messages.!
  LM_MAIN_LAST         = LM_MAIN_CLOSEPROJECT;

implementation

end.
