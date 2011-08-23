unit manager_messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages;

const
  // Designer messages
  LM_DESIGNER_DEL      = LM_USER + 1;              // WParam = Control;  LParam: 1=Force Delete, 0=Ask User.
  LM_DESIGNER_DELALL   = LM_DESIGNER_DEL + 1;
  LM_DESIGNER_LAST     = LM_DESIGNER_DELALL;       // Last Designer message... always update when inserting new messages.!

  LM_MAIN_OPENPROJECT  = LM_DESIGNER_LAST    + 1;
  LM_MAIN_OPENRECENT   = LM_MAIN_OPENPROJECT + 1;
  LM_MAIN_NEWPROJECT   = LM_MAIN_OPENRECENT  + 1;
  LM_MAIN_CLOSEPROJECT = LM_MAIN_NEWPROJECT  + 1;
  // Last Main message... always update when inserting new messages.!
  LM_MAIN_LAST         = LM_MAIN_CLOSEPROJECT;

implementation

end.

