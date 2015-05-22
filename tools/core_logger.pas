unit core_logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  epicustombase;

type

  { TCoreLogger }

  TCoreLogger = class(TForm)
    Memo1: TMemo;
  private
    { private declarations }
  public
    procedure DocumentHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  end;

implementation

{$R *.lfm}

{ TCoreLogger }

procedure TCoreLogger.DocumentHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  S: String;
begin
  if not Showing then exit;

  WriteStr(S, Initiator.ClassName, ' ', EventGroup, ' ', EventType, ' ', hexStr(Data));
  Memo1.Append(S);
end;

end.

