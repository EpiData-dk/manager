unit managerprocs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure ReadClipBoard(ClipBoardLine: TStrings);


implementation

uses
  Clipbrd;

procedure ReadClipBoard(ClipBoardLine: TStrings);
var
  TmpStr: String;
begin
  if Clipboard.HasFormat(CF_Text) then
  begin
    TmpStr := Clipboard.AsText;
    TmpStr := StringReplace(TmpStr, LineEnding, #1, [rfReplaceAll]);
    ClipBoardLine.Delimiter := #1;
    ClipBoardLine.StrictDelimiter := true;
    ClipBoardLine.DelimitedText := TmpStr;
  end;
end;

end.

