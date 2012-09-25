procedure InitFont(Out Font: TFont);
begin
  Font := TFont.Create;
  Font.Name := 'Sans';
  Font.Size := 10;
  Font.Style := [];
end;
