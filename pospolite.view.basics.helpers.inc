{ TPLStringHelper }

function TPLStringHelper.SubStr(AFrom, ACount: SizeInt): TPLString;
begin
  Result := UTF8Copy(Self, AFrom, ACount);
end;

function TPLStringHelper.SubStr(AFrom: SizeInt): TPLString;
begin
  Result := UTF8Copy(Self, AFrom, Length);
end;

function TPLStringHelper.Length: SizeInt;
begin
  Result := UTF8Length(Self);
end;

function TPLStringHelper.Find(AWhat: TPLString; AFrom: SizeInt): SizeInt;
begin
  Result := UTF8Pos(AWhat, Self, AFrom);
end;

function TPLStringHelper.Exists(AWhat: TPLString; ASearchFrom: SizeInt
  ): TPLBool;
begin
  Result := Find(AWhat, ASearchFrom) > 0;
end;

function TPLStringHelper.FromC: TPLString;
begin
  Result := Utf8EscapeControlChars(Utf8EscapeControlChars(Self, emC), emHexC);
end;

function TPLStringHelper.IsWhiteSpaceAt(APos: SizeInt): TPLBool;
begin
  if (APos < 0) or (APos > Length) then
    Result := false
  else
    Result := Self[APos] in [' ', #9, #10, #13, #12];
end;

function TPLStringHelper.At(AIndex: SizeInt): TPLString;
begin
  Result := SubStr(AIndex, 1);
end;

function TPLStringHelper.Codepoint(AIndex: SizeInt): Cardinal;
var
  dummy: integer;
begin
  Result := UTF8CodepointToUnicode(PChar(At(AIndex)), dummy);
end;

function TPLStringHelper.AsCSSCode(AIndex: SizeInt): TPLString;
begin
  Result := '\' + IntToStr(Codepoint(AIndex)).AsHex;
end;

function TPLStringHelper.AsHTMLCode(AIndex: SizeInt): TPLString;
begin
  Result := '&#x' + IntToStr(Codepoint(AIndex)).AsHex + ';';
end;

// https://en.wikipedia.org/wiki/Emoji
function TPLStringHelper.IsEmoji(AIndex: SizeInt): TPLBool;
var
  cp: TPLInt;
begin
  cp := Codepoint(AIndex);
  Result := InRanges(cp, [
    Range($2700, $27BF), // Dingbats
    Range($1F600, $1F64F), // Emoticons
    Range($2600, $26FF), // Miscellaneous Symbols
    Range($1F300, $1F5FF), // Miscellaneous Symbols and Pictographs
    Range($1F900, $1F9FF), // Supplemental Symbols and Pictographs
    Range($1FA70, $1FAFF), // Symbols and Pictographs Extended-A
    Range($1F680, $1F6FF) // Transport and Map Symbols
  ]);
end;

function TPLStringHelper.AsHex: TPLString;
var
  a: SizeInt = 0;
begin
  TryStrToInt64(Self, a);
  Result := '%X'.Format([a]);
end;

function TPLStringHelper.FromHex: integer;
begin
  Result := 0;
  TryStrToInt('$' + Self, Result);
end;

{ TPLCharHelper }

function TPLCharHelper.IsWhiteSpace: TPLBool;
begin
  Result := TPLString(Self).IsWhiteSpaceAt();
end;

function TPLCharHelper.FromHex: integer;
begin
  Result := 0;
  TryStrToInt('$' + Self, Result);
end;

function TPLCharHelper.FromC: TPLString;
begin
  Result := TPLString('\' + Self).FromC;
end;
