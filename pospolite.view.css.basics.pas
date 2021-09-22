unit Pospolite.View.CSS.Basics;

{
  +-------------------------+
  | Package: Pospolite View |
  | Author: Matek0611       |
  | Email: matiowo@wp.pl    |
  | Version: 1.0p           |
  +-------------------------+

  Comments:
  ...
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics;

const
  // https://developer.mozilla.org/en-US/docs/Glossary/Vendor_Prefix
  PLCSSCommonPrefixes: array[0..4] of TPLString = (
    '-webkit-', // Chrome, Safari, newer versions of Opera, almost all iOS browsers including Firefox for iOS; basically, any WebKit based browser
    '-moz-', // Firefox
    '-ms-', // Internet Explorer and Microsoft Edge
    '-o-', // old pre-WebKit versions of Opera
    '-pl-' // Pospolite View (for experimental features)
  );

procedure RemoveCSSComments(var ASource: TPLString);
function WithoutCommonPrefix(const AName: TPLString): TPLString;

implementation

procedure RemoveCSSComments(var ASource: TPLString);
var
  i, j: SizeInt;
  found: TPLBool;
begin
  i := ASource.Find('/*');
  if i = 0 then exit;

  j := i;
  i += 2;
  found := true;
  while i <= ASource.Length do begin
    if found and (ASource[i] = '*') and (i + 1 <= ASource.Length) and (ASource[i+1] = '/') then begin
      found := false;
      ASource := ASource.SubStr(1, j - 1) + ASource.SubStr(i + 2);
      i := j - 1;
      continue;
    end;

    if not found and (ASource[i] = '/') and (i + 1 <= ASource.Length) and (ASource[i+1] = '*') then begin
      found := true;
      j := i;
      i += 2;
      continue;
    end;

    Inc(i);
  end;
end;

function WithoutCommonPrefix(const AName: TPLString): TPLString;
var
  s: TPLString;
begin
  Result := AName;

  for s in PLCSSCommonPrefixes do
    if Result.StartsWith(s, true) then begin
      Result := Result.SubStr(s.Length + 1);
      break;
    end;
end;

end.

