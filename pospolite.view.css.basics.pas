unit Pospolite.View.CSS.Basics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics;

procedure RemoveCSSComments(var ASource: TPLString);

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

end.

