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
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, math, Pospolite.View.Basics, Pospolite.View.Drawing.Basics,
  dialogs;

type

  // https://www.w3.org/TR/SVG/coords.html#NestedTransformations
  TPLCSSMatrixData = array[0..5] of TPLFloat;

  { TPLCSSMatrix }

  TPLCSSMatrix = packed record
  private
    FData: TPLCSSMatrixData;
  public
    constructor Create(const AData: TPLCSSMatrixData);
    constructor Create(const AData: array of TPLFloat);

    class function Identity: TPLCSSMatrix; static; inline;

    function Flip(const AVertical: TPLBool = true): TPLCSSMatrix; inline;
    function Scale(const AX, AY: TPLFloat): TPLCSSMatrix;
    function Translate(const AX, AY: TPLFloat): TPLCSSMatrix; inline;
    function Skew(const AX, AY: TPLFloat): TPLCSSMatrix; inline;
    function Rotate(const AAngle: TPLFloat): TPLCSSMatrix;
    procedure Transform(var APoint: TPLPointF); overload;
    procedure Transform(var APoints: TPLPointFArray); overload;

    class operator =(m1, m2: TPLCSSMatrix) r: TPLBool; inline;
    class operator *(m1, m2: TPLCSSMatrix) r: TPLCSSMatrix;
    class operator *(m: TPLCSSMatrix; x: TPLFloat) r: TPLCSSMatrix;
    class operator *(x: TPLFloat; m: TPLCSSMatrix) r: TPLCSSMatrix; inline;
  end;

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
  i: SizeInt = 1;
  pom: TPLString = '';
  qt: TPLBool = false;
begin
  if Length(ASource) = 0 then exit;

  while i <= Length(ASource) do begin
    if ASource[i] in ['"', ''''] then qt := not qt else
    if ASource[i] in [#10, #11, #13] then qt := false;

    if not qt and (Copy(ASource, i, 2) = '/*') then begin
      i += 2;
      while (i <= Length(ASource)) and (Copy(ASource, i, 2) <> '*/') do i += 1;
      if Copy(ASource, i, 2) = '*/' then i += 2;
    end;

    if (i <= Length(ASource)) and not (ASource[i] in [#0..#13]) then pom += ASource[i];

    i += 1;
  end;

  ASource := pom;
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

{ TPLCSSMatrix }

constructor TPLCSSMatrix.Create(const AData: TPLCSSMatrixData);
var
  i: SizeInt;
begin
  for i := 0 to 5 do
    FData[i] := AData[i];
end;

constructor TPLCSSMatrix.Create(const AData: array of TPLFloat);
var
  i: SizeInt;
begin
  if Length(AData) <> 6 then begin
    for i := 0 to 5 do FData[i] := 0;
    exit;
  end;

  for i := 0 to 5 do
    FData[i] := AData[i];
end;

class function TPLCSSMatrix.Identity: TPLCSSMatrix;
begin
  Result := TPLCSSMatrix.Create([1, 0, 0, 1, 0, 0]);
end;

function TPLCSSMatrix.Flip(const AVertical: TPLBool): TPLCSSMatrix;
begin
  Result := Self * TPLCSSMatrix.Create([IfThen(AVertical, 1, -1), 0, 0, IfThen(AVertical, -1, 1), 0, 0]);
end;

function TPLCSSMatrix.Scale(const AX, AY: TPLFloat): TPLCSSMatrix;
var
  i: SizeInt;
begin
  Result := Self;

  for i := 0 to 5 do
    Result.FData[i] *= IfThen(i mod 2 = 0, AX, AY);
end;

function TPLCSSMatrix.Translate(const AX, AY: TPLFloat): TPLCSSMatrix;
begin
  Result := Self * TPLCSSMatrix.Create([1, 0, 0, 1, AX, AY]);
end;

function TPLCSSMatrix.Skew(const AX, AY: TPLFloat): TPLCSSMatrix;
begin
  Result := Self * TPLCSSMatrix.Create([1, Tan(RadToDeg(AX)), Tan(RadToDeg(AY)), 1, 0, 0]);
end;

function TPLCSSMatrix.Rotate(const AAngle: TPLFloat): TPLCSSMatrix;
var
  c, s: TPLFloat;
begin
  SinCos(RadToDeg(AAngle), s, c);
  Result := Self * TPLCSSMatrix.Create([c, s, -s, c, 0, 0]);
end;

procedure TPLCSSMatrix.Transform(var APoint: TPLPointF);
var
  x: TPLFloat;
begin
  x := APoint.X;

  APoint.X := x * FData[0] + APoint.Y * FData[2] + FData[4];
  APoint.Y := x * FData[1] + APoint.Y * FData[3] + FData[5];
end;

procedure TPLCSSMatrix.Transform(var APoints: TPLPointFArray);
var
  i: SizeInt;
begin
  for i := Low(APoints) to High(APoints) do
    Transform(APoints[i]);
end;

class operator TPLCSSMatrix.=(m1, m2: TPLCSSMatrix)r: TPLBool;
begin
  r := (m1.FData[0] = m2.FData[0]) and (m1.FData[1] = m2.FData[1]) and
    (m1.FData[2] = m2.FData[2]) and (m1.FData[3] = m2.FData[3]) and
    (m1.FData[4] = m2.FData[4]) and (m1.FData[5] = m2.FData[5]);
end;

class operator TPLCSSMatrix.*(m1, m2: TPLCSSMatrix)r: TPLCSSMatrix;
var
  p0, p2, p4: TPLFloat;
begin
  p0 := m1.FData[0] * m2.FData[0] + m1.FData[1] * m2.FData[2];
  p2 := m1.FData[2] * m2.FData[0] + m1.FData[3] * m2.FData[2];
  p4 := m1.FData[4] * m2.FData[0] + m1.FData[5] * m2.FData[2] + m2.FData[4];

  m1.FData[1] := m1.FData[0] * m2.FData[1] + m1.FData[1] * m2.FData[3];
  m1.FData[3] := m1.FData[2] * m2.FData[1] + m1.FData[3] * m2.FData[3];
  m1.FData[5] := m1.FData[4] * m2.FData[1] + m1.FData[5] * m2.FData[3] + m2.FData[5];

  m1.FData[0] := p0;
  m1.FData[2] := p2;
  m1.FData[4] := p4;

  r := m1;
end;

class operator TPLCSSMatrix.*(m: TPLCSSMatrix; x: TPLFloat)r: TPLCSSMatrix;
var
  i: SizeInt;
begin
  for i := 0 to 5 do
    m.FData[i] *= x;

  r := m;
end;

class operator TPLCSSMatrix.*(x: TPLFloat; m: TPLCSSMatrix)r: TPLCSSMatrix;
begin
  r := m * x;
end;

end.

