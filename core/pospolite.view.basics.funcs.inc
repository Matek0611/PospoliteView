function Range(const AMin, AMax: TPLInt): TPLIntRange;
begin
  Result := TPLIntRange.Create(AMin, AMax);
end;

function InRanges(const AValue: TPLInt;
  const ARanges: array of TPLIntRange): TPLBool;
begin
  Result := TPLIntRange.InRanges(AValue, ARanges);
end;

function fmod(a, b: TPLFloat): TPLFloat;
begin
  Result := abs(a);
  b := abs(b);

  while Result >= b do Result := Result - b;

  if a < 0 then exit(-Result);
end;

function AngleDeg(AAngle: TPLFloat; const AUnit: TPLString): TPLFloat;
begin
  Result := 0;

  case AUnit.ToLower of
    'deg': begin
      if (AAngle >= 0) and (AAngle <= 360) then Result := AAngle else
      if AAngle < 0 then Result := ceil64(-AAngle / 360) * 360 + AAngle else
      Result := ceil64(AAngle / 360) * 360 - AAngle;
    end;
    'rad': Result := AngleDeg(radtodeg(AAngle));
    'grad': Result := AngleDeg(gradtodeg(AAngle));
    'turn': Result := AngleDeg(AAngle * 360);
  end;
end;

function ScaleLengthToScreen(AValueInPx: TPLFloat; AHTMLObject: TPLHTMLObject): TPLFloat;
var
  k: TPLFloat = 1;
begin
  if Assigned(AHTMLObject) then k := AHTMLObject.Zoom;
  Result := Screen.PixelsPerInch / 96 * k * AValueInPx;
end;

function AbsoluteLengthToPx(AValue: TPLFloat; const AUnit: TPLString; AHTMLObject: TPLHTMLObject): TPLFloat;
begin
  case AUnit.ToLower of
    'px': Result := AValue;
    'cm': Result := AValue * 37.795275590551181102362204724409;
    'mm': Result := AValue * 3.7795275590551181102362204724409;
    'q' : Result := AValue * 0.94488188976377952755905511811024;
    'in': Result := AValue * 96;
    'pc': Result := AValue * 16;
    'pt': Result := AValue * 1.3333333333333333333333333333333;
    else Result := 0;
  end;

  Result := ScaleLengthToScreen(Result, AHTMLObject);
end;

function RelativeLengthToPx(AValue: TPLFloat; const AUnit: TPLString; AHTMLObject: TPLHTMLObject; APropName: TPLString): TPLFloat;
begin
  if not Assigned(AHTMLObject) then exit(0);
  Result := AHTMLObject.CalculateRelativeLength(AValue, AUnit, APropName);
end;

function AutoLengthToPx(AValue: TPLFloat; const AUnit: TPLString; AHTMLObject: TPLHTMLObject; APropName: TPLString): TPLFloat;
begin
  case AUnit.ToLower of
    'px', 'cm', 'mm', 'q', 'in', 'pc', 'pt': Result := AbsoluteLengthToPx(AValue, AUnit, AHTMLObject);
    '%', 'ch', 'em', 'lh', 'ex', 'rem', 'vh', 'vw', 'vmin', 'vmax': Result := RelativeLengthToPx(AValue, AUnit, AHTMLObject, APropName);
    else Result := AValue;
  end;
end;

function ObjectToVariant(AObject: TObject): Variant;
begin
  tvardata(Result).vtype := varbyref or varvariant;
  tvardata(Result).vpointer := AObject;
end;

operator := (a: TPLFloat) b: TPLString;
begin
  b := FloatToStr(a, PLFormatSettingsDef);
end;

operator := (a: TPLString) b: TPLFloat;
begin
  b := 0;
  TryStrToFloat(a, b, PLFormatSettingsDef);
end;

operator := (a: TPLInt) b: TPLString;
begin
  b := IntToStr(a);
end;

operator := (a: TPLString) b: TPLInt;
begin
  b := 0;
  TryStrToInt64(a, b);
end;

operator := (a: TPLFloat) b: TPLBool;
begin
  b := a <> 0;
end;

operator := (a: TPLCSSElementState) b: TPLString;
begin
  case a of
    esActive: b := 'active';
    esFocus: b := 'focus';
    esFocusWithin: b := 'focus-within';
    esFocusVisible: b := 'focus-visible';
    esHover: b := 'hover';
    esTarget: b := 'target';
    esVisited: b := 'visited';
    else b := 'normal';
  end;
end;

operator := (a: TPLString) b: TPLCSSElementState;
begin
  case a.ToLower.Trim of
    'active': b := esActive;
    'focus': b := esFocus;
    'focus-within': b := esFocusWithin;
    'focus-visible': b := esFocusVisible;
    'hover': b := esHover;
    'target': b := esTarget;
    'visited': b := esVisited;
    else b := esNormal;
  end;
end;

operator * (a: TPLString; b: TPLInt) r: TPLString;
var
  i: TPLInt;
begin
  if b < 1 then r := '' else begin
    r := a;
    for i := 2 to b do r += a;
  end;
end;

operator mod (a, b: TPLFloat) r: TPLFloat;
begin
  r := fmod(a, b);
end;

operator in (a: TPLString; tab: specialize TArray<TPLString>): TPLBool;
var
  i: SizeInt;
begin
  for i := Low(tab) to High(tab) do begin
    if a = tab[i] then exit(true);
  end;

  Result := false;
end;

operator in (a: TPLFloat; tab: specialize TArray<TPLFloat>): TPLBool;
var
  i: SizeInt;
begin
  for i := Low(tab) to High(tab) do begin
    if a = tab[i] then exit(true);
  end;

  Result := false;
end;

operator + (a, b: specialize TArray<TPLString>): specialize TArray<TPLString>;
var
  i: SizeInt;
begin
  SetLength(Result, Length(a) + Length(b));
  for i := Low(a) to High(a) do Result[i] := a[i];
  for i := Low(b) to High(b) do Result[High(a)+i+1] := b[i];
end;

