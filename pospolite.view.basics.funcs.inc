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

  while Result >= b do Result := Result-b;

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

operator * (a: TPLString; b: TPLInt) r: TPLString;
var
  i: TPLInt;
begin
  if b < 1 then r := '' else begin
    r := a;
    for i := 2 to b do r += a;
  end;
end;

operator mod (a, b: TPLFloat)r: TPLFloat;
begin
  r := fmod(a, b);
end;
